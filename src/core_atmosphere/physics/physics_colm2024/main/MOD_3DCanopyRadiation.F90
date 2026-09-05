#include <define.h>

MODULE MOD_3DCanopyRadiation

!-----------------------------------------------------------------------
!
!            --- A 3D Canopy Radiation Transfer Model ---
!                for Plant Community (PC) Simulation
!
!                                             Sun
!                                            ///
!                                           ///
!            _____  tree           _____              --- Layer3
!          /|||||||               |||||||
!         /|||||||||             |||||||||
!        /  \|||||//            / \|||||//
!       /      |  /            /     |  /             --- Layer2
!      /       | /            /      | /        /xx\
!     / shadow |/     grass  /       |/   shrub/\xx/
!  __/.........|_________\\//\/......|________/..|/__ --- Layer1
! /////////////////////////////////////////////////////////////////////
!
!-----------------------------------------------------------------------

   USE MOD_Precision
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
#ifdef LULC_IGBP_PC
   PUBLIC :: ThreeDCanopy_wrap
#endif
   PUBLIC :: ThreeDCanopy


!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------


#ifdef LULC_IGBP_PC

   SUBROUTINE ThreeDCanopy_wrap (ipatch, czen, albg, albv, tran, ssun, ssha)

!-----------------------------------------------------------------------
!
! !DESCRIPTION:
!  This is a wrap SUBROUTINE to CALL 3D canopy radiative model below
!     CALL ThreeDCanopy()
!
!  Created by Hua Yuan, 08/2019
!
! !REFERENCES:
!  Yuan, H., R. E. Dickinson, Y. Dai, M. J. Shaikh, L. Zhou, W. Shangguan,
!  and D. Ji, 2014: A 3D canopy radiative transfer model for global climate
!  modeling: Description, validation, and application. Journal of Climate,
!  27, 1168-1192, https://doi.org/10.1175/JCLI-D-13-00155.1.
!
! !REVISIONS:
!
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Namelist, only: DEF_VEG_SNOW, DEF_PC_CROP_SPLIT
   USE MOD_LandPFT, only: patch_pft_s, patch_pft_e
   USE MOD_Vars_Global
   USE MOD_Const_PFT
   USE MOD_Vars_PFTimeInvariants
   USE MOD_Vars_PFTimeVariables

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer,  intent(in)  :: ipatch
   real(r8), intent(in)  :: czen
   real(r8), intent(in)  :: albg(2,2)
   real(r8), intent(out) :: albv(2,2)
   real(r8), intent(out) :: tran(2,3)
   real(r8), intent(out) :: ssun(2,2)
   real(r8), intent(out) :: ssha(2,2)

!-------------------------- Local Variables ----------------------------
   integer :: i, p, ps, pe, pn;

   ! sunlit absorption fraction calculation mode
   ! .true. USE 3D model, otherwise USE 1D case
   ! NOTE: The 3D version will be activated in the new release,
   !       accompanied by a new set of canopy structure data.
   logical, parameter :: fsun3D = .false.

   ! define allocatable variables
   integer,  allocatable :: canlay(:)
   real(r8), allocatable :: albd(:,:), albi(:,:)
   real(r8), allocatable :: fabd(:,:), fabi(:,:), fadd(:,:)
   real(r8), allocatable :: ftdd(:,:), ftid(:,:), ftii(:,:)
   real(r8), allocatable :: rho (:,:), tau (:,:)
   real(r8), allocatable :: csiz(:), chgt(:), chil(:), lsai(:)
   real(r8), allocatable :: fsun_id(:), fsun_ii(:), psun(:)
   real(r8), allocatable :: phi1(:), phi2(:), gdir(:), fcover(:)

   ! vegetation snow optical properties, 1:vis, 2:nir
   real(r8) :: rho_sno(2), tau_sno(2)
   data rho_sno(1), rho_sno(2) /0.5, 0.2/
   data tau_sno(1), tau_sno(2) /0.3, 0.2/
!-----------------------------------------------------------------------

      ! get patch PFT index
      ps = patch_pft_s(ipatch)
      pe = patch_pft_e(ipatch)

      ! Calculate the end index of natrue PFT. Some patches can have an
      ! empty PFT slice (ps > pe); keep pn below ps in that case.
      pn = ps - 1
      DO i = ps, pe
         pn = i
         p = pftclass(i)
         IF (DEF_PC_CROP_SPLIT .and. p.ge.15) THEN
            pn = pn - 1
            EXIT
         ENDIF
      ENDDO

      ! If pn less than start index, there is no nature PFT
      ! Otherwise, set the new end index
      IF (pn.ge.ps) THEN
         pe = pn
      ELSE
         RETURN
      ENDIF

      ! allocate memory for defined variables
      allocate (albd   (ps:pe, 2) )
      allocate (albi   (ps:pe, 2) )
      allocate (fabd   (ps:pe, 2) )
      allocate (fabi   (ps:pe, 2) )
      allocate (fadd   (ps:pe, 2) )
      allocate (ftdd   (ps:pe, 2) )
      allocate (ftid   (ps:pe, 2) )
      allocate (ftii   (ps:pe, 2) )
      allocate (rho    (ps:pe, 2) )
      allocate (tau    (ps:pe, 2) )
      allocate (csiz   (ps:pe)    )
      allocate (chgt   (ps:pe)    )
      allocate (chil   (ps:pe)    )
      allocate (lsai   (ps:pe)    )
      allocate (canlay (ps:pe)    )
      allocate (fsun_id(ps:pe)    )
      allocate (fsun_ii(ps:pe)    )
      allocate (psun   (ps:pe)    )
      allocate (phi1   (ps:pe)    )
      allocate (phi2   (ps:pe)    )
      allocate (gdir   (ps:pe)    )
      allocate (fcover (ps:pe)    )

      ! initialization
      albd=1.; albi=1.; fabd=0.; fabi=0.;
      ftdd=1.; ftid=0.; ftii=1.; fadd=0.;
      csiz(:) = (htop_p(ps:pe) - hbot_p(ps:pe)) / 2
      chgt(:) = (htop_p(ps:pe) + hbot_p(ps:pe)) / 2
      lsai(:) = lai_p(ps:pe) + sai_p(ps:pe)
      fcover(ps:pe) = pftfrac(ps:pe) / sum(pftfrac(ps:pe))

      ! calculate weighted plant optical properties
      ! loop for each PFT
      rho = 0.
      tau = 0.
      DO i = ps, pe

         p = pftclass(i)
         canlay(i) = canlay_p(p)
         chil(i)   = chil_p(p)

         IF (lsai(i) > 0.) THEN
            rho(i,:) = rho_p(:,1,p)*lai_p(i)/lsai(i) &
                     + rho_p(:,2,p)*sai_p(i)/lsai(i)
            tau(i,:) = tau_p(:,1,p)*lai_p(i)/lsai(i) &
                     + tau_p(:,2,p)*sai_p(i)/lsai(i)
         ENDIF

         ! account for snow on vegetation
         IF ( DEF_VEG_SNOW ) THEN
            ! modify rho, tau, USE: fwet_snow_p
            rho(i,:) = (1-fwet_snow_p(i))*rho(i,:) + fwet_snow_p(i)*rho_sno(:)
            tau(i,:) = (1-fwet_snow_p(i))*tau(i,:) + fwet_snow_p(i)*tau_sno(:)
         ENDIF

      ENDDO

      ! CALL 3D canopy radiation transfer model
      CALL ThreeDCanopy(ps, pe, canlay, fcover(ps:pe), csiz, chgt, chil, czen, &
                        lsai, rho, tau, albg(:,1), albg(:,2), albd, albi, &
                        fabd, fabi, ftdd, ftid, ftii, fadd, psun, fsun_id, fsun_ii, &
                        thermk_p(ps:pe), fshade_p(ps:pe) )

      ! calculate extkb_p, extkd_p
      ! applied for 1D case
      extkd_p(ps:pe) = 0.719 !used for scaling-up coefficients from leaf to canopy

      ! 11/07/2018: calculate gee FUNCTION consider LAD
      DO i = ps, pe
         p = pftclass(i)
         phi1(i) = 0.5 - 0.633 * chil_p(p) - 0.33 * chil_p(p) * chil_p(p)
         phi2(i) = 0.877 * ( 1. - 2. * phi1(i) )
      ENDDO

      ! 11/07/2018: calculate gee FUNCTION consider LAD
      gdir = phi1 + phi2*czen
      extkb_p(ps:pe) = gdir/czen

      fsun_id(:) = 0.
      fsun_ii(:) = 0.

      ! 1D sunlit leaves absorption fraction in diffuse format
      ! Table 3, Yuan et al., (2014).
      DO p = ps, pe
         IF (lsai(p) > 0. .and. .not.fsun3D) THEN
            fsun_id(p) = (1._r8 - exp(-2._r8*extkb_p(p)*lsai(p))) &
                       / (1._r8 - exp(-extkb_p(p)*lsai(p))) &
                       / 2.0_r8 * psun(p)

            fsun_ii(p) = (1._r8 - exp(-extkb_p(p)*lsai(p)-lsai(p))) &
                       / (1._r8 - exp(-lsai(p))) &
                       / (1._r8 + extkb_p(p)) * psun(p)
         ENDIF
      ENDDO

      ! Calculate albv, ssun, ssha and tran for PFTs
      ! NOTE: CoLM (1/2,): vis/nir; (,1/2): dir/dif
      albv(1,1) = albd(ps,1); albv(1,2) = albi(ps,1)
      albv(2,1) = albd(ps,2); albv(2,2) = albi(ps,2)

      ! ssun(band, dir/dif, pft), fabd/fadd(pft, band)
      ssun_p(1,1,ps:pe) = (fabd(:,1)-fadd(:,1)) * fsun_id + fadd(:,1)
      ssun_p(2,1,ps:pe) = (fabd(:,2)-fadd(:,2)) * fsun_id + fadd(:,2)
      ssha_p(1,1,ps:pe) = (fabd(:,1)-fadd(:,1)) * (1.-fsun_id)
      ssha_p(2,1,ps:pe) = (fabd(:,2)-fadd(:,2)) * (1.-fsun_id)
      ssun_p(1,2,ps:pe) = fabi(:,1) * fsun_ii
      ssun_p(2,2,ps:pe) = fabi(:,2) * fsun_ii
      ssha_p(1,2,ps:pe) = fabi(:,1) * (1.-fsun_ii)
      ssha_p(2,2,ps:pe) = fabi(:,2) * (1.-fsun_ii)

      ssun(1,1) = sum( ssun_p(1,1,ps:pe) * pftfrac(ps:pe) )
      ssun(2,1) = sum( ssun_p(2,1,ps:pe) * pftfrac(ps:pe) )
      ssun(1,2) = sum( ssun_p(1,2,ps:pe) * pftfrac(ps:pe) )
      ssun(2,2) = sum( ssun_p(2,2,ps:pe) * pftfrac(ps:pe) )

      ssha(1,1) = sum( ssha_p(1,1,ps:pe) * pftfrac(ps:pe) )
      ssha(2,1) = sum( ssha_p(2,1,ps:pe) * pftfrac(ps:pe) )
      ssha(1,2) = sum( ssha_p(1,2,ps:pe) * pftfrac(ps:pe) )
      ssha(2,2) = sum( ssha_p(2,2,ps:pe) * pftfrac(ps:pe) )

      tran(1,1) = ftid(ps,1)
      tran(2,1) = ftid(ps,2)
      tran(1,3) = ftdd(ps,1)
      tran(2,3) = ftdd(ps,2)
      tran(1,2) = ftii(ps,1)
      tran(2,2) = ftii(ps,2)

      ! deallocate memory for defined variables
      deallocate (albd    )
      deallocate (albi    )
      deallocate (fabd    )
      deallocate (fabi    )
      deallocate (fadd    )
      deallocate (ftdd    )
      deallocate (ftid    )
      deallocate (ftii    )
      deallocate (rho     )
      deallocate (tau     )
      deallocate (csiz    )
      deallocate (chgt    )
      deallocate (chil    )
      deallocate (lsai    )
      deallocate (canlay  )
      deallocate (fsun_id )
      deallocate (fsun_ii )
      deallocate (psun    )
      deallocate (phi1    )
      deallocate (phi2    )
      deallocate (gdir    )
      deallocate (fcover  )

   END SUBROUTINE ThreeDCanopy_wrap
#endif


   SUBROUTINE ThreeDCanopy(ps, pe, canlay, fcover, csiz, chgt, chil, coszen, &
                           lsai, rho, tau, albgrd, albgri, albd, albi, &
                           fabd, fabi, ftdd, ftid, ftii, fadd, psun, &
                           fsun_id, fsun_ii, thermk, fshade)
!-----------------------------------------------------------------------
! !DESCRIPTION:
!  ThreeDCanopy based on Dickinson (2008) using three canopy layer
!  to calculate fluxes absorbed by vegetation, reflected by vegetation,
!  and transmitted through vegetation for unit incoming direct or
!  diffuse flux given an underlying surface with known albedo.
!
!  Created by Hua Yuan, 08/2019
!
! !HISTORY:
!  Before 2013: Robert E. Dickinson proposed the initial idea. Dickinson and
!               Muhammad J. Shake contributed to the code writing.
!
! !REFERENCES:
!  Yuan, H., R. E. Dickinson, Y. Dai, M. J. Shaikh, L. Zhou, W. Shangguan,
!  and D. Ji, 2014: A 3D canopy radiative transfer model for global climate
!  modeling: Description, validation, and application. Journal of Climate,
!  27, 1168-1192, https://doi.org/10.1175/JCLI-D-13-00155.1.
!
! !REVISIONS:
!
!-----------------------------------------------------------------------

   IMPLICIT NONE

   integer, parameter :: numrad = 2

!-------------------------- Dummy Arguments ----------------------------
   integer , intent(in)  :: ps, pe               !pft index bounds
   integer , intent(in)  :: canlay(ps:pe)        !canopy level for current pft
   real(r8), intent(in)  :: fcover(ps:pe)        !fractional cover of pft within a patch
   real(r8), intent(in)  :: csiz  (ps:pe)        !crown size of vegetation
   real(r8), intent(in)  :: chgt  (ps:pe)        !central height of crown
   ! NOTE: The 'cdcw' parameter will be activated in the new release, accompanied by
   !       a new set of canopy structure data. Currently we set cdcw = 1, i.e., sphere
   real(r8)              :: cdcw  (ps:pe)        !crown depth to crown width
   real(r8), intent(in)  :: chil  (ps:pe)        !leaf angle distribution parameter
   real(r8), intent(in)  :: lsai  (ps:pe)        !LAI+SAI
   real(r8), intent(in)  :: rho   (ps:pe,numrad) !leaf/stem refl weighted by fraction LAI and SAI
   real(r8), intent(in)  :: tau   (ps:pe,numrad) !leaf/stem tran weighted by fraction LAI and SAI

   real(r8), intent(in)  :: coszen               !cosine solar zenith angle for next time step
   real(r8), intent(in)  :: albgrd(numrad)       !ground albedo (direct) (column-level)
   real(r8), intent(in)  :: albgri(numrad)       !ground albedo (diffuse)(column-level)

   real(r8), intent(out) :: albd(ps:pe,numrad)   !surface albedo (direct)
   real(r8), intent(out) :: albi(ps:pe,numrad)   !surface albedo (diffuse)
   real(r8), intent(out) :: fabd(ps:pe,numrad)   !flux absorbed by veg per unit direct flux
   real(r8), intent(out) :: fabi(ps:pe,numrad)   !flux absorbed by veg per unit diffuse flux
   real(r8), intent(out) :: ftdd(ps:pe,numrad)   !down direct flux below veg per unit dir flx
   real(r8), intent(out) :: ftid(ps:pe,numrad)   !down diffuse flux below veg per unit dir flx
   real(r8), intent(out) :: ftii(ps:pe,numrad)   !down diffuse flux below veg per unit dif flx
   real(r8), intent(out) :: fadd(ps:pe,numrad)   !absorbed flux in direct mode per unit direct flux
   real(r8), intent(out) :: psun    (ps:pe)      !percent sunlit vegetation cover
   real(r8), intent(out) :: fsun_id (ps:pe)      !frac of dif rad abs. by sunlit leaves incident dir
   real(r8), intent(out) :: fsun_ii (ps:pe)      !frac of dif rad abs. by sunlit leaves incident dif
   real(r8), intent(out) :: thermk  (ps:pe)      !direct transmittance of diffuse radiation
   real(r8), intent(out) :: fshade  (ps:pe)      !shadow in diffuse case of vegetation

!-------------------------- Local Variables ----------------------------
   real(r8), parameter :: mpe = 1.0e-06_r8       !prevents overflow for division by zero
   integer , parameter :: nlay=3                 !number of canopy layers
   real(r8), parameter :: D0=0.0_r8              !double accuracy real number
   real(r8), parameter :: D1=1.0_r8              !double accuracy real number
   real(r8), parameter :: D2=2.0_r8              !double accuracy real number
   real(r8), parameter :: D3=3.0_r8              !double accuracy real number
   real(r8), parameter :: D4=4.0_r8              !double accuracy real number
   real(r8), parameter :: D6=6.0_r8              !double accuracy real number
   real(r8), parameter :: D7=7.0_r8              !double accuracy real number
   real(r8), parameter :: D8=8.0_r8              !double accuracy real number
   real(r8), parameter :: D9=9.0_r8              !double accuracy real number
   real(r8), parameter :: D10=10.0_r8            !double accuracy real number
   real(r8), parameter :: D16=16.0_r8            !double accuracy real number
   real(r8), parameter :: DH=0.5_r8              !quad accuracy real number
   real(r16),parameter :: DDH=0.5_r16            !quad accuracy real number
   real(r16),parameter :: DD0=0.0_r16            !quad accuracy real number
   real(r16),parameter :: DD1=1.0_r16            !quad accuracy real number
   real(r8) ,parameter :: pi=3.14159265358979323846_r8 !pi

   integer  :: ib                        !band index 1:vis 2:nir
   integer  :: ip,ic,ig,kband            !array indices for pft,column,grid
   integer  :: kfr                       !variable for layer radiation coming from
   integer  :: klay                      !variable for layer absorbing radiation
   integer  :: kto                       !variable for layer radiation is transmitted to
   integer  :: lev                       !do loop variable
   integer  :: nn                        !do loop variable
   integer  :: nsoilveg                  !number of pfts in gridcell with veg and cosz > 0
   integer  :: nstep                     !time step index
   integer  :: clev                      !canopy level for current pft

   real(r8) :: albd_col(numrad)          !surface reflection (direct) for column
   real(r8) :: albi_col(numrad)          !surface reflection (diffuse) for column
   real(r8) :: hbot_lay(nlay)            !average canopy bottom in layer
   real(r8) :: chgt_lay(nlay)            !average canopy height in layer
   real(r8) :: csiz_lay(nlay)            !average canopy size in layer
   real(r8) :: cdcw_lay(nlay)            !crown depth to crown width for layers
   real(r8) :: omg_lay(nlay,numrad)      !average omega for all three layer
   real(r8) :: rho_lay(nlay,numrad)      !average rho for all three layer
   real(r8) :: tau_lay(nlay,numrad)      !average tau for all three layer
   real(r8) :: lsai_lay(nlay)            !average lsai for each layer
   real(r8) :: cosz_lay(nlay)            !0.001 <= coszen <= 1.000
   real(r8) :: cosd_lay(nlay)            !0.001 <= coszen <= 1.000
   real(r8) :: delta                     !variable for increment layer in loop
   real(r8) :: dif                       !diffuse radiation transmitted
   real(r8) :: dir                       !direct radiation transmitted
   real(r8) :: fabd_col(numrad)          !flux absorbed by veg per unit diffuse flux
   real(r8) :: fabd_lay(nlay,numrad)     !layer absorption for direct beam
   real(r8) :: fabi_col(numrad)          !flux absorbed by veg per unit diffuse flux
   real(r8) :: fabi_lay(nlay,numrad)     !layer absorption for diffuse beam
   real(r8) :: fabs_lay(0:4,numrad)      !layer absorption for all five layers
   real(r8) :: fabs_leq(0:4,numrad)      !layer absorption for all five layers
   real(r8) :: A(6,6)                    !three-layer radiation transfer equation
                                         !(EQ. 19, Yuan et al., 2014)
   real(r8) :: B(6,2)                    !three-layer radiation transfer equation
                                         !(EQ. 19, Yuan et al., 2014)
   real(r8) :: X(6,2)                    !three-layer radiation transfer equation
                                         !(EQ. 19, Yuan et al., 2014)
   real(r8) :: fabsm                     !pft absorption for multiple reflections
   real(r8) :: faid_lay(nlay)            !layer diffused absorption for direct beam
   real(r8) :: faid_p                    !pft absorption direct beam
   real(r8) :: faii_lay(nlay)            !layer diffused absorption for diffuse beam
   real(r8) :: faii_p                    !pft absorption diffuse beam
   real(r8) :: fc0(nlay)                 !canopy fraction for layers
   real(r8) :: frid_lay(nlay)            !layer reflection for direct beam
   real(r8) :: frid_p                    !pft reflection direct beam
   real(r8) :: frii_lay(nlay)            !layer reflection for indirect beam
   real(r8) :: ftdd_lay(nlay)            !unscattered layer transmission for direct beam
   real(r8) :: ftdi_lay(nlay)            !unscattered layer transmission for indirect beam
   real(r8) :: ftdd_lay_orig(nlay)       !unscattered layer transmission for direct beam
                                         !without lad/crown_shape calibration
   real(r8) :: ftdi_lay_orig(nlay)       !unscattered layer transmission for indirect beam
                                         !without lad/crown_shape calibration
   real(r8) :: psun_lay(nlay)            !percent sunlit vegetation cover for layers
   real(r8) :: fsun_id_lay(nlay)         !frac of dif rad abs. by sunlit leaf incident dir
   real(r8) :: fsun_ii_lay(nlay)         !frac of dif rad abs. by sunlit leaf incident dif
   real(r8) :: fsun_dd_lay(nlay)         !frac of dif rad abs. by sunlit leaf incident downward dir
   real(r8) :: fsun_dw_lay(nlay)         !frac of dif rad abs. by sunlit leaf incident downward dif
   real(r8) :: fsun_up_lay(nlay)         !frac of dif rad abs. by sunlit leaf incident upward dif
   real(r8) :: ftid_lay(nlay)            !diffused layer transmission for direct beam
   real(r8) :: ftii_lay(nlay)            !diffused layer transmission for diffuse beam
   real(r8) :: ftran                     !pft transmittance
   real(r8) :: gee=0.5_r8                !Ross G factor geometric blocking
   real(r8) :: gdir(ps:pe)               !G factor considering LAD for incident direct radiation
   real(r8) :: gdif(ps:pe)               !G factor considering LAD for incident diffuse radiation
   real(r8) :: gdir_lay(nlay)            !G factor considering LAD for incident direct radiation
   real(r8) :: gdif_lay(nlay)            !G factor considering LAD for incident diffuse radiation
   real(r8) :: fcad(ps:pe)               !calibration factor for LAD for direct radiation
   real(r8) :: fcai(ps:pe)               !calibration factor for LAD for diffuse radiation
   real(r8) :: fcad_lay(nlay)            !calibration factor for LAD for direct radiation
   real(r8) :: fcai_lay(nlay)            !calibration factor for LAD for diffuse radiation
   real(r8) :: pad                       !probability function for absorption after two scat
   real(r8) :: pai                       !probability of absorption for diffuse incident beam
   real(r8) :: pfc                       !contribution of current pft in layer
   real(r8) :: probm                     !prob photon reflect diffusely from ground reach canopy
   real(r8) :: ref(0:nlay+1,0:nlay+1)    !radiation reflected between five layers
   real(r8) :: fadd_lay(nlay,numrad)     !layer absorbed flux in direct mode per unit direct flux
   real(r8) :: shad_oa(nlay,nlay)        !shadow overlaps (direct beam)
   real(r8) :: shadow_d(nlay)            !layer shadow for direct beam
   real(r8) :: shadow_i(nlay)            !layer shadow for diffuse beam
   real(r8) :: sum_fabd(3)               !sum of absorption for all pfts in grid (direct)
   real(r8) :: sum_fabi(3)               !sum of absorption for all pfts in grid (diffuse)
   real(r8) :: sum_fadd(nlay)            !sum of absorbed flux in direct mode per unit direct flux
   real(r8) :: taud_lay(nlay)            !direct transmission for a layer
   real(r8) :: taui_lay(nlay)            !diffuse transmission for a layer
   real(r8) :: trd(0:nlay+1,0:nlay+1)    !direct radiation transmitted between five layers
   real(r8) :: tri(0:4,0:4)              !diffuse radiation transmitted between five layers
   real(r8) :: tt(0:4,0:4)               !unscattered direct radiation available at layer
   real(r8) :: wl                        !fraction of LAI+SAI that is LAI
   real(r8) :: ws                        !fraction of LAI+SAI that is SAI
   real(r8) :: zenith                    !zenith angle
   real(r8) :: ftdd_col                  !unscattered column transmission for direct beam
   real(r8) :: fsun_f                    !forward incident light sunlit leaf absorption fraction
   real(r8) :: fsun_b                    !backward incident light sunlit leaf absorption fraction
   real(r8) :: fsun_a                    !temp variable 0.5*(fsun_f+fsun_b)
   real(r8) :: fsun_d                    !temp variable 0.5*(fsun_f-fsun_b)

   real(r8) :: shadow_pd(ps:pe)          !sky shadow area
   real(r8) :: shadow_pi(ps:pe)          !sky shadow area
   real(r8) :: shadow_sky(ps:pe)         !sky shadow area
   real(r8) :: taud(ps:pe)               !transmission to direct beam
   real(r8) :: taui(ps:pe)               !transmission to diffuse beam
   real(r8) :: omega(ps:pe,numrad)       !leaf/stem transmittance weighted by frac veg
   real(r8) :: ftdi(ps:pe,numrad)        !leaf/stem transmittance weighted by frac veg
   real(r8) :: ftdd_orig(ps:pe,numrad)   !leaf/stem transmittance weighted by frac veg
   real(r8) :: ftdi_orig(ps:pe,numrad)   !leaf/stem transmittance weighted by frac veg
   real(r8) :: cosz(ps:pe)               !0.001 <= coszen <= 1.000
   real(r8) :: cosd(ps:pe)               !0.001 <= coszen <= 1.000
   logical  :: soilveg(ps:pe)            !true if pft over soil with veg and cosz > 0

   real(r8) :: phi1(ps:pe), phi2(ps:pe)
!-----------------------------------------------------------------------

      ! 11/07/2018: calculate gee FUNCTION consider LAD
      phi1 = 0.5 - 0.633 * chil - 0.33 * chil * chil
      phi2 = 0.877 * ( 1. - 2. * phi1 )

      cdcw = 1.
      cosz = coszen
      zenith = acos(coszen)
      cosz = cosz * sqrt(1 / (cdcw**2*sin(zenith)**2 + cos(zenith)**2))

      cosd = cos(60._r8/180._r8*pi)
      zenith = 60._r8/180._r8*pi
      cosd = cosd * sqrt(1 / (cdcw**2*sin(zenith)**2 + cos(zenith)**2))

      ! 11/07/2018: calculate gee FUNCTION consider LAD
      gdir = phi1 + phi2*cosz
      gdif = phi1 + phi2*cosd

      nsoilveg = 0

      fc0 = D0
      omg_lay  = D0; rho_lay  = D0; tau_lay  = D0
      chgt_lay = D0; cdcw_lay = D0; hbot_lay = D0
      csiz_lay = D0; lsai_lay = D0
      cosz_lay = D0; cosd_lay = D0
      gdir_lay = D0; gdif_lay = D0

      DO ip = ps, pe
         shadow_sky(ip) = D1

         ! check elai and pft weight are non-zero
         IF ( lsai(ip)>1.e-6_r8 .and. fcover(ip)>D0 ) THEN

            soilveg(ip) = .true.
            nsoilveg  = nsoilveg + 1

            clev      = canlay(ip)
            fc0(clev) = fc0(clev) + fcover(ip)

            csiz_lay(clev) = csiz_lay(clev) + fcover(ip)*csiz(ip)
            chgt_lay(clev) = chgt_lay(clev) + fcover(ip)*chgt(ip)
            cdcw_lay(clev) = cdcw_lay(clev) + fcover(ip)*cdcw(ip)
            lsai_lay(clev) = lsai_lay(clev) + fcover(ip)*lsai(ip)
            cosz_lay(clev) = cosz_lay(clev) + fcover(ip)*cosz(ip)
            cosd_lay(clev) = cosd_lay(clev) + fcover(ip)*cosd(ip)
            gdir_lay(clev) = gdir_lay(clev) + fcover(ip)*gdir(ip)
            gdif_lay(clev) = gdif_lay(clev) + fcover(ip)*gdif(ip)

            ! set optical properties
            DO ib = 1, numrad
               omega(ip,ib) = rho(ip,ib) + tau(ip,ib)

               ! sum of tau,rho and omega for pfts in a layer
               tau_lay(clev,ib) = tau_lay(clev,ib) + fcover(ip)*(tau(ip,ib))
               rho_lay(clev,ib) = rho_lay(clev,ib) + fcover(ip)*(rho(ip,ib))
               omg_lay(clev,ib) = omg_lay(clev,ib) + fcover(ip)*(omega(ip,ib))

            ENDDO ! ENDDO ib=1, numrad
         ELSE
            soilveg(ip) = .false.
         ENDIF
      ENDDO ! ENDDO ip

!=============================================================
! layer average of lsai,tau,rho,omega...
!=============================================================

      DO lev = 1, 3
         IF (fc0(lev) > D0) THEN
            csiz_lay(lev) = max(csiz_lay(lev)/fc0(lev),D0)
            chgt_lay(lev) = max(chgt_lay(lev)/fc0(lev),D0)
            hbot_lay(lev) = chgt_lay(lev) - csiz_lay(lev)
            cdcw_lay(lev) = max(cdcw_lay(lev)/fc0(lev),D0)
            lsai_lay(lev) = max(lsai_lay(lev)/fc0(lev),D0)
            cosz_lay(lev) = max(cosz_lay(lev)/fc0(lev),D0)
            cosd_lay(lev) = max(cosd_lay(lev)/fc0(lev),D0)
            DO ib = 1, numrad
               tau_lay(lev,ib) = max(tau_lay(lev,ib)/fc0(lev),D0)
               rho_lay(lev,ib) = max(rho_lay(lev,ib)/fc0(lev),D0)
               omg_lay(lev,ib) = max(omg_lay(lev,ib)/fc0(lev),D0)
            ENDDO
            gdir_lay(lev) = max(gdir_lay(lev)/fc0(lev),D0)
            gdif_lay(lev) = max(gdif_lay(lev)/fc0(lev),D0)
         ENDIF
      ENDDO ! ENDDO ib

!=============================================================
! layer shadows
!=============================================================

      shadow_d = D0
      shadow_i = D0
      DO lev =1, 3
         IF ( fc0(lev)>D0 .and. cosz_lay(lev)>D0 ) THEN
            shadow_d(lev) = (D1 - exp(-D1*fc0(lev)/cosz_lay(lev))) &
                          / (D1 - fc0(lev)*exp(-D1/cosz_lay(lev)))
            shadow_d(lev) = max(fc0(lev), shadow_d(lev))
            shadow_i(lev) = (D1 - exp(-D1*fc0(lev)/cosd_lay(lev))) &
                          / (D1 - fc0(lev)*exp(-D1/cosd_lay(lev)))
            shadow_i(lev) = max(fc0(lev), shadow_i(lev))
         ENDIF
      ENDDO

!=============================================================
! taud and ftdd for layers
!=============================================================

      taud_lay = D0; taui_lay = D0
      ftdd_lay = D0; ftdi_lay = D0
      fcad_lay = D1; fcai_lay = D1
      ftdd_lay_orig = D0
      ftdi_lay_orig = D0

      DO lev = 1, 3
         IF ( fc0(lev)>D0 .and. lsai_lay(lev)>D0 ) THEN

            taud_lay(lev) = D3/D4*gee*fc0(lev)*lsai_lay(lev) &
                          / (cosz_lay(lev)*shadow_d(lev))
            taui_lay(lev) = D3/D4*gee*fc0(lev)*lsai_lay(lev) &
                          / (cosd_lay(lev)*shadow_i(lev))

            ! 11/07/2018: LAD calibration
            ftdd_lay_orig(lev) = tee(DD1*taud_lay(lev))
            ftdi_lay_orig(lev) = tee(DD1*taui_lay(lev))

            ! 11/07/2018: gdir/gdif = FUNCTION(xl, cos)
            ftdd_lay(lev) = tee(DD1*taud_lay(lev)/gee*gdir_lay(lev))
            ftdi_lay(lev) = tee(DD1*taui_lay(lev)/gee*gdif_lay(lev))

            ! calibration for chil
            fcad_lay(lev) = (D1-ftdd_lay(lev)) / (D1-ftdd_lay_orig(lev))
            fcai_lay(lev) = (D1-ftdi_lay(lev)) / (D1-ftdi_lay_orig(lev))

         ENDIF
      ENDDO


!=============================================================
! absorption fraction in sunlit leaves in diffuse radiation format
! PART I
!=============================================================

      fsun_dd_lay(:) = D0
      fsun_dw_lay(:) = D0
      fsun_up_lay(:) = D0

      DO lev = 1, 3
         IF ( fc0(lev)>D0 .and. lsai_lay(lev)>D0 ) THEN

            fsun_f = 0.5*(1. - tee(DD1*2.*taud_lay(lev))) &
                   / (1. - tee(DD1*taud_lay(lev)))

            fsun_b =  2.*(tee(DD1*taud_lay(lev)) - exp(-2.*taud_lay(lev))) &
                   / (1. - tee(DD1*taud_lay(lev)))

            fsun_a = 0.5*(fsun_f + fsun_b)
            fsun_d = 0.5*(fsun_f - fsun_b)

            fsun_dd_lay(lev) = fsun_f
            fsun_dw_lay(lev) = fsun_a + 0.5*cosz_lay(lev)*fsun_d
            fsun_up_lay(lev) = fsun_a - 0.5*cosz_lay(lev)*fsun_d
         ENDIF
      ENDDO

!=============================================================
! initialize local variables for layers
!=============================================================

      albd_col = D0; albi_col = D0
      fabd_col = D0; fabd_lay = D0
      fabi_col = D0; fabi_lay = D0
      frid_lay = D0; frii_lay = D0
      tt       = D0

!=============================================================
! projection shadow overlapping fractions
!=============================================================

      zenith = acos(cosz_lay(3))
      shad_oa(3,2) = fc0(3)*OverlapArea(csiz_lay(3),chgt_lay(3)-hbot_lay(2), zenith)
      shad_oa(3,1) = fc0(3)*OverlapArea(csiz_lay(3),chgt_lay(3)-hbot_lay(1), zenith)
      zenith = acos(cosz_lay(2))
      shad_oa(2,1) = fc0(2)*OverlapArea(csiz_lay(2),chgt_lay(2)-hbot_lay(1), zenith)

!=============================================================
! unscattered direct sunlight available at  each layer
! 4:sky, 3:top 2:middle 1:bottom and 0:ground layer
!=============================================================

      ftdd_col = D0; tt = D0

      tt(4,3) = shadow_d(3)
      tt(4,3) = min(D1, max(D0, tt(4,3)))
      tt(4,2) = shadow_d(2)*(D1-shadow_d(3)+shad_oa(3,2))
      tt(4,2) = min(1-tt(4,3), max(D0, tt(4,2)))
      tt(4,1) = shadow_d(1)*(D1-(shadow_d(2)-shad_oa(2,1)) &
              - (shadow_d(3)-shad_oa(3,1)) &
              + (shadow_d(2)-shad_oa(2,1))*(shadow_d(3)-shad_oa(3,2)))
      tt(4,1) = min(1-tt(4,3)-tt(4,2), max(D0, tt(4,1)))

      tt(4,0) = D1-(shadow_d(1)+shadow_d(2)+shadow_d(3) &
              - (shadow_d(2)-shad_oa(2,1))*shadow_d(1) &
              - (shadow_d(3)-shad_oa(3,2))*shadow_d(2) &
              - (shadow_d(3)-shad_oa(3,1))*shadow_d(1) &
              + (shadow_d(2)-shad_oa(2,1))*(shadow_d(3)-shad_oa(3,2))*shadow_d(1))
      tt(4,0) = min(1-tt(4,3)-tt(4,2)-tt(4,1), max(D0, tt(4,0)))

      IF (tt(4,0) < 0) THEN
         print *, abs(tt(4,0))
      ENDIF

      ! direct sunlight passing through top canopy layer
      IF (shadow_d(3) > 0) THEN
         tt(3,2) = shadow_d(2)*(shadow_d(3)-shad_oa(3,2))
         tt(3,2) = min(shadow_d(3), max(D0, tt(3,2)))
         tt(3,1) = shadow_d(1)*(shadow_d(3)-shad_oa(3,1) &
                 - (shadow_d(3)-shad_oa(3,2))*(shadow_d(2)-shad_oa(2,1)))
         tt(3,1) = min(shadow_d(3)-tt(3,2), max(D0, tt(3,1)))
         tt(3,0) = shadow_d(3)-tt(3,2)-tt(3,1)

         tt(3,2) = tt(3,2)*ftdd_lay(3)
         tt(3,1) = tt(3,1)*ftdd_lay(3)
         tt(3,0) = tt(3,0)*ftdd_lay(3)
      ENDIF

      ! direct sunlight passing through middle canopy layer
      IF (shadow_d(2) > 0) THEN
         tt(2,1) = shadow_d(1)*(shadow_d(2)-shad_oa(2,1))
         tt(2,1) = min(shadow_d(2), max(D0, tt(2,1)))
         tt(2,0) = shadow_d(2)-tt(2,1)

         tt(2,1) = tt(2,1)*ftdd_lay(2)*(tt(4,2) + tt(3,2))/shadow_d(2)
         tt(2,0) = tt(2,0)*ftdd_lay(2)*(tt(4,2) + tt(3,2))/shadow_d(2)
      ENDIF

      ! direct sunlight passing through third canopy layer
      IF (shadow_d(1) > 0) THEN
         tt(1,0) = ftdd_lay(1)*(tt(4,1) + tt(3,1) + tt(2,1))!*shadow_d(1)/shadow_d(1)
      ENDIF

!=============================================
! Aggregate direct radiation to layers
!=============================================

      tt(4,3)   = tt(4,3)
      tt(3,2)   = tt(4,2) + tt(3,2)
      tt(2,1)   = tt(4,1) + tt(3,1) + tt(2,1)
      tt(1,0)   = tt(4,0) + tt(3,0) + tt(2,0) + tt(1,0)
      ftdd_col  = tt(1,0)

      tt(0:4,4) = D0; tt(0:3,3) = D0
      tt(4:4,2) = D0; tt(0:2,2) = D0
      tt(3:4,1) = D0; tt(0:1,1) = D0
      tt(2:4,0) = D0; tt(0:0,0) = D0


!=======================================
! start radiation beam loop
! ib=1:visible band 2:nir band
!=======================================

      DO ib = 1, numrad

      !===============================
      ! get pft level tau and ftdd
      !===============================

         ! 10/12/2017
         ftdi(:,ib) = D1

         DO ip = ps, pe

            taud(ip) = D0
            taui(ip) = D0
            shadow_pd(ip) = D0
            shadow_pi(ip) = D0

            IF (soilveg(ip)) THEN
               clev = canlay(ip)

            !================================================
            ! fractional contribution of current pft in layer
            !================================================

               pfc = min( fcover(ip)/fc0(clev), D1)
               shadow_pd(ip) = pfc*shadow_d(clev)
               shadow_pi(ip) = pfc*shadow_i(clev)

            !=====================================
            ! get taud,taui at pft level
            !=====================================

               taud(ip) = D3/D4*gee*fcover(ip)*(lsai(ip)) &
                        / (cosz(ip)*shadow_pd(ip))

               taui(ip) = D3/D4*gee*fcover(ip)*(lsai(ip)) &
                        / (cosd(ip)*shadow_pi(ip))

            !====================================
            ! transmission at pft level
            !====================================

               ftdd_orig(ip,ib) = tee(DD1*taud(ip))
               ftdi_orig(ip,ib) = tee(DD1*taui(ip))

               ! 11/07/2018: gdir/gdif = FUNCTION(xl, cos)
               ftdd(ip,ib) = tee(DD1*taud(ip)/gee*gdir(ip))
               ftdi(ip,ib) = tee(DD1*taui(ip)/gee*gdif(ip))

               ! calibration for chil
               fcad(ip) = (D1-ftdd(ip,ib)) / (D1-ftdd_orig(ip,ib))
               fcai(ip) = (D1-ftdi(ip,ib)) / (D1-ftdi_orig(ip,ib))

            ENDIF ! ENDIF soilveg
         ENDDO ! ENDDO ip

      !===============================================================
      ! absorption, reflection and transmittance for three canopy layer
      ! using average optical properties of layers
      ! subroutine CanopyRad calculates fluxes for unit input radiation
      !===============================================================

         ftid_lay=D0; ftii_lay=D1
         frid_lay=D0; frii_lay=D0
         faid_lay=D0; faii_lay=D0

         DO lev = 1, 3
            IF (shadow_d(lev) > D0) THEN
               CALL CanopyRad(taud_lay(lev), taui_lay(lev), ftdd_lay_orig(lev),&
                    ftdi_lay_orig(lev), cosz_lay(lev), cosd_lay(lev), shadow_d(lev), &
                    shadow_i(lev), fc0(lev), omg_lay(lev,ib), lsai_lay(lev), &
                    tau_lay(lev,ib), rho_lay(lev,ib), ftid_lay(lev), &
                    ftii_lay(lev), frid_lay(lev), frii_lay(lev),&
                    faid_lay(lev), faii_lay(lev))
            ENDIF
         ENDDO ! ENDDO lev

         ! 11/07/2018: calibration for LAD
         ftid_lay(:) = fcad_lay(:)*ftid_lay(:)
         ftii_lay(:) = fcai_lay(:)*(ftii_lay(:)-ftdi_lay_orig(:)) + ftdi_lay(:)
         frid_lay(:) = fcad_lay(:)*frid_lay(:)
         frii_lay(:) = fcai_lay(:)*frii_lay(:)
         faid_lay(:) = fcad_lay(:)*faid_lay(:)
         faii_lay(:) = fcai_lay(:)*faii_lay(:)

      !=============================================
      ! Calculate layer direct beam radiation absorbed
      ! in the sunlit canopy as direct
      !=============================================

         fadd_lay(:,ib) = D0

         DO lev = 1, nlay
            IF ( fc0(lev)>D0 .and. lsai_lay(lev)>D0 ) THEN
               fadd_lay(lev,ib) = tt(lev+1,lev) * &
                  (D1-ftdd_lay(lev)) * (D1-omg_lay(lev,ib))
            ENDIF
         ENDDO

         A = D0; B = D0;
         fabs_leq = D0

         ! Calculate the coefficients matrix A
         A(1,1) = 1.0; A(1,3) = -shadow_i(3)*ftii_lay(3) + shadow_i(3) - 1.0;
         A(2,2) = 1.0; A(2,3) = -shadow_i(3)*frii_lay(3);
         A(3,3) = 1.0; A(3,2) = -shadow_i(2)*frii_lay(2);
         A(3,5) = -shadow_i(2)*ftii_lay(2) + shadow_i(2) - 1.0;

         A(4,4) = 1.0; A(4,5) = -shadow_i(2)*frii_lay(2);
         A(4,2) = -shadow_i(2)*ftii_lay(2) + shadow_i(2) - 1.0;

         A(5,5) = 1.0; A(5,4) = -shadow_i(1)*frii_lay(1);
         A(5,6) =(-shadow_i(1)*ftii_lay(1) + shadow_i(1) - 1.0) * albgri(ib);

         A(6,6) = 1.0 - albgri(ib)*shadow_i(1)*frii_lay(1);
         A(6,4) = -shadow_i(1)*ftii_lay(1) + shadow_i(1) - 1.0;

         ! The constant vector B at right side
         B(1,1) = tt(4,3)*frid_lay(3); B(1,2) = shadow_i(3)*frii_lay(3);
         B(2,1) = tt(4,3)*ftid_lay(3); B(2,2) = shadow_i(3)*ftii_lay(3) - shadow_i(3) + 1.0;
         B(3,1) = tt(3,2)*frid_lay(2); B(3,2) = 0.0;
         B(4,1) = tt(3,2)*ftid_lay(2); B(4,2) = 0.0;

         B(5,1) = tt(2,1)*frid_lay(1) &
                + tt(1,0)*albgrd(ib)*(shadow_i(1)*ftii_lay(1) - shadow_i(1) + 1.0);
         B(5,2) = 0.0;

         B(6,1) = tt(2,1)*ftid_lay(1) + tt(1,0)*albgrd(ib)*shadow_i(1)*frii_lay(1);
         B(6,2) = 0.0;

         ! Get the resolution
         CALL mGauss(A, B, X)

         ! ====================================================
         ! Set back to the absorption for each layer and albedo
         ! ====================================================

         ! Albedo
         fabs_leq(4,:) = X(1,:)

         ! Three layers' absorption for incident direct radiation
         fabs_leq(3,1) = tt(4,3)*faid_lay(3) &
                       + X(3,1)                                           *shadow_i(3)*faii_lay(3)
         fabs_leq(2,1) = tt(3,2)*faid_lay(2) &
                       + (X(2,1) + X(5,1))                                *shadow_i(2)*faii_lay(2)
         fabs_leq(1,1) = tt(2,1)*faid_lay(1) &
                       + (X(4,1) + X(6,1)*albgri(ib) + tt(1,0)*albgrd(ib))*shadow_i(1)*faii_lay(1)

         ! Ground absorption
         fabs_leq(0,1) = tt(1,0)*(1.0 - albgrd(ib)) + X(6,1)*(1.0 - albgri(ib))


         ! Three layers' absorption for incident diffuse radiation
         fabs_leq(3,2) = (1.     + X(3,2))            *shadow_i(3)*faii_lay(3)
         fabs_leq(2,2) = (X(2,2) + X(5,2))            *shadow_i(2)*faii_lay(2)
         fabs_leq(1,2) = (X(4,2) + X(6,2)*albgri(ib)) *shadow_i(1)*faii_lay(1)

         ! Ground absorption
         fabs_leq(0,2) = X(6,2) * (1.0 - albgri(ib))


         ! IF everything is ok, substitute fabs_lay for fabs_leq
         ! and delete the following line and the variables defined
         ! but not used anymore
         fabs_lay = fabs_leq

         ! set column absorption and reflection
         fabd_lay(1:3,ib) = fabs_lay(1:3,1)
         fabi_lay(1:3,ib) = fabs_lay(1:3,2)
         fabd_col(ib) = fabs_lay(1,1) + fabs_lay(2,1) + fabs_lay(3,1)
         fabi_col(ib) = fabs_lay(1,2) + fabs_lay(2,2) + fabs_lay(3,2)
         albd_col(ib) = fabs_lay(4,1)
         albi_col(ib) = fabs_lay(4,2)

         ! calculation for sunlit fraction and sunlit absorption for each layer
         IF (ib == 1) THEN !visible band only

            psun_lay(:)    = D0
            fsun_id_lay(:) = D0
            fsun_ii_lay(:) = D0

            ! - layer 3 -
            IF ( fc0(3)>D0 .and. lsai_lay(3)>D0 ) THEN
               ! sunlit fraction for layers
               psun_lay(3) = tt(4,3)/shadow_d(3)
               ! absorption fraction in sunlit leaves in diffuse radiation format
               ! PART II
               fsun_id_lay(3) = (psun_lay(3)*fsun_dd_lay(3) + X(3,1)*fsun_up_lay(3)) &
                              / (psun_lay(3) + X(3,1))
               fsun_ii_lay(3) = (1.*fsun_dw_lay(3) + X(3,2)*fsun_up_lay(3)) &
                              / (1. + X(3,2))
            ENDIF

            ! - layer 2 -
            IF ( fc0(2)>D0 .and. lsai_lay(2)>D0 ) THEN
               ! sunlit fraction for layers
               psun_lay(2) = tt(3,2)/shadow_d(2)
               ! absorption fraction in sunlit leaves in diffuse radiation format
               ! PART II
               fsun_id_lay(2) = (psun_lay(2)*fsun_dd_lay(2) + X(2,1)*fsun_dw_lay(2) &
                                                            + X(5,1)*fsun_up_lay(2)) &
                              / (psun_lay(2) + X(2,1) + X(5,1))
               fsun_ii_lay(2) = (X(2,2)*fsun_dw_lay(2) + X(5,2)*fsun_up_lay(2)) &
                              / (X(2,2) + X(5,2))
            ENDIF

            ! - layer 1 -
            IF ( fc0(1)>D0 .and. lsai_lay(1)>D0 ) THEN
               ! sunlit fraction for layers
               psun_lay(1) = tt(2,1)/shadow_d(1)
               ! absorption fraction in sunlit leaves in diffuse radiation format
               ! PART II
               fsun_id_lay(1) = (psun_lay(1)*fsun_dd_lay(1) + X(4,1)*fsun_dw_lay(1) &
                              + (X(6,1)*albgri(ib) + tt(1,0)*albgrd(ib))*fsun_up_lay(1)) &
                              / (psun_lay(1) + X(4,1) + X(6,1)*albgri(ib) + tt(1,0)*albgrd(ib))
               fsun_ii_lay(1) = (X(4,2)*fsun_dw_lay(1) + X(6,2)*albgri(ib)*fsun_up_lay(1)) &
                              / (X(4,2) + X(6,2)*albgri(ib))
            ENDIF
         ENDIF

         ! balance check
         IF (abs(fabd_col(ib)+albd_col(ib)+fabs_lay(0,1)-1) > 1e-6) THEN
            print *, "Imbalance kband=1"
            print *, fabd_col(ib)+albd_col(ib)+fabs_lay(0,1)-1
         ENDIF
         IF (abs(fabi_col(ib)+albi_col(ib)+fabs_lay(0,2)-1) > 1e-6) THEN
            print *, "Imbalance kband=2"
            print *, fabi_col(ib)+albi_col(ib)+fabs_lay(0,2)-1
         ENDIF

      !====================================================
      ! Calculate individual PFT absorption
      !====================================================

         sum_fabd = D0
         sum_fabi = D0
         sum_fadd = D0

         DO ip = ps, pe
            clev = canlay(ip)
            IF (clev == D0) CYCLE
            IF ( shadow_d(clev)>D0 .and. soilveg(ip) ) THEN

            !=================================================
            ! fractional contribution of current pft in layer
            !=================================================

               pfc = min( fcover(ip)/fc0(clev), D1)

            !=========================================
            ! shadow contribution from ground to sky
            !=========================================

               shadow_sky(ip) = shadow_pi(ip)

            !=======================================================
            ! absorption, reflection and transmittance fluxes for
            ! unit incident radiation over pft.
            !=======================================================

               CALL CanopyRad(taud(ip), taui(ip), ftdd_orig(ip,ib), ftdi_orig(ip,ib), &
                    cosz(ip),cosd(ip), shadow_pd(ip), shadow_pi(ip), fcover(ip),&
                    omega(ip,ib), lsai(ip), tau(ip,ib),&
                    rho(ip,ib), ftid(ip,ib), ftii(ip,ib), albd(ip,ib),&
                    albi(ip,ib), faid_p, faii_p)

               ! calibration for LAD
               ! 11/07/2018: calibration for LAD
               ftid(ip,ib) = fcad(ip)*ftid(ip,ib)
               ftii(ip,ib) = fcai(ip)*(ftii(ip,ib)-ftdi_orig(ip,ib)) + ftdi(ip,ib)
               albd(ip,ib) = fcad(ip)*albd(ip,ib)
               albi(ip,ib) = fcai(ip)*albi(ip,ib)
               faid_p = fcad(ip)*faid_p
               faii_p = fcai(ip)*faii_p

               ! absorptions after multiple reflections for each pft
               probm = albi(ip,ib)*shadow_sky(ip)*albgri(ib)
               ftran = (D1-shadow_pd(ip)+shadow_pd(ip)*ftdd(ip,ib))*albgrd(ib) &
                     + shadow_pd(ip)*ftid(ip,ib)*albgri(ib)
               fabsm = ftran*faii_p*shadow_sky(ip)/(D1-probm)
               fabd(ip,ib) = shadow_pd(ip)*faid_p + fabsm

               probm = albi(ip,ib)*shadow_sky(ip)*albgri(ib)
               ftran = D1-shadow_pi(ip)*(D1 -ftii(ip,ib))
               fabsm = ftran*albgri(ib)*faii_p*shadow_sky(ip)/(D1-probm)
               fabi(ip,ib) = shadow_pi(ip)*faii_p + fabsm

               ! sum of pft absorptions in column
               sum_fabd(clev) = sum_fabd(clev) + fabd(ip,ib)
               sum_fabi(clev) = sum_fabi(clev) + fabi(ip,ib)

               ! pft absorption in sunlit as direct beam
               fadd(ip,ib) = shadow_pd(ip) * (D1-ftdd(ip,ib)) * (D1-omega(ip,ib))

               ! sum of pft absorption in sunlit as direct beam
               sum_fadd(clev) = sum_fadd(clev) + fadd(ip,ib)

            ENDIF ! ENDIF shadow & soilveg
         ENDDO ! ENDDO ip

         DO ip = ps, pe
            clev = canlay(ip)

         !===========================================================
         ! adjust pft absorption for total column absorption per
         ! unit column area
         !===========================================================

            IF (soilveg(ip)) THEN
               fabd(ip,ib) = fabd(ip,ib)*fabd_lay(clev,ib) &
                           / sum_fabd(clev)/fcover(ip)
               fabi(ip,ib) = fabi(ip,ib)*fabi_lay(clev,ib) &
                           / sum_fabi(clev)/fcover(ip)

               fadd(ip,ib) = fadd(ip,ib)*fadd_lay(clev,ib) &
                           / sum_fadd(clev)/fcover(ip)

               fadd(ip,ib) = min(fabd(ip,ib), fadd(ip,ib))

               psun(ip)    = psun_lay(clev)
               fsun_id(ip) = fsun_id_lay(clev)
               fsun_ii(ip) = fsun_ii_lay(clev)

            ELSE
               fabd(ip,ib) = D0
               fabi(ip,ib) = D0
               fadd(ip,ib) = D0

               psun(ip)    = D0
               fsun_id(ip) = D0
               fsun_ii(ip) = D0
            ENDIF

            ! column albedo is assigned to each pft in column
            ! Added by Yuan, 06/03/2012
            albd(ip,ib) = albd_col(ib)
            albi(ip,ib) = albi_col(ib)

            ! adjust ftdd and ftii for multi reflections between layers

! 03/06/2020, yuan: NOTE! there is no physical mean of ftdd,
! ftid, ftii anymore. they are the same for each PFT can only
! be used to calculate the ground absorption.
            ftdd(ip,ib) = ftdd_col
            ftid(ip,ib) = (D1-albd(ip,ib)-fabd_col(ib)-&
                          ftdd(ip,ib)*(D1-albgrd(ib))) /(D1-albgri(ib))
            ftii(ip,ib) = (D1-albi(ip,ib)-fabi_col(ib))/(D1-albgri(ib))

            !ftdd(ip,ib) = min(max(ftdd(ip,ib),D0),D1)
            !ftii(ip,ib) = min(max(ftii(ip,ib),D0),D1)
            !ftid(ip,ib) = min(max(ftid(ip,ib),D0),D1)

            ! check energy balance
            !fabd(ip,ib) = D1 - albd(ip,ib) &
            !            - ftdd(ip,ib)*(D1-albgrd(ib)) &
            !            - ftid(ip,ib)*(D1-albgri(ib))
            !fabi(ip,ib) = D1 - albi(ip,ib) &
            !            - ftii(ip,ib)*(D1-albgri(ib))

         ENDDO ! ENDDO ip
      ENDDO !ENDDO ib

      ! set parameters for longwave calculation
      fshade(:) = shadow_pi(:)
      thermk(:) = ftdi(:,1)

   END SUBROUTINE ThreeDCanopy

!-----------------------------------------------------------------------
! FUNCTION tee
!-----------------------------------------------------------------------

   real(selected_real_kind(12)) FUNCTION tee(tau)

   IMPLICIT NONE

   real(r16), parameter :: DDH = 0.50_r16 !128-bit accuracy real
   real(r16), parameter :: DD1 = 1.0_r16  !128-bit accuracy real
   real(r16), parameter :: DD2 = 2.0_r16  !128-bit accuracy real
   real(r16) :: tau ! transmittance

      tee = DDH*(DD1/tau/tau-(DD1/tau/tau+DD2/tau)*exp(-DD2*tau))

   END FUNCTION tee

!-----------------------------------------------------------------------
! FUNCTION overlapArea
!-----------------------------------------------------------------------

   real(selected_real_kind(12)) FUNCTION OverlapArea(radius, hgt, zenith)

   IMPLICIT NONE

   real(r8), parameter :: rpi = 3.14159265358979323846_R8  !pi
   real(r8), parameter :: D0  = 0.0_r8  !128-bit accuracy real
   real(r8), parameter :: D1  = 1.0_r8  !128-bit accuracy real

   real(r8) :: radius !radius of bus
   real(r8) :: hgt    !height of canopy
   real(r8) :: zenith !zenith angle
   real(r8) :: cost   !cosine of angle
   real(r8) :: theta  !angle

      IF (radius == D0) THEN
         OverlapArea= D0
         RETURN
      ENDIF
      cost = hgt*tan(zenith)/radius/(D1+D1/cos(zenith))
      IF (cost >= 1) THEN
         OverlapArea= D0
         RETURN
      ENDIF
      theta = acos(cost)
      OverlapArea = (theta-cost*sin(theta))*(D1+D1/cos(zenith))/rpi
      RETURN
   END FUNCTION OverlapArea

!-----------------------------------------------------------------------
! FUNCTION to calculate scattering, absorption, reflection and
! transmittance for unit input radiation
!-----------------------------------------------------------------------

   SUBROUTINE CanopyRad(tau_d, tau_i, ftdd, ftdi, cosz, cosd, &
                        shadow_d, shadow_i, fc, omg, lsai, tau_p, rho_p, &
                        ftid, ftii, frid, frii, faid, faii)
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   real(r8) :: cosz      !0.001 <= coszen <= 1.000
   real(r8) :: cosd      !0.001 <= coszen <= 1.000
   real(r8) :: faid      !direct absorption
   real(r8) :: faii      !diffuse absorption
   real(r8) :: fc        !fraction of grid covered with canopy
   real(r8) :: frid      !direct reflectance
   real(r8) :: frii      !diffuse reflectance
   real(r8) :: frio      !diffuse reflectance
   real(r8) :: ftdd      !down direct flux below veg per unit dir flx
   real(r8) :: ftdi      !down direct flux below veg per unit dif flux
   real(r8) :: ftid      !direct transmittance
   real(r8) :: ftii      !diffuse transmittance
   real(r8) :: omg       !frac of intercepted rad that is scattered
   real(r8) :: rho_p     !leaf/stem reflectance weighted by fract of LAI and SAI
   real(r8) :: shadow_d  !canopy shadow for direct solar
   real(r8) :: shadow_i  !canopy shadow for diffuse solar
   real(r8) :: tau_d     !radial optical depth for direct beam
   real(r8) :: tau_i     !radial optical depth for indirect beam
   real(r8) :: tau_p     !leaf/stem transmission weighted by frac of LAI & SAI
   real(r8) :: lsai      !elai+esai

   ! output variables
   real(r8) :: phi_dif_d !difference of rad scattered forward-backward per direct beam
   real(r8) :: phi_dif_i !difference of rad scattered forward-backward per direct beam
   real(r8) :: phi_tot_d !total rad scattered in all direction per direct beam
   real(r8) :: phi_tot_i !total rad scattered in all direction per diffuse beam
   real(r8) :: phi_tot_o !total rad scattered in all direction per direct beam
   real(r8) :: phi_dif_o !total rad scattered in all direction per diffuse beam
   real(r8) :: pa2       !total rad scattered in all direction per direct beam

!-------------------------- Local Variables ----------------------------
   logical  :: runmode = .true.
   real(r8) :: tau
   real(r8) :: muv       !forward frac of 3D scat rad in all direction for diffuse
   real(r8) :: ac        !forward frac of 3D scat rad in all direction for diffuse
   real(r8) :: ald       !forward frac of 3D scat rad in all direction for diffuse
   real(r8) :: ali       !forward frac of 3D scat rad in all direction for diffuse

   real(r8) :: wb        !EQ. (2.14), Dickinson 1983, omega*beta
   real(r8) :: alpha     !EQ. (2.14), Dickinson 1983, alpha
   real(r8) :: nd        !EQ. (4), Appendix 1, Yuan, dissertation
   real(r8) :: ni        !EQ. (4), Appendix 1, Yuan, dissertation
   real(r8) :: gee=0.5_r8                   !Ross factor geometric blocking

   real(r8) , parameter :: D0  = 0.0_r8     !64-bit real number
   real(r8) , parameter :: D1  = 1.0_r8     !64-bit real number
   real(r8) , parameter :: D2  = 2.0_r8     !64-bit real number
   real(r8) , parameter :: D3  = 3.0_r8     !64-bit real number
   real(r8) , parameter :: D4  = 4.0_r8     !64-bit real number
   real(r8) , parameter :: D6  = 6.0_r8     !64-bit real number
   real(r8) , parameter :: DH  = 0.5_r8     !64-bit real number
   real(r16), parameter :: DD1 = 1.0_r16    !128-bit real number

   real(r8) , parameter :: pi  = 3.14159265358979323846_R8  !pi
!-----------------------------------------------------------------------

      tau = D3/D4*gee*lsai

      CALL phi(runmode, tau_d, omg, tau_p, rho_p, phi_tot_d, phi_dif_d, pa2)
      CALL phi(runmode, tau_i, omg, tau_p, rho_p, phi_tot_i, phi_dif_i, pa2)
      CALL phi(runmode, tau  , omg, tau_p, rho_p, phi_tot_o, phi_dif_o, pa2)

      IF (runmode) THEN
         ! NOTE: modified
         frio = DH*(phi_tot_o - DH*phi_dif_o)
         frio = max(min(frio,D1),D0)

         muv  = D3*( D1 - sqrt(D1-sqrt(D3)*fc/(D2*pi)) ) + &
                D3*( D1 - sqrt(D1-sqrt(D3)*fc/(D6*pi)) )

         wb = D2/D3*rho_p + D1/D3*tau_p
         alpha = sqrt(D1-omg) * sqrt(D1-omg+D2*wb)
         nd = (D1 + D2*alpha) / (D1 + D2*alpha*cosz)
         ni = (D1 + D2*alpha) / (D1 + D2*alpha*cosd)

         ac  = phi_tot_o * muv * (D1-tee(DD1*tau)) * (D1-omg) / (D1-omg*pa2)
         ald = (nd-D1) * frio * fc * (D1/shadow_d - cosz/fc)
         ali = (ni-D1) * frio * fc * (D1/shadow_i - cosd/fc)
      ENDIF

!-----------------------------------------------------------------------
!frac indirect downward rad through canopy for black soil & direct solar
!-----------------------------------------------------------------------
      frid = DH*(phi_tot_d - DH*cosz*phi_dif_d)
      frii = DH*(phi_tot_i - DH*cosd*phi_dif_i)

      IF (runmode) THEN
         frid = frid + ald - DH*ac
         frii = frii + ali - DH*ac
      ENDIF

      frid = max(min(frid,D1),D0)
      frii = max(min(frii,D1),D0)

!---------------------------------------------------------------------
!downward diffuse fraction from direct and diffuse sun
!---------------------------------------------------------------------
      ftid = DH*(phi_tot_d + DH*cosz*phi_dif_d)
      ftii = DH*(phi_tot_i + DH*cosd*phi_dif_i) + ftdi

      IF (runmode) THEN
         ftid = ftid - DH*ald - DH*ac
         ftii = ftii - DH*ali - DH*ac
      ENDIF

      ftid = max(min(ftid,D1),D0)
      ftii = max(min(ftii,D1),D0)

!---------------------------------------------------------------------
! canopy absorption for direct or diffuse beams
!---------------------------------------------------------------------
      IF (.not. runmode) THEN
         faid = D1 - ftdd - phi_tot_d
         faii = D1 - ftdi - phi_tot_i
      ELSE
         faid = D1 - ftdd - frid - ftid
         faii = D1 - frii - ftii
      ENDIF

      faid = max(min(faid,D1),D0)
      faii = max(min(faii,D1),D0)

      IF (shadow_d == D0) THEN
         ! NOTE: corrected from D1 -> D0
         ftid = D0
         frid = D0
         faid = D0
      ENDIF
      IF (shadow_i == D0) THEN
         ftii = D1
         frii = D0
         faii = D0
      ENDIF

   END SUBROUTINE CanopyRad


   SUBROUTINE phi(runmode, tau, omg, tau_p, rho_p, phi_tot, phi_dif, pa2)

   IMPLICIT NONE

   ! input variables
   logical  :: runmode
   real(r8) :: omg       !frac of intercepted rad that is scattered
   real(r8) :: rho_p     !leaf/stem reflectance weighted by frac of LAI and SAI
   real(r8) :: tau       !radial optical depth for direct beam
   real(r8) :: tau_p     !leaf/stem transmission weighted by frac of LAI & SAI

   ! output variables
   real(r8) :: phi_dif   !difference of rad scattered forward-backward
   real(r8) :: phi_tot   !total rad scattered in all direction
   real(r8) :: pa2       !total rad scattered in all direction

   ! local variables
   real(r8) :: pac       !probability of absorption after two scatterings
   real(r8) :: phi_1b    !backward single scattered radiation
   real(r8) :: phi_1f    !forward single scattered radiation
   real(r8) :: phi_2a    !average second-order scattered radiation
   real(r8) :: phi_2b    !backward second-order scattered radiation
   real(r8) :: phi_2f    !forward second-order scattered radiation
   real(r8) :: phi_mb    !backward multiple scattered radiation
   real(r8) :: phi_mf    !forward multiple scattered radiation
   real(r8) :: phi_tb    !backward frac of 3D scat rad in all direction
   real(r8) :: phi_tf    !forward frac of 3D scat rad in all direction
   real(r8) :: aa,bb     !temporary constants

   real(r8) , parameter :: D0   = 0.0_r8   !64-bit real number
   real(r8) , parameter :: D1   = 1.0_r8   !64-bit real number

   real(r16), parameter :: DD1  = 1.0_r16  !128-bit real number
   real(r16), parameter :: DD2  = 2.0_r16  !128-bit real number
   real(r16), parameter :: DD3  = 3.0_r16  !128-bit real number
   real(r16), parameter :: DD4  = 4.0_r16  !128-bit real number
   real(r16), parameter :: DD9  = 9.0_r16  !128-bit real number
   real(r16), parameter :: DD10 = 10.0_r16 !128-bit real number
   real(r16), parameter :: DDH  = 0.5_r16  !128-bit real number

!----------------------------------------------------------------------
! single scattering terms for sphere with overlap corrections to path
! for direct and diffuse beams
!----------------------------------------------------------------------

      ! forward first order normalized scattering
      phi_1f = (DD1/tau/tau - (DD1/tau/tau + DD2/tau + DD2)*exp(-DD2*tau))

      ! backward first order normalized scattering
      phi_1b = DDH*(DD1 - tee(DD2*tau))

!----------------------------------------------------------------------
! sphere double scattering terms (RED 2008 Eqs. 19,20)
!----------------------------------------------------------------------

      IF (.not. runmode) THEN

         ! forward double scattering
         phi_2f = DDH*(DD4*phi_1f/DD3 + tee(DD2*tau) + tee(DD4*tau)/DD9 - &
                  DD10*tee(DD1*tau)/DD9)

         ! backward double scattering
         phi_2b = DDH*(DD1/DD3 - tee(DD2*tau) + DD2*tee(DD3*tau)/DD3)

      ELSE
         ! fitting FUNCTION for second order scattering
         aa = 0.70_r8
         bb = 1.74_r8

         phi_2b = aa*( DD1/(bb+DD1) -DD1/(bb-D1)*tee(DD2*tau) + &
                  DD2/(bb+DD1)/(bb-DD1)*tee((DD1+bb)*tau) )

         phi_2f = aa*( DD2*bb/(bb*bb-DD1)*phi_1f - &
                  (DD1/(bb+DD1)/(bb+DD1) + DD1/(bb-DD1)/(bb-DD1))*tee(DD1*tau) + &
                  DD1/(bb-DD1)/(bb-DD1)*tee(DD1*tau*bb) + &
                  DD1/(bb+DD1)/(bb+DD1)*tee(DD1*(bb+DD2)*tau) )
      ENDIF

      ! second order average scattering
      phi_2a = DDH*(phi_2b + phi_2f)

!----------------------------------------------------------------------
! probability of absorption after two scattering
!----------------------------------------------------------------------

      ! probability of absorption for diffuse beam
      ! corrected probability of absorption for direct beam
      pac = DD1-phi_2a / &
            (DD1 - tee(DD1*tau) - (rho_p*phi_1b + tau_p*phi_1f)/(tau_p+rho_p))

      pac = max(min(pac,D1),D0)
      pa2 = pac

!----------------------------------------------------------------------
!third order and higher order scatterings
!----------------------------------------------------------------------

      phi_mf = phi_2f + omg*pac*phi_2a/(DD1-omg*pac)
      phi_mb = phi_2b + omg*pac*phi_2a/(DD1-omg*pac)

!----------------------------------------------------------------------
! total sphere scattering,forward,backward, avg & diff for direct beam
!----------------------------------------------------------------------

      phi_tf  = tau_p*phi_1f + DDH*omg*omg*phi_mf
      phi_tb  = rho_p*phi_1b + DDH*omg*omg*phi_mb

      phi_tot = phi_tf + phi_tb
      phi_dif = phi_tf - phi_tb

   END SUBROUTINE phi

   SUBROUTINE mGauss(A, B, X)

   IMPLICIT NONE

   real(r8), intent(inout) :: A(6,6)
   real(r8), intent(inout) :: B(6,2)
   real(r8), intent(out)   :: X(6,2)

   integer :: i, j
   integer :: nstep(5) = (/0, 2, 1, 2, 1/)

   real(r8) :: f

      ! Elimination
      DO i = 1, 5
         DO j = i+1, i+nstep(i)
            IF (abs(A(i,i)) < 1.e-10) THEN
               print *, "Error in Gauss's solution"
               RETURN
            ENDIF
            f = - A(j,i)/A(i,i)
            A(j,:) = A(j,:) + f*A(i,:)
            B(j,:) = B(j,:) + f*B(i,:)
         ENDDO
      ENDDO

      ! Back substitution
      X(6,:) = B(6,:)/A(6,6)
      DO i = 5, 1, -1
         X(i,1) = (B(i,1) - sum(A(i,i+1:6)*X(i+1:6,1))) / A(i,i)
         X(i,2) = (B(i,2) - sum(A(i,i+1:6)*X(i+1:6,2))) / A(i,i)
      ENDDO

   END SUBROUTINE mGauss

END MODULE MOD_3DCanopyRadiation
! --------- EOP ----------
