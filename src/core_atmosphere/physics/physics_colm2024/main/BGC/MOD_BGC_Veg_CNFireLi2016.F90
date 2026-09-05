#include <define.h>
#ifdef BGC
MODULE MOD_BGC_Veg_CNFireLi2016

!-------------------------------------------------------------------------------------------------------------------
! !DESCRIPTION:
! This module calculate burned area of each fire. The burned area is used to calculate fire induced CN loss rates
! in bgc_veg_CNFireBaseMod.F90
!
! !REFERENCES:
! Li, F., Levis, S., and Ward, D. S. 2013a. Quantifying the role of fire in the Earth system - Part 1: Improved global fire
! modeling in the Community Earth System Model (CESM1). Biogeosciences 10:2293-2314.
! Li, F., and Lawrence, D. 2017. Role of fire in the global land water budget during the 20th century through changing
! ecosystems. J. Clim. 30: 1894-1908.
!
! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5)
!
! !REVISION:
! Xingjie Lu, 2021, revised the CLM5 code to be compatible with CoLM code structure.

   USE MOD_Precision
   USE MOD_TimeManager
   USE MOD_Const_Physical, only: tfrz
   USE MOD_Vars_1DForcing, only: &
       forc_q, forc_t, forc_psrf, forc_us, forc_vs
   USE MOD_Const_PFT, only: isshrub, isgrass, isbetr, isbdtr, isbare, iscrop, isnatveg, fd_pft, fsr_pft, rootfr_p
   USE MOD_Vars_TimeInvariants, only: &
       i_cwd, occur_hi_gdp_tree, gdp_lf, abm_lf, peatf_lf, &
       lfuel, ufuel, cropfire_a1, borealat, troplat, non_boreal_peatfire_c, boreal_peatfire_c, rh_low, rh_hgh, &
       bt_min, bt_max, pot_hmn_ign_counts_alpha, g0_fire, psi0, porsl, bsw
#ifdef vanGenuchten_Mualem_SOIL_MODEL
   USE MOD_Vars_TimeInvariants, only: theta_r, alpha_vgm, n_vgm, L_vgm, sc_vgm, fc_vgm
#endif
   USE MOD_Vars_TimeVariables, only: &
       decomp_cpools_vr , totlitc    , totvegc   ,  cropf      , lfwt     , fuelc     , fuelc_crop , fsr     , &
       fd               , rootc      , lgdp      , lgdp1       , lpop     , wtlf      , &
       trotr1           , trotr2     , hdm_lf    , lnfm        , baf_crop , baf_peatf , &
       farea_burned     , nfire      , fsat      , prec60      , wf2      , &
       tsoi17           , rh30       , t_soisno  , wliq_soisno
   USE MOD_BGC_Vars_PFTimeVariables, only: &
       burndate_p
   USE MOD_Vars_PFTimeInvariants, only: pftclass, pftfrac
   USE MOD_BGC_Vars_PFTimeVariables, only:  leafc_p     , leafc_storage_p     , leafc_xfer_p     , &
                                       frootc_p    , frootc_storage_p    , frootc_xfer_p    , &
                                       deadcrootc_p, deadcrootc_storage_p, deadcrootc_xfer_p, &
                                       livecrootc_p, livecrootc_storage_p, livecrootc_xfer_p
   USE MOD_Eroot, only: eroot
   USE MOD_Qsadv

   IMPLICIT NONE

   PUBLIC CNFireArea

CONTAINS

   SUBROUTINE CNFireArea(i,ps,pe,dlat,nl_soil,idate,dz_soi)

   integer ,intent(in) :: i                  ! patch index
   integer ,intent(in) :: ps                 ! start pft index
   integer ,intent(in) :: pe                 ! END pft index
   real(r8),intent(in) :: dlat               ! latitude (degree)
   integer ,intent(in) :: nl_soil            ! number of total soil layers
   integer ,intent(in) :: idate(3)           ! current date (year, day of the year, seconds of the day)
   real(r8),intent(in) :: dz_soi(1:nl_soil)  ! thicknesses of each soil layer

   integer  :: g,l,c,p,j,fc,fp,kyr, kmo, kda, mcsec   ! index variables
   integer  :: ivt
   real(r8) :: dayspyr  ! days per year
   real(r8) :: fb       ! availability of fuel for regs A and C
   real(r8) :: fhd      ! impact of hd on agricultural fire
   real(r8) :: fgdp     ! impact of gdp on agricultural fire
   real(r8) :: fire_m   ! combustability of fuel for fire occurrence
   real(r8) :: spread_m ! combustability of fuel for fire spread
   real(r8) :: Lb_lf    ! length-to-breadth ratio added by Lifang
   real(r8) :: lh       ! anthro. ignitions (count/km2/hr)
   real(r8) :: fs       ! hd-dependent fires suppression (0-1)
   real(r8) :: ig       ! total ignitions (count/km2/hr)
   real(r8) :: arh, arh30 !combustability of fuel related to RH and RH30
   real(r8) :: afuel    !weight for arh and arh30
   real(r8) :: eq
   real(r8) :: deqdT
   real(r8) :: qsatq
   real(r8) :: qsatqdT
   real(r8) :: forc_rh
   real(r8) :: rootr(nl_soil)
   real(r8) :: rresis(nl_soil)
   real(r8) :: smp_node
   real(r8) :: s_node
   real(r8) :: tmp1d(nl_soil)
   real(r8) :: tmp0d
   real(r8) :: btran2
   real(r8) :: btran2_p(ps:pe)

   real(r8),parameter :: secsphr = 3600._r8
   real(r8),parameter :: secspday = 86400._r8
   real(r8),parameter :: PI = 4.*atan(1.)
   integer m

      tsoi17 = forc_t(i)  ! Temporarily use air temperature for tsoi17, need to revised later.
      prec60 = 0          ! Temporarily use 0 for prec60, need to revised later
      wf2    = 0.5        ! Temporarily set up, need to revise later.
      fsat   = 0          ! Temporarily set up, need to revise later.
      rh30   = 0          ! Temporarily set up, need to revise later.

      CALL julian2monthday(idate(1),idate(2),kmo,kda)

      DO m = ps, pe
         CALL eroot(nl_soil,0._r8,porsl(1:,i),&
#ifdef Campbell_SOIL_MODEL
                    bsw(1:,i),&
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
                    theta_r(1:,i), alpha_vgm(1:,i), n_vgm(1:,i), L_vgm(1:,i), sc_vgm(1:,i), fc_vgm(1:,i), &
#endif
                    psi0(1:,i),rootfr_p(1:,pftclass(m)),dz_soi(1:),&
                    t_soisno(1:,i),wliq_soisno(1:,i),tmp1d,tmp0d,btran2_p(m))
         btran2 = sum(btran2_p(ps:pe) * pftfrac(m))
      ENDDO
    !
    ! Calculate fraction of crop (cropf_col) and non-crop and non-bare-soil
    ! vegetation (lfwt) in vegetated column
    !
      cropf(i) = 0._r8
      lfwt (i) = 0._r8

      ! For crop veg types
      DO m = ps, pe
         IF( iscrop(pftclass(m)) )THEN
            cropf(i) = cropf(i) + pftfrac(m)
         ENDIF
      ! For natural vegetation (non-crop and non-bare-soil)
         IF( isnatveg(pftclass(m)))THEN
            lfwt (i) = lfwt(i) + pftfrac(m)
         ENDIF
      ENDDO

      !
      ! Calculate crop fuel
      !
      fuelc_crop(i)=0._r8

      ! For crop PFTs, fuel load includes leaf and litter; only
      ! column-level litter carbon
      ! is available, so we use leaf carbon to estimate the
      ! litter carbon for crop PFTs
      DO m = ps, pe
         IF( iscrop(pftclass(m)) .and. sum(leafc_p(ps:pe)*pftfrac(ps:pe)) > 0._r8 )THEN
            fuelc_crop(i)= fuelc_crop(i) + (leafc_p(m) + leafc_storage_p(m) + leafc_xfer_p(m))*pftfrac(m)/cropf(i) &
                         + totlitc(i)*leafc_p(m)/sum(leafc_p(ps:pe)*pftfrac(ps:pe))*pftfrac(m)/cropf(i)
         ENDIF
      ENDDO
      !
      ! Calculate noncrop column variables
      !
      fsr   (i) = 0._r8
      fd    (i) = 0._r8
      rootc (i) = 0._r8
      lgdp  (i) = 0._r8
      lgdp1 (i) = 0._r8
      lpop  (i) = 0._r8
      wtlf  (i) = 0._r8
      trotr1(i) = 0._r8
      trotr2(i) = 0._r8

      ! Warning : ivt is not initialized.
      ! For non-crop -- natural vegetation and bare-soil
      IF( isnatveg(ivt) .or. isbare(ivt) )THEN
         IF (btran2  <=  1._r8 ) THEN
            wtlf(i)      = 1._r8
         ENDIF

         IF( isbetr(ivt) )THEN
            trotr1(i)=1._r8
         ENDIF
         IF( isbdtr(ivt) .and. abs(dlat) .lt. troplat)THEN
            trotr2(i)=1._r8
         ENDIF

         rootc(i) = rootc(i) + sum((frootc_p(ps:pe) + frootc_storage_p(ps:pe) + &
                          frootc_xfer_p(ps:pe) + deadcrootc_p(ps:pe) +                &
                          deadcrootc_storage_p(ps:pe) + deadcrootc_xfer_p(ps:pe) +    &
                          livecrootc_p(ps:pe)+livecrootc_storage_p(ps:pe) +           &
                          livecrootc_xfer_p(ps:pe)) * pftfrac(ps:pe))

         fsr(i) = fsr_pft(ivt)

         ! all these constants are in Li et al. BG (2012a,b;2013)

         IF( hdm_lf(i)  >  0.1_r8 )THEN
            ! For not bare-soil
            IF(.not. isbare(ivt) )THEN
               ! For shrub and grass (crop already excluded above)
               IF( isshrub(ivt) .or. isgrass(ivt) )THEN      !for shurb and grass
                  lgdp(i)  = lgdp(i) + (0.1_r8 + 0.9_r8*    &
                                 exp(-1._r8*PI* &
                                 (gdp_lf(i)/8._r8)**0.5_r8))/(1.0_r8-cropf(i))
                  lgdp1(i) = lgdp1(i) + (0.2_r8 + 0.8_r8*   &
                                 exp(-1._r8*PI* &
                                 (gdp_lf(i)/7._r8)))/(1._r8-cropf(i))
                  lpop(i)  = lpop(i) + (0.2_r8 + 0.8_r8*    &
                                 exp(-1._r8*PI* &
                                 (hdm_lf(i)/450._r8)**0.5_r8))/(1._r8-cropf(i))
               ELSE   ! for trees
                  IF( gdp_lf(i)  >  20._r8 )THEN
                     lgdp(i) =lgdp(i)+occur_hi_gdp_tree/(1._r8-cropf(i))
                     lgdp1(i) =lgdp1(i)+0.62_r8/(1._r8-cropf(i))
                  ELSE
                     IF( gdp_lf(i) > 8._r8 )THEN
                        lgdp(i)=lgdp(i)+0.79_r8/(1._r8-cropf(i))
                        lgdp1(i)=lgdp1(i)+0.83_r8/(1._r8-cropf(i))
                     ELSE
                        lgdp(i) = lgdp(i)+1._r8/(1._r8-cropf(i))
                        lgdp1(i)=lgdp1(i)+1._r8/(1._r8-cropf(i))
                     ENDIF
                  ENDIF
                  lpop(i) = lpop(i) + (0.4_r8 + 0.6_r8*    &
                                   exp(-1._r8*PI* &
                                   (hdm_lf(i)/125._r8)))/(1._r8-cropf(i))
               ENDIF
            ENDIF
         ELSE
            lgdp(i)  = lgdp(i)  + 1._r8/(1._r8-cropf(i))
            lgdp1(i) = lgdp1(i) + 1._r8/(1._r8-cropf(i))
            lpop(i)  = lpop(i)  + 1._r8/(1._r8-cropf(i))
         ENDIF

         fd(i) = fd_pft(ivt) * secsphr / (1.0_r8-cropf(i))
      ENDIF
      !
      ! calculate burned area fraction in cropland
      !
      baf_crop(i)=0._r8

      DO m = ps, pe
         IF( kmo == 1 .and. kda == 1 .and. idate(3) == 0 )THEN
            burndate_p(m) = 10000 ! init. value; actual range [0 365]
         ENDIF
      ENDDO

      ! For crop
      DO m = ps, pe
         IF( forc_t(i)  >=  tfrz .and. iscrop(ivt) .and.  &
            kmo == abm_lf(i) .and. burndate_p(m) >= 999)THEN ! catch  crop burn time

         ! calculate human density impact on ag. fire
            fhd = 0.04_r8+0.96_r8*exp(-1._r8*PI*(hdm_lf(i)/350._r8)**0.5_r8)

         ! calculate impact of GDP on ag. fire
            fgdp = 0.01_r8+0.99_r8*exp(-1._r8*PI*(gdp_lf(i)/10._r8))

         ! calculate burned area
            fb   = max(0.0_r8,min(1.0_r8,(fuelc_crop(i)-lfuel)/(ufuel-lfuel)))

         ! crop fire only for generic crop types at this time
         ! managed crops are treated as grasses IF crop model is turned on
            baf_crop(i) = baf_crop(i) + cropfire_a1/secsphr*fhd*fgdp
            IF( fb*fhd*fgdp  >  0._r8)THEN
               burndate_p(m)  =  kda
            ENDIF
         ENDIF
      ENDDO

      !
      ! calculate peatland fire
      !
      IF(dlat < borealat )THEN
         baf_peatf(i) = non_boreal_peatfire_c/secsphr*max(0._r8, &
                 min(1._r8,(4.0_r8-prec60(i)*secspday)/ &
                 4.0_r8))**2*peatf_lf(i)*(1._r8-fsat(i))
      ELSE
         baf_peatf(i) = boreal_peatfire_c/secsphr*exp(-PI*(max(wf2(i),0._r8)/0.3_r8))* &
                 max(0._r8,min(1._r8,(tsoi17(i)-tfrz)/10._r8))*peatf_lf(i)* &
                 (1._r8-fsat(i))
      ENDIF
      !
      ! calculate other fires
      !

      CALL qsadv(forc_t(i),forc_psrf(i),eq,deqdT,qsatq,qsatqdT)
      forc_rh = forc_q(i) / eq

      IF( cropf(i)  <  1._r8 )THEN
         fuelc(i) = totlitc(i)+totvegc(i)-rootc(i)-fuelc_crop(i)*cropf(i)
         DO j = 1, nl_soil
            fuelc(i) = fuelc(i)+decomp_cpools_vr(j,i_cwd,i) * dz_soi(j)
         ENDDO
         fuelc(i) = fuelc(i)/(1._r8-cropf(i))
         fb       = max(0.0_r8,min(1.0_r8,(fuelc(i)-lfuel)/(ufuel-lfuel)))
         IF (trotr1(i)+trotr2(i)<=0.6_r8) THEN
            afuel  =min(1._r8,max(0._r8,(fuelc(i)-2500._r8)/(5000._r8-2500._r8)))
            arh=1._r8-max(0._r8, min(1._r8,(forc_rh-rh_low)/(rh_hgh-rh_low)))
            arh30=1._r8-max(0.7_r8, min(1._r8,rh30(i)/90._r8))
            IF (forc_rh < rh_hgh.and. wtlf(i) > 0._r8 .and. tsoi17(i)> tfrz)THEN
               fire_m   = ((afuel*arh30+(1._r8-afuel)*arh)**1.5_r8)*((1._r8 -max(0._r8,&
                  min(1._r8,(btran2/wtlf(i)-bt_min)/(bt_max-bt_min))))**0.5_r8)
            ELSE
               fire_m   = 0._r8
            ENDIF
            lh       = pot_hmn_ign_counts_alpha*6.8_r8*hdm_lf(i)**(0.43_r8)/30._r8/24._r8
            fs       = 1._r8-(0.01_r8+0.98_r8*exp(-0.025_r8*hdm_lf(i)))
            ig       = (lh+lnfm(i)/(5.16_r8+2.16_r8*cos(PI/180._r8*3*min(60._r8,abs(dlat/PI*180))))*0.22_r8)  &
                       *(1._r8-fs)*(1._r8-cropf(i))
            nfire(i) = ig/secsphr*fb*fire_m*lgdp(i) !fire counts/km2/sec
            Lb_lf    = 1._r8+10._r8*(1._r8-EXP(-0.06_r8*sqrt(forc_us(i)*forc_us(i)+forc_vs(i)*forc_vs(i))))
            spread_m = fire_m**0.5_r8
            farea_burned(i) = min(1._r8,(g0_fire*spread_m*fsr(i)* &
                    fd(i)/1000._r8)**2*lgdp1(i)* &
                    lpop(i)*nfire(i)*PI*Lb_lf+ &
                    baf_crop(i)+baf_peatf(i))  ! fraction (0-1) per sec
         ELSE
            farea_burned(i)=min(1._r8,baf_crop(i)+baf_peatf(i))
         ENDIF
      ELSE
         farea_burned(i) = min(1._r8,baf_crop(i)+baf_peatf(i))
      ENDIF
   END SUBROUTINE CNFireArea

END MODULE MOD_BGC_Veg_CNFireLi2016
#endif
