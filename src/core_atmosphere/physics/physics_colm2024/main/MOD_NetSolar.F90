#include <define.h>

MODULE MOD_NetSolar

!-----------------------------------------------------------------------
   USE MOD_Precision
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: netsolar


!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------


   SUBROUTINE netsolar (ipatch,idate,deltim,dlon,patchtype,&
                        forc_sols,forc_soll,forc_solsd,forc_solld,&
                        alb,ssun,ssha,lai,sai,rho,tau,ssoi,ssno,ssno_lyr,fsno,&
                        parsun,parsha,sabvsun,sabvsha,sabg,sabg_soil,sabg_snow,sabg_snow_lyr,&
                        sr,solvd,solvi,solnd,solni,srvd,srvi,srnd,srni,&
                        solvdln,solviln,solndln,solniln,srvdln,srviln,srndln,srniln)

!-----------------------------------------------------------------------
! !DESCRIPTION:
!  Net solar absorbed by surface
!
!  Original author: Yongjiu Dai, 09/15/1999; 09/11/2001
!
! !REVISIONS:
!  05/2014, Hua Yuan: added for solar radiation output [vars: so*, sr*]
!
!  08/2014, Hua Yuan: added for local noon calculation
!
!  08/2020, Hua Yuan: added for PFT and PC calculation
!
!  12/2022, Hua Yuan: calculated snow layer absorption by SNICAR model
!
!-----------------------------------------------------------------------
! !USES:
   USE MOD_Precision
   USE MOD_Vars_Global
   USE MOD_Namelist, only: DEF_USE_SNICAR
   USE MOD_TimeManager, only: isgreenwich
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
   USE MOD_LandPFT, only: patch_pft_s, patch_pft_e
   USE MOD_Vars_PFTimeInvariants
   USE MOD_Vars_PFTimeVariables
   USE MOD_Vars_1DPFTFluxes
#endif

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer,  intent(in) :: ipatch     !patch index
   integer,  intent(in) :: idate(3)   !model time
   integer,  intent(in) :: patchtype  !land patch type (99-sea)

   real(r8), intent(in) :: dlon       !longitude in radians
   real(r8), intent(in) :: deltim     !seconds in a time step [second]

   real(r8), intent(in) :: &
         forc_sols,  &! atm vis direct beam solar rad onto srf [W/m2]
         forc_soll,  &! atm nir direct beam solar rad onto srf [W/m2]
         forc_solsd, &! atm vis diffuse solar rad onto srf [W/m2]
         forc_solld   ! atm nir diffuse solar rad onto srf [W/m2]

   real(r8), dimension(1:2,1:2), intent(in) :: &
         alb          ! averaged albedo [-]

   real(r8), dimension(1:2,1:2), intent(inout) :: &
         ssun,       &! sunlit canopy absorption for solar radiation
         ssha,       &! shaded canopy absorption for solar radiation
         ssoi,       &! ground soil absorption [-]
         ssno         ! ground snow absorption [-]

   real(r8), dimension(1:2,1:2,maxsnl+1:1), intent(inout) :: &
         ssno_lyr     ! snow layer absorption

   real(r8), intent(in) :: &
         lai,        &! leaf area index
         sai,        &! stem area index
         rho(2,2),   &! leaf reflectance (iw=iband, il=life and dead)
         tau(2,2)     ! leaf transmittance (iw=iband, il=life and dead)

   real(r8), intent(in) :: &
         fsno         ! snow fractional cover

   real(r8), intent(out) :: &
         parsun,     &! PAR absorbed by sunlit vegetation [W/m2]
         parsha,     &! PAR absorbed by shaded vegetation [W/m2]
         sabvsun,    &! solar absorbed by sunlit vegetation [W/m2]
         sabvsha,    &! solar absorbed by shaded vegetation [W/m2]
         sabg,       &! solar absorbed by ground  [W/m2]
! 03/06/2020, yuan:
         sabg_soil,  &! solar absorbed by ground soil [W/m2]
         sabg_snow,  &! solar absorbed by ground snow [W/m2]
         sr,         &! total reflected solar radiation (W/m2)
         solvd,      &! incident direct beam vis solar radiation (W/m2)
         solvi,      &! incident diffuse beam vis solar radiation (W/m2)
         solnd,      &! incident direct beam nir solar radiation (W/m2)
         solni,      &! incident diffuse beam nir solar radiation (W/m2)
         srvd,       &! reflected direct beam vis solar radiation (W/m2)
         srvi,       &! reflected diffuse beam vis solar radiation (W/m2)
         srnd,       &! reflected direct beam nir solar radiation (W/m2)
         srni,       &! reflected diffuse beam nir solar radiation (W/m2)
         solvdln,    &! incident direct beam vis solar radiation at local noon(W/m2)
         solviln,    &! incident diffuse beam vis solar radiation at local noon(W/m2)
         solndln,    &! incident direct beam nir solar radiation at local noon(W/m2)
         solniln,    &! incident diffuse beam nir solar radiation at local noon(W/m2)
         srvdln,     &! reflected direct beam vis solar radiation at local noon(W/m2)
         srviln,     &! reflected diffuse beam vis solar radiation at local noon(W/m2)
         srndln,     &! reflected direct beam nir solar radiation at local noon(W/m2)
         srniln       ! reflected diffuse beam nir solar radiation at local noon(W/m2)

   real(r8), intent(out) :: &
         sabg_snow_lyr(maxsnl+1:1)   ! solar absorbed by snow layers [W/m2]

!-------------------------- Local Variables ----------------------------
   integer  :: local_secs
   real(r8) :: radpsec, sabvg, sabg_noadj

   integer ps, pe, p

!-----------------------------------------------------------------------

      sabvsun = 0.
      sabvsha = 0.
      parsun  = 0.
      parsha  = 0.

      IF (lai+sai <= 1.e-6) THEN
         ssun(:,:) = 0.
         ssha(:,:) = 0.
      ENDIF

      sabg = 0.
      sabg_soil = 0.
      sabg_snow = 0.
      sabg_snow_lyr(:) = 0.

      IF (patchtype == 0) THEN
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
         ps = patch_pft_s(ipatch)
         pe = patch_pft_e(ipatch)

         sabvsun_p(ps:pe) = 0.
         sabvsha_p(ps:pe) = 0.
         parsun_p (ps:pe) = 0.
         parsha_p (ps:pe) = 0.

         DO p = ps, pe
            IF (lai_p(p)+sai_p(p) <= 1.e-6) THEN
               ssun_p(:,:,p) = 0.
               ssha_p(:,:,p) = 0.
            ENDIF
         ENDDO

         ssun(1,1) = sum( ssun_p(1,1,ps:pe)*pftfrac(ps:pe) )
         ssun(1,2) = sum( ssun_p(1,2,ps:pe)*pftfrac(ps:pe) )
         ssun(2,1) = sum( ssun_p(2,1,ps:pe)*pftfrac(ps:pe) )
         ssun(2,2) = sum( ssun_p(2,2,ps:pe)*pftfrac(ps:pe) )

         ssha(1,1) = sum( ssha_p(1,1,ps:pe)*pftfrac(ps:pe) )
         ssha(1,2) = sum( ssha_p(1,2,ps:pe)*pftfrac(ps:pe) )
         ssha(2,1) = sum( ssha_p(2,1,ps:pe)*pftfrac(ps:pe) )
         ssha(2,2) = sum( ssha_p(2,2,ps:pe)*pftfrac(ps:pe) )
#endif
      ENDIF

      IF (forc_sols+forc_soll+forc_solsd+forc_solld > 0.) THEN
         IF (patchtype < 4) THEN    !non lake and ocean
          ! Radiative fluxes onto surface
            parsun  = forc_sols*ssun(1,1) + forc_solsd*ssun(1,2)
            parsha  = forc_sols*ssha(1,1) + forc_solsd*ssha(1,2)
            sabvsun = forc_sols*ssun(1,1) + forc_solsd*ssun(1,2) &
                    + forc_soll*ssun(2,1) + forc_solld*ssun(2,2)
            sabvsha = forc_sols*ssha(1,1) + forc_solsd*ssha(1,2) &
                    + forc_soll*ssha(2,1) + forc_solld*ssha(2,2)
            sabvg   = forc_sols *(1.-alb(1,1)) + forc_solsd*(1.-alb(1,2)) &
                    + forc_soll *(1.-alb(2,1)) + forc_solld*(1.-alb(2,2))
            sabg    = sabvg - sabvsun - sabvsha

            IF (patchtype == 0) THEN

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)

               parsun_p (ps:pe) = forc_sols*ssun_p(1,1,ps:pe) + forc_solsd*ssun_p(1,2,ps:pe)
               parsha_p (ps:pe) = forc_sols*ssha_p(1,1,ps:pe) + forc_solsd*ssha_p(1,2,ps:pe)
               sabvsun_p(ps:pe) = forc_sols*ssun_p(1,1,ps:pe) + forc_solsd*ssun_p(1,2,ps:pe) &
                                + forc_soll*ssun_p(2,1,ps:pe) + forc_solld*ssun_p(2,2,ps:pe)
               sabvsha_p(ps:pe) = forc_sols*ssha_p(1,1,ps:pe) + forc_solsd*ssha_p(1,2,ps:pe) &
                                + forc_soll*ssha_p(2,1,ps:pe) + forc_solld*ssha_p(2,2,ps:pe)
#endif
            ENDIF

         ELSE                       !lake and ocean
            sabvg = forc_sols *(1.-alb(1,1)) + forc_soll *(1.-alb(2,1)) &
                  + forc_solsd*(1.-alb(1,2)) + forc_solld*(1.-alb(2,2))
            sabg  = sabvg
         ENDIF

         ! calculate soil and snow solar absorption
         sabg_soil = forc_sols*ssoi(1,1) + forc_solsd*ssoi(1,2) &
                   + forc_soll*ssoi(2,1) + forc_solld*ssoi(2,2)
         sabg_snow = forc_sols*ssno(1,1) + forc_solsd*ssno(1,2) &
                   + forc_soll*ssno(2,1) + forc_solld*ssno(2,2)

         sabg_soil = sabg_soil * (1.-fsno)
         sabg_snow = sabg_snow * fsno

         ! balance check and adjustment for soil and snow absorption
         ! this could happen when there is adjustment to ssun,ssha
         IF (abs(sabg_soil+sabg_snow-sabg)>1.e-6) THEN
            IF (.not. (idate(2)==1 .and. idate(3)==int(deltim))) THEN
               print *, "MOD_NetSolar.F90: NOTE imbalance in spliting soil and snow surface!", &
                         sabg_soil+sabg_snow-sabg
               print *, "Patchtype = ", patchtype
               print *, "sabg:", sabg, "sabg_soil:", sabg_soil, "sabg_snow", sabg_snow
               print *, "sabg_soil+sabg_snow:", sabg_soil+sabg_snow, "fsno:", fsno
            ENDIF

            sabg_noadj = sabg_soil + sabg_snow

            IF (sabg_noadj > 0.) THEN
               sabg_soil = sabg_soil * sabg/sabg_noadj
               sabg_snow = sabg_snow * sabg/sabg_noadj
               ssoi(:,:) = ssoi(:,:) * sabg/sabg_noadj
               ssno(:,:) = ssno(:,:) * sabg/sabg_noadj
            ENDIF
         ENDIF

         ! snow layer absorption calculation and adjustment for SNICAR model
         IF (DEF_USE_SNICAR) THEN
            ! adjust snow layer absorption due to multiple reflection between ground and canopy
            IF(sum(ssno_lyr(1,1,:))>0.) THEN
               ssno_lyr(1,1,:) = ssno(1,1) * ssno_lyr(1,1,:)/sum(ssno_lyr(1,1,:))
            ELSE
               ssno_lyr(1,1,1) = ssno(1,1)
            ENDIF

            IF(sum(ssno_lyr(1,2,:))>0.) THEN
               ssno_lyr(1,2,:) = ssno(1,2) * ssno_lyr(1,2,:)/sum(ssno_lyr(1,2,:))
            ELSE
               ssno_lyr(1,2,1) = ssno(1,2)
            ENDIF

            IF(sum(ssno_lyr(2,1,:))>0.) THEN
               ssno_lyr(2,1,:) = ssno(2,1) * ssno_lyr(2,1,:)/sum(ssno_lyr(2,1,:))
            ELSE
               ssno_lyr(2,1,1) = ssno(2,1)
            ENDIF

            IF(sum(ssno_lyr(2,2,:))>0.) THEN
               ssno_lyr(2,2,:) = ssno(2,2) * ssno_lyr(2,2,:)/sum(ssno_lyr(2,2,:))
            ELSE
               ssno_lyr(2,2,1) = ssno(2,2)
            ENDIF

            ! snow layer absorption
            sabg_snow_lyr(:) = forc_sols*ssno_lyr(1,1,:) + forc_solsd*ssno_lyr(1,2,:) &
                             + forc_soll*ssno_lyr(2,1,:) + forc_solld*ssno_lyr(2,2,:)

            ! convert to the whole area multiplied by snow fractional cover
            sabg_snow_lyr(:) = sabg_snow_lyr(:)*fsno

            ! attribute the first layer absorption to soil absorption
            sabg_soil = sabg_soil + sabg_snow_lyr(1)
            sabg_snow = sabg_snow - sabg_snow_lyr(1)

            ! make the soil absorption consistent
            sabg_snow_lyr(1) = sabg_soil
         ENDIF

      ENDIF

      solvd = forc_sols
      solvi = forc_solsd
      solnd = forc_soll
      solni = forc_solld
      srvd  = solvd*alb(1,1)
      srvi  = solvi*alb(1,2)
      srnd  = solnd*alb(2,1)
      srni  = solni*alb(2,2)
      sr    = srvd + srvi + srnd + srni

      ! calculate the local secs
      radpsec = pi/12./3600.
      IF ( isgreenwich ) THEN
         local_secs = idate(3) + nint((dlon/radpsec)/deltim)*deltim
         local_secs = mod(local_secs,86400)
      ELSE
         local_secs = idate(3)
      ENDIF

      IF (local_secs == 86400/2) THEN
         solvdln = forc_sols
         solviln = forc_solsd
         solndln = forc_soll
         solniln = forc_solld
         srvdln  = solvdln*alb(1,1)
         srviln  = solviln*alb(1,2)
         srndln  = solndln*alb(2,1)
         srniln  = solniln*alb(2,2)
      ELSE
         solvdln = spval
         solviln = spval
         solndln = spval
         solniln = spval
         srvdln  = spval
         srviln  = spval
         srndln  = spval
         srniln  = spval
      ENDIF

   END SUBROUTINE netsolar

END MODULE MOD_NetSolar
! ---------- EOP ------------
