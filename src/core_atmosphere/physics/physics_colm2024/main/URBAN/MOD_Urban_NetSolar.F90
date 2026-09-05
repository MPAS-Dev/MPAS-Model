#include <define.h>

MODULE MOD_Urban_NetSolar

   USE MOD_Precision
   IMPLICIT NONE
   SAVE

   PUBLIC :: netsolar_urban

CONTAINS

   SUBROUTINE netsolar_urban (ipatch,idate,dlon,deltim,&
                              forc_sols,forc_soll,forc_solsd,forc_solld,lai,sai,rho,tau,&
                              alb,ssun,ssha,sroof,swsun,swsha,sgimp,sgper,slake,&
                              sr,sabv,par,sabroof,sabwsun,sabwsha,sabgimp,sabgper,sablake,&
                              solvd,solvi,solnd,solni,srvd,srvi,srnd,srni,&
                              solvdln,solviln,solndln,solniln,srvdln,srviln,srndln,srniln)

!-----------------------------------------------------------------------
! !DESCRIPTION:
!  Net solar absorbed by urban surface.
!
!  Created by Hua Yuan, 09/2021
!
! !REVISIONS:
!
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Vars_Global
   USE MOD_TimeManager, only: isgreenwich
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer,  intent(in) :: ipatch   ! patch index
   integer,  intent(in) :: idate(3) ! model time

   real(r8), intent(in) :: dlon     ! longitude in radians
   real(r8), intent(in) :: deltim   ! seconds in a time step [second]

   real(r8), intent(in) :: &
        forc_sols,   &! atm vis direct beam solar rad onto srf [W/m2]
        forc_soll,   &! atm nir direct beam solar rad onto srf [W/m2]
        forc_solsd,  &! atm vis diffuse solar rad onto srf [W/m2]
        forc_solld    ! atm nir diffuse solar rad onto srf [W/m2]

   real(r8), intent(in) :: &
        lai,      &! leaf area index
        sai,      &! stem area index
        rho(2,2), &! leaf reflectance (iw=iband, il=life and dead)
        tau(2,2)   ! leaf transmittance (iw=iband, il=life and dead)

   real(r8), dimension(1:2,1:2), intent(in) :: &
        alb,      &! averaged albedo [-]
        ssun,     &! sunlit canopy absorption for solar radiation
        ssha,     &! shaded canopy absorption for solar radiation
        sroof,    &! roof absorption for solar radiation
        swsun,    &! sunlit wall absorption for solar radiation
        swsha,    &! shaded wall absorption for solar radiation
        sgimp,    &! impervious ground absorption for solar radiation
        sgper,    &! pervious ground absorption for solar radiation
        slake      ! lake absorption for solar radiation


   real(r8), intent(out) :: &
        sr,       &! total reflected solar radiation (W/m2)
        par,      &! PAR absorbed by sunlit vegetation [W/m2]
        sabv,     &! solar absorbed by sunlit vegetation [W/m2]
        sabroof,  &! roof absorbed solar radiation (W/m2)
        sabwsun,  &! sunlit wall absorbed solar radiation (W/m2)
        sabwsha,  &! shaded wall absorbed solar radiation (W/m2)
        sabgimp,  &! impervious ground absorbed solar radiation (W/m2)
        sabgper,  &! pervious ground absorbed solar radiation (W/m2)
        sablake,  &! solar absorbed by ground  [W/m2]
        solvd,    &! incident direct beam vis solar radiation (W/m2)
        solvi,    &! incident diffuse beam vis solar radiation (W/m2)
        solnd,    &! incident direct beam nir solar radiation (W/m2)
        solni,    &! incident diffuse beam nir solar radiation (W/m2)
        srvd,     &! reflected direct beam vis solar radiation (W/m2)
        srvi,     &! reflected diffuse beam vis solar radiation (W/m2)
        srnd,     &! reflected direct beam nir solar radiation (W/m2)
        srni,     &! reflected diffuse beam nir solar radiation (W/m2)
        solvdln,  &! incident direct beam vis solar radiation at local noon(W/m2)
        solviln,  &! incident diffuse beam vis solar radiation at local noon(W/m2)
        solndln,  &! incident direct beam nir solar radiation at local noon(W/m2)
        solniln,  &! incident diffuse beam nir solar radiation at local noon(W/m2)
        srvdln,   &! reflected direct beam vis solar radiation at local noon(W/m2)
        srviln,   &! reflected diffuse beam vis solar radiation at local noon(W/m2)
        srndln,   &! reflected direct beam nir solar radiation at local noon(W/m2)
        srniln     ! reflected diffuse beam nir solar radiation at local noon(W/m2)

!-------------------------- Local Variables ----------------------------
   integer  :: local_secs
   real(r8) :: radpsec

!-----------------------------------------------------------------------

      sabroof = 0.
      sabwsun = 0.
      sabwsha = 0.
      sabgimp = 0.
      sabgper = 0.
      sablake = 0.
      sabv    = 0.
      par     = 0.

      IF (forc_sols+forc_soll+forc_solsd+forc_solld > 0.) THEN

         sabroof = forc_sols *sroof(1,1) + forc_soll *sroof(2,1) &
                 + forc_solsd*sroof(1,2) + forc_solld*sroof(2,2)

         sabwsun = forc_sols *swsun(1,1) + forc_soll *swsun(2,1) &
                 + forc_solsd*swsun(1,2) + forc_solld*swsun(2,2)

         sabwsha = forc_sols *swsha(1,1) + forc_soll *swsha(2,1) &
                 + forc_solsd*swsha(1,2) + forc_solld*swsha(2,2)

         sabgimp = forc_sols *sgimp(1,1) + forc_soll *sgimp(2,1) &
                 + forc_solsd*sgimp(1,2) + forc_solld*sgimp(2,2)

         sabgper = forc_sols *sgper(1,1) + forc_soll *sgper(2,1) &
                 + forc_solsd*sgper(1,2) + forc_solld*sgper(2,2)

         sabv    = forc_sols *ssun (1,1) + forc_soll *ssun (2,1) &
                 + forc_solsd*ssun (1,2) + forc_solld*ssun (2,2)

         par     = forc_sols *ssun (1,1) + forc_solsd*ssun (1,2)

         ! LAI PAR
         !TODO: to distinguish lai and sai
         !par     = par * lai*(1.-rho(1,1)-tau(1,1)) / &
         !              ( lai*(1.-rho(1,1)-tau(1,1)) + &
         !                sai*(1.-rho(1,2)-tau(1,2)) )

         ! for lake
         sablake = forc_sols *slake(1,1) + forc_soll *slake(2,1) &
                 + forc_solsd*slake(1,2) + forc_solld*slake(2,2)

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

   END SUBROUTINE netsolar_urban

END MODULE MOD_Urban_NetSolar
! ---------- EOP ------------
