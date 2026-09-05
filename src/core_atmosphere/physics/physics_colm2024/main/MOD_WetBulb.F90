MODULE MOD_WetBulb

!-----------------------------------------------------------------------
   USE MOD_Precision
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: wetbulb


!-----------------------------------------------------------------------

   CONTAINS

!-----------------------------------------------------------------------


   SUBROUTINE wetbulb(t,p,q,twc)

!=======================================================================
!  Wet-bulb temperature
!
!  Yongjiu Dai, 07/2013
!=======================================================================

   USE MOD_Precision
   USE MOD_Const_Physical, only: tfrz, hvap, cpair
   USE MOD_Qsadv

   IMPLICIT NONE
   real(r8), intent(in) :: t     ! air temperature [K]
   real(r8), intent(in) :: p     ! atmos pressure [pa]
   real(r8), intent(in) :: q     ! air specific humidity [kg/kg]
   real(r8), intent(out) :: twc  ! wet bulb temperature [K]

   integer i
   real(r8) es, esdT, qs, qsdT, r, rws

! ----------------------------------------------------------
!     real(r8) tcair ! dry-bulb temperature in celsius
!     real(r8) bp    ! approximate average barometric pressure [mb]
!     real(r8) ea    ! water vapor pressure in air [mb]
!     real(r8) eas   ! saturated water vapor pressure in air [mb]
!     real(r8) delt  ! delt=eas*4278.63/((tcair+242.792)*(tcair+242.792))
!     real(r8) tav   ! average of dry-bulb temperature and wet bulb temperature in celsius
!     real(r8) eav   ! eav=2.7489E8*exp(-4278.63/(tav+242.792))
!     real(r8) rh    ! relative humidity
! ----------------------------------------------------------
!  WETBULB computes wet-bulb temperatures from dry-bulb (tkair) and
!  vapor pressure of air(ea). routine adapted from e. anderson, p. 188.
! ----------------------------------------------------------
!     CALL qsadv(t,p,es,esdT,qs,qsdT)
!     rh  = min(1.0,q/qs)
!     bp  = p/100.0             ! mb
!     eas = es/100.0            ! mb
!     ea  = eas                 ! mb
!     delt = esdT/100.          ! mb/K
!
!     tcair = t - tfrz
!
!*    eas = 2.7489e8*exp(-4278.63/(tcair+242.792))
!*    delt = eas*4278.63/((tcair+242.792)*(tcair+242.792))
!
!     DO i = 1, 3
!        twc  = delt*tcair+6.6e-4 *bp*tcair+7.59e-7*bp*tcair*tcair+ea-eas
!        twc  = twc/(delt+6.6e-4*bp+7.59e-7*bp*tcair)    ! in celsius
!
!        tav  = 0.5*(tcair+twc)+tfrz
!        CALL qsadv(tav,p,es,esdT,qs,qsdT)
!        eav  = es/100.
!        delt = esdT/100.
!
!*       tav  = 0.5*(tcair+twc)
!*       eav  = 2.7489e8*exp(-4278.63/(tav+242.792))
!*       delt = eav*4278.63/((tav+242.792)*(tav+242.792))
!     ENDDO
!     twc = twc + tfrz
! ----------------------------------------------------------

! ----------------------------------------------------------
! the defining equation for the wetbulb temp Twb is
!     f(Twb) = Twb-T - Lv/Cp [r-rs(Twb)] = 0,
! WHERE
!     T = the dry-bulb temp (K),
!     Lv = the latent heat of vaporization (J/kg/K),
!     Cp = the specific heat of air at constant pressure,
!     r = the water vapor mixing ratio [q/(1-q)],
!     rs(Twb) = the saturation mixing ratio at wetbulb temp.
! http://www.asp.ucar.edu/colloquium/1992/notes/paet1/node81.html
! ----------------------------------------------------------
      CALL qsadv(t,p,es,esdT,qs,qsdT)
      r = q/(1.0-q)
      IF (q >= qs) r = qs/(1.0-qs)
      twc = t
      DO i = 1, 6
         CALL qsadv(twc,p,es,esdT,qs,qsdT)
         rws= qs/(1.0-qs)
         twc = (twc + t + hvap/cpair*(r-rws))/2.0
      ENDDO

!*----------------------------------------------------------
!*wetbulb temp as air temp and relative humidity at standard sea level pressure.
!*valid for RH% (5%-99%), T (-20C-50C). R. Stull, 2011: Wet-bulb temperature form
!*relative humidity and air temperature. J. Appl. Meteor. and Climatol., vol 50, 2267-2269.
!*----------------------------------------------------------
!*    tcair = t - tfrz
!*    CALL qsadv(t,p,es,esdT,qs,qsdT)
!*    rh  = min(1.0,q/qs)
!*    twc = tcair*atan(0.151977*(rh*100.+8.313659)**0.5) &
!*        + atan(tcair+rh*100.)-atan(rh*100.-1.676331) &
!*        + 0.00391838*(rh*100.)**1.5*atan(0.023101*rh*100.)-4.686035
!*    twc = twc + tfrz
!*----------------------------------------------------------

   END SUBROUTINE wetbulb

END MODULE MOD_WetBulb
! ---------- EOP ------------
