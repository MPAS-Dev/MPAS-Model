#include <define.h>

MODULE MOD_OrbCosazi

!-----------------------------------------------------------------------
   USE MOD_Precision
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: orb_cosazi
!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------

   FUNCTION orb_cosazi(calday, dlon, dlat, coszen)

   USE MOD_Precision
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   real(r8), intent(in) :: calday        !Julian cal day (1.xx to 365.xx)
   real(r8), intent(in) :: dlat          !Centered latitude (radians)
   real(r8), intent(in) :: dlon          !Centered longitude (radians)
   real(r8), intent(in) :: coszen        !cosine of sun zenith angle
   real(r8) :: orb_cosazi                !cosine of sun azimuth angle

!-------------------------- Local Variables ----------------------------
   real(r8) declin                       !Solar declination (radians)
   real(r8) eccf                         !Earth-sun distance factor (ie. (1/r)**2)
   real(r8) lambm                        !Lambda m, mean long of perihelion (rad)
   real(r8) lmm                          !Intermediate argument involving lambm
   real(r8) lamb                         !Lambda, the earths long of perihelion
   real(r8) invrho                       !Inverse normalized sun/earth distance
   real(r8) sinl                         !Sine of lmm
   real(r8) pi                           !3.14159265358979323846...
   real(r8), parameter :: &
            dayspy=365.0,               &!days per year
            ve=80.5,                    &!Calday of vernal equinox assumes Jan 1 = calday 1
            eccen=1.672393084E-2,       &!Eccentricity
            obliqr=0.409214646,         &!Earths obliquity in radians
            lambm0=-3.2625366E-2,       &!Mean long of perihelion at the vernal equinox (radians)
            mvelpp=4.92251015            !moving vernal equinox longitude of
                                         !perihelion plus pi (radians)
!-----------------------------------------------------------------------

      pi = 4.*atan(1.)
      lambm = lambm0 + (calday - ve)*2.*pi/dayspy
      lmm = lambm  - mvelpp

      sinl = sin(lmm)
      lamb = lambm + eccen*(2.*sinl + eccen*(1.25*sin(2.*lmm) &
           + eccen*((13.0/12.0)*sin(3.*lmm) - 0.25*sinl)))
      invrho = (1. + eccen*cos(lamb - mvelpp)) / (1. - eccen*eccen)

      declin = asin(sin(obliqr)*sin(lamb))
      eccf = invrho*invrho

      orb_cosazi = (-1*cos(declin)*cos(calday*2.0*pi+dlon) - &
         coszen*cos(dlat))/(sin(dlat)*sqrt(1-coszen*coszen))

      IF (orb_cosazi<-1) orb_cosazi = -1
      IF (orb_cosazi>1 ) orb_cosazi = 1

   END FUNCTION orb_cosazi

END MODULE MOD_OrbCosazi
! ---------- EOP ------------
