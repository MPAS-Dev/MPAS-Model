#include <define.h>
#ifdef BGC
MODULE MOD_BGC_Daylength

!-----------------------------------------------------------------------
! !DESCRIPTION:
! Computes day length and solar declination angle based on given latitude and date.
!
! ! ORIGINAL:
! The Community Land Model version 5.0 (CLM5.0)

! REVISION:
! Xingjie Lu, 2022, modify original CLM5 code to be compatible with CoLM code structure.

   USE MOD_Precision
   IMPLICIT NONE
   SAVE
   PRIVATE

   PUBLIC  :: daylength           ! function to compute daylength
   PRIVATE :: declin_angle        ! function to compute solar decliation angle
  !
  !-----------------------------------------------------------------------

CONTAINS

  !-----------------------------------------------------------------------
   real(r8) FUNCTION daylength(dlat, idate2)
!
! !DESCRIPTION:
! Computes daylength (in seconds)
!
! Latitude and solar declination angle should both be specified in radians. decl must
! be strictly less than pi/2; lat must be less than pi/2 within a small tolerance.
!
   real(r8), intent(in) :: dlat    ! latitude (degrees)
   integer , intent(in) :: idate2  ! day of the year

   real(r8),parameter :: PI = 4.*atan(1.)!
   ! !LOCAL VARIABLES:
   real(r8) :: my_lat             ! local version of lat, possibly adjusted slightly
   real(r8) :: temp               ! temporary variable
   real(r8) :: decl

   ! number of seconds per radian of hour-angle
   real(r8), parameter :: secs_per_radian = 13750.9871_r8

   ! epsilon for defining latitudes "near" the pole
   real(r8), parameter :: lat_epsilon = 10._r8 * epsilon(1._r8)

   ! Define an offset pole as slightly less than pi/2 to avoid problems with cos(lat) being negative
   real(r8), parameter :: pole = PI/2.0_r8
   real(r8), parameter :: offset_pole = pole - lat_epsilon
    !-----------------------------------------------------------------------

      decl=declin_angle(idate2)

      ! lat must be less than pi/2 within a small tolerance
      IF (abs(dlat/180*PI) >= (pole + lat_epsilon)) THEN
         daylength = -9999
         write(*,*)"error in latitude",dlat

      ! decl must be strictly less than pi/2
      ELSE IF (abs(decl) >= pole) THEN
         daylength = -9999
         write(*,*)"error in idate:",idate2

      ! normal case
      ELSE
         ! Ensure that latitude isn't too close to pole, to avoid problems with cos(lat) being negative
         my_lat = min(offset_pole, max(-1._r8 * offset_pole, dlat/180*PI))

         temp = -(sin(my_lat)*sin(decl))/(cos(my_lat) * cos(decl))
         temp = min(1._r8,max(-1._r8,temp))
         daylength = 2.0_r8 * secs_per_radian * acos(temp)
      ENDIF

   END FUNCTION daylength

   real(r8) FUNCTION declin_angle(idate2)

   integer ,intent(in) :: idate2  ! day of the year
   real(r8),parameter  :: PI = 4.*atan(1.)  ! circular constant

      declin_angle=-23.44_r8/180._r8*PI*cos(2*PI/365*(idate2+10))

   END FUNCTION declin_angle

END MODULE MOD_BGC_Daylength
#endif
