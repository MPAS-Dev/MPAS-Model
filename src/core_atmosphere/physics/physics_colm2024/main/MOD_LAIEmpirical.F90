#include <define.h>

MODULE MOD_LAIEmpirical

!-----------------------------------------------------------------------
   USE MOD_Precision
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: LAI_empirical


!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------


   SUBROUTINE LAI_empirical(ivt,nl_soil,rootfr,t,lai,sai,fveg,green)

!-----------------------------------------------------------------------
!  provides leaf and stem area parameters
!  Original author: Yongjiu Dai, 08/31/2002
!-----------------------------------------------------------------------

   USE MOD_Precision
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer,  intent(in)  :: ivt               !land cover type
   integer,  intent(in)  :: nl_soil           !number of soil layers

   real(r8), intent(in)  :: rootfr(1:nl_soil) !root fraction
   real(r8), intent(in)  :: t(1:nl_soil)      !soil temperature
   real(r8), intent(out) :: lai               !leaf area index
   real(r8), intent(out) :: sai               !Stem area index
   real(r8), intent(out) :: fveg              !fractional cover of vegetation
   real(r8), intent(out) :: green             !greenness

!-------------------------- Local Variables ----------------------------
   real(r8) f      !
   real(r8) roota  !accumulates root fraction
   integer jrt     !number of soil layers with 90% root fraction
   integer j       !number of soil layers

!-----------------------------------------------------------------------
#if (defined LULC_USGS)
! Maximum fractional cover of vegetation [-]
   real(r8), dimension(24), parameter :: &
   vegc=(/1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
          1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, &
          1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0 /)
! Maximum leaf area index, the numbers are based on the data of
! "worldwide historical estimates of leaf area index, 1932-2000" :
! http://www.daac.ornl.gov/global_vegetation/HistoricalLai/data"
   real(r8), dimension(24), parameter :: &
   xla=(/1.50, 3.29, 4.18, 3.50, 2.50, 3.60, 2.02, 1.53, &
         2.00, 0.85, 4.43, 4.42, 4.56, 3.95, 4.50, 0.00, &
         4.00, 3.63, 0.00, 0.64, 1.60, 1.00, 0.00, 0.00 /)
! Minimum leaf area index
   real(r8), dimension(24), parameter :: &
   xla0=(/1.00, 0.50, 0.50, 0.50, 1.00, 0.50, 0.50, 0.50, &
          0.50, 0.30, 0.50, 0.50, 4.00, 4.00, 4.00, 0.00, &
          3.00, 3.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00 /)
! Stem area index [-]
   real(r8), dimension(24), parameter :: &
   sai0=(/0.20, 0.20, 0.30, 0.30, 0.50, 0.50, 1.00, 0.50, &
          1.00, 0.50, 2.00, 2.00, 2.00, 2.00, 2.00, 0.00, &
          2.00, 2.00, 0.00, 0.10, 0.10, 0.10, 0.00, 0.00 /)
#elif (defined SIB2_CLASSIFICATION)
   real(r8), dimension(11), parameter :: &
   vegc=(/1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0/)
   real(r8), dimension(11), parameter :: &
   xla =(/4.8, 3.9, 5.6, 5.5, 4.6, 1.7, 1.3, 2.1, 3.6, 0.0, 0.0/)
   real(r8), dimension(11), parameter :: &
   xla0=(/4.0, 0.6, 0.5, 5.0, 0.5, 0.3, 0.6, 0.4, 0.2, 0.0, 0.0/)
   real(r8), dimension(11), parameter :: &
   sai0=(/1.6, 1.8, 1.6, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.0, 0.0/)
#elif (defined BATS_CLASSIFICATION)
   real(r8), dimension(19), parameter :: &
   vegc=(/1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0,&
          1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0/)
   real(r8), dimension(19), parameter :: &
   xla =(/5.1, 1.6, 4.8, 4.8, 4.8, 5.4, 4.8, 0.0, 3.6, 4.8,&
          0.6, 0.0, 4.8, 0.0, 0.0, 4.8, 4.8, 4.8, 4.8/)
   real(r8), dimension(19), parameter :: &
   xla0=(/0.425, 0.4, 4.0, 0.8, 0.8, 4.5, 0.4, 0.0, 0.3, 0.4,&
          0.05, 0.0, 0.4, 0.0, 0.0, 4.0, 0.8, 2.4, 2.4/)
   real(r8), dimension(19), parameter :: &
   sai0=(/0.425, 3.2, 1.6, 1.6, 1.6, 1.8, 1.6, 0.0, 0.3, 0.4,&
          0.2, 0.0, 1.6, 0.0, 0.0, 1.6, 1.6, 1.6, 1.6/)
#elif (defined OGE_CLASSIFICATION)

#else
!#elif(defined LULC_IGBP)
   real(r8), dimension(17), parameter :: &
   vegc=(/1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,&
          1.0, 1.0, 1.0, 1.0, 0.0, 1.0, 0.0/)
   real(r8), dimension(17), parameter :: &
   xla =(/4.8, 5.4, 4.8, 4.8, 4.7, 4.7, 1.6, 4.7, 4.8, 1.7,&
          4.6, 4.9, 3.8, 4.8, 0.0, 0.06, 0.0/)
   real(r8), dimension(17), parameter :: &
   xla0=(/4.0, 4.5, 0.8, 0.8, 2.2, 1.6, 0.15, 1.8, 0.9, 0.4,&
          0.4, 0.4, 0.9, 2.0, 0.0, 0.006, 0.0/)
   real(r8), dimension(17), parameter :: &
   sai0=(/1.6, 1.8, 1.6, 1.6, 1.5, 1.5, 0.45, 1.4, 1.6, 3.1,&
          1.6, 0.4, 1.1, 1.3, 0.0, 0.14, 0.0/)
#endif

!-----------------------------------------------------------------------
      roota = 0.
      jrt = 1
      DO j = 1, nl_soil
         roota = roota + rootfr(j)
         IF(roota>0.9)THEN
            jrt = j
            EXIT
         ENDIF
      ENDDO

! Adjust leaf area index for seasonal variation

      f = max(0.0,1.-0.0016*max(298.-t(jrt),0.0)**2)
      lai = xla(ivt) + (xla0(ivt)-xla(ivt))*(1.-f)

! Sum leaf area index and stem area index
      sai = sai0(ivt)

! Fractional vegetation cover
      fveg = vegc(ivt)

      green = 0.0
      IF(fveg > 0.) green = 1.0

   END SUBROUTINE LAI_empirical

END MODULE MOD_LAIEmpirical
