#include <define.h>

#ifdef BGC
MODULE MOD_BGC_CNAnnualUpdate

!------------------------------------------------------------------------------------------------------
! !DESCRIPTION:
! On the radiation time step, update annual summation variables mostly for phenology modules:
! annsum_potential_gpp : annual sum of potential gpp of last year is used to calculate the available
!                        retranslocation N
! annmax_retransn      : maximum of retranslocation N pool size of last year in a whole year, used to
!                        calculate the available rentranslocation N
! annavg_tref          : annual 2m air temperature of last year is used to calculate onset phenology
! annsum_npp           : annual NPP of last year is used to calculate the allocation partition.
!
! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5.0)

! !REFERENCES:
! Lawrence, D.M., Fisher, R.A., Koven, C.D., Oleson, K.W., Swenson, S.C., Bonan, G., Collier, N.,
! Ghimire, B., van Kampenhout, L., Kennedy, D. and Kluzek, E., 2019.
! The Community Land Model version 5: Description of new features, benchmarking,
! and impact of forcing uncertainty. Journal of Advances in Modeling Earth Systems, 11(12), 4245-4287.

! !REVISION:
! Xingjie Lu, 2022, modify original CLM5 to be compatible with CoLM code structure.


   USE MOD_Vars_PFTimeVariables, only: &
       tempsum_potential_gpp_p, tempmax_retransn_p, tempavg_tref_p, tempsum_npp_p, &
       annsum_potential_gpp_p , annmax_retransn_p , annavg_tref_p , annsum_npp_p

   USE MOD_TimeManager, only: isendofyear
   USE MOD_Precision

   IMPLICIT NONE

   PUBLIC CNAnnualUpdate

CONTAINS

   SUBROUTINE CNAnnualUpdate(i,ps,pe,deltim,idate)

   integer ,intent(in) :: i         ! patch index
   integer ,intent(in) :: ps        ! start pft index
   integer ,intent(in) :: pe        ! END pft index
   real(r8),intent(in) :: deltim    ! time step in seconds
   integer ,intent(in) :: idate(3)  ! date (year, days of year, seconds of the day)

  !!LOCAL VARIABLES:
   integer m


      IF (isendofyear(idate,deltim)) THEN

         DO m = ps, pe
       ! update annual plant ndemand accumulator
            annsum_potential_gpp_p(m)  = tempsum_potential_gpp_p(m)
            tempsum_potential_gpp_p(m) = 0._r8

       ! update annual total N retranslocation accumulator
            annmax_retransn_p(m)  = tempmax_retransn_p(m)
            tempmax_retransn_p(m) = 0._r8

       ! update annual average 2m air temperature accumulator
            annavg_tref_p(m)  = tempavg_tref_p(m)
            tempavg_tref_p(m) = 0._r8

       ! update annual NPP accumulator, convert to annual total
            annsum_npp_p(m)  = tempsum_npp_p(m) * deltim
            tempsum_npp_p(m) = 0._r8

         ENDDO

      ENDIF

   END SUBROUTINE CNAnnualUpdate

END MODULE MOD_BGC_CNAnnualUpdate

#endif
