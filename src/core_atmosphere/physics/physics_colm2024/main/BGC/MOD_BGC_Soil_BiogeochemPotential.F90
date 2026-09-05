#include <define.h>
#ifdef BGC
MODULE MOD_BGC_Soil_BiogeochemPotential

!---------------------------------------------------------------------------------------------------------------------------------
! !DESCRIPTION:
! This module calculates the potential C exit flux and the potential N immobilisation and mineralisation flux. The potential C exit flux
! (p_decomp_cpool_loss) equals the product of donor C pool size (decomp_cpools_vr) and transfer pathway fraction (pathfrac_decomp).
! The potential N immobilisation and mineralisation flux (pmnf_decomp) equals:
! the receiver's N demand to immobalize new carbon (p_decomp_cpool_loss * (1 - rf_decomp)/cn_decomp_pools(receiver)) minus actual N
! transfer (p_decomp_cpool_loss * cn_decomp_pools(donor))
! p_decomp_cpool_loss and pmnf_decomp are THEN used in bgc_soil_SoilBiogeochemDecompMod.F90
!
! !REFERENCES:
! Thornton, P.E., Law, B.E., Gholz, H.L., Clark, K.L., Falge, E., Ellsworth, D.S., Goldstein, A.H., Monson,
! R.K., Hollinger, D., Falk, M. and Chen, J., 2002. Modeling and measuring the effects of disturbance
! history and climate on carbon and water budgets in evergreen needleleaf forests.
! Agricultural and forest meteorology, 113(1-4), 185-222.
!
! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5.0)
!
! !REVISION:
! Xingjie Lu, 2021, revised original CLM5 code to be compatible with CoLM code structure.

   USE MOD_Precision
   USE MOD_BGC_Vars_TimeInvariants, only: &
       floating_cn_ratio, initial_cn_ratio, rf_decomp, receiver_pool, donor_pool, i_atm, pathfrac_decomp

   USE MOD_BGC_Vars_TimeVariables, only: &
       ! decomposition carbon & nitrogen pools
       decomp_cpools_vr, decomp_npools_vr, decomp_k, &

       ! other variables
       cn_decomp_pools

   USE MOD_BGC_Vars_1DFluxes, only: &
       ! decomposition fluxes variables
       pmnf_decomp, p_decomp_cpool_loss, gross_nmin_vr, &

       ! mineral N fluxes
       potential_immob_vr, phr_vr


   IMPLICIT NONE

   PUBLIC SoilBiogeochemPotential

CONTAINS

   SUBROUTINE SoilBiogeochemPotential(i,nl_soil,ndecomp_pools,ndecomp_transitions)

   integer ,intent(in) :: i                   ! patch index
   integer ,intent(in) :: nl_soil             ! number of total soil layers
   integer ,intent(in) :: ndecomp_pools       ! number of total soil & litter pools in the decompositions
   integer ,intent(in) :: ndecomp_transitions ! number of total transfers between soil and litter pools in the decomposition

   integer j,k,l
   real(r8) immob(1:nl_soil)
   real(r8) ratio

      p_decomp_cpool_loss(:, :, i) = 0._r8
      pmnf_decomp(:, :, i) = 0._r8

      ! column loop to calculate potential decomp rates and total immobilization demand

      !! calculate c:n ratios of applicable pools
      DO l = 1, ndecomp_pools
         IF ( floating_cn_ratio(l) ) THEN
            DO j = 1,nl_soil
               IF ( decomp_npools_vr(j,l,i) > 0._r8 ) THEN
                  cn_decomp_pools(j,l,i) = decomp_cpools_vr(j,l,i) / decomp_npools_vr(j,l,i)
               ENDIF
            ENDDO
         ELSE
            DO j = 1,nl_soil
               cn_decomp_pools(j,l,i) = initial_cn_ratio(l)
            ENDDO
         ENDIF
      ENDDO

      ! calculate the non-nitrogen-limited fluxes
      ! these fluxes include the  "/ dt" term to put them on a
      ! per second basis, since the rate constants have been
      ! calculated on a per timestep basis.

      DO k = 1, ndecomp_transitions
         DO j = 1,nl_soil

            IF (decomp_cpools_vr(j,donor_pool(k),i) > 0._r8 .and. &
                 decomp_k(j,donor_pool(k),i) > 0._r8 ) THEN
               p_decomp_cpool_loss(j,k,i) = decomp_cpools_vr(j,donor_pool(k),i) &
                    * decomp_k(j,donor_pool(k),i)  * pathfrac_decomp(j,k,i)
               IF ( .not. floating_cn_ratio(receiver_pool(k)) ) THEN  !! not transition of cwd to litter

                  IF (receiver_pool(k) /= i_atm ) THEN  ! not 100% respiration
                     ratio = 0._r8

                     IF (decomp_npools_vr(j,donor_pool(k),i) > 0._r8) THEN
                        ratio = cn_decomp_pools(j,receiver_pool(k),i)/cn_decomp_pools(j,donor_pool(k),i)
                     ENDIF

                     pmnf_decomp(j,k,i) = (p_decomp_cpool_loss(j,k,i) * (1.0_r8 - rf_decomp(j,k,i) - ratio) &
                          / cn_decomp_pools(j,receiver_pool(k),i) )

                  ELSE   ! 100% respiration
                     pmnf_decomp(j,k,i) = - p_decomp_cpool_loss(j,k,i) / cn_decomp_pools(j,donor_pool(k),i)
                  ENDIF

               ELSE   ! CWD -> litter
                  pmnf_decomp(j,k,i) = 0._r8
               ENDIF
            ENDIF

         ENDDO
      ENDDO

      ! Sum up all the potential immobilization fluxes (positive pmnf flux)
      ! and all the mineralization fluxes (negative pmnf flux)
      DO j = 1,nl_soil
         immob(j) = 0._r8
      ENDDO
      DO k = 1, ndecomp_transitions
         DO j = 1,nl_soil
            IF (pmnf_decomp(j,k,i) > 0._r8) THEN
               immob(j) = immob(j) + pmnf_decomp(j,k,i)
            ELSE
               gross_nmin_vr(j,i) = gross_nmin_vr(j,i) - pmnf_decomp(j,k,i)
            ENDIF
         ENDDO
      ENDDO

      DO j = 1,nl_soil
         potential_immob_vr(j,i) = immob(j)
      ENDDO

      ! Add up potential hr for methane calculations
      DO j = 1,nl_soil
         phr_vr(j,i) = 0._r8
      ENDDO
      DO k = 1, ndecomp_transitions
         DO j = 1,nl_soil
            phr_vr(j,i) = phr_vr(j,i) + rf_decomp(j,k,i) * p_decomp_cpool_loss(j,k,i)
         ENDDO
      ENDDO

   END SUBROUTINE SoilBiogeochemPotential

END MODULE MOD_BGC_Soil_BiogeochemPotential
#endif
