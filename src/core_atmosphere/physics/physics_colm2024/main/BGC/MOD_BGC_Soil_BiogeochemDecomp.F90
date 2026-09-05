#include <define.h>
#ifdef BGC
MODULE MOD_BGC_Soil_BiogeochemDecomp

!-----------------------------------------------------------------------------------------------------------
! !DESCRIPTION:
! This MODULE calculates the CN transfer fluxes between different soil and litter pools,
! which includes CN transfer fluxes (decomp_ctransfer or decomp_ntransfer), heterotrophic respiration (decomp_hr),
! net mineralisation and gross mineralisation. Denitrification flux will be also calculated when nitrification model
! is activated.
!
! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5.0)
!
! !REVISION:
! Xingjie Lu, 2021, revised original CLM5 code to be compatible with CoLM code structure.

   USE MOD_Precision
   USE MOD_Namelist, only: DEF_USE_NITRIF
   USE MOD_BGC_Vars_TimeInvariants, only: &
       floating_cn_ratio, initial_cn_ratio, dnp, rf_decomp, receiver_pool, donor_pool, i_atm

   USE MOD_BGC_Vars_TimeVariables, only: &
       ! decomposition carbon & nitrogen pools
       decomp_cpools_vr, decomp_npools_vr, &

       ! other variables
       cn_decomp_pools, fpi_vr

   USE MOD_BGC_Vars_1DFluxes, only: &
       ! decomposition fluxes variables
       decomp_sminn_flux_vr, decomp_hr_vr, decomp_ctransfer_vr, decomp_ntransfer_vr, &
       pmnf_decomp, p_decomp_cpool_loss, sminn_to_denit_decomp_vr, &
       net_nmin_vr, gross_nmin_vr, net_nmin, gross_nmin


   IMPLICIT NONE

   PUBLIC SoilBiogeochemDecomp

CONTAINS

   SUBROUTINE SoilBiogeochemDecomp(i,nl_soil,ndecomp_pools,ndecomp_transitions, dz_soi)

   integer ,intent(in) :: i                   ! patch index
   integer ,intent(in) :: nl_soil             ! number of total soil layers
   integer ,intent(in) :: ndecomp_pools       ! number of total soil & litter pools in the decompositions
   integer ,intent(in) :: ndecomp_transitions ! number of total transfers between soil and litter pools in the decomposition
   real(r8),intent(in) :: dz_soi(1:nl_soil)   ! thicknesses of each soil layer

   integer j,k,l
      ! calculate c:n ratios of applicable pools
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

      ! column loop to calculate actual immobilization and decomp rates, following
      ! resolution of plant/heterotroph  competition for mineral N

      ! upon RETURN from SoilBiogeochemCompetition, the fraction of potential immobilization
      ! has been set (soilbiogeochem_state_inst%fpi_vr_col). now finish the decomp calculations.
      ! only the immobilization steps are limited by fpi_vr (pmnf > 0)
      ! Also calculate denitrification losses as a simple proportion
      ! of mineralization flux.

      DO k = 1, ndecomp_transitions
         DO j = 1,nl_soil
            IF (decomp_cpools_vr(j,donor_pool(k),i) > 0._r8) THEN
               IF ( pmnf_decomp(j,k,i) > 0._r8 ) THEN
                  p_decomp_cpool_loss(j,k,i) = p_decomp_cpool_loss(j,k,i) * fpi_vr(j,i)
                  pmnf_decomp(j,k,i) = pmnf_decomp(j,k,i) * fpi_vr(j,i)
                  IF(.not. DEF_USE_NITRIF)THEN
                     sminn_to_denit_decomp_vr(j,k,i) = 0._r8
                  ENDIF
               ELSE
                  IF(.not. DEF_USE_NITRIF)THEN
                     sminn_to_denit_decomp_vr(j,k,i) = -dnp * pmnf_decomp(j,k,i)
                  ENDIF
               ENDIF
               decomp_hr_vr(j,k,i) = rf_decomp(j,k,i) * p_decomp_cpool_loss(j,k,i)
               decomp_ctransfer_vr(j,k,i) = (1._r8 - rf_decomp(j,k,i)) * p_decomp_cpool_loss(j,k,i)
               IF (decomp_npools_vr(j,donor_pool(k),i) > 0._r8 .and. receiver_pool(k) /= i_atm) THEN
                  decomp_ntransfer_vr(j,k,i) = p_decomp_cpool_loss(j,k,i) / cn_decomp_pools(j,donor_pool(k),i)
               ELSE
                  decomp_ntransfer_vr(j,k,i) = 0._r8
               ENDIF
               IF ( receiver_pool(k) /= 0 ) THEN
                  decomp_sminn_flux_vr(j,k,i) = pmnf_decomp(j,k,i)
               ELSE  ! keep sign convention negative for terminal pools
                  decomp_sminn_flux_vr(j,k,i) = - pmnf_decomp(j,k,i)
               ENDIF
               net_nmin_vr(j,i) = net_nmin_vr(j,i) - pmnf_decomp(j,k,i)
            ELSE
               decomp_ntransfer_vr(j,k,i) = 0._r8
               IF(.not. DEF_USE_NITRIF)THEN
                  sminn_to_denit_decomp_vr(j,k,i) = 0._r8
               ENDIF
               decomp_sminn_flux_vr(j,k,i) = 0._r8
            ENDIF

         ENDDO
      ENDDO

      DO j = 1,nl_soil
         net_nmin(i) = net_nmin(i) + net_nmin_vr(j,i) * dz_soi(j)
         gross_nmin(i) = gross_nmin(i) + gross_nmin_vr(j,i) * dz_soi(j)
      ENDDO

   END SUBROUTINE SoilBiogeochemDecomp

END MODULE MOD_BGC_Soil_BiogeochemDecomp
#endif
