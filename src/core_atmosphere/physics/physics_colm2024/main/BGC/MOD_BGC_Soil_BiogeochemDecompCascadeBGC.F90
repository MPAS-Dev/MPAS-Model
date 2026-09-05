#include <define.h>
#ifdef BGC
MODULE MOD_BGC_Soil_BiogeochemDecompCascadeBGC

!---------------------------------------------------------------------------------------------------
! !DESCRIPTION:
! Calculate the soil decomposition rate according to soil temperature, soil matric potential, and depth
!
! !REFERENCES:
! Koven, C.D., Riley, W.J., Subin, Z.M., Tang, J.Y., Torn, M.S., Collins, W.D., Bonan, G.B., Lawrence,
! D.M. and Swenson, S.C., 2013. The effect of vertically resolved soil biogeochemistry and alternate
! soil C and N models on C dynamics of CLM4. Biogeosciences, 10(11), 7109-7131.
! Thornton, P.E., Law, B.E., Gholz, H.L., Clark, K.L., Falge, E., Ellsworth, D.S., Goldstein, A.H., Monson,
! R.K., Hollinger, D., Falk, M. and Chen, J., 2002. Modeling and measuring the effects of disturbance
! history and climate on carbon and water budgets in evergreen needleleaf forests.
! Agricultural and forest meteorology, 113(1-4), 185-222.
!
! !ORIGINAL:
! The Community Land Model version 5.0 (CLM5)
!
! !REVISION:
! Xingjie Lu, 2021, revised the CLM5 code to be compatible with CoLM code structure.

   USE MOD_Precision
   USE MOD_Vars_TimeInvariants, only: &
       Q10, smpmax_hr, smpmin_hr, tau_l1, tau_l2_l3, tau_s1, tau_s2, tau_s3, tau_cwd, froz_q10, &
       i_met_lit,i_cel_lit,i_lig_lit ,i_cwd,i_soil1,i_soil2,i_soil3
   USE MOD_Vars_TimeVariables, only: &
       smp, t_soisno, t_scalar, w_scalar, o_scalar, depth_scalar, decomp_k
   USE MOD_Vars_Global, only: PI

   IMPLICIT NONE

   PUBLIC decomp_rate_constants_bgc

CONTAINS

   SUBROUTINE decomp_rate_constants_bgc(i,nl_soil,z_soi)

   integer ,intent(in) :: i                 ! patch index
   integer ,intent(in) :: nl_soil           ! number of total soil layers
   real(r8),intent(in) :: z_soi(1:nl_soil)  ! depth of each soil layer

   real(r8) normalization_factor ! factor by which to offset the decomposition rates frm century to a q10 formulation
   real(r8),parameter :: decomp_depth_efolding = 10._r8
   real(r8) k_l1, k_l2_l3, k_s1, k_s2, k_s3, k_frag
   real(r8) psi
   integer j
   real(r8) catanf
   real(r8) catanf_30
   real(r8) t1

      catanf(t1) = 11.75_r8 +(29.7_r8 / PI) * atan( PI * 0.031_r8  * ( t1 - 15.4_r8 ))

      ! translate to per-second time constant
      k_l1 = 1._r8    / (86400._r8 * 365._r8 * tau_l1)
      k_l2_l3 = 1._r8 / (86400._r8 * 365._r8 * tau_l2_l3)
      k_s1 = 1._r8    / (86400._r8 * 365._r8 * tau_s1)
      k_s2 = 1._r8    / (86400._r8 * 365._r8 * tau_s2)
      k_s3 = 1._r8    / (86400._r8 * 365._r8 * tau_s3)
      k_frag = 1._r8  / (86400._r8 * 365._r8 * tau_cwd)

      ! calc ref rate
      catanf_30 = catanf(30._r8)

       DO j = 1, nl_soil
          IF (t_soisno(j,i) >= 273.15_r8) THEN
             t_scalar(j,i)= (Q10**((t_soisno(j,i)-(273.15_r8+25._r8))/10._r8))
          ELSE
             t_scalar(j,i)= (Q10**(-25._r8/10._r8))*(froz_q10**((t_soisno(j,i)-273.15_r8)/10._r8))
          ENDIF
       ENDDO

      ! calculate the rate constant scalar for soil water content.
      ! Uses the log relationship with water potential given in
      ! Andren, O., and K. Paustian, 1987. Barley straw decomposition in the field:
      ! a comparison of models. Ecology, 68(5):1190-1200.
      ! and supported by data in
      ! Orchard, V.A., and F.J. Cook, 1983. Relationship between soil respiration
      ! and soil moisture. Soil Biol. Biochem., 15(4):447-453.

      DO j = 1,nl_soil
         psi = min(smp(j,i),smpmax_hr)
         ! decomp only IF soilpsi is higher than minpsi
         IF (psi > smpmin_hr) THEN
            w_scalar(j,i) = (log(smpmin_hr/psi)/log(smpmin_hr/smpmax_hr))
         ELSE
            w_scalar(j,i) = 0.001_r8
         ! update froot_prof_p for root carbon turnover when soil is frozen
         ENDIF
      ENDDO

      o_scalar(1:nl_soil,i) = 1._r8

      ! scale all decomposition rates by a constant to compensate for offset between original CENTURY temp func and Q10
      normalization_factor = (catanf(15._r8)/catanf_30) / (Q10**((15._r8-25._r8)/10._r8))
      DO j = 1, nl_soil
         t_scalar(j,i) = t_scalar(j,i) * normalization_factor
      ENDDO

      DO j = 1, nl_soil
         depth_scalar(j,i) = exp(-z_soi(j)/decomp_depth_efolding)
      ENDDO

      DO j = 1, nl_soil
         decomp_k(j,i_met_lit,i) = k_l1    * t_scalar(j,i) * w_scalar(j,i) * depth_scalar(j,i) * o_scalar(j,i) !&
         decomp_k(j,i_cel_lit,i) = k_l2_l3 * t_scalar(j,i) * w_scalar(j,i) * depth_scalar(j,i) * o_scalar(j,i) !&
         decomp_k(j,i_lig_lit,i) = k_l2_l3 * t_scalar(j,i) * w_scalar(j,i) * depth_scalar(j,i) * o_scalar(j,i) !&
         decomp_k(j,i_soil1  ,i) = k_s1    * t_scalar(j,i) * w_scalar(j,i) * depth_scalar(j,i) * o_scalar(j,i) !&
         decomp_k(j,i_soil2  ,i) = k_s2    * t_scalar(j,i) * w_scalar(j,i) * depth_scalar(j,i) * o_scalar(j,i) !&
         decomp_k(j,i_soil3  ,i) = k_s3    * t_scalar(j,i) * w_scalar(j,i) * depth_scalar(j,i) * o_scalar(j,i) !&
      ENDDO

      DO j = 1,nl_soil
         decomp_k(j,i_cwd,i)   = k_frag  * t_scalar(j,i) * w_scalar(j,i) * depth_scalar(j,i) * &
                 o_scalar(j,i)
      ENDDO

   END SUBROUTINE decomp_rate_constants_bgc

END MODULE MOD_BGC_Soil_BiogeochemDecompCascadeBGC
#endif
