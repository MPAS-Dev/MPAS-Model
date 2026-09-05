#include <define.h>

MODULE MOD_Hydro_SoilFunction

!----------------------------------------------------------------------------
! Description:
!
!    Soil function type 1:
!    Campbell model
!    CAMPBELL, G. S. (1974), Soil Science, 117(6), 311-314.
!
!    Soil function type 2:
!    Modified van Genuchten & Mualem model by introducing an air-entry value
!    Ippisch et al. (2006), Advances in Water Resources, 29(12), 1780-1789.
!
! Created by Shupeng Zhang, 2022.
!----------------------------------------------------------------------------

   USE MOD_Precision

   IMPLICIT NONE

   real(r8), parameter :: minsmp = -1.e8

   PUBLIC :: get_derived_parameters_vGM

   PUBLIC :: soil_psi_from_vliq
   PUBLIC :: soil_hk_from_psi
   PUBLIC :: soil_vliq_from_psi

CONTAINS

   !-------------------------------------
   SUBROUTINE get_derived_parameters_vGM ( &
         psi_s, alpha_vgm, n_vgm, sc_vgm, fc_vgm)

   real(r8), intent(in) :: psi_s
   real(r8), intent(in) :: alpha_vgm
   real(r8), intent(in) :: n_vgm

   real(r8), intent(out) :: sc_vgm
   real(r8), intent(out) :: fc_vgm

   ! Local variables
   real(r8) :: m_vgm

      m_vgm = 1.0_r8 - 1.0_r8 / n_vgm
      sc_vgm = (1.0_r8 + (- alpha_vgm * psi_s)**n_vgm) ** (-m_vgm)
      fc_vgm = 1.0_r8 - (1.0_r8 - sc_vgm ** (1.0_r8/m_vgm)) ** m_vgm

   END SUBROUTINE get_derived_parameters_vGM

   !------------------------------------------------------------------
   real(r8) FUNCTION soil_hk_from_psi (psi, &
         psi_s, hksat, nprm, prms)

   IMPLICIT NONE

   real(r8), intent(in) :: psi

   real(r8), intent(in) :: psi_s
   real(r8), intent(in) :: hksat

   integer,  intent(in) :: nprm
   real(r8), intent(in) :: prms(nprm)

   ! Local variables
   real(r8) :: m_vgm, esat

      IF (psi >= psi_s) THEN
         soil_hk_from_psi = hksat
         RETURN
      ENDIF

#ifdef Campbell_SOIL_MODEL
      ! bsw => prms(1)
      soil_hk_from_psi = hksat * (psi / psi_s)**(- 3.0_r8 / prms(1) - 2.0_r8)
#endif

#ifdef vanGenuchten_Mualem_SOIL_MODEL
      ! alpha_vgm => prms(1), n_vgm => prms(2), L_vgm => prms(3), sc_vgm => prms(4), fc_vgm => prms(5)
      m_vgm = 1.0_r8 - 1.0_r8 / prms(2)
      esat = (1.0_r8 + (- prms(1) * psi)**(prms(2)))**(-m_vgm) / prms(4)
      soil_hk_from_psi = hksat * esat**prms(3) &
         * ((1.0_r8 - (1.0_r8 - (esat*prms(4))**(1.0_r8/m_vgm))**m_vgm) / prms(5))**2.0_r8
#endif

   END FUNCTION soil_hk_from_psi


   !-----------------------------------------------------------------
   real(r8) FUNCTION soil_psi_from_vliq (vliq, &
         porsl, vl_r, psi_s, nprm, prms)

   IMPLICIT NONE

   real(r8), intent(in) :: vliq

   real(r8), intent(in) :: porsl
   real(r8), intent(in) :: vl_r
   real(r8), intent(in) :: psi_s

   integer,  intent(in) :: nprm
   real(r8), intent(in) :: prms(nprm)

   ! Local variables
   real(r8) :: esat, m_vgm

      IF (vliq >= porsl) THEN
         soil_psi_from_vliq = psi_s
         RETURN
      ELSEIF (vliq <= max(vl_r,1.0e-8)) THEN
         soil_psi_from_vliq = minsmp
         RETURN
      ENDIF

#ifdef Campbell_SOIL_MODEL
      ! bsw => prms(1)
      soil_psi_from_vliq = psi_s * (vliq / porsl)**(-prms(1))
#endif

#ifdef vanGenuchten_Mualem_SOIL_MODEL
      ! alpha_vgm => prms(1), n_vgm => prms(2), L_vgm => prms(3), sc_vgm => prms(4), fc_vgm => prms(5)
      m_vgm = 1.0_r8 - 1.0_r8 / prms(2)
      esat = (vliq - vl_r) / (porsl - vl_r)
      soil_psi_from_vliq = - ((esat*prms(4))**(- 1.0_r8/m_vgm) - 1.0_r8)**(1.0_r8/prms(2)) &
         / prms(1)
#endif

      soil_psi_from_vliq = max(soil_psi_from_vliq, minsmp)


   END FUNCTION soil_psi_from_vliq

   !------------------------------------------------------------------
   real(r8) FUNCTION soil_vliq_from_psi (psi, &
         porsl, vl_r, psi_s, nprm, prms)

   IMPLICIT NONE

   real(r8), intent(in) :: psi

   real(r8), intent(in) :: porsl
   real(r8), intent(in) :: vl_r
   real(r8), intent(in) :: psi_s

   integer,  intent(in) :: nprm
   real(r8), intent(in) :: prms(nprm)

   ! Local variables
   real(r8) :: esat, m_vgm

      IF (psi >= psi_s) THEN
         soil_vliq_from_psi = porsl
         RETURN
      ENDIF

#ifdef Campbell_SOIL_MODEL
      ! bsw => prms(1)
      soil_vliq_from_psi = porsl * (psi / psi_s)**(-1.0/prms(1))
#endif

#ifdef vanGenuchten_Mualem_SOIL_MODEL
      ! alpha_vgm => prms(1), n_vgm => prms(2), L_vgm => prms(3), sc_vgm => prms(4), fc_vgm => prms(5)
      m_vgm = 1.0_r8 - 1.0_r8 / prms(2)
      esat = (1.0_r8 + (psi * (-prms(1)))**(prms(2))) ** (-m_vgm) / prms(4)
      soil_vliq_from_psi = (porsl - vl_r) * esat + vl_r
#endif

   END FUNCTION soil_vliq_from_psi


END MODULE MOD_Hydro_SoilFunction
