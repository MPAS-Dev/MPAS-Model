module PsychrometricVariableGlacierMod

!!! Compute psychrometric variables for glacier ground

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine PsychrometricVariableGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: None (embedded in ENERGY_GLACIER subroutine)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                             &
              PressureAirRefHeight => noahmp%forcing%PressureAirRefHeight ,& ! in,  air pressure [Pa] at reference height
              LatHeatVapGrd        => noahmp%energy%state%LatHeatVapGrd   ,& ! out, latent heat of vaporization/subli [J/kg], ground
              PsychConstGrd        => noahmp%energy%state%PsychConstGrd    & ! out, psychrometric constant [Pa/K], ground
             )
! ----------------------------------------------------------------------

    LatHeatVapGrd = ConstLatHeatSublim
    PsychConstGrd = ConstHeatCapacAir * PressureAirRefHeight / (0.622 * LatHeatVapGrd)

    end associate

  end subroutine PsychrometricVariableGlacier

end module PsychrometricVariableGlacierMod
