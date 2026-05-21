module PsychrometricVariableMod

!!! Compute psychrometric variables for canopy and ground

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine PsychrometricVariable(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: None (embedded in ENERGY subroutine)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! --------------------------------------------------------------------
    associate(                                                               &
              PressureAirRefHeight => noahmp%forcing%PressureAirRefHeight   ,& ! in,  air pressure [Pa] at reference height
              TemperatureCanopy    => noahmp%energy%state%TemperatureCanopy ,& ! in,  vegetation temperature [K]
              TemperatureGrd       => noahmp%energy%state%TemperatureGrd    ,& ! in,  ground temperature [K]
              LatHeatVapCanopy     => noahmp%energy%state%LatHeatVapCanopy  ,& ! out, latent heat of vaporization/subli [J/kg], canopy
              LatHeatVapGrd        => noahmp%energy%state%LatHeatVapGrd     ,& ! out, latent heat of vaporization/subli [J/kg], ground
              FlagFrozenCanopy     => noahmp%energy%state%FlagFrozenCanopy  ,& ! out, used to define latent heat pathway
              FlagFrozenGround     => noahmp%energy%state%FlagFrozenGround  ,& ! out, frozen ground (logical) to define latent heat pathway
              PsychConstCanopy     => noahmp%energy%state%PsychConstCanopy  ,& ! out, psychrometric constant [Pa/K], canopy
              PsychConstGrd        => noahmp%energy%state%PsychConstGrd      & ! out, psychrometric constant [Pa/K], ground
             )
! ----------------------------------------------------------------------

    ! for canopy  ! Barlage: add distinction between ground and vegetation in v3.6
    if ( TemperatureCanopy > ConstFreezePoint ) then
       LatHeatVapCanopy = ConstLatHeatEvap
       FlagFrozenCanopy = .false.
    else
       LatHeatVapCanopy = ConstLatHeatSublim
       FlagFrozenCanopy = .true.
    endif
    PsychConstCanopy    = ConstHeatCapacAir * PressureAirRefHeight / (0.622*LatHeatVapCanopy)

    ! for ground
    if ( TemperatureGrd > ConstFreezePoint ) then
       LatHeatVapGrd    = ConstLatHeatEvap
       FlagFrozenGround = .false.
    else
       LatHeatVapGrd    = ConstLatHeatSublim
       FlagFrozenGround = .true.
    endif
    PsychConstGrd       = ConstHeatCapacAir * PressureAirRefHeight / (0.622*LatHeatVapGrd)

    end associate

  end subroutine PsychrometricVariable

end module PsychrometricVariableMod
