module NoahmpVarType

!!! Define column (1-D) Noah-MP model variable data types

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

  use ForcingVarType
  use ConfigVarType
  use EnergyVarType
  use WaterVarType
  use BiochemVarType

  implicit none
  save
  private

  type, public :: noahmp_type

    ! define specific variable types for Noah-MP
    type(forcing_type)  :: forcing
    type(config_type)   :: config
    type(energy_type)   :: energy
    type(water_type)    :: water
    type(biochem_type)  :: biochem

  end type noahmp_type

end module NoahmpVarType
