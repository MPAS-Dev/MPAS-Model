module ForcingVarInitMod

!!! Initialize column (1-D) Noah-MP forcing variables
!!! Forcing variables should be first defined in ForcingVarType.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

  use Machine
  use NoahmpVarType

  implicit none

contains

!=== initialize with default values
  subroutine ForcingVarInitDefault(noahmp)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

    noahmp%forcing%SpecHumidityRefHeight    = undefined_real 
    noahmp%forcing%TemperatureAirRefHeight  = undefined_real
    noahmp%forcing%WindEastwardRefHeight    = undefined_real
    noahmp%forcing%WindNorthwardRefHeight   = undefined_real
    noahmp%forcing%RadLwDownRefHeight       = undefined_real
    noahmp%forcing%RadSwDownRefHeight       = undefined_real
    noahmp%forcing%PrecipConvRefHeight      = undefined_real
    noahmp%forcing%PrecipNonConvRefHeight   = undefined_real
    noahmp%forcing%PrecipShConvRefHeight    = undefined_real
    noahmp%forcing%PrecipSnowRefHeight      = undefined_real
    noahmp%forcing%PrecipGraupelRefHeight   = undefined_real
    noahmp%forcing%PrecipHailRefHeight      = undefined_real
    noahmp%forcing%PressureAirSurface       = undefined_real
    noahmp%forcing%PressureAirRefHeight     = undefined_real
    noahmp%forcing%TemperatureSoilBottom    = undefined_real

  end subroutine ForcingVarInitDefault

end module ForcingVarInitMod
