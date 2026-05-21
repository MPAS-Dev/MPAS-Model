module ForcingVarType

!!! Define column (1-D) Noah-MP forcing variables
!!! Forcing variable initialization is done in ForcingVarInitMod.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

  use Machine

  implicit none
  save
  private

  type, public :: forcing_type

    real(kind=kind_noahmp) :: SpecHumidityRefHeight    ! Specific humidity [kg water vapor / kg moist air] forcing at reference height
    real(kind=kind_noahmp) :: TemperatureAirRefHeight  ! Air temperature [K] forcing at reference height
    real(kind=kind_noahmp) :: WindEastwardRefHeight    ! wind speed [m/s] in eastward dir at reference height
    real(kind=kind_noahmp) :: WindNorthwardRefHeight   ! wind speed [m/s] in northward dir at reference height
    real(kind=kind_noahmp) :: RadSwDownRefHeight       ! downward shortwave radiation [W/m2] at reference height
    real(kind=kind_noahmp) :: RadLwDownRefHeight       ! downward longwave radiation [W/m2] at reference height
    real(kind=kind_noahmp) :: PressureAirRefHeight     ! air pressure [Pa] at reference height
    real(kind=kind_noahmp) :: PressureAirSurface       ! air pressure [Pa] at surface-atmosphere interface (lowest atmos model boundary)
    real(kind=kind_noahmp) :: PrecipConvRefHeight      ! convective precipitation rate [mm/s] at reference height
    real(kind=kind_noahmp) :: PrecipNonConvRefHeight   ! non-convective precipitation rate [mm/s] at reference height
    real(kind=kind_noahmp) :: PrecipShConvRefHeight    ! shallow convective precipitation rate [mm/s] at reference height
    real(kind=kind_noahmp) :: PrecipSnowRefHeight      ! snowfall rate [mm/s] at reference height
    real(kind=kind_noahmp) :: PrecipGraupelRefHeight   ! graupel rate [mm/s] at reference height
    real(kind=kind_noahmp) :: PrecipHailRefHeight      ! hail rate [mm/s] at reference height
    real(kind=kind_noahmp) :: TemperatureSoilBottom    ! bottom boundary condition for soil temperature [K]

  end type forcing_type

end module ForcingVarType
