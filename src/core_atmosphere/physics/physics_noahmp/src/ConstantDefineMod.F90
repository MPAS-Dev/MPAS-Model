module ConstantDefineMod

!!! Define Noah-MP constant variable values

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------
 
  use Machine

  implicit none
  save
  private

  ! define specific physical constants
  real(kind=kind_noahmp), public, parameter :: ConstGravityAcc          = 9.80616     ! acceleration due to gravity [m/s2]
  real(kind=kind_noahmp), public, parameter :: ConstStefanBoltzmann     = 5.67e-08    ! Stefan-Boltzmann constant [W/m2/K4]
  real(kind=kind_noahmp), public, parameter :: ConstVonKarman           = 0.40        ! von Karman constant
  real(kind=kind_noahmp), public, parameter :: ConstFreezePoint         = 273.16      ! freezing/melting temperature point [K]
  real(kind=kind_noahmp), public, parameter :: ConstLatHeatSublim       = 2.8440e06   ! latent heat of sublimation [J/kg]
  real(kind=kind_noahmp), public, parameter :: ConstLatHeatEvap         = 2.5104e06   ! latent heat of vaporization [J/kg]
  real(kind=kind_noahmp), public, parameter :: ConstLatHeatFusion       = 0.3336e06   ! latent heat of fusion of water [J/kg]
  real(kind=kind_noahmp), public, parameter :: ConstHeatCapacWater      = 4.188e06    ! specific heat capacity of water [J/m3/K]
  real(kind=kind_noahmp), public, parameter :: ConstHeatCapacIce        = 2.094e06    ! specific heat capacity of ice [J/m3/K]
  real(kind=kind_noahmp), public, parameter :: ConstHeatCapacAir        = 1004.64     ! specific heat capacity of dry air [J/kg/K]
  real(kind=kind_noahmp), public, parameter :: ConstThermConductWater   = 0.57        ! thermal conductivity of water [W/m/K]
  real(kind=kind_noahmp), public, parameter :: ConstThermConductIce     = 2.2         ! thermal conductivity of ice [W/m/K]
  real(kind=kind_noahmp), public, parameter :: ConstThermConductAir     = 0.023       ! thermal conductivity of air [W/m/K]
  real(kind=kind_noahmp), public, parameter :: ConstThermConductQuartz  = 7.7         ! thermal conductivity for quartz [W/m/K]
  real(kind=kind_noahmp), public, parameter :: ConstThermConductSoilOth = 2.0         ! thermal conductivity for other soil components [W/m/K]
  real(kind=kind_noahmp), public, parameter :: ConstGasDryAir           = 287.04      ! gas constant for dry air [J/kg/K]
  real(kind=kind_noahmp), public, parameter :: ConstGasWaterVapor       = 461.269     ! gas constant for water vapor [J/kg/K]
  real(kind=kind_noahmp), public, parameter :: ConstDensityWater        = 1000.0      ! density of water [kg/m3]
  real(kind=kind_noahmp), public, parameter :: ConstDensityIce          = 917.0       ! density of ice [kg/m3]
  real(kind=kind_noahmp), public, parameter :: ConstPI                  = 3.14159265  ! pi value
  real(kind=kind_noahmp), public, parameter :: ConstDensityGraupel      = 500.0       ! graupel bulk density [kg/m3]
  real(kind=kind_noahmp), public, parameter :: ConstDensityHail         = 917.0       ! hail bulk density [kg/m3]

end module ConstantDefineMod
