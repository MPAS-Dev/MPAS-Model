module HumiditySaturationMod

!!! Compute saturated surface specific humidity and changing rate to temperature

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine HumiditySaturation(TemperatureAir, PressureAir, MixingRatioSat, MixingRatioSatTempD)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: CALHUM
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    real(kind=kind_noahmp), intent(in)  :: TemperatureAir                                   ! air temperature (K)
    real(kind=kind_noahmp), intent(in)  :: PressureAir                                      ! air pressure (pa)
    real(kind=kind_noahmp), intent(out) :: MixingRatioSat                                   ! saturated mixing ratio (g/g)
    real(kind=kind_noahmp), intent(out) :: MixingRatioSatTempD                              ! d(MixingRatioSat)/d(T)

! local variable
    real(kind=kind_noahmp), parameter   :: Const1          = 17.67                          ! constant 1
    real(kind=kind_noahmp), parameter   :: TemperatureFrz  = 273.15                         ! freezing temperature 0degC [K]
    real(kind=kind_noahmp), parameter   :: Const2          = 29.65                          ! constant 2
    real(kind=kind_noahmp), parameter   :: ConstLatHeatVap = 2.501e6                        ! latent heat of vaporization [J/kg]
    real(kind=kind_noahmp), parameter   :: Const3          = Const1*(TemperatureFrz-Const2) ! constant 3
    real(kind=kind_noahmp), parameter   :: VapPresSatFrz   = 0.611                          ! vapor pressure at 0 degC [Pa]
    real(kind=kind_noahmp), parameter   :: GasConstWatVap  = 461.0                          ! specific gas constant for water vapor [J/kg/K]
    real(kind=kind_noahmp), parameter   :: RatioGasConst   = 0.622                          ! ratio of gas constant of dry air to water vapor
    real(kind=kind_noahmp)              :: VapPresSatTemp                                   ! saturated vapor pressure at air temperature [Pa]
    real(kind=kind_noahmp)              :: PressureAirKpa                                   ! air pressure in KPa unit 

! ----------------------------------------------------------------------

    ! calculated saturated vapor pressure at air temperature
    VapPresSatTemp = VapPresSatFrz * exp(ConstLatHeatVap / GasConstWatVap * &
                                        (1.0/TemperatureFrz - 1.0/TemperatureAir))

    ! convert PressureAir from Pa to KPa
    PressureAirKpa = PressureAir * 1.0e-3

    ! calculate saturated mixing ratio
    MixingRatioSat = RatioGasConst * VapPresSatTemp / (PressureAirKpa - VapPresSatTemp)

    ! convert from  g/g to g/kg
    MixingRatioSat = MixingRatioSat * 1.0e3

    ! MixingRatioSatTempD is calculated assuming MixingRatioSat is a specific humidity
    MixingRatioSatTempD = (MixingRatioSat / (1+MixingRatioSat)) * Const3 / (TemperatureAir-Const2)**2

    ! MixingRatioSat needs to be in g/g when returned for surface flux calculation
    MixingRatioSat = MixingRatioSat / 1.0e3

  end subroutine HumiditySaturation

end module HumiditySaturationMod
