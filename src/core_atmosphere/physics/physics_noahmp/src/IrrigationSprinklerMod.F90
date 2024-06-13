module IrrigationSprinklerMod

!!! Estimate irrigation water depth (m) based on sprinkler method 
!!! Reference: chapter 11 of NRCS, Part 623 National Engineering Handbook. 
!!! Irrigation water will be applied over the canopy, affecting  present soil moisture, 
!!! infiltration rate of the soil, and evaporative loss, which should be executed before canopy process.
 
  use Machine
  use CheckNanMod
  use NoahmpVarType
  use ConstantDefineMod
  use IrrigationInfilPhilipMod, only : IrrigationInfilPhilip

  implicit none

contains

  subroutine IrrigationSprinkler(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: SPRINKLER_IRRIGATION
! Original code: P. Valayamkunnath (NCAR) <prasanth@ucar.edu> (08/06/2020)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    logical                          :: FlagNan           ! NaN value flag: if NaN, return true
    real(kind=kind_noahmp)           :: InfilRateSfc      ! surface infiltration rate [m/s]
    real(kind=kind_noahmp)           :: IrriRateTmp       ! temporary irrigation rate [m/timestep]
    real(kind=kind_noahmp)           :: WindSpdTot        ! total wind speed [m/s]
    real(kind=kind_noahmp)           :: IrriLossTmp       ! temporary irrigation water loss [%]
    real(kind=kind_noahmp)           :: PressureVaporSat  ! satuarated vapor pressure [Pa]

! --------------------------------------------------------------------
    associate(                                                                       &
              MainTimeStep            => noahmp%config%domain%MainTimeStep          ,& ! in,    noahmp main time step [s]
              TemperatureAirRefHeight => noahmp%forcing%TemperatureAirRefHeight     ,& ! in,    air temperature [K] at reference height
              WindEastwardRefHeight   => noahmp%forcing%WindEastwardRefHeight       ,& ! in,    wind speed [m/s] in eastward direction at reference height
              WindNorthwardRefHeight  => noahmp%forcing%WindNorthwardRefHeight      ,& ! in,    wind speed [m/s] in northward direction at reference height
              PressureVaporRefHeight  => noahmp%energy%state%PressureVaporRefHeight ,& ! in,    vapor pressure air [Pa]
              IrriSprinklerRate       => noahmp%water%param%IrriSprinklerRate       ,& ! in,    sprinkler irrigation rate [mm/h]
              IrrigationFracSprinkler => noahmp%water%state%IrrigationFracSprinkler ,& ! in,    sprinkler irrigation fraction (0 to 1)
              SoilMoisture            => noahmp%water%state%SoilMoisture            ,& ! in,    total soil moisture [m3/m3]
              SoilLiqWater            => noahmp%water%state%SoilLiqWater            ,& ! in,    soil water content [m3/m3]
              HeatLatentIrriEvap      => noahmp%energy%flux%HeatLatentIrriEvap      ,& ! inout, latent heating due to sprinkler evaporation [W/m2]
              IrrigationAmtSprinkler  => noahmp%water%state%IrrigationAmtSprinkler  ,& ! inout, irrigation water amount [m] to be applied, Sprinkler
              EvapIrriSprinkler       => noahmp%water%flux%EvapIrriSprinkler        ,& ! inout, evaporation of irrigation water, sprinkler [mm/s]
              RainfallRefHeight       => noahmp%water%flux%RainfallRefHeight        ,& ! inout, rainfall [mm/s] at reference height
              IrrigationRateSprinkler => noahmp%water%flux%IrrigationRateSprinkler  ,& ! inout, rate of irrigation by sprinkler [m/timestep]
              IrriEvapLossSprinkler   => noahmp%water%flux%IrriEvapLossSprinkler    ,& ! inout, loss of irrigation water to evaporation,sprinkler [m/timestep]
              SoilIce                 => noahmp%water%state%SoilIce                  & ! out,   soil ice content [m3/m3]
             )
! ----------------------------------------------------------------------

    ! initialize
    SoilIce(:) = max(0.0, SoilMoisture(:)-SoilLiqWater(:))

    ! estimate infiltration rate based on Philips Eq.
    call IrrigationInfilPhilip(noahmp, MainTimeStep, InfilRateSfc)

    ! irrigation rate of sprinkler
    IrriRateTmp             = IrriSprinklerRate * (1.0/1000.0) * MainTimeStep / 3600.0              ! NRCS rate/time step - calibratable
    IrrigationRateSprinkler = min(InfilRateSfc*MainTimeStep, IrrigationAmtSprinkler, IrriRateTmp)   ! Limit irrigation rate to minimum of infiltration rate
                                                                                                    ! and to the NRCS recommended rate
    ! evaporative loss from droplets: Based on Bavi et al., (2009). Evaporation 
    ! losses from sprinkler irrigation systems under various operating 
    ! conditions. Journal of Applied Sciences, 9(3), 597-600.
    WindSpdTot       = sqrt((WindEastwardRefHeight**2.0) + (WindNorthwardRefHeight**2.0))
    PressureVaporSat = 610.8 * exp((17.27*(TemperatureAirRefHeight-273.15)) / (237.3+(TemperatureAirRefHeight-273.15)))

    if ( TemperatureAirRefHeight > 273.15 ) then ! Equation (3)
       IrriLossTmp   = 4.375 * (exp(0.106*WindSpdTot)) * (((PressureVaporSat-PressureVaporRefHeight)*0.01)**(-0.092)) * &
                       ((TemperatureAirRefHeight-273.15)**(-0.102))
    else ! Equation (4)
       IrriLossTmp   = 4.337 * (exp(0.077*WindSpdTot)) * (((PressureVaporSat-PressureVaporRefHeight)*0.01)**(-0.098))
    endif
    ! Old PGI Fortran compiler does not support ISNAN function
    call CheckRealNaN(IrriLossTmp, FlagNan)
    if ( FlagNan .eqv. .true. ) IrriLossTmp = 4.0                           ! In case if IrriLossTmp is NaN
    if ( (IrriLossTmp > 100.0) .or. (IrriLossTmp < 0.0) ) IrriLossTmp = 4.0 ! In case if IrriLossTmp is out of range

    ! Sprinkler water [m] for sprinkler fraction 
    IrrigationRateSprinkler    = IrrigationRateSprinkler * IrrigationFracSprinkler
    if ( IrrigationRateSprinkler >= IrrigationAmtSprinkler ) then
       IrrigationRateSprinkler = IrrigationAmtSprinkler
       IrrigationAmtSprinkler  = 0.0
    else
       IrrigationAmtSprinkler  = IrrigationAmtSprinkler - IrrigationRateSprinkler
    endif

    IrriEvapLossSprinkler      = IrrigationRateSprinkler * IrriLossTmp * (1.0/100.0)
    IrrigationRateSprinkler    = IrrigationRateSprinkler - IrriEvapLossSprinkler

    ! include sprinkler water to total rain for canopy process later
    RainfallRefHeight  = RainfallRefHeight + (IrrigationRateSprinkler * 1000.0 / MainTimeStep)

    ! cooling and humidification due to sprinkler evaporation, per m^2 calculation 
    HeatLatentIrriEvap = IrriEvapLossSprinkler * 1000.0 * ConstLatHeatEvap / MainTimeStep   ! heat used for evaporation [W/m2]
    EvapIrriSprinkler  = IrriEvapLossSprinkler * 1000.0 / MainTimeStep                      ! sprinkler evaporation [mm/s]

    end associate

  end subroutine IrrigationSprinkler

end module IrrigationSprinklerMod
