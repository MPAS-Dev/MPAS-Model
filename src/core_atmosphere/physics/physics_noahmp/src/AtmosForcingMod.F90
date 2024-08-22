module AtmosForcingMod

!!! Process input atmospheric forcing variables

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine ProcessAtmosForcing(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: ATM
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local varibles
    integer                          :: LoopInd                ! loop index
    integer, parameter               :: LoopNum = 10           ! iterations for Twet calculation
    real(kind=kind_noahmp)           :: PrecipFrozenTot        ! total frozen precipitation [mm/s] ! MB/AN : v3.7
    real(kind=kind_noahmp)           :: RadDirFrac             ! direct solar radiation fraction
    real(kind=kind_noahmp)           :: RadVisFrac             ! visible band solar radiation fraction
    real(kind=kind_noahmp)           :: VapPresSat             ! saturated vapor pressure of air
    real(kind=kind_noahmp)           :: LatHeatVap             ! latent heat of vapor/sublimation
    real(kind=kind_noahmp)           :: PsychConst             ! (cp*p)/(eps*L), psychrometric coefficient
    real(kind=kind_noahmp)           :: TemperatureDegC        ! air temperature [C]
    real(kind=kind_noahmp)           :: TemperatureWetBulb     ! wetbulb temperature

! ------------------------------------------------------------------------
    associate(                                                                        &
              CosSolarZenithAngle     => noahmp%config%domain%CosSolarZenithAngle    ,& ! in,  cosine solar zenith angle [0-1]
              OptRainSnowPartition    => noahmp%config%nmlist%OptRainSnowPartition   ,& ! in,  rain-snow partition physics option
              PressureAirRefHeight    => noahmp%forcing%PressureAirRefHeight         ,& ! in,  air pressure [Pa] at reference height
              TemperatureAirRefHeight => noahmp%forcing%TemperatureAirRefHeight      ,& ! in,  air temperature [K] at reference height
              SpecHumidityRefHeight   => noahmp%forcing%SpecHumidityRefHeight        ,& ! in,  specific humidity [kg/kg] forcing at reference height
              PrecipConvRefHeight     => noahmp%forcing%PrecipConvRefHeight          ,& ! in,  convective precipitation rate [mm/s] at reference height
              PrecipNonConvRefHeight  => noahmp%forcing%PrecipNonConvRefHeight       ,& ! in,  non-convective precipitation rate [mm/s] at reference height
              PrecipShConvRefHeight   => noahmp%forcing%PrecipShConvRefHeight        ,& ! in,  shallow convective precipitation rate [mm/s] at reference height
              PrecipSnowRefHeight     => noahmp%forcing%PrecipSnowRefHeight          ,& ! in,  snowfall rate [mm/s] at reference height
              PrecipGraupelRefHeight  => noahmp%forcing%PrecipGraupelRefHeight       ,& ! in,  graupel rate [mm/s] at reference height
              PrecipHailRefHeight     => noahmp%forcing%PrecipHailRefHeight          ,& ! in,  hail rate [mm/s] at reference height
              RadSwDownRefHeight      => noahmp%forcing%RadSwDownRefHeight           ,& ! in,  downward shortwave radiation [W/m2] at reference height
              WindEastwardRefHeight   => noahmp%forcing%WindEastwardRefHeight        ,& ! in,  wind speed [m/s] in eastward direction at reference height
              WindNorthwardRefHeight  => noahmp%forcing%WindNorthwardRefHeight       ,& ! in,  wind speed [m/s] in northward direction at reference height
              SnowfallDensityMax      => noahmp%water%param%SnowfallDensityMax       ,& ! in,  maximum fresh snowfall density [kg/m3]
              TemperaturePotRefHeight => noahmp%energy%state%TemperaturePotRefHeight ,& ! out, surface potential temperature [K]
              PressureVaporRefHeight  => noahmp%energy%state%PressureVaporRefHeight  ,& ! out, vapor pressure air [Pa] at reference height
              DensityAirRefHeight     => noahmp%energy%state%DensityAirRefHeight     ,& ! out, density air [kg/m3]
              WindSpdRefHeight        => noahmp%energy%state%WindSpdRefHeight        ,& ! out, wind speed [m/s] at reference height
              RadSwDownDir            => noahmp%energy%flux%RadSwDownDir             ,& ! out, incoming direct solar radiation [W/m2]
              RadSwDownDif            => noahmp%energy%flux%RadSwDownDif             ,& ! out, incoming diffuse solar radiation [W/m2]
              RainfallRefHeight       => noahmp%water%flux%RainfallRefHeight         ,& ! out, rainfall [mm/s] at reference height
              SnowfallRefHeight       => noahmp%water%flux%SnowfallRefHeight         ,& ! out, liquid equivalent snowfall [mm/s] at reference height
              PrecipTotRefHeight      => noahmp%water%flux%PrecipTotRefHeight        ,& ! out, total precipitation [mm/s] at reference height
              PrecipConvTotRefHeight  => noahmp%water%flux%PrecipConvTotRefHeight    ,& ! out, total convective precipitation [mm/s] at reference height
              PrecipLargeSclRefHeight => noahmp%water%flux%PrecipLargeSclRefHeight   ,& ! out, large-scale precipitation [mm/s] at reference height
              PrecipAreaFrac          => noahmp%water%state%PrecipAreaFrac           ,& ! out, fraction of area receiving precipitation
              FrozenPrecipFrac        => noahmp%water%state%FrozenPrecipFrac         ,& ! out, frozen precipitation fraction
              SnowfallDensity         => noahmp%water%state%SnowfallDensity           & ! out, bulk density of snowfall [kg/m3]
             )
! ------------------------------------------------------------------------

    ! surface air variables
    TemperaturePotRefHeight = TemperatureAirRefHeight * &
                              (PressureAirRefHeight / PressureAirRefHeight) ** (ConstGasDryAir / ConstHeatCapacAir) 
    PressureVaporRefHeight  = SpecHumidityRefHeight * PressureAirRefHeight / (0.622 + 0.378*SpecHumidityRefHeight)
    DensityAirRefHeight     = (PressureAirRefHeight - 0.378*PressureVaporRefHeight) / &
                              (ConstGasDryAir * TemperatureAirRefHeight)

    ! downward solar radiation
    RadDirFrac = 0.7
    RadVisFrac = 0.5
    if ( CosSolarZenithAngle <= 0.0 ) RadSwDownRefHeight = 0.0                  ! filter by solar zenith angle
    RadSwDownDir(1) = RadSwDownRefHeight * RadDirFrac       * RadVisFrac        ! direct  vis
    RadSwDownDir(2) = RadSwDownRefHeight * RadDirFrac       * (1.0-RadVisFrac)  ! direct  nir
    RadSwDownDif(1) = RadSwDownRefHeight * (1.0-RadDirFrac) * RadVisFrac        ! diffuse vis
    RadSwDownDif(2) = RadSwDownRefHeight * (1.0-RadDirFrac) * (1.0-RadVisFrac)  ! diffuse nir

    ! precipitation
    PrecipTotRefHeight = PrecipConvRefHeight + PrecipNonConvRefHeight + PrecipShConvRefHeight
    if ( OptRainSnowPartition == 4 ) then
       PrecipConvTotRefHeight  = PrecipConvRefHeight + PrecipShConvRefHeight
       PrecipLargeSclRefHeight = PrecipNonConvRefHeight
    else
       PrecipConvTotRefHeight  = 0.10 * PrecipTotRefHeight
       PrecipLargeSclRefHeight = 0.90 * PrecipTotRefHeight
    endif

    ! fractional area that receives precipitation (see, Niu et al. 2005)
    PrecipAreaFrac = 0.0
    if ( (PrecipConvTotRefHeight+PrecipLargeSclRefHeight) > 0.0 ) then
       PrecipAreaFrac = (PrecipConvTotRefHeight + PrecipLargeSclRefHeight) / &
                        (10.0*PrecipConvTotRefHeight + PrecipLargeSclRefHeight)
    endif

    ! partition precipitation into rain and snow. Moved from CANWAT MB/AN: v3.7
    ! Jordan (1991)
    if ( OptRainSnowPartition == 1 ) then
       if ( TemperatureAirRefHeight > (ConstFreezePoint+2.5) ) then
          FrozenPrecipFrac = 0.0
       else
          if ( TemperatureAirRefHeight <= (ConstFreezePoint+0.5) ) then
             FrozenPrecipFrac = 1.0
          elseif ( TemperatureAirRefHeight <= (ConstFreezePoint+2.0) ) then
             FrozenPrecipFrac = 1.0 - (-54.632 + 0.2*TemperatureAirRefHeight)
          else
             FrozenPrecipFrac = 0.6
          endif
       endif
    endif

    ! BATS scheme
    if ( OptRainSnowPartition == 2 ) then
       if ( TemperatureAirRefHeight >= (ConstFreezePoint+2.2) ) then
          FrozenPrecipFrac = 0.0
       else
          FrozenPrecipFrac = 1.0
       endif
    endif

    ! Simple temperature scheme
    if ( OptRainSnowPartition == 3 ) then
       if ( TemperatureAirRefHeight >= ConstFreezePoint ) then
          FrozenPrecipFrac = 0.0
       else
          FrozenPrecipFrac = 1.0
       endif
    endif

    ! Use WRF microphysics output
    ! Hedstrom NR and JW Pomeroy (1998), Hydrol. Processes, 12, 1611-1625
    SnowfallDensity = min( SnowfallDensityMax, 67.92+51.25*exp((TemperatureAirRefHeight-ConstFreezePoint)/2.59) )   ! fresh snow density !MB/AN: change to MIN  
    if ( OptRainSnowPartition == 4 ) then
       PrecipFrozenTot = PrecipSnowRefHeight + PrecipGraupelRefHeight + PrecipHailRefHeight
       if ( (PrecipNonConvRefHeight > 0.0) .and. (PrecipFrozenTot > 0.0) ) then
          FrozenPrecipFrac  = min( 1.0, PrecipFrozenTot/PrecipNonConvRefHeight )
          FrozenPrecipFrac  = max( 0.0, FrozenPrecipFrac )
          SnowfallDensity   = SnowfallDensity     * (PrecipSnowRefHeight/PrecipFrozenTot)    + &
                              ConstDensityGraupel * (PrecipGraupelRefHeight/PrecipFrozenTot) + &
                              ConstDensityHail    * (PrecipHailRefHeight/PrecipFrozenTot)
       else
          FrozenPrecipFrac  = 0.0
       endif
    endif

    ! wet-bulb scheme (Wang et al., 2019 GRL), C.He, 12/18/2020
    if ( OptRainSnowPartition == 5 ) then
       TemperatureDegC = min( 50.0, max(-50.0,(TemperatureAirRefHeight-ConstFreezePoint)) )    ! Kelvin to degree Celsius with limit -50 to +50
       if ( TemperatureAirRefHeight > ConstFreezePoint ) then
          LatHeatVap = ConstLatHeatEvap
       else
          LatHeatVap = ConstLatHeatSublim
       endif
       PsychConst            = ConstHeatCapacAir * PressureAirRefHeight / (0.622 * LatHeatVap)
       TemperatureWetBulb    = TemperatureDegC - 5.0    ! first guess wetbulb temperature
       do LoopInd = 1, LoopNum
          VapPresSat         = 610.8 * exp( (17.27*TemperatureWetBulb) / (237.3+TemperatureWetBulb) )
          TemperatureWetBulb = TemperatureWetBulb - (VapPresSat - PressureVaporRefHeight) / PsychConst   ! Wang et al., 2019 GRL Eq.2
       enddo
       FrozenPrecipFrac      = 1.0 / (1.0 + 6.99e-5 * exp(2.0*(TemperatureWetBulb+3.97)))                ! Wang et al., 2019 GRL Eq. 1
    endif

    ! rain-snow partitioning
    RainfallRefHeight = PrecipTotRefHeight * (1.0 - FrozenPrecipFrac)
    SnowfallRefHeight = PrecipTotRefHeight * FrozenPrecipFrac

    ! wind speed at reference height for turbulence calculation
    WindSpdRefHeight = max(sqrt(WindEastwardRefHeight**2.0 + WindNorthwardRefHeight**2.0), 1.0)

    end associate

  end subroutine ProcessAtmosForcing

end module AtmosForcingMod
