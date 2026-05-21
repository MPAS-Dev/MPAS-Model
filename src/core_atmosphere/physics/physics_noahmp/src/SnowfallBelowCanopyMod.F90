module SnowfallBelowCanopyMod

!!! Snowfall process after canopy interception
!!! Update snow water equivalent and snow depth

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SnowfallAfterCanopyIntercept(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: SNOWFALL
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: IndNewSnowLayer  ! 0-no new layers, 1-creating new layers

! --------------------------------------------------------------------
    associate(                                                                        &
              MainTimeStep            => noahmp%config%domain%MainTimeStep           ,& ! in,    noahmp main time step [s]
              SnowfallGround          => noahmp%water%flux%SnowfallGround            ,& ! in,    snowfall rate at ground [mm/s]
              SnowDepthIncr           => noahmp%water%flux%SnowDepthIncr             ,& ! in,    snow depth increasing rate [m/s] due to snowfall
              TemperatureAirRefHeight => noahmp%forcing%TemperatureAirRefHeight      ,& ! in,    air temperature [K] at reference height
              NumSnowLayerNeg         => noahmp%config%domain%NumSnowLayerNeg        ,& ! inout, actual number of snow layers (negative)
              ThicknessSnowSoilLayer  => noahmp%config%domain%ThicknessSnowSoilLayer ,& ! inout, thickness of snow/soil layers [m]
              SnowDepth               => noahmp%water%state%SnowDepth                ,& ! inout, snow depth [m]
              SnowWaterEquiv          => noahmp%water%state%SnowWaterEquiv           ,& ! inout, snow water equivalent [mm]
              SnowIce                 => noahmp%water%state%SnowIce                  ,& ! inout, snow layer ice [mm]
              SnowLiqWater            => noahmp%water%state%SnowLiqWater             ,& ! inout, snow layer liquid water [mm]
              TemperatureSoilSnow     => noahmp%energy%state%TemperatureSoilSnow      & ! inout, snow and soil layer temperature [K]
             ) 
! ----------------------------------------------------------------------

    IndNewSnowLayer = 0

    ! shallow snow / no layer
    if ( (NumSnowLayerNeg == 0) .and. (SnowfallGround > 0.0) ) then
       SnowDepth      = SnowDepth + SnowDepthIncr * MainTimeStep
       SnowWaterEquiv = SnowWaterEquiv + SnowfallGround * MainTimeStep
    endif

    ! creating a new layer
    !if ( (NumSnowLayerNeg == 0)  .and. (SnowfallGround > 0.0) .and. (SnowDepth >= 0.05) ) then
    !if ( (NumSnowLayerNeg == 0)  .and. (SnowfallGround > 0.0) .and. (SnowDepth >= 0.025) ) then !MB: change limit
    ! C.He: remove SnowfallGround > 0.0 to allow adjusting snow layer number based on SnowDepth when no snowfall
    if ( (NumSnowLayerNeg == 0) .and. (SnowDepth >= 0.025) ) then
       NumSnowLayerNeg           = -1
       IndNewSnowLayer           =  1
       ThicknessSnowSoilLayer(0) = SnowDepth
       SnowDepth                 = 0.0
       TemperatureSoilSnow(0)    = min(273.16, TemperatureAirRefHeight)   ! temporary setup
       SnowIce(0)                = SnowWaterEquiv
       SnowLiqWater(0)           = 0.0
    endif

    ! snow with layers
    if ( (NumSnowLayerNeg < 0) .and. (IndNewSnowLayer == 0) .and. (SnowfallGround > 0.0) ) then
       SnowIce(NumSnowLayerNeg+1) = SnowIce(NumSnowLayerNeg+1) + SnowfallGround * MainTimeStep
       ThicknessSnowSoilLayer(NumSnowLayerNeg+1) = ThicknessSnowSoilLayer(NumSnowLayerNeg+1) + &
                                                   SnowDepthIncr * MainTimeStep
    endif

    end associate

  end subroutine SnowfallAfterCanopyIntercept

end module SnowfallBelowCanopyMod
