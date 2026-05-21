module GroundThermalPropertyMod

!!! Compute snow and soil thermal conductivity and heat capacity

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SnowThermalPropertyMod, only : SnowThermalProperty
  use SoilThermalPropertyMod, only : SoilThermalProperty

  implicit none

contains

  subroutine GroundThermalProperty(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: THERMOPROP
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: LoopInd    ! loop index

! --------------------------------------------------------------------
    associate(                                                                       &
              NumSoilLayer           => noahmp%config%domain%NumSoilLayer           ,& ! in,  number of soil layers
              SurfaceType            => noahmp%config%domain%SurfaceType            ,& ! in,  surface type 1-soil; 2-lake
              MainTimeStep           => noahmp%config%domain%MainTimeStep           ,& ! in,  main noahmp timestep [s]
              ThicknessSnowSoilLayer => noahmp%config%domain%ThicknessSnowSoilLayer ,& ! in,  thickness of snow/soil layers [m]
              NumSnowLayerNeg        => noahmp%config%domain%NumSnowLayerNeg        ,& ! in,  actual number of snow layers (negative)
              FlagUrban              => noahmp%config%domain%FlagUrban              ,& ! in,  logical flag for urban grid
              SnowDepth              => noahmp%water%state%SnowDepth                ,& ! in,  snow depth [m]
              TemperatureSoilSnow    => noahmp%energy%state%TemperatureSoilSnow     ,& ! in,  snow and soil layer temperature [K]
              ThermConductSoilSnow   => noahmp%energy%state%ThermConductSoilSnow    ,& ! out, thermal conductivity [W/m/K] for all soil & snow
              HeatCapacSoilSnow      => noahmp%energy%state%HeatCapacSoilSnow       ,& ! out, heat capacity [J/m3/K] for all soil & snow
              PhaseChgFacSoilSnow    => noahmp%energy%state%PhaseChgFacSoilSnow     ,& ! out, energy factor for soil & snow phase change
              HeatCapacVolSnow       => noahmp%energy%state%HeatCapacVolSnow        ,& ! out, snow layer volumetric specific heat [J/m3/K]
              ThermConductSnow       => noahmp%energy%state%ThermConductSnow        ,& ! out, snow layer thermal conductivity [W/m/K]
              HeatCapacVolSoil       => noahmp%energy%state%HeatCapacVolSoil        ,& ! out, soil layer volumetric specific heat [J/m3/K] 
              ThermConductSoil       => noahmp%energy%state%ThermConductSoil         & ! out, soil layer thermal conductivity [W/m/K]
             )
! ----------------------------------------------------------------------

    ! initialize
    HeatCapacSoilSnow    = 0.0
    ThermConductSoilSnow = 0.0

    ! compute snow thermal conductivity and heat capacity
    call SnowThermalProperty(noahmp)
    do LoopInd = NumSnowLayerNeg+1, 0
       ThermConductSoilSnow(LoopInd) = ThermConductSnow(LoopInd)
       HeatCapacSoilSnow(LoopInd)    = HeatCapacVolSnow(LoopInd)
    enddo

    ! compute soil thermal properties
    call SoilThermalProperty(noahmp)
    do LoopInd = 1, NumSoilLayer
       ThermConductSoilSnow(LoopInd) = ThermConductSoil(LoopInd)
       HeatCapacSoilSnow(LoopInd)    = HeatCapacVolSoil(LoopInd)
    enddo
    if ( FlagUrban .eqv. .true. ) then
       do LoopInd = 1, NumSoilLayer
          ThermConductSoilSnow(LoopInd) = 3.24
       enddo
    endif

    ! heat flux reduction effect from the overlying green canopy, adapted from 
    ! section 2.1.2 of Peters-Lidard et al. (1997, JGR, VOL 102(D4)).
    ! not in use because of the separation of the canopy layer from the ground.
    ! but this may represent the effects of leaf litter (Niu comments)
    ! ThermConductSoilSnow(1) = ThermConductSoilSnow(1) * EXP (SBETA * VegFracGreen)

    ! compute lake thermal properties (no consideration of turbulent mixing for this version)
    if ( SurfaceType == 2 ) then
       do LoopInd = 1, NumSoilLayer
          if ( TemperatureSoilSnow(LoopInd) > ConstFreezePoint) then
             HeatCapacSoilSnow(LoopInd)    = ConstHeatCapacWater
             ThermConductSoilSnow(LoopInd) = ConstThermConductWater  !+ KEDDY * ConstHeatCapacWater 
          else
             HeatCapacSoilSnow(LoopInd)    = ConstHeatCapacIce
             ThermConductSoilSnow(LoopInd) = ConstThermConductIce
          endif
       enddo
    endif

    ! combine a temporary variable used for melting/freezing of snow and frozen soil
    do LoopInd = NumSnowLayerNeg+1, NumSoilLayer
       PhaseChgFacSoilSnow(LoopInd) = MainTimeStep / (HeatCapacSoilSnow(LoopInd) * ThicknessSnowSoilLayer(LoopInd))
    enddo

    ! snow/soil interface
    if ( NumSnowLayerNeg == 0 ) then
       ThermConductSoilSnow(1) = (ThermConductSoilSnow(1)*ThicknessSnowSoilLayer(1) + 0.35*SnowDepth) / &
                                 (SnowDepth + ThicknessSnowSoilLayer(1))
    else
       ThermConductSoilSnow(1) = (ThermConductSoilSnow(1)*ThicknessSnowSoilLayer(1) + &
                                  ThermConductSoilSnow(0)*ThicknessSnowSoilLayer(0)) / &
                                 (ThicknessSnowSoilLayer(0) + ThicknessSnowSoilLayer(1))
    endif

    end associate

  end subroutine GroundThermalProperty

end module GroundThermalPropertyMod
