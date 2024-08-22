module GroundThermalPropertyGlacierMod

!!! Compute snow and glacier ice thermal conductivity and heat capacity

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SnowThermalPropertyMod,       only : SnowThermalProperty
  use GlacierIceThermalPropertyMod, only : GlacierIceThermalProperty

  implicit none

contains

  subroutine GroundThermalPropertyGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: THERMOPROP_GLACIER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                :: LoopInd         ! loop index

! --------------------------------------------------------------------
    associate(                                                                       &
              NumSoilLayer           => noahmp%config%domain%NumSoilLayer           ,& ! in,  number of soil layers
              MainTimeStep           => noahmp%config%domain%MainTimeStep           ,& ! in,  main noahmp timestep [s]
              ThicknessSnowSoilLayer => noahmp%config%domain%ThicknessSnowSoilLayer ,& ! in,  thickness of snow/soil layers [m]
              NumSnowLayerNeg        => noahmp%config%domain%NumSnowLayerNeg        ,& ! in,  actual number of snow layers (negative)
              SnowDepth              => noahmp%water%state%SnowDepth                ,& ! in,  snow depth [m]
              ThermConductSoilSnow   => noahmp%energy%state%ThermConductSoilSnow    ,& ! out, thermal conductivity [W/m/K] for all soil & snow
              HeatCapacSoilSnow      => noahmp%energy%state%HeatCapacSoilSnow       ,& ! out, heat capacity [J/m3/K] for all soil & snow
              PhaseChgFacSoilSnow    => noahmp%energy%state%PhaseChgFacSoilSnow     ,& ! out, energy factor for soil & snow phase change
              HeatCapacVolSnow       => noahmp%energy%state%HeatCapacVolSnow        ,& ! out, snow layer volumetric specific heat [J/m3/K]
              ThermConductSnow       => noahmp%energy%state%ThermConductSnow        ,& ! out, snow layer thermal conductivity [W/m/K]
              HeatCapacGlaIce        => noahmp%energy%state%HeatCapacGlaIce         ,& ! out, glacier ice layer volumetric specific heat [J/m3/K]
              ThermConductGlaIce     => noahmp%energy%state%ThermConductGlaIce       & ! out, glacier ice layer thermal conductivity [W/m/K]
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

    ! compute glacier ice thermal properties (using Noah glacial ice approximations)
    call GlacierIceThermalProperty(noahmp)
    do LoopInd = 1, NumSoilLayer
       ThermConductSoilSnow(LoopInd) = ThermConductGlaIce(LoopInd)
       HeatCapacSoilSnow(LoopInd)    = HeatCapacGlaIce(LoopInd)
    enddo

    ! combine a temporary variable used for melting/freezing of snow and glacier ice
    do LoopInd = NumSnowLayerNeg+1, NumSoilLayer
       PhaseChgFacSoilSnow(LoopInd) = MainTimeStep / (HeatCapacSoilSnow(LoopInd)*ThicknessSnowSoilLayer(LoopInd))
    enddo

    ! snow/glacier ice interface
    if ( NumSnowLayerNeg == 0 ) then
       ThermConductSoilSnow(1) = (ThermConductSoilSnow(1)*ThicknessSnowSoilLayer(1) + 0.35*SnowDepth) / &
                                 (SnowDepth + ThicknessSnowSoilLayer(1))
    else
       ThermConductSoilSnow(1) = (ThermConductSoilSnow(1)*ThicknessSnowSoilLayer(1) + &
                                  ThermConductSoilSnow(0)*ThicknessSnowSoilLayer(0)) / &
                                 (ThicknessSnowSoilLayer(0) + ThicknessSnowSoilLayer(1))
    endif

    end associate

  end subroutine GroundThermalPropertyGlacier

end module GroundThermalPropertyGlacierMod
