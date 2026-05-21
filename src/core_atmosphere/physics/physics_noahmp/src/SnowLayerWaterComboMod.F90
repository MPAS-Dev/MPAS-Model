module SnowLayerWaterComboMod

!!! Update snow water and temperature for combined snowpack layer

  use Machine
  use ConstantDefineMod

  implicit none

contains

  subroutine SnowLayerWaterCombo(ThickLayer1, LiqLayer1, IceLayer1, TempLayer1, &
                                 ThickLayer2, LiqLayer2, IceLayer2, TempLayer2)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: COMBO
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

! IN and OUT variables
    real(kind=kind_noahmp), intent(in)    :: ThickLayer2        ! nodal thickness of 2 elements being combined [m]
    real(kind=kind_noahmp), intent(in)    :: LiqLayer2          ! liquid water of element 2 [kg/m2]
    real(kind=kind_noahmp), intent(in)    :: IceLayer2          ! ice of element 2 [kg/m2]
    real(kind=kind_noahmp), intent(in)    :: TempLayer2         ! nodal temperature of element 2 [K]
    real(kind=kind_noahmp), intent(inout) :: ThickLayer1        ! nodal thickness of 1 elements being combined [m]
    real(kind=kind_noahmp), intent(inout) :: LiqLayer1          ! liquid water of element 1
    real(kind=kind_noahmp), intent(inout) :: IceLayer1          ! ice of element 1 [kg/m2]
    real(kind=kind_noahmp), intent(inout) :: TempLayer1         ! node temperature of element 1 [K]

! local variable
    real(kind=kind_noahmp)                :: ThickLayerComb     ! total thickness of nodes 1 and 2
    real(kind=kind_noahmp)                :: LiqLayerComb       ! combined liquid water [kg/m2]
    real(kind=kind_noahmp)                :: IceLayerComb       ! combined ice [kg/m2]
    real(kind=kind_noahmp)                :: TempLayerComb      ! combined node temperature [K]
    real(kind=kind_noahmp)                :: EnthLayer1         ! enthalpy of element 1 [J/m2]
    real(kind=kind_noahmp)                :: EnthLayer2         ! enthalpy of element 2 [J/m2]
    real(kind=kind_noahmp)                :: EnthLayerComb      ! combined enthalpy [J/m2]

! ----------------------------------------------------------------------

    ThickLayerComb = ThickLayer1 + ThickLayer2
    IceLayerComb   = IceLayer1 + IceLayer2
    LiqLayerComb   = LiqLayer1 + LiqLayer2
    EnthLayer1     = (ConstHeatCapacIce*IceLayer1  + ConstHeatCapacWater*LiqLayer1) * &
                     (TempLayer1-ConstFreezePoint) + ConstLatHeatFusion*LiqLayer1
    EnthLayer2     = (ConstHeatCapacIce*IceLayer2  + ConstHeatCapacWater*LiqLayer2) * &
                     (TempLayer2-ConstFreezePoint) + ConstLatHeatFusion*LiqLayer2

    EnthLayerComb  = EnthLayer1 + EnthLayer2
    if ( EnthLayerComb < 0.0 ) then
       TempLayerComb = ConstFreezePoint + EnthLayerComb / &
                       (ConstHeatCapacIce*IceLayerComb + ConstHeatCapacWater*LiqLayerComb)
    else if ( EnthLayerComb <= (ConstLatHeatFusion*LiqLayerComb) ) then
       TempLayerComb = ConstFreezePoint
    else
       TempLayerComb = ConstFreezePoint + (EnthLayerComb-ConstLatHeatFusion*LiqLayerComb) / &
                       (ConstHeatCapacIce*IceLayerComb + ConstHeatCapacWater*LiqLayerComb)
    endif

    ThickLayer1 = ThickLayerComb
    IceLayer1   = IceLayerComb
    LiqLayer1   = LiqLayerComb
    TempLayer1  = TempLayerComb

  end subroutine SnowLayerWaterCombo

end module SnowLayerWaterComboMod
