module WaterTableEquilibriumMod

!!! Calculate equilibrium water table depth (Niu et al., 2005)

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine WaterTableEquilibrium(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: ZWTEQ
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: IndSoil                           ! do-loop index
    integer, parameter               :: NumSoilFineLy = 100               ! no. of fine soil layers of 6m soil
    real(kind=kind_noahmp)           :: WatDeficitCoarse                  ! water deficit from coarse (4-L) soil moisture profile
    real(kind=kind_noahmp)           :: WatDeficitFine                    ! water deficit from fine (100-L) soil moisture profile
    real(kind=kind_noahmp)           :: ThickSoilFineLy                   ! layer thickness of the 100-L soil layers to 6.0 m
    real(kind=kind_noahmp)           :: TmpVar                            ! temporary variable
    real(kind=kind_noahmp), dimension(1:NumSoilFineLy) :: DepthSoilFineLy ! layer-bottom depth of the 100-L soil layers to 6.0 m

! --------------------------------------------------------------------
    associate(                                                                       &
              NumSoilLayer           => noahmp%config%domain%NumSoilLayer           ,& ! in,  number of soil layers
              DepthSoilLayer         => noahmp%config%domain%DepthSoilLayer         ,& ! in,  depth [m] of layer-bottom from soil surface
              ThicknessSnowSoilLayer => noahmp%config%domain%ThicknessSnowSoilLayer ,& ! in,  thickness of snow/soil layers [m]
              SoilLiqWater           => noahmp%water%state%SoilLiqWater             ,& ! in,  soil water content [m3/m3]
              SoilMoistureSat        => noahmp%water%param%SoilMoistureSat          ,& ! in,  saturated value of soil moisture [m3/m3]
              SoilMatPotentialSat    => noahmp%water%param%SoilMatPotentialSat      ,& ! in,  saturated soil matric potential [m]
              SoilExpCoeffB          => noahmp%water%param%SoilExpCoeffB            ,& ! in,  soil B parameter
              WaterTableDepth        => noahmp%water%state%WaterTableDepth           & ! out, water table depth [m]
             )
! ----------------------------------------------------------------------

    DepthSoilFineLy(1:NumSoilFineLy) = 0.0
    WatDeficitCoarse                 = 0.0
    do IndSoil = 1, NumSoilLayer
       WatDeficitCoarse = WatDeficitCoarse + (SoilMoistureSat(1) - SoilLiqWater(IndSoil)) * &
                                             ThicknessSnowSoilLayer(IndSoil)   ! [m]
    enddo

    ThickSoilFineLy = 3.0 * (-DepthSoilLayer(NumSoilLayer)) / NumSoilFineLy
    do IndSoil = 1, NumSoilFineLy
       DepthSoilFineLy(IndSoil) = float(IndSoil) * ThickSoilFineLy
    enddo

    WaterTableDepth = -3.0 * DepthSoilLayer(NumSoilLayer) - 0.001              ! initial value [m]

    WatDeficitFine = 0.0
    do IndSoil = 1, NumSoilFineLy
       TmpVar         = 1.0 + (WaterTableDepth - DepthSoilFineLy(IndSoil)) / SoilMatPotentialSat(1)
       WatDeficitFine = WatDeficitFine + SoilMoistureSat(1) * &
                                         (1.0 - TmpVar**(-1.0/SoilExpCoeffB(1))) * ThickSoilFineLy
       if ( abs(WatDeficitFine-WatDeficitCoarse) <= 0.01 ) then
          WaterTableDepth = DepthSoilFineLy(IndSoil)
          exit
       endif
    enddo

    end associate

  end subroutine WaterTableEquilibrium

end module WaterTableEquilibriumMod
