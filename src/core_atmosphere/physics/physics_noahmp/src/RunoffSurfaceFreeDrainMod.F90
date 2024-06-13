module RunoffSurfaceFreeDrainMod

!!! Calculate inflitration rate at soil surface and surface runoff for free drainage scheme

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SoilHydraulicPropertyMod, only : SoilDiffusivityConductivityOpt2

  implicit none

contains

  subroutine RunoffSurfaceFreeDrain(noahmp, TimeStep)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: INFIL
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

! IN & OUT variabls
    type(noahmp_type)     , intent(inout) :: noahmp
    real(kind=kind_noahmp), intent(in)    :: TimeStep                      ! timestep (may not be the same as model timestep)

! local variable
    integer                :: IndSoilFrz                                   ! number of interaction
    integer                :: LoopInd1, LoopInd2,  LoopInd3                ! do-loop index
    integer, parameter     :: FrzSoilFac = 3                               ! frozen soil pre-factor
    real(kind=kind_noahmp) :: FracVoidRem                                  ! remaining fraction
    real(kind=kind_noahmp) :: SoilWatHoldMaxRem                            ! remaining accumulated maximum holdable soil water [m]
    real(kind=kind_noahmp) :: WaterInSfc                                   ! surface in water [m]
    real(kind=kind_noahmp) :: TimeStepDay                                  ! time indices
    real(kind=kind_noahmp) :: SoilWatHoldMaxAcc                            ! accumulated maximum holdable soil water [m]
    real(kind=kind_noahmp) :: SoilIceWatTmp                                ! maximum soil ice water [m]
    real(kind=kind_noahmp) :: SoilImpervFrac                               ! impervious fraction due to frozen soil
    real(kind=kind_noahmp) :: IndAcc                                       ! accumulation index
    real(kind=kind_noahmp) :: SoilIceCoeff                                 ! soil ice coefficient
    real(kind=kind_noahmp) :: SoilWatDiffusivity                           ! soil water diffusivity [m2/s]
    real(kind=kind_noahmp) :: SoilWatConductivity                          ! soil water conductivity [m/s]
    real(kind=kind_noahmp) :: SoilWatHoldCap                               ! soil moisture holding capacity [m3/m3]
    real(kind=kind_noahmp) :: InfilRateMax                                 ! maximum infiltration rate [m/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilWatMaxHold    ! maximum soil water that can hold [m]

! --------------------------------------------------------------------
    associate(                                                               &
              NumSoilLayer        => noahmp%config%domain%NumSoilLayer      ,& ! in,  number of soil layers
              DepthSoilLayer      => noahmp%config%domain%DepthSoilLayer    ,& ! in,  depth [m] of layer-bottom from soil surface
              FlagUrban           => noahmp%config%domain%FlagUrban         ,& ! in,  logical flag for urban grid
              SoilLiqWater        => noahmp%water%state%SoilLiqWater        ,& ! in,  soil water content [m3/m3]
              SoilIce             => noahmp%water%state%SoilIce             ,& ! in,  soil ice content [m3/m3]
              SoilIceMax          => noahmp%water%state%SoilIceMax          ,& ! in,  maximum soil ice content [m3/m3]
              SoilSfcInflowMean   => noahmp%water%flux%SoilSfcInflowMean    ,& ! in,  water input on soil surface [m/s]
              SoilMoistureSat     => noahmp%water%param%SoilMoistureSat     ,& ! in,  saturated value of soil moisture [m3/m3]
              SoilMoistureWilt    => noahmp%water%param%SoilMoistureWilt    ,& ! in,  wilting point soil moisture [m3/m3]
              SoilInfilMaxCoeff   => noahmp%water%param%SoilInfilMaxCoeff   ,& ! in,  parameter to calculate maximum infiltration rate
              SoilImpervFracCoeff => noahmp%water%param%SoilImpervFracCoeff ,& ! in,  parameter to calculate frozen soil impermeable fraction
              RunoffSurface       => noahmp%water%flux%RunoffSurface        ,& ! out, surface runoff [m/s]
              InfilRateSfc        => noahmp%water%flux%InfilRateSfc          & ! out, infiltration rate at surface [m/s]
             )
! ----------------------------------------------------------------------

    ! initialize
    if (.not. allocated(SoilWatMaxHold)) allocate(SoilWatMaxHold(1:NumSoilLayer))
    SoilWatMaxHold(1:NumSoilLayer) = 0.0

    ! start infiltration for free drainage scheme
    if ( SoilSfcInflowMean > 0.0 ) then

       TimeStepDay    = TimeStep / 86400.0
       SoilWatHoldCap = SoilMoistureSat(1) - SoilMoistureWilt(1)

       ! compute maximum infiltration rate
       SoilWatMaxHold(1) = -DepthSoilLayer(1) * SoilWatHoldCap
       SoilIceWatTmp     = -DepthSoilLayer(1) * SoilIce(1)
       SoilWatMaxHold(1) =  SoilWatMaxHold(1) * (1.0-(SoilLiqWater(1)+SoilIce(1)-SoilMoistureWilt(1)) / SoilWatHoldCap)
       SoilWatHoldMaxAcc =  SoilWatMaxHold(1)
       do LoopInd3 = 2, NumSoilLayer
          SoilIceWatTmp            = SoilIceWatTmp + (DepthSoilLayer(LoopInd3-1) - DepthSoilLayer(LoopInd3))*SoilIce(LoopInd3)
          SoilWatMaxHold(LoopInd3) = (DepthSoilLayer(LoopInd3-1) - DepthSoilLayer(LoopInd3)) * SoilWatHoldCap
          SoilWatMaxHold(LoopInd3) = SoilWatMaxHold(LoopInd3) * (1.0 - (SoilLiqWater(LoopInd3) + SoilIce(LoopInd3) - &
                                                                 SoilMoistureWilt(LoopInd3)) / SoilWatHoldCap)
          SoilWatHoldMaxAcc        = SoilWatHoldMaxAcc + SoilWatMaxHold(LoopInd3)
       enddo
       FracVoidRem       = 1.0 - exp(-1.0 * SoilInfilMaxCoeff * TimeStepDay)
       SoilWatHoldMaxRem = SoilWatHoldMaxAcc * FracVoidRem
       WaterInSfc        = max(0.0, SoilSfcInflowMean * TimeStep)
       InfilRateMax      = (WaterInSfc * (SoilWatHoldMaxRem/(WaterInSfc + SoilWatHoldMaxRem))) / TimeStep

       ! impermeable fraction due to frozen soil
       SoilImpervFrac = 1.0
       if ( SoilIceWatTmp > 1.0e-2 ) then
          SoilIceCoeff = FrzSoilFac * SoilImpervFracCoeff / SoilIceWatTmp
          IndAcc       = 1.0
          IndSoilFrz   = FrzSoilFac - 1
          do LoopInd1 = 1, IndSoilFrz
             LoopInd3  = 1
             do LoopInd2 = LoopInd1+1, IndSoilFrz
                LoopInd3 = LoopInd3 * LoopInd2
             enddo
             IndAcc = IndAcc + (SoilIceCoeff ** (FrzSoilFac-LoopInd1)) / float(LoopInd3)
          enddo
          SoilImpervFrac = 1.0 - exp(-SoilIceCoeff) * IndAcc
       endif

       ! correction of infiltration limitation
       InfilRateMax = InfilRateMax * SoilImpervFrac
       ! jref for urban areas
       ! if ( FlagUrban .eqv. .true. ) InfilRateMax == InfilRateMax * 0.05

       ! soil hydraulic conductivity and diffusivity
       call SoilDiffusivityConductivityOpt2(noahmp, SoilWatDiffusivity, SoilWatConductivity, SoilLiqWater(1), SoilIceMax, 1)

       InfilRateMax = max(InfilRateMax, SoilWatConductivity)
       InfilRateMax = min(InfilRateMax, WaterInSfc/TimeStep)

       ! compute surface runoff and infiltration rate
       RunoffSurface = max(0.0, SoilSfcInflowMean-InfilRateMax)
       InfilRateSfc  = SoilSfcInflowMean - RunoffSurface

    endif ! SoilSfcInflowMean > 0.0

    ! deallocate local arrays to avoid memory leaks
    deallocate(SoilWatMaxHold)

    end associate

  end subroutine RunoffSurfaceFreeDrain

end module RunoffSurfaceFreeDrainMod
