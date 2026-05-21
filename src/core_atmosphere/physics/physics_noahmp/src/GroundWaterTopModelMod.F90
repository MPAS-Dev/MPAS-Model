module GroundWaterTopModelMod

!!! Compute groundwater flow and subsurface runoff based on TOPMODEL (Niu et al., 2007)

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine GroundWaterTopModel(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: GROUNDWATER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                :: LoopInd                                        ! loop index
    integer                :: IndUnsatSoil                                   ! layer index of the first unsaturated layer
    real(kind=8)           :: SatDegUnsatSoil                                ! degree of saturation of IndUnsatSoil layer
    real(kind=kind_noahmp) :: SoilMatPotFrz                                  ! soil matric potential (frozen effects) [mm]
    real(kind=kind_noahmp) :: AquiferWatConduct                              ! aquifer hydraulic conductivity [mm/s]
    real(kind=kind_noahmp) :: WaterHeadTbl                                   ! water head at water table [mm]
    real(kind=kind_noahmp) :: WaterHead                                      ! water head at layer above water table [mm]
    real(kind=kind_noahmp) :: WaterFillPore                                  ! water used to fill air pore [mm]
    real(kind=kind_noahmp) :: WatConductAcc                                  ! sum of SoilWatConductTmp*ThicknessSoil
    real(kind=kind_noahmp) :: SoilMoistureMin                                ! minimum soil moisture [m3/m3]
    real(kind=kind_noahmp) :: WaterExcessSat                                 ! excessive water above saturation [mm]
    real(kind=kind_noahmp), allocatable, dimension(:) :: ThicknessSoil       ! layer thickness [mm]
    real(kind=kind_noahmp), allocatable, dimension(:) :: DepthSoilMid        ! node depth [m]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilLiqTmp          ! liquid water mass [kg/m2 or mm]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilEffPorosity     ! soil effective porosity
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilWatConductTmp   ! hydraulic conductivity [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilMoisture        ! total soil water  content [m3/m3]

! --------------------------------------------------------------------
    associate(                                                                     &
              NumSoilLayer           => noahmp%config%domain%NumSoilLayer         ,& ! in,    number of soil layers
              SoilTimeStep           => noahmp%config%domain%SoilTimeStep         ,& ! in,    noahmp soil timestep [s]
              DepthSoilLayer         => noahmp%config%domain%DepthSoilLayer       ,& ! in,    depth of soil layer-bottom [m]
              SoilImpervFracMax      => noahmp%water%state%SoilImpervFracMax      ,& ! in,    maximum soil imperviousness fraction
              SoilIce                => noahmp%water%state%SoilIce                ,& ! in,    soil ice content [m3/m3]
              SoilWatConductivity    => noahmp%water%state%SoilWatConductivity    ,& ! in,    soil hydraulic conductivity [m/s]
              SoilMoistureSat        => noahmp%water%param%SoilMoistureSat        ,& ! in,    saturated value of soil moisture [m3/m3]
              GridTopoIndex          => noahmp%water%param%GridTopoIndex          ,& ! in,    gridcell mean topgraphic index (global mean)
              SoilMatPotentialSat    => noahmp%water%param%SoilMatPotentialSat    ,& ! in,    saturated soil matric potential
              SoilExpCoeffB          => noahmp%water%param%SoilExpCoeffB          ,& ! in,    soil B parameter
              SpecYieldGw            => noahmp%water%param%SpecYieldGw            ,& ! in,    specific yield [-], default:0.2
              MicroPoreContent       => noahmp%water%param%MicroPoreContent       ,& ! in,    microprore content (0.0-1.0), default:0.2
              SoilWatConductivitySat => noahmp%water%param%SoilWatConductivitySat ,& ! in,    saturated soil hydraulic conductivity [m/s]
              SoilLiqWater           => noahmp%water%state%SoilLiqWater           ,& ! inout, soil water content [m3/m3]
              WaterTableDepth        => noahmp%water%state%WaterTableDepth        ,& ! inout, water table depth [m]
              WaterStorageAquifer    => noahmp%water%state%WaterStorageAquifer    ,& ! inout, water storage in aquifer [mm]
              WaterStorageSoilAqf    => noahmp%water%state%WaterStorageSoilAqf    ,& ! inout, water storage in aquifer + saturated soil [mm]
              RunoffDecayFac         => noahmp%water%param%RunoffDecayFac         ,& ! inout, runoff decay factor (1/m)
              BaseflowCoeff          => noahmp%water%param%BaseflowCoeff          ,& ! inout, baseflow coefficient [mm/s]
              RechargeGw             => noahmp%water%flux%RechargeGw              ,& ! out,   groundwater recharge rate [mm/s]
              DischargeGw            => noahmp%water%flux%DischargeGw              & ! out,   groundwater discharge rate [mm/s]
             )
! ----------------------------------------------------------------------

    ! initialization
    if (.not. allocated(DepthSoilMid)     ) allocate(DepthSoilMid     (1:NumSoilLayer))
    if (.not. allocated(ThicknessSoil)    ) allocate(ThicknessSoil    (1:NumSoilLayer))
    if (.not. allocated(SoilLiqTmp)       ) allocate(SoilLiqTmp       (1:NumSoilLayer))
    if (.not. allocated(SoilEffPorosity)  ) allocate(SoilEffPorosity  (1:NumSoilLayer))
    if (.not. allocated(SoilWatConductTmp)) allocate(SoilWatConductTmp(1:NumSoilLayer))
    if (.not. allocated(SoilMoisture)     ) allocate(SoilMoisture     (1:NumSoilLayer))
    DepthSoilMid      = 0.0
    ThicknessSoil     = 0.0
    SoilLiqTmp        = 0.0
    SoilEffPorosity   = 0.0
    SoilWatConductTmp = 0.0
    SoilMoisture      = 0.0
    DischargeGw       = 0.0
    RechargeGw        = 0.0

    ! Derive layer-bottom depth in [mm]; KWM:Derive layer thickness in mm
    ThicknessSoil(1) = -DepthSoilLayer(1) * 1.0e3
    do LoopInd = 2, NumSoilLayer
       ThicknessSoil(LoopInd) = 1.0e3 * (DepthSoilLayer(LoopInd-1) - DepthSoilLayer(LoopInd))
    enddo

    ! Derive node (middle) depth in [m]; KWM: Positive number, depth below ground surface in m
    DepthSoilMid(1) = -DepthSoilLayer(1) / 2.0
    do LoopInd = 2, NumSoilLayer
       DepthSoilMid(LoopInd) = -DepthSoilLayer(LoopInd-1) + &
                               0.5 * (DepthSoilLayer(LoopInd-1) - DepthSoilLayer(LoopInd))
    enddo

    ! Convert volumetric soil moisture to mass
    do LoopInd = 1, NumSoilLayer
       SoilMoisture(LoopInd)      = SoilLiqWater(LoopInd) + SoilIce(LoopInd)
       SoilLiqTmp(LoopInd)        = SoilLiqWater(LoopInd) * ThicknessSoil(LoopInd)
       SoilEffPorosity(LoopInd)   = max(0.01, SoilMoistureSat(LoopInd)-SoilIce(LoopInd))
       SoilWatConductTmp(LoopInd) = 1.0e3 * SoilWatConductivity(LoopInd)
    enddo

    ! The layer index of the first unsaturated layer (the layer right above the water table)
    IndUnsatSoil = NumSoilLayer
    do LoopInd = 2, NumSoilLayer
       if ( WaterTableDepth <= -DepthSoilLayer(LoopInd) ) then
          IndUnsatSoil = LoopInd - 1
          exit
       endif
    enddo

    ! Groundwater discharge [mm/s]
    !RunoffDecayFac    = 6.0
    !BaseflowCoeff     = 5.0
    !DischargeGw       = (1.0 - SoilImpervFracMax) * BaseflowCoeff * &
    !                    exp(-GridTopoIndex) * exp(-RunoffDecayFac * (WaterTableDepth-2.0))
    ! Update from GY Niu 2022
    RunoffDecayFac    = SoilExpCoeffB(IndUnsatSoil) / 3.0
    BaseflowCoeff     = SoilWatConductTmp(IndUnsatSoil) * 1.0e3 * exp(3.0)  ! [mm/s]
    DischargeGw       = (1.0 - SoilImpervFracMax) * BaseflowCoeff * exp(-GridTopoIndex) * &
                        exp(-RunoffDecayFac * WaterTableDepth)

    ! Matric potential at the layer above the water table
    SatDegUnsatSoil   = min(1.0, SoilMoisture(IndUnsatSoil)/SoilMoistureSat(IndUnsatSoil))
    SatDegUnsatSoil   = max(SatDegUnsatSoil, real(0.01,kind=8))
    SoilMatPotFrz     = -SoilMatPotentialSat(IndUnsatSoil) * 1000.0 * &
                        SatDegUnsatSoil**(-SoilExpCoeffB(IndUnsatSoil))   ! m -> mm
    SoilMatPotFrz     = max(-120000.0, MicroPoreContent*SoilMatPotFrz)

    ! Recharge rate qin to groundwater
    !AquiferWatConduct = SoilWatConductTmp(IndUnsatSoil)
    AquiferWatConduct = 2.0 * (SoilWatConductTmp(IndUnsatSoil) * SoilWatConductivitySat(IndUnsatSoil)*1.0e3) / &
                        (SoilWatConductTmp(IndUnsatSoil) + SoilWatConductivitySat(IndUnsatSoil)*1.0e3)  ! harmonic average, GY Niu's update 2022
    WaterHeadTbl      = -WaterTableDepth * 1.0e3                 !(mm)
    WaterHead         = SoilMatPotFrz - DepthSoilMid(IndUnsatSoil) * 1.0e3   !(mm)
    RechargeGw        = -AquiferWatConduct * (WaterHeadTbl - WaterHead) / &
                        ((WaterTableDepth-DepthSoilMid(IndUnsatSoil)) * 1.0e3)
    RechargeGw        = max(-10.0/SoilTimeStep, min(10.0/SoilTimeStep, RechargeGw))

    ! Water storage in the aquifer + saturated soil
    WaterStorageSoilAqf = WaterStorageSoilAqf + (RechargeGw - DischargeGw) * SoilTimeStep     !(mm)
    if ( IndUnsatSoil == NumSoilLayer ) then
       WaterStorageAquifer      = WaterStorageAquifer + (RechargeGw - DischargeGw) * SoilTimeStep     !(mm)
       WaterStorageSoilAqf      = WaterStorageAquifer
       WaterTableDepth          = (-DepthSoilLayer(NumSoilLayer) + 25.0) - &
                                  WaterStorageAquifer / 1000.0 / SpecYieldGw      !(m)
       SoilLiqTmp(NumSoilLayer) = SoilLiqTmp(NumSoilLayer) - RechargeGw * SoilTimeStep        ! [mm]
       SoilLiqTmp(NumSoilLayer) = SoilLiqTmp(NumSoilLayer) + max(0.0, (WaterStorageAquifer-5000.0))
       WaterStorageAquifer      = min(WaterStorageAquifer, 5000.0)
    else
       if ( IndUnsatSoil == NumSoilLayer-1 ) then
          WaterTableDepth = -DepthSoilLayer(NumSoilLayer) - (WaterStorageSoilAqf - SpecYieldGw*1000.0*25.0) / &
                                                            (SoilEffPorosity(NumSoilLayer)) / 1000.0
       else
          WaterFillPore   = 0.0   ! water used to fill soil air pores
          do LoopInd = IndUnsatSoil+2, NumSoilLayer
             WaterFillPore = WaterFillPore + SoilEffPorosity(LoopInd) * ThicknessSoil(LoopInd)
          enddo
          WaterTableDepth  = -DepthSoilLayer(IndUnsatSoil+1) - (WaterStorageSoilAqf - SpecYieldGw*1000.0*25.0 - &
                                                                WaterFillPore) / (SoilEffPorosity(IndUnsatSoil+1)) / 1000.0
       endif
       WatConductAcc = 0.0
       do LoopInd = 1, NumSoilLayer
          WatConductAcc = WatConductAcc + SoilWatConductTmp(LoopInd) * ThicknessSoil(LoopInd)
       enddo
       do LoopInd = 1, NumSoilLayer           ! Removing subsurface runoff
          SoilLiqTmp(LoopInd) = SoilLiqTmp(LoopInd) - DischargeGw * SoilTimeStep * &
                                                      SoilWatConductTmp(LoopInd) * ThicknessSoil(LoopInd) / WatConductAcc
       enddo
    endif
    WaterTableDepth = max(1.5, WaterTableDepth)

    ! Limit SoilLiqTmp to be greater than or equal to SoilMoistureMin
    ! Get water needed to bring SoilLiqTmp equal SoilMoistureMin from lower layer.
    SoilMoistureMin = 0.01
    do LoopInd = 1, NumSoilLayer-1
       if ( SoilLiqTmp(LoopInd) < 0.0 ) then
          WaterExcessSat = SoilMoistureMin - SoilLiqTmp(LoopInd)
       else
          WaterExcessSat = 0.0
       endif
       SoilLiqTmp(LoopInd  ) = SoilLiqTmp(LoopInd  ) + WaterExcessSat
       SoilLiqTmp(LoopInd+1) = SoilLiqTmp(LoopInd+1) - WaterExcessSat
    enddo
    LoopInd = NumSoilLayer
    if ( SoilLiqTmp(LoopInd) < SoilMoistureMin ) then
       WaterExcessSat   = SoilMoistureMin - SoilLiqTmp(LoopInd)
    else
       WaterExcessSat   = 0.0
    endif
    SoilLiqTmp(LoopInd) = SoilLiqTmp(LoopInd) + WaterExcessSat
    WaterStorageAquifer = WaterStorageAquifer - WaterExcessSat
    WaterStorageSoilAqf = WaterStorageSoilAqf - WaterExcessSat

    ! update soil moisture
    do LoopInd = 1, NumSoilLayer
        SoilLiqWater(LoopInd) = SoilLiqTmp(LoopInd) / ThicknessSoil(LoopInd)
    enddo

    ! deallocate local arrays to avoid memory leaks
    deallocate(DepthSoilMid     )
    deallocate(ThicknessSoil    )
    deallocate(SoilLiqTmp       )
    deallocate(SoilEffPorosity  )
    deallocate(SoilWatConductTmp)
    deallocate(SoilMoisture     )

    end associate

  end subroutine GroundWaterTopModel

end module GroundWaterTopModelMod
