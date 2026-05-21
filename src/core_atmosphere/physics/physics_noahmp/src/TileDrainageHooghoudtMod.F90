module TileDrainageHooghoudtMod

!!! Calculate tile drainage discharge [mm] based on Hooghoudt's equation

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use TileDrainageEquiDepthMod, only : TileDrainageEquiDepth
  use WaterTableDepthSearchMod, only : WaterTableDepthSearch
  use WaterTableEquilibriumMod, only : WaterTableEquilibrium

  implicit none

contains

  subroutine TileDrainageHooghoudt(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: TILE_HOOGHOUDT
! Original code: P. Valayamkunnath (NCAR)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    integer                          :: IndSoil                                ! soil layer loop index 
    integer                          :: NumDrain                               ! number of drains
    real(kind=kind_noahmp)           :: ThickSatZoneTot                        ! total thickness of saturated zone
    real(kind=kind_noahmp)           :: LateralFlow                            ! lateral flow
    real(kind=kind_noahmp)           :: DepthToLayerTop                        ! depth to top of the layer
    real(kind=kind_noahmp)           :: WatTblTmp1                             ! temporary water table variable
    real(kind=kind_noahmp)           :: WatTblTmp2                             ! temporary water table variable
    real(kind=kind_noahmp)           :: LateralWatCondAve                      ! average lateral hydruaic conductivity
    real(kind=kind_noahmp)           :: DrainWatHgtAbvImp                      ! Height of water table in the drain Above Impermeable Layer
    real(kind=kind_noahmp)           :: DepthSfcToImp                          ! Effective Depth to impermeable layer from soil surface
    real(kind=kind_noahmp)           :: HgtDrnToWatTbl                         ! Effective Height between water level in drains to water table MiDpoint
    real(kind=kind_noahmp)           :: DrainCoeffTmp                          ! Drainage Coefficient
    real(kind=kind_noahmp)           :: TileDrainTmp                           ! temporary drainage discharge
    real(kind=kind_noahmp)           :: DrainDepthToImpTmp                     ! drain depth to impermeable layer
    real(kind=kind_noahmp)           :: WatExcFieldCapTot                      ! amount of water over field capacity
    real(kind=kind_noahmp), allocatable, dimension(:) :: ThickSatZone          ! thickness of saturated zone
    real(kind=kind_noahmp), allocatable, dimension(:) :: LateralWatCondTmp     ! lateral hydraulic ocnductivity kth layer
    real(kind=kind_noahmp), allocatable, dimension(:) :: WatExcFieldCapTmp     ! layer-wise amount of water over field capacity
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilLiqWaterAftDrain  ! remaining water after tile drain

! ----------------------------------------------------------------------------
    associate(                                                                 &
              NumSoilLayer         => noahmp%config%domain%NumSoilLayer       ,& ! in,    number of soil layers
              DepthSoilLayer       => noahmp%config%domain%DepthSoilLayer     ,& ! in,    depth [m] of layer-bottom from soil surface
              SoilTimeStep         => noahmp%config%domain%SoilTimeStep       ,& ! in,    noahmp soil timestep [s]
              GridSize             => noahmp%config%domain%GridSize           ,& ! in,    noahmp model grid spacing [m]
              ThicknessSoilLayer   => noahmp%config%domain%ThicknessSoilLayer ,& ! in,    soil layer thickness [m]
              SoilMoistureFieldCap => noahmp%water%param%SoilMoistureFieldCap ,& ! in,    reference soil moisture (field capacity) [m3/m3]
              TileDrainCoeff       => noahmp%water%param%TileDrainCoeff       ,& ! in,    drainage coefficent [m/day]
              DrainDepthToImperv   => noahmp%water%param%DrainDepthToImperv   ,& ! in,    Actual depth to impermeable layer from surface [m]
              LateralWatCondFac    => noahmp%water%param%LateralWatCondFac    ,& ! in,    multiplication factor to determine lateral hydraulic conductivity
              TileDrainDepth       => noahmp%water%param%TileDrainDepth       ,& ! in,    Depth of drain [m]
              DrainTubeDist        => noahmp%water%param%DrainTubeDist        ,& ! in,    distance between two drain tubes or tiles [m]
              DrainTubeRadius      => noahmp%water%param%DrainTubeRadius      ,& ! in,    effective radius of drains [m]
              SoilWatConductivity  => noahmp%water%state%SoilWatConductivity  ,& ! in,    soil hydraulic conductivity [m/s]
              SoilIce              => noahmp%water%state%SoilIce              ,& ! in,    soil ice content [m3/m3]
              WaterTableHydro      => noahmp%water%state%WaterTableHydro      ,& ! in,    water table depth estimated in WRF-Hydro fine grids [m]
              SoilLiqWater         => noahmp%water%state%SoilLiqWater         ,& ! inout, soil water content [m3/m3]
              SoilMoisture         => noahmp%water%state%SoilMoisture         ,& ! inout, total soil moisture [m3/m3]
              WaterTableDepth      => noahmp%water%state%WaterTableDepth      ,& ! inout, water table depth [m]
              TileDrain            => noahmp%water%flux%TileDrain              & ! inout, tile drainage [mm/s]
             )
! ----------------------------------------------------------------------

    ! initialization
    if (.not. allocated(ThickSatZone)        ) allocate(ThickSatZone        (1:NumSoilLayer))
    if (.not. allocated(LateralWatCondTmp)   ) allocate(LateralWatCondTmp   (1:NumSoilLayer))
    if (.not. allocated(WatExcFieldCapTmp)   ) allocate(WatExcFieldCapTmp   (1:NumSoilLayer))
    if (.not. allocated(SoilLiqWaterAftDrain)) allocate(SoilLiqWaterAftDrain(1:NumSoilLayer))
    ThickSatZone         = 0.0
    LateralWatCondTmp    = 0.0
    WatExcFieldCapTmp    = 0.0
    SoilLiqWaterAftDrain = 0.0
    DepthToLayerTop      = 0.0
    LateralFlow          = 0.0
    ThickSatZoneTot      = 0.0
    DrainCoeffTmp        = TileDrainCoeff * 1000.0 * SoilTimeStep / (24.0 * 3600.0)                   ! m per day to mm per timestep

    ! Thickness of soil layers    
    do IndSoil = 1, NumSoilLayer
       if ( IndSoil == 1 ) then
          ThicknessSoilLayer(IndSoil) = -1.0 * DepthSoilLayer(IndSoil)
       else
          ThicknessSoilLayer(IndSoil) = (DepthSoilLayer(IndSoil-1) - DepthSoilLayer(IndSoil))
       endif
    enddo

#ifdef WRF_HYDRO
    ! Depth to water table from WRF-HYDRO, m
    WatTblTmp2 = WaterTableHydro
#else
    call WaterTableDepthSearch(noahmp)
    !call WaterTableEquilibrium(noahmp)
    WatTblTmp2 = WaterTableDepth
#endif

    if ( WatTblTmp2 > DrainDepthToImperv) WatTblTmp2 = DrainDepthToImperv

    ! Depth of saturated zone
    do IndSoil = 1, NumSoilLayer
       if ( WatTblTmp2 > (-1.0*DepthSoilLayer(IndSoil)) ) then
          ThickSatZone(IndSoil) = 0.0
       else
          ThickSatZone(IndSoil) = (-1.0 * DepthSoilLayer(IndSoil)) - WatTblTmp2
          WatTblTmp1            = (-1.0 * DepthSoilLayer(IndSoil)) - DepthToLayerTop
          if ( ThickSatZone(IndSoil) > WatTblTmp1 ) ThickSatZone(IndSoil) = WatTblTmp1
       endif
       DepthToLayerTop = -1.0 * DepthSoilLayer(IndSoil)
    enddo

    ! amount of water over field capacity
    WatExcFieldCapTot = 0.0
    do IndSoil = 1, NumSoilLayer
       WatExcFieldCapTmp(IndSoil) = (SoilLiqWater(IndSoil) - (SoilMoistureFieldCap(IndSoil)-SoilIce(IndSoil))) * &
                                    ThicknessSoilLayer(IndSoil) * 1000.0
       if ( WatExcFieldCapTmp(IndSoil) < 0.0 ) WatExcFieldCapTmp(IndSoil) = 0.0
       WatExcFieldCapTot = WatExcFieldCapTot + WatExcFieldCapTmp(IndSoil)
    enddo

    ! lateral hydraulic conductivity and total lateral flow
    do IndSoil = 1, NumSoilLayer
       LateralWatCondTmp(IndSoil) = SoilWatConductivity(IndSoil) * LateralWatCondFac * SoilTimeStep      ! m/s to m/timestep
       LateralFlow                = LateralFlow + (ThickSatZone(IndSoil) * LateralWatCondTmp(IndSoil))
       ThickSatZoneTot            = ThickSatZoneTot + ThickSatZone(IndSoil)
    enddo
    if ( ThickSatZoneTot < 0.001 ) ThickSatZoneTot = 0.001                                               ! unit is m
    if ( LateralFlow < 0.001 )     LateralFlow     = 0.0                                                 ! unit is m
    LateralWatCondAve  = LateralFlow / ThickSatZoneTot                                                   ! lateral hydraulic conductivity per timestep
    DrainDepthToImpTmp = DrainDepthToImperv - TileDrainDepth

    call TileDrainageEquiDepth(DrainDepthToImpTmp, DrainTubeDist, DrainTubeRadius, DrainWatHgtAbvImp)

    DepthSfcToImp  = DrainWatHgtAbvImp + TileDrainDepth
    HgtDrnToWatTbl = TileDrainDepth - WatTblTmp2
    if ( HgtDrnToWatTbl <= 0.0 ) then
       TileDrain = 0.0
    else
       TileDrain = ((8.0*LateralWatCondAve*DrainWatHgtAbvImp*HgtDrnToWatTbl) + &
                   (4.0*LateralWatCondAve*HgtDrnToWatTbl*HgtDrnToWatTbl)) / (DrainTubeDist*DrainTubeDist)
    endif
    TileDrain    = TileDrain * 1000.0                                                                     ! m per timestep to mm/timestep /one tile
    if ( TileDrain <= 0.0 ) TileDrain = 0.0
    if ( TileDrain > DrainCoeffTmp ) TileDrain = DrainCoeffTmp
    NumDrain  = int(GridSize / DrainTubeDist)
    TileDrain = TileDrain * NumDrain
    if ( TileDrain > WatExcFieldCapTot ) TileDrain = WatExcFieldCapTot

    ! update soil moisture after drainage: moisture drains from top to bottom
    TileDrainTmp = TileDrain
    do IndSoil = 1, NumSoilLayer
       if ( TileDrainTmp > 0.0) then
          if ( (ThickSatZone(IndSoil) > 0.0) .and. (WatExcFieldCapTmp(IndSoil) > 0.0) ) then
             SoilLiqWaterAftDrain(IndSoil) = WatExcFieldCapTmp(IndSoil) - TileDrainTmp                    ! remaining water after tile drain
             if ( SoilLiqWaterAftDrain(IndSoil) > 0.0 ) then
                SoilLiqWater(IndSoil) = (SoilMoistureFieldCap(IndSoil) - SoilIce(IndSoil)) + &
                                         SoilLiqWaterAftDrain(IndSoil) / (ThicknessSoilLayer(IndSoil) * 1000.0)
                SoilMoisture(IndSoil) = SoilLiqWater(IndSoil) + SoilIce(IndSoil)
                exit
             else
                SoilLiqWater(IndSoil) = SoilMoistureFieldCap(IndSoil) - SoilIce(IndSoil)
                SoilMoisture(IndSoil) = SoilLiqWater(IndSoil) + SoilIce (IndSoil)
                TileDrainTmp          = TileDrainTmp - WatExcFieldCapTmp(IndSoil)
             endif
          endif
       endif
    enddo

    TileDrain = TileDrain / SoilTimeStep            ! mm/s

    ! deallocate local arrays to avoid memory leaks
    deallocate(ThickSatZone        )
    deallocate(LateralWatCondTmp   )
    deallocate(WatExcFieldCapTmp   )
    deallocate(SoilLiqWaterAftDrain)

    end associate

  end subroutine TileDrainageHooghoudt

end module TileDrainageHooghoudtMod
