module SoilSnowWaterPhaseChangeMod

!!! Compute the phase change (melting/freezing) of snow water and soil water

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use SoilWaterSupercoolKoren99Mod, only : SoilWaterSupercoolKoren99
  use SoilWaterSupercoolNiu06Mod,   only : SoilWaterSupercoolNiu06

  implicit none

contains

  subroutine SoilSnowWaterPhaseChange(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: PHASECHANGE
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type)     , intent(inout) :: noahmp

! local variable
    integer                               :: LoopInd                        ! do loop index
    real(kind=kind_noahmp)                :: EnergyResLeft                  ! energy residual or loss after melting/freezing
    real(kind=kind_noahmp)                :: SnowWaterPrev                  ! old/previous snow water equivalent [kg/m2]
    real(kind=kind_noahmp)                :: SnowWaterRatio                 ! ratio of previous vs updated snow water equivalent 
    real(kind=kind_noahmp)                :: HeatLhTotPhsChg                ! total latent heat of phase change
    real(kind=kind_noahmp), allocatable, dimension(:) :: EnergyRes          ! energy residual [w/m2]
    real(kind=kind_noahmp), allocatable, dimension(:) :: WaterPhaseChg      ! melting or freezing water [kg/m2]
    real(kind=kind_noahmp), allocatable, dimension(:) :: MassWatTotInit     ! initial total water (ice + liq) mass
    real(kind=kind_noahmp), allocatable, dimension(:) :: MassWatIceInit     ! initial ice content
    real(kind=kind_noahmp), allocatable, dimension(:) :: MassWatLiqInit     ! initial liquid content
    real(kind=kind_noahmp), allocatable, dimension(:) :: MassWatIceTmp      ! soil/snow ice mass [mm]
    real(kind=kind_noahmp), allocatable, dimension(:) :: MassWatLiqTmp      ! soil/snow liquid water mass [mm]

! --------------------------------------------------------------------
    associate(                                                                       &
              OptSoilSupercoolWater  => noahmp%config%nmlist%OptSoilSupercoolWater  ,& ! in,    options for soil supercooled liquid water
              NumSoilLayer           => noahmp%config%domain%NumSoilLayer           ,& ! in,    number of soil layers
              NumSnowLayerMax        => noahmp%config%domain%NumSnowLayerMax        ,& ! in,    maximum number of snow layers
              NumSnowLayerNeg        => noahmp%config%domain%NumSnowLayerNeg        ,& ! in,    actual number of snow layers (negative)
              MainTimeStep           => noahmp%config%domain%MainTimeStep           ,& ! in,    main noahmp timestep [s]
              SurfaceType            => noahmp%config%domain%SurfaceType            ,& ! in,    surface type 1-soil; 2-lake
              ThicknessSnowSoilLayer => noahmp%config%domain%ThicknessSnowSoilLayer ,& ! in,    thickness of snow/soil layers [m]
              SoilExpCoeffB          => noahmp%water%param%SoilExpCoeffB            ,& ! in,    soil B parameter
              SoilMatPotentialSat    => noahmp%water%param%SoilMatPotentialSat      ,& ! in,    saturated soil matric potential [m]
              SoilMoistureSat        => noahmp%water%param%SoilMoistureSat          ,& ! in,    saturated value of soil moisture [m3/m3]
              PhaseChgFacSoilSnow    => noahmp%energy%state%PhaseChgFacSoilSnow     ,& ! in,    energy factor for soil & snow phase change
              TemperatureSoilSnow    => noahmp%energy%state%TemperatureSoilSnow     ,& ! inout, snow and soil layer temperature [K]
              SoilLiqWater           => noahmp%water%state%SoilLiqWater             ,& ! inout, soil water content [m3/m3]
              SoilMoisture           => noahmp%water%state%SoilMoisture             ,& ! inout, total soil moisture [m3/m3]
              SnowIce                => noahmp%water%state%SnowIce                  ,& ! inout, snow layer ice [mm]
              SnowLiqWater           => noahmp%water%state%SnowLiqWater             ,& ! inout, snow layer liquid water [mm]
              SnowDepth              => noahmp%water%state%SnowDepth                ,& ! inout, snow depth [m]
              SnowWaterEquiv         => noahmp%water%state%SnowWaterEquiv           ,& ! inout, snow water equivalent [mm]
              IndexPhaseChange       => noahmp%water%state%IndexPhaseChange         ,& ! out,   phase change index [0-none;1-melt;2-refreeze]
              SoilSupercoolWater     => noahmp%water%state%SoilSupercoolWater       ,& ! out,   supercooled water in soil [kg/m2]
              PondSfcThinSnwMelt     => noahmp%water%state%PondSfcThinSnwMelt       ,& ! out,   surface ponding [mm] from melt when thin snow w/o layer
              MeltGroundSnow         => noahmp%water%flux%MeltGroundSnow             & ! out,   ground snowmelt rate [mm/s]
             )
! ----------------------------------------------------------------------

    !--- Initialization
    if (.not. allocated(EnergyRes)     ) allocate(EnergyRes     (-NumSnowLayerMax+1:NumSoilLayer))
    if (.not. allocated(WaterPhaseChg) ) allocate(WaterPhaseChg (-NumSnowLayerMax+1:NumSoilLayer))
    if (.not. allocated(MassWatTotInit)) allocate(MassWatTotInit(-NumSnowLayerMax+1:NumSoilLayer))
    if (.not. allocated(MassWatIceInit)) allocate(MassWatIceInit(-NumSnowLayerMax+1:NumSoilLayer))
    if (.not. allocated(MassWatLiqInit)) allocate(MassWatLiqInit(-NumSnowLayerMax+1:NumSoilLayer))
    if (.not. allocated(MassWatIceTmp) ) allocate(MassWatIceTmp (-NumSnowLayerMax+1:NumSoilLayer))
    if (.not. allocated(MassWatLiqTmp) ) allocate(MassWatLiqTmp (-NumSnowLayerMax+1:NumSoilLayer))
    EnergyRes          = 0.0
    WaterPhaseChg      = 0.0
    MassWatTotInit     = 0.0
    MassWatIceInit     = 0.0
    MassWatLiqInit     = 0.0
    MassWatIceTmp      = 0.0
    MassWatLiqTmp      = 0.0
    MeltGroundSnow     = 0.0
    PondSfcThinSnwMelt = 0.0
    HeatLhTotPhsChg    = 0.0

    ! supercooled water content
    do LoopInd = -NumSnowLayerMax+1, NumSoilLayer 
         SoilSupercoolWater(LoopInd) = 0.0
    enddo

    ! snow layer water mass
    do LoopInd = NumSnowLayerNeg+1, 0
       MassWatIceTmp(LoopInd) = SnowIce(LoopInd)
       MassWatLiqTmp(LoopInd) = SnowLiqWater(LoopInd)
    enddo

    ! soil layer water mass
    do LoopInd = 1, NumSoilLayer
       MassWatLiqTmp(LoopInd) = SoilLiqWater(LoopInd) * ThicknessSnowSoilLayer(LoopInd) * 1000.0
       MassWatIceTmp(LoopInd) = (SoilMoisture(LoopInd) - SoilLiqWater(LoopInd)) * ThicknessSnowSoilLayer(LoopInd) * 1000.0
    enddo

    ! other required variables
    do LoopInd = NumSnowLayerNeg+1, NumSoilLayer
       IndexPhaseChange(LoopInd) = 0
       EnergyRes(LoopInd)        = 0.0
       WaterPhaseChg(LoopInd)    = 0.0
       MassWatIceInit(LoopInd)   = MassWatIceTmp(LoopInd)
       MassWatLiqInit(LoopInd)   = MassWatLiqTmp(LoopInd)
       MassWatTotInit(LoopInd)   = MassWatIceTmp(LoopInd) + MassWatLiqTmp(LoopInd)
    enddo

    !--- compute soil supercool water content
    if ( SurfaceType == 1 ) then ! land points
       do LoopInd = 1, NumSoilLayer
          if ( OptSoilSupercoolWater == 1 ) then
             if ( TemperatureSoilSnow(LoopInd) < ConstFreezePoint ) then
                call SoilWaterSupercoolNiu06(noahmp, LoopInd, SoilSupercoolWater(LoopInd),TemperatureSoilSnow(LoopInd))
                SoilSupercoolWater(LoopInd) = SoilSupercoolWater(LoopInd) * ThicknessSnowSoilLayer(LoopInd) * 1000.0
             endif
          endif
          if ( OptSoilSupercoolWater == 2 ) then
             if ( TemperatureSoilSnow(LoopInd) < ConstFreezePoint ) then
                call SoilWaterSupercoolKoren99(noahmp, LoopInd, SoilSupercoolWater(LoopInd), &
                                               TemperatureSoilSnow(LoopInd), SoilMoisture(LoopInd), SoilLiqWater(LoopInd))
                SoilSupercoolWater(LoopInd) = SoilSupercoolWater(LoopInd) * ThicknessSnowSoilLayer(LoopInd) * 1000.0
             endif
          endif
       enddo
    endif

    !--- determine melting or freezing state
    do LoopInd = NumSnowLayerNeg+1, NumSoilLayer
       if ( (MassWatIceTmp(LoopInd) > 0.0) .and. (TemperatureSoilSnow(LoopInd) >= ConstFreezePoint) ) then
          IndexPhaseChange(LoopInd) = 1  ! melting
       endif
       if ( (MassWatLiqTmp(LoopInd) > SoilSupercoolWater(LoopInd)) .and. &
            (TemperatureSoilSnow(LoopInd) < ConstFreezePoint) ) then
          IndexPhaseChange(LoopInd) = 2  ! freezing
       endif
       ! If snow exists, but its thickness is not enough to create a layer
       if ( (NumSnowLayerNeg == 0) .and. (SnowWaterEquiv > 0.0) .and. (LoopInd == 1) ) then
          if ( TemperatureSoilSnow(LoopInd) >= ConstFreezePoint ) then
             IndexPhaseChange(LoopInd) = 1
          endif
       endif
    enddo

    !--- Calculate the energy surplus and loss for melting and freezing
    do LoopInd = NumSnowLayerNeg+1, NumSoilLayer
       if ( IndexPhaseChange(LoopInd) > 0 ) then
          EnergyRes(LoopInd)           = (TemperatureSoilSnow(LoopInd)-ConstFreezePoint) / PhaseChgFacSoilSnow(LoopInd)
          TemperatureSoilSnow(LoopInd) = ConstFreezePoint
       endif
       if ( (IndexPhaseChange(LoopInd) == 1) .and. (EnergyRes(LoopInd) < 0.0) ) then
          EnergyRes(LoopInd)        = 0.0
          IndexPhaseChange(LoopInd) = 0
       endif
       if ( (IndexPhaseChange(LoopInd) == 2) .and. (EnergyRes(LoopInd) > 0.0) ) then
          EnergyRes(LoopInd)        = 0.0
          IndexPhaseChange(LoopInd) = 0
       endif
       WaterPhaseChg(LoopInd) = EnergyRes(LoopInd) * MainTimeStep / ConstLatHeatFusion
    enddo

    !--- The rate of melting for snow without a layer, needs more work.
    if ( (NumSnowLayerNeg == 0) .and. (SnowWaterEquiv > 0.0) .and. (WaterPhaseChg(1) > 0.0) ) then
       SnowWaterPrev  = SnowWaterEquiv
       SnowWaterEquiv = max(0.0, SnowWaterPrev-WaterPhaseChg(1))
       SnowWaterRatio = SnowWaterEquiv / SnowWaterPrev
       SnowDepth      = max(0.0, SnowWaterRatio*SnowDepth )
       SnowDepth      = min(max(SnowDepth,SnowWaterEquiv/500.0), SnowWaterEquiv/50.0)      ! limit adjustment to a reasonable density
       EnergyResLeft  = EnergyRes(1) - ConstLatHeatFusion * (SnowWaterPrev - SnowWaterEquiv) / MainTimeStep
       if ( EnergyResLeft > 0.0 ) then
          WaterPhaseChg(1) = EnergyResLeft * MainTimeStep / ConstLatHeatFusion
          EnergyRes(1)     = EnergyResLeft
       else
          WaterPhaseChg(1) = 0.0
          EnergyRes(1)     = 0.0
       endif
       MeltGroundSnow     = max(0.0, (SnowWaterPrev-SnowWaterEquiv)) / MainTimeStep
       HeatLhTotPhsChg    = ConstLatHeatFusion * MeltGroundSnow
       PondSfcThinSnwMelt = SnowWaterPrev - SnowWaterEquiv
    endif

    ! The rate of melting and freezing for multi-layer snow and soil
    do LoopInd = NumSnowLayerNeg+1, NumSoilLayer
       if ( (IndexPhaseChange(LoopInd) > 0) .and. (abs(EnergyRes(LoopInd)) > 0.0) ) then
          EnergyResLeft = 0.0
          if ( WaterPhaseChg(LoopInd) > 0.0 ) then
             MassWatIceTmp(LoopInd) = max(0.0, MassWatIceInit(LoopInd)-WaterPhaseChg(LoopInd))
             EnergyResLeft          = EnergyRes(LoopInd) - ConstLatHeatFusion * &
                                      (MassWatIceInit(LoopInd) - MassWatIceTmp(LoopInd)) / MainTimeStep
          elseif ( WaterPhaseChg(LoopInd) < 0.0 ) then
             if ( LoopInd <= 0 ) then  ! snow layer
                MassWatIceTmp(LoopInd) = min(MassWatTotInit(LoopInd), MassWatIceInit(LoopInd)-WaterPhaseChg(LoopInd))
             else                      ! soil layer
                if ( MassWatTotInit(LoopInd) < SoilSupercoolWater(LoopInd) ) then
                   MassWatIceTmp(LoopInd) = 0.0
                else
                   MassWatIceTmp(LoopInd) = min(MassWatTotInit(LoopInd)-SoilSupercoolWater(LoopInd), &
                                                MassWatIceInit(LoopInd)-WaterPhaseChg(LoopInd))
                   MassWatIceTmp(LoopInd) = max(MassWatIceTmp(LoopInd), 0.0)
                endif
             endif
             EnergyResLeft = EnergyRes(LoopInd) - ConstLatHeatFusion * (MassWatIceInit(LoopInd) - &
                                                                        MassWatIceTmp(LoopInd)) / MainTimeStep
          endif
          MassWatLiqTmp(LoopInd) = max(0.0, MassWatTotInit(LoopInd)-MassWatIceTmp(LoopInd)) ! update liquid water mass

          ! update soil/snow temperature and energy surplus/loss
          if ( abs(EnergyResLeft) > 0.0 ) then
             TemperatureSoilSnow(LoopInd) = TemperatureSoilSnow(LoopInd) + PhaseChgFacSoilSnow(LoopInd) * EnergyResLeft
             if ( LoopInd <= 0 ) then  ! snow
                if ( (MassWatLiqTmp(LoopInd)*MassWatIceTmp(LoopInd)) > 0.0 ) &
                   TemperatureSoilSnow(LoopInd) = ConstFreezePoint
                if ( MassWatIceTmp(LoopInd) == 0.0 ) then         ! BARLAGE
                   TemperatureSoilSnow(LoopInd) = ConstFreezePoint
                   EnergyRes(LoopInd+1)         = EnergyRes(LoopInd+1) + EnergyResLeft
                   WaterPhaseChg(LoopInd+1)     = EnergyRes(LoopInd+1) * MainTimeStep / ConstLatHeatFusion
                endif
             endif
          endif
          HeatLhTotPhsChg = HeatLhTotPhsChg + ConstLatHeatFusion * &
                            (MassWatIceInit(LoopInd) - MassWatIceTmp(LoopInd)) / MainTimeStep
          ! snow melting rate
          if ( LoopInd < 1 ) then
             MeltGroundSnow = MeltGroundSnow + max(0.0, (MassWatIceInit(LoopInd)-MassWatIceTmp(LoopInd))) / MainTimeStep
          endif
       endif
    enddo

    !--- update snow and soil ice and liquid content
    do LoopInd = NumSnowLayerNeg+1, 0     ! snow
       SnowLiqWater(LoopInd) = MassWatLiqTmp(LoopInd)
       SnowIce(LoopInd)      = MassWatIceTmp(LoopInd)
    enddo
    do LoopInd = 1, NumSoilLayer       ! soil
       SoilLiqWater(LoopInd) = MassWatLiqTmp(LoopInd) / (1000.0 * ThicknessSnowSoilLayer(LoopInd))
       SoilMoisture(LoopInd) = (MassWatLiqTmp(LoopInd)+MassWatIceTmp(LoopInd)) / (1000.0*ThicknessSnowSoilLayer(LoopInd))
    enddo

    ! deallocate local arrays to avoid memory leaks
    deallocate(EnergyRes     )
    deallocate(WaterPhaseChg )
    deallocate(MassWatTotInit)
    deallocate(MassWatIceInit)
    deallocate(MassWatLiqInit)
    deallocate(MassWatIceTmp )
    deallocate(MassWatLiqTmp )

    end associate

  end subroutine SoilSnowWaterPhaseChange

end module SoilSnowWaterPhaseChangeMod
