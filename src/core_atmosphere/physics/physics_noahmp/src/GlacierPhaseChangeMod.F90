module GlacierPhaseChangeMod

!!! Compute the phase change (melting/freezing) of snow and glacier ice

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine GlacierPhaseChange(noahmp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: PHASECHANGE_GLACIER
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type)     , intent(inout) :: noahmp

! local variable
    integer                               :: LoopInd1, LoopInd2              ! loop index
    real(kind=kind_noahmp)                :: SnowWaterPrev                   ! old/previous snow water equivalent [kg/m2]
    real(kind=kind_noahmp)                :: SnowWaterRatio                  ! ratio of previous vs updated snow water equivalent
    real(kind=kind_noahmp)                :: HeatLhTotPhsChg                 ! total latent heat of phase change
    real(kind=kind_noahmp), allocatable, dimension(:) :: EnergyRes           ! energy residual [W/m2]
    real(kind=kind_noahmp), allocatable, dimension(:) :: GlacierPhaseChg     ! melting or freezing glacier water [kg/m2]
    real(kind=kind_noahmp), allocatable, dimension(:) :: MassWatTotInit      ! initial total water (ice + liq) mass
    real(kind=kind_noahmp), allocatable, dimension(:) :: MassWatIceInit      ! initial ice content
    real(kind=kind_noahmp), allocatable, dimension(:) :: MassWatLiqInit      ! initial liquid content
    real(kind=kind_noahmp), allocatable, dimension(:) :: MassWatIceTmp       ! soil/snow ice mass [mm]
    real(kind=kind_noahmp), allocatable, dimension(:) :: MassWatLiqTmp       ! soil/snow liquid water mass [mm]
    real(kind=kind_noahmp), allocatable, dimension(:) :: EnergyResLeft       ! energy residual or loss after melting/freezing

! --------------------------------------------------------------------
    associate(                                                                       &
              OptGlacierTreatment    => noahmp%config%nmlist%OptGlacierTreatment    ,& ! in,    options for glacier treatment
              NumSoilLayer           => noahmp%config%domain%NumSoilLayer           ,& ! in,    number of soil layers
              NumSnowLayerMax        => noahmp%config%domain%NumSnowLayerMax        ,& ! in,    maximum number of snow layers
              NumSnowLayerNeg        => noahmp%config%domain%NumSnowLayerNeg        ,& ! in,    actual number of snow layers (negative)
              MainTimeStep           => noahmp%config%domain%MainTimeStep           ,& ! in,    main noahmp timestep [s]
              ThicknessSnowSoilLayer => noahmp%config%domain%ThicknessSnowSoilLayer ,& ! in,    thickness of snow/soil layers [m]
              PhaseChgFacSoilSnow    => noahmp%energy%state%PhaseChgFacSoilSnow     ,& ! in,    energy factor for soil & snow phase change
              TemperatureSoilSnow    => noahmp%energy%state%TemperatureSoilSnow     ,& ! inout, snow and soil layer temperature [K]
              SoilLiqWater           => noahmp%water%state%SoilLiqWater             ,& ! inout, soil water content [m3/m3]
              SoilMoisture           => noahmp%water%state%SoilMoisture             ,& ! inout, total soil moisture [m3/m3]
              SnowIce                => noahmp%water%state%SnowIce                  ,& ! inout, snow layer ice [mm]
              SnowLiqWater           => noahmp%water%state%SnowLiqWater             ,& ! inout, snow layer liquid water [mm]
              SnowDepth              => noahmp%water%state%SnowDepth                ,& ! inout, snow depth [m]
              SnowWaterEquiv         => noahmp%water%state%SnowWaterEquiv           ,& ! inout, snow water equivalent [mm]
              IndexPhaseChange       => noahmp%water%state%IndexPhaseChange         ,& ! out,   phase change index [0-none;1-melt;2-refreeze]
              MeltGroundSnow         => noahmp%water%flux%MeltGroundSnow            ,& ! out,   ground snowmelt rate [mm/s]
              PondSfcThinSnwMelt     => noahmp%water%state%PondSfcThinSnwMelt        & ! out,   surface ponding [mm] from snowmelt when thin snow has no layer
             )
! ----------------------------------------------------------------------

    !--- Initialization
    if (.not. allocated(EnergyRes)      ) allocate(EnergyRes      (-NumSnowLayerMax+1:NumSoilLayer))
    if (.not. allocated(GlacierPhaseChg)) allocate(GlacierPhaseChg(-NumSnowLayerMax+1:NumSoilLayer))
    if (.not. allocated(MassWatTotInit) ) allocate(MassWatTotInit (-NumSnowLayerMax+1:NumSoilLayer))
    if (.not. allocated(MassWatIceInit) ) allocate(MassWatIceInit (-NumSnowLayerMax+1:NumSoilLayer))
    if (.not. allocated(MassWatLiqInit) ) allocate(MassWatLiqInit (-NumSnowLayerMax+1:NumSoilLayer))
    if (.not. allocated(MassWatIceTmp)  ) allocate(MassWatIceTmp  (-NumSnowLayerMax+1:NumSoilLayer))
    if (.not. allocated(MassWatLiqTmp)  ) allocate(MassWatLiqTmp  (-NumSnowLayerMax+1:NumSoilLayer))
    if (.not. allocated(EnergyResLeft)  ) allocate(EnergyResLeft  (-NumSnowLayerMax+1:NumSoilLayer))
    EnergyRes          = 0.0
    GlacierPhaseChg    = 0.0
    MassWatTotInit     = 0.0
    MassWatIceInit     = 0.0
    MassWatLiqInit     = 0.0
    MassWatIceTmp      = 0.0
    MassWatLiqTmp      = 0.0
    EnergyResLeft      = 0.0
    MeltGroundSnow     = 0.0
    PondSfcThinSnwMelt = 0.0
    HeatLhTotPhsChg    = 0.0

    !--- treat snowpack over glacier ice first

    ! snow layer water mass
    do LoopInd1 = NumSnowLayerNeg+1, 0
       MassWatIceTmp(LoopInd1) = SnowIce(LoopInd1)
       MassWatLiqTmp(LoopInd1) = SnowLiqWater(LoopInd1)
    enddo

    ! other required variables
    do LoopInd1 = NumSnowLayerNeg+1, 0
       IndexPhaseChange(LoopInd1) = 0
       EnergyRes       (LoopInd1) = 0.0
       GlacierPhaseChg (LoopInd1) = 0.0
       EnergyResLeft   (LoopInd1) = 0.0
       MassWatIceInit  (LoopInd1) = MassWatIceTmp(LoopInd1)
       MassWatLiqInit  (LoopInd1) = MassWatLiqTmp(LoopInd1)
       MassWatTotInit  (LoopInd1) = MassWatIceTmp(LoopInd1) + MassWatLiqTmp(LoopInd1)
    enddo

    ! determine melting or freezing state
    do LoopInd1 = NumSnowLayerNeg+1, 0
       if ( (MassWatIceTmp(LoopInd1) > 0.0) .and. (TemperatureSoilSnow(LoopInd1) >= ConstFreezePoint) ) then
          IndexPhaseChange(LoopInd1) = 1  ! melting
       endif
       if ( (MassWatLiqTmp(LoopInd1) > 0.0) .and. (TemperatureSoilSnow(LoopInd1) < ConstFreezePoint) ) then
          IndexPhaseChange(LoopInd1) = 2  ! freezing
       endif
    enddo

    ! Calculate the energy surplus and loss for melting and freezing
    do LoopInd1 = NumSnowLayerNeg+1, 0
       if ( IndexPhaseChange(LoopInd1) > 0 ) then
          EnergyRes(LoopInd1)           = (TemperatureSoilSnow(LoopInd1) - ConstFreezePoint) / PhaseChgFacSoilSnow(LoopInd1)
          TemperatureSoilSnow(LoopInd1) = ConstFreezePoint
       endif
       if ( (IndexPhaseChange(LoopInd1) == 1) .and. (EnergyRes(LoopInd1) < 0.0) ) then
          EnergyRes(LoopInd1)           = 0.0
          IndexPhaseChange(LoopInd1)    = 0
       endif
       if ( (IndexPhaseChange(LoopInd1) == 2) .and. (EnergyRes(LoopInd1) > 0.0) ) then
          EnergyRes(LoopInd1)           = 0.0
          IndexPhaseChange(LoopInd1)    = 0
       endif
       GlacierPhaseChg(LoopInd1)        = EnergyRes(LoopInd1) * MainTimeStep / ConstLatHeatFusion
    enddo

    ! The rate of melting for snow without a layer, needs more work.
    if ( OptGlacierTreatment == 2 ) then
       if ( (NumSnowLayerNeg == 0) .and. (SnowWaterEquiv > 0.0) .and. (TemperatureSoilSnow(1) > ConstFreezePoint) ) then
          EnergyRes(1)           = (TemperatureSoilSnow(1) - ConstFreezePoint) / PhaseChgFacSoilSnow(1)                     ! available heat
          TemperatureSoilSnow(1) = ConstFreezePoint                                                                         ! set T to freezing
          GlacierPhaseChg(1)     = EnergyRes(1) * MainTimeStep / ConstLatHeatFusion                                         ! total snow melt possible
          SnowWaterPrev          = SnowWaterEquiv
          SnowWaterEquiv         = max(0.0, SnowWaterPrev-GlacierPhaseChg(1))                                               ! snow remaining
          SnowWaterRatio         = SnowWaterEquiv / SnowWaterPrev                                                           ! fraction melted
          SnowDepth              = max(0.0, SnowWaterRatio*SnowDepth)                                                       ! new snow height
          SnowDepth              = min(max(SnowDepth,SnowWaterEquiv/500.0), SnowWaterEquiv/50.0)                            ! limit to a reasonable snow density
          EnergyResLeft(1)       = EnergyRes(1) - ConstLatHeatFusion * (SnowWaterPrev - SnowWaterEquiv) / MainTimeStep      ! excess heat
          if ( EnergyResLeft(1) > 0.0 ) then
             GlacierPhaseChg(1)     = EnergyResLeft(1) * MainTimeStep / ConstLatHeatFusion
             TemperatureSoilSnow(1) = TemperatureSoilSnow(1) + PhaseChgFacSoilSnow(1) * EnergyResLeft(1)                    ! re-heat ice
          else
             GlacierPhaseChg(1) = 0.0
             EnergyRes(1)       = 0.0
          endif
          MeltGroundSnow     = max(0.0, SnowWaterPrev-SnowWaterEquiv) / MainTimeStep                                        ! melted snow rate
          HeatLhTotPhsChg    = ConstLatHeatFusion * MeltGroundSnow                                                          ! melted snow energy
          PondSfcThinSnwMelt = SnowWaterPrev - SnowWaterEquiv                                                               ! melt water
       endif
    endif ! OptGlacierTreatment==2

    ! The rate of melting and freezing for multi-layer snow
    do LoopInd1 = NumSnowLayerNeg+1, 0
       if ( (IndexPhaseChange(LoopInd1) > 0) .and. (abs(EnergyRes(LoopInd1)) > 0.0) ) then
          EnergyResLeft(LoopInd1)    = 0.0
          if ( GlacierPhaseChg(LoopInd1) > 0.0 ) then
             MassWatIceTmp(LoopInd1) = max(0.0, MassWatIceInit(LoopInd1)-GlacierPhaseChg(LoopInd1))
             EnergyResLeft(LoopInd1) = EnergyRes(LoopInd1) - ConstLatHeatFusion * &
                                       (MassWatIceInit(LoopInd1) - MassWatIceTmp(LoopInd1)) / MainTimeStep
          elseif ( GlacierPhaseChg(LoopInd1) < 0.0 ) then
             MassWatIceTmp(LoopInd1) = min(MassWatTotInit(LoopInd1), MassWatIceInit(LoopInd1)-GlacierPhaseChg(LoopInd1))
             EnergyResLeft(LoopInd1) = EnergyRes(LoopInd1) - ConstLatHeatFusion * &
                                       (MassWatIceInit(LoopInd1) - MassWatIceTmp(LoopInd1)) / MainTimeStep
          endif
          MassWatLiqTmp(LoopInd1)    = max(0.0, MassWatTotInit(LoopInd1)-MassWatIceTmp(LoopInd1))                           ! update liquid water mass

          ! update snow temperature and energy surplus/loss
          if ( abs(EnergyResLeft(LoopInd1)) > 0.0 ) then
             TemperatureSoilSnow(LoopInd1) = TemperatureSoilSnow(LoopInd1) + &
                                             PhaseChgFacSoilSnow(LoopInd1) * EnergyResLeft(LoopInd1)
             if ( (MassWatLiqTmp(LoopInd1)*MassWatIceTmp(LoopInd1)) > 0.0 ) &
                TemperatureSoilSnow(LoopInd1) = ConstFreezePoint
          endif
          HeatLhTotPhsChg = HeatLhTotPhsChg + &
                            ConstLatHeatFusion * (MassWatIceInit(LoopInd1) - MassWatIceTmp(LoopInd1)) / MainTimeStep

          ! snow melting rate
          MeltGroundSnow  = MeltGroundSnow + max(0.0, (MassWatIceInit(LoopInd1)-MassWatIceTmp(LoopInd1))) / MainTimeStep
       endif
    enddo

    !---- glacier ice layer treatment

    if ( OptGlacierTreatment == 1 ) then

       ! ice layer water mass
       do LoopInd1 = 1, NumSoilLayer
          MassWatLiqTmp(LoopInd1) = SoilLiqWater(LoopInd1) * ThicknessSnowSoilLayer(LoopInd1) * 1000.0
          MassWatIceTmp(LoopInd1) = (SoilMoisture(LoopInd1) - SoilLiqWater(LoopInd1)) * ThicknessSnowSoilLayer(LoopInd1) * 1000.0
       enddo

       ! other required variables
       do LoopInd1 = 1, NumSoilLayer
          IndexPhaseChange(LoopInd1) = 0
          EnergyRes(LoopInd1)        = 0.0
          GlacierPhaseChg(LoopInd1)  = 0.0
          EnergyResLeft(LoopInd1)    = 0.0
          MassWatIceInit(LoopInd1)   = MassWatIceTmp(LoopInd1)
          MassWatLiqInit(LoopInd1)   = MassWatLiqTmp(LoopInd1)
          MassWatTotInit(LoopInd1)   = MassWatIceTmp(LoopInd1) + MassWatLiqTmp(LoopInd1)
       enddo

       ! determine melting or freezing state
       do LoopInd1 = 1, NumSoilLayer
          if ( (MassWatIceTmp(LoopInd1) > 0.0) .and. (TemperatureSoilSnow(LoopInd1) >= ConstFreezePoint) ) then
             IndexPhaseChange(LoopInd1) = 1  ! melting
          endif
          if ( (MassWatLiqTmp(LoopInd1) > 0.0) .and. (TemperatureSoilSnow(LoopInd1) < ConstFreezePoint) ) then
             IndexPhaseChange(LoopInd1) = 2  ! freezing
          endif
          ! If snow exists, but its thickness is not enough to create a layer
          if ( (NumSnowLayerNeg == 0) .and. (SnowWaterEquiv > 0.0) .and. (LoopInd1 == 1) ) then
             if ( TemperatureSoilSnow(LoopInd1) >= ConstFreezePoint ) then
                IndexPhaseChange(LoopInd1) = 1
             endif
          endif
       enddo

       ! Calculate the energy surplus and loss for melting and freezing
       do LoopInd1 = 1, NumSoilLayer
          if ( IndexPhaseChange(LoopInd1) > 0 ) then
             EnergyRes(LoopInd1)           = (TemperatureSoilSnow(LoopInd1) - ConstFreezePoint) / PhaseChgFacSoilSnow(LoopInd1)
             TemperatureSoilSnow(LoopInd1) = ConstFreezePoint
          endif
          if ( (IndexPhaseChange(LoopInd1) == 1) .and. (EnergyRes(LoopInd1) < 0.0) ) then
             EnergyRes(LoopInd1)        = 0.0
             IndexPhaseChange(LoopInd1) = 0
          endif
          if ( (IndexPhaseChange(LoopInd1) == 2) .and. (EnergyRes(LoopInd1) > 0.0) ) then
             EnergyRes(LoopInd1)        = 0.0
             IndexPhaseChange(LoopInd1) = 0
          endif
          GlacierPhaseChg(LoopInd1) = EnergyRes(LoopInd1) * MainTimeStep / ConstLatHeatFusion
       enddo

       ! The rate of melting for snow without a layer, needs more work.
       if ( (NumSnowLayerNeg == 0) .and. (SnowWaterEquiv > 0.0) .and. (GlacierPhaseChg(1) > 0.0) ) then
          SnowWaterPrev = SnowWaterEquiv
          SnowWaterEquiv     = max(0.0, SnowWaterPrev-GlacierPhaseChg(1))
          SnowWaterRatio   = SnowWaterEquiv / SnowWaterPrev
          SnowDepth          = max(0.0, SnowWaterRatio*SnowDepth)
          SnowDepth          = min(max(SnowDepth,SnowWaterEquiv/500.0), SnowWaterEquiv/50.0)  ! limit to a reasonable snow density
          EnergyResLeft(1)   = EnergyRes(1) - ConstLatHeatFusion * (SnowWaterPrev - SnowWaterEquiv) / MainTimeStep
          if ( EnergyResLeft(1) > 0.0 ) then
             GlacierPhaseChg(1)  = EnergyResLeft(1) * MainTimeStep / ConstLatHeatFusion
             EnergyRes(1)        = EnergyResLeft(1)
             IndexPhaseChange(1) = 1
          else
             GlacierPhaseChg(1)  = 0.0
             EnergyRes(1)        = 0.0
             IndexPhaseChange(1) = 0
          endif
          MeltGroundSnow         = max(0.0, (SnowWaterPrev-SnowWaterEquiv)) / MainTimeStep
          HeatLhTotPhsChg        = ConstLatHeatFusion * MeltGroundSnow
          PondSfcThinSnwMelt     = SnowWaterPrev - SnowWaterEquiv
       endif

       ! The rate of melting and freezing for glacier ice
       do LoopInd1 = 1, NumSoilLayer
          if ( (IndexPhaseChange(LoopInd1) > 0) .and. (abs(EnergyRes(LoopInd1)) > 0.0) ) then
             EnergyResLeft(LoopInd1)    = 0.0
             if ( GlacierPhaseChg(LoopInd1) > 0.0 ) then
                MassWatIceTmp(LoopInd1) = max(0.0, MassWatIceInit(LoopInd1)-GlacierPhaseChg(LoopInd1))
                EnergyResLeft(LoopInd1) = EnergyRes(LoopInd1) - ConstLatHeatFusion * &
                                          (MassWatIceInit(LoopInd1) - MassWatIceTmp(LoopInd1)) / MainTimeStep
             elseif ( GlacierPhaseChg(LoopInd1) < 0.0 ) then
                MassWatIceTmp(LoopInd1) = min(MassWatTotInit(LoopInd1), MassWatIceInit(LoopInd1)-GlacierPhaseChg(LoopInd1))
                EnergyResLeft(LoopInd1) = EnergyRes(LoopInd1) - ConstLatHeatFusion * &
                                          (MassWatIceInit(LoopInd1) - MassWatIceTmp(LoopInd1)) / MainTimeStep
             endif
             MassWatLiqTmp(LoopInd1)    = max(0.0, MassWatTotInit(LoopInd1)-MassWatIceTmp(LoopInd1)) ! update liquid water mass

             ! update ice temperature and energy surplus/loss
             if ( abs(EnergyResLeft(LoopInd1)) > 0.0 ) then
                TemperatureSoilSnow(LoopInd1) = TemperatureSoilSnow(LoopInd1) + &
                                                PhaseChgFacSoilSnow(LoopInd1) * EnergyResLeft(LoopInd1)
             endif
             HeatLhTotPhsChg = HeatLhTotPhsChg + &
                               ConstLatHeatFusion * (MassWatIceInit(LoopInd1) - MassWatIceTmp(LoopInd1)) / MainTimeStep
          endif
       enddo
       EnergyResLeft   = 0.0
       GlacierPhaseChg = 0.0

       !--- Deal with residuals in ice/soil

       ! first remove excess heat by reducing layer temperature
       if ( any(TemperatureSoilSnow(1:NumSoilLayer) > ConstFreezePoint) .and. &
            any(TemperatureSoilSnow(1:NumSoilLayer) < ConstFreezePoint) ) then
          do LoopInd1 = 1, NumSoilLayer
             if ( TemperatureSoilSnow(LoopInd1) > ConstFreezePoint ) then
                EnergyResLeft(LoopInd1) = (TemperatureSoilSnow(LoopInd1) - ConstFreezePoint) / PhaseChgFacSoilSnow(LoopInd1)
                do LoopInd2 = 1, NumSoilLayer
                   if ( (LoopInd1 /= LoopInd2) .and. (TemperatureSoilSnow(LoopInd2) < ConstFreezePoint) .and. &
                        (EnergyResLeft(LoopInd1) > 0.1) ) then
                      EnergyResLeft(LoopInd2) = (TemperatureSoilSnow(LoopInd2) - ConstFreezePoint) / &
                                                PhaseChgFacSoilSnow(LoopInd2)
                      if ( abs(EnergyResLeft(LoopInd2)) > EnergyResLeft(LoopInd1) ) then ! LAYER ABSORBS ALL
                         EnergyResLeft(LoopInd2)       = EnergyResLeft(LoopInd2) + EnergyResLeft(LoopInd1)
                         TemperatureSoilSnow(LoopInd2) = ConstFreezePoint + &
                                                         EnergyResLeft(LoopInd2) * PhaseChgFacSoilSnow(LoopInd2)
                         EnergyResLeft(LoopInd1)       = 0.0
                      else
                         EnergyResLeft(LoopInd1)       = EnergyResLeft(LoopInd1) + EnergyResLeft(LoopInd2)
                         EnergyResLeft(LoopInd2)       = 0.0
                         TemperatureSoilSnow(LoopInd2) = ConstFreezePoint
                      endif
                   endif
                enddo
                TemperatureSoilSnow(LoopInd1) = ConstFreezePoint + EnergyResLeft(LoopInd1) * PhaseChgFacSoilSnow(LoopInd1)
             endif
          enddo
       endif

       ! now remove excess cold by increasing temperture (may not be necessary with above loop)
       if ( any(TemperatureSoilSnow(1:NumSoilLayer) > ConstFreezePoint) .and. &
            any(TemperatureSoilSnow(1:NumSoilLayer) < ConstFreezePoint) ) then
          do LoopInd1 = 1, NumSoilLayer
             if ( TemperatureSoilSnow(LoopInd1) < ConstFreezePoint ) then
                EnergyResLeft(LoopInd1) = (TemperatureSoilSnow(LoopInd1) - ConstFreezePoint) / PhaseChgFacSoilSnow(LoopInd1)
                do LoopInd2 = 1, NumSoilLayer
                   if ( (LoopInd1 /= LoopInd2) .and. (TemperatureSoilSnow(LoopInd2) > ConstFreezePoint) .and. &
                        (EnergyResLeft(LoopInd1) < -0.1) ) then
                      EnergyResLeft(LoopInd2) = (TemperatureSoilSnow(LoopInd2) - ConstFreezePoint) / &
                                                PhaseChgFacSoilSnow(LoopInd2)
                      if ( EnergyResLeft(LoopInd2) > abs(EnergyResLeft(LoopInd1)) ) then  ! LAYER ABSORBS ALL
                         EnergyResLeft(LoopInd2)       = EnergyResLeft(LoopInd2) + EnergyResLeft(LoopInd1)
                         TemperatureSoilSnow(LoopInd2) = ConstFreezePoint + &
                                                         EnergyResLeft(LoopInd2) * PhaseChgFacSoilSnow(LoopInd2)
                         EnergyResLeft(LoopInd1)       = 0.0
                      else
                         EnergyResLeft(LoopInd1)       = EnergyResLeft(LoopInd1) + EnergyResLeft(LoopInd2)
                         EnergyResLeft(LoopInd2)       = 0.0
                         TemperatureSoilSnow(LoopInd2) = ConstFreezePoint
                      endif
                   endif
                enddo
                TemperatureSoilSnow(LoopInd1) = ConstFreezePoint + EnergyResLeft(LoopInd1) * PhaseChgFacSoilSnow(LoopInd1)
             endif
          enddo
       endif

       ! now remove excess heat by melting ice
       if ( any(TemperatureSoilSnow(1:NumSoilLayer) > ConstFreezePoint) .and. &
            any(MassWatIceTmp(1:NumSoilLayer) > 0.0) ) then
          do LoopInd1 = 1, NumSoilLayer
             if ( TemperatureSoilSnow(LoopInd1) > ConstFreezePoint ) then
                EnergyResLeft(LoopInd1)   = (TemperatureSoilSnow(LoopInd1) - ConstFreezePoint) / PhaseChgFacSoilSnow(LoopInd1)
                GlacierPhaseChg(LoopInd1) = EnergyResLeft(LoopInd1) * MainTimeStep / ConstLatHeatFusion
                do LoopInd2 = 1, NumSoilLayer
                   if ( (LoopInd1 /= LoopInd2) .and. (MassWatIceTmp(LoopInd2) > 0.0) .and. &
                        (GlacierPhaseChg(LoopInd1) > 0.1) ) then
                      if ( MassWatIceTmp(LoopInd2) > GlacierPhaseChg(LoopInd1) ) then  ! LAYER ABSORBS ALL
                         MassWatIceTmp(LoopInd2)       = MassWatIceTmp(LoopInd2) - GlacierPhaseChg(LoopInd1)
                         HeatLhTotPhsChg               = HeatLhTotPhsChg + &
                                                         ConstLatHeatFusion * GlacierPhaseChg(LoopInd1)/MainTimeStep
                         TemperatureSoilSnow(LoopInd2) = ConstFreezePoint
                         GlacierPhaseChg(LoopInd1)     = 0.0
                      else
                         GlacierPhaseChg(LoopInd1)     = GlacierPhaseChg(LoopInd1) - MassWatIceTmp(LoopInd2)
                         HeatLhTotPhsChg               = HeatLhTotPhsChg + &
                                                         ConstLatHeatFusion * MassWatIceTmp(LoopInd2) / MainTimeStep
                         MassWatIceTmp(LoopInd2)       = 0.0
                         TemperatureSoilSnow(LoopInd2) = ConstFreezePoint
                      endif
                      MassWatLiqTmp(LoopInd2) = max(0.0, MassWatTotInit(LoopInd2)-MassWatIceTmp(LoopInd2))
                   endif
                enddo
                EnergyResLeft(LoopInd1)       = GlacierPhaseChg(LoopInd1) * ConstLatHeatFusion / MainTimeStep
                TemperatureSoilSnow(LoopInd1) = ConstFreezePoint + EnergyResLeft(LoopInd1) * PhaseChgFacSoilSnow(LoopInd1)
             endif
          enddo
       endif

       ! snow remove excess cold by refreezing liquid (may not be necessary with above loop)
       if ( any(TemperatureSoilSnow(1:NumSoilLayer) < ConstFreezePoint) .and. &
            any(MassWatLiqTmp(1:NumSoilLayer) > 0.0) ) then
          do LoopInd1 = 1, NumSoilLayer
             if ( TemperatureSoilSnow(LoopInd1) < ConstFreezePoint ) then
                EnergyResLeft(LoopInd1)   = (TemperatureSoilSnow(LoopInd1) - ConstFreezePoint) / PhaseChgFacSoilSnow(LoopInd1)
                GlacierPhaseChg(LoopInd1) = EnergyResLeft(LoopInd1) * MainTimeStep / ConstLatHeatFusion
                do LoopInd2 = 1, NumSoilLayer
                   if ( (LoopInd1 /= LoopInd2) .and. (MassWatLiqTmp(LoopInd2) > 0.0) .and. &
                        (GlacierPhaseChg(LoopInd1) < -0.1) ) then
                      if ( MassWatLiqTmp(LoopInd2) > abs(GlacierPhaseChg(LoopInd1)) ) then  ! LAYER ABSORBS ALL
                         MassWatIceTmp(LoopInd2)       = MassWatIceTmp(LoopInd2) - GlacierPhaseChg(LoopInd1)
                         HeatLhTotPhsChg               = HeatLhTotPhsChg + &
                                                         ConstLatHeatFusion * GlacierPhaseChg(LoopInd1) / MainTimeStep
                         TemperatureSoilSnow(LoopInd2) = ConstFreezePoint
                         GlacierPhaseChg(LoopInd1)     = 0.0
                      else
                         GlacierPhaseChg(LoopInd1)     = GlacierPhaseChg(LoopInd1) + MassWatLiqTmp(LoopInd2)
                         HeatLhTotPhsChg               = HeatLhTotPhsChg - &
                                                         ConstLatHeatFusion * MassWatLiqTmp(LoopInd2) / MainTimeStep
                         MassWatIceTmp(LoopInd2)       = MassWatTotInit(LoopInd2)
                         TemperatureSoilSnow(LoopInd2) = ConstFreezePoint
                      endif
                      MassWatLiqTmp(LoopInd2) = max(0.0, MassWatTotInit(LoopInd2)-MassWatIceTmp(LoopInd2))
                   endif
                enddo
                EnergyResLeft(LoopInd1)       = GlacierPhaseChg(LoopInd1) * ConstLatHeatFusion / MainTimeStep
                TemperatureSoilSnow(LoopInd1) = ConstFreezePoint + EnergyResLeft(LoopInd1) * PhaseChgFacSoilSnow(LoopInd1)
             endif
          enddo
       endif

    endif ! OptGlacierTreatment==1

    !--- update snow and soil ice and liquid content
    do LoopInd1 = NumSnowLayerNeg+1, 0     ! snow
       SnowLiqWater(LoopInd1) = MassWatLiqTmp(LoopInd1)
       SnowIce(LoopInd1)      = MassWatIceTmp(LoopInd1)
    enddo
    do LoopInd1 = 1, NumSoilLayer       ! glacier ice
       if ( OptGlacierTreatment == 1 ) then
          SoilLiqWater(LoopInd1) = MassWatLiqTmp(LoopInd1) / (1000.0 * ThicknessSnowSoilLayer(LoopInd1))
          SoilLiqWater(LoopInd1) = max(0.0, min(1.0,SoilLiqWater(LoopInd1)))
       elseif ( OptGlacierTreatment == 2 ) then
          SoilLiqWater(LoopInd1) = 0.0             ! ice, assume all frozen forever
       endif
       SoilMoisture(LoopInd1)    = 1.0
    enddo

    ! deallocate local arrays to avoid memory leaks
    deallocate(EnergyRes      )
    deallocate(GlacierPhaseChg)
    deallocate(MassWatTotInit )
    deallocate(MassWatIceInit )
    deallocate(MassWatLiqInit )
    deallocate(MassWatIceTmp  )
    deallocate(MassWatLiqTmp  )
    deallocate(EnergyResLeft  )

    end associate

  end subroutine GlacierPhaseChange

end module GlacierPhaseChangeMod
