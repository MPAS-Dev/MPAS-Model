module WaterVarInTransferMod

!!! Transfer input 2-D NoahmpIO Water variables to 1-D column variable
!!! 1-D variables should be first defined in /src/WaterVarType.F90
!!! 2-D variables should be first defined in NoahmpIOVarType.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType
  use NoahmpVarType
  use PedoTransferSR2006Mod

  implicit none

contains

!=== initialize with input data or table values

  subroutine WaterVarInTransfer(noahmp, NoahmpIO)

    implicit none

    type(noahmp_type),   intent(inout) :: noahmp
    type(NoahmpIO_type), intent(inout) :: NoahmpIO

    ! local variables 
    integer                            :: IndexSoilLayer
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilSand
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilClay
    real(kind=kind_noahmp), allocatable, dimension(:) :: SoilOrg

! -------------------------------------------------------------------------
    associate(                                                         &
              I               => noahmp%config%domain%GridIndexI      ,&
              NumSnowLayerMax => noahmp%config%domain%NumSnowLayerMax ,&
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer    ,&
              VegType         => noahmp%config%domain%VegType         ,&
              SoilType        => noahmp%config%domain%SoilType        ,&
              FlagUrban       => noahmp%config%domain%FlagUrban       ,&
              RunoffSlopeType => noahmp%config%domain%RunoffSlopeType ,&
              NumSnowLayerNeg => noahmp%config%domain%NumSnowLayerNeg  &
             )
! -------------------------------------------------------------------------

    ! water state variables
    noahmp%water%state%CanopyLiqWater                     = NoahmpIO%CANLIQXY   (I)
    noahmp%water%state%CanopyIce                          = NoahmpIO%CANICEXY   (I)
    noahmp%water%state%CanopyWetFrac                      = NoahmpIO%FWETXY     (I)
    noahmp%water%state%SnowWaterEquiv                     = NoahmpIO%SNOW       (I)
    noahmp%water%state%SnowWaterEquivPrev                 = NoahmpIO%SNEQVOXY   (I) 
    noahmp%water%state%SnowDepth                          = NoahmpIO%SNOWH      (I)
    noahmp%water%state%IrrigationFracFlood                = NoahmpIO%FIFRACT    (I)
    noahmp%water%state%IrrigationAmtFlood                 = NoahmpIO%IRWATFI    (I)
    noahmp%water%state%IrrigationFracMicro                = NoahmpIO%MIFRACT    (I)
    noahmp%water%state%IrrigationAmtMicro                 = NoahmpIO%IRWATMI    (I) 
    noahmp%water%state%IrrigationFracSprinkler            = NoahmpIO%SIFRACT    (I)
    noahmp%water%state%IrrigationAmtSprinkler             = NoahmpIO%IRWATSI    (I)  
    noahmp%water%state%WaterTableDepth                    = NoahmpIO%ZWTXY      (I) 
    noahmp%water%state%SoilMoistureToWT                   = NoahmpIO%SMCWTDXY   (I)
    noahmp%water%state%TileDrainFrac                      = NoahmpIO%TD_FRACTION(I)
    noahmp%water%state%WaterStorageAquifer                = NoahmpIO%WAXY       (I)
    noahmp%water%state%WaterStorageSoilAqf                = NoahmpIO%WTXY       (I)
    noahmp%water%state%WaterStorageLake                   = NoahmpIO%WSLAKEXY   (I)
    noahmp%water%state%IrrigationFracGrid                 = NoahmpIO%IRFRACT    (I)
    noahmp%water%state%IrrigationCntSprinkler             = NoahmpIO%IRNUMSI    (I)     
    noahmp%water%state%IrrigationCntMicro                 = NoahmpIO%IRNUMMI    (I)
    noahmp%water%state%IrrigationCntFlood                 = NoahmpIO%IRNUMFI    (I)
    noahmp%water%state%SnowIce     (-NumSnowLayerMax+1:0) = NoahmpIO%SNICEXY    (I,-NumSnowLayerMax+1:0)
    noahmp%water%state%SnowLiqWater(-NumSnowLayerMax+1:0) = NoahmpIO%SNLIQXY    (I,-NumSnowLayerMax+1:0)
    noahmp%water%state%SoilLiqWater      (1:NumSoilLayer) = NoahmpIO%SH2O       (I,1:NumSoilLayer)
    noahmp%water%state%SoilMoisture      (1:NumSoilLayer) = NoahmpIO%SMOIS      (I,1:NumSoilLayer)    
    noahmp%water%state%SoilMoistureEqui  (1:NumSoilLayer) = NoahmpIO%SMOISEQ    (I,1:NumSoilLayer)
    noahmp%water%state%RechargeGwDeepWT                   = 0.0
    noahmp%water%state%RechargeGwShallowWT                = 0.0
#ifdef WRF_HYDRO
    noahmp%water%state%WaterTableHydro                    = NoahmpIO%ZWATBLE2D  (I)
    noahmp%water%state%WaterHeadSfc                       = NoahmpIO%sfcheadrt  (I)
#endif

    ! water flux variables
    noahmp%water%flux%EvapSoilSfcLiqAcc                   = NoahmpIO%ACC_QSEVAXY (I)
    noahmp%water%flux%SoilSfcInflowAcc                    = NoahmpIO%ACC_QINSURXY(I)
    noahmp%water%flux%SfcWaterTotChgAcc                   = NoahmpIO%ACC_DWATERXY(I)
    noahmp%water%flux%PrecipTotAcc                        = NoahmpIO%ACC_PRCPXY  (I)
    noahmp%water%flux%EvapCanopyNetAcc                    = NoahmpIO%ACC_ECANXY  (I)
    noahmp%water%flux%TranspirationAcc                    = NoahmpIO%ACC_ETRANXY (I)
    noahmp%water%flux%EvapGroundNetAcc                    = NoahmpIO%ACC_EDIRXY  (I)
    noahmp%water%flux%TranspWatLossSoilAcc(1:NumSoilLayer)= NoahmpIO%ACC_ETRANIXY(I,1:NumSoilLayer)

    ! water parameter variables
    noahmp%water%param%DrainSoilLayerInd                  = NoahmpIO%DRAIN_LAYER_OPT_TABLE
    noahmp%water%param%CanopyLiqHoldCap                   = NoahmpIO%CH2OP_TABLE(VegType)
    noahmp%water%param%SnowCompactBurdenFac               = NoahmpIO%C2_SNOWCOMPACT_TABLE
    noahmp%water%param%SnowCompactAgingFac1               = NoahmpIO%C3_SNOWCOMPACT_TABLE
    noahmp%water%param%SnowCompactAgingFac2               = NoahmpIO%C4_SNOWCOMPACT_TABLE
    noahmp%water%param%SnowCompactAgingFac3               = NoahmpIO%C5_SNOWCOMPACT_TABLE
    noahmp%water%param%SnowCompactAgingMax                = NoahmpIO%DM_SNOWCOMPACT_TABLE
    noahmp%water%param%SnowViscosityCoeff                 = NoahmpIO%ETA0_SNOWCOMPACT_TABLE
    noahmp%water%param%SnowLiqFracMax                     = NoahmpIO%SNLIQMAXFRAC_TABLE
    noahmp%water%param%SnowLiqHoldCap                     = NoahmpIO%SSI_TABLE
    noahmp%water%param%SnowLiqReleaseFac                  = NoahmpIO%SNOW_RET_FAC_TABLE
    noahmp%water%param%IrriFloodRateFac                   = NoahmpIO%FIRTFAC_TABLE
    noahmp%water%param%IrriMicroRate                      = NoahmpIO%MICIR_RATE_TABLE
    noahmp%water%param%SoilConductivityRef                = NoahmpIO%REFDK_TABLE
    noahmp%water%param%SoilInfilFacRef                    = NoahmpIO%REFKDT_TABLE
    noahmp%water%param%GroundFrzCoeff                     = NoahmpIO%FRZK_TABLE
    noahmp%water%param%GridTopoIndex                      = NoahmpIO%TIMEAN_TABLE
    noahmp%water%param%SoilSfcSatFracMax                  = NoahmpIO%FSATMX_TABLE
    noahmp%water%param%SpecYieldGw                        = NoahmpIO%ROUS_TABLE
    noahmp%water%param%MicroPoreContent                   = NoahmpIO%CMIC_TABLE
    noahmp%water%param%WaterStorageLakeMax                = NoahmpIO%WSLMAX_TABLE
    noahmp%water%param%SnoWatEqvMaxGlacier                = NoahmpIO%SWEMAXGLA_TABLE
    noahmp%water%param%IrriStopDayBfHarvest               = NoahmpIO%IRR_HAR_TABLE
    noahmp%water%param%IrriTriggerLaiMin                  = NoahmpIO%IRR_LAI_TABLE
    noahmp%water%param%SoilWatDeficitAllow                = NoahmpIO%IRR_MAD_TABLE
    noahmp%water%param%IrriFloodLossFrac                  = NoahmpIO%FILOSS_TABLE
    noahmp%water%param%IrriSprinklerRate                  = NoahmpIO%SPRIR_RATE_TABLE
    noahmp%water%param%IrriFracThreshold                  = NoahmpIO%IRR_FRAC_TABLE
    noahmp%water%param%IrriStopPrecipThr                  = NoahmpIO%IR_RAIN_TABLE
    noahmp%water%param%SnowfallDensityMax                 = NoahmpIO%SNOWDEN_MAX_TABLE
    noahmp%water%param%SnowMassFullCoverOld               = NoahmpIO%SWEMX_TABLE
    noahmp%water%param%SoilMatPotentialWilt               = NoahmpIO%PSIWLT_TABLE
    noahmp%water%param%SnowMeltFac                        = NoahmpIO%MFSNO_TABLE(VegType)
    noahmp%water%param%SnowCoverFac                       = NoahmpIO%SCFFAC_TABLE(VegType)
    noahmp%water%param%InfilFacVic                        = NoahmpIO%BVIC_TABLE(SoilType(1))
    noahmp%water%param%TensionWatDistrInfl                = NoahmpIO%AXAJ_TABLE(SoilType(1))
    noahmp%water%param%TensionWatDistrShp                 = NoahmpIO%BXAJ_TABLE(SoilType(1))
    noahmp%water%param%FreeWatDistrShp                    = NoahmpIO%XXAJ_TABLE(SoilType(1))
    noahmp%water%param%InfilHeteroDynVic                  = NoahmpIO%BBVIC_TABLE(SoilType(1))
    noahmp%water%param%InfilCapillaryDynVic               = NoahmpIO%GDVIC_TABLE(SoilType(1))
    noahmp%water%param%InfilFacDynVic                     = NoahmpIO%BDVIC_TABLE(SoilType(1))
    noahmp%water%param%TileDrainCoeffSp                   = NoahmpIO%TD_DC_TABLE(SoilType(1))
    noahmp%water%param%TileDrainTubeDepth                 = NoahmpIO%TD_DEPTH_TABLE(SoilType(1))
    noahmp%water%param%DrainFacSoilWat                    = NoahmpIO%TDSMC_FAC_TABLE(SoilType(1))
    noahmp%water%param%TileDrainCoeff                     = NoahmpIO%TD_DCOEF_TABLE(SoilType(1))
    noahmp%water%param%DrainDepthToImperv                 = NoahmpIO%TD_ADEPTH_TABLE(SoilType(1))
    noahmp%water%param%LateralWatCondFac                  = NoahmpIO%KLAT_FAC_TABLE(SoilType(1))
    noahmp%water%param%TileDrainDepth                     = NoahmpIO%TD_DDRAIN_TABLE(SoilType(1))
    noahmp%water%param%DrainTubeDist                      = NoahmpIO%TD_SPAC_TABLE(SoilType(1))
    noahmp%water%param%DrainTubeRadius                    = NoahmpIO%TD_RADI_TABLE(SoilType(1))
    noahmp%water%param%DrainWatDepToImperv                = NoahmpIO%TD_D_TABLE(SoilType(1))
    noahmp%water%param%NumSoilLayerRoot                   = NoahmpIO%NROOT_TABLE(VegType)
    noahmp%water%param%SoilDrainSlope                     = NoahmpIO%SLOPE_TABLE(RunoffSlopeType)

    do IndexSoilLayer = 1, size(SoilType)
       noahmp%water%param%SoilMoistureSat       (IndexSoilLayer) = NoahmpIO%SMCMAX_TABLE(SoilType(IndexSoilLayer))
       noahmp%water%param%SoilMoistureWilt      (IndexSoilLayer) = NoahmpIO%SMCWLT_TABLE(SoilType(IndexSoilLayer))
       noahmp%water%param%SoilMoistureFieldCap  (IndexSoilLayer) = NoahmpIO%SMCREF_TABLE(SoilType(IndexSoilLayer))
       noahmp%water%param%SoilMoistureDry       (IndexSoilLayer) = NoahmpIO%SMCDRY_TABLE(SoilType(IndexSoilLayer))
       noahmp%water%param%SoilWatDiffusivitySat (IndexSoilLayer) = NoahmpIO%DWSAT_TABLE (SoilType(IndexSoilLayer))
       noahmp%water%param%SoilWatConductivitySat(IndexSoilLayer) = NoahmpIO%DKSAT_TABLE (SoilType(IndexSoilLayer))
       noahmp%water%param%SoilExpCoeffB         (IndexSoilLayer) = NoahmpIO%BEXP_TABLE  (SoilType(IndexSoilLayer))
       noahmp%water%param%SoilMatPotentialSat   (IndexSoilLayer) = NoahmpIO%PSISAT_TABLE(SoilType(IndexSoilLayer))
    enddo
   
    ! spatial varying soil texture and properties directly from input
    if ( noahmp%config%nmlist%OptSoilProperty == 4 ) then
       ! 3D soil properties
       noahmp%water%param%SoilExpCoeffB          = NoahmpIO%BEXP_3D  (I,1:NumSoilLayer) ! C-H B exponent
       noahmp%water%param%SoilMoistureDry        = NoahmpIO%SMCDRY_3D(I,1:NumSoilLayer) ! Soil Moisture Limit: Dry
       noahmp%water%param%SoilMoistureWilt       = NoahmpIO%SMCWLT_3D(I,1:NumSoilLayer) ! Soil Moisture Limit: Wilt
       noahmp%water%param%SoilMoistureFieldCap   = NoahmpIO%SMCREF_3D(I,1:NumSoilLayer) ! Soil Moisture Limit: Reference
       noahmp%water%param%SoilMoistureSat        = NoahmpIO%SMCMAX_3D(I,1:NumSoilLayer) ! Soil Moisture Limit: Max
       noahmp%water%param%SoilWatConductivitySat = NoahmpIO%DKSAT_3D (I,1:NumSoilLayer) ! Saturated Soil Conductivity
       noahmp%water%param%SoilWatDiffusivitySat  = NoahmpIO%DWSAT_3D (I,1:NumSoilLayer) ! Saturated Soil Diffusivity
       noahmp%water%param%SoilMatPotentialSat    = NoahmpIO%PSISAT_3D(I,1:NumSoilLayer) ! Saturated Matric Potential
       noahmp%water%param%SoilConductivityRef    = NoahmpIO%REFDK_2D (I)                ! Reference Soil Conductivity
       noahmp%water%param%SoilInfilFacRef        = NoahmpIO%REFKDT_2D(I)                ! Soil Infiltration Parameter
       ! 2D additional runoff6~8 parameters
       noahmp%water%param%InfilFacVic            = NoahmpIO%BVIC_2D (I)                 ! VIC model infiltration parameter
       noahmp%water%param%TensionWatDistrInfl    = NoahmpIO%AXAJ_2D (I)                 ! Xinanjiang: Tension water distribution inflection parameter
       noahmp%water%param%TensionWatDistrShp     = NoahmpIO%BXAJ_2D (I)                 ! Xinanjiang: Tension water distribution shape parameter
       noahmp%water%param%FreeWatDistrShp        = NoahmpIO%XXAJ_2D (I)                 ! Xinanjiang: Free water distribution shape parameter
       noahmp%water%param%InfilFacDynVic         = NoahmpIO%BDVIC_2D(I)                 ! VIC model infiltration parameter
       noahmp%water%param%InfilCapillaryDynVic   = NoahmpIO%GDVIC_2D(I)                 ! Mean Capillary Drive for infiltration models
       noahmp%water%param%InfilHeteroDynVic      = NoahmpIO%BBVIC_2D(I)                 ! DVIC heterogeniety parameter for infiltraton
       ! 2D irrigation params
       noahmp%water%param%IrriFracThreshold      = NoahmpIO%IRR_FRAC_2D  (I)            ! irrigation Fraction
       noahmp%water%param%IrriStopDayBfHarvest   = NoahmpIO%IRR_HAR_2D   (I)            ! number of days before harvest date to stop irrigation 
       noahmp%water%param%IrriTriggerLaiMin      = NoahmpIO%IRR_LAI_2D   (I)            ! Minimum lai to trigger irrigation
       noahmp%water%param%SoilWatDeficitAllow    = NoahmpIO%IRR_MAD_2D   (I)            ! management allowable deficit (0-1)
       noahmp%water%param%IrriFloodLossFrac      = NoahmpIO%FILOSS_2D    (I)            ! fraction of flood irrigation loss (0-1) 
       noahmp%water%param%IrriSprinklerRate      = NoahmpIO%SPRIR_RATE_2D(I)            ! mm/h, sprinkler irrigation rate
       noahmp%water%param%IrriMicroRate          = NoahmpIO%MICIR_RATE_2D(I)            ! mm/h, micro irrigation rate
       noahmp%water%param%IrriFloodRateFac       = NoahmpIO%FIRTFAC_2D   (I)            ! flood application rate factor
       noahmp%water%param%IrriStopPrecipThr      = NoahmpIO%IR_RAIN_2D   (I)            ! maximum precipitation to stop irrigation trigger
       ! 2D tile drainage parameters
       noahmp%water%param%LateralWatCondFac      = NoahmpIO%KLAT_FAC (I)                ! factor multiplier to hydraulic conductivity
       noahmp%water%param%DrainFacSoilWat        = NoahmpIO%TDSMC_FAC(I)                ! factor multiplier to field capacity
       noahmp%water%param%TileDrainCoeffSp       = NoahmpIO%TD_DC    (I)                ! drainage coefficient for simple
       noahmp%water%param%TileDrainCoeff         = NoahmpIO%TD_DCOEF (I)                ! drainge coefficient for Hooghoudt 
       noahmp%water%param%TileDrainDepth         = NoahmpIO%TD_DDRAIN(I)                ! depth of drain
       noahmp%water%param%DrainTubeRadius        = NoahmpIO%TD_RADI  (I)                ! tile tube radius
       noahmp%water%param%DrainTubeDist          = NoahmpIO%TD_SPAC  (I)                ! tile spacing
    endif

    ! derived water parameters
    noahmp%water%param%SoilInfilMaxCoeff  = noahmp%water%param%SoilInfilFacRef *           &
                                            noahmp%water%param%SoilWatConductivitySat(1) / &
                                            noahmp%water%param%SoilConductivityRef
    if ( FlagUrban .eqv. .true. ) then
       noahmp%water%param%SoilMoistureSat      = 0.45
       noahmp%water%param%SoilMoistureFieldCap = 0.42
       noahmp%water%param%SoilMoistureWilt     = 0.40
       noahmp%water%param%SoilMoistureDry      = 0.40
    endif

    if ( SoilType(1) /= 14 ) then
       noahmp%water%param%SoilImpervFracCoeff = noahmp%water%param%GroundFrzCoeff *       &
                                                ((noahmp%water%param%SoilMoistureSat(1) / &
                                                 noahmp%water%param%SoilMoistureFieldCap(1)) * (0.412/0.468))
    endif

    noahmp%water%state%SnowIceFracPrev = 0.0
    noahmp%water%state%SnowIceFracPrev(NumSnowLayerNeg+1:0) = NoahmpIO%SNICEXY(I,NumSnowLayerNeg+1:0) /  & 
                                                              (NoahmpIO%SNICEXY(I,NumSnowLayerNeg+1:0) + &
                                                               NoahmpIO%SNLIQXY(I,NumSnowLayerNeg+1:0))

    if ( (noahmp%config%nmlist%OptSoilProperty == 3) .and. (.not. noahmp%config%domain%FlagUrban) ) then
       if (.not. allocated(SoilSand)) allocate( SoilSand(1:NumSoilLayer) )
       if (.not. allocated(SoilClay)) allocate( SoilClay(1:NumSoilLayer) )
       if (.not. allocated(SoilOrg) ) allocate( SoilOrg (1:NumSoilLayer) )
       SoilSand = 0.01 * NoahmpIO%soilcomp(I,1:NumSoilLayer)
       SoilClay = 0.01 * NoahmpIO%soilcomp(I,(NumSoilLayer+1):(NumSoilLayer*2))
       SoilOrg  = 0.0
       if (noahmp%config%nmlist%OptPedotransfer == 1) &
          call PedoTransferSR2006(NoahmpIO,noahmp,SoilSand,SoilClay,SoilOrg)
       deallocate(SoilSand)
       deallocate(SoilClay)
       deallocate(SoilOrg )
    endif

    end associate

  end subroutine WaterVarInTransfer

end module WaterVarInTransferMod
