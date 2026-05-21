module WaterVarOutTransferMod

!!! Transfer column (1-D) Noah-MP water variables to 2D NoahmpIO for output

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType
  use NoahmpVarType

  implicit none

contains

!=== Transfer model states to output =====

  subroutine WaterVarOutTransfer(noahmp, NoahmpIO)

    implicit none

    type(noahmp_type),   intent(inout) :: noahmp
    type(NoahmpIO_type), intent(inout) :: NoahmpIO

! -------------------------------------------------------------------------
    associate(                                                         &
              I               => noahmp%config%domain%GridIndexI      ,&
              NumSnowLayerMax => noahmp%config%domain%NumSnowLayerMax ,&
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer    ,&
              IndicatorIceSfc => noahmp%config%domain%IndicatorIceSfc  &
             )
! -------------------------------------------------------------------------

    ! special treatment for glacier point output
    if ( IndicatorIceSfc == -1 ) then ! land ice point
       noahmp%water%state%SnowCoverFrac      = 1.0
       noahmp%water%flux%EvapCanopyNet       = 0.0
       noahmp%water%flux%Transpiration       = 0.0
       noahmp%water%flux%InterceptCanopySnow = 0.0
       noahmp%water%flux%InterceptCanopyRain = 0.0
       noahmp%water%flux%DripCanopySnow      = 0.0
       noahmp%water%flux%DripCanopyRain      = 0.0
       noahmp%water%flux%ThroughfallSnow     = noahmp%water%flux%SnowfallRefHeight
       noahmp%water%flux%ThroughfallRain     = noahmp%water%flux%RainfallRefHeight
       noahmp%water%flux%SublimCanopyIce     = 0.0
       noahmp%water%flux%FrostCanopyIce      = 0.0
       noahmp%water%flux%FreezeCanopyLiq     = 0.0
       noahmp%water%flux%MeltCanopyIce       = 0.0
       noahmp%water%flux%EvapCanopyLiq       = 0.0
       noahmp%water%flux%DewCanopyLiq        = 0.0
       noahmp%water%state%CanopyIce          = 0.0
       noahmp%water%state%CanopyLiqWater     = 0.0
       noahmp%water%flux%TileDrain           = 0.0
       noahmp%water%flux%RunoffSurface       = noahmp%water%flux%RunoffSurface * noahmp%config%domain%MainTimeStep
       noahmp%water%flux%RunoffSubsurface    = noahmp%water%flux%RunoffSubsurface * noahmp%config%domain%MainTimeStep
       NoahmpIO%QFX(I)                       = noahmp%water%flux%EvapGroundNet
    endif

    if ( IndicatorIceSfc == 0 ) then ! land soil point
       NoahmpIO%QFX(I) = noahmp%water%flux%EvapCanopyNet + noahmp%water%flux%EvapGroundNet + &
                         noahmp%water%flux%Transpiration + noahmp%water%flux%EvapIrriSprinkler
    endif

    NoahmpIO%SMSTAV      (I) = 0.0  ! [maintained as Noah consistency] water
    NoahmpIO%SMSTOT      (I) = 0.0  ! [maintained as Noah consistency] water
    NoahmpIO%SFCRUNOFF   (I) = NoahmpIO%SFCRUNOFF(I) + noahmp%water%flux%RunoffSurface
    NoahmpIO%UDRUNOFF    (I) = NoahmpIO%UDRUNOFF (I) + noahmp%water%flux%RunoffSubsurface
    NoahmpIO%QTDRAIN     (I) = NoahmpIO%QTDRAIN  (I) + noahmp%water%flux%TileDrain
    NoahmpIO%SNOWC       (I) = noahmp%water%state%SnowCoverFrac
    NoahmpIO%SNOW        (I) = noahmp%water%state%SnowWaterEquiv
    NoahmpIO%SNOWH       (I) = noahmp%water%state%SnowDepth
    NoahmpIO%CANWAT      (I) = noahmp%water%state%CanopyLiqWater + noahmp%water%state%CanopyIce
    NoahmpIO%ACSNOW      (I) = NoahmpIO%ACSNOW(I) + (NoahmpIO%RAINBL (I) * noahmp%water%state%FrozenPrecipFrac)
    NoahmpIO%ACSNOM      (I) = NoahmpIO%ACSNOM(I) + (noahmp%water%flux%MeltGroundSnow * NoahmpIO%DTBL) + &
                                 noahmp%water%state%PondSfcThinSnwMelt + noahmp%water%state%PondSfcThinSnwComb + &
                                 noahmp%water%state%PondSfcThinSnwTrans
    NoahmpIO%CANLIQXY    (I) = noahmp%water%state%CanopyLiqWater
    NoahmpIO%CANICEXY    (I) = noahmp%water%state%CanopyIce
    NoahmpIO%FWETXY      (I) = noahmp%water%state%CanopyWetFrac
    NoahmpIO%SNEQVOXY    (I) = noahmp%water%state%SnowWaterEquivPrev
    NoahmpIO%QSNOWXY     (I) = noahmp%water%flux%SnowfallGround
    NoahmpIO%QRAINXY     (I) = noahmp%water%flux%RainfallGround
    NoahmpIO%WSLAKEXY    (I) = noahmp%water%state%WaterStorageLake
    NoahmpIO%ZWTXY       (I) = noahmp%water%state%WaterTableDepth
    NoahmpIO%WAXY        (I) = noahmp%water%state%WaterStorageAquifer
    NoahmpIO%WTXY        (I) = noahmp%water%state%WaterStorageSoilAqf
    NoahmpIO%RUNSFXY     (I) = noahmp%water%flux%RunoffSurface
    NoahmpIO%RUNSBXY     (I) = noahmp%water%flux%RunoffSubsurface
    NoahmpIO%ECANXY      (I) = noahmp%water%flux%EvapCanopyNet
    NoahmpIO%EDIRXY      (I) = noahmp%water%flux%EvapGroundNet
    NoahmpIO%ETRANXY     (I) = noahmp%water%flux%Transpiration
    NoahmpIO%QINTSXY     (I) = noahmp%water%flux%InterceptCanopySnow
    NoahmpIO%QINTRXY     (I) = noahmp%water%flux%InterceptCanopyRain
    NoahmpIO%QDRIPSXY    (I) = noahmp%water%flux%DripCanopySnow
    NoahmpIO%QDRIPRXY    (I) = noahmp%water%flux%DripCanopyRain
    NoahmpIO%QTHROSXY    (I) = noahmp%water%flux%ThroughfallSnow
    NoahmpIO%QTHRORXY    (I) = noahmp%water%flux%ThroughfallRain
    NoahmpIO%QSNSUBXY    (I) = noahmp%water%flux%SublimSnowSfcIce
    NoahmpIO%QSNFROXY    (I) = noahmp%water%flux%FrostSnowSfcIce
    NoahmpIO%QSUBCXY     (I) = noahmp%water%flux%SublimCanopyIce
    NoahmpIO%QFROCXY     (I) = noahmp%water%flux%FrostCanopyIce
    NoahmpIO%QEVACXY     (I) = noahmp%water%flux%EvapCanopyLiq
    NoahmpIO%QDEWCXY     (I) = noahmp%water%flux%DewCanopyLiq
    NoahmpIO%QFRZCXY     (I) = noahmp%water%flux%FreezeCanopyLiq
    NoahmpIO%QMELTCXY    (I) = noahmp%water%flux%MeltCanopyIce
    NoahmpIO%QSNBOTXY    (I) = noahmp%water%flux%SnowBotOutflow
    NoahmpIO%QMELTXY     (I) = noahmp%water%flux%MeltGroundSnow
    NoahmpIO%PONDINGXY   (I) = noahmp%water%state%PondSfcThinSnwTrans + &
                                 noahmp%water%state%PondSfcThinSnwComb + noahmp%water%state%PondSfcThinSnwMelt
    NoahmpIO%FPICEXY     (I) = noahmp%water%state%FrozenPrecipFrac
    NoahmpIO%RAINLSM     (I) = noahmp%water%flux%RainfallRefHeight
    NoahmpIO%SNOWLSM     (I) = noahmp%water%flux%SnowfallRefHeight
    NoahmpIO%ACC_QINSURXY(I) = noahmp%water%flux%SoilSfcInflowAcc
    NoahmpIO%ACC_QSEVAXY (I) = noahmp%water%flux%EvapSoilSfcLiqAcc
    NoahmpIO%ACC_DWATERXY(I) = noahmp%water%flux%SfcWaterTotChgAcc
    NoahmpIO%ACC_PRCPXY  (I) = noahmp%water%flux%PrecipTotAcc
    NoahmpIO%ACC_ECANXY  (I) = noahmp%water%flux%EvapCanopyNetAcc
    NoahmpIO%ACC_ETRANXY (I) = noahmp%water%flux%TranspirationAcc
    NoahmpIO%ACC_EDIRXY  (I) = noahmp%water%flux%EvapGroundNetAcc
    NoahmpIO%RECHXY      (I) = NoahmpIO%RECHXY(I) + (noahmp%water%state%RechargeGwShallowWT*1.0e3)
    NoahmpIO%DEEPRECHXY  (I) = NoahmpIO%DEEPRECHXY(I) + noahmp%water%state%RechargeGwDeepWT
    NoahmpIO%SMCWTDXY    (I) = noahmp%water%state%SoilMoistureToWT
    NoahmpIO%SMOIS       (I,1:NumSoilLayer)       = noahmp%water%state%SoilMoisture(1:NumSoilLayer)
    NoahmpIO%SH2O        (I,1:NumSoilLayer)       = noahmp%water%state%SoilLiqWater(1:NumSoilLayer)
    NoahmpIO%ACC_ETRANIXY(I,1:NumSoilLayer)       = noahmp%water%flux%TranspWatLossSoilAcc(1:NumSoilLayer)
    NoahmpIO%SNICEXY     (I,-NumSnowLayerMax+1:0) = noahmp%water%state%SnowIce(-NumSnowLayerMax+1:0)
    NoahmpIO%SNLIQXY     (I,-NumSnowLayerMax+1:0) = noahmp%water%state%SnowLiqWater(-NumSnowLayerMax+1:0)

    ! irrigation
    NoahmpIO%IRNUMSI   (I) = noahmp%water%state%IrrigationCntSprinkler
    NoahmpIO%IRNUMMI   (I) = noahmp%water%state%IrrigationCntMicro
    NoahmpIO%IRNUMFI   (I) = noahmp%water%state%IrrigationCntFlood
    NoahmpIO%IRWATSI   (I) = noahmp%water%state%IrrigationAmtSprinkler
    NoahmpIO%IRWATMI   (I) = noahmp%water%state%IrrigationAmtMicro
    NoahmpIO%IRWATFI   (I) = noahmp%water%state%IrrigationAmtFlood
    NoahmpIO%IRSIVOL   (I) = NoahmpIO%IRSIVOL(I)+(noahmp%water%flux%IrrigationRateSprinkler*1000.0)
    NoahmpIO%IRMIVOL   (I) = NoahmpIO%IRMIVOL(I)+(noahmp%water%flux%IrrigationRateMicro*1000.0)
    NoahmpIO%IRFIVOL   (I) = NoahmpIO%IRFIVOL(I)+(noahmp%water%flux%IrrigationRateFlood*1000.0)
    NoahmpIO%IRELOSS   (I) = NoahmpIO%IRELOSS(I)+(noahmp%water%flux%EvapIrriSprinkler*NoahmpIO%DTBL)

#ifdef WRF_HYDRO
    NoahmpIO%infxsrt   (I) = max(noahmp%water%flux%RunoffSurface, 0.0)               ! mm, surface runoff
    NoahmpIO%soldrain  (I) = max(noahmp%water%flux%RunoffSubsurface, 0.0)            ! mm, underground runoff
    NoahmpIO%qtiledrain(I) = max(noahmp%water%flux%TileDrain, 0.0)                   ! mm, tile drainage
#endif

    end associate

  end subroutine WaterVarOutTransfer

end module WaterVarOutTransferMod
