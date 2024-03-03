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
              J               => noahmp%config%domain%GridIndexJ      ,&
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
       NoahmpIO%QFX(I,J)                = noahmp%water%flux%EvapGroundNet
    endif

    if ( IndicatorIceSfc == 0 ) then ! land soil point
       NoahmpIO%QFX(I,J) = noahmp%water%flux%EvapCanopyNet + noahmp%water%flux%EvapGroundNet + &
                           noahmp%water%flux%Transpiration + noahmp%water%flux%EvapIrriSprinkler
    endif

    NoahmpIO%SMSTAV      (I,J) = 0.0  ! [maintained as Noah consistency] water
    NoahmpIO%SMSTOT      (I,J) = 0.0  ! [maintained as Noah consistency] water
    NoahmpIO%SFCRUNOFF   (I,J) = NoahmpIO%SFCRUNOFF(I,J) + noahmp%water%flux%RunoffSurface
    NoahmpIO%UDRUNOFF    (I,J) = NoahmpIO%UDRUNOFF (I,J) + noahmp%water%flux%RunoffSubsurface
    NoahmpIO%QTDRAIN     (I,J) = NoahmpIO%QTDRAIN  (I,J) + noahmp%water%flux%TileDrain
    NoahmpIO%SNOWC       (I,J) = noahmp%water%state%SnowCoverFrac
    NoahmpIO%SNOW        (I,J) = noahmp%water%state%SnowWaterEquiv
    NoahmpIO%SNOWH       (I,J) = noahmp%water%state%SnowDepth
    NoahmpIO%CANWAT      (I,J) = noahmp%water%state%CanopyLiqWater + noahmp%water%state%CanopyIce
    NoahmpIO%ACSNOW      (I,J) = NoahmpIO%ACSNOW(I,J) + (NoahmpIO%RAINBL (I,J) * noahmp%water%state%FrozenPrecipFrac)
    NoahmpIO%ACSNOM      (I,J) = NoahmpIO%ACSNOM(I,J) + (noahmp%water%flux%MeltGroundSnow * NoahmpIO%DTBL) + &
                                 noahmp%water%state%PondSfcThinSnwMelt + noahmp%water%state%PondSfcThinSnwComb + &
                                 noahmp%water%state%PondSfcThinSnwTrans
    NoahmpIO%CANLIQXY    (I,J) = noahmp%water%state%CanopyLiqWater
    NoahmpIO%CANICEXY    (I,J) = noahmp%water%state%CanopyIce
    NoahmpIO%FWETXY      (I,J) = noahmp%water%state%CanopyWetFrac
    NoahmpIO%SNEQVOXY    (I,J) = noahmp%water%state%SnowWaterEquivPrev
    NoahmpIO%QSNOWXY     (I,J) = noahmp%water%flux%SnowfallGround
    NoahmpIO%QRAINXY     (I,J) = noahmp%water%flux%RainfallGround
    NoahmpIO%WSLAKEXY    (I,J) = noahmp%water%state%WaterStorageLake
    NoahmpIO%ZWTXY       (I,J) = noahmp%water%state%WaterTableDepth
    NoahmpIO%WAXY        (I,J) = noahmp%water%state%WaterStorageAquifer
    NoahmpIO%WTXY        (I,J) = noahmp%water%state%WaterStorageSoilAqf
    NoahmpIO%RUNSFXY     (I,J) = noahmp%water%flux%RunoffSurface
    NoahmpIO%RUNSBXY     (I,J) = noahmp%water%flux%RunoffSubsurface
    NoahmpIO%ECANXY      (I,J) = noahmp%water%flux%EvapCanopyNet
    NoahmpIO%EDIRXY      (I,J) = noahmp%water%flux%EvapGroundNet
    NoahmpIO%ETRANXY     (I,J) = noahmp%water%flux%Transpiration
    NoahmpIO%QINTSXY     (I,J) = noahmp%water%flux%InterceptCanopySnow
    NoahmpIO%QINTRXY     (I,J) = noahmp%water%flux%InterceptCanopyRain
    NoahmpIO%QDRIPSXY    (I,J) = noahmp%water%flux%DripCanopySnow
    NoahmpIO%QDRIPRXY    (I,J) = noahmp%water%flux%DripCanopyRain
    NoahmpIO%QTHROSXY    (I,J) = noahmp%water%flux%ThroughfallSnow
    NoahmpIO%QTHRORXY    (I,J) = noahmp%water%flux%ThroughfallRain
    NoahmpIO%QSNSUBXY    (I,J) = noahmp%water%flux%SublimSnowSfcIce
    NoahmpIO%QSNFROXY    (I,J) = noahmp%water%flux%FrostSnowSfcIce
    NoahmpIO%QSUBCXY     (I,J) = noahmp%water%flux%SublimCanopyIce
    NoahmpIO%QFROCXY     (I,J) = noahmp%water%flux%FrostCanopyIce
    NoahmpIO%QEVACXY     (I,J) = noahmp%water%flux%EvapCanopyLiq
    NoahmpIO%QDEWCXY     (I,J) = noahmp%water%flux%DewCanopyLiq
    NoahmpIO%QFRZCXY     (I,J) = noahmp%water%flux%FreezeCanopyLiq
    NoahmpIO%QMELTCXY    (I,J) = noahmp%water%flux%MeltCanopyIce
    NoahmpIO%QSNBOTXY    (I,J) = noahmp%water%flux%SnowBotOutflow
    NoahmpIO%QMELTXY     (I,J) = noahmp%water%flux%MeltGroundSnow
    NoahmpIO%PONDINGXY   (I,J) = noahmp%water%state%PondSfcThinSnwTrans + &
                                 noahmp%water%state%PondSfcThinSnwComb + noahmp%water%state%PondSfcThinSnwMelt
    NoahmpIO%FPICEXY     (I,J) = noahmp%water%state%FrozenPrecipFrac
    NoahmpIO%RAINLSM     (I,J) = noahmp%water%flux%RainfallRefHeight
    NoahmpIO%SNOWLSM     (I,J) = noahmp%water%flux%SnowfallRefHeight
    NoahmpIO%ACC_QINSURXY(I,J) = noahmp%water%flux%SoilSfcInflowAcc
    NoahmpIO%ACC_QSEVAXY (I,J) = noahmp%water%flux%EvapSoilSfcLiqAcc
    NoahmpIO%ACC_DWATERXY(I,J) = noahmp%water%flux%SfcWaterTotChgAcc
    NoahmpIO%ACC_PRCPXY  (I,J) = noahmp%water%flux%PrecipTotAcc
    NoahmpIO%ACC_ECANXY  (I,J) = noahmp%water%flux%EvapCanopyNetAcc
    NoahmpIO%ACC_ETRANXY (I,J) = noahmp%water%flux%TranspirationAcc
    NoahmpIO%ACC_EDIRXY  (I,J) = noahmp%water%flux%EvapGroundNetAcc
    NoahmpIO%RECHXY      (I,J) = NoahmpIO%RECHXY(I,J) + (noahmp%water%state%RechargeGwShallowWT*1.0e3)
    NoahmpIO%DEEPRECHXY  (I,J) = NoahmpIO%DEEPRECHXY(I,J) + noahmp%water%state%RechargeGwDeepWT
    NoahmpIO%SMCWTDXY    (I,J) = noahmp%water%state%SoilMoistureToWT
    NoahmpIO%SMOIS       (I,1:NumSoilLayer,J)       = noahmp%water%state%SoilMoisture(1:NumSoilLayer)
    NoahmpIO%SH2O        (I,1:NumSoilLayer,J)       = noahmp%water%state%SoilLiqWater(1:NumSoilLayer)
    NoahmpIO%ACC_ETRANIXY(I,1:NumSoilLayer,J)       = noahmp%water%flux%TranspWatLossSoilAcc(1:NumSoilLayer)
    NoahmpIO%SNICEXY     (I,-NumSnowLayerMax+1:0,J) = noahmp%water%state%SnowIce(-NumSnowLayerMax+1:0)
    NoahmpIO%SNLIQXY     (I,-NumSnowLayerMax+1:0,J) = noahmp%water%state%SnowLiqWater(-NumSnowLayerMax+1:0)

    ! irrigation
    NoahmpIO%IRNUMSI   (I,J) = noahmp%water%state%IrrigationCntSprinkler
    NoahmpIO%IRNUMMI   (I,J) = noahmp%water%state%IrrigationCntMicro
    NoahmpIO%IRNUMFI   (I,J) = noahmp%water%state%IrrigationCntFlood
    NoahmpIO%IRWATSI   (I,J) = noahmp%water%state%IrrigationAmtSprinkler
    NoahmpIO%IRWATMI   (I,J) = noahmp%water%state%IrrigationAmtMicro
    NoahmpIO%IRWATFI   (I,J) = noahmp%water%state%IrrigationAmtFlood
    NoahmpIO%IRSIVOL   (I,J) = NoahmpIO%IRSIVOL(I,J)+(noahmp%water%flux%IrrigationRateSprinkler*1000.0)
    NoahmpIO%IRMIVOL   (I,J) = NoahmpIO%IRMIVOL(I,J)+(noahmp%water%flux%IrrigationRateMicro*1000.0)
    NoahmpIO%IRFIVOL   (I,J) = NoahmpIO%IRFIVOL(I,J)+(noahmp%water%flux%IrrigationRateFlood*1000.0)
    NoahmpIO%IRELOSS   (I,J) = NoahmpIO%IRELOSS(I,J)+(noahmp%water%flux%EvapIrriSprinkler*NoahmpIO%DTBL)

#ifdef WRF_HYDRO
    NoahmpIO%infxsrt   (I,J) = max(noahmp%water%flux%RunoffSurface, 0.0)               ! mm, surface runoff
    NoahmpIO%soldrain  (I,J) = max(noahmp%water%flux%RunoffSubsurface, 0.0)            ! mm, underground runoff
    NoahmpIO%qtiledrain(I,J) = max(noahmp%water%flux%TileDrain, 0.0)                   ! mm, tile drainage
#endif

    end associate

  end subroutine WaterVarOutTransfer

end module WaterVarOutTransferMod
