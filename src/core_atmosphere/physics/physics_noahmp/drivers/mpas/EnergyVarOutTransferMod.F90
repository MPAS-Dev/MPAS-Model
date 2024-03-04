module EnergyVarOutTransferMod

!!! Transfer column (1-D) Noah-MP Energy variables to 2D NoahmpIO for output

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

  subroutine EnergyVarOutTransfer(noahmp, NoahmpIO)

    implicit none

    type(NoahmpIO_type), intent(inout) :: NoahmpIO
    type(noahmp_type),   intent(inout) :: noahmp

    ! local variables
    integer                          :: LoopInd                   ! snow/soil layer loop index
    real(kind=kind_noahmp)           :: LeafAreaIndSunlit         ! sunlit leaf area index [m2/m2]
    real(kind=kind_noahmp)           :: LeafAreaIndShade          ! shaded leaf area index [m2/m2]
    real(kind=kind_noahmp)           :: ResistanceLeafBoundary    ! leaf boundary layer resistance [s/m]
    real(kind=kind_noahmp)           :: ThicknessSnowSoilLayer    ! temporary snow/soil layer thickness [m]

!-----------------------------------------------------------------------
    associate(                                                         &
              I               => noahmp%config%domain%GridIndexI      ,&
              J               => noahmp%config%domain%GridIndexJ      ,&
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer    ,&
              NumSnowLayerMax => noahmp%config%domain%NumSnowLayerMax ,&
              NumSnowLayerNeg => noahmp%config%domain%NumSnowLayerNeg ,&
              IndicatorIceSfc => noahmp%config%domain%IndicatorIceSfc  &
             )
!-----------------------------------------------------------------------

    ! special treatment for glacier point output
    if ( IndicatorIceSfc == -1 ) then ! land ice point
       noahmp%energy%state%VegFrac             = 0.0
       noahmp%energy%state%RoughLenMomSfcToAtm = 0.002
       noahmp%energy%flux%RadSwAbsVeg          = 0.0
       noahmp%energy%flux%RadLwNetCanopy       = 0.0
       noahmp%energy%flux%RadLwNetVegGrd       = 0.0
       noahmp%energy%flux%HeatSensibleCanopy   = 0.0
       noahmp%energy%flux%HeatSensibleVegGrd   = 0.0
       noahmp%energy%flux%HeatLatentVegGrd     = 0.0
       noahmp%energy%flux%HeatGroundVegGrd     = 0.0
       noahmp%energy%flux%HeatCanStorageChg    = 0.0
       noahmp%energy%flux%HeatLatentCanTransp  = 0.0
       noahmp%energy%flux%HeatLatentCanEvap    = 0.0
       noahmp%energy%flux%HeatPrecipAdvCanopy  = 0.0
       noahmp%energy%flux%HeatPrecipAdvVegGrd  = 0.0
       noahmp%energy%flux%HeatLatentCanopy     = 0.0
       noahmp%energy%flux%HeatLatentTransp     = 0.0
       noahmp%energy%flux%RadLwNetBareGrd      = noahmp%energy%flux%RadLwNetSfc
       noahmp%energy%flux%HeatSensibleBareGrd  = noahmp%energy%flux%HeatSensibleSfc
       noahmp%energy%flux%HeatLatentBareGrd    = noahmp%energy%flux%HeatLatentGrd
       noahmp%energy%flux%HeatGroundBareGrd    = noahmp%energy%flux%HeatGroundTot
       noahmp%energy%state%TemperatureGrdBare  = noahmp%energy%state%TemperatureGrd
       noahmp%energy%state%ExchCoeffShBare     = noahmp%energy%state%ExchCoeffShSfc
       NoahmpIO%LH(I,J)                        = noahmp%energy%flux%HeatLatentGrd
    endif

    if ( IndicatorIceSfc == 0 ) then ! land soil point
       NoahmpIO%LH(I,J) = noahmp%energy%flux%HeatLatentGrd + noahmp%energy%flux%HeatLatentCanopy + &
                          noahmp%energy%flux%HeatLatentTransp + noahmp%energy%flux%HeatLatentIrriEvap 
    endif

    ! energy flux variables
    NoahmpIO%HFX        (I,J) = noahmp%energy%flux%HeatSensibleSfc
    NoahmpIO%GRDFLX     (I,J) = noahmp%energy%flux%HeatGroundTot
    NoahmpIO%FSAXY      (I,J) = noahmp%energy%flux%RadSwAbsSfc
    NoahmpIO%FIRAXY     (I,J) = noahmp%energy%flux%RadLwNetSfc
    NoahmpIO%APARXY     (I,J) = noahmp%energy%flux%RadPhotoActAbsCan
    NoahmpIO%SAVXY      (I,J) = noahmp%energy%flux%RadSwAbsVeg
    NoahmpIO%SAGXY      (I,J) = noahmp%energy%flux%RadSwAbsGrd
    NoahmpIO%IRCXY      (I,J) = noahmp%energy%flux%RadLwNetCanopy
    NoahmpIO%IRGXY      (I,J) = noahmp%energy%flux%RadLwNetVegGrd
    NoahmpIO%SHCXY      (I,J) = noahmp%energy%flux%HeatSensibleCanopy
    NoahmpIO%SHGXY      (I,J) = noahmp%energy%flux%HeatSensibleVegGrd
    NoahmpIO%EVGXY      (I,J) = noahmp%energy%flux%HeatLatentVegGrd
    NoahmpIO%GHVXY      (I,J) = noahmp%energy%flux%HeatGroundVegGrd
    NoahmpIO%IRBXY      (I,J) = noahmp%energy%flux%RadLwNetBareGrd
    NoahmpIO%SHBXY      (I,J) = noahmp%energy%flux%HeatSensibleBareGrd
    NoahmpIO%EVBXY      (I,J) = noahmp%energy%flux%HeatLatentBareGrd
    NoahmpIO%GHBXY      (I,J) = noahmp%energy%flux%HeatGroundBareGrd
    NoahmpIO%TRXY       (I,J) = noahmp%energy%flux%HeatLatentCanTransp
    NoahmpIO%EVCXY      (I,J) = noahmp%energy%flux%HeatLatentCanEvap
    NoahmpIO%CANHSXY    (I,J) = noahmp%energy%flux%HeatCanStorageChg
    NoahmpIO%PAHXY      (I,J) = noahmp%energy%flux%HeatPrecipAdvSfc
    NoahmpIO%PAHGXY     (I,J) = noahmp%energy%flux%HeatPrecipAdvVegGrd
    NoahmpIO%PAHVXY     (I,J) = noahmp%energy%flux%HeatPrecipAdvCanopy
    NoahmpIO%PAHBXY     (I,J) = noahmp%energy%flux%HeatPrecipAdvBareGrd
    NoahmpIO%ACC_SSOILXY(I,J) = noahmp%energy%flux%HeatGroundTotAcc
    NoahmpIO%EFLXBXY    (I,J) = noahmp%energy%flux%HeatFromSoilBot

    ! energy state variables
    NoahmpIO%TSK     (I,J) = noahmp%energy%state%TemperatureRadSfc
    NoahmpIO%EMISS   (I,J) = noahmp%energy%state%EmissivitySfc
    NoahmpIO%QSFC    (I,J) = noahmp%energy%state%SpecHumiditySfcMean
    NoahmpIO%TVXY    (I,J) = noahmp%energy%state%TemperatureCanopy
    NoahmpIO%TGXY    (I,J) = noahmp%energy%state%TemperatureGrd
    NoahmpIO%EAHXY   (I,J) = noahmp%energy%state%PressureVaporCanAir
    NoahmpIO%TAHXY   (I,J) = noahmp%energy%state%TemperatureCanopyAir
    NoahmpIO%CMXY    (I,J) = noahmp%energy%state%ExchCoeffMomSfc
    NoahmpIO%CHXY    (I,J) = noahmp%energy%state%ExchCoeffShSfc
    NoahmpIO%ALBOLDXY(I,J) = noahmp%energy%state%AlbedoSnowPrev
    NoahmpIO%LAI     (I,J) = noahmp%energy%state%LeafAreaIndex
    NoahmpIO%XSAIXY  (I,J) = noahmp%energy%state%StemAreaIndex
    NoahmpIO%TAUSSXY (I,J) = noahmp%energy%state%SnowAgeNondim
    NoahmpIO%Z0      (I,J) = noahmp%energy%state%RoughLenMomSfcToAtm
    NoahmpIO%ZNT     (I,J) = noahmp%energy%state%RoughLenMomSfcToAtm
    NoahmpIO%T2MVXY  (I,J) = noahmp%energy%state%TemperatureAir2mVeg
    NoahmpIO%T2MBXY  (I,J) = noahmp%energy%state%TemperatureAir2mBare
    NoahmpIO%TRADXY  (I,J) = noahmp%energy%state%TemperatureRadSfc
    NoahmpIO%FVEGXY  (I,J) = noahmp%energy%state%VegFrac
    NoahmpIO%RSSUNXY (I,J) = noahmp%energy%state%ResistanceStomataSunlit
    NoahmpIO%RSSHAXY (I,J) = noahmp%energy%state%ResistanceStomataShade
    NoahmpIO%BGAPXY  (I,J) = noahmp%energy%state%GapBtwCanopy
    NoahmpIO%WGAPXY  (I,J) = noahmp%energy%state%GapInCanopy
    NoahmpIO%TGVXY   (I,J) = noahmp%energy%state%TemperatureGrdVeg
    NoahmpIO%TGBXY   (I,J) = noahmp%energy%state%TemperatureGrdBare
    NoahmpIO%CHVXY   (I,J) = noahmp%energy%state%ExchCoeffShAbvCan
    NoahmpIO%CHBXY   (I,J) = noahmp%energy%state%ExchCoeffShBare
    NoahmpIO%CHLEAFXY(I,J) = noahmp%energy%state%ExchCoeffShLeaf
    NoahmpIO%CHUCXY  (I,J) = noahmp%energy%state%ExchCoeffShUndCan
    NoahmpIO%CHV2XY  (I,J) = noahmp%energy%state%ExchCoeffSh2mVeg
    NoahmpIO%CHB2XY  (I,J) = noahmp%energy%state%ExchCoeffSh2mBare
    NoahmpIO%Q2MVXY  (I,J) = noahmp%energy%state%SpecHumidity2mVeg /(1.0-noahmp%energy%state%SpecHumidity2mVeg)  ! spec humidity to mixing ratio
    NoahmpIO%Q2MBXY  (I,J) = noahmp%energy%state%SpecHumidity2mBare/(1.0-noahmp%energy%state%SpecHumidity2mBare)
    NoahmpIO%ALBEDO  (I,J) = noahmp%energy%state%AlbedoSfc
    NoahmpIO%IRRSPLH (I,J) = NoahmpIO%IRRSPLH(I,J) + &
                             (noahmp%energy%flux%HeatLatentIrriEvap * noahmp%config%domain%MainTimeStep)
    NoahmpIO%TSLB    (I,1:NumSoilLayer,J)       = noahmp%energy%state%TemperatureSoilSnow(1:NumSoilLayer)
    NoahmpIO%TSNOXY  (I,-NumSnowLayerMax+1:0,J) = noahmp%energy%state%TemperatureSoilSnow(-NumSnowLayerMax+1:0)

    ! New Calculation of total Canopy/Stomatal Conductance Based on Bonan et al. (2011), Inverse of Canopy Resistance (below)
    LeafAreaIndSunlit      = max(noahmp%energy%state%LeafAreaIndSunlit, 0.0)
    LeafAreaIndShade       = max(noahmp%energy%state%LeafAreaIndShade, 0.0)
    ResistanceLeafBoundary = max(noahmp%energy%state%ResistanceLeafBoundary, 0.0)
    if ( (noahmp%energy%state%ResistanceStomataSunlit <= 0.0) .or. (noahmp%energy%state%ResistanceStomataShade <= 0.0) .or. &
         (LeafAreaIndSunlit == 0.0) .or. (LeafAreaIndShade == 0.0)       .or. &
         (noahmp%energy%state%ResistanceStomataSunlit == undefined_real) .or. &
         (noahmp%energy%state%ResistanceStomataShade == undefined_real) ) then
       NoahmpIO%RS   (I,J) = 0.0
    else
       NoahmpIO%RS   (I,J) = ((1.0 / (noahmp%energy%state%ResistanceStomataSunlit + ResistanceLeafBoundary) * &
                              noahmp%energy%state%LeafAreaIndSunlit) + &
                             ((1.0 / (noahmp%energy%state%ResistanceStomataShade + ResistanceLeafBoundary)) * &
                              noahmp%energy%state%LeafAreaIndShade))
       NoahmpIO%RS   (I,J) = 1.0 / NoahmpIO%RS (I,J) ! Resistance
    endif

    ! calculation of snow and soil energy storage
    NoahmpIO%SNOWENERGY(I,J) = 0.0
    NoahmpIO%SOILENERGY(I,J) = 0.0
    do LoopInd = NumSnowLayerNeg+1, NumSoilLayer
       if ( LoopInd == NumSnowLayerNeg+1 ) then
          ThicknessSnowSoilLayer = -noahmp%config%domain%DepthSnowSoilLayer(LoopInd)
       else
          ThicknessSnowSoilLayer = noahmp%config%domain%DepthSnowSoilLayer(LoopInd-1) - &
                                   noahmp%config%domain%DepthSnowSoilLayer(LoopInd)
       endif
       if ( LoopInd >= 1 ) then
          NoahmpIO%SOILENERGY(I,J) = NoahmpIO%SOILENERGY(I,J) + ThicknessSnowSoilLayer * &
                                     noahmp%energy%state%HeatCapacSoilSnow(LoopInd) * &
                                     (noahmp%energy%state%TemperatureSoilSnow(LoopInd) - 273.16) * 0.001
       else
          NoahmpIO%SNOWENERGY(I,J) = NoahmpIO%SNOWENERGY(I,J) + ThicknessSnowSoilLayer * &
                                     noahmp%energy%state%HeatCapacSoilSnow(LoopInd) * &
                                     (noahmp%energy%state%TemperatureSoilSnow(LoopInd) - 273.16) * 0.001
       endif
    enddo

    end associate

  end subroutine EnergyVarOutTransfer

end module EnergyVarOutTransferMod
