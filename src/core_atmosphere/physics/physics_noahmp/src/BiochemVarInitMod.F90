module BiochemVarInitMod

!!! Initialize column (1-D) Noah-MP biochemistry (carbon,nitrogen,etc) variables
!!! Biochemistry variables should be first defined in BiochemVarType.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

  use Machine
  use NoahmpVarType

  implicit none

contains

!=== initialize with default values

  subroutine BiochemVarInitDefault(noahmp)

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

    associate( NumCropGrowStage => noahmp%config%domain%NumCropGrowStage )

    ! biochem state variables
    noahmp%biochem%state%PlantGrowStage             = undefined_int
    noahmp%biochem%state%IndexPlanting              = undefined_int
    noahmp%biochem%state%IndexHarvest               = undefined_int
    noahmp%biochem%state%IndexGrowSeason            = undefined_real
    noahmp%biochem%state%NitrogenConcFoliage        = undefined_real
    noahmp%biochem%state%LeafMass                   = undefined_real
    noahmp%biochem%state%RootMass                   = undefined_real
    noahmp%biochem%state%StemMass                   = undefined_real
    noahmp%biochem%state%WoodMass                   = undefined_real
    noahmp%biochem%state%CarbonMassDeepSoil         = undefined_real
    noahmp%biochem%state%CarbonMassShallowSoil      = undefined_real
    noahmp%biochem%state%CarbonMassSoilTot          = undefined_real
    noahmp%biochem%state%CarbonMassLiveTot          = undefined_real
    noahmp%biochem%state%LeafAreaPerMass            = undefined_real
    noahmp%biochem%state%StemAreaPerMass            = undefined_real
    noahmp%biochem%state%LeafMassMin                = undefined_real
    noahmp%biochem%state%StemMassMin                = undefined_real
    noahmp%biochem%state%CarbonFracToLeaf           = undefined_real
    noahmp%biochem%state%CarbonFracToRoot           = undefined_real
    noahmp%biochem%state%CarbonFracToWood           = undefined_real
    noahmp%biochem%state%CarbonFracToStem           = undefined_real
    noahmp%biochem%state%WoodCarbonFrac             = undefined_real
    noahmp%biochem%state%CarbonFracToWoodRoot       = undefined_real
    noahmp%biochem%state%MicroRespFactorSoilWater   = undefined_real
    noahmp%biochem%state%MicroRespFactorSoilTemp    = undefined_real
    noahmp%biochem%state%RespFacNitrogenFoliage     = undefined_real
    noahmp%biochem%state%RespFacTemperature         = undefined_real
    noahmp%biochem%state%RespReductionFac           = undefined_real
    noahmp%biochem%state%GrainMass                  = undefined_real
    noahmp%biochem%state%GrowDegreeDay              = undefined_real

    ! biochem flux variables
    noahmp%biochem%flux%PhotosynLeafSunlit          = undefined_real
    noahmp%biochem%flux%PhotosynLeafShade           = undefined_real
    noahmp%biochem%flux%PhotosynCrop                = undefined_real
    noahmp%biochem%flux%PhotosynTotal               = undefined_real
    noahmp%biochem%flux%GrossPriProduction          = undefined_real
    noahmp%biochem%flux%NetPriProductionTot         = undefined_real
    noahmp%biochem%flux%NetEcoExchange              = undefined_real
    noahmp%biochem%flux%RespirationPlantTot         = undefined_real
    noahmp%biochem%flux%RespirationSoilOrg          = undefined_real
    noahmp%biochem%flux%CarbonToAtmos               = undefined_real
    noahmp%biochem%flux%NetPriProductionLeaf        = undefined_real
    noahmp%biochem%flux%NetPriProductionRoot        = undefined_real
    noahmp%biochem%flux%NetPriProductionWood        = undefined_real
    noahmp%biochem%flux%NetPriProductionStem        = undefined_real
    noahmp%biochem%flux%GrowthRespLeaf              = undefined_real
    noahmp%biochem%flux%GrowthRespRoot              = undefined_real
    noahmp%biochem%flux%GrowthRespWood              = undefined_real
    noahmp%biochem%flux%GrowthRespStem              = undefined_real
    noahmp%biochem%flux%LeafMassMaxChg              = undefined_real
    noahmp%biochem%flux%StemMassMaxChg              = undefined_real
    noahmp%biochem%flux%CarbonDecayToStable         = undefined_real
    noahmp%biochem%flux%RespirationLeaf             = undefined_real
    noahmp%biochem%flux%RespirationStem             = undefined_real
    noahmp%biochem%flux%GrowthRespGrain             = undefined_real
    noahmp%biochem%flux%NetPriProductionGrain       = undefined_real
    noahmp%biochem%flux%ConvRootToGrain             = undefined_real
    noahmp%biochem%flux%ConvStemToGrain             = undefined_real
    noahmp%biochem%flux%RespirationWood             = undefined_real
    noahmp%biochem%flux%RespirationLeafMaint        = undefined_real
    noahmp%biochem%flux%RespirationRoot             = undefined_real
    noahmp%biochem%flux%DeathLeaf                   = undefined_real
    noahmp%biochem%flux%DeathStem                   = undefined_real
    noahmp%biochem%flux%CarbonAssim                 = undefined_real
    noahmp%biochem%flux%TurnoverLeaf                = undefined_real
    noahmp%biochem%flux%TurnoverStem                = undefined_real
    noahmp%biochem%flux%TurnoverWood                = undefined_real
    noahmp%biochem%flux%RespirationSoil             = undefined_real
    noahmp%biochem%flux%TurnoverRoot                = undefined_real
    noahmp%biochem%flux%CarbohydrAssim              = undefined_real
    noahmp%biochem%flux%TurnoverGrain               = undefined_real
    noahmp%biochem%flux%ConvLeafToGrain             = undefined_real
    noahmp%biochem%flux%RespirationGrain            = undefined_real
 
    ! biochem parameter variables
    noahmp%biochem%param%DatePlanting               = undefined_int
    noahmp%biochem%param%DateHarvest                = undefined_int
    noahmp%biochem%param%QuantumEfficiency25C       = undefined_real
    noahmp%biochem%param%CarboxylRateMax25C         = undefined_real
    noahmp%biochem%param%CarboxylRateMaxQ10         = undefined_real
    noahmp%biochem%param%PhotosynPathC3             = undefined_real
    noahmp%biochem%param%SlopeConductToPhotosyn     = undefined_real
    noahmp%biochem%param%TemperatureMinPhotosyn     = undefined_real
    noahmp%biochem%param%LeafAreaPerMass1side       = undefined_real
    noahmp%biochem%param%NitrogenConcFoliageMax     = undefined_real
    noahmp%biochem%param%WoodToRootRatio            = undefined_real
    noahmp%biochem%param%WoodPoolIndex              = undefined_real
    noahmp%biochem%param%TurnoverCoeffLeafVeg       = undefined_real
    noahmp%biochem%param%LeafDeathWaterCoeffVeg     = undefined_real
    noahmp%biochem%param%LeafDeathTempCoeffVeg      = undefined_real
    noahmp%biochem%param%MicroRespCoeff             = undefined_real
    noahmp%biochem%param%RespMaintQ10               = undefined_real
    noahmp%biochem%param%RespMaintLeaf25C           = undefined_real
    noahmp%biochem%param%RespMaintStem25C           = undefined_real
    noahmp%biochem%param%RespMaintRoot25C           = undefined_real
    noahmp%biochem%param%RespMaintGrain25C          = undefined_real
    noahmp%biochem%param%GrowthRespFrac             = undefined_real
    noahmp%biochem%param%TemperaureLeafFreeze       = undefined_real
    noahmp%biochem%param%LeafAreaPerBiomass         = undefined_real
    noahmp%biochem%param%TempBaseGrowDegDay         = undefined_real
    noahmp%biochem%param%TempMaxGrowDegDay          = undefined_real
    noahmp%biochem%param%GrowDegDayEmerg            = undefined_real
    noahmp%biochem%param%GrowDegDayInitVeg          = undefined_real
    noahmp%biochem%param%GrowDegDayPostVeg          = undefined_real
    noahmp%biochem%param%GrowDegDayInitReprod       = undefined_real
    noahmp%biochem%param%GrowDegDayMature           = undefined_real
    noahmp%biochem%param%PhotosynRadFrac            = undefined_real
    noahmp%biochem%param%TempMinCarbonAssim         = undefined_real
    noahmp%biochem%param%TempMaxCarbonAssim         = undefined_real
    noahmp%biochem%param%TempMaxCarbonAssimMax      = undefined_real
    noahmp%biochem%param%CarbonAssimRefMax          = undefined_real
    noahmp%biochem%param%LightExtCoeff              = undefined_real
    noahmp%biochem%param%LightUseEfficiency         = undefined_real
    noahmp%biochem%param%CarbonAssimReducFac        = undefined_real
    noahmp%biochem%param%StemAreaIndexMin           = undefined_real
    noahmp%biochem%param%WoodAllocFac               = undefined_real
    noahmp%biochem%param%WaterStressCoeff           = undefined_real
    noahmp%biochem%param%LeafAreaIndexMin           = undefined_real
    noahmp%biochem%param%TurnoverCoeffRootVeg       = undefined_real
    noahmp%biochem%param%WoodRespCoeff              = undefined_real

    if ( .not. allocated(noahmp%biochem%param%LeafDeathTempCoeffCrop) )  &
       allocate( noahmp%biochem%param%LeafDeathTempCoeffCrop(1:NumCropGrowStage) )
    if ( .not. allocated(noahmp%biochem%param%LeafDeathWaterCoeffCrop) ) &
       allocate( noahmp%biochem%param%LeafDeathWaterCoeffCrop(1:NumCropGrowStage) )
    if ( .not. allocated(noahmp%biochem%param%CarbohydrLeafToGrain) )    &
       allocate( noahmp%biochem%param%CarbohydrLeafToGrain(1:NumCropGrowStage) )
    if ( .not. allocated(noahmp%biochem%param%CarbohydrStemToGrain) )    &
       allocate( noahmp%biochem%param%CarbohydrStemToGrain(1:NumCropGrowStage) )
    if ( .not. allocated(noahmp%biochem%param%CarbohydrRootToGrain) )    &
       allocate( noahmp%biochem%param%CarbohydrRootToGrain(1:NumCropGrowStage) )
    if ( .not. allocated(noahmp%biochem%param%CarbohydrFracToLeaf) )     &
       allocate( noahmp%biochem%param%CarbohydrFracToLeaf(1:NumCropGrowStage) )
    if ( .not. allocated(noahmp%biochem%param%CarbohydrFracToStem) )     &
       allocate( noahmp%biochem%param%CarbohydrFracToStem(1:NumCropGrowStage) )
    if ( .not. allocated(noahmp%biochem%param%CarbohydrFracToRoot) )     &
       allocate( noahmp%biochem%param%CarbohydrFracToRoot(1:NumCropGrowStage) )
    if ( .not. allocated(noahmp%biochem%param%CarbohydrFracToGrain) )    &
       allocate( noahmp%biochem%param%CarbohydrFracToGrain(1:NumCropGrowStage) )
    if ( .not. allocated(noahmp%biochem%param%TurnoverCoeffLeafCrop) )   &
       allocate( noahmp%biochem%param%TurnoverCoeffLeafCrop(1:NumCropGrowStage) )
    if ( .not. allocated(noahmp%biochem%param%TurnoverCoeffStemCrop) )   &
       allocate( noahmp%biochem%param%TurnoverCoeffStemCrop(1:NumCropGrowStage) )
    if ( .not. allocated(noahmp%biochem%param%TurnoverCoeffRootCrop) )   &
       allocate( noahmp%biochem%param%TurnoverCoeffRootCrop(1:NumCropGrowStage) )

    noahmp%biochem%param%LeafDeathTempCoeffCrop (:) = undefined_real
    noahmp%biochem%param%LeafDeathWaterCoeffCrop(:) = undefined_real
    noahmp%biochem%param%CarbohydrLeafToGrain   (:) = undefined_real
    noahmp%biochem%param%CarbohydrStemToGrain   (:) = undefined_real
    noahmp%biochem%param%CarbohydrRootToGrain   (:) = undefined_real
    noahmp%biochem%param%CarbohydrFracToLeaf    (:) = undefined_real
    noahmp%biochem%param%CarbohydrFracToStem    (:) = undefined_real
    noahmp%biochem%param%CarbohydrFracToRoot    (:) = undefined_real
    noahmp%biochem%param%CarbohydrFracToGrain   (:) = undefined_real
    noahmp%biochem%param%TurnoverCoeffLeafCrop  (:) = undefined_real
    noahmp%biochem%param%TurnoverCoeffStemCrop  (:) = undefined_real
    noahmp%biochem%param%TurnoverCoeffRootCrop  (:) = undefined_real

    end associate

  end subroutine BiochemVarInitDefault

end module BiochemVarInitMod
