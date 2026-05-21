module CarbonFluxCropMod

!!! Main Carbon assimilation for crops
!!! based on RE Dickinson et al.(1998), modifed by Guo-Yue Niu, 2004
!!! Modified by Xing Liu, 2014
        
  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none
        
contains

  subroutine CarbonFluxCrop(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: CO2FLUX_CROP
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath & refactor team (He et al. 2023)
! -------------------------------------------------------------------------
 
    implicit none
        
    type(noahmp_type), intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp)           :: DeathCoeffTemp       ! temperature stress death coefficient
    real(kind=kind_noahmp)           :: DeathCoeffWater      ! water stress death coefficient
    real(kind=kind_noahmp)           :: NetPriProdLeafAdd    ! leaf assimil after resp. losses removed [gCH2O/m2/s] 
    real(kind=kind_noahmp)           :: NetPriProdStemAdd    ! stem assimil after resp. losses removed [gCH2O/m2/s]
   !real(kind=kind_noahmp)           :: RespTmp, Temp0       ! temperary vars for function below
   !RespTmp(Temp0) = exp(0.08 * (Temp0 - 298.16))            ! Respiration as a function of temperature

!------------------------------------------------------------------------
    associate(                                                                           &
              MainTimeStep             => noahmp%config%domain%MainTimeStep             ,& ! in,    main noahmp timestep [s]
              WaterStressCoeff         => noahmp%biochem%param%WaterStressCoeff         ,& ! in,    water stress coeficient
              LeafAreaIndexMin         => noahmp%biochem%param%LeafAreaIndexMin         ,& ! in,    minimum leaf area index [m2/m2]
              StemAreaIndexMin         => noahmp%biochem%param%StemAreaIndexMin         ,& ! in,    minimum stem area index [m2/m2]
              NitrogenConcFoliageMax   => noahmp%biochem%param%NitrogenConcFoliageMax   ,& ! in,    foliage nitrogen concentration when f(n)=1 [%]
              RespMaintQ10             => noahmp%biochem%param%RespMaintQ10             ,& ! in,    change in maintenance respiration for each 10C temp. change
              RespMaintLeaf25C         => noahmp%biochem%param%RespMaintLeaf25C         ,& ! in,    leaf maintenance respiration at 25C [umol CO2/m2/s]
              RespMaintRoot25C         => noahmp%biochem%param%RespMaintRoot25C         ,& ! in,    root maintenance respiration at 25C [umol CO2/kgCH2O/s]
              RespMaintStem25C         => noahmp%biochem%param%RespMaintStem25C         ,& ! in,    stem maintenance respiration at 25C [umol CO2/kgCH2O/s]
              RespMaintGrain25C        => noahmp%biochem%param%RespMaintGrain25C        ,& ! in,    grain maintenance respiration at 25C [umol CO2/kgCH2O/s]
              GrowthRespFrac           => noahmp%biochem%param%GrowthRespFrac           ,& ! in,    fraction of growth respiration
              CarbohydrFracToLeaf      => noahmp%biochem%param%CarbohydrFracToLeaf      ,& ! in,    fraction of carbohydrate flux to leaf
              CarbohydrFracToStem      => noahmp%biochem%param%CarbohydrFracToStem      ,& ! in,    fraction of carbohydrate flux to stem
              CarbohydrFracToRoot      => noahmp%biochem%param%CarbohydrFracToRoot      ,& ! in,    fraction of carbohydrate flux to root
              CarbohydrFracToGrain     => noahmp%biochem%param%CarbohydrFracToGrain     ,& ! in,    fraction of carbohydrate flux to grain
              TurnoverCoeffLeafCrop    => noahmp%biochem%param%TurnoverCoeffLeafCrop    ,& ! in,    leaf turnover coefficient [1/s] for crop
              TurnoverCoeffRootCrop    => noahmp%biochem%param%TurnoverCoeffRootCrop    ,& ! in,    root tunrover coefficient [1/s] for crop
              TurnoverCoeffStemCrop    => noahmp%biochem%param%TurnoverCoeffStemCrop    ,& ! in,    stem turnover coefficient [1/s] for crop
              TemperaureLeafFreeze     => noahmp%biochem%param%TemperaureLeafFreeze     ,& ! in,    characteristic temperature for leaf freezing [K]
              LeafDeathWaterCoeffCrop  => noahmp%biochem%param%LeafDeathWaterCoeffCrop  ,& ! in,    coeficient for water leaf stress death [1/s] for crop
              LeafDeathTempCoeffCrop   => noahmp%biochem%param%LeafDeathTempCoeffCrop   ,& ! in,    coeficient for temperature leaf stress death [1/s] for crop
              CarbohydrLeafToGrain     => noahmp%biochem%param%CarbohydrLeafToGrain     ,& ! in,    fraction of carbohydrate translocation from leaf to grain
              CarbohydrStemToGrain     => noahmp%biochem%param%CarbohydrStemToGrain     ,& ! in,    fraction of carbohydrate translocation from stem to grain
              CarbohydrRootToGrain     => noahmp%biochem%param%CarbohydrRootToGrain     ,& ! in,    fraction of carbohydrate translocation from root to grain
              MicroRespCoeff           => noahmp%biochem%param%MicroRespCoeff           ,& ! in,    microbial respiration parameter [umol CO2/kgC/s]
              LeafAreaPerBiomass       => noahmp%biochem%param%LeafAreaPerBiomass       ,& ! in,    leaf area per living leaf biomass [m2/g]
              SoilWaterRootZone        => noahmp%water%state%SoilWaterRootZone          ,& ! in,    root zone soil water
              SoilWaterStress          => noahmp%water%state%SoilWaterStress            ,& ! in,    water stress coeficient (1.0 for wilting)
              PhotosynTotal            => noahmp%biochem%flux%PhotosynTotal             ,& ! in,    total leaf photosynthesis [umol CO2/m2/s]
              NitrogenConcFoliage      => noahmp%biochem%state%NitrogenConcFoliage      ,& ! in,    foliage nitrogen concentration [%]
              IndexPlanting            => noahmp%biochem%state%IndexPlanting            ,& ! in,    Planting index
              PlantGrowStage           => noahmp%biochem%state%PlantGrowStage           ,& ! in,    plant growing stage
              TemperatureSoilSnow      => noahmp%energy%state%TemperatureSoilSnow       ,& ! in,    snow and soil layer temperature [K]
              TemperatureCanopy        => noahmp%energy%state%TemperatureCanopy         ,& ! in,    vegetation temperature [K]
              LeafAreaIndex            => noahmp%energy%state%LeafAreaIndex             ,& ! inout, leaf area index
              StemAreaIndex            => noahmp%energy%state%StemAreaIndex             ,& ! inout, stem area index
              LeafMass                 => noahmp%biochem%state%LeafMass                 ,& ! inout, leaf mass [gCH2O/m2]
              RootMass                 => noahmp%biochem%state%RootMass                 ,& ! inout, mass of fine roots [gCH2O/m2]
              StemMass                 => noahmp%biochem%state%StemMass                 ,& ! inout, stem mass [gCH2O/m2]
              CarbonMassDeepSoil       => noahmp%biochem%state%CarbonMassDeepSoil       ,& ! inout, stable carbon in deep soil [gC/m2]
              CarbonMassShallowSoil    => noahmp%biochem%state%CarbonMassShallowSoil    ,& ! inout, short-lived carbon in shallow soil [gC/m2]
              GrainMass                => noahmp%biochem%state%GrainMass                ,& ! inout, mass of grain [gCH2O/m2]
              RespFacNitrogenFoliage   => noahmp%biochem%state%RespFacNitrogenFoliage   ,& ! out,   foliage nitrogen adjustemt to respiration (<= 1)
              MicroRespFactorSoilWater => noahmp%biochem%state%MicroRespFactorSoilWater ,& ! out,   soil water factor for microbial respiration
              MicroRespFactorSoilTemp  => noahmp%biochem%state%MicroRespFactorSoilTemp  ,& ! out,   soil temperature factor for microbial respiration
              LeafMassMin              => noahmp%biochem%state%LeafMassMin              ,& ! out,   minimum leaf mass [gCH2O/m2]
              StemMassMin              => noahmp%biochem%state%StemMassMin              ,& ! out,   minimum stem mass [gCH2O/m2]
              StemAreaPerMass          => noahmp%biochem%state%StemAreaPerMass          ,& ! out,   stem area per unit mass [m2/g]
              RespFacTemperature       => noahmp%biochem%state%RespFacTemperature       ,& ! out,   temperature factor
              CarbonMassSoilTot        => noahmp%biochem%state%CarbonMassSoilTot        ,& ! out,   total soil carbon [gC/m2]
              CarbonMassLiveTot        => noahmp%biochem%state%CarbonMassLiveTot        ,& ! out,   total living carbon [gC/m2]
              CarbonAssim              => noahmp%biochem%flux%CarbonAssim               ,& ! out,   carbon assimilated rate [gC/m2/s]
              CarbohydrAssim           => noahmp%biochem%flux%CarbohydrAssim            ,& ! out,   carbohydrate assimilated rate [gCH2O/m2/s]
              TurnoverLeaf             => noahmp%biochem%flux%TurnoverLeaf              ,& ! out,   leaf turnover rate [gCH2O/m2/s]
              TurnoverStem             => noahmp%biochem%flux%TurnoverStem              ,& ! out,   stem turnover rate [gCH2O/m2/s]
              TurnoverRoot             => noahmp%biochem%flux%TurnoverRoot              ,& ! out,   root carbon loss rate by turnover [gCH2O/m2/s]
              ConvLeafToGrain          => noahmp%biochem%flux%ConvLeafToGrain           ,& ! out,   leaf to grain conversion [gCH2O/m2]
              ConvRootToGrain          => noahmp%biochem%flux%ConvRootToGrain           ,& ! out,   root to grain conversion [gCH2O/m2]
              ConvStemToGrain          => noahmp%biochem%flux%ConvStemToGrain           ,& ! out,   stem to grain conversion [gCH2O/m2]
              RespirationPlantTot      => noahmp%biochem%flux%RespirationPlantTot       ,& ! out,   total plant respiration [gC/m2/s C]
              CarbonToAtmos            => noahmp%biochem%flux%CarbonToAtmos             ,& ! out,   carbon flux to atmosphere [gC/m2/s]
              GrossPriProduction       => noahmp%biochem%flux%GrossPriProduction        ,& ! out,   gross primary production [gC/m2/s]
              NetPriProductionTot      => noahmp%biochem%flux%NetPriProductionTot       ,& ! out,   total net primary productivity [gC/m2/s]
              NetPriProductionLeaf     => noahmp%biochem%flux%NetPriProductionLeaf      ,& ! out,   leaf net primary productivity [gCH2O/m2/s]
              NetPriProductionRoot     => noahmp%biochem%flux%NetPriProductionRoot      ,& ! out,   root net primary productivity [gCH2O/m2/s]
              NetPriProductionStem     => noahmp%biochem%flux%NetPriProductionStem      ,& ! out,   stem net primary productivity [gCH2O/m2/s]
              NetPriProductionGrain    => noahmp%biochem%flux%NetPriProductionGrain     ,& ! out,   grain net primary productivity [gCH2O/m2/s]
              NetEcoExchange           => noahmp%biochem%flux%NetEcoExchange            ,& ! out,   net ecosystem exchange [gCO2/m2/s]
              GrowthRespGrain          => noahmp%biochem%flux%GrowthRespGrain           ,& ! out,   growth respiration rate for grain [gCH2O/m2/s]
              GrowthRespLeaf           => noahmp%biochem%flux%GrowthRespLeaf            ,& ! out,   growth respiration rate for leaf [gCH2O/m2/s]
              GrowthRespRoot           => noahmp%biochem%flux%GrowthRespRoot            ,& ! out,   growth respiration rate for root [gCH2O/m2/s]
              GrowthRespStem           => noahmp%biochem%flux%GrowthRespStem            ,& ! out,   growth respiration rate for stem [gCH2O/m2/s]
              RespirationSoilOrg       => noahmp%biochem%flux%RespirationSoilOrg        ,& ! out,   soil organic respiration rate [gC/m2/s]
              LeafMassMaxChg           => noahmp%biochem%flux%LeafMassMaxChg            ,& ! out,   maximum leaf mass available to change [gCH2O/m2/s]
              StemMassMaxChg           => noahmp%biochem%flux%StemMassMaxChg            ,& ! out,   maximum steam  mass available to change [gCH2O/m2/s]
              RespirationLeaf          => noahmp%biochem%flux%RespirationLeaf           ,& ! out,   leaf respiration rate [umol CO2/m2/s]
              RespirationStem          => noahmp%biochem%flux%RespirationStem           ,& ! out,   stem respiration rate [gCH2O/m2/s]
              RespirationLeafMaint     => noahmp%biochem%flux%RespirationLeafMaint      ,& ! out,   leaf maintenance respiration rate [gCH2O/m2/s]
              RespirationRoot          => noahmp%biochem%flux%RespirationRoot           ,& ! out,   fine root respiration rate [gCH2O/m2/s]
              RespirationSoil          => noahmp%biochem%flux%RespirationSoil           ,& ! out,   soil respiration rate [gCH2O/m2/s]
              RespirationGrain         => noahmp%biochem%flux%RespirationGrain          ,& ! out,   grain respiration rate [gCH2O/m2/s]
              DeathLeaf                => noahmp%biochem%flux%DeathLeaf                 ,& ! out,   death rate of leaf mass [gCH2O/m2/s]
              CarbonDecayToStable      => noahmp%biochem%flux%CarbonDecayToStable        & ! out,   decay rate of fast carbon to slow carbon [gCH2O/m2/s]
             )
!----------------------------------------------------------------------

    ! initialization
    StemAreaPerMass = 3.0 * 0.001         ! m2/kg -->m2/g
    LeafMassMin     = LeafAreaIndexMin / 0.035
    StemMassMin     = StemAreaIndexMin / StemAreaPerMass

    !!! carbon assimilation starts
    ! 1 mole -> 12 g carbon or 44 g CO2 or 30 g CH20
    CarbonAssim     = PhotosynTotal * 12.0e-6   !*IndexPlanting   !umol co2 /m2/ s -> g/m2/s C
    CarbohydrAssim  = PhotosynTotal * 30.0e-6   !*IndexPlanting   !umol co2 /m2/ s -> g/m2/s CH2O

    ! mainteinance respiration
    RespFacNitrogenFoliage = min(NitrogenConcFoliage / max(1.0e-06, NitrogenConcFoliageMax), 1.0)
    RespFacTemperature     = RespMaintQ10**((TemperatureCanopy - 298.16) / 10.0)
    RespirationLeaf        = RespMaintLeaf25C * RespFacTemperature * RespFacNitrogenFoliage * &
                             LeafAreaIndex * (1.0 - SoilWaterStress)                                       ! umolCO2/m2/s
    RespirationLeafMaint   = min((LeafMass - LeafMassMin) / MainTimeStep, RespirationLeaf*30.0e-6)         ! gCH2O/m2/s
    RespirationRoot        = RespMaintRoot25C * (RootMass * 1.0e-3) * RespFacTemperature * 30.0e-6         ! gCH2O/m2/s
    RespirationStem        = RespMaintStem25C * (StemMass * 1.0e-3) * RespFacTemperature * 30.0e-6         ! gCH2O/m2/s
    RespirationGrain       = RespMaintGrain25C * (GrainMass * 1.0e-3) * RespFacTemperature * 30.0e-6       ! gCH2O/m2/s

    ! calculate growth respiration for leaf, root and grain
    GrowthRespLeaf  = max(0.0, GrowthRespFrac * (CarbohydrFracToLeaf(PlantGrowStage)*CarbohydrAssim - RespirationLeafMaint))  ! gCH2O/m2/s
    GrowthRespStem  = max(0.0, GrowthRespFrac * (CarbohydrFracToStem(PlantGrowStage)*CarbohydrAssim - RespirationStem))       ! gCH2O/m2/s
    GrowthRespRoot  = max(0.0, GrowthRespFrac * (CarbohydrFracToRoot(PlantGrowStage)*CarbohydrAssim - RespirationRoot))       ! gCH2O/m2/s
    GrowthRespGrain = max(0.0, GrowthRespFrac * (CarbohydrFracToGrain(PlantGrowStage)*CarbohydrAssim - RespirationGrain))     ! gCH2O/m2/s

    ! leaf turnover, stem turnover, root turnover and leaf death caused by soil water and soil temperature stress
    TurnoverLeaf    = TurnoverCoeffLeafCrop(PlantGrowStage) * 1.0e-6 * LeafMass     ! gCH2O/m2/s
    TurnoverRoot    = TurnoverCoeffRootCrop(PlantGrowStage) * 1.0e-6 * RootMass     ! gCH2O/m2/s
    TurnoverStem    = TurnoverCoeffStemCrop(PlantGrowStage) * 1.0e-6 * StemMass     ! gCH2O/m2/s
    DeathCoeffTemp  = exp(-0.3 * max(0.0, TemperatureCanopy-TemperaureLeafFreeze)) * (LeafMass/120.0)
    DeathCoeffWater = exp((SoilWaterStress - 1.0) * WaterStressCoeff)
    DeathLeaf       = LeafMass * 1.0e-6 * (LeafDeathWaterCoeffCrop(PlantGrowStage) * DeathCoeffWater + &
                                           LeafDeathTempCoeffCrop(PlantGrowStage) * DeathCoeffTemp)      ! gCH2O/m2/s

    ! Allocation of CarbohydrAssim to leaf, stem, root and grain at each growth stage
    !NetPriProdLeafAdd = max(0.0, CarbohydrFracToLeaf(PlantGrowStage)*CarbohydrAssim - GrowthRespLeaf - RespirationLeafMaint) ! gCH2O/m2/s
    NetPriProdLeafAdd = CarbohydrFracToLeaf(PlantGrowStage)*CarbohydrAssim - GrowthRespLeaf - RespirationLeafMaint            ! gCH2O/m2/s
    !NetPriProdStemAdd = max(0.0, CarbohydrFracToStem(PlantGrowStage)*CarbohydrAssim - GrowthRespStem - RespirationStem)      ! gCH2O/m2/s
    NetPriProdStemAdd = CarbohydrFracToStem(PlantGrowStage)*CarbohydrAssim - GrowthRespStem - RespirationStem                 ! gCH2O/m2/s
    
    ! avoid reducing leaf mass below its minimum value but conserve mass
    LeafMassMaxChg = (LeafMass - LeafMassMin) / MainTimeStep                         ! gCH2O/m2/s
    StemMassMaxChg = (StemMass - StemMassMin) / MainTimeStep                         ! gCH2O/m2/s
    TurnoverLeaf   = min(TurnoverLeaf, LeafMassMaxChg+NetPriProdLeafAdd)             ! gCH2O/m2/s
    TurnoverStem   = min(TurnoverStem, StemMassMaxChg+NetPriProdStemAdd)             ! gCH2O/m2/s
    DeathLeaf      = min(DeathLeaf, LeafMassMaxChg+NetPriProdLeafAdd-TurnoverLeaf)   ! gCH2O/m2/s

    ! net primary productivities
    !NetPriProductionLeaf  = max(NetPriProdLeafAdd, -LeafMassMaxChg)    ! gCH2O/m2/s
    NetPriProductionLeaf  = NetPriProdLeafAdd                           ! gCH2O/m2/s
    !NetPriProductionStem  = max(NetPriProdStemAdd, -StemMassMaxChg)    ! gCH2O/m2/s
    NetPriProductionStem  = NetPriProdStemAdd                           ! gCH2O/m2/s
    NetPriProductionRoot  = CarbohydrFracToRoot(PlantGrowStage) * CarbohydrAssim - RespirationRoot - GrowthRespRoot     ! gCH2O/m2/s
    NetPriProductionGrain = CarbohydrFracToGrain(PlantGrowStage) * CarbohydrAssim - RespirationGrain - GrowthRespGrain  ! gCH2O/m2/s

    ! masses of plant components
    LeafMass           = LeafMass + (NetPriProductionLeaf - TurnoverLeaf - DeathLeaf) * MainTimeStep ! gCH2O/m2
    StemMass           = StemMass + (NetPriProductionStem - TurnoverStem) * MainTimeStep             ! gCH2O/m2
    RootMass           = RootMass + (NetPriProductionRoot - TurnoverRoot) * MainTimeStep             ! gCH2O/m2
    GrainMass          = GrainMass + NetPriProductionGrain * MainTimeStep                            ! gCH2O/m2
    GrossPriProduction = CarbohydrAssim * 0.4    ! gC/m2/s  0.4=12/30, CH20 to C

    ! carbon convert to grain ! Zhe Zhang 2020-07-13
    ConvLeafToGrain = 0.0
    ConvStemToGrain = 0.0
    ConvRootToGrain = 0.0
    ConvLeafToGrain = LeafMass * (CarbohydrLeafToGrain(PlantGrowStage) * MainTimeStep / 3600.0)      ! gCH2O/m2
    ConvStemToGrain = StemMass * (CarbohydrStemToGrain(PlantGrowStage) * MainTimeStep / 3600.0)      ! gCH2O/m2
    ConvRootToGrain = RootMass * (CarbohydrRootToGrain(PlantGrowStage) * MainTimeStep / 3600.0)      ! gCH2O/m2
    LeafMass        = LeafMass - ConvLeafToGrain                                                     ! gCH2O/m2
    StemMass        = StemMass - ConvStemToGrain                                                     ! gCH2O/m2
    RootMass        = RootMass - ConvRootToGrain                                                     ! gCH2O/m2
    GrainMass       = GrainMass + ConvStemToGrain + ConvRootToGrain + ConvLeafToGrain                ! gCH2O/m2
    !if ( PlantGrowStage==6 ) then
    !   ConvStemToGrain = StemMass * (0.00005 * MainTimeStep / 3600.0)    ! gCH2O/m2
    !   StemMass        = StemMass - ConvStemToGrain                      ! gCH2O/m2
    !   ConvRootToGrain = RootMass * (0.0005 * MainTimeStep / 3600.0)     ! gCH2O/m2
    !   RootMass        = RootMass - ConvRootToGrain                      ! gCH2O/m2
    !   GrainMass       = GrainMass + ConvStemToGrain + ConvRootToGrain   ! gCH2O/m2
    !endif
    
    if ( RootMass < 0.0 ) then
       TurnoverRoot = NetPriProductionRoot
       RootMass     = 0.0
    endif
    if ( GrainMass < 0.0 ) then
       GrainMass    = 0.0
    endif

    ! soil carbon budgets
    !if ( (PlantGrowStage == 1) .or. (PlantGrowStage == 2) .or. (PlantGrowStage == 8) ) then
    !   CarbonMassShallowSoil = 1000
    !else
    CarbonMassShallowSoil = CarbonMassShallowSoil + &
                            (TurnoverRoot+TurnoverLeaf+TurnoverStem+DeathLeaf) * MainTimeStep * 0.4  ! 0.4: gCH2O/m2 -> gC/m2 
    !endif
    MicroRespFactorSoilTemp  = 2.0**((TemperatureSoilSnow(1) - 283.16) / 10.0)
    MicroRespFactorSoilWater = SoilWaterRootZone / (0.20 + SoilWaterRootZone) * 0.23 / (0.23 + SoilWaterRootZone)
    RespirationSoil          = MicroRespFactorSoilWater * MicroRespFactorSoilTemp * &
                               MicroRespCoeff * max(0.0, CarbonMassShallowSoil*1.0e-3) * 30.0e-6     ! gCH2O/m2/s
    CarbonDecayToStable      = 0.1 * RespirationSoil                                                 ! gCH2O/m2/s
    CarbonMassShallowSoil    = CarbonMassShallowSoil - (RespirationSoil + CarbonDecayToStable) * MainTimeStep * 0.4    ! 0.4: gCH2O/m2 -> gC/m2
    CarbonMassDeepSoil       = CarbonMassDeepSoil + CarbonDecayToStable * MainTimeStep * 0.4                           ! 0.4: gCH2O/m2 -> gC/m2
 
    !  total carbon flux
    CarbonToAtmos  = - CarbonAssim + (RespirationLeafMaint + RespirationRoot + RespirationStem + RespirationGrain + &
                       0.9*RespirationSoil + GrowthRespLeaf + GrowthRespRoot + GrowthRespStem + GrowthRespGrain) * 0.4 ! gC/m2/s 0.4=12/30, CH20 to C

    ! for outputs
    NetPriProductionTot = (NetPriProductionLeaf + NetPriProductionStem + &
                           NetPriProductionRoot + NetPriProductionGrain) * 0.4                              ! gC/m2/s  0.4=12/30, CH20 to C 
    RespirationPlantTot = (RespirationRoot + RespirationGrain + RespirationLeafMaint + RespirationStem + &
                           GrowthRespLeaf + GrowthRespRoot + GrowthRespGrain + GrowthRespStem) * 0.4        ! gC/m2/s  0.4=12/30, CH20 to C
    RespirationSoilOrg  = 0.9 * RespirationSoil * 0.4                                                       ! gC/m2/s  0.4=12/30, CH20 to C
    NetEcoExchange      = (RespirationPlantTot + RespirationSoilOrg - GrossPriProduction) * 44.0 / 12.0     ! gCO2/m2/s
    CarbonMassSoilTot   = CarbonMassShallowSoil + CarbonMassDeepSoil                                        ! gC/m2
    CarbonMassLiveTot   = (LeafMass + RootMass + StemMass + GrainMass) * 0.4                                ! gC/m2 0.4=12/30, CH20 to C
 
    ! leaf area index and stem area index
    LeafAreaIndex = max(LeafMass*LeafAreaPerBiomass, LeafAreaIndexMin)
    StemAreaIndex = max(StemMass*StemAreaPerMass, StemAreaIndexMin)
   
    ! After harversting
    !if ( PlantGrowStage == 8 ) then
    !   LeafMass  = 0.62
    !   StemMass  = 0.0
    !   GrainMass = 0.0
    !endif

    !if ( (PlantGrowStage == 1) .or. (PlantGrowStage == 2) .or. (PlantGrowStage == 8) ) then
    if ( (PlantGrowStage == 8) .and. &
         ((GrainMass > 0) .or. (LeafMass > 0) .or. (StemMass > 0) .or. (RootMass > 0)) ) then
       LeafAreaIndex = 0.05
       StemAreaIndex = 0.05
       LeafMass      = LeafMassMin
       StemMass      = StemMassMin
       RootMass      = 0.0
       GrainMass     = 0.0
    endif 
        
    end associate

  end subroutine CarbonFluxCrop

end module CarbonFluxCropMod
