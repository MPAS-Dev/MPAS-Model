module CarbonFluxNatureVegMod

!!! Main Carbon assimilation for natural/generic vegetation
!!! based on RE Dickinson et al.(1998), modifed by Guo-Yue Niu, 2004

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
        
  implicit none
        
contains

  subroutine CarbonFluxNatureVeg(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: CO2FLUX
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath & refactor team (He et al. 2023)
! -------------------------------------------------------------------------
        
    implicit none
        
    type(noahmp_type), intent(inout) :: noahmp

! local variables
    real(kind=kind_noahmp)           :: DeathCoeffTemp       ! temperature stress death coefficient
    real(kind=kind_noahmp)           :: DeathCoeffWater      ! water stress death coefficient
    real(kind=kind_noahmp)           :: NetPriProdLeafAdd    ! leaf assimil after resp. losses removed [gC/m2/s] 
    real(kind=kind_noahmp)           :: NetPriProdStemAdd    ! stem assimil after resp. losses removed [gC/m2/s]
    real(kind=kind_noahmp)           :: RespTmp, Temp0       ! temperary vars for function below
    RespTmp(Temp0) = exp(0.08 * (Temp0 - 298.16))            ! Respiration as a function of temperature

!------------------------------------------------------------------------
    associate(                                                                           &
              VegType                  => noahmp%config%domain%VegType                  ,& ! in,    vegetation type
              MainTimeStep             => noahmp%config%domain%MainTimeStep             ,& ! in,    main noahmp timestep [s]
              IndexEBLForest           => noahmp%config%domain%IndexEBLForest           ,& ! in,    flag for Evergreen Broadleaf Forest
              WoodToRootRatio          => noahmp%biochem%param%WoodToRootRatio          ,& ! in,    wood to root ratio
              TurnoverCoeffLeafVeg     => noahmp%biochem%param%TurnoverCoeffLeafVeg     ,& ! in,    leaf turnover coefficient [1/s] for generic vegetation
              TemperaureLeafFreeze     => noahmp%biochem%param%TemperaureLeafFreeze     ,& ! in,    characteristic temperature for leaf freezing [K]
              LeafDeathWaterCoeffVeg   => noahmp%biochem%param%LeafDeathWaterCoeffVeg   ,& ! in,    coeficient for leaf water stress death [1/s] for generic veg
              LeafDeathTempCoeffVeg    => noahmp%biochem%param%LeafDeathTempCoeffVeg    ,& ! in,    coeficient for leaf temp. stress death [1/s] for generic veg
              GrowthRespFrac           => noahmp%biochem%param%GrowthRespFrac           ,& ! in,    fraction of growth respiration
              TemperatureMinPhotosyn   => noahmp%biochem%param%TemperatureMinPhotosyn   ,& ! in,    minimum temperature for photosynthesis [K]
              MicroRespCoeff           => noahmp%biochem%param%MicroRespCoeff           ,& ! in,    microbial respiration parameter [umol CO2/kgC/s]
              NitrogenConcFoliageMax   => noahmp%biochem%param%NitrogenConcFoliageMax   ,& ! in,    foliage nitrogen concentration when f(n)=1 (%)
              RespMaintQ10             => noahmp%biochem%param%RespMaintQ10             ,& ! in,    q10 for maintenance respiration
              RespMaintLeaf25C         => noahmp%biochem%param%RespMaintLeaf25C         ,& ! in,    leaf maintenance respiration at 25c [umol CO2/m2/s]
              RespMaintRoot25C         => noahmp%biochem%param%RespMaintRoot25C         ,& ! in,    root maintenance respiration at 25c [umol CO2/kgC/s]
              RespMaintStem25C         => noahmp%biochem%param%RespMaintStem25C         ,& ! in,    stem maintenance respiration at 25c [umol CO2/kgC/s]
              WoodPoolIndex            => noahmp%biochem%param%WoodPoolIndex            ,& ! in,    wood pool index (0~1) depending on woody or not
              TurnoverCoeffRootVeg     => noahmp%biochem%param%TurnoverCoeffRootVeg     ,& ! in,    root turnover coefficient [1/s] for generic vegetation
              WoodRespCoeff            => noahmp%biochem%param%WoodRespCoeff            ,& ! in,    wood respiration coeficient [1/s]
              WoodAllocFac             => noahmp%biochem%param%WoodAllocFac             ,& ! in,    parameter for present wood allocation
              WaterStressCoeff         => noahmp%biochem%param%WaterStressCoeff         ,& ! in,    water stress coeficient
              LeafAreaIndexMin         => noahmp%biochem%param%LeafAreaIndexMin         ,& ! in,    minimum leaf area index [m2/m2]
              StemAreaIndexMin         => noahmp%biochem%param%StemAreaIndexMin         ,& ! in,    minimum stem area index [m2/m2]
              IndexGrowSeason          => noahmp%biochem%state%IndexGrowSeason          ,& ! in,    growing season index (0=off, 1=on)
              NitrogenConcFoliage      => noahmp%biochem%state%NitrogenConcFoliage      ,& ! in,    foliage nitrogen concentration [%]
              LeafAreaPerMass          => noahmp%biochem%state%LeafAreaPerMass          ,& ! in,    leaf area per unit mass [m2/g]
              PhotosynTotal            => noahmp%biochem%flux%PhotosynTotal             ,& ! in,    total leaf photosynthesis [umolCO2/m2/s]
              SoilWaterRootZone        => noahmp%water%state%SoilWaterRootZone          ,& ! in,    root zone soil water
              SoilWaterStress          => noahmp%water%state%SoilWaterStress            ,& ! in,    water stress coeficient (1.0 for wilting)
              TemperatureSoilSnow      => noahmp%energy%state%TemperatureSoilSnow       ,& ! in,    snow and soil layer temperature [K]
              TemperatureCanopy        => noahmp%energy%state%TemperatureCanopy         ,& ! in,    vegetation temperature [K]
              LeafAreaIndex            => noahmp%energy%state%LeafAreaIndex             ,& ! inout, leaf area index
              StemAreaIndex            => noahmp%energy%state%StemAreaIndex             ,& ! inout, stem area index
              LeafMass                 => noahmp%biochem%state%LeafMass                 ,& ! inout, leaf mass [gC/m2]
              RootMass                 => noahmp%biochem%state%RootMass                 ,& ! inout, mass of fine roots [gC/m2]
              StemMass                 => noahmp%biochem%state%StemMass                 ,& ! inout, stem mass [gC/m2]
              WoodMass                 => noahmp%biochem%state%WoodMass                 ,& ! inout, mass of wood (incl. woody roots) [gC/m2]
              CarbonMassDeepSoil       => noahmp%biochem%state%CarbonMassDeepSoil       ,& ! inout, stable carbon in deep soil [gC/m2]
              CarbonMassShallowSoil    => noahmp%biochem%state%CarbonMassShallowSoil    ,& ! inout, short-lived carbon in shallow soil [gC/m2]
              CarbonMassSoilTot        => noahmp%biochem%state%CarbonMassSoilTot        ,& ! out,   total soil carbon [gC/m2]
              CarbonMassLiveTot        => noahmp%biochem%state%CarbonMassLiveTot        ,& ! out,   total living carbon ([gC/m2]
              LeafMassMin              => noahmp%biochem%state%LeafMassMin              ,& ! out,   minimum leaf mass [gC/m2]
              CarbonFracToLeaf         => noahmp%biochem%state%CarbonFracToLeaf         ,& ! out,   fraction of carbon allocated to leaves
              WoodCarbonFrac           => noahmp%biochem%state%WoodCarbonFrac           ,& ! out,   calculated wood to root ratio
              CarbonFracToWoodRoot     => noahmp%biochem%state%CarbonFracToWoodRoot     ,& ! out,   fraction of carbon to root and wood
              CarbonFracToRoot         => noahmp%biochem%state%CarbonFracToRoot         ,& ! out,   fraction of carbon flux to roots
              CarbonFracToWood         => noahmp%biochem%state%CarbonFracToWood         ,& ! out,   fraction of carbon flux to wood
              CarbonFracToStem         => noahmp%biochem%state%CarbonFracToStem         ,& ! out,   fraction of carbon flux to stem
              MicroRespFactorSoilWater => noahmp%biochem%state%MicroRespFactorSoilWater ,& ! out,   soil water factor for microbial respiration
              MicroRespFactorSoilTemp  => noahmp%biochem%state%MicroRespFactorSoilTemp  ,& ! out,   soil temperature factor for microbial respiration
              RespFacNitrogenFoliage   => noahmp%biochem%state%RespFacNitrogenFoliage   ,& ! out,   foliage nitrogen adjustemt to respiration (<= 1)
              RespFacTemperature       => noahmp%biochem%state%RespFacTemperature       ,& ! out,   temperature factor
              RespReductionFac         => noahmp%biochem%state%RespReductionFac         ,& ! out,   respiration reduction factor (<= 1)
              StemMassMin              => noahmp%biochem%state%StemMassMin              ,& ! out,   minimum stem mass [gC/m2]
              StemAreaPerMass          => noahmp%biochem%state%StemAreaPerMass          ,& ! out,   stem area per unit mass [m2/g]
              CarbonAssim              => noahmp%biochem%flux%CarbonAssim               ,& ! out,   carbon assimilated rate [gC/m2/s]
              GrossPriProduction       => noahmp%biochem%flux%GrossPriProduction        ,& ! out,   gross primary production [gC/m2/s]
              NetPriProductionTot      => noahmp%biochem%flux%NetPriProductionTot       ,& ! out,   total net primary productivity [gC/m2/s]
              NetEcoExchange           => noahmp%biochem%flux%NetEcoExchange            ,& ! out,   net ecosystem exchange [gCO2/m2/s]
              RespirationPlantTot      => noahmp%biochem%flux%RespirationPlantTot       ,& ! out,   total plant respiration [gC/m2/s]
              RespirationSoilOrg       => noahmp%biochem%flux%RespirationSoilOrg        ,& ! out,   soil organic respiration [gC/m2/s]
              CarbonToAtmos            => noahmp%biochem%flux%CarbonToAtmos             ,& ! out,   carbon flux to atmosphere [gC/m2/s]
              NetPriProductionLeaf     => noahmp%biochem%flux%NetPriProductionLeaf      ,& ! out,   leaf net primary productivity [gC/m2/s]
              NetPriProductionRoot     => noahmp%biochem%flux%NetPriProductionRoot      ,& ! out,   root net primary productivity [gC/m2/s]
              NetPriProductionWood     => noahmp%biochem%flux%NetPriProductionWood      ,& ! out,   wood net primary productivity [gC/m2/s]
              NetPriProductionStem     => noahmp%biochem%flux%NetPriProductionStem      ,& ! out,   stem net primary productivity [gC/m2/s]
              GrowthRespLeaf           => noahmp%biochem%flux%GrowthRespLeaf            ,& ! out,   growth respiration rate for leaf [gC/m2/s]
              GrowthRespRoot           => noahmp%biochem%flux%GrowthRespRoot            ,& ! out,   growth respiration rate for root [gC/m2/s]
              GrowthRespWood           => noahmp%biochem%flux%GrowthRespWood            ,& ! out,   growth respiration rate for wood [gC/m2/s]
              GrowthRespStem           => noahmp%biochem%flux%GrowthRespStem            ,& ! out,   growth respiration rate for stem [gC/m2/s]
              LeafMassMaxChg           => noahmp%biochem%flux%LeafMassMaxChg            ,& ! out,   maximum leaf mass available to change [gC/m2/s]
              CarbonDecayToStable      => noahmp%biochem%flux%CarbonDecayToStable       ,& ! out,   decay rate of fast carbon to slow carbon [gC/m2/s]
              RespirationLeaf          => noahmp%biochem%flux%RespirationLeaf           ,& ! out,   leaf respiration rate [umol CO2/m2/s]
              RespirationStem          => noahmp%biochem%flux%RespirationStem           ,& ! out,   stem respiration rate [gC/m2/s]
              RespirationWood          => noahmp%biochem%flux%RespirationWood           ,& ! out,   wood respiration rate [gC/m2/s]
              RespirationLeafMaint     => noahmp%biochem%flux%RespirationLeafMaint      ,& ! out,   leaf maintenance respiration rate [gC/m2/s]
              RespirationRoot          => noahmp%biochem%flux%RespirationRoot           ,& ! out,   fine root respiration rate [gC/m2/s]
              RespirationSoil          => noahmp%biochem%flux%RespirationSoil           ,& ! out,   soil respiration rate [gC/m2/s]
              DeathLeaf                => noahmp%biochem%flux%DeathLeaf                 ,& ! out,   death rate of leaf mass [gC/m2/s]
              DeathStem                => noahmp%biochem%flux%DeathStem                 ,& ! out,   death rate of stem mass [gC/m2/s]
              TurnoverLeaf             => noahmp%biochem%flux%TurnoverLeaf              ,& ! out,   leaf turnover rate [gC/m2/s]
              TurnoverStem             => noahmp%biochem%flux%TurnoverStem              ,& ! out,   stem turnover rate [gC/m2/s]
              TurnoverWood             => noahmp%biochem%flux%TurnoverWood              ,& ! out,   wood turnover rate [gC/m2/s]
              TurnoverRoot             => noahmp%biochem%flux%TurnoverRoot              ,& ! out,   root turnover rate [gC/m2/s]
              StemMassMaxChg           => noahmp%biochem%flux%StemMassMaxChg             & ! out,   maximum steam mass available to change [gC/m2/s]
             )
!-----------------------------------------------------------------------

    ! initialization
    StemAreaPerMass = 3.0 * 0.001      ! m2/kg -->m2/g
    LeafMassMin     = LeafAreaIndexMin / LeafAreaPerMass   ! gC/m2
    StemMassMin     = StemAreaIndexMin / StemAreaPerMass   ! gC/m2
        
    ! respiration
    if ( IndexGrowSeason == 0.0 ) then
       RespReductionFac = 0.5
    else
       RespReductionFac = 1.0
    endif             
    RespFacNitrogenFoliage = min(NitrogenConcFoliage / max(1.0e-06,NitrogenConcFoliageMax), 1.0)
    RespFacTemperature     = RespMaintQ10**((TemperatureCanopy - 298.16) / 10.0)
    RespirationLeaf        = RespMaintLeaf25C * RespFacTemperature * RespFacNitrogenFoliage * &
                             LeafAreaIndex * RespReductionFac * (1.0 - SoilWaterStress)                               ! umol CO2/m2/s
    RespirationLeafMaint   = min((LeafMass-LeafMassMin)/MainTimeStep, RespirationLeaf*12.0e-6)                        ! gC/m2/s
    RespirationRoot        = RespMaintRoot25C * (RootMass*1.0e-3) * RespFacTemperature * RespReductionFac * 12.0e-6   ! gC/m2/s
    RespirationStem        = RespMaintStem25C * ((StemMass-StemMassMin) * 1.0e-3) * &
                             RespFacTemperature * RespReductionFac * 12.0e-6                                          ! gC/m2/s
    RespirationWood        = WoodRespCoeff * RespTmp(TemperatureCanopy) * WoodMass * WoodPoolIndex                    ! gC/m2/s
    
    !!! carbon assimilation start
    ! 1 mole -> 12 g carbon or 44 g CO2; 1 umol -> 12.e-6 g carbon;   
    CarbonAssim = PhotosynTotal * 12.0e-6      ! umol CO2/m2/s -> gC/m2/s

    ! fraction of carbon into leaf versus nonleaf
    CarbonFracToLeaf     = exp(0.01 * (1.0 - exp(0.75*LeafAreaIndex)) * LeafAreaIndex)
    if ( VegType == IndexEBLForest ) CarbonFracToLeaf = exp(0.01 * (1.0 - exp(0.50*LeafAreaIndex)) * LeafAreaIndex)
    CarbonFracToWoodRoot = 1.0 - CarbonFracToLeaf
    CarbonFracToStem     = LeafAreaIndex / 10.0 * CarbonFracToLeaf
    CarbonFracToLeaf     = CarbonFracToLeaf - CarbonFracToStem
      
    !  fraction of carbon into wood versus root 
    if ( WoodMass > 1.0e-6 ) then
       WoodCarbonFrac = (1.0 - exp(-WoodAllocFac * (WoodToRootRatio*RootMass/WoodMass)) / WoodAllocFac) * WoodPoolIndex
    else
       WoodCarbonFrac = WoodPoolIndex
    endif   
    CarbonFracToRoot  = CarbonFracToWoodRoot * (1.0 - WoodCarbonFrac)
    CarbonFracToWood  = CarbonFracToWoodRoot * WoodCarbonFrac

    ! leaf and root turnover per time step  
    TurnoverLeaf = TurnoverCoeffLeafVeg * 5.0e-7 * LeafMass   ! gC/m2/s
    TurnoverStem = TurnoverCoeffLeafVeg * 5.0e-7 * StemMass   ! gC/m2/s
    TurnoverRoot = TurnoverCoeffRootVeg * RootMass            ! gC/m2/s
    TurnoverWood = 9.5e-10 * WoodMass                         ! gC/m2/s
       
    ! seasonal leaf die rate dependent on temp and water stress
    ! water stress is set to 1 at permanent wilting point      
    DeathCoeffTemp  = exp(-0.3 * max(0.0, TemperatureCanopy-TemperaureLeafFreeze)) * (LeafMass / 120.0) 
    DeathCoeffWater = exp((SoilWaterStress - 1.0) * WaterStressCoeff)
    DeathLeaf       = LeafMass * 1.0e-6 * (LeafDeathWaterCoeffVeg * DeathCoeffWater + LeafDeathTempCoeffVeg * DeathCoeffTemp)  ! gC/m2/s
    DeathStem       = StemMass * 1.0e-6 * (LeafDeathWaterCoeffVeg * DeathCoeffWater + LeafDeathTempCoeffVeg * DeathCoeffTemp)  ! gC/m2/s
     
    ! calculate growth respiration for leaf, root and wood 
    GrowthRespLeaf = max(0.0, GrowthRespFrac * (CarbonFracToLeaf*CarbonAssim - RespirationLeafMaint))  ! gC/m2/s
    GrowthRespStem = max(0.0, GrowthRespFrac * (CarbonFracToStem*CarbonAssim - RespirationStem))       ! gC/m2/s
    GrowthRespRoot = max(0.0, GrowthRespFrac * (CarbonFracToRoot*CarbonAssim - RespirationRoot))       ! gC/m2/s
    GrowthRespWood = max(0.0, GrowthRespFrac * (CarbonFracToWood*CarbonAssim - RespirationWood))       ! gC/m2/s
        
    ! Impose lower T limit for photosynthesis
    NetPriProdLeafAdd = max(0.0, CarbonFracToLeaf*CarbonAssim - GrowthRespLeaf - RespirationLeafMaint) ! gC/m2/s
    NetPriProdStemAdd = max(0.0, CarbonFracToStem*CarbonAssim - GrowthRespStem - RespirationStem)      ! gC/m2/s
   !NetPriProdLeafAdd = CarbonFracToLeaf*CarbonAssim - GrowthRespLeaf - RespirationLeafMaint  ! MB: test Kjetil 
   !NetPriProdStemAdd = CarbonFracToStem*CarbonAssim - GrowthRespStem - RespirationStem       ! MB: test Kjetil 
    if ( TemperatureCanopy < TemperatureMinPhotosyn ) NetPriProdLeafAdd = 0.0
    if ( TemperatureCanopy < TemperatureMinPhotosyn ) NetPriProdStemAdd = 0.0
     
    ! update leaf, root, and wood carbon
    ! avoid reducing leaf mass below its minimum value but conserve mass
    LeafMassMaxChg = (LeafMass - LeafMassMin) / MainTimeStep                        ! gC/m2/s
    StemMassMaxChg = (StemMass - StemMassMin) / MainTimeStep                        ! gC/m2/s
    DeathLeaf      = min(DeathLeaf, LeafMassMaxChg+NetPriProdLeafAdd-TurnoverLeaf)  ! gC/m2/s
    DeathStem      = min(DeathStem, StemMassMaxChg+NetPriProdStemAdd-TurnoverStem)  ! gC/m2/s
      
    ! net primary productivities
    NetPriProductionLeaf = max(NetPriProdLeafAdd, -LeafMassMaxChg)                             ! gC/m2/s
    NetPriProductionStem = max(NetPriProdStemAdd, -StemMassMaxChg)                             ! gC/m2/s
    NetPriProductionRoot = CarbonFracToRoot * CarbonAssim - RespirationRoot - GrowthRespRoot   ! gC/m2/s
    NetPriProductionWood = CarbonFracToWood * CarbonAssim - RespirationWood - GrowthRespWood   ! gC/m2/s
       
    ! masses of plant components
    LeafMass = LeafMass + (NetPriProductionLeaf - TurnoverLeaf - DeathLeaf) * MainTimeStep   ! gC/m2
    StemMass = StemMass + (NetPriProductionStem - TurnoverStem - DeathStem) * MainTimeStep   ! gC/m2
    RootMass = RootMass + (NetPriProductionRoot - TurnoverRoot) * MainTimeStep               ! gC/m2
    if ( RootMass < 0.0 ) then
       TurnoverRoot = NetPriProductionRoot
       RootMass     = 0.0
    endif 
    WoodMass = (WoodMass + (NetPriProductionWood - TurnoverWood) * MainTimeStep ) * WoodPoolIndex  ! gC/m2

    ! soil carbon budgets 
    CarbonMassShallowSoil    = CarbonMassShallowSoil + &
                               (TurnoverRoot+TurnoverLeaf+TurnoverStem+TurnoverWood+DeathLeaf+DeathStem) * MainTimeStep  ! gC/m2, MB: add DeathStem v3.7
    MicroRespFactorSoilTemp  = 2.0**( (TemperatureSoilSnow(1) - 283.16) / 10.0 ) 
    MicroRespFactorSoilWater = SoilWaterRootZone / (0.20 + SoilWaterRootZone) * 0.23 / (0.23 + SoilWaterRootZone)
    RespirationSoil          = MicroRespFactorSoilWater * MicroRespFactorSoilTemp * &
                               MicroRespCoeff * max(0.0, CarbonMassShallowSoil*1.0e-3) * 12.0e-6              ! gC/m2/s
    CarbonDecayToStable      = 0.1 * RespirationSoil                                                          ! gC/m2/s
    CarbonMassShallowSoil    = CarbonMassShallowSoil - (RespirationSoil + CarbonDecayToStable) * MainTimeStep ! gC/m2
    CarbonMassDeepSoil       = CarbonMassDeepSoil + CarbonDecayToStable * MainTimeStep                        ! gC/m2
     
    !  total carbon flux ! MB: add RespirationStem,GrowthRespStem,0.9*RespirationSoil v3.7    
    CarbonToAtmos       = - CarbonAssim + RespirationLeafMaint + RespirationRoot + RespirationWood + RespirationStem + &
                          0.9*RespirationSoil + GrowthRespLeaf + GrowthRespRoot + GrowthRespWood + GrowthRespStem        ! gC/m2/s

    ! for outputs ! MB: add RespirationStem, GrowthRespStem in RespirationPlantTot v3.7
    GrossPriProduction  = CarbonAssim                                                                                 ! gC/m2/s
    NetPriProductionTot = NetPriProductionLeaf + NetPriProductionWood + NetPriProductionRoot + NetPriProductionStem   ! gC/m2/s
    RespirationPlantTot = RespirationRoot + RespirationWood + RespirationLeafMaint + RespirationStem + &
                          GrowthRespLeaf + GrowthRespRoot + GrowthRespWood + GrowthRespStem                           ! gC/m2/s
    RespirationSoilOrg  = 0.9 * RespirationSoil                                                                       ! gC/m2/s MB: add 0.9* v3.7
    NetEcoExchange      = (RespirationPlantTot + RespirationSoilOrg - GrossPriProduction) * 44.0 / 12.0               ! gCO2/m2/s
    CarbonMassSoilTot   = CarbonMassShallowSoil + CarbonMassDeepSoil                                                  ! gC/m2
    CarbonMassLiveTot   = LeafMass + RootMass + StemMass + WoodMass                                                   ! gC/m2   MB: add StemMass v3.7
    
    ! leaf area index and stem area index
    LeafAreaIndex       = max(LeafMass*LeafAreaPerMass, LeafAreaIndexMin)
    StemAreaIndex       = max(StemMass*StemAreaPerMass, StemAreaIndexMin)

    end associate

  end subroutine CarbonFluxNatureVeg

end module CarbonFluxNatureVegMod
