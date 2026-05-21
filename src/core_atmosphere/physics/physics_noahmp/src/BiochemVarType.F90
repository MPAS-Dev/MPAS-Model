module BiochemVarType

!!! Define column (1-D) Noah-MP Biochemistry (carbon,nitrogen,etc) variables
!!! Biochemistry variable initialization is done in BiochemVarInitMod.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

  use Machine

  implicit none
  save
  private

!=== define "flux" sub-type of biochem (biochem%flux%variable)
  type :: flux_type

    real(kind=kind_noahmp) :: PhotosynTotal              ! total leaf photosynthesis [umol co2/m2/s]
    real(kind=kind_noahmp) :: PhotosynLeafSunlit         ! sunlit leaf photosynthesis [umol co2/m2/s]
    real(kind=kind_noahmp) :: PhotosynLeafShade          ! shaded leaf photosynthesis [umol co2/m2/s]
    real(kind=kind_noahmp) :: PhotosynCrop               ! crop photosynthesis rate [umol co2/m2/s]
    real(kind=kind_noahmp) :: GrossPriProduction         ! gross primary production [g/m2/s C]
    real(kind=kind_noahmp) :: NetEcoExchange             ! net ecosystem exchange [g/m2/s CO2]
    real(kind=kind_noahmp) :: NetPriProductionTot        ! total net primary production [g/m2/s C]
    real(kind=kind_noahmp) :: NetPriProductionLeaf       ! leaf net primary production [g/m2/s]
    real(kind=kind_noahmp) :: NetPriProductionRoot       ! root net primary production [g/m2/s]
    real(kind=kind_noahmp) :: NetPriProductionWood       ! wood net primary production [g/m2/s]
    real(kind=kind_noahmp) :: NetPriProductionStem       ! stem net primary production [g/m2/s]
    real(kind=kind_noahmp) :: NetPriProductionGrain      ! grain net primary production [g/m2/s] 
    real(kind=kind_noahmp) :: RespirationPlantTot        ! total plant respiration (leaf,stem,root,wood,grain) [g/m2/s C]
    real(kind=kind_noahmp) :: RespirationSoilOrg         ! soil heterotrophic (organic) respiration [g/m2/s C]
    real(kind=kind_noahmp) :: CarbonToAtmos              ! carbon flux to atmosphere [g/m2/s]
    real(kind=kind_noahmp) :: GrowthRespLeaf             ! growth respiration rate for leaf [g/m2/s]
    real(kind=kind_noahmp) :: GrowthRespRoot             ! growth respiration rate for root [g/m2/s]
    real(kind=kind_noahmp) :: GrowthRespWood             ! growth respiration rate for wood [g/m2/s]
    real(kind=kind_noahmp) :: GrowthRespStem             ! growth respiration rate for stem [g/m2/s]
    real(kind=kind_noahmp) :: GrowthRespGrain            ! growth respiration rate for grain [g/m2/s]
    real(kind=kind_noahmp) :: LeafMassMaxChg             ! maximum leaf mass available to change [g/m2/s]
    real(kind=kind_noahmp) :: StemMassMaxChg             ! maximum stem mass available to change [g/m2/s]
    real(kind=kind_noahmp) :: CarbonDecayToStable        ! decay rate of fast carbon to slow carbon [g/m2/s]
    real(kind=kind_noahmp) :: RespirationLeaf            ! leaf respiration [umol CO2/m2/s]
    real(kind=kind_noahmp) :: RespirationStem            ! stem respiration [g/m2/s]
    real(kind=kind_noahmp) :: RespirationWood            ! wood respiration rate [g/m2/s]
    real(kind=kind_noahmp) :: RespirationLeafMaint       ! leaf maintenance respiration rate [g/m2/s]
    real(kind=kind_noahmp) :: RespirationRoot            ! fine root respiration rate [g/m2/s]
    real(kind=kind_noahmp) :: RespirationSoil            ! soil respiration rate [g/m2/s]
    real(kind=kind_noahmp) :: RespirationGrain           ! grain respiration rate [g/m2/s]
    real(kind=kind_noahmp) :: ConvRootToGrain            ! root to grain conversion [g/m2/s]
    real(kind=kind_noahmp) :: ConvStemToGrain            ! stem to grain conversion [g/m2/s]
    real(kind=kind_noahmp) :: ConvLeafToGrain            ! leaf to grain conversion [g/m2/s]
    real(kind=kind_noahmp) :: TurnoverLeaf               ! leaf turnover rate [g/m2/s]
    real(kind=kind_noahmp) :: TurnoverStem               ! stem turnover rate [g/m2/s]
    real(kind=kind_noahmp) :: TurnoverWood               ! wood turnover rate [g/m2/s]
    real(kind=kind_noahmp) :: TurnoverRoot               ! root turnover rate [g/m2/s]
    real(kind=kind_noahmp) :: TurnoverGrain              ! grain turnover rate [g/m2/s]
    real(kind=kind_noahmp) :: DeathLeaf                  ! death rate of leaf mass [g/m2/s]
    real(kind=kind_noahmp) :: DeathStem                  ! death rate of stem mass [g/m2/s]
    real(kind=kind_noahmp) :: CarbonAssim                ! carbon assimilated rate [g/m2/s]
    real(kind=kind_noahmp) :: CarbohydrAssim             ! carbohydrate assimilated rate [g/m2/s]

  end type flux_type


!=== define "state" sub-type of biochem (biochem%state%variable)
  type :: state_type

    integer                :: PlantGrowStage             ! plant growing stage
    integer                :: IndexPlanting              ! Planting index (0=off, 1=on)
    integer                :: IndexHarvest               ! Harvest index (0=on,1=off)
    real(kind=kind_noahmp) :: IndexGrowSeason            ! growing season index (0=off, 1=on)    
    real(kind=kind_noahmp) :: NitrogenConcFoliage        ! foliage nitrogen concentration [%]
    real(kind=kind_noahmp) :: LeafMass                   ! leaf mass [g/m2]
    real(kind=kind_noahmp) :: RootMass                   ! mass of fine roots [g/m2]
    real(kind=kind_noahmp) :: StemMass                   ! stem mass [g/m2]
    real(kind=kind_noahmp) :: WoodMass                   ! mass of wood (include woody roots) [g/m2]
    real(kind=kind_noahmp) :: GrainMass                  ! mass of grain [g/m2]
    real(kind=kind_noahmp) :: CarbonMassDeepSoil         ! stable carbon in deep soil [g/m2]
    real(kind=kind_noahmp) :: CarbonMassShallowSoil      ! short-lived carbon in shallow soil [g/m2]
    real(kind=kind_noahmp) :: CarbonMassSoilTot          ! total soil carbon mass [g/m2 C]
    real(kind=kind_noahmp) :: CarbonMassLiveTot          ! total living carbon mass ([g/m2 C]
    real(kind=kind_noahmp) :: LeafAreaPerMass            ! leaf area per unit mass [m2/g]
    real(kind=kind_noahmp) :: StemAreaPerMass            ! stem area per unit mass (m2/g)
    real(kind=kind_noahmp) :: LeafMassMin                ! minimum leaf mass [g/m2]
    real(kind=kind_noahmp) :: StemMassMin                ! minimum stem mass [g/m2]
    real(kind=kind_noahmp) :: CarbonFracToLeaf           ! fraction of carbon flux allocated to leaves
    real(kind=kind_noahmp) :: CarbonFracToRoot           ! fraction of carbon flux allocated to roots
    real(kind=kind_noahmp) :: CarbonFracToWood           ! fraction of carbon flux allocated to wood
    real(kind=kind_noahmp) :: CarbonFracToStem           ! fraction of carbon flux allocated to stem
    real(kind=kind_noahmp) :: WoodCarbonFrac             ! wood carbon fraction in (root + wood) carbon
    real(kind=kind_noahmp) :: CarbonFracToWoodRoot       ! fraction of carbon to root and wood
    real(kind=kind_noahmp) :: MicroRespFactorSoilWater   ! soil water factor for microbial respiration
    real(kind=kind_noahmp) :: MicroRespFactorSoilTemp    ! soil temperature factor for microbial respiration
    real(kind=kind_noahmp) :: RespFacNitrogenFoliage     ! foliage nitrogen adjustemt factor to respiration (<= 1)
    real(kind=kind_noahmp) :: RespFacTemperature         ! temperature factor for respiration
    real(kind=kind_noahmp) :: RespReductionFac           ! respiration reduction factor (<= 1)
    real(kind=kind_noahmp) :: GrowDegreeDay              ! growing degree days

  end type state_type


!=== define "parameter" sub-type of biochem (biochem%param%variable)
  type :: parameter_type

    integer                :: DatePlanting               ! planting date
    integer                :: DateHarvest                ! harvest date
    real(kind=kind_noahmp) :: QuantumEfficiency25C       ! quantum efficiency at 25c [umol CO2/umol photon]
    real(kind=kind_noahmp) :: CarboxylRateMax25C         ! maximum rate of carboxylation at 25c [umol CO2/m2/s]
    real(kind=kind_noahmp) :: CarboxylRateMaxQ10         ! change in maximum rate of carboxylation for every 10-deg C temperature change
    real(kind=kind_noahmp) :: PhotosynPathC3             ! C3 photosynthetic pathway indicator: 0.0 = c4, 1.0 = c3
    real(kind=kind_noahmp) :: SlopeConductToPhotosyn     ! slope of conductance-to-photosynthesis relationship
    real(kind=kind_noahmp) :: TemperatureMinPhotosyn     ! minimum temperature for photosynthesis [K]
    real(kind=kind_noahmp) :: LeafAreaPerMass1side       ! single-side leaf area per mass [m2/kg] 
    real(kind=kind_noahmp) :: NitrogenConcFoliageMax     ! foliage nitrogen concentration when f(n)=1 (%)
    real(kind=kind_noahmp) :: WoodToRootRatio            ! wood to root ratio
    real(kind=kind_noahmp) :: WoodPoolIndex              ! wood pool index (0~1) depending on woody or not
    real(kind=kind_noahmp) :: TurnoverCoeffLeafVeg       ! leaf turnover coefficient [1/s] for generic vegetation
    real(kind=kind_noahmp) :: LeafDeathWaterCoeffVeg     ! coeficient for leaf water stress death [1/s] for generic vegetation
    real(kind=kind_noahmp) :: LeafDeathTempCoeffVeg      ! coeficient for leaf temperature stress death [1/s] for generic vegetation
    real(kind=kind_noahmp) :: MicroRespCoeff             ! microbial respiration coefficient [umol co2 /kg c/ s]
    real(kind=kind_noahmp) :: RespMaintQ10               ! change in maintenance respiration for every 10-deg C temperature change
    real(kind=kind_noahmp) :: RespMaintLeaf25C           ! leaf maintenance respiration at 25C [umol CO2/m2  /s]
    real(kind=kind_noahmp) :: RespMaintStem25C           ! stem maintenance respiration at 25C [umol CO2/kg bio/s], bio: C or CH2O
    real(kind=kind_noahmp) :: RespMaintRoot25C           ! root maintenance respiration at 25C [umol CO2/kg bio/s], bio: C or CH2O
    real(kind=kind_noahmp) :: RespMaintGrain25C          ! grain maintenance respiration at 25C [umol CO2/kg bio/s], bio: C or CH2O
    real(kind=kind_noahmp) :: GrowthRespFrac             ! fraction of growth respiration 
    real(kind=kind_noahmp) :: TemperaureLeafFreeze       ! characteristic temperature for leaf freezing [K]
    real(kind=kind_noahmp) :: LeafAreaPerBiomass         ! leaf area per living leaf biomass [m2/g]
    real(kind=kind_noahmp) :: TempBaseGrowDegDay         ! Base temperature for growing degree day (GDD) accumulation [C]
    real(kind=kind_noahmp) :: TempMaxGrowDegDay          ! Maximum temperature for growing degree day (GDD) accumulation [C]
    real(kind=kind_noahmp) :: GrowDegDayEmerg            ! growing degree day (GDD) from seeding to emergence
    real(kind=kind_noahmp) :: GrowDegDayInitVeg          ! growing degree day (GDD) from seeding to initial vegetative 
    real(kind=kind_noahmp) :: GrowDegDayPostVeg          ! growing degree day (GDD) from seeding to post vegetative 
    real(kind=kind_noahmp) :: GrowDegDayInitReprod       ! growing degree day (GDD) from seeding to intial reproductive
    real(kind=kind_noahmp) :: GrowDegDayMature           ! growing degree day (GDD) from seeding to pysical maturity 
    real(kind=kind_noahmp) :: PhotosynRadFrac            ! Fraction of incoming solar radiation to photosynthetically active radiation 
    real(kind=kind_noahmp) :: TempMinCarbonAssim         ! Minimum temperature for CO2 assimulation [C]
    real(kind=kind_noahmp) :: TempMaxCarbonAssim         ! CO2 assimulation linearly increasing until reaching this temperature [C]
    real(kind=kind_noahmp) :: TempMaxCarbonAssimMax      ! CO2 assmilation rate remain at CarbonAssimRefMax until reaching this temperature [C]
    real(kind=kind_noahmp) :: CarbonAssimRefMax          ! reference maximum CO2 assimilation rate [g co2/m2/s] 
    real(kind=kind_noahmp) :: LightExtCoeff              ! light extinction coefficient 
    real(kind=kind_noahmp) :: LightUseEfficiency         ! initial light use efficiency 
    real(kind=kind_noahmp) :: CarbonAssimReducFac        ! CO2 assimilation reduction factor(0-1) (caused by non-modeling part,e.g.pest,weeds)
    real(kind=kind_noahmp) :: StemAreaIndexMin           ! minimum stem area index [m2/m2]
    real(kind=kind_noahmp) :: WoodAllocFac               ! present wood allocation factor
    real(kind=kind_noahmp) :: WaterStressCoeff           ! water stress coeficient
    real(kind=kind_noahmp) :: LeafAreaIndexMin           ! minimum leaf area index [m2/m2] 
    real(kind=kind_noahmp) :: TurnoverCoeffRootVeg       ! root turnover coefficient [1/s] for generic vegetation
    real(kind=kind_noahmp) :: WoodRespCoeff              ! wood respiration coeficient [1/s]

    real(kind=kind_noahmp), allocatable, dimension(:) :: LeafDeathTempCoeffCrop      ! coeficient for leaf temperature stress death [1/s] for crop
    real(kind=kind_noahmp), allocatable, dimension(:) :: LeafDeathWaterCoeffCrop     ! coeficient for leaf water stress death [1/s] for crop
    real(kind=kind_noahmp), allocatable, dimension(:) :: CarbohydrLeafToGrain        ! fraction of carbohydrate flux transallocate from leaf to grain
    real(kind=kind_noahmp), allocatable, dimension(:) :: CarbohydrStemToGrain        ! fraction of carbohydrate flux transallocate from stem to grain
    real(kind=kind_noahmp), allocatable, dimension(:) :: CarbohydrRootToGrain        ! fraction of carbohydrate flux transallocate from root to grain
    real(kind=kind_noahmp), allocatable, dimension(:) :: CarbohydrFracToLeaf         ! fraction of carbohydrate flux to leaf for crop
    real(kind=kind_noahmp), allocatable, dimension(:) :: CarbohydrFracToStem         ! fraction of carbohydrate flux to stem for crop
    real(kind=kind_noahmp), allocatable, dimension(:) :: CarbohydrFracToRoot         ! fraction of carbohydrate flux to root for crop
    real(kind=kind_noahmp), allocatable, dimension(:) :: CarbohydrFracToGrain        ! fraction of carbohydrate flux to grain for crop
    real(kind=kind_noahmp), allocatable, dimension(:) :: TurnoverCoeffLeafCrop       ! leaf turnover coefficient [1/s] for crop
    real(kind=kind_noahmp), allocatable, dimension(:) :: TurnoverCoeffStemCrop       ! stem turnover coefficient [1/s] for crop
    real(kind=kind_noahmp), allocatable, dimension(:) :: TurnoverCoeffRootCrop       ! root tunrover coefficient [1/s] for crop

  end type parameter_type


!=== define biochem type that includes 3 subtypes (flux,state,parameter)
  type, public :: biochem_type

    type(flux_type)      :: flux
    type(state_type)     :: state
    type(parameter_type) :: param

  end type biochem_type

end module BiochemVarType
