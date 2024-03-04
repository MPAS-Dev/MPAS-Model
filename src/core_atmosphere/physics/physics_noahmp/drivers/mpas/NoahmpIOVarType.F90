module NoahmpIOVarType

!!! Define Noah-MP Input variables (2D forcing, namelist, table, static)
!!! Input variable initialization is done in NoahmpIOVarInitMod.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

  use Machine

  implicit none
  save
  private

  type, public :: NoahmpIO_type

!------------------------------------------------------------------------
! general 2-D/3-D Noah-MP variables
!------------------------------------------------------------------------

    ! IN only (as defined in WRF)
    integer                                                ::  ids,ide, &          ! d -> domain 
                                                               jds,jde, &          ! d -> domain
                                                               kds,kde, &          ! d -> domain
                                                               ims,ime, &          ! m -> memory
                                                               jms,jme, &          ! m -> memory
                                                               kms,kme, &          ! m -> memory
                                                               its,ite, &          ! t -> tile
                                                               jts,jte, &          ! t -> tile
                                                               kts,kte             ! t -> tile
    integer                                                ::  ITIMESTEP           ! timestep number
    integer                                                ::  YR                  ! 4-digit year
    integer                                                ::  NSOIL               ! number of soil layers
    integer                                                ::  ICE                 ! Sea-ice point
    integer                                                ::  ISICE               ! land cover category for ice
    integer                                                ::  ISURBAN             ! land cover category for urban
    integer                                                ::  ISWATER             ! land cover category for water
    integer                                                ::  ISLAKE              ! land cover category for lake
    integer                                                ::  URBTYPE_beg         ! urban type start number - 1
    integer                                                ::  IOPT_DVEG           ! dynamic vegetation   
    integer                                                ::  IOPT_CRS            ! canopy stomatal resistance (1-> Ball-Berry; 2->Jarvis)   
    integer                                                ::  IOPT_BTR            ! soil moisture factor for stomatal resistance (1-> Noah; 2-> CLM; 3-> SSiB)
    integer                                                ::  IOPT_RUNSRF         ! surface runoff and groundwater (1->SIMGM; 2->SIMTOP; 3->Schaake96; 4->BATS)
    integer                                                ::  IOPT_RUNSUB         ! subsurface runoff option
    integer                                                ::  IOPT_SFC            ! surface layer drag coeff (CH & CM) (1->M-O; 2->Chen97)
    integer                                                ::  IOPT_FRZ            ! supercooled liquid water (1-> NY06; 2->Koren99)
    integer                                                ::  IOPT_INF            ! frozen soil permeability (1-> NY06; 2->Koren99)
    integer                                                ::  IOPT_RAD            ! radiation transfer (1->gap=F(3D,cosz); 2->gap=0; 3->gap=1-Fveg)
    integer                                                ::  IOPT_ALB            ! snow surface albedo (1->BATS; 2->CLASS)
    integer                                                ::  IOPT_SNF            ! rainfall & snowfall (1-Jordan91; 2->BATS; 3->Noah)
    integer                                                ::  IOPT_TKSNO          ! snow thermal conductivity: 1 -> Stieglitz(yen,1965) scheme (default), 2 -> Anderson, 1976 scheme, 3 -> constant, 4 -> Verseghy (1991) scheme, 5 -> Douvill(Yen, 1981) scheme
    integer                                                ::  IOPT_TBOT           ! lower boundary of soil temperature (1->zero-flux; 2->Noah)
    integer                                                ::  IOPT_STC            ! snow/soil temperature time scheme
    integer                                                ::  IOPT_GLA            ! glacier option (1->phase change; 2->simple)
    integer                                                ::  IOPT_RSF            ! surface resistance option (1->Zeng; 2->simple)
    integer                                                ::  IZ0TLND             ! option of Chen adjustment of Czil (not used)
    integer                                                ::  IOPT_SOIL           ! soil configuration option
    integer                                                ::  IOPT_PEDO           ! soil pedotransfer function option
    integer                                                ::  IOPT_CROP           ! crop model option (0->none; 1->Liu et al.)
    integer                                                ::  IOPT_IRR            ! irrigation scheme (0->none; >1 irrigation scheme ON)
    integer                                                ::  IOPT_IRRM           ! irrigation method (0->dynamic; 1-> sprinkler; 2-> micro; 3-> flood)
    integer                                                ::  IOPT_INFDV          ! infiltration options for dynamic VIC (1->Philip; 2-> Green-Ampt;3->Smith-Parlange)
    integer                                                ::  IOPT_TDRN           ! drainage option (0->off; 1->simple scheme; 2->Hooghoudt's scheme)
    real(kind=kind_noahmp)                                 ::  XICE_THRESHOLD      ! fraction of grid determining seaice
    real(kind=kind_noahmp)                                 ::  JULIAN              ! Julian day
    real(kind=kind_noahmp)                                 ::  DTBL                ! timestep [s]
    real(kind=kind_noahmp)                                 ::  DX                  ! horizontal grid spacing [m]
    real(kind=kind_noahmp)                                 ::  soiltstep           ! soil time step (s) (default=0: same as main NoahMP timstep)
    logical                                                ::  FNDSNOWH            ! snow depth present in input
    logical                                                ::  calculate_soil      ! logical index for if do soil calculation
    integer                                                ::  soil_update_steps   ! number of model time steps to update soil process
    integer,                allocatable, dimension(:,:)    ::  IVGTYP              ! vegetation type
    integer,                allocatable, dimension(:,:)    ::  ISLTYP              ! soil type
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  COSZEN              ! cosine zenith angle
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  XLAT                ! latitude [rad]
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  DZ8W                ! thickness of atmo layers [m]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  DZS                 ! thickness of soil layers [m]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  ZSOIL               ! depth to soil interfaces [m]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  VEGFRA              ! vegetation fraction []
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  TMN                 ! deep soil temperature [K]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  XLAND               ! =2 ocean; =1 land/seaice
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  XICE                ! fraction of grid that is seaice
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  SEAICE              ! seaice fraction

    ! forcings    
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  T_PHY               ! 3D atmospheric temperature valid at mid-levels [K]
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  QV_CURR             ! 3D water vapor mixing ratio [kg/kg_dry]
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  U_PHY               ! 3D U wind component [m/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  V_PHY               ! 3D V wind component [m/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  SWDOWN              ! solar down at surface [W m-2]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  GLW                 ! longwave down at surface [W m-2]
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  P8W                 ! 3D pressure, valid at interface [Pa]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  RAINBL              ! precipitation entering land model [mm] per time step
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  SNOWBL              ! snow entering land model [mm] per time step
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  SR                  ! frozen precip ratio entering land model [-]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  RAINCV              ! convective precip forcing [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  RAINNCV             ! non-convective precip forcing [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  RAINSHV             ! shallow conv. precip forcing [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  SNOWNCV             ! non-covective snow forcing (subset of rainncv) [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  GRAUPELNCV          ! non-convective graupel forcing (subset of rainncv) [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  HAILNCV             ! non-convective hail forcing (subset of rainncv) [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  MP_RAINC            ! convective precipitation entering land model [mm] ! MB/AN : v3.7
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  MP_RAINNC           ! large-scale precipitation entering land model [mm]! MB/AN : v3.7
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  MP_SHCV             ! shallow conv precip entering land model [mm]      ! MB/AN : v3.7
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  MP_SNOW             ! snow precipitation entering land model [mm]       ! MB/AN : v3.7 
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  MP_GRAUP            ! graupel precipitation entering land model [mm]    ! MB/AN : v3.7
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  MP_HAIL             ! hail precipitation entering land model [mm]       ! MB/AN : v3.7 
    
#ifdef WRF_HYDRO
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: infxsrt              ! surface infiltration
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: sfcheadrt            ! surface water head
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: soldrain             ! soil drainage
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: qtiledrain           ! tile drainage
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: ZWATBLE2D            ! water table depth
#endif

    ! Spatially varying fields (for now it is de-activated)
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  soilcomp            ! Soil sand and clay content [fraction]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  soilcl1             ! Soil texture class with depth
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  soilcl2             ! Soil texture class with depth
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  soilcl3             ! Soil texture class with depth
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  soilcl4             ! Soil texture class with depth
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  bexp_3D             ! C-H B exponent  
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  smcdry_3D           ! Soil Moisture Limit: Dry
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  smcwlt_3D           ! Soil Moisture Limit: Wilt
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  smcref_3D           ! Soil Moisture Limit: Reference
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  smcmax_3D           ! Soil Moisture Limit: Max
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  dksat_3D            ! Saturated Soil Conductivity
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  dwsat_3D            ! Saturated Soil Diffusivity
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  psisat_3D           ! Saturated Matric Potential
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  quartz_3D           ! Soil quartz content
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  refdk_2D            ! Reference Soil Conductivity
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  refkdt_2D           ! Soil Infiltration Parameter
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  irr_frac_2D         ! irrigation Fraction
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  irr_har_2D          ! number of days before harvest date to stop irrigation 
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  irr_lai_2D          ! Minimum lai to trigger irrigation
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  irr_mad_2D          ! management allowable deficit (0-1)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  filoss_2D           ! fraction of flood irrigation loss (0-1) 
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  sprir_rate_2D       ! mm/h, sprinkler irrigation rate
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  micir_rate_2D       ! mm/h, micro irrigation rate
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  firtfac_2D          ! flood application rate factor
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  ir_rain_2D          ! maximum precipitation to stop irrigation trigger
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  bvic_2d             ! VIC model infiltration parameter [-] opt_run=6
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  axaj_2D             ! Tension water distribution inflection parameter [-] opt_run=7
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  bxaj_2D             ! Tension water distribution shape parameter [-] opt_run=7
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  xxaj_2D             ! Free water distribution shape parameter [-] opt_run=7
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  bdvic_2d            ! VIC model infiltration parameter [-] opt_run=8
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  gdvic_2d            ! Mean Capillary Drive (m) for infiltration models opt_run=8
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  bbvic_2d            ! DVIC heterogeniety parameter for infiltration [-] opt_run=8
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  KLAT_FAC            ! factor multiplier to hydraulic conductivity
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  TDSMC_FAC           ! factor multiplier to field capacity
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  TD_DC               ! drainage coefficient for simple
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  TD_DCOEF            ! drainge coefficient for Hooghoudt 
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  TD_DDRAIN           ! depth of drain
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  TD_RADI             ! tile radius
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  TD_SPAC             ! tile spacing

    ! INOUT (with generic LSM equivalent) (as defined in WRF)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  TSK                 ! surface radiative temperature [K]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  HFX                 ! sensible heat flux [W m-2]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QFX                 ! latent heat flux [kg s-1 m-2]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  LH                  ! latent heat flux [W m-2]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  GRDFLX              ! ground/snow heat flux [W m-2]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  SMSTAV              ! soil moisture avail. [not used]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  SMSTOT              ! total soil water [mm][not used]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  SFCRUNOFF           ! accumulated surface runoff [m]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  UDRUNOFF            ! accumulated sub-surface runoff [m]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  ALBEDO              ! total grid albedo []
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  SNOWC               ! snow cover fraction []
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  SMOISEQ             ! volumetric soil moisture [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  SMOIS               ! volumetric soil moisture [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  SH2O                ! volumetric liquid soil moisture [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  TSLB                ! soil temperature [K]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  SNOW                ! snow water equivalent [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  SNOWH               ! physical snow depth [m]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  CANWAT              ! total canopy water + ice [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  ACSNOM              ! accumulated snow melt leaving pack
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  ACSNOW              ! accumulated snow on grid
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  EMISS               ! surface bulk emissivity
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QSFC                ! bulk surface specific humidity

    ! INOUT (with no Noah LSM equivalent) (as defined in WRF)
    integer, allocatable, dimension(:,:)                   ::  ISNOWXY             ! actual no. of snow layers
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  TVXY                ! vegetation leaf temperature
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  TGXY                ! bulk ground surface temperature
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  CANICEXY            ! canopy-intercepted ice (mm)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  CANLIQXY            ! canopy-intercepted liquid water (mm)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  EAHXY               ! canopy air vapor pressure (pa)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  TAHXY               ! canopy air temperature (k)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  CMXY                ! bulk momentum drag coefficient
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  CHXY                ! bulk sensible heat exchange coefficient
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  FWETXY              ! wetted or snowed fraction of the canopy (-)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  SNEQVOXY            ! snow mass at last time step(mm h2o)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  ALBOLDXY            ! snow albedo at last time step (-)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QSNOWXY             ! snowfall on the ground [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QRAINXY             ! rainfall on the ground [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  WSLAKEXY            ! lake water storage [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  ZWTXY               ! water table depth [m]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  WAXY                ! water in the "aquifer" [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  WTXY                ! groundwater storage [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  SMCWTDXY            ! groundwater storage [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  DEEPRECHXY          ! groundwater storage [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  RECHXY              ! groundwater storage [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  TSNOXY              ! snow temperature [K]
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  ZSNSOXY             ! snow layer depth [m]
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  SNICEXY             ! snow layer ice [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  SNLIQXY             ! snow layer liquid water [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  LFMASSXY            ! leaf mass [g/m2]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  RTMASSXY            ! mass of fine roots [g/m2]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  STMASSXY            ! stem mass [g/m2]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  WOODXY              ! mass of wood (incl. woody roots) [g/m2]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  GRAINXY             ! XING mass of grain!THREE
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  GDDXY               ! XINGgrowingdegressday
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  STBLCPXY            ! stable carbon in deep soil [g/m2]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  FASTCPXY            ! short-lived carbon, shallow soil [g/m2]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  LAI                 ! leaf area index
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  XSAIXY              ! stem area index
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  TAUSSXY             ! snow age factor

    ! irrigation
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: IRFRACT              ! irrigation fraction
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: SIFRACT              ! sprinkler irrigation fraction
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: MIFRACT              ! micro irrigation fraction
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: FIFRACT              ! flood irrigation fraction   
    integer, allocatable, dimension(:,:)                   :: IRNUMSI              ! irrigation event number, Sprinkler
    integer, allocatable, dimension(:,:)                   :: IRNUMMI              ! irrigation event number, Micro
    integer, allocatable, dimension(:,:)                   :: IRNUMFI              ! irrigation event number, Flood 
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: IRWATSI              ! irrigation water amount [m] to be applied, Sprinkler
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: IRWATMI              ! irrigation water amount [m] to be applied, Micro
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: IRWATFI              ! irrigation water amount [m] to be applied, Flood
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: IRELOSS              ! loss of irrigation water to evaporation,sprinkler [m/timestep]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: IRSIVOL              ! amount of irrigation by sprinkler (mm)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: IRMIVOL              ! amount of irrigation by micro (mm)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: IRFIVOL              ! amount of irrigation by micro (mm)  
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: IRRSPLH              ! latent heating from sprinkler evaporation (w/m2)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: LOCTIM               ! local time
 
    ! OUT (with no Noah LSM equivalent) (as defined in WRF)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  T2MVXY              ! 2m temperature of vegetation part
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  T2MBXY              ! 2m temperature of bare ground part
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  Q2MVXY              ! 2m mixing ratio of vegetation part
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  Q2MBXY              ! 2m mixing ratio of bare ground part
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  TRADXY              ! surface radiative temperature (k)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  NEEXY               ! net ecosys exchange (g/m2/s CO2)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  GPPXY               ! gross primary assimilation [g/m2/s C]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  NPPXY               ! net primary productivity [g/m2/s C]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  FVEGXY              ! Noah-MP vegetation fraction [-]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  RUNSFXY             ! surface runoff [mm per soil timestep]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  RUNSBXY             ! subsurface runoff [mm per soil timestep]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  ECANXY              ! evaporation of intercepted water (mm/s)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  EDIRXY              ! soil surface evaporation rate (mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  ETRANXY             ! transpiration rate (mm/s)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  FSAXY               ! total absorbed solar radiation (w/m2)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  FIRAXY              ! total net longwave rad (w/m2) [+ to atm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  APARXY              ! photosyn active energy by canopy (w/m2)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  PSNXY               ! total photosynthesis (umol co2/m2/s) [+]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  SAVXY               ! solar rad absorbed by veg. (w/m2)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  SAGXY               ! solar rad absorbed by ground (w/m2)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  RSSUNXY             ! sunlit leaf stomatal resistance (s/m)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  RSSHAXY             ! shaded leaf stomatal resistance (s/m)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  BGAPXY              ! between gap fraction
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  WGAPXY              ! within gap fraction
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  TGVXY               ! under canopy ground temperature[K]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  TGBXY               ! bare ground temperature [K]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  CHVXY               ! sensible heat exchange coefficient vegetated
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  CHBXY               ! sensible heat exchange coefficient bare-ground
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  SHGXY               ! veg ground sen. heat [w/m2]   [+ to atm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  SHCXY               ! canopy sen. heat [w/m2]   [+ to atm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  SHBXY               ! bare sensible heat [w/m2]  [+ to atm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  EVGXY               ! veg ground evap. heat [w/m2]  [+ to atm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  EVBXY               ! bare soil evaporation [w/m2]  [+ to atm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  GHVXY               ! veg ground heat flux [w/m2]  [+ to soil]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  GHBXY               ! bare ground heat flux [w/m2] [+ to soil]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  IRGXY               ! veg ground net LW rad. [w/m2] [+ to atm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  IRCXY               ! canopy net LW rad. [w/m2] [+ to atm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  IRBXY               ! bare net longwave rad. [w/m2] [+ to atm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  TRXY                ! transpiration [w/m2]  [+ to atm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  EVCXY               ! canopy evaporation heat [w/m2]  [+ to atm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  CHLEAFXY            ! leaf exchange coefficient 
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  CHUCXY              ! under canopy exchange coefficient 
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  CHV2XY              ! veg 2m exchange coefficient 
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  CHB2XY              ! bare 2m exchange coefficient 
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  RS                  ! Total stomatal resistance [s/m]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  Z0                  ! roughness length output to WRF
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  ZNT                 ! roughness length output to WRF
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QTDRAIN             ! tile drain discharge [mm]

    ! additional output variables
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  PAHXY               ! precipitation advected heat [W/m2]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  PAHGXY              ! precipitation advected heat [W/m2]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  PAHBXY              ! precipitation advected heat [W/m2]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  PAHVXY              ! precipitation advected heat [W/m2]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QINTSXY             ! canopy intercepted snow [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QINTRXY             ! canopy intercepted rain [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QDRIPSXY            ! canopy dripping snow [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QDRIPRXY            ! canopy dripping rain [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QTHROSXY            ! canopy throughfall snow [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QTHRORXY            ! canopy throughfall rain [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QSNSUBXY            ! snowpack sublimation rate [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QMELTXY             ! snowpack melting rate due to phase change [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QSNFROXY            ! snowpack frost rate [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QSUBCXY             ! canopy snow sublimation rate [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QFROCXY             ! canopy snow frost rate [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QEVACXY             ! canopy water evaporation rate [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QDEWCXY             ! canopy water dew rate [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QFRZCXY             ! canopy water freezing rate [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QMELTCXY            ! canopy snow melting rate [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QSNBOTXY            ! total water (melt+rain through snow) out of snowpack bottom [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  PONDINGXY           ! total surface ponding [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  FPICEXY             ! fraction of ice in total precipitation
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  RAINLSM             ! total rain rate at the surface [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  SNOWLSM             ! total snow rate at the surface [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  FORCTLSM            ! surface temperature as LSM forcing [K]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  FORCQLSM            ! surface specific humidity as LSM forcing [kg/kg]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  FORCPLSM            ! surface pressure as LSM forcing [Pa]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  FORCZLSM            ! reference height as LSM input [m]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  FORCWLSM            ! surface wind speed as LSM forcing [m/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  ACC_SSOILXY         ! accumulated ground heat flux [W/m2 * dt_soil/dt_main]  
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  ACC_QINSURXY        ! accumulated water flux into soil [m/s * dt_soil/dt_main]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  ACC_QSEVAXY         ! accumulated soil surface evaporation [m/s * dt_soil/dt_main]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  EFLXBXY             ! accumulated heat flux through soil bottom per soil timestep [J/m2]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  SOILENERGY          ! energy content in soil relative to 273.16 [KJ/m2]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  SNOWENERGY          ! energy content in snow relative to 273.16 [KJ/m2]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  CANHSXY             ! canopy heat storage change [W/m2]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  ACC_DWATERXY        ! accumulated snow,soil,canopy water change per soil timestep [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  ACC_PRCPXY          ! accumulated precipitation per soil timestep [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  ACC_ECANXY          ! accumulated net canopy evaporation per soil timestep [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  ACC_ETRANXY         ! accumulated transpiration per soil timestep [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  ACC_EDIRXY          ! accumulated net ground (soil/snow) evaporation per soil timestep [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  ACC_ETRANIXY        ! accumualted transpiration rate within soil timestep [m/s * dt_soil/dt_main]

!------------------------------------------------------------------------
! Needed for MMF_RUNOFF (IOPT_RUN = 5); not part of MP driver in WRF
!------------------------------------------------------------------------

    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  MSFTX               ! mapping factor x
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  MSFTY               ! mapping factor y
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  EQZWT               ! equilibrium water table
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  RIVERBEDXY          ! riverbed depth
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  RIVERCONDXY         ! river conductivity
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  PEXPXY              ! exponential factor
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  FDEPTHXY            ! depth
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  AREAXY              ! river area
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QRFSXY              ! accumulated groundwater baseflow [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QSPRINGSXY          ! accumulated seeping water [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QRFXY               ! groundwater baselow [m]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QSPRINGXY           ! seeping water [m]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QSLATXY             ! accumulated lateral flow [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QLATXY              ! lateral flow [m]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  RECHCLIM            ! climatology recharge
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  RIVERMASK           ! river mask
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  NONRIVERXY          ! non-river portion
    real(kind=kind_noahmp)                                 ::  WTDDT  = 30.0       ! frequency of groundwater call [minutes]
    integer                                                ::  STEPWTD             ! step of groundwater call

!------------------------------------------------------------------------
! Needed for TILE DRAINAGE IF IOPT_TDRN = 1 OR 2
!------------------------------------------------------------------------
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  TD_FRACTION         ! tile drainage fraction

!------------------------------------------------------------------------
! Needed for crop model (OPT_CROP=1)
!------------------------------------------------------------------------

    integer, allocatable, dimension(:,:)                   :: PGSXY                ! plant growth stage
    integer, allocatable, dimension(:,:)                   :: CROPCAT              ! crop category
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: PLANTING             ! planting day
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: HARVEST              ! harvest day
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: SEASON_GDD           ! seasonal GDD
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  :: CROPTYPE             ! crop type

!------------------------------------------------------------------------
! Single- and Multi-layer Urban Models
!------------------------------------------------------------------------

    integer                                                ::  num_urban_atmosphere ! atmospheric levels including ZLVL for BEP/BEM models
    integer                                                ::  IRI_URBAN            ! urban irrigation flag (move from module_sf_urban to here)
    real(kind=kind_noahmp)                                 ::  GMT                  ! Hour of day (fractional) (needed for urban)
    integer                                                ::  JULDAY               ! Integer day (needed for urban)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  HRANG                ! hour angle (needed for urban)
    real(kind=kind_noahmp)                                 ::  DECLIN               ! declination (needed for urban)
    integer                                                ::  num_roof_layers = 4  ! roof layer number
    integer                                                ::  num_road_layers = 4  ! road layer number
    integer                                                ::  num_wall_layers = 4  ! wall layer number
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  cmr_sfcdif
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  chr_sfcdif
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  cmc_sfcdif
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  chc_sfcdif
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  cmgr_sfcdif
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  chgr_sfcdif
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  tr_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  tb_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  tg_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  tc_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  qc_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  uc_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  xxxr_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  xxxb_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  xxxg_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  xxxc_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  trl_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  tbl_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  tgl_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  sh_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  lh_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  g_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  rn_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  ts_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  psim_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  psih_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  u10_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  v10_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  GZ1OZ0_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  AKMS_URB2D
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  th2_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  q2_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  ust_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  dzr
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  dzb
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  dzg
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  cmcr_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  tgr_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  tgrl_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  smr_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  drelr_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  drelb_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  drelg_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  flxhumr_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  flxhumb_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  flxhumg_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  frc_urb2d
    integer, allocatable, dimension(:,:)                   ::  utype_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  chs
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  chs2
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  cqs2
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  trb_urb4d
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  tw1_urb4d
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  tw2_urb4d
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  tgb_urb4d
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  tlev_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  qlev_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  tw1lev_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  tw2lev_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  tglev_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  tflev_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  sf_ac_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  lf_ac_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  cm_ac_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  sfvent_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  lfvent_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  sfwin1_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  sfwin2_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  sfw1_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  sfw2_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  sfr_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  sfg_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  lp_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  hi_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  lb_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  hgt_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  mh_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  stdh_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  lf_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  theta_urban
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  u_urban
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  v_urban
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  dz_urban
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  rho_urban
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  p_urban
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  ust
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  a_u_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  a_v_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  a_t_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  a_q_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  a_e_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  b_u_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  b_v_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  b_t_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  b_q_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  b_e_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  dlg_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  dl_u_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  sf_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  vl_bep
    real(kind=kind_noahmp)                                 ::  height_urban

    ! new urban variables for green roof, PVP for BEP_BEM scheme=3, Zonato et al., 2021
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  EP_PV_URB3D
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  QGR_URB3D
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  TGR_URB3D
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  DRAINGR_URB3D
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  T_PV_URB3D
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  TRV_URB4D
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  QR_URB4D
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  DRAIN_URB4D
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  SFRV_URB3D
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  LFRV_URB3D
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  DGR_URB3D
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  DG_URB3D
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  LFR_URB3D
    real(kind=kind_noahmp), allocatable, dimension(:,:,:)  ::  LFG_URB3D
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  SWDDIR              ! solar down at surface [W m-2]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  SWDDIF

!------------------------------------------------------------------------
! 2D variables not used in WRF - should be removed?
!------------------------------------------------------------------------

    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  XLONG               ! longitude
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  TERRAIN             ! terrain height
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  GVFMIN              ! annual minimum in vegetation fraction
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  GVFMAX              ! annual maximum in vegetation fraction

!------------------------------------------------------------------------
! End 2D variables not used in WRF
!------------------------------------------------------------------------

    CHARACTER(LEN=256)                                     ::  MMINSL  = 'STAS'    ! soil classification
    CHARACTER(LEN=256)                                     ::  LLANDUSE            ! (=USGS, using USGS landuse classification)

!------------------------------------------------------------------------
! Timing:
!------------------------------------------------------------------------

    integer                                                ::  NTIME               ! timesteps
    integer                                                ::  clock_count_1 = 0
    integer                                                ::  clock_count_2 = 0
    integer                                                ::  clock_rate    = 0
    real(kind=kind_noahmp)                                 ::  timing_sum    = 0.0
    integer                                                ::  sflx_count_sum
    integer                                                ::  count_before_sflx
    integer                                                ::  count_after_sflx

!---------------------------------------------------------------------
!  DECLARE/Initialize constants
!---------------------------------------------------------------------

    integer                                                ::  I
    integer                                                ::  J
    integer                                                ::  SLOPETYP
    integer                                                ::  YEARLEN
    integer                                                ::  NSNOW = 3            ! number of snow layers fixed to 3
    logical                                                ::  update_lai, update_veg
    integer                                                ::  spinup_loop
    logical                                                ::  reset_spinup_date

!---------------------------------------------------------------------
!  File naming, parallel
!---------------------------------------------------------------------

    character(len=19)                                      ::  olddate, &
                                                               newdate, &
                                                               startdate
    character                                              ::  hgrid
    integer                                                ::  igrid
    logical                                                ::  lexist
    integer                                                ::  imode
    integer                                                ::  ixfull
    integer                                                ::  jxfull
    integer                                                ::  ixpar
    integer                                                ::  jxpar
    integer                                                ::  xstartpar
    integer                                                ::  ystartpar
    integer                                                ::  rank = 0
    character(len=256)                                     ::  inflnm,  &
                                                               outflnm, &
                                                               inflnm_template
    logical                                                ::  restart_flag
    character(len=256)                                     ::  restart_flnm
    integer                                                ::  ierr

!---------------------------------------------------------------------
! Attributes from LDASIN input file (or HRLDAS_SETUP_FILE, as the case may be)
!---------------------------------------------------------------------

    integer                                                ::  IX
    integer                                                ::  JX
    real(kind=kind_noahmp)                                 ::  DY
    real(kind=kind_noahmp)                                 ::  TRUELAT1
    real(kind=kind_noahmp)                                 ::  TRUELAT2
    real(kind=kind_noahmp)                                 ::  CEN_LON
    integer                                                ::  MAPPROJ
    real(kind=kind_noahmp)                                 ::  LAT1
    real(kind=kind_noahmp)                                 ::  LON1

!---------------------------------------------------------------------
!  NAMELIST start
!---------------------------------------------------------------------

    character(len=256)                                     ::  indir
    ! nsoil defined above
    integer                                                ::  forcing_timestep
    integer                                                ::  noah_timestep      
    integer                                                ::  start_year
    integer                                                ::  start_month
    integer                                                ::  start_day
    integer                                                ::  start_hour
    integer                                                ::  start_min
    character(len=256)                                     ::  outdir
    character(len=256)                                     ::  restart_filename_requested
    integer                                                ::  restart_frequency_hours
    integer                                                ::  output_timestep
    integer                                                ::  spinup_loops

    integer                                                ::  sf_urban_physics
    integer                                                ::  use_wudapt_lcz
    integer                                                ::  num_urban_ndm
    integer                                                ::  num_urban_ng
    integer                                                ::  num_urban_nwr
    integer                                                ::  num_urban_ngb
    integer                                                ::  num_urban_nf
    integer                                                ::  num_urban_nz
    integer                                                ::  num_urban_nbui
    integer                                                ::  num_urban_hi
    integer                                                ::  num_urban_ngr
    real(kind=kind_noahmp)                                 ::  urban_atmosphere_thickness

   ! derived urban dimensions
    integer                                                ::  urban_map_zrd
    integer                                                ::  urban_map_zwd
    integer                                                ::  urban_map_gd
    integer                                                ::  urban_map_zd
    integer                                                ::  urban_map_zdf
    integer                                                ::  urban_map_bd
    integer                                                ::  urban_map_wd
    integer                                                ::  urban_map_gbd
    integer                                                ::  urban_map_fbd
    integer                                                ::  urban_map_zgrd
    integer                                                ::  max_urban_dim  ! C. He: maximum urban dimension for urban variable

    character(len=256)                                     ::  forcing_name_T
    character(len=256)                                     ::  forcing_name_Q
    character(len=256)                                     ::  forcing_name_U
    character(len=256)                                     ::  forcing_name_V
    character(len=256)                                     ::  forcing_name_P
    character(len=256)                                     ::  forcing_name_LW
    character(len=256)                                     ::  forcing_name_SW
    character(len=256)                                     ::  forcing_name_PR
    character(len=256)                                     ::  forcing_name_SN

    integer                                                ::  noahmp_output       ! =0: default output; >0 include additional output
    integer                                                ::  split_output_count
    logical                                                ::  skip_first_output
    integer                                                ::  khour
    integer                                                ::  kday
    real(kind=kind_noahmp)                                 ::  zlvl 
    character(len=256)                                     ::  hrldas_setup_file
    character(len=256)                                     ::  spatial_filename
    character(len=256)                                     ::  external_veg_filename_template
    character(len=256)                                     ::  external_lai_filename_template
    character(len=256)                                     ::  agdata_flnm
    character(len=256)                                     ::  tdinput_flnm
    integer                                                ::  xstart
    integer                                                ::  ystart
    integer                                                ::  xend
    integer                                                ::  yend
    integer                                                ::  MAX_SOIL_LEVELS
    real(kind=kind_noahmp),  allocatable, dimension(:)     ::  soil_thick_input

!----------------------------------------------------------------
! Noahmp Parameters Table 
!----------------------------------------------------------------

    ! vegetation parameters
    character(len=256)                                     :: VEG_DATASET_DESCRIPTION_TABLE
    integer                                                :: NVEG_TABLE                ! number of vegetation types
    integer                                                :: ISURBAN_TABLE             ! urban flag
    integer                                                :: ISWATER_TABLE             ! water flag
    integer                                                :: ISBARREN_TABLE            ! barren ground flag
    integer                                                :: ISICE_TABLE               ! ice flag
    integer                                                :: ISCROP_TABLE              ! cropland flag
    integer                                                :: EBLFOREST_TABLE           ! evergreen broadleaf forest flag
    integer                                                :: NATURAL_TABLE             ! natural vegetation type
    integer                                                :: LCZ_1_TABLE               ! urban LCZ 1
    integer                                                :: LCZ_2_TABLE               ! urban LCZ 2
    integer                                                :: LCZ_3_TABLE               ! urban LCZ 3
    integer                                                :: LCZ_4_TABLE               ! urban LCZ 4
    integer                                                :: LCZ_5_TABLE               ! urban LCZ 5
    integer                                                :: LCZ_6_TABLE               ! urban LCZ 6
    integer                                                :: LCZ_7_TABLE               ! urban LCZ 7
    integer                                                :: LCZ_8_TABLE               ! urban LCZ 8
    integer                                                :: LCZ_9_TABLE               ! urban LCZ 9
    integer                                                :: LCZ_10_TABLE              ! urban LCZ 10
    integer                                                :: LCZ_11_TABLE              ! urban LCZ 11
    real(kind=kind_noahmp), allocatable, dimension(:)      :: CH2OP_TABLE               ! maximum intercepted h2o per unit lai+sai (mm)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: DLEAF_TABLE               ! characteristic leaf dimension (m)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: Z0MVT_TABLE               ! momentum roughness length (m)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: HVT_TABLE                 ! top of canopy (m)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: HVB_TABLE                 ! bottom of canopy (m)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: DEN_TABLE                 ! tree density (no. of trunks per m2)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: RC_TABLE                  ! tree crown radius (m)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: MFSNO_TABLE               ! snowmelt curve parameter
    real(kind=kind_noahmp), allocatable, dimension(:)      :: SCFFAC_TABLE              ! snow cover factor (m) (replace original hard-coded 2.5*z0 in SCF formulation)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: CBIOM_TABLE               ! canopy biomass heat capacity parameter (m) 
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: SAIM_TABLE                ! monthly stem area index, one-sided
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: LAIM_TABLE                ! monthly leaf area index, one-sided
    real(kind=kind_noahmp), allocatable, dimension(:)      :: SLA_TABLE                 ! single-side leaf area per Kg [m2/kg]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: DILEFC_TABLE              ! coeficient for leaf stress death [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: DILEFW_TABLE              ! coeficient for leaf stress death [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: FRAGR_TABLE               ! fraction of growth respiration  !original was 0.3 
    real(kind=kind_noahmp), allocatable, dimension(:)      :: LTOVRC_TABLE              ! leaf turnover [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: C3PSN_TABLE               ! photosynthetic pathway: 0. = c4, 1. = c3
    real(kind=kind_noahmp), allocatable, dimension(:)      :: KC25_TABLE                ! co2 michaelis-menten constant at 25c (pa)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: AKC_TABLE                 ! q10 for kc25
    real(kind=kind_noahmp), allocatable, dimension(:)      :: KO25_TABLE                ! o2 michaelis-menten constant at 25c (pa)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: AKO_TABLE                 ! q10 for ko25
    real(kind=kind_noahmp), allocatable, dimension(:)      :: VCMX25_TABLE              ! maximum rate of carboxylation at 25c (umol co2/m2/s)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: AVCMX_TABLE               ! q10 for vcmx25
    real(kind=kind_noahmp), allocatable, dimension(:)      :: BP_TABLE                  ! minimum leaf conductance (umol/m2/s)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: MP_TABLE                  ! slope of conductance-to-photosynthesis relationship
    real(kind=kind_noahmp), allocatable, dimension(:)      :: QE25_TABLE                ! quantum efficiency at 25c (umol co2 / umol photon)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: AQE_TABLE                 ! q10 for qe25
    real(kind=kind_noahmp), allocatable, dimension(:)      :: RMF25_TABLE               ! leaf maintenance respiration at 25c (umol co2/m2/s)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: RMS25_TABLE               ! stem maintenance respiration at 25c (umol co2/kg bio/s)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: RMR25_TABLE               ! root maintenance respiration at 25c (umol co2/kg bio/s)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: ARM_TABLE                 ! q10 for maintenance respiration
    real(kind=kind_noahmp), allocatable, dimension(:)      :: FOLNMX_TABLE              ! foliage nitrogen concentration when f(n)=1 (%)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: TMIN_TABLE                ! minimum temperature for photosynthesis (k)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: XL_TABLE                  ! leaf/stem orientation index
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: RHOL_TABLE                ! leaf reflectance: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: RHOS_TABLE                ! stem reflectance: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: TAUL_TABLE                ! leaf transmittance: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: TAUS_TABLE                ! stem transmittance: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:)      :: MRP_TABLE                 ! microbial respiration parameter (umol co2 /kg c/ s)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: CWPVT_TABLE               ! empirical canopy wind parameter
    real(kind=kind_noahmp), allocatable, dimension(:)      :: WRRAT_TABLE               ! wood to non-wood ratio
    real(kind=kind_noahmp), allocatable, dimension(:)      :: WDPOOL_TABLE              ! wood pool (switch 1 or 0) depending on woody or not [-]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: TDLEF_TABLE               ! characteristic T for leaf freezing [K]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: NROOT_TABLE               ! number of soil layers with root present
    real(kind=kind_noahmp), allocatable, dimension(:)      :: RGL_TABLE                 ! Parameter used in radiation stress function
    real(kind=kind_noahmp), allocatable, dimension(:)      :: RS_TABLE                  ! Minimum stomatal resistance [s m-1]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: HS_TABLE                  ! Parameter used in vapor pressure deficit function
    real(kind=kind_noahmp), allocatable, dimension(:)      :: TOPT_TABLE                ! Optimum transpiration air temperature [K]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: RSMAX_TABLE               ! Maximal stomatal resistance [s m-1]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: RTOVRC_TABLE              ! root turnover coefficient [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: RSWOODC_TABLE             ! wood respiration coeficient [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: BF_TABLE                  ! parameter for present wood allocation [-]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: WSTRC_TABLE               ! water stress coeficient [-]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: LAIMIN_TABLE              ! minimum leaf area index [m2/m2]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: XSAMIN_TABLE              ! minimum stem area index [m2/m2]

    ! radiation parameters
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: ALBSAT_TABLE              ! saturated soil albedos: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: ALBDRY_TABLE              ! dry soil albedos: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:)      :: ALBICE_TABLE              ! albedo land ice: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:)      :: ALBLAK_TABLE              ! albedo frozen lakes: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:)      :: OMEGAS_TABLE              ! two-stream parameter omega for snow
    real(kind=kind_noahmp)                                 :: BETADS_TABLE              ! two-stream parameter betad for snow
    real(kind=kind_noahmp)                                 :: BETAIS_TABLE              ! two-stream parameter betad for snow
    real(kind=kind_noahmp), allocatable, dimension(:)      :: EG_TABLE                  ! emissivity soil surface
    real(kind=kind_noahmp)                                 :: EICE_TABLE                ! ice surface emissivity

    ! global parameters
    real(kind=kind_noahmp)                                 :: CO2_TABLE                 ! co2 partial pressure
    real(kind=kind_noahmp)                                 :: O2_TABLE                  ! o2 partial pressure
    real(kind=kind_noahmp)                                 :: TIMEAN_TABLE              ! gridcell mean topgraphic index (global mean)
    real(kind=kind_noahmp)                                 :: FSATMX_TABLE              ! maximum surface saturated fraction (global mean)
    real(kind=kind_noahmp)                                 :: Z0SNO_TABLE               ! snow surface roughness length (m) (0.002)
    real(kind=kind_noahmp)                                 :: SSI_TABLE                 ! liquid water holding capacity for snowpack (m3/m3) (0.03)
    real(kind=kind_noahmp)                                 :: SNOW_RET_FAC_TABLE        ! snowpack water release timescale factor (1/s)
    real(kind=kind_noahmp)                                 :: SNOW_EMIS_TABLE           ! snow emissivity
    real(kind=kind_noahmp)                                 :: SWEMX_TABLE               ! new snow mass to fully cover old snow (mm)
    real(kind=kind_noahmp)                                 :: TAU0_TABLE                ! tau0 from Yang97 eqn. 10a
    real(kind=kind_noahmp)                                 :: GRAIN_GROWTH_TABLE        ! growth from vapor diffusion Yang97 eqn. 10b
    real(kind=kind_noahmp)                                 :: EXTRA_GROWTH_TABLE        ! extra growth near freezing Yang97 eqn. 10c
    real(kind=kind_noahmp)                                 :: DIRT_SOOT_TABLE           ! dirt and soot term Yang97 eqn. 10d
    real(kind=kind_noahmp)                                 :: BATS_COSZ_TABLE           ! zenith angle snow albedo adjustment; b in Yang97 eqn. 15
    real(kind=kind_noahmp)                                 :: BATS_VIS_NEW_TABLE        ! new snow visible albedo
    real(kind=kind_noahmp)                                 :: BATS_NIR_NEW_TABLE        ! new snow NIR albedo
    real(kind=kind_noahmp)                                 :: BATS_VIS_AGE_TABLE        ! age factor for diffuse visible snow albedo Yang97 eqn. 17
    real(kind=kind_noahmp)                                 :: BATS_NIR_AGE_TABLE        ! age factor for diffuse NIR snow albedo Yang97 eqn. 18
    real(kind=kind_noahmp)                                 :: BATS_VIS_DIR_TABLE        ! cosz factor for direct visible snow albedo Yang97 eqn. 15
    real(kind=kind_noahmp)                                 :: BATS_NIR_DIR_TABLE        ! cosz factor for direct NIR snow albedo Yang97 eqn. 16
    real(kind=kind_noahmp)                                 :: RSURF_SNOW_TABLE          ! surface resistance for snow(s/m)
    real(kind=kind_noahmp)                                 :: RSURF_EXP_TABLE           ! exponent in the shape parameter for soil resistance option 1
    real(kind=kind_noahmp)                                 :: C2_SNOWCOMPACT_TABLE      ! overburden snow compaction parameter (m3/kg)
    real(kind=kind_noahmp)                                 :: C3_SNOWCOMPACT_TABLE      ! snow desctructive metamorphism compaction parameter1 [1/s]
    real(kind=kind_noahmp)                                 :: C4_SNOWCOMPACT_TABLE      ! snow desctructive metamorphism compaction parameter2 [1/k]
    real(kind=kind_noahmp)                                 :: C5_SNOWCOMPACT_TABLE      ! snow desctructive metamorphism compaction parameter3
    real(kind=kind_noahmp)                                 :: DM_SNOWCOMPACT_TABLE      ! upper Limit on destructive metamorphism compaction [kg/m3]
    real(kind=kind_noahmp)                                 :: ETA0_SNOWCOMPACT_TABLE    ! snow viscosity coefficient [kg-s/m2]
    real(kind=kind_noahmp)                                 :: SNLIQMAXFRAC_TABLE        ! maximum liquid water fraction in snow
    real(kind=kind_noahmp)                                 :: SWEMAXGLA_TABLE           ! Maximum SWE allowed at glaciers (mm)
    real(kind=kind_noahmp)                                 :: WSLMAX_TABLE              ! maximum lake water storage (mm)
    real(kind=kind_noahmp)                                 :: ROUS_TABLE                ! specific yield [-] for Niu et al. 2007 groundwater scheme
    real(kind=kind_noahmp)                                 :: CMIC_TABLE                ! microprore content (0.0-1.0), 0.0: close to free drainage
    real(kind=kind_noahmp)                                 :: SNOWDEN_MAX_TABLE         ! maximum fresh snowfall density (kg/m3)
    real(kind=kind_noahmp)                                 :: CLASS_ALB_REF_TABLE       ! reference snow albedo in CLASS scheme
    real(kind=kind_noahmp)                                 :: CLASS_SNO_AGE_TABLE       ! snow aging e-folding time (s) in CLASS albedo scheme
    real(kind=kind_noahmp)                                 :: CLASS_ALB_NEW_TABLE       ! fresh snow albedo in CLASS scheme
    real(kind=kind_noahmp)                                 :: PSIWLT_TABLE              ! soil metric potential for wilting point (m)
    real(kind=kind_noahmp)                                 :: Z0SOIL_TABLE              ! Bare-soil roughness length (m) (i.e., under the canopy)
    real(kind=kind_noahmp)                                 :: Z0LAKE_TABLE              ! Lake surface roughness length (m)

    ! irrigation parameters
    integer                                                :: IRR_HAR_TABLE             ! number of days before harvest date to stop irrigation 
    real(kind=kind_noahmp)                                 :: IRR_FRAC_TABLE            ! irrigation Fraction
    real(kind=kind_noahmp)                                 :: IRR_LAI_TABLE             ! Minimum lai to trigger irrigation
    real(kind=kind_noahmp)                                 :: IRR_MAD_TABLE             ! management allowable deficit (0-1)
    real(kind=kind_noahmp)                                 :: FILOSS_TABLE              ! factor of flood irrigation loss
    real(kind=kind_noahmp)                                 :: SPRIR_RATE_TABLE          ! mm/h, sprinkler irrigation rate
    real(kind=kind_noahmp)                                 :: MICIR_RATE_TABLE          ! mm/h, micro irrigation rate
    real(kind=kind_noahmp)                                 :: FIRTFAC_TABLE             ! flood application rate factor
    real(kind=kind_noahmp)                                 :: IR_RAIN_TABLE             ! maximum precipitation to stop irrigation trigger

    ! tile drainage parameters
    integer                                                :: DRAIN_LAYER_OPT_TABLE     ! tile drainage layer
    integer               , allocatable, dimension(:)      :: TD_DEPTH_TABLE            ! tile drainage depth (layer number) from soil surface
    real(kind=kind_noahmp), allocatable, dimension(:)      :: TDSMC_FAC_TABLE           ! tile drainage soil moisture factor
    real(kind=kind_noahmp), allocatable, dimension(:)      :: TD_DC_TABLE               ! tile drainage coefficient [mm/d]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: TD_DCOEF_TABLE            ! tile drainage coefficient [mm/d]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: TD_D_TABLE                ! depth to impervious layer from drain water level [m]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: TD_ADEPTH_TABLE           ! actual depth of impervious layer from land surface [m]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: TD_RADI_TABLE             ! effective radius of drain tubes [m]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: TD_SPAC_TABLE             ! distance between two drain tubes or tiles [m]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: TD_DDRAIN_TABLE           ! tile drainage depth [m]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: KLAT_FAC_TABLE            ! hydraulic conductivity mutiplification factor

    ! crop parameters
    integer                                                :: DEFAULT_CROP_TABLE        ! Default crop index
    integer               , allocatable, dimension(:)      :: PLTDAY_TABLE              ! Planting date
    integer               , allocatable, dimension(:)      :: HSDAY_TABLE               ! Harvest date
    real(kind=kind_noahmp), allocatable, dimension(:)      :: PLANTPOP_TABLE            ! Plant density [per ha] - used?
    real(kind=kind_noahmp), allocatable, dimension(:)      :: IRRI_TABLE                ! Irrigation strategy 0= non-irrigation 1=irrigation (no water-stress)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: GDDTBASE_TABLE            ! Base temperature for GDD accumulation [C]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: GDDTCUT_TABLE             ! Upper temperature for GDD accumulation [C]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: GDDS1_TABLE               ! GDD from seeding to emergence
    real(kind=kind_noahmp), allocatable, dimension(:)      :: GDDS2_TABLE               ! GDD from seeding to initial vegetative 
    real(kind=kind_noahmp), allocatable, dimension(:)      :: GDDS3_TABLE               ! GDD from seeding to post vegetative 
    real(kind=kind_noahmp), allocatable, dimension(:)      :: GDDS4_TABLE               ! GDD from seeding to intial reproductive
    real(kind=kind_noahmp), allocatable, dimension(:)      :: GDDS5_TABLE               ! GDD from seeding to pysical maturity 
    real(kind=kind_noahmp), allocatable, dimension(:)      :: C3PSNI_TABLE              ! photosynthetic pathway: 0. = c4, 1. = c3 ! Zhe Zhang 2020-07-03
    real(kind=kind_noahmp), allocatable, dimension(:)      :: KC25I_TABLE               ! co2 michaelis-menten constant at 25c (pa)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: AKCI_TABLE                ! q10 for kc25
    real(kind=kind_noahmp), allocatable, dimension(:)      :: KO25I_TABLE               ! o2 michaelis-menten constant at 25c (pa)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: AKOI_TABLE                ! q10 for ko25
    real(kind=kind_noahmp), allocatable, dimension(:)      :: VCMX25I_TABLE             ! maximum rate of carboxylation at 25c (umol co2/m2/s)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: AVCMXI_TABLE              ! q10 for vcmx25
    real(kind=kind_noahmp), allocatable, dimension(:)      :: BPI_TABLE                 ! minimum leaf conductance (umol/m2/s)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: MPI_TABLE                 ! slope of conductance-to-photosynthesis relationship
    real(kind=kind_noahmp), allocatable, dimension(:)      :: QE25I_TABLE               ! quantum efficiency at 25c (umol co2 / umol photon)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: FOLNMXI_TABLE             ! foliage nitrogen concentration when f(n)=1 (%)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: AREF_TABLE                ! reference maximum CO2 assimulation rate 
    real(kind=kind_noahmp), allocatable, dimension(:)      :: PSNRF_TABLE               ! CO2 assimulation reduction factor(0-1) (caused by non-modeled part, pest,weeds)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: I2PAR_TABLE               ! Fraction of incoming solar radiation to photosynthetically active radiation
    real(kind=kind_noahmp), allocatable, dimension(:)      :: TASSIM0_TABLE             ! Minimum temperature for CO2 assimulation [C]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: TASSIM1_TABLE             ! CO2 assimulation linearly increasing until temperature reaches T1 [C]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: TASSIM2_TABLE             ! CO2 assmilation rate remain at Aref until temperature reaches T2 [C]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: K_TABLE                   ! light extinction coefficient
    real(kind=kind_noahmp), allocatable, dimension(:)      :: EPSI_TABLE                ! initial light use efficiency
    real(kind=kind_noahmp), allocatable, dimension(:)      :: Q10MR_TABLE               ! q10 for maintainance respiration
    real(kind=kind_noahmp), allocatable, dimension(:)      :: LEFREEZ_TABLE             ! characteristic T for leaf freezing [K]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: DILE_FC_TABLE             ! coeficient for temperature leaf stress death [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: DILE_FW_TABLE             ! coeficient for water leaf stress death [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: FRA_GR_TABLE              ! fraction of growth respiration
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: LF_OVRC_TABLE             ! fraction of leaf turnover  [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: ST_OVRC_TABLE             ! fraction of stem turnover  [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: RT_OVRC_TABLE             ! fraction of root tunrover  [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: LFMR25_TABLE              ! leaf maintenance respiration at 25C [umol CO2/m2/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: STMR25_TABLE              ! stem maintenance respiration at 25C [umol CO2/kg bio/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: RTMR25_TABLE              ! root maintenance respiration at 25C [umol CO2/kg bio/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: GRAINMR25_TABLE           ! grain maintenance respiration at 25C [umol CO2/kg bio/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: LFPT_TABLE                ! fraction of carbohydrate flux to leaf
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: STPT_TABLE                ! fraction of carbohydrate flux to stem
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: RTPT_TABLE                ! fraction of carbohydrate flux to root
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: GRAINPT_TABLE             ! fraction of carbohydrate flux to grain
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: LFCT_TABLE                ! fraction of carbohydrate translocation from leaf to grain 
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: STCT_TABLE                ! fraction of carbohydrate translocation from stem to grain
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: RTCT_TABLE                ! fraction of carbohydrate translocation from root to grain
    real(kind=kind_noahmp), allocatable, dimension(:)      :: BIO2LAI_TABLE             ! leaf area per living leaf biomass [m2/kg]

    ! soil parameters
    integer                                                :: SLCATS_TABLE              ! number of soil categories
    real(kind=kind_noahmp), allocatable, dimension(:)      :: BEXP_TABLE                ! soil B parameter
    real(kind=kind_noahmp), allocatable, dimension(:)      :: SMCDRY_TABLE              ! dry soil moisture threshold
    real(kind=kind_noahmp), allocatable, dimension(:)      :: SMCMAX_TABLE              ! porosity, saturated value of soil moisture (volumetric)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: SMCREF_TABLE              ! reference soil moisture (field capacity) (volumetric)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: PSISAT_TABLE              ! saturated soil matric potential
    real(kind=kind_noahmp), allocatable, dimension(:)      :: DKSAT_TABLE               ! saturated soil hydraulic conductivity
    real(kind=kind_noahmp), allocatable, dimension(:)      :: DWSAT_TABLE               ! saturated soil hydraulic diffusivity
    real(kind=kind_noahmp), allocatable, dimension(:)      :: SMCWLT_TABLE              ! wilting point soil moisture (volumetric)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: QUARTZ_TABLE              ! soil quartz content
    real(kind=kind_noahmp), allocatable, dimension(:)      :: BVIC_TABLE                ! VIC model infiltration parameter (-) for opt_run=6
    real(kind=kind_noahmp), allocatable, dimension(:)      :: AXAJ_TABLE                ! Xinanjiang: Tension water distribution inflection parameter [-] for opt_run=7
    real(kind=kind_noahmp), allocatable, dimension(:)      :: BXAJ_TABLE                ! Xinanjiang: Tension water distribution shape parameter [-] for opt_run=7
    real(kind=kind_noahmp), allocatable, dimension(:)      :: XXAJ_TABLE                ! Xinanjiang: Free water distribution shape parameter [-] for opt_run=7
    real(kind=kind_noahmp), allocatable, dimension(:)      :: BDVIC_TABLE               ! VIC model infiltration parameter (-)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: GDVIC_TABLE               ! mean capilary drive (m)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: BBVIC_TABLE               ! heterogeniety parameter for DVIC infiltration [-]

    ! general parameters
    real(kind=kind_noahmp), allocatable, dimension(:)      :: SLOPE_TABLE               ! slope factor for soil drainage
    real(kind=kind_noahmp)                                 :: CSOIL_TABLE               ! Soil heat capacity [J m-3 K-1]
    real(kind=kind_noahmp)                                 :: REFDK_TABLE               ! Parameter in the surface runoff parameterization
    real(kind=kind_noahmp)                                 :: REFKDT_TABLE              ! Parameter in the surface runoff parameterization
    real(kind=kind_noahmp)                                 :: FRZK_TABLE                ! Frozen ground parameter
    real(kind=kind_noahmp)                                 :: ZBOT_TABLE                ! Depth [m] of lower boundary soil temperature
    real(kind=kind_noahmp)                                 :: CZIL_TABLE                ! Parameter used in the calculation of the roughness length for heat

    ! optional parameters
    real(kind=kind_noahmp)                                 :: sr2006_theta_1500t_a_TABLE      ! sand coefficient
    real(kind=kind_noahmp)                                 :: sr2006_theta_1500t_b_TABLE      ! clay coefficient
    real(kind=kind_noahmp)                                 :: sr2006_theta_1500t_c_TABLE      ! orgm coefficient
    real(kind=kind_noahmp)                                 :: sr2006_theta_1500t_d_TABLE      ! sand*orgm coefficient
    real(kind=kind_noahmp)                                 :: sr2006_theta_1500t_e_TABLE      ! clay*orgm coefficient
    real(kind=kind_noahmp)                                 :: sr2006_theta_1500t_f_TABLE      ! sand*clay coefficient
    real(kind=kind_noahmp)                                 :: sr2006_theta_1500t_g_TABLE      ! constant adjustment
    real(kind=kind_noahmp)                                 :: sr2006_theta_1500_a_TABLE       ! theta_1500t coefficient
    real(kind=kind_noahmp)                                 :: sr2006_theta_1500_b_TABLE       ! constant adjustment
    real(kind=kind_noahmp)                                 :: sr2006_theta_33t_a_TABLE        ! sand coefficient
    real(kind=kind_noahmp)                                 :: sr2006_theta_33t_b_TABLE        ! clay coefficient
    real(kind=kind_noahmp)                                 :: sr2006_theta_33t_c_TABLE        ! orgm coefficient
    real(kind=kind_noahmp)                                 :: sr2006_theta_33t_d_TABLE        ! sand*orgm coefficient
    real(kind=kind_noahmp)                                 :: sr2006_theta_33t_e_TABLE        ! clay*orgm coefficient
    real(kind=kind_noahmp)                                 :: sr2006_theta_33t_f_TABLE        ! sand*clay coefficient
    real(kind=kind_noahmp)                                 :: sr2006_theta_33t_g_TABLE        ! constant adjustment
    real(kind=kind_noahmp)                                 :: sr2006_theta_33_a_TABLE         ! theta_33t*theta_33t coefficient
    real(kind=kind_noahmp)                                 :: sr2006_theta_33_b_TABLE         ! theta_33t coefficient
    real(kind=kind_noahmp)                                 :: sr2006_theta_33_c_TABLE         ! constant adjustment
    real(kind=kind_noahmp)                                 :: sr2006_theta_s33t_a_TABLE       ! sand coefficient
    real(kind=kind_noahmp)                                 :: sr2006_theta_s33t_b_TABLE       ! clay coefficient
    real(kind=kind_noahmp)                                 :: sr2006_theta_s33t_c_TABLE       ! orgm coefficient
    real(kind=kind_noahmp)                                 :: sr2006_theta_s33t_d_TABLE       ! sand*orgm coefficient
    real(kind=kind_noahmp)                                 :: sr2006_theta_s33t_e_TABLE       ! clay*orgm coefficient
    real(kind=kind_noahmp)                                 :: sr2006_theta_s33t_f_TABLE       ! sand*clay coefficient
    real(kind=kind_noahmp)                                 :: sr2006_theta_s33t_g_TABLE       ! constant adjustment
    real(kind=kind_noahmp)                                 :: sr2006_theta_s33_a_TABLE        ! theta_s33t coefficient
    real(kind=kind_noahmp)                                 :: sr2006_theta_s33_b_TABLE        ! constant adjustment
    real(kind=kind_noahmp)                                 :: sr2006_psi_et_a_TABLE           ! sand coefficient
    real(kind=kind_noahmp)                                 :: sr2006_psi_et_b_TABLE           ! clay coefficient
    real(kind=kind_noahmp)                                 :: sr2006_psi_et_c_TABLE           ! theta_s33 coefficient
    real(kind=kind_noahmp)                                 :: sr2006_psi_et_d_TABLE           ! sand*theta_s33 coefficient
    real(kind=kind_noahmp)                                 :: sr2006_psi_et_e_TABLE           ! clay*theta_s33 coefficient
    real(kind=kind_noahmp)                                 :: sr2006_psi_et_f_TABLE           ! sand*clay coefficient
    real(kind=kind_noahmp)                                 :: sr2006_psi_et_g_TABLE           ! constant adjustment
    real(kind=kind_noahmp)                                 :: sr2006_psi_e_a_TABLE            ! psi_et*psi_et coefficient
    real(kind=kind_noahmp)                                 :: sr2006_psi_e_b_TABLE            ! psi_et coefficient
    real(kind=kind_noahmp)                                 :: sr2006_psi_e_c_TABLE            ! constant adjustment
    real(kind=kind_noahmp)                                 :: sr2006_smcmax_a_TABLE           ! sand adjustment
    real(kind=kind_noahmp)                                 :: sr2006_smcmax_b_TABLE           ! constant adjustment

  end type NoahmpIO_type

end module NoahmpIOVarType
