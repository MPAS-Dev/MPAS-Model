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
    integer                                                ::  its,ite, &          ! t -> tile
                                                               kts,kte             ! t -> tile
    integer                                                ::  itimestep           ! timestep number
    integer                                                ::  yr                  ! 4-digit year
    integer                                                ::  month               ! 2-digit month
    integer                                                ::  day                 ! 2-digit day
    integer                                                ::  nsoil               ! number of soil layers
    integer                                                ::  ice                 ! sea-ice point
    integer                                                ::  isice               ! land cover category for ice
    integer                                                ::  isurban             ! land cover category for urban
    integer                                                ::  iswater             ! land cover category for water
    integer                                                ::  islake              ! land cover category for lake
    integer                                                ::  urbtype_beg         ! urban type start number - 1
    integer                                                ::  iopt_dveg           ! dynamic vegetation
    integer                                                ::  iopt_crs            ! canopy stomatal resistance (1-> Ball-Berry; 2->Jarvis)
    integer                                                ::  iopt_btr            ! soil moisture factor for stomatal resistance (1-> Noah; 2-> CLM; 3-> SSiB)
    integer                                                ::  iopt_runsrf         ! surface runoff and groundwater (1->SIMGM; 2->SIMTOP; 3->Schaake96; 4->BATS)
    integer                                                ::  iopt_runsub         ! subsurface runoff option
    integer                                                ::  iopt_sfc            ! surface layer drag coeff (CH & CM) (1->M-O; 2->Chen97)
    integer                                                ::  iopt_frz            ! supercooled liquid water (1-> NY06; 2->Koren99)
    integer                                                ::  iopt_inf            ! frozen soil permeability (1-> NY06; 2->Koren99)
    integer                                                ::  iopt_rad            ! radiation transfer (1->gap=F(3D,cosz); 2->gap=0; 3->gap=1-Fveg)
    integer                                                ::  iopt_alb            ! snow surface albedo (1->BATS; 2->CLASS)
    integer                                                ::  iopt_snf            ! rainfall & snowfall (1-Jordan91; 2->BATS; 3->Noah)
    integer                                                ::  iopt_tksno          ! snow thermal conductivity: 1 -> Stieglitz(yen,1965) scheme (default), 2 -> Anderson, 1976 scheme, 3 -> constant, 4 -> Verseghy (1991) scheme, 5 -> Douvill(Yen, 1981) scheme
    integer                                                ::  iopt_tbot           ! lower boundary of soil temperature (1->zero-flux; 2->Noah)
    integer                                                ::  iopt_stc            ! snow/soil temperature time scheme
    integer                                                ::  iopt_gla            ! glacier option (1->phase change; 2->simple)
    integer                                                ::  iopt_rsf            ! surface resistance option (1->Zeng; 2->simple)
    integer                                                ::  iz0tlnd             ! option of Chen adjustment of Czil (not used)
    integer                                                ::  iopt_soil           ! soil configuration option
    integer                                                ::  iopt_pedo           ! soil pedotransfer function option
    integer                                                ::  iopt_crop           ! crop model option (0->none; 1->Liu et al.)
    integer                                                ::  iopt_irr            ! irrigation scheme (0->none; >1 irrigation scheme ON)
    integer                                                ::  iopt_irrm           ! irrigation method (0->dynamic; 1-> sprinkler; 2-> micro; 3-> flood)
    integer                                                ::  iopt_infdv          ! infiltration options for dynamic VIC (1->Philip; 2-> Green-Ampt;3->Smith-Parlange)
    integer                                                ::  iopt_tdrn           ! drainage option (0->off; 1->simple scheme; 2->Hooghoudt's scheme)
    real(kind=kind_noahmp)                                 ::  xice_threshold      ! fraction of grid determining seaice
    real(kind=kind_noahmp)                                 ::  julian              ! julian day
    real(kind=kind_noahmp)                                 ::  dtbl                ! timestep [s]
    real(kind=kind_noahmp)                                 ::  dx                  ! horizontal grid spacing [m]
    real(kind=kind_noahmp)                                 ::  soiltstep           ! soil time step (s) (default=0: same as main NoahMP timstep)
    logical                                                ::  fndsnowh            ! snow depth present in input
    logical                                                ::  calculate_soil      ! logical index for if do soil calculation
    integer                                                ::  soil_update_steps   ! number of model time steps to update soil process
    integer,                allocatable, dimension(:)      ::  ivgtyp              ! vegetation type
    integer,                allocatable, dimension(:)      ::  isltyp              ! soil type
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  coszen              ! cosine zenith angle
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  xlat                ! latitude [rad]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  dz8w                ! thickness of atmo layers [m]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  dzs                 ! thickness of soil layers [m]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  zsoil               ! depth to soil interfaces [m]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  vegfra              ! vegetation fraction []
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  tmn                 ! deep soil temperature [K]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  xland               ! =2 ocean; =1 land/seaice
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  xice                ! fraction of grid that is seaice
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  seaice              ! seaice fraction

    ! forcings    
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::    t_phy             ! 3D atmospheric temperature valid at mid-levels [K]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::    qv_curr           ! 3D water vapor mixing ratio [kg/kg_dry]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::    u_phy             ! 3D U wind component [m/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::    v_phy             ! 3D V wind component [m/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::    swdown            ! solar down at surface [W m-2]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::    glw               ! longwave down at surface [W m-2]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::    p8w               ! 3D pressure, valid at interface [Pa]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::    rainbl            ! precipitation entering land model [mm] per time step
    real(kind=kind_noahmp), allocatable, dimension(:)      ::    snowbl            ! snow entering land model [mm] per time step
    real(kind=kind_noahmp), allocatable, dimension(:)      ::    sr                ! frozen precip ratio entering land model [-]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::    raincv            ! convective precip forcing [mm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::    rainncv           ! non-convective precip forcing [mm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::    rainshv           ! shallow conv. precip forcing [mm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::    snowncv           ! non-covective snow forcing (subset of rainncv) [mm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::    graupelncv        ! non-convective graupel forcing (subset of rainncv) [mm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::    hailncv           ! non-convective hail forcing (subset of rainncv) [mm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::    mp_rainc          ! convective precipitation entering land model [mm] ! MB/AN : v3.7
    real(kind=kind_noahmp), allocatable, dimension(:)      ::    mp_rainnc         ! large-scale precipitation entering land model [mm]! MB/AN : v3.7
    real(kind=kind_noahmp), allocatable, dimension(:)      ::    mp_shcv           ! shallow conv precip entering land model [mm]    ! MB/AN : v3.7
    real(kind=kind_noahmp), allocatable, dimension(:)      ::    mp_snow           ! snow precipitation entering land model [mm]     ! MB/AN : v3.7
    real(kind=kind_noahmp), allocatable, dimension(:)      ::    mp_graup          ! graupel precipitation entering land model [mm]  ! MB/AN : v3.7
    real(kind=kind_noahmp), allocatable, dimension(:)      ::    mp_hail           ! hail precipitation entering land model [mm]     ! MB/AN : v3.7

#ifdef WRF_HYDRO
    real(kind=kind_noahmp), allocatable, dimension(:)      ::   infxsrt            ! surface infiltration
    real(kind=kind_noahmp), allocatable, dimension(:)      ::   sfcheadrt          ! surface water head
    real(kind=kind_noahmp), allocatable, dimension(:)      ::   soldrain           ! soil drainage
    real(kind=kind_noahmp), allocatable, dimension(:)      ::   qtiledrain         ! tile drainage
    real(kind=kind_noahmp), allocatable, dimension(:)      ::   zwatble2d          ! water table depth
#endif

    ! Spatially varying fields (for now it is de-activated)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  soilcomp            ! Soil sand and clay content [fraction]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  soilcl1             ! Soil texture class with depth
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  soilcl2             ! Soil texture class with depth
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  soilcl3             ! Soil texture class with depth
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  soilcl4             ! Soil texture class with depth
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  bexp_3D             ! C-H B exponent
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  smcdry_3D           ! Soil Moisture Limit: Dry
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  smcwlt_3D           ! Soil Moisture Limit: Wilt
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  smcref_3D           ! Soil Moisture Limit: Reference
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  smcmax_3D           ! Soil Moisture Limit: Max
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  dksat_3D            ! Saturated Soil Conductivity
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  dwsat_3D            ! Saturated Soil Diffusivity
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  psisat_3D           ! Saturated Matric Potential
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  quartz_3D           ! Soil quartz content
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  refdk_2D            ! Reference Soil Conductivity
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  refkdt_2D           ! Soil Infiltration Parameter
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  irr_frac_2D         ! irrigation Fraction
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  irr_har_2D          ! number of days before harvest date to stop irrigation
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  irr_lai_2D          ! Minimum lai to trigger irrigation
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  irr_mad_2D          ! management allowable deficit (0-1)
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  filoss_2D           ! fraction of flood irrigation loss (0-1)
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  sprir_rate_2D       ! mm/h, sprinkler irrigation rate
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  micir_rate_2D       ! mm/h, micro irrigation rate
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  firtfac_2D          ! flood application rate factor
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  ir_rain_2D          ! maximum precipitation to stop irrigation trigger
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  bvic_2d             ! VIC model infiltration parameter [-] opt_run=6
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  axaj_2D             ! Tension water distribution inflection parameter [-] opt_run=7
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  bxaj_2D             ! Tension water distribution shape parameter [-] opt_run=7
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  xxaj_2D             ! Free water distribution shape parameter [-] opt_run=7
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  bdvic_2d            ! VIC model infiltration parameter [-] opt_run=8
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  gdvic_2d            ! Mean Capillary Drive (m) for infiltration models opt_run=8
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  bbvic_2d            ! DVIC heterogeniety parameter for infiltration [-] opt_run=8
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  KLAT_FAC            ! factor multiplier to hydraulic conductivity
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  TDSMC_FAC           ! factor multiplier to field capacity
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  TD_DC               ! drainage coefficient for simple
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  TD_DCOEF            ! drainge coefficient for Hooghoudt
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  TD_DDRAIN           ! depth of drain
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  TD_RADI             ! tile radius
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  TD_SPAC             ! tile spacing

    ! INOUT (with generic LSM equivalent) (as defined in WRF)
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  tsk                 ! surface radiative temperature [K]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  hfx                 ! sensible heat flux [W m-2]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qfx                 ! latent heat flux [kg s-1 m-2]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  lh                  ! latent heat flux [W m-2]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  grdflx              ! ground/snow heat flux [W m-2]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  smstav              ! soil moisture avail. [not used]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  smstot              ! total soil water [mm][not used]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  sfcrunoff           ! accumulated surface runoff [m]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  udrunoff            ! accumulated sub-surface runoff [m]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  albedo              ! total grid albedo []
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  snowc               ! snow cover fraction []
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  smoiseq             ! volumetric soil moisture [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  smois               ! volumetric soil moisture [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  sh2o                ! volumetric liquid soil moisture [m3/m3]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  tslb                ! soil temperature [K]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  snow                ! snow water equivalent [mm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  snowh               ! physical snow depth [m]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  canwat              ! total canopy water + ice [mm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  acsnom              ! accumulated snow melt leaving pack
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  acsnow              ! accumulated snow on grid
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  emiss               ! surface bulk emissivity
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qsfc                ! bulk surface specific humidity

    ! INOUT (with no Noah LSM equivalent) (as defined in WRF)
    integer, allocatable, dimension(:)                     ::  isnowxy             ! actual no. of snow layers
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  tvxy                ! vegetation leaf temperature
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  tgxy                ! bulk ground surface temperature
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  canicexy            ! canopy-intercepted ice (mm)
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  canliqxy            ! canopy-intercepted liquid water (mm)
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  eahxy               ! canopy air vapor pressure (Pa)
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  tahxy               ! canopy air temperature (K)
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  cmxy                ! bulk momentum drag coefficient
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  chxy                ! bulk sensible heat exchange coefficient
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  fwetxy              ! wetted or snowed fraction of the canopy (-)
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  sneqvoxy            ! snow mass at last time step(mm h2o)
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  alboldxy            ! snow albedo at last time step (-)
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qsnowxy             ! snowfall on the ground [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qrainxy             ! rainfall on the ground [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  wslakexy            ! lake water storage [mm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  zwtxy               ! water table depth [m]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  waxy                ! water in the "aquifer" [mm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  wtxy                ! groundwater storage [mm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  smcwtdxy            ! groundwater storage [mm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  deeprechxy          ! groundwater storage [mm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  rechxy              ! groundwater storage [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  tsnoxy              ! snow temperature [K]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  zsnsoxy             ! snow layer depth [m]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  snicexy             ! snow layer ice [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  snliqxy             ! snow layer liquid water [mm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  lfmassxy            ! leaf mass [g/m2]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  rtmassxy            ! mass of fine roots [g/m2]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  stmassxy            ! stem mass [g/m2]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  woodxy              ! mass of wood (incl. woody roots) [g/m2]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  grainxy             ! xing mass of grain!three
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  gddxy               ! xinggrowingdegressday
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  stblcpxy            ! stable carbon in deep soil [g/m2]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  fastcpxy            ! short-lived carbon, shallow soil [g/m2]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  lai                 ! leaf area index
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  xsaixy              ! stem area index
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  taussxy             ! snow age factor

    ! irrigation
    real(kind=kind_noahmp), allocatable, dimension(:)      :: irfract              ! irrigation fraction
    real(kind=kind_noahmp), allocatable, dimension(:)      :: sifract              ! sprinkler irrigation fraction
    real(kind=kind_noahmp), allocatable, dimension(:)      :: mifract              ! micro irrigation fraction
    real(kind=kind_noahmp), allocatable, dimension(:)      :: fifract              ! flood irrigation fraction
    integer, allocatable, dimension(:)                     :: irnumsi              ! irrigation event number, sprinkler
    integer, allocatable, dimension(:)                     :: irnummi              ! irrigation event number, micro
    integer, allocatable, dimension(:)                     :: irnumfi              ! irrigation event number, flood
    real(kind=kind_noahmp), allocatable, dimension(:)      :: irwatsi              ! irrigation water amount [m] to be applied, sprinkler
    real(kind=kind_noahmp), allocatable, dimension(:)      :: irwatmi              ! irrigation water amount [m] to be applied, micro
    real(kind=kind_noahmp), allocatable, dimension(:)      :: irwatfi              ! irrigation water amount [m] to be applied, flood
    real(kind=kind_noahmp), allocatable, dimension(:)      :: ireloss              ! loss of irrigation water to evaporation,sprinkler [m/timestep]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: irsivol              ! amount of irrigation by sprinkler (mm)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: irmivol              ! amount of irrigation by micro (mm)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: irfivol              ! amount of irrigation by micro (mm)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: irrsplh              ! latent heating from sprinkler evaporation (W/m2)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: loctim               ! local time
 
    ! OUT (with no Noah LSM equivalent) (as defined in WRF)
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  t2mvxy              ! 2m temperature of vegetation part [K]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  t2mbxy              ! 2m temperature of bare ground part [K]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  t2mxy               ! 2m grid-mean temperature [K]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  q2mvxy              ! 2m mixing ratio of vegetation part [kg/kg]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  q2mbxy              ! 2m mixing ratio of bare ground part [kg/kg]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  q2mxy               ! 2m grid-mean mixing ratio [kg/kg]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  tradxy              ! surface radiative temperature (K)
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  neexy               ! net ecosys exchange (g/m2/s CO2)
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  gppxy               ! gross primary assimilation [g/m2/s C]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  nppxy               ! net primary productivity [g/m2/s C]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  fvegxy              ! noah-mp vegetation fraction [-]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  runsfxy             ! surface runoff [mm per soil timestep]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  runsbxy             ! subsurface runoff [mm per soil timestep]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  ecanxy              ! evaporation of intercepted water (mm/s)
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  edirxy              ! soil surface evaporation rate (mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  etranxy             ! transpiration rate (mm/s)
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  fsaxy               ! total absorbed solar radiation (W/m2)
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  firaxy              ! total net longwave rad (w/m2) [+ to atm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  aparxy              ! photosyn active energy by canopy (W/m2)
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  psnxy               ! total photosynthesis (umol co2/m2/s) [+]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  savxy               ! solar rad absorbed by veg. (W/m2)
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  sagxy               ! solar rad absorbed by ground (W/m2)
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  rssunxy             ! sunlit leaf stomatal resistance (s/m)
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  rsshaxy             ! shaded leaf stomatal resistance (s/m)
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  bgapxy              ! between gap fraction
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  wgapxy              ! within gap fraction
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  tgvxy               ! under canopy ground temperature[K]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  tgbxy               ! bare ground temperature [K]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  chvxy               ! sensible heat exchange coefficient vegetated
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  chbxy               ! sensible heat exchange coefficient bare-ground
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  shgxy               ! veg ground sen. heat [W/m2]   [+ to atm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  shcxy               ! canopy sen. heat [W/m2]   [+ to atm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  shbxy               ! bare sensible heat [W/m2]  [+ to atm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  evgxy               ! veg ground evap. heat [W/m2]  [+ to atm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  evbxy               ! bare soil evaporation [W/m2]  [+ to atm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  ghvxy               ! veg ground heat flux [W/m2]  [+ to soil]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  ghbxy               ! bare ground heat flux [W/m2] [+ to soil]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  irgxy               ! veg ground net lw rad. [W/m2] [+ to atm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  ircxy               ! canopy net lw rad. [W/m2] [+ to atm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  irbxy               ! bare net longwave rad. [W/m2] [+ to atm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  trxy                ! transpiration [W/m2]  [+ to atm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  evcxy               ! canopy evaporation heat [W/m2]  [+ to atm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  chleafxy            ! leaf exchange coefficient
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  chucxy              ! under canopy exchange coefficient
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  chv2xy              ! veg 2m exchange coefficient
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  chb2xy              ! bare 2m exchange coefficient
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  rs                  ! total stomatal resistance [s/m]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  z0                  ! roughness length output to wrf
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  znt                 ! roughness length output to wrf
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qtdrain             ! tile drain discharge [mm]

    ! additional output variables
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  pahxy               ! precipitation advected heat [W/m2]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  pahgxy              ! precipitation advected heat [W/m2]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  pahbxy              ! precipitation advected heat [W/m2]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  pahvxy              ! precipitation advected heat [W/m2]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qintsxy             ! canopy intercepted snow [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qintrxy             ! canopy intercepted rain [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qdripsxy            ! canopy dripping snow [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qdriprxy            ! canopy dripping rain [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qthrosxy            ! canopy throughfall snow [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qthrorxy            ! canopy throughfall rain [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qsnsubxy            ! snowpack sublimation rate [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qmeltxy             ! snowpack melting rate due to phase change [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qsnfroxy            ! snowpack frost rate [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qsubcxy             ! canopy snow sublimation rate [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qfrocxy             ! canopy snow frost rate [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qevacxy             ! canopy water evaporation rate [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qdewcxy             ! canopy water dew rate [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qfrzcxy             ! canopy water freezing rate [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qmeltcxy            ! canopy snow melting rate [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qsnbotxy            ! total water (melt+rain through snow) out of snowpack bottom [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  pondingxy           ! total surface ponding [mm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  fpicexy             ! fraction of ice in total precipitation
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  rainlsm             ! total rain rate at the surface [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  snowlsm             ! total snow rate at the surface [mm/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  forctlsm            ! surface temperature as lsm forcing [K]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  forcqlsm            ! surface specific humidity as lsm forcing [kg/kg]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  forcplsm            ! surface pressure as lsm forcing [Pa]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  forczlsm            ! reference height as lsm input [m]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  forcwlsm            ! surface wind speed as lsm forcing [m/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  acc_ssoilxy         ! accumulated ground heat flux [W/m2 * dt_soil/dt_main]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  acc_qinsurxy        ! accumulated water flux into soil [m/s * dt_soil/dt_main]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  acc_qsevaxy         ! accumulated soil surface evaporation [m/s * dt_soil/dt_main]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  eflxbxy             ! accumulated heat flux through soil bottom per soil timestep [J/m2]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  soilenergy          ! energy content in soil relative to 273.16 [kJ/m2]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  snowenergy          ! energy content in snow relative to 273.16 [kJ/m2]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  canhsxy             ! canopy heat storage change [W/m2]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  acc_dwaterxy        ! accumulated snow,soil,canopy water change per soil timestep [mm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  acc_prcpxy          ! accumulated precipitation per soil timestep [mm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  acc_ecanxy          ! accumulated net canopy evaporation per soil timestep [mm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  acc_etranxy         ! accumulated transpiration per soil timestep [mm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  acc_edirxy          ! accumulated net ground (soil/snow) evaporation per soil timestep [mm]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  acc_etranixy        ! accumualted transpiration rate within soil timestep [m/s * dt_soil/dt_main]

!------------------------------------------------------------------------
! Needed for MMF_RUNOFF (IOPT_RUN = 5); not part of MP driver in WRF
!------------------------------------------------------------------------

    real(kind=kind_noahmp), allocatable, dimension(:)      ::  msftx               ! mapping factor x
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  msfty               ! mapping factor y
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  eqzwt               ! equilibrium water table
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  riverbedxy          ! riverbed depth
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  rivercondxy         ! river conductivity
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  pexpxy              ! exponential factor
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  fdepthxy            ! depth
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  areaxy              ! river area
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qrfsxy              ! accumulated groundwater baseflow [mm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qspringsxy          ! accumulated seeping water [mm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qrfxy               ! groundwater baselow [m]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qspringxy           ! seeping water [m]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qslatxy             ! accumulated lateral flow [mm]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qlatxy              ! lateral flow [m]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  rechclim            ! climatology recharge
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  rivermask           ! river mask
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  nonriverxy          ! non-river portion
    real(kind=kind_noahmp)                                 ::  wtddt  = 30.0       ! frequency of groundwater call [minutes]
    integer                                                ::  stepwtd             ! step of groundwater call

!------------------------------------------------------------------------
! Needed for TILE DRAINAGE IF IOPT_TDRN = 1 OR 2
!------------------------------------------------------------------------
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  td_fraction         ! tile drainage fraction

!------------------------------------------------------------------------
! Needed for crop model (OPT_CROP=1)
!------------------------------------------------------------------------

    integer, allocatable, dimension(:)                     :: pgsxy                ! plant growth stage
    integer, allocatable, dimension(:)                     :: cropcat              ! crop category
    real(kind=kind_noahmp), allocatable, dimension(:)      :: planting             ! planting day
    real(kind=kind_noahmp), allocatable, dimension(:)      :: harvest              ! harvest day
    real(kind=kind_noahmp), allocatable, dimension(:)      :: season_gdd           ! seasonal gdd
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: croptype             ! crop type

!------------------------------------------------------------------------
! Single- and Multi-layer Urban Models
!------------------------------------------------------------------------

    integer                                                ::  num_urban_atmosphere ! atmospheric levels including ZLVL for BEP/BEM models
    integer                                                ::  iri_urban            ! urban irrigation flag (move from module_sf_urban to here)
    real(kind=kind_noahmp)                                 ::  gmt                  ! hour of day (fractional) (needed for urban)
    integer                                                ::  julday               ! integer day (needed for urban)
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  hrang                ! hour angle (needed for urban)
    real(kind=kind_noahmp)                                 ::  declin               ! declination (needed for urban)
    integer                                                ::  num_roof_layers = 4  ! roof layer number
    integer                                                ::  num_road_layers = 4  ! road layer number
    integer                                                ::  num_wall_layers = 4  ! wall layer number
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  cmr_sfcdif
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  chr_sfcdif
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  cmc_sfcdif
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  chc_sfcdif
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  cmgr_sfcdif
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  chgr_sfcdif
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  tr_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  tb_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  tg_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  tc_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qc_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  uc_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  xxxr_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  xxxb_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  xxxg_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  xxxc_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  trl_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  tbl_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  tgl_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  sh_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  lh_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  g_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  rn_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  ts_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  psim_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  psih_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  u10_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  v10_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  gz1oz0_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  akms_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  th2_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  q2_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  ust_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  dzr
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  dzb
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  dzg
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  cmcr_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  tgr_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  tgrl_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  smr_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  drelr_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  drelb_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  drelg_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  flxhumr_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  flxhumb_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  flxhumg_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  frc_urb2d
    integer, allocatable, dimension(:)                     ::  utype_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  chs
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  chs2
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  cqs2
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  trb_urb4d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  tw1_urb4d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  tw2_urb4d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  tgb_urb4d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  tlev_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  qlev_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  tw1lev_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  tw2lev_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  tglev_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  tflev_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  sf_ac_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  lf_ac_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  cm_ac_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  sfvent_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  lfvent_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  sfwin1_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  sfwin2_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  sfw1_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  sfw2_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  sfr_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  sfg_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  lp_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  hi_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  lb_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  hgt_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  mh_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  stdh_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  lf_urb2d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  theta_urban
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  u_urban
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  v_urban
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  dz_urban
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  rho_urban
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  p_urban
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  ust
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  a_u_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  a_v_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  a_t_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  a_q_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  a_e_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  b_u_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  b_v_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  b_t_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  b_q_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  b_e_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  dlg_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  dl_u_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  sf_bep
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  vl_bep
    real(kind=kind_noahmp)                                 ::  height_urban

    ! new urban variables for green roof, PVP for BEP_BEM scheme=3, Zonato et al., 2021
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  ep_pv_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  qgr_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  tgr_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  draingr_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  t_pv_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  trv_urb4d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  qr_urb4d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  drain_urb4d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  sfrv_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  lfrv_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  dgr_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  dg_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  lfr_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:,:)    ::  lfg_urb3d
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  swddir              ! solar down at surface [w m-2]
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  swddif

!------------------------------------------------------------------------
! 2D variables not used in WRF - should be removed?
!------------------------------------------------------------------------

    real(kind=kind_noahmp), allocatable, dimension(:)      ::  xlong               ! longitude
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  terrain             ! terrain height
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  gvfmin              ! annual minimum in vegetation fraction
    real(kind=kind_noahmp), allocatable, dimension(:)      ::  gvfmax              ! annual maximum in vegetation fraction

!------------------------------------------------------------------------
! End 2D variables not used in WRF
!------------------------------------------------------------------------

    CHARACTER(LEN=256)                                     ::  mminsl  = 'STAS'    ! soil classification
    CHARACTER(LEN=256)                                     ::  llanduse            ! (=USGS, using USGS landuse classification)

!------------------------------------------------------------------------
! Timing:
!------------------------------------------------------------------------

    integer                                                ::  ntime               ! timesteps
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

    integer                                                ::  i
    integer                                                ::  j
    integer                                                ::  slopetyp
    integer                                                ::  yearlen
    integer                                                ::  nsnow
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

    integer                                                ::  ix
    integer                                                ::  jx
    real(kind=kind_noahmp)                                 ::  dy
    real(kind=kind_noahmp)                                 ::  truelat1
    real(kind=kind_noahmp)                                 ::  truelat2
    real(kind=kind_noahmp)                                 ::  cen_lon
    integer                                                ::  mapproj
    real(kind=kind_noahmp)                                 ::  lat1
    real(kind=kind_noahmp)                                 ::  lon1

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
    integer                                                ::  MAX_SOIL_LEVELS
    real(kind=kind_noahmp),  allocatable, dimension(:)     ::  soil_thick_input

!----------------------------------------------------------------
! Noahmp Parameters Table 
!----------------------------------------------------------------

    ! vegetation parameters
    character(len=256)                                     :: veg_dataset_description_table
    integer                                                :: nveg_table                ! number of vegetation types
    integer                                                :: isurban_table             ! urban flag
    integer                                                :: iswater_table             ! water flag
    integer                                                :: isbarren_table            ! barren ground flag
    integer                                                :: isice_table               ! ice flag
    integer                                                :: iscrop_table              ! cropland flag
    integer                                                :: eblforest_table           ! evergreen broadleaf forest flag
    integer                                                :: natural_table             ! natural vegetation type
    integer                                                :: lcz_1_table               ! urban lcz 1
    integer                                                :: lcz_2_table               ! urban lcz 2
    integer                                                :: lcz_3_table               ! urban lcz 3
    integer                                                :: lcz_4_table               ! urban lcz 4
    integer                                                :: lcz_5_table               ! urban lcz 5
    integer                                                :: lcz_6_table               ! urban lcz 6
    integer                                                :: lcz_7_table               ! urban lcz 7
    integer                                                :: lcz_8_table               ! urban lcz 8
    integer                                                :: lcz_9_table               ! urban lcz 9
    integer                                                :: lcz_10_table              ! urban lcz 10
    integer                                                :: lcz_11_table              ! urban lcz 11
    real(kind=kind_noahmp), allocatable, dimension(:)      :: ch2op_table               ! maximum intercepted h2o per unit lai+sai (mm)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: dleaf_table               ! characteristic leaf dimension (m)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: z0mvt_table               ! momentum roughness length (m)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: hvt_table                 ! top of canopy (m)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: hvb_table                 ! bottom of canopy (m)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: den_table                 ! tree density (no. of trunks per m2)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: rc_table                  ! tree crown radius (m)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: mfsno_table               ! snowmelt curve parameter
    real(kind=kind_noahmp), allocatable, dimension(:)      :: scffac_table              ! snow cover factor (m) (replace original hard-coded 2.5*z0 in SCF formulation)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: cbiom_table               ! canopy biomass heat capacity parameter (m)
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: saim_table                ! monthly stem area index, one-sided
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: laim_table                ! monthly leaf area index, one-sided
    real(kind=kind_noahmp), allocatable, dimension(:)      :: sla_table                 ! single-side leaf area per kg [m2/kg]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: dilefc_table              ! coeficient for leaf stress death [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: dilefw_table              ! coeficient for leaf stress death [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: fragr_table               ! fraction of growth respiration  !original was 0.3
    real(kind=kind_noahmp), allocatable, dimension(:)      :: ltovrc_table              ! leaf turnover [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: c3psn_table               ! photosynthetic pathway: 0. = c4, 1. = c3
    real(kind=kind_noahmp), allocatable, dimension(:)      :: kc25_table                ! co2 michaelis-menten constant at 25C (Pa)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: akc_table                 ! q10 for kc25
    real(kind=kind_noahmp), allocatable, dimension(:)      :: ko25_table                ! o2 michaelis-menten constant at 25C (Pa)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: ako_table                 ! q10 for ko25
    real(kind=kind_noahmp), allocatable, dimension(:)      :: vcmx25_table              ! maximum rate of carboxylation at 25C (umol CO2/m2/s)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: avcmx_table               ! q10 for vcmx25
    real(kind=kind_noahmp), allocatable, dimension(:)      :: bp_table                  ! minimum leaf conductance (umol/m2/s)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: mp_table                  ! slope of conductance-to-photosynthesis relationship
    real(kind=kind_noahmp), allocatable, dimension(:)      :: qe25_table                ! quantum efficiency at 25C (umol CO2 / umol photon)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: aqe_table                 ! q10 for qe25
    real(kind=kind_noahmp), allocatable, dimension(:)      :: rmf25_table               ! leaf maintenance respiration at 25C (umol CO2/m2/s)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: rms25_table               ! stem maintenance respiration at 25C (umol CO2/kg bio/s)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: rmr25_table               ! root maintenance respiration at 25C (umol CO2/kg bio/s)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: arm_table                 ! q10 for maintenance respiration
    real(kind=kind_noahmp), allocatable, dimension(:)      :: folnmx_table              ! foliage nitrogen concentration when f(n)=1 (%)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: tmin_table                ! minimum temperature for photosynthesis (K)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: xl_table                  ! leaf/stem orientation index
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: rhol_table                ! leaf reflectance: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: rhos_table                ! stem reflectance: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: taul_table                ! leaf transmittance: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: taus_table                ! stem transmittance: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:)      :: mrp_table                 ! microbial respiration parameter (umol CO2 /kg c/ s)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: cwpvt_table               ! empirical canopy wind parameter
    real(kind=kind_noahmp), allocatable, dimension(:)      :: wrrat_table               ! wood to non-wood ratio
    real(kind=kind_noahmp), allocatable, dimension(:)      :: wdpool_table              ! wood pool (switch 1 or 0) depending on woody or not [-]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: tdlef_table               ! characteristic t for leaf freezing [K]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: nroot_table               ! number of soil layers with root present
    real(kind=kind_noahmp), allocatable, dimension(:)      :: rgl_table                 ! parameter used in radiation stress function
    real(kind=kind_noahmp), allocatable, dimension(:)      :: rs_table                  ! minimum stomatal resistance [s m-1]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: hs_table                  ! parameter used in vapor pressure deficit function
    real(kind=kind_noahmp), allocatable, dimension(:)      :: topt_table                ! optimum transpiration air temperature [K]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: rsmax_table               ! maximal stomatal resistance [s m-1]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: rtovrc_table              ! root turnover coefficient [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: rswoodc_table             ! wood respiration coeficient [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: bf_table                  ! parameter for present wood allocation [-]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: wstrc_table               ! water stress coeficient [-]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: laimin_table              ! minimum leaf area index [m2/m2]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: xsamin_table              ! minimum stem area index [m2/m2]

    ! radiation parameters
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: albsat_table              ! saturated soil albedos: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: albdry_table              ! dry soil albedos: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:)      :: albice_table              ! albedo land ice: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:)      :: alblak_table              ! albedo frozen lakes: 1=vis, 2=nir
    real(kind=kind_noahmp), allocatable, dimension(:)      :: omegas_table              ! two-stream parameter omega for snow
    real(kind=kind_noahmp)                                 :: betads_table              ! two-stream parameter betad for snow
    real(kind=kind_noahmp)                                 :: betais_table              ! two-stream parameter betad for snow
    real(kind=kind_noahmp), allocatable, dimension(:)      :: eg_table                  ! emissivity soil surface
    real(kind=kind_noahmp)                                 :: eice_table                ! ice surface emissivity

    ! global parameters
    real(kind=kind_noahmp)                                 :: co2_table                 ! co2 partial pressure
    real(kind=kind_noahmp)                                 :: o2_table                  ! o2 partial pressure
    real(kind=kind_noahmp)                                 :: timean_table              ! gridcell mean topgraphic index (global mean)
    real(kind=kind_noahmp)                                 :: fsatmx_table              ! maximum surface saturated fraction (global mean)
    real(kind=kind_noahmp)                                 :: z0sno_table               ! snow surface roughness length (m) (0.002)
    real(kind=kind_noahmp)                                 :: ssi_table                 ! liquid water holding capacity for snowpack (m3/m3) (0.03)
    real(kind=kind_noahmp)                                 :: snow_ret_fac_table        ! snowpack water release timescale factor (1/s)
    real(kind=kind_noahmp)                                 :: snow_emis_table           ! snow emissivity
    real(kind=kind_noahmp)                                 :: swemx_table               ! new snow mass to fully cover old snow (mm)
    real(kind=kind_noahmp)                                 :: tau0_table                ! tau0 from Yang97 eqn. 10a
    real(kind=kind_noahmp)                                 :: grain_growth_table        ! growth from vapor diffusion Yang97 eqn. 10b
    real(kind=kind_noahmp)                                 :: extra_growth_table        ! extra growth near freezing Yang97 eqn. 10c
    real(kind=kind_noahmp)                                 :: dirt_soot_table           ! dirt and soot term Yang97 eqn. 10d
    real(kind=kind_noahmp)                                 :: bats_cosz_table           ! zenith angle snow albedo adjustment; b in Yang97 eqn. 15
    real(kind=kind_noahmp)                                 :: bats_vis_new_table        ! new snow visible albedo
    real(kind=kind_noahmp)                                 :: bats_nir_new_table        ! new snow nir albedo
    real(kind=kind_noahmp)                                 :: bats_vis_age_table        ! age factor for diffuse visible snow albedo Yang97 eqn. 17
    real(kind=kind_noahmp)                                 :: bats_nir_age_table        ! age factor for diffuse nir snow albedo Yang97 eqn. 18
    real(kind=kind_noahmp)                                 :: bats_vis_dir_table        ! cosz factor for direct visible snow albedo Yang97 eqn. 15
    real(kind=kind_noahmp)                                 :: bats_nir_dir_table        ! cosz factor for direct nir snow albedo Yang97 eqn. 16
    real(kind=kind_noahmp)                                 :: rsurf_snow_table          ! surface resistance for snow(s/m)
    real(kind=kind_noahmp)                                 :: rsurf_exp_table           ! exponent in the shape parameter for soil resistance option 1
    real(kind=kind_noahmp)                                 :: c2_snowcompact_table      ! overburden snow compaction parameter (m3/kg)
    real(kind=kind_noahmp)                                 :: c3_snowcompact_table      ! snow desctructive metamorphism compaction parameter1 [1/s]
    real(kind=kind_noahmp)                                 :: c4_snowcompact_table      ! snow desctructive metamorphism compaction parameter2 [1/k]
    real(kind=kind_noahmp)                                 :: c5_snowcompact_table      ! snow desctructive metamorphism compaction parameter3
    real(kind=kind_noahmp)                                 :: dm_snowcompact_table      ! upper limit on destructive metamorphism compaction [kg/m3]
    real(kind=kind_noahmp)                                 :: eta0_snowcompact_table    ! snow viscosity coefficient [kg-s/m2]
    real(kind=kind_noahmp)                                 :: snliqmaxfrac_table        ! maximum liquid water fraction in snow
    real(kind=kind_noahmp)                                 :: swemaxgla_table           ! maximum swe allowed at glaciers (mm)
    real(kind=kind_noahmp)                                 :: wslmax_table              ! maximum lake water storage (mm)
    real(kind=kind_noahmp)                                 :: rous_table                ! specific yield [-] for Niu et al. 2007 groundwater scheme
    real(kind=kind_noahmp)                                 :: cmic_table                ! microprore content (0.0-1.0), 0.0: close to free drainage
    real(kind=kind_noahmp)                                 :: snowden_max_table         ! maximum fresh snowfall density (kg/m3)
    real(kind=kind_noahmp)                                 :: class_alb_ref_table       ! reference snow albedo in class scheme
    real(kind=kind_noahmp)                                 :: class_sno_age_table       ! snow aging e-folding time (s) in class albedo scheme
    real(kind=kind_noahmp)                                 :: class_alb_new_table       ! fresh snow albedo in class scheme
    real(kind=kind_noahmp)                                 :: psiwlt_table              ! soil metric potential for wilting point (m)
    real(kind=kind_noahmp)                                 :: z0soil_table              ! bare-soil roughness length (m) (i.e., under the canopy)
    real(kind=kind_noahmp)                                 :: z0lake_table              ! lake surface roughness length (m)

    ! irrigation parameters
    integer                                                :: irr_har_table             ! number of days before harvest date to stop irrigation
    real(kind=kind_noahmp)                                 :: irr_frac_table            ! irrigation fraction
    real(kind=kind_noahmp)                                 :: irr_lai_table             ! minimum lai to trigger irrigation
    real(kind=kind_noahmp)                                 :: irr_mad_table             ! management allowable deficit (0-1)
    real(kind=kind_noahmp)                                 :: filoss_table              ! factor of flood irrigation loss
    real(kind=kind_noahmp)                                 :: sprir_rate_table          ! mm/h, sprinkler irrigation rate
    real(kind=kind_noahmp)                                 :: micir_rate_table          ! mm/h, micro irrigation rate
    real(kind=kind_noahmp)                                 :: firtfac_table             ! flood application rate factor
    real(kind=kind_noahmp)                                 :: ir_rain_table             ! maximum precipitation to stop irrigation trigger

    ! tile drainage parameters
    integer                                                :: drain_layer_opt_table     ! tile drainage layer
    integer               , allocatable, dimension(:)      :: td_depth_table            ! tile drainage depth (layer number) from soil surface
    real(kind=kind_noahmp), allocatable, dimension(:)      :: tdsmc_fac_table           ! tile drainage soil moisture factor
    real(kind=kind_noahmp), allocatable, dimension(:)      :: td_dc_table               ! tile drainage coefficient [mm/d]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: td_dcoef_table            ! tile drainage coefficient [mm/d]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: td_d_table                ! depth to impervious layer from drain water level [m]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: td_adepth_table           ! actual depth of impervious layer from land surface [m]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: td_radi_table             ! effective radius of drain tubes [m]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: td_spac_table             ! distance between two drain tubes or tiles [m]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: td_ddrain_table           ! tile drainage depth [m]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: klat_fac_table            ! hydraulic conductivity mutiplification factor

    ! crop parameters
    integer                                                :: default_crop_table        ! default crop index
    integer               , allocatable, dimension(:)      :: pltday_table              ! planting date
    integer               , allocatable, dimension(:)      :: hsday_table               ! harvest date
    real(kind=kind_noahmp), allocatable, dimension(:)      :: plantpop_table            ! plant density [per ha] - used?
    real(kind=kind_noahmp), allocatable, dimension(:)      :: irri_table                ! irrigation strategy 0= non-irrigation 1=irrigation (no water-stress)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: gddtbase_table            ! base temperature for gdd accumulation [C]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: gddtcut_table             ! upper temperature for gdd accumulation [C]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: gdds1_table               ! gdd from seeding to emergence
    real(kind=kind_noahmp), allocatable, dimension(:)      :: gdds2_table               ! gdd from seeding to initial vegetative
    real(kind=kind_noahmp), allocatable, dimension(:)      :: gdds3_table               ! gdd from seeding to post vegetative
    real(kind=kind_noahmp), allocatable, dimension(:)      :: gdds4_table               ! gdd from seeding to intial reproductive
    real(kind=kind_noahmp), allocatable, dimension(:)      :: gdds5_table               ! gdd from seeding to pysical maturity
    real(kind=kind_noahmp), allocatable, dimension(:)      :: c3psni_table              ! photosynthetic pathway: 0. = c4, 1. = c3 ! Zhe Zhang 2020-07-03
    real(kind=kind_noahmp), allocatable, dimension(:)      :: kc25i_table               ! co2 michaelis-menten constant at 25c (Pa)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: akci_table                ! q10 for kc25
    real(kind=kind_noahmp), allocatable, dimension(:)      :: ko25i_table               ! o2 michaelis-menten constant at 25c (Pa)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: akoi_table                ! q10 for ko25
    real(kind=kind_noahmp), allocatable, dimension(:)      :: vcmx25i_table             ! maximum rate of carboxylation at 25c (umol CO2/m2/s)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: avcmxi_table              ! q10 for vcmx25
    real(kind=kind_noahmp), allocatable, dimension(:)      :: bpi_table                 ! minimum leaf conductance (umol/m2/s)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: mpi_table                 ! slope of conductance-to-photosynthesis relationship
    real(kind=kind_noahmp), allocatable, dimension(:)      :: qe25i_table               ! quantum efficiency at 25c (umol CO2 / umol photon)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: folnmxi_table             ! foliage nitrogen concentration when f(n)=1 (%)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: aref_table                ! reference maximum CO2 assimulation rate
    real(kind=kind_noahmp), allocatable, dimension(:)      :: psnrf_table               ! co2 assimulation reduction factor(0-1) (caused by non-modeled part, pest,weeds)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: i2par_table               ! fraction of incoming solar radiation to photosynthetically active radiation
    real(kind=kind_noahmp), allocatable, dimension(:)      :: tassim0_table             ! minimum temperature for CO2 assimulation [C]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: tassim1_table             ! co2 assimulation linearly increasing until temperature reaches t1 [C]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: tassim2_table             ! co2 assmilation rate remain at aref until temperature reaches t2 [C]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: k_table                   ! light extinction coefficient
    real(kind=kind_noahmp), allocatable, dimension(:)      :: epsi_table                ! initial light use efficiency
    real(kind=kind_noahmp), allocatable, dimension(:)      :: q10mr_table               ! q10 for maintainance respiration
    real(kind=kind_noahmp), allocatable, dimension(:)      :: lefreez_table             ! characteristic t for leaf freezing [K]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: dile_fc_table             ! coeficient for temperature leaf stress death [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: dile_fw_table             ! coeficient for water leaf stress death [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: fra_gr_table              ! fraction of growth respiration
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: lf_ovrc_table             ! fraction of leaf turnover  [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: st_ovrc_table             ! fraction of stem turnover  [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: rt_ovrc_table             ! fraction of root tunrover  [1/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: lfmr25_table              ! leaf maintenance respiration at 25C [umol CO2/m2/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: stmr25_table              ! stem maintenance respiration at 25C [umol CO2/kg bio/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: rtmr25_table              ! root maintenance respiration at 25C [umol CO2/kg bio/s]
    real(kind=kind_noahmp), allocatable, dimension(:)      :: grainmr25_table           ! grain maintenance respiration at 25C [umol CO2/kg bio/s]
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: lfpt_table                ! fraction of carbohydrate flux to leaf
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: stpt_table                ! fraction of carbohydrate flux to stem
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: rtpt_table                ! fraction of carbohydrate flux to root
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: grainpt_table             ! fraction of carbohydrate flux to grain
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: lfct_table                ! fraction of carbohydrate translocation from leaf to grain
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: stct_table                ! fraction of carbohydrate translocation from stem to grain
    real(kind=kind_noahmp), allocatable, dimension(:,:)    :: rtct_table                ! fraction of carbohydrate translocation from root to grain
    real(kind=kind_noahmp), allocatable, dimension(:)      :: bio2lai_table             ! leaf area per living leaf biomass [m2/kg]

    ! soil parameters
    integer                                                :: slcats_table              ! number of soil categories
    real(kind=kind_noahmp), allocatable, dimension(:)      :: bexp_table                ! soil b parameter
    real(kind=kind_noahmp), allocatable, dimension(:)      :: smcdry_table              ! dry soil moisture threshold
    real(kind=kind_noahmp), allocatable, dimension(:)      :: smcmax_table              ! porosity, saturated value of soil moisture (volumetric)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: smcref_table              ! reference soil moisture (field capacity) (volumetric)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: psisat_table              ! saturated soil matric potential
    real(kind=kind_noahmp), allocatable, dimension(:)      :: dksat_table               ! saturated soil hydraulic conductivity
    real(kind=kind_noahmp), allocatable, dimension(:)      :: dwsat_table               ! saturated soil hydraulic diffusivity
    real(kind=kind_noahmp), allocatable, dimension(:)      :: smcwlt_table              ! wilting point soil moisture (volumetric)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: quartz_table              ! soil quartz content
    real(kind=kind_noahmp), allocatable, dimension(:)      :: bvic_table                ! vic model infiltration parameter (-) for opt_run=6
    real(kind=kind_noahmp), allocatable, dimension(:)      :: axaj_table                ! xinanjiang: tension water distribution inflection parameter [-] for opt_run=7
    real(kind=kind_noahmp), allocatable, dimension(:)      :: bxaj_table                ! xinanjiang: tension water distribution shape parameter [-] for opt_run=7
    real(kind=kind_noahmp), allocatable, dimension(:)      :: xxaj_table                ! xinanjiang: free water distribution shape parameter [-] for opt_run=7
    real(kind=kind_noahmp), allocatable, dimension(:)      :: bdvic_table               ! vic model infiltration parameter (-)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: gdvic_table               ! mean capilary drive (m)
    real(kind=kind_noahmp), allocatable, dimension(:)      :: bbvic_table               ! heterogeniety parameter for dvic infiltration [-]

    ! general parameters
    real(kind=kind_noahmp), allocatable, dimension(:)      :: slope_table               ! slope factor for soil drainage
    real(kind=kind_noahmp)                                 :: csoil_table               ! Soil heat capacity [J m-3 K-1]
    real(kind=kind_noahmp)                                 :: refdk_table               ! parameter in the surface runoff parameterization
    real(kind=kind_noahmp)                                 :: refkdt_table              ! parameter in the surface runoff parameterization
    real(kind=kind_noahmp)                                 :: frzk_table                ! frozen ground parameter
    real(kind=kind_noahmp)                                 :: zbot_table                ! depth [m] of lower boundary soil temperature
    real(kind=kind_noahmp)                                 :: czil_table                ! parameter used in the calculation of the roughness length for heat

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
