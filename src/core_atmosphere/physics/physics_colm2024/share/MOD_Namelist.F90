#include <define.h>

MODULE MOD_Namelist

!-----------------------------------------------------------------------
! !DESCRIPTION:
!
!    Variables in namelist files and subroutines to read namelist files.
!
!  Initial Authors: Shupeng Zhang, Zhongwang Wei, Xingjie Lu, Nan Wei,
!                   Hua Yuan, Wenzong Dong et al., May 2023
!-----------------------------------------------------------------------

   USE MOD_Precision, only: r8
   IMPLICIT NONE
   SAVE

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ----- Part 0: CASE name -----
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   character(len=256) :: DEF_CASE_NAME = 'CASENAME'

! ~~~~~~~~~~~~~~~~~~~~~~~~~~
! ----- Part 1: domain -----
! ~~~~~~~~~~~~~~~~~~~~~~~~~~

   type nl_domain_type
      real(r8) :: edges = -90.0
      real(r8) :: edgen = 90.0
      real(r8) :: edgew = -180.0
      real(r8) :: edgee = 180.0
   END type nl_domain_type

   type (nl_domain_type) :: DEF_domain

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ----- Part 2: blocks and MPI  -----
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   ! "blocks" is used to deal with high resolution data.
   ! It is defined by one of the following (in order of priority):
   !   1) "DEF_BlockInfoFile" : "lat_s","lat_n","lon_w","lon_e" in file ;
   !   2) "DEF_AverageElementSize" : diameter of element (in kilometer);
   !   3) "DEF_nx_blocks" and "DEF_ny_blocks" : number of blocks;
   character(len=256) :: DEF_BlockInfoFile = 'null'
   real(r8) :: DEF_AverageElementSize = -1.
   integer  :: DEF_nx_blocks = 72
   integer  :: DEF_ny_blocks = 36

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ----- Part 3: For Single Point -----
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   character(len=256) :: SITE_fsitedata = 'null'

   real(r8) :: SITE_lon_location = -1.e36_r8
   real(r8) :: SITE_lat_location = -1.e36_r8

   integer  :: SITE_landtype             = -1

   logical  :: USE_SITE_landtype         = .false.
   logical  :: USE_SITE_pctpfts          = .true.
   logical  :: USE_SITE_pctcrop          = .true.
   logical  :: USE_SITE_htop             = .true.
   logical  :: USE_SITE_LAI              = .true.
   logical  :: USE_SITE_lakedepth        = .true.
   logical  :: USE_SITE_soilreflectance  = .true.
   logical  :: USE_SITE_soilparameters   = .true.
   logical  :: USE_SITE_dbedrock         = .true.
   logical  :: USE_SITE_topography       = .true.
   logical  :: USE_SITE_urban_geometry   = .true.
   logical  :: USE_SITE_urban_ecology    = .true.
   logical  :: USE_SITE_urban_radiation  = .true.
   logical  :: USE_SITE_urban_thermal    = .false.
   logical  :: USE_SITE_urban_human      = .true.
   logical  :: USE_SITE_HistWriteBack    = .true.
   logical  :: USE_SITE_ForcingReadAhead = .true.

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ----- Part 4: simulation time type -----
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   type nl_simulation_time_type
      logical  :: greenwich     = .TRUE.
      integer  :: start_year    = 2000
      integer  :: start_month   = 1
      integer  :: start_day     = 1
      integer  :: start_sec     = 0
      integer  :: end_year      = 2003
      integer  :: end_month     = 1
      integer  :: end_day       = 1
      integer  :: end_sec       = 0
      integer  :: spinup_year   = 0
      integer  :: spinup_month  = 1
      integer  :: spinup_day    = 1
      integer  :: spinup_sec    = 0
      integer  :: spinup_repeat = 1
      real(r8) :: timestep      = 1800.
   END type nl_simulation_time_type

   type (nl_simulation_time_type) :: DEF_simulation_time

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ----- Part 5: directories and files -----
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   character(len=256) :: DEF_dir_rawdata  = 'path/to/rawdata/'
   character(len=256) :: DEF_dir_runtime  = 'path/to/runtime/'
   character(len=512) :: DEF_dir_output   = 'path/to/output/data'

   character(len=256) :: DEF_dir_landdata = 'path/to/landdata'
   character(len=1024) :: DEF_dir_restart = 'path/to/restart'
   character(len=256) :: DEF_dir_history  = 'path/to/history'

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ----- Part 6: landdata and surface inputs -----
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   character(len=256) :: DEF_file_mesh          = 'path/to/mesh/file'
   real(r8) :: DEF_GRIDBASED_lon_res = 0.5
   real(r8) :: DEF_GRIDBASED_lat_res = 0.5

   character(len=256) :: DEF_CatchmentMesh_data = 'path/to/catchment/data'

   ! ----- Use surface data from existing dataset -----
   ! case 1: from a larger region
   logical :: USE_srfdata_from_larger_region = .false.
   character(len=256) :: DEF_dir_existing_srfdata = 'path/to/landdata'
   ! case 2: from gridded data with dimensions [patch,lon,lat] or [pft,lon,lat]
   !         only available for USGS/IGBP/PFT CLASSIFICATION
   logical :: USE_srfdata_from_3D_gridded_data = .false.

   ! ----- land cover data year (for static land cover, i.e. non-LULCC) -----
   ! NOTE: Please check the LC data year range available
   integer :: DEF_LC_YEAR  = 2005

   ! ----- Subgrid scheme -----
   logical :: DEF_USE_USGS = .false.
   logical :: DEF_USE_IGBP = .false.
   logical :: DEF_USE_LCT  = .false.
   logical :: DEF_USE_PFT  = .false.
   logical :: DEF_USE_PC   = .false.
   logical :: DEF_SOLO_PFT = .false.
   logical :: DEF_FAST_PC  = .true.
   logical :: DEF_PC_CROP_SPLIT = .true.
   character(len=256) :: DEF_SUBGRID_SCHEME = 'LCT'

   logical :: DEF_LANDONLY                  = .true.
   logical :: DEF_USE_DOMINANT_PATCHTYPE    = .false.

   ! soil hydraulic parameters are upscaled from rawdata (1km resolution)
   ! to model patches through FIT algorithm (Montzka et al., 2017).
   logical :: DEF_USE_SOILPAR_UPS_FIT = .true.

   ! Options for soil reflectance setting schemes
   ! 1: Guessed soil color type according to land cover classes
   ! 2: Read a global soil color map from CLM
   integer :: DEF_SOIL_REFL_SCHEME = 2

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ----- Part 7: Leaf Area Index -----
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   ! NOTE: Please check the LAI data year range available
   integer :: DEF_LAI_START_YEAR = 2000
   integer :: DEF_LAI_END_YEAR   = 2020

   ! add by zhongwang wei @ sysu 2021/12/23
   ! To allow read satellite observed LAI
   ! 06/2023, note by hua yuan: change DEF_LAI_CLIM to DEF_LAI_MONTHLY
   logical :: DEF_LAI_MONTHLY = .true.

   ! ------LAI change and Land cover year setting ----------
   ! 06/2023, add by wenzong dong and hua yuan: use for updating LAI with simulation year
   logical :: DEF_LAI_CHANGE_YEARLY = .true.

   ! 05/2023, add by Xingjie Lu: use for updating LAI with leaf carbon
   logical :: DEF_USE_LAIFEEDBACK = .false.

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ----- Part 8: Initialization -----
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   logical :: DEF_USE_SoilInit  = .false.
   character(len=256) :: DEF_file_SoilInit = 'null'

   logical :: DEF_USE_SnowInit  = .false.
   character(len=256) :: DEF_file_SnowInit = 'null'

   logical :: DEF_USE_CN_INIT   = .false.
   character(len=256) :: DEF_file_cn_init  = 'null'

   logical :: DEF_USE_WaterTableInit = .false.
   character(len=256) :: DEF_file_WaterTable = 'null'

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ----- Part 9: LULCC related ------
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   ! Options for LULCC year-to-year transfer schemes
   ! 1: Same Type Assignment scheme (STA), state variables assignment for the same type (LC, PFT or PC)
   ! 2: Mass and Energy Conservation scheme (MEC), DO mass and energy conservation calculation
   integer :: DEF_LULCC_SCHEME = 1

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ----- Part 10: Urban model related ------
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   ! Options for urban type scheme
   ! 1: NCAR Urban Classification, 3 urban type with Tall Building, High Density and Medium Density
   ! 2: LCZ Classification, 10 urban type with LCZ 1-10
   integer :: DEF_URBAN_type_scheme = 1
   integer :: DEF_URBAN_geom_data   = 1
   logical :: DEF_URBAN_ONLY        = .false.
   logical :: DEF_URBAN_RUN         = .false.
   logical :: DEF_URBAN_BEM         = .true.
   logical :: DEF_URBAN_TREE        = .true.
   logical :: DEF_URBAN_WATER       = .true.
   logical :: DEF_URBAN_LUCY        = .true.
   logical :: DEF_USE_CANYON_HWR    = .true.

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ----- Part 11: parameterization schemes -----
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   integer :: DEF_Interception_scheme = 1  !1:CoLM；2:CLM4.5; 3:CLM5; 4:Noah-MP; 5:MATSIRO; 6:VIC; 7:JULES

   ! ----- SOIL parameters and supercool water setting ------
   integer :: DEF_THERMAL_CONDUCTIVITY_SCHEME = 4  ! Options for soil thermal conductivity schemes
                                                   ! 1: Farouki (1981)
                                                   ! 2: Johansen(1975)
                                                   ! 3: Cote and Konrad (2005)
                                                   ! 4: Balland and Arp (2005)
                                                   ! 5: Lu et al. (2007)
                                                   ! 6: Tarnawski and Leong (2012)
                                                   ! 7: De Vries (1963)
                                                   ! 8: Yan Hengnian, He Hailong et al.(2019)

   logical :: DEF_USE_SUPERCOOL_WATER = .true.     ! supercooled soil water scheme, Niu & Yang (2006)

   ! ----- Options for soil surface resistance schemes -----
   ! 0: NONE soil surface resistance
   ! 1: SL14, Swenson and Lawrence (2014)
   ! 2: SZ09, Sakaguchi and Zeng (2009)
   ! 3: TR13, Tang and Riley (2013)
   ! 4: LP92, Lee and Pielke (1992)
   ! 5: S92,  Sellers et al (1992)
   integer :: DEF_RSS_SCHEME = 1

   ! ----- Options for runoff parameterization schemes -----
   ! 0: scheme from TOPMODEL, also used in CoLM2014
   ! 1: scheme from VIC model
   ! 2: scheme from XinAnJiang model, also used in ECMWF model
   ! 3: scheme from Simple VIC, also used in NoahMP 5.0

   integer :: DEF_Runoff_SCHEME = 3
   character(len=256) :: DEF_file_VIC_para = 'null'
   character(len=256) :: DEF_file_VIC_OPT = 'null'
   logical :: DEF_VIC_OPT = .false.

   integer :: DEF_TOPMOD_method = 0

   ! ----- Treat exposed soil and snow surface separately -----
   ! including solar absorption, sensible/latent heat, ground temperature,
   ! ground heat flux and ground evp/dew/subl/fros. Corresponding vars are
   ! named as ***_soil, ***_snow.
   logical :: DEF_SPLIT_SOILSNOW = .false.

   ! ----- Account for vegetation snow process -----
   ! NOTE: This option will be activated in the new release, accompanied by
   !       a new set of canopy structure data, include the snow-free LAI.
   logical :: DEF_VEG_SNOW = .false.

   ! ----- Variably Saturated Flow Soil Water -----
   logical :: DEF_USE_VariablySaturatedFlow = .true.
   logical :: DEF_USE_BEDROCK               = .false.

   ! ----- Ozone stress -----
   logical :: DEF_USE_OZONESTRESS = .true.
   logical :: DEF_USE_OZONEDATA   = .true.

   ! ----- SNICAR model related -----
   logical :: DEF_USE_SNICAR                  = .false.
   character(len=256) :: DEF_file_snowoptics  = 'null'
   character(len=256) :: DEF_file_snowaging   = 'null'

   ! ----- Hyperspectral related -----
   logical :: DEF_HighResSoil                      = .true.
   logical :: DEF_HighResVeg                       = .true.
   logical :: DEF_PROSPECT                         = .false.

   CHARACTER(LEN=256) :: DEF_HighResUrban_albedo   = 'null'
   ! logical :: DEF_Satellite_Params                 = .false.
   ! character(len=256) :: DEF_file_soiloptics       = 'null'
   ! character(len=256) :: DEF_file_satellite_params = 'null'
   ! character(len=256) :: DEF_sla_varname           = 'null'

   ! .true. read aerosol deposition data from file or .false. set in the code
   logical :: DEF_Aerosol_Readin              = .true.

   ! .true. Read aerosol deposition climatology data or .false. yearly changed
   logical :: DEF_Aerosol_Clim                = .false.

   ! ----- Atmospheric Nitrogen Deposition -----
   !add by Fang Shang @ pku 2023/08
   !1: To allow annuaul ndep data to be read in
   !2: To allow monthly ndep data to be read in
   integer :: DEF_NDEP_FREQUENCY = 1

   ! ----- lateral flow related -----
   character(len=256) :: DEF_ElementNeighbour_file = 'null'
   character(len=256) :: DEF_UnitCatchment_file    = 'null'
   character(len=256) :: DEF_ReservoirPara_file    = 'null'

   logical  :: DEF_USE_EstimatedRiverDepth  = .true.
   integer  :: DEF_Reservoir_Method         = 0
   real(r8) :: DEF_GRIDBASED_ROUTING_MAX_DT = 3600.

   ! ----- sediment module -----
   logical  :: DEF_USE_SEDIMENT        = .false.
   real(r8) :: DEF_SED_LAMBDA          = 0.4
   real(r8) :: DEF_SED_LYRDPH          = 0.00005
   real(r8) :: DEF_SED_DENSITY         = 2.65
   real(r8) :: DEF_SED_WATER_DENSITY   = 1.0
   real(r8) :: DEF_SED_VISKIN          = 1.0e-6
   real(r8) :: DEF_SED_VONKAR          = 0.4
   real(r8) :: DEF_SED_PSET            = 1.0
   integer  :: DEF_SED_TOTLYRNUM       = 5
   real(r8) :: DEF_SED_CFL_ADV         = 0.5
   real(r8) :: DEF_SED_IGNORE_DPH      = 0.05
   real(r8) :: DEF_SED_DT_MAX          = 3600.
   character(len=256) :: DEF_SED_DIAMETER = "0.0002,0.002,0.02"
   real(r8) :: DEF_SED_PYLD            = 0.01
   real(r8) :: DEF_SED_PYLDC           = 2.0
   real(r8) :: DEF_SED_PYLDPC          = 2.0
   real(r8) :: DEF_SED_DSYLUNIT        = 1.0e-6

   ! ----- others -----
   character(len=5)   :: DEF_precip_phase_discrimination_scheme = 'II'

   character(len=256) :: DEF_SSP        = '585'   ! Co2 path for CMIP6 future scenario.

   logical :: DEF_USE_IRRIGATION        = .false. ! use irrigation
   integer :: DEF_IRRIGATION_ALLOCATION = 1       ! irrigation allocated method

   logical :: DEF_USE_NOSTRESSNITROGEN  = .false. ! photosynthesis stress option
   integer :: DEF_RSTFAC                = 1       ! root resistance factors option
   logical :: DEF_USE_PLANTHYDRAULICS   = .true.  ! Plant Hydraulics
   logical :: DEF_USE_MEDLYNST          = .false. ! Medlyn stomata model
   logical :: DEF_USE_WUEST             = .true.  ! WUE stomata model

   logical :: DEF_USE_SASU              = .false. ! Semi-Analytic-Spin-Up
   logical :: DEF_USE_DiagMatrix        = .false.
   logical :: DEF_USE_PN                = .false. ! Punctuated nitrogen addition Spin up

   logical :: DEF_USE_FERT              = .true.  ! Fertilisation on crop
   integer :: DEF_FERT_SOURCE           = 1       ! Fertilisation data source
   logical :: DEF_USE_NITRIF            = .true.  ! Nitrification and denitrification switch
   logical :: DEF_USE_CNSOYFIXN         = .true.  ! Soy nitrogen fixation

   logical :: DEF_USE_FIRE              = .false. ! Fire MODULE

   logical :: DEF_USE_Dynamic_Lake      = .false. ! Dynamic Lake model

   logical :: DEF_USE_Dynamic_Wetland   = .false. ! Dynamic wetland model

   logical :: DEF_CheckEquilibrium      = .false.

   logical :: DEF_Output_2mWMO          = .false. ! 2m WMO temperature

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ----- Part 12: forcing -----
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   character(len=256) :: DEF_forcing_namelist  = 'null'

   character(len=256) :: DEF_dir_forcing       = 'path/to/forcing/data'

   type nl_forcing_type

      character(len=256) :: dataset            = 'CRUNCEP'
      logical            :: solarin_all_band   = .true.
      character(len=256) :: HEIGHT_mode        = 'absolute'
      real(r8)           :: HEIGHT_V           = 100.0
      real(r8)           :: HEIGHT_T           = 50.
      real(r8)           :: HEIGHT_Q           = 50.

      logical            :: regional           = .false.
      real(r8)           :: regbnd(4)          = (/-90.0, 90.0, -180.0, 180.0/)
      logical            :: has_missing_value  = .false.
      character(len=256) :: missing_value_name = 'missing_value'

      integer            :: NVAR               = 8              ! variable number of forcing data
      integer            :: startyr            = 2000           ! start year of forcing data
      integer            :: startmo            = 1              ! start month of forcing data
      integer            :: endyr              = 2003           ! end year of forcing data
      integer            :: endmo              = 12             ! end month of forcing data
      integer            :: dtime(8)           = (/21600,21600,21600,21600,0,21600,21600,21600/)
      integer            :: offset(8)          = (/10800,10800,10800,10800,0,10800,0,10800/)
      integer            :: nlands             = 1              ! land grid number in 1d

      logical            :: leapyear           = .false.        ! leapyear calendar
      logical            :: data2d             = .true.         ! data in 2 dimension (lon, lat)
      logical            :: hightdim           = .false.        ! have "z" dimension
      logical            :: dim2d              = .true.         ! lat/lon value in 2 dimension (lon, lat)

      character(len=256) :: latname            = 'LATIXY'       ! dimension name of latitude
      character(len=256) :: lonname            = 'LONGXY'       ! dimension name of longitude

      character(len=256) :: groupby            = 'month'        ! file grouped by year/month

      character(len=256) :: fprefix(8)         = (/ &
         'TPHWL6Hrly/clmforc.cruncep.V4.c2011.0.5d.TPQWL.', &
         'TPHWL6Hrly/clmforc.cruncep.V4.c2011.0.5d.TPQWL.', &
         'TPHWL6Hrly/clmforc.cruncep.V4.c2011.0.5d.TPQWL.', &
         'Precip6Hrly/clmforc.cruncep.V4.c2011.0.5d.Prec.', &
         'NULL                                           ', &
         'TPHWL6Hrly/clmforc.cruncep.V4.c2011.0.5d.TPQWL.', &
         'Solar6Hrly/clmforc.cruncep.V4.c2011.0.5d.Solr. ', &
         'TPHWL6Hrly/clmforc.cruncep.V4.c2011.0.5d.TPQWL.' /)
      character(len=256) :: vname(8)           = (/ &
         'TBOT    ','QBOT    ','PSRF    ','PRECTmms', &
         'NULL    ','WIND    ','FSDS    ','FLDS    ' /)
      character(len=256) :: timelog(8)         = (/ &
         'instant ','instant ','instant ','forward ', &
         'NULL    ','instant ','forward ','forward ' /)
      character(len=256) :: tintalgo(8)        = (/ &
         'linear ','linear ','linear ','nearest', &
         'NULL   ','linear ','coszen ','linear ' /)

      character(len=256) :: CBL_fprefix        = 'TPHWL6Hrly/clmforc.cruncep.V4.c2011.0.5d.TPQWL.'
      character(len=256) :: CBL_vname          = 'blh'
      character(len=256) :: CBL_tintalgo       = 'linear'
      integer            :: CBL_dtime          = 21600
      integer            :: CBL_offset         = 10800
   END type nl_forcing_type

   type (nl_forcing_type) :: DEF_forcing

   !CBL height
   logical           :: DEF_USE_CBL_HEIGHT     = .false.

   character(len=20) :: DEF_Forcing_Interp_Method = 'arealweight' ! 'arealweight' (default) or 'bilinear'

   logical           :: DEF_USE_Forcing_Downscaling        = .false.
   logical           :: DEF_USE_Forcing_Downscaling_Simple = .false.
   character(len=256):: DEF_DS_HiresTopographyDataDir      = 'null'
   character(len=5)  :: DEF_DS_precipitation_adjust_scheme = 'I'
   character(len=5)  :: DEF_DS_longwave_adjust_scheme      = 'II'

   logical           :: DEF_USE_ClimForcing_for_Spinup     = .false.

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ----- Part 13: data assimilation -----
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   character(len=256) :: DEF_DA_obsdir     = 'null'
   logical            :: DEF_DA_TWS        = .false.
   logical            :: DEF_DA_TWS_GRACE  = .false.
   logical            :: DEF_DA_SM         = .false.
   integer            :: DEF_DA_ENS_NUM    = 20
   logical            :: DEF_DA_ENS_SM     = .false.
   logical            :: DEF_DA_SM_SMAP    = .false.
   logical            :: DEF_DA_SM_FY      = .false.
   logical            :: DEF_DA_SM_SYNOP   = .false.
   integer            :: DEF_DA_RTM_diel   = 0
   integer            :: DEF_DA_RTM_rough  = 0

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ----- Part 14: parameter optimization -----
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   logical :: DEF_Optimize_Baseflow = .false.

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ----- Part 15: history and restart -----
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   logical  :: DEF_HISTORY_IN_VECTOR            = .false.

   logical  :: DEF_HIST_grid_as_forcing         = .false.
   real(r8) :: DEF_HIST_lon_res                 = 0.5
   real(r8) :: DEF_HIST_lat_res                 = 0.5

   character(len=256) :: DEF_WRST_FREQ          = 'none'  ! write restart file frequency: TIMESTEP/HOURLY/DAILY/MONTHLY/YEARLY
   character(len=256) :: DEF_HIST_FREQ          = 'none'  ! write history file frequency: TIMESTEP/HOURLY/DAILY/MONTHLY/YEARLY
   character(len=256) :: DEF_HIST_groupby       = 'MONTH' ! history file in one file: DAY/MONTH/YEAR
   character(len=256) :: DEF_HIST_mode          = 'one'
   logical  :: DEF_HIST_WriteBack               = .false.
   integer  :: DEF_REST_CompressLevel           = 1
   integer  :: DEF_HIST_CompressLevel           = 1

   character(len=256) :: DEF_HIST_vars_namelist = 'null'
   logical  :: DEF_HIST_vars_out_default        = .true.


   ! ----- history variables -----
   type history_var_type

      logical :: xy_us                            = .true.
      logical :: xy_vs                            = .true.
      logical :: xy_t                             = .true.
      logical :: xy_q                             = .true.
      logical :: xy_prc                           = .true.
      logical :: xy_prl                           = .true.
      logical :: xy_pbot                          = .true.
      logical :: xy_frl                           = .true.
      logical :: xy_solarin                       = .true.
      logical :: xy_rain                          = .true.
      logical :: xy_snow                          = .true.
      logical :: xy_ozone                         = .true.
      logical :: xy_hpbl                          = .true.

      logical :: taux                             = .true.
      logical :: tauy                             = .true.
      logical :: fsena                            = .true.
      logical :: lfevpa                           = .true.
      logical :: fevpa                            = .true.
      logical :: fsenl                            = .true.
      logical :: fevpl                            = .true.
      logical :: etr                              = .true.
      logical :: fseng                            = .true.
      logical :: fevpg                            = .true.
      logical :: fgrnd                            = .true.
      logical :: sabvsun                          = .true.
      logical :: sabvsha                          = .true.
      logical :: sabg                             = .true.
      logical :: olrg                             = .true.
      logical :: rnet                             = .true.
      logical :: xerr                             = .true.
      logical :: zerr                             = .true.
      logical :: rsur                             = .true.
      logical :: rsur_se                          = .true.
      logical :: rsur_ie                          = .true.
      logical :: rsub                             = .true.
      logical :: rnof                             = .true.
      logical :: xwsur                            = .true.
      logical :: xwsub                            = .true.
      logical :: fldarea                          = .true.
      logical :: qintr                            = .true.
      logical :: qinfl                            = .true.
      logical :: qdrip                            = .true.
      logical :: wat                              = .true.
      logical :: wat_inst                         = .true.
      logical :: wetwat                           = .true.
      logical :: wetwat_inst                      = .true.
      logical :: wetzwt                           = .true.
      logical :: assim                            = .true.
      logical :: respc                            = .true.
      logical :: qcharge                          = .true.
      logical :: t_grnd                           = .true.
      logical :: tleaf                            = .true.
      logical :: ldew                             = .true.
      logical :: scv                              = .true.
      logical :: snowdp                           = .true.
      logical :: fsno                             = .true.
      logical :: frcsat                           = .true.
      logical :: sigf                             = .true.
      logical :: green                            = .true.
      logical :: lai                              = .true.
      logical :: laisun                           = .true.
      logical :: laisha                           = .true.
      logical :: sai                              = .true.
      logical :: alb                              = .true.
      logical :: alb_hires                        = .true.
      logical :: reflectance_out                  = .true.
      logical :: transmittance_out                = .true.
      logical :: emis                             = .true.
      logical :: z0m                              = .true.
      logical :: trad                             = .true.
      logical :: rss                              = .true.
      logical :: tref                             = .true.
      logical :: t2m_wmo                          = .true.
      logical :: qref                             = .true.

      logical :: fsen_roof                        = .true.
      logical :: fsen_wsun                        = .true.
      logical :: fsen_wsha                        = .true.
      logical :: fsen_gimp                        = .true.
      logical :: fsen_gper                        = .true.
      logical :: fsen_urbl                        = .true.
      logical :: lfevp_roof                       = .true.
      logical :: lfevp_gimp                       = .true.
      logical :: lfevp_gper                       = .true.
      logical :: lfevp_urbl                       = .true.
      logical :: fhac                             = .true.
      logical :: fwst                             = .true.
      logical :: fach                             = .true.
      logical :: fhah                             = .true.
      logical :: meta                             = .true.
      logical :: vehc                             = .true.
      logical :: t_room                           = .true.
      logical :: tafu                             = .true.
      logical :: t_roof                           = .true.
      logical :: t_wall                           = .true.

      logical :: assimsun                         = .true.
      logical :: assimsha                         = .true.
      logical :: etrsun                           = .true.
      logical :: etrsha                           = .true.

      logical :: o3uptakesun                      = .true.
      logical :: o3uptakesha                      = .true.

      logical :: leafc                            = .true.
      logical :: leafc_storage                    = .true.
      logical :: leafc_xfer                       = .true.
      logical :: frootc                           = .true.
      logical :: frootc_storage                   = .true.
      logical :: frootc_xfer                      = .true.
      logical :: livestemc                        = .true.
      logical :: livestemc_storage                = .true.
      logical :: livestemc_xfer                   = .true.
      logical :: deadstemc                        = .true.
      logical :: deadstemc_storage                = .true.
      logical :: deadstemc_xfer                   = .true.
      logical :: livecrootc                       = .true.
      logical :: livecrootc_storage               = .true.
      logical :: livecrootc_xfer                  = .true.
      logical :: deadcrootc                       = .true.
      logical :: deadcrootc_storage               = .true.
      logical :: deadcrootc_xfer                  = .true.
      logical :: grainc                           = .true.
      logical :: grainc_storage                   = .true.
      logical :: grainc_xfer                      = .true.
      logical :: leafn                            = .true.
      logical :: leafn_storage                    = .true.
      logical :: leafn_xfer                       = .true.
      logical :: frootn                           = .true.
      logical :: frootn_storage                   = .true.
      logical :: frootn_xfer                      = .true.
      logical :: livestemn                        = .true.
      logical :: livestemn_storage                = .true.
      logical :: livestemn_xfer                   = .true.
      logical :: deadstemn                        = .true.
      logical :: deadstemn_storage                = .true.
      logical :: deadstemn_xfer                   = .true.
      logical :: livecrootn                       = .true.
      logical :: livecrootn_storage               = .true.
      logical :: livecrootn_xfer                  = .true.
      logical :: deadcrootn                       = .true.
      logical :: deadcrootn_storage               = .true.
      logical :: deadcrootn_xfer                  = .true.
      logical :: grainn                           = .true.
      logical :: grainn_storage                   = .true.
      logical :: grainn_xfer                      = .true.
      logical :: retrasn                          = .true.
      logical :: gpp                              = .true.
      logical :: downreg                          = .true.
      logical :: ar                               = .true.
      logical :: cwdprod                          = .true.
      logical :: cwddecomp                        = .true.
      logical :: hr                               = .true.
      logical :: fpg                              = .true.
      logical :: fpi                              = .true.
      logical :: totvegc                          = .true.
      logical :: totlitc                          = .true.
      logical :: totcwdc                          = .true.
      logical :: totsomc                          = .true.
      logical :: totcolc                          = .true.
      logical :: totvegn                          = .true.
      logical :: totlitn                          = .true.
      logical :: totcwdn                          = .true.
      logical :: totsomn                          = .true.
      logical :: totcoln                          = .true.
      logical :: totsoiln_vr                      = .true.
      logical :: gpp_enftemp                      = .false. !1
      logical :: gpp_enfboreal                    = .false. !2
      logical :: gpp_dnfboreal                    = .false. !3
      logical :: gpp_ebftrop                      = .false. !4
      logical :: gpp_ebftemp                      = .false. !5
      logical :: gpp_dbftrop                      = .false. !6
      logical :: gpp_dbftemp                      = .false. !7
      logical :: gpp_dbfboreal                    = .false. !8
      logical :: gpp_ebstemp                      = .false. !9
      logical :: gpp_dbstemp                      = .false. !10
      logical :: gpp_dbsboreal                    = .false. !11
      logical :: gpp_c3arcgrass                   = .false. !12
      logical :: gpp_c3grass                      = .false. !13
      logical :: gpp_c4grass                      = .false. !14
      logical :: leafc_enftemp                    = .false. !1
      logical :: leafc_enfboreal                  = .false. !2
      logical :: leafc_dnfboreal                  = .false. !3
      logical :: leafc_ebftrop                    = .false. !4
      logical :: leafc_ebftemp                    = .false. !5
      logical :: leafc_dbftrop                    = .false. !6
      logical :: leafc_dbftemp                    = .false. !7
      logical :: leafc_dbfboreal                  = .false. !8
      logical :: leafc_ebstemp                    = .false. !9
      logical :: leafc_dbstemp                    = .false. !10
      logical :: leafc_dbsboreal                  = .false. !11
      logical :: leafc_c3arcgrass                 = .false. !12
      logical :: leafc_c3grass                    = .false. !13
      logical :: leafc_c4grass                    = .false. !14
      logical :: lai_enftemp                      = .false. !1
      logical :: lai_enfboreal                    = .false. !2
      logical :: lai_dnfboreal                    = .false. !3
      logical :: lai_ebftrop                      = .false. !4
      logical :: lai_ebftemp                      = .false. !5
      logical :: lai_dbftrop                      = .false. !6
      logical :: lai_dbftemp                      = .false. !7
      logical :: lai_dbfboreal                    = .false. !8
      logical :: lai_ebstemp                      = .false. !9
      logical :: lai_dbstemp                      = .false. !10
      logical :: lai_dbsboreal                    = .false. !11
      logical :: lai_c3arcgrass                   = .false. !12
      logical :: lai_c3grass                      = .false. !13
      logical :: lai_c4grass                      = .false. !14
      logical :: npp_enftemp                      = .false. !1
      logical :: npp_enfboreal                    = .false. !2
      logical :: npp_dnfboreal                    = .false. !3
      logical :: npp_ebftrop                      = .false. !4
      logical :: npp_ebftemp                      = .false. !5
      logical :: npp_dbftrop                      = .false. !6
      logical :: npp_dbftemp                      = .false. !7
      logical :: npp_dbfboreal                    = .false. !8
      logical :: npp_ebstemp                      = .false. !9
      logical :: npp_dbstemp                      = .false. !10
      logical :: npp_dbsboreal                    = .false. !11
      logical :: npp_c3arcgrass                   = .false. !12
      logical :: npp_c3grass                      = .false. !13
      logical :: npp_c4grass                      = .false. !14
      logical :: npptoleafc_enftemp               = .false. !1
      logical :: npptoleafc_enfboreal             = .false. !2
      logical :: npptoleafc_dnfboreal             = .false. !3
      logical :: npptoleafc_ebftrop               = .false. !4
      logical :: npptoleafc_ebftemp               = .false. !5
      logical :: npptoleafc_dbftrop               = .false. !6
      logical :: npptoleafc_dbftemp               = .false. !7
      logical :: npptoleafc_dbfboreal             = .false. !8
      logical :: npptoleafc_ebstemp               = .false. !9
      logical :: npptoleafc_dbstemp               = .false. !10
      logical :: npptoleafc_dbsboreal             = .false. !11
      logical :: npptoleafc_c3arcgrass            = .false. !12
      logical :: npptoleafc_c3grass               = .false. !13
      logical :: npptoleafc_c4grass               = .false. !14

      logical :: cphase                           = .true.
      logical :: gddmaturity                      = .true.
      logical :: gddplant                         = .true.
      logical :: vf                               = .true.
      logical :: hui                              = .true.
      logical :: cropprod1c                       = .true.
      logical :: cropprod1c_loss                  = .true.
      logical :: cropseedc_deficit                = .true.
      logical :: grainc_to_cropprodc              = .true.
      logical :: plantdate_rainfed_temp_corn      = .true.
      logical :: plantdate_irrigated_temp_corn    = .true.
      logical :: plantdate_rainfed_spwheat        = .true.
      logical :: plantdate_irrigated_spwheat      = .true.
      logical :: plantdate_rainfed_wtwheat        = .true.
      logical :: plantdate_irrigated_wtwheat      = .true.
      logical :: plantdate_rainfed_temp_soybean   = .true.
      logical :: plantdate_irrigated_temp_soybean = .true.
      logical :: plantdate_rainfed_cotton         = .true.
      logical :: plantdate_irrigated_cotton       = .true.
      logical :: plantdate_rainfed_rice           = .true.
      logical :: plantdate_irrigated_rice         = .true.
      logical :: plantdate_rainfed_sugarcane      = .true.
      logical :: plantdate_irrigated_sugarcane    = .true.
      logical :: plantdate_rainfed_trop_corn      = .true.
      logical :: plantdate_irrigated_trop_corn    = .true.
      logical :: plantdate_rainfed_trop_soybean   = .true.
      logical :: plantdate_irrigated_trop_soybean = .true.
      logical :: plantdate_unmanagedcrop          = .true.
      logical :: cropprodc_rainfed_temp_corn      = .true.
      logical :: cropprodc_irrigated_temp_corn    = .true.
      logical :: cropprodc_rainfed_spwheat        = .true.
      logical :: cropprodc_irrigated_spwheat      = .true.
      logical :: cropprodc_rainfed_wtwheat        = .true.
      logical :: cropprodc_irrigated_wtwheat      = .true.
      logical :: cropprodc_rainfed_temp_soybean   = .true.
      logical :: cropprodc_irrigated_temp_soybean = .true.
      logical :: cropprodc_rainfed_cotton         = .true.
      logical :: cropprodc_irrigated_cotton       = .true.
      logical :: cropprodc_rainfed_rice           = .true.
      logical :: cropprodc_irrigated_rice         = .true.
      logical :: cropprodc_rainfed_sugarcane      = .true.
      logical :: cropprodc_irrigated_sugarcane    = .true.
      logical :: cropprodc_rainfed_trop_corn      = .true.
      logical :: cropprodc_irrigated_trop_corn    = .true.
      logical :: cropprodc_rainfed_trop_soybean   = .true.
      logical :: cropprodc_irrigated_trop_soybean = .true.
      logical :: cropprodc_unmanagedcrop          = .true.

      logical :: grainc_to_seed                   = .true.
      logical :: fert_to_sminn                    = .true.

      logical :: huiswheat                        = .true.
      logical :: pdcorn                           = .false.
      logical :: pdswheat                         = .false.
      logical :: pdwwheat                         = .false.
      logical :: pdsoybean                        = .false.
      logical :: pdcotton                         = .false.
      logical :: pdrice1                          = .false.
      logical :: pdrice2                          = .false.
      logical :: pdsugarcane                      = .false.
      logical :: manunitro                        = .false.
      logical :: fertnitro_corn                   = .true.
      logical :: fertnitro_swheat                 = .true.
      logical :: fertnitro_wwheat                 = .true.
      logical :: fertnitro_soybean                = .true.
      logical :: fertnitro_cotton                 = .true.
      logical :: fertnitro_rice1                  = .true.
      logical :: fertnitro_rice2                  = .true.
      logical :: fertnitro_sugarcane              = .true.
      logical :: irrig_method_corn                = .true.
      logical :: irrig_method_swheat              = .true.
      logical :: irrig_method_wwheat              = .true.
      logical :: irrig_method_soybean             = .true.
      logical :: irrig_method_cotton              = .true.
      logical :: irrig_method_rice1               = .true.
      logical :: irrig_method_rice2               = .true.
      logical :: irrig_method_sugarcane           = .true.

      logical :: sum_irrig                        = .true.
      logical :: sum_deficit_irrig                = .true.
      logical :: sum_irrig_count                  = .true.
      logical :: waterstorage                     = .true.
      logical :: groundwater_demand               = .true.
      logical :: groundwater_supply               = .true.
      logical :: reservoirriver_demand            = .true.
      logical :: reservoirriver_supply            = .true.

      logical :: ndep_to_sminn                    = .true.
      logical :: CONC_O2_UNSAT                    = .false.
      logical :: O2_DECOMP_DEPTH_UNSAT            = .false.
      logical :: abm                              = .false.
      logical :: gdp                              = .false.
      logical :: peatf                            = .false.
      logical :: hdm                              = .false.
      logical :: lnfm                             = .false.

      logical :: leafcCap                         = .false.
      logical :: leafc_storageCap                 = .false.
      logical :: leafc_xferCap                    = .false.
      logical :: frootcCap                        = .false.
      logical :: frootc_storageCap                = .false.
      logical :: frootc_xferCap                   = .false.
      logical :: livestemcCap                     = .false.
      logical :: livestemc_storageCap             = .false.
      logical :: livestemc_xferCap                = .false.
      logical :: deadstemcCap                     = .false.
      logical :: deadstemc_storageCap             = .false.
      logical :: deadstemc_xferCap                = .false.
      logical :: livecrootcCap                    = .false.
      logical :: livecrootc_storageCap            = .false.
      logical :: livecrootc_xferCap               = .false.
      logical :: deadcrootcCap                    = .false.
      logical :: deadcrootc_storageCap            = .false.
      logical :: deadcrootc_xferCap               = .false.
      logical :: leafnCap                         = .false.
      logical :: leafn_storageCap                 = .false.
      logical :: leafn_xferCap                    = .false.
      logical :: frootnCap                        = .false.
      logical :: frootn_storageCap                = .false.
      logical :: frootn_xferCap                   = .false.
      logical :: livestemnCap                     = .false.
      logical :: livestemn_storageCap             = .false.
      logical :: livestemn_xferCap                = .false.
      logical :: deadstemnCap                     = .false.
      logical :: deadstemn_storageCap             = .false.
      logical :: deadstemn_xferCap                = .false.
      logical :: livecrootnCap                    = .false.
      logical :: livecrootn_storageCap            = .false.
      logical :: livecrootn_xferCap               = .false.
      logical :: deadcrootnCap                    = .false.
      logical :: deadcrootn_storageCap            = .false.
      logical :: deadcrootn_xferCap               = .false.
      logical :: t_scalar                         = .false.
      logical :: w_scalar                         = .false.

      logical :: t_soisno                         = .true.
      logical :: wliq_soisno                      = .true.
      logical :: wice_soisno                      = .true.

      logical :: h2osoi                           = .true.
      logical :: qlayer                           = .true.
      logical :: lake_deficit                     = .true.
      logical :: rstfacsun                        = .true.
      logical :: rstfacsha                        = .true.
      logical :: gssun                            = .true.
      logical :: gssha                            = .true.
      logical :: rootr                            = .true.
      logical :: vegwp                            = .true.
      logical :: BD_all                           = .true.
      logical :: wfc                              = .true.
      logical :: OM_density                       = .true.
      logical :: wdsrf                            = .true.
      logical :: wdsrf_inst                       = .true.
      logical :: zwt                              = .true.
      logical :: wa                               = .true.
      logical :: wa_inst                          = .true.

      logical :: dz_lake                          = .true.
      logical :: t_lake                           = .true.
      logical :: lake_icefrac                     = .true.

      logical :: DA_wliq_h2osoi_5cm               = .true.
      logical :: DA_wliq_h2osoi_5cm_a             = .true.
      logical :: DA_t_soisno_5cm                  = .true.
      logical :: DA_t_soisno_5cm_a                = .true.
      logical :: DA_wliq_soisno_ens               = .true.
      logical :: DA_t_soisno_ens                  = .true.
      logical :: DA_wliq_soisno_5cm_ens_std       = .true.
      logical :: DA_t_soisno_5cm_ens_std          = .true.
      logical :: DA_t_brt_smap                    = .true.
      logical :: DA_t_brt_smap_a                  = .true.
      logical :: DA_t_brt_smap_ens                = .true.
      logical :: DA_t_brt_smap_ens_std            = .true.
      logical :: DA_t_brt_fy3d                    = .true.
      logical :: DA_t_brt_fy3d_a                  = .true.
      logical :: DA_t_brt_fy3d_ens                = .true.
      logical :: DA_t_brt_fy3d_ens_std            = .true.

      logical :: litr1c_vr                        = .true.
      logical :: litr2c_vr                        = .true.
      logical :: litr3c_vr                        = .true.
      logical :: soil1c_vr                        = .true.
      logical :: soil2c_vr                        = .true.
      logical :: soil3c_vr                        = .true.
      logical :: cwdc_vr                          = .true.
      logical :: litr1n_vr                        = .true.
      logical :: litr2n_vr                        = .true.
      logical :: litr3n_vr                        = .true.
      logical :: soil1n_vr                        = .true.
      logical :: soil2n_vr                        = .true.
      logical :: soil3n_vr                        = .true.
      logical :: cwdn_vr                          = .true.

      logical :: litr1cCap_vr                     = .false.
      logical :: litr2cCap_vr                     = .false.
      logical :: litr3cCap_vr                     = .false.
      logical :: soil1cCap_vr                     = .false.
      logical :: soil2cCap_vr                     = .false.
      logical :: soil3cCap_vr                     = .false.
      logical :: cwdcCap_vr                       = .false.
      logical :: litr1nCap_vr                     = .false.
      logical :: litr2nCap_vr                     = .false.
      logical :: litr3nCap_vr                     = .false.
      logical :: soil1nCap_vr                     = .false.
      logical :: soil2nCap_vr                     = .false.
      logical :: soil3nCap_vr                     = .false.
      logical :: cwdnCap_vr                       = .false.

      logical :: sminn_vr                         = .true.

      logical :: ustar                            = .true.
      logical :: ustar2                           = .true.
      logical :: tstar                            = .true.
      logical :: qstar                            = .true.
      logical :: zol                              = .true.
      logical :: rib                              = .true.
      logical :: fm                               = .true.
      logical :: fh                               = .true.
      logical :: fq                               = .true.
      logical :: us10m                            = .true.
      logical :: vs10m                            = .true.
      logical :: fm10m                            = .true.
      logical :: sr                               = .true.
      logical :: solvd                            = .true.
      logical :: solvi                            = .true.
      logical :: solnd                            = .true.
      logical :: solni                            = .true.
      logical :: srvd                             = .true.
      logical :: srvi                             = .true.
      logical :: srnd                             = .true.
      logical :: srni                             = .true.

      logical :: solvdln                          = .true.
      logical :: solviln                          = .true.
      logical :: solndln                          = .true.
      logical :: solniln                          = .true.
      logical :: srvdln                           = .true.
      logical :: srviln                           = .true.
      logical :: srndln                           = .true.
      logical :: srniln                           = .true.

      logical :: sol_dir_ln_hires                 = .true.
      logical :: sol_dif_ln_hires                 = .true.
      logical :: sr_dir_ln_hires                  = .true.
      logical :: sr_dif_ln_hires                  = .true.

      logical :: xsubs_bsn                        = .true.
      logical :: xsubs_hru                        = .true.
      logical :: riv_height                       = .true.
      logical :: riv_veloct                       = .true.
      logical :: discharge                        = .true.
      logical :: floodarea                        = .true.
      logical :: floodfrc                         = .true.
      logical :: wdsrf_hru                        = .true.
      logical :: veloc_hru                        = .true.
      logical :: volresv                          = .true.
      logical :: qresv_in                         = .true.
      logical :: qresv_out                        = .true.

      logical :: sedcon                           = .true.
      logical :: sedout                           = .true.
      logical :: bedout                           = .true.
      logical :: sedinp                           = .true.
      logical :: netflw                           = .true.
      logical :: sedlayer                         = .true.
      logical :: shearvel                         = .false.

      logical :: sensors                          = .true.

   END type history_var_type

   type (history_var_type) :: DEF_hist_vars

CONTAINS

   SUBROUTINE read_namelist (nlfile)

   USE MOD_MPAS_MPI
   USE MOD_Utils, only: make_directory
   IMPLICIT NONE

   character(len=*), intent(in) :: nlfile

   ! Local variables
   logical :: fexists
   integer :: ivar
   integer :: ierr
   character(len=256) :: iomesg

   namelist /nl_colm/                         &
      DEF_CASE_NAME,                          &
      DEF_domain,                             &

      SITE_fsitedata,                         &
      SITE_lon_location,                      &
      SITE_lat_location,                      &
      SITE_landtype,                          &
      USE_SITE_landtype,                      &
      USE_SITE_pctpfts,                       &
      USE_SITE_pctcrop,                       &
      USE_SITE_htop,                          &
      USE_SITE_LAI,                           &
      USE_SITE_lakedepth,                     &
      USE_SITE_soilreflectance,               &
      USE_SITE_soilparameters,                &
      USE_SITE_dbedrock,                      &
      USE_SITE_topography,                    &
      USE_SITE_HistWriteBack,                 &
      USE_SITE_ForcingReadAhead,              &
      USE_SITE_urban_geometry,                &
      USE_SITE_urban_ecology,                 &
      USE_SITE_urban_radiation,               &
      USE_SITE_urban_thermal,                 &
      USE_SITE_urban_human,                   &

      DEF_BlockInfoFile,                      &
      DEF_AverageElementSize,                 &
      DEF_nx_blocks,                          &
      DEF_ny_blocks,                          &
      DEF_simulation_time,                    &
      DEF_dir_rawdata,                        &
      DEF_dir_runtime,                        &
      DEF_dir_output,                         &
      DEF_file_mesh,                          &
      DEF_GRIDBASED_lon_res,                  &
      DEF_GRIDBASED_lat_res,                  &
      DEF_CatchmentMesh_data,                 &

      DEF_USE_LCT,                            &
      DEF_USE_PFT,                            &
      DEF_USE_PC,                             &
      DEF_FAST_PC,                            &
      DEF_PC_CROP_SPLIT,                      &
      DEF_SOLO_PFT,                           &
      DEF_SUBGRID_SCHEME,                     &

      DEF_LAI_MONTHLY,                        & !add by zhongwang wei @ sysu 2021/12/23
      DEF_NDEP_FREQUENCY,                     & !add by Fang Shang    @ pku  2023/08
      DEF_Interception_scheme,                & !add by zhongwang wei @ sysu 2022/05/23
      DEF_SSP,                                & !add by zhongwang wei @ sysu 2023/02/07

      DEF_LAI_START_YEAR,                     &
      DEF_LAI_END_YEAR,                       &
      DEF_LAI_CHANGE_YEARLY,                  &
      DEF_USE_LAIFEEDBACK,                    & !add by Xingjie Lu, use for updating LAI with leaf carbon
      DEF_USE_IRRIGATION,                     & !add by Hongbin Liang @ sysu
      DEF_IRRIGATION_ALLOCATION,              & !add by Hongbin Liang @ sysu
      DEF_USE_NOSTRESSNITROGEN,               & !add by Hongbin Liang @ sysu
      DEF_RSTFAC,                             & !add by Hongbin Liang @ sysu
      DEF_LC_YEAR,                            &
      DEF_LULCC_SCHEME,                       &

      DEF_URBAN_type_scheme,                  &
      DEF_URBAN_geom_data,                    &
      DEF_URBAN_ONLY,                         &
      DEF_URBAN_RUN,                          & !add by hua yuan, open urban model or not
      DEF_URBAN_BEM,                          & !add by hua yuan, open urban BEM model or not
      DEF_URBAN_TREE,                         & !add by hua yuan, modeling urban tree or not
      DEF_URBAN_WATER,                        & !add by hua yuan, modeling urban water or not
      DEF_URBAN_LUCY,                         &
      DEF_USE_CANYON_HWR,                     &

      DEF_USE_SOILPAR_UPS_FIT,                &
      DEF_THERMAL_CONDUCTIVITY_SCHEME,        &
      DEF_USE_SUPERCOOL_WATER,                &
      DEF_SOIL_REFL_SCHEME,                   &
      DEF_RSS_SCHEME,                         &
      DEF_Runoff_SCHEME,                      &
      DEF_TOPMOD_method,                      &
      DEF_SPLIT_SOILSNOW,                     &
      DEF_VEG_SNOW,                           &
      DEF_file_VIC_para,                      &
      DEF_file_VIC_OPT,                       &
      DEF_VIC_OPT,                            & !add by Qijia Guo @ sysu

      DEF_dir_existing_srfdata,               &
      USE_srfdata_from_larger_region,         &
      USE_srfdata_from_3D_gridded_data,       &

      DEF_USE_CBL_HEIGHT,                     & !add by zhongwang wei @ sysu 2022/12/31
      DEF_USE_PLANTHYDRAULICS,                & !add by xingjie lu @ sysu 2023/05/28
      DEF_USE_MEDLYNST,                       & !add by xingjie lu @ sysu 2023/05/28
      DEF_USE_WUEST,                          & !add by xingjie lu @ sysu 2024/05/28
      DEF_USE_SASU,                           & !add by Xingjie Lu @ sysu 2023/06/27
      DEF_USE_DiagMatrix,                     & !add by Xingjie Lu @ sysu 2023/06/27
      DEF_USE_PN,                             & !add by Xingjie Lu @ sysu 2023/06/27
      DEF_USE_FERT,                           & !add by Xingjie Lu @ sysu 2023/06/27
      DEF_FERT_SOURCE,                        & !add by Hongbin Liang @ sysu
      DEF_USE_NITRIF,                         & !add by Xingjie Lu @ sysu 2023/06/27
      DEF_USE_CNSOYFIXN,                      & !add by Xingjie Lu @ sysu 2023/06/27
      DEF_USE_FIRE,                           & !add by Xingjie Lu @ sysu 2023/06/27

      DEF_USE_Dynamic_Lake,                   & !add by Shupeng Zhang @ sysu 2024/09/12
      DEF_USE_Dynamic_Wetland,                & !add by Shupeng Zhang @ sysu 2026/01/09

      DEF_CheckEquilibrium,                   & !add by Shupeng Zhang @ sysu 2024/11/26
      DEF_Output_2mWMO,                       &

      DEF_LANDONLY,                           &
      DEF_USE_DOMINANT_PATCHTYPE,             &
      DEF_USE_VariablySaturatedFlow,          &
      DEF_USE_BEDROCK,                        &
      DEF_USE_OZONESTRESS,                    &
      DEF_USE_OZONEDATA,                      &
      DEF_USE_SNICAR,                         &
      DEF_Aerosol_Readin,                     &
      DEF_Aerosol_Clim,                       &
      DEF_USE_EstimatedRiverDepth,            &
      DEF_Reservoir_Method,                   &
      DEF_GRIDBASED_ROUTING_MAX_DT,           &

      DEF_USE_SEDIMENT,                       &
      DEF_SED_LAMBDA,                         &
      DEF_SED_LYRDPH,                         &
      DEF_SED_DENSITY,                        &
      DEF_SED_WATER_DENSITY,                  &
      DEF_SED_VISKIN,                         &
      DEF_SED_VONKAR,                         &
      DEF_SED_PSET,                           &
      DEF_SED_TOTLYRNUM,                      &
      DEF_SED_CFL_ADV,                        &
      DEF_SED_IGNORE_DPH,                     &
      DEF_SED_DT_MAX,                         &
      DEF_SED_DIAMETER,                       &
      DEF_SED_PYLD,                           &
      DEF_SED_PYLDC,                          &
      DEF_SED_PYLDPC,                         &
      DEF_SED_DSYLUNIT,                       &

      DEF_precip_phase_discrimination_scheme, &

      DEF_USE_SoilInit,                       &
      DEF_file_SoilInit,                      &

      DEF_HighResSoil,                        &
      DEF_HighResVeg,                         &
      DEF_PROSPECT,                           &
      DEF_HighResUrban_albedo,                &
      ! DEF_Satellite_Params,                   &
      ! DEF_file_soiloptics,                    &
      ! DEF_file_satellite_params,              &
      ! DEF_sla_varname,                        &

      DEF_USE_SnowInit,                       &
      DEF_file_SnowInit,                      &

      DEF_USE_CN_INIT,                        &
      DEF_file_cn_init,                       &

      DEF_USE_WaterTableInit,                 &
      DEF_file_WaterTable,                    &

      DEF_file_snowoptics,                    &
      DEF_file_snowaging ,                    &

      DEF_ElementNeighbour_file,              &
      DEF_UnitCatchment_file,                 &
      DEF_ReservoirPara_file,                 &

      DEF_DA_obsdir,                          &
      DEF_DA_TWS,                             &
      DEF_DA_TWS_GRACE,                       &
      DEF_DA_SM,                              &
      DEF_DA_ENS_NUM,                         &
      DEF_DA_ENS_SM,                          &
      DEF_DA_SM_SMAP,                         &
      DEF_DA_SM_FY,                           &
      DEF_DA_SM_SYNOP,                        &
      DEF_DA_RTM_diel,                        &
      DEF_DA_RTM_rough,                       &

      DEF_Optimize_Baseflow,                  &

      DEF_forcing_namelist,                   &

      DEF_Forcing_Interp_Method,              &

      DEF_USE_Forcing_Downscaling,            &
      DEF_USE_Forcing_Downscaling_Simple,     &
      DEF_DS_HiresTopographyDataDir,          &
      DEF_DS_precipitation_adjust_scheme,     &
      DEF_DS_longwave_adjust_scheme,          &
      DEF_USE_ClimForcing_for_Spinup,         &

      DEF_HISTORY_IN_VECTOR,                  &
      DEF_HIST_lon_res,                       &
      DEF_HIST_lat_res,                       &
      DEF_HIST_grid_as_forcing,               &
      DEF_WRST_FREQ,                          &
      DEF_HIST_FREQ,                          &
      DEF_HIST_groupby,                       &
      DEF_HIST_mode,                          &
      DEF_HIST_WriteBack,                     &
      DEF_REST_CompressLevel,                 &
      DEF_HIST_CompressLevel,                 &
      DEF_HIST_vars_namelist,                 &
      DEF_HIST_vars_out_default

   namelist /nl_colm_forcing/ DEF_dir_forcing, DEF_forcing
   namelist /nl_colm_history/ DEF_hist_vars

      ! ----- open the namelist file -----
      IF (mpas_is_root) THEN

         open(10, status='OLD', file=nlfile, form="FORMATTED")
         read(10, nml=nl_colm, iostat=ierr, iomsg=iomesg)
         IF (ierr /= 0) THEN
            write(*,*) 'ERROR in ', trim(nlfile), ' : ', trim(iomesg)
            CALL CoLM_Stop (' ***** ERROR: Problem reading namelist: '// trim(nlfile))
         ENDIF
         close(10)

         IF (DEF_RSTFAC < 1 .or. DEF_RSTFAC > 2) THEN
            write(*,'(A,I0,A)') 'ERROR: DEF_RSTFAC is ', DEF_RSTFAC, '; supported values are 1-2.'
            CALL CoLM_stop ()
         ENDIF

         IF (DEF_THERMAL_CONDUCTIVITY_SCHEME < 1 .or. DEF_THERMAL_CONDUCTIVITY_SCHEME > 8) THEN
            write(*,'(A,I0,A)') 'ERROR: DEF_THERMAL_CONDUCTIVITY_SCHEME is ', &
               DEF_THERMAL_CONDUCTIVITY_SCHEME, '; supported values are 1-8.'
            CALL CoLM_stop ()
         ENDIF

         IF (DEF_Runoff_SCHEME < 0 .or. DEF_Runoff_SCHEME > 3) THEN
            write(*,'(A,I0,A)') 'ERROR: DEF_Runoff_SCHEME is ', DEF_Runoff_SCHEME, &
               '; supported values are 0-3.'
            CALL CoLM_stop ()
         ENDIF

         IF (DEF_Interception_scheme < 1 .or. DEF_Interception_scheme > 7) THEN
            write(*,'(A,I0,A)') 'ERROR: DEF_Interception_scheme is ', DEF_Interception_scheme, &
               '; supported values are 1-7; scheme 8 is not complete.'
            CALL CoLM_stop ()
         ENDIF

#ifndef MPAS_EMBEDDED_COLM
	         open(10, status='OLD', file=trim(DEF_forcing_namelist), form="FORMATTED")
	         read(10, nml=nl_colm_forcing, iostat=ierr, iomsg=iomesg)
	         IF (ierr /= 0) THEN
	            write(*,*) 'ERROR in ', trim(DEF_forcing_namelist), ' : ', trim(iomesg)
	            CALL CoLM_Stop (' ***** ERROR: Problem reading namelist: '// trim(DEF_forcing_namelist))
	         ENDIF
	         close(10)

	         IF (trim(DEF_forcing%dataset) == 'POINT') THEN
	            DEF_forcing%has_missing_value = .false.
	         ENDIF
#endif

         DEF_dir_landdata = trim(DEF_dir_output) // '/' // trim(adjustl(DEF_CASE_NAME)) // '/landdata'
         DEF_dir_restart  = trim(DEF_dir_output) // '/' // trim(adjustl(DEF_CASE_NAME)) // '/restart'
         DEF_dir_history  = trim(DEF_dir_output) // '/' // trim(adjustl(DEF_CASE_NAME)) // '/history'

	         CALL make_directory(DEF_dir_output)
#ifndef MPAS_EMBEDDED_COLM
	         CALL make_directory(DEF_dir_landdata)
#endif
	         CALL make_directory(DEF_dir_restart)
#ifndef MPAS_EMBEDDED_COLM
	         CALL make_directory(DEF_dir_history)
#endif

#ifdef SinglePoint
         DEF_nx_blocks = 360
         DEF_ny_blocks = 180
         DEF_HIST_mode = 'one'
#endif

#ifndef MPAS_EMBEDDED_COLM
         IF (DEF_simulation_time%timestep > 3600.) THEN
            write(*,*) '                  *****                  '
            write(*,*) 'Warning: timestep should be less than or equal to 3600 seconds.'
            CALL CoLM_Stop ()
         ENDIF
#endif

! ===============================================================
! ----- Macros&Namelist conflicts and dependency management -----
! ===============================================================


! ----- SOIL model related ------ Macros&Namelist conflicts and dependency management
#if (defined vanGenuchten_Mualem_SOIL_MODEL)
         write(*,*) '                  *****                  '
         write(*,*) 'Note: DEF_USE_VariablySaturatedFlow is automaticlly set to .true.  '
         write(*,*) 'when using vanGenuchten_Mualem_SOIL_MODEL. '
         DEF_USE_VariablySaturatedFlow = .true.
#endif
#if (defined CatchLateralFlow)
         write(*,*) '                  *****                  '
         write(*,*) 'Note: DEF_USE_VariablySaturatedFlow is automaticlly set to .true.  '
         write(*,*) 'when defined CatchLateralFlow. '
         DEF_USE_VariablySaturatedFlow = .true.
#endif
#ifdef SinglePoint
         IF (DEF_Runoff_SCHEME == 0) THEN
            write(*,*) 'Note: DEF_TOPMOD_method is set to 0 in SinglePoint.'
            DEF_TOPMOD_method = 0
         ENDIF
#endif

         IF (DEF_Runoff_SCHEME == 1) THEN
            DEF_file_VIC_para = trim(DEF_dir_runtime)//'/vic/vic_para.txt'
            IF (DEF_VIC_OPT) THEN
               DEF_file_VIC_OPT = trim(DEF_dir_runtime)//'vic/vic_para.nc'
            ENDIF
         ENDIF

! ----- subgrid type related ------ Macros&Namelist conflicts and dependency management

#if (defined LULC_USGS || defined LULC_IGBP)
         DEF_USE_LCT  = .true.
         DEF_USE_PFT  = .false.
         DEF_USE_PC   = .false.
         DEF_FAST_PC  = .false.
         DEF_SOLO_PFT = .false.
#ifdef vanGenuchten_Mualem_SOIL_MODEL
         write(*,*) '                  *****                  '
         write(*,*) 'Note: Soil resistance is automaticlly turned off for VG soil + USGS|IGBP scheme.'
         DEF_RSS_SCHEME = 0
#endif
#endif

#ifdef LULC_IGBP_PFT
         DEF_USE_LCT  = .false.
         DEF_USE_PFT  = .true.
         DEF_USE_PC   = .false.
         DEF_FAST_PC  = .false.
#endif

#ifdef LULC_IGBP_PC
         DEF_USE_LCT  = .false.
         DEF_USE_PFT  = .false.
         DEF_USE_PC   = .true.
         DEF_SOLO_PFT = .false.
#endif

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
         IF (.not.DEF_LAI_MONTHLY) THEN
            write(*,*) '                  *****                  '
            write(*,*) 'Warning: 8-day LAI data is not supported for '
            write(*,*) 'LULC_IGBP_PFT and LULC_IGBP_PC.'
            write(*,*) 'Changed to monthly data, set DEF_LAI_MONTHLY = .true.'
            DEF_LAI_MONTHLY = .true.
         ENDIF
#endif


! ----- BGC and CROP model related ------ Macros&Namelist conflicts and dependency management

#ifndef BGC
         IF(DEF_USE_LAIFEEDBACK)THEN
            DEF_USE_LAIFEEDBACK = .false.
            write(*,*) '                  *****                  '
            write(*,*) 'Warning: LAI feedback is not supported for BGC off.'
            write(*,*) 'DEF_USE_LAIFEEDBACK is set to false automatically when BGC is turned off.'
         ENDIF

         IF(DEF_USE_SASU)THEN
            DEF_USE_SASU = .false.
            write(*,*) '                  *****                  '
            write(*,*) 'Warning: Semi-Analytic Spin-up is on when BGC is off.'
            write(*,*) 'DEF_USE_SASU is set to false automatically when BGC is turned off.'
         ENDIF

         IF(DEF_USE_DiagMatrix)THEN
            DEF_USE_DiagMatrix = .false.
            write(*,*) '                  *****                  '
            write(*,*) 'Warning: CN Matrix Diagnostic is on when BGC is off.'
            write(*,*) 'DEF_USE_DiagMatrix is set to false automatically when BGC is turned off.'
         ENDIF

         IF(DEF_USE_PN)THEN
            DEF_USE_PN = .false.
            write(*,*) '                  *****                  '
            write(*,*) 'Warning: Punctuated nitrogen addition spin up is on when BGC is off.'
            write(*,*) 'DEF_USE_PN is set to false automatically when BGC is turned off.'
         ENDIF

         IF(DEF_USE_NITRIF)THEN
            DEF_USE_NITRIF = .false.
            write(*,*) '                  *****                  '
            write(*,*) 'Warning: Nitrification-Denitrification is on when BGC is off.'
            write(*,*) 'DEF_USE_NITRIF is set to false automatically when BGC is turned off.'
         ENDIF

         IF(DEF_USE_FIRE)THEN
            DEF_USE_FIRE = .false.
            write(*,*) '                  *****                  '
            write(*,*) 'Warning: Fire model is on when BGC is off.'
            write(*,*) 'DEF_USE_FIRE is set to false automatically when BGC is turned off.'
         ENDIF
#endif

#ifndef CROP
         IF(DEF_USE_FERT)THEN
            DEF_USE_FERT = .false.
            write(*,*) '                  *****                  '
            write(*,*) 'Warning: Fertilization is on when CROP is off.'
            write(*,*) 'DEF_USE_FERT is set to false automatically when CROP is turned off.'
         ENDIF

         IF(DEF_USE_CNSOYFIXN)THEN
            DEF_USE_CNSOYFIXN = .false.
            write(*,*) '                  *****                  '
            write(*,*) 'Warning: Soy nitrogen fixation is on when CROP is off.'
            write(*,*) 'DEF_USE_CNSOYFIXN is set to false automatically when CROP is turned off.'
         ENDIF

         IF(DEF_USE_IRRIGATION)THEN
            DEF_USE_IRRIGATION = .false.
            write(*,*) '                  *****                  '
            write(*,*) 'Warning: irrigation is on when CROP is off.'
            write(*,*) 'DEF_USE_IRRIGATION is set to false automatically when CROP is turned off.'
         ENDIF
#endif

         IF(.not.(DEF_FERT_SOURCE == 1 .or. DEF_FERT_SOURCE == 2))THEN
            write(*,*) '                  *****                  '
            write(*,'(A,I0,A)') 'ERROR: DEF_FERT_SOURCE is ',DEF_FERT_SOURCE,' , should only = 1 or 2'
            CALL CoLM_stop ()
         ENDIF

         IF(.not. DEF_USE_OZONESTRESS)THEN
            IF(DEF_USE_OZONEDATA)THEN
               DEF_USE_OZONEDATA = .false.
               write(*,*) '                  *****                  '
               write(*,*) 'Warning: DEF_USE_OZONEDATA is not supported for OZONESTRESS off.'
               write(*,*) 'DEF_USE_OZONEDATA is set to false automatically.'
            ENDIF
         ENDIF

         IF(DEF_USE_MEDLYNST)THEN
            IF(DEF_USE_WUEST)THEN
                DEF_USE_MEDLYNST = .false.
                DEF_USE_WUEST    = .false.
                write(*,*) '                  *****                  '
                write(*,*) 'Warning: configure conflict, both DEF_USE_MEDLYNST and DEF_USE_WUEST were set true.'
                write(*,*) 'set both DEF_USE_MEDLYNST and DEF_USE_WUEST to false.'
            ENDIF
         ENDIF

! ----- SNICAR model ------ Macros&Namelist conflicts and dependency management

         DEF_file_snowoptics = trim(DEF_dir_runtime)//'/snicar/snicar_optics_5bnd_mam_c211006.nc'
         DEF_file_snowaging  = trim(DEF_dir_runtime)//'/snicar/snicar_drdt_bst_fit_60_c070416.nc'

         IF (.not. DEF_USE_SNICAR) THEN
            IF (DEF_Aerosol_Readin) THEN
               DEF_Aerosol_Readin = .false.
               write(*,*) '                  *****                  '
               write(*,*) 'Warning: DEF_Aerosol_Readin is not needed for DEF_USE_SNICAR off. '
               write(*,*) 'DEF_Aerosol_Readin is set to false automatically.'
            ENDIF
         ENDIF


! ----- Urban model ----- Macros&Namelist conflicts and dependency management

#ifdef URBAN_MODEL
         DEF_URBAN_RUN = .true.

         write(*,*) '                  *****                  '
         write(*,*) 'When URBAN model is opened, WUEST/SUPERCOOL_WATER/PLANTHYDRAULICS/OZONESTRESS/SOILSNOW'
         write(*,*) 'will be set to false automatically for simplicity.'
         DEF_USE_WUEST           = .false.
         DEF_USE_SUPERCOOL_WATER = .false.
         DEF_USE_PLANTHYDRAULICS = .false.
         DEF_USE_OZONESTRESS     = .false.
         DEF_USE_OZONEDATA       = .false.
         DEF_SPLIT_SOILSNOW      = .false.
#else
         IF (DEF_URBAN_RUN) THEN
            write(*,*) '                  *****                  '
            write(*,*) 'Note: The Urban model is not opened. IF you want to run Urban model '
            write(*,*) 'please #define URBAN_MODEL in define.h. otherwise DEF_URBAN_RUN will '
            write(*,*) 'be set to false automatically.'
            DEF_URBAN_RUN = .false.
         ENDIF
#endif


! ----- LULCC ----- Macros&Namelist conflicts and dependency management

#ifdef LULCC

         write(*,*) '                  *****                  '
         write(*,*) 'Warning: The LULCC data is provided for years 2000 to 2020 right now! '
         write(*,*) 'Please make sure the year range you set is suitable. '

#if (defined LULC_USGS || defined BGC)
         write(*,*) '                  *****                  '
         write(*,*) 'Fatal ERROR: LULCC is not supported for LULC_USGS/BGC at present. STOP! '
         CALL CoLM_stop ()
#endif
         IF (.not.DEF_LAI_MONTHLY) THEN
            write(*,*) '                  *****                  '
            write(*,*) 'Note: When LULCC is opened, DEF_LAI_MONTHLY '
            write(*,*) 'will be set to true automatically.'
            DEF_LAI_MONTHLY = .true.
         ENDIF

         IF (.not.DEF_LAI_CHANGE_YEARLY) THEN
            write(*,*) '                  *****                  '
            write(*,*) 'Note: When LULCC is opened, DEF_LAI_CHANGE_YEARLY '
            write(*,*) 'will be set to true automatically.'
            DEF_LAI_CHANGE_YEARLY = .true.
         ENDIF

#if (defined LULC_IGBP_PC || defined URBAN)
         !write(*,*) '                  *****                  '
         !write(*,*) 'Fatal ERROR: LULCC is not supported for LULC_IGBP_PC/URBAN at present. STOP! '
         !write(*,*) 'It is coming soon. '
         !CALL CoLM_stop ()
         ![update] 24/10/2023: right now IGBP/PFT/PC and Urban are all supported.
#endif

#if (defined SinglePoint)
         write(*,*) '                  *****                  '
         write(*,*) 'Fatal ERROR: LULCC is not supported for Single Point run at present. STOP! '
         write(*,*) 'It will come later. '
         CALL CoLM_stop ()
#endif

#endif

#if (defined DEF_LAI_CHANGE_YEARLY)
         write(*,*) '                  *****                  '
         write(*,*) 'Warning: The LAI data is provided for years 2000 to 2020 right now! '
         write(*,*) 'Any year before 2000 or after 2020 will be treated as 2000 or 2020. '
#endif


! ----- single point run ----- Macros&Namelist conflicts and dependency management

#if (defined SinglePoint)
#ifdef SrfdataDiag
         write(*,*) '                  *****                  '
         write(*,*) 'Surface data diagnose is closed in SinglePoint case.'
#undef SrfdataDiag
#endif
         IF (trim(DEF_Forcing_Interp_Method) == 'bilinear') THEN
            DEF_Forcing_Interp_Method = 'arealweight'
            write(*,*) '                  *****                  '
            write(*,*) 'Bilinear interpolation is not supported in SinglePoint case.'
         ENDIF
#endif

! ----- Soil water and temperature Initialization ----- Namelist conflicts

         IF (DEF_USE_SoilInit .and. DEF_USE_WaterTableInit) THEN
            write(*,*) '                  *****                  '
            write(*,*) 'If both DEF_USE_SoilInit and DEF_USE_WaterTableInit are .TRUE.,   '
            write(*,*) 'initial value of water table depth is read from DEF_file_SoilInit,'
            write(*,*) 'instead of DEF_file_WaterTable (which is useless in this CASE).   '
         ENDIF

! ----- dynamic lake run ----- Macros&Namelist conflicts and dependency management

#ifndef CATCHMENT
         IF ((.not. DEF_USE_VariablySaturatedFlow) .and. DEF_USE_Dynamic_Lake) THEN
            DEF_USE_Dynamic_Lake = .false.
            write(*,*) '                  *****                  '
            write(*,*) 'Warning: Dynamic Lake is closed if variably saturated flow algorithm is not used.'
         ENDIF
         IF (DEF_USE_Dynamic_Lake) THEN
            write(*,*) '                  *****                  '
            write(*,*) 'Warning: Dynamic Lake is not well supported without lateral flow.'
         ENDIF
#else
         DEF_USE_Dynamic_Lake = .true.
         write(*,*) '                  *****                  '
         write(*,*) 'Warning: Dynamic Lake is used if CATCHMENT-based lateral flow used.'
#endif

! ----- 2m WMO temperature ---- Macros&Namelist conflicts and dependency management

#if !defined(GRIDBASED) || (defined  LULC_IGBP || defined LULC_USGS)
         IF (DEF_Output_2mWMO) THEN
            DEF_Output_2mWMO = .false.
            write(*,*) '                  *****                  '
            write(*,*) 'Warning: 2m WMO temperature is not well supported for IGBP and USGS'
            write(*,*) 'DEF_Output_2mWMO will be set to false automatically.'
         ENDIF
#endif

! ----- [Complement IF needed] ----- Macros&Namelist conflicts and dependency management


! -----END Macros&Namelist conflicts and dependency management -----
! ===============================================================


      ENDIF


#ifdef MPAS_MPI
      CALL mpi_bcast (DEF_Output_2mWMO                       ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_CASE_NAME                          ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_domain%edges                       ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_domain%edgen                       ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_domain%edgew                       ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_domain%edgee                       ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_BlockInfoFile                      ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_AverageElementSize                 ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_nx_blocks                          ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_ny_blocks                          ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_simulation_time%greenwich          ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_simulation_time%start_year         ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_simulation_time%start_month        ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_simulation_time%start_day          ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_simulation_time%start_sec          ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_simulation_time%end_year           ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_simulation_time%end_month          ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_simulation_time%end_day            ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_simulation_time%end_sec            ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_simulation_time%spinup_year        ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_simulation_time%spinup_month       ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_simulation_time%spinup_day         ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_simulation_time%spinup_sec         ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_simulation_time%spinup_repeat      ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_simulation_time%timestep           ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_dir_rawdata                        ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_dir_runtime                        ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
	      CALL mpi_bcast (DEF_dir_output             ,len(DEF_dir_output)  ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_dir_forcing                        ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_dir_landdata                       ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
	      CALL mpi_bcast (DEF_dir_restart            ,len(DEF_dir_restart) ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_dir_history                        ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

#if (defined GRIDBASED || defined UNSTRUCTURED)
      CALL mpi_bcast (DEF_file_mesh                          ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_GRIDBASED_lon_res                  ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_GRIDBASED_lat_res                  ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
#endif

#ifdef CATCHMENT
      CALL mpi_bcast (DEF_CatchmentMesh_data                 ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
#endif

      CALL mpi_bcast (DEF_dir_existing_srfdata               ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (USE_srfdata_from_larger_region         ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (USE_srfdata_from_3D_gridded_data       ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      ! 07/2023, added by yuan: subgrid setting related
      CALL mpi_bcast (DEF_USE_LCT                            ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_USE_PFT                            ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_USE_PC                             ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_FAST_PC                            ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_PC_CROP_SPLIT                      ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_SOLO_PFT                           ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_SUBGRID_SCHEME                     ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_LAI_START_YEAR                     ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_LAI_END_YEAR                       ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_LAI_CHANGE_YEARLY                  ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      ! 05/2023, added by Xingjie lu
      CALL mpi_bcast (DEF_USE_LAIFEEDBACK                    ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_USE_IRRIGATION                     ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_IRRIGATION_ALLOCATION              ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_USE_NOSTRESSNITROGEN               ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_RSTFAC                             ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      ! LULC related
      CALL mpi_bcast (DEF_LC_YEAR                            ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_LULCC_SCHEME                       ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_URBAN_type_scheme                  ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_URBAN_geom_data                    ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      ! 05/2023, added by yuan
      CALL mpi_bcast (DEF_URBAN_ONLY                         ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_URBAN_RUN                          ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_URBAN_BEM                          ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_URBAN_TREE                         ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_URBAN_WATER                        ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_URBAN_LUCY                         ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_USE_CANYON_HWR                     ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      ! 06/2023, added by weinan
      CALL mpi_bcast (DEF_USE_SOILPAR_UPS_FIT                ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_THERMAL_CONDUCTIVITY_SCHEME        ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_USE_SUPERCOOL_WATER                ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      ! 06/2023, added by hua yuan
      CALL mpi_bcast (DEF_SOIL_REFL_SCHEME                   ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      ! 07/2023, added by zhuo liu
      CALL mpi_bcast (DEF_RSS_SCHEME                         ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      ! 02/2024, added by Shupeng Zhang
      CALL mpi_bcast (DEF_Runoff_SCHEME                      ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_VIC_OPT                            ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_file_VIC_para                      ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_file_VIC_OPT                       ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_TOPMOD_method                      ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      ! 08/2023, added by hua yuan
      CALL mpi_bcast (DEF_SPLIT_SOILSNOW                     ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_VEG_SNOW                           ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_LAI_MONTHLY                        ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_NDEP_FREQUENCY                     ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_Interception_scheme                ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_SSP                                ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_USE_CBL_HEIGHT                     ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_USE_PLANTHYDRAULICS                ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_USE_MEDLYNST                       ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_USE_WUEST                          ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_USE_SASU                           ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_USE_DiagMatrix                     ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_USE_PN                             ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_USE_FERT                           ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_FERT_SOURCE                        ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_USE_NITRIF                         ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_USE_CNSOYFIXN                      ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_USE_FIRE                           ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_USE_Dynamic_Lake                   ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_USE_Dynamic_Wetland                ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_CheckEquilibrium                   ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_LANDONLY                           ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_USE_DOMINANT_PATCHTYPE             ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_USE_VariablySaturatedFlow          ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_USE_BEDROCK                        ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_USE_OZONESTRESS                    ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_USE_OZONEDATA                      ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_precip_phase_discrimination_scheme ,5   ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_USE_SoilInit                       ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_file_SoilInit                      ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_HighResSoil                        ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_HighResVeg                         ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_PROSPECT                           ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_HighResUrban_albedo                ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_USE_SnowInit                       ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_file_SnowInit                      ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_USE_CN_INIT                        ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_file_cn_init                       ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_USE_WaterTableInit                 ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_file_WaterTable                    ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_USE_SNICAR                         ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_file_snowoptics                    ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_file_snowaging                     ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_ElementNeighbour_file              ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_UnitCatchment_file                 ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_ReservoirPara_file                 ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_DA_obsdir                          ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_DA_TWS                             ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_DA_TWS_GRACE                       ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_DA_SM                              ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_DA_ENS_NUM                         ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_DA_ENS_SM                          ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_DA_SM_SMAP                         ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_DA_SM_FY                           ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_DA_SM_SYNOP                        ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_DA_RTM_diel                        ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_DA_RTM_rough                       ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_Optimize_Baseflow                  ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_Aerosol_Readin                     ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_Aerosol_Clim                       ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_USE_EstimatedRiverDepth            ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_Reservoir_Method                   ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_GRIDBASED_ROUTING_MAX_DT           ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_USE_SEDIMENT                       ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_SED_LAMBDA                         ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_SED_LYRDPH                         ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_SED_DENSITY                        ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_SED_WATER_DENSITY                  ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_SED_VISKIN                         ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_SED_VONKAR                         ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_SED_PSET                           ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_SED_TOTLYRNUM                      ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_SED_CFL_ADV                        ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_SED_IGNORE_DPH                     ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_SED_DT_MAX                         ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_SED_DIAMETER                       ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_SED_PYLD                           ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_SED_PYLDC                          ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_SED_PYLDPC                         ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_SED_DSYLUNIT                       ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_HISTORY_IN_VECTOR                  ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_HIST_lon_res                       ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_HIST_lat_res                       ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_HIST_grid_as_forcing               ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      CALL mpi_bcast (DEF_WRST_FREQ                          ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_HIST_FREQ                          ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_HIST_groupby                       ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_HIST_mode                          ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_HIST_WriteBack                     ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_REST_CompressLevel                 ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_HIST_CompressLevel                 ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

	      CALL mpi_bcast (DEF_Forcing_Interp_Method              ,20  ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_USE_Forcing_Downscaling            ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_USE_Forcing_Downscaling_Simple     ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_DS_HiresTopographyDataDir          ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_DS_precipitation_adjust_scheme     ,5   ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_DS_longwave_adjust_scheme          ,5   ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
	      CALL mpi_bcast (DEF_USE_ClimForcing_for_Spinup         ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

#ifndef MPAS_EMBEDDED_COLM
	      CALL mpi_bcast (DEF_forcing%dataset                    ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%solarin_all_band           ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%HEIGHT_mode                ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%HEIGHT_V                   ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%HEIGHT_T                   ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%HEIGHT_Q                   ,1   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%regional                   ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%regbnd                     ,4   ,mpi_real8     ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%has_missing_value          ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%missing_value_name         ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%NVAR                       ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%startyr                    ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%startmo                    ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%endyr                      ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%endmo                      ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%dtime                      ,8   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%offset                     ,8   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%nlands                     ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%leapyear                   ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%data2d                     ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%hightdim                   ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%dim2d                      ,1   ,mpi_logical   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%latname                    ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%lonname                    ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%groupby                    ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)

      DO ivar = 1, 8
         CALL mpi_bcast (DEF_forcing%fprefix(ivar)           ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
         CALL mpi_bcast (DEF_forcing%vname(ivar)             ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
         CALL mpi_bcast (DEF_forcing%timelog(ivar)           ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
         CALL mpi_bcast (DEF_forcing%tintalgo(ivar)          ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      ENDDO
      CALL mpi_bcast (DEF_forcing%CBL_fprefix                ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%CBL_vname                  ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%CBL_tintalgo               ,256 ,mpi_character ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
      CALL mpi_bcast (DEF_forcing%CBL_dtime                  ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
	      CALL mpi_bcast (DEF_forcing%CBL_offset                 ,1   ,mpi_integer   ,mpas_root ,mpas_comm ,mpas_mpi_ierr)
#endif
      CALL mpas_mpi_check('CoLM namelist broadcast')
#endif

#ifndef MPAS_EMBEDDED_COLM
	      CALL sync_hist_vars (set_defaults = .true.)

      IF (mpas_is_root) THEN

         inquire (file=trim(DEF_HIST_vars_namelist), exist=fexists)
         IF (.not. fexists) THEN
            write(*,*) 'History namelist file: ', trim(DEF_HIST_vars_namelist), ' does not exist.'
         ELSE
            open(10, status='OLD', file=trim(DEF_HIST_vars_namelist), form="FORMATTED")
            read(10, nml=nl_colm_history, iostat=ierr, iomsg=iomesg)
            IF (ierr /= 0) THEN
               write(*,*) 'ERROR in ', trim(DEF_HIST_vars_namelist), ' : ', trim(iomesg)
               CALL CoLM_Stop (' ***** ERROR: Problem reading namelist: ' &
                  // trim(DEF_HIST_vars_namelist))
            ENDIF
            close(10)
         ENDIF

         IF(DEF_USE_DiagMatrix)THEN
            DEF_hist_vars%leafcCap                         = .true.
            DEF_hist_vars%leafc_storageCap                 = .true.
            DEF_hist_vars%leafc_xferCap                    = .true.
            DEF_hist_vars%frootcCap                        = .true.
            DEF_hist_vars%frootc_storageCap                = .true.
            DEF_hist_vars%frootc_xferCap                   = .true.
            DEF_hist_vars%livestemcCap                     = .true.
            DEF_hist_vars%livestemc_storageCap             = .true.
            DEF_hist_vars%livestemc_xferCap                = .true.
            DEF_hist_vars%deadstemcCap                     = .true.
            DEF_hist_vars%deadstemc_storageCap             = .true.
            DEF_hist_vars%deadstemc_xferCap                = .true.
            DEF_hist_vars%livecrootcCap                    = .true.
            DEF_hist_vars%livecrootc_storageCap            = .true.
            DEF_hist_vars%livecrootc_xferCap               = .true.
            DEF_hist_vars%deadcrootcCap                    = .true.
            DEF_hist_vars%deadcrootc_storageCap            = .true.
            DEF_hist_vars%deadcrootc_xferCap               = .true.
            DEF_hist_vars%leafnCap                         = .true.
            DEF_hist_vars%leafn_storageCap                 = .true.
            DEF_hist_vars%leafn_xferCap                    = .true.
            DEF_hist_vars%frootnCap                        = .true.
            DEF_hist_vars%frootn_storageCap                = .true.
            DEF_hist_vars%frootn_xferCap                   = .true.
            DEF_hist_vars%livestemnCap                     = .true.
            DEF_hist_vars%livestemn_storageCap             = .true.
            DEF_hist_vars%livestemn_xferCap                = .true.
            DEF_hist_vars%deadstemnCap                     = .true.
            DEF_hist_vars%deadstemn_storageCap             = .true.
            DEF_hist_vars%deadstemn_xferCap                = .true.
            DEF_hist_vars%livecrootnCap                    = .true.
            DEF_hist_vars%livecrootn_storageCap            = .true.
            DEF_hist_vars%livecrootn_xferCap               = .true.
            DEF_hist_vars%deadcrootnCap                    = .true.
            DEF_hist_vars%deadcrootn_storageCap            = .true.
            DEF_hist_vars%deadcrootn_xferCap               = .true.
            DEF_hist_vars%t_scalar                         = .true.
            DEF_hist_vars%w_scalar                         = .true.

            DEF_hist_vars%litr1cCap_vr                     = .true.
            DEF_hist_vars%litr2cCap_vr                     = .true.
            DEF_hist_vars%litr3cCap_vr                     = .true.
            DEF_hist_vars%soil1cCap_vr                     = .true.
            DEF_hist_vars%soil2cCap_vr                     = .true.
            DEF_hist_vars%soil3cCap_vr                     = .true.
            DEF_hist_vars%cwdcCap_vr                       = .true.
            DEF_hist_vars%litr1nCap_vr                     = .true.
            DEF_hist_vars%litr2nCap_vr                     = .true.
            DEF_hist_vars%litr3nCap_vr                     = .true.
            DEF_hist_vars%soil1nCap_vr                     = .true.
            DEF_hist_vars%soil2nCap_vr                     = .true.
            DEF_hist_vars%soil3nCap_vr                     = .true.
            DEF_hist_vars%cwdnCap_vr                       = .true.
         ENDIF
      ENDIF

	      CALL sync_hist_vars (set_defaults = .false.)
#endif

   END SUBROUTINE read_namelist

   ! ---------------
   SUBROUTINE sync_hist_vars (set_defaults)

   IMPLICIT NONE

   logical, intent(in) :: set_defaults

      CALL sync_hist_vars_one (DEF_hist_vars%xy_us       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%xy_vs       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%xy_t        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%xy_q        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%xy_prc      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%xy_prl      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%xy_pbot     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%xy_frl      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%xy_solarin  , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%xy_rain     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%xy_snow     , set_defaults)

      CALL sync_hist_vars_one (DEF_hist_vars%xy_hpbl     , set_defaults)

      CALL sync_hist_vars_one (DEF_hist_vars%taux        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%tauy        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%fsena       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%lfevpa      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%fevpa       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%fsenl       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%fevpl       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%etr         , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%fseng       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%fevpg       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%fgrnd       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%sabvsun     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%sabvsha     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%sabg        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%olrg        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%rnet        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%xerr        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%zerr        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%rsur        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%rsur_se     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%rsur_ie     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%rsub        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%rnof        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%xwsur       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%xwsub       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%fldarea     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%qintr       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%qinfl       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%qdrip       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%wat         , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%wat_inst    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%wetwat      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%wetwat_inst , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%wetzwt      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%assim       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%respc       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%qcharge     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%t_grnd      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%tleaf       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%ldew        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%scv         , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%snowdp      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%fsno        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%frcsat      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%sigf        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%green       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%lai         , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%laisun      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%laisha      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%sai         , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%alb         , set_defaults)
#ifdef HYPERSPECTRAL
      CALL sync_hist_vars_one (DEF_hist_vars%alb_hires   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%reflectance_out   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%transmittance_out , set_defaults)
#endif
      CALL sync_hist_vars_one (DEF_hist_vars%emis        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%z0m         , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%trad        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%rss         , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%tref        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%t2m_wmo     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%qref        , set_defaults)
#ifdef URBAN_MODEL
      CALL sync_hist_vars_one (DEF_hist_vars%fsen_roof   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%fsen_wsun   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%fsen_wsha   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%fsen_gimp   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%fsen_gper   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%fsen_urbl   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%lfevp_roof  , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%lfevp_gimp  , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%lfevp_gper  , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%lfevp_urbl  , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%fhac        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%fwst        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%fach        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%fhah        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%meta        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%vehc        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%t_room      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%tafu        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%t_roof      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%t_wall      , set_defaults)
#endif
      CALL sync_hist_vars_one (DEF_hist_vars%assimsun    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%assimsha    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%etrsun      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%etrsha      , set_defaults)
#ifdef BGC
      CALL sync_hist_vars_one (DEF_hist_vars%leafc              , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%leafc_storage      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%leafc_xfer         , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%frootc             , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%frootc_storage     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%frootc_xfer        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%livestemc          , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%livestemc_storage  , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%livestemc_xfer     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%deadstemc          , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%deadstemc_storage  , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%deadstemc_xfer     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%livecrootc         , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%livecrootc_storage , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%livecrootc_xfer    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%deadcrootc         , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%deadcrootc_storage , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%deadcrootc_xfer    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%grainc             , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%grainc_storage     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%grainc_xfer        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%leafn              , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%leafn_storage      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%leafn_xfer         , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%frootn             , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%frootn_storage     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%frootn_xfer        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%livestemn          , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%livestemn_storage  , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%livestemn_xfer     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%deadstemn          , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%deadstemn_storage  , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%deadstemn_xfer     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%livecrootn         , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%livecrootn_storage , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%livecrootn_xfer    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%deadcrootn         , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%deadcrootn_storage , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%deadcrootn_xfer    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%grainn             , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%grainn_storage     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%grainn_xfer        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%retrasn            , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%gpp                , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%downreg            , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%ar                 , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cwdprod            , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cwddecomp          , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%hr                 , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%fpg                , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%fpi                , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%totvegc            , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%totlitc            , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%totcwdc            , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%totsomc            , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%totcolc            , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%totvegn            , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%totlitn            , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%totcwdn            , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%totsomn            , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%totcoln            , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%totsoiln_vr        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%gpp_enftemp        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%gpp_enfboreal      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%gpp_dnfboreal      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%gpp_ebftrop        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%gpp_ebftemp        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%gpp_dbftrop        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%gpp_dbftemp        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%gpp_dbfboreal      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%gpp_ebstemp        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%gpp_dbstemp        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%gpp_dbsboreal      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%gpp_c3arcgrass     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%gpp_c3grass        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%gpp_c4grass        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%leafc_enftemp      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%leafc_enfboreal    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%leafc_dnfboreal    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%leafc_ebftrop      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%leafc_ebftemp      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%leafc_dbftrop      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%leafc_dbftemp      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%leafc_dbfboreal    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%leafc_ebstemp      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%leafc_dbstemp      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%leafc_dbsboreal    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%leafc_c3arcgrass   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%leafc_c3grass      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%leafc_c4grass      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%lai_enftemp      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%lai_enfboreal    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%lai_dnfboreal    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%lai_ebftrop      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%lai_ebftemp      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%lai_dbftrop      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%lai_dbftemp      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%lai_dbfboreal    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%lai_ebstemp      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%lai_dbstemp      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%lai_dbsboreal    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%lai_c3arcgrass   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%lai_c3grass      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%lai_c4grass      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npp_enftemp      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npp_enfboreal    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npp_dnfboreal    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npp_ebftrop      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npp_ebftemp      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npp_dbftrop      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npp_dbftemp      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npp_dbfboreal    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npp_ebstemp      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npp_dbstemp      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npp_dbsboreal    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npp_c3arcgrass   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npp_c3grass      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npp_c4grass      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npptoleafc_enftemp      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npptoleafc_enfboreal    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npptoleafc_dnfboreal    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npptoleafc_ebftrop      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npptoleafc_ebftemp      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npptoleafc_dbftrop      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npptoleafc_dbftemp      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npptoleafc_dbfboreal    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npptoleafc_ebstemp      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npptoleafc_dbstemp      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npptoleafc_dbsboreal    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npptoleafc_c3arcgrass   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npptoleafc_c3grass      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%npptoleafc_c4grass      , set_defaults)
#ifdef CROP
      CALL sync_hist_vars_one (DEF_hist_vars%cphase                          , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%gddmaturity                     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%gddplant                        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%vf                              , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%hui                             , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cropprod1c                      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cropprod1c_loss                 , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cropseedc_deficit               , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%grainc_to_cropprodc             , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%plantdate_rainfed_temp_corn     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%plantdate_irrigated_temp_corn   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%plantdate_rainfed_spwheat       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%plantdate_irrigated_spwheat     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%plantdate_rainfed_wtwheat       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%plantdate_irrigated_wtwheat     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%plantdate_rainfed_temp_soybean  , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%plantdate_irrigated_temp_soybean, set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%plantdate_rainfed_cotton        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%plantdate_irrigated_cotton      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%plantdate_rainfed_rice          , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%plantdate_irrigated_rice        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%plantdate_rainfed_sugarcane     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%plantdate_irrigated_sugarcane   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%plantdate_rainfed_trop_corn     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%plantdate_irrigated_trop_corn   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%plantdate_rainfed_trop_soybean  , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%plantdate_irrigated_trop_soybean, set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%plantdate_unmanagedcrop         , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cropprodc_rainfed_temp_corn     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cropprodc_irrigated_temp_corn   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cropprodc_rainfed_spwheat       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cropprodc_irrigated_spwheat     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cropprodc_rainfed_wtwheat       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cropprodc_irrigated_wtwheat     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cropprodc_rainfed_temp_soybean  , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cropprodc_irrigated_temp_soybean, set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cropprodc_rainfed_cotton        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cropprodc_irrigated_cotton      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cropprodc_rainfed_rice          , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cropprodc_irrigated_rice        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cropprodc_rainfed_sugarcane     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cropprodc_irrigated_sugarcane   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cropprodc_rainfed_trop_corn     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cropprodc_irrigated_trop_corn   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cropprodc_rainfed_trop_soybean  , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cropprodc_irrigated_trop_soybean, set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cropprodc_unmanagedcrop         , set_defaults)

      CALL sync_hist_vars_one (DEF_hist_vars%grainc_to_seed                  , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%fert_to_sminn                   , set_defaults)

      IF(DEF_USE_FERT)THEN
         CALL sync_hist_vars_one (DEF_hist_vars%manunitro                    , set_defaults)
         CALL sync_hist_vars_one (DEF_hist_vars%fertnitro_corn               , set_defaults)
         CALL sync_hist_vars_one (DEF_hist_vars%fertnitro_swheat             , set_defaults)
         CALL sync_hist_vars_one (DEF_hist_vars%fertnitro_wwheat             , set_defaults)
         CALL sync_hist_vars_one (DEF_hist_vars%fertnitro_soybean            , set_defaults)
         CALL sync_hist_vars_one (DEF_hist_vars%fertnitro_cotton             , set_defaults)
         CALL sync_hist_vars_one (DEF_hist_vars%fertnitro_rice1              , set_defaults)
         CALL sync_hist_vars_one (DEF_hist_vars%fertnitro_rice2              , set_defaults)
         CALL sync_hist_vars_one (DEF_hist_vars%fertnitro_sugarcane          , set_defaults)
      ENDIF

      IF(DEF_USE_IRRIGATION)THEN
         CALL sync_hist_vars_one (DEF_hist_vars%sum_irrig                    , set_defaults)
         CALL sync_hist_vars_one (DEF_hist_vars%sum_deficit_irrig            , set_defaults)
         CALL sync_hist_vars_one (DEF_hist_vars%sum_irrig_count              , set_defaults)
         CALL sync_hist_vars_one (DEF_hist_vars%waterstorage                 , set_defaults)
         CALL sync_hist_vars_one (DEF_hist_vars%groundwater_demand           , set_defaults)
         CALL sync_hist_vars_one (DEF_hist_vars%groundwater_supply           , set_defaults)
         CALL sync_hist_vars_one (DEF_hist_vars%reservoirriver_demand        , set_defaults)
         CALL sync_hist_vars_one (DEF_hist_vars%reservoirriver_supply        , set_defaults)
      ENDIF
#endif
      CALL sync_hist_vars_one (DEF_hist_vars%ndep_to_sminn                   , set_defaults)
      IF(DEF_USE_NITRIF)THEN
         CALL sync_hist_vars_one (DEF_hist_vars%CONC_O2_UNSAT                , set_defaults)
         CALL sync_hist_vars_one (DEF_hist_vars%O2_DECOMP_DEPTH_UNSAT        , set_defaults)
      ENDIF
      IF(DEF_USE_FIRE)THEN
         CALL sync_hist_vars_one (DEF_hist_vars%abm                          , set_defaults)
         CALL sync_hist_vars_one (DEF_hist_vars%gdp                          , set_defaults)
         CALL sync_hist_vars_one (DEF_hist_vars%peatf                        , set_defaults)
         CALL sync_hist_vars_one (DEF_hist_vars%hdm                          , set_defaults)
         CALL sync_hist_vars_one (DEF_hist_vars%lnfm                         , set_defaults)
      ENDIF
      CALL sync_hist_vars_one (DEF_hist_vars%leafcCap                        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%leafc_storageCap                , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%leafc_xferCap                   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%frootcCap                       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%frootc_storageCap               , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%frootc_xferCap                  , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%livestemcCap                    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%livestemc_storageCap            , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%livestemc_xferCap               , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%deadstemcCap                    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%deadstemc_storageCap            , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%deadstemc_xferCap               , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%livecrootcCap                   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%livecrootc_storageCap           , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%livecrootc_xferCap              , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%deadcrootcCap                   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%deadcrootc_storageCap           , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%deadcrootc_xferCap              , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%leafnCap                        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%leafn_storageCap                , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%leafn_xferCap                   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%frootnCap                       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%frootn_storageCap               , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%frootn_xferCap                  , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%livestemnCap                    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%livestemn_storageCap            , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%livestemn_xferCap               , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%deadstemnCap                    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%deadstemn_storageCap            , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%deadstemn_xferCap               , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%livecrootnCap                   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%livecrootn_storageCap           , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%livecrootn_xferCap              , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%deadcrootnCap                   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%deadcrootn_storageCap           , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%deadcrootn_xferCap              , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%t_scalar                        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%w_scalar                        , set_defaults)

      CALL sync_hist_vars_one (DEF_hist_vars%litr1cCap_vr                    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%litr2cCap_vr                    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%litr3cCap_vr                    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%soil1cCap_vr                    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%soil2cCap_vr                    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%soil3cCap_vr                    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cwdcCap_vr                      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%litr1nCap_vr                    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%litr2nCap_vr                    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%litr3nCap_vr                    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%soil1nCap_vr                    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%soil2nCap_vr                    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%soil3nCap_vr                    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cwdnCap_vr                      , set_defaults)
#endif
      IF(DEF_USE_OZONESTRESS)THEN
         CALL sync_hist_vars_one (DEF_hist_vars%o3uptakesun                  , set_defaults)
         CALL sync_hist_vars_one (DEF_hist_vars%o3uptakesha                  , set_defaults)
      ENDIF

#ifdef DataAssimilation
      CALL sync_hist_vars_one (DEF_hist_vars%DA_wliq_h2osoi_5cm        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%DA_wliq_h2osoi_5cm_a      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%DA_t_soisno_5cm           , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%DA_t_soisno_5cm_a         , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%DA_wliq_soisno_ens        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%DA_t_soisno_ens           , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%DA_wliq_soisno_5cm_ens_std, set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%DA_t_soisno_5cm_ens_std   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%DA_t_brt_smap             , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%DA_t_brt_smap_a           , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%DA_t_brt_smap_ens         , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%DA_t_brt_smap_ens_std     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%DA_t_brt_fy3d             , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%DA_t_brt_fy3d_a           , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%DA_t_brt_fy3d_ens         , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%DA_t_brt_fy3d_ens_std     , set_defaults)
#endif

      CALL sync_hist_vars_one (DEF_hist_vars%t_soisno    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%wliq_soisno , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%wice_soisno , set_defaults)

      CALL sync_hist_vars_one (DEF_hist_vars%h2osoi      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%qlayer      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%lake_deficit, set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%rstfacsun   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%rstfacsha   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%gssun       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%gssha       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%rootr       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%vegwp       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%BD_all      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%wfc         , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%OM_density  , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%wdsrf       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%wdsrf_inst  , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%zwt         , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%wa          , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%wa_inst     , set_defaults)

      CALL sync_hist_vars_one (DEF_hist_vars%dz_lake     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%t_lake      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%lake_icefrac, set_defaults)

#ifdef BGC
      CALL sync_hist_vars_one (DEF_hist_vars%litr1c_vr   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%litr2c_vr   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%litr3c_vr   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%soil1c_vr   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%soil2c_vr   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%soil3c_vr   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cwdc_vr     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%litr1n_vr   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%litr2n_vr   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%litr3n_vr   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%soil1n_vr   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%soil2n_vr   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%soil3n_vr   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%cwdn_vr     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%sminn_vr    , set_defaults)
#endif

      CALL sync_hist_vars_one (DEF_hist_vars%ustar       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%ustar2      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%tstar       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%qstar       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%zol         , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%rib         , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%fm          , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%fh          , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%fq          , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%us10m       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%vs10m       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%fm10m       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%sr          , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%solvd       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%solvi       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%solnd       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%solni       , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%srvd        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%srvi        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%srnd        , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%srni        , set_defaults)

      CALL sync_hist_vars_one (DEF_hist_vars%solvdln     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%solviln     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%solndln     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%solniln     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%srvdln      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%srviln      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%srndln      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%srniln      , set_defaults)
#ifdef HYPERSPECTRAL
      CALL sync_hist_vars_one (DEF_hist_vars%sol_dir_ln_hires, set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%sol_dif_ln_hires, set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%sr_dir_ln_hires , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%sr_dif_ln_hires , set_defaults)
#endif

      CALL sync_hist_vars_one (DEF_hist_vars%xsubs_bsn   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%xsubs_hru   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%riv_height  , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%riv_veloct  , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%discharge   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%floodarea   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%floodfrc    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%wdsrf_hru   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%veloc_hru   , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%volresv     , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%qresv_in    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%qresv_out   , set_defaults)

      CALL sync_hist_vars_one (DEF_hist_vars%sedcon      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%sedout      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%bedout      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%sedinp      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%netflw      , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%sedlayer    , set_defaults)
      CALL sync_hist_vars_one (DEF_hist_vars%shearvel    , set_defaults)

      CALL sync_hist_vars_one (DEF_hist_vars%sensors     , set_defaults)

   END SUBROUTINE sync_hist_vars

   SUBROUTINE sync_hist_vars_one (onoff, set_defaults)

   USE MOD_MPAS_MPI
   IMPLICIT NONE

   logical, intent(inout) :: onoff
   logical, intent(in)    :: set_defaults

      IF (mpas_is_root) THEN
         IF (set_defaults) THEN
            onoff = DEF_HIST_vars_out_default
         ENDIF
      ENDIF

#ifdef MPAS_MPI
      CALL mpi_bcast (onoff, 1, mpi_logical, mpas_root, mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('CoLM history-option broadcast')
#endif

   END SUBROUTINE sync_hist_vars_one

END MODULE MOD_Namelist
