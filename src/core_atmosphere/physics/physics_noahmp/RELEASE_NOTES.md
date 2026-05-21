# Noah-MP model release notes

## Noah-MP version 5.0 release

### LSM capabilities/enhancements

- Modernization/refactoring:

  - Major re-structure/refactoring of the entire Noah-MP code with modern Fortran standards without physics changes.

### LSM bug fixes

- None

### External modules capabilities/enhancements

- None

### Driver capabilities/enhancements

- Refactored driver to work with the modernized Noah-MP version 5.0

### Driver bug fixes

- None


## Noah-MP version 4.5 release

### LSM capabilities/enhancements

- Urban modeling:

  - Update the local climate zone numbers

- Canopy heat storage:

  - bring hard-coded tunable canopy heat capacity parameter to MPTABLE

### LSM bug fixes

- Several bug fixes in urban, runoff, canopy, crop processes

### External modules capabilities/enhancements

- None

### Driver capabilities/enhancements

- None

### Driver bug fixes

- None


## Noah-MP version 4.4 release

### LSM capabilities/enhancements

- Tile drainage:

  - Add new tile drainage physics and options

- Snowpack process enhancement:

  - Improved snow viscosity to enhance snowpack compaction

- Canopy heat storage:

  - add canopy heat storage in vegetation temperature calculation

- Runoff scheme:

  - Updated formulation in runoff option =1 (TOPMODEL with groundwater)

- Soil processes:

  - Add new capabilities to allow using a different soil timestep with main Noah-MP timestep using namelist control

- Input/output:

  - Add new capabilities to output additional detailed Noah-MP water budget terms using namelist control

### LSM bug fixes

- Several bug fixes in inout variables, energy, water, and canopy processes

### External modules capabilities/enhancements

- None

### Driver capabilities/enhancements

- None

### Driver bug fixes

- None


## Noah-MP version 4.3 release

### LSM capabilities/enhancements

- Snow-related updates:

  - Add wet-bulb temperature snow-rain partitioning scheme (OPT_SNF=5) based on Wang et al. 2019 (NWM)
  - Add snow retention process at the snowpack bottom to improve streamflow modeling (NWM)
  - Modify wind-canopy absorption coefficient (CWPVT) parameter values in MPTABLE to be vegetation dependent based on Goudriaan1977
  - Bring hard-coded snow emissivity and parameter (2.5*z0) in snow cover formulation to tunable MPTABLE parameters
  - Update MFSNO in snow cover formulation with optimized vegetation-dependent values
  - Limit the bulk leaf boundary layer resistance (RB) to a more realistic range (5~50)

- New irrigation scheme:

  - multiple irrigation methods: sprinkler, micro, and surface flooding

- Crop scheme update:

  - separate the original generic crop physiology parameters in the modis vegetation section into C3/C4 specific parameters in the crop section

- New urban physics working with Noah-MP:

  - Local climate zone (LCZ), solar panel, green roof, new building drag parameterization

### LSM bug fixes

- None

### External modules capabilities/enhancements

- None

### Driver capabilities/enhancements

- None

### Driver bug fixes

- None


## Noah-MP version 4.1 release

### LSM capabilities/enhancements

- Consolidate NWM changes into WRF version (#18)
  - add unpopulated header required by NOAA
  - add BATS parameters to data structure and output band snow albedo
  - update MPTABLE for BATS albedo parameters
  - add BATS albedo local variables to noahmpdrv
  - transfer new BATS table values to parameters data structure in noahmpdrv
  - add RSURF_EXP parameter to data structure and update MPTABLE
  - change snow water equivalent limit to 5000mm
  - assume LAI is stand LAI and doesn't need to be rescaled by FVEG
  - conserve snow pack heat when layer melts completely
  - change output messages and Fortran open/read unit numbers to WCOSS standard
  - include a few missed changes from WRF

### LSM bug fixes

- Define and declare a few variables in physics routines

- Noah-MP bulk urban roughness length set to table values

### External modules capabilities/enhancements

- Air conditioning fraction for BEM model

- Improve urban memory by allowing different dimensions for urban variables

### Driver capabilities/enhancements

- None

### Driver bug fixes

- None


## Noah-MP version 4.0.1 release

### LSM capabilities/enhancements

- None

### LSM bug fixes

- Noah-MP frozen soil initialization- An incorrect sign change was introduced in v4.0, impacting soil moisture and soil temperature initialization.

- Array out of bounds Noah-MP - Fix possible/likely array out of bounds by assuming homogeneous soil with depth.Only applies to opt_run=2.

- Noah-MP snow liquid water movement - prevent excessive gravitational water movement. Fixes unrealistic snow density values during melt season.

- Noah-MP divide by zero - Bug fix in v4.0 introduced a possible divide by zero when LAI is zero.

- Noah-MP leaf aerodynamic resistance - limit leaf aerodynamic resistance to prevent very large canopy exchange coefficients with high wind speed.

### Driver capabilities/enhancements

-  Add new single point driver based on Bondville data

### Driver bug fixes

-  Missing quotation mark in spatial_filename check print statement


## Noah-MP version 4.0 release

### LSM capabilities/enhancements

- Add pedotransfer function option for soil propertis 
    - add optional read for soil composition and multi-layer soil texture from setup/input file
    - activated with opt_soil and opt_pedo
    - update MPTABLE.TBL with pedotransfer function coefficients

- Add Gecros crop model
    - activated with opt_crop=2 (Liu et al. crop now opt_crop=1)
    - some modifications for crop initialization

- Groundwater module (opt_run=5) updates 
    - move init to driver for parallel capability 
    - remove rivermask/nonriver from input

- EPA modifications to output total stomatal resistance

### LSM bug fixes

- None

### Driver capabilities/enhancements

-  Change some predefined defaults in user_build_options.compiler files based on some Cheyenne tests

-  Add ISLAKE to the preprocessing and driver to accommodate WRF files that define a distinct lake category

### Driver bug fixes

-  Change PGSXY and CROPCAT to be initialized undefined_int


## Noah-MP version 3.9 release

### LSM capabilities/enhancements

- Crop modifications in v3.9 to read in crop datasets and initialize properly

- Modifications in v3.9 to read in groundwater datasets

- Noah-MP can now run with single-layer and multi-layer urban models

### LSM bug fixes

- Several fixes in Section 1 of SOILPARM.TBL

- Fix strange Noah-MP behavior in soil water in certain conditions

- Fix uninitialized variable in Noah-MP surface exchange option

### Driver capabilities/enhancements

-  Add capability to include snow in forcing files
   - Need to set FORCING_NAME_SN and PCP_PARTITION_OPTION = 4
   - Snow is assumed to be <= incoming precipitation

-  Add capability to define name of forcing variables in namelist.hrldas

-  Add spinup option to namelist
   - controlled by spinup_loops in namelist.hrldas
   - will run kday/khour spinup_loops times before starting the simulation

-  Add capability to exclude the first output file since this file contains only initial states 
   - and no computed fluxes
   - activated by namelist.hrldas option: SKIP_FIRST_OUTPUT = .true.

-  Added README.namelist to describe all the namelist.hrldas options

### Driver bug fixes

-  None


## Noah-MP version 3.8.1 release

### LSM capabilities/enhancements

- None

### LSM bug fixes

- Change C3C4 in MPTABLE to integer

- Set some limits on stability function for OPT_SFC = 2

- Change limit for minimum wood pool in dynamic vegetation
  
- Fix bug in QSFC calculation

- Prevent divide by zero when soil moisture is zero

- Fix a few bugs in the crop code; make DVEG = 10 activate crop model

### Driver capabilities/enhancements

- Added configure script for generating user_build_options file

### Driver bug fixes

- None


## Noah-MP version 3.8 release

### LSM capabilities/enhancements

- Added 3 new dveg option for reading LAI from forcing and 1 new dveg option for reading FVEG;

   - Also added initial commit of crop model; currently runs crop everywhere
   - dveg =  6 -> dynamic vegetation on  (use FVEG = SHDFAC from input)
   - dveg =  7 -> dynamic vegetation off (use input LAI; use FVEG = SHDFAC from input)
   - dveg =  8 -> dynamic vegetation off (use input LAI; calculate FVEG)
   - dveg =  9 -> dynamic vegetation off (use input LAI; use maximum vegetation fraction)
   - dveg = 10 -> crop model on (use maximum vegetation fraction)

- Added glacier options:

   - opt_gla = 1 -> original Noah-MP version
   - opt_gla = 2 -> no ice phase change or sublimation (like Noah glacier)

- Added surface resistance as an option (now four options)

   - opt_sfc = 1 -> Sakaguchi and Zeng, 2009 (has been Noah-MP default)
   - opt_sfc = 2 -> Sellers (1992)
   - opt_sfc = 3 -> adjusted Sellers to decrease RSURF for wet soil
   - opt_sfc = 4 -> option 1 for non-snow; rsurf = rsurf_snow for snow (set as RSURF_SNOW in MPTABLE)

- Made the specification of urban types more general 

   - (LOW_DENSITY_RESIDENTIAL, HIGH_DENSITY_RESIDENTIAL, HIGH_INTENSITY_INDUSTRIAL), 
   - now set in the MPTABLE dependent on classification scheme (i.e., not limited to 31,32,33); 
   - this is for future coupling with urban models.

### LSM bug fixes

- Fixed two bugs with OPT_STC=3

- Fixed bug in new surface resistance option causing divide by 0

- Write a message if incoming snow water and snow depth are inconsistent;
  Reduce SWE to 2000mm if input is >2000mm, Noah-MP limits SWE internally to 2000mm
  
- Recalculate ESTG in glacier code when snow is melting, will decrease sublimation, but likely increase melting

### Driver capabilities/enhancements

- Added instructions and scripts for extraction of single point forcing and setup files from 
   2D datasets (e.g., NLDAS)

- Structure for spatially-varying soil properties added to DRV and LSM; 
   Use of the 2D/3D fields in the driver and DRV commented to be consistent with WRF

### Driver bug fixes

- Zero forcing where not land to prevent overflow with ifort


## Noah-MP version 3.7.1 release

### LSM capabilities/enhancements

- Added depth dimension to soil parameters.

### LSM bug fixes

- Reorganized parameters to fix problems with OpenMP in WRF simulations.

### Driver capabilities/enhancements

- none

### Driver bug fixes

- Initialized some accumulated fields at 0 (instead of undefined).


## Noah-MP version 3.7 release

### New capabilities:

- A parallel capability has been added by Wei Yu (weiyu@ncar.edu) to support mpi only.

  - To compile with parallel version, edit the file 'user_build_options', 
    uncommment the compiler section with MPI (available for pgf90 and ifort compilers)
  - To compile with sequential version, edit the file 'user_build_options', uncommment the compiler section without MPI

- System setup and execution now requires only a WRF/WPS geo_em file, Dependence on the wrfinput file has been removed.

- As part of #2, initialization no longer occurs in the first forcing file, 

  - but in the file listed in the namelist as: HRLDAS_SETUP_FILE = "
  - The initialization fields are: SNOW,CANWAT,TSK,TSLB,SMOIS
  - This file also contains the static grid/domain information: XLAT,XLONG,TMN,HGT,SEAICE,MAPFAC_MX,MAPFAC_MY,SHDMAX,SHDMIN,XLAND,IVGTYP,ISLTYP,DZS,ZS
  - This file can also contains some optional fields: LAI  
  - NOTE: a WRF input file can be used as a HRLDAS_SETUP_FILE

- The timing structure has changed:

  - The initial conditions are the states at START time. 
  - First forcing file used is START time + FORCING_TIMESTEP 
  - First integration is START time + NOAH_TIMESTEP 

- First output file is now START time + OUTPUT_TIMESTEP     

- RESTART file states are consistent with OUTPUT file states with the same time stamp

- Instructions for using GLDAS and NLDAS as forcing has been provided in addition to the NARR instructions (see /docs)
  - Also, a NCL script has been included for preparing single- or multi-point forcing
     
- Initial LAI (if present in the HRLDAS_SETUP_FILE) will be used to initialize the leaf and stem carbon pools

- Removed dependence on external GRIB tables for forcing creation; now in namelist only



Updated: March 10, 2023
