
Modifications made by Pedro. S. Peixoto
===========================================

   Date: Oct 2022

   Features:
   - Added grid utilities
   - Pre/post processing tools 
   - Add consistent scheme of Peixoto JCP 2016 to MPAS 
              (this is currently functioning only in a Branch of MPAS version 4)

   All Modifications where marked with PXT text and a comment
   All things that would be moved/removed was commented

   Updated on Feb 2022 to account for version 7.2


GRID UTILITIES
-------------------------------

* grids folder:
- grids/ : script to get grids from ncar site
- utilities/ :
  - convert_grid_hcm : legacy code to convert mpas grids to hcm grid positioning
  - convert_lat_lon_to_nc_mpas_grid : utility to convert lat-lon grids to mpas grid format
  - convert_nc_grid_to_xyz: utility to extract xyz node positions from mpas grid format
  - grid_gen_global_scvt: original grid generator from ringler/duda
  - grid_rotate : rotate mpas grids
  - jigsaw : jigsaw grid generation tools -> build directly to mpas grid format
  - double_to_float_grid : float type grid convertion tool



INSTALATION
----------------------------------

 * local_software folder:
 - iolib_installation.sh : install all required dependencies for MPAS, with the correct versions
 - setup_env.sh : script to load mpas env variables to use local_software
 - make : use USE_PIO2=true OPENMP=true
     ex: make gfortran CORE=init_atmosphere OPENMP=true USE_PIO2=true AUTOCLEAN=true
         make gfortran CORE=atmosphere OPENMP=true USE_PIO2=true AUTOCLEAN=true


BENCHMARKS
----------------------------------

* benchmarks folder:
- Python scripts to create namelists and streams for test cases 


DOCS 
----------------------------------
* docs folder: documentation and notes
- pxt_useful/ : some day-to-day useful notes to use mpas
- mpas_refs/  : mpas papers and tutorials


POST_PROCESSING
----------------------------------

*post_proc folder: scripts for post processing 
- py: python scripts 
    -- geometry_lat_lon_2d_plot : scripts to plot geometric grid features 
    -- scalar_lat_lon_2d_plot   : scripts to plot scalar fields
    -- time_series_error_plot   : error plot of time series for idealized tests
    -- other scrips -- see folder


OPERATORS
----------------------------------
* mpas_consistent.F: new module with implmentantion regarding Peixoto 2016 paper.   
* Makefile : Added: 

	OBJS = 
	mpas_consistent.o #PXT added mpas_consitent
	
        #PXT added mpas_consitent
	mpas_consistent.o: mpas_vector_operations.o


INIT_ATMOSPHERE CORE
----------------------------------
* Registry : Added new config to allow generation of initial conditions on HCm grid in  <nml_record name="nhyd_model" in_defaults="true">

               <!-- ************************************** PXT-ADDED************************************ -->
               <nml_option name="config_hcm_staggering"        type="logical"       default_value="false" in_defaults="false"
                     units="-"
                     description="Flag to set wind position to cell edge midpoint instead of the \newline
                                   default edge triangle vs cell edge intersection point. This is known as \newline
                                   HCm staggering from Peixoto (2016) JCP paper."
                     possible_values="True or False"/>

* namelist.init_atmosphere : New parameter choice (not automatic)

   config_hcm_staggering = true
  
* mpas_init_atm_cases.F: Added HCm capability in specific points:

   !PXT - module to convert to HCm grid
   use mpas_consistent
   !PXT - config to use HCm grid - use midpoint of Voronoi edges instead of midpoint of Triangle edges
   logical, pointer :: config_hcm_staggering
   
   !PXT - config to use HCm grid - use midpoint of Voronoi edges instead of midpoint of Triangle edges
   call mpas_pool_get_config(domain % blocklist % configs, 'config_hcm_staggering', config_hcm_staggering)

   !PXT - Convert grid to HCm if required
   ! It overrides the content of latEdge, lonEdge
   if(config_hcm_staggering)then
      block_ptr => domain % blocklist
         do while (associated(block_ptr))
           call mpas_pool_get_subpool(block_ptr % structs, 'mesh', mesh)
           !Configure grid for modified/consistent scheme
           !call mpas_log_write( ' Converting grid to HCm staggering ' )
           call mpas_convert_grid_to_hcm(mesh)
           !call mpas_consist_verify_config(mesh, block_ptr % configs)
           block_ptr => block_ptr % next
         end do
      endif
      
  

ATMOSPHERE_MODEL
-------------------------------

* Registry : added new config on nhyd_model

               <!-- PXT - Added configuration flags-->
                <nml_option name="config_consistent_scheme"          type="logical"       default_value="false"
                     units="-"
                     description="Flag to change MPAS horizontal dynamics to Peixoto 2016 consistent scheme"
                     possible_values="true or false"/>
                <nml_option name="config_consistent_dyn"             type="logical"       default_value="false"
                     units="-"
                     description="Flag to change MPAS dynamics to include 3d coriolis term, therefore with consistent dynamics"
                     possible_values="true or false"/>
                <nml_option name="config_hcm_staggering"             type="logical"       default_value="false"
                     units="-"
                     description="Flag to change MPAS horiontal scheme to consiter HCm variable allocation"
                     possible_values="true or false"/>
                <nml_option name="config_KE_vecrecon_perot"          type="logical"       default_value="false"
                     units="-"
                     description="Flag to change MPAS horiontal kinetic energy scheme to Perot reconstruction"
                     possible_values="true or false"/>
                <nml_option name="config_KE_vecrecon_rbf"            type="logical"       default_value="false"
                     units="-"
                     description="Flag to change MPAS horiontal kinetic energy scheme to RBF reconstruction"
                     possible_values="true or false"/>
                <nml_option name="config_bary_interpol_edge"         type="logical"       default_value="false"
                     units="-"
                     description="Flag to change MPAS horiontal interpolation to barycentric interpolation on edges (important for HCm)"
                     possible_values="true or false"/>
                <nml_option name="config_bary_interpol_vertex"       type="logical"       default_value="false"
                     units="-"
                     description="Flag to change MPAS horiontal interpolation to barycentric interpolation on vertices (important for HCm)"
                     possible_values="true or false"/>
                <nml_option name="config_consist_perp"               type="logical"       default_value="false"
                     units="-"
                     description="Flag to change MPAS horiontal interpolation to Peixoto 2016 Coriolis scheme"
                     possible_values="true or false"/>
                <nml_option name="config_hollingsworth"              type="logical"       default_value="true"
                     units="-"
                     description="Flag to change MPAS horiontal Kinetic energy scheme a la Gassmann ((reduced Holingsworth instability)"
                     possible_values="true or false"/>


* namelist.atmosphere : Will automatically have new parameter choices:

	config_consistent_scheme = true
    	config_KE_vecrecon_perot = true
      config_KE_vecrecon_rbf = true
      config_bary_interpol_edge = false
      config_bary_interpol_tri = false
	config_consist_perp = true
	config_hollingsworth = true

     OBS:   config_consistent_scheme = true => 
	       config_KE_vecrecon_perot = config_bary_interpol = config_consist_perp = true
	       config_KE_vecrecon_rbf = false

* mpas_atm_core/atm_mpas_init_block

	- Added use mpas_consistent and a warning stating that for the consistent scheme it is recommended to be on HCm grid
	- Moved RBF initialisation (mpas_init_reconstruct) to before solve_diagonstics
	- Moved RBF reconstruction to inside solve_diagnostics 

* atm_time_integration/atm_init_coupled_diagnostics 

	- Added calculation of barycentric coordinates on mass flux (ru) calculation to alow HCm (TODO)

* atm_time_integration/atm_compute_solve_diagnostics (TODO)

	- Added calculation of barycentric coordinates for h_edge (TODO)
	- Added vector reconstruction to cell centre - either Perot or RBF
	   Perot calculates for halos as well, but RBF not, because the coefficients
	   for the halo cell are missing
	- Added new kinetic energy calculation, based on reconstructions 



* Variable declarations added

      !PXT additional flags
      logical, pointer :: config_consistent_scheme
      logical, pointer :: config_KE_vecrecon_perot
      logical, pointer :: config_KE_vecrecon_rbf
      logical, pointer :: config_bary_interpol_edge
      logical, pointer :: config_bary_interpol_tri
      logical, pointer :: config_consist_perp
      logical, pointer :: config_hollingsworth

* Variable calls from pool

      !PXT additional flags
      call mpas_pool_get_config(configs, 'config_consistent_scheme', config_consistent_scheme)
      call mpas_pool_get_config(configs, 'config_KE_vecrecon_perot', config_KE_vecrecon_perot)
      call mpas_pool_get_config(configs, 'config_KE_vecrecon_rbf', config_KE_vecrecon_rbf)
      call mpas_pool_get_config(configs, 'config_bary_interpol_edge', config_bary_interpol_edge)
      call mpas_pool_get_config(configs, 'config_bary_interpol_tri', config_bary_interpol_tri)
      call mpas_pool_get_config(configs, 'config_consist_perp', config_consist_perp)
      call mpas_pool_get_config(configs, 'config_hollingsworth', config_hollingsworth)



