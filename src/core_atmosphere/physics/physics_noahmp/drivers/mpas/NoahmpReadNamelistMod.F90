module NoahmpReadNamelistMod

!!! Initialize Noah-MP namelist variables
!!! Namelist variables should be first defined in NoahmpIOVarType.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType

  implicit none

contains

!=== read namelist values

  subroutine NoahmpReadNamelist(NoahmpIO)

    implicit none

    type(NoahmpIO_type), intent(inout)  :: NoahmpIO

!---------------------------------------------------------------------
!  NAMELIST start
!---------------------------------------------------------------------

    ! local namelist variables
    
    character(len=256)      :: indir = '.'
    integer                 :: ierr
    integer                 :: NSOIL                 ! number of soil layers
    integer                 :: forcing_timestep
    integer                 :: noah_timestep
    integer                 :: start_year
    integer                 :: start_month
    integer                 :: start_day
    integer                 :: start_hour
    integer                 :: start_min
    character(len=256)      :: outdir = "."
    character(len=256)      :: restart_filename_requested = " "
    integer                 :: restart_frequency_hours
    integer                 :: output_timestep
    integer                 :: spinup_loops     = 0
    integer                 :: sf_urban_physics = 0
    integer                 :: use_wudapt_lcz   = 0  ! add for LCZ urban
    integer                 :: num_urban_ndm    = 2
    integer                 :: num_urban_ng     = 10
    integer                 :: num_urban_nwr    = 10
    integer                 :: num_urban_ngb    = 10
    integer                 :: num_urban_nf     = 10
    integer                 :: num_urban_nz     = 18
    integer                 :: num_urban_nbui   = 15
    integer                 :: num_urban_hi     = 15 
    integer                 :: num_urban_ngr    = 10  ! = ngr_u in bep_bem.F
    integer                 :: noahmp_output    = 0
    real(kind=kind_noahmp)  :: urban_atmosphere_thickness = 2.0
    real(kind=kind_noahmp)  :: soil_timestep    = 0.0   ! soil timestep (default=0: same as main noahmp timestep)

    ! derived urban dimensions
    character(len=256)      :: forcing_name_T  = "T2D"
    character(len=256)      :: forcing_name_Q  = "Q2D"
    character(len=256)      :: forcing_name_U  = "U2D"
    character(len=256)      :: forcing_name_V  = "V2D"
    character(len=256)      :: forcing_name_P  = "PSFC"
    character(len=256)      :: forcing_name_LW = "LWDOWN"
    character(len=256)      :: forcing_name_SW = "SWDOWN"
    character(len=256)      :: forcing_name_PR = "RAINRATE"
    character(len=256)      :: forcing_name_SN = ""
    integer                 :: dynamic_veg_option                 = 4
    integer                 :: canopy_stomatal_resistance_option  = 1
    integer                 :: btr_option                         = 1
    integer                 :: surface_runoff_option              = 3
    integer                 :: subsurface_runoff_option           = 3
    integer                 :: surface_drag_option                = 1
    integer                 :: supercooled_water_option           = 1
    integer                 :: frozen_soil_option                 = 1
    integer                 :: radiative_transfer_option          = 3
    integer                 :: snow_albedo_option                 = 1
    integer                 :: snow_thermal_conductivity          = 1
    integer                 :: pcp_partition_option               = 1
    integer                 :: tbot_option                        = 2
    integer                 :: temp_time_scheme_option            = 1
    integer                 :: glacier_option                     = 1
    integer                 :: surface_resistance_option          = 1
    integer                 :: soil_data_option                   = 1
    integer                 :: pedotransfer_option                = 1
    integer                 :: crop_option                        = 0
    integer                 :: irrigation_option                  = 0 
    integer                 :: irrigation_method                  = 0
    integer                 :: dvic_infiltration_option           = 1
    integer                 :: tile_drainage_option               = 0
    integer                 :: split_output_count                 = 1
    logical                 :: skip_first_output                  = .false.
    integer                 :: khour                              = -9999
    integer                 :: kday                               = -9999
    real(kind=kind_noahmp)  :: zlvl                               = 10.
    character(len=256)      :: hrldas_setup_file                  = " "
    character(len=256)      :: spatial_filename                   = " "
    character(len=256)      :: external_veg_filename_template     = " "
    character(len=256)      :: external_lai_filename_template     = " "
    character(len=256)      :: agdata_flnm                        = " "
    character(len=256)      :: tdinput_flnm                       = " "
    integer, parameter      :: MAX_SOIL_LEVELS                    = 10     ! maximum soil levels in namelist
    real(kind=kind_noahmp), dimension(MAX_SOIL_LEVELS) :: soil_thick_input ! depth to soil interfaces from namelist [m]
    
    namelist / NOAHLSM_OFFLINE /    &
#ifdef WRF_HYDRO
         finemesh,finemesh_factor,forc_typ, snow_assim , GEO_STATIC_FLNM, HRLDAS_ini_typ, &
#endif
         indir, nsoil, soil_thick_input, forcing_timestep, noah_timestep, soil_timestep,  &
         start_year, start_month, start_day, start_hour, start_min,                       &
         outdir, skip_first_output, noahmp_output,                                        &
         restart_filename_requested, restart_frequency_hours, output_timestep,            &
         spinup_loops,                                                                    &
         forcing_name_T,forcing_name_Q,forcing_name_U,forcing_name_V,forcing_name_P,      &
         forcing_name_LW,forcing_name_SW,forcing_name_PR,forcing_name_SN,                 &
         dynamic_veg_option, canopy_stomatal_resistance_option,                           &
         btr_option, surface_drag_option, supercooled_water_option,        &
         frozen_soil_option, radiative_transfer_option, snow_albedo_option,               &
         snow_thermal_conductivity, surface_runoff_option, subsurface_runoff_option,      &
         pcp_partition_option, tbot_option, temp_time_scheme_option,                      &
         glacier_option, surface_resistance_option,                                       &
         irrigation_option, irrigation_method, dvic_infiltration_option,                  &
         tile_drainage_option,soil_data_option, pedotransfer_option, crop_option,         &
         sf_urban_physics,use_wudapt_lcz,num_urban_hi,urban_atmosphere_thickness,         &
         num_urban_ndm,num_urban_ng,num_urban_nwr ,num_urban_ngb ,                        &
         num_urban_nf ,num_urban_nz,num_urban_nbui,num_urban_ngr ,                        &
         split_output_count,                                                              & 
         khour, kday, zlvl, hrldas_setup_file,                                            &
         spatial_filename, agdata_flnm, tdinput_flnm,                                     &
         external_veg_filename_template, external_lai_filename_template


    !---------------------------------------------------------------
    !  Initialize namelist variables to dummy values, so we can tell
    !  if they have not been set properly.
    !---------------------------------------------------------------
    if (.not. allocated(NoahmpIO%soil_thick_input)) allocate(NoahmpIO%soil_thick_input(1:MAX_SOIL_LEVELS))
    NoahmpIO%nsoil                   = undefined_int
    NoahmpIO%soil_thick_input        = undefined_real
    NoahmpIO%DTBL                    = undefined_real
    NoahmpIO%soiltstep               = undefined_real
    NoahmpIO%start_year              = undefined_int
    NoahmpIO%start_month             = undefined_int
    NoahmpIO%start_day               = undefined_int
    NoahmpIO%start_hour              = undefined_int
    NoahmpIO%start_min               = undefined_int
    NoahmpIO%khour                   = undefined_int
    NoahmpIO%kday                    = undefined_int
    NoahmpIO%zlvl                    = undefined_real
    NoahmpIO%forcing_timestep        = undefined_int
    NoahmpIO%noah_timestep           = undefined_int
    NoahmpIO%output_timestep         = undefined_int
    NoahmpIO%restart_frequency_hours = undefined_int
    NoahmpIO%spinup_loops            = 0
    NoahmpIO%noahmp_output           = 0

    !---------------------------------------------------------------
    ! read namelist.input
    !---------------------------------------------------------------
    
    open(30, file="namelist.hrldas", form="FORMATTED")
    read(30, NOAHLSM_OFFLINE, iostat=ierr)
    if (ierr /= 0) then
       write(*,'(/," ***** ERROR: Problem reading namelist NOAHLSM_OFFLINE",/)')
       rewind(30)
       read(30, NOAHLSM_OFFLINE)
       stop " ***** ERROR: Problem reading namelist NOAHLSM_OFFLINE"
    endif
    close(30)
  
    NoahmpIO%DTBL            = real(noah_timestep)
    NoahmpIO%soiltstep       = soil_timestep
    NoahmpIO%NSOIL           = nsoil

    !---------------------------------------------------------------------
    !  NAMELIST end
    !---------------------------------------------------------------------
   
    !---------------------------------------------------------------------
    !  NAMELIST check begin
    !---------------------------------------------------------------------
    NoahmpIO%update_lai = .true.   ! default: use LAI if present in forcing file
    if(dynamic_veg_option == 1 .or. dynamic_veg_option == 2 .or. &
       dynamic_veg_option == 3 .or. dynamic_veg_option == 4 .or. &
       dynamic_veg_option == 5 .or. dynamic_veg_option == 6) &    ! remove dveg=10 and add dveg=1,3,4 into the update_lai flag false condition
       NoahmpIO%update_lai = .false.

    NoahmpIO%update_veg = .false.  ! default: don't use VEGFRA if present in forcing file
    if (dynamic_veg_option == 1 .or. dynamic_veg_option == 6 .or. dynamic_veg_option == 7) &
        NoahmpIO%update_veg = .true.

    if (nsoil < 0) then
        stop " ***** ERROR: NSOIL must be set in the namelist."
    endif

    if ((khour < 0) .and. (kday < 0)) then
        write(*, '(" ***** Namelist error: ************************************")')
        write(*, '(" ***** ")')
        write(*, '(" *****      Either KHOUR or KDAY must be defined.")')
        write(*, '(" ***** ")')
        stop
    else if (( khour < 0 ) .and. (kday > 0)) then
        khour = kday * 24
    else if ((khour > 0) .and. (kday > 0)) then
        write(*, '("Namelist warning:  KHOUR and KDAY both defined.")')
    else
        ! all is well.  KHOUR defined
    endif

    if (forcing_timestep < 0) then
        write(*, *)
        write(*, '(" ***** Namelist error: *****************************************")')
        write(*, '(" ***** ")')
        write(*, '(" *****       FORCING_TIMESTEP needs to be set greater than zero.")')
        write(*, '(" ***** ")')
        write(*, *)
        stop
    endif

    if (noah_timestep < 0) then
        write(*, *)
        write(*, '(" ***** Namelist error: *****************************************")')
        write(*, '(" ***** ")')
        write(*, '(" *****       NOAH_TIMESTEP needs to be set greater than zero.")')
        write(*, '(" *****                     900 seconds is recommended.       ")')
        write(*, '(" ***** ")')
        write(*, *)
        stop
    endif

    !
    ! Check that OUTPUT_TIMESTEP fits into NOAH_TIMESTEP:
    !
    if (output_timestep /= 0) then
       if (mod(output_timestep, noah_timestep) > 0) then
         write(*, *)
         write(*, '(" ***** Namelist error: *********************************************************")')
         write(*, '(" ***** ")')
         write(*, '(" *****       OUTPUT_TIMESTEP should set to an integer multiple of NOAH_TIMESTEP.")')
         write(*, '(" *****            OUTPUT_TIMESTEP = ", I12, " seconds")') output_timestep
         write(*, '(" *****            NOAH_TIMESTEP   = ", I12, " seconds")') noah_timestep
         write(*, '(" ***** ")')
         write(*, *)
         stop
       endif
    endif

   !
   ! Check that RESTART_FREQUENCY_HOURS fits into NOAH_TIMESTEP:
   !
    if (restart_frequency_hours /= 0) then
       if (mod(restart_frequency_hours*3600, noah_timestep) > 0) then
         write(*, *)
         write(*, '(" ***** Namelist error: ******************************************************")')
         write(*, '(" ***** ")')
         write(*, '(" *****       RESTART_FREQUENCY_HOURS (converted to seconds) should set to an ")')
         write(*, '(" *****       integer multiple of NOAH_TIMESTEP.")')
         write(*, '(" *****            RESTART_FREQUENCY_HOURS = ", I12, " hours:  ", I12, " seconds")') &
               restart_frequency_hours, restart_frequency_hours*3600
         write(*, '(" *****            NOAH_TIMESTEP           = ", I12, " seconds")') noah_timestep
         write(*, '(" ***** ")')
         write(*, *)
         stop
       endif
    endif

    if (dynamic_veg_option == 2 .or. dynamic_veg_option == 5 .or. dynamic_veg_option == 6) then
      if ( canopy_stomatal_resistance_option /= 1) then
         write(*, *)
         write(*, '(" ***** Namelist error: ******************************************************")')
         write(*, '(" ***** ")')
         write(*, '(" *****       CANOPY_STOMATAL_RESISTANCE_OPTION must be 1 when DYNAMIC_VEG_OPTION == 2/5/6")')
         write(*, *)
         stop
      endif
    endif

    if (soil_data_option == 4 .and. spatial_filename == " ") then
        write(*, *)
        write(*, '(" ***** Namelist error: ******************************************************")')
        write(*, '(" ***** ")')
        write(*, '(" *****       SPATIAL_FILENAME must be provided when SOIL_DATA_OPTION == 4")')
        write(*, *)
        stop
    endif

    if (sf_urban_physics == 2 .or. sf_urban_physics == 3) then
       if ( urban_atmosphere_thickness <= 0.0) then
         write(*, *)
         write(*, '(" ***** Namelist error: ******************************************************")')
         write(*, '(" ***** ")')
         write(*, '(" *****       When running BEP/BEM, URBAN_ATMOSPHERE_LEVELS must contain at least 3 levels")')
         write(*, *)
         stop
       endif
       NoahmpIO%num_urban_atmosphere = int(zlvl/urban_atmosphere_thickness)
       if (zlvl - NoahmpIO%num_urban_atmosphere*urban_atmosphere_thickness >= 0.5*urban_atmosphere_thickness)  &
           NoahmpIO%num_urban_atmosphere = NoahmpIO%num_urban_atmosphere + 1
       if ( NoahmpIO%num_urban_atmosphere <= 2) then
         write(*, *)
         write(*, '(" ***** Namelist error: ******************************************************")')
         write(*, '(" ***** ")')
         write(*, '(" *****       When running BEP/BEM, num_urban_atmosphere must contain at least 3 levels, ")')
         write(*, '(" *****        decrease URBAN_ATMOSPHERE_THICKNESS")')
         write(*, *)
         stop
       endif
    endif
    
    !---------------------------------------------------------------------
    !  Transfer Namelist locals to input data structure
    !---------------------------------------------------------------------
    ! physics option 
    NoahmpIO%IOPT_DVEG                         = dynamic_veg_option 
    NoahmpIO%IOPT_CRS                          = canopy_stomatal_resistance_option
    NoahmpIO%IOPT_BTR                          = btr_option
    NoahmpIO%IOPT_RUNSRF                       = surface_runoff_option
    NoahmpIO%IOPT_RUNSUB                       = subsurface_runoff_option
    NoahmpIO%IOPT_SFC                          = surface_drag_option
    NoahmpIO%IOPT_FRZ                          = supercooled_water_option
    NoahmpIO%IOPT_INF                          = frozen_soil_option
    NoahmpIO%IOPT_RAD                          = radiative_transfer_option
    NoahmpIO%IOPT_ALB                          = snow_albedo_option
    NoahmpIO%IOPT_SNF                          = pcp_partition_option
    NoahmpIO%IOPT_TKSNO                        = snow_thermal_conductivity 
    NoahmpIO%IOPT_TBOT                         = tbot_option
    NoahmpIO%IOPT_STC                          = temp_time_scheme_option
    NoahmpIO%IOPT_GLA                          = glacier_option
    NoahmpIO%IOPT_RSF                          = surface_resistance_option
    NoahmpIO%IOPT_SOIL                         = soil_data_option
    NoahmpIO%IOPT_PEDO                         = pedotransfer_option
    NoahmpIO%IOPT_CROP                         = crop_option
    NoahmpIO%IOPT_IRR                          = irrigation_option
    NoahmpIO%IOPT_IRRM                         = irrigation_method
    NoahmpIO%IOPT_INFDV                        = dvic_infiltration_option
    NoahmpIO%IOPT_TDRN                         = tile_drainage_option
    ! basic model setup variables
    NoahmpIO%indir                             = indir
    NoahmpIO%forcing_timestep                  = forcing_timestep
    NoahmpIO%noah_timestep                     = noah_timestep
    NoahmpIO%start_year                        = start_year
    NoahmpIO%start_month                       = start_month
    NoahmpIO%start_day                         = start_day
    NoahmpIO%start_hour                        = start_hour
    NoahmpIO%start_min                         = start_min
    NoahmpIO%outdir                            = outdir
    NoahmpIO%noahmp_output                     = noahmp_output
    NoahmpIO%restart_filename_requested        = restart_filename_requested
    NoahmpIO%restart_frequency_hours           = restart_frequency_hours
    NoahmpIO%output_timestep                   = output_timestep
    NoahmpIO%spinup_loops                      = spinup_loops
    NoahmpIO%sf_urban_physics                  = sf_urban_physics
    NoahmpIO%use_wudapt_lcz                    = use_wudapt_lcz
    NoahmpIO%num_urban_ndm                     = num_urban_ndm
    NoahmpIO%num_urban_ng                      = num_urban_ng
    NoahmpIO%num_urban_nwr                     = num_urban_nwr
    NoahmpIO%num_urban_ngb                     = num_urban_ngb
    NoahmpIO%num_urban_nf                      = num_urban_nf
    NoahmpIO%num_urban_nz                      = num_urban_nz
    NoahmpIO%num_urban_nbui                    = num_urban_nbui
    NoahmpIO%num_urban_hi                      = num_urban_hi
    NoahmpIO%urban_atmosphere_thickness        = urban_atmosphere_thickness
    NoahmpIO%num_urban_ngr                     = num_urban_ngr
    NoahmpIO%forcing_name_T                    = forcing_name_T
    NoahmpIO%forcing_name_Q                    = forcing_name_Q
    NoahmpIO%forcing_name_U                    = forcing_name_U
    NoahmpIO%forcing_name_V                    = forcing_name_V
    NoahmpIO%forcing_name_P                    = forcing_name_P
    NoahmpIO%forcing_name_LW                   = forcing_name_LW
    NoahmpIO%forcing_name_SW                   = forcing_name_SW
    NoahmpIO%forcing_name_PR                   = forcing_name_PR
    NoahmpIO%forcing_name_SN                   = forcing_name_SN
    NoahmpIO%split_output_count                = split_output_count
    NoahmpIO%skip_first_output                 = skip_first_output
    NoahmpIO%khour                             = khour
    NoahmpIO%kday                              = kday
    NoahmpIO%zlvl                              = zlvl
    NoahmpIO%hrldas_setup_file                 = hrldas_setup_file
    NoahmpIO%spatial_filename                  = spatial_filename
    NoahmpIO%external_veg_filename_template    = external_veg_filename_template
    NoahmpIO%external_lai_filename_template    = external_lai_filename_template
    NoahmpIO%agdata_flnm                       = agdata_flnm
    NoahmpIO%tdinput_flnm                      = tdinput_flnm
    NoahmpIO%MAX_SOIL_LEVELS                   = MAX_SOIL_LEVELS
    NoahmpIO%soil_thick_input                  = soil_thick_input 
 
!---------------------------------------------------------------------
!  NAMELIST check end
!---------------------------------------------------------------------

  end subroutine NoahmpReadNamelist

end module NoahmpReadNamelistMod
