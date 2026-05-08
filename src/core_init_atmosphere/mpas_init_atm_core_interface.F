! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
module init_atm_core_interface

   use mpas_attlist
   use mpas_derived_types
   use mpas_pool_routines
   use mpas_dmpar
   use mpas_io_units
   use mpas_log, only : mpas_log_write

   private :: setup_hydrometeor_packages, &
              mpas_split_string_new

   contains


   !***********************************************************************
   !
   !  routine init_atm_setup_core
   !
   !> \brief   Init atmosphere core setup routine
   !> \author  Doug Jacobsen, Michael Duda
   !> \date    18 March 2015
   !> \details 
   !>  This routine is intended to setup the necessary variables within 
   !>  a core_type for the init atmosphere core.
   !
   !-----------------------------------------------------------------------
   subroutine init_atm_setup_core(core)

      use mpas_derived_types, only : core_type
      use init_atm_core, only : init_atm_core_init, init_atm_core_run, init_atm_core_finalize

      implicit none

      type (core_type), pointer :: core

      core % core_init => init_atm_core_init
      core % core_run => init_atm_core_run
      core % core_finalize => init_atm_core_finalize
      core % define_packages => init_atm_define_packages
      core % setup_packages => init_atm_setup_packages
      core % setup_decompositions => init_atm_setup_decompositions
      core % setup_clock => init_atm_setup_clock
      core % setup_log => init_atm_setup_log
      core % get_mesh_stream => init_atm_get_mesh_stream
      core % setup_immutable_streams => init_atm_setup_immutable_streams
      core % setup_derived_dimensions => init_atm_setup_derived_dimensions
      core % setup_decomposed_dimensions => init_atm_setup_decomposed_dimensions
      core % setup_block => init_atm_setup_block
      core % setup_namelist => init_atm_setup_namelists

      core % Conventions = 'MPAS'
      core % source = 'MPAS'

#include "core_variables.inc"

   end subroutine init_atm_setup_core


   !***********************************************************************
   !
   !  routine init_atm_setup_domain
   !
   !> \brief   Init atmosphere domain setup routine
   !> \author  Doug Jacobsen, Michael Duda
   !> \date    18 March 2015
   !> \details 
   !>  This routine is intended to setup the necessary variables within 
   !>  a domain_type for the init atmosphere core.
   !
   !-----------------------------------------------------------------------
   subroutine init_atm_setup_domain(domain)

      use mpas_derived_types, only : domain_type

      implicit none

      type (domain_type), pointer :: domain

#include "domain_variables.inc"

   end subroutine init_atm_setup_domain


   !***********************************************************************
   !
   !  function init_atm_setup_packages
   !
   !> \brief   Package setup routine
   !> \author  Michael Duda
   !> \date    6 August 2014
   !> \details 
   !>  This routine is responsible for setting up packages for the
   !>  init_atmosphere core. It may use ay logic based on configuration options
   !>  to set packages variables to either .true. or .false. Model fields are
   !>  not allocated until after this routine has been called.
   !
   !-----------------------------------------------------------------------
   function init_atm_setup_packages(configs, streamInfo, packages, iocontext, dminfo) result(ierr)

      use mpas_derived_types, only : mpas_pool_type, mpas_io_context_type, MPAS_streamInfo_type
      use mpas_pool_routines, only : mpas_pool_get_config, mpas_pool_get_package

      implicit none

      type (mpas_pool_type), intent(inout) :: configs
      type (MPAS_streamInfo_type), intent(inout) :: streamInfo
      type (mpas_pool_type), intent(inout) :: packages
      type (mpas_io_context_type), intent(inout) :: iocontext
      type (dm_info), intent(in) :: dminfo
      integer :: ierr
      logical :: lexist

      logical, pointer :: initial_conds, sfc_update, lbcs
      logical, pointer :: gwd_stage_in, gwd_stage_out, vertical_stage_in, vertical_stage_out, met_stage_in, met_stage_out
      logical, pointer :: gwd_gsl_stage_out
      logical, pointer :: config_native_gwd_static, config_static_interp, config_vertical_grid, config_met_interp
      logical, pointer :: config_native_gwd_gsl_static
      logical, pointer :: first_guess_field
      logical, pointer :: microphysics_aerosols
      integer, pointer :: config_init_case
      logical, pointer :: noahmp, config_noahmp_static


      ierr = init_atm_setup_packages_when(configs, packages)
      if (ierr /= 0) then
         return
      end if

      call mpas_pool_get_config(configs, 'config_init_case', config_init_case)
      call mpas_pool_get_config(configs, 'config_static_interp', config_static_interp)
      call mpas_pool_get_config(configs, 'config_native_gwd_static', config_native_gwd_static)
      call mpas_pool_get_config(configs, 'config_native_gwd_gsl_static', config_native_gwd_gsl_static)
      call mpas_pool_get_config(configs, 'config_vertical_grid', config_vertical_grid)
      call mpas_pool_get_config(configs, 'config_met_interp', config_met_interp)

      nullify(initial_conds)
      call mpas_pool_get_package(packages, 'initial_condsActive', initial_conds)

      nullify(sfc_update)
      call mpas_pool_get_package(packages, 'sfc_updateActive', sfc_update)

      nullify(lbcs)
      call mpas_pool_get_package(packages, 'lbcsActive', lbcs)

      nullify(gwd_stage_in)
      call mpas_pool_get_package(packages, 'gwd_stage_inActive', gwd_stage_in)

      nullify(gwd_stage_out)
      call mpas_pool_get_package(packages, 'gwd_stage_outActive', gwd_stage_out)

      nullify(gwd_gsl_stage_out)
      call mpas_pool_get_package(packages, 'gwd_gsl_stage_outActive', gwd_gsl_stage_out)

      nullify(vertical_stage_in)
      call mpas_pool_get_package(packages, 'vertical_stage_inActive', vertical_stage_in)

      nullify(vertical_stage_out)
      call mpas_pool_get_package(packages, 'vertical_stage_outActive', vertical_stage_out)

      nullify(met_stage_in)
      call mpas_pool_get_package(packages, 'met_stage_inActive', met_stage_in)

      nullify(met_stage_out)
      call mpas_pool_get_package(packages, 'met_stage_outActive', met_stage_out)

      nullify(microphysics_aerosols)
      call mpas_pool_get_package(packages, 'microphysics_aerosolsActive', microphysics_aerosols)

      if (.not. associated(initial_conds) .or. &
          .not. associated(sfc_update) .or. &
          .not. associated(gwd_stage_in) .or. &
          .not. associated(gwd_stage_out) .or. &
          .not. associated(gwd_gsl_stage_out) .or. &
          .not. associated(vertical_stage_in) .or. &
          .not. associated(vertical_stage_out) .or. &
          .not. associated(met_stage_in) .or. &
          .not. associated(met_stage_out) .or. &
          .not. associated(microphysics_aerosols)) then
         call mpas_log_write('********************************************************************************', messageType=MPAS_LOG_ERR)
         call mpas_log_write('* Error while setting up packages for init_atmosphere core.',                      messageType=MPAS_LOG_ERR)
         call mpas_log_write('********************************************************************************', messageType=MPAS_LOG_ERR)
         ierr = 1
         return
      end if

      if (config_init_case == 8) then
         initial_conds = .false.
         sfc_update = .true.
      else
         initial_conds = .true.
         sfc_update = .false.
      end if

      if (config_init_case == 9) then
         lbcs = .true.
         microphysics_aerosols = .false.
         inquire(file="QNWFA_QNIFA_SIGMA_MONTHLY.dat",exist=lexist)
         if(lexist) microphysics_aerosols = .true.
      else
         lbcs = .false.
         microphysics_aerosols = .false.
      end if

      if (config_init_case == 7) then

         !
         ! The logic here is a little convoluted
         ! For input, we want to read in fields from all earlier stages, except if those earlier stages are being run now
         ! For output, we want to output the fields that were computed in a stage and all of those from earlier stages
         !
         gwd_stage_in = config_native_gwd_static .and. &
                        (.not. config_static_interp)
         gwd_stage_out = config_native_gwd_static
         gwd_gsl_stage_out = config_native_gwd_gsl_static
         vertical_stage_in = config_vertical_grid .and. &
                             (.not. config_native_gwd_static) .and. &
                             (.not. config_static_interp)
         vertical_stage_out = config_vertical_grid
         met_stage_in = config_met_interp .and. &
                        (.not. config_native_gwd_static) .and. &
                        (.not. config_static_interp) .and. &
                        (.not. config_vertical_grid)
         met_stage_out = config_met_interp

         microphysics_aerosols = .false.
         inquire(file="QNWFA_QNIFA_SIGMA_MONTHLY.dat",exist=lexist)
         if((lexist .and. met_stage_out) .or. (lexist .and. met_stage_in)) microphysics_aerosols = .true.

      else if (config_init_case == 8) then
         gwd_stage_in = .false.
         gwd_stage_out = .false.
         gwd_gsl_stage_out = .false.
         vertical_stage_in = .true.
         vertical_stage_out = .false.
         met_stage_in = .false.
         met_stage_out = .false.

      !
      ! When interpolating LBC fields, we need all inputs that would be needed for the interpolation
      ! of ICs, so met_stage_in = .true.
      !
      else if (config_init_case == 9) then
         gwd_stage_in = .false.
         gwd_stage_out = .false.
         gwd_gsl_stage_out = .false.
         vertical_stage_in = .false.
         vertical_stage_out = .false.
         met_stage_in = .true.
         met_stage_out = .true.

         microphysics_aerosols = .false.
         inquire(file="QNWFA_QNIFA_SIGMA_MONTHLY.dat",exist=lexist)
         if((lexist .and. met_stage_out) .or. (lexist .and. met_stage_in)) microphysics_aerosols = .true.

         initial_conds = .false.   ! Also, turn off the initial_conds package to avoid writing the IC "output" stream

      else if (config_init_case == 13) then
         gwd_stage_in = .false.
         gwd_stage_out = .false.
         gwd_gsl_stage_out = .false.
         vertical_stage_in = .false.
         vertical_stage_out = .true.
         met_stage_in = .false.
         met_stage_out = .false.

      else
         gwd_stage_in = .false.
         gwd_stage_out = .false.
         gwd_gsl_stage_out = .false.
         vertical_stage_in = .false.
         vertical_stage_out = .false.
         met_stage_in = .false.
         met_stage_out = .true.
      end if

      !
      ! Package for 3-d first-guess atmospheric and land-surface fields is only active if
      ! we are interpolating real-data ICs or LBCs
      !
      nullify(first_guess_field)
      call mpas_pool_get_package(packages, 'first_guess_fieldActive', first_guess_field)

      first_guess_field = .false.
      if ((config_init_case == 7 .and. config_met_interp) .or. config_init_case == 9) then
         first_guess_field = .true.
      end if

      !
      ! Noah-MP
      !
      nullify(config_noahmp_static)
      call mpas_pool_get_config(configs, 'config_noahmp_static', config_noahmp_static)

      nullify(noahmp)
      call mpas_pool_get_package(packages, 'noahmpActive', noahmp)

      if (associated(config_noahmp_static) .and. associated(noahmp)) then
         noahmp = config_noahmp_static
      else
         call mpas_log_write('********************************************************************************', &
                             messageType=MPAS_LOG_ERR)
         call mpas_log_write('* Error while setting up packages for init_atmosphere core:',                      &
                             messageType=MPAS_LOG_ERR)
         call mpas_log_write('*    Either the ''noahmp'' package or the ''config_noahmp_static'' namelist',      &
                             messageType=MPAS_LOG_ERR)
         call mpas_log_write('*    option is not defined.', &
                             messageType=MPAS_LOG_ERR)
         call mpas_log_write('********************************************************************************', &
                             messageType=MPAS_LOG_ERR)
         ierr = 1
         return
      end if

      if (config_init_case == 7 .or. config_init_case == 9) then
         if (met_stage_out) then
             call setup_hydrometeor_packages(packages, configs, dminfo, ierr)
         end if
      end if

   end function init_atm_setup_packages


   !***********************************************************************
   !
   !  function setup_hydrometeor_packages
   !
   !> \brief   Set up packages for hydrometeors based on contents of intermediate file
   !> \author  Michael Duda
   !> \date    27 May 2026
   !> \details
   !> This routine is responsible for setting up packages for hydrometeor species (qc,
   !> qr, etc.) based on the value of the 'config_active_hydrometeors' namelist option,
   !> and possibly on the contents of the initial intermediate file (i.e., the
   !> intermediate file valid at the start time of a simulation; the assumption is
   !> that all intermediate files contain the same set of fields).
   !>
   !> 1) If config_active_hydrometeors is set to 'detect_automatically', packages will
   !>    be set up by scanning through the fields in the initial intermediate file,
   !>    with packages being activated only for those hydrometeors that are present in
   !>    the file.
   !>
   !> 2) Alternatively, and to avoid the need to scan through all fields in the
   !>    initial intermediate file, if config_active_hydrometeors is set to a
   !>    semicolon-separated list of zero or more hydrometeor names (e.g., 'qc;qr'),
   !>    packages for those specified hydrometeors will be active, and the packages
   !>    for all unspecified hydrometeors will be inactive.
   !>
   !> If the setup of all hydrometeor packages is successful, a value of 0 is returned
   !> in the ierr output argument; otherwise, a non-zero value is returned.
   !
   !-----------------------------------------------------------------------
   subroutine setup_hydrometeor_packages(packages, configs, dminfo, ierr)

      use init_atm_read_met, only : read_met_init, read_next_met_field, read_met_close, met_data
      use mpas_timer, only : mpas_timer_start, mpas_timer_stop

      implicit none

      type (mpas_pool_type), intent(inout) :: packages
      type (mpas_pool_type), intent(in) :: configs
      type (dm_info), intent(in) :: dminfo
      integer, intent(out) :: ierr

      character(len=StrKIND), pointer :: config_active_hydrometeors
      character(len=StrKIND), pointer :: config_met_prefix, config_start_time
      integer :: istatus
      type (met_data) :: field

      integer :: i
      character(len=:), dimension(:), allocatable :: hydrometeors

      logical, pointer :: qc, qr


      call mpas_timer_start('setup_hydrometeor_packages')

      ierr = 0

      call mpas_pool_get_config(configs, 'config_active_hydrometeors', config_active_hydrometeors)

      if (.not. associated(config_active_hydrometeors)) then
         call mpas_log_write( &
                 "The namelist option 'config_active_hydrometeors' could not be found when setting up hydrometeor packages.", &
                 messageType=MPAS_LOG_ERR)

         ierr = 1
         call mpas_timer_stop('setup_hydrometeor_packages')
         return
      end if

      call mpas_pool_get_package(packages, 'qcActive', qc)
      call mpas_pool_get_package(packages, 'qrActive', qr)

      if (.not. associated(qc) &
          .or. .not. associated(qr) &
         ) then

         call mpas_log_write('One or more packages could not be found when setting up hydrometeor packages.', &
                             messageType=MPAS_LOG_ERR)

         ierr = 1
         call mpas_timer_stop('setup_hydrometeor_packages')
         return
      end if

      qc = .false.
      qr = .false.

      if (trim(config_active_hydrometeors) == 'detect_automatically') then

         call mpas_pool_get_config(configs, 'config_met_prefix', config_met_prefix)
         call mpas_pool_get_config(configs, 'config_start_time', config_start_time)

         if (.not. associated(config_met_prefix) &
             .or. .not. associated(config_start_time) &
            ) then

            call mpas_log_write( &
                    'One or more namelist options could not be found when setting up hydrometeor packages.', &
                    messageType=MPAS_LOG_ERR)

            ierr = 1
            call mpas_timer_stop('setup_hydrometeor_packages')
            return
         end if

         call mpas_log_write('Setting up hydrometeor packages from '//trim(config_met_prefix)//':'//config_start_time(1:13))

         if (dminfo % my_proc_id == IO_NODE) then
            call read_met_init(trim(config_met_prefix), .false., config_start_time(1:13), ierr)

            if (ierr == 0) then
               call read_next_met_field(field, istatus)
            else
               call mpas_log_write('Could not open intermediate file ' &
                                   //trim(config_met_prefix)//':'//config_start_time(1:13) &
                                   //' to set up hydrometeor packages.', &
                                   messageType=MPAS_LOG_ERR)
               istatus = 1
            end if

            do while (istatus == 0)
               if (trim(field % field) == 'QC') then
                  qc = .true.
               else if (trim(field % field) == 'QR') then
                  qr = .true.
               end if

               deallocate(field % slab)

               ! If all packages are true, there is no point in scanning through the rest
               ! of the intermediate file, since this loop over fields can only switch packages
               ! from false to true.
               if (qc .and. qr) exit

               call read_next_met_field(field, istatus)
            end do

            call read_met_close()

            call mpas_dmpar_bcast_int(dminfo, ierr)
            call mpas_dmpar_bcast_logical(dminfo, qc)
            call mpas_dmpar_bcast_logical(dminfo, qr)
         else
            call mpas_dmpar_bcast_int(dminfo, ierr)
            call mpas_dmpar_bcast_logical(dminfo, qc)
            call mpas_dmpar_bcast_logical(dminfo, qr)
         end if

      else

         call mpas_log_write('Setting up hydrometeor packages from config_active_hydrometeors list: ' &
                             //trim(config_active_hydrometeors))

         call mpas_split_string_new(trim(config_active_hydrometeors), ';', hydrometeors)

         do i = 1, size(hydrometeors)
            select case (trim(hydrometeors(i)))
               case ('qc')
                  qc = .true.
               case ('qr')
                  qr = .true.
               case default
                  call mpas_log_write('Unrecognized hydrometeor '//trim(hydrometeors(i)) &
                                      //' found in config_active_hydrometeors', &
                                      messageType=MPAS_LOG_WARN)
            end select
         end do

         deallocate(hydrometeors)

      end if

      if (ierr /= 0) then
         call mpas_log_write('Failed to set up hydrometeor packages.', messageType=MPAS_LOG_ERR)
         call mpas_timer_stop('setup_hydrometeor_packages')
         return
      end if

      call mpas_log_write('  QC = $l', logicArgs=[qc])
      call mpas_log_write('  QR = $l', logicArgs=[qr])
      call mpas_log_write('----- done setting up hydrometeor packages -----')
      call mpas_log_write('')

      call mpas_timer_stop('setup_hydrometeor_packages')

   end subroutine setup_hydrometeor_packages


   !***********************************************************************
   !
   !  function init_atm_setup_clock
   !
   !> \brief   Simulation clock setup routine
   !> \author  Michael Duda
   !> \date    6 August 2014
   !> \details 
   !>  The purpose of this routine is to allow the core to set up a simulation
   !>  clock that will be used by the I/O subsystem for timing reads and writes
   !>  of I/O streams.
   !>  This routine is called from the superstructure after the framework 
   !>  has been initialized but before any fields have been allocated and 
   !>  initial fields have been read from input files. However, all namelist
   !>  options are available.
   !
   !-----------------------------------------------------------------------
   function init_atm_setup_clock(core_clock, configs) result(ierr)

      use mpas_timekeeping, only : mpas_set_time, mpas_set_timeInterval, mpas_create_clock
      use mpas_derived_types, only : MPAS_Clock_type, MPAS_Time_type, MPAS_Timeinterval_type, mpas_pool_type
      use mpas_pool_routines, only : mpas_pool_get_config
      use mpas_kind_types, only : StrKIND

      implicit none

      type (MPAS_Clock_type), intent(inout) :: core_clock
      type (mpas_pool_type), intent(inout) :: configs
      integer :: ierr

      character(len=StrKIND), pointer :: config_start_time, config_stop_time
      integer, pointer :: config_fg_interval

      type (MPAS_Time_type)         :: start_time, stop_time
      type (MPAS_TimeInterval_type) :: dt


      ierr = 0

      call mpas_pool_get_config(configs, 'config_start_time', config_start_time)
      call mpas_pool_get_config(configs, 'config_stop_time', config_stop_time)
      call mpas_pool_get_config(configs, 'config_fg_interval', config_fg_interval)

      call mpas_set_time(start_time, dateTimeString=trim(config_start_time))
      call mpas_set_time(stop_time, dateTimeString=trim(config_stop_time))
      call mpas_set_timeInterval(dt, S=config_fg_interval)
      call mpas_create_clock(core_clock, start_time, dt, stopTime=stop_time)

   end function init_atm_setup_clock


   !***********************************************************************
   !
   !  function init_atm_setup_log
   !
   !> \brief   Log setup routine
   !> \author  Matt Hoffman
   !> \date    14 February 2017
   !> \details
   !>  The purpose of this routine is to set up the logging manager
   !>  and allow the core to specify details of the configuration.
   !
   !-----------------------------------------------------------------------
   function init_atm_setup_log(logInfo, domain, unitNumbers) result(iErr)!{{{

      use mpas_derived_types, only : mpas_log_type, domain_type
      use mpas_log, only : mpas_log_init, mpas_log_open
      use mpas_framework, only : mpas_framework_report_settings

      implicit none

      type (mpas_log_type), intent(inout), pointer :: logInfo  !< logging information object to set up
      type (domain_type), intent(in), pointer :: domain  !< domain object to provide info for setting up log manager
      integer, dimension(2), intent(in), optional :: unitNumbers !< Fortran unit numbers to use for output and error logs
      integer :: iErr

      ! Local variables
      integer :: local_err

      iErr = 0

      ! Initialize log manager
      call mpas_log_init(logInfo, domain, unitNumbers=unitNumbers, err=local_err)
      iErr = ior(iErr, local_err)

      ! Set core specific options here
      ! (At present, there are not any.  There could eventually be choices about the file naming conventions
      !  or other settings controlling behavior.)

      ! After core has had a chance to modify log defaults, open the output log
      call mpas_log_open(err=local_err)
      iErr = ior(iErr, local_err)

      call mpas_log_write('')
      call mpas_log_write('MPAS Init-Atmosphere Version '//trim(domain % core % modelVersion))
      call mpas_log_write('')

      call mpas_framework_report_settings(domain)

   end function init_atm_setup_log!}}}


   !***********************************************************************
   !
   !  function init_atm_get_mesh_stream
   !
   !> \brief   Returns the name of the stream containing mesh information
   !> \author  Michael Duda
   !> \date    8 August 2014
   !> \details 
   !>  This routine returns the name of the I/O stream containing dimensions,
   !>  attributes, and mesh fields needed by the framework bootstrapping 
   !>  routine. At the time this routine is called, only namelist options 
   !>  are available.
   !
   !-----------------------------------------------------------------------
   function init_atm_get_mesh_stream(configs, streamInfo, stream) result(ierr)

      use mpas_kind_types, only : StrKIND
      use mpas_derived_types, only : mpas_pool_type, MPAS_streamInfo_type
      use mpas_pool_routines, only : mpas_pool_get_config

      implicit none

      type (mpas_pool_type), intent(inout) :: configs
      type (MPAS_streamInfo_type), intent(inout) :: streamInfo
      character(len=StrKIND), intent(out) :: stream
      integer :: ierr

      ierr = 0

      write(stream,'(a)') 'input'

   end function init_atm_get_mesh_stream


   !***********************************************************************
   !
   !  function init_atm_setup_decompositions
   !
   !> \brief   Decomposition setup function
   !> \author  Doug Jacobsen, Michael Duda
   !> \date    11 March 2015
   !> \details 
   !>  This function is intended to create the decomposition list within a
   !>  domain type, and register any decompositons the core wants within it.
   !
   !-----------------------------------------------------------------------
   function init_atm_setup_decompositions(decompList) result(ierr)

      use mpas_derived_types, only : mpas_decomp_list, mpas_decomp_function, MPAS_DECOMP_NOERR
      use mpas_decomp, only : mpas_decomp_create_decomp_list, mpas_decomp_register_method, &
                              mpas_uniform_decomp 

      implicit none

      type (mpas_decomp_list), pointer :: decompList
      integer :: ierr

      procedure (mpas_decomp_function), pointer :: decompFunc

      ierr = 0

      call mpas_decomp_create_decomp_list(decompList)

      decompFunc => mpas_uniform_decomp

      call mpas_decomp_register_method(decompList, 'uniform', decompFunc, ierr)

      if ( ierr == MPAS_DECOMP_NOERR ) then
         ierr = 0
      end if

   end function init_atm_setup_decompositions


   !***********************************************************************
   !
   !  function init_atm_setup_block
   !
   !> \brief   Block setup function
   !> \author  Doug Jacobsen
   !> \date    03/18/2015
   !> \details 
   !>  This function is a wrapper function to properly setup a block to be a
   !>  init atmosphere core block.
   !
   !-----------------------------------------------------------------------
   function init_atm_setup_block(block) result(ierr)

      use mpas_derived_types, only : block_type

      implicit none

      type (block_type), pointer :: block
      integer :: ierr

      ierr = 0

      call init_atm_generate_structs(block, block % structs, block % dimensions, block % packages)

   end function init_atm_setup_block


   !-----------------------------------------------------------------------
   !  routine mpas_split_string_new
   !
   !> \brief Splits a string at a specified delimiter, returning an array of strings
   !> \author Michael Duda
   !> \date   28 May 2026
   !> \details
   !>  This routine takes as input a string and a delimiter character, and returns an
   !>  array of sub-strings from the input string that are separated by one or more
   !>  delimiter characters.
   !>
   !>  If more than one delimiter character appears consecutively, no empty string
   !>  in the output subStrings array is generated.
   !>
   !>  The length of the strings in the output subStrings argument is equal to the
   !>  length of the longest substring in the input string.
   !
   !-----------------------------------------------------------------------
   subroutine mpas_split_string_new(string, delimiter, subStrings)

      implicit none

      ! Arguments
      character(len=*), intent(in) :: string
      character, intent(in) :: delimiter
      character(len=:), dimension(:), allocatable, intent(inout) :: subStrings

      ! Local variables
      integer :: i, j, n_strs, strlen, max_strlen


      i = 1
      n_strs = 0
      strlen = 0
      max_strlen = 1
      PARSE_LOOP: do while (i <= len(string))
         do while (string(i:i) == delimiter)
            i = i + 1
            if (i > len(string)) exit PARSE_LOOP
         end do

         n_strs = n_strs + 1
         strlen = 0
         do while (string(i:i) /= delimiter)
            i = i + 1
            strlen = strlen + 1
            if (i > len(string)) exit
         end do
         max_strlen = max(strlen, max_strlen)
      end do PARSE_LOOP

      if (allocated(subStrings)) deallocate(subStrings)
      allocate(character(len=max_strlen) :: subStrings(n_strs))

      i = 1
      n_strs = 0
      COPY_LOOP: do while (i <= len(string))
         do while (string(i:i) == delimiter)
            i = i + 1
            if (i > len(string)) exit COPY_LOOP
         end do

         n_strs = n_strs + 1
         j = 1
         do while (string(i:i) /= delimiter)
            subStrings(n_strs)(j:j) = string(i:i)
            i = i + 1
            j = j + 1
            if (i > len(string)) exit
         end do
         subStrings(n_strs)(j:max_strlen) = ''
      end do COPY_LOOP

   end subroutine mpas_split_string_new


#include "setup_immutable_streams.inc"

#include "block_dimension_routines.inc"

#include "define_packages.inc"

#include "setup_packages.inc"

#include "structs_and_variables.inc"

#include "namelist_call.inc"

#include "namelist_defines.inc"

end module init_atm_core_interface
