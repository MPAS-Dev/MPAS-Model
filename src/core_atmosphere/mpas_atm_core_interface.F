! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
module atm_core_interface

   use mpas_attlist
   use mpas_derived_types
   use mpas_pool_routines
   use mpas_dmpar
   use mpas_io_units

   integer, save, private :: radiation_ranks_per_node, dynamics_ranks_per_node
   integer, save, private :: ranks_per_node
   integer, save, private :: local_rank = -1


   contains


   !***********************************************************************
   !
   !  routine atm_setup_core
   !
   !> \brief   Atmosphere core setup routine
   !> \author  Doug Jacobsen, Michael Duda
   !> \date    18 March 2015
   !> \details 
   !>  This routine is intended to setup the necessary variables within 
   !>  a core_type for the atm core.
   !
   !-----------------------------------------------------------------------
   subroutine atm_setup_core(core)

      use mpas_derived_types, only : core_type
      use atm_core, only : atm_core_init, atm_core_run, atm_core_finalize

      implicit none

      type (core_type), pointer :: core

      core % core_init => atm_core_init
      core % core_run => atm_core_run
      core % core_finalize => atm_core_finalize
      core % define_packages => atm_define_packages
      core % setup_packages => atm_setup_packages
      core % setup_decompositions => atm_setup_decompositions
      core % setup_clock => atm_setup_clock
      core % setup_log => atm_setup_log
      core % get_mesh_stream => atm_get_mesh_stream
      core % setup_immutable_streams => atm_setup_immutable_streams
      core % setup_derived_dimensions => atm_setup_derived_dimensions
      core % setup_decomposed_dimensions => atm_setup_decomposed_dimensions
      core % setup_block => atm_setup_block
      core % setup_namelist => atm_setup_namelists
      core % component_role => atm_component_role
      core % component_role_leader => atm_component_role_leader

      core % Conventions = 'MPAS'
      core % source = 'MPAS'

#include "inc/core_variables.inc"

   end subroutine atm_setup_core


   !***********************************************************************
   !
   !  routine atm_component_role
   !
   !> \brief Define roles for each MPI task and OpenMP thread
   !> \author Michael Duda
   !> \date 2 January 2018
   !> \details 
   !>  This routine returns an integer value representing the "role" for
   !>  any given MPI task and OpenMP thread.
   !
   !-----------------------------------------------------------------------
   integer function atm_component_role(self)

      use mpas_derived_types, only : mpas_coupler_type
      use mpas_kind_types, only : StrKIND
      use mpas_abort, only : mpas_dmpar_global_abort
      use mpas_intracoupler, only : ROLE_INTEGRATE, ROLE_RADIATION

      implicit none

      class (mpas_coupler_type), intent(in) :: self

      character(len=StrKIND) :: radiation_ranks_per_node_str
      character(len=StrKIND) :: dynamics_ranks_per_node_str

      integer :: istat


#if 1
      !
      ! If this routine has not yet been called, read environment variables
      !
      if (local_rank == -1) then
          call get_environment_variable('MPAS_RADIATION_RANKS_PER_NODE', value=radiation_ranks_per_node_str, status=istat)
          if (istat /= 0) then
              call mpas_dmpar_global_abort('Error: Could not read environment variable MPAS_RADIATION_RANKS_PER_NODE')
          end if

          call get_environment_variable('MPAS_DYNAMICS_RANKS_PER_NODE', value=dynamics_ranks_per_node_str, status=istat)
          if (istat /= 0) then
              call mpas_dmpar_global_abort('Error: Could not read environment variable MPAS_DYNAMICS_RANKS_PER_NODE')
          end if

          read(radiation_ranks_per_node_str, *) radiation_ranks_per_node
          read(dynamics_ranks_per_node_str, *) dynamics_ranks_per_node

          ranks_per_node = radiation_ranks_per_node + dynamics_ranks_per_node
          local_rank = mod(self % intercomm_rank, ranks_per_node)
      end if

      if (local_rank < dynamics_ranks_per_node) then
          atm_component_role = ROLE_INTEGRATE
      else
          atm_component_role = ROLE_RADIATION
      end if
#else
      atm_component_role = ior(ROLE_INTEGRATE, ROLE_RADIATION)
#endif

   end function atm_component_role


   !***********************************************************************
   !
   !  routine atm_component_role_leader
   !
   !> \brief Identifies the "leader" task for a given role
   !> \author Michael Duda
   !> \date 3 January 2018
   !> \details 
   !>  Given a role, this function returns the MPI task that acts as the leader
   !>  for that role. This is needed to establish MPI intercommunicators
   !>  between two roles.
   !
   !-----------------------------------------------------------------------
   integer function atm_component_role_leader(self, role)

      use mpas_derived_types, only : mpas_coupler_type
      use mpas_intracoupler, only : ROLE_INTEGRATE

      implicit none

      class (mpas_coupler_type), intent(in) :: self
      integer, intent(in) :: role
      integer :: number_of_dynamics_rank


#if 1
      if (iand(role, ROLE_INTEGRATE) == ROLE_INTEGRATE) then
          atm_component_role_leader = 0
      else
          atm_component_role_leader = dynamics_ranks_per_node
      end if
#else
      atm_component_role_leader = 0
#endif

   end function atm_component_role_leader


   !***********************************************************************
   !
   !  routine atm_setup_domain
   !
   !> \brief   Atmosphere domain setup routine
   !> \author  Doug Jacobsen, Michael Duda
   !> \date    18 March 2015
   !> \details 
   !>  This routine is intended to setup the necessary variables within 
   !>  a domain_type for the init atm core.
   !
   !-----------------------------------------------------------------------
   subroutine atm_setup_domain(domain)

      use mpas_derived_types, only : domain_type

      implicit none

      type (domain_type), pointer :: domain

#include "inc/domain_variables.inc"

   end subroutine atm_setup_domain


   !***********************************************************************
   !
   !  function atm_setup_packages
   !
   !> \brief   Package setup routine
   !> \author  Michael Duda
   !> \date    6 August 2014
   !> \details 
   !>  This routine is responsible for setting up packages for the
   !>  atmosphere core. It may use ay logic based on configuration options
   !>  to set packages variables to either .true. or .false. Model fields are
   !>  not allocated until after this routine has been called.
   !
   !-----------------------------------------------------------------------
   function atm_setup_packages(configs, packages, iocontext) result(ierr)

      use mpas_dmpar
      use mpas_derived_types, only : mpas_pool_type, mpas_io_context_type
      use mpas_pool_routines, only : mpas_pool_get_config, mpas_pool_get_package

#ifdef DO_PHYSICS
      use mpas_atmphys_control
      use mpas_atmphys_packages
#endif

      implicit none

      type (mpas_pool_type), intent(inout) :: configs
      type (mpas_pool_type), intent(inout) :: packages
      type (mpas_io_context_type), intent(inout) :: iocontext
      integer :: ierr

      logical, pointer :: iauActive
      character(len=StrKIND), pointer :: config_iau_option

      ierr = 0

      nullify(config_iau_option)
      call mpas_pool_get_config(configs, 'config_IAU_option', config_iau_option)

      nullify(iauActive)
      call mpas_pool_get_package(packages, 'iauActive', iauActive)

      if (trim(config_iau_option) /= 'off') then
         iauActive = .true.
      else
         iauActive = .false.
      end if

#ifdef DO_PHYSICS
      !check that all the physics options are correctly defined and that at
      !least one physics parameterization is called (using the logical moist_physics):
      call physics_namelist_check(configs)

      ierr = atmphys_setup_packages(configs,packages,iocontext)
      if(ierr /= 0) then
         call mpas_log_write('Package setup failed for atmphys in core_atmosphere', messageType=MPAS_LOG_ERR)
      endif
#endif

   end function atm_setup_packages


   !***********************************************************************
   !
   !  function atm_setup_clock
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
   function atm_setup_clock(core_clock, configs) result(ierr)

      use mpas_derived_types, only : MPAS_Clock_type, mpas_pool_type
      use atm_core, only : atm_simulation_clock_init

      implicit none

      type (MPAS_Clock_type), intent(inout) :: core_clock
      type (mpas_pool_type), intent(inout) :: configs
      integer :: ierr

      ierr = 0

      call atm_simulation_clock_init(core_clock, configs, ierr)

   end function atm_setup_clock


   !***********************************************************************
   !
   !  function atm_setup_log
   !
   !> \brief   Log setup routine
   !> \author  Matt Hoffman
   !> \date    14 February 2017
   !> \details
   !>  The purpose of this routine is to set up the logging manager
   !>  and allow the core to specify details of the configuration.
   !
   !-----------------------------------------------------------------------
   function atm_setup_log(logInfo, domain) result(iErr)!{{{

      use mpas_derived_types, only : mpas_log_type, domain_type, mpas_coupler_type
      use mpas_log, only : mpas_log_init, mpas_log_open

      implicit none

      type (mpas_log_type), intent(inout), pointer :: logInfo  !< logging information object to set up
      type (domain_type), intent(in), pointer :: domain  !< domain object to provide info for setting up log manager
      integer :: iErr

      ! Local variables
      integer :: local_err
      type (mpas_coupler_type), pointer :: mpas_cpl

      iErr = 0

      ! Initialize log manager
      call mpas_log_init(logInfo, domain, err=local_err)
      iErr = ior(iErr, local_err)

      ! Set core specific options here
      ! (At present, there are not any.  There could eventually be choices about the file naming conventions
      !  or other settings controlling behavior.)

      !
      ! Make log filenames specific to the role of each MPI task
      !
      mpas_cpl => domain % mpas_cpl
      write(logInfo % outputLog % filename, '(a,i2.2,a,i4.4,a)') 'log.atmosphere.role', mpas_cpl % role(), '.', domain % dminfo % my_proc_id, '.out'
      write(logInfo % errorLog % filename, '(a,i2.2,a,i4.4,a)') 'log.atmosphere.role', mpas_cpl % role(), '.', domain % dminfo % my_proc_id, '.err'

      ! After core has had a chance to modify log defaults, open the output log
      call mpas_log_open(err=local_err)
      iErr = ior(iErr, local_err)

      call mpas_log_write('')
#ifdef SINGLE_PRECISION
      call mpas_log_write('Using default single-precision reals')
#else
      call mpas_log_write('Using default double-precision reals')
#endif
      call mpas_log_write('')

   end function atm_setup_log!}}}


   !***********************************************************************
   !
   !  function atm_get_mesh_stream
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
   function atm_get_mesh_stream(configs, stream) result(ierr)

      use mpas_kind_types, only : StrKIND
      use mpas_derived_types, only : mpas_pool_type
      use mpas_pool_routines, only : mpas_pool_get_config

      implicit none

      type (mpas_pool_type), intent(inout) :: configs
      character(len=StrKIND), intent(out) :: stream
      integer :: ierr

      logical, pointer :: config_do_restart

      ierr = 0

      call mpas_pool_get_config(configs, 'config_do_restart', config_do_restart)

      if (.not. associated(config_do_restart)) then
         call mpas_log_write('config_do_restart was not found when defining mesh stream.', messageType=MPAS_LOG_ERR)
         ierr = 1
      else if (config_do_restart) then
         write(stream,'(a)') 'restart'
      else
         write(stream,'(a)') 'input'
      end if

   end function atm_get_mesh_stream


   !***********************************************************************
   !
   !  function atm_setup_decompositions
   !
   !> \brief   Decomposition setup function
   !> \author  Doug Jacobsen, Michael Duda
   !> \date    11 March 2015
   !> \details 
   !>  This function is intended to create the decomposition list within a
   !>  domain type, and register any decompositons the core wants within it.
   !
   !-----------------------------------------------------------------------
   function atm_setup_decompositions(decompList) result(ierr)

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

   end function atm_setup_decompositions


   !***********************************************************************
   !
   !  function atm_setup_block
   !
   !> \brief   Block setup function
   !> \author  Doug Jacobsen, Michael Duda
   !> \date    03/18/2015
   !> \details 
   !>  This function is a wrapper function to properly setup a block to be
   !>  an atmosphere core block.
   !
   !-----------------------------------------------------------------------
   function atm_setup_block(block) result(ierr)

      use mpas_derived_types, only : block_type

      implicit none

      type (block_type), pointer :: block
      integer :: ierr

      ierr = 0

      call atm_generate_structs(block, block % structs, block % dimensions, block % packages)

   end function atm_setup_block


#include "inc/setup_immutable_streams.inc"

#include "inc/block_dimension_routines.inc"

#include "inc/define_packages.inc"

#include "inc/structs_and_variables.inc"

#include "inc/namelist_call.inc"

#include "inc/namelist_defines.inc"

end module atm_core_interface
