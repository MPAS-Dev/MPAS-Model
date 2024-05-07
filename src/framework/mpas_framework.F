! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!-----------------------------------------------------------------------
!  mpas_framework
!
!> \brief MPAS Framework routines
!> \author Michael Duda, Doug Jacobsen
!> \date   03/26/13
!> \details
!>  This module contains all routines related to the general MPAS framework interface.
!
!-----------------------------------------------------------------------
module mpas_framework

   use mpas_dmpar
   use mpas_derived_types
   use mpas_domain_routines
   use mpas_pool_routines
   use mpas_timer
   use mpas_timekeeping
   use mpas_io
   use mpas_io_units
   use mpas_block_decomp

   private :: report_acc_devices


   contains


!-----------------------------------------------------------------------
!  routine mpas_framework_init_phase1
!
!> \brief MPAS framework initialization phase 1 routine.
!> \author Michael Duda, Doug Jacobsen
!> \date   03/26/13
!> \details
!>  This routine initializes the first parts of MPAS framework. It initializes
!>  MPI, the log unit numbers.
!
!-----------------------------------------------------------------------
   subroutine mpas_framework_init_phase1(dminfo, external_comm)!{{{

#ifdef MPAS_USE_MPI_F08
      use mpi_f08, only : MPI_Comm
#endif

      implicit none

      type (dm_info), pointer :: dminfo
#ifdef MPAS_USE_MPI_F08
      type (MPI_Comm), intent(in), optional :: external_comm
#else
      integer, intent(in), optional :: external_comm
#endif

      allocate(dminfo)
      call mpas_dmpar_init(dminfo, external_comm)

   end subroutine mpas_framework_init_phase1!}}}

!-----------------------------------------------------------------------
!  routine mpas_framework_init_phase2
!
!> \brief MPAS framework initialization phase 2 routine.
!> \author Michael Duda, Doug Jacobsen
!> \date   03/26/13
!> \details
!>  This routine finalizes the initialization of the MPAS framework. It calls initializes 
!>  the time keeper, and the IO infrastructure.
!
!-----------------------------------------------------------------------
   subroutine mpas_framework_init_phase2(domain, io_system, calendar)!{{{

      use mpas_log, only : mpas_log_write
      use mpas_derived_types, only : MPAS_LOG_CRIT

      implicit none

      type (domain_type), pointer :: domain

#ifdef MPAS_PIO_SUPPORT
      type (iosystem_desc_t), optional, pointer :: io_system
#else
      integer, optional, pointer :: io_system
#endif
      character(len=*), intent(in), optional :: calendar

      character(len=StrKIND), pointer :: config_calendar_type
      integer, pointer :: config_pio_num_iotasks, config_pio_stride
      integer :: pio_num_iotasks
      integer :: pio_stride

      call mpas_timer_init(domain)

#ifdef MPAS_DEBUG
      call mpas_pool_set_error_level(MPAS_POOL_WARN)
#endif

      if (present(calendar)) then
         call mpas_timekeeping_init(calendar)
      else
         call mpas_pool_get_config(domain % configs, 'config_calendar_type', config_calendar_type)
         call mpas_timekeeping_init(config_calendar_type)
      end if

      !
      ! Note: pio_num_iotasks and pio_stride are only used in MPAS_io_init if io_system is
      !       not present. In stand-alone configurations, we expect that io_system will not
      !       be present and that pio_num_iotasks and pio_stride will be available from
      !       the namelist; in other systems, a PIO io_system may be provided.
      !
      if (.not. present(io_system)) then
         call mpas_pool_get_config(domain % configs, 'config_pio_num_iotasks', config_pio_num_iotasks)
         call mpas_pool_get_config(domain % configs, 'config_pio_stride', config_pio_stride)
         pio_num_iotasks = config_pio_num_iotasks
         pio_stride = config_pio_stride

         !
         ! If at most one of config_pio_num_iotasks and config_io_stride are zero, compute
         ! a sensible value for the zero-valued option
         !
         if (pio_num_iotasks == 0 .and. pio_stride == 0) then
            call mpas_log_write('Namelist options config_pio_num_iotasks and config_pio_stride cannot both be zero.', &
                                messageType=MPAS_LOG_CRIT)
         else if (pio_num_iotasks == 0) then
            pio_num_iotasks = domain % dminfo % nprocs / pio_stride
         else if (pio_stride == 0) then
            pio_stride = domain % dminfo % nprocs / pio_num_iotasks
         end if

         call mpas_log_write('')
         call mpas_log_write('----- I/O task configuration: -----')
         call mpas_log_write('')
         call mpas_log_write('    I/O task count  = $i', intArgs=[pio_num_iotasks])
         call mpas_log_write('    I/O task stride = $i', intArgs=[pio_stride])
         call mpas_log_write('')
      else
         pio_num_iotasks = -1    ! Not used when external io_system is provided
         pio_stride      = -1    ! Not used when external io_system is provided
      end if

      domain % ioContext % dminfo => domain % dminfo

      call MPAS_io_init(domain % ioContext, pio_num_iotasks, pio_stride, io_system)

   end subroutine mpas_framework_init_phase2!}}}


!-----------------------------------------------------------------------
!  routine mpas_framework_finalize
!
!> \brief MPAS framework finalization routine.
!> \author Michael Duda, Doug Jacobsen
!> \date   03/26/13
!> \details
!>  This routine finalizes the MPAS framework. It calls routines related to finalizing different parts of MPAS, that are housed within the framework.
!
!-----------------------------------------------------------------------  
   subroutine mpas_framework_finalize(dminfo, domain, io_system)!{{{
  
      implicit none

      type (dm_info), pointer :: dminfo
      type (domain_type), pointer :: domain
#ifdef MPAS_PIO_SUPPORT
      type (iosystem_desc_t), optional, pointer :: io_system
#else
      integer, optional, pointer :: io_system
#endif

      call MPAS_io_finalize(domain % ioContext, .false.)

      call mpas_deallocate_domain(domain)

      call mpas_dmpar_finalize(dminfo)

      call mpas_finish_block_proc_list(dminfo)

      call mpas_timekeeping_finalize()

   end subroutine mpas_framework_finalize!}}}


!-----------------------------------------------------------------------
!  routine mpas_framework_report_settings
!
!> \brief Report information about compile- and run-time settings to the log file
!> \author Michael Duda
!> \date 1 May 2024
!> \details
!>  This routine writes information about compile-time and run-time settings for
!>  an MPAS core to the log file.
!
!-----------------------------------------------------------------------  
   subroutine mpas_framework_report_settings(domain)

#ifdef MPAS_OPENMP
      use mpas_threading, only : mpas_threading_get_num_threads
#endif
  
      implicit none

      type (domain_type), pointer :: domain


      call mpas_log_write('')
      call mpas_log_write('Output from ''git describe --dirty'': '//trim(domain % core % git_version))

      call mpas_log_write('')
      call mpas_log_write('Compile-time options:')
      call mpas_log_write('  Build target: '//trim(domain % core % build_target))
      call mpas_log_write('  OpenMP support: ' // &
#ifdef MPAS_OPENMP
                          'yes')
#else
                          'no')
#endif
      call mpas_log_write('  OpenACC support: ' // &
#ifdef MPAS_OPENACC
                          'yes')
#else
                          'no')
#endif
      call mpas_log_write('  Default real precision: ' // &
#ifdef SINGLE_PRECISION
                          'single')
#else
                          'double')
#endif
      call mpas_log_write('  Compiler flags: ' // &
#ifdef MPAS_DEBUG
                          'debug')
#else
                          'optimize')
#endif
      call mpas_log_write('  I/O layer: ' // &
#ifdef MPAS_PIO_SUPPORT
#ifdef USE_PIO2
                          'PIO 2.x')
#else
                          'PIO 1.x')
#endif
#else
                          'SMIOL')
#endif
      call mpas_log_write('')

      call mpas_log_write('Run-time settings:')
      call mpas_log_write('  MPI task count: $i', intArgs=[domain % dminfo % nprocs])
#ifdef MPAS_OPENMP
      call mpas_log_write('  OpenMP max threads: $i', intArgs=[mpas_threading_get_max_threads()])
#endif
      call mpas_log_write('')

#ifdef MPAS_OPENACC
      call report_acc_devices()
#endif

   end subroutine mpas_framework_report_settings


#ifdef MPAS_OPENACC
   !***********************************************************************
   !
   !  function report_acc_devices
   !
   !> \brief   Queries OpenACC devices and reports device info to log file
   !> \author  Michael G. Duda
   !> \date    28 March 2024
   !> \details
   !>  This routine makes use of the OpenACC runtime library to obtain
   !>  information about how many and which kind of OpenACC devices are
   !>  available to the current MPI rank.
   !>
   !>  NB: This routine is only compiled and only called if OPENACC=true.
   !
   !-----------------------------------------------------------------------
   subroutine report_acc_devices()

      use mpas_c_interfacing, only : mpas_sanitize_string
      use openacc, only : acc_get_property_string, acc_get_property, acc_get_num_devices, acc_get_device_num, &
                          acc_get_device_type, acc_device_kind, acc_device_property, acc_property_vendor, &
                          acc_property_name, acc_property_driver

      implicit none

      integer(kind=acc_device_kind) :: device
      character(len=StrKIND) :: device_vendor, device_name, driver_vers
      integer :: ndevices, device_num


      device = acc_get_device_type()
      ndevices = acc_get_num_devices(device)
      device_num = acc_get_device_num(device_num)
      call acc_get_property_string(device_num, device, acc_property_vendor, device_vendor)
      call acc_get_property_string(device_num, device, acc_property_name, device_name)
      call acc_get_property_string(device_num, device, acc_property_driver, driver_vers)

      call mpas_sanitize_string(device_vendor)
      call mpas_sanitize_string(device_name)
      call mpas_sanitize_string(driver_vers)

      call mpas_log_write('OpenACC configuration:')
      call mpas_log_write('  Number of visible devices: $i', intArgs=[ndevices])
      call mpas_log_write('  Device # for this MPI task: $i', intArgs=[device_num])
      call mpas_log_write('  Device vendor: '//trim(device_vendor))
      call mpas_log_write('  Device name: '//trim(device_name))
      call mpas_log_write('  Device driver version: '//trim(driver_vers))
      call mpas_log_write('')

   end subroutine report_acc_devices
#endif

end module mpas_framework
