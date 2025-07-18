! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
module test_core

   use mpas_framework
   use mpas_timekeeping

   use test_core_halo_exch
   use test_core_field_tests
   use test_core_timekeeping_tests
   use mpas_stream_manager

   type (MPAS_Clock_type), pointer :: clock

   contains

   !***********************************************************************
   !
   !  function test_core_init
   !
   !> \brief   MPAS Core Initialization Function
   !> \author  Doug Jacobsen
   !> \date    04/06/2015
   !> \details 
   !>  This function performs the necessary initialization of a core. This can
   !>   involve things like setting up coeffiecients for remapping and advection,
   !>   but more generally can include anything the core needs to initialize.
   !
   !-----------------------------------------------------------------------
   function test_core_init(domain, startTimeStamp) result(iErr)!{{{
   
      use mpas_derived_types
   
      implicit none
   
      type (domain_type), intent(inout) :: domain
      character(len=*), intent(out) :: startTimeStamp
   
      type (MPAS_Time_Type) :: startTime
      type (mpas_pool_type), pointer :: modelPool
      character (len=StrKIND), pointer :: xtime
      integer :: iErr


      iErr = 0

      !
      ! Set "local" clock to point to the clock contained in the domain type
      !
      clock => domain % clock

      !
      ! Set startTimeStamp based on the start time of the simulation clock
      !
      startTime = mpas_get_clock_time(clock, MPAS_START_TIME, iErr)
      call mpas_get_time(startTime, dateTimeString=startTimeStamp) 

      call mpas_pool_get_subpool(domain % blocklist % structs, 'model', modelPool)
      call mpas_pool_get_array(modelPool, 'xtime', xtime)
      xtime = startTimeStamp

      call mpas_stream_mgr_read(domain % streamManager, ierr=iErr)
      call mpas_stream_mgr_reset_alarms(domain % streamManager, direction=MPAS_STREAM_INPUT, ierr=iErr)

   end function test_core_init!}}}


   !***********************************************************************
   !
   !  function test_core_run
   !
   !> \brief   MPAS Core Run Function
   !> \author  Doug Jacobsen
   !> \date    04/06/2015
   !> \details 
   !>  This function performs a run of the MPAS core. This can involve time
   !>  stepping if the core requires time stepping, but more generally includes
   !>  anything a core would typically do after it was initialized.
   !
   !-----------------------------------------------------------------------
   function test_core_run(domain) result(iErr)!{{{
   
      use mpas_derived_types
      use mpas_kind_types
      use mpas_timer
      use mpas_vector_operations
      use mpas_geometry_utils
      use test_core_streams, only : test_core_streams_test
      use test_core_sorting, only : test_core_test_sorting
      use mpas_halo_testing, only : mpas_halo_tests
      use test_core_string_utils, only : mpas_test_string_utils
      use mpas_test_core_dmpar, only : mpas_test_dmpar
      use mpas_test_core_stream_inquiry, only : mpas_test_stream_inquiry
      use mpas_test_core_openacc, only : mpas_test_openacc

      implicit none
   
      type (domain_type), intent(inout) :: domain
      integer :: iErr

      type (block_type), pointer :: block

      type (mpas_pool_type), pointer :: pool
      type (mpas_pool_iterator_type) :: itr
      integer :: numThreads, threadLimit, maxThreads
      integer, dimension(:), allocatable :: threadErrs

      maxThreads = mpas_threading_get_max_threads()
      allocate(threadErrs(maxThreads))
      threadErrs(:) = 0

      !
      ! Test performance of framework sorting routines
      !
      call test_core_test_sorting(domain, iErr)
      if (iErr == 0) then
         call mpas_log_write(' * Sorting tests: SUCCESS')
      else
         call mpas_log_write(' * Sorting tests: FAILURE', MPAS_LOG_ERR)
      end if

      !
      ! Test functionality of mpas_halo module
      !
      call mpas_log_write('')
      call mpas_log_write('Testing mpas_halo module:')
      call mpas_halo_tests(domain, iErr)
      if (iErr == 0) then
         call mpas_log_write('* mpas_halo tests: SUCCESS')
      else
         call mpas_log_write('* mpas_halo tests: FAILURE', MPAS_LOG_ERR)
      end if
      call mpas_log_write('')

      iErr = 0

      call mpas_unit_test_fix_periodicity(iErr)
      call mpas_unit_test_triangle_signed_area_sphere(iErr)

      call mpas_unit_test_velocity_conversion(iErr)
      call mpas_unit_test_wachspress_hexagon(iErr)
      call mpas_unit_test_wachspress_triangle(iErr)

      !$omp parallel default(firstprivate) shared(domain, threadErrs)
      call test_core_halo_exch_test(domain, threadErrs, iErr)
      !$omp end parallel
      if ( iErr == 0 ) then
         call mpas_log_write(' * Halo Exchange Test: SUCCESS')
      else
         call mpas_log_write(' * Halo Exchange Test: FAILURE', MPAS_LOG_ERR)
      end if

      !$omp parallel default(firstprivate) shared(domain, threadErrs)
      call test_core_test_fields(domain, threadErrs, ierr)
       if ( iErr == 0 ) then
         call mpas_log_write(' * Field Tests: SUCCESS')
      else
         call mpas_log_write(' * Field Tests: FAILURE', MPAS_LOG_ERR)
      end if
      !$omp end parallel

      call test_core_streams_test(domain, threadErrs, iErr)
      if ( iErr == 0 ) then
         call mpas_log_write('Stream I/O tests: SUCCESS')
      else
         call mpas_log_write('Stream I/O tests: FAILURE', MPAS_LOG_ERR)
      end if

      ! Run string util tests
      call mpas_log_write('')
      call mpas_test_string_utils(iErr)
      call mpas_log_write('')

      !
      ! Run mpas_dmpar tests
      !
      call mpas_log_write('')
      iErr = mpas_test_dmpar(domain % dminfo)
      if (iErr == 0) then
          call mpas_log_write('All tests PASSED')
      else
          call mpas_log_write('$i tests FAILED', intArgs=[iErr])
      end if
      call mpas_log_write('')

      !
      ! Run mpas_stream_inquiry tests
      !
      call mpas_log_write('')
      iErr = mpas_test_stream_inquiry(domain % dminfo)
      if (iErr == 0) then
          call mpas_log_write('All tests PASSED')
      else
          call mpas_log_write('$i tests FAILED', intArgs=[iErr])
      end if
      call mpas_log_write('')

      call test_core_test_intervals(domain, threadErrs, iErr)

      ! Test writing of block write streams, which have the prefix 'block_'
      block => domain % blocklist
      do while (associated(block))
         call mpas_stream_mgr_reset_alarms(domain % streamManager, streamID="block_.*")
         call mpas_stream_mgr_block_write(domain % streamManager, block, streamID="block_.*", forceWriteNow=.true.)
         block => block % next
      end do

      call mpas_stream_mgr_write(domain % streamManager, forceWriteNow=.true.)

      !
      ! Run mpas_test_openacc
      !
      call mpas_log_write('')
#ifdef MPAS_OPENACC
      iErr = mpas_test_openacc(domain)
      if (iErr == 0) then
          call mpas_log_write('All tests PASSED')
      else
          call mpas_log_write('$i tests FAILED', intArgs=[iErr])
      end if
#else
      call mpas_log_write('MPAS_OPENACC not defined, skipping OpenACC tests')
#endif
      call mpas_log_write('')

      !
      ! Test functionality of adjustments to alarm reference time
      !
      call mpas_log_write('')
      call mpas_log_write('Testing mpas_adjust_alarm_to_reference_time:')
      call mpas_adjust_alarm_tests(domain, iErr)
      if (iErr == 0) then
         call mpas_log_write('* mpas_adjust_alarm_tests tests - all tests passed: SUCCESS')
      else
         call mpas_log_write('* mpas_adjust_alarm_tests tests - $i failed tests: FAILURE', intArgs=[iErr])
      end if
      call mpas_log_write('')

      deallocate(threadErrs)

   end function test_core_run!}}}

   !***********************************************************************
   !
   !  function test_core_finalize
   !
   !> \brief   MPAS Core Finalization Function
   !> \author  Doug Jacobsen
   !> \date    04/06/2015
   !> \details 
   !>  This function finalizes the MPAS core. It should at a minimum destroy the
   !>  simulation clock, but can perform any functions necessary to clean up the
   !>  MPAS core.
   !
   !-----------------------------------------------------------------------
   function test_core_finalize(domain) result(iErr)!{{{
   
      use mpas_derived_types
   
      implicit none

      type (domain_type), intent(inout) :: domain 
      integer :: iErr
 

      iErr = 0

      call mpas_destroy_clock(clock, iErr)

   end function test_core_finalize!}}}

end module test_core
