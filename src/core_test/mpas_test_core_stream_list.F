! Copyright (c) 2025 The University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at https://mpas-dev.github.io/license.html .
!
module mpas_test_core_stream_list

   use mpas_derived_types
   use mpas_log
   use mpas_stream_list

   implicit none
   private

   public :: mpas_test_stream_list

contains

   !**************************************************************************************
   !  Subroutine mpas_test_create_list
   !
   !> \brief   Test creating an empty stream list and verify initialization.
   !>
   !> \details This subroutine tests the creation of a new MPAS stream list, ensuring
   !>          that the list is properly initialized with zero items and no head stream.
   !>          The list is then destroyed to clean up any allocated memory.
   !>
   !> \param err  The error code that indicates the result of the test.
   !
   !--------------------------------------------------------------------------------------
   subroutine mpas_test_create_list(err)
      integer, intent(out) :: err
      type(MPAS_stream_list_type), pointer :: list

      err = 0

      call MPAS_stream_list_create(list)

      if (.not. associated(list)) then
         err = err + 1
      end if

      if (list%nItems /= 0) then
         err = err + 1
      end if

      if (associated(list%head)) then
         err = err + 1
      end if

      call MPAS_stream_list_destroy(list)
   end subroutine mpas_test_create_list

   !**************************************************************************************
   !  Subroutine mpas_test_insert_single
   !
   !> \brief   Test inserting a single stream into the list and verify correct insertion.
   !>
   !> \details This subroutine tests the insertion of a single stream into an MPAS
   !>          stream list. It verifies that the stream is correctly added to the
   !>          list, and checks that the list contains the expected stream with the
   !>          correct number of items.
   !>
   !> \param err  The error code that indicates the result of the test.
   !
   !--------------------------------------------------------------------------------------
   subroutine mpas_test_insert_single(err)
      integer, intent(out) :: err
      type(MPAS_stream_list_type), pointer :: list, stream
      integer :: ierr

      err = 0

      call MPAS_stream_list_create(list)
      allocate(stream)
      stream%name = 'stream1'

      call MPAS_stream_list_insert(list, stream, ierr)

      if (ierr /= MPAS_STREAM_LIST_NOERR) then
         err = err + 1
      end if

      if (.not. associated(list%head)) then
         err = err + 1
      end if

      if (list%nItems /= 1) then
         err = err + 1
      end if

      if (trim(list%head%name) /= 'stream1') then
         err = err + 1
      end if

      call MPAS_stream_list_destroy(list)
   end subroutine mpas_test_insert_single

   !**************************************************************************************
   !  Subroutine mpas_test_query_exact_match
   !
   !> \brief   Test querying for an exact stream match and ensure correct stream is found.
   !>
   !> \details This subroutine tests querying a stream list for an exact match of a
   !>          stream's name. It ensures that the correct stream is found and that
   !>          the query operation behaves as expected.
   !>
   !> \param err  The error code that indicates the result of the test.
   !
   !--------------------------------------------------------------------------------------
   subroutine mpas_test_query_exact_match(err)
      integer, intent(out) :: err
      type(MPAS_stream_list_type), pointer :: list, stream, found
      logical :: matched

      err = 0

      call MPAS_stream_list_create(list)
      allocate(stream)
      stream%name = 'stream1'
      call MPAS_stream_list_insert(list, stream)
      found => null()

      matched = MPAS_stream_list_query(list, 'stream1', found)

      if (.not. matched) then
         err = err + 1
      end if

      if (.not. associated(found)) then
         err = err + 1
      else if (trim(found%name) /= 'stream1') then
         err = err + 1
      end if

      call MPAS_stream_list_destroy(list)
   end subroutine mpas_test_query_exact_match

   !**************************************************************************************
   !  Subroutine mpas_test_remove_existing_streams
   !
   !> \brief   Test removing streams from the beginning, middle, and end of a list.
   !>
   !> \details This subroutine verifies that removing streams from different positions
   !>          in an MPAS stream list works as expected. It inserts three streams into
   !>          the list, then removes one from the middle, one from the end, and one
   !>          from the beginning, checking that the correct stream is removed in each
   !>          case and that the operation returns a success code.
   !>
   !> \param err  The error code that indicates the result of the test.
   !>
   !--------------------------------------------------------------------------------------
   subroutine mpas_test_remove_existing_streams(err)
      integer, intent(out) :: err
      type(MPAS_stream_list_type), pointer :: list, s1, s2, s3, &
            removed1, removed2, removed3
      integer :: ierr

      err = 0

      allocate(s1)
      s1%name = 'stream1'
      allocate(s2)
      s2%name = 'stream2'
      allocate(s3)
      s3%name = 'stream3'

      call MPAS_stream_list_create(list, ierr=ierr)
      if (ierr /= MPAS_STREAM_LIST_NOERR) then
         err = err + 1
      end if

      call MPAS_stream_list_insert(list, s1, ierr=ierr)
      if (ierr /= MPAS_STREAM_LIST_NOERR) then
         err = err + 1
      end if

      call MPAS_stream_list_insert(list, s2, ierr=ierr)
      if (ierr /= MPAS_STREAM_LIST_NOERR) then
         err = err + 1
      end if

      call MPAS_stream_list_insert(list, s3, ierr=ierr)
      if (ierr /= MPAS_STREAM_LIST_NOERR) then
         err = err + 1
      end if

      ! Remove from the middle
      call MPAS_stream_list_remove(list, 'stream2', removed2, ierr=ierr)
      if (ierr /= MPAS_STREAM_LIST_NOERR) then
         err = err + 1
      end if
      if (.not. associated(removed2)) then
         err = err + 1
      end if
      if (trim(removed2%name) /= 'stream2') then
         err = err + 1
      end if

      ! Remove from the end
      call MPAS_stream_list_remove(list, 'stream3', removed3, ierr=ierr)
      if (ierr /= MPAS_STREAM_LIST_NOERR) then
         err = err + 1
      end if
      if (.not. associated(removed3)) then
         err = err + 1
      end if
      if (trim(removed3%name) /= 'stream3') then
         err = err + 1
      end if

      ! Remove from the beginning
      call MPAS_stream_list_remove(list, 'stream1', removed1, ierr=ierr)
      if (ierr /= MPAS_STREAM_LIST_NOERR) then
         err = err + 1
      end if
      if (.not. associated(removed1)) then
         err = err + 1
      end if
      if (trim(removed1%name) /= 'stream1') then
         err = err + 1
      end if

      call MPAS_stream_list_destroy(list)
      deallocate(removed1)
      deallocate(removed2)
      deallocate(removed3)
   end subroutine mpas_test_remove_existing_streams

   !**************************************************************************************
   !  Subroutine mpas_test_insert_non_adjacent_duplicate
   !
   !> \brief   Test inserting a non-adjacent duplicate of the first item added to the list.
   !>
   !> \details This subroutine verifies that inserting a duplicate of the first stream
   !>          added to an MPAS stream list results in the correct duplicate error
   !>          code when the duplicate is not inserted immediately after the original.
   !>          It does so by inserting two unique streams into the list, then attempting
   !>          to insert the first stream again. This confirms that duplicate detection
   !>          works for non-adjacent duplicates in insertion order.
   !>
   !> \param err  The error code that indicates the result of the test.
   !--------------------------------------------------------------------------------------
   subroutine mpas_test_insert_non_adjacent_duplicate(err)
      integer, intent(out) :: err
      type(MPAS_stream_list_type), pointer :: list
      type(MPAS_stream_list_type), pointer :: s1, s2
      integer :: ierr

      err = 0
      call MPAS_stream_list_create(list)

      allocate(s1)
      s1%name = 'stream1'
      allocate(s2)
      s2%name = 'stream2'

      call MPAS_stream_list_insert(list, s1, ierr)
      if (ierr /= MPAS_STREAM_LIST_NOERR) then
         err = err + 1
      end if

      call MPAS_stream_list_insert(list, s2, ierr)
      if (ierr /= MPAS_STREAM_LIST_NOERR) then
         err = err + 1
      end if

      call MPAS_stream_list_insert(list, s1, ierr)
      if (ierr /= MPAS_STREAM_LIST_DUPLICATE) then
         err = err + 1
      end if
      if (MPAS_stream_list_length(list) /= 2) then
         err = err + 1
      end if
      ! Verify that inserting s1 again does not break the linkage bewtween s1 and s2
      if (.not. associated(s1%next)) then
         err = err + 1
      end if

      call MPAS_stream_list_destroy(list)
   end subroutine mpas_test_insert_non_adjacent_duplicate

   !**************************************************************************************
   !  Subroutine mpas_test_remove_from_empty_list
   !
   !> \brief   Test attempting to remove a stream from an empty list, expect error.
   !>
   !> \details This subroutine tests the behavior of attempting to remove a stream
   !>          from an empty list. It ensures that the correct error
   !>          is returned when the stream is not found.
   !>
   !> \param err  The error code that indicates the result of the test.
   !
   !--------------------------------------------------------------------------------------
   subroutine mpas_test_remove_from_empty_list(err)
      integer, intent(out) :: err
      type(MPAS_stream_list_type), pointer :: list, removed
      integer :: ierr

      err = 0

      call MPAS_stream_list_create(list)

      call MPAS_stream_list_remove(list, 'stream1', removed, ierr)
      if (ierr /= MPAS_STREAM_LIST_NOT_FOUND) then
         err = err + 1
      end if
      if (associated(removed)) then
         err = err + 1
      end if
      call MPAS_stream_list_destroy(list)
   end subroutine mpas_test_remove_from_empty_list

   !**************************************************************************************
   !  Subroutine mpas_test_remove_not_found
   !
   !> \brief   Attempt to remove a stream not in a non-empty list; expect NOT_FOUND.
   !>
   !> \details This subroutine populates the list with a couple of streams, then
   !>          attempts to remove a stream name that does not exist. It verifies that
   !>          MPAS_STREAM_LIST_NOT_FOUND is returned and that no node is returned.
   !>
   !> \param err  The error code that indicates the result of the test.
   !>
   !--------------------------------------------------------------------------------------
   subroutine mpas_test_remove_not_found(err)
      integer, intent(out) :: err
      type(MPAS_stream_list_type), pointer :: list, removed
      type(MPAS_stream_list_type), pointer :: s1, s2
      integer :: ierr

      err = 0

      allocate(s1)
      s1%name = 'stream1'
      allocate(s2)
      s2%name = 'stream2'

      call MPAS_stream_list_create(list)

      call MPAS_stream_list_insert(list, s1, ierr)
      if (ierr /= MPAS_STREAM_LIST_NOERR) then
         err = err + 1
      end if

      call MPAS_stream_list_insert(list, s2, ierr)
      if (ierr /= MPAS_STREAM_LIST_NOERR) then
         err = err + 1
      end if

      call MPAS_stream_list_remove(list, 'stream3', removed, ierr)
      if (ierr /= MPAS_STREAM_LIST_NOT_FOUND) then
         err = err + 1
      end if
      if (associated(removed)) then
         err = err + 1
      end if

      call MPAS_stream_list_destroy(list)
   end subroutine mpas_test_remove_not_found

   !**************************************************************************************
   !  Subroutine mpas_test_list_length
   !
   !> \brief   Test the length of the stream list after inserting multiple streams.
   !>
   !> \details This subroutine tests that the length of an MPAS stream list is correctly
   !>          updated after multiple streams are inserted. It verifies that the length
   !>          matches the expected value.
   !>
   !> \param err  The error code that indicates the result of the test.
   !
   !--------------------------------------------------------------------------------------
   subroutine mpas_test_list_length(err)
      integer, intent(out) :: err
      type(MPAS_stream_list_type), pointer :: list, s1, s2, s3

      err = 0
      call MPAS_stream_list_create(list)

      allocate(s1)
      s1%name = 'stream1'
      allocate(s2)
      s2%name = 'stream2'
      allocate(s3)
      s3%name = 'stream3'

      call MPAS_stream_list_insert(list, s1)
      if (MPAS_stream_list_length(list) /= 1) then
         err = err + 1
      end if
      call MPAS_stream_list_insert(list, s2)
      if (MPAS_stream_list_length(list) /= 2) then
         err = err + 1
      end if
      call MPAS_stream_list_insert(list, s3)
      if (MPAS_stream_list_length(list) /= 3) then
         err = err + 1
      end if

      call MPAS_stream_list_destroy(list)
   end subroutine mpas_test_list_length

   !**************************************************************************************
   !  Subroutine mpas_test_query_partial_match
   !
   !> \brief   Test querying for a partial stream name match, ensuring no match is found.
   !>
   !> \details This subroutine tests the querying of a stream list for a partial match
   !>          of a stream's name. It verifies that no match is found for a partial
   !>          name match.
   !>
   !> \param err  The error code that indicates the result of the test.
   !
   !--------------------------------------------------------------------------------------
   subroutine mpas_test_query_partial_match(err)
      integer, intent(out) :: err
      type(MPAS_stream_list_type), pointer :: list, stream, found
      logical :: matched
      integer :: ierr

      err = 0
      call MPAS_stream_list_create(list)
      allocate(stream)
      stream%name = 'stream1'
      nullify(found)

      call MPAS_stream_list_insert(list, stream)

      matched = MPAS_stream_list_query(list, 'stream', found, ierr)

      if (ierr /= MPAS_STREAM_LIST_NOERR) then
         err = err + 1
      end if
      if (matched .or. associated(found)) then
         err = err + 1
      end if

      call MPAS_stream_list_destroy(list)
   end subroutine mpas_test_query_partial_match

   !**************************************************************************************
   !  Subroutine mpas_test_insert_duplicate_at_begin
   !
   !> \brief   Test inserting a duplicate stream at the beginning of the list.
   !>
   !> \details This subroutine tests the insertion of a duplicate stream at the
   !>          beginning of an MPAS stream list. It ensures that the correct error
   !>          is returned when attempting to insert a duplicate stream.
   !>
   !> \param err  The error code that indicates the result of the test.
   !
   !--------------------------------------------------------------------------------------
   subroutine mpas_test_insert_duplicate_at_begin(err)
      integer, intent(out) :: err
      type(MPAS_stream_list_type), pointer :: list
      type(MPAS_stream_list_type), pointer :: s1
      integer :: ierr

      err = 0
      call MPAS_stream_list_create(list)

      allocate(s1)
      s1%name = 'stream1'

      call MPAS_stream_list_insert(list, s1, ierr)
      if (ierr /= MPAS_STREAM_LIST_NOERR) then
         err = err + 1
      end if

      call MPAS_stream_list_insert(list, s1, ierr)
      if (ierr /= MPAS_STREAM_LIST_DUPLICATE) then
         err = err + 1
      end if
      if (MPAS_stream_list_length(list) /= 1) then
         err = err + 1
      end if

      call MPAS_stream_list_destroy(list)
   end subroutine mpas_test_insert_duplicate_at_begin

   !**************************************************************************************
   !  Subroutine mpas_test_insert_duplicate_at_end
   !
   !> \brief   Test inserting a duplicate stream at the end of the list.
   !>
   !> \details This subroutine tests the insertion of a duplicate stream at the
   !>          end of an MPAS stream list. It ensures that the correct error
   !>          is returned when attempting to insert a duplicate stream.
   !>
   !> \param err  The error code that indicates the result of the test.
   !
   !--------------------------------------------------------------------------------------
   subroutine mpas_test_insert_duplicate_at_end(err)
      integer, intent(out) :: err
      type(MPAS_stream_list_type), pointer :: list
      type(MPAS_stream_list_type), pointer :: s1, s2
      integer :: ierr

      err = 0
      call MPAS_stream_list_create(list)

      allocate(s1)
      s1%name = 'stream1'

      allocate(s2)
      s2%name = 'stream2'

      call MPAS_stream_list_insert(list, s1, ierr)
      if (ierr /= MPAS_STREAM_LIST_NOERR) then
         err = err + 1
      end if

      call MPAS_stream_list_insert(list, s2, ierr)
      if (ierr /= MPAS_STREAM_LIST_NOERR) then
         err = err + 1
      end if

      call MPAS_stream_list_insert(list, s2, ierr)
      if (ierr /= MPAS_STREAM_LIST_DUPLICATE) then
         err = err + 1
      end if
      if (MPAS_stream_list_length(list) /= 2) then
         err = err + 1
      end if

      call MPAS_stream_list_destroy(list)
   end subroutine mpas_test_insert_duplicate_at_end

   !**************************************************************************************
   !  Subroutine mpas_test_stream_list
   !
   !> \brief   Core test suite for MPAS stream list routines.
   !>
   !> \details This subroutine runs all the test cases for the MPAS stream list
   !>          routines, including tests for stream creation, insertion, querying,
   !>          removal, and checking for duplicate entries. Each test case is executed
   !>          and its result is logged with a success or failure message.
   !>
   !> \param err  The error code that indicates the result of the test. It accumulates
   !>             errors from all individual test cases.
   !
   !--------------------------------------------------------------------------------------
   subroutine mpas_test_stream_list(err)
      integer, intent(out) :: err
      integer :: test_err

      err = 0

      call mpas_log_write('Testing MPAS stream list routines:')

      ! Test stream list creation and verify initialization.
      call mpas_test_create_list(test_err)
      if (test_err == 0) then
         call mpas_log_write('   mpas_test_create_list: SUCCESS')
      else
         err = err + test_err
         call mpas_log_write('   mpas_test_create_list: FAILURE')
      end if

      ! Test inserting a single stream into the list and verify correct insertion.
      call mpas_test_insert_single(test_err)
      if (test_err == 0) then
         call mpas_log_write('   mpas_test_insert_single: SUCCESS')
      else
         err = err + test_err
         call mpas_log_write('   mpas_test_insert_single: FAILURE')
      end if

      ! Test querying for an exact stream match and ensure correct stream is found.
      call mpas_test_query_exact_match(test_err)
      if (test_err == 0) then
         call mpas_log_write('   mpas_test_query_exact_match: SUCCESS')
      else
         err = err + test_err
         call mpas_log_write('   mpas_test_query_exact_match: FAILURE')
      end if

      ! Test removing streams at beginning, middle, and end of a list.
      call mpas_test_remove_existing_streams(test_err)
      if (test_err == 0) then
         call mpas_log_write('   mpas_test_remove_existing_streams: SUCCESS')
      else
         err = err + test_err
         call mpas_log_write('   mpas_test_remove_existing_streams: FAILURE')
      end if

      ! Test inserting a non-adjacent duplicate of the first stream added.
      call mpas_test_insert_non_adjacent_duplicate(test_err)
      if (test_err == 0) then
         call mpas_log_write('   mpas_test_insert_non_adjacent_duplicate: SUCCESS')
      else
         err = err + test_err
         call mpas_log_write('   mpas_test_insert_non_adjacent_duplicate: FAILURE')
      end if

      ! Test attempting to remove a non-existent stream from an empty list, expect error.
      call mpas_test_remove_from_empty_list(test_err)
      if (test_err == 0) then
         call mpas_log_write('   mpas_test_remove_from_empty_list: SUCCESS')
      else
         err = err + test_err
         call mpas_log_write('   mpas_test_remove_from_empty_list: FAILURE')
      end if

      ! Test attempting to remove a stream not found in the list, expect error.
      call mpas_test_remove_not_found(test_err)
      if (test_err == 0) then
         call mpas_log_write('   mpas_test_remove_not_found: SUCCESS')
      else
         err = err + test_err
         call mpas_log_write('   mpas_test_remove_not_found: FAILURE')
      end if

      ! Test the length of the stream list after inserting multiple streams.
      call mpas_test_list_length(test_err)
      if (test_err == 0) then
         call mpas_log_write('   mpas_test_list_length: SUCCESS')
      else
         err = err + test_err
         call mpas_log_write('   mpas_test_list_length: FAILURE')
      end if

      ! Test querying for a partial stream name match, ensuring no match is found.
      call mpas_test_query_partial_match(test_err)
      if (test_err == 0) then
         call mpas_log_write('   mpas_test_query_partial_match: SUCCESS')
      else
         err = err + test_err
         call mpas_log_write('   mpas_test_query_partial_match: FAILURE')
      end if

      ! Test inserting a duplicate stream at the beginning of the list.
      call mpas_test_insert_duplicate_at_begin(test_err)
      if (test_err == 0) then
         call mpas_log_write('   mpas_test_insert_duplicate_at_begin: SUCCESS')
      else
         err = err + test_err
         call mpas_log_write('   mpas_test_insert_duplicate_at_begin: FAILURE')
      end if

      ! Test inserting a duplicate stream at the end of the list.
      call mpas_test_insert_duplicate_at_end(test_err)
      if (test_err == 0) then
         call mpas_log_write('   mpas_test_insert_duplicate_at_end: SUCCESS')
      else
         err = err + test_err
         call mpas_log_write('   mpas_test_insert_duplicate_at_end: FAILURE')
      end if
   end subroutine mpas_test_stream_list

end module mpas_test_core_stream_list
