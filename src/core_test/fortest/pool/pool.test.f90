module test_pool_mod

   use mpas_pool_routines, only: mpas_pool_add_config, mpas_pool_create_pool, &
         mpas_pool_get_config
   use mpas_derived_types, only: mpas_pool_type
   use iso_c_binding, only: c_ptr, c_f_pointer, c_int
   use pool_fixture, only: pool_fixture_t
   use bdd_logging, only: when, and_then, should
   use fortest_assert, only: assert_equal, assert_true, assert_false
   implicit none
contains

   subroutine test_add_and_retrieve_unique_configs(f_ptr, ts_ptr, s_ptr)
      implicit none

      type(c_ptr), value :: f_ptr, ts_ptr, s_ptr
      type(pool_fixture_t), pointer :: f

      call c_f_pointer(f_ptr, f)
      ! -------------------------------------------------------------------
      call when("you add three unique key-value pairs to the pool")
      ! -------------------------------------------------------------------
      call mpas_pool_add_config(f%pool_size1, f%key1, f%val1)
      call mpas_pool_add_config(f%pool_size1, f%key2, f%val2)
      call mpas_pool_add_config(f%pool_size1, f%key3, f%val3)

      ! -------------------------------------------------------------------
      call and_then("you retrieve each value by its key")
      ! -------------------------------------------------------------------
      call mpas_pool_get_config(f%pool_size1, f%key1, f%val1_ptr)
      call mpas_pool_get_config(f%pool_size1, f%key2, f%val2_ptr)
      call mpas_pool_get_config(f%pool_size1, f%key3, f%val3_ptr)

      ! -------------------------------------------------------------------
      call should("have each value pointer associated")
      ! -------------------------------------------------------------------
      call assert_true(associated(f%val1_ptr), verbosity = 2)
      call assert_true(associated(f%val2_ptr), verbosity = 2)
      call assert_true(associated(f%val3_ptr), verbosity = 2)

      ! -------------------------------------------------------------------
      call and_then("those values match what was inserted")
      ! -------------------------------------------------------------------
      if (associated(f%val1_ptr)) call assert_equal(f%val1_ptr, f%val1, verbosity = 2)
      if (associated(f%val2_ptr)) call assert_equal(f%val2_ptr, f%val2, verbosity = 2)
      if (associated(f%val3_ptr)) call assert_equal(f%val3_ptr, f%val3, verbosity = 2)
   end subroutine test_add_and_retrieve_unique_configs

   subroutine test_add_and_retrieve_unique_configs_4(f_ptr, ts_ptr, s_ptr)
      implicit none

      type(c_ptr), value :: f_ptr, ts_ptr, s_ptr
      type(pool_fixture_t), pointer :: f

      call c_f_pointer(f_ptr, f)
      ! -------------------------------------------------------------------
      call when("you add three unique key-value pairs to the pool")
      ! -------------------------------------------------------------------
      call mpas_pool_add_config(f%pool_size4, f%key1, f%val1)
      call mpas_pool_add_config(f%pool_size4, f%key2, f%val2)
      call mpas_pool_add_config(f%pool_size4, f%key3, f%val3)

      ! -------------------------------------------------------------------
      call and_then("you retrieve each value by its key")
      ! -------------------------------------------------------------------
      call mpas_pool_get_config(f%pool_size4, f%key1, f%val1_ptr)
      call mpas_pool_get_config(f%pool_size4, f%key2, f%val2_ptr)
      call mpas_pool_get_config(f%pool_size4, f%key3, f%val3_ptr)

      ! -------------------------------------------------------------------
      call should("have each value pointer associated")
      ! -------------------------------------------------------------------
      call assert_true(associated(f%val1_ptr), verbosity = 2)
      call assert_true(associated(f%val2_ptr), verbosity = 2)
      call assert_true(associated(f%val3_ptr), verbosity = 2)

      ! -------------------------------------------------------------------
      call and_then("those values match what was inserted")
      ! -------------------------------------------------------------------
      if (associated(f%val1_ptr)) call assert_equal(f%val1_ptr, f%val1, verbosity = 2)
      if (associated(f%val2_ptr)) call assert_equal(f%val2_ptr, f%val2, verbosity = 2)
      if (associated(f%val3_ptr)) call assert_equal(f%val3_ptr, f%val3, verbosity = 2)
   end subroutine test_add_and_retrieve_unique_configs_4

   subroutine test_insert_duplicate_config_fails(f_ptr, ts_ptr, s_ptr)
      implicit none

      type(c_ptr), value :: f_ptr, ts_ptr, s_ptr
      type(pool_fixture_t), pointer :: f
      character(len = :), allocatable :: dup_key

      call c_f_pointer(f_ptr, f)
      dup_key = 'a'
      ! -------------------------------------------------------------------
      call when("you add three unique key-value pairs to the pool")
      ! -------------------------------------------------------------------
      call mpas_pool_add_config(f%pool_size1, f%key1, f%val1)
      call mpas_pool_add_config(f%pool_size1, f%key2, f%val2)
      call mpas_pool_add_config(f%pool_size1, f%key3, f%val3)
      ! -------------------------------------------------------------------
      call and_then("you attempt to add a duplicate key")
      ! -------------------------------------------------------------------
      call mpas_pool_add_config(f%pool_size1, dup_key, f%val1, ierr=f%ierr)
      ! -------------------------------------------------------------------
      call should("receive an error code indicating failure")
      ! -------------------------------------------------------------------
      call assert_false(f%ierr == 0, verbosity = 2)
   end subroutine test_insert_duplicate_config_fails

   subroutine test_insert_duplicate_config_fails_4(f_ptr, ts_ptr, s_ptr)
      implicit none

      type(c_ptr), value :: f_ptr, ts_ptr, s_ptr
      type(pool_fixture_t), pointer :: f
      character(len = :), allocatable :: dup_key

      call c_f_pointer(f_ptr, f)
      dup_key = 'a'
      ! -------------------------------------------------------------------
      call when("you add three unique key-value pairs to the pool")
      ! -------------------------------------------------------------------
      call mpas_pool_add_config(f%pool_size4, f%key1, f%val1)
      call mpas_pool_add_config(f%pool_size4, f%key2, f%val2)
      call mpas_pool_add_config(f%pool_size4, f%key3, f%val3)
      ! -------------------------------------------------------------------
      call and_then("you attempt to add a duplicate key")
      ! -------------------------------------------------------------------
      call mpas_pool_add_config(f%pool_size4, dup_key, f%val1, ierr=f%ierr)
      ! -------------------------------------------------------------------
      call should("receive an error code indicating failure")
      ! -------------------------------------------------------------------
      call assert_false(f%ierr == 0, verbosity = 2)
   end subroutine test_insert_duplicate_config_fails_4

   subroutine test_insert_duplicate_config_at_tail_fails(f_ptr, ts_ptr, s_ptr)
      implicit none

      type(c_ptr), value :: f_ptr, ts_ptr, s_ptr
      type(pool_fixture_t), pointer :: f
      character(len = :), allocatable :: dup_key

      call c_f_pointer(f_ptr, f)

      dup_key = 'c'  ! Duplicate of key3
      ! -------------------------------------------------------------------
      call when("you add three unique key-value pairs to the pool")
      ! -------------------------------------------------------------------
      call mpas_pool_add_config(f%pool_size4, f%key1, f%val1)
      call mpas_pool_add_config(f%pool_size4, f%key2, f%val2)
      call mpas_pool_add_config(f%pool_size4, f%key3, f%val3)
      ! -------------------------------------------------------------------
      call and_then("you attempt to add a duplicate key at the tail")
      ! -------------------------------------------------------------------
      call mpas_pool_add_config(f%pool_size4, dup_key, f%val3, f%ierr)
      ! -------------------------------------------------------------------
      call should("receive an error code indicating failure")
      ! -------------------------------------------------------------------
      call assert_false(f%ierr == 0, verbosity = 2)

   end subroutine test_insert_duplicate_config_at_tail_fails

   subroutine test_insert_duplicate_config_at_tail_fails_4(f_ptr, ts_ptr, s_ptr)
      implicit none

      type(c_ptr), value :: f_ptr, ts_ptr, s_ptr
      type(pool_fixture_t), pointer :: f
      character(len = :), allocatable :: dup_key

      call c_f_pointer(f_ptr, f)

      dup_key = 'c'  ! Duplicate of key3
      ! -------------------------------------------------------------------
      call when("you add three unique key-value pairs to the pool")
      ! -------------------------------------------------------------------
      call mpas_pool_add_config(f%pool_size4, f%key1, f%val1)
      call mpas_pool_add_config(f%pool_size4, f%key2, f%val2)
      call mpas_pool_add_config(f%pool_size4, f%key3, f%val3)
      ! -------------------------------------------------------------------
      call and_then("you attempt to add a duplicate key at the tail")
      ! -------------------------------------------------------------------
      call mpas_pool_add_config(f%pool_size4, dup_key, f%val3, f%ierr)
      ! -------------------------------------------------------------------
      call should("receive an error code indicating failure")
      ! -------------------------------------------------------------------
      call assert_false(f%ierr == 0, verbosity = 2)

   end subroutine test_insert_duplicate_config_at_tail_fails_4

end module test_pool_mod


program test_pool
   use fortest_test_session, only: test_session_t
   use pool_fixture, only: pool_fixture_t, setup_pool, teardown_pool
   use iso_c_binding, only: c_loc, c_ptr
   use test_pool_mod, only: test_add_and_retrieve_unique_configs, &
         test_add_and_retrieve_unique_configs_4, &
         test_insert_duplicate_config_fails, &
         test_insert_duplicate_config_fails_4, &
         test_insert_duplicate_config_at_tail_fails, &
         test_insert_duplicate_config_at_tail_fails_4

   implicit none
   type(test_session_t) :: session
   type(c_ptr) :: test_fixture_ptr
   type(pool_fixture_t), target :: test_fixture
   integer :: ierr

   call MPI_Init(ierr)
   test_fixture_ptr = c_loc(test_fixture)

   call session%register_test_suite("pool_test_suite")

   call session%register_fixture(&
         setup = setup_pool, &
         teardown = teardown_pool, &
         args = test_fixture_ptr, &
         scope = "test", &
         test_suite_name = "pool_test_suite")

   call session%register_test(&
         test_suite_name = "pool_test_suite", &
         test_name = "add_and_retrieve_unique_configs", &
         test = test_add_and_retrieve_unique_configs)

   call session%register_test(&
         test_suite_name = "pool_test_suite", &
         test_name = "add_and_retrieve_unique_configs_4", &
         test = test_add_and_retrieve_unique_configs_4)
   call session%register_test(&
         test_suite_name = "pool_test_suite", &
         test_name = "insert_duplicate_config_fails", &
         test = test_insert_duplicate_config_fails)
   call session%register_test(&
         test_suite_name = "pool_test_suite", &
         test_name = "insert_duplicate_config_fails_4", &
         test = test_insert_duplicate_config_fails_4)
   call session%register_test(&
         test_suite_name = "pool_test_suite", &
         test_name = "insert_duplicate_config_at_tail_fails", &
         test = test_insert_duplicate_config_at_tail_fails)
   call session%register_test(&
         test_suite_name = "pool_test_suite", &
         test_name = "insert_duplicate_config_at_tail_fails_4", &
         test = test_insert_duplicate_config_at_tail_fails_4)

   ! Run the tests
   call session%run()
   call session%finalize()
   call MPI_Finalize(ierr)

end program