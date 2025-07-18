! Copyright (c) 2023-2024, The University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html .
!
module mpas_halo_testing

   private

   public :: mpas_halo_tests

   contains

   !***********************************************************************
   !
   !  routine mpas_halo_tests
   !
   !> \brief   Tests functionality of the mpas_halo module
   !> \author  Michael Duda
   !> \date    31 May 2023
   !> \details
   !>  This routine tests the functionality of the mpas_halo module by building
   !>  different halo exchange groups, exchanging halos for fields in those
   !>  groups, and checking the values in the halos.
   !>
   !>  If no errors are encountered, the ierr argument is set to 0; otherwise,
   !>  ierr is set to a positive integer.
   !
   !-----------------------------------------------------------------------
   subroutine mpas_halo_tests(domain, ierr)

      use mpas_derived_types, only : domain_type, mpas_pool_type, field1DReal, field2DReal, field3DReal
      use mpas_kind_types, only : StrKIND, RKIND
      use mpas_log, only : mpas_log_write
      use mpas_dmpar, only : mpas_dmpar_max_int
      use mpas_pool_routines, only : mpas_pool_get_subpool, mpas_pool_get_field, mpas_pool_get_array, &
                                     mpas_pool_get_dimension
      use mpas_field_routines, only : mpas_allocate_scratch_field, mpas_deallocate_scratch_field
      use mpas_halo

      implicit none

      type (domain_type), intent(inout) :: domain
      integer, intent(out) :: ierr

      integer :: j, k
      real (kind=RKIND) :: diff
      integer :: ierr_local, ierr_global
      character(len=StrKIND) :: test_mesg
      type (mpas_pool_type), pointer :: haloExchTest_pool
      type (mpas_pool_type), pointer :: mesh_pool
      type (field1DReal), pointer :: scratch_1d
      type (field2DReal), pointer :: scratch_2d
      type (field3DReal), pointer :: scratch_3d
      real (kind=RKIND), dimension(:), pointer :: array_1d
      real (kind=RKIND), dimension(:,:), pointer :: array_2d
      real (kind=RKIND), dimension(:,:,:), pointer :: array_3d
      integer, dimension(:), pointer :: indexToCellID
      integer, pointer :: nCells, nCellsSolve


      ierr = 0
      ierr_local = 0


      nullify(haloExchTest_pool)
      call mpas_pool_get_subpool(domain % blocklist % structs, 'haloExchTest', haloExchTest_pool)

      nullify(mesh_pool)
      call mpas_pool_get_subpool(domain % blocklist % structs, 'mesh', mesh_pool)

      nullify(indexToCellID)
      call mpas_pool_get_array(mesh_pool, 'indexToCellID', indexToCellID)

      nullify(nCells)
      call mpas_pool_get_dimension(mesh_pool, 'nCells', nCells)

      nullify(nCellsSolve)
      call mpas_pool_get_dimension(mesh_pool, 'nCellsSolve', nCellsSolve)

      !
      ! Initialize the mpas_halo module
      !
      write(test_mesg, '(a)') '  Initializing the mpas_halo module: '
      call mpas_halo_init(domain, ierr_local)
      ierr = ior(ierr, ierr_local)

      if (ierr_local == 0) then
         test_mesg = trim(test_mesg)//' SUCCESS'
      else
         test_mesg = trim(test_mesg)//' FAILURE'
      end if
      call mpas_log_write(trim(test_mesg))

      !
      ! Create a group with persistent fields
      !
      write(test_mesg, '(a)') '  Creating a halo group with persistent fields: '
      call mpas_halo_exch_group_create(domain, 'persistent_group', ierr_local)
      ierr = ior(ierr, ierr_local)

      call mpas_halo_exch_group_add_field(domain, 'persistent_group', 'cellPersistReal1D', iErr=ierr_local)
      ierr = ior(ierr, ierr_local)

      call mpas_halo_exch_group_add_field(domain, 'persistent_group', 'cellPersistReal2D', iErr=ierr_local)
      ierr = ior(ierr, ierr_local)

      call mpas_halo_exch_group_add_field(domain, 'persistent_group', 'cellPersistReal3D', iErr=ierr_local)
      ierr = ior(ierr, ierr_local)

      call mpas_halo_exch_group_complete(domain, 'persistent_group', ierr_local)
      ierr = ior(ierr, ierr_local)

      if (ierr == 0) then
         test_mesg = trim(test_mesg)//' SUCCESS'
      else
         test_mesg = trim(test_mesg)//' FAILURE'
      end if
      call mpas_log_write(trim(test_mesg))

      !
      ! Create a group with scratch fields
      !
      write(test_mesg, '(a)') '  Creating a halo group with scratch fields: '
      call mpas_halo_exch_group_create(domain, 'scratch_group', ierr_local)
      ierr = ior(ierr, ierr_local)

      call mpas_halo_exch_group_add_field(domain, 'scratch_group', 'cellScratchReal3D', iErr=ierr_local)
      ierr = ior(ierr, ierr_local)

      call mpas_halo_exch_group_add_field(domain, 'scratch_group', 'cellScratchReal2D', iErr=ierr_local)
      ierr = ior(ierr, ierr_local)

      call mpas_halo_exch_group_add_field(domain, 'scratch_group', 'cellScratchReal1D', iErr=ierr_local)
      ierr = ior(ierr, ierr_local)

      call mpas_halo_exch_group_complete(domain, 'scratch_group', ierr_local)
      ierr = ior(ierr, ierr_local)

      if (ierr == 0) then
         test_mesg = trim(test_mesg)//' SUCCESS'
      else
         test_mesg = trim(test_mesg)//' FAILURE'
      end if
      call mpas_log_write(trim(test_mesg))

      !
      ! Exchange a group with persistent fields
      !
      write(test_mesg, '(a)') '  Exchanging a halo group with persistent fields: '

      call mpas_pool_get_array(haloExchTest_pool, 'cellPersistReal1D', array_1d)
      array_1d(:) = -1.0_RKIND
      array_1d(1:nCellsSolve) = real(indexToCellID(1:nCellsSolve), kind=RKIND)

      call mpas_pool_get_array(haloExchTest_pool, 'cellPersistReal2D', array_2d)
      do k = 1, size(array_2d, dim=1)
         array_2d(k,:) = -1.0_RKIND
         array_2d(k,1:nCellsSolve) = real(indexToCellID(1:nCellsSolve), kind=RKIND)
      end do

      call mpas_pool_get_array(haloExchTest_pool, 'cellPersistReal3D', array_3d)
      do k = 1, size(array_3d, dim=1)
      do j = 1, size(array_3d, dim=2)
         array_3d(k,j,:) = -1.0_RKIND
         array_3d(k,j,1:nCellsSolve) = real(indexToCellID(1:nCellsSolve), kind=RKIND)
      end do
      end do

      call mpas_halo_exch_group_full_halo_exch(domain, 'persistent_group', ierr_local)
      ierr = ior(ierr, ierr_local)

      diff = 0.0_RKIND

      diff = diff + sum(abs(array_1d(1:nCells) - real(indexToCellID(1:nCells), kind=RKIND)))

      do k = 1, size(array_2d, dim=1)
         diff = diff + sum(abs(array_2d(k,1:nCells) - real(indexToCellID(1:nCells), kind=RKIND)))
      end do

      do k = 1, size(array_3d, dim=1)
      do j = 1, size(array_3d, dim=2)
         diff = diff + sum(abs(array_3d(k,j,1:nCells) - real(indexToCellID(1:nCells), kind=RKIND)))
      end do
      end do

      if (diff > 0.0_RKIND) then
         ierr_local = 1
         ierr = ior(ierr, ierr_local)
      end if

      if (ierr_local == 0) then
         test_mesg = trim(test_mesg)//' SUCCESS'
      else
         test_mesg = trim(test_mesg)//' FAILURE'
      end if
      call mpas_log_write(trim(test_mesg))

      !
      ! Exchange a group with scratch fields
      !
      write(test_mesg, '(a)') '  Exchanging a halo group with scratch fields: '

      call mpas_pool_get_field(haloExchTest_pool, 'cellScratchReal1D', scratch_1d)
      call mpas_pool_get_field(haloExchTest_pool, 'cellScratchReal2D', scratch_2d)
      call mpas_pool_get_field(haloExchTest_pool, 'cellScratchReal3D', scratch_3d)

      call mpas_allocate_scratch_field(scratch_1d)
      call mpas_allocate_scratch_field(scratch_2d)
      call mpas_allocate_scratch_field(scratch_3d)

      call mpas_pool_get_array(haloExchTest_pool, 'cellScratchReal1D', array_1d)
      array_1d(:) = -1.0_RKIND
      array_1d(1:nCellsSolve) = real(indexToCellID(1:nCellsSolve), kind=RKIND)

      call mpas_pool_get_array(haloExchTest_pool, 'cellScratchReal2D', array_2d)
      do k = 1, size(array_2d, dim=1)
         array_2d(k,:) = -1.0_RKIND
         array_2d(k,1:nCellsSolve) = real(indexToCellID(1:nCellsSolve), kind=RKIND)
      end do

      call mpas_pool_get_array(haloExchTest_pool, 'cellScratchReal3D', array_3d)
      do k = 1, size(array_3d, dim=1)
      do j = 1, size(array_3d, dim=2)
         array_3d(k,j,:) = -1.0_RKIND
         array_3d(k,j,1:nCellsSolve) = real(indexToCellID(1:nCellsSolve), kind=RKIND)
      end do
      end do

      call mpas_halo_exch_group_full_halo_exch(domain, 'scratch_group', ierr_local)
      ierr = ior(ierr, ierr_local)

      diff = 0.0_RKIND

      diff = diff + sum(abs(array_1d(1:nCells) - real(indexToCellID(1:nCells), kind=RKIND)))

      do k = 1, size(array_2d, dim=1)
         diff = diff + sum(abs(array_2d(k,1:nCells) - real(indexToCellID(1:nCells), kind=RKIND)))
      end do

      do k = 1, size(array_3d, dim=1)
      do j = 1, size(array_3d, dim=2)
         diff = diff + sum(abs(array_3d(k,j,1:nCells) - real(indexToCellID(1:nCells), kind=RKIND)))
      end do
      end do

      call mpas_deallocate_scratch_field(scratch_1d)
      call mpas_deallocate_scratch_field(scratch_2d)
      call mpas_deallocate_scratch_field(scratch_3d)

      if (diff > 0.0_RKIND) then
         ierr_local = 1
         ierr = ior(ierr, ierr_local)
      end if

      if (ierr_local == 0) then
         test_mesg = trim(test_mesg)//' SUCCESS'
      else
         test_mesg = trim(test_mesg)//' FAILURE'
      end if
      call mpas_log_write(trim(test_mesg))

      !
      ! Destroy a group with persistent fields
      !
      write(test_mesg, '(a)') '  Destroying a halo group with persistent fields: '
      call mpas_halo_exch_group_destroy(domain, 'persistent_group', ierr_local)
      ierr = ior(ierr, ierr_local)

      if (ierr_local == 0) then
         test_mesg = trim(test_mesg)//' SUCCESS'
      else
         test_mesg = trim(test_mesg)//' FAILURE'
      end if
      call mpas_log_write(trim(test_mesg))

      !
      ! Destroy a group with scratch fields
      !
      write(test_mesg, '(a)') '  Destroying a halo group with scratch fields: '
      call mpas_halo_exch_group_destroy(domain, 'scratch_group', ierr_local)
      ierr = ior(ierr, ierr_local)

      if (ierr_local == 0) then
         test_mesg = trim(test_mesg)//' SUCCESS'
      else
         test_mesg = trim(test_mesg)//' FAILURE'
      end if
      call mpas_log_write(trim(test_mesg))

      !
      ! Finalize the mpas_halo module
      !
      write(test_mesg, '(a)') '  Finalizing the mpas_halo module: '
      call mpas_halo_finalize(domain, ierr_local)
      ierr = ior(ierr, ierr_local)

      if (ierr_local == 0) then
         test_mesg = trim(test_mesg)//' SUCCESS'
      else
         test_mesg = trim(test_mesg)//' FAILURE'
      end if
      call mpas_log_write(trim(test_mesg))

      call mpas_dmpar_max_int(domain % dminfo, ierr, ierr_global)
      ierr = ierr_global

   end subroutine mpas_halo_tests

end module mpas_halo_testing
