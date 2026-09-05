#include <define.h>

MODULE MOD_Mesh

!------------------------------------------------------------------------------------
! !DESCRIPTION:
!
!    MESH refers to the set of largest elements in CoLM.
!
!    In CoLM, the global/regional area is divided into a hierarchical structure:
!    1. If GRIDBASED or UNSTRUCTURED is defined, it is
!       ELEMENT >>> PATCH
!    2. If CATCHMENT is defined, it is
!       ELEMENT >>> HRU >>> PATCH
!    If Plant Function Type classification is used, PATCH is further divided into PFT.
!    If Plant Community classification is used,     PATCH is further divided into PC.
!
!    To represent ELEMENT in CoLM, the land surface is first divided into pixels,
!    which are rasterized points defined by fine-resolution data.
!
!    ELEMENT in MESH is set of pixels:
!    1. If GRIDBASED,    ELEMENT is set of pixels in a longitude-latitude rectangle.
!    2. If UNSTRUCTURED, ELEMENT is set of pixels in an irregular area (usually polygon).
!    3. If CATCHMENT,    ELEMENT is set of pixels in a catchment whose area is less than
!       a predefined value.
!
!    If GRIDBASED is defined, MESH is built by using input files containing mask of
!    land area or by defining the resolution of longitude-latitude grid.
!    If CATCHMENT or UNSTRUCTURED is defined, MESH is built by using input files
!    containing index of elements.
!
!  Created by Shupeng Zhang, May 2023
!------------------------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Grid
   IMPLICIT NONE

   ! ---- data types ----
   type :: irregular_elm_type

      integer*8 :: indx
      integer   :: xblk, yblk

      integer :: npxl
      integer, allocatable :: ilon(:)
      integer, allocatable :: ilat(:)

   END type irregular_elm_type

   ! ---- Instance ----
   type (grid_type) :: gridmesh

   integer :: numelm
   type (irregular_elm_type), allocatable :: mesh (:)

   integer, allocatable :: nelm_blk(:,:)

#ifdef GRIDBASED
   logical :: read_mesh_from_file = .true.
#endif

CONTAINS

   ! ------
#ifdef GRIDBASED
   SUBROUTINE init_gridbased_mesh_grid ()

   USE MOD_MPAS_MPI
   USE MOD_Namelist
   IMPLICIT NONE

      IF (mpas_is_root) THEN
         inquire (file=trim(DEF_file_mesh), exist=read_mesh_from_file)
      ENDIF
#ifdef MPAS_MPI
      CALL mpi_bcast (read_mesh_from_file, 1, MPI_LOGICAL, mpas_root, mpas_comm, mpas_mpi_ierr)
#endif
      IF (read_mesh_from_file) THEN
         CALL gridmesh%define_from_file (DEF_file_mesh)
      ELSE
         CALL gridmesh%define_by_res (DEF_GRIDBASED_lon_res, DEF_GRIDBASED_lat_res)
      ENDIF

   END SUBROUTINE init_gridbased_mesh_grid
#endif

   ! -------
   SUBROUTINE copy_elm (elm_from, elm_to)

   IMPLICIT NONE
   type (irregular_elm_type), intent(in)  :: elm_from
   type (irregular_elm_type), intent(out) :: elm_to

      elm_to%indx = elm_from%indx
      elm_to%npxl = elm_from%npxl
      elm_to%xblk = elm_from%xblk
      elm_to%yblk = elm_from%yblk

      IF (allocated(elm_to%ilat)) deallocate(elm_to%ilat)
      IF (allocated(elm_to%ilon)) deallocate(elm_to%ilon)

      allocate (elm_to%ilat (elm_to%npxl))
      allocate (elm_to%ilon (elm_to%npxl))
      elm_to%ilon = elm_from%ilon
      elm_to%ilat = elm_from%ilat

   END SUBROUTINE copy_elm

   ! --------------------------------


   ! --------------------------------
   SUBROUTINE mesh_free_mem ()

   IMPLICIT NONE

   ! Local variables
   integer :: ie

      IF (allocated(mesh)) THEN
         DO ie = 1, size(mesh)
            IF (allocated(mesh(ie)%ilon)) deallocate (mesh(ie)%ilon)
            IF (allocated(mesh(ie)%ilat)) deallocate (mesh(ie)%ilat)
         ENDDO

         deallocate (mesh)
      ENDIF

      IF (allocated(nelm_blk)) deallocate(nelm_blk)
      CALL grid_free_mem(gridmesh)

      numelm = 0

   END SUBROUTINE mesh_free_mem

END MODULE MOD_Mesh
