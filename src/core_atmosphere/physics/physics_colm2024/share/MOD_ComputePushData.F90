#include <define.h>

MODULE MOD_ComputePushData
!--------------------------------------------------------------------------------
! DESCRIPTION:
!--------------------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_DataType
   USE MOD_MPAS_MPI
   USE MOD_Utils
   USE, INTRINSIC :: ieee_arithmetic, only: ieee_is_finite
   IMPLICIT NONE

   ! -- Data Type : push data between ranks --
   type :: compute_pushdata_type

      integer :: num_req_uniq

      integer,  allocatable :: addr_single (:)

      integer,  allocatable :: addr_multi  (:,:)
      real(r8), allocatable :: area_multi  (:,:)
      real(r8), allocatable :: sum_area    (:)

      ! data is on the same processor
      integer :: nself
      integer,  allocatable :: self_from (:)
      integer,  allocatable :: self_to   (:)
#ifdef MPAS_MPI
      ! data is on other processors
      integer, allocatable :: n_to_other   (:)
      integer, allocatable :: n_from_other (:)
      type(pointer_int32_1d), allocatable :: to_other (:)
      type(pointer_int32_1d), allocatable :: other_to (:)
#endif
   CONTAINS
      final :: compute_pushdata_free_mem
   END type compute_pushdata_type


   ! -- Data Type : remap data on ranks --
   type :: compute_remapdata_type

      integer :: num_grid
      integer, allocatable :: ilon_me (:)
      integer, allocatable :: ilat_me (:)
      integer, allocatable :: ids_me  (:)

      integer :: npset
      real(r8), allocatable :: sum_area (:)
      integer,  allocatable :: npart    (:)
      type(pointer_int32_1d), allocatable :: part_to  (:) !
      type(pointer_real8_1d), allocatable :: areapart (:) ! intersection area

   CONTAINS
      final :: compute_remapdata_free_mem
   END type compute_remapdata_type

   ! -- public subroutines --
   INTERFACE build_compute_pushdata
      MODULE procedure build_compute_pushdata_single
      MODULE procedure build_compute_pushdata_multi
   END INTERFACE build_compute_pushdata

   PUBLIC :: build_compute_remapdata

   INTERFACE compute_push_data
      MODULE procedure compute_push_data_single_real8
      MODULE procedure compute_push_data_single_int32
      MODULE procedure compute_push_data_multi_real8
   END INTERFACE compute_push_data

   INTERFACE compute_remap_data_pset2grid
      MODULE procedure compute_remap_data_pset2grid_real8
   END INTERFACE compute_remap_data_pset2grid

   INTERFACE compute_remap_data_grid2pset
      MODULE procedure compute_remap_data_grid2pset_real8
   END INTERFACE compute_remap_data_grid2pset

CONTAINS

   ! ----------
   SUBROUTINE build_compute_pushdata_uniq (num_me, ids_me, n_req_uniq, ids_req_uniq, pushdata)

   IMPLICIT NONE

   integer, intent(in) :: num_me,     ids_me       (:)
   integer, intent(in) :: n_req_uniq, ids_req_uniq (:)
   type(compute_pushdata_type), intent(inout) :: pushdata

   ! Local Variables
   integer, allocatable :: ids_me_sorted(:), order_ids(:), self_from(:)
#ifdef MPAS_MPI
   integer, allocatable :: ids(:), loc_from_me(:), loc_from_other(:)
   integer :: request(3)
#endif
   integer :: i, iloc, irank, jrank, n_req_other


      IF (.true.) THEN

         IF (num_me < 0 .or. num_me > size(ids_me)) THEN
            CALL CoLM_stop('Invalid local ID count while building CoLM compute push data.')
         ENDIF
         IF (n_req_uniq < 0 .or. n_req_uniq > size(ids_req_uniq)) THEN
            CALL CoLM_stop('Invalid request ID count while building CoLM compute push data.')
         ENDIF

	         allocate (ids_me_sorted (num_me))
	         allocate (order_ids     (num_me))
	         IF (num_me > 0) THEN
	            ids_me_sorted = ids_me(1:num_me)
	            order_ids = (/(i, i = 1, num_me)/)
	            CALL quicksort (num_me, ids_me_sorted, order_ids)
	            IF (num_me > 1) THEN
	               IF (any(ids_me_sorted(2:num_me) == ids_me_sorted(1:num_me-1))) THEN
	                  CALL CoLM_stop('Duplicate local source IDs while building CoLM compute push data.')
	               ENDIF
	            ENDIF
	         ENDIF

         pushdata%nself = 0

         IF (n_req_uniq > 0) THEN
            allocate (self_from (n_req_uniq))
            self_from(:) = -1

            DO i = 1, n_req_uniq
               iloc = find_in_sorted_list1 (ids_req_uniq(i), num_me, ids_me_sorted)
               IF (iloc > 0) THEN
                  self_from(i) = order_ids(iloc)
               ENDIF
            ENDDO

            pushdata%nself = count(self_from > 0)
            IF (pushdata%nself > 0) THEN
               allocate (pushdata%self_from (pushdata%nself))
               allocate (pushdata%self_to   (pushdata%nself))
               pushdata%self_from = pack(self_from, self_from > 0)
               pushdata%self_to   = pack((/(i,i=1,n_req_uniq)/), self_from > 0)
            ENDIF
         ENDIF

#ifdef MPAS_MPI
         CALL mpi_barrier (mpas_comm, mpas_mpi_ierr)
         CALL mpas_mpi_check('compute-push layout synchronization')

         allocate (pushdata%n_to_other   (0:mpas_size-1))
         allocate (pushdata%to_other     (0:mpas_size-1))

         allocate (pushdata%n_from_other (0:mpas_size-1))
         allocate (pushdata%other_to     (0:mpas_size-1))

         pushdata%n_to_other  (:) = 0
         pushdata%n_from_other(:) = 0

         IF (n_req_uniq > 0) allocate (loc_from_other (n_req_uniq))

         irank = modulo(mpas_rank+1, mpas_size)
         jrank = modulo(mpas_rank-1, mpas_size)
         DO WHILE (irank /= mpas_rank)

            CALL mpi_isend (n_req_uniq, 1, MPI_INTEGER, jrank, 10, &
               mpas_comm, request(1), mpas_mpi_ierr)
            CALL mpas_mpi_check('compute-push request-count send')

            IF (n_req_uniq > 0) THEN
               CALL mpi_isend(ids_req_uniq, n_req_uniq, MPI_INTEGER, jrank, 11, &
                  mpas_comm, request(2), mpas_mpi_ierr)
               CALL mpas_mpi_check('compute-push request-ID send')
            ENDIF

            CALL mpi_recv (n_req_other, 1, MPI_INTEGER, irank, 10, &
               mpas_comm, mpas_status, mpas_mpi_ierr)
            CALL mpas_mpi_check('compute-push request-count receive')

            IF (n_req_other > 0) THEN

               allocate (ids (n_req_other))
               CALL mpi_recv (ids, n_req_other, MPI_INTEGER, irank, 11, &
                  mpas_comm, mpas_status, mpas_mpi_ierr)
               CALL mpas_mpi_check('compute-push request-ID receive')

               allocate (loc_from_me (n_req_other))
               loc_from_me(:) = -1

               IF (num_me > 0) THEN
                  DO i = 1, n_req_other
                     iloc = find_in_sorted_list1 (ids(i), num_me, ids_me_sorted)
                     IF (iloc > 0) THEN
                        loc_from_me(i) = order_ids(iloc)
                     ENDIF
                  ENDDO
               ENDIF

               pushdata%n_to_other(irank) = count(loc_from_me > 0)
               IF (pushdata%n_to_other(irank) > 0) THEN
                  allocate (pushdata%to_other(irank)%val (pushdata%n_to_other(irank)))
                  pushdata%to_other(irank)%val = pack(loc_from_me, loc_from_me > 0)
               ENDIF

               CALL mpi_isend (loc_from_me, n_req_other, MPI_INTEGER, irank, 12, &
                  mpas_comm, request(3), mpas_mpi_ierr)
               CALL mpas_mpi_check('compute-push source-location send')

            ENDIF

            IF (n_req_uniq > 0) THEN

               CALL mpi_recv (loc_from_other, n_req_uniq, MPI_INTEGER, &
                  jrank, 12, mpas_comm, mpas_status, mpas_mpi_ierr)
               CALL mpas_mpi_check('compute-push source-location receive')

               pushdata%n_from_other(jrank) = count(loc_from_other > 0)
               IF (pushdata%n_from_other(jrank) > 0) THEN
                  allocate (pushdata%other_to(jrank)%val (pushdata%n_from_other(jrank)))
                  pushdata%other_to(jrank)%val = pack((/(i,i=1,n_req_uniq)/), loc_from_other > 0)
               ENDIF

            ENDIF

            CALL mpi_wait(request(1), MPI_STATUS_IGNORE, mpas_mpi_ierr)
            CALL mpas_mpi_check('compute-push request-count send completion')
            IF (n_req_uniq > 0) THEN
               CALL mpi_wait(request(2), MPI_STATUS_IGNORE, mpas_mpi_ierr)
               CALL mpas_mpi_check('compute-push request-ID send completion')
            ENDIF
            IF (n_req_other > 0) THEN
               CALL mpi_wait(request(3), MPI_STATUS_IGNORE, mpas_mpi_ierr)
               CALL mpas_mpi_check('compute-push source-location send completion')
            ENDIF

            IF (allocated(ids        )) deallocate(ids        )
            IF (allocated(loc_from_me)) deallocate(loc_from_me)

            irank = modulo(irank+1, mpas_size)
            jrank = modulo(jrank-1, mpas_size)
         ENDDO

         IF (allocated (loc_from_other)) deallocate (loc_from_other)

         CALL mpi_barrier (mpas_comm, mpas_mpi_ierr)
         CALL mpas_mpi_check('compute-push layout completion')
#endif

         IF (allocated(ids_me_sorted)) deallocate(ids_me_sorted)
         IF (allocated(order_ids    )) deallocate(order_ids    )
         IF (allocated(self_from    )) deallocate(self_from    )

      ENDIF

   END SUBROUTINE build_compute_pushdata_uniq

   ! ----------
   SUBROUTINE build_compute_pushdata_single (num_me, ids_me, num_req, ids_req, pushdata)

   IMPLICIT NONE

   integer, intent(in) :: num_me,  ids_me  (:)
   integer, intent(in) :: num_req, ids_req (:)
   type(compute_pushdata_type), intent(inout) :: pushdata

   ! Local Variables
   integer :: n_req_uniq, iloc, i
   integer, allocatable :: ids_req_uniq (:)

      IF (.true.) THEN

         IF (num_me < 0 .or. num_me > size(ids_me)) THEN
            CALL CoLM_stop('Invalid local ID count while building single CoLM compute push data.')
         ENDIF
         IF (num_req < 0 .or. num_req > size(ids_req)) THEN
            CALL CoLM_stop('Invalid request count while building single CoLM compute push data.')
         ENDIF

         n_req_uniq = 0

	         allocate (ids_req_uniq (num_req))
	         IF (num_req > 0) THEN
	            DO i = 1, num_req
	               CALL insert_into_sorted_list1 (ids_req(i), n_req_uniq, ids_req_uniq, iloc)
	            ENDDO

            allocate (pushdata%addr_single (num_req))
            DO i = 1, num_req
               pushdata%addr_single(i) = &
                  find_in_sorted_list1 (ids_req(i), n_req_uniq, ids_req_uniq(1:n_req_uniq))
            ENDDO
         ENDIF

         pushdata%num_req_uniq = n_req_uniq

         CALL build_compute_pushdata_uniq ( &
            num_me, ids_me, n_req_uniq, ids_req_uniq(1:n_req_uniq), pushdata)

         IF (allocated (ids_req_uniq)) deallocate(ids_req_uniq)

      ENDIF

   END SUBROUTINE build_compute_pushdata_single

   ! ----------
   SUBROUTINE build_compute_pushdata_multi ( &
         num_me, ids_me, num_req, ids_req, area_req, pushdata)

   IMPLICIT NONE

   integer,  intent(in) :: num_me,  ids_me  (:)
   integer,  intent(in) :: num_req, ids_req (:,:)
   real(r8), intent(in) :: area_req(:,:)
   type(compute_pushdata_type), intent(inout) :: pushdata

   ! Local Variables
   integer :: ndim1, n_req_uniq, iloc, i, j, irank
   integer, allocatable :: ids_req_uniq (:)
   logical, allocatable :: id_found     (:)

      IF (.true.) THEN

         IF (num_me < 0 .or. num_me > size(ids_me)) THEN
            CALL CoLM_stop('Invalid local ID count while building multi CoLM compute push data.')
         ENDIF
         IF (num_req < 0 .or. num_req > size(ids_req, 2) .or. num_req > size(area_req, 2)) THEN
            CALL CoLM_stop('Invalid request count while building multi CoLM compute push data.')
         ENDIF
         IF (size(area_req, 1) /= size(ids_req, 1)) THEN
            CALL CoLM_stop('ID and area shapes differ while building multi CoLM compute push data.')
         ENDIF
         IF (num_req > 0) THEN
            IF (.not. all(ieee_is_finite(area_req(:,1:num_req)))) THEN
               CALL CoLM_stop('Non-finite overlap area while building multi CoLM compute push data.')
            ENDIF
         ENDIF

         n_req_uniq = 0

	         ndim1 = size(ids_req,1)
	         allocate (ids_req_uniq (ndim1*num_req))

	         IF (num_req > 0) THEN

	            DO j = 1, num_req
	               DO i = 1, ndim1
                  CALL insert_into_sorted_list1 (ids_req(i,j), n_req_uniq, ids_req_uniq, iloc)
               ENDDO
            ENDDO

            allocate (pushdata%addr_multi (ndim1,num_req))

            DO j = 1, num_req
               DO i = 1, ndim1
                  pushdata%addr_multi(i,j) = &
                     find_in_sorted_list1 (ids_req(i,j), n_req_uniq, ids_req_uniq(1:n_req_uniq))
               ENDDO
            ENDDO
         ENDIF

         pushdata%num_req_uniq = n_req_uniq

         CALL build_compute_pushdata_uniq ( &
            num_me, ids_me, n_req_uniq, ids_req_uniq(1:n_req_uniq), pushdata)

         IF (num_req > 0) THEN
            allocate (pushdata%area_multi (ndim1,num_req))
            allocate (pushdata%sum_area   (num_req))

            pushdata%area_multi = area_req(:,1:num_req)

            WHERE ((pushdata%area_multi <= 0.) .or. (ids_req(:,1:num_req) <= 0))
               pushdata%area_multi = 0.
            END WHERE

            allocate (id_found (n_req_uniq))
            id_found(:) = .false.

            IF (pushdata%nself > 0) id_found(pushdata%self_to) = .true.
#ifdef MPAS_MPI
            DO irank = 0, mpas_size-1
               IF (pushdata%n_from_other(irank) > 0) THEN
                  id_found(pushdata%other_to(irank)%val) = .true.
               ENDIF
            ENDDO
#endif

            DO j = 1, num_req
               DO i = 1, ndim1
                  IF (.not. id_found(pushdata%addr_multi(i,j))) then
                     pushdata%area_multi(i,j) = 0.
                  ENDIF
               ENDDO
            ENDDO

            pushdata%sum_area = sum(pushdata%area_multi, dim = 1)

            deallocate (id_found)
         ENDIF

         IF (allocated (ids_req_uniq)) deallocate(ids_req_uniq)

      ENDIF

   END SUBROUTINE build_compute_pushdata_multi

   ! ----------

   ! ----------
   SUBROUTINE build_compute_remapdata (pixelset, grid, remapdata)

   USE MOD_Grid
   USE MOD_Pixelset
   USE MOD_SpatialMapping
   IMPLICIT NONE

   type(pixelset_type), intent(in) :: pixelset
   type(grid_type),     intent(in) :: grid

   type(compute_remapdata_type), intent(inout) :: remapdata

   ! Local Variables
   type(spatial_mapping_type) :: mapping
   integer, allocatable :: ilon_me(:), ilat_me(:)
   integer :: ngrid, iproc, ig, iloc, iset, ipart


      CALL mapping%build_arealweighted (grid, pixelset)

      IF (.true.) THEN

         ngrid = 0
         DO iproc = 0, mpas_size-1
            ngrid = ngrid + mapping%glist(iproc)%ng
         ENDDO

         IF (ngrid > 0) THEN
            allocate (ilon_me (ngrid))
            allocate (ilat_me (ngrid))
         ENDIF

         ngrid = 0
         DO iproc = 0, mpas_size-1
            DO ig = 1, mapping%glist(iproc)%ng
               CALL insert_into_sorted_list2 ( &
                  mapping%glist(iproc)%ilon(ig), mapping%glist(iproc)%ilat(ig), &
                  ngrid, ilon_me, ilat_me, iloc)
            ENDDO
         ENDDO

         remapdata%num_grid = ngrid

         IF (ngrid > 0) THEN
            allocate (remapdata%ilon_me (ngrid))
            allocate (remapdata%ilat_me (ngrid))
            allocate (remapdata%ids_me  (ngrid))
            remapdata%ilon_me = ilon_me(1:ngrid)
            remapdata%ilat_me = ilat_me(1:ngrid)
            remapdata%ids_me  = (ilat_me(1:ngrid)-1) * grid%nlon + ilon_me(1:ngrid)
         ENDIF

         remapdata%npset = mapping%npset

         IF (remapdata%npset < 0) THEN
            CALL CoLM_stop('Negative pixel-set count while building CoLM compute remap data.')
         ENDIF

         IF (remapdata%npset > 0) THEN

            IF (.not. allocated(mapping%npart) .or. .not. allocated(mapping%address) .or. &
                .not. allocated(mapping%areapart) .or. .not. allocated(mapping%glist)) THEN
               CALL CoLM_stop('Incomplete spatial mapping while building CoLM compute remap data.')
            ENDIF
            IF (size(mapping%npart) < remapdata%npset .or. &
                size(mapping%address) < remapdata%npset .or. &
                size(mapping%areapart) < remapdata%npset) THEN
               CALL CoLM_stop('Spatial mapping count mismatch while building CoLM compute remap data.')
            ENDIF

            allocate (remapdata%sum_area (remapdata%npset))
            allocate (remapdata%npart    (remapdata%npset))
            allocate (remapdata%part_to  (remapdata%npset))
            allocate (remapdata%areapart (remapdata%npset))

            remapdata%npart = mapping%npart
            remapdata%sum_area = 0._r8

            IF (any(remapdata%npart < 0)) THEN
               CALL CoLM_stop('Negative part count while building CoLM compute remap data.')
            ENDIF

            DO iset = 1, remapdata%npset
               IF (remapdata%npart(iset) > 0) THEN
                  IF (.not. allocated(mapping%address(iset)%val) .or. &
                      .not. allocated(mapping%areapart(iset)%val)) THEN
                     CALL CoLM_stop('Incomplete spatial mapping while building CoLM compute remap data.')
                  ENDIF
                  IF (size(mapping%address(iset)%val,1) < 2 .or. &
                      size(mapping%address(iset)%val,2) < remapdata%npart(iset) .or. &
                      size(mapping%areapart(iset)%val) < remapdata%npart(iset)) THEN
                     CALL CoLM_stop('Spatial mapping shape mismatch while building CoLM compute remap data.')
                  ENDIF
                  allocate (remapdata%part_to(iset)%val  (remapdata%npart(iset)))
                  allocate (remapdata%areapart(iset)%val (remapdata%npart(iset)))
               ENDIF

               DO ipart = 1, remapdata%npart(iset)
                  iproc = mapping%address(iset)%val(1,ipart)
                  iloc  = mapping%address(iset)%val(2,ipart)
                  IF (iproc < lbound(mapping%glist,1) .or. iproc > ubound(mapping%glist,1)) THEN
                     CALL CoLM_stop('Spatial mapping rank is outside the CoLM active communicator.')
                  ENDIF
                  IF (.not. allocated(mapping%glist(iproc)%ilon) .or. &
                      .not. allocated(mapping%glist(iproc)%ilat)) THEN
                     CALL CoLM_stop('Spatial mapping grid list is incomplete.')
                  ENDIF
                  IF (iloc < 1 .or. iloc > mapping%glist(iproc)%ng .or. &
                      iloc > size(mapping%glist(iproc)%ilon) .or. &
                      iloc > size(mapping%glist(iproc)%ilat)) THEN
                     CALL CoLM_stop('Spatial mapping address is outside the local CoLM grid list.')
                  ENDIF
                  remapdata%part_to(iset)%val(ipart)  = find_in_sorted_list2 ( &
                     mapping%glist(iproc)%ilon(iloc), mapping%glist(iproc)%ilat(iloc), ngrid, ilon_me, ilat_me)
                  IF (remapdata%part_to(iset)%val(ipart) <= 0) THEN
                     CALL CoLM_stop('Spatial mapping target is missing from the local CoLM remap grid.')
                  ENDIF
                  ! from km^2 to m^2
                  remapdata%areapart(iset)%val(ipart) = mapping%areapart(iset)%val(ipart) * 1.e6_r8
                  IF (.not. ieee_is_finite(remapdata%areapart(iset)%val(ipart)) .or. &
                      remapdata%areapart(iset)%val(ipart) <= 0._r8) THEN
                     CALL CoLM_stop('Invalid overlap area while building CoLM compute remap data.')
                  ENDIF
               ENDDO

               IF (remapdata%npart(iset) > 0) THEN
                  remapdata%sum_area(iset) = sum(remapdata%areapart(iset)%val)
                  IF (.not. ieee_is_finite(remapdata%sum_area(iset)) .or. &
                      remapdata%sum_area(iset) <= 0._r8) THEN
                     CALL CoLM_stop('Invalid total area while building CoLM compute remap data.')
                  ENDIF
               ENDIF
            ENDDO
         ENDIF

         IF (allocated(ilon_me)) deallocate(ilon_me)
         IF (allocated(ilat_me)) deallocate(ilat_me)

      ENDIF

   END SUBROUTINE build_compute_remapdata

   ! ----------
   SUBROUTINE validate_compute_push_layout (pushdata, send_size, recv_size)

   IMPLICIT NONE

   type(compute_pushdata_type), intent(in) :: pushdata
   integer, intent(in) :: send_size, recv_size

   integer :: irank

      IF (send_size < 0 .or. recv_size < 0 .or. pushdata%num_req_uniq < 0 .or. &
          pushdata%num_req_uniq > recv_size .or. pushdata%nself < 0) THEN
         CALL CoLM_stop('Invalid dimensions in CoLM compute push layout.')
      ENDIF

      IF (pushdata%nself > 0) THEN
         IF (.not. allocated(pushdata%self_from) .or. .not. allocated(pushdata%self_to)) THEN
            CALL CoLM_stop('Incomplete self-address vectors in CoLM compute push layout.')
         ENDIF
         IF (size(pushdata%self_from) < pushdata%nself .or. size(pushdata%self_to) < pushdata%nself) THEN
            CALL CoLM_stop('Short self-address vectors in CoLM compute push layout.')
         ENDIF
         IF (any(pushdata%self_from(1:pushdata%nself) < 1) .or. &
             any(pushdata%self_from(1:pushdata%nself) > send_size) .or. &
             any(pushdata%self_to(1:pushdata%nself) < 1) .or. &
             any(pushdata%self_to(1:pushdata%nself) > recv_size)) THEN
            CALL CoLM_stop('Out-of-range self address in CoLM compute push layout.')
         ENDIF
      ENDIF

#ifdef MPAS_MPI
      IF (.not. allocated(pushdata%n_to_other) .or. .not. allocated(pushdata%n_from_other) .or. &
          .not. allocated(pushdata%to_other) .or. .not. allocated(pushdata%other_to)) THEN
         CALL CoLM_stop('Incomplete remote-address tables in CoLM compute push layout.')
      ENDIF
      IF (lbound(pushdata%n_to_other,1) > 0 .or. ubound(pushdata%n_to_other,1) < mpas_size-1 .or. &
          lbound(pushdata%n_from_other,1) > 0 .or. ubound(pushdata%n_from_other,1) < mpas_size-1 .or. &
          lbound(pushdata%to_other,1) > 0 .or. ubound(pushdata%to_other,1) < mpas_size-1 .or. &
          lbound(pushdata%other_to,1) > 0 .or. ubound(pushdata%other_to,1) < mpas_size-1) THEN
         CALL CoLM_stop('Remote-address table bounds do not match the CoLM compute communicator.')
      ENDIF
      IF (any(pushdata%n_to_other(0:mpas_size-1) < 0) .or. &
          any(pushdata%n_from_other(0:mpas_size-1) < 0)) THEN
         CALL CoLM_stop('Negative remote message count in CoLM compute push layout.')
      ENDIF

      DO irank = 0, mpas_size-1
         IF (pushdata%n_to_other(irank) > 0) THEN
            IF (.not. allocated(pushdata%to_other(irank)%val)) THEN
               CALL CoLM_stop('Missing remote send addresses in CoLM compute push layout.')
            ENDIF
            IF (size(pushdata%to_other(irank)%val) < pushdata%n_to_other(irank) .or. &
                any(pushdata%to_other(irank)%val(1:pushdata%n_to_other(irank)) < 1) .or. &
                any(pushdata%to_other(irank)%val(1:pushdata%n_to_other(irank)) > send_size)) THEN
               CALL CoLM_stop('Out-of-range remote send address in CoLM compute push layout.')
            ENDIF
         ENDIF
         IF (pushdata%n_from_other(irank) > 0) THEN
            IF (.not. allocated(pushdata%other_to(irank)%val)) THEN
               CALL CoLM_stop('Missing remote receive addresses in CoLM compute push layout.')
            ENDIF
            IF (size(pushdata%other_to(irank)%val) < pushdata%n_from_other(irank) .or. &
                any(pushdata%other_to(irank)%val(1:pushdata%n_from_other(irank)) < 1) .or. &
                any(pushdata%other_to(irank)%val(1:pushdata%n_from_other(irank)) > recv_size)) THEN
               CALL CoLM_stop('Out-of-range remote receive address in CoLM compute push layout.')
            ENDIF
         ENDIF
      ENDDO
#endif

   END SUBROUTINE validate_compute_push_layout

   ! ----------
   SUBROUTINE compute_push_data_uniq_real8 ( &
         pushdata, vec_send, vec_recv, fillvalue)

   IMPLICIT NONE

   type(compute_pushdata_type), intent(in) :: pushdata

   real(r8), intent(in)   , optional :: vec_send (:)
   real(r8), intent(inout), optional :: vec_recv (:)
   real(r8), intent(in)   , optional :: fillvalue

   ! Local Variables
   integer :: ndatasend
   integer,  allocatable :: req_send  (:)
   real(r8), allocatable :: sendcache (:)

   integer :: ndatarecv
   integer,  allocatable :: req_recv  (:)
   real(r8), allocatable :: recvcache (:)

   integer :: irank, iproc, idsp, istt, iend, i, i_to


      IF (.true.) THEN

         IF (.not. present(vec_send) .or. .not. present(vec_recv) .or. .not. present(fillvalue)) THEN
            CALL CoLM_stop('CoLM real compute push requires send, receive, and fill-value arguments.')
         ENDIF
         CALL validate_compute_push_layout(pushdata, size(vec_send), size(vec_recv))

         IF (pushdata%num_req_uniq > 0) THEN
            vec_recv = fillvalue
         ENDIF

         IF (pushdata%nself > 0) THEN
            vec_recv(pushdata%self_to) = vec_send(pushdata%self_from)
         ENDIF

#ifdef MPAS_MPI
         ndatasend = sum(pushdata%n_to_other)
         IF (ndatasend > 0) THEN

            allocate (sendcache(ndatasend))
            allocate (req_send (count(pushdata%n_to_other > 0)))

            iproc = 0
            idsp  = 0
            DO irank = 0, mpas_size-1
               IF (pushdata%n_to_other(irank) > 0) THEN
                  iproc = iproc + 1
                  istt  = idsp + 1
                  iend  = idsp + pushdata%n_to_other(irank)

                  sendcache(istt:iend) = vec_send(pushdata%to_other(irank)%val)
                  CALL mpi_isend(sendcache(istt:iend), pushdata%n_to_other(irank), MPI_REAL8, &
                     irank, 101, mpas_comm, req_send(iproc), mpas_mpi_ierr)
                  CALL mpas_mpi_check('real compute-push data send')

                  idsp = iend
               ENDIF
            ENDDO
         ENDIF

         ndatarecv = sum(pushdata%n_from_other)
         IF (ndatarecv > 0) THEN

            allocate (recvcache(ndatarecv))
            allocate (req_recv (count(pushdata%n_from_other > 0)))

            iproc = 0
            idsp  = 0
            DO irank = 0, mpas_size-1
               IF (pushdata%n_from_other(irank) > 0) THEN
                  iproc = iproc + 1
                  istt  = idsp + 1
                  iend  = idsp + pushdata%n_from_other(irank)

                  CALL mpi_irecv(recvcache(istt:iend), pushdata%n_from_other(irank), MPI_REAL8, &
                     irank, 101, mpas_comm, req_recv(iproc), mpas_mpi_ierr)
                  CALL mpas_mpi_check('real compute-push data receive')

                  idsp = iend
               ENDIF
            ENDDO
         ENDIF

         IF (ndatarecv > 0) THEN

            CALL mpi_waitall(size(req_recv), req_recv, MPI_STATUSES_IGNORE, mpas_mpi_ierr)
            CALL mpas_mpi_check('real compute-push receive completion')

            idsp = 0
            DO irank = 0, mpas_size-1
               DO i = 1, pushdata%n_from_other(irank)

                  IF (recvcache(idsp+i) /= fillvalue) THEN
                     i_to = pushdata%other_to(irank)%val(i)
                     IF (vec_recv(i_to) == fillvalue) THEN
                        vec_recv(i_to) = recvcache(idsp+i)
                     ELSE
                        vec_recv(i_to) = vec_recv(i_to) + recvcache(idsp+i)
                     ENDIF
                  ENDIF

               ENDDO
               idsp = idsp + pushdata%n_from_other(irank)
            ENDDO

         ENDIF

         IF (ndatasend > 0) THEN
            CALL mpi_waitall(size(req_send), req_send, MPI_STATUSES_IGNORE, mpas_mpi_ierr)
            CALL mpas_mpi_check('real compute-push send completion')
         ENDIF

         IF (allocated(req_send )) deallocate(req_send )
         IF (allocated(sendcache)) deallocate(sendcache)
         IF (allocated(req_recv )) deallocate(req_recv )
         IF (allocated(recvcache)) deallocate(recvcache)
#endif

      ENDIF

   END SUBROUTINE compute_push_data_uniq_real8

   ! ----------
   SUBROUTINE compute_push_data_uniq_int32 ( &
         pushdata, vec_send, vec_recv, fillvalue)

   IMPLICIT NONE

   type(compute_pushdata_type), intent(in) :: pushdata

   integer, intent(in)   , optional :: vec_send (:)
   integer, intent(inout), optional :: vec_recv (:)
   integer, intent(in)   , optional :: fillvalue

   ! Local Variables
   integer :: ndatasend
   integer, allocatable :: req_send  (:)
   integer, allocatable :: sendcache (:)

   integer :: ndatarecv
   integer, allocatable :: req_recv  (:)
   integer, allocatable :: recvcache (:)

   integer :: irank, iproc, idsp, istt, iend, i, i_to


      IF (.true.) THEN

         IF (.not. present(vec_send) .or. .not. present(vec_recv) .or. .not. present(fillvalue)) THEN
            CALL CoLM_stop('CoLM integer compute push requires send, receive, and fill-value arguments.')
         ENDIF
         CALL validate_compute_push_layout(pushdata, size(vec_send), size(vec_recv))

         IF (pushdata%num_req_uniq > 0) THEN
            vec_recv = fillvalue
         ENDIF

         IF (pushdata%nself > 0) THEN
            vec_recv(pushdata%self_to) = vec_send(pushdata%self_from)
         ENDIF

#ifdef MPAS_MPI
         ndatasend = sum(pushdata%n_to_other)
         IF (ndatasend > 0) THEN

            allocate (sendcache(ndatasend))
            allocate (req_send (count(pushdata%n_to_other > 0)))

            iproc = 0
            idsp  = 0
            DO irank = 0, mpas_size-1
               IF (pushdata%n_to_other(irank) > 0) THEN
                  iproc = iproc + 1
                  istt  = idsp + 1
                  iend  = idsp + pushdata%n_to_other(irank)

                  sendcache(istt:iend) = vec_send(pushdata%to_other(irank)%val)
                  CALL mpi_isend(sendcache(istt:iend), pushdata%n_to_other(irank), MPI_INTEGER, &
                     irank, 101, mpas_comm, req_send(iproc), mpas_mpi_ierr)
                  CALL mpas_mpi_check('integer compute-push data send')

                  idsp = iend
               ENDIF
            ENDDO
         ENDIF

         ndatarecv = sum(pushdata%n_from_other)
         IF (ndatarecv > 0) THEN

            allocate (recvcache(ndatarecv))
            allocate (req_recv (count(pushdata%n_from_other > 0)))

            iproc = 0
            idsp  = 0
            DO irank = 0, mpas_size-1
               IF (pushdata%n_from_other(irank) > 0) THEN
                  iproc = iproc + 1
                  istt  = idsp + 1
                  iend  = idsp + pushdata%n_from_other(irank)

                  CALL mpi_irecv(recvcache(istt:iend), pushdata%n_from_other(irank), MPI_INTEGER, &
                     irank, 101, mpas_comm, req_recv(iproc), mpas_mpi_ierr)
                  CALL mpas_mpi_check('integer compute-push data receive')

                  idsp = iend
               ENDIF
            ENDDO
         ENDIF

         IF (ndatarecv > 0) THEN

            CALL mpi_waitall(size(req_recv), req_recv, MPI_STATUSES_IGNORE, mpas_mpi_ierr)
            CALL mpas_mpi_check('integer compute-push receive completion')

            idsp = 0
            DO irank = 0, mpas_size-1
               DO i = 1, pushdata%n_from_other(irank)

                  IF (recvcache(idsp+i) /= fillvalue) THEN
                     i_to = pushdata%other_to(irank)%val(i)
                     IF (vec_recv(i_to) == fillvalue) THEN
                        vec_recv(i_to) = recvcache(idsp+i)
                     ELSE
                        vec_recv(i_to) = vec_recv(i_to) + recvcache(idsp+i)
                     ENDIF
                  ENDIF

               ENDDO
               idsp = idsp + pushdata%n_from_other(irank)
            ENDDO

         ENDIF

         IF (ndatasend > 0) THEN
            CALL mpi_waitall(size(req_send), req_send, MPI_STATUSES_IGNORE, mpas_mpi_ierr)
            CALL mpas_mpi_check('integer compute-push send completion')
         ENDIF

         IF (allocated(req_send )) deallocate(req_send )
         IF (allocated(sendcache)) deallocate(sendcache)
         IF (allocated(req_recv )) deallocate(req_recv )
         IF (allocated(recvcache)) deallocate(recvcache)
#endif

      ENDIF

   END SUBROUTINE compute_push_data_uniq_int32

   ! ----------
   SUBROUTINE compute_push_data_single_real8 (pushdata, vec_send, vec_recv, fillvalue)

   IMPLICIT NONE

   type(compute_pushdata_type) :: pushdata

   real(r8), intent(in)    :: vec_send (:)
   real(r8), intent(inout) :: vec_recv (:)
   real(r8), intent(in)    :: fillvalue

   ! Local Variables
   real(r8), allocatable   :: vec_recv_uniq (:)

      IF (.true.) THEN

         IF (size(vec_recv) > 0) vec_recv = fillvalue
         IF (allocated(pushdata%addr_single)) THEN
            IF (size(vec_recv) < size(pushdata%addr_single)) THEN
               CALL CoLM_stop('Receive array is too small for single CoLM compute push data.')
            ENDIF
         ELSEIF (pushdata%num_req_uniq > 0) THEN
            CALL CoLM_stop('Single CoLM compute push data has no request-address map.')
         ENDIF

         ! Always allocate (zero-length if no requests) to avoid passing
         ! unallocated array to compute_push_data_uniq_real8.
         allocate (vec_recv_uniq (pushdata%num_req_uniq))
         IF (pushdata%num_req_uniq > 0) THEN
            vec_recv_uniq(:) = fillvalue
         ENDIF

         CALL compute_push_data_uniq_real8 (pushdata, vec_send, vec_recv_uniq, fillvalue)

         IF (pushdata%num_req_uniq > 0 .and. allocated(pushdata%addr_single)) THEN
            vec_recv(1:size(pushdata%addr_single)) = vec_recv_uniq(pushdata%addr_single)
         ENDIF
         deallocate (vec_recv_uniq)

      ENDIF

   END SUBROUTINE compute_push_data_single_real8

   ! ----------
   SUBROUTINE compute_push_data_multi_real8 (pushdata, vec_send, vec_recv, fillvalue, mode)

   IMPLICIT NONE

   type(compute_pushdata_type) :: pushdata

   real(r8), intent(in)    :: vec_send (:)
   real(r8), intent(inout) :: vec_recv (:)
   real(r8), intent(in)    :: fillvalue

   character(len=*), intent(in) :: mode

   ! Local Variables
   real(r8), allocatable :: vec_recv_uniq (:)
   integer  :: i, j
   real(r8) :: val, sumarea

      IF (.true.) THEN

         IF (trim(mode) /= 'sum' .and. trim(mode) /= 'average') THEN
            CALL CoLM_stop('CoLM multi compute push mode must be sum or average.')
         ENDIF
         IF (size(vec_recv) > 0) vec_recv = fillvalue
         IF (allocated(pushdata%addr_multi)) THEN
            IF (size(vec_recv) < size(pushdata%addr_multi, 2)) THEN
               CALL CoLM_stop('Receive array is too small for multi CoLM compute push data.')
            ENDIF
         ELSEIF (pushdata%num_req_uniq > 0) THEN
            CALL CoLM_stop('Multi CoLM compute push data has no request-address map.')
         ENDIF

         ! Always allocate (zero-length if no requests) to avoid passing
         ! unallocated array to compute_push_data_uniq_real8.
         allocate (vec_recv_uniq (pushdata%num_req_uniq))
         IF (pushdata%num_req_uniq > 0) THEN
            vec_recv_uniq(:) = fillvalue
         ENDIF

         CALL compute_push_data_uniq_real8 (pushdata, vec_send, vec_recv_uniq, fillvalue)

         IF (pushdata%num_req_uniq > 0) THEN

            DO j = 1, size(pushdata%addr_multi,2)

               sumarea = 0.

               DO i = 1, size(pushdata%addr_multi,1)
                  val = vec_recv_uniq(pushdata%addr_multi(i,j))
                  IF (val /= fillvalue) THEN
                     IF (vec_recv(j) == fillvalue) THEN
                        vec_recv(j) = val * pushdata%area_multi(i,j)
                     ELSE
                        vec_recv(j) = vec_recv(j) + val * pushdata%area_multi(i,j)
                     ENDIF
                     sumarea = sumarea + pushdata%area_multi(i,j)
                  ENDIF
               ENDDO

               IF (trim(mode) == 'average') THEN
                  IF (vec_recv(j) /= fillvalue) THEN
                     IF (sumarea > 0._r8) THEN
                        vec_recv(j) = vec_recv(j) / sumarea
                     ELSE
                        vec_recv(j) = fillvalue
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO

         ENDIF
         deallocate (vec_recv_uniq)

      ENDIF

   END SUBROUTINE compute_push_data_multi_real8

   ! ----------
   SUBROUTINE compute_push_data_single_int32 (pushdata, vec_send, vec_recv, fillvalue)

   IMPLICIT NONE

   type(compute_pushdata_type) :: pushdata

   integer, intent(in)    :: vec_send (:)
   integer, intent(inout) :: vec_recv (:)
   integer, intent(in)    :: fillvalue

   ! Local Variables
   integer, allocatable   :: vec_recv_uniq (:)

      IF (.true.) THEN

         IF (size(vec_recv) > 0) vec_recv = fillvalue
         IF (allocated(pushdata%addr_single)) THEN
            IF (size(vec_recv) < size(pushdata%addr_single)) THEN
               CALL CoLM_stop('Receive array is too small for integer CoLM compute push data.')
            ENDIF
         ELSEIF (pushdata%num_req_uniq > 0) THEN
            CALL CoLM_stop('Integer CoLM compute push data has no request-address map.')
         ENDIF

         allocate (vec_recv_uniq (pushdata%num_req_uniq))
         IF (pushdata%num_req_uniq > 0) THEN
            vec_recv_uniq(:) = fillvalue
         ENDIF

         CALL compute_push_data_uniq_int32 (pushdata, vec_send, vec_recv_uniq, fillvalue)

         IF (pushdata%num_req_uniq > 0 .and. allocated(pushdata%addr_single)) THEN
            vec_recv(1:size(pushdata%addr_single)) = vec_recv_uniq(pushdata%addr_single)
         ENDIF
         deallocate (vec_recv_uniq)

      ENDIF

   END SUBROUTINE compute_push_data_single_int32

   ! ---------
   SUBROUTINE compute_remap_data_pset2grid_real8 (remapdata, vec_in, vec_out, fillvalue, filter)

   IMPLICIT NONE

   type(compute_remapdata_type), intent(in) :: remapdata

   real(r8), intent(in)    :: vec_in (:)
   real(r8), intent(inout) :: vec_out(:)
   real(r8), intent(in)    :: fillvalue
   logical,  intent(in)    :: filter (:)

   ! Local Variables
   integer  :: iset, ipart, iloc
   real(r8) :: area


      IF (.true.) THEN
         IF (size(vec_in) < remapdata%npset .or. size(filter) < remapdata%npset) THEN
            CALL CoLM_stop('Pixel-set input is too small for CoLM compute remap data.')
         ENDIF
         IF (size(vec_out) < remapdata%num_grid) THEN
            CALL CoLM_stop('Grid output is too small for CoLM compute remap data.')
         ENDIF
         IF (remapdata%num_grid > 0) THEN

            vec_out(:) = fillvalue

            DO iset = 1, remapdata%npset
               IF (filter(iset) .and. (vec_in(iset) /= fillvalue)) THEN
                  DO ipart = 1, remapdata%npart(iset)

                     iloc = remapdata%part_to(iset)%val(ipart)
                     area = remapdata%areapart(iset)%val(ipart)
                     IF (iloc < 1 .or. iloc > remapdata%num_grid) THEN
                        CALL CoLM_stop('Pixel-set to grid remap contains an invalid target address.')
                     ENDIF

                     IF (vec_out(iloc) == fillvalue) THEN
                        vec_out(iloc) = vec_in(iset) * area
                     ELSE
                        vec_out(iloc) = vec_out(iloc) + vec_in(iset) * area
                     ENDIF

                  ENDDO
               ENDIF
            ENDDO

         ENDIF
      ENDIF

   END SUBROUTINE compute_remap_data_pset2grid_real8

   ! ---------
   SUBROUTINE compute_remap_data_grid2pset_real8 (remapdata, vec_in, vec_out, fillvalue, mode)

   IMPLICIT NONE

   type(compute_remapdata_type), intent(in) :: remapdata

   real(r8), intent(in)    :: vec_in (:)
   real(r8), intent(inout) :: vec_out(:)
   real(r8), intent(in)    :: fillvalue

   character(len=*), intent(in) :: mode

   ! Local Variables
   integer  :: iset, ipart, iloc
   real(r8) :: area, sumarea

      IF (.true.) THEN
         IF (trim(mode) /= 'sum' .and. trim(mode) /= 'average') THEN
            CALL CoLM_stop('CoLM compute remap mode must be sum or average.')
         ENDIF
         IF (size(vec_in) < remapdata%num_grid) THEN
            CALL CoLM_stop('Grid input is too small for CoLM compute remap data.')
         ENDIF
         IF (size(vec_out) < remapdata%npset) THEN
            CALL CoLM_stop('Pixel-set output is too small for CoLM compute remap data.')
         ENDIF
         IF (remapdata%npset > 0) THEN

            vec_out(:) = fillvalue

            DO iset = 1, remapdata%npset

               sumarea = 0.

               DO ipart = 1, remapdata%npart(iset)
                  iloc = remapdata%part_to(iset)%val(ipart)
                  area = remapdata%areapart(iset)%val(ipart)
                  IF (iloc < 1 .or. iloc > remapdata%num_grid) THEN
                     CALL CoLM_stop('Grid to pixel-set remap contains an invalid source address.')
                  ENDIF

                  IF (vec_in(iloc) /= fillvalue) THEN
                     IF (vec_out(iset) == fillvalue) THEN
                        vec_out(iset) = vec_in(iloc) * area
                     ELSE
                        vec_out(iset) = vec_out(iset) + vec_in(iloc) * area
                     ENDIF
                     sumarea = sumarea + area
                  ENDIF
               ENDDO

               IF (trim(mode) == 'average') THEN
                  IF (vec_out(iset) /= fillvalue) THEN
                     IF (sumarea > 0._r8 .and. ieee_is_finite(sumarea)) THEN
                        vec_out(iset) = vec_out(iset) / sumarea
                     ELSE
                        vec_out(iset) = fillvalue
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO

         ENDIF
      ENDIF

   END SUBROUTINE compute_remap_data_grid2pset_real8

   ! ---------
   SUBROUTINE compute_pushdata_free_mem (this)

   IMPLICIT NONE
   type(compute_pushdata_type) :: this

      IF (allocated(this%addr_single )) deallocate(this%addr_single )
      IF (allocated(this%addr_multi  )) deallocate(this%addr_multi  )
      IF (allocated(this%area_multi  )) deallocate(this%area_multi  )
      IF (allocated(this%sum_area    )) deallocate(this%sum_area    )
      IF (allocated(this%self_from   )) deallocate(this%self_from   )
      IF (allocated(this%self_to     )) deallocate(this%self_to     )
#ifdef MPAS_MPI
      IF (allocated(this%n_to_other  )) deallocate(this%n_to_other  )
      IF (allocated(this%n_from_other)) deallocate(this%n_from_other)
      IF (allocated(this%to_other    )) deallocate(this%to_other    )
      IF (allocated(this%other_to    )) deallocate(this%other_to    )
#endif

   END SUBROUTINE compute_pushdata_free_mem

   ! ---------
   SUBROUTINE compute_remapdata_free_mem (this)

   IMPLICIT NONE
   type(compute_remapdata_type) :: this

      IF (allocated(this%ilon_me )) deallocate(this%ilon_me )
      IF (allocated(this%ilat_me )) deallocate(this%ilat_me )
      IF (allocated(this%ids_me  )) deallocate(this%ids_me  )
      IF (allocated(this%npart   )) deallocate(this%npart   )
      IF (allocated(this%sum_area)) deallocate(this%sum_area)
      IF (allocated(this%part_to )) deallocate(this%part_to )
      IF (allocated(this%areapart)) deallocate(this%areapart)

   END SUBROUTINE compute_remapdata_free_mem

END MODULE MOD_ComputePushData
