#include <define.h>

#ifdef GridRiverLakeFlow
MODULE MOD_Grid_RiverLakeNetwork
!--------------------------------------------------------------------------------
! DESCRIPTION:
!--------------------------------------------------------------------------------

   USE MOD_Grid
   USE MOD_ComputePushData
   IMPLICIT NONE

   ! ----- River Lake network -----

   type(grid_type) :: griducat

   integer :: totalnumucat = 0
   integer :: numucat = 0
   integer, allocatable :: ucat_ucid (:)   ! index in unit catchment numbering

   ! ----- Part 1: between runoff input elements and unit catchments -----
   integer :: numinpm = 0
   integer,  allocatable :: inpm_gdid (:)

   integer :: inpn = 0
   integer,  allocatable :: idmap_gd2uc (:,:)
   real(r8), allocatable :: area_gd2uc  (:,:)

   integer :: nucpart = 0
   integer,  allocatable :: idmap_uc2gd (:,:)
   real(r8), allocatable :: area_uc2gd  (:,:)

   type(compute_remapdata_type) :: remap_patch2inpm
   type(compute_pushdata_type)  :: push_inpm2ucat
   type(compute_pushdata_type)  :: push_ucat2inpm

   ! ----- Part 2: between upstream and downstream unit catchments -----
   integer,  allocatable :: ucat_next (:)  ! next unit catchment
   integer :: upnmax = 0
   integer,  allocatable :: ucat_ups (:,:) ! upstream unit catchments
   real(r8), allocatable :: wts_ups  (:,:)

   type(compute_pushdata_type) :: push_next2ucat
   type(compute_pushdata_type) :: push_ups2ucat

   ! ----- Part 3: river systems -----
   integer :: numrivsys = 0
   integer, allocatable :: irivsys (:)
#ifdef MPAS_MPI
   integer :: num_owned_rivsys = 0
   integer, allocatable :: rivsys_send_counts (:)
   integer, allocatable :: rivsys_send_displs (:)
   integer, allocatable :: rivsys_recv_counts (:)
   integer, allocatable :: rivsys_recv_displs (:)
   integer, allocatable :: rivsys_send_local  (:)
   integer, allocatable :: rivsys_recv_owner  (:)
#endif


   ! ----- Parameters for River and Lake -----

   integer,  allocatable :: lake_type      (:)   ! 0: river; 2: reservoir.

   real(r8), allocatable :: topo_rivelv    (:)   ! river bed elevation [m]
   real(r8), allocatable :: topo_rivhgt    (:)   ! river channel depth [m]
   real(r8), allocatable :: topo_rivlen    (:)   ! river channel length [m]
   real(r8), allocatable :: topo_rivman    (:)   ! river manning coefficient [m]
   real(r8), allocatable :: topo_rivwth    (:)   ! river channel width [m]
   real(r8), allocatable :: topo_rivare    (:)   ! river channel area [m^2]
   real(r8), allocatable :: topo_rivstomax (:)   ! max river channel storage [m^3]

   real(r8), allocatable :: topo_area      (:)   ! floodplain area [m^2]
   real(r8), allocatable :: topo_fldhgt    (:,:) ! floodplain height profile [m]

   real(r8), allocatable :: bedelv_next    (:)   ! downstream river bed elevation [m]
   real(r8), allocatable :: outletwth      (:)   ! river outlet width [m]

   type :: vol_dep_curve_type
      integer  :: nlfp
      real(r8) :: rivhgt
      real(r8) :: rivare
      real(r8) :: rivstomax
      real(r8), allocatable :: flphgt    (:) ! floodplain height profile [m]
      real(r8), allocatable :: flparea   (:) ! flood plain area [m^2]
      real(r8), allocatable :: flpaccare (:) ! flood plain accumulated area [m^2]
      real(r8), allocatable :: flpstomax (:) ! max flood plain storage [m^3]
   CONTAINS
      procedure, PUBLIC :: depth     => retrieve_depth_from_volume
      procedure, PUBLIC :: volume    => retrieve_volume_from_depth
      procedure, PUBLIC :: floodarea => retrieve_area_from_depth
      final :: vol_depth_curve_free_mem
   END type vol_dep_curve_type

   type(vol_dep_curve_type), allocatable :: floodplain_curve (:)


CONTAINS

   ! ----------
   SUBROUTINE build_riverlake_network ()

   USE MOD_MPAS_MPI, only: mpas_comm, mpas_mpi_ierr, mpas_mpi_check
   IMPLICIT NONE

      CALL mpi_barrier (mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('river-network construction entry')
      CALL build_riverlake_network_mpas_embedded()

   END SUBROUTINE build_riverlake_network

#ifdef MPAS_EMBEDDED_COLM
   ! ---------
   SUBROUTINE build_riverlake_network_mpas_embedded ()

   USE MOD_MPAS_MPI
   USE MOD_Namelist
   USE MOD_NetCDFSerial
   USE MOD_Mesh
   USE MOD_LandPatch
   USE MOD_Vars_Global, only: spval
   IMPLICIT NONE

   character(len=256) :: parafile
   integer, allocatable :: varsize(:)
   integer, allocatable :: idmap_x(:,:), idmap_y(:,:)
   integer :: nlat_ucat, nlon_ucat
   integer :: i, j

      parafile = DEF_UnitCatchment_file

      CALL ncio_inquire_length (parafile, 'seq_next', totalnumucat)
      CALL ncio_inquire_length (parafile, 'lon', nlon_ucat)
      CALL ncio_inquire_length (parafile, 'lat', nlat_ucat)

      CALL griducat%define_by_ndims (nlon_ucat, nlat_ucat)
      CALL build_compute_remapdata (landpatch, griducat, remap_patch2inpm)

      IF (.true.) THEN
         numinpm = remap_patch2inpm%num_grid
         IF (numinpm > 0) THEN
            allocate (inpm_gdid (numinpm))
            inpm_gdid = remap_patch2inpm%ids_me
         ELSE
            allocate (inpm_gdid (0))
         ENDIF
      ENDIF

      CALL build_mpas_embedded_local_ucats (parafile, nlon_ucat, nlat_ucat, numinpm, inpm_gdid)

      CALL ncio_inquire_varsize (parafile, 'inpmat_x', varsize)
      IF (size(varsize) /= 2 .or. varsize(1) < 1 .or. varsize(2) /= totalnumucat) THEN
         CALL CoLM_Stop ('ERROR: invalid inpmat_x dimensions in the embedded CoLM unit-catchment file.')
      ENDIF
      inpn = varsize(1)
      deallocate (varsize)

      IF (.true.) THEN
         IF (numucat > 0) THEN
            CALL ncio_read_indexed_serial (parafile, 'inpmat_x',    ucat_ucid, idmap_x)
            CALL ncio_read_indexed_serial (parafile, 'inpmat_y',    ucat_ucid, idmap_y)
            CALL ncio_read_indexed_serial (parafile, 'inpmat_area', ucat_ucid, area_gd2uc)
            CALL validate_mpas_embedded_inpmat (idmap_x, idmap_y, area_gd2uc, inpn, nlon_ucat, nlat_ucat)

            allocate (idmap_gd2uc (inpn,numucat))
            idmap_gd2uc = (idmap_y-1)*nlon_ucat + idmap_x

            WHERE ((area_gd2uc <= 0._r8) .or. (idmap_gd2uc <= 0))
               idmap_gd2uc = 0
               area_gd2uc  = 0._r8
            END WHERE

            deallocate (idmap_x)
            deallocate (idmap_y)
         ELSE
            allocate (idmap_gd2uc (inpn,0))
            allocate (area_gd2uc  (inpn,0))
         ENDIF

         CALL build_mpas_embedded_uc2gd (parafile, nlon_ucat, nlat_ucat, inpn, numinpm, inpm_gdid, &
            nucpart, idmap_uc2gd, area_uc2gd)
      ENDIF

      IF (.true.) THEN
         CALL build_compute_pushdata (numinpm, inpm_gdid, numucat, idmap_gd2uc, area_gd2uc, push_inpm2ucat)
         CALL build_compute_pushdata (numucat, ucat_ucid, numinpm, idmap_uc2gd, area_uc2gd, push_ucat2inpm)
      ENDIF

      CALL build_mpas_embedded_local_topology (parafile)

	      IF (.true.) THEN
	         allocate (wts_ups (upnmax,numucat))
	         IF (numucat > 0) wts_ups(:,:) = 1._r8

	         CALL build_compute_pushdata (numucat, ucat_ucid, numucat, ucat_next, push_next2ucat)
	         CALL build_compute_pushdata (numucat, ucat_ucid, numucat, ucat_ups,  wts_ups, push_ups2ucat )
	         CALL check_mpas_embedded_topology_ownership ()
	      ENDIF

      CALL build_mpas_embedded_river_systems (parafile)

      CALL readin_riverlake_parameter (parafile, 'topo_rivelv',    rdata1d = topo_rivelv   )
      CALL readin_riverlake_parameter (parafile, 'topo_rivhgt',    rdata1d = topo_rivhgt   )
      CALL readin_riverlake_parameter (parafile, 'topo_rivlen',    rdata1d = topo_rivlen   )
      CALL readin_riverlake_parameter (parafile, 'topo_rivman',    rdata1d = topo_rivman   )
      CALL readin_riverlake_parameter (parafile, 'topo_rivwth',    rdata1d = topo_rivwth   )
      CALL readin_riverlake_parameter (parafile, 'topo_rivstomax', rdata1d = topo_rivstomax)
      CALL readin_riverlake_parameter (parafile, 'topo_area',      rdata1d = topo_area     )
      CALL readin_riverlake_parameter (parafile, 'topo_fldhgt',    rdata2d = topo_fldhgt   )
      CALL validate_mpas_embedded_river_parameters ()

      IF (.true.) THEN
         IF (numucat > 0) THEN
            allocate (lake_type (numucat))
            lake_type(:) = 0

            allocate (topo_rivare (numucat))
            topo_rivare = topo_rivstomax / topo_rivhgt

            allocate (floodplain_curve (numucat))
            DO i = 1, numucat
               floodplain_curve(i)%nlfp      = size(topo_fldhgt,1)
               floodplain_curve(i)%rivhgt    = topo_rivhgt(i)
               floodplain_curve(i)%rivstomax = topo_rivstomax(i)
               floodplain_curve(i)%rivare    = topo_rivare(i)

               allocate (floodplain_curve(i)%flphgt    (0:floodplain_curve(i)%nlfp))
               allocate (floodplain_curve(i)%flparea   (0:floodplain_curve(i)%nlfp))
               allocate (floodplain_curve(i)%flpaccare (0:floodplain_curve(i)%nlfp))
               allocate (floodplain_curve(i)%flpstomax (0:floodplain_curve(i)%nlfp))

               floodplain_curve(i)%flphgt(0)  = 0._r8
               floodplain_curve(i)%flphgt(1:) = topo_fldhgt(:,i)

               floodplain_curve(i)%flparea(0)  = 0._r8
               floodplain_curve(i)%flparea(1:) = topo_area(i) / real(floodplain_curve(i)%nlfp, r8)

               floodplain_curve(i)%flpaccare(0) = 0._r8
               DO j = 1, floodplain_curve(i)%nlfp
                  floodplain_curve(i)%flpaccare(j) = &
                     floodplain_curve(i)%flpaccare(j-1) + floodplain_curve(i)%flparea(j)
               ENDDO

               floodplain_curve(i)%flpstomax(0) = 0._r8
               DO j = 1, floodplain_curve(i)%nlfp
                  floodplain_curve(i)%flpstomax(j) = floodplain_curve(i)%flpstomax(j-1)        &
                     + 0.5_r8 * (floodplain_curve(i)%flparea(j) + floodplain_curve(i)%flparea(j-1)) &
                           * (floodplain_curve(i)%flphgt(j)  - floodplain_curve(i)%flphgt(j-1))
               ENDDO
            ENDDO

            allocate (bedelv_next (numucat))
            allocate (outletwth   (numucat))
         ELSE
            allocate (bedelv_next (0))
            allocate (outletwth   (0))
         ENDIF
      ENDIF

      CALL compute_push_data (push_next2ucat, topo_rivelv, bedelv_next, fillvalue = spval)
      CALL compute_push_data (push_next2ucat, topo_rivwth, outletwth  , fillvalue = spval)

      IF (.true.) THEN
         IF (numucat > 0) THEN
            WHERE (ucat_next > 0)
               outletwth = (outletwth + topo_rivwth) * 0.5_r8
            ELSEWHERE
               outletwth = topo_rivwth
            END WHERE
         ENDIF

      ENDIF

   END SUBROUTINE build_riverlake_network_mpas_embedded

   ! ---------
   SUBROUTINE build_mpas_embedded_river_systems (parafile)

   USE MOD_MPAS_MPI
   USE MOD_NetCDFSerial
   USE MOD_Utils
   IMPLICIT NONE

   character(len=*), intent(in) :: parafile

   integer, allocatable :: mouth_id(:), next_id(:)
   integer, allocatable :: request(:), request_order(:), request_next(:)
   integer, allocatable :: local_mouths(:)
   integer :: i, iloc, nactive, niter, nlocal_mouths
   logical :: is_new

      IF (.not. .true.) RETURN

      allocate (irivsys (numucat))
      allocate (mouth_id (numucat))
      allocate (local_mouths (max(1,numucat)))
      mouth_id(:) = 0

      IF (numucat > 0) THEN
         allocate (next_id (numucat))
         next_id = ucat_next

         DO i = 1, numucat
            IF (next_id(i) <= 0) mouth_id(i) = ucat_ucid(i)
         ENDDO

         nactive = count(next_id > 0)
         niter = 0
         DO WHILE (nactive > 0)
            niter = niter + 1
            IF (niter > totalnumucat) THEN
               CALL CoLM_Stop ('ERROR: MPAS embedded CoLM river network has a downstream cycle.')
            ENDIF

            allocate (request (nactive))
            allocate (request_order (nactive))
            iloc = 0
            DO i = 1, numucat
               IF (next_id(i) > 0) THEN
                  iloc = iloc + 1
                  request(iloc) = next_id(i)
                  request_order(iloc) = i
               ENDIF
            ENDDO

            CALL quicksort (nactive, request, request_order)
            CALL ncio_read_indexed_serial (parafile, 'seq_next', request, request_next)

            nactive = 0
            DO iloc = 1, size(request)
               i = request_order(iloc)
               IF (request_next(iloc) > 0) THEN
                  next_id(i) = request_next(iloc)
                  nactive = nactive + 1
               ELSE
                  mouth_id(i) = request(iloc)
                  next_id(i) = 0
               ENDIF
            ENDDO

            deallocate (request)
            deallocate (request_order)
            deallocate (request_next)
         ENDDO

         deallocate (next_id)
      ENDIF

      nlocal_mouths = 0
      DO i = 1, numucat
         CALL insert_into_sorted_list1 (mouth_id(i), nlocal_mouths, local_mouths, iloc, is_new)
      ENDDO

      numrivsys = nlocal_mouths
#ifdef MPAS_MPI
      CALL build_river_system_min_exchange(local_mouths(1:numrivsys))
#endif

      DO i = 1, numucat
         irivsys(i) = find_in_sorted_list1 (mouth_id(i), numrivsys, local_mouths(1:numrivsys))
         IF (irivsys(i) <= 0) CALL CoLM_Stop ('ERROR: MPAS embedded CoLM river-system map is incomplete.')
      ENDDO

      deallocate (mouth_id)
      deallocate (local_mouths)

   END SUBROUTINE build_mpas_embedded_river_systems

#ifdef MPAS_MPI
   ! ---------
   SUBROUTINE build_river_system_min_exchange(local_system_id)

   USE MOD_MPAS_MPI
   USE MOD_Utils, only: quicksort
   IMPLICIT NONE

   integer, intent(in) :: local_system_id(:)

   integer, allocatable :: next_position(:)
   integer, allocatable :: recv_system_id(:)
   integer, allocatable :: send_system_id(:)
   integer, allocatable :: sorted_order(:)
   integer, allocatable :: sorted_system_id(:)
   integer :: i
   integer :: irank
   integer :: nrecv
   integer :: owner
   integer :: position

      IF (size(local_system_id) /= numrivsys) THEN
         CALL CoLM_Stop('ERROR: local embedded CoLM river-system count is inconsistent.')
      ENDIF
      IF (numrivsys > 0) THEN
         IF (any(local_system_id <= 0)) THEN
            CALL CoLM_Stop('ERROR: embedded CoLM river-system IDs must be positive.')
         ENDIF
         IF (numrivsys > 1) THEN
            IF (any(local_system_id(2:numrivsys) <= local_system_id(1:numrivsys-1))) THEN
               CALL CoLM_Stop('ERROR: local embedded CoLM river-system IDs must be sorted and unique.')
            ENDIF
         ENDIF
      ENDIF
      IF (allocated(rivsys_send_counts) .or. allocated(rivsys_send_displs) .or. &
          allocated(rivsys_recv_counts) .or. allocated(rivsys_recv_displs) .or. &
          allocated(rivsys_send_local) .or. allocated(rivsys_recv_owner)) THEN
         CALL CoLM_Stop('ERROR: embedded CoLM river-system exchange was initialized more than once.')
      ENDIF

      allocate(rivsys_send_counts(0:mpas_size-1), rivsys_send_displs(0:mpas_size-1))
      allocate(rivsys_recv_counts(0:mpas_size-1), rivsys_recv_displs(0:mpas_size-1))
      rivsys_send_counts = 0
      DO i = 1, numrivsys
         owner = modulo(local_system_id(i) - 1, mpas_size)
         rivsys_send_counts(owner) = rivsys_send_counts(owner) + 1
      ENDDO

      rivsys_send_displs(0) = 0
      DO irank = 1, mpas_size-1
         rivsys_send_displs(irank) = rivsys_send_displs(irank-1) + rivsys_send_counts(irank-1)
      ENDDO

      allocate(next_position(0:mpas_size-1))
      next_position = rivsys_send_displs
      allocate(rivsys_send_local(numrivsys))
      allocate(send_system_id(max(1,numrivsys)))
      DO i = 1, numrivsys
         owner = modulo(local_system_id(i) - 1, mpas_size)
         position = next_position(owner) + 1
         next_position(owner) = position
         rivsys_send_local(position) = i
         send_system_id(position) = local_system_id(i)
      ENDDO

      CALL mpi_alltoall(rivsys_send_counts, 1, MPI_INTEGER, rivsys_recv_counts, 1, MPI_INTEGER, &
                        mpas_comm, mpas_mpi_ierr)
      IF (mpas_mpi_ierr /= MPI_SUCCESS) CALL CoLM_Stop('ERROR: embedded CoLM river-system count exchange failed.')

      rivsys_recv_displs(0) = 0
      DO irank = 1, mpas_size-1
         rivsys_recv_displs(irank) = rivsys_recv_displs(irank-1) + rivsys_recv_counts(irank-1)
      ENDDO
      nrecv = sum(rivsys_recv_counts)
      allocate(recv_system_id(max(1,nrecv)))

      CALL mpi_alltoallv(send_system_id, rivsys_send_counts, rivsys_send_displs, MPI_INTEGER, &
                         recv_system_id, rivsys_recv_counts, rivsys_recv_displs, MPI_INTEGER, &
                         mpas_comm, mpas_mpi_ierr)
      IF (mpas_mpi_ierr /= MPI_SUCCESS) CALL CoLM_Stop('ERROR: embedded CoLM river-system ID exchange failed.')

      allocate(rivsys_recv_owner(nrecv))
      num_owned_rivsys = 0
      IF (nrecv > 0) THEN
         IF (any(recv_system_id(1:nrecv) <= 0)) THEN
            CALL CoLM_Stop('ERROR: embedded CoLM received an invalid river-system ID.')
         ENDIF
         DO i = 1, nrecv
            IF (modulo(recv_system_id(i) - 1, mpas_size) /= mpas_rank) THEN
               CALL CoLM_Stop('ERROR: embedded CoLM river-system request reached the wrong owner rank.')
            ENDIF
         ENDDO

         allocate(sorted_system_id(nrecv), sorted_order(nrecv))
         sorted_system_id = recv_system_id(1:nrecv)
         sorted_order = (/(i, i = 1, nrecv)/)
         IF (nrecv > 1) CALL quicksort(nrecv, sorted_system_id, sorted_order)

         DO i = 1, nrecv
            IF (i == 1) THEN
               num_owned_rivsys = num_owned_rivsys + 1
            ELSEIF (sorted_system_id(i) /= sorted_system_id(i-1)) THEN
               num_owned_rivsys = num_owned_rivsys + 1
            ENDIF
            rivsys_recv_owner(sorted_order(i)) = num_owned_rivsys
         ENDDO
         deallocate(sorted_system_id, sorted_order)
      ENDIF

      deallocate(next_position, recv_system_id, send_system_id)

   END SUBROUTINE build_river_system_min_exchange

   ! ---------
   SUBROUTINE synchronize_river_system_min(system_value)

   USE MOD_MPAS_MPI
   USE, INTRINSIC :: ieee_arithmetic, only: ieee_is_finite
   IMPLICIT NONE

   real(r8), intent(inout) :: system_value(:)

   real(r8), allocatable :: owner_min(:)
   real(r8), allocatable :: recv_value(:)
   real(r8), allocatable :: return_value(:)
   real(r8), allocatable :: send_value(:)
   real(r8), allocatable :: synced_value(:)
   integer :: i
   integer :: nrecv
   integer :: owner_index

      IF (size(system_value) < max(1,numrivsys)) THEN
         CALL CoLM_Stop('ERROR: embedded CoLM river-system timestep array is too small.')
      ENDIF
      IF (.not. allocated(rivsys_send_counts) .or. .not. allocated(rivsys_send_displs) .or. &
          .not. allocated(rivsys_recv_counts) .or. .not. allocated(rivsys_recv_displs) .or. &
          .not. allocated(rivsys_send_local) .or. .not. allocated(rivsys_recv_owner)) THEN
         CALL CoLM_Stop('ERROR: embedded CoLM river-system exchange is not initialized.')
      ENDIF
      IF (numrivsys > 0) THEN
         IF (.not. all(ieee_is_finite(system_value(1:numrivsys))) .or. &
             any(system_value(1:numrivsys) < 0._r8)) THEN
            CALL CoLM_Stop('ERROR: embedded CoLM river-system exchange received an invalid timestep.')
         ENDIF
      ENDIF

      nrecv = sum(rivsys_recv_counts)
      allocate(send_value(max(1,numrivsys)), synced_value(max(1,numrivsys)))
      allocate(recv_value(max(1,nrecv)), return_value(max(1,nrecv)))
      allocate(owner_min(max(1,num_owned_rivsys)))

      DO i = 1, numrivsys
         send_value(i) = system_value(rivsys_send_local(i))
      ENDDO
      CALL mpi_alltoallv(send_value, rivsys_send_counts, rivsys_send_displs, MPI_REAL8, &
                         recv_value, rivsys_recv_counts, rivsys_recv_displs, MPI_REAL8, &
                         mpas_comm, mpas_mpi_ierr)
      IF (mpas_mpi_ierr /= MPI_SUCCESS) CALL CoLM_Stop('ERROR: embedded CoLM river-system timestep exchange failed.')

      owner_min = huge(1._r8)
      DO i = 1, nrecv
         owner_index = rivsys_recv_owner(i)
         IF (owner_index < 1 .or. owner_index > num_owned_rivsys) THEN
            CALL CoLM_Stop('ERROR: embedded CoLM river-system owner address is invalid.')
         ENDIF
         owner_min(owner_index) = min(owner_min(owner_index), recv_value(i))
      ENDDO
      DO i = 1, nrecv
         return_value(i) = owner_min(rivsys_recv_owner(i))
      ENDDO

      CALL mpi_alltoallv(return_value, rivsys_recv_counts, rivsys_recv_displs, MPI_REAL8, &
                         synced_value, rivsys_send_counts, rivsys_send_displs, MPI_REAL8, &
                         mpas_comm, mpas_mpi_ierr)
      IF (mpas_mpi_ierr /= MPI_SUCCESS) CALL CoLM_Stop('ERROR: embedded CoLM river-system timestep return exchange failed.')

      DO i = 1, numrivsys
         system_value(rivsys_send_local(i)) = synced_value(i)
      ENDDO

      deallocate(owner_min, recv_value, return_value, send_value, synced_value)

   END SUBROUTINE synchronize_river_system_min
#endif

   ! ---------
   SUBROUTINE build_mpas_embedded_local_ucats (parafile, nlon_ucat, nlat_ucat, numinpm, inpm_gdid)

   USE MOD_MPAS_MPI
   USE MOD_NetCDFSerial
   USE MOD_Utils
   IMPLICIT NONE

   character(len=*), intent(in) :: parafile
   integer, intent(in) :: nlon_ucat, nlat_ucat
   integer, intent(in) :: numinpm
   integer, intent(in) :: inpm_gdid(:)

   integer, parameter :: ucat_chunk_size = 131072
   integer, allocatable :: inpm_sorted(:), inpm_order(:)
   integer, allocatable :: seq_x_blk(:), seq_y_blk(:)
   integer, allocatable :: local_owner_blk(:), owner_blk(:)
   integer :: istart, iend, iucat, iblk, iloc, grid_id, nfound
   integer :: covered_count, global_owned

      IF (.not. .true.) RETURN

      IF (numinpm > 0) THEN
         allocate (inpm_sorted (numinpm))
         allocate (inpm_order  (numinpm))
         inpm_sorted = inpm_gdid
         inpm_order  = (/(iucat, iucat = 1, numinpm)/)
         CALL quicksort (numinpm, inpm_sorted, inpm_order)
         IF (any(inpm_sorted < 1) .or. any(inpm_sorted > nlon_ucat*nlat_ucat)) THEN
            CALL CoLM_Stop ('ERROR: embedded CoLM runoff input grid contains an out-of-range grid ID.')
         ENDIF
         IF (numinpm > 1) THEN
            IF (any(inpm_sorted(2:) == inpm_sorted(:numinpm-1))) THEN
               CALL CoLM_Stop ('ERROR: embedded CoLM runoff input grid contains duplicate local grid IDs.')
            ENDIF
         ENDIF
      ENDIF

      nfound = 0
      covered_count = 0
      istart = 1
      DO WHILE (istart <= totalnumucat)
         iend = min(istart + ucat_chunk_size - 1, totalnumucat)
         CALL ncio_read_part_serial (parafile, 'seq_x', istart, iend, seq_x_blk)
         CALL ncio_read_part_serial (parafile, 'seq_y', istart, iend, seq_y_blk)
         IF (size(seq_x_blk) /= size(seq_y_blk)) THEN
            CALL CoLM_Stop ('ERROR: seq_x and seq_y lengths differ in the embedded CoLM river network.')
         ENDIF
         IF (any(seq_x_blk < 1) .or. any(seq_x_blk > nlon_ucat) .or. &
             any(seq_y_blk < 1) .or. any(seq_y_blk > nlat_ucat)) THEN
            CALL CoLM_Stop ('ERROR: embedded CoLM unit-catchment coordinates are outside the runoff input grid.')
         ENDIF

         allocate (local_owner_blk (size(seq_x_blk)))
         allocate (owner_blk       (size(seq_x_blk)))
         local_owner_blk(:) = huge(1)

         DO iucat = lbound(seq_x_blk,1), ubound(seq_x_blk,1)
            iblk = iucat - lbound(seq_x_blk,1) + 1
            IF (numinpm > 0 .and. seq_x_blk(iucat) > 0 .and. seq_y_blk(iucat) > 0) THEN
               grid_id = (seq_y_blk(iucat)-1) * nlon_ucat + seq_x_blk(iucat)
               iloc = find_in_sorted_list1 (grid_id, numinpm, inpm_sorted)
               IF (iloc > 0) local_owner_blk(iblk) = mpas_rank
            ENDIF
         ENDDO

#ifdef MPAS_MPI
         CALL mpi_allreduce (local_owner_blk, owner_blk, size(local_owner_blk), MPI_INTEGER, &
            MPI_MIN, mpas_comm, mpas_mpi_ierr)
         CALL mpas_mpi_check('river-network first-pass ownership reduction')
#else
         owner_blk = local_owner_blk
#endif

         covered_count = covered_count + count(owner_blk < huge(1))
         nfound = nfound + count(owner_blk == mpas_rank)

         deallocate (seq_x_blk)
         deallocate (seq_y_blk)
         deallocate (local_owner_blk)
         deallocate (owner_blk)
         istart = iend + 1
      ENDDO

#ifdef MPAS_MPI
      CALL mpi_allreduce (nfound, global_owned, 1, MPI_INTEGER, MPI_SUM, mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('river-network ownership-count reduction')
#else
      global_owned = nfound
#endif

      ! owner_blk is the communicator-wide minimum candidate rank, so every
      ! covered unit catchment is assigned to exactly one rank.  Requiring both
      ! counts to match the file length also rejects disconnected, uncovered
      ! basins instead of silently dropping them from the routed network.
      IF (covered_count /= totalnumucat .or. global_owned /= totalnumucat) THEN
         IF (mpas_is_root) THEN
            write(*,'(A,I0,A,I0,A,I0)') 'ERROR: MPAS embedded CoLM river network contains ', totalnumucat, &
               ' unit catchment(s), covered ', covered_count, ', and assigned ', global_owned
         ENDIF
         CALL CoLM_Stop ('ERROR: MPAS embedded CoLM river network coverage is incomplete or inconsistent.')
      ENDIF

      numucat = nfound
      allocate (ucat_ucid (numucat))

      nfound = 0
      istart = 1
      DO WHILE (istart <= totalnumucat)
         iend = min(istart + ucat_chunk_size - 1, totalnumucat)
         CALL ncio_read_part_serial (parafile, 'seq_x', istart, iend, seq_x_blk)
         CALL ncio_read_part_serial (parafile, 'seq_y', istart, iend, seq_y_blk)
         IF (size(seq_x_blk) /= size(seq_y_blk)) THEN
            CALL CoLM_Stop ('ERROR: seq_x and seq_y lengths differ in the embedded CoLM river network.')
         ENDIF
         IF (any(seq_x_blk < 1) .or. any(seq_x_blk > nlon_ucat) .or. &
             any(seq_y_blk < 1) .or. any(seq_y_blk > nlat_ucat)) THEN
            CALL CoLM_Stop ('ERROR: embedded CoLM unit-catchment coordinates are outside the runoff input grid.')
         ENDIF

         allocate (local_owner_blk (size(seq_x_blk)))
         allocate (owner_blk       (size(seq_x_blk)))
         local_owner_blk(:) = huge(1)

         DO iucat = lbound(seq_x_blk,1), ubound(seq_x_blk,1)
            iblk = iucat - lbound(seq_x_blk,1) + 1
            IF (numinpm > 0 .and. seq_x_blk(iucat) > 0 .and. seq_y_blk(iucat) > 0) THEN
               grid_id = (seq_y_blk(iucat)-1) * nlon_ucat + seq_x_blk(iucat)
               iloc = find_in_sorted_list1 (grid_id, numinpm, inpm_sorted)
               IF (iloc > 0) local_owner_blk(iblk) = mpas_rank
            ENDIF
         ENDDO

#ifdef MPAS_MPI
         CALL mpi_allreduce (local_owner_blk, owner_blk, size(local_owner_blk), MPI_INTEGER, &
            MPI_MIN, mpas_comm, mpas_mpi_ierr)
         CALL mpas_mpi_check('river-network final ownership reduction')
#else
         owner_blk = local_owner_blk
#endif

         DO iucat = lbound(seq_x_blk,1), ubound(seq_x_blk,1)
            iblk = iucat - lbound(seq_x_blk,1) + 1
            IF (owner_blk(iblk) == mpas_rank) THEN
               grid_id = (seq_y_blk(iucat)-1) * nlon_ucat + seq_x_blk(iucat)
               nfound = nfound + 1
               ucat_ucid(nfound) = istart + iucat - lbound(seq_x_blk,1)
            ENDIF
         ENDDO

         deallocate (seq_x_blk)
         deallocate (seq_y_blk)
         deallocate (local_owner_blk)
         deallocate (owner_blk)
         istart = iend + 1
      ENDDO

      IF (allocated(inpm_sorted)) deallocate (inpm_sorted)
      IF (allocated(inpm_order )) deallocate (inpm_order )

   END SUBROUTINE build_mpas_embedded_local_ucats

   SUBROUTINE build_mpas_embedded_local_topology (parafile)

   USE MOD_MPAS_MPI
   USE MOD_NetCDFSerial
   USE MOD_Utils
   IMPLICIT NONE

   character(len=*), intent(in) :: parafile

   integer, parameter :: ucat_chunk_size = 131072
   integer, allocatable :: ucat_sorted(:), ucat_order(:), ups_count(:), ups_fill(:)
   integer, allocatable :: seq_next_blk(:)
   integer :: istart, iend, iucat, iloc, idn, local_upnmax

      IF (.not. .true.) RETURN

      IF (numucat > 0) THEN
         CALL ncio_read_indexed_serial (parafile, 'seq_next', ucat_ucid, ucat_next)

         allocate (ucat_sorted (numucat))
         allocate (ucat_order  (numucat))
         allocate (ups_count   (numucat))

         ucat_sorted = ucat_ucid
         ucat_order  = (/(iucat, iucat = 1, numucat)/)
         CALL quicksort (numucat, ucat_sorted, ucat_order)

         ups_count(:) = 0
         istart = 1
         DO WHILE (istart <= totalnumucat)
            iend = min(istart + ucat_chunk_size - 1, totalnumucat)
            CALL ncio_read_part_serial (parafile, 'seq_next', istart, iend, seq_next_blk)
            CALL validate_mpas_embedded_seq_next (seq_next_blk, istart)

            DO iucat = lbound(seq_next_blk,1), ubound(seq_next_blk,1)
               idn = seq_next_blk(iucat)
               IF (idn > 0) THEN
                  iloc = find_in_sorted_list1 (idn, numucat, ucat_sorted)
                  IF (iloc > 0) ups_count(ucat_order(iloc)) = ups_count(ucat_order(iloc)) + 1
               ENDIF
            ENDDO

            deallocate (seq_next_blk)
            istart = iend + 1
         ENDDO

         local_upnmax = maxval(ups_count)
      ELSE
         allocate (ucat_next (0))
         local_upnmax = 0
      ENDIF

#ifdef MPAS_MPI
      CALL mpi_allreduce (local_upnmax, upnmax, 1, MPI_INTEGER, MPI_MAX, mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('river-network upstream-degree reduction')
#else
      upnmax = local_upnmax
#endif
      upnmax = max(1, upnmax)

      allocate (ucat_ups (upnmax,numucat))
      ucat_ups(:,:) = 0

      IF (numucat > 0) THEN
         allocate (ups_fill (numucat))
         ups_fill(:) = 0

         istart = 1
         DO WHILE (istart <= totalnumucat)
            iend = min(istart + ucat_chunk_size - 1, totalnumucat)
            CALL ncio_read_part_serial (parafile, 'seq_next', istart, iend, seq_next_blk)

            DO iucat = lbound(seq_next_blk,1), ubound(seq_next_blk,1)
               idn = seq_next_blk(iucat)
               IF (idn > 0) THEN
                  iloc = find_in_sorted_list1 (idn, numucat, ucat_sorted)
                  IF (iloc > 0) THEN
                     ups_fill(ucat_order(iloc)) = ups_fill(ucat_order(iloc)) + 1
                     ucat_ups(ups_fill(ucat_order(iloc)), ucat_order(iloc)) = &
                        istart + iucat - lbound(seq_next_blk,1)
                  ENDIF
               ENDIF
            ENDDO

            deallocate (seq_next_blk)
            istart = iend + 1
         ENDDO

         deallocate (ups_fill)
         deallocate (ups_count)
         deallocate (ucat_sorted)
         deallocate (ucat_order)
      ENDIF

   END SUBROUTINE build_mpas_embedded_local_topology

   ! ---------
   SUBROUTINE validate_mpas_embedded_seq_next (seq_next, first_id)

   USE MOD_MPAS_MPI, only: CoLM_Stop
   IMPLICIT NONE

   integer, intent(in) :: seq_next(:)
   integer, intent(in) :: first_id
   integer :: i, source_id

      IF (any(seq_next > totalnumucat)) THEN
         CALL CoLM_Stop ('ERROR: embedded CoLM seq_next contains an out-of-range unit-catchment ID.')
      ENDIF
      IF (any(seq_next <= 0 .and. seq_next /= -9 .and. seq_next /= -10 .and. seq_next /= -99)) THEN
         CALL CoLM_Stop ('ERROR: embedded CoLM seq_next contains an unsupported outlet code.')
      ENDIF
      DO i = 1, size(seq_next)
         source_id = first_id + i - 1
         IF (seq_next(i) == source_id) THEN
            CALL CoLM_Stop ('ERROR: embedded CoLM seq_next contains a self-loop.')
         ENDIF
      ENDDO

   END SUBROUTINE validate_mpas_embedded_seq_next

   ! ---------
	   SUBROUTINE check_mpas_embedded_topology_ownership ()

	   USE MOD_MPAS_MPI
	   IMPLICIT NONE

	   integer, allocatable :: next_owner_count(:)
	   integer, allocatable :: ups_owner_count(:)
	   integer :: i, iup, irank, ireq
	   integer :: local_bad, global_bad
	   integer :: first_bad, global_first_bad

	      IF (.not. .true.) RETURN

	      local_bad = 0
	      first_bad = huge(1)

	      allocate (next_owner_count(push_next2ucat%num_req_uniq))
	      next_owner_count(:) = 0
	      DO i = 1, push_next2ucat%nself
	         ireq = push_next2ucat%self_to(i)
	         next_owner_count(ireq) = next_owner_count(ireq) + 1
	      ENDDO
#ifdef MPAS_MPI
	      DO irank = 0, mpas_size-1
	         DO i = 1, push_next2ucat%n_from_other(irank)
	            ireq = push_next2ucat%other_to(irank)%val(i)
	            next_owner_count(ireq) = next_owner_count(ireq) + 1
	         ENDDO
	      ENDDO
#endif

	      allocate (ups_owner_count(push_ups2ucat%num_req_uniq))
	      ups_owner_count(:) = 0
	      DO i = 1, push_ups2ucat%nself
	         ireq = push_ups2ucat%self_to(i)
	         ups_owner_count(ireq) = ups_owner_count(ireq) + 1
	      ENDDO
#ifdef MPAS_MPI
	      DO irank = 0, mpas_size-1
	         DO i = 1, push_ups2ucat%n_from_other(irank)
	            ireq = push_ups2ucat%other_to(irank)%val(i)
	            ups_owner_count(ireq) = ups_owner_count(ireq) + 1
	         ENDDO
	      ENDDO
#endif

	      DO i = 1, numucat
	         IF (ucat_next(i) > 0) THEN
	            ireq = push_next2ucat%addr_single(i)
	            IF (ireq <= 0 .or. ireq > size(next_owner_count)) THEN
	               local_bad = local_bad + 1
	               first_bad = min(first_bad, ucat_next(i))
	            ELSEIF (next_owner_count(ireq) /= 1) THEN
	               local_bad = local_bad + 1
	               first_bad = min(first_bad, ucat_next(i))
	            ENDIF
	         ENDIF

	         DO iup = 1, upnmax
	            IF (ucat_ups(iup,i) > 0) THEN
	               ireq = push_ups2ucat%addr_multi(iup,i)
	               IF (ireq <= 0 .or. ireq > size(ups_owner_count)) THEN
	                  local_bad = local_bad + 1
	                  first_bad = min(first_bad, ucat_ups(iup,i))
	               ELSEIF (ups_owner_count(ireq) /= 1) THEN
	                  local_bad = local_bad + 1
	                  first_bad = min(first_bad, ucat_ups(iup,i))
	               ENDIF
	            ENDIF
	         ENDDO
	      ENDDO

	      deallocate (next_owner_count)
	      deallocate (ups_owner_count)

#ifdef MPAS_MPI
	      CALL mpi_allreduce (local_bad, global_bad, 1, MPI_INTEGER, MPI_SUM, mpas_comm, mpas_mpi_ierr)
	      CALL mpas_mpi_check('river-network invalid-owner count reduction')
	      CALL mpi_allreduce (first_bad, global_first_bad, 1, MPI_INTEGER, MPI_MIN, mpas_comm, mpas_mpi_ierr)
	      CALL mpas_mpi_check('river-network first invalid-owner reduction')
#else
	      global_bad = local_bad
	      global_first_bad = first_bad
#endif

	      IF (global_bad > 0) THEN
	         IF (mpas_is_root) THEN
	            write(*,'(A,I0,A,I0)') 'ERROR: MPAS embedded CoLM GridRiverLakeFlow has ', &
	               global_bad, ' non-unique or missing topology owner reference(s); first unit-catchment ID = ', &
	               global_first_bad
	         ENDIF
	         CALL CoLM_Stop ('ERROR: MPAS domain/landdata must contain exactly one owner for every upstream and downstream unit catchment.')
	      ENDIF

	   END SUBROUTINE check_mpas_embedded_topology_ownership

	   ! ---------
	   SUBROUTINE validate_mpas_embedded_inpmat (idmap_x, idmap_y, area, inpn, nlon_ucat, nlat_ucat)

	   USE, INTRINSIC :: ieee_arithmetic, only: ieee_is_finite
	   USE MOD_MPAS_MPI, only: CoLM_Stop
	   IMPLICIT NONE

	   integer, intent(in) :: idmap_x(:,:), idmap_y(:,:)
	   real(r8), intent(in) :: area(:,:)
	   integer, intent(in) :: inpn, nlon_ucat, nlat_ucat
	   integer :: first_map
	   integer :: grid_id
	   integer :: imap
	   integer :: imap_other
	   integer :: iucat

	      IF (size(idmap_x,1) /= inpn .or. size(idmap_y,1) /= inpn .or. size(area,1) /= inpn .or. &
	          size(idmap_x,2) /= size(idmap_y,2) .or. size(idmap_x,2) /= size(area,2)) THEN
	         CALL CoLM_Stop ('ERROR: inconsistent inpmat dimensions in the embedded CoLM unit-catchment file.')
	      ENDIF
	      IF (.not. all(ieee_is_finite(area))) THEN
	         CALL CoLM_Stop ('ERROR: non-finite inpmat_area in the embedded CoLM unit-catchment file.')
	      ENDIF
	      IF (any(area < 0._r8)) THEN
	         CALL CoLM_Stop ('ERROR: negative inpmat_area in the embedded CoLM unit-catchment file.')
	      ENDIF
	      IF (any((area > 0._r8) .and. &
	              (idmap_x < 1 .or. idmap_x > nlon_ucat .or. idmap_y < 1 .or. idmap_y > nlat_ucat))) THEN
	         CALL CoLM_Stop ('ERROR: positive unit-catchment overlap area has an out-of-range runoff grid index.')
	      ENDIF
	      DO iucat = 1, size(area,2)
	         first_map = 0
	         DO imap = 1, inpn
	            IF (area(imap,iucat) <= 0._r8) CYCLE
	            IF (first_map == 0) first_map = imap
	            grid_id = (idmap_y(imap,iucat)-1) * nlon_ucat + idmap_x(imap,iucat)
	            DO imap_other = imap + 1, inpn
	               IF (area(imap_other,iucat) <= 0._r8) CYCLE
	               IF ((idmap_y(imap_other,iucat)-1) * nlon_ucat + idmap_x(imap_other,iucat) == grid_id) THEN
	                  CALL CoLM_Stop ('ERROR: a unit catchment maps to the same runoff grid more than once.')
	               ENDIF
	            ENDDO
	         ENDDO
	         IF (first_map == 0) THEN
	            CALL CoLM_Stop ('ERROR: a unit catchment has no positive runoff-grid overlap area.')
	         ENDIF
	      ENDDO

	   END SUBROUTINE validate_mpas_embedded_inpmat

	   ! ---------
	   SUBROUTINE build_mpas_embedded_uc2gd (parafile, nlon_ucat, nlat_ucat, inpn, numinpm, inpm_gdid, &
      nucpart, idmap_uc2gd, area_uc2gd)

   USE MOD_MPAS_MPI
   USE MOD_NetCDFSerial
   USE MOD_Utils
   IMPLICIT NONE

   character(len=*), intent(in) :: parafile
   integer, intent(in) :: nlon_ucat, nlat_ucat
   integer, intent(in) :: inpn
   integer, intent(in) :: numinpm
   integer, intent(in) :: inpm_gdid(:)
   integer, intent(out) :: nucpart
   integer, allocatable, intent(out) :: idmap_uc2gd(:,:)
   real(r8), allocatable, intent(out) :: area_uc2gd(:,:)

   integer, parameter :: ucat_chunk_size = 131072
   integer, allocatable :: inpm_sorted(:), inpm_order(:), nucat_g(:)
   integer, allocatable :: idmap_x_blk(:,:), idmap_y_blk(:,:)
   real(r8), allocatable :: area_blk(:,:)
   integer :: istart, iend, iucat, imap, iloc, igrd, grid_id
   integer :: nucpart_local

      IF (numinpm > 0) THEN
         allocate (inpm_sorted (numinpm))
         allocate (inpm_order  (numinpm))
         allocate (nucat_g     (numinpm))

         inpm_sorted = inpm_gdid
         inpm_order  = (/(igrd, igrd = 1, numinpm)/)
         CALL quicksort (numinpm, inpm_sorted, inpm_order)
         nucat_g = 0

         istart = 1
         DO WHILE (istart <= totalnumucat)
            iend = min(istart + ucat_chunk_size - 1, totalnumucat)
            CALL ncio_read_part_serial (parafile, 'inpmat_x',    (/1,istart/), (/inpn,iend/), idmap_x_blk)
            CALL ncio_read_part_serial (parafile, 'inpmat_y',    (/1,istart/), (/inpn,iend/), idmap_y_blk)
            CALL ncio_read_part_serial (parafile, 'inpmat_area', (/1,istart/), (/inpn,iend/), area_blk)
            CALL validate_mpas_embedded_inpmat (idmap_x_blk, idmap_y_blk, area_blk, inpn, nlon_ucat, nlat_ucat)

            DO iucat = lbound(idmap_x_blk,2), ubound(idmap_x_blk,2)
               DO imap = 1, inpn
                  IF (area_blk(imap,iucat) > 0._r8 .and. idmap_x_blk(imap,iucat) > 0 .and. &
                      idmap_y_blk(imap,iucat) > 0) THEN
                     grid_id = (idmap_y_blk(imap,iucat)-1) * nlon_ucat + idmap_x_blk(imap,iucat)
                     iloc = find_in_sorted_list1 (grid_id, numinpm, inpm_sorted)
                     IF (iloc > 0) THEN
                        igrd = inpm_order(iloc)
                        nucat_g(igrd) = nucat_g(igrd) + 1
                     ENDIF
                  ENDIF
               ENDDO
            ENDDO

            deallocate (idmap_x_blk)
            deallocate (idmap_y_blk)
            deallocate (area_blk)
            istart = iend + 1
         ENDDO

         nucpart_local = maxval(nucat_g)
      ELSE
         nucpart_local = 0
      ENDIF

#ifdef MPAS_MPI
      CALL mpi_allreduce (nucpart_local, nucpart, 1, MPI_INTEGER, MPI_MAX, mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('river-network unit-overlap reduction')
#else
      nucpart = nucpart_local
#endif
      nucpart = max(1, nucpart)

      allocate (idmap_uc2gd (nucpart,numinpm))
      allocate (area_uc2gd  (nucpart,numinpm))
      idmap_uc2gd = 0
      area_uc2gd  = 0._r8

      IF (numinpm > 0 .and. nucpart > 0) THEN
         nucat_g = 0

         istart = 1
         DO WHILE (istart <= totalnumucat)
            iend = min(istart + ucat_chunk_size - 1, totalnumucat)
            CALL ncio_read_part_serial (parafile, 'inpmat_x',    (/1,istart/), (/inpn,iend/), idmap_x_blk)
            CALL ncio_read_part_serial (parafile, 'inpmat_y',    (/1,istart/), (/inpn,iend/), idmap_y_blk)
            CALL ncio_read_part_serial (parafile, 'inpmat_area', (/1,istart/), (/inpn,iend/), area_blk)
            CALL validate_mpas_embedded_inpmat (idmap_x_blk, idmap_y_blk, area_blk, inpn, nlon_ucat, nlat_ucat)

            DO iucat = lbound(idmap_x_blk,2), ubound(idmap_x_blk,2)
               DO imap = 1, inpn
                  IF (area_blk(imap,iucat) > 0._r8 .and. idmap_x_blk(imap,iucat) > 0 .and. &
                      idmap_y_blk(imap,iucat) > 0) THEN
                     grid_id = (idmap_y_blk(imap,iucat)-1) * nlon_ucat + idmap_x_blk(imap,iucat)
                     iloc = find_in_sorted_list1 (grid_id, numinpm, inpm_sorted)
                     IF (iloc > 0) THEN
                        igrd = inpm_order(iloc)
                        nucat_g(igrd) = nucat_g(igrd) + 1
                        idmap_uc2gd(nucat_g(igrd),igrd) = &
                           istart + iucat - lbound(idmap_x_blk,2)
                        area_uc2gd (nucat_g(igrd),igrd) = area_blk(imap,iucat)
                     ENDIF
                  ENDIF
               ENDDO
            ENDDO

            deallocate (idmap_x_blk)
            deallocate (idmap_y_blk)
            deallocate (area_blk)
            istart = iend + 1
         ENDDO
      ENDIF

      IF (allocated(inpm_sorted)) deallocate (inpm_sorted)
      IF (allocated(inpm_order )) deallocate (inpm_order )
      IF (allocated(nucat_g    )) deallocate (nucat_g    )

   END SUBROUTINE build_mpas_embedded_uc2gd
#endif

   ! ---------
   SUBROUTINE validate_mpas_embedded_river_parameters ()

   USE, INTRINSIC :: ieee_arithmetic, only: ieee_is_finite
   USE MOD_MPAS_MPI, only: CoLM_Stop
   IMPLICIT NONE

   integer :: j

      IF (.not. .true. .or. numucat <= 0) RETURN

      IF (.not. allocated(topo_rivelv) .or. .not. allocated(topo_rivhgt) .or. &
          .not. allocated(topo_rivlen) .or. .not. allocated(topo_rivman) .or. &
          .not. allocated(topo_rivwth) .or. .not. allocated(topo_rivstomax) .or. &
          .not. allocated(topo_area) .or. .not. allocated(topo_fldhgt)) THEN
         CALL CoLM_Stop ('ERROR: incomplete embedded CoLM river parameter data.')
      ENDIF
      IF (size(topo_rivelv) /= numucat .or. size(topo_rivhgt) /= numucat .or. &
          size(topo_rivlen) /= numucat .or. size(topo_rivman) /= numucat .or. &
          size(topo_rivwth) /= numucat .or. size(topo_rivstomax) /= numucat .or. &
          size(topo_area) /= numucat .or. size(topo_fldhgt,2) /= numucat .or. &
          size(topo_fldhgt,1) < 1) THEN
         CALL CoLM_Stop ('ERROR: embedded CoLM river parameter dimensions do not match local unit catchments.')
      ENDIF
      IF (.not. all(ieee_is_finite(topo_rivelv)) .or. .not. all(ieee_is_finite(topo_rivhgt)) .or. &
          .not. all(ieee_is_finite(topo_rivlen)) .or. .not. all(ieee_is_finite(topo_rivman)) .or. &
          .not. all(ieee_is_finite(topo_rivwth)) .or. .not. all(ieee_is_finite(topo_rivstomax)) .or. &
          .not. all(ieee_is_finite(topo_area)) .or. .not. all(ieee_is_finite(topo_fldhgt))) THEN
         CALL CoLM_Stop ('ERROR: non-finite embedded CoLM river parameter data.')
      ENDIF
      IF (any(topo_rivhgt <= 0._r8) .or. any(topo_rivlen <= 0._r8) .or. &
          any(topo_rivman <= 0._r8) .or. any(topo_rivwth <= 0._r8) .or. &
          any(topo_rivstomax <= 0._r8) .or. any(topo_area <= 0._r8)) THEN
         CALL CoLM_Stop ('ERROR: embedded CoLM river geometry and roughness parameters must be positive.')
      ENDIF
      IF (any(topo_fldhgt(1,:) <= 0._r8)) THEN
         CALL CoLM_Stop ('ERROR: embedded CoLM floodplain heights must start above the river bank.')
      ENDIF
      DO j = 2, size(topo_fldhgt,1)
         IF (any(topo_fldhgt(j,:) <= topo_fldhgt(j-1,:))) THEN
            CALL CoLM_Stop ('ERROR: embedded CoLM floodplain heights must be strictly increasing.')
         ENDIF
      ENDDO

   END SUBROUTINE validate_mpas_embedded_river_parameters

   ! ---------
   SUBROUTINE readin_riverlake_parameter (parafile, varname, rdata1d, rdata2d, idata1d)

   USE MOD_MPAS_MPI, only: mpas_comm, mpas_mpi_ierr, mpas_mpi_check
   USE MOD_NetCDFSerial, only: ncio_read_indexed_serial
   IMPLICIT NONE

   character(len=*), intent(in) :: parafile
   character(len=*), intent(in) :: varname
   real(r8), allocatable, intent(inout), optional :: rdata1d (:)
   real(r8), allocatable, intent(inout), optional :: rdata2d (:,:)
   integer,  allocatable, intent(inout), optional :: idata1d (:)

      IF (.true.) THEN
         IF (present(rdata1d)) CALL ncio_read_indexed_serial (parafile, varname, ucat_ucid, rdata1d)
         IF (present(rdata2d)) CALL ncio_read_indexed_serial (parafile, varname, ucat_ucid, rdata2d)
         IF (present(idata1d)) CALL ncio_read_indexed_serial (parafile, varname, ucat_ucid, idata1d)
      ENDIF
      CALL mpi_barrier (mpas_comm, mpas_mpi_ierr)
      CALL mpas_mpi_check('river-parameter indexed read completion')

   END SUBROUTINE readin_riverlake_parameter

   !
   FUNCTION retrieve_depth_from_volume (this, volume) result(depth)

   IMPLICIT NONE

   class(vol_dep_curve_type) :: this
   real(r8), intent(in)      :: volume
   real(r8) :: depth

   ! Local Variables
   real(r8) :: v0, g
   integer  :: i

      v0 = volume - this%rivstomax
      IF (v0 <= 0) THEN
         depth = volume / this%rivare
      ELSE
         i = 1
         DO WHILE (i <= this%nlfp)
            IF (v0 > this%flpstomax(i)) THEN
               i = i + 1
            ELSE
               EXIT
            ENDIF
         ENDDO

         IF (i == this%nlfp+1) THEN
            depth = this%rivhgt + this%flphgt(this%nlfp) &
               + (v0-this%flpstomax(this%nlfp)) / this%flpaccare(this%nlfp)
         ELSE
            g = (this%flphgt(i)-this%flphgt(i-1))/this%flparea(i)
            depth = this%rivhgt + this%flphgt(i-1) &
               + g * (-this%flpaccare(i-1)+sqrt((this%flpaccare(i-1))**2+2*(v0-this%flpstomax(i-1))/g))
         ENDIF
      ENDIF

   END FUNCTION retrieve_depth_from_volume

   !
   FUNCTION retrieve_volume_from_depth (this, depth) result(volume)

   IMPLICIT NONE

   class(vol_dep_curve_type) :: this
   real(r8), intent(in)  :: depth
   real(r8) :: volume

   ! Local Variables
   real(r8) :: h, d
   integer  :: i

      IF (depth <= this%rivhgt) THEN
         volume = this%rivare * depth
      ELSE
         i = 1
         DO WHILE (i <= this%nlfp)
            IF (depth > this%rivhgt+this%flphgt(i)) THEN
               i = i + 1
            ELSE
               EXIT
            ENDIF
         ENDDO

         d = depth - this%rivhgt - this%flphgt(i-1)
         IF (i == this%nlfp+1) THEN
            volume = this%rivstomax + this%flpstomax(this%nlfp) + d * this%flpaccare(this%nlfp)
         ELSE
            h = this%flphgt(i)-this%flphgt(i-1)
            volume = this%rivstomax + this%flpstomax(i-1) &
               + (d/h*this%flparea(i)+2*this%flpaccare(i-1))*d*0.5
         ENDIF
      ENDIF

   END FUNCTION retrieve_volume_from_depth

   !
   FUNCTION retrieve_area_from_depth (this, depth) result(area)

   IMPLICIT NONE

   class(vol_dep_curve_type) :: this
   real(r8), intent(in)  :: depth
   real(r8) :: area

   ! Local Variables
   real(r8) :: h, d
   integer  :: i

      IF (depth <= this%rivhgt) THEN
         area = 0.
      ELSE
         i = 1
         DO WHILE (i <= this%nlfp)
            IF (depth > this%rivhgt+this%flphgt(i)) THEN
               i = i + 1
            ELSE
               EXIT
            ENDIF
         ENDDO

         IF (i == this%nlfp+1) THEN
            area = this%flpaccare(this%nlfp)
         ELSE
            h = this%flphgt(i)-this%flphgt(i-1)
            d = depth - this%rivhgt - this%flphgt(i-1)
            area = this%flpaccare(i-1) + d/h * this%flparea(i)
         ENDIF
      ENDIF

   END FUNCTION retrieve_area_from_depth

   ! ---
   SUBROUTINE vol_depth_curve_free_mem (this)

   IMPLICIT NONE
   type(vol_dep_curve_type) :: this

      IF (allocated(this%flphgt   )) deallocate (this%flphgt   )
      IF (allocated(this%flparea  )) deallocate (this%flparea  )
      IF (allocated(this%flpaccare)) deallocate (this%flpaccare)
      IF (allocated(this%flpstomax)) deallocate (this%flpstomax)

   END SUBROUTINE vol_depth_curve_free_mem

   ! ---------
   SUBROUTINE riverlake_network_final ()

   IMPLICIT NONE

      IF (allocated(ucat_ucid        )) deallocate(ucat_ucid        )

      IF (allocated(inpm_gdid        )) deallocate(inpm_gdid        )
      IF (allocated(idmap_gd2uc      )) deallocate(idmap_gd2uc      )
      IF (allocated(area_gd2uc       )) deallocate(area_gd2uc       )
      IF (allocated(idmap_uc2gd      )) deallocate(idmap_uc2gd      )
      IF (allocated(area_uc2gd       )) deallocate(area_uc2gd       )
      IF (allocated(ucat_next        )) deallocate(ucat_next        )
      IF (allocated(ucat_ups         )) deallocate(ucat_ups         )
      IF (allocated(wts_ups          )) deallocate(wts_ups          )
      IF (allocated(irivsys          )) deallocate(irivsys          )
#ifdef MPAS_MPI
      IF (allocated(rivsys_send_counts)) deallocate(rivsys_send_counts)
      IF (allocated(rivsys_send_displs)) deallocate(rivsys_send_displs)
      IF (allocated(rivsys_recv_counts)) deallocate(rivsys_recv_counts)
      IF (allocated(rivsys_recv_displs)) deallocate(rivsys_recv_displs)
      IF (allocated(rivsys_send_local )) deallocate(rivsys_send_local )
      IF (allocated(rivsys_recv_owner )) deallocate(rivsys_recv_owner )
#endif

      IF (allocated(topo_rivelv      )) deallocate(topo_rivelv      )
      IF (allocated(topo_rivhgt      )) deallocate(topo_rivhgt      )
      IF (allocated(topo_rivlen      )) deallocate(topo_rivlen      )
      IF (allocated(topo_rivman      )) deallocate(topo_rivman      )
      IF (allocated(topo_rivwth      )) deallocate(topo_rivwth      )
      IF (allocated(topo_rivare      )) deallocate(topo_rivare      )
      IF (allocated(topo_rivstomax   )) deallocate(topo_rivstomax   )
      IF (allocated(topo_area        )) deallocate(topo_area        )
      IF (allocated(topo_fldhgt      )) deallocate(topo_fldhgt      )
      IF (allocated(bedelv_next      )) deallocate(bedelv_next      )
      IF (allocated(outletwth        )) deallocate(outletwth        )

      IF (allocated(floodplain_curve )) deallocate(floodplain_curve )

      CALL grid_free_mem(griducat)
      CALL compute_remapdata_free_mem(remap_patch2inpm)
      CALL compute_pushdata_free_mem(push_inpm2ucat)
      CALL compute_pushdata_free_mem(push_ucat2inpm)
      CALL compute_pushdata_free_mem(push_next2ucat)
      CALL compute_pushdata_free_mem(push_ups2ucat)

      totalnumucat = 0
      numucat = 0
      numinpm = 0
      inpn = 0
      nucpart = 0
      upnmax = 0
      numrivsys = 0
#ifdef MPAS_MPI
      num_owned_rivsys = 0
#endif

   END SUBROUTINE riverlake_network_final

END MODULE MOD_Grid_RiverLakeNetwork
#endif
