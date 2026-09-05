#include <define.h>

#ifdef GridRiverLakeFlow
MODULE MOD_Grid_Reservoir
!-----------------------------------------------------------------------
! DESCRIPTION:
!
!    Reservoir module in gridded mesh.
!
! Created by Shupeng Zhang, Oct 2025
!-----------------------------------------------------------------------
   USE MOD_Precision
   USE MOD_DataType

   integer :: totalnumresv = 0
   integer :: numresv = 0
   integer,  allocatable :: ucat2resv   (:)
   integer,  allocatable :: resv_global_index(:)
   type(pointer_int32_1d), allocatable :: resv_data_address (:)


   ! parameters
   integer,  allocatable :: dam_GRAND_ID  (:)  ! GRAND dam ID

   integer,  allocatable :: dam_build_year(:)  ! year in which the dam/barrier was built

   real(r8), allocatable :: volresv_total (:)  ! total reservoir volume      [m^3]
   real(r8), allocatable :: volresv_emerg (:)  ! emergency reservoir volume  [m^3]
   real(r8), allocatable :: volresv_adjust(:)  ! adjustment reservoir volume [m^3]
   real(r8), allocatable :: volresv_normal(:)  ! normal reservoir volume     [m^3]

   real(r8), allocatable :: qresv_flood   (:)  ! flood reservoir outflow      [m^3/s]
   real(r8), allocatable :: qresv_adjust  (:)  ! adjustment reservoir outflow [m^3/s]
   real(r8), allocatable :: qresv_normal  (:)  ! normal reservoir outflow     [m^3/s]

   ! fluxes
   real(r8), allocatable :: qresv_in      (:)  ! reservoir inflow  [m^3/s]
   real(r8), allocatable :: qresv_out     (:)  ! reservoir outflow [m^3/s]

   ! -- PUBLIC SUBROUTINEs --
   PUBLIC :: reservoir_init
   PUBLIC :: reservoir_operation
   PUBLIC :: reservoir_final

CONTAINS

   ! -------
   SUBROUTINE reservoir_init ( )

   USE MOD_MPAS_MPI
   USE MOD_NetCDFSerial
   USE MOD_Utils
	   USE MOD_Namelist,              only: DEF_ReservoirPara_file
	   USE MOD_Grid_RiverLakeNetwork, only: numucat, ucat_ucid, lake_type
	   USE, INTRINSIC :: ieee_arithmetic, only: ieee_is_finite

   IMPLICIT NONE

   ! Local variables
   character(len=256) :: parafile

   integer,  allocatable :: dam_seq(:), order(:), local_ucid(:)
   real(r8), allocatable :: rcache (:)
   integer,  allocatable :: icache (:)

   integer, parameter :: dam_seq_chunk_size = 1048576
	   integer :: i, iloc, irsv
#ifndef MPAS_EMBEDDED_COLM
	   integer :: nresv, irank
#endif
   integer :: istart, iend, local_index, global_resv_index


      parafile = DEF_ReservoirPara_file

#ifndef MPAS_EMBEDDED_COLM
      IF (mpas_is_root) THEN
         CALL ncio_read_serial (parafile, 'dam_GRAND_ID', dam_GRAND_ID)
      ENDIF

      CALL ncio_read_bcast_serial (parafile, 'dam_seq', dam_seq)

      totalnumresv = size(dam_seq)
#else
      CALL ncio_inquire_length (parafile, 'dam_seq', totalnumresv)
#endif

      IF (.true.) THEN

         allocate (ucat2resv (numucat))
         ucat2resv = 0
         allocate (resv_global_index(numucat))

         numresv = 0

#ifdef MPAS_EMBEDDED_COLM
         IF (numucat > 0 .and. totalnumresv > 0) THEN
            allocate (order      (numucat))
            allocate (local_ucid (numucat))

            order = (/(i, i = 1, numucat)/)
            local_ucid = ucat_ucid
            CALL quicksort (numucat, local_ucid, order)

            istart = 1
            DO WHILE (istart <= totalnumresv)
               iend = min(istart + dam_seq_chunk_size - 1, totalnumresv)
               CALL ncio_read_part_serial (parafile, 'dam_seq', istart, iend, dam_seq)

               DO i = lbound(dam_seq,1), ubound(dam_seq,1)
                  iloc = find_in_sorted_list1 (dam_seq(i), numucat, local_ucid)
                  IF (iloc > 0) THEN
                     local_index = order(iloc)
	                  global_resv_index = istart + i - lbound(dam_seq,1)
	                  IF (ucat2resv(local_index) /= 0) THEN
	                     CALL CoLM_stop('Duplicate dam_seq entry for a local embedded CoLM unit catchment.')
	                  ENDIF
	                  numresv = numresv + 1
	                  lake_type(local_index) = 2
	                  ucat2resv(local_index) = numresv
	                  resv_global_index(numresv) = global_resv_index
                  ENDIF
               ENDDO

               deallocate (dam_seq)
               istart = iend + 1
            ENDDO
         ENDIF
#else
         allocate (order (totalnumresv))
         order = (/(i, i = 1, totalnumresv)/)

         CALL quicksort (totalnumresv, dam_seq, order)

         DO i = 1, numucat
            iloc = find_in_sorted_list1 (ucat_ucid(i), totalnumresv, dam_seq)
            IF (iloc > 0) THEN
               numresv = numresv + 1
               lake_type(i) = 2
               ucat2resv(i) = numresv
               resv_global_index(numresv) = order(iloc)
            ENDIF
         ENDDO
#endif

      ENDIF

#ifndef MPAS_EMBEDDED_COLM
#ifdef MPAS_MPI
      IF (.not. allocated(resv_data_address)) allocate (resv_data_address (0:mpas_size-1))

      IF (mpas_is_root) THEN
         DO irank = 0, mpas_size-1

            IF (irank == mpas_rank) THEN
               nresv = numresv
            ELSE
               CALL mpi_recv (nresv, 1, MPI_INTEGER, &
                  irank, mpi_tag_mesg, mpas_comm, mpas_status, mpas_mpi_ierr)
            ENDIF

            IF (nresv > 0) THEN
               allocate (resv_data_address(irank)%val (nresv))
               IF (irank == mpas_rank) THEN
                  resv_data_address(irank)%val = resv_global_index(1:nresv)
               ELSE
                  CALL mpi_recv (resv_data_address(irank)%val, nresv, MPI_INTEGER, &
                     irank, mpi_tag_data, mpas_comm, mpas_status, mpas_mpi_ierr)
               ENDIF
            ENDIF
         ENDDO

      ENDIF

      IF (.true. .and. (.not. mpas_is_root)) THEN

         CALL mpi_send (numresv, 1, MPI_INTEGER, mpas_root, mpi_tag_mesg, mpas_comm, mpas_mpi_ierr)

         IF (numresv > 0) THEN
            CALL mpi_send (resv_global_index(1:numresv), numresv, MPI_INTEGER, mpas_root, &
               mpi_tag_data, mpas_comm, mpas_mpi_ierr)
         ENDIF

      ENDIF
#else
      IF (numresv > 0) THEN
         allocate (resv_data_address (0:0))
         allocate (resv_data_address(0)%val (numresv))
         resv_data_address(0)%val = resv_global_index(1:numresv)
      ENDIF
#endif
#endif

      IF (.true.) THEN

         IF (numresv > 0) THEN

            allocate (dam_build_year (numresv))

            allocate (volresv_total  (numresv))
            allocate (volresv_emerg  (numresv))
            allocate (volresv_adjust (numresv))
            allocate (volresv_normal (numresv))

            allocate (qresv_flood    (numresv))
            allocate (qresv_adjust   (numresv))
            allocate (qresv_normal   (numresv))

	            allocate (qresv_in       (numresv))
	            allocate (qresv_out      (numresv))
	            qresv_in(:) = 0._r8
	            qresv_out(:) = 0._r8

         ENDIF

      ENDIF

#ifdef MPAS_EMBEDDED_COLM
      IF (.true. .and. (numresv > 0)) THEN
         CALL ncio_read_indexed_serial (parafile, 'dam_year', resv_global_index(1:numresv), icache)
         dam_build_year = icache

         CALL ncio_read_indexed_serial (parafile, 'dam_TotalVol_mcm', resv_global_index(1:numresv), rcache)
         volresv_total = rcache*1.e6

         CALL ncio_read_indexed_serial (parafile, 'dam_ConVol_mcm', resv_global_index(1:numresv), rcache)
         volresv_normal = rcache*1.e6

         CALL ncio_read_indexed_serial (parafile, 'dam_Qn', resv_global_index(1:numresv), rcache)
         qresv_normal = rcache

         CALL ncio_read_indexed_serial (parafile, 'dam_Qf', resv_global_index(1:numresv), rcache)
         qresv_flood = rcache
      ENDIF
#else
      CALL ncio_read_bcast_serial (parafile, 'dam_year', icache)
      IF (.true. .and. (numresv > 0)) THEN
         dam_build_year = icache(resv_global_index(1:numresv))
      ENDIF

      CALL ncio_read_bcast_serial (parafile, 'dam_TotalVol_mcm', rcache)
      IF (.true. .and. (numresv > 0)) THEN
         volresv_total = rcache(resv_global_index(1:numresv))*1.e6
      ENDIF

      CALL ncio_read_bcast_serial (parafile, 'dam_ConVol_mcm', rcache)
      IF (.true. .and. (numresv > 0)) THEN
         volresv_normal = rcache(resv_global_index(1:numresv))*1.e6
      ENDIF

      CALL ncio_read_bcast_serial (parafile, 'dam_Qn', rcache)
      IF (.true. .and. (numresv > 0)) THEN
         qresv_normal = rcache(resv_global_index(1:numresv))
      ENDIF

      CALL ncio_read_bcast_serial (parafile, 'dam_Qf', rcache)
      IF (.true. .and. (numresv > 0)) THEN
         qresv_flood = rcache(resv_global_index(1:numresv))
      ENDIF
#endif


	      IF (.true.) THEN
	         IF (numresv > 0) THEN
	            IF (any(resv_global_index(1:numresv) < 1) .or. &
	                any(resv_global_index(1:numresv) > totalnumresv)) THEN
	               CALL CoLM_stop('Embedded CoLM reservoir index is outside the parameter file.')
	            ENDIF
	            IF (.not. all(ieee_is_finite(volresv_total)) .or. &
	                .not. all(ieee_is_finite(volresv_normal)) .or. &
	                .not. all(ieee_is_finite(qresv_normal)) .or. &
	                .not. all(ieee_is_finite(qresv_flood))) THEN
	               CALL CoLM_stop('Embedded CoLM reservoir parameters contain non-finite values.')
	            ENDIF
	            IF (any(dam_build_year <= 0) .or. any(volresv_total <= 0._r8) .or. &
                any(volresv_normal <= 0._r8) .or. any(volresv_normal > volresv_total) .or. &
                any(qresv_normal < 0._r8) .or. any(qresv_flood < qresv_normal)) THEN
               CALL CoLM_stop('Embedded CoLM reservoir parameters are outside their physical ranges.')
            ENDIF
	         ENDIF
	         DO irsv = 1, numresv
            volresv_emerg (irsv) = volresv_total(irsv) * 0.94
            volresv_adjust(irsv) = volresv_total(irsv) * 0.77
            volresv_normal(irsv) = min(volresv_total(irsv)*0.7, volresv_normal(irsv))
            qresv_adjust  (irsv) = (qresv_normal(irsv) + qresv_flood(irsv)) * 0.5
         ENDDO
      ENDIF

      IF (allocated(dam_seq)) deallocate(dam_seq)
      IF (allocated(order  )) deallocate(order  )
      IF (allocated(local_ucid)) deallocate(local_ucid)
      IF (allocated(rcache )) deallocate(rcache )
      IF (allocated(icache )) deallocate(icache )

   END SUBROUTINE reservoir_init


	   SUBROUTINE reservoir_operation (method, irsv, qin, vol, qout)

	   USE MOD_MPAS_MPI, only: CoLM_stop
	   USE, INTRINSIC :: ieee_arithmetic, only: ieee_is_finite
	   IMPLICIT NONE
   integer,  intent(in)  :: method
   integer,  intent(in)  :: irsv
   real(r8), intent(in)  :: qin, vol
   real(r8), intent(out) :: qout

   ! local variables
	   real(r8) :: q1

	      IF (irsv < 1 .or. irsv > numresv) THEN
	         CALL CoLM_stop('Embedded CoLM reservoir operation received an invalid local reservoir index.')
	      ENDIF
	      IF (.not. ieee_is_finite(qin) .or. .not. ieee_is_finite(vol) .or. vol < 0._r8) THEN
	         CALL CoLM_stop('Embedded CoLM reservoir operation received invalid state or forcing.')
	      ENDIF
	      IF (method /= 1) THEN
	         CALL CoLM_stop('Unsupported embedded CoLM reservoir operation method.')
	      ENDIF

	      IF (method == 1) THEN
         ! *** Reference ***
         ! [1] Mizuki Funato, Dai Yamazaki, Dung Trung Vu.
         ! Development of an improved reservoir operation scheme for global flood modeling.
         ! ESS Open Archive . October 24, 2024.

         IF (vol > volresv_emerg(irsv)) THEN
            qout = max(qin, qresv_flood(irsv))
         ELSEIF (vol > volresv_adjust(irsv)) THEN
            qout = qresv_adjust(irsv) + (qresv_flood(irsv)-qresv_adjust(irsv)) &
               * ((vol-volresv_adjust(irsv))/(volresv_emerg(irsv)-volresv_adjust(irsv)))**0.1
            IF (qin > qresv_flood(irsv)) THEN
               q1 = qresv_normal(irsv) + (qin-qresv_normal(irsv)) &
                  * (vol-volresv_normal(irsv))/(volresv_emerg(irsv)-volresv_normal(irsv))
               qout = max(q1, qout)
            ENDIF
         ELSEIF (vol > volresv_normal(irsv)) THEN
            qout = qresv_normal(irsv) + (qresv_adjust(irsv)-qresv_normal(irsv)) &
               * ((vol-volresv_normal(irsv))/(volresv_adjust(irsv)-volresv_normal(irsv)))**3.
         ELSE
            qout = (vol/volresv_normal(irsv))**0.5 * qresv_normal(irsv)
         ENDIF

      ENDIF

   END SUBROUTINE reservoir_operation


   SUBROUTINE reservoir_final ()

   IMPLICIT NONE

      IF (allocated(ucat2resv        )) deallocate (ucat2resv        )
      IF (allocated(resv_global_index)) deallocate (resv_global_index)
      IF (allocated(resv_data_address)) deallocate (resv_data_address)

      IF (allocated(dam_GRAND_ID     )) deallocate (dam_GRAND_ID     )
      IF (allocated(dam_build_year   )) deallocate (dam_build_year   )

      IF (allocated(volresv_total    )) deallocate (volresv_total    )
      IF (allocated(volresv_emerg    )) deallocate (volresv_emerg    )
      IF (allocated(volresv_adjust   )) deallocate (volresv_adjust   )
      IF (allocated(volresv_normal   )) deallocate (volresv_normal   )

      IF (allocated(qresv_flood      )) deallocate (qresv_flood      )
      IF (allocated(qresv_adjust     )) deallocate (qresv_adjust     )
      IF (allocated(qresv_normal     )) deallocate (qresv_normal     )

      IF (allocated(qresv_in         )) deallocate (qresv_in         )
      IF (allocated(qresv_out        )) deallocate (qresv_out        )

      totalnumresv = 0
      numresv = 0

   END SUBROUTINE reservoir_final

END MODULE MOD_Grid_Reservoir
#endif
