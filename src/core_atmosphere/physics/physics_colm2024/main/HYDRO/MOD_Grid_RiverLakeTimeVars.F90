#include <define.h>

#ifdef GridRiverLakeFlow
MODULE MOD_Grid_RiverLakeTimeVars
!-------------------------------------------------------------------------------------
! DESCRIPTION:
!
!   Time Variables in gridded hydrological processes.
!
! Created by Shupeng Zhang, Oct 2025
!-------------------------------------------------------------------------------------

   USE MOD_Precision
#ifdef GridRiverLakeSediment
   USE MOD_Grid_RiverLakeSediment, only: write_sediment_restart
#endif
   IMPLICIT NONE

   ! -- state variables --
   real(r8), allocatable :: wdsrf_ucat (:) ! river or lake water depth [m]
   real(r8), allocatable :: veloc_riv  (:) ! river velocity            [m/s]
   real(r8), allocatable :: discharge_riv (:) ! routing-period mean downstream discharge [m3/s]
   real(r8), allocatable :: momen_riv  (:) ! unit river momentum       [m^2/s]
   real(r8), allocatable :: volresv    (:) ! reservoir water volume    [m^3]
   real(r8)              :: acctime_rnof = 0._r8
   real(r8), allocatable :: acc_rnof_uc (:) ! accumulated runoff volume [m^3]

   ! Area-weighted diagnostics on the local CoLM element/MPAS-cell mesh.
   real(r8), allocatable :: river_water_depth_elm (:)
   real(r8), allocatable :: river_velocity_elm    (:)
   real(r8), allocatable :: river_discharge_elm   (:)

   ! -- restart file path (saved for deferred sediment restart read) --
   character(len=512) :: gridriver_restart_file = ''

   ! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: allocate_GridRiverLakeTimeVars
   PUBLIC :: deallocate_GridRiverLakeTimeVars

   PUBLIC :: read_GridRiverLakeTimeVars
   PUBLIC :: write_GridRiverLakeTimeVars
   PUBLIC :: validate_GridRiverRestart
   PUBLIC :: update_GridRiverLakeElementDiagnostics

CONTAINS

   SUBROUTINE allocate_GridRiverLakeTimeVars

   USE MOD_MPAS_MPI
   USE MOD_Grid_RiverLakeNetwork, only: numucat
   USE MOD_Grid_Reservoir,        only: numresv
   USE MOD_Mesh,                  only: numelm
   USE MOD_Vars_Global,           only: spval
   IMPLICIT NONE

	      IF (.true.) THEN
	         IF (numucat < 0 .or. numresv < 0) THEN
	            CALL CoLM_stop('Cannot allocate embedded CoLM river state with negative local dimensions.')
	         ENDIF
	         IF (allocated(wdsrf_ucat) .or. allocated(veloc_riv) .or. allocated(discharge_riv) .or. &
	             allocated(momen_riv) .or. allocated(volresv) .or. allocated(acc_rnof_uc) .or. &
	             allocated(river_water_depth_elm) .or. allocated(river_velocity_elm) .or. &
	             allocated(river_discharge_elm)) THEN
	            CALL CoLM_stop('Embedded CoLM river state was allocated more than once.')
	         ENDIF

	         allocate (wdsrf_ucat (numucat))
	         allocate (veloc_riv  (numucat))
	         allocate (discharge_riv(numucat))
	         allocate (momen_riv  (numucat))
	         allocate (volresv    (numresv))
	         allocate (acc_rnof_uc(numucat))
	         allocate (river_water_depth_elm(numelm))
	         allocate (river_velocity_elm   (numelm))
	         allocate (river_discharge_elm  (numelm))
	         wdsrf_ucat = 0._r8
	         veloc_riv = 0._r8
	         discharge_riv = 0._r8
	         momen_riv = 0._r8
	         volresv = spval
	         acc_rnof_uc = 0._r8
	         river_water_depth_elm = spval
	         river_velocity_elm = spval
	         river_discharge_elm = spval
	         acctime_rnof = 0._r8

	      ENDIF

   END SUBROUTINE allocate_GridRiverLakeTimeVars


   SUBROUTINE READ_GridRiverLakeTimeVars (file_restart, require_complete_restart, checkpoint_family_id, &
                                          patch_file, pft_file, gridriver_file)

   USE MOD_MPAS_MPI
   USE MOD_Namelist
	   USE MOD_NetCDFSerial, only: ncio_read_bcast_serial, ncio_read_indexed_serial, &
	                              ncio_var_exist_bcast_serial
	   USE MOD_Grid_RiverLakeNetwork, only: numucat, totalnumucat, ucat_ucid
	   USE MOD_Grid_Reservoir,        only: numresv, totalnumresv, resv_global_index
	   USE MOD_Vars_Global,           only: spval
	   USE, INTRINSIC :: ieee_arithmetic, only: ieee_is_finite
   IMPLICIT NONE

	   character(len=*), intent(in) :: file_restart
	   logical, intent(in) :: require_complete_restart
	   character(len=*), intent(in), optional :: checkpoint_family_id
	   character(len=*), intent(in), optional :: patch_file, pft_file, gridriver_file
	   logical :: has_acc_rnof
	   logical :: has_acctime_rnof
	   logical :: has_discharge

	      gridriver_restart_file = trim(file_restart)
#ifdef MPAS_EMBEDDED_COLM
	      IF (require_complete_restart) THEN
	         IF (.not. present(checkpoint_family_id) .or. .not. present(patch_file) .or. &
	             .not. present(pft_file) .or. .not. present(gridriver_file)) THEN
	            CALL CoLM_stop('Complete embedded CoLM river restart validation requires a checkpoint family manifest.')
	            RETURN
	         ENDIF
	         CALL validate_GridRiverRestart(file_restart, checkpoint_family_id, patch_file, &
	                                            pft_file, gridriver_file)
	      ENDIF
#endif
	      ! Optional-field decisions must be identical before any collective read.
	      ! Local indexed reads below still require this file on a shared filesystem.
	      has_acc_rnof = ncio_var_exist_bcast_serial(file_restart, 'acc_rnof_uc', .false.)
	      has_acctime_rnof = ncio_var_exist_bcast_serial(file_restart, 'acctime_rnof', .false.)
	      has_discharge = ncio_var_exist_bcast_serial(file_restart, 'discharge_riv', .false.)

      IF (.true.) THEN
         CALL validate_gridriver_indices(ucat_ucid, numucat, totalnumucat, 'unit-catchment')
      ENDIF
      IF (.true. .and. numucat > 0) THEN
         CALL ncio_read_indexed_serial (file_restart, 'wdsrf_ucat', ucat_ucid, wdsrf_ucat)
         CALL ncio_read_indexed_serial (file_restart, 'veloc_riv',  ucat_ucid, veloc_riv )
	         IF (has_discharge) CALL ncio_read_indexed_serial (file_restart, 'discharge_riv', ucat_ucid, discharge_riv)
	         IF (has_acc_rnof) CALL ncio_read_indexed_serial (file_restart, 'acc_rnof_uc', ucat_ucid, acc_rnof_uc)
      ENDIF
	      IF (has_acctime_rnof) CALL ncio_read_bcast_serial (file_restart, 'acctime_rnof', acctime_rnof)
	      IF (.not. has_acc_rnof .or. .not. has_acctime_rnof) THEN
	         IF (require_complete_restart) THEN
	            IF (mpas_is_root) THEN
	               write(*,'(A)') 'CoLM river restart is missing runoff-accumulator state required for water-continuous restart.'
	            ENDIF
	            CALL CoLM_stop('Use a restart containing acc_rnof_uc and acctime_rnof, or start a new simulation.')
	         ELSE
	            acc_rnof_uc = 0._r8
	            acctime_rnof = 0._r8
	            IF (mpas_is_root) THEN
	               write(*,'(A)') 'Initializing CoLM river runoff accumulators to zero from legacy cold-start data.'
	            ENDIF
	         ENDIF
	      ENDIF
	      IF (.not. ieee_is_finite(acctime_rnof) .or. acctime_rnof < 0._r8) THEN
	         CALL CoLM_stop('Embedded CoLM river restart contains an invalid runoff accumulation time.')
	      ENDIF
	      IF (.true. .and. numucat > 0) THEN
	         IF (.not. all(ieee_is_finite(wdsrf_ucat)) .or. any(wdsrf_ucat < 0._r8) .or. &
	             .not. all(ieee_is_finite(veloc_riv)) .or. .not. all(ieee_is_finite(discharge_riv)) .or. &
	             .not. all(ieee_is_finite(acc_rnof_uc)) .or. &
	             any(acc_rnof_uc < 0._r8)) THEN
	            CALL CoLM_stop('Embedded CoLM river restart contains invalid unit-catchment state.')
	         ENDIF
	      ENDIF

      IF (DEF_Reservoir_Method > 0) THEN
         IF (totalnumresv > 0) THEN
            IF (.true. .and. numresv > 0) THEN
               CALL validate_gridriver_indices(resv_global_index, numresv, totalnumresv, 'reservoir')
	               CALL ncio_read_indexed_serial (file_restart, 'volresv', resv_global_index(1:numresv), volresv)
	               IF (.not. all(ieee_is_finite(volresv)) .or. &
	                   any(volresv < 0._r8 .and. abs(volresv) < 0.5_r8 * abs(spval))) THEN
	                  CALL CoLM_stop('Embedded CoLM river restart contains invalid reservoir storage.')
	               ENDIF
            ENDIF
         ENDIF
      ENDIF

      CALL update_GridRiverLakeElementDiagnostics()

      ! Note: sediment restart is read separately in grid_sediment_read_restart,
      ! called from grid_riverlake_flow_init after sediment module is initialized.

   END SUBROUTINE READ_GridRiverLakeTimeVars


   SUBROUTINE WRITE_GridRiverLakeTimeVars (file_restart, checkpoint_family_id, patch_file, pft_file, gridriver_file)

   USE MOD_MPAS_MPI
   USE MOD_Namelist
   IMPLICIT NONE

   character(len=*), intent(in) :: file_restart
   character(len=*), intent(in), optional :: checkpoint_family_id
   character(len=*), intent(in), optional :: patch_file, pft_file, gridriver_file

#ifdef MPAS_EMBEDDED_COLM
      IF (.not. present(checkpoint_family_id) .or. .not. present(patch_file) .or. &
          .not. present(pft_file) .or. .not. present(gridriver_file)) THEN
         CALL CoLM_stop('Embedded CoLM river restart write requires a checkpoint family manifest.')
         RETURN
      ENDIF
      CALL write_gridriver_restart_mpas_embedded(file_restart, checkpoint_family_id, patch_file, &
                                                pft_file, gridriver_file)
#else
      CALL write_gridriver_restart_mpas_embedded(file_restart)
#endif

#ifdef GridRiverLakeSediment
      IF (DEF_USE_SEDIMENT) THEN
         CALL write_sediment_restart(file_restart)
      ENDIF
#endif

   END SUBROUTINE WRITE_GridRiverLakeTimeVars

   SUBROUTINE write_gridriver_restart_mpas_embedded (file_restart, checkpoint_family_id, patch_file, &
                                                     pft_file, gridriver_file)

   USE mpi, only: MPI_INFO_NULL, MPI_OFFSET_KIND
   USE pnetcdf
   USE MOD_MPAS_MPI, only: mpas_comm, mpas_rank, CoLM_stop
   USE MOD_Namelist, only: DEF_Reservoir_Method
   USE MOD_Grid_RiverLakeNetwork, only: numucat, totalnumucat, ucat_ucid
   USE MOD_Grid_Reservoir,        only: numresv, totalnumresv, resv_global_index
   IMPLICIT NONE

   character(len=*), intent(in) :: file_restart
   character(len=*), intent(in), optional :: checkpoint_family_id
   character(len=*), intent(in), optional :: patch_file, pft_file, gridriver_file

   integer :: ierr
   integer :: ncid
   integer :: dim_ucatch
   integer :: dim_reservoir
   integer :: var_wdsrf
   integer :: var_veloc
   integer :: var_discharge
   integer :: var_acc_rnof
   integer :: var_acctime_rnof
   integer :: var_volresv
   logical :: write_reservoir
   logical :: write_family_manifest
   character(len=2048) :: patch_basename, pft_basename, gridriver_basename

      write_reservoir = DEF_Reservoir_Method > 0 .and. totalnumresv > 0
	      write_family_manifest = present(checkpoint_family_id) .and. present(patch_file) .and. &
	                              present(pft_file) .and. present(gridriver_file)
	      IF (write_family_manifest) THEN
	         CALL gridriver_restart_basename(patch_file, patch_basename)
	         CALL gridriver_restart_basename(pft_file, pft_basename)
	         CALL gridriver_restart_basename(gridriver_file, gridriver_basename)
	         IF (trim(checkpoint_family_id) == '' .or. trim(patch_basename) == '' .or. &
	             trim(pft_basename) == '' .or. trim(gridriver_basename) == '' .or. &
	             trim(gridriver_basename) == 'NONE') THEN
	            CALL CoLM_stop('Embedded CoLM river restart received an invalid checkpoint family manifest.')
	         ENDIF
	      ENDIF

      IF (totalnumucat < 1) THEN
         CALL CoLM_stop('Cannot write an embedded CoLM river restart with no global unit catchments.')
      ENDIF
      CALL validate_gridriver_indices(ucat_ucid, numucat, totalnumucat, 'unit-catchment')
      IF (write_reservoir) THEN
         CALL validate_gridriver_indices(resv_global_index, numresv, totalnumresv, 'reservoir')
      ENDIF

      ierr = nf90mpi_create(mpas_comm, trim(file_restart), &
         IOR(NF90_CLOBBER, NF90_64BIT_OFFSET), MPI_INFO_NULL, ncid)
      CALL pnetcdf_check(ierr, 'create', file_restart)

      ierr = nf90mpi_def_dim(ncid, 'ucatch', int(totalnumucat, MPI_OFFSET_KIND), dim_ucatch)
      CALL pnetcdf_check(ierr, 'define ucatch dimension', file_restart)

      ierr = nf90mpi_def_var(ncid, 'wdsrf_ucat', NF90_DOUBLE, (/dim_ucatch/), var_wdsrf)
      CALL pnetcdf_check(ierr, 'define wdsrf_ucat', file_restart)

      ierr = nf90mpi_def_var(ncid, 'veloc_riv', NF90_DOUBLE, (/dim_ucatch/), var_veloc)
      CALL pnetcdf_check(ierr, 'define veloc_riv', file_restart)

      ierr = nf90mpi_def_var(ncid, 'discharge_riv', NF90_DOUBLE, (/dim_ucatch/), var_discharge)
      CALL pnetcdf_check(ierr, 'define discharge_riv', file_restart)

      ierr = nf90mpi_def_var(ncid, 'acc_rnof_uc', NF90_DOUBLE, (/dim_ucatch/), var_acc_rnof)
      CALL pnetcdf_check(ierr, 'define acc_rnof_uc', file_restart)

      ierr = nf90mpi_def_var(ncid, 'acctime_rnof', NF90_DOUBLE, varid = var_acctime_rnof)
      CALL pnetcdf_check(ierr, 'define acctime_rnof', file_restart)

      IF (write_reservoir) THEN
         ierr = nf90mpi_def_dim(ncid, 'reservoir', int(totalnumresv, MPI_OFFSET_KIND), dim_reservoir)
         CALL pnetcdf_check(ierr, 'define reservoir dimension', file_restart)

         ierr = nf90mpi_def_var(ncid, 'volresv', NF90_DOUBLE, (/dim_reservoir/), var_volresv)
         CALL pnetcdf_check(ierr, 'define volresv', file_restart)
      ENDIF

	      IF (write_family_manifest) THEN
	         ierr = nf90mpi_put_att(ncid, NF90_GLOBAL, 'colm_checkpoint_family_id', trim(checkpoint_family_id))
	         CALL pnetcdf_check(ierr, 'define checkpoint family ID attribute', file_restart)
	         ierr = nf90mpi_put_att(ncid, NF90_GLOBAL, 'colm_checkpoint_role', 'gridriver')
	         CALL pnetcdf_check(ierr, 'define checkpoint role attribute', file_restart)
	         ierr = nf90mpi_put_att(ncid, NF90_GLOBAL, 'colm_checkpoint_patch_file', trim(patch_basename))
	         CALL pnetcdf_check(ierr, 'define checkpoint patch-file attribute', file_restart)
	         ierr = nf90mpi_put_att(ncid, NF90_GLOBAL, 'colm_checkpoint_pft_file', trim(pft_basename))
	         CALL pnetcdf_check(ierr, 'define checkpoint PFT-file attribute', file_restart)
	         ierr = nf90mpi_put_att(ncid, NF90_GLOBAL, 'colm_checkpoint_gridriver_file', trim(gridriver_basename))
	         CALL pnetcdf_check(ierr, 'define checkpoint grid-river-file attribute', file_restart)
	      ENDIF

      ierr = nf90mpi_enddef(ncid)
      CALL pnetcdf_check(ierr, 'end define mode', file_restart)

      ierr = nf90mpi_begin_indep_data(ncid)
      CALL pnetcdf_check(ierr, 'begin independent data mode', file_restart)

      CALL pnetcdf_write_real8_points(ncid, var_wdsrf, ucat_ucid, wdsrf_ucat, numucat, totalnumucat, &
         'wdsrf_ucat', file_restart)
      CALL pnetcdf_write_real8_points(ncid, var_veloc, ucat_ucid, veloc_riv, numucat, totalnumucat, &
         'veloc_riv', file_restart)
      CALL pnetcdf_write_real8_points(ncid, var_discharge, ucat_ucid, discharge_riv, numucat, totalnumucat, &
         'discharge_riv', file_restart)
      CALL pnetcdf_write_real8_points(ncid, var_acc_rnof, ucat_ucid, acc_rnof_uc, numucat, totalnumucat, &
         'acc_rnof_uc', file_restart)

      IF (mpas_rank == 0) THEN
         ierr = nf90mpi_put_var(ncid, var_acctime_rnof, acctime_rnof)
         CALL pnetcdf_check(ierr, 'write acctime_rnof', file_restart)
      ENDIF

      IF (write_reservoir) THEN
         CALL pnetcdf_write_real8_points(ncid, var_volresv, resv_global_index, volresv, numresv, totalnumresv, &
            'volresv', file_restart)
      ENDIF

      ierr = nf90mpi_end_indep_data(ncid)
      CALL pnetcdf_check(ierr, 'end independent data mode', file_restart)

      ierr = nf90mpi_close(ncid)
      CALL pnetcdf_check(ierr, 'close', file_restart)

   END SUBROUTINE write_gridriver_restart_mpas_embedded

   SUBROUTINE validate_GridRiverRestart(file_restart, checkpoint_family_id, patch_file, &
                                        pft_file, gridriver_file)

   USE netcdf, only: nf90_open, nf90_nowrite, nf90_global, nf90_get_att, nf90_close, nf90_noerr
   USE MOD_MPAS_MPI, only: CoLM_stop
   IMPLICIT NONE

   character(len=*), intent(in) :: file_restart
   character(len=*), intent(in) :: checkpoint_family_id
   character(len=*), intent(in) :: patch_file, pft_file, gridriver_file

   integer :: ierr
   integer :: ncid
   character(len=2048) :: actual_family_id, actual_role
   character(len=2048) :: actual_patch_file, actual_pft_file, actual_gridriver_file
   character(len=2048) :: expected_patch_file, expected_pft_file, expected_gridriver_file

      CALL gridriver_restart_basename(patch_file, expected_patch_file)
      CALL gridriver_restart_basename(pft_file, expected_pft_file)
      CALL gridriver_restart_basename(gridriver_file, expected_gridriver_file)

      ierr = nf90_open(trim(file_restart), nf90_nowrite, ncid)
      IF (ierr /= nf90_noerr) CALL CoLM_stop('Cannot open embedded CoLM river restart family member: '//trim(file_restart))
      ierr = nf90_get_att(ncid, nf90_global, 'colm_checkpoint_family_id', actual_family_id)
      IF (ierr /= nf90_noerr) CALL CoLM_stop('Embedded CoLM river restart is missing its checkpoint family ID.')
      ierr = nf90_get_att(ncid, nf90_global, 'colm_checkpoint_role', actual_role)
      IF (ierr /= nf90_noerr) CALL CoLM_stop('Embedded CoLM river restart is missing its checkpoint role.')
      ierr = nf90_get_att(ncid, nf90_global, 'colm_checkpoint_patch_file', actual_patch_file)
      IF (ierr /= nf90_noerr) CALL CoLM_stop('Embedded CoLM river restart is missing its patch-file manifest.')
      ierr = nf90_get_att(ncid, nf90_global, 'colm_checkpoint_pft_file', actual_pft_file)
      IF (ierr /= nf90_noerr) CALL CoLM_stop('Embedded CoLM river restart is missing its PFT-file manifest.')
      ierr = nf90_get_att(ncid, nf90_global, 'colm_checkpoint_gridriver_file', actual_gridriver_file)
      IF (ierr /= nf90_noerr) CALL CoLM_stop('Embedded CoLM river restart is missing its grid-river-file manifest.')
      ierr = nf90_close(ncid)
      IF (ierr /= nf90_noerr) CALL CoLM_stop('Cannot close embedded CoLM river restart family member: '//trim(file_restart))

      IF (trim(actual_family_id) /= trim(checkpoint_family_id)) THEN
         CALL CoLM_stop('Embedded CoLM river restart belongs to a different checkpoint generation.')
      ENDIF
      IF (trim(actual_role) /= 'gridriver') THEN
         CALL CoLM_stop('Embedded CoLM river restart has the wrong checkpoint family role.')
      ENDIF
      IF (trim(actual_patch_file) /= trim(expected_patch_file) .or. &
          trim(actual_pft_file) /= trim(expected_pft_file) .or. &
          trim(actual_gridriver_file) /= trim(expected_gridriver_file)) THEN
         CALL CoLM_stop('Embedded CoLM river restart checkpoint family manifest does not match the requested family.')
      ENDIF

   END SUBROUTINE validate_GridRiverRestart

   SUBROUTINE gridriver_restart_basename(filename, basename)

   IMPLICIT NONE

   character(len=*), intent(in) :: filename
   character(len=*), intent(out) :: basename
   integer :: separator

      basename = ''
      IF (trim(filename) == 'NONE') THEN
         basename = 'NONE'
         RETURN
      ENDIF
      separator = max(scan(trim(filename), '/', back=.true.), &
                      scan(trim(filename), achar(92), back=.true.))
      IF (separator >= len_trim(filename)) RETURN
      basename = filename(separator+1:len_trim(filename))

   END SUBROUTINE gridriver_restart_basename

   SUBROUTINE pnetcdf_write_real8_points(ncid, varid, index, data, ndata, global_size, varname, filename)

   USE mpi, only: MPI_OFFSET_KIND
   USE pnetcdf, only: nf90mpi_put_var
   USE MOD_MPAS_MPI, only: CoLM_stop
   USE, intrinsic :: ieee_arithmetic, only: ieee_is_finite
   IMPLICIT NONE

   integer, intent(in) :: ncid
   integer, intent(in) :: varid
   integer, intent(in) :: index(:)
   real(r8), intent(inout) :: data(:)
   integer, intent(in) :: ndata
   integer, intent(in) :: global_size
   character(len=*), intent(in) :: varname
   character(len=*), intent(in) :: filename

   integer :: ierr
   integer :: i
   integer :: iend
   integer(kind=MPI_OFFSET_KIND) :: start(1)
   integer(kind=MPI_OFFSET_KIND) :: count(1)

      IF (ndata < 0 .or. ndata > size(index) .or. ndata > size(data)) THEN
         CALL CoLM_stop('PnetCDF indexed write size mismatch for '//trim(varname))
      ENDIF

      DO i = 1, ndata
         IF (index(i) < 1 .or. index(i) > global_size) THEN
            CALL CoLM_stop('PnetCDF indexed write invalid index for '//trim(varname))
         ENDIF
         IF (.not. ieee_is_finite(data(i))) THEN
            CALL CoLM_stop('PnetCDF indexed write contains non-finite data for '//trim(varname))
         ENDIF
      ENDDO

      i = 1
      DO WHILE (i <= ndata)
         iend = i
         DO WHILE (iend < ndata)
            IF (index(iend + 1) /= index(iend) + 1) EXIT
            iend = iend + 1
         ENDDO
         start(1) = int(index(i), MPI_OFFSET_KIND)
         count(1) = int(iend - i + 1, MPI_OFFSET_KIND)
         ierr = nf90mpi_put_var(ncid, varid, data(i:iend), start=start, count=count)
         CALL pnetcdf_check(ierr, 'write '//trim(varname), filename)
         i = iend + 1
      ENDDO

   END SUBROUTINE pnetcdf_write_real8_points

   SUBROUTINE validate_gridriver_indices(index, ndata, global_size, label)

   USE MOD_MPAS_MPI, only: CoLM_stop
   IMPLICIT NONE

   integer, intent(in) :: index(:)
   integer, intent(in) :: ndata
   integer, intent(in) :: global_size
   character(len=*), intent(in) :: label

      IF (ndata < 0 .or. ndata > size(index)) THEN
         CALL CoLM_stop('Invalid local '//trim(label)//' index count in embedded CoLM river routing.')
      ENDIF
      IF (global_size < 0) THEN
         CALL CoLM_stop('Invalid global '//trim(label)//' count in embedded CoLM river routing.')
      ENDIF
      IF (ndata > 0) THEN
         IF (global_size < 1 .or. any(index(1:ndata) < 1) .or. any(index(1:ndata) > global_size)) THEN
            CALL CoLM_stop('Out-of-range '//trim(label)//' index in embedded CoLM river routing.')
         ENDIF
      ENDIF
      IF (ndata > 1) THEN
         IF (any(index(2:ndata) <= index(1:ndata-1))) THEN
            CALL CoLM_stop('Duplicate or unordered '//trim(label)//' indices in embedded CoLM river routing.')
         ENDIF
      ENDIF

   END SUBROUTINE validate_gridriver_indices

   SUBROUTINE pnetcdf_check(status, action, filename)

   USE pnetcdf, only: NF90_NOERR, nf90mpi_strerror
   USE MOD_MPAS_MPI, only: CoLM_stop
   IMPLICIT NONE

   integer, intent(in) :: status
   character(len=*), intent(in) :: action
   character(len=*), intent(in) :: filename

      IF (status /= NF90_NOERR) THEN
         write(*,'(A)') 'PnetCDF error during '//trim(action)//' for '//trim(filename)//': ' &
            //trim(nf90mpi_strerror(status))
         CALL CoLM_stop()
      ENDIF

   END SUBROUTINE pnetcdf_check

   SUBROUTINE update_GridRiverLakeElementDiagnostics()

   IMPLICIT NONE

      CALL map_GridRiverLakeUcatToElement(wdsrf_ucat, river_water_depth_elm)
      CALL map_GridRiverLakeUcatToElement(veloc_riv, river_velocity_elm)
      CALL map_GridRiverLakeUcatToElement(discharge_riv, river_discharge_elm)

   END SUBROUTINE update_GridRiverLakeElementDiagnostics

   SUBROUTINE map_GridRiverLakeUcatToElement(ucat_data, element_data)

   USE MOD_ComputePushData, only: compute_push_data, compute_remap_data_grid2pset
   USE MOD_Grid_RiverLakeNetwork, only: numucat, numinpm, push_ucat2inpm, remap_patch2inpm
   USE MOD_LandPatch, only: numpatch, elm_patch
   USE MOD_Mesh, only: numelm
   USE MOD_MPAS_MPI, only: CoLM_stop
   USE MOD_Vars_Global, only: spval
   USE, INTRINSIC :: ieee_arithmetic, only: ieee_is_finite
   IMPLICIT NONE

   real(r8), intent(in) :: ucat_data(:)
   real(r8), intent(inout) :: element_data(:)

   real(r8), allocatable :: grid_data(:)
   real(r8), allocatable :: patch_data(:)
   real(r8) :: weight
   real(r8) :: weight_sum
   integer :: element
   integer :: patch
   integer :: patch_start
   integer :: patch_end

      IF (size(ucat_data) /= numucat .or. size(element_data) /= numelm) THEN
         CALL CoLM_stop('Embedded CoLM river diagnostic dimensions do not match the local decomposition.')
      ENDIF
      IF (.not. allocated(elm_patch%substt) .or. .not. allocated(elm_patch%subend) .or. &
          .not. allocated(elm_patch%subfrc)) THEN
         CALL CoLM_stop('Embedded CoLM river diagnostics require a complete element-patch map.')
      ENDIF
      IF (size(elm_patch%substt) < numelm .or. size(elm_patch%subend) < numelm .or. &
          size(elm_patch%subfrc) < numpatch) THEN
         CALL CoLM_stop('Embedded CoLM river diagnostic element-patch map has inconsistent dimensions.')
      ENDIF
      IF (numucat > 0) THEN
         IF (.not. all(ieee_is_finite(ucat_data))) THEN
            CALL CoLM_stop('Embedded CoLM river diagnostics contain non-finite unit-catchment values.')
         ENDIF
      ENDIF

      allocate(grid_data(numinpm))
      allocate(patch_data(numpatch))
      grid_data = spval
      patch_data = spval
      element_data = spval

      CALL compute_push_data(push_ucat2inpm, ucat_data, grid_data, fillvalue=spval, mode='average')
      CALL compute_remap_data_grid2pset(remap_patch2inpm, grid_data, patch_data, fillvalue=spval, mode='average')

      DO element = 1, numelm
         patch_start = elm_patch%substt(element)
         patch_end = elm_patch%subend(element)
         IF (patch_start < 1 .or. patch_end < patch_start .or. patch_end > numpatch) THEN
            CALL CoLM_stop('Embedded CoLM river diagnostic encountered an invalid element-patch range.')
         ENDIF

         element_data(element) = 0._r8
         weight_sum = 0._r8
         DO patch = patch_start, patch_end
            weight = elm_patch%subfrc(patch)
            IF (.not. ieee_is_finite(weight) .or. weight < 0._r8) THEN
               CALL CoLM_stop('Embedded CoLM river diagnostic encountered an invalid patch fraction.')
            ENDIF
            IF (weight > 0._r8 .and. ieee_is_finite(patch_data(patch)) .and. &
                abs(patch_data(patch)) < 0.5_r8 * abs(spval)) THEN
               element_data(element) = element_data(element) + weight * patch_data(patch)
               weight_sum = weight_sum + weight
            ENDIF
         ENDDO
         IF (weight_sum > 0._r8) THEN
            element_data(element) = element_data(element) / weight_sum
         ELSE
            element_data(element) = spval
         ENDIF
      ENDDO

      deallocate(grid_data)
      deallocate(patch_data)

   END SUBROUTINE map_GridRiverLakeUcatToElement

   SUBROUTINE deallocate_GridRiverLakeTimeVars

   IMPLICIT NONE

      IF (allocated (wdsrf_ucat)) deallocate (wdsrf_ucat)
      IF (allocated (veloc_riv )) deallocate (veloc_riv )
      IF (allocated (discharge_riv)) deallocate (discharge_riv)
      IF (allocated (momen_riv )) deallocate (momen_riv )
      IF (allocated (volresv   )) deallocate (volresv   )
      IF (allocated (acc_rnof_uc)) deallocate (acc_rnof_uc)
      IF (allocated (river_water_depth_elm)) deallocate (river_water_depth_elm)
      IF (allocated (river_velocity_elm   )) deallocate (river_velocity_elm   )
      IF (allocated (river_discharge_elm  )) deallocate (river_discharge_elm  )
      acctime_rnof = 0._r8
      gridriver_restart_file = ''

   END SUBROUTINE deallocate_GridRiverLakeTimeVars

END MODULE MOD_Grid_RiverLakeTimeVars
#endif
