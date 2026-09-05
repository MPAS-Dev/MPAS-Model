#include <define.h>

MODULE MOD_Opt_Baseflow

   USE MOD_Precision
   USE MOD_MPAS_MPI
   USE MOD_Namelist
   USE MOD_Vars_Global, only: spval
   IMPLICIT NONE

   real(r8), allocatable :: scale_baseflow (:)

   real(r8), allocatable :: zwt_init  (:)
   real(r8), allocatable :: rchg_year (:)
   real(r8), allocatable :: rsub_year (:)

   integer :: iter_bf_opt

CONTAINS

   ! -----
   SUBROUTINE Opt_Baseflow_init ()

   USE MOD_NetCDFVector
   USE MOD_Vars_TimeVariables, only: zwt
   USE MOD_LandPatch,          only: numpatch, landpatch
	   USE MOD_Utils,              only: make_directory
	   USE, INTRINSIC :: ieee_arithmetic, only: ieee_is_finite
   IMPLICIT NONE

   ! Local Variables
   character(len=:), allocatable :: file_restart


      file_restart = trim(DEF_dir_restart) // '/ParaOpt/' // trim(DEF_CASE_NAME) //'_baseflow.nc'
      CALL ncio_read_vector (file_restart, 'scale_baseflow', landpatch, scale_baseflow, defval = 1.)
      IF (numpatch > 0) THEN
         IF (.not. allocated(scale_baseflow)) THEN
            CALL CoLM_stop('Embedded CoLM baseflow scale is not allocated.')
         ENDIF
         IF (size(scale_baseflow) /= numpatch) THEN
            CALL CoLM_stop('Embedded CoLM baseflow scale does not match the local patch decomposition.')
         ENDIF
         IF (.not. all(ieee_is_finite(scale_baseflow)) .or. any(scale_baseflow <= 0._r8)) THEN
            CALL CoLM_stop('Embedded CoLM baseflow scale must contain finite positive values.')
         ENDIF
      ENDIF

      IF (mpas_is_root) CALL make_directory(trim(DEF_dir_restart)//'/ParaOpt')
#ifdef MPAS_MPI
      CALL mpi_barrier (mpas_comm, mpas_mpi_ierr)
	      CALL mpas_mpi_check('baseflow-scale initialization')
#endif

      IF (DEF_Optimize_Baseflow) THEN

         IF (.true.) THEN
            IF (numpatch > 0) THEN
               allocate (zwt_init  (numpatch));  zwt_init  (:) = zwt
               allocate (rchg_year (numpatch));  rchg_year (:) = spval
               allocate (rsub_year (numpatch));  rsub_year (:) = spval
            ENDIF
         ENDIF

         iter_bf_opt = 0

      ENDIF

   END SUBROUTINE Opt_Baseflow_init

   ! -----
   SUBROUTINE BaseFlow_Optimize (idate, deltim, is_spinup)

   USE MOD_TimeManager
   USE MOD_NetCDFVector
   USE MOD_Vars_TimeInvariants, only: patchtype
   USE MOD_Vars_TimeVariables,  only: wice_soisno, zwt
   USE MOD_Vars_1DForcing,      only: forc_prc, forc_prl
   USE MOD_Vars_1DFluxes,       only: fevpa, rsur, rsub
   USE MOD_LandPatch,           only: numpatch, landpatch
   USE MOD_Utils,               only: make_directory
   IMPLICIT NONE

   integer,  intent(in) :: idate(3)
   real(r8), intent(in) :: deltim
   logical,  intent(in) :: is_spinup

   ! Local Variables
   real(r8), allocatable :: recharge(:)
   integer  :: ipatch
   character(len=:), allocatable :: file_restart
   character(len=5)   :: strcyc

      IF (DEF_Optimize_Baseflow .and. is_spinup) THEN

         IF (.true.) THEN
            IF (numpatch > 0) THEN

               allocate (recharge(numpatch));   recharge(:) = spval
               WHERE ((forc_prc /= spval) .and. (forc_prl /= spval) .and. (fevpa /= spval) .and. (rsur /= spval))
                  recharge = forc_prc + forc_prl - fevpa - rsur
               END WHERE

               CALL add_spv (recharge, rchg_year, deltim)

               deallocate (recharge)

               CALL add_spv (rsub, rsub_year, deltim)

            ENDIF
         ENDIF

         IF (isendofyear(idate,deltim)) THEN

            iter_bf_opt = iter_bf_opt + 1

            write(strcyc,'(A1,I4.4)') 'c', iter_bf_opt
            IF (mpas_is_root) CALL make_directory(trim(DEF_dir_restart)//'/ParaOpt/'//strcyc)
#ifdef MPAS_MPI
            CALL mpi_barrier (mpas_comm, mpas_mpi_ierr)
#endif
            file_restart = trim(DEF_dir_restart)//'/ParaOpt/'//strcyc//'/'//trim(DEF_CASE_NAME)//'_baseflow.nc'
            CALL ncio_create_file_vector (file_restart, landpatch)
            CALL ncio_define_dimension_vector (file_restart, landpatch, 'patch')
            CALL ncio_write_vector (file_restart, 'zwt', 'patch', landpatch, zwt, 1)
            CALL ncio_write_vector (file_restart, 'zwt_init', 'patch', landpatch, zwt_init, 1)
            CALL ncio_write_vector (file_restart, 'scale_baseflow', 'patch', landpatch, scale_baseflow, 1)
            CALL ncio_write_vector (file_restart, 'total_recharge', 'patch', landpatch, rchg_year, 1)
            CALL ncio_write_vector (file_restart, 'total_subsurface_runoff', 'patch', landpatch, rsub_year, 1)

            IF (.true.) THEN
               DO ipatch = 1, numpatch
                  IF (patchtype(ipatch) <= 1) THEN

                     IF ((rchg_year(ipatch) > 0) .and. (rsub_year(ipatch) > 0)) THEN

                        IF ((zwt(ipatch) > zwt_init(ipatch)) .and. (rchg_year(ipatch) < rsub_year(ipatch))) THEN

                           scale_baseflow(ipatch) = rchg_year(ipatch)/rsub_year(ipatch) * scale_baseflow(ipatch)

                        ENDIF

                        IF ((zwt(ipatch) < zwt_init(ipatch)) .and. (rchg_year(ipatch) > rsub_year(ipatch))) THEN

                           scale_baseflow(ipatch) = rchg_year(ipatch)/rsub_year(ipatch) * scale_baseflow(ipatch)

                        ENDIF

                     ENDIF

                     scale_baseflow(ipatch) = max(1.e-8, scale_baseflow(ipatch))

                  ENDIF
               ENDDO
            ENDIF

            file_restart = trim(DEF_dir_restart) // '/ParaOpt/' // trim(DEF_CASE_NAME) //'_baseflow.nc'
            CALL ncio_create_file_vector (file_restart, landpatch)
            CALL ncio_define_dimension_vector (file_restart, landpatch, 'patch')
            CALL ncio_write_vector (file_restart, 'scale_baseflow', 'patch', landpatch, scale_baseflow, 1)

            IF (.true.) THEN
               IF (numpatch > 0) THEN
                  rchg_year(:) = spval
                  rsub_year(:) = spval
               ENDIF
            ENDIF

         ENDIF

      ENDIF

   END SUBROUTINE BaseFlow_Optimize

   ! -----
   SUBROUTINE Opt_Baseflow_final ()

   IMPLICIT NONE

      IF (allocated(scale_baseflow))  deallocate(scale_baseflow)
      IF (allocated(zwt_init      ))  deallocate(zwt_init      )
      IF (allocated(rchg_year     ))  deallocate(rchg_year     )
      IF (allocated(rsub_year     ))  deallocate(rsub_year     )

   END SUBROUTINE Opt_Baseflow_final

   !-----------------------------------------------------------------------
   SUBROUTINE add_spv (var, s, dt)

   USE MOD_Precision

   IMPLICIT NONE

   real(r8), intent(in)    :: var(:)
   real(r8), intent(inout) :: s  (:)
   real(r8), intent(in), optional :: dt
   ! Local variables
   integer :: i

      IF (present(dt)) THEN
         DO i = lbound(var,1), ubound(var,1)
            IF (var(i) /= spval) THEN
               IF (s(i) /= spval) THEN
                  s(i) = s(i) + var(i)*dt
               ELSE
                  s(i) = var(i)*dt
               ENDIF
            ENDIF
         ENDDO
      ELSE
         DO i = lbound(var,1), ubound(var,1)
            IF (var(i) /= spval) THEN
               IF (s(i) /= spval) THEN
                  s(i) = s(i) + var(i)
               ELSE
                  s(i) = var(i)
               ENDIF
            ENDIF
         ENDDO
      ENDIF

   END SUBROUTINE add_spv
END MODULE MOD_Opt_Baseflow
