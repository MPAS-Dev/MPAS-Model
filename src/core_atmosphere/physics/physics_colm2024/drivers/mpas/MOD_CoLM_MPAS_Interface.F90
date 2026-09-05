#include <define.h>

MODULE MOD_CoLM_MPAS_Interface

   USE MOD_Precision
   USE, INTRINSIC :: ieee_arithmetic, only: ieee_is_finite
   USE MOD_LandPatch, only: numpatch, landpatch, elm_patch
   USE MOD_Vars_Global, only: spval, nl_soil, dz_soi, pi
   USE MOD_Vars_1DForcing, only: forc_pco2m, forc_po2m, forc_us, forc_vs, forc_t, forc_q, &
      forc_prc, forc_prl, forc_rain, forc_snow, forc_psrf, forc_pbot, forc_sols, forc_soll, &
      forc_solsd, forc_solld, forc_frl, forc_swrad, forc_hgt_u, forc_hgt_t, forc_hgt_q, &
      forc_rhoair, forc_ozone, forc_hpbl, forc_aerdep
#ifdef HYPERSPECTRAL
   USE MOD_Vars_1DForcing, only: forc_solarin
#endif
   USE MOD_Vars_1DFluxes, only: oroflag, fsena, lfevpa, fevpa, fgrnd, rnof, rsur, rsub
   USE MOD_Vars_TimeInvariants, only: patchmask, patchtype, htop
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
	   USE MOD_Vars_PFTimeInvariants, only: pftfrac
	   USE MOD_LandPFT, only: patch_pft_s, patch_pft_e
#endif
   USE MOD_Vars_TimeVariables, only: t_grnd, trad, tref, qref, qsfc, emis, z0m, displa, alb, &
	      ldew, scv, snowdp, fsno, lai, fveg, rstfacsun_out, rstfacsha_out, &
	      t_soisno, wliq_soisno, wice_soisno, coszen, zol, rib, ustar, fm, fh, fq
	   USE MOD_Vars_TimeVariables, only: native_u10m => diag_u10m, native_v10m => diag_v10m, &
	                                      native_chs2 => diag_chs2, native_cqs2 => diag_cqs2, &
	                                      native_cd10 => diag_cd10
	   USE MOD_Const_Physical, only: vonkar, denh2o, denice

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: colm_mpas_initialize_from_namelist
   PUBLIC :: colm_mpas_finalize
   PUBLIC :: colm_mpas_ready
   PUBLIC :: colm_mpas_set_element_forcing
   PUBLIC :: colm_mpas_step
   PUBLIC :: colm_mpas_force_restart
   PUBLIC :: colm_mpas_get_surface
   PUBLIC :: colm_mpas_get_element_surface
   PUBLIC :: colm_mpas_get_element_albedo
   PUBLIC :: colm_mpas_get_element_boundary_state
   PUBLIC :: colm_mpas_get_element_state
   PUBLIC :: colm_mpas_get_element_river_state

   logical, save :: colm_mpas_initialized = .false.
   character(len=256), save :: colm_mpas_casename = ''
   character(len=256), save :: colm_mpas_dir_landdata = ''
   character(len=1024), save :: colm_mpas_dir_restart = ''
   integer, save :: colm_mpas_lc_year = -1
   integer, save :: colm_mpas_last_lai_year = -1
   integer, save :: colm_mpas_last_lai_period = -1
   integer, save :: colm_mpas_last_idate(3) = -1
   integer, save :: colm_mpas_last_restart_idate(3) = -1
   logical, save :: colm_mpas_restart_ready = .false.

   INTERFACE
      SUBROUTINE CoLMDRIVER(idate,deltim,dolai,doalb,dosst,oro)
         USE MOD_Precision
         USE MOD_LandPatch, only: numpatch
         integer, intent(in) :: idate(3)
         real(r8), intent(in) :: deltim
         logical, intent(in) :: dolai, doalb, dosst
         real(r8), intent(inout) :: oro(numpatch)
      END SUBROUTINE CoLMDRIVER
   END INTERFACE

CONTAINS

   SUBROUTINE colm_mpas_initialize_from_namelist(nlfile, ierr, mpas_comm, mpas_cell_id, &
                                                mpas_cell_lat, mpas_cell_lon, mpas_cell_area, &
                                                mpas_cell_landmask, &
                                                n_mpas_cells, cell_to_element, mpas_start_idate, &
	                                                mpas_stop_idate, mpas_timestep, mpas_is_restart)
	      USE MOD_Namelist, only: read_namelist, DEF_CASE_NAME, DEF_dir_landdata, &
		         DEF_dir_restart, DEF_LC_YEAR, DEF_simulation_time, DEF_USE_SNICAR, &
	         DEF_file_snowoptics, DEF_file_snowaging, DEF_Reservoir_Method, &
		         DEF_WRST_FREQ, DEF_HIST_FREQ, DEF_HIST_WriteBack, DEF_LAI_MONTHLY, &
		         DEF_LAI_CHANGE_YEARLY
	      USE MOD_Vars_Global, only: Init_GlobalVars
      USE MOD_MPAS_MPI, only: mpas_mpi_attach, mpas_is_root
	      USE MOD_Const_LC, only: Init_LC_Const
#ifdef HYPERSPECTRAL
	      USE MOD_Const_PFT, only: Init_PFT_Const, rho_p, tau_p
#else
	      USE MOD_Const_PFT, only: Init_PFT_Const
#endif
      USE MOD_TimeManager, only: initimetype, monthday2julian, julian2monthday, adj2begin, adj2end
      USE MOD_Block, only: gblock
      USE MOD_Pixel, only: pixel
      USE MOD_Mesh, only: numelm
      USE MOD_LandElm, only: landelm
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
      USE MOD_LandPFT, only: landpft, numpft, map_patch_to_pft
#endif
      USE MOD_SrfdataRestart, only: mesh_load_from_file, pixelset_load_from_file
      USE MOD_Vars_TimeInvariants, only: allocate_TimeInvariants, READ_TimeInvariants
	      USE MOD_Vars_TimeVariables, only: allocate_TimeVariables, READ_TimeVariables
	      USE MOD_NetCDFVector, only: ncio_set_distributed_identity
	      USE MOD_Opt_Baseflow, only: Opt_Baseflow_init
	      USE MOD_Vars_1DForcing, only: allocate_1D_Forcing
	      USE MOD_Vars_1DFluxes, only: allocate_1D_Fluxes
#ifdef GridRiverLakeFlow
	      USE MOD_Grid_RiverLakeNetwork, only: build_riverlake_network
	      USE MOD_Grid_Reservoir, only: reservoir_init
	      USE MOD_Grid_RiverLakeFlow, only: grid_riverlake_flow_init
#endif
#ifdef HYPERSPECTRAL
	      USE MOD_SnowSnicar_HiRes, only: SnowAge_init, SnowOptics_init
	      USE MOD_HighRes_Parameters, only: flux_frac_init, leaf_property_init, &
         get_water_optical_properties
#else
      USE MOD_SnowSnicar, only: SnowAge_init, SnowOptics_init
#endif
      character(len=*), intent(in) :: nlfile
      integer, intent(out) :: ierr
      integer, intent(in) :: mpas_comm
      integer, intent(in) :: mpas_cell_id(:)
      real(r8), intent(in) :: mpas_cell_lat(:)
      real(r8), intent(in) :: mpas_cell_lon(:)
      real(r8), intent(in) :: mpas_cell_area(:)
      integer, intent(in) :: mpas_cell_landmask(:)
      integer, intent(in) :: n_mpas_cells
      integer, intent(out) :: cell_to_element(:)
      integer, intent(in) :: mpas_start_idate(3)
      integer, intent(in) :: mpas_stop_idate(3)
      real(r8), intent(in) :: mpas_timestep
	      logical, intent(in) :: mpas_is_restart

      character(len=256) :: casename
      character(len=256) :: dir_landdata
      character(len=1024) :: dir_restart
      integer :: lc_year
      integer :: sdate(3)
      integer :: jdate(3)
      integer :: s_julian
      integer :: p_julian
      integer :: n_mpas
      integer :: i
	      integer :: lai_month
	      integer :: lai_mday
      integer*8, allocatable :: mpas_cell_id_i8(:)
	      logical :: river_network_ready
	      logical :: time_invariants_ready
	      logical :: time_variables_ready
	      logical :: forcing_ready
	      logical :: fluxes_ready

      ierr = 1
      IF (colm_mpas_initialized) THEN
         ierr = 0
         RETURN
      ENDIF
	      river_network_ready = .false.
	      time_invariants_ready = .false.
	      time_variables_ready = .false.
	      forcing_ready = .false.
	      fluxes_ready = .false.

      CALL mpas_mpi_attach(mpas_comm)

	      CALL read_namelist(trim(nlfile))
	      ierr = 0
	      IF (.not. ieee_is_finite(mpas_timestep) .or. mpas_timestep <= 0._r8 .or. mpas_timestep > 3600._r8) THEN
	         IF (mpas_is_root) write(*,'(A)') 'CoLM2024 MPAS timestep must be finite and in (0, 3600] seconds.'
	         ierr = 1
	      ENDIF
	      CALL colm_mpas_sync_initialize_status(ierr, 'timestep validation')
	      IF (ierr /= 0) GO TO 900
	      DEF_simulation_time%timestep = mpas_timestep
	      DEF_WRST_FREQ = 'none'
	      CALL colm_mpas_check_embedded_io(ierr)
	      CALL colm_mpas_sync_initialize_status(ierr, 'embedded I/O option validation')
	      IF (ierr /= 0) GO TO 900

	      casename = DEF_CASE_NAME
      dir_landdata = DEF_dir_landdata
      dir_restart = DEF_dir_restart
      lc_year = DEF_LC_YEAR

      CALL initimetype(DEF_simulation_time%greenwich)
      CALL monthday2julian(DEF_simulation_time%start_year, DEF_simulation_time%start_month, &
                           DEF_simulation_time%start_day, s_julian)
      CALL monthday2julian(DEF_simulation_time%spinup_year, DEF_simulation_time%spinup_month, &
                           DEF_simulation_time%spinup_day, p_julian)
      sdate(1) = DEF_simulation_time%start_year
      sdate(2) = s_julian
      sdate(3) = DEF_simulation_time%start_sec

	      ierr = 0
	      IF (mpas_is_restart) THEN
	         sdate(:) = mpas_start_idate(:)
	      ELSEIF (any(sdate /= mpas_start_idate)) THEN
	         IF (mpas_is_root) THEN
	            write(*,'(A,3(I0,1X))') 'CoLM2024 namelist start timestamp (year, day-of-year, second): ', sdate
	            write(*,'(A,3(I0,1X))') 'MPAS start timestamp (year, day-of-year, second): ', mpas_start_idate
	            write(*,'(A)') 'Set DEF_simulation_time start values to the MPAS cold-start time.'
	         ENDIF
	         ierr = 1
	      ENDIF
	      CALL colm_mpas_sync_initialize_status(ierr, 'start-time validation')
	      IF (ierr /= 0) GO TO 900

	      ierr = 0
	      IF (DEF_simulation_time%spinup_repeat /= 1 .or. &
	          DEF_simulation_time%spinup_year > sdate(1) .or. &
	          (DEF_simulation_time%spinup_year == sdate(1) .and. p_julian > sdate(2)) .or. &
	          (DEF_simulation_time%spinup_year == sdate(1) .and. p_julian == sdate(2) .and. &
	           DEF_simulation_time%spinup_sec > sdate(3))) THEN
	         IF (mpas_is_root) THEN
	            write(*,'(A)') 'CoLM2024 standalone spinup cycles cannot be embedded inside the MPAS atmosphere clock.'
	            write(*,'(A)') 'Use spinup_repeat = 1 and a spinup endpoint no later than the MPAS start timestamp.'
	         ENDIF
	         ierr = 1
	      ENDIF
	      CALL colm_mpas_sync_initialize_status(ierr, 'spinup-clock validation')
	      IF (ierr /= 0) GO TO 900

      colm_mpas_casename = casename
      colm_mpas_dir_landdata = dir_landdata
      colm_mpas_dir_restart = dir_restart
      colm_mpas_lc_year = lc_year
      colm_mpas_last_lai_year = -1
      colm_mpas_last_lai_period = -1
      colm_mpas_last_idate(:) = -1
      colm_mpas_last_restart_idate(:) = -1

      CALL Init_GlobalVars
      CALL Init_LC_Const
      CALL Init_PFT_Const

      n_mpas = 0
      n_mpas = n_mpas_cells
      ierr = 0
      IF (n_mpas < 0) ierr = 1
      IF (size(mpas_cell_id) < max(n_mpas, 0)) ierr = 1
      IF (size(mpas_cell_lat) < max(n_mpas, 0)) ierr = 1
      IF (size(mpas_cell_lon) < max(n_mpas, 0)) ierr = 1
      IF (size(mpas_cell_area) < max(n_mpas, 0)) ierr = 1
      IF (size(mpas_cell_landmask) < max(n_mpas, 0)) ierr = 1
      IF (size(cell_to_element) < max(n_mpas, 0)) ierr = 1
      CALL colm_mpas_sync_initialize_status(ierr, 'MPAS cell-array validation')
      IF (ierr /= 0) GO TO 900

      allocate(mpas_cell_id_i8(n_mpas))
      DO i = 1, n_mpas
         mpas_cell_id_i8(i) = int(mpas_cell_id(i), 8)
      ENDDO

      CALL pixel%load_from_file(dir_landdata)
      CALL gblock%load_from_file(dir_landdata)

      CALL colm_mpas_claim_owned_blocks(dir_landdata, lc_year, mpas_cell_id_i8, mpas_cell_lat, &
                                         mpas_cell_lon, n_mpas, ierr)
      CALL colm_mpas_sync_initialize_status(ierr, 'owned-block discovery')
      IF (ierr /= 0) GO TO 900

      CALL mesh_load_from_file(dir_landdata, lc_year, subset_eindex=mpas_cell_id_i8)
      CALL pixelset_load_from_file(dir_landdata, 'landelm', landelm, numelm, lc_year, &
                                   subset_eindex=mpas_cell_id_i8)
      CALL pixelset_load_from_file(dir_landdata, 'landpatch', landpatch, numpatch, lc_year, &
                                   subset_eindex=mpas_cell_id_i8)

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
      CALL pixelset_load_from_file(dir_landdata, 'landpft', landpft, numpft, lc_year, &
                                   subset_eindex=mpas_cell_id_i8)
#endif

      IF (n_mpas > 0) THEN
         CALL colm_mpas_restrict_to_mpas_cells(mpas_cell_id, n_mpas, cell_to_element, ierr)
      ELSE
         ierr = 0
      ENDIF
      CALL colm_mpas_sync_initialize_status(ierr, 'MPAS cell restriction')
      IF (ierr /= 0) GO TO 900

      IF (n_mpas > 0) THEN
         CALL colm_mpas_validate_cell_geometry(mpas_cell_id, mpas_cell_lat, mpas_cell_lon, &
                                               mpas_cell_area, n_mpas, cell_to_element, ierr)
      ELSE
         ierr = 0
      ENDIF
      CALL colm_mpas_sync_initialize_status(ierr, 'MPAS/CoLM cell-geometry validation')
      IF (ierr /= 0) GO TO 900
	      CALL ncio_set_distributed_identity(casename, mpas_cell_id_i8, mpas_cell_lat(1:n_mpas), &
	                                         mpas_cell_lon(1:n_mpas), mpas_cell_area(1:n_mpas))

      CALL elm_patch%build(landelm, landpatch, use_frac = .true.)
      CALL colm_mpas_validate_element_patch_map(.false., ierr)
      CALL colm_mpas_sync_initialize_status(ierr, 'element/patch coverage validation')
      IF (ierr /= 0) GO TO 900

#ifdef GridRiverLakeFlow
      CALL colm_mpas_check_embedded_riverlake(ierr)
	      CALL colm_mpas_sync_initialize_status(ierr, 'embedded river-option validation')
	      IF (ierr /= 0) GO TO 900
      CALL build_riverlake_network()
	      river_network_ready = .true.
      IF (DEF_Reservoir_Method > 0) CALL reservoir_init()
#endif

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
      CALL map_patch_to_pft
#endif

      CALL adj2end(sdate)
      jdate = sdate
      CALL adj2begin(jdate)

	      CALL allocate_TimeInvariants()
	      time_invariants_ready = .true.
	      CALL READ_TimeInvariants(lc_year, casename, dir_restart)
	      CALL colm_mpas_validate_cell_surface_type(mpas_cell_id, mpas_cell_landmask, &
	                                                n_mpas, cell_to_element, ierr)
	      CALL colm_mpas_sync_initialize_status(ierr, 'MPAS/CoLM surface-type validation')
	      IF (ierr /= 0) GO TO 900
	      CALL colm_mpas_validate_element_patch_map(.true., ierr)
	      CALL colm_mpas_sync_initialize_status(ierr, 'active element/patch validation')
	      IF (ierr /= 0) GO TO 900
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
	      CALL colm_mpas_validate_pft_fractions(ierr)
	      CALL colm_mpas_sync_initialize_status(ierr, 'PFT coverage/fraction validation')
	      IF (ierr /= 0) GO TO 900
#endif
	      CALL allocate_TimeVariables()
	      time_variables_ready = .true.
	      CALL READ_TimeVariables(jdate, lc_year, casename, dir_restart, mpas_is_restart)
	      ! Soil hydrology always applies this factor. The embedded path reads a
	      ! calibrated vector when present and otherwise initializes every patch to 1.
	      CALL Opt_Baseflow_init()

	      IF (DEF_LAI_CHANGE_YEARLY) THEN
	         colm_mpas_last_lai_year = jdate(1)
	      ELSE
	         colm_mpas_last_lai_year = lc_year
	      ENDIF
	      IF (DEF_LAI_MONTHLY) THEN
	         CALL julian2monthday(jdate(1), jdate(2), lai_month, lai_mday)
	         colm_mpas_last_lai_period = lai_month
	      ELSE
	         colm_mpas_last_lai_period = 1 + (jdate(2) - 1) / 8
	      ENDIF

      IF (DEF_USE_SNICAR) THEN
         CALL SnowOptics_init(DEF_file_snowoptics)
         CALL SnowAge_init(DEF_file_snowaging)
      ENDIF

#ifdef HYPERSPECTRAL
      CALL flux_frac_init()
      CALL leaf_property_init(rho_p, tau_p)
      CALL get_water_optical_properties()
#endif

	      CALL allocate_1D_Forcing()
	      forcing_ready = .true.
	      CALL allocate_1D_Fluxes()
	      fluxes_ready = .true.

#ifdef GridRiverLakeFlow
	      CALL grid_riverlake_flow_init()
#endif

	      colm_mpas_last_idate(:) = sdate(:)
	      colm_mpas_restart_ready = .true.
	      colm_mpas_initialized = .true.
	      ierr = 0
	      RETURN

900   CONTINUE
	      CALL colm_mpas_cleanup_failed_initialize(river_network_ready, time_invariants_ready, &
	                                               time_variables_ready, forcing_ready, fluxes_ready)
	   END SUBROUTINE colm_mpas_initialize_from_namelist

	   SUBROUTINE colm_mpas_sync_initialize_status(ierr, stage)
	      USE mpi, only: MPI_Allreduce, MPI_INTEGER, MPI_MAX
	      USE MOD_MPAS_MPI, only: mpas_comm, mpas_is_root, mpas_mpi_ierr, mpas_mpi_check
	      integer, intent(inout) :: ierr
	      character(len=*), intent(in) :: stage
	      integer :: local_status
	      integer :: global_status

	      local_status = 0
	      IF (ierr /= 0) local_status = 1
	      CALL MPI_Allreduce(local_status, global_status, 1, MPI_INTEGER, MPI_MAX, &
	                         mpas_comm, mpas_mpi_ierr)
	      CALL mpas_mpi_check('CoLM initialization status reduction: '//trim(stage))
	      ierr = global_status
	      IF (global_status /= 0 .and. mpas_is_root) THEN
	         write(*,'(A)') 'CoLM2024 MPAS initialization failed during '//trim(stage)//'.'
	      ENDIF
	   END SUBROUTINE colm_mpas_sync_initialize_status

	   SUBROUTINE colm_mpas_cleanup_failed_initialize(river_network_ready, time_invariants_ready, &
	                                                   time_variables_ready, forcing_ready, fluxes_ready)
#ifdef GridRiverLakeFlow
	      USE MOD_Grid_RiverLakeFlow, only: grid_riverlake_flow_final
#endif
	      USE MOD_Vars_1DForcing, only: deallocate_1D_Forcing
	      USE MOD_Vars_1DFluxes, only: deallocate_1D_Fluxes
	      USE MOD_Vars_TimeVariables, only: deallocate_TimeVariables
	      USE MOD_Vars_TimeInvariants, only: deallocate_TimeInvariants
	      USE MOD_Opt_Baseflow, only: Opt_Baseflow_final
	      USE MOD_NetCDFVector, only: ncio_reset_distributed_validation
	      USE MOD_MPAS_MPI, only: mpas_mpi_detach
	      USE MOD_LandElm, only: landelm
	      USE MOD_LandPatch, only: patch2elm, grid_patch
	      USE MOD_Mesh, only: mesh_free_mem
	      USE MOD_Grid, only: grid_free_mem
	      USE MOD_Pixel, only: pixel
	      USE MOD_Block, only: gblock
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
	      USE MOD_LandPFT, only: landpft, numpft, pft2patch
#endif
	      logical, intent(in) :: river_network_ready
	      logical, intent(in) :: time_invariants_ready
	      logical, intent(in) :: time_variables_ready
	      logical, intent(in) :: forcing_ready
	      logical, intent(in) :: fluxes_ready

#ifdef GridRiverLakeFlow
	      IF (river_network_ready) CALL grid_riverlake_flow_final()
#endif
	      CALL Opt_Baseflow_final()
	      IF (fluxes_ready) CALL deallocate_1D_Fluxes()
	      IF (forcing_ready) CALL deallocate_1D_Forcing()
	      IF (time_variables_ready) CALL deallocate_TimeVariables()
	      IF (time_invariants_ready) CALL deallocate_TimeInvariants()

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
	      CALL landpft%forc_free_mem()
	      IF (allocated(patch_pft_s)) deallocate(patch_pft_s)
	      IF (allocated(patch_pft_e)) deallocate(patch_pft_e)
	      IF (allocated(pft2patch)) deallocate(pft2patch)
	      numpft = 0
#endif
	      IF (allocated(elm_patch%substt)) deallocate(elm_patch%substt)
	      IF (allocated(elm_patch%subend)) deallocate(elm_patch%subend)
	      IF (allocated(elm_patch%subfrc)) deallocate(elm_patch%subfrc)
	      IF (allocated(patch2elm%sup)) deallocate(patch2elm%sup)
	      CALL landpatch%forc_free_mem()
	      CALL landelm%forc_free_mem()
	      CALL grid_free_mem(grid_patch)
	      numpatch = 0
	      CALL mesh_free_mem()

	      IF (allocated(pixel%lat_s)) deallocate(pixel%lat_s)
	      IF (allocated(pixel%lat_n)) deallocate(pixel%lat_n)
	      IF (allocated(pixel%lon_w)) deallocate(pixel%lon_w)
	      IF (allocated(pixel%lon_e)) deallocate(pixel%lon_e)
	      pixel%nlon = 0
	      pixel%nlat = 0

	      IF (allocated(gblock%lat_s)) deallocate(gblock%lat_s)
	      IF (allocated(gblock%lat_n)) deallocate(gblock%lat_n)
	      IF (allocated(gblock%lon_w)) deallocate(gblock%lon_w)
	      IF (allocated(gblock%lon_e)) deallocate(gblock%lon_e)
	      IF (allocated(gblock%owner_rank)) deallocate(gblock%owner_rank)
	      IF (allocated(gblock%xblkme)) deallocate(gblock%xblkme)
	      IF (allocated(gblock%yblkme)) deallocate(gblock%yblkme)
	      gblock%nxblk = 0
	      gblock%nyblk = 0
	      gblock%nblkme = 0
	      CALL ncio_reset_distributed_validation()

	      CALL mpas_mpi_detach()
	      colm_mpas_initialized = .false.
	      colm_mpas_restart_ready = .false.
	      colm_mpas_casename = ''
	      colm_mpas_dir_landdata = ''
	      colm_mpas_dir_restart = ''
	      colm_mpas_lc_year = -1
	      colm_mpas_last_lai_year = -1
	      colm_mpas_last_lai_period = -1
	      colm_mpas_last_idate(:) = -1
	      colm_mpas_last_restart_idate(:) = -1
	   END SUBROUTINE colm_mpas_cleanup_failed_initialize

#ifdef GridRiverLakeFlow
	   SUBROUTINE colm_mpas_check_embedded_riverlake(ierr)
	      USE MOD_Namelist, only: DEF_USE_SEDIMENT, DEF_Reservoir_Method
	      USE MOD_MPAS_MPI, only: mpas_size, mpas_is_root
	      integer, intent(out) :: ierr

	      ierr = 0
	      IF (DEF_USE_SEDIMENT) THEN
	         IF (mpas_is_root) THEN
	            write(*,'(A)') 'CoLM2024 MPAS embedded mode does not support GridRiverLakeSediment yet.'
	            write(*,'(A)') 'Set DEF_USE_SEDIMENT = .false. until sediment routing is migrated to MPAS-owned rank decomposition.'
	         ENDIF
	         ierr = 1
	         RETURN
	      ENDIF
	      IF (DEF_Reservoir_Method < 0 .or. DEF_Reservoir_Method > 1) THEN
	         IF (mpas_is_root) THEN
	            write(*,'(A,I0)') 'Unsupported CoLM2024 embedded reservoir method: ', DEF_Reservoir_Method
	            write(*,'(A)') 'Use DEF_Reservoir_Method = 0 (off) or 1.'
	         ENDIF
	         ierr = 1
	         RETURN
	      ENDIF
	      IF (mpas_size > 1 .and. mpas_is_root) THEN
	         write(*,'(A)') 'CoLM2024 MPAS embedded GridRiverLakeFlow uses MPAS communicator ranks for distributed routing.'
	         write(*,'(A)') 'No replicated full-river-network fallback is used.'
	      ENDIF
		   END SUBROUTINE colm_mpas_check_embedded_riverlake
#endif

	   SUBROUTINE colm_mpas_check_embedded_io(ierr)
	      USE MOD_Namelist, only: DEF_HIST_FREQ, DEF_HIST_WriteBack, USE_SITE_HistWriteBack, &
	                              DEF_USE_SNICAR, DEF_Aerosol_Readin, DEF_USE_Forcing_Downscaling, &
	                              DEF_USE_Forcing_Downscaling_Simple, DEF_USE_ClimForcing_for_Spinup, &
	                              DEF_Optimize_Baseflow, DEF_CheckEquilibrium, DEF_DA_TWS, DEF_DA_SM, &
	                              DEF_DA_ENS_SM, DEF_DA_TWS_GRACE, DEF_DA_SM_SMAP, DEF_DA_SM_FY, &
	                              DEF_DA_SM_SYNOP
	      USE MOD_MPAS_MPI, only: mpas_is_root
	      integer, intent(out) :: ierr

	      ierr = 0
	      USE_SITE_HistWriteBack = .false.
	      IF ((trim(adjustl(DEF_HIST_FREQ)) /= 'none' .and. trim(adjustl(DEF_HIST_FREQ)) /= 'NONE') .or. &
	          DEF_HIST_WriteBack) THEN
	         IF (mpas_is_root) THEN
	            write(*,'(A)') 'CoLM2024 MPAS embedded mode currently writes fluxes through MPAS streams.'
	            write(*,'(A)') 'Disable CoLM DEF_HIST_FREQ/DEF_HIST_WriteBack; CoLM restart files remain supported for patch/PFT state.'
	            write(*,'(A)') 'CoLM USE_SITE_HistWriteBack is forced off in MPAS embedded mode.'
	         ENDIF
	         ierr = 1
	      ENDIF
	      IF (DEF_USE_SNICAR .and. DEF_Aerosol_Readin) THEN
	         IF (mpas_is_root) THEN
	            write(*,'(A)') 'CoLM2024 MPAS embedded mode does not yet receive the 14-component aerosol deposition forcing.'
	            write(*,'(A)') 'Set DEF_Aerosol_Readin = .false., or add an MPAS aerosol-deposition source before enabling it.'
	         ENDIF
	         ierr = 1
	      ENDIF
	      IF (DEF_USE_Forcing_Downscaling .or. DEF_USE_Forcing_Downscaling_Simple .or. &
	          DEF_USE_ClimForcing_for_Spinup) THEN
	         IF (mpas_is_root) THEN
	            write(*,'(A)') 'CoLM2024 MPAS embedded mode receives forcing directly from MPAS.'
	            write(*,'(A)') 'Disable CoLM forcing downscaling and climatological-forcing spinup options.'
	         ENDIF
	         ierr = 1
	      ENDIF
	      IF (DEF_Optimize_Baseflow) THEN
	         IF (mpas_is_root) THEN
	            write(*,'(A)') 'CoLM2024 MPAS embedded mode does not run the standalone annual baseflow optimizer.'
	            write(*,'(A)') 'Set DEF_Optimize_Baseflow = .false.; an existing scale_baseflow vector is still honored.'
	         ENDIF
	         ierr = 1
	      ENDIF
	      IF (DEF_CheckEquilibrium) THEN
	         IF (mpas_is_root) THEN
	            write(*,'(A)') 'CoLM2024 standalone spinup-equilibrium checking is unavailable under the MPAS clock.'
	            write(*,'(A)') 'Set DEF_CheckEquilibrium = .false. for MPAS embedded runs.'
	         ENDIF
	         ierr = 1
	      ENDIF
	      IF (DEF_DA_TWS .or. DEF_DA_SM .or. DEF_DA_ENS_SM .or. DEF_DA_TWS_GRACE .or. &
	          DEF_DA_SM_SMAP .or. DEF_DA_SM_FY .or. DEF_DA_SM_SYNOP) THEN
	         IF (mpas_is_root) THEN
	            write(*,'(A)') 'CoLM2024 standalone data-assimilation drivers are not part of the MPAS embedded coupling.'
	            write(*,'(A)') 'Disable all DEF_DA_* switches and perform atmospheric/land analysis through MPAS.'
	         ENDIF
	         ierr = 1
	      ENDIF
	   END SUBROUTINE colm_mpas_check_embedded_io

	   SUBROUTINE colm_mpas_claim_owned_blocks(dir_landdata, lc_year, mpas_cell_id, mpas_cell_lat, &
	                                            mpas_cell_lon, n_mpas_cells, ierr)
	      USE MOD_Block, only: gblock, get_filename_block
	      USE MOD_MPAS_MPI, only: mpas_rank
	      USE MOD_NetCDFSerial, only: ncio_read_serial
	      USE MOD_Utils, only: quicksort, find_in_sorted_list1, find_nearest_south, &
	                           find_nearest_west, normalize_longitude
	      character(len=*), intent(in) :: dir_landdata
	      integer, intent(in) :: lc_year
	      integer*8, intent(in) :: mpas_cell_id(:)
	      real(r8), intent(in) :: mpas_cell_lat(:)
	      real(r8), intent(in) :: mpas_cell_lon(:)
	      integer, intent(in) :: n_mpas_cells
	      integer, intent(out) :: ierr

	      logical, allocatable :: candidate_block(:,:)
	      logical, allocatable :: keep_block(:,:)
	      logical, allocatable :: found_element(:)
	      logical :: fexists
	      integer :: i
	      integer :: ie
	      integer :: iblk
	      integer :: jblk
	      integer :: iblkme
	      integer :: match
	      integer :: scan_pass
	      real(r8) :: lat_deg
	      real(r8) :: lon_deg
	      character(len=256) :: filename
	      character(len=256) :: fileblock
	      character(len=256) :: cyear
	      integer, allocatable :: order(:)
	      integer*8, allocatable :: elmindx(:)
	      integer*8, allocatable :: sorted_cell_id(:)

	      ierr = 1
	      IF (n_mpas_cells < 0) RETURN
	      IF (.not. allocated(gblock%owner_rank)) RETURN
	      IF (.not. allocated(gblock%lon_w)) RETURN
	      IF (.not. allocated(gblock%lat_s)) RETURN
	      IF (size(mpas_cell_id) < n_mpas_cells) RETURN
	      IF (size(mpas_cell_lat) < n_mpas_cells) RETURN
	      IF (size(mpas_cell_lon) < n_mpas_cells) RETURN

	      allocate(candidate_block(gblock%nxblk, gblock%nyblk))
	      allocate(keep_block(gblock%nxblk, gblock%nyblk))
	      candidate_block = .false.
	      keep_block = .false.

	      IF (n_mpas_cells == 0) THEN
	         gblock%owner_rank(:,:) = -1
	         IF (allocated(gblock%xblkme)) deallocate(gblock%xblkme)
	         IF (allocated(gblock%yblkme)) deallocate(gblock%yblkme)
	         gblock%nblkme = 0
	         deallocate(candidate_block, keep_block)
	         ierr = 0
	         RETURN
	      ENDIF

	      allocate(found_element(n_mpas_cells))
	      allocate(sorted_cell_id(n_mpas_cells))
	      found_element = .false.
	      sorted_cell_id = mpas_cell_id(1:n_mpas_cells)
	      DO i = 1, n_mpas_cells
	         IF (.not. ieee_is_finite(mpas_cell_lat(i)) .or. .not. ieee_is_finite(mpas_cell_lon(i))) THEN
	            deallocate(candidate_block, keep_block, found_element, sorted_cell_id)
	            RETURN
	         ENDIF
	         IF (abs(mpas_cell_lat(i)) > 0.5_r8 * pi + 1.e-12_r8 .or. &
	             abs(mpas_cell_lon(i)) > 4._r8 * pi) THEN
	            deallocate(candidate_block, keep_block, found_element, sorted_cell_id)
	            RETURN
	         ENDIF
	         lat_deg = mpas_cell_lat(i) * 180._r8 / pi
	         lon_deg = mpas_cell_lon(i) * 180._r8 / pi
	         CALL normalize_longitude(lon_deg)
	         iblk = find_nearest_west(lon_deg, gblock%nxblk, gblock%lon_w)
	         jblk = find_nearest_south(lat_deg, gblock%nyblk, gblock%lat_s)
	         candidate_block(iblk,jblk) = .true.
	      ENDDO

	      IF (n_mpas_cells > 1) THEN
	         allocate(order(n_mpas_cells))
	         order = (/ (i, i = 1, n_mpas_cells) /)
	         CALL quicksort(n_mpas_cells, sorted_cell_id, order)
	         deallocate(order)

	         DO i = 2, n_mpas_cells
	            IF (sorted_cell_id(i) == sorted_cell_id(i-1)) THEN
	               write(*,'(A,I0,A,I0)') 'CoLM2024 MPAS embedded duplicate cell/eindex on rank ', &
	                  mpas_rank, ': ', sorted_cell_id(i)
	               deallocate(candidate_block, keep_block, found_element, sorted_cell_id)
	               RETURN
	            ENDIF
	         ENDDO
	      ENDIF

	      write(cyear,'(i4.4)') lc_year
	      filename = trim(dir_landdata) // '/mesh/' // trim(cyear) // '/mesh.nc'

	      ! CoLM writes an element to the block containing most of its pixels, which
	      ! is usually the block containing the MPAS cell center but is not guaranteed
	      ! to be. Probe center blocks first, verify by exact eindex, then scan only
	      ! the remaining blocks when an irregular or boundary cell was not found.
	      DO scan_pass = 1, 2
	         IF (scan_pass == 2 .and. all(found_element)) EXIT
	         DO jblk = 1, gblock%nyblk
	            DO iblk = 1, gblock%nxblk
	               IF (scan_pass == 1 .and. .not. candidate_block(iblk,jblk)) CYCLE
	               IF (scan_pass == 2 .and. candidate_block(iblk,jblk)) CYCLE
	            CALL get_filename_block(filename, iblk, jblk, fileblock)
	            inquire(file=trim(fileblock), exist=fexists)
	            IF (.not. fexists) CYCLE

	            CALL ncio_read_serial(fileblock, 'elmindex', elmindx)
	            DO ie = 1, size(elmindx)
	               match = find_in_sorted_list1(elmindx(ie), n_mpas_cells, sorted_cell_id)
	               IF (match > 0) THEN
	                  IF (found_element(match)) THEN
	                     write(*,'(A,I0,A,I0)') 'CoLM2024 MPAS embedded mesh contains duplicate eindex on rank ', &
	                        mpas_rank, ': ', sorted_cell_id(match)
	                     deallocate(elmindx, candidate_block, keep_block, found_element, sorted_cell_id)
	                     RETURN
	                  ENDIF
	                  keep_block(iblk,jblk) = .true.
	                  found_element(match) = .true.
	               ENDIF
	            ENDDO
	            IF (allocated(elmindx)) deallocate(elmindx)
	            ENDDO
	         ENDDO
	      ENDDO

	      IF (count(found_element) /= n_mpas_cells) THEN
	         DO i = 1, n_mpas_cells
	            IF (.not. found_element(i)) THEN
	               write(*,'(A,I0,A,I0)') 'CoLM2024 MPAS embedded mesh is missing cell/eindex on rank ', &
	                  mpas_rank, ': ', sorted_cell_id(i)
	               EXIT
	            ENDIF
	         ENDDO
	         deallocate(candidate_block, keep_block, found_element, sorted_cell_id)
	         RETURN
	      ENDIF

	      gblock%owner_rank(:,:) = -1
	      WHERE (keep_block)
	         gblock%owner_rank = mpas_rank
	      END WHERE

	      IF (allocated(gblock%xblkme)) deallocate(gblock%xblkme)
	      IF (allocated(gblock%yblkme)) deallocate(gblock%yblkme)
	      gblock%nblkme = count(keep_block)
	      IF (gblock%nblkme < 1) THEN
	         write(*,'(A,I0)') 'CoLM2024 MPAS embedded found no local mesh blocks for rank ', mpas_rank
	         deallocate(candidate_block, keep_block, found_element, sorted_cell_id)
	         RETURN
	      ENDIF

	      allocate(gblock%xblkme(gblock%nblkme))
	      allocate(gblock%yblkme(gblock%nblkme))
	      iblkme = 0
	      DO iblk = 1, gblock%nxblk
	         DO jblk = 1, gblock%nyblk
	            IF (keep_block(iblk,jblk)) THEN
	               iblkme = iblkme + 1
	               gblock%xblkme(iblkme) = iblk
	               gblock%yblkme(iblkme) = jblk
	            ENDIF
	         ENDDO
	      ENDDO

	      deallocate(candidate_block, keep_block, found_element, sorted_cell_id)
	      ierr = 0
	   END SUBROUTINE colm_mpas_claim_owned_blocks

	   SUBROUTINE colm_mpas_finalize(ierr)
#ifdef GridRiverLakeFlow
	      USE MOD_Grid_RiverLakeFlow, only: grid_riverlake_flow_final
#endif
	      USE MOD_Vars_1DForcing, only: deallocate_1D_Forcing
	      USE MOD_Vars_1DFluxes, only: deallocate_1D_Fluxes
	      USE MOD_Vars_TimeVariables, only: deallocate_TimeVariables
	      USE MOD_Vars_TimeInvariants, only: deallocate_TimeInvariants
	      USE MOD_Opt_Baseflow, only: Opt_Baseflow_final
	      USE MOD_NetCDFVector, only: ncio_reset_distributed_validation
	      USE MOD_MPAS_MPI, only: mpas_mpi_detach
	      USE MOD_LandElm, only: landelm
	      USE MOD_LandPatch, only: patch2elm, grid_patch
	      USE MOD_Mesh, only: mesh_free_mem
	      USE MOD_Grid, only: grid_free_mem
	      USE MOD_Pixel, only: pixel
	      USE MOD_Block, only: gblock
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
	      USE MOD_LandPFT, only: landpft, numpft, pft2patch
#endif
	      integer, intent(out) :: ierr

	      ierr = 0
	      IF (.not. colm_mpas_initialized) RETURN

#ifdef GridRiverLakeFlow
	      CALL grid_riverlake_flow_final()
#endif
	      CALL Opt_Baseflow_final()
	      CALL deallocate_1D_Fluxes()
	      CALL deallocate_1D_Forcing()
	      CALL deallocate_TimeVariables()
	      CALL deallocate_TimeInvariants()

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
	      CALL landpft%forc_free_mem()
	      IF (allocated(patch_pft_s)) deallocate(patch_pft_s)
	      IF (allocated(patch_pft_e)) deallocate(patch_pft_e)
	      IF (allocated(pft2patch)) deallocate(pft2patch)
	      numpft = 0
#endif
	      IF (allocated(elm_patch%substt)) deallocate(elm_patch%substt)
	      IF (allocated(elm_patch%subend)) deallocate(elm_patch%subend)
	      IF (allocated(elm_patch%subfrc)) deallocate(elm_patch%subfrc)
	      IF (allocated(patch2elm%sup)) deallocate(patch2elm%sup)
	      CALL landpatch%forc_free_mem()
	      CALL landelm%forc_free_mem()
	      CALL grid_free_mem(grid_patch)
	      numpatch = 0
	      CALL mesh_free_mem()

	      IF (allocated(pixel%lat_s)) deallocate(pixel%lat_s)
	      IF (allocated(pixel%lat_n)) deallocate(pixel%lat_n)
	      IF (allocated(pixel%lon_w)) deallocate(pixel%lon_w)
	      IF (allocated(pixel%lon_e)) deallocate(pixel%lon_e)
	      pixel%nlon = 0
	      pixel%nlat = 0

	      IF (allocated(gblock%lat_s)) deallocate(gblock%lat_s)
	      IF (allocated(gblock%lat_n)) deallocate(gblock%lat_n)
	      IF (allocated(gblock%lon_w)) deallocate(gblock%lon_w)
	      IF (allocated(gblock%lon_e)) deallocate(gblock%lon_e)
	      IF (allocated(gblock%owner_rank)) deallocate(gblock%owner_rank)
	      IF (allocated(gblock%xblkme)) deallocate(gblock%xblkme)
	      IF (allocated(gblock%yblkme)) deallocate(gblock%yblkme)
	      gblock%nxblk = 0
	      gblock%nyblk = 0
	      gblock%nblkme = 0
	      CALL ncio_reset_distributed_validation()

	      CALL mpas_mpi_detach()
	      colm_mpas_initialized = .false.
	      colm_mpas_restart_ready = .false.
	      colm_mpas_casename = ''
	      colm_mpas_dir_landdata = ''
	      colm_mpas_dir_restart = ''
	      colm_mpas_lc_year = -1
	      colm_mpas_last_lai_year = -1
	      colm_mpas_last_lai_period = -1
	      colm_mpas_last_idate(:) = -1
	      colm_mpas_last_restart_idate(:) = -1
	   END SUBROUTINE colm_mpas_finalize

	   SUBROUTINE colm_mpas_restrict_to_mpas_cells(mpas_cell_id, n_mpas_cells, cell_to_element, ierr)
	      USE MOD_LandElm, only: landelm
	      USE MOD_LandPatch, only: landpatch, numpatch
	      USE MOD_Mesh, only: mesh, numelm
	      USE MOD_Utils, only: quicksort, find_in_sorted_list1
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
	      USE MOD_LandPFT, only: landpft, numpft
#endif
      integer, intent(in) :: mpas_cell_id(:)
      integer, intent(in) :: n_mpas_cells
      integer, intent(out) :: cell_to_element(:)
      integer, intent(out) :: ierr

	      logical, allocatable :: seen_element(:)
	      integer, allocatable :: element_order(:)
	      integer*8, allocatable :: sorted_eindex(:)
	      integer :: i
	      integer :: match
	      integer :: element

	      ierr = 1
	      IF (n_mpas_cells < 1) RETURN
	      IF (numelm /= n_mpas_cells) RETURN
	      IF (size(mpas_cell_id) < n_mpas_cells) RETURN
	      IF (size(cell_to_element) < n_mpas_cells) RETURN
	      IF (.not. allocated(mesh)) RETURN
	      IF (size(mesh) /= numelm) RETURN
	      IF (.not. allocated(landelm%ipxstt) .or. .not. allocated(landelm%ipxend)) RETURN
	      IF (size(landelm%ipxstt) /= landelm%nset .or. size(landelm%ipxend) /= landelm%nset) RETURN

	      CALL colm_mpas_validate_pixelset_element_map(landelm, numelm, ierr)
	      IF (ierr /= 0 .or. landelm%nset /= numelm) RETURN
	      IF (numpatch /= landpatch%nset) RETURN
	      CALL colm_mpas_validate_pixelset_element_map(landpatch, numelm, ierr)
	      IF (ierr /= 0) RETURN
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
	      IF (numpft /= landpft%nset) RETURN
	      CALL colm_mpas_validate_pixelset_element_map(landpft, numelm, ierr)
	      IF (ierr /= 0) RETURN
#endif

	      allocate(seen_element(numelm))
	      allocate(sorted_eindex(numelm))
	      allocate(element_order(numelm))

	      seen_element = .false.
	      DO i = 1, landelm%nset
	         element = landelm%ielm(i)
	         IF (seen_element(element)) RETURN
	         IF (landelm%ipxstt(i) /= 1 .or. landelm%ipxend(i) /= mesh(element)%npxl) RETURN
	         seen_element(element) = .true.
	      ENDDO
	      IF (.not. all(seen_element)) RETURN
	      seen_element = .false.
	      DO i = 1, numelm
	         sorted_eindex(i) = mesh(i)%indx
	      ENDDO
	      element_order = (/ (i, i = 1, numelm) /)
	      IF (numelm > 1) CALL quicksort(numelm, sorted_eindex, element_order)
	      IF (numelm > 1) THEN
	         IF (any(sorted_eindex(2:numelm) == sorted_eindex(1:numelm-1))) RETURN
	      ENDIF

	      DO i = 1, n_mpas_cells
	         match = find_in_sorted_list1(int(mpas_cell_id(i), kind=8), numelm, sorted_eindex)
	         IF (match < 1) RETURN
	         element = element_order(match)
	         IF (seen_element(element)) RETURN
	         seen_element(element) = .true.
	         cell_to_element(i) = element
	      ENDDO
	      IF (.not. all(seen_element)) RETURN

	      ierr = 0
	   END SUBROUTINE colm_mpas_restrict_to_mpas_cells

	   SUBROUTINE colm_mpas_validate_cell_geometry(mpas_cell_id, mpas_cell_lat, mpas_cell_lon, &
	                                                mpas_cell_area, n_mpas_cells, cell_to_element, ierr)
	      USE MOD_Mesh, only: mesh, numelm
	      USE MOD_Pixel, only: pixel
	      USE MOD_Utils, only: normalize_longitude, lon_between_floor, areaquad
	      USE MOD_MPAS_MPI, only: mpas_rank
	      integer, intent(in) :: mpas_cell_id(:)
	      real(r8), intent(in) :: mpas_cell_lat(:)
	      real(r8), intent(in) :: mpas_cell_lon(:)
	      real(r8), intent(in) :: mpas_cell_area(:)
	      integer, intent(in) :: n_mpas_cells
	      integer, intent(in) :: cell_to_element(:)
	      integer, intent(out) :: ierr

	      integer :: i
	      integer :: element
	      integer :: ipxl
	      integer :: ilat
	      integer :: ilon
	      real(r8) :: lat_deg
	      real(r8) :: lon_deg
	      real(r8) :: lon_east_delta
	      real(r8) :: colm_area
	      real(r8) :: relative_area_error
	      logical :: contains_center
	      real(r8), parameter :: angular_tolerance = 1.e-8_r8
	      real(r8), parameter :: area_tolerance = 0.05_r8

	      ierr = 1
	      IF (n_mpas_cells < 1) RETURN
	      IF (size(mpas_cell_id) < n_mpas_cells .or. size(mpas_cell_lat) < n_mpas_cells .or. &
	          size(mpas_cell_lon) < n_mpas_cells .or. size(mpas_cell_area) < n_mpas_cells .or. &
	          size(cell_to_element) < n_mpas_cells) RETURN
	      IF (.not. allocated(mesh)) RETURN
	      IF (size(mesh) /= numelm) RETURN
	      IF (.not. allocated(pixel%lat_s) .or. .not. allocated(pixel%lat_n) .or. &
	          .not. allocated(pixel%lon_w) .or. .not. allocated(pixel%lon_e)) RETURN

	      DO i = 1, n_mpas_cells
	         IF (.not. ieee_is_finite(mpas_cell_lat(i)) .or. .not. ieee_is_finite(mpas_cell_lon(i)) .or. &
	             .not. ieee_is_finite(mpas_cell_area(i)) .or. mpas_cell_area(i) <= 0._r8) RETURN
	         IF (abs(mpas_cell_lat(i)) > 0.5_r8 * pi + 1.e-12_r8 .or. &
	             abs(mpas_cell_lon(i)) > 4._r8 * pi) RETURN

	         element = cell_to_element(i)
	         IF (element < 1 .or. element > numelm) RETURN
	         IF (mesh(element)%indx /= int(mpas_cell_id(i), kind=8)) RETURN
	         IF (mesh(element)%npxl < 1) RETURN
	         IF (.not. allocated(mesh(element)%ilat) .or. .not. allocated(mesh(element)%ilon)) RETURN
	         IF (size(mesh(element)%ilat) /= mesh(element)%npxl .or. &
	             size(mesh(element)%ilon) /= mesh(element)%npxl) RETURN

	         lat_deg = mpas_cell_lat(i) * 180._r8 / pi
	         lon_deg = mpas_cell_lon(i) * 180._r8 / pi
	         CALL normalize_longitude(lon_deg)
	         contains_center = .false.
	         colm_area = 0._r8

	         DO ipxl = 1, mesh(element)%npxl
	            ilat = mesh(element)%ilat(ipxl)
	            ilon = mesh(element)%ilon(ipxl)
	            IF (ilat < 1 .or. ilat > pixel%nlat .or. ilon < 1 .or. ilon > pixel%nlon) RETURN
	            IF (.not. all(ieee_is_finite((/pixel%lat_s(ilat), pixel%lat_n(ilat), &
	                                            pixel%lon_w(ilon), pixel%lon_e(ilon)/)))) RETURN
	            colm_area = colm_area + 1.e6_r8 * areaquad(pixel%lat_s(ilat), pixel%lat_n(ilat), &
	                                                       pixel%lon_w(ilon), pixel%lon_e(ilon))

	            lon_east_delta = abs(modulo(lon_deg - pixel%lon_e(ilon) + 180._r8, 360._r8) - 180._r8)
	            IF (lat_deg >= pixel%lat_s(ilat) - angular_tolerance .and. &
	                lat_deg <= pixel%lat_n(ilat) + angular_tolerance .and. &
	                (lon_between_floor(lon_deg, pixel%lon_w(ilon), pixel%lon_e(ilon)) .or. &
	                 lon_east_delta <= angular_tolerance)) THEN
	               contains_center = .true.
	            ENDIF
	         ENDDO

	         IF (.not. contains_center) THEN
	            write(*,'(A,I0,A,I0,A,I0,A,2(F13.7,1X))') &
	               'CoLM2024 MPAS mesh mismatch on rank ', mpas_rank, ': local cell ', i, &
	               ', eindex ', mpas_cell_id(i), ', center lon/lat (degrees) ', lon_deg, lat_deg
	            RETURN
	         ENDIF
	         IF (.not. ieee_is_finite(colm_area) .or. colm_area <= 0._r8) RETURN
	         relative_area_error = abs(colm_area - mpas_cell_area(i)) / max(colm_area, mpas_cell_area(i))
	         IF (relative_area_error > area_tolerance) THEN
	            write(*,'(A,I0,A,I0,A,I0,A,2(ES14.6,1X),A,F8.3,A)') &
	               'CoLM2024 MPAS area mismatch on rank ', mpas_rank, ': local cell ', i, &
	               ', eindex ', mpas_cell_id(i), ', MPAS/CoLM area (m2) ', mpas_cell_area(i), colm_area, &
	               ', relative error ', 100._r8 * relative_area_error, '%'
	            RETURN
	         ENDIF
	      ENDDO

	      ierr = 0
	   END SUBROUTINE colm_mpas_validate_cell_geometry

	   SUBROUTINE colm_mpas_validate_cell_surface_type(mpas_cell_id, mpas_cell_landmask, &
	                                                    n_mpas_cells, cell_to_element, ierr)
	      USE MOD_MPAS_MPI, only: mpas_rank
	      integer, intent(in) :: mpas_cell_id(:)
	      integer, intent(in) :: mpas_cell_landmask(:)
	      integer, intent(in) :: n_mpas_cells
	      integer, intent(in) :: cell_to_element(:)
	      integer, intent(out) :: ierr

	      integer :: i
	      integer :: element
	      integer :: patch
	      integer :: istt
	      integer :: iend
	      real(r8) :: land_fraction
	      real(r8) :: water_fraction
	      real(r8) :: wt
	      logical :: mpas_is_land

	      ierr = 1
	      IF (n_mpas_cells < 0) RETURN
	      IF (n_mpas_cells == 0) THEN
	         ierr = 0
	         RETURN
	      ENDIF
	      IF (size(mpas_cell_id) < n_mpas_cells .or. size(mpas_cell_landmask) < n_mpas_cells .or. &
	          size(cell_to_element) < n_mpas_cells) RETURN
	      IF (.not. allocated(elm_patch%substt) .or. .not. allocated(elm_patch%subend) .or. &
	          .not. allocated(elm_patch%subfrc) .or. .not. allocated(patchtype)) RETURN

	      DO i = 1, n_mpas_cells
	         IF (mpas_cell_landmask(i) /= 0 .and. mpas_cell_landmask(i) /= 1) RETURN
	         element = cell_to_element(i)
	         IF (element < 1 .or. element > size(elm_patch%substt) .or. &
	             element > size(elm_patch%subend)) RETURN
	         istt = elm_patch%substt(element)
	         iend = elm_patch%subend(element)
	         IF (istt < 1 .or. iend < istt .or. iend > numpatch .or. &
	             iend > size(elm_patch%subfrc) .or. iend > size(patchtype)) RETURN

	         land_fraction = 0._r8
	         water_fraction = 0._r8
	         DO patch = istt, iend
	            IF (allocated(patchmask)) THEN
	               IF (patch > size(patchmask)) RETURN
	               IF (.not. patchmask(patch)) CYCLE
	            ENDIF
	            wt = elm_patch%subfrc(patch)
	            IF (.not. ieee_is_finite(wt) .or. wt < 0._r8) RETURN
	            IF (patchtype(patch) >= 99) THEN
	               water_fraction = water_fraction + wt
	            ELSE
	               land_fraction = land_fraction + wt
	            ENDIF
	         ENDDO
	         IF (land_fraction + water_fraction <= 0._r8) RETURN

	         mpas_is_land = mpas_cell_landmask(i) == 1
	         IF ((mpas_is_land .and. land_fraction < water_fraction) .or. &
	             (.not. mpas_is_land .and. water_fraction < land_fraction)) THEN
	            write(*,'(A,I0,A,I0,A,I0,A,I0,A,2(F8.5,1X))') &
	               'CoLM2024 MPAS surface-type mismatch on rank ', mpas_rank, ': local cell ', i, &
	               ', eindex ', mpas_cell_id(i), ', MPAS landmask ', mpas_cell_landmask(i), &
	               ', CoLM land/water fractions ', land_fraction, water_fraction
	            RETURN
	         ENDIF
	      ENDDO

	      ierr = 0
	   END SUBROUTINE colm_mpas_validate_cell_surface_type

	   SUBROUTINE colm_mpas_validate_pixelset_element_map(pixelset, num_elements, ierr)
	      USE MOD_Pixelset, only: pixelset_type
	      USE MOD_Mesh, only: mesh
	      type(pixelset_type), intent(in) :: pixelset
	      integer, intent(in) :: num_elements
	      integer, intent(out) :: ierr

	      integer :: iset
	      integer :: element

	      ierr = 1
	      IF (pixelset%nset < 0) RETURN
	      IF (pixelset%nset == 0) THEN
	         ierr = 0
	         RETURN
	      ENDIF
	      IF (.not. allocated(pixelset%eindex) .or. .not. allocated(pixelset%ielm)) RETURN
	      IF (size(pixelset%eindex) /= pixelset%nset .or. size(pixelset%ielm) /= pixelset%nset) RETURN
	      DO iset = 1, pixelset%nset
	         element = pixelset%ielm(iset)
	         IF (element < 1 .or. element > num_elements) RETURN
	         IF (pixelset%eindex(iset) /= mesh(element)%indx) RETURN
	      ENDDO

	      ierr = 0
	   END SUBROUTINE colm_mpas_validate_pixelset_element_map

   SUBROUTINE colm_mpas_validate_element_patch_map(require_active_patch, ierr)
      USE MOD_LandElm, only: landelm
      USE MOD_MPAS_MPI, only: mpas_rank
      logical, intent(in) :: require_active_patch
      integer, intent(out) :: ierr

      integer :: element
      integer :: patch
      integer :: istt
      integer :: iend
      integer :: local_missing
      integer :: first_missing
	      logical :: valid_coverage
	      real(r8) :: fraction
	      real(r8) :: total_fraction
	      real(r8) :: active_fraction
	      real(r8), parameter :: fraction_tolerance = 1.e-10_r8

      ierr = 1
      IF (.not. allocated(elm_patch%substt)) RETURN
      IF (.not. allocated(elm_patch%subend)) RETURN
      IF (.not. allocated(elm_patch%subfrc)) RETURN
      IF (size(elm_patch%substt) < landelm%nset) RETURN
      IF (size(elm_patch%subend) < landelm%nset) RETURN
      IF (size(elm_patch%subfrc) < numpatch) RETURN
      IF (require_active_patch .and. numpatch > 0 .and. .not. allocated(patchmask)) RETURN
      IF (require_active_patch .and. numpatch > 0) THEN
         IF (size(patchmask) < numpatch) RETURN
      ENDIF

      local_missing = 0
      first_missing = -1

	      DO element = 1, landelm%nset
	         istt = elm_patch%substt(element)
	         iend = elm_patch%subend(element)
	         valid_coverage = istt >= 1 .and. iend >= istt .and. iend <= numpatch
	         total_fraction = 0._r8
	         active_fraction = 0._r8

	         IF (valid_coverage) THEN
	            DO patch = istt, iend
	               fraction = elm_patch%subfrc(patch)
	               IF (.not. ieee_is_finite(fraction) .or. fraction < 0._r8) THEN
	                  valid_coverage = .false.
	                  EXIT
	               ENDIF
	               total_fraction = total_fraction + fraction
	               IF (.not. require_active_patch) THEN
	                  active_fraction = active_fraction + fraction
	               ELSEIF (patchmask(patch)) THEN
	                  active_fraction = active_fraction + fraction
	               ENDIF
	            ENDDO
	         ENDIF
	         IF (.not. ieee_is_finite(total_fraction) .or. &
	             abs(total_fraction - 1._r8) > fraction_tolerance) valid_coverage = .false.
	         IF (require_active_patch) THEN
	            IF (.not. ieee_is_finite(active_fraction) .or. &
	                abs(active_fraction - total_fraction) > fraction_tolerance) valid_coverage = .false.
	         ENDIF

	         IF (.not. valid_coverage) THEN
	            local_missing = local_missing + 1
	            IF (first_missing < 0) first_missing = element
         ENDIF
      ENDDO

      IF (local_missing > 0) THEN
         IF (first_missing > 0 .and. first_missing <= landelm%nset) THEN
            write(*,'(A,I0,A,I0,A,I0,A,I0)') &
	               'CoLM2024 MPAS embedded landdata has incomplete patch coverage on rank ', &
               mpas_rank, ': ', local_missing, ' element(s); first local element ', &
               first_missing, ', eindex ', landelm%eindex(first_missing)
         ELSE
            write(*,'(A,I0,A,I0)') &
	               'CoLM2024 MPAS embedded landdata has incomplete patch coverage on rank ', &
               mpas_rank, ': ', local_missing
         ENDIF
         RETURN
      ENDIF

      ierr = 0
	   END SUBROUTINE colm_mpas_validate_element_patch_map

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
	   SUBROUTINE colm_mpas_validate_pft_fractions(ierr)
	      USE MOD_LandPFT, only: numpft
	      USE MOD_MPAS_MPI, only: mpas_rank
	      integer, intent(out) :: ierr

	      integer :: patch
	      integer :: pft_start
	      integer :: pft_end
	      real(r8) :: fraction_sum
	      real(r8), parameter :: fraction_tolerance = 1.e-10_r8

	      ierr = 1
	      IF (numpatch == 0) THEN
	         IF (numpft /= 0) RETURN
	         ierr = 0
	         RETURN
	      ENDIF
	      IF (.not. allocated(patch_pft_s) .or. .not. allocated(patch_pft_e)) RETURN
	      IF (size(patch_pft_s) /= numpatch .or. size(patch_pft_e) /= numpatch) RETURN
	      IF (numpft > 0) THEN
	         IF (.not. allocated(pftfrac)) RETURN
	         IF (size(pftfrac) /= numpft) RETURN
	      ENDIF

	      DO patch = 1, numpatch
	         pft_start = patch_pft_s(patch)
	         pft_end = patch_pft_e(patch)
	         IF (pft_start == -1 .or. pft_end == -1) THEN
	            IF (pft_start /= -1 .or. pft_end /= -1) EXIT
	            ! Soil/vegetation patches enter Thermal's PFT reduction and
	            ! therefore must not have an empty PFT slice.  Non-vegetated
	            ! patch types legitimately carry no PFTs.
	            IF (patchtype(patch) == 0) EXIT
	         ELSE
	            IF (pft_start < 1 .or. pft_end < pft_start .or. pft_end > numpft) EXIT
	            IF (.not. all(ieee_is_finite(pftfrac(pft_start:pft_end)))) EXIT
	            IF (any(pftfrac(pft_start:pft_end) < 0._r8)) EXIT
	            fraction_sum = sum(pftfrac(pft_start:pft_end))
	            IF (.not. ieee_is_finite(fraction_sum) .or. &
	                abs(fraction_sum - 1._r8) > fraction_tolerance) EXIT
	         ENDIF
	      ENDDO

	      IF (patch <= numpatch) THEN
	         write(*,'(A,I0,A,I0,A,I0)') 'CoLM2024 MPAS embedded landdata has invalid PFT fractions on rank ', &
	            mpas_rank, ': local patch ', patch, ', eindex ', landpatch%eindex(patch)
	         RETURN
	      ENDIF

	      ierr = 0
	   END SUBROUTINE colm_mpas_validate_pft_fractions
#endif

	   SUBROUTINE colm_mpas_ready(ready, patch_count)
      logical, intent(out) :: ready
      integer, intent(out), optional :: patch_count

      IF (numpatch == 0) THEN
         ready = colm_mpas_initialized .and. allocated(elm_patch%substt) .and. &
            allocated(elm_patch%subend) .and. allocated(elm_patch%subfrc)
      ELSE
         ready = colm_mpas_initialized .and. allocated(forc_t) .and. allocated(oroflag) .and. &
            allocated(fsena) .and. allocated(t_grnd) .and. allocated(patchmask) &
            .and. allocated(elm_patch%substt) .and. allocated(elm_patch%subend) .and. allocated(elm_patch%subfrc)
      ENDIF
      IF (present(patch_count)) THEN
         IF (allocated(oroflag)) THEN
            patch_count = size(oroflag)
         ELSE
            patch_count = 0
         ENDIF
      ENDIF
   END SUBROUTINE colm_mpas_ready

   SUBROUTINE colm_mpas_set_forcing(patch, pco2m, po2m, us, vs, tair, qair, prc, prl, rain, snow, solar_cosine, &
                                    psrf, pbot, sols, soll, solsd, solld, frl, hgt_u, hgt_t, hgt_q, &
                                    rhoair, hpbl, aerdep, oro, ozone, ierr)
      integer, intent(in) :: patch
      real(r8), intent(in) :: pco2m, po2m, us, vs, tair, qair, prc, prl, rain, snow, solar_cosine
      real(r8), intent(in) :: psrf, pbot, sols, soll, solsd, solld, frl
      real(r8), intent(in) :: hgt_u, hgt_t, hgt_q, rhoair, hpbl
      real(r8), intent(in) :: aerdep(14)
      real(r8), intent(in), optional :: oro
      real(r8), intent(in), optional :: ozone
      integer, intent(out) :: ierr
      real(r8) :: patch_oro

      ierr = 1
      IF (.not. allocated(forc_t)) RETURN
      IF (.not. allocated(coszen)) RETURN
      IF (patch < 1 .or. patch > size(forc_t)) RETURN
	      IF (patch > size(coszen)) RETURN
	      IF (.not. ieee_is_finite(solar_cosine) .or. solar_cosine < -1._r8 .or. solar_cosine > 1._r8) RETURN
      IF (present(oro)) THEN
         IF (.not. allocated(oroflag) .or. .not. allocated(patchtype)) RETURN
         IF (patch > size(oroflag) .or. patch > size(patchtype)) RETURN
      ENDIF

      forc_pco2m(patch) = pco2m
      forc_po2m (patch) = po2m
      forc_us   (patch) = us
      forc_vs   (patch) = vs
      forc_t    (patch) = tair
      forc_q    (patch) = qair
      forc_prc  (patch) = prc
      forc_prl  (patch) = prl
      forc_rain (patch) = rain
      forc_snow (patch) = snow
	      coszen    (patch) = solar_cosine
      forc_psrf (patch) = psrf
      forc_pbot (patch) = pbot
      forc_sols (patch) = sols
      forc_soll (patch) = soll
      forc_solsd(patch) = solsd
      forc_solld(patch) = solld
      forc_frl  (patch) = frl
      forc_swrad(patch) = sols + soll + solsd + solld
#ifdef HYPERSPECTRAL
      forc_solarin(patch) = forc_swrad(patch)
#endif
      forc_hgt_u(patch) = hgt_u
      forc_hgt_t(patch) = hgt_t
      forc_hgt_q(patch) = hgt_q
      forc_rhoair(patch) = rhoair
      forc_hpbl (patch) = hpbl
      forc_aerdep(:,patch) = aerdep(:)
      IF (present(oro)) THEN
         patch_oro = 1._r8
         IF (patchtype(patch) >= 99) THEN
            patch_oro = 0._r8
            IF (nint(oro) == 2) patch_oro = 2._r8
         ENDIF
         oroflag(patch) = patch_oro
      ENDIF
      IF (present(ozone)) THEN
         forc_ozone(patch) = ozone
      ELSE
         forc_ozone(patch) = 0._r8
      ENDIF
      ierr = 0
   END SUBROUTINE colm_mpas_set_forcing

   SUBROUTINE colm_mpas_set_element_forcing(element, pco2m, po2m, us, vs, tair, qair, prc, prl, rain, snow, &
	                                           solar_cosine, &
                                            psrf, pbot, sols, soll, solsd, solld, frl, hgt_u, hgt_t, hgt_q, &
                                            rhoair, hpbl, aerdep, oro, ozone, ierr)
      integer, intent(in) :: element
      real(r8), intent(in) :: pco2m, po2m, us, vs, tair, qair, prc, prl, rain, snow, solar_cosine
      real(r8), intent(in) :: psrf, pbot, sols, soll, solsd, solld, frl
      real(r8), intent(in) :: hgt_u, hgt_t, hgt_q, rhoair, hpbl
      real(r8), intent(in) :: aerdep(14)
      real(r8), intent(in), optional :: oro
      real(r8), intent(in), optional :: ozone
      integer, intent(out) :: ierr

      integer :: patch
      integer :: patch_ierr
      integer :: istt
      integer :: iend
      logical :: did_set

      ierr = 1
      IF (.not. allocated(elm_patch%substt)) RETURN
      IF (.not. allocated(elm_patch%subend)) RETURN
      IF (.not. allocated(elm_patch%subfrc)) RETURN
      IF (numpatch < 1) RETURN
      IF (size(elm_patch%subfrc) < numpatch) RETURN
      IF (allocated(patchmask)) THEN
         IF (size(patchmask) < numpatch) RETURN
      ENDIF
      IF (element < 1 .or. element > size(elm_patch%substt) .or. &
          element > size(elm_patch%subend)) RETURN

      istt = elm_patch%substt(element)
      iend = elm_patch%subend(element)
      IF (istt < 1 .or. iend < istt .or. iend > numpatch .or. &
          iend > size(elm_patch%subfrc)) RETURN

      did_set = .false.
      DO patch = istt, iend
         IF (patch < 1 .or. patch > numpatch) RETURN
         IF (allocated(patchmask)) THEN
            IF (.not. patchmask(patch)) CYCLE
         ENDIF
         IF (present(oro) .and. present(ozone)) THEN
            CALL colm_mpas_set_forcing(patch, pco2m, po2m, us, vs, tair, qair, prc, prl, rain, snow, solar_cosine, &
                                       psrf, pbot, sols, soll, solsd, solld, frl, hgt_u, hgt_t, hgt_q, &
                                       rhoair, hpbl, aerdep, oro=oro, ozone=ozone, ierr=patch_ierr)
         ELSEIF (present(oro)) THEN
            CALL colm_mpas_set_forcing(patch, pco2m, po2m, us, vs, tair, qair, prc, prl, rain, snow, solar_cosine, &
                                       psrf, pbot, sols, soll, solsd, solld, frl, hgt_u, hgt_t, hgt_q, &
                                       rhoair, hpbl, aerdep, oro=oro, ierr=patch_ierr)
         ELSEIF (present(ozone)) THEN
            CALL colm_mpas_set_forcing(patch, pco2m, po2m, us, vs, tair, qair, prc, prl, rain, snow, solar_cosine, &
                                       psrf, pbot, sols, soll, solsd, solld, frl, hgt_u, hgt_t, hgt_q, &
                                       rhoair, hpbl, aerdep, ozone=ozone, ierr=patch_ierr)
         ELSE
            CALL colm_mpas_set_forcing(patch, pco2m, po2m, us, vs, tair, qair, prc, prl, rain, snow, solar_cosine, &
                                       psrf, pbot, sols, soll, solsd, solld, frl, hgt_u, hgt_t, hgt_q, &
                                       rhoair, hpbl, aerdep, ierr=patch_ierr)
         ENDIF
         IF (patch_ierr /= 0) RETURN
         did_set = .true.
      ENDDO

      IF (did_set) ierr = 0
   END SUBROUTINE colm_mpas_set_element_forcing

	   SUBROUTINE colm_mpas_step(idate, deltim, dolai, doalb, dosst, ierr)
	      USE MOD_LAIReadin, only: LAI_readin
	      USE MOD_Namelist, only: DEF_LAI_MONTHLY, DEF_LAI_CHANGE_YEARLY
	      USE MOD_TimeManager, only: julian2monthday
#ifdef GridRiverLakeFlow
	      USE MOD_Grid_RiverLakeFlow, only: grid_riverlake_flow
#endif
	      integer, intent(in) :: idate(3)
	      real(r8), intent(in) :: deltim
	      logical, intent(in) :: dolai, doalb, dosst
      integer, intent(out) :: ierr

	      integer :: lai_month
	      integer :: lai_mday
	      integer :: lai_period
	      integer :: lai_time
	      integer :: lai_year
	      logical :: ready

      CALL colm_mpas_ready(ready)
      ierr = 1
      IF (.not. ready) RETURN

	      IF (DEF_LAI_MONTHLY) THEN
	         CALL julian2monthday(idate(1), idate(2), lai_month, lai_mday)
	         lai_period = lai_month
	         lai_time = lai_month
	      ELSE
	         lai_period = 1 + (idate(2) - 1) / 8
	         lai_time = 1 + 8 * (lai_period - 1)
	      ENDIF
	      IF (DEF_LAI_CHANGE_YEARLY) THEN
	         lai_year = idate(1)
	      ELSE
	         lai_year = colm_mpas_lc_year
	      ENDIF
	      IF (numpatch > 0) CALL CoLMDRIVER(idate, deltim, dolai, doalb, dosst, oroflag)
#ifdef GridRiverLakeFlow
	      CALL grid_riverlake_flow(idate(1), deltim)
#endif
	      ! CoLM updates prescribed LAI/SAI after a period-boundary step so that
	      ! the new vegetation data are used by the following step.
	      IF (lai_year /= colm_mpas_last_lai_year .or. lai_period /= colm_mpas_last_lai_period) THEN
	         CALL LAI_readin(lai_year, lai_time, colm_mpas_dir_landdata)
	         colm_mpas_last_lai_year = lai_year
	         colm_mpas_last_lai_period = lai_period
	      ENDIF
	      colm_mpas_last_idate(:) = idate(:)
	      ierr = 0
	   END SUBROUTINE colm_mpas_step

	   SUBROUTINE colm_mpas_force_restart(ierr)
	      integer, intent(out) :: ierr

	      ierr = 1
	      IF (.not. colm_mpas_initialized) RETURN
	      IF (colm_mpas_last_idate(1) <= 0) RETURN
	      CALL colm_mpas_write_restart(colm_mpas_last_idate, ierr)
	   END SUBROUTINE colm_mpas_force_restart

	   SUBROUTINE colm_mpas_write_restart(idate, ierr)
	      USE MOD_TimeManager, only: adj2begin
	      USE MOD_Vars_TimeVariables, only: WRITE_TimeVariables
	      integer, intent(in) :: idate(3)
	      integer, intent(out) :: ierr

	      integer :: write_idate(3)
	      integer :: write_lc_year

	      ierr = 0
	      IF (.not. colm_mpas_restart_ready) RETURN

	      write_idate(:) = idate(:)
	      CALL adj2begin(write_idate)

	      IF (all(write_idate == colm_mpas_last_restart_idate)) RETURN

#ifdef LULCC
	      IF (write_idate(1) >= 2000) THEN
	         write_lc_year = write_idate(1)
	      ELSE
	         write_lc_year = (write_idate(1) / 5) * 5
	      ENDIF
#else
	      write_lc_year = colm_mpas_lc_year
#endif

	      CALL WRITE_TimeVariables(write_idate, write_lc_year, colm_mpas_casename, colm_mpas_dir_restart)
	      colm_mpas_last_restart_idate(:) = write_idate(:)
	   END SUBROUTINE colm_mpas_write_restart

	   SUBROUTINE colm_mpas_get_surface(patch, sensible, latent, evaporation, ground_heat, runoff, &
	                                    surface_runoff, subsurface_runoff, skin_temp, t2m, q2m, &
	                                    u10m, v10m, &
	                                    surface_humidity, emissivity, roughness, albedo, friction_velocity, &
	                                    mechanical_friction_velocity, &
	                                    stability_zeta, bulk_richardson, momentum_profile, heat_profile, &
	                                    moisture_profile, air_density, heat_exchange_velocity, &
	                                    moisture_exchange_velocity, heat_exchange_velocity_2m, &
	                                    moisture_exchange_velocity_2m, momentum_coefficient, enthalpy_coefficient, &
	                                    momentum_coefficient_10m, &
	                                    inverse_monin_obukhov, ierr)
	      integer, intent(in) :: patch
	      real(r8), intent(out) :: sensible, latent, evaporation, ground_heat, runoff
	      real(r8), intent(out) :: surface_runoff, subsurface_runoff, skin_temp, t2m, q2m
	      real(r8), intent(out) :: u10m, v10m
	      real(r8), intent(out) :: surface_humidity
	      real(r8), intent(out) :: emissivity, roughness, albedo
	      real(r8), intent(out) :: friction_velocity, mechanical_friction_velocity
	      real(r8), intent(out) :: stability_zeta, bulk_richardson
	      real(r8), intent(out) :: momentum_profile, heat_profile
	      real(r8), intent(out) :: moisture_profile, air_density
	      real(r8), intent(out) :: heat_exchange_velocity, moisture_exchange_velocity
	      real(r8), intent(out) :: heat_exchange_velocity_2m, moisture_exchange_velocity_2m
	      real(r8), intent(out) :: momentum_coefficient, enthalpy_coefficient
	      real(r8), intent(out) :: momentum_coefficient_10m
	      real(r8), intent(out) :: inverse_monin_obukhov
	      integer, intent(out) :: ierr
	      real(r8) :: incoming_shortwave
	      real(r8) :: bad_value
	      real(r8) :: reference_height_u
	      real(r8) :: reference_height_t
	      real(r8) :: reference_height_q

      ierr = 1
      sensible = spval
      latent = spval
      evaporation = spval
      ground_heat = spval
      runoff = spval
      surface_runoff = spval
      subsurface_runoff = spval
	      skin_temp = spval
	      t2m = spval
	      q2m = spval
	      u10m = spval
	      v10m = spval
	      surface_humidity = spval
	      emissivity = spval
	      roughness = spval
      albedo = spval
	      friction_velocity = spval
	      mechanical_friction_velocity = spval
	      stability_zeta = spval
	      bulk_richardson = spval
	      momentum_profile = spval
	      heat_profile = spval
	      moisture_profile = spval
	      air_density = spval
	      heat_exchange_velocity = spval
	      moisture_exchange_velocity = spval
	      heat_exchange_velocity_2m = spval
	      moisture_exchange_velocity_2m = spval
	      momentum_coefficient = spval
	      enthalpy_coefficient = spval
	      momentum_coefficient_10m = spval
	      inverse_monin_obukhov = spval

      IF (.not. allocated(fsena)) RETURN
      IF (.not. allocated(lfevpa)) RETURN
      IF (.not. allocated(fevpa)) RETURN
      IF (.not. allocated(fgrnd)) RETURN
      IF (.not. allocated(rnof)) RETURN
      IF (.not. allocated(rsur)) RETURN
      IF (.not. allocated(rsub)) RETURN
      IF (.not. allocated(trad)) RETURN
      IF (.not. allocated(tref)) RETURN
      IF (.not. allocated(qref)) RETURN
      IF (.not. allocated(emis)) RETURN
      IF (.not. allocated(z0m)) RETURN
      IF (patch < 1) RETURN
      IF (patch > size(fsena)) RETURN
      IF (patch > size(lfevpa)) RETURN
      IF (patch > size(fevpa)) RETURN
      IF (patch > size(fgrnd)) RETURN
      IF (patch > size(rnof)) RETURN
      IF (patch > size(rsur)) RETURN
      IF (patch > size(rsub)) RETURN
      IF (patch > size(trad)) RETURN
      IF (patch > size(tref)) RETURN
      IF (patch > size(qref)) RETURN
      IF (patch > size(emis)) RETURN
      IF (patch > size(z0m)) RETURN

      sensible = fsena(patch)
      latent = lfevpa(patch)
      evaporation = fevpa(patch)
      ground_heat = fgrnd(patch)
      runoff = rnof(patch)
      surface_runoff = rsur(patch)
      subsurface_runoff = rsub(patch)
	      skin_temp = trad(patch)
	      t2m = tref(patch)
	      q2m = qref(patch)
	      surface_humidity = qref(patch)
	      IF (allocated(qsfc)) THEN
	         IF (patch <= size(qsfc)) THEN
	            IF (qsfc(patch) >= 0._r8 .and. qsfc(patch) < 1._r8) surface_humidity = qsfc(patch)
	         ENDIF
	      ENDIF
	      emissivity = emis(patch)
      roughness = z0m(patch)
	      bad_value = 0.5_r8 * abs(spval)
	      IF (allocated(alb)) THEN
	         IF (patch <= size(alb,3)) THEN
	            IF (all(abs(alb(:,:,patch)) < bad_value)) THEN
	               incoming_shortwave = forc_sols(patch) + forc_soll(patch) + forc_solsd(patch) + forc_solld(patch)
	               IF (incoming_shortwave > tiny(1._r8)) THEN
	                  albedo = (alb(1,1,patch) * forc_sols(patch) + alb(2,1,patch) * forc_soll(patch) + &
	                            alb(1,2,patch) * forc_solsd(patch) + alb(2,2,patch) * forc_solld(patch)) / &
	                           incoming_shortwave
	               ELSE
	                  albedo = sum(alb(:,:,patch)) / real(size(alb(:,:,patch)), r8)
	               ENDIF
	            ENDIF
	         ENDIF
	      ENDIF
	      IF (allocated(ustar)) THEN
	         IF (patch <= size(ustar)) friction_velocity = ustar(patch)
	      ENDIF
	      IF (allocated(zol)) THEN
	         IF (patch <= size(zol)) stability_zeta = zol(patch)
	      ENDIF
	      IF (allocated(rib)) THEN
	         IF (patch <= size(rib)) bulk_richardson = rib(patch)
	      ENDIF
	      IF (allocated(fm)) THEN
	         IF (patch <= size(fm)) momentum_profile = fm(patch)
	      ENDIF
	      IF (allocated(fh)) THEN
	         IF (patch <= size(fh)) heat_profile = fh(patch)
	      ENDIF
	      IF (allocated(fq)) THEN
	         IF (patch <= size(fq)) moisture_profile = fq(patch)
	      ENDIF
	      IF (allocated(forc_rhoair)) THEN
	         IF (patch <= size(forc_rhoair)) air_density = forc_rhoair(patch)
	      ENDIF
	      IF (allocated(forc_us) .and. allocated(forc_vs)) THEN
	         IF (patch <= size(forc_us) .and. patch <= size(forc_vs) .and. &
	             abs(momentum_profile) < bad_value .and. momentum_profile > tiny(1._r8) .and. &
	             all(ieee_is_finite((/forc_us(patch), forc_vs(patch)/)))) THEN
	            ! Match MPAS USTM: use the resolved wind, excluding the
	            ! convective velocity enhancement included in CoLM USTAR.
	            mechanical_friction_velocity = vonkar * &
	               sqrt(forc_us(patch)**2 + forc_vs(patch)**2) / momentum_profile
	         ENDIF
	      ENDIF
	      IF (abs(friction_velocity) < bad_value .and. friction_velocity >= 0._r8) THEN
	         IF (abs(heat_profile) < bad_value .and. heat_profile > tiny(1._r8)) &
	            heat_exchange_velocity = vonkar * friction_velocity / heat_profile
	         IF (abs(moisture_profile) < bad_value .and. moisture_profile > tiny(1._r8)) &
	            moisture_exchange_velocity = vonkar * friction_velocity / moisture_profile
	      ENDIF
	      IF (abs(air_density) < bad_value .and. air_density > 0._r8 .and. &
	          abs(moisture_exchange_velocity) < bad_value .and. &
	          moisture_exchange_velocity > tiny(1._r8)) THEN
	         IF (.not. allocated(forc_q)) RETURN
	         IF (patch > size(forc_q)) RETURN
	         IF (.not. ieee_is_finite(forc_q(patch)) .or. forc_q(patch) < 0._r8 .or. &
	             forc_q(patch) >= 1._r8) RETURN
	         ! The atmospheric boundary sees canopy-air humidity, not ground qg.
	         ! Reconstruct the effective bulk value from CoLM's total vapor flux.
	         surface_humidity = forc_q(patch) + evaporation / &
	                            (air_density * moisture_exchange_velocity)
	         IF (.not. ieee_is_finite(surface_humidity) .or. surface_humidity < 0._r8 .or. &
	             surface_humidity >= 1._r8) RETURN
	      ENDIF
	      IF (abs(momentum_profile) < bad_value .and. momentum_profile > tiny(1._r8)) THEN
	         momentum_coefficient = (vonkar / momentum_profile)**2
	         IF (abs(moisture_profile) < bad_value .and. moisture_profile > tiny(1._r8)) &
	            enthalpy_coefficient = vonkar**2 / (momentum_profile * moisture_profile)
	      ENDIF
	      CALL colm_mpas_get_patch_reference_heights(patch, reference_height_u, reference_height_t, &
	                                                 reference_height_q, ierr)
	      IF (ierr /= 0) RETURN
	      IF (abs(stability_zeta) < bad_value .and. allocated(displa)) THEN
	         IF (patch <= size(forc_hgt_u) .and. patch <= size(displa)) THEN
	            IF (reference_height_u - displa(patch) > tiny(1._r8)) &
	               inverse_monin_obukhov = stability_zeta / (reference_height_u - displa(patch))
	         ENDIF
	      ENDIF
	      CALL colm_mpas_get_patch_similarity_diagnostics(patch, u10m, v10m, &
	                                                       heat_exchange_velocity_2m, &
	                                                       moisture_exchange_velocity_2m, &
	                                                       momentum_coefficient_10m, ierr)
	      IF (ierr /= 0) RETURN
	      ierr = 0
	   END SUBROUTINE colm_mpas_get_surface

	   SUBROUTINE colm_mpas_get_patch_reference_heights(patch, height_u, height_t, height_q, ierr)
	      integer, intent(in) :: patch
	      real(r8), intent(out) :: height_u
	      real(r8), intent(out) :: height_t
	      real(r8), intent(out) :: height_q
	      integer, intent(out) :: ierr

	      ierr = 1
	      height_u = spval
	      height_t = spval
	      height_q = spval
	      IF (.not. allocated(forc_hgt_u) .or. .not. allocated(forc_hgt_t) .or. &
	          .not. allocated(forc_hgt_q)) RETURN
	      IF (patch < 1 .or. patch > size(forc_hgt_u) .or. patch > size(forc_hgt_t) .or. &
	          patch > size(forc_hgt_q)) RETURN
	      IF (.not. all(ieee_is_finite((/forc_hgt_u(patch), forc_hgt_t(patch), &
	                                      forc_hgt_q(patch)/)))) RETURN
	      IF (forc_hgt_u(patch) <= 0._r8 .or. forc_hgt_t(patch) <= 0._r8 .or. &
	          forc_hgt_q(patch) <= 0._r8) RETURN

	      ! The similarity diagnostics were computed at the actual forcing heights.
	      ! Canopy clearance is enforced in the leaf-temperature solvers; never
	      ! relabel the same wind/temperature/humidity state at a different height.
	      height_u = forc_hgt_u(patch)
	      height_t = forc_hgt_t(patch)
	      height_q = forc_hgt_q(patch)
	      ierr = 0
	   END SUBROUTINE colm_mpas_get_patch_reference_heights

	   SUBROUTINE colm_mpas_get_patch_similarity_diagnostics(patch, u10m, v10m, &
	                                                          heat_exchange_velocity_2m, &
	                                                          moisture_exchange_velocity_2m, &
	                                                          momentum_coefficient_10m, ierr)
	      integer, intent(in) :: patch
	      real(r8), intent(out) :: u10m
	      real(r8), intent(out) :: v10m
	      real(r8), intent(out) :: heat_exchange_velocity_2m
	      real(r8), intent(out) :: moisture_exchange_velocity_2m
	      real(r8), intent(out) :: momentum_coefficient_10m
	      integer, intent(out) :: ierr

	      ierr = 1
	      u10m = spval
	      v10m = spval
	      heat_exchange_velocity_2m = spval
	      moisture_exchange_velocity_2m = spval
	      momentum_coefficient_10m = spval
	      IF (.not. allocated(native_u10m) .or. .not. allocated(native_v10m) .or. &
	          .not. allocated(native_chs2) .or. .not. allocated(native_cqs2) .or. &
	          .not. allocated(native_cd10)) RETURN
	      IF (patch < 1 .or. patch > size(native_u10m) .or. patch > size(native_v10m) .or. &
	          patch > size(native_chs2) .or. patch > size(native_cqs2) .or. &
	          patch > size(native_cd10)) RETURN

	      u10m = native_u10m(patch)
	      v10m = native_v10m(patch)
	      heat_exchange_velocity_2m = native_chs2(patch)
	      moisture_exchange_velocity_2m = native_cqs2(patch)
	      momentum_coefficient_10m = native_cd10(patch)
	      IF (.not. all(ieee_is_finite((/u10m, v10m, heat_exchange_velocity_2m, &
	                                      moisture_exchange_velocity_2m, &
	                                      momentum_coefficient_10m/)))) RETURN
	      IF (heat_exchange_velocity_2m < 0._r8 .or. moisture_exchange_velocity_2m < 0._r8 .or. &
	          momentum_coefficient_10m < 0._r8) RETURN
	      ierr = 0
	   END SUBROUTINE colm_mpas_get_patch_similarity_diagnostics

	   SUBROUTINE colm_mpas_get_element_surface(element, sensible, latent, evaporation, ground_heat, runoff, &
	                                            surface_runoff, subsurface_runoff, skin_temp, t2m, q2m, &
	                                            u10m, v10m, &
	                                            surface_humidity, emissivity, roughness, albedo, friction_velocity, &
	                                            mechanical_friction_velocity, &
	                                            stability_zeta, bulk_richardson, momentum_profile, heat_profile, &
	                                            moisture_profile, air_density, heat_exchange_velocity, &
	                                            moisture_exchange_velocity, heat_exchange_velocity_2m, &
	                                            moisture_exchange_velocity_2m, momentum_coefficient, enthalpy_coefficient, &
	                                            momentum_coefficient_10m, &
	                                            inverse_monin_obukhov, ierr)
	      integer, intent(in) :: element
	      real(r8), intent(out) :: sensible, latent, evaporation, ground_heat, runoff
	      real(r8), intent(out) :: surface_runoff, subsurface_runoff, skin_temp, t2m, q2m
	      real(r8), intent(out) :: u10m, v10m
	      real(r8), intent(out) :: surface_humidity
	      real(r8), intent(out) :: emissivity, roughness, albedo
	      real(r8), intent(out) :: friction_velocity, mechanical_friction_velocity
	      real(r8), intent(out) :: stability_zeta, bulk_richardson
	      real(r8), intent(out) :: momentum_profile, heat_profile
	      real(r8), intent(out) :: moisture_profile, air_density
	      real(r8), intent(out) :: heat_exchange_velocity, moisture_exchange_velocity
	      real(r8), intent(out) :: heat_exchange_velocity_2m, moisture_exchange_velocity_2m
	      real(r8), intent(out) :: momentum_coefficient, enthalpy_coefficient
	      real(r8), intent(out) :: momentum_coefficient_10m
	      real(r8), intent(out) :: inverse_monin_obukhov
      integer, intent(out) :: ierr

      integer :: patch
      integer :: patch_ierr
      integer :: istt
      integer :: iend
      real(r8) :: wt
      real(r8) :: sumwt
	      real(r8) :: albedo_sum
	      real(r8) :: albedo_wt
	      real(r8) :: radiative_wt
	      real(r8) :: heat_conductance_sum
	      real(r8) :: moisture_conductance_sum
	      real(r8) :: heat_2m_conductance_sum
	      real(r8) :: moisture_2m_conductance_sum
	      real(r8) :: humidity_conductance_sum
	      real(r8) :: exchange_wt(16)
	      real(r8) :: bad_value
      real(r8) :: patch_sensible, patch_latent, patch_evaporation, patch_ground_heat
      real(r8) :: patch_runoff, patch_surface_runoff, patch_subsurface_runoff
	      real(r8) :: patch_skin_temp, patch_t2m, patch_q2m, patch_u10m, patch_v10m, patch_surface_humidity
	      real(r8) :: patch_emissivity, patch_roughness, patch_albedo
	      real(r8) :: patch_friction_velocity, patch_mechanical_friction_velocity
	      real(r8) :: patch_stability_zeta, patch_bulk_richardson
	      real(r8) :: patch_momentum_profile, patch_heat_profile
	      real(r8) :: patch_moisture_profile, patch_air_density
	      real(r8) :: patch_heat_exchange_velocity, patch_moisture_exchange_velocity
	      real(r8) :: patch_heat_exchange_velocity_2m, patch_moisture_exchange_velocity_2m
	      real(r8) :: patch_momentum_coefficient, patch_enthalpy_coefficient
	      real(r8) :: patch_momentum_coefficient_10m
	      real(r8) :: patch_inverse_monin_obukhov

      ierr = 1
      sensible = spval
      latent = spval
      evaporation = spval
      ground_heat = spval
      runoff = spval
      surface_runoff = spval
      subsurface_runoff = spval
	      skin_temp = spval
	      t2m = spval
	      q2m = spval
	      u10m = spval
	      v10m = spval
	      surface_humidity = spval
	      emissivity = spval
      roughness = spval
      albedo = spval
	      friction_velocity = spval
	      mechanical_friction_velocity = spval
	      stability_zeta = spval
	      bulk_richardson = spval
	      momentum_profile = spval
	      heat_profile = spval
	      moisture_profile = spval
	      air_density = spval
	      heat_exchange_velocity = spval
	      moisture_exchange_velocity = spval
	      heat_exchange_velocity_2m = spval
	      moisture_exchange_velocity_2m = spval
	      momentum_coefficient = spval
	      enthalpy_coefficient = spval
	      momentum_coefficient_10m = spval
	      inverse_monin_obukhov = spval

      IF (.not. allocated(elm_patch%substt)) RETURN
      IF (.not. allocated(elm_patch%subend)) RETURN
      IF (.not. allocated(elm_patch%subfrc)) RETURN
      IF (numpatch < 1) RETURN
      IF (size(elm_patch%subfrc) < numpatch) RETURN
      IF (allocated(patchmask)) THEN
         IF (size(patchmask) < numpatch) RETURN
      ENDIF
      IF (element < 1 .or. element > size(elm_patch%substt) .or. &
          element > size(elm_patch%subend)) RETURN

      istt = elm_patch%substt(element)
      iend = elm_patch%subend(element)
      IF (istt < 1 .or. iend < istt .or. iend > numpatch .or. &
          iend > size(elm_patch%subfrc)) RETURN

      sensible = 0._r8
      latent = 0._r8
      evaporation = 0._r8
      ground_heat = 0._r8
      runoff = 0._r8
      surface_runoff = 0._r8
      subsurface_runoff = 0._r8
	      skin_temp = 0._r8
	      t2m = 0._r8
	      q2m = 0._r8
	      u10m = 0._r8
	      v10m = 0._r8
	      surface_humidity = 0._r8
	      emissivity = 0._r8
      roughness = 0._r8
	      friction_velocity = 0._r8
	      mechanical_friction_velocity = 0._r8
	      stability_zeta = 0._r8
	      bulk_richardson = 0._r8
	      momentum_profile = 0._r8
	      heat_profile = 0._r8
	      moisture_profile = 0._r8
	      air_density = 0._r8
	      heat_exchange_velocity = 0._r8
	      moisture_exchange_velocity = 0._r8
	      heat_exchange_velocity_2m = 0._r8
	      moisture_exchange_velocity_2m = 0._r8
	      momentum_coefficient = 0._r8
	      enthalpy_coefficient = 0._r8
	      momentum_coefficient_10m = 0._r8
	      inverse_monin_obukhov = 0._r8
      albedo_sum = 0._r8
      albedo_wt = 0._r8
	      radiative_wt = 0._r8
	      heat_conductance_sum = 0._r8
	      moisture_conductance_sum = 0._r8
	      heat_2m_conductance_sum = 0._r8
	      moisture_2m_conductance_sum = 0._r8
	      humidity_conductance_sum = 0._r8
	      exchange_wt(:) = 0._r8
	      bad_value = 0.5_r8 * abs(spval)
      sumwt = 0._r8

	      DO patch = istt, iend
	         IF (patch < 1 .or. patch > numpatch) THEN
	            WRITE(*,*) 'Error: CoLM2024 element surface references invalid patch:', element, patch, numpatch
	            RETURN
	         ENDIF
	         IF (allocated(patchmask)) THEN
	            IF (.not. patchmask(patch)) CYCLE
	         ENDIF
	         wt = elm_patch%subfrc(patch)
	         IF (.not. ieee_is_finite(wt)) RETURN
	         IF (wt <= 0._r8) CYCLE
	         CALL colm_mpas_get_surface(patch, patch_sensible, patch_latent, patch_evaporation, patch_ground_heat, &
	                                    patch_runoff, patch_surface_runoff, patch_subsurface_runoff, &
		                                    patch_skin_temp, patch_t2m, patch_q2m, patch_u10m, patch_v10m, &
		                                    patch_surface_humidity, &
	                                    patch_emissivity, patch_roughness, patch_albedo, patch_friction_velocity, &
	                                    patch_mechanical_friction_velocity, &
	                                    patch_stability_zeta, patch_bulk_richardson, patch_momentum_profile, &
	                                    patch_heat_profile, patch_moisture_profile, patch_air_density, &
	                                    patch_heat_exchange_velocity, patch_moisture_exchange_velocity, &
	                                    patch_heat_exchange_velocity_2m, patch_moisture_exchange_velocity_2m, &
	                                    patch_momentum_coefficient, patch_enthalpy_coefficient, &
	                                    patch_momentum_coefficient_10m, &
	                                    patch_inverse_monin_obukhov, patch_ierr)
	         IF (patch_ierr /= 0) THEN
	            WRITE(*,*) 'Error: failed to retrieve CoLM2024 patch surface:', element, patch, patch_ierr
	            RETURN
	         ENDIF
	         IF (.not. ieee_is_finite(patch_skin_temp) .or. patch_skin_temp <= 0._r8 .or. &
	             abs(patch_skin_temp) >= bad_value) RETURN
	         IF (.not. all(ieee_is_finite((/patch_sensible, patch_latent, patch_evaporation, patch_ground_heat, &
	                                        patch_runoff, patch_surface_runoff, patch_subsurface_runoff, patch_t2m, &
	                                        patch_q2m, patch_u10m, patch_v10m, patch_surface_humidity, &
	                                        patch_emissivity, patch_roughness/)))) RETURN
	         IF (any(abs((/patch_sensible, patch_latent, patch_evaporation, patch_ground_heat, patch_runoff, &
	                       patch_surface_runoff, patch_subsurface_runoff, patch_t2m, patch_q2m, &
	                       patch_u10m, patch_v10m, patch_surface_humidity, patch_emissivity, &
	                       patch_roughness/)) >= bad_value)) RETURN
	         IF (patch_t2m <= 0._r8 .or. patch_q2m < 0._r8 .or. patch_q2m >= 1._r8 .or. &
	             patch_surface_humidity < 0._r8 .or. patch_surface_humidity >= 1._r8 .or. &
	             patch_emissivity < 0._r8 .or. patch_emissivity > 1._r8 .or. patch_roughness < 0._r8) RETURN
	         IF (.not. all(ieee_is_finite((/patch_air_density, patch_friction_velocity, &
	                                        patch_mechanical_friction_velocity, patch_heat_exchange_velocity, &
	                                        patch_moisture_exchange_velocity, patch_heat_exchange_velocity_2m, &
	                                        patch_moisture_exchange_velocity_2m, patch_momentum_coefficient, &
	                                        patch_enthalpy_coefficient, patch_momentum_coefficient_10m, &
	                                        patch_inverse_monin_obukhov/)))) RETURN
	         IF (any(abs((/patch_air_density, patch_friction_velocity, patch_mechanical_friction_velocity, &
	                       patch_heat_exchange_velocity, patch_moisture_exchange_velocity, &
	                       patch_heat_exchange_velocity_2m, patch_moisture_exchange_velocity_2m, &
	                       patch_momentum_coefficient, patch_enthalpy_coefficient, &
	                       patch_momentum_coefficient_10m, patch_inverse_monin_obukhov/)) >= bad_value)) RETURN
	         IF (patch_air_density <= 0._r8 .or. patch_heat_exchange_velocity < 0._r8 .or. &
	             patch_moisture_exchange_velocity < 0._r8 .or. &
	             patch_heat_exchange_velocity_2m < 0._r8 .or. patch_moisture_exchange_velocity_2m < 0._r8 .or. &
	             patch_mechanical_friction_velocity < 0._r8 .or. patch_momentum_coefficient < 0._r8 .or. &
	             patch_enthalpy_coefficient < 0._r8 .or. patch_momentum_coefficient_10m < 0._r8 .or. &
	             abs(patch_inverse_monin_obukhov) >= bad_value) RETURN

	         sumwt = sumwt + wt
	         sensible = sensible + wt * patch_sensible
	         latent = latent + wt * patch_latent
	         evaporation = evaporation + wt * patch_evaporation
         ground_heat = ground_heat + wt * patch_ground_heat
         runoff = runoff + wt * patch_runoff
         surface_runoff = surface_runoff + wt * patch_surface_runoff
         subsurface_runoff = subsurface_runoff + wt * patch_subsurface_runoff
	         ! CoLM defines trad from outgoing longwave radiation, so preserve the
	         ! area-weighted radiative flux when reducing patches to one MPAS cell.
	         skin_temp = skin_temp + wt * patch_emissivity * patch_skin_temp**4
	         radiative_wt = radiative_wt + wt * patch_emissivity
	         t2m = t2m + wt * patch_t2m
	         q2m = q2m + wt * patch_q2m
	         u10m = u10m + wt * patch_u10m
	         v10m = v10m + wt * patch_v10m
	         surface_humidity = surface_humidity + wt * patch_surface_humidity
	         emissivity = emissivity + wt * patch_emissivity
	         roughness = roughness + wt * patch_roughness
	         IF (abs(patch_friction_velocity) < bad_value .and. patch_friction_velocity >= 0._r8) THEN
	            ! Momentum stress is proportional to ustar**2, so reduce stress
	            ! rather than linearly averaging patch friction velocities.
	            friction_velocity = friction_velocity + wt * patch_friction_velocity**2
	            exchange_wt(1) = exchange_wt(1) + wt
	         ENDIF
	         mechanical_friction_velocity = mechanical_friction_velocity + &
	                                        wt * patch_mechanical_friction_velocity**2
	         exchange_wt(13) = exchange_wt(13) + wt
	         IF (abs(patch_stability_zeta) < bad_value) THEN
	            stability_zeta = stability_zeta + wt * patch_stability_zeta
	            exchange_wt(2) = exchange_wt(2) + wt
	         ENDIF
	         IF (abs(patch_bulk_richardson) < bad_value) THEN
	            bulk_richardson = bulk_richardson + wt * patch_bulk_richardson
	            exchange_wt(3) = exchange_wt(3) + wt
	         ENDIF
	         IF (abs(patch_momentum_profile) < bad_value .and. patch_momentum_profile > tiny(1._r8)) THEN
	            momentum_profile = momentum_profile + wt * patch_momentum_profile
	            exchange_wt(4) = exchange_wt(4) + wt
	         ENDIF
	         IF (abs(patch_heat_profile) < bad_value .and. patch_heat_profile > tiny(1._r8)) THEN
	            heat_profile = heat_profile + wt * patch_heat_profile
	            exchange_wt(5) = exchange_wt(5) + wt
	         ENDIF
	         IF (abs(patch_moisture_profile) < bad_value .and. patch_moisture_profile > tiny(1._r8)) THEN
	            moisture_profile = moisture_profile + wt * patch_moisture_profile
	            exchange_wt(6) = exchange_wt(6) + wt
	         ENDIF
	         air_density = air_density + wt * patch_air_density
	         exchange_wt(7) = exchange_wt(7) + wt
	         ! Return one cell-scale exchange velocity whose product with the
	         ! cell air density exactly preserves the patch-mean conductance.
	         heat_conductance_sum = heat_conductance_sum + &
	                                wt * patch_air_density * patch_heat_exchange_velocity
	         moisture_conductance_sum = moisture_conductance_sum + &
	                                    wt * patch_air_density * patch_moisture_exchange_velocity
	         heat_2m_conductance_sum = heat_2m_conductance_sum + &
	                                   wt * patch_air_density * patch_heat_exchange_velocity_2m
	         moisture_2m_conductance_sum = moisture_2m_conductance_sum + &
	                                       wt * patch_air_density * patch_moisture_exchange_velocity_2m
	         humidity_conductance_sum = humidity_conductance_sum + &
	                                    wt * patch_air_density * patch_moisture_exchange_velocity * &
	                                    patch_surface_humidity
	         exchange_wt(8) = exchange_wt(8) + wt
	         exchange_wt(9) = exchange_wt(9) + wt
	         exchange_wt(14) = exchange_wt(14) + wt
	         exchange_wt(15) = exchange_wt(15) + wt
	         IF (abs(patch_momentum_coefficient) < bad_value .and. patch_momentum_coefficient >= 0._r8) THEN
	            momentum_coefficient = momentum_coefficient + wt * patch_momentum_coefficient
	            exchange_wt(10) = exchange_wt(10) + wt
	         ENDIF
	         IF (abs(patch_enthalpy_coefficient) < bad_value .and. patch_enthalpy_coefficient >= 0._r8) THEN
	            enthalpy_coefficient = enthalpy_coefficient + wt * patch_enthalpy_coefficient
	            exchange_wt(11) = exchange_wt(11) + wt
	         ENDIF
	         IF (abs(patch_inverse_monin_obukhov) < bad_value) THEN
	            inverse_monin_obukhov = inverse_monin_obukhov + wt * patch_inverse_monin_obukhov
	            exchange_wt(12) = exchange_wt(12) + wt
	         ENDIF
	         momentum_coefficient_10m = momentum_coefficient_10m + wt * patch_momentum_coefficient_10m
	         exchange_wt(16) = exchange_wt(16) + wt
         IF (patch_albedo >= 0._r8 .and. patch_albedo <= 1._r8) THEN
            albedo_sum = albedo_sum + wt * patch_albedo
            albedo_wt = albedo_wt + wt
         ENDIF
      ENDDO

      IF (sumwt <= 0._r8) THEN
         sensible = spval
         latent = spval
         evaporation = spval
         ground_heat = spval
         runoff = spval
         surface_runoff = spval
         subsurface_runoff = spval
	         skin_temp = spval
	         t2m = spval
	         q2m = spval
	         u10m = spval
	         v10m = spval
	         surface_humidity = spval
	         emissivity = spval
	         roughness = spval
	         friction_velocity = spval
	         mechanical_friction_velocity = spval
	         stability_zeta = spval
	         bulk_richardson = spval
	         momentum_profile = spval
	         heat_profile = spval
	         moisture_profile = spval
	         air_density = spval
	         heat_exchange_velocity = spval
	         moisture_exchange_velocity = spval
	         heat_exchange_velocity_2m = spval
	         moisture_exchange_velocity_2m = spval
	         momentum_coefficient = spval
	         enthalpy_coefficient = spval
	         momentum_coefficient_10m = spval
	         inverse_monin_obukhov = spval
	         RETURN
      ENDIF

      sensible = sensible / sumwt
      latent = latent / sumwt
      evaporation = evaporation / sumwt
      ground_heat = ground_heat / sumwt
      runoff = runoff / sumwt
      surface_runoff = surface_runoff / sumwt
      subsurface_runoff = subsurface_runoff / sumwt
	      IF (radiative_wt <= 0._r8) RETURN
	      skin_temp = (skin_temp / radiative_wt)**0.25_r8
	      t2m = t2m / sumwt
	      q2m = q2m / sumwt
	      u10m = u10m / sumwt
	      v10m = v10m / sumwt
	      emissivity = emissivity / sumwt
	      roughness = roughness / sumwt
	      IF (exchange_wt(1) > 0._r8) THEN
	         friction_velocity = sqrt(friction_velocity / exchange_wt(1))
	      ELSE
	         friction_velocity = spval
	      ENDIF
	      IF (exchange_wt(13) > 0._r8) THEN
	         mechanical_friction_velocity = sqrt(mechanical_friction_velocity / exchange_wt(13))
	      ELSE
	         mechanical_friction_velocity = spval
	      ENDIF
	      IF (exchange_wt(2) > 0._r8) THEN
	         stability_zeta = stability_zeta / exchange_wt(2)
	      ELSE
	         stability_zeta = spval
	      ENDIF
	      IF (exchange_wt(3) > 0._r8) THEN
	         bulk_richardson = bulk_richardson / exchange_wt(3)
	      ELSE
	         bulk_richardson = spval
	      ENDIF
	      IF (exchange_wt(4) > 0._r8) THEN
	         momentum_profile = momentum_profile / exchange_wt(4)
	      ELSE
	         momentum_profile = spval
	      ENDIF
	      IF (exchange_wt(5) > 0._r8) THEN
	         heat_profile = heat_profile / exchange_wt(5)
	      ELSE
	         heat_profile = spval
	      ENDIF
	      IF (exchange_wt(6) > 0._r8) THEN
	         moisture_profile = moisture_profile / exchange_wt(6)
	      ELSE
	         moisture_profile = spval
	      ENDIF
	      IF (exchange_wt(7) > 0._r8) THEN
	         air_density = air_density / exchange_wt(7)
	      ELSE
	         air_density = spval
	      ENDIF
	      IF (exchange_wt(8) > 0._r8 .and. air_density > 0._r8) THEN
	         heat_exchange_velocity = heat_conductance_sum / (sumwt * air_density)
	      ELSE
	         heat_exchange_velocity = spval
	      ENDIF
	      IF (exchange_wt(9) > 0._r8 .and. air_density > 0._r8) THEN
	         moisture_exchange_velocity = moisture_conductance_sum / (sumwt * air_density)
	      ELSE
	         moisture_exchange_velocity = spval
	      ENDIF
	      IF (exchange_wt(14) > 0._r8 .and. air_density > 0._r8) THEN
	         heat_exchange_velocity_2m = heat_2m_conductance_sum / (sumwt * air_density)
	      ELSE
	         heat_exchange_velocity_2m = spval
	      ENDIF
	      IF (exchange_wt(15) > 0._r8 .and. air_density > 0._r8) THEN
	         moisture_exchange_velocity_2m = moisture_2m_conductance_sum / (sumwt * air_density)
	      ELSE
	         moisture_exchange_velocity_2m = spval
	      ENDIF
	      IF (moisture_conductance_sum > tiny(1._r8)) THEN
	         surface_humidity = humidity_conductance_sum / moisture_conductance_sum
	      ELSE
	         surface_humidity = surface_humidity / sumwt
	      ENDIF
	      IF (.not. ieee_is_finite(surface_humidity) .or. surface_humidity < 0._r8 .or. &
	          surface_humidity >= 1._r8) RETURN
	      IF (exchange_wt(10) > 0._r8) THEN
	         momentum_coefficient = momentum_coefficient / exchange_wt(10)
	      ELSE
	         momentum_coefficient = spval
	      ENDIF
	      IF (exchange_wt(11) > 0._r8) THEN
	         enthalpy_coefficient = enthalpy_coefficient / exchange_wt(11)
	      ELSE
	         enthalpy_coefficient = spval
	      ENDIF
	      IF (exchange_wt(12) > 0._r8) THEN
	         inverse_monin_obukhov = inverse_monin_obukhov / exchange_wt(12)
	      ELSE
	         inverse_monin_obukhov = spval
	      ENDIF
	      IF (exchange_wt(16) > 0._r8) THEN
	         momentum_coefficient_10m = momentum_coefficient_10m / exchange_wt(16)
	      ELSE
	         momentum_coefficient_10m = spval
	      ENDIF
      IF (albedo_wt > 0._r8) albedo = albedo_sum / albedo_wt
      ierr = 0
	   END SUBROUTINE colm_mpas_get_element_surface

	   SUBROUTINE colm_mpas_get_element_albedo(element, albedo_vis_direct, albedo_vis_diffuse, &
	                                            albedo_nir_direct, albedo_nir_diffuse, ierr)
	      integer, intent(in) :: element
	      real(r8), intent(out) :: albedo_vis_direct, albedo_vis_diffuse
	      real(r8), intent(out) :: albedo_nir_direct, albedo_nir_diffuse
	      integer, intent(out) :: ierr

	      integer :: patch
	      integer :: istt
	      integer :: iend
	      real(r8) :: wt
	      real(r8) :: sumwt

	      ierr = 1
	      albedo_vis_direct = spval
	      albedo_vis_diffuse = spval
	      albedo_nir_direct = spval
	      albedo_nir_diffuse = spval

	      IF (.not. allocated(elm_patch%substt) .or. .not. allocated(elm_patch%subend) .or. &
	          .not. allocated(elm_patch%subfrc) .or. .not. allocated(alb)) RETURN
	      IF (size(alb,1) < 2 .or. size(alb,2) < 2) RETURN
	      IF (numpatch < 1 .or. size(elm_patch%subfrc) < numpatch .or. size(alb,3) < numpatch) RETURN
	      IF (allocated(patchmask)) THEN
	         IF (size(patchmask) < numpatch) RETURN
	      ENDIF
	      IF (element < 1 .or. element > size(elm_patch%substt) .or. &
	          element > size(elm_patch%subend)) RETURN

	      istt = elm_patch%substt(element)
	      iend = elm_patch%subend(element)
	      IF (istt < 1 .or. iend < istt .or. iend > numpatch .or. &
	          iend > size(elm_patch%subfrc) .or. iend > size(alb,3)) RETURN

	      albedo_vis_direct = 0._r8
	      albedo_vis_diffuse = 0._r8
	      albedo_nir_direct = 0._r8
	      albedo_nir_diffuse = 0._r8
	      sumwt = 0._r8

	      DO patch = istt, iend
	         IF (allocated(patchmask)) THEN
	            IF (.not. patchmask(patch)) CYCLE
	         ENDIF
	         wt = elm_patch%subfrc(patch)
	         IF (.not. ieee_is_finite(wt) .or. wt < 0._r8) RETURN
	         IF (wt <= 0._r8) CYCLE
	         IF (.not. all(ieee_is_finite((/alb(1,1,patch), alb(1,2,patch), &
	                                        alb(2,1,patch), alb(2,2,patch)/)))) RETURN
	         IF (any((/alb(1,1,patch), alb(1,2,patch), alb(2,1,patch), alb(2,2,patch)/) < 0._r8) .or. &
	             any((/alb(1,1,patch), alb(1,2,patch), alb(2,1,patch), alb(2,2,patch)/) > 1._r8)) RETURN

	         albedo_vis_direct = albedo_vis_direct + wt * alb(1,1,patch)
	         albedo_vis_diffuse = albedo_vis_diffuse + wt * alb(1,2,patch)
	         albedo_nir_direct = albedo_nir_direct + wt * alb(2,1,patch)
	         albedo_nir_diffuse = albedo_nir_diffuse + wt * alb(2,2,patch)
	         sumwt = sumwt + wt
	      ENDDO

	      IF (sumwt <= 0._r8) RETURN
	      albedo_vis_direct = albedo_vis_direct / sumwt
	      albedo_vis_diffuse = albedo_vis_diffuse / sumwt
	      albedo_nir_direct = albedo_nir_direct / sumwt
	      albedo_nir_diffuse = albedo_nir_diffuse / sumwt
	      ierr = 0
	   END SUBROUTINE colm_mpas_get_element_albedo

	   SUBROUTINE colm_mpas_get_element_boundary_state(element, skin_temp, surface_humidity, &
	                                                    emissivity, roughness, albedo, ierr)
	      integer, intent(in) :: element
	      real(r8), intent(out) :: skin_temp, surface_humidity, emissivity, roughness, albedo
	      integer, intent(out) :: ierr

	      integer :: patch
	      integer :: istt
	      integer :: iend
	      real(r8) :: wt
	      real(r8) :: sumwt
	      real(r8) :: radiative_wt
	      real(r8) :: patch_humidity
	      real(r8) :: patch_albedo
	      real(r8) :: bad_value

	      ierr = 1
	      skin_temp = spval
	      surface_humidity = spval
	      emissivity = spval
	      roughness = spval
	      albedo = spval

	      IF (.not. allocated(elm_patch%substt) .or. .not. allocated(elm_patch%subend) .or. &
	          .not. allocated(elm_patch%subfrc)) RETURN
	      IF (.not. allocated(trad) .or. .not. allocated(qref) .or. .not. allocated(emis) .or. &
	          .not. allocated(z0m) .or. .not. allocated(alb)) RETURN
	      IF (numpatch < 1 .or. size(elm_patch%subfrc) < numpatch) RETURN
	      IF (allocated(patchmask)) THEN
	         IF (size(patchmask) < numpatch) RETURN
	      ENDIF
	      IF (element < 1 .or. element > size(elm_patch%substt) .or. &
	          element > size(elm_patch%subend)) RETURN

	      istt = elm_patch%substt(element)
	      iend = elm_patch%subend(element)
	      IF (istt < 1 .or. iend < istt .or. iend > numpatch) RETURN

	      skin_temp = 0._r8
	      surface_humidity = 0._r8
	      emissivity = 0._r8
	      roughness = 0._r8
	      albedo = 0._r8
	      sumwt = 0._r8
	      radiative_wt = 0._r8
	      bad_value = 0.5_r8 * abs(spval)

	      DO patch = istt, iend
	         IF (allocated(patchmask)) THEN
	            IF (.not. patchmask(patch)) CYCLE
	         ENDIF
	         wt = elm_patch%subfrc(patch)
	         IF (.not. ieee_is_finite(wt) .or. wt < 0._r8) RETURN
	         IF (wt <= 0._r8) CYCLE

	         patch_humidity = qref(patch)
	         IF (allocated(qsfc)) THEN
	            IF (patch <= size(qsfc)) THEN
	               IF (ieee_is_finite(qsfc(patch)) .and. abs(qsfc(patch)) < bad_value .and. &
	                   qsfc(patch) >= 0._r8 .and. qsfc(patch) < 1._r8) patch_humidity = qsfc(patch)
	            ENDIF
	         ENDIF
	         patch_albedo = sum(alb(:,:,patch)) / real(size(alb(:,:,patch)), r8)

	         IF (.not. all(ieee_is_finite((/trad(patch), patch_humidity, emis(patch), z0m(patch), &
	                                        patch_albedo/)))) RETURN
	         IF (any(abs((/trad(patch), patch_humidity, emis(patch), z0m(patch), patch_albedo/)) >= &
	                 bad_value)) RETURN
	         IF (trad(patch) <= 0._r8 .or. patch_humidity < 0._r8 .or. patch_humidity >= 1._r8 .or. &
	             emis(patch) <= 0._r8 .or. emis(patch) > 1._r8 .or. z0m(patch) < 0._r8 .or. &
	             patch_albedo < 0._r8 .or. patch_albedo > 1._r8) RETURN

	         skin_temp = skin_temp + wt * emis(patch) * trad(patch)**4
	         radiative_wt = radiative_wt + wt * emis(patch)
	         surface_humidity = surface_humidity + wt * patch_humidity
	         emissivity = emissivity + wt * emis(patch)
	         roughness = roughness + wt * z0m(patch)
	         albedo = albedo + wt * patch_albedo
	         sumwt = sumwt + wt
	      ENDDO

	      IF (sumwt <= 0._r8 .or. radiative_wt <= 0._r8) RETURN
	      skin_temp = (skin_temp / radiative_wt)**0.25_r8
	      surface_humidity = surface_humidity / sumwt
	      emissivity = emissivity / sumwt
	      roughness = roughness / sumwt
	      albedo = albedo / sumwt
	      ierr = 0
	   END SUBROUTINE colm_mpas_get_element_boundary_state

	   SUBROUTINE colm_mpas_get_element_river_state(element, water_depth, velocity, discharge, ierr)
#ifdef GridRiverLakeFlow
	      USE MOD_Grid_RiverLakeTimeVars, only: river_water_depth_elm, river_velocity_elm, river_discharge_elm
#endif
	      integer, intent(in) :: element
	      real(r8), intent(out) :: water_depth
	      real(r8), intent(out) :: velocity
	      real(r8), intent(out) :: discharge
	      integer, intent(out) :: ierr

	      real(r8) :: bad_value

	      water_depth = spval
	      velocity = spval
	      discharge = spval
	      ierr = 1
	      IF (.not. colm_mpas_initialized) RETURN

#ifdef GridRiverLakeFlow
	      IF (.not. allocated(river_water_depth_elm) .or. .not. allocated(river_velocity_elm) .or. &
	          .not. allocated(river_discharge_elm)) RETURN
	      IF (element < 1 .or. element > size(river_water_depth_elm) .or. &
	          element > size(river_velocity_elm) .or. element > size(river_discharge_elm)) RETURN

	      water_depth = river_water_depth_elm(element)
	      velocity = river_velocity_elm(element)
	      discharge = river_discharge_elm(element)
	      bad_value = 0.5_r8 * abs(spval)
	      IF (.not. all(ieee_is_finite((/water_depth, velocity, discharge/)))) RETURN
	      IF (abs(water_depth) < bad_value .and. water_depth < 0._r8) RETURN
#endif

	      ierr = 0
	   END SUBROUTINE colm_mpas_get_element_river_state

	   SUBROUTINE colm_mpas_get_element_state(element, canopy_water, snow_water, snow_depth, snow_cover, &
                                          leaf_area_index, vegetation_fraction, soil_availability, ground_temperature, &
                                          soil_liquid, soil_ice, soil_moisture, soil_temperature, ierr)
      integer, intent(in) :: element
      real(r8), intent(out) :: canopy_water, snow_water, snow_depth, snow_cover, leaf_area_index
      real(r8), intent(out) :: vegetation_fraction, soil_availability, ground_temperature
      real(r8), intent(out) :: soil_liquid(:), soil_ice(:), soil_moisture(:), soil_temperature(:)
      integer, intent(out) :: ierr

      integer :: patch
      integer :: istt
      integer :: iend
      integer :: n
      integer :: nlev
      real(r8) :: wt
      real(r8) :: sumwt
      real(r8) :: vegetation_wt
      real(r8) :: availability_wt
      real(r8) :: bad_value
      real(r8), parameter :: negative_tolerance = 1.e-10_r8
      real(r8) :: patch_canopy_water
      real(r8) :: patch_snow_water
      real(r8) :: patch_snow_depth
      real(r8) :: patch_snow_cover
      real(r8) :: patch_lai
      real(r8) :: patch_ground_temperature
      real(r8) :: patch_fveg
      real(r8) :: patch_availability
      real(r8) :: patch_liquid
      real(r8) :: patch_ice
      real(r8) :: patch_moisture
      real(r8) :: soil_wt(nl_soil)
      integer :: navailability

      ierr = 1
      canopy_water = spval
      snow_water = spval
      snow_depth = spval
      snow_cover = spval
      leaf_area_index = spval
      vegetation_fraction = spval
      soil_availability = spval
      ground_temperature = spval
      soil_liquid(:) = spval
      soil_ice(:) = spval
      soil_moisture(:) = spval
      soil_temperature(:) = spval

      IF (size(soil_liquid) < nl_soil) RETURN
      IF (size(soil_ice) < nl_soil) RETURN
      IF (size(soil_moisture) < nl_soil) RETURN
      IF (size(soil_temperature) < nl_soil) RETURN
      nlev = nl_soil
      IF (.not. allocated(elm_patch%substt)) RETURN
      IF (.not. allocated(elm_patch%subend)) RETURN
      IF (.not. allocated(elm_patch%subfrc)) RETURN
      IF (.not. allocated(ldew)) RETURN
      IF (.not. allocated(scv)) RETURN
      IF (.not. allocated(snowdp)) RETURN
      IF (.not. allocated(fsno)) RETURN
      IF (.not. allocated(lai)) RETURN
      IF (.not. allocated(t_grnd)) RETURN
      IF (.not. allocated(t_soisno)) RETURN
      IF (.not. allocated(wliq_soisno)) RETURN
      IF (.not. allocated(wice_soisno)) RETURN
      IF (numpatch < 1) RETURN
      IF (size(elm_patch%subfrc) < numpatch) RETURN
      IF (size(ldew) < numpatch .or. size(scv) < numpatch .or. size(snowdp) < numpatch .or. &
          size(fsno) < numpatch .or. size(lai) < numpatch .or. size(t_grnd) < numpatch) RETURN
      IF (allocated(patchmask)) THEN
         IF (size(patchmask) < numpatch) RETURN
      ENDIF
      IF (lbound(t_soisno, 1) > 1 .or. ubound(t_soisno, 1) < nl_soil) RETURN
      IF (lbound(wliq_soisno, 1) > 1 .or. ubound(wliq_soisno, 1) < nl_soil) RETURN
      IF (lbound(wice_soisno, 1) > 1 .or. ubound(wice_soisno, 1) < nl_soil) RETURN
      IF (lbound(t_soisno, 2) > 1 .or. ubound(t_soisno, 2) < numpatch) RETURN
      IF (lbound(wliq_soisno, 2) > 1 .or. ubound(wliq_soisno, 2) < numpatch) RETURN
      IF (lbound(wice_soisno, 2) > 1 .or. ubound(wice_soisno, 2) < numpatch) RETURN
      IF (element < 1 .or. element > size(elm_patch%substt) .or. &
          element > size(elm_patch%subend)) RETURN

      istt = elm_patch%substt(element)
      iend = elm_patch%subend(element)
      IF (istt < 1 .or. iend < istt .or. iend > numpatch .or. iend > size(elm_patch%subfrc)) RETURN

      bad_value = 0.5_r8 * abs(spval)
      canopy_water = 0._r8
      snow_water = 0._r8
      snow_depth = 0._r8
      snow_cover = 0._r8
      leaf_area_index = 0._r8
      vegetation_fraction = 0._r8
      soil_availability = 0._r8
      ground_temperature = 0._r8
      soil_liquid(:) = 0._r8
      soil_ice(:) = 0._r8
      soil_moisture(:) = 0._r8
      soil_temperature(:) = 0._r8
      soil_wt(:) = 0._r8
      sumwt = 0._r8
      vegetation_wt = 0._r8
      availability_wt = 0._r8

	      DO patch = istt, iend
	         IF (patch < 1 .or. patch > numpatch) THEN
	            WRITE(*,*) 'Error: CoLM2024 element state references invalid patch:', element, patch, numpatch
	            RETURN
	         ENDIF
	         IF (allocated(patchmask)) THEN
	            IF (.not. patchmask(patch)) CYCLE
	         ENDIF
         wt = elm_patch%subfrc(patch)
         IF (.not. ieee_is_finite(wt)) RETURN
         IF (wt <= 0._r8) CYCLE

         patch_canopy_water = ldew(patch)
         patch_snow_water = scv(patch)
         patch_snow_depth = snowdp(patch)
         patch_snow_cover = fsno(patch)
         patch_lai = lai(patch)
         patch_ground_temperature = t_grnd(patch)
         IF (.not. all(ieee_is_finite((/patch_canopy_water, patch_snow_water, patch_snow_depth, &
                                        patch_snow_cover, patch_lai, patch_ground_temperature/)))) RETURN
         IF (any(abs((/patch_canopy_water, patch_snow_water, patch_snow_depth, patch_snow_cover, &
                       patch_lai, patch_ground_temperature/)) >= bad_value)) RETURN
         IF (patch_canopy_water < -negative_tolerance .or. patch_snow_water < -negative_tolerance .or. &
             patch_snow_depth < -negative_tolerance .or. patch_snow_cover < -negative_tolerance .or. &
             patch_snow_cover > 1._r8 + negative_tolerance .or. patch_lai < -negative_tolerance) RETURN

         sumwt = sumwt + wt
         canopy_water = canopy_water + wt * max(0._r8, patch_canopy_water)
         snow_water = snow_water + wt * max(0._r8, patch_snow_water)
         snow_depth = snow_depth + wt * max(0._r8, patch_snow_depth)
         snow_cover = snow_cover + wt * max(0._r8, min(1._r8, patch_snow_cover))
         leaf_area_index = leaf_area_index + wt * max(0._r8, patch_lai)
         ground_temperature = ground_temperature + wt * patch_ground_temperature

         IF (allocated(fveg)) THEN
            IF (patch <= size(fveg)) THEN
               patch_fveg = fveg(patch)
               IF (abs(patch_fveg) < bad_value) THEN
                  vegetation_fraction = vegetation_fraction + wt * max(0._r8, min(1._r8, patch_fveg))
                  vegetation_wt = vegetation_wt + wt
               ENDIF
            ENDIF
         ENDIF

         patch_availability = 0._r8
         navailability = 0
         IF (allocated(rstfacsun_out)) THEN
            IF (patch <= size(rstfacsun_out)) THEN
               IF (abs(rstfacsun_out(patch)) < bad_value) THEN
                  patch_availability = patch_availability + max(0._r8, min(1._r8, rstfacsun_out(patch)))
                  navailability = navailability + 1
               ENDIF
            ENDIF
         ENDIF
         IF (allocated(rstfacsha_out)) THEN
            IF (patch <= size(rstfacsha_out)) THEN
               IF (abs(rstfacsha_out(patch)) < bad_value) THEN
                  patch_availability = patch_availability + max(0._r8, min(1._r8, rstfacsha_out(patch)))
                  navailability = navailability + 1
               ENDIF
            ENDIF
         ENDIF
         IF (navailability > 0) THEN
            soil_availability = soil_availability + wt * patch_availability / real(navailability, r8)
            availability_wt = availability_wt + wt
         ENDIF

         DO n = 1, nlev
            IF (.not. ieee_is_finite(dz_soi(n)) .or. dz_soi(n) <= 0._r8) RETURN
            IF (.not. all(ieee_is_finite((/t_soisno(n, patch), wliq_soisno(n, patch), &
                                           wice_soisno(n, patch)/)))) RETURN
            IF (any(abs((/t_soisno(n, patch), wliq_soisno(n, patch), &
                          wice_soisno(n, patch)/)) >= bad_value)) RETURN
            IF (wliq_soisno(n, patch) < -negative_tolerance .or. &
                wice_soisno(n, patch) < -negative_tolerance) RETURN

	            patch_liquid = max(0._r8, wliq_soisno(n, patch) / (denh2o * dz_soi(n)))
	            patch_ice = max(0._r8, wice_soisno(n, patch) / (denice * dz_soi(n)))
	            patch_moisture = patch_liquid + patch_ice
            soil_liquid(n) = soil_liquid(n) + wt * patch_liquid
            soil_ice(n) = soil_ice(n) + wt * patch_ice
            soil_moisture(n) = soil_moisture(n) + wt * patch_moisture
            soil_temperature(n) = soil_temperature(n) + wt * t_soisno(n, patch)
            soil_wt(n) = soil_wt(n) + wt
         ENDDO
      ENDDO

      IF (sumwt <= 0._r8) THEN
         canopy_water = spval
         snow_water = spval
         snow_depth = spval
         snow_cover = spval
         leaf_area_index = spval
         vegetation_fraction = spval
         soil_availability = spval
         ground_temperature = spval
         soil_liquid(:) = spval
         soil_ice(:) = spval
         soil_moisture(:) = spval
         soil_temperature(:) = spval
         RETURN
      ENDIF

      canopy_water = canopy_water / sumwt
      snow_water = snow_water / sumwt
      snow_depth = snow_depth / sumwt
      snow_cover = snow_cover / sumwt
      leaf_area_index = leaf_area_index / sumwt
      ground_temperature = ground_temperature / sumwt
      IF (vegetation_wt > 0._r8) THEN
         vegetation_fraction = vegetation_fraction / vegetation_wt
      ELSE
         vegetation_fraction = spval
      ENDIF
      IF (availability_wt > 0._r8) THEN
         soil_availability = soil_availability / availability_wt
      ELSE
         soil_availability = spval
      ENDIF

      IF (any(soil_wt(1:nlev) <= 0._r8)) RETURN
      DO n = 1, nlev
         soil_liquid(n) = soil_liquid(n) / soil_wt(n)
         soil_ice(n) = soil_ice(n) / soil_wt(n)
         soil_moisture(n) = soil_moisture(n) / soil_wt(n)
         soil_temperature(n) = soil_temperature(n) / soil_wt(n)
      ENDDO

      ierr = 0
   END SUBROUTINE colm_mpas_get_element_state

	END MODULE MOD_CoLM_MPAS_Interface
