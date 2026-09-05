! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module mpas_atmphys_driver_lsm_colm2024
 use mpas_kind_types
 use mpas_derived_types, only: block_type, dm_info
 use mpas_pool_routines
 use mpas_timekeeping, only: MPAS_Clock_type, MPAS_Time_type, MPAS_START_TIME, MPAS_STOP_TIME, &
                             mpas_get_clock_time, mpas_get_time
 use, intrinsic :: ieee_arithmetic, only: ieee_is_finite

 use mpas_atmphys_constants, only: R_d, R_v, cp
 use mpas_atmphys_utilities, only: physics_error_fatal
 use mpas_atmphys_vars, only: dt_pbl, xice_threshold, rrtmg_co2vmr

#ifdef MPAS_COLM2024
	 use mpi, only: MPI_COMM_NULL, MPI_SUCCESS, MPI_Comm_dup, MPI_Comm_free
	 use MOD_Precision, only: colm_r8 => r8
 use MOD_CoLM_MPAS_Interface, only: colm_mpas_initialize_from_namelist, colm_mpas_ready, &
                                    colm_mpas_set_element_forcing, &
	                                    colm_mpas_step, colm_mpas_get_element_surface, &
	                                    colm_mpas_get_element_albedo, &
	                                    colm_mpas_get_element_boundary_state, &
	                                    colm_mpas_get_element_state, &
	                                    colm_mpas_get_element_river_state, &
                                    colm_mpas_force_restart, &
                                    colm_mpas_finalize
 use MOD_Vars_Global, only: colm_nl_soil => nl_soil, colm_dz_soi => dz_soi, colm_z_soi => z_soi
	 use MOD_Const_Physical, only: colm_denh2o => denh2o, colm_denice => denice
 use MOD_TimeManager, only: colm_ticktime => ticktime, colm_adj2begin => adj2begin, &
                            colm_adj2end => adj2end, &
                            colm_julian2monthday => julian2monthday
 use MOD_MonthlyinSituCO2MaunaLoa, only: colm_init_monthly_co2_mlo => init_monthly_co2_mlo, &
                                         colm_get_monthly_co2_mlo => get_monthly_co2_mlo
 use MOD_Namelist, only: colm_use_ozone_data => DEF_USE_OZONEDATA
#endif

 implicit none
 private
 public :: init_lsm_colm2024, push_lsm_colm2024, step_lsm_colm2024, pull_lsm_colm2024
 public :: checkpoint_lsm_colm2024, finalize_lsm_colm2024

 logical, save :: colm_update_lai_next_step = .true.
#ifdef MPAS_COLM2024
 type colm_block_map_type
    integer :: local_block_id = -1
    integer, allocatable :: element_for_cell(:)
    type(colm_block_map_type), pointer :: next => null()
 end type colm_block_map_type

	 type(colm_block_map_type), pointer, save :: colm_block_maps => null()
	 logical, save :: colm_initialized = .false.
	 integer, save :: colm_mpas_comm = MPI_COMM_NULL
	 integer, save :: colm_step_idate(3) = -1
#endif

 contains

!=================================================================================================================
 subroutine init_lsm_colm2024(dminfo,configs,clock,block)
!=================================================================================================================

 type(dm_info),intent(in):: dminfo
 type(mpas_pool_type),intent(in):: configs
 type(MPAS_Clock_type),intent(in):: clock
 type(block_type),intent(in):: block
#ifdef MPAS_COLM2024
 logical :: colm_ready
	 logical,pointer:: config_do_restart
 integer :: colm_ierr
	 integer :: mpas_domain_comm
	 integer :: mpi_ierr
 integer :: colm_patch_count
 integer :: block_cells
 integer :: n_mpas_cells
 integer :: offset
 integer :: start_year, start_month, start_day, start_doy
 integer :: start_hour, start_minute, start_second, start_second_num, start_second_den
 integer :: stop_year, stop_doy
 integer :: stop_hour, stop_minute, stop_second, stop_second_num, stop_second_den
 integer :: mpas_start_idate(3), mpas_stop_idate(3)
 character(len=StrKIND),pointer:: config_colm2024_namelist
 character(len=StrKIND),pointer:: config_calendar_type
 character(len=StrKIND),pointer:: config_pbl_interval
 character(len=StrKIND),pointer:: config_pbl_scheme
 character(len=StrKIND),pointer:: config_radt_lw_scheme
 character(len=StrKIND),pointer:: config_radt_sw_scheme
 character(len=StrKIND),pointer:: config_sfclayer_scheme
 integer,pointer:: nCellsSolve
 integer,dimension(:),pointer:: indexToCellID, landmask
 integer,allocatable:: cell_to_element(:), mpas_cell_id(:), mpas_cell_landmask(:)
 real(kind=RKIND),dimension(:),pointer:: latCell, lonCell, areaCell
 real(kind=colm_r8),allocatable:: mpas_cell_lat(:), mpas_cell_lon(:), mpas_cell_area(:)
 real(kind=colm_r8) :: mpas_colm_timestep
 type(MPAS_Time_type):: start_time, stop_time
 type(block_type),pointer:: block_cursor
 type(colm_block_map_type),pointer:: block_map
 type(mpas_pool_type),pointer:: block_mesh, block_diag_physics, block_diag_physics_colm2024, block_sfc_input
#endif

#ifdef MPAS_COLM2024
	 call mpas_pool_get_config(configs,'config_colm2024_namelist',config_colm2024_namelist)
	 call mpas_pool_get_config(configs,'config_do_restart',config_do_restart)
	 if(trim(config_colm2024_namelist) == 'none' .or. trim(config_colm2024_namelist) == '') then
	    call physics_error_fatal("config_lsm_scheme='sf_colm2024' requires config_colm2024_namelist.")
	 endif

 call mpas_pool_get_config(configs,'config_pbl_scheme'     ,config_pbl_scheme     )
 call mpas_pool_get_config(configs,'config_pbl_interval'   ,config_pbl_interval   )
 call mpas_pool_get_config(configs,'config_radt_lw_scheme' ,config_radt_lw_scheme )
 call mpas_pool_get_config(configs,'config_radt_sw_scheme' ,config_radt_sw_scheme )
 call mpas_pool_get_config(configs,'config_sfclayer_scheme',config_sfclayer_scheme)
 if(trim(config_radt_lw_scheme) == 'off') then
    call physics_error_fatal("CoLM2024 requires an active MPAS longwave-radiation scheme.")
 endif
 if(trim(config_radt_sw_scheme) /= 'rrtmg_sw') then
    call physics_error_fatal("CoLM2024 requires config_radt_sw_scheme='rrtmg_sw' for VIS/NIR direct/diffuse fluxes.")
 endif
 if(trim(config_sfclayer_scheme) == 'off') then
    call physics_error_fatal("CoLM2024 requires an active MPAS surface-layer scheme for PBL coupling.")
 endif
 if(trim(config_pbl_scheme) == 'off') then
    call physics_error_fatal("CoLM2024 requires an active MPAS PBL scheme to feed surface fluxes back to the atmosphere.")
 endif
 if(trim(config_pbl_interval) /= 'none') then
    call physics_error_fatal("CoLM2024 requires config_pbl_interval='none' so its timestep matches each MPAS physics step.")
 endif

	 if(colm_initialized) return
	 if(associated(block % next)) return
	 if(.not. associated(block % domain)) call physics_error_fatal("CoLM2024 MPAS block is missing its owning domain.")

 call mpas_pool_get_config(configs,'config_calendar_type',config_calendar_type)
 if(trim(config_calendar_type) /= 'gregorian') then
    call physics_error_fatal("CoLM2024 embedded time management requires config_calendar_type='gregorian'.")
 endif

 start_time = mpas_get_clock_time(clock, MPAS_START_TIME, colm_ierr)
 if(colm_ierr /= 0) call physics_error_fatal("Unable to obtain the MPAS start time for CoLM2024.")
 call mpas_get_time(start_time, YYYY=start_year, MM=start_month, DD=start_day, DoY=start_doy, &
                    H=start_hour, M=start_minute, S=start_second, S_n=start_second_num, &
                    S_d=start_second_den, ierr=colm_ierr)
 if(colm_ierr /= 0) call physics_error_fatal("Unable to decode the MPAS start time for CoLM2024.")
 if(start_second_num /= 0) then
    call physics_error_fatal("CoLM2024 embedded time management requires an integer-second MPAS start time.")
 endif
 mpas_start_idate = (/start_year, start_doy, start_hour * 3600 + start_minute * 60 + start_second/)

 stop_time = mpas_get_clock_time(clock, MPAS_STOP_TIME, colm_ierr)
 if(colm_ierr /= 0) call physics_error_fatal("Unable to obtain the MPAS stop time for CoLM2024.")
 call mpas_get_time(stop_time, YYYY=stop_year, DoY=stop_doy, H=stop_hour, M=stop_minute, &
                    S=stop_second, S_n=stop_second_num, S_d=stop_second_den, ierr=colm_ierr)
 if(colm_ierr /= 0) call physics_error_fatal("Unable to decode the MPAS stop time for CoLM2024.")
 if(stop_second_num /= 0) then
    call physics_error_fatal("CoLM2024 embedded time management requires an integer-second MPAS stop time.")
 endif
 mpas_stop_idate = (/stop_year, stop_doy, stop_hour * 3600 + stop_minute * 60 + stop_second/)
 call colm_adj2end(mpas_stop_idate)

 mpas_colm_timestep = real(dt_pbl, colm_r8)
 if(.not. ieee_is_finite(mpas_colm_timestep) .or. mpas_colm_timestep <= 0._colm_r8 .or. &
    mpas_colm_timestep > 3600._colm_r8) then
    call physics_error_fatal("CoLM2024 requires an MPAS land-surface timestep in (0, 3600] seconds.")
 endif
 if(abs(mpas_colm_timestep - real(nint(mpas_colm_timestep), colm_r8)) > 1.e-6_colm_r8) then
    call physics_error_fatal("CoLM2024 embedded time management requires an integer-second MPAS timestep.")
 endif

 call colm2024_free_block_maps()

 n_mpas_cells = 0
 block_cursor => block % domain % blocklist
 do while(associated(block_cursor))
    call mpas_pool_get_subpool(block_cursor % structs,'mesh',block_mesh)
    call mpas_pool_get_dimension(block_mesh,'nCellsSolve',nCellsSolve)
    n_mpas_cells = n_mpas_cells + nCellsSolve
    block_cursor => block_cursor % next
 enddo
 allocate(mpas_cell_id(n_mpas_cells), mpas_cell_landmask(n_mpas_cells), &
          mpas_cell_lat(n_mpas_cells), mpas_cell_lon(n_mpas_cells), &
          mpas_cell_area(n_mpas_cells), cell_to_element(n_mpas_cells))

 offset = 0
 block_cursor => block % domain % blocklist
 do while(associated(block_cursor))
    call mpas_pool_get_subpool(block_cursor % structs,'mesh',block_mesh)
    call mpas_pool_get_dimension(block_mesh,'nCellsSolve',nCellsSolve)
    call mpas_pool_get_array(block_mesh,'indexToCellID',indexToCellID)
    call mpas_pool_get_array(block_mesh,'latCell',latCell)
    call mpas_pool_get_array(block_mesh,'lonCell',lonCell)
    call mpas_pool_get_array(block_mesh,'areaCell',areaCell)
    call mpas_pool_get_subpool(block_cursor % structs,'sfc_input',block_sfc_input)
    call mpas_pool_get_array(block_sfc_input,'landmask',landmask)

    block_cells = nCellsSolve
    if(block_cells > 0) then
       mpas_cell_id(offset+1:offset+block_cells) = indexToCellID(1:block_cells)
       mpas_cell_lat(offset+1:offset+block_cells) = real(latCell(1:block_cells), colm_r8)
       mpas_cell_lon(offset+1:offset+block_cells) = real(lonCell(1:block_cells), colm_r8)
       mpas_cell_area(offset+1:offset+block_cells) = real(areaCell(1:block_cells), colm_r8)
       mpas_cell_landmask(offset+1:offset+block_cells) = landmask(1:block_cells)
    endif
    call colm2024_add_block_map(block_cursor % localBlockID, size(indexToCellID))
    offset = offset + block_cells
    block_cursor => block_cursor % next
 enddo

#ifdef MPAS_USE_MPI_F08
	 mpas_domain_comm = dminfo % comm % mpi_val
#else
	 mpas_domain_comm = dminfo % comm
#endif
	 if(colm_mpas_comm /= MPI_COMM_NULL) then
	    call physics_error_fatal("CoLM2024 MPAS communication context was initialized more than once.")
	 endif
	 call MPI_Comm_dup(mpas_domain_comm, colm_mpas_comm, mpi_ierr)
	 if(mpi_ierr /= MPI_SUCCESS) then
	    call physics_error_fatal("MPAS could not create the CoLM2024 communication context.")
	 endif

	 call colm_mpas_initialize_from_namelist(trim(config_colm2024_namelist), colm_ierr, &
                                        mpas_comm=colm_mpas_comm, &
                                        mpas_cell_id=mpas_cell_id, &
                                        mpas_cell_lat=mpas_cell_lat, &
                                        mpas_cell_lon=mpas_cell_lon, &
                                        mpas_cell_area=mpas_cell_area, &
                                        mpas_cell_landmask=mpas_cell_landmask, &
                                        n_mpas_cells=n_mpas_cells, &
                                        cell_to_element=cell_to_element, &
                                        mpas_start_idate=mpas_start_idate, &
                                        mpas_stop_idate=mpas_stop_idate, &
	                                        mpas_timestep=mpas_colm_timestep, &
	                                        mpas_is_restart=config_do_restart)
 if(colm_ierr /= 0) then
    call colm2024_free_block_maps()
    if(colm_mpas_comm /= MPI_COMM_NULL) then
       call MPI_Comm_free(colm_mpas_comm, mpi_ierr)
       colm_mpas_comm = MPI_COMM_NULL
    endif
    call physics_error_fatal("CoLM2024 initialization from config_colm2024_namelist failed.")
 endif
 call colm_init_monthly_co2_mlo()

 offset = 0
 block_cursor => block % domain % blocklist
 do while(associated(block_cursor))
    call mpas_pool_get_subpool(block_cursor % structs,'mesh',block_mesh)
    call mpas_pool_get_dimension(block_mesh,'nCellsSolve',nCellsSolve)
    call colm2024_find_block_map(block_cursor % localBlockID, block_map)
    if(.not. associated(block_map)) call physics_error_fatal("CoLM2024 MPAS block map was not created.")
    block_map % element_for_cell(:) = 0
    block_cells = nCellsSolve
    if(block_cells > 0) block_map % element_for_cell(1:block_cells) = cell_to_element(offset+1:offset+block_cells)
    offset = offset + block_cells
    block_cursor => block_cursor % next
 enddo

 call colm_mpas_ready(colm_ready, colm_patch_count)
 if(.not. colm_ready) call colm2024_adapter_not_ready

	 colm_update_lai_next_step = .true.
	 colm_step_idate(:) = mpas_start_idate(:)
	 call colm_adj2end(colm_step_idate)
	 call colm2024_update_atmospheric_co2(colm_step_idate)
	 colm_initialized = .true.

	 block_cursor => block % domain % blocklist
	 do while(associated(block_cursor))
	    call mpas_pool_get_subpool(block_cursor % structs,'mesh'                    ,block_mesh                    )
	    call mpas_pool_get_subpool(block_cursor % structs,'diag_physics'            ,block_diag_physics            )
	    call mpas_pool_get_subpool(block_cursor % structs,'diag_physics_colm2024'    ,block_diag_physics_colm2024    )
	    call mpas_pool_get_subpool(block_cursor % structs,'sfc_input'               ,block_sfc_input               )
	    call mpas_pool_get_dimension(block_mesh,'nCellsSolve',nCellsSolve)
	    call colm2024_find_block_map(block_cursor % localBlockID, block_map)
	    if(.not. associated(block_map)) call physics_error_fatal("CoLM2024 MPAS block map was not created.")
	    if(nCellsSolve > 0) then
	       call colm2024_pull_mpas_surface(block_map % element_for_cell, block_diag_physics, &
	                                        block_diag_physics_colm2024, block_sfc_input, &
	                                        mpas_colm_timestep, 1, nCellsSolve, state_only=.true.)
	    endif
	    block_cursor => block_cursor % next
	 enddo
#else
	 call colm2024_adapter_not_built
#endif

 end subroutine init_lsm_colm2024

!=================================================================================================================
 subroutine push_lsm_colm2024(block,configs,mesh,state,time_lev,diag,diag_physics,atm_input,sfc_input,its,ite)
!=================================================================================================================

 type(block_type),intent(in):: block
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: state
 type(mpas_pool_type),intent(in):: diag
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(in):: atm_input
 type(mpas_pool_type),intent(inout):: sfc_input
 integer,intent(in):: time_lev
 integer,intent(in):: its,ite
#ifdef MPAS_COLM2024
 logical :: colm_ready
 integer :: colm_patch_count
 type(colm_block_map_type),pointer:: block_map
#endif

#ifdef MPAS_COLM2024
 call colm_mpas_ready(colm_ready, colm_patch_count)
 if(.not. colm_ready) call colm2024_adapter_not_ready
 call colm2024_find_block_map(block % localBlockID, block_map)
 if(.not. associated(block_map)) call physics_error_fatal("CoLM2024 MPAS cell-to-element map is not initialized.")
 if(size(block_map % element_for_cell) < ite) call physics_error_fatal("CoLM2024 MPAS cell-to-element map is smaller " // &
                                                                       "than the MPAS cell range.")

 call colm2024_push_mpas_forcing(block_map % element_for_cell, configs, mesh, state, time_lev, diag, diag_physics, &
	                                 atm_input, sfc_input, its, ite)
#else
 call colm2024_adapter_not_built
#endif

 end subroutine push_lsm_colm2024

!=================================================================================================================
 subroutine step_lsm_colm2024()
!=================================================================================================================

#ifdef MPAS_COLM2024
 logical :: colm_ready
 logical :: colm_update_daily_lai
 integer :: colm_ierr
 integer :: colm_patch_count
 integer :: colm_previous_date(3)
 integer :: colm_current_date(3)
 real(kind=colm_r8) :: colm_dt

 call colm_mpas_ready(colm_ready, colm_patch_count)
 if(.not. colm_ready) call colm2024_adapter_not_ready
 colm_dt = real(dt_pbl, colm_r8)
 if(colm_dt <= 0._colm_r8) call physics_error_fatal("CoLM2024 requires a positive land-surface timestep.")
 if(abs(colm_dt - real(nint(colm_dt), colm_r8)) > 1.e-6_colm_r8) then
    call physics_error_fatal("CoLM2024 embedded time management requires an integer-second land-surface timestep.")
 endif
 colm_previous_date(:) = colm_step_idate(:)
 call colm_adj2begin(colm_previous_date)
 call colm_ticktime(colm_dt, colm_step_idate)
 colm_current_date(:) = colm_step_idate(:)
 call colm_adj2begin(colm_current_date)
 colm_update_daily_lai = colm_update_lai_next_step
	 call colm_mpas_step(colm_step_idate, colm_dt, colm_update_daily_lai, .true., .false., colm_ierr)
	 if(colm_ierr /= 0) call physics_error_fatal("CoLM2024 driver returned before completing a land-surface step.")
	 call colm2024_update_atmospheric_co2(colm_step_idate)
	 colm_update_lai_next_step = any(colm_current_date(1:2) /= colm_previous_date(1:2))
#else
 call colm2024_adapter_not_built
#endif

 end subroutine step_lsm_colm2024

!=================================================================================================================
 subroutine pull_lsm_colm2024(block,diag_physics,diag_physics_colm2024,sfc_input,its,ite)
!=================================================================================================================

 type(block_type),intent(in):: block
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: diag_physics_colm2024
 type(mpas_pool_type),intent(inout):: sfc_input
 integer,intent(in):: its,ite
#ifdef MPAS_COLM2024
 logical :: colm_ready
 integer :: colm_patch_count
 type(colm_block_map_type),pointer:: block_map

 call colm_mpas_ready(colm_ready, colm_patch_count)
 if(.not. colm_ready) call colm2024_adapter_not_ready
 call colm2024_find_block_map(block % localBlockID, block_map)
 if(.not. associated(block_map)) call physics_error_fatal("CoLM2024 MPAS cell-to-element map is not initialized.")
 if(size(block_map % element_for_cell) < ite) call physics_error_fatal("CoLM2024 MPAS cell-to-element map is smaller " // &
                                                                       "than the MPAS cell range.")

 call colm2024_pull_mpas_surface(block_map % element_for_cell, diag_physics, diag_physics_colm2024, &
                                 sfc_input, real(dt_pbl, colm_r8), its, ite)
#else
 call colm2024_adapter_not_built
#endif

 end subroutine pull_lsm_colm2024

!=================================================================================================================
 subroutine checkpoint_lsm_colm2024()
!=================================================================================================================

#ifdef MPAS_COLM2024
	 integer :: colm_ierr

 if(.not. colm_initialized) return
 call colm_mpas_force_restart(colm_ierr)
 if(colm_ierr /= 0) call physics_error_fatal("CoLM2024 failed to write a checkpoint with the MPAS restart stream.")
#endif

 end subroutine checkpoint_lsm_colm2024

!=================================================================================================================
	 subroutine finalize_lsm_colm2024()
	!=================================================================================================================

#ifdef MPAS_COLM2024
			 integer :: colm_ierr, mpi_ierr

		 if(.not. colm_initialized) return
			 call colm_mpas_finalize(colm_ierr)
			 if(colm_ierr /= 0) call physics_error_fatal("CoLM2024 finalization failed.")
			 if(colm_mpas_comm == MPI_COMM_NULL) then
			    call physics_error_fatal("CoLM2024 MPAS communication context is missing during finalization.")
			 endif
			 call MPI_Comm_free(colm_mpas_comm, mpi_ierr)
			 if(mpi_ierr /= MPI_SUCCESS) then
			    call physics_error_fatal("MPAS could not release the CoLM2024 communication context.")
			 endif
			 colm_mpas_comm = MPI_COMM_NULL
			 call colm2024_free_block_maps()
		 colm_update_lai_next_step = .true.
		 colm_step_idate(:) = -1
		 rrtmg_co2vmr = 379.e-6_RKIND
		 colm_initialized = .false.
#endif

	 end subroutine finalize_lsm_colm2024

#ifdef MPAS_COLM2024
!=================================================================================================================
 subroutine colm2024_update_atmospheric_co2(idate)
!=================================================================================================================

 integer,intent(in):: idate(3)
 integer:: lookup_idate(3), lookup_month, lookup_mday
 real(kind=colm_r8):: co2_mole_fraction

 lookup_idate(:) = idate(:)
 call colm_adj2begin(lookup_idate)
 call colm_julian2monthday(lookup_idate(1), lookup_idate(2), lookup_month, lookup_mday)
 co2_mole_fraction = 1.e-6_colm_r8 * colm_get_monthly_co2_mlo(lookup_idate(1), lookup_month)
 if(.not. ieee_is_finite(co2_mole_fraction) .or. co2_mole_fraction <= 0._colm_r8) then
    call physics_error_fatal("CoLM2024 produced an invalid atmospheric CO2 mole fraction.")
 endif
 rrtmg_co2vmr = real(co2_mole_fraction, RKIND)

 end subroutine colm2024_update_atmospheric_co2

!=================================================================================================================
 subroutine colm2024_add_block_map(local_block_id, nCells)
!=================================================================================================================

 integer,intent(in):: local_block_id
 integer,intent(in):: nCells
 type(colm_block_map_type),pointer:: block_map, cursor

 allocate(block_map)
 block_map % local_block_id = local_block_id
 allocate(block_map % element_for_cell(nCells))
 block_map % element_for_cell(:) = 0
 nullify(block_map % next)

 if(.not. associated(colm_block_maps)) then
    colm_block_maps => block_map
 else
    cursor => colm_block_maps
    do while(associated(cursor % next))
       cursor => cursor % next
    enddo
    cursor % next => block_map
 endif

 end subroutine colm2024_add_block_map

!=================================================================================================================
 subroutine colm2024_find_block_map(local_block_id, block_map)
!=================================================================================================================

 integer,intent(in):: local_block_id
 type(colm_block_map_type),pointer:: block_map

 block_map => colm_block_maps
 do while(associated(block_map))
    if(block_map % local_block_id == local_block_id) return
    block_map => block_map % next
 enddo
 nullify(block_map)

 end subroutine colm2024_find_block_map

!=================================================================================================================
 subroutine colm2024_free_block_maps()
!=================================================================================================================

 type(colm_block_map_type),pointer:: block_map, next_map

 block_map => colm_block_maps
 do while(associated(block_map))
    next_map => block_map % next
    if(allocated(block_map % element_for_cell)) deallocate(block_map % element_for_cell)
    deallocate(block_map)
    block_map => next_map
 enddo
 nullify(colm_block_maps)

 end subroutine colm2024_free_block_maps

!=================================================================================================================
 subroutine colm2024_push_mpas_forcing(element_for_cell,configs,mesh,state,time_lev,diag,diag_physics,atm_input, &
	                                       sfc_input,its,ite)
!=================================================================================================================

	 integer,dimension(:),intent(in):: element_for_cell
	 type(mpas_pool_type),intent(in):: configs
	 type(mpas_pool_type),intent(in):: mesh
	 type(mpas_pool_type),intent(in):: state
	 type(mpas_pool_type),intent(in):: diag
	 type(mpas_pool_type),intent(in):: diag_physics
	 type(mpas_pool_type),intent(in):: atm_input
	 type(mpas_pool_type),intent(in):: sfc_input
	 integer,intent(in):: time_lev
	 integer,intent(in):: its,ite

	 logical,pointer:: config_o3climatology
	 logical:: micro_has_solid
	 character(len=StrKIND),pointer:: config_convection_scheme, config_microp_scheme
	 integer,pointer:: index_qv
	 integer:: i, k
	 integer:: ierr, ozone_ierr
	 real(kind=colm_r8):: aerdep(14)
	 real(kind=colm_r8),parameter:: precip_negative_tolerance = 1.e-10_colm_r8
	 real(kind=colm_r8):: co2_mole_fraction, dt, micro_rain, micro_snow, micro_total, precip_tolerance, &
	                      hgt, hpbl_forcing, oro, ozone_ppbv, &
	                      pbot, psrf, prc, prl, qair, &
	                      rain, rhoair, snow, frl_down, &
	                      solld, soll, solsd, sols, spectral_sw, spectral_swdir, spectral_swdif, &
	                      sw_consistency_tolerance, swdir, swdif, solar_cosine, tair, uair, vair
	 real(kind=RKIND):: qv_cell
	 real(kind=RKIND),dimension(:),pointer:: coszr, glw, hpbl, swddir, swddif, swvisdir, swvisdif, swnirdir, swnirdif
	 real(kind=RKIND),dimension(:),pointer:: surface_pressure, xice, xland
	 real(kind=RKIND),dimension(:),pointer:: pin
	 real(kind=RKIND),dimension(:),pointer:: raincv, rainncv, snowncv, graupelncv
	 real(kind=RKIND),dimension(:,:),pointer:: exner, o3clim, pressure_b, pressure_p, theta_m, u, v, zgrid
	 real(kind=RKIND),dimension(:,:,:),pointer:: scalars

 call mpas_pool_get_config(configs,'config_convection_scheme',config_convection_scheme)
	 call mpas_pool_get_config(configs,'config_microp_scheme'    ,config_microp_scheme    )
	 call mpas_pool_get_config(configs,'config_o3climatology'    ,config_o3climatology    )
	 if(colm_use_ozone_data .and. .not. config_o3climatology) then
	    call physics_error_fatal("CoLM2024 DEF_USE_OZONEDATA requires MPAS config_o3climatology=true.")
	 endif

	 call mpas_pool_get_dimension(state,'index_qv',index_qv)
	 call mpas_pool_get_array(mesh,'zgrid',zgrid)
	 call mpas_pool_get_array(state,'theta_m',theta_m,time_lev)
	 call mpas_pool_get_array(state,'scalars',scalars,time_lev)
	 call mpas_pool_get_array(diag,'exner'                 ,exner     )
	 call mpas_pool_get_array(diag,'pressure_base'         ,pressure_b)
	 call mpas_pool_get_array(diag,'pressure_p'            ,pressure_p)
	 call mpas_pool_get_array(diag,'uReconstructZonal'     ,u         )
	 call mpas_pool_get_array(diag,'uReconstructMeridional',v         )
	 call mpas_pool_get_array(diag,'surface_pressure',surface_pressure)
	 if(colm_use_ozone_data) then
	    call mpas_pool_get_array(atm_input,'pin',pin)
	    call mpas_pool_get_array(diag_physics,'o3clim',o3clim)
	 endif
	 call mpas_pool_get_array(diag_physics,'glw'   ,glw   )
	 call mpas_pool_get_array(diag_physics,'coszr' ,coszr )
	 call mpas_pool_get_array(diag_physics,'hpbl'  ,hpbl  )
	 call mpas_pool_get_array(diag_physics,'swddir',swddir)
	 call mpas_pool_get_array(diag_physics,'swddif',swddif)
	 call mpas_pool_get_array(diag_physics,'swvisdir',swvisdir)
	 call mpas_pool_get_array(diag_physics,'swvisdif',swvisdif)
	 call mpas_pool_get_array(diag_physics,'swnirdir',swnirdir)
	 call mpas_pool_get_array(diag_physics,'swnirdif',swnirdif)
	 if(config_convection_scheme .ne. 'off') call mpas_pool_get_array(diag_physics,'raincv',raincv)
	 micro_has_solid = .false.
	 select case(trim(config_microp_scheme))
	 case('off')
	 case('mp_kessler')
	    call mpas_pool_get_array(diag_physics,'rainncv',rainncv)
	 case('mp_thompson','mp_thompson_aerosols','mp_wsm6')
	    micro_has_solid = .true.
	    call mpas_pool_get_array(diag_physics,'rainncv'   ,rainncv   )
	    call mpas_pool_get_array(diag_physics,'snowncv'   ,snowncv   )
	    call mpas_pool_get_array(diag_physics,'graupelncv',graupelncv)
	 case default
	    call physics_error_fatal("CoLM2024 encountered an unsupported MPAS microphysics scheme.")
	 end select
	 call mpas_pool_get_array(sfc_input,'xland',xland)
	 call mpas_pool_get_array(sfc_input,'xice' ,xice )

 dt = real(dt_pbl, colm_r8)
 if(.not. ieee_is_finite(dt) .or. dt <= 0._colm_r8) then
    call physics_error_fatal("CoLM2024 precipitation forcing requires a positive finite timestep.")
 endif
	 co2_mole_fraction = real(rrtmg_co2vmr, colm_r8)
	 if(.not. ieee_is_finite(co2_mole_fraction) .or. co2_mole_fraction <= 0._colm_r8) then
	    call physics_error_fatal("CoLM2024 produced an invalid atmospheric CO2 mole fraction.")
	 endif
	 aerdep(:) = 0._colm_r8

	 k = lbound(theta_m, 1)
	 do i = its,ite
	    if(element_for_cell(i) <= 0) call physics_error_fatal("CoLM2024 MPAS cell-to-element map has an invalid element index.")

	    if(.not. ieee_is_finite(xland(i)) .or. .not. ieee_is_finite(xice(i))) then
	       call physics_error_fatal("MPAS supplied a non-finite land-ocean or sea-ice mask to CoLM2024.")
	    endif
	    if(xland(i) < 1._RKIND .or. xland(i) > 2._RKIND .or. xice(i) < 0._RKIND .or. xice(i) > 1._RKIND) then
	       call physics_error_fatal("MPAS supplied an out-of-range land-ocean or sea-ice mask to CoLM2024.")
	    endif
	    if(config_convection_scheme .ne. 'off') then
	       if(.not. ieee_is_finite(raincv(i))) then
	          call physics_error_fatal("MPAS supplied non-finite convective precipitation to CoLM2024.")
	       endif
	       if(real(raincv(i), colm_r8) < -precip_negative_tolerance) &
	          call physics_error_fatal("MPAS supplied negative convective precipitation to CoLM2024.")
	    endif
	    if(config_microp_scheme .ne. 'off') then
	       if(.not. ieee_is_finite(rainncv(i)) .or. &
	          real(rainncv(i), colm_r8) < -precip_negative_tolerance) &
	          call physics_error_fatal("MPAS supplied invalid total microphysics precipitation to CoLM2024.")
	       if(micro_has_solid) then
	          if(.not. all(ieee_is_finite((/snowncv(i), graupelncv(i)/))) .or. &
	             real(snowncv(i), colm_r8) < -precip_negative_tolerance .or. &
	             real(graupelncv(i), colm_r8) < -precip_negative_tolerance) &
	             call physics_error_fatal("MPAS supplied invalid solid microphysics precipitation to CoLM2024.")
	       endif
	    endif

	    psrf = real(surface_pressure(i), colm_r8)
	    pbot = real(pressure_b(k,i) + pressure_p(k,i), colm_r8)
	    if(.not. ieee_is_finite(scalars(index_qv,k,i))) then
	       call physics_error_fatal("MPAS supplied non-finite lowest-level water vapor to CoLM2024.")
	    endif
	    if(scalars(index_qv,k,i) < -1.e-10_RKIND) then
	       call physics_error_fatal("MPAS supplied negative lowest-level water vapor to CoLM2024.")
	    endif
	    qv_cell = max(0._RKIND, scalars(index_qv,k,i))
	    tair = real((theta_m(k,i) / (1._RKIND + R_v / R_d * qv_cell)) * exner(k,i), colm_r8)
	    qair = real(qv_cell / (1._RKIND + qv_cell), colm_r8)
	    uair = real(u(k,i), colm_r8)
	    vair = real(v(k,i), colm_r8)
	    hgt = 2._colm_r8
	    if(ubound(zgrid, 1) >= k + 1) hgt = max(2._colm_r8, real(0.5_RKIND * (zgrid(k+1,i) - zgrid(k,i)), colm_r8))
	    rhoair = pbot / (real(R_d, colm_r8) * tair * (1._colm_r8 + 0.61_colm_r8 * qair))
	    if(.not. ieee_is_finite(rhoair) .or. rhoair <= 0._colm_r8) then
	       call physics_error_fatal("MPAS atmospheric state produced invalid air density for CoLM2024.")
	    endif
	    prc = 0._colm_r8
	    prl = 0._colm_r8
	    rain = 0._colm_r8
	    snow = 0._colm_r8

	    if(config_convection_scheme .ne. 'off') then
	       prc = max(0._colm_r8, real(raincv(i), colm_r8) / dt)
	       rain = rain + prc
	    endif
	    if(config_microp_scheme .ne. 'off') then
	       micro_total = max(0._colm_r8, real(rainncv(i), colm_r8))
	       micro_snow = 0._colm_r8
	       if(micro_has_solid) micro_snow = max(0._colm_r8, real(snowncv(i), colm_r8)) + &
	                                          max(0._colm_r8, real(graupelncv(i), colm_r8))
	       precip_tolerance = max(1.e-10_colm_r8, 1.e-6_colm_r8 * max(1._colm_r8, micro_total))
	       if(micro_snow > micro_total + precip_tolerance) then
	          call physics_error_fatal("MPAS solid grid-scale precipitation exceeds total precipitation for CoLM2024.")
	       endif
	       micro_snow = min(micro_total, micro_snow) / dt
	       micro_rain = max(0._colm_r8, micro_total / dt - micro_snow)
	       prl = micro_total / dt
	       rain = rain + micro_rain
	       snow = snow + micro_snow
	    endif

	    if(.not. all(ieee_is_finite((/real(swddir(i), colm_r8), real(swddif(i), colm_r8), &
	                                   real(swvisdir(i), colm_r8), real(swvisdif(i), colm_r8), &
	                                   real(swnirdir(i), colm_r8), real(swnirdif(i), colm_r8)/)))) then
	       call physics_error_fatal("MPAS supplied non-finite shortwave forcing to CoLM2024.")
	    endif
	    if(any((/real(swddir(i), colm_r8), real(swddif(i), colm_r8), &
	              real(swvisdir(i), colm_r8), real(swvisdif(i), colm_r8), &
	              real(swnirdir(i), colm_r8), real(swnirdif(i), colm_r8)/) < -1.e-6_colm_r8)) then
	       call physics_error_fatal("MPAS supplied negative shortwave forcing to CoLM2024.")
	    endif
	    swdir = max(0._colm_r8, real(swddir(i), colm_r8))
	    swdif = max(0._colm_r8, real(swddif(i), colm_r8))
	    sols = max(0._colm_r8, real(swvisdir(i), colm_r8))
	    solsd = max(0._colm_r8, real(swvisdif(i), colm_r8))
	    soll = max(0._colm_r8, real(swnirdir(i), colm_r8))
	    solld = max(0._colm_r8, real(swnirdif(i), colm_r8))
	    spectral_swdir = sols + soll
	    spectral_swdif = solsd + solld
	    spectral_sw = spectral_swdir + spectral_swdif
	    solar_cosine = real(coszr(i), colm_r8)
	    if(.not. ieee_is_finite(solar_cosine) .or. solar_cosine < -1._colm_r8 .or. &
	       solar_cosine > 1._colm_r8) then
	       call physics_error_fatal("MPAS supplied an invalid solar-zenith cosine to CoLM2024.")
	    endif
	    if(spectral_sw <= 0._colm_r8 .and. swdir + swdif > 0._colm_r8) then
	       call physics_error_fatal("MPAS shortwave forcing lacks the VIS/NIR direct/diffuse components required by CoLM2024.")
	    endif
	    sw_consistency_tolerance = max(1._colm_r8, 1.e-3_colm_r8 * max(swdir, spectral_swdir))
	    if(abs(swdir - spectral_swdir) > sw_consistency_tolerance) then
	       call physics_error_fatal("MPAS broadband and VIS/NIR direct shortwave forcing are inconsistent for CoLM2024.")
	    endif
	    sw_consistency_tolerance = max(1._colm_r8, 1.e-3_colm_r8 * max(swdif, spectral_swdif))
	    if(abs(swdif - spectral_swdif) > sw_consistency_tolerance) then
	       call physics_error_fatal("MPAS broadband and VIS/NIR diffuse shortwave forcing are inconsistent for CoLM2024.")
	    endif
	    oro = 1._colm_r8
	    if(xland(i) >= 1.5_RKIND) oro = 0._colm_r8
	    if(xice(i) >= xice_threshold) oro = 2._colm_r8
	    hpbl_forcing = real(hpbl(i), colm_r8)
	    if(.not. ieee_is_finite(hpbl_forcing) .or. hpbl_forcing < 0._colm_r8) then
	       call physics_error_fatal("MPAS supplied an invalid boundary-layer height to CoLM2024.")
	    endif

	    frl_down = real(glw(i), colm_r8)
	    if(.not. all(ieee_is_finite((/psrf, pbot, tair, qair, uair, vair, prc, prl, rain, snow, &
	                                   sols, soll, solsd, solld, frl_down, hgt, rhoair/)))) then
	       call physics_error_fatal("MPAS supplied non-finite atmospheric forcing to CoLM2024.")
	    endif
	    if(frl_down < -1.e-6_colm_r8) then
	       call physics_error_fatal("MPAS supplied negative downward longwave forcing to CoLM2024.")
	    endif
	    frl_down = max(0._colm_r8, frl_down)
	    if(psrf <= 0._colm_r8 .or. pbot <= 0._colm_r8 .or. tair <= 0._colm_r8 .or. &
	       qair < 0._colm_r8 .or. qair >= 1._colm_r8) then
	       call physics_error_fatal("MPAS supplied out-of-range pressure, temperature, or humidity to CoLM2024.")
	    endif
	    ozone_ppbv = 100._colm_r8
	    if(colm_use_ozone_data) then
	       call colm2024_interpolate_ozone(pbot, pin, o3clim(:,i), ozone_ppbv, ozone_ierr)
	       if(ozone_ierr /= 0) call physics_error_fatal("Failed to interpolate MPAS ozone forcing for CoLM2024.")
	    endif

	    call colm_mpas_set_element_forcing(element_for_cell(i), pbot * co2_mole_fraction, &
	                                       pbot * 0.209_colm_r8, uair, vair, tair, qair, &
	                                       prc, prl, rain, snow, solar_cosine, psrf, pbot, &
	                                       sols, soll, solsd, solld, &
	                                       frl_down, hgt, hgt, hgt, rhoair, hpbl_forcing, &
	                                       aerdep, oro=oro, ozone=ozone_ppbv, ierr=ierr)
	    if(ierr /= 0) call physics_error_fatal("Failed to pass MPAS forcing into CoLM2024.")
	 enddo

 end subroutine colm2024_push_mpas_forcing

!=================================================================================================================
 subroutine colm2024_interpolate_ozone(target_pressure,pressure_levels,ozone_profile,ozone_ppbv,ierr)
!=================================================================================================================

 real(kind=colm_r8),intent(in):: target_pressure
 real(kind=RKIND),dimension(:),intent(in):: pressure_levels, ozone_profile
 real(kind=colm_r8),intent(out):: ozone_ppbv
 integer,intent(out):: ierr

 integer:: i, lower_index, upper_index
 real(kind=colm_r8):: lower_pressure, upper_pressure, weight

 ierr = 1
 ozone_ppbv = -1._colm_r8
 if(size(pressure_levels) /= size(ozone_profile) .or. size(pressure_levels) < 1) return
 if(.not. ieee_is_finite(target_pressure) .or. target_pressure <= 0._colm_r8) return

 lower_index = 0
 upper_index = 0
 lower_pressure = -huge(1._colm_r8)
 upper_pressure = huge(1._colm_r8)
 do i = 1,size(pressure_levels)
    if(.not. ieee_is_finite(pressure_levels(i)) .or. pressure_levels(i) <= 0._RKIND) cycle
    if(.not. ieee_is_finite(ozone_profile(i)) .or. ozone_profile(i) < 0._RKIND) cycle
    if(real(pressure_levels(i),colm_r8) <= target_pressure .and. &
       real(pressure_levels(i),colm_r8) > lower_pressure) then
       lower_pressure = real(pressure_levels(i),colm_r8)
       lower_index = i
    endif
    if(real(pressure_levels(i),colm_r8) >= target_pressure .and. &
       real(pressure_levels(i),colm_r8) < upper_pressure) then
       upper_pressure = real(pressure_levels(i),colm_r8)
       upper_index = i
    endif
 enddo

 if(lower_index == 0 .and. upper_index == 0) return
 if(lower_index == 0) lower_index = upper_index
 if(upper_index == 0) upper_index = lower_index
 if(lower_index == upper_index) then
    ozone_ppbv = 1.e9_colm_r8 * real(ozone_profile(lower_index),colm_r8)
 else
    weight = (log(target_pressure) - log(lower_pressure)) / (log(upper_pressure) - log(lower_pressure))
    ozone_ppbv = 1.e9_colm_r8 * ((1._colm_r8 - weight) * real(ozone_profile(lower_index),colm_r8) + &
                                  weight * real(ozone_profile(upper_index),colm_r8))
 endif
 if(.not. ieee_is_finite(ozone_ppbv) .or. ozone_ppbv < 0._colm_r8) return
 ierr = 0

 end subroutine colm2024_interpolate_ozone

!=================================================================================================================
subroutine colm2024_pull_mpas_surface(element_for_cell,diag_physics,diag_physics_colm2024,sfc_input,dt,its,ite,state_only)
!=================================================================================================================

 integer,dimension(:),intent(in):: element_for_cell
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: diag_physics_colm2024
 type(mpas_pool_type),intent(inout):: sfc_input
 real(kind=colm_r8),intent(in):: dt
 integer,intent(in):: its,ite
 logical,intent(in),optional:: state_only

 integer:: i, ns, nlev, nvalid_soil
 integer:: ierr
 logical:: pull_fluxes
 real(kind=colm_r8):: sensible, latent, evaporation, ground_heat, runoff
	 real(kind=colm_r8):: surface_runoff, subsurface_runoff, skin_temp, t2m_colm, q2m_colm, u10m_colm, v10m_colm, qsfc_colm
 real(kind=colm_r8):: emissivity, roughness, albedo
 real(kind=colm_r8):: albedo_vis_direct, albedo_vis_diffuse, albedo_nir_direct, albedo_nir_diffuse
 real(kind=colm_r8):: friction_velocity, mechanical_friction_velocity
 real(kind=colm_r8):: stability_zeta, bulk_richardson, momentum_profile, heat_profile
 real(kind=colm_r8):: moisture_profile, air_density, heat_exchange_velocity, moisture_exchange_velocity
 real(kind=colm_r8):: heat_exchange_velocity_2m, moisture_exchange_velocity_2m
 real(kind=colm_r8):: momentum_coefficient, enthalpy_coefficient
 real(kind=colm_r8):: momentum_coefficient_10m, inverse_monin_obukhov
 real(kind=colm_r8):: canopy_water, snow_water, snow_depth, snow_cover, leaf_area_index, ground_temperature
	 real(kind=colm_r8):: vegetation_fraction, soil_availability
	 real(kind=colm_r8):: river_water_depth, river_velocity, river_discharge
 real(kind=colm_r8):: soil_moisture_sum
 real(kind=colm_r8),parameter:: colm_state_bad_value = 1.e30_colm_r8
	 real(kind=colm_r8),dimension(:),allocatable:: soil_liquid, soil_ice, soil_moisture, soil_temperature
	 real(kind=colm_r8),dimension(:),allocatable:: soil_water_equivalent

 real(kind=RKIND),dimension(:),pointer:: hfx, lh, qfx, grdflx, sfcrunoff, udrunoff
	 real(kind=RKIND),dimension(:),pointer:: sfc_albedo, sfc_emiss, z0, znt, qsfc, t2m, q2, u10, v10
	 real(kind=RKIND),dimension(:),pointer:: sfc_albedo_vis_dir, sfc_albedo_vis_dif
	 real(kind=RKIND),dimension(:),pointer:: sfc_albedo_nir_dir, sfc_albedo_nir_dif
 real(kind=RKIND),dimension(:),pointer:: ust, ustm, zol, br, fm, fh, rmol, ch, chs, chs2, cqs2
 real(kind=RKIND),dimension(:),pointer:: cd, cda, cka, flhc, flqc
	 real(kind=RKIND),dimension(:),pointer:: skintemp, canwat, smstav, smstot, snow, snowc, snowh, lai, vegfra, xice, xland
 real(kind=RKIND),dimension(:,:),pointer:: dzs, sh2o, smois, tslb
 real(kind=RKIND),dimension(:,:),pointer:: colm_soil_liquid, colm_soil_ice, colm_soil_moisture
	 real(kind=RKIND),dimension(:,:),pointer:: colm_soil_temperature, colm_soil_layer_thickness, colm_soil_layer_depth
	 real(kind=RKIND),dimension(:),pointer:: colm_river_water_depth, colm_river_velocity, colm_river_discharge

 pull_fluxes = .true.
 if(present(state_only)) pull_fluxes = .not. state_only

 call mpas_pool_get_array(diag_physics,'hfx'       ,hfx       )
 call mpas_pool_get_array(diag_physics,'lh'        ,lh        )
 call mpas_pool_get_array(diag_physics,'qfx'       ,qfx       )
 call mpas_pool_get_array(diag_physics,'grdflx'    ,grdflx    )
 call mpas_pool_get_array(diag_physics,'sfcrunoff' ,sfcrunoff )
 call mpas_pool_get_array(diag_physics,'udrunoff'  ,udrunoff  )
 call mpas_pool_get_array(diag_physics,'sfc_albedo',sfc_albedo)
	 call mpas_pool_get_array(diag_physics,'sfc_albedo_vis_dir',sfc_albedo_vis_dir)
	 call mpas_pool_get_array(diag_physics,'sfc_albedo_vis_dif',sfc_albedo_vis_dif)
	 call mpas_pool_get_array(diag_physics,'sfc_albedo_nir_dir',sfc_albedo_nir_dir)
	 call mpas_pool_get_array(diag_physics,'sfc_albedo_nir_dif',sfc_albedo_nir_dif)
 call mpas_pool_get_array(diag_physics,'sfc_emiss' ,sfc_emiss )
 call mpas_pool_get_array(diag_physics,'z0'        ,z0        )
 call mpas_pool_get_array(diag_physics,'znt'       ,znt       )
 call mpas_pool_get_array(diag_physics,'qsfc'      ,qsfc      )
 call mpas_pool_get_array(diag_physics,'t2m'       ,t2m       )
	 call mpas_pool_get_array(diag_physics,'q2'        ,q2        )
	 call mpas_pool_get_array(diag_physics,'u10'       ,u10       )
	 call mpas_pool_get_array(diag_physics,'v10'       ,v10       )
	 call mpas_pool_get_array(diag_physics,'ust'       ,ust       )
	 call mpas_pool_get_array(diag_physics,'ustm'      ,ustm      )
	 call mpas_pool_get_array(diag_physics,'zol'       ,zol       )
	 call mpas_pool_get_array(diag_physics,'br'        ,br        )
	 call mpas_pool_get_array(diag_physics,'fm'        ,fm        )
	 call mpas_pool_get_array(diag_physics,'fh'        ,fh        )
	 call mpas_pool_get_array(diag_physics,'rmol'      ,rmol      )
	 call mpas_pool_get_array(diag_physics,'ch'        ,ch        )
	 call mpas_pool_get_array(diag_physics,'chs'       ,chs       )
	 call mpas_pool_get_array(diag_physics,'chs2'      ,chs2      )
	 call mpas_pool_get_array(diag_physics,'cqs2'      ,cqs2      )
	 call mpas_pool_get_array(diag_physics,'cd'        ,cd        )
	 call mpas_pool_get_array(diag_physics,'cda'       ,cda       )
	 call mpas_pool_get_array(diag_physics,'cka'       ,cka       )
	 call mpas_pool_get_array(diag_physics,'flhc'      ,flhc      )
	 call mpas_pool_get_array(diag_physics,'flqc'      ,flqc      )
 call mpas_pool_get_array(diag_physics,'canwat'    ,canwat    )
 call mpas_pool_get_array(diag_physics,'smstav'    ,smstav    )
 call mpas_pool_get_array(diag_physics,'smstot'    ,smstot    )
 call mpas_pool_get_array(diag_physics,'lai'       ,lai       )
 call mpas_pool_get_array(sfc_input,'skintemp',skintemp)
 call mpas_pool_get_array(sfc_input,'snow'    ,snow    )
 call mpas_pool_get_array(sfc_input,'snowc'   ,snowc   )
 call mpas_pool_get_array(sfc_input,'snowh'   ,snowh   )
	 call mpas_pool_get_array(sfc_input,'vegfra'  ,vegfra  )
	 call mpas_pool_get_array(sfc_input,'xland'   ,xland   )
	 call mpas_pool_get_array(sfc_input,'xice'    ,xice    )
	 call mpas_pool_get_array(sfc_input,'dzs'     ,dzs     )
 call mpas_pool_get_array(sfc_input,'sh2o'    ,sh2o    )
 call mpas_pool_get_array(sfc_input,'smois'   ,smois   )
 call mpas_pool_get_array(sfc_input,'tslb'    ,tslb    )
	 call mpas_pool_get_array(diag_physics_colm2024,'colm_soil_liquid'         ,colm_soil_liquid         )
	 call mpas_pool_get_array(diag_physics_colm2024,'colm_soil_ice'            ,colm_soil_ice            )
	 call mpas_pool_get_array(diag_physics_colm2024,'colm_soil_moisture'       ,colm_soil_moisture       )
	 call mpas_pool_get_array(diag_physics_colm2024,'colm_soil_temperature'    ,colm_soil_temperature    )
	 call mpas_pool_get_array(diag_physics_colm2024,'colm_soil_layer_thickness',colm_soil_layer_thickness)
	 call mpas_pool_get_array(diag_physics_colm2024,'colm_soil_layer_depth'    ,colm_soil_layer_depth    )
	 call mpas_pool_get_array(diag_physics_colm2024,'colm_river_water_depth'   ,colm_river_water_depth   )
	 call mpas_pool_get_array(diag_physics_colm2024,'colm_river_velocity'      ,colm_river_velocity      )
	 call mpas_pool_get_array(diag_physics_colm2024,'colm_river_discharge'     ,colm_river_discharge     )

 nlev = size(tslb, 1)
	 if(size(sh2o,1) /= nlev .or. size(smois,1) /= nlev .or. size(dzs,1) /= nlev) then
	    call physics_error_fatal("MPAS generic soil arrays have inconsistent vertical dimensions.")
	 endif
	 if(nlev /= colm_nl_soil) then
	    call physics_error_fatal("CoLM2024 requires all MPAS soil arrays to retain its 10 native layers.")
	 endif
	 if(size(colm_soil_liquid,1) /= colm_nl_soil .or. size(colm_soil_ice,1) /= colm_nl_soil .or. &
	    size(colm_soil_moisture,1) /= colm_nl_soil .or. size(colm_soil_temperature,1) /= colm_nl_soil .or. &
	    size(colm_soil_layer_thickness,1) /= colm_nl_soil .or. size(colm_soil_layer_depth,1) /= colm_nl_soil) then
	    call physics_error_fatal("MPAS CoLM2024 registry fields must retain all 10 native soil layers.")
	 endif
	 allocate(soil_liquid(colm_nl_soil), soil_ice(colm_nl_soil), soil_moisture(colm_nl_soil), &
	          soil_temperature(colm_nl_soil), soil_water_equivalent(colm_nl_soil))

	 do i = its,ite
	    if(element_for_cell(i) <= 0) call physics_error_fatal("CoLM2024 MPAS cell-to-element map has an invalid element index.")
	    if(.not. ieee_is_finite(xland(i)) .or. .not. ieee_is_finite(xice(i))) &
	       call physics_error_fatal("MPAS supplied a non-finite land-ocean or sea-ice mask to CoLM2024.")
	    if(xland(i) < 1._RKIND .or. xland(i) > 2._RKIND .or. xice(i) < 0._RKIND .or. xice(i) > 1._RKIND) &
	       call physics_error_fatal("MPAS supplied an out-of-range land-ocean or sea-ice mask to CoLM2024.")
	    sfc_albedo_vis_dir(i) = sfc_albedo(i)
	    sfc_albedo_vis_dif(i) = sfc_albedo(i)
	    sfc_albedo_nir_dir(i) = sfc_albedo(i)
	    sfc_albedo_nir_dif(i) = sfc_albedo(i)
	    vegfra(i) = -9999._RKIND
	    smstav(i) = -9999._RKIND
	    if(xland(i) >= 1.5_RKIND .or. xice(i) >= xice_threshold) then
	       vegfra(i) = 0._RKIND
	       colm_soil_liquid(:,i) = -9999._RKIND
	       colm_soil_ice(:,i) = -9999._RKIND
	       colm_soil_moisture(:,i) = -9999._RKIND
	       colm_soil_temperature(:,i) = -9999._RKIND
	       colm_soil_layer_thickness(:,i) = -9999._RKIND
	       colm_soil_layer_depth(:,i) = -9999._RKIND
	       colm_river_water_depth(i) = -9999._RKIND
	       colm_river_velocity(i) = -9999._RKIND
	       colm_river_discharge(i) = -9999._RKIND
	       cycle
	    endif

	    if(pull_fluxes) then
	       call colm_mpas_get_element_surface(element_for_cell(i), sensible, latent, evaporation, ground_heat, runoff, &
		                                          surface_runoff, subsurface_runoff, skin_temp, t2m_colm, q2m_colm, &
	                                          u10m_colm, v10m_colm, qsfc_colm, emissivity, roughness, albedo, &
	                                          friction_velocity, mechanical_friction_velocity, &
	                                          stability_zeta, bulk_richardson, momentum_profile, heat_profile, &
	                                          moisture_profile, air_density, heat_exchange_velocity, &
	                                          moisture_exchange_velocity, heat_exchange_velocity_2m, &
	                                          moisture_exchange_velocity_2m, momentum_coefficient, enthalpy_coefficient, &
	                                          momentum_coefficient_10m, &
	                                          inverse_monin_obukhov, ierr)
	       if(ierr /= 0) call physics_error_fatal("Failed to retrieve CoLM2024 surface fluxes.")
	    else
	       call colm_mpas_get_element_boundary_state(element_for_cell(i), skin_temp, qsfc_colm, emissivity, &
	                                                   roughness, albedo, ierr)
	       if(ierr /= 0) call physics_error_fatal("Failed to retrieve the initial CoLM2024 surface boundary state.")
	    endif
	    call colm_mpas_get_element_albedo(element_for_cell(i), albedo_vis_direct, albedo_vis_diffuse, &
	                                      albedo_nir_direct, albedo_nir_diffuse, ierr)
	    if(ierr /= 0) call physics_error_fatal("Failed to retrieve the CoLM2024 spectral surface albedo.")
	    if(.not. all(ieee_is_finite((/albedo_vis_direct, albedo_vis_diffuse, &
	                                   albedo_nir_direct, albedo_nir_diffuse/))) .or. &
	       any((/albedo_vis_direct, albedo_vis_diffuse, albedo_nir_direct, albedo_nir_diffuse/) < 0._colm_r8) .or. &
	       any((/albedo_vis_direct, albedo_vis_diffuse, albedo_nir_direct, albedo_nir_diffuse/) > 1._colm_r8)) then
	       call physics_error_fatal("CoLM2024 returned an invalid spectral surface albedo.")
	    endif
	    call colm_mpas_get_element_state(element_for_cell(i), canopy_water, snow_water, snow_depth, snow_cover, &
	                                     leaf_area_index, vegetation_fraction, soil_availability, ground_temperature, &
	                                     soil_liquid, soil_ice, soil_moisture, soil_temperature, ierr)
	    if(ierr /= 0) call physics_error_fatal("Failed to retrieve CoLM2024 surface state.")
	    call colm_mpas_get_element_river_state(element_for_cell(i), river_water_depth, river_velocity, &
	                                           river_discharge, ierr)
	    if(ierr /= 0) call physics_error_fatal("Failed to retrieve CoLM2024 river-routing diagnostics.")
	    colm_river_water_depth(i) = -9999._RKIND
	    colm_river_velocity(i) = -9999._RKIND
	    colm_river_discharge(i) = -9999._RKIND
	    if(ieee_is_finite(river_water_depth) .and. abs(river_water_depth) < colm_state_bad_value) &
	       colm_river_water_depth(i) = real(river_water_depth, RKIND)
	    if(ieee_is_finite(river_velocity) .and. abs(river_velocity) < colm_state_bad_value) &
	       colm_river_velocity(i) = real(river_velocity, RKIND)
	    if(ieee_is_finite(river_discharge) .and. abs(river_discharge) < colm_state_bad_value) &
	       colm_river_discharge(i) = real(river_discharge, RKIND)
	    if(.not. all(ieee_is_finite((/canopy_water, snow_water, snow_depth, snow_cover, leaf_area_index, &
	                                   ground_temperature/))) .or. &
	       any(abs((/canopy_water, snow_water, snow_depth, snow_cover, leaf_area_index, &
	                  ground_temperature/)) >= colm_state_bad_value)) then
	       call physics_error_fatal("CoLM2024 returned a non-finite or missing required surface state.")
	    endif
	    if(pull_fluxes) then
	       if(.not. all(ieee_is_finite((/sensible, latent, evaporation, ground_heat, runoff, surface_runoff, &
		                                   subsurface_runoff, skin_temp, t2m_colm, q2m_colm, u10m_colm, v10m_colm, &
		                                   qsfc_colm, emissivity, &
	                                   roughness, albedo/))) .or. &
	          any(abs((/sensible, latent, evaporation, ground_heat, runoff, surface_runoff, subsurface_runoff, &
		                  skin_temp, t2m_colm, q2m_colm, u10m_colm, v10m_colm, qsfc_colm, emissivity, roughness, &
		                  albedo/)) >= colm_state_bad_value)) then
	          call physics_error_fatal("CoLM2024 returned a non-finite or missing required surface field.")
	       endif
	       if(.not. all(ieee_is_finite((/friction_velocity, mechanical_friction_velocity, &
	                                   stability_zeta, bulk_richardson, momentum_profile, &
	                                   heat_profile, moisture_profile, air_density, heat_exchange_velocity, &
	                                   moisture_exchange_velocity, heat_exchange_velocity_2m, &
	                                   moisture_exchange_velocity_2m, momentum_coefficient, enthalpy_coefficient, &
	                                   momentum_coefficient_10m, &
	                                   inverse_monin_obukhov/))) .or. &
	          any(abs((/friction_velocity, mechanical_friction_velocity, stability_zeta, bulk_richardson, &
	                  momentum_profile, heat_profile, &
	                  moisture_profile, air_density, heat_exchange_velocity, moisture_exchange_velocity, &
	                  heat_exchange_velocity_2m, moisture_exchange_velocity_2m, momentum_coefficient, &
	                  enthalpy_coefficient, momentum_coefficient_10m, &
	                  inverse_monin_obukhov/)) >= colm_state_bad_value)) then
	          call physics_error_fatal("CoLM2024 returned a non-finite or missing surface-exchange field.")
	       endif
	       if(q2m_colm < 0._colm_r8 .or. q2m_colm >= 1._colm_r8 .or. qsfc_colm < 0._colm_r8 .or. &
	       qsfc_colm >= 1._colm_r8 .or. emissivity < 0._colm_r8 .or. emissivity > 1._colm_r8 .or. &
	       albedo < 0._colm_r8 .or. albedo > 1._colm_r8 .or. &
	       roughness < 0._colm_r8 .or. friction_velocity < 0._colm_r8 .or. &
	       mechanical_friction_velocity < 0._colm_r8 .or. air_density <= 0._colm_r8 .or. &
	       momentum_profile <= 0._colm_r8 .or. heat_profile <= 0._colm_r8 .or. moisture_profile <= 0._colm_r8 .or. &
	       heat_exchange_velocity < 0._colm_r8 .or. moisture_exchange_velocity < 0._colm_r8 .or. &
	       heat_exchange_velocity_2m < 0._colm_r8 .or. moisture_exchange_velocity_2m < 0._colm_r8 .or. &
	       momentum_coefficient < 0._colm_r8 .or. enthalpy_coefficient < 0._colm_r8 .or. &
	       momentum_coefficient_10m < 0._colm_r8) then
	          call physics_error_fatal("CoLM2024 returned an out-of-range surface state or exchange field.")
	       endif

	    hfx(i) = real(sensible, RKIND)
	    lh(i) = real(latent, RKIND)
	    qfx(i) = real(evaporation, RKIND)
	    ! CoLM fgrnd and MPAS/Noah-MP grdflx are positive into soil/snow.
	    grdflx(i) = real(ground_heat, RKIND)
	    ! MPAS runoff diagnostics are accumulated depths, matching Noah/Noah-MP.
	    sfcrunoff(i) = sfcrunoff(i) + real(surface_runoff * dt, RKIND)
	    udrunoff(i) = udrunoff(i) + real(subsurface_runoff * dt, RKIND)
	    skintemp(i) = real(skin_temp, RKIND)
		    t2m(i) = real(t2m_colm, RKIND)
		    u10(i) = real(u10m_colm, RKIND)
		    v10(i) = real(v10m_colm, RKIND)
	    ! CoLM qref and MPAS q2/qsfc are specific humidity.
	    qsfc(i) = real(qsfc_colm, RKIND)
	    q2(i) = real(q2m_colm, RKIND)
	    sfc_emiss(i) = real(emissivity, RKIND)
	    z0(i) = real(roughness, RKIND)
	    znt(i) = real(roughness, RKIND)
	    sfc_albedo(i) = real(albedo, RKIND)
	    ust(i) = real(friction_velocity, RKIND)
	    ustm(i) = real(mechanical_friction_velocity, RKIND)
	    zol(i) = real(stability_zeta, RKIND)
	    br(i) = real(bulk_richardson, RKIND)
	    fm(i) = real(momentum_profile, RKIND)
	    fh(i) = real(heat_profile, RKIND)
	    rmol(i) = real(inverse_monin_obukhov, RKIND)
	    ch(i) = real(heat_exchange_velocity, RKIND)
	    chs(i) = real(heat_exchange_velocity, RKIND)
	    chs2(i) = real(heat_exchange_velocity_2m, RKIND)
	    cqs2(i) = real(moisture_exchange_velocity_2m, RKIND)
	    flhc(i) = real(air_density * real(cp,colm_r8) * heat_exchange_velocity, RKIND)
	    flqc(i) = real(air_density * moisture_exchange_velocity, RKIND)
	    cd(i) = real(momentum_coefficient_10m, RKIND)
	    cda(i) = real(momentum_coefficient, RKIND)
	    cka(i) = real(enthalpy_coefficient, RKIND)
	    else
	       if(.not. all(ieee_is_finite((/skin_temp, qsfc_colm, emissivity, roughness, albedo/))) .or. &
	          any(abs((/skin_temp, qsfc_colm, emissivity, roughness, albedo/)) >= colm_state_bad_value) .or. &
	          skin_temp <= 0._colm_r8 .or. qsfc_colm < 0._colm_r8 .or. qsfc_colm >= 1._colm_r8 .or. &
	          emissivity <= 0._colm_r8 .or. emissivity > 1._colm_r8 .or. roughness < 0._colm_r8 .or. &
	          albedo < 0._colm_r8 .or. albedo > 1._colm_r8) then
	          call physics_error_fatal("CoLM2024 returned an invalid initial surface boundary state.")
	       endif
	       skintemp(i) = real(skin_temp, RKIND)
	       qsfc(i) = real(qsfc_colm, RKIND)
	       sfc_emiss(i) = real(emissivity, RKIND)
	       z0(i) = real(roughness, RKIND)
	       znt(i) = real(roughness, RKIND)
	       sfc_albedo(i) = real(albedo, RKIND)
	    endif
	    sfc_albedo_vis_dir(i) = real(albedo_vis_direct, RKIND)
	    sfc_albedo_vis_dif(i) = real(albedo_vis_diffuse, RKIND)
	    sfc_albedo_nir_dir(i) = real(albedo_nir_direct, RKIND)
	    sfc_albedo_nir_dif(i) = real(albedo_nir_diffuse, RKIND)
	    canwat(i) = real(canopy_water, RKIND)
    snow(i) = real(snow_water, RKIND)
    snowh(i) = real(snow_depth, RKIND)
	    snowc(i) = real(max(0._colm_r8, min(1._colm_r8, snow_cover)), RKIND)
	    lai(i) = real(max(0._colm_r8, leaf_area_index), RKIND)
	    if(ieee_is_finite(vegetation_fraction) .and. abs(vegetation_fraction) < colm_state_bad_value) &
	       vegfra(i) = real(100._colm_r8 * max(0._colm_r8, min(1._colm_r8, vegetation_fraction)), RKIND)
	    if(ieee_is_finite(soil_availability) .and. abs(soil_availability) < colm_state_bad_value) &
	       smstav(i) = real(max(0._colm_r8, min(1._colm_r8, soil_availability)), RKIND)

	    soil_moisture_sum = 0._colm_r8
	    nvalid_soil = 0
	    smstot(i) = -9999._RKIND
	    sh2o(:,i) = -9999._RKIND
	    smois(:,i) = -9999._RKIND
	    tslb(:,i) = -9999._RKIND
	    colm_soil_liquid(:,i) = -9999._RKIND
	    colm_soil_ice(:,i) = -9999._RKIND
	    colm_soil_moisture(:,i) = -9999._RKIND
	    colm_soil_temperature(:,i) = -9999._RKIND
	    soil_water_equivalent(:) = -1._colm_r8
	    do ns = 1,colm_nl_soil
	       colm_soil_layer_thickness(ns,i) = real(colm_dz_soi(ns), RKIND)
	       colm_soil_layer_depth(ns,i) = real(colm_z_soi(ns), RKIND)
	       if(ieee_is_finite(soil_liquid(ns)) .and. ieee_is_finite(soil_ice(ns)) .and. &
	          ieee_is_finite(soil_moisture(ns)) .and. ieee_is_finite(soil_temperature(ns)) .and. &
	          soil_liquid(ns) >= 0._colm_r8 .and. soil_ice(ns) >= 0._colm_r8 .and. &
	          soil_moisture(ns) >= 0._colm_r8 .and. soil_temperature(ns) > 0._colm_r8 .and. &
	          abs(soil_liquid(ns)) < colm_state_bad_value .and. &
	          abs(soil_ice(ns)) < colm_state_bad_value .and. &
	          abs(soil_moisture(ns)) < colm_state_bad_value .and. &
	          abs(soil_temperature(ns)) < colm_state_bad_value) then
	          colm_soil_liquid(ns,i) = real(soil_liquid(ns), RKIND)
	          colm_soil_ice(ns,i) = real(soil_ice(ns), RKIND)
	          colm_soil_moisture(ns,i) = real(soil_moisture(ns), RKIND)
	          colm_soil_temperature(ns,i) = real(soil_temperature(ns), RKIND)
	          soil_water_equivalent(ns) = soil_liquid(ns) + soil_ice(ns) * colm_denice / colm_denh2o
	          soil_moisture_sum = soil_moisture_sum + soil_water_equivalent(ns) * colm_dz_soi(ns)
	          nvalid_soil = nvalid_soil + 1
	       endif
	    enddo
	    if(nvalid_soil == colm_nl_soil) then
	       smstot(i) = real(1000._colm_r8 * soil_moisture_sum, RKIND)
	       sh2o(:,i) = real(soil_liquid(:), RKIND)
	       smois(:,i) = real(soil_water_equivalent(:), RKIND)
	       tslb(:,i) = real(soil_temperature(:), RKIND)
	    else
	       colm_soil_liquid(:,i) = -9999._RKIND
	       colm_soil_ice(:,i) = -9999._RKIND
	       colm_soil_moisture(:,i) = -9999._RKIND
	       colm_soil_temperature(:,i) = -9999._RKIND
	    endif
	 enddo

	 deallocate(soil_liquid, soil_ice, soil_moisture, soil_temperature, soil_water_equivalent)

	 end subroutine colm2024_pull_mpas_surface
#endif

!=================================================================================================================
 subroutine colm2024_adapter_not_ready()
!=================================================================================================================

 call physics_error_fatal("CoLM2024 is linked, but CoLM landpatch/restart state has not been initialized " // &
                          "for this MPAS run.")

 end subroutine colm2024_adapter_not_ready

!=================================================================================================================
 subroutine colm2024_adapter_not_built()
!=================================================================================================================

 call physics_error_fatal("CoLM2024 requires building MPAS with COLM2024=true.")

 end subroutine colm2024_adapter_not_built

!=================================================================================================================
 end module mpas_atmphys_driver_lsm_colm2024
!=================================================================================================================
