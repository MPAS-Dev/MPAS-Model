! Copyright (c) 2016,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
module mpas_isobaric_diagnostics

    use mpas_dmpar
    use mpas_kind_types
    use mpas_derived_types
    use mpas_pool_routines
    use mpas_constants, only: rvord, r_earth=>a
    use mpas_log, only : mpas_log_write

    type (MPAS_pool_type), pointer :: mesh
    type (MPAS_pool_type), pointer :: state
    type (MPAS_pool_type), pointer :: diag
    type (MPAS_pool_type), pointer :: diag_physics
    type (MPAS_pool_type), pointer :: tend_physics
    type (MPAS_pool_type), pointer :: configs 

    type (MPAS_clock_type), pointer :: clock
    type (domain_type), pointer :: domain

    !
    ! Abstract interface for routine used to communicate halos of fields
    ! in a named group
    !
    abstract interface
       subroutine halo_exchange_routine(domain, halo_group, ierr)

          use mpas_derived_types, only : domain_type

          type (domain_type), intent(inout) :: domain
          character(len=*), intent(in) :: halo_group
          integer, intent(out), optional :: ierr

       end subroutine halo_exchange_routine
    end interface


    public :: isobaric_diagnostics_setup, &
              isobaric_diagnostics_compute, &
              isobaric_pv, isobaric_pv_tendencies

    private


    logical :: need_mslp, need_meanT_500_300, &
               need_temp_isobaric, need_theta_isobaric, need_dewp_isobaric, need_relhum_isobaric, need_qv_isobaric, &
               need_uzonal_isobaric, need_umerid_isobaric, &
               need_hgt_isobaric, need_geohgt_isobaric, need_w_isobaric, need_vort_isobaric, &          
               ! PV variables
               need_ertelpv, &
               need_lw, need_sw, need_bl, need_cu, need_mp, need_mix, &
               need_fric_mix, need_fric_bl, need_fric_cu, &
               need_diab, need_fric, need_dyn, &
               need_evap_rw, need_evap_cw, need_depo, need_melt, need_frez, need_mp_all, &
               need_pvtend_isobaric, need_mptend_isobaric, &
               ! Latent heating rates 
               need_dtheta_dt_cu, need_dtheta_dt_mp, need_thtend_isobaric  

    contains


    !-----------------------------------------------------------------------
    !  routine isobaric_diagnostics_setup
    !
    !> \brief Set up the isobaric diagnostics module
    !> \author Michael Duda
    !> \date   21 October 2016
    !> \details
    !>  This routine sets up the isobaric diagnostics module, principally by
    !>  saving pointers to pools that are used in the computation of diagnostics.
    !>
    !>  MC: added specification of isobaric levels to this subroutine
    !-----------------------------------------------------------------------
    subroutine isobaric_diagnostics_setup(configs_in, all_pools, simulation_clock)

        use mpas_derived_types, only : MPAS_pool_type, MPAS_clock_type
        use mpas_pool_routines, only : mpas_pool_get_subpool, mpas_pool_get_config
        use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array

        implicit none
      
        type (MPAS_pool_type), pointer :: configs_in
        type (MPAS_pool_type), pointer :: all_pools
        type (MPAS_clock_type), pointer :: simulation_clock

        logical, pointer :: config_isobaric, config_pv_isobaric

        ! Isobaric levels for interpolation
        integer, pointer :: nIsoLevels
        real (kind=RKIND), dimension(:), pointer :: iso_levels

        call mpas_pool_get_subpool(all_pools, 'mesh', mesh)
        call mpas_pool_get_subpool(all_pools, 'state', state)
        call mpas_pool_get_subpool(all_pools, 'diag', diag)

        clock => simulation_clock
        configs => configs_in

        ! check config_isobaric:
        call mpas_pool_get_config(configs, 'config_isobaric', config_isobaric)
        call mpas_pool_get_config(configs, 'config_pv_isobaric', config_pv_isobaric)

        call mpas_log_write(' ')
        call mpas_log_write('   config_isobaric      is:     $l', logicArgs=(/config_isobaric/))
        call mpas_log_write('   config_pv_isobaric   is:     $l', logicArgs=(/config_pv_isobaric/))
        call mpas_log_write(' ')

        if (config_isobaric .or. config_pv_isobaric) then 
           call mpas_log_write(' ')
           call mpas_log_write(' ----- Setting up isobaric diagnostics ----- ')
           call mpas_log_write(' ')                

           call mpas_pool_get_dimension(mesh, 'nIsoLevels', nIsoLevels)
           call mpas_pool_get_array(diag, 'iso_levels', iso_levels)

           call mpas_log_write('   Number of isobaric levels: $i', intArgs=(/nIsoLevels/))  
           iso_levels = 0.0

           ! Define isobaric levels.
           iso_levels(:) = (/10000.0, 12500.0, 15000.0, 17500.0, 20000.0, 22500.0, 25000.0, 27500.0, 30000.0, &
                             32500.0, 35000.0, 40000.0, 45000.0, 50000.0, 55000.0, 60000.0, 65000.0, 70000.0, &
                             75000.0, 77500.0, 80000.0, 82500.0, 85000.0, 87500.0, 90000.0, 92500.0, 95000.0, 100000.0/)

        end if    

    end subroutine isobaric_diagnostics_setup


    !-----------------------------------------------------------------------
    !  routine isobaric_diagnostics_compute
    !
    !> \brief Compute isobaric diagnostic before model output is written
    !> \author Michael Duda
    !> \date   21 October 2016
    !> \details
    !>  Compute isobaric diagnostic before model output is written. Code called
    !>  from here was previously in mpas_atm_interp_diagnostics.F.
    !
    !-----------------------------------------------------------------------
    subroutine isobaric_diagnostics_compute(domain, exchange_halo_group)

        use mpas_atm_diagnostics_utils, only : MPAS_field_will_be_written
        use mpas_pool_routines, only: mpas_pool_get_config 

        implicit none

        type (domain_type), intent(inout) :: domain                     ! MC: halo exchange 
        procedure (halo_exchange_routine) :: exchange_halo_group        ! MC: halo exchange         

        logical :: need_any_diags
        logical, pointer :: config_isobaric, config_pv_diag

        need_any_diags = .false.

        need_mslp = .false.
        need_meanT_500_300 = .false. 

        need_temp_isobaric = .false.
        need_theta_isobaric = .false.
        need_dewp_isobaric = .false.
        need_relhum_isobaric = .false.
        need_qv_isobaric = .false.
        need_uzonal_isobaric = .false.
        need_umerid_isobaric = .false.
        need_hgt_isobaric = .false.
        need_geohgt_isobaric = .false.
        need_w_isobaric = .false.
        need_vort_isobaric = .false.

        call mpas_pool_get_config(configs, 'config_isobaric', config_isobaric)
        call mpas_pool_get_config(configs, 'config_pv_diag', config_pv_diag)

        if (config_isobaric) then
           need_mslp            = MPAS_field_will_be_written('mslp')
           need_meanT_500_300   = MPAS_field_will_be_written('meanT_500_300')

           need_temp_isobaric   = MPAS_field_will_be_written('temperature_isobaric')
           need_temp_isobaric   = need_temp_isobaric .or. need_meanT_500_300

           need_theta_isobaric  = MPAS_field_will_be_written('theta_isobaric')
           need_dewp_isobaric   = MPAS_field_will_be_written('dewpoint_isobaric')
           need_relhum_isobaric = MPAS_field_will_be_written('relhum_isobaric')
           need_qv_isobaric     = MPAS_field_will_be_written('qvapor_isobaric')
           need_uzonal_isobaric = MPAS_field_will_be_written('uzonal_isobaric')
           need_umerid_isobaric = MPAS_field_will_be_written('umeridional_isobaric')
           need_hgt_isobaric    = MPAS_field_will_be_written('height_isobaric')
           need_geohgt_isobaric = MPAS_field_will_be_written('geoheight_isobaric')
           need_w_isobaric      = MPAS_field_will_be_written('w_isobaric')
           need_vort_isobaric   = MPAS_field_will_be_written('vorticity_isobaric')

           need_any_diags = need_any_diags .or. need_mslp .or. need_meanT_500_300 .or. &
                            need_temp_isobaric .or. need_theta_isobaric .or. need_dewp_isobaric .or. &
                            need_relhum_isobaric .or. need_qv_isobaric .or. need_uzonal_isobaric .or. &
                            need_umerid_isobaric .or. need_hgt_isobaric .or. need_geohgt_isobaric .or. &
                            need_w_isobaric .or. need_vort_isobaric

           if (need_any_diags) then
              call mpas_log_write('Calling isobaric interpolation subroutine.')      
              call interp_diagnostics(domain, mesh, state, 1, diag, exchange_halo_group)
           end if
        end if 

    end subroutine isobaric_diagnostics_compute


   !==================================================================================================
    subroutine isobaric_pv(domain, exchange_halo_group)
   !
   ! > MC: subroutine is called by mpas_pv_diagnostics.F to interpolate PV to isobaric levels. 
   !       unlike for PV tendencies, only need to do this before writing file. this will be
   !       determined in mpas_pv_diagnostics.F. 
   !==================================================================================================

        use mpas_atm_diagnostics_utils, only : MPAS_field_will_be_written

        implicit none

        type (domain_type), intent(inout) :: domain                     ! MC: halo exchange 
        procedure (halo_exchange_routine) :: exchange_halo_group        ! MC: halo exchange  

        need_ertelpv = .false.
        need_ertelpv =  MPAS_field_will_be_written('ertel_pv_isobaric')
        
        if (need_ertelpv) then 
           call mpas_log_write('Calling isobaric interpolation subroutine for ertel_pv.')
           call interp_diagnostics_pv(domain, mesh, state, 1, diag, exchange_halo_group)
        end if 

    end subroutine isobaric_pv

   !==================================================================================================
    subroutine isobaric_pv_tendencies(domain, exchange_halo_group)
   !
   ! > MC: subroutine is called by mpas_pv_diagnostics.F to interpolate PV tendencies to isobaric levels. 
   !       If config_pv_tend is activated, need to do this at each time step. this is determined in 
   !       mpas_pv_diagnostics.F. 
   !==================================================================================================

        implicit none

        type (domain_type), intent(inout) :: domain                     ! MC: halo exchange 
        procedure (halo_exchange_routine) :: exchange_halo_group        ! MC: halo exchange         

        call mpas_log_write('Calling interp_diagnostics_pv_tend.')
        call interp_diagnostics_pv_tend(domain, mesh, state, 1, diag, exchange_halo_group)

     end subroutine isobaric_pv_tendencies

   !==================================================================================================
    subroutine interp_diagnostics(domain, mesh, state, time_lev, diag, exchange_halo_group)
    !
    !> MC: Interpolates conventional model fields (e.g., potential temperature) to array of prescribed 
    !      isobaric levels
   !==================================================================================================

        implicit none

        ! Input arguments:
        type (mpas_pool_type), intent(in)  :: mesh
        type (domain_type), intent(inout) :: domain                     ! MC: halo exchange
        type (mpas_pool_type), intent(in) :: state
        integer, intent(in) :: time_lev                                 ! which time level to use from state
        type (mpas_pool_type), intent(inout) :: diag
        procedure (halo_exchange_routine) :: exchange_halo_group        ! MC: halo exchange

        ! Local variables
        integer :: iCell, k, kk
        
        ! Mesh variables and dimensions 
        integer, pointer :: index_qv, num_scalars
        integer, pointer :: nCells, nVertLevels
        integer, dimension(:), pointer :: nEdgesOnCell
        integer, dimension(:,:), pointer :: verticesOnCell, cellsOnVertex
        real (kind=RKIND), dimension(:), pointer :: areaCell
        real (kind=RKIND), dimension(:,:), pointer :: kiteAreasOnVertex
        
        ! Isobaric levels for interpolation
        integer, pointer :: nIsoLevels

        ! Isolevels for all fields
        real (kind=RKIND), dimension(:), pointer :: iso_levels         

        ! Pressure variables
        real (kind=RKIND), dimension(:,:), pointer :: pressure_b, pressure_p
        real (kind=RKIND), dimension(:,:), allocatable :: pressure

        ! Fields to be interpolated (or from which fields are derived)
        real (kind=RKIND) :: evp
        real (kind=RKIND), dimension(:,:), pointer :: exner, height, theta, relhum, vvel
        real (kind=RKIND), dimension(:,:), pointer :: qv, uzonal, umeridional, vorticity  
        real (kind=RKIND), dimension(:,:,:), pointer :: scalars
            
        real (kind=RKIND), dimension(:,:), allocatable :: temperature, dewpoint, vorticity_cell 

        ! Isobaric interpolated fields
        real (kind=RKIND), dimension(:,:), pointer :: temperature_isobaric, theta_isobaric, &
                                                      dewpoint_isobaric, relhum_isobaric, &
                                                      qvapor_isobaric, height_isobaric, &
                                                      geoheight_isobaric, w_isobaric, &
                                                      uzonal_isobaric, umeridional_isobaric, &
                                                      vorticity_isobaric

        ! Additional fields
        real (kind=RKIND), dimension(:), pointer :: mslp, meanT_500_300
        
        ! For mean-layer calculations
        real (kind=RKIND), dimension(:,:), allocatable :: press_in, field_in

        ! Mesh variables
        call mpas_pool_get_dimension(mesh, 'nCells', nCells)
        call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
        call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
        call mpas_pool_get_array(mesh, 'verticesOnCell', verticesOnCell)
        call mpas_pool_get_array(mesh, 'cellsOnVertex', cellsOnVertex)
        call mpas_pool_get_array(mesh, 'areaCell', areaCell)
        call mpas_pool_get_array(mesh, 'kiteAreasOnVertex', kiteAreasOnVertex)

        ! Isobaric levels -- need to amend if additonal level dims are used
        call mpas_pool_get_dimension(mesh, 'nIsoLevels', nIsoLevels)
        call mpas_pool_get_array(diag, 'iso_levels', iso_levels)

        ! Pressure variables 
        call exchange_halo_group(domain, 'isobaric:pressure_p')
        call mpas_pool_get_array(diag, 'pressure_base', pressure_b)
        call mpas_pool_get_array(diag, 'pressure_p', pressure_p)

        ! Fields to be interpolated (or from which fields are derived):
        call mpas_pool_get_dimension(state, 'num_scalars', num_scalars)
        call mpas_pool_get_dimension(state, 'index_qv', index_qv)
        call mpas_pool_get_array(mesh, 'zgrid', height)
        call mpas_pool_get_array(diag, 'theta', theta, time_lev)
        call mpas_pool_get_array(diag, 'exner', exner)
        call mpas_pool_get_array(state, 'scalars', scalars, 1)
        call mpas_pool_get_array(diag, 'relhum', relhum)
        call mpas_pool_get_array(diag, 'uReconstructZonal', uzonal)
        call mpas_pool_get_array(diag, 'uReconstructMeridional', umeridional)
        call mpas_pool_get_array(state, 'w', vvel, time_lev)

        ! Fields to interpolate:   
        call mpas_pool_get_array(diag, 'temperature_isobaric', temperature_isobaric)
        call mpas_pool_get_array(diag, 'theta_isobaric', theta_isobaric)
        call mpas_pool_get_array(diag, 'dewpoint_isobaric', dewpoint_isobaric)
        call mpas_pool_get_array(diag, 'relhum_isobaric', relhum_isobaric)
        call mpas_pool_get_array(diag, 'qvapor_isobaric', qvapor_isobaric)
        call mpas_pool_get_array(diag, 'uzonal_isobaric', uzonal_isobaric)
        call mpas_pool_get_array(diag, 'umeridional_isobaric', umeridional_isobaric)
        call mpas_pool_get_array(diag, 'height_isobaric', height_isobaric)
        call mpas_pool_get_array(diag, 'geoheight_isobaric', geoheight_isobaric)
        call mpas_pool_get_array(diag, 'w_isobaric', w_isobaric)
        call mpas_pool_get_array(diag, 'vorticity_isobaric', vorticity_isobaric)

        call exchange_halo_group(domain, 'isobaric:vorticity')     
        call mpas_pool_get_array(diag, 'vorticity', vorticity)

        ! Additional fields
        call mpas_pool_get_array(diag, 'mslp', mslp)
        call mpas_pool_get_array(diag, 'meanT_500_300', meanT_500_300)
        
        ! Initialize qv 
        qv => scalars(index_qv,:,:)

        if(.not.allocated(pressure)) allocate(pressure(nVertLevels,nCells+1))
        if(.not.allocated(temperature)) allocate(temperature(nVertLevels,nCells))
        if(.not.allocated(dewpoint)) allocate(dewpoint(nVertLevels,nCells))

        temperature(:,:) = 0.0
        dewpoint(:,:) = 0.0

        ! -----------------------------------------------------------------
        ! Calculate total pressure at mass points:
        do iCell = 1,nCells
            do k = 1,nVertLevels
               pressure(k,iCell) = (pressure_p(k,iCell) + pressure_b(k,iCell)) / 100._RKIND
            end do
        end do

        ! -----------------------------------------------------------------
        ! Calculate temperature and dewpoint:
        if (need_temp_isobaric .or. need_dewp_isobaric .or. need_mslp .or. need_meanT_500_300) then        
            call calc_temperature_dewpoint(nCells, nVertLevels, qv, exner, theta, pressure, temperature, dewpoint)
        end if

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!!!!!!!!! Interpolate fields to array of pressure levels !!!!!!!!!!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        !--------------------------------------------------------------------
        ! Interpolate temperature:
        if (need_temp_isobaric) then
           call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, temperature, nIsoLevels, iso_levels, temperature_isobaric) 
        end if
        
        !--------------------------------------------------------------------
        ! Interpolate theta:
        if (need_theta_isobaric) then
           call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, theta, nIsoLevels, iso_levels, theta_isobaric) 
        end if

        !--------------------------------------------------------------------
        ! Interpolate dewpoint:
        if (need_dewp_isobaric) then
           call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, dewpoint, nIsoLevels, iso_levels, dewpoint_isobaric) 
        end if

        !--------------------------------------------------------------------
        ! Interpolate relative humidity:
        if (need_relhum_isobaric) then
           call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, relhum, nIsoLevels, iso_levels, relhum_isobaric) 
        end if

        !--------------------------------------------------------------------
        ! Interpolate qv (water vapor mixing ratio):
        if (need_qv_isobaric) then
           call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, qv, nIsoLevels, iso_levels, qvapor_isobaric) 
        end if

        !--------------------------------------------------------------------
        ! Interpolate geometric height and convert to geopotential height:
        if (need_hgt_isobaric .or. need_geohgt_isobaric) then
           call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, height, nIsoLevels, iso_levels, height_isobaric) 

           if (need_geohgt_isobaric) then
              geoheight_isobaric(:,:) = (r_earth * height_isobaric(:,:)) / (r_earth + height_isobaric(:,:))
           end if
        end if 

        !--------------------------------------------------------------------
        ! Interpolate uReconstructZonal:
        if (need_uzonal_isobaric) then
           call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, uzonal, nIsoLevels, iso_levels, uzonal_isobaric) 
        end if

        !--------------------------------------------------------------------
        ! Interpolate uReconstructMeridional:
        if (need_umerid_isobaric) then
           call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, umeridional, nIsoLevels, iso_levels, umeridional_isobaric) 
        end if    

        !--------------------------------------------------------------------
        ! Interpolate vertical vorticity:   
        if (need_vort_isobaric) then
           if(.not.allocated(vorticity_cell)) allocate(vorticity_cell(nVertLevels,nCells))
           vorticity_cell(:,:) = 0.0

           ! first, reconstruct vorticity to cell center (decreases number of points by roughly half) 
           call interp_absVertVort(vorticity, nCells, nEdgesOnCell, verticesOnCell, &
                                   cellsOnVertex, areaCell, kiteAreasOnVertex, vorticity_cell)

           call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, vorticity_cell, nIsoLevels, iso_levels, vorticity_isobaric)  
           if (allocated(vorticity_cell)) deallocate(vorticity_cell)
        end if

        !--------------------------------------------------------------------
        ! Interpolate vertical velocity:
        if (need_w_isobaric) then
           call interp_field_cell_w_levels(nCells, nVertLevels, pressure, height, vvel, nIsoLevels, iso_levels, w_isobaric) 
        end if

        !--------------------------------------------------------------------
        ! Calculate layer-mean quantities 
        
        if (need_meanT_500_300) then
           if(.not.allocated(field_in)) allocate(field_in(nCells,nVertLevels))
           if(.not.allocated(press_in)) allocate(press_in(nCells,nVertLevels))
           
           !reverse the vertical axis of pressure and quantity being averaged
           do iCell=1,nCells
              do k=1,nVertLevels
                 kk = nVertLevels+1-k
                 press_in(iCell,kk) = pressure(k,iCell) * 100.
                 field_in(iCell,kk) = temperature(k,iCell)
              end do
           end do

           call compute_layer_mean(meanT_500_300, 50000.0_RKIND, 30000.0_RKIND, field_in, press_in)

           if(allocated(field_in)) deallocate(field_in)
           if(allocated(press_in)) deallocate(press_in)
        end if        

        !--------------------------------------------------------------------
        ! Calculate SLP field:
        if (need_mslp) then
          !... compute SLP (requires temp, height, pressure, qvapor)
          call compute_slp(nCells, nVertLevels, num_scalars, temperature, height, pressure, index_qv, scalars, mslp)
          mslp(:) = mslp(:) * 100.0   ! Convert from hPa to Pa
          !... alternative way
          !do iCell = 1, nCells
            !   mslp(iCell) = diag % surface_pressure % array(iCell) + 11.38*height(1,iCell)
            !   mslp(iCell) = mslp(iCell)/100.
           !enddo
        end if

        
        if (allocated(pressure)) deallocate(pressure)
        if (allocated(temperature)) deallocate(temperature)
        if (allocated(dewpoint)) deallocate(dewpoint)
        
    end subroutine interp_diagnostics
    
   !==================================================================================================
    subroutine interp_diagnostics_pv(domain, mesh, state, time_lev, diag, exchange_halo_group)
    !> MC: Interpolates ertel_pv to array of prescribed isobaric levels
   !==================================================================================================

        implicit none

        ! Input arguments:
        type (mpas_pool_type), intent(in)  :: mesh
        type (domain_type), intent(inout) :: domain                     ! MC: halo exchange
        type (mpas_pool_type), intent(in) :: state
        integer, intent(in) :: time_lev              ! which time level to use from state
        type (mpas_pool_type), intent(inout) :: diag
        procedure (halo_exchange_routine) :: exchange_halo_group        ! MC: halo exchange

        ! Local variables
        integer :: iCell, k, kk
        
        ! Mesh variables and dimensions 
        integer, pointer :: nCells, nVertLevels
        
        ! Isobaric levels for interpolation
        integer, pointer :: nIsoLevels

        ! Isolevels for all fields
        real (kind=RKIND), dimension(:), pointer :: iso_levels         

        ! Pressure variables
        real (kind=RKIND), dimension(:,:), pointer :: pressure_b, pressure_p
        real (kind=RKIND), dimension(:,:), allocatable :: pressure

        ! Fields to be interpolated (or from which fields are derived)
        real (kind=RKIND), dimension(:,:), pointer :: ertel_pv

        ! Fields to interpolate:   
        real (kind=RKIND), dimension(:,:), pointer :: ertel_pv_isobaric

        ! Mesh variables
        call mpas_pool_get_dimension(mesh, 'nCells', nCells)
        call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)

        ! Isobaric levels -- need to amend if additonal level dims are used
        call mpas_pool_get_dimension(mesh, 'nIsoLevels', nIsoLevels)
        call mpas_pool_get_array(diag, 'iso_levels', iso_levels)

        ! Pressure variables 
        call exchange_halo_group(domain, 'isobaric:pressure_p')
        call mpas_pool_get_array(diag, 'pressure_base', pressure_b)
        call mpas_pool_get_array(diag, 'pressure_p', pressure_p)

        ! Fields to be interpolated
        call mpas_pool_get_array(diag, 'ertel_pv', ertel_pv)

        ! Fields to interpolate:  
        call mpas_pool_get_array(diag, 'ertel_pv_isobaric', ertel_pv_isobaric)

        if(.not.allocated(pressure)) allocate(pressure(nVertLevels,nCells+1))

        ! -----------------------------------------------------------------
        ! Calculate total pressure at mass points:
        do iCell = 1,nCells
            do k = 1,nVertLevels
               pressure(k,iCell) = (pressure_p(k,iCell) + pressure_b(k,iCell)) / 100._RKIND
            end do
        end do

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!!!!!!!!! Interpolate fields to array of pressure levels !!!!!!!!!!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        !--------------------------------------------------------------------
        ! Interpolate PV:

        call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, ertel_pv, nIsoLevels, iso_levels, ertel_pv_isobaric) 

        if (allocated(pressure)) deallocate(pressure)

    end subroutine interp_diagnostics_pv

    
   !==================================================================================================
    subroutine interp_diagnostics_pv_tend(domain, mesh, state, time_lev, diag, exchange_halo_group)
   !==================================================================================================

        use mpas_pool_routines, only: mpas_pool_get_config
        
        implicit none

        ! Input arguments:
        type (mpas_pool_type), intent(in)  :: mesh
        type (domain_type), intent(inout) :: domain                     ! MC: halo exchange
        type (mpas_pool_type), intent(in) :: state
        integer, intent(in) :: time_lev              ! which time level to use from state
        type (mpas_pool_type), intent(inout) :: diag
        procedure (halo_exchange_routine) :: exchange_halo_group        ! MC: halo exchange

        logical, pointer :: config_pv_microphys

        ! Local variables
        integer :: iCell, k, kk
        
        ! Mesh variables and dimensions 
        integer, pointer :: nCells, nVertLevels
        
        ! Isobaric levels for interpolation
        integer, pointer :: nIsoLevels

        ! Isolevels for all fields
        real (kind=RKIND), dimension(:), pointer :: iso_levels         

        ! Pressure variables
        real (kind=RKIND), dimension(:,:), pointer :: pressure_b, pressure_p
        real (kind=RKIND), dimension(:,:), allocatable :: pressure

        ! Fields to be interpolated (or from which fields are derived)
        real (kind=RKIND), dimension(:,:), pointer :: depv_dt_lw, depv_dt_sw, depv_dt_bl, &
                                                      depv_dt_cu, depv_dt_mp, &
                                                      depv_dt_mix,  &
                                                      depv_dt_fric_bl, depv_dt_fric_cu, depv_dt_fric_mix, &
                                                      depv_dt_diab, depv_dt_fric, depv_dt_dyn, &
                                                      dtheta_dt_cu, dtheta_dt_mp, &
                                                      depv_dt_mp_evap_rw, depv_dt_mp_evap_cw, &
                                                      depv_dt_mp_depo_ice, depv_dt_mp_melt_ice, &
                                                      depv_dt_mp_frez_ice, depv_dt_mp_allproc

        ! Fields to interpolate:   
        real (kind=RKIND), dimension(:,:), pointer :: depv_dt_lw_isobaric, depv_dt_sw_isobaric, &
                                                      depv_dt_bl_isobaric, depv_dt_cu_isobaric, &
                                                      depv_dt_mp_isobaric, depv_dt_mix_isobaric, &
                                                      depv_dt_fric_bl_isobaric, depv_dt_fric_cu_isobaric, &
                                                      depv_dt_fric_mix_isobaric, &
                                                      depv_dt_diab_isobaric, depv_dt_fric_isobaric, &
                                                      depv_dt_dyn_isobaric, &
                                                      dtheta_dt_cu_isobaric, dtheta_dt_mp_isobaric, &
                                                      depv_dt_mp_evap_rw_isobaric, depv_dt_mp_evap_cw_isobaric, &
                                                      depv_dt_mp_depo_ice_isobaric, depv_dt_mp_melt_ice_isobaric, &
                                                      depv_dt_mp_frez_ice_isobaric, depv_dt_mp_allproc_isobaric

        ! Mesh variables
        call mpas_pool_get_dimension(mesh, 'nCells', nCells)
        call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)

        ! Isobaric levels -- need to amend if additonal level dims are used
        call mpas_pool_get_dimension(mesh, 'nIsoLevels', nIsoLevels)
        call mpas_pool_get_array(diag, 'iso_levels', iso_levels)

        ! Pressure variables 
        call exchange_halo_group(domain, 'isobaric:pressure_p')
        call mpas_pool_get_array(diag, 'pressure_base', pressure_b)
        call mpas_pool_get_array(diag, 'pressure_p', pressure_p)

        ! Tendency variables to be interpolated
        call mpas_pool_get_array(diag, 'depv_dt_lw', depv_dt_lw)
        call mpas_pool_get_array(diag, 'depv_dt_sw', depv_dt_sw)
        call mpas_pool_get_array(diag, 'depv_dt_bl', depv_dt_bl)
        call mpas_pool_get_array(diag, 'depv_dt_cu', depv_dt_cu)
        call mpas_pool_get_array(diag, 'depv_dt_mp', depv_dt_mp)
        call mpas_pool_get_array(diag, 'depv_dt_mix', depv_dt_mix)
        call mpas_pool_get_array(diag, 'depv_dt_fric_bl', depv_dt_fric_bl)
        call mpas_pool_get_array(diag, 'depv_dt_fric_cu', depv_dt_fric_cu)
        call mpas_pool_get_array(diag, 'depv_dt_fric_mix', depv_dt_fric_mix)
        call mpas_pool_get_array(diag, 'depv_dt_diab', depv_dt_diab)
        call mpas_pool_get_array(diag, 'depv_dt_fric', depv_dt_fric)
        call mpas_pool_get_array(diag, 'depv_dt_dyn', depv_dt_dyn)
        call mpas_pool_get_array(diag, 'dtheta_dt_cu', dtheta_dt_cu)            
        call mpas_pool_get_array(diag, 'dtheta_dt_mp', dtheta_dt_mp) 
        
        ! Tendency variables to interpolate
        call mpas_pool_get_array(diag, 'depv_dt_lw_isobaric', depv_dt_lw_isobaric)
        call mpas_pool_get_array(diag, 'depv_dt_sw_isobaric', depv_dt_sw_isobaric)
        call mpas_pool_get_array(diag, 'depv_dt_bl_isobaric', depv_dt_bl_isobaric)
        call mpas_pool_get_array(diag, 'depv_dt_cu_isobaric', depv_dt_cu_isobaric)
        call mpas_pool_get_array(diag, 'depv_dt_mp_isobaric', depv_dt_mp_isobaric)
        call mpas_pool_get_array(diag, 'depv_dt_mix_isobaric', depv_dt_mix_isobaric)
        call mpas_pool_get_array(diag, 'depv_dt_fric_bl_isobaric', depv_dt_fric_bl_isobaric)
        call mpas_pool_get_array(diag, 'depv_dt_fric_cu_isobaric', depv_dt_fric_cu_isobaric)
        call mpas_pool_get_array(diag, 'depv_dt_fric_mix_isobaric', depv_dt_fric_mix_isobaric)
        call mpas_pool_get_array(diag, 'depv_dt_diab_isobaric', depv_dt_diab_isobaric)
        call mpas_pool_get_array(diag, 'depv_dt_fric_isobaric', depv_dt_fric_isobaric)
        call mpas_pool_get_array(diag, 'depv_dt_dyn_isobaric', depv_dt_dyn_isobaric)
        call mpas_pool_get_array(diag, 'dtheta_dt_cu_isobaric', dtheta_dt_cu_isobaric)
        call mpas_pool_get_array(diag, 'dtheta_dt_mp_isobaric', dtheta_dt_mp_isobaric)

        ! Thompson microphysics process tendencies:
        call mpas_pool_get_config(configs, 'config_pv_microphys', config_pv_microphys)
        
        if (config_pv_microphys) then 
           call mpas_pool_get_array(diag, 'depv_dt_mp_evap_rw', depv_dt_mp_evap_rw)
           call mpas_pool_get_array(diag, 'depv_dt_mp_evap_cw', depv_dt_mp_evap_cw)
           call mpas_pool_get_array(diag, 'depv_dt_mp_depo_ice', depv_dt_mp_depo_ice)
           call mpas_pool_get_array(diag, 'depv_dt_mp_melt_ice', depv_dt_mp_melt_ice)
           call mpas_pool_get_array(diag, 'depv_dt_mp_frez_ice', depv_dt_mp_frez_ice)
           call mpas_pool_get_array(diag, 'depv_dt_mp_allproc', depv_dt_mp_allproc)

           call mpas_pool_get_array(diag, 'depv_dt_mp_evap_rw_isobaric', depv_dt_mp_evap_rw_isobaric)
           call mpas_pool_get_array(diag, 'depv_dt_mp_evap_cw_isobaric', depv_dt_mp_evap_cw_isobaric)
           call mpas_pool_get_array(diag, 'depv_dt_mp_depo_ice_isobaric', depv_dt_mp_depo_ice_isobaric)
           call mpas_pool_get_array(diag, 'depv_dt_mp_melt_ice_isobaric', depv_dt_mp_melt_ice_isobaric)
           call mpas_pool_get_array(diag, 'depv_dt_mp_frez_ice_isobaric', depv_dt_mp_frez_ice_isobaric)
           call mpas_pool_get_array(diag, 'depv_dt_mp_allproc_isobaric', depv_dt_mp_allproc_isobaric)
        end if 
   
        if(.not.allocated(pressure)) allocate(pressure(nVertLevels,nCells+1))

        ! -----------------------------------------------------------------
        ! Calculate total pressure at mass points:
        do iCell = 1,nCells
            do k = 1,nVertLevels
               pressure(k,iCell) = (pressure_p(k,iCell) + pressure_b(k,iCell)) / 100._RKIND
            end do
        end do


        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!!!!!!!!! Interpolate fields to array of pressure levels !!!!!!!!!!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        !--------------------------------------------------------------------
        ! Interpolate PV tendencies:

        ! Longwave radiation:
        call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, depv_dt_lw, nIsoLevels, iso_levels, depv_dt_lw_isobaric) 
    
        ! Shortwave radiation:
        call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, depv_dt_sw, nIsoLevels, iso_levels, depv_dt_sw_isobaric) 

        ! PBL:
        call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, depv_dt_bl, nIsoLevels, iso_levels, depv_dt_bl_isobaric) 

        ! Cumulus:
        call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, depv_dt_cu, nIsoLevels, iso_levels, depv_dt_cu_isobaric) 

        ! Microphysics:
        call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, depv_dt_mp, nIsoLevels, iso_levels, depv_dt_mp_isobaric) 

        ! Mixing:
        call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, depv_dt_mix, nIsoLevels, iso_levels, depv_dt_mix_isobaric) 

        ! Friction - PBL:
        call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, depv_dt_fric_bl, nIsoLevels, iso_levels, depv_dt_fric_bl_isobaric) 

        ! Friction - Cumulus:
        call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, depv_dt_fric_cu, nIsoLevels, iso_levels, depv_dt_fric_cu_isobaric) 

        ! Friction - Mixing:
        call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, depv_dt_fric_mix, nIsoLevels, iso_levels, depv_dt_fric_mix_isobaric)
                                                      
        ! Diabatic:
        call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, depv_dt_diab, nIsoLevels, iso_levels, depv_dt_diab_isobaric) 

        ! Friction:
        call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, depv_dt_fric, nIsoLevels, iso_levels, depv_dt_fric_isobaric) 

        ! Dynamics:
        call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, depv_dt_dyn, nIsoLevels, iso_levels, depv_dt_dyn_isobaric) 

        !--------------------------------------------------------------------
        ! Interpolate potential temperature tendencies from latent heating:

        if (associated(dtheta_dt_cu)) then
           call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, dtheta_dt_cu, nIsoLevels, iso_levels, dtheta_dt_cu_isobaric) 
        end if

        if (associated(dtheta_dt_mp)) then
           call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, dtheta_dt_mp, nIsoLevels, iso_levels, dtheta_dt_mp_isobaric) 
        end if        
        
        !--------------------------------------------------------------------
        ! Interpolate PV tendencies from specific microphysical processes:
        if (config_pv_microphys) then

           call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, depv_dt_mp_evap_rw, nIsoLevels, iso_levels, depv_dt_mp_evap_rw_isobaric) 
           call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, depv_dt_mp_evap_cw, nIsoLevels, iso_levels, depv_dt_mp_evap_cw_isobaric) 
           call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, depv_dt_mp_depo_ice, nIsoLevels, iso_levels, depv_dt_mp_depo_ice_isobaric) 
           call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, depv_dt_mp_melt_ice, nIsoLevels, iso_levels, depv_dt_mp_melt_ice_isobaric) 
           call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, depv_dt_mp_frez_ice, nIsoLevels, iso_levels, depv_dt_mp_frez_ice_isobaric) 
           call interp_field_cell_mass_levels(nCells, nVertLevels, pressure, depv_dt_mp_allproc, nIsoLevels, iso_levels, depv_dt_mp_allproc_isobaric) 
        end if 

        if (allocated(pressure)) deallocate(pressure)
        
    end subroutine interp_diagnostics_pv_tend
    

   !==================================================================================================
    subroutine interp_tofixed_pressure(ncol,nlev_in,nlev_out,pres_in,field_in,pres_out,field_out)
   !==================================================================================================
   
   !input arguments:
    integer,intent(in):: ncol,nlev_in,nlev_out
   
    real(kind=RKIND),intent(in),dimension(ncol,nlev_in) :: pres_in,field_in
    real(kind=RKIND),intent(in),dimension(ncol,nlev_out):: pres_out
   
   !output arguments:
    real(kind=RKIND),intent(out),dimension(ncol,nlev_out):: field_out
   
   !local variables:
   ! integer:: i1,i2,icol,k,kk
    integer:: icol,k,kk
    integer:: kkstart,kount
    integer,dimension(ncol):: kupper
   
    real(kind=RKIND):: dpl,dpu
   
   !--------------------------------------------------------------------------------------------------
   
   !call mpas_log_write('')
   !call mpas_log_write('--- enter subroutine interp_tofixed_pressure:')
   !call mpas_log_write('... ncol     = $i',intArgs=(/ncol/))
   !call mpas_log_write('... nlev_in  = $i',intArgs=(/nlev_in/))
   !call mpas_log_write('... nlev_out = $i',intArgs=(/nlev_out/))
   !i1=1 ; i2=ncol
   !do k = 1, nlev_in
   !   call mpas_log_write('$i $r $r $r $r', intArgs=(/k/), realArgs=(/pres_in(i1,k),field_in(i1,k),pres_in(i2,k),field_in(i2,k)/))
   !enddo
   !call mpas_log_write('')
   
    do icol = 1, ncol
       kupper(icol) = 1
    enddo
   
    do k = 1, nlev_out 
   
       kkstart = nlev_in
       do icol = 1, ncol
          kkstart = min0(kkstart,kupper(icol))
       enddo
       kount = 0
   
       do kk = kkstart, nlev_in-1
          do icol = 1, ncol
             if(pres_out(icol,k).gt.pres_in(icol,kk).and.pres_out(icol,k).le.pres_in(icol,kk+1)) then
                kupper(icol) = kk
                kount = kount + 1
   !            call mpas_log_write('$i $i $r $r $r', intArgs=(/k,kupper(icol)/), realArgs=(/pres_out(icol,k),pres_in(icol,kk),pres_in(icol,kk+1)/))
             endif
          enddo
   
          if(kount.eq.ncol) then
             do icol = 1, ncol
                dpu = pres_out(icol,k) - pres_in(icol,kupper(icol))
                dpl = pres_in(icol,kupper(icol)+1) - pres_out(icol,k)
                field_out(icol,k) = (field_in(icol,kupper(icol))*dpl &
                                  + field_in(icol,kupper(icol)+1)*dpu)/(dpl + dpu)
             end do
             goto 35
           end if
       enddo
   
       do icol = 1, ncol
          if(pres_out(icol,k) .lt. pres_in(icol,1)) then
             field_out(icol,k) = field_in(icol,1)*pres_out(icol,k)/pres_in(icol,1)
          elseif(pres_out(icol,k) .gt. pres_in(icol,nlev_in)) then
             field_out(icol,k) = field_in(icol,nlev_in)
          else
             dpu = pres_out(icol,k) - pres_in(icol,kupper(icol))
             dpl = pres_in(icol,kupper(icol)+1) - pres_out(icol,k)
             field_out(icol,k) = (field_in(icol,kupper(icol))*dpl &
                               + field_in(icol,kupper(icol)+1)*dpu)/(dpl + dpu)
          endif
       enddo
   
    35 continue
   !   call mpas_log_write('$i $r $r $r $r $r $r', intArgs=(/kupper(i1)/), &
   !                       realArgs=(/pres_out(i1,k),pres_in(i1,kupper(i1)),pres_in(i1,kupper(i1)+1),field_out(i1,k),field_in(i1,kupper(i1)),field_in(i1,kupper(i1)+1)/))
   !   call mpas_log_write('$i $r $r $r $r $r $r', intArgs=(/kupper(i2)/), &
   !                       realArgs=(/pres_out(i2,k),pres_in(i2,kupper(i2)),pres_in(i2,kupper(i2)+1),field_out(i2,k),field_in(i2,kupper(i2)),field_in(i2,kupper(i2)+1)/))
   
    enddo
   
    end subroutine interp_tofixed_pressure

   !==================================================================================================
    subroutine interp_field_cell_mass_levels(nCells, nVertLevels, pressure, field, num_iso_levels, &
                                             iso_levels, field_iso)
   !==================================================================================================
   
        implicit none
        
        integer, intent(in) :: nCells, nVertLevels
        real (kind=RKIND), dimension(:,:), intent(in) :: pressure
        real (kind=RKIND), dimension(:,:), intent(in) :: field
        integer, intent(in) :: num_iso_levels    
        real (kind=RKIND), dimension(:), intent(in) :: iso_levels
        real (kind=RKIND), dimension(:,:), intent(inout) :: field_iso

        ! Local index variables
        integer :: iCell, k, kk

        ! Pressure variables
        real (kind=RKIND), dimension(:,:), allocatable :: pressureCp1

        !local interpolated fields:
        real (kind=RKIND), dimension(:,:), allocatable :: field_in, press_in, press_in2
        real (kind=RKIND), dimension(:,:), allocatable :: field_interp, press_interp

        if(.not.allocated(pressureCp1) ) allocate(pressureCp1(nVertLevels,nCells+1) )

       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       !calculation of total pressure at cell centers (at mass points):
        do iCell = 1, nCells
            do k = 1, nVertLevels
               pressureCp1(k,iCell) = pressure(k,iCell)
            end do
        end do
        do iCell = nCells+1,nCells+1
            do k =1,nVertLevels
               pressureCp1(k,iCell) = pressure(k,iCell) 
            end do
        end do

        if(.not.allocated(press_interp)) allocate(press_interp(nCells, num_iso_levels))

        ! populate array with pressure levels for interpolation [in Pa]
        do k=1,num_iso_levels
           press_interp(:,k) = iso_levels(k)
        end do

        !--------------------------------------------------------------------
        ! Interpolate field:
        if(.not.allocated(field_in)) allocate(field_in(nCells,nVertLevels))
        if(.not.allocated(press_in)) allocate(press_in(nCells,nVertLevels))
        if(.not.allocated(field_interp)) allocate(field_interp(nCells, num_iso_levels))

        !reverse the vertical axis of array
        do iCell=1,nCells
           do k=1,nVertLevels
              kk = nVertLevels+1-k
              press_in(iCell,kk) = pressure(k,iCell) * 100.
              field_in(iCell,kk) = field(k,iCell)
           end do
        end do

        call interp_tofixed_pressure(nCells, nVertLevels, num_iso_levels, press_in, field_in, press_interp, field_interp)

        do k=1,num_iso_levels
           field_iso(k,1:nCells) = field_interp(1:nCells,k)
        end do

        if(allocated(press_in)) deallocate(press_in)
        if(allocated(field_in)) deallocate(field_in)
        if(allocated(field_interp)) deallocate(field_interp)

        if(allocated(pressureCp1)) deallocate(pressureCp1)

   end subroutine interp_field_cell_mass_levels     


   !==================================================================================================
    subroutine interp_field_vertex_mass_levels(nCells, nVertLevels, nVertices, vertexDegree, cellsOnVertex, & 
                                               kiteAreasOnVertex, areaTriangle, pressure, field,  &
                                               num_iso_levels, iso_levels, field_iso)
   !==================================================================================================
   
        implicit none
        
        integer, intent(in) :: nCells, nVertLevels, nVertices, vertexDegree
        integer, dimension(:,:), intent(in) :: cellsOnVertex
        real (kind=RKIND), dimension(:,:), intent(in) :: kiteAreasOnVertex
        real (kind=RKIND), dimension(:),   intent(in) :: areaTriangle
        real (kind=RKIND), dimension(:,:), intent(in) :: pressure                         ! in hPa 
        real (kind=RKIND), dimension(:,:), intent(in) :: field
        integer, intent(in) :: num_iso_levels    
        real (kind=RKIND), dimension(:), intent(in) :: iso_levels
        real (kind=RKIND), dimension(:,:), intent(inout) :: field_iso

        ! Local index variables
        integer :: iCell, k, kk, iVert, iVertD

        ! Pressure variables
        real (kind=RKIND), dimension(:,:), allocatable :: pressureCp1, pressure_v

        !local interpolated fields:
        real (kind=RKIND), dimension(:,:), allocatable :: field_in, press_in, press_in2
        real (kind=RKIND), dimension(:,:), allocatable :: field_interp, press_interp

        if(.not.allocated(pressureCp1)) allocate(pressureCp1(nVertLevels,nCells+1) )
        if(.not.allocated(pressure_v)) allocate(pressure_v(nVertLevels,nVertices))

       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       !calculation of total pressure at cell centers (at mass points):
        do iCell = 1, nCells
            do k = 1, nVertLevels
               pressureCp1(k,iCell) = pressure(k,iCell)
            end do
        end do
        do iCell = nCells+1,nCells+1
            do k =1,nVertLevels
               pressureCp1(k,iCell) = pressure(k,iCell) 
            end do
        end do

       !calculation of total pressure at cell vertices (at mass points):
        do iVert = 1, nVertices
           pressure_v(:,iVert) = 0._RKIND

           do k=1,nVertLevels
               do iVertD = 1, vertexDegree
                  pressure_v(k,iVert) = pressure_v(k,iVert) &
                      + kiteAreasOnVertex(iVertD,iVert)*pressureCp1(k,cellsOnVertex(iVertD,iVert))
               end do
               pressure_v(k,iVert) = pressure_v(k,iVert) / areaTriangle(iVert)
           end do
        end do

        if(.not.allocated(press_interp)) allocate(press_interp(nVertices, num_iso_levels))

        ! populate array with pressure levels for interpolation [in Pa]
        do k=1,num_iso_levels
           press_interp(:,k) = iso_levels(k)
        end do

        !--------------------------------------------------------------------
        ! Interpolate field:
        if(.not.allocated(field_in)) allocate(field_in(nVertices,nVertLevels))
        if(.not.allocated(press_in)) allocate(press_in(nVertices,nVertLevels))
        if(.not.allocated(field_interp)) allocate(field_interp(nVertices, num_iso_levels))

        !reverse the vertical axis of array
        do iVert=1,nVertices
           do k=1,nVertLevels
              kk = nVertLevels+1-k
              press_in(iVert,kk) = pressure_v(k,iVert) * 100.
              field_in(iVert,kk) = field(k,iVert)
           end do
        end do

        call interp_tofixed_pressure(nVertices, nVertLevels, num_iso_levels, press_in, field_in, press_interp, field_interp)

        do k=1,num_iso_levels
           field_iso(k,1:nVertices) = field_interp(1:nVertices,k)
        end do

        if(allocated(press_in)) deallocate(press_in)
        if(allocated(field_in)) deallocate(field_in)
        if(allocated(field_interp)) deallocate(field_interp)

        if(allocated(pressureCp1)) deallocate(pressureCp1)
        if(allocated(pressure_v)) deallocate(pressure_v)

   end subroutine interp_field_vertex_mass_levels    

   !==================================================================================================
    subroutine interp_field_cell_w_levels(nCells, nVertLevels, pressure, height, field, num_iso_levels, &
                                          iso_levels, field_iso)
   !==================================================================================================
   
        implicit none

        integer, intent(in) :: nCells, nVertLevels
        real (kind=RKIND), dimension(:,:), intent(in) :: pressure
        real (kind=RKIND), dimension(:,:), intent(in) :: height
        real (kind=RKIND), dimension(:,:), intent(in) :: field
        integer, intent(in) :: num_iso_levels    
        real (kind=RKIND), dimension(:), intent(in) :: iso_levels
        real (kind=RKIND), dimension(:,:), intent(inout) :: field_iso

        ! Local index variables
        integer :: iCell, k, kk
        integer :: nVertLevelsP1 

        ! Pressure variables
        real (kind=RKIND), dimension(:,:), allocatable :: pressure2

        !local interpolated fields:
        real (kind=RKIND) :: w1,w2,z0,z1,z2
        real (kind=RKIND), dimension(:,:), allocatable :: field_in, press_in, press_in2
        real (kind=RKIND), dimension(:,:), allocatable :: field_interp, press_interp

        nVertLevelsP1 = nVertLevels + 1
        
        if(.not.allocated(pressure2)) allocate(pressure2(nVertLevelsP1,nCells+1))

       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       !calculation of total pressure at cell centers (at vertical velocity points):
        k = nVertLevelsP1
        do iCell=1,nCells
           z0 = height(k,iCell)
           z1 = 0.5*(height(k,iCell)+height(k-1,iCell))
           z2 = 0.5*(height(k-1,iCell)+height(k-2,iCell))
           w1 = (z0-z2)/(z1-z2)
           w2 = 1.-w1
           ! use log of pressure to avoid occurrences of negative top-of-the-model pressure.
           pressure2(k,iCell) = exp(w1*log(pressure(k-1,iCell))+w2*log(pressure(k-2,iCell)))
        end do

        do k=2,nVertLevels
            do iCell=1,nCells
               w1 = (height(k,iCell)-height(k-1,iCell)) / (height(k+1,iCell)-height(k-1,iCell))
               w2 = (height(k+1,iCell)-height(k,iCell)) / (height(k+1,iCell)-height(k-1,iCell))
               ! switch to use ln(pressure) for more accurate vertical interpolation, WCS 20230407
               pressure2(k,iCell) = exp(w1*log(pressure(k,iCell)) + w2*log(pressure(k-1,iCell)))
            end do
        end do

        k = 1
        do iCell=1,nCells
           z0 = height(k,iCell)
           z1 = 0.5*(height(k,iCell)+height(k+1,iCell))
           z2 = 0.5*(height(k+1,iCell)+height(k+2,iCell))
           w1 = (z0-z2)/(z1-z2)
           w2 = 1.-w1
           ! switch to use ln(pressure) for more accurate vertical interpolation, WCS 20230407
           pressure2(k,iCell) = exp(w1*log(pressure(k,iCell))+w2*log(pressure(k+1,iCell)))
        end do

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!!!!!!!!! Interpolate fields to array of pressure levels !!!!!!!!!!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        if(.not.allocated(press_interp)) allocate(press_interp(nCells, num_iso_levels))

        ! populate array with pressure levels for interpolation [in Pa]
        do k=1,num_iso_levels
           press_interp(:,k) = iso_levels(k)
        end do

       !--------------------------------------------------------------------
        ! Interpolate field:
        if(.not.allocated(field_in)) allocate(field_in(nCells,nVertLevelsP1))
        if(.not.allocated(press_in)) allocate(press_in(nCells,nVertLevelsP1))
        if(.not.allocated(field_interp)) allocate(field_interp(nCells, num_iso_levels))

        !reverse the vertical axis of array
        do iCell=1,nCells
           do k=1,nVertLevelsP1
              kk = nVertLevelsP1+1-k
              press_in(iCell,kk) = pressure2(k,iCell) * 100.
              field_in(iCell,kk) = field(k,iCell)
           end do
        end do

        call interp_tofixed_pressure(nCells, nVertLevelsP1, num_iso_levels, press_in, field_in, press_interp, field_interp) 
        
        do k=1,num_iso_levels
           field_iso(k,1:nCells) = field_interp(1:nCells,k)
        end do
        
        if(allocated(press_in)) deallocate(press_in)
        if(allocated(field_in)) deallocate(field_in)
        if(allocated(field_interp)) deallocate(field_interp)

        if(allocated(pressure2)) deallocate(pressure2)

   end subroutine interp_field_cell_w_levels     


   !==================================================================================================
    subroutine calc_temperature_dewpoint(nCells, nVertLevels, qv, exner, theta, pressure, temperature, dewpoint)
   !==================================================================================================
   
        implicit none

        integer, intent(in) :: nCells, nVertLevels
        real (kind=RKIND), dimension(:,:), intent(in) :: qv, theta
        real (kind=RKIND), dimension(:,:), intent(in) :: exner, pressure
        real (kind=RKIND), dimension(:,:), intent(inout) :: temperature, dewpoint
    
        ! Local variables
        integer :: iCell, k
        real :: evp
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !calculation of temperature and dewpoint 
        do iCell=1,nCells
           do k=1,nVertLevels
              temperature(k,iCell) = theta(k,iCell)*exner(k,iCell) 

              ! Vapor pressure (NB: pressure here is already in hPa)
              evp = pressure(k,iCell) * qv(k,iCell) / (qv(k,iCell) + 0.622_RKIND)
              evp = max(evp, 1.0e-8_RKIND)

              ! Dewpoint temperature following Bolton (1980)
              dewpoint(k,iCell) = (243.5_RKIND * log(evp/6.112_RKIND)) / (17.67_RKIND - log(evp/6.112_RKIND))
              dewpoint(k,iCell) = dewpoint(k,iCell) + 273.15
           end do
        end do

    end subroutine calc_temperature_dewpoint
    

   !==================================================================================================
    subroutine compute_slp(ncol,nlev_in,nscalars,t,height,p,index_qv,scalars,slp)
   !==================================================================================================
   
       implicit none
   
      !input arguments:
       integer, intent(in) :: ncol, nlev_in, nscalars
      
      !p: in mb
      !t: in K
      !scalars: in kg/kg
      !height: in m
       real(kind=RKIND), intent(in), dimension(nlev_in,ncol) :: p,t
       real(kind=RKIND), intent(in), dimension(nlev_in+1,ncol) :: height
       integer, intent(in) :: index_qv
       real(kind=RKIND), intent(in), dimension(nscalars,nlev_in,ncol) :: scalars
      
      !output arguments:
       real(kind=RKIND), intent(out), dimension(ncol) :: slp
      
      !local variables:
       integer :: icol, k, kcount
       integer :: klo, khi
      
       real(kind=RKIND) :: gamma, rr, grav
       parameter (rr=287.0, grav=9.80616, gamma=0.0065)
      
       real(kind=RKIND) :: tc, pconst
       parameter (tc=273.16+17.5, pconst=100.)
      
       logical mm5_test
       parameter (mm5_test=.true.)
      
       integer, dimension(:), allocatable :: level
       real(kind=RKIND), dimension(:), allocatable :: t_surf, t_msl
       real(kind=RKIND) :: plo , phi , tlo, thi , zlo , zhi
       real(kind=RKIND) :: p_at_pconst , t_at_pconst , z_at_pconst, z_half_lowest
      
       logical :: l1, l2, l3, found
      
      ! Find least zeta level that is PCONST Pa above the surface.  We later use this
      ! level to extrapolate a surface pressure and temperature, which is supposed
      ! to reduce the effect of the diurnal heating cycle in the pressure field.
      
       if (.not.allocated(level))  allocate(level(ncol))
       if (.not.allocated(t_surf)) allocate(t_surf(ncol))
       if (.not.allocated(t_msl))  allocate(t_msl(ncol))
      
       do icol = 1 , ncol
          level(icol) = -1
      
          k = 1
          found = .false.
          do while ( (.not. found) .and. (k.le.nlev_in))
                if ( p(k,icol) .lt. p(1,icol)-pconst ) then
                   level(icol) = k
                   found = .true.
                end if
                k = k+1
          end do
      
          if ( level(icol) .eq. -1 ) then
             call mpas_log_write('Troubles finding level $r above ground.', realArgs=(/pconst/))
             call mpas_log_write('Problems first occur at ($i)', intArgs=(/icol/))
             call mpas_log_write('Surface pressure = $r hPa.', realArgs=(/p(1,icol)/))
             call mpas_log_write('*** MSLP field will not be computed')
             slp(:) = 0.0
             return
          end if
      
       end do
      
      ! Get temperature PCONST hPa above surface.  Use this to extrapolate
      ! the temperature at the surface and down to sea level.
      
       do icol = 1 , ncol
      
          klo = max ( level(icol) - 1 , 1      )
          khi = min ( klo + 1        , nlev_in - 1 )
      
          if ( klo .eq. khi ) then
             call mpas_log_write('Trapping levels are weird.')
             call mpas_log_write('icol = $i', intArgs=(/icol/))
             call mpas_log_write('klo = $i, khi = $i: and they should not be equal.', intArgs=(/klo,khi/))
             call mpas_log_write('*** MSLP field will not be computed')
             slp(:) = 0.0
             return
          end if
      
          plo = p(klo,icol)
          phi = p(khi,icol)
          tlo = t(klo,icol) * (1. + 0.608 * scalars(index_qv,klo,icol))
          thi = t(khi,icol) * (1. + 0.608 * scalars(index_qv,khi,icol))
          zlo = 0.5*(height(klo,icol)+height(klo+1,icol))
          zhi = 0.5*(height(khi,icol)+height(khi+1,icol))
      
          p_at_pconst = p(1,icol) - pconst
          t_at_pconst = thi-(thi-tlo)*log(p_at_pconst/phi)*log(plo/phi)
          z_at_pconst = zhi-(zhi-zlo)*log(p_at_pconst/phi)*log(plo/phi)
      
          t_surf(icol) = t_at_pconst*(p(1,icol)/p_at_pconst)**(gamma*rr/grav)
          t_msl(icol) = t_at_pconst+gamma*z_at_pconst
      
       end do
      
      ! If we follow a traditional computation, there is a correction to the sea level
      ! temperature if both the surface and sea level temnperatures are *too* hot.
      
       if ( mm5_test ) then
          kcount = 0
          do icol = 1 , ncol
                l1 = t_msl(icol) .lt. tc
                l2 = t_surf(icol) .le. tc
                l3 = .not. l1
                if ( l2 .and. l3 ) then
                   t_msl(icol) = tc
                else
                   t_msl(icol) = tc - 0.005*(t_surf(icol)-tc)**2
                   kcount = kcount+1
                end if
          end do
      !   call mpas_log_write('These number of points had t_msl adjusted $i', intArgs=(/kcount/))
       end if
      
       do icol = 1 , ncol
          z_half_lowest=0.5*(height(1,icol)+height(2,icol))
          slp(icol) = p(1,icol) * exp((2.*grav*z_half_lowest)/ &
                                    (rr*(t_msl(icol)+t_surf(icol))))
       end do
      
       if (allocated(level))  deallocate(level)
       if (allocated(t_surf)) deallocate(t_surf)
       if (allocated(t_msl))  deallocate(t_msl)
   
    end subroutine compute_slp


   !==================================================================================================
    subroutine interp_absVertVort(vorticity_vertex, nCells, nEdgesOnCell, verticesOnCell, &
                                cellsOnVertex, areaCell, kiteAreasOnVertex, vorticity_cell)
   !
   ! MC added: Subroutine to interpolate vertical vorticity to cell centers from the vertical vorticity at vertices                     
   !==================================================================================================

       IMPLICIT NONE

       integer, intent(in) :: nCells
       integer, dimension(:), intent(in) :: nEdgesOnCell
       integer, dimension(:,:), intent(in) :: verticesOnCell, cellsOnVertex
       real(kind=RKIND), dimension(:), intent(in) :: areaCell
       real(kind=RKIND), dimension(:,:), intent(in) :: vorticity_vertex, kiteAreasOnVertex
       real(kind=RKIND), dimension(:,:), intent(out) :: vorticity_cell
       integer :: i, j, cellIndOnVertex, iVertex

       vorticity_cell(:,:) = 0.0_RKIND

       do i=1,nCells
          do j=1,nEdgesOnCell(i)
             iVertex = verticesOnCell(j,i)
             cellIndOnVertex = FINDLOC(cellsOnVertex(:,iVertex),VALUE=i,DIM=1)
             vorticity_cell(:,i) = vorticity_cell(:,i) + kiteAreasOnVertex(cellIndOnVertex,iVertex) * vorticity_vertex(:,iVertex)
          end do
          vorticity_cell(:,i) = vorticity_cell(:,i) / areaCell(i)
       end do

    end subroutine interp_absVertVort


   !***********************************************************************
   !
   !  routine compute_layer_mean
   !
   !> \brief   Computes the mean of a field in the specified layer.
   !> \author  Michael Duda
   !> \date    3 July 2014
   !> \details
   !>  Given a 3d pressure field, press_in(nCells,nVertLevels), with pressure 
   !>  increasing with vertical index, and a 3d field, 
   !>  field_in(nCells,nVertLevels) with levels in the same order, this routine
   !>  will compute the mean of the field for each column between pressures
   !>  p1 and p2.
   !
   !----------------------------------------------------------------------- 
    subroutine compute_layer_mean(layerMean, p1, p2, field_in, press_in)
   
       implicit none
   
       real(kind=RKIND), dimension(:), intent(out) :: layerMean
       real(kind=RKIND), intent(in) :: p1, p2
       real(kind=RKIND), dimension(:,:), intent(in) :: field_in
       real(kind=RKIND), dimension(:,:), intent(in) :: press_in
   
       integer :: nCells, nVertLevels
       integer :: iCell, k
       integer :: k_bot, k_top
       real(kind=RKIND) :: p_bot, p_top
       real(kind=RKIND) :: wtop_p, wtop_m
       real(kind=RKIND) :: wbot_p, wbot_m
       real(kind=RKIND) :: wtotal, w
       real(kind=RKIND) :: temp
   
   
       !
       ! Get dimensions of input arrays
       !
       nCells = size(field_in, 1) 
       nVertLevels = size(field_in, 2) 
   
   
       !
       ! Check that pressure is increasing with index
       !
       if (press_in(1,1) > press_in(1,nVertLevels)) then
           call mpas_log_write('Error in compute_layer_mean: pressure should increase with index', messageType=MPAS_LOG_ERR)
           layerMean(:) = 0.0
           return
       end if
   
       
       !
       ! Set the pressure at the top and bottom of the layer
       !
       if (p1 < p2) then
          p_top = p1
          p_bot = p2
       else
          p_top = p2
          p_bot = p1
       end if
   
   
       !
       ! For each column, compute the mean value of the field between p_bot and
       ! p_top, with the field weighted by delta-p in each layer
       !
       do iCell=1,nCells
          k_bot = -1
          k_top = -1
   
          ! Search for trapping levels: k_top is the index just above (or equal to)
          ! p_top, and k_bot is the index just above (or equal to) p_bot.
          do k=1,nVertLevels-1
             if (press_in(iCell,k) <= p_top .and. press_in(iCell,k+1) > p_top) then
                k_top = k
                wtop_p = (p_top - press_in(iCell,k)) / (press_in(iCell,k+1) - press_in(iCell,k))
                wtop_m = (press_in(iCell,k+1) - p_top) / (press_in(iCell,k+1) - press_in(iCell,k))
             end if
             if (press_in(iCell,k) <= p_bot .and. press_in(iCell,k+1) > p_bot) then
                k_bot = k
                wbot_m = (p_bot - press_in(iCell,k)) / (press_in(iCell,k+1) - press_in(iCell,k))
                wbot_p = (press_in(iCell,k+1) - p_bot) / (press_in(iCell,k+1) - press_in(iCell,k))
             end if
          end do
   
          if (k_top == -1 .or. k_bot == -1) then      ! Layer intersects top or bottom boundary
   
             layerMean(iCell) = 0.0  
   
          else if (k_top == k_bot) then               ! Layer lies entirely within a single model layer
   
             layerMean(iCell) = wtop_m * field_in(iCell,k_top) + wtop_p * field_in(iCell,k_top+1)
             layerMean(iCell) = layerMean(iCell) + wbot_m * field_in(iCell,k_bot) + wbot_p * field_in(iCell,k_bot+1)
             layerMean(iCell) = 0.5 * layerMean(iCell)
   
          else
   
             ! First layer: from p_top down to press_in(iCell,k_top+1)
             wtotal = press_in(iCell,k_top+1) - p_top  
             temp = wtop_m * field_in(iCell,k_top) + wtop_p * field_in(iCell,k_top+1)
             layerMean(iCell) = wtotal * 0.5 * (field_in(iCell,k_top+1) + temp)
   
             ! Middle layers
             do k=k_top+1,k_bot-1
                w = press_in(iCell,k+1) - press_in(iCell,k)
                wtotal = wtotal + w
                layerMean(iCell) = layerMean(iCell) + w * 0.5 * (field_in(iCell,k) + field_in(iCell,k+1))
             end do
   
             ! Last layer: from press_in(iCell,k_bot) down to p_bot
             w = p_bot - press_in(iCell,k_bot)
             wtotal = wtotal + w
             temp = wbot_m * field_in(iCell,k_bot) + wbot_p * field_in(iCell,k_bot+1)
             layerMean(iCell) = layerMean(iCell) + w * 0.5 * (field_in(iCell,k_bot) + temp)
   
             layerMean(iCell) = layerMean(iCell) / wtotal
          end if
   
       end do
   
    end subroutine compute_layer_mean
   
end module mpas_isobaric_diagnostics
