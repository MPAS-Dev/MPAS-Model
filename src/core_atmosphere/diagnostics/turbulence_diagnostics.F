!  turbulence_diagnostics.F
!
!  Created by John Wong, July / 2018
!  Copyright (c) 2018 The Weather Company. All rights reserved.
!
!  This module computes diagnostics for turbulence computations.
!
!----------------------------------------------------------------------->

module turbulence_diagnostics

    use mpas_derived_types, only : MPAS_pool_type
    use mpas_kind_types, only : RKIND
    use mpas_pool_routines
    use mpas_constants
    use mpas_field_routines

    type (MPAS_pool_type), pointer :: mesh, state, diag, scratch
    real (kind=RKIND), pointer :: dt
    integer, pointer :: nCells, nCellsSolve, nVertLevels, nEdges, nEdgesSolve

    ! Top for turbulence calculations
    real (kind=RKIND), parameter :: turbulenceTop = 18288 ! m ~ FL600, top of class A airspace
    integer :: nTurbulenceLevels

    logical :: stream_ellrod3, stream_fronto, stream_mwtds
    real (kind=RKIND), dimension(:,:), allocatable :: div_old, div_save ! divergence from the previous time step
    real (kind=RKIND), dimension(:,:), allocatable :: th_old, th_save ! theta from previous time step
    real (kind=RKIND), dimension(:)  , allocatable :: mwtds_terfac ! terrain factor for mwtds
    integer          , dimension(:)  , allocatable :: mwtds_windtop ! top layer less than 1500m

    ! For interpolation
    real (kind=RKIND), dimension(:), pointer :: fzm, fzp
    real (kind=RKIND), pointer :: cf1, cf2, cf3

    public :: turbulence_diagnostics_setup, &
              turbulence_diagnostics_update, &
              turbulence_diagnostics_compute, &
              turbulence_diagnostics_reset, &
              turbulence_diagnostics_cleanup

    private

    contains


    !-----------------------------------------------------------------------
    !  routine turbulence_diagnostics_setup
    !
    !> \brief   Initialize the turbulence_diagnostics module
    !> \author  John Wong
    !> \date    9 July 2018
    !> \details
    !>  Initialize the diagnostic module.
    !
    !-----------------------------------------------------------------------
    subroutine turbulence_diagnostics_setup(configs, all_pools)

        use mpas_derived_types, only : MPAS_pool_type, MPAS_clock_type
        use mpas_atm_diagnostics_utils, only : mpas_stream_inclusion_count

        implicit none

        type (MPAS_pool_type), pointer :: all_pools, configs

        call mpas_pool_get_subpool(all_pools, 'mesh', mesh)
        call mpas_pool_get_subpool(all_pools, 'state', state)
        call mpas_pool_get_subpool(all_pools, 'diag', diag)
        call mpas_pool_get_subpool(all_pools, 'turbDiagnosticsScratch', scratch)

        call mpas_pool_get_dimension(mesh, 'nCells', nCells)
        call mpas_pool_get_dimension(mesh, 'nCellsSolve', nCellsSolve)
        call mpas_pool_get_dimension(mesh, 'nEdges', nEdges)
        call mpas_pool_get_dimension(mesh, 'nEdgesSolve', nEdgesSolve)
        call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
        call mpas_pool_get_config(configs, 'config_dt', dt)

        ! Interpolation constants
        call mpas_pool_get_array(mesh, 'fzm', fzm)
        call mpas_pool_get_array(mesh, 'fzp', fzp)
        call mpas_pool_get_array(mesh, 'cf1', cf1)
        call mpas_pool_get_array(mesh, 'cf2', cf2)
        call mpas_pool_get_array(mesh, 'cf3', cf3)

        call initializeArrays()

        ! Allocate scratch space for diagnostics
        if (mpas_stream_inclusion_count('ellrod3', direction=MPAS_STREAM_OUTPUT) > 0) then
            stream_ellrod3 = .true.
            allocate(div_save(nVertLevels,nCells+1))
            allocate(div_old(nVertLevels,nCells+1))

            div_save(:,:) = 0._RKIND
        else
            stream_ellrod3 = .false.
        end if
        if (mpas_stream_inclusion_count('frontogenesis', direction=MPAS_STREAM_OUTPUT) > 0) then
            stream_fronto = .true.
            allocate(th_save(nVertLevels,nCells+1))
            allocate(th_old(nVertLevels,nCells+1))

            th_save(:,:) = 0._RKIND
        else
            stream_fronto = .false.
        end if
        if (mpas_stream_inclusion_count('mwtds', direction=MPAS_STREAM_OUTPUT) > 0) then
            stream_mwtds = .true.
            allocate(mwtds_terfac(nCells))
            allocate(mwtds_windtop(nCells))

            call prepare_mwtds_terfac()
        else
            stream_mwtds = .false.
        end if

        nTurbulenceLevels = kTurbulenceTop()
        write(0,*) 'Turbulence top is set to level ', nTurbulenceLevels

    end subroutine


    !-----------------------------------------------------------------------
    !  routine turbulence_diagnostics_update
    !
    !> \brief  Update certain variables every single time step
    !> \author John Wong
    !> \date    9 July 2018
    !> \details
    !>  Update certain diagnostics at every time step.
    !
    !-----------------------------------------------------------------------
    subroutine turbulence_diagnostics_update()
        real (kind=RKIND), dimension(:,:), pointer :: divergence
        real (kind=RKIND), dimension(:,:), pointer :: theta

        if (stream_ellrod3) then
            call mpas_pool_get_array(diag, 'divergence', divergence)
            div_old(1:nTurbulenceLevels,:)  = div_save(1:nTurbulenceLevels,:)
            div_save(1:nTurbulenceLevels,:) = divergence(1:nTurbulenceLevels,:)
        end if
        if (stream_fronto) then
            call mpas_pool_get_array(diag, 'theta', theta)
            th_old(1:nTurbulenceLevels,:)  = th_save(1:nTurbulenceLevels,:)
            th_save(1:nTurbulenceLevels,:) = theta(1:nTurbulenceLevels,:)
        end if
    end subroutine turbulence_diagnostics_update


    !-----------------------------------------------------------------------
    !  routine turbulence_diagnostics_compute
    !
    !> \brief  Compute turbulence diagnostics at output timesteps
    !> \author John Wong
    !> \date    9 July 2018
    !> \details
    !>  Compute turbulence diagnostic at output intervals.
    !
    !-----------------------------------------------------------------------
    subroutine turbulence_diagnostics_compute()
        use mpas_atm_diagnostics_utils, only : MPAS_field_will_be_written

        implicit none

        logical :: need_windshear, need_temp, need_windsh_tw, need_gradTemp, need_ngm1
        logical :: need_def, need_ellrod2, need_ellrod3, need_ri, need_ritw, need_mwtds, need_iawind, need_fronto

        ! Scratch spaces
        type(field2DReal), pointer :: nsq_f, invH_f, gTsq_f, temp_f
        real (kind=RKIND), dimension(:,:), pointer :: nsq, invH

        need_ri        = ( MPAS_field_will_be_written('richno') .or. MPAS_field_will_be_written('invrichno') )
        need_mwtds     = ( stream_mwtds .and. MPAS_field_will_be_written('mwtds') )
        need_ngm1      = ( MPAS_field_will_be_written('ngm1') )
        need_ellrod2   = ( MPAS_field_will_be_written('ellrod2') )
        need_ellrod3   = ( stream_ellrod3 .and. MPAS_field_will_be_written('ellrod3') )
        need_windshear = ( need_ri .or. need_ellrod3 .or. MPAS_field_will_be_written('windshear') )
        need_def       = ( need_ngm1 .or. need_ellrod2 .or. need_ellrod3 .or. MPAS_field_will_be_written('deformation') )
        need_iawind    = ( MPAS_field_will_be_written('iawind') )
        need_fronto    = ( MPAS_field_will_be_written('frontogenesis') )

        ! Temperature gradient/thermal wind
        need_ritw      = ( MPAS_field_will_be_written('richno_tw') .or. MPAS_field_will_be_written('invrichno_tw') )
        need_windsh_tw = ( need_ritw .or. MPAS_field_will_be_written('windshear_tw') )
        need_gradTemp  = ( need_windsh_tw .or. MPAS_field_will_be_written('gradTmp') )
        need_temp      = ( MPAS_field_will_be_written('temperature') )

        ! Scratch space for shared diagnostics
        call mpas_pool_get_field(scratch, 'bruntvaisalaSq', nsq_f)
        call mpas_pool_get_field(scratch, 'invHeight', invH_f)
        call mpas_allocate_scratch_field(nsq_f, .true.)
        call mpas_allocate_scratch_field(invH_f, .true.)
        nsq => nsq_f % array
        invH => invH_f % array

        ! Compute inversed layer heights and buoyancy
        call layer_height_compute(invH)
        call buoyancy_compute(nsq)

        if ( need_windshear .or. need_windsh_tw ) then
            call speed_shear_compute()
        end if

        if ( need_def ) then
            call deformation_compute()
        end if

        if ( need_ngm1 ) then
            call ngm_compute()
        end if

        if (need_ellrod2) then
            call ellrod2_compute()
        end if

        if (need_ellrod3) then
            call ellrod3_compute()
        end if

        if (need_ri) then
            call richno_compute()
        end if

        if (need_gradTemp .or. need_temp) then
            call mpas_pool_get_field(scratch, 'gradTmpSq', gTsq_f)
            call mpas_allocate_scratch_field(gTsq_f, .true.)
            call gradTemp_compute()
            call thermal_wind_compute()
            call mpas_deallocate_scratch_field(gTsq_f, .true.)
        end if

        if (need_mwtds) then
            call mwtds_compute()
        end if

        if (need_iawind) then
            call iawind_compute()
        end if

        if (need_fronto) then
            call fronto_compute()
        end if

        call mpas_deallocate_scratch_field(nsq_f, .true.)
        call mpas_deallocate_scratch_field(invH_f, .true.)

    end subroutine turbulence_diagnostics_compute


    subroutine turbulence_diagnostics_reset()
        implicit none
        ! Nothing to reset for turbulence
    end subroutine turbulence_diagnostics_reset


    subroutine turbulence_diagnostics_cleanup()
        implicit none

        if (allocated(div_save)) deallocate(div_save)
        if (allocated(div_old)) deallocate(div_old)
        if (allocated(th_save)) deallocate(th_save)
        if (allocated(th_old)) deallocate(th_old)
        if (allocated(mwtds_terfac)) deallocate(mwtds_terfac)
        if (allocated(mwtds_windtop)) deallocate(mwtds_windtop)
    end subroutine turbulence_diagnostics_cleanup

!-----------------------------------------------------------------------------------
! Helper methods

    ! Find turbulence top to reduce computation
    function kTurbulenceTop()
        real (kind=RKIND), dimension(:), pointer :: dzu
        real (kind=RKIND) :: z
        integer :: k, kTurbulenceTop

        call mpas_pool_get_array(mesh, 'dzu', dzu)
        kTurbulenceTop = nVertLevels
        do k=1,nVertLevels
            z = z + dzu(k)
            if (z .gt. turbulenceTop) then
                kTurbulenceTop=k
                return
            end if
        end do
    end function kTurbulenceTop

    ! Initialize 3D arrays to zero
    subroutine initializeArrays()
        real (kind=RKIND), dimension(:,:), pointer :: p

        call mpas_pool_get_array(diag, 'gradTmp', p)
        p(:,:) = 0._RKIND
        call mpas_pool_get_array(diag, 'ngm1', p)
        p(:,:) = 0._RKIND
        call mpas_pool_get_array(diag, 'windshear', p)
        p(:,:) = 0._RKIND
        call mpas_pool_get_array(diag, 'deformation', p)
        p(:,:) = 0._RKIND
        call mpas_pool_get_array(diag, 'ellrod2', p)
        p(:,:) = 0._RKIND
        call mpas_pool_get_array(diag, 'richno', p)
        p(:,:) = 0._RKIND
        call mpas_pool_get_array(diag, 'invrichno', p)
        p(:,:) = 0._RKIND
        call mpas_pool_get_array(diag, 'richno_tw', p)
        p(:,:) = 0._RKIND
        call mpas_pool_get_array(diag, 'invrichno_tw', p)
        p(:,:) = 0._RKIND
        call mpas_pool_get_array(diag, 'iawind', p)
        p(:,:) = 0._RKIND
        call mpas_pool_get_array(diag, 'frontogenesis', p)
        p(:,:) = 0._RKIND
    end subroutine

    ! Precompute layer height
    subroutine layer_height_compute(rheight)
        real (kind=RKIND), dimension(:,:), pointer :: zz
        real (kind=RKIND), dimension(:), pointer :: rdzw
        real (kind=RKIND), dimension(:,:), intent(out) :: rheight
        integer :: iCell

        call mpas_pool_get_array(mesh, 'rdzw', rdzw) ! 1/d(zeta)
        call mpas_pool_get_array(mesh, 'zz', zz)     ! d(zeta)/dz
        do iCell=1,nCells
            rheight(:,iCell) = rdzw(:) * zz(:, iCell)
        end do
    end subroutine layer_height_compute

    ! Compute vertical differences at u from variables at u levels
    subroutine compute_vert_diff_u_u(uc, duc, nz)
        integer, intent(in) :: nz
        real (kind=RKIND), dimension(nz), intent(IN) :: uc ! column variable at u levels
        real (kind=RKIND), dimension(nz), intent(OUT) :: duc ! d(uc) at u levels
        integer :: k

        duc(1) = fzm(2) * uc(2) + fzp(2) * uc(1) - (cf1 * uc(1) + cf2 * uc(2) + cf3 * uc(3))
        do k=2, nz-1
            duc(k) = fzm(k+1) * uc(k+1) + (fzp(k+1) - fzm(k)) * uc(k) - fzp(k) * uc(k-1)
        end do
        duc(nz) = duc(nz-1)
    end subroutine compute_vert_diff_u_u

    ! Compute the brunt-vaisala frequency (squared)
    subroutine buoyancy_compute(Nsquared)
        real (kind=RKIND), dimension(:,:), intent(out) :: Nsquared
        real (kind=RKIND), dimension(:,:), pointer :: theta, rdz
        real (kind=RKIND), dimension(:), allocatable :: dth
        integer :: iCell

        allocate(dth(nVertLevels))
        call mpas_pool_get_array(scratch, 'invHeight', rdz)
        call mpas_pool_get_array(diag, 'theta', theta)
        do iCell = 1,nCells
            call compute_vert_diff_u_u(theta(:,iCell), dth, nVertLevels)
            Nsquared(:,iCell) = gravity/theta(:,iCell) * dth(:) * rdz(:,iCell)
        end do
        deallocate(dth)
    end subroutine buoyancy_compute

!-----------------------------------------------------------------------------------
! Wind speed, wind shears


    !-----------------------------------------------------------------------
    !  routine speed_shear_compute
    !
    !> \brief  Compute the wind speed and vertical wind shear at cell centers
    !> \author John Wong
    !> \date    9 July 2018
    !> \details
    !>  Compute wind speed and vertical shear at cell center and u levels.
    !
    !-----------------------------------------------------------------------
    subroutine speed_shear_compute()

        ! inputs
        real (kind=RKIND), dimension(:,:), pointer :: u, v, rdz

        ! output
        real (kind=RKIND), dimension(:,:), pointer :: windshear

        ! Local variables
        integer :: iCell, k
        real (kind=RKIND), dimension(:), allocatable :: du, dv, shearsq
        real (kind=RKIND), dimension(:), pointer :: uc, vc ! columns

        call mpas_pool_get_array(diag, 'uReconstructMeridional', v)
        call mpas_pool_get_array(diag, 'uReconstructZonal', u)

        call mpas_pool_get_array(diag, 'windshear', windshear)

        allocate(du(nTurbulenceLevels))
        allocate(dv(nTurbulenceLevels))
        call mpas_pool_get_array(scratch, 'invHeight', rdz)

        do iCell = 1, nCellsSolve
            ! Associate uc,vc with the current column
            uc => u(1:nTurbulenceLevels,iCell)
            vc => v(1:nTurbulenceLevels,iCell)

            ! Wind shear at cell centers
            call compute_vert_diff_u_u(uc, du, nTurbulenceLevels)
            call compute_vert_diff_u_u(vc, dv, nTurbulenceLevels)
            windshear(1:nTurbulenceLevels,iCell) = sqrt(du*du + dv*dv) * rdz(1:nTurbulenceLevels,iCell)
        end do
    end subroutine speed_shear_compute

!-----------------------------------------------------------------------------------
! Deformation

    !-----------------------------------------------------------------------
    !  routine deformation_compute
    !
    !> \brief  Compute deformation for model output
    !> \author John Wong
    !> \date   20 Nov 2017
    !> \details
    !>  Compute deformation around a cell. Implementation follows what was
    !>  done for Smagorinsky horizontal eddy viscosity.
    !
    !-----------------------------------------------------------------------
    subroutine deformation_compute()

        implicit none

        integer, pointer :: nEdges
        integer, dimension(:), pointer :: nEdgesOnCell
        integer, dimension(:,:), pointer :: edgesOnCell

        real (kind=RKIND), dimension(:,:), pointer :: u
        real (kind=RKIND), dimension(:,:), pointer :: v
        real (kind=RKIND), dimension(:,:), pointer :: defc_a
        real (kind=RKIND), dimension(:,:), pointer :: defc_b

        real (kind=RKIND), dimension(:), allocatable :: d_diag, d_off_diag

        real (kind=RKIND), dimension(:,:), pointer :: deformation

        integer :: iEdge, iCell, k, e

        ! Mesh information
        call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
        call mpas_pool_get_array(mesh, 'edgesOnCell', edgesOnCell)

        ! States and diagnostics, integration coefficients
        call mpas_pool_get_array(state, 'u', u, 2)
        call mpas_pool_get_array(diag, 'v', v)
        call mpas_pool_get_array(mesh, 'defc_a', defc_a)
        call mpas_pool_get_array(mesh, 'defc_b', defc_b)

        ! Target array
        call mpas_pool_get_array(diag, 'deformation', deformation)

        ! Allocate arrays for holding diagonal and off-diagonal deformation terms
        allocate(d_diag(nTurbulenceLevels))
        allocate(d_off_diag(nTurbulenceLevels))

        do iCell=1,nCellsSolve
            d_diag(:) = 0.0
            d_off_diag(:) = 0.0

            ! Integrate over edges around the cell
            do iEdge=1,nEdgesOnCell(iCell)
                e = edgesOnCell(iEdge,iCell)
                d_diag(:)     = d_diag(:)     + defc_a(iEdge,iCell)*u(1:nTurbulenceLevels,e) &
                                              - defc_b(iEdge,iCell)*v(1:nTurbulenceLevels,e)
                d_off_diag(:) = d_off_diag(:) + defc_b(iEdge,iCell)*u(1:nTurbulenceLevels,e) &
                                              + defc_a(iEdge,iCell)*v(1:nTurbulenceLevels,e)
            end do

            ! Combine the diagonal and off-diagonal terms
            deformation(1:nTurbulenceLevels,iCell) = sqrt(d_diag(:)**2 + d_off_diag(:)**2)
        end do

        deallocate(d_diag)
        deallocate(d_off_diag)

    end subroutine deformation_compute

!-----------------------------------------------------------------------------------
! NGM indices

    subroutine ngm_compute()

        implicit none

        ! input
        real (kind=RKIND), dimension(:,:), pointer :: u, v, deformation

        ! output
        real (kind=RKIND), dimension(:,:), pointer :: ngm1 ! wspd x |def|

        ! Local variables
        integer :: iCell, nT

        call mpas_pool_get_array(diag, 'uReconstructMeridional', v)
        call mpas_pool_get_array(diag, 'uReconstructZonal', u)
        call mpas_pool_get_array(diag, 'deformation', deformation)

        call mpas_pool_get_array(diag, 'ngm1', ngm1)

        nT = nTurbulenceLevels
        do iCell=1,nCellsSolve
            ngm1(1:nT,iCell) = sqrt(u(1:nT,iCell)**2 + v(1:nT,iCell)**2) * deformation(1:nT,iCell)
        end do

    end subroutine ngm_compute

!-----------------------------------------------------------------------------------
! Ellrod indices

    subroutine ellrod2_compute()
        real (kind=RKIND), dimension(:,:), pointer :: ellrod2, vws, def, div
        integer :: nT

        call mpas_pool_get_array(diag, 'windshear', vws)
        call mpas_pool_get_array(diag, 'deformation', def)
        call mpas_pool_get_array(diag, 'divergence', div)
        call mpas_pool_get_array(diag, 'ellrod2', ellrod2)

        nT = nTurbulenceLevels
        ellrod2(1:nT,:) = vws(1:nT,:) * (def(1:nT,:) - div(1:nT,:))
    end subroutine ellrod2_compute

    subroutine ellrod3_compute()
        real (kind=RKIND), dimension(:,:), pointer :: ellrod3, vws, def
        integer :: nT
        real (kind=RKIND), parameter :: CDVT = 1000._RKIND

        call mpas_pool_get_array(diag, 'windshear', vws)
        call mpas_pool_get_array(diag, 'deformation', def)
        call mpas_pool_get_array(diag, 'ellrod3', ellrod3)

        nT = nTurbulenceLevels
        ellrod3(1:nT,:) =  vws(1:nT,:) * def(1:nT,:) + abs(div_save(1:nT,:) - div_old(1:nT,:)) * CDVT/dt
    end subroutine ellrod3_compute

!-----------------------------------------------------------------------------------
! Richardson numbers

    subroutine richno_compute()

        ! inputs
        real (kind=RKIND), dimension(:,:), pointer :: shear, nsq

        ! outputs
        real (kind=RKIND), dimension(:,:), pointer :: ri, invri

        ! Local variables
        integer :: iCell, k
        real (kind=RKIND) :: shearsq

        call mpas_pool_get_array(diag, 'windshear', shear)
        call mpas_pool_get_array(diag, 'richno', ri)
        call mpas_pool_get_array(diag, 'invrichno', invri)
        call mpas_pool_get_array(scratch, 'bruntvaisalaSq', nsq)

        do iCell = 1,nCellsSolve
            do k = 1,nTurbulenceLevels
                shearsq = shear(k,iCell)*shear(k,iCell)
                ri(k,iCell) = nsq(k,iCell)/max(shearsq,1.0e-15_RKIND)
                invri(k,iCell) = shearsq/max(nsq(k,iCell),1.0e-15_RKIND)
            end do
        end do
    end subroutine richno_compute

!-----------------------------------------------------------------------------------
! Temperature gradient and thermal wind diagnostics

    subroutine gradTemp_compute()

        use mpas_dmpar, only : mpas_dmpar_exch_halo_field
        use mpas_derived_types, only : field3DReal, field2DReal
        use mpas_vector_reconstruction, only : mpas_reconstruct

        ! inputs
        real (kind=RKIND), dimension(:,:), pointer :: exner, theta_m, pressure_b, pressure_p
        real (kind=RKIND), dimension(:,:,:), pointer :: scalars
        integer, pointer :: index_qv
        real (kind=RKIND), dimension(:), pointer :: rdzw, invDcEdge

        ! fields for halo exchanges
        type (field3DReal), pointer :: scalar_f
        type (field2DReal), pointer :: theta_f, exner_f, pb_f, pp_f

        ! outputs
        real (kind=RKIND), dimension(:,:), pointer :: gradTmp

        ! scratch
        type (field3DReal), pointer :: gTXYZ_f
        type (field2DReal), pointer :: gTn_f, gTzonal_f, gTmeridional_f, gTsq_f
        real (kind=RKIND), dimension(:,:), pointer :: gTn, gTX, gTY, gTZ, gTzonal, gTmeridional, gTsq
        real (kind=RKIND), dimension(:,:), pointer :: temperature, zgrid, rdz, zxu

        ! local variables
        integer, dimension(:,:), pointer :: cellsOnEdge
        real (kind=RKIND), dimension(:,:), allocatable :: dTdz
        real (kind=RKIND), dimension(:), allocatable :: dT
        integer :: iCell, iEdge, k, cell1, cell2

        ! Exhange halos for prognostics
        call mpas_pool_get_field(state, 'scalars', scalar_f)
        call mpas_pool_get_field(state, 'theta_m', theta_f)
        call mpas_pool_get_field(diag , 'exner',   exner_f)

        call mpas_dmpar_exch_halo_field(scalar_f)
        call mpas_dmpar_exch_halo_field(theta_f)
        call mpas_dmpar_exch_halo_field(exner_f)

        ! Get the actual variables
        call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
        call mpas_pool_get_array(mesh, 'invDcEdge', invDcEdge)
        call mpas_pool_get_array(mesh, 'zxu', zxu)
        call mpas_pool_get_dimension(state, 'index_qv', index_qv)
        call mpas_pool_get_array(state, 'theta_m', theta_m, 1)
        call mpas_pool_get_array(state, 'scalars', scalars, 1)
        call mpas_pool_get_array(diag, 'exner', exner)
        call mpas_pool_get_array(scratch, 'invHeight', rdz)
        call mpas_pool_get_array(diag, 'temperature', temperature)

        ! Prepare scratch space
        call mpas_pool_get_field(scratch, 'gradTmp_n', gTn_f)
        call mpas_pool_get_field(scratch, 'gradTmpXYZ', gTXYZ_f)
        call mpas_pool_get_field(scratch, 'gradTmpZonal', gTzonal_f)
        call mpas_pool_get_field(scratch, 'gradTmpMeridional', gTmeridional_f)
        call mpas_allocate_scratch_field(gTn_f, .true.)
        call mpas_allocate_scratch_field(gTXYZ_f, .true.)
        call mpas_allocate_scratch_field(gTzonal_f, .true.)
        call mpas_allocate_scratch_field(gTmeridional_f, .true.)
        gTn => gTn_f % array
        gTX => gTXYZ_f % array(1,:,:)
        gTY => gTXYZ_f % array(2,:,:)
        gTZ => gTXYZ_f % array(3,:,:)
        gTzonal => gTzonal_f % array
        gTmeridional => gTmeridional_f % array

        ! Compute primary diagnostics
        temperature(:,:) = theta_m(:,:)/(1._RKIND+rvord*scalars(index_qv,:,:)) * exner(:,:)

        ! Compute dT/dz
        allocate(dT(nVertLevels))
        allocate(dTdz(nVertLevels,nCells))
        do iCell = 1,nCells
            call compute_vert_diff_u_u(temperature(:,iCell), dT, nVertLevels)
            dTdz(:,iCell) = dT * rdz(:,iCell)
        end do

        ! Compute gradients alone edges
        do iEdge = 1,nEdgesSolve
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)
            gTn(:,iEdge) = (temperature(:,cell2) - temperature(:,cell1)) * invDcEdge(iEdge) &
                            - 0.5 * (dTdz(:,cell2) + dTdz(:,cell1)) * zxu(:,iEdge)
        end do

        ! Reconstruct to cell center
        call mpas_reconstruct(mesh, gTn, gTX, gTY, gTZ, gTzonal, gTmeridional)

        ! Compute the final gradient
        call mpas_pool_get_array(diag, 'gradTmp', gradTmp)
        call mpas_pool_get_array(scratch, 'gradTmpSq', gTsq)
        gTsq(:,:) = gTzonal * gTzonal + gTmeridional * gTmeridional
        gradTmp(1:nTurbulenceLevels,:) = sqrt(gTsq(1:nTurbulenceLevels,:))

        ! Cleanup
        deallocate(dTdz)

        ! Release scratch space
        call mpas_deallocate_scratch_field(gTn_f, .true.)
        call mpas_deallocate_scratch_field(gTXYZ_f, .true.)
        call mpas_deallocate_scratch_field(gTzonal_f, .true.)
        call mpas_deallocate_scratch_field(gTmeridional_f, .true.)
    end subroutine gradTemp_compute


    ! Compute thermal wind derived wind shear and richardson number
    subroutine thermal_wind_compute()

        real (kind=RKIND), dimension(:,:), pointer :: nsq, rdz, gTsq, temperature
        real (kind=RKIND), dimension(:,:), pointer :: shear_tw, richno_tw, invrichno_tw

        real (kind=RKIND) :: fCellsq
        real (kind=RKIND), dimension(:), allocatable :: richno_n, richno_d
        integer :: iCell, k

        call mpas_pool_get_array(diag, 'temperature', temperature)

        ! diagnostics
        call mpas_pool_get_array(diag, 'windshear_tw', shear_tw)
        call mpas_pool_get_array(diag, 'richno_tw', richno_tw)
        call mpas_pool_get_array(diag, 'invrichno_tw', invrichno_tw)

        ! scratch fields
        call mpas_pool_get_array(scratch, 'gradTmpSq', gTsq)
        call mpas_pool_get_array(scratch, 'bruntvaisalaSq', nsq)
        call mpas_pool_get_array(scratch, 'invHeight', rdz)

        allocate(richno_n(nTurbulenceLevels))
        allocate(richno_d(nTurbulenceLevels))

        fCellsq = omega * omega ! Use omega to approximate f
        do iCell = 1,nCellsSolve
            richno_n = nsq(1:nTurbulenceLevels,iCell) * fCellsq
            richno_d = gTsq(1:nTurbulenceLevels,iCell) * (gravity / temperature(1:nTurbulenceLevels,iCell))**2
            shear_tw(1:nTurbulenceLevels,iCell) = sqrt(richno_d/fCellsq)
            do k = 1,nTurbulenceLevels
                richno_tw(k,iCell) = max(richno_n(k) / max(richno_d(k),1.0e-15_RKIND), 1.e-3_RKIND)
                invrichno_tw(k,iCell) = min(richno_d(k) / max(richno_n(k),1.0e-15_RKIND), 1.e+3_RKIND)
            end do
        end do

        deallocate(richno_n)
        deallocate(richno_d)

    end subroutine thermal_wind_compute

!-----------------------------------------------------------------------------------
! Mountain wave near-surface diagnostic (ds)

    ! Compute the terrain gradient at cell centers
    subroutine compute_terrain_gradient(ter, gradTer)
        use mpas_vector_reconstruction, only : mpas_reconstruct

        real (kind=RKIND), dimension(:), intent(out) :: gradTer
        integer          , dimension(:,:), pointer :: cellsOnEdge
        real (kind=RKIND), dimension(:), pointer :: invDcEdge, ter

        real (kind=RKIND), dimension(:), allocatable :: gradTer_edge, gTX, gTY, gTZ
        real (kind=RKIND), dimension(:), allocatable :: gTzonal, gTmeridional

        integer :: iEdge, cell1, cell2

        call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
        call mpas_pool_get_array(mesh, 'invDcEdge', invDcEdge)

        allocate(gradTer_edge(nEdges))
        allocate(gTX(nCells))
        allocate(gTY(nCells))
        allocate(gTZ(nCells))
        allocate(gTzonal(nCells))
        allocate(gTmeridional(nCells))

        do iEdge = 1, nEdgesSolve
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)
            gradTer_edge(iEdge) = (ter(cell2)-ter(cell1)) * invDcEdge(iEdge)
        end do

        call mpas_reconstruct(mesh, gradTer_edge, gTX, gTY, gTZ, gTzonal, gTmeridional)
        gradTer = sqrt(gTzonal*gTzonal + gTmeridional*gTmeridional)

        deallocate(gradTer_edge)
        deallocate(gTX)
        deallocate(gTY)
        deallocate(gTZ)
        deallocate(gTzonal)
        deallocate(gTmeridional)
    end subroutine compute_terrain_gradient

    ! Precompute terrain factor and wind speed for MWT near-surface diagnostics (mwtds)
    subroutine prepare_mwtds_terfac()
        real (kind=RKIND), dimension(:,:), pointer :: zgrid
        real (kind=RKIND), dimension(:), pointer :: ter
        real (kind=RKIND), dimension(:), allocatable :: gradTer

        integer :: iCell, k
        real (kind=RKIND), parameter :: heightThreshold = 200.0 ! m
        real (kind=RKIND), parameter :: gradThreshold = 0.005 ! m m^{-1}
        real (kind=RKIND), parameter :: terfacMin = 2750.0 ! m
        real (kind=RKIND), parameter :: windtopThreshold = 1500.0 ! m

        allocate(gradTer(nCells))
        call mpas_pool_get_array(mesh, 'zgrid', zgrid)
        ter => zgrid(1,:)

        call compute_terrain_gradient(ter,gradTer)

        do iCell = 1,nCellsSolve
            ! Compute the terrain height factor
            if (ter(iCell) < heightThreshold .or. gradTer(iCell) < gradThreshold) then
                mwtds_terfac(iCell) = 0.0_RKIND
            else
                mwtds_terfac(iCell) = min(terfacMin, ter(iCell))
            end if

            ! Find the last level less than windtopThreshold
            k = 1
            do while((zgrid(k+1,iCell)-ter(iCell)) .lt. windtopThreshold)
                k = k + 1
            end do
            mwtds_windtop(iCell) = k
        end do

        deallocate(gradTer)
    end subroutine prepare_mwtds_terfac

    ! Compute mwtds
    subroutine mwtds_compute()
        real (kind=RKIND), dimension(:), pointer :: mwtds
        real (kind=RKIND), dimension(:,:), pointer :: uz, um
        real (Kind=RKIND), dimension(:), allocatable :: wspdsq
        integer :: iCell, bltop

        nVertLevelsP1 = nVertLevels + 1

        call mpas_pool_get_array(diag, 'mwtds', mwtds)
        call mpas_pool_get_array(diag, 'uReconstructZonal', uz)
        call mpas_pool_get_array(diag, 'uReconstructMeridional', um)

        allocate(wspdsq(nVertLevels))
        do iCell = 1,nCellsSolve
            bltop = mwtds_windtop(iCell)
            wspdsq(1:bltop) = uz(1:bltop,iCell)**2 + um(1:bltop,iCell)**2
            mwtds(iCell) = sqrt(maxval(wspdsq(1:bltop))) * mwtds_terfac(iCell)
        end do
        deallocate(wspdsq)
    end subroutine mwtds_compute

!-----------------------------------------------------------------------------------
! Inertial advective wind diagnostic

    subroutine iawind_compute

        use mpas_dmpar, only : mpas_dmpar_exch_halo_field
        use mpas_derived_types, only : field3DReal, field2DReal
        use mpas_vector_reconstruction, only : mpas_reconstruct

        ! scratch
        type (field3DReal), pointer :: gTXYZ_f
        type (field2DReal), pointer :: gTn_f, gTzonal_f, gTmeridional_f, gTsq_f, um_f, uz_f
        real (kind=RKIND), dimension(:,:), pointer :: gTn, gTX, gTY, gTZ, gTzonal, gTmeridional

        ! input
        real (kind=RKIND), dimension(:,:), pointer :: u, v, zxu, rdz
        integer          , dimension(:,:), pointer :: cellsOnEdge
        real (kind=RKIND), dimension(:)  , pointer :: invDcEdge
        ! output
        real (kind=RKIND), dimension(:,:), pointer :: iawind

        ! Local variables
        real (kind=RKIND), dimension(:,:), allocatable :: dudz, dvdz
        real (kind=RKIND), dimension(:), allocatable :: du, dv
        integer :: iCell, iEdge, cell1, cell2, nT

        ! Top of calculations performed for turbulence
        nT = nTurbulenceLevels

        ! Exchange u and v halos for gradient calculations
        call mpas_pool_get_field(diag, 'uReconstructZonal', uz_f)
        call mpas_pool_get_field(diag, 'uReconstructMeridional', um_f)
        call mpas_dmpar_exch_halo_field(uz_f)
        call mpas_dmpar_exch_halo_field(um_f)

        ! Allocate scratch space for gradient reconstruction
        call mpas_pool_get_field(scratch, 'gradTmp_n', gTn_f)
        call mpas_pool_get_field(scratch, 'gradTmpXYZ', gTXYZ_f)
        call mpas_pool_get_field(scratch, 'gradTmpZonal', gTzonal_f)
        call mpas_pool_get_field(scratch, 'gradTmpMeridional', gTmeridional_f)
        call mpas_allocate_scratch_field(gTn_f, .true.)
        call mpas_allocate_scratch_field(gTXYZ_f, .true.)
        call mpas_allocate_scratch_field(gTzonal_f, .true.)
        call mpas_allocate_scratch_field(gTmeridional_f, .true.)
        gTn => gTn_f % array
        gTX => gTXYZ_f % array(1,:,:)
        gTY => gTXYZ_f % array(2,:,:)
        gTZ => gTXYZ_f % array(3,:,:)
        gTzonal => gTzonal_f % array
        gTmeridional => gTmeridional_f % array

        ! Get u, v
        call mpas_pool_get_array(diag, 'uReconstructZonal', u)
        call mpas_pool_get_array(diag, 'uReconstructMeridional', v)
        call mpas_pool_get_array(mesh, 'invDcEdge', invDcEdge)
        call mpas_pool_get_array(mesh, 'zxu', zxu)
        call mpas_pool_get_array(scratch, 'invHeight', rdz)
        call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)

        ! Compute du/dz and dv/dz
        allocate(du(nVertLevels))
        allocate(dv(nVertLevels))
        allocate(dudz(nVertLevels, nCells))
        allocate(dvdz(nVertLevels, nCells))
        do iCell = 1,nCells
            call compute_vert_diff_u_u(u(:,iCell), du, nVertLevels)
            call compute_vert_diff_u_u(v(:,iCell), dv, nVertLevels)
            dudz(:,iCell) = du * rdz(:,iCell)
            dvdz(:,iCell) = dv * rdz(:,iCell)
        end do

        call mpas_pool_get_array(diag, 'iawind', iawind)

        ! Compute for u * dudx
        do iEdge = 1,nEdgesSolve
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)
            gTn(:,iEdge) = (u(:,cell2) - u(:,cell1)) * invDcEdge(iEdge) &
                            - 0.5 * (dudz(:,cell2) + dudz(:,cell1)) * zxu(:,iEdge)
        end do
        call mpas_reconstruct(mesh, gTn, gTX, gTY, gTZ, gTzonal, gTmeridional)
        iawind(1:nT,:) = (u(1:nT,:) * gTzonal(1:nT,:))**2

        ! Compute for v * dvdy
        do iEdge = 1,nEdgesSolve
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)
            gTn(:,iEdge) = (v(:,cell2) - v(:,cell1)) * invDcEdge(iEdge) &
                        - 0.5 * (dvdz(:,cell2) + dvdz(:,cell1)) * zxu(:,iEdge)
        end do
        call mpas_reconstruct(mesh, gTn, gTX, gTY, gTZ, gTzonal, gTmeridional)
        iawind(1:nT,:) = iawind(1:nT,:) + (v(1:nT,:) * gTmeridional(1:nT,:))**2

        ! Finalize iawind
        iawind(1:nT,:) = sqrt(iawind(1:nT,:))/omega

        ! Clean up
        deallocate(du)
        deallocate(dv)
        deallocate(dudz)
        deallocate(dvdz)

        ! Release scratch space
        call mpas_deallocate_scratch_field(gTn_f, .true.)
        call mpas_deallocate_scratch_field(gTXYZ_f, .true.)
        call mpas_deallocate_scratch_field(gTzonal_f, .true.)
        call mpas_deallocate_scratch_field(gTmeridional_f, .true.)
    end subroutine iawind_compute

!-----------------------------------------------------------------------------------
! 2D Frontogenesis diagnostic as Theta gradient tendency

    subroutine fronto_compute()
        use mpas_derived_types, only : field3DReal, field2DReal
        use mpas_vector_reconstruction, only : mpas_reconstruct

        ! scratch
        type (field3DReal), pointer :: gTXYZ_f
        type (field2DReal), pointer :: gTn_f, gTzonal_f, gTmeridional_f, gTsq_f
        real (kind=RKIND), dimension(:,:), pointer :: gTn, gTX, gTY, gTZ, gTzonal, gTmeridional

        ! input
        real (kind=RKIND), dimension(:,:), pointer :: zxu, rdz
        integer          , dimension(:,:), pointer :: cellsOnEdge
        real (kind=RKIND), dimension(:)  , pointer :: invDcEdge
        ! output
        real (kind=RKIND), dimension(:,:), pointer :: frontogenesis

        ! Local variables
        real (kind=RKIND), dimension(:,:), allocatable :: dthdz
        real (kind=RKIND), dimension(:), allocatable :: dth
        integer :: iCell, iEdge, cell1, cell2, nT

        ! Top of calculations performed for turbulence
        nT = nTurbulenceLevels

        ! Allocate scratch space for gradient reconstruction
        call mpas_pool_get_field(scratch, 'gradTmp_n', gTn_f)
        call mpas_pool_get_field(scratch, 'gradTmpXYZ', gTXYZ_f)
        call mpas_pool_get_field(scratch, 'gradTmpZonal', gTzonal_f)
        call mpas_pool_get_field(scratch, 'gradTmpMeridional', gTmeridional_f)
        call mpas_allocate_scratch_field(gTn_f, .true.)
        call mpas_allocate_scratch_field(gTXYZ_f, .true.)
        call mpas_allocate_scratch_field(gTzonal_f, .true.)
        call mpas_allocate_scratch_field(gTmeridional_f, .true.)
        gTn => gTn_f % array
        gTX => gTXYZ_f % array(1,:,:)
        gTY => gTXYZ_f % array(2,:,:)
        gTZ => gTXYZ_f % array(3,:,:)
        gTzonal => gTzonal_f % array
        gTmeridional => gTmeridional_f % array

        ! Get mesh structure
        call mpas_pool_get_array(mesh, 'invDcEdge', invDcEdge)
        call mpas_pool_get_array(mesh, 'zxu', zxu)
        call mpas_pool_get_array(scratch, 'invHeight', rdz)
        call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)

        allocate(dth(nVertLevels))
        allocate(dthdz(nVertLevels, nCells))

        ! Compute dth/dz (save)
        do iCell = 1,nCells
            call compute_vert_diff_u_u(th_save(:,iCell), dth, nVertLevels)
            dthdz(:,iCell) = dth * rdz(:,iCell)
        end do

        ! Compute for GradTh (save)
        do iEdge = 1,nEdgesSolve
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)
            gTn(:,iEdge) = (th_save(:,cell2) - th_save(:,cell1)) * invDcEdge(iEdge) &
                            - 0.5 * (dthdz(:,cell2) + dthdz(:,cell1)) * zxu(:,iEdge)
        end do
        call mpas_reconstruct(mesh, gTn, gTX, gTY, gTZ, gTzonal, gTmeridional)

        call mpas_pool_get_array(diag, 'frontogenesis', frontogenesis)
        frontogenesis(1:nT,:) = sqrt(gTzonal(1:nT,:)**2 + gTmeridional(1:nT,:)**2)


        ! Compute dth/dz (old)
        do iCell = 1,nCells
            call compute_vert_diff_u_u(th_old(:,iCell), dth, nVertLevels)
            dthdz(:,iCell) = dth * rdz(:,iCell)
        end do

        ! Compute for GradTh (save)
        do iEdge = 1,nEdgesSolve
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)
            gTn(:,iEdge) = (th_old(:,cell2) - th_old(:,cell1)) * invDcEdge(iEdge) &
                            - 0.5 * (dthdz(:,cell2) + dthdz(:,cell1)) * zxu(:,iEdge)
        end do
        call mpas_reconstruct(mesh, gTn, gTX, gTY, gTZ, gTzonal, gTmeridional)
        frontogenesis(1:nT,:) = frontogenesis(1:nT,:) - sqrt(gTzonal(1:nT,:)**2 + gTmeridional(1:nT,:)**2)

        ! Finalize frontogenesis
        frontogenesis = abs(frontogenesis/dt)

        ! Clean up
        deallocate(dth)
        deallocate(dthdz)

        ! Release scratch space
        call mpas_deallocate_scratch_field(gTn_f, .true.)
        call mpas_deallocate_scratch_field(gTXYZ_f, .true.)
        call mpas_deallocate_scratch_field(gTzonal_f, .true.)
        call mpas_deallocate_scratch_field(gTmeridional_f, .true.)
    end subroutine fronto_compute


end module turbulence_diagnostics
