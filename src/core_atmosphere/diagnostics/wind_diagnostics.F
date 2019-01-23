! Copyright (c) 2016,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
! Brett Wilt, TWC / IBM, 10/19/2018
! This routine computes wind (zonal and meridional components) at hub heights

module wind_diagnostics

    use mpas_kind_types, only : RKIND, StrKIND
    use mpas_derived_types, only : MPAS_pool_type

    type (MPAS_pool_type), pointer :: mesh
    type (MPAS_pool_type), pointer :: diag
    type (MPAS_pool_type), pointer :: sfc_input

    public :: wind_diagnostics_setup, &
              wind_diagnostics_compute

    contains


    !-----------------------------------------------------------------------
    !  routine wind_diagnostics_setup
    !
    !> \brief Set-up the wind diagnostics module
    !> \author Brett Wilt
    !> \date   10/19/2018
    !> \details
    !>  To avoid later work in dereferencing pointers to various pools,
    !>  this routine saves pool pointers for use by
    !>  the surface_diagnostics_compute routine.
    !
    !-----------------------------------------------------------------------
    subroutine wind_diagnostics_setup(all_pools)

        use mpas_derived_types, only : MPAS_pool_type
        use mpas_pool_routines, only : mpas_pool_get_subpool

        implicit none

        type (MPAS_pool_type), pointer :: all_pools
        
        call mpas_pool_get_subpool(all_pools, 'mesh', mesh)
        call mpas_pool_get_subpool(all_pools, 'diag', diag)
        call mpas_pool_get_subpool(all_pools, 'sfc_input', sfc_input)

    end subroutine wind_diagnostics_setup


    !-----------------------------------------------------------------------
    !  routine wind_diagnostics_compute
    !
    !> \brief Computes wind diagnostics
    !> \author Brett Wilt
    !> \date   10/19/2018
    !> \details
    !>  This routine computes wind diagnostics at various hub heights
    !>  ex: 80m agl
    !
    !-----------------------------------------------------------------------
    subroutine wind_diagnostics_compute()

        use mpas_atm_diagnostics_utils, only : MPAS_field_will_be_written
        use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array

        implicit none

        integer :: iCell
        integer, pointer :: nCells, nVertLevels

        real (kind=RKIND), dimension(:), pointer :: height_terrain
        real (kind=RKIND), dimension(:,:), pointer :: height
        real (kind=RKIND), dimension(:,:), pointer :: uzonal
        real (kind=RKIND), dimension(:,:), pointer :: umeridional
    
        real (kind=RKIND), dimension(:), pointer :: uzonal_60m
        real (kind=RKIND), dimension(:), pointer :: uzonal_80m
        real (kind=RKIND), dimension(:), pointer :: uzonal_100m
        real (kind=RKIND), dimension(:), pointer :: umeridional_60m
        real (kind=RKIND), dimension(:), pointer :: umeridional_80m
        real (kind=RKIND), dimension(:), pointer :: umeridional_100m

        real (kind=RKIND), dimension(:), allocatable :: zagl

        logical :: need_uzonal_60m, need_umeridional_60m
        logical :: need_uzonal_80m, need_umeridional_80m
        logical :: need_uzonal_100m, need_umeridional_100m
        logical :: need_any_diags

        need_any_diags = .false.
        need_uzonal_60m = MPAS_field_will_be_written('uzonal_60m')
        need_any_diags = need_any_diags .or. need_uzonal_60m
        need_umeridional_60m = MPAS_field_will_be_written('umeridional_60m')
        need_any_diags = need_any_diags .or. need_umeridional_60m
        need_uzonal_80m = MPAS_field_will_be_written('uzonal_80m')
        need_any_diags = need_any_diags .or. need_uzonal_80m
        need_umeridional_80m = MPAS_field_will_be_written('umeridional_80m')
        need_any_diags = need_any_diags .or. need_umeridional_80m
        need_uzonal_100m = MPAS_field_will_be_written('uzonal_100m')
        need_any_diags = need_any_diags .or. need_uzonal_100m
        need_umeridional_100m = MPAS_field_will_be_written('umeridional_100m')
        need_any_diags = need_any_diags .or. need_umeridional_100m

        if(need_any_diags) then

            call mpas_pool_get_dimension(mesh, 'nCells', nCells)
            call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
            call mpas_pool_get_array(sfc_input, 'ter', height_terrain)
            call mpas_pool_get_array(diag, 'zgrid_unstag', height)
            call mpas_pool_get_array(diag, 'uReconstructZonal', uzonal)
            call mpas_pool_get_array(diag, 'uReconstructMeridional', umeridional)

            if (need_uzonal_60m) then
                call mpas_pool_get_array(diag, 'uzonal_60m', uzonal_60m)
            end if
            if (need_umeridional_60m) then
                call mpas_pool_get_array(diag, 'umeridional_60m', umeridional_60m)
            end if
            if (need_uzonal_80m) then
                call mpas_pool_get_array(diag, 'uzonal_80m', uzonal_80m)
            end if
            if (need_umeridional_80m) then
                call mpas_pool_get_array(diag, 'umeridional_80m', umeridional_80m)
            end if
            if (need_uzonal_100m) then
                call mpas_pool_get_array(diag, 'uzonal_100m', uzonal_100m)
            end if
            if (need_umeridional_100m) then
                call mpas_pool_get_array(diag, 'umeridional_100m', umeridional_100m)
            end if

            allocate(zagl(nVertLevels))

            do iCell=1,nCells
                zagl(1:nVertLevels) = height(1:nVertLevels,iCell) - height_terrain(iCell)
                if (need_uzonal_60m) then
                    uzonal_60m(iCell) = column_height_value(uzonal(1:nVertLevels,iCell),zagl,60.,nVertLevels)
                end if
                if (need_umeridional_60m) then
                    umeridional_60m(iCell) = column_height_value(umeridional(1:nVertLevels,iCell),zagl,60.,nVertLevels)
                end if
                if (need_uzonal_80m) then
                    uzonal_80m(iCell) = column_height_value(uzonal(1:nVertLevels,iCell),zagl,80.,nVertLevels)
                end if
                if (need_umeridional_80m) then
                    umeridional_80m(iCell) = column_height_value(umeridional(1:nVertLevels,iCell),zagl,80.,nVertLevels)
                end if
                if (need_uzonal_100m) then
                    uzonal_100m(iCell) = column_height_value(uzonal(1:nVertLevels,iCell),zagl,100.,nVertLevels)
                end if
                if (need_umeridional_100m) then
                    umeridional_100m(iCell) = column_height_value(umeridional(1:nVertLevels,iCell),zagl,100.,nVertLevels)
                end if
            end do

            deallocate(zagl)

        end if

    end subroutine wind_diagnostics_compute


    real (kind=RKIND) function column_height_value( column_values, z, z_interp, n )
    implicit none
    integer n
    real (kind=RKIND) :: column_values(n), z(n), z_interp, wz, wzp1
    integer :: kz, k

    kz = 1
    do k=1,n
      if(z(k) <= z_interp) kz = k
    end do
    kz = min(kz,n-1)
  
    wz = (z(kz+1)-z_interp)/(z(kz+1)-z(kz))
    wzp1 = 1. - wz
    column_height_value = wz*column_values(kz) + wzp1*column_values(kz+1)

    end function column_height_value


end module wind_diagnostics
