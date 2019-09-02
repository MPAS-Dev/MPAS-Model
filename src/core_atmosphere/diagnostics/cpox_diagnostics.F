!  cpox_diagnostics
!
!  Created by John Wong, August / 2019
!  Copyright (c) 2019 The Weather Company. All rights reserved.
!
!  This module computes the conditional probability of precipitation.
!
!----------------------------------------------------------------------->

module cpox_diagnostics

    use mpas_derived_types, only : MPAS_pool_type
    use mpas_kind_types, only : RKIND
    use mpas_constants

    type (MPAS_pool_type), pointer :: mesh
    type (MPAS_pool_type), pointer :: state
    type (MPAS_pool_type), pointer :: diag

    ! For interpolation
    real (kind=RKIND), dimension(:), pointer :: fzm, fzp
    real (kind=RKIND), pointer :: cf1, cf2, cf3

    real (kind=RKIND), parameter :: freeze = 273.15
    real (kind=RKIND), parameter :: area_top = 3000.0 ! m
    ! This should provide more than enough vertical levels for area_top
    integer, parameter :: area_ktop = 16

    integer, parameter :: PTYPE_RAIN = 1
    integer, parameter :: PTYPE_FRZRAIN = 2
    integer, parameter :: PTYPE_PELLETS = 3
    integer, parameter :: PTYPE_SNOW = 4
    integer, parameter :: PTYPE_SNOW_RAIN = 5
    integer, parameter :: PTYPE_PELLETS_RAIN = 6
    integer, parameter :: PTYPE_PELLETS_FRZRAIN = 7

    real, parameter :: SFC_MELT = 13.2
    real, parameter :: SFC_AREA_FACTOR = 35.0
    real, parameter :: SFC_PARTIAL_MELT = 5.6
    real, parameter :: CROSSING_LOW = 46.0
    real, parameter :: CROSSING_HIGH = 66.0
    real, parameter :: RAINTHRESHOLDTEMP = freeze + 4.0

    public :: cpox_diagnostics_setup, &
              cpox_diagnostics_update, &
              cpox_diagnostics_compute, &
              cpox_diagnostics_reset, &
              cpox_diagnostics_cleanup


    private

    contains


    !-----------------------------------------------------------------------
    !  routine cpox_diagnostics_setup
    !
    !> \brief   Initialize the cpox_diagnostics module
    !> \author  John Wong
    !> \details
    !>  Initialize the diagnostic module.
    !
    !-----------------------------------------------------------------------
    subroutine cpox_diagnostics_setup(all_pools)

        use mpas_pool_routines, only : mpas_pool_get_subpool, mpas_pool_get_array

        implicit none

        type (MPAS_pool_type), pointer :: all_pools

        call mpas_pool_get_subpool(all_pools, 'mesh', mesh)
        call mpas_pool_get_subpool(all_pools, 'state', state)
        call mpas_pool_get_subpool(all_pools, 'diag', diag)

        ! Interpolation constants
        call mpas_pool_get_array(mesh, 'fzm', fzm)
        call mpas_pool_get_array(mesh, 'fzp', fzp)
        call mpas_pool_get_array(mesh, 'cf1', cf1)
        call mpas_pool_get_array(mesh, 'cf2', cf2)
        call mpas_pool_get_array(mesh, 'cf3', cf3)

    end subroutine cpox_diagnostics_setup


    subroutine cpox_diagnostics_update()
        implicit none
        ! Nothing to update (every timestep) for cpox
    end subroutine cpox_diagnostics_update

    !-----------------------------------------------------------------------
    !  These are functions used in compute
    !-----------------------------------------------------------------------

    ! Compute the "area" between wet-bulb temperature and freezing temperature
    real function area(tw1, tw2, z1, z2)
        real (kind=RKIND), intent(in) :: tw1, tw2, z1, z2
        real (kind=RKIND) :: avgtw
        avgtw = 0.5 * (tw1 + tw2)
        area = (avgtw - freeze) * (z2 - z1) * gravity / avgtw
    end function area

    real function z_cross(t1, t2, z1, z2, tx)
        real (kind=RKIND), intent(in) :: t1, t2, z1, z2, tx
        z_cross = z1 - ((t1 - tx) / (t1 - t2)) * (z1-z2)
    end function z_cross

    ! Taken from module_ra_cam_support.F
    real function saturation_vapor_pressure_over_water(t)
        real(kind=RKIND), intent(in) :: t

        real(kind=RKIND),parameter:: ps = 1013.246
        real(kind=RKIND),parameter:: ts = 373.16

        real :: e1, e2, f1, f2, f3, f4, f5, f

        e1 = 11.344*(1.0 - t/ts)
        e2 = -3.49149*(ts/t - 1.0)
        f1 = -7.90298*(ts/t - 1.0)
        f2 = 5.02808*log10(ts/t)
        f3 = -1.3816*(10.0**e1 - 1.0)/10000000.0
        f4 = 8.1328*(10.0**e2 - 1.0)/1000.0
        f5 = log10(ps)
        f  = f1 + f2 + f3 + f4 + f5
        saturation_vapor_pressure_over_water = (10.0**f)*100.0

    end function saturation_vapor_pressure_over_water

    ! Approximation for wet-bulb temperature
    real function wet_bulb(t, p, td)
        real (kind=RKIND), intent(in) :: t, p, td

        real (kind=RKIND), parameter :: Lv = 2.5e6

        real :: es, ed, ew, s, fp, tw, de, der
        integer :: tries

        real(kind=RKIND), parameter :: c1 = 0.0091379024
        real(kind=RKIND), parameter :: c2 = 6106.396

        wet_bulb = t
        if (abs(t-td) > 0.01) then
            es = saturation_vapor_pressure_over_water(t)
            ed = saturation_vapor_pressure_over_water(td)

            s = (es - ed)/(t-td)
            fp = cp * p * rvord/ Lv
            wet_bulb = (t * fp + td * s)/(fp + s)

            ew = saturation_vapor_pressure_over_water(wet_bulb)
            de = fp * (t - wet_bulb) - (ew - ed)

            tries = 0
            do while (tries < 10 .and. de > 1e-12)
                der = ew * (c1 - c2/(wet_bulb * wet_bulb)) - fp
                wet_bulb  = wet_bulb - de/der
                ew = saturation_vapor_pressure_over_water(wet_bulb)
                de = fp * (t - wet_bulb) - (ew - ed)
                tries = tries + 1
            end do
        end if
    end function wet_bulb

    ! Compute the Bourgouin Area which captures the temperature deviation from freezing
    ! and the height through which a hydrometeor would fall through.
    subroutine compute_bourgouin_areas(tw_u, z, nVertLevels, &
                                       surface_area, positive_area, negative_area, freezingHeight)
        implicit none

        integer, intent(in) :: nVertLevels
        real (kind=RKIND), dimension(nVertLevels), intent(in) :: tw_u, z
        real (kind=RKIND), intent(out) :: surface_area, positive_area, negative_area, freezingHeight
        real :: tw1, tw2, z1, z2, zx, ztop
        integer :: k
        logical :: surface

        surface_area = 0.0
        positive_area = -1e-12
        negative_area = 1e-12
        freezingHeight = -1e12
        surface = .true.

        ztop = z(1) + area_top

        do k=1, nVertLevels-1
            tw1 = tw_u(k)
            tw2 = tw_u(k+1)
            z1 = z(k)
            z2 = z(k+1)

            ! Lower top if we are at the stop limit
            if (z2 > ztop) then
                tw2 = z_cross(z1, z2, tw1, tw2, ztop)
                z2 = ztop
            end if

            if (surface) then
                ! Still accumulating surface_area
                if (tw1 < freeze .and. tw2 > freeze) then
                    freezingHeight = z_cross(tw1, tw2, z1, z2, freeze)
                    surface_area = surface_area + area(tw1, freeze, z1, freezingHeight)
                    positive_area = positive_area + area(freeze, tw2, freezingHeight, z2)
                    surface = .false.
                else if (tw1 > freeze .and. tw2 < freeze) then
                    freezingHeight = z_cross(tw1, tw2, z1, z2, freeze)
                    surface_area = surface_area + area(tw1, freeze, z1, freezingHeight)
                    negative_area = negative_area + area(freeze, tw2, freezingHeight, z2)
                    surface = .false.
                else
                    surface_area = surface_area + area(tw1, tw2, z1, z2)
                end if
            else
                ! Done with surface area
                if (tw1 < freeze .and. tw2 > freeze) then
                    zx = z_cross(tw1, tw2, z1, z2, freeze)
                    negative_area = negative_area + area(tw1, freeze, z1, zx)
                    positive_area = positive_area + area(freeze, tw2, zx, z2)
                else if (tw1 > freeze .and. tw2 < freeze) then
                    zx = z_cross(tw1, tw2, z1, z2, freeze)
                    positive_area = positive_area + area(tw1, freeze, z1, zx)
                    negative_area = negative_area + area(freeze, tw2, zx, z2)
                else if (tw1 < freeze) then
                    negative_area = negative_area + area(tw1, tw2, z1, z2)
                else
                    positive_area = positive_area + area(tw1, tw2, z1, z2)
                end if
            end if
        end do
    end subroutine compute_bourgouin_areas

    subroutine compute_cpox(t_surf, tw_surf, surf_area, pos_area, neg_area, frzZ, cpos, cpoi, cpor)
        implicit none

        ! In gridcalc neg_area is positive, here we calculated it as negative
        ! It is converted to positive when copied to negative_area.

        ! line numbers reference BourgouinArea.C in commit d4c6a5af798
        real (kind=RKIND), intent(in) :: t_surf, tw_surf, surf_area, pos_area, neg_area, frzZ
        real (kind=RKIND), intent(out) :: cpos, cpoi, cpor

        integer :: ptype ! Can output this in the future if necessary
        real :: cpo_surf_area, cpo_pos_area, offset, negative_area, positive_area, pos_offset

        ptype = PTYPE_RAIN
        cpos = 0.0
        cpoi = 0.0
        cpor = 0.0

        if (t_surf < RAINTHRESHOLDTEMP .and. ( tw_surf < freeze .or. surf_area < SFC_MELT )) then
            ! line 796
            cpo_surf_area = surf_area
            cpo_pos_area = pos_area
            if (cpo_pos_area < 0) cpo_pos_area = 0

            if (tw_surf - freeze < -0.1 .and. cpo_surf_area > 0.0) then
                cpo_surf_area = (tw_surf - freeze) * SFC_AREA_FACTOR
            end if

            if (pos_area < 2.0) then ! line 807
                if (cpo_surf_area < SFC_PARTIAL_MELT) then
                    ptype = PTYPE_SNOW;
                    if (frzZ < 0.0) then
                        cpos = 100.0
                    else
                        offset = 1.0 - cpo_surf_area/5.6
                        cpos = 75.0 + offset * (80.0-75.0)
                        if ( pos_area <= 0.0 .and. tw_surf < freeze) then
                            cpos = cpos + (272.15 - tw_surf)
                        end if
                        if (cpos > 100.0) cpos = 100.0
                    end if

                    offset = cpo_pos_area * 0.5
                    cpoi = 0.0 + offset * 20.0
                else if (cpo_surf_area >= SFC_PARTIAL_MELT .and. cpo_surf_area <= SFC_MELT) then
                    ! Could be snow or rain
                    if (cpo_surf_area - SFC_PARTIAL_MELT <= SFC_MELT - cpo_surf_area) then
                        ptype = PTYPE_SNOW
                    else
                        ptype = PTYPE_RAIN
                    end if

                    offset = 1.0 - ((cpo_surf_area - 5.6) / (13.2 - 5.6))
                    cpos = 40.0 + offset * (59.0 - 40.0)
                    offset = 1.0 - cpo_pos_area * 0.5
                    cpoi = 0.0 + offset * 20.0
                else
                    ptype = PTYPE_RAIN
                    if (cpo_surf_area > 18.2) then
                        cpos = 0.0
                    else
                        offset = 1.0 - ((cpo_surf_area - 13.2)/(18.2 - 13.2))
                        cpos = 0.0 + offset * 39.0
                    end if
                    cpoi = 0.0
                end if
            else ! pos_area > 2.0
                ! Line 877
                negative_area = neg_area
                positive_area = pos_area

                if (cpo_surf_area < 0.0) then
                    ! offset neg_area and make positive
                    negative_area = negative_area + cpo_surf_area
                    negative_area = abs(negative_area)
                else
                    positive_area = positive_area + cpo_surf_area
                end if

                if (negative_area >= CROSSING_HIGH + 0.66 * positive_area) then ! line 890
                    pos_offset = 66.0 + 0.66 * positive_area
                    if (cpo_surf_area < SFC_PARTIAL_MELT) then
                        ptype = PTYPE_PELLETS
                        if (negative_area > 66.0 + positive_area) then
                            cpoi = 100.0
                        else
                            offset = negative_area - pos_offset
                            offset = offset/((66.0 + positive_area) - pos_offset)
                            cpoi = 20.0 + offset * (100.0 - 20.0)
                        end if
                        if (positive_area > 10.0) then
                            cpos = 40.0
                        else
                            offset = 1.0 - ((positive_area - 2.0)/(10.0 - 20.0))
                            cpos = 40.0 + offset * (59.0 - 40.0)
                        end if
                    else if (cpo_surf_area >= SFC_PARTIAL_MELT .and. cpo_surf_area <= SFC_MELT) then
                        if (cpo_surf_area - SFC_PARTIAL_MELT <= SFC_MELT - cpo_surf_area) then
                            ptype = PTYPE_PELLETS
                        else
                            ptype = PTYPE_RAIN
                        end if
                        if (positive_area > 10.0) then
                            cpos = 40.0
                        else
                            offset = 1.0 - (positive_area - 2.0)/(10.0 - 2.0)
                            cpos = 40.0 + offset * (59.0 - 40.0)
                        end if
                        if (negative_area > 66.0 + positive_area) then
                            cpoi = 100.0
                        else
                            offset = negative_area - pos_offset
                            offset = offset/((66.0 * positive_area) - pos_offset)
                            cpoi = 20.0 + offset * (100.0 - 20.0)
                        end if
                    else ! line 964
                        ptype = PTYPE_RAIN
                        if (positive_area > 10.0) then
                            cpos = 0.0
                        else
                            offset = 1.0 - ((positive_area - 2.0)/(10.0 - 2.0))
                            cpos = 0.0 + offset * (40.0 - 0.0)
                        end if
                        if (negative_area > (66.0 + positive_area)) then
                            cpoi = 100.0
                        else
                            offset = negative_area - pos_offset
                            offset = offset/((66.0 + positive_area) - pos_offset)
                            cpoi = 0.0 + offset * (20.0 - 0.0)
                        end if
                    end if
                else if (negative_area < CROSSING_LOW + 0.66 * positive_area) then ! line 987
                    if (cpo_surf_area >= 0.0 .or. tw_surf > freeze) then
                        ptype = PTYPE_RAIN
                    else
                        ptype = PTYPE_FRZRAIN
                    end if

                    if (positive_area > 10.0) then
                        cpos = 0.0
                    else
                        offset = 1.0 - (positive_area - 2.0)/(10.0 - 2.0)
                        cpos = 0.0 + offset * 40.0
                    end if

                    offset = negative_area/ (46.0 + 0.66 * negative_area)
                    cpoi = 20.0 + offset * (100.0 - 20.0)

                else if (cpo_surf_area < SFC_MELT) then ! line 1023
                    if (cpo_surf_area <= SFC_MELT - cpo_surf_area) then
                        ptype = PTYPE_PELLETS
                    else
                        ptype = PTYPE_RAIN
                    end if
                    if (positive_area > 10.0) then
                        cpos = 40.0
                    else
                        offset = 1.0 - ((positive_area - 2.0)/(10.0 - 2.0))
                        cpos = 40.0 + offset * (59.0 - 40.0)
                    end if

                    pos_offset = 46.0 + 0.66 * positive_area
                    offset = 66.0 + 0.66 * positive_area
                    offset = 1.0 - (negative_area - pos_offset)/(offset - pos_offset)
                    cpoi = 20.0 + offset * (100.0 - 20.0)
                else ! line 1074
                    if (cpo_surf_area < 0.0) then
                        ptype = PTYPE_PELLETS
                    else
                        ptype = PTYPE_RAIN
                    end if
                    if (positive_area > 10.0) then
                        cpos = 0.0
                    else
                        offset = 1.0 - ((positive_area - 2.0)/(10.0 - 2.0))
                        cpos = 0.0 + offset * (40.0 - 0.0)
                    end if

                    pos_offset = 46.0 + 0.66 * positive_area
                    offset = 66.0 + 0.66 * positive_area
                    offset = 1.0 - (negative_area - pos_offset)/(offset - pos_offset)
                    cpoi = 0.0 + offset * (19.0 - 0.0)
                end if
            end if
        end if

        if (tw_surf < 271.60 .and. ptype == PTYPE_RAIN) then
            cpos = 0.0
            cpoi = 50.0
        end if
        if (tw_surf > 274.0 .and. ptype == PTYPE_FRZRAIN) then
            cpos = 0.0
            cpoi = 0.0
        end if

        offset = cpoi + cpos
        if (offset > 100.0) then
            cpoi = cpoi/offset * 100.0
            cpos = cpos/offset * 100.0
        endif
        cpoi = nint(max(0.0, min(cpoi, 100.0)))
        cpos = nint(max(0.0, min(cpos, 100.0)))
        cpor = max(0.0, min(100.0 - (cpoi + cpos), 100.0))

    end subroutine compute_cpox

    !-----------------------------------------------------------------------
    !  routine cpox_diagnostics_compute
    !
    !> \brief  Compute conditional probability
    !> \author John Wong
    !> \date   21 Aug 2019
    !> \details
    !>  Computes conditional probability of precipitations
    !
    !-----------------------------------------------------------------------
    subroutine cpox_diagnostics_compute()

        use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array
        use mpas_atm_diagnostics_utils, only : MPAS_field_will_be_written

        implicit none

        integer, pointer :: nVertLevels, nCells

        integer, pointer :: index_qv
        real (kind=RKIND), dimension(:,:), pointer :: exner, height, pressure_b, pressure_p, theta_m
        real (kind=RKIND), dimension(:,:,:), pointer :: scalars

        real (kind=RKIND), dimension(:), pointer :: cpoi, cpos, cpor

        real :: t_surf, surf_area, pos_area, neg_area, freezingHeight, evp, dewpoint

        real (kind=RKIND), dimension(:), allocatable :: temperature, pressure, wetbulb, tw_u

        integer :: iCell, k

        if ( MPAS_field_will_be_written('cposnow') .or. &
             MPAS_field_will_be_written('cpoice') .or. &
             MPAS_field_will_be_written('cporain') ) then
            call mpas_pool_get_dimension(mesh, 'nCells', nCells)
            call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)

            call mpas_pool_get_array(mesh, 'zgrid', height)

            call mpas_pool_get_dimension(state, 'index_qv', index_qv)
            call mpas_pool_get_array(state, 'scalars', scalars, 1)
            call mpas_pool_get_array(state, 'theta_m', theta_m, 1)
            call mpas_pool_get_array(diag, 'exner', exner)
            call mpas_pool_get_array(diag, 'pressure_base', pressure_b)
            call mpas_pool_get_array(diag, 'pressure_p', pressure_p)

            call mpas_pool_get_array(diag, 'cpoice', cpoi)
            call mpas_pool_get_array(diag, 'cposnow', cpos)
            call mpas_pool_get_array(diag, 'cporain', cpor)

            ! We can actually reduce temperature and wetbulb sizes by checking for max
            ! levels needed for the calculation (up to 3km)
            allocate(temperature(area_ktop))
            allocate(pressure(area_ktop))
            allocate(wetbulb(area_ktop))
            allocate(tw_u(area_ktop))

            do iCell=1, nCells
                ! Compute temperature and wetbulb at layer center
                temperature(1:area_ktop) = theta_m(1:area_ktop,iCell) / &
                                        (1._RKIND+rvord*scalars(index_qv,1:area_ktop,iCell)) * &
                                        exner(1:area_ktop,iCell)
                pressure(1:area_ktop) = pressure_p(1:area_ktop,iCell) + pressure_b(1:area_ktop,iCell)
                do k=1,area_ktop
                    evp = 0.01_RKIND * pressure(k) * scalars(index_qv,k,iCell) / (scalars(index_qv,k,iCell) + 0.622_RKIND)
                    evp = log(max(evp, 1.0e-8_RKIND)/6.112_RKIND)
                    dewpoint = (243.5_RKIND * evp) / (17.67_RKIND - evp)
                    wetbulb(k) = wet_bulb(temperature(k), pressure(k), dewpoint + 273.15)
                end do
                ! Interpolate Tw to layer interfaces
                t_surf  = cf1 * temperature(1) + cf2 * temperature(2) + cf3 * temperature(3)
                tw_u(1) = cf1 * wetbulb(1) + cf2 * wetbulb(2) + cf3 * wetbulb(3)
                do k=2,area_ktop
                    tw_u(k) = fzm(k) * wetbulb(k) + fzp(k) * wetbulb(k-1)
                end do
                call compute_bourgouin_areas(tw_u, height(1:area_ktop,iCell), area_ktop, &
                                             surf_area, pos_area, neg_area, freezingHeight)
                call compute_cpox(t_surf, tw_u(1), surf_area, pos_area, neg_area, freezingHeight, &
                                  cpos(iCell), cpoi(iCell), cpor(iCell))
            enddo

            deallocate(temperature)
            deallocate(pressure)
            deallocate(wetbulb)
            deallocate(tw_u)
        end if

    end subroutine cpox_diagnostics_compute


    subroutine cpox_diagnostics_reset()
        implicit none
        ! Nothing to reset
    end subroutine cpox_diagnostics_reset


    subroutine cpox_diagnostics_cleanup()
        implicit none
        ! Nothing to cleanup
    end subroutine cpox_diagnostics_cleanup


end module cpox_diagnostics

