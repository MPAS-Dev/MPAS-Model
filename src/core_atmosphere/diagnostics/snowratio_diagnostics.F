! Copyright (c) 2016,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
! Brett Wilt, TWC / IBM, 04/14/2017
! This routine computes snow water ratio using kuchera, cobb, noahsp, noahmp, static schemes

module snowratio_diagnostics

    use mpas_kind_types, only : RKIND, StrKIND
    use mpas_derived_types, only : MPAS_pool_type
    use mpas_pool_routines

    type (MPAS_pool_type), pointer :: mesh
    type (MPAS_pool_type), pointer :: state
    type (MPAS_pool_type), pointer :: diag
    type (MPAS_pool_type), pointer :: diag_physics

    public :: snowratio_diagnostics_setup, &
              snowratio_diagnostics_compute, &

    private

    character(len=StrKIND), pointer :: snowratio_scheme

    contains


    !-----------------------------------------------------------------------
    !  routine snowratio_diagnostics_setup
    !
    !> \brief Set-up the snow ratio diagnostics module
    !> \author Brett Wilt
    !> \date   14 April 2017
    !> \details
    !>  To avoid later work in dereferencing pointers to various pools,
    !>  this routine saves pool pointers for use by
    !>  the snowratio_diagnostics_compute routine.
    !
    !-----------------------------------------------------------------------
    subroutine snowratio_diagnostics_setup(configs, all_pools)

        use mpas_derived_types, only : MPAS_pool_type
        use mpas_pool_routines, only : mpas_pool_get_subpool, mpas_pool_get_config      

        implicit none

        type (MPAS_pool_type), pointer :: configs
        type (MPAS_pool_type), pointer :: all_pools

        call mpas_pool_get_subpool(all_pools, 'mesh', mesh)
        call mpas_pool_get_subpool(all_pools, 'state', state)
        call mpas_pool_get_subpool(all_pools, 'diag', diag)
        call mpas_pool_get_subpool(all_pools, 'diag_physics', diag_physics)
        call mpas_pool_get_config(configs, 'config_snowratio_scheme', snowratio_scheme)

    end subroutine snowratio_diagnostics_setup


    !------------------------------------------------------------------------------------
    !  routine snowratio_diagnostics_compute
    !
    !> \brief Computes snow ratio
    !> \author Brett Wilt
    !> \date   14 April 2017
    !> \details
    !>  This routine computes snow ratio using kuchera, cobb, noahsp, noahmp, static schemes
    !>    snow_ratio
    !
    !------------------------------------------------------------------------------------
    subroutine snowratio_diagnostics_compute()

        use mpas_constants, only : rvord
        use mpas_pool_routines, only : mpas_pool_get_dimension, mpas_pool_get_array

        implicit none

        integer :: iCell, k, kk, k_cloud_base, k_cloud_top, snowratio_scheme_count
        integer, pointer :: nCells, nVertLevels, index_qv

        real (kind=RKIND) :: pressure, w_layer, w_max, temperature_max
        real (kind=RKIND) :: temperature_top, temperature_bot, temperature
        real (kind=RKIND) :: cobb_index, cobb_interp, cobb_srk, snow_ratio_tmp
        real (kind=RKIND) :: dz_layer, weighting_factor, weighting_factor_sum
        real (kind=RKIND), dimension(:), pointer :: snow_ratio
        real (kind=RKIND), dimension(:,:), pointer :: z
        real (kind=RKIND), dimension(:,:), pointer :: theta_m
        real (kind=RKIND), dimension(:,:,:), pointer :: scalars
        real (kind=RKIND), dimension(:,:), pointer :: exner
        real (kind=RKIND), dimension(:,:), pointer :: pressure_b
        real (kind=RKIND), dimension(:,:), pointer :: pressure_p
        real (kind=RKIND), dimension(:,:), pointer :: relhum
        real (kind=RKIND), dimension(:), pointer :: snowncv
        real (kind=RKIND), dimension(:), pointer :: graupelncv
        real (kind=RKIND), dimension(:,:), pointer :: w
        real (kind=RKIND), dimension(:), allocatable :: cobb_table
        real (kind=RKIND), dimension(:), allocatable :: snow_ratio_total

        real (kind=RKIND), parameter :: snow_ratio_default = 1.
        real (kind=RKIND), parameter :: snow_ratio_static = 10.
        real (kind=RKIND), parameter :: snow_ratio_min = 6.
        real (kind=RKIND), parameter :: snow_ratio_max = 18.
        real (kind=RKIND), parameter :: rh_cloud = 70.

        call mpas_pool_get_dimension(mesh, 'nCells', nCells)
        call mpas_pool_get_dimension(state, 'index_qv', index_qv)
        call mpas_pool_get_array(state, 'theta_m', theta_m, 1)
        call mpas_pool_get_array(state, 'scalars', scalars, 1)
        call mpas_pool_get_array(diag, 'exner', exner)
        call mpas_pool_get_array(diag_physics, 'snowncv', snowncv)
        call mpas_pool_get_array(diag_physics, 'graupelncv', graupelncv)
        call mpas_pool_get_array(diag, 'snow_ratio', snow_ratio)

        allocate(snow_ratio_total(nCells))
        snow_ratio_total(:) = 0.


        ! static snow ratio
        if (index(snowratio_scheme, "static") /= 0) then

           do iCell = 1, nCells
              snow_ratio_tmp = snow_ratio_default
              if (snowncv(iCell)+graupelncv(iCell) > 0) then
                 snow_ratio_tmp = snow_ratio_static
              end if
              snow_ratio_total(iCell) = snow_ratio_total(iCell) + snow_ratio_tmp
           end do
           snowratio_scheme_count = snowratio_scheme_count + 1

        end if

 
        ! kuchera snow ratio       
        if (index(snowratio_scheme, "kuchera") /= 0) then

           call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
           call mpas_pool_get_array(diag, 'pressure_base', pressure_b)
           call mpas_pool_get_array(diag, 'pressure_p', pressure_p)

           do iCell = 1, nCells
              snow_ratio_tmp = snow_ratio_default
              if (snowncv(iCell)+graupelncv(iCell) > 0) then
                 do k = 1, nVertLevels-1
                    pressure = (pressure_p(k,iCell)+pressure_b(k,iCell))/100._RKIND
                    temperature_top = (theta_m(k+1,iCell)/(1._RKIND+rvord*scalars(index_qv,k+1,iCell)))*exner(k+1,iCell)
                    temperature_bot = (theta_m(k,iCell)/(1._RKIND+rvord*scalars(index_qv,k,iCell)))*exner(k,iCell)
                    temperature = 0.5*(temperature_top+temperature_bot)
                    if (k == 1) then
                       temperature_max = temperature
                    else if (pressure >= 500) then
                       temperature_max = max(temperature_max, temperature)
                    else
                       exit
                    end if
                 end do
                 if (temperature_max > 271.16) then
                    snow_ratio_tmp = 12.0+2.0*(271.16-temperature_max)
                 else
                    snow_ratio_tmp = 12.0+(271.16-temperature_max)
                 end if
              end if
              snow_ratio_tmp = max(snow_ratio_min, min(snow_ratio_max, snow_ratio_tmp))
              snow_ratio_total(iCell) = snow_ratio_total(iCell) + snow_ratio_tmp
           end do
           snowratio_scheme_count = snowratio_scheme_count + 1

        end if


        ! cobb snow ratio
        if (index(snowratio_scheme, "cobb") /= 0) then

           call mpas_pool_get_dimension(mesh, 'nVertLevels', nVertLevels)
           call mpas_pool_get_array(mesh, 'zgrid', z)
           call mpas_pool_get_array(diag, 'relhum', relhum)
           call mpas_pool_get_array(state, 'w', w, 1)

           ! modified cobb table
           allocate(cobb_table(31))
           cobb_table(1)=6.0
           cobb_table(2)=6.5
           cobb_table(3)=7.0
           cobb_table(4)=7.5
           cobb_table(5)=8.0
           cobb_table(6)=8.5
           cobb_table(7)=9.0
           cobb_table(8)=9.5
           cobb_table(9)=10.0
           cobb_table(10)=11.0
           cobb_table(11)=12.0
           cobb_table(12)=13.0
           cobb_table(13)=14.0
           cobb_table(14)=15.0
           cobb_table(15)=16.0
           cobb_table(16)=17.0
           cobb_table(17)=18.0
           cobb_table(18)=17.0
           cobb_table(19)=16.0
           cobb_table(20)=15.0
           cobb_table(21)=14.0
           cobb_table(22)=13.0
           cobb_table(23)=12.0
           cobb_table(24)=11.0
           cobb_table(25)=10.0
           cobb_table(26)=9.0
           cobb_table(27)=8.0
           cobb_table(28)=7.5
           cobb_table(29)=7.0
           cobb_table(30)=7.5
           cobb_table(31)=8.0

           do iCell = 1, nCells

              snow_ratio_tmp = snow_ratio_default
              if (snowncv(iCell)+graupelncv(iCell) > 0) then
                 snow_ratio_tmp = snow_ratio_static

                 ! find cloud base, cloud top, and maximum vertical motion
                 k_cloud_base = 0
                 k_cloud_top = 0
                 w_max = 0.
                 do k = 1, nVertLevels-1
                    w_layer = 0.5*(w(k+1,iCell)+w(k,iCell))
                    if (relhum(k,iCell) >= rh_cloud .and. w_layer .gt. 0) then
                       w_max = max(w_max, w_layer)
                       if (k_cloud_base == 0) k_cloud_base = k
                       k_cloud_top = k
                    end if
                 end do

                 if (w_max > 0) then
   
                    ! weighting factor sum
                    weighting_factor_sum = 0.
                    do k = k_cloud_base, k_cloud_top
                       w_layer = 0.5*(w(k+1,iCell)+w(k,iCell))
                       if (w_layer > 0) then
                          dz_layer = z(k+1,iCell)-z(k,iCell)
                          weighting_factor = w_layer*dz_layer
                         !weighting_factor = w_layer*(w_layer/w_max)**2*dz_layer
                          weighting_factor_sum = weighting_factor_sum + weighting_factor
                       end if
                    end do

                    ! compute cobb snow ratio
                    snow_ratio_tmp = 0.
                    do k = k_cloud_base, k_cloud_top
                       w_layer = 0.5*(w(k+1,iCell)+w(k,iCell))
                       if (w_layer > 0) then
                          ! temperature (C)
                          temperature = (theta_m(k,iCell)/(1._RKIND+rvord*scalars(index_qv,k,iCell)))*exner(k,iCell)-273.15
                          ! interpolate cobb snow ratio table using temperature
                          if (temperature >= 0) then
                             cobb_srk=cobb_table(1)
                          elseif (temperature <= -30) then
                             cobb_srk=cobb_table(31)
                          else
                             cobb_index = abs(temperature)+1.
                             cobb_interp = cobb_index-int(cobb_index)
                             cobb_srk = cobb_interp*cobb_table(int(cobb_index+1.))+(1.-cobb_interp)*cobb_table(int(cobb_index))
                          end if
                          ! weight layer cobb snow ratio by upward vertical motion contribution       
                          dz_layer = z(k+1,iCell)-z(k,iCell)
                          weighting_factor = w_layer*dz_layer
                         !weighting_factor = w_layer*(w_layer/w_max)**2*dz_layer
                          snow_ratio_tmp = snow_ratio_tmp+cobb_srk*weighting_factor/weighting_factor_sum
                       end if
                    end do
                 end if
              end if
              snow_ratio_tmp = max(snow_ratio_min, min(snow_ratio_max, snow_ratio_tmp))
              snow_ratio_total(iCell) = snow_ratio_total(iCell) + snow_ratio_tmp
           end do
           snowratio_scheme_count = snowratio_scheme_count + 1

           deallocate(cobb_table)

        end if


        ! noahsp snow ratio (gottlieb)
        if (index(snowratio_scheme, "noahsp") /= 0) then

           do iCell = 1, nCells
              snow_ratio_tmp = snow_ratio_default
              if (snowncv(iCell)+graupelncv(iCell) > 0) then
                 temperature_top = (theta_m(2,iCell)/(1._RKIND+rvord*scalars(index_qv,2,iCell)))*exner(2,iCell)
                 temperature_bot = (theta_m(1,iCell)/(1._RKIND+rvord*scalars(index_qv,1,iCell)))*exner(1,iCell)
                 temperature = 0.5*(temperature_top+temperature_bot)
                 if (temperature <= -15.) then
                    snow_ratio_tmp = 20.
                 else
                    snow_ratio_tmp = 1./(0.05+0.0017*(temperature+15.)**1.5)
                 end if
              end if
              snow_ratio_tmp = max(snow_ratio_min, min(snow_ratio_max, snow_ratio_tmp))
              snow_ratio_total(iCell) = snow_ratio_total(iCell) + snow_ratio_tmp
           end do
           snowratio_scheme_count = snowratio_scheme_count + 1

        end if


        ! noahmp snow ratio (hedstrom & pomeroy)
        if (index(snowratio_scheme, "noahmp") /= 0) then

           do iCell = 1, nCells
              snow_ratio_tmp = snow_ratio_default
              if (snowncv(iCell)+graupelncv(iCell) > 0) then
                 temperature_top = (theta_m(2,iCell)/(1._RKIND+rvord*scalars(index_qv,2,iCell)))*exner(2,iCell)
                 temperature_bot = (theta_m(1,iCell)/(1._RKIND+rvord*scalars(index_qv,1,iCell)))*exner(1,iCell)
                 temperature = 0.5*(temperature_top+temperature_bot)
                 snow_ratio_tmp = 1000./(67.92+51.25*exp(temperature/2.59))
              end if
              snow_ratio_tmp = max(snow_ratio_min, min(snow_ratio_max, snow_ratio_tmp))
              snow_ratio_total(iCell) = snow_ratio_total(iCell) + snow_ratio_tmp
           end do
           snowratio_scheme_count = snowratio_scheme_count + 1

        end if

        ! update snow ratio
        if (snowratio_scheme_count > 0) then
           snow_ratio(:) = snow_ratio_total(:) / snowratio_scheme_count
        else
           snow_ratio(:) = snow_ratio_static
        end if
        deallocate(snow_ratio_total)


    end subroutine snowratio_diagnostics_compute

end module snowratio_diagnostics
