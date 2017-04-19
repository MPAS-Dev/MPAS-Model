! Copyright (c) 2016,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module module_lightning_flash_diagnostics
 use mpas_kind_types
 use mpas_derived_types
 use mpas_pool_routines,only: mpas_pool_get_array,     &
                              mpas_pool_get_config,    &
                              mpas_pool_get_dimension, &
                              mpas_pool_get_subpool
 use mpas_atm_diagnostics_utils,only: mpas_field_will_be_written

!from physics and physics_wrf directories:
 use mpas_atmphys_constants,only: R_v,R_d

 implicit none
 private
 public:: lightning_diagnostics_setup,   &
          lightning_diagnostics_compute

 logical,pointer,public:: config_lightning_flash
 type(mpas_pool_type),pointer,public :: diag_lightning

 type(mpas_pool_type),pointer :: configs
 type(mpas_pool_type),pointer :: mesh
 type(mpas_pool_type),pointer :: state
 type(mpas_pool_type),pointer :: diag
 type(mpas_pool_type),pointer :: diag_physics
 type(mpas_pool_type),pointer :: sfc_input
 type(mpas_clock_type),pointer:: clock

 logical:: need_pim_fr
 logical:: need_iwp_fr
 logical:: need_cldtoph_fr
 logical:: need_upvol_fr
 logical:: need_dbz35_fr
 logical:: need_max_vvel_fr
 logical:: need_npiceflx_fr


 contains


!-----------------------------------------------------------------------------------------------------------------
 subroutine lightning_diagnostics_setup(all_pools,simulation_clock)

 type(MPAS_pool_type),pointer :: all_pools
 type(MPAS_clock_type),pointer:: simulation_clock

!-----------------------------------------------------------------------------------------------------------------
 write(0,*)
 write(0,*) '--- enter subroutine lightning_diagnostics_setup:'

 clock => simulation_clock

 call mpas_pool_get_subpool(all_pools,'mesh' ,mesh )
 call mpas_pool_get_subpool(all_pools,'state',state)
 call mpas_pool_get_subpool(all_pools,'diag' ,diag )
 call mpas_pool_get_subpool(all_pools,'diag_physics'  ,diag_physics  )
 call mpas_pool_get_subpool(all_pools,'sfc_input'     ,sfc_input     )
 call mpas_pool_get_subpool(all_pools,'diag_lightning',diag_lightning)

 write(0,*) '--- end subroutine lightning_diagnostics_setup:'

 end subroutine lightning_diagnostics_setup

!-----------------------------------------------------------------------------------------------------------------
 subroutine lightning_diagnostics_compute()

!local variables:
 logical:: need_any_diags

!-----------------------------------------------------------------------------------------------------------------
!write(0,*)
 write(0,*) '--- enter subroutine lightning_diagnostics_compute:'

 need_any_diags   = .false.

 need_iwp_fr      = .false.
 need_cldtoph_fr  = .false. 
 need_pim_fr      = .false.
 need_upvol_fr    = .false.
 need_dbz35_fr    = .false. 
 need_max_vvel_fr = .false.
 need_npiceflx_fr = .false.

 need_iwp_fr      = mpas_field_will_be_written('ice_path_flashrate')
 need_any_diags   = need_any_diags .or. need_iwp_fr
 need_pim_fr      = mpas_field_will_be_written('precip_ice_flashrate')
 need_any_diags   = need_any_diags .or. need_pim_fr
 need_cldtoph_fr  = mpas_field_will_be_written('cloud_toph_flashrate') 
 need_any_diags   = need_any_diags .or. need_cldtoph_fr
 need_upvol_fr    = mpas_field_will_be_written('updraft_vol_flashrate')
 need_any_diags   = need_any_diags .or. need_upvol_fr
 need_dbz35_fr    = mpas_field_will_be_written('dbz35_flashrate')
 need_any_diags   = need_any_diags .or. need_dbz35_fr
 need_max_vvel_fr = mpas_field_will_be_written('max_vvel_flashrate')
 need_any_diags   = need_any_diags .or. need_max_vvel_fr
 need_npiceflx_fr = mpas_field_will_be_written('npice_massflux_flashrate')
 need_any_diags   = need_any_diags .or. need_npiceflx_fr

 if(need_any_diags) &
    call lightning_diagnostics(mesh,state,1,diag,diag_physics,sfc_input,diag_lightning)

 write(0,*) '--- end subroutine lightning_diagnostics_compute:'
 write(0,*)

 end subroutine lightning_diagnostics_compute

!-----------------------------------------------------------------------------------------------------------------
 subroutine lightning_diagnostics(mesh,state,time_lev,diag,diag_physics,sfc_input,diag_lightning)
!-----------------------------------------------------------------------------------------------------------------

!input arguments:
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: state
 type(mpas_pool_type),intent(in):: diag
 type(mpas_pool_type),intent(in):: diag_physics
 type(mpas_pool_type),intent(in):: sfc_input
 integer,intent(in):: time_lev

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_lightning

!local pointers and variables:
 integer,pointer:: nCells,nVertLevels
 integer,pointer:: index_qv,index_qr,index_qi,index_qs,index_qg

 integer:: iCell,k,k440

 real(kind=RKIND),dimension(:),pointer    :: area
 real(kind=RKIND),dimension(:),pointer    :: xland
 real(kind=RKIND),dimension(:,:),pointer  :: zgrid,zz
 real(kind=RKIND),dimension(:,:),pointer  :: rho_zz,theta_m,vvel
 real(kind=RKIND),dimension(:,:),pointer  :: pressure_b,pressure_p,exner
 real(kind=RKIND),dimension(:,:),pointer  :: qv,qr,qi,qs,qg
 real(kind=RKIND),dimension(:,:),pointer  :: vqr,vqi,vqs,vqg
 real(kind=RKIND),dimension(:,:,:),pointer:: scalars

 real(kind=RKIND),dimension(:),pointer  :: iwp,swp,gwp,iwp_fr
 real(kind=RKIND),dimension(:),pointer  :: pim_imass,pim_smass,pim_gmass,pim_fr
 real(kind=RKIND),dimension(:),pointer  :: cld_toph,cldtoph_fr 
 real(kind=RKIND),dimension(:),pointer  :: updraft_vol,upvol_fr
 real(kind=RKIND),dimension(:),pointer  :: dbz35_vol,dbz35_fr
 real(kind=RKIND),dimension(:),pointer  :: max_vvel,max_vvel_fr
 real(kind=RKIND),dimension(:),pointer  :: npice_flx,npiceflx_fr
 real(kind=RKIND),dimension(:,:),pointer:: refl10cm

 real(kind=RKIND):: dz,rho_a,twp,frate
 real(kind=RKIND),dimension(:),allocatable  :: dp
 real(kind=RKIND),dimension(:,:),allocatable:: tp,pres

!-----------------------------------------------------------------------------------------------------------------
 write(0,*) '--- enter subroutine lightning_diagnostics:'

 call mpas_pool_get_dimension(mesh,'nCells'     ,nCells     )
 call mpas_pool_get_dimension(mesh,'nVertLevels',nVertLevels)

 call mpas_pool_get_dimension(state,'index_qv',index_qv)
 call mpas_pool_get_dimension(state,'index_qr',index_qr)
 call mpas_pool_get_dimension(state,'index_qi',index_qi)
 call mpas_pool_get_dimension(state,'index_qs',index_qs)
 call mpas_pool_get_dimension(state,'index_qg',index_qg)

 call mpas_pool_get_array(mesh,'areaCell',area )
 call mpas_pool_get_array(mesh,'zgrid'   ,zgrid)
 call mpas_pool_get_array(mesh,'zz'      ,zz   )

 call mpas_pool_get_array(state,'theta_m',theta_m,time_lev)
 call mpas_pool_get_array(state,'w'      ,vvel   ,time_lev)
 call mpas_pool_get_array(state,'rho_zz' ,rho_zz ,time_lev)
 call mpas_pool_get_array(state,'scalars',scalars,time_lev)
 qv => scalars(index_qv,:,:)
 qr => scalars(index_qr,:,:)
 qi => scalars(index_qi,:,:)
 qs => scalars(index_qs,:,:)
 qg => scalars(index_qg,:,:)

 call mpas_pool_get_array(diag,'exner',exner)
 call mpas_pool_get_array(diag,'pressure_base',pressure_b)
 call mpas_pool_get_array(diag,'pressure_p'   ,pressure_p)

 call mpas_pool_get_array(diag_lightning,'refl10cm',refl10cm)

!--- allocation of local arrays:
 if(.not.allocated(dp)  ) allocate(dp(1:nVertLevels))
 if(.not.allocated(pres)) allocate(pres(1:nVertLevels,1:nCells))
 if(.not.allocated(tp)  ) allocate(tp(1:nVertLevels,1:nCells)  )

!--- calculation of the temperature:
 do iCell = 1, nCells
    do k = 1, nVertLevels
       pres(k,iCell) = (pressure_b(k,iCell)+pressure_p(k,iCell))/100._RKIND
       tp(k,iCell)   = theta_m(k,iCell)*exner(k,iCell)/(1._RKIND + R_v/R_d*qv(k,iCell))
    enddo
 enddo


!--- calculation of the ice path flash rate:
 if(need_iwp_fr) then
    write(0,*)
    write(0,*) '--- ice path flash rate:'
    call mpas_pool_get_array(diag_lightning,'ice_water_path',iwp)
    call mpas_pool_get_array(diag_lightning,'snow_path'     ,swp)
    call mpas_pool_get_array(diag_lightning,'graupel_path'  ,gwp)
    call mpas_pool_get_array(diag_lightning,'ice_path_flashrate',iwp_fr)

    do iCell = 1, nCells
       iwp(iCell) = 0._RKIND
       swp(iCell) = 0._RKIND
       gwp(iCell) = 0._RKIND
       iwp_fr(iCell) = 0._RKIND

       do k = 1, nVertLevels
          dz = zgrid(k+1,iCell) - zgrid(k,iCell)
          rho_a = zz(k,iCell)*rho_zz(k,iCell)

          if(tp(k,iCell) .lt. 263.15 .and. refl10cm(k,iCell) .gt. 25.) then
             iwp(iCell) = iwp(iCell) + qi(k,iCell)*rho_a*dz
             swp(iCell) = swp(iCell) + qs(k,iCell)*rho_a*dz
             gwp(iCell) = gwp(iCell) + qg(k,iCell)*rho_a*dz
          endif
       enddo

       twp   = iwp(iCell)+swp(iCell)+gwp(iCell)
       frate = 33.33*twp - 0.17
       iwp_fr(iCell) = max(frate,0._RKIND)
       iwp_fr(iCell) = iwp_fr(iCell)/1440.
    enddo
    write(0,*) '--- end ice path flash rate'
 endif


!--- calculation of the precipitation ice mass, snow mass, and graupel mass flahs rate:
 if(need_pim_fr) then
    write(0,*)
    write(0,*) '--- precipitation ice mass flash rate:'
    call mpas_pool_get_array(diag_physics,'vqi',vqi)
    call mpas_pool_get_array(diag_physics,'vqs',vqs)
    call mpas_pool_get_array(diag_physics,'vqg',vqg)

    call mpas_pool_get_array(diag_lightning,'precip_ice_mass'     ,pim_imass)
    call mpas_pool_get_array(diag_lightning,'precip_snow_mass'    ,pim_smass)
    call mpas_pool_get_array(diag_lightning,'precip_graupel_mass' ,pim_gmass)
    call mpas_pool_get_array(diag_lightning,'precip_ice_flashrate',pim_fr   )

    do iCell = 1, nCells
       pim_imass(iCell) = 0._RKIND
       pim_smass(iCell) = 0._RKIND
       pim_gmass(iCell) = 0._RKIND
       pim_fr(iCell)    = 0._RKIND

       do k = 1, nVertLevels
          dz = (zgrid(k+1,iCell) - zgrid(k,iCell))*area(iCell)
          rho_a = zz(k,iCell)*rho_zz(k,iCell)

          if(tp(k,iCell) .lt. 268.15) then
             pim_imass(iCell) = pim_imass(iCell) + qi(k,iCell)*rho_a*dz
             if(qs(k,iCell).gt.0.00025 .and. vvel(k,iCell).lt.vqs(k,iCell)) &
                pim_smass(iCell) = pim_smass(iCell) + qs(k,iCell)*rho_a*dz
             if(qg(k,iCell) .gt. 0.00050) &
                pim_gmass(iCell) = pim_gmass(iCell) + qg(k,iCell)*rho_a*dz          
          endif
       enddo

       twp = pim_imass(iCell)+pim_smass(iCell)+pim_gmass(iCell)
       frate = twp*3.4e-08 - 18.1
       pim_fr(iCell) = max(frate,0._RKIND)
    enddo
    write(0,*) '--- end precipitation ice mass flash rate'
 endif


!--- calculation of the cloud top height flash rate:
 if(need_cldtoph_fr) then
    write(0,*)
    write(0,*) '--- cloud top height flash rate:'
    call mpas_pool_get_array(sfc_input,'xland',xland)
    call mpas_pool_get_array(diag_lightning,'cloud_top_height',cld_toph)
    call mpas_pool_get_array(diag_lightning,'cloud_toph_flashrate',cldtoph_fr)
    do iCell = 1, nCells
       cld_toph(iCell)   = 0._RKIND
       cldtoph_fr(iCell) = 0._RKIND

       do k = 1, nVertLevels
          if(tp(k,iCell) .lt. 273.15 .and. refl10cm(k,iCell) .gt. 25.) then
             dz = zgrid(k,iCell) - zgrid(1,iCell)
             if(dz .gt. cld_toph(iCell)) cld_toph(iCell) = dz
          endif
       enddo
       if(xland(iCell) < 1.5_RKIND) then
          cldtoph_fr(iCell) = 3.44e-05*(cld_toph(iCell)**4.9) 
       elseif(xland(iCell) >= 2._RKIND) then
          cldtoph_fr(iCell) = 6.57e-06*(cld_toph(iCell)**4.9)
       endif
    enddo
    write(0,*) '--- end cloud top height flash rate'
 endif


!--- calculation of the updraft volume flash rate:
 if(need_upvol_fr) then
    write(0,*)
    write(0,*) '--- updraft volume flash rate:'
    call mpas_pool_get_array(diag_lightning,'updraft_volume',updraft_vol)
    call mpas_pool_get_array(diag_lightning,'updraft_vol_flashrate',upvol_fr)

    do iCell = 1, nCells
       updraft_vol(iCell) = 0._RKIND
       upvol_fr(iCell)    = 0._RKIND

       do k = 1, nVertLevels
          if(tp(k,iCell).lt.263.15 .and. tp(k,iCell).gt.233.15) then
             if(vvel(k,iCell) .gt. 5.) then
                dz = (zgrid(k+1,iCell) - zgrid(k,iCell))*area(iCell)
                updraft_vol(iCell) = updraft_vol(iCell) + dz
             endif
          endif
       enddo

       upvol_fr(iCell) = 0.0337e-09*updraft_vol(iCell)
       upvol_fr(iCell) = max(upvol_fr(iCell),0._RKIND)
    enddo
    write(0,*) '--- end updraft volume flash rate'
 endif


!--- calculation of the maximum vertical velocity flash rate:
 if(need_max_vvel_fr) then
    write(0,*)
    write(0,*) '--- maximum vertical velocity flash rate:'
    call mpas_pool_get_array(sfc_input,'xland',xland)
    call mpas_pool_get_array(diag_lightning,'max_vvel',max_vvel)
    call mpas_pool_get_array(diag_lightning,'max_vvel_flashrate',max_vvel_fr)

    do iCell = 1, nCells
       max_vvel(iCell) = 0._RKIND
       max_vvel(iCell) = maxval(vvel(1:nVertLevels,iCell))
       max_vvel(iCell) = max(max_vvel(iCell),0._RKIND)

       if(xland(iCell) < 1.5_RKIND) then
          max_vvel_fr(iCell) = 5.005e-06*(max_vvel(iCell)**4.55)
       elseif(xland(iCell) >= 2._RKIND) then
          max_vvel_fr(iCell) = 6.4e-04*(max_vvel(iCell)**1.73)
       endif
    enddo
    write(0,*) '--- end maximum vertical velocity flash rate'
 endif


!--- calculation of the volume of 35dBZ 10cm reflectivity flash rate:
 if(need_dbz35_fr) then 
    write(0,*)
    write(0,*) '--- 35dBZ 10cm reflectivity flash rate:'
    call mpas_pool_get_array(diag_lightning,'dbz35_volume',dbz35_vol)
    call mpas_pool_get_array(diag_lightning,'dbz35_flashrate',dbz35_fr)

    do iCell = 1, nCells
       dbz35_vol(iCell) = 0._RKIND
       dbz35_fr(iCell)  = 0._RKIND

       do k = 1, nVertLevels
          if(tp(k,iCell).lt.268.15 .and. tp(k,iCell).gt.233.15) then
             if(refl10cm(k,iCell) .gt. 35.) then
                dz = (zgrid(k+1,iCell) - zgrid(k,iCell))*area(iCell)
                dbz35_vol(iCell) = dbz35_vol(iCell) + dz*1.e-09
             endif
          endif
       enddo

       dbz35_fr(iCell) = 7.2e-02*dbz35_vol(iCell)
    enddo
    write(0,*) '--- end 35dBZ 10cm reflectivity flash rate'
 endif


!--- calculation of the precipitating and non-precipitating ice massflux flash rate:
 if(need_npiceflx_fr) then
    write(0,*)
    write(0,*) '--- non-precipitating ice massflux flash rate:'
    call mpas_pool_get_array(mesh,'areaCell',area)
    call mpas_pool_get_array(diag_physics,'vqi',vqi)
    call mpas_pool_get_array(diag_physics,'vqs',vqs)
    call mpas_pool_get_array(diag_physics,'vqg',vqg)

    call mpas_pool_get_array(diag_lightning,'npice_massflux',npice_flx)
    call mpas_pool_get_array(diag_lightning,'npice_massflux_flashrate',npiceflx_fr)

    do iCell = 1, nCells
       npice_flx(iCell)   = 0._RKIND
       npiceflx_fr(iCell) = 0._RKIND

       !--- look for the pressure level closest to 400 hPa:
       k440 = 1
       do k = 1, nVertLevels
          dp(k) = abs(440._RKIND-pres(k,iCell))
       enddo
       k440 = minloc(dp,1)

       !--- non precipitating snow massflux:
       if(vvel(k440,iCell).gt.vqs(k440,iCell) .and. qs(k440,iCell).gt.0.00025) then
         rho_a = zz(k440,iCell)*rho_zz(k440,iCell)
         npice_flx(iCell) = npice_flx(iCell) + qs(k440,iCell)*rho_a*(vvel(k440,iCell)-vqs(k440,iCell))
       endif

       !--- non precipitating ice massflux:
       if(vvel(k440,iCell).gt.0. .and. qs(k440,iCell).gt.0.) then
          rho_a = zz(k440,iCell)*rho_zz(k440,iCell)
          npice_flx(iCell) = npice_flx(iCell) + qi(k440,iCell)*rho_a*vvel(k440,iCell)
       endif

       !--- flash rate:
       npiceflx_fr(iCell) = npice_flx(iCell)*6.58e-7*area(iCell)
    enddo
    write(0,*) '--- end non-precipitating ice massflux flash rate'
 endif
 202 format(2i7,5(1x,e15.8))


!--- deallocation of local arrays:
 if(allocated(dp)  ) deallocate(dp  )
 if(allocated(pres)) deallocate(pres)
 if(allocated(tp)  ) deallocate(tp  )

 write(0,*) '--- end subroutine lightning_diagnostics:'

 end subroutine lightning_diagnostics

!=================================================================================================================
 end module module_lightning_flash_diagnostics
!=================================================================================================================
