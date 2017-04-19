! Copyright (c) 2016,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module module_reflectivity_diagnostics
 use mpas_kind_types
 use mpas_derived_types
 use mpas_pool_routines,only: mpas_pool_get_array,     &
                              mpas_pool_get_config,    &
                              mpas_pool_get_dimension, &
                              mpas_pool_get_subpool
 use mpas_atm_diagnostics_utils,only: mpas_field_will_be_written
 use module_lightning_flash_diagnostics,only: config_lightning_flash,        &
                                              diag_lightning,                &
                                              lightning_diagnostics_compute, &
                                              lightning_diagnostics_setup

!from physics and physics_wrf directories:
 use mpas_atmphys_constants,only: R_v,R_d,t00
 use mpas_atmphys_utilities,only: physics_error_fatal
 use module_mp_wsm6,only: refl10cm_wsm6
 use module_mp_thompson,only: calc_refl10cm

 implicit none
 private
 public:: reflectivity_diagnostics_setup,  &
          reflectivity_diagnostics_compute

 type(mpas_pool_type),pointer :: configs
 type(mpas_pool_type),pointer :: mesh
 type(mpas_pool_type),pointer :: state
 type(mpas_pool_type),pointer :: diag
 type(mpas_pool_type),pointer :: diag_physics
 type(mpas_clock_type),pointer:: clock

 character(len=StrKIND),pointer:: microp_scheme

!module_reflectivity_diagnostics compute the reflectivity diagnostics for the WSM6 and Thompson cloud microphysics
!schemes.
!Laura D. Fowler (laura@ucar.edu) / 2017-04-13. 


 contains


!-----------------------------------------------------------------------------------------------------------------
 subroutine reflectivity_diagnostics_setup(configs,all_pools,simulation_clock)

 type(MPAS_pool_type),pointer :: configs
 type(MPAS_pool_type),pointer :: all_pools
 type(MPAS_clock_type),pointer:: simulation_clock

!-----------------------------------------------------------------------------------------------------------------

 clock => simulation_clock

 call mpas_pool_get_subpool(all_pools,'mesh' ,mesh )
 call mpas_pool_get_subpool(all_pools,'state',state)
 call mpas_pool_get_subpool(all_pools,'diag' ,diag )
 call mpas_pool_get_subpool(all_pools,'diag_physics',diag_physics)

 call mpas_pool_get_config(configs,'config_microp_scheme',microp_scheme)

!added calculation of storm parameters for calculation of lightning flash rates:
 call mpas_pool_get_config(configs,'config_lightning_flash',config_lightning_flash)
 if(config_lightning_flash) then
    call lightning_diagnostics_setup(all_pools,simulation_clock)
 endif

 end subroutine reflectivity_diagnostics_setup

!-----------------------------------------------------------------------------------------------------------------
 subroutine reflectivity_diagnostics_compute()

!local variables:
 logical:: need_any_diags
 logical:: need_refl10cm_max
 logical:: need_refl10cm_1km
 logical:: need_refl10cm_1km_max

!-----------------------------------------------------------------------------------------------------------------

 need_any_diags        = .false.
 need_refl10cm_max     = .false.
 need_refl10cm_1km     = .false.
 need_refl10cm_1km_max = .false.

 need_refl10cm_max = mpas_field_will_be_written('refl10cm_max')
 need_any_diags = need_any_diags .or. need_refl10cm_max

 need_refl10cm_1km = mpas_field_will_be_written('refl10cm_1km')
 need_any_diags = need_any_diags .or. need_refl10cm_1km

 need_refl10cm_1km_max = mpas_field_will_be_written('refl10cm_1km_max')
 need_any_diags = need_any_diags .or. need_refl10cm_1km_max

 if(config_lightning_flash) then
    call reflectivity_diagnostics(mesh,state,1,diag,diag_physics,diag_lightning)
    call lightning_diagnostics_compute()
 elseif(.not.config_lightning_flash .and. need_any_diags) then
    call reflectivity_diagnostics(mesh,state,1,diag,diag_physics)
 endif

 end subroutine reflectivity_diagnostics_compute

!-----------------------------------------------------------------------------------------------------------------
 subroutine reflectivity_diagnostics(mesh,state,time_lev,diag,diag_physics,diag_lightning)
!-----------------------------------------------------------------------------------------------------------------

!input arguments:
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: state
 type(mpas_pool_type),intent(in),optional:: diag_lightning
 integer,intent(in):: time_lev

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag
 type(mpas_pool_type),intent(inout):: diag_physics

!local pointers and variables:
 integer,pointer:: nCells,nVertLevels
 integer,pointer:: index_qv,index_qc,index_qr,index_qs,index_qg,index_nr
 integer:: iCell,k,kts,kte,kp,j

 real(kind=RKIND),dimension(:),pointer    :: refl10cm_max,refl10cm_1km,refl10cm_1km_max
 real(kind=RKIND),dimension(:,:),pointer  :: refl10cm

 real(kind=RKIND),dimension(:,:),pointer  :: zgrid 
 real(kind=RKIND),dimension(:,:),pointer  :: pressure_b,pressure_p,exner,theta_m
 real(kind=RKIND),dimension(:,:),pointer  :: qv,qc,qr,qs,qg,nr
 real(kind=RKIND),dimension(:,:,:),pointer:: scalars
 
 real(kind=RKIND):: dz,w1,w2
 real(kind=RKIND),dimension(:),allocatable:: qv1d,qc1d,qr1d,qs1d,qg1d,t1d,p1d,nr1d,dBZ1d,zp

!-----------------------------------------------------------------------------------------------------------------
 write(0,*)
 write(0,*) '--- enter subroutine reflectivity_diagnostics: lightning_flash = ',config_lightning_flash

 call mpas_pool_get_dimension(mesh,'nCells'     ,nCells     )
 call mpas_pool_get_dimension(mesh,'nVertLevels',nVertLevels)

 call mpas_pool_get_array(mesh,'zgrid',zgrid)

 call mpas_pool_get_dimension(state,'index_qv',index_qv)
 call mpas_pool_get_dimension(state,'index_qr',index_qr)
 call mpas_pool_get_dimension(state,'index_qs',index_qs)
 call mpas_pool_get_dimension(state,'index_qg',index_qg)

 call mpas_pool_get_array(state,'theta_m',theta_m,time_lev)
 call mpas_pool_get_array(state,'scalars',scalars,time_lev)
 qv => scalars(index_qv,:,:)
 qr => scalars(index_qr,:,:)
 qs => scalars(index_qs,:,:)
 qg => scalars(index_qg,:,:)

 call mpas_pool_get_array(diag,'pressure_p'      ,pressure_p)
 call mpas_pool_get_array(diag,'pressure_base'   ,pressure_b)
 call mpas_pool_get_array(diag,'exner'           ,exner     )

 call mpas_pool_get_array(diag_physics,'refl10cm_max',refl10cm_max)
 call mpas_pool_get_array(diag_physics,'refl10cm_1km',refl10cm_1km)
 call mpas_pool_get_array(diag_physics,'refl10cm_1km_max',refl10cm_1km_max)

 kts = 1
 kte = nVertLevels
 j   = 1

!--- allocate local arrays shared by the WSM6 and Thompson cloud microphysics:
 if(.not.allocated(p1d)  ) allocate(p1d(kts:kte)  )
 if(.not.allocated(t1d)  ) allocate(t1d(kts:kte)  )
 if(.not.allocated(qv1d) ) allocate(qv1d(kts:kte) )
 if(.not.allocated(qr1d) ) allocate(qr1d(kts:kte) )
 if(.not.allocated(qs1d) ) allocate(qs1d(kts:kte) )
 if(.not.allocated(qg1d) ) allocate(qg1d(kts:kte) )
 if(.not.allocated(dBz1d)) allocate(dBZ1d(kts:kte))
 if(.not.allocated(zp)   ) allocate(zp(kts:kte)   )

 microp_select: select case(microp_scheme)

    case ("mp_kessler")
       call physics_error_fatal('--- calculation of radar reflectivity is not available' // &
                                 'with kessler cloud microphysics')

    case ("mp_wsm6")
       do iCell = 1, nCells
          dz = zgrid(2,iCell)-zgrid(1,iCell)
          do k = kts,kte
             p1d(k)   = pressure_p(k,iCell) + pressure_b(k,iCell)
             t1d(k)   = theta_m(k,iCell)*exner(k,iCell)/(1._RKIND + R_v/R_d*qv(k,iCell))             
             qv1d(k)  = qv(k,iCell)
             qr1d(k)  = qr(k,iCell)
             qs1d(k)  = qs(k,iCell)
             qg1d(k)  = qg(k,iCell)
             dBZ1d(k) = -35._RKIND
             zp(k) = zgrid(k,iCell)-zgrid(1,iCell)+0.5*dz ! height AGL
          enddo

          call refl10cm_wsm6(qv1d,qr1d,qs1d,qg1d,t1d,p1d,dBZ1d,kts,kte,iCell,j)

          kp = 1
          do k = kts,kte
             dBZ1d(k) = max(-35._RKIND,dBZ1d(k))
             if(zp(k) .lt. 1000.) kp = k
          enddo
          refl10cm_max(iCell) = maxval(dBZ1d(:))
          w1 = (zp(kp+1)-1000.)/(zp(kp+1)-zp(kp))
          w2 = 1.0 - w1
          refl10cm_1km(iCell) = w1*dBZ1d(kp) + w2*dBZ1d(kp+1)
          refl10cm_1km_max(iCell) = max(refl10cm_1km_max(iCell),refl10cm_1km(iCell))

          if(present(diag_lightning)) then
             call mpas_pool_get_array(diag_lightning,'refl10cm',refl10cm)
             if(config_lightning_flash) then
                do k = kts,kte
                   refl10cm(k,iCell) = dbZ1d(k)
                enddo
             else
                do k = kts,kte
                   refl10cm(k,iCell) = 0._RKIND
                enddo
             endif
          endif
       enddo

    case ("mp_thompson")
       call mpas_pool_get_dimension(state,'index_qc',index_qc)
       call mpas_pool_get_dimension(state,'index_nr',index_nr)
       qc => scalars(index_qc,:,:)
       nr => scalars(index_nr,:,:)
       if(.not.allocated(qc1d)) allocate(qc1d(kts:kte))
       if(.not.allocated(nr1d)) allocate(nr1d(kts:kte))

       do iCell = 1, nCells
          dz = zgrid(2,iCell)-zgrid(1,iCell)
          do k = kts,kte
             p1d(k)   = pressure_p(k,iCell) + pressure_b(k,iCell)
             t1d(k)   = theta_m(k,iCell)*exner(k,iCell)/(1._RKIND + R_v/R_d*qv(k,iCell))             
             qv1d(k)  = qv(k,iCell)
             qc1d(k)  = qc(k,iCell)
             qr1d(k)  = qr(k,iCell)
             qs1d(k)  = qs(k,iCell)
             qg1d(k)  = qg(k,iCell)
             nr1d(k)  = nr(k,iCell)
             dBZ1d(k) = -35._RKIND
             zp(k) = zgrid(k,iCell)-zgrid(1,iCell)+0.5*dz ! height AGL
          enddo

          call calc_refl10cm(qv1d,qc1d,qr1d,nr1d,qs1d,qg1d,t1d,p1d,dBZ1d,kts,kte,iCell,j)

          kp = 1
          do k = kts,kte
             dBZ1d(k) = max(-35._RKIND,dBZ1d(k))
             if(zp(k) .lt. 1000.) kp = k
          enddo
          refl10cm_max(iCell) = maxval(dBZ1d(:))
          w1 = (zp(kp+1)-1000.)/(zp(kp+1)-zp(kp))
          w2 = 1.0 - w1
          refl10cm_1km(iCell) = w1*dBZ1d(kp) + w2*dBZ1d(kp+1)
          refl10cm_1km_max(iCell) = max(refl10cm_1km_max(iCell),refl10cm_1km(iCell))

          if(present(diag_lightning)) then
             call mpas_pool_get_array(diag_lightning,'refl10cm',refl10cm)
             if(config_lightning_flash) then
                do k = kts,kte
                   refl10cm(k,iCell) = dbZ1d(k)
                enddo
             else
                do k = kts,kte
                   refl10cm(k,iCell) = 0._RKIND
                enddo
             endif
          endif

       enddo

       if(allocated(qc1d)) deallocate(qc1d)
       if(allocated(nr1d)) deallocate(nr1d)

    case default

 end select microp_select

!--- deallocate local arrays shared by the WSM6 and Thompson cloud microphysics:
 if(allocated(p1d)  ) deallocate(p1d  )
 if(allocated(t1d)  ) deallocate(t1d  )
 if(allocated(qv1d) ) deallocate(qv1d )
 if(allocated(qr1d) ) deallocate(qr1d )
 if(allocated(qs1d) ) deallocate(qs1d )
 if(allocated(qg1d) ) deallocate(qg1d )
 if(allocated(dBz1d)) deallocate(dBZ1d)
 if(allocated(zp)   ) deallocate(zp   )

 end subroutine reflectivity_diagnostics

!=================================================================================================================
 end module module_reflectivity_diagnostics
!=================================================================================================================
