! Copyright (c) 2016,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module mpas_atmphys_compute_diagnostics_bl
 use mpas_derived_types
 use mpas_kind_types
 use mpas_log
 use mpas_pool_routines,only: mpas_pool_get_array,     &
                              mpas_pool_get_dimension, &
                              mpas_pool_get_subpool
 use mpas_atm_diagnostics_utils,only: mpas_field_will_be_written
 use mpas_atmphys_constants,only: R_v,R_d

 implicit none
 private
 public:: need_atmphys_diagnostics_bl, &
          atmphys_compute_diagnostics_bl

 logical:: need_pblp_top,need_pblt_top,need_pblh_top
 logical:: need_qvbl_vavg,need_qcbl_vavg,need_qrbl_vavg,need_qibl_vavg,need_qsbl_vavg,need_qgbl_vavg


 contains


!=================================================================================================================
 subroutine atmphys_compute_diagnostics_bl(mesh,state,time_lev,diag,diag_physics,diag_physics_bl)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: state
 type(mpas_pool_type),intent(in):: diag
 type(mpas_pool_type),intent(in):: diag_physics
 integer,intent(in):: time_lev

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics_bl

!local variables, arrays, and pointers:
 integer:: iCell,k,kp
 integer,pointer:: nCellsSolve,nVertLevels
 integer,pointer:: index_qv,index_qc,index_qr,index_qi,index_qs,index_qg
 integer,dimension(:),pointer:: kpbl

 real(kind=RKIND),dimension(:),pointer    :: pblp_top,pblt_top,pblh_top
 real(kind=RKIND),dimension(:),pointer    :: qvbl_vavg,qcbl_vavg,qrbl_vavg, &
                                             qibl_vavg,qsbl_vavg,qgbl_vavg
 real(kind=RKIND),dimension(:,:),pointer  :: rho_zz,zgrid,zz
 real(kind=RKIND),dimension(:,:),pointer  :: theta_m
 real(kind=RKIND),dimension(:,:),pointer  :: pressure_b,pressure_p,exner
 real(kind=RKIND),dimension(:,:),pointer  :: qv,qc,qr,qi,qs,qg
 real(kind=RKIND),dimension(:,:,:),pointer:: scalars

 real(kind=RKIND),dimension(:,:),allocatable:: dz,press,rhoa,temp,zgridl

!-----------------------------------------------------------------------------------------------------------------
 call mpas_log_write(' ')
 call mpas_log_write('--- enter subroutine atmphys_compute_diagnostics_bl:')

 call mpas_pool_get_dimension(mesh,'nCellsSolve',nCellsSolve)
 call mpas_pool_get_dimension(mesh,'nVertLevels',nVertLevels)

 call mpas_pool_get_dimension(state,'index_qv',index_qv)
 call mpas_pool_get_dimension(state,'index_qc',index_qc)
 call mpas_pool_get_dimension(state,'index_qr',index_qr)
 call mpas_pool_get_dimension(state,'index_qi',index_qi)
 call mpas_pool_get_dimension(state,'index_qs',index_qs)
 call mpas_pool_get_dimension(state,'index_qg',index_qg)

 call mpas_pool_get_array(mesh,'zz'   ,zz   )
 call mpas_pool_get_array(mesh,'zgrid',zgrid)

 call mpas_pool_get_array(state,'rho_zz' ,rho_zz ,time_lev)
 call mpas_pool_get_array(state,'theta_m',theta_m,time_lev)
 call mpas_pool_get_array(state,'scalars',scalars,time_lev)
 qv => scalars(index_qv,:,:)
 qc => scalars(index_qc,:,:)
 qr => scalars(index_qr,:,:)
 qi => scalars(index_qi,:,:)
 qs => scalars(index_qs,:,:)
 qg => scalars(index_qg,:,:)

 call mpas_pool_get_array(diag,'pressure_base',pressure_b)
 call mpas_pool_get_array(diag,'pressure_p'   ,pressure_p)
 call mpas_pool_get_array(diag,'exner'        ,exner     )

 if(.not.allocated(dz)    ) allocate(dz(nVertLevels,nCellsSolve)    )
 if(.not.allocated(press) ) allocate(press(nVertLevels,nCellsSolve) )
 if(.not.allocated(rhoa)  ) allocate(rhoa(nVertLevels,nCellsSolve)  )
 if(.not.allocated(temp)  ) allocate(temp(nVertLevels,nCellsSolve)  )
 if(.not.allocated(zgridl)) allocate(zgridl(nVertLevels,nCellsSolve))
 do k = 1, nVertLevels
    do iCell = 1, nCellsSolve
       dz(k,iCell)     = zgrid(k+1,iCell)-zgrid(k,iCell)
       zgridl(k,iCell) = 0.5_RKIND*(zgrid(k+1,iCell)+zgrid(k,iCell))
       press(k,iCell)  = pressure_p(k,iCell)+pressure_b(k,iCell)
       temp(k,iCell)   = theta_m(k,iCell)*exner(k,iCell)/(1._RKIND+R_v/R_d*qv(k,iCell))
       rhoa(k,iCell)   = zz(k,iCell)*rho_zz(k,iCell)
       rhoa(k,iCell)   = rhoa(k,iCell)*(1._RKIND+qv(k,iCell))*dz(k,iCell)
    enddo
 enddo

!--- computes PBL diagnostics:
 call mpas_pool_get_array(diag_physics,'kpbl',kpbl)

 call mpas_pool_get_array(diag_physics_bl,'pblp_top',pblp_top)
 call mpas_pool_get_array(diag_physics_bl,'pblt_top',pblt_top)
 call mpas_pool_get_array(diag_physics_bl,'pblh_top',pblh_top)

 call mpas_pool_get_array(diag_physics_bl,'qv_bl_vavg',qvbl_vavg)
 call mpas_pool_get_array(diag_physics_bl,'qc_bl_vavg',qcbl_vavg)
 call mpas_pool_get_array(diag_physics_bl,'qr_bl_vavg',qrbl_vavg)
 call mpas_pool_get_array(diag_physics_bl,'qi_bl_vavg',qibl_vavg)
 call mpas_pool_get_array(diag_physics_bl,'qs_bl_vavg',qsbl_vavg)
 call mpas_pool_get_array(diag_physics_bl,'qg_bl_vavg',qgbl_vavg)

 if(need_pblp_top .or. need_pblt_top .or. need_pblh_top) then
    do iCell = 1, nCellsSolve
       pblp_top(iCell) = -999._RKIND
       pblt_top(iCell) = -999._RKIND
       pblh_top(iCell) = -999._RKIND

       kp = kpbl(iCell)
       if(kp.gt.0) then
          pblp_top(iCell) = press(kp,iCell)
          pblt_top(iCell) = temp(kp,iCell)
          pblh_top(iCell) = zgridl(kp,iCell)
       endif
    enddo
 endif

 if(need_qvbl_vavg .or. need_qcbl_vavg .or. need_qrbl_vavg .or. &
    need_qibl_vavg .or. need_qsbl_vavg .or. need_qgbl_vavg) then
    do iCell = 1, nCellsSolve
       qvbl_vavg(iCell) = 0._RKIND
       qcbl_vavg(iCell) = 0._RKIND
       qrbl_vavg(iCell) = 0._RKIND
       qibl_vavg(iCell) = 0._RKIND
       qsbl_vavg(iCell) = 0._RKIND
       qgbl_vavg(iCell) = 0._RKIND

       kp = kpbl(iCell)
       if(kp.gt.0) then
          do k = 1, kp
             qvbl_vavg(iCell) = qvbl_vavg(iCell) + qv(k,iCell)*rhoa(k,iCell)
             qcbl_vavg(iCell) = qcbl_vavg(iCell) + qc(k,iCell)*rhoa(k,iCell)
             qrbl_vavg(iCell) = qrbl_vavg(iCell) + qr(k,iCell)*rhoa(k,iCell)
             qibl_vavg(iCell) = qibl_vavg(iCell) + qi(k,iCell)*rhoa(k,iCell)
             qsbl_vavg(iCell) = qsbl_vavg(iCell) + qs(k,iCell)*rhoa(k,iCell)
             qgbl_vavg(iCell) = qgbl_vavg(iCell) + qg(k,iCell)*rhoa(k,iCell)
          enddo
       endif
    enddo
 endif

 if(allocated(dz)    ) deallocate(dz    )
 if(allocated(press) ) deallocate(press )
 if(allocated(rhoa)  ) deallocate(rhoa  )
 if(allocated(temp)  ) deallocate(temp  )
 if(allocated(zgridl)) deallocate(zgridl)

 call mpas_log_write('--- exit subroutine atmphys_compute_diagnostics_bl:')
 call mpas_log_write(' ')

 end subroutine atmphys_compute_diagnostics_bl

!=================================================================================================================
 function need_atmphys_diagnostics_bl() result(need_any_diags)
!=================================================================================================================

!local variables:
 logical:: need_any_diags

!-----------------------------------------------------------------------------------------------------------------
 need_any_diags = .false.

 need_pblp_top  = .false. ; need_pblt_top  = .false. ; need_pblh_top  = .false.
 need_qvbl_vavg = .false. ; need_qcbl_vavg = .false. ; need_qrbl_vavg = .false.
 need_qibl_vavg = .false. ; need_qsbl_vavg = .false. ; need_qgbl_vavg = .false.

 need_pblp_top  = mpas_field_will_be_written('pblp_top')
 need_any_diags = need_any_diags .or. need_pblp_top
 need_pblt_top  = mpas_field_will_be_written('pblt_top')
 need_any_diags = need_any_diags .or. need_pblt_top
 need_pblh_top  = mpas_field_will_be_written('pblh_top')
 need_any_diags = need_any_diags .or. need_pblh_top

 need_qvbl_vavg = mpas_field_will_be_written('qv_bl_vavg')
 need_any_diags = need_any_diags .or. need_qvbl_vavg
 need_qcbl_vavg = mpas_field_will_be_written('qc_bl_vavg')
 need_any_diags = need_any_diags .or. need_qcbl_vavg
 need_qrbl_vavg  = mpas_field_will_be_written('qr_bl_vavg')
 need_any_diags = need_any_diags .or. need_qrbl_vavg
 need_qibl_vavg  = mpas_field_will_be_written('qi_bl_vavg')
 need_any_diags = need_any_diags .or. need_qibl_vavg
 need_qsbl_vavg = mpas_field_will_be_written('qs_bl_vavg')
 need_any_diags = need_any_diags .or. need_qsbl_vavg
 need_qgbl_vavg = mpas_field_will_be_written('qg_bl_vavg')
 need_any_diags = need_any_diags .or. need_qgbl_vavg

 end function need_atmphys_diagnostics_bl

!=================================================================================================================
 end module mpas_atmphys_compute_diagnostics_bl
!=================================================================================================================






