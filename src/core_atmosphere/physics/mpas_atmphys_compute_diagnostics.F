! Copyright (c) 2016,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module mpas_atmphys_compute_diagnostics
 use mpas_derived_types
 use mpas_kind_types
 use mpas_log
 use mpas_pool_routines,only: mpas_pool_get_array,     &
                              mpas_pool_get_dimension, &
                              mpas_pool_get_subpool
 use mpas_atm_diagnostics_utils,only: mpas_field_will_be_written
 use mpas_atmphys_constants,only: R_v,R_d
 use mpas_atmphys_functions,only: rsif,rslf

 implicit none
 private
 public:: atmphys_compute_diagnostics_setup, &
          atmphys_compute_diagnostics

 type(mpas_clock_type),pointer:: clock

 type(mpas_pool_type),pointer:: mesh
 type(mpas_pool_type),pointer:: state
 type(mpas_pool_type),pointer:: fg
 type(mpas_pool_type),pointer:: diag
 type(mpas_pool_type),pointer:: diag_physics
 type(mpas_pool_type),pointer:: diag_physics_vavg
 type(mpas_pool_type),pointer:: diag_physics_interp
 type(mpas_pool_type),pointer:: tend_physics
 type(mpas_pool_type),pointer:: tend_physics_vavg
 type(mpas_pool_type),pointer:: tend_physics_interp

 logical:: need_qv_vavg,need_qc_vavg,need_qr_vavg,need_qi_vavg,need_qs_vavg,need_qg_vavg
 logical:: need_qcf_vavg,need_qrf_vavg,need_qif_vavg,need_qsf_vavg,need_qgf_vavg
 
 logical:: need_qv_interp,need_qc_interp,need_qr_interp,need_qi_interp,need_qs_interp,need_qg_interp
 logical:: need_qcf_interp,need_qrf_interp,need_qif_interp,need_qsf_interp,need_qgf_interp,need_cld_interp

 logical:: need_dtcup_vavg,need_dqvcup_vavg,need_dqccup_vavg,need_dqrcup_vavg, &
           need_dqicup_vavg,need_dqscup_vavg
 logical:: need_dtmp_vavg,need_dqvmp_vavg,need_dqcmp_vavg,need_dqrmp_vavg, &
           need_dqimp_vavg,need_dqsmp_vavg,need_dqgmp_vavg
 logical:: need_dtbl_vavg,need_dqvbl_vavg,need_dqcbl_vavg,need_dqibl_vavg
 logical:: need_dtlw_vavg,need_dtsw_vavg

 logical:: need_dtcup_interp,need_dqvcup_interp,need_dqccup_interp,need_dqrcup_interp, &
           need_dqicup_interp,need_dqscup_interp
 logical:: need_dtmp_interp,need_dqvmp_interp,need_dqcmp_interp,need_dqrmp_interp, &
           need_dqimp_interp,need_dqsmp_interp,need_dqgmp_interp
 logical:: need_dtbl_interp,need_dqvbl_interp,need_dqcbl_interp,need_dqibl_interp
 logical:: need_dtlw_interp,need_dtsw_interp


! add-ons and modifications to sourcecode:
! ----------------------------------------
! * in subroutine atmphys_compute_tendencies_vavg, corrected the calculation of the verticall-averaged tendencies
!   of temperature (dtcup_vavg,dtmp_vavg,dtbl_vavg,dtlw_vavg,dtsw_vavg).
!   Laura D. Fowler (laura@ucar.edu) / 2018-05-29.
! * in subroutine atmphys_diagnostics_interp, added the relative humidity interpolated to fixed pressure
!   levels.
!   Laura D. Fowler (laura@ucar.edu) / 2018-06-01.


 contains


!=================================================================================================================
 subroutine atmphys_compute_diagnostics_setup(all_pools,simulation_clock)
!=================================================================================================================

 type(MPAS_pool_type),pointer :: all_pools
 type(MPAS_clock_type),pointer:: simulation_clock

!-----------------------------------------------------------------------------------------------------------------
 call mpas_log_write(' ')
 call mpas_log_write('--- enter subroutine atmphys_compute_diagnostics_setup:')

 clock => simulation_clock

 call mpas_pool_get_subpool(all_pools,'mesh' ,mesh )
 call mpas_pool_get_subpool(all_pools,'state',state)
 call mpas_pool_get_subpool(all_pools,'fg'   ,fg   )
 call mpas_pool_get_subpool(all_pools,'diag' ,diag )

 call mpas_pool_get_subpool(all_pools,'diag_physics',diag_physics)
 call mpas_pool_get_subpool(all_pools,'diag_physics_vavg'  ,diag_physics_vavg  )
 call mpas_pool_get_subpool(all_pools,'diag_physics_interp',diag_physics_interp)

 call mpas_pool_get_subpool(all_pools,'tend_physics',tend_physics)
 call mpas_pool_get_subpool(all_pools,'tend_physics_vavg'  ,tend_physics_vavg  )
 call mpas_pool_get_subpool(all_pools,'tend_physics_interp',tend_physics_interp)

 call mpas_log_write('--- exit subroutine atmphys_compute_diagnostics_setup:')

 end  subroutine atmphys_compute_diagnostics_setup

!=================================================================================================================
 subroutine atmphys_compute_diagnostics()
!=================================================================================================================
 call mpas_log_write(' ')
 call mpas_log_write('--- enter subroutine atmphys_diagnostics_compute:')

 if(need_atmphys_diagnostics_vavg()) &
    call atmphys_compute_diagnostics_vavg(mesh,state,1,diag_physics,diag_physics_vavg)

 if(need_atmphys_diagnostics_interp()) &
    call atmphys_diagnostics_interp(mesh,state,1,fg,diag,diag_physics,diag_physics_interp)

 if(need_atmphys_tendencies_vavg()) &
    call atmphys_compute_tendencies_vavg(mesh,state,1,diag,tend_physics,tend_physics_vavg)

 if(need_atmphys_tendencies_interp()) &
    call atmphys_tendencies_interp(mesh,fg,diag,tend_physics,tend_physics_interp)

 call mpas_log_write(' ')
 call mpas_log_write('--- exit subroutine atmphys_diagnostics_compute:')

 end subroutine atmphys_compute_diagnostics

!=================================================================================================================
 subroutine atmphys_compute_diagnostics_vavg(mesh,state,time_lev,diag_physics,diag_physics_vavg)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: state
 type(mpas_pool_type),intent(in):: diag_physics
 integer,intent(in):: time_lev

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics_vavg

!local variabl_interpes, arrays, and pointers:
 integer:: iCell,k
 integer,pointer:: nCellsSolve,nVertLevels
 integer,pointer:: index_qv,index_qc,index_qr,index_qi,index_qs,index_qg

 real:: dz,rhoa
 real(kind=RKIND),dimension(:),pointer    :: qv_vavg,qc_vavg,qr_vavg,qi_vavg,qs_vavg,qg_vavg
 real(kind=RKIND),dimension(:),pointer    :: qcf_vavg,qrf_vavg,qif_vavg,qsf_vavg,qgf_vavg
 real(kind=RKIND),dimension(:),pointer    :: cld_max
 real(kind=RKIND),dimension(:,:),pointer  :: rho_zz,zgrid,zz
 real(kind=RKIND),dimension(:,:),pointer  :: cld
 real(kind=RKIND),dimension(:,:),pointer  :: qv,qc,qr,qi,qs,qg
 real(kind=RKIND),dimension(:,:,:),pointer:: scalars

!-----------------------------------------------------------------------------------------------------------------
 call mpas_log_write(' ')
 call mpas_log_write('--- enter subroutine atmphys_compute_diagnostics_vavg:')

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
 call mpas_pool_get_array(state,'scalars',scalars,time_lev)
 qv => scalars(index_qv,:,:)
 qc => scalars(index_qc,:,:)
 qr => scalars(index_qr,:,:)
 qi => scalars(index_qi,:,:)
 qs => scalars(index_qs,:,:)
 qg => scalars(index_qg,:,:)


!--- calculation of vertically-averaged mixing ratios and maximum cloud fraction: 
 call mpas_pool_get_array(diag_physics,'cldfrac',cld)

 call mpas_pool_get_array(diag_physics_vavg,'cld_max',cld_max)
 call mpas_pool_get_array(diag_physics_vavg,'qv_vavg',qv_vavg)
 call mpas_pool_get_array(diag_physics_vavg,'qc_vavg',qc_vavg)
 call mpas_pool_get_array(diag_physics_vavg,'qr_vavg',qr_vavg)
 call mpas_pool_get_array(diag_physics_vavg,'qi_vavg',qi_vavg)
 call mpas_pool_get_array(diag_physics_vavg,'qs_vavg',qs_vavg)
 call mpas_pool_get_array(diag_physics_vavg,'qg_vavg',qg_vavg)

 call mpas_pool_get_array(diag_physics_vavg,'qcf_vavg',qcf_vavg)
 call mpas_pool_get_array(diag_physics_vavg,'qrf_vavg',qrf_vavg)
 call mpas_pool_get_array(diag_physics_vavg,'qif_vavg',qif_vavg)
 call mpas_pool_get_array(diag_physics_vavg,'qsf_vavg',qsf_vavg)
 call mpas_pool_get_array(diag_physics_vavg,'qgf_vavg',qgf_vavg)

 do iCell = 1, nCellsSolve
    cld_max(iCell)  = 0._RKIND
    qv_vavg(iCell)  = 0._RKIND
    qc_vavg(iCell)  = 0._RKIND
    qr_vavg(iCell)  = 0._RKIND
    qi_vavg(iCell)  = 0._RKIND
    qs_vavg(iCell)  = 0._RKIND
    qg_vavg(iCell)  = 0._RKIND
    qcf_vavg(iCell) = 0._RKIND
    qrf_vavg(iCell) = 0._RKIND
    qif_vavg(iCell) = 0._RKIND
    qsf_vavg(iCell) = 0._RKIND
    qgf_vavg(iCell) = 0._RKIND

    do k = 1, nVertLevels
       dz   = zgrid(k+1,iCell)-zgrid(k,iCell)
       rhoa = zz(k,iCell)*rho_zz(k,iCell)
       rhoa = rhoa*(1._RKIND+qv(k,iCell))

       cld_max(iCell) = max(cld_max(iCell),cld(k,iCell))

       !--- vertically-averaged mixing ratios:
       qv_vavg(iCell) = qv_vavg(iCell) + qv(k,iCell)*rhoa*dz
       qc_vavg(iCell) = qc_vavg(iCell) + qc(k,iCell)*rhoa*dz
       qr_vavg(iCell) = qr_vavg(iCell) + qr(k,iCell)*rhoa*dz
       qi_vavg(iCell) = qi_vavg(iCell) + qi(k,iCell)*rhoa*dz
       qs_vavg(iCell) = qs_vavg(iCell) + qs(k,iCell)*rhoa*dz
       qg_vavg(iCell) = qg_vavg(iCell) + qg(k,iCell)*rhoa*dz

       !--- vertically-averaged mixing ratios weighted by the cloud fraction:
       qcf_vavg(iCell) = qcf_vavg(iCell) + cld(k,iCell)*qc(k,iCell)*rhoa*dz
       qrf_vavg(iCell) = qrf_vavg(iCell) + cld(k,iCell)*qr(k,iCell)*rhoa*dz
       qif_vavg(iCell) = qif_vavg(iCell) + cld(k,iCell)*qi(k,iCell)*rhoa*dz
       qsf_vavg(iCell) = qsf_vavg(iCell) + cld(k,iCell)*qs(k,iCell)*rhoa*dz
       qgf_vavg(iCell) = qgf_vavg(iCell) + cld(k,iCell)*qg(k,iCell)*rhoa*dz
    enddo
 enddo


 call mpas_log_write('--- exit subroutine atmphys_compute_diagnostics_vavg:')

 end subroutine atmphys_compute_diagnostics_vavg

!=================================================================================================================
 subroutine atmphys_diagnostics_interp(mesh,state,time_lev,fg,diag,diag_physics,diag_physics_interp)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: state
 type(mpas_pool_type),intent(in):: fg
 type(mpas_pool_type),intent(in):: diag
 type(mpas_pool_type),intent(in):: diag_physics
 integer,intent(in):: time_lev

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics_interp

!local variabl_interpes, arrays, and pointers:
 integer:: iCell,k,kk,nIntP
 integer,pointer:: nCells,nCellsSolve,nFGLevels,nVertLevels
 integer,pointer:: index_qv,index_qc,index_qr,index_qi,index_qs,index_qg

 real(kind=RKIND),dimension(:,:),pointer  :: p_fg
 real(kind=RKIND),dimension(:,:),pointer  :: cld,theta_m
 real(kind=RKIND),dimension(:,:),pointer  :: qv,qc,qr,qi,qs,qg
 real(kind=RKIND),dimension(:,:),pointer  :: pressure_b,pressure_p,exner
 real(kind=RKIND),dimension(:,:),pointer  :: qv_interp,qc_interp,qr_interp,qi_interp,qs_interp,qg_interp
 real(kind=RKIND),dimension(:,:),pointer  :: qcf_interp,qrf_interp,qif_interp,qsf_interp,qgf_interp
 real(kind=RKIND),dimension(:,:),pointer  :: cld_interp,rh_interp,t_interp
 real(kind=RKIND),dimension(:,:,:),pointer:: scalars

!local interpolated fields:
 real(kind=RKIND),dimension(:,:),allocatable:: rh_in
 real(kind=RKIND),dimension(:,:),allocatable:: field_in,press_in
 real(kind=RKIND),dimension(:,:),allocatable:: field_interp,press_interp

!-----------------------------------------------------------------------------------------------------------------
 call mpas_log_write(' ')
 call mpas_log_write('--- enter subroutine atmphys_diagnostics_interp:')

 call mpas_pool_get_dimension(mesh,'nCells'     ,nCells)
 call mpas_pool_get_dimension(mesh,'nCellsSolve',nCellsSolve)
 call mpas_pool_get_dimension(mesh,'nFGLevels'  ,nFGLevels  )
 call mpas_pool_get_dimension(mesh,'nVertLevels',nVertLevels)

 call mpas_pool_get_dimension(state,'index_qv',index_qv)
 call mpas_pool_get_dimension(state,'index_qc',index_qc)
 call mpas_pool_get_dimension(state,'index_qr',index_qr)
 call mpas_pool_get_dimension(state,'index_qi',index_qi)
 call mpas_pool_get_dimension(state,'index_qs',index_qs)
 call mpas_pool_get_dimension(state,'index_qg',index_qg)

 call mpas_pool_get_array(state,'theta_m',theta_m,time_lev)
 call mpas_pool_get_array(state,'scalars',scalars,time_lev)
 qv => scalars(index_qv,:,:)
 qc => scalars(index_qc,:,:)
 qr => scalars(index_qr,:,:)
 qi => scalars(index_qi,:,:)
 qs => scalars(index_qs,:,:)
 qg => scalars(index_qg,:,:)

 call mpas_pool_get_array(fg,'p_fg',p_fg)
 call mpas_pool_get_array(diag,'pressure_base',pressure_b)
 call mpas_pool_get_array(diag,'pressure_p'   ,pressure_p)
 call mpas_pool_get_array(diag,'exner'        ,exner     )

 nullify(qv_interp)
 nullify(qc_interp)
 nullify(qr_interp)
 nullify(qi_interp)
 nullify(qs_interp)
 nullify(qg_interp)
 nullify(t_interp )
 nullify(rh_interp)
 call mpas_pool_get_array(diag_physics_interp,'qv_interp',qv_interp)
 call mpas_pool_get_array(diag_physics_interp,'qc_interp',qc_interp)
 call mpas_pool_get_array(diag_physics_interp,'qr_interp',qr_interp)
 call mpas_pool_get_array(diag_physics_interp,'qi_interp',qi_interp)
 call mpas_pool_get_array(diag_physics_interp,'qs_interp',qs_interp)
 call mpas_pool_get_array(diag_physics_interp,'qg_interp',qg_interp)
 call mpas_pool_get_array(diag_physics_interp,'t_interp' ,t_interp )
 call mpas_pool_get_array(diag_physics_interp,'rh_interp',rh_interp)

 nullify(qcf_interp)
 nullify(qrf_interp)
 nullify(qif_interp)
 nullify(qsf_interp)
 nullify(qgf_interp)
 nullify(cld_interp)
 call mpas_pool_get_array(diag_physics_interp,'qcf_interp',qcf_interp)
 call mpas_pool_get_array(diag_physics_interp,'qrf_interp',qrf_interp)
 call mpas_pool_get_array(diag_physics_interp,'qif_interp',qif_interp)
 call mpas_pool_get_array(diag_physics_interp,'qsf_interp',qsf_interp)
 call mpas_pool_get_array(diag_physics_interp,'qgf_interp',qgf_interp)
 call mpas_pool_get_array(diag_physics_interp,'cld_interp',cld_interp)

 call mpas_pool_get_array(diag_physics,'cldfrac',cld)


!--- define index of the 10 hPa layer; interpolate data to fixed pressure levels:
 nIntP = -1
 iCell = 1
 do k = 1, nFGLevels-1
    if(p_fg(k,iCell) .ge. 1000._RKIND) nIntP = k
 enddo
!call mpas_log_write('--- nIntP = $i',intArgs=(/nIntP/))

 if(.not.allocated(press_in)) allocate(press_in(nCellsSolve,nVertLevels))
 if(.not.allocated(field_in)) allocate(field_in(nCellsSolve,nVertLevels))
 do k = 1, nVertLevels
    kk = nVertLevels+1-k
    do iCell = 1, nCellsSolve
       press_in(iCell,kk) = pressure_p(k,iCell)+pressure_b(k,iCell)
    enddo
 enddo

 if(.not.allocated(field_interp)) allocate(field_interp(nCellsSolve,nIntP))
 if(.not.allocated(press_interp)) allocate(press_interp(nCellsSolve,nIntP))
 do k = 1, nIntP
    kk = nIntP+1-k
    do iCell = 1, nCellsSolve
       press_interp(iCell,kk) = p_fg(k,iCell)
    enddo
 enddo

!--- t_interp:
 do k = 1, nVertLevels
    kk = nVertLevels+1-k
    do iCell = 1, nCellsSolve
       field_in(iCell,kk) = theta_m(k,iCell)*exner(k,iCell)/(1._RKIND+R_v/R_d*qv(k,iCell))
    enddo
 enddo
 call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
 do k = 1, nIntP
    kk = nIntp+1-k
    do iCell = 1, nCellsSolve
       t_interp(k,iCell) = field_interp(iCell,kk)
    enddo
 enddo
!call mpas_log_write('--- end interpolate temperature:')

!--- rh_interp:
 if(.not.allocated(rh_in)) allocate(rh_in(nVertLevels,nCells+1))
 call compute_rh(nCells,nCellsSolve,nVertLevels,pressure_b,pressure_p,exner,theta_m,qv,rh_in)
 do k = 1, nVertLevels
    kk = nVertLevels+1-k
    do iCell = 1, nCellsSolve
       field_in(iCell,kk) = rh_in(k,iCell)
    enddo
 enddo
 call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
 do k = 1, nIntP
    kk = nIntp+1-k
    do iCell = 1, nCellsSolve
       rh_interp(k,iCell) = field_interp(iCell,kk)
    enddo
 enddo
 if(allocated(rh_in)) deallocate(rh_in)
!call mpas_log_write('--- end interpolate relative humidity:')

!--- qv_interp:
 do k = 1, nVertLevels
    kk = nVertLevels+1-k
    do iCell = 1, nCellsSolve
       field_in(iCell,kk) = qv(k,iCell)
    enddo
 enddo
 call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
 do k = 1, nIntP
    kk = nIntp+1-k
    do iCell = 1, nCellsSolve
       qv_interp(k,iCell) = field_interp(iCell,kk)
    enddo
 enddo
!call mpas_log_write('--- end interpolate qv:')

!--- qc_interp:
 do k = 1, nVertLevels
    kk = nVertLevels+1-k
    do iCell = 1, nCellsSolve
       field_in(iCell,kk) = qc(k,iCell)
    enddo
 enddo
 call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
 do k = 1, nIntP
    kk = nIntp+1-k
    do iCell = 1, nCellsSolve
       qc_interp(k,iCell) = field_interp(iCell,kk)
    enddo
 enddo
!call mpas_log_write('--- end interpolate qc:')

!--- qr_interp:
 do k = 1, nVertLevels
    kk = nVertLevels+1-k
    do iCell = 1, nCellsSolve
       field_in(iCell,kk) = qr(k,iCell)
    enddo
 enddo
 call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
 do k = 1, nIntP
    kk = nIntp+1-k
    do iCell = 1, nCellsSolve
       qr_interp(k,iCell) = field_interp(iCell,kk)
    enddo
 enddo
!call mpas_log_write('--- end interpolate qr:')

!--- qi_interp:
 do k = 1, nVertLevels
    kk = nVertLevels+1-k
    do iCell = 1, nCellsSolve
       field_in(iCell,kk) = qi(k,iCell)
    enddo
 enddo
 call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
 do k = 1, nIntP
    kk = nIntp+1-k
    do iCell = 1, nCellsSolve
       qi_interp(k,iCell) = field_interp(iCell,kk)
    enddo
 enddo
!call mpas_log_write('--- end interpolate qi:')

!--- qs_interp:
 do k = 1, nVertLevels
    kk = nVertLevels+1-k
    do iCell = 1, nCellsSolve
       field_in(iCell,kk) = qs(k,iCell)
    enddo
 enddo
 call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
 do k = 1, nIntP
    kk = nIntp+1-k
    do iCell = 1, nCellsSolve
       qs_interp(k,iCell) = field_interp(iCell,kk)
    enddo
 enddo
!call mpas_log_write('--- end interpolate qs:')

!--- qg_interp:
 do k = 1, nVertLevels
    kk = nVertLevels+1-k
    do iCell = 1, nCellsSolve
       field_in(iCell,kk) = qg(k,iCell)
    enddo
 enddo
 call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
 do k = 1, nIntP
    kk = nIntp+1-k
    do iCell = 1, nCellsSolve
       qg_interp(k,iCell) = field_interp(iCell,kk)
    enddo
 enddo
!call mpas_log_write('--- end interpolate qg:')


!--- qcf_interp:
 do k = 1, nVertLevels
    kk = nVertLevels+1-k
    do iCell = 1, nCellsSolve
       field_in(iCell,kk) = cld(k,iCell)*qc(k,iCell)
    enddo
 enddo
 call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
 do k = 1, nIntP
    kk = nIntp+1-k
    do iCell = 1, nCellsSolve
       qcf_interp(k,iCell) = field_interp(iCell,kk)
    enddo
 enddo
!call mpas_log_write('--- end interpolate qcf:')

!--- qrf_interp:
 do k = 1, nVertLevels
    kk = nVertLevels+1-k
    do iCell = 1, nCellsSolve
       field_in(iCell,kk) = cld(k,iCell)*qr(k,iCell)
    enddo
 enddo
 call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
 do k = 1, nIntP
    kk = nIntp+1-k
    do iCell = 1, nCellsSolve
       qrf_interp(k,iCell) = field_interp(iCell,kk)
    enddo
 enddo
!call mpas_log_write('--- end interpolate qrf:')

!--- qif_interp:
 do k = 1, nVertLevels
    kk = nVertLevels+1-k
    do iCell = 1, nCellsSolve
       field_in(iCell,kk) = cld(k,iCell)*qi(k,iCell)
    enddo
 enddo
 call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
 do k = 1, nIntP
    kk = nIntp+1-k
    do iCell = 1, nCellsSolve
       qif_interp(k,iCell) = field_interp(iCell,kk)
    enddo
 enddo
!call mpas_log_write('--- end interpolate qif:')

!--- qsf_interp:
 do k = 1, nVertLevels
    kk = nVertLevels+1-k
    do iCell = 1, nCellsSolve
       field_in(iCell,kk) = cld(k,iCell)*qs(k,iCell)
    enddo
 enddo
 call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
 do k = 1, nIntP
    kk = nIntp+1-k
    do iCell = 1, nCellsSolve
       qsf_interp(k,iCell) = field_interp(iCell,kk)
    enddo
 enddo
!call mpas_log_write('--- end interpolate qsf:')

!--- qgf_interp:
 do k = 1, nVertLevels
    kk = nVertLevels+1-k
    do iCell = 1, nCellsSolve
       field_in(iCell,kk) = cld(k,iCell)*qg(k,iCell)
    enddo
 enddo
 call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
 do k = 1, nIntP
    kk = nIntp+1-k
    do iCell = 1, nCellsSolve
       qgf_interp(k,iCell) = field_interp(iCell,kk)
    enddo
 enddo
!call mpas_log_write('--- end interpolate qgf:')

!--- cld_interp:
 do k = 1, nVertLevels
    kk = nVertLevels+1-k
    do iCell = 1, nCellsSolve
       field_in(iCell,kk) = cld(k,iCell)
    enddo
 enddo
 call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
 do k = 1, nIntP
    kk = nIntp+1-k
    do iCell = 1, nCellsSolve
       cld_interp(k,iCell) = field_interp(iCell,kk)
    enddo
 enddo
!call mpas_log_write('--- end interpolate cld:')


!--- deallocate local arrays:
 if(allocated(press_in)) deallocate(press_in)
 if(allocated(field_in)) deallocate(field_in)
 if(allocated(field_interp)) deallocate(field_interp)
 if(allocated(press_interp)) deallocate(press_interp)


 call mpas_log_write('--- exit subroutine atmphys_diagnostics_interp:')

 end subroutine atmphys_diagnostics_interp

!=================================================================================================================
 subroutine atmphys_compute_tendencies_vavg(mesh,state,time_lev,diag,tend_physics,tend_physics_vavg)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: state
 type(mpas_pool_type),intent(in):: diag
 type(mpas_pool_type),intent(in):: tend_physics
 integer,intent(in):: time_lev

!inout arguments:
 type(mpas_pool_type),intent(inout):: tend_physics_vavg

!local variabl_interpes, arrays, and pointers:
 integer:: iCell,k
 integer,pointer:: nCellsSolve,nVertLevels
 integer,pointer:: index_qv

 real:: dz,rhoa,rhoat,sumr
 real(kind=RKIND),dimension(:),pointer    :: dtcup_vavg,dqvcup_vavg,dqccup_vavg,dqrcup_vavg, &
                                             dqicup_vavg,dqscup_vavg
 real(kind=RKIND),dimension(:),pointer    :: dtmp_vavg,dqvmp_vavg,dqcmp_vavg,dqrmp_vavg, &
                                             dqimp_vavg,dqsmp_vavg,dqgmp_vavg
 real(kind=RKIND),dimension(:),pointer    :: dtbl_vavg,dqvbl_vavg,dqcbl_vavg,dqibl_vavg
 real(kind=RKIND),dimension(:),pointer    :: dtlw_vavg,dtsw_vavg                                         
 real(kind=RKIND),dimension(:,:),pointer  :: exner,rho_zz,zgrid,zz
 real(kind=RKIND),dimension(:,:),pointer  :: qv
 real(kind=RKIND),dimension(:,:),pointer  :: dtcup,dqvcup,dqccup,dqrcup,dqicup,dqscup
 real(kind=RKIND),dimension(:,:),pointer  :: dtmp,dqvmp,dqcmp,dqrmp,dqimp,dqsmp,dqgmp
 real(kind=RKIND),dimension(:,:),pointer  :: dtbl,dqvbl,dqcbl,dqibl
 real(kind=RKIND),dimension(:,:),pointer  :: dtlw,dtsw                                          
 real(kind=RKIND),dimension(:,:,:),pointer:: scalars

!-----------------------------------------------------------------------------------------------------------------
 call mpas_log_write(' ')
 call mpas_log_write('--- enter subroutine atmphys_compute_tendencies_vavg:')

 call mpas_pool_get_dimension(mesh,'nCellsSolve',nCellsSolve)
 call mpas_pool_get_dimension(mesh,'nVertLevels',nVertLevels)

 call mpas_pool_get_dimension(state,'index_qv',index_qv)

 call mpas_pool_get_array(mesh,'zz'   ,zz   )
 call mpas_pool_get_array(mesh,'zgrid',zgrid)

 call mpas_pool_get_array(diag,'exner',exner)

 call mpas_pool_get_array(state,'rho_zz' ,rho_zz ,time_lev)
 call mpas_pool_get_array(state,'scalars',scalars,time_lev)
 qv => scalars(index_qv,:,:)

 nullify(dtcup)
 nullify(dqvcup)
 nullify(dqccup)
 nullify(dqrcup)
 nullify(dqicup)
 nullify(dqscup)
 call mpas_pool_get_array(tend_physics,'rthcuten',dtcup )
 call mpas_pool_get_array(tend_physics,'rqvcuten',dqvcup)
 call mpas_pool_get_array(tend_physics,'rqccuten',dqccup)
 call mpas_pool_get_array(tend_physics,'rqrcuten',dqrcup)
 call mpas_pool_get_array(tend_physics,'rqicuten',dqicup)
 call mpas_pool_get_array(tend_physics,'rqscuten',dqscup)
 
 nullify(dtmp)
 nullify(dqvmp)
 nullify(dqcmp)
 nullify(dqrmp)
 nullify(dqimp)
 nullify(dqsmp)
 nullify(dqgmp)
 call mpas_pool_get_array(tend_physics,'rthmpten',dtmp )
 call mpas_pool_get_array(tend_physics,'rqvmpten',dqvmp)
 call mpas_pool_get_array(tend_physics,'rqcmpten',dqcmp)
 call mpas_pool_get_array(tend_physics,'rqrmpten',dqrmp)
 call mpas_pool_get_array(tend_physics,'rqimpten',dqimp)
 call mpas_pool_get_array(tend_physics,'rqsmpten',dqsmp)
 call mpas_pool_get_array(tend_physics,'rqgmpten',dqgmp)

 nullify(dtbl)
 nullify(dqvbl)
 nullify(dqcbl)
 nullify(dqibl)
 call mpas_pool_get_array(tend_physics,'rthblten',dtbl )
 call mpas_pool_get_array(tend_physics,'rqvblten',dqvbl)
 call mpas_pool_get_array(tend_physics,'rqcblten',dqcbl)
 call mpas_pool_get_array(tend_physics,'rqiblten',dqibl)

 nullify(dtlw)
 nullify(dtsw)
 call mpas_pool_get_array(tend_physics,'rthratenlw',dtlw)
 call mpas_pool_get_array(tend_physics,'rthratensw',dtsw)

 nullify(dtcup_vavg)
 nullify(dqvcup_vavg)
 nullify(dqccup_vavg)
 nullify(dqrcup_vavg)
 nullify(dqicup_vavg)
 nullify(dqscup_vavg)
 call mpas_pool_get_array(tend_physics_vavg,'dtcup_vavg' ,dtcup_vavg )
 call mpas_pool_get_array(tend_physics_vavg,'dqvcup_vavg',dqvcup_vavg)
 call mpas_pool_get_array(tend_physics_vavg,'dqccup_vavg',dqccup_vavg)
 call mpas_pool_get_array(tend_physics_vavg,'dqrcup_vavg',dqrcup_vavg)
 call mpas_pool_get_array(tend_physics_vavg,'dqicup_vavg',dqicup_vavg)
 call mpas_pool_get_array(tend_physics_vavg,'dqscup_vavg',dqscup_vavg)

 nullify(dtmp_vavg)
 nullify(dqvmp_vavg)
 nullify(dqcmp_vavg)
 nullify(dqrmp_vavg)
 nullify(dqimp_vavg)
 nullify(dqsmp_vavg)
 nullify(dqgmp_vavg)
 call mpas_pool_get_array(tend_physics_vavg,'dtmp_vavg' ,dtmp_vavg )
 call mpas_pool_get_array(tend_physics_vavg,'dqvmp_vavg',dqvmp_vavg)
 call mpas_pool_get_array(tend_physics_vavg,'dqcmp_vavg',dqcmp_vavg)
 call mpas_pool_get_array(tend_physics_vavg,'dqrmp_vavg',dqrmp_vavg)
 call mpas_pool_get_array(tend_physics_vavg,'dqimp_vavg',dqimp_vavg)
 call mpas_pool_get_array(tend_physics_vavg,'dqsmp_vavg',dqsmp_vavg)
 call mpas_pool_get_array(tend_physics_vavg,'dqgmp_vavg',dqgmp_vavg)

 nullify(dtbl_vavg)
 nullify(dqvbl_vavg)
 nullify(dqcbl_vavg)
 nullify(dqibl_vavg)
 call mpas_pool_get_array(tend_physics_vavg,'dtbl_vavg' ,dtbl_vavg )
 call mpas_pool_get_array(tend_physics_vavg,'dqvbl_vavg',dqvbl_vavg)
 call mpas_pool_get_array(tend_physics_vavg,'dqcbl_vavg',dqcbl_vavg)
 call mpas_pool_get_array(tend_physics_vavg,'dqibl_vavg',dqibl_vavg)

 nullify(dtlw_vavg)
 nullify(dtsw_vavg)
 call mpas_pool_get_array(tend_physics_vavg,'dtlw_vavg',dtlw_vavg)
 call mpas_pool_get_array(tend_physics_vavg,'dtsw_vavg',dtsw_vavg)

!--- calculation of vertically-averaged tendencies:
 if(associated(dtcup)) then
    do iCell = 1, nCellsSolve
       dtcup_vavg(iCell)  = 0._RKIND
       dqvcup_vavg(iCell) = 0._RKIND
       dqccup_vavg(iCell) = 0._RKIND
       dqicup_vavg(iCell) = 0._RKIND

       sumr = 0._RKIND
       do k = 1, nVertLevels
          dz    = zgrid(k+1,iCell)-zgrid(k,iCell)
          rhoa  = zz(k,iCell)*rho_zz(k,iCell)
          rhoa  = rhoa*(1._RKIND+qv(k,iCell))
          rhoat = rhoa*exner(k,iCell)
          sumr  = sumr+rhoa*dz

          !--- vertically-averaged convective tendencies:
          dtcup_vavg(iCell)  = dtcup_vavg(iCell)  + dtcup(k,iCell)*rhoat*dz
          dqvcup_vavg(iCell) = dqvcup_vavg(iCell) + dqvcup(k,iCell)*rhoa*dz
          dqccup_vavg(iCell) = dqccup_vavg(iCell) + dqccup(k,iCell)*rhoa*dz
          dqicup_vavg(iCell) = dqicup_vavg(iCell) + dqicup(k,iCell)*rhoa*dz
       enddo
       dtcup_vavg(iCell) = dtcup_vavg(iCell)/sumr

       if(associated(dqrcup)) then
          dqrcup_vavg(iCell) = 0._RKIND
          dqscup_vavg(iCell) = 0._RKIND

          do k = 1, nVertLevels
             dz   = zgrid(k+1,iCell)-zgrid(k,iCell)
             rhoa = zz(k,iCell)*rho_zz(k,iCell)
             rhoa = rhoa*(1._RKIND+qv(k,iCell))

             !--- vertically-averaged convective tendencies:
             dqrcup_vavg(iCell) = dqrcup_vavg(iCell) + dqrcup(k,iCell)*rhoa*dz
             dqscup_vavg(iCell) = dqscup_vavg(iCell) + dqscup(k,iCell)*rhoa*dz
          enddo
       endif
    enddo
 endif

 if(associated(dtmp)) then
    do iCell = 1, nCellsSolve
       dtmp_vavg(iCell)   = 0._RKIND
       dqvmp_vavg(iCell)  = 0._RKIND
       dqcmp_vavg(iCell)  = 0._RKIND
       dqrmp_vavg(iCell)  = 0._RKIND
       dqimp_vavg(iCell)  = 0._RKIND
       dqsmp_vavg(iCell)  = 0._RKIND
       dqgmp_vavg(iCell)  = 0._RKIND

       sumr = 0._RKIND
       do k = 1, nVertLevels
          dz    = zgrid(k+1,iCell)-zgrid(k,iCell)
          rhoa  = zz(k,iCell)*rho_zz(k,iCell)
          rhoa  = rhoa*(1._RKIND+qv(k,iCell))
          rhoat = rhoa*exner(k,iCell)
          sumr  = sumr+rhoa*dz

          !--- vertically-integrated grid-scale tendencies:
          dtmp_vavg(iCell)  = dtmp_vavg(iCell)  + dtmp(k,iCell)*rhoat*dz
          dqvmp_vavg(iCell) = dqvmp_vavg(iCell) + dqvmp(k,iCell)*rhoa*dz
          dqcmp_vavg(iCell) = dqcmp_vavg(iCell) + dqcmp(k,iCell)*rhoa*dz
          dqrmp_vavg(iCell) = dqrmp_vavg(iCell) + dqrmp(k,iCell)*rhoa*dz
          dqimp_vavg(iCell) = dqimp_vavg(iCell) + dqimp(k,iCell)*rhoa*dz
          dqsmp_vavg(iCell) = dqsmp_vavg(iCell) + dqsmp(k,iCell)*rhoa*dz
          dqgmp_vavg(iCell) = dqgmp_vavg(iCell) + dqgmp(k,iCell)*rhoa*dz
       enddo
       dtmp_vavg(iCell) = dtmp_vavg(iCell)/sumr
    enddo
 endif

 if(associated(dtbl)) then
    do iCell = 1, nCellsSolve
       dtbl_vavg(iCell)   = 0._RKIND
       dqvbl_vavg(iCell)  = 0._RKIND
       dqcbl_vavg(iCell)  = 0._RKIND
       dqibl_vavg(iCell)  = 0._RKIND

       sumr = 0._RKIND
       do k = 1, nVertLevels
          dz    = zgrid(k+1,iCell)-zgrid(k,iCell)
          rhoa  = zz(k,iCell)*rho_zz(k,iCell)
          rhoa  = rhoa*(1._RKIND+qv(k,iCell))
          rhoat = rhoa*exner(k,iCell)
          sumr  = sumr+rhoa*dz

          !--- vertically-integrated PBL tendencies:       
          dtbl_vavg(iCell)  = dtbl_vavg(iCell)  + dtbl(k,iCell)*rhoat*dz
          dqvbl_vavg(iCell) = dqvbl_vavg(iCell) + dqvbl(k,iCell)*rhoa*dz
          dqcbl_vavg(iCell) = dqcbl_vavg(iCell) + dqcbl(k,iCell)*rhoa*dz
          dqibl_vavg(iCell) = dqibl_vavg(iCell) + dqibl(k,iCell)*rhoa*dz
       enddo
       dtbl_vavg(iCell) = dtbl_vavg(iCell)/sumr
    enddo
 endif

 if(associated(dtlw)) then
    do iCell = 1, nCellsSolve
       dtlw_vavg(iCell)   = 0._RKIND

       sumr = 0._RKIND
       do k = 1, nVertLevels
          dz    = zgrid(k+1,iCell)-zgrid(k,iCell)
          rhoa  = zz(k,iCell)*rho_zz(k,iCell)
          rhoa  = rhoa*(1._RKIND+qv(k,iCell))
          rhoat = rhoa*exner(k,iCell)
          sumr  = sumr+rhoa*dz

          !--- vertically-integrated lw radiation tendency:
          dtlw_vavg(iCell) = dtlw_vavg(iCell)  + dtlw(k,iCell)*rhoat*dz
       enddo
       dtlw_vavg(iCell) = dtlw_vavg(iCell)/sumr
    enddo
 endif

 if(associated(dtsw)) then
    do iCell = 1, nCellsSolve
       dtsw_vavg(iCell)   = 0._RKIND

       sumr = 0._RKIND
       do k = 1, nVertLevels
          dz    = zgrid(k+1,iCell)-zgrid(k,iCell)
          rhoa  = zz(k,iCell)*rho_zz(k,iCell)
          rhoa  = rhoa*(1._RKIND+qv(k,iCell))
          rhoat = rhoa*exner(k,iCell)
          sumr  = sumr+rhoa*dz

          !--- vertically-integrated lw radiation tendency:
          dtsw_vavg(iCell) = dtsw_vavg(iCell)  + dtsw(k,iCell)*rhoat*dz
       enddo
       dtsw_vavg(iCell) = dtsw_vavg(iCell)/sumr
    enddo
 endif


 call mpas_log_write('--- exit subroutine atmphys_compute_tendencies_vavg:')

 end subroutine atmphys_compute_tendencies_vavg

!=================================================================================================================
 subroutine atmphys_tendencies_interp(mesh,fg,diag,tend_physics,tend_physics_interp)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: fg
 type(mpas_pool_type),intent(in):: diag
 type(mpas_pool_type),intent(in):: tend_physics

!inout arguments:
 type(mpas_pool_type),intent(inout):: tend_physics_interp

!local variabl_interpes, arrays, and pointers:
 integer:: iCell,k,kk,nIntP
 integer,pointer:: nCellsSolve,nFGLevels,nVertLevels

 real(kind=RKIND),dimension(:,:),pointer:: p_fg
 real(kind=RKIND),dimension(:,:),pointer:: pressure_b,pressure_p,exner
 real(kind=RKIND),dimension(:,:),pointer:: dtcup,dqvcup,dqccup,dqrcup,dqicup,dqscup
 real(kind=RKIND),dimension(:,:),pointer:: dtmp,dqvmp,dqcmp,dqrmp,dqimp,dqsmp,dqgmp
 real(kind=RKIND),dimension(:,:),pointer:: dtbl,dqvbl,dqcbl,dqibl
 real(kind=RKIND),dimension(:,:),pointer:: dtlw,dtsw                                          
 real(kind=RKIND),dimension(:,:),pointer:: dtcup_interp,dqvcup_interp,dqccup_interp,dqrcup_interp, &
                                           dqicup_interp,dqscup_interp
 real(kind=RKIND),dimension(:,:),pointer:: dtmp_interp,dqvmp_interp,dqcmp_interp,dqrmp_interp, &
                                           dqimp_interp,dqsmp_interp,dqgmp_interp
 real(kind=RKIND),dimension(:,:),pointer:: dtbl_interp,dqvbl_interp,dqcbl_interp,dqibl_interp
 real(kind=RKIND),dimension(:,:),pointer:: dtlw_interp,dtsw_interp                                         

!local interpolated fields:
 real(kind=RKIND),dimension(:,:),allocatable:: field_in,press_in
 real(kind=RKIND),dimension(:,:),allocatable:: field_interp,press_interp

!-----------------------------------------------------------------------------------------------------------------
 call mpas_log_write(' ')
 call mpas_log_write('--- enter subroutine atmphys_interp_tendencies:')

 call mpas_pool_get_dimension(mesh,'nCellsSolve',nCellsSolve)
 call mpas_pool_get_dimension(mesh,'nFGLevels'  ,nFGLevels  )
 call mpas_pool_get_dimension(mesh,'nVertLevels',nVertLevels)

 call mpas_pool_get_array(fg,'p_fg',p_fg)
 call mpas_pool_get_array(diag,'pressure_base',pressure_b)
 call mpas_pool_get_array(diag,'pressure_p'   ,pressure_p)
 call mpas_pool_get_array(diag,'exner'        ,exner     )

 nullify(dtcup)
 nullify(dqvcup)
 nullify(dqccup)
 nullify(dqrcup)
 nullify(dqicup)
 nullify(dqscup)
 call mpas_pool_get_array(tend_physics,'rthcuten',dtcup )
 call mpas_pool_get_array(tend_physics,'rqvcuten',dqvcup)
 call mpas_pool_get_array(tend_physics,'rqccuten',dqccup)
 call mpas_pool_get_array(tend_physics,'rqrcuten',dqrcup)
 call mpas_pool_get_array(tend_physics,'rqicuten',dqicup)
 call mpas_pool_get_array(tend_physics,'rqscuten',dqscup)
 
 nullify(dtmp)
 nullify(dqvmp)
 nullify(dqcmp)
 nullify(dqrmp)
 nullify(dqimp)
 nullify(dqsmp)
 nullify(dqgmp)
 call mpas_pool_get_array(tend_physics,'rthmpten',dtmp )
 call mpas_pool_get_array(tend_physics,'rqvmpten',dqvmp)
 call mpas_pool_get_array(tend_physics,'rqcmpten',dqcmp)
 call mpas_pool_get_array(tend_physics,'rqrmpten',dqrmp)
 call mpas_pool_get_array(tend_physics,'rqimpten',dqimp)
 call mpas_pool_get_array(tend_physics,'rqsmpten',dqsmp)
 call mpas_pool_get_array(tend_physics,'rqgmpten',dqgmp)

 nullify(dtbl)
 nullify(dqvbl)
 nullify(dqcbl)
 nullify(dqibl)
 call mpas_pool_get_array(tend_physics,'rthblten',dtbl )
 call mpas_pool_get_array(tend_physics,'rqvblten',dqvbl)
 call mpas_pool_get_array(tend_physics,'rqcblten',dqcbl)
 call mpas_pool_get_array(tend_physics,'rqiblten',dqibl)

 nullify(dtlw)
 nullify(dtsw)
 call mpas_pool_get_array(tend_physics,'rthratenlw',dtlw)
 call mpas_pool_get_array(tend_physics,'rthratensw',dtsw)

 nullify(dtcup_interp)
 nullify(dqvcup_interp)
 nullify(dqccup_interp)
 nullify(dqrcup_interp)
 nullify(dqicup_interp)
 nullify(dqscup_interp)
 call mpas_pool_get_array(tend_physics_interp,'dtcup_interp' ,dtcup_interp )
 call mpas_pool_get_array(tend_physics_interp,'dqvcup_interp',dqvcup_interp)
 call mpas_pool_get_array(tend_physics_interp,'dqccup_interp',dqccup_interp)
 call mpas_pool_get_array(tend_physics_interp,'dqrcup_interp',dqrcup_interp)
 call mpas_pool_get_array(tend_physics_interp,'dqicup_interp',dqicup_interp)
 call mpas_pool_get_array(tend_physics_interp,'dqscup_interp',dqscup_interp)

 nullify(dtmp_interp)
 nullify(dqvmp_interp)
 nullify(dqcmp_interp)
 nullify(dqrmp_interp)
 nullify(dqimp_interp)
 nullify(dqsmp_interp)
 nullify(dqgmp_interp)
 call mpas_pool_get_array(tend_physics_interp,'dtmp_interp' ,dtmp_interp )
 call mpas_pool_get_array(tend_physics_interp,'dqvmp_interp',dqvmp_interp)
 call mpas_pool_get_array(tend_physics_interp,'dqcmp_interp',dqcmp_interp)
 call mpas_pool_get_array(tend_physics_interp,'dqrmp_interp',dqrmp_interp)
 call mpas_pool_get_array(tend_physics_interp,'dqimp_interp',dqimp_interp)
 call mpas_pool_get_array(tend_physics_interp,'dqsmp_interp',dqsmp_interp)
 call mpas_pool_get_array(tend_physics_interp,'dqgmp_interp',dqgmp_interp)

 nullify(dtbl_interp)
 nullify(dqvbl_interp)
 nullify(dqcbl_interp)
 nullify(dqibl_interp)
 call mpas_pool_get_array(tend_physics_interp,'dtbl_interp' ,dtbl_interp )
 call mpas_pool_get_array(tend_physics_interp,'dqvbl_interp',dqvbl_interp)
 call mpas_pool_get_array(tend_physics_interp,'dqcbl_interp',dqcbl_interp)
 call mpas_pool_get_array(tend_physics_interp,'dqibl_interp',dqibl_interp)

 nullify(dtlw_interp)
 nullify(dtsw_interp)
 call mpas_pool_get_array(tend_physics_interp,'dtlw_interp',dtlw_interp)
 call mpas_pool_get_array(tend_physics_interp,'dtsw_interp',dtsw_interp)


!--- define index of the 10 hPa layer; interpolate data to fixed pressure levels:
 nIntP = -1
 iCell = 1
 do k = 1, nFGLevels-1
    if(p_fg(k,iCell) .ge. 1000._RKIND) nIntP = k
 enddo
!call mpas_log_write('--- nIntP = $i',intArgs=(/nIntP/))

 if(.not.allocated(press_in)) allocate(press_in(nCellsSolve,nVertLevels))
 if(.not.allocated(field_in)) allocate(field_in(nCellsSolve,nVertLevels))
 do k = 1, nVertLevels
    kk = nVertLevels+1-k
    do iCell = 1, nCellsSolve
       press_in(iCell,kk) = pressure_p(k,iCell)+pressure_b(k,iCell)
    enddo
 enddo

 if(.not.allocated(field_interp)) allocate(field_interp(nCellsSolve,nIntP))
 if(.not.allocated(press_interp)) allocate(press_interp(nCellsSolve,nIntP))
 do k = 1, nIntP
    kk = nIntP+1-k
    do iCell = 1, nCellsSolve
       press_interp(iCell,kk) = p_fg(k,iCell)
    enddo
 enddo


!--- interpolate convective tendencies to fixed pressure levels: every parameterization of convection outputs
!    tendencies for the temperature,and water vapor,cloud liquid water,and cloud ice mixing ratios. the Kain-
!    Fritsch parameterization also outputs tendencies for rain and snow.
 if(associated(dtcup)) then
    !--- dtcup_interp:
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = dtcup(k,iCell)*exner(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          dtcup_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
!   call mpas_log_write('--- end interpolate dtcup:')

    !--- dqvcup_interp:
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = dqvcup(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          dqvcup_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
!   call mpas_log_write('--- end interpolate dqvcup:')

    !--- dqccup_interp:
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = dqccup(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          dqccup_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
!   call mpas_log_write('--- end interpolate dqccup:')

    !--- dqicup_interp:
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = dqicup(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          dqicup_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
!   call mpas_log_write('--- end interpolate dqicup:')

    if(associated(dqrcup)) then
       !--- dqrcup_interp:
       do k = 1, nVertLevels
          kk = nVertLevels+1-k
          do iCell = 1, nCellsSolve
             field_in(iCell,kk) = dqrcup(k,iCell)
          enddo
       enddo
       call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
       do k = 1, nIntP
          kk = nIntp+1-k
          do iCell = 1, nCellsSolve
             dqrcup_interp(k,iCell) = field_interp(iCell,kk)
          enddo
       enddo
!      call mpas_log_write('--- end interpolate dqrcup:')

       !--- dqscup_interp:
       do k = 1, nVertLevels
          kk = nVertLevels+1-k
          do iCell = 1, nCellsSolve
             field_in(iCell,kk) = dqscup(k,iCell)
          enddo
       enddo
       call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
       do k = 1, nIntP
          kk = nIntp+1-k
          do iCell = 1, nCellsSolve
             dqscup_interp(k,iCell) = field_interp(iCell,kk)
          enddo
       enddo
!      call mpas_log_write('--- end interpolate dqscup:')
    else
       do k = 1, nFGLevels
          do iCell = 1, nCellsSolve
             dqrcup_interp(k,iCell) = 0._RKIND
             dqscup_interp(k,iCell) = 0._RKIND
          enddo
       enddo
    endif
 else
    do k = 1, nFGLevels
       do iCell = 1, nCellsSolve
          dtcup_interp(k,iCell)  = 0._RKIND
          dqvcup_interp(k,iCell) = 0._RKIND
          dqccup_interp(k,iCell) = 0._RKIND
          dqicup_interp(k,iCell) = 0._RKIND
       enddo
    enddo
 endif


!--- interpolate grid-scale tendencies to fixed pressure levels: every parameterization of cloud microphysics
!    outputs tendencies for the temperature,and water vapor,cloud liquid water,rain,cloud ice,snow, and graupel
!    mixing ratios.
 if(associated(dtmp)) then
    !--- dtmp_interp:
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = dtmp(k,iCell)*exner(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          dtmp_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
!   call mpas_log_write('--- end interpolate dtmp:')

    !--- dqvmp_interp:
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = dqvmp(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          dqvmp_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
!   call mpas_log_write('--- end interpolate dqvmp:')

    !--- dqcmp_interp:
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = dqcmp(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          dqcmp_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
!   call mpas_log_write('--- end interpolate dqcmp:')

    !--- dqrmp_interp:
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = dqrmp(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          dqrmp_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
!   call mpas_log_write('--- end interpolate dqrmp:')

    !--- dqimp_interp:
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = dqimp(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          dqimp_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
!   call mpas_log_write('--- end interpolate dqimp:')

    !--- dqsmp_interp:
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = dqsmp(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          dqsmp_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
!   call mpas_log_write('--- end interpolate dqsmp:')

    !--- dqgmp_interp:
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = dqgmp(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          dqgmp_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
!   call mpas_log_write('--- end interpolate dqgmp:')
 else
    do k = 1, nFGLevels
       do iCell = 1, nCellsSolve
          dtmp_interp(k,iCell)  = 0._RKIND
          dqvmp_interp(k,iCell) = 0._RKIND
          dqcmp_interp(k,iCell) = 0._RKIND
          dqrmp_interp(k,iCell) = 0._RKIND
          dqimp_interp(k,iCell) = 0._RKIND
          dqsmp_interp(k,iCell) = 0._RKIND
          dqgmp_interp(k,iCell) = 0._RKIND
       enddo
    enddo
 endif


!--- interpolate PBL tendencies to fixed pressure levels: every PBL parameterization outputs tendencies for
!    the temperature,and water vapor,cloud liquid water, and cloud ice mixing ratios.
 if(associated(dtbl)) then
    !--- dtbl_interp:
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = dtbl(k,iCell)*exner(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          dtbl_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
!   call mpas_log_write('--- end interpolate dtbl:')

    !--- dqvbl_interp:
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = dqvbl(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          dqvbl_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
!   call mpas_log_write('--- end interpolate dqvbl:')

    !--- dqcbl_interp:
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = dqcbl(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          dqcbl_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
!   call mpas_log_write('--- end interpolate dqcbl:')

    !--- dqibl_interp:
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = dqibl(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          dqibl_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
!   call mpas_log_write('--- end interpolate dqibl:')
 else
    do k = 1, nFGLevels
       do iCell = 1, nCellsSolve
          dtbl_interp(k,iCell)  = 0._RKIND
          dqvbl_interp(k,iCell) = 0._RKIND
          dqcbl_interp(k,iCell) = 0._RKIND
          dqibl_interp(k,iCell) = 0._RKIND
       enddo
    enddo
 endif


 if(associated(dtlw)) then
    !--- dtlw_interp:
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = dtlw(k,iCell)*exner(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          dtlw_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
!   call mpas_log_write('--- end interpolate dtlw:')
 else
    do k = 1, nFGLevels
       do iCell = 1, nCellsSolve
          dtlw_interp(k,iCell)  = 0._RKIND
       enddo
    enddo
 endif


 if(associated(dtsw)) then
    !--- dtsw_interp:
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = dtsw(k,iCell)*exner(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          dtsw_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
!   call mpas_log_write('--- end interpolate dtw:')
 else
    do k = 1, nFGLevels
       do iCell = 1, nCellsSolve
          dtsw_interp(k,iCell)  = 0._RKIND
       enddo
    enddo
 endif


!--- deallocate local arrays:
 if(allocated(press_in)) deallocate(press_in)
 if(allocated(field_in)) deallocate(field_in)
 if(allocated(field_interp)) deallocate(field_interp)
 if(allocated(press_interp)) deallocate(press_interp)


 call mpas_log_write('--- exit subroutine atmphys_interp_tendencies:')

 end subroutine atmphys_tendencies_interp

!=================================================================================================================
 function need_atmphys_diagnostics_vavg() result(need_any_diags)
!=================================================================================================================

!local variabl_interpes:
 logical:: need_any_diags

!-----------------------------------------------------------------------------------------------------------------
 need_any_diags = .false.

 need_qv_vavg = .false. ; need_qc_vavg = .false. ; need_qr_vavg = .false. ; need_qi_vavg = .false.
 need_qs_vavg = .false. ; need_qs_vavg = .false.

 need_qcf_vavg = .false. ; need_qrf_vavg = .false. ; need_qif_vavg = .false.
 need_qsf_vavg = .false. ; need_qgf_vavg = .false.

 need_qv_vavg   = mpas_field_will_be_written('qv_vavg')
 need_any_diags = need_any_diags .or. need_qv_vavg
 need_qc_vavg   = mpas_field_will_be_written('qc_vavg')
 need_any_diags = need_any_diags .or. need_qc_vavg
 need_qr_vavg   = mpas_field_will_be_written('qr_vavg')
 need_any_diags = need_any_diags .or. need_qr_vavg
 need_qi_vavg   = mpas_field_will_be_written('qi_vavg')
 need_any_diags = need_any_diags .or. need_qi_vavg
 need_qs_vavg   = mpas_field_will_be_written('qs_vavg')
 need_any_diags = need_any_diags .or. need_qs_vavg
 need_qg_vavg   = mpas_field_will_be_written('qg_vavg')
 need_any_diags = need_any_diags .or. need_qg_vavg

 need_qcf_vavg  = mpas_field_will_be_written('qcf_vavg')
 need_any_diags = need_any_diags .or. need_qcf_vavg
 need_qrf_vavg  = mpas_field_will_be_written('qrf_vavg')
 need_any_diags = need_any_diags .or. need_qrf_vavg
 need_qif_vavg  = mpas_field_will_be_written('qif_vavg')
 need_any_diags = need_any_diags .or. need_qif_vavg
 need_qsf_vavg  = mpas_field_will_be_written('qsf_vavg')
 need_any_diags = need_any_diags .or. need_qsf_vavg
 need_qgf_vavg  = mpas_field_will_be_written('qgf_vavg')
 need_any_diags = need_any_diags .or. need_qgf_vavg

 end function need_atmphys_diagnostics_vavg

!=================================================================================================================
 function need_atmphys_diagnostics_interp() result(need_any_diags)
!=================================================================================================================

!local variabl_interpes:
 logical:: need_any_diags

!-----------------------------------------------------------------------------------------------------------------
 need_any_diags = .false.

 need_qv_interp = .false. ; need_qc_interp = .false. ; need_qr_interp = .false. ; need_qi_interp = .false.
 need_qs_interp = .false. ; need_qs_interp = .false.

 need_qcf_interp = .false. ; need_qrf_interp = .false. ; need_qif_interp = .false.
 need_qsf_interp = .false. ; need_qgf_interp = .false. ; need_cld_interp = .false.

 need_qv_interp = mpas_field_will_be_written('qv_interp')
 need_any_diags = need_any_diags .or. need_qv_interp
 need_qc_interp = mpas_field_will_be_written('qc_interp')
 need_any_diags = need_any_diags .or. need_qc_interp
 need_qr_interp = mpas_field_will_be_written('qr_interp')
 need_any_diags = need_any_diags .or. need_qr_interp
 need_qi_interp = mpas_field_will_be_written('qi_interp')
 need_any_diags = need_any_diags .or. need_qi_interp
 need_qs_interp = mpas_field_will_be_written('qs_interp')
 need_any_diags = need_any_diags .or. need_qs_interp
 need_qg_interp = mpas_field_will_be_written('qg_interp')
 need_any_diags = need_any_diags .or. need_qg_interp

 need_qcf_interp = mpas_field_will_be_written('qcf_interp')
 need_any_diags  = need_any_diags .or. need_qcf_interp
 need_qrf_interp = mpas_field_will_be_written('qrf_interp')
 need_any_diags  = need_any_diags .or. need_qrf_interp
 need_qif_interp = mpas_field_will_be_written('qif_interp')
 need_any_diags  = need_any_diags .or. need_qif_interp
 need_qsf_interp = mpas_field_will_be_written('qsf_interp')
 need_any_diags  = need_any_diags .or. need_qsf_interp
 need_qgf_interp = mpas_field_will_be_written('qgf_interp')
 need_any_diags  = need_any_diags .or. need_qgf_interp
 need_cld_interp = mpas_field_will_be_written('cld_interp')
 need_any_diags  = need_any_diags .or. need_cld_interp

 end function need_atmphys_diagnostics_interp

!=================================================================================================================
 function need_atmphys_tendencies_vavg() result(need_any_diags)
!=================================================================================================================

!local variabl_interpes:
 logical:: need_any_diags

!-----------------------------------------------------------------------------------------------------------------
 need_any_diags = .false.

 need_dtcup_vavg  = .false. ; need_dqvcup_vavg = .false. ; need_dqccup_vavg = .false.
 need_dqrcup_vavg = .false. ; need_dqicup_vavg = .false. ; need_dqscup_vavg = .false.

 need_dtmp_vavg   = .false. ; need_dqvmp_vavg  = .false. ; need_dqcmp_vavg  = .false.
 need_dqrmp_vavg  = .false. ; need_dqimp_vavg  = .false. ; need_dqsmp_vavg  = .false.
 need_dqgmp_vavg  = .false.

 need_dtbl_vavg   = .false. ; need_dqvbl_vavg  = .false. ; need_dqcbl_vavg  = .false.
 need_dqibl_vavg  = .false.

 need_dtlw_vavg   = .false. ; need_dtsw_vavg   = .false.


 need_dtcup_vavg  = mpas_field_will_be_written('dtcup_vavg')
 need_any_diags   = need_any_diags .or. need_dtcup_vavg
 need_dqvcup_vavg = mpas_field_will_be_written('dqvcup_vavg')
 need_any_diags   = need_any_diags .or. need_dqvcup_vavg
 need_dqccup_vavg = mpas_field_will_be_written('dqccup_vavg')
 need_any_diags   = need_any_diags .or. need_dqccup_vavg
 need_dqrcup_vavg = mpas_field_will_be_written('dqrcup_vavg')
 need_any_diags   = need_any_diags .or. need_dqrcup_vavg
 need_dqicup_vavg = mpas_field_will_be_written('dqicup_vavg')
 need_any_diags   = need_any_diags .or. need_dqicup_vavg
 need_dqscup_vavg = mpas_field_will_be_written('dqscup_vavg')
 need_any_diags   = need_any_diags .or. need_dqscup_vavg

 need_dtmp_vavg   = mpas_field_will_be_written('dtmp_vavg')
 need_any_diags   = need_any_diags .or. need_dtmp_vavg
 need_dqvmp_vavg  = mpas_field_will_be_written('dqvmp_vavg')
 need_any_diags   = need_any_diags .or. need_dqvmp_vavg
 need_dqcmp_vavg  = mpas_field_will_be_written('dqcmp_vavg')
 need_any_diags   = need_any_diags .or. need_dqcmp_vavg
 need_dqrmp_vavg  = mpas_field_will_be_written('dqrmp_vavg')
 need_any_diags   = need_any_diags .or. need_dqrmp_vavg
 need_dqimp_vavg  = mpas_field_will_be_written('dqimp_vavg')
 need_any_diags   = need_any_diags .or. need_dqimp_vavg
 need_dqsmp_vavg  = mpas_field_will_be_written('dqsmp_vavg')
 need_any_diags   = need_any_diags .or. need_dqsmp_vavg
 need_dqgmp_vavg  = mpas_field_will_be_written('dqgmp_vavg')
 need_any_diags   = need_any_diags .or. need_dqgmp_vavg

 need_dtbl_vavg   = mpas_field_will_be_written('dtbl_vavg')
 need_any_diags   = need_any_diags .or. need_dtbl_vavg
 need_dqvbl_vavg  = mpas_field_will_be_written('dqvbl_vavg')
 need_any_diags   = need_any_diags .or. need_dqvbl_vavg
 need_dqcbl_vavg  = mpas_field_will_be_written('dqcbl_vavg')
 need_any_diags   = need_any_diags .or. need_dqcbl_vavg
 need_dqibl_vavg  = mpas_field_will_be_written('dqibl_vavg')
 need_any_diags   = need_any_diags .or. need_dqibl_vavg

 need_dtlw_vavg   = mpas_field_will_be_written('dtlw_vavg')
 need_any_diags   = need_any_diags .or. need_dtlw_vavg
 need_dtsw_vavg   = mpas_field_will_be_written('dtsw_vavg')
 need_any_diags   = need_any_diags .or. need_dtsw_vavg


 end function need_atmphys_tendencies_vavg

!=================================================================================================================
 function need_atmphys_tendencies_interp() result(need_any_diags)
!=================================================================================================================

!local variabl_interpes:
 logical:: need_any_diags

!-----------------------------------------------------------------------------------------------------------------
 need_any_diags = .false.

 need_dtcup_interp  = .false. ; need_dqvcup_interp = .false. ; need_dqccup_interp = .false.
 need_dqrcup_interp = .false. ; need_dqicup_interp = .false. ; need_dqscup_interp = .false.

 need_dtmp_interp   = .false. ; need_dqvmp_interp  = .false. ; need_dqcmp_interp  = .false.
 need_dqrmp_interp  = .false. ; need_dqimp_interp  = .false. ; need_dqsmp_interp  = .false.
 need_dqgmp_interp  = .false.

 need_dtbl_interp   = .false. ; need_dqvbl_interp  = .false. ; need_dqcbl_interp  = .false.
 need_dqibl_interp  = .false.

 need_dtlw_interp   = .false. ; need_dtsw_interp   = .false.


 need_dtcup_interp  = mpas_field_will_be_written('dtcup_interp')
 need_any_diags     = need_any_diags .or. need_dtcup_interp
 need_dqvcup_interp = mpas_field_will_be_written('dqvcup_interp')
 need_any_diags     = need_any_diags .or. need_dqvcup_interp
 need_dqccup_interp = mpas_field_will_be_written('dqccup_interp')
 need_any_diags     = need_any_diags .or. need_dqccup_interp
 need_dqrcup_interp = mpas_field_will_be_written('dqrcup_interp')
 need_any_diags     = need_any_diags .or. need_dqrcup_interp
 need_dqicup_interp = mpas_field_will_be_written('dqicup_interp')
 need_any_diags     = need_any_diags .or. need_dqicup_interp
 need_dqscup_interp = mpas_field_will_be_written('dqscup_interp')
 need_any_diags     = need_any_diags .or. need_dqscup_interp

 need_dtmp_interp   = mpas_field_will_be_written('dtmp_interp')
 need_any_diags     = need_any_diags .or. need_dtmp_interp
 need_dqvmp_interp  = mpas_field_will_be_written('dqvmp_interp')
 need_any_diags     = need_any_diags .or. need_dqvmp_interp
 need_dqcmp_interp  = mpas_field_will_be_written('dqcmp_interp')
 need_any_diags     = need_any_diags .or. need_dqcmp_interp
 need_dqrmp_interp  = mpas_field_will_be_written('dqrmp_interp')
 need_any_diags     = need_any_diags .or. need_dqrmp_interp
 need_dqimp_interp  = mpas_field_will_be_written('dqimp_interp')
 need_any_diags     = need_any_diags .or. need_dqimp_interp
 need_dqsmp_interp  = mpas_field_will_be_written('dqsmp_interp')
 need_any_diags     = need_any_diags .or. need_dqsmp_interp
 need_dqgmp_interp  = mpas_field_will_be_written('dqgmp_interp')
 need_any_diags     = need_any_diags .or. need_dqgmp_interp

 need_dtbl_interp   = mpas_field_will_be_written('dtbl_interp')
 need_any_diags     = need_any_diags .or. need_dtbl_interp
 need_dqvbl_interp  = mpas_field_will_be_written('dqvbl_interp')
 need_any_diags     = need_any_diags .or. need_dqvbl_interp
 need_dqcbl_interp  = mpas_field_will_be_written('dqcbl_interp')
 need_any_diags     = need_any_diags .or. need_dqcbl_interp
 need_dqibl_interp  = mpas_field_will_be_written('dqibl_interp')
 need_any_diags     = need_any_diags .or. need_dqibl_interp

 need_dtlw_interp   = mpas_field_will_be_written('dtlw_interp')
 need_any_diags     = need_any_diags .or. need_dtlw_interp
 need_dtsw_interp   = mpas_field_will_be_written('dtsw_interp')
 need_any_diags     = need_any_diags .or. need_dtsw_interp


 end function need_atmphys_tendencies_interp

!=================================================================================================================
 subroutine interp_tofixed_pressure(ncol,nlev_in,nlev_out,pres_in,field_in,pres_out,field_out)
!=================================================================================================================

!input arguments:
 integer,intent(in):: ncol,nlev_in,nlev_out

 real(kind=RKIND),intent(in),dimension(ncol,nlev_in) :: pres_in,field_in
 real(kind=RKIND),intent(in),dimension(ncol,nlev_out):: pres_out

!output arguments:
 real(kind=RKIND),intent(out),dimension(ncol,nlev_out):: field_out

!local variabl_interpes:
! integer:: i1,i2,icol,k,kk
 integer:: icol,k,kk
 integer:: kkstart,kount
 integer,dimension(ncol):: kupper

 real(kind=RKIND):: dpl,dpu

!-----------------------------------------------------------------------------------------------------------------

!write(0,*)
!write(0,*) '--- enter subroutine interp_tofixed_pressure:'
!write(0,*) '... ncol     = ',ncol
!write(0,*) '... nlev_in  = ',nlev_in
!write(0,*) '... nlev_out = ',nlev_out
!i1=1 ; i2=ncol
!do k = 1, nlev_in
!   write(0,201) k,pres_in(i1,k),field_in(i1,k),pres_in(i2,k),field_in(i2,k)
!enddo
!write(0,*)

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
!            write(0,201) kupper(icol),pres_out(icol,k),pres_in(icol,kk),pres_in(icol,kk+1)
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
 enddo

 end subroutine interp_tofixed_pressure

!=================================================================================================================
 subroutine compute_rh(nCells,nCellsSolve,nVertLevels,pressure_b,pressure_p,exner,theta_m,qv,rh)
!=================================================================================================================

!input arguments:
 integer,intent(in):: nCells,nCellsSolve,nVertLevels
 real(kind=RKIND),intent(in),dimension(nVertLevels,nCells+1):: pressure_b,pressure_p,exner,theta_m,qv

!output arguments:
 real(kind=RKIND),intent(out),dimension(nVertLevels,nCells+1):: rh

!local variables and array:
 integer:: iCell,k

 real(kind=RKIND):: tempc
 real(kind=RKIND),dimension(:),allocatable:: qv1d,qvs1d,t1d,p1d

!-----------------------------------------------------------------------------------------------------------------

 do k = 1, nVertLevels
    do iCell = 1, nCellsSolve
       rh(k,iCell) = 0._RKIND
    enddo
 enddo

 if(.not.allocated(p1d)  ) allocate(p1d(nVertLevels)  )
 if(.not.allocated(t1d)  ) allocate(t1d(nVertLevels)  )
 if(.not.allocated(qv1d) ) allocate(qv1d(nVertLevels) )
 if(.not.allocated(qvs1d)) allocate(qvs1d(nVertLevels))

 do iCell = 1, nCellsSolve
    do k = 1, nVertLevels
       p1d(k)   = pressure_p(k,iCell) + pressure_b(k,iCell)
       t1d(k)   = theta_m(k,iCell)*exner(k,iCell)/(1._RKIND+R_v/R_d*qv(k,iCell))
       qv1d(k)  = qv(k,iCell)
       tempc    = t1d(k) - 273.16_RKIND
       qvs1d(k) = rslf(p1d(k),t1d(k))
       if(tempc .le. 0._RKIND) qvs1d(k) = rsif(p1d(k),t1d(k))

       rh(k,iCell) = qv1d(k) / qvs1d(k)
    enddo
 enddo

 if(allocated(p1d)  ) deallocate(p1d  )
 if(allocated(t1d)  ) deallocate(t1d  )
 if(allocated(qv1d) ) deallocate(qv1d )
 if(allocated(qvs1d)) deallocate(qvs1d)

 end subroutine compute_rh

!=================================================================================================================
 end module mpas_atmphys_compute_diagnostics
!=================================================================================================================
