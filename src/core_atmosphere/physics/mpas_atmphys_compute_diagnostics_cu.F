! Copyright (c) 2016,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module mpas_atmphys_compute_diagnostics_cu
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
 public:: need_atmphys_diagnostics_cu,    &
          need_atmphys_tendencies_cu,     &
          atmphys_compute_diagnostics_cu, &
          atmphys_compute_tendencies_cu,  &
          interp_tofixed_pressure

 type(mpas_clock_type),pointer:: clock

 type(mpas_pool_type),pointer:: mesh
 type(mpas_pool_type),pointer:: state
 type(mpas_pool_type),pointer:: diag
 type(mpas_pool_type),pointer:: diag_physics
 type(mpas_pool_type),pointer:: diag_physics_cu

 logical:: need_cldp_k22s,need_cldt_k22s,need_cldh_k22s, &
           need_cldp_k22m,need_cldt_k22m,need_cldh_k22m, &
           need_cldp_k22d,need_cldt_k22d,need_cldh_k22d 
 logical:: need_cldp_kbcons,need_cldt_kbcons,need_cldh_kbcons, &
           need_cldp_kbconm,need_cldt_kbconm,need_cldh_kbconm, &
           need_cldp_kbcond,need_cldt_kbcond,need_cldh_kbcond 
 logical:: need_cldp_ktops,need_cldt_ktops,need_cldh_ktops, &
           need_cldp_ktopm,need_cldt_ktopm,need_cldh_ktopm, &
           need_cldp_ktopd,need_cldt_ktopd,need_cldh_ktopd 

 logical:: need_zu_shalc_interp,need_zd_shalc_interp, &
           need_zu_midlc_interp,need_zd_midlc_interp, &
           need_zu_deepc_interp,need_zd_deepc_interp

 logical:: need_dtcups_vavg ,need_dqvcups_vavg,need_dqccups_vavg, &
           need_dqicups_vavg,need_dqrcups_vavg,need_dqscups_vavg


 contains


!=================================================================================================================
 subroutine atmphys_compute_diagnostics_cu(mesh,state,time_lev,fg,diag,diag_physics,diag_physics_bl, &
                                           diag_physics_cu)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: state
 type(mpas_pool_type),intent(in):: fg
 type(mpas_pool_type),intent(in):: diag
 type(mpas_pool_type),intent(in):: diag_physics
 type(mpas_pool_type),intent(in):: diag_physics_bl
 integer,intent(in):: time_lev

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics_cu

!local variables, arrays, and pointers:
 integer:: iCell,k,kb,kc,kt
 integer,pointer:: nCellsSolve,nVertLevels
 integer,pointer:: index_qv,index_qc,index_qr,index_qi,index_qs,index_qg
 integer,dimension(:),pointer:: k22s,k22m,k22d,kbcons,kbconm,kbcond,ktops,ktopm,ktopd
 integer,dimension(:),pointer:: kpbl

 real(kind=RKIND),dimension(:,:),pointer  :: rho_zz,zgrid,zz
 real(kind=RKIND),dimension(:,:),pointer  :: theta_m
 real(kind=RKIND),dimension(:,:),pointer  :: pressure_b,pressure_p,exner
 real(kind=RKIND),dimension(:,:),pointer  :: qv,qc,qr,qi,qs,qg
 real(kind=RKIND),dimension(:,:),pointer  :: qc_cu,qi_cu,qc_cu_shalc,qi_cu_shalc
 real(kind=RKIND),dimension(:,:),pointer  :: zu_shalc,zd_shalc,zu_midlc,zd_midlc,zu_deepc,zd_deepc
 real(kind=RKIND),dimension(:,:,:),pointer:: scalars

 real(kind=RKIND),dimension(:),pointer:: cldp_k22s,cldt_k22s,cldh_k22s, &
                                         cldp_k22m,cldt_k22m,cldh_k22m, &
                                         cldp_k22d,cldt_k22d,cldh_k22d
 real(kind=RKIND),dimension(:),pointer:: cldp_kbcons,cldt_kbcons,cldh_kbcons, &
                                         cldp_kbconm,cldt_kbconm,cldh_kbconm, &
                                         cldp_kbcond,cldt_kbcond,cldh_kbcond
 real(kind=RKIND),dimension(:),pointer:: cldp_ktops,cldt_ktops,cldh_ktops, &
                                         cldp_ktopm,cldt_ktopm,cldh_ktopm, &
                                         cldp_ktopd,cldt_ktopd,cldh_ktopd

 real(kind=RKIND),dimension(:),pointer:: qc_shalc,qc_midlc,qc_deepc,qr_shalc,qr_midlc,qr_deepc, &
                                         qi_shalc,qi_midlc,qi_deepc,qs_shalc,qs_midlc,qs_deepc, &
                                         qg_shalc,qg_midlc,qg_deepc

 real(kind=RKIND),dimension(:),pointer:: qc_kbcons,qc_kbconm,qc_kbcond, &
                                         qr_kbcons,qr_kbconm,qr_kbcond, &
                                         qi_kbcons,qi_kbconm,qi_kbcond, &
                                         qs_kbcons,qs_kbconm,qs_kbcond, &
                                         qg_kbcons,qg_kbconm,qg_kbcond

 real(kind=RKIND),dimension(:),pointer:: qc_ktops,qc_ktopm,qc_ktopd, &
                                         qr_ktops,qr_ktopm,qr_ktopd, &
                                         qi_ktops,qi_ktopm,qi_ktopd, &
                                         qs_ktops,qs_ktopm,qs_ktopd, &
                                         qg_ktops,qg_ktopm,qg_ktopd

 real(kind=RKIND),dimension(:),pointer:: qc_aconv,qr_aconv,qi_aconv, &
                                         qs_aconv,qg_aconv

 real(kind=RKIND),dimension(:),pointer:: qccu_vavg,qicu_vavg,qccu_shalc_vavg,qicu_shalc_vavg

 real(kind=RKIND),dimension(:,:),pointer:: zu_shalc_interp,zd_shalc_interp, &
                                           zu_midlc_interp,zd_midlc_interp, &
                                           zu_deepc_interp,zd_deepc_interp

 real(kind=RKIND),dimension(:,:),allocatable:: dz,press,rhoa,temp,zgridl

!--- additional tests:
 real(kind=RKIND),dimension(:),pointer:: rinc_d,rinc_s,rinc_o

!--- interpolation to fixed pressure levels:
 integer:: kk,nIntP
 integer,pointer:: nFGLevels

 real(kind=RKIND),dimension(:,:),pointer:: p_fg

 real(kind=RKIND),dimension(:,:),allocatable:: field_in,press_in
 real(kind=RKIND),dimension(:,:),allocatable:: field_interp,press_interp

!--- temporary:
 real(kind=RKIND),dimension(:),pointer:: raincv


!-----------------------------------------------------------------------------------------------------------------
 call mpas_log_write(' ')
 call mpas_log_write('--- enter subroutine atmphys_compute_diagnostics_cu:')

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
!do iCell = 1, nCellsSolve
!   do k = 1, nVertLevels
!      call mpas_log_write('$i $i $r $r $r $r $r',intArgs=(/iCell,k/),realArgs=(/zgridl(k,iCell),dz(k,iCell), &
!                           press(k,iCell)/100.,temp(k,iCell),rhoa(k,iCell)/dz(k,iCell)/))
!   enddo
!   call mpas_log_write(' ')
!enddo

!--- calculation of pressure, temperature, and height at k22,kbcon,and ktop levels for shallow,middle,and deep
!    convection:
 call mpas_pool_get_array(diag_physics,'kpbl',kpbl)
 call mpas_pool_get_array(diag_physics,'rinc_shalc',rinc_s)
 call mpas_pool_get_array(diag_physics,'rinc_deepc',rinc_d)
 call mpas_pool_get_array(diag_physics,'rinc_overlap',rinc_o)

 call mpas_pool_get_array(diag_physics_cu,'k22_shalc',k22s)
 call mpas_pool_get_array(diag_physics_cu,'k22_midlc',k22m)
 call mpas_pool_get_array(diag_physics_cu,'k22_deepc',k22d)

 call mpas_pool_get_array(diag_physics_cu,'kbcon_shalc',kbcons)
 call mpas_pool_get_array(diag_physics_cu,'kbcon_midlc',kbconm)
 call mpas_pool_get_array(diag_physics_cu,'kbcon_deepc',kbcond)

 call mpas_pool_get_array(diag_physics_cu,'ktop_shalc',ktops)
 call mpas_pool_get_array(diag_physics_cu,'ktop_midlc',ktopm)
 call mpas_pool_get_array(diag_physics_cu,'ktop_deepc',ktopd)

 call mpas_pool_get_array(diag_physics_cu,'cldp_k22s',cldp_k22s)
 call mpas_pool_get_array(diag_physics_cu,'cldt_k22s',cldt_k22s)
 call mpas_pool_get_array(diag_physics_cu,'cldh_k22s',cldh_k22s)
 call mpas_pool_get_array(diag_physics_cu,'cldp_k22m',cldp_k22m)
 call mpas_pool_get_array(diag_physics_cu,'cldt_k22m',cldt_k22m)
 call mpas_pool_get_array(diag_physics_cu,'cldh_k22m',cldh_k22m)
 call mpas_pool_get_array(diag_physics_cu,'cldp_k22d',cldp_k22d)
 call mpas_pool_get_array(diag_physics_cu,'cldt_k22d',cldt_k22d)
 call mpas_pool_get_array(diag_physics_cu,'cldh_k22d',cldh_k22d)

 call mpas_pool_get_array(diag_physics_cu,'cldp_kbcons',cldp_kbcons)
 call mpas_pool_get_array(diag_physics_cu,'cldt_kbcons',cldt_kbcons)
 call mpas_pool_get_array(diag_physics_cu,'cldh_kbcons',cldh_kbcons)
 call mpas_pool_get_array(diag_physics_cu,'cldp_kbconm',cldp_kbconm)
 call mpas_pool_get_array(diag_physics_cu,'cldt_kbconm',cldt_kbconm)
 call mpas_pool_get_array(diag_physics_cu,'cldh_kbconm',cldh_kbconm)
 call mpas_pool_get_array(diag_physics_cu,'cldp_kbcond',cldp_kbcond)
 call mpas_pool_get_array(diag_physics_cu,'cldt_kbcond',cldt_kbcond)
 call mpas_pool_get_array(diag_physics_cu,'cldh_kbcond',cldh_kbcond)

 call mpas_pool_get_array(diag_physics_cu,'cldp_ktops' ,cldp_ktops)
 call mpas_pool_get_array(diag_physics_cu,'cldt_ktops' ,cldt_ktops)
 call mpas_pool_get_array(diag_physics_cu,'cldh_ktops' ,cldh_ktops)
 call mpas_pool_get_array(diag_physics_cu,'cldp_ktopm' ,cldp_ktopm)
 call mpas_pool_get_array(diag_physics_cu,'cldt_ktopm' ,cldt_ktopm)
 call mpas_pool_get_array(diag_physics_cu,'cldh_ktopm' ,cldh_ktopm)
 call mpas_pool_get_array(diag_physics_cu,'cldp_ktopd' ,cldp_ktopd)
 call mpas_pool_get_array(diag_physics_cu,'cldt_ktopd' ,cldt_ktopd)
 call mpas_pool_get_array(diag_physics_cu,'cldh_ktopd' ,cldh_ktopd)

 do iCell = 1, nCellsSolve
    cldp_k22s(iCell) = -999._RKIND
    cldt_k22s(iCell) = -999._RKIND
    cldh_k22s(iCell) = -999._RKIND
    cldp_k22m(iCell) = -999._RKIND
    cldt_k22m(iCell) = -999._RKIND
    cldh_k22m(iCell) = -999._RKIND
    cldp_k22d(iCell) = -999._RKIND
    cldt_k22d(iCell) = -999._RKIND
    cldh_k22d(iCell) = -999._RKIND

    cldp_kbcons(iCell) = -999._RKIND
    cldt_kbcons(iCell) = -999._RKIND
    cldh_kbcons(iCell) = -999._RKIND
    cldp_kbconm(iCell) = -999._RKIND
    cldt_kbconm(iCell) = -999._RKIND
    cldh_kbconm(iCell) = -999._RKIND
    cldp_kbcond(iCell) = -999._RKIND
    cldt_kbcond(iCell) = -999._RKIND
    cldh_kbcond(iCell) = -999._RKIND
 
    cldp_ktops(iCell) = -999._RKIND
    cldt_ktops(iCell) = -999._RKIND
    cldh_ktops(iCell) = -999._RKIND
    cldp_ktopm(iCell) = -999._RKIND
    cldt_ktopm(iCell) = -999._RKIND
    cldh_ktopm(iCell) = -999._RKIND
    cldp_ktopd(iCell) = -999._RKIND
    cldt_ktopd(iCell) = -999._RKIND
    cldh_ktopd(iCell) = -999._RKIND
 enddo

 if(need_cldp_k22s .or. need_cldt_k22s .or. need_cldh_k22s) then
    do iCell = 1, nCellsSolve
       if(k22s(iCell).gt.0 .and. k22s(iCell).lt.nVertLevels) then
          k = k22s(iCell)
          cldp_k22s(iCell) = press(k,iCell)
          cldt_k22s(iCell) = temp(k,iCell)
          cldh_k22s(iCell) = zgridl(k,iCell)
       endif
    enddo
 endif
 if(need_cldp_k22m .or. need_cldt_k22m .or. need_cldh_k22m) then
    do iCell = 1, nCellsSolve!
       if(k22m(iCell).gt.0 .and. k22m(iCell).lt.nVertLevels) then
          k = k22m(iCell)
          cldp_k22m(iCell) = press(k,iCell)
          cldt_k22m(iCell) = temp(k,iCell)
          cldh_k22m(iCell) = zgridl(k,iCell)
       endif
    enddo
 endif
 if(need_cldp_k22d .or. need_cldt_k22d .or. need_cldh_k22d) then
    do iCell = 1, nCellsSolve
       if(k22d(iCell).gt.0 .and. k22d(iCell).lt.nVertLevels) then
          k = k22d(iCell)
          cldp_k22d(iCell) = press(k,iCell)
          cldt_k22d(iCell) = temp(k,iCell)
          cldh_k22d(iCell) = zgridl(k,iCell)
       endif
    enddo
 endif

 if(need_cldp_kbcons .or. need_cldt_kbcons .or. need_cldh_kbcons) then
    do iCell = 1, nCellsSolve
       if(kbcons(iCell).gt.0 .and. kbcons(iCell).lt.nVertLevels) then
          k = kbcons(iCell)
          cldp_kbcons(iCell) = press(k,iCell)
          cldt_kbcons(iCell) = temp(k,iCell)
          cldh_kbcons(iCell) = zgridl(k,iCell)
       endif
    enddo
 endif
 if(need_cldp_kbconm .or. need_cldt_kbconm .or. need_cldh_kbconm) then
    do iCell = 1, nCellsSolve
       if(kbconm(iCell).gt.0 .and. kbconm(iCell).lt.nVertLevels) then
          k = kbconm(iCell)
          cldp_kbconm(iCell) = press(k,iCell)
          cldt_kbconm(iCell) = temp(k,iCell)
          cldh_kbconm(iCell) = zgridl(k,iCell)
       endif
    enddo
 endif
 if(need_cldp_kbcond .or. need_cldt_kbcond .or. need_cldh_kbcond) then
    do iCell = 1, nCellsSolve
       if(kbcond(iCell).gt.0 .and. kbcond(iCell).lt.nVertLevels) then
          k = kbcond(iCell)
          cldp_kbcond(iCell) = press(k,iCell)
          cldt_kbcond(iCell) = temp(k,iCell)
          cldh_kbcond(iCell) = zgridl(k,iCell)
       endif
    enddo
 endif

 if(need_cldp_ktops .or. need_cldt_ktops .or. need_cldh_ktops) then
    do iCell = 1, nCellsSolve
       if(ktops(iCell) .gt. 1) then
          k = ktops(iCell)
          cldp_ktops(iCell) = press(k,iCell)
          cldt_ktops(iCell) = temp(k,iCell)
          cldh_ktops(iCell) = zgridl(k,iCell)
       endif
    enddo
 endif
 if(need_cldp_ktopm .or. need_cldt_ktopm .or. need_cldh_ktopm) then
    do iCell = 1, nCellsSolve
       if(ktopm(iCell) .gt. 1) then
          k = ktopm(iCell)
          cldp_ktopm(iCell) = press(k,iCell)
          cldt_ktopm(iCell) = temp(k,iCell)
          cldh_ktopm(iCell) = zgridl(k,iCell)
       endif
    enddo
 endif
 if(need_cldp_ktopd .or. need_cldt_ktopd .or. need_cldh_ktopd) then
    do iCell = 1, nCellsSolve
       if(ktopd(iCell) .gt. 1) then
          k = ktopd(iCell)
          cldp_ktopd(iCell) = press(k,iCell)
          cldt_ktopd(iCell) = temp(k,iCell)
          cldh_ktopd(iCell) = zgridl(k,iCell)
       endif
    enddo
 endif

!--- calculation of cloud water, rain water, cloud ice, snow, and graupel paths for shallow, mid-level,
!    and deep convection:
 call mpas_pool_get_array(diag_physics_cu,'qc_shalc',qc_shalc)
 call mpas_pool_get_array(diag_physics_cu,'qc_midlc',qc_midlc)
 call mpas_pool_get_array(diag_physics_cu,'qc_deepc',qc_deepc)
 call mpas_pool_get_array(diag_physics_cu,'qr_shalc',qr_shalc)
 call mpas_pool_get_array(diag_physics_cu,'qr_midlc',qr_midlc)
 call mpas_pool_get_array(diag_physics_cu,'qr_deepc',qr_deepc)
 call mpas_pool_get_array(diag_physics_cu,'qi_shalc',qi_shalc)
 call mpas_pool_get_array(diag_physics_cu,'qi_midlc',qi_midlc)
 call mpas_pool_get_array(diag_physics_cu,'qi_deepc',qi_deepc)
 call mpas_pool_get_array(diag_physics_cu,'qs_shalc',qs_shalc)
 call mpas_pool_get_array(diag_physics_cu,'qs_midlc',qs_midlc)
 call mpas_pool_get_array(diag_physics_cu,'qs_deepc',qs_deepc)
 call mpas_pool_get_array(diag_physics_cu,'qg_shalc',qg_shalc)
 call mpas_pool_get_array(diag_physics_cu,'qg_midlc',qg_midlc)
 call mpas_pool_get_array(diag_physics_cu,'qg_deepc',qg_deepc)

 call mpas_pool_get_array(diag_physics_cu,'qc_kbcon_shalc',qc_kbcons)
 call mpas_pool_get_array(diag_physics_cu,'qr_kbcon_shalc',qr_kbcons)
 call mpas_pool_get_array(diag_physics_cu,'qi_kbcon_shalc',qi_kbcons)
 call mpas_pool_get_array(diag_physics_cu,'qs_kbcon_shalc',qs_kbcons)
 call mpas_pool_get_array(diag_physics_cu,'qg_kbcon_shalc',qg_kbcons)

 call mpas_pool_get_array(diag_physics_cu,'qc_kbcon_midlc',qc_kbconm)
 call mpas_pool_get_array(diag_physics_cu,'qr_kbcon_midlc',qr_kbconm)
 call mpas_pool_get_array(diag_physics_cu,'qi_kbcon_midlc',qi_kbconm)
 call mpas_pool_get_array(diag_physics_cu,'qs_kbcon_midlc',qs_kbconm)
 call mpas_pool_get_array(diag_physics_cu,'qg_kbcon_midlc',qg_kbconm)

 call mpas_pool_get_array(diag_physics_cu,'qc_kbcon_deepc',qc_kbcond)
 call mpas_pool_get_array(diag_physics_cu,'qr_kbcon_deepc',qr_kbcond)
 call mpas_pool_get_array(diag_physics_cu,'qi_kbcon_deepc',qi_kbcond)
 call mpas_pool_get_array(diag_physics_cu,'qs_kbcon_deepc',qs_kbcond)
 call mpas_pool_get_array(diag_physics_cu,'qg_kbcon_deepc',qg_kbcond)

 call mpas_pool_get_array(diag_physics_cu,'qc_ktop_shalc',qc_ktops)
 call mpas_pool_get_array(diag_physics_cu,'qr_ktop_shalc',qr_ktops)
 call mpas_pool_get_array(diag_physics_cu,'qi_ktop_shalc',qi_ktops)
 call mpas_pool_get_array(diag_physics_cu,'qs_ktop_shalc',qs_ktops)
 call mpas_pool_get_array(diag_physics_cu,'qg_ktop_shalc',qg_ktops)

 call mpas_pool_get_array(diag_physics_cu,'qc_ktop_midlc',qc_ktopm)
 call mpas_pool_get_array(diag_physics_cu,'qr_ktop_midlc',qr_ktopm)
 call mpas_pool_get_array(diag_physics_cu,'qi_ktop_midlc',qi_ktopm)
 call mpas_pool_get_array(diag_physics_cu,'qs_ktop_midlc',qs_ktopm)
 call mpas_pool_get_array(diag_physics_cu,'qg_ktop_midlc',qg_ktopm)

 call mpas_pool_get_array(diag_physics_cu,'qc_ktop_deepc',qc_ktopd)
 call mpas_pool_get_array(diag_physics_cu,'qr_ktop_deepc',qr_ktopd)
 call mpas_pool_get_array(diag_physics_cu,'qi_ktop_deepc',qi_ktopd)
 call mpas_pool_get_array(diag_physics_cu,'qs_ktop_deepc',qs_ktopd)
 call mpas_pool_get_array(diag_physics_cu,'qg_ktop_deepc',qg_ktopd)

 call mpas_pool_get_array(diag_physics_cu,'qc_above_conv',qc_aconv)
 call mpas_pool_get_array(diag_physics_cu,'qr_above_conv',qr_aconv)
 call mpas_pool_get_array(diag_physics_cu,'qi_above_conv',qi_aconv)
 call mpas_pool_get_array(diag_physics_cu,'qs_above_conv',qs_aconv)
 call mpas_pool_get_array(diag_physics_cu,'qg_above_conv',qg_aconv)

 call mpas_log_write(' ')
 call mpas_log_write('--- diagnostics_cu:')
 do iCell = 1, nCellsSolve
    qc_shalc(iCell) = 0._RKIND
    qc_midlc(iCell) = 0._RKIND
    qc_deepc(iCell) = 0._RKIND
    qr_shalc(iCell) = 0._RKIND
    qr_midlc(iCell) = 0._RKIND
    qr_deepc(iCell) = 0._RKIND
    qi_shalc(iCell) = 0._RKIND
    qi_midlc(iCell) = 0._RKIND
    qi_deepc(iCell) = 0._RKIND
    qs_shalc(iCell) = 0._RKIND
    qs_midlc(iCell) = 0._RKIND
    qs_deepc(iCell) = 0._RKIND
    qg_shalc(iCell) = 0._RKIND
    qg_midlc(iCell) = 0._RKIND
    qg_deepc(iCell) = 0._RKIND

    qc_kbcons(iCell) = 0._RKIND
    qc_kbconm(iCell) = 0._RKIND
    qc_kbcond(iCell) = 0._RKIND
    qr_kbcons(iCell) = 0._RKIND
    qr_kbconm(iCell) = 0._RKIND
    qr_kbcond(iCell) = 0._RKIND
    qi_kbcons(iCell) = 0._RKIND
    qi_kbconm(iCell) = 0._RKIND
    qi_kbcond(iCell) = 0._RKIND
    qs_kbcons(iCell) = 0._RKIND
    qs_kbconm(iCell) = 0._RKIND
    qs_kbcond(iCell) = 0._RKIND
    qg_kbcons(iCell) = 0._RKIND
    qg_kbconm(iCell) = 0._RKIND
    qg_kbcond(iCell) = 0._RKIND

    qc_ktops(iCell) = 0._RKIND
    qc_ktopm(iCell) = 0._RKIND
    qc_ktopd(iCell) = 0._RKIND
    qr_ktops(iCell) = 0._RKIND
    qr_ktopm(iCell) = 0._RKIND
    qr_ktopd(iCell) = 0._RKIND
    qi_ktops(iCell) = 0._RKIND
    qi_ktopm(iCell) = 0._RKIND
    qi_ktopd(iCell) = 0._RKIND
    qs_ktops(iCell) = 0._RKIND
    qs_ktopm(iCell) = 0._RKIND
    qs_ktopd(iCell) = 0._RKIND
    qg_ktops(iCell) = 0._RKIND
    qg_ktopm(iCell) = 0._RKIND
    qg_ktopd(iCell) = 0._RKIND

    qc_aconv(iCell) = 0._RKIND
    qr_aconv(iCell) = 0._RKIND
    qi_aconv(iCell) = 0._RKIND
    qs_aconv(iCell) = 0._RKIND
    qg_aconv(iCell) = 0._RKIND

    !--- cloud liquid water, rain, cloud ice, snow, and graupel paths between the base and top of
    !    shallow convection when there is shallow convection (otherwise, set to zero):
    if((need_cldp_kbcons.and.need_cldp_ktops) .or. (need_cldt_kbcons.and.need_cldt_kbcons) .or. &
       (need_cldh_kbcons.and.need_cldh_ktops)) then
       kb = kbcons(iCell)
       kt = ktops(iCell)
       if(kb.lt.nVertLevels .and. kt.gt.1) then
          do k = kb, kt
             qc_shalc(iCell) = qc_shalc(iCell) + qc(k,iCell)*rhoa(k,iCell)  
             qr_shalc(iCell) = qr_shalc(iCell) + qr(k,iCell)*rhoa(k,iCell)  
             qi_shalc(iCell) = qi_shalc(iCell) + qi(k,iCell)*rhoa(k,iCell)  
             qs_shalc(iCell) = qs_shalc(iCell) + qs(k,iCell)*rhoa(k,iCell)  
             qg_shalc(iCell) = qg_shalc(iCell) + qg(k,iCell)*rhoa(k,iCell)  
          enddo
       endif
    endif

    !--- cloud liquid water, rain, cloud ice, snow, and graupel paths between the base and top of
    !    mid-level convection when there is midl-level convection (otherwise set to zero):
    if((need_cldp_kbconm.and.need_cldp_ktopm) .or. (need_cldt_kbconm.and.need_cldt_kbconm) .or. &
       (need_cldh_kbconm.and.need_cldh_ktopm)) then
       kb = kbconm(iCell)
       kt = ktopm(iCell)
       if(kb.lt.nVertLevels .and. kt.gt.1) then
          do k = kb, kt
             qc_midlc(iCell) = qc_midlc(iCell) + qc(k,iCell)*rhoa(k,iCell)  
             qr_midlc(iCell) = qr_midlc(iCell) + qr(k,iCell)*rhoa(k,iCell)  
             qi_midlc(iCell) = qi_midlc(iCell) + qi(k,iCell)*rhoa(k,iCell)  
             qs_midlc(iCell) = qs_midlc(iCell) + qs(k,iCell)*rhoa(k,iCell)  
             qg_midlc(iCell) = qg_midlc(iCell) + qg(k,iCell)*rhoa(k,iCell)  
          enddo
       endif
    endif

    !--- cloud liquid water, rain, cloud ice, snow, and graupel paths between the base and top of
    !    deep-level convection when there is deep convection (otherwise set to zero):
    if((need_cldp_kbcond.and.need_cldp_ktopd) .or. (need_cldt_kbcond.and.need_cldt_kbcond) .or. &
       (need_cldh_kbcond.and.need_cldh_ktopd)) then
       kb = kbcond(iCell)
       kt = ktopd(iCell)
       if(kb.lt.nVertLevels .and. kt.gt.1) then
          do k = kb, kt
             qc_deepc(iCell) = qc_deepc(iCell) + qc(k,iCell)*rhoa(k,iCell)  
             qr_deepc(iCell) = qr_deepc(iCell) + qr(k,iCell)*rhoa(k,iCell)  
             qi_deepc(iCell) = qi_deepc(iCell) + qi(k,iCell)*rhoa(k,iCell)  
             qs_deepc(iCell) = qs_deepc(iCell) + qs(k,iCell)*rhoa(k,iCell)  
             qg_deepc(iCell) = qg_deepc(iCell) + qg(k,iCell)*rhoa(k,iCell)  
          enddo
       endif
    endif

    if(rinc_s(iCell).gt.0._RKIND .and. rinc_d(iCell).gt.0._RKIND) &
       call mpas_log_write('$i kbcons = $i ktops = $i lwp_shalc = $r $i kbcond = $i ktopd = $i lwp_deepcc = $r', &
                     intArgs=(/kbcons(iCell),ktops(iCell),kbcond(iCell),ktopd(iCell)/), &
                     realArgs=(/qc_shalc(iCell),qc_deepc(iCell)/))

    !--- cloud liquid water, rain, cloud ice, snow, and graupel paths between surface and cloud base
    !    for shallow convection:
    if(need_cldp_kbcons .or. need_cldt_kbcons .or. need_cldh_kbcons) then
!      kb = kpbl(iCell)
!      kc = kbcons(iCell)
!      if(kc.gt.0 .and. kc.lt.nVertLevels) kb = kc
!      if(kb.gt.0) then
!         do k = 1,kb
!            qc_kbcons(iCell) = qc_kbcons(iCell) + qc(k,iCell)*rhoa(k,iCell)  
!            qr_kbcons(iCell) = qr_kbcons(iCell) + qr(k,iCell)*rhoa(k,iCell)  
!            qi_kbcons(iCell) = qi_kbcons(iCell) + qi(k,iCell)*rhoa(k,iCell)  
!            qs_kbcons(iCell) = qs_kbcons(iCell) + qs(k,iCell)*rhoa(k,iCell)  
!            qg_kbcons(iCell) = qg_kbcons(iCell) + qg(k,iCell)*rhoa(k,iCell)  
!         enddo
!      endif
       kc = kbcons(iCell)
       if(kc.gt.0 .and. kc.lt.nVertLevels) then
          do k = 1, kc
             qc_kbcons(iCell) = qc_kbcons(iCell) + qc(k,iCell)*rhoa(k,iCell)  
             qr_kbcons(iCell) = qr_kbcons(iCell) + qr(k,iCell)*rhoa(k,iCell)  
             qi_kbcons(iCell) = qi_kbcons(iCell) + qi(k,iCell)*rhoa(k,iCell)  
             qs_kbcons(iCell) = qs_kbcons(iCell) + qs(k,iCell)*rhoa(k,iCell)  
             qg_kbcons(iCell) = qg_kbcons(iCell) + qg(k,iCell)*rhoa(k,iCell)  
          enddo
       endif
    endif

    !--- between surface and cloud base for mid-level convection:
    if(need_cldp_kbconm .or. need_cldt_kbconm .or. need_cldh_kbconm) then
!      kb = kpbl(iCell)
!      kc = kbconm(iCell)
!      if(kc.gt.0 .and. kc.lt.nVertLevels) kb = kc
!      if(kb.gt.0) then
!         do k = 1,kb
!            qc_kbconm(iCell) = qc_kbconm(iCell) + qc(k,iCell)*rhoa(k,iCell)  
!            qr_kbconm(iCell) = qr_kbconm(iCell) + qr(k,iCell)*rhoa(k,iCell)  
!            qi_kbconm(iCell) = qi_kbconm(iCell) + qi(k,iCell)*rhoa(k,iCell)  
!            qs_kbconm(iCell) = qs_kbconm(iCell) + qs(k,iCell)*rhoa(k,iCell)  
!            qg_kbconm(iCell) = qg_kbconm(iCell) + qg(k,iCell)*rhoa(k,iCell)  
!         enddo
!      endif
       kc = kbconm(iCell)
       if(kc.gt.0 .and. kc.lt.nVertLevels) then
          do k = 1, kc
             qc_kbconm(iCell) = qc_kbconm(iCell) + qc(k,iCell)*rhoa(k,iCell)  
             qr_kbconm(iCell) = qr_kbconm(iCell) + qr(k,iCell)*rhoa(k,iCell)  
             qi_kbconm(iCell) = qi_kbconm(iCell) + qi(k,iCell)*rhoa(k,iCell)  
             qs_kbconm(iCell) = qs_kbconm(iCell) + qs(k,iCell)*rhoa(k,iCell)  
             qg_kbconm(iCell) = qg_kbconm(iCell) + qg(k,iCell)*rhoa(k,iCell)  
          enddo
       endif
    endif

    !--- between surface and cloud base for deep convection:
    if(need_cldp_kbcond .or. need_cldt_kbcond .or. need_cldh_kbcond) then
!      kb = kpbl(iCell)
!      kc = kbcond(iCell)
!      if(kc.gt.0 .and. kc.lt.nVertLevels) kb = kc
!      if(kb.gt.0) then
!         do k = 1,kb
!            qc_kbcond(iCell) = qc_kbcond(iCell) + qc(k,iCell)*rhoa(k,iCell)  
!            qr_kbcond(iCell) = qr_kbcond(iCell) + qr(k,iCell)*rhoa(k,iCell)  
!            qi_kbcond(iCell) = qi_kbcond(iCell) + qi(k,iCell)*rhoa(k,iCell)  
!            qs_kbcond(iCell) = qs_kbcond(iCell) + qs(k,iCell)*rhoa(k,iCell)  
!            qg_kbcond(iCell) = qg_kbcond(iCell) + qg(k,iCell)*rhoa(k,iCell)  
!         enddo
!      endif
       kc = kbcond(iCell)
       if(kc.gt.0 .and. kc.lt.nVertLevels) then
          do k = 1,kc
             qc_kbcond(iCell) = qc_kbcond(iCell) + qc(k,iCell)*rhoa(k,iCell)
             qr_kbcond(iCell) = qr_kbcond(iCell) + qr(k,iCell)*rhoa(k,iCell)
             qi_kbcond(iCell) = qi_kbcond(iCell) + qi(k,iCell)*rhoa(k,iCell)
             qs_kbcond(iCell) = qs_kbcond(iCell) + qs(k,iCell)*rhoa(k,iCell)
             qg_kbcond(iCell) = qg_kbcond(iCell) + qg(k,iCell)*rhoa(k,iCell)
          enddo
       endif
    endif

    !--- between surface and cloud top for shallow convection:
    if(need_cldp_ktops .or. need_cldt_ktops .or. need_cldh_ktops) then
!      kt = kpbl(iCell)
!      kc = ktops(iCell)
!      if(kc.gt.1) kt = kc
!      if(kt.gt.0) then
!         do k = 1,kt
!            qc_ktops(iCell) = qc_ktops(iCell) + qc(k,iCell)*rhoa(k,iCell)
!            qr_ktops(iCell) = qr_ktops(iCell) + qr(k,iCell)*rhoa(k,iCell)
!            qi_ktops(iCell) = qi_ktops(iCell) + qi(k,iCell)*rhoa(k,iCell)
!            qs_ktops(iCell) = qs_ktops(iCell) + qs(k,iCell)*rhoa(k,iCell)
!            qg_ktops(iCell) = qg_ktops(iCell) + qg(k,iCell)*rhoa(k,iCell)
!         enddo
!      endif
       kc = ktops(iCell)
       if(kc.gt.1) then
          do k = 1, kc
             qc_ktops(iCell) = qc_ktops(iCell) + qc(k,iCell)*rhoa(k,iCell)
             qr_ktops(iCell) = qr_ktops(iCell) + qr(k,iCell)*rhoa(k,iCell)
             qi_ktops(iCell) = qi_ktops(iCell) + qi(k,iCell)*rhoa(k,iCell)
             qs_ktops(iCell) = qs_ktops(iCell) + qs(k,iCell)*rhoa(k,iCell)
             qg_ktops(iCell) = qg_ktops(iCell) + qg(k,iCell)*rhoa(k,iCell)
          enddo
       endif
    endif

    !--- between surface and cloud top for mid-level convection:
    if(need_cldp_ktopm .or. need_cldt_ktopm .or. need_cldh_ktopm) then
!      kt = kpbl(iCell)
!      kc = ktopm(iCell)
!      if(kc.gt.1) kt = kc
!      if(kt.gt.0) then
!         do k = 1,kt
!            qc_ktopm(iCell) = qc_ktopm(iCell) + qc(k,iCell)*rhoa(k,iCell)
!            qr_ktopm(iCell) = qr_ktopm(iCell) + qr(k,iCell)*rhoa(k,iCell)
!            qi_ktopm(iCell) = qi_ktopm(iCell) + qi(k,iCell)*rhoa(k,iCell)
!            qs_ktopm(iCell) = qs_ktopm(iCell) + qs(k,iCell)*rhoa(k,iCell)
!            qg_ktopm(iCell) = qg_ktopm(iCell) + qg(k,iCell)*rhoa(k,iCell)
!         enddo
!      endif
       kc = ktopm(iCell)
       if(kc.gt.1) then
          do k = 1, kc
             qc_ktopm(iCell) = qc_ktopm(iCell) + qc(k,iCell)*rhoa(k,iCell)
             qr_ktopm(iCell) = qr_ktopm(iCell) + qr(k,iCell)*rhoa(k,iCell)
             qi_ktopm(iCell) = qi_ktopm(iCell) + qi(k,iCell)*rhoa(k,iCell)
             qs_ktopm(iCell) = qs_ktopm(iCell) + qs(k,iCell)*rhoa(k,iCell)
             qg_ktopm(iCell) = qg_ktopm(iCell) + qg(k,iCell)*rhoa(k,iCell)
          enddo
       endif
    endif

    !--- cloud liquid water, rain, cloud ice, snow, and graupel path between surface and cloud top
    !    for deep convection (if no deep convection, the cloud-top index is the maximum value between
    !    the PBL top and cloud-tops for shallow and mid-level convection):
    if(need_cldp_ktopd .or. need_cldt_ktopd .or. need_cldh_ktopd) then
!      kt = kpbl(iCell)
!      kc = ktopd(iCell)
!      if(kc.gt.1) kt = kc
!      if(kt.gt.0) then
!         do k = 1,kt
!            qc_ktopd(iCell) = qc_ktopd(iCell) + qc(k,iCell)*rhoa(k,iCell)
!            qr_ktopd(iCell) = qr_ktopd(iCell) + qr(k,iCell)*rhoa(k,iCell)
!            qi_ktopd(iCell) = qi_ktopd(iCell) + qi(k,iCell)*rhoa(k,iCell)
!            qs_ktopd(iCell) = qs_ktopd(iCell) + qs(k,iCell)*rhoa(k,iCell)
!            qg_ktopd(iCell) = qg_ktopd(iCell) + qg(k,iCell)*rhoa(k,iCell)
!         enddo
!      endif
       kc = ktopd(iCell)
       if(kc.gt.1) then
          do k = 1, kc
             qc_ktopd(iCell) = qc_ktopd(iCell) + qc(k,iCell)*rhoa(k,iCell)
             qr_ktopd(iCell) = qr_ktopd(iCell) + qr(k,iCell)*rhoa(k,iCell)
             qi_ktopd(iCell) = qi_ktopd(iCell) + qi(k,iCell)*rhoa(k,iCell)
             qs_ktopd(iCell) = qs_ktopd(iCell) + qs(k,iCell)*rhoa(k,iCell)
             qg_ktopd(iCell) = qg_ktopd(iCell) + qg(k,iCell)*rhoa(k,iCell)
          enddo
       endif
    endif

    !--- between PBL top or convective (shallow,mid-level,deep) cloud top and the
    !    top of the atmosphere:
    kt = max(kpbl(iCell),ktops(iCell))
    kt = max(kt,ktopm(iCell))
    kt = max(kt,ktopd(iCell))
    if(kt.gt.1) then
       do k = kt,nVertLevels
          qc_aconv(iCell) = qc_aconv(iCell) + qc(k,iCell)*rhoa(k,iCell)
          qr_aconv(iCell) = qr_aconv(iCell) + qr(k,iCell)*rhoa(k,iCell)
          qi_aconv(iCell) = qi_aconv(iCell) + qi(k,iCell)*rhoa(k,iCell)
          qs_aconv(iCell) = qs_aconv(iCell) + qs(k,iCell)*rhoa(k,iCell)
          qg_aconv(iCell) = qg_aconv(iCell) + qg(k,iCell)*rhoa(k,iCell)
       enddo
    endif
 enddo
 call mpas_log_write('--- end compute qg_aconv:')

!--- calculation of the in-cloud convection cloud water and cloud ice paths (total and shallow convection):
 call mpas_pool_get_array(diag_physics,'qc_cu',qc_cu)
 call mpas_pool_get_array(diag_physics,'qi_cu',qi_cu)
 call mpas_pool_get_array(diag_physics,'qc_cu_shalc',qc_cu_shalc)
 call mpas_pool_get_array(diag_physics,'qi_cu_shalc',qi_cu_shalc)

 call mpas_pool_get_array(diag_physics_cu,'qccu_vavg',qccu_vavg)
 call mpas_pool_get_array(diag_physics_cu,'qicu_vavg',qicu_vavg)
 call mpas_pool_get_array(diag_physics_cu,'qccu_shalc_vavg',qccu_shalc_vavg)
 call mpas_pool_get_array(diag_physics_cu,'qicu_shalc_vavg',qicu_shalc_vavg)

 do iCell = 1, nCellsSolve
    qccu_vavg(iCell)       = 0._RKIND
    qicu_vavg(iCell)       = 0._RKIND
    qccu_shalc_vavg(iCell) = 0._RKIND
    qicu_shalc_vavg(iCell) = 0._RKIND

    do k = 1,nVertLevels
       qccu_vavg(iCell) = qccu_vavg(iCell) + qc_cu(k,iCell)*rhoa(k,iCell)
       qicu_vavg(iCell) = qicu_vavg(iCell) + qi_cu(k,iCell)*rhoa(k,iCell)
       qccu_shalc_vavg(iCell) = qccu_shalc_vavg(iCell) + qc_cu_shalc(k,iCell)*rhoa(k,iCell)
       qicu_shalc_vavg(iCell) = qicu_shalc_vavg(iCell) + qi_cu_shalc(k,iCell)*rhoa(k,iCell)
    enddo
 enddo

 if(allocated(dz)    ) deallocate(dz    )
 if(allocated(press) ) deallocate(press )
 if(allocated(rhoa)  ) deallocate(rhoa  )
 if(allocated(temp)  ) deallocate(temp  )
 if(allocated(zgridl)) deallocate(zgridl)
 call mpas_log_write('--- end compute qicu_shalc:')

!--- interpolation of the shallow, mid-level, and deep convection mass fluxes to fixed pressure levels:
 call mpas_pool_get_dimension(mesh,'nFGLevels',nFGLevels)
 call mpas_pool_get_array(fg,'p_fg',p_fg)

 call mpas_pool_get_array(diag_physics,'zu_shalc',zu_shalc)
 call mpas_pool_get_array(diag_physics,'zd_shalc',zd_shalc)
 call mpas_pool_get_array(diag_physics,'zu_midlc',zu_midlc)
 call mpas_pool_get_array(diag_physics,'zd_midlc',zd_midlc)
 call mpas_pool_get_array(diag_physics,'zu_deepc',zu_deepc)
 call mpas_pool_get_array(diag_physics,'zd_deepc',zd_deepc)
 call mpas_log_write('--- end get_array zd_deepc:')

 nullify(zu_shalc_interp)
 nullify(zd_shalc_interp)
 nullify(zu_midlc_interp)
 nullify(zd_midlc_interp)
 nullify(zu_deepc_interp)
 nullify(zd_deepc_interp)
 call mpas_pool_get_array(diag_physics_cu,'zu_shalc_interp',zu_shalc_interp)
 call mpas_pool_get_array(diag_physics_cu,'zd_shalc_interp',zd_shalc_interp)
 call mpas_pool_get_array(diag_physics_cu,'zu_midlc_interp',zu_midlc_interp)
 call mpas_pool_get_array(diag_physics_cu,'zd_midlc_interp',zd_midlc_interp)
 call mpas_pool_get_array(diag_physics_cu,'zu_deepc_interp',zu_deepc_interp)
 call mpas_pool_get_array(diag_physics_cu,'zd_deepc_interp',zd_deepc_interp)
 call mpas_log_write('--- end get_array zd_deepc_interp:')

 nIntP = -1
 iCell = 1
 do k = 1, nFGLevels-1
    if(p_fg(k,iCell) .ge. 1000._RKIND) nIntP = k
 enddo
 call mpas_log_write('--- nIntP = $i',intArgs=(/nIntP/))

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

!--- zu_shalc_interp:
 if(need_zu_shalc_interp) then
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = zu_shalc(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          zu_shalc_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
    call mpas_log_write('--- end interpolate zu_shalc:')
 endif

!--- zd_shalc_interp:
 if(need_zd_shalc_interp) then
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = zd_shalc(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          zd_shalc_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
    call mpas_log_write('--- end interpolate zd_shalc:')
 endif

!--- zu_midlc_interp:
 if(need_zu_midlc_interp) then
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = zu_midlc(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          zu_midlc_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
    call mpas_log_write('--- end interpolate zu_midlc:')
 endif

!--- zd_midlc_interp:
 if(need_zd_midlc_interp) then
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = zd_midlc(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          zd_midlc_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
    call mpas_log_write('--- end interpolate zd_midlc:')
 endif

!--- zu_deepc_interp:
 if(need_zu_deepc_interp) then
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = zu_deepc(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          zu_deepc_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
    call mpas_log_write('--- end interpolate zu_deepc:')
 endif

!--- zd_deepc_interp:
 if(need_zd_deepc_interp) then
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = zd_deepc(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          zd_deepc_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
    call mpas_log_write('--- end interpolate zd_deepc:')
 endif

!--- deallocate local arrays:
 if(allocated(press_in)) deallocate(press_in)
 if(allocated(field_in)) deallocate(field_in)
 if(allocated(field_interp)) deallocate(field_interp)
 if(allocated(press_interp)) deallocate(press_interp)

!call mpas_pool_get_array(diag_physics,'raincv',raincv)
!call mpas_log_write('--- nFGLevels = $i',intArgs=(/nFGLevels/))

!do iCell = 1, nCellsSolve
!   if(raincv(iCell) .gt. 0._RKIND) then
!      do k = 1, nIntP
!         call mpas_log_write('$i $i $r $r',intArgs=(/iCell,k/),realArgs=(/p_fg(
!call mpas_log_write(' ')
 

 call mpas_log_write('--- exit subroutine atmphys_compute_diagnostics_cu:')

 end subroutine atmphys_compute_diagnostics_cu

!=================================================================================================================
 subroutine atmphys_compute_tendencies_cu(mesh,fg,state,time_lev,diag,tend_physics,tend_physics_cu)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: fg
 type(mpas_pool_type),intent(in):: state
 type(mpas_pool_type),intent(in):: diag
 type(mpas_pool_type),intent(in):: tend_physics
 integer,intent(in):: time_lev

!inout arguments:
 type(mpas_pool_type),intent(inout):: tend_physics_cu

!local variables, arrays, and pointers:
 integer:: iCell,k,kk,nIntP
 integer,pointer:: nCellsSolve,nFGLevels,nVertLevels
 integer,pointer:: index_qv

 real:: dz,rhoa,rhoat,sumr
 real(kind=RKIND),dimension(:),pointer    :: dtcups_vavg,dqvcups_vavg,dqccups_vavg,dqrcups_vavg, &
                                             dqicups_vavg,dqscups_vavg
 real(kind=RKIND),dimension(:,:),pointer  :: p_fg
 real(kind=RKIND),dimension(:,:),pointer  :: pressure_b,pressure_p,exner
 real(kind=RKIND),dimension(:,:),pointer  :: qv,rho_zz,zgrid,zz
 real(kind=RKIND),dimension(:,:),pointer  :: dtcups,dqvcups,dqccups,dqrcups,dqicups,dqscups
 real(kind=RKIND),dimension(:,:),pointer  :: dtcups_interp,dqvcups_interp,dqccups_interp,dqrcups_interp, &
                                             dqicups_interp,dqscups_interp
 real(kind=RKIND),dimension(:,:,:),pointer:: scalars

!local interpolated fields:
 real(kind=RKIND),dimension(:,:),allocatable:: field_in,press_in
 real(kind=RKIND),dimension(:,:),allocatable:: field_interp,press_interp

!-----------------------------------------------------------------------------------------------------------------
 call mpas_log_write(' ')
 call mpas_log_write('--- enter subroutine atmphys_compute_tendencies_cu:')

 call mpas_pool_get_dimension(mesh,'nCellsSolve',nCellsSolve)
 call mpas_pool_get_dimension(mesh,'nFGLevels'  ,nFGLevels  )
 call mpas_pool_get_dimension(mesh,'nVertLevels',nVertLevels)

 call mpas_pool_get_dimension(state,'index_qv',index_qv)

 call mpas_pool_get_array(mesh,'zz'   ,zz   )
 call mpas_pool_get_array(mesh,'zgrid',zgrid)

 call mpas_pool_get_array(fg,'p_fg',p_fg)
 call mpas_pool_get_array(diag,'pressure_base',pressure_b)
 call mpas_pool_get_array(diag,'pressure_p'   ,pressure_p)
 call mpas_pool_get_array(diag,'exner'        ,exner     )

 call mpas_pool_get_array(state,'rho_zz' ,rho_zz ,time_lev)
 call mpas_pool_get_array(state,'scalars',scalars,time_lev)
 qv => scalars(index_qv,:,:)

 nullify(dtcups)
 nullify(dqvcups)
 nullify(dqccups)
 nullify(dqrcups)
 nullify(dqicups)
 nullify(dqscups)
 call mpas_pool_get_array(tend_physics,'rthcuten_shalc',dtcups )
 call mpas_pool_get_array(tend_physics,'rqvcuten_shalc',dqvcups)
 call mpas_pool_get_array(tend_physics,'rqccuten_shalc',dqccups)
 call mpas_pool_get_array(tend_physics,'rqicuten_shalc',dqicups)
 call mpas_pool_get_array(tend_physics,'rqrcuten',dqrcups)
 call mpas_pool_get_array(tend_physics,'rqscuten',dqscups)

 nullify(dtcups_vavg)
 nullify(dqvcups_vavg)
 nullify(dqccups_vavg)
 nullify(dqrcups_vavg)
 nullify(dqicups_vavg)
 nullify(dqscups_vavg)
 call mpas_pool_get_array(tend_physics_cu,'dtcups_vavg' ,dtcups_vavg )
 call mpas_pool_get_array(tend_physics_cu,'dqvcups_vavg',dqvcups_vavg)
 call mpas_pool_get_array(tend_physics_cu,'dqccups_vavg',dqccups_vavg)
 call mpas_pool_get_array(tend_physics_cu,'dqrcups_vavg',dqrcups_vavg)
 call mpas_pool_get_array(tend_physics_cu,'dqicups_vavg',dqicups_vavg)
 call mpas_pool_get_array(tend_physics_cu,'dqscups_vavg',dqscups_vavg)

 nullify(dtcups_interp)
 nullify(dqvcups_interp)
 nullify(dqccups_interp)
 nullify(dqrcups_interp)
 nullify(dqicups_interp)
 nullify(dqscups_interp)
 call mpas_pool_get_array(tend_physics_cu,'dtcups_interp' ,dtcups_interp )
 call mpas_pool_get_array(tend_physics_cu,'dqvcups_interp',dqvcups_interp)
 call mpas_pool_get_array(tend_physics_cu,'dqccups_interp',dqccups_interp)
 call mpas_pool_get_array(tend_physics_cu,'dqrcups_interp',dqrcups_interp)
 call mpas_pool_get_array(tend_physics_cu,'dqicups_interp',dqicups_interp)
 call mpas_pool_get_array(tend_physics_cu,'dqscups_interp',dqscups_interp)


!--- calculation of vertically-averaged convective tendencies due to shallow convection:
 if(associated(dtcups)) then
    do iCell = 1, nCellsSolve
       dtcups_vavg(iCell)  = 0._RKIND
       dqvcups_vavg(iCell) = 0._RKIND
       dqccups_vavg(iCell) = 0._RKIND
       dqicups_vavg(iCell) = 0._RKIND

       sumr = 0._RKIND
       do k = 1, nVertLevels
          dz    = zgrid(k+1,iCell)-zgrid(k,iCell)
          rhoa  = zz(k,iCell)*rho_zz(k,iCell)
          rhoa  = rhoa*(1._RKIND+qv(k,iCell))
          rhoat = rhoa*exner(k,iCell)
          sumr  = sumr+rhoa*dz

          !--- vertically-averaged convective tendencies:
          dtcups_vavg(iCell)  = dtcups_vavg(iCell)  + dtcups(k,iCell)*rhoat*dz
          dqvcups_vavg(iCell) = dqvcups_vavg(iCell) + dqvcups(k,iCell)*rhoa*dz
          dqccups_vavg(iCell) = dqccups_vavg(iCell) + dqccups(k,iCell)*rhoa*dz
          dqicups_vavg(iCell) = dqicups_vavg(iCell) + dqicups(k,iCell)*rhoa*dz
       enddo
       dtcups_vavg(iCell) = dtcups_vavg(iCell)/sumr

       if(associated(dqrcups)) then
          dqrcups_vavg(iCell) = 0._RKIND
          dqscups_vavg(iCell) = 0._RKIND

          do k = 1, nVertLevels
             dz   = zgrid(k+1,iCell)-zgrid(k,iCell)
             rhoa = zz(k,iCell)*rho_zz(k,iCell)
             rhoa = rhoa*(1._RKIND+qv(k,iCell))

             !--- vertically-averaged convective tendencies:
             dqrcups_vavg(iCell) = dqrcups_vavg(iCell) + dqrcups(k,iCell)*rhoa*dz
             dqscups_vavg(iCell) = dqscups_vavg(iCell) + dqscups(k,iCell)*rhoa*dz
          enddo
       endif
    enddo
 endif


!--- interpolate convective tendencies due to shallow convection to fixed pressure levels: every
!    parameterization of convection outputs tendencies for the temperature,and water vapor,cloud
!    liquid water,and cloud ice mixing ratios. the Kain-Fritsch parameterization also outputs
!    tendencies for rain and snow.
 nIntP = -1
 iCell = 1
 do k = 1, nFGLevels-1
    if(p_fg(k,iCell) .ge. 1000._RKIND) nIntP = k
 enddo
 call mpas_log_write('--- nIntP = $i',intArgs=(/nIntP/))

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

!iCell = nCellsSolve
!do k = 1, nVertLevels
!   call mpas_log_write('$i $i $r',intArgs=(/iCell,k/),realArgs=(/press_in(iCell,k)/))
!enddo
!call mpas_log_write(' ')
!do k = 1, nIntP
!   call mpas_log_write('$i $i $r',intArgs=(/iCell,k/),realArgs=(/press_interp(iCell,k)/))
!enddo
!call mpas_log_write(' ')

 if(associated(dtcups)) then
    !--- dtcups_interp:
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = dtcups(k,iCell)*exner(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          dtcups_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
!   do iCell = 1, nCellsSolve
!      if(dtcups_vavg(iCell).gt.0._RKIND) then
!      do k = 1, nFGLevels
!         call mpas_log_write('$i $i $r $r',intArgs=(/iCell,k/),realArgs=(/p_fg(k,iCell), &
!                             dtcups_interp(k,iCell)/))
!      enddo
!      call mpas_log_write(' ')
!      endif
!   enddo
!   call mpas_log_write('--- end interpolate dtcups:')

    !--- dqvcups_interp:
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = dqvcups(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          dqvcups_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
!   call mpas_log_write('--- end interpolate dqvcups:')

    !--- dqccups_interp:
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = dqccups(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          dqccups_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
!   call mpas_log_write('--- end interpolate dqccups:')

    !--- dqicups_interp:
    do k = 1, nVertLevels
       kk = nVertLevels+1-k
       do iCell = 1, nCellsSolve
          field_in(iCell,kk) = dqicups(k,iCell)
       enddo
    enddo
    call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
    do k = 1, nIntP
       kk = nIntp+1-k
       do iCell = 1, nCellsSolve
          dqicups_interp(k,iCell) = field_interp(iCell,kk)
       enddo
    enddo
!   call mpas_log_write('--- end interpolate dqicups:')

    if(associated(dqrcups)) then
       !--- dqrcups_interp:
       do k = 1, nVertLevels
          kk = nVertLevels+1-k
          do iCell = 1, nCellsSolve
             field_in(iCell,kk) = dqrcups(k,iCell)
          enddo
       enddo
       call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
       do k = 1, nIntP
          kk = nIntp+1-k
          do iCell = 1, nCellsSolve
             dqrcups_interp(k,iCell) = field_interp(iCell,kk)
          enddo
       enddo
!      call mpas_log_write('--- end interpolate dqrcups:')

       !--- dqscups_interp:
       do k = 1, nVertLevels
          kk = nVertLevels+1-k
          do iCell = 1, nCellsSolve
             field_in(iCell,kk) = dqscups(k,iCell)
          enddo
       enddo
       call interp_tofixed_pressure(nCellsSolve,nVertLevels,nIntP,press_in,field_in,press_interp,field_interp)
       do k = 1, nIntP
          kk = nIntp+1-k
          do iCell = 1, nCellsSolve
             dqscups_interp(k,iCell) = field_interp(iCell,kk)
          enddo
       enddo
!      call mpas_log_write('--- end interpolate dqscups:')
    else
       do k = 1, nFGLevels
          do iCell = 1, nCellsSolve
             dqrcups_interp(k,iCell) = 0._RKIND
             dqscups_interp(k,iCell) = 0._RKIND
          enddo
       enddo
    endif
 else
    do k = 1, nFGLevels
       do iCell = 1, nCellsSolve
          dtcups_interp(k,iCell)  = 0._RKIND
          dqvcups_interp(k,iCell) = 0._RKIND
          dqccups_interp(k,iCell) = 0._RKIND
          dqicups_interp(k,iCell) = 0._RKIND
       enddo
    enddo
 endif

 if(allocated(press_in)) deallocate(press_in)
 if(allocated(field_in)) deallocate(field_in)
 if(allocated(press_interp)) deallocate(press_interp)
 if(allocated(field_interp)) deallocate(field_interp)


!call mpas_log_write('--- end subroutine atmphys_compute_tendencies_cu:')
!call mpas_log_write(' ')

 end subroutine atmphys_compute_tendencies_cu

!=================================================================================================================
 function need_atmphys_diagnostics_cu() result(need_any_diags)
!=================================================================================================================

!local variables:
 logical:: need_any_diags

!-----------------------------------------------------------------------------------------------------------------
 need_any_diags = .false.

 need_cldp_k22s = .false. ; need_cldt_k22s = .false. ; need_cldh_k22s = .false.
 need_cldp_k22m = .false. ; need_cldt_k22m = .false. ; need_cldh_k22m = .false.
 need_cldp_k22d = .false. ; need_cldt_k22d = .false. ; need_cldh_k22d = .false.

 need_cldp_kbcons = .false. ; need_cldt_kbcons = .false. ; need_cldh_kbcons = .false.
 need_cldp_kbconm = .false. ; need_cldt_kbconm = .false. ; need_cldh_kbconm = .false.
 need_cldp_kbcond = .false. ; need_cldt_kbcond = .false. ; need_cldh_kbcond = .false.

 need_cldp_ktops = .false. ; need_cldt_ktops = .false. ; need_cldh_ktops = .false.
 need_cldp_ktopm = .false. ; need_cldt_ktopm = .false. ; need_cldh_ktopm = .false.
 need_cldp_ktopd = .false. ; need_cldt_ktopd = .false. ; need_cldh_ktopd = .false.

 need_zu_shalc_interp = .false. ; need_zd_shalc_interp = .false.
 need_zu_midlc_interp = .false. ; need_zd_midlc_interp = .false.
 need_zu_deepc_interp = .false. ; need_zd_deepc_interp = .false.


 need_cldp_k22s   = mpas_field_will_be_written('cldp_k22s') 
 need_any_diags   = need_any_diags .or. need_cldp_k22s
 need_cldp_k22m   = mpas_field_will_be_written('cldp_k22m') 
 need_any_diags   = need_any_diags .or. need_cldp_k22m
 need_cldp_k22d   = mpas_field_will_be_written('cldp_k22d') 
 need_any_diags   = need_any_diags .or. need_cldp_k22d
 need_cldt_k22s   = mpas_field_will_be_written('cldt_k22s') 
 need_any_diags   = need_any_diags .or. need_cldt_k22s
 need_cldt_k22m   = mpas_field_will_be_written('cldt_k22m') 
 need_any_diags   = need_any_diags .or. need_cldt_k22m
 need_cldt_k22d   = mpas_field_will_be_written('cldt_k22d') 
 need_any_diags   = need_any_diags .or. need_cldt_k22d
 need_cldh_k22s   = mpas_field_will_be_written('cldh_k22s') 
 need_any_diags   = need_any_diags .or. need_cldh_k22s
 need_cldh_k22m   = mpas_field_will_be_written('cldh_k22m') 
 need_any_diags   = need_any_diags .or. need_cldh_k22m
 need_cldh_k22d   = mpas_field_will_be_written('cldh_k22d') 
 need_any_diags   = need_any_diags .or. need_cldh_k22d

 need_cldp_kbcons = mpas_field_will_be_written('cldp_kbcons') 
 need_any_diags   = need_any_diags .or. need_cldp_kbcons
 need_cldp_kbconm = mpas_field_will_be_written('cldp_kbconm') 
 need_any_diags   = need_any_diags .or. need_cldp_kbconm
 need_cldp_kbcond = mpas_field_will_be_written('cldp_kbcond') 
 need_any_diags   = need_any_diags .or. need_cldp_kbcond
 need_cldt_kbcons = mpas_field_will_be_written('cldt_kbcons') 
 need_any_diags   = need_any_diags .or. need_cldt_kbcons
 need_cldt_kbconm = mpas_field_will_be_written('cldt_kbconm') 
 need_any_diags   = need_any_diags .or. need_cldt_kbconm
 need_cldt_kbcond = mpas_field_will_be_written('cldt_kbcond') 
 need_any_diags   = need_any_diags .or. need_cldt_kbcond
 need_cldh_kbcons = mpas_field_will_be_written('cldh_kbcons') 
 need_any_diags   = need_any_diags .or. need_cldh_kbcons
 need_cldh_kbconm = mpas_field_will_be_written('cldh_kbconm') 
 need_any_diags   = need_any_diags .or. need_cldh_kbconm
 need_cldh_kbcond = mpas_field_will_be_written('cldh_kbcond') 
 need_any_diags   = need_any_diags .or. need_cldh_kbcond

 need_cldp_ktops  = mpas_field_will_be_written('cldp_ktops') 
 need_any_diags   = need_any_diags .or. need_cldp_ktops
 need_cldp_ktopm  = mpas_field_will_be_written('cldp_ktopm') 
 need_any_diags   = need_any_diags .or. need_cldp_ktopm
 need_cldp_ktopd  = mpas_field_will_be_written('cldp_ktopd') 
 need_any_diags   = need_any_diags .or. need_cldp_ktopd
 need_cldt_ktops  = mpas_field_will_be_written('cldt_ktops') 
 need_any_diags   = need_any_diags .or. need_cldt_ktops
 need_cldt_ktopm  = mpas_field_will_be_written('cldt_ktopm') 
 need_any_diags   = need_any_diags .or. need_cldt_ktopm
 need_cldt_ktopd  = mpas_field_will_be_written('cldt_ktopd') 
 need_any_diags   = need_any_diags .or. need_cldt_ktopd
 need_cldh_ktops  = mpas_field_will_be_written('cldh_ktops') 
 need_any_diags   = need_any_diags .or. need_cldh_ktops
 need_cldh_ktopm  = mpas_field_will_be_written('cldh_ktopm') 
 need_any_diags   = need_any_diags .or. need_cldh_ktopm
 need_cldh_ktopd  = mpas_field_will_be_written('cldh_ktopd') 
 need_any_diags   = need_any_diags .or. need_cldh_ktopd

 need_zu_shalc_interp = mpas_field_will_be_written('zu_shalc_interp')
 need_any_diags       = need_any_diags .or. need_zu_shalc_interp
 need_zd_shalc_interp = mpas_field_will_be_written('zd_shalc_interp')
 need_any_diags       = need_any_diags .or. need_zd_shalc_interp
 need_zu_midlc_interp = mpas_field_will_be_written('zu_midlc_interp')
 need_any_diags       = need_any_diags .or. need_zu_midlc_interp
 need_zd_midlc_interp = mpas_field_will_be_written('zd_midlc_interp')
 need_any_diags       = need_any_diags .or. need_zd_midlc_interp
 need_zu_deepc_interp = mpas_field_will_be_written('zu_deepc_interp')
 need_any_diags       = need_any_diags .or. need_zu_deepc_interp
 need_zd_deepc_interp = mpas_field_will_be_written('zd_deepc_interp')
 need_any_diags       = need_any_diags .or. need_zd_deepc_interp

 end function need_atmphys_diagnostics_cu

!=================================================================================================================
 function need_atmphys_tendencies_cu() result(need_any_diags)
!=================================================================================================================

!local variables:
 logical:: need_any_diags

!-----------------------------------------------------------------------------------------------------------------
 need_any_diags = .false.

 need_dtcups_vavg = .false.  ; need_dqvcups_vavg = .false. ; need_dqccups_vavg = .false.
 need_dqicups_vavg = .false. ; need_dqrcups_vavg = .false. ; need_dqscups_vavg = .false.

 need_dtcups_vavg = mpas_field_will_be_written('dtcups_vavg') 
 need_any_diags   = need_any_diags .or. need_dtcups_vavg
 need_dqvcups_vavg = mpas_field_will_be_written('dqvcups_vavg') 
 need_any_diags    = need_any_diags .or. need_dqvcups_vavg
 need_dqccups_vavg = mpas_field_will_be_written('dqccups_vavg') 
 need_any_diags    = need_any_diags .or. need_dqccups_vavg
 need_dqrcups_vavg = mpas_field_will_be_written('dqrcups_vavg') 
 need_any_diags    = need_any_diags .or. need_dqrcups_vavg
 need_dqicups_vavg = mpas_field_will_be_written('dqicups_vavg') 
 need_any_diags    = need_any_diags .or. need_dqicups_vavg
 need_dqscups_vavg = mpas_field_will_be_written('dqscups_vavg') 
 need_any_diags    = need_any_diags .or. need_dqscups_vavg

 end function need_atmphys_tendencies_cu

!=================================================================================================================
 subroutine interp_tofixed_pressure(ncol,nlev_in,nlev_out,pres_in,field_in,pres_out,field_out)
!=================================================================================================================

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
 end module mpas_atmphys_compute_diagnostics_cu
!=================================================================================================================
