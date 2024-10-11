! Copyright (c) 2024 The University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module mpas_atmphys_sfc_diagnostics
 use mpas_kind_types,only: RKIND,StrKIND
 use mpas_derived_types,only: mpas_pool_type
 use mpas_log,only: mpas_log_write
 use mpas_pool_routines,only: mpas_pool_get_config,mpas_pool_get_dimension,mpas_pool_get_array

 use mpas_atmphys_constants,only: cp,P0,R_d,rcp
 use mpas_atmphys_vars,only: xice_threshold


 implicit none
 private
 public:: atmphys_sfc_diagnostics


 contains


!=================================================================================================================
 subroutine atmphys_sfc_diagnostics(configs,mesh,diag,diag_physics,sfc_input,output_noahmp,its,ite)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: diag
 type(mpas_pool_type),intent(in):: output_noahmp
 type(mpas_pool_type),intent(in):: sfc_input
 integer,intent(in):: its,ite

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics

!local variables and pointers:
 character(len=StrKIND),pointer:: lsm_scheme

 integer,pointer:: nCellsSolve
 integer:: i

 real(kind=RKIND),dimension(:),pointer:: psfc
 real(kind=RKIND),dimension(:),pointer:: tsk,xice,xland
 real(kind=RKIND),dimension(:),pointer:: hfx,qfx,qsfc,chs2,cqs2
 real(kind=RKIND),dimension(:),pointer:: q2mxy,t2mxy
 real(kind=RKIND),dimension(:),pointer:: q2,t2m,th2m

 real(kind=RKIND):: rho

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write(' ')
!call mpas_log_write('--- enter subroutine atmphys_sfc_diagnostics:')

 call mpas_pool_get_config(configs,'config_lsm_scheme',lsm_scheme)

 call mpas_pool_get_dimension(mesh,'nCellsSolve',nCellsSolve)

 call mpas_pool_get_array(diag,'surface_pressure',psfc)

 call mpas_pool_get_array(diag_physics,'chs2',chs2)
 call mpas_pool_get_array(diag_physics,'cqs2',cqs2)
 call mpas_pool_get_array(diag_physics,'hfx' ,hfx )
 call mpas_pool_get_array(diag_physics,'qfx' ,qfx )
 call mpas_pool_get_array(diag_physics,'qsfc',qsfc)
 call mpas_pool_get_array(diag_physics,'q2'  ,q2  )
 call mpas_pool_get_array(diag_physics,'t2m' ,t2m )
 call mpas_pool_get_array(diag_physics,'th2m',th2m)

 call mpas_pool_get_array(sfc_input,'skintemp',tsk  )
 call mpas_pool_get_array(sfc_input,'xice'    ,xice )
 call mpas_pool_get_array(sfc_input,'xland'   ,xland)

 sf_select: select case(trim(lsm_scheme))
    case("sf_noah")
       do i = 1,nCellsSolve
          rho = psfc(i)/(R_d*tsk(i))
          if(cqs2(i) .lt. 1.e-5) then
             q2(i) = qsfc(i)
          else
             q2(i) = qsfc(i) - qfx(i)/(rho*cqs2(i))
          endif
          if(chs2(i) .lt. 1.e-5) then
             t2m(i) = tsk(i)
          else
             t2m(i) = tsk(i) - hfx(i)/(rho*cp*chs2(i))
          endif
          th2m(i) = t2m(i)*(P0/psfc(i))**rcp
       enddo

    case("sf_noahmp")
       call mpas_pool_get_array(output_noahmp,'q2mxy',q2mxy)
       call mpas_pool_get_array(output_noahmp,'t2mxy',t2mxy)
       do i = 1,nCellsSolve
          rho = psfc(i)/(R_d*tsk(i))
          if((xland(i)-1.5 .gt. 0._RKIND) .or. (xland(i)-1.5.le.0._RKIND .and. xice(i).ge.xice_threshold)) then
             if(cqs2(i) .lt. 1.e-5) then
                q2(i) = qsfc(i)
             else
                q2(i) = qsfc(i) - qfx(i)/(rho*cqs2(i))
             endif
             if(chs2(i) .lt. 1.e-5) then
                t2m(i) = tsk(i)
             else
                t2m(i) = tsk(i) - hfx(i)/(rho*cp*chs2(i))
             endif
          else
             q2(i)  = q2mxy(i)
             t2m(i) = t2mxy(i)
          endif
          th2m(i) = t2m(i)*(P0/psfc(i))**rcp
       enddo

    case default
 end select sf_select

!call mpas_log_write('--- end subroutine atmphys_sfc_diagnostics:')

 end subroutine atmphys_sfc_diagnostics

!=================================================================================================================
 end module mpas_atmphys_sfc_diagnostics
!=================================================================================================================

