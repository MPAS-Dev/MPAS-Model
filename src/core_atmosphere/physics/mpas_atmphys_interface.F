! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module mpas_atmphys_interface
 use mpas_kind_types
 use mpas_pool_routines

 use mpas_atmphys_constants
 use mpas_atmphys_vars

 implicit none
 private
 public:: allocate_forall_physics,   &
          deallocate_forall_physics, &
          MPAS_to_physics,           &
          MPAS_to_physics_gpu,       &
          microphysics_from_MPAS,    &
          microphysics_to_MPAS

!Interface for conversion between variables used in the MPAS dynamical core and variables needed in the
!physics parameterizations.
!Laura D. Fowler (send comments to laura@ucar.edu).
!2013-05-01.
!
! subroutines in mpas_atmphys_interface_nhyd:
! -------------------------------------------
! allocate_forall_physics  : allocation of all meteorological variables needed in the physics.
! deallocate_forall_physics: deallocation of all meteorological variables needed in the physics.
! MPAS_to_physics          : conversion of input "MPAS" variables to "physics" variables.
! microphysics_from_MPAS   : initialize local arrays needed in cloud microphysics schemes.
! microphysics_to_MPAS     : copy local arrays needed in cloud microphysics schemesto MPAS arrays.
!
! add-ons and modifications to sourcecode:
! ----------------------------------------
! * in subroutine MPAS_to_physics, moved the calculation of the local arrays qv_p,qc_p, and qr_p above the 
!   calculation of th_p so that we do not need to use the pointer qv.
! * in subroutine microphysics_from_MPAS, moved the calculation of the local arrays qv_p,qc_p, and qr_p above 
!   the calculation of th_p so that we do not need to use the pointer qv.
! * in subroutine microphysics_to_MPAS, since microphysics schemes update the temperature and water vapor
!   mixing ratio, we first update the total pressure and exner functions. Then, we update the modified
!   potential temperature and calculate the diabatic tendency due to cloud microphysics processes.
!   Laura D. Fowler (laura@ucar.edu) / 2013-11-07.
! * replaced the variable g (that originally pointed to gravity) with gravity, for simplicity.
!   Laura D. Fowler (laura@ucar.edu) / 2014-03-21.
! * throughout the sourcecode, replaced all "var_struct" defined arrays by local pointers.
!   Laura D. Fowler (laura@ucar.edu) / 2014-04-22.
! * modified sourcecode to use pools.
!   Laura D. Fowler (laura@ucar.edu) / 2014-05-15.
! * added calculation of the surface pressure tendency. Moved the calculation of znu_p below the calculation 
!   of the surface pressure to avoid dividing by zero if the surface pressure is not output in the init file.
!   Laura D. Fowler (birch.mmm.ucar.ecu) / 2014-06-23. 
! * renamed module mpas_atmphys_interface_nhyd to mpas_atmphys_interface.
!   Laura D. Fowler (laura@ucar.edu) / 2014-09-19.
! * in subroutine microphysics_to_MPAS, reverted the calculation of cloud microphysics tendency rt_diabatic_tend, 
!   and update of the state variables to git hash identifier 7a66f273e182f4. This change reflects the fact that
!   we want to compute rt_diabatic_tend at constant volume.
!   Laura D. Fowler (laura@ucar.edu) / 2014-01-015. 
! * added the initialization of local variables needed in the parameterization of the MYNN surface layer scheme
!   and planetary boundary layer scheme from WRF 3.6.1.
!   Laura D. Fowler (laura@ucar.edu) / 2016-04-11.
! * corrected the calculation of the surface pressure, mainly extrapolation of the air density to the surface.
!   Laura D. Fowler (laura@ucar.edu) / 2016-04-25.


 contains


!=================================================================================================================
 subroutine allocate_forall_physics(configs)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs

!local pointers:
 character(len=StrKIND),pointer:: pbl_scheme

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_pbl_scheme',pbl_scheme)

 if(.not.allocated(psfc_p) ) allocate(psfc_p(ims:ime,jms:jme)         )
 if(.not.allocated(ptop_p) ) allocate(ptop_p(ims:ime,jms:jme)         )

 if(.not.allocated(u_p)    ) allocate(u_p(ims:ime,kms:kme,jms:jme)    )
 if(.not.allocated(v_p)    ) allocate(v_p(ims:ime,kms:kme,jms:jme)    )
 if(.not.allocated(fzm_p)  ) allocate(fzm_p(ims:ime,kms:kme,jms:jme)  )
 if(.not.allocated(fzp_p)  ) allocate(fzp_p(ims:ime,kms:kme,jms:jme)  )
 if(.not.allocated(zz_p)   ) allocate(zz_p(ims:ime,kms:kme,jms:jme)   )
 if(.not.allocated(pres_p) ) allocate(pres_p(ims:ime,kms:kme,jms:jme) )
 if(.not.allocated(pi_p)   ) allocate(pi_p(ims:ime,kms:kme,jms:jme)   )
 if(.not.allocated(z_p)    ) allocate(z_p(ims:ime,kms:kme,jms:jme)    )
 if(.not.allocated(zmid_p) ) allocate(zmid_p(ims:ime,kms:kme,jms:jme) )
 if(.not.allocated(dz_p)   ) allocate(dz_p(ims:ime,kms:kme,jms:jme)   )
 if(.not.allocated(t_p)    ) allocate(t_p(ims:ime,kms:kme,jms:jme)    )
 if(.not.allocated(th_p)   ) allocate(th_p(ims:ime,kms:kme,jms:jme)   )
 if(.not.allocated(al_p)   ) allocate(al_p(ims:ime,kms:kme,jms:jme)   )
 if(.not.allocated(rho_p)  ) allocate(rho_p(ims:ime,kms:kme,jms:jme)  )
 if(.not.allocated(rh_p)   ) allocate(rh_p(ims:ime,kms:kme,jms:jme)   )
 if(.not.allocated(znu_p)  ) allocate(znu_p(ims:ime,kms:kme,jms:jme)  )

 if(.not.allocated(w_p)    ) allocate(w_p(ims:ime,kms:kme,jms:jme)    )
 if(.not.allocated(pres2_p)) allocate(pres2_p(ims:ime,kms:kme,jms:jme))
 if(.not.allocated(t2_p)   ) allocate(t2_p(ims:ime,kms:kme,jms:jme)   )
 
 if(.not.allocated(qv_p)   ) allocate(qv_p(ims:ime,kms:kme,jms:jme)   )
 if(.not.allocated(qc_p)   ) allocate(qc_p(ims:ime,kms:kme,jms:jme)   )
 if(.not.allocated(qr_p)   ) allocate(qr_p(ims:ime,kms:kme,jms:jme)   )
 if(.not.allocated(qi_p)   ) allocate(qi_p(ims:ime,kms:kme,jms:jme)   )
 if(.not.allocated(qs_p)   ) allocate(qs_p(ims:ime,kms:kme,jms:jme)   )
 if(.not.allocated(qg_p)   ) allocate(qg_p(ims:ime,kms:kme,jms:jme)   )

 pbl_select: select case (trim(pbl_scheme))
    case("bl_mynn")
       if(.not.allocated(ni_p)) allocate(ni_p(ims:ime,kms:kme,jms:jme))

    case default

 end select pbl_select

!... arrays used for calculating the hydrostatic pressure and exner function:
 if(.not.allocated(psfc_hyd_p)  ) allocate(psfc_hyd_p(ims:ime,jms:jme)          )
 if(.not.allocated(psfc_hydd_p) ) allocate(psfc_hydd_p(ims:ime,jms:jme)         )
 if(.not.allocated(pres_hyd_p)  ) allocate(pres_hyd_p(ims:ime,kms:kme,jms:jme)  )
 if(.not.allocated(pres_hydd_p) ) allocate(pres_hydd_p(ims:ime,kms:kme,jms:jme) )
 if(.not.allocated(pres2_hyd_p) ) allocate(pres2_hyd_p(ims:ime,kms:kme,jms:jme) )
 if(.not.allocated(pres2_hydd_p)) allocate(pres2_hydd_p(ims:ime,kms:kme,jms:jme))
 if(.not.allocated(znu_hyd_p)   ) allocate(znu_hyd_p(ims:ime,kms:kme,jms:jme)   )
 
 end subroutine allocate_forall_physics

!=================================================================================================================
 subroutine deallocate_forall_physics(configs)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs

!local pointers:
 character(len=StrKIND),pointer:: pbl_scheme

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_pbl_scheme',pbl_scheme)

 if(allocated(psfc_p)  ) deallocate(psfc_p  )
 if(allocated(ptop_p)  ) deallocate(ptop_p  )

 if(allocated(u_p)     ) deallocate(u_p     )
 if(allocated(v_p)     ) deallocate(v_p     )
 if(allocated(fzm_p)   ) deallocate(fzm_p   )
 if(allocated(fzp_p)   ) deallocate(fzp_p   )
 if(allocated(zz_p)    ) deallocate(zz_p    )
 if(allocated(pres_p)  ) deallocate(pres_p  )
 if(allocated(pi_p)    ) deallocate(pi_p    )
 if(allocated(z_p)     ) deallocate(z_p     )
 if(allocated(zmid_p)  ) deallocate(zmid_p  )
 if(allocated(dz_p)    ) deallocate(dz_p    )
 if(allocated(t_p)     ) deallocate(t_p     )
 if(allocated(th_p)    ) deallocate(th_p    )
 if(allocated(al_p)    ) deallocate(al_p    )
 if(allocated(rho_p)   ) deallocate(rho_p   ) 
 if(allocated(rh_p)    ) deallocate(rh_p    ) 
 if(allocated(znu_p)   ) deallocate(znu_p   )

 if(allocated(w_p)     ) deallocate(w_p     )
 if(allocated(pres2_p) ) deallocate(pres2_p )
 if(allocated(t2_p)    ) deallocate(t2_p    )

 if(allocated(qv_p)    ) deallocate(qv_p    )
 if(allocated(qc_p)    ) deallocate(qc_p    )
 if(allocated(qr_p)    ) deallocate(qr_p    )
 if(allocated(qi_p)    ) deallocate(qi_p    )
 if(allocated(qs_p)    ) deallocate(qs_p    )
 if(allocated(qg_p)    ) deallocate(qg_p    )

 pbl_select: select case (trim(pbl_scheme))
    case("bl_mynn")
       if(allocated(ni_p)) deallocate(ni_p)

    case default

 end select pbl_select

 if(allocated(psfc_hyd_p)  ) deallocate(psfc_hyd_p  )
 if(allocated(psfc_hydd_p) ) deallocate(psfc_hydd_p )
 if(allocated(pres_hyd_p)  ) deallocate(pres_hyd_p  )
 if(allocated(pres_hydd_p) ) deallocate(pres_hydd_p )
 if(allocated(pres2_hyd_p) ) deallocate(pres2_hyd_p )
 if(allocated(pres2_hydd_p)) deallocate(pres2_hydd_p)
 if(allocated(znu_hyd_p)   ) deallocate(znu_hyd_p   )
 
 end subroutine deallocate_forall_physics

!=================================================================================================================
 subroutine MPAS_to_physics(configs,mesh,state,time_lev,diag,diag_physics,mpas_cpl,cells_to_rad_handle,cells_from_rad_handle,its,ite)
!=================================================================================================================

 use mpas_derived_types, only : mpas_coupler_type
 use mpas_intracoupler, only : mpas_coupler_transfer_field
 use mpas_intracoupler, only : ROLE_INTEGRATE, ROLE_RADIATION

!input variables:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: state
 type(mpas_pool_type),intent(in):: diag

 type (mpas_coupler_type), pointer :: mpas_cpl
 integer, intent(in) :: cells_to_rad_handle, cells_from_rad_handle

 integer,intent(in):: its,ite
 integer,intent(in):: time_lev

!inout variables:
 type(mpas_pool_type),intent(inout):: diag_physics

!local pointers:
 character(len=StrKIND),pointer:: pbl_scheme

 integer,pointer:: index_qv,index_qc,index_qr,index_qi,index_qs,index_qg
 integer,pointer:: index_ni

 real(kind=RKIND),dimension(:),pointer    :: latCell,lonCell
 real(kind=RKIND),dimension(:),pointer    :: fzm,fzp,rdzw
 real(kind=RKIND),dimension(:),pointer    :: surface_pressure,plrad
 real(kind=RKIND),dimension(:,:),pointer  :: zgrid
 real(kind=RKIND),dimension(:,:),pointer  :: zz,exner,pressure_b,rtheta_p,rtheta_b
 real(kind=RKIND),dimension(:,:),pointer  :: rho_zz,theta_m,pressure_p,u,v,w
 real(kind=RKIND),dimension(:,:),pointer  :: qv,qc,qr,qi,qs,qg
 real(kind=RKIND),dimension(:,:),pointer  :: ni
 real(kind=RKIND),dimension(:,:,:),pointer:: scalars

 type (field1DReal), pointer :: surface_pressureField, plradField
 type (field2DReal), pointer :: exnerField, pressure_bField, rtheta_pField, rtheta_bField
 type (field2DReal), pointer :: rho_zzField, theta_mField, pressure_pField, uField, vField, wField
 type (field3DReal), pointer :: scalarsField

!local variables:
 integer:: i,k,j
 integer :: iErr

 real(kind=RKIND):: z0,z1,z2,w1,w2
 real(kind=RKIND):: rho_a,rho1,rho2,tem1,tem2

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write('')
!call mpas_log_write('--- enter subroutine MPAS_to_phys:')
!call mpas_log_write('ims=$i ime=$i',intArgs=(/ims,ime/))
!call mpas_log_write('jms=$i jme=$i',intArgs=(/jms,jme/))
!call mpas_log_write('kms=$i kme=$i',intArgs=(/kms,kme/))
!call mpas_log_write('')
!call mpas_log_write('its=$i ite=$i',intArgs=(/its,ite/))
!call mpas_log_write('jts=$i jte=$i',intArgs=(/jts,jte/))
!call mpas_log_write('kts=$i kte=$i',intArgs=(/kts,kte/))

!initialization:
 call mpas_pool_get_config(configs,'config_pbl_scheme',pbl_scheme)

 call mpas_pool_get_array(mesh,'latCell',latCell)
 call mpas_pool_get_array(mesh,'lonCell',lonCell)
 call mpas_pool_get_array(mesh,'fzm'    ,fzm    )
 call mpas_pool_get_array(mesh,'fzp'    ,fzp    )
 call mpas_pool_get_array(mesh,'rdzw'   ,rdzw   )
 call mpas_pool_get_array(mesh,'zgrid'  ,zgrid  )
 call mpas_pool_get_array(mesh,'zz'     ,zz     )

 call mpas_pool_get_array(diag,'surface_pressure'      ,surface_pressure)
 call mpas_pool_get_array(diag,'exner'                 ,exner           )
 call mpas_pool_get_array(diag,'pressure_base'         ,pressure_b      )
 call mpas_pool_get_array(diag,'pressure_p'            ,pressure_p      )
 call mpas_pool_get_array(diag,'rtheta_base'           ,rtheta_b        )
 call mpas_pool_get_array(diag,'rtheta_p'              ,rtheta_p        )
 call mpas_pool_get_array(diag,'uReconstructZonal'     ,u               )
 call mpas_pool_get_array(diag,'uReconstructMeridional',v               )

 call mpas_pool_get_field(diag,'surface_pressure'      ,surface_pressureField)
 call mpas_pool_get_field(diag,'exner'                 ,exnerField           )
 call mpas_pool_get_field(diag,'pressure_base'         ,pressure_bField      )
 call mpas_pool_get_field(diag,'pressure_p'            ,pressure_pField      )
 call mpas_pool_get_field(diag,'rtheta_base'           ,rtheta_bField        )
 call mpas_pool_get_field(diag,'rtheta_p'              ,rtheta_pField        )
 call mpas_pool_get_field(diag,'uReconstructZonal'     ,uField               )
 call mpas_pool_get_field(diag,'uReconstructMeridional',vField               )

 call mpas_pool_get_array(state,'rho_zz' ,rho_zz ,time_lev)
 call mpas_pool_get_array(state,'theta_m',theta_m,time_lev)
 call mpas_pool_get_array(state,'w'      ,w      ,time_lev)

 call mpas_pool_get_field(state,'rho_zz' ,rho_zzField ,time_lev)
 call mpas_pool_get_field(state,'theta_m',theta_mField,time_lev)
 call mpas_pool_get_field(state,'w'      ,wField      ,time_lev)

 call mpas_pool_get_dimension(state,'index_qv',index_qv)
 call mpas_pool_get_dimension(state,'index_qc',index_qc)
 call mpas_pool_get_dimension(state,'index_qr',index_qr)
 call mpas_pool_get_dimension(state,'index_qi',index_qi)
 call mpas_pool_get_dimension(state,'index_qs',index_qs)
 call mpas_pool_get_dimension(state,'index_qg',index_qg)

 call mpas_pool_get_array(diag_physics,'plrad',plrad)
 call mpas_pool_get_array(state,'scalars',scalars,time_lev)
 qv => scalars(index_qv,:,:)
 qc => scalars(index_qc,:,:)
 qr => scalars(index_qr,:,:)
 qi => scalars(index_qi,:,:)
 qs => scalars(index_qs,:,:)
 qg => scalars(index_qg,:,:)

 call mpas_pool_get_field(diag_physics,'plrad',plradField)
 call mpas_pool_get_field(state,'scalars',scalarsField,time_lev)

 if (l_radtlw .or. l_radtsw) then
    if (mpas_cpl%role_is(ROLE_INTEGRATE) .or. &
        mpas_cpl%role_is(ROLE_RADIATION)) then

!$OMP BARRIER
!$OMP MASTER
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, exnerField, ierr=ierr)
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, pressure_bField, ierr=ierr)
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, pressure_pField, ierr=ierr)
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, rtheta_bField, ierr=ierr)
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, rtheta_pField, ierr=ierr)
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, uField, ierr=ierr)
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, vField, ierr=ierr)
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, rho_zzField, ierr=ierr)
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, theta_mField, ierr=ierr)
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, wField, ierr=ierr)
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, scalarsField, ierr=ierr)
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, surface_pressureField, ierr=ierr)
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, plradField, ierr=ierr)
!$OMP END MASTER
!$OMP BARRIER

    end if
 end if

 do j = jts, jte
 do k = kts, kte
 do i = its, ite

    !water vapor and moist arrays:
    qv_p(i,k,j) = max(0.,qv(k,i))
    qc_p(i,k,j) = max(0.,qc(k,i))
    qr_p(i,k,j) = max(0.,qr(k,i))
    qi_p(i,k,j) = max(0.,qi(k,i))
    qs_p(i,k,j) = max(0.,qs(k,i))
    qg_p(i,k,j) = max(0.,qg(k,i))

    !arrays located at theta points:
    u_p(i,k,j) = u(k,i)
    v_p(i,k,j) = v(k,i)

    zz_p(i,k,j)  = zz(k,i)
    rho_p(i,k,j) = zz(k,i) * rho_zz(k,i)
    rho_p(i,k,j) = rho_p(i,k,j)*(1._RKIND + qv_p(i,k,j))
    th_p(i,k,j)  = theta_m(k,i) / (1._RKIND + R_v/R_d * qv_p(i,k,j))
    t_p(i,k,j)   = th_p(i,k,j)*exner(k,i)

    pi_p(i,k,j)   = exner(k,i)
    pres_p(i,k,j) = pressure_p(k,i) + pressure_b(k,i)

    zmid_p(i,k,j) = 0.5*(zgrid(k+1,i)+zgrid(k,i))
    dz_p(i,k,j)   = zgrid(k+1,i)-zgrid(k,i)

 enddo
 enddo
 enddo

 pbl_select: select case (trim(pbl_scheme))
    case("bl_mynn")
       call mpas_pool_get_dimension(state,'index_ni',index_ni)
       ni => scalars(index_ni,:,:)

       do j = jts,jte
       do k = kts,kte
       do i = its,ite
          ni_p(i,k,j) = max(0.,ni(k,i))
       enddo
       enddo
       enddo

    case default

 end select pbl_select

!calculation of the surface pressure using hydrostatic assumption down to the surface::
 do j = jts,jte
 do i = its,ite
    tem1 = zgrid(2,i)-zgrid(1,i)
    tem2 = zgrid(3,i)-zgrid(2,i)
    rho1 = rho_zz(1,i) * zz(1,i) * (1. + qv_p(i,1,j))
    rho2 = rho_zz(2,i) * zz(2,i) * (1. + qv_p(i,2,j))
!   surface_pressure(i) = 0.5*gravity*(zgrid(2,i)-zgrid(1,i)) &
!                   * (rho1 + 0.5*(rho2-rho1)*tem1/(tem1+tem2))
    surface_pressure(i) = 0.5*gravity*(zgrid(2,i)-zgrid(1,i)) &
                    * (rho1 - 0.5*(rho2-rho1)*tem1/(tem1+tem2))
    surface_pressure(i) = surface_pressure(i) + pressure_p(1,i) + pressure_b(1,i)
 enddo
 
 do k = kts,kte
 do i = its,ite
    znu_p(i,k,j)  = pres_p(i,k,j) / surface_pressure(i)
 enddo
 enddo
 enddo

!arrays located at w points:
 do j = jts, jte
 do k = kts,kte+1
 do i = its,ite
    w_p(i,k,j) = w(k,i)
    z_p(i,k,j) = zgrid(k,i)
 enddo
 enddo
 enddo

!check that the pressure in the layer above the surface is greater than that in the layer
!above it:
 do j = jts,jte
 do i = its,ite
    if(pres_p(i,1,j) .le. pres_p(i,2,j)) then
       call mpas_log_write('')
       call mpas_log_write('--- subroutine MPAS_to_phys - pressure(1) < pressure(2):')
       call mpas_log_write('i      =$i', intArgs=(/i/))
       call mpas_log_write('latCell=$r', realArgs=(/latCell(i)/degrad/))
       call mpas_log_write('lonCell=$r', realArgs=(/lonCell(i)/degrad/))
       do k = kts,kte
          call mpas_log_write('$i $i $i $r $r $r $r $r $r $r $r', intArgs=(/j,i,k/),&
             realArgs=(/dz_p(i,k,j),pressure_b(k,i),pressure_p(k,i),pres_p(i,k,j), &
                        rho_p(i,k,j),th_p(i,k,j),t_p(i,k,j),qv_p(i,k,j)/))
       enddo
!      call mpas_log_write('pressure increasing with height', messageType=MPAS_LOG_CRIT)
    endif
 enddo
 enddo

!interpolation of pressure and temperature from theta points to w points:
 do j = jts,jte
 do k = kts+1,kte
 do i = its,ite
    tem1 = 1./(zgrid(k+1,i)-zgrid(k-1,i))
    fzm_p(i,k,j) = (zgrid(k,i)-zgrid(k-1,i)) * tem1
    fzp_p(i,k,j) = (zgrid(k+1,i)-zgrid(k,i)) * tem1
    t2_p(i,k,j)    = fzm_p(i,k,j)*t_p(i,k,j) + fzp_p(i,k,j)*t_p(i,k-1,j)
    pres2_p(i,k,j) = fzm_p(i,k,j)*pres_p(i,k,j) + fzp_p(i,k,j)*pres_p(i,k-1,j)
 enddo
 enddo
 enddo

!interpolation of pressure and temperature from theta points to the top-of-the-model: follows
!the calculation of the top-of-the-model pressure and temperature in WRF (subroutine phy_prep
!in ./dyn_em/module_big_step_utilities.F):
 k = kte+1
 do j = jts,jte
 do i = its,ite
    z0 = zgrid(k,i)
    z1 = 0.5*(zgrid(k,i)+zgrid(k-1,i)) 
    z2 = 0.5*(zgrid(k-1,i)+zgrid(k-2,i))
    w1 = (z0-z2)/(z1-z2)
    w2 = 1.-w1
    t2_p(i,k,j) = w1*t_p(i,k-1,j) + w2*t_p(i,k-2,j)
    !use log of pressure to avoid occurrences of negative top-of-the-model pressure.
    pres2_p(i,k,j) = exp(w1*log(pres_p(i,k-1,j))+w2*log(pres_p(i,k-2,j)))
 enddo
 enddo

!ldf (2012-06-22): recalculates the pressure at the surface as an extrapolation of the
!pressures in the 2 layers above the surface, as was originally done:
 k = kts
 do j = jts,jte
 do i = its,ite
    z0 = zgrid(k,i)
    z1 = 0.5*(zgrid(k,i)+zgrid(k+1,i)) 
    z2 = 0.5*(zgrid(k+1,i)+zgrid(k+2,i))
    w1 = (z0-z2)/(z1-z2)
    w2 = 1.-w1
    t2_p(i,k,j)    = w1*t_p(i,k,j)+w2*t_p(i,k+1,j)
    pres2_p(i,k,j) = w1*pres_p(i,k,j)+w2*pres_p(i,k+1,j)
!   psfc_p(i,j) = pres2_p(i,k,j)
    psfc_p(i,j) = surface_pressure(i)
 enddo
 enddo

!calculation of the hydrostatic pressure:
 do j = jts,jte
 do i = its,ite
    !pressure at w-points:
    k = kte+1
    pres2_hyd_p(i,k,j)  = pres2_p(i,k,j)
    pres2_hydd_p(i,k,j) = pres2_p(i,k,j)
    do k = kte,1,-1
       rho_a = rho_p(i,k,j) / (1.+qv_p(i,k,j))
       pres2_hyd_p(i,k,j)  = pres2_hyd_p(i,k+1,j)  + gravity*rho_p(i,k,j)*dz_p(i,k,j)
       pres2_hydd_p(i,k,j) = pres2_hydd_p(i,k+1,j) + gravity*rho_a*dz_p(i,k,j)
    enddo
    !pressure at theta-points:
    do k = kte,1,-1
       pres_hyd_p(i,k,j)  = 0.5*(pres2_hyd_p(i,k+1,j)+pres2_hyd_p(i,k,j))
       pres_hydd_p(i,k,j) = 0.5*(pres2_hydd_p(i,k+1,j)+pres2_hydd_p(i,k,j))
    enddo
    !surface pressure:
    psfc_hyd_p(i,j) = pres2_hyd_p(i,1,j)
    psfc_hydd_p(i,j) = pres2_hydd_p(i,1,j)
    !znu:
    do k = kte,1,-1
       znu_hyd_p(i,k,j) = pres_hyd_p(i,k,j) / psfc_hyd_p(i,j) 
    enddo
 enddo
 enddo

!save the model-top pressure:
 do j = jts,jte
 do i = its,ite
    plrad(i) = pres2_p(i,kte+1,j) 
 enddo
 enddo

 end subroutine MPAS_to_physics

!=================================================================================================================
 subroutine MPAS_to_physics_gpu(configs,mesh,state,time_lev,diag,diag_physics,mpas_cpl,cells_to_rad_handle,cells_from_rad_handle,its,ite)
!=================================================================================================================

 use mpas_derived_types, only : mpas_coupler_type
 use mpas_intracoupler, only : mpas_coupler_transfer_field
 use mpas_intracoupler, only : ROLE_INTEGRATE, ROLE_RADIATION

!input variables:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: state
 type(mpas_pool_type),intent(in):: diag

 type (mpas_coupler_type), pointer :: mpas_cpl
 integer, intent(in) :: cells_to_rad_handle, cells_from_rad_handle

 integer,intent(in):: its,ite
 integer,intent(in):: time_lev

!inout variables:
 type(mpas_pool_type),intent(inout):: diag_physics

!local pointers:
 character(len=StrKIND),pointer:: pbl_scheme

 integer,pointer:: index_qv,index_qc,index_qr,index_qi,index_qs,index_qg
 integer,pointer:: index_ni

 real(kind=RKIND),dimension(:),pointer    :: latCell,lonCell
 real(kind=RKIND),dimension(:),pointer    :: fzm,fzp,rdzw
 real(kind=RKIND),dimension(:),pointer    :: surface_pressure,plrad
 real(kind=RKIND),dimension(:,:),pointer  :: zgrid
 real(kind=RKIND),dimension(:,:),pointer  :: zz,exner,pressure_b,rtheta_p,rtheta_b
 real(kind=RKIND),dimension(:,:),pointer  :: rho_zz,theta_m,pressure_p,u,v,w
 real(kind=RKIND),dimension(:,:),pointer  :: qv,qc,qr,qi,qs,qg
 real(kind=RKIND),dimension(:,:),pointer  :: ni
 real(kind=RKIND),dimension(:,:,:),pointer:: scalars

 type (field1DReal), pointer :: surface_pressureField, plradField
 type (field2DReal), pointer :: exnerField, pressure_bField, rtheta_pField, rtheta_bField
 type (field2DReal), pointer :: rho_zzField, theta_mField, pressure_pField, uField, vField, wField
 type (field3DReal), pointer :: scalarsField

!local variables:
 integer:: i,k,j
 integer :: iErr

 real(kind=RKIND):: z0,z1,z2,w1,w2
 real(kind=RKIND):: rho_a,rho1,rho2,tem1,tem2

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write('')
!call mpas_log_write('--- enter subroutine MPAS_to_phys:')
!call mpas_log_write('ims=$i ime=$i',intArgs=(/ims,ime/))
!call mpas_log_write('jms=$i jme=$i',intArgs=(/jms,jme/))
!call mpas_log_write('kms=$i kme=$i',intArgs=(/kms,kme/))
!call mpas_log_write('')
!call mpas_log_write('its=$i ite=$i',intArgs=(/its,ite/))
!call mpas_log_write('jts=$i jte=$i',intArgs=(/jts,jte/))
!call mpas_log_write('kts=$i kte=$i',intArgs=(/kts,kte/))

!initialization:
 call mpas_pool_get_config(configs,'config_pbl_scheme',pbl_scheme)

 call mpas_pool_get_array_gpu(mesh,'latCell',latCell)
 call mpas_pool_get_array_gpu(mesh,'lonCell',lonCell)
 call mpas_pool_get_array_gpu(mesh,'fzm'    ,fzm    )
 call mpas_pool_get_array_gpu(mesh,'fzp'    ,fzp    )
 call mpas_pool_get_array_gpu(mesh,'rdzw'   ,rdzw   )
 call mpas_pool_get_array_gpu(mesh,'zgrid'  ,zgrid  )
 call mpas_pool_get_array_gpu(mesh,'zz'     ,zz     )

 call mpas_pool_get_array_gpu(diag,'surface_pressure'      ,surface_pressure)
 call mpas_pool_get_array_gpu(diag,'exner'                 ,exner           )
 call mpas_pool_get_array_gpu(diag,'pressure_base'         ,pressure_b      )
 call mpas_pool_get_array_gpu(diag,'pressure_p'            ,pressure_p      )
 call mpas_pool_get_array_gpu(diag,'rtheta_base'           ,rtheta_b        )
 call mpas_pool_get_array_gpu(diag,'rtheta_p'              ,rtheta_p        )
 call mpas_pool_get_array_gpu(diag,'uReconstructZonal'     ,u               )
 call mpas_pool_get_array_gpu(diag,'uReconstructMeridional',v               )

 call mpas_pool_get_field(diag,'surface_pressure'      ,surface_pressureField)
 call mpas_pool_get_field(diag,'exner'                 ,exnerField           )
 call mpas_pool_get_field(diag,'pressure_base'         ,pressure_bField      )
 call mpas_pool_get_field(diag,'pressure_p'            ,pressure_pField      )
 call mpas_pool_get_field(diag,'rtheta_base'           ,rtheta_bField        )
 call mpas_pool_get_field(diag,'rtheta_p'              ,rtheta_pField        )
 call mpas_pool_get_field(diag,'uReconstructZonal'     ,uField               )
 call mpas_pool_get_field(diag,'uReconstructMeridional',vField               )

 call mpas_pool_get_array_gpu(state,'rho_zz' ,rho_zz ,time_lev)
 call mpas_pool_get_array_gpu(state,'theta_m',theta_m,time_lev)
 call mpas_pool_get_array_gpu(state,'w'      ,w      ,time_lev)

 call mpas_pool_get_field(state,'rho_zz' ,rho_zzField ,time_lev)
 call mpas_pool_get_field(state,'theta_m',theta_mField,time_lev)
 call mpas_pool_get_field(state,'w'      ,wField      ,time_lev)

 call mpas_pool_get_dimension(state,'index_qv',index_qv)
 call mpas_pool_get_dimension(state,'index_qc',index_qc)
 call mpas_pool_get_dimension(state,'index_qr',index_qr)
 call mpas_pool_get_dimension(state,'index_qi',index_qi)
 call mpas_pool_get_dimension(state,'index_qs',index_qs)
 call mpas_pool_get_dimension(state,'index_qg',index_qg)

 call mpas_pool_get_array_gpu(diag_physics,'plrad',plrad)
 call mpas_pool_get_array_gpu(state,'scalars',scalars,time_lev)
!!!$acc update host(latCell, lonCell, fzm, fzp, rdzw, zgrid, zz, surface_pressure, &
!!!$acc             exner, pressure_b, pressure_p, rtheta_b, rtheta_p, u, v, rho_zz, &
!!!$acc             theta_m, w, plrad, scalars)
!!! qv => scalars(index_qv,:,:)
!!! qc => scalars(index_qc,:,:)
!!! qr => scalars(index_qr,:,:)
!!! qi => scalars(index_qi,:,:)
!!! qs => scalars(index_qs,:,:)
!!! qg => scalars(index_qg,:,:)

 call mpas_pool_get_field(diag_physics,'plrad',plradField)
 call mpas_pool_get_field(state,'scalars',scalarsField,time_lev)

 if (l_radtlw .or. l_radtsw) then
!$acc update host(exner, pressure_b, pressure_p, rtheta_b, rtheta_p, u, v, rho_zz, &
!$acc             theta_m, w, scalars, surface_pressure, plrad)
    if (mpas_cpl%role_is(ROLE_INTEGRATE) .or. &
        mpas_cpl%role_is(ROLE_RADIATION)) then

!$OMP BARRIER
!$OMP MASTER
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, exnerField, ierr=ierr)
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, pressure_bField, ierr=ierr)
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, pressure_pField, ierr=ierr)
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, rtheta_bField, ierr=ierr)
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, rtheta_pField, ierr=ierr)
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, uField, ierr=ierr)
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, vField, ierr=ierr)
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, rho_zzField, ierr=ierr)
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, theta_mField, ierr=ierr)
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, wField, ierr=ierr)
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, scalarsField, ierr=ierr)
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, surface_pressureField, ierr=ierr)
       call MPAS_coupler_transfer_field(mpas_cpl, cells_to_rad_handle, plradField, ierr=ierr)
!$OMP END MASTER
!$OMP BARRIER

    end if
!$acc update device(exner, pressure_b, pressure_p, rtheta_b, rtheta_p, u, v, rho_zz, &
!$acc             theta_m, w, scalars, surface_pressure, plrad)
 end if

 do j = jts, jte
!$acc parallel vector_length(128)
!$acc loop gang vector collapse(2)
 do k = kts, kte
 do i = its, ite

    !water vapor and moist arrays:
    qv_p(i,k,j) = max(0.,scalars(index_qv,k,i))
    qc_p(i,k,j) = max(0.,scalars(index_qc,k,i))
    qr_p(i,k,j) = max(0.,scalars(index_qr,k,i))
    qi_p(i,k,j) = max(0.,scalars(index_qi,k,i))
    qs_p(i,k,j) = max(0.,scalars(index_qs,k,i))
    qg_p(i,k,j) = max(0.,scalars(index_qg,k,i))

    !arrays located at theta points:
    u_p(i,k,j) = u(k,i)
    v_p(i,k,j) = v(k,i)

    zz_p(i,k,j)  = zz(k,i)
    rho_p(i,k,j) = zz(k,i) * rho_zz(k,i)
    rho_p(i,k,j) = rho_p(i,k,j)*(1._RKIND + qv_p(i,k,j))
    th_p(i,k,j)  = theta_m(k,i) / (1._RKIND + R_v/R_d * qv_p(i,k,j))
    t_p(i,k,j)   = th_p(i,k,j)*exner(k,i)

    pi_p(i,k,j)   = exner(k,i)
    pres_p(i,k,j) = pressure_p(k,i) + pressure_b(k,i)

    zmid_p(i,k,j) = 0.5*(zgrid(k+1,i)+zgrid(k,i))
    dz_p(i,k,j)   = zgrid(k+1,i)-zgrid(k,i)

 enddo
 enddo
!$acc end parallel
 enddo
!!!$acc update host(qv_p,qc_p,qr_p,qi_p,qs_p,qg_p,u_p,v_p, &
!!!$acc zz_p,rho_p,th_p,t_p,pi_p,pres_p,zmid_p,dz_p)

 pbl_select: select case (trim(pbl_scheme))
    case("bl_mynn")
       call mpas_pool_get_dimension(state,'index_ni',index_ni)
       ni => scalars(index_ni,:,:)

       do j = jts,jte
       do k = kts,kte
       do i = its,ite
          ni_p(i,k,j) = max(0.,ni(k,i))
       enddo
       enddo
       enddo

    case default

 end select pbl_select

!calculation of the surface pressure using hydrostatic assumption down to the surface::
 do j = jts,jte
!$acc parallel vector_length(128)
!$acc loop gang vector private(tem1,tem2,rho1,rho2)
 do i = its,ite
    tem1 = zgrid(2,i)-zgrid(1,i)
    tem2 = zgrid(3,i)-zgrid(2,i)
    rho1 = rho_zz(1,i) * zz(1,i) * (1. + qv_p(i,1,j))
    rho2 = rho_zz(2,i) * zz(2,i) * (1. + qv_p(i,2,j))
!   surface_pressure(i) = 0.5*gravity*(zgrid(2,i)-zgrid(1,i)) &
!                   * (rho1 + 0.5*(rho2-rho1)*tem1/(tem1+tem2))
    surface_pressure(i) = 0.5*gravity*(zgrid(2,i)-zgrid(1,i)) &
                    * (rho1 - 0.5*(rho2-rho1)*tem1/(tem1+tem2))
    surface_pressure(i) = surface_pressure(i) + pressure_p(1,i) + pressure_b(1,i)
 enddo
!$acc end parallel
 
!$acc parallel vector_length(128)
!$acc loop gang vector collapse(2)
 do k = kts,kte
 do i = its,ite
    znu_p(i,k,j)  = pres_p(i,k,j) / surface_pressure(i)
 enddo
 enddo
!$acc end parallel
 enddo

!arrays located at w points:
 do j = jts, jte
!$acc parallel vector_length(128)
!$acc loop gang vector collapse(2)
 do k = kts,kte+1
 do i = its,ite
    w_p(i,k,j) = w(k,i)
    z_p(i,k,j) = zgrid(k,i)
 enddo
 enddo
!$acc end parallel
 enddo
!!!$acc update host(znu_p,w_p,z_p)

!check that the pressure in the layer above the surface is greater than that in the layer
!above it:
!!$acc update host(pressure_b,pressure_p,pres_p,dz_p,rho_p,th_p,t_p,qv_p)
#ifndef MPAS_OPENACC
 do j = jts,jte
 do i = its,ite
    if(pres_p(i,1,j) .le. pres_p(i,2,j)) then
       call mpas_log_write('')
       call mpas_log_write('--- subroutine MPAS_to_phys - pressure(1) < pressure(2):')
       call mpas_log_write('i      =$i', intArgs=(/i/))
       call mpas_log_write('latCell=$r', realArgs=(/latCell(i)/degrad/))
       call mpas_log_write('lonCell=$r', realArgs=(/lonCell(i)/degrad/))
       do k = kts,kte
          call mpas_log_write('$i $i $i $r $r $r $r $r $r $r $r', intArgs=(/j,i,k/),&
             realArgs=(/dz_p(i,k,j),pressure_b(k,i),pressure_p(k,i),pres_p(i,k,j), &
                        rho_p(i,k,j),th_p(i,k,j),t_p(i,k,j),qv_p(i,k,j)/))
       enddo
!      call mpas_log_write('pressure increasing with height', messageType=MPAS_LOG_CRIT)
    endif
 enddo
 enddo
#endif

!interpolation of pressure and temperature from theta points to w points:
 do j = jts,jte
!$acc parallel vector_length(128)
!$acc loop gang vector collapse(2) private(tem1)
 do k = kts+1,kte
 do i = its,ite
    tem1 = 1./(zgrid(k+1,i)-zgrid(k-1,i))
    fzm_p(i,k,j) = (zgrid(k,i)-zgrid(k-1,i)) * tem1
    fzp_p(i,k,j) = (zgrid(k+1,i)-zgrid(k,i)) * tem1
    t2_p(i,k,j)    = fzm_p(i,k,j)*t_p(i,k,j) + fzp_p(i,k,j)*t_p(i,k-1,j)
    pres2_p(i,k,j) = fzm_p(i,k,j)*pres_p(i,k,j) + fzp_p(i,k,j)*pres_p(i,k-1,j)
 enddo
 enddo
!$acc end parallel
 enddo

!interpolation of pressure and temperature from theta points to the top-of-the-model: follows
!the calculation of the top-of-the-model pressure and temperature in WRF (subroutine phy_prep
!in ./dyn_em/module_big_step_utilities.F):
 k = kte+1
 do j = jts,jte
!$acc parallel vector_length(128)
!$acc loop gang vector private(z0,z1,z2,w1,w2)
 do i = its,ite
    z0 = zgrid(k,i)
    z1 = 0.5*(zgrid(k,i)+zgrid(k-1,i)) 
    z2 = 0.5*(zgrid(k-1,i)+zgrid(k-2,i))
    w1 = (z0-z2)/(z1-z2)
    w2 = 1.-w1
    t2_p(i,k,j) = w1*t_p(i,k-1,j) + w2*t_p(i,k-2,j)
    !use log of pressure to avoid occurrences of negative top-of-the-model pressure.
    pres2_p(i,k,j) = exp(w1*log(pres_p(i,k-1,j))+w2*log(pres_p(i,k-2,j)))
 enddo
!$acc end parallel
 enddo

!ldf (2012-06-22): recalculates the pressure at the surface as an extrapolation of the
!pressures in the 2 layers above the surface, as was originally done:
 k = kts
 do j = jts,jte
!$acc parallel vector_length(128)
!$acc loop gang vector private(z0,z1,z2,w1,w2)
 do i = its,ite
    z0 = zgrid(k,i)
    z1 = 0.5*(zgrid(k,i)+zgrid(k+1,i)) 
    z2 = 0.5*(zgrid(k+1,i)+zgrid(k+2,i))
    w1 = (z0-z2)/(z1-z2)
    w2 = 1.-w1
    t2_p(i,k,j)    = w1*t_p(i,k,j)+w2*t_p(i,k+1,j)
    pres2_p(i,k,j) = w1*pres_p(i,k,j)+w2*pres_p(i,k+1,j)
!   psfc_p(i,j) = pres2_p(i,k,j)
    psfc_p(i,j) = surface_pressure(i)
 enddo
!$acc end parallel
 enddo
!!!$acc update host(fzm_p,fzp_p,t2_p,pres2_p,psfc_p)

!calculation of the hydrostatic pressure:
 do j = jts,jte
!$acc parallel vector_length(128)
!$acc loop gang vector
 do i = its,ite
    !pressure at w-points:
!!!    k = kte+1
    pres2_hyd_p(i,kte+1,j)  = pres2_p(i,kte+1,j)
    pres2_hydd_p(i,kte+1,j) = pres2_p(i,kte+1,j)
 enddo
!$acc end parallel
 enddo
 do j = jts,jte
!$acc parallel vector_length(128)
!$acc loop seq
 do k = kte,1,-1
!$acc loop gang vector private(rho_a)
    do i = its,ite
       rho_a = rho_p(i,k,j) / (1.+qv_p(i,k,j))
       pres2_hyd_p(i,k,j)  = pres2_hyd_p(i,k+1,j)  + gravity*rho_p(i,k,j)*dz_p(i,k,j)
       pres2_hydd_p(i,k,j) = pres2_hydd_p(i,k+1,j) + gravity*rho_a*dz_p(i,k,j)
    enddo
 enddo
!$acc end parallel
 enddo
 do j = jts,jte
!$acc parallel vector_length(128)
    !pressure at theta-points:
!$acc loop gang vector collapse(2)
 do k = kte,1,-1
    do i = its,ite
       pres_hyd_p(i,k,j)  = 0.5*(pres2_hyd_p(i,k+1,j)+pres2_hyd_p(i,k,j))
       pres_hydd_p(i,k,j) = 0.5*(pres2_hydd_p(i,k+1,j)+pres2_hydd_p(i,k,j))
    enddo
 enddo
!$acc end parallel
 enddo
 do j = jts,jte
!$acc parallel vector_length(128)
!$acc loop gang vector
 do i = its,ite
    !surface pressure:
    psfc_hyd_p(i,j) = pres2_hyd_p(i,1,j)
    psfc_hydd_p(i,j) = pres2_hydd_p(i,1,j)
    !znu:
 enddo
!$acc end parallel
 enddo
 do j = jts,jte
!$acc parallel vector_length(128)
!$acc loop gang vector collapse(2)
 do k = kte,1,-1
    do i = its,ite
       znu_hyd_p(i,k,j) = pres_hyd_p(i,k,j) / psfc_hyd_p(i,j) 
    enddo
 enddo
!$acc end parallel
 enddo
!!!$acc update host(pres2_hyd_p,pres2_hydd_p,pres_hyd_p, &
!!!$acc pres_hydd_p,psfc_hyd_p,psfc_hydd_p,znu_hyd_p)

!save the model-top pressure:
 do j = jts,jte
!$acc parallel vector_length(128)
!$acc loop gang vector
 do i = its,ite
    plrad(i) = pres2_p(i,kte+1,j) 
 enddo
!$acc end parallel
 enddo

 end subroutine MPAS_to_physics_gpu

!=================================================================================================================
 subroutine microphysics_from_MPAS(configs,mesh,state,time_lev,diag,diag_physics,its,ite)
!=================================================================================================================

!input variables:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: state
 type(mpas_pool_type),intent(in):: diag
 type(mpas_pool_type),intent(in):: diag_physics

 integer,intent(in):: its,ite
 integer:: time_lev

!local pointers:
 character(len=StrKIND),pointer:: microp_scheme
 integer,pointer:: index_qv,index_qc,index_qr,index_qi,index_qs,index_qg
 integer:: sindex_qv,sindex_qc,sindex_qr,sindex_qi,sindex_qs,sindex_qg
 integer,pointer:: index_ni,index_nr
 integer:: sindex_ni,sindex_nr
 real(kind=RKIND),dimension(:),pointer    :: nt_c,mu_c
 real(kind=RKIND),dimension(:,:),pointer  :: zgrid,w
 real(kind=RKIND),dimension(:,:),pointer  :: zz,exner,pressure_b
 real(kind=RKIND),dimension(:,:),pointer  :: rho_zz,theta_m,pressure_p
 real(kind=RKIND),dimension(:,:),pointer  :: qv,qc,qr,qi,qs,qg
 real(kind=RKIND),dimension(:,:),pointer  :: ni,nr
 real(kind=RKIND),dimension(:,:),pointer  :: rainprod,evapprod
 real(kind=RKIND),dimension(:,:),pointer  :: re_cloud,re_ice,re_snow
 real(kind=RKIND),dimension(:,:,:),pointer:: scalars

!local variables:
 integer:: i,k,j

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_microp_scheme',microp_scheme)

 call mpas_pool_get_array_gpu(mesh,'zgrid',zgrid)
 call mpas_pool_get_array_gpu(mesh,'zz'   ,zz   )

 call mpas_pool_get_array_gpu(diag,'exner'        ,exner     )
 call mpas_pool_get_array_gpu(diag,'pressure_base',pressure_b)
 call mpas_pool_get_array_gpu(diag,'pressure_p'   ,pressure_p)

 call mpas_pool_get_array_gpu(diag_physics,'nt_c'    ,nt_c    )
 call mpas_pool_get_array_gpu(diag_physics,'mu_c'    ,mu_c    )
 call mpas_pool_get_array_gpu(diag_physics,'rainprod',rainprod)
 call mpas_pool_get_array_gpu(diag_physics,'evapprod',evapprod)
 call mpas_pool_get_array_gpu(diag_physics,'re_cloud',re_cloud)
 call mpas_pool_get_array_gpu(diag_physics,'re_ice'  ,re_ice  )
 call mpas_pool_get_array_gpu(diag_physics,'re_snow' ,re_snow )

 call mpas_pool_get_array_gpu(state,'rho_zz' ,rho_zz ,time_lev)
 call mpas_pool_get_array_gpu(state,'theta_m',theta_m,time_lev)
 call mpas_pool_get_array_gpu(state,'w'      ,w      ,time_lev)

 call mpas_pool_get_dimension(state,'index_qv'  ,index_qv  )
 call mpas_pool_get_dimension(state,'index_qc'  ,index_qc  )
 call mpas_pool_get_dimension(state,'index_qr'  ,index_qr  )
 call mpas_pool_get_dimension(state,'index_qi'  ,index_qi  )
 call mpas_pool_get_dimension(state,'index_qs'  ,index_qs  )
 call mpas_pool_get_dimension(state,'index_qg'  ,index_qg  )
 call mpas_pool_get_dimension(state,'index_ni'  ,index_ni  )
 call mpas_pool_get_dimension(state,'index_nr'  ,index_nr  )

 sindex_qv = index_qv
 sindex_qc = index_qc
 sindex_qr = index_qr
 sindex_qi = index_qi
 sindex_qs = index_qs
 sindex_qg = index_qg
 sindex_ni = index_ni
 sindex_nr = index_nr

 call mpas_pool_get_array_gpu(state,'scalars',scalars,time_lev)
!!$acc update host(zgrid, zz, exner, pressure_b, pressure_p, nt_c, mu_c, &
!!$acc             rainprod, evapprod, re_cloud, re_ice, re_snow, rho_zz, &
!!$acc             theta_m, w, scalars)
! qv   => scalars(index_qv,:,:)
! qc   => scalars(index_qc,:,:)
! qr   => scalars(index_qr,:,:)

!$acc parallel vector_length(32)
!$acc loop gang vector collapse(3)
!initialize variables needed in the cloud microphysics schemes:
 do j = jts, jte
 do i = its, ite
 do k = kts, kte
    qv_p(i,k,j) = scalars(sindex_qv,k,i)
    qc_p(i,k,j) = scalars(sindex_qc,k,i)
    qr_p(i,k,j) = scalars(sindex_qr,k,i)

    rho_p(i,k,j)  = zz(k,i) * rho_zz(k,i)
    th_p(i,k,j)   = theta_m(k,i) / (1._RKIND + R_v/R_d * max(0._RKIND,qv_p(i,k,j)))

    pi_p(i,k,j)   = exner(k,i)
    pres_p(i,k,j) = pressure_b(k,i) + pressure_p(k,i)

    z_p(i,k,j)    = zgrid(k,i)
    dz_p(i,k,j)   = zgrid(k+1,i) - zgrid(k,i)
    w_p(i,k,j)    = w(k,i)
 enddo
 enddo
 enddo
 !$acc end parallel

!additional initialization as function of cloud microphysics scheme:
 microp_select_init: select case(microp_scheme)

    case ("mp_thompson","mp_wsm6")
!       qi   => scalars(index_qi,:,:)
!       qs   => scalars(index_qs,:,:)
!       qg   => scalars(index_qg,:,:)

!$acc parallel vector_length(32)
!$acc loop gang vector collapse(3)
       do j = jts, jte
       do k = kts, kte
       do i = its, ite
          qi_p(i,k,j) = scalars(sindex_qi,k,i)
          qs_p(i,k,j) = scalars(sindex_qs,k,i)
          qg_p(i,k,j) = scalars(sindex_qg,k,i)
          recloud_p(i,k,j) = re_cloud(k,i)
          reice_p(i,k,j)   = re_ice(k,i)
          resnow_p(i,k,j)  = re_snow(k,i)
       enddo
       enddo
       enddo
!$acc end parallel

    microp2_select: select case(microp_scheme)

       case("mp_thompson")
          ni   => scalars(index_ni,:,:)
          nr   => scalars(index_nr,:,:)

          do j = jts,jte
          do i = its,ite
             muc_p(i,j) = mu_c(i)
             ntc_p(i,j) = nt_c(i)
          enddo
          enddo
          do j = jts, jte
          do k = kts, kte
          do i = its, ite
             ni_p(i,k,j) = ni(k,i)
             nr_p(i,k,j) = nr(k,i)
             rainprod_p(i,k,j) = rainprod(k,i)
             evapprod_p(i,k,j) = evapprod(k,i)
          enddo
          enddo
          enddo

       case default

    end select microp2_select

    case default

 end select microp_select_init

 end subroutine microphysics_from_MPAS

!=================================================================================================================
 subroutine microphysics_to_MPAS(configs,mesh,state,time_lev,diag,diag_physics,tend,itimestep,its,ite)
!=================================================================================================================

!input variables:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: mesh

 integer,intent(in):: itimestep,time_lev
 integer,intent(in):: its,ite

!output variables:
 type(mpas_pool_type),intent(inout):: state
 type(mpas_pool_type),intent(inout):: diag
 type(mpas_pool_type),intent(inout):: tend
 type(mpas_pool_type),intent(inout):: diag_physics

!local pointers:
 character(len=StrKIND),pointer:: microp_scheme
 integer,pointer:: index_qv,index_qc,index_qr,index_qi,index_qs,index_qg
  integer:: sindex_qv,sindex_qc,sindex_qr,sindex_qi,sindex_qs,sindex_qg
 integer,pointer:: index_ni,index_nr
 integer:: sindex_ni,sindex_nr
 real(kind=RKIND),dimension(:),pointer    :: surface_pressure,tend_sfc_pressure
 real(kind=RKIND),dimension(:,:),pointer  :: zgrid
 real(kind=RKIND),dimension(:,:),pointer  :: zz,exner,exner_b,pressure_b,rtheta_p,rtheta_b
 real(kind=RKIND),dimension(:,:),pointer  :: rho_zz,theta_m,pressure_p
 real(kind=RKIND),dimension(:,:),pointer  :: rt_diabatic_tend
 real(kind=RKIND),dimension(:,:),pointer  :: qv,qc,qr,qi,qs,qg
 real(kind=RKIND),dimension(:,:),pointer  :: ni,nr
 real(kind=RKIND),dimension(:,:),pointer  :: rainprod,evapprod
 real(kind=RKIND),dimension(:,:),pointer  :: re_cloud,re_ice,re_snow
 real(kind=RKIND),dimension(:,:,:),pointer:: scalars

!local variables:
 integer:: icount
 integer:: i,k,j
 real(kind=RKIND):: rho1,rho2,tem1,tem2

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_microp_scheme',microp_scheme)

 call mpas_pool_get_array_gpu(mesh,'zz'   ,zz   )
 call mpas_pool_get_array_gpu(mesh,'zgrid',zgrid)

 call mpas_pool_get_array_gpu(diag,'exner'           ,exner           )
 call mpas_pool_get_array_gpu(diag,'exner_base'      ,exner_b         )
 call mpas_pool_get_array_gpu(diag,'pressure_base'   ,pressure_b      )
 call mpas_pool_get_array_gpu(diag,'pressure_p'      ,pressure_p      )
 call mpas_pool_get_array_gpu(diag,'rtheta_base'     ,rtheta_b        )
 call mpas_pool_get_array_gpu(diag,'rtheta_p'        ,rtheta_p        )
 call mpas_pool_get_array_gpu(diag,'surface_pressure',surface_pressure)

 call mpas_pool_get_array_gpu(diag_physics,'rainprod',rainprod)
 call mpas_pool_get_array_gpu(diag_physics,'evapprod',evapprod)
 call mpas_pool_get_array_gpu(diag_physics,'re_cloud',re_cloud)
 call mpas_pool_get_array_gpu(diag_physics,'re_ice'  ,re_ice  )
 call mpas_pool_get_array_gpu(diag_physics,'re_snow' ,re_snow )

 call mpas_pool_get_array_gpu(tend,'tend_sfc_pressure',tend_sfc_pressure)

 call mpas_pool_get_array_gpu(state,'rho_zz' ,rho_zz ,time_lev)
 call mpas_pool_get_array_gpu(state,'theta_m',theta_m,time_lev)

 call mpas_pool_get_dimension(state,'index_qv'  ,index_qv  )
 call mpas_pool_get_dimension(state,'index_qc'  ,index_qc  )
 call mpas_pool_get_dimension(state,'index_qr'  ,index_qr  )
 call mpas_pool_get_dimension(state,'index_qi'  ,index_qi  )
 call mpas_pool_get_dimension(state,'index_qs'  ,index_qs  )
 call mpas_pool_get_dimension(state,'index_qg'  ,index_qg  )
 call mpas_pool_get_dimension(state,'index_ni'  ,index_ni  )
 call mpas_pool_get_dimension(state,'index_nr'  ,index_nr  )

 sindex_qv = index_qv
 sindex_qc = index_qc
 sindex_qr = index_qr
 sindex_qi = index_qi
 sindex_qs = index_qs
 sindex_qg = index_qg
 sindex_ni = index_ni
 sindex_nr = index_nr
 call mpas_pool_get_array_gpu(state,'scalars',scalars,time_lev)
 !qv   => scalars(index_qv,:,:)
 !qc   => scalars(index_qc,:,:)
 !qr   => scalars(index_qr,:,:)

 call mpas_pool_get_array_gpu(tend,'rt_diabatic_tend',rt_diabatic_tend)

!update variables needed in the dynamical core:
 do j = jts,jte
!$acc parallel vector_length(32)
!$acc loop gang vector collapse(2)
 do i = its,ite
 do k = kts,kte
    scalars(sindex_qv,k,i) = qv_p(i,k,j)
    scalars(sindex_qc,k,i) = qc_p(i,k,j)
    scalars(sindex_qr,k,i) = qr_p(i,k,j)

    !potential temperature and diabatic forcing:
    rt_diabatic_tend(k,i) = theta_m(k,i)
    theta_m(k,i) = th_p(i,k,j) * (1. + R_v/R_d * qv_p(i,k,j))
    rt_diabatic_tend(k,i) = (theta_m(k,i) - rt_diabatic_tend(k,i)) / dt_dyn

    !density-weighted perturbation potential temperature:
    rtheta_p(k,i) = rho_zz(k,i) * theta_m(k,i) - rtheta_b(k,i)

    !exner function:
    exner(k,i) = (zz(k,i)*(R_d/P0)*(rtheta_p(k,i)+rtheta_b(k,i)))**rcv

    !pertubation pressure:
    pressure_p(k,i) = zz(k,i)*R_d*(exner(k,i)*rtheta_p(k,i) &
                    + (exner(k,i)-exner_b(k,i))*rtheta_b(k,i))

 enddo
 enddo
 !$acc end parallel
 enddo

!updates the surface pressure and calculates the surface pressure tendency:
 do j = jts,jte
 !$acc parallel vector_length(32)
!$acc loop gang vector private(tem1,tem2,rho1,rho2)
 do i = its,ite
    tem1 = zgrid(2,i)-zgrid(1,i)
    tem2 = zgrid(3,i)-zgrid(2,i)
    rho1 = rho_zz(1,i) * zz(1,i) * (1. + qv_p(i,1,j))
    rho2 = rho_zz(2,i) * zz(2,i) * (1. + qv_p(i,2,j))

    tend_sfc_pressure(i) = surface_pressure(i)
!   surface_pressure(i)  = 0.5*gravity*(zgrid(2,i)-zgrid(1,i)) &
!                        * (rho1 + 0.5*(rho2-rho1)*tem1/(tem1+tem2))
    surface_pressure(i)  = 0.5*gravity*(zgrid(2,i)-zgrid(1,i)) &
                         * (rho1 - 0.5*(rho2-rho1)*tem1/(tem1+tem2))
    surface_pressure(i)  = surface_pressure(i) + pressure_p(1,i) + pressure_b(1,i)
    tend_sfc_pressure(i)   = (surface_pressure(i)-tend_sfc_pressure(i)) / dt_dyn
 enddo
 !$acc end parallel
 enddo

!variables specific to different cloud microphysics schemes:
 microp_select_init: select case(microp_scheme)

    case ("mp_thompson","mp_wsm6")
!       qi   => scalars(index_qi,:,:)
!       qs   => scalars(index_qs,:,:)
!       qg   => scalars(index_qg,:,:)
       do j = jts, jte
!$acc parallel vector_length(32)
!$acc loop gang vector collapse(2)
       do k = kts, kte
       do i = its, ite
          scalars(sindex_qi,k,i) = qi_p(i,k,j)
          scalars(sindex_qs,k,i) = qs_p(i,k,j)
          scalars(sindex_qg,k,i) = qg_p(i,k,j)
          re_cloud(k,i) = recloud_p(i,k,j)
          re_ice(k,i)   = reice_p(i,k,j)
          re_snow(k,i)  = resnow_p(i,k,j)
       enddo
       enddo
!$acc end parallel
       enddo

    microp2_select: select case(microp_scheme)

       case("mp_thompson")
          ni   => scalars(index_ni,:,:)
          nr   => scalars(index_nr,:,:)

          do j = jts, jte
          do k = kts, kte
          do i = its, ite
             ni(k,i) = ni_p(i,k,j)
             nr(k,i) = nr_p(i,k,j)
             rainprod(k,i) = rainprod_p(i,k,j)
             evapprod(k,i) = evapprod_p(i,k,j)
          enddo
          enddo
          enddo

       case default

    end select microp2_select

    case default

 end select microp_select_init

!!$acc update device(zz, zgrid, exner, exner_b, pressure_b, pressure_p, rtheta_b, &
!!$acc               rtheta_p, surface_pressure, rainprod, evapprod, re_cloud, re_ice, &
!!$acc               re_snow, tend_sfc_pressure, rho_zz, theta_m, scalars, rt_diabatic_tend)
 end subroutine microphysics_to_MPAS

!=================================================================================================================
 end module mpas_atmphys_interface
!=================================================================================================================
