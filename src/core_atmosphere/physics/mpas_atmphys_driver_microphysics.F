! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module mpas_atmphys_driver_microphysics
 use mpas_kind_types
 use mpas_pool_routines
 use mpas_timer, only : mpas_timer_start, mpas_timer_stop

 use mpas_atmphys_constants
 use mpas_atmphys_init_microphysics
 use mpas_atmphys_interface
 use mpas_atmphys_vars

!wrf physics:
 use module_mp_kessler
 use module_mp_thompson
 use module_mp_wsm6,only: wsm6
 use mp_wsm6,only: mp_wsm6_init,refl10cm_wsm6
 use microphy_p3,only: p3_init_for_mpas,mp_p3_wrapper_mpas


 implicit none
 private
 public:: allocate_microphysics,   &
          deallocate_microphysics, &
          driver_microphysics,     &
          init_microphysics


!MPAS driver for parameterization of cloud microphysics processes.
!Laura D. Fowler (send comments to laura@ucar.edu).
!2013-05-01.
!
! subroutines in mpas_atmphys_driver_microphysics:
! ------------------------------------------------
! allocate_microphysics     : allocate local arrays for parameterization of cloud microphysics.
! deallocate_microphysics   : deallocate local arrays for parameterization of cloud microphysics.
! microphysics_init         : initialization of individual cloud microphysics schemes.
! driver_microphysics       : main driver (called from mpas_atm_time_integration).
! precip_from_MPAS          : initialize timestep local arrays for precipitation.
! precip_to_MPAS            : copy local arrays to MPAS arrays.
! compute_radar_reflectivity: compute radar reflectivities.
! compute_relhum            : compute relative humidity.
!
! WRF physics called from microphysics_driver:
! --------------------------------------------
! * module_mp_kessler : Kessler cloud microphysics.
! * module_mp_thompson: Thompson cloud microphysics.
! * module_mp_wsm6    : WSM6 cloud microphysics.
! * microphy_p3       : P3 cloud microphysics (module_mp_p3.F).
!
! comments:
! unlike all the other physics parameterizations, parameterizations of cloud microphysics schemes
! are called at the bottom of subroutine atm_srk3 in module atm_time_integration.
!
! add-ons and modifications to sourcecode:
! ----------------------------------------
! * removed call to the Thompson cloud microphysics scheme until scheme is updated to that in WRF revision 3.5.
!   Laura D. Fowler (laura@ucar.edu) / 2013-05-29.
! * added subroutine compute_relhum to calculate the relative humidity using the functions rslf and rsif from
!   the Thompson cloud microphysics scheme.
!   Laura D. Fowler (laura@ucar.edu) / 2013-07-12.
! * removed the argument tend from the call to microphysics_from_MPAS (not needed).
!   Laura D. Fowler (laura@ucar.edu) / 2013-11-07.
! * in call to subroutine wsm6, replaced the variable g (that originally pointed to gravity) with gravity,
!   for simplicity.
!   Laura D. Fowler (laura@ucar.edu) / 2014-03-21.
! * throughout the sourcecode, replaced all "var_struct" defined arrays by local pointers.
!   Laura D. Fowler (laura@ucar.edu) / 2014-04-22.
! * modified sourcecode to use pools.
!   Laura D. Fowler (laura@ucar.edu) / 2014-05-15.
! * moved the variable relhum from the diag_physics to the diag pool. Changed the argument
!   list for the subroutine compute_relhum accordingly.
!   Laura D. Fowler (laura@ucar.edu) / 2015-04-22.
! * added parameterization of the Thompson cloud microphysics from WRF version 3.8.
!   Laura D. Fowler (laura@ucar.edu) / 2016-03-28.
! * in subroutine compute_relhum, multiply relhum by 100. so that it has the same unit as in the initial
!   conditions.
!   Laura D. Fowler (laura@ucar.edu) / 2016-06-20.
! * added parameterization of the WSM6 cloud microphysics from WRF version 3.8.1. To initialize WSM6 as in its
!   original version, set the hail_option to 0.
!   Laura D. Fowler (laura@ucar.edu) / 2016-09-19.
! * since we removed the local variable microp_scheme from mpas_atmphys_vars.F, now defines microp_scheme as a
!   pointer to config_microp_scheme.
!   Laura D. Fowler (laura@ucar.edu) / 2917-02-16.
! * added the P3 (Predicted Particle Properties) cloud microphysics, with one to three free ice categories and
!   optional triple-moment ice, predicted liquid fraction and predicted aerosols.
!   Hugh Morrison (morrison@ucar.edu) / 2026-10-08.

!--- initialization option for WSM6 from WRF version 3.8.1. this option could also be set as a namelist parameter.
 integer,parameter:: hail_opt = 0

!--- P3 equivalent reflectivity (dBZ) computed inside P3 (diag_ze) at the last call to the P3 cloud microphysics;
!    used in compute_radar_reflectivity.
 real(kind=RKIND),dimension(:,:,:),allocatable:: refl_p3_p


 contains


!=================================================================================================================
 subroutine allocate_microphysics(configs)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs

!local pointers:
 character(len=StrKIND),pointer:: microp_scheme
 logical,pointer:: p3_aerosols

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_microp_scheme',microp_scheme)

!sounding variables:
 if(.not.allocated(rho_p) ) allocate(rho_p(ims:ime,kms:kme,jms:jme) )
 if(.not.allocated(th_p)  ) allocate(th_p(ims:ime,kms:kme,jms:jme)  )
 if(.not.allocated(pi_p)  ) allocate(pi_p(ims:ime,kms:kme,jms:jme)  )
 if(.not.allocated(pres_p)) allocate(pres_p(ims:ime,kms:kme,jms:jme))
 if(.not.allocated(z_p)   ) allocate(z_p(ims:ime,kms:kme,jms:jme)   )
 if(.not.allocated(dz_p)  ) allocate(dz_p(ims:ime,kms:kme,jms:jme)  )
 if(.not.allocated(w_p)   ) allocate(w_p(ims:ime,kms:kme,jms:jme)   )

!mass mixing ratios:
 if(.not.allocated(qv_p)) allocate(qv_p(ims:ime,kms:kme,jms:jme))
 if(.not.allocated(qc_p)) allocate(qc_p(ims:ime,kms:kme,jms:jme))
 if(.not.allocated(qr_p)) allocate(qr_p(ims:ime,kms:kme,jms:jme))

 !surface precipitation:
 if(.not.allocated(rainnc_p) ) allocate(rainnc_p(ims:ime,jms:jme) )
 if(.not.allocated(rainncv_p)) allocate(rainncv_p(ims:ime,jms:jme))

 microp_select: select case(trim(microp_scheme))
    case ("mp_thompson","mp_thompson_aerosols","mp_wsm6")
       !mass mixing ratios:
       if(.not.allocated(qi_p)) allocate(qi_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(qs_p)) allocate(qs_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(qg_p)) allocate(qg_p(ims:ime,kms:kme,jms:jme))

       !surface precipitation:
       if(.not.allocated(sr_p)        ) allocate(sr_p(ims:ime,jms:jme)        )
       if(.not.allocated(snownc_p)    ) allocate(snownc_p(ims:ime,jms:jme)    )
       if(.not.allocated(snowncv_p)   ) allocate(snowncv_p(ims:ime,jms:jme)   )
       if(.not.allocated(graupelnc_p) ) allocate(graupelnc_p(ims:ime,jms:jme) )
       if(.not.allocated(graupelncv_p)) allocate(graupelncv_p(ims:ime,jms:jme))

       !cloud water,cloud ice,and snow effective radii:
       if(.not.allocated(recloud_p)) allocate(recloud_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(reice_p)  ) allocate(reice_p(ims:ime,kms:kme,jms:jme)  )
       if(.not.allocated(resnow_p) ) allocate(resnow_p(ims:ime,kms:kme,jms:jme) )

       !precipitation flux:
       if(.not.allocated(rainprod_p)) allocate(rainprod_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(evapprod_p)) allocate(evapprod_p(ims:ime,kms:kme,jms:jme))

    microp2_select: select case(trim(microp_scheme))
       case("mp_thompson","mp_thompson_aerosols")
          if(.not.allocated(ntc_p)) allocate(ntc_p(ims:ime,jms:jme))
          if(.not.allocated(muc_p)) allocate(muc_p(ims:ime,jms:jme))
          if(.not.allocated(ni_p) ) allocate(ni_p(ims:ime,kms:kme,jms:jme))
          if(.not.allocated(nr_p) ) allocate(nr_p(ims:ime,kms:kme,jms:jme))

          microp3_select: select case(trim(microp_scheme))
             case("mp_thompson_aerosols")
                if(.not.allocated(nifa2d_p)) allocate(nifa2d_p(ims:ime,jms:jme))
                if(.not.allocated(nwfa2d_p)) allocate(nwfa2d_p(ims:ime,jms:jme))
                if(.not.allocated(nc_p)    ) allocate(nc_p(ims:ime,kms:kme,jms:jme)  )
                if(.not.allocated(nifa_p)  ) allocate(nifa_p(ims:ime,kms:kme,jms:jme))
                if(.not.allocated(nwfa_p)  ) allocate(nwfa_p(ims:ime,kms:kme,jms:jme))

             case default
          end select microp3_select

       case default
    end select microp2_select

    case ("mp_p3")
       !the P3 ice-category scalars are used directly from state%scalars; only the bulk hydrometeors, the
       !numbers and the diagnostics need local arrays:
       if(.not.allocated(nr_p)) allocate(nr_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(qi_p)) allocate(qi_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(qs_p)) allocate(qs_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(qg_p)) allocate(qg_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(ni_p)) allocate(ni_p(ims:ime,kms:kme,jms:jme))

       if(.not.allocated(rainprod_p)) allocate(rainprod_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(evapprod_p)) allocate(evapprod_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(refl_p3_p) ) allocate(refl_p3_p(ims:ime,kms:kme,jms:jme) )
       if(.not.allocated(nc_p)) allocate(nc_p(ims:ime,kms:kme,jms:jme))

       !aerosol numbers and their surface source only with predicted aerosols:
       call mpas_pool_get_config(configs,'config_p3_aerosols',p3_aerosols)
       if(p3_aerosols) then
          if(.not.allocated(nwfa_p)  ) allocate(nwfa_p(ims:ime,kms:kme,jms:jme))
          if(.not.allocated(nifa_p)  ) allocate(nifa_p(ims:ime,kms:kme,jms:jme))
          if(.not.allocated(nwfa2d_p)) allocate(nwfa2d_p(ims:ime,jms:jme))
          if(.not.allocated(nifa2d_p)) allocate(nifa2d_p(ims:ime,jms:jme))
       endif

       if(.not.allocated(sr_p)        ) allocate(sr_p(ims:ime,jms:jme)        )
       if(.not.allocated(snownc_p)    ) allocate(snownc_p(ims:ime,jms:jme)    )
       if(.not.allocated(snowncv_p)   ) allocate(snowncv_p(ims:ime,jms:jme)   )
       if(.not.allocated(graupelnc_p) ) allocate(graupelnc_p(ims:ime,jms:jme) )
       if(.not.allocated(graupelncv_p)) allocate(graupelncv_p(ims:ime,jms:jme))

       !effective radii of cloud water and ice from P3, for the radiation:
       if(.not.allocated(recloud_p)) allocate(recloud_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(reice_p)  ) allocate(reice_p(ims:ime,kms:kme,jms:jme)  )
       if(.not.allocated(resnow_p) ) allocate(resnow_p(ims:ime,kms:kme,jms:jme) )

    case default
 end select microp_select

 end subroutine allocate_microphysics

!=================================================================================================================
 subroutine deallocate_microphysics(configs)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs

!local pointers:
 character(len=StrKIND),pointer:: microp_scheme

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_microp_scheme',microp_scheme)

!sounding variables:
 if(allocated(rho_p) ) deallocate(rho_p )
 if(allocated(th_p)  ) deallocate(th_p  )
 if(allocated(pi_p)  ) deallocate(pi_p  )
 if(allocated(pres_p)) deallocate(pres_p)
 if(allocated(z_p)   ) deallocate(z_p   )
 if(allocated(dz_p)  ) deallocate(dz_p  )
 if(allocated(w_p)   ) deallocate(w_p   )

!mass mixing ratios:
 if(allocated(qv_p)) deallocate(qv_p)
 if(allocated(qc_p)) deallocate(qc_p)
 if(allocated(qr_p)) deallocate(qr_p)

 !surface precipitation:
 if(allocated(rainnc_p) ) deallocate(rainnc_p )
 if(allocated(rainncv_p)) deallocate(rainncv_p)

 microp_select: select case(trim(microp_scheme))
    case ("mp_thompson","mp_thompson_aerosols","mp_wsm6")
       !mass mixing ratios:
       if(allocated(qi_p)) deallocate(qi_p)
       if(allocated(qs_p)) deallocate(qs_p)
       if(allocated(qg_p)) deallocate(qg_p)

       !surface precipitation:
       if(allocated(sr_p)        ) deallocate(sr_p        )
       if(allocated(snownc_p)    ) deallocate(snownc_p    )
       if(allocated(snowncv_p)   ) deallocate(snowncv_p   )
       if(allocated(graupelnc_p) ) deallocate(graupelnc_p )
       if(allocated(graupelncv_p)) deallocate(graupelncv_p)

       !cloud water,cloud ice,and snow effective radii:
       if(allocated(recloud_p)) deallocate(recloud_p)
       if(allocated(reice_p)  ) deallocate(reice_p  )
       if(allocated(resnow_p) ) deallocate(resnow_p )

       !precipitation flux:
       if(allocated(rainprod_p)) deallocate(rainprod_p)
       if(allocated(evapprod_p)) deallocate(evapprod_p)

    microp2_select: select case(trim(microp_scheme))
       case("mp_thompson","mp_thompson_aerosols")
          if(allocated(ntc_p)) deallocate(ntc_p)
          if(allocated(muc_p)) deallocate(muc_p)
          if(allocated(ni_p) ) deallocate(ni_p )
          if(allocated(nr_p) ) deallocate(nr_p )

          microp3_select: select case(trim(microp_scheme))
             case("mp_thompson_aerosols")
                if(allocated(nifa2d_p)) deallocate(nifa2d_p)
                if(allocated(nwfa2d_p)) deallocate(nwfa2d_p)
                if(allocated(nc_p)    ) deallocate(nc_p    )
                if(allocated(nifa_p)  ) deallocate(nifa_p  )
                if(allocated(nwfa_p)  ) deallocate(nwfa_p  )

             case default
          end select microp3_select

       case default
    end select microp2_select

    case ("mp_p3")
       if(allocated(refl_p3_p)) deallocate(refl_p3_p)

       if(allocated(nc_p)    ) deallocate(nc_p    )
       if(allocated(nwfa_p)  ) deallocate(nwfa_p  )
       if(allocated(nifa_p)  ) deallocate(nifa_p  )
       if(allocated(nwfa2d_p)) deallocate(nwfa2d_p)
       if(allocated(nifa2d_p)) deallocate(nifa2d_p)
       if(allocated(nr_p)    ) deallocate(nr_p    )

       if(allocated(qi_p)) deallocate(qi_p)
       if(allocated(qs_p)) deallocate(qs_p)
       if(allocated(qg_p)) deallocate(qg_p)
       if(allocated(ni_p)) deallocate(ni_p)

       if(allocated(rainprod_p)) deallocate(rainprod_p)
       if(allocated(evapprod_p)) deallocate(evapprod_p)

       if(allocated(sr_p)        ) deallocate(sr_p        )
       if(allocated(snownc_p)    ) deallocate(snownc_p    )
       if(allocated(snowncv_p)   ) deallocate(snowncv_p   )
       if(allocated(graupelnc_p) ) deallocate(graupelnc_p )
       if(allocated(graupelncv_p)) deallocate(graupelncv_p)

       if(allocated(recloud_p)) deallocate(recloud_p)
       if(allocated(reice_p)  ) deallocate(reice_p  )
       if(allocated(resnow_p) ) deallocate(resnow_p )

    case default
 end select microp_select

 end subroutine deallocate_microphysics

!=================================================================================================================
 subroutine init_microphysics(dminfo,configs,mesh,state,time_lev,sfc_input,diag_physics)
!=================================================================================================================

!input arguments:
 type(dm_info),intent(in):: dminfo
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: sfc_input
 integer,intent(in):: time_lev

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: state

!local pointer:
 logical,pointer:: do_restart
 character(len=StrKIND),pointer:: microp_scheme

!local pointers and variables specific to the P3 cloud microphysics:
 character(len=StrKIND),pointer:: p3_lookup_file_dir
 logical,pointer:: p3_trplMomI,p3_liqfrac,p3_predictNc,p3_aerosols
 integer,pointer:: p3_nCat,p3_autoAccr_param
 integer:: p3_stat

!CCPP-compliant flags:
 character(len=StrKIND):: errmsg
 integer:: errflg

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write(' ')
!call mpas_log_write('--- enter subroutine init_microphysics:')

!initialization of CCPP-compliant flags:
 errmsg = ' '
 errflg = 0

 call mpas_pool_get_config(configs,'config_microp_scheme',microp_scheme)
 call mpas_pool_get_config(configs,'config_do_restart'   ,do_restart   )

 microp_select: select case(trim(microp_scheme))
    case("mp_thompson","mp_thompson_aerosols")
       call thompson_init(l_mp_tables)
       call init_thompson_clouddroplets_forMPAS(mesh,sfc_input,diag_physics)

       microp2_select: select case(trim(microp_scheme))
          case("mp_thompson_aerosols")
             call init_thompson_aerosols_forMPAS(do_restart,dminfo,mesh,state,time_lev,diag_physics)

          case default
       end select microp2_select

    case("mp_wsm6")
       call mp_wsm6_init(den0=rho_a,denr=rho_r,dens=rho_s,cl=cliq,cpv=cpv, &
                         hail_opt=hail_opt,errmsg=errmsg,errflg=errflg)

    case("mp_p3")
       !cold-start initialization of the P3 scalars and, with predicted aerosols, of the aerosol numbers nwfa
       !and nifa and of their surface source nwfa2d and nifa2d:
       call mpas_pool_get_config(configs,'config_p3_aerosols',p3_aerosols)
       call init_p3_forMPAS(do_restart,dminfo,configs,state,time_lev)
       if(p3_aerosols) call init_p3_aerosols_forMPAS(do_restart,dminfo,mesh,state,time_lev,diag_physics)

       call mpas_pool_get_config(configs,'config_p3_lookup_file_dir',p3_lookup_file_dir)
       call mpas_pool_get_config(configs,'config_p3_nCat'           ,p3_nCat           )
       call mpas_pool_get_config(configs,'config_p3_trplMomI'       ,p3_trplMomI       )
       call mpas_pool_get_config(configs,'config_p3_liqfrac'        ,p3_liqfrac        )
       call mpas_pool_get_config(configs,'config_p3_predictNc'      ,p3_predictNc      )
       call mpas_pool_get_config(configs,'config_p3_autoAccr_param' ,p3_autoAccr_param )

       !warm-rain autoconversion and accretion: P3 has no branch for values other than 1, 2 and 3, and would
       !then switch off autoconversion, self-collection and accretion:
       if(p3_autoAccr_param < 1 .or. p3_autoAccr_param > 3) then
          write(mpas_err_message,'(A,I0,A)') 'config_p3_autoAccr_param = ',p3_autoAccr_param, &
               ' is invalid (valid: 1, 2 or 3)'
          call physics_error_fatal(trim(mpas_err_message))
       endif

       !predicted aerosols require double-moment cloud water (P3 otherwise writes a warning to stdout at every
       !grid point and time step):
       if(p3_aerosols .and. .not. p3_predictNc) &
          call physics_error_fatal('config_p3_aerosols = true requires config_p3_predictNc = true')

       !P3 lookup tables and parameters:
       p3_stat = 0
       call p3_init_for_mpas(dminfo,trim(p3_lookup_file_dir),p3_nCat,p3_trplMomI,p3_liqfrac,      &
                             stat=p3_stat,abort_on_err=.true.,model='WRF',                        &
                             autoAccr_param_in=p3_autoAccr_param,p3_aerosols=p3_aerosols,         &
                             predictNc=p3_predictNc)
       if(p3_stat /= 0) then
          write(mpas_err_message,'(A,I0)') 'p3_init_for_mpas failed, stat = ',p3_stat
          call physics_error_fatal(trim(mpas_err_message))
       endif

    case default
 end select microp_select

!call mpas_log_write('--- end subroutine init_microphysics:')

 end subroutine init_microphysics

!=================================================================================================================
 subroutine driver_microphysics(configs,mesh,state,time_lev,diag,diag_physics,tend_physics,tend,itimestep,its,ite)
!=================================================================================================================

 use mpas_constants, only : rvord

!input arguments:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: mesh

 integer,intent(in):: time_lev
 integer,intent(in):: itimestep
 integer,intent(in):: its,ite

!inout arguments:
 type(mpas_pool_type),intent(inout):: state
 type(mpas_pool_type),intent(inout):: diag
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: tend_physics
 type(mpas_pool_type),intent(inout):: tend

!local pointers:
 character(len=StrKIND),pointer:: microp_scheme

!local variables and arrays:
 integer:: istep

!local pointers, variables and arrays specific to the P3 cloud microphysics:
 character(len=StrKIND),pointer:: pbl_scheme
 logical,pointer:: p3_trplMomI,p3_liqfrac,p3_aerosols
 integer,pointer:: p3_nCat
 integer,pointer:: idx_qit,idx_qni,idx_qir,idx_qib,idx_qzi,idx_qli
 real(kind=RKIND),dimension(:),pointer:: hpbl_mp
 real(kind=RKIND),dimension(:,:),pointer:: qke_mp,kzh_mp,sigw_p3
 real(kind=RKIND),dimension(:,:,:),pointer:: scalars
 real(kind=RKIND),dimension(:,:,:),pointer:: p3_rhoi,p3_vmi,p3_dmi

 integer:: i,j,k,c
 integer:: sigw_opt
 real(kind=RKIND):: sigw_l,zagl
 real(kind=RKIND):: rain_step_mm,snow_step_mm,graup_step_mm,ice_step_mm,sr_step
 real(kind=RKIND),dimension(:),allocatable:: t1d,p1d,w1d,sigw1d,dz1d
 real(kind=RKIND),dimension(:),allocatable:: qv1d,qc1d,qr1d,qi1d,qs1d,qg1d,ni1d,nr1d,nc1d,nwfa1d,nifa1d
 real(kind=RKIND),dimension(:),allocatable:: thold1d,qvold1d
 real(kind=RKIND),dimension(:),allocatable:: rainprod1d,evapprod1d,zdbz1d,effc1d,effiave1d
 real(kind=RKIND),dimension(:,:),allocatable:: qit1d,qni1d,qir1d,qib1d,qzi1d,qli1d
 real(kind=RKIND),dimension(:,:),allocatable:: vmi1d,dmi1d,rhoi1d,dhmax1d

!sub-grid vertical velocity for droplet activation with the YSU PBL: mixing length c*hpbl (with a minimum
!length) inside the PBL, and the mixing length of the YSU free-atmosphere closure above the PBL:
 real(kind=RKIND),parameter:: sigw_c_ysu     = 0.1_RKIND
 real(kind=RKIND),parameter:: sigw_lmin_ysu  = 10._RKIND
 real(kind=RKIND),parameter:: sigw_lfree_ysu = 30._RKIND

!CCPP-compliant flags:
 character(len=StrKIND):: errmsg
 integer:: errflg

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write('')
!call mpas_log_write('---enter subroutine driver_microphysics:')

!initialization of CCPP-compliant flags:
 errmsg = ' '
 errflg = 0

 call mpas_pool_get_config(configs,'config_microp_scheme',microp_scheme)

!... allocation of microphysics arrays:
!$OMP MASTER
 call allocate_microphysics(configs)
!$OMP END MASTER
!$OMP BARRIER

!... initialization of precipitation related arrays:
 call precip_from_MPAS(configs,diag_physics,its,ite)

!... initialization of soundings for non-hydrostatic dynamical cores.
 call microphysics_from_MPAS(configs,mesh,state,time_lev,diag,diag_physics,tend_physics,its,ite)

!... call to different cloud microphysics schemes:
 microp_select: select case(trim(microp_scheme))
    case ("mp_kessler")
       call mpas_timer_start('mp_kessler')
       call kessler( &
                  t        = th_p      , qv    = qv_p  , qc     = qc_p     ,                &
                  qr       = qr_p      , rho   = rho_p , pii    = pi_p     ,                &
                  dt_in    = dt_microp , z     = z_p   , xlv    = xlv      ,                &
                  cp       = cp        , ep2   = ep_2  , svp1   = svp1     ,                &
                  svp2     = svp2      , svp3  = svp3  , svpt0  = svpt0    ,                &
                  rhowater = rho_w     , dz8w  = dz_p  , rainnc = rainnc_p ,                &
                  rainncv  = rainncv_p ,                                                    &
                  ids = ids , ide = ide , jds = jds , jde = jde , kds = kds , kde = kde   , &
                  ims = ims , ime = ime , jms = jms , jme = jme , kms = kms , kme = kme   , &
                  its = its , ite = ite , jts = jts , jte = jte , kts = kts , kte = kte     &
                   )
       call mpas_timer_stop('mp_kessler')

    case ("mp_thompson")
       call mpas_timer_start('mp_thompson')
       istep = 1
       do while (istep .le. n_microp)
       call mp_gt_driver( &
                  th        = th_p        , qv         = qv_p         , qc         = qc_p         , &
                  qr        = qr_p        , qi         = qi_p         , qs         = qs_p         , &
                  qg        = qg_p        , ni         = ni_p         , nr         = nr_p         , &
                  pii       = pi_p        , p          = pres_p       , dz         = dz_p         , &
                  w         = w_p         , dt_in      = dt_microp    , itimestep  = itimestep    , &
                  rainnc    = rainnc_p    , rainncv    = rainncv_p    , snownc     = snownc_p     , &
                  snowncv   = snowncv_p   , graupelnc  = graupelnc_p  , graupelncv = graupelncv_p , &
                  sr        = sr_p        , rainprod   = rainprod_p   , evapprod   = evapprod_p   , &
                  re_cloud  = recloud_p   , re_ice     = reice_p      , re_snow    = resnow_p     , &
                  has_reqc  = has_reqc    , has_reqi   = has_reqi     , has_reqs   = has_reqs     , &
                  ntc       = ntc_p       , muc        = muc_p                                    , &
                  ids = ids , ide = ide , jds = jds , jde = jde , kds = kds , kde = kde           , &
                  ims = ims , ime = ime , jms = jms , jme = jme , kms = kms , kme = kme           , &
                  its = its , ite = ite , jts = jts , jte = jte , kts = kts , kte = kte             &
                           )
       istep = istep + 1
       enddo
       call mpas_timer_stop('mp_thompson')

    case ("mp_thompson_aerosols")
       call mpas_timer_start('mp_thompson_aerosols')
       istep = 1
       do while (istep .le. n_microp)
       call mp_gt_driver( &
                  th        = th_p        , qv         = qv_p         , qc         = qc_p         , &
                  qr        = qr_p        , qi         = qi_p         , qs         = qs_p         , &
                  qg        = qg_p        , ni         = ni_p         , nr         = nr_p         , &
                  pii       = pi_p        , p          = pres_p       , dz         = dz_p         , &
                  w         = w_p         , dt_in      = dt_microp    , itimestep  = itimestep    , &
                  rainnc    = rainnc_p    , rainncv    = rainncv_p    , snownc     = snownc_p     , &
                  snowncv   = snowncv_p   , graupelnc  = graupelnc_p  , graupelncv = graupelncv_p , &
                  sr        = sr_p        , rainprod   = rainprod_p   , evapprod   = evapprod_p   , &
                  re_cloud  = recloud_p   , re_ice     = reice_p      , re_snow    = resnow_p     , &
                  has_reqc  = has_reqc    , has_reqi   = has_reqi     , has_reqs   = has_reqs     , &
                  nc        = nc_p        , nifa       = nifa_p       , nwfa       = nwfa_p       , &
                  nifa2d    = nifa2d_p    , nwfa2d     = nwfa2d_p     , ntc        = ntc_p        , &
                  muc       = muc_p       ,                                                         &
                  ids = ids , ide = ide , jds = jds , jde = jde , kds = kds , kde = kde           , &
                  ims = ims , ime = ime , jms = jms , jme = jme , kms = kms , kme = kme           , &
                  its = its , ite = ite , jts = jts , jte = jte , kts = kts , kte = kte             &
                           )
          istep = istep + 1
          enddo
          call mpas_timer_stop('mp_thompson_aerosols')

    case ("mp_wsm6")
       call mpas_timer_start('mp_wsm6')
       call wsm6( &
                  th        = th_p        , q          = qv_p         , qc        = qc_p      , &
                  qr        = qr_p        , qi         = qi_p         , qs        = qs_p      , &
                  qg        = qg_p        , den        = rho_p        , pii       = pi_p      , &
                  p         = pres_p      , delz       = dz_p         , delt      = dt_microp , &
                  g         = gravity     , cpd        = cp           , cpv       = cpv       , &
                  rd        = R_d         , rv         = R_v          , t0c       = svpt0     , &
                  ep1       = ep_1        , ep2        = ep_2         , qmin      = epsilon   , &
                  xls       = xls         , xlv0       = xlv          , xlf0      = xlf       , &
                  den0      = rho_a       , denr       = rho_w        , cliq      = cliq      , &
                  cice      = cice        , psat       = psat         , rain      = rainnc_p  , &
                  rainncv   = rainncv_p   , snow       = snownc_p     , snowncv   = snowncv_p , &
                  graupel   = graupelnc_p , graupelncv = graupelncv_p , sr        = sr_p      , &
                  re_cloud  = recloud_p   , re_ice     = reice_p      , re_snow   = resnow_p  , &
                  has_reqc  = has_reqc    , has_reqi   = has_reqi     , has_reqs  = has_reqs  , &
                  re_qc_bg  = re_qc_bg    , re_qi_bg   = re_qi_bg     , re_qs_bg  = re_qs_bg  , &
                  re_qc_max = re_qc_max   , re_qi_max  = re_qi_max    , re_qs_max = re_qs_max , &
                  errmsg    = errmsg      , errflg     = errflg                               , &
                  ids = ids , ide = ide , jds = jds , jde = jde , kds = kds , kde = kde       , &
                  ims = ims , ime = ime , jms = jms , jme = jme , kms = kms , kme = kme       , &
                  its = its , ite = ite , jts = jts , jte = jte , kts = kts , kte = kte         &
               )
       call mpas_timer_stop('mp_wsm6')

    case ("mp_p3")
       call mpas_timer_start('mp_p3')

       call mpas_pool_get_config(configs,'config_p3_nCat'    ,p3_nCat    )
       call mpas_pool_get_config(configs,'config_p3_trplMomI',p3_trplMomI)
       call mpas_pool_get_config(configs,'config_p3_liqfrac' ,p3_liqfrac )
       call mpas_pool_get_config(configs,'config_p3_aerosols',p3_aerosols)

       !the P3 ice-category scalars are read from and written to state%scalars directly; the scalars of
       !category c are at index_p3_xxx + c-1:
       call mpas_pool_get_array(state,'scalars',scalars,time_lev)
       call mpas_pool_get_dimension(state,'index_p3_qit',idx_qit)
       call mpas_pool_get_dimension(state,'index_p3_qni',idx_qni)
       call mpas_pool_get_dimension(state,'index_p3_qir',idx_qir)
       call mpas_pool_get_dimension(state,'index_p3_qib',idx_qib)
       call mpas_pool_get_dimension(state,'index_p3_qzi',idx_qzi)
       call mpas_pool_get_dimension(state,'index_p3_qli',idx_qli)

       !bulk density, mass-weighted fall speed and mean diameter of each ice category (0 where the category is
       !empty), from the last substep. p3_rhoi is also used in p3cu_add_tend (mpas_atmphys_p3_cumix) to choose
       !the ice category of the ice detrained by convection when config_p3_nCat > 1:
       call mpas_pool_get_array(diag_physics,'p3_rhoi',p3_rhoi)
       call mpas_pool_get_array(diag_physics,'p3_vmi' ,p3_vmi )
       call mpas_pool_get_array(diag_physics,'p3_dmi' ,p3_dmi )

       !sub-grid vertical velocity for droplet activation, at layer centres (as in WRF/CM1 P3: w' = sqrt(2/3 TKE),
       !isotropic turbulence). MYNN: TKE = qke/2 (MYNN does not fill tke_pbl in MPAS). YSU (no TKE): w' = kzh/l,
       !with kzh averaged from the lower faces of the layers onto the layer centres; l = 0.1*hpbl (>= 10 m) inside
       !the PBL, which gives ~0.65 w* at the peak of the YSU K-profile, the peak of the Lenschow et al. (1980)
       !profile, and l = 30 m (mixing length of the YSU free-atmosphere closure) above it. Other PBL schemes: 0.
       call mpas_pool_get_config(configs,'config_pbl_scheme',pbl_scheme)
       call mpas_pool_get_array(diag_physics,'sigw_p3',sigw_p3)
       sigw_opt = 0
       if(trim(pbl_scheme) == 'bl_mynn') then
          call mpas_pool_get_array(diag_physics,'qke',qke_mp)
          sigw_opt = 1
       elseif(trim(pbl_scheme) == 'bl_ysu') then
          call mpas_pool_get_array(diag_physics,'kzh',kzh_mp)
          call mpas_pool_get_array(diag_physics,'hpbl',hpbl_mp)
          sigw_opt = 2
       endif

       allocate(t1d(kts:kte),p1d(kts:kte),w1d(kts:kte),sigw1d(kts:kte),dz1d(kts:kte))
       allocate(qv1d(kts:kte),qc1d(kts:kte),qr1d(kts:kte),qi1d(kts:kte),qs1d(kts:kte),qg1d(kts:kte))
       allocate(ni1d(kts:kte),nr1d(kts:kte),nc1d(kts:kte),nwfa1d(kts:kte),nifa1d(kts:kte))
       allocate(thold1d(kts:kte),qvold1d(kts:kte))
       allocate(rainprod1d(kts:kte),evapprod1d(kts:kte),zdbz1d(kts:kte),effc1d(kts:kte),effiave1d(kts:kte))
       allocate(qit1d(kts:kte,p3_nCat),qni1d(kts:kte,p3_nCat),qir1d(kts:kte,p3_nCat),qib1d(kts:kte,p3_nCat))
       allocate(qzi1d(kts:kte,p3_nCat),qli1d(kts:kte,p3_nCat))
       allocate(vmi1d(kts:kte,p3_nCat),dmi1d(kts:kte,p3_nCat),rhoi1d(kts:kte,p3_nCat),dhmax1d(kts:kte,p3_nCat))

       do j = jts,jte
       do i = its,ite

          !copy the column to the P3 arrays:
          do k = kts,kte
             !"old" theta and qv = state at the start of the microphysics time step (all substeps):
             thold1d(k) = th_p(i,k,j)
             qvold1d(k) = qv_p(i,k,j)
             qv1d(k) = qv_p(i,k,j)
             qc1d(k) = qc_p(i,k,j)
             qr1d(k) = qr_p(i,k,j)
             nr1d(k) = nr_p(i,k,j)
             p1d(k)  = pres_p(i,k,j)
             !w at layer centres (w_p is filled for kts:kte only, and w = 0 at the model top):
             if(k < kte) then
                w1d(k) = 0.5_RKIND*(w_p(i,k,j) + w_p(i,k+1,j))
             else
                w1d(k) = 0.5_RKIND*w_p(i,k,j)
             endif
             dz1d(k) = dz_p(i,k,j)

             select case(sigw_opt)
                case(1)
                   sigw1d(k) = sqrt(2._RKIND/3._RKIND*max(0._RKIND,0.5_RKIND*qke_mp(k,i)))
                case(2)
                   zagl = z_p(i,k,j) - z_p(i,kts,j) + 0.5_RKIND*dz_p(i,k,j)
                   if(zagl < hpbl_mp(i)) then
                      sigw_l = max(sigw_c_ysu*hpbl_mp(i),sigw_lmin_ysu)
                   else
                      sigw_l = sigw_lfree_ysu
                   endif
                   if(k < kte) then
                      sigw1d(k) = 0.5_RKIND*(kzh_mp(k,i) + kzh_mp(k+1,i))/sigw_l
                   else
                      sigw1d(k) = 0.5_RKIND*kzh_mp(k,i)/sigw_l
                   endif
                case default
                   sigw1d(k) = 0._RKIND
             end select
             if(sigw1d(k) < 0._RKIND) sigw1d(k) = 0._RKIND
             sigw_p3(k,i) = sigw1d(k)

             t1d(k)  = th_p(i,k,j) * pi_p(i,k,j)
             nc1d(k) = nc_p(i,k,j)
             if(p3_aerosols) then
                nwfa1d(k) = nwfa_p(i,k,j)
                nifa1d(k) = nifa_p(i,k,j)
             else
                nwfa1d(k) = 0._RKIND
                nifa1d(k) = 0._RKIND
             endif
          enddo

          do c = 1,p3_nCat
             do k = kts,kte
                qit1d(k,c) = scalars(idx_qit+c-1,k,i)
                qni1d(k,c) = scalars(idx_qni+c-1,k,i)
                qir1d(k,c) = scalars(idx_qir+c-1,k,i)
                qib1d(k,c) = scalars(idx_qib+c-1,k,i)
                if(p3_trplMomI) then
                   qzi1d(k,c) = scalars(idx_qzi+c-1,k,i)
                else
                   qzi1d(k,c) = 0._RKIND
                endif
                if(p3_liqfrac) then
                   qli1d(k,c) = scalars(idx_qli+c-1,k,i)
                else
                   qli1d(k,c) = 0._RKIND
                endif
             enddo
          enddo

          do istep = 1,n_microp

             rain_step_mm = 0._RKIND
             snow_step_mm = 0._RKIND

             call mp_p3_wrapper_mpas( &
                  qv1d,qc1d,qi1d,qr1d,qs1d,qg1d,ni1d,nr1d,nc1d,nwfa1d,nifa1d,thold1d,qvold1d, &
                  t1d,p1d,w1d,sigw1d,dz1d,zdbz1d,effc1d,effiave1d,                            &
                  qit1d,qni1d,qir1d,qib1d,qzi1d,qli1d,vmi1d,dmi1d,rhoi1d,dhmax1d,             &
                  rain_step_mm,snow_step_mm,graup_step_mm,ice_step_mm,                        &
                  itimestep=itimestep,kts=kts,kte=kte,nCat=p3_nCat,dt=dt_microp,              &
                  ii=i,jj=j,rainprod1d=rainprod1d,evapprod1d=evapprod1d)

             !as for the Thompson cloud microphysics, rainprod and evapprod are the rates of the last
             !substep:
             do k = kts,kte
                rainprod_p(i,k,j) = rainprod1d(k)
                evapprod_p(i,k,j) = evapprod1d(k)
             enddo

             !surface source of aerosols in the lowest layer, as for the aerosol-aware Thompson cloud
             !microphysics:
             if(p3_aerosols) then
                nwfa1d(kts) = nwfa1d(kts) + nwfa2d_p(i,j)*dt_microp
                nifa1d(kts) = nifa1d(kts) + nifa2d_p(i,j)*dt_microp
             endif

             !snow ratio = solid / total surface precipitation (P3: all ice reaches the surface as snow):
             if(rain_step_mm > 1.e-12_RKIND) then
                sr_step = min(1._RKIND,max(0._RKIND,snow_step_mm/rain_step_mm))
             else
                sr_step = 0._RKIND
             endif

             !surface precipitation (mm):
             rainnc_p(i,j)     = rainnc_p(i,j) + rain_step_mm
             rainncv_p(i,j)    = rain_step_mm
             snownc_p(i,j)     = snownc_p(i,j) + snow_step_mm
             snowncv_p(i,j)    = snow_step_mm
             sr_p(i,j)         = sr_step
             graupelnc_p(i,j)  = graupelnc_p(i,j) + graup_step_mm
             graupelncv_p(i,j) = graup_step_mm

          enddo

          !copy the column back (the Exner function is not changed by the microphysics). The bulk qi, qs, qg
          !and ni returned by P3 are not used: qi_p, qs_p, qg_p and ni_p are filled in MPAS_to_physics.
          do k = kts,kte
             th_p(i,k,j) = t1d(k) / pi_p(i,k,j)
             qv_p(i,k,j) = qv1d(k)
             qc_p(i,k,j) = qc1d(k)
             qr_p(i,k,j) = qr1d(k)
             nr_p(i,k,j) = nr1d(k)
             nc_p(i,k,j) = nc1d(k)
             if(p3_aerosols) then
                nwfa_p(i,k,j) = nwfa1d(k)
                nifa_p(i,k,j) = nifa1d(k)
             endif

             !equivalent reflectivity (dBZ) from the last substep:
             refl_p3_p(i,k,j) = zdbz1d(k)

             !effective radii (m) for the radiation:
             recloud_p(i,k,j) = max(1.e-9_RKIND,effc1d(k))
             reice_p(i,k,j)   = max(1.e-9_RKIND,effiave1d(k))
             resnow_p(i,k,j)  = max(1.e-9_RKIND,effiave1d(k))
          enddo

          do c = 1,p3_nCat
             do k = kts,kte
                scalars(idx_qit+c-1,k,i) = qit1d(k,c)
                scalars(idx_qni+c-1,k,i) = qni1d(k,c)
                scalars(idx_qir+c-1,k,i) = qir1d(k,c)
                scalars(idx_qib+c-1,k,i) = qib1d(k,c)
                if(p3_trplMomI) scalars(idx_qzi+c-1,k,i) = qzi1d(k,c)
                if(p3_liqfrac)  scalars(idx_qli+c-1,k,i) = qli1d(k,c)
                !diagnostics of each ice category from the last substep:
                p3_rhoi(c,k,i) = rhoi1d(k,c)
                p3_vmi(c,k,i)  = vmi1d(k,c)
                p3_dmi(c,k,i)  = dmi1d(k,c)
             enddo
          enddo

       enddo
       enddo

       deallocate(t1d,p1d,w1d,sigw1d,dz1d)
       deallocate(qv1d,qc1d,qr1d,qi1d,qs1d,qg1d,ni1d,nr1d,nc1d,nwfa1d,nifa1d)
       deallocate(thold1d,qvold1d)
       deallocate(rainprod1d,evapprod1d,zdbz1d,effc1d,effiave1d)
       deallocate(qit1d,qni1d,qir1d,qib1d,qzi1d,qli1d,vmi1d,dmi1d,rhoi1d,dhmax1d)

       call mpas_timer_stop('mp_p3')

    case default
 end select microp_select

!... calculate the 10cm radar reflectivity and relative humidity, if needed:
 if (l_diags) then
    !ensure that we only call compute_radar_reflectivity() if we are using an MPS that supports
    !the computation of simulated radar reflectivity:
    if(trim(microp_scheme) == "mp_wsm6"     .or. &
       trim(microp_scheme) == "mp_thompson" .or. &
       trim(microp_scheme) == "mp_thompson_aerosols" .or. &
       trim(microp_scheme) == "mp_p3") then
       call compute_radar_reflectivity(configs,diag_physics,its,ite)
    else
       call mpas_log_write('*** NOTICE: NOT computing simulated radar reflectivity')
       call mpas_log_write('            since WSM6, Thompson or P3 microphysics scheme was not selected')
    end if

    !calculate the relative humidity over water if the temperature is strictly greater than 0.C,
    !over ice otherwise.
    call compute_relhum(diag,its,ite)
 end if

!... copy updated precipitation from the wrf-physics grid back to the geodesic-dynamics grid:
 call precip_to_MPAS(configs,diag_physics,its,ite)

!... copy updated cloud microphysics variables from the wrf-physics grid back to the geodesic-
!    dynamics grid:
 call microphysics_to_MPAS(configs,mesh,state,time_lev,diag,diag_physics,tend_physics,tend,its,ite)

!... deallocation of all microphysics arrays:
!$OMP BARRIER
!$OMP MASTER
 call deallocate_microphysics(configs)
!$OMP END MASTER

!call mpas_log_write('---enter subroutine driver_microphysics:')
!call mpas_log_write('')

 end subroutine driver_microphysics

!=================================================================================================================
 subroutine precip_from_MPAS(configs,diag_physics,its,ite)
!=================================================================================================================

!input variables:
 type(mpas_pool_type),intent(in):: configs
 integer,intent(in):: its,ite

!output variables:
 type(mpas_pool_type),intent(inout):: diag_physics

!local pointers:
 character(len=StrKIND),pointer:: microp_scheme
 integer,pointer:: nCellsSolve
 real,dimension(:),pointer:: graupelncv,rainncv,snowncv,sr

!local variables and arrays:
 integer:: i,j

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_microp_scheme',microp_scheme)

 call mpas_pool_get_array(diag_physics,'graupelncv',graupelncv)
 call mpas_pool_get_array(diag_physics,'rainncv'   ,rainncv   )
 call mpas_pool_get_array(diag_physics,'snowncv'   ,snowncv   )
 call mpas_pool_get_array(diag_physics,'sr'        ,sr        )

!variables common to all cloud microphysics schemes:
 do j = jts, jte
 do i = its, ite
    rainncv_p(i,j) = 0._RKIND
    rainnc_p(i,j)  = 0._RKIND
 enddo
 enddo

 do i = its,ite
    rainncv(i) = 0._RKIND
 enddo

!variables specific to different cloud microphysics schemes:
 microp_select: select case(trim(microp_scheme))
    case ("mp_thompson","mp_thompson_aerosols","mp_wsm6","mp_p3")
       do j = jts, jte
       do i = its, ite
          snowncv_p(i,j)    = 0._RKIND
          graupelncv_p(i,j) = 0._RKIND
          snownc_p(i,j)     = 0._RKIND
          graupelnc_p(i,j)  = 0._RKIND
          sr_p(i,j)         = 0._RKIND
       enddo
       enddo

       do i = its,ite
          snowncv(i)    = 0._RKIND
          graupelncv(i) = 0._RKIND
          sr(i)         = 0._RKIND
       enddo

    case default
 end select microp_select

 end subroutine precip_from_MPAS

!=================================================================================================================
 subroutine precip_to_MPAS(configs,diag_physics,its,ite)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs
 integer,intent(in):: its,ite

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics

!local pointers:
 character(len=StrKIND),pointer:: microp_scheme
 integer,dimension(:),pointer:: i_rainnc

 real(kind=RKIND),pointer:: config_bucket_rainnc
 real(kind=RKIND),dimension(:),pointer:: precipw
 real(kind=RKIND),dimension(:),pointer:: graupelnc,rainnc,snownc
 real(kind=RKIND),dimension(:),pointer:: graupelncv,rainncv,snowncv,sr

!local variables and arrays:
 integer:: i,j,k
 real(kind=RKIND):: rho_a

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_microp_scheme',microp_scheme       )
 call mpas_pool_get_config(configs,'config_bucket_rainnc',config_bucket_rainnc)

 call mpas_pool_get_array(diag_physics,'i_rainnc'  ,i_rainnc  )
 call mpas_pool_get_array(diag_physics,'precipw'   ,precipw   )
 call mpas_pool_get_array(diag_physics,'graupelnc' ,graupelnc )
 call mpas_pool_get_array(diag_physics,'graupelncv',graupelncv)
 call mpas_pool_get_array(diag_physics,'rainnc'    ,rainnc    )
 call mpas_pool_get_array(diag_physics,'rainncv'   ,rainncv   )
 call mpas_pool_get_array(diag_physics,'snownc'    ,snownc    )
 call mpas_pool_get_array(diag_physics,'snowncv'   ,snowncv   )
 call mpas_pool_get_array(diag_physics,'sr'        ,sr        )

 do i = its,ite
    precipw(i) = 0._RKIND
 enddo

!variables common to all cloud microphysics schemes:
 do j = jts,jte
 do i = its,ite

    !precipitable water:
    do k = kts,kte
       rho_a = rho_p(i,k,j) / (1._RKIND + qv_p(i,k,j))
       precipw(i) = precipw(i) + qv_p(i,k,j) * rho_a * dz_p(i,k,j)
    enddo

    !time-step precipitation:
    rainncv(i) = rainnc_p(i,j)

    !accumulated precipitation:
    rainnc(i) = rainnc(i) + rainncv(i)

   if(l_acrain .and. config_bucket_rainnc.gt.0._RKIND .and. &
      rainnc(i).gt.config_bucket_rainnc) then
      i_rainnc(i) = i_rainnc(i) + 1
      rainnc(i)   = rainnc(i) - config_bucket_rainnc
   endif

 enddo
 enddo

!variables specific to different cloud microphysics schemes:
 microp_select: select case(trim(microp_scheme))
    case ("mp_thompson","mp_thompson_aerosols","mp_wsm6")
       do j = jts,jte
       do i = its,ite
          !time-step precipitation:
          snowncv(i)    = snownc_p(i,j)
          graupelncv(i) = graupelnc_p(i,j)
          sr(i) = (snownc_p(i,j) + graupelnc_p(i,j)) / (rainnc_p(i,j)+1.e-12)

          !accumulated precipitation:
          snownc(i)    = snownc(i) + snowncv(i)
          graupelnc(i) = graupelnc(i) + graupelncv(i)
       enddo
       enddo

    case ("mp_p3")
       do j = jts,jte
       do i = its,ite
          !time-step precipitation:
          snowncv(i)    = snownc_p(i,j)
          graupelncv(i) = graupelnc_p(i,j)
          sr(i)         = sr_p(i,j)

          !accumulated precipitation:
          snownc(i)    = snownc(i) + snowncv(i)
          graupelnc(i) = graupelnc(i) + graupelncv(i)
       enddo
       enddo

    case default
 end select microp_select

 end subroutine precip_to_MPAS

!=================================================================================================================
 subroutine compute_radar_reflectivity(configs,diag_physics,its,ite)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs
 integer,intent(in):: its,ite

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics

!local pointers:
 character(len=StrKIND),pointer:: microp_scheme
 real(kind=RKIND),dimension(:,:),pointer:: refl10cm
 real(kind=RKIND),dimension(:),pointer:: refl10cm_max,refl10cm_1km,refl10cm_1km_max

!local variables and arrays:
 integer:: i,j,k,kp
 real(kind=RKIND),dimension(:),allocatable:: qv1d,qc1d,qr1d,qs1d,qg1d,t1d,p1d,nr1d,dBZ1d,zp
 real(kind=RKIND):: w1,w2

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_microp_scheme',microp_scheme)

 call mpas_pool_get_array(diag_physics,'refl10cm',refl10cm)
 call mpas_pool_get_array(diag_physics,'refl10cm_max',refl10cm_max)
 call mpas_pool_get_array(diag_physics,'refl10cm_1km',refl10cm_1km)
 call mpas_pool_get_array(diag_physics,'refl10cm_1km_max',refl10cm_1km_max)

 microp_select: select case(trim(microp_scheme))
    case ("mp_kessler")
       call physics_error_fatal('--- calculation of radar reflectivity is not available' // &
                                 'with kessler cloud microphysics')

    case ("mp_wsm6")
       if(.not.allocated(p1d)  ) allocate(p1d(kts:kte)  )
       if(.not.allocated(t1d)  ) allocate(t1d(kts:kte)  )
       if(.not.allocated(qv1d) ) allocate(qv1d(kts:kte) )
       if(.not.allocated(qr1d) ) allocate(qr1d(kts:kte) )
       if(.not.allocated(qs1d) ) allocate(qs1d(kts:kte) )
       if(.not.allocated(qg1d) ) allocate(qg1d(kts:kte) )
       if(.not.allocated(dBz1d)) allocate(dBZ1d(kts:kte))
       if(.not.allocated(zp)   ) allocate(zp(kts:kte)   )

       do j = jts,jte
       do i = its,ite
          do k = kts,kte
             p1d(k) = pres_p(i,k,j)
             t1d(k) = th_p(i,k,j) * pi_p(i,k,j)
             qv1d(k)  = qv_p(i,k,j)
             qr1d(k)  = qr_p(i,k,j)
             qs1d(k)  = qs_p(i,k,j)
             qg1d(k)  = qg_p(i,k,j)
             dBZ1d(k) = -35._RKIND
             zp(k) = z_p(i,k,j) - z_p(i,1,j) + 0.5*dz_p(i,k,j) ! height AGL
          enddo

          call refl10cm_wsm6(qv1d,qr1d,qs1d,qg1d,t1d,p1d,dBZ1d,kts,kte)

          kp = 1
          do k = kts,kte
             dBZ1d(k) = max(-35._RKIND,dBZ1d(k))
             refl10cm(k,i) = dBZ1d(k)
             if(zp(k) .lt. 1000.) kp = k
          enddo
          refl10cm_max(i) = maxval(dBZ1d(:))
          w1 = (zp(kp+1)-1000.)/(zp(kp+1)-zp(kp))
          w2 = 1.0 - w1
          refl10cm_1km(i) = w1*dBZ1d(kp) + w2*dBZ1d(kp+1)
          refl10cm_1km_max(i) = max(refl10cm_1km_max(i),refl10cm_1km(i))
       enddo
       enddo

       if(allocated(p1d)  ) deallocate(p1d  )
       if(allocated(t1d)  ) deallocate(t1d  )
       if(allocated(qv1d) ) deallocate(qv1d )
       if(allocated(qr1d) ) deallocate(qr1d )
       if(allocated(qs1d) ) deallocate(qs1d )
       if(allocated(qg1d) ) deallocate(qg1d )
       if(allocated(dBz1d)) deallocate(dBZ1d)
       if(allocated(zp)   ) deallocate(zp   )

    case ("mp_thompson","mp_thompson_aerosols")
       if(.not.allocated(p1d)  ) allocate(p1d(kts:kte)  )
       if(.not.allocated(t1d)  ) allocate(t1d(kts:kte)  )
       if(.not.allocated(qv1d) ) allocate(qv1d(kts:kte) )
       if(.not.allocated(qc1d) ) allocate(qc1d(kts:kte) )
       if(.not.allocated(qr1d) ) allocate(qr1d(kts:kte) )
       if(.not.allocated(qs1d) ) allocate(qs1d(kts:kte) )
       if(.not.allocated(qg1d) ) allocate(qg1d(kts:kte) )
       if(.not.allocated(nr1d) ) allocate(nr1d(kts:kte) )
       if(.not.allocated(dBz1d)) allocate(dBZ1d(kts:kte))
       if(.not.allocated(zp)   ) allocate(zp(kts:kte)   )

       do j = jts,jte
       do i = its,ite
          do k = kts,kte
             p1d(k) = pres_p(i,k,j)
             t1d(k) = th_p(i,k,j) * pi_p(i,k,j)
             qv1d(k)  = qv_p(i,k,j)
             qc1d(k)  = qc_p(i,k,j)
             qr1d(k)  = qr_p(i,k,j)
             qs1d(k)  = qs_p(i,k,j)
             qg1d(k)  = qg_p(i,k,j)
             nr1d(k)  = nr_p(i,k,j)
             dBZ1d(k) = -35._RKIND
             zp(k) = z_p(i,k,j) - z_p(i,1,j) + 0.5*dz_p(i,k,j) ! height AGL
          enddo

          call calc_refl10cm(qv1d,qc1d,qr1d,nr1d,qs1d,qg1d,t1d,p1d,dBZ1d,kts,kte,i,j)

          kp = 1
          do k = kts,kte
             dBZ1d(k) = max(-35._RKIND,dBZ1d(k))
             refl10cm(k,i) = dBZ1d(k)
             if(zp(k) .lt. 1000.) kp = k
          enddo
          refl10cm_max(i) = maxval(dBZ1d(:))
          w1 = (zp(kp+1)-1000.)/(zp(kp+1)-zp(kp))
          w2 = 1.0 - w1
          refl10cm_1km(i) = w1*dBZ1d(kp) + w2*dBZ1d(kp+1)
          refl10cm_1km_max(i) = max(refl10cm_1km_max(i),refl10cm_1km(i))
       enddo
       enddo

       if(allocated(p1d)  ) deallocate(p1d  )
       if(allocated(t1d)  ) deallocate(t1d  )
       if(allocated(qv1d) ) deallocate(qv1d )
       if(allocated(qc1d) ) deallocate(qc1d )
       if(allocated(qr1d) ) deallocate(qr1d )
       if(allocated(qs1d) ) deallocate(qs1d )
       if(allocated(qg1d) ) deallocate(qg1d )
       if(allocated(nr1d) ) deallocate(nr1d )
       if(allocated(dBz1d)) deallocate(dBZ1d)
       if(allocated(zp)   ) deallocate(zp   )

    case ("mp_p3")
       !equivalent reflectivity computed inside P3 (diag_ze, consistent with the P3 cloud, rain and ice size
       !distributions), saved in refl_p3_p in driver_microphysics. No -35 dBZ floor: P3 values are used as
       !they are (about -99 dBZ without hydrometeors).
       if(.not.allocated(dBz1d)) allocate(dBZ1d(kts:kte))
       if(.not.allocated(zp)   ) allocate(zp(kts:kte)   )

       do j = jts,jte
       do i = its,ite
          do k = kts,kte
             dBZ1d(k) = refl_p3_p(i,k,j)
             zp(k)    = z_p(i,k,j) - z_p(i,1,j) + 0.5_RKIND*dz_p(i,k,j) ! height AGL
          enddo

          kp = 1
          do k = kts,kte
             refl10cm(k,i) = dBZ1d(k)
             if(zp(k) .lt. 1000._RKIND) kp = k
          enddo
          refl10cm_max(i) = maxval(dBZ1d(:))
          kp = min(max(kp,kts),kte-1)
          w1 = (zp(kp+1)-1000._RKIND)/(zp(kp+1)-zp(kp))
          w2 = 1.0_RKIND - w1
          refl10cm_1km(i) = w1*dBZ1d(kp) + w2*dBZ1d(kp+1)
          refl10cm_1km_max(i) = max(refl10cm_1km_max(i),refl10cm_1km(i))
       enddo
       enddo

       if(allocated(dBz1d)) deallocate(dBZ1d)
       if(allocated(zp)   ) deallocate(zp   )

    case default
 end select microp_select

 end subroutine compute_radar_reflectivity

!=================================================================================================================
 subroutine compute_relhum(diag,its,ite)
!=================================================================================================================

!input arguments:
 integer,intent(in):: its,ite

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag

!local pointers:
 real(kind=RKIND),dimension(:,:),pointer:: relhum

!local variables and arrays:
 integer:: i,j,k

 real(kind=RKIND):: tempc, rh
 real(kind=RKIND),dimension(:),allocatable:: qv1d,qvs1d,t1d,p1d

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_array(diag,'relhum',relhum)
 do k = kts,kte
    do i = its,ite
       relhum(k,i) = 0._RKIND
    enddo
 enddo

 if(.not.allocated(p1d)  ) allocate(p1d(kts:kte)  )
 if(.not.allocated(t1d)  ) allocate(t1d(kts:kte)  )
 if(.not.allocated(qv1d) ) allocate(qv1d(kts:kte) )
 if(.not.allocated(qvs1d)) allocate(qvs1d(kts:kte))

 do j = jts,jte
 do i = its,ite
    do k = kts,kte
       p1d(k)  = pres_p(i,k,j)
       t1d(k)  = th_p(i,k,j) * pi_p(i,k,j)
       tempc   = t1d(k) - 273.16_RKIND
       qvs1d(k) = rslf(p1d(k),t1d(k))
       if(tempc .le. 0._RKIND) qvs1d(k) = rsif(p1d(k),t1d(k))
       qv1d(k) = qv_p(i,k,j)
       relhum(k,i) = qv1d(k) / qvs1d(k)
       relhum(k,i) = relhum(k,i) * 100._RKIND
    enddo
 enddo
 enddo

 if(allocated(p1d)  ) deallocate(p1d  )
 if(allocated(t1d)  ) deallocate(t1d  )
 if(allocated(qv1d) ) deallocate(qv1d )
 if(allocated(qvs1d)) deallocate(qvs1d)

 end subroutine compute_relhum

!=================================================================================================================
 end module mpas_atmphys_driver_microphysics
!=================================================================================================================
