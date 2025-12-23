! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module mpas_atmphys_driver_pbl
 use mpas_kind_types
 use mpas_pool_routines
 use mpas_timer,only: mpas_timer_start,mpas_timer_stop

 use mpas_atmphys_constants
 use mpas_atmphys_vars

 use bl_mynn,only: bl_mynn_init
 use module_bl_mynn,only: mynn_bl_driver
 use module_bl_ysu

 implicit none
 private
 public:: allocate_pbl,   &
          deallocate_pbl, &
          init_pbl,       &
          driver_pbl

!MPAS driver for parameterization of Planetary Boundary Layer (PBL) processes.
!Laura D. Fowler (send comments to laura@ucar.edu).
!2013-05-01.
!
! subroutines in mpas_atmphys_driver_pbl:
! ---------------------------------------
! allocate_pbl  : allocate local arrays for parameterization of PBL processes.
! deallocate_pbl: deallocate local arrays for parameterization of PBL processes.
! driver_pbl    : main driver (called from subroutine physics_driver).
! pbl_from_MPAS : initialize local arrays.
! pbl_to_MPAS   : copy local arrays to MPAS arrays.
!
! WRF physics called from driver_pbl:
! -----------------------------------
! * module_bl_ysu : YSU PBL scheme.
!
! add-ons and modifications to sourcecode:
! ----------------------------------------
! * removed the pre-processor option "do_hydrostatic_pressure" before call to subroutine ysu.
!   Laura D. Fowler (birch.ucar.edu) / 2013-05-29.
! * in call to subroutine ysu, replaced the variable g (that originally pointed to gravity)
!   with gravity, for simplicity.
!   Laura D. Fowler (laura@ucar.edu) / 2014-03-21.
! * throughout the sourcecode, replaced all "var_struct" defined arrays by local pointers.
!   Laura D. Fowler (laura@ucar.edu) / 2014-04-22.
! * modified sourcecode to use pools.
!   Laura D. Fowler (laura@ucar.edu) / 2014-05-15.
! * renamed "ysu" with "bl_ysu".
!   Laura D. Fowler (laura@ucar.edu) / 2016-03-25.
! * added the implementation of the MYNN PBL scheme from WRF 3.6.1.
!   Laura D. Fowler (laura@ucar.edu) / 2016-03-30.
! * corrected the initialization of sh3d for the mynn parameterization.
!   Laura D. Fowler (laura@ucar.edu) / 2016-04-13.
! * for the mynn parameterization, change the definition of dx_p to match that used in other physics
!   parameterizations.
!   Laura D. Fowler (laura@ucar.edu) / 2016-10-18.
! * updated the call to subroutine ysu in conjunction with updating module_bl_ysu.F from WRF version 3.6.1 to
!   WRF version 3.8.1
!   Laura D. Fowler (laura@ucar.edu) / 2016-10-27.
! * since we removed the local variable pbl_scheme from mpas_atmphys_vars.F, now defines pbl_scheme as a pointer
!   to config_pbl_scheme.
!   Laura D. Fowler (laura@ucar.edu) / 2917-02-16.
! * after updating module_bl_ysu.F to WRF version 4.0.3, corrected call to subroutine ysu to output diagnostics of
!   exchange coefficients exch_h and exch_m.
!   Laura D. Fowler (laura@ucar.edu) / 2019-03-12.
! * updated the call to subroutine ysu after updating the YSU PBL scheme to that in WRF 4.4.1. added the flags
!   errmsg and errflg in the call to subroutine ysu for compliance with the CCPP framework. also removed local
!   variable regime_p which is no longer needed in the call to subroutine ysu.
!   Laura D. Fowler (laura@ucar.edu) / 2023-05-15.
! * in the call to subroutine mynn_bl_driver,renamed f_qnc to f_nc, and f_qni to f_ni.
!   Laura D. Fowler (laura@ucar.edu) / 2024-02-14.
! * updated the MYNN PBL scheme to the sourcecode from WRF version 4.6.
!   Laura D. Fowler (laura@ucar.edu) / 2024-02.15.


 contains


!=================================================================================================================
 subroutine allocate_pbl(configs)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs

!local pointers:
 character(len=StrKIND),pointer:: pbl_scheme

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_pbl_scheme',pbl_scheme)

 if(.not.allocated(hfx_p)  ) allocate(hfx_p(ims:ime,jms:jme)  )
 if(.not.allocated(qfx_p)  ) allocate(qfx_p(ims:ime,jms:jme)  )
 if(.not.allocated(ust_p)  ) allocate(ust_p(ims:ime,jms:jme)  )
 if(.not.allocated(wspd_p) ) allocate(wspd_p(ims:ime,jms:jme) )
 if(.not.allocated(xland_p)) allocate(xland_p(ims:ime,jms:jme))
 if(.not.allocated(hpbl_p) ) allocate(hpbl_p(ims:ime,jms:jme) )
 if(.not.allocated(kpbl_p) ) allocate(kpbl_p(ims:ime,jms:jme) )
 if(.not.allocated(znt_p)  ) allocate(znt_p(ims:ime,jms:jme)  )
 if(.not.allocated(uoce_p) ) allocate(uoce_p(ims:ime,jms:jme) )
 if(.not.allocated(voce_p) ) allocate(voce_p(ims:ime,jms:jme) )

 !tendencies:
 if(.not.allocated(rublten_p) ) allocate(rublten_p(ims:ime,kms:kme,jms:jme) )
 if(.not.allocated(rvblten_p) ) allocate(rvblten_p(ims:ime,kms:kme,jms:jme) )
 if(.not.allocated(rthblten_p)) allocate(rthblten_p(ims:ime,kms:kme,jms:jme))
 if(.not.allocated(rqvblten_p)) allocate(rqvblten_p(ims:ime,kms:kme,jms:jme))
 if(.not.allocated(rqcblten_p)) allocate(rqcblten_p(ims:ime,kms:kme,jms:jme))
 if(.not.allocated(rqiblten_p)) allocate(rqiblten_p(ims:ime,kms:kme,jms:jme))

 if(.not.allocated(rthraten_p)) allocate(rthraten_p(ims:ime,kms:kme,jms:jme))

 !exchange coefficients:
 if(.not.allocated(kzh_p)) allocate(kzh_p(ims:ime,kms:kme,jms:jme))
 if(.not.allocated(kzm_p)) allocate(kzm_p(ims:ime,kms:kme,jms:jme))
 if(.not.allocated(kzq_p)) allocate(kzq_p(ims:ime,kms:kme,jms:jme))

 pbl_select: select case (trim(pbl_scheme))

    case("bl_ysu")
       !from surface-layer model:
       if(.not.allocated(br_p)    ) allocate(br_p(ims:ime,jms:jme)          )
       if(.not.allocated(ctopo_p) ) allocate(ctopo_p(ims:ime,jms:jme)       )
       if(.not.allocated(ctopo2_p)) allocate(ctopo2_p(ims:ime,jms:jme)      )
       if(.not.allocated(delta_p) ) allocate(delta_p(ims:ime,jms:jme)       )
       if(.not.allocated(psih_p)  ) allocate(psih_p(ims:ime,jms:jme)        )
       if(.not.allocated(psim_p)  ) allocate(psim_p(ims:ime,jms:jme)        )
       if(.not.allocated(u10_p)   ) allocate(u10_p(ims:ime,jms:jme)         )
       if(.not.allocated(v10_p)   ) allocate(v10_p(ims:ime,jms:jme)         )
       if(.not.allocated(exch_p)  ) allocate(exch_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(wstar_p) ) allocate(wstar_p(ims:ime,jms:jme)       )

    case("bl_mynn")
       if(.not.allocated(kbl_plume_p) ) allocate(kbl_plume_p(ims:ime,jms:jme)         )
       if(.not.allocated(dx_p)        ) allocate(dx_p(ims:ime,jms:jme)                )
       if(.not.allocated(ch_p)        ) allocate(ch_p(ims:ime,jms:jme)                )
       if(.not.allocated(qsfc_p)      ) allocate(qsfc_p(ims:ime,jms:jme)              )
       if(.not.allocated(rmol_p)      ) allocate(rmol_p(ims:ime,jms:jme)              )
       if(.not.allocated(tsk_p)       ) allocate(tsk_p(ims:ime,jms:jme)               )
       if(.not.allocated(maxwidthbl_p)) allocate(maxwidthbl_p(ims:ime,jms:jme)        )
       if(.not.allocated(maxmfbl_p)   ) allocate(maxmfbl_p(ims:ime,jms:jme)           )
       if(.not.allocated(zbl_plume_p) ) allocate(zbl_plume_p(ims:ime,jms:jme)         )
       if(.not.allocated(cov_p)       ) allocate(cov_p(ims:ime,kms:kme,jms:jme)       )
       if(.not.allocated(qke_p)       ) allocate(qke_p(ims:ime,kms:kme,jms:jme)       )
       if(.not.allocated(qsq_p)       ) allocate(qsq_p(ims:ime,kms:kme,jms:jme)       )
       if(.not.allocated(tsq_p)       ) allocate(tsq_p(ims:ime,kms:kme,jms:jme)       )
       if(.not.allocated(qkeadv_p)    ) allocate(qkeadv_p(ims:ime,kms:kme,jms:jme)    )
       if(.not.allocated(elpbl_p)     ) allocate(elpbl_p(ims:ime,kms:kme,jms:jme)     )
       if(.not.allocated(tkepbl_p)    ) allocate(tkepbl_p(ims:ime,kms:kme,jms:jme)    )
       if(.not.allocated(sh3d_p)      ) allocate(sh3d_p(ims:ime,kms:kme,jms:jme)      )
       if(.not.allocated(sm3d_p)      ) allocate(sm3d_p(ims:ime,kms:kme,jms:jme)      )
       if(.not.allocated(dqke_p)      ) allocate(dqke_p(ims:ime,kms:kme,jms:jme)      )
       if(.not.allocated(qbuoy_p)     ) allocate(qbuoy_p(ims:ime,kms:kme,jms:jme)     )
       if(.not.allocated(qdiss_p)     ) allocate(qdiss_p(ims:ime,kms:kme,jms:jme)     )
       if(.not.allocated(qshear_p)    ) allocate(qshear_p(ims:ime,kms:kme,jms:jme)    )
       if(.not.allocated(qwt_p)       ) allocate(qwt_p(ims:ime,kms:kme,jms:jme)       )
       if(.not.allocated(qcbl_p)      ) allocate(qcbl_p(ims:ime,kms:kme,jms:jme)      )
       if(.not.allocated(qibl_p)      ) allocate(qibl_p(ims:ime,kms:kme,jms:jme)      )
       if(.not.allocated(cldfrabl_p)  ) allocate(cldfrabl_p(ims:ime,kms:kme,jms:jme)  )
       if(.not.allocated(edmfa_p)     ) allocate(edmfa_p(ims:ime,kms:kme,jms:jme)     )
       if(.not.allocated(edmfw_p)     ) allocate(edmfw_p(ims:ime,kms:kme,jms:jme)     )
       if(.not.allocated(edmfqt_p)    ) allocate(edmfqt_p(ims:ime,kms:kme,jms:jme)    )
       if(.not.allocated(edmfthl_p)   ) allocate(edmfthl_p(ims:ime,kms:kme,jms:jme)   )
       if(.not.allocated(edmfent_p)   ) allocate(edmfent_p(ims:ime,kms:kme,jms:jme)   )
       if(.not.allocated(edmfqc_p)    ) allocate(edmfqc_p(ims:ime,kms:kme,jms:jme)    )
       if(.not.allocated(subthl_p)    ) allocate(subthl_p(ims:ime,kms:kme,jms:jme)    )
       if(.not.allocated(subqv_p)     ) allocate(subqv_p(ims:ime,kms:kme,jms:jme)     )
       if(.not.allocated(detthl_p)    ) allocate(detthl_p(ims:ime,kms:kme,jms:jme)    )
       if(.not.allocated(detqv_p)     ) allocate(detqv_p(ims:ime,kms:kme,jms:jme)     )

       !additional tendencies:
       if(.not.allocated(rqsblten_p)  ) allocate(rqsblten_p(ims:ime,kms:kme,jms:jme)  )
       if(.not.allocated(rncblten_p)  ) allocate(rncblten_p(ims:ime,kms:kme,jms:jme)  )
       if(.not.allocated(rniblten_p)  ) allocate(rniblten_p(ims:ime,kms:kme,jms:jme)  )
       if(.not.allocated(rnifablten_p)) allocate(rnifablten_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(rnwfablten_p)) allocate(rnwfablten_p(ims:ime,kms:kme,jms:jme))

       !allocation of additional arrays:
       if(.not.allocated(pattern_spp_pbl)) allocate(pattern_spp_pbl(ims:ime,kms:kme,jms:jme))

    case default

 end select pbl_select

 end subroutine allocate_pbl

!=================================================================================================================
 subroutine deallocate_pbl(configs)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs

!local pointers:
 character(len=StrKIND),pointer:: pbl_scheme

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_pbl_scheme',pbl_scheme)

 if(allocated(hfx_p)  ) deallocate(hfx_p  )
 if(allocated(qfx_p)  ) deallocate(qfx_p  )
 if(allocated(ust_p)  ) deallocate(ust_p  )
 if(allocated(wspd_p) ) deallocate(wspd_p )
 if(allocated(xland_p)) deallocate(xland_p)
 if(allocated(hpbl_p) ) deallocate(hpbl_p )
 if(allocated(kpbl_p) ) deallocate(kpbl_p )
 if(allocated(znt_p)  ) deallocate(znt_p  )
 if(allocated(uoce_p) ) deallocate(uoce_p )
 if(allocated(voce_p) ) deallocate(voce_p )

 !tendencies:
 if(allocated(rublten_p) ) deallocate(rublten_p )
 if(allocated(rvblten_p) ) deallocate(rvblten_p )
 if(allocated(rthblten_p)) deallocate(rthblten_p)
 if(allocated(rqvblten_p)) deallocate(rqvblten_p)
 if(allocated(rqcblten_p)) deallocate(rqcblten_p)
 if(allocated(rqiblten_p)) deallocate(rqiblten_p)

 if(allocated(rthraten_p)) deallocate(rthraten_p)

 !exchange coefficients:
 if(allocated(kzh_p)) deallocate(kzh_p)
 if(allocated(kzm_p)) deallocate(kzm_p)
 if(allocated(kzq_p)) deallocate(kzq_p)

 pbl_select: select case (trim(pbl_scheme))

    case("bl_ysu")
       !from surface-layer model:
       if(allocated(br_p)    ) deallocate(br_p    )
       if(allocated(ctopo_p) ) deallocate(ctopo_p )
       if(allocated(ctopo2_p)) deallocate(ctopo2_p)
       if(allocated(delta_p) ) deallocate(delta_p )
       if(allocated(psih_p)  ) deallocate(psih_p  )
       if(allocated(psim_p)  ) deallocate(psim_p  )
       if(allocated(u10_p)   ) deallocate(u10_p   )
       if(allocated(v10_p)   ) deallocate(v10_p   )
       if(allocated(exch_p)  ) deallocate(exch_p  )
       if(allocated(wstar_p) ) deallocate(wstar_p )

    case("bl_mynn")
       if(allocated(kbl_plume_p) ) deallocate(kbl_plume_p )
       if(allocated(dx_p)        ) deallocate(dx_p        )
       if(allocated(ch_p)        ) deallocate(ch_p        )
       if(allocated(qsfc_p)      ) deallocate(qsfc_p      )
       if(allocated(rmol_p)      ) deallocate(rmol_p      )
       if(allocated(tsk_p)       ) deallocate(tsk_p       )
       if(allocated(maxwidthbl_p)) deallocate(maxwidthbl_p)
       if(allocated(maxmfbl_p)   ) deallocate(maxmfbl_p   )
       if(allocated(zbl_plume_p) ) deallocate(zbl_plume_p )

       if(allocated(cov_p)       ) deallocate(cov_p       )
       if(allocated(qke_p)       ) deallocate(qke_p       )
       if(allocated(qsq_p)       ) deallocate(qsq_p       )
       if(allocated(tsq_p)       ) deallocate(tsq_p       )
       if(allocated(qkeadv_p)    ) deallocate(qkeadv_p    )
       if(allocated(elpbl_p)     ) deallocate(elpbl_p     )
       if(allocated(tkepbl_p)    ) deallocate(tkepbl_p    )
       if(allocated(sh3d_p)      ) deallocate(sh3d_p      )
       if(allocated(sm3d_p)      ) deallocate(sm3d_p      )
       if(allocated(dqke_p)      ) deallocate(dqke_p      )
       if(allocated(qbuoy_p)     ) deallocate(qbuoy_p     )
       if(allocated(qdiss_p)     ) deallocate(qdiss_p     )
       if(allocated(qshear_p)    ) deallocate(qshear_p    )
       if(allocated(qwt_p)       ) deallocate(qwt_p       )
       if(allocated(qcbl_p)      ) deallocate(qcbl_p      )
       if(allocated(qibl_p)      ) deallocate(qibl_p      )
       if(allocated(cldfrabl_p)  ) deallocate(cldfrabl_p  )
       if(allocated(edmfa_p)     ) deallocate(edmfa_p     )
       if(allocated(edmfw_p)     ) deallocate(edmfw_p     )
       if(allocated(edmfqt_p)    ) deallocate(edmfqt_p    )
       if(allocated(edmfthl_p)   ) deallocate(edmfthl_p   )
       if(allocated(edmfent_p)   ) deallocate(edmfent_p   )
       if(allocated(edmfqc_p)    ) deallocate(edmfqc_p    )
       if(allocated(subthl_p)    ) deallocate(subthl_p    )
       if(allocated(subqv_p)     ) deallocate(subqv_p     )
       if(allocated(detthl_p)    ) deallocate(detthl_p    )
       if(allocated(detqv_p)     ) deallocate(detqv_p     )

       !additional tendencies:
       if(allocated(rqsblten_p)  ) deallocate(rqsblten_p  )
       if(allocated(rncblten_p)  ) deallocate(rncblten_p  )
       if(allocated(rniblten_p)  ) deallocate(rniblten_p  )
       if(allocated(rnifablten_p)) deallocate(rnifablten_p)
       if(allocated(rnwfablten_p)) deallocate(rnwfablten_p)

       !deallocation of additional arrays:
       if(allocated(pattern_spp_pbl)) deallocate(pattern_spp_pbl)

    case default

 end select pbl_select

 end subroutine deallocate_pbl

!=================================================================================================================
 subroutine pbl_from_MPAS(configs,mesh,sfc_input,diag_physics,tend_physics,its,ite)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: diag_physics
 type(mpas_pool_type),intent(in):: sfc_input
 type(mpas_pool_type),intent(in):: tend_physics

 integer,intent(in):: its,ite

!local variables:
 integer:: i,k,j

!local pointers:
 character(len=StrKIND),pointer:: pbl_scheme

 real(kind=RKIND),dimension(:),pointer:: hfx,hpbl,qfx,ust,wspd,xland,znt
 real(kind=RKIND),dimension(:),pointer:: delta,wstar

!local pointers for YSU scheme:
 logical,pointer:: config_ysu_pblmix
 real(kind=RKIND),dimension(:),pointer:: br,fh,fm,u10,v10
 real(kind=RKIND),dimension(:,:),pointer:: rthratenlw,rthratensw

!local pointers for MYNN scheme:
 real(kind=RKIND),pointer:: len_disp
 real(kind=RKIND),dimension(:),pointer  :: meshDensity
 real(kind=RKIND),dimension(:),pointer  :: ch,qsfc,rmol,skintemp
 real(kind=RKIND),dimension(:,:),pointer:: cov,qke,qsq,tsq,sh3d,sm3d,tke_pbl,qke_adv,el_pbl
 real(kind=RKIND),dimension(:,:),pointer:: cldfrac_bl,qc_bl,qi_bl
 real(kind=RKIND),dimension(:,:),pointer:: edmf_a,edmf_ent,edmf_qc,edmf_qt,edmf_thl,edmf_w
 real(kind=RKIND),dimension(:,:),pointer:: sub_thl,sub_qv,det_thl,det_qv

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_pbl_scheme',pbl_scheme)

 call mpas_pool_get_array(diag_physics,'hfx' ,hfx )
 call mpas_pool_get_array(diag_physics,'hpbl',hpbl)
 call mpas_pool_get_array(diag_physics,'qfx' ,qfx )
 call mpas_pool_get_array(diag_physics,'ust' ,ust )
 call mpas_pool_get_array(diag_physics,'wspd',wspd)
 call mpas_pool_get_array(diag_physics,'znt' ,znt )

 call mpas_pool_get_array(tend_physics,'rthratenlw',rthratenlw)
 call mpas_pool_get_array(tend_physics,'rthratensw',rthratensw)

 call mpas_pool_get_array(sfc_input,'xland',xland)

 do j = jts,jte
 do i = its,ite
    !from surface-layer model:
    hfx_p(i,j)   = hfx(i)
    hpbl_p(i,j)  = hpbl(i)
    qfx_p(i,j)   = qfx(i)
    ust_p(i,j)   = ust(i)
    wspd_p(i,j)  = wspd(i)
    xland_p(i,j) = xland(i)
    kpbl_p(i,j)  = 1
    znt_p(i,j)   = znt(i)
    !... ocean currents are set to zero:
    uoce_p(i,j)  = 0._RKIND
    voce_p(i,j)  = 0._RKIND
 enddo
 do k = kts,kte
 do i = its,ite
    rthraten_p(i,k,j) = rthratenlw(k,i) + rthratensw(k,i)
 enddo
 enddo
 enddo

 pbl_select: select case (trim(pbl_scheme))

    case("bl_ysu")
       call mpas_pool_get_config(configs,'config_ysu_pblmix',config_ysu_pblmix)

       call mpas_pool_get_array(diag_physics,'br'   ,br   )
       call mpas_pool_get_array(diag_physics,'delta',delta)
       call mpas_pool_get_array(diag_physics,'fm'   ,fm   )
       call mpas_pool_get_array(diag_physics,'fh'   ,fh   )
       call mpas_pool_get_array(diag_physics,'u10'  ,u10  )
       call mpas_pool_get_array(diag_physics,'v10'  ,v10  )
       call mpas_pool_get_array(diag_physics,'wstar',wstar)

       ysu_pblmix = 0
       if(config_ysu_pblmix) ysu_pblmix = 1

       do j = jts,jte
       do i = its,ite
          !from surface-layer model:
          br_p(i,j)     = br(i)
          psim_p(i,j)   = fm(i)
          psih_p(i,j)   = fh(i)
          u10_p(i,j)    = u10(i)
          v10_p(i,j)    = v10(i)
          delta_p(i,j)  = delta(i)
          wstar_p(i,j)  = wstar(i)
          !initialization for YSU PBL scheme:
          ctopo_p(i,j)  = 1._RKIND
          ctopo2_p(i,j) = 1._RKIND
       enddo
       enddo

       do j = jts,jte
       do k = kts,kte
       do i = its,ite
          exch_p(i,k,j) = 0._RKIND
       enddo
       enddo
       enddo

    case("bl_mynn")
       call mpas_pool_get_config(configs,'config_len_disp',len_disp)
       call mpas_pool_get_array(mesh,'meshDensity',meshDensity)

       call mpas_pool_get_array(sfc_input,'skintemp',skintemp)
       call mpas_pool_get_array(diag_physics,'ch'        ,ch        )
       call mpas_pool_get_array(diag_physics,'qsfc'      ,qsfc      )
       call mpas_pool_get_array(diag_physics,'rmol'      ,rmol      )
       call mpas_pool_get_array(diag_physics,'el_pbl'    ,el_pbl    )
       call mpas_pool_get_array(diag_physics,'cov'       ,cov       )
       call mpas_pool_get_array(diag_physics,'qke'       ,qke       )
       call mpas_pool_get_array(diag_physics,'qke_adv'   ,qke_adv   )
       call mpas_pool_get_array(diag_physics,'qsq'       ,qsq       )
       call mpas_pool_get_array(diag_physics,'tsq'       ,tsq       )
       call mpas_pool_get_array(diag_physics,'tke_pbl'   ,tke_pbl   )
       call mpas_pool_get_array(diag_physics,'sh3d'      ,sh3d      )
       call mpas_pool_get_array(diag_physics,'sm3d'      ,sm3d      )
       call mpas_pool_get_array(diag_physics,'cldfrac_bl',cldfrac_bl)
       call mpas_pool_get_array(diag_physics,'qc_bl'     ,qc_bl     )
       call mpas_pool_get_array(diag_physics,'qi_bl'     ,qi_bl     )
       call mpas_pool_get_array(diag_physics,'edmf_a'    ,edmf_a    )
       call mpas_pool_get_array(diag_physics,'edmf_ent'  ,edmf_ent  )
       call mpas_pool_get_array(diag_physics,'edmf_qc'   ,edmf_qc   )
       call mpas_pool_get_array(diag_physics,'edmf_qt'   ,edmf_qt   )
       call mpas_pool_get_array(diag_physics,'edmf_thl'  ,edmf_thl  )
       call mpas_pool_get_array(diag_physics,'edmf_w'    ,edmf_w    )
       call mpas_pool_get_array(diag_physics,'sub_thl'   ,sub_thl   )
       call mpas_pool_get_array(diag_physics,'sub_qv'    ,sub_qv    )
       call mpas_pool_get_array(diag_physics,'det_thl'   ,det_thl   )
       call mpas_pool_get_array(diag_physics,'det_qv'    ,det_qv    )

       do j = jts,jte
       do i = its,ite
          dx_p(i,j)   = len_disp / meshDensity(i)**0.25
          ch_p(i,j)   = ch(i)
          qsfc_p(i,j) = qsfc(i)
          rmol_p(i,j) = rmol(i)
          tsk_p(i,j)  = skintemp(i)
       enddo
       enddo

       do j = jts,jte
       do k = kts,kte
       do i = its,ite
          elpbl_p(i,k,j)    = el_pbl(k,i)
          cov_p(i,k,j)      = cov(k,i)
          qke_p(i,k,j)      = qke(k,i)
          qsq_p(i,k,j)      = qsq(k,i)
          tsq_p(i,k,j)      = tsq(k,i)
          tkepbl_p(i,k,j)   = tke_pbl(k,i)
          qkeadv_p(i,k,j)   = qke_adv(k,i)
          sh3d_p(i,k,j)     = sh3d(k,i)
          sm3d_p(i,k,j)     = sm3d(k,i)
          cldfrabl_p(i,k,j) = cldfrac_bl(k,i)
          qcbl_p(i,k,j)     = qc_bl(k,i)
          qibl_p(i,k,j)     = qi_bl(k,i)
          edmfa_p(i,k,j)    = edmf_a(k,i)
          edmfent_p(i,k,j)  = edmf_ent(k,i)
          edmfqc_p(i,k,j)   = edmf_qc(k,i)
          edmfqt_p(i,k,j)   = edmf_qt(k,i)
          edmfthl_p(i,k,j)  = edmf_thl(k,i)
          edmfw_p(i,k,j)    = edmf_w(k,i)
          subthl_p(i,k,j)   = sub_thl(k,i)
          subqv_p(i,k,j)    = sub_qv(k,i)
          detthl_p(i,k,j)   = det_thl(k,i)
          detqv_p(i,k,j)    = det_qv(k,i)
          dqke_p(i,k,j)     = 0._RKIND
          qbuoy_p(i,k,j)    = 0._RKIND
          qdiss_p(i,k,j)    = 0._RKIND
          qshear_p(i,k,j)   = 0._RKIND
          qwt_p(i,k,j)      = 0._RKIND

          rqsblten_p(i,k,j)   = 0._RKIND
          rncblten_p(i,k,j)   = 0._RKIND
          rniblten_p(i,k,j)   = 0._RKIND
          rnifablten_p(i,k,j) = 0._RKIND
          rnwfablten_p(i,k,j) = 0._RKIND

          pattern_spp_pbl(i,k,j) = 0._RKIND
       enddo
       enddo
       do i = its,ite
          kbl_plume_p(i,j)  = 0
          maxwidthbl_p(i,j) = 0._RKIND
          maxmfbl_p(i,j)    = 0._RKIND
          zbl_plume_p(i,j)  = 0
       enddo
       enddo

    case default

 end select pbl_select

 do j = jts,jte
 do k = kts,kte
 do i = its,ite
    rublten_p(i,k,j)  = 0._RKIND
    rvblten_p(i,k,j)  = 0._RKIND
    rthblten_p(i,k,j) = 0._RKIND
    rqvblten_p(i,k,j) = 0._RKIND
    rqcblten_p(i,k,j) = 0._RKIND
    rqiblten_p(i,k,j) = 0._RKIND

    kzh_p(i,k,j) = 0._RKIND
    kzm_p(i,k,j) = 0._RKIND
    kzq_p(i,k,j) = 0._RKIND
 enddo
 enddo
 enddo

 end subroutine pbl_from_MPAS

!=================================================================================================================
 subroutine pbl_to_MPAS(configs,diag_physics,tend_physics,its,ite)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: tend_physics

 integer,intent(in):: its,ite

!local variables:
 integer:: i,k,j

!local pointers:
 character(len=StrKIND),pointer:: pbl_scheme

 integer,dimension(:),pointer:: kpbl

 real(kind=RKIND),dimension(:),pointer  :: hpbl
 real(kind=RKIND),dimension(:,:),pointer:: kzh,kzm,kzq
 real(kind=RKIND),dimension(:,:),pointer:: rublten,rvblten,rthblten,rqvblten,rqcblten,rqiblten,rqsblten
 real(kind=RKIND),dimension(:,:),pointer:: rncblten,rniblten,rnifablten,rnwfablten

!local pointers for YSU scheme:
 real(kind=RKIND),dimension(:,:),pointer:: exch_h

!local pointers for MYNN scheme:
 real(kind=RKIND),dimension(:),pointer  :: delta,wstar
 real(kind=RKIND),dimension(:,:),pointer:: cov,qke,qsq,tsq,sh3d,sm3d,tke_pbl,qke_adv,el_pbl,dqke,qbuoy, &
                                           qdiss,qshear,qwt
 real(kind=RKIND),dimension(:,:),pointer:: cldfrac_bl,qc_bl,qi_bl
 real(kind=RKIND),dimension(:,:),pointer:: edmf_a,edmf_ent,edmf_qc,edmf_qt,edmf_thl,edmf_w
 real(kind=RKIND),dimension(:,:),pointer:: sub_thl,sub_qv,det_thl,det_qv

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_pbl_scheme',pbl_scheme)

 call mpas_pool_get_array(diag_physics,'kpbl' ,kpbl )
 call mpas_pool_get_array(diag_physics,'hpbl' ,hpbl )
 call mpas_pool_get_array(diag_physics,'kzh'  ,kzh  )
 call mpas_pool_get_array(diag_physics,'kzm'  ,kzm  )
 call mpas_pool_get_array(diag_physics,'kzq'  ,kzq  )

 call mpas_pool_get_array(tend_physics,'rublten' ,rublten )
 call mpas_pool_get_array(tend_physics,'rvblten' ,rvblten )
 call mpas_pool_get_array(tend_physics,'rthblten',rthblten)
 call mpas_pool_get_array(tend_physics,'rqvblten',rqvblten)
 call mpas_pool_get_array(tend_physics,'rqcblten',rqcblten)
 call mpas_pool_get_array(tend_physics,'rqiblten',rqiblten)

 do j = jts,jte
 do i = its,ite
    hpbl(i) = hpbl_p(i,j)
    kpbl(i) = kpbl_p(i,j)
 enddo
 enddo

 do j = jts,jte
 do k = kts,kte
 do i = its,ite
    rublten(k,i)  = rublten_p(i,k,j)
    rvblten(k,i)  = rvblten_p(i,k,j)
    rthblten(k,i) = rthblten_p(i,k,j)
    rqvblten(k,i) = rqvblten_p(i,k,j)
    rqcblten(k,i) = rqcblten_p(i,k,j)
    rqiblten(k,i) = rqiblten_p(i,k,j)

    kzh(k,i) = kzh_p(i,k,j)
    kzm(k,i) = kzm_p(i,k,j)
    kzq(k,i) = kzh_p(i,k,j)
 enddo
 enddo
 enddo

 pbl_select: select case (trim(pbl_scheme))

    case("bl_ysu")
       call mpas_pool_get_array(diag_physics,'delta',delta  )
       call mpas_pool_get_array(diag_physics,'wstar' ,wstar )
       call mpas_pool_get_array(diag_physics,'exch_h',exch_h)

       do j = jts,jte
       do i = its,ite
          delta(i) = delta_p(i,j)
          wstar(i) = wstar_p(i,j)
       enddo
       do k = kts,kte
       do i = its,ite
          exch_h(k,i) = exch_p(i,k,j)
       enddo
       enddo
       enddo

    case("bl_mynn")
       call mpas_pool_get_array(diag_physics,'el_pbl'    ,el_pbl    )
       call mpas_pool_get_array(diag_physics,'cov'       ,cov       )
       call mpas_pool_get_array(diag_physics,'qke'       ,qke       )
       call mpas_pool_get_array(diag_physics,'qke_adv'   ,qke_adv   )
       call mpas_pool_get_array(diag_physics,'qsq'       ,qsq       )
       call mpas_pool_get_array(diag_physics,'tsq'       ,tsq       )
       call mpas_pool_get_array(diag_physics,'tke_pbl'   ,tke_pbl   )
       call mpas_pool_get_array(diag_physics,'sh3d'      ,sh3d      )
       call mpas_pool_get_array(diag_physics,'sm3d'      ,sm3d      )
       call mpas_pool_get_array(diag_physics,'dqke'      ,dqke      )
       call mpas_pool_get_array(diag_physics,'qbuoy'     ,qbuoy     )
       call mpas_pool_get_array(diag_physics,'qdiss'     ,qdiss     )
       call mpas_pool_get_array(diag_physics,'qshear'    ,qshear    )
       call mpas_pool_get_array(diag_physics,'qwt'       ,qwt       )
       call mpas_pool_get_array(diag_physics,'cldfrac_bl',cldfrac_bl)
       call mpas_pool_get_array(diag_physics,'qc_bl'     ,qc_bl     )
       call mpas_pool_get_array(diag_physics,'qi_bl'     ,qi_bl     )
       call mpas_pool_get_array(diag_physics,'edmf_a'    ,edmf_a    )
       call mpas_pool_get_array(diag_physics,'edmf_ent'  ,edmf_ent  )
       call mpas_pool_get_array(diag_physics,'edmf_qc'   ,edmf_qc   )
       call mpas_pool_get_array(diag_physics,'edmf_qt'   ,edmf_qt   )
       call mpas_pool_get_array(diag_physics,'edmf_thl'  ,edmf_thl  )
       call mpas_pool_get_array(diag_physics,'edmf_w'    ,edmf_w    )
       call mpas_pool_get_array(diag_physics,'sub_thl'   ,sub_thl   )
       call mpas_pool_get_array(diag_physics,'sub_qv'    ,sub_qv    )
       call mpas_pool_get_array(diag_physics,'det_thl'   ,det_thl   )
       call mpas_pool_get_array(diag_physics,'det_qv'    ,det_qv    )

       call mpas_pool_get_array(tend_physics,'rqsblten'  ,rqsblten  )

       do j = jts,jte
       do k = kts,kte
       do i = its,ite
          el_pbl(k,i)     = elpbl_p(i,k,j)
          cov(k,i)        = cov_p(i,k,j)
          qke(k,i)        = qke_p(i,k,j)
          qsq(k,i)        = qsq_p(i,k,j)
          tsq(k,i)        = tsq_p(i,k,j)
          sh3d(k,i)       = sh3d_p(i,k,j)
          sm3d(k,i)       = sm3d_p(i,k,j)
          tke_pbl(k,i)    = tkepbl_p(i,k,j)
          qke_adv(k,i)    = qkeadv_p(i,k,j)
          cldfrac_bl(k,i) = cldfrabl_p(i,k,j)
          qc_bl(k,i)      = qcbl_p(i,k,j)
          qi_bl(k,i)      = qibl_p(i,k,j)
          edmf_a(k,i)     = edmfa_p(i,k,j)
          edmf_ent(k,i)   = edmfent_p(i,k,j)
          edmf_qc(k,i)    = edmfqc_p(i,k,j)
          edmf_qt(k,i)    = edmfqt_p(i,k,j)
          edmf_thl(k,i)   = edmfthl_p(i,k,j)
          edmf_w(k,i)     = edmfw_p(i,k,j)
          sub_thl(k,i)    = subthl_p(i,k,j)
          sub_qv(k,i)     = subqv_p(i,k,j)
          det_thl(k,i)    = detthl_p(i,k,j)
          det_qv(k,i)     = detqv_p(i,k,j)
          dqke(k,i)       = dqke_p(i,k,j)
          qbuoy(k,i)      = qbuoy_p(i,k,j)
          qdiss(k,i)      = qdiss_p(i,k,j)
          qshear(k,i)     = qshear_p(i,k,j)
          qwt(k,i)        = qwt_p(i,k,j)

          rqsblten(k,i)   = rqsblten_p(i,k,j)
       enddo
       enddo
       enddo

       if(f_ni) then
          call mpas_pool_get_array(tend_physics,'rniblten',rniblten)
          do j = jts,jte
             do k = kts,kte
                do i = its,ite
                   rniblten(k,i) = rniblten_p(i,k,j)
                enddo
             enddo
          enddo
       endif
       if(f_nc .and. f_nifa .and. f_nwfa) then
          call mpas_pool_get_array(tend_physics,'rncblten'  ,rncblten  )
          call mpas_pool_get_array(tend_physics,'rnifablten',rnifablten)
          call mpas_pool_get_array(tend_physics,'rnwfablten',rnwfablten)
          do j = jts,jte
             do k = kts,kte
                do i = its,ite
                   rncblten(k,i)   = rncblten_p(i,k,j)
                   rnifablten(k,i) = rnifablten_p(i,k,j)
                   rnwfablten(k,i) = rnwfablten_p(i,k,j)
                enddo
             enddo
          enddo
       endif

    case default

 end select pbl_select

 end subroutine pbl_to_MPAS
 
!=================================================================================================================
 subroutine init_pbl(configs)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs

!local variables and pointers:
 character(len=StrKIND),pointer:: pbl_scheme
 character(len=StrKIND):: errmsg
 integer:: errflg

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_pbl_scheme',pbl_scheme)

 pbl_select: select case (trim(pbl_scheme))

    case("bl_mynn")
!      call mpas_log_write('--- enter subroutine bl_mynn_init:')
       call bl_mynn_init(cp,cpv,cice,cliq,ep_1,ep_2,gravity,karman,P0,R_d,R_v,svp1,svp2,svp3,svpt0, &
                         xlf,xls,xlv,errmsg,errflg)
!      call mpas_log_write('--- end subroutine bl_mynn_init:')

    case default

 end select pbl_select

 end subroutine init_pbl

!=================================================================================================================
 subroutine driver_pbl(itimestep,configs,mesh,sfc_input,diag_physics,tend_physics,its,ite)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: mesh

 integer,intent(in):: its,ite
 integer,intent(in):: itimestep

!inout arguments:
 type(mpas_pool_type),intent(inout):: sfc_input
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: tend_physics

!local pointers:
 logical,pointer:: config_do_DAcycling, &
                   config_do_restart,   &
                   bl_mynn_tkeadvect

 character(len=StrKIND),pointer:: pbl_scheme

 integer,pointer:: bl_mynn_cloudpdf,    &
                   bl_mynn_mixlength,   &
                   bl_mynn_stfunc,      &
                   bl_mynn_topdown,     &
                   bl_mynn_scaleaware,  &
                   bl_mynn_dheat_opt,   &
                   bl_mynn_edmf,        &
                   bl_mynn_edmf_dd,     &
                   bl_mynn_edmf_mom,    &
                   bl_mynn_edmf_tke,    &
                   bl_mynn_edmf_output, &
                   bl_mynn_mixscalars,  &
                   bl_mynn_cloudmix,    &
                   bl_mynn_mixqt,       &
                   bl_mynn_tkebudget

 real(kind=RKIND),pointer:: bl_mynn_closure

!local variables:
 integer:: initflag
 integer:: i,k,j

!CCPP-compliant flags:
 character(len=StrKIND):: errmsg
 integer:: errflg

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write('')
!call mpas_log_write('--- enter subroutine driver_pbl:')

!initialization of CCPP-compliant flags:
 errmsg = ' '
 errflg = 0

 call mpas_pool_get_config(configs,'config_do_DAcycling',config_do_DAcycling)
 call mpas_pool_get_config(configs,'config_do_restart'  ,config_do_restart  )
 call mpas_pool_get_config(configs,'config_pbl_scheme'  ,pbl_scheme         )

!copy MPAS arrays to local arrays:
 call pbl_from_MPAS(configs,mesh,sfc_input,diag_physics,tend_physics,its,ite)

 initflag = 1
 if(config_do_restart .or. itimestep > 1) initflag = 0

 pbl_select: select case (trim(pbl_scheme))

    case("bl_ysu")
       call mpas_timer_start('bl_ysu')
       call ysu ( &
                 p3d      = pres_hyd_p , p3di     = pres2_hyd_p , psfc     = psfc_p     , &
                 t3d      = t_p        , dz8w     = dz_p        , pi3d     = pi_p       , &
                 u3d      = u_p        , v3d      = v_p         , qv3d     = qv_p       , &
                 qc3d     = qc_p       , qi3d     = qi_p        , rublten  = rublten_p  , &
                 rvblten  = rvblten_p  , rthblten = rthblten_p  , rqvblten = rqvblten_p , &
                 rqcblten = rqcblten_p , rqiblten = rqiblten_p  , flag_qc  = f_qc       , &
                 flag_qi  = f_qi       , cp       = cp          , g        = gravity    , &
                 rovcp    = rcp        , rd       = R_d         , rovg     = rdg        , & 
                 ep1      = ep_1       , ep2      = ep_2        , karman   = karman     , &
                 xlv      = xlv        , rv       = R_v         , znt      = znt_p      , &
                 ust      = ust_p      , hpbl     = hpbl_p      , psim     = psim_p     , &
                 psih     = psih_p     , xland    = xland_p     , hfx      = hfx_p      , &
                 qfx      = qfx_p      , wspd     = wspd_p      , br       = br_p       , &
                 dt       = dt_pbl     , kpbl2d   = kpbl_p      , exch_h   = kzh_p      , &
                 exch_m   = kzm_p      , wstar    = wstar_p     , delta    = delta_p    , &
                 uoce     = uoce_p     , voce     = voce_p      , rthraten = rthraten_p , &
                 u10      = u10_p      , v10      = v10_p       , ctopo    = ctopo_p    , &
                 ctopo2   = ctopo2_p   , flag_bep = flag_bep    , idiff    = idiff      , &
                 ysu_topdown_pblmix = ysu_pblmix ,                                        &
                 errmsg   = errmsg     , errflg   = errflg      ,                         &
                 ids = ids , ide = ide , jds = jds , jde = jde , kds = kds , kde = kde  , &
                 ims = ims , ime = ime , jms = jms , jme = jme , kms = kms , kme = kme  , &
                 its = its , ite = ite , jts = jts , jte = jte , kts = kts , kte = kte    &
                )
       call mpas_timer_stop('bl_ysu')

    case("bl_mynn")
       call mpas_pool_get_config(configs,'config_mynn_cloudpdf'   ,bl_mynn_cloudpdf   )
       call mpas_pool_get_config(configs,'config_mynn_mixlength'  ,bl_mynn_mixlength  )
       call mpas_pool_get_config(configs,'config_mynn_stfunc'     ,bl_mynn_stfunc     )
       call mpas_pool_get_config(configs,'config_mynn_topdown'    ,bl_mynn_topdown    )
       call mpas_pool_get_config(configs,'config_mynn_scaleaware' ,bl_mynn_scaleaware )
       call mpas_pool_get_config(configs,'config_mynn_dheat_opt'  ,bl_mynn_dheat_opt  )
       call mpas_pool_get_config(configs,'config_mynn_edmf'       ,bl_mynn_edmf       )
       call mpas_pool_get_config(configs,'config_mynn_edmf_dd'    ,bl_mynn_edmf_dd    )
       call mpas_pool_get_config(configs,'config_mynn_edmf_mom'   ,bl_mynn_edmf_mom   )
       call mpas_pool_get_config(configs,'config_mynn_edmf_tke'   ,bl_mynn_edmf_tke   )
       call mpas_pool_get_config(configs,'config_mynn_edmf_output',bl_mynn_edmf_output)
       call mpas_pool_get_config(configs,'config_mynn_closure'    ,bl_mynn_closure    )
       call mpas_pool_get_config(configs,'config_mynn_mixscalars' ,bl_mynn_mixscalars )
       call mpas_pool_get_config(configs,'config_mynn_mixclouds'  ,bl_mynn_cloudmix   )
       call mpas_pool_get_config(configs,'config_mynn_mixqt'      ,bl_mynn_mixqt      )
       call mpas_pool_get_config(configs,'config_mynn_tkeadvect'  ,bl_mynn_tkeadvect  )
       call mpas_pool_get_config(configs,'config_mynn_tkebudget'  ,bl_mynn_tkebudget  )

!      call mpas_log_write(' ')
!      call mpas_log_write('--- enter subroutine mynn_bl_driver:')
!      call mpas_log_write('--- config_mynn_cloudpdf    = $i',intArgs=(/bl_mynn_cloudpdf/))
!      call mpas_log_write('--- config_mynn_mixlength   = $i',intArgs=(/bl_mynn_mixlength/))
!      call mpas_log_write('--- config_mynn_stfunc      = $i',intArgs=(/bl_mynn_stfunc/))
!      call mpas_log_write('--- config_mynn_topdown     = $i',intArgs=(/bl_mynn_topdown/))
!      call mpas_log_write('--- config_mynn_scaleaware  = $i',intArgs=(/bl_mynn_scaleaware/))
!      call mpas_log_write('--- config_mynn_dheat_opt   = $i',intArgs=(/bl_mynn_dheat_opt/))
!      call mpas_log_write('--- config_mynn_edmf        = $i',intArgs=(/bl_mynn_edmf/))
!      call mpas_log_write('--- config_mynn_edmf_dd     = $i',intArgs=(/bl_mynn_edmf_dd/))
!      call mpas_log_write('--- config_mynn_edmf_mom    = $i',intArgs=(/bl_mynn_edmf_mom/))
!      call mpas_log_write('--- config_mynn_edmf_tke    = $i',intArgs=(/bl_mynn_edmf_tke/))
!      call mpas_log_write('--- config_mynn_edmf_output = $i',intArgs=(/bl_mynn_edmf_output/))
!      call mpas_log_write('--- config_mynn_mixscalars  = $i',intArgs=(/bl_mynn_mixscalars/))
!      call mpas_log_write('--- config_mynn_mixclouds   = $i',intArgs=(/bl_mynn_cloudmix/))
!      call mpas_log_write('--- config_mynn_mixqt       = $i',intArgs=(/bl_mynn_mixqt/))
!      call mpas_log_write('--- config_mynn_tkeadvect   = $l',logicArgs=(/bl_mynn_tkeadvect/))
!      call mpas_log_write('--- config_mynn_tkebudget   = $i',intArgs=(/bl_mynn_tkebudget/))
!      call mpas_log_write('--- config_mynn_closure     = $r',realArgs=(/bl_mynn_closure/))
!      call mpas_log_write(' ')
!      call mpas_log_write('--- f_qc   = $l',logicArgs=(/f_qc/)  )
!      call mpas_log_write('--- f_qi   = $l',logicArgs=(/f_qi/)  )
!      call mpas_log_write('--- f_qs   = $l',logicArgs=(/f_qs/)  )
!      call mpas_log_write('--- f_qoz  = $l',logicArgs=(/f_qoz/) )
!      call mpas_log_write('--- f_nc   = $l',logicArgs=(/f_nc/)  )
!      call mpas_log_write('--- f_ni   = $l',logicArgs=(/f_ni/)  )
!      call mpas_log_write('--- f_nifa = $l',logicArgs=(/f_nifa/))
!      call mpas_log_write('--- f_nwfa = $l',logicArgs=(/f_nwfa/))
!      call mpas_log_write('--- f_nbca = $l',logicArgs=(/f_nbca/))

       call mpas_timer_start('bl_mynn')
       call mynn_bl_driver( &
                  f_qc       = f_qc         , f_qi       = f_qi         , f_qs        = f_qs            , &
                  f_qoz      = f_qoz        , f_nc       = f_nc         , f_ni        = f_ni            , &
                  f_nifa     = f_nifa       , f_nwfa     = f_nwfa       , f_nbca      = f_nbca          , &
                  icloud_bl  = icloud_bl    , delt       = dt_pbl       , dx          = dx_p            , &
                  xland      = xland_p      , ps         = psfc_p       , ts          = tsk_p           , &
                  qsfc       = qsfc_p       , ust        = ust_p        , ch          = ch_p            , &
                  hfx        = hfx_p        , qfx        = qfx_p        , rmol        = rmol_p          , &
                  wspd       = wspd_p       , znt        = znt_p        , uoce        = uoce_p          , &
                  voce       = voce_p       , dz         = dz_p         , u           = u_p             , &
                  v          = v_p          , w          = w_p          , th          = th_p            , &
                  tt         = t_p          , p          = pres_hyd_p   , exner       = pi_p            , &
                  rho        = rho_p        , qv         = qv_p         , qc          = qc_p            , &
                  qi         = qi_p         , qs         = qs_p         , nc          = nc_p            , &
                  ni         = ni_p         , nifa       = nifa_p       , nwfa        = nwfa_p          , &
                  rthraten   = rthraten_p   , pblh       = hpbl_p       , kpbl        = kpbl_p          , &
                  cldfra_bl  = cldfrabl_p   , qc_bl      = qcbl_p       , qi_bl       = qibl_p          , &
                  maxwidth   = maxwidthbl_p , maxmf      = maxmfbl_p    , ktop_plume  = kbl_plume_p     , &
                  ztop_plume = zbl_plume_p  , dqke       = dqke_p       , qke_adv     = qkeadv_p        , &
                  tsq        = tsq_p        , qsq        = qsq_p        , cov         = cov_p           , &
                  el_pbl     = elpbl_p      , rublten    = rublten_p    , rvblten     = rvblten_p       , &
                  rthblten   = rthblten_p   , rqvblten   = rqvblten_p   , rqcblten    = rqcblten_p      , &
                  rqiblten   = rqiblten_p   , rqsblten   = rqsblten_p   , rncblten    = rncblten_p      , &
                  rniblten   = rniblten_p   , rnifablten = rnifablten_p , rnwfablten  = rnwfablten_p    , &
                  edmf_a     = edmfa_p      , edmf_w     = edmfw_p      , edmf_qt     = edmfqt_p        , &
                  edmf_thl   = edmfthl_p    , edmf_ent   = edmfent_p    , edmf_qc     = edmfqc_p        , &
                  sub_thl    = subthl_p     , sub_sqv    = subqv_p      , det_thl     = detthl_p        , &
                  det_sqv    = detqv_p      , exch_h     = kzh_p        , exch_m      = kzm_p           , &
                  qke        = qke_p        , qwt        = qwt_p        , qshear      = qshear_p        , &
                  qbuoy      = qbuoy_p      , qdiss      = qdiss_p      , sh3d        = sh3d_p          , &
                  sm3d       = sm3d_p       , spp_pbl    = spp_pbl      , pattern_spp = pattern_spp_pbl , &
                  do_restart         = config_do_restart   ,                                              &
                  do_DAcycling       = config_do_DAcycling ,                                              &
                  initflag           = initflag            ,                                              &
                  bl_mynn_tkeadvect  = bl_mynn_tkeadvect   ,                                              &
                  bl_mynn_tkebudget  = bl_mynn_tkebudget   ,                                              &
                  bl_mynn_cloudpdf   = bl_mynn_cloudpdf    ,                                              &
                  bl_mynn_mixlength  = bl_mynn_mixlength   ,                                              &
                  bl_mynn_closure    = bl_mynn_closure     ,                                              &
                  bl_mynn_stfunc     = bl_mynn_stfunc      ,                                              &
                  bl_mynn_topdown    = bl_mynn_topdown     ,                                              &
                  bl_mynn_scaleaware = bl_mynn_scaleaware  ,                                              &
                  bl_mynn_dheat_opt  = bl_mynn_dheat_opt   ,                                              &
                  bl_mynn_edmf       = bl_mynn_edmf        ,                                              &
                  bl_mynn_edmf_dd    = bl_mynn_edmf_dd     ,                                              &
                  bl_mynn_edmf_mom   = bl_mynn_edmf_mom    ,                                              &
                  bl_mynn_edmf_tke   = bl_mynn_edmf_tke    ,                                              &
                  bl_mynn_output     = bl_mynn_edmf_output ,                                              &
                  bl_mynn_mixscalars = bl_mynn_mixscalars  ,                                              &
                  bl_mynn_cloudmix   = bl_mynn_cloudmix    ,                                              &
                  bl_mynn_mixqt      = bl_mynn_mixqt       ,                                              &
                  ids = ids , ide = ide , jds = jds , jde = jde , kds = kds , kde = kde ,                 &
                  ims = ims , ime = ime , jms = jms , jme = jme , kms = kms , kme = kme ,                 &
                  its = its , ite = ite , jts = jts , jte = jte , kts = kts , kte = kte ,                 &
                  errmsg = errmsg , errflg = errflg                                                       &
                          )
       call mpas_timer_stop('bl_mynn')
!      call mpas_log_write('--- exit subroutine mynn_bl_driver:')
!      call mpas_log_write(' ')

    case default

 end select pbl_select

!copy local arrays to MPAS grid:
 call pbl_to_MPAS(configs,diag_physics,tend_physics,its,ite)

!call mpas_log_write('--- end subroutine driver_pbl.')

 end subroutine driver_pbl

!=================================================================================================================
 end module mpas_atmphys_driver_pbl
!=================================================================================================================
