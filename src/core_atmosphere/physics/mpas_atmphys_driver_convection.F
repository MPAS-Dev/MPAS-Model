! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module mpas_atmphys_driver_convection
 use mpas_kind_types
 use mpas_pool_routines
 use mpas_timer, only : mpas_timer_start, mpas_timer_stop

 use mpas_atmphys_constants
 use mpas_atmphys_utilities
 use mpas_atmphys_vars

!wrf physics:
 use module_cu_gf
 use module_cu_gf_wrf390
 use module_cu_kfeta
 use module_cu_tiedtke
 use module_cu_ntiedtke

 implicit none
 private
 public:: allocate_convection,     &
          deallocate_convection,   &
          init_convection,         &
          driver_convection,       &
          update_convection_step1, &
          update_convection_step2


!MPAS driver for parameterization of convection.
!Laura D. Fowler (send comments to laura@ucar.edu).
!2013-05-01.
!
! subroutines in mpas_atmphys_driver_convection:
! ----------------------------------------------
! allocate_convection     : allocate local arrays for parameterization of convection.
! deallocate_convection   : deallocate local arrays for parameterization of convection.
! init_convection         : initialization of individual convection scheme.
! driver_convection       : main driver (called from subroutine physics_driver).
! convection_from_MPAS    : initialize local arrays.
! convection_to_MPAS      : copy local arrays to MPAS arrays.
! update_convection_step1 : updates lifetime of deep convective clouds in Kain-Fritsch scheme.
! update_convection_step2 : updates accumulated precipitation output from convection schemes.
!
! WRF physics called from driver_convection:
! ------------------------------------------
! * module_cu_kfeta   : Kain-Fritsch convection scheme.
! * module_cu_tiedtke : Tiedtke convection scheme.

! add-ons and modifications to sourcecode:
! ----------------------------------------
! * removed the pre-processor option "do_hydrostatic_pressure" before call to the subroutines kf_eta_cps
!   and tiedtke. 
! * removed call to the updated Kain-Fritsch convection scheme.
!   Laura D. Fowler (laura@ucar.edu) / 2013-05-29.
! * added the mean distance between cell centers in the call to the Kain-Fritsch parameterization of convection.
!   Laura D. Fowler (laura@ucar.edu) / 2013-08-22. 
! * in call to subroutine kf_eta_cps, replaced the variable g (that originally pointed to gravity) with gravity,
!   for simplicity.
!   Laura D. Fowler (laura@ucar.edu) / 2014-03-21.
! * throughout the sourcecode, replaced all "var_struct" defined arrays by local pointers.
!   Laura D. Fowler (laura@ucar.edu) / 2014-04-22.
! * Modified sourcecode to use pools.
!   Laura D. Fowler (laura@ucar.edu) / 2014-05-15.
! * in ./physics_wrf, updated the Tiedtke convection scheme to that in WRF version 3.6.1. The call to subroutine 
!   cu_tiedtke has been updated accordingly to include the sensible heat flux.
!   Laura D. Fowler (laura@ucar.edu) / 2014-09-11.
! * renamed "kain_fritsch" with "cu_kain_fritsch" and "tiedtke" with "cu_tiedtke".
!   Laura D. Fowler (laura@ucar.edu) / 2016-03-22.
! * added the implementation of the Grell-Freitas convection scheme (option cu_grell_freitas).
!   Laura D. Fowler (laura@ucar.edu) / 2016-03-30.
!   code cleaner, now that we are using physics packages.
!   Laura D. Fowler (laura@ucar.edu) / 2016-04-01.
! * in the call to subroutine cu_grell_freitas, added the array pratec for the calculation of rainc.
!   Laura D. Fowler (laura@ucar.edu) / 2016-04-14.
! * in the call to subroutine cu_grell_freitas, removed the argument ktop_deep.
!   Laura D. Fowler (laura@ucar.edu) / 2016-04-20.
! * updated the call to the Tiedtke parameterization of convection to WRF version 3.8.1. This option is
!   triggered using the option cu_tiedtke.
!   Laura D. Fowler (laura@ucar.edu) / 2016-08-18.
! * added the call to the "new" Tiedtke parameterization of convection from WRF version 3.8.1. This option is
!   triggered using the option cu_ntiedtke.
!   Laura D. Fowler (laura@ucar.edu) / 2016-09-20.
! * for the kain_fritsch parameterization of convection, change the definition of dx_p to match that used in the
!   Grell-Freitas and "new Tiedtke" parameterization.
!   Laura D. Fowler (laura@ucar.edu) / 2016-10-18.
! * since we removed the local variable convection_scheme from mpas_atmphys_vars.F, now defines convection_scheme
!   as a pointer to config_convection_scheme.
!   Laura D. Fowler (laura@ucar.edu) / 2917-02-16.
! * renamed the variables xmb_total_p and xmb_shallow_p to xmb_deepc_p and xmb_shalc_p, respectively. Added the
!   variable xmb_midlc_p as the cloud base mass flux for mid-level convection.
!   Laura D. Fowler (laura@ucar.edu) / 2017-04-24.
! * added the variables pratecm_p,pratecs_p,raincm_p,raincs_p,raincvm_p,and raincvs_p to save diagnostics of the
!   instantaneous and accumulated precipitation for mid-level and shallow convection from the Grell-Freitas
!   convection scheme.
!   Laura D. Fowler (laura@ucar.edu) / 2017-04-25.
! * added the variables k22_deepc_p,k22_midlc_p,k22_shalc_p,kbcon_deepc_p,kbcon_midlc_p,kbcon_shalc_p,
!   ktop_deepc_p,ktop_midlc_p,and ktop_shalc_p to save diagnostics of the k22, cloud-base, and cloud_top levels
!   for deep, mid-level, and shallow convection from the Grell-Freitas convection scheme.
!   Laura D. Fowler (laura@ucar.edu) / 2017-04-26.
! * added outputs of the time-step and accumulated convective precipitation for the 16 individual closures
!   for the Grell-Freitas convection scheme.
!   Laura D. Fowler (laura@ucar.edu) / 2017-05-10.
! * for the Grell-Freitas scheme, added diagnostics of the updraft and downdraft massflux, and tendencies for
!   shallow, mid-level, and deep convection, separately.
!   Laura D. Fowler (laura@ucar.edu) / 2017-09-11.
! * updated the Kain-Fritsch convection scheme to that in WRF version 3.9.1.
!   Laura D. Fowler (laura@ucar.edu) / 2018-02-05.
! * in subroutine convection_from_MPAS, rewrote the initialization of variables needed to run the Tiedtke and
!   nTiedtke convection scheme. now use "association" so that the schemes can be run without full physics for
!   testing purposes.
!   Laura D. Fowler (laura@ucar.edu) / 2018-02-26.


 contains


!=================================================================================================================
 subroutine allocate_convection(configs,mesh)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: mesh

!local variables and pointers:
 character(len=StrKIND),pointer:: convection_scheme
 integer,pointer:: nclosures

 integer:: i,k,j

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_convection_scheme',convection_scheme)

 if(.not.allocated(cu_act_flag)) allocate(cu_act_flag(ims:ime,jms:jme)       )
 if(.not.allocated(rthcuten_p) ) allocate(rthcuten_p(ims:ime,kms:kme,jms:jme))
 if(.not.allocated(rqvcuten_p) ) allocate(rqvcuten_p(ims:ime,kms:kme,jms:jme))
 if(.not.allocated(rqccuten_p) ) allocate(rqccuten_p(ims:ime,kms:kme,jms:jme))
 if(.not.allocated(rqicuten_p) ) allocate(rqicuten_p(ims:ime,kms:kme,jms:jme))
 if(.not.allocated(pratec_p)   ) allocate(pratec_p(ims:ime,jms:jme)          )
 if(.not.allocated(raincv_p)   ) allocate(raincv_p(ims:ime,jms:jme)          )

 do i = its,ite
 do j = jts,jte
    pratec_p(i,j) = 0._RKIND
    raincv_p(i,j) = 0._RKIND
 enddo
 enddo

 do i = its,ite
 do k = kts,kte
 do j = jts,jte
    rthcuten_p(i,k,j) = 0._RKIND
    rqvcuten_p(i,k,j) = 0._RKIND
    rqccuten_p(i,k,j) = 0._RKIND
    rqicuten_p(i,k,j) = 0._RKIND
 enddo
 enddo
 enddo

 convection_select: select case(convection_scheme)

    case ("cu_grell_freitas","cu_grell_freitas_wrf390")
       if(.not.allocated(ht_p)           ) allocate(ht_p(ims:ime,jms:jme)               )
       if(.not.allocated(cubot_p)        ) allocate(cubot_p(ims:ime,jms:jme)            )
       if(.not.allocated(cutop_p)        ) allocate(cutop_p(ims:ime,jms:jme)            )
       if(.not.allocated(kpbl_p)         ) allocate(kpbl_p(ims:ime,jms:jme)             )
       if(.not.allocated(k22_deepc_p)    ) allocate(k22_deepc_p(ims:ime,jms:jme)        )
       if(.not.allocated(k22_midlc_p)    ) allocate(k22_midlc_p(ims:ime,jms:jme)        )
       if(.not.allocated(k22_shalc_p)    ) allocate(k22_shalc_p(ims:ime,jms:jme)        )
       if(.not.allocated(kbcon_deepc_p)  ) allocate(kbcon_deepc_p(ims:ime,jms:jme)      )
       if(.not.allocated(kbcon_midlc_p)  ) allocate(kbcon_midlc_p(ims:ime,jms:jme)      )
       if(.not.allocated(kbcon_shalc_p)  ) allocate(kbcon_shalc_p(ims:ime,jms:jme)      )
       if(.not.allocated(ktop_deepc_p)   ) allocate(ktop_deepc_p(ims:ime,jms:jme)       )
       if(.not.allocated(ktop_midlc_p)   ) allocate(ktop_midlc_p(ims:ime,jms:jme)       )
       if(.not.allocated(ktop_shalc_p)   ) allocate(ktop_shalc_p(ims:ime,jms:jme)       )

       if(.not.allocated(dx_p)           ) allocate(dx_p(ims:ime,jms:jme)               )
       if(.not.allocated(area_p)         ) allocate(area_p(ims:ime,jms:jme)             )
       if(.not.allocated(gsw_p)          ) allocate(gsw_p(ims:ime,jms:jme)              )
       if(.not.allocated(hfx_p)          ) allocate(hfx_p(ims:ime,jms:jme)              )
       if(.not.allocated(qfx_p)          ) allocate(qfx_p(ims:ime,jms:jme)              )
       if(.not.allocated(xland_p)        ) allocate(xland_p(ims:ime,jms:jme)            )

       if(.not.allocated(rthblten_p)     ) allocate(rthblten_p(ims:ime,kms:kme,jms:jme) )
       if(.not.allocated(rthdynten_p)    ) allocate(rthdynten_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(rthraten_p)     ) allocate(rthraten_p(ims:ime,kms:kme,jms:jme) )
       if(.not.allocated(rqvblten_p)     ) allocate(rqvblten_p(ims:ime,kms:kme,jms:jme) )
       if(.not.allocated(rqvdynten_p)    ) allocate(rqvdynten_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(rucuten_p)      ) allocate(rucuten_p(ims:ime,kms:kme,jms:jme)  )
       if(.not.allocated(rvcuten_p)      ) allocate(rvcuten_p(ims:ime,kms:kme,jms:jme)  )

       if(.not.allocated(xmb_deepc_p)    ) allocate(xmb_deepc_p(ims:ime,jms:jme)        )
       if(.not.allocated(xmb_midlc_p)    ) allocate(xmb_midlc_p(ims:ime,jms:jme)        )
       if(.not.allocated(xmb_shalc_p)    ) allocate(xmb_shalc_p(ims:ime,jms:jme)        )

       if(.not.allocated(qccu_p)         ) allocate(qccu_p(ims:ime,kms:kme,jms:jme)     )
       if(.not.allocated(qicu_p)         ) allocate(qicu_p(ims:ime,kms:kme,jms:jme)     )
       if(.not.allocated(zu_deepc_p)     ) allocate(zu_deepc_p(ims:ime,kms:kme,jms:jme) )
       if(.not.allocated(zd_deepc_p)     ) allocate(zd_deepc_p(ims:ime,kms:kme,jms:jme) )
       if(.not.allocated(zu_midlc_p)     ) allocate(zu_midlc_p(ims:ime,kms:kme,jms:jme) )
       if(.not.allocated(zd_midlc_p)     ) allocate(zd_midlc_p(ims:ime,kms:kme,jms:jme) )
       if(.not.allocated(zu_shalc_p)     ) allocate(zu_shalc_p(ims:ime,kms:kme,jms:jme) )

       if(.not.allocated(rthcuten_deepc_p)) allocate(rthcuten_deepc_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(rthcuten_midlc_p)) allocate(rthcuten_midlc_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(rthcuten_shalc_p)) allocate(rthcuten_shalc_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(rqvcuten_deepc_p)) allocate(rqvcuten_deepc_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(rqvcuten_midlc_p)) allocate(rqvcuten_midlc_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(rqvcuten_shalc_p)) allocate(rqvcuten_shalc_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(rqccuten_deepc_p)) allocate(rqccuten_deepc_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(rqccuten_midlc_p)) allocate(rqccuten_midlc_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(rqccuten_shalc_p)) allocate(rqccuten_shalc_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(rqicuten_deepc_p)) allocate(rqicuten_deepc_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(rqicuten_midlc_p)) allocate(rqicuten_midlc_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(rqicuten_shalc_p)) allocate(rqicuten_shalc_p(ims:ime,kms:kme,jms:jme))

       if(.not.allocated(pratecd_p)      ) allocate(pratecd_p(ims:ime,jms:jme)          )
       if(.not.allocated(pratecm_p)      ) allocate(pratecm_p(ims:ime,jms:jme)          )
       if(.not.allocated(pratecs_p)      ) allocate(pratecs_p(ims:ime,jms:jme)          )
       if(.not.allocated(raincd_p)       ) allocate(raincd_p(ims:ime,jms:jme)           )
       if(.not.allocated(raincm_p)       ) allocate(raincm_p(ims:ime,jms:jme)           )
       if(.not.allocated(raincs_p)       ) allocate(raincs_p(ims:ime,jms:jme)           )
       if(.not.allocated(raincvd_p)      ) allocate(raincvd_p(ims:ime,jms:jme)          )
       if(.not.allocated(raincvm_p)      ) allocate(raincvm_p(ims:ime,jms:jme)          )
       if(.not.allocated(raincvs_p)      ) allocate(raincvs_p(ims:ime,jms:jme)          )

       call mpas_pool_get_dimension(mesh,'nClosures',nclosures)
       if(.not.allocated(weights_clos_p) ) allocate(weights_clos_p(ims:ime,1:nclosures,jms:jme))
       do i = its,ite
       do k = 1,nclosures
       do j = jts,jte
          weights_clos_p(i,k,j) = 0.
       enddo
       enddo
       enddo

    case ("cu_kain_fritsch")
       if(.not.allocated(dx_p)           ) allocate(dx_p(ims:ime,jms:jme)                   )
       if(.not.allocated(area_p)         ) allocate(area_p(ims:ime,jms:jme)                 )
       if(.not.allocated(nca_p)          ) allocate(nca_p(ims:ime,jms:jme)                  )
       if(.not.allocated(cubot_p)        ) allocate(cubot_p(ims:ime,jms:jme)                )
       if(.not.allocated(cutop_p)        ) allocate(cutop_p(ims:ime,jms:jme)                )
       if(.not.allocated(timec_p)        ) allocate(timec_p(ims:ime,jms:jme)                )
       if(.not.allocated(w0avg_p)        ) allocate(w0avg_p(ims:ime,kms:kme,jms:jme)        )
       if(.not.allocated(qccu_p)         ) allocate(qccu_p(ims:ime,kms:kme,jms:jme)         )
       if(.not.allocated(qicu_p)         ) allocate(qicu_p(ims:ime,kms:kme,jms:jme)         )
       if(.not.allocated(cldfrac_deepc_p)) allocate(cldfrac_deepc_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(cldfrac_shalc_p)) allocate(cldfrac_shalc_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(rqrcuten_p)     ) allocate(rqrcuten_p(ims:ime,kms:kme,jms:jme)     )
       if(.not.allocated(rqscuten_p)     ) allocate(rqscuten_p(ims:ime,kms:kme,jms:jme)     )
       if(.not.allocated(udr_kf_p)       ) allocate(udr_kf_p(ims:ime,kms:kme,jms:jme)       )
       if(.not.allocated(ddr_kf_p)       ) allocate(ddr_kf_p(ims:ime,kms:kme,jms:jme)       )
       if(.not.allocated(uer_kf_p)       ) allocate(uer_kf_p(ims:ime,kms:kme,jms:jme)       )
       if(.not.allocated(der_kf_p)       ) allocate(der_kf_p(ims:ime,kms:kme,jms:jme)       )

       do i = its,ite
       do j = jts,jte
          cubot_p(i,j) = DBLE(kte+1)
          cutop_p(i,j) = DBLE(kts)
       enddo
       enddo

       do i = its,ite
       do k = kts,kte
       do j = jts,jte
          rqrcuten_p(i,k,j) = 0._RKIND
          rqscuten_p(i,k,j) = 0._RKIND
       enddo
       enddo
       enddo

    case ("cu_tiedtke","cu_ntiedtke")
       if(.not.allocated(hfx_p)  ) allocate(hfx_p(ims:ime,jms:jme)  )
       if(.not.allocated(qfx_p)  ) allocate(qfx_p(ims:ime,jms:jme)  )
       if(.not.allocated(xland_p)) allocate(xland_p(ims:ime,jms:jme))
       if(.not.allocated(rqvdynten_p)) allocate(rqvdynten_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(rqvblten_p) ) allocate(rqvblten_p(ims:ime,kms:kme,jms:jme) )
       if(.not.allocated(rucuten_p)  ) allocate(rucuten_p(ims:ime,kms:kme,jms:jme)  )
       if(.not.allocated(rvcuten_p)  ) allocate(rvcuten_p(ims:ime,kms:kme,jms:jme)  )

       do i = its,ite
       do k = kts,kte
       do j = jts,jte
          rucuten_p(i,k,j)     = 0._RKIND
          rvcuten_p(i,k,j)     = 0._RKIND
       enddo
       enddo
       enddo

       cu_tiedtke_select: select case(convection_scheme)
          case("cu_ntiedtke")
             if(.not.allocated(dx_p)) allocate(dx_p(ims:ime,jms:jme))
             if(.not.allocated(rthdynten_p)) allocate(rthdynten_p(ims:ime,kms:kme,jms:jme))
             if(.not.allocated(rthblten_p) ) allocate(rthblten_p(ims:ime,kms:kme,jms:jme) )
             if(.not.allocated(rqvften_p)  ) allocate(rqvften_p(ims:ime,kms:kme,jms:jme)  )
             if(.not.allocated(rthften_p)  ) allocate(rthften_p(ims:ime,kms:kme,jms:jme)  )
           case default
       end select cu_tiedtke_select

    case default

 end select convection_select

 end subroutine allocate_convection

!=================================================================================================================
 subroutine deallocate_convection(configs)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs

!local pointers:
 character(len=StrKIND),pointer:: convection_scheme

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_convection_scheme',convection_scheme)

 if(allocated(cu_act_flag)) deallocate(cu_act_flag)
 if(allocated(rthcuten_p) ) deallocate(rthcuten_p )
 if(allocated(rqvcuten_p) ) deallocate(rqvcuten_p )
 if(allocated(rqccuten_p) ) deallocate(rqccuten_p )
 if(allocated(rqicuten_p) ) deallocate(rqicuten_p )
 if(allocated(pratec_p)   ) deallocate(pratec_p   )
 if(allocated(raincv_p)   ) deallocate(raincv_p   )

 convection_select: select case(convection_scheme)

    case ("cu_grell_freitas","cu_grell_freitas_wrf390")
       if(allocated(ht_p)           ) deallocate(ht_p           )
       if(allocated(cubot_p)        ) deallocate(cubot_p        )
       if(allocated(cutop_p)        ) deallocate(cutop_p        )
       if(allocated(kpbl_p)         ) deallocate(kpbl_p         )
       if(allocated(k22_deepc_p)    ) deallocate(k22_deepc_p    )
       if(allocated(k22_midlc_p)    ) deallocate(k22_midlc_p    )
       if(allocated(k22_shalc_p)    ) deallocate(k22_shalc_p    )
       if(allocated(kbcon_deepc_p)  ) deallocate(kbcon_deepc_p  )
       if(allocated(kbcon_midlc_p)  ) deallocate(kbcon_midlc_p  )
       if(allocated(kbcon_shalc_p)  ) deallocate(kbcon_shalc_p  )
       if(allocated(ktop_deepc_p)   ) deallocate(ktop_deepc_p   )
       if(allocated(ktop_midlc_p)   ) deallocate(ktop_midlc_p   )
       if(allocated(ktop_shalc_p)   ) deallocate(ktop_shalc_p   )

       if(allocated(dx_p)           ) deallocate(dx_p           )
       if(allocated(area_p)         ) deallocate(area_p         )
       if(allocated(gsw_p)          ) deallocate(gsw_p          )
       if(allocated(hfx_p)          ) deallocate(hfx_p          )
       if(allocated(qfx_p)          ) deallocate(qfx_p          )
       if(allocated(xland_p)        ) deallocate(xland_p        )
       if(allocated(rthblten_p)     ) deallocate(rthblten_p     )
       if(allocated(rthdynten_p)    ) deallocate(rthdynten_p    )
       if(allocated(rthraten_p)     ) deallocate(rthraten_p     )
       if(allocated(rqvblten_p)     ) deallocate(rqvblten_p     )
       if(allocated(rqvdynten_p)    ) deallocate(rqvdynten_p    )
       if(allocated(rucuten_p)      ) deallocate(rucuten_p      )
       if(allocated(rvcuten_p)      ) deallocate(rvcuten_p      )

       if(allocated(xmb_deepc_p)    ) deallocate(xmb_deepc_p    )
       if(allocated(xmb_midlc_p)    ) deallocate(xmb_midlc_p    )
       if(allocated(xmb_shalc_p)    ) deallocate(xmb_shalc_p    )

       if(allocated(qccu_p)         ) deallocate(qccu_p         )
       if(allocated(qicu_p)         ) deallocate(qicu_p         )
       if(allocated(zu_deepc_p)     ) deallocate(zu_deepc_p     )
       if(allocated(zd_deepc_p)     ) deallocate(zd_deepc_p     )
       if(allocated(zu_midlc_p)     ) deallocate(zu_midlc_p     )
       if(allocated(zd_midlc_p)     ) deallocate(zd_midlc_p     )
       if(allocated(zu_shalc_p)     ) deallocate(zu_shalc_p     )

       if(allocated(rthcuten_deepc_p)) deallocate(rthcuten_deepc_p)
       if(allocated(rthcuten_midlc_p)) deallocate(rthcuten_midlc_p)
       if(allocated(rthcuten_shalc_p)) deallocate(rthcuten_shalc_p)
       if(allocated(rqvcuten_deepc_p)) deallocate(rqvcuten_deepc_p)
       if(allocated(rqvcuten_midlc_p)) deallocate(rqvcuten_midlc_p)
       if(allocated(rqvcuten_shalc_p)) deallocate(rqvcuten_shalc_p)
       if(allocated(rqccuten_deepc_p)) deallocate(rqccuten_deepc_p)
       if(allocated(rqccuten_midlc_p)) deallocate(rqccuten_midlc_p)
       if(allocated(rqccuten_shalc_p)) deallocate(rqccuten_shalc_p)
       if(allocated(rqicuten_deepc_p)) deallocate(rqicuten_deepc_p)
       if(allocated(rqicuten_midlc_p)) deallocate(rqicuten_midlc_p)
       if(allocated(rqicuten_shalc_p)) deallocate(rqicuten_shalc_p)

       if(allocated(pratecd_p)      ) deallocate(pratecd_p      )
       if(allocated(pratecm_p)      ) deallocate(pratecm_p      )
       if(allocated(pratecs_p)      ) deallocate(pratecs_p      )
       if(allocated(raincd_p)       ) deallocate(raincd_p       )
       if(allocated(raincm_p)       ) deallocate(raincm_p       )
       if(allocated(raincs_p)       ) deallocate(raincs_p       )
       if(allocated(raincvd_p)      ) deallocate(raincvd_p      )
       if(allocated(raincvm_p)      ) deallocate(raincvm_p      )
       if(allocated(raincvs_p)      ) deallocate(raincvs_p      )
       if(allocated(weights_clos_p) ) deallocate(weights_clos_p )

    case ("cu_kain_fritsch")
       if(allocated(dx_p)           ) deallocate(dx_p           )
       if(allocated(area_p)         ) deallocate(area_p         )
       if(allocated(nca_p)          ) deallocate(nca_p          )
       if(allocated(cubot_p)        ) deallocate(cubot_p        )
       if(allocated(cutop_p)        ) deallocate(cutop_p        )
       if(allocated(timec_p)        ) deallocate(timec_p        )
       if(allocated(w0avg_p)        ) deallocate(w0avg_p        )
       if(allocated(qccu_p)         ) deallocate(qccu_p         )
       if(allocated(qicu_p)         ) deallocate(qicu_p         )
       if(allocated(cldfrac_deepc_p)) deallocate(cldfrac_deepc_p)
       if(allocated(cldfrac_shalc_p)) deallocate(cldfrac_shalc_p)
       if(allocated(rqrcuten_p)     ) deallocate(rqrcuten_p     )
       if(allocated(rqscuten_p)     ) deallocate(rqscuten_p     )
       if(allocated(udr_kf_p)       ) deallocate(udr_kf_p       )
       if(allocated(ddr_kf_p)       ) deallocate(ddr_kf_p       )
       if(allocated(uer_kf_p)       ) deallocate(uer_kf_p       )
       if(allocated(der_kf_p)       ) deallocate(der_kf_p       )

    case ("cu_tiedtke","cu_ntiedtke")
       if(allocated(hfx_p)  ) deallocate(hfx_p  )
       if(allocated(qfx_p)  ) deallocate(qfx_p  )
       if(allocated(xland_p)) deallocate(xland_p)
       if(allocated(rqvdynten_p)) deallocate(rqvdynten_p)
       if(allocated(rqvblten_p) ) deallocate(rqvblten_p )
       if(allocated(rucuten_p)  ) deallocate(rucuten_p  )
       if(allocated(rvcuten_p)  ) deallocate(rvcuten_p  )

       cu_tiedtke_select: select case(convection_scheme)
          case ("cu_ntiedtke")
             if(allocated(dx_p)) deallocate(dx_p)
             if(allocated(rthdynten_p)) deallocate(rthdynten_p)
             if(allocated(rthblten_p) ) deallocate(rthblten_p )
             if(allocated(rqvften_p)  ) deallocate(rqvften_p  )
             if(allocated(rthften_p)  ) deallocate(rthften_p  )
          case default
       end select cu_tiedtke_select

    case default

 end select convection_select

 end subroutine deallocate_convection

!=================================================================================================================
 subroutine init_convection(mesh,configs,diag_physics)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: configs

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics

!local pointers:
 logical,pointer:: config_do_restart
 character(len=StrKIND),pointer:: convection_scheme

 integer,pointer:: nCells
 real(kind=RKIND),dimension(:),pointer:: nca

!local variables:
 integer:: iCell

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_dimension(mesh,'nCells',nCells)
 call mpas_pool_get_config(configs,'config_do_restart'       ,config_do_restart)
 call mpas_pool_get_config(configs,'config_convection_scheme',convection_scheme)

 convection_select: select case(convection_scheme)

    case ("cu_kain_fritsch")
       call mpas_pool_get_array(diag_physics,'nca',nca)
       if(.not. config_do_restart) then
          do iCell = 1, nCells
             nca(iCell) = -100._RKIND
          enddo
       endif
       call kf_lutab(svp1,svp2,svp3,svpt0)

    case default

 end select convection_select

 end subroutine init_convection

!=================================================================================================================
 subroutine driver_convection(itimestep,configs,mesh,sfc_input,diag_physics,tend_physics,its,ite)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: sfc_input

 integer,intent(in):: its,ite
 integer,intent(in):: itimestep

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: tend_physics

!local variables and arrays:
 logical:: log_convection

 integer:: i,j,k
 integer:: icount,initflag

 real(kind=RKIND):: dx

!local pointers:
 logical,pointer:: config_do_restart
 logical,pointer:: gfconv_deepc,gfconv_midlc,gfconv_shalc
 integer,pointer:: gfconv_closure_deepc,gfconv_closure_midlc,gfconv_closure_shalc
 integer,pointer:: nclosures

 integer,pointer:: gfconv_closure_deep,gfconv_closure_shallow
 character(len=StrKIND),pointer:: convection_scheme
 real(kind=RKIND),pointer:: len_disp

!variables specific to Kain_Fritsch parameterization:
 logical:: warm_rain,adapt_step_flag
 integer:: ktau
 real(kind=RKIND):: curr_secs
 real(kind=RKIND):: cudt
 real(kind=RKIND):: cudtacttime

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write('')
!call mpas_log_write('--- enter subroutine driver_convection:')

 call mpas_pool_get_config(configs,'config_gfconv_closure_deep',gfconv_closure_deep)
 call mpas_pool_get_config(configs,'config_gfconv_closure_shallow',gfconv_closure_shallow)
 call mpas_pool_get_config(configs,'config_len_disp'         ,len_disp         )
 call mpas_pool_get_config(configs,'config_do_restart'       ,config_do_restart)
 call mpas_pool_get_config(configs,'config_convection_scheme',convection_scheme)

!initialize instantaneous precipitation, and copy convective tendencies from the dynamics to
!the physics grid:
 call convection_from_MPAS(dt_dyn,configs,mesh,sfc_input,diag_physics,tend_physics,its,ite)

!... convert the convection time-step to minutes:
 cudt = dt_cu/60.

!... call to convection schemes:
 curr_secs   = -1
 cudtacttime = -1
 adapt_step_flag = .false.
 do j = jts, jte
 do i = its, ite
    cu_act_flag(i,j) = .false.
 enddo
 enddo

!... initialization of initflag needed in the calls cu_tiedtke and cu_ntiedtke:
 initflag = 1
 if(config_do_restart .or. itimestep > 1) initflag = 0

 convection_select: select case(convection_scheme)

    case ("cu_grell_freitas")
       call mpas_timer_start('Grell-Freitas')
       call cu_grell_freitas( &
             itimestep       = itimestep              , dt            = dt_dyn          , &
             dxCell          = dx_p                   , areaCell      = area_p          , &
             u               = u_p                    , v             = v_p             , &
             w               = w_p                    , t             = t_p             , &
             q               = qv_p                   , rho           = rho_p           , &
             p               = pres_hyd_p             , pi            = pi_p            , &
             p8w             = pres2_hyd_p            , dz8w          = dz_p            , &
             ht              = ht_p                   , xland         = xland_p         , &
             gsw             = gsw_p                  , xlv           = xlv             , &
             cp              = cp                     , g             = gravity         , &
             r_v             = R_v                    , hfx           = hfx_p           , &
             qfx             = qfx_p                  , rthblten      = rthblten_p      , &
             rqvblten        = rqvblten_p             , rthften       = rthdynten_p     , &
             rqvften         = rqvdynten_p            , rthraten      = rthraten_p      , &
             kpbl            = kpbl_p                 , raincv        = raincv_p        , &
             pratec          = pratec_p               , htop          = cutop_p         , &
             hbot            = cubot_p                , k22_shallow   = k22_shalc_p     , & 
             kbcon_shallow   = kbcon_shalc_p          , ktop_shallow  = ktop_shalc_p    , & 
             xmb_total       = xmb_deepc_p            , xmb_shallow   = xmb_shalc_p     , &
             gdc             = qccu_p                 , gdc2          = qicu_p          , &
             rthcuten        = rthcuten_p             , rqvcuten      = rqvcuten_p      , & 
             rqccuten        = rqccuten_p             , rqicuten      = rqicuten_p      , &
             ichoice_deep    = gfconv_closure_deep                                      , &
             ichoice_shallow = gfconv_closure_shallow                                   , &
             ishallow_g3     = ishallow                                                 , &
             ids = ids , ide = ide , jds = jds , jde = jde , kds = kds , kde = kde      , &
             ims = ims , ime = ime , jms = jms , jme = jme , kms = kds , kme = kme      , &
             its = its , ite = ite , jts = jts , jte = jte , kts = kts , kte = kte        &
                            )
       call mpas_timer_stop('Grell-Freitas')

    case ("cu_grell_freitas_wrf390")
       call mpas_log_write('')
       call mpas_log_write('--- enter subroutine cu_grell_freitas_wrf390:')

       call mpas_pool_get_config(configs,'config_gfconv_deepc',gfconv_deepc)
       call mpas_pool_get_config(configs,'config_gfconv_midlc',gfconv_midlc)
       call mpas_pool_get_config(configs,'config_gfconv_shalc',gfconv_shalc)
       call mpas_pool_get_config(configs,'config_gfconv_closure_deepc',gfconv_closure_deepc)
       call mpas_pool_get_config(configs,'config_gfconv_closure_midlc',gfconv_closure_midlc)
       call mpas_pool_get_config(configs,'config_gfconv_closure_shalc',gfconv_closure_shalc)

       call mpas_pool_get_dimension(mesh,'nClosures',nclosures)

       call mpas_log_write('gfconv_deepc = $l',logicArgs=(/gfconv_deepc/))
       call mpas_log_write('gfconv_midlc = $l',logicArgs=(/gfconv_midlc/))
       call mpas_log_write('gfconv_shalc = $l',logicArgs=(/gfconv_shalc/))
       call mpas_log_write('gfconv_closure_deepc = $i',intArgs=(/gfconv_closure_deepc/))
       call mpas_log_write('gfconv_closure_midlc = $i',intArgs=(/gfconv_closure_midlc/))
       call mpas_log_write('gfconv_closure_shalc = $i',intArgs=(/gfconv_closure_shalc/))

       call cu_grell_freitas_wrf390( &
               dt           = dt_dyn               , dx           = dx_p                 , &
               area         = area_p               , u            = u_p                  , &
               v            = v_p                  , w            = w_p                  , &
               t            = t_p                  , q            = qv_p                 , &
               rho          = rho_p                , p            = pres_hyd_p           , &
               pi           = pi_p                 , p8w          = pres2_hyd_p          , &
               dz8w         = dz_p                 , ht           = ht_p                 , &
               xland        = xland_p              , hfx          = hfx_p                , &
               qfx          = qfx_p                , kpbl         = kpbl_p               , &
               rthraten     = rthraten_p           , rthften      = rthdynten_p          , &
               rqvften      = rqvdynten_p          , rqvblten     = rqvblten_p           , &
               rthblten     = rthblten_p           , do_deepc     = gfconv_deepc         , &
               do_midlc     = gfconv_midlc         , do_shalc     = gfconv_shalc         , &
               ichoice      = gfconv_closure_deepc , ichoice_m    = gfconv_closure_midlc , &
               ichoice_s    = gfconv_closure_shalc , kbcon_deepc  = kbcon_deepc_p        , &
               ktop_deepc   = ktop_deepc_p         , k22_deepc    = k22_deepc_p          , &
               kbcon_midlc  = kbcon_midlc_p        , ktop_midlc   = ktop_midlc_p         , &
               k22_midlc    = k22_midlc_p          , kbcon_shalc  = kbcon_shalc_p        , &
               ktop_shalc   = ktop_shalc_p         , k22_shalc    = k22_shalc_p          , &
               xmb_deepc    = xmb_deepc_p          , xmb_midlc    = xmb_midlc_p          , &
               xmb_shalc    = xmb_shalc_p          , zu_deepc     = zu_deepc_p           , &
               zd_deepc     = zd_deepc_p           , zu_midlc     = zu_midlc_p           , &
               zd_midlc     = zd_midlc_p           , zu_shalc     = zu_shalc_p           , &
               raincv       = raincv_p             , raincvd      = raincvd_p            , &
               raincvm      = raincvm_p            , raincvs      = raincvs_p            , &
               pratec       = pratec_p             , pratecd      = pratecd_p            , &
               pratecm      = pratecm_p            , pratecs      = pratecs_p            , &
               rthcuten     = rthcuten_p           , rqvcuten     = rqvcuten_p           , &
               rqccuten     = rqccuten_p           , rqicuten     = rqicuten_p           , &
               rucuten      = rucuten_p            , rvcuten      = rvcuten_p            , &
               rthcuten_deepc = rthcuten_deepc_p   , rqvcuten_deepc = rqvcuten_deepc_p   , &
               rqccuten_deepc = rqccuten_deepc_p   , rqicuten_deepc = rqicuten_deepc_p   , &
               rthcuten_midlc = rthcuten_midlc_p   , rqvcuten_midlc = rqvcuten_midlc_p   , &
               rqccuten_midlc = rqccuten_midlc_p   , rqicuten_midlc = rqicuten_midlc_p   , &
               rthcuten_shalc = rthcuten_shalc_p   , rqvcuten_shalc = rqvcuten_shalc_p   , &
               rqccuten_shalc = rqccuten_shalc_p   , rqicuten_shalc = rqicuten_shalc_p   , &
               gdc          = qccu_p               , gdc2         = qicu_p               , &
               nclos        = nclosures            , weights_clos = weights_clos_p       , &
               ids = ids , ide = ide , jds = jds , jde = jde , kds = kds , kde = kde     , &
               ims = ims , ime = ime , jms = jms , jme = jme , kms = kds , kme = kme     , &
               its = its , ite = ite , jts = jts , jte = jte , kts = kts , kte = kte       &
                                   )
       call mpas_log_write('--- end subroutine cu_grell_freitas_wrf390.')
       call mpas_log_write('')

    case ("cu_kain_fritsch")
       if(itimestep == 1) then
          ktau = itimestep
       else
          ktau = itimestep + 1
       endif

       call kf_eta_cps( &
             pcps         = pres_hyd_p      , t               = t_p             ,    &
             dt           = dt_dyn          , ktau            = ktau            ,    &
             dxCell       = dx_p            , areaCell        = area_p          ,    &
             cudt         = cudt            , adapt_step_flag = adapt_step_flag ,    &
             rho          = rho_p           , raincv          = raincv_p        ,    &
             pratec       = pratec_p        , nca             = nca_p           ,    &
             u            = u_p             , v               = v_p             ,    &
             th           = th_p            , pi              = pi_p            ,    &
             w            = w_p             , dz8w            = dz_p            ,    &
             w0avg        = w0avg_p         , xlv0            = xlv0            ,    &
             xlv1         = xlv1            , xls0            = xls0            ,    &
             xls1         = xls1            , cp              = cp              ,    &
             r            = r_d             , g               = gravity         ,    &
             ep1          = ep_1            , ep2             = ep_2            ,    &
             svp1         = svp1            , svp2            = svp2            ,    &
             svp3         = svp3            , svpt0           = svpt0           ,    &
             stepcu       = n_cu            , cu_act_flag     = cu_act_flag     ,    &
             warm_rain    = warm_rain       , cutop           = cutop_p         ,    &
             cubot        = cubot_p         , qv              = qv_p            ,    &
             f_qv         = f_qv            , f_qc            = f_qc            ,    &
             f_qr         = f_qr            , f_qi            = f_qi            ,    &
             f_qs         = f_qs            , kf_edrates      = kf_edrates      ,    &
             timec_kf     = timec_p         , qc_kf           = qccu_p          ,    &
             qi_kf        = qicu_p          , cldfra_dp_kf    = cldfrac_deepc_p ,    &
             cldfra_sh_kf = cldfrac_shalc_p , udr_kf          = udr_kf_p        ,    &
             ddr_kf       = ddr_kf_p        , uer_kf          = uer_kf_p        ,    &
             der_kf       = der_kf_p        , rthcuten        = rthcuten_p      ,    &
             rqvcuten     = rqvcuten_p      , rqccuten        = rqccuten_p      ,    &
             rqrcuten     = rqrcuten_p      , rqicuten        = rqicuten_p      ,    &
             rqscuten     = rqscuten_p         ,                                     &
             ids = ids , ide = ide , jds = jds , jde = jde , kds = kds , kde = kde , &
             ims = ims , ime = ime , jms = jms , jme = jme , kms = kds , kme = kme , &
             its = its , ite = ite , jts = jts , jte = jte , kts = kts , kte = kte   &
                        )
       call mpas_timer_stop('Kain-Fritsch')

    case("cu_tiedtke")
       call mpas_timer_start('Tiedtke')
       call cu_tiedtke( &
             pcps        = pres_hyd_p    , p8w       = pres2_hyd_p   ,               &
             znu         = znu_hyd_p     , t3d       = t_p           ,               &
             dt          = dt_dyn        , itimestep = initflag      ,               &
             stepcu      = n_cu          , raincv    = raincv_p      ,               &
             pratec      = pratec_p      , qfx       = qfx_p         ,               &
             u3d         = u_p           , v3d       = v_p           ,               &
             w           = w_p           , qv3d      = qv_p          ,               &
             qc3d        = qc_p          , qi3d      = qi_p          ,               &
             pi3d        = pi_p          , rho3d     = rho_p         ,               &
             qvften      = rqvdynten_p   , qvpblten  = rqvblten_p    ,               &
             dz8w        = dz_p          , xland     = xland_p       ,               &
             cu_act_flag = cu_act_flag   , f_qv      = f_qv          ,               &
             f_qc        = f_qc          , f_qr      = f_qr          ,               &
             f_qi        = f_qi          , f_qs      = f_qs          ,               &
             rthcuten    = rthcuten_p    , rqvcuten  = rqvcuten_p    ,               &
             rqccuten    = rqccuten_p    , rqicuten  = rqicuten_p    ,               &
             rucuten     = rucuten_p     , rvcuten   = rvcuten_p     ,               &
             ids = ids , ide = ide , jds = jds , jde = jde , kds = kds , kde = kde , &
             ims = ims , ime = ime , jms = jms , jme = jme , kms = kds , kme = kme , &
             its = its , ite = ite , jts = jts , jte = jte , kts = kts , kte = kte   &
                      )
       call mpas_timer_stop('Tiedtke')

    case("cu_ntiedtke")
       call mpas_timer_start('New_Tiedtke')
       call cu_ntiedtke( &
             pcps      = pres_hyd_p  , p8w         = pres2_hyd_p ,                   &
             t3d       = t_p         , dz8w        = dz_p        ,                   &
             dt        = dt_dyn      , itimestep   = initflag    ,                   &
             stepcu    = n_cu        , raincv      = raincv_p    ,                   & 
             pratec    = pratec_p    , qfx         = qfx_p       ,                   &
             hfx       = hfx_p       , xland       = xland_p     ,                   &
             dx        = dx_p        , u3d         = u_p         ,                   &
             v3d       = v_p         , w           = w_p         ,                   &
             qv3d      = qv_p        , qc3d        = qc_p        ,                   &
             qi3d      = qi_p        , pi3d        = pi_p        ,                   &
             rho3d     = rho_p       , qvften      = rqvften_p   ,                   &
             thften    = rthften_p   , cu_act_flag = cu_act_flag ,                   &
             f_qv      = f_qv        , f_qc        = f_qc        ,                   &
             f_qr      = f_qr        , f_qi        = f_qi        ,                   &
             f_qs      = f_qs        , rthcuten    = rthcuten_p  ,                   &
             rqvcuten  = rqvcuten_p  , rqccuten    = rqccuten_p  ,                   &
             rqicuten  = rqicuten_p  , rucuten     = rucuten_p   ,                   &
             rvcuten   = rvcuten_p   ,                                               &
             ids = ids , ide = ide , jds = jds , jde = jde , kds = kds , kde = kde , &
             ims = ims , ime = ime , jms = jms , jme = jme , kms = kds , kme = kme , &
             its = its , ite = ite , jts = jts , jte = jte , kts = kts , kte = kte   &
                              )
       call mpas_timer_stop('New_Tiedtke')

    case default

 end select convection_select

!copy instantaneous and accumulated precipitation, convective tendencies, and "other" arrays
!specific to convection parameterization back to the dynamics grid:
 call convection_to_MPAS(configs,mesh,diag_physics,tend_physics,its,ite)

!call mpas_log_write('--- end subroutine driver_convection.')
!call mpas_log_write('')

 end subroutine driver_convection

!=================================================================================================================
 subroutine convection_from_MPAS(dt_dyn,configs,mesh,sfc_input,diag_physics,tend_physics,its,ite)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: sfc_input
 type(mpas_pool_type),intent(in):: diag_physics
 type(mpas_pool_type),intent(in):: tend_physics

 integer,intent(in):: its,ite
 real(kind=RKIND),intent(in):: dt_dyn

!local pointers:
 character(len=StrKIND),pointer:: convection_scheme
 integer,dimension(:),pointer:: kpbl
 integer,dimension(:),pointer:: k22_deepc,k22_midlc,k22_shalc
 integer,dimension(:),pointer:: kbcon_deepc,kbcon_midlc,kbcon_shalc
 integer,dimension(:),pointer:: ktop_deepc,ktop_midlc,ktop_shalc
 real(kind=RKIND),dimension(:),pointer  :: areaCell,meshDensity
 real(kind=RKIND),dimension(:),pointer  :: nca,timec,cubot,cutop,cuprec,raincv
 real(kind=RKIND),dimension(:),pointer  :: gsw,hfx,qfx,xland
 real(kind=RKIND),dimension(:),pointer  :: xmb_deepc
 real(kind=RKIND),dimension(:,:),pointer:: zgrid
 real(kind=RKIND),dimension(:,:),pointer:: qc_cu,qi_cu
 real(kind=RKIND),dimension(:,:),pointer:: cldfrac_deepc,cldfrac_shalc
 real(kind=RKIND),dimension(:,:),pointer:: zu_deepc,zd_deepc,zu_midlc,zd_midlc,zu_shalc
 real(kind=RKIND),dimension(:,:),pointer:: udr_kf,uer_kf,ddr_kf,der_kf
 real(kind=RKIND),dimension(:,:),pointer:: w0avg
 real(kind=RKIND),dimension(:,:),pointer:: rthcuten,rqvcuten,rqccuten,rqicuten,rqrcuten,rqscuten 
 real(kind=RKIND),dimension(:,:),pointer:: rthblten,rthdynten,rthratenlw,rthratensw
 real(kind=RKIND),dimension(:,:),pointer:: rqvblten,rqvdynten,rucuten,rvcuten
 real(kind=RKIND),dimension(:,:),pointer:: rthcuten_deepc,rqvcuten_deepc,rqccuten_deepc,rqicuten_deepc
 real(kind=RKIND),dimension(:,:),pointer:: rthcuten_midlc,rqvcuten_midlc,rqccuten_midlc,rqicuten_midlc
 real(kind=RKIND),dimension(:,:),pointer:: rthcuten_shalc,rqvcuten_shalc,rqccuten_shalc,rqicuten_shalc

!local pointers for the Grell-Freitas convection scheme:
 real(kind=RKIND),dimension(:),pointer  :: xmb_midlc,xmb_shalc
 real(kind=RKIND),dimension(:),pointer  :: cuprecd,cuprecm,cuprecs,raincvd,raincvm,raincvs

!local variables:
 integer:: i,j,k
 integer:: iEdge
 real(kind=RKIND),pointer:: len_disp

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_convection_scheme',convection_scheme)

 call mpas_pool_get_array(diag_physics,'cuprec',cuprec)
 call mpas_pool_get_array(diag_physics,'raincv',raincv)

 call mpas_pool_get_array(tend_physics,'rthcuten',rthcuten)
 call mpas_pool_get_array(tend_physics,'rqvcuten',rqvcuten)
 call mpas_pool_get_array(tend_physics,'rqccuten',rqccuten)
 call mpas_pool_get_array(tend_physics,'rqicuten',rqicuten)

 do j = jts,jte
 do i = its,ite
    raincv_p(i,j) = raincv(i)
    pratec_p(i,j) = cuprec(i)
    do k = kts,kte
       rthcuten_p(i,k,j) = rthcuten(k,i)
       rqvcuten_p(i,k,j) = rqvcuten(k,i)
       rqccuten_p(i,k,j) = rqccuten(k,i)
       rqicuten_p(i,k,j) = rqicuten(k,i)
    enddo
 enddo
 enddo

 convection_select: select case(convection_scheme)

    case ("cu_grell_freitas","cu_grell_freitas_wrf390")
       call mpas_pool_get_config(configs,'config_len_disp',len_disp)

       call mpas_pool_get_array(mesh,'areaCell'   ,areaCell   )
       call mpas_pool_get_array(mesh,'meshDensity',meshDensity)
       call mpas_pool_get_array(mesh,'zgrid'      ,zgrid      )

       call mpas_pool_get_array(sfc_input,'xland',xland)

       call mpas_pool_get_array(diag_physics,'gsw'        ,gsw        )
       call mpas_pool_get_array(diag_physics,'hfx'        ,hfx        )
       call mpas_pool_get_array(diag_physics,'qfx'        ,qfx        )
       call mpas_pool_get_array(diag_physics,'kpbl'       ,kpbl       )
       call mpas_pool_get_array(diag_physics,'cubot'      ,cubot      )
       call mpas_pool_get_array(diag_physics,'cutop'      ,cutop      )
       call mpas_pool_get_array(diag_physics,'xmb_deepc'  ,xmb_deepc  )
       call mpas_pool_get_array(diag_physics,'xmb_midlc'  ,xmb_midlc  )
       call mpas_pool_get_array(diag_physics,'xmb_shalc'  ,xmb_shalc  )
       call mpas_pool_get_array(diag_physics,'k22_deepc'  ,k22_deepc  )
       call mpas_pool_get_array(diag_physics,'k22_midlc'  ,k22_midlc  )
       call mpas_pool_get_array(diag_physics,'k22_shalc'  ,k22_shalc  )
       call mpas_pool_get_array(diag_physics,'kbcon_deepc',kbcon_deepc)
       call mpas_pool_get_array(diag_physics,'kbcon_midlc',kbcon_midlc)
       call mpas_pool_get_array(diag_physics,'kbcon_shalc',kbcon_shalc)
       call mpas_pool_get_array(diag_physics,'ktop_deepc' ,ktop_deepc )
       call mpas_pool_get_array(diag_physics,'ktop_midlc' ,ktop_midlc )
       call mpas_pool_get_array(diag_physics,'ktop_shalc' ,ktop_shalc )
       call mpas_pool_get_array(diag_physics,'cuprecd'    ,cuprecd    )
       call mpas_pool_get_array(diag_physics,'cuprecm'    ,cuprecm    )
       call mpas_pool_get_array(diag_physics,'cuprecs'    ,cuprecs    )
       call mpas_pool_get_array(diag_physics,'raincvd'    ,raincvd    )
       call mpas_pool_get_array(diag_physics,'raincvm'    ,raincvm    )
       call mpas_pool_get_array(diag_physics,'raincvs'    ,raincvs    )
       call mpas_pool_get_array(diag_physics,'qc_cu'      ,qc_cu      )
       call mpas_pool_get_array(diag_physics,'qi_cu'      ,qi_cu      )
       call mpas_pool_get_array(diag_physics,'zu_deepc'   ,zu_deepc   )
       call mpas_pool_get_array(diag_physics,'zd_deepc'   ,zd_deepc   )
       call mpas_pool_get_array(diag_physics,'zu_midlc'   ,zu_midlc   )
       call mpas_pool_get_array(diag_physics,'zd_midlc'   ,zd_midlc   )
       call mpas_pool_get_array(diag_physics,'zu_shalc'   ,zu_shalc   )

       call mpas_pool_get_array(tend_physics,'rqvblten'  ,rqvblten  )
       call mpas_pool_get_array(tend_physics,'rqvdynten' ,rqvdynten )
       call mpas_pool_get_array(tend_physics,'rthblten'  ,rthblten  )
       call mpas_pool_get_array(tend_physics,'rthdynten' ,rthdynten )
       call mpas_pool_get_array(tend_physics,'rthratenlw',rthratenlw)
       call mpas_pool_get_array(tend_physics,'rthratensw',rthratensw)
       call mpas_pool_get_array(tend_physics,'rucuten'   ,rucuten   )
       call mpas_pool_get_array(tend_physics,'rvcuten'   ,rvcuten   )
       call mpas_pool_get_array(tend_physics,'rthcuten_deepc',rthcuten_deepc)
       call mpas_pool_get_array(tend_physics,'rqvcuten_deepc',rqvcuten_deepc)
       call mpas_pool_get_array(tend_physics,'rqccuten_deepc',rqccuten_deepc)
       call mpas_pool_get_array(tend_physics,'rqicuten_deepc',rqicuten_deepc)
       call mpas_pool_get_array(tend_physics,'rthcuten_midlc',rthcuten_midlc)
       call mpas_pool_get_array(tend_physics,'rqvcuten_midlc',rqvcuten_midlc)
       call mpas_pool_get_array(tend_physics,'rqccuten_midlc',rqccuten_midlc)
       call mpas_pool_get_array(tend_physics,'rqicuten_midlc',rqicuten_midlc)
       call mpas_pool_get_array(tend_physics,'rthcuten_shalc',rthcuten_shalc)
       call mpas_pool_get_array(tend_physics,'rqvcuten_shalc',rqvcuten_shalc)
       call mpas_pool_get_array(tend_physics,'rqccuten_shalc',rqccuten_shalc)
       call mpas_pool_get_array(tend_physics,'rqicuten_shalc',rqicuten_shalc)

       do j = jts,jte
       do i = its,ite
          dx_p(i,j) = len_disp / meshDensity(i)**0.25
          area_p(i,j)        = areaCell(i)
          ht_p(i,j)          = zgrid(1,i)
          xland_p(i,j)       = xland(i)
          gsw_p(i,j)         = gsw(i)
          hfx_p(i,j)         = hfx(i)
          qfx_p(i,j)         = qfx(i)
          kpbl_p(i,j)        = kpbl(i)
          cubot_p(i,j)       = cubot(i)
          cutop_p(i,j)       = cutop(i)

          xmb_deepc_p(i,j)   = xmb_deepc(i)
          xmb_midlc_p(i,j)   = xmb_midlc(i)
          xmb_shalc_p(i,j)   = xmb_shalc(i)
          pratecd_p(i,j)     = cuprecd(i)
          pratecm_p(i,j)     = cuprecm(i)
          pratecs_p(i,j)     = cuprecs(i)
          raincvd_p(i,j)     = raincvd(i)
          raincvm_p(i,j)     = raincvm(i)
          raincvs_p(i,j)     = raincvs(i)

          k22_deepc_p(i,j)   = k22_deepc(i)
          k22_midlc_p(i,j)   = k22_midlc(i)
          k22_shalc_p(i,j)   = k22_shalc(i)
          kbcon_deepc_p(i,j) = kbcon_deepc(i)
          kbcon_midlc_p(i,j) = kbcon_midlc(i)
          kbcon_shalc_p(i,j) = kbcon_shalc(i)
          ktop_deepc_p(i,j)  = ktop_deepc(i)
          ktop_midlc_p(i,j)  = ktop_midlc(i)
          ktop_shalc_p(i,j)  = ktop_shalc(i)

          do k = kts,kte
             qccu_p(i,k,j)      = qc_cu(k,i)
             qicu_p(i,k,j)      = qi_cu(k,i)
             zu_deepc_p(i,k,j)  = zu_deepc(k,i)
             zd_deepc_p(i,k,j)  = zd_deepc(k,i)
             zu_midlc_p(i,k,j)  = zu_midlc(k,i)
             zd_midlc_p(i,k,j)  = zd_midlc(k,i)
             zu_shalc_p(i,k,j)  = zu_shalc(k,i)

             rthcuten_deepc_p(i,k,j) = rthcuten_deepc(k,i)
             rqvcuten_deepc_p(i,k,j) = rqvcuten_deepc(k,i)
             rqccuten_deepc_p(i,k,j) = rqccuten_deepc(k,i)
             rqicuten_deepc_p(i,k,j) = rqicuten_deepc(k,i)
             rthcuten_midlc_p(i,k,j) = rthcuten_midlc(k,i)
             rqvcuten_midlc_p(i,k,j) = rqvcuten_midlc(k,i)
             rqccuten_midlc_p(i,k,j) = rqccuten_midlc(k,i)
             rqicuten_midlc_p(i,k,j) = rqicuten_midlc(k,i)
             rthcuten_shalc_p(i,k,j) = rthcuten_shalc(k,i)
             rqvcuten_shalc_p(i,k,j) = rqvcuten_shalc(k,i)
             rqccuten_shalc_p(i,k,j) = rqccuten_shalc(k,i)
             rqicuten_shalc_p(i,k,j) = rqicuten_shalc(k,i)

             rthblten_p(i,k,j)  = rthblten(k,i)
             rthdynten_p(i,k,j) = rthdynten(k,i)
             rthraten_p(i,k,j)  = rthratenlw(k,i) + rthratensw(k,i)
             rqvblten_p(i,k,j)  = rqvblten(k,i)
             rqvdynten_p(i,k,j) = rqvdynten(k,i)
             rucuten_p(i,k,j)   = rucuten(k,i)
             rvcuten_p(i,k,j)   = rvcuten(k,k)
          enddo
       enddo
       enddo

    case ("cu_kain_fritsch")
       call mpas_pool_get_config(configs,'config_len_disp',len_disp)

       call mpas_pool_get_array(mesh,'areaCell',areaCell)
       call mpas_pool_get_array(mesh,'meshDensity',meshDensity)

       call mpas_pool_get_array(diag_physics,'cubot' ,cubot )
       call mpas_pool_get_array(diag_physics,'cutop' ,cutop )
       call mpas_pool_get_array(diag_physics,'nca'   ,nca   )
       call mpas_pool_get_array(diag_physics,'w0avg' ,w0avg )
       call mpas_pool_get_array(diag_physics,'timec' ,timec )
       call mpas_pool_get_array(diag_physics,'udr_kf',udr_kf)
       call mpas_pool_get_array(diag_physics,'uer_kf',uer_kf)
       call mpas_pool_get_array(diag_physics,'ddr_kf',ddr_kf)
       call mpas_pool_get_array(diag_physics,'der_kf',der_kf)
       call mpas_pool_get_array(diag_physics,'qc_cu' ,qc_cu )
       call mpas_pool_get_array(diag_physics,'qi_cu' ,qi_cu )
       call mpas_pool_get_array(diag_physics,'cldfrac_deepc',cldfrac_deepc)
       call mpas_pool_get_array(diag_physics,'cldfrac_shalc',cldfrac_shalc)

       call mpas_pool_get_array(tend_physics,'rqrcuten',rqrcuten)
       call mpas_pool_get_array(tend_physics,'rqscuten',rqscuten)

       do j = jts,jte
       do i = its,ite
          !area of grid-cell:       
          dx_p(i,j)    = len_disp / meshDensity(i)**0.25
          area_p(i,j)  = areaCell(i)
          cubot_p(i,j) = cubot(i)
          cutop_p(i,j) = cutop(i)
          timec_p(i,j) = timec(i)

          do k = kts,kte
             udr_kf_p(i,k,j)        = udr_kf(k,i)
             uer_kf_p(i,k,j)        = uer_kf(k,i)
             ddr_kf_p(i,k,j)        = ddr_kf(k,i)
             der_kf_p(i,k,j)        = der_kf(k,i)
             qccu_p(i,k,j)          = qc_cu(k,i)
             qicu_p(i,k,j)          = qi_cu(k,i)
             cldfrac_deepc_p(i,k,j) = cldfrac_deepc(k,i)
             cldfrac_shalc_p(i,k,j) = cldfrac_shalc(k,i)
             rqrcuten_p(i,k,j)      = rqrcuten(k,i)
             rqscuten_p(i,k,j)      = rqscuten(k,i)
          enddo

          !decreases the characteristic time period that convection remains active. When nca_p
          !becomes less than the convective timestep, convective tendencies and precipitation
          !are reset to zero (note that this is also done in subroutine kf_eta_cps).
          nca_p(i,j) = nca(i)

          do k = kts,kte
             w0avg_p(i,k,j) = w0avg(k,i)
          enddo
       enddo
       enddo

    case ("cu_tiedtke","cu_ntiedtke")
       nullify(hfx)
       nullify(qfx)
       call mpas_pool_get_array(sfc_input,'xland',xland)
       call mpas_pool_get_array(diag_physics,'hfx',hfx)
       call mpas_pool_get_array(diag_physics,'qfx',qfx)

       nullify(rqvblten )
       nullify(rqvdynten)
       call mpas_pool_get_array(tend_physics,'rqvblten' ,rqvblten )
       call mpas_pool_get_array(tend_physics,'rqvdynten',rqvdynten)
       call mpas_pool_get_array(tend_physics,'rucuten'  ,rucuten  )
       call mpas_pool_get_array(tend_physics,'rvcuten'  ,rvcuten  )

       do j = jts,jte
          do i = its,ite
             xland_p(i,j) = xland(i)
             hfx_p(i,j)   = 0._RKIND
             qfx_p(i,j)   = 0._RKIND
             if(associated(hfx) .and. associated(qfx)) then
                hfx_p(i,j)   = hfx(i)
                qfx_p(i,j)   = qfx(i)
             endif
          enddo
          do k = kts,kte
          do i = its,ite
             rqvblten_p(i,k,j)  = 0._RKIND
             rqvdynten_p(i,k,j) = 0._RKIND
             if(associated(rqvblten) ) rqvblten_p(i,k,j)  = rqvblten(k,i)
             if(associated(rqvdynten)) rqvdynten_p(i,k,j) = rqvdynten(k,i)

             rucuten_p(i,k,j) = rucuten(k,i)
             rvcuten_p(i,k,j) = rvcuten(k,i)
          enddo  
          enddo
       enddo

       cu_tiedtke_select: select case(convection_scheme)
          case("cu_ntiedtke")
             call mpas_pool_get_config(configs,'config_len_disp',len_disp)
             call mpas_pool_get_array(mesh,'meshDensity',meshDensity)

             nullify(rthdynten)
             nullify(rthblten)
             nullify(rthratenlw)
             nullify(rthratensw)
             call mpas_pool_get_array(tend_physics,'rthdynten' ,rthdynten )
             call mpas_pool_get_array(tend_physics,'rthblten'  ,rthblten  )
             call mpas_pool_get_array(tend_physics,'rthratenlw',rthratenlw)
             call mpas_pool_get_array(tend_physics,'rthratensw',rthratensw)
             
             do j = jts,jte
                do i = its,ite
                   dx_p(i,j) = len_disp / meshDensity(i)**0.25
                enddo
                do k = kts,kte
                do i = its,ite
                   rthften_p(i,k,j) = 0._RKIND
                   rqvften_p(i,k,j) = rqvdynten_p(i,k,j) + rqvblten_p(i,k,j)

                   if(associated(rthdynten)) rthften_p(i,k,j) = rthdynten(k,i)
                   if(associated(rthblten))  rthften_p(i,k,j) = rthften_p(i,k,j) + rthblten(k,i)
                   if(associated(rthratenlw) .and. associated(rthratensw)) &
                      rthften_p(i,k,j) = rthften_p(i,k,j) + rthratenlw(k,i) + rthratensw(k,i)
                   rthften_p(i,k,j) = rthften_p(i,k,j)*pi_p(i,k,j) 
                enddo
                enddo
             enddo
          case default
       end select cu_tiedtke_select

    case default

 end select convection_select

 end subroutine convection_from_MPAS

!=================================================================================================================
 subroutine convection_to_MPAS(configs,mesh,diag_physics,tend_physics,its,ite)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: mesh
 integer,intent(in):: its,ite

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: tend_physics

!local variables:
 integer:: i,k,j

!local pointers:
 character(len=StrKIND),pointer:: convection_scheme
 integer,pointer:: nclosures
 integer,dimension(:),pointer:: k22_deepc,k22_midlc,k22_shalc
 integer,dimension(:),pointer:: kbcon_deepc,kbcon_midlc,kbcon_shalc
 integer,dimension(:),pointer:: ktop_deepc,ktop_midlc,ktop_shalc
 real(kind=RKIND),dimension(:),pointer  :: nca,timec,cubot,cutop,cuprec,raincv
 real(kind=RKIND),dimension(:),pointer  :: xmb_deepc
 real(kind=RKIND),dimension(:,:),pointer:: qc_cu,qi_cu
 real(kind=RKIND),dimension(:,:),pointer:: cldfrac_deepc,cldfrac_shalc
 real(kind=RKIND),dimension(:,:),pointer:: zu_deepc,zd_deepc,zu_midlc,zd_midlc,zu_shalc
 real(kind=RKIND),dimension(:,:),pointer:: udr_kf,uer_kf,ddr_kf,der_kf
 real(kind=RKIND),dimension(:,:),pointer:: w0avg
 real(kind=RKIND),dimension(:,:),pointer:: rthcuten,rqvcuten,rqccuten,rqicuten,rqrcuten,rqscuten
 real(kind=RKIND),dimension(:,:),pointer:: rucuten,rvcuten
 real(kind=RKIND),dimension(:,:),pointer:: rthcuten_deepc,rqvcuten_deepc,rqccuten_deepc,rqicuten_deepc
 real(kind=RKIND),dimension(:,:),pointer:: rthcuten_midlc,rqvcuten_midlc,rqccuten_midlc,rqicuten_midlc
 real(kind=RKIND),dimension(:,:),pointer:: rthcuten_shalc,rqvcuten_shalc,rqccuten_shalc,rqicuten_shalc

!local pointers for the Grell-Freitas convection scheme:
 real(kind=RKIND),dimension(:),pointer  :: xmb_midlc,xmb_shalc
 real(kind=RKIND),dimension(:),pointer  :: cuprecd,cuprecm,cuprecs,raincvd,raincvm,raincvs
 real(kind=RKIND),dimension(:,:),pointer:: weights_closures,raincvd_closures

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_convection_scheme',convection_scheme)

 call mpas_pool_get_array(diag_physics,'cuprec',cuprec)
 call mpas_pool_get_array(diag_physics,'raincv',raincv)

 call mpas_pool_get_array(tend_physics,'rthcuten',rthcuten)
 call mpas_pool_get_array(tend_physics,'rqvcuten',rqvcuten)
 call mpas_pool_get_array(tend_physics,'rqccuten',rqccuten)
 call mpas_pool_get_array(tend_physics,'rqicuten',rqicuten)

 do j = jts,jte
 do i = its,ite
    raincv(i) = raincv_p(i,j)
    cuprec(i) = pratec_p(i,j)
    do k = kts, kte
       rthcuten(k,i) = rthcuten_p(i,k,j)
       rqvcuten(k,i) = rqvcuten_p(i,k,j)
       rqccuten(k,i) = rqccuten_p(i,k,j)
       rqicuten(k,i) = rqicuten_p(i,k,j)
    enddo
 enddo
 enddo

 convection_select: select case(convection_scheme)

    case ("cu_grell_freitas","cu_grell_freitas_wrf390")
       call mpas_pool_get_dimension(mesh,'nClosures',nclosures)

       call mpas_pool_get_array(diag_physics,'cubot'      ,cubot      )
       call mpas_pool_get_array(diag_physics,'cutop'      ,cutop      )
       call mpas_pool_get_array(diag_physics,'xmb_deepc'  ,xmb_deepc  )
       call mpas_pool_get_array(diag_physics,'xmb_midlc'  ,xmb_midlc  )
       call mpas_pool_get_array(diag_physics,'xmb_shalc'  ,xmb_shalc  )
       call mpas_pool_get_array(diag_physics,'k22_deepc'  ,k22_deepc  )
       call mpas_pool_get_array(diag_physics,'k22_midlc'  ,k22_midlc  )
       call mpas_pool_get_array(diag_physics,'k22_shalc'  ,k22_shalc  )
       call mpas_pool_get_array(diag_physics,'kbcon_deepc',kbcon_deepc)
       call mpas_pool_get_array(diag_physics,'kbcon_midlc',kbcon_midlc)
       call mpas_pool_get_array(diag_physics,'kbcon_shalc',kbcon_shalc)
       call mpas_pool_get_array(diag_physics,'ktop_deepc' ,ktop_deepc )
       call mpas_pool_get_array(diag_physics,'ktop_midlc' ,ktop_midlc )
       call mpas_pool_get_array(diag_physics,'ktop_shalc' ,ktop_shalc )
       call mpas_pool_get_array(diag_physics,'cuprecd'    ,cuprecd    )
       call mpas_pool_get_array(diag_physics,'cuprecm'    ,cuprecm    )
       call mpas_pool_get_array(diag_physics,'cuprecs'    ,cuprecs    )
       call mpas_pool_get_array(diag_physics,'raincvd'    ,raincvd    )
       call mpas_pool_get_array(diag_physics,'raincvm'    ,raincvm    )
       call mpas_pool_get_array(diag_physics,'raincvs'    ,raincvs    )
       call mpas_pool_get_array(diag_physics,'qc_cu'      ,qc_cu      )
       call mpas_pool_get_array(diag_physics,'qi_cu'      ,qi_cu      )
       call mpas_pool_get_array(diag_physics,'zu_deepc'   ,zu_deepc   )
       call mpas_pool_get_array(diag_physics,'zd_deepc'   ,zd_deepc   )
       call mpas_pool_get_array(diag_physics,'zu_midlc'   ,zu_midlc   )
       call mpas_pool_get_array(diag_physics,'zd_midlc'   ,zd_midlc   )
       call mpas_pool_get_array(diag_physics,'zu_shalc'   ,zu_shalc   )

       call mpas_pool_get_array(diag_physics,'weights_closures',weights_closures)
       call mpas_pool_get_array(diag_physics,'raincvd_closures',raincvd_closures)

       call mpas_pool_get_array(tend_physics,'rucuten',rucuten)
       call mpas_pool_get_array(tend_physics,'rvcuten',rvcuten)
       call mpas_pool_get_array(tend_physics,'rthcuten_deepc',rthcuten_deepc)
       call mpas_pool_get_array(tend_physics,'rqvcuten_deepc',rqvcuten_deepc)
       call mpas_pool_get_array(tend_physics,'rqccuten_deepc',rqccuten_deepc)
       call mpas_pool_get_array(tend_physics,'rqicuten_deepc',rqicuten_deepc)
       call mpas_pool_get_array(tend_physics,'rthcuten_midlc',rthcuten_midlc)
       call mpas_pool_get_array(tend_physics,'rqvcuten_midlc',rqvcuten_midlc)
       call mpas_pool_get_array(tend_physics,'rqccuten_midlc',rqccuten_midlc)
       call mpas_pool_get_array(tend_physics,'rqicuten_midlc',rqicuten_midlc)
       call mpas_pool_get_array(tend_physics,'rthcuten_shalc',rthcuten_shalc)
       call mpas_pool_get_array(tend_physics,'rqvcuten_shalc',rqvcuten_shalc)
       call mpas_pool_get_array(tend_physics,'rqccuten_shalc',rqccuten_shalc)
       call mpas_pool_get_array(tend_physics,'rqicuten_shalc',rqicuten_shalc)

       do j = jts,jte
       do i = its,ite
          cubot(i)         = cubot_p(i,j)
          cutop(i)         = cutop_p(i,j)
          xmb_deepc(i)     = xmb_deepc_p(i,j)
          xmb_midlc(i)     = xmb_midlc_p(i,j)
          xmb_shalc(i)     = xmb_shalc_p(i,j)
          cuprecd(i)       = pratecd_p(i,j)
          cuprecm(i)       = pratecm_p(i,j)
          cuprecs(i)       = pratecs_p(i,j)
          raincvd(i)       = raincvd_p(i,j)
          raincvm(i)       = raincvm_p(i,j)
          raincvs(i)       = raincvs_p(i,j)

          k22_deepc(i)   = k22_deepc_p(i,j)
          k22_midlc(i)   = k22_midlc_p(i,j)
          k22_shalc(i)   = k22_shalc_p(i,j)
          kbcon_deepc(i) = kbcon_deepc_p(i,j)
          kbcon_midlc(i) = kbcon_midlc_p(i,j)
          kbcon_shalc(i) = kbcon_shalc_p(i,j)
          ktop_deepc(i)  = ktop_deepc_p(i,j)
          ktop_midlc(i)  = ktop_midlc_p(i,j)
          ktop_shalc(i)  = ktop_shalc_p(i,j)

          do k = kts,kte
             qc_cu(k,i)    = qccu_p(i,k,j)
             qi_cu(k,i)    = qicu_p(i,k,j)
             zu_deepc(k,i) = zu_deepc_p(i,k,j)
             zd_deepc(k,i) = zd_deepc_p(i,k,j)
             zu_midlc(k,i) = zu_midlc_p(i,k,j)
             zd_midlc(k,i) = zd_midlc_p(i,k,j)
             zu_shalc(k,i) = zu_shalc_p(i,k,j)
             rucuten(k,i)  = rucuten_p(i,k,j)
             rvcuten(k,k)  = rvcuten_p(i,k,j)

             rthcuten_deepc(k,i) = rthcuten_deepc_p(i,k,j)
             rqvcuten_deepc(k,i) = rqvcuten_deepc_p(i,k,j)
             rqccuten_deepc(k,i) = rqccuten_deepc_p(i,k,j)
             rqicuten_deepc(k,i) = rqicuten_deepc_p(i,k,j)
             rthcuten_midlc(k,i) = rthcuten_midlc_p(i,k,j)
             rqvcuten_midlc(k,i) = rqvcuten_midlc_p(i,k,j)
             rqccuten_midlc(k,i) = rqccuten_midlc_p(i,k,j)
             rqicuten_midlc(k,i) = rqicuten_midlc_p(i,k,j)
             rthcuten_shalc(k,i) = rthcuten_shalc_p(i,k,j)
             rqvcuten_shalc(k,i) = rqvcuten_shalc_p(i,k,j)
             rqccuten_shalc(k,i) = rqccuten_shalc_p(i,k,j)
             rqicuten_shalc(k,i) = rqicuten_shalc_p(i,k,j)
          enddo

          do k = 1,nclosures
             weights_closures(k,i) = weights_clos_p(i,k,j)
             raincvd_closures(k,i) = raincvd(i)*weights_closures(k,i)
          enddo
       enddo
       enddo

    case ("cu_kain_fritsch")
       call mpas_pool_get_array(diag_physics,'cubot',cubot)
       call mpas_pool_get_array(diag_physics,'cutop',cutop)
       call mpas_pool_get_array(diag_physics,'nca'  ,nca  )
       call mpas_pool_get_array(diag_physics,'w0avg',w0avg)
       call mpas_pool_get_array(diag_physics,'timec' ,timec )
       call mpas_pool_get_array(diag_physics,'udr_kf',udr_kf)
       call mpas_pool_get_array(diag_physics,'uer_kf',uer_kf)
       call mpas_pool_get_array(diag_physics,'ddr_kf',ddr_kf)
       call mpas_pool_get_array(diag_physics,'der_kf',der_kf)
       call mpas_pool_get_array(diag_physics,'qc_cu' ,qc_cu )
       call mpas_pool_get_array(diag_physics,'qi_cu' ,qi_cu )
       call mpas_pool_get_array(diag_physics,'cldfrac_deepc',cldfrac_deepc)
       call mpas_pool_get_array(diag_physics,'cldfrac_shalc',cldfrac_shalc)


       call mpas_pool_get_array(tend_physics,'rqrcuten',rqrcuten)
       call mpas_pool_get_array(tend_physics,'rqscuten',rqscuten)

       do j = jts,jte
       do i = its,ite
          cubot(i) = cubot_p(i,j)
          cutop(i) = cutop_p(i,j)
          nca(i)   = nca_p(i,j)
          timec(i) = timec_p(i,j)
          do k = kts, kte
             w0avg(k,i)         = w0avg_p(i,k,j)
             udr_kf(k,i)        = udr_kf_p(i,k,j)
             uer_kf(k,i)        = uer_kf_p(i,k,j)
             ddr_kf(k,i)        = ddr_kf_p(i,k,j)
             der_kf(k,i)        = der_kf_p(i,k,j)
             qc_cu(k,i)         = qccu_p(i,k,j)
             qi_cu(k,i)         = qicu_p(i,k,j)
             cldfrac_deepc(k,i) = cldfrac_deepc_p(i,k,j)
             cldfrac_shalc(k,i) = cldfrac_shalc_p(i,k,j)
             rqrcuten(k,i)      = rqrcuten_p(i,k,j)
             rqscuten(k,i)      = rqscuten_p(i,k,j)
          enddo                          
       enddo
       enddo

    case ("cu_tiedtke","cu_ntiedtke")
       call mpas_pool_get_array(tend_physics,'rucuten',rucuten)
       call mpas_pool_get_array(tend_physics,'rvcuten',rvcuten)

       do j = jts,jte
       do k = kts,kte
       do i = its,ite
          rucuten(k,i) = rucuten_p(i,k,j)
          rvcuten(k,i) = rvcuten_p(i,k,j)
       enddo
       enddo
       enddo

    case default

 end select convection_select

 end subroutine convection_to_MPAS

!=================================================================================================================
 subroutine update_convection_step1(configs,diag_physics,tend_physics,its,ite)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs
 integer,intent(in):: its,ite

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: tend_physics

!local pointers:
 character(len=StrKIND),pointer:: convection_scheme
 real(kind=RKIND),dimension(:),pointer  :: nca,cubot,cutop,cuprec,raincv
 real(kind=RKIND),dimension(:,:),pointer:: rthcuten,rqvcuten,rqccuten,rqicuten,rqrcuten,rqscuten

!local variables and arrays:
 integer:: i,k

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_convection_scheme',convection_scheme)

 convection_select: select case(convection_scheme)

    case ("cu_kain_fritsch")
      call mpas_pool_get_array(diag_physics,'nca'   ,nca   )
      call mpas_pool_get_array(diag_physics,'cubot' ,cubot )
      call mpas_pool_get_array(diag_physics,'cutop' ,cutop )
      call mpas_pool_get_array(diag_physics,'cuprec',cuprec)
      call mpas_pool_get_array(diag_physics,'raincv',raincv)

      call mpas_pool_get_array(tend_physics,'rthcuten',rthcuten)
      call mpas_pool_get_array(tend_physics,'rqvcuten',rqvcuten)
      call mpas_pool_get_array(tend_physics,'rqccuten',rqccuten)
      call mpas_pool_get_array(tend_physics,'rqicuten',rqicuten)
      call mpas_pool_get_array(tend_physics,'rqrcuten',rqrcuten)
      call mpas_pool_get_array(tend_physics,'rqscuten',rqscuten)

       do i = its, ite
          !decreases the characteristic time period that convection remains active. When nca_p
          !becomes less than the convective timestep, convective tendencies and precipitation
          !are reset to zero (note that this is also done in subroutine kf_eta_cps).
          if(nca(i) .gt. 0.) then
             nca(i) = nca(i) - dt_dyn
             
             if(nca(i) .lt. 0.5*dt_dyn) then
                do k = kts,kte
                   rthcuten(k,i) = 0._RKIND
                   rqvcuten(k,i) = 0._RKIND
                   rqccuten(k,i) = 0._RKIND
                   rqrcuten(k,i) = 0._RKIND
                   rqicuten(k,i) = 0._RKIND
                   rqscuten(k,i) = 0._RKIND
                enddo
                raincv(i) = 0._RKIND
                cuprec(i) = 0._RKIND
                cubot(i)  = kte+1
                cutop(i)  = kts
             endif
          endif
       enddo

    case default

 end select convection_select

 end subroutine update_convection_step1

!=================================================================================================================
 subroutine update_convection_step2(configs,mesh,diag_physics,its,ite)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: mesh
 integer,intent(in):: its,ite

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics

!local pointers:
 character(len=StrKIND),pointer:: convection_scheme
 integer,pointer:: nclosures
 integer,dimension(:),pointer  :: i_rainc
 integer,dimension(:,:),pointer:: i_raincd_closures

 real(kind=RKIND),pointer:: bucket_rainc
 real(kind=RKIND),dimension(:),pointer  :: cuprec,rainc
 real(kind=RKIND),dimension(:),pointer  :: cuprecd,cuprecm,cuprecs,raincd,raincm,raincs
 real(kind=RKIND),dimension(:,:),pointer:: raincd_closures,weights_closures

!local variables and arrays:
 integer:: i,n

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_convection_scheme',convection_scheme)
 call mpas_pool_get_config(configs,'config_bucket_rainc'     ,bucket_rainc     )

 call mpas_pool_get_array(diag_physics,'i_rainc',i_rainc)
 call mpas_pool_get_array(diag_physics,'cuprec' ,cuprec )
 call mpas_pool_get_array(diag_physics,'rainc'  ,rainc  )

!update the accumulated precipitation at the end of each dynamic time-step:
 do i = its, ite
    rainc(i) = rainc(i) + cuprec(i) * dt_dyn

    if(l_acrain .and. bucket_rainc.gt.0._RKIND .and. &
       rainc(i).gt.bucket_rainc) then
       i_rainc(i) = i_rainc(i) + 1
       rainc(i)   = rainc(i) - bucket_rainc
    endif
 enddo

!update the accumulated precipitation from mid-level and shallow convection from the Grell-Freitas at the
!end of each dynamic time-step:
 convection_select: select case(convection_scheme)

    case ("cu_grell_freitas_wrf390")
       call mpas_pool_get_dimension(mesh,'nClosures',nclosures)

       call mpas_pool_get_array(diag_physics,'cuprecd',cuprecd)
       call mpas_pool_get_array(diag_physics,'cuprecm',cuprecm)
       call mpas_pool_get_array(diag_physics,'cuprecs',cuprecs)
       call mpas_pool_get_array(diag_physics,'raincd' ,raincd)
       call mpas_pool_get_array(diag_physics,'raincm' ,raincm)
       call mpas_pool_get_array(diag_physics,'raincs' ,raincs)
       call mpas_pool_get_array(diag_physics,'weights_closures' ,weights_closures)
       call mpas_pool_get_array(diag_physics,'raincd_closures'  ,raincd_closures  )
       call mpas_pool_get_array(diag_physics,'i_raincd_closures',i_raincd_closures)

       do i = its, ite
          raincd(i) = raincd(i) + cuprecd(i)*dt_dyn
          raincm(i) = raincm(i) + cuprecm(i)*dt_dyn
          raincs(i) = raincs(i) + cuprecs(i)*dt_dyn

          do n = 1, nclosures
             raincd_closures(n,i) = raincd_closures(n,i)+weights_closures(n,i)*cuprecd(i)*dt_dyn

             if(l_acrain .and. bucket_rainc.gt.0._RKIND .and. &
                raincd_closures(n,i).gt.bucket_rainc) then
                i_raincd_closures(n,i) = i_raincd_closures(n,i) + 1
                raincd_closures(n,i)   = raincd_closures(n,i) - bucket_rainc
             endif
          enddo
       enddo

     case default

 end select convection_select

 end subroutine update_convection_step2

!=================================================================================================================
 end module mpas_atmphys_driver_convection
!=================================================================================================================
