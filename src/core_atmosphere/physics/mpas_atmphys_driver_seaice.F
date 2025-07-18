! Copyright (c) 2024 The University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module mpas_atmphys_driver_seaice
 use mpas_kind_types
 use mpas_pool_routines,only: mpas_pool_get_array,mpas_pool_get_config,mpas_pool_type
 use mpas_log

 use mpas_atmphys_constants,only: rcp
 use mpas_atmphys_lsm_shared,only: correct_tsk_over_seaice
 use mpas_atmphys_vars
 use module_sf_noah_seaice_drv
 use module_sf_sfcdiags

 implicit none
 private
 public:: allocate_seaice,   &
          deallocate_seaice, &
          driver_seaice

 logical,parameter:: frpcpn   = .false.

!urban physics: MPAS does not plan to run the urban physics option.
 integer,parameter:: sf_urban_physics = 0 !activate urban canopy model (=0: no urban canopy)


!MPAS driver for parameterization of surface processes over seaice points.
!Laura D. Fowler (laura@ucar.edu) / 2024-03-13.


 contains


!=================================================================================================================
 subroutine allocate_seaice(configs)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs

!local pointers:
 character(len=StrKIND),pointer:: lsm_scheme

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_lsm_scheme',lsm_scheme)

 if(.not.allocated(acsnom_p)    ) allocate(acsnom_p(ims:ime,jms:jme)    )
 if(.not.allocated(acsnow_p)    ) allocate(acsnow_p(ims:ime,jms:jme)    )
 if(.not.allocated(albsi_p)     ) allocate(albsi_p(ims:ime,jms:jme)     )
 if(.not.allocated(br_p)        ) allocate(br_p(ims:ime,jms:jme)        )
 if(.not.allocated(chs_p)       ) allocate(chs_p(ims:ime,jms:jme)       )
 if(.not.allocated(chs2_p)      ) allocate(chs2_p(ims:ime,jms:jme)      )
 if(.not.allocated(cpm_p)       ) allocate(cpm_p(ims:ime,jms:jme)       )
 if(.not.allocated(cqs2_p)      ) allocate(cqs2_p(ims:ime,jms:jme)      )
 if(.not.allocated(qgh_p)       ) allocate(qgh_p(ims:ime,jms:jme)       )
 if(.not.allocated(qsfc_p)      ) allocate(qsfc_p(ims:ime,jms:jme)      )
 if(.not.allocated(glw_p)       ) allocate(glw_p(ims:ime,jms:jme)       )
 if(.not.allocated(grdflx_p)    ) allocate(grdflx_p(ims:ime,jms:jme)    )
 if(.not.allocated(icedepth_p)  ) allocate(icedepth_p(ims:ime,jms:jme)  )
 if(.not.allocated(hfx_p)       ) allocate(hfx_p(ims:ime,jms:jme)       )
 if(.not.allocated(qfx_p)       ) allocate(qfx_p(ims:ime,jms:jme)       )
 if(.not.allocated(lh_p)        ) allocate(lh_p(ims:ime,jms:jme)        )
 if(.not.allocated(rainbl_p)    ) allocate(rainbl_p(ims:ime,jms:jme)    )
 if(.not.allocated(sfc_albedo_p)) allocate(sfc_albedo_p(ims:ime,jms:jme))
 if(.not.allocated(sfc_emiss_p) ) allocate(sfc_emiss_p(ims:ime,jms:jme) )
 if(.not.allocated(sfcrunoff_p) ) allocate(sfcrunoff_p(ims:ime,jms:jme) )
 if(.not.allocated(snoalb_p)    ) allocate(snoalb_p(ims:ime,jms:jme)    )
 if(.not.allocated(snow_p)      ) allocate(snow_p(ims:ime,jms:jme)      )
 if(.not.allocated(snowc_p)     ) allocate(snowc_p(ims:ime,jms:jme)     )
 if(.not.allocated(snowh_p)     ) allocate(snowh_p(ims:ime,jms:jme)     )
 if(.not.allocated(snowsi_p)    ) allocate(snowsi_p(ims:ime,jms:jme)    )
 if(.not.allocated(swdown_p)    ) allocate(swdown_p(ims:ime,jms:jme)    )
 if(.not.allocated(sr_p)        ) allocate(sr_p(ims:ime,jms:jme)        )
 if(.not.allocated(tsk_p)       ) allocate(tsk_p(ims:ime,jms:jme)       )
 if(.not.allocated(xice_p)      ) allocate(xice_p(ims:ime,jms:jme)      )
 if(.not.allocated(z0_p)        ) allocate(z0_p(ims:ime,jms:jme)        )
 if(.not.allocated(znt_p)       ) allocate(znt_p(ims:ime,jms:jme)       )

 if(.not.allocated(tsk_sea)     ) allocate(tsk_sea(ims:ime,jms:jme)     )
 if(.not.allocated(tsk_ice)     ) allocate(tsk_ice(ims:ime,jms:jme)     )
 if(.not.allocated(albsi_p)     ) allocate(albsi_p(ims:ime,jms:jme)     )
 if(.not.allocated(icedepth_p)  ) allocate(icedepth_p(ims:ime,jms:jme)  )
 if(.not.allocated(snowsi_p)    ) allocate(snowsi_p(ims:ime,jms:jme)    )

 if(.not.allocated(tslb_p)) allocate(tslb_p(ims:ime,1:num_soils,jms:jme))

 sf_select: select case(trim(lsm_scheme))
    case("sf_noah")
       if(.not.allocated(noahres_p)) allocate(noahres_p(ims:ime,jms:jme))
       if(.not.allocated(potevp_p) ) allocate(potevp_p(ims:ime,jms:jme) )
       if(.not.allocated(snopcx_p) ) allocate(snopcx_p(ims:ime,jms:jme) )

    case default
 end select sf_select

 end subroutine allocate_seaice

!=================================================================================================================
 subroutine deallocate_seaice(configs)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs

!local pointers:
 character(len=StrKIND),pointer:: lsm_scheme

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_lsm_scheme',lsm_scheme)

 if(allocated(acsnom_p)    ) deallocate(acsnom_p    )
 if(allocated(acsnow_p)    ) deallocate(acsnow_p    )
 if(allocated(albsi_p)     ) deallocate(albsi_p     )
 if(allocated(br_p)        ) deallocate(br_p        )
 if(allocated(chs_p)       ) deallocate(chs_p       )
 if(allocated(chs2_p)      ) deallocate(chs2_p      )
 if(allocated(cpm_p)       ) deallocate(cpm_p       )
 if(allocated(cqs2_p)      ) deallocate(cqs2_p      )
 if(allocated(qgh_p)       ) deallocate(qgh_p       )
 if(allocated(qsfc_p)      ) deallocate(qsfc_p      )
 if(allocated(glw_p)       ) deallocate(glw_p       )
 if(allocated(grdflx_p)    ) deallocate(grdflx_p    )
 if(allocated(icedepth_p)  ) deallocate(icedepth_p  )
 if(allocated(hfx_p)       ) deallocate(hfx_p       )
 if(allocated(qfx_p)       ) deallocate(qfx_p       )
 if(allocated(lh_p)        ) deallocate(lh_p        )
 if(allocated(rainbl_p)    ) deallocate(rainbl_p    )
 if(allocated(sfc_albedo_p)) deallocate(sfc_albedo_p)
 if(allocated(sfc_emiss_p) ) deallocate(sfc_emiss_p )
 if(allocated(sfcrunoff_p) ) deallocate(sfcrunoff_p )
 if(allocated(snoalb_p)    ) deallocate(snoalb_p    )
 if(allocated(snow_p)      ) deallocate(snow_p      )
 if(allocated(snowc_p)     ) deallocate(snowc_p     )
 if(allocated(snowh_p)     ) deallocate(snowh_p     )
 if(allocated(snowsi_p)    ) deallocate(snowsi_p    )
 if(allocated(swdown_p)    ) deallocate(swdown_p    )
 if(allocated(sr_p)        ) deallocate(sr_p        )
 if(allocated(tsk_p)       ) deallocate(tsk_p       )
 if(allocated(xice_p)      ) deallocate(xice_p      )
 if(allocated(z0_p)        ) deallocate(z0_p        )
 if(allocated(znt_p)       ) deallocate(znt_p       )

 if(allocated(chs_sea)     ) deallocate(chs_sea     )
 if(allocated(chs2_sea)    ) deallocate(chs2_sea    )
 if(allocated(cqs2_sea)    ) deallocate(cqs2_sea    )
 if(allocated(cpm_sea)     ) deallocate(cpm_sea     )
 if(allocated(hfx_sea)     ) deallocate(hfx_sea     )
 if(allocated(qfx_sea)     ) deallocate(qfx_sea     )
 if(allocated(qgh_sea)     ) deallocate(qgh_sea     )
 if(allocated(qsfc_sea)    ) deallocate(qsfc_sea    )
 if(allocated(lh_sea)      ) deallocate(lh_sea      )
 if(allocated(tsk_sea)     ) deallocate(tsk_sea     )
 if(allocated(tsk_ice)     ) deallocate(tsk_ice     )
 if(allocated(albsi_p)     ) deallocate(albsi_p     )
 if(allocated(icedepth_p)  ) deallocate(icedepth_p  )
 if(allocated(snowsi_p)    ) deallocate(snowsi_p    )

 if(allocated(tslb_p)) deallocate(tslb_p)

 sf_select: select case(trim(lsm_scheme))
    case("sf_noah")
       if(allocated(noahres_p)) deallocate(noahres_p)
       if(allocated(potevp_p) ) deallocate(potevp_p )
       if(allocated(snopcx_p) ) deallocate(snopcx_p )

    case default
 end select sf_select

 end subroutine deallocate_seaice

!=================================================================================================================
 subroutine seaice_from_MPAS(configs,diag_physics,sfc_input,its,ite)
!=================================================================================================================

!input and inout arguments:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: sfc_input
 integer,intent(in):: its,ite

!local pointers:
 character(len=StrKIND),pointer:: convection_scheme, &
                                  lsm_scheme,        &
                                  microp_scheme

 real(kind=RKIND),dimension(:),pointer:: acsnom,acsnow,br,chs,chs2,cpm,cqs2,qgh,qsfc,glw,gsw,grdflx,hfx, &
                                         qfx,lh,noahres,potevp,sfc_albedo,sfc_emiss,sfcrunoff,snopcx,z0, &
                                         znt,raincv,rainncv,sr
 real(kind=RKIND),dimension(:),pointer:: snoalb,snow,snowc,snowh,skintemp,xice
 real(kind=RKIND),dimension(:,:),pointer:: tslb

!local variables and arrays:
 integer:: i,j,n

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write('--- enter subroutine seaice_from_MPAS:')

 call mpas_pool_get_config(configs,'config_convection_scheme',convection_scheme)
 call mpas_pool_get_config(configs,'config_lsm_scheme'       ,lsm_scheme       )
 call mpas_pool_get_config(configs,'config_microp_scheme'    ,microp_scheme    )

 call mpas_pool_get_array(diag_physics,'acsnom'    ,acsnom    )
 call mpas_pool_get_array(diag_physics,'acsnow'    ,acsnow    )
 call mpas_pool_get_array(diag_physics,'br'        ,br        )
 call mpas_pool_get_array(diag_physics,'chs'       ,chs       )
 call mpas_pool_get_array(diag_physics,'chs2'      ,chs2      )
 call mpas_pool_get_array(diag_physics,'cpm'       ,cpm       )
 call mpas_pool_get_array(diag_physics,'cqs2'      ,cqs2      )
 call mpas_pool_get_array(diag_physics,'qgh'       ,qgh       )
 call mpas_pool_get_array(diag_physics,'qsfc'      ,qsfc      )
 call mpas_pool_get_array(diag_physics,'glw'       ,glw       )
 call mpas_pool_get_array(diag_physics,'gsw'       ,gsw       )
 call mpas_pool_get_array(diag_physics,'grdflx'    ,grdflx    )
 call mpas_pool_get_array(diag_physics,'hfx'       ,hfx       )
 call mpas_pool_get_array(diag_physics,'qfx'       ,qfx       )
 call mpas_pool_get_array(diag_physics,'lh'        ,lh        )
 call mpas_pool_get_array(diag_physics,'sfc_albedo',sfc_albedo)
 call mpas_pool_get_array(diag_physics,'sfc_emiss' ,sfc_emiss )
 call mpas_pool_get_array(diag_physics,'sfcrunoff' ,sfcrunoff )
 call mpas_pool_get_array(diag_physics,'z0'        ,z0        )
 call mpas_pool_get_array(diag_physics,'znt'       ,znt       )

 call mpas_pool_get_array(sfc_input,'snoalb'  ,snoalb  )
 call mpas_pool_get_array(sfc_input,'snow'    ,snow    )
 call mpas_pool_get_array(sfc_input,'snowc'   ,snowc   )
 call mpas_pool_get_array(sfc_input,'snowh'   ,snowh   )
 call mpas_pool_get_array(sfc_input,'skintemp',skintemp)
 call mpas_pool_get_array(sfc_input,'tslb'    ,tslb    )
 call mpas_pool_get_array(sfc_input,'xice'    ,xice    )

 do j = jts,jte
    do i = its,ite
       !--- in variables:
       xice_p(i,j)      = xice(i)
       glw_p(i,j)       = glw(i)
       qgh_p(i,j)       = qgh(i)
       snoalb_p(i,j)    = snoalb(i)
       br_p(i,j)        = br(i)
       chs_p(i,j)       = chs(i)
       swdown_p(i,j)    = gsw(i)/(1._RKIND-sfc_albedo(i))

       !--- inout variables:
       do n = 1,num_soils
          tslb_p(i,n,j) = tslb(n,i)
       enddo
       z0_p(i,j)        = z0(i)
       snow_p(i,j)      = snow(i)
       snowc_p(i,j)     = snowc(i)
       snowh_p(i,j)     = snowh(i)
       tsk_p(i,j)       = skintemp(i)
       cqs2_p(i,j)      = cqs2(i)
       acsnom_p(i,j)    = acsnom(i)
       acsnow_p(i,j)    = acsnow(i)
       sfcrunoff_p(i,j) = sfcrunoff(i)
       albsi_p(i,j)     = seaice_albedo_default
       snowsi_p(i,j)    = seaice_snowdepth_min
       icedepth_p(i,j)  = seaice_thickness_default

       !--- output variables:
       hfx_p(i,j)       = hfx(i)
       lh_p(i,j)        = lh(i)
       qfx_p(i,j)       = qfx(i)
       znt_p(i,j)       = znt(i)
       grdflx_p(i,j)    = grdflx(i)
       qsfc_p(i,j)      = qsfc(i)
       chs2_p(i,j)      = chs2(i)

       !modify the surface albedo and surface emissivity, and surface temperatures over sea-ice points:
       if(xice(i).ge.xice_threshold .and. xice(i).le.1._RKIND) then
          sfc_albedo_p(i,j) = (sfc_albedo(i) - 0.08_RKIND*(1._RKIND-xice(i))) / xice(i)
          sfc_emiss_p(i,j)  = (sfc_emiss(i) - 0.98_RKIND*(1._RKIND-xice(i))) / xice(i)
       else
          sfc_emiss_p(i,j)  = sfc_emiss(i)
          sfc_albedo_p(i,j) = sfc_albedo(i)
       endif
    enddo

    !calculate sea-surface and sea-ice temperatures over sea-ice grid cells:
    call correct_tsk_over_seaice(ims,ime,jms,jme,its,ite,jts,jte,xice_threshold,xice_p, &
                                 tsk_p,tsk_sea,tsk_ice)
    do i = its,ite
       tsk_p(i,j) = tsk_ice(i,j)
    enddo
 enddo

 do j = jts,jte
    do i = its,ite
       sr_p(i,j)     = 0._RKIND
       rainbl_p(i,j) = 0._RKIND
    enddo
    if(microp_scheme .ne. 'off') then
       call mpas_pool_get_array(diag_physics,'sr',sr)
       call mpas_pool_get_array(diag_physics,'rainncv',rainncv)
       do i = its,ite
          sr_p(i,j) = sr(i)
          rainbl_p(i,j) = rainbl_p(i,j) + rainncv(i)
       enddo
    endif
    if(convection_scheme .ne. 'off') then
       call mpas_pool_get_array(diag_physics,'raincv',raincv)
       do i = its,ite
          rainbl_p(i,j) = rainbl_p(i,j) + raincv(i)
       enddo
    endif
 enddo

 sf_select: select case(trim(lsm_scheme))
    case("sf_noah")
       call mpas_pool_get_array(diag_physics,'noahres',noahres)
       call mpas_pool_get_array(diag_physics,'potevp' ,potevp )
       call mpas_pool_get_array(diag_physics,'snopcx' ,snopcx )

       do j = jts,jte
          do i = its,ite
             !--- inout and out optional variables:
             noahres_p(i,j) = noahres(i)
             potevp_p(i,j)  = potevp(i)
             snopcx_p(i,j)  = snopcx(i)
          enddo
       enddo

    case default
 end select sf_select

!call mpas_log_write('--- end subroutine seaice_from_MPAS:')

 end subroutine seaice_from_MPAS

!=================================================================================================================
 subroutine seaice_to_MPAS(configs,diag_physics,sfc_input,its,ite)
!=================================================================================================================

!input and inout arguments:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: sfc_input
 integer,intent(in):: its,ite

!local pointers:
 character(len=StrKIND),pointer:: lsm_scheme

 real(kind=RKIND),dimension(:),pointer:: acsnom,acsnow,chs,chs2,cpm,cqs2,qgh,qsfc,grdflx,hfx, qfx,lh,noahres, &
                                         potevp,sfc_albedo,sfc_emiss,sfcrunoff,snopcx,z0,znt
 real(kind=RKIND),dimension(:),pointer:: snow,snowc,snowh,skintemp,xice
 real(kind=RKIND),dimension(:,:),pointer:: tslb

!local variables and arrays:
 integer:: i,j,n

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write('--- enter subroutine seaice_to_MPAS:')

 call mpas_pool_get_config(configs,'config_lsm_scheme',lsm_scheme)

 call mpas_pool_get_array(diag_physics,'acsnom'    ,acsnom    )
 call mpas_pool_get_array(diag_physics,'acsnow'    ,acsnow    )
 call mpas_pool_get_array(diag_physics,'chs'       ,chs       )
 call mpas_pool_get_array(diag_physics,'chs2'      ,chs2      )
 call mpas_pool_get_array(diag_physics,'cpm'       ,cpm       )
 call mpas_pool_get_array(diag_physics,'cqs2'      ,cqs2      )
 call mpas_pool_get_array(diag_physics,'qgh'       ,qgh       )
 call mpas_pool_get_array(diag_physics,'qsfc'      ,qsfc      )
 call mpas_pool_get_array(diag_physics,'grdflx'    ,grdflx    )
 call mpas_pool_get_array(diag_physics,'hfx'       ,hfx       )
 call mpas_pool_get_array(diag_physics,'qfx'       ,qfx       )
 call mpas_pool_get_array(diag_physics,'lh'        ,lh        )
 call mpas_pool_get_array(diag_physics,'sfc_albedo',sfc_albedo)
 call mpas_pool_get_array(diag_physics,'sfc_emiss' ,sfc_emiss )
 call mpas_pool_get_array(diag_physics,'sfcrunoff' ,sfcrunoff )
 call mpas_pool_get_array(diag_physics,'z0'        ,z0        )
 call mpas_pool_get_array(diag_physics,'znt'       ,znt       )

 call mpas_pool_get_array(sfc_input,'snow'    ,snow    )
 call mpas_pool_get_array(sfc_input,'snowc'   ,snowc   )
 call mpas_pool_get_array(sfc_input,'snowh'   ,snowh   )
 call mpas_pool_get_array(sfc_input,'skintemp',skintemp)
 call mpas_pool_get_array(sfc_input,'tslb'    ,tslb    )
 call mpas_pool_get_array(sfc_input,'xice'    ,xice    )

!--- reconstruct local variables as functions of the seaice fraction:
 do j = jts,jte
    do i = its,ite
       if(xice_p(i,j).ge.xice_threshold .and. xice_p(i,j).le.1._RKIND) then
          cpm(i) = xice_p(i,j)*cpm(i) + (1._RKIND-xice_p(i,j))*cpm_sea(i,j)

          chs_p(i,j)  = xice_p(i,j)*chs_p(i,j)  + (1._RKIND-xice_p(i,j))*chs_sea(i,j)
          chs2_p(i,j) = xice_p(i,j)*chs2_p(i,j) + (1._RKIND-xice_p(i,j))*chs2_sea(i,j)
          cqs2_p(i,j) = xice_p(i,j)*cqs2_p(i,j) + (1._RKIND-xice_p(i,j))*cqs2_sea(i,j)
          hfx_p(i,j)  = xice_p(i,j)*hfx_p(i,j)  + (1._RKIND-xice_p(i,j))*hfx_sea(i,j)
          lh_p(i,j)   = xice_p(i,j)*lh_p(i,j)   + (1._RKIND-xice_p(i,j))*lh_sea(i,j)
          qfx_p(i,j)  = xice_p(i,j)*qfx_p(i,j)  + (1._RKIND-xice_p(i,j))*qfx_sea(i,j)
          qgh_p(i,j)  = xice_p(i,j)*qgh_p(i,j)  + (1._RKIND-xice_p(i,j))*qgh_sea(i,j)
          qsfc_p(i,j) = xice_p(i,j)*qsfc_p(i,j) + (1._RKIND-xice_p(i,j))*qsfc_sea(i,j)
          tsk_p(i,j)  = xice_p(i,j)*tsk_p(i,j)  + (1._RKIND-xice_p(i,j))*tsk_sea(i,j)
          sfc_albedo_p(i,j) = xice_p(i,j)*sfc_albedo_p(i,j) + (1._RKIND-xice_p(i,j))*0.08_RKIND
          sfc_emiss_p(i,j)  = xice_p(i,j)*sfc_emiss_p(i,j)  + (1._RKIND-xice_p(i,j))*0.98_RKIND
       endif
    enddo
 enddo

!--- update all variables:
 do j = jts,jte
    do i = its,ite
       !--- inout variables:
       do n = 1,num_soils
          tslb(n,i) = tslb_p(i,n,j)
       enddo
       z0(i)        = z0_p(i,j)
       snow(i)      = snow_p(i,j)
       snowc(i)     = snowc_p(i,j)
       snowh(i)     = snowh_p(i,j)
       skintemp(i)  = tsk_p(i,j)
       acsnom(i)    = acsnom_p(i,j)
       acsnow(i)    = acsnow_p(i,j)
       sfcrunoff(i) = sfcrunoff_p(i,j)

       !--- output variables:
       znt(i)       = znt_p(i,j)
       grdflx(i)    = grdflx_p(i,j)

       chs(i)  = chs_p(i,j)
       chs2(i) = chs2_p(i,j)
       cqs2(i) = cqs2_p(i,j)
       qsfc(i) = qsfc_p(i,j)
       qgh(i)  = qgh_p(i,j)
       hfx(i)  = hfx_p(i,j)
       qfx(i)  = qfx_p(i,j)
       lh(i)   = lh_p(i,j)
       sfc_albedo(i) = sfc_albedo_p(i,j)
       sfc_emiss(i)  = sfc_emiss_p(i,j)
    enddo
 enddo

 sf_select: select case(trim(lsm_scheme))
    case("sf_noah")
       call mpas_pool_get_array(diag_physics,'noahres',noahres)
       call mpas_pool_get_array(diag_physics,'potevp' ,potevp )
       call mpas_pool_get_array(diag_physics,'snopcx' ,snopcx )

       do j = jts,jte
          do i = its,ite
             !--- inout and out optional variables:
             noahres(i) = noahres_p(i,j)
             potevp(i)  = potevp_p(i,j)
             snopcx(i)  = snopcx_p(i,j)
          enddo
       enddo

    case default
 end select sf_select

!call mpas_log_write('--- end subroutine seaice_to_MPAS:')

 end subroutine seaice_to_MPAS

!=================================================================================================================
 subroutine driver_seaice(configs,diag_physics,sfc_input,its,ite)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs
 integer,intent(in):: its,ite

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: sfc_input

!local pointers:
 character(len=StrKIND),pointer:: lsm_scheme

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write(' ')
!call mpas_log_write('--- enter subroutine driver_seaice: xice_threshold = $r',realArgs=(/xice_threshold/))

 call mpas_pool_get_config(configs,'config_lsm_scheme',lsm_scheme)

!copy MPAS arrays to local arrays:
 call seaice_from_MPAS(configs,diag_physics,sfc_input,its,ite)

 sf_select: select case(trim(lsm_scheme))
    case("sf_noah")
       call seaice_noah( &
                dz8w   = dz_p         , p8w3d     = pres2_hyd_p , t3d      = t_p         , &
                qv3d   = qv_p         , xice      = xice_p      , snoalb2d = snoalb_p    , &
                glw    = glw_p        , swdown    = swdown_p    , rainbl   = rainbl_p    , &
                sr     = sr_p         , qgh       = qgh_p       , tsk      = tsk_p       , &
                hfx    = hfx_p        , qfx       =  qfx_p      , lh       = lh_p        , &
                grdflx = grdflx_p     , qsfc      = qsfc_p      , emiss    = sfc_emiss_p , &
                albedo = sfc_albedo_p , rib       = br_p        , cqs2     = cqs2_p      , &
                chs    = chs_p        , chs2      = chs2_p      , z02d     = z0_p        , &
                znt    = znt_p        , tslb      = tslb_p      , snow     = snow_p      , &
                snowc  = snowc_p      , snowh2d   = snowh_p     , acsnow   = acsnow_p    , &
                acsnom = acsnom_p     , sfcrunoff = sfcrunoff_p , albsi    = albsi_p     , &
                snowsi = snowsi_p     , icedepth  = icedepth_p  , dt       = dt_pbl      , &
                frpcpn = frpcpn       , noahres = noahres_p     , potevp  = potevp_p     , &
                snopcx    = snopcx_p                                                     , &
                seaice_albedo_opt        = seaice_albedo_opt        ,                      &
                seaice_albedo_default    = seaice_albedo_default    ,                      &
                seaice_thickness_opt     = seaice_thickness_opt     ,                      &
                seaice_thickness_default = seaice_thickness_default ,                      &
                seaice_snowdepth_opt     = seaice_snowdepth_opt     ,                      &
                seaice_snowdepth_max     = seaice_snowdepth_max     ,                      &
                seaice_snowdepth_min     = seaice_snowdepth_min     ,                      &
                xice_threshold           = xice_threshold           ,                      &
                num_soil_layers          = num_soils                ,                      &
                sf_urban_physics         = sf_urban_physics         ,                      &
                ids = ids , ide = ide , jds = jds , jde = jde , kds = kds , kde = kde ,    &
                ims = ims , ime = ime , jms = jms , jme = jme , kms = kms , kme = kme ,    &
                its = its , ite = ite , jts = jts , jte = jte , kts = kts , kte = kte      &
                       )


    case("sf_noahmp")
       call seaice_noah( &
                dz8w   = dz_p         , p8w3d     = pres2_hyd_p , t3d      = t_p         , &
                qv3d   = qv_p         , xice      = xice_p      , snoalb2d = snoalb_p    , &
                glw    = glw_p        , swdown    = swdown_p    , rainbl   = rainbl_p    , &
                sr     = sr_p         , qgh       = qgh_p       , tsk      = tsk_p       , &
                hfx    = hfx_p        , qfx       =  qfx_p      , lh       = lh_p        , &
                grdflx = grdflx_p     , qsfc      = qsfc_p      , emiss    = sfc_emiss_p , &
                albedo = sfc_albedo_p , rib       = br_p        , cqs2     = cqs2_p      , &
                chs    = chs_p        , chs2      = chs2_p      , z02d     = z0_p        , &
                znt    = znt_p        , tslb      = tslb_p      , snow     = snow_p      , &
                snowc  = snowc_p      , snowh2d   = snowh_p     , acsnow   = acsnow_p    , &
                acsnom = acsnom_p     , sfcrunoff = sfcrunoff_p , albsi    = albsi_p     , &
                snowsi = snowsi_p     , icedepth  = icedepth_p  , dt       = dt_pbl      , &
                frpcpn = frpcpn       ,                                                    &
                seaice_albedo_opt        = seaice_albedo_opt        ,                      &
                seaice_albedo_default    = seaice_albedo_default    ,                      &
                seaice_thickness_opt     = seaice_thickness_opt     ,                      &
                seaice_thickness_default = seaice_thickness_default ,                      &
                seaice_snowdepth_opt     = seaice_snowdepth_opt     ,                      &
                seaice_snowdepth_max     = seaice_snowdepth_max     ,                      &
                seaice_snowdepth_min     = seaice_snowdepth_min     ,                      &
                xice_threshold           = xice_threshold           ,                      &
                num_soil_layers          = num_soils                ,                      &
                sf_urban_physics         = sf_urban_physics         ,                      &
                ids = ids , ide = ide , jds = jds , jde = jde , kds = kds , kde = kde ,    &
                ims = ims , ime = ime , jms = jms , jme = jme , kms = kms , kme = kme ,    &
                its = its , ite = ite , jts = jts , jte = jte , kts = kts , kte = kte      &
                       )

    case default
 end select sf_select

!copy local arrays to MPAS grid:
 call seaice_to_MPAS(configs,diag_physics,sfc_input,its,ite)

!call mpas_log_write('--- end subroutine driver_seaice:')

 end subroutine driver_seaice

!=================================================================================================================
 end module mpas_atmphys_driver_seaice
!=================================================================================================================
