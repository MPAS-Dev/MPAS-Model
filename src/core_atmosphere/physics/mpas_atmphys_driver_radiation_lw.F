! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module mpas_atmphys_driver_radiation_lw
 use mpas_kind_types
 use mpas_pool_routines
 use mpas_timer, only : mpas_timer_start, mpas_timer_stop

 use mpas_atmphys_driver_radiation_sw, only: radconst
 use mpas_atmphys_constants
 use mpas_atmphys_manager, only: gmt,curr_julday,julday,year
 use mpas_atmphys_rrtmg_lwinit
 use mpas_atmphys_vars

!wrf physics:
 use module_ra_rrtmg_lw
 use module_ra_rrtmg_vinterp

 implicit none
 private
 public:: allocate_radiation_lw,   &
          deallocate_radiation_lw, &
          driver_radiation_lw,     &
          init_radiation_lw


!MPAS driver for parameterization of longwave radiation codes.
!Laura D. Fowler (send comments to laura@ucar.edu).
!2013-05-01.
!
!
! subroutines in mpas_atmphys_driver_radiation_lw:
! ------------------------------------------------
! allocate_radiation_lw  : allocate local arrays for parameterization of lw radiation codes.
! deallocate_radiation_lw: deallocate local arrays for parameterization of lw radiation codes.
! init_radiation_lw      : initialization of individual lw radiation codes.
! driver_radiation_lw    : main driver (called from subroutine physics_driver).
! radiation_lw_from_MPAS : initialize local arrays.
! radiation_lw_to_MPAS   : copy local arrays to MPAS arrays.
!
! WRF physics called from driver_radiation_lw:
! --------------------------------------------
! * module_ra_rrtmg_lw   : RRTMG long wave radiation code.
!
! add-ons and modifications to sourcecode:
! ----------------------------------------
! * removed the pre-processor option "do_hydrostatic_pressure" before call to subroutines
!   rrtmg_lw and camrad.
!   Laura D. Fowler (birch.mmm,ucar.edu) / 2013-05-29.
! * added structure diag in the call to subroutine init_radiation_lw.
!   Laura D. Fowler (laura@ucar.edu) / 2013-07-01.
! * modified the call to subroutine rrtmg_lwrad to include the option of using the same ozone
!   climatology as the one used in the CAM radiation codes.
!   Laura D. Fowler (laura@ucar.edu) / 2013-07-17.
! * in call to subroutine rrtmg_lwrad, replaced the variable g (that originally pointed to 
!   gravity) with gravity, for simplicity.
!   Laura D. Fowler (laura@ucar.edu) / 2014-03-21.
! * throughout the sourcecode, replaced all "var_struct" defined arrays by local pointers.
!   Laura D. Fowler (laura@ucar.edu) / 2014-04-22.
! * modified sourcecode to use pools.
!   Laura D. Fowler (laura@ucar.edu) / 2014-05-15.
! * cleaned-up the call to rrtmg_lwrad after cleaning up subroutine rrtmg_lwrad in module_ra_rrtmg_lw.F.
!   Laura D. Fowler (laura@ucar.edu) / 2016-06-30.
! * added the cloud radii for cloud water, cloud ice, and snow calculated in the Thompson cloud microphysics
!   scheme in the call to subroutine rrtmg_lwrad.
!   Laura D. Fowler (laura@ucar.edu) / 2016-07-07.
! * added diagnostics of the effective radii for cloud water, cloud ice, and snow used in rrtmg_lwrad.
!   Laura D. Fowler (laura@ucar.edu) / 2016-07-08.
! * removed qr_p, and qg_p in the call to rrtmg_lwrad since not used in the calculation of the cloud optical
!   properties.
!   Laura D. Fowler (laura@ucar.edu) / 2016-07-08.
! * in the call to rrtmg_lwrad, substituted the variables qv_p, qc_p, qi_p, and qs_p with qvrad_p, qcrad_p,
!   qirad_p, and qsrad_p initialized in subroutine cloudiness_from_MPAS.
!   Laura D. Fowler (laura@ucar.edu) / 2016-07-09.
! * substituted "use mpas_atmphys_o3climatology" with "use module_ra_rrtmg_vinterp" since we moved subroutine
!   vinterp_ozn to is own module in physics_wrf.
!   laura D. Fowler (laura@ucar.edu) / 2017-01-27.
! * in subroutines radiation_lw_from_MPAS and radiation_lw_to_MPAS, revised the initialization of re_cloud,
!   re_ice, re_snow, and rre_cloud, rre_ice, and rre_snow to handle the case when the cloud microphysics
!   parameterization is turned off, i.e. config_microp_scheme='off'.
!   Laura D. Fowler (laura@ucar.edu) / 2017-02-10.
! * since we removed the local variable radt_lw_scheme from mpas_atmphys_vars.F, now defines radt_lw_scheme
!   as a pointer to config_radt_lw_scheme.
!   Laura D. Fowler (laura@ucar.edu) / 2917-02-16.


 contains


!=================================================================================================================
 subroutine allocate_radiation_lw(configs,xtime_s)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs
 real(kind=RKIND),intent(in):: xtime_s

!local pointers:
 character(len=StrKIND),pointer:: radt_lw_scheme

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_radt_lw_scheme',radt_lw_scheme)

 if(.not.allocated(f_ice)        ) allocate(f_ice(ims:ime,kms:kme,jms:jme)        )
 if(.not.allocated(f_rain)       ) allocate(f_rain(ims:ime,kms:kme,jms:jme)       )

 if(.not.allocated(sfc_emiss_p)  ) allocate(sfc_emiss_p(ims:ime,jms:jme)          )
 if(.not.allocated(snow_p)       ) allocate(snow_p(ims:ime,jms:jme)               )
 if(.not.allocated(tsk_p)        ) allocate(tsk_p(ims:ime,jms:jme)                )
 if(.not.allocated(xice_p)       ) allocate(xice_p(ims:ime,jms:jme)               ) 
 if(.not.allocated(xland_p)      ) allocate(xland_p(ims:ime,jms:jme)              ) 

 if(.not.allocated(glw_p)        ) allocate(glw_p(ims:ime,jms:jme)                )
 if(.not.allocated(lwcf_p)       ) allocate(lwcf_p(ims:ime,jms:jme)               )
 if(.not.allocated(lwdnb_p)      ) allocate(lwdnb_p(ims:ime,jms:jme)              )
 if(.not.allocated(lwdnbc_p)     ) allocate(lwdnbc_p(ims:ime,jms:jme)             )
 if(.not.allocated(lwdnt_p)      ) allocate(lwdnt_p(ims:ime,jms:jme)              )
 if(.not.allocated(lwdntc_p)     ) allocate(lwdntc_p(ims:ime,jms:jme)             )
 if(.not.allocated(lwupb_p)      ) allocate(lwupb_p(ims:ime,jms:jme)              )
 if(.not.allocated(lwupbc_p)     ) allocate(lwupbc_p(ims:ime,jms:jme)             )
 if(.not.allocated(lwupt_p)      ) allocate(lwupt_p(ims:ime,jms:jme)              )
 if(.not.allocated(lwuptc_p)     ) allocate(lwuptc_p(ims:ime,jms:jme)             )
 if(.not.allocated(olrtoa_p)     ) allocate(olrtoa_p(ims:ime,jms:jme)             )

 if(.not.allocated(rthratenlw_p) ) allocate(rthratenlw_p(ims:ime,kms:kme,jms:jme) )

 radiation_lw_select: select case (trim(radt_lw_scheme))
    case("rrtmg_lw")

       if(.not.allocated(recloud_p)    ) allocate(recloud_p(ims:ime,kms:kme,jms:jme)       )
       if(.not.allocated(reice_p)      ) allocate(reice_p(ims:ime,kms:kme,jms:jme)         )
       if(.not.allocated(resnow_p)     ) allocate(resnow_p(ims:ime,kms:kme,jms:jme)        )
       if(.not.allocated(rrecloud_p)   ) allocate(rrecloud_p(ims:ime,kms:kme,jms:jme)      )
       if(.not.allocated(rreice_p)     ) allocate(rreice_p(ims:ime,kms:kme,jms:jme)        )
       if(.not.allocated(rresnow_p)    ) allocate(rresnow_p(ims:ime,kms:kme,jms:jme)       )

       if(.not.allocated(lwdnflx_p)    ) allocate(lwdnflx_p(ims:ime,kms:kme+1,jms:jme)     )
       if(.not.allocated(lwdnflxc_p)   ) allocate(lwdnflxc_p(ims:ime,kms:kme+1,jms:jme)    )

       if(.not.allocated(lwdnflx_p)    ) allocate(lwdnflx_p(ims:ime,kms:kme+1,jms:jme)     )
       if(.not.allocated(lwdnflxc_p)   ) allocate(lwdnflxc_p(ims:ime,kms:kme+1,jms:jme)    )
       if(.not.allocated(lwupflx_p)    ) allocate(lwupflx_p(ims:ime,kms:kme+1,jms:jme)     )
       if(.not.allocated(lwupflxc_p)   ) allocate(lwupflxc_p(ims:ime,kms:kme+1,jms:jme)    )

       if(.not.allocated(pin_p)        ) allocate(pin_p(num_oznlevels)                     )
       if(.not.allocated(o3clim_p)     ) allocate(o3clim_p(ims:ime,1:num_oznlevels,jms:jme))

    case default
 end select radiation_lw_select

 end subroutine allocate_radiation_lw

!=================================================================================================================
 subroutine deallocate_radiation_lw(configs)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs

!local pointers:
 character(len=StrKIND),pointer:: radt_lw_scheme

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_radt_lw_scheme',radt_lw_scheme)

 if(allocated(f_ice)        ) deallocate(f_ice        )
 if(allocated(f_rain)       ) deallocate(f_rain       )
 if(allocated(sfc_emiss_p)  ) deallocate(sfc_emiss_p  )
 if(allocated(snow_p)       ) deallocate(snow_p       )
 if(allocated(snow_p)       ) deallocate(snow_p       )
 if(allocated(tsk_p)        ) deallocate(tsk_p        )
 if(allocated(xland_p)      ) deallocate(xland_p      )
 if(allocated(glw_p)        ) deallocate(glw_p        )
 if(allocated(lwcf_p)       ) deallocate(lwcf_p       )
 if(allocated(lwdnb_p)      ) deallocate(lwdnb_p      )
 if(allocated(lwdnbc_p)     ) deallocate(lwdnbc_p     )
 if(allocated(lwdnt_p)      ) deallocate(lwdnt_p      )
 if(allocated(lwdntc_p)     ) deallocate(lwdntc_p     )
 if(allocated(lwupb_p)      ) deallocate(lwupb_p      )
 if(allocated(lwupbc_p)     ) deallocate(lwupbc_p     )
 if(allocated(lwupt_p)      ) deallocate(lwupt_p      )
 if(allocated(lwuptc_p)     ) deallocate(lwuptc_p     )
 if(allocated(olrtoa_p)     ) deallocate(olrtoa_p     )
 
 if(allocated(rthratenlw_p) ) deallocate(rthratenlw_p )

 radiation_lw_select: select case (trim(radt_lw_scheme))
    case("rrtmg_lw")
       if(allocated(recloud_p)    ) deallocate(recloud_p    )
       if(allocated(reice_p)      ) deallocate(reice_p      )
       if(allocated(resnow_p)     ) deallocate(resnow_p     )
       if(allocated(rrecloud_p)   ) deallocate(rrecloud_p   )
       if(allocated(rreice_p)     ) deallocate(rreice_p     )
       if(allocated(rresnow_p)    ) deallocate(rresnow_p    )

       if(allocated(lwdnflx_p)    ) deallocate(lwdnflx_p    )
       if(allocated(lwdnflxc_p)   ) deallocate(lwdnflxc_p   )
       if(allocated(lwupflx_p)    ) deallocate(lwupflx_p    )
       if(allocated(lwupflxc_p)   ) deallocate(lwupflxc_p   )

       if(allocated(pin_p)        ) deallocate(pin_p        )
       if(allocated(o3clim_p)     ) deallocate(o3clim_p     )

    case default
 end select radiation_lw_select

 end subroutine deallocate_radiation_lw

!=================================================================================================================
 subroutine radiation_lw_from_MPAS(xtime_s,configs,mesh,state,time_lev,diag_physics,atm_input, &
                                   sfc_input,its,ite)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: state
 type(mpas_pool_type),intent(in):: atm_input
 type(mpas_pool_type),intent(in):: sfc_input

 integer,intent(in):: its,ite
 integer,intent(in):: time_lev


 real(kind=RKIND),intent(in):: xtime_s

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics

!local pointers:
 logical,pointer:: config_o3climatology
 logical,pointer:: config_microp_re
 character(len=StrKIND),pointer:: radt_lw_scheme
 character(len=StrKIND),pointer:: microp_scheme

 real(kind=RKIND),dimension(:),pointer    :: latCell,lonCell
 real(kind=RKIND),dimension(:),pointer    :: skintemp,snow,xice,xland
 real(kind=RKIND),dimension(:),pointer    :: pin
 real(kind=RKIND),dimension(:),pointer    :: sfc_albedo,sfc_emiss
 real(kind=RKIND),dimension(:,:),pointer  :: cldfrac,o3clim,o3vmr
 real(kind=RKIND),dimension(:,:),pointer  :: re_cloud,re_ice,re_snow
 real(kind=RKIND),dimension(:,:,:),pointer:: ozmixm

!local variables and arrays:
 integer:: ncols,nlevs
 integer:: i,j,k,n
 real(kind=RKIND),dimension(:,:),allocatable:: p2d,o32d

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_microp_re'     ,config_microp_re    )
 call mpas_pool_get_config(configs,'config_o3climatology' ,config_o3climatology)
 call mpas_pool_get_config(configs,'config_radt_lw_scheme',radt_lw_scheme      )
 call mpas_pool_get_config(configs,'config_microp_scheme' ,microp_scheme       )

 call mpas_pool_get_array(mesh,'latCell',latCell)
 call mpas_pool_get_array(mesh,'lonCell',lonCell)

 call mpas_pool_get_array(sfc_input,'skintemp',skintemp)
 call mpas_pool_get_array(sfc_input,'snow'    ,snow    )
 call mpas_pool_get_array(sfc_input,'xice'    ,xice    )
 call mpas_pool_get_array(sfc_input,'xland'   ,xland   )

 call mpas_pool_get_array(atm_input,'pin'     ,pin     )
 call mpas_pool_get_array(atm_input,'ozmixm'  ,ozmixm  )

 call mpas_pool_get_array(diag_physics,'sfc_albedo',sfc_albedo)
 call mpas_pool_get_array(diag_physics,'sfc_emiss' ,sfc_emiss )
 call mpas_pool_get_array(diag_physics,'cldfrac'   ,cldfrac   )
 call mpas_pool_get_array(diag_physics,'o3clim'    ,o3clim    )
 call mpas_pool_get_array(diag_physics,'o3vmr'     ,o3vmr     )
 call mpas_pool_get_array(diag_physics,'m_hybi'    ,m_hybi    )
 call mpas_pool_get_array(diag_physics,'m_ps'      ,m_ps      )

 do j = jts,jte
 do i = its,ite
    sfc_emiss_p(i,j) = sfc_emiss(i)
    tsk_p(i,j)       = skintemp(i)
    snow_p(i,j)      = snow(i)
    xice_p(i,j)      = xice(i)
    xland_p(i,j)     = xland(i)
 enddo
 enddo
 do j = jts,jte
 do k = kts,kte
 do i = its,ite
    cldfrac_p(i,k,j) = cldfrac(k,i)
 enddo
 enddo
 enddo

!initialization:
 do j = jts,jte
 do k = kts,kte
 do i = its,ite
    f_ice(i,k,j)  = 0.0_RKIND
    f_rain(i,k,j) = 0.0_RKIND
 enddo
 enddo
 enddo

 do j = jts,jte
 do i = its,ite
    glw_p(i,j)      = 0.0_RKIND
    lwcf_p(i,j)     = 0.0_RKIND
    lwdnb_p(i,j)    = 0.0_RKIND
    lwdnbc_p(i,j)   = 0.0_RKIND
    lwdnt_p(i,j)    = 0.0_RKIND
    lwdntc_p(i,j)   = 0.0_RKIND
    lwupb_p(i,j)    = 0.0_RKIND
    lwupbc_p(i,j)   = 0.0_RKIND
    lwupt_p(i,j)    = 0.0_RKIND
    lwuptc_p(i,j)   = 0.0_RKIND
    olrtoa_p(i,j)   = 0.0_RKIND
 enddo
 
 do k = kts,kte
 do i = its,ite
    rthratenlw_p(i,k,j) = 0.0_RKIND
 enddo
 enddo
 enddo

 radiation_lw_select: select case (trim(radt_lw_scheme))
    case("rrtmg_lw")
       microp_select: select case(microp_scheme)
          case("mp_thompson","mp_wsm6")
             if(config_microp_re) then
                call mpas_pool_get_array(diag_physics,'re_cloud',re_cloud)
                call mpas_pool_get_array(diag_physics,'re_ice'  ,re_ice  )
                call mpas_pool_get_array(diag_physics,'re_snow' ,re_snow )

                do j = jts,jte
                do k = kts,kte
                do i = its,ite
                   recloud_p(i,k,j) = re_cloud(k,i)
                   reice_p(i,k,j)   = re_ice(k,i)
                   resnow_p(i,k,j)  = re_snow(k,i)
                enddo
                enddo
                enddo
             else
                ! These are set in module mpas_atmphys_manager and should not be set again
                !has_reqc = 0
                !has_reqi = 0
                !has_reqs = 0
                do j = jts,jte
                do k = kts,kte
                do i = its,ite
                   recloud_p(i,k,j) = 0._RKIND
                   reice_p(i,k,j)   = 0._RKIND
                   resnow_p(i,k,j)  = 0._RKIND
                enddo
                enddo
                enddo
             endif
             do j = jts,jte
             do k = kts,kte
             do i = its,ite
                rrecloud_p(i,k,j) = 0._RKIND
                rreice_p(i,k,j)   = 0._RKIND
                rresnow_p(i,k,j)  = 0._RKIND
             enddo
             enddo
             enddo

          case default
       end select microp_select

       do j = jts,jte
       do k = kts,kte+2
       do i = its,ite
          lwdnflx_p(i,k,j)  = 0.0_RKIND
          lwdnflxc_p(i,k,j) = 0.0_RKIND
          lwupflx_p(i,k,j)  = 0.0_RKIND
          lwupflxc_p(i,k,j) = 0.0_RKIND
       enddo
       enddo
       enddo

       if(config_o3climatology) then
          !ozone mixing ratio:
          do k = 1, num_oznLevels
             pin_p(k) = pin(k)
          enddo
          do j = jts,jte
          do k = 1, num_oznLevels
             do i = its,ite
                o3clim_p(i,k,j) = o3clim(k,i)
             enddo
          enddo
          enddo

          nlevs = kte-kts+1
          ncols = ite-its+1
          if(.not.allocated(p2d) ) allocate(p2d(its:ite,kts:kte) )
          if(.not.allocated(o32d)) allocate(o32d(its:ite,kts:kte))
          do j = jts,jte
             do i = its,ite
             do k = kts,kte
                o32d(i,k) = 0._RKIND
                p2d(i,k)  = pres_hyd_p(i,k,j) / 100._RKIND
             enddo
             enddo
             call vinterp_ozn(1,ncols,ncols,nlevs,p2d,pin_p,num_oznlevels,o3clim_p(1,1,j),o32d)
             do i = its,ite
             do k = kts,kte
                o3vmr(k,i) = o32d(i,k)
             enddo
             enddo
          enddo
          if(allocated(p2d))  deallocate(p2d)
          if(allocated(o32d)) deallocate(o32d)
       else
          do k = 1, num_oznLevels
             pin_p(k) = 0.0_RKIND
          enddo
          do j = jts,jte
          do k = 1, num_oznLevels
             do i = its,ite
                o3clim_p(i,k,j) = 0.0_RKIND
             enddo
          enddo
          enddo
       endif

    case default
 end select radiation_lw_select

 end subroutine radiation_lw_from_MPAS

!=================================================================================================================
 subroutine radiation_lw_to_MPAS(configs,diag_physics,tend_physics,its,ite)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: tend_physics

 integer,intent(in):: its,ite

!local pointers:
 logical,pointer:: config_microp_re
 character(len=StrKIND),pointer:: radt_lw_scheme
 character(len=StrKIND),pointer:: microp_scheme

 real(kind=RKIND),dimension(:),pointer :: glw,lwcf,lwdnb,lwdnbc,lwdnt,lwdntc,lwupb,lwupbc, &
                                          lwupt,lwuptc,olrtoa
 real(kind=RKIND),dimension(:,:),pointer:: rthratenlw
 real(kind=RKIND),dimension(:,:),pointer:: rre_cloud,rre_ice,rre_snow

!local variables and arrays:
 integer:: nlay,pcols
 integer:: i,j,k
 real(kind=RKIND),dimension(:,:),allocatable:: p1d

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_microp_re',config_microp_re)
 call mpas_pool_get_config(configs,'config_radt_lw_scheme',radt_lw_scheme)
 call mpas_pool_get_config(configs,'config_microp_scheme',microp_scheme)

 call mpas_pool_get_array(diag_physics,'glw'   ,glw   )
 call mpas_pool_get_array(diag_physics,'lwcf'  ,lwcf  )
 call mpas_pool_get_array(diag_physics,'lwdnb' ,lwdnb )
 call mpas_pool_get_array(diag_physics,'lwdnbc',lwdnbc)
 call mpas_pool_get_array(diag_physics,'lwdnt' ,lwdnt )
 call mpas_pool_get_array(diag_physics,'lwdntc',lwdntc)
 call mpas_pool_get_array(diag_physics,'lwupb' ,lwupb )
 call mpas_pool_get_array(diag_physics,'lwupbc',lwupbc)
 call mpas_pool_get_array(diag_physics,'lwupt' ,lwupt )
 call mpas_pool_get_array(diag_physics,'lwuptc',lwuptc)
 call mpas_pool_get_array(diag_physics,'olrtoa',olrtoa)

 call mpas_pool_get_array(tend_physics,'rthratenlw',rthratenlw)

 do j = jts,jte
 do i = its,ite
    glw(i)    = glw_p(i,j)
    lwcf(i)   = lwcf_p(i,j)
    lwdnb(i)  = lwdnb_p(i,j)
    lwdnbc(i) = lwdnbc_p(i,j)
    lwdnt(i)  = lwdnt_p(i,j)
    lwdntc(i) = lwdntc_p(i,j)
    lwupb(i)  = lwupb_p(i,j)
    lwupbc(i) = lwupbc_p(i,j)
    lwupt(i)  = lwupt_p(i,j)
    lwuptc(i) = lwuptc_p(i,j)
    olrtoa(i) = olrtoa_p(i,j)
 enddo

 do k = kts,kte
 do i = its,ite
    rthratenlw(k,i) = rthratenlw_p(i,k,j)
 enddo
 enddo
 enddo

 radiation_lw_select: select case (trim(radt_lw_scheme))

    case("rrtmg_lw")

       microp_select: select case(microp_scheme)
          case("mp_thompson","mp_wsm6")
             call mpas_pool_get_array(diag_physics,'rre_cloud',rre_cloud)
             call mpas_pool_get_array(diag_physics,'rre_ice'  ,rre_ice  )
             call mpas_pool_get_array(diag_physics,'rre_snow' ,rre_snow )
             if(config_microp_re) then
                do j = jts,jte
                do k = kts,kte
                do i = its,ite
                   rre_cloud(k,i) = rrecloud_p(i,k,j)
                   rre_ice(k,i)   = rreice_p(i,k,j)
                   rre_snow(k,i)  = rresnow_p(i,k,j)
                enddo
                enddo
                enddo
             else
                do j = jts,jte
                do k = kts,kte
                do i = its,ite
                   rre_cloud(k,i) = 0._RKIND
                   rre_ice(k,i)   = 0._RKIND
                   rre_snow(k,i)  = 0._RKIND
                enddo
                enddo
                enddo
             endif

          case default
       end select microp_select

 end select radiation_lw_select

 end subroutine radiation_lw_to_MPAS

!=================================================================================================================
 subroutine init_radiation_lw(dminfo,configs,mesh,atm_input,diag,state,time_lev)
!=================================================================================================================

!input arguments:
 type(dm_info),intent(in):: dminfo
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in),optional:: mesh
 type(mpas_pool_type),intent(in),optional:: diag
 type(mpas_pool_type),intent(in),optional:: diag_physics

 integer,intent(in),optional:: time_lev

!inout arguments:
 type(mpas_pool_type),intent(inout),optional:: atm_input
 type(mpas_pool_type),intent(inout),optional:: state

!local pointers:
 character(len=StrKIND),pointer:: radt_lw_scheme

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_radt_lw_scheme',radt_lw_scheme)

 radiation_lw_select: select case (trim(radt_lw_scheme))
    case ("rrtmg_lw")
       call rrtmg_initlw_forMPAS(dminfo)
    
    case default
 end select radiation_lw_select

 end subroutine init_radiation_lw

!=================================================================================================================
 subroutine driver_radiation_lw(xtime_s,configs,mesh,state,time_lev,diag_physics,atm_input, &
                                sfc_input,tend_physics,its,ite)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: configs
 integer,intent(in):: its,ite

 integer,intent(in):: time_lev
 real(kind=RKIND),intent(in):: xtime_s

!inout arguments:
 type(mpas_pool_type),intent(inout):: state
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: atm_input
 type(mpas_pool_type),intent(inout):: sfc_input
 type(mpas_pool_type),intent(inout):: tend_physics

!local pointers:
 logical,pointer:: config_o3climatology
 character(len=StrKIND),pointer:: radt_lw_scheme

!local variables:
 integer:: o3input
 real(kind=RKIND):: radt,xtime_m

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write(' --- enter subroutine driver_radiation_lw: ')

 call mpas_pool_get_config(configs,'config_o3climatology' ,config_o3climatology)
 call mpas_pool_get_config(configs,'config_radt_lw_scheme',radt_lw_scheme      )

!copy MPAS arrays to local arrays:
 call radiation_lw_from_MPAS(xtime_s,configs,mesh,state,time_lev,diag_physics,atm_input,sfc_input,its,ite)

!call to longwave radiation scheme:
 radiation_lw_select: select case (trim(radt_lw_scheme))
    case ("rrtmg_lw")
       o3input = 0
       if(config_o3climatology) o3input = 2

       call mpas_timer_start('RRTMG_lw')
       call rrtmg_lwrad( &
            p3d        = pres_hyd_p    , p8w       = pres2_hyd_p , pi3d     = pi_p     , &
            t3d        = t_p           , t8w       = t2_p        , dz8w     = dz_p     , &
!           qv3d       = qv_p          , qc3d      = qc_p        , qi3d     = qi_p     , &
!           qs3d       = qs_p          , cldfra3d  = cldfrac_p   , tsk      = tsk_p    , &
            qv3d       = qvrad_p       , qc3d      = qcrad_p     , qi3d     = qirad_p  , &
            qs3d       = qsrad_p       , cldfra3d  = cldfrac_p   , tsk      = tsk_p    , &
            emiss      = sfc_emiss_p   , xland     = xland_p     , xice     = xice_p   , &
            snow       = snow_p        , icloud    = icloud      , o3input  = o3input  , &
            noznlevels = num_oznlevels , pin       = pin_p       , o3clim   = o3clim_p , &
            glw        = glw_p         , olr       = olrtoa_p    , lwcf     = lwcf_p   , &
            rthratenlw = rthratenlw_p  , has_reqc  = has_reqc    , has_reqi = has_reqi , &
            has_reqs   = has_reqs      , re_cloud  = recloud_p   , re_ice   = reice_p  , &
            re_snow    = resnow_p      , rre_cloud = rrecloud_p  , rre_ice  = rreice_p , &
            rre_snow   = rresnow_p     , lwupt     = lwupt_p     , lwuptc   = lwuptc_p , &
            lwdnt      = lwdnt_p       , lwdntc    = lwdntc_p    , lwupb    = lwupb_p  , &
            lwupbc     = lwupbc_p      , lwdnb     = lwdnb_p     , lwdnbc   = lwdnbc_p , &
            ids = ids , ide = ide , jds = jds , jde = jde , kds = kds , kde = kde ,      &
            ims = ims , ime = ime , jms = jms , jme = jme , kms = kms , kme = kme ,      &
            its = its , ite = ite , jts = jts , jte = jte , kts = kts , kte = kte        &
                       )
       call mpas_timer_stop('RRTMG_lw')

    case default
 end select radiation_lw_select

!copy local arrays to MPAS grid:
 call radiation_lw_to_MPAS(configs,diag_physics,tend_physics,its,ite)

!call mpas_log_write('--- end subroutine driver_radiation_lw.')

 end subroutine driver_radiation_lw

!=================================================================================================================
 end module mpas_atmphys_driver_radiation_lw
!=================================================================================================================
