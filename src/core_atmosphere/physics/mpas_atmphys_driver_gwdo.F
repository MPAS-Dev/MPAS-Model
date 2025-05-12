! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module mpas_atmphys_driver_gwdo
 use mpas_kind_types
 use mpas_pool_routines
 use mpas_timer,only: mpas_timer_start,mpas_timer_stop

 use mpas_atmphys_constants
 use mpas_atmphys_vars
 use mpas_atmphys_manager,only: curr_julday

!wrf physics:
 use module_bl_gwdo
 use module_bl_ugwp_gwdo

 implicit none
 private
 public:: allocate_gwdo,   &
          deallocate_gwdo, &
          driver_gwdo


!MPAS driver for parameterization of gravity wave drag over orography.
!Laura D. Fowler (send comments to laura@ucar.edu).
!2013-05-01.
!
! subroutines in mpas_atmphys_driver_gwdo:
! ----------------------------------------
! allocate_gwdo  : allocate local arrays for parameterization of gravity wave drag.
! deallocate_gwdo: deallocate local arrays for parameterization of gravity wave drag.
! driver_gwdo    : main driver (called from subroutine physics_driver).
! gwdo_from_MPAS : initialize local arrays.
! gwdo_to_MPAS   : copy local arrays to MPAS arrays.
!
! WRF physics called from driver_gwdo:
! --------------------------- --------
! * module_bl_gwdo       : parameterization of gravity wave drag over orography. 
!
! add-ons and modifications to sourcecode:
! ----------------------------------------
! * removed the pre-processor option "do_hydrostatic_pressure" before call to subroutine gwdo. 
!   Laura D. Fowler (birch.ucar.edu) / 2013-05-29.
! * changed the definition of dx_p to the mean distance between cell centers.
!   Laura D. Fowler (laura@ucar.edu) / 2013-08-23.
! * in call to subroutine gwdo, replaced the variable g (that originally pointed to gravity)
!   with gravity, for simplicity.
!   Laura D. Fowler (laura@ucar.edu) / 2014-03-21.
! * throughout the sourcecode, replaced all "var_struct" defined arrays by local pointers.
!   Laura D. Fowler (laura@ucar.edu) / 2014-04-22.
! * modified sourcecode to use pools.
!   Laura D. Fowler (laura@ucar.edu) / 2014-05-15.
! * renamed "ysu_gwdo" to "bl_gwdo_ysu".
!   Laura D. Fowler (laura@ucar.edu) / 2016-03-25. 
! * change the definition of dx_p to match that used in other physics parameterizations.
!   Laura D. Fowler (laura@ucar.edu) / 2016-10-18.
! * modified the call to subroutine gwdo following the update of module_gwdo.F to that
!   of WRF version 4.0.2.
!   Laura D. Fowler (laura@ucar.edu) / 2019-01-30.
! * added the flags errmsg and errflg in the call to subroutine gwdo for compliance with the CCPP framework.
!   Laura D. Fowler (laura@ucar.edu) / 2023-05-15.
! * added the NOAA UFS unified gravity wave drag scheme
!   Michael D. Toy (michael.toy@noaa.gov) / 2024-10-21


 contains


!=================================================================================================================
 subroutine allocate_gwdo(configs)
!=================================================================================================================

 !input arguments:
 type(mpas_pool_type),intent(in):: configs

 !local variables:
 character(len=StrKIND),pointer:: gwdo_scheme
 logical,pointer:: ugwp_diags,ngw_scheme

 call mpas_pool_get_config(configs,'config_gwdo_scheme',gwdo_scheme)
 call mpas_pool_get_config(configs,'config_ugwp_diags',ugwp_diags)
 call mpas_pool_get_config(configs,'config_ngw_scheme',ngw_scheme)

 if(.not.allocated(cosa_p)  ) allocate(cosa_p(ims:ime,jms:jme)  )
 if(.not.allocated(sina_p)  ) allocate(sina_p(ims:ime,jms:jme)  )

 if(.not.allocated(dx_p)    ) allocate(dx_p(ims:ime,jms:jme)    )
 if(.not.allocated(kpbl_p  )) allocate(kpbl_p(ims:ime,jms:jme)  )
 if(.not.allocated(dusfcg_p)) allocate(dusfcg_p(ims:ime,jms:jme))
 if(.not.allocated(dvsfcg_p)) allocate(dvsfcg_p(ims:ime,jms:jme))
 if(.not.allocated(dtaux3d_p)) allocate(dtaux3d_p(ims:ime,kms:kme,jms:jme))
 if(.not.allocated(dtauy3d_p)) allocate(dtauy3d_p(ims:ime,kms:kme,jms:jme))
 if(.not.allocated(rublten_p)) allocate(rublten_p(ims:ime,kms:kme,jms:jme))
 if(.not.allocated(rvblten_p)) allocate(rvblten_p(ims:ime,kms:kme,jms:jme))
 if(.not.allocated(rthblten_p)) allocate(rthblten_p(ims:ime,kms:kme,jms:jme))

 gwdo_select: select case (trim(gwdo_scheme))

   case("bl_ysu_gwdo")
    if(.not.allocated(var2d_p) ) allocate(var2d_p(ims:ime,jms:jme) )
    if(.not.allocated(con_p)   ) allocate(con_p(ims:ime,jms:jme)   )
    if(.not.allocated(oa1_p)   ) allocate(oa1_p(ims:ime,jms:jme)   )
    if(.not.allocated(oa2_p)   ) allocate(oa2_p(ims:ime,jms:jme)   )
    if(.not.allocated(oa3_p)   ) allocate(oa3_p(ims:ime,jms:jme)   )
    if(.not.allocated(oa4_p)   ) allocate(oa4_p(ims:ime,jms:jme)   )
    if(.not.allocated(ol1_p)   ) allocate(ol1_p(ims:ime,jms:jme)   )
    if(.not.allocated(ol2_p)   ) allocate(ol2_p(ims:ime,jms:jme)   )
    if(.not.allocated(ol3_p)   ) allocate(ol3_p(ims:ime,jms:jme)   )
    if(.not.allocated(ol4_p)   ) allocate(ol4_p(ims:ime,jms:jme)   )

   case("bl_ugwp_gwdo")
    if(.not.allocated(var2dls_p) ) allocate(var2dls_p(ims:ime,jms:jme) )
    if(.not.allocated(conls_p)   ) allocate(conls_p(ims:ime,jms:jme)   )
    if(.not.allocated(oa1ls_p)   ) allocate(oa1ls_p(ims:ime,jms:jme)   )
    if(.not.allocated(oa2ls_p)   ) allocate(oa2ls_p(ims:ime,jms:jme)   )
    if(.not.allocated(oa3ls_p)   ) allocate(oa3ls_p(ims:ime,jms:jme)   )
    if(.not.allocated(oa4ls_p)   ) allocate(oa4ls_p(ims:ime,jms:jme)   )
    if(.not.allocated(ol1ls_p)   ) allocate(ol1ls_p(ims:ime,jms:jme)   )
    if(.not.allocated(ol2ls_p)   ) allocate(ol2ls_p(ims:ime,jms:jme)   )
    if(.not.allocated(ol3ls_p)   ) allocate(ol3ls_p(ims:ime,jms:jme)   )
    if(.not.allocated(ol4ls_p)   ) allocate(ol4ls_p(ims:ime,jms:jme)   )
    if(.not.allocated(var2dss_p) ) allocate(var2dss_p(ims:ime,jms:jme) )
    if(.not.allocated(conss_p)   ) allocate(conss_p(ims:ime,jms:jme)   )
    if(.not.allocated(oa1ss_p)   ) allocate(oa1ss_p(ims:ime,jms:jme)   )
    if(.not.allocated(oa2ss_p)   ) allocate(oa2ss_p(ims:ime,jms:jme)   )
    if(.not.allocated(oa3ss_p)   ) allocate(oa3ss_p(ims:ime,jms:jme)   )
    if(.not.allocated(oa4ss_p)   ) allocate(oa4ss_p(ims:ime,jms:jme)   )
    if(.not.allocated(ol1ss_p)   ) allocate(ol1ss_p(ims:ime,jms:jme)   )
    if(.not.allocated(ol2ss_p)   ) allocate(ol2ss_p(ims:ime,jms:jme)   )
    if(.not.allocated(ol3ss_p)   ) allocate(ol3ss_p(ims:ime,jms:jme)   )
    if(.not.allocated(ol4ss_p)   ) allocate(ol4ss_p(ims:ime,jms:jme)   )
    if(.not.allocated(hpbl_p)    ) allocate(hpbl_p(ims:ime,jms:jme)    )
    if(.not.allocated(br_p)      ) allocate(br_p(ims:ime,jms:jme)      )
    if(.not.allocated(xland_p )  ) allocate(xland_p(ims:ime,jms:jme)   )
    if (ugwp_diags) then
       if(.not.allocated(dusfc_ls_p)) allocate(dusfc_ls_p(ims:ime,jms:jme))
       if(.not.allocated(dvsfc_ls_p)) allocate(dvsfc_ls_p(ims:ime,jms:jme))
       if(.not.allocated(dusfc_bl_p)) allocate(dusfc_bl_p(ims:ime,jms:jme))
       if(.not.allocated(dvsfc_bl_p)) allocate(dvsfc_bl_p(ims:ime,jms:jme))
       if(.not.allocated(dusfc_ss_p)) allocate(dusfc_ss_p(ims:ime,jms:jme))
       if(.not.allocated(dvsfc_ss_p)) allocate(dvsfc_ss_p(ims:ime,jms:jme))
       if(.not.allocated(dusfc_fd_p)) allocate(dusfc_fd_p(ims:ime,jms:jme))
       if(.not.allocated(dvsfc_fd_p)) allocate(dvsfc_fd_p(ims:ime,jms:jme))
       if(.not.allocated(dtaux3d_ls_p)) allocate(dtaux3d_ls_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(dtauy3d_ls_p)) allocate(dtauy3d_ls_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(dtaux3d_bl_p)) allocate(dtaux3d_bl_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(dtauy3d_bl_p)) allocate(dtauy3d_bl_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(dtaux3d_ss_p)) allocate(dtaux3d_ss_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(dtauy3d_ss_p)) allocate(dtauy3d_ss_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(dtaux3d_fd_p)) allocate(dtaux3d_fd_p(ims:ime,kms:kme,jms:jme))
       if(.not.allocated(dtauy3d_fd_p)) allocate(dtauy3d_fd_p(ims:ime,kms:kme,jms:jme))
       if (ngw_scheme) then
          if(.not.allocated(dudt_ngw_p)) allocate(dudt_ngw_p(ims:ime,kms:kme,jms:jme))
          if(.not.allocated(dvdt_ngw_p)) allocate(dvdt_ngw_p(ims:ime,kms:kme,jms:jme))
          if(.not.allocated(dtdt_ngw_p)) allocate(dtdt_ngw_p(ims:ime,kms:kme,jms:jme))
       endif
    endif
    if (ngw_scheme) then
       if(.not.allocated(xlat_p)) allocate(xlat_p(ims:ime,jms:jme))
       if(.not.allocated(raincv_p)  ) allocate(raincv_p(ims:ime,jms:jme)  )
       if(.not.allocated(rainncv_p) ) allocate(rainncv_p(ims:ime,jms:jme) )
       if(.not.allocated(jindx1_tau_p)) allocate(jindx1_tau_p(ims:ime,jms:jme))
       if(.not.allocated(jindx2_tau_p)) allocate(jindx2_tau_p(ims:ime,jms:jme))
       if(.not.allocated(ddy_j1tau_p)) allocate(ddy_j1tau_p(ims:ime,jms:jme))
       if(.not.allocated(ddy_j2tau_p)) allocate(ddy_j2tau_p(ims:ime,jms:jme))
    endif

   case default

 end select gwdo_select

 end subroutine allocate_gwdo

!=================================================================================================================
 subroutine deallocate_gwdo(configs)
!=================================================================================================================

 !input arguments:
 type(mpas_pool_type),intent(in):: configs

 !local variables:
 character(len=StrKIND),pointer:: gwdo_scheme
 logical,pointer:: ugwp_diags,ngw_scheme

 call mpas_pool_get_config(configs,'config_gwdo_scheme',gwdo_scheme)
 call mpas_pool_get_config(configs,'config_ugwp_diags',ugwp_diags)
 call mpas_pool_get_config(configs,'config_ngw_scheme',ngw_scheme)

 if(allocated(cosa_p)  ) deallocate(cosa_p  )
 if(allocated(sina_p)  ) deallocate(sina_p  )

 if(allocated(dx_p)    ) deallocate(dx_p    )
 if(allocated(kpbl_p)  ) deallocate(kpbl_p  )
 if(allocated(dusfcg_p)) deallocate(dusfcg_p)
 if(allocated(dvsfcg_p)) deallocate(dvsfcg_p)
 if(allocated(dtaux3d_p)) deallocate(dtaux3d_p)
 if(allocated(dtauy3d_p)) deallocate(dtauy3d_p)
 if(allocated(rublten_p)) deallocate(rublten_p)
 if(allocated(rvblten_p)) deallocate(rvblten_p)
 if(allocated(rthblten_p)) deallocate(rthblten_p)

 gwdo_select: select case (trim(gwdo_scheme))

   case("bl_ysu_gwdo")
    if(allocated(var2d_p) ) deallocate(var2d_p )
    if(allocated(con_p)   ) deallocate(con_p   )
    if(allocated(oa1_p)   ) deallocate(oa1_p   )
    if(allocated(oa2_p)   ) deallocate(oa2_p   )
    if(allocated(oa3_p)   ) deallocate(oa3_p   )
    if(allocated(oa4_p)   ) deallocate(oa4_p   )
    if(allocated(ol1_p)   ) deallocate(ol1_p   )
    if(allocated(ol2_p)   ) deallocate(ol2_p   )
    if(allocated(ol3_p)   ) deallocate(ol3_p   )
    if(allocated(ol4_p)   ) deallocate(ol4_p   )

   case("bl_ugwp_gwdo")
    if(allocated(var2dls_p) ) deallocate(var2dls_p )
    if(allocated(conls_p)   ) deallocate(conls_p   )
    if(allocated(oa1ls_p)   ) deallocate(oa1ls_p   )
    if(allocated(oa2ls_p)   ) deallocate(oa2ls_p   )
    if(allocated(oa3ls_p)   ) deallocate(oa3ls_p   )
    if(allocated(oa4ls_p)   ) deallocate(oa4ls_p   )
    if(allocated(ol1ls_p)   ) deallocate(ol1ls_p   )
    if(allocated(ol2ls_p)   ) deallocate(ol2ls_p   )
    if(allocated(ol3ls_p)   ) deallocate(ol3ls_p   )
    if(allocated(ol4ls_p)   ) deallocate(ol4ls_p   )
    if(allocated(var2dss_p) ) deallocate(var2dss_p )
    if(allocated(conss_p)   ) deallocate(conss_p   )
    if(allocated(oa1ss_p)   ) deallocate(oa1ss_p   )
    if(allocated(oa2ss_p)   ) deallocate(oa2ss_p   )
    if(allocated(oa3ss_p)   ) deallocate(oa3ss_p   )
    if(allocated(oa4ss_p)   ) deallocate(oa4ss_p   )
    if(allocated(ol1ss_p)   ) deallocate(ol1ss_p   )
    if(allocated(ol2ss_p)   ) deallocate(ol2ss_p   )
    if(allocated(ol3ss_p)   ) deallocate(ol3ss_p   )
    if(allocated(ol4ss_p)   ) deallocate(ol4ss_p   )
    if(allocated(hpbl_p)     ) deallocate(hpbl_p  )
    if(allocated(br_p)       ) deallocate(br_p    )
    if(allocated(xland_p)    ) deallocate(xland_p )
    if (ugwp_diags) then
       if(allocated(dusfc_ls_p)) deallocate(dusfc_ls_p)
       if(allocated(dvsfc_ls_p)) deallocate(dvsfc_ls_p)
       if(allocated(dusfc_bl_p)) deallocate(dusfc_bl_p)
       if(allocated(dvsfc_bl_p)) deallocate(dvsfc_bl_p)
       if(allocated(dusfc_ss_p)) deallocate(dusfc_ss_p)
       if(allocated(dvsfc_ss_p)) deallocate(dvsfc_ss_p)
       if(allocated(dusfc_fd_p)) deallocate(dusfc_fd_p)
       if(allocated(dvsfc_fd_p)) deallocate(dvsfc_fd_p)
       if(allocated(dtaux3d_ls_p)) deallocate(dtaux3d_ls_p)
       if(allocated(dtauy3d_ls_p)) deallocate(dtauy3d_ls_p)
       if(allocated(dtaux3d_bl_p)) deallocate(dtaux3d_bl_p)
       if(allocated(dtauy3d_bl_p)) deallocate(dtauy3d_bl_p)
       if(allocated(dtaux3d_ss_p)) deallocate(dtaux3d_ss_p)
       if(allocated(dtauy3d_ss_p)) deallocate(dtauy3d_ss_p)
       if(allocated(dtaux3d_fd_p)) deallocate(dtaux3d_fd_p)
       if(allocated(dtauy3d_fd_p)) deallocate(dtauy3d_fd_p)
       if (ngw_scheme) then
          if(allocated(dudt_ngw_p)) deallocate(dudt_ngw_p)
          if(allocated(dvdt_ngw_p)) deallocate(dvdt_ngw_p)
          if(allocated(dtdt_ngw_p)) deallocate(dtdt_ngw_p)
       endif
    endif
    if (ngw_scheme) then
       if(allocated(xlat_p)) deallocate(xlat_p)
       if(allocated(raincv_p)  ) deallocate(raincv_p)
       if(allocated(rainncv_p) ) deallocate(rainncv_p)
       if(allocated(jindx1_tau_p)) deallocate(jindx1_tau_p)
       if(allocated(jindx2_tau_p)) deallocate(jindx2_tau_p)
       if(allocated(ddy_j1tau_p)) deallocate(ddy_j1tau_p)
       if(allocated(ddy_j2tau_p)) deallocate(ddy_j2tau_p)
    endif

   case default

 end select gwdo_select

 end subroutine deallocate_gwdo

!=================================================================================================================
 subroutine gwdo_from_MPAS(configs,mesh,sfc_input,ngw_input,diag_physics,tend_physics,its,ite)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: sfc_input
 type(mpas_pool_type),intent(in):: ngw_input
 type(mpas_pool_type),intent(in):: diag_physics
 type(mpas_pool_type),intent(in):: tend_physics

 integer,intent(in):: its,ite

!local variables:
 integer:: i,k,j
 character(len=StrKIND),pointer:: gwdo_scheme
 character(len=StrKIND),pointer:: convection_scheme,microp_scheme
 logical,pointer:: ugwp_diags,ngw_scheme
 real(kind=RKIND),parameter :: rad2deg = 180./3.1415926

!local pointers:
 integer,dimension(:),pointer:: kpbl
 integer,dimension(:),pointer:: jindx1_tau,jindx2_tau
 real(kind=RKIND),pointer:: len_disp
 real(kind=RKIND),dimension(:),pointer  :: meshDensity
 real(kind=RKIND),dimension(:),pointer  :: oa1,oa2,oa3,oa4,ol1,ol2,ol3,ol4,con,var2d
 real(kind=RKIND),dimension(:),pointer  :: oa1ls,oa2ls,oa3ls,oa4ls,ol1ls,ol2ls,       &
                                           ol3ls,ol4ls,conls,var2dls
 real(kind=RKIND),dimension(:),pointer  :: oa1ss,oa2ss,oa3ss,oa4ss,ol1ss,ol2ss,       &
                                           ol3ss,ol4ss,conss,var2dss
 real(kind=RKIND),dimension(:),pointer  :: dusfcg,dvsfcg
 real(kind=RKIND),dimension(:,:),pointer:: dtaux3d,dtauy3d,rublten,rvblten
 real(kind=RKIND),dimension(:,:),pointer:: rthblten
 real(kind=RKIND),dimension(:),pointer  :: dusfc_ls,dvsfc_ls,dusfc_bl,dvsfc_bl,     &
                                           dusfc_ss,dvsfc_ss,dusfc_fd,dvsfc_fd
 real(kind=RKIND),dimension(:),pointer  :: hpbl,xland,br1
 real(kind=RKIND),dimension(:),pointer  :: latCell,ddy_j1tau,ddy_j2tau,raincv,rainncv
 real(kind=RKIND),dimension(:,:),pointer:: dtaux3d_ls,dtauy3d_ls,dtaux3d_bl,dtauy3d_bl, &
                                           dtaux3d_ss,dtauy3d_ss,dtaux3d_fd,dtauy3d_fd
 real(kind=RKIND),dimension(:,:),pointer:: dudt_ngw,dvdt_ngw,dtdt_ngw

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_len_disp',len_disp)
 call mpas_pool_get_config(configs,'config_gwdo_scheme',gwdo_scheme)
 call mpas_pool_get_config(configs,'config_ugwp_diags',ugwp_diags)
 call mpas_pool_get_config(configs,'config_ngw_scheme',ngw_scheme)
 call mpas_pool_get_config(configs,'config_convection_scheme',convection_scheme)
 call mpas_pool_get_config(configs,'config_microp_scheme',microp_scheme)
 call mpas_pool_get_array(mesh,'meshDensity',meshDensity)


 gwdo_select: select case (trim(gwdo_scheme))

   case("bl_ysu_gwdo")
      call mpas_pool_get_array(sfc_input,'var2d',var2d)
      call mpas_pool_get_array(sfc_input,'con'  ,con  )
      call mpas_pool_get_array(sfc_input,'oa1'  ,oa1  )
      call mpas_pool_get_array(sfc_input,'oa2'  ,oa2  )
      call mpas_pool_get_array(sfc_input,'oa3'  ,oa3  )
      call mpas_pool_get_array(sfc_input,'oa4'  ,oa4  )
      call mpas_pool_get_array(sfc_input,'ol1'  ,ol1  )
      call mpas_pool_get_array(sfc_input,'ol2'  ,ol2  )
      call mpas_pool_get_array(sfc_input,'ol3'  ,ol3  )
      call mpas_pool_get_array(sfc_input,'ol4'  ,ol4  )
      do j = jts,jte
      do i = its,ite
         var2d_p(i,j) = var2d(i)
         con_p(i,j)   = con(i)
         oa1_p(i,j)   = oa1(i)
         oa2_p(i,j)   = oa2(i)
         oa3_p(i,j)   = oa3(i)
         oa4_p(i,j)   = oa4(i)
         ol1_p(i,j)   = ol1(i)
         ol2_p(i,j)   = ol2(i)
         ol3_p(i,j)   = ol3(i)
         ol4_p(i,j)   = ol4(i)
      enddo
      enddo

   case("bl_ugwp_gwdo")
      call mpas_pool_get_array(sfc_input,'var2dls',var2dls)
      call mpas_pool_get_array(sfc_input,'conls'  ,conls  )
      call mpas_pool_get_array(sfc_input,'oa1ls'  ,oa1ls  )
      call mpas_pool_get_array(sfc_input,'oa2ls'  ,oa2ls  )
      call mpas_pool_get_array(sfc_input,'oa3ls'  ,oa3ls  )
      call mpas_pool_get_array(sfc_input,'oa4ls'  ,oa4ls  )
      call mpas_pool_get_array(sfc_input,'ol1ls'  ,ol1ls  )
      call mpas_pool_get_array(sfc_input,'ol2ls'  ,ol2ls  )
      call mpas_pool_get_array(sfc_input,'ol3ls'  ,ol3ls  )
      call mpas_pool_get_array(sfc_input,'ol4ls'  ,ol4ls  )
      call mpas_pool_get_array(sfc_input,'var2dss',var2dss)
      call mpas_pool_get_array(sfc_input,'conss'  ,conss  )
      call mpas_pool_get_array(sfc_input,'oa1ss'  ,oa1ss  )
      call mpas_pool_get_array(sfc_input,'oa2ss'  ,oa2ss  )
      call mpas_pool_get_array(sfc_input,'oa3ss'  ,oa3ss  )
      call mpas_pool_get_array(sfc_input,'oa4ss'  ,oa4ss  )
      call mpas_pool_get_array(sfc_input,'ol1ss'  ,ol1ss  )
      call mpas_pool_get_array(sfc_input,'ol2ss'  ,ol2ss  )
      call mpas_pool_get_array(sfc_input,'ol3ss'  ,ol3ss  )
      call mpas_pool_get_array(sfc_input,'ol4ss'  ,ol4ss  )
      call mpas_pool_get_array(diag_physics,'hpbl',hpbl   )
      call mpas_pool_get_array(diag_physics,'br'  ,br1    )
      call mpas_pool_get_array(sfc_input,'xland'  ,xland  )
      do j = jts,jte
      do i = its,ite
         var2dls_p(i,j) = var2dls(i)
         conls_p(i,j)   = conls(i)
         oa1ls_p(i,j)   = oa1ls(i)
         oa2ls_p(i,j)   = oa2ls(i)
         oa3ls_p(i,j)   = oa3ls(i)
         oa4ls_p(i,j)   = oa4ls(i)
         ol1ls_p(i,j)   = ol1ls(i)
         ol2ls_p(i,j)   = ol2ls(i)
         ol3ls_p(i,j)   = ol3ls(i)
         ol4ls_p(i,j)   = ol4ls(i)
         var2dss_p(i,j) = var2dss(i)
         conss_p(i,j)   = conss(i)
         oa1ss_p(i,j)   = oa1ss(i)
         oa2ss_p(i,j)   = oa2ss(i)
         oa3ss_p(i,j)   = oa3ss(i)
         oa4ss_p(i,j)   = oa4ss(i)
         ol1ss_p(i,j)   = ol1ss(i)
         ol2ss_p(i,j)   = ol2ss(i)
         ol3ss_p(i,j)   = ol3ss(i)
         ol4ss_p(i,j)   = ol4ss(i)
         hpbl_p(i,j)    = hpbl(i)
         br_p(i,j)      = br1(i)
         xland_p(i,j)   = xland(i)
      enddo
      enddo
      if (ugwp_diags) then
         call mpas_pool_get_array(diag_physics,'dusfc_ls'   ,dusfc_ls   )
         call mpas_pool_get_array(diag_physics,'dvsfc_ls'   ,dvsfc_ls   )
         call mpas_pool_get_array(diag_physics,'dusfc_bl'   ,dusfc_bl   )
         call mpas_pool_get_array(diag_physics,'dvsfc_bl'   ,dvsfc_bl   )
         call mpas_pool_get_array(diag_physics,'dusfc_ss'   ,dusfc_ss   )
         call mpas_pool_get_array(diag_physics,'dvsfc_ss'   ,dvsfc_ss   )
         call mpas_pool_get_array(diag_physics,'dusfc_fd'   ,dusfc_fd   )
         call mpas_pool_get_array(diag_physics,'dvsfc_fd'   ,dvsfc_fd   )
         call mpas_pool_get_array(diag_physics,'dtaux3d_ls' ,dtaux3d_ls )
         call mpas_pool_get_array(diag_physics,'dtauy3d_ls' ,dtauy3d_ls )
         call mpas_pool_get_array(diag_physics,'dtaux3d_bl' ,dtaux3d_bl )
         call mpas_pool_get_array(diag_physics,'dtauy3d_bl' ,dtauy3d_bl )
         call mpas_pool_get_array(diag_physics,'dtaux3d_ss' ,dtaux3d_ss )
         call mpas_pool_get_array(diag_physics,'dtauy3d_ss' ,dtauy3d_ss )
         call mpas_pool_get_array(diag_physics,'dtaux3d_fd' ,dtaux3d_fd )
         call mpas_pool_get_array(diag_physics,'dtauy3d_fd' ,dtauy3d_fd )
         do j = jts,jte
         do i = its,ite
            dusfc_ls_p(i,j) = dusfc_ls(i)
            dvsfc_ls_p(i,j) = dvsfc_ls(i)
            dusfc_bl_p(i,j) = dusfc_bl(i)
            dvsfc_bl_p(i,j) = dvsfc_bl(i)
            dusfc_ss_p(i,j) = dusfc_ss(i)
            dvsfc_ss_p(i,j) = dvsfc_ss(i)
            dusfc_fd_p(i,j) = dusfc_fd(i)
            dvsfc_fd_p(i,j) = dvsfc_fd(i)
         enddo
         enddo
         do j = jts,jte
         do k = kts,kte
         do i = its,ite
            dtaux3d_ls_p(i,k,j) = dtaux3d_ls(k,i)
            dtauy3d_ls_p(i,k,j) = dtauy3d_ls(k,i)
            dtaux3d_bl_p(i,k,j) = dtaux3d_bl(k,i)
            dtauy3d_bl_p(i,k,j) = dtauy3d_bl(k,i)
            dtaux3d_ss_p(i,k,j) = dtaux3d_ss(k,i)
            dtauy3d_ss_p(i,k,j) = dtauy3d_ss(k,i)
            dtaux3d_fd_p(i,k,j) = dtaux3d_fd(k,i)
            dtauy3d_fd_p(i,k,j) = dtauy3d_fd(k,i)
         enddo
         enddo
         enddo
      endif
      if (ugwp_diags.and.ngw_scheme) then
         call mpas_pool_get_array(diag_physics,'dudt_ngw',dudt_ngw)
         call mpas_pool_get_array(diag_physics,'dvdt_ngw',dvdt_ngw)
         call mpas_pool_get_array(diag_physics,'dtdt_ngw',dtdt_ngw)
         do j = jts,jte
         do k = kts,kte
         do i = its,ite
            dudt_ngw_p(i,k,j) = dudt_ngw(k,i)
            dvdt_ngw_p(i,k,j) = dvdt_ngw(k,i)
            dtdt_ngw_p(i,k,j) = dtdt_ngw(k,i)
         enddo
         enddo
         enddo
      endif
      if (ngw_scheme) then
         call mpas_pool_get_array(mesh,'latCell',latCell)
         if(trim(convection_scheme) /= "off") &
            call mpas_pool_get_array(diag_physics,'raincv',raincv)
         if(trim(microp_scheme) /= "off") &
            call mpas_pool_get_array(diag_physics,'rainncv',rainncv)
         call mpas_pool_get_array(ngw_input,'jindx1_tau',jindx1_tau)
         call mpas_pool_get_array(ngw_input,'jindx2_tau',jindx2_tau)
         call mpas_pool_get_array(ngw_input,'ddy_j1tau', ddy_j1tau)
         call mpas_pool_get_array(ngw_input,'ddy_j2tau', ddy_j2tau)
         do j = jts,jte
         do i = its,ite
            xlat_p(i,j) = latCell(i)*rad2deg   ! latitude in degrees
            jindx1_tau_p(i,j) = jindx1_tau(i)
            jindx2_tau_p(i,j) = jindx2_tau(i)
            ddy_j1tau_p(i,j)  = ddy_j1tau(i)
            ddy_j2tau_p(i,j)  = ddy_j2tau(i)
         enddo
         enddo
         ! Treat rain rates conditionally
         if(trim(convection_scheme) == "off") then
            raincv_p(:,:) = 0._RKIND
         else
            do j = jts,jte
            do i = its,ite
               raincv_p(i,j) = raincv(i)
            enddo
            enddo
         endif
         if(trim(microp_scheme) == "off") then
            rainncv_p(:,:) = 0._RKIND
         else
            do j = jts,jte
            do i = its,ite
               rainncv_p(i,j) = rainncv(i)
            enddo
            enddo
         endif

      endif

   case default

 end select gwdo_select


 call mpas_pool_get_array(diag_physics,'kpbl'    ,kpbl    )
 call mpas_pool_get_array(diag_physics,'dusfcg'  ,dusfcg  )
 call mpas_pool_get_array(diag_physics,'dvsfcg'  ,dvsfcg  )
 call mpas_pool_get_array(diag_physics,'dtaux3d' ,dtaux3d )
 call mpas_pool_get_array(diag_physics,'dtauy3d' ,dtauy3d )
 call mpas_pool_get_array(tend_physics,'rublten' ,rublten )
 call mpas_pool_get_array(tend_physics,'rvblten' ,rvblten )
 call mpas_pool_get_array(tend_physics,'rthblten',rthblten)

 do j = jts,jte
 do i = its,ite
    sina_p(i,j)  = 0._RKIND
    cosa_p(i,j)  = 1._RKIND
    dx_p(i,j) = len_disp / meshDensity(i)**0.25
    kpbl_p(i,j)   = kpbl(i)
    dusfcg_p(i,j) = dusfcg(i)
    dvsfcg_p(i,j) = dvsfcg(i)
 enddo
 enddo

 do j = jts,jte
 do k = kts,kte
 do i = its,ite
    dtaux3d_p(i,k,j) = dtaux3d(k,i)
    dtauy3d_p(i,k,j) = dtauy3d(k,i)
    rublten_p(i,k,j) = rublten(k,i)
    rvblten_p(i,k,j) = rvblten(k,i)
    rthblten_p(i,k,j) = rthblten(k,i)
 enddo
 enddo
 enddo

 end subroutine gwdo_from_MPAS
 
!=================================================================================================================
 subroutine gwdo_to_MPAS(configs,diag_physics,tend_physics,its,ite)
!=================================================================================================================

!input arguments:
 integer,intent(in):: its,ite
 type(mpas_pool_type),intent(in):: configs

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: tend_physics

!local variables:
 integer:: i,k,j
 character(len=StrKIND),pointer:: gwdo_scheme
 logical,pointer:: ugwp_diags,ngw_scheme

!local pointers:
 real(kind=RKIND),dimension(:),pointer  :: dusfcg,dvsfcg
 real(kind=RKIND),dimension(:,:),pointer:: dtaux3d,dtauy3d,rubldiff,rvbldiff,rublten,rvblten
 real(kind=RKIND),dimension(:,:),pointer:: rthblten

 real(kind=RKIND),dimension(:),pointer  :: oa1ls,oa2ls,oa3ls,oa4ls,ol1ls,ol2ls,       &
                                           ol3ls,ol4ls,conls,var2dls
 real(kind=RKIND),dimension(:),pointer  :: oa1ss,oa2ss,oa3ss,oa4ss,ol1ss,ol2ss,       &
                                           ol3ss,ol4ss,conss,var2dss
 real(kind=RKIND),dimension(:),pointer  :: dusfc_ls,dvsfc_ls,dusfc_bl,dvsfc_bl,       &
                                           dusfc_ss,dvsfc_ss,dusfc_fd,dvsfc_fd
 real(kind=RKIND),dimension(:,:),pointer:: dtaux3d_ls,dtauy3d_ls,dtaux3d_bl,dtauy3d_bl, &
                                           dtaux3d_ss,dtauy3d_ss,dtaux3d_fd,dtauy3d_fd
 real(kind=RKIND),dimension(:,:),pointer:: dudt_ngw,dvdt_ngw,dtdt_ngw

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_gwdo_scheme',gwdo_scheme)
 call mpas_pool_get_config(configs,'config_ugwp_diags',ugwp_diags)
 call mpas_pool_get_config(configs,'config_ngw_scheme',ngw_scheme)
 call mpas_pool_get_array(diag_physics,'dusfcg'  ,dusfcg  )
 call mpas_pool_get_array(diag_physics,'dvsfcg'  ,dvsfcg  )
 call mpas_pool_get_array(diag_physics,'dtaux3d' ,dtaux3d )
 call mpas_pool_get_array(diag_physics,'dtauy3d' ,dtauy3d )
 call mpas_pool_get_array(diag_physics,'rubldiff',rubldiff)
 call mpas_pool_get_array(diag_physics,'rvbldiff',rvbldiff)
 call mpas_pool_get_array(tend_physics,'rublten' ,rublten )
 call mpas_pool_get_array(tend_physics,'rvblten' ,rvblten )
 call mpas_pool_get_array(tend_physics,'rthblten',rthblten)


 gwdo_select: select case (trim(gwdo_scheme))

   case("bl_ugwp_gwdo")
      if (ugwp_diags) then
         call mpas_pool_get_array(diag_physics,'dusfc_ls'  ,dusfc_ls  )
         call mpas_pool_get_array(diag_physics,'dvsfc_ls'  ,dvsfc_ls  )
         call mpas_pool_get_array(diag_physics,'dusfc_bl'  ,dusfc_bl  )
         call mpas_pool_get_array(diag_physics,'dvsfc_bl'  ,dvsfc_bl  )
         call mpas_pool_get_array(diag_physics,'dusfc_ss'  ,dusfc_ss  )
         call mpas_pool_get_array(diag_physics,'dvsfc_ss'  ,dvsfc_ss  )
         call mpas_pool_get_array(diag_physics,'dusfc_fd'  ,dusfc_fd  )
         call mpas_pool_get_array(diag_physics,'dvsfc_fd'  ,dvsfc_fd  )
         call mpas_pool_get_array(diag_physics,'dtaux3d_ls' ,dtaux3d_ls )
         call mpas_pool_get_array(diag_physics,'dtauy3d_ls' ,dtauy3d_ls )
         call mpas_pool_get_array(diag_physics,'dtaux3d_bl' ,dtaux3d_bl )
         call mpas_pool_get_array(diag_physics,'dtauy3d_bl' ,dtauy3d_bl )
         call mpas_pool_get_array(diag_physics,'dtaux3d_ss' ,dtaux3d_ss )
         call mpas_pool_get_array(diag_physics,'dtauy3d_ss' ,dtauy3d_ss )
         call mpas_pool_get_array(diag_physics,'dtaux3d_fd' ,dtaux3d_fd )
         call mpas_pool_get_array(diag_physics,'dtauy3d_fd' ,dtauy3d_fd )
         do j = jts,jte
         do i = its,ite
            dusfc_ls(i) = dusfc_ls_p(i,j)
            dvsfc_ls(i) = dvsfc_ls_p(i,j)
            dusfc_bl(i) = dusfc_bl_p(i,j)
            dvsfc_bl(i) = dvsfc_bl_p(i,j)
            dusfc_ss(i) = dusfc_ss_p(i,j)
            dvsfc_ss(i) = dvsfc_ss_p(i,j)
            dusfc_fd(i) = dusfc_fd_p(i,j)
            dvsfc_fd(i) = dvsfc_fd_p(i,j)
         enddo
         enddo
         do j = jts,jte
         do k = kts,kte
         do i = its,ite
            dtaux3d_ls(k,i) = dtaux3d_ls_p(i,k,j)
            dtauy3d_ls(k,i) = dtauy3d_ls_p(i,k,j)
            dtaux3d_bl(k,i) = dtaux3d_bl_p(i,k,j)
            dtauy3d_bl(k,i) = dtauy3d_bl_p(i,k,j)
            dtaux3d_ss(k,i) = dtaux3d_ss_p(i,k,j)
            dtauy3d_ss(k,i) = dtauy3d_ss_p(i,k,j)
            dtaux3d_fd(k,i) = dtaux3d_fd_p(i,k,j)
            dtauy3d_fd(k,i) = dtauy3d_fd_p(i,k,j)
         enddo
         enddo
         enddo
         if (ngw_scheme) then
            call mpas_pool_get_array(diag_physics,'dudt_ngw' ,dudt_ngw )
            call mpas_pool_get_array(diag_physics,'dvdt_ngw' ,dvdt_ngw )
            call mpas_pool_get_array(diag_physics,'dtdt_ngw' ,dtdt_ngw )
            do j = jts,jte
            do k = kts,kte
            do i = its,ite
               dudt_ngw(k,i) = dudt_ngw_p(i,k,j)
               dvdt_ngw(k,i) = dvdt_ngw_p(i,k,j)
               dtdt_ngw(k,i) = dtdt_ngw_p(i,k,j)
            enddo
            enddo
            enddo
         endif
      endif

   case default

 end select gwdo_select

 do j = jts,jte
 do i = its,ite
    dusfcg(i) = dusfcg_p(i,j) 
    dvsfcg(i) = dvsfcg_p(i,j)
 enddo
 enddo

 do j = jts,jte
 do k = kts,kte
 do i = its,ite
    dtaux3d(k,i)  = dtaux3d_p(i,k,j)
    dtauy3d(k,i)  = dtauy3d_p(i,k,j)
    rubldiff(k,i) = rublten_p(i,k,j)-rublten(k,i)
    rvbldiff(k,i) = rvblten_p(i,k,j)-rvblten(k,i) 
    rublten(k,i)  = rublten_p(i,k,j)
    rvblten(k,i)  = rvblten_p(i,k,j)
    rthblten(k,i) = rthblten_p(i,k,j)
 enddo
 enddo
 enddo

 end subroutine gwdo_to_MPAS
 
!=================================================================================================================
 subroutine driver_gwdo(itimestep,configs,mesh,sfc_input,ngw_input,diag_physics,tend_physics,its,ite)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: sfc_input

 integer,intent(in):: its,ite
 integer,intent(in):: itimestep

!inout arguments:
 type(mpas_pool_type),intent(inout):: ngw_input
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: tend_physics

!local variables:
 character(len=StrKIND),pointer:: gwdo_scheme
 logical,pointer:: ugwp_diags,ngw_scheme
 integer,pointer:: ntau_d1y_ptr,ntau_d2t_ptr
 real(kind=RKIND),dimension(:),pointer :: days_limb_ptr
 real(kind=RKIND),dimension(:,:),pointer:: tau_limb_ptr
 integer:: ntau_d1y,ntau_d2t
 real(kind=RKIND),dimension(:),allocatable::   days_limb
 real(kind=RKIND),dimension(:,:),allocatable:: tau_limb

 integer:: i
 real(kind=RKIND),dimension(:),allocatable:: dx_max

!CCPP-compliant flags:
 character(len=StrKIND):: errmsg
 integer:: errflg

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write('')
!call mpas_log_write('--- enter subroutine driver_gwdo:')

!initialization of CCPP-compliant flags:
 errmsg = ' '
 errflg = 0

 call mpas_pool_get_config(configs,'config_gwdo_scheme',gwdo_scheme)
 call mpas_pool_get_config(configs,'config_ugwp_diags',ugwp_diags)
 call mpas_pool_get_config(configs,'config_ngw_scheme',ngw_scheme)

 ! Call up variables needed for NGW scheme
 if (ngw_scheme) then
    call mpas_pool_get_dimension(mesh,'lat',ntau_d1y_ptr)
    call mpas_pool_get_dimension(mesh,'days',ntau_d2t_ptr)
    call mpas_pool_get_array(ngw_input,'DAYS',days_limb_ptr)
    call mpas_pool_get_array(ngw_input,'ABSMF',tau_limb_ptr)
    ntau_d1y = ntau_d1y_ptr
    ntau_d2t = ntau_d2t_ptr
    if(.not.allocated(days_limb)) allocate(days_limb(ntau_d2t))
    if(.not.allocated(tau_limb) ) allocate(tau_limb (ntau_d1y,ntau_d2t))
    days_limb(:)   = days_limb_ptr(:)
    tau_limb (:,:) = tau_limb_ptr(:,:)
 endif


!copy MPAS arrays to local arrays:
 call gwdo_from_MPAS(configs,mesh,sfc_input,ngw_input,diag_physics,tend_physics,its,ite)

 gwdo_select: select case (trim(gwdo_scheme))

    case("bl_ysu_gwdo")
       call mpas_timer_start('bl_gwdo')
       call gwdo ( &
                  p3d       = pres_hydd_p , p3di      = pres2_hydd_p , pi3d    = pi_p      , &
                  u3d       = u_p         , v3d       = v_p          , t3d     = t_p       , & 
                  qv3d      = qv_p        , z         = zmid_p       , rublten = rublten_p , &
                  rvblten   = rvblten_p   , dtaux3d   = dtaux3d_p    , dtauy3d = dtauy3d_p , &
                  dusfcg    = dusfcg_p    , dvsfcg    = dvsfcg_p     , kpbl2d  = kpbl_p    , &
                  itimestep = itimestep   , dt        = dt_pbl       , dx      = dx_p      , & 
                  cp        = cp          , g         = gravity      , rd      = R_d       , & 
                  rv        = R_v         , ep1       = ep_1         , pi      = pii       , & 
                  var2d     = var2d_p     , oc12d     = con_p        , oa2d1   = oa1_p     , & 
                  oa2d2     = oa2_p       , oa2d3     = oa3_p        , oa2d4   = oa4_p     , &
                  ol2d1     = ol1_p       , ol2d2     = ol2_p        , ol2d3   = ol3_p     , & 
                  ol2d4     = ol4_p       , sina      = sina_p       , cosa    = cosa_p    , &
                  errmsg    = errmsg      , errflg    = errflg       ,                       &
                  ids = ids , ide = ide , jds = jds , jde = jde , kds = kds , kde = kde ,    &
                  ims = ims , ime = ime , jms = jms , jme = jme , kms = kms , kme = kme ,    &
                  its = its , ite = ite , jts = jts , jte = jte , kts = kts , kte = kte      &
                 )
       call mpas_timer_stop('bl_gwdo')

    case("bl_ugwp_gwdo")
       call mpas_timer_start('bl_ugwp_gwdo')
       call gwdo_ugwp ( & 
                  p3d        = pres_hydd_p , p3di       = pres2_hydd_p, pi3d     = pi_p      ,  &
                  u3d        = u_p         , v3d        = v_p         , t3d      = t_p       ,  &
                  qv3d       = qv_p        , z          = zmid_p      , rublten  = rublten_p ,  &
                  rvblten    = rvblten_p   , rthblten   = rthblten_p  ,                         &
                  dtaux3d    = dtaux3d_p   , dtauy3d    = dtauy3d_p   ,                         &
                  dusfcg     = dusfcg_p    , dvsfcg     = dvsfcg_p    , kpbl2d   = kpbl_p    ,  &
                  itimestep  = itimestep   , dt         = dt_pbl      , dx       = dx_p      ,  &
                  pblh       = hpbl_p      , br1        = br_p        , xland    = xland_p   ,  &
                  cp         = cp          , g          = gravity     , rd       = R_d       ,  &
                  rv         = R_v         , ep1        = ep_1        , pi       = pii       ,  &
                  sina       = sina_p      , cosa       = cosa_p      , dz       = dz_p      ,  &
                  var2dls    = var2dls_p   , oc12dls    = conls_p     , oa2d1ls  = oa1ls_p   ,  &
                  oa2d2ls    = oa2ls_p     , oa2d3ls    = oa3ls_p     , oa2d4ls  = oa4ls_p   ,  &
                  ol2d1ls    = ol1ls_p     , ol2d2ls    = ol2ls_p     , ol2d3ls  = ol3ls_p   ,  &
                  ol2d4ls    = ol4ls_p     , var2dss    = var2dss_p   , oc12dss  = conss_p   ,  &
                  oa2d1ss    = oa1ss_p     , oa2d2ss    = oa2ss_p     , oa2d3ss  = oa3ss_p   ,  &
                  oa2d4ss    = oa4ss_p     , ol2d1ss    = ol1ss_p     , ol2d2ss  = ol2ss_p   ,  &
                  ol2d3ss    = ol3ss_p     , ol2d4ss    = ol4ss_p     , zi       = z_p       ,  &
                  dusfc_ls   = dusfc_ls_p  , dvsfc_ls   = dvsfc_ls_p  , dusfc_bl = dusfc_bl_p,  &
                  dvsfc_bl   = dvsfc_bl_p  , dusfc_ss   = dusfc_ss_p  , dvsfc_ss = dvsfc_ss_p,  &
                  dusfc_fd   = dusfc_fd_p  , dvsfc_fd   = dvsfc_fd_p  ,                         &
                  dtaux3d_ls = dtaux3d_ls_p, dtauy3d_ls = dtauy3d_ls_p,                         &
                  dtaux3d_bl = dtaux3d_bl_p, dtauy3d_bl = dtauy3d_bl_p,                         &
                  dtaux3d_ss = dtaux3d_ss_p, dtauy3d_ss = dtauy3d_ss_p,                         &
                  dtaux3d_fd = dtaux3d_fd_p, dtauy3d_fd = dtauy3d_fd_p,                         &
                  ugwp_diags = ugwp_diags  , ngw_scheme = ngw_scheme  , xlatd    = xlat_p     , &
                  jindx1_tau = jindx1_tau_p, jindx2_tau = jindx2_tau_p,                         &
                  ddy_j1tau  = ddy_j1tau_p , ddy_j2tau  = ddy_j2tau_p , r_DoY    = curr_julday, &
                  raincv     = raincv_p    , rainncv    = rainncv_p   , ntau_d1y = ntau_d1y   , &
                  ntau_d2t   = ntau_d2t    , days_limb  = days_limb   , tau_limb = tau_limb   , &
                  dudt_ngw   = dudt_ngw_p  , dvdt_ngw   = dvdt_ngw_p  , dtdt_ngw = dtdt_ngw_p , &
                  errmsg     = errmsg      , errflg     = errflg      ,                         &
                  ids = ids , ide = ide , jds = jds , jde = jde , kds = kds , kde = kde ,       &
                  ims = ims , ime = ime , jms = jms , jme = jme , kms = kms , kme = kme ,       &
                  its = its , ite = ite , jts = jts , jte = jte , kts = kts , kte = kte         &
                 )
       if (ngw_scheme) then
          if(allocated(days_limb)) deallocate(days_limb)
          if(allocated(tau_limb) ) deallocate(tau_limb )
       endif
       call mpas_timer_stop('bl_ugwp_gwdo')

     case default

 end select gwdo_select

!copy local arrays to MPAS grid:
 call gwdo_to_MPAS(configs,diag_physics,tend_physics,its,ite)

!call mpas_log_write('--- end subroutine driver_gwdo.')
!call mpas_log_write('')

 end subroutine driver_gwdo

!=================================================================================================================
 end module mpas_atmphys_driver_gwdo
!=================================================================================================================
