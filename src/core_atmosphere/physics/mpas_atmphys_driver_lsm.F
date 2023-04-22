! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module mpas_atmphys_driver_lsm
 use mpas_kind_types
 use mpas_pool_routines
 use mpas_timer, only : mpas_timer_start, mpas_timer_stop

 use mpas_atmphys_constants
 use mpas_atmphys_landuse, only: isurban
 use mpas_atmphys_lsm_noahinit
 use mpas_atmphys_vars

!wrf physics
 use module_sf_noahdrv
 use module_sf_noah_seaice_drv
 use module_sf_sfcdiags
 use module_sf_pxlsm
 
 implicit none
 private
 public:: init_lsm,       &
          allocate_lsm,   &
          deallocate_lsm, &
          driver_lsm

 
 logical,parameter:: rdmaxalb = .false. !use snow albedo from geogrid;false use table values
 logical,parameter:: myj      = .false. !true if using Mellor-Yamada PBL scheme.
 logical,parameter:: frpcpn   = .false.
 logical,parameter:: rdlai2d  = .false.

!urban physics: MPAS does not plan to run the urban physics option.
 integer,parameter:: sf_urban_physics = 0 !activate urban canopy model (=0: no urban canopy) 


!MPAS driver for parameterization of land surface processes.
!Laura D. Fowler (send comments to laura@ucar.edu).
!2013-05-01.
!
! subroutines in mpas_atmphys_driver_lsm:
! ---------------------------------------
! allocate_lsm  : allocate local arrays for land surface parameterization.
! deallocate_lsm: deallocate local arrays for land surface parameterization.
! driver_lsm    : main driver (called from subroutine physics_driver).
! lsm_from_MPAS : initialize local arrays.
! lsm_to_MPAS   : copy local arrays to MPAS arrays.
!
! WRF physics called from driver_lsm:
! ------------------------ ----------
!    * module_sf_noahdrv : NOAH 4-layers land surface scheme.
!
! add-ons and modifications to sourcecode:
! ----------------------------------------
! * removed the pre-processor option "do_hydrostatic_pressure" before call to subroutine lsm.
!   Laura D. Fowler (birch.ucar.edu) / 2013-05-29.
! * throughout the sourcecode, replaced all "var_struct" defined arrays by local pointers.
!   Laura D. Fowler (laura@ucar.edu) / 2014-04-22.
! * modified sourcecode to use pools.
!   Laura D. Fowler (laura@ucar.edu) / 2014-05-15.
! * moved the definition of isurban to landuse_init_forMPAS in mpas_atmphys_landuse.F.
!   isurban is now defined as a function of the input landuse data file.
!   Dominikus Heinzeller (IMK) / 2014-07-24.
! * removed the global variable qz0 and initialized the local variable qz0_p to 0. qz0 is only
!   used in the MYJ PBL parameterization which is not available in MPAS.
!   Laura D. Fowler (laura@ucar.edu) / 2015-03-05.
! * in subroutine lsm_from_MPAS, modified the initialization of local variable dzs_p.
!   Laura D. Fowler (laura@ucar.edu) / 2015-04-11.
! * in subroutine lsm_to_MPAS, removed updating isltyp, ivgtyp, shdmin, and shdmax as they
!   are constant in the Noah lsm.
!   Laura D. Fowler (laura@ucar.edu) / 2015-04-11.
! * in subroutine lsm_from_MPAS, modified the calculation of rain_bl, now that the convection
!   and cloud microphysics parameterizations are contained in "packages." The original
!   calculation failed when configuration_convection_scheme or config_microphysics_scheme
!   was set of 'off'.
!   Laura D. Fowler (laura@ucar.edu) / 2016-04-13.
! * added call to the subroutine sfcdiags to update the 2-meter temperature, potential temperature, and
!   water vapor mixing ratio after call to lsm.
!   Laura D. Fowler (laura@ucar.edu) / 2016-05-11.
! * added the calculation of surface variables over seaice cells when config_frac_seaice is set to true.
!   Laura D. Fowler (laura@ucar.edu) / 2016-10-03.
! * now use isice and iswater initialized in the init file instead of initialized in mpas_atmphys_landuse.F.
!   Laura D. Fowler (laura@ucar.edu) / 2017-01-13.
! * updated the call to subroutine lsm as we updated the Noah LSM scheme to WRF version 3.9.0.
!   Laura D. Fowler (laura@ucar.edu) / 2017-01-30.
! * since we removed the local variable lsm_scheme from mpas_atmphys_vars.F, now defines lsm_scheme as a
!   pointer to config_lsm_scheme.
!   Laura D. Fowler (laura@ucar.edu) / 2017-02-16.
! * added call to seaice_noah to include the parameterization of seaice for the updated Noah land surface
!   scheme.
!   Laura D. Fowler (laura@ucar.edu) / 2017-02-19.
! * added MODIS LAI and vegetation fraction, plus soil type fraction, improvements to P-X LSM
!   Jerold A. Herwehe (herwehe.jerry@epa.gov) / 2018-08-28

!
! DOCUMENTATION:
! ./physics_wrf/module_sf_noahdrv.F: main driver for the "NOAH" land-surface parameterization.
! In the argument list,I added "OPTIONAL" to the declaration of a few arrays to avoid compiling
! with the "urban physics" option. These arrays are:
! .. num_roof_layers; num_wall_layers; num_road_layers;num_urban_layers.
! .. ust_urb2d;frc_urb2d;utype_urb2d.
! Laura D. Fowler (01-18-2011).


 contains


!=================================================================================================================
 subroutine allocate_lsm(config_frac_seaice)
!=================================================================================================================

 logical,intent(in):: config_frac_seaice
!-----------------------------------------------------------------------------------------------------------------

!arrays for soil layer properties:
 if(.not.allocated(dzs_p)   ) allocate(dzs_p(1:num_soils)                   )
 if(.not.allocated(smcrel_p)) allocate(smcrel_p(ims:ime,1:num_soils,jms:jme))
 if(.not.allocated(sh2o_p)  ) allocate(sh2o_p(ims:ime,1:num_soils,jms:jme)  )
 if(.not.allocated(smois_p) ) allocate(smois_p(ims:ime,1:num_soils,jms:jme) )
 if(.not.allocated(tslb_p)  ) allocate(tslb_p(ims:ime,1:num_soils,jms:jme)  )

!arrays for land use and soil type fractions
 if(.not.allocated(landusef_p)) allocate(landusef_p(ims:ime,1:num_landu,jms:jme))
 if(.not.allocated(soiltypf_p)) allocate(soiltypf_p(ims:ime,1:num_soilc,jms:jme))

!other arrays:
 if(.not.allocated(acsnom_p)    ) allocate(acsnom_p(ims:ime,jms:jme)    )
 if(.not.allocated(acsnow_p)    ) allocate(acsnow_p(ims:ime,jms:jme)    )
 if(.not.allocated(canwat_p)    ) allocate(canwat_p(ims:ime,jms:jme)    )
 if(.not.allocated(chs_p)       ) allocate(chs_p(ims:ime,jms:jme)       )
 if(.not.allocated(chs2_p)      ) allocate(chs2_p(ims:ime,jms:jme)      )
 if(.not.allocated(chklowq_p)   ) allocate(chklowq_p(ims:ime,jms:jme)   )
 if(.not.allocated(cpm_p)       ) allocate(cpm_p(ims:ime,jms:jme)       )
 if(.not.allocated(cqs2_p)      ) allocate(cqs2_p(ims:ime,jms:jme)      )
 if(.not.allocated(isltyp_p)    ) allocate(isltyp_p(ims:ime,jms:jme)    )
 if(.not.allocated(ivgtyp_p)    ) allocate(ivgtyp_p(ims:ime,jms:jme)    )
 if(.not.allocated(glw_p)       ) allocate(glw_p(ims:ime,jms:jme)       )
 if(.not.allocated(grdflx_p)    ) allocate(grdflx_p(ims:ime,jms:jme)    )
 if(.not.allocated(gsw_p)       ) allocate(gsw_p(ims:ime,jms:jme)       )
 if(.not.allocated(hfx_p)       ) allocate(hfx_p(ims:ime,jms:jme)       )
 if(.not.allocated(lai_p)       ) allocate(lai_p(ims:ime,jms:jme)       )
 if(.not.allocated(lh_p)        ) allocate(lh_p(ims:ime,jms:jme)        ) 
 if(.not.allocated(noahres_p)   ) allocate(noahres_p(ims:ime,jms:jme)   )
 if(.not.allocated(potevp_p)    ) allocate(potevp_p(ims:ime,jms:jme)    )
 if(.not.allocated(qfx_p)       ) allocate(qfx_p(ims:ime,jms:jme)       )
 if(.not.allocated(qgh_p)       ) allocate(qgh_p(ims:ime,jms:jme)       )
 if(.not.allocated(qsfc_p)      ) allocate(qsfc_p(ims:ime,jms:jme)      )
 if(.not.allocated(qz0_p)       ) allocate(qz0_p(ims:ime,jms:jme)       )
 if(.not.allocated(rainbl_p)    ) allocate(rainbl_p(ims:ime,jms:jme)    )
 if(.not.allocated(br_p)        ) allocate(br_p(ims:ime,jms:jme)        )
 if(.not.allocated(sfc_albbck_p)) allocate(sfc_albbck_p(ims:ime,jms:jme))
 if(.not.allocated(sfc_albedo_p)) allocate(sfc_albedo_p(ims:ime,jms:jme))
 if(.not.allocated(sfc_emibck_p)) allocate(sfc_emibck_p(ims:ime,jms:jme))
 if(.not.allocated(sfc_emiss_p) ) allocate(sfc_emiss_p(ims:ime,jms:jme) )
 if(.not.allocated(sfcrunoff_p) ) allocate(sfcrunoff_p(ims:ime,jms:jme) )
 if(.not.allocated(shdmin_p)    ) allocate(shdmin_p(ims:ime,jms:jme)    )
 if(.not.allocated(shdmax_p)    ) allocate(shdmax_p(ims:ime,jms:jme)    )
 if(.not.allocated(smstav_p)    ) allocate(smstav_p(ims:ime,jms:jme)    )
 if(.not.allocated(smstot_p)    ) allocate(smstot_p(ims:ime,jms:jme)    )
 if(.not.allocated(snoalb_p)    ) allocate(snoalb_p(ims:ime,jms:jme)    )
 if(.not.allocated(snotime_p)   ) allocate(snotime_p(ims:ime,jms:jme)   )
 if(.not.allocated(snopcx_p)    ) allocate(snopcx_p(ims:ime,jms:jme)    )
 if(.not.allocated(snow_p)      ) allocate(snow_p(ims:ime,jms:jme)      )
 if(.not.allocated(snowc_p)     ) allocate(snowc_p(ims:ime,jms:jme)     )
 if(.not.allocated(snowh_p)     ) allocate(snowh_p(ims:ime,jms:jme)     )
 if(.not.allocated(sr_p)        ) allocate(sr_p(ims:ime,jms:jme)        )
 if(.not.allocated(sst_p)       ) allocate(sst_p(ims:ime,jms:jme)       )
 if(.not.allocated(swdown_p)    ) allocate(swdown_p(ims:ime,jms:jme)    )
 if(.not.allocated(tmn_p)       ) allocate(tmn_p(ims:ime,jms:jme)       )
 if(.not.allocated(tsk_p)       ) allocate(tsk_p(ims:ime,jms:jme)       )
 if(.not.allocated(udrunoff_p)  ) allocate(udrunoff_p(ims:ime,jms:jme)  )
 if(.not.allocated(vegfra_p)    ) allocate(vegfra_p(ims:ime,jms:jme)    )
 if(.not.allocated(xice_p)      ) allocate(xice_p(ims:ime,jms:jme)      )
 if(.not.allocated(xland_p)     ) allocate(xland_p(ims:ime,jms:jme)     )
 if(.not.allocated(z0_p)        ) allocate(z0_p(ims:ime,jms:jme)        )
 if(.not.allocated(znt_p)       ) allocate(znt_p(ims:ime,jms:jme)       )
 if(.not.allocated(t2m_p)       ) allocate(t2m_p(ims:ime,jms:jme)       )
 if(.not.allocated(th2m_p)      ) allocate(th2m_p(ims:ime,jms:jme)      )
 if(.not.allocated(q2_p)        ) allocate(q2_p(ims:ime,jms:jme)        )
 if(.not.allocated(flxsnow_p)   ) allocate(flxsnow_p(ims:ime,jms:jme)   )
 if(.not.allocated(fvbsnow_p)   ) allocate(fvbsnow_p(ims:ime,jms:jme)   )
 if(.not.allocated(fbursnow_p)  ) allocate(fbursnow_p(ims:ime,jms:jme)  )
 if(.not.allocated(fgsnsnow_p)  ) allocate(fgsnsnow_p(ims:ime,jms:jme)  )
 if(.not.allocated(frc_urb_p)   ) allocate(frc_urb_p(ims:ime,jms:jme)   )
 if(.not.allocated(ust_urb_p)   ) allocate(ust_urb_p(ims:ime,jms:jme)   )
 if(.not.allocated(utype_urb_p) ) allocate(utype_urb_p(ims:ime,jms:jme) )
 if(.not.allocated(infxsrt_p)   ) allocate(infxsrt_p(ims:ime,jms:jme)   )
 if(.not.allocated(sfcheadrt_p) ) allocate(sfcheadrt_p(ims:ime,jms:jme) )
 if(.not.allocated(soldrain_p)  ) allocate(soldrain_p(ims:ime,jms:jme)  )

!additional arrays for px:
 if(.not.allocated(mavail_p)    ) allocate(mavail_p(ims:ime,jms:jme)    )
 if(.not.allocated(hpbl_p)      ) allocate(hpbl_p(ims:ime,jms:jme)      )
 if(.not.allocated(rmol_p)      ) allocate(rmol_p(ims:ime,jms:jme)      )
 if(.not.allocated(ust_p)       ) allocate(ust_p(ims:ime,jms:jme)       )
 if(.not.allocated(ra_p)        ) allocate(ra_p(ims:ime,jms:jme)        )
 if(.not.allocated(rs_p)        ) allocate(rs_p(ims:ime,jms:jme)        )
 if(.not.allocated(psih_p)      ) allocate(psih_p(ims:ime,jms:jme)      )
 if(.not.allocated(imperv_p)    ) allocate(imperv_p(ims:ime,jms:jme)    )
 if(.not.allocated(canfra_p)    ) allocate(canfra_p(ims:ime,jms:jme)    )
 if(.not.allocated(vegpx_p)     ) allocate(vegpx_p(ims:ime,jms:jme)     )
 if(.not.allocated(lai_modis_p) ) allocate(lai_modis_p(ims:ime,jms:jme) )
 if(.not.allocated(lai_px_p)    ) allocate(lai_px_p(ims:ime,jms:jme)    )
 if(.not.allocated(wwlt_px_p)   ) allocate(wwlt_px_p(ims:ime,jms:jme)   )
 if(.not.allocated(wfc_px_p)    ) allocate(wfc_px_p(ims:ime,jms:jme)    )
 if(.not.allocated(wsat_px_p)   ) allocate(wsat_px_p(ims:ime,jms:jme)   )
 if(.not.allocated(clay_px_p)   ) allocate(clay_px_p(ims:ime,jms:jme)   )
 if(.not.allocated(csand_px_p)  ) allocate(csand_px_p(ims:ime,jms:jme)  )
 if(.not.allocated(fmsand_px_p) ) allocate(fmsand_px_p(ims:ime,jms:jme) )
 if(.not.allocated(t2anl_p)     ) allocate(t2anl_p(ims:ime,jms:jme)     )
 if(.not.allocated(t2anl1_p)    ) allocate(t2anl1_p(ims:ime,jms:jme)    )
 if(.not.allocated(t2anl2_p)    ) allocate(t2anl2_p(ims:ime,jms:jme)    )
 if(.not.allocated(rh2anl_p)    ) allocate(rh2anl_p(ims:ime,jms:jme)    )
 if(.not.allocated(rh2anl1_p)   ) allocate(rh2anl1_p(ims:ime,jms:jme)   )
 if(.not.allocated(rh2anl2_p)   ) allocate(rh2anl2_p(ims:ime,jms:jme)   )
 if(.not.allocated(snoanl_p)    ) allocate(snoanl_p(ims:ime,jms:jme)    )
 if(.not.allocated(snoanl1_p)   ) allocate(snoanl1_p(ims:ime,jms:jme)   )
 if(.not.allocated(snoanl2_p)   ) allocate(snoanl2_p(ims:ime,jms:jme)   )

 if(config_frac_seaice) then
    if(.not.allocated(tsk_sea)   ) allocate(tsk_sea(ims:ime,jms:jme)   )
    if(.not.allocated(tsk_ice)   ) allocate(tsk_ice(ims:ime,jms:jme)   )
    if(.not.allocated(albsi_p)   ) allocate(albsi_p(ims:ime,jms:jme)   )
    if(.not.allocated(icedepth_p)) allocate(icedepth_p(ims:ime,jms:jme))
    if(.not.allocated(snowsi_p)  ) allocate(snowsi_p(ims:ime,jms:jme)  )
 endif

 end subroutine allocate_lsm

!=================================================================================================================
 subroutine deallocate_lsm(config_frac_seaice)
!=================================================================================================================

 logical,intent(in):: config_frac_seaice
!-----------------------------------------------------------------------------------------------------------------

!arrays for soil layer properties:
 if(allocated(dzs_p)   ) deallocate(dzs_p   )
 if(allocated(smcrel_p)) deallocate(smcrel_p)
 if(allocated(sh2o_p)  ) deallocate(sh2o_p  )
 if(allocated(smois_p) ) deallocate(smois_p )
 if(allocated(tslb_p)  ) deallocate(tslb_p  )

!arrays for land use and soil type fractions
 if(allocated(landusef_p)) deallocate(landusef_p)
 if(allocated(soiltypf_p)) deallocate(soiltypf_p)

!other arrays:
 if(allocated(acsnom_p)    ) deallocate(acsnom_p    )
 if(allocated(acsnow_p)    ) deallocate(acsnow_p    )
 if(allocated(canwat_p)    ) deallocate(canwat_p    )
 if(allocated(chs_p)       ) deallocate(chs_p       )
 if(allocated(chs2_p)      ) deallocate(chs2_p      )
 if(allocated(chklowq_p)   ) deallocate(chklowq_p   )
 if(allocated(cpm_p)       ) deallocate(cpm_p       )
 if(allocated(cqs2_p)      ) deallocate(cqs2_p      )
 if(allocated(glw_p)       ) deallocate(glw_p       )
 if(allocated(grdflx_p)    ) deallocate(grdflx_p    )
 if(allocated(gsw_p)       ) deallocate(gsw_p       )
 if(allocated(hfx_p)       ) deallocate(hfx_p       )
 if(allocated(isltyp_p)    ) deallocate(isltyp_p    )
 if(allocated(ivgtyp_p)    ) deallocate(ivgtyp_p    )
 if(allocated(lai_p)       ) deallocate(lai_p       )
 if(allocated(lh_p)        ) deallocate(lh_p        )
 if(allocated(noahres_p)   ) deallocate(noahres_p   )
 if(allocated(potevp_p)    ) deallocate(potevp_p    )
 if(allocated(qfx_p)       ) deallocate(qfx_p       )
 if(allocated(qgh_p)       ) deallocate(qgh_p       )
 if(allocated(qsfc_p)      ) deallocate(qsfc_p      )
 if(allocated(qz0_p)       ) deallocate(qz0_p       )
 if(allocated(rainbl_p)    ) deallocate(rainbl_p    )
 if(allocated(br_p)        ) deallocate(br_p        )
 if(allocated(sfc_albbck_p)) deallocate(sfc_albbck_p)
 if(allocated(sfc_albedo_p)) deallocate(sfc_albedo_p)
 if(allocated(sfc_emibck_p)) deallocate(sfc_emibck_p)
 if(allocated(sfc_emiss_p) ) deallocate(sfc_emiss_p )
 if(allocated(sfcrunoff_p) ) deallocate(sfcrunoff_p )
 if(allocated(shdmin_p)    ) deallocate(shdmin_p    )
 if(allocated(shdmax_p)    ) deallocate(shdmax_p    )
 if(allocated(smstav_p)    ) deallocate(smstav_p    )
 if(allocated(smstot_p)    ) deallocate(smstot_p    )
 if(allocated(snoalb_p)    ) deallocate(snoalb_p    )
 if(allocated(snotime_p)   ) deallocate(snotime_p   )
 if(allocated(snopcx_p)    ) deallocate(snopcx_p    )
 if(allocated(snow_p)      ) deallocate(snow_p      )
 if(allocated(snowc_p)     ) deallocate(snowc_p     )
 if(allocated(snowh_p)     ) deallocate(snowh_p     )
 if(allocated(sr_p)        ) deallocate(sr_p        )
 if(allocated(sst_p)       ) deallocate(sst_p       )
 if(allocated(swdown_p)    ) deallocate(swdown_p    )
 if(allocated(tmn_p)       ) deallocate(tmn_p       )
 if(allocated(tsk_p)       ) deallocate(tsk_p       )
 if(allocated(udrunoff_p)  ) deallocate(udrunoff_p  )
 if(allocated(vegfra_p)    ) deallocate(vegfra_p    )
 if(allocated(xice_p)      ) deallocate(xice_p      )
 if(allocated(xland_p)     ) deallocate(xland_p     )
 if(allocated(z0_p)        ) deallocate(z0_p        )
 if(allocated(znt_p)       ) deallocate(znt_p       )
 if(allocated(t2m_p)       ) deallocate(t2m_p       )
 if(allocated(th2m_p)      ) deallocate(th2m_p      )
 if(allocated(q2_p)        ) deallocate(q2_p        )
 if(allocated(flxsnow_p)   ) deallocate(flxsnow_p   )
 if(allocated(fvbsnow_p)   ) deallocate(fvbsnow_p   )
 if(allocated(fbursnow_p)  ) deallocate(fbursnow_p  )
 if(allocated(fgsnsnow_p)  ) deallocate(fgsnsnow_p  )
 if(allocated(frc_urb_p)   ) deallocate(frc_urb_p   )
 if(allocated(ust_urb_p)   ) deallocate(ust_urb_p   )
 if(allocated(utype_urb_p) ) deallocate(utype_urb_p )
 if(allocated(infxsrt_p)   ) deallocate(infxsrt_p   )
 if(allocated(sfcheadrt_p) ) deallocate(sfcheadrt_p )
 if(allocated(soldrain_p)  ) deallocate(soldrain_p  )

!additional arrays for px:
 if(allocated(mavail_p)    ) deallocate(mavail_p    )
 if(allocated(hpbl_p)      ) deallocate(hpbl_p      )
 if(allocated(rmol_p)      ) deallocate(rmol_p      )
 if(allocated(ust_p)       ) deallocate(ust_p       )
 if(allocated(ra_p)        ) deallocate(ra_p        )
 if(allocated(rs_p)        ) deallocate(rs_p        )
 if(allocated(psih_p)      ) deallocate(psih_p      )
 if(allocated(imperv_p)    ) deallocate(imperv_p    )
 if(allocated(canfra_p)    ) deallocate(canfra_p    )
 if(allocated(vegpx_p)     ) deallocate(vegpx_p     )
 if(allocated(lai_modis_p) ) deallocate(lai_modis_p )
 if(allocated(lai_px_p)    ) deallocate(lai_px_p    )
 if(allocated(wwlt_px_p)   ) deallocate(wwlt_px_p   )
 if(allocated(wfc_px_p)    ) deallocate(wfc_px_p    )
 if(allocated(wsat_px_p)   ) deallocate(wsat_px_p   )
 if(allocated(clay_px_p)   ) deallocate(clay_px_p   )
 if(allocated(csand_px_p)  ) deallocate(csand_px_p  )
 if(allocated(fmsand_px_p) ) deallocate(fmsand_px_p )
 if(allocated(t2anl_p)     ) deallocate(t2anl_p     )
 if(allocated(t2anl1_p)    ) deallocate(t2anl1_p    )
 if(allocated(t2anl2_p)    ) deallocate(t2anl2_p    )
 if(allocated(rh2anl_p)    ) deallocate(rh2anl_p    )
 if(allocated(rh2anl1_p)   ) deallocate(rh2anl1_p   )
 if(allocated(rh2anl2_p)   ) deallocate(rh2anl2_p   )
 if(allocated(snoanl_p)    ) deallocate(snoanl_p    )
 if(allocated(snoanl1_p)   ) deallocate(snoanl1_p   )
 if(allocated(snoanl2_p)   ) deallocate(snoanl2_p   )

 if(config_frac_seaice) then
    if(allocated(chs_sea)   ) deallocate(chs_sea   )
    if(allocated(chs2_sea)  ) deallocate(chs2_sea  )
    if(allocated(cqs2_sea)  ) deallocate(cqs2_sea  )
    if(allocated(cpm_sea)   ) deallocate(cpm_sea   )
    if(allocated(hfx_sea)   ) deallocate(hfx_sea   )
    if(allocated(qfx_sea)   ) deallocate(qfx_sea   )
    if(allocated(qgh_sea)   ) deallocate(qgh_sea   )
    if(allocated(qsfc_sea)  ) deallocate(qsfc_sea  )
    if(allocated(lh_sea)    ) deallocate(lh_sea    )
    if(allocated(tsk_sea)   ) deallocate(tsk_sea   )
    if(allocated(tsk_ice)   ) deallocate(tsk_ice   )
    if(allocated(albsi_p)   ) deallocate(albsi_p   )
    if(allocated(icedepth_p)) deallocate(icedepth_p)
    if(allocated(snowsi_p)  ) deallocate(snowsi_p  )
 endif

 end subroutine deallocate_lsm

!=================================================================================================================
 subroutine lsm_from_MPAS(configs,mesh,diag_physics,sfc_input,its,ite)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: sfc_input
 integer,intent(in):: its,ite

!local pointers:
 logical,pointer:: config_frac_seaice

 character(len=StrKIND),pointer:: config_microp_scheme,    &
                                  config_convection_scheme

 integer,dimension(:),pointer:: isltyp,ivgtyp

 real(kind=RKIND),dimension(:),pointer  :: acsnom,acsnow,canwat,chs,chs2,chklowq,cpm,cqs2,glw,   &
                                           grdflx,gsw,hfx,lai,lh,noahres,potevp,qfx,qgh,qsfc,    &
                                           br,sfc_albedo,sfc_albedo_seaice,sfc_emibck,sfc_emiss, &
                                           sfcrunoff,smstav,smstot,snotime,snopcx,sr,udrunoff,   &
                                           z0,znt
 real(kind=RKIND),dimension(:),pointer  :: shdmin,shdmax,snoalb,sfc_albbck,snow,snowc,snowh,tmn, &
                                           skintemp,sst,vegfra,xice,xland,hpbl,rmol,ust,psih
 real(kind=RKIND),dimension(:),pointer  :: t2m,th2m,q2,t2anl1,t2anl2,rh2anl1,rh2anl2,snoanl1,snoanl2
 real(kind=RKIND),dimension(:),pointer  :: t2anl,rh2anl,snoanl,ra,rs,imperv,canfra,vegpx,mavail
 real(kind=RKIND),dimension(:),pointer  :: lai_modis,lai_px,wwlt_px,wfc_px,wsat_px,clay_px,csand_px,fmsand_px
 real(kind=RKIND),dimension(:),pointer  :: raincv,rainncv
 real(kind=RKIND),dimension(:,:),pointer:: sh2o,smcrel,smois,tslb,dzs,landusef,soiltypf

!local variables and arrays:
 logical:: do_fill
 integer:: ip,iEdg
 integer:: i,j,n

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_frac_seaice'      ,config_frac_seaice      )
 call mpas_pool_get_config(configs,'config_convection_scheme',config_convection_scheme)
 call mpas_pool_get_config(configs,'config_microp_scheme'    ,config_microp_scheme    )

 call mpas_pool_get_array(diag_physics,'acsnom'           ,acsnom           )
 call mpas_pool_get_array(diag_physics,'acsnow'           ,acsnow           )
 call mpas_pool_get_array(diag_physics,'canfra'           ,canfra           )
 call mpas_pool_get_array(diag_physics,'canwat'           ,canwat           )
 call mpas_pool_get_array(diag_physics,'chs'              ,chs              )
 call mpas_pool_get_array(diag_physics,'chs2'             ,chs2             )
 call mpas_pool_get_array(diag_physics,'chklowq'          ,chklowq          )
 call mpas_pool_get_array(diag_physics,'clay_px'          ,clay_px          )
 call mpas_pool_get_array(diag_physics,'cpm'              ,cpm              )
 call mpas_pool_get_array(diag_physics,'cqs2'             ,cqs2             )
 call mpas_pool_get_array(diag_physics,'csand_px'         ,csand_px         )
 call mpas_pool_get_array(diag_physics,'fmsand_px'        ,fmsand_px        )
 call mpas_pool_get_array(diag_physics,'glw'              ,glw              )
 call mpas_pool_get_array(diag_physics,'grdflx'           ,grdflx           )
 call mpas_pool_get_array(diag_physics,'gsw'              ,gsw              )
 call mpas_pool_get_array(diag_physics,'hfx'              ,hfx              )
 call mpas_pool_get_array(diag_physics,'hpbl'             ,hpbl             )
 call mpas_pool_get_array(diag_physics,'imperv'           ,imperv           )
 call mpas_pool_get_array(diag_physics,'lai'              ,lai              )
 call mpas_pool_get_array(diag_physics,'lai_px'           ,lai_px           )
 call mpas_pool_get_array(diag_physics,'lh'               ,lh               )
 call mpas_pool_get_array(diag_physics,'mavail'           ,mavail           )
 call mpas_pool_get_array(diag_physics,'noahres'          ,noahres          )
 call mpas_pool_get_array(diag_physics,'potevp'           ,potevp           )
 call mpas_pool_get_array(diag_physics,'psih'             ,psih             )
 call mpas_pool_get_array(diag_physics,'qfx'              ,qfx              )
 call mpas_pool_get_array(diag_physics,'qgh'              ,qgh              )
 call mpas_pool_get_array(diag_physics,'qsfc'             ,qsfc             )
 call mpas_pool_get_array(diag_physics,'br'               ,br               )
 call mpas_pool_get_array(diag_physics,'ra'               ,ra               )
 call mpas_pool_get_array(diag_physics,'rh2anl1'          ,rh2anl1          )
 call mpas_pool_get_array(diag_physics,'rh2anl2'          ,rh2anl2          )
 call mpas_pool_get_array(diag_physics,'rmol'             ,rmol             )
 call mpas_pool_get_array(diag_physics,'rs'               ,rs               )
 call mpas_pool_get_array(diag_physics,'sfc_albedo'       ,sfc_albedo       )
 call mpas_pool_get_array(diag_physics,'sfc_albedo_seaice',sfc_albedo_seaice)
 call mpas_pool_get_array(diag_physics,'sfc_emibck'       ,sfc_emibck       )
 call mpas_pool_get_array(diag_physics,'sfc_emiss'        ,sfc_emiss        )
 call mpas_pool_get_array(diag_physics,'sfcrunoff'        ,sfcrunoff        )
 call mpas_pool_get_array(diag_physics,'smstav'           ,smstav           )
 call mpas_pool_get_array(diag_physics,'smstot'           ,smstot           )
 call mpas_pool_get_array(diag_physics,'snoanl1'          ,snoanl1          )
 call mpas_pool_get_array(diag_physics,'snoanl2'          ,snoanl2          )
 call mpas_pool_get_array(diag_physics,'snotime'          ,snotime          )
 call mpas_pool_get_array(diag_physics,'snopcx'           ,snopcx           )
 call mpas_pool_get_array(diag_physics,'t2anl1'           ,t2anl1           )
 call mpas_pool_get_array(diag_physics,'t2anl2'           ,t2anl2           )
 call mpas_pool_get_array(diag_physics,'udrunoff'         ,udrunoff         )
 call mpas_pool_get_array(diag_physics,'ust'              ,ust              )
 call mpas_pool_get_array(diag_physics,'vegpx'            ,vegpx            )
 call mpas_pool_get_array(diag_physics,'wfc_px'           ,wfc_px           )
 call mpas_pool_get_array(diag_physics,'wsat_px'          ,wsat_px          )
 call mpas_pool_get_array(diag_physics,'wwlt_px'          ,wwlt_px          )
 call mpas_pool_get_array(diag_physics,'z0'               ,z0               )
 call mpas_pool_get_array(diag_physics,'znt'              ,znt              )
 call mpas_pool_get_array(diag_physics,'t2m'              ,t2m              )
 call mpas_pool_get_array(diag_physics,'th2m'             ,th2m             )
 call mpas_pool_get_array(diag_physics,'q2'               ,q2               )

 call mpas_pool_get_array(sfc_input,'isltyp'    ,isltyp    )
 call mpas_pool_get_array(sfc_input,'ivgtyp'    ,ivgtyp    )
 call mpas_pool_get_array(sfc_input,'lai_modis' ,lai_modis )
 call mpas_pool_get_array(sfc_input,'rh2anl'    ,rh2anl    )
 call mpas_pool_get_array(sfc_input,'shdmin'    ,shdmin    )
 call mpas_pool_get_array(sfc_input,'shdmax'    ,shdmax    )
 call mpas_pool_get_array(sfc_input,'snoalb'    ,snoalb    )
 call mpas_pool_get_array(sfc_input,'sfc_albbck',sfc_albbck)
 call mpas_pool_get_array(sfc_input,'snoanl'    ,snoanl    )
 call mpas_pool_get_array(sfc_input,'snow'      ,snow      )
 call mpas_pool_get_array(sfc_input,'snowc'     ,snowc     )
 call mpas_pool_get_array(sfc_input,'snowh'     ,snowh     )
 call mpas_pool_get_array(sfc_input,'sst'       ,sst       )
 call mpas_pool_get_array(sfc_input,'t2anl'     ,t2anl     )
 call mpas_pool_get_array(sfc_input,'tmn'       ,tmn       )
 call mpas_pool_get_array(sfc_input,'skintemp'  ,skintemp  )
 call mpas_pool_get_array(sfc_input,'vegfra'    ,vegfra    )
 call mpas_pool_get_array(sfc_input,'xice'      ,xice      )
 call mpas_pool_get_array(sfc_input,'xland'     ,xland     )
 call mpas_pool_get_array(sfc_input,'dzs'       ,dzs       )
 call mpas_pool_get_array(sfc_input,'sh2o'      ,sh2o      )
 call mpas_pool_get_array(sfc_input,'smcrel'    ,smcrel    )
 call mpas_pool_get_array(sfc_input,'smois'     ,smois     )
 call mpas_pool_get_array(sfc_input,'tslb'      ,tslb      )
 call mpas_pool_get_array(sfc_input,'landusef'  ,landusef  )
 call mpas_pool_get_array(sfc_input,'soiltypf'  ,soiltypf  )

!In Registry.xml, dzs is a function of nCells. In the Noah lsm scheme, dzs is independent
!of cell locations:
 do n = 1,num_soils
    dzs_p(n) = dzs(n,its)
 enddo

 do j = jts,jte
 do n = 1,num_soils
 do i = its,ite
    sh2o_p(i,n,j)   = sh2o(n,i)
    smcrel_p(i,n,j) = smcrel(n,i)
    smois_p(i,n,j)  = smois(n,i)
    tslb_p(i,n,j)   = tslb(n,i)
 enddo
 enddo
 enddo

 do j = jts,jte
 do n = 1,num_soilc
 do i = its,ite
    soiltypf_p(i,n,j) = soiltypf(n,i)
 enddo
 enddo
 enddo

 do j = jts,jte
 do n = 1,num_landu
 do i = its,ite
    landusef_p(i,n,j) = landusef(n,i)
 enddo
 enddo
 enddo

 do j = jts,jte
 do i = its,ite
    acsnom_p(i,j)     = acsnom(i)
    acsnow_p(i,j)     = acsnow(i)
    canfra_p(i,j)     = canfra(i)
    canwat_p(i,j)     = canwat(i)
    chs_p(i,j)        = chs(i)
    chs2_p(i,j)       = chs2(i)
    chklowq_p(i,j)    = chklowq(i)
    clay_px_p(i,j)    = clay_px(i)
    cpm_p(i,j)        = cpm(i)
    cqs2_p(i,j)       = cqs2(i)
    csand_px_p(i,j)   = csand_px(i)
    fmsand_px_p(i,j)  = fmsand_px(i)
    glw_p(i,j)        = glw(i)
    grdflx_p(i,j)     = grdflx(i)
    gsw_p(i,j)        = gsw(i)
    hfx_p(i,j)        = hfx(i)
    hpbl_p(i,j)       = hpbl(i)
    imperv_p(i,j)     = imperv(i)
    lai_p(i,j)        = lai(i)
    lai_px_p(i,j)     = lai_px(i)
    lh_p(i,j)         = lh(i)
    mavail_p(i,j)     = mavail(i)
    noahres_p(i,j)    = noahres(i)
    potevp_p(i,j)     = potevp(i)
    psih_p(i,j)       = psih(i)
    qfx_p(i,j)        = qfx(i)
    qgh_p(i,j)        = qgh(i)
    qsfc_p(i,j)       = qsfc(i)
    br_p(i,j)         = br(i)
    ra_p(i,j)         = ra(i)
    rh2anl1_p(i,j)    = rh2anl1(i)
    rh2anl2_p(i,j)    = rh2anl2(i)
    rmol_p(i,j)       = rmol(i)
    rs_p(i,j)         = rs(i)
    sfc_albedo_p(i,j) = sfc_albedo(i)
    sfc_emibck_p(i,j) = sfc_emibck(i)
    sfc_emiss_p(i,j)  = sfc_emiss(i)
    sfcrunoff_p(i,j)  = sfcrunoff(i)
    smstav_p(i,j)     = smstav(i)
    smstot_p(i,j)     = smstot(i)
    snoanl1_p(i,j)    = snoanl1(i)
    snoanl2_p(i,j)    = snoanl2(i)
    snotime_p(i,j)    = snotime(i)
    snopcx_p(i,j)     = snopcx(i)
    t2anl1_p(i,j)     = t2anl1(i)
    t2anl2_p(i,j)     = t2anl2(i)
    udrunoff_p(i,j)   = udrunoff(i)
    ust_p(i,j)        = ust(i)
    vegpx_p(i,j)      = vegpx(i)
    wfc_px_p(i,j)     = wfc_px(i)
    wsat_px_p(i,j)    = wsat_px(i)
    wwlt_px_p(i,j)    = wwlt_px(i)
    z0_p(i,j)         = z0(i)
    znt_p(i,j)        = znt(i)
    t2m_p(i,j)        = t2m(i)
    th2m_p(i,j)       = th2m(i)
    q2_p(i,j)         = q2(i)

    isltyp_p(i,j)     = isltyp(i)
    ivgtyp_p(i,j)     = ivgtyp(i)
    lai_modis_p(i,j)  = lai_modis(i)
    rh2anl_p(i,j)     = rh2anl(i)
    shdmin_p(i,j)     = shdmin(i)
    shdmax_p(i,j)     = shdmax(i)
    snoalb_p(i,j)     = snoalb(i)
    snoanl_p(i,j)     = snoanl(i)
    sfc_albbck_p(i,j) = sfc_albbck(i)
    snow_p(i,j)       = snow(i)
    snowc_p(i,j)      = snowc(i)
    snowh_p(i,j)      = snowh(i)
    sst_p(i,j)        = sst(i)
    t2anl_p(i,j)      = t2anl(i)
    tmn_p(i,j)        = tmn(i)
    tsk_p(i,j)        = skintemp(i)
    vegfra_p(i,j)     = vegfra(i)
    xice_p(i,j)       = xice(i)
    xland_p(i,j)      = xland(i)

    qz0_p(i,j)        = 0._RKIND

    !initialization of arrays to run the UA Noah LSM snow cover parameterization:
    flxsnow_p(i,j)    = 0._RKIND
    fvbsnow_p(i,j)    = 0._RKIND
    fbursnow_p(i,j)   = 0._RKIND
    fgsnsnow_p(i,j)   = 0._RKIND

    !initialization of arrays to run the Noah LSM urban parameterization (not currently
    frc_urb_p(i,j)    = 0._RKIND
    ust_urb_p(i,j)    = 0._RKIND
    utype_urb_p(i,j)  = low_density_residential

    !initialization of arrays to run the Noah LSM hydrological parameterization (not currently
    !implemented in MPAS):
    infxsrt_p(i,j)    = 0._RKIND
    sfcheadrt_p(i,j)  = 0._RKIND
    soldrain_p(i,j)   = 0._RKIND
 enddo
 enddo

 if(config_frac_seaice) then
    do j = jts,jte
    do i = its,ite
       !modify the surface albedo and surface emissivity, and surface temperatures over sea-ice points:
       if(xice(i).ge.xice_threshold .and. xice(i).le.1._RKIND) then
          sfc_albedo_p(i,j) = (sfc_albedo(i) - 0.08_RKIND*(1._RKIND-xice(i))) / xice(i)
          sfc_emiss_p(i,j)  = (sfc_emiss(i) - 0.98_RKIND*(1._RKIND-xice(i))) / xice(i)
       else
          sfc_albedo_p(i,j) = sfc_albedo(i)
          sfc_emiss_p(i,j)  = sfc_emiss(i)
       endif
    enddo
    enddo

    !calculate sea-surface and sea-ice temperatures over sea-ice grid cells:
    call correct_tsk_over_seaice(ims,ime,jms,jme,its,ite,jts,jte,xice_threshold,xice_p, &
                                 tsk_p,tsk_sea,tsk_ice)

    do j = jts,jte
    do i = its,ite
       tsk_p(i,j) = tsk_ice(i,j)
    enddo
    enddo

    !initialize the surface albedo, the surface albedo over snow-covered seaice, and the
    !seaice thickness.
    do j = jts,jte
    do i = its,ite
       albsi_p(i,j)    = sfc_albedo_seaice(i)
       icedepth_p(i,j) = seaice_thickness_default
       snowsi_p(i,j)   = seaice_snowdepth_min
    enddo
    enddo
 endif

 do j = jts,jte
 do i = its,ite
    sr_p(i,j)     = 0._RKIND
    rainbl_p(i,j) = 0._RKIND
 enddo
 enddo

 if(config_microp_scheme .ne. 'off') then
    call mpas_pool_get_array(diag_physics,'sr'     ,sr     ) 
    call mpas_pool_get_array(diag_physics,'rainncv',rainncv)

    do j = jts,jte
    do i = its,ite
       sr_p(i,j) = sr(i)
       rainbl_p(i,j) = rainbl_p(i,j) + rainncv(i)
    enddo
    enddo
 endif
 if(config_convection_scheme .ne. 'off') then
    call mpas_pool_get_array(diag_physics,'raincv',raincv)

    do j = jts,jte
    do i = its,ite
       rainbl_p(i,j) = rainbl_p(i,j) + raincv(i)
    enddo
    enddo
 endif

 do j = jts,jte
 do i = its,ite
    swdown_p(i,j) = gsw(i) / (1._RKIND - sfc_albedo(i))
 enddo
 enddo

 end subroutine lsm_from_MPAS
 
!=================================================================================================================
 subroutine lsm_to_MPAS(configs,mesh,diag_physics,sfc_input,its,ite)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: sfc_input

 integer,intent(in):: its,ite

!local pointers:
 logical,pointer:: config_frac_seaice

 character(len=StrKIND),pointer:: config_microp_scheme

 integer,dimension(:),pointer:: isltyp,ivgtyp

 real(kind=RKIND),dimension(:),pointer  :: acsnom,acsnow,canwat,chs,chs2,chklowq,cpm,cqs2,glw, &
                                           grdflx,gsw,hfx,lai,lh,noahres,potevp,qfx,qgh,qsfc,  &
                                           br,sfc_albedo,sfc_emibck,sfc_emiss,sfcrunoff,       &
                                           smstav,smstot,snotime,snopcx,sr,udrunoff,z0,znt
 real(kind=RKIND),dimension(:),pointer  :: shdmin,shdmax,snoalb,sfc_albbck,snow,snowc,snowh,tmn, &
                                           skintemp,sst,vegfra,xice,xland,hpbl,rmol,ust,psih
 real(kind=RKIND),dimension(:),pointer  :: t2m,th2m,q2,t2anl1,t2anl2,rh2anl1,rh2anl2,snoanl1,snoanl2
 real(kind=RKIND),dimension(:),pointer  :: t2anl,rh2anl,snoanl,ra,rs,imperv,canfra,vegpx,mavail
 real(kind=RKIND),dimension(:),pointer  :: lai_modis,lai_px,wwlt_px,wfc_px,wsat_px,clay_px,csand_px,fmsand_px
 real(kind=RKIND),dimension(:),pointer  :: raincv,rainncv
 real(kind=RKIND),dimension(:,:),pointer:: sh2o,smcrel,smois,tslb,landusef,soiltypf

!local variables and arrays:
 integer:: ip,iEdg
 integer:: i,j,n

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_frac_seaice'  ,config_frac_seaice  )
 call mpas_pool_get_config(configs,'config_microp_scheme',config_microp_scheme)

 call mpas_pool_get_array(diag_physics,'acsnom'    ,acsnom    )
 call mpas_pool_get_array(diag_physics,'acsnow'    ,acsnow    )
 call mpas_pool_get_array(diag_physics,'canfra'    ,canfra    )
 call mpas_pool_get_array(diag_physics,'canwat'    ,canwat    )
 call mpas_pool_get_array(diag_physics,'chs'       ,chs       )
 call mpas_pool_get_array(diag_physics,'chs2'      ,chs2      )
 call mpas_pool_get_array(diag_physics,'chklowq'   ,chklowq   )
 call mpas_pool_get_array(diag_physics,'clay_px'   ,clay_px   )
 call mpas_pool_get_array(diag_physics,'cpm'       ,cpm       )
 call mpas_pool_get_array(diag_physics,'cqs2'      ,cqs2      )
 call mpas_pool_get_array(diag_physics,'csand_px'  ,csand_px  )
 call mpas_pool_get_array(diag_physics,'fmsand_px' ,fmsand_px )
 call mpas_pool_get_array(diag_physics,'glw'       ,glw       )
 call mpas_pool_get_array(diag_physics,'grdflx'    ,grdflx    )
 call mpas_pool_get_array(diag_physics,'gsw'       ,gsw       )
 call mpas_pool_get_array(diag_physics,'hfx'       ,hfx       )
 call mpas_pool_get_array(diag_physics,'hpbl'      ,hpbl      )
 call mpas_pool_get_array(diag_physics,'imperv'    ,imperv    )
 call mpas_pool_get_array(diag_physics,'lai'       ,lai       )
 call mpas_pool_get_array(diag_physics,'lai_px'    ,lai_px    )
 call mpas_pool_get_array(diag_physics,'lh'        ,lh        )
 call mpas_pool_get_array(diag_physics,'mavail'    ,mavail    )
 call mpas_pool_get_array(diag_physics,'noahres'   ,noahres   )
 call mpas_pool_get_array(diag_physics,'potevp'    ,potevp    )
 call mpas_pool_get_array(diag_physics,'psih'      ,psih      )
 call mpas_pool_get_array(diag_physics,'qfx'       ,qfx       )
 call mpas_pool_get_array(diag_physics,'qgh'       ,qgh       )
 call mpas_pool_get_array(diag_physics,'qsfc'      ,qsfc      )
 call mpas_pool_get_array(diag_physics,'br'        ,br        )
 call mpas_pool_get_array(diag_physics,'ra'        ,ra        )
 call mpas_pool_get_array(diag_physics,'raincv'    ,raincv    )
 call mpas_pool_get_array(diag_physics,'rainncv'   ,rainncv   )
 call mpas_pool_get_array(diag_physics,'rh2anl1'   ,rh2anl1   )
 call mpas_pool_get_array(diag_physics,'rh2anl2'   ,rh2anl2   )
 call mpas_pool_get_array(diag_physics,'rmol'      ,rmol      )
 call mpas_pool_get_array(diag_physics,'rs'        ,rs        )
 call mpas_pool_get_array(diag_physics,'sfc_albedo',sfc_albedo)
 call mpas_pool_get_array(diag_physics,'sfc_emibck',sfc_emibck)
 call mpas_pool_get_array(diag_physics,'sfc_emiss' ,sfc_emiss )
 call mpas_pool_get_array(diag_physics,'sfcrunoff' ,sfcrunoff )
 call mpas_pool_get_array(diag_physics,'smstav'    ,smstav    )
 call mpas_pool_get_array(diag_physics,'smstot'    ,smstot    )
 call mpas_pool_get_array(diag_physics,'snoanl1'   ,snoanl1   )
 call mpas_pool_get_array(diag_physics,'snoanl2'   ,snoanl2   )
 call mpas_pool_get_array(diag_physics,'snotime'   ,snotime   )
 call mpas_pool_get_array(diag_physics,'snopcx'    ,snopcx    )
 call mpas_pool_get_array(diag_physics,'t2anl1'    ,t2anl1    )
 call mpas_pool_get_array(diag_physics,'t2anl2'    ,t2anl2    )
 call mpas_pool_get_array(diag_physics,'udrunoff'  ,udrunoff  )
 call mpas_pool_get_array(diag_physics,'ust'       ,ust       )
 call mpas_pool_get_array(diag_physics,'vegpx'     ,vegpx     )
 call mpas_pool_get_array(diag_physics,'wfc_px'    ,wfc_px    )
 call mpas_pool_get_array(diag_physics,'wsat_px'   ,wsat_px   )
 call mpas_pool_get_array(diag_physics,'wwlt_px'   ,wwlt_px   )
 call mpas_pool_get_array(diag_physics,'z0'        ,z0        )
 call mpas_pool_get_array(diag_physics,'znt'       ,znt       )
 call mpas_pool_get_array(diag_physics,'t2m'       ,t2m       )
 call mpas_pool_get_array(diag_physics,'th2m'      ,th2m      )
 call mpas_pool_get_array(diag_physics,'q2'        ,q2        )

 call mpas_pool_get_array(sfc_input,'isltyp'       ,isltyp    )
 call mpas_pool_get_array(sfc_input,'ivgtyp'       ,ivgtyp    )
 call mpas_pool_get_array(sfc_input,'lai_modis'    ,lai_modis )
 call mpas_pool_get_array(sfc_input,'rh2anl'       ,rh2anl    )
 call mpas_pool_get_array(sfc_input,'shdmin'       ,shdmin    )
 call mpas_pool_get_array(sfc_input,'shdmax'       ,shdmax    )
 call mpas_pool_get_array(sfc_input,'snoalb'       ,snoalb    )
 call mpas_pool_get_array(sfc_input,'sfc_albbck'   ,sfc_albbck)
 call mpas_pool_get_array(sfc_input,'snoanl'       ,snoanl    )
 call mpas_pool_get_array(sfc_input,'snow'         ,snow      )
 call mpas_pool_get_array(sfc_input,'snowc'        ,snowc     )
 call mpas_pool_get_array(sfc_input,'snowh'        ,snowh     )
 call mpas_pool_get_array(sfc_input,'sst'          ,sst       )
 call mpas_pool_get_array(sfc_input,'t2anl'        ,t2anl     )
 call mpas_pool_get_array(sfc_input,'tmn'          ,tmn       )
 call mpas_pool_get_array(sfc_input,'skintemp'     ,skintemp  )
 call mpas_pool_get_array(sfc_input,'vegfra'       ,vegfra    )
 call mpas_pool_get_array(sfc_input,'xice'         ,xice      )
 call mpas_pool_get_array(sfc_input,'xland'        ,xland     )
 call mpas_pool_get_array(sfc_input,'sh2o'         ,sh2o      )
 call mpas_pool_get_array(sfc_input,'smcrel'       ,smcrel    )
 call mpas_pool_get_array(sfc_input,'smois'        ,smois     )
 call mpas_pool_get_array(sfc_input,'tslb'         ,tslb      )
 call mpas_pool_get_array(sfc_input,'landusef'     ,landusef  )
 call mpas_pool_get_array(sfc_input,'soiltypf'     ,soiltypf  )

 do j = jts,jte
 do n = 1,num_soils
 do i = its,ite
    sh2o(n,i)   = sh2o_p(i,n,j)
    smcrel(n,i) = smcrel_p(i,n,j)
    smois(n,i)  = smois_p(i,n,j)
    tslb(n,i)   = tslb_p(i,n,j)
 enddo
 enddo
 enddo

 do j = jts,jte
 do n = 1,num_soilc
 do i = its,ite
    soiltypf(n,i)   = soiltypf_p(i,n,j)
 enddo
 enddo
 enddo

 do j = jts,jte
 do n = 1,num_landu
 do i = its,ite
    landusef(n,i)   = landusef_p(i,n,j)
 enddo
 enddo
 enddo

 do j = jts,jte
 do i = its,ite
    acsnom(i)     = acsnom_p(i,j)
    acsnow(i)     = acsnow_p(i,j)
    canfra(i)     = canfra_p(i,j)
    canwat(i)     = canwat_p(i,j)
    chs(i)        = chs_p(i,j)
    chs2(i)       = chs2_p(i,j)
    chklowq(i)    = chklowq_p(i,j)
    clay_px(i)    = clay_px_p(i,j)
    cpm(i)        = cpm_p(i,j)
    cqs2(i)       = cqs2_p(i,j)
    csand_px(i)   = csand_px_p(i,j)
    fmsand_px(i)  = fmsand_px_p(i,j)
    glw(i)        = glw_p(i,j)
    grdflx(i)     = grdflx_p(i,j)
    gsw(i)        = gsw_p(i,j)
    hfx(i)        = hfx_p(i,j)
    hpbl(i)       = hpbl_p(i,j)
    imperv(i)     = imperv_p(i,j)
    lai(i)        = lai_p(i,j)
    lai_px(i)     = lai_px_p(i,j)
    lh(i)         = lh_p(i,j)
    mavail(i)     = mavail_p(i,j)
    noahres(i)    = noahres_p(i,j)
    potevp(i)     = potevp_p(i,j)
    psih(i)       = psih_p(i,j)
    qfx(i)        = qfx_p(i,j)
    qgh(i)        = qgh_p(i,j)
    qsfc(i)       = qsfc_p(i,j)
    br(i)         = br_p(i,j)
    ra(i)         = ra_p(i,j)
    rh2anl1(i)    = rh2anl1_p(i,j)
    rh2anl2(i)    = rh2anl2_p(i,j)
    rmol(i)       = rmol_p(i,j)
    rs(i)         = rs_p(i,j)
    sfc_albedo(i) = sfc_albedo_p(i,j)
    sfc_emibck(i) = sfc_emibck_p(i,j)
    sfc_emiss(i)  = sfc_emiss_p(i,j)
    sfcrunoff(i)  = sfcrunoff_p(i,j)
    smstav(i)     = smstav_p(i,j)
    smstot(i)     = smstot_p(i,j)
    snoanl1(i)    = snoanl1_p(i,j)
    snoanl2(i)    = snoanl2_p(i,j)
    snotime(i)    = snotime_p(i,j)
    snopcx(i)     = snopcx_p(i,j)
    t2anl1(i)     = t2anl1_p(i,j)
    t2anl2(i)     = t2anl2_p(i,j)
    udrunoff(i)   = udrunoff_p(i,j)
    ust(i)        = ust_p(i,j)
    vegpx(i)      = vegpx_p(i,j)
    wfc_px(i)     = wfc_px_p(i,j)
    wsat_px(i)    = wsat_px_p(i,j)
    wwlt_px(i)    = wwlt_px_p(i,j)
    z0(i)         = z0_p(i,j)
    znt(i)        = znt_p(i,j)
    t2m(i)        = t2m_p(i,j)
    th2m(i)       = th2m_p(i,j)
    q2(i)         = q2_p(i,j)

    isltyp(i)     = isltyp_p(i,j)
    ivgtyp(i)     = ivgtyp_p(i,j)
    lai_modis(i)  = lai_modis_p(i,j)
    rh2anl(i)     = rh2anl_p(i,j)
    shdmin(i)     = shdmin_p(i,j)
    shdmax(i)     = shdmax_p(i,j)
    snoalb(i)     = snoalb_p(i,j)
    sfc_albbck(i) = sfc_albbck_p(i,j)
    snoanl(i)     = snoanl_p(i,j)
    snow(i)       = snow_p(i,j)
    snowc(i)      = snowc_p(i,j)
    snowh(i)      = snowh_p(i,j)
    skintemp(i)   = tsk_p(i,j)
    sst(i)        = sst_p(i,j)
    t2anl(i)      = t2anl_p(i,j)
    tmn(i)        = tmn_p(i,j)
    vegfra(i)     = vegfra_p(i,j)
    xice(i)       = xice_p(i,j)
    xland(i)      = xland_p(i,j)
 enddo
 enddo

 if(config_frac_seaice) then
    do j = jts,jte
    do i = its,ite
       if(xice_p(i,j).ge.xice_threshold .and. xice_p(i,j).le.1._RKIND) then
          chs(i)        = xice_p(i,j)*chs_p(i,j)  + (1._RKIND-xice_p(i,j))*chs_sea(i,j)
          chs2(i)       = xice_p(i,j)*chs2_p(i,j) + (1._RKIND-xice_p(i,j))*chs2_sea(i,j)
          cqs2(i)       = xice_p(i,j)*cqs2_p(i,j) + (1._RKIND-xice_p(i,j))*cqs2_sea(i,j)
          cpm(i)        = xice_p(i,j)*cpm_p(i,j)  + (1._RKIND-xice_p(i,j))*cpm_sea(i,j)
          hfx(i)        = xice_p(i,j)*hfx_p(i,j)  + (1._RKIND-xice_p(i,j))*hfx_sea(i,j)
          lh(i)         = xice_p(i,j)*lh_p(i,j)   + (1._RKIND-xice_p(i,j))*lh_sea(i,j)
          qfx(i)        = xice_p(i,j)*qfx_p(i,j)  + (1._RKIND-xice_p(i,j))*qfx_sea(i,j)
          qgh(i)        = xice_p(i,j)*qgh_p(i,j)  + (1._RKIND-xice_p(i,j))*qgh_sea(i,j)
          qsfc(i)       = xice_p(i,j)*qsfc_p(i,j) + (1._RKIND-xice_p(i,j))*qsfc_sea(i,j)
          skintemp(i)   = xice_p(i,j)*tsk_p(i,j)  + (1._RKIND-xice_p(i,j))*tsk_sea(i,j)
          sfc_albedo(i) = xice_p(i,j)*sfc_albedo_p(i,j) + (1._RKIND-xice_p(i,j))*0.08_RKIND
          sfc_emiss(i)  = xice_p(i,j)*sfc_emiss_p(i,j)  + (1._RKIND-xice_p(i,j))*0.98_RKIND
       endif
    enddo
    enddo
 endif

 if(config_microp_scheme .ne. 'off') then
    call mpas_pool_get_array(diag_physics,'sr',sr) 

    do j = jts,jte
    do i = its,ite
       sr(i) = sr_p(i,j)
    enddo
    enddo
 endif

 end subroutine lsm_to_MPAS
 
!=================================================================================================================
 subroutine init_lsm(dminfo,mesh,configs,diag_physics,sfc_input)
!=================================================================================================================

!input arguments:
 type(dm_info),intent(in):: dminfo
 type(mpas_pool_type):: mesh
 type(mpas_pool_type),intent(in):: configs

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: sfc_input

!local pointers:
 character(len=StrKIND),pointer:: lsm_scheme

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_config(configs,'config_lsm_scheme',lsm_scheme)

 lsm_select: select case (trim(lsm_scheme))

    case ("noah")
       call noah_init_forMPAS(dminfo,mesh,configs,diag_physics,sfc_input)

    case ("px")
       call noah_init_forMPAS(dminfo,mesh,configs,diag_physics,sfc_input) !may be customized for PX in the future
    
    case default
 
 end select lsm_select

 end subroutine init_lsm

!=================================================================================================================
 subroutine driver_lsm(itimestep,configs,mesh,diag_physics,sfc_input,its,ite)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: configs

 integer,intent(in):: its,ite
 integer,intent(in):: itimestep

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: sfc_input

!local pointers:
 logical,pointer               :: config_sfc_albedo,config_frac_seaice
 logical,pointer               :: config_do_restart
 character(len=StrKIND),pointer:: lsm_scheme
 character(len=StrKIND),pointer:: mminlu
 character(len=StrKIND),pointer:: config_landuse_data
 character(len=StrKIND),pointer:: config_soilndg_interval
 integer,pointer               :: config_px_soilndg
 integer,pointer               :: config_px_smoisinit
 integer,pointer               :: config_px_modis_veg
 integer,pointer               :: isice

 integer :: nlcat

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write('')
!call mpas_log_write('--- enter subroutine driver_lsm:')

 call mpas_pool_get_config(configs, 'config_sfc_albedo' ,config_sfc_albedo )
 call mpas_pool_get_config(configs, 'config_frac_seaice',config_frac_seaice)
 call mpas_pool_get_config(configs, 'config_lsm_scheme',lsm_scheme)
 call mpas_pool_get_config(configs, 'config_landuse_data',config_landuse_data )
 call mpas_pool_get_config(configs, 'config_soilndg_interval',config_soilndg_interval )
 call mpas_pool_get_config(configs, 'config_px_soilndg',config_px_soilndg )
 call mpas_pool_get_config(configs, 'config_px_smoisinit',config_px_smoisinit )
 call mpas_pool_get_config(configs, 'config_px_modis_veg',config_px_modis_veg )
 call mpas_pool_get_config(configs, 'config_do_restart',config_do_restart)
 call mpas_pool_get_array(sfc_input,'mminlu',mminlu)
 call mpas_pool_get_array(sfc_input,'isice' ,isice )

!copy MPAS arrays to local arrays:
 call lsm_from_MPAS(configs,mesh,diag_physics,sfc_input,its,ite)
!write(0,*) '--- end lsm_from_MPAS'

!call to land-surface scheme:
 lsm_select: select case (trim(lsm_scheme))

    case("noah")
       call mpas_timer_start('Noah')
       call lsm( &
                dz8w        = dz_p        , p8w3d       = pres2_hyd_p  , t3d       = t_p          , &
                qv3d        = qv_p        , xland       = xland_p      , xice      = xice_p       , &
                ivgtyp      = ivgtyp_p    , isltyp      = isltyp_p     , tmn       = tmn_p        , &
                vegfra      = vegfra_p    , shdmin      = shdmin_p     , shdmax    = shdmax_p     , &
                snoalb      = snoalb_p    , glw         = glw_p        , gsw       = gsw_p        , &
                swdown      = swdown_p    , rainbl      = rainbl_p     , embck     = sfc_emibck_p , &
                sr          = sr_p        , qgh         = qgh_p        , cpm       = cpm_p        , &
                qz0         = qz0_p       , tsk         = tsk_p        , hfx       = hfx_p        , &
                qfx         = qfx_p       , lh          = lh_p         , grdflx    = grdflx_p     , &
                qsfc        = qsfc_p      , cqs2        = cqs2_p       , chs       = chs_p        , &
                chs2        = chs2_p      , snow        = snow_p       , snowc     = snowc_p      , &
                snowh       = snowh_p     , canwat      = canwat_p     , smstav    = smstav_p     , &
                smstot      = smstot_p    , sfcrunoff   = sfcrunoff_p  , udrunoff  = udrunoff_p   , &
                acsnom      = acsnom_p    , acsnow      = acsnow_p     , snotime   = snotime_p    , &
                snopcx      = snopcx_p    , emiss       = sfc_emiss_p  , rib       = br_p         , &
                potevp      = potevp_p    , albedo      = sfc_albedo_p , albbck    = sfc_albbck_p , &
                z0          = z0_p        , znt         = znt_p        , lai       = lai_p        , &
                noahres     = noahres_p   , chklowq     = chklowq_p    , sh2o      = sh2o_p       , &
                smois       = smois_p     , tslb        = tslb_p       , smcrel    = smcrel_p     , &
                dzs         = dzs_p       , isurban     = isurban      , isice     = isice        , &
                rovcp       = rcp         , dt          = dt_pbl       , myj       = myj          , &
                itimestep   = itimestep   , frpcpn      = frpcpn       , rdlai2d   = rdlai2d      , &
                opt_thcnd   = opt_thcnd   , ua_phys     = ua_phys      , flx4_2d   = flxsnow_p    , &
                fvb_2d      = fvbsnow_p   , fbur_2d     = fbursnow_p   , fgsn_2d   = fgsnsnow_p   , &
                utype_urb2d = utype_urb_p , frc_urb2d   = frc_urb_p    , ust_urb2d = ust_urb_p    , &
                infxsrt     = infxsrt_p   , sfcheadrt   = sfcheadrt_p  , soldrain  = soldrain_p   , &
                fasdas      = fasdas      , julian      = 0            , julyr     = 0            , &
                xice_threshold   = xice_threshold    ,                                              &
                usemonalb        = config_sfc_albedo ,                                              &
                mminlu           = mminlu ,                                                         &
                num_soil_layers  = num_soils ,                                                      &
                num_roof_layers  = num_soils ,                                                      &
                num_wall_layers  = num_soils ,                                                      &
                num_road_layers  = num_soils ,                                                      &
                num_urban_layers = num_soils ,                                                      &
                num_urban_hi     = num_soils ,                                                      &
                sf_urban_physics = sf_urban_physics ,                                               &
                ids = ids , ide = ide , jds = jds , jde = jde , kds = kds , kde = kde ,             &
                ims = ims , ime = ime , jms = jms , jme = jme , kms = kms , kme = kme ,             &
                its = its , ite = ite , jts = jts , jte = jte , kts = kts , kte = kte               &
               )

       if(config_frac_seaice) then
          call seaice_noah( &
                dz8w      = dz_p        , p8w3d   = pres2_hyd_p  , t3d      = t_p      , &
                qv3d      = qv_p        , xice    = xice_p       , snoalb2d = snoalb_p , &
                glw       = glw_p       , swdown  = swdown_p     , rainbl   = rainbl_p , &
                sr        = sr_p        , qgh     = qgh_p        , tsk      = tsk_p    , &
                hfx       = hfx_p       , qfx     = qfx_p        , lh       = lh_p     , &
                grdflx    = grdflx_p    , potevp  = potevp_p     , qsfc     = qsfc_p   , &
                emiss     = sfc_emiss_p , albedo  = sfc_albedo_p , rib      = br_p     , &
                cqs2      = cqs2_p      , chs     = chs_p        , chs2     = chs2_p   , &
                z02d      = z0_p        , znt     = znt_p        , tslb     = tslb_p   , &
                snow      = snow_p      , snowc   = snowc_p      , snowh2d  = snowh_p  , &
                snopcx    = snopcx_p    , acsnow  = acsnow_p     , acsnom   = acsnom_p , &
                sfcrunoff = sfcrunoff_p , albsi   = albsi_p      , snowsi   = snowsi_p , &
                icedepth  = icedepth_p  , noahres = noahres_p    , dt       = dt_pbl   , &
                frpcpn    = frpcpn      ,                                                &
                seaice_albedo_opt        = seaice_albedo_opt        ,                    &
                seaice_albedo_default    = seaice_albedo_default    ,                    &
                seaice_thickness_opt     = seaice_thickness_opt     ,                    &
                seaice_thickness_default = seaice_thickness_default ,                    &
                seaice_snowdepth_opt     = seaice_snowdepth_opt     ,                    &
                seaice_snowdepth_max     = seaice_snowdepth_max     ,                    &
                seaice_snowdepth_min     = seaice_snowdepth_min     ,                    &
                xice_threshold           = xice_threshold           ,                    &
                num_soil_layers          = num_soils                ,                    &
                sf_urban_physics         = sf_urban_physics         ,                    &
                ids = ids , ide = ide , jds = jds , jde = jde , kds = kds , kde = kde ,  &
                ims = ims , ime = ime , jms = jms , jme = jme , kms = kms , kme = kme ,  &
                its = its , ite = ite , jts = jts , jte = jte , kts = kts , kte = kte    &
                          )
       endif

       call sfcdiags( &
                hfx   = hfx_p  , qfx     = qfx_p   , tsk  = tsk_p , qsfc = qsfc_p , chs = chs_p , &
                chs2  = chs2_p , cqs2    = cqs2_p  , t2   = t2m_p , th2  = th2m_p , q2  = q2_p  , &
                psfc  = psfc_p , t3d     = t_p     , qv3d = qv_p  , cp   = cp     , R_d = R_d   , &
                rovcp = rcp    , ua_phys = ua_phys                                              , &
                ids = ids , ide = ide , jds = jds , jde = jde , kds = kds , kde = kde ,           &
                ims = ims , ime = ime , jms = jms , jme = jme , kms = kms , kme = kme ,           &
                its = its , ite = ite , jts = jts , jte = jte , kts = kts , kte = kte             &
                    )
       call mpas_timer_stop('Noah')

    case("px")
       call mpas_timer_start('PX')
       call pxlsm( &
                dz8w      = dz_p      , qv3d      = qv_p         , t3d       = t_p          , &
                th3d      = th_p      , rho       = rho_p        , psfc      = psfc_p       , &  
                gsw       = gsw_p     , glw       = glw_p        , rainbl    = rainbl_p     , &
                emiss     =sfc_emiss_p, itimestep = itimestep    , restart =config_do_restart,&
                nsoil     = num_soils , anal_intervalc=config_soilndg_interval              , &
                xland     = xland_p   , xice      = xice_p       , albedo    = sfc_albedo_p , &
                snoalb    = snoalb_p  , smois     = smois_p      , tslb      = tslb_p       , &
                mavail    = mavail_p  , ta2       = t2m_p        , qa2       = q2_p         , &
                qsfc      = qsfc_p    , dzs       = dzs_p        , psih      = psih_p       , &
                landusef  = landusef_p, soilcbot  = soiltypf_p   ,                            &
                vegfra    = vegfra_p  , vegf_px   = vegpx_p      , ivgtyp    = ivgtyp_p     , &
                isltyp    = isltyp_p  , ra        = ra_p         , rs        = rs_p         , &
                lai       = lai_modis_p,imperv    = imperv_p     , canfra    = canfra_p     , &
                nlcat     = num_landu , nscat     = num_soilc    ,                            &
                hfx       = hfx_p     , qfx       = qfx_p        ,                            &
                lh        = lh_p      , tsk       = tsk_p        , sst       = sst_p        , &
                znt       = znt_p     , canwat    = canwat_p     , grdflx    = grdflx_p     , &
                shdmin    = shdmin_p  , shdmax    = shdmax_p     , snowc     = snowc_p      , &
                pblh      = hpbl_p    , rmol      = rmol_p       , ust       = ust_p        , &
                dtbl      = dt_pbl    , t2_ndg_old= t2anl1_p     , t2_ndg_new= t2anl2_p     , &
                q2_ndg_old= rh2anl1_p , q2_ndg_new= rh2anl2_p    , sn_ndg_old= snoanl1_p    , &
                sn_ndg_new= snoanl2_p , snow      = snow_p       , snowh     = snowh_p      , &
                snowncv   = acsnow_p  , t2obs     = t2anl_p      , q2obs     = rh2anl_p     , &
                pxlsm_smois_init = config_px_smoisinit, pxlsm_soil_nudge = config_px_soilndg, &   
                pxlsm_modis_veg  = config_px_modis_veg           ,                            &
                lai_px    = lai_px_p  ,                                                       & 
                wwlt_px   = wwlt_px_p , wfc_px    = wfc_px_p     , wsat_px   = wsat_px_p    , & 
                clay_px   = clay_px_p , csand_px  = csand_px_p   , fmsand_px = fmsand_px_p  , &
                ids = ids , ide = ide , jds = jds , jde = jde , kds = kds , kde = kde       , &
                ims = ims , ime = ime , jms = jms , jme = jme , kms = kms , kme = kme       , &
                its = its , ite = ite , jts = jts , jte = jte , kts = kts , kte = kte         &
               )                 
!       if(config_frac_seaice) then
!          call seaice_px( )        ! Future site of PX-compatible handling of fractional sea-ice
!       endif
       call mpas_timer_stop('PX')

    case default

 end select lsm_select

!copy local arrays to MPAS grid:
 call lsm_to_MPAS(configs,mesh,diag_physics,sfc_input,its,ite)

!call mpas_log_write('--- end subroutine driver_lsm.')

 end subroutine driver_lsm

!=================================================================================================================
 subroutine correct_tsk_over_seaice(ims,ime,jms,jme,its,ite,jts,jte,xice_thresh,xice,tsk,tsk_sea,tsk_ice)
!=================================================================================================================

!input arguments:
 integer,intent(in):: ims,ime,its,ite,jms,jme,jts,jte
 real(kind=RKIND),intent(in):: xice_thresh
 real(kind=RKIND),intent(in),dimension(ims:ime,jms:jme):: tsk,xice

!inout arguments:
 real(kind=RKIND),intent(inout),dimension(ims:ime,jms:jme):: tsk_sea,tsk_ice

!local variables:
 integer:: i,j

!-----------------------------------------------------------------------------------------------------------------

!initialize the local sea-surface temperature and local sea-ice temperature to the local surface
!temperature:
 do j = jts,jte
 do i = its,ite
    tsk_sea(i,j) = tsk(i,j)
    tsk_ice(i,j) = tsk(i,j)

    if(xice(i,j).ge.xice_thresh .and. xice(i,j).le.1._RKIND) then
       !over sea-ice grid cells, limit sea-surface temperatures to temperatures warmer than 271.4:
       tsk_sea(i,j) = max(tsk_sea(i,j),271.4_RKIND)

       !over sea-ice grid cells, avoids unphysically too cold sea-ice temperatures for grid cells
       !with small sea-ice fractions:
       if(xice(i,j).lt.0.2_RKIND .and. tsk_ice(i,j).lt.253.15_RKIND) tsk_ice(i,j) = 253.15_RKIND
       if(xice(i,j).lt.0.1_RKIND .and. tsk_ice(i,j).lt.263.15_RKIND) tsk_ice(i,j) = 263.15_RKIND
    endif
 enddo
 enddo

 end subroutine correct_tsk_over_seaice

!=================================================================================================================
 end module mpas_atmphys_driver_lsm
!=================================================================================================================
