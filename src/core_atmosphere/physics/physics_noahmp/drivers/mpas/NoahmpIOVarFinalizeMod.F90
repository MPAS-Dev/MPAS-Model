module NoahmpIOVarFinalizeMod

!!! Initialize Noah-MP input/output variables
!!! Input/Output variables should be first defined in NoahmpIOVarType.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType

  implicit none

contains

!=== initialize with default values

  subroutine NoahmpIOVarFinalizeDefault(NoahmpIO)

    implicit none

    type(NoahmpIO_type), intent(inout) :: NoahmpIO
   
! ------------------------------------------------- 
    associate(                               &
              nsoil   =>  NoahmpIO%nsoil    ,&
              nsnow   =>  NoahmpIO%nsnow     &
             )
! -------------------------------------------------

    ! Input variables
    if ( allocated (NoahmpIO%coszen)    ) deallocate ( NoahmpIO%coszen              ) ! cosine zenith angle
    if ( allocated (NoahmpIO%xlat)      ) deallocate ( NoahmpIO%xlat                ) ! latitude [radians]
    if ( allocated (NoahmpIO%dzs)       ) deallocate ( NoahmpIO%dzs                 ) ! thickness of soil layers [m]
    if ( allocated (NoahmpIO%zsoil)     ) deallocate ( NoahmpIO%zsoil               ) ! depth to soil interfaces [m]
    if ( allocated (NoahmpIO%ivgtyp)    ) deallocate ( NoahmpIO%ivgtyp              ) ! vegetation type
    if ( allocated (NoahmpIO%isltyp)    ) deallocate ( NoahmpIO%isltyp              ) ! soil type
    if ( allocated (NoahmpIO%vegfra)    ) deallocate ( NoahmpIO%vegfra              ) ! vegetation fraction []
    if ( allocated (NoahmpIO%tmn)       ) deallocate ( NoahmpIO%tmn                 ) ! deep soil temperature [K]
    if ( allocated (NoahmpIO%xland)     ) deallocate ( NoahmpIO%xland               ) ! =2 ocean; =1 land/seaice
    if ( allocated (NoahmpIO%xice)      ) deallocate ( NoahmpIO%xice                ) ! fraction of grid that is seaice
    if ( allocated (NoahmpIO%swdown)    ) deallocate ( NoahmpIO%swdown              ) ! solar down at surface [W m-2]
    if ( allocated (NoahmpIO%swddir)    ) deallocate ( NoahmpIO%swddir              ) ! solar down at surface [W m-2] for new urban solar panel
    if ( allocated (NoahmpIO%swddif)    ) deallocate ( NoahmpIO%swddif              ) ! solar down at surface [W m-2] for new urban solar panel
    if ( allocated (NoahmpIO%glw)       ) deallocate ( NoahmpIO%glw                 ) ! longwave down at surface [W m-2]
    if ( allocated (NoahmpIO%rainbl)    ) deallocate ( NoahmpIO%rainbl              ) ! total precipitation entering land model [mm] per time step
    if ( allocated (NoahmpIO%snowbl)    ) deallocate ( NoahmpIO%snowbl              ) ! snow entering land model [mm] per time step
    if ( allocated (NoahmpIO%sr)        ) deallocate ( NoahmpIO%sr                  ) ! frozen precip ratio entering land model [-]
    if ( allocated (NoahmpIO%raincv)    ) deallocate ( NoahmpIO%raincv              ) ! convective precip forcing [mm]
    if ( allocated (NoahmpIO%rainncv)   ) deallocate ( NoahmpIO%rainncv             ) ! non-convective precip forcing [mm]
    if ( allocated (NoahmpIO%rainshv)   ) deallocate ( NoahmpIO%rainshv             ) ! shallow conv. precip forcing [mm]
    if ( allocated (NoahmpIO%snowncv)   ) deallocate ( NoahmpIO%snowncv             ) ! non-covective snow forcing (subset of rainncv) [mm]
    if ( allocated (NoahmpIO%graupelncv)) deallocate ( NoahmpIO%graupelncv          ) ! non-convective graupel forcing (subset of rainncv) [mm]
    if ( allocated (NoahmpIO%hailncv)   ) deallocate ( NoahmpIO%hailncv             ) ! non-convective hail forcing (subset of rainncv) [mm]
    if ( allocated (NoahmpIO%mp_rainc)  ) deallocate ( NoahmpIO%mp_rainc            ) ! convective precip forcing [mm]
    if ( allocated (NoahmpIO%mp_rainnc) ) deallocate ( NoahmpIO%mp_rainnc           ) ! non-convective precip forcing [mm]
    if ( allocated (NoahmpIO%mp_shcv)   ) deallocate ( NoahmpIO%mp_shcv             ) ! shallow conv. precip forcing [mm]
    if ( allocated (NoahmpIO%mp_snow)   ) deallocate ( NoahmpIO%mp_snow             ) ! non-covective snow (subset of rainnc) [mm]
    if ( allocated (NoahmpIO%mp_graup)  ) deallocate ( NoahmpIO%mp_graup            ) ! non-convective graupel (subset of rainnc) [mm]
    if ( allocated (NoahmpIO%mp_hail)   ) deallocate ( NoahmpIO%mp_hail             ) ! non-convective hail (subset of rainnc) [mm]
    if ( allocated (NoahmpIO%seaice)    ) deallocate ( NoahmpIO%seaice              ) ! seaice fraction
    if ( allocated (NoahmpIO%dz8w)      ) deallocate ( NoahmpIO%dz8w                ) ! thickness of atmo layers [m]
    if ( allocated (NoahmpIO%t_phy)     ) deallocate ( NoahmpIO%t_phy               ) ! 3d atmospheric temperature valid at mid-levels [K]
    if ( allocated (NoahmpIO%qv_curr)   ) deallocate ( NoahmpIO%qv_curr             ) ! 3d water vapor mixing ratio [kg/kg_dry]
    if ( allocated (NoahmpIO%u_phy)     ) deallocate ( NoahmpIO%u_phy               ) ! 3d u wind component [m/s]
    if ( allocated (NoahmpIO%v_phy)     ) deallocate ( NoahmpIO%v_phy               ) ! 3d v wind component [m/s]
    if ( allocated (NoahmpIO%p8w)       ) deallocate ( NoahmpIO%p8w                 ) ! 3d pressure, valid at interface [Pa]
 
    ! spatial varying parameter map
    if ( NoahmpIO%iopt_soil > 1 ) then
       if ( allocated (NoahmpIO%soilcomp)) deallocate ( NoahmpIO%soilcomp           ) ! soil sand and clay content [fraction]
       if ( allocated (NoahmpIO%soilcl1) ) deallocate ( NoahmpIO%soilcl1            ) ! soil texture class with depth
       if ( allocated (NoahmpIO%soilcl2) ) deallocate ( NoahmpIO%soilcl2            ) ! soil texture class with depth
       if ( allocated (NoahmpIO%soilcl3) ) deallocate ( NoahmpIO%soilcl3            ) ! soil texture class with depth
       if ( allocated (NoahmpIO%soilcl4) ) deallocate ( NoahmpIO%soilcl4            ) ! soil texture class with depth
    endif
    if ( NoahmpIO%iopt_soil == 4 ) then
       if ( allocated (NoahmpIO%bexp_3d)      ) deallocate ( NoahmpIO%bexp_3d       ) ! c-h b exponent
       if ( allocated (NoahmpIO%smcdry_3d)    ) deallocate ( NoahmpIO%smcdry_3d     ) ! soil moisture limit: dry
       if ( allocated (NoahmpIO%smcwlt_3d)    ) deallocate ( NoahmpIO%smcwlt_3d     ) ! soil moisture limit: wilt
       if ( allocated (NoahmpIO%smcref_3d)    ) deallocate ( NoahmpIO%smcref_3d     ) ! soil moisture limit: reference
       if ( allocated (NoahmpIO%smcmax_3d)    ) deallocate ( NoahmpIO%smcmax_3d     ) ! soil moisture limit: max
       if ( allocated (NoahmpIO%dksat_3d)     ) deallocate ( NoahmpIO%dksat_3d      ) ! saturated soil conductivity
       if ( allocated (NoahmpIO%dwsat_3d)     ) deallocate ( NoahmpIO%dwsat_3d      ) ! saturated soil diffusivity
       if ( allocated (NoahmpIO%psisat_3d)    ) deallocate ( NoahmpIO%psisat_3d     ) ! saturated matric potential
       if ( allocated (NoahmpIO%quartz_3d)    ) deallocate ( NoahmpIO%quartz_3d     ) ! soil quartz content
       if ( allocated (NoahmpIO%refdk_2d)     ) deallocate ( NoahmpIO%refdk_2d      ) ! reference soil conductivity
       if ( allocated (NoahmpIO%refkdt_2d)    ) deallocate ( NoahmpIO%refkdt_2d     ) ! soil infiltration parameter
       if ( allocated (NoahmpIO%irr_frac_2d)  ) deallocate ( NoahmpIO%irr_frac_2d   ) ! irrigation fraction
       if ( allocated (NoahmpIO%irr_har_2d)   ) deallocate ( NoahmpIO%irr_har_2d    ) ! number of days before harvest date to stop irrigation
       if ( allocated (NoahmpIO%irr_lai_2d)   ) deallocate ( NoahmpIO%irr_lai_2d    ) ! minimum lai to trigger irrigation
       if ( allocated (NoahmpIO%irr_mad_2d)   ) deallocate ( NoahmpIO%irr_mad_2d    ) ! management allowable deficit (0-1)
       if ( allocated (NoahmpIO%filoss_2d)    ) deallocate ( NoahmpIO%filoss_2d     ) ! fraction of flood irrigation loss (0-1)
       if ( allocated (NoahmpIO%sprir_rate_2d)) deallocate ( NoahmpIO%sprir_rate_2d ) ! mm/h, sprinkler irrigation rate
       if ( allocated (NoahmpIO%micir_rate_2d)) deallocate ( NoahmpIO%micir_rate_2d ) ! mm/h, micro irrigation rate
       if ( allocated (NoahmpIO%firtfac_2d)   ) deallocate ( NoahmpIO%firtfac_2d    ) ! flood application rate factor
       if ( allocated (NoahmpIO%ir_rain_2d)   ) deallocate ( NoahmpIO%ir_rain_2d    ) ! maximum precipitation to stop irrigation trigger
       if ( allocated (NoahmpIO%bvic_2d)      ) deallocate ( NoahmpIO%bvic_2d       ) ! VIC model infiltration parameter [-]
       if ( allocated (NoahmpIO%axaj_2d)      ) deallocate ( NoahmpIO%axaj_2d       ) ! tension water distribution inflection parameter [-]
       if ( allocated (NoahmpIO%bxaj_2d)      ) deallocate ( NoahmpIO%bxaj_2d       ) ! tension water distribution shape parameter [-]
       if ( allocated (NoahmpIO%xxaj_2d)      ) deallocate ( NoahmpIO%xxaj_2d       ) ! free water distribution shape parameter [-]
       if ( allocated (NoahmpIO%bdvic_2d)     ) deallocate ( NoahmpIO%bdvic_2d      ) ! DVIC model infiltration parameter [-]
       if ( allocated (NoahmpIO%gdvic_2d)     ) deallocate ( NoahmpIO%gdvic_2d      ) ! mean capillary drive (m) for infiltration models
       if ( allocated (NoahmpIO%bbvic_2d)     ) deallocate ( NoahmpIO%bbvic_2d      ) ! dvic heterogeniety parameter for infiltration [-]
       if ( allocated (NoahmpIO%klat_fac)     ) deallocate ( NoahmpIO%klat_fac      ) ! factor multiplier to hydraulic conductivity
       if ( allocated (NoahmpIO%tdsmc_fac)    ) deallocate ( NoahmpIO%tdsmc_fac     ) ! factor multiplier to field capacity
       if ( allocated (NoahmpIO%td_dc)        ) deallocate ( NoahmpIO%td_dc         ) ! drainage coefficient for simple
       if ( allocated (NoahmpIO%td_dcoef)     ) deallocate ( NoahmpIO%td_dcoef      ) ! drainage coefficient for Hooghoudt
       if ( allocated (NoahmpIO%td_ddrain)    ) deallocate ( NoahmpIO%td_ddrain     ) ! depth of drain
       if ( allocated (NoahmpIO%td_radi)      ) deallocate ( NoahmpIO%td_radi       ) ! tile radius
       if ( allocated (NoahmpIO%td_spac)      ) deallocate ( NoahmpIO%td_spac       ) ! tile spacing
    endif

    ! INOUT (with generic LSM equivalent) (as defined in WRF)
    if ( allocated (NoahmpIO%tsk)      ) deallocate ( NoahmpIO%tsk                  ) ! surface radiative temperature [K]
    if ( allocated (NoahmpIO%hfx)      ) deallocate ( NoahmpIO%hfx                  ) ! sensible heat flux [W m-2]
    if ( allocated (NoahmpIO%qfx)      ) deallocate ( NoahmpIO%qfx                  ) ! latent heat flux [kg s-1 m-2]
    if ( allocated (NoahmpIO%lh)       ) deallocate ( NoahmpIO%lh                   ) ! latent heat flux [W m-2]
    if ( allocated (NoahmpIO%grdflx)   ) deallocate ( NoahmpIO%grdflx               ) ! ground/snow heat flux [W m-2]
    if ( allocated (NoahmpIO%smstav)   ) deallocate ( NoahmpIO%smstav               ) ! soil moisture avail. [not used]
    if ( allocated (NoahmpIO%smstot)   ) deallocate ( NoahmpIO%smstot               ) ! total soil water [mm][not used]
    if ( allocated (NoahmpIO%sfcrunoff)) deallocate ( NoahmpIO%sfcrunoff            ) ! accumulated surface runoff [m]
    if ( allocated (NoahmpIO%udrunoff) ) deallocate ( NoahmpIO%udrunoff             ) ! accumulated sub-surface runoff [m]
    if ( allocated (NoahmpIO%albedo)   ) deallocate ( NoahmpIO%albedo               ) ! total grid albedo []
    if ( allocated (NoahmpIO%snowc)    ) deallocate ( NoahmpIO%snowc                ) ! snow cover fraction []
    if ( allocated (NoahmpIO%snow)     ) deallocate ( NoahmpIO%snow                 ) ! snow water equivalent [mm]
    if ( allocated (NoahmpIO%snowh)    ) deallocate ( NoahmpIO%snowh                ) ! physical snow depth [m]
    if ( allocated (NoahmpIO%canwat)   ) deallocate ( NoahmpIO%canwat               ) ! total canopy water + ice [mm]
    if ( allocated (NoahmpIO%acsnom)   ) deallocate ( NoahmpIO%acsnom               ) ! accumulated snow melt leaving pack
    if ( allocated (NoahmpIO%acsnow)   ) deallocate ( NoahmpIO%acsnow               ) ! accumulated snow on grid
    if ( allocated (NoahmpIO%emiss)    ) deallocate ( NoahmpIO%emiss                ) ! surface bulk emissivity
    if ( allocated (NoahmpIO%qsfc)     ) deallocate ( NoahmpIO%qsfc                 ) ! bulk surface specific humidity
    if ( allocated (NoahmpIO%smoiseq)  ) deallocate ( NoahmpIO%smoiseq              ) ! equilibrium volumetric soil moisture [m3/m3]
    if ( allocated (NoahmpIO%smois)    ) deallocate ( NoahmpIO%smois                ) ! volumetric soil moisture [m3/m3]
    if ( allocated (NoahmpIO%sh2o)     ) deallocate ( NoahmpIO%sh2o                 ) ! volumetric liquid soil moisture [m3/m3]
    if ( allocated (NoahmpIO%tslb)     ) deallocate ( NoahmpIO%tslb                 ) ! soil temperature [K]

    ! INOUT (with no Noah LSM equivalent) (as defined in WRF)
    if ( allocated (NoahmpIO%isnowxy)   ) deallocate ( NoahmpIO%isnowxy             ) ! actual no. of snow layers
    if ( allocated (NoahmpIO%tvxy)      ) deallocate ( NoahmpIO%tvxy                ) ! vegetation leaf temperature
    if ( allocated (NoahmpIO%tgxy)      ) deallocate ( NoahmpIO%tgxy                ) ! bulk ground surface temperature
    if ( allocated (NoahmpIO%canicexy)  ) deallocate ( NoahmpIO%canicexy            ) ! canopy-intercepted ice (mm)
    if ( allocated (NoahmpIO%canliqxy)  ) deallocate ( NoahmpIO%canliqxy            ) ! canopy-intercepted liquid water (mm)
    if ( allocated (NoahmpIO%eahxy)     ) deallocate ( NoahmpIO%eahxy               ) ! canopy air vapor pressure (Pa)
    if ( allocated (NoahmpIO%tahxy)     ) deallocate ( NoahmpIO%tahxy               ) ! canopy air temperature (K)
    if ( allocated (NoahmpIO%cmxy)      ) deallocate ( NoahmpIO%cmxy                ) ! bulk momentum drag coefficient
    if ( allocated (NoahmpIO%chxy)      ) deallocate ( NoahmpIO%chxy                ) ! bulk sensible heat exchange coefficient
    if ( allocated (NoahmpIO%fwetxy)    ) deallocate ( NoahmpIO%fwetxy              ) ! wetted or snowed fraction of the canopy (-)
    if ( allocated (NoahmpIO%sneqvoxy)  ) deallocate ( NoahmpIO%sneqvoxy            ) ! snow mass at last time step(mm H2O)
    if ( allocated (NoahmpIO%alboldxy)  ) deallocate ( NoahmpIO%alboldxy            ) ! snow albedo at last time step (-)
    if ( allocated (NoahmpIO%qsnowxy)   ) deallocate ( NoahmpIO%qsnowxy             ) ! snowfall on the ground [mm/s]
    if ( allocated (NoahmpIO%qrainxy)   ) deallocate ( NoahmpIO%qrainxy             ) ! rainfall on the ground [mm/s]
    if ( allocated (NoahmpIO%wslakexy)  ) deallocate ( NoahmpIO%wslakexy            ) ! lake water storage [mm]
    if ( allocated (NoahmpIO%zwtxy)     ) deallocate ( NoahmpIO%zwtxy               ) ! water table depth [m]
    if ( allocated (NoahmpIO%waxy)      ) deallocate ( NoahmpIO%waxy                ) ! water in the "aquifer" [mm]
    if ( allocated (NoahmpIO%wtxy)      ) deallocate ( NoahmpIO%wtxy                ) ! groundwater storage [mm]
    if ( allocated (NoahmpIO%smcwtdxy)  ) deallocate ( NoahmpIO%smcwtdxy            ) ! soil moisture below the bottom of the column (m3 m-3)
    if ( allocated (NoahmpIO%deeprechxy)) deallocate ( NoahmpIO%deeprechxy          ) ! recharge to the water table when deep (m)
    if ( allocated (NoahmpIO%rechxy)    ) deallocate ( NoahmpIO%rechxy              ) ! recharge to the water table (diagnostic) (m)
    if ( allocated (NoahmpIO%lfmassxy)  ) deallocate ( NoahmpIO%lfmassxy            ) ! leaf mass [g/m2]
    if ( allocated (NoahmpIO%rtmassxy)  ) deallocate ( NoahmpIO%rtmassxy            ) ! mass of fine roots [g/m2]
    if ( allocated (NoahmpIO%stmassxy)  ) deallocate ( NoahmpIO%stmassxy            ) ! stem mass [g/m2]
    if ( allocated (NoahmpIO%woodxy)    ) deallocate ( NoahmpIO%woodxy              ) ! mass of wood (incl. woody roots) [g/m2]
    if ( allocated (NoahmpIO%grainxy)   ) deallocate ( NoahmpIO%grainxy             ) ! mass of grain xing [g/m2]
    if ( allocated (NoahmpIO%gddxy)     ) deallocate ( NoahmpIO%gddxy               ) ! growing degree days xing four
    if ( allocated (NoahmpIO%stblcpxy)  ) deallocate ( NoahmpIO%stblcpxy            ) ! stable carbon in deep soil [g/m2]
    if ( allocated (NoahmpIO%fastcpxy)  ) deallocate ( NoahmpIO%fastcpxy            ) ! short-lived carbon, shallow soil [g/m2]
    if ( allocated (NoahmpIO%lai)       ) deallocate ( NoahmpIO%lai                 ) ! leaf area index
    if ( allocated (NoahmpIO%xsaixy)    ) deallocate ( NoahmpIO%xsaixy              ) ! stem area index
    if ( allocated (NoahmpIO%taussxy)   ) deallocate ( NoahmpIO%taussxy             ) ! snow age factor
    if ( allocated (NoahmpIO%tsnoxy)    ) deallocate ( NoahmpIO%tsnoxy              ) ! snow temperature [K]
    if ( allocated (NoahmpIO%zsnsoxy)   ) deallocate ( NoahmpIO%zsnsoxy             ) ! snow layer depth [m]
    if ( allocated (NoahmpIO%snicexy)   ) deallocate ( NoahmpIO%snicexy             ) ! snow layer ice [mm]
    if ( allocated (NoahmpIO%snliqxy)   ) deallocate ( NoahmpIO%snliqxy             ) ! snow layer liquid water [mm]

    ! irrigation
    if ( allocated (NoahmpIO%irfract) ) deallocate ( NoahmpIO%irfract               ) ! irrigation fraction
    if ( allocated (NoahmpIO%sifract) ) deallocate ( NoahmpIO%sifract               ) ! sprinkler irrigation fraction
    if ( allocated (NoahmpIO%mifract) ) deallocate ( NoahmpIO%mifract               ) ! micro irrigation fraction
    if ( allocated (NoahmpIO%fifract) ) deallocate ( NoahmpIO%fifract               ) ! flood irrigation fraction
    if ( allocated (NoahmpIO%irnumsi) ) deallocate ( NoahmpIO%irnumsi               ) ! irrigation event number, sprinkler
    if ( allocated (NoahmpIO%irnummi) ) deallocate ( NoahmpIO%irnummi               ) ! irrigation event number, micro
    if ( allocated (NoahmpIO%irnumfi) ) deallocate ( NoahmpIO%irnumfi               ) ! irrigation event number, flood
    if ( allocated (NoahmpIO%irwatsi) ) deallocate ( NoahmpIO%irwatsi               ) ! irrigation water amount [m] to be applied, sprinkler
    if ( allocated (NoahmpIO%irwatmi) ) deallocate ( NoahmpIO%irwatmi               ) ! irrigation water amount [m] to be applied, micro
    if ( allocated (NoahmpIO%irwatfi) ) deallocate ( NoahmpIO%irwatfi               ) ! irrigation water amount [m] to be applied, flood
    if ( allocated (NoahmpIO%ireloss) ) deallocate ( NoahmpIO%ireloss               ) ! loss of irrigation water to evaporation,sprinkler [mm]
    if ( allocated (NoahmpIO%irsivol) ) deallocate ( NoahmpIO%irsivol               ) ! amount of irrigation by sprinkler (mm)
    if ( allocated (NoahmpIO%irmivol) ) deallocate ( NoahmpIO%irmivol               ) ! amount of irrigation by micro (mm)
    if ( allocated (NoahmpIO%irfivol) ) deallocate ( NoahmpIO%irfivol               ) ! amount of irrigation by micro (mm)
    if ( allocated (NoahmpIO%irrsplh) ) deallocate ( NoahmpIO%irrsplh               ) ! latent heating from sprinkler evaporation (W/m2)
    if ( allocated (NoahmpIO%loctim)  ) deallocate ( NoahmpIO%loctim                ) ! local time
  
    ! OUT (with no Noah LSM equivalent) (as defined in WRF)   
    if ( allocated (NoahmpIO%t2mvxy)     ) deallocate ( NoahmpIO%t2mvxy             ) ! 2m temperature of vegetation part
    if ( allocated (NoahmpIO%t2mbxy)     ) deallocate ( NoahmpIO%t2mbxy             ) ! 2m temperature of bare ground part
    if ( allocated (NoahmpIO%t2mxy)      ) deallocate ( NoahmpIO%t2mxy              ) ! 2m grid-mean temperature
    if ( allocated (NoahmpIO%q2mvxy)     ) deallocate ( NoahmpIO%q2mvxy             ) ! 2m mixing ratio of vegetation part
    if ( allocated (NoahmpIO%q2mbxy)     ) deallocate ( NoahmpIO%q2mbxy             ) ! 2m mixing ratio of bare ground part
    if ( allocated (NoahmpIO%q2mxy)      ) deallocate ( NoahmpIO%q2mxy              ) ! 2m grid-mean mixing ratio
    if ( allocated (NoahmpIO%tradxy)     ) deallocate ( NoahmpIO%tradxy             ) ! surface radiative temperature (K)
    if ( allocated (NoahmpIO%neexy)      ) deallocate ( NoahmpIO%neexy              ) ! net ecosys exchange (g/m2/s CO2)
    if ( allocated (NoahmpIO%gppxy)      ) deallocate ( NoahmpIO%gppxy              ) ! gross primary assimilation [g/m2/s C]
    if ( allocated (NoahmpIO%nppxy)      ) deallocate ( NoahmpIO%nppxy              ) ! net primary productivity [g/m2/s C]
    if ( allocated (NoahmpIO%fvegxy)     ) deallocate ( NoahmpIO%fvegxy             ) ! noah-mp vegetation fraction [-]
    if ( allocated (NoahmpIO%runsfxy)    ) deallocate ( NoahmpIO%runsfxy            ) ! surface runoff [mm per soil timestep]
    if ( allocated (NoahmpIO%runsbxy)    ) deallocate ( NoahmpIO%runsbxy            ) ! subsurface runoff [mm per soil timestep]
    if ( allocated (NoahmpIO%ecanxy)     ) deallocate ( NoahmpIO%ecanxy             ) ! evaporation of intercepted water (mm/s)
    if ( allocated (NoahmpIO%edirxy)     ) deallocate ( NoahmpIO%edirxy             ) ! soil surface evaporation rate (mm/s]
    if ( allocated (NoahmpIO%etranxy)    ) deallocate ( NoahmpIO%etranxy            ) ! transpiration rate (mm/s)
    if ( allocated (NoahmpIO%fsaxy)      ) deallocate ( NoahmpIO%fsaxy              ) ! total absorbed solar radiation (W/m2)
    if ( allocated (NoahmpIO%firaxy)     ) deallocate ( NoahmpIO%firaxy             ) ! total net longwave rad (W/m2) [+ to atm]
    if ( allocated (NoahmpIO%aparxy)     ) deallocate ( NoahmpIO%aparxy             ) ! photosyn active energy by canopy (W/m2)
    if ( allocated (NoahmpIO%psnxy)      ) deallocate ( NoahmpIO%psnxy              ) ! total photosynthesis (umol CO2/m2/s) [+]
    if ( allocated (NoahmpIO%savxy)      ) deallocate ( NoahmpIO%savxy              ) ! solar rad absorbed by veg. (W/m2)
    if ( allocated (NoahmpIO%sagxy)      ) deallocate ( NoahmpIO%sagxy              ) ! solar rad absorbed by ground (W/m2)
    if ( allocated (NoahmpIO%rssunxy)    ) deallocate ( NoahmpIO%rssunxy            ) ! sunlit leaf stomatal resistance (s/m)
    if ( allocated (NoahmpIO%rsshaxy)    ) deallocate ( NoahmpIO%rsshaxy            ) ! shaded leaf stomatal resistance (s/m)
    if ( allocated (NoahmpIO%bgapxy)     ) deallocate ( NoahmpIO%bgapxy             ) ! between gap fraction
    if ( allocated (NoahmpIO%wgapxy)     ) deallocate ( NoahmpIO%wgapxy             ) ! within gap fraction
    if ( allocated (NoahmpIO%tgvxy)      ) deallocate ( NoahmpIO%tgvxy              ) ! under canopy ground temperature[K]
    if ( allocated (NoahmpIO%tgbxy)      ) deallocate ( NoahmpIO%tgbxy              ) ! bare ground temperature [K]
    if ( allocated (NoahmpIO%chvxy)      ) deallocate ( NoahmpIO%chvxy              ) ! sensible heat exchange coefficient vegetated
    if ( allocated (NoahmpIO%chbxy)      ) deallocate ( NoahmpIO%chbxy              ) ! sensible heat exchange coefficient bare-ground
    if ( allocated (NoahmpIO%shgxy)      ) deallocate ( NoahmpIO%shgxy              ) ! veg ground sen. heat [W/m2]   [+ to atm]
    if ( allocated (NoahmpIO%shcxy)      ) deallocate ( NoahmpIO%shcxy              ) ! canopy sen. heat [W/m2]   [+ to atm]
    if ( allocated (NoahmpIO%shbxy)      ) deallocate ( NoahmpIO%shbxy              ) ! bare sensible heat [W/m2]  [+ to atm]
    if ( allocated (NoahmpIO%evgxy)      ) deallocate ( NoahmpIO%evgxy              ) ! veg ground evap. heat [W/m2]  [+ to atm]
    if ( allocated (NoahmpIO%evbxy)      ) deallocate ( NoahmpIO%evbxy              ) ! bare soil evaporation [W/m2]  [+ to atm]
    if ( allocated (NoahmpIO%ghvxy)      ) deallocate ( NoahmpIO%ghvxy              ) ! veg ground heat flux [W/m2]  [+ to soil]
    if ( allocated (NoahmpIO%ghbxy)      ) deallocate ( NoahmpIO%ghbxy              ) ! bare ground heat flux [W/m2] [+ to soil]
    if ( allocated (NoahmpIO%irgxy)      ) deallocate ( NoahmpIO%irgxy              ) ! veg ground net lw rad. [W/m2] [+ to atm]
    if ( allocated (NoahmpIO%ircxy)      ) deallocate ( NoahmpIO%ircxy              ) ! canopy net lw rad. [W/m2] [+ to atm]
    if ( allocated (NoahmpIO%irbxy)      ) deallocate ( NoahmpIO%irbxy              ) ! bare net longwave rad. [W/m2] [+ to atm]
    if ( allocated (NoahmpIO%trxy)       ) deallocate ( NoahmpIO%trxy               ) ! transpiration [w/m2]  [+ to atm]
    if ( allocated (NoahmpIO%evcxy)      ) deallocate ( NoahmpIO%evcxy              ) ! canopy evaporation heat [W/m2]  [+ to atm]
    if ( allocated (NoahmpIO%chleafxy)   ) deallocate ( NoahmpIO%chleafxy           ) ! leaf exchange coefficient
    if ( allocated (NoahmpIO%chucxy)     ) deallocate ( NoahmpIO%chucxy             ) ! under canopy exchange coefficient
    if ( allocated (NoahmpIO%chv2xy)     ) deallocate ( NoahmpIO%chv2xy             ) ! veg 2m exchange coefficient
    if ( allocated (NoahmpIO%chb2xy)     ) deallocate ( NoahmpIO%chb2xy             ) ! bare 2m exchange coefficient
    if ( allocated (NoahmpIO%rs)         ) deallocate ( NoahmpIO%rs                 ) ! total stomatal resistance (s/m)
    if ( allocated (NoahmpIO%z0)         ) deallocate ( NoahmpIO%z0                 ) ! roughness length output to WRF
    if ( allocated (NoahmpIO%znt)        ) deallocate ( NoahmpIO%znt                ) ! roughness length output to WRF
    if ( allocated (NoahmpIO%qtdrain)    ) deallocate ( NoahmpIO%qtdrain            ) ! tile drainage (mm)
    if ( allocated (NoahmpIO%td_fraction)) deallocate ( NoahmpIO%td_fraction        ) ! tile drainage fraction
    if ( allocated (NoahmpIO%xlong)      ) deallocate ( NoahmpIO%xlong              ) ! longitude
    if ( allocated (NoahmpIO%terrain)    ) deallocate ( NoahmpIO%terrain            ) ! terrain height
    if ( allocated (NoahmpIO%gvfmin)     ) deallocate ( NoahmpIO%gvfmin             ) ! annual minimum in vegetation fraction
    if ( allocated (NoahmpIO%gvfmax)     ) deallocate ( NoahmpIO%gvfmax             ) ! annual maximum in vegetation fraction

    ! additional output variables
    if ( allocated (NoahmpIO%pahxy)       ) deallocate ( NoahmpIO%pahxy             )
    if ( allocated (NoahmpIO%pahgxy)      ) deallocate ( NoahmpIO%pahgxy            )
    if ( allocated (NoahmpIO%pahbxy)      ) deallocate ( NoahmpIO%pahbxy            )
    if ( allocated (NoahmpIO%pahvxy)      ) deallocate ( NoahmpIO%pahvxy            )
    if ( allocated (NoahmpIO%qintsxy)     ) deallocate ( NoahmpIO%qintsxy           )
    if ( allocated (NoahmpIO%qintrxy)     ) deallocate ( NoahmpIO%qintrxy           )
    if ( allocated (NoahmpIO%qdripsxy)    ) deallocate ( NoahmpIO%qdripsxy          )
    if ( allocated (NoahmpIO%qdriprxy)    ) deallocate ( NoahmpIO%qdriprxy          )
    if ( allocated (NoahmpIO%qthrosxy)    ) deallocate ( NoahmpIO%qthrosxy          )
    if ( allocated (NoahmpIO%qthrorxy)    ) deallocate ( NoahmpIO%qthrorxy          )
    if ( allocated (NoahmpIO%qsnsubxy)    ) deallocate ( NoahmpIO%qsnsubxy          )
    if ( allocated (NoahmpIO%qsnfroxy)    ) deallocate ( NoahmpIO%qsnfroxy          )
    if ( allocated (NoahmpIO%qsubcxy)     ) deallocate ( NoahmpIO%qsubcxy           )
    if ( allocated (NoahmpIO%qfrocxy)     ) deallocate ( NoahmpIO%qfrocxy           )
    if ( allocated (NoahmpIO%qevacxy)     ) deallocate ( NoahmpIO%qevacxy           )
    if ( allocated (NoahmpIO%qdewcxy)     ) deallocate ( NoahmpIO%qdewcxy           )
    if ( allocated (NoahmpIO%qfrzcxy)     ) deallocate ( NoahmpIO%qfrzcxy           )
    if ( allocated (NoahmpIO%qmeltcxy)    ) deallocate ( NoahmpIO%qmeltcxy          )
    if ( allocated (NoahmpIO%qsnbotxy)    ) deallocate ( NoahmpIO%qsnbotxy          )
    if ( allocated (NoahmpIO%qmeltxy)     ) deallocate ( NoahmpIO%qmeltxy           )
    if ( allocated (NoahmpIO%pondingxy)   ) deallocate ( NoahmpIO%pondingxy         )
    if ( allocated (NoahmpIO%fpicexy)     ) deallocate ( NoahmpIO%fpicexy           )
    if ( allocated (NoahmpIO%rainlsm)     ) deallocate ( NoahmpIO%rainlsm           )
    if ( allocated (NoahmpIO%snowlsm)     ) deallocate ( NoahmpIO%snowlsm           )
    if ( allocated (NoahmpIO%forctlsm)    ) deallocate ( NoahmpIO%forctlsm          )
    if ( allocated (NoahmpIO%forcqlsm)    ) deallocate ( NoahmpIO%forcqlsm          )
    if ( allocated (NoahmpIO%forcplsm)    ) deallocate ( NoahmpIO%forcplsm          )
    if ( allocated (NoahmpIO%forczlsm)    ) deallocate ( NoahmpIO%forczlsm          )
    if ( allocated (NoahmpIO%forcwlsm)    ) deallocate ( NoahmpIO%forcwlsm          )
    if ( allocated (NoahmpIO%eflxbxy)     ) deallocate ( NoahmpIO%eflxbxy           )
    if ( allocated (NoahmpIO%soilenergy)  ) deallocate ( NoahmpIO%soilenergy        )
    if ( allocated (NoahmpIO%snowenergy)  ) deallocate ( NoahmpIO%snowenergy        )
    if ( allocated (NoahmpIO%canhsxy)     ) deallocate ( NoahmpIO%canhsxy           )
    if ( allocated (NoahmpIO%acc_dwaterxy)) deallocate ( NoahmpIO%acc_dwaterxy      )
    if ( allocated (NoahmpIO%acc_prcpxy)  ) deallocate ( NoahmpIO%acc_prcpxy        )
    if ( allocated (NoahmpIO%acc_ecanxy)  ) deallocate ( NoahmpIO%acc_ecanxy        )
    if ( allocated (NoahmpIO%acc_etranxy) ) deallocate ( NoahmpIO%acc_etranxy       )
    if ( allocated (NoahmpIO%acc_edirxy)  ) deallocate ( NoahmpIO%acc_edirxy        )
    if ( allocated (NoahmpIO%acc_ssoilxy) ) deallocate ( NoahmpIO%acc_ssoilxy       )
    if ( allocated (NoahmpIO%acc_qinsurxy)) deallocate ( NoahmpIO%acc_qinsurxy      )
    if ( allocated (NoahmpIO%acc_qsevaxy) ) deallocate ( NoahmpIO%acc_qsevaxy       )
    if ( allocated (NoahmpIO%acc_etranixy)) deallocate ( NoahmpIO%acc_etranixy      )

    ! needed for mmf_runoff (iopt_run = 5); not part of mp driver in WRF
    if ( allocated (NoahmpIO%msftx)      ) deallocate ( NoahmpIO%msftx              )
    if ( allocated (NoahmpIO%msfty)      ) deallocate ( NoahmpIO%msfty              )
    if ( allocated (NoahmpIO%eqzwt)      ) deallocate ( NoahmpIO%eqzwt              )
    if ( allocated (NoahmpIO%riverbedxy) ) deallocate ( NoahmpIO%riverbedxy         )
    if ( allocated (NoahmpIO%rivercondxy)) deallocate ( NoahmpIO%rivercondxy        )
    if ( allocated (NoahmpIO%pexpxy)     ) deallocate ( NoahmpIO%pexpxy             )
    if ( allocated (NoahmpIO%fdepthxy)   ) deallocate ( NoahmpIO%fdepthxy           )
    if ( allocated (NoahmpIO%areaxy)     ) deallocate ( NoahmpIO%areaxy             )
    if ( allocated (NoahmpIO%qrfsxy)     ) deallocate ( NoahmpIO%qrfsxy             )
    if ( allocated (NoahmpIO%qspringsxy) ) deallocate ( NoahmpIO%qspringsxy         )
    if ( allocated (NoahmpIO%qrfxy)      ) deallocate ( NoahmpIO%qrfxy              )
    if ( allocated (NoahmpIO%qspringxy)  ) deallocate ( NoahmpIO%qspringxy          )
    if ( allocated (NoahmpIO%qslatxy)    ) deallocate ( NoahmpIO%qslatxy            )
    if ( allocated (NoahmpIO%qlatxy)     ) deallocate ( NoahmpIO%qlatxy             )
    if ( allocated (NoahmpIO%rechclim)   ) deallocate ( NoahmpIO%rechclim           )
    if ( allocated (NoahmpIO%rivermask)  ) deallocate ( NoahmpIO%rivermask          )
    if ( allocated (NoahmpIO%nonriverxy) ) deallocate ( NoahmpIO%nonriverxy         )

    ! needed for crop model (opt_crop=1)
    if ( allocated (NoahmpIO%pgsxy)     ) deallocate ( NoahmpIO%pgsxy               )
    if ( allocated (NoahmpIO%cropcat)   ) deallocate ( NoahmpIO%cropcat             )
    if ( allocated (NoahmpIO%planting)  ) deallocate ( NoahmpIO%planting            )
    if ( allocated (NoahmpIO%harvest)   ) deallocate ( NoahmpIO%harvest             )
    if ( allocated (NoahmpIO%season_gdd)) deallocate ( NoahmpIO%season_gdd          )
    if ( allocated (NoahmpIO%croptype)  ) deallocate ( NoahmpIO%croptype            )

    ! Single- and Multi-layer Urban Models
    if ( NoahmpIO%sf_urban_physics > 0 )  then
       if ( allocated (NoahmpIO%sh_urb2d)   ) deallocate ( NoahmpIO%sh_urb2d        )
       if ( allocated (NoahmpIO%lh_urb2d)   ) deallocate ( NoahmpIO%lh_urb2d        )
       if ( allocated (NoahmpIO%g_urb2d)    ) deallocate ( NoahmpIO%g_urb2d         )
       if ( allocated (NoahmpIO%rn_urb2d)   ) deallocate ( NoahmpIO%rn_urb2d        )
       if ( allocated (NoahmpIO%ts_urb2d)   ) deallocate ( NoahmpIO%ts_urb2d        )
       if ( allocated (NoahmpIO%hrang)      ) deallocate ( NoahmpIO%hrang           )
       if ( allocated (NoahmpIO%frc_urb2d)  ) deallocate ( NoahmpIO%frc_urb2d       )
       if ( allocated (NoahmpIO%utype_urb2d)) deallocate ( NoahmpIO%utype_urb2d     )
       if ( allocated (NoahmpIO%lp_urb2d)   ) deallocate ( NoahmpIO%lp_urb2d        )
       if ( allocated (NoahmpIO%lb_urb2d)   ) deallocate ( NoahmpIO%lb_urb2d        )
       if ( allocated (NoahmpIO%hgt_urb2d)  ) deallocate ( NoahmpIO%hgt_urb2d       )
       if ( allocated (NoahmpIO%ust)        ) deallocate ( NoahmpIO%ust             )
    endif
         
    if(NoahmpIO%sf_urban_physics == 1 ) then  ! single layer urban model
       if ( allocated (NoahmpIO%cmr_sfcdif)   ) deallocate ( NoahmpIO%cmr_sfcdif    )
       if ( allocated (NoahmpIO%chr_sfcdif)   ) deallocate ( NoahmpIO%chr_sfcdif    )
       if ( allocated (NoahmpIO%cmc_sfcdif)   ) deallocate ( NoahmpIO%cmc_sfcdif    )
       if ( allocated (NoahmpIO%chc_sfcdif)   ) deallocate ( NoahmpIO%chc_sfcdif    )
       if ( allocated (NoahmpIO%cmgr_sfcdif)  ) deallocate ( NoahmpIO%cmgr_sfcdif   )
       if ( allocated (NoahmpIO%chgr_sfcdif)  ) deallocate ( NoahmpIO%chgr_sfcdif   )
       if ( allocated (NoahmpIO%tr_urb2d)     ) deallocate ( NoahmpIO%tr_urb2d      )
       if ( allocated (NoahmpIO%tb_urb2d)     ) deallocate ( NoahmpIO%tb_urb2d      )
       if ( allocated (NoahmpIO%tg_urb2d)     ) deallocate ( NoahmpIO%tg_urb2d      )
       if ( allocated (NoahmpIO%tc_urb2d)     ) deallocate ( NoahmpIO%tc_urb2d      )
       if ( allocated (NoahmpIO%qc_urb2d)     ) deallocate ( NoahmpIO%qc_urb2d      )
       if ( allocated (NoahmpIO%uc_urb2d)     ) deallocate ( NoahmpIO%uc_urb2d      )
       if ( allocated (NoahmpIO%xxxr_urb2d)   ) deallocate ( NoahmpIO%xxxr_urb2d    )
       if ( allocated (NoahmpIO%xxxb_urb2d)   ) deallocate ( NoahmpIO%xxxb_urb2d    )
       if ( allocated (NoahmpIO%xxxg_urb2d)   ) deallocate ( NoahmpIO%xxxg_urb2d    )
       if ( allocated (NoahmpIO%xxxc_urb2d)   ) deallocate ( NoahmpIO%xxxc_urb2d    )
       if ( allocated (NoahmpIO%psim_urb2d)   ) deallocate ( NoahmpIO%psim_urb2d    )
       if ( allocated (NoahmpIO%psih_urb2d)   ) deallocate ( NoahmpIO%psih_urb2d    )
       if ( allocated (NoahmpIO%u10_urb2d)    ) deallocate ( NoahmpIO%u10_urb2d     )
       if ( allocated (NoahmpIO%v10_urb2d)    ) deallocate ( NoahmpIO%v10_urb2d     )
       if ( allocated (NoahmpIO%gz1oz0_urb2d) ) deallocate ( NoahmpIO%gz1oz0_urb2d  )
       if ( allocated (NoahmpIO%akms_urb2d)   ) deallocate ( NoahmpIO%akms_urb2d    )
       if ( allocated (NoahmpIO%th2_urb2d)    ) deallocate ( NoahmpIO%th2_urb2d     )
       if ( allocated (NoahmpIO%q2_urb2d)     ) deallocate ( NoahmpIO%q2_urb2d      )
       if ( allocated (NoahmpIO%ust_urb2d)    ) deallocate ( NoahmpIO%ust_urb2d     )
       if ( allocated (NoahmpIO%cmcr_urb2d)   ) deallocate ( NoahmpIO%cmcr_urb2d    )
       if ( allocated (NoahmpIO%tgr_urb2d)    ) deallocate ( NoahmpIO%tgr_urb2d     )
       if ( allocated (NoahmpIO%drelr_urb2d)  ) deallocate ( NoahmpIO%drelr_urb2d   )
       if ( allocated (NoahmpIO%drelb_urb2d)  ) deallocate ( NoahmpIO%drelb_urb2d   )
       if ( allocated (NoahmpIO%drelg_urb2d)  ) deallocate ( NoahmpIO%drelg_urb2d   )
       if ( allocated (NoahmpIO%flxhumr_urb2d)) deallocate ( NoahmpIO%flxhumr_urb2d )
       if ( allocated (NoahmpIO%flxhumb_urb2d)) deallocate ( NoahmpIO%flxhumb_urb2d )
       if ( allocated (NoahmpIO%flxhumg_urb2d)) deallocate ( NoahmpIO%flxhumg_urb2d )
       if ( allocated (NoahmpIO%chs)          ) deallocate ( NoahmpIO%chs           )
       if ( allocated (NoahmpIO%chs2)         ) deallocate ( NoahmpIO%chs2          )
       if ( allocated (NoahmpIO%cqs2)         ) deallocate ( NoahmpIO%cqs2          )
       if ( allocated (NoahmpIO%mh_urb2d)     ) deallocate ( NoahmpIO%mh_urb2d      )
       if ( allocated (NoahmpIO%stdh_urb2d)   ) deallocate ( NoahmpIO%stdh_urb2d    )
       if ( allocated (NoahmpIO%lf_urb2d)     ) deallocate ( NoahmpIO%lf_urb2d      )
       if ( allocated (NoahmpIO%trl_urb3d)    ) deallocate ( NoahmpIO%trl_urb3d     )
       if ( allocated (NoahmpIO%tbl_urb3d)    ) deallocate ( NoahmpIO%tbl_urb3d     )
       if ( allocated (NoahmpIO%tgl_urb3d)    ) deallocate ( NoahmpIO%tgl_urb3d     )
       if ( allocated (NoahmpIO%tgrl_urb3d)   ) deallocate ( NoahmpIO%tgrl_urb3d    )
       if ( allocated (NoahmpIO%smr_urb3d)    ) deallocate ( NoahmpIO%smr_urb3d     )
       if ( allocated (NoahmpIO%dzr)          ) deallocate ( NoahmpIO%dzr           )
       if ( allocated (NoahmpIO%dzb)          ) deallocate ( NoahmpIO%dzb           )
       if ( allocated (NoahmpIO%dzg)          ) deallocate ( NoahmpIO%dzg           )
    endif

    if(NoahmpIO%sf_urban_physics == 2 .or. NoahmpIO%sf_urban_physics == 3) then  ! bep or bem urban models
       if ( allocated (NoahmpIO%trb_urb4d)  ) deallocate ( NoahmpIO%trb_urb4d       )
       if ( allocated (NoahmpIO%tw1_urb4d)  ) deallocate ( NoahmpIO%tw1_urb4d       )
       if ( allocated (NoahmpIO%tw2_urb4d)  ) deallocate ( NoahmpIO%tw2_urb4d       )
       if ( allocated (NoahmpIO%tgb_urb4d)  ) deallocate ( NoahmpIO%tgb_urb4d       )
       if ( allocated (NoahmpIO%sfw1_urb3d) ) deallocate ( NoahmpIO%sfw1_urb3d      )
       if ( allocated (NoahmpIO%sfw2_urb3d) ) deallocate ( NoahmpIO%sfw2_urb3d      )
       if ( allocated (NoahmpIO%sfr_urb3d)  ) deallocate ( NoahmpIO%sfr_urb3d       )
       if ( allocated (NoahmpIO%sfg_urb3d)  ) deallocate ( NoahmpIO%sfg_urb3d       )
       if ( allocated (NoahmpIO%hi_urb2d)   ) deallocate ( NoahmpIO%hi_urb2d        )
       if ( allocated (NoahmpIO%theta_urban)) deallocate ( NoahmpIO%theta_urban     )
       if ( allocated (NoahmpIO%u_urban)    ) deallocate ( NoahmpIO%u_urban         )
       if ( allocated (NoahmpIO%v_urban)    ) deallocate ( NoahmpIO%v_urban         )
       if ( allocated (NoahmpIO%dz_urban)   ) deallocate ( NoahmpIO%dz_urban        )
       if ( allocated (NoahmpIO%rho_urban)  ) deallocate ( NoahmpIO%rho_urban       )
       if ( allocated (NoahmpIO%p_urban)    ) deallocate ( NoahmpIO%p_urban         )
       if ( allocated (NoahmpIO%a_u_bep)    ) deallocate ( NoahmpIO%a_u_bep         )
       if ( allocated (NoahmpIO%a_v_bep)    ) deallocate ( NoahmpIO%a_v_bep         )
       if ( allocated (NoahmpIO%a_t_bep)    ) deallocate ( NoahmpIO%a_t_bep         )
       if ( allocated (NoahmpIO%a_q_bep)    ) deallocate ( NoahmpIO%a_q_bep         )
       if ( allocated (NoahmpIO%a_e_bep)    ) deallocate ( NoahmpIO%a_e_bep         )
       if ( allocated (NoahmpIO%b_u_bep)    ) deallocate ( NoahmpIO%b_u_bep         )
       if ( allocated (NoahmpIO%b_v_bep)    ) deallocate ( NoahmpIO%b_v_bep         )
       if ( allocated (NoahmpIO%b_t_bep)    ) deallocate ( NoahmpIO%b_t_bep         )
       if ( allocated (NoahmpIO%b_q_bep)    ) deallocate ( NoahmpIO%b_q_bep         )
       if ( allocated (NoahmpIO%b_e_bep)    ) deallocate ( NoahmpIO%b_e_bep         )
       if ( allocated (NoahmpIO%dlg_bep)    ) deallocate ( NoahmpIO%dlg_bep         )
       if ( allocated (NoahmpIO%dl_u_bep)   ) deallocate ( NoahmpIO%dl_u_bep        )
       if ( allocated (NoahmpIO%sf_bep)     ) deallocate ( NoahmpIO%sf_bep          )
       if ( allocated (NoahmpIO%vl_bep)     ) deallocate ( NoahmpIO%vl_bep          )
    endif

    if(NoahmpIO%sf_urban_physics == 3) then  ! bem urban model
       if ( allocated (NoahmpIO%tlev_urb3d)   ) deallocate ( NoahmpIO%tlev_urb3d    )
       if ( allocated (NoahmpIO%qlev_urb3d)   ) deallocate ( NoahmpIO%qlev_urb3d    )
       if ( allocated (NoahmpIO%tw1lev_urb3d) ) deallocate ( NoahmpIO%tw1lev_urb3d  )
       if ( allocated (NoahmpIO%tw2lev_urb3d) ) deallocate ( NoahmpIO%tw2lev_urb3d  )
       if ( allocated (NoahmpIO%tglev_urb3d)  ) deallocate ( NoahmpIO%tglev_urb3d   )
       if ( allocated (NoahmpIO%tflev_urb3d)  ) deallocate ( NoahmpIO%tflev_urb3d   )
       if ( allocated (NoahmpIO%sf_ac_urb3d)  ) deallocate ( NoahmpIO%sf_ac_urb3d   )
       if ( allocated (NoahmpIO%lf_ac_urb3d)  ) deallocate ( NoahmpIO%lf_ac_urb3d   )
       if ( allocated (NoahmpIO%cm_ac_urb3d)  ) deallocate ( NoahmpIO%cm_ac_urb3d   )
       if ( allocated (NoahmpIO%sfvent_urb3d) ) deallocate ( NoahmpIO%sfvent_urb3d  )
       if ( allocated (NoahmpIO%lfvent_urb3d) ) deallocate ( NoahmpIO%lfvent_urb3d  )
       if ( allocated (NoahmpIO%sfwin1_urb3d) ) deallocate ( NoahmpIO%sfwin1_urb3d  )
       if ( allocated (NoahmpIO%sfwin2_urb3d) ) deallocate ( NoahmpIO%sfwin2_urb3d  )
       if ( allocated (NoahmpIO%ep_pv_urb3d)  ) deallocate ( NoahmpIO%ep_pv_urb3d   )
       if ( allocated (NoahmpIO%t_pv_urb3d)   ) deallocate ( NoahmpIO%t_pv_urb3d    )
       if ( allocated (NoahmpIO%trv_urb4d)    ) deallocate ( NoahmpIO%trv_urb4d     )
       if ( allocated (NoahmpIO%qr_urb4d)     ) deallocate ( NoahmpIO%qr_urb4d      )
       if ( allocated (NoahmpIO%qgr_urb3d)    ) deallocate ( NoahmpIO%qgr_urb3d     )
       if ( allocated (NoahmpIO%tgr_urb3d)    ) deallocate ( NoahmpIO%tgr_urb3d     )
       if ( allocated (NoahmpIO%drain_urb4d)  ) deallocate ( NoahmpIO%drain_urb4d   )
       if ( allocated (NoahmpIO%draingr_urb3d)) deallocate ( NoahmpIO%draingr_urb3d )
       if ( allocated (NoahmpIO%sfrv_urb3d)   ) deallocate ( NoahmpIO%sfrv_urb3d    )
       if ( allocated (NoahmpIO%lfrv_urb3d)   ) deallocate ( NoahmpIO%lfrv_urb3d    )
       if ( allocated (NoahmpIO%dgr_urb3d)    ) deallocate ( NoahmpIO%dgr_urb3d     )
       if ( allocated (NoahmpIO%dg_urb3d)     ) deallocate ( NoahmpIO%dg_urb3d      )
       if ( allocated (NoahmpIO%lfr_urb3d)    ) deallocate ( NoahmpIO%lfr_urb3d     )
       if ( allocated (NoahmpIO%lfg_urb3d)    ) deallocate ( NoahmpIO%lfg_urb3d     )

    endif

#ifdef WRF_HYDRO
    if ( allocated (NoahmpIO%infxsrt)   ) deallocate ( NoahmpIO%infxsrt             )
    if ( allocated (NoahmpIO%sfcheadrt) ) deallocate ( NoahmpIO%sfcheadrt           )
    if ( allocated (NoahmpIO%soldrain)  ) deallocate ( NoahmpIO%soldrain            )
    if ( allocated (NoahmpIO%qtiledrain)) deallocate ( NoahmpIO%qtiledrain          )
    if ( allocated (NoahmpIO%zwatble2d) ) deallocate ( NoahmpIO%zwatble2d           )
#endif    

    end associate
 
  end subroutine NoahmpIOVarFinalizeDefault

end module NoahmpIOVarFinalizeMod
