module NoahmpIOVarInitMod

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

  subroutine NoahmpIOVarInitDefault(NoahmpIO)

    implicit none

    type(NoahmpIO_type), intent(inout) :: NoahmpIO
   
! ------------------------------------------------- 
    associate( &
              its   =>  NoahmpIO%its   ,&
              ite   =>  NoahmpIO%ite   ,&
              kts   =>  NoahmpIO%kts   ,&
              kte   =>  NoahmpIO%kte   ,&
              nsoil =>  NoahmpIO%nsoil ,&
              nsnow =>  NoahmpIO%nsnow  &
             )
! -------------------------------------------------

    ! Input variables
    if ( .not. allocated (NoahmpIO%coszen)    ) allocate ( NoahmpIO%coszen     (its:ite            )      ) ! cosine zenith angle
    if ( .not. allocated (NoahmpIO%xlat)      ) allocate ( NoahmpIO%xlat       (its:ite            )      ) ! latitude [radians]
    if ( .not. allocated (NoahmpIO%dzs)       ) allocate ( NoahmpIO%dzs        (1:nsoil            )      ) ! thickness of soil layers [m]
    if ( .not. allocated (NoahmpIO%zsoil)     ) allocate ( NoahmpIO%zsoil      (1:nsoil            )      ) ! depth to soil interfaces [m]
    if ( .not. allocated (NoahmpIO%ivgtyp)    ) allocate ( NoahmpIO%ivgtyp     (its:ite            )      ) ! vegetation type
    if ( .not. allocated (NoahmpIO%isltyp)    ) allocate ( NoahmpIO%isltyp     (its:ite            )      ) ! soil type
    if ( .not. allocated (NoahmpIO%vegfra)    ) allocate ( NoahmpIO%vegfra     (its:ite            )      ) ! vegetation fraction []
    if ( .not. allocated (NoahmpIO%tmn)       ) allocate ( NoahmpIO%tmn        (its:ite            )      ) ! deep soil temperature [K]
    if ( .not. allocated (NoahmpIO%xland)     ) allocate ( NoahmpIO%xland      (its:ite            )      ) ! =2 ocean; =1 land/seaice
    if ( .not. allocated (NoahmpIO%xice)      ) allocate ( NoahmpIO%xice       (its:ite            )      ) ! fraction of grid that is seaice
    if ( .not. allocated (NoahmpIO%swdown)    ) allocate ( NoahmpIO%swdown     (its:ite            )      ) ! solar down at surface [W m-2]
    if ( .not. allocated (NoahmpIO%swddir)    ) allocate ( NoahmpIO%swddir     (its:ite            )      ) ! solar down at surface [W m-2] for new urban solar panel
    if ( .not. allocated (NoahmpIO%swddif)    ) allocate ( NoahmpIO%swddif     (its:ite            )      ) ! solar down at surface [W m-2] for new urban solar panel
    if ( .not. allocated (NoahmpIO%glw)       ) allocate ( NoahmpIO%glw        (its:ite            )      ) ! longwave down at surface [W m-2]
    if ( .not. allocated (NoahmpIO%rainbl)    ) allocate ( NoahmpIO%rainbl     (its:ite            )      ) ! total precipitation entering land model [mm] per time step
    if ( .not. allocated (NoahmpIO%snowbl)    ) allocate ( NoahmpIO%snowbl     (its:ite            )      ) ! snow entering land model [mm] per time step
    if ( .not. allocated (NoahmpIO%sr)        ) allocate ( NoahmpIO%sr         (its:ite            )      ) ! frozen precip ratio entering land model [-]
    if ( .not. allocated (NoahmpIO%raincv)    ) allocate ( NoahmpIO%raincv     (its:ite            )      ) ! convective precip forcing [mm]
    if ( .not. allocated (NoahmpIO%rainncv)   ) allocate ( NoahmpIO%rainncv    (its:ite            )      ) ! non-convective precip forcing [mm]
    if ( .not. allocated (NoahmpIO%rainshv)   ) allocate ( NoahmpIO%rainshv    (its:ite            )      ) ! shallow conv. precip forcing [mm]
    if ( .not. allocated (NoahmpIO%snowncv)   ) allocate ( NoahmpIO%snowncv    (its:ite            )      ) ! non-covective snow forcing (subset of rainncv) [mm]
    if ( .not. allocated (NoahmpIO%graupelncv)) allocate ( NoahmpIO%graupelncv (its:ite            )      ) ! non-convective graupel forcing (subset of rainncv) [mm]
    if ( .not. allocated (NoahmpIO%hailncv)   ) allocate ( NoahmpIO%hailncv    (its:ite            )      ) ! non-convective hail forcing (subset of rainncv) [mm]
    if ( .not. allocated (NoahmpIO%mp_rainc)  ) allocate ( NoahmpIO%mp_rainc   (its:ite            )      ) ! convective precip forcing [mm]
    if ( .not. allocated (NoahmpIO%mp_rainnc) ) allocate ( NoahmpIO%mp_rainnc  (its:ite            )      ) ! non-convective precip forcing [mm]
    if ( .not. allocated (NoahmpIO%mp_shcv)   ) allocate ( NoahmpIO%mp_shcv    (its:ite            )      ) ! shallow conv. precip forcing [mm]
    if ( .not. allocated (NoahmpIO%mp_snow)   ) allocate ( NoahmpIO%mp_snow    (its:ite            )      ) ! non-covective snow (subset of rainnc) [mm]
    if ( .not. allocated (NoahmpIO%mp_graup)  ) allocate ( NoahmpIO%mp_graup   (its:ite            )      ) ! non-convective graupel (subset of rainnc) [mm]
    if ( .not. allocated (NoahmpIO%mp_hail)   ) allocate ( NoahmpIO%mp_hail    (its:ite            )      ) ! non-convective hail (subset of rainnc) [mm]
    if ( .not. allocated (NoahmpIO%seaice)    ) allocate ( NoahmpIO%seaice     (its:ite            )      ) ! seaice fraction
    if ( .not. allocated (NoahmpIO%dz8w)      ) allocate ( NoahmpIO%dz8w       (its:ite,kts:kte    )      ) ! thickness of atmo layers [m]
    if ( .not. allocated (NoahmpIO%t_phy)     ) allocate ( NoahmpIO%t_phy      (its:ite,kts:kte    )      ) ! 3d atmospheric temperature valid at mid-levels [K]
    if ( .not. allocated (NoahmpIO%qv_curr)   ) allocate ( NoahmpIO%qv_curr    (its:ite,kts:kte    )      ) ! 3d water vapor mixing ratio [kg/kg_dry]
    if ( .not. allocated (NoahmpIO%u_phy)     ) allocate ( NoahmpIO%u_phy      (its:ite,kts:kte    )      ) ! 3d u wind component [m/s]
    if ( .not. allocated (NoahmpIO%v_phy)     ) allocate ( NoahmpIO%v_phy      (its:ite,kts:kte    )      ) ! 3d v wind component [m/s]
    if ( .not. allocated (NoahmpIO%p8w)       ) allocate ( NoahmpIO%p8w        (its:ite,kts:kte    )      ) ! 3d pressure, valid at interface [Pa]
 
    ! spatial varying parameter map
    if ( NoahmpIO%iopt_soil > 1 ) then
       if ( .not. allocated (NoahmpIO%soilcomp)) allocate ( NoahmpIO%soilcomp (its:ite,1:2*nsoil)         ) ! soil sand and clay content [fraction]
       if ( .not. allocated (NoahmpIO%soilcl1) ) allocate ( NoahmpIO%soilcl1  (its:ite          )         ) ! soil texture class with depth
       if ( .not. allocated (NoahmpIO%soilcl2) ) allocate ( NoahmpIO%soilcl2  (its:ite          )         ) ! soil texture class with depth
       if ( .not. allocated (NoahmpIO%soilcl3) ) allocate ( NoahmpIO%soilcl3  (its:ite          )         ) ! soil texture class with depth
       if ( .not. allocated (NoahmpIO%soilcl4) ) allocate ( NoahmpIO%soilcl4  (its:ite          )         ) ! soil texture class with depth
    endif
    if ( NoahmpIO%iopt_soil == 4 ) then
       if ( .not. allocated (NoahmpIO%bexp_3d)      ) allocate ( NoahmpIO%bexp_3d       (its:ite,1:nsoil) ) ! c-h b exponent
       if ( .not. allocated (NoahmpIO%smcdry_3d)    ) allocate ( NoahmpIO%smcdry_3d     (its:ite,1:nsoil) ) ! soil moisture limit: dry
       if ( .not. allocated (NoahmpIO%smcwlt_3d)    ) allocate ( NoahmpIO%smcwlt_3d     (its:ite,1:nsoil) ) ! soil moisture limit: wilt
       if ( .not. allocated (NoahmpIO%smcref_3d)    ) allocate ( NoahmpIO%smcref_3d     (its:ite,1:nsoil) ) ! soil moisture limit: reference
       if ( .not. allocated (NoahmpIO%smcmax_3d)    ) allocate ( NoahmpIO%smcmax_3d     (its:ite,1:nsoil) ) ! soil moisture limit: max
       if ( .not. allocated (NoahmpIO%dksat_3d)     ) allocate ( NoahmpIO%dksat_3d      (its:ite,1:nsoil) ) ! saturated soil conductivity
       if ( .not. allocated (NoahmpIO%dwsat_3d)     ) allocate ( NoahmpIO%dwsat_3d      (its:ite,1:nsoil) ) ! saturated soil diffusivity
       if ( .not. allocated (NoahmpIO%psisat_3d)    ) allocate ( NoahmpIO%psisat_3d     (its:ite,1:nsoil) ) ! saturated matric potential
       if ( .not. allocated (NoahmpIO%quartz_3d)    ) allocate ( NoahmpIO%quartz_3d     (its:ite,1:nsoil) ) ! soil quartz content
       if ( .not. allocated (NoahmpIO%refdk_2d)     ) allocate ( NoahmpIO%refdk_2d      (its:ite        ) ) ! reference soil conductivity
       if ( .not. allocated (NoahmpIO%refkdt_2d)    ) allocate ( NoahmpIO%refkdt_2d     (its:ite        ) ) ! soil infiltration parameter
       if ( .not. allocated (NoahmpIO%irr_frac_2d)  ) allocate ( NoahmpIO%irr_frac_2d   (its:ite        ) ) ! irrigation fraction
       if ( .not. allocated (NoahmpIO%irr_har_2d)   ) allocate ( NoahmpIO%irr_har_2d    (its:ite        ) ) ! number of days before harvest date to stop irrigation
       if ( .not. allocated (NoahmpIO%irr_lai_2d)   ) allocate ( NoahmpIO%irr_lai_2d    (its:ite        ) ) ! minimum lai to trigger irrigation
       if ( .not. allocated (NoahmpIO%irr_mad_2d)   ) allocate ( NoahmpIO%irr_mad_2d    (its:ite        ) ) ! management allowable deficit (0-1)
       if ( .not. allocated (NoahmpIO%filoss_2d)    ) allocate ( NoahmpIO%filoss_2d     (its:ite        ) ) ! fraction of flood irrigation loss (0-1)
       if ( .not. allocated (NoahmpIO%sprir_rate_2d)) allocate ( NoahmpIO%sprir_rate_2d (its:ite        ) ) ! mm/h, sprinkler irrigation rate
       if ( .not. allocated (NoahmpIO%micir_rate_2d)) allocate ( NoahmpIO%micir_rate_2d (its:ite        ) ) ! mm/h, micro irrigation rate
       if ( .not. allocated (NoahmpIO%firtfac_2d)   ) allocate ( NoahmpIO%firtfac_2d    (its:ite        ) ) ! flood application rate factor
       if ( .not. allocated (NoahmpIO%ir_rain_2d)   ) allocate ( NoahmpIO%ir_rain_2d    (its:ite        ) ) ! maximum precipitation to stop irrigation trigger
       if ( .not. allocated (NoahmpIO%bvic_2d)      ) allocate ( NoahmpIO%bvic_2d       (its:ite        ) ) ! VIC model infiltration parameter [-]
       if ( .not. allocated (NoahmpIO%axaj_2d)      ) allocate ( NoahmpIO%axaj_2d       (its:ite        ) ) ! tension water distribution inflection parameter [-]
       if ( .not. allocated (NoahmpIO%bxaj_2d)      ) allocate ( NoahmpIO%bxaj_2d       (its:ite        ) ) ! tension water distribution shape parameter [-]
       if ( .not. allocated (NoahmpIO%xxaj_2d)      ) allocate ( NoahmpIO%xxaj_2d       (its:ite        ) ) ! free water distribution shape parameter [-]
       if ( .not. allocated (NoahmpIO%bdvic_2d)     ) allocate ( NoahmpIO%bdvic_2d      (its:ite        ) ) ! DVIC model infiltration parameter [-]
       if ( .not. allocated (NoahmpIO%gdvic_2d)     ) allocate ( NoahmpIO%gdvic_2d      (its:ite        ) ) ! mean capillary drive (m) for infiltration models
       if ( .not. allocated (NoahmpIO%bbvic_2d)     ) allocate ( NoahmpIO%bbvic_2d      (its:ite        ) ) ! dvic heterogeniety parameter for infiltration [-]
       if ( .not. allocated (NoahmpIO%klat_fac)     ) allocate ( NoahmpIO%klat_fac      (its:ite        ) ) ! factor multiplier to hydraulic conductivity
       if ( .not. allocated (NoahmpIO%tdsmc_fac)    ) allocate ( NoahmpIO%tdsmc_fac     (its:ite        ) ) ! factor multiplier to field capacity
       if ( .not. allocated (NoahmpIO%td_dc)        ) allocate ( NoahmpIO%td_dc         (its:ite        ) ) ! drainage coefficient for simple
       if ( .not. allocated (NoahmpIO%td_dcoef)     ) allocate ( NoahmpIO%td_dcoef      (its:ite        ) ) ! drainage coefficient for Hooghoudt
       if ( .not. allocated (NoahmpIO%td_ddrain)    ) allocate ( NoahmpIO%td_ddrain     (its:ite        ) ) ! depth of drain
       if ( .not. allocated (NoahmpIO%td_radi)      ) allocate ( NoahmpIO%td_radi       (its:ite        ) ) ! tile radius
       if ( .not. allocated (NoahmpIO%td_spac)      ) allocate ( NoahmpIO%td_spac       (its:ite        ) ) ! tile spacing
    endif

    ! INOUT (with generic LSM equivalent) (as defined in WRF)
    if ( .not. allocated (NoahmpIO%tsk)      ) allocate ( NoahmpIO%tsk       (its:ite        )            ) ! surface radiative temperature [K]
    if ( .not. allocated (NoahmpIO%hfx)      ) allocate ( NoahmpIO%hfx       (its:ite        )            ) ! sensible heat flux [W m-2]
    if ( .not. allocated (NoahmpIO%qfx)      ) allocate ( NoahmpIO%qfx       (its:ite        )            ) ! latent heat flux [kg s-1 m-2]
    if ( .not. allocated (NoahmpIO%lh)       ) allocate ( NoahmpIO%lh        (its:ite        )            ) ! latent heat flux [W m-2]
    if ( .not. allocated (NoahmpIO%grdflx)   ) allocate ( NoahmpIO%grdflx    (its:ite        )            ) ! ground/snow heat flux [W m-2]
    if ( .not. allocated (NoahmpIO%smstav)   ) allocate ( NoahmpIO%smstav    (its:ite        )            ) ! soil moisture avail. [not used]
    if ( .not. allocated (NoahmpIO%smstot)   ) allocate ( NoahmpIO%smstot    (its:ite        )            ) ! total soil water [mm][not used]
    if ( .not. allocated (NoahmpIO%sfcrunoff)) allocate ( NoahmpIO%sfcrunoff (its:ite        )            ) ! accumulated surface runoff [m]
    if ( .not. allocated (NoahmpIO%udrunoff) ) allocate ( NoahmpIO%udrunoff  (its:ite        )            ) ! accumulated sub-surface runoff [m]
    if ( .not. allocated (NoahmpIO%albedo)   ) allocate ( NoahmpIO%albedo    (its:ite        )            ) ! total grid albedo []
    if ( .not. allocated (NoahmpIO%snowc)    ) allocate ( NoahmpIO%snowc     (its:ite        )            ) ! snow cover fraction []
    if ( .not. allocated (NoahmpIO%snow)     ) allocate ( NoahmpIO%snow      (its:ite        )            ) ! snow water equivalent [mm]
    if ( .not. allocated (NoahmpIO%snowh)    ) allocate ( NoahmpIO%snowh     (its:ite        )            ) ! physical snow depth [m]
    if ( .not. allocated (NoahmpIO%canwat)   ) allocate ( NoahmpIO%canwat    (its:ite        )            ) ! total canopy water + ice [mm]
    if ( .not. allocated (NoahmpIO%acsnom)   ) allocate ( NoahmpIO%acsnom    (its:ite        )            ) ! accumulated snow melt leaving pack
    if ( .not. allocated (NoahmpIO%acsnow)   ) allocate ( NoahmpIO%acsnow    (its:ite        )            ) ! accumulated snow on grid
    if ( .not. allocated (NoahmpIO%emiss)    ) allocate ( NoahmpIO%emiss     (its:ite        )            ) ! surface bulk emissivity
    if ( .not. allocated (NoahmpIO%qsfc)     ) allocate ( NoahmpIO%qsfc      (its:ite        )            ) ! bulk surface specific humidity
    if ( .not. allocated (NoahmpIO%smoiseq)  ) allocate ( NoahmpIO%smoiseq   (its:ite,1:nsoil)            ) ! equilibrium volumetric soil moisture [m3/m3]
    if ( .not. allocated (NoahmpIO%smois)    ) allocate ( NoahmpIO%smois     (its:ite,1:nsoil)            ) ! volumetric soil moisture [m3/m3]
    if ( .not. allocated (NoahmpIO%sh2o)     ) allocate ( NoahmpIO%sh2o      (its:ite,1:nsoil)            ) ! volumetric liquid soil moisture [m3/m3]
    if ( .not. allocated (NoahmpIO%tslb)     ) allocate ( NoahmpIO%tslb      (its:ite,1:nsoil)            ) ! soil temperature [K]

    ! INOUT (with no Noah LSM equivalent) (as defined in WRF)
    if ( .not. allocated (NoahmpIO%isnowxy)   ) allocate ( NoahmpIO%isnowxy    (its:ite               )   ) ! actual no. of snow layers
    if ( .not. allocated (NoahmpIO%tvxy)      ) allocate ( NoahmpIO%tvxy       (its:ite               )   ) ! vegetation leaf temperature
    if ( .not. allocated (NoahmpIO%tgxy)      ) allocate ( NoahmpIO%tgxy       (its:ite               )   ) ! bulk ground surface temperature
    if ( .not. allocated (NoahmpIO%canicexy)  ) allocate ( NoahmpIO%canicexy   (its:ite               )   ) ! canopy-intercepted ice (mm)
    if ( .not. allocated (NoahmpIO%canliqxy)  ) allocate ( NoahmpIO%canliqxy   (its:ite               )   ) ! canopy-intercepted liquid water (mm)
    if ( .not. allocated (NoahmpIO%eahxy)     ) allocate ( NoahmpIO%eahxy      (its:ite               )   ) ! canopy air vapor pressure (Pa)
    if ( .not. allocated (NoahmpIO%tahxy)     ) allocate ( NoahmpIO%tahxy      (its:ite               )   ) ! canopy air temperature (K)
    if ( .not. allocated (NoahmpIO%cmxy)      ) allocate ( NoahmpIO%cmxy       (its:ite               )   ) ! bulk momentum drag coefficient
    if ( .not. allocated (NoahmpIO%chxy)      ) allocate ( NoahmpIO%chxy       (its:ite               )   ) ! bulk sensible heat exchange coefficient
    if ( .not. allocated (NoahmpIO%fwetxy)    ) allocate ( NoahmpIO%fwetxy     (its:ite               )   ) ! wetted or snowed fraction of the canopy (-)
    if ( .not. allocated (NoahmpIO%sneqvoxy)  ) allocate ( NoahmpIO%sneqvoxy   (its:ite               )   ) ! snow mass at last time step(mm H2O)
    if ( .not. allocated (NoahmpIO%alboldxy)  ) allocate ( NoahmpIO%alboldxy   (its:ite               )   ) ! snow albedo at last time step (-)
    if ( .not. allocated (NoahmpIO%qsnowxy)   ) allocate ( NoahmpIO%qsnowxy    (its:ite               )   ) ! snowfall on the ground [mm/s]
    if ( .not. allocated (NoahmpIO%qrainxy)   ) allocate ( NoahmpIO%qrainxy    (its:ite               )   ) ! rainfall on the ground [mm/s]
    if ( .not. allocated (NoahmpIO%wslakexy)  ) allocate ( NoahmpIO%wslakexy   (its:ite               )   ) ! lake water storage [mm]
    if ( .not. allocated (NoahmpIO%zwtxy)     ) allocate ( NoahmpIO%zwtxy      (its:ite               )   ) ! water table depth [m]
    if ( .not. allocated (NoahmpIO%waxy)      ) allocate ( NoahmpIO%waxy       (its:ite               )   ) ! water in the "aquifer" [mm]
    if ( .not. allocated (NoahmpIO%wtxy)      ) allocate ( NoahmpIO%wtxy       (its:ite               )   ) ! groundwater storage [mm]
    if ( .not. allocated (NoahmpIO%smcwtdxy)  ) allocate ( NoahmpIO%smcwtdxy   (its:ite               )   ) ! soil moisture below the bottom of the column (m3 m-3)
    if ( .not. allocated (NoahmpIO%deeprechxy)) allocate ( NoahmpIO%deeprechxy (its:ite               )   ) ! recharge to the water table when deep (m)
    if ( .not. allocated (NoahmpIO%rechxy)    ) allocate ( NoahmpIO%rechxy     (its:ite               )   ) ! recharge to the water table (diagnostic) (m)
    if ( .not. allocated (NoahmpIO%lfmassxy)  ) allocate ( NoahmpIO%lfmassxy   (its:ite               )   ) ! leaf mass [g/m2]
    if ( .not. allocated (NoahmpIO%rtmassxy)  ) allocate ( NoahmpIO%rtmassxy   (its:ite               )   ) ! mass of fine roots [g/m2]
    if ( .not. allocated (NoahmpIO%stmassxy)  ) allocate ( NoahmpIO%stmassxy   (its:ite               )   ) ! stem mass [g/m2]
    if ( .not. allocated (NoahmpIO%woodxy)    ) allocate ( NoahmpIO%woodxy     (its:ite               )   ) ! mass of wood (incl. woody roots) [g/m2]
    if ( .not. allocated (NoahmpIO%grainxy)   ) allocate ( NoahmpIO%grainxy    (its:ite               )   ) ! mass of grain xing [g/m2]
    if ( .not. allocated (NoahmpIO%gddxy)     ) allocate ( NoahmpIO%gddxy      (its:ite               )   ) ! growing degree days xing four
    if ( .not. allocated (NoahmpIO%stblcpxy)  ) allocate ( NoahmpIO%stblcpxy   (its:ite               )   ) ! stable carbon in deep soil [g/m2]
    if ( .not. allocated (NoahmpIO%fastcpxy)  ) allocate ( NoahmpIO%fastcpxy   (its:ite               )   ) ! short-lived carbon, shallow soil [g/m2]
    if ( .not. allocated (NoahmpIO%lai)       ) allocate ( NoahmpIO%lai        (its:ite               )   ) ! leaf area index
    if ( .not. allocated (NoahmpIO%xsaixy)    ) allocate ( NoahmpIO%xsaixy     (its:ite               )   ) ! stem area index
    if ( .not. allocated (NoahmpIO%taussxy)   ) allocate ( NoahmpIO%taussxy    (its:ite               )   ) ! snow age factor
    if ( .not. allocated (NoahmpIO%tsnoxy)    ) allocate ( NoahmpIO%tsnoxy     (its:ite,-nsnow+1:0    )   ) ! snow temperature [K]
    if ( .not. allocated (NoahmpIO%zsnsoxy)   ) allocate ( NoahmpIO%zsnsoxy    (its:ite,-nsnow+1:nsoil)   ) ! snow layer depth [m]
    if ( .not. allocated (NoahmpIO%snicexy)   ) allocate ( NoahmpIO%snicexy    (its:ite,-nsnow+1:0    )   ) ! snow layer ice [mm]
    if ( .not. allocated (NoahmpIO%snliqxy)   ) allocate ( NoahmpIO%snliqxy    (its:ite,-nsnow+1:0    )   ) ! snow layer liquid water [mm]

    ! irrigation
    if ( .not. allocated (NoahmpIO%irfract) ) allocate ( NoahmpIO%irfract (its:ite) ) ! irrigation fraction
    if ( .not. allocated (NoahmpIO%sifract) ) allocate ( NoahmpIO%sifract (its:ite) ) ! sprinkler irrigation fraction
    if ( .not. allocated (NoahmpIO%mifract) ) allocate ( NoahmpIO%mifract (its:ite) ) ! micro irrigation fraction
    if ( .not. allocated (NoahmpIO%fifract) ) allocate ( NoahmpIO%fifract (its:ite) ) ! flood irrigation fraction
    if ( .not. allocated (NoahmpIO%irnumsi) ) allocate ( NoahmpIO%irnumsi (its:ite) ) ! irrigation event number, sprinkler
    if ( .not. allocated (NoahmpIO%irnummi) ) allocate ( NoahmpIO%irnummi (its:ite) ) ! irrigation event number, micro
    if ( .not. allocated (NoahmpIO%irnumfi) ) allocate ( NoahmpIO%irnumfi (its:ite) ) ! irrigation event number, flood
    if ( .not. allocated (NoahmpIO%irwatsi) ) allocate ( NoahmpIO%irwatsi (its:ite) ) ! irrigation water amount [m] to be applied, sprinkler
    if ( .not. allocated (NoahmpIO%irwatmi) ) allocate ( NoahmpIO%irwatmi (its:ite) ) ! irrigation water amount [m] to be applied, micro
    if ( .not. allocated (NoahmpIO%irwatfi) ) allocate ( NoahmpIO%irwatfi (its:ite) ) ! irrigation water amount [m] to be applied, flood
    if ( .not. allocated (NoahmpIO%ireloss) ) allocate ( NoahmpIO%ireloss (its:ite) ) ! loss of irrigation water to evaporation,sprinkler [mm]
    if ( .not. allocated (NoahmpIO%irsivol) ) allocate ( NoahmpIO%irsivol (its:ite) ) ! amount of irrigation by sprinkler (mm)
    if ( .not. allocated (NoahmpIO%irmivol) ) allocate ( NoahmpIO%irmivol (its:ite) ) ! amount of irrigation by micro (mm)
    if ( .not. allocated (NoahmpIO%irfivol) ) allocate ( NoahmpIO%irfivol (its:ite) ) ! amount of irrigation by micro (mm)
    if ( .not. allocated (NoahmpIO%irrsplh) ) allocate ( NoahmpIO%irrsplh (its:ite) ) ! latent heating from sprinkler evaporation (W/m2)
    if ( .not. allocated (NoahmpIO%loctim)  ) allocate ( NoahmpIO%loctim  (its:ite) ) ! local time
  
    ! OUT (with no Noah LSM equivalent) (as defined in WRF)   
    if ( .not. allocated (NoahmpIO%t2mvxy)     ) allocate ( NoahmpIO%t2mvxy      (its:ite) ) ! 2m temperature of vegetation part
    if ( .not. allocated (NoahmpIO%t2mbxy)     ) allocate ( NoahmpIO%t2mbxy      (its:ite) ) ! 2m temperature of bare ground part
    if ( .not. allocated (NoahmpIO%t2mxy)      ) allocate ( NoahmpIO%t2mxy       (its:ite) ) ! 2m grid-mean temperature
    if ( .not. allocated (NoahmpIO%q2mvxy)     ) allocate ( NoahmpIO%q2mvxy      (its:ite) ) ! 2m mixing ratio of vegetation part
    if ( .not. allocated (NoahmpIO%q2mbxy)     ) allocate ( NoahmpIO%q2mbxy      (its:ite) ) ! 2m mixing ratio of bare ground part
    if ( .not. allocated (NoahmpIO%q2mxy)      ) allocate ( NoahmpIO%q2mxy       (its:ite) ) ! 2m grid-mean mixing ratio
    if ( .not. allocated (NoahmpIO%tradxy)     ) allocate ( NoahmpIO%tradxy      (its:ite) ) ! surface radiative temperature (K)
    if ( .not. allocated (NoahmpIO%neexy)      ) allocate ( NoahmpIO%neexy       (its:ite) ) ! net ecosys exchange (g/m2/s CO2)
    if ( .not. allocated (NoahmpIO%gppxy)      ) allocate ( NoahmpIO%gppxy       (its:ite) ) ! gross primary assimilation [g/m2/s C]
    if ( .not. allocated (NoahmpIO%nppxy)      ) allocate ( NoahmpIO%nppxy       (its:ite) ) ! net primary productivity [g/m2/s C]
    if ( .not. allocated (NoahmpIO%fvegxy)     ) allocate ( NoahmpIO%fvegxy      (its:ite) ) ! noah-mp vegetation fraction [-]
    if ( .not. allocated (NoahmpIO%runsfxy)    ) allocate ( NoahmpIO%runsfxy     (its:ite) ) ! surface runoff [mm per soil timestep]
    if ( .not. allocated (NoahmpIO%runsbxy)    ) allocate ( NoahmpIO%runsbxy     (its:ite) ) ! subsurface runoff [mm per soil timestep]
    if ( .not. allocated (NoahmpIO%ecanxy)     ) allocate ( NoahmpIO%ecanxy      (its:ite) ) ! evaporation of intercepted water (mm/s)
    if ( .not. allocated (NoahmpIO%edirxy)     ) allocate ( NoahmpIO%edirxy      (its:ite) ) ! soil surface evaporation rate (mm/s]
    if ( .not. allocated (NoahmpIO%etranxy)    ) allocate ( NoahmpIO%etranxy     (its:ite) ) ! transpiration rate (mm/s)
    if ( .not. allocated (NoahmpIO%fsaxy)      ) allocate ( NoahmpIO%fsaxy       (its:ite) ) ! total absorbed solar radiation (W/m2)
    if ( .not. allocated (NoahmpIO%firaxy)     ) allocate ( NoahmpIO%firaxy      (its:ite) ) ! total net longwave rad (W/m2) [+ to atm]
    if ( .not. allocated (NoahmpIO%aparxy)     ) allocate ( NoahmpIO%aparxy      (its:ite) ) ! photosyn active energy by canopy (W/m2)
    if ( .not. allocated (NoahmpIO%psnxy)      ) allocate ( NoahmpIO%psnxy       (its:ite) ) ! total photosynthesis (umol CO2/m2/s) [+]
    if ( .not. allocated (NoahmpIO%savxy)      ) allocate ( NoahmpIO%savxy       (its:ite) ) ! solar rad absorbed by veg. (W/m2)
    if ( .not. allocated (NoahmpIO%sagxy)      ) allocate ( NoahmpIO%sagxy       (its:ite) ) ! solar rad absorbed by ground (W/m2)
    if ( .not. allocated (NoahmpIO%rssunxy)    ) allocate ( NoahmpIO%rssunxy     (its:ite) ) ! sunlit leaf stomatal resistance (s/m)
    if ( .not. allocated (NoahmpIO%rsshaxy)    ) allocate ( NoahmpIO%rsshaxy     (its:ite) ) ! shaded leaf stomatal resistance (s/m)
    if ( .not. allocated (NoahmpIO%bgapxy)     ) allocate ( NoahmpIO%bgapxy      (its:ite) ) ! between gap fraction
    if ( .not. allocated (NoahmpIO%wgapxy)     ) allocate ( NoahmpIO%wgapxy      (its:ite) ) ! within gap fraction
    if ( .not. allocated (NoahmpIO%tgvxy)      ) allocate ( NoahmpIO%tgvxy       (its:ite) ) ! under canopy ground temperature[K]
    if ( .not. allocated (NoahmpIO%tgbxy)      ) allocate ( NoahmpIO%tgbxy       (its:ite) ) ! bare ground temperature [K]
    if ( .not. allocated (NoahmpIO%chvxy)      ) allocate ( NoahmpIO%chvxy       (its:ite) ) ! sensible heat exchange coefficient vegetated
    if ( .not. allocated (NoahmpIO%chbxy)      ) allocate ( NoahmpIO%chbxy       (its:ite) ) ! sensible heat exchange coefficient bare-ground
    if ( .not. allocated (NoahmpIO%shgxy)      ) allocate ( NoahmpIO%shgxy       (its:ite) ) ! veg ground sen. heat [W/m2]   [+ to atm]
    if ( .not. allocated (NoahmpIO%shcxy)      ) allocate ( NoahmpIO%shcxy       (its:ite) ) ! canopy sen. heat [W/m2]   [+ to atm]
    if ( .not. allocated (NoahmpIO%shbxy)      ) allocate ( NoahmpIO%shbxy       (its:ite) ) ! bare sensible heat [W/m2]  [+ to atm]
    if ( .not. allocated (NoahmpIO%evgxy)      ) allocate ( NoahmpIO%evgxy       (its:ite) ) ! veg ground evap. heat [W/m2]  [+ to atm]
    if ( .not. allocated (NoahmpIO%evbxy)      ) allocate ( NoahmpIO%evbxy       (its:ite) ) ! bare soil evaporation [W/m2]  [+ to atm]
    if ( .not. allocated (NoahmpIO%ghvxy)      ) allocate ( NoahmpIO%ghvxy       (its:ite) ) ! veg ground heat flux [W/m2]  [+ to soil]
    if ( .not. allocated (NoahmpIO%ghbxy)      ) allocate ( NoahmpIO%ghbxy       (its:ite) ) ! bare ground heat flux [W/m2] [+ to soil]
    if ( .not. allocated (NoahmpIO%irgxy)      ) allocate ( NoahmpIO%irgxy       (its:ite) ) ! veg ground net lw rad. [W/m2] [+ to atm]
    if ( .not. allocated (NoahmpIO%ircxy)      ) allocate ( NoahmpIO%ircxy       (its:ite) ) ! canopy net lw rad. [W/m2] [+ to atm]
    if ( .not. allocated (NoahmpIO%irbxy)      ) allocate ( NoahmpIO%irbxy       (its:ite) ) ! bare net longwave rad. [W/m2] [+ to atm]
    if ( .not. allocated (NoahmpIO%trxy)       ) allocate ( NoahmpIO%trxy        (its:ite) ) ! transpiration [w/m2]  [+ to atm]
    if ( .not. allocated (NoahmpIO%evcxy)      ) allocate ( NoahmpIO%evcxy       (its:ite) ) ! canopy evaporation heat [W/m2]  [+ to atm]
    if ( .not. allocated (NoahmpIO%chleafxy)   ) allocate ( NoahmpIO%chleafxy    (its:ite) ) ! leaf exchange coefficient
    if ( .not. allocated (NoahmpIO%chucxy)     ) allocate ( NoahmpIO%chucxy      (its:ite) ) ! under canopy exchange coefficient
    if ( .not. allocated (NoahmpIO%chv2xy)     ) allocate ( NoahmpIO%chv2xy      (its:ite) ) ! veg 2m exchange coefficient
    if ( .not. allocated (NoahmpIO%chb2xy)     ) allocate ( NoahmpIO%chb2xy      (its:ite) ) ! bare 2m exchange coefficient
    if ( .not. allocated (NoahmpIO%rs)         ) allocate ( NoahmpIO%rs          (its:ite) ) ! total stomatal resistance (s/m)
    if ( .not. allocated (NoahmpIO%z0)         ) allocate ( NoahmpIO%z0          (its:ite) ) ! roughness length output to WRF
    if ( .not. allocated (NoahmpIO%znt)        ) allocate ( NoahmpIO%znt         (its:ite) ) ! roughness length output to WRF
    if ( .not. allocated (NoahmpIO%qtdrain)    ) allocate ( NoahmpIO%qtdrain     (its:ite) ) ! tile drainage (mm)
    if ( .not. allocated (NoahmpIO%td_fraction)) allocate ( NoahmpIO%td_fraction (its:ite) ) ! tile drainage fraction
    if ( .not. allocated (NoahmpIO%xlong)      ) allocate ( NoahmpIO%xlong       (its:ite) ) ! longitude
    if ( .not. allocated (NoahmpIO%terrain)    ) allocate ( NoahmpIO%terrain     (its:ite) ) ! terrain height
    if ( .not. allocated (NoahmpIO%gvfmin)     ) allocate ( NoahmpIO%gvfmin      (its:ite) ) ! annual minimum in vegetation fraction
    if ( .not. allocated (NoahmpIO%gvfmax)     ) allocate ( NoahmpIO%gvfmax      (its:ite) ) ! annual maximum in vegetation fraction

    ! additional output variables
    if ( .not. allocated (NoahmpIO%pahxy)       ) allocate ( NoahmpIO%pahxy        (its:ite)         )
    if ( .not. allocated (NoahmpIO%pahgxy)      ) allocate ( NoahmpIO%pahgxy       (its:ite)         )
    if ( .not. allocated (NoahmpIO%pahbxy)      ) allocate ( NoahmpIO%pahbxy       (its:ite)         )
    if ( .not. allocated (NoahmpIO%pahvxy)      ) allocate ( NoahmpIO%pahvxy       (its:ite)         )
    if ( .not. allocated (NoahmpIO%qintsxy)     ) allocate ( NoahmpIO%qintsxy      (its:ite)         )
    if ( .not. allocated (NoahmpIO%qintrxy)     ) allocate ( NoahmpIO%qintrxy      (its:ite)         )
    if ( .not. allocated (NoahmpIO%qdripsxy)    ) allocate ( NoahmpIO%qdripsxy     (its:ite)         )
    if ( .not. allocated (NoahmpIO%qdriprxy)    ) allocate ( NoahmpIO%qdriprxy     (its:ite)         )
    if ( .not. allocated (NoahmpIO%qthrosxy)    ) allocate ( NoahmpIO%qthrosxy     (its:ite)         )
    if ( .not. allocated (NoahmpIO%qthrorxy)    ) allocate ( NoahmpIO%qthrorxy     (its:ite)         )
    if ( .not. allocated (NoahmpIO%qsnsubxy)    ) allocate ( NoahmpIO%qsnsubxy     (its:ite)         )
    if ( .not. allocated (NoahmpIO%qsnfroxy)    ) allocate ( NoahmpIO%qsnfroxy     (its:ite)         )
    if ( .not. allocated (NoahmpIO%qsubcxy)     ) allocate ( NoahmpIO%qsubcxy      (its:ite)         )
    if ( .not. allocated (NoahmpIO%qfrocxy)     ) allocate ( NoahmpIO%qfrocxy      (its:ite)         )
    if ( .not. allocated (NoahmpIO%qevacxy)     ) allocate ( NoahmpIO%qevacxy      (its:ite)         )
    if ( .not. allocated (NoahmpIO%qdewcxy)     ) allocate ( NoahmpIO%qdewcxy      (its:ite)         )
    if ( .not. allocated (NoahmpIO%qfrzcxy)     ) allocate ( NoahmpIO%qfrzcxy      (its:ite)         )
    if ( .not. allocated (NoahmpIO%qmeltcxy)    ) allocate ( NoahmpIO%qmeltcxy     (its:ite)         )
    if ( .not. allocated (NoahmpIO%qsnbotxy)    ) allocate ( NoahmpIO%qsnbotxy     (its:ite)         )
    if ( .not. allocated (NoahmpIO%qmeltxy)     ) allocate ( NoahmpIO%qmeltxy      (its:ite)         )
    if ( .not. allocated (NoahmpIO%pondingxy)   ) allocate ( NoahmpIO%pondingxy    (its:ite)         )
    if ( .not. allocated (NoahmpIO%fpicexy)     ) allocate ( NoahmpIO%fpicexy      (its:ite)         )
    if ( .not. allocated (NoahmpIO%rainlsm)     ) allocate ( NoahmpIO%rainlsm      (its:ite)         )
    if ( .not. allocated (NoahmpIO%snowlsm)     ) allocate ( NoahmpIO%snowlsm      (its:ite)         )
    if ( .not. allocated (NoahmpIO%forctlsm)    ) allocate ( NoahmpIO%forctlsm     (its:ite)         )
    if ( .not. allocated (NoahmpIO%forcqlsm)    ) allocate ( NoahmpIO%forcqlsm     (its:ite)         )
    if ( .not. allocated (NoahmpIO%forcplsm)    ) allocate ( NoahmpIO%forcplsm     (its:ite)         )
    if ( .not. allocated (NoahmpIO%forczlsm)    ) allocate ( NoahmpIO%forczlsm     (its:ite)         )
    if ( .not. allocated (NoahmpIO%forcwlsm)    ) allocate ( NoahmpIO%forcwlsm     (its:ite)         )
    if ( .not. allocated (NoahmpIO%eflxbxy)     ) allocate ( NoahmpIO%eflxbxy      (its:ite)         )
    if ( .not. allocated (NoahmpIO%soilenergy)  ) allocate ( NoahmpIO%soilenergy   (its:ite)         )
    if ( .not. allocated (NoahmpIO%snowenergy)  ) allocate ( NoahmpIO%snowenergy   (its:ite)         )
    if ( .not. allocated (NoahmpIO%canhsxy)     ) allocate ( NoahmpIO%canhsxy      (its:ite)         )
    if ( .not. allocated (NoahmpIO%acc_dwaterxy)) allocate ( NoahmpIO%acc_dwaterxy (its:ite)         )
    if ( .not. allocated (NoahmpIO%acc_prcpxy)  ) allocate ( NoahmpIO%acc_prcpxy   (its:ite)         )
    if ( .not. allocated (NoahmpIO%acc_ecanxy)  ) allocate ( NoahmpIO%acc_ecanxy   (its:ite)         )
    if ( .not. allocated (NoahmpIO%acc_etranxy) ) allocate ( NoahmpIO%acc_etranxy  (its:ite)         )
    if ( .not. allocated (NoahmpIO%acc_edirxy)  ) allocate ( NoahmpIO%acc_edirxy   (its:ite)         )
    if ( .not. allocated (NoahmpIO%acc_ssoilxy) ) allocate ( NoahmpIO%acc_ssoilxy  (its:ite)         )
    if ( .not. allocated (NoahmpIO%acc_qinsurxy)) allocate ( NoahmpIO%acc_qinsurxy (its:ite)         )
    if ( .not. allocated (NoahmpIO%acc_qsevaxy) ) allocate ( NoahmpIO%acc_qsevaxy  (its:ite)         )
    if ( .not. allocated (NoahmpIO%acc_etranixy)) allocate ( NoahmpIO%acc_etranixy (its:ite,1:nsoil) )

    ! needed for mmf_runoff (iopt_run = 5); not part of mp driver in WRF
    if ( .not. allocated (NoahmpIO%msftx)      ) allocate ( NoahmpIO%msftx       (its:ite) )
    if ( .not. allocated (NoahmpIO%msfty)      ) allocate ( NoahmpIO%msfty       (its:ite) )
    if ( .not. allocated (NoahmpIO%eqzwt)      ) allocate ( NoahmpIO%eqzwt       (its:ite) )
    if ( .not. allocated (NoahmpIO%riverbedxy) ) allocate ( NoahmpIO%riverbedxy  (its:ite) )
    if ( .not. allocated (NoahmpIO%rivercondxy)) allocate ( NoahmpIO%rivercondxy (its:ite) )
    if ( .not. allocated (NoahmpIO%pexpxy)     ) allocate ( NoahmpIO%pexpxy      (its:ite) )
    if ( .not. allocated (NoahmpIO%fdepthxy)   ) allocate ( NoahmpIO%fdepthxy    (its:ite) )
    if ( .not. allocated (NoahmpIO%areaxy)     ) allocate ( NoahmpIO%areaxy      (its:ite) )
    if ( .not. allocated (NoahmpIO%qrfsxy)     ) allocate ( NoahmpIO%qrfsxy      (its:ite) )
    if ( .not. allocated (NoahmpIO%qspringsxy) ) allocate ( NoahmpIO%qspringsxy  (its:ite) )
    if ( .not. allocated (NoahmpIO%qrfxy)      ) allocate ( NoahmpIO%qrfxy       (its:ite) )
    if ( .not. allocated (NoahmpIO%qspringxy)  ) allocate ( NoahmpIO%qspringxy   (its:ite) )
    if ( .not. allocated (NoahmpIO%qslatxy)    ) allocate ( NoahmpIO%qslatxy     (its:ite) )
    if ( .not. allocated (NoahmpIO%qlatxy)     ) allocate ( NoahmpIO%qlatxy      (its:ite) )
    if ( .not. allocated (NoahmpIO%rechclim)   ) allocate ( NoahmpIO%rechclim    (its:ite) )
    if ( .not. allocated (NoahmpIO%rivermask)  ) allocate ( NoahmpIO%rivermask   (its:ite) )
    if ( .not. allocated (NoahmpIO%nonriverxy) ) allocate ( NoahmpIO%nonriverxy  (its:ite) )

    ! needed for crop model (opt_crop=1)
    if ( .not. allocated (NoahmpIO%pgsxy)     ) allocate ( NoahmpIO%pgsxy      (its:ite)   )
    if ( .not. allocated (NoahmpIO%cropcat)   ) allocate ( NoahmpIO%cropcat    (its:ite)   )
    if ( .not. allocated (NoahmpIO%planting)  ) allocate ( NoahmpIO%planting   (its:ite)   )
    if ( .not. allocated (NoahmpIO%harvest)   ) allocate ( NoahmpIO%harvest    (its:ite)   )
    if ( .not. allocated (NoahmpIO%season_gdd)) allocate ( NoahmpIO%season_gdd (its:ite)   )
    if ( .not. allocated (NoahmpIO%croptype)  ) allocate ( NoahmpIO%croptype   (its:ite,5) )

    ! Single- and Multi-layer Urban Models
    if ( NoahmpIO%sf_urban_physics > 0 )  then
       if ( .not. allocated (NoahmpIO%sh_urb2d)   ) allocate ( NoahmpIO%sh_urb2d    (its:ite) )
       if ( .not. allocated (NoahmpIO%lh_urb2d)   ) allocate ( NoahmpIO%lh_urb2d    (its:ite) )
       if ( .not. allocated (NoahmpIO%g_urb2d)    ) allocate ( NoahmpIO%g_urb2d     (its:ite) )
       if ( .not. allocated (NoahmpIO%rn_urb2d)   ) allocate ( NoahmpIO%rn_urb2d    (its:ite) )
       if ( .not. allocated (NoahmpIO%ts_urb2d)   ) allocate ( NoahmpIO%ts_urb2d    (its:ite) )
       if ( .not. allocated (NoahmpIO%hrang)      ) allocate ( NoahmpIO%hrang       (its:ite) )
       if ( .not. allocated (NoahmpIO%frc_urb2d)  ) allocate ( NoahmpIO%frc_urb2d   (its:ite) )
       if ( .not. allocated (NoahmpIO%utype_urb2d)) allocate ( NoahmpIO%utype_urb2d (its:ite) )
       if ( .not. allocated (NoahmpIO%lp_urb2d)   ) allocate ( NoahmpIO%lp_urb2d    (its:ite) )
       if ( .not. allocated (NoahmpIO%lb_urb2d)   ) allocate ( NoahmpIO%lb_urb2d    (its:ite) )
       if ( .not. allocated (NoahmpIO%hgt_urb2d)  ) allocate ( NoahmpIO%hgt_urb2d   (its:ite) )
       if ( .not. allocated (NoahmpIO%ust)        ) allocate ( NoahmpIO%ust         (its:ite) )
       !endif
         
       !if(NoahmpIO%sf_urban_physics == 1 ) then  ! single layer urban model
       if ( .not. allocated (NoahmpIO%cmr_sfcdif)   ) allocate ( NoahmpIO%cmr_sfcdif    (its:ite)         )
       if ( .not. allocated (NoahmpIO%chr_sfcdif)   ) allocate ( NoahmpIO%chr_sfcdif    (its:ite)         )
       if ( .not. allocated (NoahmpIO%cmc_sfcdif)   ) allocate ( NoahmpIO%cmc_sfcdif    (its:ite)         )
       if ( .not. allocated (NoahmpIO%chc_sfcdif)   ) allocate ( NoahmpIO%chc_sfcdif    (its:ite)         )
       if ( .not. allocated (NoahmpIO%cmgr_sfcdif)  ) allocate ( NoahmpIO%cmgr_sfcdif   (its:ite)         )
       if ( .not. allocated (NoahmpIO%chgr_sfcdif)  ) allocate ( NoahmpIO%chgr_sfcdif   (its:ite)         )
       if ( .not. allocated (NoahmpIO%tr_urb2d)     ) allocate ( NoahmpIO%tr_urb2d      (its:ite)         )
       if ( .not. allocated (NoahmpIO%tb_urb2d)     ) allocate ( NoahmpIO%tb_urb2d      (its:ite)         )
       if ( .not. allocated (NoahmpIO%tg_urb2d)     ) allocate ( NoahmpIO%tg_urb2d      (its:ite)         )
       if ( .not. allocated (NoahmpIO%tc_urb2d)     ) allocate ( NoahmpIO%tc_urb2d      (its:ite)         )
       if ( .not. allocated (NoahmpIO%qc_urb2d)     ) allocate ( NoahmpIO%qc_urb2d      (its:ite)         )
       if ( .not. allocated (NoahmpIO%uc_urb2d)     ) allocate ( NoahmpIO%uc_urb2d      (its:ite)         )
       if ( .not. allocated (NoahmpIO%xxxr_urb2d)   ) allocate ( NoahmpIO%xxxr_urb2d    (its:ite)         )
       if ( .not. allocated (NoahmpIO%xxxb_urb2d)   ) allocate ( NoahmpIO%xxxb_urb2d    (its:ite)         )
       if ( .not. allocated (NoahmpIO%xxxg_urb2d)   ) allocate ( NoahmpIO%xxxg_urb2d    (its:ite)         )
       if ( .not. allocated (NoahmpIO%xxxc_urb2d)   ) allocate ( NoahmpIO%xxxc_urb2d    (its:ite)         )
       if ( .not. allocated (NoahmpIO%psim_urb2d)   ) allocate ( NoahmpIO%psim_urb2d    (its:ite)         )
       if ( .not. allocated (NoahmpIO%psih_urb2d)   ) allocate ( NoahmpIO%psih_urb2d    (its:ite)         )
       if ( .not. allocated (NoahmpIO%u10_urb2d)    ) allocate ( NoahmpIO%u10_urb2d     (its:ite)         )
       if ( .not. allocated (NoahmpIO%v10_urb2d)    ) allocate ( NoahmpIO%v10_urb2d     (its:ite)         )
       if ( .not. allocated (NoahmpIO%gz1oz0_urb2d) ) allocate ( NoahmpIO%gz1oz0_urb2d  (its:ite)         )
       if ( .not. allocated (NoahmpIO%akms_urb2d)   ) allocate ( NoahmpIO%akms_urb2d    (its:ite)         )
       if ( .not. allocated (NoahmpIO%th2_urb2d)    ) allocate ( NoahmpIO%th2_urb2d     (its:ite)         )
       if ( .not. allocated (NoahmpIO%q2_urb2d)     ) allocate ( NoahmpIO%q2_urb2d      (its:ite)         )
       if ( .not. allocated (NoahmpIO%ust_urb2d)    ) allocate ( NoahmpIO%ust_urb2d     (its:ite)         )
       if ( .not. allocated (NoahmpIO%cmcr_urb2d)   ) allocate ( NoahmpIO%cmcr_urb2d    (its:ite)         )
       if ( .not. allocated (NoahmpIO%tgr_urb2d)    ) allocate ( NoahmpIO%tgr_urb2d     (its:ite)         )
       if ( .not. allocated (NoahmpIO%drelr_urb2d)  ) allocate ( NoahmpIO%drelr_urb2d   (its:ite)         )
       if ( .not. allocated (NoahmpIO%drelb_urb2d)  ) allocate ( NoahmpIO%drelb_urb2d   (its:ite)         )
       if ( .not. allocated (NoahmpIO%drelg_urb2d)  ) allocate ( NoahmpIO%drelg_urb2d   (its:ite)         )
       if ( .not. allocated (NoahmpIO%flxhumr_urb2d)) allocate ( NoahmpIO%flxhumr_urb2d (its:ite)         )
       if ( .not. allocated (NoahmpIO%flxhumb_urb2d)) allocate ( NoahmpIO%flxhumb_urb2d (its:ite)         )
       if ( .not. allocated (NoahmpIO%flxhumg_urb2d)) allocate ( NoahmpIO%flxhumg_urb2d (its:ite)         )
       if ( .not. allocated (NoahmpIO%chs)          ) allocate ( NoahmpIO%chs           (its:ite)         )
       if ( .not. allocated (NoahmpIO%chs2)         ) allocate ( NoahmpIO%chs2          (its:ite)         )
       if ( .not. allocated (NoahmpIO%cqs2)         ) allocate ( NoahmpIO%cqs2          (its:ite)         )
       if ( .not. allocated (NoahmpIO%mh_urb2d)     ) allocate ( NoahmpIO%mh_urb2d      (its:ite)         )
       if ( .not. allocated (NoahmpIO%stdh_urb2d)   ) allocate ( NoahmpIO%stdh_urb2d    (its:ite)         )
       if ( .not. allocated (NoahmpIO%lf_urb2d)     ) allocate ( NoahmpIO%lf_urb2d      (its:ite,4)       )
       if ( .not. allocated (NoahmpIO%trl_urb3d)    ) allocate ( NoahmpIO%trl_urb3d     (its:ite,1:nsoil) )
       if ( .not. allocated (NoahmpIO%tbl_urb3d)    ) allocate ( NoahmpIO%tbl_urb3d     (its:ite,1:nsoil) )
       if ( .not. allocated (NoahmpIO%tgl_urb3d)    ) allocate ( NoahmpIO%tgl_urb3d     (its:ite,1:nsoil) )
       if ( .not. allocated (NoahmpIO%tgrl_urb3d)   ) allocate ( NoahmpIO%tgrl_urb3d    (its:ite,1:nsoil) )
       if ( .not. allocated (NoahmpIO%smr_urb3d)    ) allocate ( NoahmpIO%smr_urb3d     (its:ite,1:nsoil) )
       if ( .not. allocated (NoahmpIO%dzr)          ) allocate ( NoahmpIO%dzr           (            1:nsoil) )
       if ( .not. allocated (NoahmpIO%dzb)          ) allocate ( NoahmpIO%dzb           (            1:nsoil) )
       if ( .not. allocated (NoahmpIO%dzg)          ) allocate ( NoahmpIO%dzg           (            1:nsoil) )
       !endif

       !if(sf_urban_physics == 2 .or. sf_urban_physics == 3) then  ! bep or bem urban models
       if ( .not. allocated (NoahmpIO%trb_urb4d)  ) allocate ( NoahmpIO%trb_urb4d   (its:ite,NoahmpIO%urban_map_zrd) )
       if ( .not. allocated (NoahmpIO%tw1_urb4d)  ) allocate ( NoahmpIO%tw1_urb4d   (its:ite,NoahmpIO%urban_map_zwd) )
       if ( .not. allocated (NoahmpIO%tw2_urb4d)  ) allocate ( NoahmpIO%tw2_urb4d   (its:ite,NoahmpIO%urban_map_zwd) )
       if ( .not. allocated (NoahmpIO%tgb_urb4d)  ) allocate ( NoahmpIO%tgb_urb4d   (its:ite,NoahmpIO%urban_map_gd ) )
       if ( .not. allocated (NoahmpIO%sfw1_urb3d) ) allocate ( NoahmpIO%sfw1_urb3d  (its:ite,NoahmpIO%urban_map_zd ) )
       if ( .not. allocated (NoahmpIO%sfw2_urb3d) ) allocate ( NoahmpIO%sfw2_urb3d  (its:ite,NoahmpIO%urban_map_zd ) )
       if ( .not. allocated (NoahmpIO%sfr_urb3d)  ) allocate ( NoahmpIO%sfr_urb3d   (its:ite,NoahmpIO%urban_map_zdf) )
       if ( .not. allocated (NoahmpIO%sfg_urb3d)  ) allocate ( NoahmpIO%sfg_urb3d   (its:ite,NoahmpIO%num_urban_ndm) )
       if ( .not. allocated (NoahmpIO%hi_urb2d)   ) allocate ( NoahmpIO%hi_urb2d    (its:ite,NoahmpIO%num_urban_hi ) )
       if ( .not. allocated (NoahmpIO%theta_urban)) allocate ( NoahmpIO%theta_urban (its:ite,kts:kte               ) )
       if ( .not. allocated (NoahmpIO%u_urban)    ) allocate ( NoahmpIO%u_urban     (its:ite,kts:kte               ) )
       if ( .not. allocated (NoahmpIO%v_urban)    ) allocate ( NoahmpIO%v_urban     (its:ite,kts:kte               ) )
       if ( .not. allocated (NoahmpIO%dz_urban)   ) allocate ( NoahmpIO%dz_urban    (its:ite,kts:kte               ) )
       if ( .not. allocated (NoahmpIO%rho_urban)  ) allocate ( NoahmpIO%rho_urban   (its:ite,kts:kte               ) )
       if ( .not. allocated (NoahmpIO%p_urban)    ) allocate ( NoahmpIO%p_urban     (its:ite,kts:kte               ) )
       if ( .not. allocated (NoahmpIO%a_u_bep)    ) allocate ( NoahmpIO%a_u_bep     (its:ite,kts:kte               ) )
       if ( .not. allocated (NoahmpIO%a_v_bep)    ) allocate ( NoahmpIO%a_v_bep     (its:ite,kts:kte               ) )
       if ( .not. allocated (NoahmpIO%a_t_bep)    ) allocate ( NoahmpIO%a_t_bep     (its:ite,kts:kte               ) )
       if ( .not. allocated (NoahmpIO%a_q_bep)    ) allocate ( NoahmpIO%a_q_bep     (its:ite,kts:kte               ) )
       if ( .not. allocated (NoahmpIO%a_e_bep)    ) allocate ( NoahmpIO%a_e_bep     (its:ite,kts:kte               ) )
       if ( .not. allocated (NoahmpIO%b_u_bep)    ) allocate ( NoahmpIO%b_u_bep     (its:ite,kts:kte               ) )
       if ( .not. allocated (NoahmpIO%b_v_bep)    ) allocate ( NoahmpIO%b_v_bep     (its:ite,kts:kte               ) )
       if ( .not. allocated (NoahmpIO%b_t_bep)    ) allocate ( NoahmpIO%b_t_bep     (its:ite,kts:kte               ) )
       if ( .not. allocated (NoahmpIO%b_q_bep)    ) allocate ( NoahmpIO%b_q_bep     (its:ite,kts:kte               ) )
       if ( .not. allocated (NoahmpIO%b_e_bep)    ) allocate ( NoahmpIO%b_e_bep     (its:ite,kts:kte               ) )
       if ( .not. allocated (NoahmpIO%dlg_bep)    ) allocate ( NoahmpIO%dlg_bep     (its:ite,kts:kte               ) )
       if ( .not. allocated (NoahmpIO%dl_u_bep)   ) allocate ( NoahmpIO%dl_u_bep    (its:ite,kts:kte               ) )
       if ( .not. allocated (NoahmpIO%sf_bep)     ) allocate ( NoahmpIO%sf_bep      (its:ite,kts:kte               ) )
       if ( .not. allocated (NoahmpIO%vl_bep)     ) allocate ( NoahmpIO%vl_bep      (its:ite,kts:kte               ) )
       !endif

        !if(sf_urban_physics == 3) then  ! bem urban model
       if ( .not. allocated (NoahmpIO%tlev_urb3d)   ) allocate ( NoahmpIO%tlev_urb3d    (its:ite,NoahmpIO%urban_map_bd  ) )
       if ( .not. allocated (NoahmpIO%qlev_urb3d)   ) allocate ( NoahmpIO%qlev_urb3d    (its:ite,NoahmpIO%urban_map_bd  ) )
       if ( .not. allocated (NoahmpIO%tw1lev_urb3d) ) allocate ( NoahmpIO%tw1lev_urb3d  (its:ite,NoahmpIO%urban_map_wd  ) )
       if ( .not. allocated (NoahmpIO%tw2lev_urb3d) ) allocate ( NoahmpIO%tw2lev_urb3d  (its:ite,NoahmpIO%urban_map_wd  ) )
       if ( .not. allocated (NoahmpIO%tglev_urb3d)  ) allocate ( NoahmpIO%tglev_urb3d   (its:ite,NoahmpIO%urban_map_gbd ) )
       if ( .not. allocated (NoahmpIO%tflev_urb3d)  ) allocate ( NoahmpIO%tflev_urb3d   (its:ite,NoahmpIO%urban_map_fbd ) )
       if ( .not. allocated (NoahmpIO%sf_ac_urb3d)  ) allocate ( NoahmpIO%sf_ac_urb3d   (its:ite                        ) )
       if ( .not. allocated (NoahmpIO%lf_ac_urb3d)  ) allocate ( NoahmpIO%lf_ac_urb3d   (its:ite                        ) )
       if ( .not. allocated (NoahmpIO%cm_ac_urb3d)  ) allocate ( NoahmpIO%cm_ac_urb3d   (its:ite                        ) )
       if ( .not. allocated (NoahmpIO%sfvent_urb3d) ) allocate ( NoahmpIO%sfvent_urb3d  (its:ite                        ) )
       if ( .not. allocated (NoahmpIO%lfvent_urb3d) ) allocate ( NoahmpIO%lfvent_urb3d  (its:ite                        ) )
       if ( .not. allocated (NoahmpIO%sfwin1_urb3d) ) allocate ( NoahmpIO%sfwin1_urb3d  (its:ite,NoahmpIO%urban_map_wd  ) )
       if ( .not. allocated (NoahmpIO%sfwin2_urb3d) ) allocate ( NoahmpIO%sfwin2_urb3d  (its:ite,NoahmpIO%urban_map_wd  ) )
       if ( .not. allocated (NoahmpIO%ep_pv_urb3d)  ) allocate ( NoahmpIO%ep_pv_urb3d   (its:ite                        ) )
       if ( .not. allocated (NoahmpIO%t_pv_urb3d)   ) allocate ( NoahmpIO%t_pv_urb3d    (its:ite,NoahmpIO%urban_map_zdf ) )
       if ( .not. allocated (NoahmpIO%trv_urb4d)    ) allocate ( NoahmpIO%trv_urb4d     (its:ite,NoahmpIO%urban_map_zgrd) )
       if ( .not. allocated (NoahmpIO%qr_urb4d)     ) allocate ( NoahmpIO%qr_urb4d      (its:ite,NoahmpIO%urban_map_zgrd) )
       if ( .not. allocated (NoahmpIO%qgr_urb3d)    ) allocate ( NoahmpIO%qgr_urb3d     (its:ite                        ) )
       if ( .not. allocated (NoahmpIO%tgr_urb3d)    ) allocate ( NoahmpIO%tgr_urb3d     (its:ite                        ) )
       if ( .not. allocated (NoahmpIO%drain_urb4d)  ) allocate ( NoahmpIO%drain_urb4d   (its:ite,NoahmpIO%urban_map_zdf ) )
       if ( .not. allocated (NoahmpIO%draingr_urb3d)) allocate ( NoahmpIO%draingr_urb3d (its:ite                        ) )
       if ( .not. allocated (NoahmpIO%sfrv_urb3d)   ) allocate ( NoahmpIO%sfrv_urb3d    (its:ite,NoahmpIO%urban_map_zdf ) )
       if ( .not. allocated (NoahmpIO%lfrv_urb3d)   ) allocate ( NoahmpIO%lfrv_urb3d    (its:ite,NoahmpIO%urban_map_zdf ) )
       if ( .not. allocated (NoahmpIO%dgr_urb3d)    ) allocate ( NoahmpIO%dgr_urb3d     (its:ite,NoahmpIO%urban_map_zdf ) )
       if ( .not. allocated (NoahmpIO%dg_urb3d)     ) allocate ( NoahmpIO%dg_urb3d      (its:ite,NoahmpIO%num_urban_ndm ) )
       if ( .not. allocated (NoahmpIO%lfr_urb3d)    ) allocate ( NoahmpIO%lfr_urb3d     (its:ite,NoahmpIO%urban_map_zdf ) )
       if ( .not. allocated (NoahmpIO%lfg_urb3d)    ) allocate ( NoahmpIO%lfg_urb3d     (its:ite,NoahmpIO%num_urban_ndm ) )

    endif

#ifdef WRF_HYDRO
    if ( .not. allocated (NoahmpIO%infxsrt)   ) allocate ( NoahmpIO%infxsrt    (its:ite) )
    if ( .not. allocated (NoahmpIO%sfcheadrt) ) allocate ( NoahmpIO%sfcheadrt  (its:ite) )
    if ( .not. allocated (NoahmpIO%soldrain)  ) allocate ( NoahmpIO%soldrain   (its:ite) )
    if ( .not. allocated (NoahmpIO%qtiledrain)) allocate ( NoahmpIO%qtiledrain (its:ite) )
    if ( .not. allocated (NoahmpIO%zwatble2d) ) allocate ( NoahmpIO%zwatble2d  (its:ite) )
#endif    

    !-------------------------------------------------------------------
    ! Initialize variables with default values 
    !-------------------------------------------------------------------
    
    NoahmpIO%ice             = undefined_int
    NoahmpIO%ivgtyp          = undefined_int
    NoahmpIO%isltyp          = undefined_int
    NoahmpIO%isnowxy         = undefined_int
    NoahmpIO%coszen          = undefined_real
    NoahmpIO%xlat            = undefined_real
    NoahmpIO%dz8w            = undefined_real
    NoahmpIO%dzs             = undefined_real
    NoahmpIO%zsoil           = undefined_real
    NoahmpIO%vegfra          = undefined_real
    NoahmpIO%tmn             = undefined_real
    NoahmpIO%xland           = undefined_real
    NoahmpIO%xice            = undefined_real
    NoahmpIO%t_phy           = undefined_real
    NoahmpIO%qv_curr         = undefined_real
    NoahmpIO%u_phy           = undefined_real
    NoahmpIO%v_phy           = undefined_real
    NoahmpIO%swdown          = undefined_real
    NoahmpIO%swddir          = undefined_real
    NoahmpIO%swddif          = undefined_real
    NoahmpIO%glw             = undefined_real
    NoahmpIO%p8w             = undefined_real
    NoahmpIO%rainbl          = undefined_real
    NoahmpIO%snowbl          = undefined_real
    NoahmpIO%sr              = undefined_real
    NoahmpIO%raincv          = undefined_real
    NoahmpIO%rainncv         = undefined_real
    NoahmpIO%rainshv         = undefined_real
    NoahmpIO%snowncv         = undefined_real
    NoahmpIO%graupelncv      = undefined_real
    NoahmpIO%hailncv         = undefined_real
    NoahmpIO%qsfc            = undefined_real
    NoahmpIO%tsk             = undefined_real
    NoahmpIO%qfx             = undefined_real
    NoahmpIO%smstav          = undefined_real
    NoahmpIO%smstot          = undefined_real
    NoahmpIO%smois           = undefined_real
    NoahmpIO%sh2o            = undefined_real
    NoahmpIO%tslb            = undefined_real
    NoahmpIO%snow            = undefined_real
    NoahmpIO%snowh           = undefined_real
    NoahmpIO%canwat          = undefined_real
    NoahmpIO%smoiseq         = undefined_real
    NoahmpIO%albedo          = undefined_real
    NoahmpIO%tvxy            = undefined_real
    NoahmpIO%tgxy            = undefined_real
    NoahmpIO%canicexy        = undefined_real
    NoahmpIO%canliqxy        = undefined_real
    NoahmpIO%eahxy           = undefined_real
    NoahmpIO%tahxy           = undefined_real
    NoahmpIO%cmxy            = undefined_real
    NoahmpIO%chxy            = undefined_real
    NoahmpIO%fwetxy          = undefined_real
    NoahmpIO%sneqvoxy        = undefined_real
    NoahmpIO%alboldxy        = undefined_real
    NoahmpIO%qsnowxy         = undefined_real
    NoahmpIO%qrainxy         = undefined_real
    NoahmpIO%wslakexy        = undefined_real
    NoahmpIO%zwtxy           = undefined_real
    NoahmpIO%waxy            = undefined_real
    NoahmpIO%wtxy            = undefined_real
    NoahmpIO%tsnoxy          = undefined_real
    NoahmpIO%snicexy         = undefined_real
    NoahmpIO%snliqxy         = undefined_real
    NoahmpIO%lfmassxy        = undefined_real
    NoahmpIO%rtmassxy        = undefined_real
    NoahmpIO%stmassxy        = undefined_real
    NoahmpIO%woodxy          = undefined_real
    NoahmpIO%stblcpxy        = undefined_real
    NoahmpIO%fastcpxy        = undefined_real
    NoahmpIO%lai             = undefined_real
    NoahmpIO%xsaixy          = undefined_real
    NoahmpIO%xlong           = undefined_real
    NoahmpIO%seaice          = undefined_real
    NoahmpIO%smcwtdxy        = undefined_real
    NoahmpIO%zsnsoxy         = undefined_real
    NoahmpIO%grdflx          = undefined_real
    NoahmpIO%hfx             = undefined_real
    NoahmpIO%lh              = undefined_real
    NoahmpIO%emiss           = undefined_real
    NoahmpIO%snowc           = undefined_real
    NoahmpIO%t2mvxy          = undefined_real
    NoahmpIO%t2mbxy          = undefined_real
    NoahmpIO%t2mxy           = undefined_real
    NoahmpIO%q2mvxy          = undefined_real
    NoahmpIO%q2mbxy          = undefined_real
    NoahmpIO%q2mxy           = undefined_real
    NoahmpIO%tradxy          = undefined_real
    NoahmpIO%neexy           = undefined_real
    NoahmpIO%gppxy           = undefined_real
    NoahmpIO%nppxy           = undefined_real
    NoahmpIO%fvegxy          = undefined_real
    NoahmpIO%runsfxy         = undefined_real
    NoahmpIO%runsbxy         = undefined_real
    NoahmpIO%ecanxy          = undefined_real
    NoahmpIO%edirxy          = undefined_real
    NoahmpIO%etranxy         = undefined_real
    NoahmpIO%fsaxy           = undefined_real
    NoahmpIO%firaxy          = undefined_real
    NoahmpIO%aparxy          = undefined_real
    NoahmpIO%psnxy           = undefined_real
    NoahmpIO%savxy           = undefined_real
    NoahmpIO%sagxy           = undefined_real
    NoahmpIO%rssunxy         = undefined_real
    NoahmpIO%rsshaxy         = undefined_real
    NoahmpIO%bgapxy          = undefined_real
    NoahmpIO%wgapxy          = undefined_real
    NoahmpIO%tgvxy           = undefined_real
    NoahmpIO%tgbxy           = undefined_real
    NoahmpIO%chvxy           = undefined_real
    NoahmpIO%chbxy           = undefined_real
    NoahmpIO%shgxy           = undefined_real
    NoahmpIO%shcxy           = undefined_real
    NoahmpIO%shbxy           = undefined_real
    NoahmpIO%evgxy           = undefined_real
    NoahmpIO%evbxy           = undefined_real
    NoahmpIO%ghvxy           = undefined_real
    NoahmpIO%ghbxy           = undefined_real
    NoahmpIO%irgxy           = undefined_real
    NoahmpIO%ircxy           = undefined_real
    NoahmpIO%irbxy           = undefined_real
    NoahmpIO%trxy            = undefined_real
    NoahmpIO%evcxy           = undefined_real
    NoahmpIO%chleafxy        = undefined_real
    NoahmpIO%chucxy          = undefined_real
    NoahmpIO%chv2xy          = undefined_real
    NoahmpIO%chb2xy          = undefined_real
    NoahmpIO%rs              = undefined_real
    NoahmpIO%canhsxy         = undefined_real
    NoahmpIO%z0              = undefined_real
    NoahmpIO%znt             = undefined_real
    NoahmpIO%taussxy         = 0.0
    NoahmpIO%deeprechxy      = 0.0
    NoahmpIO%rechxy          = 0.0
    NoahmpIO%acsnom          = 0.0
    NoahmpIO%acsnow          = 0.0
    NoahmpIO%mp_rainc        = 0.0
    NoahmpIO%mp_rainnc       = 0.0
    NoahmpIO%mp_shcv         = 0.0
    NoahmpIO%mp_snow         = 0.0
    NoahmpIO%mp_graup        = 0.0
    NoahmpIO%mp_hail         = 0.0
    NoahmpIO%sfcrunoff       = 0.0
    NoahmpIO%udrunoff        = 0.0

    ! additional output
    NoahmpIO%pahxy           = undefined_real
    NoahmpIO%pahgxy          = undefined_real
    NoahmpIO%pahbxy          = undefined_real
    NoahmpIO%pahvxy          = undefined_real
    NoahmpIO%qintsxy         = undefined_real
    NoahmpIO%qintrxy         = undefined_real
    NoahmpIO%qdripsxy        = undefined_real
    NoahmpIO%qdriprxy        = undefined_real
    NoahmpIO%qthrosxy        = undefined_real
    NoahmpIO%qthrorxy        = undefined_real
    NoahmpIO%qsnsubxy        = undefined_real
    NoahmpIO%qsnfroxy        = undefined_real
    NoahmpIO%qsubcxy         = undefined_real
    NoahmpIO%qfrocxy         = undefined_real
    NoahmpIO%qevacxy         = undefined_real
    NoahmpIO%qdewcxy         = undefined_real
    NoahmpIO%qfrzcxy         = undefined_real
    NoahmpIO%qmeltcxy        = undefined_real
    NoahmpIO%qsnbotxy        = undefined_real
    NoahmpIO%qmeltxy         = undefined_real
    NoahmpIO%fpicexy         = undefined_real
    NoahmpIO%rainlsm         = undefined_real
    NoahmpIO%snowlsm         = undefined_real
    NoahmpIO%forctlsm        = undefined_real
    NoahmpIO%forcqlsm        = undefined_real
    NoahmpIO%forcplsm        = undefined_real
    NoahmpIO%forczlsm        = undefined_real
    NoahmpIO%forcwlsm        = undefined_real
    NoahmpIO%eflxbxy         = undefined_real
    NoahmpIO%soilenergy      = undefined_real
    NoahmpIO%snowenergy      = undefined_real
    NoahmpIO%pondingxy       = 0.0
    NoahmpIO%acc_ssoilxy     = 0.0
    NoahmpIO%acc_qinsurxy    = 0.0
    NoahmpIO%acc_qsevaxy     = 0.0
    NoahmpIO%acc_etranixy    = 0.0
    NoahmpIO%acc_dwaterxy    = 0.0
    NoahmpIO%acc_prcpxy      = 0.0
    NoahmpIO%acc_ecanxy      = 0.0
    NoahmpIO%acc_etranxy     = 0.0
    NoahmpIO%acc_edirxy      = 0.0

    ! MMF Groundwater
    NoahmpIO%terrain         = undefined_real
    NoahmpIO%gvfmin          = undefined_real
    NoahmpIO%gvfmax          = undefined_real
    NoahmpIO%msftx           = undefined_real
    NoahmpIO%msfty           = undefined_real
    NoahmpIO%eqzwt           = undefined_real
    NoahmpIO%riverbedxy      = undefined_real
    NoahmpIO%rivercondxy     = undefined_real
    NoahmpIO%pexpxy          = undefined_real
    NoahmpIO%fdepthxy        = undefined_real
    NoahmpIO%areaxy          = undefined_real
    NoahmpIO%qrfsxy          = undefined_real
    NoahmpIO%qspringsxy      = undefined_real
    NoahmpIO%qrfxy           = undefined_real
    NoahmpIO%qspringxy       = undefined_real
    NoahmpIO%qslatxy         = undefined_real
    NoahmpIO%qlatxy          = undefined_real

    ! crop model
    NoahmpIO%pgsxy           = undefined_int
    NoahmpIO%cropcat         = undefined_int
    NoahmpIO%planting        = undefined_real
    NoahmpIO%harvest         = undefined_real
    NoahmpIO%season_gdd      = undefined_real
    NoahmpIO%croptype        = undefined_real

    ! tile drainage
    NoahmpIO%qtdrain         = 0.0
    NoahmpIO%td_fraction     = undefined_real

    ! irrigation
    NoahmpIO%irfract         = 0.0
    NoahmpIO%sifract         = 0.0
    NoahmpIO%mifract         = 0.0
    NoahmpIO%fifract         = 0.0
    NoahmpIO%irnumsi         = 0
    NoahmpIO%irnummi         = 0
    NoahmpIO%irnumfi         = 0
    NoahmpIO%irwatsi         = 0.0
    NoahmpIO%irwatmi         = 0.0
    NoahmpIO%irwatfi         = 0.0
    NoahmpIO%ireloss         = 0.0
    NoahmpIO%irsivol         = 0.0
    NoahmpIO%irmivol         = 0.0
    NoahmpIO%irfivol         = 0.0
    NoahmpIO%irrsplh         = 0.0
    NoahmpIO%loctim          = undefined_real

    ! spatial varying soil texture
    if ( NoahmpIO%iopt_soil > 1 ) then
       NoahmpIO%soilcl1      = undefined_real
       NoahmpIO%soilcl2      = undefined_real
       NoahmpIO%soilcl3      = undefined_real
       NoahmpIO%soilcl4      = undefined_real
       NoahmpIO%soilcomp     = undefined_real
    endif

    ! urban model 
    if ( NoahmpIO%sf_urban_physics > 0 ) then
       NoahmpIO%julday        = undefined_int_neg
       NoahmpIO%iri_urban     = undefined_int_neg
       NoahmpIO%utype_urb2d   = undefined_int_neg
       NoahmpIO%hrang         = undefined_real_neg
       NoahmpIO%declin        = undefined_real_neg
       NoahmpIO%sh_urb2d      = undefined_real_neg
       NoahmpIO%lh_urb2d      = undefined_real_neg
       NoahmpIO%g_urb2d       = undefined_real_neg
       NoahmpIO%rn_urb2d      = undefined_real_neg
       NoahmpIO%ts_urb2d      = undefined_real_neg
       NoahmpIO%gmt           = undefined_real_neg
       NoahmpIO%frc_urb2d     = undefined_real_neg
       NoahmpIO%lp_urb2d      = undefined_real_neg
       NoahmpIO%lb_urb2d      = undefined_real_neg
       NoahmpIO%hgt_urb2d     = undefined_real_neg
       NoahmpIO%ust           = undefined_real_neg
       NoahmpIO%cmr_sfcdif    = 1.0e-4
       NoahmpIO%chr_sfcdif    = 1.0e-4
       NoahmpIO%cmc_sfcdif    = 1.0e-4
       NoahmpIO%chc_sfcdif    = 1.0e-4
       NoahmpIO%cmgr_sfcdif   = 1.0e-4
       NoahmpIO%chgr_sfcdif   = 1.0e-4
       NoahmpIO%tr_urb2d      = undefined_real_neg
       NoahmpIO%tb_urb2d      = undefined_real_neg
       NoahmpIO%tg_urb2d      = undefined_real_neg
       NoahmpIO%tc_urb2d      = undefined_real_neg
       NoahmpIO%qc_urb2d      = undefined_real_neg
       NoahmpIO%uc_urb2d      = undefined_real_neg
       NoahmpIO%xxxr_urb2d    = undefined_real_neg
       NoahmpIO%xxxb_urb2d    = undefined_real_neg
       NoahmpIO%xxxg_urb2d    = undefined_real_neg
       NoahmpIO%xxxc_urb2d    = undefined_real_neg
       NoahmpIO%trl_urb3d     = undefined_real_neg
       NoahmpIO%tbl_urb3d     = undefined_real_neg
       NoahmpIO%tgl_urb3d     = undefined_real_neg
       NoahmpIO%psim_urb2d    = undefined_real_neg
       NoahmpIO%psih_urb2d    = undefined_real_neg
       NoahmpIO%u10_urb2d     = undefined_real_neg
       NoahmpIO%v10_urb2d     = undefined_real_neg
       NoahmpIO%gz1oz0_urb2d  = undefined_real_neg
       NoahmpIO%akms_urb2d    = undefined_real_neg
       NoahmpIO%th2_urb2d     = undefined_real_neg
       NoahmpIO%q2_urb2d      = undefined_real_neg
       NoahmpIO%ust_urb2d     = undefined_real_neg
       NoahmpIO%dzr           = undefined_real_neg
       NoahmpIO%dzb           = undefined_real_neg
       NoahmpIO%dzg           = undefined_real_neg
       NoahmpIO%cmcr_urb2d    = undefined_real_neg
       NoahmpIO%tgr_urb2d     = undefined_real_neg
       NoahmpIO%tgrl_urb3d    = undefined_real_neg
       NoahmpIO%smr_urb3d     = undefined_real_neg
       NoahmpIO%drelr_urb2d   = undefined_real_neg
       NoahmpIO%drelb_urb2d   = undefined_real_neg
       NoahmpIO%drelg_urb2d   = undefined_real_neg
       NoahmpIO%flxhumr_urb2d = undefined_real_neg
       NoahmpIO%flxhumb_urb2d = undefined_real_neg
       NoahmpIO%flxhumg_urb2d = undefined_real_neg
       NoahmpIO%chs           = 1.0e-4
       NoahmpIO%chs2          = 1.0e-4
       NoahmpIO%cqs2          = 1.0e-4
       NoahmpIO%mh_urb2d      = undefined_real_neg
       NoahmpIO%stdh_urb2d    = undefined_real_neg
       NoahmpIO%lf_urb2d      = undefined_real_neg
       NoahmpIO%trb_urb4d     = undefined_real_neg
       NoahmpIO%tw1_urb4d     = undefined_real_neg
       NoahmpIO%tw2_urb4d     = undefined_real_neg
       NoahmpIO%tgb_urb4d     = undefined_real_neg
       NoahmpIO%sfw1_urb3d    = undefined_real_neg
       NoahmpIO%sfw2_urb3d    = undefined_real_neg
       NoahmpIO%sfr_urb3d     = undefined_real_neg
       NoahmpIO%sfg_urb3d     = undefined_real_neg
       NoahmpIO%hi_urb2d      = undefined_real_neg
       NoahmpIO%theta_urban   = undefined_real_neg
       NoahmpIO%u_urban       = undefined_real_neg
       NoahmpIO%v_urban       = undefined_real_neg
       NoahmpIO%dz_urban      = undefined_real_neg
       NoahmpIO%rho_urban     = undefined_real_neg
       NoahmpIO%p_urban       = undefined_real_neg
       NoahmpIO%a_u_bep       = undefined_real_neg
       NoahmpIO%a_v_bep       = undefined_real_neg
       NoahmpIO%a_t_bep       = undefined_real_neg
       NoahmpIO%a_q_bep       = undefined_real_neg
       NoahmpIO%a_e_bep       = undefined_real_neg
       NoahmpIO%b_u_bep       = undefined_real_neg
       NoahmpIO%b_v_bep       = undefined_real_neg
       NoahmpIO%b_t_bep       = undefined_real_neg
       NoahmpIO%b_q_bep       = undefined_real_neg
       NoahmpIO%b_e_bep       = undefined_real_neg
       NoahmpIO%dlg_bep       = undefined_real_neg
       NoahmpIO%dl_u_bep      = undefined_real_neg
       NoahmpIO%sf_bep        = undefined_real_neg
       NoahmpIO%vl_bep        = undefined_real_neg
       NoahmpIO%tlev_urb3d    = undefined_real_neg
       NoahmpIO%qlev_urb3d    = undefined_real_neg
       NoahmpIO%tw1lev_urb3d  = undefined_real_neg
       NoahmpIO%tw2lev_urb3d  = undefined_real_neg
       NoahmpIO%tglev_urb3d   = undefined_real_neg
       NoahmpIO%tflev_urb3d   = undefined_real_neg
       NoahmpIO%sf_ac_urb3d   = undefined_real_neg
       NoahmpIO%lf_ac_urb3d   = undefined_real_neg
       NoahmpIO%cm_ac_urb3d   = undefined_real_neg
       NoahmpIO%sfvent_urb3d  = undefined_real_neg
       NoahmpIO%lfvent_urb3d  = undefined_real_neg
       NoahmpIO%sfwin1_urb3d  = undefined_real_neg
       NoahmpIO%sfwin2_urb3d  = undefined_real_neg
       NoahmpIO%ep_pv_urb3d   = undefined_real_neg
       NoahmpIO%t_pv_urb3d    = undefined_real_neg
       NoahmpIO%trv_urb4d     = undefined_real_neg
       NoahmpIO%qr_urb4d      = undefined_real_neg
       NoahmpIO%qgr_urb3d     = undefined_real_neg
       NoahmpIO%tgr_urb3d     = undefined_real_neg
       NoahmpIO%drain_urb4d   = undefined_real_neg
       NoahmpIO%draingr_urb3d = undefined_real_neg
       NoahmpIO%sfrv_urb3d    = undefined_real_neg
       NoahmpIO%lfrv_urb3d    = undefined_real_neg
       NoahmpIO%dgr_urb3d     = undefined_real_neg
       NoahmpIO%dg_urb3d      = undefined_real_neg
       NoahmpIO%lfr_urb3d     = undefined_real_neg
       NoahmpIO%lfg_urb3d     = undefined_real_neg
    endif

    NoahmpIO%slopetyp          = 1        ! soil parameter slope type
    NoahmpIO%soil_update_steps = 1        ! number of model time step to update soil proces
    NoahmpIO%calculate_soil    = .false.  ! index for if do soil process

#ifdef WRF_HYDRO
    NoahmpIO%infxsrt         = 0.0
    NoahmpIO%sfcheadrt       = 0.0
    NoahmpIO%soldrain        = 0.0
    NoahmpIO%qtiledrain      = 0.0
    NoahmpIO%zwatble2d       = 0.0
#endif 
   
    end associate
 
  end subroutine NoahmpIOVarInitDefault

end module NoahmpIOVarInitMod
