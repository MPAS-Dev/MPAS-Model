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
    associate(                               &
              XSTART  =>  NoahmpIO%XSTART   ,&
              XEND    =>  NoahmpIO%XEND     ,&
              YSTART  =>  NoahmpIO%YSTART   ,&
              YEND    =>  NoahmpIO%YEND     ,&
              KDS     =>  NoahmpIO%KDS      ,&
              KDE     =>  NoahmpIO%KDE      ,&
              NSOIL   =>  NoahmpIO%NSOIL    ,&
              NSNOW   =>  NoahmpIO%NSNOW     &
             )
! -------------------------------------------------

    ! Input variables
    if ( .not. allocated (NoahmpIO%COSZEN)    ) allocate ( NoahmpIO%COSZEN     (XSTART:XEND,        YSTART:YEND) ) ! cosine zenith angle
    if ( .not. allocated (NoahmpIO%XLAT)      ) allocate ( NoahmpIO%XLAT       (XSTART:XEND,        YSTART:YEND) ) ! latitude [radians] 
    if ( .not. allocated (NoahmpIO%DZS)       ) allocate ( NoahmpIO%DZS        (1:NSOIL)                         ) ! thickness of soil layers [m]
    if ( .not. allocated (NoahmpIO%ZSOIL)     ) allocate ( NoahmpIO%ZSOIL      (1:NSOIL)                         ) ! depth to soil interfaces [m] 
    if ( .not. allocated (NoahmpIO%IVGTYP)    ) allocate ( NoahmpIO%IVGTYP     (XSTART:XEND,        YSTART:YEND) ) ! vegetation type
    if ( .not. allocated (NoahmpIO%ISLTYP)    ) allocate ( NoahmpIO%ISLTYP     (XSTART:XEND,        YSTART:YEND) ) ! soil type
    if ( .not. allocated (NoahmpIO%VEGFRA)    ) allocate ( NoahmpIO%VEGFRA     (XSTART:XEND,        YSTART:YEND) ) ! vegetation fraction []
    if ( .not. allocated (NoahmpIO%TMN)       ) allocate ( NoahmpIO%TMN        (XSTART:XEND,        YSTART:YEND) ) ! deep soil temperature [K]
    if ( .not. allocated (NoahmpIO%XLAND)     ) allocate ( NoahmpIO%XLAND      (XSTART:XEND,        YSTART:YEND) ) ! =2 ocean; =1 land/seaice
    if ( .not. allocated (NoahmpIO%XICE)      ) allocate ( NoahmpIO%XICE       (XSTART:XEND,        YSTART:YEND) ) ! fraction of grid that is seaice
    if ( .not. allocated (NoahmpIO%SWDOWN)    ) allocate ( NoahmpIO%SWDOWN     (XSTART:XEND,        YSTART:YEND) ) ! solar down at surface [W m-2]
    if ( .not. allocated (NoahmpIO%SWDDIR)    ) allocate ( NoahmpIO%SWDDIR     (XSTART:XEND,        YSTART:YEND) ) ! solar down at surface [W m-2] for new urban solar panel
    if ( .not. allocated (NoahmpIO%SWDDIF)    ) allocate ( NoahmpIO%SWDDIF     (XSTART:XEND,        YSTART:YEND) ) ! solar down at surface [W m-2] for new urban solar panel
    if ( .not. allocated (NoahmpIO%GLW)       ) allocate ( NoahmpIO%GLW        (XSTART:XEND,        YSTART:YEND) ) ! longwave down at surface [W m-2]
    if ( .not. allocated (NoahmpIO%RAINBL)    ) allocate ( NoahmpIO%RAINBL     (XSTART:XEND,        YSTART:YEND) ) ! total precipitation entering land model [mm] per time step
    if ( .not. allocated (NoahmpIO%SNOWBL)    ) allocate ( NoahmpIO%SNOWBL     (XSTART:XEND,        YSTART:YEND) ) ! snow entering land model [mm] per time step
    if ( .not. allocated (NoahmpIO%SR)        ) allocate ( NoahmpIO%SR         (XSTART:XEND,        YSTART:YEND) ) ! frozen precip ratio entering land model [-]
    if ( .not. allocated (NoahmpIO%RAINCV)    ) allocate ( NoahmpIO%RAINCV     (XSTART:XEND,        YSTART:YEND) ) ! convective precip forcing [mm]
    if ( .not. allocated (NoahmpIO%RAINNCV)   ) allocate ( NoahmpIO%RAINNCV    (XSTART:XEND,        YSTART:YEND) ) ! non-convective precip forcing [mm]
    if ( .not. allocated (NoahmpIO%RAINSHV)   ) allocate ( NoahmpIO%RAINSHV    (XSTART:XEND,        YSTART:YEND) ) ! shallow conv. precip forcing [mm]
    if ( .not. allocated (NoahmpIO%SNOWNCV)   ) allocate ( NoahmpIO%SNOWNCV    (XSTART:XEND,        YSTART:YEND) ) ! non-covective snow forcing (subset of rainncv) [mm]
    if ( .not. allocated (NoahmpIO%GRAUPELNCV)) allocate ( NoahmpIO%GRAUPELNCV (XSTART:XEND,        YSTART:YEND) ) ! non-convective graupel forcing (subset of rainncv) [mm]
    if ( .not. allocated (NoahmpIO%HAILNCV)   ) allocate ( NoahmpIO%HAILNCV    (XSTART:XEND,        YSTART:YEND) ) ! non-convective hail forcing (subset of rainncv) [mm]
    if ( .not. allocated (NoahmpIO%MP_RAINC)  ) allocate ( NoahmpIO%MP_RAINC   (XSTART:XEND,        YSTART:YEND) ) ! convective precip forcing [mm]
    if ( .not. allocated (NoahmpIO%MP_RAINNC) ) allocate ( NoahmpIO%MP_RAINNC  (XSTART:XEND,        YSTART:YEND) ) ! non-convective precip forcing [mm]
    if ( .not. allocated (NoahmpIO%MP_SHCV)   ) allocate ( NoahmpIO%MP_SHCV    (XSTART:XEND,        YSTART:YEND) ) ! shallow conv. precip forcing [mm]
    if ( .not. allocated (NoahmpIO%MP_SNOW)   ) allocate ( NoahmpIO%MP_SNOW    (XSTART:XEND,        YSTART:YEND) ) ! non-covective snow (subset of rainnc) [mm]
    if ( .not. allocated (NoahmpIO%MP_GRAUP)  ) allocate ( NoahmpIO%MP_GRAUP   (XSTART:XEND,        YSTART:YEND) ) ! non-convective graupel (subset of rainnc) [mm]
    if ( .not. allocated (NoahmpIO%MP_HAIL)   ) allocate ( NoahmpIO%MP_HAIL    (XSTART:XEND,        YSTART:YEND) ) ! non-convective hail (subset of rainnc) [mm]
    if ( .not. allocated (NoahmpIO%SEAICE)    ) allocate ( NoahmpIO%SEAICE     (XSTART:XEND,        YSTART:YEND) ) ! seaice fraction
    if ( .not. allocated (NoahmpIO%DZ8W)      ) allocate ( NoahmpIO%DZ8W       (XSTART:XEND,KDS:KDE,YSTART:YEND) ) ! thickness of atmo layers [m]
    if ( .not. allocated (NoahmpIO%T_PHY)     ) allocate ( NoahmpIO%T_PHY      (XSTART:XEND,KDS:KDE,YSTART:YEND) ) ! 3D atmospheric temperature valid at mid-levels [K]
    if ( .not. allocated (NoahmpIO%QV_CURR)   ) allocate ( NoahmpIO%QV_CURR    (XSTART:XEND,KDS:KDE,YSTART:YEND) ) ! 3D water vapor mixing ratio [kg/kg_dry]
    if ( .not. allocated (NoahmpIO%U_PHY)     ) allocate ( NoahmpIO%U_PHY      (XSTART:XEND,KDS:KDE,YSTART:YEND) ) ! 3D U wind component [m/s]
    if ( .not. allocated (NoahmpIO%V_PHY)     ) allocate ( NoahmpIO%V_PHY      (XSTART:XEND,KDS:KDE,YSTART:YEND) ) ! 3D V wind component [m/s]
    if ( .not. allocated (NoahmpIO%P8W)       ) allocate ( NoahmpIO%P8W        (XSTART:XEND,KDS:KDE,YSTART:YEND) ) ! 3D pressure, valid at interface [Pa]
 
    ! spatial varying parameter map
    if ( NoahmpIO%IOPT_SOIL > 1 ) then
       if ( .not. allocated (NoahmpIO%soilcomp)) allocate ( NoahmpIO%soilcomp (XSTART:XEND,1:2*NSOIL,YSTART:YEND) ) ! Soil sand and clay content [fraction]
       if ( .not. allocated (NoahmpIO%soilcl1) ) allocate ( NoahmpIO%soilcl1  (XSTART:XEND,          YSTART:YEND) ) ! Soil texture class with depth
       if ( .not. allocated (NoahmpIO%soilcl2) ) allocate ( NoahmpIO%soilcl2  (XSTART:XEND,          YSTART:YEND) ) ! Soil texture class with depth
       if ( .not. allocated (NoahmpIO%soilcl3) ) allocate ( NoahmpIO%soilcl3  (XSTART:XEND,          YSTART:YEND) ) ! Soil texture class with depth
       if ( .not. allocated (NoahmpIO%soilcl4) ) allocate ( NoahmpIO%soilcl4  (XSTART:XEND,          YSTART:YEND) ) ! Soil texture class with depth
    endif
    if ( NoahmpIO%IOPT_SOIL == 4 ) then
       if ( .not. allocated (NoahmpIO%bexp_3d)      ) allocate ( NoahmpIO%bexp_3d       (XSTART:XEND,1:NSOIL,YSTART:YEND) ) ! C-H B exponent
       if ( .not. allocated (NoahmpIO%smcdry_3D)    ) allocate ( NoahmpIO%smcdry_3D     (XSTART:XEND,1:NSOIL,YSTART:YEND) ) ! Soil Moisture Limit: Dry
       if ( .not. allocated (NoahmpIO%smcwlt_3D)    ) allocate ( NoahmpIO%smcwlt_3D     (XSTART:XEND,1:NSOIL,YSTART:YEND) ) ! Soil Moisture Limit: Wilt
       if ( .not. allocated (NoahmpIO%smcref_3D)    ) allocate ( NoahmpIO%smcref_3D     (XSTART:XEND,1:NSOIL,YSTART:YEND) ) ! Soil Moisture Limit: Reference
       if ( .not. allocated (NoahmpIO%smcmax_3D)    ) allocate ( NoahmpIO%smcmax_3D     (XSTART:XEND,1:NSOIL,YSTART:YEND) ) ! Soil Moisture Limit: Max
       if ( .not. allocated (NoahmpIO%dksat_3D)     ) allocate ( NoahmpIO%dksat_3D      (XSTART:XEND,1:NSOIL,YSTART:YEND) ) ! Saturated Soil Conductivity
       if ( .not. allocated (NoahmpIO%dwsat_3D)     ) allocate ( NoahmpIO%dwsat_3D      (XSTART:XEND,1:NSOIL,YSTART:YEND) ) ! Saturated Soil Diffusivity
       if ( .not. allocated (NoahmpIO%psisat_3D)    ) allocate ( NoahmpIO%psisat_3D     (XSTART:XEND,1:NSOIL,YSTART:YEND) ) ! Saturated Matric Potential
       if ( .not. allocated (NoahmpIO%quartz_3D)    ) allocate ( NoahmpIO%quartz_3D     (XSTART:XEND,1:NSOIL,YSTART:YEND) ) ! Soil quartz content
       if ( .not. allocated (NoahmpIO%refdk_2D)     ) allocate ( NoahmpIO%refdk_2D      (XSTART:XEND,        YSTART:YEND) ) ! Reference Soil Conductivity
       if ( .not. allocated (NoahmpIO%refkdt_2D)    ) allocate ( NoahmpIO%refkdt_2D     (XSTART:XEND,        YSTART:YEND) ) ! Soil Infiltration Parameter
       if ( .not. allocated (NoahmpIO%irr_frac_2D)  ) allocate ( NoahmpIO%irr_frac_2D   (XSTART:XEND,        YSTART:YEND) ) ! irrigation Fraction
       if ( .not. allocated (NoahmpIO%irr_har_2D)   ) allocate ( NoahmpIO%irr_har_2D    (XSTART:XEND,        YSTART:YEND) ) ! number of days before harvest date to stop irrigation 
       if ( .not. allocated (NoahmpIO%irr_lai_2D)   ) allocate ( NoahmpIO%irr_lai_2D    (XSTART:XEND,        YSTART:YEND) ) ! Minimum lai to trigger irrigation
       if ( .not. allocated (NoahmpIO%irr_mad_2D)   ) allocate ( NoahmpIO%irr_mad_2D    (XSTART:XEND,        YSTART:YEND) ) ! management allowable deficit (0-1)
       if ( .not. allocated (NoahmpIO%filoss_2D)    ) allocate ( NoahmpIO%filoss_2D     (XSTART:XEND,        YSTART:YEND) ) ! fraction of flood irrigation loss (0-1) 
       if ( .not. allocated (NoahmpIO%sprir_rate_2D)) allocate ( NoahmpIO%sprir_rate_2D (XSTART:XEND,        YSTART:YEND) ) ! mm/h, sprinkler irrigation rate
       if ( .not. allocated (NoahmpIO%micir_rate_2D)) allocate ( NoahmpIO%micir_rate_2D (XSTART:XEND,        YSTART:YEND) ) ! mm/h, micro irrigation rate
       if ( .not. allocated (NoahmpIO%firtfac_2D)   ) allocate ( NoahmpIO%firtfac_2D    (XSTART:XEND,        YSTART:YEND) ) ! flood application rate factor
       if ( .not. allocated (NoahmpIO%ir_rain_2D)   ) allocate ( NoahmpIO%ir_rain_2D    (XSTART:XEND,        YSTART:YEND) ) ! maximum precipitation to stop irrigation trigger
       if ( .not. allocated (NoahmpIO%bvic_2D)      ) allocate ( NoahmpIO%bvic_2D       (XSTART:XEND,        YSTART:YEND) ) ! VIC model infiltration parameter [-]
       if ( .not. allocated (NoahmpIO%axaj_2D)      ) allocate ( NoahmpIO%axaj_2D       (XSTART:XEND,        YSTART:YEND) ) ! Tension water distribution inflection parameter [-]
       if ( .not. allocated (NoahmpIO%bxaj_2D)      ) allocate ( NoahmpIO%bxaj_2D       (XSTART:XEND,        YSTART:YEND) ) ! Tension water distribution shape parameter [-]
       if ( .not. allocated (NoahmpIO%xxaj_2D)      ) allocate ( NoahmpIO%xxaj_2D       (XSTART:XEND,        YSTART:YEND) ) ! Free water distribution shape parameter [-]
       if ( .not. allocated (NoahmpIO%bdvic_2D)     ) allocate ( NoahmpIO%bdvic_2D      (XSTART:XEND,        YSTART:YEND) ) ! DVIC model infiltration parameter [-]
       if ( .not. allocated (NoahmpIO%gdvic_2D)     ) allocate ( NoahmpIO%gdvic_2D      (XSTART:XEND,        YSTART:YEND) ) ! Mean Capillary Drive (m) for infiltration models
       if ( .not. allocated (NoahmpIO%bbvic_2D)     ) allocate ( NoahmpIO%bbvic_2D      (XSTART:XEND,        YSTART:YEND) ) ! DVIC heterogeniety parameter for infiltration [-]
       if ( .not. allocated (NoahmpIO%KLAT_FAC)     ) allocate ( NoahmpIO%KLAT_FAC      (XSTART:XEND,        YSTART:YEND) ) ! factor multiplier to hydraulic conductivity
       if ( .not. allocated (NoahmpIO%TDSMC_FAC)    ) allocate ( NoahmpIO%TDSMC_FAC     (XSTART:XEND,        YSTART:YEND) ) ! factor multiplier to field capacity
       if ( .not. allocated (NoahmpIO%TD_DC)        ) allocate ( NoahmpIO%TD_DC         (XSTART:XEND,        YSTART:YEND) ) ! drainage coefficient for simple
       if ( .not. allocated (NoahmpIO%TD_DCOEF)     ) allocate ( NoahmpIO%TD_DCOEF      (XSTART:XEND,        YSTART:YEND) ) ! drainge coefficient for Hooghoudt 
       if ( .not. allocated (NoahmpIO%TD_DDRAIN)    ) allocate ( NoahmpIO%TD_DDRAIN     (XSTART:XEND,        YSTART:YEND) ) ! depth of drain
       if ( .not. allocated (NoahmpIO%TD_RADI)      ) allocate ( NoahmpIO%TD_RADI       (XSTART:XEND,        YSTART:YEND) ) ! tile radius
       if ( .not. allocated (NoahmpIO%TD_SPAC)      ) allocate ( NoahmpIO%TD_SPAC       (XSTART:XEND,        YSTART:YEND) ) ! tile spacing
    endif

    ! INOUT (with generic LSM equivalent) (as defined in WRF)
    if ( .not. allocated (NoahmpIO%TSK)      ) allocate ( NoahmpIO%TSK       (XSTART:XEND,        YSTART:YEND) ) ! surface radiative temperature [K]
    if ( .not. allocated (NoahmpIO%HFX)      ) allocate ( NoahmpIO%HFX       (XSTART:XEND,        YSTART:YEND) ) ! sensible heat flux [W m-2]
    if ( .not. allocated (NoahmpIO%QFX)      ) allocate ( NoahmpIO%QFX       (XSTART:XEND,        YSTART:YEND) ) ! latent heat flux [kg s-1 m-2]
    if ( .not. allocated (NoahmpIO%LH)       ) allocate ( NoahmpIO%LH        (XSTART:XEND,        YSTART:YEND) ) ! latent heat flux [W m-2]
    if ( .not. allocated (NoahmpIO%GRDFLX)   ) allocate ( NoahmpIO%GRDFLX    (XSTART:XEND,        YSTART:YEND) ) ! ground/snow heat flux [W m-2]
    if ( .not. allocated (NoahmpIO%SMSTAV)   ) allocate ( NoahmpIO%SMSTAV    (XSTART:XEND,        YSTART:YEND) ) ! soil moisture avail. [not used]
    if ( .not. allocated (NoahmpIO%SMSTOT)   ) allocate ( NoahmpIO%SMSTOT    (XSTART:XEND,        YSTART:YEND) ) ! total soil water [mm][not used]
    if ( .not. allocated (NoahmpIO%SFCRUNOFF)) allocate ( NoahmpIO%SFCRUNOFF (XSTART:XEND,        YSTART:YEND) ) ! accumulated surface runoff [m]
    if ( .not. allocated (NoahmpIO%UDRUNOFF) ) allocate ( NoahmpIO%UDRUNOFF  (XSTART:XEND,        YSTART:YEND) ) ! accumulated sub-surface runoff [m]
    if ( .not. allocated (NoahmpIO%ALBEDO)   ) allocate ( NoahmpIO%ALBEDO    (XSTART:XEND,        YSTART:YEND) ) ! total grid albedo []
    if ( .not. allocated (NoahmpIO%SNOWC)    ) allocate ( NoahmpIO%SNOWC     (XSTART:XEND,        YSTART:YEND) ) ! snow cover fraction []
    if ( .not. allocated (NoahmpIO%SNOW)     ) allocate ( NoahmpIO%SNOW      (XSTART:XEND,        YSTART:YEND) ) ! snow water equivalent [mm]
    if ( .not. allocated (NoahmpIO%SNOWH)    ) allocate ( NoahmpIO%SNOWH     (XSTART:XEND,        YSTART:YEND) ) ! physical snow depth [m]
    if ( .not. allocated (NoahmpIO%CANWAT)   ) allocate ( NoahmpIO%CANWAT    (XSTART:XEND,        YSTART:YEND) ) ! total canopy water + ice [mm]
    if ( .not. allocated (NoahmpIO%ACSNOM)   ) allocate ( NoahmpIO%ACSNOM    (XSTART:XEND,        YSTART:YEND) ) ! accumulated snow melt leaving pack
    if ( .not. allocated (NoahmpIO%ACSNOW)   ) allocate ( NoahmpIO%ACSNOW    (XSTART:XEND,        YSTART:YEND) ) ! accumulated snow on grid
    if ( .not. allocated (NoahmpIO%EMISS)    ) allocate ( NoahmpIO%EMISS     (XSTART:XEND,        YSTART:YEND) ) ! surface bulk emissivity
    if ( .not. allocated (NoahmpIO%QSFC)     ) allocate ( NoahmpIO%QSFC      (XSTART:XEND,        YSTART:YEND) ) ! bulk surface specific humidity
    if ( .not. allocated (NoahmpIO%SMOISEQ)  ) allocate ( NoahmpIO%SMOISEQ   (XSTART:XEND,1:NSOIL,YSTART:YEND) ) ! equilibrium volumetric soil moisture [m3/m3]
    if ( .not. allocated (NoahmpIO%SMOIS)    ) allocate ( NoahmpIO%SMOIS     (XSTART:XEND,1:NSOIL,YSTART:YEND) ) ! volumetric soil moisture [m3/m3]
    if ( .not. allocated (NoahmpIO%SH2O)     ) allocate ( NoahmpIO%SH2O      (XSTART:XEND,1:NSOIL,YSTART:YEND) ) ! volumetric liquid soil moisture [m3/m3]
    if ( .not. allocated (NoahmpIO%TSLB)     ) allocate ( NoahmpIO%TSLB      (XSTART:XEND,1:NSOIL,YSTART:YEND) ) ! soil temperature [K]

    ! INOUT (with no Noah LSM equivalent) (as defined in WRF)
    if ( .not. allocated (NoahmpIO%ISNOWXY)   ) allocate ( NoahmpIO%ISNOWXY    (XSTART:XEND,               YSTART:YEND) ) ! actual no. of snow layers
    if ( .not. allocated (NoahmpIO%TVXY)      ) allocate ( NoahmpIO%TVXY       (XSTART:XEND,               YSTART:YEND) ) ! vegetation leaf temperature
    if ( .not. allocated (NoahmpIO%TGXY)      ) allocate ( NoahmpIO%TGXY       (XSTART:XEND,               YSTART:YEND) ) ! bulk ground surface temperature
    if ( .not. allocated (NoahmpIO%CANICEXY)  ) allocate ( NoahmpIO%CANICEXY   (XSTART:XEND,               YSTART:YEND) ) ! canopy-intercepted ice (mm)
    if ( .not. allocated (NoahmpIO%CANLIQXY)  ) allocate ( NoahmpIO%CANLIQXY   (XSTART:XEND,               YSTART:YEND) ) ! canopy-intercepted liquid water (mm)
    if ( .not. allocated (NoahmpIO%EAHXY)     ) allocate ( NoahmpIO%EAHXY      (XSTART:XEND,               YSTART:YEND) ) ! canopy air vapor pressure (pa)
    if ( .not. allocated (NoahmpIO%TAHXY)     ) allocate ( NoahmpIO%TAHXY      (XSTART:XEND,               YSTART:YEND) ) ! canopy air temperature (k)
    if ( .not. allocated (NoahmpIO%CMXY)      ) allocate ( NoahmpIO%CMXY       (XSTART:XEND,               YSTART:YEND) ) ! bulk momentum drag coefficient
    if ( .not. allocated (NoahmpIO%CHXY)      ) allocate ( NoahmpIO%CHXY       (XSTART:XEND,               YSTART:YEND) ) ! bulk sensible heat exchange coefficient
    if ( .not. allocated (NoahmpIO%FWETXY)    ) allocate ( NoahmpIO%FWETXY     (XSTART:XEND,               YSTART:YEND) ) ! wetted or snowed fraction of the canopy (-)
    if ( .not. allocated (NoahmpIO%SNEQVOXY)  ) allocate ( NoahmpIO%SNEQVOXY   (XSTART:XEND,               YSTART:YEND) ) ! snow mass at last time step(mm h2o)
    if ( .not. allocated (NoahmpIO%ALBOLDXY)  ) allocate ( NoahmpIO%ALBOLDXY   (XSTART:XEND,               YSTART:YEND) ) ! snow albedo at last time step (-)
    if ( .not. allocated (NoahmpIO%QSNOWXY)   ) allocate ( NoahmpIO%QSNOWXY    (XSTART:XEND,               YSTART:YEND) ) ! snowfall on the ground [mm/s]
    if ( .not. allocated (NoahmpIO%QRAINXY)   ) allocate ( NoahmpIO%QRAINXY    (XSTART:XEND,               YSTART:YEND) ) ! rainfall on the ground [mm/s]
    if ( .not. allocated (NoahmpIO%WSLAKEXY)  ) allocate ( NoahmpIO%WSLAKEXY   (XSTART:XEND,               YSTART:YEND) ) ! lake water storage [mm]
    if ( .not. allocated (NoahmpIO%ZWTXY)     ) allocate ( NoahmpIO%ZWTXY      (XSTART:XEND,               YSTART:YEND) ) ! water table depth [m]
    if ( .not. allocated (NoahmpIO%WAXY)      ) allocate ( NoahmpIO%WAXY       (XSTART:XEND,               YSTART:YEND) ) ! water in the "aquifer" [mm]
    if ( .not. allocated (NoahmpIO%WTXY)      ) allocate ( NoahmpIO%WTXY       (XSTART:XEND,               YSTART:YEND) ) ! groundwater storage [mm]
    if ( .not. allocated (NoahmpIO%SMCWTDXY)  ) allocate ( NoahmpIO%SMCWTDXY   (XSTART:XEND,               YSTART:YEND) ) ! soil moisture below the bottom of the column (m3m-3)
    if ( .not. allocated (NoahmpIO%DEEPRECHXY)) allocate ( NoahmpIO%DEEPRECHXY (XSTART:XEND,               YSTART:YEND) ) ! recharge to the water table when deep (m)
    if ( .not. allocated (NoahmpIO%RECHXY)    ) allocate ( NoahmpIO%RECHXY     (XSTART:XEND,               YSTART:YEND) ) ! recharge to the water table (diagnostic) (m)
    if ( .not. allocated (NoahmpIO%LFMASSXY)  ) allocate ( NoahmpIO%LFMASSXY   (XSTART:XEND,               YSTART:YEND) ) ! leaf mass [g/m2]
    if ( .not. allocated (NoahmpIO%RTMASSXY)  ) allocate ( NoahmpIO%RTMASSXY   (XSTART:XEND,               YSTART:YEND) ) ! mass of fine roots [g/m2]
    if ( .not. allocated (NoahmpIO%STMASSXY)  ) allocate ( NoahmpIO%STMASSXY   (XSTART:XEND,               YSTART:YEND) ) ! stem mass [g/m2]
    if ( .not. allocated (NoahmpIO%WOODXY)    ) allocate ( NoahmpIO%WOODXY     (XSTART:XEND,               YSTART:YEND) ) ! mass of wood (incl. woody roots) [g/m2]
    if ( .not. allocated (NoahmpIO%GRAINXY)   ) allocate ( NoahmpIO%GRAINXY    (XSTART:XEND,               YSTART:YEND) ) ! mass of grain XING [g/m2]
    if ( .not. allocated (NoahmpIO%GDDXY)     ) allocate ( NoahmpIO%GDDXY      (XSTART:XEND,               YSTART:YEND) ) ! growing degree days XING FOUR
    if ( .not. allocated (NoahmpIO%STBLCPXY)  ) allocate ( NoahmpIO%STBLCPXY   (XSTART:XEND,               YSTART:YEND) ) ! stable carbon in deep soil [g/m2]
    if ( .not. allocated (NoahmpIO%FASTCPXY)  ) allocate ( NoahmpIO%FASTCPXY   (XSTART:XEND,               YSTART:YEND) ) ! short-lived carbon, shallow soil [g/m2]
    if ( .not. allocated (NoahmpIO%LAI)       ) allocate ( NoahmpIO%LAI        (XSTART:XEND,               YSTART:YEND) ) ! leaf area index
    if ( .not. allocated (NoahmpIO%XSAIXY)    ) allocate ( NoahmpIO%XSAIXY     (XSTART:XEND,               YSTART:YEND) ) ! stem area index
    if ( .not. allocated (NoahmpIO%TAUSSXY)   ) allocate ( NoahmpIO%TAUSSXY    (XSTART:XEND,               YSTART:YEND) ) ! snow age factor
    if ( .not. allocated (NoahmpIO%TSNOXY)    ) allocate ( NoahmpIO%TSNOXY     (XSTART:XEND,-NSNOW+1:0,    YSTART:YEND) ) ! snow temperature [K]
    if ( .not. allocated (NoahmpIO%ZSNSOXY)   ) allocate ( NoahmpIO%ZSNSOXY    (XSTART:XEND,-NSNOW+1:NSOIL,YSTART:YEND) ) ! snow layer depth [m]
    if ( .not. allocated (NoahmpIO%SNICEXY)   ) allocate ( NoahmpIO%SNICEXY    (XSTART:XEND,-NSNOW+1:0,    YSTART:YEND) ) ! snow layer ice [mm]
    if ( .not. allocated (NoahmpIO%SNLIQXY)   ) allocate ( NoahmpIO%SNLIQXY    (XSTART:XEND,-NSNOW+1:0,    YSTART:YEND) ) ! snow layer liquid water [mm]

    ! irrigation
    if ( .not. allocated (NoahmpIO%IRFRACT) ) allocate ( NoahmpIO%IRFRACT (XSTART:XEND,YSTART:YEND) ) ! irrigation fraction
    if ( .not. allocated (NoahmpIO%SIFRACT) ) allocate ( NoahmpIO%SIFRACT (XSTART:XEND,YSTART:YEND) ) ! sprinkler irrigation fraction
    if ( .not. allocated (NoahmpIO%MIFRACT) ) allocate ( NoahmpIO%MIFRACT (XSTART:XEND,YSTART:YEND) ) ! micro irrigation fraction
    if ( .not. allocated (NoahmpIO%FIFRACT) ) allocate ( NoahmpIO%FIFRACT (XSTART:XEND,YSTART:YEND) ) ! flood irrigation fraction   
    if ( .not. allocated (NoahmpIO%IRNUMSI) ) allocate ( NoahmpIO%IRNUMSI (XSTART:XEND,YSTART:YEND) ) ! irrigation event number, Sprinkler
    if ( .not. allocated (NoahmpIO%IRNUMMI) ) allocate ( NoahmpIO%IRNUMMI (XSTART:XEND,YSTART:YEND) ) ! irrigation event number, Micro
    if ( .not. allocated (NoahmpIO%IRNUMFI) ) allocate ( NoahmpIO%IRNUMFI (XSTART:XEND,YSTART:YEND) ) ! irrigation event number, Flood 
    if ( .not. allocated (NoahmpIO%IRWATSI) ) allocate ( NoahmpIO%IRWATSI (XSTART:XEND,YSTART:YEND) ) ! irrigation water amount [m] to be applied, Sprinkler
    if ( .not. allocated (NoahmpIO%IRWATMI) ) allocate ( NoahmpIO%IRWATMI (XSTART:XEND,YSTART:YEND) ) ! irrigation water amount [m] to be applied, Micro
    if ( .not. allocated (NoahmpIO%IRWATFI) ) allocate ( NoahmpIO%IRWATFI (XSTART:XEND,YSTART:YEND) ) ! irrigation water amount [m] to be applied, Flood
    if ( .not. allocated (NoahmpIO%IRELOSS) ) allocate ( NoahmpIO%IRELOSS (XSTART:XEND,YSTART:YEND) ) ! loss of irrigation water to evaporation,sprinkler [mm]
    if ( .not. allocated (NoahmpIO%IRSIVOL) ) allocate ( NoahmpIO%IRSIVOL (XSTART:XEND,YSTART:YEND) ) ! amount of irrigation by sprinkler (mm)
    if ( .not. allocated (NoahmpIO%IRMIVOL) ) allocate ( NoahmpIO%IRMIVOL (XSTART:XEND,YSTART:YEND) ) ! amount of irrigation by micro (mm)
    if ( .not. allocated (NoahmpIO%IRFIVOL) ) allocate ( NoahmpIO%IRFIVOL (XSTART:XEND,YSTART:YEND) ) ! amount of irrigation by micro (mm)
    if ( .not. allocated (NoahmpIO%IRRSPLH) ) allocate ( NoahmpIO%IRRSPLH (XSTART:XEND,YSTART:YEND) ) ! latent heating from sprinkler evaporation (w/m2)
    if ( .not. allocated (NoahmpIO%LOCTIM)  ) allocate ( NoahmpIO%LOCTIM  (XSTART:XEND,YSTART:YEND) ) ! local time
  
    ! OUT (with no Noah LSM equivalent) (as defined in WRF)   
    if ( .not. allocated (NoahmpIO%T2MVXY)     ) allocate ( NoahmpIO%T2MVXY      (XSTART:XEND,YSTART:YEND) ) ! 2m temperature of vegetation part
    if ( .not. allocated (NoahmpIO%T2MBXY)     ) allocate ( NoahmpIO%T2MBXY      (XSTART:XEND,YSTART:YEND) ) ! 2m temperature of bare ground part
    if ( .not. allocated (NoahmpIO%Q2MVXY)     ) allocate ( NoahmpIO%Q2MVXY      (XSTART:XEND,YSTART:YEND) ) ! 2m mixing ratio of vegetation part
    if ( .not. allocated (NoahmpIO%Q2MBXY)     ) allocate ( NoahmpIO%Q2MBXY      (XSTART:XEND,YSTART:YEND) ) ! 2m mixing ratio of bare ground part
    if ( .not. allocated (NoahmpIO%TRADXY)     ) allocate ( NoahmpIO%TRADXY      (XSTART:XEND,YSTART:YEND) ) ! surface radiative temperature (k)
    if ( .not. allocated (NoahmpIO%NEEXY)      ) allocate ( NoahmpIO%NEEXY       (XSTART:XEND,YSTART:YEND) ) ! net ecosys exchange (g/m2/s CO2)
    if ( .not. allocated (NoahmpIO%GPPXY)      ) allocate ( NoahmpIO%GPPXY       (XSTART:XEND,YSTART:YEND) ) ! gross primary assimilation [g/m2/s C]
    if ( .not. allocated (NoahmpIO%NPPXY)      ) allocate ( NoahmpIO%NPPXY       (XSTART:XEND,YSTART:YEND) ) ! net primary productivity [g/m2/s C]
    if ( .not. allocated (NoahmpIO%FVEGXY)     ) allocate ( NoahmpIO%FVEGXY      (XSTART:XEND,YSTART:YEND) ) ! Noah-MP vegetation fraction [-]
    if ( .not. allocated (NoahmpIO%RUNSFXY)    ) allocate ( NoahmpIO%RUNSFXY     (XSTART:XEND,YSTART:YEND) ) ! surface runoff [mm per soil timestep]
    if ( .not. allocated (NoahmpIO%RUNSBXY)    ) allocate ( NoahmpIO%RUNSBXY     (XSTART:XEND,YSTART:YEND) ) ! subsurface runoff [mm per soil timestep]
    if ( .not. allocated (NoahmpIO%ECANXY)     ) allocate ( NoahmpIO%ECANXY      (XSTART:XEND,YSTART:YEND) ) ! evaporation of intercepted water (mm/s)
    if ( .not. allocated (NoahmpIO%EDIRXY)     ) allocate ( NoahmpIO%EDIRXY      (XSTART:XEND,YSTART:YEND) ) ! soil surface evaporation rate (mm/s]
    if ( .not. allocated (NoahmpIO%ETRANXY)    ) allocate ( NoahmpIO%ETRANXY     (XSTART:XEND,YSTART:YEND) ) ! transpiration rate (mm/s)
    if ( .not. allocated (NoahmpIO%FSAXY)      ) allocate ( NoahmpIO%FSAXY       (XSTART:XEND,YSTART:YEND) ) ! total absorbed solar radiation (w/m2)
    if ( .not. allocated (NoahmpIO%FIRAXY)     ) allocate ( NoahmpIO%FIRAXY      (XSTART:XEND,YSTART:YEND) ) ! total net longwave rad (w/m2) [+ to atm]
    if ( .not. allocated (NoahmpIO%APARXY)     ) allocate ( NoahmpIO%APARXY      (XSTART:XEND,YSTART:YEND) ) ! photosyn active energy by canopy (w/m2)
    if ( .not. allocated (NoahmpIO%PSNXY)      ) allocate ( NoahmpIO%PSNXY       (XSTART:XEND,YSTART:YEND) ) ! total photosynthesis (umol co2/m2/s) [+]
    if ( .not. allocated (NoahmpIO%SAVXY)      ) allocate ( NoahmpIO%SAVXY       (XSTART:XEND,YSTART:YEND) ) ! solar rad absorbed by veg. (w/m2)
    if ( .not. allocated (NoahmpIO%SAGXY)      ) allocate ( NoahmpIO%SAGXY       (XSTART:XEND,YSTART:YEND) ) ! solar rad absorbed by ground (w/m2)
    if ( .not. allocated (NoahmpIO%RSSUNXY)    ) allocate ( NoahmpIO%RSSUNXY     (XSTART:XEND,YSTART:YEND) ) ! sunlit leaf stomatal resistance (s/m)
    if ( .not. allocated (NoahmpIO%RSSHAXY)    ) allocate ( NoahmpIO%RSSHAXY     (XSTART:XEND,YSTART:YEND) ) ! shaded leaf stomatal resistance (s/m)
    if ( .not. allocated (NoahmpIO%BGAPXY)     ) allocate ( NoahmpIO%BGAPXY      (XSTART:XEND,YSTART:YEND) ) ! between gap fraction
    if ( .not. allocated (NoahmpIO%WGAPXY)     ) allocate ( NoahmpIO%WGAPXY      (XSTART:XEND,YSTART:YEND) ) ! within gap fraction
    if ( .not. allocated (NoahmpIO%TGVXY)      ) allocate ( NoahmpIO%TGVXY       (XSTART:XEND,YSTART:YEND) ) ! under canopy ground temperature[K]
    if ( .not. allocated (NoahmpIO%TGBXY)      ) allocate ( NoahmpIO%TGBXY       (XSTART:XEND,YSTART:YEND) ) ! bare ground temperature [K]
    if ( .not. allocated (NoahmpIO%CHVXY)      ) allocate ( NoahmpIO%CHVXY       (XSTART:XEND,YSTART:YEND) ) ! sensible heat exchange coefficient vegetated
    if ( .not. allocated (NoahmpIO%CHBXY)      ) allocate ( NoahmpIO%CHBXY       (XSTART:XEND,YSTART:YEND) ) ! sensible heat exchange coefficient bare-ground
    if ( .not. allocated (NoahmpIO%SHGXY)      ) allocate ( NoahmpIO%SHGXY       (XSTART:XEND,YSTART:YEND) ) ! veg ground sen. heat [w/m2]   [+ to atm]
    if ( .not. allocated (NoahmpIO%SHCXY)      ) allocate ( NoahmpIO%SHCXY       (XSTART:XEND,YSTART:YEND) ) ! canopy sen. heat [w/m2]   [+ to atm]
    if ( .not. allocated (NoahmpIO%SHBXY)      ) allocate ( NoahmpIO%SHBXY       (XSTART:XEND,YSTART:YEND) ) ! bare sensible heat [w/m2]  [+ to atm]
    if ( .not. allocated (NoahmpIO%EVGXY)      ) allocate ( NoahmpIO%EVGXY       (XSTART:XEND,YSTART:YEND) ) ! veg ground evap. heat [w/m2]  [+ to atm]
    if ( .not. allocated (NoahmpIO%EVBXY)      ) allocate ( NoahmpIO%EVBXY       (XSTART:XEND,YSTART:YEND) ) ! bare soil evaporation [w/m2]  [+ to atm]
    if ( .not. allocated (NoahmpIO%GHVXY)      ) allocate ( NoahmpIO%GHVXY       (XSTART:XEND,YSTART:YEND) ) ! veg ground heat flux [w/m2]  [+ to soil]
    if ( .not. allocated (NoahmpIO%GHBXY)      ) allocate ( NoahmpIO%GHBXY       (XSTART:XEND,YSTART:YEND) ) ! bare ground heat flux [w/m2] [+ to soil]
    if ( .not. allocated (NoahmpIO%IRGXY)      ) allocate ( NoahmpIO%IRGXY       (XSTART:XEND,YSTART:YEND) ) ! veg ground net LW rad. [w/m2] [+ to atm]
    if ( .not. allocated (NoahmpIO%IRCXY)      ) allocate ( NoahmpIO%IRCXY       (XSTART:XEND,YSTART:YEND) ) ! canopy net LW rad. [w/m2] [+ to atm]
    if ( .not. allocated (NoahmpIO%IRBXY)      ) allocate ( NoahmpIO%IRBXY       (XSTART:XEND,YSTART:YEND) ) ! bare net longwave rad. [w/m2] [+ to atm]
    if ( .not. allocated (NoahmpIO%TRXY)       ) allocate ( NoahmpIO%TRXY        (XSTART:XEND,YSTART:YEND) ) ! transpiration [w/m2]  [+ to atm]
    if ( .not. allocated (NoahmpIO%EVCXY)      ) allocate ( NoahmpIO%EVCXY       (XSTART:XEND,YSTART:YEND) ) ! canopy evaporation heat [w/m2]  [+ to atm]
    if ( .not. allocated (NoahmpIO%CHLEAFXY)   ) allocate ( NoahmpIO%CHLEAFXY    (XSTART:XEND,YSTART:YEND) ) ! leaf exchange coefficient 
    if ( .not. allocated (NoahmpIO%CHUCXY)     ) allocate ( NoahmpIO%CHUCXY      (XSTART:XEND,YSTART:YEND) ) ! under canopy exchange coefficient 
    if ( .not. allocated (NoahmpIO%CHV2XY)     ) allocate ( NoahmpIO%CHV2XY      (XSTART:XEND,YSTART:YEND) ) ! veg 2m exchange coefficient 
    if ( .not. allocated (NoahmpIO%CHB2XY)     ) allocate ( NoahmpIO%CHB2XY      (XSTART:XEND,YSTART:YEND) ) ! bare 2m exchange coefficient 
    if ( .not. allocated (NoahmpIO%RS)         ) allocate ( NoahmpIO%RS          (XSTART:XEND,YSTART:YEND) ) ! Total stomatal resistance (s/m)
    if ( .not. allocated (NoahmpIO%Z0)         ) allocate ( NoahmpIO%Z0          (XSTART:XEND,YSTART:YEND) ) ! roughness length output to WRF 
    if ( .not. allocated (NoahmpIO%ZNT)        ) allocate ( NoahmpIO%ZNT         (XSTART:XEND,YSTART:YEND) ) ! roughness length output to WRF 
    if ( .not. allocated (NoahmpIO%QTDRAIN)    ) allocate ( NoahmpIO%QTDRAIN     (XSTART:XEND,YSTART:YEND) ) ! tile drainage (mm)
    if ( .not. allocated (NoahmpIO%TD_FRACTION)) allocate ( NoahmpIO%TD_FRACTION (XSTART:XEND,YSTART:YEND) ) ! tile drainage fraction
    if ( .not. allocated (NoahmpIO%XLONG)      ) allocate ( NoahmpIO%XLONG       (XSTART:XEND,YSTART:YEND) ) ! longitude
    if ( .not. allocated (NoahmpIO%TERRAIN)    ) allocate ( NoahmpIO%TERRAIN     (XSTART:XEND,YSTART:YEND) ) ! terrain height
    if ( .not. allocated (NoahmpIO%GVFMIN)     ) allocate ( NoahmpIO%GVFMIN      (XSTART:XEND,YSTART:YEND) ) ! annual minimum in vegetation fraction
    if ( .not. allocated (NoahmpIO%GVFMAX)     ) allocate ( NoahmpIO%GVFMAX      (XSTART:XEND,YSTART:YEND) ) ! annual maximum in vegetation fraction

    ! additional output variables
    if ( .not. allocated (NoahmpIO%PAHXY)       ) allocate ( NoahmpIO%PAHXY        (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%PAHGXY)      ) allocate ( NoahmpIO%PAHGXY       (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%PAHBXY)      ) allocate ( NoahmpIO%PAHBXY       (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%PAHVXY)      ) allocate ( NoahmpIO%PAHVXY       (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%QINTSXY)     ) allocate ( NoahmpIO%QINTSXY      (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%QINTRXY)     ) allocate ( NoahmpIO%QINTRXY      (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%QDRIPSXY)    ) allocate ( NoahmpIO%QDRIPSXY     (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%QDRIPRXY)    ) allocate ( NoahmpIO%QDRIPRXY     (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%QTHROSXY)    ) allocate ( NoahmpIO%QTHROSXY     (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%QTHRORXY)    ) allocate ( NoahmpIO%QTHRORXY     (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%QSNSUBXY)    ) allocate ( NoahmpIO%QSNSUBXY     (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%QSNFROXY)    ) allocate ( NoahmpIO%QSNFROXY     (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%QSUBCXY)     ) allocate ( NoahmpIO%QSUBCXY      (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%QFROCXY)     ) allocate ( NoahmpIO%QFROCXY      (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%QEVACXY)     ) allocate ( NoahmpIO%QEVACXY      (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%QDEWCXY)     ) allocate ( NoahmpIO%QDEWCXY      (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%QFRZCXY)     ) allocate ( NoahmpIO%QFRZCXY      (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%QMELTCXY)    ) allocate ( NoahmpIO%QMELTCXY     (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%QSNBOTXY)    ) allocate ( NoahmpIO%QSNBOTXY     (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%QMELTXY)     ) allocate ( NoahmpIO%QMELTXY      (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%PONDINGXY)   ) allocate ( NoahmpIO%PONDINGXY    (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%FPICEXY)     ) allocate ( NoahmpIO%FPICEXY      (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%RAINLSM)     ) allocate ( NoahmpIO%RAINLSM      (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%SNOWLSM)     ) allocate ( NoahmpIO%SNOWLSM      (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%FORCTLSM)    ) allocate ( NoahmpIO%FORCTLSM     (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%FORCQLSM)    ) allocate ( NoahmpIO%FORCQLSM     (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%FORCPLSM)    ) allocate ( NoahmpIO%FORCPLSM     (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%FORCZLSM)    ) allocate ( NoahmpIO%FORCZLSM     (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%FORCWLSM)    ) allocate ( NoahmpIO%FORCWLSM     (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%EFLXBXY)     ) allocate ( NoahmpIO%EFLXBXY      (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%SOILENERGY)  ) allocate ( NoahmpIO%SOILENERGY   (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%SNOWENERGY)  ) allocate ( NoahmpIO%SNOWENERGY   (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%CANHSXY)     ) allocate ( NoahmpIO%CANHSXY      (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%ACC_DWATERXY)) allocate ( NoahmpIO%ACC_DWATERXY (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%ACC_PRCPXY)  ) allocate ( NoahmpIO%ACC_PRCPXY   (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%ACC_ECANXY)  ) allocate ( NoahmpIO%ACC_ECANXY   (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%ACC_ETRANXY) ) allocate ( NoahmpIO%ACC_ETRANXY  (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%ACC_EDIRXY)  ) allocate ( NoahmpIO%ACC_EDIRXY   (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%ACC_SSOILXY) ) allocate ( NoahmpIO%ACC_SSOILXY  (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%ACC_QINSURXY)) allocate ( NoahmpIO%ACC_QINSURXY (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%ACC_QSEVAXY) ) allocate ( NoahmpIO%ACC_QSEVAXY  (XSTART:XEND,        YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%ACC_ETRANIXY)) allocate ( NoahmpIO%ACC_ETRANIXY (XSTART:XEND,1:NSOIL,YSTART:YEND) )

    ! Needed for MMF_RUNOFF (IOPT_RUN = 5); not part of MP driver in WRF
    if ( .not. allocated (NoahmpIO%MSFTX)      ) allocate ( NoahmpIO%MSFTX       (XSTART:XEND,YSTART:YEND) ) 
    if ( .not. allocated (NoahmpIO%MSFTY)      ) allocate ( NoahmpIO%MSFTY       (XSTART:XEND,YSTART:YEND) ) 
    if ( .not. allocated (NoahmpIO%EQZWT)      ) allocate ( NoahmpIO%EQZWT       (XSTART:XEND,YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%RIVERBEDXY) ) allocate ( NoahmpIO%RIVERBEDXY  (XSTART:XEND,YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%RIVERCONDXY)) allocate ( NoahmpIO%RIVERCONDXY (XSTART:XEND,YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%PEXPXY)     ) allocate ( NoahmpIO%PEXPXY      (XSTART:XEND,YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%FDEPTHXY)   ) allocate ( NoahmpIO%FDEPTHXY    (XSTART:XEND,YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%AREAXY)     ) allocate ( NoahmpIO%AREAXY      (XSTART:XEND,YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%QRFSXY)     ) allocate ( NoahmpIO%QRFSXY      (XSTART:XEND,YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%QSPRINGSXY) ) allocate ( NoahmpIO%QSPRINGSXY  (XSTART:XEND,YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%QRFXY)      ) allocate ( NoahmpIO%QRFXY       (XSTART:XEND,YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%QSPRINGXY)  ) allocate ( NoahmpIO%QSPRINGXY   (XSTART:XEND,YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%QSLATXY)    ) allocate ( NoahmpIO%QSLATXY     (XSTART:XEND,YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%QLATXY)     ) allocate ( NoahmpIO%QLATXY      (XSTART:XEND,YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%RECHCLIM)   ) allocate ( NoahmpIO%RECHCLIM    (XSTART:XEND,YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%RIVERMASK)  ) allocate ( NoahmpIO%RIVERMASK   (XSTART:XEND,YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%NONRIVERXY) ) allocate ( NoahmpIO%NONRIVERXY  (XSTART:XEND,YSTART:YEND) )

    ! Needed for crop model (OPT_CROP=1)
    if ( .not. allocated (NoahmpIO%PGSXY)     ) allocate ( NoahmpIO%PGSXY      (XSTART:XEND,  YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%CROPCAT)   ) allocate ( NoahmpIO%CROPCAT    (XSTART:XEND,  YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%PLANTING)  ) allocate ( NoahmpIO%PLANTING   (XSTART:XEND,  YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%HARVEST)   ) allocate ( NoahmpIO%HARVEST    (XSTART:XEND,  YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%SEASON_GDD)) allocate ( NoahmpIO%SEASON_GDD (XSTART:XEND,  YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%CROPTYPE)  ) allocate ( NoahmpIO%CROPTYPE   (XSTART:XEND,5,YSTART:YEND) )

    ! Single- and Multi-layer Urban Models
    if ( NoahmpIO%SF_URBAN_PHYSICS > 0 )  then
       if ( .not. allocated (NoahmpIO%sh_urb2d)   ) allocate ( NoahmpIO%sh_urb2d    (XSTART:XEND,YSTART:YEND) ) 
       if ( .not. allocated (NoahmpIO%lh_urb2d)   ) allocate ( NoahmpIO%lh_urb2d    (XSTART:XEND,YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%g_urb2d)    ) allocate ( NoahmpIO%g_urb2d     (XSTART:XEND,YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%rn_urb2d)   ) allocate ( NoahmpIO%rn_urb2d    (XSTART:XEND,YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%ts_urb2d)   ) allocate ( NoahmpIO%ts_urb2d    (XSTART:XEND,YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%HRANG)      ) allocate ( NoahmpIO%HRANG       (XSTART:XEND,YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%frc_urb2d)  ) allocate ( NoahmpIO%frc_urb2d   (XSTART:XEND,YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%utype_urb2d)) allocate ( NoahmpIO%utype_urb2d (XSTART:XEND,YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%lp_urb2d)   ) allocate ( NoahmpIO%lp_urb2d    (XSTART:XEND,YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%lb_urb2d)   ) allocate ( NoahmpIO%lb_urb2d    (XSTART:XEND,YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%hgt_urb2d)  ) allocate ( NoahmpIO%hgt_urb2d   (XSTART:XEND,YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%ust)        ) allocate ( NoahmpIO%ust         (XSTART:XEND,YSTART:YEND) )
       !ENDIF
         
       !IF(NoahmpIO%SF_URBAN_PHYSICS == 1 ) THEN  ! single layer urban model  
       if ( .not. allocated (NoahmpIO%cmr_sfcdif)   ) allocate ( NoahmpIO%cmr_sfcdif    (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%chr_sfcdif)   ) allocate ( NoahmpIO%chr_sfcdif    (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%cmc_sfcdif)   ) allocate ( NoahmpIO%cmc_sfcdif    (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%chc_sfcdif)   ) allocate ( NoahmpIO%chc_sfcdif    (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%cmgr_sfcdif)  ) allocate ( NoahmpIO%cmgr_sfcdif   (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%chgr_sfcdif)  ) allocate ( NoahmpIO%chgr_sfcdif   (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%tr_urb2d)     ) allocate ( NoahmpIO%tr_urb2d      (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%tb_urb2d)     ) allocate ( NoahmpIO%tb_urb2d      (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%tg_urb2d)     ) allocate ( NoahmpIO%tg_urb2d      (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%tc_urb2d)     ) allocate ( NoahmpIO%tc_urb2d      (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%qc_urb2d)     ) allocate ( NoahmpIO%qc_urb2d      (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%uc_urb2d)     ) allocate ( NoahmpIO%uc_urb2d      (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%xxxr_urb2d)   ) allocate ( NoahmpIO%xxxr_urb2d    (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%xxxb_urb2d)   ) allocate ( NoahmpIO%xxxb_urb2d    (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%xxxg_urb2d)   ) allocate ( NoahmpIO%xxxg_urb2d    (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%xxxc_urb2d)   ) allocate ( NoahmpIO%xxxc_urb2d    (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%psim_urb2d)   ) allocate ( NoahmpIO%psim_urb2d    (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%psih_urb2d)   ) allocate ( NoahmpIO%psih_urb2d    (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%u10_urb2d)    ) allocate ( NoahmpIO%u10_urb2d     (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%v10_urb2d)    ) allocate ( NoahmpIO%v10_urb2d     (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%GZ1OZ0_urb2d) ) allocate ( NoahmpIO%GZ1OZ0_urb2d  (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%AKMS_URB2D)   ) allocate ( NoahmpIO%AKMS_URB2D    (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%th2_urb2d)    ) allocate ( NoahmpIO%th2_urb2d     (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%q2_urb2d)     ) allocate ( NoahmpIO%q2_urb2d      (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%ust_urb2d)    ) allocate ( NoahmpIO%ust_urb2d     (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%cmcr_urb2d)   ) allocate ( NoahmpIO%cmcr_urb2d    (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%tgr_urb2d)    ) allocate ( NoahmpIO%tgr_urb2d     (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%drelr_urb2d)  ) allocate ( NoahmpIO%drelr_urb2d   (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%drelb_urb2d)  ) allocate ( NoahmpIO%drelb_urb2d   (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%drelg_urb2d)  ) allocate ( NoahmpIO%drelg_urb2d   (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%flxhumr_urb2d)) allocate ( NoahmpIO%flxhumr_urb2d (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%flxhumb_urb2d)) allocate ( NoahmpIO%flxhumb_urb2d (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%flxhumg_urb2d)) allocate ( NoahmpIO%flxhumg_urb2d (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%chs)          ) allocate ( NoahmpIO%chs           (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%chs2)         ) allocate ( NoahmpIO%chs2          (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%cqs2)         ) allocate ( NoahmpIO%cqs2          (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%mh_urb2d)     ) allocate ( NoahmpIO%mh_urb2d      (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%stdh_urb2d)   ) allocate ( NoahmpIO%stdh_urb2d    (XSTART:XEND,        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%lf_urb2d)     ) allocate ( NoahmpIO%lf_urb2d      (XSTART:XEND,4,      YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%trl_urb3d)    ) allocate ( NoahmpIO%trl_urb3d     (XSTART:XEND,1:NSOIL,YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%tbl_urb3d)    ) allocate ( NoahmpIO%tbl_urb3d     (XSTART:XEND,1:NSOIL,YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%tgl_urb3d)    ) allocate ( NoahmpIO%tgl_urb3d     (XSTART:XEND,1:NSOIL,YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%tgrl_urb3d)   ) allocate ( NoahmpIO%tgrl_urb3d    (XSTART:XEND,1:NSOIL,YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%smr_urb3d)    ) allocate ( NoahmpIO%smr_urb3d     (XSTART:XEND,1:NSOIL,YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%dzr)          ) allocate ( NoahmpIO%dzr           (            1:NSOIL            ) )
       if ( .not. allocated (NoahmpIO%dzb)          ) allocate ( NoahmpIO%dzb           (            1:NSOIL            ) )
       if ( .not. allocated (NoahmpIO%dzg)          ) allocate ( NoahmpIO%dzg           (            1:NSOIL            ) )
       !ENDIF

       !IF(SF_URBAN_PHYSICS == 2 .or. SF_URBAN_PHYSICS == 3) THEN  ! BEP or BEM urban models
       if ( .not. allocated (NoahmpIO%trb_urb4d)  ) allocate ( NoahmpIO%trb_urb4d   (XSTART:XEND,NoahmpIO%urban_map_zrd,YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%tw1_urb4d)  ) allocate ( NoahmpIO%tw1_urb4d   (XSTART:XEND,NoahmpIO%urban_map_zwd,YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%tw2_urb4d)  ) allocate ( NoahmpIO%tw2_urb4d   (XSTART:XEND,NoahmpIO%urban_map_zwd,YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%tgb_urb4d)  ) allocate ( NoahmpIO%tgb_urb4d   (XSTART:XEND,NoahmpIO%urban_map_gd, YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%sfw1_urb3d) ) allocate ( NoahmpIO%sfw1_urb3d  (XSTART:XEND,NoahmpIO%urban_map_zd, YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%sfw2_urb3d) ) allocate ( NoahmpIO%sfw2_urb3d  (XSTART:XEND,NoahmpIO%urban_map_zd, YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%sfr_urb3d)  ) allocate ( NoahmpIO%sfr_urb3d   (XSTART:XEND,NoahmpIO%urban_map_zdf,YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%sfg_urb3d)  ) allocate ( NoahmpIO%sfg_urb3d   (XSTART:XEND,NoahmpIO%num_urban_ndm,YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%hi_urb2d)   ) allocate ( NoahmpIO%hi_urb2d    (XSTART:XEND,NoahmpIO%num_urban_hi, YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%theta_urban)) allocate ( NoahmpIO%theta_urban (XSTART:XEND,KDS:KDE,               YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%u_urban)    ) allocate ( NoahmpIO%u_urban     (XSTART:XEND,KDS:KDE,               YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%v_urban)    ) allocate ( NoahmpIO%v_urban     (XSTART:XEND,KDS:KDE,               YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%dz_urban)   ) allocate ( NoahmpIO%dz_urban    (XSTART:XEND,KDS:KDE,               YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%rho_urban)  ) allocate ( NoahmpIO%rho_urban   (XSTART:XEND,KDS:KDE,               YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%p_urban)    ) allocate ( NoahmpIO%p_urban     (XSTART:XEND,KDS:KDE,               YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%a_u_bep)    ) allocate ( NoahmpIO%a_u_bep     (XSTART:XEND,KDS:KDE,               YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%a_v_bep)    ) allocate ( NoahmpIO%a_v_bep     (XSTART:XEND,KDS:KDE,               YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%a_t_bep)    ) allocate ( NoahmpIO%a_t_bep     (XSTART:XEND,KDS:KDE,               YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%a_q_bep)    ) allocate ( NoahmpIO%a_q_bep     (XSTART:XEND,KDS:KDE,               YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%a_e_bep)    ) allocate ( NoahmpIO%a_e_bep     (XSTART:XEND,KDS:KDE,               YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%b_u_bep)    ) allocate ( NoahmpIO%b_u_bep     (XSTART:XEND,KDS:KDE,               YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%b_v_bep)    ) allocate ( NoahmpIO%b_v_bep     (XSTART:XEND,KDS:KDE,               YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%b_t_bep)    ) allocate ( NoahmpIO%b_t_bep     (XSTART:XEND,KDS:KDE,               YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%b_q_bep)    ) allocate ( NoahmpIO%b_q_bep     (XSTART:XEND,KDS:KDE,               YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%b_e_bep)    ) allocate ( NoahmpIO%b_e_bep     (XSTART:XEND,KDS:KDE,               YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%dlg_bep)    ) allocate ( NoahmpIO%dlg_bep     (XSTART:XEND,KDS:KDE,               YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%dl_u_bep)   ) allocate ( NoahmpIO%dl_u_bep    (XSTART:XEND,KDS:KDE,               YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%sf_bep)     ) allocate ( NoahmpIO%sf_bep      (XSTART:XEND,KDS:KDE,               YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%vl_bep)     ) allocate ( NoahmpIO%vl_bep      (XSTART:XEND,KDS:KDE,               YSTART:YEND) )
       !ENDIF

        !IF(SF_URBAN_PHYSICS == 3) THEN  ! BEM urban model
       if ( .not. allocated (NoahmpIO%tlev_urb3d)   ) allocate ( NoahmpIO%tlev_urb3d    (XSTART:XEND,NoahmpIO%urban_map_bd,  YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%qlev_urb3d)   ) allocate ( NoahmpIO%qlev_urb3d    (XSTART:XEND,NoahmpIO%urban_map_bd,  YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%tw1lev_urb3d) ) allocate ( NoahmpIO%tw1lev_urb3d  (XSTART:XEND,NoahmpIO%urban_map_wd,  YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%tw2lev_urb3d) ) allocate ( NoahmpIO%tw2lev_urb3d  (XSTART:XEND,NoahmpIO%urban_map_wd,  YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%tglev_urb3d)  ) allocate ( NoahmpIO%tglev_urb3d   (XSTART:XEND,NoahmpIO%urban_map_gbd, YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%tflev_urb3d)  ) allocate ( NoahmpIO%tflev_urb3d   (XSTART:XEND,NoahmpIO%urban_map_fbd, YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%sf_ac_urb3d)  ) allocate ( NoahmpIO%sf_ac_urb3d   (XSTART:XEND,                        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%lf_ac_urb3d)  ) allocate ( NoahmpIO%lf_ac_urb3d   (XSTART:XEND,                        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%cm_ac_urb3d)  ) allocate ( NoahmpIO%cm_ac_urb3d   (XSTART:XEND,                        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%sfvent_urb3d) ) allocate ( NoahmpIO%sfvent_urb3d  (XSTART:XEND,                        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%lfvent_urb3d) ) allocate ( NoahmpIO%lfvent_urb3d  (XSTART:XEND,                        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%sfwin1_urb3d) ) allocate ( NoahmpIO%sfwin1_urb3d  (XSTART:XEND,NoahmpIO%urban_map_wd,  YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%sfwin2_urb3d) ) allocate ( NoahmpIO%sfwin2_urb3d  (XSTART:XEND,NoahmpIO%urban_map_wd,  YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%ep_pv_urb3d)  ) allocate ( NoahmpIO%ep_pv_urb3d   (XSTART:XEND,                        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%t_pv_urb3d)   ) allocate ( NoahmpIO%t_pv_urb3d    (XSTART:XEND,NoahmpIO%urban_map_zdf, YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%trv_urb4d)    ) allocate ( NoahmpIO%trv_urb4d     (XSTART:XEND,NoahmpIO%urban_map_zgrd,YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%qr_urb4d)     ) allocate ( NoahmpIO%qr_urb4d      (XSTART:XEND,NoahmpIO%urban_map_zgrd,YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%qgr_urb3d)    ) allocate ( NoahmpIO%qgr_urb3d     (XSTART:XEND,                        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%tgr_urb3d)    ) allocate ( NoahmpIO%tgr_urb3d     (XSTART:XEND,                        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%drain_urb4d)  ) allocate ( NoahmpIO%drain_urb4d   (XSTART:XEND,NoahmpIO%urban_map_zdf, YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%draingr_urb3d)) allocate ( NoahmpIO%draingr_urb3d (XSTART:XEND,                        YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%sfrv_urb3d)   ) allocate ( NoahmpIO%sfrv_urb3d    (XSTART:XEND,NoahmpIO%urban_map_zdf, YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%lfrv_urb3d)   ) allocate ( NoahmpIO%lfrv_urb3d    (XSTART:XEND,NoahmpIO%urban_map_zdf, YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%dgr_urb3d)    ) allocate ( NoahmpIO%dgr_urb3d     (XSTART:XEND,NoahmpIO%urban_map_zdf, YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%dg_urb3d)     ) allocate ( NoahmpIO%dg_urb3d      (XSTART:XEND,NoahmpIO%num_urban_ndm, YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%lfr_urb3d)    ) allocate ( NoahmpIO%lfr_urb3d     (XSTART:XEND,NoahmpIO%urban_map_zdf, YSTART:YEND) )
       if ( .not. allocated (NoahmpIO%lfg_urb3d)    ) allocate ( NoahmpIO%lfg_urb3d     (XSTART:XEND,NoahmpIO%num_urban_ndm, YSTART:YEND) )

    endif

#ifdef WRF_HYDRO
    if ( .not. allocated (NoahmpIO%infxsrt)   ) allocate ( NoahmpIO%infxsrt    (XSTART:XEND,YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%sfcheadrt) ) allocate ( NoahmpIO%sfcheadrt  (XSTART:XEND,YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%soldrain)  ) allocate ( NoahmpIO%soldrain   (XSTART:XEND,YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%qtiledrain)) allocate ( NoahmpIO%qtiledrain (XSTART:XEND,YSTART:YEND) )
    if ( .not. allocated (NoahmpIO%ZWATBLE2D) ) allocate ( NoahmpIO%ZWATBLE2D  (XSTART:XEND,YSTART:YEND) )
#endif    

    !-------------------------------------------------------------------
    ! Initialize variables with default values 
    !-------------------------------------------------------------------
    
    NoahmpIO%ICE             = undefined_int
    NoahmpIO%IVGTYP          = undefined_int
    NoahmpIO%ISLTYP          = undefined_int
    NoahmpIO%ISNOWXY         = undefined_int
    NoahmpIO%COSZEN          = undefined_real
    NoahmpIO%XLAT            = undefined_real
    NoahmpIO%DZ8W            = undefined_real
    NoahmpIO%DZS             = undefined_real
    NoahmpIO%ZSOIL           = undefined_real
    NoahmpIO%VEGFRA          = undefined_real
    NoahmpIO%TMN             = undefined_real
    NoahmpIO%XLAND           = undefined_real
    NoahmpIO%XICE            = undefined_real
    NoahmpIO%T_PHY           = undefined_real
    NoahmpIO%QV_CURR         = undefined_real
    NoahmpIO%U_PHY           = undefined_real
    NoahmpIO%V_PHY           = undefined_real
    NoahmpIO%SWDOWN          = undefined_real
    NoahmpIO%SWDDIR          = undefined_real
    NoahmpIO%SWDDIF          = undefined_real
    NoahmpIO%GLW             = undefined_real
    NoahmpIO%P8W             = undefined_real
    NoahmpIO%RAINBL          = undefined_real
    NoahmpIO%SNOWBL          = undefined_real
    NoahmpIO%SR              = undefined_real
    NoahmpIO%RAINCV          = undefined_real
    NoahmpIO%RAINNCV         = undefined_real
    NoahmpIO%RAINSHV         = undefined_real
    NoahmpIO%SNOWNCV         = undefined_real
    NoahmpIO%GRAUPELNCV      = undefined_real
    NoahmpIO%HAILNCV         = undefined_real
    NoahmpIO%QSFC            = undefined_real
    NoahmpIO%TSK             = undefined_real
    NoahmpIO%QFX             = undefined_real
    NoahmpIO%SMSTAV          = undefined_real
    NoahmpIO%SMSTOT          = undefined_real
    NoahmpIO%SMOIS           = undefined_real
    NoahmpIO%SH2O            = undefined_real
    NoahmpIO%TSLB            = undefined_real
    NoahmpIO%SNOW            = undefined_real
    NoahmpIO%SNOWH           = undefined_real
    NoahmpIO%CANWAT          = undefined_real
    NoahmpIO%SMOISEQ         = undefined_real
    NoahmpIO%ALBEDO          = undefined_real
    NoahmpIO%TVXY            = undefined_real
    NoahmpIO%TGXY            = undefined_real
    NoahmpIO%CANICEXY        = undefined_real
    NoahmpIO%CANLIQXY        = undefined_real
    NoahmpIO%EAHXY           = undefined_real
    NoahmpIO%TAHXY           = undefined_real
    NoahmpIO%CMXY            = undefined_real
    NoahmpIO%CHXY            = undefined_real
    NoahmpIO%FWETXY          = undefined_real
    NoahmpIO%SNEQVOXY        = undefined_real
    NoahmpIO%ALBOLDXY        = undefined_real
    NoahmpIO%QSNOWXY         = undefined_real
    NoahmpIO%QRAINXY         = undefined_real
    NoahmpIO%WSLAKEXY        = undefined_real
    NoahmpIO%ZWTXY           = undefined_real
    NoahmpIO%WAXY            = undefined_real
    NoahmpIO%WTXY            = undefined_real
    NoahmpIO%TSNOXY          = undefined_real
    NoahmpIO%SNICEXY         = undefined_real
    NoahmpIO%SNLIQXY         = undefined_real
    NoahmpIO%LFMASSXY        = undefined_real
    NoahmpIO%RTMASSXY        = undefined_real
    NoahmpIO%STMASSXY        = undefined_real
    NoahmpIO%WOODXY          = undefined_real
    NoahmpIO%STBLCPXY        = undefined_real
    NoahmpIO%FASTCPXY        = undefined_real
    NoahmpIO%LAI             = undefined_real
    NoahmpIO%XSAIXY          = undefined_real
    NoahmpIO%XLONG           = undefined_real
    NoahmpIO%SEAICE          = undefined_real
    NoahmpIO%SMCWTDXY        = undefined_real
    NoahmpIO%ZSNSOXY         = undefined_real
    NoahmpIO%GRDFLX          = undefined_real
    NoahmpIO%HFX             = undefined_real
    NoahmpIO%LH              = undefined_real
    NoahmpIO%EMISS           = undefined_real
    NoahmpIO%SNOWC           = undefined_real
    NoahmpIO%T2MVXY          = undefined_real
    NoahmpIO%T2MBXY          = undefined_real
    NoahmpIO%Q2MVXY          = undefined_real
    NoahmpIO%Q2MBXY          = undefined_real
    NoahmpIO%TRADXY          = undefined_real
    NoahmpIO%NEEXY           = undefined_real
    NoahmpIO%GPPXY           = undefined_real
    NoahmpIO%NPPXY           = undefined_real
    NoahmpIO%FVEGXY          = undefined_real
    NoahmpIO%RUNSFXY         = undefined_real
    NoahmpIO%RUNSBXY         = undefined_real
    NoahmpIO%ECANXY          = undefined_real
    NoahmpIO%EDIRXY          = undefined_real
    NoahmpIO%ETRANXY         = undefined_real
    NoahmpIO%FSAXY           = undefined_real
    NoahmpIO%FIRAXY          = undefined_real
    NoahmpIO%APARXY          = undefined_real
    NoahmpIO%PSNXY           = undefined_real
    NoahmpIO%SAVXY           = undefined_real
    NoahmpIO%SAGXY           = undefined_real
    NoahmpIO%RSSUNXY         = undefined_real
    NoahmpIO%RSSHAXY         = undefined_real
    NoahmpIO%BGAPXY          = undefined_real
    NoahmpIO%WGAPXY          = undefined_real
    NoahmpIO%TGVXY           = undefined_real
    NoahmpIO%TGBXY           = undefined_real
    NoahmpIO%CHVXY           = undefined_real
    NoahmpIO%CHBXY           = undefined_real
    NoahmpIO%SHGXY           = undefined_real
    NoahmpIO%SHCXY           = undefined_real
    NoahmpIO%SHBXY           = undefined_real
    NoahmpIO%EVGXY           = undefined_real
    NoahmpIO%EVBXY           = undefined_real
    NoahmpIO%GHVXY           = undefined_real
    NoahmpIO%GHBXY           = undefined_real
    NoahmpIO%IRGXY           = undefined_real
    NoahmpIO%IRCXY           = undefined_real
    NoahmpIO%IRBXY           = undefined_real
    NoahmpIO%TRXY            = undefined_real
    NoahmpIO%EVCXY           = undefined_real
    NoahmpIO%CHLEAFXY        = undefined_real
    NoahmpIO%CHUCXY          = undefined_real
    NoahmpIO%CHV2XY          = undefined_real
    NoahmpIO%CHB2XY          = undefined_real
    NoahmpIO%RS              = undefined_real
    NoahmpIO%CANHSXY         = undefined_real
    NoahmpIO%Z0              = undefined_real
    NoahmpIO%ZNT             = undefined_real
    NoahmpIO%TAUSSXY         = 0.0
    NoahmpIO%DEEPRECHXY      = 0.0
    NoahmpIO%RECHXY          = 0.0
    NoahmpIO%ACSNOM          = 0.0
    NoahmpIO%ACSNOW          = 0.0
    NoahmpIO%MP_RAINC        = 0.0
    NoahmpIO%MP_RAINNC       = 0.0
    NoahmpIO%MP_SHCV         = 0.0
    NoahmpIO%MP_SNOW         = 0.0
    NoahmpIO%MP_GRAUP        = 0.0
    NoahmpIO%MP_HAIL         = 0.0
    NoahmpIO%SFCRUNOFF       = 0.0
    NoahmpIO%UDRUNOFF        = 0.0

    ! additional output
    NoahmpIO%PAHXY           = undefined_real
    NoahmpIO%PAHGXY          = undefined_real
    NoahmpIO%PAHBXY          = undefined_real
    NoahmpIO%PAHVXY          = undefined_real
    NoahmpIO%QINTSXY         = undefined_real
    NoahmpIO%QINTRXY         = undefined_real
    NoahmpIO%QDRIPSXY        = undefined_real
    NoahmpIO%QDRIPRXY        = undefined_real
    NoahmpIO%QTHROSXY        = undefined_real
    NoahmpIO%QTHRORXY        = undefined_real
    NoahmpIO%QSNSUBXY        = undefined_real
    NoahmpIO%QSNFROXY        = undefined_real
    NoahmpIO%QSUBCXY         = undefined_real
    NoahmpIO%QFROCXY         = undefined_real
    NoahmpIO%QEVACXY         = undefined_real
    NoahmpIO%QDEWCXY         = undefined_real
    NoahmpIO%QFRZCXY         = undefined_real
    NoahmpIO%QMELTCXY        = undefined_real
    NoahmpIO%QSNBOTXY        = undefined_real
    NoahmpIO%QMELTXY         = undefined_real
    NoahmpIO%FPICEXY         = undefined_real
    NoahmpIO%RAINLSM         = undefined_real
    NoahmpIO%SNOWLSM         = undefined_real
    NoahmpIO%FORCTLSM        = undefined_real
    NoahmpIO%FORCQLSM        = undefined_real
    NoahmpIO%FORCPLSM        = undefined_real
    NoahmpIO%FORCZLSM        = undefined_real
    NoahmpIO%FORCWLSM        = undefined_real
    NoahmpIO%EFLXBXY         = undefined_real
    NoahmpIO%SOILENERGY      = undefined_real
    NoahmpIO%SNOWENERGY      = undefined_real
    NoahmpIO%PONDINGXY       = 0.0
    NoahmpIO%ACC_SSOILXY     = 0.0
    NoahmpIO%ACC_QINSURXY    = 0.0
    NoahmpIO%ACC_QSEVAXY     = 0.0
    NoahmpIO%ACC_ETRANIXY    = 0.0
    NoahmpIO%ACC_DWATERXY    = 0.0
    NoahmpIO%ACC_PRCPXY      = 0.0
    NoahmpIO%ACC_ECANXY      = 0.0
    NoahmpIO%ACC_ETRANXY     = 0.0
    NoahmpIO%ACC_EDIRXY      = 0.0

    ! MMF Groundwater
    NoahmpIO%TERRAIN         = undefined_real
    NoahmpIO%GVFMIN          = undefined_real
    NoahmpIO%GVFMAX          = undefined_real
    NoahmpIO%MSFTX           = undefined_real
    NoahmpIO%MSFTY           = undefined_real
    NoahmpIO%EQZWT           = undefined_real
    NoahmpIO%RIVERBEDXY      = undefined_real
    NoahmpIO%RIVERCONDXY     = undefined_real
    NoahmpIO%PEXPXY          = undefined_real
    NoahmpIO%FDEPTHXY        = undefined_real
    NoahmpIO%AREAXY          = undefined_real
    NoahmpIO%QRFSXY          = undefined_real
    NoahmpIO%QSPRINGSXY      = undefined_real
    NoahmpIO%QRFXY           = undefined_real
    NoahmpIO%QSPRINGXY       = undefined_real
    NoahmpIO%QSLATXY         = undefined_real
    NoahmpIO%QLATXY          = undefined_real

    ! crop model
    NoahmpIO%PGSXY           = undefined_int
    NoahmpIO%CROPCAT         = undefined_int
    NoahmpIO%PLANTING        = undefined_real
    NoahmpIO%HARVEST         = undefined_real
    NoahmpIO%SEASON_GDD      = undefined_real
    NoahmpIO%CROPTYPE        = undefined_real

    ! tile drainage
    NoahmpIO%QTDRAIN         = 0.0
    NoahmpIO%TD_FRACTION     = undefined_real

    ! irrigation
    NoahmpIO%IRFRACT         = 0.0
    NoahmpIO%SIFRACT         = 0.0
    NoahmpIO%MIFRACT         = 0.0
    NoahmpIO%FIFRACT         = 0.0
    NoahmpIO%IRNUMSI         = 0
    NoahmpIO%IRNUMMI         = 0
    NoahmpIO%IRNUMFI         = 0
    NoahmpIO%IRWATSI         = 0.0
    NoahmpIO%IRWATMI         = 0.0
    NoahmpIO%IRWATFI         = 0.0
    NoahmpIO%IRELOSS         = 0.0
    NoahmpIO%IRSIVOL         = 0.0
    NoahmpIO%IRMIVOL         = 0.0
    NoahmpIO%IRFIVOL         = 0.0
    NoahmpIO%IRRSPLH         = 0.0
    NoahmpIO%LOCTIM          = undefined_real

    ! spatial varying soil texture
    if ( NoahmpIO%IOPT_SOIL > 1 ) then
       NoahmpIO%SOILCL1      = undefined_real
       NoahmpIO%SOILCL2      = undefined_real
       NoahmpIO%SOILCL3      = undefined_real
       NoahmpIO%SOILCL4      = undefined_real
       NoahmpIO%SOILCOMP     = undefined_real
    endif

    ! urban model 
    if ( NoahmpIO%SF_URBAN_PHYSICS > 0 ) then
       NoahmpIO%JULDAY        = undefined_int_neg
       NoahmpIO%IRI_URBAN     = undefined_int_neg
       NoahmpIO%utype_urb2d   = undefined_int_neg
       NoahmpIO%HRANG         = undefined_real_neg
       NoahmpIO%DECLIN        = undefined_real_neg
       NoahmpIO%sh_urb2d      = undefined_real_neg
       NoahmpIO%lh_urb2d      = undefined_real_neg
       NoahmpIO%g_urb2d       = undefined_real_neg
       NoahmpIO%rn_urb2d      = undefined_real_neg
       NoahmpIO%ts_urb2d      = undefined_real_neg
       NoahmpIO%GMT           = undefined_real_neg
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
       NoahmpIO%GZ1OZ0_urb2d  = undefined_real_neg
       NoahmpIO%AKMS_URB2D    = undefined_real_neg
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

    NoahmpIO%XLAND             = 1.0      ! water = 2.0, land = 1.0
    NoahmpIO%XICE              = 0.0      ! fraction of grid that is seaice
    NoahmpIO%XICE_THRESHOLD    = 0.5      ! fraction of grid determining seaice (from WRF)
    NoahmpIO%SLOPETYP          = 1        ! soil parameter slope type
    NoahmpIO%soil_update_steps = 1        ! number of model time step to update soil proces
    NoahmpIO%calculate_soil    = .false.  ! index for if do soil process
    NoahmpIO%ITIMESTEP         = 0        ! model time step count

#ifdef WRF_HYDRO
    NoahmpIO%infxsrt         = 0.0
    NoahmpIO%sfcheadrt       = 0.0 
    NoahmpIO%soldrain        = 0.0
    NoahmpIO%qtiledrain      = 0.0
    NoahmpIO%ZWATBLE2D       = 0.0
#endif 
   
    end associate
 
  end subroutine NoahmpIOVarInitDefault

end module NoahmpIOVarInitMod
