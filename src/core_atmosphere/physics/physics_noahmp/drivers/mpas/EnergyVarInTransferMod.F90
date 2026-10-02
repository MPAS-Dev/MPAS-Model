module EnergyVarInTransferMod

!!! Transfer input 2-D NoahmpIO Energy variables to 1-D column variable
!!! 1-D variables should be first defined in /src/EnergyVarType.F90
!!! 2-D variables should be first defined in NoahmpIOVarType.F90

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType
  use NoahmpVarType

  implicit none

contains

!=== initialize with input data or table values

  subroutine EnergyVarInTransfer(noahmp, NoahmpIO)

    implicit none

    type(NoahmpIO_type), intent(inout) :: NoahmpIO
    type(noahmp_type),   intent(inout) :: noahmp

    ! local loop index
    integer                          :: SoilLayerIndex

! -------------------------------------------------------------------------
    associate(                                                         &
              I               => noahmp%config%domain%GridIndexI      ,&
              VegType         => noahmp%config%domain%VegType         ,&
              SoilType        => noahmp%config%domain%SoilType        ,&
              CropType        => noahmp%config%domain%CropType        ,&
              SoilColor       => noahmp%config%domain%SoilColor       ,&
              FlagUrban       => noahmp%config%domain%FlagUrban       ,&
              NumSnowLayerMax => noahmp%config%domain%NumSnowLayerMax ,&
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer    ,&
              NumSwRadBand    => noahmp%config%domain%NumSwRadBand     &
             )
! -------------------------------------------------------------------------

    ! energy state variables
    noahmp%energy%state%LeafAreaIndex                             = NoahmpIO%LAI     (I)
    noahmp%energy%state%StemAreaIndex                             = NoahmpIO%XSAIXY  (I)
    noahmp%energy%state%SpecHumiditySfcMean                       = NoahmpIO%QSFC    (I)
    noahmp%energy%state%TemperatureGrd                            = NoahmpIO%TGXY    (I)
    noahmp%energy%state%TemperatureCanopy                         = NoahmpIO%TVXY    (I)
    noahmp%energy%state%SnowAgeNondim                             = NoahmpIO%TAUSSXY (I)
    noahmp%energy%state%AlbedoSnowPrev                            = NoahmpIO%ALBOLDXY(I)
    noahmp%energy%state%PressureVaporCanAir                       = NoahmpIO%EAHXY   (I)
    noahmp%energy%state%TemperatureCanopyAir                      = NoahmpIO%TAHXY   (I)
    noahmp%energy%state%ExchCoeffShSfc                            = NoahmpIO%CHXY    (I) 
    noahmp%energy%state%ExchCoeffMomSfc                           = NoahmpIO%CMXY    (I)
    noahmp%energy%state%TemperatureSoilSnow(-NumSnowLayerMax+1:0) = NoahmpIO%TSNOXY  (I,-NumSnowLayerMax+1:0)
    noahmp%energy%state%TemperatureSoilSnow(1:NumSoilLayer)       = NoahmpIO%TSLB    (I,1:NumSoilLayer)
    noahmp%energy%state%PressureAtmosCO2                          = NoahmpIO%CO2_TABLE * noahmp%forcing%PressureAirRefHeight
    noahmp%energy%state%PressureAtmosO2                           = NoahmpIO%O2_TABLE  * noahmp%forcing%PressureAirRefHeight
    ! vegetation treatment for USGS land types (playa, lava, sand to bare)
    if ( (VegType == 25) .or. (VegType == 26) .or. (VegType == 27) ) then
       noahmp%energy%state%VegFrac       = 0.0
       noahmp%energy%state%LeafAreaIndex = 0.0
    endif

    ! energy flux variables
    noahmp%energy%flux%HeatGroundTotAcc                           = NoahmpIO%ACC_SSOILXY(I)

    ! energy parameter variables
    noahmp%energy%param%SoilHeatCapacity                          = NoahmpIO%CSOIL_TABLE
    noahmp%energy%param%SnowAgeFacBats                            = NoahmpIO%TAU0_TABLE
    noahmp%energy%param%SnowGrowVapFacBats                        = NoahmpIO%GRAIN_GROWTH_TABLE
    noahmp%energy%param%SnowSootFacBats                           = NoahmpIO%DIRT_SOOT_TABLE
    noahmp%energy%param%SnowGrowFrzFacBats                        = NoahmpIO%EXTRA_GROWTH_TABLE
    noahmp%energy%param%SolarZenithAdjBats                        = NoahmpIO%BATS_COSZ_TABLE
    noahmp%energy%param%FreshSnoAlbVisBats                        = NoahmpIO%BATS_VIS_NEW_TABLE
    noahmp%energy%param%FreshSnoAlbNirBats                        = NoahmpIO%BATS_NIR_NEW_TABLE
    noahmp%energy%param%SnoAgeFacDifVisBats                       = NoahmpIO%BATS_VIS_AGE_TABLE
    noahmp%energy%param%SnoAgeFacDifNirBats                       = NoahmpIO%BATS_NIR_AGE_TABLE
    noahmp%energy%param%SzaFacDirVisBats                          = NoahmpIO%BATS_VIS_DIR_TABLE
    noahmp%energy%param%SzaFacDirNirBats                          = NoahmpIO%BATS_NIR_DIR_TABLE
    noahmp%energy%param%SnowAlbRefClass                           = NoahmpIO%CLASS_ALB_REF_TABLE
    noahmp%energy%param%SnowAgeFacClass                           = NoahmpIO%CLASS_SNO_AGE_TABLE
    noahmp%energy%param%SnowAlbFreshClass                         = NoahmpIO%CLASS_ALB_NEW_TABLE
    noahmp%energy%param%UpscatterCoeffSnowDir                     = NoahmpIO%BETADS_TABLE
    noahmp%energy%param%UpscatterCoeffSnowDif                     = NoahmpIO%BETAIS_TABLE
    noahmp%energy%param%ZilitinkevichCoeff                        = NoahmpIO%CZIL_TABLE
    noahmp%energy%param%EmissivitySnow                            = NoahmpIO%SNOW_EMIS_TABLE
    noahmp%energy%param%EmissivitySoilLake                        = NoahmpIO%EG_TABLE
    noahmp%energy%param%AlbedoLandIce                             = NoahmpIO%ALBICE_TABLE
    noahmp%energy%param%RoughLenMomSnow                           = NoahmpIO%Z0SNO_TABLE
    noahmp%energy%param%RoughLenMomSoil                           = NoahmpIO%Z0SOIL_TABLE
    noahmp%energy%param%RoughLenMomLake                           = NoahmpIO%Z0LAKE_TABLE
    noahmp%energy%param%EmissivityIceSfc                          = NoahmpIO%EICE_TABLE
    noahmp%energy%param%ResistanceSoilExp                         = NoahmpIO%RSURF_EXP_TABLE
    noahmp%energy%param%ResistanceSnowSfc                         = NoahmpIO%RSURF_SNOW_TABLE
    noahmp%energy%param%VegFracAnnMax                             = NoahmpIO%GVFMAX(I) / 100.0
    noahmp%energy%param%VegFracGreen                              = NoahmpIO%VEGFRA(I) / 100.0
    noahmp%energy%param%TreeCrownRadius                           = NoahmpIO%RC_TABLE    (VegType)
    noahmp%energy%param%HeightCanopyTop                           = NoahmpIO%HVT_TABLE   (VegType)
    noahmp%energy%param%HeightCanopyBot                           = NoahmpIO%HVB_TABLE   (VegType)
    noahmp%energy%param%RoughLenMomVeg                            = NoahmpIO%Z0MVT_TABLE (VegType)
    noahmp%energy%param%CanopyWindExtFac                          = NoahmpIO%CWPVT_TABLE (VegType)
    noahmp%energy%param%TreeDensity                               = NoahmpIO%DEN_TABLE   (VegType)
    noahmp%energy%param%CanopyOrientIndex                         = NoahmpIO%XL_TABLE    (VegType)
    noahmp%energy%param%ConductanceLeafMin                        = NoahmpIO%BP_TABLE    (VegType)
    noahmp%energy%param%Co2MmConst25C                             = NoahmpIO%KC25_TABLE  (VegType)
    noahmp%energy%param%O2MmConst25C                              = NoahmpIO%KO25_TABLE  (VegType)
    noahmp%energy%param%Co2MmConstQ10                             = NoahmpIO%AKC_TABLE   (VegType)
    noahmp%energy%param%O2MmConstQ10                              = NoahmpIO%AKO_TABLE   (VegType)
    noahmp%energy%param%RadiationStressFac                        = NoahmpIO%RGL_TABLE   (VegType)
    noahmp%energy%param%ResistanceStomataMin                      = NoahmpIO%RS_TABLE    (VegType)
    noahmp%energy%param%ResistanceStomataMax                      = NoahmpIO%RSMAX_TABLE (VegType)
    noahmp%energy%param%AirTempOptimTransp                        = NoahmpIO%TOPT_TABLE  (VegType)
    noahmp%energy%param%VaporPresDeficitFac                       = NoahmpIO%HS_TABLE    (VegType)
    noahmp%energy%param%LeafDimLength                             = NoahmpIO%DLEAF_TABLE (VegType)
    noahmp%energy%param%HeatCapacCanFac                           = NoahmpIO%CBIOM_TABLE (VegType)
    noahmp%energy%param%LeafAreaIndexMon (1:12)                   = NoahmpIO%LAIM_TABLE  (VegType,1:12)
    noahmp%energy%param%StemAreaIndexMon (1:12)                   = NoahmpIO%SAIM_TABLE  (VegType,1:12)
    noahmp%energy%param%ReflectanceLeaf  (1:NumSwRadBand)         = NoahmpIO%RHOL_TABLE  (VegType,1:NumSwRadBand)
    noahmp%energy%param%ReflectanceStem  (1:NumSwRadBand)         = NoahmpIO%RHOS_TABLE  (VegType,1:NumSwRadBand)
    noahmp%energy%param%TransmittanceLeaf(1:NumSwRadBand)         = NoahmpIO%TAUL_TABLE  (VegType,1:NumSwRadBand)
    noahmp%energy%param%TransmittanceStem(1:NumSwRadBand)         = NoahmpIO%TAUS_TABLE  (VegType,1:NumSwRadBand)
    noahmp%energy%param%AlbedoSoilSat    (1:NumSwRadBand)         = NoahmpIO%ALBSAT_TABLE(SoilColor,1:NumSwRadBand)
    noahmp%energy%param%AlbedoSoilDry    (1:NumSwRadBand)         = NoahmpIO%ALBDRY_TABLE(SoilColor,1:NumSwRadBand)
    noahmp%energy%param%AlbedoLakeFrz    (1:NumSwRadBand)         = NoahmpIO%ALBLAK_TABLE(1:NumSwRadBand)
    noahmp%energy%param%ScatterCoeffSnow (1:NumSwRadBand)         = NoahmpIO%OMEGAS_TABLE(1:NumSwRadBand)

    do SoilLayerIndex = 1, size(SoilType)
       noahmp%energy%param%SoilQuartzFrac(SoilLayerIndex)         = NoahmpIO%QUARTZ_TABLE(SoilType(SoilLayerIndex))
    enddo

    ! spatial varying soil input
    if ( noahmp%config%nmlist%OptSoilProperty == 4 ) then
       noahmp%energy%param%SoilQuartzFrac(1:NumSoilLayer)         = NoahmpIO%QUARTZ_3D(I,1:NumSoilLayer)
    endif

    if ( FlagUrban .eqv. .true. ) noahmp%energy%param%SoilHeatCapacity = 3.0e6

    if ( CropType > 0 ) then
       noahmp%energy%param%ConductanceLeafMin                     = NoahmpIO%BPI_TABLE  (CropType)
       noahmp%energy%param%Co2MmConst25C                          = NoahmpIO%KC25I_TABLE(CropType)
       noahmp%energy%param%O2MmConst25C                           = NoahmpIO%KO25I_TABLE(CropType)
       noahmp%energy%param%Co2MmConstQ10                          = NoahmpIO%AKCI_TABLE (CropType)
       noahmp%energy%param%O2MmConstQ10                           = NoahmpIO%AKOI_TABLE (CropType)
    endif

    end associate

  end subroutine EnergyVarInTransfer

end module EnergyVarInTransferMod
