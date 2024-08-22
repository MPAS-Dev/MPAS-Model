module ResistanceCanopyStomataJarvisMod

!!! Compute canopy stomatal resistance and foliage photosynthesis based on Jarvis scheme
!!! Canopy resistance which depends on incoming solar radiation, air temperature,
!!! atmospheric water vapor pressure deficit at the lowest model level, and soil moisture (preferably
!!! unfrozen soil moisture rather than total). 
!!! Source: Jarvis (1976), Noilhan and Planton (1989), Jacquemin and Noilhan (1990). 
!!! See also Chen et al (1996, JGR, Vol 101(D3), 7251-7268): Eqns 12-14 and Table 2 of Sec. 3.1.2

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use HumiditySaturationMod, only : HumiditySaturation

  implicit none

contains

  subroutine ResistanceCanopyStomataJarvis(noahmp, IndexShade)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: CANRES
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    integer          , intent(in   ) :: IndexShade            ! index for sunlit/shaded (0=sunlit;1=shaded)
    type(noahmp_type), intent(inout) :: noahmp

! local variables
    real(kind=kind_noahmp)           :: ResistanceVapDef      ! canopy resistance multiplier
    real(kind=kind_noahmp)           :: ResistanceSolar       ! canopy resistance multiplier
    real(kind=kind_noahmp)           :: ResistanceTemp        ! canopy resistance multiplier
    real(kind=kind_noahmp)           :: RadFac                ! solar radiation factor for resistance
    real(kind=kind_noahmp)           :: SpecHumidityTmp       ! specific humidity [kg/kg]
    real(kind=kind_noahmp)           :: MixingRatioTmp        ! mixing ratio [kg/kg]
    real(kind=kind_noahmp)           :: MixingRatioSat        ! saturated mixing ratio [kg/kg]
    real(kind=kind_noahmp)           :: MixingRatioSatTempD   ! d(MixingRatioSat)/d(T)
    real(kind=kind_noahmp)           :: RadPhotoActAbsTmp     ! temporary absorbed par for leaves [W/m2]
    real(kind=kind_noahmp)           :: ResistanceStomataTmp  ! temporary leaf stomatal resistance [s/m]
    real(kind=kind_noahmp)           :: PhotosynLeafTmp       ! temporary leaf photosynthesis [umol co2/m2/s]

! --------------------------------------------------------------------
    associate(                                                                        &
              PressureAirRefHeight    => noahmp%forcing%PressureAirRefHeight         ,& ! in,  air pressure [Pa] at reference height
              SoilTranspFacAcc        => noahmp%water%state%SoilTranspFacAcc         ,& ! in,  accumulated soil water transpiration factor (0 to 1)
              RadiationStressFac      => noahmp%energy%param%RadiationStressFac      ,& ! in,  Parameter used in radiation stress function
              ResistanceStomataMin    => noahmp%energy%param%ResistanceStomataMin    ,& ! in,  Minimum stomatal resistance [s m-1]
              ResistanceStomataMax    => noahmp%energy%param%ResistanceStomataMax    ,& ! in,  Maximal stomatal resistance [s m-1]
              AirTempOptimTransp      => noahmp%energy%param%AirTempOptimTransp      ,& ! in,  Optimum transpiration air temperature [K]
              VaporPresDeficitFac     => noahmp%energy%param%VaporPresDeficitFac     ,& ! in,  Parameter used in vapor pressure deficit function
              TemperatureCanopy       => noahmp%energy%state%TemperatureCanopy       ,& ! in,  vegetation temperature [K]
              PressureVaporCanAir     => noahmp%energy%state%PressureVaporCanAir     ,& ! in,  canopy air vapor pressure [Pa]
              VegFrac                 => noahmp%energy%state%VegFrac                 ,& ! in,  greeness vegetation fraction
              RadPhotoActAbsSunlit    => noahmp%energy%flux%RadPhotoActAbsSunlit     ,& ! in,  average absorbed par for sunlit leaves [W/m2]
              RadPhotoActAbsShade     => noahmp%energy%flux%RadPhotoActAbsShade      ,& ! in,  average absorbed par for shaded leaves [W/m2]
              ResistanceStomataSunlit => noahmp%energy%state%ResistanceStomataSunlit ,& ! out, sunlit leaf stomatal resistance [s/m]
              ResistanceStomataShade  => noahmp%energy%state%ResistanceStomataShade  ,& ! out, shaded leaf stomatal resistance [s/m]
              PhotosynLeafSunlit      => noahmp%biochem%flux%PhotosynLeafSunlit      ,& ! out, sunlit leaf photosynthesis [umol CO2/m2/s]
              PhotosynLeafShade       => noahmp%biochem%flux%PhotosynLeafShade        & ! out, shaded leaf photosynthesis [umol CO2/m2/s]
             )
! ----------------------------------------------------------------------

    ! initialization
    ResistanceSolar      = 0.0
    ResistanceTemp       = 0.0
    ResistanceVapDef     = 0.0
    ResistanceStomataTmp = 0.0
    if ( IndexShade == 0 ) RadPhotoActAbsTmp = RadPhotoActAbsSunlit / max(VegFrac,1.0e-6) ! Sunlit case
    if ( IndexShade == 1 ) RadPhotoActAbsTmp = RadPhotoActAbsShade  / max(VegFrac,1.0e-6) ! Shaded case

    ! compute MixingRatioTmp and MixingRatioSat
    SpecHumidityTmp = 0.622 * PressureVaporCanAir / (PressureAirRefHeight - 0.378*PressureVaporCanAir) ! specific humidity
    MixingRatioTmp  = SpecHumidityTmp / (1.0 - SpecHumidityTmp)   ! convert to mixing ratio [kg/kg]
    call HumiditySaturation(TemperatureCanopy, PressureAirRefHeight, MixingRatioSat, MixingRatioSatTempD)

    ! contribution due to incoming solar radiation
    RadFac          = 2.0 * RadPhotoActAbsTmp / RadiationStressFac
    ResistanceSolar = (RadFac + ResistanceStomataMin/ResistanceStomataMax) / (1.0 + RadFac)
    ResistanceSolar = max(ResistanceSolar, 0.0001)

    ! contribution due to air temperature
    ResistanceTemp = 1.0 - 0.0016 * ((AirTempOptimTransp - TemperatureCanopy)**2.0)
    ResistanceTemp = max(ResistanceTemp, 0.0001)

    ! contribution due to vapor pressure deficit
    ResistanceVapDef = 1.0 / (1.0 + VaporPresDeficitFac * max(0.0, MixingRatioSat - MixingRatioTmp))
    ResistanceVapDef = max(ResistanceVapDef, 0.01)

    ! determine canopy resistance due to all factors
    ResistanceStomataTmp = ResistanceStomataMin / (ResistanceSolar * ResistanceTemp * ResistanceVapDef * SoilTranspFacAcc)
    PhotosynLeafTmp      = -999.99       ! photosynthesis not applied for dynamic carbon

    ! assign updated values
    ! Sunlit case
    if ( IndexShade == 0 ) then
       ResistanceStomataSunlit = ResistanceStomataTmp
       PhotosynLeafSunlit      = PhotosynLeafTmp
    endif
    ! Shaded case
    if ( IndexShade == 1 ) then
       ResistanceStomataShade  = ResistanceStomataTmp
       PhotosynLeafShade       = PhotosynLeafTmp
    endif

    end associate

  end subroutine ResistanceCanopyStomataJarvis

end module ResistanceCanopyStomataJarvisMod
