module ResistanceBareGroundMostMod

!!! Compute bare ground resistance and drag coefficient for momentum and heat
!!! based on Monin-Obukhov (M-O) Similarity Theory (MOST)

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine ResistanceBareGroundMOST(noahmp, IndIter, HeatSensibleTmp, MoStabParaSgn)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: SFCDIF1 for bare ground portion
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

! in & out variables
    integer               , intent(in   ) :: IndIter                     ! iteration index
    integer               , intent(inout) :: MoStabParaSgn               ! number of times moz changes sign
    real(kind=kind_noahmp), intent(in   ) :: HeatSensibleTmp             ! temporary sensible heat flux (w/m2) in each iteration
    type(noahmp_type)     , intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp)                :: MPE                         ! prevents overflow for division by zero
    real(kind=kind_noahmp)                :: TMPCM                       ! temporary calculation for CM
    real(kind=kind_noahmp)                :: TMPCH                       ! temporary calculation for CH
    real(kind=kind_noahmp)                :: FMNEW                       ! stability correction factor, momentum, for current moz
    real(kind=kind_noahmp)                :: FHNEW                       ! stability correction factor, sen heat, for current moz
    real(kind=kind_noahmp)                :: MOZOLD                      ! Monin-Obukhov stability parameter from prior iteration
    real(kind=kind_noahmp)                :: TMP1,TMP2,TMP3,TMP4,TMP5    ! temporary calculation
    real(kind=kind_noahmp)                :: TVIR                        ! temporary virtual temperature (k)
    real(kind=kind_noahmp)                :: TMPCM2                      ! temporary calculation for CM2
    real(kind=kind_noahmp)                :: TMPCH2                      ! temporary calculation for CH2
    real(kind=kind_noahmp)                :: FM2NEW                      ! stability correction factor, momentum, for current moz
    real(kind=kind_noahmp)                :: FH2NEW                      ! stability correction factor, sen heat, for current moz
    real(kind=kind_noahmp)                :: TMP12,TMP22,TMP32           ! temporary calculation
    real(kind=kind_noahmp)                :: CMFM, CHFH, CM2FM2, CH2FH2  ! temporary calculation

! --------------------------------------------------------------------
    associate(                                                                     &
              TemperatureAirRefHeight => noahmp%forcing%TemperatureAirRefHeight   ,& ! in,    air temperature [K] at reference height
              SpecHumidityRefHeight   => noahmp%forcing%SpecHumidityRefHeight     ,& ! in,    specific humidity [kg/kg] at reference height
              RefHeightAboveGrd       => noahmp%energy%state%RefHeightAboveGrd    ,& ! in,    reference height [m] above ground
              DensityAirRefHeight     => noahmp%energy%state%DensityAirRefHeight  ,& ! in,    density air [kg/m3]
              WindSpdRefHeight        => noahmp%energy%state%WindSpdRefHeight     ,& ! in,    wind speed [m/s] at reference height
              ZeroPlaneDispGrd        => noahmp%energy%state%ZeroPlaneDispGrd     ,& ! in,    ground zero plane displacement [m]
              RoughLenShBareGrd       => noahmp%energy%state%RoughLenShBareGrd    ,& ! in,    roughness length [m], sensible heat, bare ground
              RoughLenMomGrd          => noahmp%energy%state%RoughLenMomGrd       ,& ! in,    roughness length [m], momentum, ground
              MoStabCorrMomBare       => noahmp%energy%state%MoStabCorrMomBare    ,& ! inout, M-O momentum stability correction, above ZeroPlaneDisp, bare ground
              MoStabCorrShBare        => noahmp%energy%state%MoStabCorrShBare     ,& ! inout, M-O sen heat stability correction, above ZeroPlaneDisp, bare ground
              MoStabCorrMomBare2m     => noahmp%energy%state%MoStabCorrMomBare2m  ,& ! inout, M-O momentum stability correction, 2m, bare ground
              MoStabCorrShBare2m      => noahmp%energy%state%MoStabCorrShBare2m   ,& ! inout, M-O sen heat stability correction, 2m, bare ground
              FrictionVelBare         => noahmp%energy%state%FrictionVelBare      ,& ! inout, friction velocity [m/s], bare ground
              MoStabParaBare          => noahmp%energy%state%MoStabParaBare       ,& ! inout, Monin-Obukhov stability (z/L), above ZeroPlaneDisp, bare ground
              MoStabParaBare2m        => noahmp%energy%state%MoStabParaBare2m     ,& ! out,   Monin-Obukhov stability (z/L), 2m, bare ground
              MoLengthBare            => noahmp%energy%state%MoLengthBare         ,& ! out,   Monin-Obukhov length [m], above ZeroPlaneDisp, bare ground
              ExchCoeffMomBare        => noahmp%energy%state%ExchCoeffMomBare     ,& ! out,   exchange coeff [m/s] for momentum, above ZeroPlaneDisp, bare ground
              ExchCoeffShBare         => noahmp%energy%state%ExchCoeffShBare      ,& ! out,   exchange coeff [m/s]  for heat, above ZeroPlaneDisp, bare ground
              ExchCoeffSh2mBareMo     => noahmp%energy%state%ExchCoeffSh2mBareMo  ,& ! out,   exchange coeff [m/s] for heat, 2m, bare ground
              ResistanceMomBareGrd    => noahmp%energy%state%ResistanceMomBareGrd ,& ! out,   aerodynamic resistance for momentum [s/m], bare ground
              ResistanceShBareGrd     => noahmp%energy%state%ResistanceShBareGrd  ,& ! out,   aerodynamic resistance for sensible heat [s/m], bare ground
              ResistanceLhBareGrd     => noahmp%energy%state%ResistanceLhBareGrd   & ! out,   aerodynamic resistance for water vapor [s/m], bare ground
             )
! ----------------------------------------------------------------------

    ! initialization
    MPE    = 1.0e-6
    MOZOLD = MoStabParaBare  ! M-O stability parameter for next iteration
    if ( RefHeightAboveGrd <= ZeroPlaneDispGrd ) then
       write(*,*) "WARNING: critical problem: RefHeightAboveGrd <= ZeroPlaneDispGrd; model stops"
       stop "Error in ResistanceBareGroundMostMod.F90"
    endif

    ! temporary drag coefficients
    TMPCM  = log((RefHeightAboveGrd - ZeroPlaneDispGrd) / RoughLenMomGrd)
    TMPCH  = log((RefHeightAboveGrd - ZeroPlaneDispGrd) / RoughLenShBareGrd)
    TMPCM2 = log((2.0 + RoughLenMomGrd) / RoughLenMomGrd)
    TMPCH2 = log((2.0 + RoughLenShBareGrd) / RoughLenShBareGrd)

    ! compute M-O stability parameter
    if ( IndIter == 1 ) then
       FrictionVelBare  = 0.0
       MoStabParaBare   = 0.0
       MoLengthBare     = 0.0
       MoStabParaBare2m = 0.0
    else
       TVIR = (1.0 + 0.61*SpecHumidityRefHeight) * TemperatureAirRefHeight
       TMP1 = ConstVonKarman * (ConstGravityAcc/TVIR) * HeatSensibleTmp / (DensityAirRefHeight*ConstHeatCapacAir)
       if ( abs(TMP1) <= MPE ) TMP1 = MPE
       MoLengthBare     = -1.0 * FrictionVelBare**3 / TMP1
       MoStabParaBare   = min((RefHeightAboveGrd - ZeroPlaneDispGrd) / MoLengthBare, 1.0)
       MoStabParaBare2m = min((2.0 + RoughLenShBareGrd) / MoLengthBare, 1.0)
    endif

    ! accumulate number of times moz changes sign.
    if ( MOZOLD*MoStabParaBare < 0.0 ) MoStabParaSgn = MoStabParaSgn + 1
    if ( MoStabParaSgn >= 2 ) then
       MoStabParaBare      = 0.0
       MoStabCorrMomBare   = 0.0
       MoStabCorrShBare    = 0.0
       MoStabParaBare2m    = 0.0
       MoStabCorrMomBare2m = 0.0
       MoStabCorrShBare2m  = 0.0
    endif

    ! evaluate stability-dependent variables using moz from prior iteration
    if ( MoStabParaBare < 0.0 ) then
       TMP1   = (1.0 - 16.0 * MoStabParaBare)**0.25
       TMP2   = log((1.0 + TMP1*TMP1) / 2.0)
       TMP3   = log((1.0 + TMP1) / 2.0)
       FMNEW  = 2.0 * TMP3 + TMP2 - 2.0 * atan(TMP1) + 1.5707963
       FHNEW  = 2 * TMP2
       ! 2-meter quantities
       TMP12  = (1.0 - 16.0 * MoStabParaBare2m)**0.25
       TMP22  = log((1.0 + TMP12*TMP12) / 2.0)
       TMP32  = log((1.0 + TMP12) / 2.0)
       FM2NEW = 2.0 * TMP32 + TMP22 - 2.0 * atan(TMP12) + 1.5707963
       FH2NEW = 2 * TMP22
    else
       FMNEW  = -5.0 * MoStabParaBare
       FHNEW  = FMNEW
       FM2NEW = -5.0 * MoStabParaBare2m
       FH2NEW = FM2NEW
    endif

    ! except for first iteration, weight stability factors for previous
    ! iteration to help avoid flip-flops from one iteration to the next
    if ( IndIter == 1 ) then
       MoStabCorrMomBare   = FMNEW
       MoStabCorrShBare    = FHNEW
       MoStabCorrMomBare2m = FM2NEW
       MoStabCorrShBare2m  = FH2NEW
    else
       MoStabCorrMomBare   = 0.5 * (MoStabCorrMomBare   + FMNEW)
       MoStabCorrShBare    = 0.5 * (MoStabCorrShBare    + FHNEW)
       MoStabCorrMomBare2m = 0.5 * (MoStabCorrMomBare2m + FM2NEW)
       MoStabCorrShBare2m  = 0.5 * (MoStabCorrShBare2m  + FH2NEW)
    endif

    ! exchange coefficients
    MoStabCorrShBare    = min(MoStabCorrShBare   , 0.9*TMPCH )
    MoStabCorrMomBare   = min(MoStabCorrMomBare  , 0.9*TMPCM )
    MoStabCorrShBare2m  = min(MoStabCorrShBare2m , 0.9*TMPCH2)
    MoStabCorrMomBare2m = min(MoStabCorrMomBare2m, 0.9*TMPCM2)
    CMFM   = TMPCM  - MoStabCorrMomBare
    CHFH   = TMPCH  - MoStabCorrShBare
    CM2FM2 = TMPCM2 - MoStabCorrMomBare2m
    CH2FH2 = TMPCH2 - MoStabCorrShBare2m
    if ( abs(CMFM) <= MPE )   CMFM   = MPE
    if ( abs(CHFH) <= MPE )   CHFH   = MPE
    if ( abs(CM2FM2) <= MPE ) CM2FM2 = MPE
    if ( abs(CH2FH2) <= MPE ) CH2FH2 = MPE
    ExchCoeffMomBare    = ConstVonKarman * ConstVonKarman / (CMFM * CMFM)
    ExchCoeffShBare     = ConstVonKarman * ConstVonKarman / (CMFM * CHFH)
    !ExchCoeffSh2mBareMo = ConstVonKarman * ConstVonKarman / (CM2FM2 * CH2FH2)

    ! friction velocity
    FrictionVelBare     = WindSpdRefHeight * sqrt(ExchCoeffMomBare)
    ExchCoeffSh2mBareMo = ConstVonKarman * FrictionVelBare / CH2FH2

    ! aerodynamic resistance
    ResistanceMomBareGrd = max(1.0, 1.0/(ExchCoeffMomBare*WindSpdRefHeight))
    ResistanceShBareGrd  = max(1.0, 1.0/(ExchCoeffShBare*WindSpdRefHeight))
    ResistanceLhBareGrd  = ResistanceShBareGrd

    end associate

  end subroutine ResistanceBareGroundMOST

end module ResistanceBareGroundMostMod
