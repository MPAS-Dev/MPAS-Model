module ResistanceLeafToGroundMod

!!! Compute under-canopy aerodynamic resistance and leaf boundary layer resistance

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine ResistanceLeafToGround(noahmp, IndIter, VegAreaIndEff, HeatSenGrdTmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: RAGRB
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    integer               , intent(in   ) :: IndIter         ! iteration index
    real(kind=kind_noahmp), intent(in   ) :: HeatSenGrdTmp   ! temporary ground sensible heat flux (w/m2) in each iteration
    real(kind=kind_noahmp), intent(in   ) :: VegAreaIndEff   ! temporary effective vegetation area index with constraint (<=6.0)
    type(noahmp_type)     , intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp)                :: MPE             ! prevents overflow for division by zero
    real(kind=kind_noahmp)                :: KH              ! turbulent transfer coefficient, sensible heat, (m2/s)
    real(kind=kind_noahmp)                :: TMP1            ! temporary calculation
    real(kind=kind_noahmp)                :: TMP2            ! temporary calculation
    real(kind=kind_noahmp)                :: TMPRAH2         ! temporary calculation for aerodynamic resistances
    real(kind=kind_noahmp)                :: TMPRB           ! temporary calculation for rb
    real(kind=kind_noahmp)                :: FHGNEW          ! temporary vars

! --------------------------------------------------------------------
    associate(                                                                      &
              LeafDimLength          => noahmp%energy%param%LeafDimLength          ,& ! in,    characteristic leaf dimension [m]
              CanopyWindExtFac       => noahmp%energy%param%CanopyWindExtFac       ,& ! in,    canopy wind extinction parameter
              DensityAirRefHeight    => noahmp%energy%state%DensityAirRefHeight    ,& ! in,    density air [kg/m3]
              TemperatureCanopyAir   => noahmp%energy%state%TemperatureCanopyAir   ,& ! in,    canopy air temperature [K]
              ZeroPlaneDispSfc       => noahmp%energy%state%ZeroPlaneDispSfc       ,& ! in,    zero plane displacement [m]
              RoughLenMomGrd         => noahmp%energy%state%RoughLenMomGrd         ,& ! in,    roughness length [m], momentum, ground
              CanopyHeight           => noahmp%energy%state%CanopyHeight           ,& ! in,    canopy height [m]
              WindSpdCanopyTop       => noahmp%energy%state%WindSpdCanopyTop       ,& ! in,    wind speed at top of canopy [m/s]
              RoughLenShCanopy       => noahmp%energy%state%RoughLenShCanopy       ,& ! in,    roughness length [m], sensible heat, canopy
              RoughLenShVegGrd       => noahmp%energy%state%RoughLenShVegGrd       ,& ! in,    roughness length [m], sensible heat ground, below canopy
              FrictionVelVeg         => noahmp%energy%state%FrictionVelVeg         ,& ! in,    friction velocity [m/s], vegetated
              MoStabCorrShUndCan     => noahmp%energy%state%MoStabCorrShUndCan     ,& ! inout, stability correction ground, below canopy
              WindExtCoeffCanopy     => noahmp%energy%state%WindExtCoeffCanopy     ,& ! out,   canopy wind extinction coefficient
              MoStabParaUndCan       => noahmp%energy%state%MoStabParaUndCan       ,& ! out,   Monin-Obukhov stability parameter ground, below canopy
              MoLengthUndCan         => noahmp%energy%state%MoLengthUndCan         ,& ! out,   Monin-Obukhov length [m], ground, below canopy
              ResistanceMomUndCan    => noahmp%energy%state%ResistanceMomUndCan    ,& ! out,   ground aerodynamic resistance for momentum [s/m]
              ResistanceShUndCan     => noahmp%energy%state%ResistanceShUndCan     ,& ! out,   ground aerodynamic resistance for sensible heat [s/m]
              ResistanceLhUndCan     => noahmp%energy%state%ResistanceLhUndCan     ,& ! out,   ground aerodynamic resistance for water vapor [s/m]
              ResistanceLeafBoundary => noahmp%energy%state%ResistanceLeafBoundary  & ! out,   bulk leaf boundary layer resistance [s/m]
             )
! ----------------------------------------------------------------------

    ! initialization
    MPE              = 1.0e-6
    MoStabParaUndCan = 0.0
    MoLengthUndCan   = 0.0

    ! stability correction to below canopy resistance
    if ( IndIter > 1 ) then
       TMP1 = ConstVonKarman * (ConstGravityAcc / TemperatureCanopyAir) * HeatSenGrdTmp / &
              (DensityAirRefHeight * ConstHeatCapacAir)
       if ( abs(TMP1) <= MPE ) TMP1 = MPE
       MoLengthUndCan   = -1.0 * FrictionVelVeg**3 / TMP1
       MoStabParaUndCan = min((ZeroPlaneDispSfc-RoughLenMomGrd)/MoLengthUndCan, 1.0)
    endif
    if ( MoStabParaUndCan < 0.0 ) then
       FHGNEW = (1.0 - 15.0 * MoStabParaUndCan)**(-0.25)
    else
       FHGNEW = 1.0 + 4.7 * MoStabParaUndCan
    endif
    if ( IndIter == 1 ) then
       MoStabCorrShUndCan = FHGNEW
    else
       MoStabCorrShUndCan = 0.5 * (MoStabCorrShUndCan + FHGNEW)
    endif

    ! wind attenuation within canopy
    WindExtCoeffCanopy = (CanopyWindExtFac * VegAreaIndEff * CanopyHeight * MoStabCorrShUndCan)**0.5
    TMP1               = exp(-WindExtCoeffCanopy * RoughLenShVegGrd / CanopyHeight)
    TMP2               = exp(-WindExtCoeffCanopy * (RoughLenShCanopy + ZeroPlaneDispSfc) / CanopyHeight)
    TMPRAH2            = CanopyHeight * exp(WindExtCoeffCanopy) / WindExtCoeffCanopy * (TMP1-TMP2)

    ! aerodynamic resistances raw and rah between heights ZeroPlaneDisp+RoughLenShVegGrd and RoughLenShVegGrd.
    KH                  = max(ConstVonKarman*FrictionVelVeg*(CanopyHeight-ZeroPlaneDispSfc), MPE)
    ResistanceMomUndCan = 0.0
    ResistanceShUndCan  = TMPRAH2 / KH
    ResistanceLhUndCan  = ResistanceShUndCan

    ! leaf boundary layer resistance
    TMPRB                  = WindExtCoeffCanopy * 50.0 / (1.0 - exp(-WindExtCoeffCanopy/2.0))
    ResistanceLeafBoundary = TMPRB * sqrt(LeafDimLength / WindSpdCanopyTop)
    ResistanceLeafBoundary = min(max(ResistanceLeafBoundary, 5.0), 50.0)      ! limit ResistanceLeafBoundary to 5-50, typically <50

    end associate

  end subroutine ResistanceLeafToGround

end module ResistanceLeafToGroundMod
