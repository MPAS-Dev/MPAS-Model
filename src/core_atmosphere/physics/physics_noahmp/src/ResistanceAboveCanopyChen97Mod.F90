module ResistanceAboveCanopyChen97Mod

!!! Compute surface resistance and exchange coefficient for momentum and heat
!!! based on Chen et al. (1997, BLM)
!!! This scheme can handle both over open water and over solid surface

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine ResistanceAboveCanopyChen97(noahmp, IterationInd)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: SFCDIF2 for vegetated portion
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    integer               , intent(in   ) :: IterationInd     ! iteration index
    type(noahmp_type)     , intent(inout) :: noahmp

! local variables
    integer                               :: ILECH, ITR
    real(kind=kind_noahmp)                :: ZZ, PSLMU, PSLMS, PSLHU, PSLHS
    real(kind=kind_noahmp)                :: XX, PSPMU, YY, PSPMS, PSPHU, PSPHS
    real(kind=kind_noahmp)                :: ZILFC, ZU, ZT, RDZ, CXCH, DTHV, DU2
    real(kind=kind_noahmp)                :: BTGH, ZSLU, ZSLT, RLOGU, RLOGT, RLMA
    real(kind=kind_noahmp)                :: ZETALT, ZETALU, ZETAU, ZETAT, XLU4
    real(kind=kind_noahmp)                :: XLT4, XU4, XT4, XLU, XLT, XU, XT
    real(kind=kind_noahmp)                :: PSMZ, SIMM, PSHZ, SIMH, USTARK, RLMN
! local parameters
    integer               , parameter     :: ITRMX  = 5
    real(kind=kind_noahmp), parameter     :: WWST   = 1.2
    real(kind=kind_noahmp), parameter     :: WWST2  = WWST * WWST
    real(kind=kind_noahmp), parameter     :: VKRM   = 0.40
    real(kind=kind_noahmp), parameter     :: EXCM   = 0.001
    real(kind=kind_noahmp), parameter     :: BETA   = 1.0 / 270.0
    real(kind=kind_noahmp), parameter     :: BTG    = BETA * ConstGravityAcc
    real(kind=kind_noahmp), parameter     :: ELFC   = VKRM * BTG
    real(kind=kind_noahmp), parameter     :: WOLD   = 0.15
    real(kind=kind_noahmp), parameter     :: WNEW   = 1.0 - WOLD
    real(kind=kind_noahmp), parameter     :: PIHF   = 3.14159265 / 2.0
    real(kind=kind_noahmp), parameter     :: EPSU2  = 1.0e-4
    real(kind=kind_noahmp), parameter     :: EPSUST = 0.07
    real(kind=kind_noahmp), parameter     :: EPSIT  = 1.0e-4
    real(kind=kind_noahmp), parameter     :: EPSA   = 1.0e-8
    real(kind=kind_noahmp), parameter     :: ZTMIN  = -5.0
    real(kind=kind_noahmp), parameter     :: ZTMAX  = 1.0
    real(kind=kind_noahmp), parameter     :: HPBL   = 1000.0
    real(kind=kind_noahmp), parameter     :: SQVISC = 258.2
    real(kind=kind_noahmp), parameter     :: RIC    = 0.183
    real(kind=kind_noahmp), parameter     :: RRIC   = 1.0 / RIC
    real(kind=kind_noahmp), parameter     :: FHNEU  = 0.8
    real(kind=kind_noahmp), parameter     :: RFC    = 0.191
    real(kind=kind_noahmp), parameter     :: RFAC   = RIC / ( FHNEU * RFC * RFC )
! local statement functions
    ! LECH'S surface functions
    PSLMU(ZZ) = -0.96 * log(1.0 - 4.5 * ZZ)
    PSLMS(ZZ) = ZZ * RRIC - 2.076 * (1.0 - 1.0/(ZZ + 1.0))
    PSLHU(ZZ) = -0.96 * log(1.0 - 4.5 * ZZ)
    PSLHS(ZZ) = ZZ * RFAC - 2.076 * (1.0 - 1.0/(ZZ + 1.0))
    ! PAULSON'S surface functions
    PSPMU(XX) = -2.0*log( (XX+1.0)*0.5 ) - log( (XX*XX+1.0)*0.5 ) + 2.0*atan(XX) - PIHF
    PSPMS(YY) = 5.0 * YY
    PSPHU(XX) = -2.0 * log( (XX*XX + 1.0)*0.5 )
    PSPHS(YY) = 5.0 * YY

! --------------------------------------------------------------------
    associate(                                                                        &
              ZilitinkevichCoeff      => noahmp%energy%param%ZilitinkevichCoeff      ,& ! in,    Calculate roughness length of heat
              RefHeightAboveGrd       => noahmp%energy%state%RefHeightAboveGrd       ,& ! in,    reference height [m] above ground
              TemperaturePotRefHeight => noahmp%energy%state%TemperaturePotRefHeight ,& ! in,    potential temp at reference height [K]
              WindSpdRefHeight        => noahmp%energy%state%WindSpdRefHeight        ,& ! in,    wind speed [m/s] at reference height
              RoughLenMomSfc          => noahmp%energy%state%RoughLenMomSfc          ,& ! in,    roughness length [m], momentum, surface
              TemperatureCanopyAir    => noahmp%energy%state%TemperatureCanopyAir    ,& ! in,    canopy air temperature [K]
              ExchCoeffMomAbvCan      => noahmp%energy%state%ExchCoeffMomAbvCan      ,& ! inout, exchange coeff [m/s] for momentum, above ZeroPlaneDisp, vegetated
              ExchCoeffShAbvCan       => noahmp%energy%state%ExchCoeffShAbvCan       ,& ! inout, exchange coeff [m/s] for heat, above ZeroPlaneDisp, vegetated
              MoStabParaAbvCan        => noahmp%energy%state%MoStabParaAbvCan        ,& ! inout, Monin-Obukhov stability (z/L), above ZeroPlaneDisp, vegetated
              FrictionVelVertVeg      => noahmp%energy%state%FrictionVelVertVeg      ,& ! inout, friction velocity [m/s] in vertical direction, vegetated
              FrictionVelVeg          => noahmp%energy%state%FrictionVelVeg          ,& ! inout, friction velocity [m/s], vegetated
              ResistanceMomAbvCan     => noahmp%energy%state%ResistanceMomAbvCan     ,& ! out,   aerodynamic resistance for momentum [s/m], above canopy
              ResistanceShAbvCan      => noahmp%energy%state%ResistanceShAbvCan      ,& ! out,   aerodynamic resistance for sensible heat [s/m], above canopy
              ResistanceLhAbvCan      => noahmp%energy%state%ResistanceLhAbvCan       & ! out,   aerodynamic resistance for water vapor [s/m], above canopy
             )
! ----------------------------------------------------------------------

    ! ZTFC: RATIO OF ZOH/ZOM  LESS OR EQUAL THAN 1
    ! C......ZTFC=0.1
    ! ZilitinkevichCoeff: CONSTANT C IN Zilitinkevich, S. S.1995,:NOTE ABOUT ZT
    ILECH = 0
    ZILFC = -ZilitinkevichCoeff * VKRM * SQVISC
    ZU    = RoughLenMomSfc
    RDZ   = 1.0 / RefHeightAboveGrd
    CXCH  = EXCM * RDZ
    DTHV  = TemperaturePotRefHeight - TemperatureCanopyAir

    ! BELJARS correction of friction velocity u*
    DU2   = max(WindSpdRefHeight*WindSpdRefHeight, EPSU2)
    BTGH  = BTG * HPBL
    if ( IterationInd == 1 ) then
       if ( (BTGH*ExchCoeffShAbvCan*DTHV) /= 0.0 ) then
          FrictionVelVertVeg = WWST2 * abs(BTGH*ExchCoeffShAbvCan*DTHV)**(2.0/3.0)
       else
          FrictionVelVertVeg = 0.0
       endif
       FrictionVelVeg        = max(sqrt(ExchCoeffMomAbvCan*sqrt(DU2+FrictionVelVertVeg)), EPSUST)
       MoStabParaAbvCan      = ELFC * ExchCoeffShAbvCan * DTHV / FrictionVelVeg**3
    endif

    ! ZILITINKEVITCH approach for ZT
    ZT    = max(1.0e-6, exp(ZILFC*sqrt(FrictionVelVeg*RoughLenMomSfc))*RoughLenMomSfc)
    ZSLU  = RefHeightAboveGrd + ZU
    ZSLT  = RefHeightAboveGrd + ZT
    RLOGU = log(ZSLU/ZU)
    RLOGT = log(ZSLT/ZT)

    ! Monin-Obukhov length scale
    ZETALT           = max(ZSLT*MoStabParaAbvCan, ZTMIN)
    MoStabParaAbvCan = ZETALT / ZSLT
    ZETALU           = ZSLU * MoStabParaAbvCan
    ZETAU            = ZU * MoStabParaAbvCan
    ZETAT            = ZT * MoStabParaAbvCan
    if ( ILECH == 0 ) then
       if ( MoStabParaAbvCan < 0.0 ) then
          XLU4 = 1.0 - 16.0 * ZETALU
          XLT4 = 1.0 - 16.0 * ZETALT
          XU4  = 1.0 - 16.0 * ZETAU
          XT4  = 1.0 - 16.0 * ZETAT
          XLU  = sqrt(sqrt(XLU4))
          XLT  = sqrt(sqrt(XLT4))
          XU   = sqrt(sqrt(XU4))
          XT   = sqrt(sqrt(XT4))
          PSMZ = PSPMU(XU)
          SIMM = PSPMU(XLU) - PSMZ + RLOGU
          PSHZ = PSPHU(XT)
          SIMH = PSPHU(XLT) - PSHZ + RLOGT
       else
          ZETALU = min(ZETALU, ZTMAX)
          ZETALT = min(ZETALT, ZTMAX)
          ZETAU  = min(ZETAU, ZTMAX/(ZSLU/ZU))   ! Barlage: add limit on ZETAU/ZETAT
          ZETAT  = min(ZETAT, ZTMAX/(ZSLT/ZT))   ! Barlage: prevent SIMM/SIMH < 0
          PSMZ   = PSPMS(ZETAU)
          SIMM   = PSPMS(ZETALU) - PSMZ + RLOGU
          PSHZ   = PSPHS(ZETAT)
          SIMH   = PSPHS(ZETALT) - PSHZ + RLOGT
       endif
    else ! LECH's functions
       if ( MoStabParaAbvCan < 0.0 ) then
          PSMZ = PSLMU(ZETAU)
          SIMM = PSLMU(ZETALU) - PSMZ + RLOGU
          PSHZ = PSLHU(ZETAT)
          SIMH = PSLHU(ZETALT) - PSHZ + RLOGT
       else
          ZETALU = min(ZETALU, ZTMAX)
          ZETALT = min(ZETALT, ZTMAX)
          PSMZ   = PSLMS(ZETAU)
          SIMM   = PSLMS(ZETALU) - PSMZ + RLOGU
          PSHZ   = PSLHS(ZETAT)
          SIMH   = PSLHS(ZETALT) - PSHZ + RLOGT
       endif
    endif

    ! BELJARS correction of friction velocity u*
    FrictionVelVeg = max(sqrt(ExchCoeffMomAbvCan*sqrt(DU2+FrictionVelVertVeg)), EPSUST)

    ! ZILITINKEVITCH fix for ZT
    ZT     = max(1.0e-6, exp(ZILFC*sqrt(FrictionVelVeg*RoughLenMomSfc))*RoughLenMomSfc)
    ZSLT   = RefHeightAboveGrd + ZT
    RLOGT  = log(ZSLT/ZT)
    USTARK = FrictionVelVeg * VKRM

    ! avoid tangent linear problems near zero
    if ( SIMM < 1.0e-6 ) SIMM = 1.0e-6   ! Limit stability function
    ExchCoeffMomAbvCan = max(USTARK/SIMM, CXCH)
    if ( SIMH < 1.0e-6 ) SIMH = 1.0e-6   ! Limit stability function
    ExchCoeffShAbvCan  = max(USTARK/SIMH, CXCH)

    ! update vertical friction velocity w*
    if ( (BTGH*ExchCoeffShAbvCan*DTHV) /= 0.0 ) then
       FrictionVelVertVeg = WWST2 * abs(BTGH*ExchCoeffShAbvCan*DTHV)**(2.0/3.0)
    else
       FrictionVelVertVeg = 0.0
    endif

    ! update M-O stability parameter
    RLMN = ELFC * ExchCoeffShAbvCan * DTHV / FrictionVelVeg**3
    RLMA = MoStabParaAbvCan * WOLD + RLMN * WNEW
    MoStabParaAbvCan = RLMA

    ! Undo the multiplication by windspeed that applies to exchange coeff
    ExchCoeffShAbvCan   = ExchCoeffShAbvCan / WindSpdRefHeight
    ExchCoeffMomAbvCan  = ExchCoeffMomAbvCan / WindSpdRefHeight

    ! compute aerodynamic resistance
    ResistanceMomAbvCan = max(1.0, 1.0/(ExchCoeffMomAbvCan*WindSpdRefHeight))
    ResistanceShAbvCan  = max(1.0, 1.0/(ExchCoeffShAbvCan*WindSpdRefHeight))
    ResistanceLhAbvCan  = ResistanceShAbvCan

    end associate

  end subroutine ResistanceAboveCanopyChen97

end module ResistanceAboveCanopyChen97Mod
