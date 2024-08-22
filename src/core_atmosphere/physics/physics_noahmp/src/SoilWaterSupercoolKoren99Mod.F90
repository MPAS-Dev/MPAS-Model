module SoilWaterSupercoolKoren99Mod

!!! Calculate amount of supercooled liquid soil water content if soil temperature < freezing point
!!! This uses Newton-type iteration to solve the nonlinear implicit equation 
!!! Reference: Eqn.17 in Koren et al. 1999 JGR VOL 104(D16), 19569-19585
!!! New version (June 2001): much faster and more accurate Newton iteration achieved by first
!!! taking log of Eqn above -- less than 4 (typically 1 or 2) iterations achieves convergence.
!!! Explicit 1-step solution option for special case of parameter CK=0, which reduces the
!!! original implicit equation to a simpler explicit form, known as "Flerchinger Eqn". Improved
!!! handling of solution in the limit of freezing point temperature.

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SoilWaterSupercoolKoren99(noahmp, IndSoil, SoilWatSupercool, &
                                       SoilTemperature, SoilMoisture, SoilLiqWater)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: FRH2O
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variables
    type(noahmp_type)     , intent(inout) :: noahmp
    integer               , intent(in   ) :: IndSoil              ! soil layer index
    real(kind=kind_noahmp), intent(in   ) :: SoilLiqWater         ! soil liquid water content [m3/m3]
    real(kind=kind_noahmp), intent(in   ) :: SoilMoisture         ! total soil moisture content [m3/m3]
    real(kind=kind_noahmp), intent(in   ) :: SoilTemperature      ! soil temperature [K]
    real(kind=kind_noahmp), intent(out  ) :: SoilWatSupercool     ! soil supercooled liquid water content [m3/m3]

! local variable
    integer                               :: NumIter              ! number of iteration
    integer                               :: IndCnt               ! counting index 
    real(kind=kind_noahmp)                :: SoilExpB             ! temporary soil B parameter
    real(kind=kind_noahmp)                :: Denom                ! temporary denominator variable
    real(kind=kind_noahmp)                :: DF                   ! temporary nominator variable
    real(kind=kind_noahmp)                :: SoilIceChg           ! soil ice content change
    real(kind=kind_noahmp)                :: FlerFac              ! factor in Flerchinger solution
    real(kind=kind_noahmp)                :: SoilIce              ! soil ice content
    real(kind=kind_noahmp)                :: SoilIceTmp           ! temporary soil ice content
    real(kind=kind_noahmp), parameter     :: CK          = 8.0    ! parameter
    real(kind=kind_noahmp), parameter     :: SoilExpBMax = 5.5    ! limit of B soil parameter
    real(kind=kind_noahmp), parameter     :: ErrorThr    = 0.005  ! error threshold

! --------------------------------------------------------------------
    associate(                                                               &
              SoilExpCoeffB       => noahmp%water%param%SoilExpCoeffB       ,& ! in, soil B parameter
              SoilMatPotentialSat => noahmp%water%param%SoilMatPotentialSat ,& ! in, saturated soil matric potential [m]
              SoilMoistureSat     => noahmp%water%param%SoilMoistureSat      & ! in, saturated value of soil moisture [m3/m3]
             )
! ----------------------------------------------------------------------

    ! limit on parameter B: B < 5.5  (use parameter SoilExpBMax)
    ! simulations showed if B > 5.5 unfrozen water content is
    ! non-realistically high at very low temperatures
    SoilExpB = SoilExpCoeffB(IndSoil)

    ! initializing iterations counter and interative solution flag
    if ( SoilExpCoeffB(IndSoil) > SoilExpBMax ) SoilExpB = SoilExpBMax
    NumIter = 0

    ! if soil temperature not largely below freezing point, SoilLiqWater = SoilMoisture
    IndCnt = 0
    if ( SoilTemperature > (ConstFreezePoint-1.0e-3) ) then
       SoilWatSupercool = SoilMoisture
    else  ! frozen soil case

       !--- Option 1: iterated solution in Koren et al. 1999 JGR Eqn.17
       ! initial guess for SoilIce (frozen content) 
       if ( CK /= 0.0 ) then
          SoilIce = SoilMoisture - SoilLiqWater
          if ( SoilIce > (SoilMoisture-0.02) ) SoilIce = SoilMoisture - 0.02   ! keep within bounds
          ! start the iterations
          if ( SoilIce < 0.0 ) SoilIce = 0.0
1001      Continue
          if ( .not. ((NumIter < 10) .and. (IndCnt == 0)) ) goto 1002
          NumIter    = NumIter +1
          DF         = alog((SoilMatPotentialSat(IndSoil)*ConstGravityAcc/ConstLatHeatFusion) * &
                       ((1.0 + CK*SoilIce)**2.0) * (SoilMoistureSat(IndSoil)/(SoilMoisture - SoilIce))**SoilExpB) - &
                       alog(-(SoilTemperature - ConstFreezePoint) / SoilTemperature)
          Denom      = 2.0 * CK / (1.0 + CK * SoilIce) + SoilExpB / (SoilMoisture - SoilIce)
          SoilIceTmp = SoilIce - DF / Denom
          ! bounds useful for mathematical solution
          if ( SoilIceTmp > (SoilMoisture-0.02) ) SoilIceTmp = SoilMoisture - 0.02
          if ( SoilIceTmp < 0.0 ) SoilIceTmp = 0.0
          SoilIceChg = abs(SoilIceTmp - SoilIce)    ! mathematical solution bounds applied
          ! if more than 10 iterations, use explicit method (CK=0 approx.)
          ! when SoilIceChg <= ErrorThr, no more interations required.
          SoilIce = SoilIceTmp
          if ( SoilIceChg <= ErrorThr ) then
             IndCnt = IndCnt +1
          endif
          ! end of iteration
          ! bounds applied within do-block are valid for physical solution 
          goto 1001
1002      continue
          SoilWatSupercool = SoilMoisture - SoilIce
       endif
       !--- End Option 1

       !--- Option 2: explicit solution for Flerchinger Eq. i.e., CK=0
       ! in Koren et al. 1999 JGR Eqn. 17
       ! apply physical bounds to Flerchinger solution
       if ( IndCnt == 0 ) then
          print*, 'Flerchinger used in NEW version. Iterations=', NumIter
          FlerFac = (((ConstLatHeatFusion / (ConstGravityAcc * (-SoilMatPotentialSat(IndSoil)))) * &
                    ((SoilTemperature-ConstFreezePoint) / SoilTemperature))**(-1.0/SoilExpB)) * SoilMoistureSat(IndSoil)
          if ( FlerFac < 0.02 ) FlerFac = 0.02
          SoilWatSupercool = min(FlerFac, SoilMoisture)
       endif
       !--- End Option 2

    endif

    end associate

  end subroutine SoilWaterSupercoolKoren99

end module SoilWaterSupercoolKoren99Mod
