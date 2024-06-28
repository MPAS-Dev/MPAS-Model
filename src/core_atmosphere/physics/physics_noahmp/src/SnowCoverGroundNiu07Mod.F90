module SnowCoverGroundNiu07Mod

!!! Compute ground snow cover fraction based on Niu and Yang (2007, JGR) scheme

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine SnowCoverGroundNiu07(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: None (embedded in ENERGY subroutine)
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

! local variable
    real(kind=kind_noahmp)           :: SnowDensBulk   ! bulk density of snow [Kg/m3]
    real(kind=kind_noahmp)           :: MeltFac        ! melting factor for snow cover frac

! --------------------------------------------------------------------
    associate(                                                     &
              SnowMeltFac    => noahmp%water%param%SnowMeltFac    ,& ! in,  snowmelt m parameter
              SnowCoverFac   => noahmp%water%param%SnowCoverFac   ,& ! in,  snow cover factor [m]
              SnowDepth      => noahmp%water%state%SnowDepth      ,& ! in,  snow depth [m]
              SnowWaterEquiv => noahmp%water%state%SnowWaterEquiv ,& ! in,  snow water equivalent [mm]
              SnowCoverFrac  => noahmp%water%state%SnowCoverFrac   & ! out, snow cover fraction
             )
! ----------------------------------------------------------------------

    SnowCoverFrac = 0.0
    if ( SnowDepth > 0.0 ) then
         SnowDensBulk  = SnowWaterEquiv / SnowDepth
         MeltFac       = (SnowDensBulk / 100.0)**SnowMeltFac
        !SnowCoverFrac = tanh( SnowDepth /(2.5 * Z0 * MeltFac))
         SnowCoverFrac = tanh( SnowDepth /(SnowCoverFac * MeltFac)) ! C.He: bring hard-coded 2.5*z0 to MPTABLE
    endif

    end associate

  end subroutine SnowCoverGroundNiu07

end module SnowCoverGroundNiu07Mod
