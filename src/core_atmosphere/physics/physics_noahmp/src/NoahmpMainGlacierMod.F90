module NoahmpMainGlacierMod

!!! Main NoahMP glacier module including all glacier processes
!!! atmos forcing -> precip heat advect -> main energy -> main water -> balance check

  use Machine
  use NoahmpVarType
  use ConstantDefineMod
  use AtmosForcingMod,                   only : ProcessAtmosForcing
  use GeneralInitGlacierMod,             only : GeneralInitGlacier
  use PrecipitationHeatAdvectGlacierMod, only : PrecipitationHeatAdvectGlacier
  use EnergyMainGlacierMod,              only : EnergyMainGlacier
  use WaterMainGlacierMod,               only : WaterMainGlacier
  use BalanceErrorCheckGlacierMod,       only : BalanceWaterInitGlacier, &
                                                BalanceWaterCheckGlacier, BalanceEnergyCheckGlacier 
 
  implicit none

contains

  subroutine NoahmpMainGlacier(noahmp)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: NOAHMP_SFLX
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    implicit none

    type(noahmp_type), intent(inout) :: noahmp

    !---------------------------------------------------------------------
    ! Atmospheric forcing processing
    !--------------------------------------------------------------------- 

    call ProcessAtmosForcing(noahmp)

    !---------------------------------------------------------------------
    ! General initialization to prepare key variables
    !--------------------------------------------------------------------- 

    call GeneralInitGlacier(noahmp)

    !---------------------------------------------------------------------
    ! Prepare for water balance check
    !--------------------------------------------------------------------- 

    call BalanceWaterInitGlacier(noahmp)

    !---------------------------------------------------------------------
    ! Energy processes
    !--------------------------------------------------------------------- 

    call PrecipitationHeatAdvectGlacier(noahmp)
    call EnergyMainGlacier(noahmp)

    !---------------------------------------------------------------------
    ! Water processes
    !--------------------------------------------------------------------- 

    call WaterMainGlacier(noahmp)

    !---------------------------------------------------------------------
    ! Error check for energy and water balance
    !--------------------------------------------------------------------- 

    call BalanceWaterCheckGlacier(noahmp)
    call BalanceEnergyCheckGlacier(noahmp) 

    !---------------------------------------------------------------------
    ! End of all NoahMP glacier processes
    !--------------------------------------------------------------------- 

  end subroutine NoahmpMainGlacier

end module NoahmpMainGlacierMod
