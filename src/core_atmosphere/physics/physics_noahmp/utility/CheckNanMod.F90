module CheckNanMod

!!! Check NaN values

  use Machine, only : kind_noahmp

  implicit none

contains

  subroutine CheckRealNaN(NumIn, OutVal)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: embedded in NOAHMP_SFLX
! Original code: P. Valayamkunnath (2021)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    real(kind=kind_noahmp), intent(in)  :: NumIn
    logical               , intent(out) :: OutVal
 
    OutVal = (NumIn /= NumIn)

  end subroutine CheckRealNaN

end module CheckNanMod
