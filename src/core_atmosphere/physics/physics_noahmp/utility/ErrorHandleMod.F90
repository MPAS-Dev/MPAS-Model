module ErrorHandleMod

!!! define subroutines handling Noah-MP model errors

  use netcdf

  implicit none

contains

  subroutine ErrorHandle(status)

! ------------------------ Code history -----------------------------------
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

    integer, intent (in) :: status
 
    if(status /= nf90_noerr) then
       print *, trim( nf90_strerror(status) )
       stop "Stopped"
    endif

  end subroutine ErrorHandle

end module ErrorHandleMod
