!==========================================================================
elemental function gsw_ct_maxdensity (sa, p)
!==========================================================================
!
!  Calculates the Conservative Temperature of maximum density of seawater. 
!  This function returns the Conservative temperature at which the density
!  of seawater is a maximum, at given Absolute Salinity, SA, and sea 
!  pressure, p (in dbar).
!
!  SA =  Absolute Salinity                                         [ g/kg ]
!  p  =  sea pressure                                              [ dbar ]
!        ( i.e. absolute pressure - 10.1325 dbar ) 
!
!  CT_maxdensity  =  Conservative Temperature at which            [ deg C ]
!                    the density of seawater is a maximum for
!                    given Absolute Salinity and pressure.
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_alpha

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, p

real (r8) :: gsw_ct_maxdensity

integer :: number_of_iterations
real (r8) :: alpha, ct, ct_mean, ct_old, dalpha_dct

real (r8), parameter :: dct = 0.001_r8

ct = 3.978_r8 - 0.22072_r8*sa         ! the initial guess of ct.

dalpha_dct = 1.1e-5_r8                ! the initial guess for dalpha_dct.

do number_of_iterations = 1, 3
    ct_old = ct
    alpha = gsw_alpha(sa,ct_old,p)
    ct = ct_old - alpha/dalpha_dct
    ct_mean = 0.5_r8*(ct + ct_old)
    dalpha_dct = (gsw_alpha(sa,ct_mean+dct,p) &
                  - gsw_alpha(sa,ct_mean-dct,p))/(dct + dct)
    ct = ct_old - alpha/dalpha_dct
end do

! After three iterations of this modified Newton-Raphson (McDougall and 
! Wotherspoon, 2012) iteration, the error in CT_maxdensity is typically no
! larger than 1x10^-15 degress C.  

gsw_ct_maxdensity = ct

return
end function

!--------------------------------------------------------------------------
