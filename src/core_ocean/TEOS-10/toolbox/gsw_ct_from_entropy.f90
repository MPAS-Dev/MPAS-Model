!=========================================================================
elemental function gsw_ct_from_entropy (sa, entropy)
!=========================================================================
!
!  Calculates Conservative Temperature with entropy as an input variable.  
!
!  SA       =  Absolute Salinity                                   [ g/kg ]
!  entropy  =  specific entropy                                   [ deg C ]
!
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_ct_from_pt, gsw_pt_from_entropy

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, entropy

real (r8) :: gsw_ct_from_entropy

real (r8) :: pt

pt = gsw_pt_from_entropy(sa,entropy)
gsw_ct_from_entropy = gsw_ct_from_pt(sa,pt)

return
end function

!--------------------------------------------------------------------------
