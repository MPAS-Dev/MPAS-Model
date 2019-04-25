!==========================================================================
elemental function gsw_entropy_part_zerop (sa, pt0)
!==========================================================================
!
! entropy part evaluated at the sea surface
!
! sa     : Absolute Salinity                               [g/kg]
! pt0    : insitu temperature                              [deg C]
! 
! gsw_entropy_part_zerop : entropy part at the sea surface
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_sfac

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, pt0

real (r8) :: gsw_entropy_part_zerop

real (r8) :: x2, x, y, g03, g08

x2 = gsw_sfac*sa
x = sqrt(x2)
y = pt0*0.025_r8

g03 = y*(-24715.571866078_r8 + y*(2210.2236124548363_r8 + &
    y*(-592.743745734632_r8 + y*(290.12956292128547_r8 + &
    y*(-113.90630790850321_r8 + y*21.35571525415769_r8)))))

g08 = x2*(x*(x*(y*(-137.1145018408982_r8 + y*(148.10030845687618_r8 + &
    y*(-68.5590309679152_r8 + 12.4848504784754_r8*y)))) + &
    y*(-86.1329351956084_r8 + y*(-30.0682112585625_r8 + y*3.50240264723578_r8))) + &
    y*(1760.062705994408_r8 + y*(-675.802947790203_r8 + &
    y*(365.7041791005036_r8 + y*(-108.30162043765552_r8 + 12.78101825083098_r8*y)))))

gsw_entropy_part_zerop = -(g03 + g08)*0.025_r8

return
end function

!--------------------------------------------------------------------------
