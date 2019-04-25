!==========================================================================
elemental function gsw_ct_from_pt (sa, pt) 
!==========================================================================
!
! Calculates Conservative Temperature from potential temperature of seawater  
!
! sa      : Absolute Salinity                              [g/kg]
! pt      : potential temperature with                     [deg C]
!           reference pressure of 0 dbar
!
! gsw_ct_from_pt : Conservative Temperature                [deg C]
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_cp0, gsw_sfac

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, pt 

real (r8) :: gsw_ct_from_pt

real (r8) :: pot_enthalpy, x2, x, y

x2 = gsw_sfac*sa
x = sqrt(x2)
y = pt*0.025_r8        ! normalize for F03 and F08

pot_enthalpy =  61.01362420681071_r8 + y*(168776.46138048015_r8 + &
               y*(-2735.2785605119625_r8 + y*(2574.2164453821433_r8 + &
               y*(-1536.6644434977543_r8 + y*(545.7340497931629_r8 + &
               (-50.91091728474331_r8 - 18.30489878927802_r8*y)*y))))) + &
               x2*(268.5520265845071_r8 + y*(-12019.028203559312_r8 + &
               y*(3734.858026725145_r8 + y*(-2046.7671145057618_r8 + &
               y*(465.28655623826234_r8 + (-0.6370820302376359_r8 - &
               10.650848542359153_r8*y)*y)))) + &
               x*(937.2099110620707_r8 + y*(588.1802812170108_r8 + &
               y*(248.39476522971285_r8 + (-3.871557904936333_r8 - &
               2.6268019854268356_r8*y)*y)) + &
               x*(-1687.914374187449_r8 + x*(246.9598888781377_r8 + &
               x*(123.59576582457964_r8 - 48.5891069025409_r8*x)) + &
               y*(936.3206544460336_r8 + &
               y*(-942.7827304544439_r8 + y*(369.4389437509002_r8 + &
               (-33.83664947895248_r8 - 9.987880382780322_r8*y)*y))))))

gsw_ct_from_pt = pot_enthalpy/gsw_cp0

return
end function

!--------------------------------------------------------------------------
