!==========================================================================
elemental function gsw_dilution_coefficient_t_exact (sa, t, p)
!==========================================================================
!
!  Calculates the dilution coefficient of seawater.  The dilution 
!  coefficient of seawater is defined as the Absolute Salinity times the 
!  second derivative of the Gibbs function with respect to Absolute 
!  Salinity, that is, SA.*g_SA_SA.
!
!  SA =  Absolute Salinity                                         [ g/kg ]
!  t  =  in-situ temperature (ITS-90)                             [ deg C ]
!  p  =  sea pressure                                              [ dbar ]
!        ( i.e. absolute pressure - 10.1325 dbar ) 
!
!  dilution_coefficient_t_exact  =  dilution coefficient   [ (J/kg)(kg/g) ]
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_sfac

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, t, p

real (r8) :: gsw_dilution_coefficient_t_exact

real (r8) :: g08, x, x2, y, z

x2 = gsw_sfac*sa
x = sqrt(x2)
y = t*0.025_r8
z = p*1e-4_r8      ! note.the input pressure (p) is sea pressure in units of dbar.

g08 = 2.0_r8*(8103.20462414788_r8 + &
          y*(2175.341332000392_r8 + &
              y*(-274.2290036817964_r8 + &
                  y*(197.4670779425016_r8 + &
                      y*(-68.5590309679152_r8 + 9.98788038278032_r8*y))) - &
          90.6734234051316_r8*z) + &
              1.5_r8*x*(-5458.34205214835_r8 - 980.14153344888_r8*y + &
                  (4.0_r8/3.0_r8)*x*(2247.60742726704_r8 - &
                  340.1237483177863_r8*1.25_r8*x + 220.542973797483_r8*y) + &
              180.142097805543_r8*z) + &
          z*(-219.1676534131548_r8 + &
              (-16.32775915649044_r8 - 120.7020447884644_r8*z)*z))
    
g08 = x2*g08 + & 
          x*(-7296.43987145382_r8 + &
              z*(598.378809221703_r8 + &
                  z*(-156.8822727844005_r8 + &
                      (204.1334828179377_r8 - 10.23755797323846_r8*z)*z)) + &
              y*(-1480.222530425046_r8 + &
                  z*(-525.876123559641_r8 + &
                      (249.57717834054571_r8 - 88.449193048287_r8*z)*z) + &
                  y*(-129.1994027934126_r8 + &
                      z*(1149.174198007428_r8 + &
                          z*(-162.5751787551336_r8 + 76.9195462169742_r8*z)) + &
                  y*(-30.0682112585625_r8 - 1380.9597954037708_r8*z + &
                      y*(2.626801985426835_r8 + 703.695562834065_r8*z))))) + &
      11625.62913253464_r8 + 1702.453469893412_r8*y
    
gsw_dilution_coefficient_t_exact = 0.25_r8*gsw_sfac*g08

! Note that this function avoids the singularity that occurs at SA = 0 if
! the straightforward expression for the dilution coefficient of seawater,
! SA*g_SA_SA is simply evaluated as SA.*gsw_gibbs(2,0,0,SA,t,p). 

return
end function

!--------------------------------------------------------------------------

