!==========================================================================
elemental subroutine gsw_ct_first_derivatives (sa, pt, ct_sa, ct_pt)
!==========================================================================
!
!  Calculates the following two derivatives of Conservative Temperature
!  (1) CT_SA, the derivative with respect to Absolute Salinity at 
!      constant potential temperature (with pr = 0 dbar), and
!   2) CT_pt, the derivative with respect to potential temperature
!      (the regular potential temperature which is referenced to 0 dbar)
!      at constant Absolute Salinity.
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  pt  =  potential temperature (ITS-90)                          [ deg C ]   
!         (whose reference pressure is 0 dbar)
!
!  CT_SA  =  The derivative of Conservative Temperature with respect to 
!            Absolute Salinity at constant potential temperature 
!            (the regular potential temperature which has reference 
!            sea pressure of 0 dbar).    
!            The CT_SA output has units of:                     [ K/(g/kg)]
!  CT_pt  =  The derivative of Conservative Temperature with respect to 
!            potential temperature (the regular one with pr = 0 dbar)
!            at constant SA. CT_pt is dimensionless.           [ unitless ]
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_cp0, gsw_sfac, gsw_t0

use gsw_mod_toolbox, only : gsw_gibbs_pt0_pt0

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, pt
real (r8), intent(out), optional :: ct_sa, ct_pt

real (r8) :: abs_pt, g_sa_mod, g_sa_t_mod, x, y_pt

abs_pt = gsw_t0 + pt 

if (present(ct_pt)) ct_pt = -(abs_pt*gsw_gibbs_pt0_pt0(sa,pt))/gsw_cp0

if (.not. present(ct_sa)) return

!--------------------------------------------------------------------------

x = sqrt(gsw_sfac*sa)
y_pt = 0.025_r8*pt

g_sa_t_mod = 1187.3715515697959_r8 + &
        x*(-1480.222530425046_r8 + x*(2175.341332000392_r8 + x*(-980.14153344888_r8 + 220.542973797483_r8*x) + &
        y_pt*(-548.4580073635929_r8 + y_pt*(592.4012338275047_r8 + y_pt*(-274.2361238716608_r8 + 49.9394019139016_r8*y_pt)))) + &
        y_pt*(-258.3988055868252_r8 + y_pt*(-90.2046337756875_r8 + y_pt*10.50720794170734_r8))) + &
        y_pt*(3520.125411988816_r8  + y_pt*(-1351.605895580406_r8 + &
        y_pt*(731.4083582010072_r8  + y_pt*(-216.60324087531103_r8 + 25.56203650166196_r8*y_pt))))
g_sa_t_mod = 0.5_r8*gsw_sfac*0.025_r8*g_sa_t_mod
   
g_sa_mod = 8645.36753595126_r8 + &
        x*(-7296.43987145382_r8 + x*(8103.20462414788_r8 + &
        y_pt*(2175.341332000392_r8 + y_pt*(-274.2290036817964_r8 + &
        y_pt*(197.4670779425016_r8 + y_pt*(-68.5590309679152_r8 + 9.98788038278032_r8*y_pt)))) + &
        x*(-5458.34205214835_r8 - 980.14153344888_r8*y_pt + &
        x*(2247.60742726704_r8 - 340.1237483177863_r8*x + 220.542973797483_r8*y_pt))) + &
        y_pt*(-1480.222530425046_r8 + &
        y_pt*(-129.1994027934126_r8 + &
        y_pt*(-30.0682112585625_r8 + y_pt*(2.626801985426835_r8 ))))) + &
        y_pt*(1187.3715515697959_r8 + &
        y_pt*(1760.062705994408_r8 + y_pt*(-450.535298526802_r8 + &
        y_pt*(182.8520895502518_r8 + y_pt*(-43.3206481750622_r8 + 4.26033941694366_r8*y_pt)))))
g_sa_mod = 0.5_r8*gsw_sfac*g_sa_mod   

ct_sa = (g_sa_mod - abs_pt*g_sa_t_mod)/gsw_cp0

return
end subroutine

!--------------------------------------------------------------------------
