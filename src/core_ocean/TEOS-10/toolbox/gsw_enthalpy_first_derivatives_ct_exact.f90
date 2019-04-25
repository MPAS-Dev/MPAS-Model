!==========================================================================
elemental subroutine gsw_enthalpy_first_derivatives_ct_exact (sa, ct, p, &
                                                              h_sa, h_ct)
!==========================================================================
!
!  Calculates the following two derivatives of specific enthalpy (h)
!   (1) h_SA, the derivative with respect to Absolute Salinity at 
!       constant CT and p, and
!   (2) h_CT, derivative with respect to CT at constant SA and p. 
!  Note that h_P is specific volume (1/rho) it can be calulated by calling
!  gsw_specvol_CT_exact(SA,CT,p). This function uses the full Gibbs function.
!
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  CT  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  h_SA  =  The first derivative of specific enthalpy with respect to 
!           Absolute Salinity at constant CT and p.     
!                                            [ J/(kg (g/kg))]  i.e. [ J/g ]
!  h_CT  =  The first derivative of specific enthalpy with respect to 
!           CT at constant SA and p.                           [ J/(kg K) ]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_pt_from_ct, gsw_t_from_ct

use gsw_mod_teos10_constants, only : gsw_cp0, gsw_sfac, gsw_t0, rec_db2pa

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p
real (r8), intent(out), optional :: h_sa, h_ct

real (r8) :: g_sa_mod_pt, g_sa_mod_t, pt0, t, temp_ratio
real (r8) :: x, y, y_pt, z

t = gsw_t_from_ct(sa,ct,p)
pt0 = gsw_pt_from_ct(sa,ct)  

temp_ratio = (gsw_t0 + t)/(gsw_t0 + pt0)

if (present(h_ct)) h_ct = gsw_cp0*temp_ratio

if (.not. present(h_sa)) return

x = sqrt(gsw_sfac*sa)
y = 0.025_r8*t
z = rec_db2pa*p !note.the input pressure (p) is sea pressure in units of dbar.

g_sa_mod_t = 8645.36753595126_r8 + z*(-6620.98308089678_r8 + &
       z*(769.588305957198_r8 + z*(-193.0648640214916_r8 + (31.6816345533648_r8 - 5.24960313181984_r8*z)*z))) + &
       x*(-7296.43987145382_r8 + x*(8103.20462414788_r8 + &
       y*(2175.341332000392_r8 + y*(-274.2290036817964_r8 + &
       y*(197.4670779425016_r8 + y*(-68.5590309679152_r8 + 9.98788038278032_r8*y))) - 90.6734234051316_r8*z) + &
       x*(-5458.34205214835_r8 - 980.14153344888_r8*y + &
       x*(2247.60742726704_r8 - 340.1237483177863_r8*x + 220.542973797483_r8*y) + 180.142097805543_r8*z) + &
       z*(-219.1676534131548_r8 + (-16.32775915649044_r8 - 120.7020447884644_r8*z)*z)) + &
       z*(598.378809221703_r8 + z*(-156.8822727844005_r8 + (204.1334828179377_r8 - 10.23755797323846_r8*z)*z)) + &
       y*(-1480.222530425046_r8 + z*(-525.876123559641_r8 + (249.57717834054571_r8 - 88.449193048287_r8*z)*z) + &
       y*(-129.1994027934126_r8 + z*(1149.174198007428_r8 + z*(-162.5751787551336_r8 + 76.9195462169742_r8*z)) + &
       y*(-30.0682112585625_r8 - 1380.9597954037708_r8*z + y*(2.626801985426835_r8 + 703.695562834065_r8*z))))) + &
       y*(1187.3715515697959_r8 + z*(1458.233059470092_r8 + &
       z*(-687.913805923122_r8 + z*(249.375342232496_r8 + z*(-63.313928772146_r8 + 14.09317606630898_r8*z)))) + &
       y*(1760.062705994408_r8 + y*(-450.535298526802_r8 + &
       y*(182.8520895502518_r8 + y*(-43.3206481750622_r8 + 4.26033941694366_r8*y) + &
       z*(-595.457483974374_r8 + (149.452282277512_r8 - 72.9745838003176_r8*z)*z)) + &
       z*(1388.489628266536_r8 + z*(-409.779283929806_r8 + (227.123395681188_r8 - 22.2565468652826_r8*z)*z))) + &
       z*(-1721.528607567954_r8 + z*(674.819060538734_r8 + &
       z*(-356.629112415276_r8 + (88.4080716616_r8 - 15.84003094423364_r8*z)*z)))))
  
g_sa_mod_t = 0.5_r8*gsw_sfac*g_sa_mod_t   
    
y_pt = 0.025_r8*pt0

g_sa_mod_pt = 8645.36753595126_r8 + &
        x*(-7296.43987145382_r8 + x*(8103.20462414788_r8 + &
        y_pt*(2175.341332000392_r8 + y_pt*(-274.2290036817964_r8 + &
        y_pt*(197.4670779425016_r8 + y_pt*(-68.5590309679152_r8 + 9.98788038278032_r8*y_pt)))) + &
        x*(-5458.34205214835_r8 - 980.14153344888_r8*y_pt + &
        x*(2247.60742726704_r8 - 340.1237483177863_r8*x + 220.542973797483_r8*y_pt))) + &
        y_pt*(-1480.222530425046_r8 + y_pt*(-129.1994027934126_r8 + &
        y_pt*(-30.0682112585625_r8 + y_pt*2.626801985426835_r8)))) + &
        y_pt*(1187.3715515697959_r8 + y_pt*(1760.062705994408_r8 + y_pt*(-450.535298526802_r8 + &
        y_pt*(182.8520895502518_r8 + y_pt*(-43.3206481750622_r8 + 4.26033941694366_r8*y_pt)))))
    
g_sa_mod_pt = 0.5_r8*gsw_sfac*g_sa_mod_pt   

h_sa = g_sa_mod_t - temp_ratio*g_sa_mod_pt

return
end subroutine

!--------------------------------------------------------------------------
