!==========================================================================
elemental subroutine gsw_ct_first_derivatives_wrt_t_exact (sa, t, p, &
                                       ct_sa_wrt_t, ct_t_wrt_t, ct_p_wrt_t)
!==========================================================================
!
!  Calculates the following three derivatives of Conservative Temperature.
!  These derivatives are done with respect to in-situ temperature t (in the
!  case of CT_T_wrt_t) or at constant in-situ tempertature (in the cases of
!  CT_SA_wrt_t and CT_P_wrt_t).  
!   (1) CT_SA_wrt_t, the derivative of CT with respect to Absolute Salinity 
!       at constant t and p, and
!   (2) CT_T_wrt_t, derivative of CT with respect to in-situ temperature t 
!       at constant SA and p. 
!   (3) CT_P_wrt_t, derivative of CT with respect to pressure P (in Pa) at  
!       constant SA and t.    
!
!  This function uses the full Gibbs function. Note that this function
!  avoids the NaN that would exist in CT_SA_wrt_t at SA = 0 if it were
!  evaluated in the straightforward way from the derivatives of the Gibbs 
!  function function.
!   
!  SA  =  Absolute Salinity                                        [ g/kg ]
!  t   =  in-situ temperature (ITS-90)                            [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar)
!
!  CT_SA_wrt_t  =  The first derivative of Conservative Temperature with 
!                  respect to Absolute Salinity at constant t and p.     
!                                              [ K/(g/kg)]  i.e. [ K kg/g ]
!  CT_T_wrt_t  =  The first derivative of Conservative Temperature with 
!                 respect to in-situ temperature, t, at constant SA and p.     
!                                                              [ unitless ]
!  CT_P_wrt_t  =  The first derivative of Conservative Temperature with 
!                 respect to pressure P (in Pa) at constant SA and t. 
!                                                                  [ K/Pa ]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_gibbs, gsw_pt0_from_t

use gsw_mod_teos10_constants, only : gsw_cp0, gsw_sfac, gsw_t0, rec_db2pa

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, t, p
real (r8), intent(out), optional :: ct_p_wrt_t, ct_sa_wrt_t, ct_t_wrt_t

real (r8) :: g_sa_mod, g_sa_t_mod, pt0, x, y, y_pt, z

pt0 = gsw_pt0_from_t(sa,t,p)

if (present(ct_sa_wrt_t)) then

    x = sqrt(gsw_sfac*sa)
    y = 0.025_r8*t
    y_pt = 0.025_r8*pt0
    z = rec_db2pa*p   ! the input pressure (p) is sea pressure in units of dbar

    g_sa_t_mod = 1187.3715515697959_r8 + z*(1458.233059470092_r8 + &
            z*(-687.913805923122_r8 + z*(249.375342232496_r8 + &
            z*(-63.313928772146_r8 + 14.09317606630898_r8*z)))) + &
            x*(-1480.222530425046_r8 + x*(2175.341332000392_r8 + &
            x*(-980.14153344888_r8 + 220.542973797483_r8*x) + &
            y*(-548.4580073635929_r8 + y*(592.4012338275047_r8 + &
            y*(-274.2361238716608_r8 + 49.9394019139016_r8*y))) - &
            90.6734234051316_r8*z) + z*(-525.876123559641_r8 + &
            (249.57717834054571_r8 - 88.449193048287_r8*z)*z) + &
            y*(-258.3988055868252_r8 + z*(2298.348396014856_r8 + &
            z*(-325.1503575102672_r8 + 153.8390924339484_r8*z)) + &
            y*(-90.2046337756875_r8 - 4142.8793862113125_r8*z + &
            y*(10.50720794170734_r8 + 2814.78225133626_r8*z)))) + &
            y*(3520.125411988816_r8 + y*(-1351.605895580406_r8 + &
            y*(731.4083582010072_r8 + y*(-216.60324087531103_r8 + &
            25.56203650166196_r8*y) + z*(-2381.829935897496_r8 + &
            (597.809129110048_r8 - 291.8983352012704_r8*z)*z)) + &
            z*(4165.4688847996085_r8 + z*(-1229.337851789418_r8 + &
            (681.370187043564_r8 - 66.7696405958478_r8*z)*z))) + &
            z*(-3443.057215135908_r8 + z*(1349.638121077468_r8 + &
            z*(-713.258224830552_r8 + &
            (176.8161433232_r8 - 31.68006188846728_r8*z)*z))))
    g_sa_t_mod = 0.5_r8*gsw_sfac*0.025_r8*g_sa_t_mod
   
    g_sa_mod = 8645.36753595126_r8 + &
            x*(-7296.43987145382_r8 + x*(8103.20462414788_r8 + &
            y_pt*(2175.341332000392_r8 + y_pt*(-274.2290036817964_r8 + &
            y_pt*(197.4670779425016_r8 + y_pt*(-68.5590309679152_r8 + &
            9.98788038278032_r8*y_pt)))) + &
            x*(-5458.34205214835_r8 - 980.14153344888_r8*y_pt + &
            x*(2247.60742726704_r8 - 340.1237483177863_r8*x + &
            220.542973797483_r8*y_pt))) + &
            y_pt*(-1480.222530425046_r8 + &
            y_pt*(-129.1994027934126_r8 + &
            y_pt*(-30.0682112585625_r8 + y_pt*(2.626801985426835_r8 ))))) + &
            y_pt*(1187.3715515697959_r8 + &
            y_pt*(1760.062705994408_r8 + y_pt*(-450.535298526802_r8 + &
            y_pt*(182.8520895502518_r8 + y_pt*(-43.3206481750622_r8 + &
            4.26033941694366_r8*y_pt)))))
    g_sa_mod = 0.5_r8*gsw_sfac*g_sa_mod   

    ct_sa_wrt_t = (g_sa_mod - (gsw_t0+pt0)*g_sa_t_mod)/gsw_cp0

end if

if (present(ct_t_wrt_t)) &
    ct_t_wrt_t = -(gsw_t0+pt0)*gsw_gibbs(0,2,0,sa,t,p)/gsw_cp0

if (present(ct_p_wrt_t)) &
    ct_p_wrt_t = -(gsw_t0+pt0)*gsw_gibbs(0,1,1,sa,t,p)/gsw_cp0
                       
return
end subroutine

!--------------------------------------------------------------------------
