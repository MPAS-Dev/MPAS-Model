!==========================================================================
elemental subroutine gsw_rho_alpha_beta_bsq (sa, ct, p, rho, alpha, beta, &
                                             stiffened)
!==========================================================================
!
!  Calculates in-situ density (or optionally stiffened density), and the
!  appropriate Boussinesq thermal expansion and Boussinesq saline contraction
!  coefficients of seawater from absolute salinity, conservative temperature
!  and pressure, using the computationally-efficient 55-term polynomial
!  expression for density (Roquet et al., 2014).
!
!  sa  =  Absolute Salinity                                        [ g/kg ]
!  ct  =  Conservative Temperature (ITS-90)                       [ deg C ]
!  p   =  sea pressure                                             [ dbar ]
!         ( i.e. absolute pressure - 10.1325 dbar )
!
!  rho   =  In-situ density (rho = r1 x rdot)                    [ kg/m^3 ]
!           (or optionally stiffened density, rdot)
!  alpha =  Boussinesq thermal expansion (=-drho/dCT)          [ kg/m^3/K ]
!  beta  =  Boussinesq haline contraction (=drho/dSA)     [ kg/m^3/(g/kg) ]
!
!  Check values (sa=30g/kg, ct=10degC, p=1e3dbar):
!  rho  = 1027.45140
!  a    = 0.179649406
!  b    = 0.765554495
!  r1   = 1.00447333
!  rdot = 1022.87574
!
!  Roquet, F., Madec, G., McDougall, T. J., Barker, P. M., 2014: Accurate 
!    polynomial expressions for the density and specific volume of 
!    seawater using the TEOS-10 standard. Ocean Modelling, 90:29-43.
!--------------------------------------------------------------------------

use gsw_mod_teos10_constants, only : gsw_ups

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa, ct, p
logical, intent(in), optional :: stiffened
real (r8), intent(out) :: rho
real (r8), intent(out), optional :: alpha, beta

logical :: do_stiffened

real (r8) :: pp, rz0, rz1, rz2, rz3, ss, tt, r1

real (r8), parameter :: sau = 40.0_r8*gsw_ups
real (r8), parameter :: ctu = 40.0_r8
real (r8), parameter :: zu = 1e4_r8
real (r8), parameter :: deltas = 32.0_r8

real (r8), parameter :: r10 =  4.5238001132e-02_r8
real (r8), parameter :: r11 = -5.0691457704e-03_r8
real (r8), parameter :: r12 =  2.1990865986e-04_r8 
real (r8), parameter :: r13 =  6.2587720090e-05_r8
real (r8), parameter :: r14 =  1.5194795322e-05_r8
real (r8), parameter :: r15 = -1.6777531159e-06_r8

real (r8), parameter :: alp000 = -6.5047345980e-01_r8
real (r8), parameter :: alp100 =  1.6337444787e+00_r8
real (r8), parameter :: alp200 = -2.0484575392e+00_r8 
real (r8), parameter :: alp300 =  1.4268760685e+00_r8
real (r8), parameter :: alp400 = -4.4447427136e-01_r8
real (r8), parameter :: alp500 =  4.8463173700e-02_r8 
real (r8), parameter :: alp010 =  1.8523793418e+00_r8
real (r8), parameter :: alp110 = -3.0734838779e+00_r8
real (r8), parameter :: alp210 =  3.0136782240e+00_r8 
real (r8), parameter :: alp310 = -1.4543073694e+00_r8
real (r8), parameter :: alp410 =  2.7320572723e-01_r8
real (r8), parameter :: alp020 = -1.6234028145e+00_r8 
real (r8), parameter :: alp120 =  2.5061411737e+00_r8
real (r8), parameter :: alp220 = -1.4770589780e+00_r8
real (r8), parameter :: alp320 =  2.3782870611e-01_r8 
real (r8), parameter :: alp030 =  8.3587258634e-01_r8
real (r8), parameter :: alp130 = -1.1301873278e+00_r8
real (r8), parameter :: alp230 =  5.3494903247e-01_r8 
real (r8), parameter :: alp040 = -6.7823124325e-02_r8
real (r8), parameter :: alp140 = -5.9955123381e-02_r8
real (r8), parameter :: alp050 =  2.8648472338e-02_r8 
real (r8), parameter :: alp001 =  3.4328982507e-01_r8
real (r8), parameter :: alp101 =  1.0916815960e-01_r8
real (r8), parameter :: alp201 = -1.3224824721e-01_r8 
real (r8), parameter :: alp301 =  1.7830956551e-02_r8
real (r8), parameter :: alp011 = -3.7421662855e-01_r8
real (r8), parameter :: alp111 = -1.5721498096e-01_r8 
real (r8), parameter :: alp211 =  9.0708859933e-02_r8
real (r8), parameter :: alp021 =  1.9507636737e-01_r8
real (r8), parameter :: alp121 =  3.7400054411e-02_r8 
real (r8), parameter :: alp031 = -5.5882364387e-02_r8
real (r8), parameter :: alp002 = -7.1271116782e-02_r8
real (r8), parameter :: alp102 =  1.1117840325e-02_r8 
real (r8), parameter :: alp012 =  7.5296514078e-02_r8
real (r8), parameter :: alp003 = -6.3135413999e-03_r8 

real (r8), parameter :: bet000 =  1.0785939671e+01_r8
real (r8), parameter :: bet100 = -4.4465045269e+01_r8
real (r8), parameter :: bet200 =  7.6072094337e+01_r8 
real (r8), parameter :: bet300 = -6.3964420131e+01_r8
real (r8), parameter :: bet400 =  2.6898783594e+01_r8
real (r8), parameter :: bet500 = -4.5234968986e+00_r8 
real (r8), parameter :: bet010 = -8.1303841476e-01_r8
real (r8), parameter :: bet110 =  2.0388435182e+00_r8
real (r8), parameter :: bet210 = -2.1302689715e+00_r8 
real (r8), parameter :: bet310 =  8.8477644261e-01_r8
real (r8), parameter :: bet410 = -1.2058930400e-01_r8
real (r8), parameter :: bet020 =  7.6476477580e-01_r8 
real (r8), parameter :: bet120 = -1.4997670675e+00_r8
real (r8), parameter :: bet220 =  1.0856114040e+00_r8
real (r8), parameter :: bet320 = -2.7192349143e-01_r8 
real (r8), parameter :: bet030 = -4.1572985119e-01_r8
real (r8), parameter :: bet130 =  4.9004223351e-01_r8
real (r8), parameter :: bet230 = -1.1835625260e-01_r8 
real (r8), parameter :: bet040 =  1.4061037779e-01_r8
real (r8), parameter :: bet140 = -1.3310958936e-01_r8
real (r8), parameter :: bet050 =  5.9673736141e-03_r8 
real (r8), parameter :: bet001 = -5.2308076768e-01_r8
real (r8), parameter :: bet101 =  1.2084313165e+00_r8
real (r8), parameter :: bet201 = -1.1374069553e+00_r8 
real (r8), parameter :: bet301 =  3.2360305476e-01_r8
real (r8), parameter :: bet011 = -5.4327900468e-02_r8
real (r8), parameter :: bet111 =  1.3162756682e-01_r8 
real (r8), parameter :: bet211 = -2.6620905846e-02_r8
real (r8), parameter :: bet021 =  3.9119281064e-02_r8
real (r8), parameter :: bet121 = -4.5141568126e-02_r8 
real (r8), parameter :: bet031 = -6.2040874705e-03_r8
real (r8), parameter :: bet002 = -5.6500454603e-02_r8
real (r8), parameter :: bet102 =  6.7785665385e-02_r8 
real (r8), parameter :: bet012 = -5.5328304955e-03_r8
real (r8), parameter :: bet003 = -2.2276668383e-03_r8 

real (r8), parameter :: r000 =  8.0185969881e+02_r8
real (r8), parameter :: r100 =  8.6694399997e+02_r8
real (r8), parameter :: r200 = -1.7869886805e+03_r8 
real (r8), parameter :: r300 =  2.0381548497e+03_r8
real (r8), parameter :: r400 = -1.2853207957e+03_r8
real (r8), parameter :: r500 =  4.3240996619e+02_r8 
real (r8), parameter :: r600 = -6.0597695001e+01_r8
real (r8), parameter :: r010 =  2.6018938392e+01_r8
real (r8), parameter :: r110 = -6.5349779146e+01_r8 
real (r8), parameter :: r210 =  8.1938301569e+01_r8
real (r8), parameter :: r310 = -5.7075042739e+01_r8
real (r8), parameter :: r410 =  1.7778970855e+01_r8 
real (r8), parameter :: r510 = -1.9385269480e+00_r8
real (r8), parameter :: r020 = -3.7047586837e+01_r8
real (r8), parameter :: r120 =  6.1469677558e+01_r8 
real (r8), parameter :: r220 = -6.0273564480e+01_r8
real (r8), parameter :: r320 =  2.9086147388e+01_r8
real (r8), parameter :: r420 = -5.4641145446e+00_r8 
real (r8), parameter :: r030 =  2.1645370860e+01_r8
real (r8), parameter :: r130 = -3.3415215649e+01_r8
real (r8), parameter :: r230 =  1.9694119706e+01_r8 
real (r8), parameter :: r330 = -3.1710494147e+00_r8
real (r8), parameter :: r040 = -8.3587258634e+00_r8
real (r8), parameter :: r140 =  1.1301873278e+01_r8 
real (r8), parameter :: r240 = -5.3494903247e+00_r8
real (r8), parameter :: r050 =  5.4258499460e-01_r8
real (r8), parameter :: r150 =  4.7964098705e-01_r8 
real (r8), parameter :: r060 = -1.9098981559e-01_r8
real (r8), parameter :: r001 =  2.1989266031e+01_r8
real (r8), parameter :: r101 = -4.2043785414e+01_r8 
real (r8), parameter :: r201 =  4.8565183521e+01_r8
real (r8), parameter :: r301 = -3.0473875108e+01_r8
real (r8), parameter :: r401 =  6.5025796369e+00_r8 
real (r8), parameter :: r011 = -1.3731593003e+01_r8
real (r8), parameter :: r111 = -4.3667263842e+00_r8
real (r8), parameter :: r211 =  5.2899298884e+00_r8 
real (r8), parameter :: r311 = -7.1323826203e-01_r8
real (r8), parameter :: r021 =  7.4843325711e+00_r8
real (r8), parameter :: r121 =  3.1442996192e+00_r8 
real (r8), parameter :: r221 = -1.8141771987e+00_r8
real (r8), parameter :: r031 = -2.6010182316e+00_r8
real (r8), parameter :: r131 = -4.9866739215e-01_r8 
real (r8), parameter :: r041 =  5.5882364387e-01_r8
real (r8), parameter :: r002 =  1.1144125393e+00_r8
real (r8), parameter :: r102 = -4.5413502768e+00_r8 
real (r8), parameter :: r202 =  2.7242121539e+00_r8
real (r8), parameter :: r012 =  2.8508446713e+00_r8
real (r8), parameter :: r112 = -4.4471361300e-01_r8 
real (r8), parameter :: r022 = -1.5059302816e+00_r8
real (r8), parameter :: r003 =  1.9817079368e-01_r8
real (r8), parameter :: r103 = -1.7905369937e-01_r8 
real (r8), parameter :: r013 =  2.5254165600e-01_r8 

if (present(stiffened)) then
   do_stiffened = stiffened
else
   do_stiffened = .false.
end if

! reduced variables
ss = sqrt((sa+deltas)/sau)
tt = ct/ctu
pp =  p/zu

rz3 =    r013*tt+r103*ss+r003
rz2 =   (r022*tt+r112*ss+r012)*tt+(r202*ss+r102)*ss+r002
rz1 =      (((r041*tt+r131*ss+r031)*tt                                       &
        +    (r221*ss+r121)*ss+r021)*tt                                      &
        +   ((r311*ss+r211)*ss+r111)*ss+r011)*tt                             &
        +  (((r401*ss+r301)*ss+r201)*ss+r101)*ss+r001
rz0 =    (((((r060*tt+r150*ss+r050)*tt                                       &
        +    (r240*ss+r140)*ss+r040)*tt                                      &
        +   ((r330*ss+r230)*ss+r130)*ss+r030)*tt                             &
        +  (((r420*ss+r320)*ss+r220)*ss+r120)*ss+r020)*tt                    &
        + ((((r510*ss+r410)*ss+r310)*ss+r210)*ss+r110)*ss+r010)*tt           &
        +(((((r600*ss+r500)*ss+r400)*ss+r300)*ss+r200)*ss+r100)*ss+r000

! stiffened density (rdot)
rho = ((rz3*pp + rz2)*pp + rz1)*pp + rz0

if (present(alpha) .or. present(beta) .or. .not. do_stiffened) then

   ! vertical reference profile of density
   r1 = (((((r15*pp + r14)*pp + r13)*pp + r12)*pp + r11)*pp + r10)*pp + 1.0_r8

   ! in-situ density
   if (.not. do_stiffened) rho = r1 * rho 

   ! thermal expansion alpha
   if (present(alpha)) then
      alpha = ((alp003*pp                                                    &
                + alp012*tt + alp102*ss+alp002)*pp                           &
                + ((alp031*tt                                                &
                + alp121*ss+alp021)*tt                                       &
                + (alp211*ss+alp111)*ss+alp011)*tt                           &
                + ((alp301*ss+alp201)*ss+alp101)*ss+alp001)*pp               &
                + ((((alp050*tt                                              &
                + alp140*ss+alp040)*tt                                       &
                + (alp230*ss+alp130)*ss+alp030)*tt                           &
                + ((alp320*ss+alp220)*ss+alp120)*ss+alp020)*tt               &
                + (((alp410*ss+alp310)*ss+alp210)*ss+alp110)*ss+alp010)*tt   &
                + ((((alp500*ss+alp400)*ss+alp300)*ss+alp200)*ss+alp100)*ss  &
                + alp000
      alpha = alpha * r1
   end if

   ! haline contraction beta
   if (present(beta)) then
      beta = ((bet003*pp                                                     &
                + bet012*tt + bet102*ss+bet002)*pp                           &
                + ((bet031*tt                                                &
                + bet121*ss+bet021)*tt                                       &
                + (bet211*ss+bet111)*ss+bet011)*tt                           &
                + ((bet301*ss+bet201)*ss+bet101)*ss+bet001)*pp               &
                + ((((bet050*tt                                              &
                + bet140*ss+bet040)*tt                                       &
                + (bet230*ss+bet130)*ss+bet030)*tt                           &
                + ((bet320*ss+bet220)*ss+bet120)*ss+bet020)*tt               &
                + (((bet410*ss+bet310)*ss+bet210)*ss+bet110)*ss+bet010)*tt   &
                + ((((bet500*ss+bet400)*ss+bet300)*ss+bet200)*ss+bet100)*ss  &
                + bet000
      beta = beta * r1 / ss 
   end if
end if

return
end subroutine
