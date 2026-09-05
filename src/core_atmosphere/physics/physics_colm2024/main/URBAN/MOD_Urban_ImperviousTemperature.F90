#include <define.h>

MODULE MOD_Urban_ImperviousTemperature
!-----------------------------------------------------------------------
!
! !DESCRIPTION:
!
!  The main difference between calculating the temperature conduction
!  for an impervious ground and a pervious surface lies in the need to
!  USE the thermal properties (thermal conductivity and heat capacity)
!  of the imperious surface layer instead of the soil thermal
!  properties. Additionally, when snow, ice, and water are present, the
!  heat capacity of the first impervious surface layer needs to be
!  adjusted. The impervious surface does not consider the transmission
!  of water below the surface, and the phase change process only
!  considers the first impervious surface layer (surface water/ice) and
!  the overlying snow cover layer.
!
!  Created by Yongjiu Dai and Hua Yuan, 05/2020
!
!-----------------------------------------------------------------------
   USE MOD_Precision
   IMPLICIT NONE
   SAVE

   PUBLIC :: UrbanImperviousTem

CONTAINS

   SUBROUTINE UrbanImperviousTem (patchtype,lb,deltim, &
                                  capr,cnfac,csol,k_solids,porsl,psi0,dkdry,dksatu,dksatf,&
                                  vf_quartz,vf_gravels,vf_om,vf_sand,wf_gravels,wf_sand,&
                                  BA_alpha, BA_beta,&
                                  cv_gimp,tk_gimp,dz_gimpsno,z_gimpsno,zi_gimpsno,&
                                  t_gimpsno,wice_gimpsno,wliq_gimpsno,scv_gimp,snowdp_gimp,&
                                  lgimp,clgimp,sabgimp,fsengimp,fevpgimp,cgimp,htvp,&
                                  imelt,sm,xmf,fact)

!=======================================================================
!  Snow and impervious road temperatures
!  o The volumetric heat capacity is calculated as a linear combination
!    in terms of the volumetric fraction of the constituent phases.
!  o The thermal conductivity of road soil is computed from
!    the algorithm of Johansen (as reported by Farouki 1981), impervious
!    and pervious from LOOK-UP table and of snow is from the formulation
!    used in SNTHERM (Jordan 1991).
!  o Boundary conditions:
!    F = Rnet - Hg - LEg (top),  F = 0 (base of the soil column).
!  o Soil / snow temperature is predicted from heat conduction
!    in 10 soil layers and up to 5 snow layers.  The thermal
!    conductivities at the interfaces between two neighbor layers (j,j+1)
!    are derived from an assumption that the flux across the interface is
!    equal to that from the node j to the interface and the flux from the
!    interface to the node j+1. The equation is solved using the
!    Crank-Nicholson method and resulted in a tridiagonal system
!    equation.
!
!  Phase change (see MOD_PhaseChange.F90)
!
!  Original author: Yongjiu Dai, 09/15/1999; 08/30/2002; 05/2020
!=======================================================================

   USE MOD_Precision
   USE MOD_Vars_Global
   USE MOD_Const_Physical
   USE MOD_SoilThermalParameters
   USE MOD_PhaseChange, only: meltf_urban
   USE MOD_Utils, only: tridia

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer, intent(in)  :: lb                          !lower bound of array
   integer, intent(in)  :: patchtype                   !land patch type
                                                       !(0=soil,1=urban or built-up,2=wetland,
                                                       !3=land ice, 4=deep lake, 5=shallow lake)
   real(r8), intent(in) :: deltim                      !seconds in a time step [second]
   real(r8), intent(in) :: capr                        !tuning factor: turn 1st layer T to surface T
   real(r8), intent(in) :: cnfac                       !Crank Nicholson factor between 0 and 1

   real(r8), intent(in) :: csol      (1:nl_soil)       !heat capacity of soil solids [J/(m3 K)]
   real(r8), intent(in) :: k_solids  (1:nl_soil)       !thermal conductivity of minerals [W/m-K]
   real(r8), intent(in) :: porsl     (1:nl_soil)       !soil porosity [-]
   real(r8), intent(in) :: psi0      (1:nl_soil)       !soil water suction, negative potential [mm]

   real(r8), intent(in) :: dkdry     (1:nl_soil)       !thermal conductivity of dry soil [W/m-K]
   real(r8), intent(in) :: dksatu    (1:nl_soil)       !thermal conductivity of sat soil [W/m-K]
   real(r8), intent(in) :: dksatf    (1:nl_soil)       !thermal cond. of sat frozen soil [W/m-K]

   real(r8), intent(in) :: vf_quartz (1:nl_soil)       !volumetric frac of quartz in mineral soil
   real(r8), intent(in) :: vf_gravels(1:nl_soil)       !volumetric frac of gravels
   real(r8), intent(in) :: vf_om     (1:nl_soil)       !volumetric frac of organic matter
   real(r8), intent(in) :: vf_sand   (1:nl_soil)       !volumetric frac of sand
   real(r8), intent(in) :: wf_gravels(1:nl_soil)       !gravimetric frac of gravels
   real(r8), intent(in) :: wf_sand   (1:nl_soil)       !gravimetric frac of sand

   real(r8), intent(in) :: BA_alpha  (1:nl_soil)       !alpha in Balland and Arp(2005) thermal cond.
   real(r8), intent(in) :: BA_beta   (1:nl_soil)       !beta in Balland and Arp(2005) thermal cond.

   real(r8), intent(in) :: cv_gimp   (1:nl_soil)       !heat capacity of urban impervious [J/m3/K]
   real(r8), intent(in) :: tk_gimp   (1:nl_soil)       !thermal cond. of urban impervious [W/m/K]

   real(r8), intent(in) :: dz_gimpsno(lb  :nl_soil)    !layer thickness [m]
   real(r8), intent(in) :: z_gimpsno (lb  :nl_soil)    !node depth [m]
   real(r8), intent(in) :: zi_gimpsno(lb-1:nl_soil)    !interface depth [m]

   real(r8), intent(in) :: sabgimp                     !solar radiation absorbed by ground [W/m2]
   real(r8), intent(in) :: lgimp                       !atmospheric longwave radiation [W/m2]
   real(r8), intent(in) :: clgimp                      !deriv. of longwave wrt to soil temp [w/m2/k]
   real(r8), intent(in) :: fsengimp                    !sensible heat flux from ground [W/m2]
   real(r8), intent(in) :: fevpgimp                    !evaporation heat flux from ground [mm/s]
   real(r8), intent(in) :: cgimp                       !deriv. of gimp energy flux to T [w/m2/k]
   real(r8), intent(in) :: htvp                        !latent heat of vapor (or sublimation) [j/kg]

   real(r8), intent(inout) :: t_gimpsno   (lb:nl_soil) !soil temperature [K]
   real(r8), intent(inout) :: wice_gimpsno(lb:nl_soil) !ice lens [kg/m2]
   real(r8), intent(inout) :: wliq_gimpsno(lb:nl_soil) !liqui water [kg/m2]
   real(r8), intent(inout) :: scv_gimp                 !snow cover, water equivalent [mm, kg/m2]
   real(r8), intent(inout) :: snowdp_gimp              !snow depth [m]

   real(r8), intent(out) :: sm                         !rate of snowmelt [kg/(m2 s)]
   real(r8), intent(out) :: xmf                        !total latent heat of phase change in soil
   real(r8), intent(out) :: fact (lb:nl_soil)          !used in computing tridiagonal matrix
   integer,  intent(out) :: imelt(lb:nl_soil)          !flag for melting or freezing [-]

!-------------------------- Local Variables ----------------------------
   real(r8) cv (lb:nl_soil)           !heat capacity [J/(m2 K)]
   real(r8) tk (lb:nl_soil)           !thermal conductivity [W/(m K)]

   real(r8) hcap(1:nl_soil)           !J/(m3 K)
   real(r8) thk(lb:nl_soil)           !W/(m K)
   real(r8) rhosnow                   !partial density of water (ice + liquid)

   real(r8) at (lb:nl_soil)           !"a" vector for tridiagonal matrix
   real(r8) bt (lb:nl_soil)           !"b" vector for tridiagonal matrix
   real(r8) ct (lb:nl_soil)           !"c" vector for tridiagonal matrix
   real(r8) rt (lb:nl_soil)           !"r" vector for tridiagonal solution

   real(r8) fn (lb:nl_soil)           !heat diffusion through the layer interface [W/m2]
   real(r8) fn1(lb:nl_soil)           !heat diffusion through the layer interface [W/m2]
   real(r8) dzm                       !used in computing tridiagonal matrix
   real(r8) dzp                       !used in computing tridiagonal matrix

   real(r8) t_gimpsno_bef(lb:nl_soil) !soil/snow temperature before update
   real(r8) hs                        !net energy flux into the surface (w/m2)
   real(r8) dhsdt                     !d(hs)/dT
   real(r8) brr(lb:nl_soil)           !temporary set

   real(r8) vf_water(1:nl_soil)       !volumetric fraction liquid water within soil
   real(r8) vf_ice  (1:nl_soil)       !volumetric fraction ice len within soil

   integer i,j

!-----------------------------------------------------------------------

      wice_gimpsno(2:) = 0.0         !ice lens [kg/m2]
      wliq_gimpsno(2:) = 0.0         !liquid water [kg/m2]

!=======================================================================
! soil ground and wetland heat capacity
      DO i = 1, nl_soil
         vf_water(i) = wliq_gimpsno(i)/(dz_gimpsno(i)*denh2o)
         vf_ice(i)   = wice_gimpsno(i)/(dz_gimpsno(i)*denice)
         CALL soil_hcap_cond(vf_gravels(i),vf_om(i),vf_sand(i),porsl(i),&
                             wf_gravels(i),wf_sand(i),k_solids(i),&
                             csol(i),dkdry(i),dksatu(i),dksatf(i),&
                             BA_alpha(i),BA_beta(i),&
                             t_gimpsno(i),vf_water(i),vf_ice(i),hcap(i),thk(i))
         cv(i) = hcap(i)*dz_gimpsno(i)
      ENDDO
      IF(lb==1 .and. scv_gimp>0.) cv(1) = cv(1) + cpice*scv_gimp

! Snow heat capacity
      IF(lb <= 0)THEN
         cv(:0) = cpliq*wliq_gimpsno(:0) + cpice*wice_gimpsno(:0)
      ENDIF

! Snow thermal conductivity
      IF(lb <= 0)THEN
         DO i = lb, 0
            rhosnow = (wice_gimpsno(i)+wliq_gimpsno(i))/dz_gimpsno(i)

            ! presently option [1] is the default option
            ! [1] Jordan (1991) pp. 18
            thk(i) = tkair+(7.75e-5*rhosnow+1.105e-6*rhosnow*rhosnow)*(tkice-tkair)

            ! [2] Sturm et al (1997)
            ! thk(i) = 0.0138 + 1.01e-3*rhosnow + 3.233e-6*rhosnow**2
            ! [3] Ostin and Andersson presented in Sturm et al., (1997)
            ! thk(i) = -0.871e-2 + 0.439e-3*rhosnow + 1.05e-6*rhosnow**2
            ! [4] Jansson(1901) presented in Sturm et al. (1997)
            ! thk(i) = 0.0293 + 0.7953e-3*rhosnow + 1.512e-12*rhosnow**2
            ! [5] Douville et al., (1995)
            ! thk(i) = 2.2*(rhosnow/denice)**1.88
            ! [6] van Dusen (1992) presented in Sturm et al. (1997)
            ! thk(i) = 0.021 + 0.42e-3*rhosnow + 0.22e-6*rhosnow**2

         ENDDO
      ENDIF

! Thermal conductivity at the layer interface
      DO i = lb, nl_soil-1

! the following consideration is try to avoid the snow conductivity
! to be dominant in the thermal conductivity of the interface.
! Because when the distance of bottom snow node to the interface
! is larger than that of interface to top soil node,
! the snow thermal conductivity will be dominant, and the result is that
! lees heat transfer between snow and soil
         IF((i==0) .and. (z_gimpsno(i+1)-zi_gimpsno(i)<zi_gimpsno(i)-z_gimpsno(i)))THEN
            tk(i) = 2.*thk(i)*thk(i+1)/(thk(i)+thk(i+1))
            tk(i) = max(0.5*thk(i+1),tk(i))
         ELSE
            tk(i) = thk(i)*thk(i+1)*(z_gimpsno(i+1)-z_gimpsno(i)) &
                  /(thk(i)*(z_gimpsno(i+1)-zi_gimpsno(i))+thk(i+1)*(zi_gimpsno(i)-z_gimpsno(i)))
         ENDIF
      ENDDO
      tk(nl_soil) = 0.

      WHERE (tk_gimp > 0.) tk(1:) = tk_gimp(1:)
      WHERE (cv_gimp > 0.) cv(1:) = cv_gimp(1:)*dz_gimpsno(1:)

      ! snow exist when there is no snow layer
      IF (lb == 1 .and. scv_gimp > 0.0) THEN
         cv(1) = cv(1) + cpice*scv_gimp
      ENDIF

      ! ponding water or ice exist
      cv(1) = cv(1) + cpliq*wliq_gimpsno(1) + cpice*wice_gimpsno(1)

! net ground heat flux into the surface and its temperature derivative
      hs = sabgimp + lgimp - (fsengimp+fevpgimp*htvp)
      dhsdT = - cgimp + clgimp

      t_gimpsno_bef(lb:) = t_gimpsno(lb:)

      j       = lb
      fact(j) = deltim / cv(j) * dz_gimpsno(j) &
              / (0.5*(z_gimpsno(j)-zi_gimpsno(j-1)+capr*(z_gimpsno(j+1)-zi_gimpsno(j-1))))

      DO j = lb + 1, nl_soil
         fact(j) = deltim/cv(j)
      ENDDO

      DO j = lb, nl_soil - 1
        fn(j) = tk(j)*(t_gimpsno(j+1)-t_gimpsno(j))/(z_gimpsno(j+1)-z_gimpsno(j))
      ENDDO
      fn(nl_soil) = 0.

! set up vector r and vectors a, b, c that define tridiagonal matrix
      j     = lb
      dzp   = z_gimpsno(j+1)-z_gimpsno(j)
      at(j) = 0.
      bt(j) = 1+(1.-cnfac)*fact(j)*tk(j)/dzp-fact(j)*dhsdT
      ct(j) =  -(1.-cnfac)*fact(j)*tk(j)/dzp
      rt(j) = t_gimpsno(j) + fact(j)*( hs - dhsdT*t_gimpsno(j) + cnfac*fn(j) )


      DO j = lb + 1, nl_soil - 1
         dzm   = (z_gimpsno(j)-z_gimpsno(j-1))
         dzp   = (z_gimpsno(j+1)-z_gimpsno(j))
         at(j) =   - (1.-cnfac)*fact(j)* tk(j-1)/dzm
         bt(j) = 1.+ (1.-cnfac)*fact(j)*(tk(j)/dzp + tk(j-1)/dzm)
         ct(j) =   - (1.-cnfac)*fact(j)* tk(j)/dzp
         rt(j) = t_gimpsno(j) + cnfac*fact(j)*( fn(j) - fn(j-1) )
      ENDDO

      j     =  nl_soil
      dzm   = (z_gimpsno(j)-z_gimpsno(j-1))
      at(j) =   - (1.-cnfac)*fact(j)*tk(j-1)/dzm
      bt(j) = 1.+ (1.-cnfac)*fact(j)*tk(j-1)/dzm
      ct(j) = 0.
      rt(j) = t_gimpsno(j) - cnfac*fact(j)*fn(j-1)

! solve for t_gimpsno
      i = size(at)
      CALL tridia (i ,at ,bt ,ct ,rt ,t_gimpsno)

!=======================================================================
! melting or freezing
!=======================================================================

      DO j = lb, nl_soil - 1
         fn1(j) = tk(j)*(t_gimpsno(j+1)-t_gimpsno(j))/(z_gimpsno(j+1)-z_gimpsno(j))
      ENDDO
      fn1(nl_soil) = 0.

      j = lb
      brr(j) = cnfac*fn(j) + (1.-cnfac)*fn1(j)

      DO j = lb + 1, nl_soil
         brr(j) = cnfac*(fn(j)-fn(j-1)) + (1.-cnfac)*(fn1(j)-fn1(j-1))
      ENDDO

      CALL meltf_urban (lb,1,deltim, &
                  fact(lb:1),brr(lb:1),hs,dhsdT, &
                  t_gimpsno_bef(lb:1),t_gimpsno(lb:1), &
                  wliq_gimpsno(lb:1),wice_gimpsno(lb:1),imelt(lb:1), &
                  scv_gimp,snowdp_gimp,sm,xmf)

   END SUBROUTINE UrbanImperviousTem

END MODULE MOD_Urban_ImperviousTemperature
! ---------- EOP ------------
