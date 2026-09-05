#include <define.h>

MODULE MOD_Urban_PerviousTemperature
!-----------------------------------------------------------------------
!
! !DESCRIPTION:
!
!  The urban's pervious ground is equivalent to soil, and the heat
!  transfer process of the surface soil is calculated consistently. This
!  includes considering 10 layers of soil and up to 5 layers of snow,
!  with a layering scheme consistent with the soil (snow). The phase
!  change process is considered, and soil thermal parameters are
!  obtained from global data. The difference lies in the fact that the
!  shortwave and longwave radiation received at the surface, as well as
!  the turbulent exchange flux (sensible heat, latent heat), are solved
!  by the corresponding MODULE for the urban model.
!
!  Created by Yongjiu Dai and Hua Yuan, 05/2020
!
!-----------------------------------------------------------------------
   USE MOD_Precision
   IMPLICIT NONE
   SAVE

   PUBLIC :: UrbanPerviousTem

CONTAINS

   SUBROUTINE UrbanPerviousTem (patchtype,lb,deltim, &
                                capr,cnfac,csol,k_solids,porsl,psi0,dkdry,dksatu,dksatf,&
                                vf_quartz,vf_gravels,vf_om,vf_sand,wf_gravels,wf_sand,&
                                BA_alpha, BA_beta,&
#ifdef Campbell_SOIL_MODEL
                                bsw,&
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
                                theta_r,alpha_vgm,n_vgm,L_vgm,&
                                sc_vgm,fc_vgm,&
#endif
                                dz_gpersno,z_gpersno,zi_gpersno,&
                                t_gpersno,wice_gpersno,wliq_gpersno,scv_gper,snowdp_gper,&
                                lgper,clgper,sabgper,fsengper,fevpgper,cgper,htvp,&
                                imelt,sm,xmf,fact)

!=======================================================================
!  Snow and pervious road temperatures
!  o The volumetric heat capacity is calculated as a linear combination
!    in terms of the volumetric fraction of the constituent phases.
!  o The thermal conductivity of road soil is computed from
!    the algorithm of Johansen (as reported by Farouki 1981), impervious
!    and perivious from LOOK-UP table and of snow is from the formulation
!    used in SNTHERM (Jordan 1991).
!  o Boundary conditions:
!    F = Rnet - Hg - LEg (top),  F = 0 (base of the soil column).
!  o Soil / snow temperature is predicted from heat conduction
!    in 10 soil layers and up to 5 snow layers.  The thermal
!    conductivities at the interfaces between two neighbor layers
!    (j,j+1) are derived from an assumption that the flux across the
!    interface is equal to that from the node j to the interface and the
!    flux from the interface to the node j+1. The equation is solved
!    using the Crank-Nicholson method and resulted in a tridiagonal
!    system equation.
!
!  Phase change (see MOD_PhaseChange.F90)
!
!  Original author: Yongjiu Dai, 09/15/1999; 08/30/2002; 05/2020
!=======================================================================

   USE MOD_Precision
   USE MOD_Vars_Global
   USE MOD_Const_Physical
   USE MOD_SoilThermalParameters
   USE MOD_Utils, only: tridia
   USE MOD_PhaseChange, only: meltf

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
   real(r8), intent(in) :: k_solids  (1:nl_soil)       !thermal cond. of minerals soil [W/m-K]
   real(r8), intent(in) :: porsl     (1:nl_soil)       !soil porosity [-]
   real(r8), intent(in) :: psi0      (1:nl_soil)       !soil water suction, negative potential [mm]

   real(r8), intent(in) :: dkdry     (1:nl_soil)       !thermal cond. of dry soil [W/m-K]
   real(r8), intent(in) :: dksatu    (1:nl_soil)       !thermal cond. of sat soil [W/m-K]
   real(r8), intent(in) :: dksatf    (1:nl_soil)       !thermal cond. of sat frozen soil [W/m-K]

   real(r8), intent(in) :: vf_quartz (1:nl_soil)       !volumetric frac of quartz in mineral soil
   real(r8), intent(in) :: vf_gravels(1:nl_soil)       !volumetric frac of gravels
   real(r8), intent(in) :: vf_om     (1:nl_soil)       !volumetric frac of organic matter
   real(r8), intent(in) :: vf_sand   (1:nl_soil)       !volumetric frac of sand
   real(r8), intent(in) :: wf_gravels(1:nl_soil)       !gravimetric frac of gravels
   real(r8), intent(in) :: wf_sand   (1:nl_soil)       !gravimetric frac of sand

   real(r8), intent(in) :: BA_alpha  (1:nl_soil)       !alpha in Balland and Arp(2005) thermal cond.
   real(r8), intent(in) :: BA_beta   (1:nl_soil)       !beta in Balland and Arp(2005) thermal cond.

#ifdef Campbell_SOIL_MODEL
   real(r8), intent(in) :: bsw       (1:nl_soil)       !clapp and hornberger "b" parameter [-]
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
   real(r8), intent(in) :: theta_r   (1:nl_soil),&     !soil parameter for vanGenuchten scheme
                           alpha_vgm (1:nl_soil),&     !soil parameter for vanGenuchten scheme
                           n_vgm     (1:nl_soil),&     !soil parameter for vanGenuchten scheme
                           L_vgm     (1:nl_soil),&     !soil parameter for vanGenuchten scheme
                           sc_vgm    (1:nl_soil),&     !soil parameter for vanGenuchten scheme
                           fc_vgm    (1:nl_soil)       !soil parameter for vanGenuchten scheme
#endif

   real(r8), intent(in) :: dz_gpersno(lb  :nl_soil)    !layer thickness [m]
   real(r8), intent(in) :: z_gpersno (lb  :nl_soil)    !node depth [m]
   real(r8), intent(in) :: zi_gpersno(lb-1:nl_soil)    !interface depth [m]

   real(r8), intent(in) :: sabgper                     !solar radiation absorbed by ground [W/m2]
   real(r8), intent(in) :: lgper                       !atmospheric longwave radiation [W/m2]
   real(r8), intent(in) :: clgper                      !deriv. of longwave wrt to soil temp [w/m2/k]
   real(r8), intent(in) :: fsengper                    !sensible heat flux from ground [W/m2]
   real(r8), intent(in) :: fevpgper                    !evaporation heat flux from ground [mm/s]
   real(r8), intent(in) :: cgper                       !deriv. of soil energy flux to T [w/m2/k]
   real(r8), intent(in) :: htvp                        !latent heat of vapor (or sublimation) [j/kg]

   real(r8), intent(inout) :: t_gpersno   (lb:nl_soil) !soil temperature [K]
   real(r8), intent(inout) :: wice_gpersno(lb:nl_soil) !ice lens [kg/m2]
   real(r8), intent(inout) :: wliq_gpersno(lb:nl_soil) !liquid water [kg/m2]
   real(r8), intent(inout) :: scv_gper                 !snow cover, water equivalent [mm, kg/m2]
   real(r8), intent(inout) :: snowdp_gper              !snow depth [m]

   real(r8), intent(out) :: sm                         !rate of snowmelt [kg/(m2 s)]
   real(r8), intent(out) :: xmf                        !total latent heat of phase change in soil
   real(r8), intent(out) :: fact (lb:nl_soil)          !used in computing tridiagonal matrix
   integer,  intent(out) :: imelt(lb:nl_soil)          !flag for melting or freezing [-]

!-------------------------- Local Variables ----------------------------
   real(r8) cv(lb:nl_soil)            !heat capacity [J/(m2 K)]
   real(r8) tk(lb:nl_soil)            !thermal conductivity [W/(m K)]

   real(r8) hcap(1:nl_soil)           !J/(m3 K)
   real(r8) thk(lb:nl_soil)           !W/(m K)
   real(r8) rhosnow                   !partial density of water (ice + liquid)

   real(r8) at(lb:nl_soil)            !"a" vector for tridiagonal matrix
   real(r8) bt(lb:nl_soil)            !"b" vector for tridiagonal matrix
   real(r8) ct(lb:nl_soil)            !"c" vector for tridiagonal matrix
   real(r8) rt(lb:nl_soil)            !"r" vector for tridiagonal solution

   real(r8) fn (lb:nl_soil)           !heat diffusion through the layer interface [W/m2]
   real(r8) fn1(lb:nl_soil)           !heat diffusion through the layer interface [W/m2]
   real(r8) dzm                       !used in computing tridiagonal matrix
   real(r8) dzp                       !used in computing tridiagonal matrix

   real(r8) t_gpersno_bef(lb:nl_soil) !soil/snow temperature before update
   real(r8) hs                        !net energy flux into the surface (w/m2)
   real(r8) dhsdt                     !d(hs)/dT
   real(r8) brr(lb:nl_soil)           !temporary set

   real(r8) vf_water(1:nl_soil)       !volumetric fraction liquid water within soil
   real(r8) vf_ice(1:nl_soil)         !volumetric fraction ice len within soil

   integer i,j

!=======================================================================
! soil ground and wetland heat capacity
      DO i = 1, nl_soil
         vf_water(i) = wliq_gpersno(i)/(dz_gpersno(i)*denh2o)
         vf_ice(i)   = wice_gpersno(i)/(dz_gpersno(i)*denice)
         CALL soil_hcap_cond(vf_gravels(i),vf_om(i),vf_sand(i),porsl(i),&
                             wf_gravels(i),wf_sand(i),k_solids(i),&
                             csol(i),dkdry(i),dksatu(i),dksatf(i),&
                             BA_alpha(i),BA_beta(i),&
                             t_gpersno(i),vf_water(i),vf_ice(i),hcap(i),thk(i))
         cv(i) = hcap(i)*dz_gpersno(i)
      ENDDO
      IF(lb==1 .and. scv_gper>0.) cv(1) = cv(1) + cpice*scv_gper

! Snow heat capacity
      IF(lb <= 0)THEN
         cv(:0) = cpliq*wliq_gpersno(:0) + cpice*wice_gpersno(:0)
      ENDIF

! Snow thermal conductivity
      IF(lb <= 0)THEN
         DO i = lb, 0
            rhosnow = (wice_gpersno(i)+wliq_gpersno(i))/dz_gpersno(i)

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
         IF((i==0) .and. (z_gpersno(i+1)-zi_gpersno(i)<zi_gpersno(i)-z_gpersno(i)))THEN
            tk(i) = 2.*thk(i)*thk(i+1)/(thk(i)+thk(i+1))
            tk(i) = max(0.5*thk(i+1),tk(i))
         ELSE
            tk(i) = thk(i)*thk(i+1)*(z_gpersno(i+1)-z_gpersno(i)) &
                  /(thk(i)*(z_gpersno(i+1)-zi_gpersno(i))+thk(i+1)*(zi_gpersno(i)-z_gpersno(i)))
         ENDIF
      ENDDO
      tk(nl_soil) = 0.

! net ground heat flux into the surface and its temperature derivative
      hs = sabgper + lgper - (fsengper+fevpgper*htvp)
      dhsdT = - cgper + clgper

      t_gpersno_bef(lb:) = t_gpersno(lb:)

      j       = lb
      fact(j) = deltim / cv(j) * dz_gpersno(j) &
              / (0.5*(z_gpersno(j)-zi_gpersno(j-1)+capr*(z_gpersno(j+1)-zi_gpersno(j-1))))

      DO j = lb + 1, nl_soil
         fact(j) = deltim/cv(j)
      ENDDO

      DO j = lb, nl_soil - 1
        fn(j) = tk(j)*(t_gpersno(j+1)-t_gpersno(j))/(z_gpersno(j+1)-z_gpersno(j))
      ENDDO
      fn(nl_soil) = 0.

! set up vector r and vectors a, b, c that define tridiagonal matrix
      j     = lb
      dzp   = z_gpersno(j+1)-z_gpersno(j)
      at(j) = 0.
      bt(j) = 1+(1.-cnfac)*fact(j)*tk(j)/dzp-fact(j)*dhsdT
      ct(j) =  -(1.-cnfac)*fact(j)*tk(j)/dzp
      rt(j) = t_gpersno(j) + fact(j)*( hs - dhsdT*t_gpersno(j) + cnfac*fn(j) )


      DO j = lb + 1, nl_soil - 1
         dzm   = (z_gpersno(j)-z_gpersno(j-1))
         dzp   = (z_gpersno(j+1)-z_gpersno(j))
         at(j) =   - (1.-cnfac)*fact(j)* tk(j-1)/dzm
         bt(j) = 1.+ (1.-cnfac)*fact(j)*(tk(j)/dzp + tk(j-1)/dzm)
         ct(j) =   - (1.-cnfac)*fact(j)* tk(j)/dzp
         rt(j) = t_gpersno(j) + cnfac*fact(j)*( fn(j) - fn(j-1) )
      ENDDO

      j     =  nl_soil
      dzm   = (z_gpersno(j)-z_gpersno(j-1))
      at(j) =   - (1.-cnfac)*fact(j)*tk(j-1)/dzm
      bt(j) = 1.+ (1.-cnfac)*fact(j)*tk(j-1)/dzm
      ct(j) = 0.
      rt(j) = t_gpersno(j) - cnfac*fact(j)*fn(j-1)

! solve for t_gpersno
      i = size(at)
      CALL tridia (i ,at ,bt ,ct ,rt ,t_gpersno)

!=======================================================================
! melting or freezing
!=======================================================================

      DO j = lb, nl_soil - 1
         fn1(j) = tk(j)*(t_gpersno(j+1)-t_gpersno(j))/(z_gpersno(j+1)-z_gpersno(j))
      ENDDO
      fn1(nl_soil) = 0.

      j = lb
      brr(j) = cnfac*fn(j) + (1.-cnfac)*fn1(j)

      DO j = lb + 1, nl_soil
         brr(j) = cnfac*(fn(j)-fn(j-1)) + (1.-cnfac)*(fn1(j)-fn1(j-1))
      ENDDO

      CALL meltf (patchtype,.false.,lb,nl_soil,deltim,&
                  !NOTE: compatibility settings for splitting soil&snow
                  ! temporal input, as urban mode doesn't support split soil&snow
                  ! hs_soil=hs, hs_snow=hs, fsno=0.
                  fact(lb:),brr(lb:),hs,hs,hs,0.,dhsdT,&
                  t_gpersno_bef(lb:),t_gpersno(lb:),&
                  wliq_gpersno(lb:),wice_gpersno(lb:),imelt(lb:),&
                  scv_gper,snowdp_gper,sm,xmf,porsl,psi0,&
#ifdef Campbell_SOIL_MODEL
                  bsw,&
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
                  theta_r,alpha_vgm,n_vgm,L_vgm,&
                  sc_vgm,fc_vgm,&
#endif
                  dz_soi(1:nl_soil))

   END SUBROUTINE UrbanPerviousTem

END MODULE MOD_Urban_PerviousTemperature
! ---------- EOP ------------
