#include <define.h>

MODULE MOD_SoilThermalParameters

!-----------------------------------------------------------------------
   USE MOD_Precision
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: hCapacity
   PUBLIC :: hConductivity
   PUBLIC :: soil_hcap_cond


! PRIVATE MEMBER FUNCTIONS:


!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------


   SUBROUTINE hCapacity (patchtype,lb,nl_soil,csol,porsl,wice_soisno,wliq_soisno,scv,dz_soisno,cv)


!-----------------------------------------------------------------------
!  Original author: Yongjiu Dai, September 15, 1999
!
!  calculation of heat capacities of snow / soil layers the volumetric
!  heat capacity is calculated as a linear combination in terms of the
!  volumetric fraction of the constituent phases.  Only used in urban
!  model. TODO: merge with SUBROUTINE soil_hcap_cond
!
! !REVISIONS:
!  07/19/2014, Yongjiu Dai: treat the wetland as soil column instead of
!              water body.
!  08/16/2014, Nan Wei: recalculate the heat capacity of soil layers
!              underneath the lake
!
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Const_Physical, only: cpice,cpliq
   IMPLICIT NONE

   integer, intent(in) :: lb                       ! lower bound of array
   integer, intent(in) :: nl_soil                  ! upper bound of array
   integer, intent(in) :: patchtype                ! land patch type (0=soil, 1=urban, 2=wetland,
   real(r8), intent(in) :: csol(1:nl_soil)         ! heat capacity of soil soilds [J/(m3 K)]
   real(r8), intent(in) :: porsl(1:nl_soil)        ! soil porosity
   real(r8), intent(in) :: wice_soisno(lb:nl_soil) ! ice lens [kg/m2]
   real(r8), intent(in) :: wliq_soisno(lb:nl_soil) ! liquid water [kg/m2]
   real(r8), intent(in) :: dz_soisno(lb:nl_soil)   ! layer thickness [m]
   real(r8), intent(in) :: scv                     ! snow water equivalent [mm]
   real(r8), intent(out) :: cv(lb:nl_soil)         ! heat capacity [J/(m2 K)]

!-----------------------------------------------------------------------
! Soil heat capacity, which from de Vires (1963)

      IF(patchtype<=2 .or. patchtype==4)THEN ! soil ground and wetland and lake
         cv(1:) = csol(1:)*(1.-porsl(1:))*dz_soisno(1:) &
                + wice_soisno(1:)*cpice + wliq_soisno(1:)*cpliq
      ELSE               ! glacier/ice sheet
         cv(1:) = wice_soisno(1:)*cpice + wliq_soisno(1:)*cpliq
      ENDIF
      IF(lb==1 .and. scv>0.) cv(1) = cv(1) + cpice*scv

! Snow heat capacity
      IF(lb<=0)THEN
         cv(:0) = cpliq*wliq_soisno(:0) + cpice*wice_soisno(:0)
      ENDIF

   END SUBROUTINE hCapacity


   SUBROUTINE hConductivity (patchtype,lb,nl_soil,&
                             dkdry,dksatu,porsl,dz_soisno,z_soisno,zi_soisno,&
                             t_soisno,wice_soisno,wliq_soisno,tk,tktopsoil)

!-----------------------------------------------------------------------
!  Original author: Yongjiu Dai, September 15, 1999
!
!  calculation of thermal conductivities of snow / soil layers The
!  thermal conductivity of soil is computed from the algorithm of
!  Johansen (as reported by Farouki 1981), and of snow is from the
!  formulation used in SNTHERM (Jordan 1991).
!
!  The thermal conductivities at the interfaces between two neighbor
!  layers (j, j+1) are derived from an assumption that the flux across
!  the interface is equal to that from the node j to the interface and
!  the flux from the interface to the node j+1.
!
!  Only used in urban model. TODO: merge with subroutine soil_hcap_cond
!
! !REVISIONS:
!  07/19/2014, Yongjiu Dai: treat the wetland as soil column instead of
!              water body.
!  08/16/2014, Nan Wei: recalculate the heat conductivity of soil layers
!              underneath the lake
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Const_Physical, only: denh2o,denice,tfrz,tkwat,tkice,tkair
   IMPLICIT NONE

   integer, intent(in) :: lb                         !lower bound of array
   integer, intent(in) :: nl_soil                    !upper bound of array
   integer, intent(in) :: patchtype                  !land patch type (0=soil, 1=urban, 2=wetland,
                                                     !3=land ice, 4=deep lake, 5=shallow lake)
   real(r8), intent(in) ::  dkdry(1:nl_soil)         !thermal conductivity for dry soil [W/m-K]
   real(r8), intent(in) :: dksatu(1:nl_soil)         !Thermal conductivity of saturated soil [W/m-K]
   real(r8), intent(in) ::  porsl(1:nl_soil)         !fractional volume between soil grains=1.-dmvol
   real(r8), intent(in) ::   dz_soisno(lb:nl_soil)   !layer thickness [m]
   real(r8), intent(in) ::    z_soisno(lb:nl_soil)   !node depth [m]
   real(r8), intent(in) ::   zi_soisno(lb-1:nl_soil) !interface depth [m]
   real(r8), intent(in) ::    t_soisno(lb:nl_soil)   !Nodal temperature [K]
   real(r8), intent(in) :: wice_soisno(lb:nl_soil)   !ice lens [kg/m2]
   real(r8), intent(in) :: wliq_soisno(lb:nl_soil)   !liquid water [kg/m2]

   real(r8), intent(out) :: tk(lb:nl_soil)           !thermal conductivity [W/(m K)]
   real(r8), optional, intent(out) :: tktopsoil

!  local
   real(r8) rhosnow         ! partial density of water (ice + liquid)
   real(r8) dksat           ! thermal conductivity for saturated soil (j/(k s m))
   real(r8) dke             ! kersten number
   real(r8) fl              ! fraction of liquid or unfrozen water to total water
   real(r8) satw            ! relative total water content of soil.
   real(r8) thk(lb:nl_soil) ! thermal conductivity of layer
   real(r8) xicevol

   integer i

!-----------------------------------------------------------------------
! Thermal conductivity of soil from Farouki (1981),
      DO i = 1, nl_soil

         IF(patchtype<=2 .or. patchtype==4)THEN         !soil ground, wetland and lake
            thk(i) = dkdry(i)       !rock or dry soil

            IF(porsl(i)>1.e-05 .and. (wice_soisno(i)+wliq_soisno(i)) > 0.0)THEN
               satw = (wliq_soisno(i)/denh2o+wice_soisno(i)/denice)/(dz_soisno(i)*porsl(i))
               satw = min(1., satw)
               IF(satw>.1e-6)THEN
                  IF (patchtype==4) satw = 1.
                  fl = wliq_soisno(i)/(wice_soisno(i)+wliq_soisno(i))
                  IF(t_soisno(i) >= tfrz) THEN  ! Unfrozen soil
                     dke = log10(satw) + 1.0
                     dke = max(dke, 0.)
                     dksat = dksatu(i)
                  ELSE                          ! Frozen soil
                     dke = satw
                     dksat = dksatu(i)*(2.29/0.57)**((1.-fl)*porsl(i))
                  ENDIF
                  thk(i) = dke*dksat + (1.-dke)*dkdry(i)
                  IF (patchtype==4) THEN
                     satw = (wliq_soisno(i)/denh2o+wice_soisno(i)/denice)/(dz_soisno(i)*porsl(i))
                     IF(satw > 1.0)THEN
                        xicevol = (satw-1.0)*porsl(i)
                        thk(i) = (thk(i) + xicevol*tkice)/(1.0 + xicevol)/(1.0 + xicevol)
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
            IF(present(tktopsoil))tktopsoil = thk(1)
         ELSEIF (patchtype == 3)THEN                                  ! glacier
            thk(i) = tkwat
            IF(t_soisno(i)<tfrz) thk(i) = tkice
         ENDIF

      ENDDO

! Thermal conductivity of snow
      IF(lb < 1)THEN
         DO i = lb, 0
            rhosnow = (wice_soisno(i)+wliq_soisno(i))/dz_soisno(i)

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

! the following consideration is try to avoid the snow conductivity to be
! dominant in the thermal conductivity of the interface.  Because when the
! distance of bottom snow node to the interface is larger than that of
! interface to top soil node, the snow thermal conductivity will be dominant,
! and the result is that lees heat transfer between snow and soil

! modified by Nan Wei, 08/25/2014
         IF (patchtype<=3) THEN                                       ! soil ground and wetland
            IF((i==0) .and. (z_soisno(i+1)-zi_soisno(i)<zi_soisno(i)-z_soisno(i)))THEN
               tk(i) = 2.*thk(i)*thk(i+1)/(thk(i)+thk(i+1))
               tk(i) = max(0.5*thk(i+1),tk(i))
            ELSE
               tk(i) = thk(i)*thk(i+1)*(z_soisno(i+1)-z_soisno(i)) &
                     /(thk(i)*(z_soisno(i+1)-zi_soisno(i))+thk(i+1)*(zi_soisno(i)-z_soisno(i)))
            ENDIF
         ELSE                                                          ! lake
            IF (i /= 0) THEN
               tk(i) = thk(i)*thk(i+1)*(z_soisno(i+1)-z_soisno(i)) &
                     /(thk(i)*(z_soisno(i+1)-zi_soisno(i))+thk(i+1)*(zi_soisno(i)-z_soisno(i)))
            ELSEIF (i == 0 .and. i>=lb) THEN
               tk(i) = thk(i)
            ENDIF
         ENDIF
! - END -
      ENDDO
      tk(nl_soil) = 0.

   END SUBROUTINE hConductivity

   SUBROUTINE soil_hcap_cond(vf_gravels_s,vf_om_s,vf_sand_s,vf_pores_s,&
                                       wf_gravels_s,wf_sand_s,k_solids,&
                                               csol,kdry,ksat_u,ksat_f,&
                                                      BA_alpha,BA_beta,&
                                  temperature,vf_water,vf_ice,hcap,thk)

!-----------------------------------------------------------------------
! !DESCRIPTION:
!  Calculate bulk soil heat capacity and soil thermal conductivity with
!  8 optional schemes The default soil thermal conductivity scheme is
!  the fourth one (Balland V. and P. A. Arp, 2005)
!
! !REFERENCES:
!  Dai et al.,2019: Evaluation of Soil Thermal Conductivity Schemes for
!  Use in Land Surface Modeling J. of Advances in Modeling Earth
!  Systems, DOI: 10.1029/2019MS001723
!
! !Original author: Yongjiu Dai, 02/2018/
!
! !REVISIONS:
!  06/2018, Nan Wei: add to CoLM/main
!  09/2022, Nan Wei: add soil thermal conductivity of Hailong He (Yan &
!           He et al., 2019)
!  -----------------------------------------------------------------------------------------
   USE MOD_Precision
   USE MOD_Const_Physical,only:tfrz
   USE MOD_Namelist

   IMPLICIT NONE
   real(r8), intent(in) :: vf_gravels_s ! volumetric fraction of gravels within soil solids
   real(r8), intent(in) :: vf_om_s      ! volumetric fraction of organic matter within soil solids
   real(r8), intent(in) :: vf_sand_s    ! volumetric fraction of sand within soil soilds
   real(r8), intent(in) :: vf_pores_s   ! volumetric pore space of the soil

   real(r8), intent(in) :: wf_gravels_s ! gravimetric fraction of gravels
   real(r8), intent(in) :: wf_sand_s    ! gravimetric fraction of sand within soil soilds
   real(r8), intent(in) :: k_solids     ! thermal conductivity of soil solids

   real(r8), intent(in) :: temperature  !
   real(r8), intent(in) :: vf_water     !
   real(r8), intent(in) :: vf_ice       !

   real(r8), intent(in) :: csol         ! heat capacity of dry soil [J/(m3 K)]
   real(r8), intent(in) :: kdry         ! thermal conductivity for dry soil [W/m/K]
   real(r8), intent(in) :: ksat_u       ! thermal conductivity of unfrozen saturated soil [W/m/K]
   real(r8), intent(in) :: ksat_f       ! thermal conductivity of frozen saturated soil [W/m/K]
   real(r8), intent(in) :: BA_alpha     ! alpha in Balland and Arp(2005) thermal conductivity scheme
   real(r8), intent(in) :: BA_beta      ! beta in Balland and Arp(2005) thermal conductivity scheme

   real(r8), intent(out) :: hcap        ! J/(m3 K)
   real(r8), intent(out) :: thk         ! W/(m K)

   real(r8) c_water, c_ice
   real(r8) a, kappa, alpha, beta
   real(r8) aa,aaa,nwm,nw_nwm,x,ga,gc
   integer i

   real(r8) sr ! wetness or degree of saturation = (vf_water+vf_ice)/vf_pores_s
   real(r8) ke ! Kersten number or normalized thermal conductivity
   real(r8) k_air,k_water,k_ice

! =========================================================================================
! The heat capacity and thermal conductivity [J(m3 K)]
! =========================================================================================
!*    c_water = 4.18e6 ! J/(m3 K)
!*    c_ice = 1.88e6   ! J/(m3 K)
      c_water = 4.188e6   ! J/(m3 K) = 4188[J/(kg K)]*1000(kg/m3)
      c_ice = 1.94153e6   ! J/(m3 K) = 2117.27[J/(kg K)]*917(kg/m3)

      hcap = csol + vf_water*c_water + vf_ice*c_ice

! -----------------------------------------------------------------------------------------
! Setting
! -----------------------------------------------------------------------------------------
      k_air = 0.024    ! (W/m/K)
      k_water = 0.57   ! (W/m/K)
      k_ice = 2.29     ! (W/m/K)

      a =  vf_gravels_s + vf_sand_s

      sr = (vf_water+vf_ice)/vf_pores_s
!      sr = max(1.0e-6, sr)
      sr = min(1.0, sr)

      IF(sr >= 1.0e-10) THEN
         select CASE (DEF_THERMAL_CONDUCTIVITY_SCHEME)
         CASE (1)
! -----------------------------------------------------------------------------------------
! [1] Oleson et al., 2013: Technical Description of version 4.5 of the Community Land Model
!     (CLM). NCAR/TN-503+STR (Section 6.3: Soil and Snow Thermal Properties)
! -----------------------------------------------------------------------------------------
            IF(temperature > tfrz)THEN ! Unfrozen soil
               ke = log10(sr) + 1.0
            ELSE                         ! Fozen or partially frozen soils
               ke = sr
            ENDIF

         CASE (2)
! -----------------------------------------------------------------------------------------
! [2] Johansen O (1975): Thermal conductivity of soils. PhD Thesis. Trondheim, Norway:
!     University of Trondheim. US army Crops of Engineerings,
!     CRREL English Translation 637.
! -----------------------------------------------------------------------------------------
            IF(temperature > tfrz)THEN ! Unfrozen soils
               IF(a > 0.4)THEN ! coarse-grained
                  ke = 0.7*log10(max(sr,0.05)) + 1.0
               ELSE            ! Fine-grained
                  ke = log10(max(sr,0.1)) + 1.0
               ENDIF
            ELSE                         ! Frozen or partially frozen soils
               ke = sr
            ENDIF

         CASE (3)
! -----------------------------------------------------------------------------------------
! [3] Cote, J., and J.-M. Konrad (2005), A generalized thermal conductivity model for soils
!     and construction materials. Canadian Geotechnical Journal, 42(2): 443-458.
! -----------------------------------------------------------------------------------------
            IF(temperature > tfrz)THEN ! Unfrozen soils
!              kappa =                       Unfrozen
!              /gravels and coarse sand     /4.60/
!              /medium and fine sands       /3.55/
!              /silty and clayey soils      /1.90/
!              /organic fibrous soils (peat)/0.60/
               IF(a > 0.40)THEN
                  kappa = 4.60
               ELSEIF(a > 0.25)THEN
                  kappa = 3.55
               ELSEIF(a > 0.01)THEN
                  kappa = 1.90
               ELSE
                  kappa = 0.60
               ENDIF

            ELSE                         ! Frozen or partially frozen soils
!              kappa =                      Frozen
!              /gravels and coarse sand     /1.70/
!              /medium and fine sands       /0.95/
!              /silty and clayey soils      /0.85/
!              /organic fibrous soils (peat)/0.25/
               IF(a > 0.40)THEN
                  kappa = 1.70
               ELSEIF(a > 0.25)THEN
                  kappa = 0.95
               ELSEIF(a > 0.01)THEN
                  kappa = 0.85
               ELSE
                  kappa = 0.25
               ENDIF
            ENDIF
            ke = kappa*sr/(1.0+(kappa-1.0)*sr)

         CASE (4)
! -----------------------------------------------------------------------------------------
! [4] Balland V. and P. A. Arp, 2005: Modeling soil thermal conductivities over a wide
! range of conditions. J. Environ. Eng. Sci. 4: 549-558.
! be careful in specifying all k affecting fractions as VOLUME FRACTION,
! whether these fractions are part of the bulk volume, the pore space, or the solid space.
! -----------------------------------------------------------------------------------------
            IF(temperature > tfrz)THEN ! Unfrozen soil
!              alpha = 0.24 ! adjustable parameter
!              beta = 18.1  ! adjustable parameter

               ke = sr**(0.5*(1.0+vf_om_s-BA_alpha*vf_sand_s-vf_gravels_s)) &
                      * ((1.0/(1.0+exp(-BA_beta*sr)))**3-((1.0-sr)/2.0)**3)**(1.0-vf_om_s)
            ELSE                         ! Frozen or partially frozen soils
               ke = sr**(1.0+vf_om_s)
            ENDIF

         CASE (5)
! -----------------------------------------------------------------------------------------
! [5] Lu et al., 2007: An improved model for predicting soil thermal conductivity from
!     water content at room temperature. Soil Sci. Soc. Am. J. 71:8-14
! -----------------------------------------------------------------------------------------
            IF(a > 0.4)THEN ! Coarse-textured soils = soils with sand fractions >40 (%)
               alpha = 0.728
               beta = 1.165
            ELSE ! Fine-textured soils = soils with sand fractions <40 (%)
               alpha = 0.37
               beta = 1.29
            ENDIF

            IF(temperature > tfrz)THEN ! Unfrozen soils
               ke = exp(alpha*(1.0-sr**(alpha-beta)))
            ELSE                         ! Frozen or partially frozen soils
               ke = sr
            ENDIF
         END select
      ELSE
         ke = 0.0
      ENDIF

      IF (DEF_THERMAL_CONDUCTIVITY_SCHEME >= 1 .and. DEF_THERMAL_CONDUCTIVITY_SCHEME <=5) THEN
         ke = max(ke, 0.0)
         ke = min(ke, 1.0)
         IF(temperature > tfrz)THEN ! Unfrozen soil
            thk = (ksat_u-kdry)*ke + kdry
         ELSE                         ! Frozen or partially frozen soils
            thk = (ksat_f-kdry)*ke + kdry
         ENDIF
      ENDIF

      IF(DEF_THERMAL_CONDUCTIVITY_SCHEME == 6) THEN
! -----------------------------------------------------------------------------------------
! [6] Series-Parallel Models (Tarnawski and Leong, 2012)
! -----------------------------------------------------------------------------------------
         a = wf_gravels_s+wf_sand_s

! a fitting parameter of the soil solid uniform passage
         aa = 0.0237 - 0.0175*a**3

! a fitting parameter of a minuscule portion of soil water (nw)
! plus a minuscule portion of soil air (na)
         nwm = 0.088 - 0.037*a**3

! the degree of saturation of the minuscle pore space
         x = 0.6 - 0.3*a**3
         IF(sr < 1.0e-6)THEN
            nw_nwm = 0.0
         ELSE
            nw_nwm = exp(1.0-sr**(-x))
         ENDIF

         IF(temperature > tfrz)THEN ! Unfrozen soil
            thk = k_solids*aa + (1.0-vf_pores_s-aa+nwm)**2 &
                / ((1.0-vf_pores_s-aa)/k_solids+nwm/(k_water*nw_nwm+k_air*(1.0-nw_nwm))) &
                + k_water*(vf_pores_s*sr-nwm*nw_nwm) &
                + k_air*(vf_pores_s*(1.0-sr)-nwm*(1.0-nw_nwm))
         ELSE
            thk = k_solids*aa + (1.0-vf_pores_s-aa+nwm)**2 &
                / ((1.0-vf_pores_s-aa)/k_solids+nwm/(k_ice*nw_nwm+k_air*(1.0-nw_nwm))) &
                + k_ice*(vf_pores_s*sr-nwm*nw_nwm) &
                + k_air*(vf_pores_s*(1.0-sr)-nwm*(1.0-nw_nwm))
         ENDIF
      ENDIF

      IF(DEF_THERMAL_CONDUCTIVITY_SCHEME == 7) THEN
! -----------------------------------------------------------------------------------------
! [7] Thermal properties of soils, in Physics of Plant Environment,
!     ed. by W.R. van Wijk (North-Holland, Amsterdam, 1963), pp. 210-235
! -----------------------------------------------------------------------------------------
         IF(sr*vf_pores_s <= 0.09)THEN
            ga = 0.013+0.944*sr*vf_pores_s
         ELSE
            ga = 0.333 - (1.-sr)*vf_pores_s/vf_pores_s*(0.333-0.035)
         ENDIF
            gc = 1.0-2.0*ga

         IF(temperature > tfrz)THEN ! Unfrozen soil
            aa  = (2.0/(1.0+(k_air/k_water-1.0)*ga) &    ! the shape factor
                +  1.0/(1.0+(k_air/k_water-1.0)*gc))/3.0
            aaa = (2.0/(1.0+(k_solids/k_water-1.0)*0.125) &    ! the shape factor
                +  1.0/(1.0+(k_solids/k_water-1.0)*(1.0-2.0*0.125)))/3.0

            thk = (sr*vf_pores_s*k_water + (1.-sr)*vf_pores_s*aa*k_air + &
                  (1.-vf_pores_s)*aaa*k_solids) &
                / (sr*vf_pores_s + (1.-sr)*vf_pores_s*aa + (1.-vf_pores_s)*aaa)
         ELSE
            aa  = (2.0/(1.0+(k_air/k_ice-1.0)*ga) &    ! the shape factor
                +  1.0/(1.0+(k_air/k_ice-1.0)*gc))/3.0
            aaa = (2.0/(1.0+(k_solids/k_ice-1.0)*0.125) &    ! the shape factor
                +  1.0/(1.0+(k_solids/k_ice-1.0)*(1.0-2.0*0.125)))/3.0

            thk = (sr*vf_pores_s*k_ice + (1.-sr)*vf_pores_s*aa*k_air + &
                  (1.-vf_pores_s)*aaa*k_solids) &
                / (sr*vf_pores_s + (1.-sr)*vf_pores_s*aa + (1.-vf_pores_s)*aaa)
         ENDIF
      ENDIF

      IF(DEF_THERMAL_CONDUCTIVITY_SCHEME == 8) THEN
! -----------------------------------------------------------------------------------------
! [8] Yan & He et al., 2019: A generalized model for estimating effective soil thermal conductivity
!     based on the Kasubuchi algorithm, Geoderma, Vol 353, 227-242
! -----------------------------------------------------------------------------------------
         beta = -0.303*ksat_u - 0.201*wf_sand_s + 1.532
         IF(vf_water > 0.01)THEN
            ke = (1+(vf_pores_s/beta)**(-1.0*beta))/(1+(vf_water/beta)**(-1.0*beta))
         ELSE
            ke = 0.0
         ENDIF

         ke = max(ke, 0.0)
         ke = min(ke, 1.0)

         IF(temperature > tfrz)THEN ! Unfrozen soil
            thk = (ksat_u-kdry)*ke + kdry
         ELSE                         ! Frozen or partially frozen soils
            thk = (ksat_f-kdry)*ke + kdry
         ENDIF
      ENDIF

   END SUBROUTINE soil_hcap_cond

END MODULE MOD_SoilThermalParameters
! ---------- EOP ------------
