#include <define.h>

MODULE MOD_SoilSurfaceResistance
!-----------------------------------------------------------------------
! !DESCRIPTION:
!  Calculate the soil surface resistance with multiple parameterization
!  schemes
!
!  Created by Zhuo Liu and Hua Yuan, 06/2023
!
! !REVISIONS:
!
!-----------------------------------------------------------------------
! !USE

   USE MOD_Precision
   IMPLICIT NONE
   SAVE

   PUBLIC :: SoilSurfaceResistance

   ! soil-gas diffusivity schemes:
   ! 1: BBC (Buckingham-Burdine-Campbell Model), Moldrup et al., 1999.
   ! 2: P_WLR (Penman Water Linear Reduction Model), Moldrup et al., 2000
   ! 3: MI_WLR (Millington Water Linear Reduction Model), Moldrup et al., 2000
   ! 4: MA_WLR (Marshal Water Linear Reduction Model), Moldrup et al., 2000
   ! 5: M_Q, Millington and Quirk, 1961
   ! 6: 3POE (Three-Porosity-Encased), Moldrup et al., 2005
#ifdef Campbell_SOIL_MODEL
   integer, parameter :: soil_gas_diffusivity_scheme = 1
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
   integer, parameter :: soil_gas_diffusivity_scheme = 6
#endif


CONTAINS
!-----------------------------------------------------------------------

   SUBROUTINE SoilSurfaceResistance (nl_soil,forc_rhoair,hksati,porsl,psi0, &
#ifdef Campbell_SOIL_MODEL
                              bsw, &
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
                              theta_r, alpha_vgm, n_vgm, L_vgm, sc_vgm, fc_vgm, &
#endif
                              dz_soisno,t_soisno,wliq_soisno,wice_soisno,fsno,qg,rss)

!=======================================================================
! !DESCRIPTION:
!  Main SUBROUTINE to CALL soil resistance model
!  - Options for soil surface resistance schemes
!    1: SL14, Swenson and Lawrence (2014)
!    2: SZ09, Sakaguchi and Zeng (2009)
!    3: TR13, Tang and Riley (2013)
!    4: LP92, Lee and Pielke (1992)
!    5: S92,  Sellers et al (1992)
!
!  NOTE: Support for both Campbell and VG soil parameters.
!=======================================================================

   USE MOD_Precision
   USE MOD_Const_Physical, only: denice, denh2o
   USE MOD_Namelist, only: DEF_RSS_SCHEME
   USE MOD_Hydro_SoilFunction
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer, intent(in) :: &
        nl_soil                       ! upper bound of array

   real(r8), intent(in) :: &
        forc_rhoair,                 &! density air [kg/m**3]
        hksati      (1:nl_soil),     &! hydraulic conductivity at saturation [mm h2o/s]
        porsl       (1:nl_soil),     &! soil porosity [-]
        psi0        (1:nl_soil),     &! saturated soil suction [mm] (NEGATIVE)
#ifdef Campbell_SOIL_MODEL
        bsw         (1:nl_soil),     &! clapp and hornberger "b" parameter [-]
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
        theta_r     (1:nl_soil),     &! residual moisture content [-]
        ! a parameter corresponding approximately to the inverse of the air-entry value
        alpha_vgm   (1:nl_soil),     &
        n_vgm       (1:nl_soil),     &! pore-connectivity parameter [dimensionless]
        L_vgm       (1:nl_soil),     &! a shape parameter [dimensionless]
        ! saturation at the air entry value in the classical vanGenuchten model [-]
        sc_vgm      (1:nl_soil),     &
        ! a scaling factor by using air entry value in the Mualem model [-]
        fc_vgm      (1:nl_soil),     &
#endif
        dz_soisno   (1:nl_soil),     &! layer thickness [m]
        t_soisno    (1:nl_soil),     &! soil/snow skin temperature [K]
        wliq_soisno (1:nl_soil),     &! liquid water [kg/m2]
        wice_soisno (1:nl_soil),     &! ice lens [kg/m2]
        fsno,                        &! fractional snow cover [-]
        qg                            ! ground specific humidity [kg/kg]

   real(r8), intent(out) :: &
        rss                           ! soil surface resistance [s/m]

!-------------------------- Local Variables ----------------------------

   REAL(r8) :: &
        wx,               &! partial volume of ice and water of surface layer
        vol_liq,          &! water content by volume [m3/m3]
        s_node,           &! vol_liq/porosity
        smp_node,         &! matrix potential [m]
        eff_porosity,     &! effective porosity = porosity - vol_ice
        aird,             &! “air-dry” soil moisture value
        d0,               &! water vapor diffusivity in open air [m2/s]
        eps,              &! air filled pore space
        dg,               &! gaseous diffusivity [m2/s]
        dsl,              &! soil dry surface layer thickness [m]
        dw,               &! aqueous diffusivity [m2/s]
        hk,               &! hydraulic conductivity [m h2o/s]
        m_vgm,            &! pore-connectivity related parameter [dimensionless]
        S,                &! Van Genuchten relative saturation [-]
        wfc,              &! field capacity of the first layer soil
        rg_1,             &! inverse of vapor diffusion resistance [m/s]
        rw_1,             &! inverse of volatilization resistance [m/s]
        rss_1,            &! inverse of soil surface resistance [m/s]
        tao,              &! tortuosity of the vapor flow paths through the soil matrix
        eps100,           &! air-filled porosity at −1000 mm of water matric potential
        fac,              &! temporal variable for calculating wx/porsl
        fac_fc,           &! temporal variable for calculating wx/wfc
        B                  ! bunsen solubility coefficient

!-----------------------------------------------------------------------

      ! calculate the top soil volumetric water content (m3/m3), soil matrix potential
      ! and soil hydraulic conductivity
      vol_liq  = max(wliq_soisno(1),1.0e-6_r8)/(denh2o*dz_soisno(1))
      s_node   = min(1., vol_liq/porsl(1))

      ! calculate effective soil porosity
      eff_porosity = max(0.01_r8,porsl(1)-min(porsl(1), wice_soisno(1)/(dz_soisno(1)*denice)))


#ifdef Campbell_SOIL_MODEL
      smp_node = (psi0(1)/1000.)*s_node**(-bsw(1))
      hk       = (hksati(1)/1000.)*(vol_liq/porsl(1))**(2.*bsw(1)+3.)

      ! calculate air free pore space
      aird     = porsl(1)*(psi0(1)/-1.e7_r8)**(1./bsw(1))
#endif

#ifdef vanGenuchten_Mualem_SOIL_MODEL
      smp_node = soil_psi_from_vliq (s_node*(porsl(1)-theta_r(1)) + theta_r(1), &
                   porsl(1), theta_r(1), psi0(1), &
                   5, (/alpha_vgm(1), n_vgm(1), L_vgm(1), sc_vgm(1), fc_vgm(1)/))
      hk       = soil_hk_from_psi   (smp_node, psi0(1), hksati(1), &
                   5, (/alpha_vgm(1), n_vgm(1), L_vgm(1), sc_vgm(1), fc_vgm(1)/))

      smp_node = smp_node/1000.
      hk       = hk/1000.

      ! calculate air free pore space
      aird     = soil_vliq_from_psi (-1.e7_r8, porsl(1), theta_r(1), psi0(1), &
                5, (/alpha_vgm(1), n_vgm(1), L_vgm(1), sc_vgm(1), fc_vgm(1)/))
#endif

      ! D0 : 2.12e-5 unit: m2 s-1
      ! ref1: CLM5 Documentation formula (5.81)
      ! ref2: Sakaguchi and Zeng, 2009
      ! ref3: Tang and Riley, 2013. Figure 2, 3, 4, and 5.
      d0  = 2.12e-5*(t_soisno(1)/273.15)**1.75
      eps = porsl(1) - aird


      SELECTCASE (soil_gas_diffusivity_scheme)

      ! 1: BBC
      CASE (1)
#ifdef Campbell_SOIL_MODEL
         tao = eps*eps*(eps/porsl(1))**(3._r8/max(3._r8,bsw(1)))
#endif

      ! 2: P_WLR
      CASE (2)
         tao = 0.66*eps*(eps/porsl(1))

      ! 3: MI_WLR
      CASE (3)
         tao = eps**(4._r8/3._r8)*(eps/porsl(1))

      ! 4: MA_WLR
      CASE (4)
         tao = eps**(3./2.)*(eps/porsl(1))

      ! 5: M_Q
      CASE (5)
         tao = eps**(4._r8/3._r8)*(eps/porsl(1))**(2.0_r8)

      ! 6: 3POE
      CASE (6)
#ifdef Campbell_SOIL_MODEL
         eps100 = porsl(1) - porsl(1)*(psi0(1)/-1000.)**(1./bsw(1))
#endif

#ifdef vanGenuchten_Mualem_SOIL_MODEL
         eps100 = porsl(1) - soil_vliq_from_psi (-1000., porsl(1), theta_r(1), psi0(1), &
                    5, (/alpha_vgm(1), n_vgm(1), L_vgm(1), sc_vgm(1), fc_vgm(1)/))
#endif
         tao    = porsl(1)*porsl(1)*(eps/porsl(1))**(2.+log(eps100**0.25_r8)/log(eps100/porsl(1)))

      ENDSELECT


      ! calculate gas and water diffusivity (dg and dw)
      dg = d0*tao

      !NOTE: dw is only for TR13 scheme
#ifdef Campbell_SOIL_MODEL
      ! TR13, Eq.(A5):
      dw = -hk*bsw(1)*smp_node/vol_liq
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
      ! TR13, Eqs. (A2), (A7), (A8) and (A10):
      ! dw = -hk*(m-1)/(k*m*(theta_s-theta_r))*S**(-1/m)*(1-S**(1/m))**(-m)
      ! where k=alpha_vgm, S=(1+(-k*smp_node)**(n))**(-m), m=m_vgm=1-1/n_vgm
      m_vgm = 1. - 1./n_vgm(1)
      S     = (1. + (- alpha_vgm(1)*smp_node)**(n_vgm(1)))**(-m_vgm)
      dw    = -hk*(m_vgm-1.)/(alpha_vgm(1)*m_vgm*(porsl(1)-theta_r(1))) &
            * S**(-1./m_vgm)*(1.-S**(1./m_vgm))**(-m_vgm)
#endif

      SELECTCASE (DEF_RSS_SCHEME)

!-----------------------------------------------------------------------
      ! calculate rss by SL14
      CASE (1)
         dsl = dz_soisno(1)*max(1.e-6_r8,(0.8*eff_porosity - vol_liq)) &
                           /max(1.e-6_r8,(0.8*porsl(1)- aird))

         dsl = max(dsl,0._r8)
         dsl = min(dsl,0.2_r8)

         rss = dsl/dg

!-----------------------------------------------------------------------
      ! calculate rss by SZ09
      CASE (2)
         dsl = dz_soisno(1)*(exp((1._r8 - vol_liq/porsl(1))**5) - 1._r8)/ (exp(1._r8) - 1._r8)
         dsl = min(dsl,0.2_r8)
         dsl = max(dsl,0._r8)

         rss = dsl/dg

!-----------------------------------------------------------------------
      ! calculate rss by TR13
      CASE (3)
         ! TR13, Eq. (11) and Eq. (12):
         B     = denh2o/(qg*forc_rhoair)
         ! TR13, Eq. (13):
         rg_1  = 2.0_r8*dg*eps/dz_soisno(1)
         rw_1  = 2.0_r8*dw*B*vol_liq/dz_soisno(1)
         rss_1 = rg_1 + rw_1
         rss   = 1.0/rss_1

!-----------------------------------------------------------------------
      ! LP92 beta scheme
      CASE (4)
         wx  = (max(wliq_soisno(1),1.e-6)/denh2o+wice_soisno(1)/denice)/dz_soisno(1)
         fac = min(1._r8, wx/porsl(1))
         fac = max(fac , 0.001_r8)
#ifdef Campbell_SOIL_MODEL
         wfc = porsl(1)*(0.1/(86400.*hksati(1)))**(1./(2.*bsw(1)+3.))
         !NOTE: CoLM wfc = (-339.9/soil_psi_s_l(ipatch))**(-1.0*soil_lambda_l(ipatch))
         !               * soil_theta_s_l(ipatch)
         !wfc = porsl(1)*(-3399._r8/psi0(1))**(-1./bsw(1))
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
         wfc = theta_r(1)+(porsl(1)-theta_r(1))*(1+(alpha_vgm(1)*339.9)**n_vgm(1))**(1.0/n_vgm(1)-1)
#endif

         ! Lee and Pielke 1992 beta
         IF (wx < wfc ) THEN  !when water content of ths top layer is less than that at F.C.
            fac_fc = min(1._r8, wx/wfc)
            fac_fc = max(fac_fc,0.001_r8)
            rss  = 0.25_r8*(1._r8 - cos(fac_fc*3.1415926))**2._r8
         ELSE                 !when water content of ths top layer is more than that at F.C.
            rss  = 1._r8
         ENDIF

!-----------------------------------------------------------------------
      ! Sellers, 1992
      CASE (5)
         wx  = (max(wliq_soisno(1),1.e-6)/denh2o+wice_soisno(1)/denice)/dz_soisno(1)
         fac = min(1._r8, wx/porsl(1))
         fac = max(fac , 0.001_r8)
        !rss = exp(8.206-4.255*fac)   !original Sellers (1992)
         rss = exp(8.206-6.0*fac)     !adjusted Sellers (1992) to decrease rss
                                      !for wet soil according to Noah-MP v5
      ENDSELECT

!-----------------------------------------------------------------------
      ! account for snow fractional cover for rss
      IF (DEF_RSS_SCHEME .ne. 4) THEN
         ! with 1/rss = fsno/rss_snow + (1-fsno)/rss_soil,
         ! assuming rss_snow = 1, so rss is calibrated as:
         IF (1.-fsno+fsno*rss > 0.) THEN
            rss = rss / (1.-fsno+fsno*rss)
         ELSE
            rss = 0.
         ENDIF
         rss = min(1.e6_r8,rss)
      ENDIF

      ! account for snow fractional cover for LP92 beta scheme
      !NOTE: rss here is for soil beta value
      IF (DEF_RSS_SCHEME .eq. 4) THEN
         ! modify soil beta by snow cover, assuming soil beta for snow surface is 1.
         rss = (1.-fsno)*rss + fsno
      ENDIF

   END Subroutine SoilSurfaceResistance

END MODULE MOD_SoilSurfaceResistance
! ---------- EOP ------------
