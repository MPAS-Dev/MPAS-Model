#include <define.h>

MODULE MOD_SimpleOcean

!-----------------------------------------------------------------------
   USE MOD_Precision
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: socean


! PRIVATE MEMBER FUNCTIONS:
   PRIVATE :: seafluxes
   PRIVATE :: srftsb


!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------


   SUBROUTINE socean (dosst,deltim,oro,hu,ht,hq,&
              us,vs,tm,qm,rhoair,psrf,sabg,frl,tssea,tssub,scv,&
              taux,tauy,fsena,fevpa,lfevpa,fseng,fevpg,tref,qref,&
              z0m,zol,rib,ustar,qstar,tstar,fm,fh,fq,fh2m,fq2m,fm10m,emis,olrg)
!-----------------------------------------------------------------------
!            Simple Ocean Model
!  1. calculate sea surface fluxes, based on CLM
!  2. calculate sea surface albedos and seaice/snow temperatures
!                 as in NCAR CCM3.6.16
!  Original authors : Yongjiu Dai and Xin-Zhong Liang (08/30/2001)
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Const_Physical, only: tfrz, hvap, hsub, stefnc, vonkar
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------

   integer, parameter :: psrfty=7  ! Number of surface types
   integer, parameter :: plsice=4  ! number of seaice levels

   logical,  intent(in) :: dosst   ! true to update sst/ice/snow before calculation
   real(r8), intent(in) :: deltim  ! seconds in a time-step (s)
   real(r8), intent(in) :: hu      ! agcm reference height of wind [m]
   real(r8), intent(in) :: ht      ! agcm reference height of temperature [m]
   real(r8), intent(in) :: hq      ! agcm reference height of humidity [m]
   real(r8), intent(in) :: us      ! wind component in eastward direction [m/s]
   real(r8), intent(in) :: vs      ! wind component in northward direction [m/s]
   real(r8), intent(in) :: tm      ! temperature at agcm reference height [kelvin]
   real(r8), intent(in) :: qm      ! specific humidity at agcm reference height [kg/kg]
   real(r8), intent(in) :: rhoair  ! density air [kg/m3]
   real(r8), intent(in) :: psrf    ! atmosphere pressure at the surface [pa] [not used]
   real(r8), intent(in) :: sabg    ! surface solar absorbed flux [W/m2]
   real(r8), intent(in) :: frl     ! downward longwave radiation [W/m2]

   real(r8), intent(inout) :: oro  ! ocean(0)/seaice(2)/ flag
   real(r8), intent(inout) :: scv  ! snow water equivalent depth (mm)
   real(r8), intent(inout) :: tssub(plsice) ! surface/sub-surface temperatures [K]
   real(r8), intent(out) :: tssea  ! sea surface temperature [K]

   real(r8), intent(out) :: taux   ! wind stress: E-W [kg/m/s**2]
   real(r8), intent(out) :: tauy   ! wind stress: N-S [kg/m/s**2]
   real(r8), intent(out) :: fsena  ! sensible heat from reference height to atmosphere [W/m2]
   real(r8), intent(out) :: fevpa  ! evaporation from reference height to atmosphere [mm/s]
   real(r8), intent(out) :: lfevpa ! latent heat from reference height to atmosphere [W/m2]
   real(r8), intent(out) :: fseng  ! sensible heat flux from ground [W/m2]
   real(r8), intent(out) :: fevpg  ! evaporation heat flux from ground [mm/s]

   real(r8), intent(out) :: tref   ! 2 m height air temperature [kelvin]
   real(r8), intent(out) :: qref   ! 2 m height air humidity
   real(r8), intent(out) :: z0m    ! effective roughness [m]
   real(r8), intent(out) :: zol    ! dimensionless height (z/L) used in Monin-Obukhov theory
   real(r8), intent(out) :: rib    ! bulk Richardson number in surface layer
   real(r8), intent(out) :: ustar  ! friction velocity [m/s]
   real(r8), intent(out) :: tstar  ! temperature scaling parameter
   real(r8), intent(out) :: qstar  ! moisture scaling parameter
   real(r8), intent(out) :: fm     ! integral of profile FUNCTION for momentum
   real(r8), intent(out) :: fh     ! integral of profile FUNCTION for heat
   real(r8), intent(out) :: fq     ! integral of profile FUNCTION for moisture
   real(r8), intent(out) :: fh2m   ! relation for temperature at 2m
   real(r8), intent(out) :: fq2m   ! relation for specific humidity at 2m
   real(r8), intent(out) :: fm10m  ! integral of profile FUNCTION for momentum at 10m
   real(r8), intent(out) :: emis   ! averaged bulk surface emissivity
   real(r8), intent(out) :: olrg   ! longwave up flux at surface [W/m2]

!-------------------------- Local Variables ----------------------------
   integer isrfty   ! surface type index (1-7)
   real(r8) cgrndl  ! deriv, of soil sensible heat flux wrt soil temp [w/m2/k]
   real(r8) cgrnds  ! deriv of soil latent heat flux wrt soil temp [w/m**2/k]
   real(r8) dshf    ! Ts partial derivative for sensible heat flux
   real(r8) dlhf    ! Ts partial derivative for latent heat flux
   real(r8) fnt     ! net surface flux for input conditions [W/m2]
   real(r8) dfntdt  ! net surface flux ts partial derivative [W/m2]
   real(r8) tsbsf(plsice)   ! Non-adjusted srfc/sub-srfc temperatures
   real(r8) snowh   ! snow depth (liquid water equivalent) [m]
   real(r8) sicthk  ! sea-ice thickness [m]

   real(r8), parameter :: emisi  = 1.0    ! (0.97) surface emissivity for ice or snow [-]
   real(r8), parameter :: emisw  = 1.0    ! (0.97) surface emissivity for water [-]
   real(r8), parameter :: tsice  = 271.36 ! freezing point of sea ice [K]
   real(r8), parameter :: thsice = 2.0    ! initial thickness of sea ice [m]
   real(r8), parameter :: snsice = 0.005  ! initial snow water equivalent over sea ice [m]

   integer j

!-----------------------------------------------------------------------

      snowh = scv/1000.

      IF(dosst)THEN
! update sea temperatures and sea ice distribution
! as well as snow cover over sea ice
         IF(nint(oro).eq.2 .and. tssea.gt.tsice) THEN
            oro = 0.0         ! old sea ice melt out
            snowh = 0.
            scv = 0.
            sicthk = 0.
            DO j = 1,plsice
               tssub(j) = tssea
            ENDDO
         ELSEIF(nint(oro).eq.0 .and. tssea.le.tsice) THEN
            oro = 2.0         ! new sea ice formed
            snowh = snsice
            scv = snowh*1000.
            sicthk = thsice
            DO j = 1,plsice
               tssub(j) = tssea
            ENDDO
         ENDIF
      ENDIF

      tssea = tssub(1)

! compute surface fluxes, derivatives, and exchange coefficients
      CALL seafluxes (oro,hu,ht,hq,&
                      us,vs,tm,qm,rhoair,psrf,tssea,&
                      taux,tauy,fsena,fevpa,fseng,fevpg,tref,qref,&
                      z0m,zol,rib,ustar,qstar,tstar,fm,fh,fq,fh2m,fq2m,fm10m,cgrndl,cgrnds)

      IF(nint(oro).eq.0)THEN             ! ocean
         lfevpa = fevpa*hvap
         olrg = stefnc*emisw*tssea**4 + (1.-emisw)*frl
         emis = emisw

      ELSEIF(nint(oro).eq.2)THEN        ! sea ice
         lfevpa = fevpa*hsub

       ! net surface flux and derivate at current surface temperature
         dshf = cgrnds
         dlhf = hsub*cgrndl
         olrg = stefnc*emisi*tssea**4 + (1.-emisi)*frl

         fnt = sabg + frl - olrg - fsena - lfevpa
         dfntdt = -(dshf + dlhf) - stefnc*emisi*4.*tssea**3

       ! initialize surface/subsurface temperatures for srftsb
         DO j=1,plsice
           tsbsf(j) = tssub(j)
         ENDDO

       ! set sea ice surface type
         isrfty = 2

       ! diffusion calculation for temperature
         CALL srftsb(isrfty,deltim,fnt,dfntdt,snowh,tsbsf)

         DO j=1,plsice
            tsbsf(j) = min(tsbsf(j),tfrz)
            tssub(j) = tsbsf(j)
         ENDDO
         tssea = tssub(1)

         olrg = stefnc*emisi*tssea**4 + (1.-emisi)*frl
         emis = emisi

      ENDIF

   END SUBROUTINE socean



   SUBROUTINE seafluxes (oro,hu,ht,hq,&
                         us,vs,tm,qm,rhoair,psrf,tssea,&
                         taux,tauy,fsena,fevpa,fseng,fevpg,tref,qref,&
              z0m,zol,rib,ustar,qstar,tstar,fm,fh,fq,fh2m,fq2m,fm10m,cgrndl,cgrnds)

!=======================================================================
!  this is the main SUBROUTINE to execute the calculation of thermal processes
!  and surface fluxes
!
!  Original author: Yongjiu Dai, 09/15/1999; 08/30/2002
!=======================================================================

   USE MOD_Precision
   USE MOD_Const_Physical, only: cpair,rgas,vonkar,grav
   USE MOD_FrictionVelocity
   USE MOD_Qsadv
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------

   real(r8), intent(in) :: &
        oro,      &! ocean(0)/seaice(2)/ flag

        ! atmospherical variables and agcm reference height
        hu,       &! agcm reference height of wind [m]
        ht,       &! agcm reference height of temperature [m]
        hq,       &! agcm reference height of humidity [m]
        us,       &! wind component in eastward direction [m/s]
        vs,       &! wind component in northward direction [m/s]
        tm,       &! temperature at agcm reference height [kelvin]
        qm,       &! specific humidity at agcm reference height [kg/kg]
        rhoair,   &! density air [kg/m3]
        psrf,     &! atmosphere pressure at the surface [pa] [not used]

        tssea      ! sea surface temperature [K]

   real(r8), intent(out) :: &
        taux,     &! wind stress: E-W [kg/m/s**2]
        tauy,     &! wind stress: N-S [kg/m/s**2]
        fsena,    &! sensible heat from agcm reference height to atmosphere [W/m2]
        fevpa,    &! evaporation from agcm reference height to atmosphere [mm/s]
        fseng,    &! sensible heat flux from ground [W/m2]
        fevpg,    &! evaporation heat flux from ground [mm/s]

        tref,     &! 2 m height air temperature [kelvin]
        qref,     &! 2 m height air humidity
        z0m,      &! effective roughness [m]
        zol,      &! dimensionless height (z/L) used in Monin-Obukhov theory
        rib,      &! bulk Richardson number in surface layer
        ustar,    &! friction velocity [m/s]
        tstar,    &! temperature scaling parameter
        qstar,    &! moisture scaling parameter
        fm,       &! integral of profile FUNCTION for momentum
        fh,       &! integral of profile FUNCTION for heat
        fq,       &! integral of profile FUNCTION for moisture
        fh2m,     &! relation for temperature at 2m
        fq2m,     &! relation for specific humidity at 2m
        fm10m,    &! integral of profile FUNCTION for momentum at 10m
        cgrndl,   &! deriv, of soil sensible heat flux wrt soil temp [w/m2/k]
        cgrnds     ! deriv of soil latent heat flux wrt soil temp [w/m**2/k]

!-------------------------- Local Variables ----------------------------
   integer i
   integer niters,&! maximum number of iterations for surface temperature
       iter,      &! iteration index
       nmozsgn     ! number of times moz changes sign

   real(r8) :: &
       beta,      &! coefficient of convective velocity [-]
       displax,   &! zero-displacement height [m]
       dth,       &! diff of virtual temp. between ref. height and surface
       dqh,       &! diff of humidity between ref. height and surface
       dthv,      &! diff of vir. poten. temp. between ref. height and surface
       eg,        &! water vapor pressure at temperature T [Pa]
       degdT,     &! d(eg)/dT
       obu,       &! monin-obukhov length (m)
       obuold,    &! monin-obukhov length from previous iteration
       qsatg,     &! ground saturated specific humidity [kg/kg]
       qsatgdT,   &! d(qsatg)/dT
       ram,       &! aerodynamical resistance [s/m]
       rah,       &! thermal resistance [s/m]
       raw,       &! moisture resistance [s/m]
       raih,      &! temporary variable [kg/m2/s]
       raiw,      &! temporary variable [kg/m2/s]
       thm,       &! intermediate variable (tm+0.0098*ht)
       th,        &! potential temperature (kelvin)
       thv,       &! virtual potential temperature (kelvin)
       thvstar,   &! virtual potential temperature scaling parameter
       um,        &! wind speed including the stablity effect [m/s]
       ur,        &! wind speed at reference height [m/s]
       visa,      &! kinematic viscosity of dry air [m2/s]
       wc,        &! convective velocity [m/s]
       wc2,       &! wc**2
       xt,        &!
       xq,        &!
       zii,       &! convective boundary height [m]
       zldis,     &! reference height "minus" zero displacement height [m]
       z0mg,      &! roughness length over ground, momentum [m]
       z0hg,      &! roughness length over ground, sensible heat [m]
       z0qg        ! roughness length over ground, latent heat [m]

   real, parameter :: zsice = 0.04  ! sea ice aerodynamic roughness length [m]

!-----------------------------------------------------------------------
! potential temperature at the reference height
      beta = 1.      ! -  (in computing W_*)
      zii = 1000.    ! m  (pbl height)

!-----------------------------------------------------------------------
!     Compute sensible and latent fluxes and their derivatives with respect
!     to ground temperature using ground temperatures from previous time step.
!-----------------------------------------------------------------------
! Initialization variables
      nmozsgn = 0
      obuold = 0.

      CALL qsadv(tssea,psrf,eg,degdT,qsatg,qsatgdT)

! potential temperature at the reference height
      thm = tm + 0.0098*ht              ! intermediate variable equivalent to
                                        ! tm*(pgcm/psrf)**(rgas/cpair)
      th = tm*(100000./psrf)**(rgas/cpair) ! potential T
      thv = th*(1.+0.61*qm)             ! virtual potential T
      ur = max(0.1,sqrt(us*us+vs*vs))   ! limit set to 0.1

      dth   = thm-tssea
      dqh   = qm-qsatg
      dthv  = dth*(1.+0.61*qm)+0.61*th*dqh
      zldis = hu-0.

      IF(nint(oro).eq.0)THEN          ! ocean
       ! Kinematic viscosity of dry air (m2/s)- Andreas (1989) CRREL Rep. 89-11
         visa=1.326e-5*(1.+6.542e-3*tm + 8.301e-6*tm**2 - 4.84e-9*tm**3)

       ! loop to obtain initial and good ustar and zo
         ustar=0.06
         wc=0.5
         IF(dthv.ge.0.) THEN
            um=max(ur,0.1)
         ELSE
            um=sqrt(ur*ur+wc*wc)
         ENDIF

         DO i=1,5
            z0mg=0.013*ustar*ustar/grav+0.11*visa/ustar
            ustar=vonkar*um/log(zldis/z0mg)
         ENDDO

      ELSEIF(nint(oro).eq.2)THEN     ! sea ice
         z0mg = zsice
         z0qg = z0mg
         z0hg = z0mg
      ENDIF

      CALL moninobukini(ur,th,thm,thv,dth,dqh,dthv,zldis,z0mg,um,obu)

! Evaluated stability-dependent variables using moz from prior iteration
      niters=10
      displax = 0.

      !----------------------------------------------------------------
      ITERATION : DO iter = 1, niters         ! begin stability iteration
      !----------------------------------------------------------------

         IF(nint(oro).eq.0)THEN   ! ocean
            z0mg=0.013*ustar*ustar/grav + 0.11*visa/ustar
            xq=2.67*(ustar*z0mg/visa)**0.25 - 2.57
            xt= xq
            z0qg=z0mg/exp(xq)
            z0hg=z0mg/exp(xt)
         ENDIF

         CALL moninobuk(hu,ht,hq,displax,z0mg,z0hg,z0qg,obu,um,&
                        ustar,fh2m,fq2m,fm10m,fm,fh,fq)

         tstar = vonkar/fh*dth
         qstar = vonkar/fq*dqh

         thvstar=tstar*(1.+0.61*qm)+0.61*th*qstar
         zol=zldis*vonkar*grav*thvstar/(ustar**2*thv)
         IF(zol >= 0.) THEN       ! stable
           zol = min(2.,max(zol,1.e-6))
         ELSE                     ! unstable
           zol = max(-100.,min(zol,-1.e-6))
         ENDIF
         obu = zldis/zol

         IF(zol >= 0.)THEN
           um = max(ur,0.1)
         ELSE
           wc = (-grav*ustar*thvstar*zii/thv)**(1./3.)
          wc2 = beta*beta*(wc*wc)
           um = sqrt(ur*ur+wc2)
         ENDIF

         IF (obuold*obu < 0.) nmozsgn = nmozsgn+1
         IF(nmozsgn >= 4) EXIT

         obuold = obu

      !----------------------------------------------------------------
      ENDDO ITERATION                         ! END stability iteration
      !----------------------------------------------------------------

! Get derivative of fluxes with respect to ground temperature
      ram    = 1./(ustar*ustar/um)
      rah    = 1./(vonkar/fh*ustar)
      raw    = 1./(vonkar/fq*ustar)

      raih   = rhoair*cpair/rah
      raiw   = rhoair/raw
      cgrnds = raih
      cgrndl = raiw*qsatgdT

      rib = min(5.,zol*ustar**2/(vonkar**2/fh*um**2))

! surface fluxes of momentum, sensible and latent
! using ground temperatures from previous time step
      taux   = -rhoair*us/ram
      tauy   = -rhoair*vs/ram

      fseng  = -raih*dth
      fevpg  = -raiw*dqh
      fsena  = fseng
      fevpa  = fevpg

! 2 m height air temperature
      tref   = thm + vonkar/fh*dth * (fh2m/vonkar - fh/vonkar)
      qref   = qm + vonkar/fq*dqh * (fq2m/vonkar - fq/vonkar)
      z0m   = z0mg

   END SUBROUTINE seafluxes



   SUBROUTINE srftsb(isrfty,deltim,fnt,dfntdt,snowh,tsbsf)

!-----------------------------------------------------------------------
!  Compute surface and subsurface temperatures over sea-ice surfaces.
!
!  Sea ice temperatures are specified in 'plsice' layers of fixed
!  thickness and thermal properties.  The forecast temperatures are
!  determined from a backward/IMPLICIT diffusion calculation using
!  linearized sensible/latent heat fluxes. The bottom ocean temperature
!  is fixed at -2C, allowing heat flux exchange with underlying ocean.
!
!  Sub-surface layers are indexed 1 at the surface, increasing downwards
!  to plsice.  Layers have mid-points and interfaces between layers.
!
!  Temperatures are defined at mid-points, WHILE fluxes between layers
!  and the top/bottom media are defined at layer interfaces.
!
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Const_Physical, only: tkice, tkair
   USE MOD_Utils
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------

   integer, parameter :: psrfty = 7  ! Number of surface types
   integer, parameter :: plsice = 4  ! number of seaice levels

   integer, intent(in) :: isrfty  ! surface type index (1 - 7)
   real(r8), intent(in) :: deltim ! seconds i a time step (s)
   real(r8), intent(in) :: fnt    ! top surface/atmosphere net energy flux
   real(r8), intent(in) :: dfntdt ! ts partial derivative of net sfc flux
   real(r8), intent(in) :: snowh  ! snow depth (liquid water equivalent) [m]

   real(r8), intent(inout) :: tsbsf(1:plsice) ! surface/sub-surface tmps

!-------------------------- Local Variables ----------------------------

   integer :: j, jndx        ! sub-surface layer index

   real(r8) cmass (1:plsice) ! specific heat of soil (J/kg/K)
   real(r8) rho   (1:plsice) ! mass densty of sub-sfc mat (kg/m3)
   real(r8) tk    (1:plsice) ! thermal conductivity (watts/m/K)
   real(r8) diag  (1:plsice) ! diagonal matrix elements
   real(r8) htsrc (1:plsice) ! external heat source (W/m3)
   real(r8) rhs   (1:plsice) ! rhs of tri-diagonal matrix equation
   real(r8) sbdiag(1:plsice) ! sub-diagonal matrix elements
   real(r8) spdiag(1:plsice) ! super-diagonal matrix elements
   real(r8) tin   (1:plsice) ! initial sub-surface temperatures
   real(r8) z     (0:plsice) ! interface geometrical depth (m)
   real(r8) ws    (1:plsice) ! working storage for mtdlss

   real(r8) cmty   ! layer mass heat capacity
   real(r8) fbt    ! ocean heat flux into sea-ice
   real(r8) rhty   ! layer mass density
   real(r8) thck   ! layer thickness
   real(r8) tkty   ! layer thermal conductivity
   real(r8) cmsnow ! Snow mass heat capacity
   real(r8) crt    ! cmass*rho*rdtime
   real(r8) delz   ! layer thickness
   real(r8) delzmn ! thick from mid-point to lyr above mid-point
   real(r8) delzpl ! thick from mid-point to lyr below mid-point
   real(r8) fmns   ! 1/(delz*delzmn)
   real(r8) fpls   ! 1/(delz*delzpl)
   real(r8) msnow  ! mass path of snow
   real(r8) mlice  ! mass path of ice
   real(r8) rdtime ! inverse model time step
   real(r8) rhsnow ! snow mass density
   real(r8) rztop  ! 1/ztop
   real(r8) tkbot  ! bottom layer top interf thermal conduct
   real(r8) tkmns  ! layer bottom interface thermal conduct
   real(r8) tkpls  ! layer top interface thermal conductivity
   real(r8) tksnow ! snow thermal conductivity
   real(r8) tktop  ! top layer bottom interface thermal conduct
   real(r8) tmp    ! crt - dfntdt(i)*rztop
   real(r8) zbot   ! bottom layer thickness
   real(r8) zm     ! present layer mid-point depth
   real(r8) zmmn   ! layer above mid-point depth
   real(r8) zmpl   ! layer below mid-point depth
   real(r8) zsnow  ! snow geometric depth
   real(r8) ztop   ! top layer thickness
   logical scvr    ! true IF surface snow covered

!--------------------------Data Statements------------------------------
! specified (and invariant) thermal properties for surface types

   real, parameter :: cmair  = 1.00e3 ! mass specific heat of air [J/kg/K]
   real, parameter :: cmice  = 2.07e3 ! mass specific heat of ice [J/kg/K]
   real, parameter :: frcair = 0.90   ! fraction of air assumed in mix of ice
   real, parameter :: rhair  = 1.25   ! mass density of surface air [kg/m3]
   real, parameter :: rhice  = 9.20e2 ! mass density of ice [kg/m3]
   real, parameter :: snwedp = 10.0   ! snow:water equivalent depth factor [-]

   real(r8),parameter,dimension(psrfty,plsice) :: &!mass specific heat (J/kg/K)
   cmtype = reshape(&
          (/4.20e3,2.07e3,2.07e3,1.04e3,7.20e2,5.60e2,4.16e2,&
            4.20e3,2.07e3,2.07e3,1.04e3,7.20e2,5.60e2,4.16e2,&
            4.20e3,2.07e3,2.07e3,1.04e3,7.20e2,5.60e2,4.16e2,&
            4.20e3,2.07e3,2.07e3,1.04e3,7.20e2,5.60e2,4.16e2/), (/7,4/))

   real(r8),parameter,dimension(psrfty,plsice) :: &! mass density (kg/m3)
   rhtype = reshape(&
          (/1.00e3,9.20e2,9.20e2,2.50e3,2.50e3,2.50e3,2.50e3,&
            1.00e3,9.20e2,9.20e2,2.50e3,2.50e3,2.50e3,2.50e3,&
            1.00e3,9.20e2,9.20e2,2.50e3,2.50e3,2.50e3,2.50e3,&
            1.00e3,9.20e2,9.20e2,2.50e3,2.50e3,2.50e3,2.50e3/),(/7,4/))

   real(r8),parameter,dimension(psrfty,plsice) :: &!layer thicknesses (m)
   thckly = reshape(&
          (/ 2., .500, .250, .050, .090, .080, .120, &
             5., .500, .500, .366, .390, .435, .492, &
            10., .500, .500,1.369,1.459,1.628,1.841, &
            33., .500,8.500,6.990,7.450,8.310,9.400/), (/7,4/))

   real(r8),parameter,dimension(psrfty,plsice) :: &!thermal conductivity (W/m/K)
   tktype = reshape(&
          (/15.0 ,2.200 ,2.200 ,1.408 ,1.104 ,1.071 ,1.019 , &
            15.0 ,2.200 ,2.200 ,1.408 ,1.104 ,1.071 ,1.019 , &
            15.0 ,2.200 ,2.200 ,1.408 ,1.104 ,1.071 ,1.019 , &
            15.0 ,2.200 ,2.200 ,1.408 ,1.104 ,1.071 ,1.019 /), (/7,4/))

!-----------------------------------------------------------------------

      rdtime = 1./deltim

! calculate snow properties
      cmsnow = (1.-frcair)*cmice + frcair*cmair
      rhsnow = (1.-frcair)*rhice + frcair*rhair
      tksnow = (1.-frcair)*tkice + frcair*tkair

! no external heat source
      DO j=1,plsice
         htsrc(j) = 0.0
      ENDDO

! define logical for snow covered surfaces:
      scvr = snowh.gt.0.

! define thermal properties for each sub/surface layer, starting
! with the top layer
      jndx    = isrfty
      thck = thckly(jndx,1)
      cmty = cmtype(jndx,1)
      rhty = rhtype(jndx,1)
      tkty = tktype(jndx,1)

! initialize fields for no snow cover
      z(0)     = 0.0
      z(1)     = thck
      cmass(1) = cmty
      rho(1)   = rhty
      tk(1)    = tkty

! modify layer 1 fields for snow cover IF present
! snow equivalent depth times snow liquid water depth gives the physical
! depth of snow for thermal conduction computation; snow is mixed
! uniformly by mass with the top surface layer
      IF(scvr) THEN
        zsnow    = snowh*snwedp
        msnow    = rhsnow*zsnow
        mlice    = rhty*thck
        rho(1)   = (msnow*rhsnow + mlice*rhty)/(msnow+mlice)
        cmass(1) = (msnow*cmsnow + mlice*cmty)/(msnow+mlice)
        tk(1)    = (msnow*tksnow + mlice*tkty)/(msnow+mlice)
        z(1)     = (msnow+mlice) / rho(1)
      ENDIF

! set surface thermal properties for the lower sub/surface layers:
      DO j=2,plsice
         jndx     = isrfty
         thck     = thckly(jndx,j)
         cmass(j) = cmtype(jndx,j)
         rho(j)   = rhtype(jndx,j)
         tk(j)    = tktype(jndx,j)
         z(j)     = z(j-1) + thck
      ENDDO

! define set of linear equations for temperature
      DO j=1,plsice
         tin(j) = tsbsf(j)
      ENDDO

! IF sea ice, compute heat flux from underlying ocean, assumed to be at
! the temperature of -2C
      fbt = 0.0
      IF(isrfty.eq.2) THEN
         zbot = 0.5*(z(plsice) - z(plsice-1))
         fbt = -tk(plsice)*(271.16 - tin(plsice))/zbot
      ENDIF

! set up linear equations
      sbdiag(1)      = 0.
      spdiag(plsice) = 0.

! single layer
      IF (plsice.eq.1) THEN
         rztop = 1./(z(1) - z(0))
         crt = (cmass(1)*rho(1)*rdtime)
         diag(1) = crt - dfntdt*rztop
         rhs(1) = diag(1)*tin(1) + fnt*rztop - fbt*rztop + htsrc(1)

! more than one layer: top layer first
      ELSEIF (plsice.gt.1) THEN

         crt       = cmass(1)*rho(1)*rdtime
         ztop      = z(1) - z(0)
         rztop     = 1./ztop
         tktop     = 0.5*(tk(1) + tk(2))
         zmpl      = 0.5*(z(2) + z(1))
         zm        = 0.5*(z(1) + z(0))
         delzpl    = zmpl - zm
         fpls      = 1./(ztop*delzpl)
         tmp       = crt - dfntdt*rztop

         diag(1)   = tmp + tktop*fpls
         spdiag(1) = -tktop*fpls
         rhs(1)    = tmp*tin(1) + fnt*rztop + htsrc(1)

! intermediate layers
         DO j=2,plsice-1
            crt       = cmass(j)*rho(j)*rdtime
            delz      = z(j) - z(j-1)
            zmpl      = 0.5*(z(j+1) + z(j))
            zm        = 0.5*(z(j)   + z(j-1))
            zmmn      = 0.5*(z(j-1) + z(j-2))
            delzpl    = zmpl - zm
            delzmn    = zm - zmmn
            fpls      = 1./(delz*delzpl)
            fmns      = 1./(delz*delzmn)
            tkpls     = 0.5*(tk(j+1)+tk(j))
            tkmns     = 0.5*(tk(j)+tk(j-1))

            sbdiag(j) = -tkmns*fmns
            diag(j)   = crt + (tkpls*fpls + tkmns*fmns)
            spdiag(j) = -tkpls*fpls
            rhs(j)    = crt*tin(j) + htsrc(j)
         ENDDO

! bottom layer
            crt       = cmass(plsice)*rho(plsice)*rdtime
            zbot      = z(plsice) - z(plsice-1)
            zm        = 0.5*(z(plsice)   + z(plsice-1))
            zmmn      = 0.5*(z(plsice-1) + z(plsice-2))
            delzmn    = zm - zmmn
            tkbot     = 0.5*(tk(plsice-1) + tk(plsice))
            fmns      = 1./(zbot*delzmn)
            sbdiag(plsice) = -tkbot*fmns
            diag(plsice) = crt + (tkbot*fmns)
            rhs(plsice) = crt*tin(plsice) - fbt/zbot + htsrc(plsice)
      ENDIF

      IF(plsice.eq.1) THEN
         tsbsf(1) = rhs(1)/diag(1)
      ELSE
         CALL tridia (plsice,sbdiag,diag,spdiag,rhs,tsbsf)
      ENDIF

   END SUBROUTINE srftsb

END MODULE MOD_SimpleOcean
! ---------- EOP ------------
