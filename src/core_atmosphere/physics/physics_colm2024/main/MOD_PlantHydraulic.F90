
MODULE MOD_PlantHydraulic

!-----------------------------------------------------------------------
   USE MOD_Precision
   USE MOD_Namelist, only: DEF_RSS_SCHEME
   USE MOD_MPAS_MPI
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: PlantHydraulicStress_twoleaf
   PUBLIC :: getvegwp_twoleaf

! PRIVATE MEMBER FUNCTIONS:
   PRIVATE :: calcstress_twoleaf


!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------



   SUBROUTINE PlantHydraulicStress_twoleaf (nl_soil    ,nvegwcs    ,z_soi      ,&
                      dz_soi    ,rootfr    ,psrf       ,qsatl      ,&
                      qaf       ,tl        ,rb         ,rss        ,&
                      ra        ,rd        ,rstfacsun  ,rstfacsha  ,cintsun    ,&
                      cintsha   ,laisun    ,laisha     ,rhoair     ,fwet       ,&
                      sai       ,kmax_sun  ,kmax_sha   ,kmax_xyl   ,kmax_root  ,&
                      psi50_sun ,psi50_sha ,psi50_xyl  ,psi50_root ,htop       ,&
                      ck        ,smp       ,hk         ,hksati     ,vegwp      ,&
                      etrsun    ,etrsha    ,rootflux   ,qg         ,&
                      qm        ,gs0sun    ,gs0sha     ,k_soil_root,k_ax_root  ,&
                      gssun     ,gssha)

!=======================================================================
!
!  calculation of plant hydraulic stress
!
!  Author: Xingjie Lu, 16/01/2019, modified from CLM5 plant_hydraulic_stress module
!
!----------------------------------------------------------------------

   USE MOD_Precision
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer ,intent(in) :: nl_soil ! upper bound of array
   integer ,intent(in) :: nvegwcs ! upper bound of array
   real(r8),intent(in), dimension(nl_soil) :: &
       z_soi,      &! soil node depth (m)
       dz_soi       ! soil layer thicknesses (m)
   real(r8),intent(inout), dimension(nvegwcs) :: &
       vegwp        ! vegetation water potential
   real(r8),intent(inout):: &
       gs0sun,    & ! maximum stomata conductance of sunlit leaf
       gs0sha       ! maximum stomata conductance of shaded leaf

   real(r8),intent(in) :: &
       rss,          &! soil surface resistance [s/m]
       psrf,         &! surface atmospheric pressure (pa)
       qg,           &! specific humidity at ground surface [kg/kg]
       qm             ! specific humidity at reference height [kg/kg]

   real(r8),intent(in) :: &
       qsatl,        &! leaf specific humidity [kg/kg]
       qaf,          &! humidity of canopy air [kg/kg]
       tl,           &! leaf temperature (K)

       rb,           &! boundary resistance from canopy to cas (s m-1)
       rd,           &! aerodynamical resistance between ground and canopy air
       ra             ! aerodynamic resistance from cas to reference height (s m-1)

   real(r8),intent(inout) :: &
       rstfacsun,    &! canopy resistance stress factors to soil moisture for sunlit leaf
       rstfacsha      ! canopy resistance stress factors to soil moisture for shaded leaf

   real(r8),intent(in) :: &
       laisun,       &! sunlit leaf area index, one-sided
       laisha,       &! shaded leaf area index, one-sided
       sai,          &! stem area index
       kmax_sun,     &
       kmax_sha,     &
       kmax_xyl,     &
       kmax_root,    &
       psi50_sun,    &! water potential at 50% loss of sunlit leaf tissue conductance (mmH2O)
       psi50_sha,    &! water potential at 50% loss of shaded leaf tissue conductance (mmH2O)
       psi50_xyl,    &! water potential at 50% loss of xylem tissue conductance (mmH2O)
       psi50_root,   &! water potential at 50% loss of root tissue conductance (mmH2O)
       htop,         &! canopy top [m]
       ck,           &! shape-fitting parameter for vulnerability curve (-)
       rhoair,       &! density [kg/m**3]
       fwet           ! fraction of foliage that is wet [-]

   real(r8),intent(in), dimension(3) :: &
       cintsun,      &! scaling up from sunlit leaf to canopy
       cintsha        ! scaling up from shaded leaf to canopy

   real(r8),intent(in), dimension(nl_soil) :: &
       smp,          &! soil matrix potential
       rootfr,       &! root fraction
       hksati,       &! hydraulic conductivity at saturation [mm h2o/s]
       hk             ! soil hydraulic conductance [mm h2o/s]


   real(r8),intent(out) :: &! ATTENTION : all for canopy not leaf
       etrsun,       &! transpiration from sunlit leaf (mm/s)
       etrsha         ! transpiration from shaded leaf (mm/s)

   real(r8),intent(out),dimension(nl_soil) :: &
       rootflux       ! root water uptake from different layers

   real(r8),intent(inout),dimension(nl_soil) :: k_soil_root  ! radial root and soil conductance
   real(r8),intent(inout),dimension(nl_soil) :: k_ax_root    ! axial root conductance
   real(r8),intent(inout) :: gssun                           ! sunlit leaf conductance
   real(r8),intent(inout) :: gssha                           ! shaded leaf conductance

!-------------------------- Local Variables ----------------------------

   integer, parameter :: iterationtotal = 6

   real(r8) c3,       &! c3 vegetation : 1; 0 for c4

        tprcor,       &! coefficient for unit transfer
        gb_mol         ! one side leaf boundary layer conductance of sunlit leaf (leaf scale:umol H2O m-2 s-1)

   real(r8), dimension(nl_soil) :: &
       fs     !root conductance scale factor (reduction in conductance due to decreasing (more negative) root water potential)
   real(r8), dimension(nl_soil) :: &
       rai    ! soil-root interface conductance [mm/s]

   real(r8) soilflux              ! soil-root interface conductance [mm/s]
   real(r8) soil_conductance      ! soil conductance
   real(r8) root_conductance      ! root conductance
   real(r8) r_soil                ! root spacing [m]
   real(r8) root_biomass_density  ! root biomass density [g/m3]
   real(r8) root_cross_sec_area   ! root cross sectional area [m2]
   real(r8) root_length_density   ! root length density [m/m3]
   real(r8) croot_average_length  ! average coarse root length [m]
   real(r8) rs_resis              ! combined soil-root resistance [s]
   real(r8) cf                    ! s m**2/umol -> s/m

   real(r8), parameter :: croot_lateral_length = 0.25_r8   ! specified lateral coarse root length [m]
   real(r8), parameter :: c_to_b               = 2.0_r8    ! (g biomass /g C)
   real(r8), parameter :: rpi                  = 3.14159265358979_r8
   integer , parameter :: root                 = 4
   real(r8), parameter :: toldb                = 1.e-2_r8  ! tolerance for satisfactory bsun/bsha solution
   real(r8), parameter :: K_axs                = 2.0e-1

 ! temporary input
   real(r8), parameter :: froot_carbon = 288.392056287006_r8
   real(r8), parameter :: root_radius  = 2.9e-4_r8
   real(r8), parameter :: root_density = 310000._r8
   real(r8), parameter :: froot_leaf   = 1.5_r8
   real(r8), parameter :: krmax        = 3.981071705534969e-009_r8

   real(r8),dimension(nvegwcs) :: x      ! vegetation water potential

   integer j

!----------------calculate root-soil interface conductance-----------------
      DO j = 1,nl_soil

         ! calculate conversion from conductivity to conductance
         root_biomass_density = c_to_b * froot_carbon * rootfr(j) / dz_soi(j)
         ! ensure minimum root biomass (using 1gC/m2)
         root_biomass_density = max(c_to_b*1._r8,root_biomass_density)

         ! Root length density: m root per m3 soil
         root_cross_sec_area = rpi*root_radius**2
         root_length_density = root_biomass_density / (root_density * root_cross_sec_area)

         ! Root-area index (RAI)
         rai(j) = (sai+laisun+laisha) * froot_leaf * rootfr(j)

         ! fix coarse root_average_length to specified length
         croot_average_length = croot_lateral_length

         ! calculate r_soil using Gardner/spa equation (Bonan, GMD, 2014)
         r_soil = sqrt(1./(rpi*root_length_density))

         ! length scale approach
         soil_conductance = min(hksati(j),hk(j))/(1.e3*r_soil)

         ! USE vegetation plc function to adjust root conductance
         fs(j)=  plc(amax1(smp(j),-1._r8),psi50_root,ck)

         ! krmax is root conductance per area per length
         root_conductance = (fs(j)*rai(j)*krmax)/(croot_average_length + z_soi(j))
         soil_conductance = max(soil_conductance, 1.e-16_r8)
         root_conductance = max(root_conductance, 1.e-16_r8)

         ! sum resistances in soil and root
         rs_resis = 1._r8/soil_conductance + 1._r8/root_conductance

         ! conductance is inverse resistance
         ! explicitly set conductance to zero for top soil layer
         IF(rai(j)*rootfr(j) > 0._r8) THEN
            k_soil_root(j) =  1._r8/rs_resis
         ELSE
            k_soil_root(j) =  0.
         ENDIF
         k_ax_root(j) = (rootfr(j)/(dz_soi(j)*1000))*K_axs*0.6
      ENDDO
!=======================================================================

      tprcor = 44.6*273.16*psrf/1.013e5
      cf     = tprcor/tl * 1.e6_r8  ! gb->gbmol conversion factor

      ! one side leaf boundary layer conductance for water vapor [=1/(2*rb)]
      ! ATTENTION: rb in CLM is for one side leaf, but for SiB2 rb for
      ! 2-side leaf, so the gbh2o shold be " 0.5/rb * tprcor/tl "
      gb_mol = 1./rb * cf  ! resistance to conductance (s/m -> umol/m**2/s)

      x = vegwp(1:nvegwcs)

      CALL calcstress_twoleaf(x, nvegwcs, rstfacsun, rstfacsha, etrsun, etrsha, rootflux,&
              gb_mol, gs0sun, gs0sha, qsatl, qaf, qg, qm, rhoair, &
              psrf, fwet, laisun, laisha, sai, htop, tl, kmax_sun, &
              kmax_sha, kmax_xyl, kmax_root, psi50_sun, psi50_sha, psi50_xyl, psi50_root, ck, &
              nl_soil, z_soi, rss, ra, rd, smp, k_soil_root, k_ax_root, gssun, gssha)

      vegwp(1:nvegwcs) = x

   END SUBROUTINE PlantHydraulicStress_twoleaf

   SUBROUTINE calcstress_twoleaf(x,nvegwcs,rstfacsun, rstfacsha, etrsun, etrsha, rootflux,&
              gb_mol, gs0sun, gs0sha, qsatl, qaf, qg, qm,rhoair,&
              psrf, fwet, laisun, laisha, sai, htop, tl, kmax_sun, kmax_sha, kmax_xyl, kmax_root, &
              psi50_sun, psi50_sha, psi50_xyl, psi50_root, ck, nl_soil, z_soi, rss, raw, rd, smp, &
              k_soil_root, k_ax_root, gssun, gssha)
   !
   ! DESCRIPTIONS
   ! compute the transpiration stress using a plant hydraulics approach
   ! calls spacF, spacA, and getvegwp
   !
   ! !ARGUMENTS:
   integer,  intent(in)    :: nvegwcs
   real(r8), intent(inout) :: x(nvegwcs)         ! working copy of vegwp(p,:)
   real(r8), intent(out)   :: rstfacsun          ! sunlit canopy transpiration wetness factor (0 to 1)
   real(r8), intent(out)   :: rstfacsha          ! shaded sunlit canopy transpiration wetness factor (0 to 1)
   real(r8), intent(out)   :: etrsun             ! transpiration from sunlit leaf (mm/s)
   real(r8), intent(out)   :: etrsha             ! transpiration from shaded leaf (mm/s)
   real(r8), intent(out)   :: rootflux(nl_soil)  ! root water uptake from different layers

   integer,  intent(in)    :: nl_soil
   real(r8), intent(in)    :: z_soi(nl_soil)
   real(r8), intent(in)    :: gb_mol             ! leaf boundary layer conductance (umol H2O/m**2/s)
   real(r8), intent(in)    :: gs0sun             ! sunlit Ball-Berry minimum leaf conductance (umol H2O/m**2/s)
   real(r8), intent(in)    :: gs0sha             ! shaded Ball-Berry minimum leaf conductance (umol H2O/m**2/s)
   real(r8), intent(in)    :: qsatl              ! leaf specific humidity [kg/kg]
   real(r8), intent(in)    :: qaf                ! humidity of canopy air [kg/kg]
   real(r8), intent(in)    :: qg                 ! specific humidity at ground surface [kg/kg]
   real(r8), intent(in)    :: qm                 ! specific humidity at reference height [kg/kg]
   real(r8), intent(in)    :: rhoair             ! density [kg/m**3]
   real(r8), intent(in)    :: psrf               ! atmospheric pressure [Pa]
   real(r8), intent(in)    :: fwet               ! fraction of foliage that is green and dry [-]
   real(r8), intent(in)    :: rss                ! soil surface resistance [s/m]
   real(r8), intent(in)    :: raw                ! moisture resistance [s/m]
   real(r8), intent(in)    :: rd                 ! aerodynamical resistance between ground and canopy air
   real(r8), intent(in)    :: laisun             ! Sunlit leaf area index
   real(r8), intent(in)    :: laisha             ! Shaded leaf area index
   real(r8), intent(in)    :: sai                ! stem area index
   real(r8), intent(in)    :: htop               ! canopy top [m]
   real(r8), intent(in)    :: tl                 ! leaf temperature
   real(r8), intent(in)    :: kmax_sun
   real(r8), intent(in)    :: kmax_sha
   real(r8), intent(in)    :: kmax_xyl
   real(r8), intent(in)    :: kmax_root
   real(r8), intent(in)    :: psi50_sun          ! water potential at 50% loss of sunlit leaf tissue conductance (mmH2O)
   real(r8), intent(in)    :: psi50_sha          ! water potential at 50% loss of shaded leaf tissue conductance (mmH2O)
   real(r8), intent(in)    :: psi50_xyl          ! water potential at 50% loss of xylem tissue conductance (mmH2O)
   real(r8), intent(in)    :: psi50_root         ! water potential at 50% loss of root tissue conductance (mmH2O)
   real(r8), intent(in)    :: ck                 !
   real(r8), intent(in)    :: smp(nl_soil)       ! soil matrix potential
   real(r8), intent(in)    :: k_soil_root(nl_soil)   ! soil-root interface conductance [mm/s]
   real(r8), intent(in)    :: k_ax_root(nl_soil) ! root axial-direction conductance [mm/s]
   real(r8), intent(out)   :: gssun              ! sunlit leaf conductance
   real(r8), intent(out)   :: gssha              ! shaded leaf conductance


   real(r8) :: wtl                   ! water conductance for leaf [m/s]
   real(r8) :: A(nvegwcs,nvegwcs)    ! matrix relating d(vegwp) and f: d(vegwp)=A*f
   real(r8) :: f(nvegwcs)            ! flux divergence (mm/s)
   real(r8) :: dx(nvegwcs)           ! change in vegwp from one iter to the next [mm]
   real(r8) :: qflx_sun              ! [kg/m2/s]
   real(r8) :: qflx_sha              ! [kg/m2/s]
   real(r8) :: qeroot,dqeroot
   real(r8),dimension(nl_soil) :: xroot        ! local gs_mol copies
   integer  :: i,j                   ! index
   real(r8) :: cf                    ! s m**2/umol -> s/m
   integer  :: iter,iterqflx         ! newton's method iteration number
   logical  :: flag                  ! signal that matrix was not invertible
   logical  :: night                 ! signal to store vegwp within this routine, b/c it is night-time and full suite won't be called
   integer, parameter :: itmax=50    ! EXIT newton's method IF iters>itmax
   real(r8),parameter :: toldx=1.e-9 !tolerances for a satisfactory solution
   real(r8),parameter :: tolf         = 1.e-6_r8
   real(r8),parameter :: tolf_leafxyl = 1.e-16_r8
   real(r8),parameter :: tolf_root    = 1.e-14_r8 !tolerances for a satisfactory solution
   logical  :: havegs                ! signals direction of calculation gs->qflx or qflx->gs
   logical  :: haroot                ! signals direction of calculation x_root_top->qeroot or qeroot->x_root_top
   real(r8) :: soilflux              ! total soil column transpiration [mm/s]
   real(r8) :: x_root_top
   real(r8) :: x_root_top1
   real(r8) :: x_root_top2
   real(r8) :: dxsoiltop
   real(r8) :: maxscale
   real(r8), parameter :: tol_lai=1.e-7_r8 ! minimum lai WHERE transpiration is calc'd
   integer, parameter :: leafsun=1
   integer, parameter :: leafsha=2
   integer, parameter :: xyl=3
   integer, parameter :: root=4
   real(r8) fsto1,fsto2,fx,fr,grav1
   real(r8) tprcor
   !------------------------------------------------------------------------------

      !temporary flag for night time vegwp(sun)>0

      gssun=gs0sun
      gssha=gs0sha
      CALL getqflx_gs2qflx_twoleaf(gb_mol,gssun,gssha,qflx_sun,qflx_sha,qsatl,qaf,&
                                   rhoair,psrf,laisun,laisha,sai,fwet,tl,rss,raw,rd,qg,qm)
      x_root_top  = x(root)

      IF(qflx_sun .gt. 0 .or. qflx_sha .gt. 0)THEN
         CALL getrootqflx_x2qe(nl_soil,smp,x_root_top ,z_soi,k_soil_root,k_ax_root,qeroot,dqeroot)

         CALL spacAF_twoleaf(x,nvegwcs,dx,nl_soil,qflx_sun,qflx_sha,laisun,laisha,sai,htop,&
                 qeroot,dqeroot,kmax_sun,kmax_sha,kmax_xyl,kmax_root,&
                 psi50_sun,psi50_sha,psi50_xyl,psi50_root,ck)

         IF ( maxval(abs(dx)) > 200000._r8) THEN
            maxscale = min(maxval(abs(dx)),maxval(abs(x))) / 2
            dx = maxscale * dx / maxval(abs(dx))! * log(maxval(abs(dx))/maxscale) !rescale step to max of 50000
         ENDIF

         x=x+dx

      ! this is a catch to force spac gradient to atmosphere
         IF ( x(xyl) > x(root) ) x(xyl) = x(root)
         IF ( x(leafsun) > x(xyl) )  x(leafsun) = x(xyl)
         IF ( x(leafsha) > x(xyl) )  x(leafsha) = x(xyl)

      ! compute attenuated flux; the actual transpiration
         etrsun=qflx_sun*plc(x(leafsun),psi50_sun,ck)
         etrsha=qflx_sha*plc(x(leafsha),psi50_sha,ck)

      ! retrieve stressed stomatal conductance
         CALL getqflx_qflx2gs_twoleaf(gb_mol,gssun,gssha,etrsun,etrsha,qsatl,qaf,&
                                      rhoair,psrf,laisun,laisha,sai,fwet,tl,rss,raw,rd,qg,qm)

         tprcor   = 44.6*273.16*psrf/1.013e5
      ! compute water stress
      ! .. generally -> B= gs_stressed / gs_unstressed
      ! .. when gs=0 -> B= plc( x )
         rstfacsun = amax1(gssun/gs0sun,1.e-2_r8)
         rstfacsha = amax1(gssha/gs0sha,1.e-2_r8)
         qeroot = etrsun + etrsha
         CALL getrootqflx_qe2x(nl_soil,smp,z_soi,k_soil_root,k_ax_root,qeroot,xroot,x_root_top)
         x(root) = x_root_top
         DO j = 1,nl_soil
            rootflux(j) = k_soil_root(j)*(smp(j)-xroot(j))
         ENDDO
      ELSE
         IF ( x(xyl) > x(root) ) x(xyl) = x(root)
         IF ( x(leafsun) > x(xyl) )  x(leafsun) = x(xyl)
         IF ( x(leafsha) > x(xyl) )  x(leafsha) = x(xyl)
         etrsun = 0._r8
         etrsha = 0._r8
         rstfacsun = amax1(plc(x(leafsun),psi50_sun,ck),1.e-2_r8)
         rstfacsha = amax1(plc(x(leafsha),psi50_sha,ck),1.e-2_r8)
         gssun = gs0sun * rstfacsun
         gssha = gs0sha * rstfacsha
         rootflux = 0._r8
      ENDIF

      soilflux = sum(rootflux(:))

   END SUBROUTINE calcstress_twoleaf

   !------------------------------------------------------------------------------
   SUBROUTINE spacAF_twoleaf(x,nvegwcs,dx,nl_soil,qflx_sun,qflx_sha,laisun,laisha,sai,htop,&
                   qeroot,dqeroot,kmax_sun,kmax_sha,kmax_xyl,kmax_root,&
                   psi50_sun,psi50_sha,psi50_xyl,psi50_root,ck)
!-----------------------------------------------------------------------
! !DESCRIPTION
!  Returns invA, the inverse matrix relating delta(vegwp) to f
!   d(vegwp)=invA*f
!   evaluated at vegwp(p)
!
!  The methodology is currently hardcoded for linear algebra assuming the
!  number of vegetation segments is four. Thus the matrix A and it's inverse
!  invA are both 4x4 matrices. A more general method could be done using for
!  example a LINPACK linear algebra solver.
!
!-----------------------------------------------------------------------
! !ARGUMENTS:
   integer , intent(in)  :: nvegwcs
   real(r8), intent(in)  :: x(nvegwcs)  ! working copy of veg water potential for patch p [mm H2O]
   real(r8), intent(out) :: dx(nvegwcs) ! matrix relating d(vegwp) and f: d(vegwp)=invA*f
   integer , intent(in)  :: nl_soil
   real(r8), intent(in)  :: qflx_sun    ! Sunlit leaf transpiration [kg/m2/s]
   real(r8), intent(in)  :: qflx_sha    ! Shaded leaf transpiration [kg/m2/s]
   real(r8), intent(in)  :: laisun      ! Sunlit leaf area index
   real(r8), intent(in)  :: laisha      ! Shaded leaf area index
   real(r8), intent(in)  :: sai         ! Stem area index
   real(r8), intent(in)  :: htop        ! Canopy top [m]
   real(r8), intent(in)  :: qeroot      ! soil-root interface conductance [mm/s]
   real(r8), intent(in)  :: dqeroot     ! soil-root interface conductance [mm/s]
   real(r8), intent(in)  :: kmax_sun
   real(r8), intent(in)  :: kmax_sha
   real(r8), intent(in)  :: kmax_xyl
   real(r8), intent(in)  :: kmax_root
   real(r8), intent(in)  :: psi50_sun   ! water potential at 50% loss of sunlit leaf tissue conductance (mmH2O)
   real(r8), intent(in)  :: psi50_sha   ! water potential at 50% loss of shaded leaf tissue conductance (mmH2O)
   real(r8), intent(in)  :: psi50_xyl   ! water potential at 50% loss of xylem tissue conductance (mmH2O)
   real(r8), intent(in)  :: psi50_root  ! water potential at 50% loss of root tissue conductance (mmH2O)
   real(r8), intent(in)  :: ck
   !
   ! !LOCAL VARIABLES:
   real(r8) wtl          ! heat conductance for leaf [m/s]
   real(r8) fsto1        ! sunlit transpiration reduction function [-]
   real(r8) fsto2        ! shaded transpiration reduction function [-]
   real(r8) fx           ! fraction of maximum conductance, xylem-to-leaf [-]
   real(r8) fr           ! fraction of maximum conductance, root-to-xylem [-]
   real(r8) dfsto1       ! 1st derivative of fsto1 w.r.t. change in vegwp
   real(r8) dfsto2       ! 1st derivative of fsto2 w.r.t. change in vegwp
   real(r8) dfx          ! 1st derivative of fx w.r.t. change in vegwp
   real(r8) dfr          ! 1st derivative of fr w.r.t. change in vegwp
   real(r8) A11, A13, A22, A23, A31, A32, A33, A34, A43, A44   ! matrix relating vegwp to flux divergence f=A*d(vegwp)
   real(r8) leading      ! inverse of determiniant
   real(r8) determ       ! determinant of matrix
   real(r8) grav1        ! gravitational potential surface to canopy top (mm H2O)
   real(r8) invfactor    !
   real(r8) f(nvegwcs)
   real(r8), parameter :: tol_lai=1.e-7_r8 ! minimum lai WHERE transpiration is calc'd
   integer, parameter :: leafsun=1
   integer, parameter :: leafsha=2
   integer, parameter :: xyl=3
   integer, parameter :: root=4
   integer  :: j                     ! index
   !------------------------------------------------------------------------------

      grav1 = htop*1000._r8

      !compute conductance attenuation for each segment
      fsto1 =  plc(x(leafsun),psi50_sun,ck)
      fsto2 =  plc(x(leafsha),psi50_sha,ck)
      fx =     plc(x(xyl),psi50_xyl,ck)
      fr =     plc(x(root),psi50_root,ck)

      !compute 1st deriv of conductance attenuation for each segment
      dfsto1 =  d1plc(x(leafsun),psi50_sun,ck)
      dfsto2 =  d1plc(x(leafsha),psi50_sha,ck)
      dfx =     d1plc(x(xyl),psi50_xyl,ck)
      dfr =     d1plc(x(root),psi50_root,ck)


      A11 = - laisun * kmax_sun * fx - qflx_sun * dfsto1
      A13 = laisun * kmax_sun * dfx * (x(xyl)-x(leafsun)) + laisun * kmax_sun * fx
      A22 = - laisha * kmax_sha * fx - qflx_sha * dfsto2
      A23 = laisha * kmax_sha * dfx * (x(xyl)-x(leafsha)) + laisha * kmax_sha * fx
      A31 = laisun * kmax_sun * fx
      A32 = laisha * kmax_sha * fx
      A33 = - laisun * kmax_sun * dfx * (x(xyl)-x(leafsun)) - laisun * kmax_sun * fx&
            - laisha * kmax_sha * dfx * (x(xyl)-x(leafsha)) - laisha * kmax_sha * fx&
               - sai * kmax_xyl / htop * fr
      A34 = sai * kmax_xyl / htop * dfr * (x(root)-x(xyl)-grav1) + sai * kmax_xyl / htop * fr
      A43 = sai * kmax_xyl / htop * fr
      A44 = - sai * kmax_xyl / htop * fr&
            - sai * kmax_xyl / htop * dfr * (x(root)-x(xyl)-grav1) + dqeroot

      !compute flux divergence across each plant segment
      f(leafsun) = qflx_sun * fsto1 - laisun * kmax_sun * fx * (x(xyl)-x(leafsun))
      f(leafsha) = qflx_sha * fsto2 - laisha * kmax_sha * fx * (x(xyl)-x(leafsha))
      f(xyl)  = laisun * kmax_sun * fx * (x(xyl)-x(leafsun))&
              + laisha * kmax_sha * fx * (x(xyl)-x(leafsha)) &
              - sai * kmax_xyl / htop * fr * (x(root)-x(xyl)-grav1)
      f(root) = sai * kmax_xyl / htop * fr * (x(root)-x(xyl)-grav1) - qeroot

      IF(qflx_sha > 0 )THEN
         determ=A44*A22*A33*A11-A44*A22*A31*A13-A44*A32*A23*A11-A43*A11*A22*A34

         IF(determ .ne. 0)THEN
            dx(leafsun) = ((A22*A33*A44 - A22*A34*A43 - A23*A32*A44)*f(leafsun) + A13*A32*A44*f(leafsha) &
                          - A13*A22*A44*f(xyl) + A13*A22*A34*f(root)) / determ
            dx(leafsha) = ( A23*A31*A44*f(leafsun) + (A11*A33*A44 - A11*A34*A43 - A13*A31*A44)*f(leafsha) &
                          - A11*A23*A44*f(xyl) + A11*A23*A34*f(root)) / determ
            dx(xyl)     = (-A22*A31*A44*f(leafsun) - A11*A32*A44*f(leafsha) &
                          + A11*A22*A44*f(xyl) - A11*A22*A34*f(root)) / determ
            dx(root)    = ( A22*A31*A43*f(leafsun) + A11*A32*A43*f(leafsha) &
                          - A11*A22*A43*f(xyl) +(A11*A22*A33 - A11*A23*A32 - A13*A22*A31)*f(root)) / determ
         ELSE
            dx = 0._r8
         ENDIF
      ELSE
         A33 = - laisun * kmax_sun * dfx * (x(xyl)-x(leafsun)) - laisun * kmax_sun * fx - sai * kmax_xyl / htop * fr
         f(xyl) = laisun * kmax_sun * fx * (x(xyl)-x(leafsun)) - sai * kmax_xyl / htop * fr * (x(root)-x(xyl)-grav1)
         determ=A11*A33*A44-A34*A11*A43-A13*A31*A44
         IF(determ .ne. 0)THEN
            dx(leafsun) = (- A13*A44*f(xyl) + A13*A34*f(root) + (A33*A44 - A34*A43)*f(leafsun)) / determ
            dx(xyl)     = (  A11*A44*f(xyl) - A11*A34*f(root) - A31*A44*f(leafsun))             / determ
            dx(root)    = (- A11*A43*f(xyl) + (A11*A33 - A13*A31)*f(root) + A31*A43*f(leafsun)) / determ

            dx(leafsha) = x(leafsun) - x(leafsha) + dx(leafsun)
         ELSE
            dx = 0._r8
         ENDIF
      ENDIF

   END SUBROUTINE spacAF_twoleaf

   SUBROUTINE getvegwp_twoleaf(x, nvegwcs, nl_soil, z_soi, gb_mol, gs_mol_sun, gs_mol_sha, &
             qsatl, qaf,qg,qm,rhoair, psrf, fwet, laisun, laisha, htop, sai, tl, rss, &
             raw, rd, smp, k_soil_root, k_ax_root, kmax_xyl, kmax_root, rstfacsun, rstfacsha, &
             psi50_sun, psi50_sha, psi50_xyl, psi50_root, ck, rootflux, etrsun, etrsha)
!-----------------------------------------------------------------------
! !DESCRIPTION:
!  Calculates transpiration and returns corresponding vegwp in x
!
! !USES:
!  calls getqflx
!-----------------------------------------------------------------------
   USE MOD_Const_Physical, only: tfrz
   IMPLICIT NONE
   !
   ! !ARGUMENTS:
   integer,  intent(in)    :: nvegwcs
   real(r8), intent(out)   :: x(nvegwcs)     ! working copy of veg water potential for patch p
   integer,  intent(in)    :: nl_soil        ! number of soil layers
   real(r8), intent(in)    :: z_soi(nl_soil) ! node depth [m]
   real(r8), intent(in)    :: gb_mol         ! Leaf boundary layer conductance [umol H2O/m**2/s]
   real(r8), intent(inout) :: gs_mol_sun     ! Ball-Berry leaf conductance [umol H2O/m**2/s]
   real(r8), intent(inout) :: gs_mol_sha     ! Ball-Berry leaf conductance [umol H2O/m**2/s]
   real(r8), intent(in)    :: qsatl          ! Sunlit leaf specific humidity [kg/kg]
   real(r8), intent(in)    :: qaf            ! humidity of canopy air [kg/kg]
   real(r8), intent(in)    :: qg             ! specific humidity at ground surface [kg/kg]
   real(r8), intent(in)    :: qm             ! specific humidity at reference height [kg/kg]
   real(r8), intent(in)    :: rhoair         ! density [kg/m**3]
   real(r8), intent(in)    :: psrf           ! atmospheric pressure [Pa]
   real(r8), intent(in)    :: fwet           ! fraction of foliage that is green and dry [-]
   real(r8), intent(in)    :: laisun         ! Sunlit leaf area index
   real(r8), intent(in)    :: laisha         ! Shaded leaf area index
   real(r8), intent(in)    :: htop           ! canopy top [m]
   real(r8), intent(in)    :: sai            ! stem area index
   real(r8), intent(in)    :: tl             ! leaf temperature
   real(r8), intent(in)    :: kmax_xyl
   real(r8), intent(in)    :: kmax_root
   real(r8), intent(in)    :: rstfacsun
   real(r8), intent(in)    :: rstfacsha
   real(r8), intent(in)    :: psi50_sun      ! water potential at 50% loss of sunlit leaf tissue conductance (mmH2O)
   real(r8), intent(in)    :: psi50_sha      ! water potential at 50% loss of shaded leaf tissue conductance (mmH2O)
   real(r8), intent(in)    :: psi50_xyl      ! water potential at 50% loss of xylem tissue conductance (mmH2O)
   real(r8), intent(in)    :: psi50_root     ! water potential at 50% loss of root tissue conductance (mmH2O)
   real(r8), intent(in)    :: ck             !
   real(r8), intent(in)    :: rss            ! soil surface resistance [s/m]
   real(r8), intent(in)    :: raw            ! moisture resistance [s/m]
   real(r8), intent(in)    :: rd             ! aerodynamical resistance between ground and canopy air
   real(r8), intent(in)    :: smp(nl_soil)   ! soil matrix potential
   real(r8), intent(in)    :: k_soil_root(nl_soil) ! soil-root interface conductance [mm/s]
   real(r8), intent(in)    :: k_ax_root(nl_soil)   ! root axial-direction conductance [mm/s]
   real(r8), intent(out)   :: etrsun         ! transpiration from sunlit leaf (mm/s)
   real(r8), intent(out)   :: etrsha         ! transpiration from shaded leaf (mm/s)
   real(r8), intent(out)   :: rootflux(nl_soil)    ! root water uptake from different layers
   !
   ! !LOCAL VARIABLES:
!  real(r8) qflx_sun         ! Sunlit leaf transpiration [kg/m2/s]
!  real(r8) qflx_sha         ! Shaded leaf transpiration [kg/m2/s]
   real(r8) qeroot
   real(r8) dummy
   real(r8) fx               ! fraction of maximum conductance, xylem-to-leaf [-]
   real(r8) fr               ! fraction of maximum conductance, root-to-xylem [-]
   real(r8) x_root_top
   real(r8) xroot(nl_soil)
   real(r8) grav1            ! gravitational potential surface to canopy top (mm H2O)
   real(r8) grav2(nl_soil)   ! soil layer gravitational potential relative to surface (mm H2O)
   integer  j                ! index
   logical  havegs           ! signals direction of calculation gs->qflx or qflx->gs
   logical  haroot           ! signals direction of calculation x_root_top->qeroot or qeroot->x_root_top
   integer, parameter :: leafsun=1
   integer, parameter :: leafsha=2
   integer, parameter :: xyl=3
   integer, parameter :: root=4
   real(r8) :: soilflux      ! total soil column transpiration [mm/s]

      !----------------------------------------------------------------------
      grav1 = 1000._r8 * htop
      grav2(1:nl_soil) = 1000._r8 * z_soi(1:nl_soil)

      !compute transpiration demand
      havegs=.true.
      CALL getqflx_gs2qflx_twoleaf(gb_mol,gs_mol_sun,gs_mol_sha,etrsun,etrsha,qsatl,qaf, &
                   rhoair,psrf,laisun,laisha,sai,fwet,tl,rss,raw,rd,qg,qm,rstfacsun,rstfacsha)

      !calculate root water potential
      qeroot = etrsun + etrsha

      CALL getrootqflx_qe2x(nl_soil,smp,z_soi,k_soil_root,k_ax_root,qeroot,xroot,x_root_top)
      x(root) = x_root_top

      !calculate xylem water potential
      fr = plc(x(root),psi50_root,ck)
      x(xyl) = x(root) - grav1 - (etrsun+etrsha)/(fr*kmax_root/htop*sai)

      !calculate sun/sha leaf water potential
      fx = plc(x(xyl),psi50_xyl,ck)
         x(leafsha) = x(xyl) - (etrsha/(fx*kmax_xyl*laisha))
         x(leafsun) = x(xyl) - (etrsun/(fx*kmax_xyl*laisun))


      !calculate soil flux
      DO j = 1,nl_soil
         rootflux(j) = k_soil_root(j)*(smp(j)-xroot(j))
      ENDDO

      soilflux = sum(rootflux(:))

   END SUBROUTINE getvegwp_twoleaf

   SUBROUTINE getqflx_gs2qflx_twoleaf(gb_mol,gs_mol_sun,gs_mol_sha,qflx_sun,qflx_sha,qsatl,qaf,&
                             rhoair,psrf,laisun,laisha,sai,fwet,tl,rss,raw,rd,qg,qm,rstfacsun,rstfacsha)
!-----------------------------------------------------------------------
! !DESCRIPTION:
!  calculate sunlit and shaded transpiration using gb_MOL and gs_MOL
!
!-----------------------------------------------------------------------
   IMPLICIT NONE

   ! !ARGUMENTS:
   real(r8), intent(in)    :: gb_mol     ! leaf boundary layer conductance (mol H2O/m**2/s), leaf scale
   real(r8), intent(inout) :: gs_mol_sun ! Ball-Berry leaf conductance (mol H2O/m**2/s), leaf scale
   real(r8), intent(inout) :: gs_mol_sha ! Ball-Berry leaf conductance (mol H2O/m**2/s), leaf scale
   real(r8), intent(inout) :: qflx_sun   ! Sunlit leaf transpiration [kg/m2/s]
   real(r8), intent(inout) :: qflx_sha   ! Shaded leaf transpiration [kg/m2/s]
   real(r8), intent(in)    :: qsatl      ! leaf specific humidity [kg/kg]
   real(r8), intent(in)    :: qaf        ! humidity of canopy air [kg/kg]
   real(r8), intent(in)    :: qg         ! specific humidity at ground surface [kg/kg]
   real(r8), intent(in)    :: qm         ! specific humidity at reference height [kg/kg]
   real(r8), intent(in)    :: rhoair     ! density (kg/m**3)
   real(r8), intent(in)    :: psrf       ! atmospheric pressure (Pa)
   real(r8), intent(in)    :: laisun     ! sunlit leaf area index (m2/m2)
   real(r8), intent(in)    :: laisha     ! shaded leaf area index (m2/m2)
   real(r8), intent(in)    :: sai        ! stem area index (m2/m2)
   real(r8), intent(in)    :: fwet       ! fraction of foliage that is green and dry [-]
   real(r8), intent(in)    :: tl         ! shaded leaf temperature
   real(r8), intent(in)    :: rss        ! soil surface resistance [s/m]
   real(r8), intent(in)    :: raw        ! moisture resistance [s/m]
   real(r8), intent(in)    :: rd         ! aerodynamical resistance between ground and canopy air
   real(r8),optional, intent(in) :: rstfacsun
   real(r8),optional, intent(in) :: rstfacsha

   !
   ! !LOCAL VARIABLES:
   real(r8) cf       ! (umol/m**3) r = cf./g gmol(umol/m**2/s) -> r(s/m)
   real(r8) tprcor   ! tf*psur*100./1.013e5

   real(r8) wtaq0    ! normalized latent heat conductance for air [-]
   real(r8) wtgq0    ! normalized latent heat conductance for ground [-]
   real(r8) wtlq0    ! normalized latent heat cond. for air and sunlit leaf [-]
   real(r8) wtsqi    ! latent heat resistance for air, grd and leaf [-]

   real(r8) delta
   real(r8) caw      ! latent heat conductance for air [m/s]
   real(r8) cgw      ! latent heat conductance for ground [m/s]
   real(r8) cfw      ! latent heat conductance for leaf [m/s]

      !----------------------------------------------------------------------
      tprcor   = 44.6*273.16*psrf/1.013e5
      cf       = tprcor/tl * 1.e6_r8  ! gb->gbmol conversion factor

      delta = 0.0
      IF(qsatl-qaf .gt. 0.) delta = 1.0

      caw = 1. / raw
      IF (qg < qaf)THEN
         cgw = 1. / rd
      ELSE
         IF (DEF_RSS_SCHEME .eq. 4) THEN
            cgw = rss / rd
         ELSE
            cgw = 1. / (rd + rss)
         ENDIF
      ENDIF
      cfw = (1.-delta*(1.-fwet)) * (laisun+laisha+sai)*gb_mol/cf + (1.-fwet)*delta*&
          (laisun/(1._r8/gb_mol+1._r8/gs_mol_sun)/cf+laisha/(1._r8/gb_mol+1._r8/gs_mol_sha)/cf)
      wtsqi = 1. / ( caw + cgw + cfw )

      wtaq0     = caw * wtsqi
      wtgq0     = cgw * wtsqi
      wtlq0     = cfw * wtsqi

      qflx_sun  = rhoair * (1.-fwet) * delta &
            * laisun / (1./gb_mol+1./gs_mol_sun)/cf &
            * ( (wtaq0 + wtgq0)*qsatl - wtaq0*qm - wtgq0*qg )
!      IF(qflx_sun < 1.e-7_r8)THEN
!         qflx_sun   = 0._r8
!      ENDIF
      IF(present(rstfacsun))THEN
         IF(rstfacsun .le. 1.e-2)qflx_sun = 0._r8
      ENDIF
      qflx_sha  = rhoair * (1.-fwet) * delta &
            * laisha / (1./gb_mol+1./gs_mol_sha)/cf &
            * ( (wtaq0 + wtgq0)*qsatl - wtaq0*qm - wtgq0*qg )
!      IF(qflx_sha < 1.e-7)THEN
!         qflx_sha   = 0._r8
!      ENDIF
      IF(present(rstfacsha))THEN
         IF(rstfacsha .le. 1.e-2)qflx_sha = 0._r8
      ENDIF

   END SUBROUTINE getqflx_gs2qflx_twoleaf

   SUBROUTINE getqflx_qflx2gs_twoleaf(gb_mol,gs_mol_sun,gs_mol_sha,qflx_sun,qflx_sha,qsatl,qaf, &
                      rhoair,psrf,laisun,laisha,sai,fwet,tl,rss,raw,rd,qg,qm)
!-----------------------------------------------------------------------
! !DESCRIPTION:
!  calculate sunlit and shaded transpiration using gb_MOL and gs_MOL
!-----------------------------------------------------------------------
   IMPLICIT NONE

   ! !ARGUMENTS:
   real(r8), intent(in)     :: gb_mol     ! leaf boundary layer conductance (mol H2O/m**2/s), leaf scale
   real(r8), intent(inout)  :: gs_mol_sun ! Ball-Berry leaf conductance (mol H2O/m**2/s), leaf scale
   real(r8), intent(inout)  :: gs_mol_sha ! Ball-Berry leaf conductance (mol H2O/m**2/s), leaf scale
   real(r8), intent(inout)  :: qflx_sun   ! Sunlit leaf transpiration [kg/m2/s]
   real(r8), intent(inout)  :: qflx_sha   ! Shaded leaf transpiration [kg/m2/s]
   real(r8), intent(in)     :: qsatl      ! leaf specific humidity [kg/kg]
   real(r8), intent(in)     :: qaf        ! humidity of canopy air [kg/kg]
   real(r8), intent(in)     :: qg         ! specific humidity at ground surface [kg/kg]
   real(r8), intent(in)     :: qm         ! specific humidity at reference height [kg/kg]
   real(r8), intent(in)     :: rhoair     ! density (kg/m**3)
   real(r8), intent(in)     :: psrf       ! atmospheric pressure (Pa)
   real(r8), intent(in)     :: laisun     ! sunlit leaf area index (m2/m2)
   real(r8), intent(in)     :: laisha     ! shaded leaf area index (m2/m2)
   real(r8), intent(in)     :: sai        ! stem area index (m2/m2)
   real(r8), intent(in)     :: fwet       ! fraction of foliage that is green and dry [-]
   real(r8), intent(in)     :: tl         ! leaf temperature
   real(r8), intent(in)     :: rss        ! soil surface resistance [s/m]
   real(r8), intent(in)     :: raw        ! moisture resistance [s/m]
   real(r8), intent(in)     :: rd         ! aerodynamical resistance between ground and canopy air

   !
   ! !LOCAL VARIABLES:
   real(r8) wtlsun             ! heat conductance for sunlit leaf boundary [m/s]
   real(r8) wtlsha             ! heat conductance for shaded leaf boundary [m/s]
   real(r8) cf                 ! s m**2/umol -> s/m
   real(r8) tprcor             !tf*psur*100./1.013e5

   real(r8) wtaq0              ! normalized latent heat conductance for air [-]
   real(r8) wtgq0              ! normalized latent heat conductance for ground [-]
   real(r8) wtlsunq0           ! normalized latent heat cond. for air and sunlit leaf [-]
   real(r8) wtlshaq0           ! normalized latent heat cond. for air and shaded leaf [-]

   real(r8) delta
   real(r8) caw                ! latent heat conductance for air [m/s]
   real(r8) cgw                ! latent heat conductance for ground [m/s]
   real(r8) cwet               ! latent heat conductance for wet leaf [m/s]
   real(r8) csunw_dry          ! latent heat conductance for sunlit dry leaf [m/s]
   real(r8) cshaw_dry          ! latent heat conductance for shaded dry leaf [m/s]
   real(r8) cqi_wet            ! latent heat conductance for air, grd and wet leaf [-]
   real(r8) cqi_leaf           ! (wtaq0 + wtgq0)*qsatl - wtaq0*qm - wtgq0*qg [m/s]
   real(r8) A1,B1,C1,A2,B2,C2  ! in binary quadratic equations

      !----------------------------------------------------------------------
      IF(qflx_sun .gt. 0 .or. qflx_sha .gt. 0)THEN
         tprcor   = 44.6*273.16*psrf/1.013e5
         cf       = tprcor/tl * 1.e6_r8  ! gb->gbmol conversion factor

         delta = 0.0
         IF(qsatl-qaf .gt. 0.) delta = 1.0

         caw = 1. / raw
         IF (qg < qaf)THEN
            cgw = 1. / rd
         ELSE
            IF (DEF_RSS_SCHEME .eq. 4) THEN
               cgw = rss / rd
            ELSE
               cgw = 1. / (rd + rss)
            ENDIF
         ENDIF
         cwet     = (1.-delta*(1.-fwet)) * (laisun + laisha + sai) * gb_mol / cf
         cqi_wet  = caw + cgw + cwet
         cqi_leaf = caw * (qsatl - qm) + cgw * (qsatl - qg)

   !   Solve equations:
   !   A1 * csunw_dry + B1 * cfshaw_dry = C1
   !   A2 * csunw_dry + B2 * cfshaw_dry = C2

         A1 = cqi_leaf - qflx_sun / rhoair
         B1 = - qflx_sun / rhoair
         C1 = qflx_sun * cqi_wet / rhoair
         A2 = - qflx_sha / rhoair
         B2 = cqi_leaf - qflx_sha / rhoair
         C2 = qflx_sha * cqi_wet / rhoair

         csunw_dry = (B1*C2 - B2*C1)/(B1*A2 - B2*A1)
         cshaw_dry = (A1*C2 - A2*C1)/(A1*B2 - B1*A2)

         IF (qflx_sun > 0._r8) THEN
            gs_mol_sun = 1._r8 / ((1. - fwet) * delta * laisun / csunw_dry / cf - 1._r8 / gb_mol)
         ENDIF
         IF (qflx_sha > 0._r8) THEN
            gs_mol_sha = 1._r8 / ((1. - fwet) * delta * laisha / cshaw_dry / cf - 1._r8 / gb_mol)
         ENDIF
      ENDIF

   END SUBROUTINE getqflx_qflx2gs_twoleaf

   SUBROUTINE getrootqflx_x2qe(nl_soil,smp,x_root_top,z_soisno,krad,kax,qeroot,dqeroot)

   USE MOD_Utils
!-----------------------------------------------------------------------
! !DESCRIPTION
!  Return root water potential at top soil node. Return soil-root water flux.
!
!-----------------------------------------------------------------------

   integer , intent(in)    :: nl_soil
   real(r8), intent(in)    :: smp      (nl_soil)
   real(r8), intent(in) :: x_root_top
   real(r8), intent(in)    :: z_soisno (nl_soil)
   real(r8), intent(in)    :: krad     (nl_soil)
   real(r8), intent(in)    :: kax      (nl_soil)
   real(r8), intent(out) :: qeroot
   real(r8), intent(out) :: dqeroot

! Local variables
   real(r8) den_AHR,den1,den2  ! used in calculating HR(Amenu model)
   real(r8) amx_hr(nl_soil-1)  ! "a" left off diagonal of tridiagonal matrix
   real(r8) bmx_hr(nl_soil-1)  ! "b" diagonal column for tridiagonal matrix
   real(r8) cmx_hr(nl_soil-1)  ! "c" right off diagonal tridiagonal matrix
   real(r8) rmx_hr(nl_soil-1)  ! "r" forcing term of tridiagonal matrix
   real(r8) drmx_hr(nl_soil-1) ! "dr" forcing term of tridiagonal matrix for d/dxroot(1)
   real(r8) x(nl_soil-1)       ! root water potential from layer 2 to nl_soil
   real(r8) dx(nl_soil-1)      ! derivate of root water potential from layer 2 to nl_soil (dxroot(:)/dxroot(1))
   real(r8) xroot(nl_soil)     ! root water potential from layer 2 to nl_soil
   real(r8) zmm(1:nl_soil)     ! layer depth [mm]
   real(r8) qeroot_nl(1:nl_soil) ! root water potential from layer 2 to nl_soil
   real(r8) dxroot2    ! dxroot(2)/dxroot(1)
   integer j

      ! Because the depths in this routine are in mm, USE local
      ! variable arrays instead of pointers
      DO j = 1, nl_soil
         zmm(j)  = z_soisno(j)*1000.
      ENDDO

      xroot(1) = x_root_top + zmm(1)
      ! For the 2nd soil layer
      j            = 2
      den1         = zmm(j) - zmm(j-1)
      den2         = zmm(j+1) - zmm(j)
      amx_hr(j-1)  = 0
      bmx_hr(j-1)  = kax(j-1)/den1 + kax(j)/den2 + krad(j)
      cmx_hr(j-1)  = -kax(j)/den2
      rmx_hr(j-1)  = krad(j)*smp(j) + kax(j-1) - kax(j) + kax(j-1)/den1*xroot(1)
      drmx_hr(j-1) = kax(j-1)/den1

      ! For the middile soil layers
      DO j = 3, nl_soil - 1
         den1   = zmm(j) - zmm(j-1)
         den2   = zmm(j+1) - zmm(j)
         amx_hr (j-1) = -kax(j-1)/den1
         bmx_hr (j-1) = kax(j-1)/den1 + kax(j)/den2 + krad(j)
         cmx_hr (j-1) = -kax(j)/den2
         rmx_hr (j-1) = krad(j)*smp(j) + kax(j-1) - kax(j)
         drmx_hr(j-1) = 0._r8
      ENDDO

      ! For the bottom soil layer
      j           = nl_soil
      den_AHR     = zmm(j) - zmm(j-1)
      amx_hr (j-1) = -kax(j-1)/den_AHR
      bmx_hr (j-1) = kax(j-1)/den_AHR + krad(j)
      cmx_hr (j-1) = 0
      rmx_hr (j-1) = krad(j)*smp(j) + kax(j-1)
      drmx_hr(j-1) = 0._r8

      ! Solve for root pressure potential using tridiagonal matric solver x = A^-1 * r
      CALL tridia (nl_soil-1 ,amx_hr ,bmx_hr ,cmx_hr ,rmx_hr ,x)

      DO j = 2,nl_soil
         xroot(j) = x(j-1)
      ENDDO

         ! Solve the dx(:)/dxroot(1) = A^-1 * dr
      CALL tridia (nl_soil-1 ,amx_hr ,bmx_hr ,cmx_hr ,drmx_hr, dx)

      dxroot2 = dx(1)

      ! calculate the water flux
      j      = 1
      den2   = zmm(j+1) - zmm(j)
      qeroot = krad(j) * (smp(1) - xroot(1)) + (xroot(2) - xroot(1)) * kax(j)/den2 - kax(j)

      ! calculate the dqeroot/dx_root_top;
      dqeroot = - krad(j) + (dxroot2 - 1) * kax(j)/den2
      DO j = 1,nl_soil
         qeroot_nl(j) = krad(j)*(smp(j) - xroot(j))
      ENDDO

   END SUBROUTINE getrootqflx_x2qe

   SUBROUTINE getrootqflx_qe2x(nl_soil,smp,z_soisno,krad,kax,qeroot,xroot,x_root_top)

   USE MOD_Utils
!-----------------------------------------------------------------------
! !DESCRIPTION
!  Return root water potential at top soil node. Return soil-root water flux.
!-----------------------------------------------------------------------

   integer,  intent(in)  :: nl_soil
   real(r8), intent(in)  :: smp      (nl_soil)
   real(r8), intent(in)  :: z_soisno (nl_soil)
   real(r8), intent(in)  :: krad     (nl_soil)
   real(r8), intent(in)  :: kax      (nl_soil)
   real(r8), intent(in)  :: qeroot
   real(r8), intent(out) :: xroot    (nl_soil)
   real(r8), intent(out) :: x_root_top

! Local variables
   real(r8) den_AHR,den1,den2    ! used in calculating HR(Amenu model)
   real(r8) amx_hr(nl_soil)      ! "a" left off diagonal of tridiagonal matrix
   real(r8) bmx_hr(nl_soil)      ! "b" diagonal column for tridiagonal matrix
   real(r8) cmx_hr(nl_soil)      ! "c" right off diagonal tridiagonal matrix
   real(r8) rmx_hr(nl_soil)      ! "r" forcing term of tridiagonal matrix
   real(r8) x(nl_soil)           ! root water potential from layer 2 to nl_soil
   real(r8) zmm(1:nl_soil)       ! layer depth [mm]
   real(r8) qeroot_nl(1:nl_soil) ! root water potential from layer 2 to nl_soil
   integer j

      ! Because the depths in this routine are in mm, USE local
      ! variable arrays instead of pointers
      DO j = 1, nl_soil
         zmm(j)  = z_soisno(j)*1000.
      ENDDO

      j           = 1
      den2        = zmm(j+1) - zmm(j)
      amx_hr(j)   = 0
      bmx_hr(j) = kax(j)/den2 + krad(j)
      cmx_hr(j) = -kax(j)/den2
      rmx_hr(j) = krad(j)*smp(j) - qeroot - kax(j)

      ! For the middile soil layers
      DO j = 2, nl_soil - 1
          den1   = zmm(j) - zmm(j-1)
          den2   = zmm(j+1) - zmm(j)
          amx_hr(j) = -kax(j-1)/den1
          bmx_hr(j) = kax(j-1)/den1 + kax(j)/den2 + krad(j)
          cmx_hr(j) = -kax(j)/den2
          rmx_hr(j) = krad(j)*smp(j) + kax(j-1) - kax(j)
      ENDDO

      ! For the bottom soil layer
      j      = nl_soil
      den_AHR    = zmm(j) - zmm(j-1)
      amx_hr(j) = -kax(j-1)/den_AHR
      bmx_hr(j) = kax(j-1)/den_AHR + krad(j)
      cmx_hr(j) = 0
      rmx_hr(j) = krad(j)*smp(j) + kax(j-1)

      ! Solve for root pressure potential using tridiagonal matric solver
      CALL tridia (nl_soil ,amx_hr ,bmx_hr ,cmx_hr ,rmx_hr ,x)

      xroot(1:nl_soil) = x(1:nl_soil)
      x_root_top = xroot(1) - zmm(1)

   END SUBROUTINE getrootqflx_qe2x

   FUNCTION plc(x,psi50,ck)
!-----------------------------------------------------------------------
! !DESCRIPTION
!  Return value of vulnerability curve at x
!
!-----------------------------------------------------------------------

! !ARGUMENTS
   real(r8) , intent(in)  :: x             ! water potential input
!   integer  , intent(in)  :: level         ! veg segment lvl (1:nvegwcs)
!   integer  , intent(in)  :: plc_method    !
   real(r8) , intent(in)  :: psi50     ! water potential at 50% loss of sunlit leaf tissue conductance (mmH2O)
!   real(r8) , intent(in)  :: psi50_sun     ! water potential at 50% loss of sunlit leaf tissue conductance (mmH2O)
!   real(r8) , intent(in)  :: psi50_sha     ! water potential at 50% loss of shaded leaf tissue conductance (mmH2O)
!   real(r8) , intent(in)  :: psi50_xyl     ! water potential at 50% loss of xylem tissue conductance (mmH2O)
!   real(r8) , intent(in)  :: psi50_root    ! water potential at 50% loss of root tissue conductance (mmH2O)
   real(r8) , intent(in)  :: ck
   real(r8)               :: plc           ! attenuated conductance [0:1] 0=no flow
   !
   ! !PARAMETERS
!   integer , parameter :: vegetation_weibull=0  ! case number
!   integer , parameter :: leafsun = 1  ! index for sunlit leaf
!   integer , parameter :: leafsha = 2  ! index for shaded leaf
!   integer , parameter :: xyl     = 3  ! index for xylem
!   integer , parameter :: root    = 4  ! index for root

   ! !LOCAL VARIABLES
   !real(r8) psi50,tmp
   real(r8) tmp
   integer i

    !------------------------------------------------------------------------------
!    select CASE(level)
!    CASE (leafsun)
!       psi50 = psi50_sun
!    CASE (leafsha)
!       psi50 = psi50_sha
!    CASE (xyl)
!       psi50 = psi50_xyl
!    CASE (root)
!       psi50 = psi50_root
!    CASE default
!       write(*,*),'must choose level from 1 to 4 (sunlit leaf to root)'
!    END select

!    select CASE (plc_method)
       !possible to add other methods later
!    CASE (vegetation_weibull)
      tmp = amax1(-(x/psi50)**ck,-500._r8)
!       IF(tmp .lt. -500._r8)THEN
!          plc = 0._r8
!       ELSE
      plc=2._r8**tmp
!       ENDIF
      IF ( plc < 0.00001_r8) plc = 1.e-5_r8
!    CASE default
!       write(*,*),'must choose plc method'
!    END select

   END FUNCTION plc
   !--------------------------------------------------------------------------------

   FUNCTION d1plc(x,psi50,ck)
!-----------------------------------------------------------------------
! !DESCRIPTION
!  Return 1st derivative of vulnerability curve at x
!-----------------------------------------------------------------------

! !ARGUMENTS
   real(r8) , intent(in) :: x                ! water potential input
!   integer  , intent(in) :: level            ! veg segment lvl (1:nvegwcs)
!   integer  , intent(in) :: plc_method       ! 0 for vegetation, 1 for soil
   real(r8) , intent(in) :: psi50        ! water potential at 50% loss of sunlit leaf tissue conductance (mmH2O)
!   real(r8) , intent(in) :: psi50_sun        ! water potential at 50% loss of sunlit leaf tissue conductance (mmH2O)
!   real(r8) , intent(in) :: psi50_sha        ! water potential at 50% loss of shaded leaf tissue conductance (mmH2O)
!   real(r8) , intent(in) :: psi50_xyl        ! water potential at 50% loss of xylem tissue conductance (mmH2O)
!   real(r8) , intent(in) :: psi50_root       ! water potential at 50% loss of root tissue conductance (mmH2O)
   real(r8) , intent(in) :: ck
   real(r8)              :: d1plc            ! first deriv of plc curve at x
   !
   ! !PARAMETERS
!   integer , parameter :: vegetation_weibull=0  ! CASE number
!   integer , parameter :: leafsun = 1  ! index for sunlit leaf
!   integer , parameter :: leafsha = 2  ! index for shaded leaf
!   integer , parameter :: xyl     = 3  ! index for xylem
!   integer , parameter :: root    = 4  ! index for root

   ! !LOCAL VARIABLES
!   real(r8) psi50,tmp
   real(r8) tmp
      !------------------------------------------------------------------------------
!     select CASE(level)
!     CASE (leafsun)
!        psi50 = psi50_sun
!     CASE (leafsha)
!        psi50 = psi50_sha
!     CASE (xyl)
!        psi50 = psi50_xyl
!     CASE (root)
!        psi50 = psi50_root
!     CASE default
!        write(*,*),'must choose level from 1 to 4 (sunlit leaf to root)'
!     END select

!     select CASE (plc_method)
        !possible to add other methods later
!     CASE (vegetation_weibull)
         tmp = amax1(-(x/psi50)**ck,-500._r8)
!        IF(tmp .lt. -500._r8)THEN
!           d1plc = 0._r8
!        ELSE
            d1plc= ck * log(2._r8) * (2._r8**tmp) * tmp / x
!        ENDIF
!     CASE default
!        write(*,*),'must choose plc method'
!     END select

   END FUNCTION d1plc


END MODULE MOD_PlantHydraulic
! ---------- EOP ------------
