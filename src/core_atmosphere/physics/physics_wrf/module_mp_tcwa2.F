
!------------------------------------------------------------------------------
! The TCWA2 (Taiwan Central Weather Administration 2-moment) bulk microphysics scheme
!
! TCWA2 is a two-moment bulk microphysics scheme simplified from the NTU triple-moment 
! microphysics framework, which has been implemented and registered as a microphysics 
! option (56) in the WRF modeling community. TCWA2 retains a gamma-type particle size 
! distribution with variable spectral parameters, diagnoses hydrometeor-associated 
! physical properties, revises number sinks due to evaporation loss, and implements 
! theoretically based fall-speed formulations that account for particle density and 
! aspect ratio for applications in convective-scale weather simulations.
!
! The main features of TCWA2 include:
! (1) diagnostic treatments of particle size distribution parameters (AFAX) within a 
!     two-moment bulk framework;
! (2) representation of hydrometeor physical properties, including ice-particle 
!     shape (XASPR), density (RHOX), and coupling fall speed;
! (3) direct linkage between simulated hydrometeor properties and dual-polarization 
!     radar variables, including horizontal radar reflectivity (DBZM), differential 
!     reflectivity (ZDR), and specific differential phase (KDP) through an embedded 
!     bulk radar operator.
! 
! References:
! Chen, J.-P. and T.-C. Tsai, 2016: Triple-moment modal parameterization for the 
!    adaptive growth habit of pristine ice crystals. J. Atmos. Sci., 73, 2105-2122.
! Tsai, T.-C., and J.-P. Chen, 2020: Multi-moment ice bulk microphysics scheme with 
!    consideration for particle shape and apparent density. Part I: Methodology and 
!    idealized simulation. J. Atmos. Sci., 77, 1821-1850.
! Tsai, T.-C., Chen, J.-P., Liu, Z., Jiang, S.-Y., Kong, R., Wu, Y.-J., Ban, J., 
!    Hsiao, L.-F., Tang, Y.-S., Chang, P.-L., and Hong, J.-S.: Development of the 
!    TCWA2 bulk cloud microphysics scheme and its integration with a dual-polarization 
!    radar operator for forecasting applications, Geosci. Model Dev., 19, 6231–6257.
!
! Author:Tzu-Chin Tsai at Taiwan Central Weather Administration [tzuchin@cwa.gov.tw]
!==========================================================================

      MODULE module_mp_tcwa2

      IMPLICIT NONE
      PUBLIC  :: mp_tcwa2

      INTEGER, PRIVATE, PARAMETER :: RAIN_SHAPE = 1                     ! 0. SPHERICAL; 1. ASPHERICAL RAINDROPS
      INTEGER, PRIVATE, PARAMETER :: ICE_SHAPE = 1                      ! 0. SPHERICAL; 1. DIAGNOSED ASPHERICAL PRISTINE ICE (HABIT)
      INTEGER, PRIVATE, PARAMETER :: AGG_SHAPE = 1                      ! 0. SPHERICAL; 1. DIAGNOSED ASPHERICAL SNOW-AGGREGATES
      INTEGER, PRIVATE, PARAMETER :: GRAP_SHAPE = 1                     ! 0. SPHERICAL; 1. DIAGNOSED ASPHERICAL RIMED ICE (GRAUPEL)
      INTEGER, PRIVATE, PARAMETER :: ICE_RHOI = 1                       ! 0. FIXED; 1. DIAGNOSED; PRISTINE ICE DENSITY
      INTEGER, PRIVATE, PARAMETER :: ICE_RHOS = 1                       ! 0. FIXED; 1. DIAGNOSED; AGGREGATES DENSITY
      INTEGER, PRIVATE, PARAMETER :: ICE_RHOG = 1                       ! 0. FIXED; 1. DIAGNOSED  RIMED ICE/GRAUPEL DENSITY
      INTEGER, PRIVATE, PARAMETER :: ICE_VENT = 1                       ! 0. OFF; 1. CL94; VENTILATION EFFECT
      INTEGER, PRIVATE, PARAMETER :: LIQ_VTC = 0                        ! 0. FIXED; 1. THEO-NUMS CLOUD DROPS FALL SPEED
      INTEGER, PRIVATE, PARAMETER :: LIQ_VTR = 1                        ! 0. FIXED; 1. THEO-NUMS RAIN DROPS FALL SPEED
      INTEGER, PRIVATE, PARAMETER :: ICE_VTI = 1                        ! 0. FIXED; 1. THEO-NUMS PRISTINE ICE FALL SPEED
      INTEGER, PRIVATE, PARAMETER :: ICE_VTS = 1                        ! 0. FIXED; 1. THEO-NUMS AGGREGATES FALL SPEED
      INTEGER, PRIVATE, PARAMETER :: ICE_VTG = 1                        ! 0. FIXED; 1. THEO-NUMS RIMED CRYSTALS FALL SPEED
      INTEGER, PRIVATE, PARAMETER :: AFAC_3M = 1                        ! 0. FIXED; 1.,2. VARIABLE AFAC PARAMETER
      INTEGER, PRIVATE, PARAMETER :: AFAR_3M = 1                        ! 0. FIXED; 1.,2. VARIABLE AFAR PARAMETER
      INTEGER, PRIVATE, PARAMETER :: AFAI_3M = 1                        ! 0. FIXED; 1. VARIABLE AFAI PARAMETER
      INTEGER, PRIVATE, PARAMETER :: AFAS_3M = 1                        ! 0. FIXED; 1. VARIABLE AFAS PARAMETER
      INTEGER, PRIVATE, PARAMETER :: AFAG_3M = 1                        ! 0. FIXED; 1. VARIABLE AFAG PARAMETER
      INTEGER, PRIVATE, PARAMETER :: NMODE = 3
      INTEGER, PRIVATE, PARAMETER :: EXYM = 1                           ! 0. EMPIRICAL 1. THEORETICAL METHOD
      INTEGER, PRIVATE, PARAMETER :: SSAT = 2                           ! 0. ADJUST QC; 1. ADJUST QC+QI; 2. PREDICTION SATURATION
      INTEGER, PRIVATE, PARAMETER :: IACT = 1                           ! 1. RY89; 2. WC20-power law
      INTEGER, PRIVATE, PARAMETER :: EVSB = 2                           ! 0. None; 1. Mean; 2. Theoretical
!----------------- FOR MICROPHYSICAL PROCESSES -------------------------
      REAL, PRIVATE, PARAMETER :: PI = 3.1415926535897932384626434
      REAL, PRIVATE, PARAMETER :: THRD = 1./3.,     C4PI3 = 4.*PI/3.
      REAL, PRIVATE, PARAMETER :: CP = 1.00546E3,   TK0C = 2.7315E2
      REAL, PRIVATE, PARAMETER :: R = 2.87058E2,    RV = 4.61495E2
      REAL, PRIVATE, PARAMETER :: CPI = 2.093E3,    CPW = 4.218E3
      REAL, PRIVATE, PARAMETER :: CMW = 1.8015E-2,  MLRA = 0.3
      REAL, PRIVATE, PARAMETER :: RHOSU = 8.5E4/(2.8715E2*2.7315E2)
      REAL, PRIVATE, PARAMETER :: RHOW = 9.97E2,    RHOG1 = 4.E2
      REAL, PRIVATE, PARAMETER :: RHOI0 = 9.1E2,    iRHOI0 = 1./RHOI0
      REAL, PRIVATE, PARAMETER :: RHOS0 = 1.E2,     G = 9.806
      REAL, PRIVATE, PARAMETER :: RHOG0 = (0.078+0.184*6.-0.015*36.)*1000
      REAL, PRIVATE, PARAMETER :: RHOIMIN = 5.E1,   RHOIMAX = RHOI0
      REAL, PRIVATE, PARAMETER :: C4PI3W = 4.*PI*RHOW/3.
      REAL, PRIVATE, PARAMETER :: iAMI0 = 6./(PI*RHOI0)
      REAL, PRIVATE, PARAMETER :: AAW = PI/4.,      V2M3 = 6./PI
      REAL, PRIVATE, PARAMETER :: AMW = PI*RHOW/6., AMI0 = PI*RHOI0/6.
      REAL, PRIVATE, PARAMETER :: AMS0 = PI*RHOS0/6., AMG0 = PI*RHOG1/6.
      REAL, PRIVATE, PARAMETER :: iAMW = 1./AMW
      REAL, PRIVATE, PARAMETER :: AIMM = 6.6E-1,    BIMM = 1.E2
      REAL, PRIVATE, PARAMETER :: AVSG = 8.6E-1,    BVSG = 2.8E-1
      REAL, PRIVATE, PARAMETER :: AVRH = 7.8E-1,    BVRH = 3.08E-1
      REAL, PRIVATE, PARAMETER :: AVIS = 1.,        BVIS = 1.4E-1
      REAL, PRIVATE, PARAMETER :: VENC1 = 3.09E-2,  VENC2 = 1.447E-1
      REAL, PRIVATE, PARAMETER :: VENP1 = 1.05E-2,  VENP2 = 2.28E-2
      REAL, PRIVATE, PARAMETER :: VENH1 = 0.22385,  VENH2 = 0.00101
      REAL, PRIVATE, PARAMETER :: SCATF0 = 1.89E-1, ZXMIN = 1.E-21     ! 0.189 = 0.176/0.93
      REAL, PRIVATE, PARAMETER :: DBZWL = 0.11,     MLIMIT = 1.E-2      ! S = 0.11; C = 0.053; X = 0.032m
      REAL, PRIVATE, PARAMETER :: NSMALL = 1.E-2,   NSMAL1 = 1.E2
      REAL, PRIVATE, PARAMETER :: RLIMIT = 1.E-32,  SLIMIT = 1.E-2
      REAL, PRIVATE, PARAMETER :: QSMAL1 = 1.E-9,   QSMALL = 1.E-14
      REAL, PRIVATE, PARAMETER :: QLIMIT = 1.E-6,   RSMALL = 1.E-20
      REAL, PRIVATE, PARAMETER :: ISMALL = 1.E-17,  AFU = 3.125E-1
      REAL, PRIVATE, PARAMETER :: BFU = 1.0552E-3,  CFU = -2.4023
      REAL, PRIVATE, PARAMETER :: BOLTZ = 1.38E-23, EFMIN = 1.E-7
      REAL, PRIVATE, PARAMETER :: VTZ0 = 5.83,      VTC0 = 0.6          ! SURFACE ROUGHNESS PARAMETERS
      REAL, PRIVATE, PARAMETER :: VTC1 = 0.151931,  VTC2 = VTZ0**2./4.  ! VTC1=4./(VTZ0**2.*VTC0**5.E-1)
      REAL, PRIVATE, PARAMETER :: VTC3 = 0.0901804, VTC4 = 20.5209
      REAL, PRIVATE, PARAMETER :: VTA0 = 1.7E-3,    VTB0 = 0.           ! VTB0 = 0.8
      REAL, PRIVATE, PARAMETER :: AVC0 = 3.E7,      BVC0 = 2.           ! Morrison scheme
      REAL, PRIVATE, PARAMETER :: AVR0 = 841.997,   BVR0 = 0.8          ! Morrison scheme
      REAL, PRIVATE, PARAMETER :: AVI0 = 700.,      BVI0 = 1.           ! Morrison scheme
      REAL, PRIVATE, PARAMETER :: AVS0 = 11.72,     BVS0 = 0.41         ! Locatelli and Hobbs (1974)
      REAL, PRIVATE, PARAMETER :: AVG0 = 19.3,      BVG0 = 0.37         ! Ferrier (1994)
      REAL, PRIVATE, PARAMETER :: VTCMAX = 1.,      VTSMAX = 10.
      REAL, PRIVATE, PARAMETER :: VTIMAX = 10.,     VTRMAX = 15.
      REAL, PRIVATE, PARAMETER :: VTGMAX = 20.,     AFAG0 = 0.
      REAL, PRIVATE, PARAMETER :: AFAC0 = 0.,       AFAR0 = 0.
      REAL, PRIVATE, PARAMETER :: AFAI0 = 3.,       AFAS0 = 0.
      REAL, PRIVATE, PARAMETER :: SASMAX = 1.,      SASMIN = 1.E-3
      REAL, PRIVATE, PARAMETER :: KCCMIN = 0.223,   KCCMAX = 0.99
      REAL, PRIVATE, PARAMETER :: KCRMIN = 0.223,   KCRMAX = 0.99
      REAL, PRIVATE, PARAMETER :: KCIMIN = 0.556,   KCIMAX = 0.99      ! KC = 0.999, AFAMAX = 2996
      REAL, PRIVATE, PARAMETER :: KCSMIN = 0.223,   KCSMAX = 0.99      ! KC = 0.995, AFAMAX = 596
      REAL, PRIVATE, PARAMETER :: KCGMIN = 0.223,   KCGMAX = 0.99      ! KC = 0.99,  AFAMAX = 296
      REAL, PRIVATE, PARAMETER :: AFAMAX = 30.,     AFAMIN = 0.
      REAL, PRIVATE, PARAMETER :: DCMIN = 5.E-7,    DCMAX = 1.E-4
      REAL, PRIVATE, PARAMETER :: DIMIN = 6.E-6,    DIMAX = 5.E-3
      REAL, PRIVATE, PARAMETER :: DRMIN = 1.E-4,    DRMAX = 6.E-3
      REAL, PRIVATE, PARAMETER :: DSMIN = 2.E-5,    DSMAX = 1.E-2
      REAL, PRIVATE, PARAMETER :: DGMIN = 5.E-5,    DGMAX = 2.E-2
      REAL, PRIVATE, PARAMETER :: RCMIN = DCMIN/2., RCMAX = DCMAX/2.
      REAL, PRIVATE, PARAMETER :: RRMIN = DRMIN/2., RRMAX = DRMAX/2.
      REAL, PRIVATE, PARAMETER :: DI0 = 6.E-6,      DCR = 100.E-6
      REAL, PRIVATE, PARAMETER :: MI0 = AMI0*DI0**3.
      REAL, PRIVATE, PARAMETER :: SIG1 = -6.2685,   SIG2 = -2.7312E-1
      REAL, PRIVATE, PARAMETER :: SIG3 = 2.2606E-1, MNR1 = -2.2920
      REAL, PRIVATE, PARAMETER :: MNR2 = -3.5158E-1, MNR3 = 3.4708E-1
      REAL, PRIVATE, PARAMETER :: EFC1 = -1.2560,    EFC2 = -1.7904E-02
      REAL, PRIVATE, PARAMETER :: EFC3 = 8.5536E-01, EFR1 = -9.9216E-02
      REAL, PRIVATE, PARAMETER :: EFR2 = 2.9490E-02, EFR3 = 9.9238E-01

      REAL, PRIVATE, PARAMETER :: MDC1 = -61.003515, MDC2 = 21.41875
      REAL, PRIVATE, PARAMETER :: MDC3 = 0.33306645, MDC4 = -1.7611021
      REAL, PRIVATE, PARAMETER :: MDC5 = 2.6253357E-3, MDC6 = -0.091487864
      REAL, PRIVATE, PARAMETER :: MDI1 = 150.72342, MDI2 = -54.136431
      REAL, PRIVATE, PARAMETER :: MDI3 = -0.13242867, MDI4 = 5.1248879
      REAL, PRIVATE, PARAMETER :: MDI5 = 3.75543E-4, MDI6 = -0.02301361
      REAL, PRIVATE, PARAMETER :: MDR1 = 33.999045, MDR2 = -13.813047
      REAL, PRIVATE, PARAMETER :: MDR3 = 0.29834969, MDR4 = 1.6622025
      REAL, PRIVATE, PARAMETER :: MDR5 = 1.2480129E-3, MDR6 = -0.104641
      REAL, PRIVATE, PARAMETER :: MDS1 = 87.049912, MDS2 = -41.711453
      REAL, PRIVATE, PARAMETER :: MDS3 = 2.1772518, MDS4 = 4.9657494
      REAL, PRIVATE, PARAMETER :: MDS5 = 3.5452471E-3, MDS6 = -0.45465148
      REAL, PRIVATE, PARAMETER :: MDG1 = 231.32157, MDG2 = -93.387843
      REAL, PRIVATE, PARAMETER :: MDG3 = 1.810578, MDG4 = 9.5907234
      REAL, PRIVATE, PARAMETER :: MDG5 = 1.683093E-3, MDG6 = -0.37378561
      REAL, PRIVATE, PARAMETER :: MDH1 = 12.676969, MDH2 = -2.8835883
      REAL, PRIVATE, PARAMETER :: MDH3 = -3.8803716E-2, MDH4 = 0.43033064
      REAL, PRIVATE, PARAMETER :: MDH5 = 6.53219E-3, MDH6 = -6.4118002E-2
      REAL, PRIVATE, PARAMETER :: ROS1 = 75.401034, ROS2 = -25.730216
      REAL, PRIVATE, PARAMETER :: ROS3 = 0.81467519, ROS4 = 2.339118
      REAL, PRIVATE, PARAMETER :: ROS5 = 1.9976667E-3, ROS6 = -0.14086434
      REAL, PRIVATE, PARAMETER :: ROS7 = -64808.666, ROS8 = 23113.508
      REAL, PRIVATE, PARAMETER :: ROS9 = -36.46632, ROS10 = -2060.6024
      REAL, PRIVATE, PARAMETER :: ROS11 = -0.005729458, ROS12 = 6.5057411
      REAL, PRIVATE, PARAMETER :: ROG1 = -9.57E-5, ROG2 = 3.077E-3
      REAL, PRIVATE, PARAMETER :: ROG3 = -6.800923E-2, ROG4 = 6.8175231
      REAL, PRIVATE, PARAMETER :: ISP1 = 0.009207, ISP2 = -0.150212
      REAL, PRIVATE, PARAMETER :: ISP3 = 0.565632, ISP4 = 0.401116
      REAL, PRIVATE, PARAMETER :: ASP1 = 1.0207483, ASP2 = -0.40684481
      REAL, PRIVATE, PARAMETER :: ASP3 = 0.043489251, ASP4 = 0.020462134
      REAL, PRIVATE, PARAMETER :: ASP5 = 0.014926001, ASP6 = -1.1252149E-3
      REAL, PRIVATE, PARAMETER :: AFA1 = -0.35897435, AFA2 = 0.010041278
      REAL, PRIVATE, PARAMETER :: AFA3 = 1.9349749E-3, AFA4 = -0.067453607
      REAL, PRIVATE, PARAMETER :: AFA5 = -5.5693923E-3, AFA6 = 0.06376154
      REAL, PRIVATE, PARAMETER :: AFB1 = -6.5100404, AFB2 = 0.039597245
      REAL, PRIVATE, PARAMETER :: AFB3 = 2.3169628E-3, AFB4 = -0.45856646
      REAL, PRIVATE, PARAMETER :: AFB5 = -0.04537631, AFB6 = 0.25713479

      REAL, DIMENSION(0:120) :: ITBLE
      DATA ITBLE /1.000000,0.979490,0.959401,0.939723,0.920450,        &
                  0.899498,0.879023,0.857038,0.833681,0.810961,        &
                  0.783430,0.755092,0.703072,0.537032,0.467735,        &
                  0.524807,0.630957,0.812831,1.096478,1.479108,        &
                  1.905461,2.089296,2.290868,2.398833,2.454709,        &
                  2.426610,2.371374,2.290868,2.137962,1.995262,        &
                  1.862087,1.737801,1.621810,1.513561,1.396368,        &
                  1.288250,1.188502,1.096478,1.000000,0.922571,        &
                  0.851138,0.785236,0.724436,0.668344,0.616595,        &
                  0.575440,0.537032,0.501187,0.467735,0.436516,        &
                  0.407380,0.380189,0.354813,0.331131,0.316228,        &
                  0.301995,0.291743,0.285102,0.281838,0.278612,        &
                  0.275423,0.278612,0.281838,0.285102,0.291743,        &
                  0.298538,0.309030,0.319890,0.331131,0.346737,        &
                  0.367282,0.393550,0.426580,0.457088,0.489779,        &
                  0.524807,0.562341,0.609537,0.660693,0.716143,        &
                  0.785236,0.860994,0.954993,1.047129,1.148154,        &
                  1.258925,1.380384,1.496236,1.603245,1.698244,        &
                  1.778279,1.840772,1.883649,1.905461,1.905461,        &
                  1.883649,1.862087,1.840772,1.798871,1.737801,        &
                  1.698244,1.640590,1.584893,1.548817,1.513561,        &
                  1.475707,1.452112,1.428894,1.412538,1.393157,        &
                  1.377209,1.361445,1.348963,1.336596,1.327394,        &
                  1.318257,1.309182,1.303167,1.294196,1.288250,1.279381/
      REAL, DIMENSION(0:167) :: IECC
      DATA IECC /0.00,0.03,0.29,0.25,0.00,0.00,0.00,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,    &
                 0.00,0.00,0.10,0.42,0.50,0.47,0.21,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.15,0.49,0.56,0.55,0.46,0.10,0.00,    &
                 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.00,0.20,0.52,0.61,0.62,0.59,0.49,    &
                 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.00,0.00,0.25,0.62,0.72,0.75,0.74,    &
                 0.71,0.68,0.57,0.00,0.00,0.00,0.00,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.00,0.00,0.00,0.30,0.70,0.80,0.84,    &
                 0.85,0.85,0.84,0.83,0.81,0.77,0.69,0.10,0.00,0.00,    &
                 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.35,0.75,0.85,    &
                 0.89,0.91,0.92,0.92,0.91,0.90,0.89,0.87,0.85,0.82,    &
                 0.71,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.40,0.80,    &
                 0.89,0.93,0.94,0.95,0.95,0.95,0.95,0.95,0.95,0.94,    &
                 0.93,0.92,0.91,0.88,0.80,0.15,0.00,0.00/
      REAL, DIMENSION(0:167) :: IEPC
      DATA IEPC /0.00,0.00,0.13,0.41,0.00,0.00,0.00,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.25,0.54,0.56,0.39,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.08,0.57,0.75,0.83,0.85,0.86,0.84,    &
                 0.78,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.00,0.10,0.65,0.80,0.87,0.90,0.91,    &
                 0.91,0.91,0.88,0.78,0.00,0.00,0.00,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.00,0.00,0.12,0.67,0.81,0.88,0.91,    &
                 0.92,0.93,0.93,0.92,0.91,0.89,0.81,0.00,0.00,0.00,    &
                 0.00,0.00,0.00,0.00,0.00,0.00,0.15,0.68,0.82,0.89,    &
                 0.91,0.93,0.94,0.94,0.95,0.95,0.95,0.94,0.91,0.83,    &
                 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.18,0.69,0.82,    &
                 0.89,0.92,0.94,0.95,0.95,0.96,0.96,0.96,0.96,0.95,    &
                 0.94,0.91,0.80,0.00,0.00,0.00,0.00,0.00,0.22,0.70,    &
                 0.83,0.90,0.93,0.95,0.96,0.97,0.98,0.98,0.98,0.98,    &
                 0.97,0.96,0.94,0.92,0.84,0.00,0.00,0.00/
      REAL, PRIVATE, PARAMETER :: SIXD = -1./6.
      REAL, PRIVATE, PARAMETER :: TVC1 = 19.143801, TVC2 = -0.35694332
      REAL, PRIVATE, PARAMETER :: TVC3 = 1.4581512, TVC4 = -0.35745454
      REAL, PRIVATE, PARAMETER :: TVC5 = -0.25602832, TVC6 = 0.46010112
      REAL, PRIVATE, PARAMETER :: TVC7 = 1.2409587E-3, TVC8 = 6.27915E-3
      REAL, PRIVATE, PARAMETER :: TVC9 = -1.7692982E-2, TVC10 = 1.8450109E-2
      REAL, PRIVATE, PARAMETER :: TVR1 = 3.9042777, TVR2 = -1.7108382
      REAL, PRIVATE, PARAMETER :: TVR3 = 2.079987, TVR4 = -0.43640423
      REAL, PRIVATE, PARAMETER :: TVR5 = -0.26248077, TVR6 = 0.58061442
      REAL, PRIVATE, PARAMETER :: TVR7 = 2.8473168E-4, TVR8 = 4.5413262E-3
      REAL, PRIVATE, PARAMETER :: TVR9 = -1.6518188E-2, TVR10 = 1.9159654E-2
      REAL, PRIVATE, PARAMETER :: TVI1 = 6.2503876, TVI2 = -2.3672621
      REAL, PRIVATE, PARAMETER :: TVI3 = 2.8701539, TVI4 = -0.53583075
      REAL, PRIVATE, PARAMETER :: TVI5 = -0.36592677, TVI6 = 0.76858786
      REAL, PRIVATE, PARAMETER :: TVI7 = -3.1184644E-3, TVI8 = 9.0974178E-3
      REAL, PRIVATE, PARAMETER :: TVI9 = -2.9301941E-2, TVI10 = 3.1551241E-2
      REAL, PRIVATE, PARAMETER :: TVS1 = 11.340988, TVS2 = 1.3909409
      REAL, PRIVATE, PARAMETER :: TVS3 = -0.95275936, TVS4 = -1.128589E-2
      REAL, PRIVATE, PARAMETER :: TVS5 = 0.12025388, TVS6 = -0.21512648
      REAL, PRIVATE, PARAMETER :: TVS7 = 1.825244E-2, TVS8 = -1.0767504E-2
      REAL, PRIVATE, PARAMETER :: TVS9 = 3.159366E-2, TVS10 = -3.1975557E-2
      REAL, PRIVATE, PARAMETER :: AVX1 = 4.7457886, AVX2 = 0.92280056
      REAL, PRIVATE, PARAMETER :: AVX3 = -0.92255288, AVX4 = 7.9043734E-2
      REAL, PRIVATE, PARAMETER :: AVX5 = 5.9176938E-2, AVX6 = -0.12956678
      REAL, PRIVATE, PARAMETER :: AVX7 = 0.20479573, AVX8 = -0.19214126
      REAL, PRIVATE, PARAMETER :: AVX9 = 1.1777632E-2, AVX10 = 1.0020604E-2
      REAL, PRIVATE, PARAMETER :: AVX11 = -2.1600193E-2
      REAL, PRIVATE, PARAMETER :: AVY1 = 0.27573, AVY2 = -0.48822051
      REAL, PRIVATE, PARAMETER :: AVY3 = 0.45512963, AVY4 = 2.7896597E-2
      REAL, PRIVATE, PARAMETER :: AVY5 = -0.21716796, AVY6 = 0.14683485
      REAL, PRIVATE, PARAMETER :: AVY7 = -1.2330147E-3, AVY8 = 2.4876837E-2
      REAL, PRIVATE, PARAMETER :: AVY9 = -1.3548357E-2, AVY10 = -8.3478882E-4
      REAL, PRIVATE, PARAMETER :: BVX1 = 0.53870773, BVX2 = 8.3720637E-2
      REAL, PRIVATE, PARAMETER :: BVX3 = -9.6585127E-2, BVX4 = 9.6295511E-3
      REAL, PRIVATE, PARAMETER :: BVX5 = 6.3328852E-3, BVX6 = -1.3736835E-2
      REAL, PRIVATE, PARAMETER :: BVX7 = 0.19838252, BVX8 = -0.18921664
      REAL, PRIVATE, PARAMETER :: BVX9 = 1.2491957E-2, BVX10 = 9.9099794E-3
      REAL, PRIVATE, PARAMETER :: BVX11 = -2.1563555E-2
      REAL, PRIVATE, PARAMETER :: BVY1 = 0.99428907, BVY2 = -0.18786418
      REAL, PRIVATE, PARAMETER :: BVY3 = -0.17868626, BVY4 = 1.3016182E-2
      REAL, PRIVATE, PARAMETER :: BVY5 = 1.3809212E-2, BVY6 = 7.96172E-3
      REAL, PRIVATE, PARAMETER :: BVY7 = -0.19249585, BVY8 = -0.18015756
      REAL, PRIVATE, PARAMETER :: BVY9 = 1.3125865E-2, BVY10 = 1.3979134E-2
      REAL, PRIVATE, PARAMETER :: BVY11 = 8.4353006E-3

      REAL, PRIVATE, PARAMETER :: M_a = -10.2000                        ! ac = 1.
      REAL, PRIVATE, PARAMETER :: M_b = 0.960640                        ! ac = 1.
      REAL, PRIVATE, PARAMETER :: M_c = 0.754681                        ! ac = 1.
      REAL, PRIVATE, PARAMETER :: M_d = 4.7233E-3                       ! ac = 1.

      CONTAINS
!======================================================================
!      SUBROUTINE tcwa2_init

!      END SUBROUTINE tcwa2_init
!======================================================================
!
!======================================================================
      REAL FUNCTION POLYSVP(T,TYPE)
!======================================================================
!  COMPUTE SATURATION VAPOR PRESSURE POLYSVP RETURNED IN UNITS OF PA. T IS INPUT IN UNITS OF K.
!  TYPE REFERS TO SATURATION WITH RESPECT TO LIQUID (0) OR ICE (1)
!  REPLACE GOFF-GRATCH WITH FASTER FORMULATION FROM FLATAU ET AL. 1992, TABLE 4 (RIGHT-HAND COLUMN)
      IMPLICIT NONE
      REAL :: DUM,T,DT
      INTEGER :: TYPE
! ice
      REAL :: a0i,a1i,a2i,a3i,a4i,a5i,a6i,a7i,a8i 
      DATA a0i,a1i,a2i,a3i,a4i,a5i,a6i,a7i,a8i /6.11147274,0.503160820,&
           0.188439774E-1,0.420895665E-3,0.615021634E-5,0.602588177E-7,&
           0.385852041E-9,0.146898966E-11,0.252751365E-14/
! liquid
      REAL :: a0,a1,a2,a3,a4,a5,a6,a7,a8 
      DATA a0,a1,a2,a3,a4,a5,a6,a7,a8 /6.11239921,0.443987641,         &
           0.142986287E-1,0.264847430E-3,0.302950461E-5,0.206739458E-7,&
           0.640689451E-10,-0.952447341E-13,-0.976195544E-15/
      IF (TYPE.EQ.1) THEN                                               ! ICE
         DT = MAX(-80.,T-273.16)
         POLYSVP = a0i+DT*(a1i+DT*(a2i+DT*(a3i+DT*(a4i+DT*             &
                          (a5i+DT*(a6i+DT*(a7i+a8i*DT))))))) 
         POLYSVP = POLYSVP*100.
      ENDIF
      IF (TYPE.EQ.0) THEN                                               ! LIQUID
         DT = MAX(-80.,T-273.16)
         POLYSVP = a0+DT*(a1+DT*(a2+DT*(a3+DT*(a4+DT*(a5+DT*           &
                         (a6+DT*(a7+a8*DT)))))))
         POLYSVP = POLYSVP*100.
      ENDIF

      END FUNCTION POLYSVP
!======================================================================
!
!=======================================================================
      REAL FUNCTION GAMMP(A,X)
!=======================================================================
      IMPLICIT NONE
      REAL :: A,X,GAMMCF,GAMSER,GLN

      IF (X.LT.0..OR.A.LE.0.) &
         CALL wrf_error_fatal('warning : bad arguments in gammq')
      IF (X.LT.A+1.) THEN
         CALL GSER(GAMSER,A,X,GLN)
         GAMMP = GAMSER
      ELSE
         CALL CFG(GAMMCF,A,X,GLN)
         GAMMP = 1.-GAMMCF
      ENDIF
      RETURN

      END FUNCTION GAMMP
!======================================================================
!
!======================================================================
      SUBROUTINE GSER(GAMSER,A,X,GLN)
!======================================================================
      IMPLICIT NONE
      INTEGER :: N
      REAL :: A,GAMSER,GLN,X,AP,de1,summ
      INTEGER, PARAMETER :: ITMAX = 500
      REAL, PARAMETER :: EPS = 3.E-7

      GLN = LOG_GAMMA(A)
      IF (X.LE.0.) THEN
         IF (X.LT.0.) CALL wrf_error_fatal ( 'WARNING: X <0 in GSER' )
         GAMSER = 0.
         RETURN
      ENDIF
      AP = A
      summ = 1./A
      de1 = summ
      DO N = 1,ITMAX
         AP = AP+1.
         de1 = de1*X/AP
         summ = summ+de1
         IF (ABS(de1).LT.ABS(summ)*EPS) GOTO 777
      ENDDO
      CALL wrf_error_fatal ('Warning : ITMAX too small in GSER')
 777  GAMSER = summ*EXP(-X+A*LOG(X)-GLN)

      RETURN

      END SUBROUTINE GSER
!=======================================================================
      SUBROUTINE CFG(GAMMCF,A,X,GLN)
!=======================================================================
      IMPLICIT NONE
      INTEGER :: I
      REAL :: A,GAMMCF,GLN,X,AN,b,c,d,de1,h
      INTEGER, PARAMETER :: ITMAX = 500
      REAL, PARAMETER :: EPS = 3.E-7
      REAL, PARAMETER :: fpmin = 1.E-30

      GLN = LOG_GAMMA(A)
      b = X+1.-A
      c = 1./fpmin
      d = 1./b
      h = d
      DO I = 1,ITMAX
         AN = -I*(I-A)
         b = b+2.
         d = AN*d+b
         IF (ABS(d).LT.fpmin) d = fpmin
         c = b+AN/c
         IF (ABS(c).LT.fpmin) c = fpmin
         d = 1./d
         de1 = d*c
         h = h*de1
         IF (ABS(de1-1.).LT.EPS) GOTO 888
      ENDDO
      CALL wrf_error_fatal ('Warning : ITMAX too small in gcf')
 888  GAMMCF = EXP(-X+A*LOG(X)-GLN)*h
      RETURN

      END SUBROUTINE CFG
!======================================================================
!
!======================================================================
      SUBROUTINE SOLVE_AFAC(TK1D,QC1D,NC1D,LAMC,MVDC,AFAC,AVC,BVC)
!======================================================================
      IMPLICIT NONE
      REAL :: TK1D,QC1D,NC1D,LAMC,GC1,MVDC,AFAC,LAMCMIN,LAMCMAX,   &
              C3M1D,SIGC,MNRC,MVRC,EFRC,KDX,LTK,LQC,AVC,BVC,LLMC,LLMC2,&
              LLMC3,LAC2,LAC22,LAC23,MDC,GC4

      IF (NC1D.LT.NSMALL) THEN
         LTK  = LOG(TK1D)
         LQC  = -1.*LOG(QC1D)
         MDC  = EXP(MDC1+MDC2*LTK+MDC3*LQC+MDC4*LTK*LTK+MDC5*LQC*LQC+  &
                MDC6*LTK*LQC)/1.E3
         NC1D = 1.E9*QC1D*V2M3/RHOW/MDC**3.
      ENDIF
      IF (QC1D.GE.QSMAL1.AND.NC1D.LT.NSMAL1) THEN
         LTK  = LOG(TK1D)
         LQC  = -1.*LOG(QC1D)
         MDC  = EXP(MDC1+MDC2*LTK+MDC3*LQC+MDC4*LTK*LTK+MDC5*LQC*LQC+  &
                MDC6*LTK*LQC)/1.E3
         NC1D = 1.E9*QC1D*V2M3/RHOW/MDC**3.
      ENDIF
      C3M1D = QC1D*V2M3/RHOW
      IF (AFAC_3M.EQ.0) THEN
         AFAC = AFAC0
      ELSEIF (AFAC_3M.EQ.1) THEN
         MVRC = (QC1D/NC1D/C4PI3W)**THRD
         MVRC = MIN(MAX(MVRC,RCMIN),RCMAX)
         EFRC = EXP(EFC1+EFC2*LOG(NC1D)+EFC3*LOG(MVRC))
         KDX  = MAX(KCCMIN,MIN(KCCMAX,(MVRC/EFRC)**3.))
         AFAC = (6.*KDX-3.+SQRT(8.*KDX+1.))/(2.-2.*KDX)
         AFAC = MIN(MAX(AFAC,AFAMIN),AFAMAX)
      ELSEIF (AFAC_3M.EQ.2) THEN
         SIGC = EXP(SIG1+SIG2*LOG(NC1D)+SIG3*LOG(QC1D))
         MNRC = EXP(MNR1+MNR2*LOG(NC1D)+MNR3*LOG(QC1D))
         AFAC = MIN(MAX(SIGC/MNRC,AFAMIN),AFAMAX)
      ENDIF
      GC1     = LOG_GAMMA(AFAC+1.)
      GC4     = LOG_GAMMA(AFAC+4.)
      LAMC    = (EXP(GC4-GC1)*NC1D/C3M1D)**THRD
      LAMCMIN = (EXP(GC4-GC1))**THRD/DCMAX
      LAMCMAX = (EXP(GC4-GC1))**THRD/DCMIN
      IF (LAMC.LT.LAMCMIN) THEN
         LAMC = LAMCMIN
         NC1D = C3M1D*EXP(GC1-GC4+3.*LOG(LAMC))
      ELSEIF (LAMC.GT.LAMCMAX) THEN
         LAMC = LAMCMAX
         NC1D = C3M1D*EXP(GC1-GC4+3.*LOG(LAMC))
      ENDIF
      MVDC = (EXP(GC4-GC1))**THRD/LAMC
      IF (LIQ_VTC.EQ.0) THEN
         AVC = AVC0
         BVC = BVC0
      ELSEIF (LIQ_VTC.EQ.1) THEN
         LAC2  = LOG(AFAC+2.)
         LAC22 = LAC2*LAC2
         LAC23 = LAC22*LAC2
         LLMC  = LOG(LAMC)
         LLMC2 = LLMC*LLMC
         LLMC3 = LLMC2*LLMC
         IF (MVDC.GE.5.E-6) THEN
            BVC = MAX(1.7,MIN(2.,-5.0261982-1.8694889*LAC2+1.6077435*  &
                  LLMC-0.13716486*LAC22-0.12070109*LLMC2+0.27145343*   &
                  LAC2*LLMC-3.4422505E-3*LAC23+2.9798275E-3*LLMC3-     &
                  9.8095737E-3*LAC2*LLMC2+9.9805004E-3*LAC22*LLMC))
         ELSE
            BVC = 2.
         ENDIF
         AVC = EXP(-58.222888-19.207953*LAC2+17.128123*LLMC-1.4214909* &
               LAC22-1.2791527*LLMC2+2.8018451*LAC2*LLMC-3.6471975E-2* &
               LAC23+3.1452321E-2*LLMC3-0.10186224*LAC2*LLMC2+         &
               0.10459436*LAC22*LLMC)
         AVC = MIN(AVC,VTCMAX*EXP(GC4+BVC*LLMC-LOG_GAMMA(BVC+AFAC+4.)))
      ENDIF

      END SUBROUTINE SOLVE_AFAC
!======================================================================
!
!======================================================================
      SUBROUTINE SOLVE_AFAR(TK1D,QC1D,QR1D,NR1D,LAMR,MVDR,AFAR,RASPR,  &
                 AVR,BVR)
!======================================================================
      IMPLICIT NONE
      REAL :: TK1D,QR1D,NR1D,LAMR,GR1,MVDR,LAMRMIN,LAMRMAX,R3M1D,  &
              AFAR,BDR,MVRR,EFRR,KDX,LTK,LQR,AVR,BVR,LLMR,LLMR2,LLMR3, &
              LAR2,LAR22,LAR23,RASPR,LAR5,MDR,QC1D,GR4

      IF (NR1D.LT.NSMALL) THEN
         LTK  = LOG(TK1D)
         LQR  = -1.*LOG(QR1D)
         MDR  = EXP(MDR1+MDR2*LTK+MDR3*LQR+MDR4*LTK*LTK+MDR5*LQR*LQR+  &
                MDR6*LTK*LQR)/1.E3
         NR1D = 1.E9*QR1D*V2M3/RHOW/MDR**3.
      ENDIF
      R3M1D = QR1D*V2M3/RHOW
      IF (AFAR_3M.EQ.0) THEN
         AFAR = AFAR0
      ELSEIF (AFAR_3M.EQ.1) THEN
         MVRR = MIN(RRMAX,MAX(RRMIN,(QR1D/NR1D/C4PI3W)**THRD))
         IF (QC1D.GE.QSMALL.OR.NR1D.LT.0.5) THEN
            AFAR = MIN(AFAMAX,MAX(AFAMIN,(AFA1+AFA2*LOG(NR1D)+AFA3*    &
                   LOG(NR1D)**2.+AFA4*LOG(MVRR))/(1.+AFA5*LOG(NR1D)+   &
                   AFA6*LOG(MVRR))))
         ELSE
            AFAR = MIN(AFAMAX,MAX(AFAMIN,(AFB1+AFB2*LOG(NR1D)+AFB3*    &
                   LOG(NR1D)**2.+AFB4*LOG(MVRR))/(1.+AFB5*LOG(NR1D)+   &
                   AFB6*LOG(MVRR))))
         ENDIF
         IF ((NR1D*MVRR**6.).GE.1.E-16) THEN
            IF (NR1D.GE.3.8E3) THEN         
               AFAR = AFAMAX
            ELSE
               AFAR = 0.5*AFAR
            ENDIF
         ENDIF
      ELSEIF (AFAR_3M.EQ.2) THEN
         BDR  = MIN(MAX((R3M1D/NR1D)**THRD,DRMIN),DRMAX)
         AFAR = MAX(AFAMIN,19.*TANH(0.6*(1.E3*BDR-1.8))+17.)
      ENDIF
      GR1     = LOG_GAMMA(AFAR+1.)
      GR4     = LOG_GAMMA(AFAR+4.)
      LAMR    = (EXP(GR4-GR1)*NR1D/R3M1D)**THRD
      LAMRMIN = (EXP(GR4-GR1))**THRD/DRMAX
      LAMRMAX = (EXP(GR4-GR1))**THRD/DRMIN
      IF (LAMR.LT.LAMRMIN) THEN
         LAMR = LAMRMIN
         NR1D = R3M1D*EXP(GR1-GR4+3.*LOG(LAMR))
      ELSEIF (LAMR.GT.LAMRMAX) THEN
         LAMR = LAMRMAX
         NR1D = R3M1D*EXP(GR1-GR4+3.*LOG(LAMR))
      ENDIF
      MVDR = (EXP(GR4-GR1))**THRD/LAMR
      IF (RAIN_SHAPE.EQ.0) THEN
         RASPR = 1.
      ELSEIF (RAIN_SHAPE.EQ.1) THEN
         LLMR  = LOG(LAMR)
         LAR5  = LOG(AFAR+5.)
         RASPR = MIN(1.,(-32.232528-16.735319*LAR5+14.830389*LLMR-     &
                 1.8312883*LAR5**2.-1.6827601*LLMR**2.+3.6556064*LAR5* &
                 LLMR-7.4526392E-2*LAR5**3.+6.2393644E-2*LLMR**3.-     &
                 0.20029785*LAR5*LLMR**2.+0.20699596*LAR5**2.*LLMR)/10.)
      ENDIF
      IF (LIQ_VTR.EQ.0) THEN
         AVR = AVR0
         BVR = BVR0
      ELSEIF (LIQ_VTR.EQ.1) THEN
         LAR2  = LOG(AFAR+2.)
         LAR22 = LAR2*LAR2
         LAR23 = LAR22*LAR2
         LLMR  = LOG(LAMR)
         LLMR2 = LLMR*LLMR
         LLMR3 = LLMR2*LLMR
         BVR  = MAX(0.6,MIN(2.,5.7968956+2.0351912*LAR2-2.2590687*     &
                LLMR+0.42145694*LAR22+0.32980961*LLMR2-0.68616779*     &
                LAR2*LLMR+1.5334834E-2*LAR23-1.4014021E-2*LLMR3+       &
                4.4802292E-2*LAR2*LLMR2-4.9353351E-2*LAR22*LLMR))
         AVR  = EXP(45.43683+18.220453*LAR2-18.360234*LLMR+3.3500426*  &
                LAR22+2.694486*LLMR2-5.7437089*LAR2*LLMR+0.12341375*   &
                LAR23-0.11349924*LLMR3+0.36417943*LAR2*LLMR2-          &
                0.39128002*LAR22*LLMR)
         AVR  = MIN(AVR,VTRMAX*EXP(GR4+BVR*LLMR-LOG_GAMMA(BVR+AFAR+4.)))
      ENDIF

      END SUBROUTINE SOLVE_AFAR
!======================================================================

!======================================================================
      SUBROUTINE SOLVE_AFAI(TK1D,P1D,QV1D,QC1D,QI1D,NI1D,ADAGR,ZETA,   &
                 LAMI,AFAI,MVDI,RHOI,AMI,AVI,BVI)
!======================================================================
      IMPLICIT NONE
      INTEGER :: HID
      REAL :: TK1D,P1D,QV1D,QI1D,NI1D,ADAGR,ZETA,RHOI,LAMI,AFAI,   &
              AMI,AVI,BVI,MVDI,GI1,TC1D,ESW,ESI,QVSI,INHGR,LTK,LQI,BDI,&
              FDI,LBDI,MDI,IASPR,LAMIMIN,LAMIMAX,IPF,IPG,KDX,LLMI,     &
              LLMI2,LAI2,LAI22,RHOX,MVRI,QC1D,TTR,GI4

      IF (ICE_RHOI.EQ.0) THEN
         RHOI = RHOI0
      ELSEIF (ICE_RHOI.EQ.1) THEN
         TC1D = TK1D-TK0C
         HID  = MAX(MIN(NINT(ABS(TC1D)/0.25),120),0)
         ESW  = MIN(0.99*P1D,POLYSVP(TK1D,0))
         ESI  = MIN(0.99*P1D,POLYSVP(TK1D,1))
         IF (ESI.GT.ESW) ESI = ESW
         QVSI = 0.622*ESI/(P1D-ESI)
         INHGR = ITBLE(HID)
         RHOI = RHOI0*EXP(-3.*MAX((QV1D-QVSI)-5.E-5,0.)/INHGR)
      ENDIF
      IF (NI1D.LT.NSMALL) THEN
         LTK  = LOG(TK1D)
         LQI  = -1.*LOG(QI1D)
         MDI  = EXP(MDI1+MDI2*LTK+MDI3*LQI+MDI4*LTK*LTK+MDI5*LQI*LQI+  &
                MDI6*LTK*LQI)/1.E3
         NI1D = 1.E9*QI1D*V2M3/RHOI/MDI**3.
      ENDIF
      IF (QI1D.GE.QSMAL1.AND.NI1D.LT.NSMAL1) THEN
         LTK  = LOG(TK1D)
         LQI  = -1.*LOG(QI1D)
         MDI  = EXP(MDI1+MDI2*LTK+MDI3*LQI+MDI4*LTK*LTK+MDI5*LQI*LQI+  &
                MDI6*LTK*LQI)/1.E3
         NI1D = 1.E9*QI1D*V2M3/RHOI/MDI**3.
      ENDIF
      IF (AFAI_3M.EQ.0) THEN
         AFAI = AFAI0
      ELSEIF (AFAI_3M.EQ.1) THEN
         MVRI = MIN(DIMAX/2.,MAX(DIMIN/2.,(QI1D/NI1D/RHOI/C4PI3)**THRD))
         IF (QC1D.GE.QSMALL.OR.NI1D.LT.0.5) THEN
            AFAI = MIN(AFAMAX,MAX(AFAMIN,(AFA1+AFA2*LOG(NI1D)+AFA3*    &
                   LOG(NI1D)**2.+AFA4*LOG(MVRI))/(1.+AFA5*LOG(NI1D)+   &
                   AFA6*LOG(MVRI))))
         ELSE
            AFAI = MIN(AFAMAX,MAX(AFAMIN,(AFB1+AFB2*LOG(NI1D)+AFB3*    &
                   LOG(NI1D)**2.+AFB4*LOG(MVRI))/(1.+AFB5*LOG(NI1D)+   &
                   AFB6*LOG(MVRI))))
         ENDIF
      ENDIF
      GI1  = LOG_GAMMA(AFAI+1.)
      GI4  = LOG_GAMMA(AFAI+4.)
      LAMI = (EXP(GI4-GI1)*PI*RHOI*NI1D/QI1D/6.)**THRD
      LAMIMIN = (EXP(GI4-GI1))**THRD/DIMAX
      LAMIMAX = (EXP(GI4-GI1))**THRD/DIMIN
      IF (LAMI.LT.LAMIMIN) THEN
         LAMI = LAMIMIN
         NI1D = QI1D*V2M3/RHOI*EXP(GI1-GI4+3.*LOG(LAMI))
      ELSEIF (LAMI.GT.LAMIMAX) THEN
         LAMI = LAMIMAX
         NI1D = QI1D*V2M3/RHOI*EXP(GI1-GI4+3.*LOG(LAMI))
      ENDIF
      MVDI = (EXP(GI4-GI1))**THRD/LAMI
      IF (ICE_SHAPE.EQ.0) THEN
         ADAGR = 1.
      ELSEIF (ICE_SHAPE.EQ.1) THEN
         IF (MVDI.GE.DI0) THEN
            LBDI  = LOG(MVDI*1.E6)
            TC1D  = TK1D-TK0C
            HID   = MAX(MIN(NINT(ABS(TC1D)/0.25),120),0)
            INHGR = ITBLE(HID)
            TTR   = MIN(1.,MAX(0.01,ISP1*LBDI**3.+ISP2*LBDI**2.+ISP3*  &
                    LBDI+ISP4))
            ADAGR = INHGR**TTR
         ELSE
            ADAGR = 1.
         ENDIF
      ENDIF
      ZETA = (ADAGR-1.)/(ADAGR+2.)
      AMI  = PI*RHOI/6.
      IF (ICE_VTI.EQ.0) THEN
         AVI = AVI0
         BVI = BVI0
      ELSEIF (ICE_VTI.EQ.1) THEN
         LAI2  = LOG(AFAI+2.)
         LAI22 = LAI2*LAI2
         LLMI  = LOG(LAMI)
         LLMI2 = LLMI*LLMI
         IF (MVDI.GT.DI0) THEN
            BVI = MAX(0.6,MIN(2.,(1.1672061+0.29961176*LAI2-           &
                  0.32111992*LLMI+2.9231253E-2*LAI22+2.5582889E-2*     &
                  LLMI2-5.1957511E-2*LAI2*LLMI)/(1.+0.23004501*LAI2-   &
                  0.22880535*LLMI+1.7796996E-2*LAI22+1.555279E-2*      &
                  LLMI2-3.2232357E-2*LAI2*LLMI)))
         ELSE
            BVI = 2.
         ENDIF
         AVI = EXP((8.9688466+2.3629275*LAI2-2.4224483*LLMI+0.2218761* &
               LAI22+0.19151978*LLMI2-0.39781101*LAI2*LLMI)/(1.+       &
               0.22199152*LAI2-0.21793693*LLMI+1.5890892E-2*LAI22+     &
               1.4198651E-2*LLMI2-2.955316E-2*LAI2*LLMI))
         AVI = MIN(AVI,VTIMAX*EXP(GI4+BVI*LLMI-LOG_GAMMA(BVI+AFAI+4.)))
         RHOX = VXRHO(MVDI,RHOI,BVI)
         IF (ZETA.GT.0.) THEN
            AVI = AVI*DI0**(ZETA/2.)*RHOX
         ELSEIF (ZETA.LT.0.) THEN
            AVI = AVI*DI0**(-ZETA)*RHOX
         ELSE
            AVI = AVI*RHOX
         ENDIF
      ENDIF

      END SUBROUTINE SOLVE_AFAI
!======================================================================
!
!======================================================================
      SUBROUTINE SOLVE_AFAS(TK1D,QS1D,QC1D,NS1D,AFAS,LAMS,MVDS,RHOS,   &
                 SASPR,AMS,AVS,BVS)
!======================================================================
      IMPLICIT NONE
      INTEGER :: TBIN,DBIN
      REAL :: TK1D,QS1D,QC1D,NS1D,AFAS,RHOS,SASPR,LAMS,AMS,AVS,BVS,&
              MVDS,LAMSMIN,LAMSMAX,S3M1D,KDX,TC1D,BDS,FDS,LTK,LQS,LBDS,&
              MDS,LLMS,LLMS2,LLMS3,LAS2,LAS22,LMDS,LMDS2,LROS,LROS2,   &
              RHOX,GS1,MVRS,GS4

      IF (ICE_RHOS.EQ.0) THEN
         RHOS = RHOS0
      ELSEIF (ICE_RHOS.EQ.1) THEN
         LTK = LOG(TK1D)
         LQS = -1.*LOG(QS1D)
         IF (TK1D.LT.TK0C) THEN
            RHOS = MIN(900.,EXP(ROS1+ROS2*LTK+ROS3*LQS+ROS4*LTK**2.+   &
                   ROS5*LQS**2.+ROS6*LTK*LQS))
         ELSE
            RHOS = MAX(100.,EXP(ROS7+ROS8*LTK+ROS9*LQS+ROS10*LTK**2.+  &
                   ROS11*LQS**2.+ROS12*LTK*LQS))
         ENDIF
         RHOS = MIN(MAX(RHOS,100.),900.)
      ENDIF
      IF (NS1D.LT.NSMALL) THEN
         LTK  = LOG(TK1D)
         LQS  = -1.*LOG(QS1D)
         MDS  = EXP(MDS1+MDS2*LTK+MDS3*LQS+MDS4*LTK*LTK+MDS5*LQS*LQS+  &
                MDS6*LTK*LQS)/1.E3
         NS1D = 1.E9*QS1D*V2M3/RHOS/MDS**3.
      ENDIF
      AMS   = PI*RHOS/6.
      S3M1D = QS1D/AMS
      IF (AFAS_3M.EQ.0) THEN
         AFAS  = AFAS0
      ELSEIF (AFAS_3M.EQ.1) THEN
         MVRS = MIN(DSMAX/2.,MAX(DSMIN/2.,(QS1D/NS1D/RHOS/C4PI3)**THRD))
         IF (QC1D.GE.QSMALL.OR.NS1D.LT.0.5) THEN
            AFAS = MIN(AFAMAX,MAX(AFAMIN,(AFA1+AFA2*LOG(NS1D)+AFA3*    &
                   LOG(NS1D)**2.+AFA4*LOG(MVRS))/(1.+AFA5*LOG(NS1D)+   &
                   AFA6*LOG(MVRS))))
         ELSE
            AFAS = MIN(AFAMAX,MAX(AFAMIN,(AFB1+AFB2*LOG(NS1D)+AFB3*    &
                   LOG(NS1D)**2.+AFB4*LOG(MVRS))/(1.+AFB5*LOG(NS1D)+   &
                   AFB6*LOG(MVRS))))
         ENDIF
         IF (TK1D.GT.TK0C) AFAS = MIN(AFAMAX,0.01*EXP(-1.*LOG(MVRS)))
      ENDIF
      GS1  = LOG_GAMMA(AFAS+1.)
      GS4  = LOG_GAMMA(AFAS+4.)
      LAMS = (EXP(GS4-GS1)*NS1D/S3M1D)**THRD
      LAMSMIN = (EXP(GS4-GS1))**THRD/DSMAX
      LAMSMAX = (EXP(GS4-GS1))**THRD/DSMIN
      IF (LAMS.LT.LAMSMIN) THEN
         LAMS = LAMSMIN
         NS1D = S3M1D*EXP(GS1-GS4+3.*LOG(LAMS))
      ELSEIF (LAMS.GT.LAMSMAX) THEN
         LAMS = LAMSMAX
         NS1D = S3M1D*EXP(GS1-GS4+3.*LOG(LAMS))
      ENDIF
      MVDS = (EXP(GS4-GS1))**THRD/LAMS
      IF (AGG_SHAPE.EQ.0) THEN
         SASPR = 1.
      ELSEIF (AGG_SHAPE.EQ.1) THEN
         LBDS  = LOG(MVDS*1.E6)
         LROS  = LOG(RHOS)
         SASPR = MAX(0.2,MIN(SASMAX,ASP1+ASP2*LROS+ASP3*LROS**2.+ASP4* &
                 LBDS+ASP5*LBDS**2.+ASP6*LBDS**3.))
      ENDIF
      IF (ICE_VTS.EQ.0) THEN
         AVS = AVS0
         BVS = BVS0
      ELSEIF (ICE_VTS.EQ.1) THEN
         LAS2  = LOG(AFAS+2.)
         LAS22 = LAS2*LAS2
         LLMS  = LOG(LAMS)
         LLMS2 = LLMS*LLMS
         IF (MVDS.GE.3.E-5) THEN
            BVS = MAX(0.58,MIN(2.,(BVX1+BVX2*LAS2+BVX3*LLMS+BVX4*LAS22+&
                  BVX5*LLMS2+BVX6*LAS2*LLMS)/(1.+BVX7*LAS2+BVX8*LLMS+  &
                  BVX9*LAS22+BVX10*LLMS2+BVX11*LAS2*LLMS)))
         ELSE
            BVS = 2.
         ENDIF
         AVS = EXP((AVX1+AVX2*LAS2+AVX3*LLMS+AVX4*LAS22+AVX5*LLMS2+    &
               AVX6*LAS2*LLMS)/(1.+AVX7*LAS2+AVX8*LLMS+AVX9*LAS22+     &
               AVX10*LLMS2+AVX11*LAS2*LLMS))
         AVS = MIN(AVS,VTSMAX*EXP(GS4+BVS*LLMS-LOG_GAMMA(BVS+AFAS+4.)))
         RHOX  = VXRHO(MVDS,RHOS,BVS)
         LMDS  = LOG(1.E6*MVDS)
         LMDS2 = LMDS*LMDS
         LROS  = LOG(RHOS)
         LROS2 = LROS*LROS
         BVS   = MAX(1.,(BVY1+BVY2*LMDS+BVY3*LROS+BVY4*LMDS2+BVY5*     &
                 LROS2+BVY6*LMDS*LROS)/(1.+BVY7*LMDS+BVY8*LROS+BVY9*   &
                 LMDS2+BVY10*LROS2+BVY11*LMDS*LROS))*BVS
         AVS   = AVS*RHOX*SASPR**THRD
      ENDIF

      END SUBROUTINE SOLVE_AFAS
!======================================================================

!======================================================================
      SUBROUTINE SOLVE_AFAG(TK1D,RHO,QG1D,QC1D,QR1D,QI1D,NG1D,LAMG,    &
                 AFAG,MVDG,RHOG,GASPR,AMG,AVG,BVG)
!======================================================================
      IMPLICIT NONE
      INTEGER :: IHAIL
      REAL :: TK1D,QG1D,QC1D,QR1D,NG1D,G3M1D,RHOG,GASPR,LAMG,AFAG, &
              AMG,AVG,BVG,LAMGMIN,LAMGMAX,KDX,MVDG,BDG,FDG,MVDC,MVDR,  &
              MVDL,TC1D,VTQR,VTQG,VTQL,VTQ0,VTQGL,DNRG,LTK,LQG,MDG,    &
              LBDG,LLMG,LLMG2,LMDG,LMDG2,LROG,LROG2,LAG2,LAG22,RHOX,   &
              GG1,GG4,MVG0,DSLL,LAG5,LAG52,MVRG,QI1D,RHO

      IHAIL = 0
      IF (ICE_RHOG.EQ.0) THEN
         RHOG = RHOG1
      ELSEIF (ICE_RHOG.EQ.1) THEN
         RHOG = RHOG1
         TC1D = TK1D-TK0C
         MVG0 = EXP(8.8283+0.25*LOG(QG1D))*1.E-6
         DSLL = 1.E-2*(EXP(MIN(20.,-TC1D/(1.1E4*RHO*(QC1D+QR1D)-1.3E3* &
                RHO*QI1D+1.)))-1.)
         DSLL = MIN(1.,MAX(1.E-3,DSLL))
         IF (MVG0.GT.DSLL) THEN
            IHAIL = 1
            RHOG = 900.
         ELSE
            IHAIL = 0
            LQG  = -1.*LOG(QG1D)
            RHOG = MIN(900.,MAX(400.,EXP(ROG1*LQG**3.+ROG2*LQG**2.+    &
                   ROG3*LQG+ROG4)))
!            IF (QC1D.GE.QSMALL.OR.QR1D.GE.QSMALL) RHOG = ((QC1D+QR1D)* &
!            RHOW+QG1D*RHOG)/(QC1D+QR1D+QG1D)
         ENDIF
      ENDIF
      IF (NG1D.LT.NSMALL) THEN
         LTK  = LOG(TK1D)
         LQG  = -1.*LOG(QG1D)
         IF (IHAIL.EQ.0) THEN
            MDG  = EXP(MDG1+MDG2*LTK+MDG3*LQG+MDG4*LTK**2.+MDG5*       &
                   LQG**2.+MDG6*LTK*LQG)/1.E3
         ELSEIF (IHAIL.EQ.1) THEN
            MDG  = EXP(MDH1+MDH2*LTK+MDH3*LQG+MDH4*LTK**2.+MDH5*       &
                   LQG**2.+MDH6*LTK*LQG)/1.E3
         ENDIF
         NG1D = 1.E9*QG1D*V2M3/RHOG/MDG**3.
      ENDIF
      AMG   = PI*RHOG/6.
      G3M1D = QG1D*V2M3/RHOG
      IF (AFAG_3M.EQ.0) THEN
         AFAG  = AFAG0
      ELSEIF (AFAG_3M.EQ.1) THEN
         MVRG = MIN(DGMAX/2.,MAX(DGMIN/2.,(QG1D/NG1D/RHOG/C4PI3)**THRD))
         IF (QC1D.GE.QSMALL.OR.NG1D.LT.0.5) THEN
            AFAG = MIN(AFAMAX,MAX(AFAMIN,(AFA1+AFA2*LOG(NG1D)+AFA3*    &
                   LOG(NG1D)**2.+AFA4*LOG(MVRG))/(1.+AFA5*LOG(NG1D)+   &
                   AFA6*LOG(MVRG))))
         ELSE
            AFAG = MIN(AFAMAX,MAX(AFAMIN,(AFB1+AFB2*LOG(NG1D)+AFB3*    &
                   LOG(NG1D)**2.+AFB4*LOG(MVRG))/(1.+AFB5*LOG(NG1D)+   &
                   AFB6*LOG(MVRG))))
         ENDIF
         IF (TK1D.GT.TK0C) AFAG = MIN(AFAMAX,0.01*EXP(-1.*LOG(MVRG)))
      ENDIF
      GG1  = LOG_GAMMA(AFAG+1.)
      GG4  = LOG_GAMMA(AFAG+4.)
      LAMG = (EXP(GG4-GG1)*NG1D/G3M1D)**THRD
      LAMGMIN = (EXP(GG4-GG1))**THRD/DGMAX
      LAMGMAX = (EXP(GG4-GG1))**THRD/DGMIN
      IF (LAMG.LT.LAMGMIN) THEN
         LAMG = LAMGMIN
         NG1D = G3M1D*EXP(GG1-GG4+3.*LOG(LAMG))
      ELSEIF (LAMG.GT.LAMGMAX) THEN
         LAMG = LAMGMAX
         NG1D = G3M1D*EXP(GG1-GG4+3.*LOG(LAMG))
      ENDIF
      MVDG = (EXP(GG4-GG1))**THRD/LAMG
      IF (GRAP_SHAPE.EQ.0) THEN
         GASPR = 1.
      ELSEIF (GRAP_SHAPE.EQ.1) THEN
         IF (IHAIL.EQ.0) THEN
            LBDG  = LOG(MVDG*1.E6)
            LROG  = LOG(RHOG)
            GASPR = MAX(0.2,MIN(SASMAX,ASP1+ASP2*LROG+ASP3*LROG**2.+   &
                    ASP4*LBDG+ASP5*LBDG**2.+ASP6*LBDG**3.))
         ELSEIF (IHAIL.EQ.1) THEN
            LLMG  = LOG(LAMG)
            LLMG2 = LLMG*LLMG
            LAG5  = LOG(AFAG+5.)
            LAG52 = LAG5*LAG5
            GASPR = MIN(1.,((5.6387098+1.5626893*LAG5-1.5481826*LLMG+  &
                    0.12481435*LAG52+0.11244742*LLMG2-0.2336531*LAG5*  &
                    LLMG)/(1.+0.28272803*LAG5-0.2780308*LLMG+          &
                    2.1750225E-2*LAG52+1.9960971E-2*LLMG2-4.1317594E-2*&
                    LAG5*LLMG))/10.)
         ENDIF
      ENDIF

      IF (ICE_VTG.EQ.0) THEN
         AVG = AVG0
         BVG = BVG0
      ELSEIF (ICE_VTG.EQ.1) THEN
         LAG2  = LOG(AFAG+2.)
         LAG22 = LAG2*LAG2
         LLMG  = LOG(LAMG)
         LLMG2 = LLMG*LLMG
         IF (MVDG.GE.3.E-5) THEN
            BVG = MAX(0.56,MIN(2.,(BVX1+BVX2*LAG2+BVX3*LLMG+BVX4*LAG22+&
                  BVX5*LLMG2+BVX6*LAG2*LLMG)/(1.+BVX7*LAG2+BVX8*LLMG+  &
                  BVX9*LAG22+BVX10*LLMG2+BVX11*LAG2*LLMG)))
         ELSE
            BVG = 2.
         ENDIF
         AVG = EXP((AVX1+AVX2*LAG2+AVX3*LLMG+AVX4*LAG22+AVX5*LLMG2+    &
               AVX6*LAG2*LLMG)/(1.+AVX7*LAG2+AVX8*LLMG+AVX9*LAG22+     &
               AVX10*LLMG2+AVX11*LAG2*LLMG))
         AVG = MIN(AVG,VTGMAX*EXP(GG4+BVG*LLMG-LOG_GAMMA(BVG+AFAG+4.)))
         RHOX  = VXRHO(MVDG,RHOG,BVG)
         LMDG  = LOG(1.E6*MVDG)
         LMDG2 = LMDG*LMDG
         LROG  = LOG(RHOG)
         LROG2 = LROG*LROG
         BVG   = MAX(1.,(BVY1+BVY2*LMDG+BVY3*LROG+BVY4*LMDG2+BVY5*     &
                 LROG2+BVY6*LMDG*LROG)/(1.+BVY7*LMDG+BVY8*LROG+BVY9*   &
                 LMDG2+BVY10*LROG2+BVY11*LMDG*LROG))*BVG
         IF (GASPR.LT.1.) THEN
            AVG = AVG*GASPR**THRD*RHOX
         ELSEIF (GASPR.GT.1.) THEN
            AVG = AVG*GASPR**SIXD*RHOX
         ELSE
            AVG = AVG*RHOX
         ENDIF
      ENDIF

      END SUBROUTINE SOLVE_AFAG
!=========================================================================
!
!
!======================================================================
      REAL FUNCTION VXRHO(MVDX,RHOX,BVX)
!======================================================================
      IMPLICIT NONE
      REAL, INTENT(IN) :: MVDX,RHOX,BVX
      REAL :: LMDX,LMDX2,LROX,LROX2,NBVX,NAVX

      LMDX  = LOG(1.E6*MVDX)
      LMDX2 = LMDX*LMDX
      LROX  = LOG(RHOX)
      LROX2 = LROX*LROX
      NBVX  = 1.-MAX(1.,(BVY1+BVY2*LMDX+BVY3*LROX+BVY4*LMDX2+BVY5*     &
              LROX2+BVY6*LMDX*LROX)/(1.+BVY7*LMDX+BVY8*LROX+BVY9*      &
              LMDX2+BVY10*LROX2+BVY11*LMDX*LROX))
      NAVX  = MIN(1.,0.27573-0.48822051*LMDX+0.45512963*LROX+          &
              2.7896597E-2*LMDX2-0.21716796*LROX2+0.14683485*LMDX*     &
              LROX-1.2330147E-3*LMDX2*LMDX+2.4876837E-2*LROX2*LROX-    &
              1.3548357E-2*LMDX*LROX2-8.3478882E-4*LMDX2*LROX)
      VXRHO = NAVX*MVDX**(BVX*NBVX)

      END FUNCTION VXRHO
!=======================================================================
!
!======================================================================
      REAL FUNCTION VYRHO(MVDX,RHOX)
!======================================================================
      IMPLICIT NONE
      REAL, INTENT(IN) :: MVDX,RHOX
      REAL :: LMDX,LMDX2,LROX,LROX2

      LMDX  = LOG(1.E6*MVDX)
      LMDX2 = LMDX*LMDX
      LROX  = LOG(RHOX)
      LROX2 = LROX*LROX
      VYRHO = MIN(1.,0.27573-0.48822051*LMDX+0.45512963*LROX+          &
              2.7896597E-2*LMDX2-0.21716796*LROX2+0.14683485*LMDX*     &
              LROX-1.2330147E-3*LMDX2*LMDX+2.4876837E-2*LROX2*LROX-    &
              1.3548357E-2*LMDX*LROX2-8.3478882E-4*LMDX2*LROX)

      END FUNCTION VYRHO
!=======================================================================
!
!======================================================================
      REAL FUNCTION RXXM(LAXM,LLMX,RHOX)                                ! Self-collection fall speed dispersion among ice crystals
!========================================================================
      IMPLICIT NONE
      REAL, INTENT(IN) :: LAXM,LLMX,RHOX
      REAL :: LAXM2,LAXM3,LLMX2,LLMX3,RXXSS,LROX,RXXM0

      LROX  = LOG(RHOX)
      LAXM2 = LAXM*LAXM
      LAXM3 = LAXM2*LAXM
      LLMX2 = LLMX*LLMX
      LLMX3 = LLMX2*LLMX
      RXXSS = MIN(0.999,MAX(1.E-5,EXP(24.558757+1.9714007*LAXM-        &
              4.0723222*LLMX+0.63185862*LAXM2+0.6260039*LLMX2-         &
              1.1155026*LAXM*LLMX+2.7701271E-2*LAXM3-3.3999359E-2*     &
              LLMX3+9.732948E-2*LAXM*LLMX2-9.8941321E-2*LAXM2*LLMX)/   &
              1.E6))
      RXXM0 = MIN(0.999,RXXSS*EXP(0.359541*LROX**3.-4.59669*LROX**2.+  &
              20.455011*LROX-29.573284)/1.E4)    ! from 2 to 0.0005
      RXXM  = LOG(2.E3*RXXM0)

      END FUNCTION RXXM
!======================================================================
!
!======================================================================
      REAL FUNCTION ELIM(LAIM,LLMI,RVLI,MVDI,RHOI,ADAGR)               ! Collection efficiency of liquid by pristine ice
!========================================================================
      IMPLICIT NONE
      REAL, INTENT(IN) :: LAIM,LLMI,RVLI,MVDI,RHOI,ADAGR
      REAL :: LAIM2,LAIM3,LLMI2,LLMI3,ELISS,RVLI2,RVLI3,RLSA,RLSB,RLSC,&
              RLSD,RLSE,RLSF,LROI,LROI2,LROI3,LMDI,LMDI2,LMDI3,EIRHO,  &
              EISAS,ZETA,ZIA,ZIB,ZIC,ZID,ZIE,ZIF
      REAL, PARAMETER :: RLSA1 = 2.767531E-2, RLSA2 = -0.28580892
      REAL, PARAMETER :: RLSA3 = 0.36364263, RLSA4 = 2.59540393
      REAL, PARAMETER :: RLSB1 = 1.054987E-2, RLSB2 = -0.1066427
      REAL, PARAMETER :: RLSB3 = 0.11062681, RLSB4 = 0.68927181
      REAL, PARAMETER :: RLSC1 = -9.42608E-3, RLSC2 = 9.111621E-2
      REAL, PARAMETER :: RLSC3 = -6.657928E-2, RLSC4 = -0.62017925
      REAL, PARAMETER :: RLSD1 = 6.2767E-4, RLSD2 = -5.05167E-3
      REAL, PARAMETER :: RLSD3 = -3.00263E-3, RLSD4 = 3.924231E-2
      REAL, PARAMETER :: RLSE1 = 7.1369E-4, RLSE2 = -6.49088E-3
      REAL, PARAMETER :: RLSE3 = 3.58481E-3, RLSE4 = 3.456597E-2
      REAL, PARAMETER :: RLSF1 = -1.43156E-3, RLSF2 = 1.289317E-2
      REAL, PARAMETER :: RLSF3 = -5.37314E-3, RLSF4 = -7.583775E-2

      LAIM2 = LAIM*LAIM
      LAIM3 = LAIM2*LAIM
      LLMI2 = LLMI*LLMI
      LLMI3 = LLMI2*LLMI
      ELISS = MIN(1.,MAX(0.,(EXP(1.0043566-0.65426268*LAIM+0.4847183*  &
              LLMI-0.15784731*LAIM2-4.1425458E-2*LLMI2+0.16581826*LAIM*&
              LLMI+5.6458792E-4*LAIM3-3.6696523E-4*LLMI3-2.8532686E-3* &
              LAIM*LLMI2+5.436099E-3*LAIM2*LLMI)-2.)/10.))
      LMDI  = LOG(1.E6*MVDI)
      LMDI2 = LMDI*LMDI
      LMDI3 = LMDI2*LMDI
      LROI  = LOG(RHOI)
      LROI2 = LROI*LROI
      LROI3 = LROI2*LROI
      EIRHO = MIN(1.,MAX(0.,-1.7336135+12.761623/LMDI+1.600691*LROI-   &
              48.112605/LMDI2-0.262521*LROI2-5.0524958/LMDI*LROI+      &
              122.53598/LMDI3+8.1587091E-3*LROI3+0.93252494/LMDI*      &
              LROI2-6.9558452/LMDI2*LROI))
      ZETA  = (ADAGR-1.)/(ADAGR+2.)
      IF (ZETA.LT.0.) THEN
         ZIA = -2.42545104*ZETA**3.+3.09619772*ZETA**2.+0.54680314*    &
               ZETA+1.00249656
         ZIB = -0.54839557*ZETA**3.+0.94724366*ZETA**2.+0.2417824*     &
               ZETA+8.0055E-4
         ZIC = 0.48840896*ZETA**3.-0.96910594*ZETA**2.-0.26982842*     &
               ZETA-8.1531E-4
         ZID = 4.410277E-2*ZETA**3.+9.993992E-2*ZETA**2.+3.89965E-2*   &
               ZETA+8.535E-5
         ZIE = -5.81256E-3*ZETA**3.+7.475146E-2*ZETA**2.+3.084968E-2*  &
               ZETA+6.636E-5
         ZIF = -1.210965E-2*ZETA**3.-0.16132236*ZETA**2.-6.47024E-2*   &
               ZETA-1.4221E-4
         EISAS = MIN(1.,MAX(0.,ZIA+ZIB*LAIM+ZIC*LLMI+ZID*LAIM2+ZIE*    &
                 LLMI2+ZIF*LAIM*LLMI))
      ELSEIF (ZETA.GT.0.) THEN
         ZIA = 5.03504274*ZETA**3.+0.55600684*ZETA**2.+0.28180694*     &
               ZETA+1.00343329
         ZIB = 1.47833948*ZETA**3.+0.1067278*ZETA**2.+2.183138E-2*     &
               ZETA+8.9184E-4
         ZIC = -1.43220765*ZETA**3.-0.10326695*ZETA**2.-1.54912E-3*    &
               ZETA-8.4195E-4
         ZID = 7.718675E-2*ZETA**3.+9.8602E-3*ZETA**2.-1.626778E-2*    &
               ZETA+3.781E-5
         ZIE = 9.055081E-2*ZETA**3.+1.37542E-3*ZETA**2.-9.00273E-3*    &
               ZETA+4.031E-5
         ZIF = -0.17668269*ZETA**3.-6.84075E-3*ZETA**2.+2.094879E-2*   &
               ZETA-8.155E-5
         EISAS = MIN(1.,MAX(0.,ZIA+ZIB*LAIM+ZIC*LLMI+ZID*LAIM2+ZIE*    &
                 LLMI2+ZIF*LAIM*LLMI))
      ELSE
         EISAS = 1.
      ENDIF
      RVLI2 = RVLI*RVLI
      RVLI3 = RVLI2*RVLI
      RLSA  = RLSA1*RVLI3+RLSA2*RVLI2+RLSA3*RVLI+RLSA4
      RLSB  = RLSB1*RVLI3+RLSB2*RVLI2+RLSB3*RVLI+RLSB4
      RLSC  = RLSC1*RVLI3+RLSC2*RVLI2+RLSC3*RVLI+RLSC4
      RLSD  = RLSD1*RVLI3+RLSD2*RVLI2+RLSD3*RVLI+RLSD4
      RLSE  = RLSE1*RVLI3+RLSE2*RVLI2+RLSE3*RVLI+RLSE4
      RLSF  = RLSF1*RVLI3+RLSF2*RVLI2+RLSF3*RVLI+RLSF4
      ELIM  = MIN(1.,MAX(0.,(RLSA+RLSB*LAIM+RLSC*LLMI+RLSD*LAIM2+RLSE* &
              LLMI2+RLSF*LAIM*LLMI)))*ELISS*EIRHO*EISAS

      END FUNCTION ELIM
!======================================================================
!      
!======================================================================
      REAL FUNCTION ELXM(LAXM,LLMX,RVLX,MVDX,RHOX,XASPR)                ! Collection efficiency of liquid by large ice
!========================================================================
      IMPLICIT NONE
      REAL, INTENT(IN) :: LAXM,LLMX,RVLX,MVDX,RHOX,XASPR
      REAL :: LAXM2,LAXM3,LLMX2,LLMX3,ELXSS,RVLX2,RVLX3,RLSA,RLSB,RLSC,&
              RLSD,RLSE,RLSF,LROX,LROX2,LROX3,LMDX,LMDX2,LMDX3,EXRHO,  &
              LXAS,LXAS2,LXAS3,EXSAS
      REAL, PARAMETER :: RLSA1 = 2.767531E-2, RLSA2 = -0.28580892
      REAL, PARAMETER :: RLSA3 = 0.36364263, RLSA4 = 2.59540393
      REAL, PARAMETER :: RLSB1 = 1.054987E-2, RLSB2 = -0.1066427
      REAL, PARAMETER :: RLSB3 = 0.11062681, RLSB4 = 0.68927181
      REAL, PARAMETER :: RLSC1 = -9.42608E-3, RLSC2 = 9.111621E-2
      REAL, PARAMETER :: RLSC3 = -6.657928E-2, RLSC4 = -0.62017925
      REAL, PARAMETER :: RLSD1 = 6.2767E-4, RLSD2 = -5.05167E-3
      REAL, PARAMETER :: RLSD3 = -3.00263E-3, RLSD4 = 3.924231E-2
      REAL, PARAMETER :: RLSE1 = 7.1369E-4, RLSE2 = -6.49088E-3
      REAL, PARAMETER :: RLSE3 = 3.58481E-3, RLSE4 = 3.456597E-2
      REAL, PARAMETER :: RLSF1 = -1.43156E-3, RLSF2 = 1.289317E-2
      REAL, PARAMETER :: RLSF3 = -5.37314E-3, RLSF4 = -7.583775E-2

      LAXM2 = LAXM*LAXM
      LAXM3 = LAXM2*LAXM
      LLMX2 = LLMX*LLMX
      LLMX3 = LLMX2*LLMX
      ELXSS = MIN(1.,MAX(0.,(EXP(4.7200327+1.0732342*LAXM-1.1460962*   &
              LLMX+0.12464758*LAXM2+0.18710941*LLMX2-0.33050478*LAXM*  &
              LLMX+1.4103773E-2*LAXM3-1.0547583E-2*LLMX3+3.057332E-2*  &
              LAXM*LLMX2-3.1891625E-2*LAXM2*LLMX)-2.)/10.))
      LMDX  = LOG(1.E6*MVDX)
      LMDX2 = LMDX*LMDX
      LMDX3 = LMDX2*LMDX
      LROX  = LOG(RHOX)
      LROX2 = LROX*LROX
      LROX3 = LROX2*LROX
      LXAS  = LOG(2.E3*XASPR)
      LXAS2 = LXAS*LXAS
      LXAS3 = LXAS2*LXAS
      EXRHO = MIN(1.,MAX(0.,-1.7336135+12.761623/LMDX+1.600691*LROX-   &
              48.112605/LMDX2-0.262521*LROX2-5.0524958/LMDX*LROX+      &
              122.53598/LMDX3+8.1587091E-3*LROX3+0.93252494/LMDX*      &
              LROX2-6.9558452/LMDX2*LROX))
      IF (XASPR.LT.1.) THEN
         EXSAS = MIN(1.,MAX(0.,0.54690439+13.277765/LMDX-7.5772063E-2* &
                 LXAS-95.341824/LMDX2-9.7625166E-3*LXAS2+0.92399766/   &
                 LMDX*LXAS+142.49569/LMDX3-1.963527E-4*LXAS3+          &
                 8.4868963E-2/LMDX*LXAS2-2.0619998/LMDX2*LXAS))
      ELSEIF (XASPR.GT.1.) THEN
         EXSAS = MIN(1.,MAX(0.,-3.3952848+42.945279/LMDX+0.52165674*   &
                 LXAS-121.5914/LMDX2-2.3303651E-2*LXAS2-2.9575498/     &
                 LMDX*LXAS+135.53377/LMDX3+4.3848183E-4*LXAS3+         &
                 5.8301101E-2/LMDX*LXAS2+2.1429175/LMDX2*LXAS))
      ELSE
         EXSAS = 1.
      ENDIF
      RVLX2 = RVLX*RVLX
      RVLX3 = RVLX2*RVLX
      RLSA  = RLSA1*RVLX3+RLSA2*RVLX2+RLSA3*RVLX+RLSA4
      RLSB  = RLSB1*RVLX3+RLSB2*RVLX2+RLSB3*RVLX+RLSB4
      RLSC  = RLSC1*RVLX3+RLSC2*RVLX2+RLSC3*RVLX+RLSC4
      RLSD  = RLSD1*RVLX3+RLSD2*RVLX2+RLSD3*RVLX+RLSD4
      RLSE  = RLSE1*RVLX3+RLSE2*RVLX2+RLSE3*RVLX+RLSE4
      RLSF  = RLSF1*RVLX3+RLSF2*RVLX2+RLSF3*RVLX+RLSF4
      ELXM  = MIN(1.,MAX(0.,(RLSA+RLSB*LAXM+RLSC*LLMX+RLSD*LAXM2+RLSE* &
              LLMX2+RLSF*LAXM*LLMX)))*ELXSS*EXRHO*EXSAS

      END FUNCTION ELXM
!======================================================================
!
!======================================================================
      REAL FUNCTION EXRM(LARM,LLMR,RVLR,MVDR)                   ! Collection efficiency of particles by raindrops
!========================================================================
      IMPLICIT NONE
      REAL, INTENT(IN) :: LARM,LLMR,RVLR,MVDR
      REAL :: LARM2,LARM3,LLMR2,LLMR3,ELRSS,RVLR2,RVLR3,RLRA,RLRB,RLRC,&
              RLRD,RLRE,RLRF,LMDR,LMDR2,LMDR3
      REAL, PARAMETER :: RLRA1 = 5.306532E-2, RLRA2 = -0.66730349
      REAL, PARAMETER :: RLRA3 = 1.87142746, RLRA4 = 2.00473855
      REAL, PARAMETER :: RLRB1 = 1.843663E-2, RLRB2 = -0.22407434
      REAL, PARAMETER :: RLRB3 = 0.57277116, RLRB4 = 0.49339804
      REAL, PARAMETER :: RLRC1 = -1.572148E-2, RLRC2 = 0.18553725
      REAL, PARAMETER :: RLRC3 = -0.43832394, RLRC4 = -0.47998192
      REAL, PARAMETER :: RLRD1 = 1.02175E-3, RLRD2 = -1.091302E-2
      REAL, PARAMETER :: RLRD3 = 1.965474E-2, RLRD4 = 3.229736E-2
      REAL, PARAMETER :: RLRE1 = 1.07624E-3, RLRE2 = -1.192483E-2
      REAL, PARAMETER :: RLRE3 = 2.490454E-2, RLRE4 = 2.693252E-2
      REAL, PARAMETER :: RLRF1 = -2.26089E-3, RLRF2 = 2.525652E-2
      REAL, PARAMETER :: RLRF3 = -5.372595E-2, RLRF4 = -5.78179E-2

      LARM2 = LARM*LARM
      LARM3 = LARM2*LARM
      LLMR2 = LLMR*LLMR
      LLMR3 = LLMR2*LLMR
      ELRSS = MIN(1.,MAX(0.,(EXP(6.0637583+1.3660477*LARM-1.5899219*   &
              LLMR+0.14018712*LARM2+0.23456224*LLMR2-0.391976*LARM*    &
              LLMR+1.4415846E-2*LARM3-1.2196226E-2*LLMR3+3.3783973E-2* &
              LARM*LLMR2-3.3664991E-2*LARM2*LLMR)-2.)/10.))
      LMDR  = LOG(1.E6*MVDR)
      LMDR2 = LMDR*LMDR
      LMDR3 = LMDR2*LMDR
      RVLR2 = RVLR*RVLR
      RVLR3 = RVLR2*RVLR
      RLRA  = RLRA1*RVLR3+RLRA2*RVLR2+RLRA3*RVLR+RLRA4
      RLRB  = RLRB1*RVLR3+RLRB2*RVLR2+RLRB3*RVLR+RLRB4
      RLRC  = RLRC1*RVLR3+RLRC2*RVLR2+RLRC3*RVLR+RLRC4
      RLRD  = RLRD1*RVLR3+RLRD2*RVLR2+RLRD3*RVLR+RLRD4
      RLRE  = RLRE1*RVLR3+RLRE2*RVLR2+RLRE3*RVLR+RLRE4
      RLRF  = RLRF1*RVLR3+RLRF2*RVLR2+RLRF3*RVLR+RLRF4
      EXRM  = MIN(1.,MAX(0.,(RLRA+RLRB*LARM+RLRC*LLMR+RLRD*LARM2+RLRE* &
              LLMR2+RLRF*LARM*LLMR)))*ELRSS

      END FUNCTION EXRM
!======================================================================
!pcb, Tcb:  p and T at cloud base, or RH=100%
!======================================================================
      SUBROUTINE cal_A1AdA2(p,T,qv,alf,Tcb,A1,A,A2)
!======================================================================
      REAL, INTENT(IN) :: p,T,qv
      REAL, INTENT(OUT) :: Tcb,A,A1,A2,alf
      INTEGER :: i
      REAL :: e_s,cp_i,l_v,pme,TC,tmp,Dv,ka,pcb,pcb2,pcb3,y,y2,tmp1

      cp_i = CP+qv*1850.
      tmp1 = (0.622+qv)*RV/((1.+qv)*cp_i)
      pcb = 1.E5
      Tcb = T*(pcb/P)**tmp1
      i = 0
! for init. pcb
      do while (pcb-POLYSVP(Tcb,0).LT.0)
         pcb = 0.9*pcb
         Tcb = T*(pcb/P)**tmp1
         i = i+1
         if (i.GT.50) then
            print *, 'iter. too long in init pcb'
            print *,  pcb,P,T,Tcb,POLYSVP(Tcb,0)
            stop
        endif
      enddo
      y = POLYSVP(Tcb,0)*0.622/(pcb-POLYSVP(Tcb,0))-qv
      pcb2 = pcb*0.9
      i = 0
! for solve Tcb
      do while (ABS(pcb/pcb2-1.).GT.1.E-5)
         Tcb = T*(pcb2/P)**tmp1
         y2 = POLYSVP(Tcb,0)*0.622/(pcb2-POLYSVP(Tcb,0))-qv
         if (y.EQ.y2) exit
         if (i.GT.50) then
            print *,  pcb, pcb2, y, y2
         end if
         if (i.gt.150) then
            print *, 'iter. too long on solving pcb'
            print *, pcb,pcb2,y,y2
            stop
         end if
         pcb3 = pcb2-y2*(pcb2-pcb)/(y2-y)
         pcb = pcb2
         pcb2 = pcb3
         i = i+1
         y = y2
      end do
      e_s = POLYSVP(Tcb,0)
      TC  = Tcb-TK0C
      l_v = 2.501E6*(TK0C/Tcb)**(0.167+3.67E-4*Tcb)
      pme = pcb-e_s
      A2  = (pme)*(pme)/(0.622*pcb*e_s)+l_v*l_v/(cp_i*RV*Tcb*Tcb)
      tmp = l_v/Tcb
      Dv  = 2.11E-5*(Tcb/TK0C)**1.94*101325./pcb
      ka  = (2.3823E-2+7.11756E-5*TC)
      A   = (Tcb*RV/(Dv*e_s)+tmp*(tmp/RV-1.)/ka)
      A1  = (l_v/(cp_i*Tcb)-1./(0.622+qv))*(1.+qv)*(G)/(RV*Tcb)
      alf = 2.*(0.0761-1.55E-4*TC)/(RV*RHOW*Tcb)

      END SUBROUTINE cal_A1AdA2
!============================================================================
!
!=======================================================================
      SUBROUTINE mp_tcwa2(TH,P,DZ,W,PII,DT_MP,XLAND,QV,QC,QR,QI,QS,QG, &
                 NC,NR,NI,NS,NG,SR,RAINNC,RAINNCV,SNOWNC,SNOWNCV,      &
                 GRAPNC,GRAPNCV,DBZM,ZDR,KDP,SMLF,GMLF,re_cloud,re_ice,&
                 re_snow,has_reqc,has_reqi,has_reqs,IDS,IDE,JDS,JDE,   &
                 KDS,KDE,IMS,IME,JMS,JME,KMS,KME,ITS,ITE,JTS,JTE,KTS,  &
                 KTE)
!======================================================================
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE,IMS,IME,JMS,JME,  &
            KMS,KME,ITS,ITE,JTS,JTE,KTS,KTE
      REAL, INTENT(IN) :: DT_MP
      REAL, DIMENSION(IMS:IME,JMS:JME), INTENT(IN) :: XLAND
      REAL, DIMENSION(IMS:IME,JMS:JME),INTENT(INOUT) :: RAINNC,RAINNCV,&
            SNOWNC,SNOWNCV,GRAPNC,GRAPNCV,SR
      REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(IN) :: PII,DZ,W,P
      REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(INOUT) :: TH,QV,&
            QC,QR,QI,QS,QG,NC,NR,NI,NS,NG
      REAL, DIMENSION(IMS:IME,KMS:KME,JMS:JME), INTENT(INOUT) :: DBZM, &
            ZDR,KDP,SMLF,GMLF,re_cloud,re_ice,re_snow
      INTEGER :: I,K,J,NK,ITF,JTF
      REAL :: DT,DTMN
      REAL, DIMENSION(KTS:KTE) :: TK1D,P1D,Z1D,W1D,DZ1D,QV1D,QC1D,QR1D,&
            QI1D,QS1D,QG1D,NC1D,NR1D,NI1D,NS1D,NG1D,DBZM1D,ZDR1D,KDP1D,&
            SMLF1D,GMLF1D,EFRC1D,EFRI1D,EFRS1D
      REAL, DIMENSION(ITS:ITE,JTS:JTE) :: CLODNCV,ICENCV
      REAL, DIMENSION(ITS:ITE,KTS:KTE,JTS:JTE) :: TK
      INTEGER, INTENT(IN) :: has_reqc,has_reqi,has_reqs

      DT = DT_MP
      DTMN = DT_MP/60.
      ITF = MIN(ITE,IDE-1)
      JTF = MIN(JTE,JDE-1)
      DO I = ITS,ITF
      DO J = JTS,JTF
         CLODNCV(I,J) = 0.; RAINNCV(I,J) = 0.; ICENCV(I,J) = 0.
         SNOWNCV(I,J) = 0.; GRAPNCV(I,J) = 0.; SR(I,J) = 0.
!----------------- write data from 3D to 1D and assign upside down -----
         DO K = KTS,KTE
            NK = KME-K
            TK1D(K) = TH(I,NK,J)*PII(I,NK,J)
            W1D(K)  = 0.5*(W(I,NK,J)+W(I,NK+1,J)) ! W at half level
            QV1D(K) = QV(I,NK,J)
            DZ1D(K) = DZ(I,NK,J)
            P1D(K)  = P(I,NK,J)
            QC1D(K) = QC(I,NK,J)
            QR1D(K) = QR(I,NK,J)
            QI1D(K) = QI(I,NK,J)
            QS1D(K) = QS(I,NK,J)
            QG1D(K) = QG(I,NK,J)
            NC1D(K) = NC(I,NK,J)
            NR1D(K) = NR(I,NK,J)
            NI1D(K) = NI(I,NK,J)
            NS1D(K) = NS(I,NK,J)
            NG1D(K) = NG(I,NK,J)
            EFRC1D(K) = 2.5E-6; EFRI1D(K) = 5.E-6; EFRS1D(K) = 1.E-5
            DBZM1D(K) = -30.; ZDR1D(K) = 0.; KDP1D(K) = 0.
            SMLF1D(K) = 0.; GMLF1D(K) = 0.
         ENDDO

         CALL NTU_MICRO(TK1D,QV1D,DZ1D,P1D,W1D,XLAND(I,J),SR(I,J),     &
              ICENCV(I,J),CLODNCV(I,J),RAINNC(I,J),RAINNCV(I,J),       &
              SNOWNC(I,J),SNOWNCV(I,J),GRAPNC(I,J),GRAPNCV(I,J),DT,    &
              DTMN,QC1D,QR1D,QI1D,QS1D,QG1D,NC1D,NR1D,NI1D,NS1D,NG1D,  &
              KTS,KTE,DBZM1D,ZDR1D,KDP1D,SMLF1D,GMLF1D,EFRC1D,EFRI1D,  &
              EFRS1D)

         DO K = KTS,KTE
            NK = KME-K
            TH(I,NK,J) = TK1D(K)/PII(I,NK,J)
            QV(I,NK,J) = MAX(0.,QV1D(K))
            QC(I,NK,J) = MAX(0.,QC1D(K))
            QR(I,NK,J) = MAX(0.,QR1D(K))
            QI(I,NK,J) = MAX(0.,QI1D(K))
            QS(I,NK,J) = MAX(0.,QS1D(K))
            QG(I,NK,J) = MAX(0.,QG1D(K))
            NC(I,NK,J) = MAX(0.,NC1D(K))
            NR(I,NK,J) = MAX(0.,NR1D(K))
            NI(I,NK,J) = MAX(0.,NI1D(K))
            NS(I,NK,J) = MAX(0.,NS1D(K))
            NG(I,NK,J) = MAX(0.,NG1D(K))
            DBZM(I,NK,J) = MAX(-30.,DBZM1D(K))
            ZDR(I,NK,J)  = MAX(0.,ZDR1D(K))
            KDP(I,NK,J)  = MAX(0.,KDP1D(K))
            SMLF(I,NK,J) = MAX(0.,SMLF1D(K))
            GMLF(I,NK,J) = MAX(0.,GMLF1D(K))
            re_cloud(I,NK,J) = MAX(2.5E-6,MIN(EFRC1D(K),5.E-5))
            re_ice(I,NK,J)   = MAX(5.E-6,MIN(EFRI1D(K),125.E-6))
            re_snow(I,NK,J)  = MAX(1.E-5,MIN(EFRS1D(K),1.E-3))
         ENDDO
      ENDDO
      ENDDO

      END SUBROUTINE mp_tcwa2
!======================================================================
!
!======================================================================
      SUBROUTINE NTU_MICRO(TK3D,QV3D,DZ3D,P3D,W3D,LAND,SR,ICENCV,      &
                 CLODNCV,RAINNC,RAINNCV,SNOWNC,SNOWNCV,GRAPNC,GRAPNCV, &
                 DT,DTMN,QC3D,QR3D,QI3D,QS3D,QG3D,NC3D,NR3D,NI3D,NS3D, &
                 NG3D,KTS,KTE,DBZM,ZDR,KDP,SMLF,GMLF,EFRC,EFRI,EFRS)
!======================================================================
      IMPLICIT NONE 
      INTEGER, INTENT(IN) :: KTS,KTE
      INTEGER :: K,A,IV,IV0,IV1,IV2,IV3,IV4,IM,IT,J
      INTEGER, DIMENSION(KTS:KTE) :: HID,IHAIL
      REAL, INTENT(IN) :: LAND
      REAL, DIMENSION(KTS:KTE), INTENT(IN) :: W3D,P3D
      REAL, DIMENSION(KTS:KTE) :: TK3D,TC3D,QV3D,DZ3D,QC3D,QR3D,QI3D,  &
            QS3D,QG3D,NC3D,NR3D,NI3D,NS3D,NG3D,ADAGR,AMI,AVC,BVC,AVR,  &
            BVR,AVI,BVI,AMS,AVS,BVS,AMG,AVG,BVG,INHGR,QACac,NACac,     &
            VT_QC,VT_QR,VT_QI,VT_QS,VT_QG,VT_NC,VT_NR,VT_NI,VT_NS,     &
            VT_NG,RHO,CPM,ESW,ESI,QVSW,QVSI,SSRW,SSRI,XXLV,XXLS,XXLF,  &
            ZETA,ZETA2,BEST,MVRC,MVRR,SIGC,MNRC,KDX,BDR,BDI,BDS,BDG,   &
            R3M3D,S3M3D,G3M3D,FDI,FDS,FDG,LTK,LQC,LQR,LQI,LQS,LQG,LBDI,&
            LBDS,LBDG,MDC,MDR,MDI,MDS,MDG,MDH,EFRC,EFRR,WCM,ZCN99,LLMC,&
            LAC2,LAC5,LLMR,LAR2,LAR5,LLMI,LAI2,LAI5,RHOX,LLMS,LAS2,    &
            LAS5,LLMG,LAG2,LAG5,AFAZ,MVG0,DSLL,RHOI,RHOS,RHOG,MVDC,    &
            MVDR,MVDI,MVDS,MVDG,VTQC,VTQR,VTQI,VTQS,VTQG,AFAC,AFAR,    &
            AFAI,AFAS,AFAG,LAMC,LAMR,LAMI,LAMS,LAMG,SMLF,GMLF,VTNC,    &
            VTNR,VTNI,VTNS,VTNG,RASPR,IASPR,SASPR,GASPR,alf,Tcb,A1,A2, &
            A3,RACT,CND,DEP,ABW,ABI,QVS,RSUB,Y1,Y2,Y4,Y5,LMDI,LROI,    &
            LROI2,LROI3,LMDS,LROS,LMDG,LROG,LLMI2,LLMI3,LAI22,LAI23,   &
            LMDI2,LMDI3,ZETA3,LAR22,LAR23,LLMR2,LLMR3,EFRI,EFRS,DSMM,  &
            DGMM,MVRI,MVRS,MVRG,TTR,LAMSMAX,LAMSMIN,LAMGMAX,LAMGMIN
      REAL, DIMENSION(KTS:KTE) :: DBZM,DBZ1,DBZR,DBZI,DBZS,DBZG,ZDR,   &
            ZDRR,ZDRI,ZDRS,ZDRG,KDP,KDPR,KDPI,KDPS,KDPG,SCAKW,SCAW0,   &
            SCAW1,SCAWA,SCAWB,EPSIN,EPSS,ALPHE,LAMDZ,NEREL,ORA0,ORAR,  &
            ORA1,ORA2,ORA3,ORA4,SCA0,SCALA,SCALB,SCA2,SCAFA,SCAFB,     &
            SCFAA,SCFAB,SCFBB,SMLV,GMLV,ZZHR,ZZHI,ZZHS,ZZHG,ZZVR,ZZVI, &
            ZZVS,ZZVG,ZZHVR,ZZHVI,ZZHVS,ZZHVG,LMDR,EORA,ORZHR,FDBZR,   &
            FZDRR,FKDPR,KDPR1,FDBZS,ZHROX,FZDRS,ZDROX,ZDASP,LASP,FKDPS,&
            KDROX,KDASP,FDBZG,FZDRG,FKDPG,ZHASP,RHASP,ORZHI,ZHZET,     &
            FDBZI,ZDZET,FZDRI,FKDPI,KDZET,LDZET,RADS,RADG
      REAL :: SR,ICENCV,CLODNCV,RAINNC,RAINNCV,SNOWNC,SNOWNCV,GRAPNC,  &
              GRAPNCV,DT,DTMN,K1,C1,ZCMAX
      REAL, PARAMETER :: MIXA = -1.0892332E-2, MIXB = 2.0820143E-2
      REAL, PARAMETER :: MIXC = 4.8553456E-6,  MIXD = -7.6529057E-5
      REAL, PARAMETER :: MIXE = -1.1783334E-9, MIXF = -2.9599665E-6

      DO K = KTS,KTE
!------- INITIALIZE VARIABLE,ONLY T&QV ARE TREATED DIFFERENTLY ---------
         RHOI(K) = RHOI0; RHOS(K) = RHOS0; RHOG(K) = RHOG0
         AFAC(K) = AFAC0; AFAR(K) = AFAR0; AFAI(K) = AFAI0
         AFAS(K) = AFAS0; AFAG(K) = AFAG0; AVI(K) = AVI0
         BVI(K) = BVI0; AVS(K) = AVS0; BVS(K) = BVS0; AVG(K) = AVG0
         BVG(K) = BVG0; AMI(K) = AMI0; AMS(K) = AMS0
         AMG(K) = AMG0; RASPR(K) = 1.; IASPR(K) = 1.; SASPR(K) = 1.
         GASPR(K) = 1.; QACac(K) = 0.; ADAGR(K) = 1.; SMLF(K) = 0.
         GMLF(K) = 0.;  NACac(K) = 0.
         TC3D(K)   = TK3D(K)-TK0C
         ESW(K)    = MIN(0.99*P3D(K),POLYSVP(TK3D(K),0))
         ESI(K)    = MIN(0.99*P3D(K),POLYSVP(TK3D(K),1))
         IF (ESI(K).GT.ESW(K)) ESI(K) = ESW(K)
         SSRW(K)   = QV3D(K)/(0.622*ESW(K)/(P3D(K)-ESW(K)))-1.
         SSRI(K)   = QV3D(K)/(0.622*ESI(K)/(P3D(K)-ESI(K)))-1.
         XXLV(K)   = 3.1484E6-2370.*TK3D(K)
         XXLS(K)   = 3.15E6-2370.*TK3D(K)+0.3337E6
         XXLF(K)   = 2836310.8-(3.1484E6-2370.*TK3D(K))
         CPM(K)    = CP*(1.+0.887*QV3D(K))
         HID(K)    = MAX(MIN(NINT(ABS(TC3D(K))/0.25),120),0)
         RHO(K)    = P3D(K)/(TK3D(K)*(1.+0.61*QV3D(K)))/R
         IF (QC3D(K).GE.QSMALL.AND.NC3D(K).LT.NSMALL) THEN
            LTK(K)  = LOG(TK3D(K))
            LQC(K)  = -1.*LOG(QC3D(K))
            MDC(K)  = EXP(MDC1+MDC2*LTK(K)+MDC3*LQC(K)+MDC4*LTK(K)*    &
                      LTK(K)+MDC5*LQC(K)*LQC(K)+MDC6*LTK(K)*LQC(K))/1.E3
            NC3D(K) = 1.E9*QC3D(K)*V2M3/RHOW/MDC(K)**3.
            IF (AFAC_3M.EQ.0) THEN
               AFAC(K) = AFAC0
            ELSEIF (AFAC_3M.EQ.1) THEN
               MVRC(K) = (QC3D(K)/NC3D(K)/C4PI3W)**THRD
               MVRC(K) = MIN(MAX(MVRC(K),RCMIN),RCMAX)
               EFRC(K) = EXP(EFC1+EFC2*LOG(NC3D(K))+EFC3*LOG(MVRC(K)))
               KDX(K)  = MAX(KCCMIN,MIN(KCCMAX,(MVRC(K)/EFRC(K))**3.))
               KDX(K)  = MIN(KCCMAX,MAX(KCCMIN,KDX(K)))
               AFAC(K) = (6.*KDX(K)-3.+SQRT(8.*KDX(K)+1.))/(2.-2.*     &
                         KDX(K))
               AFAC(K) = MIN(MAX(AFAC(K),AFAMIN),AFAMAX)
            ELSEIF (AFAC_3M.EQ.2) THEN
               SIGC(K) = EXP(SIG1+SIG2*LOG(NC3D(K))+SIG3*LOG(QC3D(K)))
               MNRC(K) = EXP(MNR1+MNR2*LOG(NC3D(K))+MNR3*LOG(QC3D(K)))
               AFAC(K) = MIN(MAX(SIGC(K)/MNRC(K),AFAMIN),AFAMAX)
            ENDIF
         ENDIF
         IF (QR3D(K).GE.QSMALL.AND.NR3D(K).LT.NSMALL) THEN
            LTK(K)  = LOG(TK3D(K))
            LQR(K)  = -1.*LOG(QR3D(K))
            MDR(K)  = EXP(MDR1+MDR2*LTK(K)+MDR3*LQR(K)+MDR4*LTK(K)*    &
                      LTK(K)+MDR5*LQR(K)*LQR(K)+MDR6*LTK(K)*LQR(K))/1.E3
            NR3D(K) = 1.E9*QR3D(K)*V2M3/RHOW/MDR(K)**3.
            IF (AFAR_3M.EQ.0) THEN
               AFAR(K) = AFAR0
            ELSEIF (AFAR_3M.EQ.1) THEN
               MVRR(K) = (QR3D(K)/NR3D(K)/C4PI3W)**THRD
               MVRR(K) = MIN(RRMAX,MAX(RRMIN,MVRR(K)))
               IF (QC3D(K).GE.QSMALL.OR.NR3D(K).LT.0.5) THEN
                  AFAR(K) = MIN(AFAMAX,MAX(AFAMIN,(AFA1+AFA2*          &
                            LOG(NR3D(K))+AFA3*LOG(NR3D(K))**2.+AFA4*   &
                            LOG(MVRR(K)))/(1.+AFA5*LOG(NR3D(K))+AFA6*  &
                            LOG(MVRR(K)))))
               ELSE
                  AFAR(K) = MIN(AFAMAX,MAX(AFAMIN,(AFB1+AFB2*          &
                            LOG(NR3D(K))+AFB3*LOG(NR3D(K))**2.+AFB4*   &
                            LOG(MVRR(K)))/(1.+AFB5*LOG(NR3D(K))+AFB6*  &
                            LOG(MVRR(K)))))
               ENDIF
               IF ((NR3D(K)*MVRR(K)**6.).GE.1.E-16) THEN
                  IF (NR3D(K).GE.3.8E3) THEN
                     AFAR(K) = AFAMAX
                  ELSE
                     AFAR(K) = AFAR(K)*0.5
                  ENDIF
               ENDIF
            ELSEIF (AFAR_3M.EQ.2) THEN
               BDR(K)  = (QR3D(K)*V2M3/RHOW/NR3D(K))**THRD
               BDR(K)  = MIN(MAX(BDR(K),DRMIN),DRMAX)
               AFAR(K) = MAX(AFAMIN,19.*TANH(0.6*(1.E3*BDR(K)-1.8))+17.)
            ENDIF
         ENDIF
         IF (QI3D(K).GE.QSMALL.AND.NI3D(K).LT.NSMALL) THEN
            IF (ICE_RHOI.EQ.0) THEN
               RHOI(K)  = RHOI0
            ELSEIF (ICE_RHOI.EQ.1) THEN
               QVSI(K)  = 0.622*ESI(K)/(P3D(K)-ESI(K))
               INHGR(K) = ITBLE(HID(K))
               RHOI(K)  = RHOI0*EXP(-3.*MAX((QV3D(K)-QVSI(K))-5.E-5,   &
                          0.)/INHGR(K))
            ENDIF
            LTK(K) = LOG(TK3D(K))
            LQI(K) = -1.*LOG(QI3D(K))
            MDI(K) = EXP(MDI1+MDI2*LTK(K)+MDI3*LQI(K)+MDI4*LTK(K)**2.+ &
                     MDI5*LQI(K)**2.+MDI6*LTK(K)*LQI(K))/1.E3
            NI3D(K) = 1.E9*QI3D(K)*V2M3/RHOI(K)/MDI(K)**3.
            IF (ICE_SHAPE.EQ.0) THEN
               ADAGR(K) = 1.
            ELSEIF (ICE_SHAPE.EQ.1) THEN
               BDI(K) = (QI3D(K)*V2M3/RHOI(K)/NI3D(K))**THRD*1.E3
               BDI(K) = MIN(MAX(BDI(K),DIMIN*1.E3),DIMAX*1.E3)
               IF (BDI(K).GE.(DI0*1.E3)) THEN
                  LBDI(K)  = LOG(BDI(K)*1.E3)
                  INHGR(K) = ITBLE(HID(K))
                  TTR(K)   = MIN(1.,MAX(0.01,ISP1*LBDI(K)**3.+ISP2*    &
                             LBDI(K)**2.+ISP3*LBDI(K)+ISP4))
                  ADAGR(K) = INHGR(K)**TTR(K)
               ELSE
                  ADAGR(K) = 1.
               ENDIF
               ZETA(K) = (ADAGR(K)-1.)/(ADAGR(K)+2.)
            ENDIF
            IF (AFAI_3M.EQ.0) THEN
               AFAI(K) = AFAI0
            ELSEIF (AFAI_3M.EQ.1) THEN
               MVRI(K) = MIN(DIMAX/2.,MAX(DIMIN/2.,(QI3D(K)/NI3D(K)/   &
                         RHOI(K)/C4PI3)**THRD))
               IF (QC3D(K).GE.QSMALL.OR.NI3D(K).LT.0.5) THEN
                  AFAI(K) = MIN(AFAMAX,MAX(AFAMIN,(AFA1+AFA2*          &
                            LOG(NI3D(K))+AFA3*LOG(NI3D(K))**2.+AFA4*   &
                            LOG(MVRI(K)))/(1.+AFA5*LOG(NI3D(K))+AFA6*  &
                            LOG(MVRI(K)))))
               ELSE
                  AFAI(K) = MIN(AFAMAX,MAX(AFAMIN,(AFB1+AFB2*          &
                            LOG(NI3D(K))+AFB3*LOG(NI3D(K))**2.+AFB4*   &
                            LOG(MVRI(K)))/(1.+AFB5*LOG(NI3D(K))+AFB6*  &
                            LOG(MVRI(K)))))
               ENDIF
            ENDIF
         ENDIF
         IF (QS3D(K).GE.QSMALL.AND.NS3D(K).LT.NSMALL) THEN
            IF (ICE_RHOS.EQ.0) THEN
               RHOS(K) = RHOS0
            ELSEIF (ICE_RHOS.EQ.1) THEN
               LTK(K) = LOG(TK3D(K))
               LQS(K) = -1.*LOG(QS3D(K))
               IF (TK3D(K).LT.TK0C) THEN
                  RHOS(K) = MIN(900.,EXP(ROS1+ROS2*LTK(K)+ROS3*LQS(K)+ &
                            ROS4*LTK(K)**2.+ROS5*LQS(K)**2.+ROS6*      &
                            LTK(K)*LQS(K)))
               ELSE
                  RHOS(K) = MAX(100.,EXP(ROS7+ROS8*LTK(K)+ROS9*LQS(K)+ &
                            ROS10*LTK(K)**2.+ROS11*LQS(K)**2.+ROS12*   &
                            LTK(K)*LQS(K)))
               ENDIF
               RHOS(K) = MIN(900.,MAX(100.,RHOS(K)))
            ENDIF
            LTK(K) = LOG(TK3D(K))
            LQS(K) = -1.*LOG(QS3D(K))
            MDS(K) = EXP(MDS1+MDS2*LTK(k)+MDS3*LQS(k)+MDS4*LTK(k)*     &
                     LTK(K)+MDS5*LQS(k)*LQS(K)+MDS6*LTK(k)*LQS(k))/1.E3
            NS3D(K) = 1.E9*QS3D(K)*V2M3/RHOS(K)/MDS(K)**3.
            S3M3D(K) = QS3D(K)*V2M3/RHOS(K)
            IF (AGG_SHAPE.EQ.0) THEN
               SASPR(K) = 1.
            ELSEIF (AGG_SHAPE.EQ.1) THEN
               LTK(K)   = LOG(TK3D(K))
               BDS(K)   = (S3M3D(K)/NS3D(K))**THRD*1.E3
               BDS(K)   = MIN(MAX(BDS(K),DSMIN*1.E3),DSMAX*1.E3)
               LBDS(K)  = LOG(BDS(K)*1.E3)
               LROS(K)  = LOG(RHOS(K))
               SASPR(K) = MAX(0.2,MIN(SASMAX,ASP1+ASP2*LROS(K)+ASP3*   &
                          LROS(K)**2.+ASP4*LBDS(K)+ASP5*LBDS(K)**2.+   &
                          ASP6*LBDS(K)**3.))
            ENDIF
            IF (AFAS_3M.EQ.0) THEN
               AFAS(K) = AFAS0
            ELSEIF (AFAS_3M.EQ.1) THEN
               MVRS(K) = MIN(DSMAX/2.,MAX(DSMIN/2.,(QS3D(K)/NS3D(K)/   &
                         RHOS(K)/C4PI3)**THRD))
               IF (QC3D(K).GE.QSMALL.OR.NS3D(K).LT.0.5) THEN
                  AFAS(K) = MIN(AFAMAX,MAX(AFAMIN,(AFA1+AFA2*          &
                            LOG(NS3D(K))+AFA3*LOG(NS3D(K))**2.+        &
                            AFA4*LOG(MVRS(K)))/(1.+AFA5*               &
                            LOG(NS3D(K))+AFA6*LOG(MVRS(K)))))
               ELSE
                  AFAS(K) = MIN(AFAMAX,MAX(AFAMIN,(AFB1+AFB2*          &
                            LOG(NS3D(K))+AFB3*LOG(NS3D(K))**2.+        &
                            AFB4*LOG(MVRS(K)))/(1.+AFB5*               &
                            LOG(NS3D(K))+AFB6*LOG(MVRS(K)))))
               ENDIF
               IF (TK3D(K).GT.TK0C) AFAS(K) = MIN(AFAMAX,0.01*EXP(-1.* &
               LOG(MVRS(K))))
            ENDIF
         ENDIF
         IF (QG3D(K).GE.QSMALL.AND.NG3D(K).LT.NSMALL) THEN
            IHAIL(K) = 0
            IF (ICE_RHOG.EQ.0) THEN
               RHOG(K) = RHOG1
            ELSEIF (ICE_RHOG.EQ.1) THEN
               RHOG(K) = RHOG1
               TC3D(K) = TK3D(K)-TK0C
               MVG0(K) = EXP(8.8283+0.25*LOG(QG3D(K)))*1.E-6
               DSLL(K) = 1.E-2*(EXP(MIN(20.,-TC3D(K)/(1.1E4*RHO(K)*(   &
                         QC3D(K)+QR3D(K))-1.3E3*RHO(K)*QI3D(K)+1.)))-1.)
               DSLL(K) = MIN(1.,MAX(1.E-3,DSLL(K)))
               IF (MVG0(K).GT.DSLL(K)) THEN
                  IHAIL(K) = 1
                  RHOG(K) = 900.
               ELSE
                  IHAIL(K) = 0
                  LQG(K)  = -1.*LOG(QG3D(K))
                  RHOG(K) = MIN(900.,MAX(400.,EXP(ROG1*LQG(K)**3.+ROG2*&
                            LQG(K)**2.+ROG3*LQG(K)+ROG4)))
!                  IF (QC3D(K).GE.QSMALL.OR.QR3D(K).GE.QSMALL)          &
!                  RHOG(K) = ((QC3D(K)+QR3D(K))*RHOW+QG3D(K)*RHOG(K))/( &
!                  QC3D(K)+QR3D(K)+QG3D(K))
               ENDIF
            ENDIF
            LTK(K)  = LOG(TK3D(K))
            LQG(K)  = -1.*LOG(QG3D(K))
            IF (IHAIL(K).EQ.0) THEN
               MDG(K)  = EXP(MDG1+MDG2*LTK(K)+MDG3*LQG(K)+MDG4*LTK(K)**&
                         2.+MDG5*LQG(K)**2.+MDG6*LTK(K)*LQG(K))/1.E3
            ELSEIF (IHAIL(K).EQ.1) THEN
               MDG(K)  = EXP(MDH1+MDH2*LTK(K)+MDH3*LQG(K)+MDH4*LTK(K)**&
                         2.+MDH5*LQG(K)**2.+MDH6*LTK(K)*LQG(K))/1.E3
            ENDIF
            NG3D(K)  = 1.E9*QG3D(K)*V2M3/RHOG(K)/MDG(K)**3.
            G3M3D(K) = QG3D(K)*V2M3/RHOG(K)
            IF (GRAP_SHAPE.EQ.0) THEN
               GASPR(K) = 1.
            ELSEIF (GRAP_SHAPE.EQ.1) THEN
               LTK(K)   = LOG(TK3D(K))
               LBDG(K)  = LOG(MDG(K)*1.E3)
               LROG(K)  = LOG(RHOG(K))
               GASPR(K) = MAX(0.2,MIN(SASMAX,ASP1+ASP2*LROG(K)+ASP3*   &
                          LROG(K)**2.+ASP4*LBDG(K)+ASP5*LBDG(K)**2.+   &
                          ASP6*LBDG(K)**3.))
            ENDIF
            IF (AFAG_3M.EQ.0) THEN
               AFAG(K) = AFAG0
            ELSEIF (AFAG_3M.EQ.1) THEN
               MVRG(K) = MIN(DGMAX/2.,MAX(DGMIN/2.,(QG3D(K)/NG3D(K)/   &
                         RHOG(K)/C4PI3)**THRD))
               IF (QC3D(K).GE.QSMALL.OR.NG3D(K).LT.0.5) THEN
                  AFAG(K) = MIN(AFAMAX,MAX(AFAMIN,(AFA1+AFA2*          &
                            LOG(NG3D(K))+AFA3*LOG(NG3D(K))**2.+        &
                            AFA4*LOG(MVRG(K)))/(1.+AFA5*               &
                            LOG(NG3D(K))+AFA6*LOG(MVRG(K)))))
               ELSE
                  AFAG(K) = MIN(AFAMAX,MAX(AFAMIN,(AFB1+AFB2*          &
                            LOG(NG3D(K))+AFB3*LOG(NG3D(K))**2.+        &
                            AFB4*LOG(MVRG(K)))/(1.+AFB5*               &
                            LOG(NG3D(K))+AFB6*LOG(MVRG(K)))))
               ENDIF
               IF (TK3D(K).GT.TK0C) AFAG(K) = MIN(AFAMAX,0.01*EXP(-1.* &
               LOG(MVRG(K))))
            ENDIF
         ENDIF
! AT SUBSATURATION, REMOVE SMALL AMOUNTS OF CLOUD/PRECIP WATER
         IF (SSRW(K).LT.-1.E-1) THEN
            IF (QR3D(K).LT.QSMAL1) THEN
               QV3D(K) = QV3D(K)+QR3D(K)
               TK3D(K) = TK3D(K)-QR3D(K)*XXLV(K)/CPM(K)
               QR3D(K) = 0.; NR3D(K) = 0.; AFAR(K) = 0.; LAMR(K) = 0.
            ENDIF
            IF (QC3D(K).LT.QSMAL1) THEN
               QV3D(K) = QV3D(K)+QC3D(K)
               TK3D(K) = TK3D(K)-QC3D(K)*XXLV(K)/CPM(K)
               QC3D(K) = 0.; NC3D(K) = 0.
            ENDIF
         ENDIF
         IF (SSRI(K).LT.-1.E-1) THEN
            IF (QI3D(K).LT.QSMAL1) THEN
               QV3D(K) = QV3D(K)+QI3D(K)
               TK3D(K) = TK3D(K)-QI3D(K)*XXLS(K)/CPM(K)
               QI3D(K) = 0.; NI3D(K) = 0.
            ENDIF
            IF (QS3D(K).LT.QSMAL1) THEN
               QV3D(K) = QV3D(K)+QS3D(K)
               TK3D(K) = TK3D(K)-QS3D(K)*XXLS(K)/CPM(K)
               QS3D(K) = 0.; NS3D(K) = 0.
            ENDIF
            IF (QG3D(K).LT.QSMAL1) THEN
               QV3D(K) = QV3D(K)+QG3D(K)
               TK3D(K) = TK3D(K)-QG3D(K)*XXLS(K)/CPM(K)
               QG3D(K) = 0.; NG3D(K) = 0.
            ENDIF
         ENDIF
         ESW(K)  = MIN(0.99*P3D(K),POLYSVP(TK3D(K),0))
         ESI(K)  = MIN(0.99*P3D(K),POLYSVP(TK3D(K),1))
         IF (ESI(K).GT.ESW(K)) ESI(K) = ESW(K)
         QVSW(K) = 0.622*ESW(K)/(P3D(K)-ESW(K))
         QVSI(K) = 0.622*ESI(K)/(P3D(K)-ESI(K))
         SSRW(K) = QV3D(K)/QVSW(K)-1.
         SSRI(K) = QV3D(K)/QVSI(K)-1.
         XXLV(K) = 3.1484E6-2370.*TK3D(K)
         CPM(K)  = CP*(1.+0.887*QV3D(K))
         RHO(K)  = P3D(K)/(TK3D(K)*(1.+0.61*QV3D(K)))/R                ! [KG/M**3]
!------------------------- ICE NUCLEATION ------------------------------
         IF (TK3D(K).LT.(TK0C-2.).AND.SSRI(K).GT.1.E-4) THEN
            CALL ICENU(DT,TK3D(K),P3D(K),RHO(K),QV3D(K),QI3D(K),       &
                 NI3D(K))
            ESW(K)  = MIN(0.99*P3D(K),POLYSVP(TK3D(K),0))
            ESI(K)  = MIN(0.99*P3D(K),POLYSVP(TK3D(K),1))
            IF (ESI(K).GT.ESW(K)) ESI(K) = ESW(K)
            SSRW(K) = QV3D(K)/(0.622*ESW(K)/(P3D(K)-ESW(K)))-1.
            SSRI(K) = QV3D(K)/(0.622*ESI(K)/(P3D(K)-ESI(K)))-1.
            RHO(K)  = P3D(K)/(TK3D(K)*(1.+0.61*QV3D(K)))/R
         ENDIF
         IF (SSRW(K).GT.RSMALL.AND.W3D(K).GT.1.E-5) THEN
            IF (IACT.EQ.1) THEN                                        ! USE ROGERS AND YAU (1989) TO RELATE NUMBER ACTIVATED TO W BASED ON TWOMEY 1959
               C1 = 120.; K1 = 0.4
               WCM(K)   = W3D(K)*100.                                  ! CONVERT FROM M/S TO CM/S
               ZCN99(K) = 0.88*C1**(2./(K1+2.))*(7.E-2*WCM(K)**        &
                          1.5)**(K1/(K1+2.))
               NACac(K) = MIN(1.E10,MAX(0.,ZCN99(K)*1.E6-NC3D(K)))     ! CONVERT FROM CM-3 TO M-3 and FROM M-3 TO KG-1
               ESW(K)   = MIN(0.99*P3D(K),POLYSVP(TK3D(K),0))
               QVSW(K)  = 0.622*ESW(K)/(P3D(K)-ESW(K))
               XXLV(K)  = 3.1484E6-2370.*TK3D(K)
               CPM(K)   = CP*(1.+0.887*QV3D(K))
               QACac(K) = NACac(K)*4.5E-15
            ELSEIF (IACT.EQ.2) THEN
               IF (LAND.EQ.1.) THEN                                    ! CONTINENTAL AVERAGED
                  C1 = 1000./RHO(K)*1.E6; K1 = 0.5; ZCMAX = 6.4E9
               ELSEIF (LAND.EQ.2.) THEN                                ! MARINE
                  C1 = 120./RHO(K)*1.E6; K1 = 0.4; ZCMAX = 3.4E8
               ELSE                                                    ! CONTINENTAL CLEAN
                  C1 = 250./RHO(K)*1.E6; K1 = 0.45; ZCMAX = 1.E9
               ENDIF
               CALL cal_A1AdA2(P3D(K),TK3D(K),QV3D(K),alf(K),Tcb(K),   &
                    A1(K),A3(K),A2(K))
               ZCN99(K) = EXP(1./(M_b/K1+M_c)*(LOG(A1(K)*A3(K)*W3D(K)/ &
                          A2(K))-M_a+M_b/K1*LOG(C1)-M_d*Tcb(K)))
               NACac(K) = MIN(ZCMAX,MAX(0.,ZCN99(K)-NC3D(K)))
               IF (NACac(K).GT.1.E-2) THEN
                  RACT(K)  = EXP(M_a+M_d*Tcb(K))*SSRW(K)**(M_b-1.)*    &
                             NACac(K)**(M_c-1.)
                  RACT(K)  = MIN(1.E-5,MAX(1.E-8,RACT(K)))
                  QACac(K) = NACac(K)*RHOW*4.*RACT(K)**3./3.
               ENDIF
            ENDIF
            ESW(K)   = MIN(0.99*P3D(K),POLYSVP(TK3D(K),0))
            QVSW(K)  = 0.622*ESW(K)/(P3D(K)-ESW(K))
            XXLV(K)  = 3.1484E6-2370.*TK3D(K)
            CPM(K)   = CP*(1.+0.887*QV3D(K))
            QV3D(K)  = MAX(0.,QV3D(K)-QACac(K))
            QC3D(K)  = MAX(0.,QC3D(K)+QACac(K))
            NC3D(K)  = MAX(0.,NC3D(K)+NACac(K))
            TK3D(K)  = TK3D(K)+XXLV(K)*QACac(K)/CPM(K)
            RHO(K)   = P3D(K)/(TK3D(K)*(1.+0.61*QV3D(K)))/R
         ENDIF
         IF (QC3D(K).GE.QSMALL.OR.QR3D(K).GE.QSMALL.OR.QI3D(K).GE.     &
            QSMALL.OR.QS3D(K).GE.QSMALL.OR.QG3D(K).GE.QSMALL) THEN
            CALL MAIN_MP(DT,TK3D(K),QV3D(K),P3D(K),RHO(K),QC3D(K),     &
                 QR3D(K),QI3D(K),QS3D(K),QG3D(K),NC3D(K),NR3D(K),      &
                 NI3D(K),NS3D(K),NG3D(K),SMLF(K),GMLF(K),W3D(K))
         ENDIF
!----- AT SUBSATURATION, REMOVE SMALL AMOUNTS OF HYDROMETEORS ----------
         ESW(K)  = MIN(0.99*P3D(K),POLYSVP(TK3D(K),0))
         ESI(K)  = MIN(0.99*P3D(K),POLYSVP(TK3D(K),1))
         IF (ESI(K).GT.ESW(K)) ESI(K) = ESW(K)
         SSRW(K) = QV3D(K)/(0.622*ESW(K)/(P3D(K)-ESW(K)))-1.
         SSRI(K) = QV3D(K)/(0.622*ESI(K)/(P3D(K)-ESI(K)))-1.
         XXLV(K) = 3.1484E6-2370.*TK3D(K)
         XXLS(K) = 3.15E6-2.37E3*TK3D(K)+0.3337E6
         CPM(K)  = CP*(1.+0.887*QV3D(K))
         IF (SSRW(K).LT.-1.E-1) THEN
            IF (QR3D(K).LT.QSMAL1) THEN
               QV3D(K) = QV3D(K)+QR3D(K)
               TK3D(K) = TK3D(K)-QR3D(K)*XXLV(K)/CPM(K)
               QR3D(K) = 0.; NR3D(K) = 0.; AFAR(K) = 0.; LAMR(K) = 0.
            ENDIF
            IF (QC3D(K).LT.QSMAL1) THEN
               QV3D(K) = QV3D(K)+QC3D(K)
               TK3D(K) = TK3D(K)-QC3D(K)*XXLV(K)/CPM(K)
               QC3D(K) = 0.; NC3D(K) = 0.
            ENDIF
         ENDIF
         IF (SSRI(K).LT.-1.E-1) THEN
            IF (QI3D(K).LT.QSMAL1) THEN
               QV3D(K) = QV3D(K)+QI3D(K)
               TK3D(K) = TK3D(K)-QI3D(K)*XXLS(K)/CPM(K)
               QI3D(K) = 0.;  NI3D(K) = 0.
            ENDIF
            IF (QS3D(K).LT.QSMAL1) THEN
               QV3D(K) = QV3D(K)+QS3D(K)
               TK3D(K) = TK3D(K)-QS3D(K)*XXLS(K)/CPM(K)
               QS3D(K) = 0.; NS3D(K) = 0.
            ENDIF
            IF (QG3D(K).LT.QSMAL1) THEN
               QV3D(K) = QV3D(K)+QG3D(K)
               TK3D(K) = TK3D(K)-QG3D(K)*XXLS(K)/CPM(K)
               QG3D(K) = 0.; NG3D(K) = 0.
            ENDIF
         ENDIF
         IF (TK3D(K).GT.TK0C.AND.QI3D(K).GT.0.) THEN
            QR3D(K) = QR3D(K)+QI3D(K)
            TK3D(K) = TK3D(K)-QI3D(K)*XXLF(K)/CPM(K)
            NR3D(K) = NR3D(K)+NI3D(K)
            QI3D(K) = 0.;  NI3D(K) = 0.
         ENDIF
         IF (QC3D(K).LT.QSMALL.OR.NC3D(K).LT.NSMALL) THEN
            QV3D(K) = QV3D(K)+QC3D(K)
            TK3D(K) = TK3D(K)-QC3D(K)*XXLV(K)/CPM(K)
            QC3D(K) = 0.; NC3D(K) = 0.
         ENDIF
         IF (QR3D(K).LT.QSMALL.OR.NR3D(K).LT.NSMALL) THEN
            QV3D(K) = QV3D(K)+QR3D(K)
            TK3D(K) = TK3D(K)-QR3D(K)*XXLV(K)/CPM(K)
            QR3D(K) = 0.; NR3D(K) = 0.; AFAR(K) = 0.; LAMR(K) = 0.
         ENDIF
         IF (QI3D(K).LT.QSMALL.OR.NI3D(K).LT.NSMALL) THEN
            QV3D(K) = QV3D(K)+QI3D(K)
            TK3D(K) = TK3D(K)-QI3D(K)*XXLS(K)/CPM(K)
            QI3D(K) = 0.;  NI3D(K) = 0.
         ENDIF
         IF (QS3D(K).LT.QSMALL.OR.NS3D(K).LT.NSMALL) THEN
            QV3D(K) = QV3D(K)+QS3D(K)
            TK3D(K) = TK3D(K)-QS3D(K)*XXLS(K)/CPM(K)
            QS3D(K) = 0.; NS3D(K) = 0.
         ENDIF
         IF (QG3D(K).LT.QSMALL.OR.NG3D(K).LT.NSMALL) THEN
            QV3D(K) = QV3D(K)+QG3D(K)
            TK3D(K) = TK3D(K)-QG3D(K)*XXLS(K)/CPM(K)
            QG3D(K) = 0.; NG3D(K) = 0.
         ENDIF
         RHO(K) = P3D(K)/(TK3D(K)*(1.+0.61*QV3D(K)))/R
         VT_QC(K) = 0.; VT_NC(K) = 0.; VT_QR(K) = 0.; VT_NR(K) = 0.
         VT_QI(K) = 0.; VT_NI(K) = 0.; VT_QS(K) = 0.; VT_NS(K) = 0.
         VT_QG(K) = 0.; VT_NG(K) = 0.
         IF (QC3D(K).GE.QSMALL.OR.QR3D(K).GE.QSMALL.OR.QI3D(K).GE.     &
            QSMALL.OR.QS3D(K).GE.QSMALL.OR.QG3D(K).GE.QSMALL) THEN
            CALL SEDI_FALL(TK3D(K),P3D(K),QV3D(K),QC3D(K),QR3D(K),     &
                 QI3D(K),QS3D(K),QG3D(K),NC3D(K),NR3D(K),NI3D(K),      &
                 NS3D(K),NG3D(K),VT_QC(K),VT_QR(K),VT_QI(K),VT_QS(K),  &
                 VT_QG(K),VT_NC(K),VT_NR(K),VT_NI(K),VT_NS(K),VT_NG(K),&
                 RHO(K),SMLF(K),GMLF(K))
         ENDIF
         IF (QC3D(K).GE.QSMALL.AND.NC3D(K).GE.NSMALL) THEN
            MVDC(K) = (QC3D(K)*iAMW/NC3D(K))**THRD
            IF (MVDC(K).GT.DCR) THEN
               QR3D(K) = QR3D(K)+QC3D(K)
               NR3D(K) = NR3D(K)+NC3D(K)
               QC3D(K) = 0.; NC3D(K) = 0.
            ENDIF
         ENDIF
         IF (QR3D(K).GE.QSMALL.AND.NR3D(K).GE.NSMALL) THEN
            MVDR(K) = (QR3D(K)*iAMW/NR3D(K))**THRD
            IF (MVDR(K).LT.DCR) THEN
               QC3D(K) = QC3D(K)+QR3D(K)
               NC3D(K) = NC3D(K)+NR3D(K)
               QR3D(K) = 0.; NR3D(K) = 0.; AFAR(K) = 0.; LAMR(K) = 0.
            ENDIF
         ENDIF
      ENDDO
!----------------- CALCULATED FALLING TERMS ----------------------------
      CALL PTFLUX(QC3D(1),VT_QC(1),RHO(1),DZ3D(1),KTE,DT,DTMN,CLODNCV)
      CALL PTFLUX(QR3D(1),VT_QR(1),RHO(1),DZ3D(1),KTE,DT,DTMN,RAINNCV)
      CALL PTFLUX(QI3D(1),VT_QI(1),RHO(1),DZ3D(1),KTE,DT,DTMN,ICENCV)
      CALL PTFLUX(QS3D(1),VT_QS(1),RHO(1),DZ3D(1),KTE,DT,DTMN,SNOWNCV)
      CALL PTFLUX(QG3D(1),VT_QG(1),RHO(1),DZ3D(1),KTE,DT,DTMN,GRAPNCV)
      RAINNC = RAINNC+CLODNCV+RAINNCV+ICENCV+SNOWNCV+GRAPNCV
      SNOWNC = SNOWNC+SNOWNCV+ICENCV
      GRAPNC = GRAPNC+GRAPNCV
      SR     = (ICENCV+SNOWNCV+GRAPNCV)/(CLODNCV+RAINNCV+ICENCV+       &
               SNOWNCV+GRAPNCV+1.E-12)
      CALL FLFLUX(NC3D(1),VT_NC(1),RHO(1),DZ3D(1),KTE,DT)
      CALL FLFLUX(NR3D(1),VT_NR(1),RHO(1),DZ3D(1),KTE,DT)
      CALL FLFLUX(NI3D(1),VT_NI(1),RHO(1),DZ3D(1),KTE,DT)
      CALL FLFLUX(NS3D(1),VT_NS(1),RHO(1),DZ3D(1),KTE,DT)
      CALL FLFLUX(NG3D(1),VT_NG(1),RHO(1),DZ3D(1),KTE,DT)
!----------------------------------------------------------------------
      DO K = KTS,KTE
         IF (SSAT.EQ.0) THEN
            ESW(K)  = MIN(0.99*P3D(K),POLYSVP(TK3D(K),0))
            QVSW(K) = 0.622*ESW(K)/(P3D(K)-ESW(K))
            XXLV(K) = 3.1484E6-2370.*TK3D(K)
            CPM(K)  = CP*(1.+0.887*QV3D(K))
            CND(K)  = (QV3D(K)-QVSW(K))/(1.+XXLV(K)**2.*QVSW(K)/(      &
                      CPM(K)*RV*TK3D(K)**2.))
            CND(K)  = MAX(-QC3D(K),CND(K))
            QV3D(K) = MAX(0.,QV3D(K)-CND(K))
            TK3D(K) = TK3D(K)+CND(K)*XXLV(K)/CPM(K)
            QC3D(K) = MAX(0.,QC3D(K)+CND(K))
            IF (QC3D(K).GE.QSMALL.AND.NC3D(K).LT.NSMALL) THEN
               LTK(K)  = LOG(TK3D(K))
               LQC(K)  = -1.*LOG(QC3D(K))
               MDC(K)  = EXP(MDC1+MDC2*LTK(K)+MDC3*LQC(K)+MDC4*LTK(K)* &
                         LTK(K)+MDC5*LQC(K)*LQC(K)+MDC6*LTK(K)*        &
                         LQC(K))/1.E3
               NC3D(K) = 1.E9*QC3D(K)*V2M3/RHOW/MDC(K)**3.
            ENDIF
         ELSEIF (SSAT.EQ.1.OR.SSAT.EQ.2) THEN
            ESW(K)  = MIN(0.99*P3D(K),POLYSVP(TK3D(K),0))
            ESI(K)  = MIN(0.99*P3D(K),POLYSVP(TK3D(K),1))
            IF (ESI(K).GT.ESW(K)) ESI(K) = ESW(K)
            QVSW(K) = 0.622*ESW(K)/(P3D(K)-ESW(K))
            QVSI(K) = 0.622*ESI(K)/(P3D(K)-ESI(K))
            CND(K)  = (TK3D(K)-238.15)/(TK0C-238.15)
            DEP(K)  = (TK0C-TK3D(K))/(TK0C-238.15)
            XXLV(K) = 3.1484E6-2370.*TK3D(K)
            XXLS(K) = 3.15E6-2370.*TK3D(K)+0.3337E6
            CPM(K)  = CP*(1.+0.887*QV3D(K))
            ABW(K)  = XXLV(K)/(RV*TK3D(K)**2.)
            ABI(K)  = XXLS(K)/(RV*TK3D(K)**2.)
            IF (QC3D(K).LT.QSMALL) QC3D(K) = QSMALL
            IF (QI3D(K).LT.QSMALL) QI3D(K) = QSMALL
            IF (TK3D(K).GE.TK0C) THEN
               DEP(K) = 0.
               CND(K) = 1.
               QI3D(K) = 0.
            ENDIF
            IF (TK3D(K).LT.238.15) THEN
               CND(K) = 0.
               DEP(K) = 1.
               QC3D(K) = 0.
            ENDIF
            Y5(K)   = CND(K)*XXLV(K)/CPM(K)+DEP(K)*XXLS(K)/CPM(K)
            Y1(K)   = QC3D(K)*QVSW(K)/(QC3D(K)+QI3D(K))
            Y2(K)   = QI3D(K)*QVSI(K)/(QC3D(K)+QI3D(K))
            Y4(K)   = ABW(K)*Y1(K)+ABI(K)*Y2(K)
            QVS(K)  = Y1(K)+Y2(K)
            RSUB(K) = (QV3D(K)-QVS(K))/(1.+Y4(K)*Y5(K))
            CND(K)  = CND(K)*RSUB(K)
            DEP(K)  = DEP(K)*RSUB(K)
            IF (QC3D(K).LT.QSMALL) QC3D(K) = 0.
            IF (QI3D(K).LT.QSMALL) QI3D(K) = 0.
            CND(K)  = MAX(-QC3D(K),CND(K))
            DEP(K)  = MAX(-QI3D(K),DEP(K))
            TK3D(K) = TK3D(K)+CND(K)*XXLV(K)/CPM(K)+DEP(K)*XXLS(K)/    &
                      CPM(K)
            QV3D(K) = MAX(0.,QV3D(K)-CND(K)-DEP(K))
            QC3D(K) = MAX(0.,QC3D(K)+CND(K))
            QI3D(K) = MAX(0.,QI3D(K)+DEP(K))
            IF (QC3D(K).GE.QSMALL.AND.NC3D(K).LT.NSMALL) THEN
               LTK(K)  = LOG(TK3D(K))
               LQC(K)  = -1.*LOG(QC3D(K))
               MDC(K)  = EXP(MDC1+MDC2*LTK(K)+MDC3*LQC(K)+MDC4*LTK(K)* &
                         LTK(K)+MDC5*LQC(K)*LQC(K)+MDC6*LTK(K)*        &
                         LQC(K))/1.E3
               NC3D(K) = 1.E9*QC3D(K)*V2M3/RHOW/MDC(K)**3.
            ENDIF
            IF (QI3D(K).GE.QSMALL.AND.NI3D(K).LT.NSMALL) THEN
               IF (ICE_RHOI.EQ.0) THEN
                  RHOI(K)  = RHOI0
               ELSEIF (ICE_RHOI.EQ.1) THEN
                  TC3D(K)  = TK3D(K)-TK0C
                  ESW(K)   = MIN(0.99*P3D(K),POLYSVP(TK3D(K),0))
                  ESI(K)   = MIN(0.99*P3D(K),POLYSVP(TK3D(K),1))
                  IF (ESI(K).GT.ESW(K)) ESI(K) = ESW(K)
                  HID(K)   = MAX(MIN(NINT(ABS(TC3D(K))/0.25),120),0)
                  QVSI(K)  = 0.622*ESI(K)/(P3D(K)-ESI(K))
                  INHGR(K) = ITBLE(HID(K))
                  RHOI(K)  = RHOI0*EXP(-3.*MAX((QV3D(K)-QVSI(K))-5.E-5,&
                             0.)/INHGR(K))
               ENDIF
               LTK(K) = LOG(TK3D(K))
               LQI(K) = -1.*LOG(QI3D(K))
               MDI(K) = EXP(MDI1+MDI2*LTK(K)+MDI3*LQI(K)+MDI4*         &
                        LTK(K)**2.+MDI5*LQI(K)**2.+MDI6*LTK(K)*        &
                        LQI(K))/1.E3
               NI3D(K) = 1.E9*QI3D(K)*V2M3/RHOI(K)/MDI(K)**3.
            ENDIF
         ENDIF   ! SAT
!----- DUAL-POLARIATION RADAR OPERATOR CALCULATIONS ----------------------
         ZZHR(K) = ZXMIN;  ZZHI(K) = ZXMIN;  ZZHS(K) = ZXMIN
         ZZHG(K) = ZXMIN;  ZZVR(K) = ZXMIN;  ZZVI(K) = ZXMIN
         ZZVS(K) = ZXMIN;  ZZVG(K) = ZXMIN;  ZZHVR(K) = ZXMIN
         ZZHVI(K) = ZXMIN; ZZHVS(K) = ZXMIN; ZZHVG(K) = ZXMIN
         RADS(K) = 0.; RADG(K) = 0.; DBZM(K) = -30.; ZDR(K) = 0.
         KDP(K) = 0.;  KDPR(K) = 0.; KDPI(K) = 0.; KDPS(K) = 0.
         KDPG(K) = 0.
         TC3D(K)  = TK3D(K)-TK0C
         EPSIN(K) = 5.27137+2.16474E-2*TC3D(K)-1.31198E-3*TC3D(K)**2.
         EPSS(K)  = 78.54*(1.-4.579E-3*(TC3D(K)-25.)+1.19E-5*(TC3D(K)- &
                    25.)**2.-2.8E-8*(TC3D(K)-25.)**3.)
         ALPHE(K) = -16.8129/TK3D(K)+6.09265E-2
         LAMDZ(K) = 3.3836E-4*EXP(2513.98/TK3D(K))*1.E-2
         NEREL(K) = 1.+2.*(LAMDZ(K)/DBZWL)**(1.-ALPHE(K))*SIN(PI*      &
                    ALPHE(K)/2.)+(LAMDZ(K)/DBZWL)**(2.-2.*ALPHE(K))
         SCAWA(K) = EPSIN(K)+((EPSS(K)-EPSIN(K))*((LAMDZ(K)/DBZWL)**(  &
                    1.-ALPHE(K))*SIN(ALPHE(K)*PI/2.)+1.))/NEREL(K)
         SCAWB(K) = ((EPSS(K)-EPSIN(K))*((LAMDZ(K)/DBZWL)**(1.-        &
                    ALPHE(K))*COS(ALPHE(K)*PI/2.)))/NEREL(K)+1.25664*  &
                    DBZWL/1.88496
         SCAW0(K) = SCAWA(K)**2.+4.*SCAWA(K)+4.+SCAWB(K)**2.
         SCAKW(K) = ((SCAWA(K)**2.+SCAWA(K)-2.+SCAWB(K)**2.)/          &
                    SCAW0(K))**2.+(3.*SCAWB(K)/SCAW0(K))**2.
         SCAW1(K) = SQRT(((SCAWA(K)-1.)/((SCAWA(K)-1.)**2.+SCAWB(K)**  &
                    2.))**2.+(SCAWB(K)/((SCAWA(K)-1.)**2.+SCAWB(K)**   &
                    2.)**2.))
         IF (QC3D(K).GE.QSMALL.AND.NC3D(K).GE.NSMALL) THEN
            CALL SOLVE_AFAC(TK3D(K),QC3D(K),NC3D(K),LAMC(K),MVDC(K),   &
                 AFAC(K),AVC(K),BVC(K))
            MVRC(K) = (QC3D(K)/NC3D(K)/C4PI3W)**THRD
            MVRC(K) = MIN(MAX(MVRC(K),RCMIN),RCMAX)
            EFRC(K) = EXP(EFC1+EFC2*LOG(NC3D(K))+EFC3*LOG(MVRC(K)))
         ELSE
            QC3D(K) = 0.; NC3D(K) = 0.; EFRC(K) = 0.
         ENDIF
         IF (QR3D(K).GE.QSMALL.AND.NR3D(K).GE.NSMALL) THEN
            CALL SOLVE_AFAR(TK3D(K),QC3D(K),QR3D(K),NR3D(K),LAMR(K),   &
                 MVDR(K),AFAR(K),RASPR(K),AVR(K),BVR(K))
            IF (QR3D(K).GE.QLIMIT) THEN
               ORA0(K) = 10.
               ORAR(K) = EXP(-2.*(ORA0(K)*PI/1.8E2)**2.)
               LLMR(K) = LOG(LAMR(K))
               LAR2(K) = LOG(AFAR(K)+2.)
               LLMR2(K) = LLMR(K)*LLMR(K)
               LLMR3(K) = LLMR2(K)*LLMR(K)
               LAR22(K) = LAR2(K)*LAR2(K)
               LAR23(K) = LAR22(K)*LAR2(K)
               LMDR(K) = LOG(1.E6*MVDR(K))
               EORA(K) = EXP(ORA0(K)*PI/180.)
               ORZHR(K) = 0.90655197-0.22828439*LMDR(K)+0.72204255*    &
                          EORA(K)+0.038145645*LMDR(K)**2.-0.55980115*  &
                          EORA(K)**2.+0.10191288*LMDR(K)*EORA(K)-      &
                          1.1570334E-3*LMDR(K)**3.+0.1019446*EORA(K)** &
                          3.+0.010382487*LMDR(K)*EORA(K)**2.-          &
                          0.01654166*LMDR(K)**2.*EORA(K)
               FDBZR(K) = 1950.2082+29.685692*LAR2(K)-17.761215*       &
                          SQRT(LAR2(K))+328.61414*LOG(LLMR(K))**2.-    &
                          1122.8963*SQRT(LLMR(K))
               ZZHR(K) = NR3D(K)*10.**((FDBZR(K)-200.)/10.-18.)*ORZHR(K)
               DBZR(K) = 10.*LOG10(1.E18*ZZHR(K))
               IF (AFAR(K).GE.3.) THEN
                  FZDRR(K) = EXP(27.639873+3.7680268*LAR2(K)-4.3382556*&
                             LLMR(K)+0.36498072*LAR22(K)+0.29286255*   &
                             LLMR2(K)-0.58159197*LAR2(K)*LLMR(K)+      &
                             9.968596E-3*LAR23(K)-7.1806791E-3*        &
                             LLMR3(K)+0.023642573*LAR2(K)*LLMR2(K)-    &
                             0.028767891*LAR22(K)*LLMR(K))/1.E4*       &
                             ORAR(K)/0.9409
               ELSE
                  FZDRR(K) = EXP(10.243103+0.46561642*LAR2(K)+         &
                             1.7270962*LLMR(K)+5.8974032E-3*LAR22(K)-  &
                             0.34268382*LLMR2(K)+0.10193409*LAR2(K)*   &
                             LLMR(K)-0.11005788*LAR23(K)+0.015622039*  &
                             LLMR3(K)-0.038623263*LAR2(K)*LLMR2(K)+    &
                             0.10413007*LAR22(K)*LLMR(K))/1.E4*ORAR(K)/&
                             0.9409
               ENDIF
               ZZVR(K)  = 10.**(DBZR(K)/10.-18.-FZDRR(K)/10.)
               IF (MVDR(K).LT.7.E-4) THEN
                  FKDPR(K) = MIN(1.5/MAX(1.,NR3D(K)),EXP(14.837602-    &
                             18.786496/LAR2(K)+72.211021/LAR22(K)-     &
                             124.16879/LAR23(K)+98.39131/LAR2(K)**4.-  &
                             28.355229/LAR2(K)**5.-0.092813834*        &
                             LLMR(K))/1.E10)
               ELSE
                  IF (AFAR(K).LE.3.) THEN
                     FKDPR(K) = EXP(35.450715+2.4505311*LAR2(K)-       &
                                1.4651261*LLMR(K)-0.22974058*LAR22(K)- &
                                0.22139098*LLMR2(K)+0.29382343*LAR2(K)*&
                                LLMR(K))/1.E10
                  ELSE
                     FKDPR(K) = EXP((38.35738+8.8170679*LAR2(K)-       &
                                8.971269*LLMR(K)+0.49634922*LAR22(K)+  &
                                0.51897478*LLMR2(K)-1.0162854*LAR2(K)* &
                                LLMR(K))/(1.+0.16321213*LAR2(K)-       &
                                0.1676006*LLMR(K)+4.0072333E-3*        &
                                LAR22(K)+4.9701831E-3*LLMR2(K)-        &
                                9.1512549E-3*LAR2(K)*LLMR(K)))/1.E10
                  ENDIF
                  FKDPR(K) = MIN(10./MAX(1.,NR3D(K)),FKDPR(K))
               ENDIF
               KDPR(K) = NR3D(K)*FKDPR(K)*0.11/DBZWL*ORAR(K)/0.9409
               IF (ZZVR(K).GT.ZZHR(K)) ZZVR(K) = 0.9999*ZZHR(K)
               ZDRR(K) = 10.*LOG10(ZZHR(K)/ZZVR(K))
            ENDIF
         ELSE
            QR3D(K) = 0.; NR3D(K) = 0.; AFAR(K) = 0.; LAMR(K) = 0.
         ENDIF
         IF (QI3D(K).GE.QSMALL.AND.NI3D(K).GE.NSMALL) THEN
            CALL SOLVE_AFAI(TK3D(K),P3D(K),QV3D(K),QC3D(K),QI3D(K),    &
                 NI3D(K),ADAGR(K),ZETA(K),LAMI(K),AFAI(K),MVDI(K),     &
                 RHOI(K),AMI(K),AVI(K),BVI(K))
            EFRI(K) = (AFAI(K)+3.)/LAMI(K)
            IF (QI3D(K).GE.QLIMIT) THEN
               LLMI(K) = LOG(LAMI(K))
               LAI2(K) = LOG(AFAI(K)+2.)
               LROI(K) = LOG(RHOI(K))
               LMDI(K) = LOG(1.E6*MVDI(K))
               LLMI2(K) = LLMI(K)*LLMI(K)
               LLMI3(K) = LLMI2(K)*LLMI(K)
               LAI22(K) = LAI2(K)*LAI2(K)
               LAI23(K) = LAI22(K)*LAI2(K)
               LMDI2(K) = LMDI(K)*LMDI(K)
               LMDI3(K) = LMDI2(K)*LMDI(K)
               LROI2(K) = LROI(K)*LROI(K)
               LROI3(K) = LROI2(K)*LROI(K)
               ZETA2(K) = ZETA(K)*ZETA(K)
               ZETA3(K) = ZETA2(K)*ZETA(K)
               IF ((ADAGR(K)-1.).GE.SLIMIT) THEN
                  ORA0(K)  = 30.
                  ORAR(K)  = EXP(-2.*(ORA0(K)*PI/1.8E2)**2.)
                  FDBZI(K) = 389.04082+18.161704*LAI2(K)+1.3401764*    &
                             LAI22(K)-0.07360894*LAI23(K)-26.137103*   &
                             LLMI(K)
                  ZHROX(K) = 1.9382E-6*EXP(1.9260732373*LROI(K))
                  LMDI(K)  = LOG(1.E6*MVDI(K))
                  EORA(K)  = EXP(ORA0(K)*PI/180.)
                  ORZHI(K) = 1.0455067-0.078843114*LMDI(K)+0.010633945*&
                             EORA(K)+4.5711471E-5*LMDI2(K)-0.03498671* &
                             EORA(K)**2.+0.059252519*LMDI(K)*EORA(K)-  &
                             1.6814661E-6*LMDI3(K)+0.0078095037*       &
                             EORA(K)**3.-0.0076692047*LMDI(K)*         &
                             EORA(K)**2.+2.020532E-5*LMDI2(K)*EORA(K)
                  ZHZET(K) = 1.
                  ZZHI(K)  = NI3D(K)*10.**((FDBZI(K)-200.)/10.-18.)*   &
                             ZHROX(K)*ZHZET(K)*ORZHI(K)
                  DBZI(K)  = 10.*LOG10(1.E18*ZZHI(K))
                  ZDROX(K) = 0.0585053*LROI3(K)-0.80223023*LROI2(K)+   &
                             3.75756297*LROI(K)-5.86526003
                  ZDZET(K) = MAX(0.,-0.01644484+0.010454643*LMDI(K)+   &
                             9.4367973*ZETA(K)-0.0026137203*LMDI2(K)+  &
                             5.7453232*ZETA2(K)+0.3234453*LMDI(K)*     &
                             ZETA(K)+2.0622912E-4*LMDI3(K)-1.3945604*  &
                             ZETA3(K)-2.2879493*LMDI(K)*ZETA2(K)-      &
                             0.013856582*LMDI2(K)*ZETA(K))
                  FZDRI(K) = EXP((15.181145+0.34794935*LAI2(K)+        &
                             0.11677701*LAI22(K)-6.3942628E-3*LAI23(K)-&
                             1.0984758*LLMI(K))/(1.+0.022930408*       &
                             LAI2(K)+7.5639466E-3*LAI22(K)-4.146688E-4*&
                             LAI23(K)-0.071648235*LLMI(K)))/1.E6*      &
                             ORAR(K)/0.5779*ZDZET(K)*ZDROX(K)
                  ZZVI(K)  = 10.**(DBZI(K)/10.-18.+FZDRI(K)/10.)
                  FKDPI(K) = EXP((54.728473+7.4097498*LAI2(K)-         &
                             7.5413134*LLMI(K)+0.25580463*LAI22(K)+    &
                             0.25257362*LLMI2(K)-0.50222586*LAI2(K)*   &
                             LLMI(K))/(1.+0.081702748*LAI2(K)-         &
                             0.085895343*LLMI(K)+7.6712618E-4*LAI22(K)+&
                             4.5412963E-4*LLMI2(K)-9.3521894E-4*       &
                             LAI2(K)*LLMI(K)))/2.E18
                  KDROX(K) = 1.75E-6*EXP(1.93990711*LROI(K))
                  KDZET(K) = MAX(0.,-0.0067191022+0.030489666*LMDI(K)+ &
                             7.6470878*ZETA(K)-0.01097138*LMDI2(K)+    &
                             9.8497711*ZETA2(K)+0.83287327*LMDI(K)*    &
                             ZETA(K)+9.1590015E-4*LMDI3(K)-7.2917335*  &
                             ZETA3(K)-2.2235781*LMDI(K)*ZETA2(K)-      &
                             0.053116593*LMDI2(K)*ZETA(K))
                  KDPI(K)  = NI3D(K)*FKDPI(K)*KDROX(K)*KDZET(K)*0.11/  &
                             DBZWL*ORAR(K)/0.5779
                  IF (ZZVI(K).LT.ZZHI(K)) ZZVI(K) = 1.0001*ZZHI(K)
                  ZDRI(K) = 10.*LOG10(ZZVI(K)/ZZHI(K))
               ELSEIF ((1.-ADAGR(K)).GE.SLIMIT) THEN
                  ORA0(K)  = 10.
                  ORAR(K)  = EXP(-2.*(ORA0(K)*PI/1.8E2)**2.)
                  FDBZI(K) = 390.47265+18.157805*LAI2(K)+1.3558212*    &
                             LAI22(K)-0.074452587*LAI23(K)-26.228751*  &
                             LLMI(K)
                  ZHROX(K) = 1.4172E-6*EXP(1.9687416987*LROI(K))
                  LMDI(K)  = LOG(1.E6*MVDI(K))
                  EORA(K)  = EXP(ORA0(K)*PI/180.)
                  ORZHI(K) = 0.67789527-9.5992589E-5*LMDI(K)+0.7382078*&
                             EORA(K)-0.0010254973*LMDI2(K)-0.57079919* &
                             EORA(K)**2.+0.025474344*LMDI(K)*EORA(K)+  &
                             5.146872E-7*LMDI3(K)+0.15025366*          &
                             EORA(K)**3.-0.021740747*LMDI(K)*          &
                             EORA(K)**2.+9.0541117E-4*LMDI2(K)*EORA(K)
                  ZHZET(K) = 1.1531946-0.10474946*LMDI(K)+1.3268439*   &
                             ZETA(K)+0.013483871*LMDI2(K)+3.0239757*   &
                             ZETA2(K)-0.53774565*LMDI(K)*ZETA(K)-      &
                             7.6839476E-4*LMDI3(K)+4.3808336*ZETA3(K)+ &
                             0.080046674*LMDI(K)*ZETA2(K)+0.015810946* &
                             LMDI2(K)*ZETA(K)
                  ZZHI(K)  = NI3D(K)*10.**((FDBZI(K)-200.)/10.-18.)*   &
                             ZHROX(K)*ZHZET(K)*ORZHI(K)
                  DBZI(K)  = 10.*LOG10(1.E18*ZZHI(K))
                  ZDROX(K) = 0.05683556*LROI3(K)-0.77585835*LROI2(K)+  &
                             3.62207807*LROI(K)-5.63826259
                  ZDZET(K) = MAX(0.,0.12567123-0.077409338*LMDI(K)-    &
                             8.3245922*ZETA(K)+0.013440196*LMDI2(K)+   &
                             8.7068519*ZETA2(K)-0.53878869*LMDI(K)*    &
                             ZETA(K)-7.0609359E-4*LMDI3(K)+6.371616*   &
                             ZETA3(K)-1.6563927*LMDI(K)*ZETA2(K)+      &
                             0.032682807*LMDI2(K)*ZETA(K))
                  FZDRI(K) = EXP((15.587817+0.23578544*LAI2(K)+        &
                             0.045497555*LAI22(K)-1.0862071*LLMI(K)+   &
                             3.4688369E-3*LLMI2(K))/(1.+0.015556043*   &
                             LAI2(K)+2.1779895E-3*LAI22(K)+            &
                             7.2733931E-5*LAI23(K)-0.064401818*        &
                             LLMI(K)))/1.E6*ORAR(K)/0.9409*ZDZET(K)*   &
                             ZDROX(K)
                  ZZVI(K)  = 10.**(DBZI(K)/10.-18.-FZDRI(K)/10.)
                  FKDPI(K) = EXP((55.096057+7.4047265*LAI2(K)-         &
                             7.6277374*LLMI(K)+0.25324833*LAI22(K)+    &
                             0.25735457*LLMI2(K)-0.50528828*LAI2(K)*   &
                             LLMI(K))/(1.+0.078240442*LAI2(K)-         &
                             0.083742581*LLMI(K)+5.4080817E-4*         &
                             LAI22(K)+2.9610344E-4*LLMI2(K)-           &
                             5.9621769E-4*LAI2(K)*LLMI(K)))/2.E18
                  KDROX(K) = 1.89E-6*EXP(1.92963337*LROI(K))
                  KDZET(K) = MAX(0.,0.044182106-0.020578987*LMDI(K)-   &
                             8.8659688*ZETA(K)+0.0022174675*LMDI2(K)+  &
                             5.2415098*ZETA2(K)-0.40918383*LMDI(K)*    &
                             ZETA(K)-3.696556E-5*LMDI3(K)+3.5504491*   &
                             ZETA3(K)-1.4353113*LMDI(K)*ZETA2(K)+      &
                             0.022796706*LMDI2(K)*ZETA(K))
                  KDPI(K)  = NI3D(K)*FKDPI(K)*KDROX(K)*KDZET(K)*0.11/  &
                             DBZWL*ORAR(K)/0.9409
                  IF (ZZVI(K).GT.ZZHI(K)) ZZVI(K) = 0.9999*ZZHI(K)
                  ZDRI(K) = 10.*LOG10(ZZHI(K)/ZZVI(K))
               ELSEIF (ABS(ADAGR(K)-1.).LT.SLIMIT) THEN
                  ORA0(K)  = 45.
                  ORAR(K)  = EXP(-2.*(ORA0(K)*PI/1.8E2)**2.)
                  FDBZI(K) = 390.47265+18.157805*LAI2(K)+1.3558212*    &
                             LAI22(K)-0.074452587*LAI23(K)-26.228751*  &
                             LLMI(K)
                  ZHROX(K) = 1.9382E-6*EXP(1.9260732373*LROI(K))
                  ORZHI(K) = 1.
                  ZHZET(K) = 1.
                  ZZHI(K)  = NI3D(K)*10.**((FDBZI(K)-200.)/10.-18.)*   &
                             ZHROX(K)*ZHZET(K)*ORZHI(K)
                  DBZI(K)  = 10.*LOG10(1.E18*ZZHI(K))
                  ZZVI(K)  = 10.**(DBZI(K)/10.-18.)
                  KDPI(K)  = 0.
                  ZDRI(K)  = 0.
               ENDIF
            ENDIF
         ELSE
            QI3D(K) = 0.; NI3D(K) = 0.; EFRI(K) = 0.
         ENDIF
         IF (QS3D(K).GE.QSMALL.AND.NS3D(K).GE.NSMALL) THEN
            CALL SOLVE_AFAS(TK3D(K),QS3D(K),QC3D(K),NS3D(K),AFAS(K),   &
                 LAMS(K),MVDS(K),RHOS(K),SASPR(K),AMS(K),AVS(K),BVS(K))
            EFRS(K) = (AFAS(K)+3.)/LAMS(K)
            IF (QS3D(K).GE.QLIMIT) THEN
               IF (SMLF(K).GE.MLIMIT) THEN

                  S3M3D(K) = QS3D(K)*V2M3/RHOS(K)
                  MVRS(K) = MIN(DSMAX/2.,MAX(DSMIN/2.,(QS3D(K)/NS3D(K)/&
                            RHOS(K)/C4PI3)**THRD))
                  AFAS(K) = MIN(AFAMAX,MAX(AFAMIN,EXP(-18.420681-0.5*  &
                            LOG(NS3D(K))-2.94*LOG(MVRS(K)))))
                  LAMS(K) = (EXP(LOG_GAMMA(AFAS(K)+4.)-LOG_GAMMA(      &
                            AFAS(K)+1.))*NS3D(K)/S3M3D(K))**THRD
                  LAMSMIN(K) = (EXP(LOG_GAMMA(AFAS(K)+4.)-             &
                               LOG_GAMMA(AFAS(K)+1.)))**THRD/DSMAX
                  LAMSMAX(K) = (EXP(LOG_GAMMA(AFAS(K)+4.)-             &
                               LOG_GAMMA(AFAS(K)+1.)))**THRD/DSMIN
                  IF (LAMS(K).LT.LAMSMIN(K)) THEN
                     LAMS(K) = LAMSMIN(K)
                     NS3D(K) = S3M3D(K)*EXP(LOG_GAMMA(AFAS(K)+1.)-     &
                               LOG_GAMMA(AFAS(K)+4.)+3.*LOG(LAMS(K)))
                  ELSEIF (LAMS(K).GT.LAMSMAX(K)) THEN
                     LAMS(K) = LAMSMAX(K)
                     NS3D(K) = S3M3D(K)*EXP(LOG_GAMMA(AFAS(K)+1.)-     &
                               LOG_GAMMA(AFAS(K)+4.)+3.*LOG(LAMS(K)))
                  ENDIF

                  SMLV(K) = (RHOS(K)/(RHOW/SMLF(K)-RHOW+RHOS(K)))**MLRA
                  LLMS(K)  = LOG(LAMS(K))
                  LAS5(K)  = LOG(AFAS(K)+5.)
                  SASPR(K) = SMLF(K)*(MIN(1.,(-32.232528-16.735319*    &
                             LAS5(K)+14.830389*LLMS(K)-1.8312883*      &
                             LAS5(K)**2.-1.6827601*LLMS(K)**2.+        &
                             3.6556064*LAS5(K)*LLMS(K)-7.4526392E-2*   &
                             LAS5(K)**3.+6.2393644E-2*LLMS(K)**3.-     &
                             0.20029785*LAS5(K)*LLMS(K)**2.+0.20699596*&
                             LAS5(K)**2.*LLMS(K))/10.))+(1.-SMLF(K))*  &
                             SASPR(K)
                  RHOS(K) = MIN(RHOIMAX,MAX(RHOIMIN,SMLF(K)*RHOW+(1.-  &
                            SMLF(K))*RHOS(K)))
                  ORA0(K) = 10.*SMLF(K)+(1.-SMLF(K))*40.
                  ORAR(K) = EXP(-2.*(ORA0(K)*PI/1.8E2)**2.)
                  ORA1(K) = (3.+4.*ORAR(K)+ORAR(K)**4.)/8.
                  ORA2(K) = (3.-4.*ORAR(K)+ORAR(K)**4.)/8.
                  ORA3(K) = (1.-ORAR(K)**4.)/8.
                  IF (AGG_SHAPE.EQ.0) THEN
                     SCALA(K) = THRD
                  ELSEIF (AGG_SHAPE.EQ.1.OR.AGG_SHAPE.EQ.2) THEN
                     SCA0(K)  = MAX(1.E-6,SQRT(SASPR(K)**(-2.)-1.))
                     SCALA(K) = (1.+SCA0(K)**2.)/SCA0(K)**2.*(1.-      &
                                ATAN(SCA0(K))/SCA0(K))
                  ENDIF
                  SCALB(K) = (1.-SCALA(K))/2.
                  SCAW1(K) = MIXA+MIXB/SMLV(K)+MIXC*RHOS(K)+MIXD/      &
                             SMLV(K)**2.+MIXE*RHOS(K)**2.+MIXF*RHOS(K)/&
                             SMLV(K)
                  SCAFA(K) = 1./(SCALA(K)+SCAW1(K))
                  SCAFB(K) = 1./(SCALB(K)+SCAW1(K))
                  RADS(K) = NS3D(K)*EXP(LOG_GAMMA(AFAS(K)+7.)-         &
                            LOG_GAMMA(AFAS(K)+1.)-6.*LOG(LAMS(K)))
                  ZZHS(K) = MAX(ZXMIN,RADS(K)*(ORA1(K)*SCAFB(K)**2.+   &
                            2.*ORA3(K)*SCAFA(K)*SCAFB(K)+ORA2(K)*      &
                            SCAFA(K)**2.)/(9.*SCAKW(K)))
                  ZZHS(K) = ZZHS(K)*(MAX(0.,-0.1*AFAS(K)+0.5)*SMLF(K)+ &
                            1.)
                  ZZVS(K) = MAX(ZXMIN,RADS(K)*(ORA1(K)*SCAFA(K)**2.+   &
                            2.*ORA3(K)*SCAFA(K)*SCAFB(K)+ORA2(K)*      &
                            SCAFB(K)**2.)/(9.*SCAKW(K)))
                  DBZS(K) = 10.*LOG10(1.E18*ZZHS(K))
                  KDPS(K) = 3.E4*PI/DBZWL*NS3D(K)*EXP(LOG_GAMMA(       &
                            AFAS(K)+4.)-LOG_GAMMA(AFAS(K)+1.)-3.*      &
                            LOG(LAMS(K)))*ABS(SCAFA(K)-SCAFB(K))*ORAR(K)
                  IF (ZZVS(K).GT.ZZHS(K)) ZZVS(K) = 0.9999*ZZHS(K)
                  ZDRS(K) = 10.*LOG10(ZZHS(K)/ZZVS(K))
               ELSE
                  ORA0(K)  = 40.
                  ORAR(K)  = EXP(-2.*(ORA0(K)*PI/1.8E2)**2.)
                  LLMS(K)  = LOG(LAMS(K))
                  LAS2(K)  = LOG(AFAS(K)+2.)
                  LROS(K)  = LOG(RHOS(K))
                  LASP(K)  = LOG(200.*MAX(0.01,SASPR(K)))
                  FDBZS(K) = 428.62235+18.226687*LAS2(K)+1.299724*     &
                             LAS2(K)**2.-0.070558679*LAS2(K)**3.-      &
                             26.070123*LLMS(K)
                  ZHROX(K) = 1.544E-5*EXP(1.95714566*LROS(K))
                  ZZHS(K)  = NS3D(K)*10.**((FDBZS(K)-250.)/10.-18.)*   &
                             ZHROX(K)
                  DBZS(K)  = 10.*LOG10(1.E18*ZZHS(K))
                  ZDROX(K) = 0.14542522*LROS(K)**3.-1.97576772*        &
                             LROS(K)**2.+9.33469066*LROS(K)-14.95051206
                  ZDASP(K) = MAX(0.,-8.85535E-3*LASP(K)**3.-0.10877652*&
                             LASP(K)**2.+0.24567081*LASP(K)+3.06007047)
                  FZDRS(K) = 0.384*ZDROX(K)*ZDASP(K)*ORAR(K)/0.37728
                  ZZVS(K)  = 10.**(DBZS(K)/10.-18.-FZDRS(K)/10.)
                  ZZHVS(K) = 0.9975*10.**(DBZS(K)/10.-18.-FZDRS(K)/20.)
                  FKDPS(K) = EXP(44.30884+2.9903578*LAS2(K)-0.25943237/&
                             LAS2(K)-3.0036608*LLMS(K))/1.E15
                  KDROX(K) = 1.974E-5*EXP(1.90820312*LROS(K))
                  KDASP(K) = MAX(0.,-0.00958782*LASP(K)**3.-0.10043861*&
                             LASP(K)**2.+0.22095032*LASP(K)+3.06677159)
                  KDPS(K)  = NS3D(K)*FKDPS(K)*KDROX(K)*KDASP(K)*0.11/  &
                             DBZWL*ORAR(K)/0.37728
                  IF (ZZVS(K).GT.ZZHS(K)) ZZVS(K) = 0.9999*ZZHS(K)
                  ZDRS(K) = 10.*LOG10(ZZHS(K)/ZZVS(K))
               ENDIF
            ENDIF        
         ELSE
            QS3D(K) = 0.; NS3D(K) = 0.; EFRS(K) = 0.
         ENDIF
         IF (QG3D(K).GE.QSMALL.AND.NG3D(K).GE.NSMALL) THEN
            CALL SOLVE_AFAG(TK3D(K),RHO(K),QG3D(K),QC3D(K),QR3D(K),    &
                 QI3D(K),NG3D(K),LAMG(K),AFAG(K),MVDG(K),RHOG(K),      &
                 GASPR(K),AMG(K),AVG(K),BVG(K))
            IF (QG3D(K).GE.QLIMIT) THEN
               IF (GMLF(K).GE.MLIMIT) THEN

                  G3M3D(K) = QG3D(K)*V2M3/RHOG(K)
                  MVRG(K) = MIN(DGMAX/2.,MAX(DGMIN/2.,(QG3D(K)/NG3D(K)/&
                            RHOG(K)/C4PI3)**THRD))
                  AFAG(K) = MIN(AFAMAX,MAX(AFAMIN,EXP(-18.420681-0.5*  &
                            LOG(NG3D(K))-2.94*LOG(MVRG(K)))))
                  LAMG(K) = (EXP(LOG_GAMMA(AFAG(K)+4.)-LOG_GAMMA(      &
                            AFAG(K)+1.))*NG3D(K)/G3M3D(K))**THRD
                  LAMGMIN(K) = (EXP(LOG_GAMMA(AFAG(K)+4.)-             &
                               LOG_GAMMA(AFAG(K)+1.)))**THRD/DGMAX
                  LAMGMAX(K) = (EXP(LOG_GAMMA(AFAG(K)+4.)-             &
                               LOG_GAMMA(AFAG(K)+1.)))**THRD/DGMIN
                  IF (LAMG(K).LT.LAMGMIN(K)) THEN
                     LAMG(K) = LAMGMIN(K)
                     NG3D(K) = G3M3D(K)*EXP(LOG_GAMMA(AFAG(K)+1.)-     &
                               LOG_GAMMA(AFAG(K)+4.)+3.*LOG(LAMG(K)))
                  ELSEIF (LAMG(K).GT.LAMGMAX(K)) THEN
                     LAMG(K) = LAMGMAX(K)
                     NG3D(K) = G3M3D(K)*EXP(LOG_GAMMA(AFAG(K)+1.)-     &
                               LOG_GAMMA(AFAG(K)+4.)+3.*LOG(LAMG(K)))
                  ENDIF

                  GMLV(K) = (RHOG(K)/(RHOW/GMLF(K)-RHOW+RHOG(K)))**MLRA
                  LLMG(K)  = LOG(LAMG(K))
                  LAG5(K)  = LOG(AFAG(K)+5.)
                  GASPR(K) = GMLF(K)*(MIN(1.,(-32.232528-16.735319*    &
                             LAG5(K)+14.830389*LLMG(K)-1.8312883*      &
                             LAG5(K)**2.-1.6827601*LLMG(K)**2.+        &
                             3.6556064*LAG5(K)*LLMG(K)-7.4526392E-2*   &
                             LAG5(K)**3.+6.2393644E-2*LLMG(K)**3.-     &
                             0.20029785*LAG5(K)*LLMG(K)**2.+0.20699596*&
                             LAG5(K)**2.*LLMG(K))/10.))+(1.-GMLF(K))*  &
                             GASPR(K)
                  RHOG(K) = MIN(900.,MAX(100.,GMLF(K)*RHOW+(1.-        &
                            GMLF(K))*RHOG(K)))
                  ORA0(K) = 10.*GMLF(K)+(1.-GMLF(K))*40.
                  ORAR(K) = EXP(-2.*(ORA0(K)*PI/1.8E2)**2.)
                  ORA1(K) = (3.+4.*ORAR(K)+ORAR(K)**4.)/8.
                  ORA2(K) = (3.-4.*ORAR(K)+ORAR(K)**4.)/8.
                  ORA3(K) = (1.-ORAR(K)**4.)/8.
                  IF (GRAP_SHAPE.EQ.0) THEN
                     SCALA(K) = THRD
                  ELSEIF (GRAP_SHAPE.EQ.1.OR.GRAP_SHAPE.EQ.2) THEN
                     SCA0(K)  = MAX(1.E-6,SQRT(GASPR(K)**(-2.)-1.))
                     SCALA(K) = (1.+SCA0(K)**2.)/SCA0(K)**2.*(1.-      &
                                ATAN(SCA0(K))/SCA0(K))
                  ENDIF
                  SCALB(K) = (1.-SCALA(K))/2.
                  SCAW1(K) = MIXA+MIXB/GMLV(K)+MIXC*RHOG(K)+MIXD/      &
                             GMLV(K)**2.+MIXE*RHOG(K)**2.+MIXF*RHOG(K)/&
                             GMLV(K)
                  SCAFA(K) = 1./(SCALA(K)+SCAW1(K))
                  SCAFB(K) = 1./(SCALB(K)+SCAW1(K))
                  RADG(K) = NG3D(K)*EXP(LOG_GAMMA(AFAG(K)+7.)-         &
                            LOG_GAMMA(AFAG(K)+1.)-6.*LOG(LAMG(K)))
                  ZZHG(K) = MAX(ZXMIN,RADG(K)*(ORA1(K)*SCAFB(K)**2.+   &
                            2.*ORA3(K)*SCAFA(K)*SCAFB(K)+ORA2(K)*      &
                            SCAFA(K)**2.)/(9.*SCAKW(K)))
                  ZZHG(K) = ZZHG(K)*(MAX(0.,-0.1*AFAG(K)+0.5)*GMLF(K)+ &
                            1.)
                  ZZVG(K) = MAX(ZXMIN,RADG(K)*(ORA1(K)*SCAFA(K)**2.+   &
                            2.*ORA3(K)*SCAFA(K)*SCAFB(K)+ORA2(K)*      &
                            SCAFB(K)**2.)/(9.*SCAKW(K)))
                  DBZG(K) = 10.*LOG10(1.E18*ZZHG(K))
                  IF (ZZVG(K).LE.ZZHG(K)) THEN
                     ZDRG(K) = 10.*LOG10(ZZHG(K)/ZZVG(K))
                  ELSE
                     ZDRG(K) = 10.*LOG10(ZZVG(K)/ZZHG(K))
                  ENDIF
                  KDPG(K) = 3.E4*PI/DBZWL*NG3D(K)*EXP(LOG_GAMMA(       &
                            AFAG(K)+4.)-LOG_GAMMA(AFAG(K)+1.)-3.*      &
                            LOG(LAMG(K)))*ABS(SCAFA(K)-SCAFB(K))*ORAR(K)
               ELSE
                  ORA0(K) = 40.
                  ORAR(K) = EXP(-2.*(ORA0(K)*PI/1.8E2)**2.)
                  LLMG(K) = LOG(LAMG(K))
                  LAG2(K) = LOG(AFAG(K)+2.)
                  LROG(K) = LOG(RHOG(K))
                  IF (GASPR(K).LE.1.) THEN
                     LASP(K)  = LOG(200.*MAX(0.01,GASPR(K)))
                     ZHROX(K) = 1.544E-5*EXP(1.95714566*LROG(K))
                     FDBZG(K) = 428.62235+18.226687*LAG2(K)+1.299724*  &
                                LAG2(K)**2.-0.070558679*LAG2(K)**3.-   &
                                26.070123*LLMG(K)
                     ZZHG(K)  = NG3D(K)*10.**((FDBZG(K)-250.)/10.-18.)*&
                                ZHROX(K)
                     DBZG(K)  = 10.*LOG10(1.E18*ZZHG(K))
                     ZDROX(K) = 0.14542522*LROG(K)**3.-1.97576772*     &
                                LROG(K)**2.+9.33469066*LROG(K)-        &
                                14.95051206
                     ZDASP(K) = MAX(0.,-8.85535E-3*LASP(K)**3.-        &
                                0.10877652*LASP(K)**2.+0.24567081*     &
                                LASP(K)+3.06007047)
                     FZDRG(K) = 0.384*ZDROX(K)*ZDASP(K)*ORAR(K)/0.37728
                     ZZVG(K)  = 10.**(DBZG(K)/10.-18.-FZDRG(K)/10.)
                     ZZHVG(K) = 0.9975*10.**(DBZG(K)/10.-18.-FZDRG(K)/ &
                                20.)
                     FKDPG(K) = EXP(44.30884+2.9903578*LAG2(K)-        &
                                0.25943237/LAG2(K)-3.0036608*LLMG(K))/ &
                                1.E15
                     KDROX(K) = 1.974E-5*EXP(1.90820312*LROG(K))
                     KDASP(K) = MAX(0.,-0.00958782*LASP(K)**3.-        &
                                0.10043861*LASP(K)**2.+0.22095032*     &
                                LASP(K)+3.06677159)
                     KDPG(K) = NG3D(K)*FKDPG(K)*KDROX(K)*KDASP(K)*0.11/&
                               DBZWL*ORAR(K)/0.37728
                     IF (ZZVG(K).GT.ZZHG(K)) ZZVG(K) = 0.9999*ZZHG(K)
                     ZDRG(K) = 10.*LOG10(ZZHG(K)/ZZVG(K))
                  ELSE
                     LASP(K)  = LOG(200.*MIN(1.E2,GASPR(K)))
                     FDBZG(K) = 428.57273+18.226687*LAG2(K)+1.299724*  &
                                LAG2(K)**2.-0.070558679*LAG2(K)**3.-   &
                                26.070123*LLMG(K)
                     ZHROX(K) = 1.63247E-5*EXP(1.9461111336*LROG(K))
                     ZZHG(K)  = NG3D(K)*10.**((FDBZG(K)-250.)/10.-18.)*&
                                ZHROX(K)
                     DBZG(K)  = 10.*LOG10(1.E18*ZZHG(K))
                     ZDROX(K) = 0.21196364*LROG(K)**3.-2.99793531*     &
                                LROG(K)**2.+14.49837419*LROG(K)-       &
                                23.49571954
                     ZDASP(K) = MAX(0.,0.04602081*LASP(K)**3.-         &
                                1.22524043*LASP(K)**2.+10.864809*      &
                                LASP(K)-30.01372544)
                     FZDRG(K) = 0.328*ZDROX(K)*ZDASP(K)*ORAR(K)/0.37728
                     ZZVG(K)  = 10.**(DBZG(K)/10.-18.+FZDRG(K)/10.)
                     ZZHVG(K) = 0.9982*10.**(DBZG(K)/10.-18.-FZDRG(K)/ &
                                20.)
                     FKDPG(K) = EXP(44.18702+2.9903578*LAG2(K)-        &
                                0.25943237/LAG2(K)-3.0036608*LLMG(K))/ &
                                1.E15
                     KDROX(K) = 1.424E-5*EXP(1.97240205*LROG(K))
                     KDASP(K) = MAX(0.,0.043095*LASP(K)**3.-1.16143866*&
                                LASP(K)**2.+10.43169617*LASP(K)-       &
                                29.07639304)
                     KDPG(K) = NG3D(K)*FKDPG(K)*KDROX(K)*KDASP(K)*0.11/&
                               DBZWL*ORAR(K)/0.37728
                     IF (ZZVG(K).LT.ZZHG(K)) ZZVG(K) = 1.0001*ZZHG(K)
                     ZDRG(K) = 10.*LOG10(ZZVG(K)/ZZHG(K))
                  ENDIF
               ENDIF
            ENDIF
         ELSE
            QG3D(K) = 0.; NG3D(K) = 0.
         ENDIF
         IF ((ZZHR(K)+ZZHI(K)+ZZHS(K)+ZZHG(K)).GE.ZXMIN) THEN
            DBZM(K) = MAX(-30.,10.*LOG10(1.E18*(ZZHR(K)+ZZHI(K)+       &
                      ZZHS(K)+ZZHG(K))))
            ZDR(K)  = MAX(0.,10.*LOG10((ZZHR(K)+MAX(ZZHI(K),ZZVI(K))+  &
                      ZZHS(K)+MAX(ZZHG(K),ZZVG(K)))/(ZZVR(K)+MIN(      &
                      ZZVI(K),ZZHI(K))+ZZVS(K)+MIN(ZZVG(K),ZZHG(K)))))
            KDP(K)  = MAX(0.,KDPR(K)+KDPI(K)+KDPS(K)+KDPG(K))
         ENDIF
      ENDDO 

      END SUBROUTINE NTU_MICRO
!======================================================================
!
!======================================================================
      SUBROUTINE ICENU(DT,TK1D,P1D,RHO,QV1D,QI1D,NI1D)
!======================================================================
      IMPLICIT NONE
      INTEGER :: IDEPNU
      REAL :: DT,TK1D,P1D,RHO,QV1D,QI1D,NI1D,QNIN,QVSI,XXLS,ESI,QNDvi, &
              NNDvi,NVI0,SRI,TC1D,QVDMAX,NNUMAX,SSRI,CPM

      QNDvi = 0.; NNDvi = 0.

      IDEPNU = 7
      QNIN   = 4.E5
      TC1D   = TK1D-TK0C
      XXLS   = 3.15E6-2.37E3*TK1D+0.3337E6
      ESI    = MIN(0.99*P1D,POLYSVP(TK1D,1))
      QVSI   = 0.622*ESI/(P1D-ESI)
      SRI    = QV1D/QVSI
      SSRI   = QV1D/QVSI-1.
      CPM    = CP*(1.+0.887*QV1D)
      QVDMAX = (QV1D-QVSI)/(1.+XXLS**2.*QVSI/(CPM*RV*TK1D**2.))         ! SUPERSATURATED WATER FIXED DUE TO TEMPERATURE CHANGE
      NNUMAX = 0.98*QVDMAX/MI0                                          ! INITIATION OF CLOUD ICE CONSTRAINED by 0.98*SUPICE
      SRI    = QV1D/QVSI                                                ! SATURATION RATIO OVER ICE

      IF (IDEPNU.EQ.0) THEN                                             ! NO DEPOSITION NUCLEATION
         NNDvi = 0.
      ELSEIF (IDEPNU.EQ.1) THEN                                         ! Fletcher (1962)
         NVI0  = 1.E-2*EXP(0.6*(TK0C-MAX(TK1D,2.46E2)))
         NNDvi = MIN(QNIN,NNUMAX,MAX(0.,NVI0/RHO-NI1D))
      ELSEIF (IDEPNU.EQ.2) THEN                                         ! Cooper
         NVI0  = 5.*EXP(0.304*(TK0C-MAX(TK1D,2.46E2)))
         NNDvi = MIN(QNIN,NNUMAX,MAX(0.,NVI0/RHO-NI1D))
      ELSEIF (IDEPNU.EQ.3) THEN                                         ! Huffman (1973)
         NVI0  = 1007.08*SSRI**4.5*1.E3
         NNDvi = MIN(QNIN,NNUMAX,MAX(0.,NVI0/RHO-NI1D))
      ELSEIF (IDEPNU.EQ.4) THEN                                         ! Meyers et al (1992)
         NVI0  = 1.E3*EXP(1.296E1*SSRI-6.39E-1)
         NNDvi = MIN(QNIN,NNUMAX,MAX(0.,NVI0/RHO-NI1D))
      ELSEIF (IDEPNU.EQ.5) THEN                                         ! Bacteria(P. syringae):Yankofsky 1981
         NVI0  = QNIN*1.1776*EXP(-89318./((LOG(SRI))**2.*TK1D**3.))
         NNDvi = MIN(QNIN,NNUMAX,MAX(0.,NVI0*DT))
      ELSEIF (IDEPNU.EQ.6) THEN                                         ! DUST (Field 2006)(Asia)
         NVI0  = QNIN*4.337E-1*EXP(-2.E6/((LOG(SRI))**2.*TK1D**3.))
         NNDvi = MIN(QNIN,NNUMAX,MAX(0.,NVI0*DT))
      ELSEIF (IDEPNU.EQ.7) THEN
         NVI0  = MAX(0.,5.94E-5*((TK0C-TK1D)**3.33)*((1.E6/1.E6)**     &! set naer05=1e6/kg (derived to cm-3 at STP below)
                 (0.0264*(TK0C-TK1D)+3.3E-3))*1.E3)                     ! units of #/kg
         NNDvi = MIN(QNIN,NNUMAX,MAX(0.,NVI0/RHO-NI1D))
      ENDIF
      QNDvi = NNDvi*MI0
      QNIN  = MAX(0.,QNIN-NNDvi)
      QV1D  = MAX(0.,QV1D-QNDvi)
      QI1D  = MAX(0.,QI1D+QNDvi)
      NI1D  = MAX(0.,NI1D+NNDvi)
      CPM   = CP*(1.+0.887*QV1D)
      XXLS  = 3.15E6-2370.*TK1D+0.3337E6
      TK1D  = TK1D+XXLS*QNDvi/CPM

      END SUBROUTINE ICENU
!======================================================================
!
!======================================================================
      SUBROUTINE SEDI_FALL(TK1D,P1D,QV1D,QC1D,QR1D,QI1D,QS1D,QG1D,NC1D,&
                 NR1D,NI1D,NS1D,NG1D,VTQC,VTQR,VTQI,VTQS,VTQG,VTNC,    &
                 VTNR,VTNI,VTNS,VTNG,RHO,SMLF,GMLF)
!======================================================================
      IMPLICIT NONE
      REAL :: TK1D,P1D,QV1D,QC1D,QR1D,QI1D,QS1D,QG1D,NC1D,NR1D,NI1D,   &
              NS1D,NG1D,VTQC,VTQR,VTQI,VTQS,VTQG,VTNC,VTNR,VTNI,VTNS,  &
              VTNG,QRHO,RHOAJ,RHO,LAMC,LAMR,LAMI,LAMS,LAMG,RHOI,RHOS,  &
              RHOG,AFAC,AFAR,AFAI,AFAS,AFAG,ADAGR,ZETA3,AVC,BVC,AVR,   &
              BVR,AMI,AMS,AMG,AVI,BVI,AVS,BVS,AVG,BVG,ZETA,FSQC,FSQR,  &
              FSQI,FSQS,FSQG,FSNC,FSNR,FSNI,FSNS,FSNG,MVDC,MVDR,MVDI,  &
              MVDS,MVDG,BEST,RASPR,SASPR,GASPR,LLMC,LAC2,LAC5,LLMR,    &
              LAR2,LAR5,LLMI,LLMI2,LLMI3,LAI2,LAI5,RHOX,VISHP,LLMS,    &
              LLMS2,LLMS3,LAS2,LAS5,LLMG,LLMG2,LLMG3,LAG2,LAG5,AFAZ,   &
              SMLF,GMLF,DSMM,DGMM

      QRHO  = SQRT(RHO)
      RHOAJ = (RHOSU/RHO)**0.54
      IF (QC1D.GE.QSMALL) THEN
         CALL SOLVE_AFAC(TK1D,QC1D,NC1D,LAMC,MVDC,AFAC,AVC,BVC)
         IF (LIQ_VTC.EQ.0) THEN
            FSQC = EXP(LOG_GAMMA(BVC+AFAC+4.)-LOG_GAMMA(AFAC+4.)-BVC*  &
                   LOG(LAMC))
            FSNC = EXP(LOG_GAMMA(BVC+AFAC+1.)-LOG_GAMMA(AFAC+1.)-      &
                   BVC*LOG(LAMC))
            VTQC = RHOAJ*FSQC*AVC
            VTNC = RHOAJ*FSNC*AVC
         ELSEIF (LIQ_VTC.EQ.1) THEN
            LLMC = LOG(LAMC)
            LAC2 = LOG(AFAC+2.)
            LAC5 = LOG(AFAC+5.)
            VTQC = EXP(TVC1+TVC2*LAC5+TVC3*LLMC+TVC4*LAC5**2.+TVC5*    &
                   LLMC**2.+TVC6*LAC5*LLMC+TVC7*LAC5**3.+TVC8*LLMC**3.+&
                   TVC9*LAC5*LLMC**2.+TVC10*LAC5**2.*LLMC)/1.E8
            VTNC = EXP(TVC1+TVC2*LAC2+TVC3*LLMC+TVC4*LAC2**2.+TVC5*    &
                   LLMC**2.+TVC6*LAC2*LLMC+TVC7*LAC2**3.+TVC8*LLMC**3.+&
                   TVC9*LAC2*LLMC**2.+TVC10*LAC2**2.*LLMC)/1.E8
         ENDIF
         VTQC = MIN(VTQC,VTCMAX)
         VTNC = MIN(VTNC,VTCMAX)
      ENDIF
      IF (QR1D.GE.QSMALL) THEN
         CALL SOLVE_AFAR(TK1D,QC1D,QR1D,NR1D,LAMR,MVDR,AFAR,RASPR,AVR, &
              BVR)
         IF (LIQ_VTR.EQ.0) THEN
            FSQR = EXP(LOG_GAMMA(BVR+AFAR+4.)-LOG_GAMMA(AFAR+4.)-BVR*  &
                   LOG(LAMR))
            FSNR = EXP(LOG_GAMMA(BVR+AFAR+1.)-LOG_GAMMA(AFAR+1.)-BVR*  &
                   LOG(LAMR))
            VTQR = RHOAJ*FSQR*AVR
            VTNR = RHOAJ*FSNR*AVR
         ELSEIF (LIQ_VTR.EQ.1) THEN
            LLMR = LOG(LAMR)
            LAR2 = LOG(AFAR+2.)
            LAR5 = LOG(AFAR+5.)
            VTQR = EXP(TVR1+TVR2*LAR5+TVR3*LLMR+TVR4*LAR5**2.+TVR5*    &
                   LLMR**2.+TVR6*LAR5*LLMR+TVR7*LAR5**3.+TVR8*LLMR**3.+&
                   TVR9*LAR5*LLMR**2.+TVR10*LAR5**2.*LLMR)*RHOAJ/1.E3
            VTNR = EXP(TVR1+TVR2*LAR2+TVR3*LLMR+TVR4*LAR2**2.+TVR5*    &
                   LLMR**2.+TVR6*LAR2*LLMR+TVR7*LAR2**3.+TVR8*LLMR**3.+&
                   TVR9*LAR2*LLMR**2.+TVR10*LAR2**2.*LLMR)*RHOAJ/1.E3
         ENDIF
         VTQR = MIN(VTQR,VTRMAX)
         VTNR = MIN(VTNR,VTRMAX)
      ENDIF
      IF (QI1D.GE.QSMALL) THEN
         CALL SOLVE_AFAI(TK1D,P1D,QV1D,QC1D,QI1D,NI1D,ADAGR,ZETA,LAMI, &
              AFAI,MVDI,RHOI,AMI,AVI,BVI)
         IF (ICE_VTI.EQ.0) THEN
            FSQI = EXP(LOG_GAMMA(BVI+AFAI+4.)-LOG_GAMMA(AFAI+4.)-BVI*  &
                   LOG(LAMI))
            FSNI = EXP(LOG_GAMMA(BVI+AFAI+1.)-LOG_GAMMA(AFAI+1.)-BVI*  &
                   LOG(LAMI))
            VTQI = MIN(RHOAJ*FSQI*AVI,VTIMAX)
            VTNI = MIN(RHOAJ*FSNI*AVI,VTIMAX)
         ELSEIF (ICE_VTI.EQ.1) THEN
            VISHP = 1.
            IF (ZETA.GT.0.) THEN
               VISHP = DI0**(ZETA/2.)
               AFAZ = AFAI+ZETA
            ELSEIF (ZETA.LT.0.) THEN
               VISHP = DI0**(-ZETA)
               AFAZ = AFAI-ZETA/2.
            ELSE
               VISHP = 1.
               AFAZ = AFAI
            ENDIF
            LLMI  = LOG(LAMI)
            LLMI2 = LLMI*LLMI
            LLMI3 = LLMI2*LLMI
            LAI2  = LOG(AFAZ+2.)
            LAI5  = LOG(AFAZ+5.)
            RHOX  = VYRHO(MVDI,RHOI)
            VTQI  = EXP(TVI1+TVI2*LAI5+TVI3*LLMI+TVI4*LAI5**2.+TVI5*   &
                    LLMI2+TVI6*LAI5*LLMI+TVI7*LAI5**3.+TVI8*LLMI3+     &
                    TVI9*LAI5*LLMI2+TVI10*LAI5**2.*LLMI)/1.E5*RHOX*    &
                    VISHP*RHOAJ
            VTNI  = EXP(TVI1+TVI2*LAI2+TVI3*LLMI+TVI4*LAI2**2.+TVI5*   &
                    LLMI2+TVI6*LAI2*LLMI+TVI7*LAI2**3.+TVI8*LLMI3+     &
                    TVI9*LAI2*LLMI2+TVI10*LAI2**2.*LLMI)/1.E5*RHOX*    &
                    VISHP*RHOAJ
         ENDIF
      ENDIF
      IF (QS1D.GE.QSMALL) THEN
         CALL SOLVE_AFAS(TK1D,QS1D,QC1D,NS1D,AFAS,LAMS,MVDS,RHOS,SASPR,&
              AMS,AVS,BVS)
         IF (SMLF.GE.MLIMIT) THEN
            LLMS  = LOG(LAMS)
            LAS5  = LOG(AFAS+5.)
            SASPR = SMLF*(MIN(1.,(-32.232528-16.735319*LAS5+14.830389* &
                    LLMS-1.8312883*LAS5**2.-1.6827601*LLMS**2.+        &
                    3.6556064*LAS5*LLMS-7.4526392E-2*LAS5**3.+         &
                    6.2393644E-2*LLMS**3.-0.20029785*LAS5*LLMS**2.+    &
                    0.20699596*LAS5**2.*LLMS)/10.))+(1.-SMLF)*SASPR
            RHOS = MIN(RHOIMAX,MAX(RHOIMIN,SMLF*RHOW+(1.-SMLF)*RHOS))
         ENDIF
         IF (ICE_VTS.EQ.0) THEN
            FSQS = EXP(LOG_GAMMA(BVS+AFAS+4.)-LOG_GAMMA(AFAS+4.)-BVS*  &
                   LOG(LAMS))
            FSNS = EXP(LOG_GAMMA(BVS+AFAS+1.)-LOG_GAMMA(AFAS+1.)-BVS*  &
                   LOG(LAMS))
            VTQS = MIN(RHOAJ*FSQS*AVS,VTSMAX)
            VTNS = MIN(RHOAJ*FSNS*AVS,VTSMAX)
         ELSEIF (ICE_VTS.EQ.1) THEN
            LLMS  = LOG(LAMS)
            LLMS2 = LLMS*LLMS
            LLMS3 = LLMS2*LLMS
            LAS2  = LOG(AFAS+2.)
            LAS5  = LOG(AFAS+5.)
            RHOX  = VYRHO(MVDS,RHOS)
            VTQS  = EXP(TVS1+TVS2*LAS5+TVS3*LLMS+TVS4*LAS5**2.+TVS5*   &
                    LLMS2+TVS6*LAS5*LLMS+TVS7*LAS5**3.+TVS8*LLMS3+     &
                    TVS9*LAS5*LLMS2+TVS10*LAS5**2.*LLMS)/1.E3*RHOX*    &
                    SASPR**THRD*RHOAJ
            VTNS  = EXP(TVS1+TVS2*LAS2+TVS3*LLMS+TVS4*LAS2**2.+TVS5*   &
                    LLMS2+TVS6*LAS2*LLMS+TVS7*LAS2**3.+TVS8*LLMS3+     &
                    TVS9*LAS2*LLMS2+TVS10*LAS2**2.*LLMS)/1.E3*RHOX*    &
                    SASPR**THRD*RHOAJ
         ENDIF
      ENDIF
      IF (QG1D.GE.QSMALL) THEN
         CALL SOLVE_AFAG(TK1D,RHO,QG1D,QC1D,QR1D,QI1D,NG1D,LAMG,AFAG,  &
              MVDG,RHOG,GASPR,AMG,AVG,BVG)
         IF (GMLF.GE.MLIMIT) THEN
            LLMG  = LOG(LAMG)
            LAG5  = LOG(AFAG+5.)
            GASPR = GMLF*(MIN(1.,(-32.232528-16.735319*LAG5+14.830389* &
                    LLMG-1.8312883*LAG5**2.-1.6827601*LLMG**2.+        &
                    3.6556064*LAG5*LLMG-7.4526392E-2*LAG5**3.+         &
                    6.2393644E-2*LLMG**3.-0.20029785*LAG5*LLMG**2.+    &
                    0.20699596*LAG5**2.*LLMG)/10.))+(1.-GMLF)*GASPR
            RHOG = MIN(RHOIMAX,MAX(RHOIMIN,GMLF*RHOW+(1.-GMLF)*RHOG))
         ENDIF
         IF (ICE_VTG.EQ.0) THEN
            FSQG = EXP(LOG_GAMMA(BVG+AFAG+4.)-LOG_GAMMA(AFAG+4.)-BVG*  &
                   LOG(LAMG))
            FSNG = EXP(LOG_GAMMA(BVG+AFAG+1.)-LOG_GAMMA(AFAG+1.)-BVG*  &
                   LOG(LAMG))
            VTQG = MIN(RHOAJ*FSQG*AVG,VTGMAX)
            VTNG = MIN(RHOAJ*FSNG*AVG,VTGMAX)
         ELSEIF (ICE_VTG.EQ.1) THEN
            LLMG  = LOG(LAMG)
            LLMG2 = LLMG*LLMG
            LLMG3 = LLMG2*LLMG
            LAG2  = LOG(AFAG+2.)
            LAG5  = LOG(AFAG+5.)
            RHOX  = VYRHO(MVDG,RHOG)
            VTQG  = EXP(TVS1+TVS2*LAG5+TVS3*LLMG+TVS4*LAG5**2.+TVS5*   &
                    LLMG2+TVS6*LAG5*LLMG+TVS7*LAG5**3.+TVS8*LLMG3+TVS9*&
                    LAG5*LLMG2+TVS10*LAG5**2.*LLMG)/1.E3*RHOX*RHOAJ
            VTNG  = EXP(TVS1+TVS2*LAG2+TVS3*LLMG+TVS4*LAG2**2.+TVS5*   &
                    LLMG2+TVS6*LAG2*LLMG+TVS7*LAG2**3.+TVS8*LLMG3+TVS9*&
                    LAG2*LLMG2+TVS10*LAG2**2.*LLMG)/1.E3*RHOX*RHOAJ
            IF (GASPR.LT.1.) THEN
               VTQG = VTQG*GASPR**THRD
               VTNG = VTNG*GASPR**THRD
            ELSEIF (GASPR.GT.1.) THEN
               VTQG = VTQG*GASPR**SIXD
               VTNG = VTNG*GASPR**SIXD
            ELSE
               VTQG = VTQG
               VTNG = VTNG
            ENDIF
         ENDIF
      ENDIF

      END SUBROUTINE SEDI_FALL
!======================================================================
!
!======================================================================
      SUBROUTINE PTFLUX(Q1D,VT1D,RHO,DZ,NK,DT,DTMN,PRT1D) 
!======================================================================
      IMPLICIT NONE
      INTEGER :: K,NK,NS,NSTEP
      INTEGER, PARAMETER :: MAXSTP = 1000
      REAL :: PRT1D,                                                   &! input/output ; accumulative prcipitation (cm)
              DT,                                                      &! time step for integration (=2*DTMN*60. for leapfrog scheme)
              DTMN                                                      ! time step (min)
      REAL, DIMENSION(NK) :: Q1D,VT1D,RHO,DZ,DQDT
!------------ FOR TENDENCIES DUE TO PRECIPITATION/SEDIMENTATION -------
      NSTEP = 1
      DO K = 1,NK
         NSTEP = MAX(NSTEP,INT(VT1D(K)*DT/DZ(K)+1.))
         IF (NSTEP.GT.80) THEN
             PRINT *,'IN PREFLUX',K,NSTEP,DT,VT1D(K),DZ(K),Q1D(K),RHO(K)
         ENDIF
      ENDDO
      IF (NSTEP.GT.MAXSTP) THEN
         PRINT *,'NSTEP FOR PRECIP. IS: ',NSTEP,VT1D,DT,DZ
         STOP 
      ENDIF 
      DO NS = 1,NSTEP
         DQDT(1) = -VT1D(1)*Q1D(1)/DZ(1)
         DO K = 2,NK,1
            DQDT(K) = (VT1D(K-1)*RHO(K-1)*Q1D(K-1)-                    &
                       VT1D(K)*RHO(K)*Q1D(K))/(DZ(K)*RHO(K))
         ENDDO
         PRT1D = PRT1D+VT1D(NK)*RHO(NK)*Q1D(NK)*DTMN*60./REAL(NSTEP)    ! accumulate precipitation [kg/m^2]; 1 kg/m^2 = 1 mm 
         DO K = 1,NK
            Q1D(K) = Q1D(K)+DQDT(K)*DT/REAL(NSTEP) 
         ENDDO 
      ENDDO 

      END SUBROUTINE PTFLUX
!======================================================================
!
!======================================================================
      SUBROUTINE FLFLUX(Q1D,VT1D,RHO,DZ,NK,DT) 
!======================================================================
      IMPLICIT NONE
      INTEGER :: NSTEP,K,NK,NS
      INTEGER, PARAMETER :: MAXSTP = 1000
      REAL :: DT
      REAL, DIMENSION(NK) :: Q1D,VT1D,RHO,DZ,DQDT
!-- INPUT DATA FOR STEPSIZE RK4 CALCULATION FOR TENDENCIES DUE TO PRECIPITATION/SEDIMENTATION
      NSTEP = 1
      DO K = 1,NK
         NSTEP = MAX(NSTEP,INT(VT1D(K)*DT/DZ(K)+1.))
      ENDDO
      DO NS = 1,NSTEP
         DQDT(1) = -VT1D(1)*Q1D(1)/DZ(1) 
         DO K = 2,NK,1
            DQDT(K) = (VT1D(K-1)*RHO(K-1)*Q1D(K-1)-                    &
                       VT1D(K)*RHO(K)*Q1D(K))/(DZ(K)*RHO(K))
         ENDDO
         DO K = 1,NK
            Q1D(K) = Q1D(K)+DQDT(K)*DT/REAL(NSTEP) 
         ENDDO 
      ENDDO 

      END SUBROUTINE FLFLUX
!======================================================================
!
!======================================================================
      SUBROUTINE MAIN_MP(DT,TK1D,QV1D,P1D,RHO,QC1D,QR1D,QI1D,QS1D,QG1D,&
                 NC1D,NR1D,NI1D,NS1D,NG1D,SMLF,GMLF,W1D)
!=======================================================================
      IMPLICIT NONE
      INTEGER :: I,WBIN,CBIN,PBIN,TBIN,DBIN,HID,IHAIL
      REAL :: TK1D,QV1D,P1D,W1D,TC1D,DT,AFAC,AFAR,AFAI,AFAS,AFAG,ADAGR,&
              QC1D,QR1D,QI1D,QS1D,QG1D,NC1D,NR1D,NI1D,NS1D,NG1D,RHOI,  &
              RHOS,RHOG,RHOIS,iRHOI,iRHOS,iRHOG,ZETA,ZETA2,ZETA3,ZETA4,&
              ZETA5,VENIC,VENIA,INHGR,GVHAB,iAMI,iAMG,DI0Z1,DI0Z2,     &
              DI0Z3,DI0Z4,IPG,IPH,IPF,RAT1,RAT2,DNRI,SASPR,GASPR,DNIRM,&
              DNIAG,DNSAG,DNSAC,DNSRM,AMI,AMG,AMS,RHO,iRHO,AVC,BVC,AVR,&
              BVR,AVI,BVI,AVS,BVS,AVG,BVG,CAPS,SASR1,SASR2,SASR3,SASR4,&
              SASP1,SASP2,SASP3,CAPG,GASR1,GASR2,GASR3,GASR4,MVDM,     &
              RASPR,SMLV,GMLV,MVG0,DSLL,ISEPS,ISEPL,LIM1,LIM2,LIM22,   &
              LIM23,LIMV,DSLSV,DTHL,DTHI,VENT,IJNMF,RFZQ,RFZN
      REAL :: QHOci,QHOrg,QNMci,QNMrg,QNCci,QFZci,QFZrg,QMLir,QMLic,   &
              QMLsr,QMLgr,QIMii,QIMcsi,QIMrsi,QIMcgi,QIMrgi,QBKrc,     &
              QINig,QINsg,QCNcr,QCNis,QRMci,QRMcs,QRMcg,QRMri,QRMrs,   &
              QRMrg,QCLcr,QCLir,QCLis,QCLsr,QCLgr,QCLirg,QCLsrg,QCLgrg,&
              QCLss,QACcv,QACrc,QVDvc,QVDvr,QVDvi,QVDvs,QVDvg,QSBiv,   &
              QSBsv,QSBgv,QEVcv,QEVrv,QEVsv,QEVgv,NHOci,NHOrg,NNMci,   &
              NNMrg,NNCci,NFZci,NFZrg,NMLir,NMLic,NMLsr,NMLgr,NIMii,   &
              NIMcsi,NIMrsi,NIMcgi,NIMrgi,NBKrc,NBKrr,NINig,NINsg,     &
              NCNcr,NiCNis,NRMci,NRMcs,NRMcg,NRMri,NRMrs,NRMrg,NCLcc,  &
              NCLcr,NCLrr,NCLir,NCLis,NCLsr,NCLss,NCLgr,NCLirg,NCLsrg, &
              NCLgrg,NsCNis,NACcv,NACrc,NSBiv,NSBsv,NSBgv,QCLig,QCLsg, &
              NCLig,NCLsg,NEVcv,NEVrv
      REAL :: QVSOUR,QVSINK,QCSOUR,QCSINK,QRSOUR,QRSINK,QISOUR,QISINK, &
              QSSOUR,QSSINK,QGSOUR,QGSINK,NCSOUR,NCSINK,NRSOUR,NRSINK, &
              NISOUR,NISINK,NSSOUR,NSSINK,NGSOUR,NGSINK
      REAL :: RHOAJ,CPM,SSRI,XXLS,XXLF,XXLV,QVSI,ESI,ABI,iDT,SSRW,KAP, &
              DV,MUA,SCN,ESW,SFCTNW,SFCTNV,QVSW,FF,RFZ,VOLMC,IJHOF,    &
              RATIO,WNRE,CNRE,PNRE,MVRC,MVRR,MVDC,MVDR,MVDI,MVDS,MVDG, &
              DSMM,DGMM,LMVRC,LMVRR,TMP1,GUC,GUR,RMcsq,RMrsq,RMcgq,    &
              RMrgq,RMcsa,RMrsa,RMcga,RMrga,VTQ0,VTQC,VTQR,VTQI,VTQS,  &
              VTQG,VTN0,VTNC,VTNR,VTNI,VTNS,VTNG,VTQIC,VTQSC,VTQGC,    &
              VTQRI,VTQRS,VTQRG,VTQIS,VTQIG,VTQSG,VTNIC,VTNSC,VTNGC,   &
              VTNRI,VTNRS,VTNRG,VTNIS,VTNIG,VTNSG,ECS0,ECS3,ECG0,ECG3, &
              ERS0,ERS3,ERG0,ERG3,EIR0,EIR3,ESR0,ESR3,EGR0,EGR3,EIS0,  &
              EIS3,EIG0,EIG3,ESG0,ESG3,ECI0,ECI3,ERI0,ERI3,EII0,EII3,  &
              ESS0,ESS3,ECS,ERS,ECG,ERG,SMLTQ,GMLTQ,SMLTA,GMLTA,VENQS, &
              VENQG,STOKE,MVDX,ABW,FSQC,FSNC,FSQR,FSNR,FSQI,FSNI,FSQS, &
              FSNS,FSQG,FSNG,LLMC,LAC2,LAC5,LLMR,LAR2,LAR5,LAI2,LAI5,  &
              RHOX,VISHP,LAS2,LAS22,LAS5,LAG2,LAG22,LAG5,AFAZ,LMDS,    &
              LMDS2,LROS,LROS2,LMDG,LMDG2,LROG,LROG2,DQSLDT,DQSIDT,    &
              ABW1,ABI1,BRTMP,VENQR,TAU,TAUC,TAUR,TAUI,TAUS,TAUG,ATEM
      REAL :: HIdqv,HSdqv,HGdqv,HCwqv,HRwqv,HSwqv,HGwqv,HSwcd,HGwcd,   &
              HSwrm,HGwrm,VDMAX,SBMAX,EVMAX,SUMDEP,SUMSUB,SUMEVP,      &
              SUMCND,MLMAX,ESW0,ESI0,QVSW0,QVSI0,XXLF0,BEST,VENQI,     &
              QTMP0,QTMP1,QTMP2,QTMP3,QTMP4,QTMP5,QTMP6,QTMP7,QTMP8,   &
              QTMP9,NCLS2,BTMP,BSTMP,BGTMP,H2Z,H4Z,QRMC1,NRMC1,QRMR1,  &
              NRMR1,QFZC1,NFZC1,QFZR1,NFZR1,QCLI1,NCLI1,QCNI1,NCNI1,   &
              QCLG1,NCLG1,QCLS1,NCLS1,SMLR,GMLR,SMLF,GMLF,AVWSG,BVWSG, &
              CAPWS,CAPWG,WSAPR,WGAPR,RHOWS,RHOWG,VENWS,VENWG,SSRW0,   &
              SSRI0,LLMI,LLMI2,LLMI3,LLMS,LLMS2,LLMS3,LLMG,LLMG2,LLMG3,&
              RHOIW,RHOSW,RHOGW,KINV,BEST0,C1X2,VTB1,VTA1,RVLR0,RVLR3, &
              RVLI0,RVLI3,RVLS0,RVLS3,RVLG0,RVLG3
      REAL :: LAMC,LAMR,LAMS,LAMG,LAMI,GC1,GC2,GC3,GC4,GC5,GC6,GC7,GR1,&
              GR2,GR3,GR4,GR5,GR6,GR7,GI1,GI2,GI2G1,GI2G2,GI2H1,GI3,   &
              GI3H1,GIF1,GIF2,GIG1,GIG2,GIH1,GIH2,GI4,GI5,GI2H4,GI2H3, &
              GIF3,GI2G3,GI3G1,GIG3,GI2G4,GI6,GIF4,GIG4,GIH4,GIZ1,     &
              GIZF1,GIZG1,GIZH1,GIH3,GS1,GS2,GS3,GS4,GS5,GS6,GG1,GG2,  &
              GG3,GG4,GG5,GG6
      REAL, PARAMETER :: AQ1 = 6.6793E+0, BQ1 = 1.0090E+0, CQ1 = 1.4095E+0
      REAL, PARAMETER :: AQ2 = 9.9912E+0, BQ2 = -4.7678E-1, CQ2 = -3.1388E-2
      REAL, PARAMETER :: AN3 = -4.3561E0, BN3 = 1.9934E0,  CN3 = 1.6465E-2
      REAL, PARAMETER :: AN4 = -4.0731E1, BN4 = 5.3720E5,  CN4 = -2.0139E-5
      REAL, PARAMETER :: AQ4 = -2.1370E1, BQ4 = 1.9899E9,  DN9 = 1.6328E+0
      REAL, PARAMETER :: AN5 = 1.5519E1,  BN5 = 3.1491E-0, CN5 = 4.3989E-1
      REAL, PARAMETER :: AQ5 = 2.0090E1,  BQ5 = 2.9626E0, CQ5 = 3.2358E0
      REAL, PARAMETER :: AN6 = -1.8239E1, BN6 = 2.2956E0,  CN6 = -2.3261E-4
      REAL, PARAMETER :: AN7 = -1.7431E2, BN7 = 2.6031E5,  CN7 = -9.3613E7
      REAL, PARAMETER :: AN8 = -1.6185E2, BN8 = 2.2786E5,  CN8 = -7.6988E7
      REAL, PARAMETER :: AQ8 = -2.3531E1, BQ8 = 9.8271E-1, CQ8 = -1.3202E-1
      REAL, PARAMETER :: AN9 = -1.0593E+0, BN9 = 8.9774E-1, CN9 = -2.8403E-1
      REAL, PARAMETER :: AN10 = 8.2841E+0, BN10 = 9.7219E-1, CN10 = -5.0808E-1
      REAL, PARAMETER :: XISP = 0.25
      REAL :: ZC1,ZC2,ZC3,ZC4,ZP1,ZP2,ZP3,ZP4,ZZ1,ZZ2,ZZ3
      DATA ZC1,ZC2,ZC3,ZC4/0.69509913,-0.46685819,0.30490087,1.62148100/
      DATA ZP1,ZP2,ZP3,ZP4/0.36793126,1.82782890,0.63206874,-1.00164090/
      DATA ZZ1,ZZ2,ZZ3/0.91767336,-0.37871035,1.03995157/

      MVDC   = 0.; MVDR   = 0.; MVDI   = 0.; MVDS   = 0.; MVDG   = 0.
      MVRC   = 0.; MVRR   = 0.; SMLF   = 0.; GMLF   = 0.; FF     = 0.
      HIdqv  = 0.; HSdqv  = 0.; HGdqv  = 0.; HCwqv  = 0.; HRwqv  = 0.
      HSwqv  = 0.; HGwqv  = 0.; QHOci  = 0.; QHOrg  = 0.; QNMci  = 0.
      QNMrg  = 0.; QNCci  = 0.; QFZci  = 0.; QFZrg  = 0.; QMLir  = 0.
      QMLic  = 0.; QMLsr  = 0.; QMLgr  = 0.; QIMii  = 0.; QIMcsi = 0.
      QIMrsi = 0.; QIMcgi = 0.; QIMrgi = 0.; QBKrc  = 0.; QINig  = 0.
      QINsg  = 0.; QCNcr  = 0.; QCNis  = 0.; QRMci  = 0.; QRMcs  = 0.
      QRMcg  = 0.; QRMri  = 0.; QRMrs  = 0.; QRMrg  = 0.; QCLcr  = 0.
      QCLir  = 0.; QCLis  = 0.; QCLsr  = 0.; QCLgr  = 0.; QCLirg = 0.
      QCLsrg = 0.; QCLgrg = 0.; QACcv  = 0.; QACrc  = 0.; QVDvc  = 0.
      QVDvr  = 0.; QVDvi  = 0.; QVDvs  = 0.; QVDvg  = 0.; QSBiv  = 0.
      QSBsv  = 0.; QSBgv  = 0.; QEVcv  = 0.; QEVrv  = 0.; QEVsv  = 0.
      QEVgv  = 0.; NHOci  = 0.; NHOrg  = 0.; NNMci  = 0.; NNMrg  = 0.
      NNCci  = 0.; NFZci  = 0.; NFZrg  = 0.; NMLir  = 0.; NMLic  = 0.
      NMLsr  = 0.; NMLgr  = 0.; NIMii  = 0.; NIMcsi = 0.; NIMrsi = 0.
      NIMcgi = 0.; NIMrgi = 0.; NBKrc  = 0.; NBKrr  = 0.; NINig  = 0.
      NINsg  = 0.; NCNcr  = 0.; NiCNis = 0.; NRMci  = 0.; NEVrv  = 0.
      NRMcs  = 0.; NRMcg  = 0.; NRMri  = 0.; NRMrs  = 0.; NRMrg  = 0.
      NCLcc  = 0.; NCLcr  = 0.; NCLrr  = 0.; NCLir  = 0.; NCLis  = 0.
      NCLsr  = 0.; NCLss  = 0.; NCLgr  = 0.; NCLirg = 0.; NCLsrg = 0.
      NCLgrg = 0.; NsCNis = 0.; NACcv  = 0.; NACrc  = 0.; NSBiv  = 0.
      NSBsv  = 0.; NSBgv  = 0.; QCLig  = 0.; QCLsg  = 0.; NCLig  = 0.
      NCLsg  = 0.; NEVcv  = 0.; DTHL   = 0.; DTHI   = 0.; VENT   = 0.
      ECI0 = 0.; ECI3 = 0.; ECS0 = 0.; ECS3 = 0.; ECG0 = 0.; ECG3 = 0.
      ERI0 = 0.; ERI3 = 0.; ERS0 = 0.; ERS3 = 0.; ERG0 = 0.; ERG3 = 0.
      ERI0 = 0.; ERI3 = 0.; EIR0 = 0.; EIR3 = 0.; ESR0 = 0.; ESR3 = 0.
      EGR0 = 0.; EGR3 = 0.; EII0 = 0.; EII3 = 0.; EIS0 = 0.; EIS3 = 0.
      ESS0 = 0.; ESS3 = 0.; EIG0 = 0.; EIG3 = 0.; ESG0 = 0.; ESG3 = 0.

      IF (SSAT.EQ.2) THEN
         TAU  = 1.E20; TAUC = 1.E20; TAUR = 1.E20; TAUI = 1.E20
         TAUS = 1.E20; TAUG = 1.E20
      ENDIF

      iDT    = 1./DT
      iRHO   = 1./RHO
      TC1D   = TK1D-TK0C
      IF (TK1D.GE.265..AND.TK1D.LT.268.) FF = (TC1D+8.)*THRD
      IF (TK1D.GE.268..AND.TK1D.LE.270.) FF = (-3.-TC1D)*0.5
      RHOAJ  = (RHOSU/RHO)**0.54
      CPM    = CP*(1.+0.887*QV1D)
      ESW    = MIN(0.99*P1D,POLYSVP(TK1D,0))
      ESI    = MIN(0.99*P1D,POLYSVP(TK1D,1))
      IF (ESI.GT.ESW) ESI = ESW
      QVSW   = 0.622*ESW/(P1D-ESW)
      QVSI   = 0.622*ESI/(P1D-ESI)
      SSRW   = QV1D/QVSW-1.
      SSRI   = QV1D/QVSI-1.
      ESW0   = MIN(0.99*P1D,POLYSVP(TK0C,0))
      QVSW0  = 0.622*ESW0/(P1D-ESW0)
      ESI0   = MIN(0.99*P1D,POLYSVP(TK0C,1))
      QVSI0  = 0.622*ESI0/(P1D-ESI0)
      XXLV   = 3.1484E6-2370.*TK1D
      XXLS   = 3.15E6-2370.*TK1D+0.3337E6
      XXLF   = 2836310.8-(3.1484E6-2370.*TK1D)
      XXLF0  = 2836310.8-(3.1484E6-2370.*TK0C)
      SSRW0  = QV1D/QVSW0-1.
      SSRI0  = QV1D/QVSI0-1.
      DV     = 2.11E-5*(TK1D/TK0C)**1.94*(101325./P1D)
      KAP    = 2.3971E-2+0.0078E-2*TC1D
      MUA    = 1.72E-5*(393./(TK1D+120.))*(TK1D/TK0C)**1.5
      SCN    = (MUA/(RHO*DV))**THRD
      ABW    = TK1D*RV/ESW/DV+XXLV*(XXLV/TK1D/RV-1.)/TK1D/KAP
      ABI    = TK1D*RV/ESI/DV+XXLS*(XXLS/TK1D/RV-1.)/TK1D/KAP
      DQSLDT = XXLV*QVSW/(RV*TK1D**2.)
      DQSIDT = XXLS*QVSI/(RV*TK1D**2.)
      ABW1   = 1.+DQSLDT*XXLV/CPM
      ABI1   = 1.+DQSIDT*XXLS/CPM
      SFCTNV = (0.095*TC1D+104.6)*1.E-3                                 ! SURFACE TENSION OVER ICE/VAPOR
      SFCTNW = (28.+0.25*TC1D)*1.E-3
      HID    = MAX(MIN(NINT(ABS(TC1D)/0.25),120),0)
      INHGR  = ITBLE(HID)
      GVHAB  = (INHGR-1.)/(INHGR+2.)+1.

      IF (QC1D.GE.QSMALL) THEN
         CALL SOLVE_AFAC(TK1D,QC1D,NC1D,LAMC,MVDC,AFAC,AVC,BVC)
         LLMC  = LOG(LAMC)
         GC1   = LOG_GAMMA(AFAC+1.)
         GC2   = EXP(LOG_GAMMA(AFAC+2.)-GC1-LLMC)
         GC3   = EXP(LOG_GAMMA(AFAC+3.)-GC1-2.*LLMC)
         GC4   = EXP(LOG_GAMMA(AFAC+4.)-GC1-3.*LLMC)
         GC5   = EXP(LOG_GAMMA(AFAC+5.)-GC1-4.*LLMC)
         GC6   = EXP(LOG_GAMMA(AFAC+6.)-GC1-5.*LLMC)
         GC7   = EXP(LOG_GAMMA(AFAC+7.)-GC1-6.*LLMC)
         MVRC  = MIN(MAX((QC1D/NC1D/C4PI3W)**THRD,RCMIN),RCMAX)
         LMVRC = LOG(MVRC)
         GUC   = EXP(EXP(AFU+BFU*LMVRC**3.+CFU*SQRT(RHO)**3.))
         IF (LIQ_VTC.EQ.0) THEN
            FSQC = EXP(LOG_GAMMA(BVC+AFAC+4.)-LOG_GAMMA(AFAC+4.)-BVC*  &
                   LLMC)
            FSNC = EXP(LOG_GAMMA(BVC+AFAC+1.)-GC1-BVC*LLMC)
            VTQC = RHOAJ*FSQC*AVC
            VTNC = RHOAJ*FSNC*AVC
         ELSEIF (LIQ_VTC.EQ.1) THEN
            LAC2 = LOG(AFAC+2.)
            LAC5 = LOG(AFAC+5.)
            VTQC = EXP(TVC1+TVC2*LAC5+TVC3*LLMC+TVC4*LAC5**2.+TVC5*    &
                   LLMC**2.+TVC6*LAC5*LLMC+TVC7*LAC5**3.+TVC8*LLMC**3.+&
                   TVC9*LAC5*LLMC**2.+TVC10*LAC5**2.*LLMC)/1.E8
            VTNC = EXP(TVC1+TVC2*LAC2+TVC3*LLMC+TVC4*LAC2**2.+TVC5*    &
                   LLMC**2.+TVC6*LAC2*LLMC+TVC7*LAC2**3.+TVC8*LLMC**3.+&
                   TVC9*LAC2*LLMC**2.+TVC10*LAC2**2.*LLMC)/1.E8
         ENDIF
         VTQC  = MIN(VTQC,VTCMAX)
         VTNC  = MIN(VTNC,VTCMAX)
         QRMC1 = PI*PI*RHOW*NC1D/24.
         NRMC1 = PI*NC1D/4.
         HCwqv = SSRW0*EXP(AQ1+BQ1*LOG(NC1D)+CQ1*LMVRC)/ABW
         IF (SSAT.EQ.2) THEN
            TAUC = 1./(2.*PI*DV*NC1D*EXP(LOG_GAMMA(AFAC+2.)-GC1-LLMC))
         ENDIF
      ENDIF
      IF (QR1D.GE.QSMALL) THEN
         CALL SOLVE_AFAR(TK1D,QC1D,QR1D,NR1D,LAMR,MVDR,AFAR,RASPR,AVR, &
              BVR)
         LLMR  = LOG(LAMR)
         GR1   = LOG_GAMMA(AFAR+1.)
         GR2   = EXP(LOG_GAMMA(AFAR+2.)-GR1-LLMR)
         GR3   = EXP(LOG_GAMMA(AFAR+3.)-GR1-2.*LLMR)
         GR4   = EXP(LOG_GAMMA(AFAR+4.)-GR1-3.*LLMR)
         GR5   = EXP(LOG_GAMMA(AFAR+5.)-GR1-4.*LLMR)
         GR6   = EXP(LOG_GAMMA(AFAR+6.)-GR1-5.*LLMR)
         GR7   = EXP(LOG_GAMMA(AFAR+6.)-GR1-6.*LLMR)
         MVRR  = MIN(MAX((QR1D/NR1D/C4PI3W)**THRD,RRMIN),RRMAX)
         LMVRR = LOG(MVRR)
         GUR   = EXP(EXP(AFU+BFU*LMVRR**3.+CFU*SQRT(RHO)**3.))
         IF (LIQ_VTR.EQ.0) THEN
            FSQR = EXP(LOG_GAMMA(BVR+AFAR+4.)-LOG_GAMMA(AFAR+4.)-BVR*  &
                   LLMR)
            FSNR = EXP(LOG_GAMMA(BVR+AFAR+1.)-GR1-BVR*LLMR)
            VTQR = RHOAJ*FSQR*AVR
            VTNR = RHOAJ*FSNR*AVR
         ELSEIF (LIQ_VTR.EQ.1) THEN
            LAR2 = LOG(AFAR+2.)
            LAR5 = LOG(AFAR+5.)
            VTQR = EXP(TVR1+TVR2*LAR5+TVR3*LLMR+TVR4*LAR5**2.+TVR5*    &
                   LLMR**2.+TVR6*LAR5*LLMR+TVR7*LAR5**3.+TVR8*LLMR**3.+&
                   TVR9*LAR5*LLMR**2.+TVR10*LAR5**2.*LLMR)*RHOAJ/1.E3
            VTNR = EXP(TVR1+TVR2*LAR2+TVR3*LLMR+TVR4*LAR2**2.+TVR5*    &
                   LLMR**2.+TVR6*LAR2*LLMR+TVR7*LAR2**3.+TVR8*LLMR**3.+&
                   TVR9*LAR2*LLMR**2.+TVR10*LAR2**2.*LLMR)*RHOAJ/1.E3
         ENDIF
         VTQR  = MIN(VTQR,VTRMAX)
         VTNR  = MIN(VTNR,VTRMAX)
         QRMR1 = PI*PI*RHOW*NR1D/24.
         NRMR1 = PI*NR1D/4.
         HRwqv = SSRW0*NR1D*EXP(AQ2+(BQ2+CQ2*LMVRR)*LMVRR**2.)/ABW
         IF (SSAT.EQ.2) THEN
            LLMR  = LOG(LAMR)
            BRTMP = SCN*SQRT(AVR*RHOAJ/MUA)
            QTMP1 = LLMR*(1.5+BVR/2.)
            QTMP2 = EXP(LOG_GAMMA(AFAR+2.)-GR1-LLMR)
            QTMP3 = EXP(LOG_GAMMA(BVR/2.+AFAR+2.5)-GR1-QTMP1)
            VENQR = AVRH*QTMP2+BVRH*BRTMP*QTMP3
            TAUR  = 1./(2.*PI*DV*NR1D*VENQR)
         ENDIF
      ENDIF
      IF (QI1D.GE.QSMALL) THEN
         CALL SOLVE_AFAI(TK1D,P1D,QV1D,QC1D,QI1D,NI1D,ADAGR,ZETA,LAMI, &
              AFAI,MVDI,RHOI,AMI,AVI,BVI)
         IPG    = 3./(ADAGR+2.)
         IPH    = 3.*ADAGR/(ADAGR+2.)
         IPF    = 3.*(ADAGR+1.)/(ADAGR+2.)
         ZETA2  = 2.*(ADAGR-1.)/(ADAGR+2.)
         ZETA3  = 3.*(ADAGR-1.)/(ADAGR+2.)
         DI0Z1  = DI0**ZETA
         DI0Z2  = DI0**ZETA2
         DI0Z3  = DI0**ZETA3
         DI0Z4  = DI0**(4.*ZETA)
         iRHOI  = 1./RHOI
         iAMI   = 1./AMI
         QCLI1  = PI*AMI*NI1D/4.
         QCNI1  = PI*XISP*AMI*NI1D/6.
         NCLI1  = PI*NI1D/4.
         NCNI1  = PI*XISP*NI1D/6.
         LLMI   = LOG(LAMI)
         GI1    = LOG_GAMMA(AFAI+1.)
         GI2    = EXP(LOG_GAMMA(AFAI+2.)-GI1-LLMI)
         GI3    = EXP(LOG_GAMMA(AFAI+3.)-GI1-2.*LLMI)
         GI4    = EXP(LOG_GAMMA(AFAI+4.)-GI1-3.*LLMI)
         GI5    = EXP(LOG_GAMMA(AFAI+5.)-GI1-4.*LLMI)
         GIF1   = EXP(LOG_GAMMA(AFAI+IPF+1.)-GI1-IPF*LLMI)
         GIF2   = EXP(LOG_GAMMA(AFAI+IPF+2.)-GI1-(IPF+1.)*LLMI)
         GIF3   = EXP(LOG_GAMMA(AFAI+IPF+3.)-GI1-(IPF+2.)*LLMI)
         GIF4   = EXP(LOG_GAMMA(AFAI+IPF+4.)-GI1-(IPF+3.)*LLMI)
         GIG1   = EXP(LOG_GAMMA(AFAI+IPG+1.)-GI1-IPG*LLMI)
         GIG2   = EXP(LOG_GAMMA(AFAI+IPG+2.)-GI1-(IPG+1.)*LLMI)
         GIG3   = EXP(LOG_GAMMA(AFAI+IPG+3.)-GI1-(IPG+2.)*LLMI)
         GIG3   = EXP(LOG_GAMMA(AFAI+IPG+4.)-GI1-(IPG+3.)*LLMI)
         GIH1   = EXP(LOG_GAMMA(AFAI+IPH+1.)-GI1-IPH*LLMI)
         GIH2   = EXP(LOG_GAMMA(AFAI+IPH+2.)-GI1-(IPH+1.)*LLMI)
         GIH3   = EXP(LOG_GAMMA(AFAI+IPH+3.)-GI1-(IPH+2.)*LLMI)
         GIH4   = EXP(LOG_GAMMA(AFAI+IPH+4.)-GI1-(IPH+3.)*LLMI)
         GIZ1   = EXP(LOG_GAMMA(AFAI+ZETA3+1.)-GI1-ZETA3*LLMI)
         GI2G1  = EXP(LOG_GAMMA(AFAI+2.*IPG+1.)-GI1-2.*IPG*LLMI)
         GI2G2  = EXP(LOG_GAMMA(AFAI+2.*IPG+2.)-GI1-(2.*IPG+1.)*LLMI)
         GI2G3  = EXP(LOG_GAMMA(AFAI+2.*IPG+3.)-GI1-(2.*IPG+2.)*LLMI)
         GI2G4  = EXP(LOG_GAMMA(AFAI+2.*IPG+4.)-GI1-(2.*IPG+3.)*LLMI)
         GI3G1  = EXP(LOG_GAMMA(AFAI+3.*IPG+1.)-GI1-3.*IPG*LLMI)
         GI2H1  = EXP(LOG_GAMMA(AFAI+2.*IPH+1.)-GI1-2.*IPH*LLMI)
         GI2H3  = EXP(LOG_GAMMA(AFAI+2.*IPH+3.)-GI1-(2.*IPH+2.)*LLMI)
         GI2H4  = EXP(LOG_GAMMA(AFAI+2.*IPH+4.)-GI1-(2.*IPH+3.)*LLMI)
         GI3H1  = EXP(LOG_GAMMA(AFAI+3.*IPH+1.)-GI1-3.*IPH*LLMI)
         GIZF1  = EXP(LOG_GAMMA(AFAI+ZETA3+IPF+1.)-GI1-(ZETA3+IPF)*LLMI)
         GIZG1  = EXP(LOG_GAMMA(AFAI+ZETA3+IPG+1.)-GI1-(ZETA3+IPG)*LLMI)
         GIZH1  = EXP(LOG_GAMMA(AFAI+ZETA3+IPH+1.)-GI1-(ZETA3+IPH)*LLMI)
         IF (ICE_VTI.EQ.0) THEN
            FSQI = EXP(LOG_GAMMA(BVI+AFAI+4.)-LOG_GAMMA(AFAI+4.)-BVI*  &
                   LLMI)
            FSNI = EXP(LOG_GAMMA(BVI+AFAI+1.)-GI1-BVI*LLMI)
            VTQI = MIN(RHOAJ*FSQI*AVI,VTIMAX)
            VTNI = MIN(RHOAJ*FSNI*AVI,VTIMAX)
         ELSEIF (ICE_VTI.EQ.1) THEN
            VISHP = 1.
            IF (ZETA.GT.0.) THEN
               VISHP = DI0**(ZETA/2.)
               AFAZ = AFAI+ZETA
            ELSEIF (ZETA.LT.0.) THEN
               VISHP = DI0**(-ZETA)
               AFAZ = AFAI-ZETA/2.
            ELSE
               VISHP = 1.
               AFAZ = AFAI
            ENDIF
            LLMI2 = LLMI*LLMI
            LLMI3 = LLMI2*LLMI
            LAI2  = LOG(AFAZ+2.)
            LAI5  = LOG(AFAZ+5.)
            RHOX  = VYRHO(MVDI,RHOI)
            VTQI  = EXP(TVI1+TVI2*LAI5+TVI3*LLMI+TVI4*LAI5**2.+TVI5*   &
                    LLMI2+TVI6*LAI5*LLMI+TVI7*LAI5**3.+TVI8*LLMI3+     &
                    TVI9*LAI5*LLMI2+TVI10*LAI5**2.*LLMI)/1.E5*RHOX*    &
                    VISHP*RHOAJ
            VTNI  = EXP(TVI1+TVI2*LAI2+TVI3*LLMI+TVI4*LAI2**2.+TVI5*   &
                    LLMI2+TVI6*LAI2*LLMI+TVI7*LAI2**3.+TVI8*LLMI3+     &
                    TVI9*LAI2*LLMI2+TVI10*LAI2**2.*LLMI)/1.E5*RHOX*    &
                    VISHP*RHOAJ
         ENDIF
         IF (ICE_VENT.EQ.1) THEN
            IF ((ADAGR-1.).GE.SLIMIT) THEN
               IPH   = 3.*ADAGR/(ADAGR+2.)
               IPG   = 3./(ADAGR+2.)
               ZETA2 = 2.*(ADAGR-1.)/(ADAGR+2.)
               ZETA3 = 3.*(ADAGR-1.)/(ADAGR+2.)
               ZETA4 = 4.*(ADAGR-1.)/(ADAGR+2.)
               ZETA5 = 5.*(ADAGR-1.)/(ADAGR+2.)
               H2Z   = ZC2*ZETA
               H4Z   = ZC4*ZETA
               QTMP0 = EXP(LOG_GAMMA(H2Z+AFAI+2.)-GI1-LLMI*(H2Z+1.))
               QTMP1 = EXP(LOG_GAMMA(H4Z+AFAI+2.)-GI1-LLMI*(H4Z+1.))
               BTMP  = SCN*SQRT(AVI*RHOAJ/MUA)
               QTMP2 = LLMI*(H2Z+BVI/2.+IPH/2.+1.)
               QTMP3 = LLMI*(H4Z+BVI/2.+IPH/2.+1.)
               QTMP4 = EXP(LOG_GAMMA(H2Z+BVI/2.+IPH/2.+AFAI+2.)-GI1-   &
                       QTMP2)
               QTMP5 = EXP(LOG_GAMMA(H4Z+BVI/2.+IPH/2.+AFAI+2.)-GI1-   &
                       QTMP3)
               VENQI = AVSG*ZC1*QTMP0/DI0**H2Z+AVSG*ZC3*QTMP1/DI0**H4Z+&
                       BVSG*ZC1*BTMP*QTMP4/DI0**(H2Z+ZETA)+BVSG*ZC3*   &
                       BTMP*QTMP5/DI0**(H4Z+ZETA)
               QTMP6 = LLMI*(BVI/2.+IPH/2.)
               QTMP7 = LLMI*(BVI/2.+IPG/2.)
               QTMP8 = EXP(LOG_GAMMA(BVI/2.+IPH/2.+AFAI+1.)-GI1-QTMP6)
               QTMP9 = EXP(LOG_GAMMA(BVI/2.+IPG/2.+AFAI+1.)-GI1-QTMP7)
               VENIC = AVSG+BVSG*BTMP*QTMP8/DI0**ZETA
               VENIA = AVSG+BVSG*BTMP*QTMP9*DI0**(ZETA/2.)
               INHGR = INHGR*VENIC/VENIA
               GVHAB = (INHGR-1.)/(INHGR+2.)+1.
            ELSEIF ((1.-ADAGR).GE.SLIMIT) THEN
               IPG   = 3./(ADAGR+2.)
               IPH   = 3.*ADAGR/(ADAGR+2.)
               ZETA2 = 2.*(ADAGR-1.)/(ADAGR+2.)
               ZETA3 = 3.*(ADAGR-1.)/(ADAGR+2.)
               ZETA4 = 2.5*(ADAGR-1.)/(ADAGR+2.)
               H2Z   = ZP2*ZETA
               H4Z   = ZP4*ZETA
               QTMP0 = EXP(LOG_GAMMA(H2Z+AFAI+2.)-GI1-LLMI*(H2Z+1.))
               QTMP1 = EXP(LOG_GAMMA(H4Z+AFAI+2.)-GI1-LLMI*(H4Z+1.))
               BTMP  = SCN*SQRT(AVI*RHOAJ/MUA)
               QTMP2 = LLMI*(H2Z+BVI/2.+IPG/2.+1.)
               QTMP3 = LLMI*(H4Z+BVI/2.+IPG/2.+1.)
               QTMP4 = EXP(LOG_GAMMA(H2Z+BVI/2.+IPG/2.+AFAI+2.)-GI1-   &
                       QTMP2)
               QTMP5 = EXP(LOG_GAMMA(H4Z+BVI/2.+IPG/2.+AFAI+2.)-GI1-   &
                       QTMP3)
               VENQI = AVSG*ZP1*QTMP0/DI0**H2Z+AVSG*ZP3*QTMP1/DI0**H4Z+&
                       BVSG*ZP1*BTMP*QTMP4/DI0**(H2Z-ZETA/2.)+BVSG*ZP3*&
                       BTMP*QTMP5/DI0**(H4Z-ZETA/2.)
               QTMP6 = LLMI*(BVI/2.+IPH/2.)
               QTMP7 = LLMI*(BVI/2.+IPG/2.)
               QTMP8 = EXP(LOG_GAMMA(BVI/2.+IPH/2.+AFAI+1.)-GI1-QTMP6)
               QTMP9 = EXP(LOG_GAMMA(BVI/2.+IPG/2.+AFAI+1.)-GI1-QTMP7)
               VENIC = AVSG+BVSG*BTMP*QTMP8/DI0**ZETA
               VENIA = AVSG+BVSG*BTMP*QTMP9*DI0**(ZETA/2.)
               INHGR = INHGR*VENIC/VENIA
               GVHAB = (INHGR-1.)/(INHGR+2.)+1.
            ELSEIF (ABS(ADAGR-1.).LT.SLIMIT) THEN
               QTMP0 = EXP(LOG_GAMMA(AFAI+2.)-GI1-LLMI)
               BTMP  = SCN*SQRT(AVI*RHOAJ/MUA)
               QTMP2 = LLMI*(1.5+BVI/2.)
               QTMP3 = EXP(LOG_GAMMA(BVI/2.+AFAI+2.5)-GI1-QTMP2)
               VENQI = AVSG*QTMP0+BVSG*BTMP*QTMP3
            ENDIF
         ELSEIF (ICE_VENT.EQ.0) THEN
            IF ((ADAGR-1.).GE.SLIMIT) THEN
               H2Z   = ZC2*ZETA
               H4Z   = ZC4*ZETA
               ZETA3 = 3.*(ADAGR-1.)/(ADAGR+2.)
               QTMP0 = EXP(LOG_GAMMA(H2Z+AFAI+2.)-GI1-LLMI*(H2Z+1.))
               QTMP1 = EXP(LOG_GAMMA(H4Z+AFAI+2.)-GI1-LLMI*(H4Z+1.))
               VENQI = ZC1*QTMP0/DI0**H2Z+ZC3*QTMP1/DI0**H4Z
            ELSEIF ((1.-ADAGR).GE.SLIMIT) THEN
               H2Z   = ZP2*ZETA
               H4Z   = ZP4*ZETA
               ZETA3 = 3.*(ADAGR-1.)/(ADAGR+2.)
               QTMP0 = EXP(LOG_GAMMA(H2Z+AFAI+2.)-GI1-LLMI*(H2Z+1.))
               QTMP1 = EXP(LOG_GAMMA(H4Z+AFAI+2.)-GI1-LLMI*(H4Z+1.))
               VENQI = ZP1*QTMP0/DI0**H2Z+ZP3*QTMP1/DI0**H4Z
            ELSEIF (ABS(ADAGR-1.).LT.SLIMIT) THEN
               VENQI = EXP(LOG_GAMMA(AFAI+2.)-GI1-LLMI)
            ENDIF
         ENDIF                                                         ! ICE_VENT
         HIdqv = 2.*PI*NI1D*VENQI*XXLS*SSRI0/ABI
         IF (SSAT.EQ.2) THEN
            TAUI = 1./(2.*PI*DV*NI1D*VENQI)
         ENDIF
      ENDIF                                                             ! QI QSMALL
      IF (QS1D.GE.QSMALL) THEN
         CALL SOLVE_AFAS(TK1D,QS1D,QC1D,NS1D,AFAS,LAMS,MVDS,RHOS,SASPR,&
              AMS,AVS,BVS)
         LLMS = LOG(LAMS)
         GS1  = LOG_GAMMA(AFAS+1.)
         GS2  = EXP(LOG_GAMMA(AFAS+2.)-GS1-LLMS)
         GS3  = EXP(LOG_GAMMA(AFAS+3.)-GS1-2.*LLMS)
         GS4  = EXP(LOG_GAMMA(AFAS+4.)-GS1-3.*LLMS)
         GS5  = EXP(LOG_GAMMA(AFAS+5.)-GS1-4.*LLMS)
         GS6  = EXP(LOG_GAMMA(AFAS+6.)-GS1-5.*LLMS)
         iRHOS = 1./RHOS
         IF (ICE_VTS.EQ.0) THEN
            FSQS = EXP(LOG_GAMMA(BVS+AFAS+4.)-LOG_GAMMA(AFAS+4.)-BVS*  &
                   LLMS)
            FSNS = EXP(LOG_GAMMA(BVS+AFAS+1.)-GS1-BVS*LLMS)
            VTQS = MIN(RHOAJ*FSQS*AVS,VTSMAX)
            VTNS = MIN(RHOAJ*FSNS*AVS,VTSMAX)
         ELSEIF (ICE_VTS.EQ.1) THEN
            LLMS2 = LLMS*LLMS
            LLMS3 = LLMS2*LLMS
            LAS2  = LOG(AFAS+2.)
            LAS5  = LOG(AFAS+5.)
            RHOX  = VYRHO(MVDS,RHOS)
            VTQS  = EXP(TVS1+TVS2*LAS5+TVS3*LLMS+TVS4*LAS5**2.+TVS5*   &
                    LLMS2+TVS6*LAS5*LLMS+TVS7*LAS5**3.+TVS8*LLMS3+     &
                    TVS9*LAS5*LLMS2+TVS10*LAS5**2.*LLMS)/1.E3*RHOX*    &
                    SASPR**THRD*RHOAJ
            VTNS  = EXP(TVS1+TVS2*LAS2+TVS3*LLMS+TVS4*LAS2**2.+TVS5*   &
                    LLMS2+TVS6*LAS2*LLMS+TVS7*LAS2**3.+TVS8*LLMS3+     &
                    TVS9*LAS2*LLMS2+TVS10*LAS2**2.*LLMS)/1.E3*RHOX*    &
                    SASPR**THRD*RHOAJ
         ENDIF
         QCLS1 = PI*AMS*NS1D/4.
         NCLS1 = PI*NS1D/4.
         BSTMP = SCN*SQRT(AVS*RHOAJ/MUA)
         QTMP1 = LLMS*(1.5+BVS/2.)
         QTMP2 = EXP(LOG_GAMMA(AFAS+2.)-GS1-LLMS)
         QTMP3 = EXP(LOG_GAMMA(BVS/2.+AFAS+2.5)-GS1-QTMP1)
         CAPS  = ZP1*SASPR**(ZP2/3.)+ZP3*SASPR**(ZP4/3.)
         VENQS = AVSG*QTMP2*CAPS+BVSG*BSTMP*QTMP3*CAPS
         SASR1 = SASPR**(-1./3.)
         SASR2 = SASPR**(-2./3.)
         SASR3 = SASPR**(1./3.)
         SASR4 = SASPR**(2./3.)
         HSdqv = 2.*PI*NS1D*VENQS*XXLS*SSRI0/ABI
         HSwqv = 2.*PI*NS1D*VENQS*XXLV*SSRW0/ABW
         IF (SSAT.EQ.2) THEN
            TAUS  = 1./(2.*PI*DV*NS1D*VENQS)
         ENDIF
      ENDIF
      IF (QG1D.GE.QSMALL) THEN
         CALL SOLVE_AFAG(TK1D,RHO,QG1D,QC1D,QR1D,QI1D,NG1D,LAMG,AFAG,  &
              MVDG,RHOG,GASPR,AMG,AVG,BVG)
         LLMG  = LOG(LAMG)
         GG1   = LOG_GAMMA(AFAG+1.)
         GG2   = EXP(LOG_GAMMA(AFAG+2.)-GG1-LLMG)
         GG3   = EXP(LOG_GAMMA(AFAG+3.)-GG1-2.*LLMG)
         GG4   = EXP(LOG_GAMMA(AFAG+4.)-GG1-3.*LLMG)
         GG5   = EXP(LOG_GAMMA(AFAG+5.)-GG1-4.*LLMG)
         GG6   = EXP(LOG_GAMMA(AFAG+6.)-GG1-5.*LLMG)
         iAMG  = 1./AMG
         iRHOG = 1./RHOG
         IF (ICE_VTG.EQ.0) THEN
            FSQG = EXP(LOG_GAMMA(BVG+AFAG+4.)-LOG_GAMMA(AFAG+4.)-BVG*  &
                   LLMG)
            FSNG = EXP(LOG_GAMMA(BVG+AFAG+1.)-GG1-BVG*LLMG)
            VTQG = MIN(RHOAJ*FSQG*AVG,VTGMAX)
            VTNG = MIN(RHOAJ*FSNG*AVG,VTGMAX)
         ELSEIF (ICE_VTG.EQ.1) THEN
            LLMG2 = LLMG*LLMG
            LLMG3 = LLMG2*LLMG
            LAG2  = LOG(AFAG+2.)
            LAG5  = LOG(AFAG+5.)
            RHOX  = VYRHO(MVDG,RHOG)
            VTQG  = EXP(TVS1+TVS2*LAG5+TVS3*LLMG+TVS4*LAG5**2.+TVS5*   &
                    LLMG2+TVS6*LAG5*LLMG+TVS7*LAG5**3.+TVS8*LLMG3+TVS9*&
                    LAG5*LLMG2+TVS10*LAG5**2.*LLMG)/1.E3*RHOX*RHOAJ
            VTNG  = EXP(TVS1+TVS2*LAG2+TVS3*LLMG+TVS4*LAG2**2.+TVS5*   &
                    LLMG2+TVS6*LAG2*LLMG+TVS7*LAG2**3.+TVS8*LLMG3+TVS9*&
                    LAG2*LLMG2+TVS10*LAG2**2.*LLMG)/1.E3*RHOX*RHOAJ
            IF (GASPR.LT.1.) THEN
               VTQG = VTQG*GASPR**THRD
               VTNG = VTNG*GASPR**THRD
            ELSEIF (GASPR.GT.1.) THEN
               VTQG = VTQG*GASPR**SIXD
               VTNG = VTNG*GASPR**SIXD
            ENDIF
         ENDIF
         QCLG1 = PI*AMG*NG1D/4.
         NCLG1 = PI*NG1D/4.
         BGTMP = SCN*SQRT(AVG*RHOAJ/MUA)
         QTMP1 = LLMG*(1.5+BVG/2.)
         QTMP2 = EXP(LOG_GAMMA(AFAG+2.)-GG1-LLMG)
         QTMP3 = EXP(LOG_GAMMA(BVG/2.+AFAG+2.5)-GG1-QTMP1)
         IF (GASPR.GT.1.) THEN
            CAPG = ZC1*GASPR**(ZC2/3.)+ZC3*GASPR**(ZC4/3.)
         ELSE
            CAPG = ZP1*GASPR**(ZP2/3.)+ZP3*GASPR**(ZP4/3.)
         ENDIF
         VENQG = AVSG*QTMP2*CAPG+BVSG*BGTMP*QTMP3*CAPG
         GASR1 = GASPR**(-1./3.)
         GASR2 = GASPR**(-2./3.)
         GASR3 = GASPR**(1./3.)
         GASR4 = GASPR**(2./3.)
         HGdqv = 2.*PI*NG1D*VENQG*XXLS*SSRI0/ABI
         HGwqv = 2.*PI*NG1D*VENQG*XXLV*SSRW0/ABW
         IF (SSAT.EQ.2) THEN
            TAUG  = 1./(2.*PI*DV*NG1D*VENQG)
         ENDIF
         MVG0 = EXP(8.8283+0.25*LOG(QG1D))*1.E-6
         DSLL = 1.E-2*(EXP(MIN(20.,-TC1D/(1.1E4*RHO*(QC1D+QR1D)-1.3E3* &
                RHO*QI1D+1.)))-1.)
         DSLL = MIN(1.,MAX(1.E-3,DSLL))
         IF (MVG0.GT.DSLL) THEN
            IHAIL = 1
         ELSE
            IHAIL = 0
         ENDIF
      ENDIF
!----------------- SUPERSATURATION PREDICTION ------------------------
      IF (SSAT.EQ.2) THEN
         TAU = 1./(1./TAUC+1./TAUR+(1./TAUI+1./TAUS+1./TAUG)*(1.+      &
               DQSLDT*XXLS/CPM)/ABI1)
         ATEM = ((QVSI-QVSW)/TAUI+(QVSI-QVSW)/TAUS+(QVSI-QVSW)/TAUG)*( &
                1.+DQSLDT*XXLS/CPM)/ABI1+W1D*G*DQSLDT/CPM-QVSW*G*W1D/( &
                P1D-ESW)
      ENDIF
!----------------- CHEN&LIU WARM CLOUD PROCESSES ----------------------
      IF (QC1D.GE.QSMALL) THEN
         NCLcc = EXP(AN3+BN3*LOG(NC1D)+CN3*LMVRC**3.)*GUC
         NCNcr = NC1D*NC1D*EXP(AN4+BN4*MVRC+CN4/MVRC)*GUC
         QCNcr = NCNcr*EXP(AQ4+BQ4*QC1D/NC1D)
         NCNcr = MIN(NCNcr,NC1D*iDT)
         QCNcr = MIN(QCNcr,QC1D*iDT)
      ENDIF
      IF (QR1D.GE.QSMALL) THEN        
         NCLrr = EXP(AN6+BN6*LOG(NR1D)+CN6/MVRR)*GUR
         NCLrr = MIN(NCLrr*MAX(1.,-2.*AFAR+3.),NR1D*iDT)
         NBKrr = NCLrr*EXP(AN7+(BN7+CN7*MVRR)*MVRR)
         NBKrc = NCLrr*EXP(AN8+(BN8+CN8*MVRR)*MVRR)
         QBKrc = MIN(NBKrc**BQ8*EXP(AQ8+CQ8*LMVRR),QR1D*iDT)
         NBKrc = MIN(NBKrc,NR1D*iDT)
      ENDIF
      IF (QC1D.GE.QSMALL.AND.QR1D.GE.QSMALL) THEN
         NCLcr = NC1D*NR1D*GUR*EXP(AN5+BN5*LMVRR+CN5*LMVRC)
         QCLcr = NC1D*NR1D*GUR*EXP(AQ5+BQ5*LMVRR+CQ5*LMVRC)
         NCLcr = MIN(NCLcr,NC1D*iDT)
         QCLcr = MIN(QCLcr,QC1D*iDT)
      ENDIF
!----------------- WARM RAIN PROCESSES ---------------------------------
      IF (QC1D.GE.QSMALL) THEN
         IF (SSAT.EQ.2) THEN
            QVDvc = ((ATEM*DT*TAU/TAUC+(QV1D-QVSW-ATEM*TAU)*(1.-       &
                    EXP(-DT/TAU))*TAU/TAUC)/ABW1)*iDT
            IF (QVDvc.LT.0.) THEN
               QEVcv = MIN(0.,MAX(QVDvc,-1.*QC1D*iDT))
               QVDvc = 0.
            ENDIF
         ELSE
            IF (SSRW.GT.RSMALL) THEN
               QVDvc = SSRW*EXP(AQ1+BQ1*LOG(NC1D)+CQ1*LMVRC)/ABW
               QVDvc = MIN(MAX(QVDvc,0.),QV1D*iDT)
            ELSEIF (SSRW.LT.(-1.*RSMALL)) THEN
               QEVcv = SSRW*EXP(AQ1+BQ1*LOG(NC1D)+CQ1*LMVRC)/ABW
               QEVcv = MAX(MIN(QEVcv,0.),-1.*QC1D*iDT)
            ENDIF
         ENDIF
         IF (SSRW.LT.(-1.*RSMALL)) THEN
            NACcv = -1.*EXP(AN9+BN9*LOG(NC1D)+CN9*LMVRC+DN9*LOG(-SSRW))
            NACcv = MAX(NACcv,-1.*NC1D*iDT)
            QACcv = MAX(NACcv*4.1888E-15,-1.*QC1D*iDT)
         ENDIF
      ENDIF
      IF (QR1D.GE.QSMALL) THEN
         IF (SSAT.EQ.2) THEN
            QVDvr = ((ATEM*DT*TAU/TAUR+(QV1D-QVSW-ATEM*TAU)*(1.-       &
                    EXP(-DT/TAU))*TAU/TAUR)/ABW1)*iDT
            IF (QVDvr.LT.0.) THEN
               QEVrv = MIN(0.,MAX(QVDvr,-1.*QR1D*iDT))
               QVDvr = 0.
            ENDIF
         ELSE
            IF (SSRW.GT.RSMALL) THEN
               QVDvr = SSRW*NR1D*EXP(AQ2+(BQ2+CQ2*LMVRR)*LMVRR**2.)/ABW
               QVDvr = MIN(MAX(QVDvr,0.),QV1D*iDT)
            ELSEIF (SSRW.LT.(-1.*RSMALL)) THEN
               QEVrv = SSRW*NR1D*EXP(AQ2+(BQ2+CQ2*LMVRR)*LMVRR**2.)/ABW
               QEVrv = MAX(MIN(QEVrv,0.),-1.*QR1D*iDT)
            ENDIF
         ENDIF
         IF (SSRW.LT.(-1.*RSMALL)) THEN
            NACrc = SSRW*EXP(AN10+BN10*LOG(NR1D)+CN10*LMVRR)/ABW
            NACrc = MAX(NACrc,-1.*NR1D*iDT)
            QACrc = MAX(NACrc*5.236E-10,-1.*QR1D*iDT)
         ENDIF
      ENDIF
      IF (TK1D.LT.TK0C) THEN
         IF ((HIdqv+HSdqv+HGdqv).GE.QSMALL) THEN
            VDMAX  = XXLS*(QV1D-QVSI0)/(1.+XXLS**2.*QVSI/(CPM*RV*      &
                     TK1D**2.))*iDT
            SUMDEP = HIdqv+HSdqv+HGdqv
            IF (SUMDEP.GT.VDMAX.AND.VDMAX.GE.QSMALL) THEN
               RATIO = MIN(1.,VDMAX/(SUMDEP+QSMALL))
               HIdqv = HIdqv*RATIO
               HSdqv = HSdqv*RATIO
               HGdqv = HGdqv*RATIO
            ENDIF
         ENDIF
         IF ((HIdqv+HSdqv+HGdqv).LT.0.) THEN
            SBMAX  = XXLS*(QV1D-QVSI0)/(1.+XXLS**2.*QV1D/(CPM*RV*      &
                     TK1D**2.))*iDT
            SUMSUB = HIdqv+HSdqv+HGdqv
            IF (SBMAX.LT.0..AND.SUMSUB.LT.SBMAX*0.9999) THEN
               HIdqv = HIdqv*MIN(1.,0.9999*SBMAX/SUMSUB)
               HSdqv = HSdqv*MIN(1.,0.9999*SBMAX/SUMSUB)
               HGdqv = HGdqv*MIN(1.,0.9999*SBMAX/SUMSUB)
            ENDIF
         ENDIF
!---------- HOMO/HETER FREEZING OF DROPLETS AND RAIN DROPS  ------------
         IF (QC1D.GE.QSMALL) THEN
            IF (TK1D.LT.243.15) THEN
               TMP1  = TC1D*TC1D
               VOLMC = C4PI3*(MVRC*1.E6)**3.
               IJHOF = (10.**MAX(-20.,(-606.3952-52.6611*TC1D-1.7439*  &
                       TMP1-0.0265*TMP1*TC1D-1.536e-4*TMP1**2.)))
               RFZ   = 1.-EXP(-IJHOF*VOLMC*DT)
               IF (TK1D.GT.243.15) RFZ = 0.    ! -30C
               IF (TK1D.LT.223.15) RFZ = 1.    ! -50C
               NHOci = RFZ*NC1D*iDT
               QHOci = RFZ*QC1D*iDT
            ELSE
               QFZC1 = PI*PI*RHOW*NC1D*GC7/36.
               NFZC1 = PI*NC1D*GC4/6.
               IJNMF = BIMM*(EXP(AIMM*(TK0C-TK1D))-1.)
               RFZQ  = 1.-EXP(-IJNMF*QFZC1/QC1D*DT)
               RFZN  = 1.-EXP(-IJNMF*NFZC1/NC1D*DT)
               QNMci = RFZQ*QC1D*iDT
               NNMci = RFZN*NC1D*iDT
            ENDIF
         ENDIF
         IF (QR1D.GE.QSMALL) THEN
            IF (TK1D.LT.233.15) THEN
               QHOrg = QR1D*iDT
               NHOrg = NR1D*iDT
            ELSE
               QFZR1 = PI*PI*RHOW*NR1D*GR7/36.
               NFZR1 = PI*NR1D*GR4/6.
               IJNMF = BIMM*(EXP(AIMM*(TK0C-TK1D))-1.)
               RFZQ  = 1.-EXP(-IJNMF*QFZR1/QR1D*DT)
               RFZN  = 1.-EXP(-IJNMF*NFZR1/NR1D*DT)
               QNMrg = RFZQ*QR1D*iDT
               NNMrg = RFZN*NR1D*iDT
            ENDIF
         ENDIF
!------------------ AUTO-CONVERSION PROCESSES -------------------------
         IF (QI1D.GE.QSMALL) THEN
            IF (EXYM.EQ.0.AND.MVDI.GE.1.E-5) THEN
               EII0 = MIN(1.,MAX(10.**(3.5E-2*TC1D-0.7),1.-RHOI/RHOI0))
               EII3 = EII0
            ELSEIF (EXYM.EQ.1) THEN
               LAI2  = LOG(AFAI+2.)
               LAI5  = LOG(AFAI+5.)
               RVLI0 = RXXM(LAI2,LLMI,RHOI)
               RVLI3 = RXXM(LAI5,LLMI,RHOI)
               IF (RVLI0.GE.0.7) THEN
                  EII0  = ELIM(LAI2,LLMI,RVLI0,MVDI,RHOI,ADAGR)
                  EII3  = ELIM(LAI5,LLMI,RVLI3,MVDI,RHOI,ADAGR)
               ELSE
                  EII0  = 0.
                  EII3  = 0.
               ENDIF
            ENDIF
            IF ((ADAGR-1.).GE.SLIMIT) THEN
               QCNis = QCNI1*VTQI*EII3*NI1D*(GI2H4/DI0Z4+2.*GIF4/      &
                       DI0Z1+DI0Z2*GI2G4)
               NiCNis = NCNI1*VTNI*EII0*NI1D*(GI2H1/DI0Z4+2.*GIF1/     &
                        DI0Z1+DI0Z2*GI2G1)
            ELSEIF ((1.-ADAGR).GE.SLIMIT) THEN
               QCNis = QCNI1*VTQI*EII3*NI1D*4.*DI0Z2*GI2G4
               NiCNis = NCNI1*VTNI*EII0*NI1D*4.*DI0Z2*GI2G1
            ELSEIF (ABS(ADAGR-1.).LT.SLIMIT) THEN
               QCNis = QCNI1*VTQI*EII3*NI1D*4.*GI6
               NiCNis = NCNI1*VTNI*EII0*NI1D*4.*GI3
            ENDIF
            QCNis  = MIN(2.*QCNis,QI1D*iDT)
            NsCNis = MIN(NiCNis,NI1D*iDT)
            NiCNis = MIN(2.*NiCNis,NI1D*iDT)
         ENDIF
!----------------- RIMING OF CLOUD DROPLETS ----------------------------
         IF (QI1D.GE.QSMALL.AND.QC1D.GE.QSMALL) THEN
            IF (EXYM.EQ.1.AND.VTQI.GT.VTQC) THEN
               LAI2  = LOG(AFAI+2.)
               LAI5  = LOG(AFAI+5.)
               RVLI0 = LOG(2.E3*MIN(0.999,MAX(1.E-5,VTNC/VTNI)))
               RVLI3 = LOG(2.E3*MIN(0.999,MAX(1.E-5,VTQC/VTQI)))
               ECI0  = ELIM(LAI2,LLMI,RVLI0,MVDI,RHOI,ADAGR)
               ECI3  = ELIM(LAI5,LLMI,RVLI3,MVDI,RHOI,ADAGR)
            ENDIF
            VTQ0  = VTQI-VTQC
            VTN0  = VTNI-VTNC
            VTQIC = SQRT(VTQ0*VTQ0+0.04*VTQI*VTQC)
            VTNIC = SQRT(VTN0*VTN0+0.04*VTNI*VTNC)
            IF ((ADAGR-1.).GE.SLIMIT) THEN
               IF (EXYM.EQ.0.AND.MVDI.GE.1.E-4) THEN
                  WBIN = MIN(MAX(NINT(MVDC*1.E6/10.),0),20)
                  CNRE = VTQI*DI0**(-2.*ZETA)*MVDI**IPH*RHO/MUA
                  IF (CNRE.LT.0.4) CBIN = 0
                  IF (CNRE.GE.0.4.AND.CNRE.LT.0.6) CBIN = 1
                  IF (CNRE.GE.0.6.AND.CNRE.LT.0.9) CBIN = 2
                  IF (CNRE.GE.0.9.AND.CNRE.LT.1.5) CBIN = 3
                  IF (CNRE.GE.1.5.AND.CNRE.LT.3.5) CBIN = 4
                  IF (CNRE.GE.3.5.AND.CNRE.LT.7.5) CBIN = 5
                  IF (CNRE.GE.7.5.AND.CNRE.LT.15.) CBIN = 6
                  IF (CNRE.GE.15.) CBIN = 7
                  CBIN = MIN(MAX(CBIN,0),7)
                  ECI0 = MAX(0.,IECC(CBIN*21+WBIN))
                  ECI3 = MAX(0.,IECC(CBIN*21+WBIN))
               ENDIF
               QRMci = QRMC1*ECI3*VTQIC*NI1D*(GIF1*GC4/DI0Z1+GIG1*GC5* &
                       DI0Z1+GIH1*GC5/DI0Z2+GC6)
               NRMci = NRMC1*ECI0*VTNIC*NI1D*(GIF1/DI0Z1+GIG1*GC2*     &
                       DI0Z1+GIH1*GC2/DI0Z2+GC3)
            ELSEIF ((1.-ADAGR).GE.SLIMIT) THEN
               IF (EXYM.EQ.0.AND.MVDI.GE.1.5E-4) THEN
                  WBIN = MIN(MAX(NINT(MVDC*1.E6/10.),0),20)
                  PNRE = VTQI*DI0**ZETA*MVDI**IPG*RHO/MUA
                  IF (PNRE.LT.1.5) PBIN = 0
                  IF (PNRE.GE.1.5.AND.PNRE.LT.6.0) PBIN = 1
                  IF (PNRE.GE.6.0.AND.PNRE.LT.15.) PBIN = 2
                  IF (PNRE.GE.15..AND.PNRE.LT.28.) PBIN = 3
                  IF (PNRE.GE.28..AND.PNRE.LT.43.) PBIN = 4
                  IF (PNRE.GE.43..AND.PNRE.LT.70.) PBIN = 5
                  IF (PNRE.GE.70..AND.PNRE.LT.105.) PBIN = 6
                  IF (PNRE.GE.105.) PBIN = 7
                  PBIN = MIN(MAX(PBIN,0),7)
                  ECI0 = MAX(0.,IEPC(PBIN*21+WBIN))
                  ECI3 = MAX(0.,IEPC(PBIN*21+WBIN))
               ENDIF
               QRMci = QRMC1*ECI3*VTQIC*NI1D*(GI2G1*GC4*DI0Z2+2.*GIG1* &
                       GC5*DI0Z1+GC6)
               NRMci = NRMC1*ECI0*VTNIC*NI1D*(GI2G1*DI0Z2+2.*GIG1*GC2* &
                       DI0Z1+GC3)
            ELSEIF (ABS(ADAGR-1.).LT.SLIMIT) THEN
               IF (EXYM.EQ.0.AND.MVDI.GE.2.E-4) THEN
                  ECI0 = 0.5
                  ECI3 = ECI0
               ENDIF
               QRMci = QRMC1*ECI3*VTQIC*NI1D*(GI3*GC4+2.*GI2*GC5+GC6)
               NRMci = NRMC1*ECI0*VTNIC*NI1D*(GI3+2.*GI2*GC2+GC3)
            ENDIF
            QRMci = MIN(QRMci,QC1D*iDT)
            NRMci = MIN(NRMci,NC1D*iDT)
            IF (QRMci.GT.0.) THEN
               QINig  = MIN(2.*QRMci,QI1D*iDT)
               NINig  = MIN(QINig*NI1D/QI1D,NI1D*iDT)
            ENDIF
         ENDIF
         IF (QS1D.GE.QSMALL.AND.QC1D.GE.QSMALL) THEN
            IF (EXYM.EQ.0.AND.MVDS.GE.1.5E-4) THEN
               ECS0 = MIN(MVDC,3.E-5)*3.333E4*SQRT(MIN(MVDS,1.E-3)*1.E3)! Approximation based on Pruppacher&Klett(1997) Fig.14-11
               ECS3 = ECS0
            ELSEIF (EXYM.EQ.1.AND.VTQS.GT.VTQC) THEN
               LAS2  = LOG(AFAS+2.)
               LAS5  = LOG(AFAS+5.)
               RVLS0 = LOG(2.E3*MIN(0.999,MAX(1.E-5,VTNC/VTNS)))
               RVLS3 = LOG(2.E3*MIN(0.999,MAX(1.E-5,VTQC/VTQS)))
               ECS0  = ELXM(LAS2,LLMS,RVLS0,MVDS,RHOS,SASPR)
               ECS3  = ELXM(LAS5,LLMS,RVLS3,MVDS,RHOS,SASPR)
            ENDIF
            VTQ0  = VTQS-VTQC
            VTN0  = VTNS-VTNC
            VTQSC = SQRT(VTQ0*VTQ0+0.04*VTQS*VTQC)
            VTNSC = SQRT(VTN0*VTN0+0.04*VTNS*VTNC)
            QRMcs = QRMC1*ECS3*VTQSC*NS1D*(SASR2*GS3*GC4+SASR1*2.*GS2* &
                    GC5+GC6)
            NRMcs = NRMC1*ECS0*VTNSC*NS1D*(SASR2*GS3+SASR1*2.*GS2*GC2+ &
                    GC3)
            QRMcs = MIN(QRMcs,QC1D*iDT)
            NRMcs = MIN(NRMcs,NC1D*iDT)
            IF (QRMcs.GT.0.) THEN
               NIMcsi = 3.5E8*QRMcs*FF
               QIMcsi = MIN(NIMcsi*MI0,QRMcs)
               QINsg  = MIN(2.*QRMcs,QS1D*iDT)
               NINsg  = MIN(QINsg*NS1D/QS1D,NS1D*iDT)
            ENDIF
         ENDIF
         IF (QG1D.GE.QSMALL.AND.QC1D.GE.QSMALL) THEN
            IF (EXYM.EQ.0.AND.MVDG.GE.2.E-4) THEN
               STOKE = (RHOW*VTQG*MVDC**2.)/(9.*MUA*MVDG)
               STOKE = MAX(1.5,MIN(10.,STOKE))
               ECG0  = 5.5E-1*LOG10(2.51*STOKE)                         ! parameterization based on Cober and List,1993 [JAS]
               ECG3  = ECG0
            ELSEIF (EXYM.EQ.1.AND.VTQG.GT.VTQC) THEN
               LAG2  = LOG(AFAG+2.)
               LAG5  = LOG(AFAG+5.)
               RVLG0 = LOG(2.E3*MIN(0.999,MAX(1.E-5,VTNC/VTNG)))
               RVLG3 = LOG(2.E3*MIN(0.999,MAX(1.E-5,VTQC/VTQG)))
               ECG0  = ELXM(LAG2,LLMG,RVLG0,MVDG,RHOG,GASPR)
               ECG3  = ELXM(LAG5,LLMG,RVLG3,MVDG,RHOG,GASPR)
            ENDIF
            VTQ0  = VTQG-VTQC
            VTN0  = VTNG-VTNC
            VTQGC = SQRT(VTQ0*VTQ0+0.04*VTQG*VTQC)
            VTNGC = SQRT(VTN0*VTN0+0.04*VTNG*VTNC)
            IF (GASPR.GT.1.) THEN
               QRMcg = QRMC1*ECG3*VTQGC*NG1D*(GASR3*GG3*GC4+GASR4*GG2* &
                       GC5+GASR1*GG2*GC5+GC6)
               NRMcg = NRMC1*ECG0*VTNGC*NG1D*(GASR3*GG3+GASR4*GG2*GC2+ &
                       GASR1*GG2*GC2+GC3)
            ELSE
               QRMcg = QRMC1*ECG3*VTQGC*NG1D*(GASR2*GG3*GC4+GASR1*2.*  &
                       GG2*GC5+GC6)
               NRMcg = NRMC1*ECG0*VTNGC*NG1D*(GASR2*GG3+GASR1*2.*GG2*  &
                       GC2+GC3)
            ENDIF
            QRMcg = MIN(QRMcg,QC1D*iDT)
            NRMcg = MIN(NRMcg,NC1D*iDT)
            IF (QRMcg.GT.0.) THEN
               NIMcgi = 3.5E8*QRMcg*FF
               QIMcgi = MIN(NIMcgi*MI0,QRMcg)
            ENDIF
         ENDIF
!----------------- RIMING/COLLECTION OF RAIN DROPS ---------------------
         IF (QR1D.GE.QSMALL.AND.QI1D.GE.QSMALL) THEN
            IF (EXYM.EQ.0.AND.MVDR.GE.MVDI) THEN
               EIR0 = 1.
               EIR3 = 1.
            ELSEIF (EXYM.EQ.1.AND.VTQR.GT.VTQI) THEN
               LAR2  = LOG(AFAR+2.)
               LAR5  = LOG(AFAR+5.)
               RVLR0 = LOG(2.E3*MIN(0.999,MAX(1.E-5,VTNI/VTNR)))
               RVLR3 = LOG(2.E3*MIN(0.999,MAX(1.E-5,VTQI/VTQR)))
               EIR0  = EXRM(LAR2,LLMR,RVLR0,MVDR)
               EIR3  = EXRM(LAR5,LLMR,RVLR3,MVDR)
            ENDIF
            VTQ0  = VTQR-VTQI
            VTN0  = VTNR-VTNI
            VTQRI = SQRT(VTQ0*VTQ0+0.04*VTQR*VTQI)
            VTNRI = SQRT(VTN0*VTN0+0.04*VTNR*VTNI)
            IF ((ADAGR-1.).GE.SLIMIT.AND.MVDR.GE.MVDI) THEN
               QCLir = QCLI1*EIR3*VTQRI*NR1D*(GIF4/DI0Z1+GIG4*GR2*     &
                       DI0Z1+GIH4*GR2/DI0Z2+GI4*GR3)
               NCLir = NCLI1*EIR0*VTNRI*NR1D*(GIF1/DI0Z1+GIG1*GR2*     &
                       DI0Z1+GIH1*GR2/DI0Z2+GR3)
            ELSEIF ((1.-ADAGR).GE.SLIMIT.AND.MVDR.GE.MVDI) THEN
               QCLir = QCLI1*EIR3*VTQRI*NR1D*(GI2G4*DI0Z2+2.*GIG4*GR2* &
                       DI0Z1+GI4*GR3)
               NCLir = NCLI1*EIR0*VTNRI*NR1D*(GI2G1*DI0Z2+2.*GIG1*GR2* &
                       DI0Z1+GR3)
            ELSEIF (ABS(ADAGR-1.).LT.SLIMIT.AND.MVDR.GE.MVDI) THEN
               QCLir = QCLI1*EIR3*VTQRI*NR1D*(GI6+2.*GI5*GR2+GI4*GR3)
               NCLir = NCLI1*EIR0*VTNRI*NR1D*(GI3+2.*GI2*GR2+GR3)
            ENDIF
            QCLir = MIN(QCLir,QI1D*iDT)
            NCLir = MIN(NCLir,NI1D*iDT)
            IF (EXYM.EQ.1.AND.VTQI.GT.VTQR) THEN
               LAI2  = LOG(AFAI+2.)
               LAI5  = LOG(AFAI+5.)
               RVLI0 = LOG(2.E3*MIN(0.999,MAX(1.E-5,VTNR/VTNI)))
               RVLI3 = LOG(2.E3*MIN(0.999,MAX(1.E-5,VTQR/VTQI)))
               ERI0  = ELIM(LAI2,LLMI,RVLI0,MVDI,RHOI,ADAGR)
               ERI3  = ELIM(LAI5,LLMI,RVLI3,MVDI,RHOI,ADAGR)
            ENDIF
            IF ((ADAGR-1.).GE.SLIMIT) THEN
               IF (EXYM.EQ.0.AND.MVDI.GE.1.E-4) THEN
                  ERI0 = 1.
                  ERI3 = 1.
               ENDIF
               QRMri = QRMR1*ERI3*VTQRI*NI1D*(GIF1*GR4/DI0Z1+GIG1*GR5* &
                       DI0Z1+GIH1*GR5/DI0Z2+GR6)
               NRMri = NRMR1*ERI0*VTNRI*NI1D*(GIF1/DI0Z1+GIG1*GR2*     &
                       DI0Z1+GIH1*GR2/DI0Z2+GR3)
            ELSEIF ((1.-ADAGR).GE.SLIMIT) THEN
               IF (EXYM.EQ.0.AND.MVDI.GE.1.5E-4) THEN
                  ERI0 = 1.
                  ERI3 = 1.
               ENDIF
               QRMri = QRMR1*ERI3*VTQRI*NI1D*(GI2G1*GR4*DI0Z2+2.*GIG1* &
                       GR5*DI0Z1+GR6)
               NRMri = NRMR1*ERI0*VTNRI*NI1D*(GI2G1*DI0Z2+2.*GIG1*GR2* &
                       DI0Z1+GR3)
            ELSEIF (ABS(ADAGR-1.).LT.SLIMIT) THEN
               IF (EXYM.EQ.0.AND.MVDI.GE.2.E-4) THEN
                  ERI0 = 1.
                  ERI3 = 1.
               ENDIF
               QRMri = QRMR1*ERI3*VTQRI*NI1D*(GI3*GR4+2.*GI2*GR5+GR6)
               NRMri = NRMR1*ERI0*VTNRI*NI1D*(GI3+2.*GI2*GR2+GR3)
            ENDIF
            QRMri = MIN(QRMri,QR1D*iDT)
            NRMri = MIN(NRMri,NR1D*iDT)
            IF (ICE_RHOG.EQ.0) THEN
               DNIRM = RHOG1
            ELSEIF (ICE_RHOG.EQ.1) THEN
               DNRI  = MIN(MAX(-5.E5*MVDR*VTQRI/TC1D,0.),6.)
               DNIRM = 1.E3*(0.078+0.184*DNRI-0.015*DNRI**2.)
            ENDIF
            DNIRM  = MIN(MAX(DNIRM,RHOIMIN),RHOG0)
            MVDX   = MAX(MVDI,MVDR,(MVDR**3.+MVDI**3.)**THRD)
            QCLirg = QRMri+QCLir
            NCLirg = QCLirg*V2M3/(DNIRM*MVDX**3.)
         ENDIF
         IF (QR1D.GE.QSMALL.AND.QS1D.GE.QSMALL) THEN
            IF (EXYM.EQ.0.AND.MVDS.GE.2.E-4.AND.MVDS.GE.MVDR) THEN
               ERS0 = 1.
               ERS3 = ERS0
            ELSEIF (EXYM.EQ.1.AND.VTQS.GT.VTQR) THEN
               LAS2  = LOG(AFAS+2.)
               LAS5  = LOG(AFAS+5.)
               RVLS0 = LOG(2.E3*MIN(0.999,MAX(1.E-5,VTNR/VTNS)))
               RVLS3 = LOG(2.E3*MIN(0.999,MAX(1.E-5,VTQR/VTQS)))
               ERS0  = ELXM(LAS2,LLMS,RVLS0,MVDS,RHOS,SASPR)
               ERS3  = ELXM(LAS5,LLMS,RVLS3,MVDS,RHOS,SASPR)
            ENDIF
            VTQ0  = VTQR-VTQS
            VTN0  = VTNR-VTNS
            VTQRS = SQRT(VTQ0*VTQ0+0.04*VTQR*VTQS)
            VTNRS = SQRT(VTN0*VTN0+0.04*VTNR*VTNS)
            QRMrs = QRMR1*ERS3*VTQRS*NS1D*(SASR2*GS3*GR4+SASR1*2.*GS2* &
                    GR5+GR6)
            NRMrs = NRMR1*ERS0*VTNRS*NS1D*(SASR2*GS3+SASR1*2.*GS2*GR2+ &
                    GR3)
            QRMrs = MIN(QRMrs,QR1D*iDT)
            NRMrs = MIN(NRMrs,NR1D*iDT)
            IF (EXYM.EQ.0.AND.MVDR.GE.MVDS) THEN
               ESR0 = 1.
               ESR3 = ESR0
            ELSEIF (EXYM.EQ.1.AND.VTQR.GT.VTQS) THEN
               LAR2  = LOG(AFAR+2.)
               LAR5  = LOG(AFAR+5.)
               RVLR0 = LOG(2.E3*MIN(0.999,MAX(1.E-5,VTNS/VTNR)))
               RVLR3 = LOG(2.E3*MIN(0.999,MAX(1.E-5,VTQS/VTQR)))
               ESR0  = EXRM(LAR2,LLMR,RVLR0,MVDR)
               ESR3  = EXRM(LAR5,LLMR,RVLR3,MVDR)
            ENDIF
            QCLsr = QCLS1*ESR3*VTQRS*NR1D*(SASR2*GS6+SASR1*2.*GR2*GS5+ &
                    GR3*GS4)
            NCLsr = NCLS1*ESR0*VTNRS*NR1D*(SASR2*GS3+SASR1*2.*GR2*GS2+ &
                    GR3)
            QCLsr = MIN(QCLsr,QS1D*iDT)
            NCLsr = MIN(NCLsr,NS1D*iDT)
            IF (QRMrs.GT.0.) THEN
               NIMrsi = 3.5E8*QRMrs*FF
               QIMrsi = MIN(NIMrsi*MI0,QRMrs)
            ENDIF
            IF (ICE_RHOG.EQ.0) THEN
               DNSRM = RHOG1
            ELSEIF (ICE_RHOG.EQ.1) THEN
               DNRI  = MIN(MAX(-5.E5*MVDR*VTQRS/TC1D,0.),6.)
               DNSRM = 1.E3*(0.078+0.184*DNRI-0.015*DNRI**2.)
            ENDIF
            DNSRM  = MIN(MAX(DNSRM,RHOIMIN),RHOG0)
            MVDX   = MAX(MVDR,MVDS,(MVDR**3.+MVDS**3.)**THRD)
            QCLsrg = QRMrs+QCLsr
            NCLsrg = QCLsrg*V2M3/(DNSRM*MVDX**3.)
         ENDIF
         IF (QR1D.GE.QSMALL.AND.QG1D.GE.QSMALL) THEN
            IF (EXYM.EQ.0.AND.MVDG.GE.2.E-4.AND.MVDG.GE.MVDR) THEN
               STOKE = (RHOW*ABS(VTQ0)*MVDR**2.)/(9.*MUA*MVDG)
               STOKE = MAX(1.5,MIN(10.,STOKE))
               ERG0  = 5.5E-1*LOG10(2.51*STOKE)                        ! parameterization based on Cober and List, 1993 [JAS]
               ERG3  = ERG0
            ELSEIF (EXYM.EQ.1.AND.VTQG.GT.VTQR) THEN
               LAG2  = LOG(AFAG+2.)
               LAG5  = LOG(AFAG+5.)
               RVLG0 = LOG(2.E3*MIN(0.999,MAX(1.E-5,VTNR/VTNG)))
               RVLG3 = LOG(2.E3*MIN(0.999,MAX(1.E-5,VTQR/VTQG)))
               ERG0  = ELXM(LAG2,LLMG,RVLG0,MVDG,RHOG,GASPR)
               ERG3  = ELXM(LAG5,LLMG,RVLG3,MVDG,RHOG,GASPR)
            ENDIF
            VTQ0  = VTQR-VTQG
            VTN0  = VTNR-VTNG
            VTQRG = SQRT(VTQ0*VTQ0+0.04*VTQR*VTQG)
            VTNRG = SQRT(VTN0*VTN0+0.04*VTNR*VTNG)
            IF (GASPR.GT.1.) THEN
               QRMrg = QRMR1*ERG3*VTQRG*NG1D*(GASR3*GG3*GR4+GASR4*GG2* &
                       GR5+GASR1*GG2*GR5+GR6)
               NRMrg = NRMR1*ERG0*VTNRG*NG1D*(GASR3*GG3+GASR4*GG2*GR2+ &
                       GASR1*GG2*GR2+GR3)
            ELSE
               QRMrg = QRMR1*ERG3*VTQRG*NG1D*(GASR2*GG3*GR4+GASR1*2.*  &
                       GG2*GR5+GR6)
               NRMrg = NRMR1*ERG0*VTNRG*NG1D*(GASR2*GG3+GASR1*2.*GG2*  &
                       GR2+GR3)
            ENDIF
            QRMrg = MIN(QRMrg,QR1D*iDT)
            NRMrg = MIN(NRMrg,NR1D*iDT)
            IF (EXYM.EQ.0.AND.MVDR.GE.MVDG) THEN
               STOKE = (RHOW*ABS(VTQ0)*MVDR**2.)/(9.*MUA*MVDG)
               STOKE = MAX(1.5,MIN(10.,STOKE))
               EGR0  = 5.5E-1*LOG10(2.51*STOKE)                        ! parameterization based on Cober and List, 1993 [JAS]
               EGR3  = EGR0
            ELSEIF (EXYM.EQ.1.AND.VTQR.GT.VTQG) THEN
               LAR2  = LOG(AFAR+2.)
               LAR5  = LOG(AFAR+5.)
               RVLR0 = LOG(2.E3*MIN(0.999,MAX(1.E-5,VTNG/VTNR)))
               RVLR3 = LOG(2.E3*MIN(0.999,MAX(1.E-5,VTQG/VTQR)))
               EGR0  = EXRM(LAR2,LLMR,RVLR0,MVDR)
               EGR3  = EXRM(LAR5,LLMR,RVLR3,MVDR)
            ENDIF
            IF (GASPR.GT.1.) THEN
               QCLgr = QCLG1*EGR3*VTQRG*NR1D*(GASR3*GG6+GASR4*GR2*GG5+ &
                       GASR1*GR2*GG5+GR3*GG4)
               NCLgr = NCLG1*EGR0*VTNRG*NR1D*(GASR3*GG3+GASR4*GR2*GG2+ &
                       GASR1*GR2*GG2+GR3)
            ELSE
               QCLgr = QCLG1*EGR3*VTQRG*NR1D*(GASR2*GG6+GASR1*2.*GR2*  &
                       GG5+GR3*GG4)
               NCLgr = NCLG1*EGR0*VTNRG*NR1D*(GASR2*GG3+GASR1*2.*GR2*  &
                       GG2+GR3)
            ENDIF
            QCLgr = MIN(QCLgr,QG1D*iDT)
            NCLgr = MIN(NCLgr,NG1D*iDT)
            IF (QRMrg.GT.0.) THEN
               NIMrgi = 3.5E8*QRMrg*FF
               QIMrgi = MIN(NIMrgi*MI0,QRMrg)
            ENDIF
            QCLgrg = QRMrg+QCLgr
            NCLgrg = (NRMrg+NCLgr)*RHOG/RHOW
         ENDIF
!----------------- ICE-PHASE COLLECTION&CONVERSION*INITILIZATION -------
         IF (QS1D.GE.QSMALL.AND.QI1D.GE.QSMALL) THEN
            RHOIS = (QI1D+QS1D)/(QI1D/RHOI+QS1D/RHOS+ISMALL)
            IF (EXYM.EQ.0.AND.MVDS.GE.MVDI) THEN
               EIS0 = MIN(1.,MAX(0.05*EXP(0.1*TC1D),1.-RHOIS/RHOI0,0.))
               EIS3 = EIS0
            ELSEIF (EXYM.EQ.1.AND.VTQS.GT.VTQI) THEN
               LAS2  = LOG(AFAS+2.)
               LAS5  = LOG(AFAS+5.)
               RVLS0 = LOG(2.E3*MIN(0.999,MAX(1.E-5,VTNI/VTNS)))
               RVLS3 = LOG(2.E3*MIN(0.999,MAX(1.E-5,VTQI/VTQS)))
               EIS0  = ELXM(LAS2,LLMS,RVLS0,MVDS,RHOS,SASPR)
               EIS3  = ELXM(LAS5,LLMS,RVLS3,MVDS,RHOS,SASPR)
            ENDIF
            VTQ0  = VTQS-VTQI
            VTN0  = VTNS-VTNI
            VTQIS = SQRT(VTQ0*VTQ0+0.04*VTQI*VTQS)
            VTNIS = SQRT(VTN0*VTN0+0.04*VTNI*VTNS)
            IF ((ADAGR-1.).GE.SLIMIT) THEN
               QCLis = QCLI1*EIS3*VTQIS*NS1D*(GIF4/DI0Z1+SASR1*GIG4*   &
                       GS2*DI0Z1+SASR1*GIH4*GS2/DI0Z2+SASR2*GI4*GS3)
               NCLis = NCLI1*EIS0*VTNIS*NS1D*(GIF1/DI0Z1+SASR1*GIG1*   &
                       GS2*DI0Z1+SASR1*GIH1*GS2/DI0Z2+SASR2*GS3)
            ELSEIF ((1.-ADAGR).GE.SLIMIT) THEN
               QCLis = QCLI1*EIS3*VTQIS*NS1D*(GI2G4*DI0Z2+SASR1*2.*    &
                       GIG4*GS2*DI0Z1+SASR2*GI4*GS3)
               NCLis = NCLI1*EIS0*VTNIS*NS1D*(GI2G1*DI0Z2+SASR1*2.*    &
                       GIG1*GS2*DI0Z1+SASR2*GS3)
            ELSEIF (ABS(ADAGR-1.).LT.SLIMIT) THEN
               QCLis = QCLI1*EIS3*VTQIS*NS1D*(GI6+SASR1*2.*GI5*GS2+    &
                       SASR2*GI4*GS3)
               NCLis = NCLI1*EIS0*VTNIS*NS1D*(GI3+SASR1*2.*GI2*GS2+    &
                       SASR2*GS3)
            ENDIF
            QCLis  = MIN(QCLis,QI1D*iDT)
            NCLis  = MIN(NCLis,NI1D*iDT)
         ENDIF
         IF (QG1D.GE.QSMALL.AND.QI1D.GE.QSMALL.AND.IHAIL.EQ.1) THEN
            IF (EXYM.EQ.0.AND.MVDG.GE.MVDI) THEN
               EIG0 = MIN(1.,0.01*EXP(0.1*TC1D))
               EIG3 = EIG0
            ELSEIF (EXYM.EQ.1.AND.VTQG.GT.VTQI) THEN
               LAG2  = LOG(AFAG+2.)
               LAG5  = LOG(AFAG+5.)
               RVLG0 = LOG(2.E3*MIN(0.999,MAX(1.E-5,VTNI/VTNG)))
               RVLG3 = LOG(2.E3*MIN(0.999,MAX(1.E-5,VTQI/VTQG)))
               EIG0  = ELXM(LAG2,LLMG,RVLG0,MVDG,RHOG,GASPR)
               EIG3  = ELXM(LAG5,LLMG,RVLG3,MVDG,RHOG,GASPR)
            ENDIF
            VTQ0  = VTQG-VTQI
            VTN0  = VTNG-VTNI
            VTQIG = SQRT(VTQ0*VTQ0+0.04*VTQI*VTQG)
            VTNIG = SQRT(VTN0*VTN0+0.04*VTNI*VTNG)
            IF ((ADAGR-1.).GE.SLIMIT) THEN
               QCLig = QCLI1*EIG3*VTQIG*NG1D*(GIF4/DI0Z1+GASR1*GIG4*   &
                       GG2*DI0Z1+GASR1*GIH4*GG2/DI0Z2+GASR2*GI4*GG3)
               NCLig = NCLI1*EIG0*VTNIG*NG1D*(GIF1/DI0Z1+GASR1*GIG1*   &
                       GG2*DI0Z1+GASR1*GIH1*GG2/DI0Z2+GASR2*GG3)
            ELSEIF ((1.-ADAGR).GE.SLIMIT) THEN
               QCLig = QCLI1*EIG3*VTQIG*NG1D*(GI2G4*DI0Z2+GASR1*2.*    &
                       GIG4*GG2*DI0Z1+GASR2*GI4*GG3)
               NCLig = NCLI1*EIG0*VTNIG*NG1D*(GI2G1*DI0Z2+GASR1*2.*    &
                       GIG1*GG2*DI0Z1+GASR2*GG3)
            ELSEIF (ABS(ADAGR-1.).LT.SLIMIT) THEN
               QCLig = QCLI1*EIG3*VTQIG*NG1D*(GI6+GASR1*2.*GI5*GG2+    &
                       GASR2*GI4*GG3)
               NCLig = NCLI1*EIG0*VTNIG*NG1D*(GI3+GASR1*2.*GI2*GG2+    &
                       GASR2*GG3)
            ENDIF
            QCLig = MIN(QCLig,QI1D*iDT)
            NCLig = MIN(NCLig,NI1D*iDT)
         ENDIF
         IF (QG1D.GE.QSMALL.AND.QS1D.GE.QSMALL.AND.IHAIL.EQ.1) THEN
            IF (EXYM.EQ.0.AND.MVDG.GE.MVDS) THEN
               ESG0 = MIN(1.,0.01*EXP(0.1*TC1D))
               ESG3 = ESG0
            ELSEIF (EXYM.EQ.1.AND.VTQG.GT.VTQS) THEN
               LAG2  = LOG(AFAG+2.)
               LAG5  = LOG(AFAG+5.)
               RVLG0 = LOG(2.E3*MIN(0.999,MAX(1.E-5,VTNS/VTNG)))
               RVLG3 = LOG(2.E3*MIN(0.999,MAX(1.E-5,VTQS/VTQG)))
               ESG0  = ELXM(LAG2,LLMG,RVLG0,MVDG,RHOG,GASPR)
               ESG3  = ELXM(LAG5,LLMG,RVLG3,MVDG,RHOG,GASPR)
            ENDIF
            VTQ0  = VTQG-VTQS
            VTN0  = VTNG-VTNS
            VTQSG = SQRT(VTQ0*VTQ0+0.04*VTQS*VTQG)
            VTNSG = SQRT(VTN0*VTN0+0.04*VTNS*VTNG)
            QCLsg = QCLS1*ESG3*VTQSG*NG1D*(GASR2*SASR2*GS6+GASR1*SASR1*&
                    2.*GG2*GS5+GS4*GG3)
            NCLsg = NCLS1*ESG0*VTNSG*NG1D*(GASR2*SASR2*GS3+GASR1*SASR1*&
                    2.*GS2*GG2+GG3)
            QCLsg = MIN(QCLsg,QS1D*iDT)
            NCLsg = MIN(NCLsg,NS1D*iDT)
         ENDIF
         IF (QS1D.GE.QSMALL) THEN
            IF (EXYM.EQ.0.AND.MVDS.GE.2.E-4) THEN
               ESS0 = 0.1*MIN(1.,0.05*EXP(0.1*TC1D))
               ESS3 = ESS0
            ELSEIF (EXYM.EQ.1) THEN
               LAS2  = LOG(AFAS+2.)
               LAS5  = LOG(AFAS+5.)
               RVLS0 = RXXM(LAS2,LLMS,RHOS)
               RVLS3 = RXXM(LAS5,LLMS,RHOS)
               IF (RVLS0.GE.0.7) THEN
                  ESS0  = ELXM(LAS2,LLMS,RVLS0,MVDS,RHOS,SASPR)
                  ESS3  = ELXM(LAS5,LLMS,RVLS3,MVDS,RHOS,SASPR)
               ELSE
                  ESS0  = 0.
                  ESS3  = 0.
               ENDIF
            ENDIF
            QCLss = PI**2.*RHOS*VTQS*ESS3*SASR2*NS1D**2.*GS6/6.
            IF (QCLss.GT.0.) THEN
               ISEPS = 0.6
               ISEPL = SASPR
               LIM1  = 0.7*(1.-RHOS/RHOI0)
               LIM2  = SQRT(1.-LIM1*LIM1)
               LIM22 = LIM2*LIM2
               LIM23 = LIM22*LIM2
               LIMV  = (1.+2.*ISEPL+ISEPL**2.+ISEPS+2.*ISEPL*ISEPS+    &
                       ISEPL**2.*ISEPS)/8.
               DSLSV = (1.+LIM2/SQRT(SASPR)+2.*LIM2*SQRT(SASPR)+2.*    &
                       LIM22+LIM22*SASPR+LIM23*SQRT(SASPR))*GS4*LIMV
               DNSAG = 2.*RHOS*GS4/DSLSV
               NCLss = QCLss*V2M3/DNSAG/DSLSV-PI*VTNS*ESS0*SASR2*      &
                       NS1D**2.*GS3
            ENDIF
         ENDIF
         IF (QI1D.GE.QSMALL) THEN
            IF (SSAT.EQ.2) THEN
               QVDvi = ((ATEM*DT*TAU/TAUI+(QV1D-QVSW-ATEM*TAU)*(1.-    &
                       EXP(-DT/TAU))*TAU/TAUI+(QVSW-QVSI)*DT/TAUI)/    &
                       ABI1)*iDT
            ELSE
               QVDvi = 2.*PI*NI1D*VENQI*SSRI/ABI
            ENDIF
            IF (QVDvi.LT.0.) THEN
               QSBiv = MIN(0.,MAX(QVDvi,-1.*QI1D*iDT))
               QVDvi = 0.
            ENDIF
         ENDIF                                                         ! QISMALL
         IF (QS1D.GE.QSMALL) THEN
            IF (SSAT.EQ.2) THEN
               QVDvs = ((ATEM*DT*TAU/TAUS+(QV1D-QVSW-ATEM*TAU)*(1.-    &
                       EXP(-DT/TAU))*TAU/TAUS+(QVSW-QVSI)*DT/TAUS)/    &
                       ABI1)*iDT
            ELSE
               QVDvs = 2.*PI*NS1D*VENQS*SSRI/ABI
            ENDIF
            IF (QVDvs.LT.0.) THEN
               QSBsv = MIN(0.,MAX(QVDvs,-1.*QS1D*iDT))
               QVDvs = 0.
            ENDIF
         ENDIF
         IF (QG1D.GE.QSMALL) THEN
            IF (SSAT.EQ.2) THEN
               QVDvg = ((ATEM*DT*TAU/TAUG+(QV1D-QVSW-ATEM*TAU)*(1.-    &
                       EXP(-DT/TAU))*TAU/TAUG+(QVSW-QVSI)*DT/TAUG)/    &
                       ABI1)*iDT
            ELSE
               QVDvg = 2.*PI*NG1D*VENQG*SSRI/ABI
            ENDIF
            IF (QVDvg.LT.0.) THEN
               QSBgv = MIN(0.,MAX(QVDvg,-1.*QG1D*iDT))
               QVDvg = 0.
            ENDIF
         ENDIF
      ELSE                                                              ! WARM CLOUD START
         IF ((HCwqv+HRwqv+HSwqv+HGwqv).GE.QSMALL) THEN
            VDMAX = XXLV*(QV1D-QVSW0)/ABW1*iDT
            SUMCND = HCwqv+HRwqv+HSwqv+HGwqv
            IF (SUMCND.GT.VDMAX.AND.VDMAX.GE.QSMALL) THEN
               RATIO = MIN(1.,VDMAX/(SUMCND+QSMALL))
               HCwqv = HCwqv*RATIO
               HRwqv = HRwqv*RATIO
               HSwqv = HSwqv*RATIO
               HGwqv = HGwqv*RATIO
            ENDIF
         ENDIF
         IF ((HCwqv+HRwqv+HSwqv+HGwqv).LT.0.) THEN
            EVMAX = XXLV*(QV1D-QVSW0)/ABW1*iDT
            SUMEVP = HCwqv+HRwqv+HSwqv+HGwqv
            IF (EVMAX.LT.0..AND.SUMEVP.LT.EVMAX*0.9999) THEN
               HCwqv = HCwqv*MIN(1.,0.9999*EVMAX/SUMEVP)
               HRwqv = HRwqv*MIN(1.,0.9999*EVMAX/SUMEVP)
               HSwqv = HSwqv*MIN(1.,0.9999*EVMAX/SUMEVP)
               HGwqv = HGwqv*MIN(1.,0.9999*EVMAX/SUMEVP)
            ENDIF
         ENDIF
!----------------- MELTING PROXESSES -----------------------------------
         IF (QI1D.GE.QSMALL) THEN
            IF (MVDI.GT.DCR) THEN
               QMLir = -1.*QI1D*iDT
               NMLir = MIN(QMLir*NI1D/QI1D,0.)
            ELSE
               QMLic = -1.*QI1D*iDT
               NMLic = MIN(QMLic*NI1D/QI1D,0.)
            ENDIF
         ENDIF
         IF (QS1D.GE.QSMALL) THEN
            RMcsq = 0.
            RMrsq = 0.
            RMcsa = 0.
            RMrsa = 0.
            IF (QC1D.GE.QSMALL) THEN
               ECS   = 1.
               VTQ0  = VTQS-VTQC
               VTQSC = SQRT(VTQ0*VTQ0+4.E-2*VTQS*VTQC)
               RMcsq = QRMC1*ECS*VTQSC*NS1D*(GS3*GC4+2.*GS2*GC5+GC6)
               RMcsq = MIN(RMcsq,QC1D*iDT)
            ENDIF
            IF (QR1D.GE.QSMALL) THEN
               ERS   = 1.
               VTQ0  = VTQR-VTQS
               VTQRS = SQRT(VTQ0*VTQ0+4.E-2*VTQR*VTQS)
               RMrsq = QRMR1*ERS*VTQRS*NS1D*(GS3*GR4+2.*GS2*GR5+GR6)
               RMrsq = MIN(RMrsq,QR1D*iDT)
            ENDIF
            MLMAX = 1.-MIN(1.,RHOS/RHOIMAX)
            HSwcd = 2.*PI*NS1D*KAP*VENQS*(TK0C-TK1D)
            HSwrm = CPW*(TK0C-TK1D)*(RMrsq+RMcsq)
            SMLF  = MAX(0.,-1.*(HSwcd+HSwqv+HSwrm)*DT/XXLF0)/QS1D
            IF (SMLF.LT.MLIMIT) THEN
               SMLF = 0.
               SMLR = 0.
               SMLV = 0.
            ELSE
               SMLF = MIN(MLMAX,MAX(0.,SMLF))
               SMLR = 0.01195*EXP(4.411*SMLF)
               SMLV = (RHOS/(RHOW/SMLF-RHOW+RHOS))**MLRA
            ENDIF
            AVWSG = SMLR*AVRH+(1.-SMLR)*AVSG
            BVWSG = SMLR*BVRH+(1.-SMLR)*BVSG
            RHOWS = SMLF*RHOW+(1.-SMLF)*RHOS
            IF (AGG_SHAPE.EQ.0) THEN
               WSAPR = 1.
            ELSEIF (AGG_SHAPE.EQ.1) THEN
               LLMS  = LOG(LAMS)
               LAS5  = LOG(AFAS+5.)
               WSAPR = SMLF*(MIN(1.,(-32.232528-16.735319*LAS5+        &
                       14.830389*LLMS-1.8312883*LAS5**2.-1.6827601*    &
                       LLMS**2.+3.6556064*LAS5*LLMS-7.4526392E-2*      &
                       LAS5**3.+6.2393644E-2*LLMS**3.-0.20029785*LAS5* &
                       LLMS**2.+0.20699596*LAS5**2.*LLMS)/10.))+(1.-   &
                       SMLF)*SASPR
!               DSMM  = MVDS*1.E3
!               WSAPR = SMLF*MAX(0.4,0.9951+2.51E-2*DSMM-3.644E-2*      &
!                       DSMM**2.+5.303E-3*DSMM**3.-2.492E-4*DSMM**4.)+( &
!                       1.-SMLF)*SASPR
            ENDIF
            IF (SMLF.GE.MLIMIT) THEN
               LAS2  = LOG(AFAS+2.)
               LAS22 = LAS2*LAS2
               LLMS2 = LLMS*LLMS
               LMDS  = LOG(1.E6*MVDS)
               LMDS2 = LMDS*LMDS
               LROS  = LOG(RHOWS)
               LROS2 = LROS*LROS
               RHOX  = VXRHO(MVDS,RHOWS,BVS)
               BVS   = MAX(1.,(BVY1+BVY2*LMDS+BVY3*LROS+BVY4*LMDS2+    &
                       BVY5*LROS2+BVY6*LMDS*LROS)/(1.+BVY7*LMDS+BVY8*  &
                       LROS+BVY9*LMDS2+BVY10*LROS2+BVY11*LMDS*LROS))*BVS
               AVS   = EXP((AVX1+AVX2*LAS2+AVX3*LLMS+AVX4*LAS22+AVX5*  &
                       LLMS2+AVX6*LAS2*LLMS)/(1.+AVX7*LAS2+AVX8*LLMS+  &
                       AVX9*LAS22+AVX10*LLMS2+AVX11*LAS2*LLMS))*RHOX*  &
                       WSAPR**THRD
            ELSE
               AVS = AVS
               BVS = BVS
            ENDIF
            BSTMP = SCN*SQRT(AVS*RHOAJ/MUA)
            QTMP1 = LLMS*(1.5+BVS/2.)
            QTMP2 = EXP(LOG_GAMMA(AFAS+2.)-GS1-LLMS)
            QTMP3 = EXP(LOG_GAMMA(BVS/2.+AFAS+2.5)-GS1-QTMP1)
            CAPWS = ZP1*WSAPR**(ZP2/3.)+ZP3*WSAPR**(ZP4/3.)
            VENWS = AVWSG*QTMP2*CAPWS+BVWSG*BSTMP*QTMP3*CAPWS
            SMLTQ = CPW/XXLF*(TK0C-TK1D)*(RMcsq+RMrsq)
            QMLsr = 2.*PI*NS1D*KAP*(TK0C-TK1D)/XXLF*VENWS+SMLTQ     ! NEGATIVE
            QMLsr = MAX(MIN(QMLsr,0.),-1.*QS1D*iDT)
            IF (SSRW.LT.-1.*RSMALL) THEN
               QEVsv = 2.*PI*NS1D*VENWS*SSRW/ABW
               QEVsv = MAX(MIN(QEVsv,0.),QMLsr)
               QMLsr = QMLsr-QEVsv
            ENDIF
            NMLsr = MIN((QMLsr+QEVsv)*NS1D/QS1D,0.)
         ENDIF
         IF (QG1D.GE.QSMALL) THEN
            RMcgq = 0.
            RMrgq = 0.
            RMcga = 0.
            RMrga = 0.
            IF (QC1D.GE.QSMALL) THEN
               ECG   = 1.
               VTQ0  = VTQG-VTQC
               VTQGC = SQRT(VTQ0*VTQ0+0.04*VTQG*VTQC)
               RMcgq = QRMC1*ECG*VTQGC*NG1D*(GG3*GC4+2.*GG2*GC5+GC6)
               RMcgq = MIN(RMcgq,QC1D*iDT)
            ENDIF
            IF (QR1D.GE.QSMALL) THEN
               ERG  = 1.
               VTQ0  = VTQR-VTQG
               VTQRG = SQRT(VTQ0*VTQ0+0.04*VTQR*VTQG)
               RMrgq = QRMR1*ERG*VTQRG*NG1D*(GG3*GR4+2.*GG2*GR5+GR6)
               RMrgq = MIN(RMrgq,QR1D*iDT)
            ENDIF
            MLMAX = 1.-MIN(1.,RHOG/RHOIMAX)
            HGwcd = 2.*PI*NG1D*KAP*VENQG*(TK0C-TK1D)
            HGwrm = CPW*(TK0C-TK1D)*(RMrgq+RMcgq)
            GMLF  = MAX(0.,-1.*(HGwcd+HGwqv+HGwrm)*DT/XXLF0)/QG1D
            IF (GMLF.LT.MLIMIT) THEN
               GMLF = 0.
               GMLR = 0.
               GMLV = 0.
            ELSE
               GMLF = MIN(MLMAX,MAX(0.,GMLF))
               GMLR = 0.01195*EXP(4.411*GMLF)
               GMLV = (RHOG/(RHOW/GMLF-RHOW+RHOG))**MLRA
            ENDIF
            AVWSG = GMLR*AVRH+(1.-GMLR)*AVSG
            BVWSG = GMLR*BVRH+(1.-GMLR)*BVSG
            RHOWG = GMLF*RHOW+(1.-GMLF)*RHOG
            IF (GRAP_SHAPE.EQ.0) THEN
               WGAPR = 1.
            ELSEIF (GRAP_SHAPE.EQ.1) THEN
               LLMG  = LOG(LAMG)
               LAG5  = LOG(AFAG+5.)
               WGAPR = GMLF*(MIN(1.,(-32.232528-16.735319*LAG5+        &
                       14.830389*LLMG-1.8312883*LAG5**2.-1.6827601*    &
                       LLMG**2.+3.6556064*LAG5*LLMG-7.4526392E-2*      &
                       LAG5**3.+6.2393644E-2*LLMG**3.-0.20029785*LAG5* &
                       LLMG**2.+0.20699596*LAG5**2.*LLMG)/10.))+(1.-   &
                       GMLF)*GASPR
            ENDIF
            IF (GMLF.GE.MLIMIT) THEN
               LAG2  = LOG(AFAG+2.)
               LAG22 = LAG2*LAG2
               LLMG2 = LLMG*LLMG
               LMDG  = LOG(1.E6*MVDG)
               LMDG2 = LMDG*LMDG
               LROG  = LOG(RHOWG)
               LROG2 = LROG*LROG
               RHOX  = VXRHO(MVDG,RHOWG,BVG)
               BVG   = MAX(1.,(BVY1+BVY2*LMDG+BVY3*LROG+BVY4*LMDG2+    &
                       BVY5*LROG2+BVY6*LMDG*LROG)/(1.+BVY7*LMDG+BVY8*  &
                       LROG+BVY9*LMDG2+BVY10*LROG2+BVY11*LMDG*LROG))*BVG
               AVG   = EXP((AVX1+AVX2*LAG2+AVX3*LLMG+AVX4*LAG22+AVX5*  &
                       LLMG2+AVX6*LAG2*LLMG)/(1.+AVX7*LAG2+AVX8*LLMG+  &
                       AVX9*LAG22+AVX10*LLMG2+AVX11*LAG2*LLMG))*RHOX
               IF (WGAPR.LT.1.) AVG = AVG*WGAPR**THRD
               IF (WGAPR.GT.1.) AVG = AVG*WGAPR**SIXD
            ELSE
               AVG   = AVG
               BVG   = BVG
            ENDIF
            BGTMP = SCN*SQRT(AVG*RHOAJ/MUA)
            QTMP1 = LLMG*(1.5+BVG/2.)
            QTMP2 = EXP(LOG_GAMMA(AFAG+2.)-GG1-LLMG)
            QTMP3 = EXP(LOG_GAMMA(BVG/2.+AFAG+2.5)-GG1-QTMP1)
            IF (GASPR.GT.1.) THEN
               CAPWG = ZC1*WGAPR**(ZC2/3.)+ZC3*WGAPR**(ZC4/3.)
            ELSE
               CAPWG = ZP1*WGAPR**(ZP2/3.)+ZP3*WGAPR**(ZP4/3.)
            ENDIF
            VENWG = AVWSG*QTMP2*CAPWG+BVWSG*BGTMP*QTMP3*CAPWG
            GMLTQ = CPW/XXLF*(TK0C-TK1D)*(RMcgq+RMrgq)
            QMLgr = 2.*PI*NG1D*KAP*(TK0C-TK1D)/XXLF*VENWG+GMLTQ     ! NEGATIVE
            QMLgr = MAX(MIN(QMLgr,0.),-1.*QG1D*iDT)
            IF (SSRW.LT.-1.*RSMALL) THEN
               QEVgv = 2.*PI*NG1D*VENWG*SSRW/ABW
               QEVgv = MAX(MIN(QEVgv,0.),QMLgr)
               QMLgr = QMLgr-QEVgv
            ENDIF
            NMLgr = MIN((QMLgr+QEVgv)*NG1D/QG1D,0.)
         ENDIF
      ENDIF                                                             ! TEMPERATURE LOOPS
!----------------- CONSERVATION OF HYDROMETEORS ------------------------
      QVSOUR = QV1D+(-QACcv-QEVcv-QEVrv-QEVsv-QEVgv-QSBiv-QSBsv-       &
               QSBgv)*DT
      QVSINK = (QVDvc+QVDvr+QVDvi+QVDvs+QVDvg)*DT
      IF (QVSINK.GT.QVSOUR.AND.QVSOUR.GE.QSMALL) THEN
         RATIO = MIN(1.,QVSOUR/(QVSINK+QSMALL))
         QVDvc = QVDvc*RATIO; QVDvr = QVDvr*RATIO
         QVDvi = QVDvi*RATIO; QVDvs = QVDvs*RATIO
         QVDvg = QVDvg*RATIO
      ENDIF
      QCSOUR = QC1D+(QBKrc-QMLic+QVDvc-QACrc)*DT
      QCSINK = (QHOci+QNMci+QNCci+QRMci+QRMcs+QRMcg+QCLcr+QCNcr+QIMcsi+&
               QIMcgi-QACcv-QEVcv)*DT
      IF (QCSINK.GT.QCSOUR.AND.QCSOUR.GE.QSMALL) THEN
         RATIO = MIN(1.,QCSOUR/(QCSINK+QSMALL))
         QHOci = QHOci*RATIO; QNMci = QNMci*RATIO; QNCci = QNCci*RATIO
         QRMci = QRMci*RATIO; QRMcs = QRMcs*RATIO; QRMcg = QRMcg*RATIO
         QCLcr = QCLcr*RATIO; QCNcr = QCNcr*RATIO; QIMcsi = QIMcsi*RATIO
         QIMcgi = QIMcgi*RATIO; QACcv = QACcv*RATIO; QEVcv = QEVcv*RATIO
      ENDIF
      IF (QC1D.GE.QSMALL) THEN
         IF (EVSB.EQ.0) THEN
            NEVcv = 0.
         ELSEIF (EVSB.EQ.1) THEN
            NEVcv = MIN(QEVcv*NC1D/QC1D,0.)
         ELSEIF (EVSB.EQ.2) THEN
            DTHL  = MAX(DCMIN,SQRT(-8.*MIN(0.,SSRW*DT/ABW/RHOW)))
            RATIO = MIN(0.9,MAX(0.,ABS(GAMMP(AFAC+1.,DTHL*LAMC))))
            NEVcv = -1.*NC1D*iDT*RATIO
            NEVcv = MAX(V2M3*QEVcv/RHOW/DTHL**3.,NEVcv)
         ENDIF
         NEVcv = MAX(-1.*NC1D*iDT,NEVcv)
      ENDIF
      NCSOUR = NC1D+(NBKrc-NMLic-NACrc)*DT
      NCSINK = (NHOci+NNMci+NNCci+NRMci+NRMcs+NRMcg+NCLcr+NCLcc+NCNcr+ &
               NIMcsi+NIMcgi-NACcv-NEVcv)*DT
      IF (NCSINK.GT.NCSOUR.AND.NCSOUR.GE.NSMALL) THEN
         RATIO = MIN(1.,NCSOUR/(NCSINK+NSMALL))
         NHOci = NHOci*RATIO; NNMci = NNMci*RATIO; NNCci = NNCci*RATIO
         NRMci = NRMci*RATIO; NRMcs = NRMcs*RATIO; NRMcg = NRMcg*RATIO
         NIMcgi = NIMcgi*RATIO; NCLcr = NCLcr*RATIO; NCLcc = NCLcc*RATIO
         NIMcsi = NIMcsi*RATIO; NCNcr = NCNcr*RATIO; NACcv = NACcv*RATIO
         NEVcv = NEVcv*RATIO
      ENDIF
      QRSOUR = QR1D+(QCLcr+QCNcr-QMLir-QMLsr-QMLgr+QVDvr)*DT
      QRSINK = (QHOrg+QNMrg+QRMri+QRMrs+QRMrg+QBKrc+QIMrsi+QIMrgi-     &
               QACrc-QEVrv)*DT
      IF (QRSINK.GT.QRSOUR.AND.QRSOUR.GE.QSMALL) THEN
         RATIO = MIN(1.,QRSOUR/(QRSINK+QSMALL))
         QHOrg = QHOrg*RATIO; QNMrg = QNMrg*RATIO; QRMri = QRMri*RATIO
         QRMrs = QRMrs*RATIO; QRMrg = QRMrg*RATIO; QBKrc = QBKrc*RATIO
         QIMrsi = QIMrsi*RATIO; QIMrgi = QIMrgi*RATIO
         QACrc = QACrc*RATIO; QEVrv = QEVrv*RATIO
      ENDIF
      IF (QR1D.GE.QSMALL) THEN
         IF (EVSB.EQ.0) THEN
            NEVrv = 0.
         ELSEIF (EVSB.EQ.1) THEN
            NEVrv = MIN(QEVrv*NR1D/QR1D,0.)
         ELSEIF (EVSB.EQ.2) THEN
            VENT = MAX(1.,(AVRH*MVDR+BVRH*BRTMP*MVDR**(1.5+BVR/2.))/   &
                   MVDR)
            DTHL  = MAX(DCMIN,SQRT(-8.*MIN(0.,SSRW*DT*VENT/ABW/RHOW)))
            RATIO = MIN(0.9,MAX(0.,ABS(GAMMP(AFAR+1.,DTHL*LAMR))))
            NEVrv = -1.*NR1D*iDT*RATIO
            NEVrv = MAX(V2M3*QEVrv/RHOW/DTHL**3.,NEVrv)
         ENDIF
         NEVrv = MAX(-1.*NR1D*iDT,NEVrv)
      ENDIF
      NRSOUR = NR1D+(NCNcr+NBKrr-NMLir-NMLsr-NMLgr)*DT
      NRSINK = (NHOrg+NNMrg+NRMri+NRMrs+NRMrg+NCLrr+NBKrc-NACrc+NIMrsi+&
               NIMrgi-NEVrv)*DT
      IF (NRSINK.GT.NRSOUR.AND.NRSOUR.GE.NSMALL) THEN
         RATIO = MIN(1.,NRSOUR/(NRSINK+NSMALL))
         NHOrg = NHOrg*RATIO; NNMrg = NNMrg*RATIO; NRMri = NRMri*RATIO
         NRMrs = NRMrs*RATIO; NRMrg = NRMrg*RATIO; NCLrr = NCLrr*RATIO
         NBKrc = NBKrc*RATIO; NIMrsi = NIMrsi*RATIO; NEVrv = NEVrv*RATIO
         NIMrgi = NIMrgi*RATIO; NACrc = NACrc*RATIO
      ENDIF
      QISOUR = QI1D+(QIMcsi+QIMcgi+QIMrsi+QIMrgi+QHOci+QNMci+QNCci+    &
               QRMci+QVDvi)*DT
      QISINK = (-QMLir-QMLic+QCLir+QCLis+QCLig+QINig+QCNis-QSBiv)*DT
      IF (QISINK.GT.QISOUR.AND.QISOUR.GE.QSMALL) THEN
         RATIO = MIN(1.,QISOUR/(QISINK+QSMALL))
         QMLir = QMLir*RATIO; QMLic = QMLic*RATIO; QCLir = QCLir*RATIO
         QCLis = QCLis*RATIO; QINig = QINig*RATIO; QCNis = QCNis*RATIO
         QSBiv = QSBiv*RATIO; QCLig = QCLig*RATIO
      ENDIF
      IF (QI1D.GE.QSMALL) THEN
         IF (EVSB.EQ.0) THEN
            NSBiv = 0.
         ELSEIF (EVSB.EQ.1) THEN
            NSBiv = MIN(QSBiv*NI1D/QI1D,0.)
         ELSEIF (EVSB.EQ.2) THEN
            VENT  = MAX(1.,(AVSG*MVDI+BVSG*BTMP*MVDI**(1.5+BVI))/MVDI)
            DTHI  = MAX(DCMIN,SQRT(-8.*MIN(0.,SSRI*DT*VENT/ABI/RHOI)))
            RATIO = MIN(0.9,MAX(0.,ABS(GAMMP(AFAI+1.,DTHI*LAMI))))
            NSBiv = -1.*NI1D*iDT*RATIO
            NSBiv = MAX(V2M3*QSBiv/RHOI/DTHI**3.,NSBiv)
         ENDIF
         NSBiv = MAX(-1.*NI1D*iDT,NSBiv)
      ENDIF
      NISOUR = NI1D+(NIMcsi+NIMcgi+NIMrsi+NIMrgi+NHOci+NNMci+NNCci)*DT
      NISINK = (-NMLir-NMLic+NCLir+NCLis+NCLig+NINig+NiCNis-NSBiv)*DT
      IF (NISINK.GT.NISOUR.AND.NISOUR.GE.NSMALL) THEN
         RATIO = MIN(1.,NISOUR/(NISINK+NSMALL))
         NCLis = NCLis*RATIO; NINig = NINig*RATIO; NMLir = NMLir*RATIO
         NMLic = NMLic*RATIO; NCLir = NCLir*RATIO; NiCNis = NiCNis*RATIO
         NSBiv = NSBiv*RATIO; NCLig = NCLig*RATIO
      ENDIF
      QSSOUR = QS1D+(QCNis+QCLis+QRMcs+QVDvs)*DT
      QSSINK = (-QMLsr+QCLsr+QCLsg+QINsg-QEVsv-QSBsv)*DT
      IF (QSSINK.GT.QSSOUR.AND.QSSOUR.GE.QSMALL) THEN
         RATIO = MIN(1.,QSSOUR/(QSSINK+QSMALL))
         QMLsr = QMLsr*RATIO; QINsg = QINsg*RATIO; QCLsr = QCLsr*RATIO
         QSBsv = QSBsv*RATIO; QEVsv = QEVsv*RATIO; QCLsg = QCLsg*RATIO
      ENDIF
      IF (QS1D.GE.QSMALL) THEN
         IF (EVSB.EQ.0) THEN
            NSBsv = 0.
         ELSEIF (EVSB.EQ.1) THEN
            NSBsv = MIN(QSBsv*NS1D/QS1D,0.)
         ELSEIF (EVSB.EQ.2) THEN
            VENT = MAX(1.,(AVSG*MVDS+BVSG*BSTMP*MVDS**(1.5+BVS/2.))/   &
                   MVDS)
            IF (TK1D.LT.TK0C) THEN
               DTHI = MAX(DCMIN,SQRT(-8.*MIN(0.,SSRI*DT*VENT/ABI/RHOS)))
            ELSE
               DTHI = MAX(DCMIN,SQRT(-8.*MIN(0.,SSRW*DT*VENT/ABW/RHOS)))
            ENDIF
            RATIO = MIN(0.9,MAX(0.,ABS(GAMMP(AFAS+1.,DTHI*LAMS))))
            NSBsv = -1.*NS1D*iDT*RATIO
            NSBsv = MAX(V2M3*QSBsv/RHOS/DTHI**3.,NSBsv)
         ENDIF
         NSBsv = MAX(-1.*NS1D*iDT,NSBsv)
      ENDIF
      NSSOUR = NS1D+NsCNis*DT
      NSSINK = (-NMLsr+NCLsr+NCLsg-NCLss+NINsg-NSBsv)*DT
      IF (NSSINK.GT.NSSOUR.AND.NSSOUR.GE.NSMALL) THEN
         RATIO = MIN(1.,NSSOUR/(NSSINK+NSMALL))
         NMLsr = NMLsr*RATIO; NCLss = NCLss*RATIO; NINsg = NINsg*RATIO
         NCLsr = NCLsr*RATIO; NSBsv = NSBsv*RATIO; NCLsg = NCLsg*RATIO
      ENDIF
      QGSOUR = QG1D+(QHOrg+QNMrg+QINig+QINsg+QRMcg+QCLig+QCLsg+QCLirg+ &
               QCLsrg+QCLgrg+QVDvg)*DT
      QGSINK = (-QMLgr+QCLgr-QSBgv-QEVgv)*DT
      IF (QGSINK.GT.QGSOUR.AND.QGSOUR.GE.QSMALL) THEN
         RATIO = MIN(1.,QGSOUR/(QGSINK+QSMALL))
         QMLgr = QMLgr*RATIO; QCLgr = QCLgr*RATIO
         QSBgv = QSBgv*RATIO; QEVgv = QEVgv*RATIO
      ENDIF
      IF (QG1D.GE.QSMALL) THEN
         IF (EVSB.EQ.0) THEN
            NSBgv = 0.
         ELSEIF (EVSB.EQ.1) THEN
            NSBgv = MIN(QSBgv*NG1D/QG1D,0.)
         ELSEIF (EVSB.EQ.2) THEN
            VENT = MAX(1.,(AVSG*MVDG+BVSG*BGTMP*MVDG**(1.5+BVG/2.))/   &
                   MVDG)
            IF (TK1D.LT.TK0C) THEN
               DTHI = MAX(DCMIN,SQRT(-8.*MIN(0.,SSRI*DT*VENT/ABI/RHOG)))
            ELSE
               DTHI = MAX(DCMIN,SQRT(-8.*MIN(0.,SSRW*DT*VENT/ABW/RHOG)))
            ENDIF
            RATIO = MIN(0.9,MAX(0.,ABS(GAMMP(AFAG+1.,DTHI*LAMG))))
            NSBgv = -1.*NG1D*iDT*RATIO
            NSBgv = MAX(V2M3*QSBgv/RHOG/DTHI**3.,NSBgv)
         ENDIF
         NSBgv = MAX(-1.*NG1D*iDT,NSBgv)
      ENDIF
      NGSOUR = NG1D+(NHOrg+NNMrg+NINig+NINsg+NCLirg+NCLsrg+NCLgr-      &
               NSBgv)*DT
      NGSINK = (-NMLgr+NCLgr)*DT
      IF (NGSINK.GT.NGSOUR.AND.NGSOUR.GE.NSMALL) THEN
         RATIO = MIN(1.,NGSOUR/(NGSINK+NSMALL))
         NMLgr = NMLgr*RATIO; NCLgr = NCLgr*RATIO; NSBgv = NSBgv*RATIO
      ENDIF
      QFZci = QHOci+QNMci+QNCci
      NFZci = NHOci+NNMci+NNCci
      QFZrg = QHOrg+QNMrg
      NFZrg = NHOrg+NNMrg
      QIMii = QIMcsi+QIMcgi+QIMrsi+QIMrgi
      NIMii = NIMcsi+NIMcgi+NIMrsi+NIMrgi
!----------------- UPDATE VARIABLES ------------------------------------
      QV1D = MAX(0.,QV1D+(-QACcv-QVDvc-QVDvr-QEVcv-QEVrv-QEVsv-QEVgv-  &
             QVDvi-QVDvs-QVDvg-QSBiv-QSBsv-QSBgv)*DT)
      QC1D = MAX(0.,QC1D+(QBKrc-QCLcr-QCNcr-QFZci-QMLic-QRMci-QRMcs-   &
             QRMcg-QIMcsi-QIMcgi+QACcv-QACrc+QVDvc+QEVcv)*DT)
      QR1D = MAX(0.,QR1D+(QCLcr+QCNcr-QBKrc-QFZrg-QIMrsi-QIMrgi-QMLir- &
             QMLsr-QMLgr-QRMri-QRMrs-QRMrg+QACrc+QVDvr+QEVrv)*DT)
      QI1D = MAX(0.,QI1D+(QIMii-QCNis-QCLis-QCLig-QINig+QFZci+QMLir+   &
             QMLic+QRMci-QCLir+QVDvi+QSBiv)*DT)
      QS1D = MAX(0.,QS1D+(QCNis+QCLis-QINsg+QMLsr+QRMcs-QCLsr-QCLsg+   &
             QVDvs+QSBsv+QEVsv)*DT)
      QG1D = MAX(0.,QG1D+(QFZrg+QINig+QINsg+QMLgr+QRMcg+QCLig+QCLsg-   &
             QCLgr+QCLirg+QCLsrg+QCLgrg+QVDvg+QSBgv+QEVgv)*DT)
      NC1D = MAX(0.,NC1D+(NBKrc-NCLcr-NCLcc-NCNcr-NFZci-NRMci-NRMcs-   &
             NRMcg-NMLic-NIMcsi-NIMcgi+NACcv-NACrc+NEVcv)*DT)
      NR1D = MAX(0.,NR1D+(NCNcr+NBKrr-NBKrc-NCLrr-NIMrsi-NIMrgi-NFZrg- &
             NMLir-NMLsr-NMLgr-NRMri-NRMrs-NRMrg+NACrc+NEVrv)*DT)
      NI1D = MAX(0.,NI1D+(NIMii-NiCNis-NCLis-NCLig-NINig+NFZci+NMLir+  &
             NMLic-NCLir+NSBiv)*DT)
      NS1D = MAX(0.,NS1D+(NsCNis+NCLss-NINsg+NMLsr-NCLsr-NCLsg+NSBsv)* &
             DT)
      NG1D = MAX(0.,NG1D+(NFZrg+NINig+NINsg+NMLgr-NCLgr+NCLirg+NCLsrg+ &
             NCLgrg+NSBgv)*DT)
      TK1D = TK1D+((QACcv+QACrc+QVDvc+QVDvr+QEVcv+QEVrv+QEVsv+QEVgv)*  &
             XXLV+(QFZci+QFZrg+QMLir+QMLic+QMLsr+QMLgr+QRMci+QRMcs+    &
             QRMcg+QRMri+QRMrs+QRMrg+QIMii)*XXLF+(QVDvi+QVDvs+QVDvg+   &
             QSBiv+QSBsv+QSBgv)*XXLS)/CPM*DT

      IF (QC1D.GE.QSMALL.AND.NC1D.GE.NSMALL) THEN
         MVDC = (QC1D*iAMW/NC1D)**THRD
         IF (MVDC.GT.DCR) THEN
            QR1D = QR1D+QC1D
            NR1D = NR1D+NC1D
            QC1D = 0.; NC1D = 0.
         ENDIF
      ENDIF
      IF (QR1D.GE.QSMALL.AND.NR1D.GE.NSMALL) THEN
         MVDR = (QR1D*iAMW/NR1D)**THRD
         IF (MVDR.LT.DCR) THEN
            QC1D = QC1D+QR1D
            NC1D = NC1D+NR1D
            QR1D = 0.; NR1D = 0.
         ENDIF
      ENDIF

      END SUBROUTINE MAIN_MP
!======================================================================
!
!========================================================================
      END MODULE module_mp_tcwa2
!======================================================================
