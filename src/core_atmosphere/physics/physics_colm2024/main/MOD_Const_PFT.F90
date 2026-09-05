#include <define.h>

MODULE MOD_Const_PFT

!-----------------------------------------------------------------------
!
! !DESCRIPTION:
!  Set constants for plant functional types (PFTs)
!
!  Created by Hua Yuan, 08/2019
!
! !REVISIONS:
!  10/2021, Xingjie Lu: added for crop PFTs
!
!-----------------------------------------------------------------------
! !USES:
   USE MOD_Precision
   USE MOD_Vars_Global
   USE MOD_TimeManager, only: get_calday

   IMPLICIT NONE
   SAVE

! Plant Functional Type classification
!-----------------------------------------------------------------------
! 0  not vegetated
! 1  needleleaf evergreen temperate tree
! 2  needleleaf evergreen boreal tree
! 3  needleleaf deciduous boreal tree
! 4  broadleaf evergreen tropical tree
! 5  broadleaf evergreen temperate tree
! 6  broadleaf deciduous tropical tree
! 7  broadleaf deciduous temperate tree
! 8  broadleaf deciduous boreal tree
! 9  broadleaf evergreen shrub
!10  broadleaf deciduous temperate shrub
!11  broadleaf deciduous boreal shrub
!12  c3 arctic grass
!13  c3 non-arctic grass
!14  c4 grass
!15  c3 crop
!16  c3_irrigated
!17  temperate_corn
!18  irrigated_temperate_corn
!19  spring_wheat
!20  irrigated_spring_wheat
!21  winter_wheat
!22  irrigated_winter_wheat
!23  temperate_soybean
!24  irrigated_temperate_soybean
!25  barley
!26  irrigated_barley
!27  winter_barley
!28  irrigated_winter_barley
!29  rye
!30  irrigated_rye
!31  winter_rye
!32  irrigated_winter_rye
!33  cassava
!34  irrigated_cassava
!35  citrus
!36  irrigated_citrus
!37  cocoa
!38  irrigated_cocoa
!39  coffee
!40  irrigated_coffee
!41  cotton
!42  irrigated_cotton
!43  datepalm
!44  irrigated_datepalm
!45  foddergrass
!46  irrigated_foddergrass
!47  grapes
!48  irrigated_grapes
!49  groundnuts
!50  irrigated_groundnuts
!51  millet
!52  irrigated_millet
!53  oilpalm
!54  irrigated_oilpalm
!55  potatoes
!56  irrigated_potatoes
!57  pulses
!58  irrigated_pulses
!59  rapeseed
!60  irrigated_rapeseed
!61  rice
!62  irrigated_rice
!63  sorghum
!64  irrigated_sorghum
!65  sugarbeet
!66  irrigated_sugarbeet
!67  sugarcane
!68  irrigated_sugarcane
!69  sunflower
!70  irrigated_sunflower
!71  miscanthus
!72  irrigated_miscanthus
!73  switchgrass
!74  irrigated_switchgrass
!75  tropical_corn
!76  irrigated_tropical_corn
!77  tropical_soybean
!78  irrigated_tropical_soybean

   character(len=256) :: pftclassname (0:N_PFT+N_CFT-1) = &
      (/'0  not vegetated                       ', '1  needleleaf evergreen temperate tree ', &
        '2  needleleaf evergreen boreal tree    ', '3  needleleaf deciduous boreal tree    ', &
        '4  broadleaf evergreen tropical tree   ', '5  broadleaf evergreen temperate tree  ', &
        '6  broadleaf deciduous tropical tree   ', '7  broadleaf deciduous temperate tree  ', &
        '8  broadleaf deciduous boreal tree     ', '9  broadleaf evergreen shrub           ', &
        '10  broadleaf deciduous temperate shrub', '11  broadleaf deciduous boreal shrub   ', &
        '12  c3 arctic grass                    ', '13  c3 non-arctic grass                ', &
        '14  c4 grass                           ', '15  c3 crop                            '  &
#ifdef CROP
       ,'16  c3_irrigated                       ', '17  temperate_corn                     ', &
        '18  irrigated_temperate_corn           ', '19  spring_wheat                       ', &
        '20  irrigated_spring_wheat             ', '21  winter_wheat                       ', &
        '22  irrigated_winter_wheat             ', '23  temperate_soybean                  ', &
        '24  irrigated_temperate_soybean        ', '25  barley                             ', &
        '26  irrigated_barley                   ', '27  winter_barley                      ', &
        '28  irrigated_winter_barley            ', '29  rye                                ', &
        '30  irrigated_rye                      ', '31  winter_rye                         ', &
        '32  irrigated_winter_rye               ', '33  cassava                            ', &
        '34  irrigated_cassava                  ', '35  citrus                             ', &
        '36  irrigated_citrus                   ', '37  cocoa                              ', &
        '38  irrigated_cocoa                    ', '39  coffee                             ', &
        '40  irrigated_coffee                   ', '41  cotton                             ', &
        '42  irrigated_cotton                   ', '43  datepalm                           ', &
        '44  irrigated_datepalm                 ', '45  foddergrass                        ', &
        '46  irrigated_foddergrass              ', '47  grapes                             ', &
        '48  irrigated_grapes                   ', '49  groundnuts                         ', &
        '50  irrigated_groundnuts               ', '51  millet                             ', &
        '52  irrigated_millet                   ', '53  oilpalm                            ', &
        '54  irrigated_oilpalm                  ', '55  potatoes                           ', &
        '56  irrigated_potatoes                 ', '57  pulses                             ', &
        '58  irrigated_pulses                   ', '59  rapeseed                           ', &
        '60  irrigated_rapeseed                 ', '61  rice                               ', &
        '62  irrigated_rice                     ', '63  sorghum                            ', &
        '64  irrigated_sorghum                  ', '65  sugarbeet                          ', &
        '66  irrigated_sugarbeet                ', '67  sugarcane                          ', &
        '68  irrigated_sugarcane                ', '69  sunflower                          ', &
        '70  irrigated_sunflower                ', '71  miscanthus                         ', &
        '72  irrigated_miscanthus               ', '73  switchgrass                        ', &
        '74  irrigated_switchgrass              ', '75  tropical_corn                      ', &
        '76  irrigated_tropical_corn            ', '77  tropical_soybean                   ', &
        '78  irrigated_tropical_soybean         '  &
#endif
        /)

   ! canopy layer number
   integer , parameter :: canlay_p(0:N_PFT+N_CFT-1) &
      = (/0, 2, 2, 2, 2, 2, 2, 2 &
        , 2, 1, 1, 1, 1, 1, 1, 1 &
#ifdef CROP
        , 1, 1, 1, 1, 1, 1, 1, 1 &
        , 1, 1, 1, 1, 1, 1, 1, 1 &
        , 1, 1, 1, 1, 1, 1, 1, 1 &
        , 1, 1, 1, 1, 1, 1, 1, 1 &
        , 1, 1, 1, 1, 1, 1, 1, 1 &
        , 1, 1, 1, 1, 1, 1, 1, 1 &
        , 1, 1, 1, 1, 1, 1, 1, 1 &
        , 1, 1, 1, 1, 1, 1, 1    &
#endif
         /)

   ! canopy top height
   real(r8), parameter :: htop0_p(0:N_PFT+N_CFT-1) &
      =(/ 0.5,  17.0,  17.0,  14.0,  35.0,  35.0,  18.0,  20.0&
        ,20.0,   0.5,   0.5,   0.5,   0.5,   0.5,   0.5,   0.5&
#ifdef CROP
        , 0.5,   0.5,   0.5,   0.5,   0.5,   0.5,   0.5,   0.5&
        , 0.5,   0.5,   0.5,   0.5,   0.5,   0.5,   0.5,   0.5&
        , 0.5,   0.5,   0.5,   0.5,   0.5,   0.5,   0.5,   0.5&
        , 0.5,   0.5,   0.5,   0.5,   0.5,   0.5,   0.5,   0.5&
        , 0.5,   0.5,   0.5,   0.5,   0.5,   0.5,   0.5,   0.5&
        , 0.5,   0.5,   0.5,   0.5,   0.5,   0.5,   0.5,   0.5&
        , 0.5,   0.5,   0.5,   0.5,   0.5,   0.5,   0.5,   0.5&
        , 0.5,   0.5,   0.5,   0.5,   0.5,   0.5,   0.5    &
#endif
         /)

   ! canopy bottom height
   ! 01/06/2020, yuan: adjust htop: grass/shrub -> 0, tree->1
   real(r8), parameter :: hbot0_p(0:N_PFT+N_CFT-1) &
     !TODO: check the setting values
     !=(/0.01,   8.5,   8.5,   7.0,   1.0,   1.0,  10.0,  11.5&
     !   11.5,   0.1,   0.1,   0.1,  0.01,  0.01,  0.01,  0.01/)
      =(/0.00,   1.0,   1.0,   1.0,   1.0,   1.0,   1.0,   1.0&
        , 1.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0&
#ifdef CROP
        , 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0&
        , 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0&
        , 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0&
        , 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0&
        , 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0&
        , 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0&
        , 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0&
        , 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0    &
#endif
         /)

   ! default vegetation fractional cover
   real(r8), parameter :: fveg0_p(0:N_PFT+N_CFT-1) &
      = 1.0 !(/.../)

   ! default stem area index
   real(r8), parameter :: sai0_p(0:N_PFT+N_CFT-1) &
      =(/0.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0&
       , 2.0, 0.5, 0.5, 0.5, 0.2, 0.2, 0.2, 0.2&
#ifdef CROP
       , 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2&
       , 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2&
       , 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2&
       , 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2&
       , 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2&
       , 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2&
       , 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2&
       , 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2    &
#endif
         /)

   ! ratio to calculate roughness length z0m
   real(r8), parameter :: z0mr_p(0:N_PFT+N_CFT-1) = 0.1

   ! ratio to calculate displacement height d
   real(r8), parameter :: displar_p(0:N_PFT+N_CFT-1) = 0.667

   ! inverse&sqrt leaf specific dimension size 4 cm
   real(r8), parameter :: sqrtdi_p(0:N_PFT+N_CFT-1) = 5.0

   ! leaf angle distribution parameter
   real(r8), parameter :: chil_p(0:N_PFT+N_CFT-1) &
      = (/-0.300,  0.010,  0.010,  0.010,  0.100,  0.100,  0.010,  0.250&
         , 0.250,  0.010,  0.250,  0.250, -0.300, -0.300, -0.300, -0.300&
#ifdef CROP
         ,-0.300, -0.300, -0.300, -0.300, -0.300, -0.300, -0.300, -0.300&
         ,-0.300, -0.300, -0.300, -0.300, -0.300, -0.300, -0.300, -0.300&
         ,-0.300, -0.300, -0.300, -0.300, -0.300, -0.300, -0.300, -0.300&
         ,-0.300, -0.300, -0.300, -0.300, -0.300, -0.300, -0.300, -0.300&
         ,-0.300, -0.300, -0.300, -0.300, -0.300, -0.300, -0.300, -0.300&
         ,-0.300, -0.300, -0.300, -0.300, -0.300, -0.300, -0.300, -0.300&
         ,-0.300, -0.300, -0.300, -0.300, -0.300, -0.300, -0.300, -0.300&
         ,-0.300, -0.300, -0.300, -0.300, -0.300, -0.300, -0.300    &
#endif
         /)

   ! reflectance of green leaf in visible band
#if (defined LULC_IGBP_PC)
   ! Leaf optical properties adapted from measured data (Dong et al., 2021)
   real(r8), parameter :: rhol_vis_p(0:N_PFT+N_CFT-1) &
      = (/0.110,  0.070,  0.070,  0.070,  0.100,  0.110,  0.100,  0.100&
        , 0.100,  0.070,  0.100,  0.100,  0.110,  0.110,  0.110,  0.110&
#else
   real(r8), parameter :: rhol_vis_p(0:N_PFT+N_CFT-1) &
      = (/0.110,  0.070,  0.070,  0.070,  0.100,  0.100,  0.100,  0.100&
        , 0.100,  0.070,  0.100,  0.100,  0.110,  0.110,  0.110,  0.110&
#endif
#ifdef CROP
        , 0.110,  0.110,  0.110,  0.110,  0.110,  0.110,  0.110,  0.110&
        , 0.110,  0.110,  0.110,  0.110,  0.110,  0.110,  0.110,  0.110&
        , 0.110,  0.110,  0.110,  0.110,  0.110,  0.110,  0.110,  0.110&
        , 0.110,  0.110,  0.110,  0.110,  0.110,  0.110,  0.110,  0.110&
        , 0.110,  0.110,  0.110,  0.110,  0.110,  0.110,  0.110,  0.110&
        , 0.110,  0.110,  0.110,  0.110,  0.110,  0.110,  0.110,  0.110&
        , 0.110,  0.110,  0.110,  0.110,  0.110,  0.110,  0.110,  0.110&
        , 0.110,  0.110,  0.110,  0.110,  0.110,  0.110,  0.110    &
#endif
         /)

   ! reflectance of dead leaf in visible band
   real(r8), parameter :: rhos_vis_p(0:N_PFT+N_CFT-1) &
      = (/0.310,  0.160,  0.160,  0.160,  0.160,  0.160,  0.160,  0.160&
        , 0.160,  0.160,  0.160,  0.160,  0.310,  0.310,  0.310,  0.310&
#ifdef CROP
        , 0.310,  0.310,  0.310,  0.310,  0.310,  0.310,  0.310,  0.310&
        , 0.310,  0.310,  0.310,  0.310,  0.310,  0.310,  0.310,  0.310&
        , 0.310,  0.310,  0.310,  0.310,  0.310,  0.310,  0.310,  0.310&
        , 0.310,  0.310,  0.310,  0.310,  0.310,  0.310,  0.310,  0.310&
        , 0.310,  0.310,  0.310,  0.310,  0.310,  0.310,  0.310,  0.310&
        , 0.310,  0.310,  0.310,  0.310,  0.310,  0.310,  0.310,  0.310&
        , 0.310,  0.310,  0.310,  0.310,  0.310,  0.310,  0.310,  0.310&
        , 0.310,  0.310,  0.310,  0.310,  0.310,  0.310,  0.310    &
#endif
         /)

   ! reflectance of green leaf in near infrared band
#if (defined LULC_IGBP_PC)
   ! Leaf optical properties adapted from measured data (Dong et al., 2021)
   real(r8), parameter :: rhol_nir_p(0:N_PFT+N_CFT-1) &
      = (/0.350,  0.360,  0.370,  0.360,  0.450,  0.460,  0.450,  0.420&
        , 0.450,  0.350,  0.450,  0.450,  0.350,  0.350,  0.350,  0.350&
#else
   real(r8), parameter :: rhol_nir_p(0:N_PFT+N_CFT-1) &
      = (/0.350,  0.350,  0.350,  0.350,  0.450,  0.450,  0.450,  0.450&
        , 0.450,  0.350,  0.450,  0.450,  0.350,  0.350,  0.350,  0.350&
#endif
#ifdef CROP
        , 0.350,  0.350,  0.350,  0.350,  0.350,  0.350,  0.350,  0.350&
        , 0.350,  0.350,  0.350,  0.350,  0.350,  0.350,  0.350,  0.350&
        , 0.350,  0.350,  0.350,  0.350,  0.350,  0.350,  0.350,  0.350&
        , 0.350,  0.350,  0.350,  0.350,  0.350,  0.350,  0.350,  0.350&
        , 0.350,  0.350,  0.350,  0.350,  0.350,  0.350,  0.350,  0.350&
        , 0.350,  0.350,  0.350,  0.350,  0.350,  0.350,  0.350,  0.350&
        , 0.350,  0.350,  0.350,  0.350,  0.350,  0.350,  0.350,  0.350&
        , 0.350,  0.350,  0.350,  0.350,  0.350,  0.350,  0.350    &
#endif
         /)

   ! reflectance of dead leaf in near infrared band
   real(r8), parameter :: rhos_nir_p(0:N_PFT+N_CFT-1) &
      = (/0.530,  0.390,  0.390,  0.390,  0.390,  0.390,  0.390,  0.390&
        , 0.390,  0.390,  0.390,  0.390,  0.530,  0.530,  0.530,  0.530&
#ifdef CROP
        , 0.530,  0.530,  0.530,  0.530,  0.530,  0.530,  0.530,  0.530&
        , 0.530,  0.530,  0.530,  0.530,  0.530,  0.530,  0.530,  0.530&
        , 0.530,  0.530,  0.530,  0.530,  0.530,  0.530,  0.530,  0.530&
        , 0.530,  0.530,  0.530,  0.530,  0.530,  0.530,  0.530,  0.530&
        , 0.530,  0.530,  0.530,  0.530,  0.530,  0.530,  0.530,  0.530&
        , 0.530,  0.530,  0.530,  0.530,  0.530,  0.530,  0.530,  0.530&
        , 0.530,  0.530,  0.530,  0.530,  0.530,  0.530,  0.530,  0.530&
        , 0.530,  0.530,  0.530,  0.530,  0.530,  0.530,  0.530    &
#endif
         /)

   ! transmittance of green leaf in visible band
#if (defined LULC_IGBP_PC)
   ! Leaf optical properties adapted from measured data (Dong et al., 2021)
   real(r8), parameter :: taul_vis_p(0:N_PFT+N_CFT-1) &
      = (/0.050,  0.050,  0.050,  0.050,  0.050,  0.060,  0.050,  0.060&
        , 0.050,  0.050,  0.050,  0.050,  0.050,  0.050,  0.050,  0.050&
#else
   real(r8), parameter :: taul_vis_p(0:N_PFT+N_CFT-1) &
      = (/0.050,  0.050,  0.050,  0.050,  0.050,  0.050,  0.050,  0.050&
        , 0.050,  0.050,  0.050,  0.050,  0.050,  0.050,  0.050,  0.050&
#endif
#ifdef CROP
        , 0.050,  0.050,  0.050,  0.050,  0.050,  0.050,  0.050,  0.050&
        , 0.050,  0.050,  0.050,  0.050,  0.050,  0.050,  0.050,  0.050&
        , 0.050,  0.050,  0.050,  0.050,  0.050,  0.050,  0.050,  0.050&
        , 0.050,  0.050,  0.050,  0.050,  0.050,  0.050,  0.050,  0.050&
        , 0.050,  0.050,  0.050,  0.050,  0.050,  0.050,  0.050,  0.050&
        , 0.050,  0.050,  0.050,  0.050,  0.050,  0.050,  0.050,  0.050&
        , 0.050,  0.050,  0.050,  0.050,  0.050,  0.050,  0.050,  0.050&
        , 0.050,  0.050,  0.050,  0.050,  0.050,  0.050,  0.050    &
#endif
         /)

   ! transmittance of dead leaf in visible band
   real(r8), parameter :: taus_vis_p(0:N_PFT+N_CFT-1) &
      = (/0.120,  0.001,  0.001,  0.001,  0.001,  0.001,  0.001,  0.001&
        , 0.001,  0.001,  0.001,  0.001,  0.120,  0.120,  0.120,  0.120&
#ifdef CROP
        , 0.120,  0.120,  0.120,  0.120,  0.120,  0.120,  0.120,  0.120&
        , 0.120,  0.120,  0.120,  0.120,  0.120,  0.120,  0.120,  0.120&
        , 0.120,  0.120,  0.120,  0.120,  0.120,  0.120,  0.120,  0.120&
        , 0.120,  0.120,  0.120,  0.120,  0.120,  0.120,  0.120,  0.120&
        , 0.120,  0.120,  0.120,  0.120,  0.120,  0.120,  0.120,  0.120&
        , 0.120,  0.120,  0.120,  0.120,  0.120,  0.120,  0.120,  0.120&
        , 0.120,  0.120,  0.120,  0.120,  0.120,  0.120,  0.120,  0.120&
        , 0.120,  0.120,  0.120,  0.120,  0.120,  0.120,  0.120    &
#endif
         /)

   ! transmittance of green leaf in near infrared band
#if (defined LULC_IGBP_PC)
   ! Leaf optical properties adapted from measured data (Dong et al., 2021)
   real(r8), parameter :: taul_nir_p(0:N_PFT+N_CFT-1) &
      = (/0.340,  0.280,  0.290,  0.380,  0.250,  0.330,  0.250,  0.430&
        , 0.400,  0.100,  0.250,  0.250,  0.340,  0.340,  0.340,  0.340&
#else
   real(r8), parameter :: taul_nir_p(0:N_PFT+N_CFT-1) &
      = (/0.340,  0.100,  0.100,  0.100,  0.250,  0.250,  0.250,  0.250&
        , 0.250,  0.100,  0.250,  0.250,  0.340,  0.340,  0.340,  0.340&
#endif
#ifdef CROP
        , 0.340,  0.340,  0.340,  0.340,  0.340,  0.340,  0.340,  0.340&
        , 0.340,  0.340,  0.340,  0.340,  0.340,  0.340,  0.340,  0.340&
        , 0.340,  0.340,  0.340,  0.340,  0.340,  0.340,  0.340,  0.340&
        , 0.340,  0.340,  0.340,  0.340,  0.340,  0.340,  0.340,  0.340&
        , 0.340,  0.340,  0.340,  0.340,  0.340,  0.340,  0.340,  0.340&
        , 0.340,  0.340,  0.340,  0.340,  0.340,  0.340,  0.340,  0.340&
        , 0.340,  0.340,  0.340,  0.340,  0.340,  0.340,  0.340,  0.340&
        , 0.340,  0.340,  0.340,  0.340,  0.340,  0.340,  0.340    &
#endif
         /)

   ! transmittance of dead leaf in near infrared band
   real(r8), parameter :: taus_nir_p(0:N_PFT+N_CFT-1) &
      = (/0.250,  0.001,  0.001,  0.001,  0.001,  0.001,  0.001,  0.001&
        , 0.001,  0.001,  0.001,  0.001,  0.250,  0.250,  0.250,  0.250&
#ifdef CROP
        , 0.250,  0.250,  0.250,  0.250,  0.250,  0.250,  0.250,  0.250&
        , 0.250,  0.250,  0.250,  0.250,  0.250,  0.250,  0.250,  0.250&
        , 0.250,  0.250,  0.250,  0.250,  0.250,  0.250,  0.250,  0.250&
        , 0.250,  0.250,  0.250,  0.250,  0.250,  0.250,  0.250,  0.250&
        , 0.250,  0.250,  0.250,  0.250,  0.250,  0.250,  0.250,  0.250&
        , 0.250,  0.250,  0.250,  0.250,  0.250,  0.250,  0.250,  0.250&
        , 0.250,  0.250,  0.250,  0.250,  0.250,  0.250,  0.250,  0.250&
        , 0.250,  0.250,  0.250,  0.250,  0.250,  0.250,  0.250    &
#endif
         /)

   ! maximum carboxylation rate at 25 C at canopy top
   ! /06/03/2014/ based on Bonan et al., 2011 (Table 2)
  !real(r8), parameter :: vmax25_p(0:N_PFT+N_CFT-1) &
  !   = (/ 52.0, 61.0, 54.0, 57.0, 72.0, 72.0, 52.0, 52.0&
  !      , 52.0, 72.0, 52.0, 52.0, 52.0, 52.0, 52.0, 57.0&
  ! /07/27/2022/ based on Bonan et al., 2011 (Table 2, VmaxF(N))
  ! Temporarilly tune Vegetation parameter to match VGM model (soil too wet)
#ifdef vanGenuchten_Mualem_SOIL_MODEL
   real(r8), parameter :: vmax25_p(0:N_PFT+N_CFT-1) &
!      = (/ 52.0, 16.1, 16.5, 22.5, 12.3, 10.7, 16.2, 15.1&
!         , 15.3, 21.4, 22.0, 26.6, 34.2, 20.6, 10.0, 57.0&
!       = (/ 52.0, 56.0, 54.0, 57.0, 18.0, 23.0, 31.3, 36.1&
!          , 52.0, 40.0, 37.5, 52.0, 52.0, 52.0, 13.4, 57.0&
       = (/ 52.0, 25.2, 26.5, 34.1, 13.2, 15.7, 20.3, 24.2&
          , 28.0, 33.1, 33.4, 48.5, 55.7, 41.5, 10.0, 57.0&
#ifdef CROP
         , 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0&
         , 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0&
         , 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0&
         , 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0&
         , 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0&
         , 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0&
         , 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0&
         , 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0  &
#endif
         /) * 1.e-6
#else
   real(r8), parameter :: vmax25_p(0:N_PFT+N_CFT-1) &
!      = (/ 52.0, 55.0, 42.0, 29.0, 41.0, 51.0, 36.0, 30.0&
!         , 40.0, 36.0, 30.0, 19.0, 21.0, 26.0, 25.0, 57.0&
       = (/ 52.0, 56.0, 54.0, 57.0, 18.0, 23.0, 31.3, 36.1&
          , 52.0, 40.0, 37.5, 52.0, 52.0, 52.0, 13.4, 57.0&
#ifdef CROP
         , 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0&
         , 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0&
         , 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0&
         , 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0&
         , 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0&
         , 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0&
         , 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0&
         , 57.0, 57.0, 57.0, 57.0, 57.0, 57.0, 57.0  &
#endif
         /) * 1.e-6
#endif

   ! quantum efficiency
   real(r8), parameter :: effcon_p(0:N_PFT+N_CFT-1) &
      = (/0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08&
        , 0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.05, 0.08&
#ifdef CROP
        , 0.08, 0.05, 0.05, 0.08, 0.08, 0.08, 0.08, 0.08&
        , 0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08&
        , 0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08&
        , 0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08&
        , 0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08&
        , 0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08&
        , 0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08&
        , 0.08, 0.08, 0.08, 0.05, 0.05, 0.08, 0.08  &
#endif
         /)

      !C3C4 switch 1: C3, 0: C4
   integer, parameter :: c3c4_p(0:N_PFT+N_CFT-1) &
      = (/1, 1, 1, 1, 1, 1, 1, 1&
        , 1, 1, 1, 1, 1, 1, 0, 1&
#ifdef CROP
        , 1, 0, 0, 1, 1, 1, 1, 1&
        , 1, 1, 1, 1, 1, 1, 1, 1&
        , 1, 1, 1, 1, 1, 1, 1, 1&
        , 1, 1, 1, 1, 1, 1, 1, 1&
        , 1, 1, 1, 1, 1, 1, 1, 1&
        , 1, 1, 1, 1, 1, 1, 1, 1&
        , 1, 1, 1, 1, 1, 1, 1, 1&
        , 1, 1, 1, 0, 0, 1, 1  &
#endif
         /)

   ! conductance-photosynthesis slope parameter
   real(r8), parameter :: g1_p(0:N_PFT+N_CFT-1) &
      = (/4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0&
        , 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0&
#ifdef CROP
        , 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0&
        , 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0&
        , 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0&
        , 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0&
        , 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0&
        , 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0&
        , 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0&
        , 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0  &
#endif
         /)

   ! conductance-photosynthesis intercept
   real(r8), parameter :: g0_p(0:N_PFT+N_CFT-1) &
      = (/100, 100, 100, 100, 100, 100, 100, 100&
        , 100, 100, 100, 100, 100, 100, 100, 100&
#ifdef CROP
        , 100, 100, 100, 100, 100, 100, 100, 100&
        , 100, 100, 100, 100, 100, 100, 100, 100&
        , 100, 100, 100, 100, 100, 100, 100, 100&
        , 100, 100, 100, 100, 100, 100, 100, 100&
        , 100, 100, 100, 100, 100, 100, 100, 100&
        , 100, 100, 100, 100, 100, 100, 100, 100&
        , 100, 100, 100, 100, 100, 100, 100, 100&
        , 100, 100, 100, 100, 100, 100, 100  &
#endif
         /)

   ! conductance-photosynthesis slope parameter
   real(r8), parameter :: gradm_p(0:N_PFT+N_CFT-1) &
      = (/9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0&
        , 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 4.0, 9.0&
#ifdef CROP
        , 9.0, 4.0, 4.0, 9.0, 9.0, 9.0, 9.0, 9.0&
        , 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0&
        , 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0&
        , 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0&
        , 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0&
        , 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0&
        , 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0&
        , 9.0, 9.0, 9.0, 4.0, 4.0, 9.0, 9.0  &
#endif
         /)

   ! conductance-photosynthesis intercept
   real(r8), parameter :: binter_p(0:N_PFT+N_CFT-1) &
      = (/0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01&
        , 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.04, 0.01&
#ifdef CROP
        , 0.01, 0.04, 0.04, 0.01, 0.01, 0.01, 0.01, 0.01&
        , 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01&
        , 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01&
        , 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01&
        , 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01&
        , 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01&
        , 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01&
        , 0.01, 0.01, 0.01, 0.04, 0.04, 0.01, 0.01  &
#endif
         /)

   ! respiration fraction
   real(r8), parameter :: respcp_p(0:N_PFT+N_CFT-1) &
      = (/0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015&
        , 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.025, 0.015&
#ifdef CROP
        , 0.015, 0.025, 0.025, 0.015, 0.015, 0.015, 0.015, 0.015&
        , 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015&
        , 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015&
        , 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015&
        , 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015&
        , 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015&
        , 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015&
        , 0.015, 0.015, 0.015, 0.025, 0.025, 0.015, 0.015  &
#endif
         /)

   ! slope of high temperature inhibition FUNCTION (s1)
   real(r8), parameter :: shti_p(0:N_PFT+N_CFT-1) = 0.3

   ! slope of low temperature inhibition FUNCTION (s3)
   real(r8), parameter :: slti_p(0:N_PFT+N_CFT-1) = 0.2

   ! temperature coefficient in gs-a model (s5)
   real(r8), parameter :: trda_p(0:N_PFT+N_CFT-1) = 1.3

   ! temperature coefficient in gs-a model (s6)
   real(r8), parameter :: trdm_p(0:N_PFT+N_CFT-1) = 328.0

   ! temperature coefficient in gs-a model (273.16+25)
   real(r8), parameter :: trop_p(0:N_PFT+N_CFT-1) = 298.0

   ! 1/2 point of high temperature inhibition FUNCTION (s2)
   real(r8), parameter :: hhti_p(0:N_PFT+N_CFT-1) &
      =(/308.0, 303.0, 303.0, 303.0, 313.0, 313.0, 311.0, 311.0&
        ,311.0, 313.0, 313.0, 303.0, 303.0, 308.0, 313.0, 308.0&
#ifdef CROP
        ,308.0, 308.0, 308.0, 308.0, 308.0, 308.0, 308.0, 308.0&
        ,308.0, 308.0, 308.0, 308.0, 308.0, 308.0, 308.0, 308.0&
        ,308.0, 308.0, 308.0, 308.0, 308.0, 308.0, 308.0, 308.0&
        ,308.0, 308.0, 308.0, 308.0, 308.0, 308.0, 308.0, 308.0&
        ,308.0, 308.0, 308.0, 308.0, 308.0, 308.0, 308.0, 308.0&
        ,308.0, 308.0, 308.0, 308.0, 308.0, 308.0, 308.0, 308.0&
        ,308.0, 308.0, 308.0, 308.0, 308.0, 308.0, 308.0, 308.0&
        ,308.0, 308.0, 308.0, 308.0, 308.0, 308.0, 308.0  &
#endif
         /)

   ! 1/2 point of low temperature inhibition FUNCTION (s4)
   real(r8), parameter :: hlti_p(0:N_PFT+N_CFT-1) &
      =(/281.0, 278.0, 278.0, 278.0, 288.0, 288.0, 283.0, 283.0&
        ,283.0, 283.0, 283.0, 278.0, 278.0, 281.0, 288.0, 281.0&
#ifdef CROP
        ,281.0, 281.0, 281.0, 281.0, 281.0, 281.0, 281.0, 281.0&
        ,281.0, 281.0, 281.0, 281.0, 281.0, 281.0, 281.0, 281.0&
        ,281.0, 281.0, 281.0, 281.0, 281.0, 281.0, 281.0, 281.0&
        ,281.0, 281.0, 281.0, 281.0, 281.0, 281.0, 281.0, 281.0&
        ,281.0, 281.0, 281.0, 281.0, 281.0, 281.0, 281.0, 281.0&
        ,281.0, 281.0, 281.0, 281.0, 281.0, 281.0, 281.0, 281.0&
        ,281.0, 281.0, 281.0, 281.0, 281.0, 281.0, 281.0, 281.0&
        ,281.0, 281.0, 281.0, 281.0, 281.0, 281.0, 281.0  &
#endif
         /)

   ! coefficient of leaf nitrogen allocation
   real(r8), parameter :: extkn_p(0:N_PFT+N_CFT-1) = 0.5

   real(r8) :: &
#ifndef CROP
      rho_p(2,2,0:N_PFT-1), &!leaf reflectance
      tau_p(2,2,0:N_PFT-1)   !leaf transmittance
#else
      rho_p(2,2,0:N_PFT+N_CFT-1), &!leaf reflectance
      tau_p(2,2,0:N_PFT+N_CFT-1)   !leaf transmittance
#endif

   ! depth at 50% roots
   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: d50_p &
      =(/27.0,  21.0,  12.0,  12.0,  15.0,  23.0,  16.0,  23.0&
        ,12.0,  23.5,  23.5,  23.5,   9.0,   7.0,  16.0,  22.0&
#ifdef CROP
        ,22.0,  22.0,  22.0,  22.0,  22.0,  22.0,  22.0,  22.0&
        ,22.0,  22.0,  22.0,  22.0,  22.0,  22.0,  22.0,  22.0&
        ,22.0,  22.0,  22.0,  22.0,  22.0,  22.0,  22.0,  22.0&
        ,22.0,  22.0,  22.0,  22.0,  22.0,  22.0,  22.0,  22.0&
        ,22.0,  22.0,  22.0,  22.0,  22.0,  22.0,  22.0,  22.0&
        ,22.0,  22.0,  22.0,  22.0,  22.0,  22.0,  22.0,  22.0&
        ,22.0,  22.0,  22.0,  22.0,  22.0,  22.0,  22.0,  22.0&
        ,22.0,  22.0,  22.0,  22.0,  22.0,  22.0,  22.0   &
#endif
         /)

   ! coefficient of root profile
   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: beta_p &
      =(/-2.051, -1.835, -1.880, -1.880, -1.632, -1.757, -1.681, -1.757&
       , -1.880, -1.623, -1.623, -1.623, -2.621, -1.176, -1.452, -1.796&
#ifdef CROP
       , -1.796, -1.796, -1.796, -1.796, -1.796, -1.796, -1.796, -1.796&
       , -1.796, -1.796, -1.796, -1.796, -1.796, -1.796, -1.796, -1.796&
       , -1.796, -1.796, -1.796, -1.796, -1.796, -1.796, -1.796, -1.796&
       , -1.796, -1.796, -1.796, -1.796, -1.796, -1.796, -1.796, -1.796&
       , -1.796, -1.796, -1.796, -1.796, -1.796, -1.796, -1.796, -1.796&
       , -1.796, -1.796, -1.796, -1.796, -1.796, -1.796, -1.796, -1.796&
       , -1.796, -1.796, -1.796, -1.796, -1.796, -1.796, -1.796, -1.796&
       , -1.796, -1.796, -1.796, -1.796, -1.796, -1.796, -1.796  &
#endif
         /)

   ! woody (1) or grass (0)
   integer , parameter, dimension(0:N_PFT+N_CFT-1) :: woody &
      =(/0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0 &
#ifdef CROP
       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
       , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0    &
#endif
         /)

   ! Set the root distribution parameters of PFT
   real(r8), PRIVATE, parameter :: roota(0:N_PFT+N_CFT-1) &
      =(/  0.0,   7.0,   7.0,   7.0,   7.0,   7.0,   6.0,   6.0&
        ,  6.0,   7.0,   7.0,   7.0,  11.0,  11.0,  11.0,   6.0&
#ifdef CROP
        ,  6.0,   6.0,   6.0,   6.0,   6.0,   6.0,   6.0,   6.0&
        ,  6.0,   6.0,   6.0,   6.0,   6.0,   6.0,   6.0,   6.0&
        ,  6.0,   6.0,   6.0,   6.0,   6.0,   6.0,   6.0,   6.0&
        ,  6.0,   6.0,   6.0,   6.0,   6.0,   6.0,   6.0,   6.0&
        ,  6.0,   6.0,   6.0,   6.0,   6.0,   6.0,   6.0,   6.0&
        ,  6.0,   6.0,   6.0,   6.0,   6.0,   6.0,   6.0,   6.0&
        ,  6.0,   6.0,   6.0,   6.0,   6.0,   6.0,   6.0,   6.0&
        ,  6.0,   6.0,   6.0,   6.0,   6.0,   6.0,   6.0       &
#endif
         /)

   real(r8), PRIVATE, parameter :: rootb(0:N_PFT+N_CFT-1) &
      =(/  0.0,   2.0,   2.0,   2.0,   1.0,   1.0,   2.0,   2.0&
        ,  2.0,   1.5,   1.5,   1.5,   2.0,   2.0,   2.0,   3.0&
#ifdef CROP
        ,  3.0,   3.0,   3.0,   3.0,   3.0,   3.0,   3.0,   3.0&
        ,  3.0,   3.0,   3.0,   3.0,   3.0,   3.0,   3.0,   3.0&
        ,  3.0,   3.0,   3.0,   3.0,   3.0,   3.0,   3.0,   3.0&
        ,  3.0,   3.0,   3.0,   3.0,   3.0,   3.0,   3.0,   3.0&
        ,  3.0,   3.0,   3.0,   3.0,   3.0,   3.0,   3.0,   3.0&
        ,  3.0,   3.0,   3.0,   3.0,   3.0,   3.0,   3.0,   3.0&
        ,  3.0,   3.0,   3.0,   3.0,   3.0,   3.0,   3.0,   3.0&
        ,  3.0,   3.0,   3.0,   3.0,   3.0,   3.0,   3.0       &
#endif
         /)


!   bgc PFT constants

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: grperc = 0.11_r8


   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: grpnow = 1._r8


   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: lf_flab = 0.25_r8


   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: lf_fcel = 0.5_r8


   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: lf_flig = 0.25_r8


   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: fr_flab = 0.25_r8


   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: fr_fcel = 0.5_r8


   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: fr_flig = 0.25_r8


   logical , parameter, dimension(0:N_PFT+N_CFT-1) :: isshrub & ! True => is a shrub
      =(/.False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .True.,  .True.,  .True.,  .False., .False., .False., .False. &
#ifdef CROP
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False.      &
#endif
         /)

   logical , parameter, dimension(0:N_PFT+N_CFT-1) :: isgrass & ! True => is a grass
      =(/.False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .True.,  .True.,  .True.,  .False. &
#ifdef CROP
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False.      &
#endif
         /)

   ! True => is tropical broadleaf evergreen tree
   logical , parameter, dimension(0:N_PFT+N_CFT-1) :: isbetr  &
      =(/.False., .False., .False., .False., .True.,  .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
#ifdef CROP
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False.      &
#endif
         /)

   ! True => is a broadleaf deciduous tree
   logical , parameter, dimension(0:N_PFT+N_CFT-1) :: isbdtr  &
      =(/.False., .False., .False., .False., .False., .False., .True.,  .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
#ifdef CROP
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False.      &
#endif
         /)

   logical , parameter, dimension(0:N_PFT+N_CFT-1) :: isevg   & ! True => is a evergreen tree
      =(/.False., .True.,  .True.,  .False., .True.,  .True.,  .False., .False. &
       , .False., .True.,  .False., .False., .False., .False., .False., .False. &
#ifdef CROP
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False.      &
#endif
         /)

   ! True => is a seasonal deciduous tree
   logical , parameter, dimension(0:N_PFT+N_CFT-1) :: issed   &
      =(/.False., .False., .False., .True.,  .False., .False., .False., .True.  &
       , .True.,  .False., .False., .True.,  .True.,  .False., .False., .False. &
#ifdef CROP
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False.      &
#endif
         /)

   logical , parameter, dimension(0:N_PFT+N_CFT-1) :: isstd   & ! True => is a stress deciduous tree
      =(/.False., .False., .False., .False., .False., .False., .True.,  .False. &
       , .False., .False., .True.,  .False., .False., .True.,  .True.,  .True.  &
#ifdef CROP
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False.      &
#endif
         /)

   logical , parameter, dimension(0:N_PFT+N_CFT-1) :: isbare  & ! True => is a bare land
      =(/.True.,  .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
#ifdef CROP
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False.      &
#endif
         /)

   logical , parameter, dimension(0:N_PFT+N_CFT-1) :: iscrop  & ! True => is a crop land
      =(/.False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .True.  &
#ifdef CROP
       ,  .True.,  .True.,  .True.,  .True.,  .True.,  .True.,  .True.,  .True. &
       ,  .True.,  .True.,  .True.,  .True.,  .True.,  .True.,  .True.,  .True. &
       ,  .True.,  .True.,  .True.,  .True.,  .True.,  .True.,  .True.,  .True. &
       ,  .True.,  .True.,  .True.,  .True.,  .True.,  .True.,  .True.,  .True. &
       ,  .True.,  .True.,  .True.,  .True.,  .True.,  .True.,  .True.,  .True. &
       ,  .True.,  .True.,  .True.,  .True.,  .True.,  .True.,  .True.,  .True. &
       ,  .True.,  .True.,  .True.,  .True.,  .True.,  .True.,  .True.,  .True. &
       ,  .True.,  .True.,  .True.,  .True.,  .True.,  .True.,  .True.      &
#endif
         /)

   logical , parameter, dimension(0:N_PFT+N_CFT-1) :: isnatveg &! True => is a natural vegetation
      =(/.False., .True.,  .True.,  .True.,  .True.,  .True.,  .True.,  .True.  &
       , .True.,  .True.,  .True.,  .True.,  .True.,  .True.,  .True.,  .False. &
#ifdef CROP
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False., .False. &
       , .False., .False., .False., .False., .False., .False., .False.      &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: fsr_pft &
      =(/   0.,   0.26,   0.26,   0.26,   0.25,   0.25,   0.25,   0.25 &
       ,  0.25,   0.28,   0.28,   0.28,   0.33,   0.33,   0.33,   0.33 &
#ifdef CROP
       ,  0.33,   0.33,   0.33,   0.33,   0.33,   0.33,   0.33,   0.33 &
       ,  0.33,   0.33,   0.33,   0.33,   0.33,   0.33,   0.33,   0.33 &
       ,  0.33,   0.33,   0.33,   0.33,   0.33,   0.33,   0.33,   0.33 &
       ,  0.33,   0.33,   0.33,   0.33,   0.33,   0.33,   0.33,   0.33 &
       ,  0.33,   0.33,   0.33,   0.33,   0.33,   0.33,   0.33,   0.33 &
       ,  0.33,   0.33,   0.33,   0.33,   0.33,   0.33,   0.33,   0.33 &
       ,  0.33,   0.33,   0.33,   0.33,   0.33,   0.33,   0.33,   0.33 &
       ,  0.33,   0.33,   0.33,   0.33,   0.33,   0.33,   0.33      &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: fd_pft &
      =(/   0.,     24.,     24.,     24.,     24.,     24.,     24.,     24. &
       ,   24.,     24.,     24.,     24.,     24.,     24.,     24.,     24. &
#ifdef CROP
       ,   24.,      0.,      0.,      0.,      0.,      0.,      0.,      0. &
       ,    0.,      0.,      0.,      0.,      0.,      0.,      0.,      0. &
       ,    0.,      0.,      0.,      0.,      0.,      0.,      0.,      0. &
       ,    0.,      0.,      0.,      0.,      0.,      0.,      0.,      0. &
       ,    0.,      0.,      0.,      0.,      0.,      0.,      0.,      0. &
       ,    0.,      0.,      0.,      0.,      0.,      0.,      0.,      0. &
       ,    0.,      0.,      0.,      0.,      0.,      0.,      0.,      0. &
       ,    0.,      0.,      0.,      0.,      0.,      0.,      0.      &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: leafcn &
      =(/              1.,              58.,              58., 25.8131130614352 &
       ,  29.603315571344,  29.603315571344, 23.4521575984991, 23.4521575984991 &
       , 23.4521575984991, 36.4166059723234, 23.2558139534884, 23.2558139534884 &
       , 28.0269058295964, 28.0269058295964, 35.3606789250354, 28.0269058295964 &
#ifdef CROP
       ,              25.,              25.,              25.,              20. &
       ,              20.,              20.,              20.,              20. &
       ,              20.,              20.,              20.,              20. &
       ,              20.,              20.,              20.,              20. &
       ,              20.,              20.,              20.,              20. &
       ,              20.,              20.,              20.,              20. &
       ,              20.,              20.,              20.,              20. &
       ,              20.,              20.,              20.,              20. &
       ,              20.,              20.,              20.,              20. &
       ,              20.,              20.,              20.,              20. &
       ,              20.,              20.,              20.,              20. &
       ,              20.,              20.,              20.,              20. &
       ,              20.,              20.,              20.,              25. &
       ,              25.,              20.,              20.,              20. &
       ,              20.,              20.,              20.,              25. &
       ,              25.,              20.,              20.             &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: frootcn &
      =(/   1.,     42.,     42.,     42.,     42.,     42.,     42.,     42.&
       ,   42.,     42.,     42.,     42.,     42.,     42.,     42.,     42.&
#ifdef CROP
       ,   42.,     42.,     42.,     42.,     42.,     42.,     42.,     42.&
       ,   42.,     42.,     42.,     42.,     42.,     42.,     42.,     42.&
       ,   42.,     42.,     42.,     42.,     42.,     42.,     42.,     42.&
       ,   42.,     42.,     42.,     42.,     42.,     42.,     42.,     42.&
       ,   42.,     42.,     42.,     42.,     42.,     42.,     42.,     42.&
       ,   42.,     42.,     42.,     42.,     42.,     42.,     42.,     42.&
       ,   42.,     42.,     42.,     42.,     42.,     42.,     42.,     42.&
       ,   42.,     42.,     42.,     42.,     42.,     42.,     42.      &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: livewdcn &
      =(/   1.,     50.,     50.,     50.,     50.,     50.,     50.,     50.&
       ,   50.,     50.,     50.,     50.,      0.,      0.,      0.,      0.&
#ifdef CROP
       ,    0.,     50.,     50.,     50.,     50.,     50.,     50.,     50.&
       ,   50.,     50.,     50.,     50.,     50.,     50.,     50.,     50.&
       ,   50.,     50.,     50.,     50.,     50.,     50.,     50.,     50.&
       ,   50.,     50.,     50.,     50.,     50.,     50.,     50.,     50.&
       ,   50.,     50.,     50.,     50.,     50.,     50.,     50.,     50.&
       ,   50.,     50.,     50.,     50.,     50.,     50.,     50.,     50.&
       ,   50.,     50.,     50.,     50.,     50.,     50.,     50.,     50.&
       ,   50.,     50.,     50.,     50.,     50.,     50.,     50.      &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: deadwdcn &
      =(/   1.,    500.,    500.,    500.,    500.,    500.,    500.,    500.&
       ,  500.,    500.,    500.,    500.,      0.,      0.,      0.,      0.&
#ifdef CROP
       ,    0.,    500.,    500.,    500.,    500.,    500.,    500.,    500.&
       ,  500.,    500.,    500.,    500.,    500.,    500.,    500.,    500.&
       ,  500.,    500.,    500.,    500.,    500.,    500.,    500.,    500.&
       ,  500.,    500.,    500.,    500.,    500.,    500.,    500.,    500.&
       ,  500.,    500.,    500.,    500.,    500.,    500.,    500.,    500.&
       ,  500.,    500.,    500.,    500.,    500.,    500.,    500.,    500.&
       ,  500.,    500.,    500.,    500.,    500.,    500.,    500.,    500.&
       ,  500.,    500.,    500.,    500.,    500.,    500.,    500.      &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: graincn &
      =(/-999.,   -999.,   -999.,   -999.,   -999.,   -999.,   -999.,   -999.&
       , -999.,   -999.,   -999.,   -999.,   -999.,   -999.,   -999.,   -999.&
#ifdef CROP
       , -999.,     50.,     50.,     50.,     50.,     50.,     50.,     50.&
       ,   50.,     50.,     50.,     50.,     50.,     50.,     50.,     50.&
       ,   50.,     50.,     50.,     50.,     50.,     50.,     50.,     50.&
       ,   50.,     50.,     50.,     50.,     50.,     50.,     50.,     50.&
       ,   50.,     50.,     50.,     50.,     50.,     50.,     50.,     50.&
       ,   50.,     50.,     50.,     50.,     50.,     50.,     50.,     50.&
       ,   50.,     50.,     50.,     50.,     50.,     50.,     50.,     50.&
       ,   50.,     50.,     50.,     50.,     50.,     50.,     50.      &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: lflitcn &
      =(/   1.,     70.,     80.,     50.,     60.,     60.,     50.,     50.&
       ,   50.,     60.,     50.,     50.,     50.,     50.,     50.,     50.&
#ifdef CROP
       ,   50.,     25.,     25.,     25.,     25.,     25.,     25.,     25.&
       ,   25.,     25.,     25.,     25.,     25.,     25.,     25.,     25.&
       ,   25.,     25.,     25.,     25.,     25.,     25.,     25.,     25.&
       ,   25.,     25.,     25.,     25.,     25.,     25.,     25.,     25.&
       ,   25.,     25.,     25.,     25.,     25.,     25.,     25.,     25.&
       ,   25.,     25.,     25.,     25.,     25.,     25.,     25.,     25.&
       ,   25.,     25.,     25.,     25.,     25.,     25.,     25.,     25.&
       ,   25.,     25.,     25.,     25.,     25.,     25.,     25.      &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: leaf_long &
      =(/            0., 3.30916666666667, 3.30916666666667, 0.506666666666667&
    ,            1.4025,           1.4025, 0.48333333333333, 0.483333333333333&
    , 0.483333333333333, 1.32333333333333,             0.39,              0.39&
    , 0.320833333333333, 0.32083333333333,             0.14, 0.320833333333333&
#ifdef CROP
    ,                1.,               1.,               1.,                1.&
    ,                1.,               1.,               1.,                1.&
    ,                1.,               1.,               1.,                1.&
    ,                1.,               1.,               1.,                1.&
    ,                1.,               1.,               1.,                1.&
    ,                1.,               1.,               1.,                1.&
    ,                1.,               1.,               1.,                1.&
    ,                1.,               1.,               1.,                1.&
    ,                1.,               1.,               1.,                1.&
    ,                1.,               1.,               1.,                1.&
    ,                1.,               1.,               1.,                1.&
    ,                1.,               1.,               1.,                1.&
    ,                1.,               1.,               1.,                1.&
    ,                1.,               1.,               1.,                1.&
    ,                1.,               1.,               1.,                1.&
    ,                1.,               1.,               1.      &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: cc_leaf  &
      =(/   0.,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
#ifdef CROP
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8      &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: cc_lstem &
      =(/   0.,     0.3,     0.3,     0.3,    0.27,    0.27,    0.27,    0.27&
      ,   0.27,    0.35,    0.35,    0.35,     0.8,     0.8,     0.8,     0.8&
#ifdef CROP
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8      &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: cc_dstem &
      =(/   0.,     0.3,     0.3,     0.3,    0.27,    0.27,    0.27,    0.27&
      ,   0.27,    0.35,    0.35,    0.35,     0.8,     0.8,     0.8,     0.8&
#ifdef CROP
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8      &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: cc_other &
      =(/   0.,     0.5,     0.5,     0.5,    0.45,    0.45,    0.45,    0.45&
      ,   0.45,    0.55,    0.55,    0.55,     0.8,     0.8,     0.8,     0.8&
#ifdef CROP
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8      &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: fm_leaf  &
      =(/   0.,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
#ifdef CROP
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8      &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: fm_lstem &
      =(/   0.,     0.5,     0.5,     0.5,    0.45,    0.45,    0.35,    0.35&
      ,   0.45,    0.55,    0.55,    0.55,     0.8,     0.8,     0.8,     0.8&
#ifdef CROP
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8      &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: fm_lroot &
      =(/   0.,    0.15,    0.15,    0.15,    0.13,    0.13,     0.1,     0.1&
      ,   0.13,    0.17,    0.17,    0.17,     0.2,     0.2,     0.2,     0.2&
#ifdef CROP
      ,    0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2&
      ,    0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2&
      ,    0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2&
      ,    0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2&
      ,    0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2&
      ,    0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2&
      ,    0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2&
      ,    0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2      &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: fm_root  &
      =(/   0.,    0.15,    0.15,    0.15,    0.13,    0.13,     0.1,     0.1&
      ,   0.13,    0.17,    0.17,    0.17,     0.2,     0.2,     0.2,     0.2&
#ifdef CROP
      ,    0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2&
      ,    0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2&
      ,    0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2&
      ,    0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2&
      ,    0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2&
      ,    0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2&
      ,    0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2&
      ,    0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2      &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: fm_droot &
      =(/   0.,    0.15,    0.15,    0.15,    0.13,    0.13,     0.1,     0.1&
      ,   0.13,    0.17,    0.17,    0.17,     0.2,     0.2,     0.2,     0.2&
#ifdef CROP
      ,    0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2&
      ,    0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2&
      ,    0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2&
      ,    0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2&
      ,    0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2&
      ,    0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2&
      ,    0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2&
      ,    0.2,     0.2,     0.2,     0.2,     0.2,     0.2,     0.2      &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: fm_other &
      =(/   0.,     0.5,     0.5,     0.5,    0.45,    0.45,    0.35,    0.35&
      ,   0.45,    0.55,    0.55,    0.55,     0.8,     0.8,     0.8,     0.8&
#ifdef CROP
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8&
      ,    0.8,     0.8,     0.8,     0.8,     0.8,     0.8,     0.8      &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: froot_leaf         &
      =(/   0.,     1.5,     1.5,     1.5,     1.5,     1.5,     1.5,     1.5&
      ,    1.5,     1.5,     1.5,     1.5,     1.5,     1.5,     1.5,     1.5&
#ifdef CROP
      ,     1.,      2.,      2.,      2.,      2.,      2.,      2.,      2.&
      ,     2.,      2.,      2.,      2.,      2.,      2.,      2.,      2.&
      ,     2.,      2.,      2.,      2.,      2.,      2.,      2.,      2.&
      ,     2.,      2.,      2.,      2.,      2.,      2.,      2.,      2.&
      ,     2.,      2.,      2.,      2.,      2.,      2.,      2.,      2.&
      ,     2.,      2.,      2.,      2.,      2.,      2.,      2.,      2.&
      ,     2.,      2.,      2.,      2.,      2.,      2.,      2.,      2.&
      ,     2.,      2.,      2.,      2.,      2.,      2.,      2.      &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: croot_stem         &
      =(/  0.3,     0.3,     0.3,     0.3,     0.3,     0.3,     0.3,     0.3&
      ,    0.3,     0.3,     0.3,     0.3,      0.,      0.,      0.,      0.&
#ifdef CROP
      ,     0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.&
      ,     0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.&
      ,     0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.&
      ,     0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.&
      ,     0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.&
      ,     0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.&
      ,     0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.&
      ,     0.,      0.,      0.,      0.,      0.,      0.,      0.      &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: stem_leaf          &
      =(/   0.,     2.3,     2.3,      1.,     2.3,     1.5,      1.,     2.3&
      ,    2.3,     1.4,    0.24,    0.24,      0.,      0.,      0.,      0.&
#ifdef CROP
      ,     0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.&
      ,     0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.&
      ,     0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.&
      ,     0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.&
      ,     0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.&
      ,     0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.&
      ,     0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.&
      ,     0.,      0.,      0.,      0.,      0.,      0.,      0.      &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: flivewd            &
      =(/   0.,     0.1,     0.1,     0.1,     0.1,     0.1,     0.1,     0.1&
      ,    0.1,     0.5,     0.5,     0.1,      0.,      0.,      0.,      0.&
#ifdef CROP
      ,     0.,      1.,      1.,      1.,      1.,      1.,      1.,      1.&
      ,     1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.&
      ,     1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.&
      ,     1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.&
      ,     1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.&
      ,     1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.&
      ,     1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.&
      ,     1.,      1.,      1.,      1.,      1.,      1.,      1.      &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: fcur2              &
      =(/   0.,      1.,      1.,      0.,      1.,      1.,      0.,      0.&
      ,     0.,      1.,      0.,      0.,      0.,      0.,      0.,      0.&
#ifdef CROP
      ,     0.,      1.,      1.,      1.,      1.,      1.,      1.,      1.&
      ,     1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.&
      ,     1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.&
      ,     1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.&
      ,     1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.&
      ,     1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.&
      ,     1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.&
      ,     1.,      1.,      1.,      1.,      1.,      1.,      1.      &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: dsladlai             &
      =(/   0., 0.00125,   0.001,   0.003, 0.00122,  0.0015,  0.0027,  0.0027&
      , 0.0027,      0.,      0.,      0.,      0.,      0.,      0.,      0.&
#ifdef CROP
      ,     0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.&
      ,     0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.&
      ,     0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.&
      ,     0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.&
      ,     0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.&
      ,     0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.&
      ,     0.,      0.,      0.,      0.,      0.,      0.,      0.,      0.&
      ,     0.,      0.,      0.,      0.,      0.,      0.,      0.      &
#endif
         /)

   real(r8), parameter, dimension(0:N_PFT+N_CFT-1) :: slatop             &
      =(/   0., 0.01222, 0.01122, 0.02432, 0.03143, 0.02728, 0.03385, 0.03541&
      , 0.0447, 0.01332, 0.02255, 0.01564, 0.01077, 0.02663, 0.01983, 0.04024&
#ifdef CROP
      ,  0.035,    0.05,    0.05,   0.035,   0.035,   0.035,   0.035,   0.035&
      ,  0.035,   0.035,   0.035,   0.035,   0.035,   0.035,   0.035,   0.035&
      ,  0.035,   0.035,   0.035,   0.035,   0.035,   0.035,   0.035,   0.035&
      ,  0.035,   0.035,   0.035,   0.035,   0.035,   0.035,   0.035,   0.035&
      ,  0.035,   0.035,   0.035,   0.035,   0.035,   0.035,   0.035,   0.035&
      ,  0.035,   0.035,   0.035,   0.035,   0.035,   0.035,   0.035,   0.035&
      ,  0.035,   0.035,   0.035,    0.05,    0.05,   0.035,   0.035,   0.035&
      ,  0.035,   0.035,   0.035,    0.05,    0.05,   0.035,   0.035      &
#endif
         /)
!--- crop variables ---
   ! Max fertilizer to be applied in total (kg N/m2)
   real(r8),parameter, dimension(0:N_PFT+N_CFT-1) :: manure  &
      = (/  0.,     0.,     0.,     0.,     0.,     0.,     0.,     0. &
      ,     0.,     0.,     0.,     0.,     0.,     0.,     0.,     0. &
#ifdef CROP
      ,     0., 0.0020, 0.0020, 0.0020, 0.0020, 0.0020, 0.0020, 0.0020 &
      , 0.0020, 0.0020, 0.0020, 0.0020, 0.0020, 0.0020, 0.0020, 0.0020 &
      , 0.0020,     0.,     0.,     0.,     0.,     0.,     0.,     0. &
      ,     0., 0.0020, 0.0020,     0.,     0.,     0.,     0.,     0. &
      ,     0.,     0.,     0.,     0.,     0.,     0.,     0.,     0. &
      ,     0.,     0.,     0.,     0.,     0., 0.0020, 0.0020,     0. &
      ,     0.,     0.,     0., 0.0020, 0.0020,     0.,     0.,     0. &
      ,     0.,     0.,     0., 0.0020, 0.0020, 0.0020, 0.0020      &
#endif
         /)

   real(r8),parameter, dimension(0:N_PFT+N_CFT-1) :: lfemerg   & ! parameter used in CNPhenology
      = (/-999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
#ifdef CROP
      ,   -999.9,   0.11,   0.11,   0.07,   0.07,   0.03,   0.03,   0.15 &
      ,     0.15,   0.07,   0.07,   0.03,   0.03,   0.07,   0.07,   0.03 &
      ,     0.03, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9,   0.07,   0.07, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9,   0.12,   0.12, -999.9 &
      ,   -999.9, -999.9, -999.9,   0.11,   0.11, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9,   0.11,   0.11,   0.15,   0.15      &
#endif
         /)

   integer, parameter, dimension(0:N_PFT+N_CFT-1) :: mxmat   & ! parameter used in CNPhenology
      = (/-999, -999, -999, -999, -999, -99 , -999, -999 &
      ,   -999, -999, -999, -999, -999, -999, -999, -999 &
#ifdef CROP
      ,   -999,  150,  150,  150,  150,  270,  270,  150 &
      ,    150,  150,  150,  270,  270,  150,  150,  270 &
      ,    270, -999, -999, -999, -999, -999, -999, -999 &
      ,   -999,  150,  150, -999, -999, -999, -999, -999 &
      ,   -999, -999, -999, -999, -999, -999, -999, -999 &
      ,   -999, -999, -999, -999, -999,  150,  150, -999 &
      ,   -999, -999, -999,  300,  300, -999, -999, -999 &
      ,   -999, -999, -999,  150,  150,  150,  150      &
#endif
         /)

   real(r8),parameter, dimension(0:N_PFT+N_CFT-1) :: grnfill  & ! parameter used in CNPhenology
      = (/-999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
#ifdef CROP
      ,   -999.9,   0.64,   0.64,    0.6,    0.6,   0.67,   0.67,   0.69 &
      ,     0.69,    0.6,    0.6,   0.67,   0.67,    0.6,    0.6,   0.67 &
      ,     0.67, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9,    0.6,    0.6, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9,   0.68,   0.68, -999.9 &
      ,   -999.9, -999.9, -999.9,   0.64,   0.64, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9,   0.64,   0.64,   0.69,   0.69      &
#endif
         /)


   real(r8),parameter, dimension(0:N_PFT+N_CFT-1) :: baset   & ! parameter used in accFlds
      = (/0.,  0.,  0.,  0.,  0.,  0.,  0.,  0. &
      ,   0.,  0.,  0.,  0.,  0.,  0.,  0.,  0. &
#ifdef CROP
      ,   0.,  8.,  8.,  0.,  0.,  0.,  0., 10. &
      ,   10., 0.,  0.,  0.,  0.,  0.,  0.,  0. &
      ,   0.,  0.,  0.,  0.,  0.,  0.,  0.,  0. &
      ,   0., 10., 10.,  0.,  0.,  0.,  0.,  0. &
      ,   0.,  0.,  0.,  0.,  0.,  0.,  0.,  0. &
      ,   0.,  0.,  0.,  0.,  0., 10., 10.,  0. &
      ,   0.,  0.,  0., 10., 10.,  0.,  0.,  0. &
      ,   0.,  0.,  0.,  8.,  8., 10., 10.      &
#endif
         /)

   real(r8),parameter, dimension(0:N_PFT+N_CFT-1) :: astemf  & ! parameter used in CNAllocation
      = (/-999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
#ifdef CROP
      ,   -999.9,    0.0,    0.0,   0.05,   0.05,   0.05,   0.05,   0.05 &
      ,     0.05,   0.05,   0.05,   0.05,   0.05,   0.05,   0.05,   0.05 &
      ,     0.05, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9,    0.3,    0.3, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9,   0.05,   0.05, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9,    0.0,    0.0,   0.05,   0.05      &
#endif
         /)

   real(r8),parameter, dimension(0:N_PFT+N_CFT-1) :: arooti  & ! parameter used in CNAllocation
      = (/-999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
#ifdef CROP
      ,   -999.9,    0.4,    0.4,    0.1,    0.1,    0.1,    0.1,    0.2 &
      ,      0.2,    0.1,    0.1,    0.1,    0.1,    0.1,    0.1,    0.1 &
      ,      0.1, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9,    0.1,    0.1, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9,    0.1,    0.1, -999.9 &
      ,   -999.9, -999.9, -999.9,    0.4,    0.4, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9,    0.4,    0.4,    0.2,    0.2      &
#endif
         /)

   real(r8),parameter, dimension(0:N_PFT+N_CFT-1) :: arootf  & ! parameter used in CNAllocation
      = (/-999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
#ifdef CROP
      ,   -999.9,   0.05,   0.05,    0.0,    0.0,    0.0,    0.0,    0.1 &
      ,      0.1,    0.0,    0.0,    0.0,    0.0,    0.0,    0.0,    0.0 &
      ,      0.0, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9,    0.2,    0.2, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9,    0.0,    0.0, -999.9 &
      ,   -999.9, -999.9, -999.9,   0.05,   0.05, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9,   0.05,   0.05,    0.1,    0.1      &
#endif
         /)

   real(r8),parameter, dimension(0:N_PFT+N_CFT-1) ::fleafi   & ! parameter used in CNAllocation
      = (/-999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
#ifdef CROP
      ,   -999.9,    0.8,    0.8,    0.9,    0.9,    0.9,    0.9,    0.9 &
      ,      0.9,   0.85,   0.85,    0.9,    0.9,    0.9,    0.9,    0.9 &
      ,      0.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9,   0.85,   0.85, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9,   0.75,   0.75, -999.9 &
      ,   -999.9, -999.9, -999.9,    0.8,    0.8, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9,    0.8,    0.8,   0.85,   0.85      &
#endif
         /)

   real(r8),parameter, dimension(0:N_PFT+N_CFT-1) :: bfact   & ! parameter used in CNAllocation
      = (/-999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
#ifdef CROP
      ,   -999.9,    0.1,    0.1,    0.1,    0.1,    0.1,    0.1,    0.1 &
      ,      0.1,    0.1,    0.1,    0.1,    0.1,    0.1,    0.1,    0.1 &
      ,      0.1,    0.1,    0.1,    0.1,    0.1,    0.1,    0.1,    0.1 &
      ,      0.1,    0.1,    0.1,    0.1,    0.1,    0.1,    0.1,    0.1 &
      ,      0.1,    0.1,    0.1,    0.1,    0.1,    0.1,    0.1,    0.1 &
      ,      0.1,    0.1,    0.1,    0.1,    0.1,    0.1,    0.1,    0.1 &
      ,      0.1,    0.1,    0.1,    0.1,    0.1,    0.1,    0.1,    0.1 &
      ,      0.1,    0.1,    0.1,    0.1,    0.1,    0.1,    0.1      &
#endif
         /)

   real(r8),parameter, dimension(0:N_PFT+N_CFT-1) :: declfact & ! parameter used in CNAllocation
      = (/-999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
#ifdef CROP
      ,   -999.9,   1.05,   1.05,   1.05,   1.05,   1.05,   1.05,   1.05 &
      ,     1.05,   1.05,   1.05,   1.05,   1.05,   1.05,   1.05,   1.05 &
      ,     1.05,   1.05,   1.05,   1.05,   1.05,   1.05,   1.05,   1.05 &
      ,     1.05,   1.05,   1.05,   1.05,   1.05,   1.05,   1.05,   1.05 &
      ,     1.05,   1.05,   1.05,   1.05,   1.05,   1.05,   1.05,   1.05 &
      ,     1.05,   1.05,   1.05,   1.05,   1.05,   1.05,   1.05,   1.05 &
      ,     1.05,   1.05,   1.05,   1.05,   1.05,   1.05,   1.05,   1.05 &
      ,     1.05,   1.05,   1.05,   1.05,   1.05,   1.05,   1.05      &
#endif
         /)

   real(r8),parameter, dimension(0:N_PFT+N_CFT-1) :: allconss & ! parameter used in CNAllocation
      = (/-999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
#ifdef CROP
      ,   -999.9,     2.,     2.,     1.,     1.,     1.,     1.,     5. &
      ,       5.,     1.,     1.,     1.,     1.,     1.,     1.,     1. &
      ,       1., -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9,     5.,     5., -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9,     1.,     1., -999.9 &
      ,   -999.9, -999.9, -999.9,     2.,     2., -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9,     2.,     2.,     5.,     5.      &
#endif
         /)

   real(r8),parameter, dimension(0:N_PFT+N_CFT-1) :: allconsl & ! parameter used in CNAllocation
      = (/-999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
#ifdef CROP
      ,   -999.9,     5.,     5.,     3.,     3.,     3.,     3.,     2. &
      ,       2.,     3.,     3.,     3.,     3.,     3.,     3.,     3. &
      ,       3., -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9,     2.,     2., -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9, -999.9, -999.9,     3.,     3., -999.9 &
      ,   -999.9, -999.9, -999.9,     5.,     5., -999.9, -999.9, -999.9 &
      ,   -999.9, -999.9, -999.9,     5.,     5.,     2.,     2.      &
#endif
         /)


   real(r8),parameter, dimension(0:N_PFT+N_CFT-1) :: fleafcn & ! C:N during grain fill; leaf
      = (/999., 999., 999., 999., 999., 999., 999., 999. &
      ,   999., 999., 999., 999., 999., 999., 999., 999. &
#ifdef CROP
      ,   999.,  65.,  65.,  65.,  65.,  65.,  65.,  65. &
      ,    65.,  65.,  65.,  65.,  65.,  65.,  65.,  65. &
      ,    65.,  65.,  65.,  65.,  65.,  65.,  65.,  65. &
      ,    65.,  65.,  65.,  65.,  65.,  65.,  65.,  65. &
      ,    65.,  65.,  65.,  65.,  65.,  65.,  65.,  65. &
      ,    65.,  65.,  65.,  65.,  65.,  65.,  65.,  65. &
      ,    65.,  65.,  65.,  65.,  65.,  65.,  65.,  65. &
      ,    65.,  65.,  65.,  65.,  65.,  65.,  65.      &
#endif
         /)

   real(r8),parameter, dimension(0:N_PFT+N_CFT-1) :: fstemcn & ! C:N during grain fill; stem
      = (/999., 999., 999., 999., 999., 999., 999., 999. &
      ,   999., 999., 999., 999., 999., 999., 999., 999. &
#ifdef CROP
      ,   999., 120., 120., 100., 100., 100., 100., 130. &
      ,   130., 100., 100., 100., 100., 100., 100., 100. &
      ,   100., 999., 999., 999., 999., 999., 999., 999. &
      ,   999., 130., 130., 999., 999., 999., 999., 999. &
      ,   999., 999., 999., 999., 999., 999., 999., 999. &
      ,   999., 999., 999., 999., 999., 100., 100., 999. &
      ,   999., 999., 999., 120., 120., 999., 999., 999. &
      ,   999., 999., 999., 120., 120., 130., 130.      &
#endif
         /)

   real(r8),parameter, dimension(0:N_PFT+N_CFT-1) :: ffrootcn & ! C:N during grain fill; fine root
      = (/999., 999., 999., 999., 999., 999., 999., 999. &
        , 999., 999., 999., 999., 999., 999., 999., 999. &
#ifdef CROP
        , 999.,   0.,   0.,  40.,  40.,  40.,  40.,   0. &
        ,   0.,  40.,  40.,  40.,  40.,  40.,  40.,  40. &
        ,  40., 999., 999., 999., 999., 999., 999., 999. &
        , 999.,   0.,   0., 999., 999., 999., 999., 999. &
        , 999., 999., 999., 999., 999., 999., 999., 999. &
        , 999., 999., 999., 999., 999.,  40.,  40., 999. &
        , 999., 999., 999.,   0.,   0., 999., 999., 999. &
        , 999., 999., 999.,   0.,   0.,   0.,   0.       &
#endif
         /)

   real(r8),parameter, dimension(0:N_PFT+N_CFT-1) :: laimx    & ! maximum leaf area index
      = (/-999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
        , -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
#ifdef CROP
        , -999.9,     5.,     5.,     7.,     7.,     7.,     7.,     6. &
        ,     6.,     7.,     7.,     7.,     7.,     7.,     7.,     7. &
        ,     7., -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
        , -999.9,     6.,     6., -999.9, -999.9, -999.9, -999.9, -999.9 &
        , -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9, -999.9 &
        , -999.9, -999.9, -999.9, -999.9, -999.9,     7.,     7., -999.9 &
        , -999.9, -999.9, -999.9,     5.,     5., -999.9, -999.9, -999.9 &
        , -999.9, -999.9, -999.9,     5.,     5.,     6.,      6.        &
#endif
         /)
#ifdef CROP
   integer, parameter, dimension(0:N_PFT+N_CFT-1) :: mergetoclmpft & ! merge crop functional types
      = (/0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18 &
      ,  19, 20, 21, 22, 23, 24, 19, 20, 21, 22, 19, 20, 21, 22, 61, 62, 19, 20, 61 &
      ,  62, 61, 62, 41, 42, 41, 42, 19, 20, 19, 20, 61, 62, 75, 76, 61, 62, 19, 20 &
      ,  19, 20, 19, 20, 61, 62, 75, 76, 19, 20, 67, 68, 19, 20, 75, 76, 75, 76, 75 &
      ,  76, 77, 78/)
#endif
!   end bgc variables

! Plant Hydraulics Parameters
   real(r8), parameter :: kmax_sun_p(0:N_PFT+N_CFT-1) &
      = (/     0.,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
#ifdef CROP
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
#endif
         /)

   real(r8), parameter :: kmax_sha_p(0:N_PFT+N_CFT-1) &
      = (/     0.,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
#ifdef CROP
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
#endif
         /)
   real(r8), parameter :: kmax_xyl_p(0:N_PFT+N_CFT-1) &
      = (/     0.,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
#ifdef CROP
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
#endif
         /)

   real(r8), parameter :: kmax_root_p(0:N_PFT+N_CFT-1) &
      = (/     0.,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
#ifdef CROP
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
         ,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007,1.e-007&
#endif
         /)

   ! water potential at 50% loss of sunlit leaf tissue conductance (mmH2O)
   real(r8), parameter :: psi50_sun_p(0:N_PFT+N_CFT-1) &
      = (/-150000, -530000, -400000, -380000, -250000, -270000, -340000, -270000&
         ,-200000, -400000, -390000, -390000, -340000, -340000, -340000, -340000&
#ifdef CROP
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000&
#endif
         /)

   ! water potential at 50% loss of shaded leaf tissue conductance (mmH2O)
   real(r8), parameter :: psi50_sha_p(0:N_PFT+N_CFT-1) &
      = (/-150000, -530000, -400000, -380000, -250000, -270000, -340000, -270000&
         ,-200000, -400000, -390000, -390000, -340000, -340000, -340000, -340000&
#ifdef CROP
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000&
#endif
         /)

   ! water potential at 50% loss of xylem tissue conductance (mmH2O)
   real(r8), parameter :: psi50_xyl_p(0:N_PFT+N_CFT-1) &
      = (/-200000, -530000, -400000, -380000, -250000, -270000, -340000, -270000&
         ,-200000, -400000, -390000, -390000, -340000, -340000, -340000, -340000&
#ifdef CROP
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000&
#endif
         /)

   ! water potential at 50% loss of root tissue conductance (mmH2O)
   real(r8), parameter :: psi50_root_p(0:N_PFT+N_CFT-1) &
      = (/-200000, -530000, -400000, -380000, -250000, -270000, -340000, -270000&
         ,-200000, -400000, -390000, -390000, -340000, -340000, -340000, -340000&
#ifdef CROP
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000, -340000&
         ,-340000, -340000, -340000, -340000, -340000, -340000, -340000&
#endif
         /)

   ! shape-fitting parameter for vulnerability curve (-)
   real(r8), parameter :: ck_p(0:N_PFT+N_CFT-1) &
      = (/3.95,  3.95, 3.95,  3.95, 3.95,  3.95, 3.95, 3.95&
         ,3.95,  3.95, 3.95,  3.95, 3.95,  3.95, 3.95, 3.95&
#ifdef CROP
         ,3.95,  3.95, 3.95,  3.95, 3.95,  3.95, 3.95, 3.95&
         ,3.95,  3.95, 3.95,  3.95, 3.95,  3.95, 3.95, 3.95&
         ,3.95,  3.95, 3.95,  3.95, 3.95,  3.95, 3.95, 3.95&
         ,3.95,  3.95, 3.95,  3.95, 3.95,  3.95, 3.95, 3.95&
         ,3.95,  3.95, 3.95,  3.95, 3.95,  3.95, 3.95, 3.95&
         ,3.95,  3.95, 3.95,  3.95, 3.95,  3.95, 3.95, 3.95&
         ,3.95,  3.95, 3.95,  3.95, 3.95,  3.95, 3.95, 3.95&
         ,3.95,  3.95, 3.95,  3.95, 3.95,  3.95, 3.95&
#endif
         /)
!end plant hydraulic parameters

  ! Temporally tune Vegetation parameter to match VGM model (soil too wet)
#ifdef vanGenuchten_Mualem_SOIL_MODEL
   real(r8), parameter :: lambda_p(0:N_PFT+N_CFT-1) &
      = (/1000.,   222.,  383.,   467., 2500.,   500.,  737.,  428.&
         , 199.,   749.,  751.,   200.,  150.,   480.,  800., 1000.&
#ifdef CROP
         ,1000.,  1000., 1000.,  1000., 1000.,  1000., 1000., 1000.&
         ,1000.,  1000., 1000.,  1000., 1000.,  1000., 1000., 1000.&
         ,1000.,  1000., 1000.,  1000., 1000.,  1000., 1000., 1000.&
         ,1000.,  1000., 1000.,  1000., 1000.,  1000., 1000., 1000.&
         ,1000.,  1000., 1000.,  1000., 1000.,  1000., 1000., 1000.&
         ,1000.,  1000., 1000.,  1000., 1000.,  1000., 1000., 1000.&
         ,1000.,  1000., 1000.,  1000., 1000.,  1000., 1000., 1000.&
         ,1000.,  1000., 1000.,  1000., 1000.,  1000., 1000.&
#endif
         /)
#else
   real(r8), parameter :: lambda_p(0:N_PFT+N_CFT-1) &
      = (/1000.,  1000., 1000.,  1000., 1000.,  1000., 1000., 1000.&
         ,1000.,  1000., 1000.,  1000., 1000.,  1000., 1000., 1000.&
#ifdef CROP
         ,1000.,  1000., 1000.,  1000., 1000.,  1000., 1000., 1000.&
         ,1000.,  1000., 1000.,  1000., 1000.,  1000., 1000., 1000.&
         ,1000.,  1000., 1000.,  1000., 1000.,  1000., 1000., 1000.&
         ,1000.,  1000., 1000.,  1000., 1000.,  1000., 1000., 1000.&
         ,1000.,  1000., 1000.,  1000., 1000.,  1000., 1000., 1000.&
         ,1000.,  1000., 1000.,  1000., 1000.,  1000., 1000., 1000.&
         ,1000.,  1000., 1000.,  1000., 1000.,  1000., 1000., 1000.&
         ,1000.,  1000., 1000.,  1000., 1000.,  1000., 1000.&
#endif
         /)
#endif
   ! irrigation parameter for irrigated crop
   logical , parameter :: irrig_crop(0:N_PFT+N_CFT-1)  &
           =(/.False., .False., .False., .False., .False., .False., .False., .False. &
            , .False., .False., .False., .False., .False., .False., .False., .False. &
#ifdef CROP
            , .True., .False., .True., .False., .True., .False., .True., .False. &
            , .True., .False., .True., .False., .True., .False., .True., .False. &
            , .True., .False., .True., .False., .True., .False., .True., .False. &
            , .True., .False., .True., .False., .True., .False., .True., .False. &
            , .True., .False., .True., .False., .True., .False., .True., .False. &
            , .True., .False., .True., .False., .True., .False., .True., .False. &
            , .True., .False., .True., .False., .True., .False., .True., .False. &
            , .True., .False., .True., .False., .True., .False., .True.          &
#endif
            /)


   ! scheme 1: Schenk and Jackson, 2002, 2: Zeng 2001
   integer, PRIVATE :: ROOTFR_SCHEME = 1

   !fraction of roots in each soil layer
#ifdef CROP
   real(r8), dimension(nl_soil,N_PFT+N_CFT) :: &
      rootfr_p(1:nl_soil, 0:N_PFT+N_CFT-1)
#else
   real(r8), dimension(nl_soil,N_PFT) :: &
      rootfr_p(1:nl_soil, 0:N_PFT-1)
#endif

   integer, PRIVATE :: i, nsl


   ! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: Init_PFT_Const

CONTAINS

   SUBROUTINE Init_PFT_Const

   IMPLICIT NONE

      rho_p(1,1,:) = rhol_vis_p(:)
      rho_p(2,1,:) = rhol_nir_p(:)
      rho_p(1,2,:) = rhos_vis_p(:)
      rho_p(2,2,:) = rhos_nir_p(:)
      tau_p(1,1,:) = taul_vis_p(:)
      tau_p(2,1,:) = taul_nir_p(:)
      tau_p(1,2,:) = taus_vis_p(:)
      tau_p(2,2,:) = taus_nir_p(:)

IF (ROOTFR_SCHEME == 1) THEN
#ifdef CROP
      DO i = 0, N_PFT+N_CFT-1
#else
      DO i = 0, N_PFT-1
#endif
         rootfr_p(1,i)=1./(1.+(zi_soi(1)*100./d50_p(i))**beta_p(i))
         rootfr_p(nl_soil,i)=1.-1./(1.+(zi_soi(nl_soil-1)*100./d50_p(i))**beta_p(i))

         DO nsl=2,nl_soil-1
            rootfr_p(nsl,i)=1./(1.+(zi_soi(nsl)*100./d50_p(i))**beta_p(i)) &
               -1./(1.+(zi_soi(nsl-1)*100./d50_p(i))**beta_p(i))
         ENDDO
      ENDDO
ELSE
      ! PFT rootfr_p (Zeng, 2001)
#ifdef CROP
      DO i = 0, N_PFT+N_CFT-1
#else
      DO i = 0, N_PFT-1
#endif
         rootfr_p(1,i) = 1. - 0.5*( &
              exp(-roota(i) * zi_soi(1)) &
            + exp(-rootb(i) * zi_soi(1)) )

         rootfr_p(nl_soil,i) = 0.5*( &
              exp(-roota(i) * zi_soi(nl_soil)) &
            + exp(-rootb(i) * zi_soi(nl_soil)) )

         DO nsl = 2, nl_soil-1
            rootfr_p(nsl,i) = 0.5*( &
                 exp(-roota(i) * zi_soi(nsl-1)) &
               + exp(-rootb(i) * zi_soi(nsl-1)) &
               - exp(-roota(i) * zi_soi(nsl)) &
               - exp(-rootb(i) * zi_soi(nsl)) )
         ENDDO
      ENDDO
ENDIF


   END SUBROUTINE Init_PFT_Const

END MODULE MOD_Const_PFT
! ---------- EOP ------------
