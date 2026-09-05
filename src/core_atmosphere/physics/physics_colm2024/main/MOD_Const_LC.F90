#include <define.h>

MODULE MOD_Const_LC

!-----------------------------------------------------------------------
! !DESCRIPTION:
!  Constant values set for land cover types
!
!  Created by Hua Yuan, 08/2019
!
! !REVISIONS:
!  08/2019, Hua Yuan: initial version adapted from IniTimeConst.F90 of CoLM2014
!  08/2019, Hua Yuan: added constants values for IGBP land cover types
!  05/2023, Xingjie Lu: added Plant Hydraulics Parameters
!
!-----------------------------------------------------------------------
! !USES:
   USE MOD_Precision
   USE MOD_Vars_Global
   USE MOD_Namelist, only: DEF_USE_PLANTHYDRAULICS

   IMPLICIT NONE
   SAVE

#ifdef LULC_USGS

! GLCC USGS Land Use/Land Cover System Legend
!-----------------------------------------------------------------------
! 0  Ocean
! 1  Urban and Built-Up Land
! 2  Dryland Cropland and Pasture
! 3  Irrigated Cropland and Pasture
! 4  Mixed Dryland/Irrigated Cropland and Pasture
! 5  Cropland/Grassland Mosaic
! 6  Cropland/Woodland Mosaic
! 7  Grassland
! 8  Shrubland
! 9  Mixed Shrubland/Grassland
!10  Savanna
!11  Deciduous Broadleaf Forest
!12  Deciduous Needleleaf Forest
!13  Evergreen Broadleaf Forest
!14  Evergreen Needleleaf Forest
!15  Mixed Forest
!16  Inland Water
!17  Herbaceous Wetland
!18  Wooded Wetland
!19  Barren or Sparsely Vegetated
!20  Herbaceous Tundra
!21  Wooded Tundra
!22  Mixed Tundra
!23  Bare Ground Tundra
!24  Snow or Ice

   character(len=256) :: patchclassname (0:N_land_classification) = &
      (/'0 Ocean                                       ', &
        '1 Urban and Built-Up Land                     ', &
        '2 Dryland Cropland and Pasture                ', &
        '3 Irrigated Cropland and Pasture              ', &
        '4 Mixed Dryland/Irrigated Cropland and Pasture', &
        '5 Cropland/Grassland Mosaic                   ', &
        '6 Cropland/Woodland Mosaic                    ', &
        '7 Grassland                                   ', &
        '8 Shrubland                                   ', &
        '9 Mixed Shrubland/Grassland                   ', &
        '10 Savanna                                    ', &
        '11 Deciduous Broadleaf Forest                 ', &
        '12 Deciduous Needleleaf Forest                ', &
        '13 Evergreen Broadleaf Forest                 ', &
        '14 Evergreen Needleleaf Forest                ', &
        '15 Mixed Forest                               ', &
        '16 Inland Water                               ', &
        '17 Herbaceous Wetland                         ', &
        '18 Wooded Wetland                             ', &
        '19 Barren or Sparsely Vegetated               ', &
        '20 Herbaceous Tundra                          ', &
        '21 Wooded Tundra                              ', &
        '22 Mixed Tundra                               ', &
        '23 Bare Ground Tundra                         ', &
        '24 Snow or Ice                                '/)

   ! land patch types
   ! 0: soil, 1: urban, 2: wetland, 3: ice, 4: lake
   integer , parameter, dimension(N_land_classification) :: patchtypes_usgs &
      = (/1, 0, 0, 0, 0, 0, 0, 0,&
          0, 0, 0, 0, 0, 0, 0, 4,&
          2, 2, 0, 0, 0, 0, 0, 3/)

   ! Look-up table canopy top height
   !NOTE: now read from input NetCDF file
   !NOTE: woody wetland 35m?
   ! shrub land 0.5m? grass like land 1m? all set to 0.5
   real(r8), parameter, dimension(N_land_classification) :: htop0_usgs &
     !=(/ 1.0,   1.0,   1.0,   1.0,   1.0,   1.0,   1.0,   0.5,&
     !    0.5,   1.0,  20.0,  17.0,  35.0,  17.0,  20.0,   1.0,&
     !    1.0,  35.0,   0.5,   1.0,   1.0,   1.0,   1.0,   1.0/)
      =(/ 1.0,   0.5,   0.5,   0.5,   0.5,   0.5,   0.5,   0.5,&
          0.5,   0.5,  20.0,  17.0,  35.0,  17.0,  20.0,   0.5,&
          0.5,  17.0,   0.5,   0.5,   0.5,   0.5,   0.5,   0.5/)

   ! Look-up table canopy bottom height
   ! 01/06/2020, yuan: adjust hbot: grass/shrub -> 0, tree->1
   real(r8), parameter, dimension(N_land_classification) :: hbot0_usgs &
     !=(/0.01,  0.01,  0.01,  0.01,  0.01,  0.01,  0.01,   0.1,&
     !    0.1,   0.1,  11.5,   8.5,   1.0,   8.5,  10.0,   0.1,&
     !    0.1,   1.0,   0.1,  0.01,  0.01,  0.01,  0.01,   0.01/)
      =(/ 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,&
          0.0,   0.0,   1.0,   1.0,   1.0,   1.0,   1.0,   0.0,&
          0.0,   1.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0/)

   ! defulat vegetation fractional cover
   real(r8), parameter, dimension(N_land_classification) :: fveg0_usgs &
      = 1.0 !(/.../)

   ! Look-up table stem area index
   !NOTE: now read from input NetCDF file
   real(r8), parameter, dimension(N_land_classification) :: sai0_usgs &
     !=(/0.2, 0.2, 0.3, 0.3, 0.5, 0.5, 1.0, 0.5,&
     !   1.0, 0.5, 2.0, 2.0, 2.0, 2.0, 2.0, 0.0,&
     !   2.0, 2.0, 0.0, 0.1, 0.1, 0.1, 0.0, 0.0/)
      =(/0.2, 0.2, 0.3, 0.3, 0.5, 0.5, 1.0, 0.5,&
         1.0, 0.5, 2.0, 2.0, 2.0, 2.0, 2.0, 0.0,&
         0.2, 2.0, 0.2, 0.2, 0.2, 0.2, 0.0, 0.0/)

   ! ratio to calculate roughness length z0m
   real(r8), parameter, dimension(N_land_classification) :: z0mr_usgs = 0.1

   ! ratio to calculate displacement height d
   real(r8), parameter, dimension(N_land_classification) :: displar_usgs = 0.667

   ! inverse sqrt of leaf dimension [m**-0.5, m=4 cm]
   real(r8), parameter, dimension(N_land_classification) :: sqrtdi_usgs = 5.0

   ! leaf angle distribution parameter
   real(r8), parameter, dimension(N_land_classification) :: chil_usgs &
      = (/-0.300, -0.300, -0.300, -0.300, -0.300, -0.300, -0.300,  0.010,&
           0.010, -0.300,  0.250,  0.010,  0.100,  0.010,  0.125, -0.300,&
          -0.300,  0.100,  0.010, -0.300, -0.300, -0.300, -0.300, -0.300/)

   ! reflectance of green leaf in visible band
   real(r8), parameter, dimension(N_land_classification) :: rhol_vis_usgs &
      = (/0.105,  0.105,  0.105,  0.105,  0.105,  0.105,  0.105,  0.100,&
          0.100,  0.105,  0.100,  0.070,  0.100,  0.070,  0.070,  0.105,&
          0.105,  0.100,  0.100,  0.105,  0.105,  0.105,  0.105,  0.105/)

   ! reflectance of dead leaf in visible band
   real(r8), parameter, dimension(N_land_classification) :: rhos_vis_usgs &
      = (/0.360,  0.360,  0.360,  0.360,  0.360,  0.360,  0.360,  0.160,&
          0.160,  0.360,  0.160,  0.160,  0.160,  0.160,  0.160,  0.360,&
          0.360,  0.160,  0.160,  0.360,  0.360,  0.360,  0.360,  0.360/)

   ! reflectance of green leaf in near infrared band
   real(r8), parameter, dimension(N_land_classification) :: rhol_nir_usgs &
      = (/0.580,  0.580,  0.580,  0.580,  0.580,  0.580,  0.580,  0.450,&
          0.450,  0.580,  0.450,  0.350,  0.450,  0.350,  0.400,  0.580,&
          0.580,  0.450,  0.450,  0.580,  0.580,  0.580,  0.580,  0.580/)

   ! reflectance of dead leaf in near infrared band
   real(r8), parameter, dimension(N_land_classification) :: rhos_nir_usgs &
      = (/0.580,  0.580,  0.580,  0.580,  0.580,  0.580,  0.580,  0.390,&
          0.390,  0.580,  0.390,  0.390,  0.390,  0.390,  0.390,  0.580,&
          0.580,  0.390,  0.390,  0.580,  0.580,  0.580,  0.580,  0.580/)

   ! transmittance of green leaf in visible band
   real(r8), parameter, dimension(N_land_classification) :: taul_vis_usgs &
      = (/0.070,  0.070,  0.070,  0.070,  0.070,  0.070,  0.070,  0.070,&
          0.070,  0.070,  0.050,  0.050,  0.050,  0.050,  0.050,  0.070,&
          0.070,  0.050,  0.070,  0.070,  0.070,  0.070,  0.070,  0.070/)

   ! transmittance of dead leaf in visible band
   real(r8), parameter, dimension(N_land_classification) :: taus_vis_usgs &
      = (/0.220,  0.220,  0.220,  0.220,  0.220,  0.220,  0.220,  0.001,&
          0.001,  0.220,  0.001,  0.001,  0.001,  0.001,  0.001,  0.220,&
          0.220,  0.001,  0.001,  0.220,  0.220,  0.220,  0.220,  0.220/)

   ! transmittance of green leaf in near infrared band
   real(r8), parameter, dimension(N_land_classification) :: taul_nir_usgs &
      = (/0.250,  0.250,  0.250,  0.250,  0.250,  0.250,  0.250,  0.250,&
          0.250,  0.250,  0.250,  0.100,  0.250,  0.100,  0.150,  0.250,&
          0.250,  0.250,  0.250,  0.250,  0.250,  0.250,  0.250,  0.250/)

   ! transmittance of dead leaf in near infrared band
   real(r8), parameter, dimension(N_land_classification) :: taus_nir_usgs &
      = (/0.380,  0.380,  0.380,  0.380,  0.380,  0.380,  0.380,  0.001,&
          0.001,  0.380,  0.001,  0.001,  0.001,  0.001,  0.001,  0.380,&
          0.380,  0.001,  0.001,  0.380,  0.380,  0.380,  0.380,  0.380/)

   ! maximum carboxylation rate at 25 C at canopy top
   ! /06/03/2014/ based on Bonan et al., 2010 (Table 2)
   real(r8), parameter, dimension(N_land_classification) :: vmax25_usgs &
      = (/100.0, 57.0, 57.0, 57.0, 52.0, 52.0, 52.0, 52.0,&
           52.0, 52.0, 52.0, 57.0, 72.0, 54.0, 52.0, 57.0,&
           52.0, 52.0, 52.0, 52.0, 52.0, 52.0, 52.0, 52.0/)

   ! quantum efficiency
   !TODO: no C4, 0.05 may have problem
   real(r8), parameter, dimension(N_land_classification) :: effcon_usgs &
      = (/0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08,&
          0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08,&
          0.08, 0.08, 0.08, 0.05, 0.05, 0.05, 0.05, 0.05/)

   !c3c4 flag
   integer, parameter, dimension(N_land_classification) :: c3c4_usgs &
      = (/1, 1, 1, 1, 1, 1, 1, 1,&
          1, 1, 1, 1, 1, 1, 1, 1,&
          1, 1, 1, 0, 0, 0, 0, 0/)

   ! conductance-photosynthesis slope parameter
   !TODO: no C4, 4.0 may have problem
   real(r8), parameter, dimension(N_land_classification) :: g1_usgs &
      = (/4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0,&
          4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0,&
          4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0/)

   ! conductance-photosynthesis intercept
   real(r8), parameter, dimension(N_land_classification) :: g0_usgs &
      = (/100, 100, 100, 100, 100, 100, 100, 100,&
          100, 100, 100, 100, 100, 100, 100, 100,&
          100, 100, 100, 100, 100, 100, 100, 100/)

   ! conductance-photosynthesis slope parameter
   !TODO: no C4, 4.0 may have problem
   real(r8), parameter, dimension(N_land_classification) :: gradm_usgs &
      = (/9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0,&
          9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0,&
          9.0, 9.0, 9.0, 4.0, 4.0, 4.0, 4.0, 4.0/)

   ! conductance-photosynthesis intercept
   real(r8), parameter, dimension(N_land_classification) :: binter_usgs &
      = (/0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01,&
          0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01,&
          0.01, 0.01, 0.01, 0.04, 0.04, 0.04, 0.04, 0.04/)

   ! respiration fraction
   real(r8), parameter, dimension(N_land_classification) :: respcp_usgs &
      = (/0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015,&
          0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015,&
          0.015, 0.015, 0.015, 0.025, 0.025, 0.025, 0.025, 0.025/)

   ! slope of high temperature inhibition FUNCTION (s1)
   real(r8), parameter, dimension(N_land_classification) :: shti_usgs = 0.3

   ! slope of low temperature inhibition FUNCTION (s3)
   real(r8), parameter, dimension(N_land_classification) :: slti_usgs = 0.2

   ! temperature coefficient in gs-a model (s5)
   real(r8), parameter, dimension(N_land_classification) :: trda_usgs = 1.3

   ! temperature coefficient in gs-a model (s6)
   real(r8), parameter, dimension(N_land_classification) :: trdm_usgs = 328.0

   ! temperature coefficient in gs-a model (273.16+25)
   real(r8), parameter, dimension(N_land_classification) :: trop_usgs = 298.0

   ! 1/2 point of high temperature inhibition FUNCTION (s2)
   real(r8), parameter, dimension(N_land_classification) :: hhti_usgs &
      =(/308.0, 308.0, 308.0, 308.0, 308.0, 308.0, 308.0, 313.0,&
         313.0, 308.0, 311.0, 303.0, 313.0, 303.0, 307.0, 308.0,&
         308.0, 313.0, 313.0, 313.0, 313.0, 313.0, 313.0, 308.0/)

   ! 1/2 point of low temperature inhibition FUNCTION (s4)
   real(r8), parameter, dimension(N_land_classification) :: hlti_usgs &
      =(/281.0, 281.0, 281.0, 281.0, 281.0, 281.0, 281.0, 283.0,&
         283.0, 281.0, 283.0, 278.0, 288.0, 278.0, 281.0, 281.0,&
         281.0, 288.0, 283.0, 288.0, 288.0, 288.0, 288.0, 281.0/)

   ! coefficient of leaf nitrogen allocation
   real(r8), parameter, dimension(N_land_classification) :: extkn_usgs = 0.5

   ! depth at 50% roots
   real(r8), parameter, dimension(N_land_classification) :: d50_usgs &
      =(/23.0,  21.0,  23.0,  22.0,  15.7,  19.0,   9.3,  47.0,&
         28.2,  21.7,  16.0,  16.0,  15.0,  15.0,  15.5,   1.0,&
          9.3,  15.5,  27.0,   9.0,   9.0,   9.0,   9.0,   1.0/)

   ! coefficient of root profile
   real(r8), parameter, dimension(N_land_classification) :: beta_usgs &
      =(/-1.757, -1.835, -1.757, -1.796, -1.577, -1.738, -1.359, -3.245,&
         -2.302, -1.654, -1.681, -1.681, -1.632, -1.632, -1.656, -1.000,&
         -1.359, -1.656, -2.051, -2.621, -2.621, -2.621, -2.621, -1.000/)

   ! Table 2. Zeng, 2001
   ! urban ==> cropland
   ! water/glacier ==> grass
   real(r8), parameter, dimension(N_land_classification) :: roota_usgs &
      =(/ 5.558,  5.558,  5.558,  5.558,  8.149,  5.558, 10.740,  7.022,&
          8.881,  7.920,  5.990,  7.066,  7.344,  6.706,  4.453, 10.740,&
         10.740,  4.453,  8.992,  8.992,  8.992,  8.992,  4.372, 10.740/)

   real(r8), parameter, dimension(N_land_classification) :: rootb_usgs &
      =(/ 2.614,  2.614,  2.614,  2.614,  2.611,  2.614,  2.608,  1.415,&
          2.012,  1.964,  1.955,  1.953,  1.303,  2.175,  1.631,  2.608,&
          2.608,  1.631,  8.992,  8.992,  8.992,  8.992,  0.978,  2.608/)

   ! Plant Hydraulics Parameters
   real(r8), parameter, dimension(N_land_classification) :: kmax_sun0_usgs &
      = (/     0., 2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008,&
          2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008,&
          2.e-008, 2.e-008, 2.e-008,      0., 2.e-008, 2.e-008,&
               0., 2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008/)

   real(r8), parameter, dimension(N_land_classification) :: kmax_sha0_usgs &
      = (/     0., 2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008,&
          2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008,&
          2.e-008, 2.e-008, 2.e-008,      0., 2.e-008, 2.e-008,&
               0., 2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008/)

   real(r8), parameter, dimension(N_land_classification) :: kmax_xyl0_usgs &
      = (/     0., 2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008,&
          2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008,&
          2.e-008, 2.e-008, 2.e-008,      0., 2.e-008, 2.e-008,&
               0., 2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008/)

   real(r8), parameter, dimension(N_land_classification) :: kmax_root0_usgs &
      = (/     0., 2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008,&
          2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008,&
          2.e-008, 2.e-008, 2.e-008,      0., 2.e-008, 2.e-008,&
               0., 2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008/)

   ! water potential at 50% loss of sunlit leaf tissue conductance (mmH2O)
   real(r8), parameter, dimension(N_land_classification) :: psi50_sun0_usgs &
      = (/-150000.0,-340000.0,-340000.0,-340000.0,-340000.0,-343636.4,&
          -340000.0,-393333.3,-366666.7,-340000.0,-270000.0,-380000.0,&
          -260000.0,-465000.0,-330000.0,-150000.0,-340000.0,-347272.7,&
          -150000.0,-340000.0,-342500.0,-341250.0,-150000.0,-150000.0/) *1

   ! water potential at 50% loss of shaded leaf tissue conductance (mmH2O)
   real(r8), parameter, dimension(N_land_classification) :: psi50_sha0_usgs &
      = (/-150000.0,-340000.0,-340000.0,-340000.0,-340000.0,-343636.4,&
          -340000.0,-393333.3,-366666.7,-340000.0,-270000.0,-380000.0,&
          -260000.0,-465000.0,-330000.0,-150000.0,-340000.0,-347272.7,&
          -150000.0,-340000.0,-342500.0,-341250.0,-150000.0,-150000.0/) *1

   ! water potential at 50% loss of xylem tissue conductance (mmH2O)
   real(r8), parameter, dimension(N_land_classification) :: psi50_xyl0_usgs &
      = (/-200000.0,-340000.0,-340000.0,-340000.0,-340000.0,-343636.4,&
          -340000.0,-393333.3,-366666.7,-340000.0,-270000.0,-380000.0,&
          -260000.0,-465000.0,-330000.0,-200000.0,-340000.0,-347272.7,&
          -200000.0,-340000.0,-342500.0,-341250.0,-200000.0,-200000.0/) *1

   ! water potential at 50% loss of root tissue conductance (mmH2O)
   real(r8), parameter, dimension(N_land_classification) :: psi50_root0_usgs &
      = (/-200000.0,-340000.0,-340000.0,-340000.0,-340000.0,-343636.4,&
          -340000.0,-393333.3,-366666.7,-340000.0,-270000.0,-380000.0,&
          -260000.0,-465000.0,-330000.0,-200000.0,-340000.0,-347272.7,&
          -200000.0,-340000.0,-342500.0,-341250.0,-200000.0,-200000.0/)*1

   ! shape-fitting parameter for vulnerability curve (-)
   real(r8), parameter, dimension(N_land_classification) :: ck0_usgs &
      = (/  0., 3.95, 3.95, 3.95, 3.95, 3.95, &
          3.95, 3.95, 3.95, 3.95, 3.95, 3.95, &
          3.95, 3.95, 3.95,   0., 3.95, 3.95, &
            0., 3.95, 3.95, 3.95,   0.,   0./)

   ! lambda for WUE stomata model
   real(r8), parameter, dimension(N_land_classification) :: lambda_usgs &
      = (/1000., 1000., 1000., 1000., 1000., 1000., &
          1000., 1000., 1000., 1000., 1000., 1000., &
          1000., 1000., 1000., 1000., 1000., 1000., &
          1000., 1000., 1000., 1000., 1000., 1000./)
!end plant hydraulic parameters
#else

! MODIS IGBP Land Use/Land Cover System Legend
!-----------------------------------------------------------------------
! 0  Ocean
! 1  Evergreen Needleleaf Forests
! 2  Evergreen Broadleaf Forests
! 3  Deciduous Needleleaf Forests
! 4  Deciduous Broadleaf Forests
! 5  Mixed Forests
! 6  Closed Shrublands
! 7  Open Shrublands
! 8  Woody Savannas
! 9  Savannas
!10  Grasslands
!11  Permanent Wetlands
!12  Croplands
!13  Urban and Built-up Lands
!14  Cropland/Natural Vegetation Mosaics
!15  Permanent Snow and Ice
!16  Barren
!17  Water Bodies

   character(len=256) :: patchclassname (0:N_land_classification) = &
      (/'0 Ocean                               ', '1 Evergreen Needleleaf Forests        ', &
        '2 Evergreen Broadleaf Forests         ', '3 Deciduous Needleleaf Forests        ', &
        '4 Deciduous Broadleaf Forests         ', '5 Mixed Forests                       ', &
        '6 Closed Shrublands                   ', '7 Open Shrublands                     ', &
        '8 Woody Savannas                      ', '9 Savannas                            ', &
        '10 Grasslands                         ', '11 Permanent Wetlands                 ', &
        '12 Croplands                          ', '13 Urban and Built-up Lands           ', &
        '14 Cropland/Natural Vegetation Mosaics', '15 Permanent Snow and Ice             ', &
        '16 Barren                             ', '17 Water Bodies                       ' /)

   ! land patch types
   ! 0: soil, 1: urban, 2: wetland, 3: ice, 4: lake
   integer , parameter, dimension(N_land_classification) :: patchtypes_igbp &
      = (/0, 0, 0, 0, 0, 0, 0, 0,&
          0, 0, 2, 0, 1, 0, 3, 0,&
          4 /)

   ! Look-up table canopy top height
   !NOTE: now read from input NetCDF file
   ! shrub land 0.5m? grass like land 1m? all set to 0.5
   real(r8), parameter, dimension(N_land_classification) :: htop0_igbp &
     !=(/17.0,  35.0,  17.0,  20.0,  20.0,   0.5,   0.5,   1.0,&
     !    1.0,   1.0,   1.0,   1.0,   1.0,   1.0,   1.0,   1.0,&
     !    1.0 /)
      =(/17.0,  35.0,  17.0,  20.0,  20.0,   0.5,   0.5,   1.0,&
          0.5,   0.5,   0.5,   0.5,   1.0,   0.5,   0.5,   0.5,&
          0.5 /)

   ! Look-up table canopy bottom height
   ! 01/06/2020, yuan: adjust hbop: grass/shrub -> 0, tree->1
   real(r8), parameter, dimension(N_land_classification) :: hbot0_igbp &
     !=(/ 8.5,   1.0,   8.5,  11.5,  10.0,   0.1,   0.1,   0.1,&
     !    0.1,  0.01,  0.01,  0.01,   0.3,  0.01,  0.01,  0.01,&
     !   0.01 /)
      =(/ 1.0,   1.0,   1.0,   1.0,   1.0,   0.0,   0.0,   0.0,&
          0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,&
          0.0 /)

   ! Look-up table vegetation fractional cover
   real(r8), parameter, dimension(N_land_classification) :: fveg0_igbp &
      = 1.0 !(/.../)

   ! Look-up table stem area index
   !NOTE: now read from input NetCDF file
   real(r8), parameter, dimension(N_land_classification) :: sai0_igbp &
      =(/2.0, 2.0, 2.0, 2.0, 2.0, 0.5, 0.5, 0.5,&
         0.5, 0.2, 0.2, 0.2, 0.2, 0.2, 0.0, 0.0,&
         0.0 /)

   ! ratio to calculate roughness length z0m
   real(r8), parameter, dimension(N_land_classification) :: z0mr_igbp = 0.1

   ! ratio to calculate displacement height d
   real(r8), parameter, dimension(N_land_classification) :: displar_igbp = 0.667

   ! inverse&sqrt leaf specific dimension size 4 cm
   real(r8), parameter, dimension(N_land_classification) :: sqrtdi_igbp = 5.0

   ! leaf angle distribution parameter
   real(r8), parameter, dimension(N_land_classification) :: chil_igbp &
      = (/ 0.010,  0.100,  0.010,  0.250,  0.125,  0.010,  0.010,  0.010,&
           0.010, -0.300,  0.100, -0.300,  0.010, -0.300,  0.010,  0.010,&
           0.010 /)

   ! reflectance of green leaf in visible band
   real(r8), parameter, dimension(N_land_classification) :: rhol_vis_igbp &
      = (/0.070,  0.100,  0.070,  0.100,  0.070,  0.105,  0.105,  0.105,&
          0.105,  0.105,  0.105,  0.105,  0.105,  0.105,  0.105,  0.105,&
          0.105 /)

   ! reflectance of dead leaf in visible band
   real(r8), parameter, dimension(N_land_classification) :: rhos_vis_igbp &
      = (/0.160,  0.160,  0.160,  0.160,  0.160,  0.160,  0.160,  0.160,&
          0.160,  0.360,  0.160,  0.360,  0.160,  0.360,  0.160,  0.160,&
          0.160 /)

   ! reflectance of green leaf in near infrared band
   real(r8), parameter, dimension(N_land_classification) :: rhol_nir_igbp &
      = (/0.350,  0.450,  0.350,  0.450,  0.400,  0.450,  0.450,  0.580,&
          0.580,  0.580,  0.450,  0.580,  0.450,  0.580,  0.450,  0.450,&
          0.580 /)

   ! reflectance of dead leaf in near infrared band
   real(r8), parameter, dimension(N_land_classification) :: rhos_nir_igbp &
      = (/0.390,  0.390,  0.390,  0.390,  0.390,  0.390,  0.390,  0.390,&
          0.390,  0.580,  0.390,  0.580,  0.390,  0.580,  0.390,  0.390,&
          0.580 /)

   ! transmittance of green leaf in visible band
   real(r8), parameter, dimension(N_land_classification) :: taul_vis_igbp &
      = (/0.050,  0.050,  0.050,  0.050,  0.050,  0.050,  0.050,  0.050,&
          0.050,  0.070,  0.050,  0.070,  0.050,  0.070,  0.050,  0.050,&
          0.050 /)

   ! transmittance of dead leaf in visible band
   real(r8), parameter, dimension(N_land_classification) :: taus_vis_igbp &
      = (/0.001,  0.001,  0.001,  0.001,  0.001,  0.001,  0.001,  0.001,&
          0.001,  0.220,  0.001,  0.220,  0.001,  0.220,  0.001,  0.001,&
          0.001 /)

   ! transmittance of green leaf in near infrared band
   real(r8), parameter, dimension(N_land_classification) :: taul_nir_igbp &
      = (/0.100,  0.250,  0.100,  0.250,  0.150,  0.250,  0.250,  0.250,&
          0.250,  0.250,  0.250,  0.250,  0.250,  0.250,  0.250,  0.250,&
          0.250 /)

   ! transmittance of dead leaf in near infrared band
   real(r8), parameter, dimension(N_land_classification) :: taus_nir_igbp &
      = (/0.001,  0.001,  0.001,  0.001,  0.001,  0.001,  0.001,  0.001,&
          0.001,  0.380,  0.001,  0.380,  0.001,  0.380,  0.001,  0.001,&
          0.001 /)

   ! maximum carboxylation rate at 25 C at canopy top
   ! /06/03/2014/ based on Bonan et al., 2010 (Table 2)
   real(r8), parameter, dimension(N_land_classification) :: vmax25_igbp &
      = (/ 54.0, 72.0, 57.0, 52.0, 52.0, 52.0, 52.0, 52.0,&
           52.0, 52.0, 52.0, 57.0,100.0, 57.0, 52.0, 52.0,&
           52.0 /)

   ! quantum efficiency
   !TODO: no C4
   real(r8), parameter, dimension(N_land_classification) :: effcon_igbp &
      = (/0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08,&
          0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08,&
          0.08 /)

   !c3c4 flag
   integer, parameter, dimension(N_land_classification) :: c3c4_igbp &
      = (/1, 1, 1, 1, 1, 1, 1, 1,&
          1, 1, 1, 1, 1, 1, 1, 1,&
          1 /)

   ! conductance-photosynthesis slope parameter
   real(r8), parameter, dimension(N_land_classification) :: g1_igbp &
      = (/9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0,&
          9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0,&
          9.0 /)

   ! conductance-photosynthesis intercept
   real(r8), parameter, dimension(N_land_classification) :: g0_igbp &
      = (/100, 100, 100, 100, 100, 100, 100, 100,&
          100, 100, 100, 100, 100, 100, 100, 100,&
          100 /)

   ! conductance-photosynthesis slope parameter
   real(r8), parameter, dimension(N_land_classification) :: gradm_igbp &
      = (/9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0,&
          9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0, 9.0,&
          9.0 /)

   ! conductance-photosynthesis intercept
   real(r8), parameter, dimension(N_land_classification) :: binter_igbp &
      = (/0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01,&
          0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01,&
          0.01 /)

   ! respiration fraction
   real(r8), parameter, dimension(N_land_classification) :: respcp_igbp &
      = (/0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015,&
          0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015,&
          0.015 /)

   ! slope of high temperature inhibition FUNCTION (s1)
   real(r8), parameter, dimension(N_land_classification) :: shti_igbp = 0.3

   ! slope of low temperature inhibition FUNCTION (s3)
   real(r8), parameter, dimension(N_land_classification) :: slti_igbp = 0.2

   ! temperature coefficient in gs-a model (s5)
   real(r8), parameter, dimension(N_land_classification) :: trda_igbp = 1.3

   ! temperature coefficient in gs-a model (s6)
   real(r8), parameter, dimension(N_land_classification) :: trdm_igbp = 328.0

   ! temperature coefficient in gs-a model (273.16+25)
   real(r8), parameter, dimension(N_land_classification) :: trop_igbp = 298.0

   ! 1/2 point of high temperature inhibition FUNCTION (s2)
   real(r8), parameter, dimension(N_land_classification) :: hhti_igbp &
      =(/303.0, 313.0, 303.0, 311.0, 307.0, 308.0, 313.0, 313.0,&
         313.0, 308.0, 313.0, 308.0, 308.0, 308.0, 303.0, 313.0,&
         308.0 /)

   ! 1/2 point of low temperature inhibition FUNCTION (s4)
   real(r8), parameter, dimension(N_land_classification) :: hlti_igbp &
      =(/278.0, 288.0, 278.0, 283.0, 281.0, 281.0, 288.0, 288.0,&
         288.0, 281.0, 283.0, 281.0, 281.0, 281.0, 278.0, 288.0,&
         281.0 /)

   ! coefficient of leaf nitrogen allocation
   real(r8), parameter, dimension(N_land_classification) :: extkn_igbp = 0.5

   ! depth at 50% roots
   real(r8), parameter, dimension(N_land_classification) :: d50_igbp &
      =(/15.0,  15.0,  16.0,  16.0,  15.5,  19.0,  28.0,  18.5,&
         28.0,   9.0,   9.0,  22.0,  23.0,  22.0,   1.0,   9.0,&
          1.0 /)
   ! coefficient of root profile
   real(r8), parameter, dimension(N_land_classification) :: beta_igbp &
      =(/-1.623, -1.623, -1.681, -1.681, -1.652, -1.336, -1.909, -1.582,&
         -1.798, -1.359, -1.359, -1.796, -1.757, -1.796, -1.000, -2.261,&
         -1.000 /)

   ! Table 2. Zeng, 2001
   ! water/glacier ==> grass
   ! urban ==> cropland
   real(r8), parameter, dimension(N_land_classification) :: roota_igbp &
      =(/ 6.706,  7.344,  7.066,  5.990,  4.453,  6.326,  7.718,  7.604,&
          8.235, 10.740, 10.740,  5.558,  5.558,  5.558, 10.740,  4.372,&
         10.740 /)

   real(r8), parameter, dimension(N_land_classification) :: rootb_igbp &
      =(/ 2.175,  1.303,  1.953,  1.955,  1.631,  1.567,  1.262,  2.300,&
          1.627,  2.608,  2.608,  2.614,  2.614,  2.614,  2.608,  0.978,&
          2.608 /)

   ! Plant Hydraulics Parameters
   real(r8), parameter, dimension(N_land_classification) :: kmax_sun0_igbp &
      = (/2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008, &
          2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008, &
          2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008     /)

   real(r8), parameter, dimension(N_land_classification) :: kmax_sha0_igbp &
      = (/2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008, &
          2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008, &
          2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008  /)

   real(r8), parameter, dimension(N_land_classification) :: kmax_xyl0_igbp &
      = (/2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008, &
          2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008, &
          2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008  /)

   real(r8), parameter, dimension(N_land_classification) :: kmax_root0_igbp &
      = (/2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008, &
          2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008, &
          2.e-008, 2.e-008, 2.e-008, 2.e-008, 2.e-008  /)

   ! water potential at 50% loss of sunlit leaf tissue conductance (mmH2O)
   real(r8), parameter, dimension(N_land_classification) :: psi50_sun0_igbp &
      = (/-465000.0, -260000.0, -380000.0, -270000.0, -330000.0, -393333.3, &
          -393333.3, -340000.0, -340000.0, -340000.0, -343636.4, -340000.0, &
          -150000.0, -343636.4, -150000.0, -150000.0, -150000.0/) *1

   ! water potential at 50% loss of shaded leaf tissue conductance (mmH2O)
   real(r8), parameter, dimension(N_land_classification) :: psi50_sha0_igbp &
      = (/-465000.0, -260000.0, -380000.0, -270000.0, -330000.0, -393333.3, &
          -393333.3, -340000.0, -340000.0, -340000.0, -343636.4, -340000.0, &
          -150000.0, -343636.4, -150000.0, -150000.0, -150000.0/) *1

   ! water potential at 50% loss of xylem tissue conductance (mmH2O)
   real(r8), parameter, dimension(N_land_classification) :: psi50_xyl0_igbp &
      = (/-465000.0, -260000.0, -380000.0, -270000.0, -330000.0, -393333.3, &
          -393333.3, -340000.0, -340000.0, -340000.0, -343636.4, -340000.0, &
          -200000.0, -343636.4, -200000.0, -200000.0, -200000.0/) *1

   ! water potential at 50% loss of root tissue conductance (mmH2O)
   real(r8), parameter, dimension(N_land_classification) :: psi50_root0_igbp &
      = (/-465000.0, -260000.0, -380000.0, -270000.0, -330000.0, -393333.3, &
          -393333.3, -340000.0, -340000.0, -340000.0, -343636.4, -340000.0, &
          -200000.0, -343636.4, -200000.0, -200000.0, -200000.0/) *1

   ! shape-fitting parameter for vulnerability curve (-)
   real(r8), parameter, dimension(N_land_classification) :: ck0_igbp &
      = (/3.95, 3.95, 3.95, 3.95, 3.95, 3.95, &
          3.95, 3.95, 3.95, 3.95, 3.95, 3.95, &
          3.95, 3.95, 3.95, 3.95, 3.95  /)
   !end plant hydraulic parameters

   ! lambda for WUE stomata model
   real(r8), parameter, dimension(N_land_classification) :: lambda_igbp &
      = (/1000., 1000., 1000., 1000., 1000., 1000., &
          1000., 1000., 1000., 1000., 1000., 1000., &
          1000., 1000., 1000., 1000., 1000./)
#endif

   real(r8), dimension(N_land_classification) :: &
      patchtypes, &! land patch types
      htop0,      &! canopy top height
      hbot0,      &! canopy bottom height
      fveg0,      &! canopy vegetation fractional cover
      sai0,       &! canopy stem area index
      chil,       &! leaf angle distribution factor
      z0mr,       &! ratio to calculate roughness length z0m
      displar,    &! ratio to calculate displacement height d
      sqrtdi,     &! inverse sqrt of leaf dimension [m**-0.5]

      vmax25,     &! maximum carboxylation rate at 25 C at canopy top
      effcon,     &! quantum efficiency
      g1,         &! conductance-photosynthesis slope parameter
      g0,         &! conductance-photosynthesis intercept
      gradm,      &! conductance-photosynthesis slope parameter
      binter,     &! conductance-photosynthesis intercept
      respcp,     &! respiration fraction
      shti,       &! slope of high temperature inhibition function (s1)
      slti,       &! slope of low temperature inhibition function (s3)
      trda,       &! temperature coefficient in gs-a model (s5)
      trdm,       &! temperature coefficient in gs-a model (s6)
      trop,       &! temperature coefficient in gs-a model (273.16+25)
      hhti,       &! 1/2 point of high temperature inhibition function (s2)
      hlti,       &! 1/2 point of low temperature inhibition function (s4)
      extkn,      &! coefficient of leaf nitrogen allocation

      lambda,     &! marginal water cost of carbon gain (mol mol-1)

      d50,        &! depth at 50% roots
      beta         ! coefficient of root profile

   integer, dimension(N_land_classification) :: c3c4 ! c3c4 flag

! Plant Hydraulic Parameters
   real(r8), dimension(N_land_classification) :: &
      kmax_sun,   &! Plant Hydraulics Parameters (TODO@Xingjie Lu, please give more details)
      kmax_sha,   &! Plant Hydraulics Parameters
      kmax_xyl,   &! Plant Hydraulics Parameters
      kmax_root,  &! Plant Hydraulics Parameters
      psi50_sun,  &! water potential at 50% loss of sunlit leaf tissue conductance (mmH2O)
      psi50_sha,  &! water potential at 50% loss of shaded leaf tissue conductance (mmH2O)
      psi50_xyl,  &! water potential at 50% loss of xylem tissue conductance (mmH2O)
      psi50_root, &! water potential at 50% loss of root tissue conductance (mmH2O)
      ck           ! shape-fitting parameter for vulnerability curve (-)
! end plant hydraulic parameters

   real(r8), PRIVATE, dimension(N_land_classification) :: &
      roota,      &! root fraction para
      rootb        ! root fraction para

   real(r8) ::    &
      rho(2,2,N_land_classification),&! leaf reflectance
      tau(2,2,N_land_classification)  ! leaf transmittance

   ! scheme 1: Schenk and Jackson, 2002, 2: Zeng 2001
   integer, PRIVATE :: ROOTFR_SCHEME = 1

   ! fraction of roots in each soil layer
   real(r8), dimension(nl_soil,N_land_classification) :: rootfr

   ! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: Init_LC_Const

CONTAINS

   SUBROUTINE Init_LC_Const

   USE MOD_Namelist
   IMPLICIT NONE

   integer :: i, nsl

#ifdef LULC_USGS
      patchtypes (:) = patchtypes_usgs (:)
      htop0      (:) = htop0_usgs      (:)
      hbot0      (:) = hbot0_usgs      (:)
      fveg0      (:) = fveg0_usgs      (:)
      sai0       (:) = sai0_usgs       (:)
      z0mr       (:) = z0mr_usgs       (:)
      displar    (:) = displar_usgs    (:)
      sqrtdi     (:) = sqrtdi_usgs     (:)
      chil       (:) = chil_usgs       (:)
      vmax25     (:) = vmax25_usgs     (:) * 1.e-6
      effcon     (:) = effcon_usgs     (:)
      c3c4       (:) = c3c4_usgs       (:)
      g1         (:) = g1_usgs         (:)
      g0         (:) = g0_usgs         (:)
      gradm      (:) = gradm_usgs      (:)
      binter     (:) = binter_usgs     (:)
      respcp     (:) = respcp_usgs     (:)
      shti       (:) = shti_usgs       (:)
      slti       (:) = slti_usgs       (:)
      trda       (:) = trda_usgs       (:)
      trdm       (:) = trdm_usgs       (:)
      trop       (:) = trop_usgs       (:)
      hhti       (:) = hhti_usgs       (:)
      hlti       (:) = hlti_usgs       (:)
      extkn      (:) = extkn_usgs      (:)
      d50        (:) = d50_usgs        (:)
      beta       (:) = beta_usgs       (:)
IF (DEF_USE_PLANTHYDRAULICS) THEN
      kmax_sun   (:) = kmax_sun0_usgs  (:)
      kmax_sha   (:) = kmax_sha0_usgs  (:)
      kmax_xyl   (:) = kmax_xyl0_usgs  (:)
      kmax_root  (:) = kmax_root0_usgs (:)
      psi50_sun  (:) = psi50_sun0_usgs (:)
      psi50_sha  (:) = psi50_sha0_usgs (:)
      psi50_xyl  (:) = psi50_xyl0_usgs (:)
      psi50_root (:) = psi50_root0_usgs(:)
      ck         (:) = ck0_usgs        (:)
ENDIF
IF (DEF_USE_WUEST)THEN
      lambda     (:) = lambda_usgs     (:)
ENDIF
      roota      (:) = roota_usgs      (:)
      rootb      (:) = rootb_usgs      (:)
      rho    (1,1,:) = rhol_vis_usgs   (:)
      rho    (2,1,:) = rhol_nir_usgs   (:)
      rho    (1,2,:) = rhos_vis_usgs   (:)
      rho    (2,2,:) = rhos_nir_usgs   (:)
      tau    (1,1,:) = taul_vis_usgs   (:)
      tau    (2,1,:) = taul_nir_usgs   (:)
      tau    (1,2,:) = taus_vis_usgs   (:)
      tau    (2,2,:) = taus_nir_usgs   (:)
#else
      patchtypes (:) = patchtypes_igbp (:)
      htop0      (:) = htop0_igbp      (:)
      hbot0      (:) = hbot0_igbp      (:)
      fveg0      (:) = fveg0_igbp      (:)
      sai0       (:) = sai0_igbp       (:)
      z0mr       (:) = z0mr_igbp       (:)
      displar    (:) = displar_igbp    (:)
      sqrtdi     (:) = sqrtdi_igbp     (:)
      chil       (:) = chil_igbp       (:)
      vmax25     (:) = vmax25_igbp     (:) * 1.e-6
      effcon     (:) = effcon_igbp     (:)
      c3c4       (:) = c3c4_igbp       (:)
      g1         (:) = g1_igbp         (:)
      g0         (:) = g0_igbp         (:)
      gradm      (:) = gradm_igbp      (:)
      binter     (:) = binter_igbp     (:)
      respcp     (:) = respcp_igbp     (:)
      shti       (:) = shti_igbp       (:)
      slti       (:) = slti_igbp       (:)
      trda       (:) = trda_igbp       (:)
      trdm       (:) = trdm_igbp       (:)
      trop       (:) = trop_igbp       (:)
      hhti       (:) = hhti_igbp       (:)
      hlti       (:) = hlti_igbp       (:)
      extkn      (:) = extkn_igbp      (:)
      d50        (:) = d50_igbp        (:)
      beta       (:) = beta_igbp       (:)
IF(DEF_USE_PLANTHYDRAULICS)THEN
      kmax_sun   (:) = kmax_sun0_igbp  (:)
      kmax_sha   (:) = kmax_sha0_igbp  (:)
      kmax_xyl   (:) = kmax_xyl0_igbp  (:)
      kmax_root  (:) = kmax_root0_igbp (:)
      psi50_sun  (:) = psi50_sun0_igbp (:)
      psi50_sha  (:) = psi50_sha0_igbp (:)
      psi50_xyl  (:) = psi50_xyl0_igbp (:)
      psi50_root (:) = psi50_root0_igbp(:)
      ck         (:) = ck0_igbp        (:)
ENDIF
IF (DEF_USE_WUEST)THEN
      lambda     (:) = lambda_igbp     (:)
ENDIF
      roota      (:) = roota_igbp      (:)
      rootb      (:) = rootb_igbp      (:)
      rho    (1,1,:) = rhol_vis_igbp   (:)
      rho    (2,1,:) = rhol_nir_igbp   (:)
      rho    (1,2,:) = rhos_vis_igbp   (:)
      rho    (2,2,:) = rhos_nir_igbp   (:)
      tau    (1,1,:) = taul_vis_igbp   (:)
      tau    (2,1,:) = taul_nir_igbp   (:)
      tau    (1,2,:) = taus_vis_igbp   (:)
      tau    (2,2,:) = taus_nir_igbp   (:)
#endif

      ! ----------------------------------------------------------
      ! The definition of global root distribution is based on
      ! Schenk and Jackson, 2002: The Global Biogeography of Roots.
      ! Ecological Monagraph 72(3): 311-328.
      ! ----------------------------------------------------------
      IF (ROOTFR_SCHEME == 1) THEN
         DO i = 1, N_land_classification
            rootfr(1,i)=1./(1.+(zi_soi(1)*100./d50(i))**beta(i))
            rootfr(nl_soil,i)=1.-1./(1.+(zi_soi(nl_soil-1)*100./d50(i))**beta(i))

            DO nsl=2,nl_soil-1
               rootfr(nsl,i)=1./(1.+(zi_soi(nsl)*100./d50(i))**beta(i)) &
                  -1./(1.+(zi_soi(nsl-1)*100./d50(i))**beta(i))
            ENDDO
         ENDDO
      ELSE
         DO i = 1, N_land_classification
            rootfr(1,i) = 1. - 0.5*( &
                 exp(-roota(i) * zi_soi(1)) &
               + exp(-rootb(i) * zi_soi(1)) )

            rootfr(nl_soil,i) = 0.5*( &
                 exp(-roota(i) * zi_soi(nl_soil)) &
               + exp(-rootb(i) * zi_soi(nl_soil)) )

            DO nsl = 2, nl_soil-1
               rootfr(nsl,i) = 0.5*( &
                    exp(-roota(i) * zi_soi(nsl-1)) &
                  + exp(-rootb(i) * zi_soi(nsl-1)) &
                  - exp(-roota(i) * zi_soi(nsl)) &
                  - exp(-rootb(i) * zi_soi(nsl)) )
            ENDDO
         ENDDO
      ENDIF

   END SUBROUTINE Init_LC_Const

END MODULE MOD_Const_LC
! ---------- EOP ------------
