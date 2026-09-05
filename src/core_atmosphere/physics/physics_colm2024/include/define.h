#if defined(GRIDBASED) || defined(CATCHMENT) || defined(SinglePoint) || defined(BGC) || defined(CROP) || defined(LULCC) || defined(URBAN_MODEL) || defined(DataAssimilation) || defined(HYPERSPECTRAL) || defined(GridRiverLakeSediment) || defined(USESplitAI) || defined(EXTERNAL_LAKE)
#error "Unsupported CoLM options cannot be injected into the MPAS embedded build"
#endif

! 1. Spatial structure:
!    Select one of the following options.
#undef GRIDBASED
#undef CATCHMENT
#define UNSTRUCTURED
#undef SinglePoint

! 2. Land subgrid type classification:
!    Select one of the following options.
#undef LULC_USGS
#undef LULC_IGBP
#undef LULC_IGBP_PFT
#define LULC_IGBP_PC

! 2.1 3D Urban model (put it temporarily here):
#undef URBAN_MODEL
!    Dependence:  only LULC_IGBP subgrid type for
!    single point URBAN_MODEL right now.
#if (defined URBAN_MODEL && defined SinglePoint)
#define LULC_IGBP
#undef LULC_USGS
#undef LULC_IGBP_PFT
#undef LULC_IGBP_PC
#endif

! 3. Embedded diagnostics are written through MPAS streams.
#undef CoLMDEBUG
! If defined, surface data in vector is mapped to gridded data for checking.
#undef SrfdataDiag

! 4. CoLM uses MPI exclusively through the MPAS-owned communicator.
#define MPAS_MPI

! 5. Hydrological process options.
! 5.1 Two soil hydraulic models can be used.
#undef   Campbell_SOIL_MODEL
#define  vanGenuchten_Mualem_SOIL_MODEL
! 5.2 If defined, lateral flow is modeled.
#define CatchLateralFlow
!    Conflicts :
#ifndef CATCHMENT
#undef CatchLateralFlow
#endif

! 6. Embedded grid river-lake routing.
#define GridRiverLakeFlow
!    Conflicts :
#if (defined CATCHMENT || defined SinglePoint)
#undef GridRiverLakeFlow
#endif

#undef GridRiverLakeSediment
#if (!defined GridRiverLakeFlow)
#undef GridRiverLakeSediment
#endif

! 7. If defined, BGC model is used.
#undef BGC

!    Conflicts :  only used when LULC_IGBP_PFT is defined.
#ifndef LULC_IGBP_PFT
#ifndef LULC_IGBP_PC
#undef BGC
#endif
#endif
! 7.1 If defined, CROP model is used
#undef CROP
!    Conflicts : only used when BGC is defined
#ifndef BGC
#undef CROP
#endif

! 8. If defined, open Land use and land cover change mode.
#undef LULCC

! 9. If defined, data assimilation is used.
#undef DataAssimilation
#if (defined DataAssimilation)
#define LULC_IGBP
#undef LULC_USGS
#undef LULC_IGBP_PFT
#undef LULC_IGBP_PC
#endif

! 10. Interface to AI model.
#undef USESplitAI

! 11. External lake models.
#undef EXTERNAL_LAKE

! 12. Hyperspectral scheme.
#undef HYPERSPECTRAL

! MPAS embeds CoLM as a land-surface physics package on every MPAS rank.
! CoLM keeps patch/PFT state below MPAS-owned cells and communicates through the
! MPAS communicator. MPAS supplies CoLM VIS/NIR direct/diffuse forcing.
#define MPAS_EMBEDDED_COLM

! Keep the embedded build contract explicit. These optional CoLM source trees
! and interfaces have not been integrated into the MPAS-owned execution path.
#if !defined(UNSTRUCTURED)
#error "MPAS_EMBEDDED_COLM requires the UNSTRUCTURED spatial structure"
#endif
#if defined(BGC) || defined(CROP) || defined(LULCC) || defined(URBAN_MODEL) || defined(DataAssimilation) || defined(HYPERSPECTRAL) || defined(GridRiverLakeSediment) || defined(USESplitAI) || defined(EXTERNAL_LAKE)
#error "MPAS_EMBEDDED_COLM does not support BGC, CROP, LULCC, URBAN, DA, hyperspectral, sediment, AI, or external-lake options"
#endif
