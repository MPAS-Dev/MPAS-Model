#include <define.h>

MODULE MOD_Vars_1DFluxes
!-----------------------------------------------------------------------
!  Created by Yongjiu Dai, 03/2014
!-----------------------------------------------------------------------

   USE MOD_Precision
#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
   USE MOD_Vars_1DPFTFluxes
#endif
#ifdef BGC
   USE MOD_BGC_Vars_1DFluxes
#endif
#ifdef CatchLateralFlow
   USE MOD_Catch_Vars_1DFluxes
#endif
#ifdef URBAN_MODEL
   USE MOD_Urban_Vars_1DFluxes
#endif
#ifdef DataAssimilation
   USE MOD_DA_Vars_1DFluxes
#endif
   IMPLICIT NONE
   SAVE

!-----------------------------------------------------------------------
!  Fluxes
!-----------------------------------------------------------------------
   real(r8), allocatable :: taux   (:) !wind stress: E-W [kg/m/s2]
   real(r8), allocatable :: tauy   (:) !wind stress: N-S [kg/m/s2]
   real(r8), allocatable :: fsena  (:) !sensible heat from canopy height to atmosphere [W/m2]
   real(r8), allocatable :: lfevpa (:) !latent heat flux from canopy height to atmosphere [W/m2]
   real(r8), allocatable :: fevpa  (:) !evapotranspiration from canopy to atmosphere [mm/s]
   real(r8), allocatable :: fsenl  (:) !sensible heat from leaves [W/m2]
   real(r8), allocatable :: fevpl  (:) !evaporation+transpiration from leaves [mm/s]
   real(r8), allocatable :: etr    (:) !transpiration rate [mm/s]
   real(r8), allocatable :: fseng  (:) !sensible heat flux from ground [W/m2]
   real(r8), allocatable :: fevpg  (:) !evaporation heat flux from ground [mm/s]
   real(r8), allocatable :: fgrnd  (:) !ground heat flux [W/m2]
   real(r8), allocatable :: sabvsun(:) !solar absorbed by sunlit vegetation [W/m2]
   real(r8), allocatable :: sabvsha(:) !solar absorbed by shaded vegetation [W/m2]
   real(r8), allocatable :: sabg   (:) !solar absorbed by ground  [W/m2]
   real(r8), allocatable :: sr     (:) !total reflected solar radiation (W/m2)
   real(r8), allocatable :: solvd  (:) !incident direct beam vis solar radiation (W/m2)
   real(r8), allocatable :: solvi  (:) !incident diffuse beam vis solar radiation (W/m2)
   real(r8), allocatable :: solnd  (:) !incident direct beam nir solar radiation (W/m2)
   real(r8), allocatable :: solni  (:) !incident diffuse beam nir solar radiation (W/m2)
   real(r8), allocatable :: srvd   (:) !reflected direct beam vis solar radiation (W/m2)
   real(r8), allocatable :: srvi   (:) !reflected diffuse beam vis solar radiation (W/m2)
   real(r8), allocatable :: srnd   (:) !reflected direct beam nir solar radiation (W/m2)
   real(r8), allocatable :: srni   (:) !reflected diffuse beam nir solar radiation (W/m2)
   real(r8), allocatable :: solvdln(:) !incident direct beam vis solar radiation at local noon (W/m2)
   real(r8), allocatable :: solviln(:) !incident diffuse beam vis solar radiation at local noon (W/m2)
   real(r8), allocatable :: solndln(:) !incident direct beam nir solar radiation at local noon (W/m2)
   real(r8), allocatable :: solniln(:) !incident diffuse beam nir solar radiation at local noon (W/m2)
   real(r8), allocatable :: srvdln (:) !reflected direct beam vis solar radiation at local noon (W/m2)
   real(r8), allocatable :: srviln (:) !reflected diffuse beam vis solar radiation at local noon (W/m2)
   real(r8), allocatable :: srndln (:) !reflected direct beam nir solar radiation at local noon (W/m2)
   real(r8), allocatable :: srniln (:) !reflected diffuse beam nir solar radiation at local noon (W/m2)
#ifdef HYPERSPECTRAL
   real(r8), allocatable :: sol_dir_ln_hires(:,:) !incident direct beam vis solar radiation at local noon (W/m2)
   real(r8), allocatable :: sol_dif_ln_hires(:,:) !incident diffuse beam vis solar radiation at local noon (W/m2)
   real(r8), allocatable :: sr_dir_ln_hires (:,:) !reflected direct beam nir solar radiation at local noon (W/m2)
   real(r8), allocatable :: sr_dif_ln_hires (:,:) !reflected diffuse beam nir solar radiation at local noon (W/m2)
#endif
   real(r8), allocatable :: olrg   (:) !outgoing long-wave radiation from ground+canopy [W/m2]
   real(r8), allocatable :: rnet   (:) !net radiation by surface [W/m2]
   real(r8), allocatable :: xerr   (:) !the error of water balance [mm/s]
   real(r8), allocatable :: zerr   (:) !the error of energy balance [W/m2]
   real(r8), allocatable :: frcsat (:) !fraction of saturation area [-]
   real(r8), allocatable :: rsur   (:) !surface runoff (mm h2o/s)
   real(r8), allocatable :: rsur_se(:) !saturation excess surface runoff (mm h2o/s)
   real(r8), allocatable :: rsur_ie(:) !infiltration excess surface runoff (mm h2o/s)
   real(r8), allocatable :: rsub   (:) !subsurface runoff (mm h2o/s)
   real(r8), allocatable :: rnof   (:) !total runoff (mm h2o/s)
   real(r8), allocatable :: qintr  (:) !interception (mm h2o/s)
   real(r8), allocatable :: qinfl  (:) !infiltration (mm h2o/s)
   real(r8), allocatable :: qdrip  (:) !throughfall (mm h2o/s)
   real(r8), allocatable :: assim  (:) !canopy assimilation rate (mol m-2 s-1)
   real(r8), allocatable :: respc  (:) !canopy respiration (mol m-2 s-1)

   real(r8), allocatable :: qcharge(:) !groundwater recharge [mm/s]

   real(r8), allocatable :: qlayer     (:,:) !water flux at between soil layer [mm h2o/s]
   real(r8), allocatable :: lake_deficit (:) !lake deficit due to evaporation (mm h2o/s)

   real(r8), allocatable :: oroflag(:) !/ocean(0)/seaice(2) flag

   integer, parameter :: nsensor = 1
   real(r8), allocatable :: sensors(:,:)

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: allocate_1D_Fluxes
   PUBLIC :: deallocate_1D_Fluxes

! PRIVATE MEMBER FUNCTIONS:

!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------

   SUBROUTINE allocate_1D_Fluxes
   ! -------------------------------------------------------------------
   ! Allocates memory for CoLM 1d [numpatch] variables
   ! -------------------------------------------------------------------
   USE MOD_Precision
   USE MOD_Vars_Global
   USE MOD_MPAS_MPI
   USE MOD_LandPatch
   IMPLICIT NONE

      IF (.true.) THEN

         IF (numpatch > 0) THEN

            allocate ( taux   (numpatch) )  ; taux   (:) = spval ! wind stress: E-W [kg/m/s2]
            allocate ( tauy   (numpatch) )  ; tauy   (:) = spval ! wind stress: N-S [kg/m/s2]
            allocate ( fsena  (numpatch) )  ; fsena  (:) = spval ! sensible heat from canopy height to atmosphere [W/m2]
            allocate ( lfevpa (numpatch) )  ; lfevpa (:) = spval ! latent heat flux from canopy height to atmosphere [W/m2]
            allocate ( fevpa  (numpatch) )  ; fevpa  (:) = spval ! evapotranspiration from canopy to atmosphere [mm/s]
            allocate ( fsenl  (numpatch) )  ; fsenl  (:) = spval ! sensible heat from leaves [W/m2]
            allocate ( fevpl  (numpatch) )  ; fevpl  (:) = spval ! evaporation+transpiration from leaves [mm/s]
            allocate ( etr    (numpatch) )  ; etr    (:) = spval ! transpiration rate [mm/s]
            allocate ( fseng  (numpatch) )  ; fseng  (:) = spval ! sensible heat flux from ground [W/m2]
            allocate ( fevpg  (numpatch) )  ; fevpg  (:) = spval ! evaporation heat flux from ground [mm/s]
            allocate ( fgrnd  (numpatch) )  ; fgrnd  (:) = spval ! ground heat flux [W/m2]
            allocate ( sabvsun(numpatch) )  ; sabvsun(:) = spval ! solar absorbed by sunlit vegetation [W/m2]
            allocate ( sabvsha(numpatch) )  ; sabvsha(:) = spval ! solar absorbed by shaded vegetation [W/m2]
            allocate ( sabg   (numpatch) )  ; sabg   (:) = spval ! solar absorbed by ground  [W/m2]
            allocate ( sr     (numpatch) )  ; sr     (:) = spval ! incident direct beam vis solar radiation (W/m2)
            allocate ( solvd  (numpatch) )  ; solvd  (:) = spval ! incident direct beam vis solar radiation (W/m2)
            allocate ( solvi  (numpatch) )  ; solvi  (:) = spval ! incident diffuse beam vis solar radiation (W/m2)
            allocate ( solnd  (numpatch) )  ; solnd  (:) = spval ! incident direct beam nir solar radiation (W/m2)
            allocate ( solni  (numpatch) )  ; solni  (:) = spval ! incident diffuse beam nir solar radiation (W/m2)
            allocate ( srvd   (numpatch) )  ; srvd   (:) = spval ! reflected direct beam vis solar radiation (W/m2)
            allocate ( srvi   (numpatch) )  ; srvi   (:) = spval ! reflected diffuse beam vis solar radiation (W/m2)
            allocate ( srnd   (numpatch) )  ; srnd   (:) = spval ! reflected direct beam nir solar radiation (W/m2)
            allocate ( srni   (numpatch) )  ; srni   (:) = spval ! reflected diffuse beam nir solar radiation (W/m2)
            allocate ( solvdln(numpatch) )  ; solvdln(:) = spval ! incident direct beam vis solar radiation at local noon(W/m2)
            allocate ( solviln(numpatch) )  ; solviln(:) = spval ! incident diffuse beam vis solar radiation at local noon(W/m2)
            allocate ( solndln(numpatch) )  ; solndln(:) = spval ! incident direct beam nir solar radiation at local noon(W/m2)
            allocate ( solniln(numpatch) )  ; solniln(:) = spval ! incident diffuse beam nir solar radiation at local noon(W/m2)
            allocate ( srvdln (numpatch) )  ; srvdln (:) = spval ! reflected direct beam vis solar radiation at local noon(W/m2)
            allocate ( srviln (numpatch) )  ; srviln (:) = spval ! reflected diffuse beam vis solar radiation at local noon(W/m2)
            allocate ( srndln (numpatch) )  ; srndln (:) = spval ! reflected direct beam nir solar radiation at local noon(W/m2)
            allocate ( srniln (numpatch) )  ; srniln (:) = spval ! reflected diffuse beam nir solar radiation at local noon(W/m2)
#ifdef HYPERSPECTRAL
            allocate ( sol_dir_ln_hires(211,numpatch) )  ; sol_dir_ln_hires(:,:) = spval ! incident direct beam vis solar radiation at local noon(W/m2)
            allocate ( sol_dif_ln_hires(211,numpatch) )  ; sol_dif_ln_hires(:,:) = spval ! incident diffuse beam vis solar radiation at local noon(W/m2)
            allocate ( sr_dir_ln_hires (211,numpatch) )  ; sr_dir_ln_hires (:,:) = spval ! reflected direct beam nir solar radiation at local noon(W/m2)
            allocate ( sr_dif_ln_hires (211,numpatch) )  ; sr_dif_ln_hires (:,:) = spval ! reflected diffuse beam nir solar radiation at local noon(W/m2)
#endif
            allocate ( olrg   (numpatch) )  ; olrg   (:) = spval ! outgoing long-wave radiation from ground+canopy [W/m2]
            allocate ( rnet   (numpatch) )  ; rnet   (:) = spval ! net radiation by surface [W/m2]
            allocate ( xerr   (numpatch) )  ; xerr   (:) = spval ! the error of water balance [mm/s]
            allocate ( zerr   (numpatch) )  ; zerr   (:) = spval ! the error of energy balance [W/m2]

            allocate ( frcsat (numpatch) )  ; frcsat (:) = spval ! fraction of saturation area [-]
            allocate ( rsur   (numpatch) )  ; rsur   (:) = spval ! surface runoff (mm h2o/s)
            allocate ( rsur_se(numpatch) )  ; rsur_se(:) = spval ! saturation excess surface runoff (mm h2o/s)
            allocate ( rsur_ie(numpatch) )  ; rsur_ie(:) = spval ! infiltration excess surface runoff (mm h2o/s)
            allocate ( rsub   (numpatch) )  ; rsub   (:) = spval ! subsurface runoff (mm h2o/s)
            allocate ( rnof   (numpatch) )  ; rnof   (:) = spval ! total runoff (mm h2o/s)
            allocate ( qintr  (numpatch) )  ; qintr  (:) = spval ! interception (mm h2o/s)
            allocate ( qinfl  (numpatch) )  ; qinfl  (:) = spval ! infiltration (mm h2o/s)
            allocate ( qdrip  (numpatch) )  ; qdrip  (:) = spval ! throughfall (mm h2o/s)
            allocate ( assim  (numpatch) )  ; assim  (:) = spval ! canopy assimilation rate (mol m-2 s-1)
            allocate ( respc  (numpatch) )  ; respc  (:) = spval ! canopy respiration (mol m-2 s-1)

            allocate ( qcharge(numpatch) )  ; qcharge(:) = spval ! groundwater recharge [mm/s]

            allocate ( qlayer (0:nl_soil,numpatch) ); qlayer(:,:) = spval ! water flux between soil layer [mm h2o/s]
            allocate ( lake_deficit (numpatch) ); lake_deficit(:) = spval ! lake deficit due to evaporation (mm h2o/s)

            allocate ( oroflag(numpatch) )  ; oroflag(:) = 1.0   ! /ocean(0)/seaice(2) flag

            allocate ( sensors(nsensor,numpatch) ); sensors(:,:) = spval !

         ENDIF
      ENDIF

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
      CALL allocate_1D_PFTFluxes
#endif

#ifdef BGC
      CALL allocate_1D_BGCFluxes
#endif

#ifdef CatchLateralFlow
      CALL allocate_1D_CatchFluxes
#endif

#ifdef URBAN_MODEL
      CALL allocate_1D_UrbanFluxes
#endif

#ifdef DataAssimilation
      CALL allocate_1D_DAFluxes
#endif

   END SUBROUTINE allocate_1D_Fluxes

   SUBROUTINE deallocate_1D_Fluxes ()
   ! --------------------------------------------------------------------
   ! deallocates memory for CoLM 1d [numpatch] variables
   ! --------------------------------------------------------------------
   USE MOD_MPAS_MPI
   USE MOD_LandPatch

      IF (.true.) THEN

         IF (numpatch > 0) THEN

            deallocate ( taux    )  ! wind stress: E-W [kg/m/s2]
            deallocate ( tauy    )  ! wind stress: N-S [kg/m/s2]
            deallocate ( fsena   )  ! sensible heat from canopy height to atmosphere [W/m2]
            deallocate ( lfevpa  )  ! latent heat flux from canopy height to atmosphere [W/m2]
            deallocate ( fevpa   )  ! evapotranspiration from canopy to atmosphere [mm/s]
            deallocate ( fsenl   )  ! sensible heat from leaves [W/m2]
            deallocate ( fevpl   )  ! evaporation+transpiration from leaves [mm/s]
            deallocate ( etr     )  ! transpiration rate [mm/s]
            deallocate ( fseng   )  ! sensible heat flux from ground [W/m2]
            deallocate ( fevpg   )  ! evaporation heat flux from ground [mm/s]
            deallocate ( fgrnd   )  ! ground heat flux [W/m2]
            deallocate ( sabvsun )  ! solar absorbed by sunlit vegetation [W/m2]
            deallocate ( sabvsha )  ! solar absorbed by shaded vegetation [W/m2]
            deallocate ( sabg    )  ! solar absorbed by ground  [W/m2]
            deallocate ( sr      )  ! incident direct beam vis solar radiation (W/m2)
            deallocate ( solvd   )  ! incident direct beam vis solar radiation (W/m2)
            deallocate ( solvi   )  ! incident diffuse beam vis solar radiation (W/m2)
            deallocate ( solnd   )  ! incident direct beam nir solar radiation (W/m2)
            deallocate ( solni   )  ! incident diffuse beam nir solar radiation (W/m2)
            deallocate ( srvd    )  ! reflected direct beam vis solar radiation (W/m2)
            deallocate ( srvi    )  ! reflected diffuse beam vis solar radiation (W/m2)
            deallocate ( srnd    )  ! reflected direct beam nir solar radiation (W/m2)
            deallocate ( srni    )  ! reflected diffuse beam nir solar radiation (W/m2)
            deallocate ( solvdln )  ! incident direct beam vis solar radiation at local noon(W/m2)
            deallocate ( solviln )  ! incident diffuse beam vis solar radiation at local noon(W/m2)
            deallocate ( solndln )  ! incident direct beam nir solar radiation at local noon(W/m2)
            deallocate ( solniln )  ! incident diffuse beam nir solar radiation at local noon(W/m2)
            deallocate ( srvdln  )  ! reflected direct beam vis solar radiation at local noon(W/m2)
            deallocate ( srviln  )  ! reflected diffuse beam vis solar radiation at local noon(W/m2)
            deallocate ( srndln  )  ! reflected direct beam nir solar radiation at local noon(W/m2)
            deallocate ( srniln  )  ! reflected diffuse beam nir solar radiation at local noon(W/m2)
#ifdef HYPERSPECTRAL
            deallocate ( sol_dir_ln_hires )  ! incident direct beam vis solar radiation at local noon(W/m2)
            deallocate ( sol_dif_ln_hires )  ! incident diffuse beam vis solar radiation at local noon(W/m2)
            deallocate ( sr_dir_ln_hires  )  ! reflected direct beam nir solar radiation at local noon(W/m2)
            deallocate ( sr_dif_ln_hires  )  ! reflected diffuse beam nir solar radiation at local noon(W/m2)
#endif
            deallocate ( olrg    )  ! outgoing long-wave radiation from ground+canopy [W/m2]
            deallocate ( rnet    )  ! net radiation by surface [W/m2]
            deallocate ( xerr    )  ! the error of water balance [mm/s]
            deallocate ( zerr    )  ! the error of energy balance [W/m2]
            deallocate ( frcsat  )  ! fraction of saturation area [-]
            deallocate ( rsur    )  ! surface runoff (mm h2o/s)
            deallocate ( rsur_se )  ! saturation excess surface runoff (mm h2o/s)
            deallocate ( rsur_ie )  ! infiltration excess surface runoff (mm h2o/s)
            deallocate ( rsub    )  ! subsurface runoff (mm h2o/s)
            deallocate ( rnof    )  ! total runoff (mm h2o/s)
            deallocate ( qintr   )  ! interception (mm h2o/s)
            deallocate ( qinfl   )  ! infiltration (mm h2o/s)
            deallocate ( qdrip   )  ! throughfall (mm h2o/s)
            deallocate ( assim   )  ! canopy assimilation rate (mol m-2 s-1)
            deallocate ( respc   )  ! canopy respiration (mol m-2 s-1)

            deallocate ( qcharge )  ! groundwater recharge [mm/s]
            deallocate ( qlayer  )  ! water flux between soil layer [mm h2o/s]
            deallocate ( lake_deficit )  ! lake deficit due to evaporation (mm h2o/s)

            deallocate ( oroflag )  !

            deallocate ( sensors )  !

         ENDIF
      ENDIF

#if (defined LULC_IGBP_PFT || defined LULC_IGBP_PC)
      CALL deallocate_1D_PFTFluxes
#endif

#ifdef BGC
      CALL deallocate_1D_BGCFluxes
#endif

#ifdef CatchLateralFlow
      CALL deallocate_1D_CatchFluxes
#endif

#ifdef URBAN_MODEL
      CALL deallocate_1D_UrbanFluxes
#endif

#ifdef DataAssimilation
      CALL deallocate_1D_DAFluxes
#endif

   END SUBROUTINE deallocate_1D_Fluxes

END MODULE MOD_Vars_1DFluxes
! ---------- EOP ------------
