#include <define.h>
#ifdef BGC
MODULE MOD_BGC_Veg_CNVegStructUpdate

!----------------------------------------------------------------------------------
! !DESCRIPTION:
! On the radiation time step, USE C state variables and epc to diagnose
! vegetation structure (LAI, SAI, height)
!
! ORIGINAL:
! The Community Land Model version 5.0 (CLM5)
!
! REVISION:
! Xingjie Lu, 2021, revised the CLM5 code to be compatible with CoLM code structure.
!

   USE MOD_Precision
   USE MOD_Namelist, only: DEF_USE_LAIFEEDBACK, DEF_USE_Fire
   USE MOD_Vars_Global, only: nc3crop, nc3irrig, nbrdlf_evr_shrub, nbrdlf_dcd_brl_shrub, &
                                 npcropmin, ntmp_corn, nirrig_tmp_corn, ntrp_corn, nirrig_trp_corn, &
                                 nsugarcane, nirrig_sugarcane, nmiscanthus, nirrig_miscanthus, &
                                 nswitchgrass, nirrig_switchgrass, noveg

   USE MOD_Vars_PFTimeVariables, only: lai_p, tlai_p, tsai_p, leafc_p, deadstemc_p, harvdate_p
   USE MOD_Vars_TimeVariables, only: lai, tlai
#ifdef CROP
   USE MOD_BGC_Vars_PFTimeVariables, only: peaklai_p
#endif
   USE MOD_Vars_PFTimeInvariants, only: pftclass, pftfrac
   USE MOD_BGC_Vars_TimeVariables, only: farea_burned
   USE MOD_Const_PFT, only: dsladlai, slatop, laimx, woody

   !CLM5
   PUBLIC :: CNVegStructUpdate
  !-----------------------------------------------------------------------

CONTAINS

  !-----------------------------------------------------------------------
   SUBROUTINE CNVegStructUpdate(i,ps,pe,deltim,npcropmin)

   integer,intent(in)  :: i         ! patch index
   integer,intent(in)  :: ps        ! start pft index
   integer,intent(in)  :: pe        ! END pft index
   real(r8),intent(in) :: deltim    ! time step in seconds
   integer,intent(in)  :: npcropmin ! first crop pft index

   ! !LOCAL VARIABLES:
   integer  :: p,c,g      ! indices
   integer  :: fp         ! lake filter indices
   real(r8) :: stocking                            ! #stems / ha (stocking density)
   real(r8) :: ol         ! thickness of canopy layer covered by snow (m)
   real(r8) :: fb         ! fraction of canopy layer covered by snow
   real(r8) :: tlai_old   ! for USE in Zeng tsai formula
   real(r8) :: tsai_old   ! for USE in Zeng tsai formula
   real(r8) :: tsai_min   ! PATCH derived minimum tsai
   real(r8) :: tsai_alpha ! monthly decay rate of tsai
   real(r8) :: frac_sno_adjusted ! frac_sno adjusted per frac_sno_threshold

   real(r8), parameter :: dtsmonth = 2592000._r8 ! number of seconds in a 30 day month (60x60x24x30)
   real(r8), parameter :: frac_sno_threshold = 0.999_r8  ! frac_sno values greater than this are treated as 1
   real(r8), parameter :: natlaimx = 8._r8
   real(r8), parameter :: theta = 0.8_r8
   integer m, ivt
!-----------------------------------------------------------------------
! tsai formula from Zeng et. al. 2002, Journal of Climate, p1835
!
! tsai(m) = max( tsai_alpha(ivt(m))*tsai_old + max(tlai_old-tlai(m),0_r8), tsai_min(ivt(m)) )
! notes:
! * RHS tsai & tlai are from previous timestep
! * should create tsai_alpha(ivt(m)) & tsai_min(ivt(m)) in pftconMod.F90 - slevis
! * all non-crop patches USE same values:
!   crop    tsai_alpha,tsai_min = 0.0,0.1
!   noncrop tsai_alpha,tsai_min = 0.5,1.0  (includes bare soil and urban)
!-------------------------------------------------------------------------------

      ! patch loop

      lai (i) = 0._r8
      DO m = ps, pe
         ivt = pftclass(m)
         IF (ivt /= noveg) THEN

            tlai_old = tlai_p(m) ! n-1 value
            tsai_old = tsai_p(m) ! n-1 value

            IF(DEF_USE_LAIFEEDBACK)THEN
               tlai_p(m) = ((natlaimx + slatop(ivt) * leafc_p(m)) &
                         - sqrt((natlaimx + slatop(ivt) * leafc_p(m))**2 &
                         - 4 * theta * natlaimx * slatop(ivt) * leafc_p(m)))/ (2*theta)
               tlai_p(m) = max(0._r8, tlai_p(m))
               lai_p (m) = tlai_p(m)
            ENDIF

            ! update the stem area index and height based on LAI, stem mass, and veg type.
            ! With the exception of htop for woody vegetation, this follows the DGVM logic.

            ! tsai formula from Zeng et. al. 2002, Journal of Climate, p1835 (see notes)
            ! Assumes doalb time step .eq. CLM time step, SAI min and monthly decay factor
            ! alpha are set by PFT, and alpha is scaled to CLM time step by multiplying by
            ! deltim and dividing by dtsmonth (seconds in average 30 day month)
            ! tsai_min scaled by 0.5 to match MODIS satellite derived values
            IF (ivt == nc3crop .or. ivt == nc3irrig) THEN ! generic crops

               tsai_alpha = 1.0_r8-1.0_r8*deltim/dtsmonth
               tsai_min = 0.1_r8
            ELSE
               tsai_alpha = 1.0_r8-0.5_r8*deltim/dtsmonth
               tsai_min = 1.0_r8
            ENDIF
            tsai_min = tsai_min * 0.5_r8
            tsai_p(m) = max(tsai_alpha*tsai_old+max(tlai_old-tlai_p(m),0._r8),tsai_min)

            ! calculate vegetation physiological parameters used in biomass heat storage
            !
            IF (woody(ivt) == 1._r8) THEN

               ! trees and shrubs for now have a very simple allometry, with hard-wired
               ! stem taper (height:radius) and nstem from PFT parameter file
            ELSE IF (ivt >= npcropmin) THEN ! prognostic crops
#ifdef CROP
               IF (tlai_p(m) >= laimx(ivt)) peaklai_p(m) = 1 ! used in CNAllocation

               IF (ivt == ntmp_corn .or. ivt == nirrig_tmp_corn .or. &
                   ivt == ntrp_corn .or. ivt == nirrig_trp_corn .or. &
                   ivt == nsugarcane .or. ivt == nirrig_sugarcane .or. &
                   ivt == nmiscanthus .or. ivt == nirrig_miscanthus .or. &
                   ivt == nswitchgrass .or. ivt == nirrig_switchgrass) THEN
                  tsai_p(m) = 0.1_r8 * tlai_p(m)
               ELSE
                  tsai_p(m) = 0.2_r8 * tlai_p(m)
               ENDIF

               ! "stubble" after harvest
               IF (harvdate_p(m) < 999 .and. tlai_p(m) == 0._r8) THEN
                  peaklai_p(m) = 0
                  IF(DEF_USE_Fire)THEN
                        tsai_p(m) = 0.25_r8*(1._r8-farea_burned(i)*0.90_r8)    !changed by F. Li and S. Levis
                  ENDIF
               ENDIF
#endif
            ENDIF

         ENDIF

! adjust lai and sai for burying by snow.
! snow burial fraction for short vegetation (e.g. grasses, crops) changes with vegetation height
! accounts for a 20% bending factor, as used in Lombardozzi et al. (2018) GRL 45(18), 9889-9897

! NOTE: The following snow burial code is duplicated in SatellitePhenologyMod.
! Changes in one place should be accompanied by similar changes in the other.
         lai(i) = lai(i) + lai_p(m) * pftfrac(m)
      ENDDO
      tlai(i) = lai(i)

   END SUBROUTINE CNVegStructUpdate

END MODULE MOD_BGC_Veg_CNVegStructUpdate
#endif
