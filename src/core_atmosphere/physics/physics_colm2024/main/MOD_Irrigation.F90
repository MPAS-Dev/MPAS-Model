#include <define.h>
#ifdef CROP
MODULE MOD_Irrigation

!  DESCRIPTION:
!      This MODULE has all irrigation related subroutines for irrigated crop at either IGBP/USGS or PFT Land type classification and even in the C and N cycle.
   USE MOD_Precision
   USE MOD_TimeManager
   USE MOD_Namelist, only: DEF_simulation_time, DEF_IRRIGATION_ALLOCATION, DEF_USE_VariablySaturatedFlow
   USE MOD_Const_Physical, only: tfrz, denice, denh2o
   USE MOD_Const_PFT, only: irrig_crop
   USE MOD_LandPFT, only : patch_pft_s, patch_pft_e
   USE MOD_Vars_Global, only: irrig_start_time, irrig_max_depth, irrig_threshold_fraction, irrig_supply_fraction, irrig_min_cphase, irrig_max_cphase, irrig_time_per_day, &
       irrig_method_drip, irrig_method_sprinkler, irrig_method_flood, irrig_method_paddy
   USE MOD_Qsadv, only: qsadv
   USE MOD_Vars_TimeInvariants, only: pondmx, &
#ifdef vanGenuchten_Mualem_SOIL_MODEL
       theta_r, alpha_vgm, n_vgm, L_vgm, fc_vgm, sc_vgm,&
#endif
       porsl, psi0, bsw
   USE MOD_Vars_TimeVariables, only: tref, t_soisno, wliq_soisno, wice_soisno, zwt, wa, &
       irrig_rate, sum_irrig, sum_deficit_irrig, sum_irrig_count, n_irrig_steps_left, &
       tairday, usday, vsday, pairday, rnetday, fgrndday, potential_evapotranspiration,&
       groundwater_demand, groundwater_supply, reservoirriver_demand, reservoirriver_supply, &
       reservoir_supply, river_supply, runoff_supply, &
       waterstorage, deficit_irrig, actual_irrig, irrig_gw_alloc, irrig_sw_alloc, zwt_stand
   USE MOD_Vars_PFTimeInvariants, only: pftclass
   USE MOD_Vars_PFTimeVariables, only: irrig_method_p
   USE MOD_BGC_Vars_PFTimeVariables, only: cphase_p
   USE MOD_Vars_1DForcing, only: forc_t, forc_frl, forc_psrf, forc_us, forc_vs
   USE MOD_Vars_1DFluxes, only: sabg, sabvsun, sabvsha, olrg, fgrnd
   USE MOD_Hydro_SoilFunction, only: soil_vliq_from_psi
   USE MOD_MPAS_MPI
   IMPLICIT NONE

   PUBLIC :: CalIrrigationNeeded
   PUBLIC :: CalIrrigationApplicationFluxes

CONTAINS

   SUBROUTINE CalIrrigationNeeded(i,idate,nl_soil,nbedrock,z_soi,zi_soi,dz_soi,deltim,dlon,npcropmin)

      !   DESCRIPTION:
      !   This SUBROUTINE is used to calculate how much irrigation needed in each irrigated crop patch
      integer , intent(in) :: i
      integer , intent(in) :: idate(3)
      integer , intent(in) :: nl_soil
      integer , intent(in) :: nbedrock
      real(r8), intent(in) :: z_soi(1:nl_soil)
      real(r8), intent(in) :: zi_soi(1:nl_soil)
      real(r8), intent(in) :: dz_soi(1:nl_soil)
      real(r8), intent(in) :: deltim
      real(r8), intent(in) :: dlon
      integer , intent(in) :: npcropmin

      ! local
      integer :: ps, pe, m
      integer :: irrig_nsteps_per_day
      logical :: check_for_irrig
      ps = patch_pft_s(i)
      pe = patch_pft_e(i)

      !  initialize irrigation
      deficit_irrig(i) = 0._r8
      actual_irrig(i) = 0._r8
      groundwater_demand(i) = 0._r8
      groundwater_supply(i) = 0._r8
      reservoirriver_demand(i) = 0._r8
      reservoirriver_supply(i) = 0._r8
      reservoir_supply(i) = 0._r8
      river_supply(i) = 0._r8
      runoff_supply(i) = 0._r8

      !  zero irrigation at the begin of the new year
      if (idate(2) == 1 .and. idate(3) == deltim)then
         sum_irrig(i) = 0._r8
         sum_deficit_irrig(i) = 0._r8
         sum_irrig_count(i) = 0._r8
         zwt_stand(i) = zwt(i) + 1._r8
         zwt_stand(i) = max(0., zwt_stand(i))
         zwt_stand(i) = min(80., zwt_stand(i))
      end if

      ! !   calculate last day potential evapotranspiration
      ! CALL CalPotentialEvapotranspiration(i,idate,dlon,deltim)

      !   calculate whether irrigation needed
      CALL PointNeedsCheckForIrrig(i,ps,pe,idate,deltim,dlon,npcropmin,check_for_irrig)

      !   calculate irrigation needed
      IF (check_for_irrig) THEN
         CALL CalIrrigationPotentialNeeded(i,ps,pe,nl_soil,nbedrock,z_soi,dz_soi)
         call CalIrrigationLimitedSupply(i,nl_soil,deltim,dz_soi,zi_soi)
      ENDIF

      !   calculate irrigation rate kg/m2->mm/s
      IF ((check_for_irrig) .and. (deficit_irrig(i) > 0)) THEN
         sum_deficit_irrig(i) = sum_deficit_irrig(i) + deficit_irrig(i)
      ENDIF
      IF ((check_for_irrig) .and. (actual_irrig(i) > 0)) THEN
         irrig_nsteps_per_day = nint(irrig_time_per_day/deltim)
         irrig_rate(i) = actual_irrig(i)/deltim/irrig_nsteps_per_day
         n_irrig_steps_left(i) = irrig_nsteps_per_day
         sum_irrig(i) = sum_irrig(i) + actual_irrig(i)
         sum_irrig_count(i) = sum_irrig_count(i) + 1._r8
      ENDIF
   END SUBROUTINE CalIrrigationNeeded

   SUBROUTINE CalIrrigationPotentialNeeded(i,ps,pe,nl_soil,nbedrock,z_soi,dz_soi)

      !   DESCRIPTION:
      !   This SUBROUTINE is used to calculate how much irrigation needed in each irrigated crop patch without water supply restriction
      integer , intent(in) :: i
      integer , intent(in) :: ps, pe
      integer , intent(in) :: nbedrock
      integer , intent(in) :: nl_soil
      real(r8), intent(in) :: z_soi(1:nl_soil)
      real(r8), intent(in) :: dz_soi(1:nl_soil)

      !   local variables
      integer  :: j
      integer  :: m
      logical  :: reached_max_depth
      real(r8) :: h2osoi_liq_tot
      real(r8) :: h2osoi_liq_target_tot
      real(r8) :: h2osoi_liq_wilting_point_tot
      real(r8) :: h2osoi_liq_field_capacity_tot
      real(r8) :: h2osoi_liq_saturation_capacity_tot
      real(r8) :: h2osoi_liq_wilting_point(1:nl_soil)
      real(r8) :: h2osoi_liq_field_capacity(1:nl_soil)
      real(r8) :: h2osoi_liq_saturation_capacity(1:nl_soil)
      real(r8) :: h2osoi_liq_at_threshold

      real(r8) :: smpswc = -1.5e5
      real(r8) :: smpsfc = -3.3e3

      !  initialize local variables
      reached_max_depth = .false.
      h2osoi_liq_tot = 0._r8
      h2osoi_liq_target_tot = 0._r8
      h2osoi_liq_wilting_point_tot = 0._r8
      h2osoi_liq_field_capacity_tot = 0._r8
      h2osoi_liq_saturation_capacity_tot = 0._r8

      !  calculate wilting point and field capacity
      DO j = 1, nl_soil
         IF (t_soisno(j,i) > tfrz .and. porsl(j,i) >= 1.e-6) THEN
#ifdef Campbell_SOIL_MODEL
            h2osoi_liq_wilting_point(j) = denh2o*dz_soi(j)*porsl(j,i)*((smpswc/psi0(j,i))**(-1/bsw(j,i)))
            h2osoi_liq_field_capacity(j) = denh2o*dz_soi(j)*porsl(j,i)*((smpsfc/psi0(j,i))**(-1/bsw(j,i)))
            h2osoi_liq_saturation_capacity(j) = denh2o*dz_soi(j)*porsl(j,i)
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
            h2osoi_liq_wilting_point(j) = soil_vliq_from_psi(smpswc, porsl(j,i), theta_r(j,i), psi0(j,i), 5, &
              (/alpha_vgm(j,i), n_vgm(j,i), L_vgm(j,i), sc_vgm(j,i), fc_vgm(j,i)/))
            h2osoi_liq_wilting_point(j) = denh2o*dz_soi(j)*h2osoi_liq_wilting_point(j)
            h2osoi_liq_field_capacity(j) = soil_vliq_from_psi(smpsfc, porsl(j,i), theta_r(j,i), psi0(j,i), 5, &
              (/alpha_vgm(j,i), n_vgm(j,i), L_vgm(j,i), sc_vgm(j,i), fc_vgm(j,i)/))
            h2osoi_liq_field_capacity(j) = denh2o*dz_soi(j)*h2osoi_liq_field_capacity(j)
            h2osoi_liq_saturation_capacity(j) = denh2o*dz_soi(j)*porsl(j,i)
#endif
         ENDIF
      ENDDO

      !  calculate total irrigation needed in all soil layers
      DO m = ps, pe
         DO j = 1, nl_soil
            IF (.not. reached_max_depth) THEN
               IF (z_soi(j) > irrig_max_depth) THEN
                  reached_max_depth = .true.
               ELSEIF (j > nbedrock) THEN
                  reached_max_depth = .true.
               ELSEIF (t_soisno(j,i) <= tfrz) THEN
                  reached_max_depth = .true.
               ELSE
                  h2osoi_liq_tot = h2osoi_liq_tot + wliq_soisno(j,i)
                  h2osoi_liq_wilting_point_tot = h2osoi_liq_wilting_point_tot + h2osoi_liq_wilting_point(j)
                  h2osoi_liq_field_capacity_tot = h2osoi_liq_field_capacity_tot + h2osoi_liq_field_capacity(j)
                  h2osoi_liq_saturation_capacity_tot = h2osoi_liq_saturation_capacity_tot + h2osoi_liq_saturation_capacity(j)
               ENDIF
            ENDIF
         ENDDO
         IF (irrig_method_p(m) == irrig_method_drip .or. irrig_method_p(m) == irrig_method_sprinkler .or. &
               irrig_method_p(m) == irrig_method_flood) THEN
               !  flood irrigation threshold at field capacity, but irrigation amount at saturation capacity
               h2osoi_liq_target_tot = h2osoi_liq_field_capacity_tot
         ELSEIF (irrig_method_p(m) == irrig_method_paddy) THEN
               h2osoi_liq_target_tot = h2osoi_liq_saturation_capacity_tot
         ELSE
               !  default irrigation is sprinkler irrigation
               h2osoi_liq_target_tot = h2osoi_liq_field_capacity_tot
         ENDIF
      ENDDO

      !  calculate irrigation threshold
      deficit_irrig(i) = 0._r8
      h2osoi_liq_at_threshold = h2osoi_liq_wilting_point_tot + irrig_threshold_fraction * (h2osoi_liq_target_tot - h2osoi_liq_wilting_point_tot)

      !   calculate total irrigation
      DO m = ps, pe
         IF (h2osoi_liq_tot < h2osoi_liq_at_threshold) THEN
            IF (irrig_method_p(m) == irrig_method_sprinkler) THEN
                deficit_irrig(i) = irrig_supply_fraction * (h2osoi_liq_field_capacity_tot - h2osoi_liq_tot)
                ! deficit_irrig(i) = irrig_supply_fraction * (h2osoi_liq_field_capacity_tot - h2osoi_liq_tot + potential_evapotranspiration(i))
            ELSEIF (irrig_method_p(m) == irrig_method_flood) THEN
                deficit_irrig(i) = irrig_supply_fraction * (h2osoi_liq_saturation_capacity_tot - h2osoi_liq_tot)
            ELSE
                deficit_irrig(i) = irrig_supply_fraction * (h2osoi_liq_field_capacity_tot - h2osoi_liq_tot)
            ENDIF
         ELSE
            deficit_irrig(i) = 0
         ENDIF
      ENDDO

   END SUBROUTINE CalIrrigationPotentialNeeded

   SUBROUTINE CalIrrigationApplicationFluxes(i,deltim,qflx_irrig_drip,qflx_irrig_sprinkler,qflx_irrig_flood,qflx_irrig_paddy)
      !   DESCRIPTION:
      !   This SUBROUTINE is used to calculate irrigation application fluxes for each irrigated crop patch
      integer , intent(in) :: i
      real(r8), intent(in) :: deltim
      real(r8), intent(out):: qflx_irrig_drip,qflx_irrig_sprinkler,qflx_irrig_flood,qflx_irrig_paddy

      integer :: ps, pe, m

      ps = patch_pft_s(i)
      pe = patch_pft_e(i)

      qflx_irrig_drip = 0._r8
      qflx_irrig_sprinkler = 0._r8
      qflx_irrig_flood = 0._r8
      qflx_irrig_paddy = 0._r8

      !   add irrigation fluxes to precipitation or land surface
      DO m = ps, pe
         IF (n_irrig_steps_left(i) > 0) THEN
            n_irrig_steps_left(i) = n_irrig_steps_left(i) -1
            IF (waterstorage(i) - irrig_rate(i)*deltim < 0._r8) irrig_rate(i) = waterstorage(i)/deltim
            waterstorage(i) = max(waterstorage(i) - irrig_rate(i)*deltim, 0._r8)
            IF (irrig_method_p(m) == irrig_method_drip) THEN
               qflx_irrig_drip = irrig_rate(i)
            ELSEIF (irrig_method_p(m) == irrig_method_sprinkler) THEN
               qflx_irrig_sprinkler = irrig_rate(i)
            ELSEIF (irrig_method_p(m) == irrig_method_flood) THEN
               qflx_irrig_flood = irrig_rate(i)
            ELSEIF (irrig_method_p(m) == irrig_method_paddy) THEN
               qflx_irrig_paddy = irrig_rate(i)
            ELSE
               qflx_irrig_sprinkler = irrig_rate(i)
            ENDIF
         ELSE
             irrig_rate(i) = 0._r8
         ENDIF
      ENDDO
   END SUBROUTINE CalIrrigationApplicationFluxes

   SUBROUTINE PointNeedsCheckForIrrig(i,ps,pe,idate,deltim,dlon,npcropmin,check_for_irrig)
      !   DESCRIPTION:
      !   This SUBROUTINE is used to calculate whether irrigation needed in each patch
      integer , intent(in) :: i
      integer , intent(in) :: ps, pe
      integer , intent(in) :: idate(3)
      real(r8), intent(in) :: deltim
      real(r8), intent(in) :: dlon
      integer , intent(in) :: npcropmin
      logical , intent(out):: check_for_irrig

      !   local variable
      integer :: m, ivt
      real(r8):: ldate(3)
      real(r8):: seconds_since_irrig_start_time

   !  adjust flood irrigation in rice to paddy irrigaiton
      DO m = ps, pe
         ivt = pftclass(m)
         IF ((ivt == 62) .and. (irrig_method_p(m) == irrig_method_flood)) THEN
            irrig_method_p(m) = irrig_method_paddy
         ENDIF
      ENDDO

      DO m = ps, pe
         ivt = pftclass(m)
         IF ((ivt >= npcropmin) .and. (irrig_crop(ivt)) .and. &
            (cphase_p(m) >= irrig_min_cphase) .and. (cphase_p(m)<irrig_max_cphase)) THEN
            IF (DEF_simulation_time%greenwich) THEN
                CALL gmt2local(idate, dlon, ldate)
                seconds_since_irrig_start_time = ldate(3) - irrig_start_time + deltim
            ELSE
                seconds_since_irrig_start_time = idate(3) - irrig_start_time + deltim
            ENDIF
            IF ((seconds_since_irrig_start_time >= 0._r8) .and. (seconds_since_irrig_start_time < deltim)) THEN
                check_for_irrig = .true.
            ELSE
                check_for_irrig = .false.
            ENDIF
         ELSE
            check_for_irrig = .false.
         ENDIF
      ENDDO

   END SUBROUTINE PointNeedsCheckForIrrig

   ! SUBROUTINE CalPotentialEvapotranspiration(i,idate,dlon,deltim)
   !     !   DESCRIPTION:
   !     !   This SUBROUTINE is used to calculate daily potential evapotranspiration
   !     integer , intent(in) :: i
   !     integer , intent(in) :: idate(3)
   !     real(r8), intent(in) :: dlon
   !     real(r8), intent(in) :: deltim
   !     !   local variable
   !     real(r8):: ldate(3)
   !     real(r8):: seconds_since_irrig_start_time
   !     real(r8) :: es,esdT,qs,qsdT     ! saturation vapour pressure
   !     real(r8) :: evsat               ! vapour pressure
   !     real(r8) :: ur                  ! wind speed
   !     real(r8) :: delta               ! slope of saturation vapour pressure curve
   !     real(r8) :: gamma               ! Psychrometric constant

   !     IF (DEF_simulation_time%greenwich) THEN
   !         CALL gmt2local(idate, dlon, ldate)
   !         seconds_since_irrig_start_time = ldate(3) - irrig_start_time + deltim
   !     ELSE
   !         seconds_since_irrig_start_time = idate(3) - irrig_start_time + deltim
   !     ENDIF

   !     IF (((seconds_since_irrig_start_time-deltim) >= 0) .and. ((seconds_since_irrig_start_time-deltim) < deltim)) THEN
   !         tairday(i) = (forc_t(i)-tfrz)*deltim/86400
   !         usday(i) = forc_us(i)*deltim/86400
   !         vsday(i) = forc_vs(i)*deltim/86400
   !         pairday(i) = forc_psrf(i)*deltim/86400/1000
   !         rnetday(i) = (sabg(i)+sabvsun(i)+sabvsha(i)-olrg(i)+forc_frl(i))*deltim/1000000
   !         fgrndday(i) = fgrnd(i)*deltim/1000000
   !     ELSE
   !         tairday(i) = tairday(i) + (forc_t(i)-tfrz)*deltim/86400
   !         usday(i) = usday(i) + forc_us(i)*deltim/86400
   !         vsday(i) = vsday(i) + forc_vs(i)*deltim/86400
   !         pairday(i) = pairday(i) + forc_psrf(i)*deltim/86400/1000
   !         rnetday(i) = rnetday(i) + (sabg(i)+sabvsun(i)+sabvsha(i)-olrg(i)+forc_frl(i))*deltim/1000000
   !         fgrndday(i) = fgrndday(i) + fgrnd(i)*deltim/1000000
   !     ENDIF

   !     IF ((seconds_since_irrig_start_time >= 0) .and. (seconds_since_irrig_start_time < deltim)) THEN
   !         CALL qsadv(tairday(i),pairday(i),es,esdT,qs,qsdT)
   !         IF (tairday(i) > 0)THEN
   !             evsat = 0.611*EXP(17.27*tairday(i)/(tairday(i)+237.3))
   !         ELSE
   !             evsat = 0.611*EXP(21.87*tairday(i)/(tairday(i)+265.5))
   !         ENDIF
   !         ur = max(0.1,sqrt(usday(i)*usday(i)+vsday(i)*vsday(i)))
   !         delta = 4098*evsat/((tairday(i)+237.3)*(tairday(i)+237.3))
   !         gamma = 0.665*0.001*pairday(i)
   !         potential_evapotranspiration(i) = (0.408*delta*(rnetday(i)-fgrndday(i))+gamma*(900/(tairday(i)+273))*ur* &
   !             (evsat-es))/(delta+(gamma*(1+0.34*ur)))
   !     ENDIF
   ! END SUBROUTINE CalPotentialEvapotranspiration

   SUBROUTINE CalIrrigationLimitedSupply(i,nl_soil,deltim,dz_soi,zi_soi)
        !   DESCRIPTION:
        !   This subroutine is used to calculate how much irrigation supplied in each irrigated crop patch with water supply restriction
         integer,  intent(in) :: i
         integer,  intent(in) :: nl_soil
         real(r8), intent(in) :: deltim
         real(r8), intent(in) :: dz_soi(1:nl_soil)
         real(r8), intent(in) :: zi_soi(1:nl_soil)

         real(r8) :: waterstorage_supply

         IF (deficit_irrig(i) > 0._r8) THEN
            IF (DEF_IRRIGATION_ALLOCATION == 1) THEN
               actual_irrig(i) = deficit_irrig(i)
               waterstorage(i) = waterstorage(i) + actual_irrig(i)
            ELSEIF (DEF_IRRIGATION_ALLOCATION == 2) THEN
               waterstorage_supply = min(waterstorage(i), deficit_irrig(i))
               waterstorage_supply = max(waterstorage_supply, 0._r8)
               actual_irrig(i) = actual_irrig(i) + waterstorage_supply
               !  irrigation withdraw from ground water (unconfined and confined)
               IF (deficit_irrig(i) > actual_irrig(i)) THEN
                  groundwater_demand(i) = max(deficit_irrig(i) - actual_irrig(i), 0._r8)
                  CALL CalGroudwaterWithdrawal(i,nl_soil,deltim,dz_soi,zi_soi)
                  actual_irrig(i) = actual_irrig(i) + groundwater_supply(i)
                  waterstorage(i) = waterstorage(i) + groundwater_supply(i)
               ENDIF
            ELSEIF (DEF_IRRIGATION_ALLOCATION == 3) THEN
               waterstorage_supply = min(waterstorage(i), deficit_irrig(i))
               waterstorage_supply = max(waterstorage_supply, 0._r8)
               actual_irrig(i) = actual_irrig(i) + waterstorage_supply
               !   irrigation withdraw from ground water (unconfined and confined)
               IF (deficit_irrig(i) > actual_irrig(i)) THEN
                  groundwater_demand(i) = max((deficit_irrig(i) - actual_irrig(i))*irrig_gw_alloc(i), 0._r8)
                  CALL CalGroudwaterWithdrawal(i,nl_soil,deltim,dz_soi,zi_soi)
                  actual_irrig(i) = actual_irrig(i) + groundwater_supply(i)
                  waterstorage(i) = waterstorage(i) + groundwater_supply(i)
               ENDIF
            ENDIF
         ENDIF
   END SUBROUTINE CalIrrigationLimitedSupply


   SUBROUTINE CalGroudwaterWithdrawal(i,nl_soil,deltim,dz_soi,zi_soi)
      !   DESCRIPTION:
      !   This subroutine is used to calculate irrigation withdrawals for groudwater
      integer,  intent(in) :: i
      integer,  intent(in) :: nl_soil
      real(r8), intent(in) :: deltim
      real(r8), intent(in) :: dz_soi(1:nl_soil)
      real(r8), intent(in) :: zi_soi(1:nl_soil)

      IF (.not. DEF_USE_VariablySaturatedFlow) THEN
         CALL CalWithdrawalWATER(i,nl_soil,deltim,dz_soi,zi_soi)
      ELSE
         groundwater_supply(i) = groundwater_demand(i)
      ENDIF
   END SUBROUTINE CalGroudwaterWithdrawal



   subroutine CalWithdrawalWATER(i,nl_soil,deltim,dz_soi,zi_soi)
      !   DESCRIPTION:
      !   This subroutine is used to calculate how much irrigation supplied in each irrigated crop patch with groundwater supply restriction
      IMPLICIT NONE
      integer,  INTENT(in) :: i
      integer,  INTENT(in) :: nl_soil
      real(r8), INTENT(in) :: deltim              ! land model time step (sec)
      real(r8), INTENT(in) :: dz_soi  (1:nl_soil) ! layer depth (m)
      real(r8), INTENT(in) :: zi_soi  (1:nl_soil) ! interface level below a "z" level (m)

      ! LOCAL ARGUMENTS
      integer  :: j                ! indices
      integer  :: jwt              ! index of the soil layer right above the water table (-)
      real(r8) :: dzmm(1:nl_soil)  ! layer thickness (mm)
      real(r8) :: vol_ice(1:nl_soil)! partitial volume of ice lens in layer
      real(r8) :: eff_porosity(1:nl_soil)! effective porosity = porosity - vol_ice
      real(r8) :: xs               ! water needed to bring soil moisture to watmin (mm)
      real(r8) :: xsi              ! excess soil water above saturation at layer i (mm)
      real(r8) :: xs1              ! excess soil water above saturation at layer 1 (mm)
      real(r8) :: pump_total
      real(r8) :: pump_layer
      real(r8) :: max_groundwater_supply
      real(r8) :: s_y
      real(r8) :: rous             ! specific yield [-]
   ! -------------------------------------------------------------------------

      do j = 1, nl_soil
         vol_ice(j) = min(porsl(j,i), wice_soisno(j,i)/(dz_soi(j)*denice))
         eff_porosity(j) = max(0.01, porsl(j,i)-vol_ice(j))
      end do

      ! Convert layer thicknesses from m to mm
      DO j = 1,nl_soil
         dzmm(j) = dz_soi(j)*1000.
      ENDDO

      ! The layer index of the first unsaturated layer,
      ! i.e., the layer right above the water table
      jwt = nl_soil
      ! allow jwt to equal zero when zwt is in top layer
      DO j = 1, nl_soil
         IF(zwt(i) <= zi_soi(j)) THEN
            jwt = j-1
            exit
         ENDIF
      ENDDO

      rous = porsl(nl_soil,i)*(1.-(1.-1.e3*zwt(i)/psi0(nl_soil,i))**(-1./bsw(nl_soil,i)))
      rous = max(rous,0.02)

   !-- Water table is below the soil column  ----------------------------------------
      IF (jwt == nl_soil) THEN
         max_groundwater_supply = max(1.e3*(zwt_stand(i)-zwt(i))*rous, 0._r8)
         groundwater_supply(i) = min(groundwater_demand(i), max_groundwater_supply)
         wa(i) = wa(i) - groundwater_supply(i)
         zwt(i) = max(0., zwt(i) + groundwater_supply(i)/1000./rous)
         wliq_soisno(nl_soil,i) = wliq_soisno(nl_soil,i) + max(0.,(wa(i)-5000.))
         wa(i) = min(wa(i), 5000.)
      ELSE
   !-- Water table within soil layers 1-9  ------------------------------------------
   !============================== RSUB_TOP =========================================
         !-- Now remove water via pump
         pump_total = - groundwater_demand(i)
         DO j = jwt+1, nl_soil
               ! use analytical expression for specific yield
               s_y = porsl(j,i) * ( 1. - (1.-1.e3*zwt(i)/psi0(j,i))**(-1./bsw(j,i)))
               s_y = max(s_y,0.02)

               pump_layer = max(pump_total, -(s_y*(zi_soi(j)-zwt(i))*1.e3))
               pump_layer = min(pump_layer, 0.)
               wliq_soisno(j,i) = wliq_soisno(j,i) + pump_layer

               pump_total = pump_total - pump_layer
               groundwater_supply(i) = groundwater_supply(i) - pump_layer

               IF (pump_total >= 0.) THEN
                  zwt(i) = max(0.,zwt(i) - pump_layer/s_y/1000.)
                  exit
               ELSE
                  zwt(i) = zi_soi(j)
               ENDIF
         ENDDO
   !-- Remove residual drainage  ------------------------------------------------
         max_groundwater_supply = max(1.e3*(zwt_stand(i)-zwt(i))*rous, 0._r8)
         pump_total = min(-pump_total, max_groundwater_supply)
         pump_total = max(pump_total, 0._r8)
         groundwater_supply(i) = groundwater_supply(i) + pump_total
         zwt(i) = max(0., zwt(i) + pump_total/1000./rous)
         wa(i) = wa(i) - pump_total

   !-- Recompute jwt  ---------------------------------------------------------------
         ! allow jwt to equal zero when zwt is in top layer
         jwt = nl_soil
         DO j = 1, nl_soil
               IF (zwt(i) <= zi_soi(j)) THEN
                  jwt = j-1
                  exit
               ENDIF
         ENDDO
      ENDIF   ! end of jwt if construct

      zwt(i) = max(0.0,zwt(i))
      zwt(i) = min(80.,zwt(i))


      ! Correction [2]
      ! NON-physically based corection on wliq_soisno
      ! Limit wliq_soisno to be greater than or equal to watmin.
      ! Get water needed to bring wliq_soisno equal watmin from lower layer.
      ! If insufficient water in soil layers, get from aquifer water
      xs = 0.
      DO j = 1, nl_soil
         IF (wliq_soisno(j,i) < 0.) THEN
            xs = xs + wliq_soisno(j,i)
            wliq_soisno(j,i) = 0.
         ENDIF
      ENDDO
      wa(i) = wa(i) + xs
   END SUBROUTINE CalWithdrawalWATER

END MODULE MOD_Irrigation
#endif
