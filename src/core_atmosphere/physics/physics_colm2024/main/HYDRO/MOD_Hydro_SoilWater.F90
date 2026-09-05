#include <define.h>

MODULE MOD_Hydro_SoilWater

!-------------------------------------------------------------------------
! Description:
!
!    Numerical Solver of Richards equation.
!
!    Dai, Y., Zhang, S., Yuan, H., & Wei, N. (2019).
!    Modeling Variably Saturated Flow in Stratified Soils
!      With Explicit Tracking of Wetting Front and Water Table Locations.
!    Water Resources Research. doi:10.1029/2019wr025368
!
! Created by Shupeng Zhang, 2022.
!-------------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_Hydro_SoilFunction
   USE MOD_Namelist, only: DEF_USE_PLANTHYDRAULICS
   USE MOD_UserDefFun, only: findloc_ud

   IMPLICIT NONE

   ! public subroutines and functions
   PUBLIC :: soil_water_vertical_movement
   PUBLIC :: get_water_equilibrium_state
   PUBLIC :: soilwater_aquifer_exchange
   PUBLIC :: get_zwt_from_wa


   ! boundary condition:
   ! 1: fixed pressure head
   ! 2: rainfall condition with a ponding layer on top of ground surface
   !    and a flux such as rainfall into the ponding layer
   ! 3: fixed flux
   ! 4: drainage condition with aquifers below soil columns
   integer, parameter :: BC_FIX_HEAD = 1
   integer, parameter :: BC_RAINFALL = 2
   integer, parameter :: BC_FIX_FLUX = 3
   integer, parameter :: BC_DRAINAGE = 4

   ! formula of effective hydraulic conductivity between levels
   ! Please refer to Dai et al. (2019) for definitions
   integer, parameter :: type_upstream_mean           = 1
   integer, parameter :: type_weighted_geometric_mean = 2

   integer,  parameter :: effective_hk_type  = type_weighted_geometric_mean
   integer,  parameter :: max_iters_richards = 10
   real(r8), parameter :: tol_richards = 8.e-8

#ifdef CoLMDEBUG
   integer(8) :: count_implicit = 0
   integer(8) :: count_explicit = 0
   integer(8) :: count_wet2dry  = 0
#endif

   ! private subroutines and functions
   PRIVATE :: Richards_solver

   PRIVATE :: water_balance
   PRIVATE :: initialize_sublevel_structure

   PRIVATE :: use_explicit_form

   PRIVATE :: var_perturb_level
   PRIVATE :: var_perturb_rainfall
   PRIVATE :: var_perturb_drainage

   PRIVATE :: check_and_update_level

   PRIVATE :: flux_all
   PRIVATE :: flux_sat_zone_all
   PRIVATE :: flux_sat_zone_fixed_bc
   PRIVATE :: flux_inside_hm_soil
   PRIVATE :: flux_at_unsaturated_interface
   PRIVATE :: flux_top_transitive_interface
   PRIVATE :: flux_btm_transitive_interface
   PRIVATE :: flux_both_transitive_interface

   PRIVATE :: solve_least_squares_problem
   PRIVATE :: secant_method_iteration

   PRIVATE :: find_unsat_lev_lower

CONTAINS

   ! ---- get equilibrium state ----
   SUBROUTINE get_water_equilibrium_state ( &
         zwtmm, nlev, wliq, smp, hk, wa, sp_zc, sp_zi, porsl, vl_r, psi_s, hksat, nprm, prms)

   IMPLICIT NONE

   real(r8), intent(in) :: zwtmm   ! location of water table [mm]

   integer,  intent(in) :: nlev    ! number of levels

   real(r8), intent(out) :: wliq(1:nlev) ! [mm] or [kg/m2]
   real(r8), intent(out) :: smp (1:nlev) ! [mm]
   real(r8), intent(out) :: hk  (1:nlev) ! [mm/s]
   real(r8), intent(out) :: wa    ! water in aquifer [mm]

   real(r8), intent(in) :: sp_zc (1:nlev)  ! soil parameter : centers of level [mm]
   real(r8), intent(in) :: sp_zi (0:nlev)  ! soil parameter : interfaces of level [mm]

   real(r8), intent(in) :: porsl (1:nlev)  ! soil porosity
   real(r8), intent(in) :: vl_r  (1:nlev)  ! residual soil moisture
   real(r8), intent(in) :: psi_s (1:nlev)  ! saturated capillary potential [mm, negative]
   real(r8), intent(in) :: hksat (1:nlev)  ! saturated hydraulic conductivity [mm/s]

   integer,  intent(in) :: nprm      ! number of parameters included in soil function
   real(r8), intent(in) :: prms (nprm,1:nlev)  ! parameters included in soil function

   ! Local Variables
   integer  :: izwt, ilev
   real(r8) :: psi_zwt, smp_up, vliq_up, vliq(1:nlev), psi, vl

      ! water table location
      izwt = findloc_ud(zwtmm >= sp_zi, back=.true.)

      IF (izwt <= nlev) THEN
         psi_zwt = psi_s(izwt)
      ELSE
         psi_zwt = psi_s(nlev)
      ENDIF

      DO ilev = 1, nlev
         IF (ilev < izwt) THEN
            smp (ilev) = psi_zwt - (zwtmm - sp_zc(ilev))
            vliq(ilev) = soil_vliq_from_psi (smp(ilev), porsl(ilev), vl_r(ilev), psi_s(ilev), &
               nprm, prms(:,ilev))
            wliq(ilev) = vliq(ilev) * (sp_zi(ilev)-sp_zi(ilev-1))
            hk (ilev)  = soil_hk_from_psi (smp(ilev), psi_s(ilev), hksat(ilev), nprm, prms(:,ilev))
         ELSEIF (ilev == izwt) THEN
            smp_up = psi_zwt &
               - (zwtmm-sp_zi(ilev-1)) * (sp_zi(ilev)-sp_zc(ilev))/(sp_zi(ilev)-sp_zi(ilev-1))
            vliq_up = soil_vliq_from_psi (smp_up, porsl(ilev), vl_r(ilev), psi_s(ilev), &
               nprm, prms(:,ilev))
            wliq(ilev) = vliq_up * (zwtmm-sp_zi(ilev-1)) + porsl(ilev)*(sp_zi(ilev)-zwtmm)
            vliq(ilev) = wliq(ilev) / (sp_zi(ilev)-sp_zi(ilev-1))
            smp(ilev) = soil_psi_from_vliq (vliq(ilev), porsl(ilev), vl_r(ilev), psi_s(ilev), &
               nprm, prms(:,ilev))
            hk (ilev) = soil_hk_from_psi (smp(ilev), psi_s(ilev), hksat(ilev), nprm, prms(:,ilev))
         ELSE
            wliq(ilev) = porsl(ilev) * (sp_zi(ilev)-sp_zi(ilev-1))
            smp (ilev) = psi_s(ilev)
            hk  (ilev) = hksat(ilev)
         ENDIF
      ENDDO

      IF (izwt == nlev+1) THEN
         psi = psi_zwt - (zwtmm - sp_zi(nlev)) * 0.5
         vl  = soil_vliq_from_psi (psi, porsl(nlev), vl_r(nlev), psi_s(nlev), nprm, prms(:,nlev))
         wa  = -(zwtmm-sp_zi(nlev))*(porsl(nlev)-vl)
      ELSE
         wa = 0.
      ENDIF

   END SUBROUTINE get_water_equilibrium_state

   ! --- soil water movement ---
   SUBROUTINE soil_water_vertical_movement (                                &
         nlev,       dt,         sp_zc,  sp_zi,    is_permeable,  porsl,    &
         vl_r,       psi_s,      hksat,  nprm,     prms,          porsl_wa, &
         qgtop,      etr,        rootr,  rootflux, rsubst,        qinfl,    &
         ss_dp,      zwt,        wa,     ss_vliq,  smp,           hk,       &
         qlayer,     tolerance,  wblc)

   !=======================================================================
   ! this is the main subroutine to execute the calculation of
   ! soil water movement
   !=======================================================================

   USE MOD_Const_Physical, only: tfrz

   IMPLICIT NONE

   integer,  intent(in) :: nlev    ! number of levels
   real(r8), intent(in) :: dt      ! time step (second)

   real(r8), intent(in) :: sp_zc (1:nlev)  ! soil parameter : centers of level    (mm)
   real(r8), intent(in) :: sp_zi (0:nlev)  ! soil parameter : interfaces of level (mm)

   logical,  intent(in) :: is_permeable (1:nlev)

   real(r8), intent(in) :: porsl (1:nlev)  ! soil porosity (mm^3/mm^3)
   real(r8), intent(in) :: vl_r  (1:nlev)  ! residual soil moisture (mm^3/mm^3)
   real(r8), intent(in) :: psi_s (1:nlev)  ! saturated capillary potential (mm)
   real(r8), intent(in) :: hksat (1:nlev)  ! saturated hydraulic conductivity (mm/s)

   integer,  intent(in) :: nprm      ! number of parameters included in soil function
   real(r8), intent(in) :: prms (nprm,1:nlev)  ! parameters included in soil function

   real(r8), intent(in) :: porsl_wa      ! soil porosity in aquifer (mm^3/mm^3)

   ! ground water including rain, snow melt and dew formation (mm/s)
   real(r8), intent(in) :: qgtop

   real(r8), intent(in) :: etr           ! transpiration rate (mm/s)
   real(r8), intent(in) :: rootr(1:nlev) ! root fractions (percentage)
   real(r8), intent(in) :: rootflux(1:nlev) ! root water uptake from different layers (mm/s)

   real(r8), intent(in)  :: rsubst ! subsurface runoff (mm/s)
   real(r8), intent(out) :: qinfl  ! infiltration into soil (mm/s)

   real(r8), intent(inout) :: ss_dp ! soil water state : depth of ponding water (mm)
   real(r8), intent(inout) :: zwt   ! location of water table (mm)
   real(r8), intent(inout) :: wa    ! water deficit in aquifer (negative, mm)
   real(r8), intent(inout) :: ss_vliq(1:nlev) ! volume content of liquid water (mm^3/mm^3)

   real(r8), intent(out) :: smp(1:nlev) ! soil matrix potential (mm)
   real(r8), intent(out) :: hk (1:nlev) ! hydraulic conductivity (mm/s)

   real(r8), intent(out) :: qlayer(0:nlev) ! water flux at interface of soil layers (mm/s)

   real(r8), intent(in)  :: tolerance

   real(r8), intent(out) :: wblc

   ! Local variables
   integer  :: lb, ub, ilev, izwt
   real(r8) :: sumroot, deficit, etrdef, wexchange
   real(r8) :: dp_m1, psi, vliq, zwtp, air
   logical  :: is_sat

   real(r8) :: sp_dz  (1:nlev)
   real(r8) :: etroot (1:nlev)
   real(r8) :: ss_wt  (1:nlev)

   integer  :: ubc_typ_sub
   real(r8) :: ubc_val_sub
   integer  :: lbc_typ_sub
   real(r8) :: lbc_val_sub

   real(r8) :: w_sum_before, w_sum_after, vl_before(nlev), wt_before, wa_before, dp_before

   real(r8) :: tol_q, tol_z, tol_v, tol_p

      sp_dz(1:nlev) = sp_zi(1:nlev) - sp_zi(0:nlev-1)

      dp_m1 = ss_dp

      ! tolerances
      tol_q = tolerance / real(nlev,r8) / dt /2.0
      tol_z = tol_q * dt
      tol_v = tol_z / maxval(sp_dz)
      tol_p = 1.0e-14

      ! water table location
      izwt = findloc_ud(zwt >= sp_zi, back=.true.)

      ! total water mass
      w_sum_before = ss_dp
      DO ilev = 1, nlev
         IF (is_permeable(ilev)) THEN
            IF (ilev <= izwt-1) THEN
               w_sum_before = w_sum_before + ss_vliq(ilev) * sp_dz(ilev)
            ELSEIF (ilev == izwt) THEN
               w_sum_before = w_sum_before + ss_vliq(izwt) * (zwt - sp_zi(izwt-1))
               w_sum_before = w_sum_before + porsl  (izwt) * (sp_zi(izwt) - zwt)
            ELSE
               w_sum_before = w_sum_before + porsl(ilev) * sp_dz(ilev)
            ENDIF
         ENDIF
      ENDDO
      w_sum_before = w_sum_before + wa

      vl_before = ss_vliq
      wt_before = zwt
      wa_before = wa
      dp_before = ss_dp

      ! transpiration
      IF(.not. DEF_USE_PLANTHYDRAULICS)THEN
         sumroot   = sum(rootr, mask = is_permeable .and. (rootr > 0.))
         etroot(:) = 0.
         IF (sumroot > 0.) THEN
            WHERE (is_permeable)
               etroot = etr * max(rootr, 0.) / sumroot
            END WHERE
            etrdef = 0.
         ELSE
            etrdef = etr*dt
         ENDIF
      ELSE
         etrdef = 0.
         etroot(:) = rootflux
      ENDIF

      deficit = etrdef

      DO ilev = 1, izwt-1
         IF (is_permeable(ilev)) THEN

            ss_vliq(ilev) = (ss_vliq(ilev) * sp_dz(ilev) &
               - etroot(ilev)*dt - deficit) / sp_dz(ilev)

            IF (ss_vliq(ilev) < 0) THEN
               deficit = ( - ss_vliq(ilev)) * sp_dz(ilev)
               ss_vliq(ilev) = 0
            ELSEIF (ss_vliq(ilev) > porsl(ilev)) THEN
               deficit = - (ss_vliq(ilev) - porsl(ilev)) * sp_dz(ilev)
               ss_vliq(ilev) = porsl(ilev)
            ELSE
               deficit = 0.
            ENDIF
         ELSE
            deficit = deficit + etroot(ilev)*dt
         ENDIF
      ENDDO

      DO ilev = izwt, nlev
         deficit = deficit + etroot(ilev)*dt
      ENDDO

      ! Exchange water with aquifer
      wexchange = rsubst * dt + deficit
      CALL soilwater_aquifer_exchange ( &
         nlev, wexchange, sp_zi, is_permeable, porsl, vl_r, psi_s, hksat, &
         nprm, prms, porsl_wa, ss_dp, ss_vliq, zwt, wa, izwt)

      ! water table location
      ss_wt(:) = 0._r8
      IF ((izwt >= 1) .and. (izwt <= nlev)) THEN
         ss_wt(izwt) = sp_zi(izwt) - zwt
      ENDIF
      DO ilev = izwt+1, nlev
         ss_wt(ilev) = sp_dz(ilev)
      ENDDO

      ! Impermeable levels cut the soil column into several disconnected parts.
      ! The Richards solver is called to calculate water movement part by part.
      ub = nlev
      soilcolumn : DO WHILE (ub >= 1)

         DO WHILE (.not. is_permeable(ub))

            qlayer(ub-1:ub) = 0._r8

            IF (ub > 1) THEN
               ub = ub - 1
            ELSE
               EXIT soilcolumn
            ENDIF
         ENDDO

         lb = ub
         DO WHILE (lb > 1)
            IF (is_permeable(lb-1)) THEN
               lb = lb - 1
            ELSE
               EXIT
            ENDIF
         ENDDO

         IF (lb == 1) THEN
            ubc_typ_sub = BC_RAINFALL
            ubc_val_sub = qgtop
         ELSE
            ubc_typ_sub = BC_FIX_FLUX
            ubc_val_sub = 0
         ENDIF

         IF ((ub == nlev) .and. (izwt > nlev)) THEN
            lbc_typ_sub = BC_DRAINAGE
            lbc_val_sub = 0.
         ELSE
            lbc_typ_sub = BC_FIX_FLUX
            lbc_val_sub = 0.
         ENDIF

         CALL Richards_solver ( &
            lb, ub, dt, sp_zc(lb:ub), sp_zi(lb-1:ub), &
            porsl(lb:ub), vl_r(lb:ub), psi_s(lb:ub), hksat(lb:ub), nprm, prms(:,lb:ub), &
            porsl_wa, &
            ubc_typ_sub, ubc_val_sub, lbc_typ_sub, lbc_val_sub, &
            ss_dp, wa, ss_vliq(lb:ub), ss_wt(lb:ub), qlayer(lb-1:ub), &
            tol_q, tol_z, tol_v, tol_p)

         ub = lb - 1

      ENDDO soilcolumn

      IF (.not. is_permeable(1)) THEN
         ss_dp = max(ss_dp + qgtop * dt, 0._r8)
      ENDIF

      IF (wa >= 0) THEN
         DO ilev = nlev, 1, -1
            is_sat = (.not. is_permeable(ilev)) &
               .or. (ss_vliq(ilev) > porsl(ilev) - tol_v) &
               .or. (ss_wt  (ilev) > sp_dz(ilev) - tol_z)
            IF (.not. is_sat) THEN
               zwt = sp_zi(ilev) - ss_wt(ilev)
               EXIT
            ENDIF
         ENDDO

         IF (is_sat) THEN
            zwt = 0._r8
         ENDIF
      ELSE
         CALL get_zwt_from_wa ( &
            porsl_wa, vl_r(nlev), psi_s(nlev), hksat(nlev), &
            nprm, prms(:,nlev), tol_v, tol_z, &
            wa, sp_zi(nlev), zwt)
      ENDIF

      izwt = findloc_ud(zwt >= sp_zi, back=.true.)
      DO ilev = izwt-1, 1, -1
         IF (is_permeable(ilev)) THEN
            ss_vliq(ilev) = (ss_vliq(ilev)*(sp_dz(ilev)-ss_wt(ilev)) &
               + porsl(ilev)*ss_wt(ilev)) / sp_dz(ilev)
         ENDIF
      ENDDO

      qinfl = qgtop - (ss_dp - dp_m1)/dt

      ! total water mass
      w_sum_after = ss_dp
      DO ilev = 1, nlev
         IF (is_permeable(ilev)) THEN
            IF (ilev <= izwt-1) THEN
               w_sum_after = w_sum_after + ss_vliq(ilev) * sp_dz(ilev)
            ELSEIF (ilev == izwt) THEN
               w_sum_after = w_sum_after + ss_vliq(izwt) * (zwt - sp_zi(izwt-1))
               w_sum_after = w_sum_after + porsl  (izwt) * (sp_zi(izwt) - zwt)
            ELSE
               w_sum_after = w_sum_after + porsl(ilev) * sp_dz(ilev)
            ENDIF
         ENDIF
      ENDDO
      w_sum_after = w_sum_after + wa

      wblc = w_sum_after - (w_sum_before + (qgtop - sum(etroot) - rsubst) * dt - etrdef)

      IF (abs(wblc) > tolerance) THEN
         write(*,*) 'soil_water_vertical_movement balance error: ', wblc, ' in mm.'
         write(*,*) 'qtop: ', qgtop, 'etr: ', sum(etroot)+etrdef, 'rsubst: ', rsubst
         write(*,*) 'permeable (1-10): ', is_permeable
         write(*,*) 'ponding depth: ', dp_before, '(before) to ', ss_dp, '(after)'
         write(*,*) 'porsl (c1) and liquid volume before (c2) and after (c3) (1-10) : '
         DO ilev = 1, nlev
            write(*,*) porsl(ilev), vl_before(ilev), ss_vliq(ilev)
         ENDDO
         write(*,*) 'water table  : ', wt_before, '(before) to ', zwt, '(after)'
         write(*,*) 'aquifer      : ', wa_before, '(before) to ', wa, '(after)'
      ENDIF

      DO ilev = 1, nlev
         IF (ilev < izwt) THEN
            smp(ilev) = soil_psi_from_vliq (ss_vliq(ilev),  porsl(ilev), vl_r(ilev), psi_s(ilev), &
               nprm, prms(:,ilev))
            hk (ilev) = soil_hk_from_psi   (smp(ilev), psi_s(ilev), hksat(ilev), nprm, prms(:,ilev))
         ELSEIF (ilev == izwt) THEN
            vliq = (ss_vliq(izwt) * (zwt - sp_zi(izwt-1)) + porsl(izwt) * (sp_zi(izwt) - zwt)) &
               / (sp_zi(izwt) - sp_zi(izwt-1))
            smp(ilev) = soil_psi_from_vliq (vliq, porsl(ilev), vl_r(ilev), psi_s(ilev), &
               nprm, prms(:,ilev))
            hk (ilev) = soil_hk_from_psi   (smp(ilev), psi_s(ilev), hksat(ilev), nprm, prms(:,ilev))
         ELSE
            smp(ilev) = psi_s(ilev)
            hk (ilev) = hksat(ilev)
         ENDIF
      ENDDO

   END SUBROUTINE soil_water_vertical_movement

   ! --- water exchange between soil water and aquifer ---
   SUBROUTINE soilwater_aquifer_exchange ( &
         nlev, exwater, sp_zi, is_permeable, porsl, vl_r, psi_s, hksat, &
         nprm, prms, porsl_wa, ss_dp, ss_vliq, zwt, wa, izwt)

   IMPLICIT NONE

   integer,  intent(in) :: nlev

   real(r8), intent(in) :: exwater ! total water exchange [mm]

   real(r8), intent(in) :: sp_zi (0:nlev)  ! soil parameter : interfaces of level [mm]

   logical,  intent(in) :: is_permeable (1:nlev)
   real(r8), intent(in) :: porsl (1:nlev)  ! soil porosity [mm^3/mm^3]
   real(r8), intent(in) :: vl_r  (1:nlev)  ! residual soil moisture [mm^3/mm^3]
   real(r8), intent(in) :: psi_s (1:nlev)  ! saturated capillary potential [mm]
   real(r8), intent(in) :: hksat (1:nlev)  ! saturated hydraulic conductivity [mm/s]

   integer,  intent(in) :: nprm      ! number of parameters included in soil function
   real(r8), intent(in) :: prms (nprm,1:nlev)  ! parameters included in soil function

   real(r8), intent(in) :: porsl_wa       ! soil porosity in aquifer [mm^3/mm^3]

   real(r8), intent(inout) :: ss_dp           ! depth of ponding water [mm]
   real(r8), intent(inout) :: ss_vliq(1:nlev) ! volume content of liquid water [mm^3/mm^3]
   real(r8), intent(inout) :: zwt             ! location of water table [mm]
   real(r8), intent(inout) :: wa              ! water in aquifer [mm, negative]

   integer,  intent(out)   :: izwt

   ! Local variables
   real(r8) :: sp_dz(1:nlev)
   real(r8) :: reswater, zwtp, psi, vliq, air
   real(r8) :: tol_v, tol_z

      sp_dz(1:nlev) = sp_zi(1:nlev) - sp_zi(0:nlev-1)

      ! tolerances
      tol_z = tol_richards / sqrt(real(nlev,r8)) * 0.5_r8 * 1800._r8
      tol_v = tol_z / maxval(sp_dz)

      ! water table location
      izwt = findloc_ud(zwt >= sp_zi, back=.true.)

      reswater = exwater

      IF (reswater > 0.) THEN

         IF ((zwt <= 0.) .and. (ss_dp > 0.)) THEN
            IF (ss_dp > reswater) THEN
               ss_dp = ss_dp - reswater
               reswater = 0.
            ELSE
               reswater = reswater - ss_dp
               ss_dp = 0.
            ENDIF
         ENDIF

         ! remove water from aquifer
         DO WHILE (reswater > 0.)
            IF (izwt <= nlev) THEN
               IF (is_permeable(izwt)) THEN

                  CALL get_zwt_from_wa ( &
                     porsl(izwt), vl_r(izwt), psi_s(izwt), hksat(izwt), &
                     nprm, prms(:,izwt), tol_v, tol_z, -reswater, zwt, zwtp)

                  IF (zwtp < sp_zi(izwt)) THEN
                     ss_vliq(izwt) = (ss_vliq(izwt)*(zwt-sp_zi(izwt-1))  &
                        + porsl(izwt)*(zwtp-zwt) - reswater) / (zwtp - sp_zi(izwt-1))
                     reswater = 0.
                     zwt = zwtp
                  ELSE
                     psi  = psi_s(izwt) - (zwtp - 0.5*(sp_zi(izwt) + zwt))
                     vliq = soil_vliq_from_psi (psi, &
                        porsl(izwt), vl_r(izwt), psi_s(izwt), nprm, prms(:,izwt))
                     IF (reswater > (porsl(izwt)-vliq) * (sp_zi(izwt)-zwt)) THEN
                        ss_vliq(izwt) = (ss_vliq(izwt)*(zwt-sp_zi(izwt-1))  &
                           + vliq * (sp_zi(izwt)-zwt)) / sp_dz(izwt)
                        reswater = reswater - (porsl(izwt)-vliq) * (sp_zi(izwt)-zwt)
                     ELSE
                        ss_vliq(izwt) = (ss_vliq(izwt)*(zwt-sp_zi(izwt-1))  &
                           + porsl(izwt)*(sp_zi(izwt)-zwt) - reswater) / sp_dz(izwt)
                        reswater = 0.
                     ENDIF

                     zwt  = sp_zi(izwt)
                     izwt = izwt + 1
                  ENDIF

               ELSE
                  zwt  = sp_zi(izwt)
                  izwt = izwt + 1
               ENDIF
            ELSE
               CALL get_zwt_from_wa ( &
                  porsl_wa, vl_r(nlev), psi_s(nlev), hksat(nlev), &
                  nprm, prms(:,nlev), tol_v, tol_z, wa-reswater, sp_zi(nlev), zwt)
               wa = wa - reswater
               reswater = 0.
            ENDIF
         ENDDO

      ELSEIF (reswater < 0.) THEN

         ! increase water in aquifer
         DO WHILE (reswater < 0.)
            IF (izwt > nlev) THEN
               IF (wa <= reswater) THEN
                  wa = wa - reswater
                  reswater = 0.
               ELSE
                  reswater = reswater - wa
                  wa = 0.
                  izwt = nlev
                  zwt  = sp_zi(nlev)
               ENDIF
            ELSEIF (izwt >= 1) THEN
               IF (is_permeable(izwt)) THEN
                  air = (porsl(izwt)-ss_vliq(izwt)) * (zwt-sp_zi(izwt-1))
                  IF (air > -reswater) THEN
                     ss_vliq(izwt) = ss_vliq(izwt) - reswater / (zwt-sp_zi(izwt-1))
                     reswater = 0.
                  ELSE
                     ss_vliq(izwt) = porsl(izwt)
                     reswater = reswater + air
                     izwt = izwt - 1
                     zwt  = sp_zi(izwt)
                  ENDIF
               ELSE
                  izwt = izwt - 1
                  zwt  = sp_zi(izwt)
               ENDIF
            ELSE
               ss_dp = ss_dp - reswater
               reswater = 0.
               izwt = 1
            ENDIF
         ENDDO

      ENDIF

   END SUBROUTINE soilwater_aquifer_exchange

   ! ---- Richards equation solver ----
   SUBROUTINE Richards_solver ( &
         lb, ub, dt, sp_zc, sp_zi, &
         vl_s, vl_r, psi_s, hksat, nprm, prms, &
         vl_s_wa, &
         ubc_typ, ubc_val, lbc_typ, lbc_val, &
         ss_dp, waquifer, ss_vl, ss_wt, ss_q, &
         tol_q, tol_z, tol_v, tol_p)

   IMPLICIT NONE

   integer,  intent(in) :: lb, ub      ! lower and upper boundary

   real(r8), intent(in) :: dt          ! time step (second)

   real(r8), intent(in) :: sp_zc   (lb:ub)   ! soil parameter : centers of level (mm)
   real(r8), intent(in) :: sp_zi (lb-1:ub)   ! soil parameter : interfaces of level (mm)

   real(r8), intent(in) :: vl_s  (lb:ub)  ! soil porosity (mm^3/mm^3)
   real(r8), intent(in) :: vl_r  (lb:ub)  ! residual soil moisture (mm^3/mm^3)
   real(r8), intent(in) :: psi_s (lb:ub)  ! saturated capillary potential (mm,negative)
   real(r8), intent(in) :: hksat (lb:ub)  ! saturated hydraulic conductivity (mm/s)

   integer,  intent(in) :: nprm        ! number of parameters included in soil function
   real(r8), intent(in) :: prms(nprm,lb:ub)  ! parameters included in soil function

   real(r8), intent(in) :: vl_s_wa        ! soil porosity in aquifer (mm^3/mm^3)

   integer,  intent(in) :: ubc_typ  ! upper boundary condition type
   real(r8), intent(in) :: ubc_val  ! value of upper boundary condition
   integer,  intent(in) :: lbc_typ  ! lower boundary condition type
   real(r8), intent(in) :: lbc_val  ! value of lower boundary condition

   real(r8), intent(inout) :: ss_dp    ! soil water state : depth of ponding water (mm)
   real(r8), intent(inout) :: waquifer ! water deficit in aquifer (mm, negative)
   real(r8), intent(inout) :: ss_vl   (lb:ub) ! soil water state : volume content of liquid water
   real(r8), intent(inout) :: ss_wt   (lb:ub) ! soil water state : location of water table (mm)
   real(r8), intent(out)   :: ss_q  (lb-1:ub) ! soil water state : flux between levels (mm/s)

   real(r8), intent(in) :: tol_q    ! tolerance for flux
   real(r8), intent(in) :: tol_z    ! tolerance for locations
   real(r8), intent(in) :: tol_v    ! tolerance for volumetric water content
   real(r8), intent(in) :: tol_p    ! tolerance for potential head

   ! Local variables
   real(r8) :: zwt             ! location of water table (mm)
   real(r8) :: sp_dz (lb:ub)   ! thickness of level (mm)
   real(r8) :: ss_wf (lb:ub)   ! soil water state : location of wetting front

   logical :: is_sat (lb:ub)   ! whether a level is saturated or not at this time step
   logical :: has_wf (lb:ub)   ! whether a wetting front is present or not
   logical :: has_wt (lb:ub)   ! whether a water table is present or not

   real(r8) :: psi (lb:ub)     ! water pressure head in unsaturated soil (mm)
   real(r8) :: hk  (lb:ub)     ! hydraulic conductivity in unsaturated soil (mm/s)

   real(r8) :: psi_pb (lb:ub)  ! perturbed water pressure head (mm)
   real(r8) :: hk_pb  (lb:ub)  ! perturbed hydraulic conductivity (mm/s)

   real(r8) :: q_this(lb-1:ub) ! water flux between levels (mm/s)
   real(r8) :: q_wf    (lb:ub) ! water flux at wetting front (mm/s)
   real(r8) :: q_wt    (lb:ub) ! water flux at water table (mm/s)

   real(r8) :: dp_m1           ! depth of ponding water at previous time step (mm)
   real(r8) :: wf_m1 (lb:ub)   ! location of wetting front at previous time step (mm)
   real(r8) :: vl_m1 (lb:ub)   ! volumetric water content at previous time step (mm/mm)
   real(r8) :: wt_m1 (lb:ub)   ! location of water table at previous time step (mm)
   real(r8) :: waquifer_m1     ! water deficit in aquifer at previous time step

   real(r8) :: q_0  (lb-1:ub)  ! initial value of water flux between levels (mm/s)
   real(r8) :: q_wf_0 (lb:ub)  ! initial value of water flux at wetting front (mm/s)
   real(r8) :: q_wt_0 (lb:ub)  ! initial value of water flux at water table (mm/s)

   real(r8) :: dp_pb           ! perturbed depth of ponding water (mm)
   real(r8) :: vl_pb (lb:ub)   ! perturbed volumetric water content (mm/mm)
   real(r8) :: wf_pb (lb:ub)   ! perturbed location of wetting front (mm)
   real(r8) :: wt_pb (lb:ub)   ! perturbed location of water table (mm)
   real(r8) :: zwt_pb, waquifer_pb

   real(r8) :: q_pb  (lb-1:ub) ! perturbed water flux between levels (mm/s)
   real(r8) :: q_wf_pb (lb:ub) ! perturbed water flux at wetting front (mm/s)
   real(r8) :: q_wt_pb (lb:ub) ! perturbed water flux at water table (mm/s)

   real(r8) :: blc        (lb-1:ub+1)  ! mass balance (mm water)
   logical  :: is_solvable
   logical  :: lev_update (lb-1:ub+1)  ! whether a level is updated or not
   real(r8) :: blc_pb     (lb-1:ub+1)  ! perturbed mass balance (mm water)
   logical  :: vact       (lb-1:ub+1)  ! whether a level is active or not
   integer  :: jsbl (lb:ub) ! which variable of wf,vl,wt inside each level is active

   real(r8) :: dr_dv (lb-1:ub+1,lb-1:ub+1)  ! the Jacobian matrix
   real(r8) :: dv    (lb-1:ub+1)  ! searching step of variables

   real(r8) :: f2_norm (max_iters_richards)   ! sqrt( f2 ), where f2 = sum_i (r_i ^2)

   real(r8) :: dt_this, dt_done
   real(r8) :: dt_explicit      ! time step (day) for explicit scheme

   integer  :: ilev, iter
   real(r8) :: dlt

   logical  :: wet2dry

   real(r8) :: wsum_m1, wsum, werr

      ss_wf(lb:ub) = 0

      DO ilev = lb, ub
         sp_dz(ilev) = sp_zi(ilev) - sp_zi(ilev-1)
      ENDDO

      dt_explicit = dt / max_iters_richards

      ss_q = 0
      dt_done = 0
      DO WHILE (dt_done < dt)

         dt_this = dt - dt_done

         wf_m1 = ss_wf
         vl_m1 = ss_vl
         wt_m1 = ss_wt

         wsum_m1 = sum(ss_vl * (sp_dz - ss_wt)) + sum(ss_wt * vl_s)
         IF (ubc_typ == BC_RAINFALL) THEN
            wsum_m1 = wsum_m1 + ss_dp
         ENDIF
         IF (lbc_typ == BC_DRAINAGE) THEN
            wsum_m1 = wsum_m1 + waquifer
         ENDIF

         IF (ubc_typ == BC_RAINFALL) THEN
            dp_m1 = max(ss_dp, 0._r8)
         ENDIF

         IF (lbc_typ == BC_DRAINAGE) THEN
            waquifer_m1 = waquifer
            CALL get_zwt_from_wa ( &
               vl_s_wa, vl_r(ub), psi_s(ub), hksat(ub), &
               nprm, prms(:,ub), tol_v, tol_z, &
               waquifer, sp_zi(ub), zwt)
         ENDIF

         iter = 0
         DO WHILE (.true.)

            iter = iter + 1

            CALL initialize_sublevel_structure ( &
               lb, ub, sp_dz, sp_zi(ub), &
               vl_s, vl_r, psi_s, hksat, nprm, prms, &
               ubc_typ, ubc_val, lbc_typ, lbc_val, &
               is_sat, has_wf, has_wt, &
               ss_wf, ss_vl, ss_wt, ss_dp, psi, hk, &
               tol_v, tol_z)

            lev_update (:) = .true.
            CALL flux_all ( &
               lb, ub, sp_dz, sp_zc, sp_zi, &
               vl_s, psi_s, hksat, nprm, prms, &
               ubc_typ, ubc_val, lbc_typ, lbc_val, &
               lev_update, .true., &
               is_sat, has_wf, has_wt, &
               ss_wf, ss_vl, ss_wt, ss_dp, zwt, psi, hk, &
               q_this, q_wf, q_wt, &
               tol_q, tol_z, tol_p)

            CALL water_balance ( &
               lb, ub, sp_dz, dt_this, is_sat, vl_s, q_this, &
               ubc_typ, ubc_val, lbc_typ, lbc_val, &
               ss_wf, ss_vl, ss_wt, ss_dp, waquifer, &
               wf_m1, vl_m1, wt_m1, dp_m1, waquifer_m1, &
               blc, is_solvable, tol_richards * dt_this)

            IF (iter == 1) THEN
               q_0 = q_this
               q_wf_0 = q_wf
               q_wt_0 = q_wt

               wet2dry = .false.
               IF (ubc_typ == BC_RAINFALL) THEN
                  IF ((dp_m1 > tol_z) .and. (dp_m1 - (q_0(lb-1)-ubc_val)*dt_this < tol_z)) THEN
                     wet2dry = .true.
                  ENDIF
               ENDIF
            ENDIF

            f2_norm(iter) = sqrt(sum(blc**2))

            IF (    (f2_norm(iter) < tol_richards * dt_this)  &  ! converged
               .or. (dt_this < dt_explicit)                   &
               .or. (iter >= max_iters_richards) &
               .or. (.not. is_solvable)          &
               .or. wet2dry) THEN

               IF ((dt_this < dt_explicit) &
                  .or. (iter >= max_iters_richards) &
                  .or. (.not. is_solvable) &
                  .or. wet2dry) THEN

                  dt_this = min(dt_this, dt_explicit)
                  q_this  = q_0

                  CALL use_explicit_form ( &
                     lb, ub, dt_this, sp_dz, sp_zc, sp_zi, &
                     vl_s, vl_r, psi_s, hksat, nprm, prms, &
                     vl_s_wa, &
                     ubc_typ, ubc_val, lbc_typ, lbc_val, &
                     q_this, q_wf_0, q_wt_0, &
                     ss_wf, ss_vl, ss_wt, ss_dp, waquifer, zwt, &
                     wf_m1, vl_m1, wt_m1, dp_m1, waquifer_m1, &
                     tol_q, tol_z, tol_v)

               ENDIF

               dt_done = dt_done + dt_this

#ifdef CoLMDEBUG
               IF (f2_norm(iter) < tol_richards * dt_this) THEN
                  count_implicit = count_implicit + 1
               ELSEIF (iter >= max_iters_richards) then
                  count_explicit = count_explicit + 1
               ELSEIF (wet2dry) THEN
                  count_wet2dry = count_wet2dry + 1
               ENDIF
#endif

               EXIT

            ENDIF

            dr_dv = 0
            vact  = .false.

            IF (ubc_typ == BC_RAINFALL) THEN

               CALL var_perturb_rainfall ( &
                  blc(lb-1), ss_dp, dp_pb, dlt, vact(lb-1))

               IF (vact(lb-1)) THEN
                  q_pb    = q_this
                  q_wf_pb = q_wf
                  q_wt_pb = q_wt

                  lev_update(:)    = .false.
                  lev_update(lb-1) = .true.
                  CALL flux_all ( &
                     lb, ub, sp_dz, sp_zc, sp_zi, &
                     vl_s, psi_s, hksat, nprm, prms, &
                     ubc_typ, ubc_val, lbc_typ, lbc_val, &
                     lev_update, .false., &
                     is_sat, has_wf, has_wt, &
                     ss_wf, ss_vl, ss_wt, dp_pb, zwt, psi, hk, &
                     q_pb, q_wf_pb, q_wt_pb, &
                     tol_q, tol_z, tol_p)

                  CALL water_balance ( &
                     lb, ub, sp_dz, dt_this, is_sat, vl_s, q_pb, &
                     ubc_typ, ubc_val, lbc_typ, lbc_val, &
                     ss_wf, ss_vl, ss_wt, dp_pb, waquifer, &
                     wf_m1, vl_m1, wt_m1, dp_m1, waquifer_m1, &
                     blc_pb)

                  dr_dv(:,lb-1) = (blc_pb - blc) / dlt

               ENDIF

            ENDIF

            DO ilev = lb, ub
               IF (.not. is_sat(ilev)) THEN

                  wf_pb  = ss_wf
                  vl_pb  = ss_vl
                  wt_pb  = ss_wt
                  psi_pb = psi
                  hk_pb  = hk

                  CALL var_perturb_level ( jsbl(ilev), blc(ilev), &
                     sp_dz(ilev), sp_zc(ilev), sp_zi(ilev), &
                     vl_s(ilev), vl_r(ilev), psi_s(ilev), hksat(ilev), &
                     nprm, prms(:,ilev), &
                     is_sat(ilev), has_wf(ilev), has_wt(ilev), &
                     q_this(ilev-1), q_this(ilev), q_wf(ilev), q_wt(ilev), &
                     wf_pb(ilev), vl_pb(ilev), wt_pb(ilev), dlt, &
                     psi_pb(ilev), hk_pb(ilev), vact(ilev), &
                     tol_v)

                  IF (vact(ilev)) THEN

                     q_pb    = q_this
                     q_wf_pb = q_wf
                     q_wt_pb = q_wt

                     lev_update(:)    = .false.
                     lev_update(ilev) = .true.
                     CALL flux_all ( &
                        lb, ub, sp_dz, sp_zc, sp_zi, &
                        vl_s, psi_s, hksat, nprm, prms, &
                        ubc_typ, ubc_val, lbc_typ, lbc_val, &
                        lev_update, .false., &
                        is_sat, has_wf, has_wt, &
                        wf_pb, vl_pb, wt_pb, ss_dp, zwt, psi_pb, hk_pb, &
                        q_pb, q_wf_pb, q_wt_pb, &
                        tol_q, tol_z, tol_p)

                     CALL water_balance ( &
                        lb, ub, sp_dz, dt_this, is_sat, vl_s, q_pb, &
                        ubc_typ, ubc_val, lbc_typ, lbc_val, &
                        wf_pb, vl_pb, wt_pb, ss_dp, waquifer, &
                        wf_m1, vl_m1, wt_m1, dp_m1, waquifer_m1, &
                        blc_pb)

                     dr_dv(:,ilev) = (blc_pb - blc) / dlt

                  ENDIF
               ENDIF
            ENDDO

            IF (lbc_typ == BC_DRAINAGE) THEN

               CALL var_perturb_drainage (sp_zi(ub), blc(ub+1), zwt, zwt_pb, dlt, vact(ub+1))

               IF (vact(ub+1)) THEN
                  q_pb    = q_this
                  q_wf_pb = q_wf
                  q_wt_pb = q_wt

                  waquifer_pb  =  - (zwt_pb - sp_zi(ub)) * (vl_s_wa &
                     - soil_vliq_from_psi (psi_s(ub)+(sp_zi(ub)-zwt_pb)*0.5, &
                     vl_s_wa, vl_r(ub), psi_s(ub), nprm, prms(:,ub)))

                  lev_update(:)    = .false.
                  lev_update(ub+1) = .true.
                  CALL flux_all ( &
                     lb, ub, sp_dz, sp_zc, sp_zi, &
                     vl_s, psi_s, hksat, nprm, prms, &
                     ubc_typ, ubc_val, lbc_typ, lbc_val, &
                     lev_update, .false., &
                     is_sat, has_wf, has_wt, &
                     ss_wf, ss_vl, ss_wt, ss_dp, zwt_pb, psi, hk, &
                     q_pb, q_wf_pb, q_wt_pb, &
                     tol_q, tol_z, tol_p)

                  CALL water_balance ( &
                     lb, ub, sp_dz, dt_this, is_sat, vl_s, q_pb, &
                     ubc_typ, ubc_val, lbc_typ, lbc_val, &
                     ss_wf, ss_vl, ss_wt, ss_dp, waquifer_pb, &
                     wf_m1, vl_m1, wt_m1, dp_m1, waquifer_m1, &
                     blc_pb)

                  dr_dv(:,ub+1) = (blc_pb - blc) / dlt

               ENDIF

            ENDIF

            DO ilev = lb-1, ub+1
               vact(ilev) = vact(ilev) .and. (abs(dr_dv(ilev,ilev)) > tol_q)
            ENDDO

            CALL solve_least_squares_problem (ub-lb+3, dr_dv, vact, blc, dv)

            IF (vact(lb-1)) THEN
               ss_dp = ss_dp - dv(lb-1)
               ss_dp = max(ss_dp, 0._r8)
            ENDIF

            DO ilev = lb, ub
               IF (vact(ilev)) THEN
                  IF (jsbl(ilev) == 1) THEN
                     IF ((ss_wf(ilev) == sp_dz(ilev)) .and. (dv(ilev) > 0)) THEN
                        ss_wf(ilev) = ss_wf(ilev) - min(dv(ilev), sp_dz(ilev))

                        psi(ilev) = psi_s(ilev) + (1 - q_this(ilev)/hksat(ilev)) &
                           * min(dv(ilev),sp_dz(ilev)) * (sp_zc(ilev)-sp_zi(ilev-1))/sp_dz(ilev)
                        ss_vl(ilev) = soil_vliq_from_psi (psi(ilev), &
                           vl_s(ilev), vl_r(ilev), psi_s(ilev), nprm, prms(:,ilev))
                        hk(ilev) = soil_hk_from_psi (psi(ilev), &
                           psi_s(ilev), hksat(ilev), nprm, prms(:,ilev))
                     ELSE
                        ss_wf(ilev) = ss_wf(ilev) - dv(ilev)
                        ss_wf(ilev) = max(ss_wf(ilev), 0._r8)
                        ss_wf(ilev) = min(ss_wf(ilev), sp_dz(ilev)-ss_wt(ilev))
                     ENDIF
                  ENDIF

                  IF (jsbl(ilev) == 2) THEN
                     ss_vl(ilev) = ss_vl(ilev) - dv(ilev)
                     ss_vl(ilev) = max(ss_vl(ilev), tol_v)
                     ss_vl(ilev) = min(ss_vl(ilev), vl_s(ilev))
                  ENDIF

                  IF (jsbl(ilev) == 3) THEN
                     IF ((ss_wt(ilev) == sp_dz(ilev)) .and. (dv(ilev) > 0)) THEN
                        ss_wt(ilev) = ss_wt(ilev) - min(dv(ilev), sp_dz(ilev))

                        psi(ilev) = psi_s(ilev) - (1 - q_this(ilev-1)/hksat(ilev)) &
                           * min(dv(ilev),sp_dz(ilev)) * (sp_zi(ilev)-sp_zc(ilev))/sp_dz(ilev)
                        ss_vl(ilev) = soil_vliq_from_psi (psi(ilev), &
                           vl_s(ilev), vl_r(ilev), psi_s(ilev), nprm, prms(:,ilev))
                        hk(ilev) = soil_hk_from_psi (psi(ilev), &
                           psi_s(ilev), hksat(ilev), nprm, prms(:,ilev))
                     ELSE
                        ss_wt(ilev) = ss_wt(ilev) - dv(ilev)
                        ss_wt(ilev) = max(ss_wt(ilev), 0._r8)
                        ss_wt(ilev) = min(ss_wt(ilev), sp_dz(ilev)-ss_wf(ilev))
                     ENDIF
                  ENDIF
               ENDIF

               CALL check_and_update_level (sp_dz(ilev), &
                  vl_s(ilev), vl_r(ilev), psi_s(ilev), hksat(ilev), &
                  nprm, prms(:,ilev), &
                  is_sat(ilev), has_wf(ilev), has_wt(ilev), &
                  ss_wf(ilev), ss_vl(ilev), ss_wt(ilev), psi(ilev), hk(ilev), &
                  jsbl(ilev) == 2, tol_v)
            ENDDO

            IF (vact(ub+1)) THEN
               zwt = zwt - dv(ub+1)
               zwt = max(zwt, sp_zi(ub))
               waquifer  = - (zwt - sp_zi(ub)) * (vl_s_wa &
                  - soil_vliq_from_psi (psi_s(ub)+(sp_zi(ub)-zwt)*0.5, &
                  vl_s_wa, vl_r(ub), psi_s(ub), nprm, prms(:,ub)))
            ENDIF

         ENDDO

         ss_q = ss_q + q_this * dt_this

         wsum = sum(ss_vl * (sp_dz - ss_wt - ss_wf)) + sum((ss_wt + ss_wf) * vl_s)
         IF (ubc_typ == BC_RAINFALL) THEN
            wsum = wsum + ss_dp
         ENDIF
         IF (lbc_typ == BC_DRAINAGE) THEN
            wsum = wsum + waquifer
         ENDIF

         werr = wsum - (wsum_m1 + ubc_val * dt_this - lbc_val * dt_this)

      ENDDO

      ss_q = ss_q / dt

      DO ilev = lb, ub
         IF (abs(sp_dz(ilev) - ss_wt(ilev)) > tol_z) THEN
            ss_vl(ilev) = (ss_wf(ilev) * vl_s(ilev)  &
               + (sp_dz(ilev) - ss_wf(ilev) - ss_wt(ilev)) * ss_vl(ilev)) &
               / (sp_dz(ilev) - ss_wt(ilev))
         ENDIF
      ENDDO

   END SUBROUTINE Richards_solver


   ! ---- water balance ----
   SUBROUTINE water_balance ( &
         lb, ub, dz, dt, is_sat, vl_s, q, &
         ubc_typ, ubc_val, lbc_typ, lbc_val, &
         wf, vl, wt, dp, waquifer, &
         wf_m1, vl_m1, wt_m1, dp_m1, waquifer_m1, &
         blc, is_solvable, tol)

   integer, intent(in) :: lb, ub

   real(r8), intent(in) :: dz(lb:ub)
   real(r8), intent(in) :: dt

   logical,  intent(in) :: is_sat(lb:ub)
   real(r8), intent(in) :: vl_s  (lb:ub)

   real(r8), intent(in) :: q(lb-1:ub)

   integer,  intent(in) :: ubc_typ
   real(r8), intent(in) :: ubc_val
   integer,  intent(in) :: lbc_typ
   real(r8), intent(in) :: lbc_val

   real(r8), intent(in) :: wf(lb:ub)
   real(r8), intent(in) :: vl(lb:ub)
   real(r8), intent(in) :: wt(lb:ub)
   real(r8), intent(in) :: dp
   real(r8), intent(in) :: waquifer

   real(r8), intent(in) :: wf_m1(lb:ub)
   real(r8), intent(in) :: vl_m1(lb:ub)
   real(r8), intent(in) :: wt_m1(lb:ub)
   real(r8), intent(in) :: dp_m1
   real(r8), intent(in) :: waquifer_m1

   real(r8), intent(out) :: blc(lb-1:ub+1)
   logical,  intent(out), optional :: is_solvable
   real(r8), intent(in ), optional :: tol

   ! Local variables
   integer  :: ilev, jlev
   real(r8) :: dmss, qsum

      blc(:)  = 0

      IF (ubc_typ == BC_RAINFALL) THEN
         dmss = max(dp, 0._r8) - max(dp_m1, 0._r8)
         qsum = ubc_val - q(lb-1)
         blc(lb-1) = dmss - qsum * dt
      ENDIF

      ilev = lb - 1
      DO jlev = lb, ub

         dmss = (vl_s(jlev) - vl_m1(jlev)) * (wf(jlev) - wf_m1(jlev))
         dmss = (vl_s(jlev) - vl_m1(jlev)) * (wt(jlev) - wt_m1(jlev)) + dmss
         dmss = (dz(jlev) - wt(jlev) - wf(jlev)) * (vl(jlev) - vl_m1(jlev)) + dmss

         qsum = q(jlev-1) - q(jlev)

         IF (.not. is_sat(jlev)) THEN

            ilev = jlev

            IF ((ubc_typ /= BC_RAINFALL) .and. (blc(lb-1) /= 0)) THEN
               blc(ilev) = blc(ilev) + blc(lb-1)
               blc(lb-1) = 0
            ENDIF
         ENDIF

         blc(ilev) = blc(ilev) + dmss - qsum * dt

      ENDDO

      IF (lbc_typ == BC_DRAINAGE) THEN
         IF ((waquifer == 0) .and. (q(ub) >= 0)) THEN
            blc(ilev) = blc(ilev) - waquifer_m1 - q(ub) * dt
         ELSE
            blc(ub+1) = waquifer - waquifer_m1 - q(ub) * dt

            IF ((ubc_typ /= BC_RAINFALL) .and. (blc(lb-1) /= 0)) THEN
               blc(ub+1) = blc(ub+1) + blc(lb-1)
               blc(lb-1) = 0
            ENDIF
         ENDIF
      ENDIF

      IF (present(is_solvable)) THEN
         IF (present(tol)) THEN
            is_solvable = (ubc_typ == BC_RAINFALL) .or. (blc(lb-1) < tol)
         ELSE
            is_solvable = (ubc_typ == BC_RAINFALL) .or. (blc(lb-1) == 0)
         ENDIF
      ENDIF

   END SUBROUTINE water_balance

   ! ---- initialize sublevel structure ----
   SUBROUTINE initialize_sublevel_structure ( &
         lb, ub, dz, zbtm, &
         vl_s, vl_r, psi_s, hksat, nprm, prms, &
         ubc_typ, ubc_val, lbc_typ, lbc_val, &
         is_sat, has_wf, has_wt, &
         wf, vl, wt, dp, psi, hk, &
         tol_v, tol_z)

   integer,  intent(in) :: lb, ub
   real(r8), intent(in) :: dz (lb:ub)
   real(r8), intent(in) :: zbtm

   real(r8), intent(in) :: vl_s  (lb:ub)
   real(r8), intent(in) :: vl_r  (lb:ub)
   real(r8), intent(in) :: psi_s (lb:ub)
   real(r8), intent(in) :: hksat (lb:ub)

   integer,  intent(in) :: nprm
   real(r8), intent(in) :: prms (nprm,lb:ub)

   integer,  intent(in) :: ubc_typ
   real(r8), intent(in) :: ubc_val
   integer,  intent(in) :: lbc_typ
   real(r8), intent(in) :: lbc_val

   logical, intent(inout) :: is_sat(lb:ub)
   logical, intent(inout) :: has_wf(lb:ub)
   logical, intent(inout) :: has_wt(lb:ub)

   real(r8), intent(inout) :: wf(lb:ub)
   real(r8), intent(inout) :: vl(lb:ub)
   real(r8), intent(inout) :: wt(lb:ub)
   real(r8), intent(inout) :: dp

   real(r8), intent(inout) :: psi(lb:ub)
   real(r8), intent(inout) :: hk (lb:ub)

   real(r8), intent(in) :: tol_v
   real(r8), intent(in) :: tol_z

   ! Local variables
   integer  :: ilev

      DO ilev = lb, ub
         is_sat(ilev) = (abs(vl(ilev) - vl_s(ilev)) < tol_v) &
            .or. (abs(wf(ilev) + wt(ilev) - dz(ilev)) < tol_z)
      ENDDO

      IF (ubc_typ == BC_FIX_HEAD) THEN
         IF (ubc_val < psi_s(lb)) THEN
            IF (is_sat(lb)) THEN
               is_sat(lb) = .false.

               wf(lb) = 0
               vl(lb) = vl_s(lb)
               wt(lb) = 0.9 * dz(lb)
            ELSEIF (wf(lb) >= tol_z) THEN
               vl(lb) = (wf(lb)*vl_s(lb) + vl(lb)*(dz(lb)-wf(lb)-wt(lb))) / (dz(lb)-wt(lb))
               wf(lb) = 0
            ENDIF
         ENDIF
      ENDIF

      IF (lbc_typ == BC_FIX_HEAD) THEN
         IF (lbc_val < psi_s(ub)) THEN
            IF (is_sat(ub)) THEN
               is_sat(ub) = .false.

               wf(ub) = 0.9 * dz(ub)
               vl(ub) = vl_s(ub)
               wt(ub) = 0
            ELSEIF (wt(ub) >= tol_z) THEN
               vl(ub) = (wt(ub)*vl_s(ub) + vl(ub)*(dz(ub)-wf(ub)-wt(ub))) / (dz(ub)-wf(ub))
               wt(ub) = 0
            ENDIF
         ENDIF
      ENDIF

      DO ilev = lb, ub
         IF (is_sat(ilev)) THEN
            wf(ilev) = 0
            wt(ilev) = dz(ilev)
            vl(ilev) = vl_s(ilev)
         ELSE
            IF (ilev > lb) THEN
               IF (is_sat(ilev-1)) THEN
                  has_wf(ilev) = .true.
               ELSE
                  has_wf(ilev) = (wf(ilev) >= tol_z) .or. (wt(ilev-1) >= tol_z)
               ENDIF

               IF (has_wf(ilev)) THEN
                  IF ((wf(ilev) < tol_z) .and. (psi_s(ilev) < psi_s(ilev-1))) THEN
                     wf(ilev) = 0.1 * (dz(ilev)-wt(ilev))
                  ENDIF
               ENDIF
            ELSE
               SELECTCASE (ubc_typ)
               CASE (BC_RAINFALL)
                  has_wf(lb) = (dp >= tol_z) .or. (wf(lb) >= tol_z)
               CASE (BC_FIX_HEAD)
                  has_wf(lb) = (ubc_val > psi_s(lb)) .or. (wf(lb) >= tol_z)

                  IF (has_wf(lb) .and. (wf(lb) < tol_z)) THEN
                     wf(lb) = 0.01 * (dz(lb)-wt(lb))
                  ENDIF
               CASE (BC_FIX_FLUX)
                  has_wf(lb) = wf(lb) >= tol_z
               ENDSELECT
            ENDIF

            IF (ilev < ub) THEN
               IF (is_sat(ilev+1)) THEN
                  has_wt(ilev) = .true.
               ELSE
                  has_wt(ilev) = (wt(ilev) >= tol_z) .or. (wf(ilev+1) >= tol_z)
               ENDIF

               IF (has_wt(ilev)) THEN
                  IF ((wt(ilev) < tol_z) .and. (psi_s(ilev) < psi_s(ilev+1))) THEN
                     wt(ilev) = 0.1 * (dz(ilev)-wf(ilev))
                  ENDIF
               ENDIF
            ELSE
               SELECTCASE (lbc_typ)
               CASE (BC_DRAINAGE)
                  has_wt(ub) = (wt(ub) >= tol_z)
               CASE (BC_FIX_HEAD)
                  has_wt(ub) = (lbc_val > psi_s(ub)) .or. (wt(ub) >= tol_z)

                  IF ((has_wt(ub)) .and. (wt(ub) < tol_z)) THEN
                     wt(ub) = 0.01 * (dz(ub)-wf(ub))
                  ENDIF
               CASE (BC_FIX_FLUX)
                  has_wt(ub) = (wt(ub) >= tol_z)
               ENDSELECT
            ENDIF
         ENDIF

         CALL check_and_update_level ( dz(ilev), &
            vl_s(ilev), vl_r(ilev), psi_s(ilev), hksat(ilev), &
            nprm, prms(:,ilev), &
            is_sat(ilev), has_wf(ilev), has_wt(ilev), &
            wf(ilev), vl(ilev), wt(ilev), psi(ilev), hk(ilev), &
            .true., tol_v)
      ENDDO

   END SUBROUTINE initialize_sublevel_structure

   !-----------------------------------------------------------------------
   SUBROUTINE use_explicit_form ( &
         lb, ub, dt, dz, sp_zc, sp_zi, &
         vl_s, vl_r, psi_s, hksat, nprm, prms, &
         vl_s_wa, &
         ubc_typ, ubc_val, lbc_typ, lbc_val, &
         q, q_wf, q_wt, wf, vl, wt, dp, waquifer, zwt, &
         wf_m1, vl_m1, wt_m1, dp_m1, waquifer_m1, &
         tol_q, tol_z, tol_v)

   integer,  intent(in) :: lb, ub

   real(r8), intent(in) :: dt

   real(r8), intent(in) :: dz     (lb:ub)
   real(r8), intent(in) :: sp_zc  (lb:ub)
   real(r8), intent(in) :: sp_zi(lb-1:ub)

   real(r8), intent(in) :: vl_s  (lb:ub)
   real(r8), intent(in) :: vl_r  (lb:ub)
   real(r8), intent(in) :: psi_s (lb:ub)
   real(r8), intent(in) :: hksat (lb:ub)

   integer,  intent(in) :: nprm
   real(r8), intent(in) :: prms(nprm,lb:ub)

   real(r8), intent(in) :: vl_s_wa

   integer,  intent(in) :: ubc_typ
   real(r8), intent(in) :: ubc_val
   integer,  intent(in) :: lbc_typ
   real(r8), intent(in) :: lbc_val

   real(r8), intent(inout) :: q (lb-1:ub)
   real(r8), intent(in) :: q_wf (lb:ub)
   real(r8), intent(in) :: q_wt (lb:ub)

   real(r8), intent(inout) :: wf (lb:ub)
   real(r8), intent(inout) :: vl (lb:ub)
   real(r8), intent(inout) :: wt (lb:ub)
   real(r8), intent(inout) :: dp
   real(r8), intent(inout) :: waquifer
   real(r8), intent(inout) :: zwt

   real(r8), intent(in) :: wf_m1 (lb:ub)
   real(r8), intent(in) :: vl_m1 (lb:ub)
   real(r8), intent(in) :: wt_m1 (lb:ub)
   real(r8), intent(in) :: dp_m1
   real(r8), intent(in) :: waquifer_m1

   real(r8), intent(in) :: tol_q
   real(r8), intent(in) :: tol_z
   real(r8), intent(in) :: tol_v

   ! Local variables
   integer  :: ilev
   real(r8) :: air_m1, wa_m1, dwat, dwat_s
   real(r8) :: alp, zwf_this, zwt_this, vl_wa

   real(r8) :: dmss, mblc

      ! depleted : decrease outflux from top down
      IF (ubc_typ == BC_RAINFALL) THEN
         IF (dp_m1 <  - (ubc_val - q(lb-1))*dt) THEN
            q(lb-1) = dp_m1/dt + ubc_val
         ENDIF
      ENDIF

      DO ilev = lb, ub

         dwat = (q(ilev-1) - q(ilev)) * dt
         wa_m1 = (wt_m1(ilev)+wf_m1(ilev)) * vl_s(ilev) &
            + (dz(ilev)-wt_m1(ilev)-wf_m1(ilev)) * vl_m1(ilev)
         IF (dwat <= - wa_m1) THEN
            q(ilev) = q(ilev-1) + wa_m1/dt
         ENDIF

      ENDDO

      IF ((lbc_typ == BC_FIX_FLUX) .and. (q(ub) < lbc_val)) THEN

         q(ub) = lbc_val
         DO ilev = ub, lb, -1
            dwat = (q(ilev-1) - q(ilev)) * dt
            wa_m1 = (wt_m1(ilev)+wf_m1(ilev)) * vl_s(ilev) &
               + (dz(ilev)-wt_m1(ilev)-wf_m1(ilev)) * vl_m1(ilev)
            IF (dwat <= - wa_m1) THEN
               q(ilev-1) = q(ilev) - wa_m1/dt
            ENDIF
         ENDDO

      ENDIF

      ! overfilled : increase influx from bottom up
      IF (lbc_typ == BC_DRAINAGE) THEN
         IF (q(ub)*dt > -waquifer_m1) THEN
            q(ub) = - waquifer_m1/dt
         ENDIF
      ENDIF

      DO ilev = ub, lb, -1

         dwat = (q(ilev-1) - q(ilev)) * dt
         air_m1 = (vl_s(ilev) - vl_m1(ilev)) * (dz(ilev) - wt_m1(ilev) - wf_m1(ilev))
         IF (dwat >= air_m1) THEN
            q(ilev-1) = q(ilev) + air_m1/dt
         ENDIF

      ENDDO

      IF ((ubc_typ == BC_FIX_FLUX) .and. (q(lb-1) < ubc_val)) THEN

         q(lb-1) = ubc_val
         DO ilev = lb, ub
            dwat = (q(ilev-1) - q(ilev)) * dt
            air_m1 = (vl_s(ilev) - vl_m1(ilev)) * (dz(ilev) - wt_m1(ilev) - wf_m1(ilev))
            IF (dwat >= air_m1) THEN
               q(ilev) = q(ilev-1) - air_m1/dt
            ENDIF
         ENDDO

      ENDIF

      ! update prognostic variables : dp, wf, vl, wt, zwt
      IF (ubc_typ == BC_RAINFALL) THEN
         dp = max(0., dp_m1 + (ubc_val - q(lb-1))*dt)
      ENDIF

      DO ilev = lb, ub

         dwat = (q(ilev-1) - q(ilev)) * dt

         wt(ilev) = 0.
         wf(ilev) = 0.
         vl(ilev) = ((wt_m1(ilev)+wf_m1(ilev)) * vl_s(ilev) &
               + (dz(ilev)-wt_m1(ilev)-wf_m1(ilev)) * vl_m1(ilev) + dwat) / dz(ilev)

      ENDDO

      IF (lbc_typ == BC_DRAINAGE) THEN
         waquifer = waquifer_m1 + q(ub)*dt
         CALL get_zwt_from_wa ( &
            vl_s_wa, vl_r(ub), psi_s(ub), hksat(ub), nprm, prms(:,ub), tol_v, tol_z, &
            waquifer, sp_zi(ub), zwt)
      ENDIF

   END SUBROUTINE use_explicit_form

   !----------------------------------------------------------------------
   SUBROUTINE var_perturb_level ( jsbl, blc, &
         dz, zc, zi, vl_s, vl_r, psi_s, hksat, nprm, prms, &
         is_sat, has_wf, has_wt, qin, qout, q_wf, q_wt, &
         wf_p, vl_p, wt_p, delta, psi_p, hk_p, is_act, &
         tol_v)

   integer,  intent(out) :: jsbl

   real(r8), intent(in) :: blc

   real(r8), intent(in) :: dz, zc, zi
   real(r8), intent(in) :: vl_s, vl_r, psi_s, hksat
   integer,  intent(in) :: nprm
   real(r8), intent(in) :: prms(nprm)

   logical,  intent(in) :: is_sat, has_wf, has_wt
   real(r8), intent(in) :: qin, qout, q_wf, q_wt

   real(r8), intent(inout) :: wf_p, vl_p, wt_p
   real(r8), intent(out)   :: delta
   real(r8), intent(inout) :: psi_p, hk_p
   logical,  intent(out)   :: is_act

   real(r8), intent(in)  :: tol_v

   ! Local variables
   real(r8), parameter :: vstep = 1.0e-6_r8
   real(r8), parameter :: wstep = 1.0e-1_r8

      jsbl = 2

      IF (has_wt) THEN

         IF ((wt_p == dz) .or. &
            ((blc >= 0) .and. (q_wt < qout) .and. (wt_p > 0) .and. (vl_p < vl_s))) THEN
            ! reduce water table

            jsbl = 3

            delta = - min(wstep, wt_p * 0.1_r8)

            IF (wt_p == dz) THEN
               psi_p = psi_s - (1 - qin/hksat) * (-delta)*(zi-zc)/dz
               vl_p  = soil_vliq_from_psi (psi_p, vl_s, vl_r, psi_s, nprm, prms)
               hk_p  = soil_hk_from_psi (psi_p, psi_s, hksat, nprm, prms)
            ENDIF

            wt_p = wt_p + delta

         ELSEIF ((blc < 0) .and. (q_wt > qout) .and. (vl_p < vl_s)) THEN
            ! increase water table

            jsbl = 3

            delta = min(wstep, (dz - wf_p - wt_p) * 0.1_r8)
            wt_p = wt_p + delta

         ENDIF

      ENDIF

      IF ((jsbl == 2) .and. has_wf) THEN

         IF ((wf_p == dz) .or. &
            ((blc >= 0) .and. (qin < q_wf) .and. (wf_p > 0) .and. (vl_p < vl_s))) THEN
            ! reduce wetting front

            jsbl = 1

            delta = - min(wstep, wf_p * 0.1_r8)
            IF (wf_p == dz) THEN
               psi_p = psi_s + (1 - qout/hksat) * (-delta)*(dz-(zi-zc))/dz
               vl_p  = soil_vliq_from_psi (psi_p, vl_s, vl_r, psi_s, nprm, prms)
               hk_p  = soil_hk_from_psi (psi_p, psi_s, hksat, nprm, prms)
            ENDIF

            wf_p = wf_p + delta

         ELSEIF ((blc < 0) .and. (qin > q_wf) .and. (vl_p < vl_s)) THEN
            ! increase wetting front

            jsbl = 1

            delta = min(wstep, (dz - wf_p - wt_p) * 0.1_r8)
            wf_p = wf_p + delta

         ENDIF

      ENDIF

      IF (jsbl == 2) THEN

         IF (((blc > 0) .and. (vl_p > vl_r + tol_v)) .or. (vl_p >= vl_s)) THEN
            ! reduce water content
            delta = - min(vstep, (vl_p - vl_r - tol_v) * 0.5_r8)
         ELSEIF (((blc <= 0) .and. (vl_p < vl_s)) .or. (vl_p <= vl_r+tol_v)) THEN
            ! increase water content
            delta = + min(vstep, (vl_s - vl_p) * 0.5_r8)
         ELSE
            delta = 0
         ENDIF

         vl_p = vl_p + delta

      ENDIF

      is_act = (delta /= 0)

      IF (is_act) THEN
         CALL check_and_update_level (dz, &
            vl_s, vl_r, psi_s, hksat, nprm, prms, &
            is_sat, has_wf, has_wt, wf_p, vl_p, wt_p, psi_p, hk_p, &
            jsbl == 2, tol_v)
      ENDIF

   END SUBROUTINE var_perturb_level

   !----------------------------------------------------------------
   SUBROUTINE var_perturb_rainfall ( &
         blc_srf, dp, dp_p, delta, is_act)

   real(r8), intent(in) :: blc_srf

   real(r8), intent(in)  :: dp
   real(r8), intent(out) :: dp_p
   real(r8), intent(out) :: delta
   logical,  intent(out) :: is_act

   ! Local variables
   real(r8), parameter :: wstep = 1.0e-1_r8

      delta = 0

      IF (blc_srf > 0) THEN
         IF (dp > 0) THEN
            delta = - min(wstep, dp * 0.5_r8)
         ENDIF
      ELSEIF (blc_srf < 0) THEN
         delta = wstep
      ENDIF

      dp_p = dp + delta
      is_act = (delta /= 0)

   END SUBROUTINE var_perturb_rainfall

   !----------------------------------------------------------------
   SUBROUTINE var_perturb_drainage ( &
         zmin, blc_btm, zwt, zwt_p, delta, is_act)

   real(r8), intent(in) :: zmin

   real(r8), intent(in) :: blc_btm

   real(r8), intent(in)  :: zwt
   real(r8), intent(out) :: zwt_p
   real(r8), intent(out) :: delta
   logical,  intent(out) :: is_act

   ! Local variables
   real(r8), parameter :: wstep = 1.0e-1_r8

      delta = 0

      IF (blc_btm > 0) THEN
         delta = wstep
      ELSEIF (blc_btm < 0) THEN
         delta = - min(max((zwt-zmin)*0.5_r8,0.0), wstep)
      ENDIF

      zwt_p = zwt + delta
      is_act = (delta /= 0)

   END SUBROUTINE var_perturb_drainage


   !----------------------------------------------------------------
   SUBROUTINE check_and_update_level ( dz, &
         vl_s, vl_r, psi_s, hksat, nprm, prms,  &
         is_sat, has_wf, has_wt, &
         wf, vl, wt, psi, hk, &
         is_update_psi_hk, tol_v)

   real(r8), intent(in) :: dz
   real(r8), intent(in) :: vl_s, vl_r, psi_s, hksat
   integer,  intent(in) :: nprm
   real(r8), intent(in) :: prms(nprm)
   logical,  intent(in) :: is_sat, has_wf, has_wt

   real(r8), intent(inout) :: wf, vl, wt
   real(r8), intent(inout) :: psi, hk

   logical,  intent(in) :: is_update_psi_hk

   real(r8), intent(in) :: tol_v

   ! Local variables
   real(r8) :: alpha

      IF (.not. is_sat) THEN

         IF (has_wf) THEN
            wf = min(max(wf, 0._r8), dz)
         ELSE
            wf = 0
         ENDIF

         IF (has_wt) THEN
            wt = min(max(wt, 0._r8), dz)
         ELSE
            wt = 0
         ENDIF

         IF (has_wf .and. has_wt) THEN
            IF (wf + wt > dz) THEN
               alpha = wf / (wf + wt)
               wf = dz * alpha
               wt = dz * (1.0_r8 - alpha)
            ENDIF
         ENDIF

         vl = min(vl, vl_s)
         vl = max(vl, tol_v)

         IF (is_update_psi_hk) THEN
            psi = soil_psi_from_vliq (vl,  vl_s, vl_r, psi_s, nprm, prms)
            hk  = soil_hk_from_psi   (psi, psi_s, hksat, nprm, prms)
         ENDIF
      ELSE
         vl  = vl_s
         psi = psi_s
         hk  = hksat
      ENDIF

   END SUBROUTINE check_and_update_level


   !----------------------------------------------------------------------
   SUBROUTINE flux_all ( &
         lb, ub, dz, sp_zc, sp_zi, &
         vl_s, psi_s, hksat, nprm, prms, &
         ubc_typ, ubc_val, lbc_typ, lbc_val, &
         lev_update, is_update_sublevel, &
         is_sat, has_wf, has_wt, &
         wf, vl, wt, dp, zwt, psi_us, hk_us, &
         qq, qq_wf, qq_wt, &
         tol_q, tol_z, tol_p)

   integer, intent(in) :: lb, ub

   real(r8), intent(in) :: dz      (lb:ub)
   real(r8), intent(in) :: sp_zc   (lb:ub)
   real(r8), intent(in) :: sp_zi (lb-1:ub)

   real(r8), intent(in) :: vl_s  (lb:ub)
   real(r8), intent(in) :: psi_s (lb:ub)
   real(r8), intent(in) :: hksat (lb:ub)
   integer,  intent(in) :: nprm
   real(r8), intent(in) :: prms (nprm,lb:ub)

   integer,  intent(in) :: ubc_typ
   real(r8), intent(in) :: ubc_val
   integer,  intent(in) :: lbc_typ
   real(r8), intent(in) :: lbc_val

   logical,  intent(in) :: lev_update (lb-1:ub+1)
   logical,  intent(in) :: is_update_sublevel

   logical,  intent(inout) :: is_sat (lb:ub)
   logical,  intent(inout) :: has_wf (lb:ub)
   logical,  intent(inout) :: has_wt (lb:ub)

   real(r8), intent(inout) :: wf (lb:ub)
   real(r8), intent(inout) :: vl (lb:ub)
   real(r8), intent(inout) :: wt (lb:ub)
   real(r8), intent(inout) :: dp
   real(r8), intent(inout) :: zwt
   real(r8), intent(in) :: psi_us (lb:ub)
   real(r8), intent(in) :: hk_us  (lb:ub)

   real(r8), intent(inout) :: qq  (lb-1:ub)
   real(r8), intent(inout) :: qq_wf (lb:ub)
   real(r8), intent(inout) :: qq_wt (lb:ub)

   real(r8), intent(in) :: tol_q
   real(r8), intent(in) :: tol_z
   real(r8), intent(in) :: tol_p

   ! Local variables
   integer  :: ilev_u, ilev_l
   real(r8) :: hk_top, pbtm, hk_btm
   real(r8) :: dz_this, dz_upp, dz_low
   logical  :: has_sat_zone
   real(r8) :: psi_i, hk_i
   real(r8) :: qtest


      ilev_u = lb - 1
      ilev_l = find_unsat_lev_lower (is_sat, lb, ub, ilev_u+1)

      DO WHILE (.true.)

         IF (lev_update(ilev_u) .or. lev_update(ilev_l)) THEN

            IF (ilev_l == lb) THEN
               ! CASE 1: water flux on top

               dz_this = (dz(lb)-wt(lb)-wf(lb)) * (sp_zc(lb)-sp_zi(lb-1))/dz(lb)

               SELECTCASE (ubc_typ)
               CASE (BC_FIX_HEAD)

                  IF (has_wf(lb)) THEN
                     qq(lb-1) = - hksat(lb) * ((psi_s(lb) - ubc_val) / wf(lb) - 1)
                  ELSE
                     hk_top = soil_hk_from_psi (ubc_val, &
                        psi_s(lb), hksat(lb), nprm, prms(:,lb))
                     qq(lb-1) = flux_inside_hm_soil ( &
                        psi_s(lb), hksat(lb), nprm, prms(:,lb),  &
                        dz_this, ubc_val, psi_us(lb), hk_top, hk_us(lb))
                  ENDIF

               CASE (BC_RAINFALL)

                  IF (has_wf(lb) .and. (wf(lb) >= tol_z))  THEN

                     qq(lb-1) = - hksat(lb) * ((psi_s(lb) - dp) / wf(lb) - 1)

                  ELSE

                     IF (dp > tol_z) THEN
                        qq(lb-1) = flux_inside_hm_soil ( &
                           psi_s(lb), hksat(lb), nprm, prms(:,lb),  &
                           dz_this, dp, psi_us(lb), hksat(lb), hk_us(lb))
                     ELSE
                        qtest = flux_inside_hm_soil ( &
                           psi_s(lb), hksat(lb), nprm, prms(:,lb),  &
                           dz_this, psi_s(lb), psi_us(lb), hksat(lb), hk_us(lb))

                        qq(lb-1) = min(ubc_val, qtest)

                        IF (is_update_sublevel) THEN
                           IF (qq(lb-1) > qtest) THEN
                              has_wf(lb) = .true.
                              wf(lb) = 0.0
                           ENDIF
                        ENDIF
                     ENDIF

                  ENDIF

               CASE (BC_FIX_FLUX)

                  qq(lb-1) = ubc_val

                  IF (is_update_sublevel) THEN
                     IF ((.not. has_wf(lb)) .and. (ubc_val > hksat(lb))) THEN
                        qtest = flux_inside_hm_soil ( &
                           psi_s(lb), hksat(lb), nprm, prms(:,lb),  &
                           dz_this, psi_s(lb), psi_us(lb), hksat(lb), hk_us(lb))
                        IF (qq(lb-1) > qtest) THEN
                           has_wf(lb) = .true.
                           wf(lb) = 0.0
                        ENDIF
                     ENDIF
                  ENDIF

               ENDSELECT

               IF ((has_wf(lb)) .and. (dz_this >= tol_z)) THEN
                  qq_wf(lb) = flux_inside_hm_soil ( &
                     psi_s(lb), hksat(lb), nprm, prms(:,lb),  &
                     dz_this, psi_s(lb), psi_us(lb), hksat(lb), hk_us(lb))
               ELSE
                  IF (has_wf(lb)) THEN
                     qq_wf(lb) = qq(lb)
                  ELSE
                     qq_wf(lb) = qq(lb-1)
                  ENDIF
               ENDIF

            ELSEIF (ilev_u == ub) THEN
               ! CASE 2: water flux at bottom

               dz_this = (dz(ub) - wf(ub) - wt(ub)) * (sp_zi(ub) - sp_zc(ub))/ dz(ub)

               SELECTCASE (lbc_typ)
               CASE (BC_FIX_HEAD)

                  IF (has_wt(ub)) THEN
                     qq(ub) = - hksat(ub) * ((lbc_val - psi_s(ub))/wt(ub) - 1)
                  ELSE
                     hk_btm = soil_hk_from_psi (lbc_val, &
                        psi_s(ub), hksat(ub), nprm, prms(:,ub))
                     qq(ub) = flux_inside_hm_soil ( &
                        psi_s(ub), hksat(ub), nprm, prms(:,ub), &
                        dz_this, psi_us(ub), lbc_val, hk_us(ub), hk_btm)
                  ENDIF

               CASE (BC_DRAINAGE)

                  IF (has_wt(ub)) THEN
                     IF (zwt > sp_zi(ub)) THEN
                        qq(ub) = hksat(ub)
                     ELSE
                        qq(ub) = 0
                     ENDIF
                  ELSE
                     IF (zwt > sp_zi(ub)) THEN
                        pbtm = psi_s(ub) + sp_zi(ub) - zwt
                        hk_btm = soil_hk_from_psi (pbtm, &
                           psi_s(ub), hksat(ub), nprm, prms(:,ub))
                        qq(ub) = flux_inside_hm_soil ( &
                           psi_s(ub), hksat(ub), nprm, prms(:,ub), &
                           dz_this, psi_us(ub), pbtm, hk_us(ub), hk_btm)
                     ELSE
                        qq(ub) = flux_inside_hm_soil ( &
                           psi_s(ub), hksat(ub), nprm, prms(:,ub), &
                           dz_this, psi_us(ub), psi_s(ub), hk_us(ub), hksat(ub))

                        IF (is_update_sublevel) THEN
                           IF (qq(ub) > 0) THEN
                              has_wt(ub) = .true.
                              wt(ub) = 0
                              qq(ub) = 0
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF

               CASE (BC_FIX_FLUX)

                  qq(ub) = lbc_val

                  IF (is_update_sublevel) THEN
                     IF ((.not. has_wt(ub)) .and. (lbc_val < hksat(ub))) THEN
                        qtest = flux_inside_hm_soil ( &
                           psi_s(ub), hksat(ub), nprm, prms(:,ub),  &
                           dz_this, psi_us(ub), psi_s(ub), hk_us(ub), hksat(ub))

                        IF (qtest > lbc_val) THEN
                           has_wt(ub) = .true.
                           wt(ub) = 0
                        ENDIF
                     ENDIF
                  ENDIF

               ENDSELECT

               IF ((has_wt(ub)) .and. (dz_this >= tol_z))  THEN
                  qq_wt(ub) = flux_inside_hm_soil ( &
                     psi_s(ub), hksat(ub), nprm, prms(:,ub), &
                     dz_this, psi_us(ub), psi_s(ub), hk_us(ub), hksat(ub))
               ELSE
                  IF (has_wt(ub)) THEN
                     qq_wt(ub) = qq(ub-1)
                  ELSE
                     qq_wt(ub) = qq(ub)
                  ENDIF
               ENDIF

            ELSE
               ! CASE 3: inside soil column

               IF ((ilev_u == lb-1) .or. (ilev_l == ub+1)) THEN
                  has_sat_zone = .true.
               ELSEIF (has_wf(ilev_l)) THEN
                  has_sat_zone = .true.
                  IF (ilev_l == ilev_u+1) THEN
                     IF ((wf(ilev_l) < tol_z) .and. (wt(ilev_u) < tol_z)) THEN
                        has_sat_zone = .false.
                     ENDIF
                  ENDIF
               ELSE
                  has_sat_zone = .false.
               ENDIF

               IF (has_sat_zone) THEN
                  ! CASE 3(1): inside soil column, saturated zone
                  CALL flux_sat_zone_all ( &
                     lb, ub, max(ilev_u,lb), min(ilev_l,ub), &
                     dz, sp_zc, sp_zi, vl_s, psi_s, hksat, nprm, prms, &
                     ubc_typ, ubc_val, lbc_typ, lbc_val, &
                     is_sat, has_wf, has_wt, is_update_sublevel, &
                     wf, vl, wt, dp, zwt, psi_us, hk_us, &
                     qq, qq_wt, qq_wf, tol_q, tol_z, tol_p)

               ELSE
                  ! CASE 3(2): inside soil column, unsaturated zone

                  dz_upp = (dz(ilev_u) - wf(ilev_u))  * (sp_zi(ilev_u)-sp_zc(ilev_u))/dz(ilev_u)
                  dz_low = (dz(ilev_l) - wt(ilev_l))  * (sp_zc(ilev_l)-sp_zi(ilev_u))/dz(ilev_l)

                  IF ((dz_upp >= tol_z) .and. (dz_low >= tol_z)) THEN

                     CALL flux_at_unsaturated_interface (nprm, &
                        psi_s(ilev_u), hksat(ilev_u), prms(:,ilev_u), dz_upp, psi_us(ilev_u), hk_us(ilev_u), &
                        psi_s(ilev_l), hksat(ilev_l), prms(:,ilev_l), dz_low, psi_us(ilev_l), hk_us(ilev_l), &
                        qq_wt(ilev_u), qq_wf(ilev_l), tol_q, tol_p)

                     IF (abs(qq_wt(ilev_u) - qq_wf(ilev_l)) < tol_q) THEN

                        qq(ilev_u) = (qq_wt(ilev_u) + qq_wf(ilev_l)) * 0.5_r8
                        qq_wt(ilev_u) = qq(ilev_u)
                        qq_wf(ilev_l) = qq(ilev_u)

                        IF (is_update_sublevel) THEN
                           has_wt(ilev_u) = .false.
                           has_wf(ilev_l) = .false.
                        ENDIF

                     ELSEIF (qq_wt(ilev_u) > qq_wf(ilev_l)) THEN
                        IF (is_update_sublevel) THEN
                           has_wt(ilev_u) = .true.
                           wt(ilev_u) = 0

                           has_wf(ilev_l) = .true.
                           wf(ilev_l) = 0
                        ENDIF

                        IF (has_wt(ilev_u) .and. has_wf(ilev_l)) THEN
                           IF (psi_s(ilev_u) >= psi_s(ilev_l)) THEN
                              qq(ilev_u) = qq_wt(ilev_u)
                           ELSE
                              qq(ilev_u) = qq_wf(ilev_l)
                           ENDIF
                        ELSE
                           qq(ilev_u) = (qq_wt(ilev_u) + qq_wf(ilev_l)) * 0.5_r8
                        ENDIF
                     ENDIF

                  ELSEIF ((dz_upp >= tol_z) .and. (dz_low < tol_z)) THEN

                     psi_i = min(psi_s(ilev_u), psi_s(ilev_l))
                     hk_i  = soil_hk_from_psi (psi_i, &
                        psi_s(ilev_u), hksat(ilev_u), nprm, prms(:,ilev_u))
                     qq(ilev_u) = flux_inside_hm_soil ( &
                        psi_s(ilev_u), hksat(ilev_u), nprm, prms(:,ilev_u),  &
                        dz_upp, psi_us(ilev_u), psi_i, hk_us(ilev_u), hk_i)

                     qq_wt(ilev_u) = qq(ilev_u)
                     qq_wf(ilev_l) = qq(ilev_u)
                     qq_wt(ilev_l) = qq(ilev_u)

                  ELSEIF ((dz_upp < tol_z) .and. (dz_low >= tol_z)) THEN

                     psi_i = min(psi_s(ilev_u), psi_s(ilev_l))
                     hk_i  = soil_hk_from_psi (psi_i, &
                        psi_s(ilev_l), hksat(ilev_l), nprm, prms(:,ilev_l))
                     qq(ilev_u) = flux_inside_hm_soil ( &
                        psi_s(ilev_l), hksat(ilev_l), nprm, prms(:,ilev_l), &
                        dz_low, psi_i, psi_us(ilev_l), hk_i, hk_us(ilev_l))

                     qq_wf(ilev_u) = qq(ilev_u)
                     qq_wt(ilev_u) = qq(ilev_u)
                     qq_wf(ilev_l) = qq(ilev_u)

                  ELSEIF ((dz_upp < tol_z) .and. (dz_low < tol_z)) THEN
                     ! This CASE does not exist in principle.

                     qq(ilev_u) = min(hksat(ilev_u), hksat(ilev_l))

                     qq_wf(ilev_u) = qq(ilev_u)
                     qq_wt(ilev_u) = qq(ilev_u)
                     qq_wf(ilev_l) = qq(ilev_u)
                     qq_wt(ilev_l) = qq(ilev_u)

                  ENDIF
               ENDIF

            ENDIF
         ENDIF

         IF (ilev_l == ub+1) THEN
            EXIT
         ELSE
            ilev_u = ilev_l
            ilev_l = find_unsat_lev_lower (is_sat, lb, ub, ilev_u+1)
         ENDIF
      ENDDO

   END SUBROUTINE flux_all


   !-------------------------------------------------------------------
   SUBROUTINE flux_sat_zone_all ( &
         lb, ub, i_stt, i_end, dz, sp_zc, sp_zi, &
         vl_s, psi_s, hksat, nprm, prms, &
         ubc_typ, ubc_val, lbc_typ, lbc_val, &
         is_sat, has_wf, has_wt, is_update_sublevel, &
         wf, vl, wt, wdsrf, zwt, psi_us, hk_us, &
         qq, qq_wt, qq_wf, tol_q, tol_z, tol_p)

   integer,  intent(in) :: lb, ub
   integer,  intent(in) :: i_stt, i_end

   real(r8), intent(in) :: dz      (lb:ub)
   real(r8), intent(in) :: sp_zc   (lb:ub)
   real(r8), intent(in) :: sp_zi (lb-1:ub)

   real(r8), intent(in) :: vl_s  (lb:ub)
   real(r8), intent(in) :: psi_s (lb:ub)
   real(r8), intent(in) :: hksat (lb:ub)
   integer,  intent(in) :: nprm
   real(r8), intent(in) :: prms (nprm,lb:ub)

   integer,  intent(in) :: ubc_typ
   real(r8), intent(in) :: ubc_val
   integer,  intent(in) :: lbc_typ
   real(r8), intent(in) :: lbc_val

   logical,  intent(inout) :: is_sat (lb:ub)
   logical,  intent(inout) :: has_wf (lb:ub)
   logical,  intent(inout) :: has_wt (lb:ub)
   logical,  intent(in) :: is_update_sublevel

   real(r8), intent(inout) :: wf (lb:ub)
   real(r8), intent(inout) :: vl (lb:ub)
   real(r8), intent(inout) :: wt (lb:ub)
   real(r8), intent(in) :: wdsrf
   real(r8), intent(in) :: zwt
   real(r8), intent(in) :: psi_us (lb:ub)
   real(r8), intent(in) :: hk_us  (lb:ub)

   real(r8), intent(inout) :: qq  (lb-1:ub)
   real(r8), intent(inout) :: qq_wt (lb:ub)
   real(r8), intent(inout) :: qq_wf (lb:ub)

   real(r8), intent(in) :: tol_q
   real(r8), intent(in) :: tol_z
   real(r8), intent(in) :: tol_p

   ! Local variables
   logical :: top_at_ground, top_at_interface, top_inside_level
   logical :: btm_at_bottom, btm_at_interface, btm_inside_level

   integer :: i_s, i_e, ilev, iface

   integer :: nlev_sat
   real(r8), allocatable :: qlc     (:)
   real(r8), allocatable :: dz_sat  (:)
   real(r8), allocatable :: psi_sat (:)
   real(r8), allocatable :: hk_sat  (:)

   real(r8) :: ptop, pbtm, qtop, qupper, qlower
   real(r8) :: dz_us_top, dz_us_btm

   logical :: is_trans

      top_at_ground = (i_stt == lb) .and. is_sat(i_stt)
      top_at_interface = (.not. top_at_ground) .and. (wt(i_stt) < tol_z)
      top_inside_level = .not. (top_at_ground .or. top_at_interface)

      btm_at_bottom = (i_end == ub) .and. is_sat(i_end)
      btm_at_interface = (.not. btm_at_bottom) .and. (wf(i_end) < tol_z)
      btm_inside_level = .not. (btm_at_bottom .or. btm_at_interface)

      IF (top_at_interface) THEN
         i_s = i_stt + 1
      ELSE
         i_s = i_stt
      ENDIF

      IF (btm_at_interface) THEN
         i_e = i_end - 1
      ELSE
         i_e = i_end
      ENDIF

      nlev_sat = i_e - i_s + 1

      allocate (dz_sat  (i_s:i_e))
      allocate (psi_sat (i_s:i_e))
      allocate (hk_sat  (i_s:i_e))
      allocate (qlc     (i_s:i_e))

      DO ilev = i_s, i_e
         dz_sat (ilev) = dz   (ilev)
         psi_sat(ilev) = psi_s(ilev)
         hk_sat (ilev) = hksat(ilev)
      ENDDO

      IF (top_inside_level) dz_sat(i_s) = wt(i_stt)
      IF (btm_inside_level) dz_sat(i_e) = wf(i_end)

      IF (.not. top_at_ground) THEN
         dz_us_top = (dz(i_stt) - wt(i_stt) - wf(i_stt)) &
            * (sp_zi(i_stt) - sp_zc(i_stt)) / dz(i_stt)
      ENDIF

      IF (.not. btm_at_bottom) THEN
         dz_us_btm = (dz(i_end) - wt(i_end) - wf(i_end)) &
            * (sp_zc(i_end) - sp_zi(i_end-1)) / dz(i_end)
      ENDIF

      ! Case 1
      IF (top_at_ground .and. btm_at_bottom) THEN

         ! Case 1-1
         IF ((ubc_typ == BC_FIX_HEAD) .and. (lbc_typ == BC_FIX_HEAD)) THEN
            CALL flux_sat_zone_fixed_bc (nlev_sat, &
               dz_sat, psi_sat, hk_sat, ubc_val, lbc_val, qlc)
         ENDIF

         ! Case 1-2
         IF ((ubc_typ == BC_RAINFALL) .and. (lbc_typ == BC_FIX_HEAD)) THEN

            CALL flux_sat_zone_fixed_bc (nlev_sat, &
               dz_sat, psi_sat, hk_sat, wdsrf, lbc_val, qlc)

         ENDIF

         ! Case 1-3
         IF ((ubc_typ == BC_FIX_FLUX) .and. (lbc_typ == BC_FIX_HEAD)) THEN
            CALL flux_sat_zone_fixed_bc (nlev_sat, &
               dz_sat, psi_sat, hk_sat, psi_s(lb), lbc_val, qlc, &
               flux_top = ubc_val)
         ENDIF

         ! Case 1-4
         IF ((ubc_typ == BC_FIX_HEAD) .and. (lbc_typ == BC_FIX_FLUX)) THEN
            CALL flux_sat_zone_fixed_bc (nlev_sat, &
               dz_sat, psi_sat, hk_sat, ubc_val, psi_s(ub), qlc, &
               flux_btm = lbc_val)
         ENDIF

         ! Case 1-5
         IF ((ubc_typ == BC_RAINFALL) .and. (lbc_typ == BC_FIX_FLUX)) THEN

            CALL flux_sat_zone_fixed_bc (nlev_sat, &
               dz_sat, psi_sat, hk_sat, wdsrf, psi_s(ub), qlc, &
               flux_btm = lbc_val)

         ENDIF

         ! Case 1-6
         IF ((ubc_typ == BC_FIX_FLUX) .and. (lbc_typ == BC_FIX_FLUX)) THEN
            CALL flux_sat_zone_fixed_bc (nlev_sat, &
               dz_sat, psi_sat, hk_sat, psi_s(lb), psi_s(ub), qlc, &
               flux_top = ubc_val, flux_btm = lbc_val)
         ENDIF

         ! Case 1-7
         IF ((ubc_typ == BC_FIX_HEAD) .and. (lbc_typ == BC_DRAINAGE)) THEN
            IF (zwt > sp_zi(ub)) THEN
               CALL flux_sat_zone_fixed_bc (nlev_sat, &
                  dz_sat, psi_sat, hk_sat, ubc_val, psi_s(ub), qlc)
            ELSE
               CALL flux_sat_zone_fixed_bc (nlev_sat, &
                  dz_sat, psi_sat, hk_sat, ubc_val, psi_s(ub), qlc, &
                  flux_btm = 0.0)
            ENDIF
         ENDIF

         ! Case 1-8
         IF ((ubc_typ == BC_RAINFALL) .and. (lbc_typ == BC_DRAINAGE)) THEN
            IF (zwt > sp_zi(ub)) THEN
               CALL flux_sat_zone_fixed_bc (nlev_sat, &
                  dz_sat, psi_sat, hk_sat, wdsrf, psi_s(ub), qlc)
            ELSE
               CALL flux_sat_zone_fixed_bc (nlev_sat, &
                  dz_sat, psi_sat, hk_sat, wdsrf, psi_s(ub), qlc, &
                  flux_btm = 0.0)
            ENDIF

         ENDIF

         ! Case 1-9
         IF ((ubc_typ == BC_FIX_FLUX) .and. (lbc_typ == BC_DRAINAGE)) THEN
            IF (zwt > sp_zi(ub)) THEN
               CALL flux_sat_zone_fixed_bc (nlev_sat, &
                  dz_sat, psi_sat, hk_sat, psi_s(lb), psi_s(ub), qlc, &
                  flux_top = ubc_val)
            ELSE
               CALL flux_sat_zone_fixed_bc (nlev_sat, &
                  dz_sat, psi_sat, hk_sat, psi_s(lb), psi_s(ub), qlc, &
                  flux_top = ubc_val, flux_btm = 0.0)
            ENDIF
         ENDIF
      ENDIF

      ! Case 2
      IF (top_at_ground .and. btm_at_interface) THEN

         SELECTCASE (ubc_typ)
         CASE (BC_FIX_HEAD)

            CALL flux_btm_transitive_interface ( &
               psi_s(i_end), hksat(i_end), nprm, prms(:,i_end), &
               dz_us_btm, psi_us(i_end), hk_us(i_end), &
               nlev_sat, dz_sat, psi_sat, hk_sat, ubc_val, &
               qq_wf(i_end), qlc, tol_q, tol_z, tol_p)

         CASE (BC_FIX_FLUX)

            CALL flux_btm_transitive_interface ( &
               psi_s(i_end), hksat(i_end), nprm, prms(:,i_end), &
               dz_us_btm, psi_us(i_end), hk_us(i_end), &
               nlev_sat, dz_sat, psi_sat, hk_sat, psi_s(lb), &
               qq_wf(i_end), qlc, tol_q, tol_z, tol_p, &
               flux_top = ubc_val)

         CASE (BC_RAINFALL)

            CALL flux_btm_transitive_interface ( &
               psi_s(i_end), hksat(i_end), nprm, prms(:,i_end), &
               dz_us_btm, psi_us(i_end), hk_us(i_end), &
               nlev_sat, dz_sat, psi_sat, hk_sat, wdsrf, &
               qq_wf(i_end), qlc, tol_q, tol_z, tol_p)

         ENDSELECT

      ENDIF

      ! Case 3
      IF (top_at_ground .and. btm_inside_level) THEN

         SELECTCASE (ubc_typ)
         CASE (BC_FIX_HEAD)

            CALL flux_sat_zone_fixed_bc (nlev_sat, dz_sat, psi_sat, &
               hk_sat, ubc_val, psi_s(i_end), qlc)

         CASE (BC_FIX_FLUX)

            CALL flux_sat_zone_fixed_bc (nlev_sat, dz_sat, psi_sat, &
               hk_sat, psi_s(lb), psi_s(i_end), qlc, &
               flux_top = ubc_val)

         CASE (BC_RAINFALL)

            CALL flux_sat_zone_fixed_bc (nlev_sat, dz_sat, psi_sat, &
               hk_sat, wdsrf, psi_s(i_end), qlc)

         ENDSELECT

      ENDIF

      ! Case 4
      IF (top_at_interface .and. btm_at_bottom) THEN

         SELECTCASE (lbc_typ)
         CASE (BC_FIX_HEAD)

            CALL flux_top_transitive_interface ( &
               psi_s(i_stt), hksat(i_stt), nprm, prms(:,i_stt), &
               dz_us_top, psi_us(i_stt), hk_us(i_stt), &
               nlev_sat, dz_sat, psi_sat, hk_sat, lbc_val, &
               qq_wt(i_stt), qlc, tol_q, tol_z, tol_p)

         CASE (BC_FIX_FLUX)

            CALL flux_top_transitive_interface ( &
               psi_s(i_stt), hksat(i_stt), nprm, prms(:,i_stt), &
               dz_us_top, psi_us(i_stt), hk_us(i_stt), &
               nlev_sat, dz_sat, psi_sat, hk_sat, psi_s(ub), &
               qq_wt(i_stt), qlc, tol_q, tol_z, tol_p, &
               flux_btm = lbc_val)

         CASE (BC_DRAINAGE)

            IF (zwt > sp_zi(ub)) THEN
               CALL flux_top_transitive_interface ( &
                  psi_s(i_stt), hksat(i_stt), nprm, prms(:,i_stt), &
                  dz_us_top, psi_us(i_stt), hk_us(i_stt), &
                  nlev_sat, dz_sat, psi_sat, hk_sat, psi_s(ub), &
                  qq_wt(i_stt), qlc, tol_q, tol_z, tol_p)
            ELSE
               CALL flux_top_transitive_interface ( &
                  psi_s(i_stt), hksat(i_stt), nprm, prms(:,i_stt), &
                  dz_us_top, psi_us(i_stt), hk_us(i_stt), &
                  nlev_sat, dz_sat, psi_sat, hk_sat, psi_s(ub), &
                  qq_wt(i_stt), qlc, tol_q, tol_z, tol_p, &
                  flux_btm = 0.0)
            ENDIF

         ENDSELECT

      ENDIF

      ! Case 5
      IF (top_at_interface .and. btm_at_interface) THEN

         CALL flux_both_transitive_interface ( &
            i_stt, i_end, dz(i_stt:i_end), &
            psi_s(i_stt:i_end), hksat(i_stt:i_end), nprm, prms(:,i_stt:i_end), &
            dz_us_top, psi_us(i_stt), hk_us(i_stt), &
            dz_us_btm, psi_us(i_end), hk_us(i_end), &
            qq_wt(i_stt), qq_wf(i_end), qlc, &
            tol_q, tol_z, tol_p)

      ENDIF

      ! Case 6
      IF (top_at_interface .and. btm_inside_level) THEN

         CALL flux_top_transitive_interface ( &
            psi_s(i_stt), hksat(i_stt), nprm, prms(:,i_stt), &
            dz_us_top, psi_us(i_stt), hk_us(i_stt), &
            nlev_sat, dz_sat, psi_sat, hk_sat, psi_s(i_end), &
            qq_wt(i_stt), qlc, tol_q, tol_z, tol_p)

      ENDIF

      ! Case 7
      IF (top_inside_level .and. btm_at_bottom) THEN

         SELECTCASE (lbc_typ)
         CASE (BC_FIX_HEAD)

            CALL flux_sat_zone_fixed_bc (nlev_sat, dz_sat, psi_sat, &
               hk_sat, psi_s(i_stt), lbc_val, qlc)

         CASE (BC_FIX_FLUX)

            CALL flux_sat_zone_fixed_bc (nlev_sat, &
               dz_sat, psi_sat, hk_sat, psi_s(i_stt), psi_s(ub), &
               qlc, flux_btm = lbc_val)

         CASE (BC_DRAINAGE)

            IF (zwt > sp_zi(ub)) THEN
               CALL flux_sat_zone_fixed_bc (nlev_sat, dz_sat, psi_sat, &
                  hk_sat, psi_s(i_stt), psi_s(ub), qlc)
            ELSE
               CALL flux_sat_zone_fixed_bc (nlev_sat, &
                  dz_sat, psi_sat, hk_sat, psi_s(i_stt), psi_s(ub), &
                  qlc, flux_btm = 0.0)
            ENDIF

         ENDSELECT

      ENDIF

      ! Case 8
      IF (top_inside_level .and. btm_at_interface) THEN

         CALL flux_btm_transitive_interface ( &
            psi_s(i_end), hksat(i_end), nprm, prms(:,i_end), &
            dz_us_btm, psi_us(i_end), hk_us(i_end), &
            nlev_sat, dz_sat, psi_sat, hk_sat, psi_s(i_stt), &
            qq_wf(i_end), qlc, tol_q, tol_z, tol_p)

      ENDIF

      ! Case 9
      IF (top_inside_level .and. btm_inside_level) THEN

         CALL flux_sat_zone_fixed_bc ( &
            nlev_sat, dz_sat, psi_sat, hk_sat, &
            psi_s(i_stt), psi_s(i_end), qlc)

      ENDIF

      IF (top_inside_level) THEN
         IF (dz_us_top < tol_z) THEN
            qq_wt(i_stt) = qq(i_stt-1)
         ELSE
            qq_wt(i_stt) = flux_inside_hm_soil ( &
               psi_s(i_stt), hksat(i_stt), nprm, prms(:,i_stt), &
               dz_us_top, psi_us(i_stt), psi_s(i_stt), hk_us(i_stt), hksat(i_stt))
         ENDIF
      ENDIF

      IF (top_at_interface) THEN
         IF (dz_us_top < tol_z) THEN
            qq_wf(i_stt) = qq_wt(i_stt)
         ENDIF
      ENDIF

      IF (top_at_ground) THEN

         SELECTCASE (ubc_typ)
         CASE (BC_FIX_HEAD)
            qq(lb-1) = qlc(lb)
            is_trans = .false.
         CASE (BC_FIX_FLUX)
            qq(lb-1) = ubc_val ! min(qlc(lb), ubc_val)
            is_trans = (qlc(lb) > ubc_val)
         CASE (BC_RAINFALL)
            IF (wdsrf < tol_z) THEN
               qq(lb-1) = min(ubc_val, qlc(lb))
               is_trans = (qlc(lb) > ubc_val)
            ELSE
               qq(lb-1) = qlc(lb)
               is_trans = .false.
            ENDIF
         ENDSELECT

         IF (is_update_sublevel) THEN
            IF (is_trans .and. is_sat(lb)) THEN
               is_sat(lb) = .false.
               has_wf(lb) = .false.
               has_wt(lb) = .true.

               wt(lb) = 0.9*dz(lb)
               vl(lb) = vl_s(lb)
               wf(lb) = 0

               qq_wt(lb) = qq(lb-1)
            ENDIF
         ENDIF
      ENDIF

      DO iface = i_stt, i_end-1
         IF (top_at_interface .and. (iface == i_stt)) THEN
            qupper = qq_wt(i_stt)
         ELSE
            qupper = qlc(iface)
         ENDIF

         IF (btm_at_interface .and. (iface == i_end-1)) THEN
            qlower = qq_wf(i_end)
         ELSE
            qlower = qlc(iface+1)
         ENDIF

         IF (qlower - qupper >= tol_q) THEN
            IF ((psi_s(iface) < psi_s(iface+1)) &
               .or. &
               ((psi_s(iface) == psi_s(iface+1)) .and. (is_sat(iface+1))) &
               .or. &
               (top_at_interface .and. (iface == i_stt))) THEN

               qq(iface) = qupper

               IF (is_update_sublevel .and. is_sat(iface+1)) THEN
                  is_sat(iface+1) = .false.
                  has_wf(iface+1) = .false.
                  has_wt(iface+1) = .true.

                  wt(iface+1) = dz(iface+1)
                  vl(iface+1) = vl_s(iface+1)
                  wf(iface+1) = 0

                  qq_wf(iface+1) = qq(iface)
                  qq_wt(iface+1) = qq(iface)

                  IF (top_at_interface .and. (iface == i_stt)) THEN
                     has_wt(iface) = .false.
                  ENDIF
               ENDIF

            ELSEIF ((psi_s(iface) > psi_s(iface+1)) &
                  .or. &
                  ((psi_s(iface) == psi_s(iface+1)) .and. (.not. is_sat(iface+1))) &
                  .or. &
                  (btm_at_interface .and. (iface == i_end-1))) THEN

               qq(iface) = qlower

               IF (is_update_sublevel .and. is_sat(iface)) THEN
                  is_sat(iface) = .false.
                  has_wt(iface) = .false.
                  has_wf(iface) = .true.

                  wf(iface) = dz(iface)
                  vl(iface) = vl_s(iface)
                  wt(iface) = 0

                  qq_wf(iface) = qq(iface)
                  qq_wt(iface) = qq(iface)

                  IF (btm_at_interface .and. (iface == i_end-1)) THEN
                     has_wf(iface+1) = .false.
                  ENDIF
               ENDIF
            ENDIF
         ELSEIF (qupper - qlower >= tol_q) THEN
            IF (top_at_interface .and. (iface == i_stt)) THEN
               qq(iface) = qlower
            ENDIF

            IF (btm_at_interface .and. (iface == i_end-1)) THEN
               qq(iface) = qupper
            ENDIF
         ELSE
            qq(iface) = (qupper + qlower) * 0.5_r8
         ENDIF
      ENDDO

      IF (btm_at_bottom) THEN
         qq(ub) = qlc(ub)
      ENDIF

      IF (btm_at_interface) THEN
         IF (dz_us_btm < tol_z) THEN
            qq_wt(i_end) = qq_wf(i_end)
         ENDIF
      ENDIF

      IF (btm_inside_level) THEN
         IF (dz_us_btm < tol_z) THEN
            qq_wf(i_end) = qq(i_end)
         ELSE
            qq_wf(i_end) = flux_inside_hm_soil ( &
               psi_s(i_end), hksat(i_end), nprm, prms(:,i_end), &
               dz_us_btm, psi_s(i_end), psi_us(i_end), hksat(i_end), hk_us(i_end))
         ENDIF
      ENDIF

      deallocate (qlc    )
      deallocate (dz_sat )
      deallocate (psi_sat)
      deallocate (hk_sat )

   END SUBROUTINE flux_sat_zone_all

   !--------------------------------------------------------------------------
   SUBROUTINE flux_sat_zone_fixed_bc ( &
         nlev_sat, dz_sat, psi_sat, hk_sat, &
         psi_top, psi_btm, qlc, flux_top, flux_btm)

   IMPLICIT NONE

   integer,  intent(in) :: nlev_sat

   real(r8), intent(in) :: dz_sat  (nlev_sat)
   real(r8), intent(in) :: psi_sat (nlev_sat)
   real(r8), intent(in) :: hk_sat  (nlev_sat)

   real(r8), intent(in) :: psi_top
   real(r8), intent(in) :: psi_btm

   real(r8), intent(inout) :: qlc (nlev_sat)

   real(r8), intent(in), optional :: flux_top
   real(r8), intent(in), optional :: flux_btm

   ! Local variables
   real(r8) :: psi (0:nlev_sat)
   integer  :: ilev, ilev_u, ilev_l
   integer  :: spr(1:nlev_sat)

      IF (present(flux_top) .and. present(flux_btm)) THEN
         IF (flux_top >= flux_btm) THEN
            qlc(:) = flux_btm
            RETURN
         ENDIF
      ENDIF

      psi(0) = psi_top
      psi(nlev_sat) = psi_btm

      DO ilev = 1, nlev_sat
         IF (ilev < nlev_sat) THEN
            psi(ilev) = max(psi_sat(ilev),psi_sat(ilev+1))
         ENDIF

         qlc(ilev) = - hk_sat(ilev) &
            * ((psi(ilev) - psi(ilev-1)) / dz_sat(ilev) - 1)

         spr(ilev) = ilev
      ENDDO

      ilev_u = nlev_sat
      ilev_l = ilev_u
      DO WHILE (.true.)

         IF (ilev_l < nlev_sat) THEN
            ilev = findloc_ud(spr == spr(ilev_l+1), BACK=.true.)
            DO WHILE (qlc(ilev_u) >= qlc(ilev))

               ilev_l = ilev
               qlc(ilev_u:ilev_l) = - (psi(ilev_l) - psi(ilev_u-1) &
                  - sum(dz_sat(ilev_u:ilev_l))) &
                  / sum(dz_sat(ilev_u:ilev_l) / hk_sat(ilev_u:ilev_l))

               spr(ilev_u:ilev_l) = ilev_u

               IF (ilev_l < nlev_sat) THEN
                  spr(ilev_l+1:nlev_sat) = spr(ilev_l+1:nlev_sat) - 1
                  ilev = findloc_ud(spr == spr(ilev_l+1), BACK=.true.)
               ELSE
                  EXIT
               ENDIF
            ENDDO
         ENDIF

         IF ((ilev_l == nlev_sat) .and. (present(flux_btm))) THEN
            IF (qlc(ilev_l) > flux_btm) THEN
               qlc(ilev_u:ilev_l) = flux_btm
            ENDIF
         ENDIF

         IF (ilev_u > 1) THEN
            ilev_u = ilev_u - 1
            ilev_l = ilev_u
         ELSE
            IF (present(flux_top)) THEN
               DO ilev = 1, nlev_sat
                  IF (flux_top > qlc(ilev)) THEN
                     qlc(ilev) = flux_top
                  ELSE
                     EXIT
                  ENDIF
               ENDDO
            ENDIF

            EXIT
         ENDIF

      ENDDO

   END SUBROUTINE flux_sat_zone_fixed_bc


   !-------------------------------------------------------------------------
   real(r8) FUNCTION flux_inside_hm_soil ( &
         psi_s, hksat, nprm, prms, &
         dz, psi_u, psi_l, hk_u, hk_l)

   IMPLICIT NONE

   real(r8), intent(in) :: psi_s, hksat
   integer,  intent(in) :: nprm
   real(r8), intent(in) :: prms(nprm)

   real(r8), intent(in) :: dz
   real(r8), intent(in) :: psi_u, psi_l
   real(r8), intent(in) :: hk_u,  hk_l

   ! Local variables
   real(r8) :: grad_psi
   real(r8) :: hk_m
   real(r8) :: r0, rr

      grad_psi = (1.0_r8 - (psi_l - psi_u)/dz)

      SELECTCASE (effective_hk_type)

      CASE (type_upstream_mean)

         IF (grad_psi < 0) THEN
            flux_inside_hm_soil = hk_l * grad_psi
         ELSE
            flux_inside_hm_soil = hk_u * grad_psi
         ENDIF

      CASE (type_weighted_geometric_mean)

#ifdef Campbell_SOIL_MODEL
         ! bsw => prms(1)
         r0 = 1.0_r8 / (3.0_r8 / prms(1) + 2.0_r8)
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
         ! n_gm => prms(2), L_gm => prms(3)
         r0 = 1.0_r8 / (prms(3) * (prms(2) - 1.0_r8) + prms(2) * 2.0_r8)
#endif

         IF (grad_psi < 0) THEN
            rr = r0
            hk_m = soil_hk_from_psi (psi_l-dz, psi_s, hksat, nprm, prms)
            flux_inside_hm_soil = hk_u**rr * hk_m**(1.0_r8 - rr) * grad_psi
         ELSEIF (grad_psi == 0) THEN
            flux_inside_hm_soil = 0
         ELSEIF ((grad_psi > 0) .and. (grad_psi < 1)) THEN
            rr = max(1.0_r8+r0*psi_l/dz, 1.0_r8-r0)
            flux_inside_hm_soil = hk_u**rr * hk_l**(1.0_r8-rr) * grad_psi
         ELSEIF (grad_psi == 1) THEN
            flux_inside_hm_soil = hk_u
         ELSEIF (grad_psi > 1) THEN
            rr = r0
            flux_inside_hm_soil = hk_u + (psi_u - psi_l)/dz * hk_u**(1.0_r8-rr) * hk_l**rr
         ENDIF

      ENDSELECT

   END FUNCTION flux_inside_hm_soil


   !--------------------------------------------------------
   SUBROUTINE flux_at_unsaturated_interface (&
         nprm, &
         psi_s_u, hksat_u, prms_u, dz_u, psi_u, hk_u, &
         psi_s_l, hksat_l, prms_l, dz_l, psi_l, hk_l, &
         flux_u, flux_l, tol_q, tol_p)

   IMPLICIT NONE

   integer,  intent(in) :: nprm
   real(r8), intent(in) :: psi_s_u, hksat_u, prms_u(nprm)
   real(r8), intent(in) :: psi_s_l, hksat_l, prms_l(nprm)

   real(r8), intent(in) :: dz_u, psi_u, hk_u
   real(r8), intent(in) :: dz_l, psi_l, hk_l

   real(r8), intent(out) :: flux_u
   real(r8), intent(out) :: flux_l

   real(r8), intent(in) :: tol_q
   real(r8), intent(in) :: tol_p

   ! Local variables
   real(r8) :: psi_s_min
   real(r8) :: psi_i
   real(r8) :: psi_i_r,  psi_i_l
   real(r8) :: psi_i_k1
   real(r8) :: hk_i_u, hk_i_l
   real(r8) :: fval, fval_k1
   integer  :: iter


      psi_i_r = max(psi_u + dz_u, psi_l - dz_l)
      psi_i_l = min(psi_u + dz_u, psi_l - dz_l)

      psi_s_min = min(psi_s_u, psi_s_l)

      IF (psi_i_r > psi_s_min) THEN
         hk_i_u = soil_hk_from_psi (psi_s_min, psi_s_u, hksat_u, nprm, prms_u)
         hk_i_l = soil_hk_from_psi (psi_s_min, psi_s_l, hksat_l, nprm, prms_l)

         flux_u = flux_inside_hm_soil ( &
            psi_s_u, hksat_u, nprm, prms_u,  &
            dz_u, psi_u, psi_s_min, hk_u, hk_i_u)

         flux_l = flux_inside_hm_soil (&
            psi_s_l, hksat_l, nprm, prms_l,  &
            dz_l, psi_s_min, psi_l, hk_i_l, hk_l)

         IF (flux_u >= flux_l) THEN
            RETURN
         ELSE
            psi_i_r = psi_s_min
         ENDIF
      ENDIF

      psi_i = (dz_l * psi_u + dz_u * psi_l) / (dz_u + dz_l)
      IF ((psi_i < psi_i_l) .or. (psi_i > psi_i_r)) THEN
         psi_i = (psi_i_r + psi_i_l)/2.0_r8
      ENDIF

      iter = 0
      DO WHILE (iter < 50)
         hk_i_u = soil_hk_from_psi (psi_i, psi_s_u, hksat_u, nprm, prms_u)
         hk_i_l = soil_hk_from_psi (psi_i, psi_s_l, hksat_l, nprm, prms_l)

         flux_u = flux_inside_hm_soil ( &
            psi_s_u, hksat_u, nprm, prms_u,  &
            dz_u, psi_u, psi_i, hk_u, hk_i_u)

         flux_l = flux_inside_hm_soil ( &
            psi_s_l, hksat_l, nprm, prms_l,  &
            dz_l, psi_i, psi_l, hk_i_l, hk_l)

         fval = flux_l - flux_u

         IF ((abs(fval) < tol_q) .or. (psi_i_r - psi_i_l < tol_p)) THEN
            EXIT
         ELSE
            IF (iter == 0) THEN
               IF (fval < 0) THEN
                  psi_i_l = psi_i
               ELSE
                  psi_i_r = psi_i
               ENDIF

               psi_i_k1 = psi_i
               fval_k1 = fval

               psi_i = (psi_i_r + psi_i_l)/2.0_r8
            ELSE
               CALL secant_method_iteration ( &
                  fval, fval_k1, psi_i, psi_i_k1, psi_i_l, psi_i_r)
            ENDIF
         ENDIF

         iter = iter + 1
      ENDDO

#if (defined CoLMDEBUG)
      IF (iter == 50) THEN
         write(*,*) 'Warning : flux_at_unsaturated_interface: not converged.'
      ENDIF
#endif

   END SUBROUTINE flux_at_unsaturated_interface

   !-------------------------------------------------------------------
   SUBROUTINE flux_top_transitive_interface ( &
         psi_s_u, hksat_u, nprm, prms_u, &
         dz_us, psi_us, hk_us, &
         nlev_sat, dz_sat, psi_sat, hk_sat, psi_btm, &
         q_us_up, qlc, tol_q, tol_z, tol_p, flux_btm)

   IMPLICIT NONE

   real(r8), intent(in) :: psi_s_u, hksat_u
   integer,  intent(in) :: nprm
   real(r8), intent(in) :: prms_u(nprm)

   real(r8), intent(in) :: dz_us
   real(r8), intent(in) :: psi_us
   real(r8), intent(in) :: hk_us

   integer,  intent(in) :: nlev_sat
   real(r8), intent(in) :: dz_sat  (nlev_sat)
   real(r8), intent(in) :: psi_sat (nlev_sat)
   real(r8), intent(in) :: hk_sat  (nlev_sat)

   real(r8), intent(in) :: psi_btm

   real(r8), intent(out)   :: q_us_up
   real(r8), intent(inout) :: qlc (nlev_sat)

   real(r8), intent(in) :: tol_q
   real(r8), intent(in) :: tol_z
   real(r8), intent(in) :: tol_p

   real(r8), intent(in), optional :: flux_btm

   ! Local variables
   real(r8) :: psi_i
   real(r8) :: psi_i_r,  psi_i_l
   real(r8) :: psi_i_k1
   real(r8) :: hk_i
   real(r8) :: fval, fval_k1
   integer  :: iter


      IF (dz_us < tol_z) THEN

         psi_i = max(psi_s_u, psi_sat(1))
         IF (present(flux_btm)) THEN
            CALL flux_sat_zone_fixed_bc ( &
               nlev_sat, dz_sat, psi_sat, hk_sat, &
               psi_i, psi_btm, qlc, flux_btm = flux_btm)
         ELSE
            CALL flux_sat_zone_fixed_bc ( &
               nlev_sat, dz_sat, psi_sat, hk_sat, &
               psi_i, psi_btm, qlc)
         ENDIF

         q_us_up = qlc(1)

         RETURN

      ENDIF

      IF (psi_s_u <= psi_sat(1)) THEN
         ! The case psi_s_u < psi_sat(1) does not exist in principle.

         psi_i = psi_s_u
         hk_i  = hksat_u
         q_us_up = flux_inside_hm_soil ( &
            psi_s_u, hksat_u, nprm, prms_u,  &
            dz_us, psi_us, psi_i, hk_us, hk_i)

         psi_i = psi_sat(1)
         IF (present(flux_btm)) THEN
            CALL flux_sat_zone_fixed_bc ( &
               nlev_sat, dz_sat, psi_sat, hk_sat, &
               psi_i, psi_btm, qlc, flux_btm = flux_btm)
         ELSE
            CALL flux_sat_zone_fixed_bc ( &
               nlev_sat, dz_sat, psi_sat, hk_sat, &
               psi_i, psi_btm, qlc)
         ENDIF

         RETURN

      ENDIF

      psi_i = psi_sat(1)
      hk_i  = soil_hk_from_psi (psi_i, psi_s_u, hksat_u, nprm, prms_u)
      q_us_up = flux_inside_hm_soil ( &
         psi_s_u, hksat_u, nprm, prms_u,  &
         dz_us, psi_us, psi_i, hk_us, hk_i)

      IF (present(flux_btm)) THEN
         CALL flux_sat_zone_fixed_bc ( &
            nlev_sat, dz_sat, psi_sat, hk_sat, &
            psi_i, psi_btm, qlc, flux_btm = flux_btm)
      ELSE
         CALL flux_sat_zone_fixed_bc ( &
            nlev_sat, dz_sat, psi_sat, hk_sat, &
            psi_i, psi_btm, qlc)
      ENDIF

      IF (q_us_up <= qlc(1)) THEN
         RETURN
      ELSE
         psi_i_l = psi_sat(1)
      ENDIF

      psi_i = psi_s_u
      hk_i  = hksat_u
      q_us_up = flux_inside_hm_soil (&
         psi_s_u, hksat_u, nprm, prms_u,  &
         dz_us, psi_us, psi_i, hk_us, hk_i)

      IF (present(flux_btm)) THEN
         CALL flux_sat_zone_fixed_bc ( &
            nlev_sat, dz_sat, psi_sat, hk_sat, &
            psi_i, psi_btm, qlc, flux_btm = flux_btm)
      ELSE
         CALL flux_sat_zone_fixed_bc ( &
            nlev_sat, dz_sat, psi_sat, hk_sat, &
            psi_i, psi_btm, qlc)
      ENDIF

      IF (q_us_up >= qlc(1)) THEN
         RETURN
      ELSE
         psi_i_r = psi_s_u
      ENDIF

      psi_i_k1 = psi_i_r
      fval_k1 = qlc(1) - q_us_up

      psi_i = (psi_i_r + psi_i_l)/2.0_r8
      iter = 0
      DO WHILE (iter < 50)
         hk_i = soil_hk_from_psi (psi_i, psi_s_u, hksat_u, nprm, prms_u)
         q_us_up = flux_inside_hm_soil ( &
            psi_s_u, hksat_u, nprm, prms_u,  &
            dz_us, psi_us, psi_i, hk_us, hk_i)

         IF (present(flux_btm)) THEN
            CALL flux_sat_zone_fixed_bc ( &
               nlev_sat, dz_sat, psi_sat, hk_sat, &
               psi_i, psi_btm, qlc, flux_btm = flux_btm)
         ELSE
            CALL flux_sat_zone_fixed_bc ( &
               nlev_sat, dz_sat, psi_sat, hk_sat, &
               psi_i, psi_btm, qlc)
         ENDIF

         fval = qlc(1) - q_us_up

         IF ((abs(fval) < tol_q) .or. (psi_i_r - psi_i_l < tol_p)) THEN
            EXIT
         ELSE
            CALL secant_method_iteration ( &
               fval, fval_k1, psi_i, psi_i_k1, psi_i_l, psi_i_r)
         ENDIF

         iter = iter + 1
      ENDDO

#if (defined CoLMDEBUG)
      IF (iter == 50) THEN
         write(*,*) 'Warning : flux_top_transitive_interface: not converged.'
      ENDIF
#endif

   END SUBROUTINE flux_top_transitive_interface

   !------------------------------------------------------------------------
   SUBROUTINE flux_btm_transitive_interface ( &
         psi_s_l, hksat_l, nprm, prms_l, &
         dz_us, psi_us, hk_us, &
         nlev_sat, dz_sat, psi_sat, hk_sat, psi_top, &
         q_us_l, qlc, tol_q, tol_z, tol_p, flux_top)

   IMPLICIT NONE

   real(r8), intent(in) :: psi_s_l, hksat_l
   integer,  intent(in) :: nprm
   real(r8), intent(in) :: prms_l(nprm)

   real(r8), intent(in) :: dz_us
   real(r8), intent(in) :: psi_us
   real(r8), intent(in) :: hk_us

   integer,  intent(in) :: nlev_sat
   real(r8), intent(in) :: dz_sat  (nlev_sat)
   real(r8), intent(in) :: psi_sat (nlev_sat)
   real(r8), intent(in) :: hk_sat  (nlev_sat)

   real(r8), intent(in) :: psi_top

   real(r8), intent(out)   :: q_us_l
   real(r8), intent(inout) :: qlc (nlev_sat)

   real(r8), intent(in) :: tol_q
   real(r8), intent(in) :: tol_z
   real(r8), intent(in) :: tol_p

   real(r8), intent(in), optional :: flux_top

   ! Local variables
   real(r8) :: psi_i
   real(r8) :: psi_i_r,  psi_i_l
   real(r8) :: psi_i_k1
   real(r8) :: hk_i
   real(r8) :: fval, fval_k1
   integer  :: iter


      IF (dz_us < tol_z) THEN

         psi_i = max(psi_sat(nlev_sat), psi_s_l)
         IF (present(flux_top)) THEN
            CALL flux_sat_zone_fixed_bc ( &
               nlev_sat, dz_sat, psi_sat, hk_sat, &
               psi_top, psi_i, qlc, flux_top = flux_top)
         ELSE
            CALL flux_sat_zone_fixed_bc ( &
               nlev_sat, dz_sat, psi_sat, hk_sat, &
               psi_top, psi_i, qlc)
         ENDIF

         q_us_l = qlc(nlev_sat)

         RETURN

      ENDIF

      IF (psi_sat(nlev_sat) >= psi_s_l) THEN
         ! The case psi_sat(nlev_sat) > psi_s_l does not exist in principle.

         psi_i = psi_s_l
         hk_i  = hksat_l
         q_us_l = flux_inside_hm_soil ( &
            psi_s_l, hksat_l, nprm, prms_l,  &
            dz_us, psi_i, psi_us, hk_i, hk_us)

         psi_i = psi_sat(nlev_sat)
         IF (present(flux_top)) THEN
            CALL flux_sat_zone_fixed_bc ( &
               nlev_sat, dz_sat, psi_sat, hk_sat, &
               psi_top, psi_i, qlc, flux_top = flux_top)
         ELSE
            CALL flux_sat_zone_fixed_bc ( &
               nlev_sat, dz_sat, psi_sat, hk_sat, &
               psi_top, psi_i, qlc)
         ENDIF

         RETURN

      ENDIF

      psi_i = psi_sat(nlev_sat)

      IF (present(flux_top)) THEN
         CALL flux_sat_zone_fixed_bc ( &
            nlev_sat, dz_sat, psi_sat, hk_sat, &
            psi_top, psi_i, qlc, flux_top = flux_top)
      ELSE
         CALL flux_sat_zone_fixed_bc ( &
            nlev_sat, dz_sat, psi_sat, hk_sat, &
            psi_top, psi_i, qlc)
      ENDIF

      hk_i = soil_hk_from_psi (psi_i, psi_s_l, hksat_l, nprm, prms_l)
      q_us_l = flux_inside_hm_soil ( &
         psi_s_l, hksat_l, nprm, prms_l,  &
         dz_us, psi_i, psi_us, hk_i, hk_us)

      IF (qlc(nlev_sat) <= q_us_l) THEN
         RETURN
      ELSE
         psi_i_l = psi_sat(nlev_sat)
      ENDIF

      psi_i = psi_s_l

      IF (present(flux_top)) THEN
         CALL flux_sat_zone_fixed_bc ( &
            nlev_sat, dz_sat, psi_sat, hk_sat, &
            psi_top, psi_i, qlc, flux_top = flux_top)
      ELSE
         CALL flux_sat_zone_fixed_bc ( &
            nlev_sat, dz_sat, psi_sat, hk_sat, &
            psi_top, psi_i, qlc)
      ENDIF

      hk_i = soil_hk_from_psi (psi_i, psi_s_l, hksat_l, nprm, prms_l)
      q_us_l = flux_inside_hm_soil ( &
         psi_s_l, hksat_l, nprm, prms_l,  &
         dz_us, psi_i, psi_us, hk_i, hk_us)

      IF (qlc(nlev_sat) >= q_us_l) THEN
         RETURN
      ELSE
         psi_i_r = psi_s_l
      ENDIF

      psi_i_k1 = psi_i_r
      fval_k1 = q_us_l - qlc(nlev_sat)

      psi_i = (psi_i_r + psi_i_l)/2.0_r8
      iter = 0
      DO WHILE (iter < 50)

         IF (present(flux_top)) THEN
            CALL flux_sat_zone_fixed_bc ( &
               nlev_sat, dz_sat, psi_sat, hk_sat, &
               psi_top, psi_i, qlc, flux_top = flux_top)
         ELSE
            CALL flux_sat_zone_fixed_bc ( &
               nlev_sat, dz_sat, psi_sat, hk_sat, &
               psi_top, psi_i, qlc)
         ENDIF

         hk_i = soil_hk_from_psi (psi_i, psi_s_l, hksat_l, nprm, prms_l)
         q_us_l = flux_inside_hm_soil ( &
            psi_s_l, hksat_l, nprm, prms_l,  &
            dz_us, psi_i, psi_us, hk_i, hk_us)

         fval = q_us_l - qlc(nlev_sat)

         IF ((abs(fval) < tol_q) .or. (psi_i_r - psi_i_l < tol_p)) THEN
            EXIT
         ELSE
            CALL secant_method_iteration ( &
               fval, fval_k1, psi_i, psi_i_k1, psi_i_l, psi_i_r)
         ENDIF

         iter = iter + 1
      ENDDO

#if (defined CoLMDEBUG)
      IF (iter == 50) THEN
         write(*,*) 'Warning : flux_btm_transitive_interface: not converged.'
      ENDIF
#endif

   END SUBROUTINE flux_btm_transitive_interface


   !-----------------------------------------------------------------------
   SUBROUTINE flux_both_transitive_interface ( &
         ilev_us_u, ilev_us_l, &
         dz, psi_s, hksat, nprm, prms, &
         dz_us_u, psi_us_u, hk_us_u, &
         dz_us_l, psi_us_l, hk_us_l, &
         q_us_u, q_us_l, qlc, &
         tol_q, tol_z, tol_p)

   IMPLICIT NONE

   integer,  intent(in) :: ilev_us_u, ilev_us_l
   real(r8), intent(in) :: dz    (ilev_us_u:ilev_us_l)
   real(r8), intent(in) :: psi_s (ilev_us_u:ilev_us_l)
   real(r8), intent(in) :: hksat (ilev_us_u:ilev_us_l)

   integer,  intent(in) :: nprm
   real(r8), intent(in) :: prms (nprm,ilev_us_u:ilev_us_l)

   real(r8), intent(in) :: dz_us_u, psi_us_u, hk_us_u
   real(r8), intent(in) :: dz_us_l, psi_us_l, hk_us_l

   real(r8), intent(out) :: q_us_u, q_us_l
   real(r8), intent(inout) :: qlc (ilev_us_u+1:ilev_us_l-1)

   real(r8), intent(in) :: tol_q
   real(r8), intent(in) :: tol_z
   real(r8), intent(in) :: tol_p

   ! Local variables
   integer  :: nlev_sat
   real(r8) :: psi_i
   real(r8) :: psi_i_r,  psi_i_l
   real(r8) :: psi_i_k1
   real(r8) :: hk_i
   real(r8) :: fval, fval_k1
   integer  :: iter

      nlev_sat = ilev_us_l - ilev_us_u - 1

      IF ((psi_s(ilev_us_u) <= psi_s(ilev_us_u+1)) &
         .or. (dz_us_u < tol_z)) THEN

         psi_i = max(psi_s(ilev_us_u), psi_s(ilev_us_u+1))
         CALL flux_btm_transitive_interface ( &
            psi_s(ilev_us_l), hksat(ilev_us_l), nprm, prms(:,ilev_us_l), &
            dz_us_l, psi_us_l, hk_us_l, &
            nlev_sat, dz(ilev_us_u+1:ilev_us_l-1), psi_s(ilev_us_u+1:ilev_us_l-1), &
            hksat(ilev_us_u+1:ilev_us_l-1), psi_i, &
            q_us_l, qlc, tol_q, tol_z, tol_p)

         IF (dz_us_u < tol_z) THEN
            q_us_u = qlc(ilev_us_u+1)
         ELSE
            q_us_u = flux_inside_hm_soil ( &
               psi_s(ilev_us_u), hksat(ilev_us_u), nprm, prms(:,ilev_us_u),  &
               dz_us_u, psi_us_u, psi_s(ilev_us_u), hk_us_u, hksat(ilev_us_u))
         ENDIF

         RETURN
      ENDIF

      IF ((psi_s(ilev_us_l) <= psi_s(ilev_us_l-1)) &
         .or. (dz_us_l < tol_z)) THEN

         psi_i = max(psi_s(ilev_us_l-1), psi_s(ilev_us_l))
         CALL flux_top_transitive_interface ( &
            psi_s(ilev_us_u), hksat(ilev_us_u), nprm, prms(:,ilev_us_u), &
            dz_us_u, psi_us_u, hk_us_u, &
            nlev_sat, dz(ilev_us_u+1:ilev_us_l-1), &
            psi_s(ilev_us_u+1:ilev_us_l-1), hksat(ilev_us_u+1:ilev_us_l-1), psi_i, &
            q_us_u, qlc, tol_q, tol_z, tol_p)

         IF (dz_us_l < tol_z) THEN
            q_us_l = qlc(ilev_us_l-1)
         ELSE
            q_us_l = flux_inside_hm_soil ( &
               psi_s(ilev_us_l), hksat(ilev_us_l), nprm, prms(:,ilev_us_l),  &
               dz_us_l, psi_s(ilev_us_l), psi_us_l, hksat(ilev_us_l), hk_us_l)
         ENDIF

         RETURN
      ENDIF

      psi_i_l = psi_s(ilev_us_l-1)

      CALL flux_top_transitive_interface ( &
         psi_s(ilev_us_u), hksat(ilev_us_u), nprm, prms(:,ilev_us_u), &
         dz_us_u, psi_us_u, hk_us_u, &
         nlev_sat, dz(ilev_us_u+1:ilev_us_l-1), &
         psi_s(ilev_us_u+1:ilev_us_l-1), hksat(ilev_us_u+1:ilev_us_l-1), psi_i_l, &
         q_us_u, qlc, tol_q/2.0_r8, tol_z, tol_p)

      hk_i = soil_hk_from_psi (psi_i_l, &
         psi_s(ilev_us_l), hksat(ilev_us_l), nprm, prms(:,ilev_us_l))
      q_us_l = flux_inside_hm_soil ( &
         psi_s(ilev_us_l), hksat(ilev_us_l), nprm, prms(:,ilev_us_l),  &
         dz_us_l, psi_i_l, psi_us_l, hk_i, hk_us_l)

      IF (qlc(ilev_us_l-1) <= q_us_l) THEN
         RETURN
      ENDIF

      psi_i_r = psi_s(ilev_us_l)

      CALL flux_top_transitive_interface ( &
         psi_s(ilev_us_u), hksat(ilev_us_u), nprm, prms(:,ilev_us_u), &
         dz_us_u, psi_us_u, hk_us_u, &
         nlev_sat, dz(ilev_us_u+1:ilev_us_l-1), &
         psi_s(ilev_us_u+1:ilev_us_l-1), hksat(ilev_us_u+1:ilev_us_l-1), psi_i_r, &
         q_us_u, qlc, tol_q/2.0_r8, tol_z, tol_p)

      hk_i = soil_hk_from_psi (psi_i_r, &
         psi_s(ilev_us_l), hksat(ilev_us_l), nprm, prms(:,ilev_us_l))
      q_us_l = flux_inside_hm_soil ( &
         psi_s(ilev_us_l), hksat(ilev_us_l), nprm, prms(:,ilev_us_l),  &
         dz_us_l, psi_i_r, psi_us_l, hk_i, hk_us_l)

      IF (qlc(ilev_us_l-1) >= q_us_l ) THEN
         RETURN
      ENDIF

      psi_i_k1 = psi_i_r
      fval_k1 = q_us_l - qlc(ilev_us_l-1)

      psi_i = (psi_i_r + psi_i_l)/2.0_r8
      iter = 0
      DO WHILE (iter < 50)

         CALL flux_top_transitive_interface ( &
            psi_s(ilev_us_u), hksat(ilev_us_u), nprm, prms(:,ilev_us_u), &
            dz_us_u, psi_us_u, hk_us_u, &
            nlev_sat, dz(ilev_us_u+1:ilev_us_l-1), &
            psi_s(ilev_us_u+1:ilev_us_l-1), hksat(ilev_us_u+1:ilev_us_l-1), psi_i, &
            q_us_u, qlc, tol_q/2.0_r8, tol_z, tol_p)

         hk_i = soil_hk_from_psi (psi_i, &
            psi_s(ilev_us_l), hksat(ilev_us_l), nprm, prms(:,ilev_us_l))
         q_us_l = flux_inside_hm_soil ( &
            psi_s(ilev_us_l), hksat(ilev_us_l), nprm, prms(:,ilev_us_l),  &
            dz_us_l, psi_i, psi_us_l, hk_i, hk_us_l)

         fval = q_us_l - qlc(ilev_us_l-1)

         IF ((abs(fval) < tol_q) .or. (psi_i_r - psi_i_l < tol_p)) THEN
            EXIT
         ELSE
            CALL secant_method_iteration ( &
               fval, fval_k1, psi_i, psi_i_k1, psi_i_l, psi_i_r)
         ENDIF

         iter = iter + 1
      ENDDO

#if (defined CoLMDEBUG)
      IF (iter == 50) THEN
         write(*,*) 'Warning : flux_both_transitive_interface: not converged.'
      ENDIF
#endif

   END SUBROUTINE flux_both_transitive_interface

   !-----------------------------------------------------------------
   SUBROUTINE get_zwt_from_wa ( &
         vl_s, vl_r, psi_s, hksat, nprm, prms, tol_v, tol_z, &
         wa, zmin, zwt)

   IMPLICIT NONE

   real(r8), intent(in)  :: vl_s, vl_r, psi_s, hksat
   integer,  intent(in)  :: nprm
   real(r8), intent(in)  :: prms(nprm)
   real(r8), intent(in)  :: tol_v, tol_z
   real(r8), intent(in)  :: wa, zmin
   real(r8), intent(out) :: zwt

   real(r8) :: vl
   real(r8) :: zwt_l, zwt_r, zwt_k1
   real(r8) :: fval, fval_k1
   real(r8) :: psi
   integer  :: iter

      IF (wa >= 0) THEN
         zwt = zmin
         vl  = vl_s
         RETURN
      ENDIF

      zwt = zmin + (-wa)/vl_s * 2.0
      psi = psi_s - (zwt - zmin) * 0.5
      vl  = soil_vliq_from_psi (psi, &
         vl_s, vl_r, psi_s, nprm, prms)
      DO WHILE (wa <= -(zwt-zmin)*(vl_s-vl))
         zwt = zmin + (zwt-zmin)*2 + 0.1
         psi = psi_s - (zwt - zmin) * 0.5
         vl  = soil_vliq_from_psi (psi, &
            vl_s, vl_r, psi_s, nprm, prms)
      ENDDO

      zwt_r = zwt
      zwt_l = zmin

      zwt_k1 = zwt_l
      fval_k1 = wa

      zwt = (zwt_l + zwt_r) / 2.0
      iter = 0
      DO WHILE (iter < 50)

         psi = psi_s - (zwt - zmin) * 0.5
         vl  = soil_vliq_from_psi (psi, &
               vl_s, vl_r, psi_s, nprm, prms)
         fval = wa + (zwt-zmin)* (vl_s-vl)

         IF ((abs(fval) < tol_v) .or. (zwt_r - zwt_l < tol_z)) THEN
            EXIT
         ELSE
            CALL secant_method_iteration ( &
               fval, fval_k1, zwt, zwt_k1, zwt_l, zwt_r)
         ENDIF

         iter = iter + 1
      ENDDO

#if (defined CoLMDEBUG)
      IF (iter == 50) THEN
         write(*,*) 'Warning : get_zwt_from_wa: not converged.'
      ENDIF
#endif

   END SUBROUTINE get_zwt_from_wa


   !---------------------------------------------------------------------
   SUBROUTINE solve_least_squares_problem (ndim, dr_dv, lact, rhs, dv)
      ! By using Givens rotation.

   IMPLICIT NONE

   integer,  intent(in) :: ndim
   real(r8), intent(in) :: dr_dv (ndim,ndim)
   logical,  intent(in) :: lact  (ndim)
   real(r8), intent(in) :: rhs   (ndim)

   real(r8), intent(out) :: dv (ndim)

   ! Local variables
   real(r8) :: Amatrix (ndim,ndim)
   real(r8) :: res     (ndim)
   integer  :: i, j, k
   real(r8) :: tau, c, s
   real(r8) :: tmp

      Amatrix = dr_dv
      res = rhs
      dv = 0

      DO i = 1, ndim
         IF (lact(i)) THEN

            DO j = i+1, ndim
               IF (Amatrix(j,i) /= 0) THEN
                  IF (abs(Amatrix(j,i)) > abs(Amatrix(i,i))) THEN
                     tau = Amatrix(i,i) / Amatrix(j,i)
                     s = 1 / sqrt(1 + tau**2)
                     c = s * tau
                  ELSE
                     tau = Amatrix(j,i) / Amatrix(i,i)
                     c = 1 / sqrt(1 + tau**2)
                     s = c * tau
                  ENDIF

                  Amatrix(i,i) = c * Amatrix(i,i) + s * Amatrix(j,i)
                  Amatrix(j,i) = 0

                  DO k = i+1, ndim
                     IF (lact(k)) THEN
                        tmp          =   c * Amatrix(i,k) + s * Amatrix(j,k)
                        Amatrix(j,k) = - s * Amatrix(i,k) + c * Amatrix(j,k)
                        Amatrix(i,k) = tmp
                     ENDIF
                  ENDDO

                  tmp    =   c * res(i) + s * res(j)
                  res(j) = - s * res(i) + c * res(j)
                  res(i) = tmp
               ENDIF
            ENDDO

         ENDIF
      ENDDO

      dv = 0

      DO i = ndim, 1, -1
         IF (lact(i)) THEN

            dv(i) = res(i)

            DO k = i+1, ndim
               IF (lact(k)) THEN
                  dv(i) = dv(i) - Amatrix(i,k) * dv(k)
               ENDIF
            ENDDO

            dv(i) = dv(i) / Amatrix(i,i)

         ENDIF
      ENDDO

   END SUBROUTINE solve_least_squares_problem


   !---------------------------------------------------------------------------------
   SUBROUTINE secant_method_iteration ( &
         fval, fval_k1, x_i, x_k1, x_l, x_r)

   IMPLICIT NONE

   real(r8), intent(in) :: fval
   real(r8), intent(inout) :: fval_k1

   real(r8), intent(inout) :: x_i, x_k1
   real(r8), intent(inout) :: x_l, x_r

   real(r8), parameter :: alp = 0.9_r8

   ! Local variables
   real(r8) :: x_k2, fval_k2

      IF (fval > 0.0_r8) THEN
         x_r = x_i
      ELSE
         x_l = x_i
      ENDIF

      fval_k2 = fval_k1
      fval_k1 = fval

      x_k2 = x_k1
      x_k1 = x_i

      IF (fval_k1 == fval_k2) THEN
         x_i = (x_l + x_r) * 0.5_r8
      ELSE
         x_i = (fval_k1 * x_k2 - fval_k2 * x_k1) / (fval_k1 - fval_k2)
         x_i = max(x_i, x_l * alp + x_r * (1.0_r8 - alp))
         x_i = min(x_i, x_l * (1.0_r8 - alp) + x_r * alp)
      ENDIF

   END SUBROUTINE secant_method_iteration


   !-------------------------------------------------------------------------------
   integer FUNCTION find_unsat_lev_lower (is_sat, lb, ub, ilev)

   IMPLICIT NONE

   integer, intent(in) :: lb, ub
   logical, intent(in) :: is_sat (lb:ub)
   integer, intent(in) :: ilev

      find_unsat_lev_lower = ilev
      DO WHILE (find_unsat_lev_lower <= ub)
         IF (is_sat(find_unsat_lev_lower)) THEN
            find_unsat_lev_lower = find_unsat_lev_lower + 1
         ELSE
            EXIT
         ENDIF
      ENDDO

   END FUNCTION find_unsat_lev_lower

   ! -----
   SUBROUTINE print_VSF_iteration_stat_info ()

   USE MOD_MPAS_MPI
   IMPLICIT NONE

   integer(8), SAVE :: count_implicit_accum = 0
   integer(8), SAVE :: count_explicit_accum = 0
   integer(8), SAVE :: count_wet2dry_accum  = 0
#ifdef CoLMDEBUG
	      IF (.true.) THEN
#ifdef MPAS_MPI
	         CALL mpi_allreduce (MPI_IN_PLACE, count_implicit, 1, MPI_INTEGER8, MPI_SUM, mpas_comm, mpas_mpi_ierr)
	         CALL mpas_mpi_check('VSF implicit-iteration count reduction')
	         CALL mpi_allreduce (MPI_IN_PLACE, count_explicit, 1, MPI_INTEGER8, MPI_SUM, mpas_comm, mpas_mpi_ierr)
	         CALL mpas_mpi_check('VSF explicit-iteration count reduction')
	         CALL mpi_allreduce (MPI_IN_PLACE, count_wet2dry , 1, MPI_INTEGER8, MPI_SUM, mpas_comm, mpas_mpi_ierr)
	         CALL mpas_mpi_check('VSF wet-to-dry count reduction')
#endif
	         IF (mpas_rank == mpas_root) THEN
	            count_implicit_accum = count_implicit_accum + count_implicit
	            count_explicit_accum = count_explicit_accum + count_explicit
	            count_wet2dry_accum  = count_wet2dry_accum  + count_wet2dry
	         ENDIF
	      ENDIF

	      IF (mpas_is_root) THEN
	         write(*,"(/,A,I13,A,I13,A,I13,A)") 'VSF scheme this step: ',    &
            count_implicit, ' (implicit)', count_explicit, ' (explicit)', count_wet2dry, ' (wet2dry)'
         write(*,"(A,I13,A,I13,A,I13,A)") 'VSF scheme all steps: ',      &
            count_implicit_accum, ' (implicit)', count_explicit_accum, ' (explicit)', &
            count_wet2dry_accum, ' (wet2dry)'
      ENDIF

      count_implicit = 0
      count_explicit = 0
      count_wet2dry  = 0
#endif
   END SUBROUTINE print_VSF_iteration_stat_info

END MODULE MOD_Hydro_SoilWater
