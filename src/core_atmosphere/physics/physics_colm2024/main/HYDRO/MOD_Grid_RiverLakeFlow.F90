#include <define.h>

#ifdef GridRiverLakeFlow
MODULE MOD_Grid_RiverLakeFlow
!-------------------------------------------------------------------------------------
! DESCRIPTION:
!
!   River Lake flow.
!
!-------------------------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_MPAS_MPI
   USE MOD_Namelist
   USE MOD_Grid_RiverLakeNetwork
   USE MOD_Grid_RiverLakeTimeVars
   USE MOD_Grid_Reservoir
#ifdef GridRiverLakeSediment
   USE MOD_Grid_RiverLakeSediment, only: grid_sediment_init, grid_sediment_calc, &
      grid_sediment_final, sediment_diag_accumulate, sediment_forcing_put, &
      read_sediment_restart
#endif
   IMPLICIT NONE

   real(r8), parameter :: RIVERMIN  = 1.e-5_r8

   real(r8), save :: acctime_rnof_max

   logical,  allocatable :: filter_rnof (:)

CONTAINS

   ! ---------
   SUBROUTINE grid_riverlake_flow_init ()

   USE MOD_LandPatch,           only: numpatch
   USE MOD_Vars_TimeInvariants, only: patchtype, patchmask
   USE, INTRINSIC :: ieee_arithmetic, only: ieee_is_finite
   IMPLICIT NONE

      IF (.not. ieee_is_finite(DEF_GRIDBASED_ROUTING_MAX_DT) .or. &
          DEF_GRIDBASED_ROUTING_MAX_DT <= 0._r8) THEN
         CALL CoLM_stop('DEF_GRIDBASED_ROUTING_MAX_DT must be finite and positive.')
      ENDIF
      acctime_rnof_max = DEF_GRIDBASED_ROUTING_MAX_DT

      ! excluding (patchtype >= 99), virtual patches and those forcing missed
      IF (.true.) THEN
         IF (.not. allocated(wdsrf_ucat) .or. .not. allocated(veloc_riv) .or. &
             .not. allocated(momen_riv) .or. .not. allocated(acc_rnof_uc)) THEN
            CALL CoLM_stop('Embedded CoLM river state is not allocated.')
         ENDIF
         IF (size(wdsrf_ucat) /= numucat .or. size(veloc_riv) /= numucat .or. &
             size(momen_riv) /= numucat .or. size(acc_rnof_uc) /= numucat) THEN
            CALL CoLM_stop('Embedded CoLM river state dimensions do not match local unit catchments.')
         ENDIF
         IF (DEF_Reservoir_Method > 0) THEN
            IF (.not. allocated(volresv)) THEN
               CALL CoLM_stop('Embedded CoLM river state is missing local reservoir storage.')
            ENDIF
            IF (size(volresv) /= numresv) THEN
               CALL CoLM_stop('Embedded CoLM reservoir state dimensions do not match local reservoirs.')
            ENDIF
         ENDIF
         allocate (filter_rnof (numpatch))
         IF (numpatch > 0) THEN
            filter_rnof = patchtype < 99
            filter_rnof = filter_rnof .and. patchmask
         ENDIF
      ENDIF

#ifdef GridRiverLakeSediment
      CALL grid_sediment_init()
      IF (len_trim(gridriver_restart_file) > 0) THEN
         CALL read_sediment_restart(gridriver_restart_file)
      ENDIF
#endif

   END SUBROUTINE grid_riverlake_flow_init

   ! ---------
   SUBROUTINE grid_riverlake_flow (year, deltime)

   USE MOD_Utils
   USE MOD_Namelist,       only: DEF_Reservoir_Method, DEF_USE_SEDIMENT
   USE MOD_Vars_1DFluxes,  only: rnof
   USE MOD_LandPatch,      only: elm_patch, numpatch
   USE MOD_Const_Physical, only: grav
   USE MOD_Vars_Global,    only: spval
#ifdef GridRiverLakeSediment
   USE MOD_Vars_1DForcing, only: forc_prc, forc_prl
#endif
   USE, INTRINSIC :: ieee_arithmetic, only: ieee_is_finite
   IMPLICIT NONE

   integer,  intent(in) :: year
   real(r8), intent(in) :: deltime

   ! Local Variables
   integer  :: i, irsv, ntimestep
   real(r8) :: dt_this

   real(r8), allocatable :: rnof_gd(:)
   real(r8), allocatable :: rnof_uc(:)

#ifdef GridRiverLakeSediment
   real(r8), allocatable :: prcp_gd(:)
   real(r8), allocatable :: prcp_uc(:)
   real(r8), allocatable :: prcp_pch(:)
   real(r8), allocatable :: floodarea_sed(:)
#endif

   logical,  allocatable :: is_built_resv(:)

   real(r8), allocatable :: wdsrf_next(:)
   real(r8), allocatable :: veloc_next(:)

   real(r8), allocatable :: hflux_fc(:)
   real(r8), allocatable :: mflux_fc(:)
   real(r8), allocatable :: zgrad_dn(:)
   real(r8), allocatable :: acc_discharge(:)

   real(r8), allocatable :: hflux_resv(:)
   real(r8), allocatable :: mflux_resv(:)

   real(r8), allocatable :: hflux_sumups(:)
   real(r8), allocatable :: mflux_sumups(:)
   real(r8), allocatable :: zgrad_sumups(:)

   real(r8), allocatable :: sum_hflux_riv(:)
   real(r8), allocatable :: sum_mflux_riv(:)
   real(r8), allocatable :: sum_zgrad_riv(:)

   real(r8) :: veloct_fc, height_fc
   real(r8) :: bedelv_fc, height_up, height_dn
   real(r8) :: vwave_up, vwave_dn, hflux_up, hflux_dn, mflux_up, mflux_dn
   real(r8) :: volwater, friction
   real(r8),  allocatable :: dt_res(:), dt_all(:)
   logical,   allocatable :: ucatfilter(:)
   real(r8) :: global_dt_remaining(1)
#ifdef CoLMDEBUG
   real(r8) :: totalvol_bef, totalvol_aft, totalrnof, totaldis
#endif

      IF (.not. ieee_is_finite(deltime) .or. deltime <= 0._r8) THEN
         CALL CoLM_stop('Embedded CoLM river routing timestep must be finite and positive.')
      ENDIF
      IF (.not. ieee_is_finite(acctime_rnof) .or. acctime_rnof < 0._r8) THEN
         CALL CoLM_stop('Embedded CoLM river routing accumulator time is invalid.')
      ENDIF

      IF (.true.) THEN

         allocate (rnof_gd (numinpm))
         allocate (rnof_uc (numucat))

         IF (numpatch > 0) THEN
            IF (size(filter_rnof) < numpatch .or. size(rnof) < numpatch) THEN
               CALL CoLM_stop('Embedded CoLM runoff arrays do not match the local patch count.')
            ENDIF
            IF (any(filter_rnof(1:numpatch) .and. .not. ieee_is_finite(rnof(1:numpatch)))) THEN
               CALL CoLM_stop('Embedded CoLM runoff contains non-finite values on active patches.')
            ENDIF
            CALL compute_remap_data_pset2grid (remap_patch2inpm, rnof, rnof_gd, &
               fillvalue = 0., filter = filter_rnof)
         ELSE
            rnof_gd = 0._r8
         ENDIF

         IF (numinpm > 0) THEN
            WHERE (push_ucat2inpm%sum_area > 0)
               rnof_gd = rnof_gd / push_ucat2inpm%sum_area
            END WHERE
         ENDIF

         CALL compute_push_data (push_inpm2ucat, rnof_gd, rnof_uc, &
            fillvalue = 0., mode = 'sum')

         IF (numucat > 0) THEN
            IF (.not. all(ieee_is_finite(rnof_uc))) THEN
               CALL CoLM_stop('Embedded CoLM unit-catchment runoff contains non-finite values.')
            ENDIF
            acc_rnof_uc = acc_rnof_uc + rnof_uc*1.e-3*deltime
            IF (.not. all(ieee_is_finite(acc_rnof_uc))) THEN
               CALL CoLM_stop('Embedded CoLM accumulated runoff contains non-finite values.')
            ENDIF
         ENDIF

         deallocate(rnof_gd)
         deallocate(rnof_uc)

#ifdef GridRiverLakeSediment
         IF (DEF_USE_SEDIMENT) THEN
            ! Allocate zero-length arrays on empty ranks to avoid passing unallocated
            ! arrays to assumed-shape dummy arguments in MPI communication routines.
            IF (numpatch > 0) THEN
               allocate (prcp_pch (numpatch))
               prcp_pch = forc_prc + forc_prl
            ELSE
               allocate (prcp_pch (0))
            ENDIF
            IF (numinpm > 0) THEN
               allocate (prcp_gd (numinpm))
            ELSE
               allocate (prcp_gd (0))
            ENDIF
            IF (numucat > 0) THEN
               allocate (prcp_uc (numucat))
            ELSE
               allocate (prcp_uc (0))
            ENDIF

            CALL compute_remap_data_pset2grid (remap_patch2inpm, prcp_pch, prcp_gd, &
               fillvalue = 0., filter = filter_rnof)

            IF (numinpm > 0) THEN
               WHERE (push_ucat2inpm%sum_area > 0)
                  prcp_gd = prcp_gd / push_ucat2inpm%sum_area
               END WHERE
            ENDIF

            CALL compute_push_data (push_inpm2ucat, prcp_gd, prcp_uc, &
               fillvalue = 0., mode = 'sum')

            ! Convert from area-integrated [mm/s * m²] back to flux density [mm/s].
            ! push_data(mode='sum') produces area-integrated values (like rnof_uc),
            ! but the sediment yield formula expects a rate and multiplies by area internally.
            IF (numucat > 0) THEN
               WHERE (topo_area > 0._r8)
                  prcp_uc = prcp_uc / topo_area
               END WHERE
            ENDIF

            CALL sediment_forcing_put(prcp_uc, deltime)

            deallocate(prcp_pch)
            deallocate(prcp_gd)
            deallocate(prcp_uc)
         ENDIF
#endif

      ENDIF


      acctime_rnof = acctime_rnof + deltime
      IF (.not. ieee_is_finite(acctime_rnof)) THEN
         CALL CoLM_stop('Embedded CoLM river routing accumulator time overflowed.')
      ENDIF

      IF (acctime_rnof+0.01 < acctime_rnof_max) THEN
         RETURN
      ENDIF

      IF (.true.) THEN

         allocate (is_built_resv (numucat))
         allocate (wdsrf_next    (numucat))
         allocate (veloc_next    (numucat))
         allocate (hflux_fc      (numucat))
         allocate (mflux_fc      (numucat))
         allocate (zgrad_dn      (numucat))
         allocate (acc_discharge (numucat))
         allocate (sum_hflux_riv (numucat))
         allocate (sum_mflux_riv (numucat))
         allocate (sum_zgrad_riv (numucat))
         allocate (ucatfilter    (numucat))

         allocate (hflux_sumups  (numucat))
         allocate (mflux_sumups  (numucat))
         allocate (zgrad_sumups  (numucat))

         IF (DEF_Reservoir_Method > 0) THEN
            allocate (hflux_resv (numucat))
            allocate (mflux_resv (numucat))
         ENDIF

         allocate (dt_res (max(1,numrivsys)))
         allocate (dt_all (max(1,numrivsys)))

         hflux_fc(:) = 0._r8
         mflux_fc(:) = 0._r8
         zgrad_dn(:) = 0._r8
         acc_discharge(:) = 0._r8
         sum_hflux_riv(:) = 0._r8
         sum_mflux_riv(:) = 0._r8
         sum_zgrad_riv(:) = 0._r8
         hflux_sumups(:) = 0._r8
         mflux_sumups(:) = 0._r8
         zgrad_sumups(:) = 0._r8
         IF (allocated(hflux_resv)) hflux_resv(:) = 0._r8
         IF (allocated(mflux_resv)) mflux_resv(:) = 0._r8

#ifdef CoLMDEBUG
         totalrnof = sum(acc_rnof_uc)
         totalvol_bef = 0.
#endif

         DO i = 1, numucat

            is_built_resv(i) = .false.
            IF (lake_type(i) == 2) THEN
               irsv = ucat2resv(i)
               IF (year >= dam_build_year(irsv)) THEN
                  is_built_resv(i) = .true.
                  IF (.not. ieee_is_finite(volresv(irsv))) THEN
                     CALL CoLM_stop('Embedded CoLM river routing received a non-finite reservoir volume.')
                  ELSEIF (abs(volresv(irsv)) >= 0.5_r8 * abs(spval)) THEN
                     volresv(irsv) = floodplain_curve(i)%volume (wdsrf_ucat(i))
                  ELSE
                     wdsrf_ucat(i) = floodplain_curve(i)%depth (volresv(irsv))
                  ENDIF
               ENDIF
            ENDIF

            IF (.not. is_built_resv(i)) THEN
               momen_riv(i) = wdsrf_ucat(i) * veloc_riv(i)
               volwater = floodplain_curve(i)%volume (wdsrf_ucat(i))
            ELSE
               ! water in reservoirs is assumued to be stationary.
               momen_riv(i) = 0
               veloc_riv(i) = 0
               volwater = volresv(ucat2resv(i))
            ENDIF

#ifdef CoLMDEBUG
            totalvol_bef = totalvol_bef + volwater
#endif

            volwater = volwater + acc_rnof_uc(i)

            IF (.not. is_built_resv(i)) THEN
               wdsrf_ucat(i) = floodplain_curve(i)%depth (volwater)
               IF (wdsrf_ucat(i) > RIVERMIN) THEN
                   veloc_riv(i) = momen_riv(i) / wdsrf_ucat(i)
                ELSE
                   veloc_riv(i) = 0.
               ENDIF
            ELSE
               volresv(ucat2resv(i)) = volwater
            ENDIF

         ENDDO


         ntimestep = 0
#ifdef CoLMDEBUG
         totaldis  = 0.
#endif

         dt_res(:) = 0._r8
         DO i = 1, numucat
            dt_res(irivsys(i)) = acctime_rnof
         ENDDO

         global_dt_remaining(1) = maxval(dt_res)
#ifdef MPAS_MPI
         CALL mpi_allreduce (MPI_IN_PLACE, global_dt_remaining, 1, MPI_REAL8, MPI_MAX, mpas_comm, mpas_mpi_ierr)
         IF (mpas_mpi_ierr /= MPI_SUCCESS) CALL CoLM_stop('Embedded CoLM river routing failed to synchronize remaining time.')
#endif
         IF (.not. ieee_is_finite(global_dt_remaining(1)) .or. global_dt_remaining(1) < 0._r8) THEN
            CALL CoLM_stop('Embedded CoLM river routing has invalid remaining integration time.')
         ENDIF

         DO WHILE (global_dt_remaining(1) > 0._r8)

            ntimestep = ntimestep + 1

            CALL compute_push_data (push_next2ucat, wdsrf_ucat, wdsrf_next, fillvalue = spval)
            ! velocity in ocean or inland depression is assumed to be 0.
            CALL compute_push_data (push_next2ucat, veloc_riv,  veloc_next, fillvalue = 0.)

            dt_all(:) = huge(1._r8)
            WHERE (dt_res > 0._r8)
               dt_all = min(dt_res, 60._r8)
            END WHERE

            hflux_fc(:) = 0._r8
            mflux_fc(:) = 0._r8
            zgrad_dn(:) = 0._r8
            sum_hflux_riv(:) = 0._r8
            sum_mflux_riv(:) = 0._r8
            sum_zgrad_riv(:) = 0._r8
            IF (allocated(hflux_resv)) hflux_resv(:) = 0._r8
            IF (allocated(mflux_resv)) mflux_resv(:) = 0._r8

            DO i = 1, numucat

               ucatfilter(i) = dt_res(irivsys(i)) > 0._r8

               IF (.not. ucatfilter(i)) CYCLE

               ! reservoir
               IF (is_built_resv(i)) THEN
                  hflux_fc(i) = 0.
                  mflux_fc(i) = 0.
                  zgrad_dn(i) = 0.
                  CYCLE
               ENDIF

               IF ((ucat_next(i) > 0) .or. (ucat_next(i) == -9)) THEN

                  IF (ucat_next(i) > 0) THEN
                     ! both rivers are dry.
                     IF ((wdsrf_ucat(i) < RIVERMIN) .and. (wdsrf_next(i) < RIVERMIN)) THEN
                        hflux_fc(i) = 0
                        mflux_fc(i) = 0
                        zgrad_dn(i) = 0
                        CYCLE
                     ENDIF
                  ENDIF

                  ! reconstruction of height of water near interface
                  IF (ucat_next(i) > 0) THEN
                     bedelv_fc = max(topo_rivelv(i), bedelv_next(i))
                     height_up = max(0., wdsrf_ucat(i)+topo_rivelv(i)-bedelv_fc)
                     height_dn = max(0., wdsrf_next(i)+bedelv_next(i)-bedelv_fc)
                  ELSEIF (ucat_next(i) == -9) THEN ! for river mouth
                     bedelv_fc = topo_rivelv(i)
                     height_up = wdsrf_ucat (i)
                     ! sea level is assumed to be 0. and sea bed is assumed to be negative infinity.
                     height_dn = max(0., - bedelv_fc)
                  ENDIF

                  ! velocity at river downstream face (middle region in Riemann problem)
                  veloct_fc = 0.5 * (veloc_riv(i) + veloc_next(i)) &
                     + sqrt(grav * height_up) - sqrt(grav * height_dn)

                  ! height of water at downstream face (middle region in Riemann problem)
                  height_fc = 1/grav * (0.5*(sqrt(grav*height_up) + sqrt(grav*height_dn)) &
                     + 0.25 * (veloc_riv(i) - veloc_next(i))) ** 2

                  IF (height_up > 0) THEN
                     vwave_up = min(veloc_riv(i)-sqrt(grav*height_up), veloct_fc-sqrt(grav*height_fc))
                  ELSE
                     vwave_up = veloc_next(i) - 2.0 * sqrt(grav*height_dn)
                  ENDIF

                  IF (height_dn > 0) THEN
                     vwave_dn = max(veloc_next(i)+sqrt(grav*height_dn), veloct_fc+sqrt(grav*height_fc))
                  ELSE
                     vwave_dn = veloc_riv(i) + 2.0 * sqrt(grav*height_up)
                  ENDIF

                  hflux_up = veloc_riv(i)  * height_up
                  hflux_dn = veloc_next(i) * height_dn
                  mflux_up = veloc_riv(i)**2  * height_up + 0.5*grav * height_up**2
                  mflux_dn = veloc_next(i)**2 * height_dn + 0.5*grav * height_dn**2

                  IF (vwave_up >= 0.) THEN
                     hflux_fc(i) = outletwth(i) * hflux_up
                     mflux_fc(i) = outletwth(i) * mflux_up
                  ELSEIF (vwave_dn <= 0.) THEN
                     hflux_fc(i) = outletwth(i) * hflux_dn
                     mflux_fc(i) = outletwth(i) * mflux_dn
                  ELSE
                     hflux_fc(i) = outletwth(i) * (vwave_dn*hflux_up - vwave_up*hflux_dn &
                        + vwave_up*vwave_dn*(height_dn-height_up)) / (vwave_dn-vwave_up)
                     mflux_fc(i) = outletwth(i) * (vwave_dn*mflux_up - vwave_up*mflux_dn &
                        + vwave_up*vwave_dn*(hflux_dn-hflux_up)) / (vwave_dn-vwave_up)
                  ENDIF

                  sum_zgrad_riv(i) = sum_zgrad_riv(i) + outletwth(i) * 0.5*grav * height_up**2

                  zgrad_dn(i) = outletwth(i) * 0.5*grav * height_dn**2

               ELSEIF (ucat_next(i) == -99) THEN
                  ! downstream is not in model region.
                  ! assume: 1. downstream river bed is equal to this river bed.
                  !         2. downstream water surface is equal to this river depth.
                  !         3. downstream water velocity is equal to this velocity.

                  veloc_riv(i) = max(veloc_riv(i), 0.)

                  IF (wdsrf_ucat(i) > topo_rivhgt(i)) THEN

                     ! reconstruction of height of water near interface
                     height_up = wdsrf_ucat (i)
                     height_dn = topo_rivhgt(i)

                     veloct_fc = veloc_riv(i) + sqrt(grav * height_up) - sqrt(grav * height_dn)
                     height_fc = 1/grav * (0.5*(sqrt(grav*height_up) + sqrt(grav*height_dn))) ** 2

                     vwave_up = min(veloc_riv(i)-sqrt(grav*height_up), veloct_fc-sqrt(grav*height_fc))
                     vwave_dn = max(veloc_riv(i)+sqrt(grav*height_dn), veloct_fc+sqrt(grav*height_fc))

                     hflux_up = veloc_riv(i) * height_up
                     hflux_dn = veloc_riv(i) * height_dn
                     mflux_up = veloc_riv(i)**2 * height_up + 0.5*grav * height_up**2
                     mflux_dn = veloc_riv(i)**2 * height_dn + 0.5*grav * height_dn**2

                     IF (vwave_up >= 0.) THEN
                        hflux_fc(i) = outletwth(i) * hflux_up
                        mflux_fc(i) = outletwth(i) * mflux_up
                     ELSEIF (vwave_dn <= 0.) THEN
                        hflux_fc(i) = outletwth(i) * hflux_dn
                        mflux_fc(i) = outletwth(i) * mflux_dn
                     ELSE
                        hflux_fc(i) = outletwth(i) * (vwave_dn*hflux_up - vwave_up*hflux_dn &
                           + vwave_up*vwave_dn*(height_dn-height_up)) / (vwave_dn-vwave_up)
                        mflux_fc(i) = outletwth(i) * (vwave_dn*mflux_up - vwave_up*mflux_dn &
                           + vwave_up*vwave_dn*(hflux_dn-hflux_up)) / (vwave_dn-vwave_up)
                     ENDIF

                     sum_zgrad_riv(i) = sum_zgrad_riv(i) + outletwth(i) * 0.5*grav * height_up**2

                  ELSE
                     hflux_fc(i) = 0
                     mflux_fc(i) = 0
                  ENDIF

               ELSEIF (ucat_next(i) == -10) THEN ! inland depression
                  hflux_fc(i) = 0
                  mflux_fc(i) = 0
               ENDIF

               sum_hflux_riv(i) = sum_hflux_riv(i) + hflux_fc(i)
               sum_mflux_riv(i) = sum_mflux_riv(i) + mflux_fc(i)

            ENDDO

            CALL compute_push_data (push_ups2ucat, hflux_fc, hflux_sumups, fillvalue = 0., mode = 'sum')
            CALL compute_push_data (push_ups2ucat, mflux_fc, mflux_sumups, fillvalue = 0., mode = 'sum')
            CALL compute_push_data (push_ups2ucat, zgrad_dn, zgrad_sumups, fillvalue = 0., mode = 'sum')

            IF (numucat > 0) THEN
               WHERE (ucatfilter)
                  sum_hflux_riv = sum_hflux_riv - hflux_sumups
                  sum_mflux_riv = sum_mflux_riv - mflux_sumups
                  sum_zgrad_riv = sum_zgrad_riv - zgrad_sumups
               END WHERE
            ENDIF

            ! reservoir operation.
            IF (DEF_Reservoir_Method > 0) THEN

               DO i = 1, numucat

                  IF ((.not. ucatfilter(i)) .or. (ucat_next(i) == -10)) CYCLE

                  hflux_resv(i) = 0.
                  mflux_resv(i) = 0.

                  IF (is_built_resv(i)) THEN

                     irsv = ucat2resv(i)
                     qresv_in(irsv) = - sum_hflux_riv(i)

                     IF (volresv(irsv) > 1.e-4 * volresv_total(irsv)) THEN
                        CALL reservoir_operation (DEF_Reservoir_Method, &
                           irsv, qresv_in(irsv), volresv(irsv), qresv_out(irsv))
                     ELSE
                        qresv_out (irsv) = 0.
                     ENDIF

                     hflux_fc(i) = qresv_out(irsv)
                     mflux_fc(i) = qresv_out(irsv) * sqrt(2*grav*wdsrf_ucat(i))

                     sum_hflux_riv(i) = sum_hflux_riv(i) + hflux_fc(i)
                     sum_mflux_riv(i) = sum_mflux_riv(i) + mflux_fc(i)

                     hflux_resv(i) = hflux_fc(i)
                     mflux_resv(i) = mflux_fc(i)
                  ENDIF

               ENDDO

               CALL compute_push_data (push_ups2ucat, hflux_resv, hflux_sumups, fillvalue = 0., mode = 'sum')
               CALL compute_push_data (push_ups2ucat, mflux_resv, mflux_sumups, fillvalue = 0., mode = 'sum')

               IF (numucat > 0) THEN
                  WHERE (ucatfilter)
                     sum_hflux_riv = sum_hflux_riv - hflux_sumups
                     sum_mflux_riv = sum_mflux_riv - mflux_sumups
                  END WHERE
               ENDIF

            ENDIF

            IF (numucat > 0) THEN
               IF (any(ucatfilter .and. (.not. ieee_is_finite(sum_hflux_riv) .or. &
                   .not. ieee_is_finite(sum_mflux_riv) .or. .not. ieee_is_finite(sum_zgrad_riv)))) THEN
                  CALL CoLM_stop('Embedded CoLM river routing produced non-finite fluxes.')
               ENDIF
            ENDIF

            DO i = 1, numucat

               IF (.not. ucatfilter(i)) CYCLE

               dt_this = dt_all(irivsys(i))

               ! constraint 1: CFL condition (only for rivers)
               IF (.not. is_built_resv(i)) THEN
                  IF ((abs(veloc_riv(i)) > 0._r8) .or. (wdsrf_ucat(i) > 0._r8)) THEN
                     dt_this = min(dt_this, topo_rivlen(i)/(abs(veloc_riv(i))+sqrt(grav*wdsrf_ucat(i)))*0.8)
                  ENDIF
               ENDIF

               ! constraint 2: Avoid negative values of water
               IF (sum_hflux_riv(i) > 0) THEN
                  IF (.not. is_built_resv(i)) THEN
                     ! for river or lake catchment
                     volwater = floodplain_curve(i)%volume (wdsrf_ucat(i))
                  ELSE
                     ! for reservoir
                     volwater = volresv(ucat2resv(i))
                  ENDIF

                  dt_this = min(dt_this, volwater / sum_hflux_riv(i))

               ENDIF

               ! constraint 3: Avoid change of flow direction (only for rivers)
               ! IF (.not. is_built_resv(i)) THEN
               !    IF ((abs(veloc_riv(i)) > 0.1) &
               !       .and. (veloc_riv(i) * (sum_mflux_riv(i)-sum_zgrad_riv(i)) > 0)) THEN
               !       dt_this = min(dt_this, &
               !          abs(momen_riv(i) * topo_rivare(i) / (sum_mflux_riv(i)-sum_zgrad_riv(i))))
               !    ENDIF
               ! ENDIF

               dt_all(irivsys(i)) = min(dt_this, dt_all(irivsys(i)))

            ENDDO

#ifdef MPAS_MPI
            CALL synchronize_river_system_min(dt_all)
#endif

            DO i = 1, numrivsys
               IF (dt_res(i) > 0._r8) THEN
                  IF (.not. ieee_is_finite(dt_all(i)) .or. dt_all(i) <= 0._r8 .or. dt_all(i) > dt_res(i)) THEN
                     CALL CoLM_stop('Embedded CoLM river routing produced an invalid integration substep.')
                  ENDIF
	                  IF (dt_all(i) < dt_res(i)) THEN
	                     IF (dt_res(i) - dt_all(i) >= dt_res(i)) THEN
	                        CALL CoLM_stop('Embedded CoLM river routing substep cannot advance floating-point time.')
	                     ENDIF
	                  ENDIF
               ENDIF
            ENDDO

            DO i = 1, numucat

               IF (.not. ucatfilter(i)) CYCLE

               IF (.not. is_built_resv(i)) THEN
                  volwater = floodplain_curve(i)%volume (wdsrf_ucat(i))
               ELSE
                  volwater = volresv(ucat2resv(i))
               ENDIF

               volwater = volwater - sum_hflux_riv(i) * dt_all(irivsys(i))
               volwater = max(volwater, 0.)

               ! for inland depression, remove excess water (to be optimized)
               IF (ucat_next(i) == -10) THEN
                  IF (volwater > topo_rivstomax(i)) THEN
                     hflux_fc(i) = (volwater - topo_rivstomax(i)) / dt_all(irivsys(i))
                     volwater = topo_rivstomax(i)
                  ENDIF
               ENDIF

               wdsrf_ucat(i) = floodplain_curve(i)%depth (volwater)

               IF (is_built_resv(i)) THEN
                  volresv(ucat2resv(i)) = volwater
               ENDIF

               IF ((.not. is_built_resv(i)) .and. (wdsrf_ucat(i) >= RIVERMIN)) THEN
                  friction = grav * topo_rivman(i)**2 / wdsrf_ucat(i)**(7.0/3.0) * abs(momen_riv(i))
                  momen_riv(i) = (momen_riv(i) &
                     - (sum_mflux_riv(i) - sum_zgrad_riv(i)) / topo_rivare(i) * dt_all(irivsys(i))) &
                     / (1 + friction * dt_all(irivsys(i)))
                  veloc_riv(i) = momen_riv(i) / wdsrf_ucat(i)
               ELSE
                  momen_riv(i) = 0
                  veloc_riv(i) = 0
               ENDIF

               ! inland depression river
               IF ((.not. is_built_resv(i)) .and. (ucat_next(i) == -10)) THEN
                  momen_riv(i) = min(0., momen_riv(i))
                  veloc_riv(i) = min(0., veloc_riv(i))
               ENDIF

               veloc_riv(i) = min(veloc_riv(i),  20.)
               veloc_riv(i) = max(veloc_riv(i), -20.)
               momen_riv(i) = wdsrf_ucat(i) * veloc_riv(i)

            ENDDO

            DO i = 1, numucat
	               IF (ucatfilter(i)) THEN
	                  acc_discharge(i) = acc_discharge(i) + hflux_fc(i) * dt_all(irivsys(i))

#ifdef CoLMDEBUG
                  IF (ucat_next(i) <= 0) THEN
                     totaldis = totaldis + hflux_fc(i)*dt_all(irivsys(i))
                  ENDIF
#endif

	               ENDIF
            ENDDO

            WHERE (dt_res > 0._r8)
               dt_res = max(0._r8, dt_res - dt_all)
            END WHERE

            global_dt_remaining(1) = maxval(dt_res)
#ifdef MPAS_MPI
            CALL mpi_allreduce (MPI_IN_PLACE, global_dt_remaining, 1, MPI_REAL8, MPI_MAX, mpas_comm, mpas_mpi_ierr)
            IF (mpas_mpi_ierr /= MPI_SUCCESS) CALL CoLM_stop('Embedded CoLM river routing failed to synchronize remaining time.')
#endif
            IF (.not. ieee_is_finite(global_dt_remaining(1)) .or. global_dt_remaining(1) < 0._r8) THEN
               CALL CoLM_stop('Embedded CoLM river routing has invalid remaining integration time.')
            ENDIF

#ifdef GridRiverLakeSediment
            IF (DEF_USE_SEDIMENT) THEN
               IF (numucat > 0) THEN
                  allocate(floodarea_sed(numucat))
                  DO i = 1, numucat
                     IF (ucatfilter(i)) THEN
                        floodarea_sed(i) = floodplain_curve(i)%floodarea(wdsrf_ucat(i))
                     ELSE
                        floodarea_sed(i) = 0._r8
                     ENDIF
                  ENDDO
               ELSE
                  allocate(floodarea_sed(0))
               ENDIF
               CALL sediment_diag_accumulate(dt_all, irivsys, ucatfilter, &
                  veloc_riv, wdsrf_ucat, hflux_fc, floodarea_sed)
               deallocate(floodarea_sed)
            ENDIF
#endif

         ENDDO

#ifdef CoLMDEBUG
         totalvol_aft = 0.
         DO i = 1, numucat
            IF (.not. is_built_resv(i)) THEN
               totalvol_aft = totalvol_aft + floodplain_curve(i)%volume (wdsrf_ucat(i))
            ELSE
               totalvol_aft = totalvol_aft + volresv(ucat2resv(i))
            ENDIF
         ENDDO
#endif
      ENDIF

#ifdef CoLMDEBUG
#ifdef MPAS_MPI
      IF (.not. .true.) ntimestep = 0
      CALL mpi_allreduce (MPI_IN_PLACE, ntimestep, 1, MPI_INTEGER, MPI_MAX, mpas_comm, mpas_mpi_ierr)

      IF (.not. .true.) totalvol_bef = 0.
      IF (.not. .true.) totalvol_aft = 0.
      IF (.not. .true.) totalrnof    = 0.
      IF (.not. .true.) totaldis     = 0.

      CALL mpi_allreduce (MPI_IN_PLACE, totalvol_bef, 1, MPI_REAL8, MPI_SUM, mpas_comm, mpas_mpi_ierr)
      CALL mpi_allreduce (MPI_IN_PLACE, totalvol_aft, 1, MPI_REAL8, MPI_SUM, mpas_comm, mpas_mpi_ierr)
      CALL mpi_allreduce (MPI_IN_PLACE, totalrnof,    1, MPI_REAL8, MPI_SUM, mpas_comm, mpas_mpi_ierr)
      CALL mpi_allreduce (MPI_IN_PLACE, totaldis,     1, MPI_REAL8, MPI_SUM, mpas_comm, mpas_mpi_ierr)
#endif
      IF (mpas_is_root) THEN
         write(*,'(/,A)') 'Checking River Routing Flow ...'
         write(*,'(A,F12.5,A)') 'River Lake Flow minimum average timestep: ', acctime_rnof/ntimestep, ' seconds'
         write(*,'(A,ES8.1,A)') 'Total water before :  ', totalvol_bef,  ' m^3'
         write(*,'(A,ES8.1,A)') 'Total runoff :        ', totalrnof, ' m^3'
         write(*,'(A,ES8.1,A)') 'Total discharge :     ', totaldis,  ' m^3'
         write(*,'(A,ES8.1,A)') 'Total water change :  ', totalvol_aft-totalvol_bef,  ' m^3'
         write(*,'(A,ES8.1,A)') 'Total water balance : ', totalvol_aft-totalvol_bef-totalrnof+totaldis,  ' m^3'
      ENDIF
#endif

#ifdef GridRiverLakeSediment
      IF (DEF_USE_SEDIMENT .and. .true.) THEN
         ! All ranks must participate (MPI point-to-point inside push_data).
         ! fldfrc is now computed inside grid_sediment_calc from per-routing-period
         ! accumulators (sed_acc_floodarea), not from history-period averages.
         CALL grid_sediment_calc(acctime_rnof)
      ENDIF
#endif

	      IF (.true.) THEN
	         IF (acctime_rnof <= 0._r8) THEN
	            CALL CoLM_stop('Embedded CoLM river routing cannot form diagnostics over a non-positive period.')
	         ENDIF
	         IF (numucat > 0) THEN
	            discharge_riv = acc_discharge / acctime_rnof
	            IF (.not. all(ieee_is_finite(discharge_riv))) THEN
	               CALL CoLM_stop('Embedded CoLM river routing produced non-finite period-mean discharge.')
	            ENDIF
	         ENDIF
	      ENDIF
	      CALL update_GridRiverLakeElementDiagnostics()

      acctime_rnof = 0.

      IF (.true.) THEN
         IF (numucat > 0) THEN
            acc_rnof_uc = 0.
         ENDIF
      ENDIF

      IF (allocated(is_built_resv)) deallocate(is_built_resv)
      IF (allocated(wdsrf_next   )) deallocate(wdsrf_next   )
      IF (allocated(veloc_next   )) deallocate(veloc_next   )
      IF (allocated(hflux_fc     )) deallocate(hflux_fc     )
      IF (allocated(mflux_fc     )) deallocate(mflux_fc     )
      IF (allocated(zgrad_dn     )) deallocate(zgrad_dn     )
      IF (allocated(acc_discharge)) deallocate(acc_discharge)
      IF (allocated(hflux_resv   )) deallocate(hflux_resv   )
      IF (allocated(mflux_resv   )) deallocate(mflux_resv   )
      IF (allocated(hflux_sumups )) deallocate(hflux_sumups )
      IF (allocated(mflux_sumups )) deallocate(mflux_sumups )
      IF (allocated(zgrad_sumups )) deallocate(zgrad_sumups )
      IF (allocated(sum_hflux_riv)) deallocate(sum_hflux_riv)
      IF (allocated(sum_mflux_riv)) deallocate(sum_mflux_riv)
      IF (allocated(sum_zgrad_riv)) deallocate(sum_zgrad_riv)
      IF (allocated(ucatfilter   )) deallocate(ucatfilter   )
      IF (allocated(dt_res       )) deallocate(dt_res       )
      IF (allocated(dt_all       )) deallocate(dt_all       )

   END SUBROUTINE grid_riverlake_flow

   ! ---------
   SUBROUTINE grid_riverlake_flow_final ()

      CALL riverlake_network_final ()

      IF (DEF_Reservoir_Method > 0) THEN
         CALL reservoir_final ()
      ENDIF

#ifdef GridRiverLakeSediment
      CALL grid_sediment_final()
#endif

      IF (allocated(filter_rnof)) deallocate(filter_rnof)

   END SUBROUTINE grid_riverlake_flow_final

END MODULE MOD_Grid_RiverLakeFlow
#endif
