#include <define.h>

MODULE MOD_SnowLayersCombineDivide

!-----------------------------------------------------------------------
   USE MOD_Precision
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: snowcompaction
   PUBLIC :: snowlayerscombine
   PUBLIC :: SnowLayersCombine_snicar
   PUBLIC :: snowlayersdivide
   PUBLIC :: SnowLayersDivide_snicar


! PRIVATE MEMBER FUNCTIONS:
   PRIVATE :: combo
   PRIVATE :: winddriftcompaction


!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------



   SUBROUTINE snowcompaction (lb,deltim,imelt,fiold,&
              t_soisno,wliq_soisno,wice_soisno,forc_us,forc_vs,dz_soisno)

!=======================================================================
!  Original author: Yongjiu Dai, September 15, 1999
!  Revision: Yongjiu Dai, /07/31/2023
!
!  Four of metamorphisms of changing snow characteristics are
!  implemented, i.e., destructive, overburden, melt and wind drift. The
!  treatments of the destructive compaction was from SNTHERM.89 and
!  SNTHERM.99 (1991, 1999). The contribution due to melt metamorphism is
!  simply taken as a ratio of snow ice fraction after the melting versus
!  before the melting. The treatments of the overburden compaction and
!  the drifting compaction were borrowed from CLM5.0 which based on
!  Vionnet et al. (2012) and van Kampenhout et al (2017).
!
!=======================================================================

   USE MOD_Precision
   USE MOD_Const_Physical, only: denice, denh2o, tfrz
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------

   integer,  intent(in) :: lb                 ! lower bound of array
   real(r8), intent(in) :: deltim             ! seconds i a time step [second]
   integer,  intent(in) :: imelt(lb:0)        ! signifies IF node in melting (imelt = 1)
   real(r8), intent(in) :: fiold(lb:0)        ! fraction of ice relative to the total water content
                                              ! at the previous time step
   real(r8), intent(in) :: t_soisno(lb:0)     ! nodal temperature [K]
   real(r8), intent(in) :: wice_soisno(lb:0)  ! ice lens [kg/m2]
   real(r8), intent(in) :: wliq_soisno(lb:0)  ! liquid water [kg/m2]
   real(r8), intent(in) :: forc_us            ! wind speed in eastward direction [m/s]
   real(r8), intent(in) :: forc_vs            ! wind speed in northward direction [m/s]

   real(r8), intent(inout) :: dz_soisno(lb:0) ! layer thickness [m]

!-------------------------- Local Variables ----------------------------
   integer j  ! Numeber of doing loop

   real(r8), parameter ::  c1 = 2.777e-7  ! [m2/(kg s)]
   real(r8), parameter ::  c2 = 23.0e-3   ! [m3/kg]
   real(r8), parameter ::  c3 = 2.777e-6  ! [1/s]
   real(r8), parameter ::  c4 = 0.04      ! [1/K]
   real(r8), parameter ::  c5 = 2.0       !
   real(r8), parameter ::  c6 = 5.15e-7   !
   real(r8), parameter ::  c7 = 4.0       !
   ! Upper Limit on Destructive Metamorphism Compaction [kg/m3]
   real(r8), parameter ::  dm = 100.0
   real(r8), parameter ::  eta0 = 9.e5    ! The Viscosity Coefficient Eta0 [kg-s/m2]

   real(r8) :: burden  ! pressure of overlying snow [kg/m2]
   real(r8) :: ddz1    ! rate of settling of snowpack due to destructive metamorphism.
   real(r8) :: ddz2    ! rate of compaction of snowpack due to overburden.
   real(r8) :: ddz3    ! rate of compaction of snowpack due to melt [1/s]
   real(r8) :: ddz4    ! rate of compaction of snowpack due to wind drift.

   real(r8) :: dexpf   ! expf=exp(-c4*(273.15-t_soisno)).
   real(r8) :: fi      ! fraction of ice relative to the total water content at current time step
   real(r8) :: td      ! t_soisno - tfrz [K]
   real(r8) :: pdzdtc  ! nodal rate of change in fractional-thickness due to compaction [fraction/s]
   real(r8) :: void    ! void (1 - vol_ice - vol_liq)
   real(r8) :: wx      ! water mass (ice+liquid) [kg/m2]
   real(r8) :: bi      ! partial density of ice [kg/m3]

   real(r8) :: zpseudo ! wind drift compaction / pseudo depth
                       ! (only valid IF wind_dependent_snow_density is .true.)
   logical  :: mobile  ! current snow layer is mobile, i.e. susceptible to wind drift
                       ! (only valid IF wind_dependent_snow_density is .true.)
   real(r8) :: f1, f2, eta, forc_wind

!-----------------------------------------------------------------------
      ! Begin calculation - note that the following column loops are only invoked IF lb < 0

      burden = 0.0
      zpseudo = 0.0
      mobile  = .true.

      DO j = lb, 0
         wx = wice_soisno(j) + wliq_soisno(j)
         void = 1.0-(wice_soisno(j)/denice + wliq_soisno(j)/denh2o)/dz_soisno(j)

! Disallow compaction for water saturated node and lower ice lens node.
         IF(void <= 0.001 .or. wice_soisno(j) <= .1)THEN
            burden = burden+wx

            ! saturated node is immobile
            ! This is only needed IF wind_dependent_snow_density is true, but it's
            ! simplest just to update mobile always
            mobile = .false.

            CYCLE
         ENDIF

         bi = wice_soisno(j) / dz_soisno(j)
         fi = wice_soisno(j) / wx
         td = tfrz-t_soisno(j)

         dexpf = exp(-c4*td)

! Compaction due to destructive metamorphism
         ddz1 = -c3*dexpf
         IF(bi > dm) ddz1 = ddz1*exp(-46.0e-3*(bi-dm))

! Liquid water term
         IF(wliq_soisno(j) > 0.01*dz_soisno(j)) ddz1=ddz1*c5

! Compaction due to overburden
!*       ddz2 = -burden*exp(-0.08*td-c2*bi)/eta0
         f1 = 1.0/(1.0+60.0*wliq_soisno(j)/(denh2o*dz_soisno(j)))
         f2 = 4.0 ! currently fixed to maximum value, holds in absence of angular grains
         eta = f1*f2*(bi/450.0)*exp(0.1*td + c2*bi)*7.62237e6
         ddz2 = -(burden+wx/2.0) / eta

! Compaction occurring during melt
         IF(imelt(j) == 1)THEN
            ddz3 = - 1.0/deltim * max(0.0,(fiold(j) - fi)/fiold(j))
         ELSE
            ddz3 = 0.0
         ENDIF

! Compaction occurring due to wind drift
         forc_wind = sqrt(forc_us**2+forc_vs**2)
         CALL winddriftcompaction( bi,forc_wind,dz_soisno(j),zpseudo,mobile,ddz4 )

! Time rate of fractional change in dz (units of s-1)
         pdzdtc = ddz1 + ddz2 + ddz3 + ddz4

! The change in dz_soisno due to compaction
! Limit compaction to be no greater than fully saturated layer thickness
         dz_soisno(j) = dz_soisno(j)*(1.0+pdzdtc*deltim)
         dz_soisno(j) = max(dz_soisno(j),(wice_soisno(j)/denice+ wliq_soisno(j)/denh2o))

! Pressure of overlying snow
         burden = burden+wx

      ENDDO

   END SUBROUTINE snowcompaction



   SUBROUTINE winddriftcompaction(bi,forc_wind,dz,zpseudo,mobile,compaction_rate)

!=======================================================================
!  Original author: Yongjiu Dai, September 15, 1999
!
!  Compute wind drift compaction for a single column and level.  Also
!  updates zpseudo and mobile for this column. However, zpseudo remains
!  unchanged IF mobile is already false or becomes false within this
!  SUBROUTINE.
!
!  The structure of the updates done here for zpseudo and mobile
!  requires that this SUBROUTINE be called first for the top layer of
!  snow, THEN for the 2nd layer down, etc. - and finally for the bottom
!  layer. Before beginning the loops over layers, mobile should be
!  initialized to .true. and zpseudo should be initialized to 0.
!
! !REVISIONS: Yongjiu Dai, /07/31/2023
!
! !USES:
   USE MOD_Precision
   !
   ! !ARGUMENTS:
   real(r8) , intent(in)    :: bi              ! partial density of ice [kg/m3]
   real(r8) , intent(in)    :: forc_wind       ! atmospheric wind speed [m/s]
   real(r8) , intent(in)    :: dz              ! layer depth for this column and level [m]
   ! wind drift compaction / pseudo depth for this column at this layer
   real(r8) , intent(inout) :: zpseudo
   ! whether this snow column is still mobile at this layer (i.e., susceptible to wind drift)
   logical  , intent(inout) :: mobile
   ! rate of compaction of snowpack due to wind drift, for the current column and layer
   real(r8) , intent(out)   :: compaction_rate
   !
   ! !LOCAL VARIABLES:
   real(r8) :: Frho        ! Mobility density factor [-]
   real(r8) :: MO          ! Mobility index [-]
   real(r8) :: SI          ! Driftability index [-]
   real(r8) :: gamma_drift ! Scaling factor for wind drift time scale [-]
   real(r8) :: tau_inverse ! Inverse of the effective time scale [1/s]

   real(r8), parameter :: rho_min = 50._r8      ! wind drift compaction / minimum density [kg/m3]
   real(r8), parameter :: rho_max = 350._r8     ! wind drift compaction / maximum density [kg/m3]
   ! wind drift compaction / grain size (fixed value for now)
   real(r8), parameter :: drift_gs = 0.35e-3_r8
   real(r8), parameter :: drift_sph = 1.0_r8    ! wind drift compaction / sphericity
   real(r8), parameter :: tau_ref = 48._r8 * 3600._r8  ! wind drift compaction / reference time [s]

   !-----------------------------------------------------------------------

      IF (mobile) THEN
         Frho = 1.25_r8 - 0.0042_r8*(max(rho_min, bi)-rho_min)
         ! assuming dendricity = 0, sphericity = 1, grain size = 0.35 mm Non-dendritic snow
         MO = 0.34_r8 * (-0.583_r8*drift_gs - 0.833_r8*drift_sph + 0.833_r8) + 0.66_r8*Frho
         SI = -2.868_r8 * exp(-0.085_r8*forc_wind) + 1._r8 + MO

         IF (SI > 0.0_r8) THEN
            SI = min(SI, 3.25_r8)
            ! Increase zpseudo (wind drift / pseudo depth) to the middle of
            ! the pseudo-node for the sake of the following calculation
            zpseudo = zpseudo + 0.5_r8 * dz * (3.25_r8 - SI)
            gamma_drift = SI*exp(-zpseudo/0.1_r8)
            tau_inverse = gamma_drift / tau_ref
            compaction_rate = -max(0.0_r8, rho_max-bi) * tau_inverse
            ! Further increase zpseudo to the bottom of the pseudo-node for
            ! the sake of calculations done on the underlying layer (i.e.,
            ! the next time through the j loop).
            zpseudo = zpseudo + 0.5_r8 * dz * (3.25_r8 - SI)
         ELSE  ! SI <= 0
            mobile = .false.
            compaction_rate = 0._r8
         ENDIF
      ELSE  ! .not. mobile
         compaction_rate = 0._r8
      ENDIF

   END SUBROUTINE winddriftcompaction



   !-----------------------------------------------------------------------
   SUBROUTINE snowlayerscombine (lb,snl, &
              z_soisno,dz_soisno,zi_soisno,wliq_soisno,wice_soisno,t_soisno,scv,snowdp)

!=======================================================================
!  Original author: Yongjiu Dai, September 15, 1999
!
!  checks for elements which are below prescribed minimum for thickness
!  or mass.  If snow element thickness or mass is less than a prescribed
!  minimum, it is combined with neighboring element to be best combine
!  with, and executes the combination of mass and energy in
!  clm_combo.f90
!
!=======================================================================

   USE MOD_Precision
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer, intent(in) :: lb                      ! lower bound of array

! numbering from 1 (bottom) mss (surface)
   real(r8), intent(inout) :: wice_soisno(lb:1)   ! ice lens [kg/m2]
   real(r8), intent(inout) :: wliq_soisno(lb:1)   ! liquid water {kg/m2]
   real(r8), intent(inout) :: t_soisno (lb:1)     ! node temperature [K]
   real(r8), intent(inout) :: dz_soisno  (lb:1)   ! layer thickness [m]
   real(r8), intent(inout) :: z_soisno   (lb:1)   ! node depth [m]
   real(r8), intent(inout) :: zi_soisno  (lb-1:1) ! depth of layer interface [m]
   real(r8), intent(inout) :: snowdp              ! snow depth [m]
   real(r8), intent(inout) :: scv                 ! snow mass - water equivalent [kg/m2]
   integer, intent(inout) :: snl                  ! Number of snow

!-------------------------- Local Variables ----------------------------
   real(r8) :: drr           ! thickness of the combined [m]
   real(r8) :: dzmin(5)      ! minimum of snow layer 1 (top) to msn0 (bottom)
   real(r8) :: zwice         ! total ice mass in snow
   real(r8) :: zwliq         ! total liquid water in snow

   integer :: i              ! number of DO looping
   integer :: j              ! node index
   integer :: k              ! number of DO looping
   integer :: l              ! node index
   integer :: msn_old        ! number of snow layer 1 (top) to msn0 (bottom)
   integer :: mssi           ! node index
   integer :: neibor         ! adjacent node selected for combination

   data dzmin /0.010, 0.015, 0.025, 0.055, 0.115/

!-----------------------------------------------------------------------
! check the mass of ice lens of snow, when the total less than a small value,
! combine it with the underlying neighbor
      msn_old = snl
      DO j = msn_old+1, 0
         IF(wice_soisno(j) <= .1)THEN
            wliq_soisno(j+1) = wliq_soisno(j+1) + wliq_soisno(j)
            wice_soisno(j+1) = wice_soisno(j+1) + wice_soisno(j)

! shift all elements above this down one.
            IF(j > snl+1 .and. snl < -1)THEN
               DO i =  j, snl+2, -1
                  t_soisno(i) = t_soisno(i-1)
                  wliq_soisno(i) = wliq_soisno(i-1)
                  wice_soisno(i) = wice_soisno(i-1)
                  dz_soisno(i) = dz_soisno(i-1)
               ENDDO
            ENDIF

            snl = snl + 1
!*          write(6,*) 'one snow layer is gone'

         ENDIF

      ENDDO

      IF(snl == 0)THEN
         scv = 0.
         snowdp = 0.
!*       write(6,*) 'all snow has gone'
         RETURN
      ELSE
         scv = 0.
         snowdp = 0.
         zwice = 0.
         zwliq = 0.
         DO j = snl + 1, 0
            scv = scv + wice_soisno(j) + wliq_soisno(j)
            snowdp = snowdp + dz_soisno(j)
            zwice = zwice + wice_soisno(j)
            zwliq = zwliq + wliq_soisno(j)
         ENDDO
      ENDIF
!-----------------------------------------------------------------------
! check the snow depth

      IF(snowdp < 0.01)THEN       !!! all snow gone

         snl = 0
         scv = zwice
         IF(scv <= 0.) snowdp = 0.

! the liquid water assumed ponding on soil surface
         wliq_soisno(1) = wliq_soisno(1) + zwliq
!*       write(6,'(17h all snow is gone)')
         RETURN

      ELSE                        !!! snow layers combined

! two or more layers

         IF(snl < -1)THEN
            msn_old = snl
            mssi = 1
            DO i = msn_old+1, 0

! If top node is removed, combine with bottom neighbor
               IF(dz_soisno(i) < dzmin(mssi))THEN
                  IF(i == snl+1)THEN
                     neibor = i + 1

! If the bottom neighbor is not snow, combine with the top neighbor
                  ELSEIF(i == 0)THEN
                     neibor = i - 1

! If NONE of the above special cases apply, combine with the thinnest neighbor
                  ELSE
                     neibor = i + 1
                     IF((dz_soisno(i-1)+dz_soisno(i)) < (dz_soisno(i+1)+dz_soisno(i))) neibor = i-1
                  ENDIF

! Node l and j are combined and stored as node j.

                  IF(neibor > i)THEN
                     j = neibor
                     l = i
                  ELSE
                     j = i
                     l = neibor
                  ENDIF
                  CALL combo ( dz_soisno(j), wliq_soisno(j), wice_soisno(j), t_soisno(j),&
                               dz_soisno(l), wliq_soisno(l), wice_soisno(l), t_soisno(l) )

! Now shift all elements above this down one.

                  IF(j-1 > snl+1) THEN
                     DO k = j-1, snl+2, -1
                        t_soisno(k) = t_soisno(k-1)
                        wice_soisno(k) = wice_soisno(k-1)
                        wliq_soisno(k) = wliq_soisno(k-1)
                        dz_soisno(k) = dz_soisno(k-1)
                     ENDDO
                  ENDIF

                  snl = snl + 1

!*    write(6,'(7h Nodes ,i4,4h and,i4,14h combined into,i4)') l,j,j

                  IF(snl >= -1) EXIT

! The layer thickness great than the prescribed minimum value

               ELSE
                  mssi = mssi + 1
               ENDIF
            ENDDO

         ENDIF

! Reset the node depth and the depth of layer interface

         zi_soisno(0) = 0.
         DO k = 0, snl+1, -1
            z_soisno(k) = zi_soisno(k) - 0.5*dz_soisno(k)
            zi_soisno(k-1) = zi_soisno(k) - dz_soisno(k)
         ENDDO

      ENDIF                       !!! snow layers combined

   END SUBROUTINE snowlayerscombine



   SUBROUTINE snowlayersdivide(lb,snl,z_soisno,dz_soisno,zi_soisno,wliq_soisno,wice_soisno,t_soisno)

!=======================================================================
!  Original author: Yongjiu Dai, September 15, 1999
!
!  subdivides snow layer when its thickness exceed the prescribed maximum
!=======================================================================

   USE MOD_Precision
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------

    integer, intent(in)    :: lb                  ! lower bound of array
    integer, intent(inout) :: snl                 ! Number of snow
   real(r8), intent(inout) :: wice_soisno(lb:0)   ! ice lens [kg/m2]
   real(r8), intent(inout) :: wliq_soisno(lb:0)   ! liquid water [kg/m2]
   real(r8), intent(inout) :: t_soisno   (lb:0)   ! Node temperature [K]
   real(r8), intent(inout) :: dz_soisno  (lb:0)   ! Layer thickness [m]
   real(r8), intent(inout) :: z_soisno   (lb:0)   ! Node depth [m]
   real(r8), intent(inout) :: zi_soisno  (lb-1:0) ! Depth of layer interface [m]

!-------------------------- Local Variables ----------------------------

! numbering from 1 (surface) msno (bottom)
   real(r8) :: drr      ! thickness of the combined [m]
   real(r8) :: dzsno(5) ! Snow layer thickness [m]
   real(r8) :: swice(5) ! Partial volume of ice [m3/m3]
   real(r8) :: swliq(5) ! Partial volume of liquid water [m3/m3]
   real(r8) :: tsno(5)  ! Nodel temperature [K]

   integer k            ! number of DO looping
   integer msno         ! number of snow layer 1 (top) to msno (bottom)

   real(r8) zwice,zwliq,propor

!-----------------------------------------------------------------------

      msno = abs(snl)
      DO k = 1, msno
         dzsno(k) = dz_soisno  (k + snl)
         swice(k) = wice_soisno(k + snl)
         swliq(k) = wliq_soisno(k + snl)
         tsno(k)  = t_soisno (k + snl)
      ENDDO

      IF(msno == 1)THEN
         IF(dzsno(1) > 0.03)THEN
         msno = 2
! Specified a new snow layer
         dzsno(1) = dzsno(1)/2.
         swice(1) = swice(1)/2.
         swliq(1) = swliq(1)/2.

         dzsno(2) = dzsno(1)
         swice(2) = swice(1)
         swliq(2) = swliq(1)
         tsno(2)  = tsno(1)
!        write(6,*)'Subdivided Top Node into two layer (1/2)'
         ENDIF
      ENDIF

      IF(msno > 1)THEN
         IF(dzsno(1) > 0.02)THEN
         drr = dzsno(1) - 0.02
         propor = drr/dzsno(1)
         zwice = propor*swice(1)
         zwliq = propor*swliq(1)

         propor = 0.02/dzsno(1)
         swice(1) = propor*swice(1)
         swliq(1) = propor*swliq(1)
         dzsno(1) = 0.02

         CALL combo(dzsno(2),swliq(2),swice(2),tsno(2), &
                    drr,zwliq,zwice,tsno(1))

!        write(6,*) 'Subdivided Top Node &
!                    20 mm combined into underlying neighbor'

         IF(msno <= 2 .and. dzsno(2) > 0.07)THEN
! subdivided a new layer
            msno = 3
            dzsno(2) = dzsno(2)/2.
            swice(2) = swice(2)/2.
            swliq(2) = swliq(2)/2.

            dzsno(3) = dzsno(2)
            swice(3) = swice(2)
            swliq(3) = swliq(2)
            tsno(3)  = tsno(2)
         ENDIF
         ENDIF
      ENDIF

      IF(msno > 2)THEN
         IF(dzsno(2) > 0.05)THEN
         drr = dzsno(2) - 0.05
         propor = drr/dzsno(2)
         zwice = propor*swice(2)
         zwliq = propor*swliq(2)

         propor = 0.05/dzsno(2)
         swice(2) = propor*swice(2)
         swliq(2) = propor*swliq(2)
         dzsno(2) = 0.05

         CALL combo(dzsno(3),swliq(3),swice(3),tsno(3), &
                    drr,     zwliq,   zwice,   tsno(2))

!        write(6,*)'Subdivided 50 mm from the subsurface layer &
!                   &and combined into underlying neighbor'

         IF(msno <= 3 .and. dzsno(3) > 0.18)THEN
! subdivided a new layer
            msno =  4
            dzsno(3) = dzsno(3)/2.
            swice(3) = swice(3)/2.
            swliq(3) = swliq(3)/2.

            dzsno(4) = dzsno(3)
            swice(4) = swice(3)
            swliq(4) = swliq(3)
            tsno(4)  = tsno(3)
         ENDIF
         ENDIF
      ENDIF

      IF(msno > 3)THEN
         IF(dzsno(3) > 0.11)THEN
         drr = dzsno(3) - 0.11
         propor = drr/dzsno(3)
         zwice = propor*swice(3)
         zwliq = propor*swliq(3)

         propor = 0.11/dzsno(3)
         swice(3) = propor*swice(3)
         swliq(3) = propor*swliq(3)
         dzsno(3) = 0.11

         CALL combo(dzsno(4),swliq(4),swice(4),tsno(4), &
                    drr,     zwliq,   zwice,   tsno(3))

!        write(6,*)'Subdivided 110 mm from the third Node &
!                   &and combined into underlying neighbor'

         IF(msno <= 4 .and. dzsno(4) > 0.41)THEN
! subdivided a new layer
            msno = 5
            dzsno(4) = dzsno(4)/2.
            swice(4) = swice(4)/2.
            swliq(4) = swliq(4)/2.

            dzsno(5) = dzsno(4)
            swice(5) = swice(4)
            swliq(5) = swliq(4)
            tsno(5)  = tsno(4)
         ENDIF
         ENDIF
      ENDIF

      IF(msno > 4)THEN
         IF(dzsno(4) > 0.23)THEN
         drr = dzsno(4) - 0.23
         propor = drr/dzsno(4)
         zwice = propor*swice(4)
         zwliq = propor*swliq(4)

         propor = 0.23/dzsno(4)
         swice(4) = propor*swice(4)
         swliq(4) = propor*swliq(4)
         dzsno(4) = 0.23

         CALL combo(dzsno(5),swliq(5),swice(5),tsno(5), &
                    drr,     zwliq,   zwice,   tsno(4))

!        write(6,*)'Subdivided 230 mm from the fourth Node &
!                   'and combined into underlying neighbor'
         ENDIF
      ENDIF

      snl = - msno

      DO k = snl+1, 0
         dz_soisno(k)   = dzsno(k - snl)
         wice_soisno(k) = swice(k - snl)
         wliq_soisno(k) = swliq(k - snl)
         t_soisno(k)  = tsno (k - snl)
      ENDDO

      zi_soisno(0) = 0.
      DO k = 0, snl+1, -1
         z_soisno(k)    = zi_soisno(k) - 0.5*dz_soisno(k)
         zi_soisno(k-1) = zi_soisno(k) - dz_soisno(k)
      ENDDO

   END SUBROUTINE snowlayersdivide



   SUBROUTINE combo ( dz_soisno, wliq_soisno, wice_soisno, t, &
                     dz2, wliq2, wice2, t2 )

!=======================================================================
!  Original author: Yongjiu Dai, September 15, 1999
!
!  combines two elements and returns the following combined
!  variabless: dz_soisno, t, wliq_soisno, wice_soisno.
!  the combined temperature is based on the equation:
!  the sum of the enthalpies of the two elements = that of the combined element.
!
!=======================================================================

   USE MOD_Precision
   USE MOD_Const_Physical, only: cpice, cpliq, hfus, tfrz
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------

   real(r8), intent(in) :: dz2     ! nodal thickness of 2 elements being combined [m]
   real(r8), intent(in) :: wliq2   ! liquid water of element 2 [kg/m2]
   real(r8), intent(in) :: wice2   ! ice of element 2 [kg/m2]
   real(r8), intent(in) :: t2      ! nodal temperature of element 2 [K]

   real(r8), intent(inout) :: dz_soisno   ! nodal thickness of 1 elements being combined [m]
   real(r8), intent(inout) :: wliq_soisno ! liquid water of element 1
   real(r8), intent(inout) :: wice_soisno ! ice of element 1 [kg/m2]
   real(r8), intent(inout) :: t           ! node temperature of elment 1 [K]

!-------------------------- Local Variables ----------------------------

   real(r8) dzc    ! Total thickness of nodes 1 and 2 (dzc=dz_soisno+dz2).
   real(r8) wliqc  ! Combined liquid water [kg/m2]
   real(r8) wicec  ! Combined ice [kg/m2]
   real(r8) tc     ! Combined node temperature [K]
   real(r8) h      ! enthalpy of element 1 [J/m2]
   real(r8) h2     ! enthalpy of element 2 [J/m2]
   real(r8) hc     ! temporary

!-----------------------------------------------------------------------

      dzc = dz_soisno+dz2
      wicec = (wice_soisno+wice2)
      wliqc = (wliq_soisno+wliq2)
      h   = (cpice*wice_soisno+cpliq*wliq_soisno)*(t-tfrz)+hfus*wliq_soisno
      h2  = (cpice*wice2+cpliq*wliq2)*(t2-tfrz)+hfus*wliq2

      hc = h + h2
      IF(hc < 0.)THEN
         tc = tfrz + hc/(cpice*wicec+cpliq*wliqc)
      ELSEIF(hc.le.hfus*wliqc)THEN
         tc = tfrz
      ELSE
         tc = tfrz + (hc - hfus*wliqc)/(cpice*wicec+cpliq*wliqc)
      ENDIF

      dz_soisno = dzc
      wice_soisno = wicec
      wliq_soisno = wliqc
      t = tc

   END SUBROUTINE combo


   SUBROUTINE SnowLayersCombine_snicar (lb,snl, &
              z_soisno,dz_soisno,zi_soisno,wliq_soisno,wice_soisno,t_soisno,scv,snowdp,&

! Aerosol Fluxes (Jan. 07, 2023)
              mss_bcpho, mss_bcphi, mss_ocpho, mss_ocphi,&
              mss_dst1 , mss_dst2 , mss_dst3 , mss_dst4 )
! Aerosol Fluxes (Jan. 07, 2023)


!=======================================================================
!  Original author: Yongjiu Dai, September 15, 1999; January 07, 2023
!
!  checks for elements which are below prescribed minimum for thickness or mass.
!  If snow element thickness or mass is less than a prescribed minimum,
!  it is combined with neighboring element to be best combine with,
!  and executes the combination of mass and energy in clm_combo.f90
!
! !REVISIONS:
!  Yongjiu Dai, 01/2023: added Aerosol fluxes from SNICAR model
!=======================================================================

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer, intent(in) :: lb                      ! lower bound of array

! numbering from 1 (bottom) mss (surface)
   real(r8), intent(inout) :: wice_soisno(lb:1)   ! ice lens [kg/m2]
   real(r8), intent(inout) :: wliq_soisno(lb:1)   ! liquid water {kg/m2]
   real(r8), intent(inout) :: t_soisno   (lb:1)   ! node temperature [K]
   real(r8), intent(inout) :: dz_soisno  (lb:1)   ! layer thickness [m]
   real(r8), intent(inout) :: z_soisno   (lb:1)   ! node depth [m]
   real(r8), intent(inout) :: zi_soisno  (lb-1:1) ! depth of layer interface [m]
   real(r8), intent(inout) :: snowdp  ! snow depth [m]
   real(r8), intent(inout) :: scv     ! snow mass - water equivalent [kg/m2]
   integer,  intent(inout) :: snl     ! Number of snow

! Aerosol Fluxes (Jan. 07, 2023)
   real(r8), intent(inout) :: &
        mss_bcpho (lb:0), &! mass of hydrophobic BC in snow  (col,lyr) [kg]
        mss_bcphi (lb:0), &! mass of hydrophillic BC in snow (col,lyr) [kg]
        mss_ocpho (lb:0), &! mass of hydrophobic OC in snow  (col,lyr) [kg]
        mss_ocphi (lb:0), &! mass of hydrophillic OC in snow (col,lyr) [kg]
        mss_dst1  (lb:0), &! mass of dust species 1 in snow  (col,lyr) [kg]
        mss_dst2  (lb:0), &! mass of dust species 2 in snow  (col,lyr) [kg]
        mss_dst3  (lb:0), &! mass of dust species 3 in snow  (col,lyr) [kg]
        mss_dst4  (lb:0)   ! mass of dust species 4 in snow  (col,lyr) [kg]
! Aerosol Fluxes (Jan. 07, 2023)

!-------------------------- Local Variables ----------------------------
   real(r8) :: drr          ! thickness of the combined [m]
   real(r8) :: dzmin(5)     ! minimum of snow layer 1 (top) to msn0 (bottom)
   real(r8) :: zwice        ! total ice mass in snow
   real(r8) :: zwliq        ! total liquid water in snow

   integer :: i             ! number of DO looping
   integer :: j             ! node index
   integer :: k             ! number of DO looping
   integer :: l             ! node index
   integer :: msn_old       ! number of snow layer 1 (top) to msn0 (bottom)
   integer :: mssi          ! node index
   integer :: neibor        ! adjacent node selected for combination

   data dzmin /0.010, 0.015, 0.025, 0.055, 0.115/

!-----------------------------------------------------------------------
! check the mass of ice lens of snow, when the total less than a small value,
! combine it with the underlying neighbor
      msn_old = snl
      DO j = msn_old+1, 0
         IF(wice_soisno(j) <= .1)THEN
            wliq_soisno(j+1) = wliq_soisno(j+1) + wliq_soisno(j)
            wice_soisno(j+1) = wice_soisno(j+1) + wice_soisno(j)

!Aerosol Fluxes (January 07, 2023)
            IF (j < 0) THEN  ! 01/11/2023, yuan: add j < 0
               mss_bcphi(j+1) = mss_bcphi(j+1)  + mss_bcphi(j)
               mss_bcpho(j+1) = mss_bcpho(j+1)  + mss_bcpho(j)
               mss_ocphi(j+1) = mss_ocphi(j+1)  + mss_ocphi(j)
               mss_ocpho(j+1) = mss_ocpho(j+1)  + mss_ocpho(j)
               mss_dst1 (j+1) = mss_dst1 (j+1)  + mss_dst1 (j)
               mss_dst2 (j+1) = mss_dst2 (j+1)  + mss_dst2 (j)
               mss_dst3 (j+1) = mss_dst3 (j+1)  + mss_dst3 (j)
               mss_dst4 (j+1) = mss_dst4 (j+1)  + mss_dst4 (j)
            ENDIF
!Aerosol Fluxes (January 07, 2023)


! shift all elements above this down one.
            IF(j > snl+1 .and. snl < -1)THEN
               DO i =  j, snl+2, -1
                  t_soisno(i) = t_soisno(i-1)
                  wliq_soisno(i) = wliq_soisno(i-1)
                  wice_soisno(i) = wice_soisno(i-1)
                  dz_soisno(i) = dz_soisno(i-1)

!Aerosol Fluxes (January 07, 2023)
                  mss_bcphi(i) = mss_bcphi(i-1)
                  mss_bcpho(i) = mss_bcpho(i-1)
                  mss_ocphi(i) = mss_ocphi(i-1)
                  mss_ocpho(i) = mss_ocpho(i-1)
                  mss_dst1 (i) = mss_dst1 (i-1)
                  mss_dst2 (i) = mss_dst2 (i-1)
                  mss_dst3 (i) = mss_dst3 (i-1)
                  mss_dst4 (i) = mss_dst4 (i-1)
!Aerosol Fluxes (January 07, 2023)
               ENDDO
            ENDIF

            snl = snl + 1
!*          write(6,*) 'one snow layer is gone'

         ENDIF

      ENDDO

      IF(snl == 0)THEN
         scv = 0._r8
         snowdp = 0._r8

!Aerosol Fluxes (January 07, 2023)
         mss_bcphi(:) = 0._r8
         mss_bcpho(:) = 0._r8
         mss_ocphi(:) = 0._r8
         mss_ocpho(:) = 0._r8
         mss_dst1 (:) = 0._r8
         mss_dst2 (:) = 0._r8
         mss_dst3 (:) = 0._r8
         mss_dst4 (:) = 0._r8
!Aerosol Fluxes (January 07, 2023)

!*       write(6,*) 'all snow has gone'
         RETURN
      ELSE
         scv = 0._r8
         snowdp = 0._r8
         zwice = 0._r8
         zwliq = 0._r8
         DO j = snl + 1, 0
            scv = scv + wice_soisno(j) + wliq_soisno(j)
            snowdp = snowdp + dz_soisno(j)
            zwice = zwice + wice_soisno(j)
            zwliq = zwliq + wliq_soisno(j)
         ENDDO
      ENDIF
!-----------------------------------------------------------------------
! check the snow depth

      IF(snowdp < 0.01_r8)THEN       !!! all snow gone

         snl = 0
         scv = zwice
         IF(scv <= 0._r8) snowdp = 0._r8

!Aerosol Fluxes (January 07, 2023)
         mss_bcphi(:) = 0._r8
         mss_bcpho(:) = 0._r8
         mss_ocphi(:) = 0._r8
         mss_ocpho(:) = 0._r8
         mss_dst1 (:) = 0._r8
         mss_dst2 (:) = 0._r8
         mss_dst3 (:) = 0._r8
         mss_dst4 (:) = 0._r8
!Aerosol Fluxes (January 07, 2023)

! the liquid water assumed ponding on soil surface
         wliq_soisno(1) = wliq_soisno(1) + zwliq
!*       write(6,'(17h all snow is gone)')
         RETURN

      ELSE                        !!! snow layers combined

! two or more layers

         IF(snl < -1)THEN
            msn_old = snl
            mssi = 1
            DO i = msn_old+1, 0

! If top node is removed, combine with bottom neighbor
               IF(dz_soisno(i) < dzmin(mssi))THEN
                  IF(i == snl+1)THEN
                     neibor = i + 1

! If the bottom neighbor is not snow, combine with the top neighbor
                  ELSEIF(i == 0)THEN
                     neibor = i - 1

! If NONE of the above special cases apply, combine with the thinnest neighbor
                  ELSE
                     neibor = i + 1
                     IF((dz_soisno(i-1)+dz_soisno(i)) < (dz_soisno(i+1)+dz_soisno(i))) neibor = i-1
                  ENDIF

! Node l and j are combined and stored as node j.

                  IF(neibor > i)THEN
                     j = neibor
                     l = i
                  ELSE
                     j = i
                     l = neibor
                  ENDIF
                  CALL combo ( dz_soisno(j), wliq_soisno(j), wice_soisno(j), t_soisno(j),&
                               dz_soisno(l), wliq_soisno(l), wice_soisno(l), t_soisno(l) )

!Aerosol Fluxes (January 07, 2023)
                  mss_bcphi(j) = mss_bcphi(j)  + mss_bcphi(l)
                  mss_bcpho(j) = mss_bcpho(j)  + mss_bcpho(l)
                  mss_ocphi(j) = mss_ocphi(j)  + mss_ocphi(l)
                  mss_ocpho(j) = mss_ocpho(j)  + mss_ocpho(l)
                  mss_dst1 (j) = mss_dst1 (j)  + mss_dst1 (l)
                  mss_dst2 (j) = mss_dst2 (j)  + mss_dst2 (l)
                  mss_dst3 (j) = mss_dst3 (j)  + mss_dst3 (l)
                  mss_dst4 (j) = mss_dst4 (j)  + mss_dst4 (l)
!Aerosol Fluxes (January 07, 2023)


! Now shift all elements above this down one.

                  IF(j-1 > snl+1) THEN
                     DO k = j-1, snl+2, -1
                        t_soisno(k) = t_soisno(k-1)
                        wice_soisno(k) = wice_soisno(k-1)
                        wliq_soisno(k) = wliq_soisno(k-1)
                        dz_soisno(k) = dz_soisno(k-1)

!Aerosol Fluxes (January 07, 2023)
                        mss_bcphi(k) = mss_bcphi(k-1)
                        mss_bcpho(k) = mss_bcpho(k-1)
                        mss_ocphi(k) = mss_ocphi(k-1)
                        mss_ocpho(k) = mss_ocpho(k-1)
                        mss_dst1 (k) = mss_dst1 (k-1)
                        mss_dst2 (k) = mss_dst2 (k-1)
                        mss_dst3 (k) = mss_dst3 (k-1)
                        mss_dst4 (k) = mss_dst4 (k-1)
!Aerosol Fluxes (January 07, 2023)
                     ENDDO
                  ENDIF

                  snl = snl + 1

!*    write(6,'(7h Nodes ,i4,4h and,i4,14h combined into,i4)') l,j,j

                  IF(snl >= -1) EXIT

! The layer thickness great than the prescribed minimum value

               ELSE
                  mssi = mssi + 1
               ENDIF
            ENDDO

         ENDIF

! Reset the node depth and the depth of layer interface

         zi_soisno(0) = 0._r8
         DO k = 0, snl+1, -1
            z_soisno(k) = zi_soisno(k) - 0.5_r8*dz_soisno(k)
            zi_soisno(k-1) = zi_soisno(k) - dz_soisno(k)
         ENDDO

      ENDIF                       !!! snow layers combined

   END SUBROUTINE SnowLayersCombine_snicar
!-----------------------------------------------------------------------


   SUBROUTINE SnowLayersDivide_snicar (lb,snl,z_soisno,dz_soisno,zi_soisno,&
                                       wliq_soisno,wice_soisno,t_soisno,&

! Aerosol Fluxes (Jan. 07, 2023)
                                       mss_bcpho, mss_bcphi, mss_ocpho, mss_ocphi,&
                                       mss_dst1 , mss_dst2 , mss_dst3 , mss_dst4 )
! Aerosol Fluxes (Jan. 07, 2023)


!=======================================================================
!  Original author: Yongjiu Dai, September 15, 1999, January 07, 2023
!
!  subdivides snow layer when its thickness exceed the prescribed maximum
!
! !REVISIONS:
!  Yongjiu Dai, 01/2023: added Aerosol fluxes from SNICAR model
!=======================================================================

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------

    integer, intent(in)    :: lb                  ! lower bound of array
    integer, intent(inout) :: snl                 ! Number of snow
   real(r8), intent(inout) :: wice_soisno(lb:0)   ! ice lens [kg/m2]
   real(r8), intent(inout) :: wliq_soisno(lb:0)   ! liquid water [kg/m2]
   real(r8), intent(inout) :: t_soisno   (lb:0)   ! Node temperature [K]
   real(r8), intent(inout) :: dz_soisno  (lb:0)   ! Layer thickness [m]
   real(r8), intent(inout) :: z_soisno   (lb:0)   ! Node depth [m]
   real(r8), intent(inout) :: zi_soisno  (lb-1:0) ! Depth of layer interface [m]

! Aerosol Fluxes (Jan. 07, 2023)
   real(r8), intent(inout) :: &
        mss_bcpho (lb:0), &! mass of hydrophobic BC in snow  (col,lyr) [kg]
        mss_bcphi (lb:0), &! mass of hydrophillic BC in snow (col,lyr) [kg]
        mss_ocpho (lb:0), &! mass of hydrophobic OC in snow  (col,lyr) [kg]
        mss_ocphi (lb:0), &! mass of hydrophillic OC in snow (col,lyr) [kg]
        mss_dst1  (lb:0), &! mass of dust species 1 in snow  (col,lyr) [kg]
        mss_dst2  (lb:0), &! mass of dust species 2 in snow  (col,lyr) [kg]
        mss_dst3  (lb:0), &! mass of dust species 3 in snow  (col,lyr) [kg]
        mss_dst4  (lb:0)   ! mass of dust species 4 in snow  (col,lyr) [kg]
! Aerosol Fluxes (Jan. 07, 2023)

!-------------------------- Local Variables ----------------------------

! numbering from 1 (surface) msno (bottom)
   real(r8) :: drr      ! thickness of the combined [m]
   real(r8) :: dzsno(5) ! Snow layer thickness [m]
   real(r8) :: swice(5) ! Partial volume of ice [m3/m3]
   real(r8) :: swliq(5) ! Partial volume of liquid water [m3/m3]
   real(r8) :: tsno(5)  ! Node temperature [K]

   integer k            ! number of DO looping
   integer msno         ! number of snow layer 1 (top) to msno (bottom)

   real(r8) zwice,zwliq,propor

!Aerosol Fluxes (January 07, 2023)
   real(r8) mss_aerosol(5,8)
   real(r8) z_mss_aerosol(8)
!Aerosol Fluxes (January 07, 2023)

!-----------------------------------------------------------------------

      msno = abs(snl)
      DO k = 1, msno
         dzsno(k) = dz_soisno  (k + snl)
         swice(k) = wice_soisno(k + snl)
         swliq(k) = wliq_soisno(k + snl)
         tsno (k) = t_soisno   (k + snl)

!Aerosol Fluxes (January 07, 2023)
         mss_aerosol(k, 1) = mss_bcphi(k+snl)
         mss_aerosol(k, 2) = mss_bcpho(k+snl)
         mss_aerosol(k, 3) = mss_ocphi(k+snl)
         mss_aerosol(k, 4) = mss_ocpho(k+snl)
         mss_aerosol(k, 5) = mss_dst1 (k+snl)
         mss_aerosol(k, 6) = mss_dst2 (k+snl)
         mss_aerosol(k, 7) = mss_dst3 (k+snl)
         mss_aerosol(k, 8) = mss_dst4 (k+snl)
!Aerosol Fluxes (January 07, 2023)

      ENDDO

      IF(msno == 1)THEN
         IF(dzsno(1) > 0.03)THEN
            msno = 2
! Specified a new snow layer
            dzsno(1) = dzsno(1)/2.
            swice(1) = swice(1)/2.
            swliq(1) = swliq(1)/2.
!Aerosol Fluxes (January 07, 2023)
            mss_aerosol(1,:) = mss_aerosol(1,:)/2.
!Aerosol Fluxes (January 07, 2023)

            dzsno(2) = dzsno(1)
            swice(2) = swice(1)
            swliq(2) = swliq(1)
!Aerosol Fluxes (January 07, 2023)
            mss_aerosol(2,:) = mss_aerosol(1,:)
!Aerosol Fluxes (January 07, 2023)

            tsno(2)  = tsno(1)

!           write(6,*)'Subdivided Top Node into two layer (1/2)'
         ENDIF
      ENDIF

      IF(msno > 1)THEN
         IF(dzsno(1) > 0.02)THEN
            drr = dzsno(1) - 0.02
            propor = drr/dzsno(1)
            zwice = propor*swice(1)
            zwliq = propor*swliq(1)
!Aerosol Fluxes (January 07, 2023)
            z_mss_aerosol(:) = propor*mss_aerosol(1,:)
!Aerosol Fluxes (January 07, 2023)

            propor = 0.02/dzsno(1)
            swice(1) = propor*swice(1)
            swliq(1) = propor*swliq(1)
!Aerosol Fluxes (January 07, 2023)
            mss_aerosol(1,:) = propor*mss_aerosol(1,:)
!Aerosol Fluxes (January 07, 2023)

            dzsno(1) = 0.02

            CALL combo(dzsno(2),swliq(2),swice(2),tsno(2), &
                       drr,zwliq,zwice,tsno(1))

!Aerosol Fluxes (January 07, 2023)
            mss_aerosol(2,:) = z_mss_aerosol(:) + mss_aerosol(2,:)
!Aerosol Fluxes (January 07, 2023)

!           write(6,*) 'Subdivided Top Node &
!                       20 mm combined into underlying neighbor'

            IF(msno <= 2 .and. dzsno(2) > 0.07)THEN
! subdivided a new layer
               msno = 3
               dzsno(2) = dzsno(2)/2.
               swice(2) = swice(2)/2.
               swliq(2) = swliq(2)/2.
!Aerosol Fluxes (January 07, 2023)
               mss_aerosol(2,:) = mss_aerosol(2,:)/2.
!Aerosol Fluxes (January 07, 2023)

               dzsno(3) = dzsno(2)
               swice(3) = swice(2)
               swliq(3) = swliq(2)
!Aerosol Fluxes (January 07, 2023)
               mss_aerosol(3,:) = mss_aerosol(2,:)
!Aerosol Fluxes (January 07, 2023)

               tsno(3)  = tsno(2)
            ENDIF
         ENDIF
      ENDIF

      IF(msno > 2)THEN
         IF(dzsno(2) > 0.05)THEN
            drr = dzsno(2) - 0.05
            propor = drr/dzsno(2)
            zwice = propor*swice(2)
            zwliq = propor*swliq(2)
!Aerosol Fluxes (January 07, 2023)
            z_mss_aerosol(:) = propor*mss_aerosol(2,:)
!Aerosol Fluxes (January 07, 2023)

            propor = 0.05/dzsno(2)
            swice(2) = propor*swice(2)
            swliq(2) = propor*swliq(2)
!Aerosol Fluxes (January 07, 2023)
            mss_aerosol(2,:) = propor*mss_aerosol(2,:)
!Aerosol Fluxes (January 07, 2023)

            dzsno(2) = 0.05

            CALL combo(dzsno(3),swliq(3),swice(3),tsno(3), &
                       drr,     zwliq,   zwice,   tsno(2))

!Aerosol Fluxes (January 07, 2023)
            mss_aerosol(3,:) = z_mss_aerosol(:) + mss_aerosol(3,:)
!Aerosol Fluxes (January 07, 2023)

!           write(6,*)'Subdivided 50 mm from the subsurface layer &
!                      &and combined into underlying neighbor'

            IF(msno <= 3 .and. dzsno(3) > 0.18)THEN
! subdivided a new layer
               msno =  4
               dzsno(3) = dzsno(3)/2.
               swice(3) = swice(3)/2.
               swliq(3) = swliq(3)/2.
!Aerosol Fluxes (January 07, 2023)
               mss_aerosol(3,:) = mss_aerosol(3,:)/2.
!Aerosol Fluxes (January 07, 2023)

               dzsno(4) = dzsno(3)
               swice(4) = swice(3)
               swliq(4) = swliq(3)
!Aerosol Fluxes (January 07, 2023)
               mss_aerosol(4,:) = mss_aerosol(3,:)
!Aerosol Fluxes (January 07, 2023)

               tsno(4)  = tsno(3)

            ENDIF
         ENDIF
      ENDIF

      IF(msno > 3)THEN
         IF(dzsno(3) > 0.11)THEN
            drr = dzsno(3) - 0.11
            propor = drr/dzsno(3)
            zwice = propor*swice(3)
            zwliq = propor*swliq(3)
!Aerosol Fluxes (January 07, 2023)
            z_mss_aerosol(:) = propor*mss_aerosol(3,:)
!Aerosol Fluxes (January 07, 2023)

            propor = 0.11/dzsno(3)
            swice(3) = propor*swice(3)
            swliq(3) = propor*swliq(3)
!Aerosol Fluxes (January 07, 2023)
            mss_aerosol(3,:) = propor*mss_aerosol(3,:)
!Aerosol Fluxes (January 07, 2023)

            dzsno(3) = 0.11

            CALL combo(dzsno(4),swliq(4),swice(4),tsno(4), &
                       drr,     zwliq,   zwice,   tsno(3))

!Aerosol Fluxes (January 07, 2023)
            mss_aerosol(4,:) = z_mss_aerosol(:) + mss_aerosol(4,:)
!Aerosol Fluxes (January 07, 2023)

!           write(6,*)'Subdivided 110 mm from the third Node &
!                      &and combined into underlying neighbor'

            IF(msno <= 4 .and. dzsno(4) > 0.41)THEN
! subdivided a new layer
               msno = 5
               dzsno(4) = dzsno(4)/2.
               swice(4) = swice(4)/2.
               swliq(4) = swliq(4)/2.
!Aerosol Fluxes (January 07, 2023)
               mss_aerosol(4,:) = mss_aerosol(4,:)/2.
!Aerosol Fluxes (January 07, 2023)

               dzsno(5) = dzsno(4)
               swice(5) = swice(4)
               swliq(5) = swliq(4)
!Aerosol Fluxes (January 07, 2023)
               mss_aerosol(5,:) = mss_aerosol(4,:)
!Aerosol Fluxes (January 07, 2023)

               tsno(5)  = tsno(4)

            ENDIF
         ENDIF
      ENDIF

      IF(msno > 4)THEN
         IF(dzsno(4) > 0.23)THEN
            drr = dzsno(4) - 0.23
            propor = drr/dzsno(4)
            zwice = propor*swice(4)
            zwliq = propor*swliq(4)
!Aerosol Fluxes (January 07, 2023)
            z_mss_aerosol(:) = propor*mss_aerosol(4,:)
!Aerosol Fluxes (January 07, 2023)

            propor = 0.23/dzsno(4)
            swice(4) = propor*swice(4)
            swliq(4) = propor*swliq(4)
!Aerosol Fluxes (January 07, 2023)
            mss_aerosol(4,:) = propor*mss_aerosol(4,:)
!Aerosol Fluxes (January 07, 2023)

            dzsno(4) = 0.23

            CALL combo(dzsno(5),swliq(5),swice(5),tsno(5), &
                       drr,     zwliq,   zwice,   tsno(4))

!Aerosol Fluxes (January 07, 2023)
            mss_aerosol(5,:) = z_mss_aerosol(:) + mss_aerosol(5,:)
!Aerosol Fluxes (January 07, 2023)

!           write(6,*)'Subdivided 230 mm from the fourth Node &
!                     'and combined into underlying neighbor'
         ENDIF
      ENDIF

      snl = - msno

      DO k = snl+1, 0
         dz_soisno(k)   = dzsno(k - snl)
         wice_soisno(k) = swice(k - snl)
         wliq_soisno(k) = swliq(k - snl)

!Aerosol Fluxes (January 07, 2023)
         mss_bcphi(k) = mss_aerosol(k - snl, 1)
         mss_bcpho(k) = mss_aerosol(k - snl, 2)
         mss_ocphi(k) = mss_aerosol(k - snl, 3)
         mss_ocpho(k) = mss_aerosol(k - snl, 4)
         mss_dst1 (k) = mss_aerosol(k - snl, 5)
         mss_dst2 (k) = mss_aerosol(k - snl, 6)
         mss_dst3 (k) = mss_aerosol(k - snl, 7)
         mss_dst4 (k) = mss_aerosol(k - snl, 8)
!Aerosol Fluxes (January 07, 2023)

         t_soisno(k)  = tsno (k - snl)

      ENDDO

      zi_soisno(0) = 0.
      DO k = 0, snl+1, -1
         z_soisno(k)    = zi_soisno(k) - 0.5*dz_soisno(k)
         zi_soisno(k-1) = zi_soisno(k) - dz_soisno(k)
      ENDDO

   END SUBROUTINE SnowLayersDivide_snicar

END MODULE MOD_SnowLayersCombineDivide
! ---------- EOP ------------
