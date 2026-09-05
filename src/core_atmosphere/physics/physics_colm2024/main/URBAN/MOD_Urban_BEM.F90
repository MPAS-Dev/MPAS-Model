#include <define.h>

MODULE MOD_Urban_BEM

   USE MOD_Precision
   USE MOD_Vars_Global
   USE MOD_Const_Physical
   USE MOD_Urban_Shortwave, only: MatrixInverse

   IMPLICIT NONE
   SAVE
   PRIVATE

   ! A simple building energy model to calculate room temperature
   PUBLIC :: SimpleBEM

CONTAINS

!-----------------------------------------------------------------------
   SUBROUTINE SimpleBEM (deltim, rhoair, fcover, H, troom_max, troom_min, &
                         troof_nl_bef, twsun_nl_bef, twsha_nl_bef, &
                         troof_nl, twsun_nl, twsha_nl, &
                         tkdz_roof, tkdz_wsun, tkdz_wsha, taf, &
                         troom, troof_inner, twsun_inner, twsha_inner, &
                         Fhac, Fwst, Fach, Fhah)

!-----------------------------------------------------------------------
!
! !DESCRIPTION:
!
!  A simple building energy model to calculate room temperature
!
!  The basic approach is as follows:
!
!     1. Predict indoor temperature using the indoor energy balance
!     equations (see below) without turning on the air conditioning.
!
!     2. If the indoor temperature falls within the predefined comfort
!     range, further energy consumption calculations are not necessary,
!     only indoor and outdoor heat exchange is considered.
!
!     3. If the indoor temperature falls outside the predefined comfort
!     range, calculate the minimum/maximum heating/cooling capacity
!     based on the air conditioning usage strategy.
!
!     4. Calculate the indoor and outdoor heat exchange and waste heat
!     discharge (taking into account energy utilization efficiency)
!     based on the calculated heating/cooling capacity in step 3.
!
!     Finally, energy consumption can be calculated based on the total
!     heat flux.
!
!  o Solve the following energy balance equations
!  o Variables: troom, troof_inner, twsun_inner, twsha_innter
!
!     Hc_roof = Fn_roof        .................................(1)
!     Hc_wsun = Fn_wsun        .................................(2)
!     Hc_wsha = Fn_wsha        .................................(3)
!
!                    Troom' - Troom
!     H*rhoair*cpair*-------------- =
!                          dt
!       ACH
!     ------*H*rhoair*cpair*(Taf-Troom') + Hc_roof + Hc_wsun + Hc_wsha
!      3600
!                              .................................(4)
!
!  Created by Hua Yuan, 09/2021
!
! !REVISIONS:
!
!  11/2022, Hua Yuan: Add option for constant AC.
!
!-----------------------------------------------------------------------

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   real(r8), intent(in) :: &
        deltim,          &! seconds in a time step [second]
        rhoair,          &! density air [kg/m3]
        fcover(0:2),     &! fractional cover of roof, wall
        H,               &! average building height [m]
        troom_max,       &! maximum temperature of inner building
        troom_min,       &! minimum temperature of inner building
        troof_nl_bef,    &! roof temperature at layer nl_roof
        twsun_nl_bef,    &! sunlit wall temperature at layer nl_wall
        twsha_nl_bef,    &! shaded wall temperature at layer nl_wall
        troof_nl,        &! roof temperature at layer nl_roof
        twsun_nl,        &! sunlit wall temperature at layer nl_wall
        twsha_nl,        &! shaded wall temperature at layer nl_wall
        tkdz_roof,       &! temporal var for heat transfer of roof
        tkdz_wsun,       &! temporal var for heat transfer of sunlit wall
        tkdz_wsha,       &! temporal var for heat transfer of shaded wall
        taf               ! temperature of urban air

   real(r8), intent(inout) :: &
        troom,           &! temperature of inner building
        troof_inner,     &! temperature of inner roof
        twsun_inner,     &! temperature of inner sunlit wall
        twsha_inner       ! temperature of inner shaded wall

   real(r8), intent(out) :: &
        Fhah,            &! flux from heating
        Fhac,            &! flux from heat or cool AC
        Fwst,            &! waste heat from cool or heat
        Fach              ! flux from air exchange

!-------------------------- Local Variables ----------------------------
   real(r8) :: &
        ACH,             &! air exchange coefficient
        hcv_roof,        &! convective exchange coefficient for roof<->room
        hcv_wall,        &! convective exchange coefficient for wall<->room
        waste_coef,      &! waste coefficient
        waste_cool,      &! waste heat for AC cooling
        waste_heat        ! waste heat for AC heating

   real(r8) :: &
        f_wsun,          &! weight factor for sunlit wall
        f_wsha            ! weight factor for shaded wall

   real(r8) :: &
        A(4,4),          &! Heat transfer matrix
        Ainv(4,4),       &! Inverse of Heat transfer matrix
        B(4),            &! B for Ax=B
        X(4)              ! x for Ax=B

   real(r8) :: &
        troom_pro,       &! projected room temperature
        troom_bef,       &! temperature of inner building
        troof_inner_bef, &! temperature of inner roof
        twsun_inner_bef, &! temperature of inner sunlit wall
        twsha_inner_bef   ! temperature of inner shaded wall

   logical :: cooling, heating

   ! Option for continuous AC
   logical, parameter :: Constant_AC = .true.

!-----------------------------------------------------------------------

      ACH = 0.3           !air exchange coefficient
      hcv_roof   = 4.040  !convective exchange coefficient for roof<->room (W m-2 K-1)
      hcv_wall   = 3.076  !convective exchange coefficient for wall<->room (W m-2 K-1)
      waste_cool = 0.6    !waste heat for AC cooling
      waste_heat = 0.2    !waste heat for AC heating
      cooling = .false.   !cooling case
      heating = .false.   !heating case

      f_wsun = fcover(1)/fcover(0) !weight factor for sunlit wall
      f_wsha = fcover(2)/fcover(0) !weight factor for shaded wall

      ! initialization
      Fhac = 0.; Fwst = 0.; Fach = 0.; Fhah = 0.;

      ! Ax = B
      ! set values for heat transfer matrix
      ! 1: roof, 2: sunlit wall, 3: shaded wall, 4: room
      A(:,:) = 0.
      A(1,:) = (/0.5*hcv_roof+0.5*tkdz_roof, 0., 0., -0.5*hcv_roof/)
      A(2,:) = (/0., 0.5*hcv_wall+0.5*tkdz_wsun, 0., -0.5*hcv_wall/)
      A(3,:) = (/0., 0., 0.5*hcv_wall+0.5*tkdz_wsha, -0.5*hcv_wall/)

      A(4,:) = (/-0.5*hcv_roof, -0.5*hcv_wall*f_wsun, -0.5*hcv_wall*f_wsha, &
                  0.5*hcv_roof + 0.5*hcv_wall*f_wsun + 0.5*hcv_wall*f_wsha +&
                  H*rhoair*cpair/deltim + (ACH/3600.)*H*rhoair*cpair /)

      B(1) = -0.5*hcv_roof*(troof_inner-troom) + 0.5*tkdz_roof*(troof_nl_bef-troof_inner) &
                                               + 0.5*tkdz_roof*troof_nl
      B(2) = -0.5*hcv_wall*(twsun_inner-troom) + 0.5*tkdz_wsun*(twsun_nl_bef-twsun_inner) &
                                               + 0.5*tkdz_wsun*twsun_nl
      B(3) = -0.5*hcv_wall*(twsha_inner-troom) + 0.5*tkdz_wsha*(twsha_nl_bef-twsha_inner) &
                                               + 0.5*tkdz_wsha*twsha_nl

      B(4) = H*rhoair*cpair*troom/deltim + (ACH/3600.)*H*rhoair*cpair*taf &
           + 0.5*hcv_roof*(troof_inner-troom) &
           + 0.5*hcv_wall*(twsun_inner-troom)*f_wsun &
           + 0.5*hcv_wall*(twsha_inner-troom)*f_wsha

      ! Inverse of matrix A
      Ainv = MatrixInverse(A)

      ! Matrix computing to resolve multiple reflections
      X = matmul(Ainv, B)

      troof_inner_bef = troof_inner
      twsun_inner_bef = twsun_inner
      twsha_inner_bef = twsha_inner
      troom_bef       = troom

      troof_inner = X(1)
      twsun_inner = X(2)
      twsha_inner = X(3)
      troom       = X(4)
      troom_pro   = X(4)

      Fach = (ACH/3600.)*H*rhoair*cpair*(troom - taf)

      IF (troom > troom_max) THEN !cooling case
         Fhac  = H*rhoair*cpair*(troom-troom_max)/deltim
         troom = troom_max
         Fwst  = Fhac*waste_cool
      ENDIF

      IF (troom < troom_min) THEN !heating case
         Fhac  = H*rhoair*cpair*(troom-troom_min)/deltim
         troom = troom_min
         Fwst  = abs(Fhac)*waste_heat
         ! negative value, set it to 0.
         Fhac  = 0.
      ENDIF

      ! for constant cooling or heating
      IF ((troom_pro>troom_max .or. troom_pro<troom_min) .and. Constant_AC) THEN

         IF (troom_pro > troom_max) THEN !cooling case
            troom = troom_max
            waste_coef = waste_cool
            cooling    = .true.
         ENDIF

         IF (troom_pro < troom_min) THEN !heating case
            troom = troom_min
            waste_coef = waste_heat
            heating    = .true.
         ENDIF

         Fach = (ACH/3600.)*H*rhoair*cpair*(troom - taf)

         troof_inner = (B(1)-A(1,4)*troom)/A(1,1)
         twsun_inner = (B(2)-A(2,4)*troom)/A(2,2)
         twsha_inner = (B(3)-A(3,4)*troom)/A(3,3)

         Fhac = 0.5*hcv_roof*(troof_inner_bef-troom_bef) &
              + 0.5*hcv_roof*(troof_inner-troom)
         Fhac = 0.5*hcv_wall*(twsun_inner_bef-troom_bef)*f_wsun &
              + 0.5*hcv_wall*(twsun_inner-troom)*f_wsun + Fhac
         Fhac = 0.5*hcv_wall*(twsha_inner_bef-troom_bef)*f_wsha &
              + 0.5*hcv_wall*(twsha_inner-troom)*f_wsha + Fhac

         IF ( heating ) Fhah = abs(Fhac)
         Fhac = abs(Fhac) + abs(Fach)
         Fwst = Fhac*waste_coef
         IF ( heating ) Fhac = 0.

      ENDIF

      Fhah = Fhah*fcover(0)
      Fach = Fach*fcover(0)
      Fwst = Fwst*fcover(0)
      Fhac = Fhac*fcover(0)

   END SUBROUTINE SimpleBEM

END MODULE MOD_Urban_BEM
! ---------- EOP ------------
