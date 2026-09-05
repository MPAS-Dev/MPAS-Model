#include <define.h>

MODULE MOD_Urban_Longwave

   USE MOD_Precision
   USE MOD_Vars_Global
   USE MOD_Const_Physical, only: stefnc
   USE MOD_Urban_Shortwave, only: MatrixInverse
   USE MOD_Urban_Shortwave, only: ShadowWall_dir
   USE MOD_Urban_Shortwave, only: ShadowWall_dif
   USE MOD_Urban_Shortwave, only: ShadowTree
   USE MOD_3DCanopyRadiation, only: tee, phi

   IMPLICIT NONE
   SAVE
   PRIVATE

   PUBLIC :: UrbanOnlyLongwave       !Urban Longwave radiation transfer
   PUBLIC :: UrbanVegLongwave        !Urban Longwave radiation transfer with trees

CONTAINS

   SUBROUTINE UrbanOnlyLongwave (theta, HL, fb, fgper, H, LW, &
              twsun, twsha, tgimp, tgper, ewall, egimp, egper, &
              Ainv, B, B1, dBdT, SkyVF, fcover)

!-----------------------------------------------------------------------
!                Sun
!                 \\\
!                  \\\
!                         ______
!                        |++++++|              roof
!                        |++++++|             ______
!                        |++++++|            |++++++|
!                    ______+++++|            |++++++|
!                   |++++++|++++|            |++++++|
!            sunlit |[]++[]|++++|            |++++++| shaded
!             wall  |++++++|                 |++++++|  wall
!                   |[]++[]|                 |++++++|
!                   |++++++|  impervious/pervious ground
!         __________|++++++|____________________________________
!
!
! !DESCRIPTION:
!
!  The process of long-wave radiation transmission in the absence of
!  vegetation is similar to the incident diffuse case of short-wave
!  radiation transmission in the absence of vegetation (where long-wave
!  radiation is approximated as a diffuse source). The long-wave
!  radiation flux reaching each component surface is calculated, as well
!  as the long-wave radiation emitted outward from each component
!  surface. Multiple scattering and absorption between components are
!  considered, and a long-wave radiation transmission equilibrium
!  equation is established for solving.
!
!  Created by Hua Yuan, 09/2021
!
! !REVISIONS:
!
!-----------------------------------------------------------------------

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   real(r8), intent(in) :: &
        theta,      &! Sun zenith angle [radian]
        HL,         &! Ratio of building height to ground width [-]
        fb,         &! Fraction of building area [-]
        fgper,      &! Fraction of impervious ground [-]
        H,          &! Building average height [m]
        LW,         &! Downward longwave radiation [W/m2]

        twsun,      &! Temperature of sunlit wall [K]
        twsha,      &! Temperature of shaded wall [K]
        tgimp,      &! Temperature of impervious road [K]
        tgper,      &! Temperature of pervious road [K]

        ewall,      &! Emissivity of walls [-]
        egimp,      &! Emissivity of ground [-]
        egper        ! Emissivity of ground [-]

   real(r8), intent(out) :: &
        Ainv(4,4),  &! Inverse of Radiation transfer matrix
        B(4),       &! Vectors of incident radiation on each surface
        B1(4),      &! Vectors of incident radiation on each surface
        dBdT(4),    &! Vectors of incident radiation on each surface
        SkyVF(4),   &! View factor to sky
        fcover(0:4)  ! View factor to sky

!-------------------------- Local Variables ----------------------------
   real(r8) ::    &
        W,          &! Urban ground average width [m]
        L,          &! Urban building average length [m]
        HW,         &! Ratio of H to W, H/W [-]
        fg,         &! Fraction of ground [-]
        fgimp,      &! Fraction of snow ground [-]

        Fsw,        &! View factor from sky to wall [-]
        Fsg,        &! View factor from sky to ground [-]
        Fgw,        &! View factor from ground to wall [-]
        Fgs,        &! View factor from ground to sky [-]
        Fww,        &! View factor from wall to wall [-]
        Fwg,        &! View factor from wall to ground [-]
        Fws,        &! View factor from wall to sky [-]

        Sw,         &! Shadow of wall [-]
        fwsun,      &! Fraction of sunlit wall [-]
        fwsha,      &! Fraction of shaded wall [-]
        Iwsun,      &! Incident radiation on sunlit wall [W/m2]
        Iwsha,      &! Incident radiation on shaded wall [W/m2]
        Ig,         &! Incident radiation on ground [W/m2]
        Igimp,      &! Incident radiation on impervious ground [W/m2]
        Igper        ! Incident radiation on pervious ground [W/m2]

   real(r8) :: A(4,4)     !Radiation transfer matrix

   ! Temporal
   real(r8) :: tmp, eb
!-----------------------------------------------------------------------

      ! Calculate urban structure parameters
      !-------------------------------------------------
      !W  = H/HW
      !L  = W*sqrt(fb)/(1-sqrt(fb))
      !HL = H/L !NOTE: Same as HL = HW*(1-sqrt(fb))/sqrt(fb)
      fg = 1. - fb
      fgimp = 1. - fgper

      ! Calculate view factors
      !-------------------------------------------------

      ! View factor from sky to wall(sunlit+shaded) and ground
      Fsw = ShadowWall_dif(fb/fg, HL)
      Fsg = 1 - Fsw

      ! View factor from ground to walls and sky
      Fgw = Fsw
      Fgs = Fsg

      ! View factor from wall to wall, sky and ground
      !   Fws*4*H*L/L/L = Fws*4H/L*fb = Fsw*fg
      !   Fws*4HL*fb = Fsw*fg
      !   Fws = Fsw*fg/(4HL*fb)
      ! Adjusted as below:
      Fws = Fsw*fg/fb/(4*HL)
      Fwg = Fsw*fg/fb/(4*HL)
      Fww = 1 - Fws - Fwg

      ! Calculate sunlit wall fraction
      !-------------------------------------------------

      ! Building shadow on the ground
      Sw = ShadowWall_dir(fb/fg, HL, theta)

      ! Sunlit/shaded wall fraction
      fwsun = 0.5 * (Sw*fg + fb) / (4/PI*fb*HL*tan(theta) + fb)
      fwsha = 1. - fwsun

      ! Calculate radiation transfer matrix
      !   AX = B
      ! o A: radiation transfer matrix
      ! o B: incident radiation on each surface
      ! o X: radiation emit from each surface
      !-------------------------------------------------
      A(1,:) = (/1-Fww*fwsun*(1-ewall),  -Fww*fwsun*(1-ewall), &
                  -Fgw*fwsun*(1-ewall),  -Fgw*fwsun*(1-ewall) /)

      A(2,:) = (/ -Fww*fwsha*(1-ewall), 1-Fww*fwsha*(1-ewall), &
                  -Fgw*fwsha*(1-ewall),  -Fgw*fwsha*(1-ewall) /)

      A(3,:) = (/ -Fwg*fgimp*(1-egimp),  -Fwg*fgimp*(1-egimp), &
                                 1._r8,                 0._r8 /)

      A(4,:) = (/ -Fwg*fgper*(1-egper),  -Fwg*fgper*(1-egper), &
                                 0._r8,                 1._r8 /)

      ! Inverse of matrix A
      Ainv = MatrixInverse(A)

      ! Incident LW radiation on sunlit/shaded wall and
      ! impervious/pervious ground
      Iwsun = LW*Fsw*fwsun
      Iwsha = LW*Fsw*fwsha
      Ig    = LW*Fsg
      Igimp = Ig*fgimp
      Igper = Ig*fgper

      ! Vector of initial LW radiation on each surface
      !NOTE: for 3D, absorption per unit area: 4*HL*fb/fg
      !      for canyon: absorption per unit area: 2*HW
      B(1) = Iwsun*(1.-ewall) + 4*fwsun*HL*fb/fg*stefnc*ewall*twsun**4
      B(2) = Iwsha*(1.-ewall) + 4*fwsha*HL*fb/fg*stefnc*ewall*twsha**4
      !B(1) = Iwsun*(1.-ewall) + 2*fwsun*HW*stefnc*ewall*twsun**4
      !B(2) = Iwsha*(1.-ewall) + 2*fwsha*HW*stefnc*ewall*twsha**4
      B(3) = Igimp*(1.-egimp) + fgimp*stefnc*egimp*tgimp**4
      B(4) = Igper*(1.-egper) + fgper*stefnc*egper*tgper**4

      B1(1) = 4*fwsun*HL*fb/fg*stefnc*ewall*twsun**4
      B1(2) = 4*fwsha*HL*fb/fg*stefnc*ewall*twsha**4
      !B1(1) = 2*fwsun*HW*stefnc*ewall*twsun**4
      !B1(2) = 2*fwsha*HW*stefnc*ewall*twsha**4
      B1(3) = fgimp*stefnc*egimp*tgimp**4
      B1(4) = fgper*stefnc*egper*tgper**4

      dBdT(1) = 16*fwsun*HL*fb/fg*stefnc*ewall*twsun**3
      dBdT(2) = 16*fwsha*HL*fb/fg*stefnc*ewall*twsha**3
      !dBdT(1) = 2*fwsun*HW*stefnc*ewall*twsun**3
      !dBdT(2) = 2*fwsha*HW*stefnc*ewall*twsha**3
      dBdT(3) = 4*fgimp*stefnc*egimp*tgimp**3
      dBdT(4) = 4*fgper*stefnc*egper*tgper**3

      SkyVF(1:2) = Fws
      SkyVF(3:4) = Fgs

      fcover(0) = fb
      fcover(1) = 4*fwsun*HL*fb
      fcover(2) = 4*fwsha*HL*fb
      fcover(3) = fg*fgimp
      fcover(4) = fg*fgper

      !NOTE: the below codes put into the THERMAL.F90
      ! Equation solve
      ! X = matmul(Ainv, B)

      ! LW radiation absorption by each surface (per m^2)
      !lwsun = ( ewall*X(1) - B1(1) ) / (1-ewall) !/ (4*fwsun*HL*fb/fg)
      !lwsha = ( ewall*X(2) - B1(2) ) / (1-ewall) !/ (4*fwsha*HL*fb/fg)
      !lgimp = ( egimp*X(3) - B1(3) ) / (1-egimp) !/ fgimp
      !lgper = ( egper*X(4) - B1(4) ) / (1-egper) !/ fgper

      ! Out-going LW of urban canopy
      !lout = X(1)*Fws + X(2)*Fws + X(3)*Fgs + X(4)*Fgs
      !lout = sum( X * SkyVF )

      ! Energy balance check
      !eb = lwsun + lwsha + lgimp + lgper + lout

      !IF (abs(eb-LW) > 1e-6) THEN
      !  print *, "Longwave - Energy Balance Check error!", eb-LW
      !ENDIF

      !NOTE: put it outside, after temperature change of roof, wall and ground
      ! absorption change due to temperature change, as restart variables.
      !dX = matmul(Ainv, dBdT*dT)
      !lwsun = ( ewall*dX(1) - dBdT(1)*dT(1) ) / (1-ewall) !/ (4*fwsun*HL*fb/fg)
      !lwsha = ( ewall*dX(2) - dBdT(2)*dT(2) ) / (1-ewall) !/ (4*fwsha*HL*fb/fg)
      !lgimp = ( egimp*dX(3) - dBdT(3)*dT(3) ) / (1-egimp) !/ fgimp
      !lgper = ( egper*dX(4) - dBdT(4)*dT(4) ) / (1-egper) !/ fgper

      !lout  = lout + sum( dX * SkyVF )

   END SUBROUTINE UrbanOnlyLongwave


   SUBROUTINE UrbanVegLongwave (theta, HL, fb, fgper, H, LW, &
              twsun, twsha, tgimp, tgper, ewall, egimp, egper, lai, sai, fv, hv, &
              ev, Ainv, B, B1, dBdT, SkyVF, VegVF, fcover)

!-----------------------------------------------------------------------
!              Sun
!               \\\
!                \\\
!                       ______
!                      |++++++|              roof
!                      |++++++|             ______
!                      |++++++|    ___     |++++++|
!                  ______+++++|   |||||    |++++++|
!                 |++++++|++++|  |||||||   |++++++|
!          sunlit |[]++[]|++++|   |||||    |++++++| shaded
!           wall  |++++++|          | tree |++++++|  wall
!                 |[]++[]|          |      |++++++|
!                 |++++++|  impervious/pervious ground
!       __________|++++++|___________________________________
!
! !DESCRIPTION:
!
!  The calculation of longwave radiation when considering vegetation
!  (trees only) is similar to the shortwave radiation transmission with
!  vegetation. On the basis of the longwave radiation transmission
!  balance equation without vegetation, a balanced equation with
!  vegetation is constructed, and the solution process is similar.
!
!  Created by Hua Yuan, 09/2021
!-----------------------------------------------------------------------

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   real(r8), intent(in) :: &
        theta,      &! Sun zenith angle [radian]
        HL,         &! Ratio of building height to ground width [-]
        fb,         &! Fraction of building area [-]
        fgper,      &! Fraction of impervious ground [-]
        H,          &! Building average height [m]
        LW,         &! Downward longwave radiation [W/m2]

        twsun,      &! Temperature of sunlit wall [K]
        twsha,      &! Temperature of shaded wall [K]
        tgimp,      &! Temperature of impervious road [K]
        tgper,      &! Temperature of pervious road [K]

        ewall,      &! Emissivity of walls [-]
        egimp,      &! Emissivity of ground [-]
        egper,      &! Emissivity of ground [-]
        lai,        &! leaf area index [m2/m2]
        sai,        &! stem area index [m2/m2]
        fv,         &! Fraction of tree cover [-]
        hv           ! Crown central height [m]

   real(r8), intent(out) :: &
        ev,         &! emissivity of vegetation
        Ainv(5,5),  &! Inverse of Radiation transfer matrix
        B(5),       &! Vectors of incident radiation on each surface
        B1(5),      &! Vectors of incident radiation on each surface
        dBdT(5),    &! Vectors of incident radiation on each surface
        SkyVF(5),   &! View factor to sky
        VegVF(5),   &! View factor to sky
        fcover(0:5)  ! View factor to sky

!-------------------------- Local Variables ----------------------------
   real(r16),parameter :: DD1=1.0_r16 !quad accuracy real number

   real(r8) :: &
        W,          &! Urban ground average width [m]
        L,          &! Urban building average length [m]
        HW,         &! Ratio of H to W, H/W [-]
        fg,         &! Fraction of ground [-]
        fgimp,      &! Fraction of pervious ground [-]

        Fsw,        &! View factor from sky to wall [-]
        Fsg,        &! View factor from sky to ground [-]
        Fgw,        &! View factor from ground to wall [-]
        Fgs,        &! View factor from ground to sky [-]
        Fww,        &! View factor from wall to wall [-]
        Fwg,        &! View factor from wall to ground [-]
        Fws,        &! View factor from wall to sky [-]

        Fvg,        &! View factor from tree to ground [-]
        Fvs,        &! View factor from tree to sky [-]
        Fvw,        &! View factor from tree to walls (sunlit+shaded) [-]
        Fwv,        &! View factor from wall to tree [-]
        Fgv,        &! View factor from ground to tree [-]
        Fsv,        &! View factor from sky to tree [-]

        Fgvs,       &! View factor from ground->|tree|-> to sky [-]
        Fgvw,       &! View factor from ground->|tree|-> to walls [-]
        Fsvg,       &! View factor from sky->|tree|-> to ground [-]
        Fsvw,       &! View factor from sky->|tree|-> to walls [-]
        Fwvw,       &! View factor from walls->|tree|-> to walls [-]
        Fwvs,       &! View factor from walls->|tree|-> to sky [-]
        Fwvg,       &! View factor from walls->|tree|-> to ground [-]

        Fsw_,       &! Fsw - Fsvw + Fsvw*Td [-]
        Fsg_,       &! Fsg - Fsvg + Fsvg*Td [-]
        Fgw_,       &! Fgw - Fgvw + Fgvw*Td [-]
        Fgs_,       &! Fgs - Fgvs + Fgvs*Td [-]
        Fwg_,       &! Fwg - Fwvg + Fwvg*Td [-]
        Fww_,       &! Fww - Fwvw + Fwvw*Td [-]
        Fws_,       &! Fws - Fwvs + Fwvs*Td [-]

        Sw,         &! Shadow of wall [-]
        Sw_,        &! Shadow of wall [-]
        Sv,         &! Shadow of trees [-]
        Swv,        &! Overlapped shadow between wall and trees [-]
        fv_,        &! Fraction of trees [-]
        Td,         &! Transmission of tree [-]
        fwsun,      &! Fraction of sunlit wall [-]
        fwsha,      &! Fraction of shaded wall [-]
        Iwsun,      &! Incident radiation on sunlit wall [W/m2]
        Iwsha,      &! Incident radiation on shaded wall [W/m2]
        Ig,         &! Incident radiation on ground [W/m2]
        Igimp,      &! Incident radiation on impervious ground [W/m2]
        Igper,      &! Incident radiation on pervious ground [W/m2]
        Iv           ! Incident radiation on trees [W/m2]

   ! Radiation transfer matrix and vectors
   !-------------------------------------------------
   real(r8) :: A(5,5)     !Radiation transfer matrix

   ! Temporal
   real(r8) :: tmp, eb, fac1, fac2, lsai
!-----------------------------------------------------------------------

      ! Calculate urban structure parameters
      !-------------------------------------------------
      !W  = H/HW
      !L  = W*sqrt(fb)/(1-sqrt(fb))
      !HL = H/L !NOTE: Same as HL = HW*(1-sqrt(fb))/sqrt(fb)
      L  = H/HL
      fg = 1. - fb

      fgimp = 1. - fgper

      ! Calculate transmission and albedo of tree
      !-------------------------------------------------
      lsai = (lai+sai)*fv/cos(PI/3)/ShadowTree(fv, PI/3)
      Td = tee(DD1*3/8.*lsai)
      ev = 1 - Td

      ! Calculate view factors
      !-------------------------------------------------

      ! View factor from sky to wall(sunlit+shaded) and ground
      Fsw = ShadowWall_dif(fb/fg, HL)
      Fsg = 1 - Fsw

      ! View factor from ground to walls and sky
      Fgw = Fsw
      Fgs = Fsg

      ! View factor from wall to wall, sky and ground
      ! Fws*4*H*L*L/L = Fws*4H/L*fb = Fsw*fg
      ! Fws*4HL*fb = Fsw*fg
      ! Fws = Fsw*fg/(4HL*fb)
      Fws = Fsw*fg/fb/(4*HL)
      Fwg = Fsw*fg/fb/(4*HL)
      Fww = 1 - Fws - Fwg

      ! View factor from tree to walls, ground and sky
      !-------------------------------------------------

      Sw  = ShadowWall_dif(fb/fg, HL)
      Sw_ = ShadowWall_dif(fb/fg, (H-hv)/L)

      !NOTE: fg*(fv/fg - fv/fg * Sw_)
      fv_ = fv - fv*Sw_
      Sv  = ShadowTree(fv_, PI/3)

      ! Overlapped shadow between tree and building
      ! (to ground only)
      Swv = (Sw-Sw_) * Sv

      ! convert Sv to ground ratio
      Sv  = min(1., Sv/fg)

      ! robust check
      IF (Sw+Sv-Swv > 1) THEN
         Swv = Sw+Sv-1
      ENDIF

      ! Calibrated building ground shadow
      Fsv  = Sv
      Fsvw = Swv
      Fsvg = Fsv - Fsvw

      ! View factor from veg to sky and walls above canopy
      Fvs = 0.5*(1-Sw_)
      Fvw = 0.5*Sw_

      Sw_ = ShadowWall_dif(fb/fg, hv/L)
      fv_ = fv - fv*Sw_
      Sv  = ShadowTree(fv_, PI/3)

      ! Overlapped shadow between tree and building
      ! (to ground only)
      Swv = (Sw-Sw_) * Sv

      ! convert Sv to ground ratio
      Sv  = min(1., Sv/fg)

      ! robust check
      IF (Sw+Sv-Swv > 1) THEN
         Swv = Sw+Sv-1
      ENDIF

      ! Calibrated building ground shadow
      Fgv  = Sv
      Fgvw = Swv
      Fgvs = Fgv - Fgvw

      ! View factor from veg to sky and walls below+above canopy
      Fvg = 0.5*(1-Sw_)
      Fvw = 0.5*Sw_ + Fvw

      Fvw = 1 - Fvs - Fvg

      !Fvs = Fsv*fg/min(4*fv,2*fg)
      !Fvg = Fgv*fg/min(4*fv,2*fg)
      !Fvw = 1 - Fvs - Fvg

      ! Canopy mode:
      Fwv = max(fv,0.5*(Fsv+Fgv))*2*fg*Fvw/(4*HL*fb)
      Fwv = min(0.8, Fwv)

      fac1 = 1.*hv/H
      fac2 = 1.*(H-hv)/H
      Fwvw = Fwv/(1 + Fws*fac1/Fww + Fwg*fac2/Fww)
      Fwvs = Fws*fac1/Fww*Fwvw
      Fwvg = Fwg*fac2/Fww*Fwvw

      ! set upper limit
      Fwvw = min(Fww, Fwvw)
      Fwvs = min(Fws, Fwvs)
      Fwvg = min(Fwg, Fwvg)

      Fwv = Fwvw + Fwvs + Fwvg

      ! View factors with trees
      !---------------------------------------------------------
      Fsw_ = Fsw - Fsvw + Fsvw*Td
      Fsg_ = Fsg - Fsvg + Fsvg*Td
      Fgw_ = Fgw - Fgvw + Fgvw*Td
      Fgs_ = Fgs - Fgvs + Fgvs*Td
      Fwg_ = Fwg - Fwvg + Fwvg*Td
      Fww_ = Fww - Fwvw + Fwvw*Td
      Fws_ = Fws - Fwvs + Fwvs*Td

      ! Calculate wall sunlit fraction
      !-------------------------------------------------

      ! Building wall shadow
      Sw = ShadowWall_dir(fb/fg, HL, theta)

      Sw_ = Sw; fv_ = fv;

      Sw_ = ShadowWall_dir(fb/fg, (H-hv)/L, theta)
      fv_ = fv - fv*Sw_

      ! Tree shadow (to all area)
      Sv = ShadowTree(fv_, theta)

      ! Overlapped shadow between tree and building
      ! (to ground only)
      Swv = (Sw-Sw_) * Sv

      ! convert Sv to ground ratio
      Sv = min(1., Sv/fg)

      ! robust check
      IF (Sw+Sv-Swv > 1) THEN
         Swv = Sw+Sv-1
      ENDIF

      ! Calibrated building ground shadow
      Sw = Sw - Swv

      ! Sunlit/shaded wall fraction
      fwsun = 0.5 * (Sw*fg+fb) / (4/PI*fb*HL*tan(theta) + fb)
      fwsha = 1. - fwsun

      ! Calculate radiation transfer matrix
      !   AX = B
      !-------------------------------------------------
      A(1,:) = (/1-Fww_*fwsun*(1-ewall),  -Fww_*fwsun*(1-ewall), -Fgw_*fwsun*(1-ewall), &
                  -Fgw_*fwsun*(1-ewall),  -Fvw *fwsun*(1-ewall) /)

      A(2,:) = (/ -Fww_*fwsha*(1-ewall), 1-Fww_*fwsha*(1-ewall), -Fgw_*fwsha*(1-ewall), &
                  -Fgw_*fwsha*(1-ewall),  -Fvw *fwsha*(1-ewall) /)

      A(3,:) = (/ -Fwg_*fgimp*(1-egimp),  -Fwg_*fgimp*(1-egimp),                 1._r8, &
                                  0._r8,  -Fvg *fgimp*(1-egimp) /)

      A(4,:) = (/ -Fwg_*fgper*(1-egper),  -Fwg_*fgper*(1-egper),                 0._r8, &
                                  1._r8,  -Fvg *fgper*(1-egper) /)

      A(5,:) = (/                 0._r8,                  0._r8,                 0._r8, &
                                  0._r8,                  1._r8 /)

      ! Inverse of matrix A
      Ainv = MatrixInverse(A)

      ! Incident LW radiation on sunlit/shaded wall and
      ! impervious/pervious ground
      Iwsun = LW*Fsw_*fwsun
      Iwsha = LW*Fsw_*fwsha
      Ig    = LW*Fsg_
      Igimp = Ig*fgimp
      Igper = Ig*fgper
      Iv    = LW*Fsv

      ! Vector of initial LW radiation on each surface
      !NOTE: for 3D, absorption per unit area: 4*HL*fb/fg
      !      for canyon: absorption per unit area: 2*HW
      B(1) = Iwsun*(1.-ewall) + 4*fwsun*HL*fb/fg*stefnc*ewall*twsun**4
      B(2) = Iwsha*(1.-ewall) + 4*fwsha*HL*fb/fg*stefnc*ewall*twsha**4
      B(3) = Igimp*(1.-egimp) + fgimp*stefnc*egimp*tgimp**4
      B(4) = Igper*(1.-egper) + fgper*stefnc*egper*tgper**4
      ! leaf temperature iteration in urban flux calculation
      ! see MOD_Urban_Flux.F90
      ! B(5) = 4*fv/fg*stefnc*ev*tl**4 !NOTE: 4*fv/fg or 2*fv/fg
                                       !4*fv/fg. equivalent to 2fc
      B(5) = max(2*fv/fg,Fsv+Fgv)*stefnc*ev

      B1(1) = 4*fwsun*HL*fb/fg*stefnc*ewall*twsun**4
      B1(2) = 4*fwsha*HL*fb/fg*stefnc*ewall*twsha**4
      B1(3) = fgimp*stefnc*egimp*tgimp**4
      B1(4) = fgper*stefnc*egper*tgper**4
      ! leaf temperature iteration in urban flux calculation
      ! B1(5) = 4*fv/fg*stefnc*ev*tl**4
      B1(5) = max(2*fv/fg,Fsv+Fgv)*stefnc*ev

      dBdT(1) = 16*fwsun*HL*fb/fg*stefnc*ewall*twsun**3
      dBdT(2) = 16*fwsha*HL*fb/fg*stefnc*ewall*twsha**3
      dBdT(3) = 4*fgimp*stefnc*egimp*tgimp**3
      dBdT(4) = 4*fgper*stefnc*egper*tgper**3
      ! leaf temperature iteration in urban flux calculation
      ! dBdT(5) = 16*fv/fg*stefnc*ev*tl**3
      dBdT(5) = 4*max(2*fv/fg,Fsv+Fgv)*stefnc*ev

      SkyVF(1:2) = Fws_
      SkyVF(3:4) = Fgs_
      SkyVF(5)   = Fvs

      VegVF(1:2) = Fwv
      VegVF(3:4) = Fgv
      VegVF(5)   = Fsv

      fcover(0) = fb
      fcover(1) = 4*fwsun*HL*fb
      fcover(2) = 4*fwsha*HL*fb
      fcover(3) = fg*fgimp
      fcover(4) = fg*fgper
      fcover(5) = fv

      !NOTE: the below codes are put in the leaf temperature iteration process
      ! after each iteration, update the below iterms
      !B(5)    = 4*fv/fg*stefnc*ev*tl**4
      !B1(5)   = 4*fv/fg*stefnc*ev*tl**4
      !dBdT(5) = 16*fv/fg*stefnc*ev*tl**3
      ! Equation solve
      !X = matmul(Ainv, B)

      ! LW radiation absorption by each surface (per m^2)
      !lwsun = ( ewall*X(1) - B1(1) ) / (1-ewall) !/ (4*fwsun*HL*fb/fg)
      !lwsha = ( ewall*X(2) - B1(2) ) / (1-ewall) !/ (4*fwsha*HL*fb/fg)
      !lgimp = ( egimp*X(3) - B1(3) ) / (1-egimp) !/ fgimp
      !lgper = ( egper*X(4) - B1(4) ) / (1-egper) !/ fgper

      !NOTE: before leaf temperature iteration
      !lv    = ((X(1)*Fwv + X(2)*Fwv + X(3)*Fgv + X(4)*Fgv + LW*Fsv)*ev - B1(5))!/(fv/fg)

      ! Out-going LW of urban canopy
      !SkyVF(1:2) = Fws_; SkyVF(3:4) = Fgs_; SkyVF(5) = Fvs
      !lout = X(1)*Fws_ + X(2)*Fws_ + X(3)*Fgs_ + X(4)*Fgs_ + X(5)*Fvs
      !lout = sum( X * SkyVF )

      ! Energy balance check
      !eb = lwsun + lwsha + lgimp + lgper + lv + lout

      !IF (abs(eb-LW) > 1e-6) THEN
      !  print *, "Longwave tree - Energy Balance Check error!", eb-LW
      !ENDIF

      ! Radiation difference due to the last temperature change of the leaf
      ! dBdT: the first 4 iterms is 0
      !dX = matmul(Ainv, dBdT)
      ! Finally solve the first 4 items, the leaf has been solved
      !lwsun = lwsun + ( ewall*dX(1) ) / (1-ewall) * dtl!/ (4*fwsun*HL*fb/fg)
      !lwsha = lwsha + ( ewall*dX(2) ) / (1-ewall) * dtl!/ (4*fwsha*HL*fb/fg)
      !lgimp = lwimp + ( egimp*dX(3) ) / (1-egimp) * dtl!/ fgimp
      !lgper = lgper + ( egper*dX(4) ) / (1-egper) * dtl!/ fgper

      ! update after each temperature iteration
      !lv    = lv + ((dX(1)*Fwv + dX(2)*Fwv + dX(3)*Fgv + dX(4)*Fgv)*ev - dBdT(5))*dtl!/(fv/fg)
      !dlvdt = (dX(1)*Fwv + dX(2)*Fwv + dX(3)*Fgv + dX(4)*Fgv)*ev - dBdT(5)

      !SkyVF(1:2) = Fws_; SkyVF(3:4) = Fgs_; SkyVF(5) = Fvs
      !lout  = lout + sum( dX * SkyVF * dtl )

      ! put it outside
      ! absorption change due to temperature change, as restart variables.
      ! now the leaf temperature does not change, the last iterm of dBdT is 0.
      !dX = matmul(Ainv, dBdT*dT)

      !lwsun = ( ewall*dX(1) - dBdT(1)*dT(1) ) / (1-ewall) !/ (4*fwsun*HL*fb/fg)
      !lwsha = ( ewall*dX(2) - dBdT(2)*dT(2) ) / (1-ewall) !/ (4*fwsha*HL*fb/fg)
      !lgimp = ( egimp*dX(3) - dBdT(3)*dT(3) ) / (1-egimp) !/ fgimp
      !lgper = ( esnow*dX(4) - dBdT(4)*dT(4) ) / (1-esnow) !/ fgper
      !lv    = ((dX(1)*Fwv + dX(2)*Fwv + dX(3)*Fgv + dX(4)*Fgv + dX(5)*Fgv)*ev)!/(fv/fg)

      !lout = X(1)*Fws_ + X(2)*Fws_ + X(3)*Fgs_ + X(4)*Fgs_ + X(5)*Fvs
      !SkyVF(1:2) = Fws_; SkyVF(3:4) = Fgs_; SkyVF(5) = Fvs
      !lout  = lout + sum( dX * SkyVF )

   END SUBROUTINE UrbanVegLongwave

END MODULE MOD_Urban_Longwave
! ---------- EOP ------------
