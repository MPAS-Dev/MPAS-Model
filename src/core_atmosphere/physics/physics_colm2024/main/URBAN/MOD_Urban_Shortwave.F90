#include <define.h>

MODULE MOD_Urban_Shortwave

   USE MOD_Precision
   USE MOD_LandUrban
   USE MOD_Vars_Global
   USE MOD_3DCanopyRadiation, only: tee, phi
   USE MOD_MPAS_MPI

   IMPLICIT NONE
   SAVE
   PRIVATE

   PUBLIC :: UrbanOnlyShortwave      !Radiation transfer for shortwave radiation without trees
   PUBLIC :: UrbanVegShortwave       !Radiation transfer for shortwave radiation with trees

   PUBLIC :: MatrixInverse           !Inverse of radiation transfer matrix for multiple reflections
   PUBLIC :: ShadowWall_dir          !Shadow of wall for direct radiation
   PUBLIC :: ShadowWall_dif          !Shadow of wall for diffuse radiation
   PUBLIC :: ShadowTree              !Shadow of trees

CONTAINS


   SUBROUTINE UrbanOnlyShortwave ( theta, HL, fb, fgper, H, &
        aroof, awall, agimp, agper, fwsun, sroof, swsun, swsha, sgimp, sgper, albu)

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
!  Calculate the ground shadow area, the area of the sunny and shady
!  walls taking into account mutual shading between buildings;
!  calculate the visibility factor F between the sky, walls, and
!  ground; calculate the initial radiation reaching each component
!  surface, considering multiple scattering processes, and establish
!  the radiation transfer balance equation for both incident direct
!  and diffuse radiation cases for solving.
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
        HL,         &! Ratio of building height to their side length [-]
        fb,         &! Fraction of building area [-]
        fgper,      &! Fraction of impervious ground [-]
        H            ! Building average height [m]

   real(r8), intent(in) :: &
        aroof,      &! albedo of roof [-]
        awall,      &! albedo of walls [-]
        agimp,      &! albedo of impervious road [-]
        agper        ! albedo of pervious road [-]

   real(r8), intent(out) :: &
        fwsun,      &! Fraction of sunlit wall [-]
        sroof(2),   &! Urban building roof absorption [-]
        swsun(2),   &! Urban sunlit wall absorption [-]
        swsha(2),   &! Urban shaded wall absorption [-]
        sgimp(2),   &! Urban impervious ground absorption [-]
        sgper(2),   &! Urban pervious ground absorption [-]
        albu(2)      ! Urban overall albedo [-]

!-------------------------- Local Variables ----------------------------
   real(r8) ::    &
        W,          &! Urban ground average width [m]
        L,          &! Urban building average length [m]
        HW,         &! Ratio of H to W, H/W [-]
        fg,         &! Fraction of ground [-]
        fgimp,      &! Weight of pervious ground [-]

        Fsw,        &! View factor from sky to wall [-]
        Fsg,        &! View factor from sky to ground [-]
        Fgw,        &! View factor from ground to wall [-]
        Fgs,        &! View factor from ground to sky [-]
        Fww,        &! View factor from wall to wall [-]
        Fwg,        &! View factor from wall to ground [-]
        Fws,        &! View factor from wall to sky [-]

        Sw,         &! Shadow of wall [-]
        fwsha,      &! Fraction of shaded wall [-]
        Ewsun,      &! Incident radiation on sunlit wall [-]
        Ewsha,      &! Incident radiation on shaded wall [-]
        Eg,         &! Incident radiation on ground [-]
        Egimp,      &! Incident radiation on impervious ground [-]
        Egper,      &! Incident radiation on pervious ground [-]

        A(4,4),     &! Radiation transfer matrix
        Ainv(4,4),  &! Inverse of Radiation transfer matrix
        B(4),       &! Vectors of incident radiation on each surface
        X(4)         ! Radiation emit from each surface in balance condition

   ! Temporal
   real(r8) :: fac1, fac2, eb

!-----------------------------------------------------------------------

      ! Calculate urban structure parameters
      !-------------------------------------------------
      !W  = H/HW
      !L  = W*sqrt(fb)/(1-sqrt(fb))
      !HL = H/L !NOTE: Same as: HL = HW*(1-sqrt(fb))/sqrt(fb)
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
      Fws = Fsw*fg/fb/(2*HL)*0.75
      Fwg = Fsw*fg/fb/(2*HL)*0.25
      Fww = 1 - Fws - Fwg

      ! Calculate sunlit wall fraction
      !-------------------------------------------------

      ! Building wall shadow on the ground
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
      A(1,:) = (/1-Fww*fwsun*awall,  -Fww*fwsun*awall, -Fgw*fwsun*awall, -Fgw*fwsun*awall/)
      A(2,:) = (/ -Fww*fwsha*awall, 1-Fww*fwsha*awall, -Fgw*fwsha*awall, -Fgw*fwsha*awall/)
      A(3,:) = (/ -Fwg*fgimp*agimp,  -Fwg*fgimp*agimp,            1._r8,            0._r8/)
      A(4,:) = (/ -Fwg*fgper*agper,  -Fwg*fgper*agper,            0._r8,            1._r8/)

      ! Inverse of matrix A
      Ainv = MatrixInverse(A)

      ! Radiation transfer for incident direct case
      !-------------------------------------------------

      ! Incident radiation on sunlit/shaded wall and
      ! impervious/pervious ground
      Ewsun = Sw
      Ewsha = 0.
      Eg    = 1.-Ewsun
      Egimp = Eg*fgimp
      Egper = Eg*fgper

      ! Vector of first scattering radiation on each surface
      B(:) = (/Ewsun*awall, Ewsha*awall, Egimp*agimp, Egper*agper/)

      ! Matrix computing to resolve multiple reflections
      X = matmul(Ainv, B)

      !-------------------------------------------------
      ! SAVE results for output
      !-------------------------------------------------

      ! Radiation absorption by each surface
      !NOTE: for 3D, absorption per unit area: 4*HL*fb/fg
      !      for canyon: absorption per unit area: 2*HW
      swsun(1) = X(1)/awall*(1-awall)!/(4*fwsun*HL*fb/fg)
      swsha(1) = X(2)/awall*(1-awall)!/(4*fwsha*HL*fb/fg)
      sgimp(1) = X(3)/agimp*(1-agimp)!/fgimp
      sgper(1) = X(4)/agper*(1-agper)!/fgper

      ! albedo of urban canopy
      albu(1) = X(1)*Fws + X(2)*Fws + X(3)*Fgs + X(4)*Fgs

      ! Energy balance check
      eb = swsun(1) + swsha(1) + sgimp(1) + sgper(1) + albu(1)
      IF (abs(eb-1) > 1e-6) THEN
         print *, "Direct - Energy Balance Check error!", eb-1
      ENDIF

      ! Radiation transfer for incident diffuse case
      !-------------------------------------------------

      ! Incident radiation on sunlit/shaded wall and
      ! impervious/pervious ground
      Ewsun = Fsw*fwsun
      Ewsha = Fsw*fwsha
      Eg    = Fsg
      Egimp = Eg*fgimp
      Egper = Eg*fgper

      ! Vector of first scattering radiation on each surface
      B(:) = (/Ewsun*awall, Ewsha*awall, Egimp*agimp, Egper*agper/)

      ! Equation solve
      X = matmul(Ainv, B)

      ! Radiation absorption by each surface
      !NOTE: for 3D, absorption per unit area: 4*HL*fb/fg
      !      for canyon: absorption per unit area: 2*HW
      swsun(2) = X(1)/awall*(1-awall)!/(4*fwsun*HL*fb/fg)
      swsha(2) = X(2)/awall*(1-awall)!/(4*fwsha*HL*fb/fg)
      sgimp(2) = X(3)/agimp*(1-agimp)!/fgimp
      sgper(2) = X(4)/agper*(1-agper)!/fgper

      !albedo of urban canopy
      albu(2) = X(1)*Fws + X(2)*Fws + X(3)*Fgs + X(4)*Fgs

      ! energy balance check
      eb = swsun(2) + swsha(2) + sgimp(2) + sgper(2) + albu(2)
      IF (abs(eb-1) > 1e-6) THEN
         print *, "Diffuse - Energy Balance Check error!", eb-1
      ENDIF

      ! convert to per unit area absorption
      IF (fb > 0.) THEN
         swsun = swsun/(4*fwsun*HL*fb)*fg
         swsha = swsha/(4*fwsha*HL*fb)*fg
      ENDIF
      IF (fgimp > 0.) sgimp = sgimp/fgimp
      IF (fgper > 0.) sgper = sgper/fgper

      ! roof absorption
      sroof = 1. - aroof

      ! albedo account for both roof and urban's wall and ground
      albu = aroof*fb + albu*fg

   END SUBROUTINE UrbanOnlyShortwave


   SUBROUTINE UrbanVegShortwave ( theta, HL, fb, fgper, H, &
         aroof, awall, agimp, agper, lai, sai, fv, hv, rho, tau, &
         fwsun, sroof, swsun, swsha, sgimp, sgper, sveg, albu )

!-----------------------------------------------------------------------
!                Sun
!                 \\\
!                  \\\
!                         ______
!                        |++++++|              roof
!                        |++++++|             ______
!                        |++++++|    ___     |++++++|
!                    ______+++++|   |||||    |++++++|
!                   |++++++|++++|  |||||||   |++++++|
!            sunlit |[]++[]|++++|   |||||    |++++++| shaded
!             wall  |++++++|          | tree |++++++|  wall
!                   |[]++[]|          |      |++++++|
!                   |++++++|  impervious/pervious ground
!         __________|++++++|____________________________________
!
!
! !DESCRIPTION:
!
!  The process of shortwave radiation transfer in a city considering
!  vegetation (trees only) is based on the radiation transfer without
!  vegetation (UrbanOnlyShortwave), taking into account the visibility
!  factors F between the various components including the vegetation, in
!  order to calculate the radiation transfer matrix during radiation
!  balance. A similar method is used to solve the radiation absorption
!  of walls, ground, and vegetation. The additional part compared to
!  urban radiation transfer without vegetation (UrbanOnlyShortwave) is
!  the consideration of the visibility factors and shadow area
!  calculation including the vegetation.
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
        HL,         &! Ratio of building height to their side length [-]
        fb,         &! Fraction of building area [-]
        fgper,      &! Fraction of impervious ground [-]
        H            ! Building average height [m]

   real(r8), intent(in) :: &
        aroof,      &! albedo of roof [-]
        awall,      &! albedo of walls [-]
        agimp,      &! albedo of impervious road [-]
        agper        ! albedo of pervious road [-]

   real(r8), intent(in) :: &
        lai,        &! leaf area index
        sai,        &! stem area index
        fv,         &! Fraction of tree cover [-]
        hv,         &! Central height of vegetation crown
        rho,        &! effective rho (lai + sai)
        tau          ! effective tau (lai + sai)

   real(r8), intent(out) :: &
        fwsun,      &! Fraction of sunlit wall [-]
        sroof(2),   &! Urban building roof absorption [-]
        swsun(2),   &! Urban sunlit wall absorption [-]
        swsha(2),   &! Urban shaded wall absorption [-]
        sgimp(2),   &! Urban impervious ground absorption [-]
        sgper(2),   &! Urban pervious ground absorption [-]
        sveg(2),    &! Urban building tree absorption [-]
        albu(2)      ! Urban overall albedo [-]

!-------------------------- Local Variables ----------------------------
   real(r16),parameter :: DD1=1.0_r16 !quad accuracy real number

   real(r8) :: &
        W,          &! Urban ground average width
        L,          &! Urban building average length
        HW,         &! Ratio of H to W, H/W [-]
        fg,         &! Fraction of ground [-]
        fgimp,      &! Weight of pervious ground [-]

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
        Svw,        &! Overlapped shadow between wall and trees [-]
        fv_,        &! Fraction of trees [-]
        Td,         &! Transmission of tree [-]
        av,         &! albedo of tree [-]
        fwsha,      &! Fraction of shaded wall [-]
        Ewsun,      &! Incident radiation on sunlit wall [-]
        Ewsha,      &! Incident radiation on shaded wall [-]
        Eg,         &! Incident radiation on ground [-]
        Egimp,      &! Incident radiation on impervious ground [-]
        Egper,      &! Incident radiation on pervious ground [-]
        Ev           ! Incident radiation on trees [-]

   ! Radiation transfer matrix and vectors
   !-------------------------------------------------
   real(r8) :: A(5,5)     !Radiation transfer matrix
   real(r8) :: Ainv(5,5)  !Inverse of Radiation transfer matrix
   real(r8) :: B(5)       !Vectors of incident radiation on each surface
   real(r8) :: X(5)       !Radiation emit from each surface in balance condition

   ! Temporal
   real(r8) :: fac1, fac2, eb, sumw, ws, wg, ww

   real(r8) :: phi_tot    !albedo of a single tree
   real(r8) :: phi_dif    !Temporal
   real(r8) :: pa2        !Temporal
   real(r8) :: lsai       !lai+sai
!-----------------------------------------------------------------------

      ! Calculate urban structure parameters
      !-------------------------------------------------
      !W  = H/HW
      !L  = W*sqrt(fb)/(1-sqrt(fb))
      !HL = H/L !NOTE: Same as: HL = HW*(1-sqrt(fb))/sqrt(fb)
      L  = H/HL
      fg = 1. - fb

      fgimp = 1. - fgper

      ! Calculate transmission and albedo of tree
      !-------------------------------------------------
      lsai = (lai+sai)*fv/cos(PI/3)/ShadowTree(fv, PI/3)
      Td = tee(DD1*3/8.*lsai)
      CALL phi(.true., 3/8.*lsai, tau+rho, tau, rho, phi_tot, phi_dif, pa2)
      av = phi_tot

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
      ! adjusted as below:
      Fws = Fsw*fg/fb/(2*HL)*0.75
      Fwg = Fsw*fg/fb/(2*HL)*0.25
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
      Svw = Sv * (Sw-Sw_)

      ! convert Sv to ground ratio
      Sv  = min(1., Sv/fg)

      ! robust check
      IF (Sv+Sw-Svw > 1) THEN
         Svw = Sv+Sw-1
      ENDIF

      ! Calibrated building ground shadow
      Fsv  = Sv
      Fsvw = Svw
      Fsvg = Fsv - Fsvw

      ! View factor from veg to sky and walls above canopy
      Fvs = 0.5*(1-Sw_)
      Fvw = 0.5*Sw_

      Sw_ = ShadowWall_dif(fb/fg, hv/L)
      fv_ = fv - fv*Sw_
      Sv  = ShadowTree(fv_, PI/3)

      ! Overlapped shadow between tree and building
      ! (to ground only)
      Svw = Sv * (Sw-Sw_)

      ! convert Sv to ground ratio
      Sv  = min(1., Sv/fg)

      ! robust check
      IF (Sv+Sw-Svw > 1) THEN
         Svw = Sv+Sw-1
      ENDIF

      ! Calibrated building ground shadow
      Fgv  = Sv
      Fgvw = Svw
      Fgvs = Fgv - Fgvw

      ! View factor from veg to sky and walls below+above canopy
      Fvg = 0.5*(1-Sw_)
      Fvw = 0.5*Sw_ + Fvw

      Fvw = 1 - Fvs - Fvg

      !Fvs = Fsv*fg/min(4*fv,2*fg)
      !Fvg = Fgv*fg/min(4*fv,2*fg)
      !Fvw = 1 - Fvs - Fvg

      !ws  = (phi_tot - phi_dif)/2
      !wg  = (phi_tot + phi_dif)/2
      !ww  = (phi_tot + phi_dif)/2
      !sumw = Fvs*ws + Fvg*wg + Fvw*ww
      !Fvs = Fvs*ws/sumw
      !Fvg = Fvg*wg/sumw
      !Fvw = Fvw*ww/sumw

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

      ! Calculate sunlit wall fraction
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
      Svw = (Sw-Sw_) * Sv

      ! convert Sv to ground ratio
      Sv = min(1., Sv/fg)

      ! robust check
      IF (Sv+Sw-Svw > 1) THEN
         Svw = Sv+Sw-1
      ENDIF

      ! Calibrated building ground shadow
      Sw = Sw - Svw

      ! Sunlit/shaded wall fraction
      fwsun = 0.5 * (Sw*fg + fb) / (4/PI*fb*HL*tan(theta) + fb)
      fwsha = 1. - fwsun

      ! Calculate radiation transfer matrix
      !   AX = B
      !-------------------------------------------------
      A(1,:) = (/1-Fww_*fwsun*awall,  -Fww_*fwsun*awall, &
                  -Fgw_*fwsun*awall,  -Fgw_*fwsun*awall, -Fvw*fwsun*awall/)

      A(2,:) = (/ -Fww_*fwsha*awall, 1-Fww_*fwsha*awall, &
                  -Fgw_*fwsha*awall,  -Fgw_*fwsha*awall, -Fvw*fwsha*awall/)

      A(3,:) = (/ -Fwg_*fgimp*agimp,  -Fwg_*fgimp*agimp, &
                              1._r8,              0._r8, -Fvg*fgimp*agimp/)

      A(4,:) = (/ -Fwg_*fgper*agper,  -Fwg_*fgper*agper, &
                              0._r8,              1._r8, -Fvg*fgper*agper/)

      A(5,:) = (/ -Fwv*av          ,  -Fwv*av          , &
                  -Fgv*av          ,  -Fgv*av          ,            1._r8/)

      ! Inverse of matrix A
      Ainv = MatrixInverse(A)

      ! Radiation transfer for incident direct case
      !-------------------------------------------------

      ! Incident radiation on sunlit/shaded wall and
      ! impervious/pervious ground
      Ewsun = Sw
      Ewsha = Svw*Td
      Eg    = 1-Sw-Sv+(Sv-Svw)*Td
      Egimp = Eg*fgimp
      Egper = Eg*fgper
      Ev    = Sv

      ! Vector of first scattering radiation on each surface
      B(:) = (/Ewsun*awall, Ewsha*awall, Egimp*agimp, Egper*agper, Ev*av/)

      ! Matrix computing to resolve multiple reflections
      X = matmul(Ainv, B)

      !-------------------------------------------------
      ! SAVE results for output
      !-------------------------------------------------

      ! Radiation absorption by each surface
      !NOTE: for 3D, absorption per unit area: 4*HL*fb/fg
      !      for canyon: absorption per unit area: 2*HW
      swsun(1) = X(1)/awall*(1-awall)!/(4*fwsun*HL*fb/fg)
      swsha(1) = X(2)/awall*(1-awall)!/(4*fwsha*HL*fb/fg)
      sgimp(1) = X(3)/agimp*(1-agimp)!/fgimp
      sgper(1) = X(4)/agper*(1-agper)!/fgper
      sveg (1) = X(5)/av   *(1-av-Td)!/(fv/fg)

      ! albedo of urban canopy
      albu(1) = X(1)*Fws_ + X(2)*Fws_ + X(3)*Fgs_ + X(4)*Fgs_ + X(5)*Fvs

      ! Energy balance check
      eb = swsun(1) + swsha(1) + sgimp(1) + sgper(1) + sveg(1) + albu(1)
      IF (abs(eb-1) > 1e-6) THEN
         print *, "Direct tree - Energy Balance Check error!", eb-1
      ENDIF

      ! Radiation transfer for incident diffuse case
      !-------------------------------------------------

      ! Incident radiation on sunlit/shaded wall and
      ! impervious/pervious ground
      Ewsun = Fsw_*fwsun
      Ewsha = Fsw_*fwsha
      Eg    = Fsg_
      Egimp = Eg*fgimp
      Egper = Eg*fgper
      Ev    = Fsv

      ! Vector of first scattering radiation on each surface
      B(:) = (/Ewsun*awall, Ewsha*awall, Egimp*agimp, Egper*agper, Ev*av/)

      ! Equation solve
      X = matmul(Ainv, B)

      ! Radiation absorption by each surface
      !NOTE: for 3D, absorption per unit area: 4*HL*fb/fg
      !      for canyon: absorption per unit area: 2*HW
      swsun(2) = X(1)/awall*(1-awall)!/(4*fwsun*HL*fb/fg)
      swsha(2) = X(2)/awall*(1-awall)!/(4*fwsha*HL*fb/fg)
      sgimp(2) = X(3)/agimp*(1-agimp)!/fgimp
      sgper(2) = X(4)/agper*(1-agper)!/fgper
      sveg (2) = X(5)/   av*(1-av-Td)!/(fv/fg)

      ! albedo of urban canopy
      albu(2) = X(1)*Fws_ + X(2)*Fws_ + X(3)*Fgs_ + X(4)*Fgs_ + X(5)*Fvs

      ! Energy balance check
      eb = swsun(2) + swsha(2) + sgimp(2) + sgper(2) + sveg(2) + albu(2)
      IF (abs(eb-1) > 1e-6) THEN
         print *, "Diffuse tree - Energy Balance Check error!", eb-1
      ENDIF

      ! convert to per unit area absorption
      IF (fb > 0.) THEN
         swsun = swsun/(4*fwsun*HL*fb)*fg
         swsha = swsha/(4*fwsha*HL*fb)*fg
      ENDIF
      IF (fgimp > 0.) sgimp = sgimp/fgimp
      IF (fgper > 0.) sgper = sgper/fgper
      IF (   fv > 0.) sveg  =  sveg/fv*fg

      ! roof absorption
      sroof = 1. - aroof

      ! albedo account for both roof and urban's wall and ground
      albu = aroof*fb + albu*fg

   END SUBROUTINE UrbanVegShortwave

   !-------------------------------------------------
   ! calculate shadow of wall for incident direct radiation
   FUNCTION ShadowWall_dir(f, HL, theta) result(Sw)

   IMPLICIT NONE

   real(r8), intent(in) :: f
   real(r8), intent(in) :: HL
   real(r8), intent(in) :: theta

   real(r8) :: Sw

      Sw = 1 - exp( -4/PI*f*HL*tan(theta) )

   END FUNCTION ShadowWall_dir

   !-------------------------------------------------
   ! calculate shadow of wall for incident diffuse radiation
   FUNCTION ShadowWall_dif(f, HL) result(Sw)

   IMPLICIT NONE

   real(r8), intent(in) :: f
   real(r8), intent(in) :: HL

   real(r8) :: Sw

      Sw = 1 - exp( -4/PI*f*HL*tan( (53-sqrt(f*HL*100))/180*PI ) )

   END FUNCTION ShadowWall_dif

   !-------------------------------------------------
   ! calculate shadow of tree
   FUNCTION ShadowTree(f, theta) result(Sv)

   IMPLICIT NONE

   real(r8), intent(in) :: f
   real(r8), intent(in) :: theta

   real(r8) :: mu
   real(r8) :: Sv

      mu = cos(theta)
      Sv = max( f, (1.-exp(-f/mu))/(1.-f*exp(-1./mu)) )

   END FUNCTION ShadowTree


   !-------------------------------------------------
   ! Returns the inverse of a matrix calculated by finding the LU
   ! decomposition. Depends on LAPACK.
   FUNCTION MatrixInverse(A) result(Ainv)

   IMPLICIT NONE

   real(r8), dimension(:,:), intent(in) :: A
   real(r8), dimension(size(A,1),size(A,2)) :: Ainv
   real(r8), dimension(size(A,1)) :: work !work array for LAPACK
   integer,  dimension(size(A,1)) :: ipiv !pivot indices
   integer :: n, info

   ! External procedures defined in LAPACK
   external DGETRF
   external DGETRI

      ! Store A in Ainv to prevent it from being overwritten by LAPACK
      Ainv = A
      n = size(A,1)

      ! DGETRF computes an LU factorization of a general M-by-N matrix A
      ! using partial pivoting with row interchanges.
      CALL DGETRF(n, n, Ainv, n, ipiv, info)
      IF (info /= 0) THEN
         CALL CoLM_stop('Matrix is numerically singular!')
      ENDIF

      ! DGETRI computes the inverse of a matrix using the LU factorization
      ! computed by DGETRF.
      CALL DGETRI(n, Ainv, n, ipiv, work, n, info)
      IF (info /= 0) THEN
         CALL CoLM_stop('Matrix inversion failed!')
      ENDIF

   END FUNCTION MatrixInverse

END MODULE MOD_Urban_Shortwave
! ---------- EOP ------------
