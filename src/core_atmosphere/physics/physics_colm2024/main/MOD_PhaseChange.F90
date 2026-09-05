#include <define.h>

MODULE MOD_PhaseChange

!-----------------------------------------------------------------------
   USE MOD_Precision
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: meltf
   PUBLIC :: meltf_snicar
   PUBLIC :: meltf_urban

!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------


   SUBROUTINE meltf (patchtype,is_dry_lake,lb,nl_soil,deltim, &
                     fact,brr,hs,hs_soil,hs_snow,fsno,dhsdT, &
                     t_soisno_bef,t_soisno,wliq_soisno,wice_soisno,imelt, &
                     scv,snowdp,sm,xmf,porsl,psi0,&
#ifdef Campbell_SOIL_MODEL
                     bsw,&
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
                     theta_r,alpha_vgm,n_vgm,L_vgm,&
                     sc_vgm,fc_vgm,&
#endif
                     dz)

!-----------------------------------------------------------------------
! !DESCRIPTION:
!  calculation of the phase change within snow and soil layers:
!  (1) check the conditions which the phase change may take place,
!      i.e., the layer temperature is great than the freezing point
!      and the ice mass is not equal to zero (i.e., melting),
!      or layer temperature is less than the freezing point
!      and the liquid water mass is not equal to zero (i.e., freezing);
!  (2) assess the rate of phase change from the energy excess (or deficit)
!      after setting the layer temperature to freezing point;
!  (3) re-adjust the ice and liquid mass, and the layer temperature
!
!  Original author: Yongjiu Dai, /09/1999/, /03/2014/
!
! !REVISIONS:
!  08/2020, Hua Yuan: separate soil/snow heat flux, exclude glacier (3)
!  04/2023, Nan Wei: supercooled soil water is included IF supercool is defined.
!
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_MPAS_MPI
   USE MOD_Hydro_SoilFunction
   USE MOD_Const_Physical, only: tfrz, hfus, grav
   USE MOD_Namelist
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------

    integer, intent(in) :: patchtype                   !land patch type
                                                       !(0=soil,1=urban or built-up,2=wetland,
                                                       !3=land ice, 4=deep lake, 5=shallow lake)
    logical, intent(in) :: is_dry_lake
    integer, intent(in) :: nl_soil                     !upper bound of array (i.e., soil layers)
    integer, intent(in) :: lb                          !lower bound of array (i.e., snl +1)
   real(r8), intent(in) :: deltim                      !time step [second]
   real(r8), intent(in) :: t_soisno_bef(lb:nl_soil)    !temperature at previous time step [K]
   real(r8), intent(in) :: brr (lb:nl_soil)            !
   real(r8), intent(in) :: fact(lb:nl_soil)            !temporary variables
   real(r8), intent(in) :: hs                          !net ground heat flux into the surface
   real(r8), intent(in) :: hs_soil                     !net ground heat flux into the surface soil
   real(r8), intent(in) :: hs_snow                     !net ground heat flux into the surface snow
   real(r8), intent(in) :: fsno                        !snow fractional cover
   real(r8), intent(in) :: dhsdT                       !temperature derivative of "hs"
   real(r8), intent(in) :: porsl(1:nl_soil)            !soil porosity [-]
   real(r8), intent(in) :: psi0 (1:nl_soil)            !soil water suction, negative potential [mm]
#ifdef Campbell_SOIL_MODEL
   real(r8), intent(in) :: bsw(1:nl_soil)              !clapp and hornberger "b" parameter [-]
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
   real(r8), intent(in) :: theta_r  (1:nl_soil), &
                           alpha_vgm(1:nl_soil), &
                           n_vgm    (1:nl_soil), &
                           L_vgm    (1:nl_soil), &
                           sc_vgm   (1:nl_soil), &
                           fc_vgm   (1:nl_soil)
#endif
   real(r8), intent(in) :: dz(1:nl_soil)               !soil layer thickness [m]

   real(r8), intent(inout) :: t_soisno   (lb:nl_soil)  !temperature at current time step [K]
   real(r8), intent(inout) :: wice_soisno(lb:nl_soil)  !ice lens [kg/m2]
   real(r8), intent(inout) :: wliq_soisno(lb:nl_soil)  !liquid water [kg/m2]
   real(r8), intent(inout) :: scv                      !snow mass [kg/m2]
   real(r8), intent(inout) :: snowdp                   !snow depth [m]

   real(r8), intent(out) :: sm                         !rate of snowmelt [mm/s, kg/(m2 s)]
   real(r8), intent(out) :: xmf                        !total latent heat of phase change
    integer, intent(out) :: imelt(lb:nl_soil)          !flag for melting or freezing [-]

!-------------------------- Local Variables ----------------------------
   real(r8) :: hm(lb:nl_soil)        !energy residual [W/m2]
   real(r8) :: xm(lb:nl_soil)        !melting or freezing within a time step [kg/m2]
   real(r8) :: heatr                 !energy residual or loss after melting or freezing
   real(r8) :: temp1                 !temporary variables [kg/m2]
   real(r8) :: temp2                 !temporary variables [kg/m2]
   real(r8) :: smp
   real(r8) :: supercool(1:nl_soil)  !the maximum liquid water when soil T below the tfrz [mm3/mm3]
   real(r8), dimension(lb:nl_soil) :: wmass0, wice0, wliq0
   real(r8) :: propor, tinc, we, scvold
   integer j

!-----------------------------------------------------------------------
      sm = 0.
      xmf = 0.
      DO j = lb, nl_soil
         imelt(j) = 0
         hm(j) = 0.
         xm(j) = 0.
         wice0(j) = wice_soisno(j)
         wliq0(j) = wliq_soisno(j)
         wmass0(j) = wice_soisno(j) + wliq_soisno(j)
      ENDDO

      scvold=scv
      we=0.
      IF(lb<=0) we = sum(wice_soisno(lb:0)+wliq_soisno(lb:0))

! supercooling water
      IF (DEF_USE_SUPERCOOL_WATER) THEN
         DO j = 1, nl_soil
            supercool(j) = 0.0
            IF(t_soisno(j) < tfrz .and. ((patchtype <=2) .or. is_dry_lake)) THEN
               smp = hfus * (t_soisno(j)-tfrz)/(grav*t_soisno(j)) * 1000.     ! mm
               IF (porsl(j) > 0.) THEN
#ifdef Campbell_SOIL_MODEL
                  supercool(j) = porsl(j)*(smp/psi0(j))**(-1.0/bsw(j))
#else
                  supercool(j) = soil_vliq_from_psi(smp, porsl(j), theta_r(j), -10.0, 5, &
                     (/alpha_vgm(j), n_vgm(j), L_vgm(j), sc_vgm(j), fc_vgm(j)/))
#endif
               ELSE
                  supercool(j) = 0.
               ENDIF
               supercool(j) = supercool(j)*dz(j)*1000.              ! mm
            ENDIF
         ENDDO
      ENDIF

      DO j = lb, nl_soil
         ! Melting identification
         ! IF ice exists above melt point, melt some to liquid.
         IF(wice_soisno(j) > 0. .and. t_soisno(j) > tfrz)THEN
            imelt(j) = 1
            t_soisno(j) = tfrz
         ENDIF

         ! Freezing identification
         ! IF liquid exists below melt point, freeze some to ice.
         IF(j <= 0)THEN
            IF(wliq_soisno(j) > 0. .and. t_soisno(j) < tfrz) THEN
               imelt(j) = 2
               t_soisno(j) = tfrz
            ENDIF
         ELSE
            IF (DEF_USE_SUPERCOOL_WATER) THEN
               IF(wliq_soisno(j) > supercool(j) .and. t_soisno(j) < tfrz) THEN
                  imelt(j) = 2
                  t_soisno(j) = tfrz
               ENDIF
            ELSE
               IF(wliq_soisno(j) > 0. .and. t_soisno(j) < tfrz) THEN
                  imelt(j) = 2
                  t_soisno(j) = tfrz
               ENDIF
            ENDIF
         ENDIF
      ENDDO

! If snow exists, but its thickness less than the critical value (0.01 m)
      IF(lb == 1 .and. scv > 0.)THEN
         IF(t_soisno(1) > tfrz)THEN
            imelt(1) = 1
            t_soisno(1) = tfrz
         ENDIF
      ENDIF

! Calculate the energy surplus and loss for melting and freezing
      DO j = lb, nl_soil
         IF(imelt(j) > 0)THEN
            tinc = t_soisno(j)-t_soisno_bef(j)

            IF(j > lb)THEN             ! => not the top layer
               IF (j==1 .and. DEF_SPLIT_SOILSNOW .and. ((patchtype<3) .or. is_dry_lake)) THEN
                                       ! -> interface soil layer
                  ! 03/08/2020, yuan: separate soil/snow heat flux, exclude glacier(3)
                  hm(j) = hs_soil + (1.-fsno)*dhsdT*tinc + brr(j) - tinc/fact(j)
               ELSE                    ! -> internal layers other than the interface soil layer
                  hm(j) = brr(j) - tinc/fact(j)
               ENDIF
            ELSE                       ! => top layer
               IF (j==1 .or. (.not.DEF_SPLIT_SOILSNOW) .or. patchtype==3) THEN
                                       ! -> soil layer
                  hm(j) = hs + dhsdT*tinc + brr(j) - tinc/fact(j)
               ELSE                    ! -> snow cover
                  ! 03/08/2020, yuan: separate soil/snow heat flux, exclude glacier(3)
                  hm(j) = hs_snow + fsno*dhsdT*tinc + brr(j) - tinc/fact(j)
               ENDIF
            ENDIF

         ENDIF
      ENDDO

      DO j = lb, nl_soil
         IF(imelt(j) == 1 .and. hm(j) < 0.) THEN
           hm(j) = 0.
           imelt(j) = 0
         ENDIF
! this error was checked carefully, it results from the computed error
! of "Tridiagonal-Matrix" in SUBROUTINE "thermal".
         IF(imelt(j) == 2 .and. hm(j) > 0.) THEN
           hm(j) = 0.
           imelt(j) = 0
         ENDIF
      ENDDO

! The rate of melting and freezing
      DO j = lb, nl_soil
         IF(imelt(j) > 0 .and. abs(hm(j)) > .0) THEN
            xm(j) = hm(j)*deltim/hfus                    ! kg/m2

            ! IF snow exists, but its thickness less than the critical value (1 cm)
            ! Note: more work is need on how to tune the snow depth at this case
            IF(j == 1 .and. lb == 1 .and. scv > 0. .and. xm(j) > 0.)THEN
               temp1 = scv                               ! kg/m2
               scv = max(0.,temp1-xm(j))
               propor = scv/temp1
               snowdp = propor * snowdp
               heatr = hm(j) - hfus*(temp1-scv)/deltim   ! W/m2
               IF(heatr > 0.) THEN
                  xm(j) = heatr*deltim/hfus              ! kg/m2
                  hm(j) = heatr                          ! W/m2
               ELSE
                  xm(j) = 0.
                  hm(j) = 0.
               ENDIF
               sm = max(0.,(temp1-scv))/deltim           ! kg/(m2 s)
               xmf = hfus*sm
            ENDIF

            heatr = 0.
            IF(xm(j) > 0.) THEN
               wice_soisno(j) = max(0., wice0(j)-xm(j))
               heatr = hm(j) - hfus*(wice0(j)-wice_soisno(j))/deltim
            ELSE
               IF(j <= 0) THEN  ! snow
                  wice_soisno(j) = min(wmass0(j), wice0(j)-xm(j))
               ELSE
                  IF (DEF_USE_SUPERCOOL_WATER) THEN
                     IF(wmass0(j) < supercool(j)) THEN
                        wice_soisno(j) = 0.
                     ELSE
                        wice_soisno(j) = min(wmass0(j)-supercool(j), wice0(j)-xm(j))
                     ENDIF
                  ELSE
                     wice_soisno(j) = min(wmass0(j), wice0(j)-xm(j))
                  ENDIF
               ENDIF
               heatr = hm(j) - hfus*(wice0(j)-wice_soisno(j))/deltim
            ENDIF

            wliq_soisno(j) = max(0.,wmass0(j)-wice_soisno(j))

            IF(abs(heatr) > 0.)THEN
               IF(j > lb)THEN             ! => not the top layer
                  IF (j==1 .and. DEF_SPLIT_SOILSNOW .and. ((patchtype<3) .or. is_dry_lake)) THEN
                                          ! -> interface soil layer
                     t_soisno(j) = t_soisno(j) + fact(j)*heatr/(1.-fact(j)*(1.-fsno)*dhsdT)
                  ELSE                    ! -> internal layers other than the interface soil layer
                     t_soisno(j) = t_soisno(j) + fact(j)*heatr
                  ENDIF
               ELSE                       ! => top layer
                  IF (j==1 .or. (.not.DEF_SPLIT_SOILSNOW) .or. patchtype==3) THEN
                                          ! -> soil layer
                     t_soisno(j) = t_soisno(j) + fact(j)*heatr/(1.-fact(j)*dhsdT)
                  ELSE                    ! -> snow cover
                     t_soisno(j) = t_soisno(j) + fact(j)*heatr/(1.-fact(j)*fsno*dhsdT)
                  ENDIF
               ENDIF

               IF (DEF_USE_SUPERCOOL_WATER) THEN
                  IF(j <= 0 .or. patchtype == 3)THEN !snow
                     IF(wliq_soisno(j)*wice_soisno(j) > 0.) t_soisno(j) = tfrz
                  ENDIF
               ELSE
                  IF(wliq_soisno(j)*wice_soisno(j) > 0.) t_soisno(j) = tfrz
               ENDIF
            ENDIF

            xmf = xmf + hfus * (wice0(j)-wice_soisno(j))/deltim

            IF(imelt(j) == 1 .and. j < 1) &
            sm = sm + max(0.,(wice0(j)-wice_soisno(j)))/deltim

         ENDIF
      ENDDO

      !scvold=scv
      IF(lb<=0) THEN
      we = sum(wice_soisno(lb:0)+wliq_soisno(lb:0))-we
         IF(abs(we)>1.e-6) THEN
            print*, 'meltf err : ', we
            CALL CoLM_stop()
         ENDIF
      ENDIF

   END SUBROUTINE meltf


   SUBROUTINE meltf_snicar (patchtype,is_dry_lake,lb,nl_soil,deltim, &
                     fact,brr,hs,hs_soil,hs_snow,fsno,sabg_snow_lyr,dhsdT, &
                     t_soisno_bef,t_soisno,wliq_soisno,wice_soisno,imelt, &
                     scv,snowdp,sm,xmf,porsl,psi0,&
#ifdef Campbell_SOIL_MODEL
                     bsw,&
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
                     theta_r,alpha_vgm,n_vgm,L_vgm,&
                     sc_vgm,fc_vgm,&
#endif
                     dz)

!-----------------------------------------------------------------------
! !DESCRIPTION:
!  calculation of the phase change within snow and soil layers:
!  (1) check the conditions which the phase change may take place,
!      i.e., the layer temperature is great than the freezing point
!      and the ice mass is not equal to zero (i.e., melting),
!      or layer temperature is less than the freezing point
!      and the liquid water mass is not equal to zero (i.e., freezing);
!  (2) assess the rate of phase change from the energy excess (or deficit)
!      after setting the layer temperature to freezing point;
!  (3) re-adjust the ice and liquid mass, and the layer temperature
!
!  Original author: Yongjiu Dai, /09/1999/, /03/2014/
!
! !REVISIONS:
!  08/2020, Hua Yuan: separate soil/snow heat flux, exclude glacier (3)
!  01/2023, Hua Yuan: added snow layer absorption in melting calculation
!  04/2023, Nan Wei: supercooled soil water is included IF supercool is defined.
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_MPAS_MPI
   USE MOD_Hydro_SoilFunction
   USE MOD_Const_Physical, only: tfrz, hfus, grav
   USE MOD_Namelist
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------

    integer, intent(in) :: patchtype                   !land patch type
                                                       !(0=soil,1=urban or built-up,2=wetland,
                                                       !3=land ice, 4=deep lake, 5=shallow lake)
    logical, intent(in) :: is_dry_lake
    integer, intent(in) :: nl_soil                     !upper bound of array (i.e., soil layers)
    integer, intent(in) :: lb                          !lower bound of array (i.e., snl +1)
   real(r8), intent(in) :: deltim                      !time step [second]
   real(r8), intent(in) :: t_soisno_bef(lb:nl_soil)    !temperature at previous time step [K]
   real(r8), intent(in) :: brr (lb:nl_soil)            !
   real(r8), intent(in) :: fact(lb:nl_soil)            !temporary variables
   real(r8), intent(in) :: hs                          !net ground heat flux into the surface
   real(r8), intent(in) :: hs_soil                     !net ground heat flux into the surface soil
   real(r8), intent(in) :: hs_snow                     !net ground heat flux into the surface snow
   real(r8), intent(in) :: fsno                        !snow fractional cover
   real(r8), intent(in) :: dhsdT                       !temperature derivative of "hs"
   real(r8), intent(in) :: sabg_snow_lyr (lb:1)        !snow layer absorption [W/m-2]
   real(r8), intent(in) :: porsl(1:nl_soil)            !soil porosity [-]
   real(r8), intent(in) :: psi0 (1:nl_soil)            !soil water suction, negative potential [mm]
#ifdef Campbell_SOIL_MODEL
   real(r8), intent(in) :: bsw(1:nl_soil)              !clapp and hornberger "b" parameter [-]
#endif
#ifdef vanGenuchten_Mualem_SOIL_MODEL
   real(r8), intent(in) :: theta_r  (1:nl_soil), &
                           alpha_vgm(1:nl_soil), &
                           n_vgm    (1:nl_soil), &
                           L_vgm    (1:nl_soil), &
                           sc_vgm   (1:nl_soil), &
                           fc_vgm   (1:nl_soil)
#endif
   real(r8), intent(in) :: dz(1:nl_soil)               !soil layer thickness [m]

   real(r8), intent(inout) :: t_soisno (lb:nl_soil)    !temperature at current time step [K]
   real(r8), intent(inout) :: wice_soisno(lb:nl_soil)  !ice lens [kg/m2]
   real(r8), intent(inout) :: wliq_soisno(lb:nl_soil)  !liquid water [kg/m2]
   real(r8), intent(inout) :: scv                      !snow mass [kg/m2]
   real(r8), intent(inout) :: snowdp                   !snow depth [m]

   real(r8), intent(out) :: sm                         !rate of snowmelt [mm/s, kg/(m2 s)]
   real(r8), intent(out) :: xmf                        !total latent heat of phase change
    integer, intent(out) :: imelt(lb:nl_soil)          !flag for melting or freezing [-]

!-------------------------- Local Variables ----------------------------
   real(r8) :: hm(lb:nl_soil)        !energy residual [W/m2]
   real(r8) :: xm(lb:nl_soil)        !melting or freezing within a time step [kg/m2]
   real(r8) :: heatr                 !energy residual or loss after melting or freezing
   real(r8) :: temp1                 !temporary variables [kg/m2]
   real(r8) :: temp2                 !temporary variables [kg/m2]
   real(r8) :: smp
   real(r8) :: supercool(1:nl_soil)  !the maximum liquid water when soil T below the tfrz [mm3/mm3]
   real(r8), dimension(lb:nl_soil) :: wmass0, wice0, wliq0
   real(r8) :: propor, tinc, we, scvold
   integer j

!-----------------------------------------------------------------------

      sm = 0.
      xmf = 0.
      DO j = lb, nl_soil
         imelt(j) = 0
         hm(j) = 0.
         xm(j) = 0.
         wice0(j) = wice_soisno(j)
         wliq0(j) = wliq_soisno(j)
         wmass0(j) = wice_soisno(j) + wliq_soisno(j)
      ENDDO

      scvold=scv
      we=0.
      IF(lb<=0) we = sum(wice_soisno(lb:0)+wliq_soisno(lb:0))

! supercooling water
      IF (DEF_USE_SUPERCOOL_WATER) THEN
         DO j = 1, nl_soil
            supercool(j) = 0.0
            IF(t_soisno(j) < tfrz .and. ((patchtype <= 2) .or. is_dry_lake)) THEN
               smp = hfus * (t_soisno(j)-tfrz)/(grav*t_soisno(j)) * 1000.     ! mm
               IF (porsl(j) > 0.) THEN
#ifdef Campbell_SOIL_MODEL
                  supercool(j) = porsl(j)*(smp/psi0(j))**(-1.0/bsw(j))
#else
                  supercool(j) = soil_vliq_from_psi(smp, porsl(j), theta_r(j), -10.0, 5, &
                     (/alpha_vgm(j), n_vgm(j), L_vgm(j), sc_vgm(j), fc_vgm(j)/))
#endif
               ELSE
                  supercool(j) = 0.
               ENDIF
               supercool(j) = supercool(j)*dz(j)*1000.              ! mm
            ENDIF
         ENDDO
      ENDIF


      DO j = lb, nl_soil
         ! Melting identification
         ! IF ice exists above melt point, melt some to liquid.
         IF(wice_soisno(j) > 0. .and. t_soisno(j) > tfrz)THEN
            imelt(j) = 1
            t_soisno(j) = tfrz
         ENDIF

         ! Freezing identification
         ! IF liquid exists below melt point, freeze some to ice.
         IF(j <= 0)THEN
            IF(wliq_soisno(j) > 0. .and. t_soisno(j) < tfrz) THEN
               imelt(j) = 2
               t_soisno(j) = tfrz
            ENDIF
         ELSE
            IF (DEF_USE_SUPERCOOL_WATER) THEN
               IF(wliq_soisno(j) > supercool(j) .and. t_soisno(j) < tfrz) THEN
                  imelt(j) = 2
                  t_soisno(j) = tfrz
               ENDIF
            ELSE
               IF(wliq_soisno(j) > 0. .and. t_soisno(j) < tfrz) THEN
                  imelt(j) = 2
                  t_soisno(j) = tfrz
               ENDIF
            ENDIF
         ENDIF
      ENDDO

! If snow exists, but its thickness less than the critical value (0.01 m)
      IF(lb == 1 .and. scv > 0.)THEN
         IF(t_soisno(1) > tfrz)THEN
            imelt(1) = 1
            t_soisno(1) = tfrz
         ENDIF
      ENDIF

! Calculate the energy surplus and loss for melting and freezing
      DO j = lb, nl_soil
         IF(imelt(j) > 0)THEN
            tinc = t_soisno(j)-t_soisno_bef(j)

            IF(j > lb)THEN             ! => not the top layer
               IF (j==1 .and. DEF_SPLIT_SOILSNOW .and. ((patchtype<3).or.is_dry_lake)) THEN
                                       ! -> interface soil layer
                  ! 03/08/2020, yuan: separate soil/snow heat flux, exclude glacier(3)
                  hm(j) = hs_soil + (1.-fsno)*dhsdT*tinc + brr(j) - tinc/fact(j)
               ELSE                    ! -> internal layers other than the interface soil layer
                  IF (j<1 .or. (j==1 .and. patchtype==3)) THEN
                     hm(j) = brr(j) - tinc/fact(j) + sabg_snow_lyr(j)
                  ELSE
                     hm(j) = brr(j) - tinc/fact(j)
                  ENDIF
               ENDIF
            ELSE                       ! => top layer
               IF (j==1 .or. (.not.DEF_SPLIT_SOILSNOW) .or. patchtype==3) THEN
                                       ! -> soil layer
                  hm(j) = hs + dhsdT*tinc + brr(j) - tinc/fact(j)
               ELSE                    ! -> snow cover
                  ! 03/08/2020, yuan: separate soil/snow heat flux, exclude glacier(3)
                  hm(j) = hs_snow + fsno*dhsdT*tinc + brr(j) - tinc/fact(j)
               ENDIF
            ENDIF

         ENDIF
      ENDDO

      DO j = lb, nl_soil
         IF(imelt(j) == 1 .and. hm(j) < 0.) THEN
           hm(j) = 0.
           imelt(j) = 0
         ENDIF
! this error was checked carefully, it results from the computed error
! of "Tridiagonal-Matrix" in SUBROUTINE "thermal".
         IF(imelt(j) == 2 .and. hm(j) > 0.) THEN
           hm(j) = 0.
           imelt(j) = 0
         ENDIF
      ENDDO

! The rate of melting and freezing
      DO j = lb, nl_soil
         IF(imelt(j) > 0 .and. abs(hm(j)) > .0) THEN
            xm(j) = hm(j)*deltim/hfus                    ! kg/m2

            ! IF snow exists, but its thickness less than the critical value (1 cm)
            ! Note: more work is need on how to tune the snow depth at this case
            IF(j == 1 .and. lb == 1 .and. scv > 0. .and. xm(j) > 0.)THEN
               temp1 = scv                               ! kg/m2
               scv = max(0.,temp1-xm(j))
               propor = scv/temp1
               snowdp = propor * snowdp
               heatr = hm(j) - hfus*(temp1-scv)/deltim   ! W/m2
               IF(heatr > 0.) THEN
                  xm(j) = heatr*deltim/hfus              ! kg/m2
                  hm(j) = heatr                          ! W/m2
               ELSE
                  xm(j) = 0.
                  hm(j) = 0.
               ENDIF
               sm = max(0.,(temp1-scv))/deltim           ! kg/(m2 s)
               xmf = hfus*sm
            ENDIF

            heatr = 0.
            IF(xm(j) > 0.) THEN
               wice_soisno(j) = max(0., wice0(j)-xm(j))
               heatr = hm(j) - hfus*(wice0(j)-wice_soisno(j))/deltim
            ELSE
               IF(j <= 0) THEN ! snow
                  wice_soisno(j) = min(wmass0(j), wice0(j)-xm(j))
               ELSE
                  IF (DEF_USE_SUPERCOOL_WATER) THEN
                     IF(wmass0(j) < supercool(j)) THEN
                        wice_soisno(j) = 0.
                     ELSE
                        wice_soisno(j) = min(wmass0(j)-supercool(j), wice0(j)-xm(j))
                     ENDIF
                  ELSE
                     wice_soisno(j) = min(wmass0(j), wice0(j)-xm(j))
                  ENDIF
               ENDIF
               heatr = hm(j) - hfus*(wice0(j)-wice_soisno(j))/deltim
            ENDIF

            wliq_soisno(j) = max(0.,wmass0(j)-wice_soisno(j))

            IF(abs(heatr) > 0.)THEN
               IF(j > lb)THEN             ! => not the top layer
                  IF (j==1 .and. DEF_SPLIT_SOILSNOW .and. ((patchtype<3).or.is_dry_lake)) THEN
                                          ! -> interface soil layer
                     t_soisno(j) = t_soisno(j) + fact(j)*heatr/(1.-fact(j)*(1.-fsno)*dhsdT)
                  ELSE                    ! -> internal layers other than the interface soil layer
                     t_soisno(j) = t_soisno(j) + fact(j)*heatr
                  ENDIF
               ELSE                       ! => top layer
                  IF (j==1 .or. (.not.DEF_SPLIT_SOILSNOW) .or. patchtype==3) THEN
                                          ! -> soil layer
                     t_soisno(j) = t_soisno(j) + fact(j)*heatr/(1.-fact(j)*dhsdT)
                  ELSE                    ! -> snow cover
                     t_soisno(j) = t_soisno(j) + fact(j)*heatr/(1.-fact(j)*fsno*dhsdT)
                  ENDIF
               ENDIF

               IF (DEF_USE_SUPERCOOL_WATER) THEN
                  IF(j <= 0 .or. patchtype == 3)THEN !snow
                     IF(wliq_soisno(j)*wice_soisno(j) > 0.) t_soisno(j) = tfrz
                  ENDIF
               ELSE
                  IF(wliq_soisno(j)*wice_soisno(j) > 0.) t_soisno(j) = tfrz
               ENDIF

            ENDIF

            xmf = xmf + hfus * (wice0(j)-wice_soisno(j))/deltim

            IF(imelt(j) == 1 .and. j < 1) &
            sm = sm + max(0.,(wice0(j)-wice_soisno(j)))/deltim

         ENDIF
      ENDDO

      !scvold=scv
      IF(lb<=0) THEN
      we = sum(wice_soisno(lb:0)+wliq_soisno(lb:0))-we
         IF(abs(we)>1.e-6) THEN
            print*, 'meltf err : ', we
            CALL CoLM_stop()
         ENDIF
      ENDIF

   END SUBROUTINE meltf_snicar

   SUBROUTINE meltf_urban (lb,nl_soil,deltim, &
                   fact,brr,hs,dhsdT, &
                   t_soisno_bef,t_soisno,wliq_soisno,wice_soisno,imelt, &
                   scv,snowdp,sm,xmf)

!-----------------------------------------------------------------------
!
! !DESCRIPTION:
!  calculation of the phase change within snow and soil layers:
!
!  (1) check the conditions which the phase change may take place,
!      i.e., the layer temperature is great than the freezing point
!      and the ice mass is not equal to zero (i.e., melting),
!      or layer temperature is less than the freezing point
!      and the liquid water mass is not equal to zero (i.e., freezing);
!  (2) assess the rate of phase change from the energy excess (or deficit)
!      after setting the layer temperature to freezing point;
!  (3) re-adjust the ice and liquid mass, and the layer temperature
!
!  Original author: Yongjiu Dai, /09/1999/, /03/2014/
!
! !REVISIONS:
!
!-----------------------------------------------------------------------

   USE MOD_Precision
   USE MOD_MPAS_MPI
   USE MOD_Const_Physical, only: tfrz, hfus
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------

    integer, intent(in) :: nl_soil                     !upper bound of array (i.e., soil layers)
    integer, intent(in) :: lb                          !lower bound of array (i.e., snl +1)
   real(r8), intent(in) :: deltim                      !time step [second]
   real(r8), intent(in) :: t_soisno_bef(lb:nl_soil)    !temperature at previous time step [K]
   real(r8), intent(in) :: brr (lb:nl_soil)            !
   real(r8), intent(in) :: fact(lb:nl_soil)            !temporary variables
   real(r8), intent(in) :: hs                          !net ground heat flux into the surface
   real(r8), intent(in) :: dhsdT                       !temperature derivative of "hs"

   real(r8), intent(inout) :: t_soisno (lb:nl_soil)    !temperature at current time step [K]
   real(r8), intent(inout) :: wice_soisno(lb:nl_soil)  !ice lens [kg/m2]
   real(r8), intent(inout) :: wliq_soisno(lb:nl_soil)  !liquid water [kg/m2]
   real(r8), intent(inout) :: scv                      !snow mass [kg/m2]
   real(r8), intent(inout) :: snowdp                   !snow depth [m]

   real(r8), intent(out) :: sm                         !rate of snowmelt [mm/s, kg/(m2 s)]
   real(r8), intent(out) :: xmf                        !total latent heat of phase change
   integer,  intent(out) :: imelt(lb:nl_soil)          !flag for melting or freezing [-]

!-------------------------- Local Variables ----------------------------
   real(r8) :: hm(lb:nl_soil)  !energy residual [W/m2]
   real(r8) :: xm(lb:nl_soil)  !melting or freezing within a time step [kg/m2]
   real(r8) :: heatr           !energy residual or loss after melting or freezing
   real(r8) :: temp1           !temporary variables [kg/m2]
   real(r8) :: temp2           !temporary variables [kg/m2]

   real(r8), dimension(lb:nl_soil) :: wmass0, wice0, wliq0
   real(r8) :: propor, tinc, we, scvold
   integer j

!-----------------------------------------------------------------------

      sm = 0.
      xmf = 0.
      DO j = lb, nl_soil
         imelt(j) = 0
         hm(j) = 0.
         xm(j) = 0.
         wice0(j) = wice_soisno(j)
         wliq0(j) = wliq_soisno(j)
         wmass0(j) = wice_soisno(j) + wliq_soisno(j)
      ENDDO

      scvold=scv
      we=0.
      IF(lb<=0) we = sum(wice_soisno(lb:0)+wliq_soisno(lb:0))

      DO j = lb, nl_soil
         ! Melting identification
         ! IF ice exists above melt point, melt some to liquid.
         IF(wice_soisno(j) > 0. .and. t_soisno(j) > tfrz)THEN
            imelt(j) = 1
            t_soisno(j) = tfrz
         ENDIF

      ! Freezing identification
      ! IF liquid exists below melt point, freeze some to ice.
         IF(wliq_soisno(j) > 0. .and. t_soisno(j) < tfrz) THEN
            imelt(j) = 2
            t_soisno(j) = tfrz
         ENDIF
      ENDDO

! If snow exists, but its thickness less than the critical value (0.01 m)
      IF(lb == 1 .and. scv > 0.)THEN
         IF(t_soisno(1) > tfrz)THEN
            imelt(1) = 1
            t_soisno(1) = tfrz
         ENDIF
      ENDIF

! Calculate the energy surplus and loss for melting and freezing
      DO j = lb, nl_soil
         IF(imelt(j) > 0)THEN
            tinc = t_soisno(j)-t_soisno_bef(j)
            IF(j > lb)THEN
               hm(j) = brr(j) - tinc/fact(j)
            ELSE
               hm(j) = hs + dhsdT*tinc + brr(j) - tinc/fact(j)
            ENDIF
         ENDIF
      ENDDO

      DO j = lb, nl_soil
         IF(imelt(j) == 1 .and. hm(j) < 0.) THEN
            hm(j) = 0.
            imelt(j) = 0
         ENDIF
! this error was checked carefully, it results from the computed error
! of "Tridiagonal-Matrix" in SUBROUTINE "thermal".
         IF(imelt(j) == 2 .and. hm(j) > 0.) THEN
            hm(j) = 0.
            imelt(j) = 0
         ENDIF
      ENDDO

! The rate of melting and freezing
      DO j = lb, nl_soil
         IF(imelt(j) > 0 .and. abs(hm(j)) > .0) THEN
            xm(j) = hm(j)*deltim/hfus                    ! kg/m2

         ! IF snow exists, but its thickness less than the critical value (1 cm)
         ! Note: more work is need on how to tune the snow depth at this case
            IF(j == 1 .and. lb == 1 .and. scv > 0. .and. xm(j) > 0.)THEN
               temp1 = scv                               ! kg/m2
               scv = max(0.,temp1-xm(j))
               propor = scv/temp1
               snowdp = propor * snowdp
               heatr = hm(j) - hfus*(temp1-scv)/deltim   ! W/m2
               IF(heatr > 0.) THEN
                  xm(j) = heatr*deltim/hfus              ! kg/m2
                  hm(j) = heatr                          ! W/m2
               ELSE
                  xm(j) = 0.
                  hm(j) = 0.
               ENDIF
               sm = max(0.,(temp1-scv))/deltim           ! kg/(m2 s)
               xmf = hfus*sm
            ENDIF

            heatr = 0.
            IF(xm(j) > 0.) THEN
               wice_soisno(j) = max(0., wice0(j)-xm(j))
               heatr = hm(j) - hfus*(wice0(j)-wice_soisno(j))/deltim
            ELSE
               wice_soisno(j) = min(wmass0(j), wice0(j)-xm(j))
               heatr = hm(j) - hfus*(wice0(j)-wice_soisno(j))/deltim
            ENDIF

            wliq_soisno(j) = max(0.,wmass0(j)-wice_soisno(j))

            IF(abs(heatr) > 0.)THEN
               IF(j > lb)THEN
                  t_soisno(j) = t_soisno(j) + fact(j)*heatr
               ELSE
                  t_soisno(j) = t_soisno(j) + fact(j)*heatr/(1.-fact(j)*dhsdT)
               ENDIF
               IF(wliq_soisno(j)*wice_soisno(j) > 0.) t_soisno(j) = tfrz
            ENDIF

            xmf = xmf + hfus * (wice0(j)-wice_soisno(j))/deltim

            IF(imelt(j) == 1 .and. j < 1) &
            sm = sm + max(0.,(wice0(j)-wice_soisno(j)))/deltim

         ENDIF
      ENDDO

  !scvold=scv
      IF(lb<=0) THEN
         we = sum(wice_soisno(lb:0)+wliq_soisno(lb:0))-we
         IF(abs(we)>1.e-6) THEN
            print*, 'meltf err : ', we
            CALL CoLM_stop()
         ENDIF
      ENDIF

   END SUBROUTINE meltf_urban

END MODULE MOD_PhaseChange
! ---------- EOP ------------
