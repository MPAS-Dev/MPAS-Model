MODULE MOD_NewSnow

!-----------------------------------------------------------------------
   USE MOD_Precision
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: newsnow


!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------


   SUBROUTINE newsnow (patchtype,maxsnl,deltim,t_grnd,pg_rain,pg_snow,bifall,&
                       t_precip,zi_soisno,z_soisno,dz_soisno,t_soisno,&
                       wliq_soisno,wice_soisno,fiold,snl,sag,scv,snowdp,fsno,wetwat)

!=======================================================================
!  add new snow nodes.
!  Original author: Yongjiu Dai, 09/15/1999; 08/31/2002, 07/2013, 04/2014
!=======================================================================

   USE MOD_Precision
   USE MOD_Namelist, only: DEF_USE_VariablySaturatedFlow
   USE MOD_Const_Physical, only: tfrz, cpliq, cpice

   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------

   integer, intent(in) :: maxsnl     ! maximum number of snow layers
   integer, intent(in) :: patchtype  ! land patch type (0=soil, 1=urban and built-up,
                                     ! 2=wetland, 3=land ice, 4=land water bodies, 99=ocean)
   real(r8), intent(in) :: deltim    ! model time step [second]
   real(r8), intent(in) :: t_grnd    ! ground surface temperature [k]
   real(r8), intent(in) :: pg_rain   ! rainfall onto ground including canopy runoff [kg/(m2 s)]
   real(r8), intent(in) :: pg_snow   ! snowfall onto ground including canopy runoff [kg/(m2 s)]
   real(r8), intent(in) :: bifall    ! bulk density of newly fallen dry snow [kg/m3]
   real(r8), intent(in) :: t_precip  ! snowfall/rainfall temperature [kelvin]

   real(r8), intent(inout) ::   zi_soisno(maxsnl:0)   ! interface level below a "z" level (m)
   real(r8), intent(inout) ::    z_soisno(maxsnl+1:0) ! layer depth (m)
   real(r8), intent(inout) ::   dz_soisno(maxsnl+1:0) ! layer thickness (m)
   real(r8), intent(inout) ::    t_soisno(maxsnl+1:0) ! soil + snow layer temperature [K]
   real(r8), intent(inout) :: wliq_soisno(maxsnl+1:0) ! liquid water (kg/m2)
   real(r8), intent(inout) :: wice_soisno(maxsnl+1:0) ! ice lens (kg/m2)
   real(r8), intent(inout) :: fiold(maxsnl+1:0)       ! fraction of ice relative to the total water
   integer , intent(inout) :: snl                     ! number of snow layers
   real(r8), intent(inout) :: sag                     ! non dimensional snow age [-]
   real(r8), intent(inout) :: scv                     ! snow mass (kg/m2)
   real(r8), intent(inout) :: snowdp                  ! snow depth (m)
   real(r8), intent(inout) :: fsno                    ! fraction of soil covered by snow [-]

   real(r8), intent(inout), optional :: wetwat        ! wetland water [mm]

!-------------------------- Local Variables ----------------------------

   real(r8) dz_snowf  ! layer thickness rate change due to precipitation [mm/s]
   integer newnode    ! signification when new snow node is set, (1=yes, 0=no)
   integer lb

!-----------------------------------------------------------------------

      newnode = 0

      dz_snowf = pg_snow/bifall
      snowdp = snowdp + dz_snowf*deltim
      scv = scv + pg_snow*deltim              ! snow water equivalent (mm)

      IF(patchtype==2 .and. t_grnd>tfrz)THEN  ! snowfall on warmer wetland
         IF (present(wetwat) .and. DEF_USE_VariablySaturatedFlow) THEN
            wetwat = wetwat + scv
         ENDIF
         scv=0.; snowdp=0.; sag=0.; fsno = 0.
      ENDIF

      zi_soisno(0) = 0.

! when the snow accumulation exceeds 10 mm, initialize a snow layer

      IF(snl==0 .and. pg_snow>0.0 .and. snowdp>=0.01)THEN
         snl = -1
         newnode = 1
         dz_soisno(0)  = snowdp             ! meter
         z_soisno (0)  = -0.5*dz_soisno(0)
         zi_soisno(-1) = -dz_soisno(0)

         sag = 0.                           ! snow age
         t_soisno (0) = min(tfrz, t_precip) ! K
         wice_soisno(0) = scv               ! kg/m2
         wliq_soisno(0) = 0.                ! kg/m2
         fiold(0) = 1.
         fsno = min(1.,tanh(0.1*pg_snow*deltim))
      ENDIF

      ! --------------------------------------------------
      ! snowfall on snow pack
      ! --------------------------------------------------
      ! the change of ice partial density of surface node due to precipitation
      ! only ice part of snowfall is added here, the liquid part will be added latter

      IF(snl<0 .and. newnode==0)THEN
         lb = snl + 1

         wice_soisno(lb) = wice_soisno(lb)+deltim*pg_snow
         dz_soisno(lb) = dz_soisno(lb)+dz_snowf*deltim
         z_soisno(lb) = zi_soisno(lb) - 0.5*dz_soisno(lb)
         zi_soisno(lb-1) = zi_soisno(lb) - dz_soisno(lb)

         ! update fsno by new snow event, add to previous fsno
         ! shape factor for accumulation of snow = 0.1
         fsno = 1. - (1. - tanh(0.1*pg_snow*deltim))*(1. - fsno)
         fsno = min(1., fsno)

      ENDIF

   END SUBROUTINE newsnow

END MODULE MOD_NewSnow
! ---------- EOP ------------
