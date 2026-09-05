#include <define.h>

MODULE MOD_Urban_LUCY
! -----------------------------------------------------------------------
! !DESCRIPTION:
!  Anthropogenic model to calculate anthropogenic heat flux for the rest
!
!  Original: Wenzong Dong, May, 2022
!
! -----------------------------------------------------------------------
! !USE
   USE MOD_Precision
   USE MOD_TimeManager
   USE MOD_Namelist
   USE MOD_Vars_Global
   USE MOD_Const_Physical
   USE MOD_TimeManager
   IMPLICIT NONE
   SAVE
   PUBLIC :: LUCY

CONTAINS

   SUBROUTINE LUCY( idate       , deltim  , patchlonr, fix_holiday, &
                    week_holiday, hum_prof, wdh_prof , weh_prof   , pop_den, &
                    vehicle     , Fahe    , vehc     , meta        )

! -----------------------------------------------------------------------
! !DESCRIPTION:
!  Anthropogenic heat fluxes other than building heat were calculated
!
! !REFERENCES:
!  1) Grimmond, C. S. B. (1992). The suburban energy balance:
!  Methodological considerations and results for a mid-latitude west
!  coast city under winter and spring conditions. International Journal
!  of Climatology, 12(5), 481-497. https://doi.org/10.1002/joc.3370120506
!
!  2) Allen, L., Lindberg, F., & Grimmond, C. S. B. (2011). Global to
!  city scale urban anthropogenic heat flux: Model and variability.
!  International Journal of Climatology, 31(13), 1990-2005.
!  https://doi.org/10.1002/joc.2210
!
! -----------------------------------------------------------------------
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer , intent(in) :: &
      idate(3)           ! calendar (year, julian day, seconds)

   real(r8), intent(in) :: &
      fix_holiday(365) ,&! Fixed public holidays, holiday(0) or workday(1)
      week_holiday(7)    ! week holidays

   real(r8), intent(in) :: &
      deltim           ,&! seconds in a time step [second]
      patchlonr        ,&! longitude of patch [radian]
      hum_prof(24)     ,&! Diurnal metabolic heat profile [W/person]
      wdh_prof(24)     ,&! Diurnal traffic flow profile of weekday
      weh_prof(24)     ,&! Diurnal traffic flow profile of weekend
      pop_den          ,&! population density [person per square kilometer]
      vehicle(3)         ! vehicle numbers per thousand people

   real(r8) :: &
      vehc_prof(24,2)  ,&!
      carscell         ,&! cars numbers per thousand people
      frescell         ,&! freights numbers per thousand people
      mbkscell           ! motobikes numbers per thousand people

   real(r8), intent(out) :: &
      Fahe             ,&! flux from metabolic and vehicle
      vehc             ,&! flux from vehicle
      meta               ! flux from metabolic

   real(r8) :: &
      londeg           ,&! longitude of path [degree]
      car_sp           ,&! distance traveled [km]
      traf_frac        ,&! vehicle heat profile of hour [-]
      meta_prof        ,&! metabolic heat profile of hour [-]
      carflx           ,&! flux from car [W/m2]
      motflx           ,&! flux from motorbike [W/m2]
      freflx             ! flux from freight [W/m2]

!-------------------------- Local Variables ----------------------------
   real(r8):: ldate(3)   ! local time (year, julian day, seconds)
   integer :: &
      iweek            ,&! day of week
      ihour            ,&! hour of day
      day              ,&! day of month
      month            ,&! month of year
      day_inx          ,&! holiday index, day=1(workday), day=1(holiday)
      EC               ,&! emission factor of car [J/m]
      EF               ,&! emission factor of freight [J/m]
      EM                 ! emission factor of motorbike [J/m]

!-----------------------------------------------------------------------

      ! initialization
      meta = 0.
      vehc = 0.
      Fahe = 0.

      ! set vehicle distance traveled
      car_sp = 50

      ! emission factor Sailor and Lu (2004),
      ! all vehicle are set to same value
      EC = 3975
      EM = 3975
      EF = 3975

      IF (DEF_simulation_time%greenwich) THEN
         ! convert GMT time to local time
         londeg = patchlonr*180/PI
         CALL gmt2local(idate, londeg, ldate)
      ENDIF

      vehc_prof(:,1) = wdh_prof
      vehc_prof(:,2) = weh_prof

      CALL julian2monthday(int(ldate(1)), int(ldate(2)), month, day)
      CALL timeweek(int(ldate(1)), month, day, iweek)

      ihour = CEILING(ldate(3)*1./3600)

      IF (day==366)  day=365
      IF (fix_holiday(day)==0 .or. week_holiday(iweek)==0) THEN
         day_inx = 1
      ELSE
         day_inx = 2
      ENDIF

      ! set traffic flow to be used of this time step
      traf_frac = vehc_prof(ihour,day_inx)
      ! set heat release per people of this time step
      meta_prof = hum_prof (ihour)

      carscell = vehicle(1)
      mbkscell = vehicle(2)
      frescell = vehicle(3)

      ! heat release of metabolism [W/m2]
      meta = pop_den*meta_prof/1e6
      ! heat release of cars [W/m2]
      IF (carscell > 0) THEN
         carflx = carscell*pop_den/1000
         carflx = carflx*traf_frac &
                  *EC*(car_sp*1000)/1e6
         carflx = carflx/3600
      ELSE
         carflx = 0.
      ENDIF

      ! heat release of motorbikes [W/m2]
      IF (mbkscell > 0) THEN
         motflx = mbkscell*pop_den/1000
         motflx = motflx*traf_frac &
                  *EM*(car_sp*1000)/1e6
         motflx = motflx/3600
      ELSE
         motflx = 0.
      ENDIF

      ! heat release of freight [W/m2]
      IF (frescell > 0)THEN
         freflx = frescell*pop_den/1000
         freflx = freflx*traf_frac &
                  *EF*(car_sp*1000)/1e6
         freflx = freflx/3600
      ELSE
         freflx = 0.
      ENDIF

      ! total vehicle heat flux
      vehc = carflx + motflx + freflx
      ! total anthropogenic heat flux exclude building part
      Fahe = meta + vehc

   END Subroutine LUCY

END MODULE MOD_Urban_LUCY
! ---------- EOP ------------
