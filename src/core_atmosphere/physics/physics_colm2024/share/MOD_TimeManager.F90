#include <define.h>

MODULE MOD_TimeManager

! --------------------------------------------------------
!
! !DESCRIPTION:
!  Time manager module: to provide some basic operations for time stamp
!
!  Created by Hua Yuan, 04/2014
!
! !REVISIONS:
!  06/28/2017, Hua Yuan: added issame() and monthday2julian()
!  TODO...
! --------------------------------------------------------

   USE MOD_Precision
   IMPLICIT NONE

   integer, dimension(0:12), parameter :: &
      daysofmonth_leap      = (/0,31,29,31,30,31,30,31,31,30,31,30,31/)          ,&
      daysofmonth_noleap    = (/0,31,28,31,30,31,30,31,31,30,31,30,31/)          ,&
      accdaysofmonth_leap   = (/0,31,60,91,121,152,182,213,244,274,305,335,366/) ,&
      accdaysofmonth_noleap = (/0,31,59,90,120,151,181,212,243,273,304,334,365/)

   type :: timestamp
      integer :: year, day, sec
   END type timestamp

   INTERFACE ASSIGNMENT (=)
      MODULE procedure assignidate
      MODULE procedure assigntstamp
   END INTERFACE

   INTERFACE OPERATOR (+)
      MODULE procedure addsec
   END INTERFACE

   INTERFACE OPERATOR (-)
      MODULE procedure subtstamp
   END INTERFACE

   INTERFACE OPERATOR (<=)
      MODULE procedure lessequal
   END INTERFACE

   INTERFACE OPERATOR (<)
      MODULE procedure lessthan
   END INTERFACE

   INTERFACE OPERATOR (==)
      MODULE procedure isnull
      MODULE procedure besame
   END INTERFACE

   INTERFACE calendarday
      MODULE procedure calendarday_date
      MODULE procedure calendarday_stamp
   END INTERFACE

   logical, SAVE :: isgreenwich
   real(r8),SAVE :: LocalLongitude = 0.
   PUBLIC get_calday

CONTAINS

   SUBROUTINE initimetype(greenwich)

   IMPLICIT NONE
   logical, intent(in) :: greenwich

      isgreenwich = greenwich

#ifndef SinglePoint
      IF (.not. isgreenwich) THEN
         write(*,*) 'Warning: Please USE Greenwich time for non-SinglePoint case.'
         isgreenwich = .true.
      ENDIF
#endif

   END SUBROUTINE initimetype

   SUBROUTINE assignidate(tstamp, idate)

   IMPLICIT NONE
   type(timestamp), intent(inout) :: tstamp
   integer,         intent(in)    :: idate(3)

      tstamp%year = idate(1)
      tstamp%day  = idate(2)
      tstamp%sec  = idate(3)

   END SUBROUTINE assignidate

   SUBROUTINE assigntstamp(tstamp1, tstamp2)

   IMPLICIT NONE
   type(timestamp), intent(out) :: tstamp1
   type(timestamp), intent(in)  :: tstamp2

      tstamp1%year = tstamp2%year
      tstamp1%day  = tstamp2%day
      tstamp1%sec  = tstamp2%sec

   END SUBROUTINE assigntstamp

   FUNCTION addsec(tstamp, sec)

   IMPLICIT NONE
   type(timestamp), intent(in) :: tstamp
   integer,         intent(in) :: sec
   type(timestamp) :: addsec
   integer         :: maxday

      addsec = tstamp
      addsec%sec = addsec%sec + sec
      DO WHILE (addsec%sec > 86400)
         addsec%sec = addsec%sec - 86400
         IF( isleapyear(addsec%year) ) THEN
            maxday = 366
         ELSE
            maxday = 365
         ENDIF
         addsec%day = addsec%day + 1
         IF(addsec%day > maxday) THEN
            addsec%year = addsec%year + 1
            addsec%day = 1
         ENDIF
      ENDDO
      DO WHILE (addsec%sec <= 0)
         addsec%sec = addsec%sec + 86400
         IF( isleapyear(addsec%year-1) )THEN
            maxday = 366
         ELSE
            maxday = 365
         ENDIF
         addsec%day = addsec%day - 1
         IF(addsec%day <= 0) THEN
            addsec%year = addsec%year - 1
            addsec%day = maxday
         ENDIF
      ENDDO
      RETURN

   END FUNCTION addsec

   FUNCTION subtstamp(tstamp1, tstamp2)

   IMPLICIT NONE
   type(timestamp), intent(in) :: tstamp1
   type(timestamp), intent(in) :: tstamp2
   integer :: subtstamp

      subtstamp = tstamp1%sec - tstamp2%sec
      IF (subtstamp < 0) THEN
         subtstamp = subtstamp + 86400
      ENDIF
      RETURN

   END FUNCTION subtstamp

   logical FUNCTION lessequal(tstamp1, tstamp2)

   IMPLICIT NONE
   type(timestamp), intent(in) :: tstamp1
   type(timestamp), intent(in) :: tstamp2

   integer(kind=4) :: idate1(3), idate2(3)
   integer(kind=4) :: ts1, ts2

      idate1 = (/tstamp1%year, tstamp1%day, tstamp1%sec/)
      idate2 = (/tstamp2%year, tstamp2%day, tstamp2%sec/)

      CALL adj2end(idate1)
      CALL adj2end(idate2)

      ts1 = idate1(1)*1000 + idate1(2)
      ts2 = idate2(1)*1000 + idate2(2)

      lessequal = .false.

      IF (ts1 < ts2) lessequal = .true.

      IF (ts1==ts2 .and. idate1(3)<=idate2(3)) THEN
         lessequal = .true.
      ENDIF

      RETURN

   END FUNCTION lessequal

   logical FUNCTION lessthan(tstamp1, tstamp2)

   IMPLICIT NONE
   type(timestamp), intent(in) :: tstamp1
   type(timestamp), intent(in) :: tstamp2

   integer(kind=4) :: idate1(3), idate2(3)
   integer(kind=4) :: ts1, ts2

      idate1 = (/tstamp1%year, tstamp1%day, tstamp1%sec/)
      idate2 = (/tstamp2%year, tstamp2%day, tstamp2%sec/)

      CALL adj2end(idate1)
      CALL adj2end(idate2)

      ts1 = idate1(1)*1000 + idate1(2)
      ts2 = idate2(1)*1000 + idate2(2)

      lessthan = .false.

      IF (ts1 < ts2) lessthan = .true.

      IF (ts1==ts2 .and. idate1(3)<idate2(3)) THEN
         lessthan = .true.
      ENDIF

      RETURN

   END FUNCTION lessthan

   logical FUNCTION isnull(tstamp, nullstr)

   IMPLICIT NONE
   type(timestamp), intent(in) :: tstamp
   character(4),    intent(in) :: nullstr

      IF (tstamp%year < 0 .or. tstamp%day < 0 .or. tstamp%sec < 0) THEN
         isnull = .true.
      ELSE
         isnull = .false.
      ENDIF
      RETURN

   END FUNCTION isnull

   logical FUNCTION besame(tstamp1, tstamp2)

   IMPLICIT NONE
   type(timestamp), intent(in) :: tstamp1
   type(timestamp), intent(in) :: tstamp2

   integer :: idate1(3), idate2(3)

      idate1(1) = tstamp1%year
      idate1(2) = tstamp1%day
      idate1(3) = tstamp1%sec
      idate2(1) = tstamp2%year
      idate2(2) = tstamp2%day
      idate2(3) = tstamp2%sec

      CALL adj2end(idate1)
      CALL adj2end(idate2)

      IF (idate1(1)==idate2(1) .and. &
          idate1(2)==idate2(2) .and. &
          idate1(3)==idate2(3)) THEN
         besame = .true.
      ELSE
         besame = .false.
      ENDIF
      RETURN

   END FUNCTION besame

   logical FUNCTION isleapyear(year)

   IMPLICIT NONE
   integer, intent(in) :: year

      IF( (mod(year,4)==0 .and. mod(year,100)/=0) .or. &
         mod(year,400)==0 ) THEN
         isleapyear = .true.
      ELSE
         isleapyear = .false.
      ENDIF
      RETURN
   END FUNCTION isleapyear

   SUBROUTINE julian2monthday(year, day, month, mday)

   IMPLICIT NONE
   integer, intent(in)  :: year, day
   integer, intent(out) :: month, mday

   integer :: i, monthday(0:12)

      IF ( isleapyear(year) ) THEN
         monthday(:) = accdaysofmonth_leap(:)
      ELSE
         monthday(:) = accdaysofmonth_noleap(:)
      ENDIF

    ! calculate month and day values
      DO i = 1, 12
         IF (day .le. monthday(i)) THEN
            month = i; EXIT
         ENDIF
      ENDDO
      mday = day - monthday(i-1)

   END SUBROUTINE julian2monthday

   SUBROUTINE monthday2julian(year, month, mday, day)

   IMPLICIT NONE
   integer, intent(in)  :: year, month, mday
   integer, intent(out) :: day

   integer :: monthday(0:12)

      IF ( isleapyear(year) ) THEN
         monthday(:) = accdaysofmonth_leap(:)
      ELSE
         monthday(:) = accdaysofmonth_noleap(:)
      ENDIF

    ! calculate julian day
      day  = monthday(month-1) + mday

   END SUBROUTINE monthday2julian

   logical FUNCTION isendofhour(idate, sec)

   IMPLICIT NONE
   integer, intent(in) :: idate(3)
   real(r8),intent(in) :: sec

   integer :: hour1, hour2

      hour1 = (idate(3)-1)/3600
      hour2 = (idate(3)+int(sec)-1)/3600

      isendofhour = (hour1 /= hour2)

   END FUNCTION isendofhour

   logical FUNCTION isendofday(idate, sec)

   IMPLICIT NONE
   integer, intent(in) :: idate(3)
   real(r8),intent(in) :: sec

   type(timestamp) :: tstamp1
   type(timestamp) :: tstamp2

      tstamp1 = idate
      tstamp2 = tstamp1 + int(sec)

      IF (tstamp2%day /= tstamp1%day) THEN
         isendofday = .true.
      ELSE
         isendofday = .false.
      ENDIF
      RETURN

   END FUNCTION isendofday

   logical FUNCTION isendofmonth(idate, sec)

   IMPLICIT NONE
   integer, intent(in) :: idate(3)
   real(r8),intent(in) :: sec

   type(timestamp) :: tstamp1
   type(timestamp) :: tstamp2
   integer :: month1, month2, day

      tstamp1 = idate
      tstamp2 = tstamp1 + int(sec)

      CALL julian2monthday(tstamp1%year, tstamp1%day, month1, day)
      CALL julian2monthday(tstamp2%year, tstamp2%day, month2, day)

      IF (month1 /= month2) THEN
         isendofmonth = .true.
      ELSE
         isendofmonth = .false.
      ENDIF
      RETURN

   END FUNCTION isendofmonth

   logical FUNCTION isendofyear(idate, sec)

   IMPLICIT NONE
   integer, intent(in) :: idate(3)
   real(r8),intent(in) :: sec

   type(timestamp) :: tstamp1
   type(timestamp) :: tstamp2

      tstamp1 = idate
      tstamp2 = tstamp1 + int(sec)

      IF (tstamp1%year /= tstamp2%year) THEN
         isendofyear = .true.
      ELSE
         isendofyear = .false.
      ENDIF
      RETURN

   END FUNCTION isendofyear

   SUBROUTINE adj2begin(idate)

   IMPLICIT NONE
   integer, intent(inout) :: idate(3)

      IF (idate(3) == 86400) THEN
         idate(3) = 0
         idate(2) = idate(2) + 1
         IF (isleapyear(idate(1)) .and. idate(2)==367) THEN
            idate(1) = idate(1) + 1; idate(2) = 1
         ENDIF
         IF ( .not. isleapyear(idate(1)) .and. idate(2)==366) THEN
            idate(1) = idate(1) + 1; idate(2) = 1
         ENDIF
      ENDIF

   END SUBROUTINE adj2begin

   SUBROUTINE adj2end(idate)

   IMPLICIT NONE
   integer, intent(inout) :: idate(3)

      IF (idate(3) == 0) THEN
         idate(3) = 86400
         idate(2) = idate(2) - 1
         IF (idate(2) == 0) THEN
            idate(1) = idate(1) - 1
            IF ( isleapyear(idate(1)) ) THEN
               idate(2) = 366
            ELSE
               idate(2) = 365
            ENDIF
         ENDIF
      ENDIF

   END SUBROUTINE adj2end

   SUBROUTINE localtime2gmt(idate)

   IMPLICIT NONE
   integer, intent(inout) :: idate(3)

   integer  maxday
   real(r8) tdiff

      tdiff = LocalLongitude/15.*3600.
      idate(3) = idate(3) - int(tdiff)

      IF (idate(3) < 0) THEN

         idate(3) = 86400 + idate(3)
         idate(2) = idate(2) - 1

         IF (idate(2) < 1) THEN
            idate(1) = idate(1) - 1
            IF ( isleapyear(idate(1)) ) THEN
               idate(2) = 366
            ELSE
               idate(2) = 365
            ENDIF
         ENDIF
      ENDIF

      DO WHILE (idate(3) > 86400)
         idate(3) = idate(3) - 86400
         idate(2) = idate(2) + 1

         IF ( isleapyear(idate(1)) ) THEN
            maxday = 366
         ELSE
            maxday = 365
         ENDIF

         IF(idate(2) > maxday) THEN
            idate(1) = idate(1) + 1
            idate(2) = 1
         ENDIF
      ENDDO

   END SUBROUTINE localtime2gmt

   SUBROUTINE ticktime(deltim, idate)

   IMPLICIT NONE

   real(r8),intent(in)    :: deltim
   integer, intent(inout) :: idate(3)
   integer maxday

      idate(3) = idate(3) + nint(deltim)
      DO WHILE (idate(3) > 86400)
         idate(3) = idate(3) - 86400
         idate(2) = idate(2) + 1

         IF ( isleapyear(idate(1)) ) THEN
            maxday = 366
         ELSE
            maxday = 365
         ENDIF

         IF(idate(2) > maxday) THEN
            idate(1) = idate(1) + 1
            idate(2) = 1
         ENDIF
      ENDDO

   END SUBROUTINE ticktime

   real(r8) FUNCTION calendarday_date(date)

   IMPLICIT NONE
   integer, intent(in) :: date(3)

   integer idate(3)

      idate(:) = date(:)

      IF ( .not. isgreenwich ) THEN
         CALL localtime2gmt(idate)
      ENDIF

      calendarday_date = float(idate(2)) + float(idate(3))/86400.
      RETURN

   END FUNCTION calendarday_date

   real(r8) FUNCTION calendarday_stamp(stamp)

   IMPLICIT NONE
   type(timestamp), intent(in) :: stamp

   integer idate(3)

      idate(1) = stamp%year
      idate(2) = stamp%day
      idate(3) = stamp%sec

      IF ( .not. isgreenwich ) THEN
         CALL localtime2gmt(idate)
      ENDIF

      calendarday_stamp = float(idate(2)) + float(idate(3))/86400.
      RETURN

   END FUNCTION calendarday_stamp

   integer FUNCTION get_calday(mmdd,isleap)

   IMPLICIT NONE
   integer, intent(in) :: mmdd
   logical, intent(in) :: isleap

   integer imonth, iday

      imonth = mmdd / 100
      iday   = mod(mmdd,100)
      IF(isleap)THEN
         get_calday = sum(daysofmonth_leap(0:imonth-1)) + iday
      ELSE
         get_calday = sum(daysofmonth_noleap(0:imonth-1)) + iday
      ENDIF
      RETURN
   END FUNCTION get_calday

   integer FUNCTION minutes_since_1900 (year, julianday, second)

   USE MOD_UserDefFun
   IMPLICIT NONE
   integer, intent(in) :: year, julianday, second

   integer :: refyear(10) = (/1, 1900, 1950, 1980, 1990, 2000, 2005, 2010, 2015, 2020/)
   integer :: refval (10) = (/-998776800,0,26297280,42075360,47335680,52594560,55225440,&
                              57854880,60484320,63113760/)
   integer :: iref, iyear

      iref = findloc_ud(refyear <= year, back=.true.)
      minutes_since_1900 = refval(iref)
      DO iyear = refyear(iref), year-1
         IF (isleapyear(iyear)) THEN
            minutes_since_1900 = minutes_since_1900 + 527040
         ELSE
            minutes_since_1900 = minutes_since_1900 + 525600
         ENDIF
      ENDDO

      minutes_since_1900 = minutes_since_1900 + (julianday-1) * 1440
      minutes_since_1900 = minutes_since_1900 + second/60

   END FUNCTION minutes_since_1900

   ! -----------------------------------------------------------------------
   SUBROUTINE gmt2local(idate, long, ldate)

   ! !DESCRIPTION:
   ! A SUBROUTINE to calculate local time
   ! !PURPOSE
   ! Convert GMT time to local time in global run
   ! -----------------------------------------------------------------------

   IMPLICIT NONE

   integer , intent(in ) :: idate(3)
   real(r8), intent(in ) :: long
   real(r8), intent(out) :: ldate(3)

   integer  :: maxday
   real(r8) :: tdiff

      tdiff = long/15.*3600

      ldate(3) = idate(3) + tdiff
      ldate(1) = idate(1)

      IF (ldate(3) < 0) THEN

         ldate(3) = 86400 + ldate(3)
         ldate(2) = idate(2) - 1

         IF (ldate(2) < 1) THEN
            ldate(1) = idate(1) - 1
            IF ( isleapyear(int(ldate(1))) ) THEN
               ldate(2) = 366
            ELSE
               ldate(2) = 365
            ENDIF
         ENDIF

      ELSEIF (ldate(3) > 86400) THEN

         ldate(3) = ldate(3) - 86400
         ldate(2) = idate(2) + 1

         IF ( isleapyear(int(ldate(1))) ) THEN
            maxday = 366
         ELSE
            maxday = 365
         ENDIF

         IF(ldate(2) > maxday) THEN
            ldate(1) = idate(1) + 1
            ldate(2) = 1
         ENDIF
      ELSE
         ldate(2) = idate(2)
         ldate(1) = idate(1)
      ENDIF

   END SUBROUTINE gmt2local

   ! -----------------------------------------------------------------------
   SUBROUTINE timeweek(year, month, day, iweek)

   ! !DESCRIPTION:
   ! A subroutine to calculate day of week
   ! !PURPOSE
   ! Calculate day of week to determine IF the day is week holiday
   ! -----------------------------------------------------------------------

   IMPLICIT NONE

   integer, intent(in ) :: year, month
   integer, intent(out) :: iweek, day

   integer :: myear, mmonth
   integer :: yy, mm, dd, y12, y34
   integer :: A, B, C, D, i

   integer :: monthday(0:12)

      IF ( isleapyear(year) ) THEN
         monthday(:) = daysofmonth_leap(:)
      ELSE
         monthday(:) = daysofmonth_noleap(:)
      ENDIF

      IF (month==1 .or. month==2) THEN
         mmonth = month + 12
         myear  = year  - 1
      ELSE
         mmonth = month
         myear  = year
      ENDIF

      y12 = myear/100
      y34 = myear - y12*100

      A = int(y34/4.)
      B = int(y12/4.)
      C = y12*2
      D = int(26*(mmonth+1)/10.)

      iweek = abs(mod((y34+A+B-C+D+day-1), 7))

      DO i=1, month-1
         day = day + monthday(i)
      ENDDO

      IF (iweek == 0) THEN
         iweek = 7
      ENDIF

   END SUBROUTINE timeweek

END MODULE MOD_TimeManager
