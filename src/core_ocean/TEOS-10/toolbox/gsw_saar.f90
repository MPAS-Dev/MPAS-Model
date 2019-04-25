!==========================================================================
elemental function gsw_saar (p, long, lat)
!==========================================================================
!
! Calculates the Absolute Salinity Anomaly Ratio, SAAR at a geographic point.
!
! p      : sea pressure                                    [dbar]
! long   : longitude                                       [deg E]     
! lat    : latitude                                        [deg N]
!
! gsw_saar : Absolute Salinity Anomaly Ratio               [unitless]
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_add_barrier, gsw_add_mean, gsw_util_indx

use gsw_mod_error_functions, only : gsw_error_code

use gsw_mod_saar_data

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: p, long, lat

real (r8) :: gsw_saar

integer :: indx0, indy0, indz0, k, ndepth_max

real (r8), dimension(4) :: saar, saar_old
real (r8) :: p0_original, sa_upper, sa_lower, dlong, dlat
real (r8) :: r1, s1, t1, p_tmp, long360

character (*), parameter :: func_name = "gsw_saar"

long360 = long
if (long360.lt.0.0_r8) long360 = long360 + 360.0_r8

indx0 = floor(1.0_r8 + (nx-1)*(long360-longs_ref(1)) / &
              (longs_ref(nx)-longs_ref(1)))

indy0 = floor(1.0_r8 + (ny-1)*(lat-lats_ref(1)) / (lats_ref(ny)-lats_ref(1)))

if ((indx0.ge.1 .and. indx0.le.nx) .and. (indy0.ge.1 .and. indy0.le.ny)) then
   if (indx0.eq.nx) indx0 = nx-1
   if (indy0.eq.ny) indy0 = ny-1
else
   ! This is to catch any out-of-range or nonsense lat/long input (including
   ! NaN, +Inf, -Inf etc). Note: NaNs will not satisfy any "if" conditional
   ! so will be trapped by the "else".
   gsw_saar = gsw_error_code(2,func_name)
   return
end if

! Look for the maximum valid "ndepth_ref" value around our point.
! Note: invalid "ndepth_ref" values are NaNs (a hangover from the codes
! Matlab origins), but we have replaced the NaNs with a value of 999,
! hence we need an additional upper-limit check in the code below so they
! will not be recognised as valid values.
ndepth_max = -1
do k = 1,4
   if ((ndepth_ref(indy0+delj(k),indx0+deli(k)).gt.0) .and. &
       (ndepth_ref(indy0+delj(k),indx0+deli(k)).lt.99)) &
      ndepth_max = max(ndepth_max,ndepth_ref(indy0+delj(k),indx0+deli(k)))
end do

! If we are a long way from the ocean then there will be no valid "ndepth_ref"
! values near the point (ie. surrounded by NaNs) - so just return SAAR = 0.0
if (ndepth_max.eq.-1) then
   gsw_saar = 0.0_r8 
   return
end if 

p0_original = p
p_tmp = p
if (p_tmp.gt.p_ref(ndepth_max)) p_tmp = p_ref(ndepth_max)
indz0 = gsw_util_indx(p_ref,p_tmp)

dlong = longs_ref(indx0+1) - longs_ref(indx0)
dlat = lats_ref(indy0+1) - lats_ref(indy0)

r1 = (long360-longs_ref(indx0)) / dlong
s1 = (lat-lats_ref(indy0)) / dlat
t1 = (p_tmp-p_ref(indz0))/(p_ref(indz0+1)-p_ref(indz0))

do k = 1,4
   saar(k) = saar_ref(indz0,indy0+delj(k),indx0+deli(k))
end do

if (  longs_pan(1).le.long360 .and. long360.le.longs_pan(npan)-0.001_r8 .and. &
    lats_pan(npan).le.lat     .and.     lat.le.lats_pan(1)) then
  saar_old = saar
  call gsw_add_barrier(saar_old,long360,lat,longs_ref(indx0), &
                       lats_ref(indy0),dlong,dlat,saar)
else if (abs(sum(saar)).ge.1e10_r8) then
  saar_old = saar
  call gsw_add_mean(saar_old,saar)
end if

sa_upper = (1.0_r8-s1)*(saar(1) + r1*(saar(2)-saar(1))) + s1*(saar(4) + &
              r1*(saar(3)-saar(4)))

do k = 1,4
   saar(k) = saar_ref(indz0+1,indy0+delj(k),indx0+deli(k))
end do

if (  longs_pan(1).le.long360 .and. long360.le.longs_pan(npan)-0.001_r8 .and. &
    lats_pan(npan).le.lat     .and.     lat.le.lats_pan(1)) then
   saar_old = saar
   call gsw_add_barrier(saar_old,long360,lat,longs_ref(indx0), &
                        lats_ref(indy0),dlong,dlat,saar)
else if (abs(sum(saar)).ge.1e10_r8) then 
   saar_old = saar
   call gsw_add_mean(saar_old,saar)
end if

sa_lower = (1.0_r8-s1)*(saar(1) + r1*(saar(2)-saar(1))) + s1*(saar(4) + &
              r1*(saar(3)-saar(4)))
if (abs(sa_lower).ge.1e10_r8) sa_lower = sa_upper
gsw_saar = sa_upper + t1*(sa_lower-sa_upper)

if (abs(gsw_saar).ge.1e10_r8) gsw_saar = gsw_error_code(3,func_name)

return
end function

!--------------------------------------------------------------------------
