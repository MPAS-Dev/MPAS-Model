!==========================================================================
pure function gsw_geo_strf_dyn_height (sa, ct, p, p_ref)
!==========================================================================
!
!  Calculates dynamic height anomaly as the integral of specific volume
!  anomaly from the pressure p of the bottle to the reference pressure
!  p_ref.
!
!  Hence, geo_strf_dyn_height is the dynamic height anomaly with respect
!  to a given reference pressure.  This is the geostrophic streamfunction 
!  for the difference between the horizontal velocity at the pressure 
!  concerned, p, and the horizontal velocity at p_ref.  Dynamic height 
!  anomaly is the geostrophic streamfunction in an isobaric surface.  The 
!  reference values used for the specific volume anomaly are 
!  SSO = 35.16504 g/kg and CT = 0 deg C.  This function calculates 
!  specific volume anomaly using the computationally efficient 
!  expression for specific volume of Roquet et al. (2015). 
!
!  This function evaluates the pressure integral of specific volume using 
!  SA and CT interpolated with respect to pressure using the method of 
!  Reiniger and Ross (1968).  It uses a weighted mean of (i) values 
!  obtained from linear interpolation of the two nearest data points, and 
!  (ii) a linear extrapolation of the pairs of data above and below.  This 
!  "curve fitting" method resembles the use of cubic splines.  
!
!  SA    =  Absolute Salinity                                      [ g/kg ]
!  CT    =  Conservative Temperature (ITS-90)                     [ deg C ]
!  p     =  sea pressure                                           [ dbar ]
!           ( i.e. absolute pressure - 10.1325 dbar )
!  p_ref =  reference pressure                                     [ dbar ]
!           ( i.e. reference absolute pressure - 10.1325 dbar )
!
!  geo_strf_dyn_height  =  dynamic height anomaly               [ m^2/s^2 ]
!   Note. If p_ref exceeds the pressure of the deepest bottle on a 
!     vertical profile, the dynamic height anomaly for each bottle 
!     on the whole vertical profile is returned as NaN.
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_linear_interp_sa_ct, gsw_rr68_interp_sa_ct
use gsw_mod_toolbox, only : gsw_specvol_anom_standard

use gsw_mod_error_functions, only : gsw_error_code

use gsw_mod_teos10_constants, only : db2pa

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sa(:), ct(:), p(:), p_ref

real (r8) :: gsw_geo_strf_dyn_height(size(sa))

integer, allocatable :: iidata(:)

real (r8), allocatable :: b(:), b_av(:), dp(:), dp_i(:)
real (r8), allocatable :: sa_i(:), ct_i(:), p_i(:)
real (r8), allocatable :: geo_strf_dyn_height0(:)

integer :: p_cnt, top_pad, i, nz, ibottle, ipref, np_max, np, ibpr

real (r8) :: dp_min, dp_max, p_min, p_max

! This max_dp_i is the limit we choose for the evaluation of specific
! volume in the pressure integration. That is, the vertical integration
! of specific volume with respect to pressure is perfomed with the pressure
! increment being no more than max_dp_i (the default value being 1 dbar).
real (r8), parameter :: max_dp_i = 1.0_r8

! This p_max_limit is the maximum pressure we're likely to encounter (at
! least in real-world situations - Mariana Trench is just over 11km depth).
! It is used to set memory limits for array allocation (via np_max_limit).
real (r8), parameter :: p_max_limit = 20000.0_r8

integer, parameter :: np_max_limit = nint(p_max_limit/max_dp_i)

character (*), parameter :: func_name = "gsw_geo_strf_dyn_height"

nz = size(sa)

allocate (dp(nz-1))

dp = p(2:nz) - p(1:nz-1)
dp_min = minval(dp)
dp_max = maxval(dp)

if (.not. (dp_min .gt. 0.0_r8)) then
    ! pressure must be monotonic (do negative test to trap NaNs)
    gsw_geo_strf_dyn_height = gsw_error_code(1,func_name)
    return
end if
p_min = p(1)
p_max = p(nz)

if (.not. (p_ref .le. p_max)) then
    ! the reference pressure p_ref is deeper than all bottles (do negative
    ! test to trap NaNs)
    gsw_geo_strf_dyn_height = gsw_error_code(2,func_name)
    return
end if

! Determine if there is a "bottle" at exactly p_ref
ipref = -1
do ibottle = 1, nz
    if (p(ibottle) .eq. p_ref) then
        ipref = ibottle
        exit
    end if
end do

if ((dp_max .le. max_dp_i) .and. (p(1) .eq. 0.0_r8) .and. (ipref .gt. 0)) then

    ! vertical resolution is good (bottle gap is no larger than max_dp_i) 
    ! & the vertical profile begins at the surface (i.e. at p = 0 dbar) 
    ! & the profile contains a "bottle" at exactly p_ref. 
    
    allocate (b(nz), b_av(nz-1))

    b = gsw_specvol_anom_standard(sa,ct,p)
    
    b_av = 0.5_r8*(b(1:nz-1) + b(2:nz))    
    
    ! "geo_strf_dyn_height0" is the dynamic height anomaly with respect
    ! to p_ref = 0 (the surface).  

    allocate (geo_strf_dyn_height0(nz))

    geo_strf_dyn_height0 = (/ 0.0_r8, b_av*dp*db2pa /)
    do i = 2, nz   ! cumulative sum
        geo_strf_dyn_height0(i) = geo_strf_dyn_height0(i-1) &
                                  - geo_strf_dyn_height0(i)
    end do
    gsw_geo_strf_dyn_height = geo_strf_dyn_height0 - geo_strf_dyn_height0(ipref)

else

    ! Test if there are vertical gaps between adjacent "bottles" which are
    ! greater than max_dp_i, and that there is a "bottle" exactly at the 
    ! reference pressure.

    allocate (iidata(nz))

    ibpr = 0
    if ((dp_max .le. max_dp_i) .and. (ipref .gt. 0)) then

        ! Vertical resolution is already good (no larger than max_dp_i), and
        ! there is a "bottle" at exactly p_ref. 
                
        if (p_min .gt. 0.0_r8) then
            ! resolution is fine and there is a bottle at p_ref, but
            ! there is not a bottle at p = 0. So add an extra bottle.
            allocate (sa_i(nz+1), ct_i(nz+1), p_i(nz+1))
            sa_i = (/ sa(1), sa /)
            ct_i = (/ ct(1), ct /)
            p_i = (/ 0.0_r8, p /)
            ibpr = ipref + 1
            iidata = (/ (i, i=2,nz+1) /)
        else
            ! resolution is fine, there is a bottle at p_ref, and
            ! there is a bottle at p = 0
            allocate (sa_i(nz), ct_i(nz), p_i(nz))
            sa_i = sa
            ct_i = ct
            p_i = p
            ibpr = ipref
            iidata = (/ (i, i=1,nz) /)
        end if
        p_cnt = size(p_i)

    else

        ! interpolation is needed.
        np_max = 2*nint(maxval(p/max_dp_i)+0.5_r8)
        if (np_max.gt.np_max_limit) then
            gsw_geo_strf_dyn_height = gsw_error_code(3,func_name)
            return
        end if
        allocate (p_i(np_max))

        if (p_min .gt. 0.0_r8) then
            ! there is not a bottle at p = 0.
            if (p_ref .lt. p_min) then
                ! p_ref is shallower than the minimum bottle pressure.
                p_i(1) = 0.0_r8
                call p_sequence(p_i(1),p_ref,p_i(2:),np)
                p_cnt = np + 1
                ibpr = p_cnt
                call p_sequence(p_ref,p_min,p_i(p_cnt+1:),np)
                p_cnt = p_cnt + np
                top_pad = p_cnt  
            else
                ! p_ref is deeper than the minimum bottle pressure. 
                p_i(1:2) = (/ 0.0_r8, p_min /)
                top_pad = 2
                p_cnt = 2
            end if
        else
            ! there is a bottle at p = 0.
            p_i(1) = p_min
            top_pad = 1
            p_cnt = 1
        end if

        do ibottle = 1, nz-1

            iidata(ibottle) = p_cnt
            if (p(ibottle) .eq. p_ref) ibpr = p_cnt

            if (p(ibottle) .lt. p_ref .and. p(ibottle+1) .gt. p_ref) then
                ! ... reference pressure is spanned by bottle pairs -
                ! need to include p_ref as an interpolated pressure.
                call p_sequence(p(ibottle),p_ref,p_i(p_cnt+1:),np)
                p_cnt = p_cnt + np
                ibpr = p_cnt
                call p_sequence(p_ref,p(ibottle+1),p_i(p_cnt+1:),np)
                p_cnt = p_cnt + np
            else
                ! ... reference pressure is not spanned by bottle pairs.
                call p_sequence(p(ibottle),p(ibottle+1),p_i(p_cnt+1:),np)
                p_cnt = p_cnt + np
            end if

        end do

        iidata(nz) = p_cnt
        if (p(nz) .eq. p_ref) ibpr = p_cnt

        allocate (sa_i(p_cnt), ct_i(p_cnt))

        if (top_pad .gt. 1) &
            call gsw_linear_interp_sa_ct(sa,ct,p,p_i(1:top_pad-1),sa_i,ct_i)
        call gsw_rr68_interp_sa_ct(sa,ct,p,p_i(top_pad:p_cnt), &
                                   sa_i(top_pad:),ct_i(top_pad:))
    end if

    allocate (b(p_cnt), b_av(p_cnt-1), dp_i(p_cnt-1))
    allocate (geo_strf_dyn_height0(p_cnt))

    b = gsw_specvol_anom_standard(sa_i(:p_cnt),ct_i(:p_cnt),p_i(:p_cnt))
    b_av = 0.5_r8*(b(1:p_cnt-1) + b(2:p_cnt))
    dp_i = p_i(2:p_cnt) - p_i(1:p_cnt-1)

    geo_strf_dyn_height0 = (/ 0.0_r8, b_av*dp_i /)
    do i = 2, p_cnt   ! cumulative sum
        geo_strf_dyn_height0(i) = geo_strf_dyn_height0(i-1) &
                                  - geo_strf_dyn_height0(i)
    end do
    gsw_geo_strf_dyn_height = (geo_strf_dyn_height0(iidata) &
                               - geo_strf_dyn_height0(ibpr))*db2pa

end if
return

contains

    pure subroutine p_sequence (p1, p2, pseq, nps)

    implicit none

    real (r8), intent(in) :: p1, p2
    real (r8), intent(inout) :: pseq(:)
    integer, intent(out), optional :: nps

    real (r8) :: dp, pstep

    integer :: n, i

    dp = p2 - p1
    n = ceiling(dp/max_dp_i)
    pstep = dp/n

    if (present(nps)) nps = n

    ! Generate the sequence ensuring that the value of p2 is exact to
    ! avoid round-off issues, ie. don't do "pseq = (p1+pstep*i, i=1,n)".

    pseq(1:n) = (/ (p2-pstep*i, i=n-1,0,-1) /)

    return
    end subroutine p_sequence

end function

!--------------------------------------------------------------------------
