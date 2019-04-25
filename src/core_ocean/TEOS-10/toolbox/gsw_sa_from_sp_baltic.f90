!==========================================================================
elemental function gsw_sa_from_sp_baltic (sp, long, lat)
!==========================================================================
!
! For the Baltic Sea, calculates Absolute Salinity with a value
! computed analytically from Practical Salinity
!
! sp     : Practical Salinity                              [unitless]
! long   : longitude                                       [deg E]     
! lat    : latitude                                        [deg N]
!
! gsw_sa_from_sp_baltic : Absolute Salinity                [g/kg] 
!--------------------------------------------------------------------------

use gsw_mod_toolbox, only : gsw_util_xinterp1

use gsw_mod_baltic_data

use gsw_mod_teos10_constants, only : gsw_sso

use gsw_mod_kinds

implicit none

real (r8), intent(in) :: sp, long, lat

real (r8) :: gsw_sa_from_sp_baltic

real (r8) :: xx_left, xx_right

if (xb_left(2).lt.long .and. long.lt.xb_right(1) .and. &
    yb_left(1).lt.lat  .and.  lat.lt.yb_left(3)) then
  
    xx_left = gsw_util_xinterp1(yb_left, xb_left, lat)
    
    xx_right = gsw_util_xinterp1(yb_right, xb_right, lat)
    
    if(xx_left.le.long .and. long.le.xx_right) then
        gsw_sa_from_sp_baltic = ((gsw_sso - 0.087_r8)/35.0_r8)*sp + 0.087_r8
    else
        gsw_sa_from_sp_baltic = 9e15_r8
    end if

else
    gsw_sa_from_sp_baltic = 9e15_r8
end if

return
end function

!--------------------------------------------------------------------------
