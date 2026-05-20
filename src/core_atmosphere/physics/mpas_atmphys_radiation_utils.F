! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module mpas_atmphys_radiation_utils

 use mpas_kind_types, only : RKIND
 use mpas_atmphys_constants, only : solcon_0
 use mpas_log, only : mpas_log_write

 implicit none
 private
 public:: radconst,     &
          co2_estimate

 contains

!=================================================================================================================
 subroutine radconst(declin,solcon,julian,degrad,dpd,co2,year)
!=================================================================================================================
!
! computes solar declination
! computes co2 concentration from WRF v4.2 (Jimy Dudhia)
!
!input arguments:
 real(kind=RKIND),intent(in):: julian
 real(kind=RKIND),intent(in):: degrad,dpd
 integer, intent(in):: year

!output arguments:
 real(kind=RKIND),intent(out):: declin,solcon
 real(kind=RKIND),intent(out):: co2

!local variables:
 real(kind=RKIND):: obecl,sinob,sxlong,arg,decdeg,djul,rjul,eccfac

!-----------------------------------------------------------------------------------------------------------------

 call mpas_log_write('--- enter subroutine radconst:')

 declin=0.
 solcon=0.

!obecl : obliquity = 23.5 degree.

 obecl=23.5*degrad
 sinob=sin(obecl)

!calculate longitude of the sun from vernal equinox:

 if(julian.ge.80.)sxlong=dpd*(julian-80.)
 if(julian.lt.80.)sxlong=dpd*(julian+285.)

 sxlong=sxlong*degrad
 arg=sinob*sin(sxlong)
 declin=asin(arg)
 decdeg=declin/degrad

!solar constant eccentricity factor (paltridge and platt 1976)

 djul=julian*360./365.
 rjul=djul*degrad
 eccfac=1.000110+0.034221*cos(rjul)+0.001280*sin(rjul)+0.000719*  &
        cos(2*rjul)+0.000077*sin(2*rjul)
 solcon=solcon_0*eccfac

!estimate co2 concentration from an equation from WRF4.2

 co2 = co2_estimate(year,julian)

 end subroutine radconst

!=================================================================================================================
 real(kind=RKIND) function co2_estimate(year,julian)
!=================================================================================================================

! This function calculates CO2 from an equation in WRF by J. Dudhia

 implicit none

 integer, intent(in) :: year
 real(kind=RKIND),intent(in):: julian

 real(kind=RKIND) :: rdays,ryear
 integer :: ny1, ny2, ny3

!-----------------------------------------------------------------------------------------------------------------

 rdays = 365._RKIND
 ny1 = mod(year,4)
 ny2 = mod(year,100)
 ny3 = mod(year,400)
 if( (ny1.eq.0 .and. ny2.ne.0) .or. ny3.eq.0 ) rdays = 366._RKIND
 ryear = float(year)+julian/rdays

! Annual function for co2 from WRF v4.2
 co2_estimate = (280._RKIND + 90._RKIND*exp(0.02_RKIND*(ryear-2000._RKIND)))*1.e-6_RKIND

 end function co2_estimate

!=================================================================================================================
 end module mpas_atmphys_radiation_utils
!=================================================================================================================
