#include <define.h>

! ********************************************************************************
! prospect_DB.f90
! a.k.a. PROSPECT-Dynamic
! version 6.0 (January, 16th 2017)
! subroutines required: tav.f90, dataSpec_PDB.f90
! _______________________________________________________________________
! for any question or request, please contact:
!
! Jean-Baptiste FERET
! UMR-TETIS, IRSTEA Montpellier
! Maison de la T�l�d�tection
! 500 rue Jean-Fracois Breton
! 34093 Montpellier cedex 5
! E-mail: jb.feret@teledetection.fr
!
! St�phane JACQUEMOUD
! Universit� Paris Diderot / Institut de Physique du Globe de Paris
! 35 rue H�l�ne Brion
! 75013 Paris, France
! E-mail: jacquemoud@ipgp.fr
!
! http://teledetection.ipgp.fr/prosail/
!______________________________________________________________________
! Plant leaf reflectance and transmittance are calculated from 400 nm to
! 2500 nm (1 nm step) with the following parameters:
!
!   - N   	= leaf structure parameter
!   - Cab 	= chlorophyll a+b content in �g/cm�
!   - Car 	= carotenoids content in �g/cm�
!   - Anth 	= Anthocyanin content in �g/cm�
!   - Cbrown= brown pigments content in arbitrary units
!   - Cw  	= equivalent water thickness in g/cm� or cm
! 	- Cm  	= dry matter content in g/cm�
!
!Here are some examples observed during the LOPEX'93 experiment on
!fresh (F) and dry (D) leaves :
!
!---------------------------------------------
!               N     Cab     Cw        Cm
!---------------------------------------------
!min          1.000    0.0  0.004000  0.001900
! max          3.000  100.0  0.040000  0.016500
! corn (F)     1.518   58.0  0.013100  0.003662
! rice (F)     2.275   23.7  0.007500  0.005811
! clover (F)   1.875   46.7  0.010000  0.003014
! laurel (F)   2.660   74.1  0.019900  0.013520
! ---------------------------------------------
! min          1.500    0.0  0.000063  0.0019
! max          3.600  100.0  0.000900  0.0165
! bamboo (D)   2.698   70.8  0.000117  0.009327
! lettuce (D)  2.107   35.2  0.000244  0.002250
! walnut (D)   2.656   62.8  0.000263  0.006573
! chestnut (D) 1.826   47.7  0.000307  0.004305
! ---------------------------------------------
! _______________________________________________________________________!
! if no information about Anth or Cbrown and work on green / mature leaves
! set it to 0 													 		 !
! if no information about Car and work on green / mature leaves 		 !
! set the Chl / Car ratio between 4 and 5. this is not appropriate for 	 !
! senescent leaves 														 !
! _______________________________________________________________________!
! this code includes numerical optimizations proposed in the FLUSPECT code
! Authors: Wout Verhoef, Christiaan van der Tol (c.vandertol@utwente.nl) &
! Joris Timmermans
! Date: 2007
! Update from PROSPECT to FLUSPECT: January 2011 (CvdT)
! for more info about FLUSPECT, see publication:
! Vilfan, N., van der Tol, C., Muller, O., Rascher, U., Verhoef, W., 2016.
! Fluspect-B: A model for leaf fluorescence, reflectance and transmittance
! spectra. Remote Sens. Environ. 186, 596�615. doi:10.1016/j.rse.2016.09.017

MODULE MOD_prospect_DB
USE MOD_Precision
IMPLICIT NONE
SAVE

PUBLIC :: prospect_DB

CONTAINS
subroutine prospect_DB(N,Cab,Car,Anth,Cbrown,Cw,Cm,RT)
! ********************************************************************************
! F�ret, Gitelson, Noble & Jacqumoud (2017). PROSPECT-D: Towards modeling
! leaf optical properties through a complete lifecycle
! Remote Sensing of Environment, 193:204�215
! DOI: http://doi.org/10.1016/j.rse.2017.03.004
! Jacquemoud S., Baret F. (1990), PROSPECT: a model of leaf optical properties
! spectra, Remote Sensing of Environment, 34:75-91.
! ********************************************************************************

use MOD_dataSpec_PDB
use MOD_tav_abs
implicit none

real(r8), intent(in) :: N,Cab,Car,Anth,Cbrown,Cw,Cm
real(r8), intent(out) :: RT(nw,2)

real(r8) :: k(nw), tau(nw), xx(nw), yy(nw)
real(r8) :: ralf(nw),talf(nw),r12(nw),t12(nw),r21(nw),t21(nw)
real(r8) :: theta1, theta2, denom(nw),Ra(nw),Ta(nw),r(nw),t(nw)
real(r8) :: d(nw),rq(nw),tq(nw),a(nw),b(nw)
real(r8) :: bNm1(nw),bN2(nw),a2(nw),Rsub(nw),Tsub(nw)

k	= (Cab*k_Cab+Car*k_Car+Anth*k_Anth+Cbrown*k_Brown+Cw*k_Cw+Cm*k_Cm)/N

! ********************************************************************************
! reflectance and transmittance of one layer
! ********************************************************************************
! Allen W.A., Gausman H.W., Richardson A.J., Thomas J.R. (1969), Interaction of
! isotropic ligth with a compact plant leaf, Journal of the Optical Society of
! American, 59:1376-1379.
! ********************************************************************************

! exponential integral: S13AAF routine from the NAG library

where (k.le.0.0)
	tau	= 1
end where
where (k.gt.0.0.and.k.le.4.0)
	xx	= 0.5*k-1.0
	yy	= (((((((((((((((-3.60311230482612224d-13 &
		*xx+3.46348526554087424d-12)*xx-2.99627399604128973d-11) &
		*xx+2.57747807106988589d-10)*xx-2.09330568435488303d-9) &
		*xx+1.59501329936987818d-8)*xx-1.13717900285428895d-7) &
		*xx+7.55292885309152956d-7)*xx-4.64980751480619431d-6) &
		*xx+2.63830365675408129d-5)*xx-1.37089870978830576d-4) &
		*xx+6.47686503728103400d-4)*xx-2.76060141343627983d-3) &
		*xx+1.05306034687449505d-2)*xx-3.57191348753631956d-2) &
		*xx+1.07774527938978692d-1)*xx-2.96997075145080963d-1
	yy	= (yy*xx+8.64664716763387311d-1)*xx+7.42047691268006429d-1
	yy	= yy-log(k)
	tau	= (1.0-k)*exp(-k)+k**2*yy
end where
where (k.gt.4.0.and.k.le.85.0)
	xx	= 14.5/(k+3.25)-1.0
	yy	= (((((((((((((((-1.62806570868460749d-12 &
		*xx-8.95400579318284288d-13)*xx-4.08352702838151578d-12) &
		*xx-1.45132988248537498d-11)*xx-8.35086918940757852d-11) &
		*xx-2.13638678953766289d-10)*xx-1.10302431467069770d-9) &
		*xx-3.67128915633455484d-9)*xx-1.66980544304104726d-8) &
		*xx-6.11774386401295125d-8)*xx-2.70306163610271497d-7) &
		*xx-1.05565006992891261d-6)*xx-4.72090467203711484d-6) &
		*xx-1.95076375089955937d-5)*xx-9.16450482931221453d-5) &
		*xx-4.05892130452128677d-4)*xx-2.14213055000334718d-3
	yy	= ((yy*xx-1.06374875116569657d-2)*xx-8.50699154984571871d-2)*xx+9.23755307807784058d-1
	yy	= exp(-k)*yy/k
	tau	= (1.0-k)*exp(-k)+k**2*yy
end where
where (k.gt.85.0)
	tau	= 0
end where

! transmissivity of the layer

theta1	= 90.
call tav_abs(theta1,refractive,t12)
theta2	= 40.
call tav_abs(theta2,refractive,talf)
ralf    = 1.-talf
r12     = 1.-t12
t21     = t12/(refractive**2)
r21     = 1-t21
! top surface side
denom   = 1-r21*r21*tau**2
Ta      = talf*tau*t21/denom
Ra      = ralf+r21*tau*Ta
! bottom surface side
t       = t12*tau*t21/denom
r       = r12+r21*tau*t

! ********************************************************************************
! reflectance and transmittance of N layers
! ********************************************************************************
! Stokes G.G. (1862), On the intensity of the light reflected from or transmitted
! through a pile of plates, Proceedings of the Royal Society of London, 11:545-556.
! ********************************************************************************
D       = sqrt((1.+r+t)*(1.+r-t)*(1.-r+t)*(1.-r-t))
rq      = r**2
tq      = t**2
a       = (1.+rq-tq+D)/(2*r)
b       = (1.-rq+tq+D)/(2*t)

bNm1    = b**(N-1)
bN2     = bNm1**2
a2      = a**2
denom   = a2*bN2-1.
Rsub    = a*(bN2-1.)/denom
Tsub    = bNm1*(a2-1.)/denom

! Case of zero absorption
where (r+t.ge.1.0)
	Tsub 	= t/(t+(1.-t)*(N-1))
	Rsub	= 1-Tsub
end where

! Reflectance and transmittance of the leaf: combine top layer with next N-1 layers
denom   = 1-Rsub*r
RT(:,2)    = Ta*Tsub/denom
RT(:,1)    = Ra+Ta*Rsub*t/denom

end subroutine

END MODULE