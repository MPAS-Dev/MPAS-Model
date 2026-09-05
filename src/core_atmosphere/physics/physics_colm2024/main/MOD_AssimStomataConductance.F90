#include<define.h>

MODULE MOD_AssimStomataConductance

!-----------------------------------------------------------------------
   USE MOD_Precision
   USE MOD_Namelist
   IMPLICIT NONE
   SAVE

! PUBLIC MEMBER FUNCTIONS:
   PUBLIC :: stomata
   PUBLIC :: update_photosyn

! PRIVATE MEMBER FUNCTIONS:
   PRIVATE :: sortin
   PRIVATE :: calc_photo_params
   PRIVATE :: WUE_solver


!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------

   SUBROUTINE stomata (vmax25,effcon,c3c4,slti,hlti,shti, &
                       hhti,trda,trdm,trop,g1,g0,gradm,binter,tm, &
                       psrf,po2m,pco2m,pco2a,ea,ei,tlef,par, &
!Ozone stress variables
                       o3coefv,o3coefg, &
!End ozone stress variables
!WUE stomata model parameter
                       lambda, &
!End WUE stomata model parameter
                       rb,ra,rstfac,cint,assim,respc,rst )

!=======================================================================
!
! !DESCRIPTION:
!  calculation of canopy photosynthetic rate using the integrated
!  model relating assimilation and stomatal conductance.
!
!  Original author: Yongjiu Dai, 08/11/2001
!
! !REFERENCES:
!  Dai et al., 2004: A two-big-leaf model for canopy temperature,
!  photosynthesis and stomatal conductance. J. Climate, 17: 2281-2299.
!
!
!  units are converted from mks to biological units in this routine.
!
!                       units
!                      -------
!
!     pco2m, pco2a, pco2i, po2m             : pascals
!     co2a, co2s, co2i, h2oa, h2os, h2oa    : mol mol-1
!     vmax25, respcp, assim, gs, gb, ga     : mol m-2 s-1
!     effcon                                : mol co2 mol quanta-1
!     1/rb, 1/ra, 1/rst                     : m s-1
!
!                    conversions
!                   -------------
!
!     1 mol h2o           = 0.018 kg
!     1 mol co2           = 0.044 kg
!     h2o (mol mol-1)     = ea / psrf ( pa pa-1 )
!     h2o (mol mol-1)     = q*mm/(q*mm + 1)
!     gs  (co2)           = gs (h2o) * 1./1.6
!     gs  (mol m-2 s-1 )  = gs (m s-1) * 44.6*tf/t*p/po
!     par (mol m-2 s-1 )  = par(w m-2) * 4.6*1.e-6
!     mm  (molair/molh2o) = 1.611
!
! !REVISIONS:
!  2021, Xingjie Lu: Add ozone stree and WUE model
!
!----------------------------------------------------------------------

   USE MOD_Precision
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   real(r8),intent(in) :: &
      effcon,       &! quantum efficiency of RuBP regeneration (mol CO2 / mol quanta)
      vmax25,       &! maximum carboxylation rate at 25 C at canopy top

      trop,         &! temperature coefficient in gs-a model             (298.16)
      slti,         &! slope of low temperature inhibition function      (0.2)
      hlti,         &! 1/2 point of low temperature inhibition function  (288.16)
      shti,         &! slope of high temperature inhibition function     (0.3)
      hhti,         &! 1/2 point of high temperature inhibition function (313.16)
      trda,         &! temperature coefficient in gs-a model             (1.3)
      trdm,         &! temperature coefficient in gs-a model             (328.16)
      g1,           &! conductance-photosynthesis slope parameter for medlyn model
      g0,           &! conductance-photosynthesis intercept for medlyn model
      gradm,        &! conductance-photosynthesis slope parameter
      binter         ! conductance-photosynthesis intercept
   integer, intent(in) :: &
      c3c4           ! 1 for c3, 0 for c4
   real(r8),intent(in) :: &
      tm,           &! atmospheric air temperature (K)
      psrf,         &! surface atmospheric pressure (pa)
      po2m,         &! O2 concentration in atmos. (pascals)
      pco2m,        &! CO2 concentration in atmos. (pascals)
      pco2a,        &! CO2 concentration in canopy air space (pa)
      ea,           &! canopy air space vapor pressure (pa)
      ei,           &! saturation h2o vapor pressure in leaf stomata (pa)
      tlef,         &! leaf temperature (K)
      par,          &! photosynthetic active radiation (W m-2)
!Ozone stress variables
      o3coefv,      &
      o3coefg,      &
!End ozone stress variables

!WUE stomata model parameter
      lambda,       &! marginal water cost of carbon gain ((mol h2o) (mol co2)-1)
!End WUE stomata model parameter

      rb,           &! boundary resistance from canopy to cas (s m-1)
      ra,           &! aerodynamic resistance from cas to reference height (s m-1)
      rstfac         ! canopy resistance stress factors to soil moisture

   real(r8),intent(in), dimension(3) :: &
      cint           ! scaling up from leaf to canopy

   real(r8),intent(out) :: &! ATTENTION : all for canopy not leaf
      assim,        &! canopy assimilation rate (mol m-2 s-1)
      respc,        &! canopy respiration (mol m-2 s-1)
      rst            ! canopy stomatal resistance (s m-1)

   real(r8)  gammas

!-------------------------- Local Variables ----------------------------

   integer, parameter :: iterationtotal = 6   ! total iteration number in pco2i calculation

   real(r8) &
      c3,           &! c3 vegetation : 1; 0 for c4
      c4,           &! c4 vegetation : 1; 0 for c3
      rrkk,         &! kc (1+o2/ko)

      vm,           &! maximum catalytic activity of Rubison (mol co2 m-2 s-1)
      epar,         &! electron transport rate (mol electron m-2 s-1)
      bintc,        &! residual stomatal conductance for co2 (mol co2 m-2 s-1)
      acp,          &! temporary variable for stomata model  (mol co2 m-2 s-1)
      vpd,          &! vapor pressure deficit                (kpa)

      tprcor,       &! coefficient for unit transfer
      gbh2o,        &! one side leaf boundary layer conductance (mol m-2 s-1)
      gsh2o,        &! canopy conductance (mol m-2 s-1)

      atheta,       &! wc, we coupling parameter
      btheta,       &! wc & we, ws coupling parameter
      omss,         &! intermediate calculation for oms
      omc,          &! rubisco limited assimilation (omega-c: mol m-2 s-1)
      ome,          &! light limited assimilation (omega-e: mol m-2 s-1)
      oms,          &! sink limited assimilation (omega-s: mol m-2 s-1)
      omp,          &! intermediate calculation for omc, ome

      co2m,         &! co2 concentration in atmos (mol mol-1)
      co2a,         &! co2 concentration at cas (mol mol-1)
      co2s,         &! co2 concentration at canopy surface (mol mol-1)
      co2st,        &! co2 concentration at canopy surface (mol mol-1)
      co2i,         &! internal co2 concentration (mol mol-1)
      pco2in,       &! internal co2 concentration at the new iteration (pa)
      pco2i,        &! internal co2 concentration (pa)
      pco2i_c,      &! internal co2 concentration when Rubisco is limited (pa)
      pco2i_e,      &! internal co2 concentration when RuBP regeneration is limited (pa)
      es,           &! canopy surface h2o vapor pressure (pa)

      sqrtin,       &! intermediate calculation for quadratic
      assmt,        &! net assimilation with a positive limitation (mol co2 m-2 s-1)
      assimn,       &! net assimilation (mol co2 m-2 s-1)
      hcdma,        &! a-1
      aquad,        &! a: ax^2 + bx + c = 0
      bquad,        &! b: ax^2 + bx + c = 0
      cquad          ! c: ax^2 + bx + c = 0

   real(r8) :: &
      eyy(iterationtotal),    &! differnce of pco2i at two iteration step
      pco2y(iterationtotal),  &! adjusted to total iteration number
      range                    !

   integer ic
!-----------------------------------------------------------------------

      CALL calc_photo_params(tlef, po2m, par , psrf, rstfac, rb, effcon, vmax25, c3c4, &
                             trop, slti, hlti, shti, hhti, trda, trdm, cint, &
                             vm, epar, respc, omss, gbh2o, gammas, rrkk, c3, c4)

      bintc = binter * max( 0.1, rstfac )
      bintc = bintc * cint(3)

!-----------------------------------------------------------------------
!     first guess is midway between compensation point and maximum
!     assimilation rate. ! pay attention on this iteration

      tprcor = 44.6*273.16*psrf/1.013e5

      co2m = pco2m/psrf                               ! mol mol-1
      co2a = pco2a/psrf

      range = pco2m * ( 1. - 1.6/gradm ) - gammas

      DO ic = 1, iterationtotal    ! loop for total iteration number
         pco2y(ic) = 0.
         eyy(ic) = 0.
      ENDDO

      ITERATION_LOOP: DO ic = 1, iterationtotal

         !IF(.not. DEF_USE_WUEST .or. epar .lt. 1.e-12)THEN
         IF(.not. DEF_USE_WUEST .or. abs(c4 - 1) .lt. 0.001)THEN
            CALL sortin(eyy, pco2y, range, gammas, ic, iterationtotal)
            pco2i   = pco2y(ic)
            pco2i_c = pco2i
            pco2i_e = pco2i
         ELSE
            CALL WUE_solver(gammas, lambda, co2a, ei, ea, psrf, pco2i_c, pco2i_e)
         ENDIF

!-----------------------------------------------------------------------
!                      NET ASSIMILATION
!     the leaf assimilation (or gross photosynthesis) rate is described
!     as the minimum of three limiting rates:
!     omc: the efficiency of the photosynthetic enzyme system (Rubisco-limited);
!     ome: the amount of PAR captured by leaf chlorophyll;
!     oms: the capacity of the leaf to export or utilize the products of photosynthesis.
!     to aviod the abrupt transitions, two quadratic equations are used:
!             atheta*omp^2 - omp*(omc+ome) + omc*ome = 0
!         btheta*assim^2 - assim*(omp+oms) + omp*oms = 0
!-----------------------------------------------------------------------

         atheta = 0.877
         btheta = 0.95

         ! As if DEF_USE_WUEST=.false., pco2i_c=pco2i_e=pco2i
         omc = vm   * ( pco2i_c-gammas ) / ( pco2i_c + rrkk ) * c3 + vm * c4
         ome = epar * ( pco2i_e-gammas ) / ( pco2i_e+2.*gammas ) * c3 + epar * c4
         !IF(.not. DEF_USE_WUEST .or. epar .lt. 1.e-12)THEN
         IF(.not. DEF_USE_WUEST .or. abs(c4 - 1) .lt. 0.001)THEN
            oms = omss * c3 + omss*pco2i * c4

            sqrtin= max( 0., ( (ome+omc)**2 - 4.*atheta*ome*omc ) )
            omp   = ( ( ome+omc ) - sqrt( sqrtin ) ) / ( 2.*atheta )
            sqrtin= max( 0., ( (omp+oms)**2 - 4.*btheta*omp*oms ) )
            assim = max( 0., ( ( oms+omp ) - sqrt( sqrtin ) ) / ( 2.*btheta ))
         ELSE
            assim = max( 0., min(omc, ome))
         ENDIF
         !print*,'assimn',assim,omc,ome
         assimn= ( assim - respc)                         ! mol m-2 s-1

!-----------------------------------------------------------------------
!                      STOMATAL CONDUCTANCE
!
!  (1)   pathway for co2 flux
!                                                  co2m
!                                                   o
!                                                   |
!                                                   |
!                                                   <  |
!                                        1.37/gsh2o >  |  Ac-Rd-Rsoil
!                                                   <  v
!                                                   |
!                                     <--- Ac-Rd    |
!     o------/\/\/\/\/\------o------/\/\/\/\/\------o
!    co2i     1.6/gsh2o     co2s    1.37/gbh2o     co2a
!                                                   | ^
!                                                   | | Rsoil
!                                                   | |
!
!  (2)   pathway for water vapor flux
!
!                                                  em
!                                                   o
!                                                   |
!                                                   |
!                                                   <  ^
!                                           1/gsh2o >  | Ea
!                                                   <  |
!                                                   |
!                                     ---> Ec       !
!     o------/\/\/\/\/\------o------/\/\/\/\/\------o
!     ei       1/gsh2o      es       1/gbh2o       ea
!                                                   | ^
!                                                   | | Eg
!                                                   | |
!
!  (3)   the relationship between net assimilation and tomatal conductance :
!        gsh2o = m * An * [es/ei] / [pco2s/p] + b
!        es = [gsh2o *ei + gbh2o * ea] / [gsh2o + gbh2o]
!        ===>
!        a*gsh2o^2 + b*gsh2o + c = 0
!
!-----------------------------------------------------------------------

         co2s = co2a - 1.37*assimn/gbh2o                  ! mol mol-1

         co2st = min( co2s, co2a )
         co2st = max( co2st,1.e-5 )

         assmt = max( 1.e-12, assimn )

         !IF(DEF_USE_WUEST .and. epar .ge. 1.e-12)THEN
         IF(DEF_USE_WUEST .and. .not. abs(c4 - 1) .lt. 0.001)THEN
            IF(omc .lt. ome)THEN
               pco2i = pco2i_c
            ELSE
               pco2i = pco2i_e
            ENDIF
            gsh2o = assmt / (co2a - pco2i/psrf)*1.6
            pco2in = pco2i ! No need to iteratively solve pco2i for WUE model.
                           ! Let pco2in = pco2i to exit loop.
            IF(pco2i .gt. pco2a)THEN
               write(*,*) 'warning: pco2i greater than pco2a, use bb model'
            ENDIF
         ELSE
            IF(DEF_USE_MEDLYNST)THEN
               vpd   = amax1((ei - ea),50._r8) * 1.e-3 ! in kpa
               acp   = 1.6*assmt/co2st                 ! in mol m-2 s-1
               aquad = 1._r8
               bquad = -2*(g0*1.e-6 + acp) - (g1*acp)**2/(gbh2o*vpd)       ! in mol m-2 s-1
               cquad = (g0*1.e-6)**2 + (2*g0*1.e-6+acp*(1-g1**2)/vpd)*acp  ! in (mol m-2 s-1)**2

               sqrtin= max( 0., ( bquad**2 - 4.*aquad*cquad ) )
               gsh2o = ( -bquad + sqrt ( sqrtin ) ) / (2.*aquad)

            ELSE
               hcdma = ei*co2st / ( gradm*assmt )

               aquad = hcdma
               bquad = gbh2o*hcdma - ei - bintc*hcdma
               cquad = -gbh2o*( ea + hcdma*bintc )

               sqrtin= max( 0., ( bquad**2 - 4.*aquad*cquad ) )
               gsh2o = ( -bquad + sqrt ( sqrtin ) ) / (2.*aquad)

               es  = ( gsh2o-bintc ) * hcdma                   ! pa
               es  = min( es, ei )
               es  = max( es, 1.e-2)

               gsh2o = es/hcdma + bintc                        ! mol m-2 s-1
            ENDIF

            pco2in = ( co2s - 1.6 * assimn / gsh2o )*psrf      ! pa
         ENDIF
         eyy(ic) = pco2i - pco2in                              ! pa

!-----------------------------------------------------------------------

         IF( abs(eyy(ic)) .lt. 0.1 ) EXIT

      ENDDO ITERATION_LOOP

! convert gsh2o (mol m-2 s-1) to resistance rst ( s m-1)
      rst   = min( 1.e6, 1./(gsh2o*tlef/tprcor) )     ! s m-1

   END SUBROUTINE stomata



   SUBROUTINE sortin( eyy, pco2y, range, gammas, ic, iterationtotal )

!-----------------------------------------------------------------------
!     arranges successive pco2/error pairs in order of increasing pco2.
!     estimates next guess for pco2 using combination of linear and
!     quadratic fits.
!
!     original author: P. J. Sellers (SiB2)
!-----------------------------------------------------------------------

   USE MOD_Precision
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   integer,  intent(in) :: ic,iterationtotal
   real(r8), intent(in) :: range
   real(r8), intent(in) :: gammas
   real(r8), intent(inout), dimension(iterationtotal) :: eyy, pco2y

!-------------------------- Local Variables ----------------------------
   integer i, j, n, i1, i2, i3, is, isp, ix
   real(r8) a, b, pmin, emin, eyy_a
   real(r8) pco2b, pco2yl, pco2yq
   real(r8) ac1, ac2, bc1, bc2, cc1, cc2
   real(r8) bterm, aterm, cterm

!-----------------------------------------------------------------------

      IF( ic .ge. 4 ) go to 500
      eyy_a = 1.0
      IF(eyy(1).lt.0.) eyy_a = -1.0
      pco2y(1) = gammas + 0.5*range
      pco2y(2) = gammas + range*( 0.5 - 0.3*eyy_a )
      pco2y(3) = pco2y(1) - (pco2y(1)-pco2y(2))/(eyy(1)-eyy(2)+1.e-10)*eyy(1)

      pmin = min( pco2y(1), pco2y(2) )
      emin = min(   eyy(1),   eyy(2) )
      IF ( emin .gt. 0. .and. pco2y(3) .gt. pmin ) pco2y(3) = gammas
      go to 200
500   continue

      n = ic - 1
      DO 1000 j = 2, n
      a = eyy(j)
      b = pco2y(j)
      DO 2000 i = j-1,1,-1
      IF(eyy(i) .le. a ) go to 100
      eyy(i+1) = eyy(i)
      pco2y(i+1) = pco2y(i)
2000  continue
      i = 0
100   eyy(i+1) = a
      pco2y(i+1) = b
1000  continue

      pco2b = 0.
      is    = 1
      DO 3000 ix = 1, n
      IF( eyy(ix) .lt. 0. ) pco2b = pco2y(ix)
      IF( eyy(ix) .lt. 0. ) is = ix
3000  continue
      i1 = is-1
      i1 = max(1, i1)
      i1 = min(n-2, i1)
      i2 = i1 + 1
      i3 = i1 + 2
      isp   = is + 1
      isp = min( isp, n )
      is = isp - 1

      pco2yl=pco2y(is) - (pco2y(is)-pco2y(isp))/(eyy(is)-eyy(isp)+1.e-10)*eyy(is)

!----------------------------------------------------------------------
!   method using a quadratic fit
!----------------------------------------------------------------------

      ac1 = eyy(i1)*eyy(i1) - eyy(i2)*eyy(i2)
      ac2 = eyy(i2)*eyy(i2) - eyy(i3)*eyy(i3)
      bc1 = eyy(i1) - eyy(i2)
      bc2 = eyy(i2) - eyy(i3)
      cc1 = pco2y(i1) - pco2y(i2)
      cc2 = pco2y(i2) - pco2y(i3)
      bterm = (cc1*ac2-cc2*ac1)/(bc1*ac2-ac1*bc2+1.e-10)
      aterm = (cc1-bc1*bterm)/(ac1+1.e-10)
      cterm = pco2y(i2) - aterm*eyy(i2)*eyy(i2) - bterm*eyy(i2)
      pco2yq= cterm
      pco2yq= max( pco2yq, pco2b )
      pco2y(ic) = ( pco2yl+pco2yq)/2.

200   continue

      pco2y(ic) = max ( pco2y(ic), 0.01 )

   END SUBROUTINE sortin

   SUBROUTINE calc_photo_params(tlef, po2m, par , psrf, rstfac, rb, effcon, vmax25, c3c4, &
                               trop, slti, hlti, shti, hhti, trda, trdm, cint, &
                               vm, epar, respc, omss, gbh2o, gammas, rrkk, c3, c4)

   USE MOD_Precision
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   real(r8),intent(in) :: &
            tlef,     &! leaf temperature (K)
            po2m,     &! O2 concentration in atmos. (pascals)
            par,      &! photosynthetic active radiation (W m-2)
            rstfac,   &! canopy resistance stress factors to soil moisture
            rb,       &! boundary resistance from canopy to cas (s m-1)

            effcon,   &! quantum efficiency of RuBP regeneration (mol CO2 / mol quanta)
            vmax25,   &! maximum carboxylation rate at 25 C at canopy top
                       ! the range : 30.e-6 <-> 100.e-6 (mol co2 m-2 s-1)
            trop,     &! temperature coefficient in gs-a model             (298.16)
            slti,     &! slope of low temperature inhibition function      (0.2)
            hlti,     &! 1/2 point of low temperature inhibition function  (288.16)
            shti,     &! slope of high temperature inhibition function     (0.3)
            hhti,     &! 1/2 point of high temperature inhibition function (313.16)
            trda,     &! temperature coefficient in gs-a model             (1.3)
            trdm,     &! temperature coefficient in gs-a model             (328.16)
            psrf       ! surface atmospheric pressure (pa)

   integer, intent(in) :: &
            c3c4       ! 1 for c3, 0 for c4

   real(r8),intent(in), dimension(3) :: &
            cint       ! scaling up from leaf to canopy

   real(r8),intent(out) :: &
            vm,       &! maximum catalytic activity of Rubison (mol co2 m-2 s-1)
            epar,     &! electron transport rate (mol electron m-2 s-1)
            respc,    &! canopy respiration (mol m-2 s-1)
            omss,     &! intermediate calcuation for oms
            gbh2o,    &! one side leaf boundary layer conductance (mol m-2 s-1)
            gammas,   &! CO2 compensation point
            rrkk,     &! kc (1+o2/ko)
            c3,       &! c3 vegetation : 1; 0 for c4
            c4         ! c4 vegetation : 1; 0 for c3

!-------------------------- Local Variables ----------------------------
    real(r8) :: &
            qt,       &! (tleaf - 298.16) / 10
            kc,       &! Michaelis-Menten constant for co2
            ko,       &! Michaelis-Menten constant for o2
            templ,    &! intermediate value
            temph,    &! intermediate value
            rgas,     &! universal gas contant (8.314 J mol-1 K-1)
            jmax25,   &! potential rate of whole-chain electron transport at 25 C
            jmax,     &! potential rate of whole-chain electron transport (mol electron m-2 s-1)
            respcp,   &! respiration fraction of vmax (mol co2 m-2 s-1)
            tprcor     ! coefficient for unit transfer

!-----------------------------------------------------------------------

      c3 = 0.
      IF (c3c4.eq.1) c3 = 1.
      c4 = 1. - c3

!-----------------------------------------------------------------------
! dependence on leaf temperature
!     gammas - CO2 compensation point in the absence of day respiration
!     ko     - Michaelis-Menton constant for carboxylation by Rubisco
!     kc     - Michaelis-Menton constant for oxygenation by Rubisco
!-----------------------------------------------------------------------

      qt = 0.1*( tlef - trop )

      kc = 30.     * 2.1**qt
      ko = 30000.  * 1.2**qt
      gammas = 0.5 * po2m / (2600. * 0.57**qt) * c3        ! = 0. for c4 plant ???

      rrkk = kc * ( 1. + po2m/ko ) * c3

!----------------------------------------------------------------------
! maximun capacity
! vm     - maximum catalytic activity of Rubisco in the presence of
!          saturating level of RuP2 and CO2 (mol m-2s-1)
! jmax   - potential rate of whole-chain electron transport (mol m-2s-1)
! epar   - electron transport rate for a given absorbed photon radiation
! respc  - dark resipration (mol m-2s-1)
! omss   - capacity of the leaf to export or utilize the products of photosynthesis.
! binter - coefficient from observation, 0.01 for c3 plant, 0.04 for c4 plant
!-----------------------------------------------------------------------

      vm = vmax25 * 2.1**qt        ! (mol m-2 s-1)
      templ = 1. + exp(slti*(hlti-tlef))
      temph = 1. + exp(shti*(tlef-hhti))
      vm = vm / temph * rstfac * c3 + vm / (templ*temph) * rstfac * c4
      vm = vm * cint(1)

      rgas = 8.314467591                 ! universal gas constant (J mol-1 K-1)
!---> jmax25 = 2.39 * vmax25 - 14.2e-6   ! (mol m-2 s-1)
!---> jmax25 = 2.1 * vmax25              ! (mol m-2 s-1)
!/05/2014/
      jmax25 = 1.97 * vmax25             ! (mol m-2 s-1)
      jmax = jmax25 * exp( 37.e3 * (tlef - trop) / (rgas*trop*tlef) ) * &
             ( 1. + exp( (710.*trop-220.e3)/(rgas*trop) ) ) / &
             ( 1. + exp( (710.*tlef-220.e3)/(rgas*tlef) ) )
                                         ! 37000  (J mol-1)
                                         ! 220000 (J mol-1)
                                         ! 710    (J K-1)

      jmax = jmax * rstfac
      jmax = jmax * cint(2)

!---> epar = min(4.6e-6 * par * effcon, 0.25*jmax)
! /05/2014/
      epar = min(4.6e-6 * par * effcon, jmax)

      respcp = 0.015 * c3 + 0.025 * c4
      respc = respcp * vmax25 * 2.0**qt / ( 1. + exp( trda*(tlef-trdm )) ) * rstfac
!     respc = 0.7e-6 * 2.0**qt / ( 1. + exp( trda*(tlef-trdm )) ) * rstfac
      respc = respc * cint(1)

      omss = ( vmax25/2. ) * (1.8**qt) / templ * rstfac * c3 &
           + ( vmax25/5. ) * (1.8**qt) * rstfac * c4
      omss = omss * cint(1)

!-----------------------------------------------------------------------
      tprcor = 44.6*273.16*psrf/1.013e5

! one side leaf boundary layer conductance for water vapor [=1/(2*rb)]
! ATTENTION: rb in CLM is for one side leaf, but for SiB2 rb for
! 2-side leaf, so the gbh2o shold be " 0.5/rb * tprcor/tlef "
!     gbh2o  = 0.5/rb * tprcor/tlef                   ! mol m-2 s-1
      gbh2o  = 1./rb * tprcor/tlef                    ! mol m-2 s-1

! rb is for single leaf, but here the flux is for canopy, thus
      ! Xingjie Lu: rb has already been converted to canopy scale,
      ! thus, there is no need for gbh2o *cint(3) (sunlit/shaded LAI)
!     gbh2o  = gbh2o * cint(3)

   END SUBROUTINE calc_photo_params

   SUBROUTINE update_photosyn(tlef, po2m, pco2m, pco2a, par, psrf, rstfac, rb, gsh2o,&
                             effcon, vmax25, c3c4, gradm, trop, slti, hlti, shti, hhti, trda, trdm, cint,&
                             assim, respc)

   USE MOD_Precision
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   real(r8),intent(in) :: &
            tlef,     &! leaf temperature (K)
            po2m,     &! O2 concentration in atmos. (pascals)
            pco2m,    &! CO2 concentration in atmos. (pascals)
            pco2a,    &! CO2 concentration in canopy air space (pa)
            par,      &! photosynthetic active radiation (W m-2)
            psrf,     &! surface atmospheric pressure (pa)
            rstfac,   &! canopy resistance stress factors to soil moisture
            rb,       &! boundary resistance from canopy to cas (s m-1)
            gsh2o,    &! canopy conductance (mol m-2 s-1)

            effcon,   &! quantum efficiency of RuBP regeneration (mol CO2 / mol quanta)
            vmax25,   &! maximum carboxylation rate at 25 C at canopy top
                       ! the range : 30.e-6 <-> 100.e-6 (mol co2 m-2 s-1)
            gradm,    &! conductance-photosynthesis slope parameter
            trop,     &! temperature coefficient in gs-a model             (298.16)
            slti,     &! slope of low temperature inhibition function      (0.2)
            hlti,     &! 1/2 point of low temperature inhibition function  (288.16)
            shti,     &! slope of high temperature inhibition function     (0.3)
            hhti,     &! 1/2 point of high temperature inhibition function (313.16)
            trda,     &! temperature coefficient in gs-a model             (1.3)
            trdm       ! temperature coefficient in gs-a model             (328.16)

   integer, intent(in) :: &
            c3c4       ! 1 for c3, 0 for c4

   real(r8),intent(in), dimension(3) :: &
            cint       ! scaling up from leaf to canopy

   real(r8),intent(out) :: &
            assim,    &! canopy assimilation rate (mol m-2 s-1)
            respc      ! canopy respiration (mol m-2 s-1)

!-------------------------- Local Variables ----------------------------
   real(r8) ::        &
            vm,       &! maximum catalytic activity of Rubison (mol co2 m-2 s-1)
            epar,     &! electron transport rate (mol electron m-2 s-1)
            gbh2o,    &! one side leaf boundary layer conductance (mol m-2 s-1)
            gammas,   &! CO2 compensation point
            rrkk,     &! kc (1+o2/ko)
            c3,       &! c3 vegetation : 1; 0 for c4
            c4         ! c4 vegetation : 1; 0 for c3

   real(r8) ::        &
            atheta,   &! wc, we coupling parameter
            btheta,   &! wc & we, ws coupling parameter
            omss,     &! intermediate calcuation for oms
            omc,      &! rubisco limited assimilation (omega-c: mol m-2 s-1)
            ome,      &! light limited assimilation (omega-e: mol m-2 s-1)
            oms,      &! sink limited assimilation (omega-s: mol m-2 s-1)
            omp,      &! intermediate calcuation for omc, ome

            co2a,     &! co2 concentration at cas (mol mol-1)
            co2s,     &! co2 concentration at canopy surface (mol mol-1)
            co2st,    &! co2 concentration at canopy surface (mol mol-1)
            co2i,     &! internal co2 concentration (mol mol-1)
            pco2in,   &! internal co2 concentration at the new iteration (pa)
            pco2i,    &! internal co2 concentration (pa)
            es,       &! canopy surface h2o vapor pressure (pa)

            sqrtin,   &! intermediate calculation for quadratic
            assmt,    &! net assimilation with a positive limitation (mol co2 m-2 s-1)
            assimn     ! net assimilation (mol co2 m-2 s-1)

   integer, parameter :: iterationtotal = 6   ! total iteration number in pco2i calculation

   real(r8) :: &
            eyy(iterationtotal),    &! differnce of pco2i at two iteration step
            pco2y(iterationtotal),  &! adjusted to total iteration number
            range                    !

   integer ic
!-----------------------------------------------------------------------

      CALL calc_photo_params(tlef, po2m, par , psrf, rstfac, rb, effcon, vmax25, c3c4, &
                             trop, slti, hlti, shti, hhti, trda, trdm, cint, &
                             vm, epar, respc, omss, gbh2o, gammas, rrkk, c3, c4)

      co2a = pco2a/psrf

      range = pco2m * ( 1. - 1.6/gradm ) - gammas

      DO ic = 1, iterationtotal    ! loop for total iteration number
         pco2y(ic) = 0.
         eyy(ic) = 0.
      ENDDO

      ITERATION_LOOP_UPDATE: DO ic = 1, iterationtotal

         CALL sortin(eyy, pco2y, range, gammas, ic, iterationtotal)
         pco2i =  pco2y(ic)

!-----------------------------------------------------------------------
!                      NET ASSIMILATION
!     the leaf assimilation (or gross photosynthesis) rate is described
!     as the minimum of three limiting rates:
!     omc: the efficiency of the photosynthetic enzyme system (Rubisco-limited);
!     ome: the amount of PAR captured by leaf chlorophyll;
!     oms: the capacity of the leaf to export or utilize the products of photosynthesis.
!     to aviod the abrupt transitions, two quadratic equations are used:
!             atheta*omp^2 - omp*(omc+ome) + omc*ome = 0
!         btheta*assim^2 - assim*(omp+oms) + omp*oms = 0
!-----------------------------------------------------------------------

         atheta = 0.877
         btheta = 0.95

         omc = vm   * ( pco2i-gammas ) / ( pco2i + rrkk ) * c3 + vm * c4
         ome = epar * ( pco2i-gammas ) / ( pco2i+2.*gammas ) * c3 + epar * c4
         IF(.not. DEF_USE_WUEST .or. abs(c4 - 1) .lt. 0.001)THEN
            oms = omss * c3 + omss*pco2i * c4

            sqrtin= max( 0., ( (ome+omc)**2 - 4.*atheta*ome*omc ) )
            omp   = ( ( ome+omc ) - sqrt( sqrtin ) ) / ( 2.*atheta )
            sqrtin= max( 0., ( (omp+oms)**2 - 4.*btheta*omp*oms ) )
            assim = max( 0., ( ( oms+omp ) - sqrt( sqrtin ) ) / ( 2.*btheta ))
         ELSE
            assim = max( 0., min(omc, ome))
         ENDIF

         assimn= ( assim - respc)                         ! mol m-2 s-1

!-----------------------------------------------------------------------
!                      STOMATAL CONDUCTANCE
!
!  (1)   pathway for co2 flux
!                                                  co2m
!                                                   o
!                                                   |
!                                                   |
!                                                   <  |
!                                        1.37/gsh2o >  |  Ac-Rd-Rsoil
!                                                   <  v
!                                                   |
!                                     <--- Ac-Rd    |
!     o------/\/\/\/\/\------o------/\/\/\/\/\------o
!    co2i     1.6/gsh2o     co2s    1.37/gbh2o     co2a
!                                                   | ^
!                                                   | | Rsoil
!                                                   | |
!
!  (2)   pathway for water vapor flux
!
!                                                  em
!                                                   o
!                                                   |
!                                                   |
!                                                   <  ^
!                                           1/gsh2o >  | Ea
!                                                   <  |
!                                                   |
!                                     ---> Ec       !
!     o------/\/\/\/\/\------o------/\/\/\/\/\------o
!     ei       1/gsh2o      es       1/gbh2o       ea
!                                                   | ^
!                                                   | | Eg
!                                                   | |
!
!  (3)   the relationship between net assimilation and tomatal conductance :
!        gsh2o = m * An * [es/ei] / [pco2s/p] + b
!        es = [gsh2o *ei + gbh2o * ea] / [gsh2o + gbh2o]
!        ===>
!        a*gsh2o^2 + b*gsh2o + c = 0
!
!-----------------------------------------------------------------------

         co2s = co2a - 1.37*assimn/gbh2o                  ! mol mol-1
         co2st = min( co2s, co2a )
         co2st = max( co2st,1.e-5 )

         assmt = max( 1.e-12, assimn )


         pco2in = ( co2s - 1.6 * assmt / gsh2o )*psrf    ! pa

         eyy(ic) = pco2i - pco2in                         ! pa

!-----------------------------------------------------------------------

         IF( abs(eyy(ic)) .lt. 0.1 ) EXIT

      ENDDO ITERATION_LOOP_UPDATE

   END SUBROUTINE update_photosyn

   SUBROUTINE WUE_solver(gammas, lambda, co2a, ei, ea, psrf, pco2i_c, pco2i_e)

!-----------------------------------------------------------------------
! Solve internal co2 concentration for Rubisco limit and RuBP regeneration limit.
!
! When Rubisco is limit (omc < ome), solve following equation (Liang et al., 2023, S18a)
! for pco2i_c:
!  {1-(1.6*D)/[lambda*(gammas+rrkk)]} * co2i_c^2                                        &
!    - {2*co2a+[1.6*D*(rrkk-gammas)]/[lambda*(gammas+rrkk)]-(1.6*D)/lambda} * co2i_c    &
!    + {co2a^2 - (1.6*D*co2a)/lambda + (1.6*D*rrkk*gammas)/[lambda*(gammas+rrkk)]}    = 0
!
! When RuBP is limit (omc>=ome), solve following equation (Liang et al., 2023, S18b)
! for pco2i_e:
!  [1-(1.6*D)/(3*lambda*gammas)] * co2i_e^2                                             &
!    - [2*co2a-(3.2*D)/(3*lambda)] * co2i_e                                             &
!    + [co2a^2 - (1.6*D*co2a)/lambda + (3.2*D*gammas)/(3*lambda)]                     = 0
!-----------------------------------------------------------------------

   USE MOD_Precision
   IMPLICIT NONE

!-------------------------- Dummy Arguments ----------------------------
   real(r8),intent(in) :: &
            gammas,   &! CO2 compensation point (pa)
            lambda,   &! marginal water use efficiency ((mol h2o) (mol co2)-1)
            co2a,     &! co2 concentration at cas ((mol co2) (mol air)-1)
            ea,       &! canopy air space vapor pressure (pa)
            ei,       &! saturation h2o vapor pressure in leaf stomata (pa)
            psrf       ! air pressure (pa)

   real(r8),intent(out) :: &
            pco2i_c,  &! internal co2 concentration when Rubisco is limited (pa)
            pco2i_e    ! internal co2 concentration when RuBP regeneration is limited (pa)

!-------------------------- Local Variables ----------------------------
   real(r8) :: &
            D,        &! leaf-to-air-vapour mole fraction difference ((mol h2o) (mol air)-1)
            co2i_c,   &! internal co2 concentration when Rubisco is limited ((mol co2) (mol air)-1)
            co2i_e     ! internal co2 concentration when RuBP is limited ((mol co2) (mol air)-1)

!-----------------------------------------------------------------------

      ! solve co2i_c
      D = amax1((ei - ea),50._r8) / psrf

      co2i_c = co2a - sqrt(1.6*D*(amax1(co2a-gammas/psrf,0._r8))/lambda)
      co2i_e = co2a - co2a / ( 1 + 1.37 * sqrt(lambda * gammas/psrf / D))

      pco2i_c = co2i_c * psrf
      pco2i_e = co2i_e * psrf

   END SUBROUTINE WUE_solver

END MODULE MOD_AssimStomataConductance
! -------------- EOP ---------------
