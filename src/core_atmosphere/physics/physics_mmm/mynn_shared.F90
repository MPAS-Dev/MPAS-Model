!=================================================================================================================
 module mynn_shared
 use ccpp_kind_types,only: kind_phys

 implicit none
 private
 public:: esat_blend,qsat_blend,xl_blend


!> Constants used for empirical calculations of saturation vapor pressures (in function "esat") and
!! saturation mixing ratios (in function "qsat"), reproduced from module_mp_thompson.F. 
 real(kind=kind_phys),parameter:: j0 = .611583699e03
 real(kind=kind_phys),parameter:: j1 = .444606896e02
 real(kind=kind_phys),parameter:: j2 = .143177157e01
 real(kind=kind_phys),parameter:: j3 = .264224321e-1
 real(kind=kind_phys),parameter:: j4 = .299291081e-3
 real(kind=kind_phys),parameter:: j5 = .203154182e-5
 real(kind=kind_phys),parameter:: j6 = .702620698e-8
 real(kind=kind_phys),parameter:: j7 = .379534310e-11
 real(kind=kind_phys),parameter:: j8 =-.321582393e-13

 real(kind=kind_phys),parameter:: k0 = .609868993e03
 real(kind=kind_phys),parameter:: k1 = .499320233e02
 real(kind=kind_phys),parameter:: k2 = .184672631e01
 real(kind=kind_phys),parameter:: k3 = .402737184e-1
 real(kind=kind_phys),parameter:: k4 = .565392987e-3
 real(kind=kind_phys),parameter:: k5 = .521693933e-5
 real(kind=kind_phys),parameter:: k6 = .307839583e-7
 real(kind=kind_phys),parameter:: k7 = .105785160e-9
 real(kind=kind_phys),parameter:: k8 = .161444444e-12


contains


!=================================================================================================================
!>\ingroup gsd_mynn_edmf
!! \author JAYMES- added 22 Apr 2015
!! This function calculates saturation vapor pressure.  Separate ice and liquid functions are used (identical to
!! those in module_mp_thompson.F, v3.6). Then, the final returned value is a temperature-dependent "blend".
!! Because the final value is "phase-aware", this formulation may be preferred for use throughout bl_mynn.F and
!! sf_mynn.F (replacing "svp"). 

 function esat_blend(t,t0c,tice)
 implicit none
      
 real(kind=kind_phys),intent(in):: t,t0c,tice
 real(kind=kind_phys):: esat_blend,xc,esl,esi,chi

 xc = max(-80.,t-t0c)

!For 253 < t < 273.16 K, the vapor pressures are "blended" as a function of temperature, using the approach of 
!Chaboureau and Bechtold (2002), JAS, p. 2363.  The resulting values are returned from the function.
 if(t .ge. t0c) then
    esat_blend = j0+xc*(j1+xc*(j2+xc*(j3+xc*(j4+xc*(j5+xc*(j6+xc*(j7+xc*j8))))))) 
 else if(t .le. tice) then
    esat_blend = k0+xc*(k1+xc*(k2+xc*(k3+xc*(k4+xc*(k5+xc*(k6+xc*(k7+xc*k8)))))))
 else
    esl  = j0+xc*(j1+xc*(j2+xc*(j3+xc*(j4+xc*(j5+xc*(j6+xc*(j7+xc*j8)))))))
    esi  = k0+xc*(k1+xc*(k2+xc*(k3+xc*(k4+xc*(k5+xc*(k6+xc*(k7+xc*k8)))))))
    chi  = (273.16-t)/20.16
    esat_blend = (1.-chi)*esl  + chi*esi
 end if

 end function esat_blend

!=================================================================================================================
!>\ingroup gsd_mynn_edmf
!! \author JAYMES- added 22 Apr 2015
!! This function extends function "esat" and returns a "blended" saturation mixing ratio.

 function qsat_blend(t,t0c,tice,p,waterice)
 implicit none

 real(kind=kind_phys),intent(in):: t,t0c,tice,p
 character(len=1),intent(in),optional:: waterice
 character(len=1):: wrt
 real(kind=kind_phys):: qsat_blend,xc,esl,esi,rslf,rsif,chi

 if(.not. present(waterice) ) then 
    wrt = 'b'
 else
    wrt = waterice
 endif

 xc=max(-80.,t-t0c)

 if((t .ge. t0c) .or. (wrt .eq. 'w')) then
    esl  = j0+xc*(j1+xc*(j2+xc*(j3+xc*(j4+xc*(j5+xc*(j6+xc*(j7+xc*j8))))))) 
    qsat_blend = 0.622*esl/(p-esl) 
 else if(t .le. tice) then
    esi  = k0+xc*(k1+xc*(k2+xc*(k3+xc*(k4+xc*(k5+xc*(k6+xc*(k7+xc*k8)))))))
    qsat_blend = 0.622*esi/(p-esi)
 else
    esl  = j0+xc*(j1+xc*(j2+xc*(j3+xc*(j4+xc*(j5+xc*(j6+xc*(j7+xc*j8)))))))
    esi  = k0+xc*(k1+xc*(k2+xc*(k3+xc*(k4+xc*(k5+xc*(k6+xc*(k7+xc*k8)))))))
    rslf = 0.622*esl/(p-esl)
    rsif = 0.622*esi/(p-esi)
    chi  = (t0c-t)/(t0c-tice)
    qsat_blend = (1.-chi)*rslf + chi*rsif
 end if

 end function qsat_blend

!=================================================================================================================
!>\ingroup gsd_mynn_edmf
!! \author jaymes- added 22 apr 2015
!! this function interpolates the latent heats of vaporization and sublimation into a single, temperature-
!! dependent "blended" value, following chaboureau and bechtold (2002) \cite chaboureau_2002, appendix.

 function xl_blend(t,t0c,tice,cice,cliq,cpv,xls,xlv)
 implicit none

 real(kind=kind_phys),intent(in):: t,t0c,tice
 real(kind=kind_phys),intent(in):: cice,cliq,cpv,xls,xlv
 real(kind=kind_phys):: xl_blend,xlvt,xlst,chi

 if(t .ge. t0c) then
    xl_blend = xlv + (cpv-cliq)*(t-t0c)  !vaporization/condensation
 else if (t .le. tice) then
    xl_blend = xls + (cpv-cice)*(t-t0c)  !sublimation/deposition
 else
    xlvt = xlv + (cpv-cliq)*(t-t0c)      !vaporization/condensation
    xlst = xls + (cpv-cice)*(t-t0c)      !sublimation/deposition
    chi  = (t0c-t)/(t0c-tice)
    xl_blend = (1.-chi)*xlvt + chi*xlst     !blended
 end if

 end function xl_blend

!=================================================================================================================
 end module mynn_shared
!=================================================================================================================
