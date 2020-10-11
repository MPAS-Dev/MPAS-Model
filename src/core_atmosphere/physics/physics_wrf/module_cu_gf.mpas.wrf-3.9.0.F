!=================================================================================================================
! MPAS driver for the Grell-Freitas parameterization of convection from WRF version 3.9.0:
! Laura D. Fowler (laura@ucar.edu) / 2017-03-09.

! modified made to sourcecode:
! * for shallow, middle-level, and deep convection, added the arrays cutens, cutenm, and cutens that are used to
!   set to zero outputs (precipitation, massflux, tendencies,...) when precipitation or massflux are negative.
!   cutens, cutenm, and cuten are technically not needed, but they prevent occurences of spurious data when local
!   variables are not properly reset to zero when convective clouds are removed inside the subroutines cup_gf_sh
!   and cup_gf. cutens, cutenm, and cuten will be removed in a future revision of the driver.
!   Laura D. Fowler (laura@ucar.edu) / 2017-08-08.
! * for shallow, middle-level, and deep convection, added diagnostics of the updraft and downdraft massflux.
!   Laura D. Fowler (laura@ucar.edu) / 2017-09-11.
! * added the input optional variables nc,ni,nifa,and nwfa, and output optional variables rnccuten,rnicuten,
!   rnifacuten,and rnwfacuten to include the convective transport for the number concentration of cloud liquid
!   water and cloud ice,and for the number concentation of "ice-friendly" and "water-friendly" aerosols.
!   Laura D. Fowler (laura@ucar.edu) / 2019-12-02.

!=================================================================================================================
 module module_cu_gf_wrf390
 use mpas_log
 use mpas_atmphys_constants,only: cp,g=>gravity,r_v=>R_v,xlv
 use module_cu_gf_deep,only  : cu_gf_deep_run
 use module_cu_gf_sh,only    : cu_gf_sh_run
 use module_cu_gf_shared,only: neg_check

 implicit none
 private
 public:: cu_grell_freitas_wrf390

 contains

!-----------------------------------------------------------------------------------------------------------------
 subroutine cu_grell_freitas_wrf390( &
               dt,dx,area,u,v,w,t,q,rho,p,pi,p8w,dz8w,       &
               ht,xland,hfx,qfx,kpbl,rthraten,rthften,       &
               rqvften,rqvblten,rthblten,                    &
               do_deepc,do_midlc,do_shalc,                   &
               ichoice,ichoice_m,ichoice_s,                  &
               kbcon_deepc,ktop_deepc,k22_deepc,             &
               kbcon_midlc,ktop_midlc,k22_midlc,             &
               kbcon_shalc,ktop_shalc,k22_shalc,             &
               xmb_deepc,xmb_midlc,xmb_shalc,                &
               zu_deepc,zd_deepc,zu_midlc,zd_midlc,          &
               zu_shalc,rthcuten,rqvcuten,rqccuten,          &
               rqicuten,rucuten,rvcuten,rthcuten_deepc,      &
               rqvcuten_deepc,rqccuten_deepc,rqicuten_deepc, &
               rthcuten_midlc,rqvcuten_midlc,rqccuten_midlc, &
               rqicuten_midlc,rthcuten_shalc,rqvcuten_shalc, &
               rqccuten_shalc,rqicuten_shalc,                &
               rnccuten,rnicuten,rnifacuten,rnwfacuten,      &
               gdc,gdc2,                                     &
               pratec,pratecd,pratecm,pratecs,raincv,        &
               raincvd,raincvm,raincvs,nclos,weights_clos,   &
               nc,ni,nifa,nwfa,                              &
               ids,ide,jds,jde,kds,kde,                      &
               ims,ime,jms,jme,kms,kme,                      &
               its,ite,jts,jte,kts,kte                       &
                                   )
!-----------------------------------------------------------------------------------------------------------------

!input variables:
 logical,intent(in):: do_deepc,do_midlc,do_shalc

 integer,intent(in):: ids,ide, jds,jde, kds,kde, & 
                      ims,ime, jms,jme, kms,kme, & 
                      its,ite, jts,jte, kts,kte
 integer,intent(in):: ichoice,ichoice_m,ichoice_s
 integer,intent(in):: nclos
 integer,intent(in),dimension(ims:ime,jms:jme):: kpbl

 real,intent(in):: dt
 real,intent(in),dimension(ims:ime,jms:jme):: area,dx,ht,xland
 real,intent(in),dimension(ims:ime,jms:jme):: hfx,qfx
 real,intent(in),dimension(ims:ime,kms:kme,jms:jme):: u,v,w,dz8w,pi,p,p8w,t,q,rho
 real,intent(in),dimension(ims:ime,kms:kme,jms:jme):: rthraten,rthblten,rthften,rqvblten,rqvften

 real,intent(in),dimension(ims:ime,kms:kme,jms:jme),optional:: nc,ni,nifa,nwfa

!inout variables:
 integer,intent(inout),dimension(ims:ime,jms:jme),optional:: k22_deepc,kbcon_deepc,ktop_deepc
 integer,intent(inout),dimension(ims:ime,jms:jme),optional:: k22_midlc,kbcon_midlc,ktop_midlc
 integer,intent(inout),dimension(ims:ime,jms:jme),optional:: k22_shalc,kbcon_shalc,ktop_shalc

 real,intent(inout),dimension(ims:ime,jms:jme):: raincv,pratec
 real,intent(inout),dimension(ims:ime,jms:jme),optional:: raincvs,raincvm,raincvd,pratecs,pratecm,pratecd
 real,intent(inout),dimension(ims:ime,jms:jme),optional:: xmb_deepc,xmb_midlc,xmb_shalc

 real,intent(inout),dimension(ims:ime,kms:kme,jms:jme):: rthcuten,rqvcuten,rqccuten
 real,intent(inout),dimension(ims:ime,kms:kme,jms:jme),optional:: rucuten,rvcuten
 real,intent(inout),dimension(ims:ime,kms:kme,jms:jme),optional:: gdc,gdc2,rqicuten
 real,intent(inout),dimension(ims:ime,kms:kme,jms:jme),optional:: zu_deepc,zd_deepc,zu_midlc,zd_midlc,zu_shalc
 real,intent(inout),dimension(ims:ime,kms:kme,jms:jme),optional:: rthcuten_deepc,rqvcuten_deepc,rqccuten_deepc, &
                                                                  rqicuten_deepc,rthcuten_midlc,rqvcuten_midlc, &
                                                                  rqccuten_midlc,rqicuten_midlc,rthcuten_shalc, &
                                                                  rqvcuten_shalc,rqccuten_shalc,rqicuten_shalc
 real,intent(inout),dimension(ims:ime,kms:kme,jms:jme),optional:: rnccuten,rnicuten,rnifacuten,rnwfacuten

 real,intent(inout),dimension(ims:ime,1:nclos,jms:jme),optional:: weights_clos

!local variables:
 character(len=50),dimension(its:ite):: ierrc,ierrcm,ierrcs

 integer,parameter:: ntrans=2

 integer:: i,ipr,k,j,n
 integer:: itf,ktf,jtf
 integer:: kb,kt
 integer,dimension(its:ite):: ierr,ierrm,ierrs
 integer,dimension(its:ite):: k22,kbcon,ktop,k22m,kbconm,ktopm,k22s,kbcons,ktops
 integer,dimension(its:ite):: kpbli,cactiv
 integer,dimension(its:ite):: jmin,jminm,tropics

 real:: dq
 real,dimension(its:ite):: areai,dxi
 real,dimension(its:ite):: hfxi,qfxi,psur,ter11,xlandi
 real,dimension(its:ite):: ccn,mconv
 real,dimension(its:ite):: cuten,cutenm,cutens
 real,dimension(its:ite):: pret,pretm,prets,edt,edtd,edtm,xmb,xmbm,xmbs
 real,dimension(its:ite):: dummy

 real,dimension(its:ite,10):: forcing,forcingm

 real,dimension(its:ite,kts:kte):: q2d,t2d,p2d,rhoi,us,vs,omeg,zo
 real,dimension(its:ite,kts:kte):: nc2d,ni2d,nifa2d,nwfa2d
 real,dimension(its:ite,kts:kte):: qcheck

 real,dimension(its:ite,kts:kte):: cnvwt,cnvwtm,zd,zdm,zu,zum,zus
 real,dimension(its:ite,kts:kte):: tshall,qshall,dhdt,tn,qo

 real,dimension(its:ite,kts:kte):: outt,outq,outqc,cupclw,outu,outv
 real,dimension(its:ite,kts:kte):: outtm,outqm,outqcm,cupclwm,outum,outvm
 real,dimension(its:ite,kts:kte):: outts,outqs,outqcs,cupclws,outus,outvs

 real,dimension(its:ite,kts:kte):: outnc,outni,outnifa,outnwfa
 real,dimension(its:ite,kts:kte):: outncm,outnim,outnifam,outnwfam
 real,dimension(its:ite,kts:kte):: outncs,outnis,outnifas,outnwfas

 real,dimension(its:ite,kts:kte,ntrans):: qtrans2d
 real,dimension(its:ite,kts:kte,ntrans):: outqtrans,outqtransm,outqtranss

 real,dimension(its:ite,1:nclos):: xf_ens

!local variables for stochastic parameterization:
 real,dimension(its:ite)  :: rand_mom,rand_vmas
 real,dimension(its:ite,4):: rand_clos,rstochcol

!parameters:
 integer,parameter:: imidc     = 1 !- mid-level convection
 integer,parameter:: imidd     = 0
 integer,parameter:: dicycle   = 2 !- diurnal cycle flag
 integer,parameter:: dicycle_m = 0 !- diurnal cycle flag
 integer,parameter:: nranflag  = 0

 real,parameter:: aodccn = 0.1
 real,parameter:: tcrit  = 258.
 real,parameter:: ccn0   = 150.

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write(' ')
!call mpas_log_write('--- enter subroutine cu_grell_freitas_wrf390:')

!--- initialization of local dimensions:
 itf = min(ite,ide-1)
 ktf = min(kte,kde-1)
 jtf = min(jte,jde-1)

!--- initialization for stochastic parameterization:
 rstochcol = 0.
 do i = its, ite
    rand_mom(i)  = 0.
    rand_vmas(i) = 0.
    do n = 1, 4
       rand_clos(i,n) = 0.
       rstochcol(i,n) = 0.
    enddo
 enddo

!--- initialization of a grid-point:
 ipr = 1

!--- initialization of inout and ouput variables:
 do j = jts, jte
    do i = its, ite
       if(present(k22_deepc)  ) k22_deepc(i,j)   = 0
       if(present(kbcon_deepc)) kbcon_deepc(i,j) = 0
       if(present(ktop_deepc) ) ktop_deepc(i,j)  = 0
       if(present(k22_midlc)  ) k22_midlc(i,j)   = 0
       if(present(kbcon_midlc)) kbcon_midlc(i,j) = 0
       if(present(ktop_midlc) ) ktop_midlc(i,j)  = 0
       if(present(k22_shalc)  ) k22_shalc(i,j)   = 0
       if(present(kbcon_shalc)) kbcon_shalc(i,j) = 0
       if(present(ktop_shalc) ) ktop_shalc(i,j)  = 0

       raincv(i,j) = 0.
       pratec(i,j) = 0.
       if(present(raincvd)) raincvd(i,j) = 0.
       if(present(raincvm)) raincvm(i,j) = 0.
       if(present(raincvs)) raincvs(i,j) = 0.
       if(present(pratecd)) pratecd(i,j) = 0.
       if(present(pratecm)) pratecm(i,j) = 0.
       if(present(pratecs)) pratecs(i,j) = 0.

       if(present(xmb_deepc)) xmb_deepc(i,j) = 0.
       if(present(xmb_midlc)) xmb_midlc(i,j) = 0.
       if(present(xmb_shalc)) xmb_shalc(i,j) = 0.
    enddo

    do k = kts, kte
       do i = its, ite
          rthcuten(i,k,j) = 0.
          rqvcuten(i,k,j) = 0.
          rqccuten(i,k,j) = 0.
          if(present(rqicuten)) rqicuten(i,k,j) = 0.
          if(present(gdc))  gdc(i,k,j)  = 0.
          if(present(gdc2)) gdc2(i,k,j) = 0.

          if(present(rucuten)) rucuten(i,k,j) = 0.
          if(present(rvcuten)) rvcuten(i,k,j) = 0.

          if(present(zu_deepc)) zu_deepc(i,k,j) = 0.
          if(present(zd_deepc)) zd_deepc(i,k,j) = 0.
          if(present(zu_midlc)) zu_midlc(i,k,j) = 0.
          if(present(zd_midlc)) zd_midlc(i,k,j) = 0.
          if(present(zu_shalc)) zu_shalc(i,k,j) = 0.

          if(present(rthcuten_deepc)) rthcuten_deepc(i,k,j) = 0.
          if(present(rqvcuten_deepc)) rqvcuten_deepc(i,k,j) = 0.
          if(present(rqccuten_deepc)) rqccuten_deepc(i,k,j) = 0.
          if(present(rqicuten_deepc)) rqicuten_deepc(i,k,j) = 0.
          if(present(rthcuten_midlc)) rthcuten_midlc(i,k,j) = 0.
          if(present(rqvcuten_midlc)) rqvcuten_midlc(i,k,j) = 0.
          if(present(rqccuten_midlc)) rqccuten_midlc(i,k,j) = 0.
          if(present(rqicuten_midlc)) rqicuten_midlc(i,k,j) = 0.
          if(present(rthcuten_shalc)) rthcuten_shalc(i,k,j) = 0.
          if(present(rqvcuten_shalc)) rqvcuten_shalc(i,k,j) = 0.
          if(present(rqccuten_shalc)) rqccuten_shalc(i,k,j) = 0.
          if(present(rqicuten_shalc)) rqicuten_shalc(i,k,j) = 0.

          if(present(rnccuten))   rnccuten(i,k,j)   = 0.
          if(present(rnicuten))   rnicuten(i,k,j)   = 0.
          if(present(rnifacuten)) rnifacuten(i,k,j) = 0.
          if(present(rnwfacuten)) rnwfacuten(i,k,j) = 0.
       enddo
    enddo
    if(present(rnccuten)) then
       do k = kts, kte
          do i = its, ite
             rnccuten(i,k,j) = 0.
          enddo
       enddo
    endif
    if(present(rnicuten)) then
       do k = kts, kte
          do i = its, ite
             rnicuten(i,k,j) = 0.
          enddo
       enddo
    endif
    if(present(rnifacuten)) then
       do k = kts, kte
          do i = its, ite
             rnifacuten(i,k,j) = 0.
          enddo
       enddo
    endif
    if(present(rnwfacuten)) then
       do k = kts, kte
          do i = its, ite
             rnwfacuten(i,k,j) = 0.
          enddo
       enddo
    endif
!   if(present(rqtranscuten)) then
!      do k = kts, kte
!         do i = its, ite
!            do n = 1, ntrans
!               rqtranscuten(i,k,j,n) = 0.
!            enddo
!         enddo
!      enddo
!   endif

    if(present(weights_clos)) then
       do k = 1, nclos
          do i = its, ite
             weights_clos(i,k,j) = 0.
          enddo
       enddo
    endif
 enddo

 j_loop: do j = jts, jtf

    !--- initialization of local variables:
    do i = its, itf
       tropics(i) = 0
       cactiv(i)  = 0
       ccn(i)     = ccn0
       mconv(i)   = 0.
       edtd(i)    = 0.

       !deep convection:
       forcing(i,:)  = 0.
       jmin(i)  = 0
       ierrc(i) = " "
       ierr(i)  = 0
       kbcon(i) = 0
       ktop(i)  = 0
       k22(i)   = 0
       xmb(i)   = 0.
       pret(i)  = 0.
       edt(i)   = 0.
       cuten(i) = 0.

       !mid-level convection:
       forcingm(i,:) = 0.
       jminm(i)  = 0
       ierrcm(i) = " "
       ierrm(i)  = 0
       kbconm(i) = 0
       ktopm(i)  = 0
       k22m(i)   = 0
       xmbm(i)   = 0.
       pretm(i)  = 0.
       edtm(i)   = 0.
       cutenm(i) = 0.
       dummy(i)  = 0.

       !shallow convection:
       ierrcs(i) = " "
       ierrs(i)  = 0
       kbcons(i) = 0
       ktops(i)  = 0
       k22s(i)   = 0
       xmbs(i)   = 0.
       prets(i)  = 0.
       cutens(i) = 0.
    enddo

    do k = 1, nclos
       do i = its, itf
          xf_ens(i,k) = 0.
       enddo
    enddo

    do k = kts, ktf
       do i = its, itf
          qcheck(i,k) = 0.
          omeg(i,k)   = 0.

          !deep convection:
          outt(i,k)     = 0.
          outu(i,k)     = 0.
          outv(i,k)     = 0.
          outq(i,k)     = 0.
          outqc(i,k)    = 0.
          outnc(i,k)    = 0.
          outni(i,k)    = 0.
          outnifa(i,k)  = 0.
          outnwfa(i,k)  = 0.
          cupclw(i,k)   = 0.
          cnvwt(i,k)    = 0.
          zd(i,k)       = 0.
          zu(i,k)       = 0.
          do n = 1, ntrans
             outqtrans(i,k,n)  = 0.
          enddo

          !mid-level convection:
          outtm(i,k)    = 0.
          outum(i,k)    = 0.
          outvm(i,k)    = 0.
          outqm(i,k)    = 0.
          outqcm(i,k)   = 0.
          outncm(i,k)   = 0.
          outnim(i,k)   = 0.
          outnifam(i,k) = 0.
          outnwfam(i,k) = 0.
          cupclwm(i,k)  = 0.
          cnvwtm(i,k)   = 0.
          zdm(i,k)      = 0.
          zum(i,k)      = 0.
          do n = 1, ntrans
             outqtransm(i,k,n) = 0.
          enddo

          !shallow convection:
          outts(i,k)    = 0.
          outus(i,k)    = 0.
          outvs(i,k)    = 0.
          outqs(i,k)    = 0.
          outqcs(i,k)   = 0.
          cupclws(i,k)  = 0.
          outncs(i,k)   = 0.
          outnis(i,k)   = 0.
          outnifas(i,k) = 0.
          outnwfas(i,k) = 0.
          zus(i,k)      = 0.
          do n = 1, ntrans
             outqtranss(i,k,n) = 0.
          enddo
       enddo
    enddo

    do i = its, itf
       areai(i)  = area(i,j)
       dxi(i)    = dx(i,j)
       xlandi(i) = xland(i,j)
       ter11(i)  = max(0.,ht(i,j))
       zo(i,kts) = ter11(i) + 0.5*dz8w(i,1,j)
       do k = kts+1, ktf
          zo(i,k) = zo(i,k-1) + 0.5*(dz8w(i,k-1,j)+dz8w(i,k,j))
       enddo
       hfxi(i)  = hfx(i,j)
       qfxi(i)  = qfx(i,j)
       kpbli(i) = kpbl(i,j)
       psur(i)  = p8w(i,1,j)*.01
    enddo

    do k = kts, ktf
       do i = its, itf
          us(i,k)   = u(i,k,j)
          vs(i,k)   = v(i,k,j)
          rhoi(i,k) = rho(i,k,j)
          t2d(i,k)  = t(i,k,j)
          q2d(i,k)  = q(i,k,j)
          if(q2d(i,k) .lt. 1.e-08) q2d(i,k)=1.e-08

          !forcing for shallow convection:
          tshall(i,k) = t2d(i,k) + rthblten(i,k,j)*pi(i,k,j)*dt
          qshall(i,k) = q2d(i,k) + rqvblten(i,k,j)*dt
          dhdt(i,k)   = cp*rthblten(i,k,j)*pi(i,k,j) + xlv*rqvblten(i,k,j)

          !forcing for deep convection:
          tn(i,k) = t2d(i,k) + (rthften(i,k,j)+rthraten(i,k,j)+rthblten(i,k,j))*pi(i,k,j)*dt
          qo(i,k) = q2d(i,k) + (rqvften(i,k,j)+rqvblten(i,k,j))*dt
          if(tn(i,k) .lt. 200.)   tn(i,k) = t2d(i,k)
          if(qo(i,k) .lt. 1.e-08) qo(i,k) = 1.e-08

          p2d(i,k) = p(i,k,j)*0.01
       enddo
    enddo
    if(present(nc)) then
       do k = kts, ktf
          do i = its, ite
             nc2d(i,k) = nc(i,k,j)
          enddo
       enddo
    else
       do k = kts, ktf
          do i = its, ite
             nc2d(i,k) = 0.
          enddo
       enddo
    endif
    if(present(ni)) then
       do k = kts, ktf
          do i = its, ite
             ni2d(i,k) = ni(i,k,j)
          enddo
       enddo
    else
       do k = kts, ktf
          do i = its, ite
             ni2d(i,k) = 0.
          enddo
       enddo
    endif
    if(present(nifa)) then
       do k = kts, ktf
          do i = its, ite
             nifa2d(i,k) = nifa(i,k,j)
          enddo
       enddo
    else
       do k = kts, ktf
          do i = its, ite
             nifa2d(i,k) = 0.
          enddo
       enddo
    endif
    if(present(nwfa)) then
       do k = kts, ktf
          do i = its, ite
             nwfa2d(i,k) = nwfa(i,k,j)
          enddo
       enddo
    else
       do k = kts, ktf
          do i = its, ite
             nwfa2d(i,k) = 0.
          enddo
       enddo
    endif
!   if(present(qtrans)) then
!      do n = 1, ntrans
!         do k = kts, ktf
!            do i = its, ite
!               qtrans2d(i,k,n) = qtrans(i,k,j,n)
!            enddo
!         enddo
!      enddo
!   else
       do n = 1, ntrans
          do k = kts, ktf
             do i = its, ite
                qtrans2d(i,k,n) = 0.
             enddo
          enddo
       enddo
!   endif

    !--- calculation of the moisture convergence: we do not need to calculate
    !the moisture convergence since it is initialized to zero and calculated
    !in the subroutine cup_gf. Also,now follow exactly the calculation of omeg,
    !as in WRF 3.9.0:
!   do k = kts+1, ktf
!      do i = its, itf
!         omeg(i,k) = -g*0.5*(rho(i,k,j)+rho(i,k-1,j))*w(i,k,j)
!      enddo
!   enddo
    do k = kts, ktf
       do i = its, itf
          omeg(i,k) = -g*rho(i,k,j)*w(i,k,j)
       enddo
    enddo

    !--- call to shallow convection:
    if(do_shalc) then
       call mpas_log_write('  ')
       call mpas_log_write('--- enter subroutine cu_gf_sh_run:')
       call cu_gf_sh_run( &
                !input variables:
                us,vs,zo,t2d,q2d,ter11,tshall,qshall,p2d,psur,dhdt, &
                kpbli,rhoi,hfxi,qfxi,xlandi,ichoice_s,tcrit,dt,     &
                zus,xmbs,kbcons,ktops,k22s,ierrs,ierrcs,            &
                outts,outqs,outqcs,outus,outvs,cnvwt,prets,cupclws, &
                itf,ktf,its,ite,kts,kte,ipr,tropics)

       do k = kts, ktf
          do i = its, itf
             qcheck(i,k) = q2d(i,k)
          enddo
       enddo
       call neg_check('shallow',ipr,dt,qcheck,outqs,outts,outus,outvs, &
                      outqcs,prets,its,ite,kts,kte,itf,ktf,ktops)

       do i = its,itf
          if(xmbs(i) .gt. 0.) then
             cutens(i) = 1.
          else
             k22s(i)   = 0
             kbcons(i) = 0
             ktops(i)  = 0
             xmbs(i)   = 0.
             prets(i)  = 0.
          endif
          if(present(k22_shalc)  ) k22_shalc(i,j)   = k22s(i)
          if(present(kbcon_shalc)) kbcon_shalc(i,j) = kbcons(i)
          if(present(ktop_shalc) ) ktop_shalc(i,j)  = ktops(i)
          if(present(xmb_shalc)  ) xmb_shalc(i,j)   = xmbs(i)
       enddo
       call mpas_log_write('--- end subroutine cu_gf_sh_run:')
    endif


    !--- call to mid-level convection:
    if(do_midlc) then
       call mpas_log_write('  ')
       call mpas_log_write('--- enter subroutine cu_gf_deep_run - mid-level convection:')
       call cu_gf_deep_run( &
                itf,ktf,its,ite,kts,kte,                                       &
                dicycle_m,ichoice_m,ipr,ccn,dt,imidc,kpbli,dhdt,               &
                xlandi,zo,forcingm,t2d,q2d,ter11,tshall,qshall,p2d,            &
                psur,us,vs,rhoi,hfxi,qfxi,areai,dxi,mconv,omeg,cactiv,         &
                cnvwtm,zum,zdm,dummy,edtm,dummy,xmbm,dummy,xmbs,pretm,         &
                outum,outvm,outtm,outqm,outqcm,kbconm,ktopm,k22m,cupclwm,      &
                ierrm,ierrcm,nc2d,ni2d,nifa2d,nwfa2d,ntrans,qtrans2d,          &
                outncm,outnim,outnifam,outnwfam,outqtransm,rand_mom,rand_vmas, &
                rand_clos,nranflag,jminm,tropics                               &
                  )

       do k = kts, ktf
          do i = its, itf
             qcheck(i,k) = q2d(i,k) + outqs(i,k)*dt
          enddo
       enddo
       call neg_check('mid',ipr,dt,qcheck,outqm,outtm,outum,outvm, &
                      outqcm,pretm,its,ite,kts,kte,itf,ktf,ktopm)

       do i = its, itf
          if(pretm(i) .gt. 0.) then
             cutenm(i) = 1.
          else
             k22m(i)   = 0
             kbconm(i) = 0
             ktopm(i)  = 0
             xmbm(i)   = 0.
             pretm(i)  = 0.
          endif
          if(present(k22_midlc)  ) k22_midlc(i,j)   = k22m(i)
          if(present(kbcon_midlc)) kbcon_midlc(i,j) = kbconm(i)
          if(present(ktop_midlc) ) ktop_midlc(i,j)  = ktopm(i)
          if(present(xmb_midlc)  ) xmb_midlc(i,j)   = xmbm(i)
       enddo
       call mpas_log_write('--- end subroutine cu_gf_deep_run - mid-level convection')
    endif


    !--- call to deep convection:
    if(do_deepc) then
       call mpas_log_write('  ')
       call mpas_log_write('--- enter subroutine cu_gf_deep_run - deep convection')
       call cu_gf_deep_run( &
                itf,ktf,its,ite,kts,kte,                                  &
                dicycle,ichoice,ipr,ccn,dt,imidd,kpbli,dhdt,              &
                xlandi,zo,forcing,t2d,q2d,ter11,tn,qo,p2d,                &
                psur,us,vs,rhoi,hfxi,qfxi,areai,dxi,mconv,omeg,cactiv,    &
                cnvwt,zu,zd,zdm,edt,edtm,xmb,xmbm,xmbs,pret,              &
                outu,outv,outt,outq,outqc,kbcon,ktop,k22,cupclw,          &
                ierr,ierrc,nc2d,ni2d,nifa2d,nwfa2d,ntrans,qtrans2d,       &
                outnc,outni,outnifa,outnwfa,outqtrans,rand_mom,rand_vmas, &
                rand_clos,nranflag,jmin,tropics                           &
                  )

       do k = kts, ktf
          do i = its, itf
             qcheck(i,k) = q2d(i,k) + (outqs(i,k) + outqm(i,k))*dt
          enddo
       enddo
       call neg_check('deep',ipr,dt,qcheck,outq,outt,outu,outv, &
                      outqc,pret,its,ite,kts,kte,itf,ktf,ktop)

       do i = its, itf
          if(pret(i) .gt. 0.) then
             cuten(i) = 1.
          else
             k22(i)   = 0
             kbcon(i) = 0
             ktop(i)  = 0
             xmb(i)   = 0.
             pret(i)  = 0.
          endif
          if(present(k22_deepc)  ) k22_deepc(i,j)   = k22(i)
          if(present(kbcon_deepc)) kbcon_deepc(i,j) = kbcon(i)
          if(present(ktop_deepc) ) ktop_deepc(i,j)  = ktop(i)
          if(present(xmb_deepc)  ) xmb_deepc(i,j)   = xmb(i)
       enddo

       if(present(weights_clos)) then
          do k = 1, nclos
             do i = its, ite
                if(xmb_deepc(i,j) .gt. 0) &
                   weights_clos(i,k,j) = xf_ens(i,k)/(nclos*xmb_deepc(i,j))
             enddo
          enddo
       endif
       call mpas_log_write('--- end subroutine cu_gf_deep_run - deep convection')
    endif

    !... precipitation:
    do i = its, itf
       pratec(i,j) = cuten(i)*pret(i) + cutenm(i)*pretm(i) + prets(i)
       raincv(i,j) = pratec(i,j)*dt

       if(present(pratecs)) pratecs(i,j) = cutens(i)*prets(i)
       if(present(pratecm)) pratecm(i,j) = cutenm(i)*pretm(i)
       if(present(pratecd)) pratecd(i,j) = cuten(i) *pret(i)
       if(present(raincvs)) raincvs(i,j) = cutens(i)*prets(i)*dt
       if(present(raincvm)) raincvm(i,j) = cutenm(i)*pretm(i)*dt
       if(present(raincvd)) raincvd(i,j) = cuten(i) *pret(i)*dt
    enddo

    !... always save the tendencies of potential temperature, water vapor,  and cloud water.
    !... the tendency for cloud ice is optional.
    do k = kts, ktf
       do i = its, itf
          rthcuten(i,k,j) = (cuten(i)*outt(i,k) + cutenm(i)*outtm(i,k) + cutens(i)*outts(i,k))/pi(i,k,j)
          rqvcuten(i,k,j) = cuten(i)*outq(i,k) + cutenm(i)*outqm(i,k) + cutens(i)*outqs(i,k)
          rqccuten(i,k,j) = cuten(i)*outqc(i,k) + cutenm(i)*outqcm(i,k) + cutens(i)*outqcs(i,k)
          if(present(gdc)) gdc(i,k,j) = cuten(i)*cupclw(i,k) + cutenm(i)*cupclwm(i,k) + cutens(i)*cupclws(i,k)

          if(present(rthcuten_deepc)) rthcuten_deepc(i,k,j) = cuten(i)*outt(i,k)
          if(present(rqvcuten_deepc)) rqvcuten_deepc(i,k,j) = cuten(i)*outq(i,k)
          if(present(rqccuten_deepc)) rqccuten_deepc(i,k,j) = cuten(i)*outqc(i,k)
          if(present(rthcuten_midlc)) rthcuten_midlc(i,k,j) = cutenm(i)*outtm(i,k)
          if(present(rqvcuten_midlc)) rqvcuten_midlc(i,k,j) = cutenm(i)*outqm(i,k)
          if(present(rqccuten_midlc)) rqccuten_midlc(i,k,j) = cutenm(i)*outqcm(i,k)
          if(present(rthcuten_shalc)) rthcuten_shalc(i,k,j) = cutens(i)*outts(i,k)
          if(present(rqvcuten_shalc)) rqvcuten_shalc(i,k,j) = cutens(i)*outqs(i,k)
          if(present(rqccuten_shalc)) rqccuten_shalc(i,k,j) = cutens(i)*outqcs(i,k)

          if(present(rqicuten)) then
             if(t2d(i,k) .lt. tcrit) then
                rqicuten(i,k,j) = rqccuten(i,k,j)
                rqccuten(i,k,j) = 0.
                if(present(gdc2)) then
                   gdc2(i,k,j) = gdc(i,k,j)
                   gdc(i,k,j)  = 0.
                endif
                if(present(rqicuten_deepc)) then
                   rqicuten_deepc(i,k,j) = rqccuten_deepc(i,k,j)
                   rqccuten_deepc(i,k,j) = 0.
                endif
                if(present(rqicuten_midlc)) then
                   rqicuten_midlc(i,k,j) = rqccuten_midlc(i,k,j)
                   rqccuten_midlc(i,k,j) = 0.
                endif
                if(present(rqicuten_shalc)) then
                   rqicuten_shalc(i,k,j) = rqccuten_shalc(i,k,j)
                   rqccuten_shalc(i,k,j) = 0.
                endif
             endif
          endif

          if(present(rucuten)) rucuten(i,k,j)  = cutenm(i)*outum(i,k) + cuten(i)*outu(i,k)
          if(present(rvcuten)) rvcuten(i,k,j)  = cutenm(i)*outvm(i,k) + cuten(i)*outv(i,k)

          if(present(zu_deepc)) zu_deepc(i,k,j) = cuten(i)*zu(i,k)
          if(present(zd_deepc)) zd_deepc(i,k,j) = cuten(i)*zd(i,k)
          if(present(zu_midlc)) zu_midlc(i,k,j) = cutenm(i)*zum(i,k)
          if(present(zd_midlc)) zd_midlc(i,k,j) = cutenm(i)*zdm(i,k)
          if(present(zu_shalc)) zu_shalc(i,k,j) = cutens(i)*zus(i,k)
       enddo
    enddo

    if(present(rnccuten)) then
       do k = kts, ktf
          do i = its, itf
             rnccuten(i,k,j) = cuten(i)*outnc(i,k) + cutenm(i)*outncm(i,k) + cutens(i)*outncs(i,k)
          enddo
       enddo
    endif
    if(present(rnicuten)) then
       do k = kts, ktf
          do i = its, itf
             rnicuten(i,k,j) = cuten(i)*outni(i,k) + cutenm(i)*outnim(i,k) + cutens(i)*outnis(i,k)
          enddo
       enddo
    endif
    if(present(rnifacuten)) then
       do k = kts, ktf
          do i = its, itf
             rnifacuten(i,k,j) = cuten(i)*outnifa(i,k) + cutenm(i)*outnifam(i,k) + cutens(i)*outnifas(i,k)
          enddo
       enddo
    endif
    if(present(rnwfacuten)) then
       do k = kts, ktf
          do i = its, itf
             rnwfacuten(i,k,j) = cuten(i)*outnwfa(i,k) + cutenm(i)*outnwfam(i,k) + cutens(i)*outnwfas(i,k)
          enddo
       enddo
    endif
!   if(present(rqtranscuten)) then
!      do k = kts, ktf
!         do i = its, itf
!            do n = 1, ntrans
!               rqtranscuten(i,k,j,n) = cuten(i)*outqtrans(i,k,n) + cutenm(i)*outqtransm(i,k,n)
!            enddo
!         enddo
!      enddo
!   endif

 enddo j_loop

 end subroutine cu_grell_freitas_wrf390

!=================================================================================================================
 end module module_cu_gf_wrf390
!=================================================================================================================
