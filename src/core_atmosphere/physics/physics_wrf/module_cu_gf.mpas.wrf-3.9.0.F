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

!=================================================================================================================
 module module_cu_gf_wrf390
 use mpas_log
 use mpas_atmphys_constants,only: cp,g=>gravity,r_v=>R_v,xlv
 use module_cu_gf_deep_wrf390,only: cup_gf,neg_check,autoconv,aeroevap
 use module_cu_gf_sh_wrf390,only: cup_gf_sh

 implicit none
 private
 public:: cu_grell_freitas_wrf390

 contains

!-----------------------------------------------------------------------------------------------------------------
 subroutine cu_grell_freitas_wrf390( &
               dt,dx,area,u,v,w,t,q,rho,p,pi,p8w,dz8w,     &
               ht,xland,hfx,qfx,kpbl,rthraten,rthften,     &
               rqvften,rqvblten,rthblten,                  &
               do_deepc,do_midlc,do_shalc,                 &
               ichoice,ichoice_m,ichoice_s,                &
               kbcon_deepc,ktop_deepc,k22_deepc,           &
               kbcon_midlc,ktop_midlc,k22_midlc,           &
               kbcon_shalc,ktop_shalc,k22_shalc,           &
               xmb_deepc,xmb_midlc,xmb_shalc,              &
               rthcuten,rqvcuten,rqccuten,                 &
               rqicuten,rucuten,rvcuten,gdc,gdc2,          &
               pratec,pratecd,pratecm,pratecs,raincv,      &
               raincvd,raincvm,raincvs,nclos,weights_clos, &
               ids,ide,jds,jde,kds,kde,                    &
               ims,ime,jms,jme,kms,kme,                    &
               its,ite,jts,jte,kts,kte                     &
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

 real,intent(inout),dimension(ims:ime,1:nclos,jms:jme),optional:: weights_clos

!local variables:
 character(len=50),dimension(its:ite):: ierrc,ierrcm,ierrcs

 integer:: i,ipr,k,j,n
 integer:: itf,ktf,jtf
 integer:: kb,kt
 integer,dimension(its:ite):: ierr,ierrm,ierrs
 integer,dimension(its:ite):: k22,kbcon,ktop,k22m,kbconm,ktopm,k22s,kbcons,ktops
 integer,dimension(its:ite):: kpbli,cactiv

 real:: dq
 real,dimension(its:ite):: areai,dxi
 real,dimension(its:ite):: hfxi,qfxi,psur,ter11,xlandi
 real,dimension(its:ite):: ccn,mconv
 real,dimension(its:ite):: cuten,cutenm,cutens
 real,dimension(its:ite):: pret,pretm,prets,edt,edtm,xmb,xmbm,xmbs
 real,dimension(its:ite):: dummy

 real,dimension(its:ite,10):: forcing,forcingm

 real,dimension(its:ite,kts:kte):: q2d,t2d,p2d,rhoi,us,vs,omeg,zo
 real,dimension(its:ite,kts:kte):: qcheck

 real,dimension(its:ite,kts:kte):: cnvwt,cnvwtm,zd,zdm,zu,zum,zus
 real,dimension(its:ite,kts:kte):: tshall,qshall,dhdt,tn,qo

 real,dimension(its:ite,kts:kte):: outt,outq,outqc,cupclw,outu,outv
 real,dimension(its:ite,kts:kte):: outtm,outqm,outqcm,cupclwm,outum,outvm
 real,dimension(its:ite,kts:kte):: outts,outqs,outqcs,cupclws,outus,outvs

 real,dimension(its:ite,1:nclos):: xf_ens

!local variables for stochastic parameterization:
 real,dimension(its:ite)  :: rand_mom,rand_vmas
 real,dimension(its:ite,4):: rand_clos,rstochcol

!parameters:
 integer,parameter:: imidc     = 1 !- mid-level convection
 integer,parameter:: imidd     = 0
 integer,parameter:: dicycle   = 1 !- diurnal cycle flag
 integer,parameter:: dicycle_m = 0 !- diurnal cycle flag
 integer,parameter:: nranflag  = 0

 real,parameter:: aodccn = 0.1
 real,parameter:: tcrit  = 258.
 real,parameter:: ccn0   = 150.

!-----------------------------------------------------------------------------------------------------------------

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
       enddo
    enddo

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
       cactiv(i) = 0
       ccn(i)    = ccn0
       mconv(i)  = 0.

       !deep convection:
       forcing(i,:)  = 0.
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
          outt(i,k)   = 0.
          outu(i,k)   = 0.
          outv(i,k)   = 0.
          outq(i,k)   = 0.
          outqc(i,k)  = 0.
          cupclw(i,k) = 0.
          cnvwt(i,k)  = 0.
          zd(i,k)     = 0.
          zu(i,k)     = 0.

          !mid-level convection:
          outtm(i,k)   = 0.
          outum(i,k)   = 0.
          outvm(i,k)   = 0.
          outqm(i,k)   = 0.
          outqcm(i,k)  = 0.
          cupclwm(i,k) = 0.
          cnvwtm(i,k)  = 0.
          zdm(i,k)     = 0.
          zum(i,k)     = 0.

          !shallow convection:
          outts(i,k)   = 0.
          outus(i,k)   = 0.
          outvs(i,k)   = 0.
          outqs(i,k)   = 0.
          outqcs(i,k)  = 0.
          cupclws(i,k) = 0.
          zus(i,k)     = 0.
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
       call mpas_log_write("--- enter subroutine cup_gf_sh:")
       call cup_gf_sh( &
                !input variables:
                zo,t2d,q2d,ter11,tshall,qshall,p2d,psur,dhdt,   &
                kpbli,rhoi,hfxi,qfxi,xlandi,ichoice_s,tcrit,dt, &
                zus,xmbs,kbcons,ktops,k22s,ierrs,ierrcs,        &
                outts,outqs,outqcs,cnvwt,prets,cupclws,         &
                itf,ktf,its,ite,kts,kte,ipr)

       do k = kts, ktf
          do i = its, itf
             qcheck(i,k) = q2d(i,k)
          enddo
       enddo
       call neg_check('shallow',ipr,dt,qcheck,outqs,outts,outus,outvs, &
                      outqcs,prets,its,ite,kts,kte,itf,ktf)

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
       call mpas_log_write("--- end subroutine cup_gf_sh:")
    endif


    !--- call to mid-level convection:
    if(do_midlc) then
       call mpas_log_write("--- enter subroutine cup_gf: middle")
       call cup_gf( &
                itf,ktf,its,ite,kts,kte,                                   &
                dicycle_m,ichoice_m,ipr,ccn,dt,imidc,kpbli,dhdt,           &
                xlandi,zo,forcingm,t2d,q2d,ter11,tshall,qshall,p2d,        &
                psur,us,vs,rhoi,hfxi,qfxi,areai,dxi,mconv,omeg,cactiv,     &
                cnvwtm,zum,zdm,edtm,xmbm,dummy,xmbs,pretm,outum,outvm,     &
                outtm,outqm,outqcm,kbconm,ktopm,k22m,cupclwm,ierrm,ierrcm, &
                rand_mom,rand_vmas,rand_clos,nranflag,xf_ens               &
                  )

       do k = kts, ktf
          do i = its, itf
             qcheck(i,k) = q2d(i,k) + outqs(i,k)*dt
          enddo
       enddo
       call neg_check('mid',ipr,dt,qcheck,outqm,outtm,outum,outvm, &
                      outqcm,pretm,its,ite,kts,kte,itf,ktf)

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
       call mpas_log_write("--- end subroutine cup_gf: middle")
    endif


    !--- call to deep convection:
    if(do_deepc) then
       call mpas_log_write("--- enter subroutine cup_gf: deep")
       call cup_gf( &
                itf,ktf,its,ite,kts,kte,                               &
                dicycle,ichoice,ipr,ccn,dt,imidd,kpbli,dhdt,           &
                xlandi,zo,forcing,t2d,q2d,ter11,tn,qo,p2d,             &
                psur,us,vs,rhoi,hfxi,qfxi,areai,dxi,mconv,omeg,cactiv, &
                cnvwt,zu,zd,edt,xmb,xmbm,xmbs,pret,outu,outv,          &
                outt,outq,outqc,kbcon,ktop,k22,cupclw,ierr,ierrc,      &
                rand_mom,rand_vmas,rand_clos,nranflag,xf_ens           &
                  )

       do k = kts, ktf
          do i = its, itf
             qcheck(i,k) = q2d(i,k) + (outqs(i,k) + outqm(i,k))*dt
          enddo
       enddo
       call neg_check('deep',ipr,dt,qcheck,outq,outt,outu,outv, &
                      outqc,pret,its,ite,kts,kte,itf,ktf)

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
       call mpas_log_write("--- end subroutine cup_gf: deep")
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

          if(present(rqicuten)) then
             if(t2d(i,k) .lt. tcrit) then
                rqicuten(i,k,j) = rqccuten(i,k,j)
                rqccuten(i,k,j) = 0.
                if(present(gdc2)) then
                   gdc2(i,k,j) = gdc(i,k,j)
                   gdc(i,k,j)  = 0.
                endif
             endif
          endif

          if(present(rucuten)) rucuten(i,k,j)  = cutenm(i)*outum(i,k) + cuten(i)*outu(i,k)
          if(present(rvcuten)) rvcuten(i,k,j)  = cutenm(i)*outvm(i,k) + cuten(i)*outv(i,k)
       enddo
    enddo

 enddo j_loop

 end subroutine cu_grell_freitas_wrf390

!=================================================================================================================
 end module module_cu_gf_wrf390
!=================================================================================================================
