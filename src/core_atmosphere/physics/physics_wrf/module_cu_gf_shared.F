!=================================================================================================================
!module_cu_gf_shared contains all the subroutines shared by module_cu_gf_deep.F and module_cu_gf_shared.F. All the
!subroutines are from module_cu_gf_deep.F and module_cu_gf_sh.F provided by Georg Grell and Haiqin Li on December
!2018. module_cu_gf_deep.F and module_cu_gf_sh.F are currently tested in FV3.
!Laura D. Fowler (laura@ucar.edu) / 2019-07.23.

!=================================================================================================================
 module module_cu_gf_shared
 use mpas_log
 use mpas_kind_types,only: kind_phys => RKIND
 use mpas_atmphys_constants,only: cp,R_v,xlv

 implicit none
 private
 public:: cup_env,cup_env_clev,cup_kbcon,cup_minimi,cup_up_aa0,   &
          get_cloud_bc,get_inversion_layers,get_lateral_massflux, &
          get_zu_zd_pdf_fim,rates_up_pdf,neg_check


 contains


!=================================================================================================================
   subroutine cup_env(z,qes,he,hes,t,q,p,z1,                &
              psur,ierr,tcrit,itest,                        &
              itf,ktf,                                      &
              its,ite, kts,kte                     )

   implicit none

     integer                                                &
        ,intent (in   )                   ::                &
        itf,ktf,                                            &
        its,ite, kts,kte
  !
  ! ierr error value, maybe modified in this routine
  ! q           = environmental mixing ratio
  ! qes         = environmental saturation mixing ratio
  ! t           = environmental temp
  ! tv          = environmental virtual temp
  ! p           = environmental pressure
  ! z           = environmental heights
  ! he          = environmental moist static energy
  ! hes         = environmental saturation moist static energy
  ! psur        = surface pressure
  ! z1          = terrain elevation
  ! 
  !
     real(kind=kind_phys),    dimension (its:ite,kts:kte)                &
        ,intent (in   )                   ::             &
        p,t,q
     real(kind=kind_phys),    dimension (its:ite,kts:kte)                &
        ,intent (out  )                   ::             &
        he,hes,qes
     real(kind=kind_phys),    dimension (its:ite,kts:kte)                &
        ,intent (inout)                   ::             &
        z
     real(kind=kind_phys),    dimension (its:ite)                        &
        ,intent (in   )                   ::             &
        psur,z1
     integer, dimension (its:ite)                        &
        ,intent (inout)                   ::             &
        ierr
     integer                                             &
        ,intent (in   )                   ::             &
        itest
!
!  local variables in this routine
!

     integer                              ::             &
       i,k
!     real(kind=kind_phys), dimension (1:2) :: ae,be,ht
      real(kind=kind_phys), dimension (its:ite,kts:kte) :: tv
      real(kind=kind_phys) :: tcrit,e,tvbar
!     real(kind=kind_phys), external :: satvap
!     real(kind=kind_phys) :: satvap

!     call mpas_log_write('--- enter subroutine cup_env updated:')

!      ht(1)=xlv/cp
!      ht(2)=2.834e6/cp
!      be(1)=.622*ht(1)/.286
!      ae(1)=be(1)/273.+alog(610.71)
!      be(2)=.622*ht(2)/.286
!      ae(2)=be(2)/273.+alog(610.71)
      do k=kts,ktf
      do i=its,itf
        if(ierr(i).eq.0)then
!csgb - iph is for phase, dependent on tcrit (water or ice)
!       iph=1
!       if(t(i,k).le.tcrit)iph=2
!       print *, 'ae(iph),be(iph) = ',ae(iph),be(iph),ae(iph)-be(iph),t(i,k),i,k
!       e=exp(ae(iph)-be(iph)/t(i,k))
!       print *, 'p, e = ', p(i,k), e
!       qes(i,k)=.622*e/(100.*p(i,k)-e)
        e=satvap(t(i,k))
        qes(i,k)=0.622*e/max(1.e-8,(p(i,k)-e))
        if(qes(i,k).le.1.e-16)qes(i,k)=1.e-16
        if(qes(i,k).lt.q(i,k))qes(i,k)=q(i,k)
!       if(q(i,k).gt.qes(i,k))q(i,k)=qes(i,k)
        tv(i,k)=t(i,k)+.608*q(i,k)*t(i,k)
        endif
      enddo
      enddo
!
!--- z's are calculated with changed h's and q's and t's
!--- if itest=2
!
      if(itest.eq.1 .or. itest.eq.0)then
         do i=its,itf
           if(ierr(i).eq.0)then
             z(i,1)=max(0.,z1(i))-(log(p(i,1))- &
                 log(psur(i)))*287.*tv(i,1)/9.81
           endif
         enddo

! --- calculate heights
         do k=kts+1,ktf
         do i=its,itf
           if(ierr(i).eq.0)then
              tvbar=.5*tv(i,k)+.5*tv(i,k-1)
              z(i,k)=z(i,k-1)-(log(p(i,k))- &
               log(p(i,k-1)))*287.*tvbar/9.81
           endif
         enddo
         enddo
      else if(itest.eq.2)then
         do k=kts,ktf
         do i=its,itf
           if(ierr(i).eq.0)then
             z(i,k)=(he(i,k)-1004.*t(i,k)-2.5e6*q(i,k))/9.81
             z(i,k)=max(1.e-3,z(i,k))
           endif
         enddo
         enddo
      else if(itest.eq.-1)then
      endif
!
!--- calculate moist static energy - he
!    saturated moist static energy - hes
!
       do k=kts,ktf
       do i=its,itf
         if(ierr(i).eq.0)then
         if(itest.le.0)he(i,k)=9.81*z(i,k)+1004.*t(i,k)+2.5e06*q(i,k)
         hes(i,k)=9.81*z(i,k)+1004.*t(i,k)+2.5e06*qes(i,k)
         if(he(i,k).ge.hes(i,k))he(i,k)=hes(i,k)
         endif
      enddo
      enddo

   end subroutine cup_env

!=================================================================================================================
   subroutine cup_env_clev(t,qes,q,he,hes,z,p,qes_cup,q_cup,        &
              he_cup,hes_cup,z_cup,p_cup,gamma_cup,t_cup,psur,      &
              ierr,z1,                                              &
              itf,ktf,                                              &
              its,ite, kts,kte                       )

   implicit none

     integer                                                        &
        ,intent (in   )                   ::                        &
        itf,ktf,                                                    &
        its,ite, kts,kte
  !
  ! ierr error value, maybe modified in this routine
  ! q           = environmental mixing ratio
  ! q_cup       = environmental mixing ratio on cloud levels
  ! qes         = environmental saturation mixing ratio
  ! qes_cup     = environmental saturation mixing ratio on cloud levels
  ! t           = environmental temp
  ! t_cup       = environmental temp on cloud levels
  ! p           = environmental pressure
  ! p_cup       = environmental pressure on cloud levels
  ! z           = environmental heights
  ! z_cup       = environmental heights on cloud levels
  ! he          = environmental moist static energy
  ! he_cup      = environmental moist static energy on cloud levels
  ! hes         = environmental saturation moist static energy
  ! hes_cup     = environmental saturation moist static energy on cloud levels
  ! gamma_cup   = gamma on cloud levels
  ! psur        = surface pressure
  ! z1          = terrain elevation
  ! 
  !
     real(kind=kind_phys),    dimension (its:ite,kts:kte)                        &
        ,intent (in   )                   ::                     &
        qes,q,he,hes,z,p,t
     real(kind=kind_phys),    dimension (its:ite,kts:kte)                        &
        ,intent (out  )                   ::                     &
        qes_cup,q_cup,he_cup,hes_cup,z_cup,p_cup,gamma_cup,t_cup
     real(kind=kind_phys),    dimension (its:ite)                                &
        ,intent (in   )                   ::                     &
        psur,z1
     integer, dimension (its:ite)                                &
        ,intent (inout)                   ::                     &
        ierr
!
!  local variables in this routine
!

     integer                              ::                     &
       i,k

!     call mpas_log_write('--- enter subroutine cup_env_clev updated:')

      do k=kts,ktf
      do i=its,itf
        qes_cup(i,k)=0.
        q_cup(i,k)=0.
        hes_cup(i,k)=0.
        he_cup(i,k)=0.
        z_cup(i,k)=0.
        p_cup(i,k)=0.
        t_cup(i,k)=0.
        gamma_cup(i,k)=0.
      enddo
      enddo
      do k=kts+1,ktf
      do i=its,itf
        if(ierr(i).eq.0)then
        qes_cup(i,k)=.5*(qes(i,k-1)+qes(i,k))
        q_cup(i,k)=.5*(q(i,k-1)+q(i,k))
        hes_cup(i,k)=.5*(hes(i,k-1)+hes(i,k))
        he_cup(i,k)=.5*(he(i,k-1)+he(i,k))
        if(he_cup(i,k).gt.hes_cup(i,k))he_cup(i,k)=hes_cup(i,k)
        z_cup(i,k)=.5*(z(i,k-1)+z(i,k))
        p_cup(i,k)=.5*(p(i,k-1)+p(i,k))
        t_cup(i,k)=.5*(t(i,k-1)+t(i,k))
        gamma_cup(i,k)=(xlv/cp)*(xlv/(r_v*t_cup(i,k) &
                       *t_cup(i,k)))*qes_cup(i,k)
        endif
      enddo
      enddo
      do i=its,itf
        if(ierr(i).eq.0)then
        qes_cup(i,1)=qes(i,1)
        q_cup(i,1)=q(i,1)
!       hes_cup(i,1)=hes(i,1)
!       he_cup(i,1)=he(i,1)
        hes_cup(i,1)=9.81*z1(i)+1004.*t(i,1)+2.5e6*qes(i,1)
        he_cup(i,1)=9.81*z1(i)+1004.*t(i,1)+2.5e6*q(i,1)
        z_cup(i,1)=.5*(z(i,1)+z1(i))
        p_cup(i,1)=.5*(p(i,1)+psur(i))
        z_cup(i,1)=z1(i)
        p_cup(i,1)=psur(i)
        t_cup(i,1)=t(i,1)
        gamma_cup(i,1)=xlv/cp*(xlv/(r_v*t_cup(i,1) &
                       *t_cup(i,1)))*qes_cup(i,1)
        endif
      enddo

   end subroutine cup_env_clev

!=================================================================================================================
   subroutine cup_kbcon(ierrc,cap_inc,iloop_in,k22,kbcon,he_cup,hes_cup, &
              hkb,ierr,kbmax,p_cup,cap_max,                              &
              ztexec,zqexec,                                             &
              jprnt,itf,ktf,                                             &
              its,ite, kts,kte,                                          &
              z_cup,entr_rate,heo,imid                        )

   implicit none
!

   ! only local wrf dimensions are need as of now in this routine

     integer                                                           &
        ,intent (in   )                   ::                           &
        jprnt,itf,ktf,imid,                                            &
        its,ite, kts,kte
  ! 
  ! 
  ! 
  ! ierr error value, maybe modified in this routine
  !
     real(kind=kind_phys),    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        he_cup,hes_cup,p_cup
     real(kind=kind_phys),    dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        entr_rate,ztexec,zqexec,cap_inc,cap_max
     real(kind=kind_phys),    dimension (its:ite)                                      &
        ,intent (inout   )                   ::                        &
        hkb !,cap_max
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        kbmax
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        kbcon,k22,ierr
     integer                                                           &
        ,intent (in   )                   ::                           &
        iloop_in
     character*50 :: ierrc(its:ite)
     real(kind=kind_phys), dimension (its:ite,kts:kte),intent (in) :: z_cup,heo
     integer, dimension (its:ite)      ::     iloop,start_level
!
!  local variables in this routine
!

     integer                              ::                           &
        i,k
     real(kind=kind_phys)                                ::                           &
        x_add,pbcdif,plus,hetest,dz
     real(kind=kind_phys), dimension (its:ite,kts:kte) ::hcot
!--- MPAS (modified call to the subroutine get_cloud_bc):
     real(kind=kind_phys),dimension(kts:kte):: dummy1
!--- MPAS end.
!
!--- determine the level of convective cloud base  - kbcon
!
!     call mpas_log_write('--- enter subroutine cup_kbcon updated:')

      iloop(:)=iloop_in
       do 27 i=its,itf
      kbcon(i)=1
!
! reset iloop for mid level convection
      if(cap_max(i).gt.200 .and. imid.eq.1)iloop(i)=5
!
      if(ierr(i).ne.0)go to 27
      start_level(i)=k22(i)
      kbcon(i)=k22(i)+1
      if(iloop(i).eq.5)kbcon(i)=k22(i)
!      if(iloop_in.eq.5)start_level(i)=kbcon(i)
       !== including entrainment for hetest
        hcot(i,1:start_level(i)) = hkb(i)
        do k=start_level(i)+1,kbmax(i)+3
           dz=z_cup(i,k)-z_cup(i,k-1)
           hcot(i,k)= ( (1.-0.5*entr_rate(i)*dz)*hcot(i,k-1)   &
                         + entr_rate(i)*dz*heo(i,k-1)       )/ &
                      (1.+0.5*entr_rate(i)*dz)
        enddo
       !==

      go to 32
 31   continue
      kbcon(i)=kbcon(i)+1
      if(kbcon(i).gt.kbmax(i)+2)then
         if(iloop(i).ne.4)then
                ierr(i)=3
                ierrc(i)="could not find reasonable kbcon in cup_kbcon"
         endif
        go to 27
      endif
 32   continue
      hetest=hcot(i,kbcon(i)) !hkb(i) ! he_cup(i,k22(i))
      if(hetest.lt.hes_cup(i,kbcon(i)))then
        go to 31
      endif

!     cloud base pressure and max moist static energy pressure
!     i.e., the depth (in mb) of the layer of negative buoyancy
      if(kbcon(i)-k22(i).eq.1)go to 27
      if(iloop(i).eq.5 .and. (kbcon(i)-k22(i)).le.2)go to 27
      pbcdif=-p_cup(i,kbcon(i))+p_cup(i,k22(i))
      plus=max(25.,cap_max(i)-float(iloop(i)-1)*cap_inc(i))
      if(iloop(i).eq.4)plus=cap_max(i)
!
! for shallow convection, if cap_max is greater than 25, it is the pressure at pbltop
      if(iloop(i).eq.5)plus=150.
        if(iloop(i).eq.5.and.cap_max(i).gt.200)pbcdif=-p_cup(i,kbcon(i))+cap_max(i)
      if(pbcdif.le.plus)then
        go to 27
      elseif(pbcdif.gt.plus)then
        k22(i)=k22(i)+1
        kbcon(i)=k22(i)+1
!==     since k22 has be changed, hkb has to be re-calculated
        x_add = xlv*zqexec(i)+cp*ztexec(i)
!       call get_cloud_bc(kte,he_cup (i,1:kte),hkb (i),k22(i),x_add)
        do k = kts,kte
           dummy1(k) = he_cup(i,k)
        enddo
        call get_cloud_bc(kte,dummy1,hkb (i),k22(i),x_add)

        start_level(i)=k22(i)
!        if(iloop_in.eq.5)start_level(i)=kbcon(i)
        hcot(i,1:start_level(i)) = hkb(i)
        do k=start_level(i)+1,kbmax(i)+3
           dz=z_cup(i,k)-z_cup(i,k-1)

           hcot(i,k)= ( (1.-0.5*entr_rate(i)*dz)*hcot(i,k-1)   &
                         + entr_rate(i)*dz*heo(i,k-1)       )/ &
                      (1.+0.5*entr_rate(i)*dz)
        enddo
       !==

        if(iloop(i).eq.5)kbcon(i)=k22(i)
        if(kbcon(i).gt.kbmax(i)+2)then
            if(iloop(i).ne.4)then
                ierr(i)=3
                ierrc(i)="could not find reasonable kbcon in cup_kbcon"
            endif
            go to 27
        endif
        go to 32
      endif
 27   continue

   end subroutine cup_kbcon

!=================================================================================================================
   subroutine cup_minimi(array,ks,kend,kt,ierr,              &
              itf,ktf,                                       &
              its,ite, kts,kte                     )

   implicit none
!
!  on input
!

   ! only local wrf dimensions are need as of now in this routine

     integer                                                 &
        ,intent (in   )                   ::                 &
         itf,ktf,                                            &
         its,ite, kts,kte
  ! array input array
  ! x output array with return values
  ! kt output array of levels
  ! ks,kend  check-range
     real(kind=kind_phys),    dimension (its:ite,kts:kte)                    &
        ,intent (in   )                   ::                 &
         array
     integer, dimension (its:ite)                            &
        ,intent (in   )                   ::                 &
         ierr,ks,kend
     integer, dimension (its:ite)                            &
        ,intent (out  )                   ::                 &
         kt
     real(kind=kind_phys),    dimension (its:ite)         ::                 &
         x
     integer                              ::                 &
         i,k,kstop

!     call mpas_log_write('--- enter subroutine cup_minimi updated:')

       do 200 i=its,itf
      kt(i)=ks(i)
      if(ierr(i).eq.0)then
      x(i)=array(i,ks(i))
       kstop=max(ks(i)+1,kend(i))
!
       do 100 k=ks(i)+1,kstop
         if(array(i,k).lt.x(i)) then
              x(i)=array(i,k)
              kt(i)=k
         endif
 100  continue
      endif
 200  continue

   end subroutine cup_minimi

!=================================================================================================================
   subroutine cup_up_aa0(aa0,z,zu,dby,gamma_cup,t_cup,       &
              kbcon,ktop,ierr,                               &
              itf,ktf,                                       &
              its,ite, kts,kte                     )

   implicit none
!
!  on input
!

   ! only local wrf dimensions are need as of now in this routine

     integer                                                 &
        ,intent (in   )                   ::                 &
        itf,ktf,                                             &
        its,ite, kts,kte
  ! aa0 cloud work function
  ! gamma_cup = gamma on model cloud levels
  ! t_cup = temperature (Kelvin) on model cloud levels
  ! dby = buoancy term
  ! zu= normalized updraft mass flux
  ! z = heights of model levels 
  ! ierr error value, maybe modified in this routine
  !
     real(kind=kind_phys),    dimension (its:ite,kts:kte)                     &
        ,intent (in   )                   ::                  &
        z,zu,gamma_cup,t_cup,dby
     integer, dimension (its:ite)                             &
        ,intent (in   )                   ::                  &
        kbcon,ktop
!
! input and output
!


     integer, dimension (its:ite)                             &
        ,intent (inout)                   ::                  &
        ierr
     real(kind=kind_phys),    dimension (its:ite)                             &
        ,intent (out  )                   ::                  &
        aa0
!
!  local variables in this routine
!

     integer                              ::                  &
        i,k
     real(kind=kind_phys)                                 ::                  &
        dz,da
!

!     call mpas_log_write('--- enter subroutine cup_up_aa0 updated:')

        do i=its,itf
         aa0(i)=0.
        enddo
        do 100 k=kts+1,ktf
        do 100 i=its,itf
         if(ierr(i).ne.0)go to 100
         if(k.lt.kbcon(i))go to 100
         if(k.gt.ktop(i))go to 100
         dz=z(i,k)-z(i,k-1)
         da=zu(i,k)*dz*(9.81/(1004.*( &
                (t_cup(i,k)))))*dby(i,k-1)/ &
             (1.+gamma_cup(i,k))
!         if(k.eq.ktop(i).and.da.le.0.)go to 100
         aa0(i)=aa0(i)+max(0.,da)
         if(aa0(i).lt.0.)aa0(i)=0.
100     continue

   end subroutine cup_up_aa0

!=================================================================================================================
   subroutine neg_check(name,j,dt,q,outq,outt,outu,outv,                      &
                        outqc,pret,its,ite,kts,kte,itf,ktf,ktop)
!=================================================================================================================

   integer,      intent(in   ) ::            j,its,ite,kts,kte,itf,ktf
   integer, dimension (its:ite  ),   intent(in   ) ::  ktop

     real(kind=kind_phys), dimension (its:ite,kts:kte  )                    ,                 &
      intent(inout   ) ::                                                     &
       outq,outt,outqc,outu,outv
     real(kind=kind_phys), dimension (its:ite,kts:kte  )                    ,                 &
      intent(inout   ) ::                                                     &
       q
     real(kind=kind_phys), dimension (its:ite  )                            ,                 &
      intent(inout   ) ::                                                     &
       pret
      character *(*), intent (in)         ::                                  &
       name
     real(kind=kind_phys)                                                                     &
        ,intent (in  )                   ::                                   &
        dt
     real(kind=kind_phys) :: names,scalef,thresh,qmem,qmemf,qmem2,qtest,qmem1
     integer :: icheck

!local variables:
 integer:: i,k

!-----------------------------------------------------------------------------------------------------------------

!     call mpas_log_write('--- enter subroutine neg_check updated:')
!
! first do check on vertical heating rate
!
      thresh=300.01
!      thresh=200.01        !ss
!      thresh=250.01
      names=1.
      if(name == 'shallow' .or. name == 'mid')then
        thresh=148.01
        names=1.
      endif
      scalef=86400.
      do i=its,itf
      if(ktop(i) <= 2)cycle
      icheck=0
      qmemf=1.
      qmem=0.
      do k=kts,ktop(i)
         qmem=(outt(i,k))*86400.
         if(qmem.gt.thresh)then
           qmem2=thresh/qmem
           qmemf=min(qmemf,qmem2)
      icheck=1
!
!
!          print *,'1',' adjusted massflux by factor ',i,j,k,qmem,qmem2,qmemf,dt
         endif
         if(qmem.lt.-.5*thresh*names)then
           qmem2=-.5*names*thresh/qmem
           qmemf=min(qmemf,qmem2)
      icheck=2
!
!
         endif
      enddo
      do k=kts,ktop(i)
         outq(i,k)=outq(i,k)*qmemf
         outt(i,k)=outt(i,k)*qmemf
         outu(i,k)=outu(i,k)*qmemf
         outv(i,k)=outv(i,k)*qmemf
         outqc(i,k)=outqc(i,k)*qmemf
      enddo
      pret(i)=pret(i)*qmemf 
      enddo
!      return
!
! check whether routine produces negative q's. This can happen, since 
! tendencies are calculated based on forced q's. This should have no
! influence on conservation properties, it scales linear through all
! tendencies
!
!      return
!      write(14,*)'return'
      thresh=1.e-32
      do i=its,itf
      if(ktop(i) <= 2)cycle
      qmemf=1.
      do k=kts,ktop(i)
         qmem=outq(i,k)
         if(abs(qmem).gt.0. .and. q(i,k).gt.1.e-6)then
         qtest=q(i,k)+(outq(i,k))*dt
         if(qtest.lt.thresh)then
!
! qmem2 would be the maximum allowable tendency
!
           qmem1=abs(outq(i,k))
           qmem2=abs((thresh-q(i,k))/dt)
           qmemf=min(qmemf,qmem2/qmem1)
           qmemf=max(0.,qmemf)
         endif
         endif
      enddo
      do k=kts,ktop(i)
         outq(i,k)=outq(i,k)*qmemf
         outt(i,k)=outt(i,k)*qmemf
         outu(i,k)=outu(i,k)*qmemf
         outv(i,k)=outv(i,k)*qmemf
         outqc(i,k)=outqc(i,k)*qmemf
      enddo
      pret(i)=pret(i)*qmemf 
      enddo

   end subroutine neg_check

!--------------------------------------------------------------------
 subroutine get_cloud_bc(mzp,array,x_aver,k22,add)
    implicit none
    integer, intent(in)     :: mzp,k22
    real(kind=kind_phys)   , intent(in)     :: array(mzp)
    real(kind=kind_phys)   , optional , intent(in)  :: add
    real(kind=kind_phys)   , intent(out)    :: x_aver
    integer :: i,local_order_aver,order_aver

    !-- dimension of the average
    !-- a) to pick the value at k22 level, instead of a average between
    !--    k22-order_aver, ..., k22-1, k22 set order_aver=1)
    !-- b) to average between 1 and k22 => set order_aver = k22
    order_aver = 3 !=> average between k22, k22-1 and k22-2

    local_order_aver=min(k22,order_aver)
 
    x_aver=0.
    do i = 1,local_order_aver
      x_aver = x_aver + array(k22-i+1)
    enddo
    x_aver = x_aver/float(local_order_aver)
    if(present(add)) x_aver = x_aver + add

 end subroutine get_cloud_bc
 !================================================================================================================


 subroutine rates_up_pdf(rand_vmas,ipr,name,ktop,ierr,p_cup,entr_rate_2d,hkbo,heo,heso_cup,z_cup, &
               xland,kstabi,k22,kbcon,its,ite,itf,kts,kte,ktf,zuo,kpbl,ktopdby,csum,pmin_lev)
     implicit none
     character *(*), intent (in)       :: name
     integer, intent(in) :: ipr,its,ite,itf,kts,kte,ktf
     real(kind=kind_phys), dimension (its:ite,kts:kte),intent (inout) :: entr_rate_2d,zuo
     real(kind=kind_phys), dimension (its:ite,kts:kte),intent (in) ::p_cup, heo,heso_cup,z_cup
     real(kind=kind_phys), dimension (its:ite),intent (in) :: hkbo,rand_vmas
     integer, dimension (its:ite),intent (in) :: kstabi,k22,kpbl,csum,xland,pmin_lev
     integer, dimension (its:ite),intent (inout) :: kbcon,ierr,ktop,ktopdby
     !-local vars
     real(kind=kind_phys), dimension (its:ite,kts:kte) :: hcot
     real(kind=kind_phys) :: entr_init,beta_u,dz,dbythresh,dzh2,zustart,zubeg,massent,massdetr
     real(kind=kind_phys) :: dby(kts:kte),dbm(kts:kte),zux(kts:kte)
     real(kind=kind_phys) :: zuh2(40),zh2(40)
     integer :: kklev,i,kk,kbegin,k,kfinalzu
     integer, dimension (its:ite) :: start_level 
!--- MPAS (modified to calls to the subroutine get_zu_zd_pdf_fim):
     real(kind=kind_phys),dimension(kts:kte):: dummy1,dummy2
!--- MPAS end.
     !
!    call mpas_log_write('--- enter subroutine rates_up_pdf updated:')

     zustart=.1
     dbythresh= 1. !.0.95 ! 0.85, 0.6
     if(name == 'shallow' .or. name == 'mid') dbythresh=1.
     dby(:)=0.

     do i=its,itf
      if(ierr(i) > 0 )cycle
      zux(:)=0.
      beta_u=max(.1,.2-float(csum(i))*.01)
      zuo(i,:)=0.
      dby(:)=0.
      dbm(:)=0.
      kbcon(i)=max(kbcon(i),2)
       start_level(i)=k22(i)
       zuo(i,start_level(i))=zustart
        zux(start_level(i))=zustart
        entr_init=entr_rate_2d(i,kts)
        do k=start_level(i)+1,kbcon(i)
          dz=z_cup(i,k)-z_cup(i,k-1)
          massent=dz*entr_rate_2d(i,k-1)*zuo(i,k-1)
!          massdetr=dz*1.e-9*zuo(i,k-1)
          massdetr=dz*.1*entr_init*zuo(i,k-1)
          zuo(i,k)=zuo(i,k-1)+massent-massdetr
          zux(k)=zuo(i,k)
        enddo
       zubeg=zustart !zuo(i,kbcon(i))
       if(name .eq. 'deep')then
        ktop(i)=0
        hcot(i,start_level(i))=hkbo(i)
        dz=z_cup(i,start_level(i))-z_cup(i,start_level(i)-1)
        do k=start_level(i)+1,ktf-2
           dz=z_cup(i,k)-z_cup(i,k-1)

           hcot(i,k)=( (1.-0.5*entr_rate_2d(i,k-1)*dz)*hcot(i,k-1) &
                      + entr_rate_2d(i,k-1)*dz*heo(i,k-1))/        &
                      (1.+0.5*entr_rate_2d(i,k-1)*dz)
           if(k >= kbcon(i)) dby(k)=dby(k-1)+(hcot(i,k)-heso_cup(i,k))*dz
           if(k >= kbcon(i)) dbm(k)=hcot(i,k)-heso_cup(i,k)
        enddo
        ktopdby(i)=maxloc(dby(:),1)
        kklev=maxloc(dbm(:),1)
        do k=maxloc(dby(:),1)+1,ktf-2
          if(dby(k).lt.dbythresh*maxval(dby))then
              kfinalzu=k  - 1
              ktop(i)=kfinalzu
              go to 412
          endif
        enddo
        kfinalzu=ktf-2
        ktop(i)=kfinalzu
412     continue
!
! at least overshoot by one level
!
!        kfinalzu=min(max(kfinalzu,ktopdby(i)+1),ktopdby(i)+2)
!        ktop(i)=kfinalzu
        if(kfinalzu.le.kbcon(i)+2)then
              ierr(i)=41
              ktop(i)= 0
        else
!           call get_zu_zd_pdf_fim(ipr,xland(i),zuh2,"up",ierr(i),start_level(i),             &
!           call get_zu_zd_pdf_fim(rand_vmas(i),zubeg,ipr,xland(i),zuh2,"up",ierr(i),kbcon(i), &
!            kfinalzu,zuo(i,kts:kte),kts,kte,ktf,beta_u,kpbl(i),csum(i),pmin_lev(i))
!          call get_zu_zd_pdf_fim(kklev,p_cup(i,:),rand_vmas(i),zubeg,ipr,xland(i),zuh2,"up",ierr(i),k22(i), &
!           kfinalzu+1,zuo(i,kts:kte),kts,kte,ktf,beta_u,kbcon(i),csum(i),pmin_lev(i))
           do k = kts,kte
              dummy1(k) = p_cup(i,k)
              dummy2(k) = zuo(i,k)
           enddo
           call get_zu_zd_pdf_fim(kklev,dummy1,rand_vmas(i),zubeg,ipr,xland(i),zuh2,"up",ierr(i),k22(i), &
                                  kfinalzu+1,dummy2,kts,kte,ktf,beta_u,kbcon(i),csum(i),pmin_lev(i))
           do k = kts,kte
              zuo(i,k) = dummy2(k)
           enddo
        endif
      endif ! end deep
      if ( name == 'mid' ) then
       if(ktop(i) <= kbcon(i)+2)then
              ierr(i)=41
              ktop(i)= 0
       else
           kfinalzu=ktop(i)
           ktopdby(i)=ktop(i)+1
!         call get_zu_zd_pdf_fim(kklev,p_cup(i,:),rand_vmas(i),zubeg,ipr,xland(i),zuh2,"mid",ierr(i),k22(i), &
!           ktopdby(i)+1,zuo(i,kts:kte),kts,kte,ktf,beta_u,kbcon(i),csum(i),pmin_lev(i))
           do k = kts,kte
              dummy1(k) = p_cup(i,k)
              dummy2(k) = zuo(i,k)
           enddo
           call get_zu_zd_pdf_fim(kklev,dummy1,rand_vmas(i),zubeg,ipr,xland(i),zuh2,"mid",ierr(i),k22(i), &
                                  ktopdby(i)+1,dummy2,kts,kte,ktf,beta_u,kbcon(i),csum(i),pmin_lev(i))
           do k = kts,kte
              zuo(i,k) = dummy2(k)
           enddo
       endif
      endif ! mid
      if ( name == 'shallow' ) then
       if(ktop(i) <= kbcon(i)+2)then
           ierr(i)=41
           ktop(i)= 0
       else
           kfinalzu=ktop(i)
           ktopdby(i)=ktop(i)+1
!          call get_zu_zd_pdf_fim(kbcon(i),p_cup(i,:),rand_vmas(i),zubeg,ipr,xland(i),zuh2,"sh2",ierr(i),k22(i), &
!            ktopdby(i)+1,zuo(i,kts:kte),kts,kte,ktf,beta_u,kbcon(i),csum(i),pmin_lev(i))
           do k = kts,kte
              dummy1(k) = p_cup(i,k)
              dummy2(k) = zuo(i,k)
           enddo
           call get_zu_zd_pdf_fim(kbcon(i),dummy1,rand_vmas(i),zubeg,ipr,xland(i),zuh2,"sh2",ierr(i),k22(i), &
                                  ktopdby(i)+1,dummy2,kts,kte,ktf,beta_u,kbcon(i),csum(i),pmin_lev(i))
           do k = kts,kte
              zuo(i,k) = dummy2(k)
           enddo
         endif
         endif ! shal
     enddo

  end subroutine rates_up_pdf
!-------------------------------------------------------------------------

 subroutine get_zu_zd_pdf_fim(kklev,p,rand_vmas,zubeg,ipr,xland,zuh2,draft,ierr,kb,kt,zu,kts,kte,ktf,max_mass,kpbli,csum,pmin_lev)

 implicit none
! real(kind=kind_phys), parameter :: beta_deep=1.3,g_beta_deep=0.8974707
! real(kind=kind_phys), parameter :: beta_deep=1.2,g_beta_deep=0.8974707
 real(kind=kind_phys), parameter :: beta_sh=2.5,g_beta_sh=1.329340
! real(kind=kind_phys), parameter :: beta_mid=1.3,g_beta_mid=0.8974707
 real(kind=kind_phys), parameter :: beta_mid=2.2,g_beta_mid=0.8974707
 real(kind=kind_phys), parameter :: beta_dd=4.0,g_beta_dd=6.
 integer, intent(in) ::ipr,xland,kb,kklev,kt,kts,kte,ktf,kpbli,csum,pmin_lev
 real(kind=kind_phys), intent(in) ::max_mass,zubeg
 real(kind=kind_phys), intent(inout) :: zu(kts:kte)
 real(kind=kind_phys), intent(in) :: p(kts:kte)
 real(kind=kind_phys)  :: trash,beta_deep,zuh(kts:kte),zuh2(1:40)
 integer, intent(inout) :: ierr
 character*(*), intent(in) ::draft

 !- local var
 integer :: k1,kk,k,kb_adj,kpbli_adj,kmax
 real(kind=kind_phys)    :: maxlim,krmax,kratio,tunning,fzu,rand_vmas,lev_start
 real(kind=kind_phys)    :: a,b,x1,y1,g_a,g_b,alpha2,g_alpha2
!
! very simple lookup tables
!
        real(kind=kind_phys), dimension(30) :: alpha,g_alpha
        data   (alpha(k),k=4,27)/3.699999,                               &
                      3.024999,2.559999,2.249999,2.028571,1.862500, &
                      1.733333,1.630000,1.545454,1.475000,1.415385, &
                      1.364286,1.320000,1.281250,1.247059,1.216667, &
                      1.189474,1.165000,1.142857,1.122727,1.104348, &
                      1.087500,1.075000,1.075000/
        data (g_alpha(k),k=4,27)/4.170645,                               &
                      2.046925 , 1.387837, 1.133003, 1.012418,0.9494680, &
                      0.9153771,0.8972442,0.8885444,0.8856795,0.8865333, &
                      0.8897996,0.8946404,0.9005030,0.9070138,0.9139161, &
                      0.9210315,0.9282347,0.9354376,0.9425780,0.9496124, &
                      0.9565111,0.9619183,0.9619183/
        alpha(1:3)=alpha(4)
        g_alpha(1:3)=g_alpha(4)
        alpha(28:30)=alpha(27)
        g_alpha(28:30)=g_alpha(27)

!     call mpas_log_write('--- enter subroutine get_zu_zd_pdf updated:')

 !- kb cannot be at 1st level

 !-- fill zu with zeros
 zu(:)=0.0
 zuh(:)=0.0
   kb_adj=max(kb,2)
 if(draft == "up") then
   lev_start=min(.9,.1+csum*.013)
   kb_adj=max(kb,2)
   tunning=max(p(kklev+1),.5*(p(kpbli)+p(kt)))
!   tunning=p(kklev+1) !p(kpbli+1) !p(kklev) !p(kt)+(p(kpbli)-p(kt))*lev_start
!   tunning=.5*(p(kb_adj)+p(kt)) !p(kpbli+1) !p(kklev) !p(kt)+(p(kpbli)-p(kt))*lev_start
   trash=-p(kt)+p(kb_adj)
   beta_deep=1.3 +(1.-trash/1200.)
   tunning =min(0.95, (tunning-p(kb_adj))/(p(kt)-p(kb_adj))) !=.6
   tunning =max(0.02, tunning)
   alpha2= (tunning*(beta_deep -2.)+1.)/(1.-tunning)
        do k=27,3,-1
         if(alpha(k) >= alpha2)exit
        enddo
        k1=k+1
        if(alpha(k1) .ne. alpha(k1-1))then
!           write(0,*)'k1 = ',k1
           a=alpha(k1)-alpha(k1-1)
           b=alpha(k1-1)*(k1) -(k1-1)*alpha(k1)
           x1= (alpha2-b)/a
           y1=a*x1+b
!           write(0,*)'x1,y1,a,b ',x1,y1,a,b
           g_a=g_alpha(k1)-g_alpha(k1-1)
           g_b=g_alpha(k1-1)*k1 - (k1-1)*g_alpha(k1)
           g_alpha2=g_a*x1+g_b
!           write(0,*)'g_a,g_b,g_alpha2 ',g_a,g_b,g_alpha2
         else
           g_alpha2=g_alpha(k1)
         endif

!    fzu = gamma(alpha2 + beta_deep)/(g_alpha2*g_beta_deep)
    fzu = gamma(alpha2 + beta_deep)/(gamma(alpha2)*gamma(beta_deep))
      zu(kb_adj)=zubeg
  do k=kb_adj+1,min(kte,kt-1)
      kratio= (p(k)-p(kb_adj))/(p(kt)-p(kb_adj)) !float(k)/float(kt+1)
      kratio= min(kratio,1.0)
      zu(k) = zubeg+fzu*kratio**(alpha2-1.0) * (1.0-kratio)**(beta_deep-1.0)
   enddo

   if(zu(kpbli).gt.0.)  &
      zu(kts:min(ktf,kt-1))= zu(kts:min(ktf,kt-1))/zu(kpbli)
     do k=maxloc(zu(:),1),1,-1
       if(zu(k).lt.1.e-6)then
         kb_adj=k+1
         exit
       endif
     enddo
     kb_adj=max(2,kb_adj)
     do k=kts,kb_adj-1
       zu(k)=0.
     enddo
     maxlim=1.2
     a=maxval(zu)-zu(kb_adj)
      do k=kb_adj,kt
        trash=zu(k)
        if(a.gt.maxlim)then
          zu(k)=(zu(k)-zu(kb_adj))*maxlim/a+zu(kb_adj)
!        if(p(kt).gt.400.)write(32,122)k,p(k),zu(k),trash
        endif
      enddo
122  format(1x,i4,1x,f8.1,1x,f6.2,1x,f6.2)

 elseif(draft == "sh2") then
   k=kklev
   if(kpbli.gt.5)k=kpbli
!new nov18
   tunning=p(kklev) !p(kpbli+1) !p(kklev) !p(kt)+(p(kpbli)-p(kt))*lev_start
!end new
   tunning =min(0.95, (tunning-p(kb_adj))/(p(kt)-p(kb_adj))) !=.6
   tunning =max(0.02, tunning)
   alpha2= (tunning*(beta_sh -2.)+1.)/(1.-tunning)
        do k=27,3,-1
         if(alpha(k) >= alpha2)exit
        enddo
        k1=k+1
        if(alpha(k1) .ne. alpha(k1-1))then
           a=alpha(k1)-alpha(k1-1)
           b=alpha(k1-1)*(k1) -(k1-1)*alpha(k1)
           x1= (alpha2-b)/a
           y1=a*x1+b
           g_a=g_alpha(k1)-g_alpha(k1-1)
           g_b=g_alpha(k1-1)*k1 - (k1-1)*g_alpha(k1)
           g_alpha2=g_a*x1+g_b
         else
           g_alpha2=g_alpha(k1)
         endif

    fzu = gamma(alpha2 + beta_sh)/(g_alpha2*g_beta_sh)
      zu(kb_adj) = zubeg
  do k=kb_adj+1,min(kte,kt-1)
      kratio= (p(k)-p(kb_adj))/(p(kt)-p(kb_adj)) !float(k)/float(kt+1)
      kratio= min(kratio,1.0)
      zu(k) = zubeg+fzu*kratio**(alpha2-1.0) * (1.0-kratio)**(beta_sh-1.0)
   enddo

!   beta    = 2.5 !2.5 ! max(2.5,2./tunning)
!   if(maxval(zu(kts:min(ktf,kt+1))).gt.0.)  &
!      zu(kts:min(ktf,kt+1))= zu(kts:min(ktf,kt+1))/maxval(zu(kts:min(ktf,kt+1)))
   if(zu(kpbli).gt.0.)  &
      zu(kts:min(ktf,kt-1))= zu(kts:min(ktf,kt-1))/zu(kpbli)
     do k=maxloc(zu(:),1),1,-1
       if(zu(k).lt.1.e-6)then
         kb_adj=k+1
         exit
       endif
     enddo
     maxlim=1.
     a=maxval(zu)-zu(kb_adj)
      do k=kts,kt
        if(a.gt.maxlim)zu(k)=(zu(k)-zu(kb_adj))*maxlim/a+zu(kb_adj)
!       write(32,122)k,p(k),zu(k)
      enddo

 elseif(draft == "mid") then
   kb_adj=max(kb,2)
   tunning=.5*(p(kt)+p(kpbli)) !p(kt)+(p(kb_adj)-p(kt))*.9 !*.33
!new nov18
   tunning=p(kpbli) !p(kpbli+1) !p(kklev) !p(kt)+(p(kpbli)-p(kt))*lev_start
!end new
   tunning =min(0.95, (tunning-p(kb_adj))/(p(kt)-p(kb_adj))) !=.6
   tunning =max(0.02, tunning)
   alpha2= (tunning*(beta_mid -2.)+1.)/(1.-tunning)
        do k=27,3,-1
         if(alpha(k) >= alpha2)exit
        enddo
        k1=k+1
        if(alpha(k1) .ne. alpha(k1-1))then
           a=alpha(k1)-alpha(k1-1)
           b=alpha(k1-1)*(k1) -(k1-1)*alpha(k1)
           x1= (alpha2-b)/a
           y1=a*x1+b
           g_a=g_alpha(k1)-g_alpha(k1-1)
           g_b=g_alpha(k1-1)*k1 - (k1-1)*g_alpha(k1)
           g_alpha2=g_a*x1+g_b
         else
           g_alpha2=g_alpha(k1)
         endif

!    fzu = gamma(alpha2 + beta_deep)/(g_alpha2*g_beta_deep)
    fzu = gamma(alpha2 + beta_mid)/(gamma(alpha2)*gamma(beta_mid))
!    fzu = gamma(alpha2 + beta_mid)/(g_alpha2*g_beta_mid)
      zu(kb_adj) = zubeg
  do k=kb_adj+1,min(kte,kt-1)
      kratio= (p(k)-p(kb_adj))/(p(kt)-p(kb_adj)) !float(k)/float(kt+1)
      kratio= min(kratio,1.0)
      zu(k) = zubeg+fzu*kratio**(alpha2-1.0) * (1.0-kratio)**(beta_mid-1.0)
   enddo

   if(zu(kpbli).gt.0.)  &
      zu(kts:min(ktf,kt-1))= zu(kts:min(ktf,kt-1))/zu(kpbli)
     do k=maxloc(zu(:),1),1,-1
       if(zu(k).lt.1.e-6)then
         kb_adj=k+1
         exit
       endif
     enddo
     kb_adj=max(2,kb_adj)
     do k=kts,kb_adj-1
       zu(k)=0.
     enddo
     maxlim=1.5
     a=maxval(zu)-zu(kb_adj)
      do k=kts,kt
        if(a.gt.maxlim)zu(k)=(zu(k)-zu(kb_adj))*maxlim/a+zu(kb_adj)
!       write(33,122)k,p(k),zu(k)
      enddo

 elseif(draft == "down" .or. draft == "downm") then

   tunning=p(kb)
   tunning =min(0.95, (tunning-p(1))/(p(kt)-p(1))) !=.6
   tunning =max(0.02, tunning)
   alpha2= (tunning*(beta_dd -2.)+1.)/(1.-tunning)
        do k=27,3,-1
         if(alpha(k) >= alpha2)exit
        enddo
        k1=k+1
        if(alpha(k1) .ne. alpha(k1-1))then
           a=alpha(k1)-alpha(k1-1)
           b=alpha(k1-1)*(k1) -(k1-1)*alpha(k1)
           x1= (alpha2-b)/a
           y1=a*x1+b
           g_a=g_alpha(k1)-g_alpha(k1-1)
           g_b=g_alpha(k1-1)*k1 - (k1-1)*g_alpha(k1)
           g_alpha2=g_a*x1+g_b
         else
           g_alpha2=g_alpha(k1)
         endif

   fzu = gamma(alpha2 + beta_dd)/(g_alpha2*g_beta_dd)
!   fzu = gamma(alpha2 + beta_dd)/(gamma(alpha2)*gamma(beta_dd))
  zu(:)=0.
  do k=2,min(kte,kt-1)
      kratio= (p(k)-p(1))/(p(kt)-p(1)) !float(k)/float(kt+1)
      kratio= min(kratio,1.0)
      zu(k) = fzu*kratio**(alpha2-1.0) * (1.0-kratio)**(beta_dd-1.0)
   enddo

    fzu=maxval(zu(kts:min(ktf,kt-1)))
   if(fzu.gt.0.)  &
      zu(kts:min(ktf,kt-1))= zu(kts:min(ktf,kt-1))/fzu
     zu(1)=0.
   do k=1,kb-2 !kb,2,-1
     zu(kb-k)=zu(kb-k+1)-zu(kb)*(p(kb-k)-p(kb-k+1))/(p(1)-p(kb))
   enddo
     zu(1)=0.
  endif
  end subroutine get_zu_zd_pdf_fim

!---------------------------------------------------------------------- 
 subroutine get_inversion_layers(ierr,p_cup,t_cup,z_cup,qo_cup,qeso_cup,k_inv_layers,&           
                     kstart,kend,dtempdz,itf,ktf,its,ite, kts,kte)
                                    
        implicit none
        integer                      ,intent (in ) :: itf,ktf,its,ite,kts,kte
        integer, dimension (its:ite) ,intent (in ) :: ierr,kstart,kend
        integer, dimension (its:ite) :: kend_p3
                    
        real(kind=kind_phys),    dimension (its:ite,kts:kte), intent (in ) :: p_cup,t_cup,z_cup,qo_cup,qeso_cup                            
        real(kind=kind_phys),    dimension (its:ite,kts:kte), intent (out) :: dtempdz                    
        integer, dimension (its:ite,kts:kte), intent (out) :: k_inv_layers
        !-local vars
        real(kind=kind_phys)   :: dp,l_mid,l_shal,first_deriv(kts:kte),sec_deriv(kts:kte)
        integer:: ken,kadd,kj,i,k,ilev,kk,ix,k800,k550,mid,shal
        !
!       call mpas_log_write('--- enter subroutine get_inversion_layers updated:')

        !-initialize k_inv_layers as undef
        l_mid=300.
        l_shal=100.
        k_inv_layers(:,:) = 1
         do i = its,itf
           if(ierr(i) == 0)then
           sec_deriv(:)=0.
           kend_p3(i)=kend(i)+3
           do k = kts+1,kend_p3(i)+4
            !-  get the 1st der
            first_deriv(k)= (t_cup(i,k+1)-t_cup(i,k-1))/(z_cup(i,k+1)-z_cup(i,k-1))        
            dtempdz(i,k)=first_deriv(k)
               enddo
           do k = kts+2,kend_p3(i)+3
            !  get the 2nd der
            sec_deriv(k)= (first_deriv(k+1)-first_deriv(k-1))/(z_cup(i,k+1)-z_cup(i,k-1))        
            sec_deriv(k)= abs(sec_deriv(k))        
           enddo
        
         ilev=max(kts+3,kstart(i)+1)
         ix=1
         k=ilev
         do while (ilev < kend_p3(i)) !(z_cup(i,ilev)<15000.)
           do kk=k,kend_p3(i)+2 !k,ktf-2
             
             if(sec_deriv(kk) <        sec_deriv(kk+1) .and.  &
                sec_deriv(kk) < sec_deriv(kk-1)        ) then
              k_inv_layers(i,ix)=kk
              ix=min(5,ix+1)
              ilev=kk+1
              exit   
             endif
              ilev=kk+1
               enddo
           k=ilev
         enddo         
        !- 2nd criteria
         kadd=0
         ken=maxloc(k_inv_layers(i,:),1)
         do k=1,ken
           kk=k_inv_layers(i,k+kadd)
           if(kk.eq.1)exit

           if( dtempdz(i,kk) < dtempdz(i,kk-1) .and. &
               dtempdz(i,kk) < dtempdz(i,kk+1) ) then ! the layer is not a local maximum
               kadd=kadd+1
                do kj = k,ken
               if(k_inv_layers(i,kj+kadd).gt.1)k_inv_layers(i,kj) = k_inv_layers(i,kj+kadd)
               if(k_inv_layers(i,kj+kadd).eq.1)k_inv_layers(i,kj) = 1
                enddo
           endif
         enddo
        endif
        enddo
100 format(1x,16i3)        
        !- find the locations of inversions around 800 and 550 hPa
        do i = its,itf
         if(ierr(i) /= 0) cycle

         !- now find the closest layers of 800 and 550 hPa.
         sec_deriv(:)=1.e9
         do k=1,maxloc(k_inv_layers(i,:),1) !kts,kte !kstart(i),kend(i) !kts,kte
           dp=p_cup(i,k_inv_layers(i,k))-p_cup(i,kstart(i))
           sec_deriv(k)=abs(dp)-l_shal
         enddo
         k800=minloc(abs(sec_deriv),1)
        sec_deriv(:)=1.e9

         do k=1,maxloc(k_inv_layers(i,:),1) !kts,kte !kstart(i),kend(i) !kts,kte
           dp=p_cup(i,k_inv_layers(i,k))-p_cup(i,kstart(i))
           sec_deriv(k)=abs(dp)-l_mid
         enddo
         k550=minloc(abs(sec_deriv),1)
         !-save k800 and k550 in k_inv_layers array
         shal=1
         mid=2
         k_inv_layers(i,shal)=k_inv_layers(i,k800) ! this is for shallow convection
         k_inv_layers(i,mid )=k_inv_layers(i,k550) ! this is for mid/congestus convection
         k_inv_layers(i,mid+1:kte)=-1
        enddo

        
 end subroutine get_inversion_layers
!=============================================================================================
  subroutine get_lateral_massflux(itf,ktf, its,ite, kts,kte                             &
                                  ,ierr,ktop,zo_cup,zuo,cd,entr_rate_2d                 &
                                  ,up_massentro, up_massdetro ,up_massentr, up_massdetr &
                                  ,draft,kbcon,k22,up_massentru,up_massdetru,lambau)

     implicit none
     character *(*), intent (in) :: draft
     integer, intent(in):: itf,ktf, its,ite, kts,kte
     integer, intent(in)   , dimension(its:ite)         :: ierr,ktop,kbcon,k22
     real(kind=kind_phys),    intent(in),  optional , dimension(its:ite):: lambau
     real(kind=kind_phys),    intent(in)   , dimension(its:ite,kts:kte) :: zo_cup,zuo
     real(kind=kind_phys),    intent(inout), dimension(its:ite,kts:kte) :: cd,entr_rate_2d   
     real(kind=kind_phys),    intent(  out), dimension(its:ite,kts:kte) :: up_massentro, up_massdetro  &
                                                          ,up_massentr,  up_massdetr
     real(kind=kind_phys),    intent(  out), dimension(its:ite,kts:kte),  optional ::                  &
                                                          up_massentru,up_massdetru
     !-- local vars
     integer :: i,k, incr1,incr2,turn
     real(kind=kind_phys) :: dz,trash,trash2

!    call mpas_log_write('--- enter subroutine get_lateral_massflux updated:')
     
     do k=kts,kte
      do i=its,ite
         up_massentro(i,k)=0.
         up_massdetro(i,k)=0.
         up_massentr (i,k)=0.
         up_massdetr (i,k)=0.
      enddo
     enddo
     if(present(up_massentru) .and. present(up_massdetru))then
       do k=kts,kte
        do i=its,ite
          up_massentru(i,k)=0.
          up_massdetru(i,k)=0.
        enddo
       enddo
     endif
     do i=its,itf
       if(ierr(i).eq.0)then
         
          do k=max(2,k22(i)+1),maxloc(zuo(i,:),1)
           !=> below maximum value zu -> change entrainment
           dz=zo_cup(i,k)-zo_cup(i,k-1)
        
           up_massdetro(i,k-1)=cd(i,k-1)*dz*zuo(i,k-1)
           up_massentro(i,k-1)=zuo(i,k)-zuo(i,k-1)+up_massdetro(i,k-1)
           if(up_massentro(i,k-1).lt.0.)then
              up_massentro(i,k-1)=0.
              up_massdetro(i,k-1)=zuo(i,k-1)-zuo(i,k)
              if(zuo(i,k-1).gt.0.)cd(i,k-1)=up_massdetro(i,k-1)/(dz*zuo(i,k-1))
           endif
           if(zuo(i,k-1).gt.0.)entr_rate_2d(i,k-1)=(up_massentro(i,k-1))/(dz*zuo(i,k-1))
         enddo
         do k=maxloc(zuo(i,:),1)+1,ktop(i)
           !=> above maximum value zu -> change detrainment
           dz=zo_cup(i,k)-zo_cup(i,k-1)
           up_massentro(i,k-1)=entr_rate_2d(i,k-1)*dz*zuo(i,k-1)
           up_massdetro(i,k-1)=zuo(i,k-1)+up_massentro(i,k-1)-zuo(i,k)
           if(up_massdetro(i,k-1).lt.0.)then
              up_massdetro(i,k-1)=0.
              up_massentro(i,k-1)=zuo(i,k)-zuo(i,k-1)
              if(zuo(i,k-1).gt.0.)entr_rate_2d(i,k-1)=(up_massentro(i,k-1))/(dz*zuo(i,k-1))
           endif
        
           if(zuo(i,k-1).gt.0.)cd(i,k-1)=up_massdetro(i,k-1)/(dz*zuo(i,k-1))
         enddo
         up_massdetro(i,ktop(i))=zuo(i,ktop(i))
         up_massentro(i,ktop(i))=0.
         do k=ktop(i)+1,ktf
           cd(i,k)=0.
           entr_rate_2d(i,k)=0.
           up_massentro(i,k)=0.
           up_massdetro(i,k)=0.
         enddo
         do k=2,ktf-1
           up_massentr (i,k-1)=up_massentro(i,k-1)
           up_massdetr (i,k-1)=up_massdetro(i,k-1)
         enddo         
         if(present(up_massentru) .and. present(up_massdetru))then
          turn=maxloc(zuo(i,:),1)
          do k=2,turn
           up_massentru(i,k-1)=up_massentro(i,k-1)+0.5*lambau(i)*up_massentro(i,k-1)
           up_massdetru(i,k-1)=up_massdetro(i,k-1)+0.5*lambau(i)*up_massentro(i,k-1)
          enddo
          do k=turn+1,ktf-1
           up_massentru(i,k-1)=up_massentro(i,k-1)+lambau(i)*up_massdetro(i,k-1)
           up_massdetru(i,k-1)=up_massdetro(i,k-1)+lambau(i)*up_massdetro(i,k-1)
          enddo
         endif

         trash=0.
         trash2=0.
         do k=k22(i)+1,ktop(i)
             trash2=trash2+entr_rate_2d(i,k)
         enddo
         do k=k22(i)+1,kbcon(i)
            trash=trash+entr_rate_2d(i,k)
         enddo
  
       endif
    enddo
 end subroutine get_lateral_massflux

!=================================================================================================================
 real function satvap(temp2)
!=================================================================================================================

!input arguments:
 real(kind=kind_phys),intent(in):: temp2
 
!local variables:
 real(kind=kind_phys):: temp,toot,toto,eilog,tsot,ewlog,ewlog2,ewlog3,ewlog4

!-----------------------------------------------------------------------------------------------------------------

!call mpas_log_write('--- enter function satvap updated:')

 temp = temp2-273.155
 if(temp.lt.-20.) then   !!!! ice saturation
    toot  = 273.16 / temp2
    toto  = 1 / toot
    eilog = -9.09718 * (toot - 1) - 3.56654 * (log(toot) / &
            log(10.)) + .876793 * (1 - toto) + (log(6.1071) / log(10.))
    satvap = 10 ** eilog
 else
    tsot = 373.16 / temp2
    ewlog  = -7.90298 * (tsot - 1) + 5.02808 * (log(tsot) / log(10.))
    ewlog2 = ewlog - 1.3816e-07 * (10 ** (11.344 * (1 - (1 / tsot))) - 1)
    ewlog3 = ewlog2 + .0081328 * (10 ** (-3.49149 * (tsot - 1)) - 1)
    ewlog4 = ewlog3 + (log(1013.246) / log(10.))
    satvap = 10 ** ewlog4
 end if

 end function satvap

!=================================================================================================================
 end module module_cu_gf_shared
!=================================================================================================================
