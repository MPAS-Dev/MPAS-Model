!=================================================================================================================
 module bl_mynn
 use mpas_kind_types,only: kind_phys => RKIND
 use mpas_log

 use bl_mynn_common,only: rd,xlscp,xlvcp
 use bl_mynn_subroutines

 implicit none
 private
 public:: bl_mynn_run,  &
          bl_mynn_init, &
          bl_mynn_final


 contains


!=================================================================================================================
 subroutine bl_mynn_init(errmsg,errflg)
!=================================================================================================================

 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

!-----------------------------------------------------------------------------------------------------------------
!note: this subroutine currently does nothing.

 errmsg = ' '
 errflg = 0

 end subroutine bl_mynn_init

!=================================================================================================================
 subroutine bl_mynn_run &
            (initflag         , restart            , cycling          , &
             grav_settling    , delt               , dz               , &
             dx               , znt                , u                , &
             v                , w                  , th               , &
             sqv              , sqc                , sqi              , &
             qnc              , qni                , qnwfa            , &
             qnifa            , qozone             , p                , &
             exner            , rho                , tt               , &
             xland            , ts                 , qsfc             , &
             qcg              , ps                 , ust              , &
             ch               , hfx                , qfx              , &
             rmol             , wspd               , uoce             , &
             voce             , vdfg               , qke              , &
             qke_adv          , bl_mynn_tkeadvect  , tsq              , &
             qsq              , cov                , rublten          , &
             rvblten          , rthblten           , rqvblten         , &             
             rqcblten         , rqiblten           , rqncblten        , &
             rqniblten        , rqnwfablten        , rqnifablten      , &
             rqozblten        , exch_h             , exch_m           , &
             pblh             , kpbl               , el_pbl           , &
             dqke             , qwt                , qshear           , &
             qbuoy            , qdiss              , wstar            , &
             delta            , bl_mynn_tkebudget  , bl_mynn_cloudpdf , &
             sh3d             , bl_mynn_mixlength  , icloud_bl        , &
             qc_bl            , qi_bl              , cldfra_bl        , &
             levflag          , bl_mynn_edmf       , bl_mynn_edmf_mom , &
             bl_mynn_edmf_tke , bl_mynn_mixscalars , bl_mynn_output   , &
             bl_mynn_cloudmix , bl_mynn_mixqt      , edmf_a           , &
             edmf_w           , edmf_qt            , edmf_thl         , & 
             edmf_ent         , edmf_qc            , sub_thl          , & 
             sub_sqv          , det_thl            , det_sqv          , &
             nupdraft         , maxmf              , ktop_plume       , &
             spp_pbl          , pattern_spp_pbl    , rthraten         , &
             flag_qc          , flag_qi            , flag_qnc         , &
             flag_qni         , flag_qnwfa         , flag_qnifa       , &
             !--- constants:
             cp , karman , grav , p1000mb , ep_1 , ep_2 , rcp , xlv ,   &
             its, ite , kts , kte , kme , errmsg , errflg               &
            )    

!=================================================================================================================

!input arguments:
 logical,intent(in):: &
    flag_qc,flag_qi,flag_qnc,flag_qni,flag_qnifa,flag_qnwfa

 logical,intent(in):: bl_mynn_tkeadvect

 logical,intent(in):: &
    restart,cycling

 integer,intent(in):: its,ite,kts,kte,kme

 integer,intent(in):: &
    initflag,grav_settling,icloud_bl,levflag,spp_pbl

 integer,intent(in):: &
    bl_mynn_cloudpdf,bl_mynn_mixlength,bl_mynn_edmf,bl_mynn_edmf_mom, &
    bl_mynn_edmf_tke,bl_mynn_output,bl_mynn_mixscalars,bl_mynn_cloudmix, &
    bl_mynn_mixqt,bl_mynn_tkebudget
 
 real(kind=kind_phys),intent(in):: &
    cp,karman,grav,p1000mb,ep_1,ep_2,rcp,xlv

 real(kind=kind_phys),intent(in):: &
    delt

 real(kind=kind_phys),intent(in),dimension(its:ite):: &
    dx,          &!
    xland,       &!
    ps,          &!
    ts,          &!
    qcg,         &!
    qsfc,        &!
    ust,         &!
    ch,          &!
    hfx,         &!
    qfx,         &!
    rmol,        &!
    wspd,        &!
    vdfg,        &!
    uoce,        &!
    voce,        &!
    znt           !

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte):: &
    dz,          &!
    u,           &!
    v,           &!
    th,          &!
    tt,          &!
    p,           &!
    exner,       &!
    rho,         &!
    rthraten      !

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte):: &
    sqv,         &!
    sqc,         &!
    sqi,         &!
    qnc,         &!
    qni,         &!
    qnifa,       &!
    qnwfa,       &!
    qozone        !

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte):: &
    pattern_spp_pbl

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kme):: &
    w             !

!inout arguments:
 integer,intent(inout),dimension(its:ite):: &
    kpbl,        &!
    nupdraft,    &!
    ktop_plume    !

 real(kind=kind_phys),intent(inout),dimension(its:ite):: &
    pblh,        &!
    delta,       &!
    wstar         !

 real(kind=kind_phys),intent(inout),dimension(its:ite,kts:kte):: &
    cldfra_bl,   &!
    qc_bl,       &!
    qi_bl         !

 real(kind=kind_phys),intent(inout),dimension(its:ite,kts:kte):: &
    el_pbl,      &!
    qke,         &!
    qke_adv,     &!
    cov,         &!
    qsq,         &!
    tsq,         &!
    sh3d          !

 real(kind=kind_phys),intent(inout),dimension(its:ite,kts:kte):: &
    rublten,     &!
    rvblten,     &!
    rthblten,    &!
    rqvblten,    &!
    rqcblten,    &!
    rqiblten,    &!
    rqncblten,   &!
    rqniblten,   &!
    rqnifablten, &!
    rqnwfablten, &!
    rqozblten     !

 real(kind=kind_phys),intent(inout),dimension(its:ite,kts:kte):: &
    edmf_a,      &!
    edmf_w,      &!
    edmf_qt,     &!
    edmf_thl,    &!
    edmf_ent,    &!
    edmf_qc,     &!
    sub_thl,     &!
    sub_sqv,     &!
    det_thl,     &!
    det_sqv       !

!output arguments:
 character(len=*),intent(out):: &
    errmsg        ! output error message (-).

 integer,intent(out):: &
    errflg        ! output error flag (-).

 real(kind=kind_phys),intent(out),dimension(its:ite):: &
    maxmf

 real(kind=kind_phys),intent(out),dimension(its:ite,kts:kte):: &
    exch_h,      &!
    exch_m        !

 real(kind=kind_phys),intent(out),dimension(its:ite,kts:kte):: &
    dqke,        &!
    qwt,         &!
    qshear,      &!
    qbuoy,       &!
    qdiss         !


!local variable and arrays:
 logical:: initialize_qke

 integer:: i,k

 real(kind=kind_phys):: sqc9,sqi9
 real(kind=kind_phys):: qc_bl2,qi_bl2
 real(kind=kind_phys):: cpm,exnerg,flq,flqc,flqv,flt,phh,pmz,psig_bl,psig_shcu,sqcg, &
                        th_sfc,zet,ts_decay,ztop_plume

 real(kind=kind_phys),dimension(kts:kte):: cldfra_bl1_old,qc_bl1_old,qi_bl1_old
 real(kind=kind_phys),dimension(kts:kte):: qv1,qc1,qi1
 real(kind=kind_phys),dimension(kts:kte):: det_sqc,det_u,det_v,sub_u,sub_v

 real(kind=kind_phys),dimension(kts:kte):: pdc,pdk,pdq,pdt,sgm,sqw,thetav,thl,thlsg,thvl,vq,vt

 real(kind=kind_phys),dimension(kts:kte):: dfh,dfm,dfq,qcd,tcd,diss_heat

 real(kind=kind_phys),dimension(kts:kte):: rstoch_col

 real(kind=kind_phys),dimension(kts:kte+1):: zw

 real(kind=kind_phys),dimension(kts:kte+1):: &
    s_aw1,s_awthl1,s_awqt1,s_awqv1,s_awqc1,s_awu1,s_awv1,s_awqke1,s_awqnc1,s_awqni1,s_awqnwfa1,s_awqnifa1

!JOE-add GRIMS parameters & variables
 real(kind=kind_phys),parameter::  d1 = 0.02, d2 = 0.05, d3 = 0.001
 real(kind=kind_phys),parameter::  h1 = 0.33333335, h2 = 0.6666667
 real(kind=kind_phys):: govrth,sflux,bfx0,wstar3,wm2,wm3,delb
!JOE-end GRIMS

!JOE-top-down diffusion
 logical :: cloudflg
 integer :: kk,kminrad

 real(kind=kind_phys),parameter:: pfac =2.0, zfmin = 0.01, phifac=8.0
 real(kind=kind_phys):: maxkhtopdown
 real(kind=kind_phys):: bfxpbl,dthvx,tmp1,temps,templ,zl1,wstar3_2
 real(kind=kind_phys):: ent_eff,radsum,radflux,we,rcldb,rvls,minrad,zminrad
 real(kind=kind_phys),dimension(kts:kte):: khtopdown,zfac,wscalek2,zfacent,tkeprodtd
!JOE-end top down


!local 1D input arguments:
 real(kind=kind_phys):: dx1,xland1,ps1,ts1,qcg1,qsfc1,ust1,ch1,hfx1,qfx1,rmol1,wspd1, &
    vdfg1,uoce1,voce1,znt1
 real(kind=kind_phys),dimension(kts:kte):: &
    dz1,u1,v1,th1,tk1,p1,ex1,rho1,qnc1,qni1,qnifa1,qnwfa1,qozone1,rthraten1,sqv1,sqc1,sqi1
 real(kind=kind_phys),dimension(kts:kme):: w1

!local 1D inout arguments:
 integer:: kpbl1,nupdraft1,ktop_plume1

 real(kind=kind_phys):: pblh1,delta1,wstar1
 real(kind=kind_phys),dimension(kts:kte):: cldfra_bl1,qc_bl1,qi_bl1
 real(kind=kind_phys),dimension(kts:kte):: el_pbl1,qke1,qke_adv1,cov1,qsq1,tsq1,sh1
 real(kind=kind_phys),dimension(kts:kte):: du1,dv1,dth1,dqv1,dqc1,dqi1,dqnc1,dqni1,dqnifa1,dqnwfa1,dqozone1
 real(kind=kind_phys),dimension(kts:kte):: edmf_a1,edmf_w1,edmf_qt1,edmf_thl1,edmf_ent1,edmf_qc1,sub_thl1, &
                                           sub_sqv1,det_thl1,det_sqv1

!local 1D output arguments:
 real(kind=kind_phys):: maxmf1
 real(kind=kind_phys),dimension(kts:kte):: exch_h1,exch_m1

 real(kind=kind_phys),dimension(kts:kte):: dqke1,qwt1,qshear1,qbuoy1,qdiss1

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write('--- enter subroutine bl_mynn_run:')

 errmsg = " "
 errflg = 0

 do i = its,ite

!--- initialization of 2D inout tendencies:
    do k = kts,kte
       rublten(i,k)     = 0._kind_phys
       rvblten(i,k)     = 0._kind_phys
       rthblten(i,k)    = 0._kind_phys
       rqvblten(i,k)    = 0._kind_phys
       rqcblten(i,k)    = 0._kind_phys       
       rqiblten(i,k)    = 0._kind_phys       
       rqncblten(i,k)   = 0._kind_phys       
       rqniblten(i,k)   = 0._kind_phys       
       rqnifablten(i,k) = 0._kind_phys       
       rqnwfablten(i,k) = 0._kind_phys       
    enddo

!--- initialization of 2D output variables:
    maxmf(i) = 0._kind_phys

    do k = kts,kte
       qwt(i,k)    = 0._kind_phys
       qshear(i,k) = 0._kind_phys
       qbuoy(i,k)  = 0._kind_phys
       qdiss(i,k)  = 0._kind_phys
       dqke(i,k)   = 0._kind_phys
       exch_h(i,k) = 0._kind_phys
       exch_m(i,k) = 0._kind_phys
    enddo


!--- initialization of 1D input variables using 2D input variables:
    dx1    = dx(i)
    xland1 = xland(i)
    ps1    = ps(i)
    ts1    = ts(i)
    qcg1   = qcg(i)
    qsfc1  = qsfc(i)
    ust1   = ust(i)
    ch1    = ch(i)
    hfx1   = hfx(i)
    qfx1   = qfx(i)
    rmol1  = rmol(i)
    wspd1  = wspd(i)
    vdfg1  = vdfg(i)
    uoce1  = uoce(i)
    voce1  = voce(i)
    znt1   = znt(i)

    do k = kts,kte
       dz1(k)        = dz(i,k)
       u1(k)         = u(i,k)
       v1(k)         = v(i,k)
       w1(k)         = w(i,k)
       th1(k)        = th(i,k)
       tk1(k)        = tt(i,k)
       p1(k)         = p(i,k)
       ex1(k)        = exner(i,k)
       rho1(k)       = rho(i,k)
       sh1(k)        = sh3d(i,k)
       rthraten1(k)  = rthraten(i,k)
       sqv1(k)       = sqv(i,k)
       sqc1(k)       = sqc(i,k)
       sqi1(k)       = sqi(i,k)
       qnc1(k)       = qnc(i,k)
       qni1(k)       = qni(i,k)
       qnifa1(k)     = qnifa(i,k)
       qnwfa1(k)     = qnwfa(i,k)
       qozone1(k)    = qozone(i,k)
    enddo
    do k = kte,kte+1
       w1(k) = w(i,k)
    enddo

!--- initialization of the PBL stochastic forcing:
    if(spp_pbl .eq. 1) then
       do k = kts,kte
          rstoch_col(k) = pattern_spp_pbl(i,k)
       enddo
    else
       do k = kts,kte
          rstoch_col(k) = 0._kind_phys
       enddo
    endif


!--- initialization of 1D inout variables using 2D inout variables:
    kpbl1        = kpbl(i)
    nupdraft1    = nupdraft(i)
    ktop_plume1  = ktop_plume(i)

    pblh1   = pblh(i)
    delta1  = delta(i)
    wstar1  = wstar(i)

    do k = kts,kte
       cldfra_bl1(k) = cldfra_bl(i,k)
       qc_bl1(k)     = qc_bl(i,k)
       qi_bl1(k)     = qi_bl(i,k)
    enddo

    do k = kts,kte
       el_pbl1(k)  = el_pbl(i,k)
       qke1(k)     = qke(i,k)
       qke_adv1(k) = qke_adv(i,k)
       cov1(k)     = cov(i,k)
       qsq1(k)     = qsq(i,k)
       tsq1(k)     = tsq(i,k)
       sh1(k)      = sh3d(i,k)
    enddo

!--- initialization of 1D local variables:
    ktop_plume1 = 0
    nupdraft1   = 0
    maxmf1       = 0._kind_phys
    maxkhtopdown = 0._kind_phys

    do k = kts,kte
       du1(k)      = 0._kind_phys
       dv1(k)      = 0._kind_phys
       dth1(k)     = 0._kind_phys
       dqv1(k)     = 0._kind_phys
       dqc1(k)     = 0._kind_phys
       dqi1(k)     = 0._kind_phys
       dqnc1(k)    = 0._kind_phys
       dqni1(k)    = 0._kind_phys
       dqnifa1(k)  = 0._kind_phys
       dqnwfa1(k)  = 0._kind_phys
       dqozone1(k) = 0._kind_phys
    enddo
    do k = kts,kte
       edmf_a1(k)   = 0._kind_phys
       edmf_w1(k)   = 0._kind_phys
       edmf_qc1(k)  = 0._kind_phys
       edmf_ent1(k) = 0._kind_phys
       edmf_qt1(k)  = 0._kind_phys
       edmf_thl1(k) = 0._kind_phys
       sub_thl1(k)  = 0._kind_phys
       sub_sqv1(k)  = 0._kind_phys
       det_thl1(k)  = 0._kind_phys
       det_sqv1(k)  = 0._kind_phys
    enddo
    do k = kts,kte
       dqke1(k)   = 0._kind_phys
       qwt1(k)    = 0._kind_phys
       qshear1(k) = 0._kind_phys
       qbuoy1(k)  = 0._kind_phys
       qdiss1(k)  = 0._kind_phys
       exch_h1(k) = 0._kind_phys
       exch_m1(k) = 0._kind_phys
    enddo
    do k = kts,kte
       sub_u(k)   = 0._kind_phys
       sub_v(k)   = 0._kind_phys
       det_sqc(k) = 0._kind_phys
       det_u(k)   = 0._kind_phys
       det_v(k)   = 0._kind_phys
    enddo
    do k = kts,kte+1
       s_aw1(k)      = 0._kind_phys
       s_awthl1(k)   = 0._kind_phys
       s_awqt1(k)    = 0._kind_phys
       s_awqv1(k)    = 0._kind_phys
       s_awqc1(k)    = 0._kind_phys
       s_awu1(k)     = 0._kind_phys
       s_awv1(k)     = 0._kind_phys
       s_awqke1(k)   = 0._kind_phys
       s_awqnc1(k)   = 0._kind_phys
       s_awqni1(k)   = 0._kind_phys
       s_awqnwfa1(k) = 0._kind_phys
       s_awqnifa1(k) = 0._kind_phys
    enddo
    do k = kts,kte
       cldfra_bl1_old(k) = 0._kind_phys
       qc_bl1_old(k)     = 0._kind_phys
       qi_bl1_old(k)     = 0._kind_phys
    enddo

    do k = kts,kte
       qv1(k) = sqv1(k)/(1.-sqv1(k))
       qc1(k) = sqc1(k)/(1.-sqv1(k))
       qi1(k) = sqi1(k)/(1.-sqv1(k))
    enddo

    k = kts
    zw(k) = 0._kind_phys
    do k = kts+1,kte+1
       zw(k) = zw(k-1) + dz1(k-1)
    enddo

    do k = kts,kte
       sqw(k) = sqv1(k)+sqc1(k)+sqi1(k)
       thl(k) = th1(k) - xlvcp/ex1(k)*sqc1(k) - xlscp/ex1(k)*sqi1(k)
       thetav(k) = th1(k)*(1.+0.61*sqv1(k))

       !Use form from Tripoli and Cotton (1981) with their
       !suggested min temperature to improve accuracy.    
       !thl(k)=th(k)*(1.- xlvcp/MAX(tk1(k),TKmin)*sqc1(k) - xlscp/MAX(tk1(k),TKmin)*sqi1(k))

       !compute thl using sgs clouds for pblh diag:
       if(sqc1(k)<1e-6 .and. sqi1(k)<1e-8 .and. cldfra_bl1(k)>0.001)then
          sqc9 = qc_bl1(k)*cldfra_bl1(k)
          sqi9 = qi_bl1(k)*cldfra_bl1(k)
       else
          sqc9 = sqc1(k)
          sqi9 = sqi1(k)
       endif
       thlsg(k) = th1(k) - xlvcp/ex1(k)*sqc9 - xlscp/ex1(k)*sqi9
       thvl(k)  = thlsg(k)*(1.+0.61*sqv1(k))
    enddo


!-----------------------------------------------------------------------------------------------------------------
!initflag > 0:
!-----------------------------------------------------------------------------------------------------------------
    if(initflag > 0 .and. .not.restart) then

       !test to see if we want to initialize qke1:
       if((restart .or. cycling)) then
          if(qke1(kts) < 0.0002) then
             initialize_qke = .true.
             !print*,"qke is too small, must initialize"
          else
             initialize_qke = .false.
             !print*,"using background qke, will not initialize"
          endif
       else ! not cycling or restarting:
          initialize_qke = .true.
          !print*,"not restart nor cycling, must initialize qke"
       endif

       if(.not.restart .or. .not.cycling) then
          do k = kts,kte
             sh1(k)        = 0._kind_phys
             el_pbl1(k)    = 0._kind_phys
             tsq1(k)       = 0._kind_phys
             qsq1(k)       = 0._kind_phys
             cov1(k)       = 0._kind_phys
             cldfra_bl1(k) = 0._kind_phys
             qc_bl1(k)     = 0._kind_phys
             qi_bl1(k)     = 0._kind_phys
             qke1(k)       = 0._kind_phys
          enddo
       endif
       do k = kts,kte
          cldfra_bl1_old(k) = 0._kind_phys
          qc_bl1_old(k)     = 0._kind_phys
          qi_bl1_old(k)     = 0._kind_phys
       enddo

       if(initialize_qke) then
          do k = kts,kte
             qke1(k)=5.*ust1*max((ust1*700.-zw(k))/(max(ust1,0.01)*700.),0.01)
          enddo
       endif

       !--- computes the PBL height:
       call get_pblh(kts,kte,pblh1,thvl,qke1,zw,dz1,xland1,kpbl1)

       !--- computes the similarity functions:
       if(scaleaware > 0.) then
          call scale_aware(dx1,pblh1,psig_bl,psig_shcu)
       else
          psig_bl = 0._kind_phys
          psig_shcu = 0._kind_phys
       endif

       !--- calls mym_initialize:
       call mym_initialize( & 
                kts,kte,                     &
                dz1,dx1,zw,                  &
                u1,v1,thl,sqv1,              &
                pblh1,th1,sh1,               &
                ust1, rmol1,                 &
                el_pbl1,qke1,tsq1,qsq1,cov1, &
                psig_bl,cldfra_bl1,          &
                bl_mynn_mixlength,           &
                edmf_w1,edmf_a1,edmf_qc1,    &
                bl_mynn_edmf,                &
                initialize_qke,              &
                spp_pbl,rstoch_col )

    endif
!-----------------------------------------------------------------------------------------------------------------
!end initflag > 0:
!-----------------------------------------------------------------------------------------------------------------


    if(bl_mynn_tkeadvect) then
       do k = kts,kte
          qke1(k) = qke_adv1(k)
       enddo
    endif
    !Joe-TKE budget:
    if(bl_mynn_tkebudget == 1) then
       do k = kts,kte
          dqke1(k) = qke1(k)
       enddo
    endif
    if(icloud_bl > 0) then
       do k = kts,kte
          cldfra_bl1_old(k) = cldfra_bl1(k)
          qc_bl1_old(k)     = qc_bl1(k)
          qi_bl1_old(k)     = qi_bl1(k)
       enddo
    endif

    !--- computes the PBL height:
    call get_pblh(kts,kte,pblh1,thvl,qke1,zw,dz1,xland1,kpbl1)

    !--- computes the similarity functions:
    if(scaleaware > 0.) then
       call scale_aware(dx1,pblh1,psig_bl,psig_shcu)
    else
       psig_bl   = 1._kind_phys
       psig_shcu = 1._kind_phys
    endif

    sqcg   = 0.0   !JOE, it was: qcg1/(1.+qcg1)
    cpm    = cp*(1.+0.84*qv1(kts))
    exnerg = (ps1/p1000mb)**rcp

    !-----------------------------------------------------
    !ORIGINAL CODE
    !flt = hfx(i,j)/( rho(i,kts,j)*cpm ) &
    ! +xlvcp*ch(i,j)*(sqc(kts)/exner(i,kts,j) -sqcg/exnerg)
    !flq = qfx(i,j)/  rho(i,kts,j)       &
    !    -ch(i,j)*(sqc(kts)   -sqcg )
    !-----------------------------------------------------
    ! Katata-added - The deposition velocity of cloud (fog)
    ! water is used instead of CH.
    flt = hfx1/(rho1(kts)*cpm) &
        + xlvcp*vdfg1*(sqc1(kts)/ex1(kts)-sqcg/exnerg)
    flq = qfx1/rho1(kts)       &
        - vdfg1*(sqc1(kts)-sqcg)

    !JOE-test- should this be after the call to mym_condensation?-using old vt & vq
    !same as original form
    !flt = flt + xlvcp*ch1*(sqc1(kts)/ex1(kts)-sqcg/exnerg)
    flqv = qfx1/rho1(kts)
    flqc = -vdfg1*(sqc1(kts)-sqcg)
    th_sfc = ts1/ex1(kts)

    zet = 0.5*dz1(kts)*rmol1
    if(zet >= 0.0) then
       pmz = 1.0 + (cphm_st-1.0) * zet
       phh = 1.0 +  cphh_st      * zet
    else
       pmz = 1.0/(1.0-cphm_unst*zet)**0.25 - zet
       phh = 1.0/sqrt(1.0-cphh_unst*zet)
    endif

    !-- Estimate wstar & delta for GRIMS shallow-cu-------
    govrth = grav/th1(kts)
    sflux  = hfx1/rho1(kts)/cpm + &
             qfx1/rho1(kts)*ep_1*th1(kts)
    bfx0   = max(sflux,0.)
    wstar3 = (govrth*bfx0*pblh1)
    wstar1 = wstar3**h1
    wm3    = wstar3 + 5.*ust1**3.
    wm2    = wm3**h2
    delb   = govrth*d3*pblh1
    delta1 = min(d1*pblh1 + d2*wm2/delb, 100.)
    !-- End GRIMS-----------------------------------------


    !call mym_condensation() to calculate the nonconvective component of the subgrid-scale cloud fraction
    !and mixing ratio as well as the functions used to calculate the buoyancy flux. Different cloud PDFs
    !can be selected by use of the namelist parameter bl_mynn_cloudpdf:
    do k = kts,kte
       vt(k)  = 0._kind_phys
       vq(k)  = 0._kind_phys
       sgm(k) = 0._kind_phys
    enddo

    call  mym_condensation(kts,kte,              &
              dx1,dz1,zw,thl,sqw,sqv1,sqc1,sqi1, &
              p1,ex1,tsq1,qsq1,cov1,             &
              sh1,el_pbl1,bl_mynn_cloudpdf,      &
              qc_bl1,qi_bl1,cldfra_bl1,          &
              pblh1,hfx1,                        &
              vt,vq,th1,sgm,rmol1,               &
              spp_pbl,rstoch_col)


    !add TKE source driven by cloud top cooling. calculate the buoyancy production of tke from cloud-top
    !cooling when bl_mynn_topdown =1.
    if(bl_mynn_topdown == 1)then
       cloudflg = .false.
       minrad   = 100.
       kminrad  = kpbl1
       zminrad  = pblh1
       do k = kts,kte
          khtopdown(k) = 0._kind_phys
          tkeprodtd(k) = 0._kind_phys
       enddo
       maxkhtopdown = 0._kind_phys
       !check for stratocumulus-topped boundary layers
       do kk = max(1,kpbl1-2),kpbl1+3
          if(sqc1(kk).gt. 1.e-6 .or. sqi1(kk).gt. 1.e-6 .or. cldfra_bl1(kk).gt.0.5) then
             cloudflg=.true.
          endif
          if(rthraten1(kk) < minrad)then
             minrad  = rthraten1(kk)
             kminrad = kk
             zminrad = zw(kk) + 0.5*dz1(kk)
          endif
       enddo
       if(max(kminrad,kpbl1) < 2) cloudflg = .false.
       if(cloudflg) then
          zl1 = dz1(kts)
          k = max(kpbl1-1, kminrad-1)
          !best estimate of height of tke source (top of downdrafts):
          !zminrad = 0.5*pblh(i,j) + 0.5*zminrad

          templ=thl(k)*ex1(k)
          !rvls is ws at full level
          rvls  = 100.*6.112*exp(17.67*(templ-273.16)/(templ-29.65))*(ep_2/p1(k+1))
          temps = templ + (sqw(k)-rvls)/(cp/xlv + ep_2*xlv*rvls/(rd*templ**2))
          rvls  = 100.*6.112*exp(17.67*(temps-273.15)/(temps-29.65))*(ep_2/p1(k+1))
          rcldb = max(sqw(k)-rvls,0.)

          !entrainment efficiency
          dthvx = (thl(k+2) + th1(k+2)*ep_1*sqw(k+2)) &
                - (thl(k)   + th1(k)  *ep_1*sqw(k))
          dthvx = max(dthvx,0.1)
          tmp1  = xlvcp * rcldb/(ex1(k)*dthvx)
          !originally from nichols and turton (1986), where a2 = 60, but lowered
          !here to 8, as in grenier and bretherton (2001).
          ent_eff   = 0.2 + 0.2*8.*tmp1

          radsum = 0._kind_phys
          do kk = max(1,kpbl1-3),kpbl1+3
             radflux = rthraten1(kk)*ex1(kk)         !converts theta/s to temp/s
             radflux = radflux*cp/grav*(p1(kk)-p1(kk+1)) ! converts temp/s to w/m^2
             if(radflux < 0.0) radsum = abs(radflux)+radsum
          enddo

          !more strict limits over land to reduce stable-layer mixouts
          if((xland1-1.5).ge.0)then ! water
             radsum = min(radsum,90.0)
             bfx0 = max(radsum/rho1(k)/cp,0.)
          else                      ! land
             radsum = min(0.25*radsum,30.0)!practically turn off over land
             bfx0 = max(radsum/rho1(k)/cp - max(sflux,0.0),0.)
          endif

          !entrainment from pbl top thermals
          wm3    = grav/thetav(k)*bfx0*min(pblh1,1500.) ! this is wstar3(i)
          wm2    = wm2 + wm3**h2
          bfxpbl = - ent_eff * bfx0
          dthvx  = max(thetav(k+1)-thetav(k),0.1)
          we     = max(bfxpbl/dthvx,-sqrt(wm3**h2))

          do kk = kts,kpbl1+3
             !analytic vertical profile
             zfac(kk) = min(max((1.-(zw(kk+1)-zl1)/(zminrad-zl1)),zfmin),1.)
             zfacent(kk) = 10.*max((zminrad-zw(kk+1))/zminrad,0.0)*(1.-zfac(kk))**3

             !calculate an eddy diffusivity profile (not used at the moment)
             wscalek2(kk) = (phifac*karman*wm3*(zfac(kk)))**h1
             !modify shape of kh to be similar to lock et al (2000): use pfac = 3.0
             khtopdown(kk) = wscalek2(kk)*karman*(zminrad-zw(kk+1))*(1.-zfac(kk))**3 !pfac
             khtopdown(kk) = max(khtopdown(kk),0.0)
             !do not include xkzm at kpbl-1 since it changes entrainment
             !if (kk.eq.kpbl1-1 .and. cloudflg .and. we.lt.0.0) then
             !   khtopdown(kk) = 0.0
             !endif
                   
             !calculate tke production = 2(g/th)(w'th'), where w'th' = a(th/g)wstar^3/pblh,
             !a = ent_eff, and wstar is associated with the radiative cooling at top of pbl.
             !an analytic profile controls the magnitude of this tke prod in the vertical. 
             tkeprodtd(kk) = 2.*ent_eff*wm3/max(pblh1,100.)*zfacent(kk)
             tkeprodtd(kk) = max(tkeprodtd(kk),0.0)
          enddo
       endif !end cloud check
       maxkhtopdown = maxval(khtopdown(kts:kte))
    else
       maxkhtopdown = 0._kind_phys
       do k = kts,kte
          khtopdown(k) = 0._kind_phys
          tkeprodtd(k) = 0._kind_phys
       enddo
    endif !end top-down check


    !--- calls subroutine dmp_mf():
    if(bl_mynn_edmf > 0) then
    call dmp_mf( &
             kts,kte,delt,zw,dz1,p1,           &
             bl_mynn_edmf_mom,                 &
             bl_mynn_edmf_tke,                 &
             bl_mynn_mixscalars,               &
             u1,v1,w1,th1,thl,thetav,tk1,      &
             sqw,sqv1,sqc1,qke1,               &
             qnc1,qni1,qnwfa1,qnifa1,          &
             ex1,vt,vq,sgm,                    &
             ust1,flt,flq,flqv,flqc,           &
             pblh1,kpbl1,dx1,                  &
             xland1,th_sfc,                    &
             !now outputs - tendencies
             !dth1mf,dqv1mf,dqc1mf,du1mf,dv1mf &
             !outputs - updraft properties
             edmf_a1,edmf_w1,edmf_qt1,         &
             edmf_thl1,edmf_ent1,edmf_qc1,     &
             !for the solver
             s_aw1,s_awthl1,s_awqt1,           &
             s_awqv1,s_awqc1,                  &
             s_awu1,s_awv1,s_awqke1,           &
             s_awqnc1,s_awqni1,                &
             s_awqnwfa1,s_awqnifa1,            &
             sub_thl1,sub_sqv1,                &
             sub_u,sub_v,                      &
             det_thl1,det_sqv1,det_sqc,        &
             det_u,det_v,                      &
             qc_bl1,cldfra_bl1,                &
             qc_bl1_old,cldfra_bl1_old,        &
             flag_qc,flag_qi,                  &
             flag_qnc,flag_qni,                &
             flag_qnwfa,flag_qnifa,            &
             psig_shcu,                        &
             nupdraft1,ktop_plume1,            &
             maxmf1,ztop_plume,                &
             spp_pbl,rstoch_col)
    endif


    !--- calls subroutine mym_turbulence() to collect the necessary variable
    !    to carry our successive calculations:
    call mym_turbulence &
             (kts,kte,levflag,                       &
              dz1,dx1,zw,                            &
              u1,v1,thl,sqc1,sqw,                    &
              qke1,tsq1,qsq1,cov1,                   &
              vt,vq,                                 &
              rmol1,flt,flq,                         &
              pblh1,th1,                             &
              sh1,el_pbl1,                           &
              dfm,dfh,dfq,                           &
              tcd,qcd,pdk,                           &
              pdt,pdq,pdc,                           &
              qwt1,qshear1,qbuoy1,qdiss1,            &
              bl_mynn_tkebudget,                     &
              psig_bl,psig_shcu,                     &     
              cldfra_bl1,bl_mynn_mixlength,          &
              edmf_w1,edmf_a1,edmf_qc1,bl_mynn_edmf, &
              tkeprodtd,                             &
              spp_pbl,rstoch_col)


    !--- calls subroutine mym_predict():
    call mym_predict &
             (kts,kte,levflag,               &
              delt,dz1,                      &
              ust1,flt,flq,pmz,phh,          &
              el_pbl1,dfq,pdk,pdt,pdq,pdc,   &
              qke1,tsq1,qsq1,cov1,           &
              s_aw1,s_awqke1,bl_mynn_edmf_tke)

    do k = kts,kte-1
       ! set max dissipative heating rate close to 0.1 k per hour (=0.000027...)
       diss_heat(k) = min(max(twothirds*(qke1(k)**1.5)/ &
                     (b1*max(0.5*(el_pbl1(k)+el_pbl1(k+1)),1.))/cp, 0.0),0.00003)
    enddo
    diss_heat(kte) = 0.


    !--- call to subroutine mynn_tendencies:
    call mynn_tendencies(kts,kte, &
              levflag,grav_settling,          &
              delt,dz1,rho1,                  &
              u1,v1,th1,tk1,qv1,              &
              qc1,qi1,qnc1,qni1,              &
              p1,ex1,thl,sqv1,sqc1,sqi1,sqw,  &
              qnwfa1,qnifa1,qozone1,          &
              ust1,flt,flq,flqv,flqc,         &
              wspd1,qcg1,                     &
              uoce1,voce1,                    &
              tsq1,qsq1,cov1,                 &
              tcd,qcd,                        &
              dfm,dfh,dfq,                    &
              du1,dv1,dth1,dqv1,              &
              dqc1,dqi1,dqnc1,dqni1,          &
              dqnwfa1,dqnifa1,dqozone1,       &
              vdfg1,diss_heat,                &
              !mass flux components
              s_aw1,s_awthl1,s_awqt1,         &
              s_awqv1,s_awqc1,s_awu1,s_awv1,  &
              s_awqnc1,s_awqni1,              &
              s_awqnwfa1,s_awqnifa1,          &
              sub_thl1,sub_sqv1,              &
              sub_u,sub_v,                    &
              det_thl1,det_sqv1,det_sqc,      &
              det_u,det_v,                    &
              flag_qc,flag_qi,flag_qnc,       &
              flag_qni,flag_qnwfa,flag_qnifa, &
              cldfra_bl1,                     &
              bl_mynn_cloudmix,               &
              bl_mynn_mixqt,                  &
              bl_mynn_edmf,                   &
              bl_mynn_edmf_mom,               &
              bl_mynn_mixscalars)


    !--- computes the exchange coefficients:
    call retrieve_exchange_coeffs(kts,kte,dfm,dfh,dz1,exch_m1,exch_h1)


    !--- updates the cloud fraction,and cloud liquid water and cloud ice mixing ratios:
    if(icloud_bl > 0) then
       do k = kts,kte
          !diagnostic-decay for subgrid-scale clouds
          if(cldfra_bl1(k) < cldfra_bl1_old(k)) then
             !decay timescale for calm condition is the eddy turnover
             !timescale, but for windy conditions, it is the advective 
             !timescale. use the minimum of the two.
             ts_decay = min(1800.,3.*dx1/max(sqrt(u1(k)**2 + v1(k)**2),1.0))
             cldfra_bl1(k)= max(cldfra_bl1(k),cldfra_bl1_old(k)-(0.25*delt/ts_decay))
             !qc_bl2 and qi_bl2 are decay rates 
             qc_bl2 = max(qc_bl1(k),qc_bl1_old(k))
             qc_bl2 = max(qc_bl2,1.0e-5)
             qi_bl2 = max(qi_bl1(k),qi_bl1_old(k))
             qi_bl2 = max(qi_bl2,1.0e-6)

             qc_bl1(k) = max(qc_bl1(k),qc_bl1_old(k)-(min(qc_bl2,1.0e-4) * delt/ts_decay))
             qi_bl1(k) = max(qi_bl1(k),qi_bl1_old(k)-(min(qi_bl2,1.0e-5) * delt/ts_decay))
             if(cldfra_bl1(k) < 0.005 .or. (qc_bl1(k) + qi_bl1(k)) < 1e-9) then
                cldfra_bl1(k) = 0._kind_phys
                qc_bl1(k)     = 0._kind_phys
                qi_bl1(k)     = 0._kind_phys
             endif
          endif
       enddo
    else
       do k = kts,kte
          cldfra_bl1(k) = 0._kind_phys
          qc_bl1(k)     = 0._kind_phys
          qi_bl1(k)     = 0._kind_phys
       enddo
    endif


!-----------------------------------------------------------------------------------------------------------------
!begin output of 2D variables:
!-----------------------------------------------------------------------------------------------------------------
    !output tendencies:
    do k = kts,kte
       rublten(i,k)  = du1(k)
       rvblten(i,k)  = dv1(k)
       rthblten(i,k) = dth1(k)
       rqvblten(i,k) = dqv1(k)
    enddo
    if(bl_mynn_cloudmix > 0 .and. flag_qc) then       
       do k = kts,kte
          rqcblten(i,k) = dqc1(k)
       enddo
    endif
    if(bl_mynn_cloudmix > 0 .and. flag_qi) then
       do k = kts,kte
          rqiblten(i,k) = dqi1(k)
       enddo
    endif
    if(bl_mynn_cloudmix > 0 .and. bl_mynn_mixscalars > 0 .and. flag_qnc) then
       do k = kts,kte
          rqncblten(i,k) = dqnc1(k)
       enddo
    endif
    if(bl_mynn_cloudmix > 0 .and. bl_mynn_mixscalars > 0 .and. flag_qni) then
       do k = kts,kte
          rqniblten(i,k) = dqni1(k)
       enddo
    endif
    if(bl_mynn_cloudmix > 0 .and. bl_mynn_mixscalars > 0 .and. flag_qnifa) then
       do k = kts,kte
          rqnifablten(i,k) = dqnifa1(k)
       enddo
    endif
    if(bl_mynn_cloudmix > 0 .and. bl_mynn_mixscalars > 0 .and. flag_qnwfa) then
       do k = kts,kte
          rqnwfablten(i,k) = dqnwfa1(k)
       enddo
    endif
    do k = kts,kte
       rqozblten(i,k) = 0._kind_phys
    enddo

    !inout arrays:
    kpbl(i)       = kpbl1
    nupdraft(i)   = nupdraft1
    ktop_plume(i) = ktop_plume1

    pblh(i)  = pblh1
    delta(i) = delta1
    wstar(i) = wstar1

    do k = kts,kte
       cldfra_bl(i,k) = cldfra_bl1(k)
       qc_bl(i,k)     = qc_bl1(k)
       qi_bl(i,k)     = qi_bl1(k)
    enddo

    do k = kts,kte
       el_pbl(i,k)  = el_pbl1(k)
       qke(i,k)     = qke1(k)
       qke_adv(i,k) = qke_adv1(k)
       cov(i,k)     = cov1(k)
       qsq(i,k)     = qsq1(k)
       tsq(i,k)     = tsq1(k)
       sh3d(i,k)    = sh1(k)
    enddo

    !optional inout arrays:
    if(bl_mynn_edmf > 0) then
       if(bl_mynn_output > 0) then
          do k = kts,kte
             edmf_a(i,k)   = edmf_a1(k)
             edmf_w(i,k)   = edmf_w1(k)
             edmf_qt(i,k)  = edmf_qt1(k)
             edmf_thl(i,k) = edmf_thl1(k)
             edmf_ent(i,k) = edmf_ent1(k)
             edmf_qc(i,k)  = edmf_qc1(k)
             sub_thl(i,k)  = sub_thl1(k)
             sub_sqv(i,k)  = sub_sqv1(k)
             det_thl(i,k)  = det_thl1(k)
             det_sqv(i,k)  = det_sqv1(k)
          enddo
       endif
    endif   

    !output arrays:
    !initialization of output variables:
    maxmf(i) = maxmf1

    do k = kts,kte
       qwt(i,k)    = qwt1(k)
       qshear(i,k) = qshear1(k)
       qbuoy(i,k)  = qbuoy1(k)
       qdiss(i,k)  = qdiss1(k)
       dqke(i,k)   = dqke1(k)
       exch_h(i,k) = exch_h1(k)
       exch_m(i,k) = exch_m1(k)
    enddo

 enddo

!call mpas_log_write('--- end subroutine bl_mynn_run:')

 end subroutine bl_mynn_run

!=================================================================================================================
 subroutine bl_mynn_final(errmsg,errflg)
!=================================================================================================================

 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

!-----------------------------------------------------------------------------------------------------------------
!note: this subroutine currently does nothing.

 errmsg = ' '
 errflg = 0

 end subroutine bl_mynn_final

!=================================================================================================================
 end module bl_mynn
!=================================================================================================================
