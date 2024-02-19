!=================================================================================================================
 module bl_mynn
 use mpas_kind_types,only: kind_phys => RKIND

 use bl_mynn_common,only: &
        cp      , cpv       , cliq      , cice , ep_1 , ep_2  , ep_3  , grav   , karman , p1000mb , &
        r_d     , r_v       , svp1      , svp2 , svp3 , svpt0 , xlf   , xls    , xlv    , p608    , &
        t0c     , tref      , tkmin     , tv0  , gtr  , xlvcp , xlscp , rvovrd , rcp    , cphh_st , &
        cphm_st , cphh_unst , cphm_unst , b1   , b2   , zero
 use bl_mynn_subroutines


 implicit none
 private
 public:: bl_mynn_init,     &
          bl_mynn_finalize, &
          bl_mynn_run


 contains


!=================================================================================================================
 subroutine bl_mynn_init(con_cp,con_cpv,con_cice,con_cliq,con_ep1,con_ep2,con_grav,con_karman,con_p0, &
                         con_rd,con_rv,con_svp1,con_svp2,con_svp3,con_svpt0,con_xlf,con_xls,con_xlv,  &
                         errmsg,errflg)
!=================================================================================================================

!-- input arguments:
 real(kind=kind_phys),intent(in):: &
    con_cp,     &
    con_cpv,    &
    con_cice,   &
    con_cliq

 real(kind=kind_phys),intent(in):: &
    con_ep1,    &
    con_ep2

 real(kind=kind_phys),intent(in):: &
    con_grav

 real(kind=kind_phys),intent(in):: &
    con_karman

 real(kind=kind_phys),intent(in):: &
    con_p0

 real(kind=kind_phys),intent(in):: &
    con_rd,     &
    con_rv

 real(kind=kind_phys),intent(in):: &
    con_svp1,   &
    con_svp2,   &
    con_svp3,   &
    con_svpt0

 real(kind=kind_phys),intent(in):: &
    con_xlf,    &
    con_xls,    &
    con_xlv

 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

!-----------------------------------------------------------------------------------------------------------------

!--- initialization of physics constants needed in the MYNN PBL scheme and already available from MPAS:
 cp      = con_cp
 cpv     = con_cpv
 cliq    = con_cliq
 cice    = con_cice
 ep_1    = con_ep1
 ep_2    = con_ep2
 grav    = con_grav
 karman  = con_karman
 p1000mb = con_p0
 r_d     = con_rd
 r_v     = con_rv
 rvovrd  = r_v/r_d
 svp1    = con_svp1
 svp2    = con_svp2
 svp3    = con_svp3
 svpt0   = con_svpt0
 xlf     = con_xlf
 xls     = con_xls
 xlv     = con_xlv

!--- initialization of derived physics constants needed in the MYNN PBL scheme:
 ep_3    = 1.-ep_2
 gtr     = grav/tref
 p608    = ep_1
 rcp     = r_d/cp
 t0c     = svpt0
 tv0     = p608*tref
 xlscp   = (xlv+xlf)/cp
 xlvcp   = xlv/cp

!ev      = xlv
!rk      = cp/r_d
!svp11   = svp1*1.e3
!tv1     = (1.+p608)*tref
!vk      = karman

 errmsg = " "
 errflg = 0

 end subroutine bl_mynn_init

!=================================================================================================================
 subroutine bl_mynn_finalize(errmsg,errflg)
!=================================================================================================================

 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

!-----------------------------------------------------------------------------------------------------------------
!note: this subroutine currently does nothing.

 errmsg = ' '
 errflg = 0

 end subroutine bl_mynn_finalize

!=================================================================================================================
 subroutine bl_mynn_run &
            (initflag          , restart            , cycling            , &
             delt              , dz                 , dx                 , &
             znt               , u                  , v                  , &
             w                 , th                 , sqv                , &
             sqc               , sqi                , sqs                , &
             qnc               , qni                , qnwfa              , &
             qnifa             , qnbca              , qozone             , &
             p                 , exner              , rho                , &
             tt                , xland              , ts                 , &
             qsfc              , ps                 , ust                , &
             ch                , hfx                , qfx                , &
             rmol              , wspd               , uoce               , &
             voce              , qke                , qke_adv            , &
             tsq               , qsq                , cov                , &
             rublten           , rvblten            , rthblten           , &
             rqvblten          , rqcblten           , rqiblten           , &
             rqsblten          , rqncblten          , rqniblten          , &
             rqnwfablten       , rqnifablten        , rqnbcablten        , &
             rqozblten         , exch_h             , exch_m             , &
             pblh              , kpbl               , el_pbl             , &
             dqke              , qwt                , qshear             , &
             qbuoy             , qdiss              , sh                 , &
             sm                , qc_bl              , qi_bl              , &
             cldfra_bl         , icloud_bl          , bl_mynn_tkeadvect  , &
             bl_mynn_tkebudget , bl_mynn_cloudpdf   , bl_mynn_mixlength  , &
             bl_mynn_closure   , bl_mynn_stfunc     , bl_mynn_topdown    , &
             bl_mynn_edmf      , bl_mynn_edmf_dd    , bl_mynn_edmf_mom   , &
             bl_mynn_edmf_tke  , bl_mynn_mixscalars , bl_mynn_output     , &
             bl_mynn_cloudmix  , bl_mynn_mixqt      , edmf_a             , &
             edmf_w            , edmf_qt            , edmf_thl           , &
             edmf_ent          , edmf_qc            , sub_thl            , &
             sub_sqv           , det_thl            , det_sqv            , &
             edmf_a_dd         , edmf_w_dd          , edmf_qt_dd         , &
             edmf_thl_dd       , edmf_ent_dd        , edmf_qc_dd         , &
             maxwidth          , maxmf              , ztop_plume         , &
             ktop_plume        , spp_pbl            , pattern_spp_pbl    , &
             rthraten          , scaleaware         , dheat_opt          , &
             flag_qc           , flag_qi            , flag_qs            , &
             flag_qnc          , flag_qni           , flag_qnwfa         , &
             flag_qnifa        , flag_qnbca         , flag_qoz           , &
#if(WRF_CHEM == 1)
             mix_chem          , nchem              , kdvel              , &
             ndvel             , chem               , emis_ant_no        , &
             frp               , vdep               ,                      &
#endif
             its, ite , kts , kte , kme , errmsg , errflg                  &
            )    

!subroutine bl_mynn_run &
!            rqozblten         ,
!            its, ite , kts , kte , kme , errmsg , errflg                  &
!           )

! SUBROUTINE mynn_bl_driver(            &
!      &dozone,                         &

!=================================================================================================================

!input arguments:
 logical,intent(in):: &
    flag_qc,flag_qi,flag_qs,flag_qoz,flag_qnc,flag_qni,flag_qnifa,flag_qnwfa,flag_qnbca


 logical,intent(in):: bl_mynn_tkeadvect

 logical,intent(in):: &
    restart,cycling

 integer,intent(in):: its,ite,kts,kte,kme

 integer,intent(in):: &
    initflag,icloud_bl,spp_pbl,dheat_opt

 integer,intent(in):: &
    bl_mynn_cloudpdf,bl_mynn_mixlength,bl_mynn_stfunc,bl_mynn_topdown,bl_mynn_edmf,      &
    bl_mynn_edmf_dd,bl_mynn_edmf_mom,bl_mynn_edmf_tke,bl_mynn_output,bl_mynn_mixscalars, &
    bl_mynn_cloudmix,bl_mynn_mixqt,bl_mynn_tkebudget
 
 real(kind=kind_phys),intent(in):: &
    bl_mynn_closure,scaleaware

 real(kind=kind_phys),intent(in):: &
    delt

 real(kind=kind_phys),intent(in),dimension(its:ite):: &
    dx,          &!
    xland,       &!
    ps,          &!
    ts,          &!
    qsfc,        &!
    ust,         &!
    ch,          &!
    hfx,         &!
    qfx,         &!
    rmol,        &!
    wspd,        &!
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
    sqs,         &!
    qnc,         &!
    qni,         &!
    qnifa,       &!
    qnwfa,       &!
    qnbca,       &!
    qozone        !

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte):: &
    pattern_spp_pbl

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kme):: &
    w             !

!inout arguments:
 integer,intent(inout),dimension(its:ite):: &
    kpbl,        &!
    ktop_plume    !

 real(kind=kind_phys),intent(inout),dimension(its:ite):: &
    pblh

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
    sh,          &!
    sm            !

 real(kind=kind_phys),intent(inout),dimension(its:ite,kts:kte):: &
    rublten,     &!
    rvblten,     &!
    rthblten,    &!
    rqvblten,    &!
    rqcblten,    &!
    rqiblten,    &!
    rqsblten,    &!
    rqncblten,   &!
    rqniblten,   &!
    rqnifablten, &!
    rqnwfablten, &!
    rqnbcablten, &!
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

 real(kind=kind_phys),intent(inout),dimension(its:ite,kts:kte),optional:: &
    edmf_a_dd,   &!
    edmf_w_dd,   &!
    edmf_qt_dd,  &!
    edmf_thl_dd, &!
    edmf_ent_dd, &!
    edmf_qc_dd


!output arguments:
 character(len=*),intent(out):: &
    errmsg        ! output error message (-).

 integer,intent(out):: &
    errflg        ! output error flag (-).

 real(kind=kind_phys),intent(out),dimension(its:ite):: &
    maxwidth,   &!
    maxmf,      &!
    ztop_plume

 real(kind=kind_phys),intent(out),dimension(its:ite,kts:kte):: &
    exch_h,      &!
    exch_m        !

 real(kind=kind_phys),intent(out),dimension(its:ite,kts:kte),optional:: &
    dqke,        &!
    qwt,         &!
    qshear,      &!
    qbuoy,       &!
    qdiss         !


!local variable and arrays:
 logical:: initialize_qke

 integer:: i,k

 real(kind=kind_phys):: qc_bl2,qi_bl2
 real(kind=kind_phys):: cpm,exnerg,flq,flqc,flqv,flt,fltv,phh,pmz,psig_bl,psig_shcu,sqcg,phi_m, &
                        th_sfc,zet,ts_decay

 real(kind=kind_phys),dimension(kts:kte):: cldfra_bl1_old,qc_bl1_old,qi_bl1_old
 real(kind=kind_phys),dimension(kts:kte):: qv1,qc1,qi1,qs1
 real(kind=kind_phys),dimension(kts:kte):: det_sqc,det_u,det_v,sub_u,sub_v

 real(kind=kind_phys),dimension(kts:kte):: pdc,pdk,pdq,pdt,sgm,sqw,thetav,thl,vq,vt,kzero

 real(kind=kind_phys),dimension(kts:kte):: dfh,dfm,dfq,qcd,tcd,diss_heat

 real(kind=kind_phys),dimension(kts:kte):: rstoch_col

 real(kind=kind_phys),dimension(kts:kte+1):: zw

 real(kind=kind_phys),dimension(kts:kte+1):: &
    s_aw1,s_awthl1,s_awqt1,s_awqv1,s_awqc1,s_awu1,s_awv1,s_awqke1,s_awqnc1,s_awqni1, &
    s_awqnwfa1,s_awqnifa1,s_awqnbca1

 real(kind=kind_phys),dimension(kts:kte+1):: &
    sd_aw1,sd_awthl1,sd_awqt1,sd_awqv1,sd_awqc1,sd_awu1,sd_awv1,sd_awqke1

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
 real(kind=kind_phys):: dx1,xland1,ps1,ts1,qsfc1,ust1,ch1,hfx1,qfx1,rmol1,wspd1, &
    uoce1,voce1,znt1
 real(kind=kind_phys),dimension(kts:kte):: &
    dz1,u1,v1,th1,tk1,p1,ex1,rho1,qnc1,qni1,qnifa1,qnwfa1,qnbca1,qozone1,rthraten1,sqv1,sqc1,sqi1,sqs1
 real(kind=kind_phys),dimension(kts:kme):: w1

!local 1D inout arguments:
 integer:: kpbl1,ktop_plume1

 real(kind=kind_phys):: pblh1
 real(kind=kind_phys),dimension(kts:kte):: cldfra_bl1,qc_bl1,qi_bl1
 real(kind=kind_phys),dimension(kts:kte):: el_pbl1,qke1,qke_adv1,cov1,qsq1,tsq1,sh1,sm1
 real(kind=kind_phys),dimension(kts:kte):: du1,dv1,dth1,dqv1,dqc1,dqi1,dqs1,dqnc1,dqni1,dqnifa1,dqnwfa1, &
                                           dqnbca1,dqozone1
 real(kind=kind_phys),dimension(kts:kte):: edmf_a1,edmf_w1,edmf_qt1,edmf_thl1,edmf_ent1,edmf_qc1,sub_thl1, &
                                           sub_sqv1,det_thl1,det_sqv1
 real(kind=kind_phys),dimension(kts:kte):: edmf_a_dd1,edmf_w_dd1,edmf_qt_dd1,edmf_thl_dd1,edmf_ent_dd1, &
                                           edmf_qc_dd1

!local 1D output arguments:
 real(kind=kind_phys):: maxwidth1,maxmf1,ztop_plume1
 real(kind=kind_phys),dimension(kts:kte):: exch_h1,exch_m1

 real(kind=kind_phys),dimension(kts:kte):: dqke1,qwt1,qshear1,qbuoy1,qdiss1

!substepping TKE:
 integer:: nsub
 real(kind=kind_phys):: delt2



!VARIABLES NEEDED FOR MIXING OF CHEMICAL SPECIES:
#if(WRF_CHEM == 1)
!--- inputs:
 logical,intent(in):: mix_chem
 integer,intent(in):: nchem,kdvel,ndvel
 real(kind=kind_phys),intent(in),dimension(its:ite),optional:: frp,emis_ant_no
 real(kind=kind_phys),intent(in),dimension(its:ite,ndvel):: vdep

!--- inouts:
 real(kind=kind_phys),intent(inout),dimension(its:ite,kts:kte,nchem):: chem
#else
 logical,parameter:: mix_chem = .false.
 integer,parameter:: nchem = 1
 integer,parameter:: kdvel = 1
 integer,parameter:: ndvel = 1
#endif
!--- local variables and arrays:
 logical,parameter:: rrfs_sd   = .false.
 logical,parameter:: smoke_dbg = .false.
 logical,parameter:: enh_mix   = .false.

 integer:: ic 
 real(kind=kind_phys):: emis_ant_no1,frp1
 real(kind=kind_phys),dimension(ndvel):: vd1
 real(kind=kind_phys),dimension(kts:kte,nchem)::   chem1
 real(kind=kind_phys),dimension(kts:kte+1,nchem):: s_awchem1          
 !END VARIABLES NEEDED FOR MIXING OF CHEMICAL SPECIES.

!-----------------------------------------------------------------------------------------------------------------

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
       rqsblten(i,k)    = 0._kind_phys
       rqncblten(i,k)   = 0._kind_phys       
       rqniblten(i,k)   = 0._kind_phys       
       rqnifablten(i,k) = 0._kind_phys       
       rqnwfablten(i,k) = 0._kind_phys       
       rqnbcablten(i,k) = 0._kind_phys
       rqozblten(i,k)   = 0._kind_phys
    enddo

!--- initialization of 2D output variables:
    ktop_plume(i) = 0
    maxwidth(i)   = 0._kind_phys
    maxmf(i)      = 0._kind_phys
    ztop_plume(i) = 0._kind_phys

!--- initialization of 1D input variables using 2D input variables:
    dx1    = dx(i)
    xland1 = xland(i)
    ps1    = ps(i)
    ts1    = ts(i)
    qsfc1  = qsfc(i)
    ust1   = ust(i)
    ch1    = ch(i)
    hfx1   = hfx(i)
    qfx1   = qfx(i)
    rmol1  = rmol(i)
    wspd1  = wspd(i)
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
       sh1(k)        = sh(i,k)
       sm1(k)        = sm(i,k)
       rthraten1(k)  = rthraten(i,k)
       sqv1(k)       = sqv(i,k)
       sqc1(k)       = sqc(i,k)
       sqi1(k)       = sqi(i,k)
       sqs1(k)       = sqs(i,k)
       qnc1(k)       = qnc(i,k)
       qni1(k)       = qni(i,k)
       qnifa1(k)     = qnifa(i,k)
       qnwfa1(k)     = qnwfa(i,k)
       qnbca1(k)     = qnbca(i,k)
       qozone1(k)    = qozone(i,k)
       kzero(k)      = 0._kind_phys
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
    kpbl1 = kpbl(i)
    pblh1 = pblh(i)

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
       sh1(k)      = sh(i,k)
       sm1(k)      = sm(i,k)
    enddo

!--- initialization of 1D local variables:
    ktop_plume1  = 0
    maxwidth1    = 0._kind_phys
    maxmf1       = 0._kind_phys
    ztop_plume1  = 0._kind_phys
    maxkhtopdown = 0._kind_phys

    do k = kts,kte
       du1(k)      = 0._kind_phys
       dv1(k)      = 0._kind_phys
       dth1(k)     = 0._kind_phys
       dqv1(k)     = 0._kind_phys
       dqc1(k)     = 0._kind_phys
       dqi1(k)     = 0._kind_phys
       dqs1(k)     = 0._kind_phys
       dqnc1(k)    = 0._kind_phys
       dqni1(k)    = 0._kind_phys
       dqnifa1(k)  = 0._kind_phys
       dqnwfa1(k)  = 0._kind_phys
       dqnbca1(k)  = 0._kind_phys
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

       edmf_a_dd1(k)   = 0._kind_phys
       edmf_w_dd1(k)   = 0._kind_phys
       edmf_qc_dd1(k)  = 0._kind_phys
       edmf_ent_dd1(k) = 0._kind_phys
       edmf_qt_dd1(k)  = 0._kind_phys
       edmf_thl_dd1(k) = 0._kind_phys
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
       s_awqnbca1(k) = 0._kind_phys
    enddo
    do k = kts,kte+1
       sd_aw1(k)     = 0._kind_phys
       sd_awthl1(k)  = 0._kind_phys
       sd_awqt1(k)   = 0._kind_phys
       sd_awqv1(k)   = 0._kind_phys
       sd_awqc1(k)   = 0._kind_phys
       sd_awu1(k)    = 0._kind_phys
       sd_awv1(k)    = 0._kind_phys
       sd_awqke1(k)  = 0._kind_phys
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
       qs1(k) = sqs1(k)/(1.-sqv1(k))
    enddo

    k = kts
    zw(k) = 0._kind_phys
    do k = kts+1,kte+1
       zw(k) = zw(k-1) + dz1(k-1)
    enddo

!INITIALIZATION OF LOCAL CHEMICAL SPECIES:
#if(WRF_CHEM == 1)
    do ic = 1,nchem
       vd1(ic) = vdep(i,ic)
       do k = kts,kte
          chem1(k,ic) = chem(i,k,ic)
       enddo
    enddo
    if(present(emis_ant_no) .and. present(frp)) then
       emis_ant_no1 = emis_ant_no(i)
       frp1 = frp(i)
    else
       emis_ant_no1 = 0._kind_phys
       frp1 = 0._kind_phys
    endif
    !END INITIALIZATION OF LOCAL CHEMICAL SPECIES.
#else
    do ic = 1,nchem
       vd1(ic) = 0._kind_phys
       do k = kts,kte
          chem1(k,ic) = 0._kind_phys
       enddo
    enddo
    emis_ant_no1 = 0._kind_phys
    frp1 = 0._kind_phys
#endif
    do ic = 1,nchem
       do k = kts,kte+1
          s_awchem1(k,ic) = 0._kind_phys
       enddo
    enddo
!END INITIALIZATION OF LOCAL CHEMICAL SPECIES.


    do k = kts,kte
       !keep snow out for now - increase ceiling bias
       sqw(k) = sqv1(k)+sqc1(k)+sqi1(k) !+sqs1(k)
       thl(k) = th1(k) - xlvcp/ex1(k)*sqc1(k) - xlscp/ex1(k)*(sqi1(k))!+sqs1(k))
       thetav(k) = th1(k)*(1.+0.608*sqv1(k))

       !Use form from Tripoli and Cotton (1981) with their
       !suggested min temperature to improve accuracy.    
       !thl(k)=th(k)*(1.- xlvcp/MAX(tk1(k),TKmin)*sqc1(k) - xlscp/MAX(tk1(k),TKmin)*sqi1(k))
       !thetav(k) = th1(k)*(1.+p608)*sqv1(k)
    enddo

!-----------------------------------------------------------------------------------------------------------------
!initflag > 0:
!-----------------------------------------------------------------------------------------------------------------
    if(initflag > 0 .and. .not.restart) then

       !test to see if we want to initialize qke1:
       if((restart .or. cycling)) then
          if(qke1(kts) < 0.0002) then
             initialize_qke = .true.
          else
             initialize_qke = .false.
          endif
       else ! not cycling or restarting:
          initialize_qke = .true.
       endif

       if(.not.restart .or. .not.cycling) then
          do k = kts,kte
             sh1(k)        = 0._kind_phys
             sm1(k)        = 0._kind_phys
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
       call get_pblh(kts,kte,pblh1,thetav,qke1,zw,dz1,xland1,kpbl1)

       !--- computes the similarity functions:
       if(scaleaware > 0.) then
          call scale_aware(dx1,pblh1,psig_bl,psig_shcu)
       else
          psig_bl = 1._kind_phys
          psig_shcu = 1._kind_phys
       endif

       !--- calls mym_initialize:
       call mym_initialize(                   & 
                kts,kte,xland1,               &
                dz1,dx1,zw,                   &
                u1,v1,thl,sqv1,               &
                pblh1,th1,thetav,sh1,sm1,     &
                ust1, rmol1,                  &
                el_pbl1,qke1,tsq1,qsq1,cov1,  &
                psig_bl,cldfra_bl1,           &
                bl_mynn_mixlength,            &
                edmf_w1,edmf_a1,              &
                initialize_qke,               &
                spp_pbl,rstoch_col)

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
    call get_pblh(kts,kte,pblh1,thetav,qke1,zw,dz1,xland1,kpbl1)

    !--- computes the similarity functions:
    if(scaleaware > 0.) then
       call scale_aware(dx1,pblh1,psig_bl,psig_shcu)
    else
       psig_bl   = 1._kind_phys
       psig_shcu = 1._kind_phys
    endif

    sqcg   = 0.0   !ill-defined variable; qcg has been removed
    cpm    = cp*(1.+0.84*qv1(kts))
    exnerg = (ps1/p1000mb)**rcp

    !-----------------------------------------------------
    !ORIGINAL CODE
    !flt = hfx(i)/( rho(i,kts)*cpm ) &
    ! +xlvcp*ch(i)*(sqc(kts)/exner(i,kts) -sqcg/exnerg)
    !flq = qfx(i)/  rho(i,kts)       &
    !    -ch(i)*(sqc(kts)   -sqcg )
    !-----------------------------------------------------
    flqv   = qfx1/rho1(kts)
    flqc   = 0.0 !currently no sea-spray fluxes, fog settling hangled elsewhere
    th_sfc = ts1/ex1(kts)

    !--- turbulent flux for the TKE voundary conditions:
    flq  = flqv + flqc                                  ! Latent
    flt  = hfx1/(rho1(kts)*cpm ) - xlvcp*flqc/ex1(kts)  ! Temperature flux
    fltv = flt + flqv*p608*th_sfc                       ! Virtual temperature flux  

    !--- update 1/L using updated sfc heat flux and friction velocity:
    rmol1 = -karman*gtr*fltv/max(ust1**3,1.0e-6)
    zet = 0.5*dz1(kts)*rmol1
    zet = max(zet, -20.)
    zet = min(zet,  20.)                                         

    !if(i.eq.idbg)print*,"updated z/L=",zet                      
    if(bl_mynn_stfunc == 0) then                                
       !original Kansas-type stability functions:
       if(zet >= 0.0) then                                    
          pmz = 1.0 + (cphm_st-1.0) * zet
          phh = 1.0 +  cphh_st      * zet
       else
          pmz = 1.0/    (1.0-cphm_unst*zet)**0.25 - zet          
          phh = 1.0/sqrt(1.0-cphh_unst*zet)                      
       endif
       phi_m = pmz + zet
    else
       !updated stability functions (Puhales, 2020):
       phi_m = phim(zet)
       pmz   = phi_m - zet                                       
       phh   = phih(zet)                                         
    endif 

    !call mym_condensation() to calculate the nonconvective component of the subgrid-scale cloud fraction
    !and mixing ratio as well as the functions used to calculate the buoyancy flux. Different cloud PDFs
    !can be selected by use of the namelist parameter bl_mynn_cloudpdf:
    do k = kts,kte
       vt(k)  = 0._kind_phys
       vq(k)  = 0._kind_phys
       sgm(k) = 0._kind_phys
    enddo

    call  mym_condensation(kts,kte,          &
              dx1,dz1,zw,xland1,             &
              thl,sqw,sqv1,sqc1,sqi1,sqs1,   &
              p1,ex1,tsq1,qsq1,cov1,         &
              sh1,el_pbl1,bl_mynn_cloudpdf,  &
              qc_bl1,qi_bl1,cldfra_bl1,      &
              pblh1,hfx1,                    &
              vt,vq,th1,sgm,rmol1,           &
              spp_pbl,rstoch_col)


    !add TKE source driven by cloud top cooling. calculate the buoyancy production of tke from cloud-top
    !cooling when bl_mynn_topdown =1.
    if(bl_mynn_topdown > 0)then
       call topdown_cloudrad(kts,kte,dz1,zw,fltv,             &
                    xland1,kpbl1,pblh1,                       &
                    sqc1,sqi1,sqw,thl,th1,ex1,p1,rho1,thetav, &
                    cldfra_bl1,rthraten1,                     &
                    maxkhtopdown,khtopdown,tkeprodtd)
    else
       maxkhtopdown = 0._kind_phys
       do k = kts,kte
          khtopdown(k) = 0._kind_phys
          tkeprodtd(k) = 0._kind_phys
       enddo
    endif


    !--- calls subroutine dmp_mf():
    if(bl_mynn_edmf > 0) then
       call dmp_mf( i,                             &
                kts,kte,delt,zw,dz1,p1,rho1,       &
                bl_mynn_edmf_mom,                  &
                bl_mynn_edmf_tke,                  &
                bl_mynn_mixscalars,                &
                u1,v1,w1,th1,thl,thetav,tk1,       &
                sqw,sqv1,sqc1,qke1,                &
                qnc1,qni1,qnwfa1,qnifa1,qnbca1,    &
                ex1,vt,vq,sgm,                     &
                ust1,flt,fltv,flq,flqv,            &
                pblh1,kpbl1,dx1,                   &
                xland1,th_sfc,                     &
                !now outputs - tendencies
                !dth1mf,dqv1mf,dqc1mf,du1mf,dv1mf, &
                !outputs - updraft properties
                edmf_a1,edmf_w1,edmf_qt1,          &
                edmf_thl1,edmf_ent1,edmf_qc1,      &
                !for the solver
                s_aw1,s_awthl1,s_awqt1,            &
                s_awqv1,s_awqc1,                   &
                s_awu1,s_awv1,s_awqke1,            &
                s_awqnc1,s_awqni1,                 &
                s_awqnwfa1,s_awqnifa1,s_awqnbca1,  &
                sub_thl1,sub_sqv1,                 &
                sub_u,sub_v,                       &
                det_thl1,det_sqv1,det_sqc,         &
                det_u,det_v,                       &
                !chem/smoke mixing
                nchem,chem1,s_awchem1,             &
                mix_chem,                          &
                qc_bl1,cldfra_bl1,                 &
                qc_bl1_old,cldfra_bl1_old,         &
                flag_qc,flag_qi,                   &
                flag_qnc,flag_qni,                 &
                flag_qnwfa,flag_qnifa,flag_qnbca,  &
                psig_shcu,                         &
                maxwidth1,ktop_plume1,             &
                maxmf1,ztop_plume1,                &
                spp_pbl,rstoch_col)

       if(bl_mynn_edmf_dd == 1) then
          call ddmf_jpl(kts,kte,delt,zw,dz1,p1,         &
                    u1,v1,th1,thl,thetav,tk1,           &
                    sqw,sqv1,sqc1,rho1,ex1,             &
                    ust1,flt,flq,                       &
                    pblh1,kpbl1,                        &
                    edmf_a_dd1,edmf_w_dd1,edmf_qt_dd1,  &
                    edmf_thl_dd1,edmf_ent_dd1,          &
                    edmf_qc_dd1,                        &
                    sd_aw1,sd_awthl1,sd_awqt1,          &
                    sd_awqv1,sd_awqc1,sd_awu1,sd_awv1,  &
                    sd_awqke1,                          &
                    qc_bl1,cldfra_bl1,                  &
                    rthraten)
       endif
    endif


    !--- capability to substep the eddy-diffusivity portion:
    !do nsub = 1,2
    delt2 = delt !*0.5    !only works if topdown=0

    call mym_turbulence                        &
             (kts,kte,xland1,bl_mynn_closure,  &
             dz1,dx1,zw,                       &
             u1,v1,thl,thetav,sqc1,sqw,        &
             qke1,tsq1,qsq1,cov1,              &
             vt,vq,                            &
             rmol1,flt,fltv,flq,               &
             pblh1,th1,                        &
             sh1,sm1,el_pbl1,                  &
             dfm,dfh,dfq,                      &
             tcd,qcd,pdk,                      &
             pdt,pdq,pdc,                      &
             qwt1,qshear1,qbuoy1,qdiss1,       &
             bl_mynn_tkebudget,                &
             psig_bl,psig_shcu,                &
             cldfra_bl1,bl_mynn_mixlength,     &
             edmf_w1,edmf_a1,                  &
             tkeprodtd,                        &
             spp_pbl,rstoch_col)


    !--- calls subroutine mym_predict() to solve TKE:
    call mym_predict                             &
             (kts,kte,bl_mynn_closure,           &
              delt2,dz1,                         &
              ust1,flt,flq,pmz,phh,              &
              el_pbl1,dfq,rho1,pdk,pdt,pdq,pdc,  &
              qke1,tsq1,qsq1,cov1,               &
              s_aw1,s_awqke1,bl_mynn_edmf_tke,   &
              qwt1,qdiss1,bl_mynn_tkebudget)      ! TKE budget (Puhales 2020)

    if(dheat_opt > 0) then
       do k = kts,kte-1
          !set max dissipative heating rate to 7.2 K per hour
          diss_heat(k) = min(max(1.0*(qke1(k)**1.5)/(b1*max(0.5*(el_pbl1(k)+el_pbl1(k+1)),1.))/cp,0.0),0.002)

          !limit heating above 100 mb:
          diss_heat(k) = diss_heat(k) * exp(-10000./MAX(p1(k),1.))
       enddo
       diss_heat(kte) = 0.
    else
       do k = kts,kte
          diss_heat(k) = 0.
       enddo
    endif       


    !--- call to subroutine mynn_tendencies:
    call mynn_tendencies(kts,kte, &
              delt,dz1,rho1,                     &
              u1,v1,th1,tk1,qv1,                 &
              qc1,qi1,kzero,qnc1,qni1,           & !kzero replaces qs1 - not mixing snow
              ps1,p1,ex1,thl,                    &
              sqv1,sqc1,sqi1,kzero,sqw,          & !kzero replaces sqs - not mxing snow
              qnwfa1,qnifa1,qnbca1,qozone1,      &
              ust1,flt,flq,flqv,flqc,            &
              wspd1,uoce1,voce1,                 &
              tsq1,qsq1,cov1,                    &
              tcd,qcd,                           &
              dfm,dfh,dfq,                       &
              du1,dv1,dth1,dqv1,                 &
              dqc1,dqi1,dqs1,dqnc1,dqni1,        &
              dqnwfa1,dqnifa1,dqnbca1,           &
              dqozone1,                          &
              diss_heat,                         &
              !mass flux components
              s_aw1,s_awthl1,s_awqt1,            &
              s_awqv1,s_awqc1,s_awu1,s_awv1,     &
              s_awqnc1,s_awqni1,                 &
              s_awqnwfa1,s_awqnifa1,s_awqnbca1,  &
              sd_aw1,sd_awthl1,sd_awqt1,         &
              sd_awqv1,sd_awqc1,                 &
              sd_awu1,sd_awv1,                   &
              sub_thl1,sub_sqv1,                 &
              sub_u,sub_v,                       &
              det_thl1,det_sqv1,det_sqc,         &
              det_u,det_v,                       &
              flag_qc,flag_qi,flag_qnc,          &
              flag_qni,flag_qs,                  &
              flag_qnwfa,flag_qnifa,             &
              flag_qnbca,flag_qoz,               &
              cldfra_bl1,                        &
              bl_mynn_cloudmix,                  &
              bl_mynn_mixqt,                     &
              bl_mynn_edmf,                      &
              bl_mynn_edmf_mom,                  &
              bl_mynn_mixscalars)


    !--- call to subroutine mynn_mix_chem for PBL and tropospheric mixing of
    !    chemical species:
    if(mix_chem) then
       if(rrfs_sd) then
          call mynn_mix_chem(kts,kte,   &
                    delt,dz1,pblh1,     &
                    nchem,kdvel,ndvel,  &
                    chem1,vd1,          &
                    rho1,flt,           &
                    tcd,qcd,            &
                    dfh,                &
                    s_aw1,s_awchem1,    &
                    emis_ant_no1,       &
                    frp1,rrfs_sd,       &
                    enh_mix,smoke_dbg)
       else
          call mynn_mix_chem(kts,kte,   &
                    delt,dz1,pblh1,     &
                    nchem,kdvel,ndvel,  &
                    chem1,vd1,          &
                    rho1,flt,           &
                    tcd,qcd,            &
                    dfh,                &
                    s_aw1,s_awchem1,    &
                    zero,               &
                    zero,rrfs_sd,       &
                    enh_mix,smoke_dbg)
       endif
    endif
#if(WRF == 1)
    !directly updates chem3 instead of computing a tendency:
    do ic = 1,nchem
       do k = kts,kte
          chem(i,k,ic) = max(1.e-12,chem1(k,ic))
       enddo
    enddo
#endif


    !--- computes the exchange coefficients:
    call retrieve_exchange_coeffs(kts,kte,dfm,dfh,dz1,exch_m1,exch_h1)


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
    if(bl_mynn_cloudmix > 0 .and. flag_qs) then
       do k = kts,kte
         rqsblten(i,k) = dqs1(k)
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
    if(bl_mynn_cloudmix > 0 .and. bl_mynn_mixscalars > 0 .and. flag_qnbca) then
       do k = kts,kte
          rqnbcablten(i,k) = dqnbca1(k)
       enddo
    endif
    do k = kts,kte
       rqozblten(i,k) = 0._kind_phys
    enddo

    !inout arrays:
    kpbl(i)       = kpbl1
    ktop_plume(i) = ktop_plume1

    pblh(i)  = pblh1

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
       sh(i,k)      = sh1(k)
       sm(i,k)      = sm1(k)
    enddo


    !the TKE budget is now given in m**2/s**-3 (Puhales, 2020):
    if(present(qwt)   .and. present(qbuoy) .and. present(qshear) .and. &
       present(qdiss) .and. present(dqke)) then
       if(bl_mynn_tkebudget == 1) then
          !lower boundary condtions (using similarity relationships such as the prognostic equation for Qke)
          k = kts
          qshear1(k) = 4.*(ust1**3*phi_m/(karman*dz1(k)))-qshear1(k+1) ! staggered
          qbuoy1(k)  = 4.*(-ust1**3*zet/(karman*dz1(k)))-qbuoy1(k+1)   ! staggered

          !unstaggering shear and buoy and trasnfering all TKE budget to 2D arrays:
          do k = kts,kte-1
             qshear(i,k) = 0.5*(qshear1(k) + qshear1(k+1)) ! unstaggering in z
             qbuoy(i,k)  = 0.5*(qbuoy1(k)  + qbuoy1(k+1))  ! unstaggering in z
             qwt(i,k)    = qwt1(k)
             qdiss(i,k)  = qdiss1(k)
             dqke(i,k)   = (qke1(k)-dqke(i,k))*0.5/delt
          enddo
          !upper boundary conditions
          k = kte
          qshear(i,k) = 0._kind_phys
          qbuoy(i,k)  = 0._kind_phys
          qwt(i,k)    = 0._kind_phys
          qdiss(i,k)  = 0._kind_phys
          dqke(i,k)   = 0._kind_phys
       else
          do k = kts,kte
             qshear(i,k) = 0._kind_phys
             qbuoy(i,k)  = 0._kind_phys
             qwt(i,k)    = 0._kind_phys
             qdiss(i,k)  = 0._kind_phys
             dqke(i,k)   = 0._kind_phys
          enddo
       endif
    endif


    !optional inout arrays for updraft/downdraft properties:
    if(bl_mynn_output > 0 .and. bl_mynn_edmf > 0) then
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
    else
       do k = kts,kte
          edmf_a(i,k)   = 0._kind_phys
          edmf_w(i,k)   = 0._kind_phys
          edmf_qt(i,k)  = 0._kind_phys
          edmf_thl(i,k) = 0._kind_phys
          edmf_ent(i,k) = 0._kind_phys
          edmf_qc(i,k)  = 0._kind_phys
          sub_thl(i,k)  = 0._kind_phys
          sub_sqv(i,k)  = 0._kind_phys
          det_thl(i,k)  = 0._kind_phys
          det_sqv(i,k)  = 0._kind_phys
       enddo
    endif
    if(bl_mynn_output > 0 .and. bl_mynn_edmf_dd > 0) then
       if(present(edmf_a_dd)   .and. present(edmf_w_dd)   .and. present(edmf_qt_dd) .and. &
          present(edmf_thl_dd) .and. present(edmf_ent_dd) .and. present(edmf_qc_dd)) then
          do k = kts,kte
             edmf_a_dd(i,k)   = edmf_a_dd1(k)
             edmf_w_dd(i,k)   = edmf_w_dd1(k)
             edmf_qt_dd(i,k)  = edmf_qt_dd1(k)
             edmf_thl_dd(i,k) = edmf_thl_dd1(k)
             edmf_ent_dd(i,k) = edmf_ent_dd1(k)
             edmf_qc_dd(i,k)  = edmf_qc_dd1(k)
          enddo
       endif
    endif

    !output arrays:
    maxwidth(i)   = maxwidth1
    maxmf(i)      = maxmf1
    ztop_plume(i) = ztop_plume1

    do k = kts,kte
       exch_h(i,k) = exch_h1(k)
       exch_m(i,k) = exch_m1(k)
    enddo

 enddo

 end subroutine bl_mynn_run

!=================================================================================================================
 end module bl_mynn
!=================================================================================================================
