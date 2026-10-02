! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module mpas_atmphys_driver_lsm_noahmp
 use mpas_kind_types
 use mpas_log
 use mpas_pool_routines
 use mpas_timer,only: mpas_timer_start, mpas_timer_stop

 use mpas_atmphys_constants,only: R_d,R_v
 use mpas_atmphys_manager,only  : year,curr_julday,month,day
 use mpas_atmphys_vars,only     : mpas_noahmp,xice_threshold


 use NoahmpIOVarType
 use NoahmpDriverMainMod,only: NoahmpDriverMain

 implicit none
 private
 public:: driver_lsm_noahmp

 
 contains


!=================================================================================================================
 subroutine lsm_noahmp_fromMPAS(configs,mesh,diag,diag_physics,diag_physics_noahmp,output_noahmp,sfc_input, &
                                state,time_lev,itimestep)
!=================================================================================================================

!--- input arguments:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: diag
 type(mpas_pool_type),intent(in):: state

 integer,intent(in):: time_lev
 integer,intent(in):: itimestep


!--- inout arguments:
 type(mpas_pool_type),intent(in):: diag_physics
 type(mpas_pool_type),intent(in):: diag_physics_noahmp
 type(mpas_pool_type),intent(in):: output_noahmp
 type(mpas_pool_type),intent(in):: sfc_input 

 
!--- local variables and arrays:
 logical,pointer:: do_restart

 character(len=StrKIND),pointer:: microp_scheme,    &
                                  convection_scheme

 integer:: i,its,ite
 integer:: n,ns,nsoil,nsnow,nzsnow

 real(kind=RKIND),pointer:: soiltstep
 real(kind=RKIND),dimension(:),pointer:: latCell,lonCell
 real(kind=RKIND),dimension(:),pointer:: shdmax,shdmin,vegfra,tmn,xice,xland

 real(kind=RKIND),dimension(:),pointer:: coszr,glw,gsw,swddir,swddif
 real(kind=RKIND),dimension(:),pointer:: graupelncv,raincv,rainncv,snowncv,sr


!--- local INOUT pointers (with generic LSM equivalent as defined in WRF):
 real(kind=RKIND),dimension(:),pointer:: acsnom,acsnow,canwat,hfx,qfx,qsfc,lh,grdflx,sfc_albedo,sfc_emiss,  &
                                         sfcrunoff,skintemp,smstav,smstot,udrunoff,snow,snowc,snowh,lai,z0, &
                                         znt
 real(kind=RKIND),dimension(:,:),pointer:: sh2o,smois,tslb,smoiseq


!--- local INOUT pointers (with no Noah LSM equivalent as defined in WRF):
 integer,dimension(:),pointer:: isnowxy
 integer,dimension(:),pointer:: irnumsi,irnummi,irnumfi, cropcat, pgsxy
 real(kind=RKIND),dimension(:),pointer:: tvxy,tgxy,canicexy,canliqxy,eahxy,tahxy,cmxy,chxy,fwetxy,sneqvoxy, &
                                         alboldxy,wslakexy,zwtxy,waxy,wtxy,                                 &
                                         lfmassxy,rtmassxy,stmassxy,woodxy,grainxy,gddxy,stblcpxy,          &
                                         fastcpxy,xsaixy,taussxy,qtdrain
 real(kind=RKIND),dimension(:,:),pointer:: tsnoxy,zsnsoxy,snicexy,snliqxy
 real(kind=RKIND),dimension(:),pointer:: irwatsi,irsivol,ireloss,irrsplh,irwatmi,irmivol,irwatfi,irfivol
 real(kind=RKIND),dimension(:),pointer:: smcwtdxy,deeprechxy,rechxy,qrfsxy,qspringsxy,qslatxy, &
                                         rivercondxy,pexpxy
 real(kind=RKIND),dimension(:),pointer:: fsatxy,wsurfxy
 real(kind=RKIND),dimension(:),pointer:: acc_ssoilxy,acc_qinsurxy,acc_qsevaxy,acc_dwaterxy,acc_prcpxy,      &
                                         acc_ecanxy,acc_etranxy,acc_edirxy,acc_glaflwxy
 real(kind=RKIND),dimension(:,:),pointer:: acc_etranixy
 real(kind=RKIND),dimension(:,:),pointer:: albsoildirxy,albsoildifxy
 real(kind=RKIND),dimension(:,:),pointer:: snrdsxy,snfrxy,bcphixy,bcphoxy,ocphixy,ocphoxy,  &
                                           dust1xy,dust2xy,dust3xy,dust4xy,dust5xy,         &
                                           concbcphixy,concbcphoxy,concocphixy,concocphoxy, &
                                           concdust1xy,concdust2xy,concdust3xy,concdust4xy,concdust5xy


!--- local OUT pointers (with no Noah LSM equivalent as defined in WRF):
 real(kind=RKIND),dimension(:),pointer:: tradxy,neexy,gppxy,nppxy,fvegxy,runsfxy,runsbxy,ecanxy,edirxy,  &
                                         etranxy,fsaxy,firaxy,aparxy,psnxy,savxy,sagxy,rssunxy,rsshaxy,  &
                                         bgapxy,wgapxy,tgvxy,tgbxy,chvxy,chbxy,shgxy,shcxy,shbxy,evgxy,  &
                                         evbxy,ghvxy,ghbxy,irgxy,ircxy,irbxy,trxy,evcxy,chleafxy,chucxy, &
                                         chv2xy,chb2xy,rs,qsnowxy,qrainxy


!--- local OUT additional variables:
 real(kind=RKIND),dimension(:),pointer:: pahxy,pahgxy,pahbxy,pahvxy,qintsxy,qintrxy,qdripsxy,qdriprxy,         &
                                         qthrosxy,qthrorxy,qsnsubxy,qmeltxy,qsnfroxy,qsubcxy,qfrocxy,          &
                                         qevacxy,qdewcxy,qfrzcxy,qmeltcxy,qsnbotxy,pondingxy,fpicexy,          &
                                         rainlsm,snowlsm,forctlsm,forcqlsm,forcplsm,forczlsm,forcwlsm,         &
                                         eflxbxy,soilenergy,snowenergy,canhsxy

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write('--- enter subroutine lsm_noahmp_fromMPAS: itimestep = $i',intArgs=(/itimestep/))

 call mpas_pool_get_config(configs,'config_do_restart'       ,do_restart       )
 call mpas_pool_get_config(configs,'config_noahmp_soil_dt'   ,soiltstep        )
 call mpas_pool_get_config(configs,'config_convection_scheme',convection_scheme)
 call mpas_pool_get_config(configs,'config_microp_scheme'    ,microp_scheme    )


!--- initialization of local dimensions:
 its    = mpas_noahmp%its
 ite    = mpas_noahmp%ite
 nsoil  = mpas_noahmp%nsoil
 nsnow  = mpas_noahmp%nsnow
 nzsnow = nsnow + nsoil


!--- initialization of time-varying variables:
 mpas_noahmp%restart_flag = do_restart
 mpas_noahmp%soiltstep    = soiltstep
 mpas_noahmp%itimestep    = itimestep
 mpas_noahmp%yr           = year
 mpas_noahmp%month        = month
 mpas_noahmp%day          = day
 mpas_noahmp%julian       = curr_julday 
 mpas_noahmp%iri_urban    = 0  ! need to connect to iri_scheme in URBPARM.TBL when coupling urban

!--- initialization of xice_threshold:
 mpas_noahmp%xice_threshold = xice_threshold


!--- initialization of INPUT surface variables:
 call mpas_pool_get_array(sfc_input,'shdmax',shdmax)
 call mpas_pool_get_array(sfc_input,'shdmin',shdmin)
 call mpas_pool_get_array(sfc_input,'vegfra',vegfra)
 call mpas_pool_get_array(sfc_input,'tmn'   ,tmn   )
 call mpas_pool_get_array(sfc_input,'xice'  ,xice  )
 call mpas_pool_get_array(sfc_input,'xland' ,xland )

 call mpas_pool_get_array(diag_physics,'coszr'     ,coszr     )
 call mpas_pool_get_array(diag_physics,'glw'       ,glw       )
 call mpas_pool_get_array(diag_physics,'gsw'       ,gsw       )
 call mpas_pool_get_array(diag_physics,'sfc_albedo',sfc_albedo)
 call mpas_pool_get_array(diag_physics,'swddir'    ,swddir    )
 call mpas_pool_get_array(diag_physics,'swddif'    ,swddif    )
 call mpas_pool_get_array(diag_physics,'sr'        ,sr        )
 call mpas_pool_get_array(diag_physics,'raincv'    ,raincv    )
 call mpas_pool_get_array(diag_physics,'rainncv'   ,rainncv   )
 call mpas_pool_get_array(diag_physics,'snowncv'   ,snowncv   )
 call mpas_pool_get_array(diag_physics,'graupelncv',graupelncv)

 do i = its,ite
    mpas_noahmp%coszen(i)     = coszr(i)
    mpas_noahmp%gvfmax(i)     = shdmax(i)
    mpas_noahmp%gvfmin(i)     = shdmin(i)
    mpas_noahmp%vegfra(i)     = vegfra(i)
    mpas_noahmp%tmn(i)        = tmn(i)
    mpas_noahmp%xland(i)      = xland(i)
    mpas_noahmp%xice(i)       = xice(i)
    mpas_noahmp%swdown(i)     = gsw(i) / (1.-sfc_albedo(i)) ! CH: use swdnb? are they equivalent?
    mpas_noahmp%swddir(i)     = swddir(i)
    mpas_noahmp%swddif(i)     = swddif(i)
    mpas_noahmp%glw(i)        = glw(i)
    mpas_noahmp%rainbl(i)     = 0.
    mpas_noahmp%snowbl(i)     = 0.
    mpas_noahmp%rainshv(i)    = 0.
    mpas_noahmp%hailncv(i)    = 0.
    mpas_noahmp%mp_hail(i)    = 0.
    mpas_noahmp%mp_shcv(i)    = 0.
    mpas_noahmp%seaice(i)     = 0.
 enddo

!--- calculation of the instantaneous precipitation rates of rain and snow:
 if (microp_scheme .ne. 'off') then
    do i = its,ite
       mpas_noahmp%sr(i) = sr(i)
       mpas_noahmp%rainncv(i)    = rainncv(i)
       mpas_noahmp%snowncv(i)    = snowncv(i)
       mpas_noahmp%graupelncv(i) = graupelncv(i)
       mpas_noahmp%rainbl(i)     = mpas_noahmp%rainbl(i) + mpas_noahmp%rainncv(i)
       mpas_noahmp%snowbl(i)     = mpas_noahmp%snowbl(i) + mpas_noahmp%snowncv(i)
       mpas_noahmp%mp_rainnc(i)  = rainncv(i)
       mpas_noahmp%mp_snow(i)    = snowncv(i)
       mpas_noahmp%mp_graup(i)   = graupelncv(i)
    enddo
 else
    do i = its,ite
       mpas_noahmp%sr(i) = 0.
       mpas_noahmp%rainncv(i)    = 0.
       mpas_noahmp%snowncv(i)    = 0.
       mpas_noahmp%graupelncv(i) = 0.
       mpas_noahmp%mp_rainnc(i)  = 0.
       mpas_noahmp%mp_snow(i)    = 0.
       mpas_noahmp%mp_graup(i)   = 0.
    enddo
 endif

 if (convection_scheme .ne. 'off') then
    do i = its,ite
       mpas_noahmp%raincv(i)   = raincv(i)
       mpas_noahmp%rainbl(i)   = mpas_noahmp%rainbl(i) + mpas_noahmp%raincv(i)
       mpas_noahmp%mp_rainc(i) = raincv(i)
    enddo
 else
   do i = its,ite
      mpas_noahmp%raincv(i)   = 0.
      mpas_noahmp%mp_rainc(i) = 0.
   enddo
 endif

!--- calculation of the incidence of fractional seaice:
 do i = its,ite
    mpas_noahmp%seaice(i) = 0.
    if(mpas_noahmp%xice(i) .ge. xice_threshold) mpas_noahmp%seaice(i) = 1.
 enddo


!--- initialization of INPUT sounding variables:
 call lsm_noahmp_sounding_fromMPAS(mesh,state,time_lev,diag)


!--- initialization of INOUT variables (with generic LSM equivalent as defined in WRF), i.e.
!    see lines 162-184 in module NoahmpIOVarType.F90):
 call mpas_pool_get_array(sfc_input,'skintemp',skintemp)
 call mpas_pool_get_array(sfc_input,'snowc'   ,snowc   )
 call mpas_pool_get_array(sfc_input,'snow'    ,snow    )
 call mpas_pool_get_array(sfc_input,'snowh'   ,snowh   )
 call mpas_pool_get_array(sfc_input,'sh2o'    ,sh2o    )
 call mpas_pool_get_array(sfc_input,'smois'   ,smois   )
 call mpas_pool_get_array(sfc_input,'tslb'    ,tslb    )

 call mpas_pool_get_array(diag_physics,'hfx'      ,hfx        )
 call mpas_pool_get_array(diag_physics,'qfx'      ,qfx        )
 call mpas_pool_get_array(diag_physics,'lh '      ,lh         )
 call mpas_pool_get_array(diag_physics,'grdflx'   ,grdflx     )
 call mpas_pool_get_array(diag_physics,'smstav'    ,smstav    )
 call mpas_pool_get_array(diag_physics,'smstot'    ,smstot    )
 call mpas_pool_get_array(diag_physics,'sfcrunoff' ,sfcrunoff )
 call mpas_pool_get_array(diag_physics,'udrunoff'  ,udrunoff  )
 call mpas_pool_get_array(diag_physics,'canwat'    ,canwat    )
 call mpas_pool_get_array(diag_physics,'acsnom'    ,acsnom    )
 call mpas_pool_get_array(diag_physics,'acsnow'    ,acsnow    )
 call mpas_pool_get_array(diag_physics,'sfc_emiss' ,sfc_emiss )
 call mpas_pool_get_array(diag_physics,'qsfc'      ,qsfc      )
 call mpas_pool_get_array(diag_physics,'lai'       ,lai       )
 call mpas_pool_get_array(diag_physics,'z0'        ,z0        )
 call mpas_pool_get_array(diag_physics,'znt'       ,znt       )


 do i = its,ite
    mpas_noahmp%tsk(i)       = skintemp(i)
    mpas_noahmp%hfx(i)       = hfx(i)
    mpas_noahmp%qfx(i)       = qfx(i)
    mpas_noahmp%lh(i)        = lh(i)
    mpas_noahmp%grdflx(i)    = grdflx(i)
    mpas_noahmp%smstav(i)    = smstav(i)
    mpas_noahmp%smstot(i)    = smstot(i)
    mpas_noahmp%sfcrunoff(i) = sfcrunoff(i)
    mpas_noahmp%udrunoff(i)  = udrunoff(i)
    mpas_noahmp%albedo(i)    = sfc_albedo(i)
    mpas_noahmp%snowc(i)     = snowc(i)
    mpas_noahmp%snow(i)      = snow(i)
    mpas_noahmp%snowh(i)     = snowh(i)
    mpas_noahmp%canwat(i)    = canwat(i)
    mpas_noahmp%acsnom(i)    = acsnom(i)
    mpas_noahmp%acsnow(i)    = acsnow(i)
    mpas_noahmp%emiss(i)     = sfc_emiss(i)
    mpas_noahmp%qsfc(i)      = qsfc(i)
    mpas_noahmp%lai(i)       = lai(i)
    mpas_noahmp%z0(i)        = z0(i)
    mpas_noahmp%znt(i)       = znt(i)
 enddo

 do ns = 1,nsoil
    do i = its,ite
       mpas_noahmp%sh2o(i,ns)    = sh2o(ns,i)
       mpas_noahmp%smois(i,ns)   = smois(ns,i)
       mpas_noahmp%tslb(i,ns)    = tslb(ns,i)
    enddo
 enddo


!--- initialization of INOUT variables (with no Noah LSM equivalent as defined in WRF), i.e.
!    see lines 186-222 in module NoahmpIOVarType.F90:
 call mpas_pool_get_array(diag_physics_noahmp,'isnowxy'     ,isnowxy     )
 call mpas_pool_get_array(diag_physics_noahmp,'tvxy'        ,tvxy        )
 call mpas_pool_get_array(diag_physics_noahmp,'tgxy'        ,tgxy        )
 call mpas_pool_get_array(diag_physics_noahmp,'canicexy'    ,canicexy    )
 call mpas_pool_get_array(diag_physics_noahmp,'canliqxy'    ,canliqxy    )
 call mpas_pool_get_array(diag_physics_noahmp,'eahxy'       ,eahxy       )
 call mpas_pool_get_array(diag_physics_noahmp,'tahxy'       ,tahxy       )
 call mpas_pool_get_array(diag_physics_noahmp,'cmxy'        ,cmxy        )
 call mpas_pool_get_array(diag_physics_noahmp,'chxy'        ,chxy        )
 call mpas_pool_get_array(diag_physics_noahmp,'fwetxy'      ,fwetxy      )
 call mpas_pool_get_array(diag_physics_noahmp,'sneqvoxy'    ,sneqvoxy    )
 call mpas_pool_get_array(diag_physics_noahmp,'alboldxy'    ,alboldxy    )
 call mpas_pool_get_array(diag_physics_noahmp,'wslakexy'    ,wslakexy    )
 call mpas_pool_get_array(diag_physics_noahmp,'zwtxy'       ,zwtxy       )
 call mpas_pool_get_array(diag_physics_noahmp,'waxy'        ,waxy        )
 call mpas_pool_get_array(diag_physics_noahmp,'wtxy'        ,wtxy        )
 call mpas_pool_get_array(diag_physics_noahmp,'deeprechxy'  ,deeprechxy  )
 call mpas_pool_get_array(diag_physics_noahmp,'rechxy'      ,rechxy      )
 call mpas_pool_get_array(diag_physics_noahmp,'tsnoxy'      ,tsnoxy      )
 call mpas_pool_get_array(diag_physics_noahmp,'zsnsoxy'     ,zsnsoxy     )
 call mpas_pool_get_array(diag_physics_noahmp,'snicexy'     ,snicexy     )
 call mpas_pool_get_array(diag_physics_noahmp,'snliqxy'     ,snliqxy     )
 call mpas_pool_get_array(diag_physics_noahmp,'lfmassxy'    ,lfmassxy    )
 call mpas_pool_get_array(diag_physics_noahmp,'rtmassxy'    ,rtmassxy    )
 call mpas_pool_get_array(diag_physics_noahmp,'stmassxy'    ,stmassxy    )
 call mpas_pool_get_array(diag_physics_noahmp,'woodxy'      ,woodxy      )
 call mpas_pool_get_array(diag_physics_noahmp,'stblcpxy'    ,stblcpxy    )
 call mpas_pool_get_array(diag_physics_noahmp,'fastcpxy'    ,fastcpxy    )
 call mpas_pool_get_array(diag_physics_noahmp,'xsaixy'      ,xsaixy      )
 call mpas_pool_get_array(diag_physics_noahmp,'taussxy'     ,taussxy     )
 call mpas_pool_get_array(diag_physics_noahmp,'acc_dwaterxy',acc_dwaterxy)
 call mpas_pool_get_array(diag_physics_noahmp,'acc_prcpxy'  ,acc_prcpxy  )
 call mpas_pool_get_array(diag_physics_noahmp,'acc_ecanxy'  ,acc_ecanxy  )
 call mpas_pool_get_array(diag_physics_noahmp,'acc_etranxy' ,acc_etranxy )
 call mpas_pool_get_array(diag_physics_noahmp,'acc_edirxy'  ,acc_edirxy  )
 call mpas_pool_get_array(diag_physics_noahmp,'acc_etranixy',acc_etranixy)
 call mpas_pool_get_array(diag_physics_noahmp,'acc_glaflwxy',acc_glaflwxy)
 call mpas_pool_get_array(diag_physics_noahmp,'acc_ssoilxy' ,acc_ssoilxy )
 call mpas_pool_get_array(diag_physics_noahmp,'acc_qinsurxy',acc_qinsurxy)
 call mpas_pool_get_array(diag_physics_noahmp,'acc_qsevaxy' ,acc_qsevaxy )
 call mpas_pool_get_array(diag_physics_noahmp,'albsoildirxy',albsoildirxy)
 call mpas_pool_get_array(diag_physics_noahmp,'albsoildifxy',albsoildifxy)

 do i = its,ite
    mpas_noahmp%isnowxy(i)      = isnowxy(i)
    mpas_noahmp%tvxy(i)         = tvxy(i)
    mpas_noahmp%tgxy(i)         = tgxy(i)
    mpas_noahmp%canicexy(i)     = canicexy(i)
    mpas_noahmp%canliqxy(i)     = canliqxy(i)
    mpas_noahmp%eahxy(i)        = eahxy(i)
    mpas_noahmp%tahxy(i)        = tahxy(i)
    mpas_noahmp%cmxy(i)         = cmxy(i)
    mpas_noahmp%chxy(i)         = chxy(i)
    mpas_noahmp%fwetxy(i)       = fwetxy(i)
    mpas_noahmp%sneqvoxy(i)     = sneqvoxy(i)
    mpas_noahmp%alboldxy(i)     = alboldxy(i)
    mpas_noahmp%wslakexy(i)     = wslakexy(i)
    mpas_noahmp%zwtxy(i)        = zwtxy(i)
    mpas_noahmp%waxy(i)         = waxy(i)
    mpas_noahmp%wtxy(i)         = wtxy(i)
    mpas_noahmp%deeprechxy(i)   = deeprechxy(i)
    mpas_noahmp%rechxy(i)       = rechxy(i)
    mpas_noahmp%lfmassxy(i)     = lfmassxy(i)
    mpas_noahmp%rtmassxy(i)     = rtmassxy(i)
    mpas_noahmp%stmassxy(i)     = stmassxy(i)
    mpas_noahmp%woodxy(i)       = woodxy(i)
    mpas_noahmp%stblcpxy(i)     = stblcpxy(i)
    mpas_noahmp%fastcpxy(i)     = fastcpxy(i)
    mpas_noahmp%xsaixy(i)       = xsaixy(i)
    mpas_noahmp%taussxy(i)      = taussxy(i)
    mpas_noahmp%acc_dwaterxy(i) = acc_dwaterxy(i)
    mpas_noahmp%acc_prcpxy(i)   = acc_prcpxy(i)
    mpas_noahmp%acc_ecanxy(i)   = acc_ecanxy(i)
    mpas_noahmp%acc_etranxy(i)  = acc_etranxy(i)
    mpas_noahmp%acc_edirxy(i)   = acc_edirxy(i)
    mpas_noahmp%acc_ssoilxy(i)  = acc_ssoilxy(i)
    mpas_noahmp%acc_qinsurxy(i) = acc_qinsurxy(i)
    mpas_noahmp%acc_qsevaxy(i)  = acc_qsevaxy(i)
    mpas_noahmp%acc_glaflwxy(i) = acc_glaflwxy(i)
    do n = 1,mpas_noahmp%numrad 
       mpas_noahmp%albsoildirxy(i,n) = albsoildirxy(n,i)
       mpas_noahmp%albsoildifxy(i,n) = albsoildifxy(n,i)
    enddo
 enddo

 do ns = 1,nsoil
    do i = its,ite
       mpas_noahmp%acc_etranixy(i,ns) = acc_etranixy(ns,i)
    enddo
 enddo

 do ns = 1,nsnow
    n = ns - nsnow
    do i = its,ite
       mpas_noahmp%tsnoxy(i,n)  = tsnoxy(ns,i)
       mpas_noahmp%snicexy(i,n) = snicexy(ns,i)
       mpas_noahmp%snliqxy(i,n) = snliqxy(ns,i)
    enddo
 enddo
 do ns = 1,nzsnow
    n = ns - nsnow
    do i = its,ite
       mpas_noahmp%zsnsoxy(i,n) = zsnsoxy(ns,i)
    enddo
 enddo

 if (mpas_noahmp%iopt_crop > 0) then
    call mpas_pool_get_array(diag_physics_noahmp,'cropcat',cropcat)
    call mpas_pool_get_array(diag_physics_noahmp,'gddxy'  ,gddxy  )
    call mpas_pool_get_array(diag_physics_noahmp,'grainxy',grainxy)
    call mpas_pool_get_array(diag_physics_noahmp,'pgsxy'  ,pgsxy  )

    do i = its,ite
       mpas_noahmp%cropcat(i) = cropcat(i)
       mpas_noahmp%grainxy(i) = grainxy(i)
       mpas_noahmp%gddxy(i)   = gddxy(i)
       mpas_noahmp%pgsxy(i)   = pgsxy(i)
    enddo
 endif

 if (mpas_noahmp%iopt_tdrn > 0) then
    call mpas_pool_get_array(diag_physics_noahmp,'qtdrain',qtdrain)

    do i = its,ite
       mpas_noahmp%qtdrain(i) = qtdrain(i)
    enddo
 endif

 if (mpas_noahmp%iopt_irr > 0) then
    call mpas_pool_get_array(diag_physics_noahmp,'irnumsi',irnumsi)
    call mpas_pool_get_array(diag_physics_noahmp,'irsivol',irsivol)
    call mpas_pool_get_array(diag_physics_noahmp,'irwatsi',irwatsi)
    call mpas_pool_get_array(diag_physics_noahmp,'ireloss',ireloss)
    call mpas_pool_get_array(diag_physics_noahmp,'irrsplh',irrsplh)
    call mpas_pool_get_array(diag_physics_noahmp,'irnummi',irnummi)
    call mpas_pool_get_array(diag_physics_noahmp,'irwatmi',irwatmi)
    call mpas_pool_get_array(diag_physics_noahmp,'irmivol',irmivol)
    call mpas_pool_get_array(diag_physics_noahmp,'irnumfi',irnumfi)
    call mpas_pool_get_array(diag_physics_noahmp,'irwatfi',irwatfi)
    call mpas_pool_get_array(diag_physics_noahmp,'irfivol',irfivol)

    do i = its,ite
       mpas_noahmp%irnumsi(i) = irnumsi(i)
       mpas_noahmp%irwatsi(i) = irwatsi(i)
       mpas_noahmp%irsivol(i) = irsivol(i)
       mpas_noahmp%ireloss(i) = ireloss(i)
       mpas_noahmp%irrsplh(i) = irrsplh(i)
       mpas_noahmp%irnummi(i) = irnummi(i)
       mpas_noahmp%irwatmi(i) = irwatmi(i)
       mpas_noahmp%irmivol(i) = irmivol(i)
       mpas_noahmp%irnumfi(i) = irnumfi(i)
       mpas_noahmp%irwatfi(i) = irwatfi(i)
       mpas_noahmp%irfivol(i) = irfivol(i)
    enddo
 endif

 if (mpas_noahmp%iopt_runsub == 5) then  
    call mpas_pool_get_array(diag_physics_noahmp,'smcwtdxy'   ,smcwtdxy   )
    call mpas_pool_get_array(diag_physics_noahmp,'deeprechxy' ,deeprechxy )
    call mpas_pool_get_array(diag_physics_noahmp,'rechxy'     ,rechxy     )
    call mpas_pool_get_array(diag_physics_noahmp,'qrfsxy'     ,qrfsxy     )
    call mpas_pool_get_array(diag_physics_noahmp,'qspringsxy' ,qspringsxy )
    call mpas_pool_get_array(diag_physics_noahmp,'qslatxy'    ,qslatxy    )
    call mpas_pool_get_array(diag_physics_noahmp,'rivercondxy',rivercondxy)
    call mpas_pool_get_array(diag_physics_noahmp,'pexpxy'     ,pexpxy     )
    call mpas_pool_get_array(diag_physics_noahmp,'smoiseq'    ,smoiseq    )

    do i = its,ite
       mpas_noahmp%smcwtdxy(i)    = smcwtdxy(i)
       mpas_noahmp%deeprechxy(i)  = deeprechxy(i)
       mpas_noahmp%rechxy(i)      = rechxy(i)
       mpas_noahmp%qrfsxy(i)      = qrfsxy(i)
       mpas_noahmp%qspringsxy(i)  = qspringsxy(i)
       mpas_noahmp%qslatxy(i)     = qslatxy(i)
       mpas_noahmp%rivercondxy(i) = rivercondxy(i)
       mpas_noahmp%pexpxy(i)      = pexpxy(i)
       do ns = 1,nsoil
          mpas_noahmp%smoiseq(i,ns) = smoiseq(ns,i)
       enddo
    enddo
 endif

 if (mpas_noahmp%iopt_wetland > 0) then  
    call mpas_pool_get_array(diag_physics_noahmp,'fsatxy' ,fsatxy )
    call mpas_pool_get_array(diag_physics_noahmp,'wsurfxy',wsurfxy)

    do i = its,ite
       mpas_noahmp%fsatxy(i)  = fsatxy(i)
       mpas_noahmp%wsurfxy(i) = wsurfxy(i)
    enddo
 endif

 if (mpas_noahmp%iopt_alb == 3) then
    call mpas_pool_get_array(diag_physics_noahmp,'snrdsxy'    ,snrdsxy    )
    call mpas_pool_get_array(diag_physics_noahmp,'snfrxy'     ,snfrxy     )
    call mpas_pool_get_array(diag_physics_noahmp,'bcphixy'    ,bcphixy    )
    call mpas_pool_get_array(diag_physics_noahmp,'bcphoxy'    ,bcphoxy    )
    call mpas_pool_get_array(diag_physics_noahmp,'ocphixy'    ,ocphixy    )
    call mpas_pool_get_array(diag_physics_noahmp,'ocphoxy'    ,ocphoxy    )
    call mpas_pool_get_array(diag_physics_noahmp,'dust1xy'    ,dust1xy    )
    call mpas_pool_get_array(diag_physics_noahmp,'dust2xy'    ,dust2xy    )
    call mpas_pool_get_array(diag_physics_noahmp,'dust3xy'    ,dust3xy    )
    call mpas_pool_get_array(diag_physics_noahmp,'dust4xy'    ,dust4xy    )
    call mpas_pool_get_array(diag_physics_noahmp,'dust5xy'    ,dust5xy    )
    call mpas_pool_get_array(diag_physics_noahmp,'concbcphixy',concbcphixy)
    call mpas_pool_get_array(diag_physics_noahmp,'concbcphoxy',concbcphoxy)
    call mpas_pool_get_array(diag_physics_noahmp,'concocphixy',concocphixy)
    call mpas_pool_get_array(diag_physics_noahmp,'concocphoxy',concocphoxy)
    call mpas_pool_get_array(diag_physics_noahmp,'concdust1xy',concdust1xy)
    call mpas_pool_get_array(diag_physics_noahmp,'concdust2xy',concdust2xy)
    call mpas_pool_get_array(diag_physics_noahmp,'concdust3xy',concdust3xy)
    call mpas_pool_get_array(diag_physics_noahmp,'concdust4xy',concdust4xy)
    call mpas_pool_get_array(diag_physics_noahmp,'concdust5xy',concdust5xy)

    do ns = 1,nsnow
       n = ns - nsnow
       do i = its,ite
          mpas_noahmp%snrdsxy(i,n)         = snrdsxy(ns,i)
          mpas_noahmp%snfrxy(i,n)          = snfrxy(ns,i)
          mpas_noahmp%bcphixy(i,n)         = bcphixy(ns,i)
          mpas_noahmp%bcphoxy(i,n)         = bcphoxy(ns,i)
          mpas_noahmp%ocphixy(i,n)         = ocphixy(ns,i)
          mpas_noahmp%ocphoxy(i,n)         = ocphoxy(ns,i)
          mpas_noahmp%dust1xy(i,n)         = dust1xy(ns,i)
          mpas_noahmp%dust2xy(i,n)         = dust2xy(ns,i)
          mpas_noahmp%dust3xy(i,n)         = dust3xy(ns,i)
          mpas_noahmp%dust4xy(i,n)         = dust4xy(ns,i)
          mpas_noahmp%dust5xy(i,n)         = dust5xy(ns,i)
          mpas_noahmp%massconcbcphixy(i,n) = concbcphixy(ns,i)
          mpas_noahmp%massconcbcphoxy(i,n) = concbcphoxy(ns,i)
          mpas_noahmp%massconcocphixy(i,n) = concocphixy(ns,i)
          mpas_noahmp%massconcocphoxy(i,n) = concocphoxy(ns,i)
          mpas_noahmp%massconcdust1xy(i,n) = concdust1xy(ns,i)
          mpas_noahmp%massconcdust2xy(i,n) = concdust2xy(ns,i)
          mpas_noahmp%massconcdust3xy(i,n) = concdust3xy(ns,i)
          mpas_noahmp%massconcdust4xy(i,n) = concdust4xy(ns,i)
          mpas_noahmp%massconcdust5xy(i,n) = concdust5xy(ns,i)
       enddo
    enddo
 endif


!--- initialization of OUT (with no Noah LSM equivalent as defined in WRF), i.e.
!    see lines 242-290 in module NoahmpIOVarType.F90):
 call mpas_pool_get_array(output_noahmp,'tradxy'  ,tradxy  )
 call mpas_pool_get_array(output_noahmp,'neexy'   ,neexy   )
 call mpas_pool_get_array(output_noahmp,'gppxy'   ,gppxy   )
 call mpas_pool_get_array(output_noahmp,'nppxy'   ,nppxy   )
 call mpas_pool_get_array(output_noahmp,'fvegxy'  ,fvegxy  )
 call mpas_pool_get_array(output_noahmp,'runsfxy' ,runsfxy )
 call mpas_pool_get_array(output_noahmp,'runsbxy' ,runsbxy )
 call mpas_pool_get_array(output_noahmp,'ecanxy'  ,ecanxy  )
 call mpas_pool_get_array(output_noahmp,'edirxy'  ,edirxy  )
 call mpas_pool_get_array(output_noahmp,'etranxy' ,etranxy )
 call mpas_pool_get_array(output_noahmp,'fsaxy'   ,fsaxy   )
 call mpas_pool_get_array(output_noahmp,'firaxy'  ,firaxy  )
 call mpas_pool_get_array(output_noahmp,'aparxy'  ,aparxy  )
 call mpas_pool_get_array(output_noahmp,'psnxy'   ,psnxy   )
 call mpas_pool_get_array(output_noahmp,'savxy'   ,savxy   )
 call mpas_pool_get_array(output_noahmp,'sagxy'   ,sagxy   )
 call mpas_pool_get_array(output_noahmp,'rssunxy' ,rssunxy )
 call mpas_pool_get_array(output_noahmp,'rsshaxy' ,rsshaxy )
 call mpas_pool_get_array(output_noahmp,'bgapxy'  ,bgapxy  )
 call mpas_pool_get_array(output_noahmp,'wgapxy'  ,wgapxy  )
 call mpas_pool_get_array(output_noahmp,'tgvxy'   ,tgvxy   )
 call mpas_pool_get_array(output_noahmp,'tgbxy'   ,tgbxy   )
 call mpas_pool_get_array(output_noahmp,'chvxy'   ,chvxy   )
 call mpas_pool_get_array(output_noahmp,'chbxy'   ,chbxy   )
 call mpas_pool_get_array(output_noahmp,'shgxy'   ,shgxy   )
 call mpas_pool_get_array(output_noahmp,'shcxy'   ,shcxy   )
 call mpas_pool_get_array(output_noahmp,'shbxy'   ,shbxy   )
 call mpas_pool_get_array(output_noahmp,'evgxy'   ,evgxy   )
 call mpas_pool_get_array(output_noahmp,'evbxy'   ,evbxy   )
 call mpas_pool_get_array(output_noahmp,'ghvxy'   ,ghvxy   )
 call mpas_pool_get_array(output_noahmp,'ghbxy'   ,ghbxy   )
 call mpas_pool_get_array(output_noahmp,'irgxy'   ,irgxy   )
 call mpas_pool_get_array(output_noahmp,'ircxy'   ,ircxy   )
 call mpas_pool_get_array(output_noahmp,'irbxy'   ,irbxy   )
 call mpas_pool_get_array(output_noahmp,'trxy'    ,trxy    )
 call mpas_pool_get_array(output_noahmp,'evcxy'   ,evcxy   )
 call mpas_pool_get_array(output_noahmp,'chleafxy',chleafxy)
 call mpas_pool_get_array(output_noahmp,'chucxy'  ,chucxy  )
 call mpas_pool_get_array(output_noahmp,'chv2xy'  ,chv2xy  )
 call mpas_pool_get_array(output_noahmp,'chb2xy'  ,chb2xy  )
 call mpas_pool_get_array(output_noahmp,'rs'      ,rs      )
 call mpas_pool_get_array(output_noahmp,'qsnowxy' ,qsnowxy )
 call mpas_pool_get_array(output_noahmp,'qrainxy' ,qrainxy )

 do i = its,ite
    mpas_noahmp%qsnowxy(i)  = qsnowxy(i)
    mpas_noahmp%qrainxy(i)  = qrainxy(i)
    mpas_noahmp%tradxy(i)   = tradxy(i)
    mpas_noahmp%neexy(i)    = neexy(i)
    mpas_noahmp%gppxy(i)    = gppxy(i)
    mpas_noahmp%nppxy(i)    = nppxy(i)
    mpas_noahmp%fvegxy(i)   = fvegxy(i)
    mpas_noahmp%runsfxy(i)  = runsfxy(i)
    mpas_noahmp%runsbxy(i)  = runsbxy(i)
    mpas_noahmp%ecanxy(i)   = ecanxy(i)
    mpas_noahmp%edirxy(i)   = edirxy(i)
    mpas_noahmp%etranxy(i)  = etranxy(i)
    mpas_noahmp%fsaxy(i)    = fsaxy(i)
    mpas_noahmp%firaxy(i)   = firaxy(i)
    mpas_noahmp%aparxy(i)   = aparxy(i)
    mpas_noahmp%psnxy(i)    = psnxy(i)
    mpas_noahmp%savxy(i)    = savxy(i)
    mpas_noahmp%sagxy(i)    = sagxy(i)
    mpas_noahmp%rssunxy(i)  = rssunxy(i)
    mpas_noahmp%rsshaxy(i)  = rsshaxy(i)
    mpas_noahmp%bgapxy(i)   = bgapxy(i)
    mpas_noahmp%wgapxy(i)   = wgapxy(i)
    mpas_noahmp%tgvxy(i)    = tgvxy(i)
    mpas_noahmp%tgbxy(i)    = tgbxy(i)
    mpas_noahmp%chvxy(i)    = chvxy(i)
    mpas_noahmp%chbxy(i)    = chbxy(i)
    mpas_noahmp%shgxy(i)    = shgxy(i)
    mpas_noahmp%shcxy(i)    = shcxy(i)
    mpas_noahmp%shbxy(i)    = shbxy(i)
    mpas_noahmp%evgxy(i)    = evgxy(i)
    mpas_noahmp%evbxy(i)    = evbxy(i)
    mpas_noahmp%ghvxy(i)    = ghvxy(i)
    mpas_noahmp%ghbxy(i)    = ghbxy(i)
    mpas_noahmp%irgxy(i)    = irgxy(i)
    mpas_noahmp%ircxy(i)    = ircxy(i)
    mpas_noahmp%irbxy(i)    = irbxy(i)
    mpas_noahmp%trxy(i)     = trxy(i)
    mpas_noahmp%evcxy(i)    = evcxy(i)
    mpas_noahmp%chleafxy(i) = chleafxy(i)
    mpas_noahmp%chucxy(i)   = chucxy(i)
    mpas_noahmp%chv2xy(i)   = chv2xy(i)
    mpas_noahmp%chb2xy(i)   = chb2xy(i)
    mpas_noahmp%rs(i)       = rs(i)
 enddo


 !--- update of OUT additional variables, i.e. see lines 292-334 in module NoahmpIOVarType.F90:
 call mpas_pool_get_array(output_noahmp,'pahxy'       ,pahxy        )
 call mpas_pool_get_array(output_noahmp,'pahgxy'      ,pahgxy       )
 call mpas_pool_get_array(output_noahmp,'pahbxy'      ,pahbxy       )
 call mpas_pool_get_array(output_noahmp,'pahvxy'      ,pahvxy       )
 call mpas_pool_get_array(output_noahmp,'qintsxy'     ,qintsxy      )
 call mpas_pool_get_array(output_noahmp,'qintrxy'     ,qintrxy      )
 call mpas_pool_get_array(output_noahmp,'qdripsxy'    ,qdripsxy     )
 call mpas_pool_get_array(output_noahmp,'qdriprxy'    ,qdriprxy     )
 call mpas_pool_get_array(output_noahmp,'qthrosxy'    ,qthrosxy     )
 call mpas_pool_get_array(output_noahmp,'qthrorxy'    ,qthrorxy     )
 call mpas_pool_get_array(output_noahmp,'qsnsubxy'    ,qsnsubxy     )
 call mpas_pool_get_array(output_noahmp,'qmeltxy'     ,qmeltxy      )
 call mpas_pool_get_array(output_noahmp,'qsnfroxy'    ,qsnfroxy     )
 call mpas_pool_get_array(output_noahmp,'qsubcxy'     ,qsubcxy      )
 call mpas_pool_get_array(output_noahmp,'qfrocxy'     ,qfrocxy      )
 call mpas_pool_get_array(output_noahmp,'qevacxy'     ,qevacxy      )
 call mpas_pool_get_array(output_noahmp,'qdewcxy'     ,qdewcxy      )
 call mpas_pool_get_array(output_noahmp,'qfrzcxy'     ,qfrzcxy      )
 call mpas_pool_get_array(output_noahmp,'qmeltcxy'    ,qmeltcxy     )
 call mpas_pool_get_array(output_noahmp,'qsnbotxy'    ,qsnbotxy     )
 call mpas_pool_get_array(output_noahmp,'pondingxy'   ,pondingxy    )
 call mpas_pool_get_array(output_noahmp,'fpicexy'     ,fpicexy      )
 call mpas_pool_get_array(output_noahmp,'rainlsm'     ,rainlsm      )
 call mpas_pool_get_array(output_noahmp,'snowlsm'     ,snowlsm      )
 call mpas_pool_get_array(output_noahmp,'forctlsm'    ,forctlsm     )
 call mpas_pool_get_array(output_noahmp,'forcqlsm'    ,forcqlsm     )
 call mpas_pool_get_array(output_noahmp,'forcplsm'    ,forcplsm     )
 call mpas_pool_get_array(output_noahmp,'forczlsm'    ,forczlsm     )
 call mpas_pool_get_array(output_noahmp,'forcwlsm'    ,forcwlsm     )
 call mpas_pool_get_array(output_noahmp,'eflxbxy'     ,eflxbxy      )
 call mpas_pool_get_array(output_noahmp,'soilenergy'  ,soilenergy   )
 call mpas_pool_get_array(output_noahmp,'snowenergy'  ,snowenergy   )
 call mpas_pool_get_array(output_noahmp,'canhsxy'     ,canhsxy      )

 do i = its,ite
    mpas_noahmp%pahxy(i)        = pahxy(i)
    mpas_noahmp%pahgxy(i)       = pahgxy(i)
    mpas_noahmp%pahbxy(i)       = pahbxy(i)
    mpas_noahmp%pahvxy(i)       = pahvxy(i)
    mpas_noahmp%qintsxy(i)      = qintsxy(i)
    mpas_noahmp%qintrxy(i)      = qintrxy(i)
    mpas_noahmp%qdripsxy(i)     = qdripsxy(i)
    mpas_noahmp%qdriprxy(i)     = qdriprxy(i)
    mpas_noahmp%qthrosxy(i)     = qthrosxy(i)
    mpas_noahmp%qthrorxy(i)     = qthrorxy(i)
    mpas_noahmp%qsnsubxy(i)     = qsnsubxy(i)
    mpas_noahmp%qmeltxy(i)      = qmeltxy(i)
    mpas_noahmp%qsnfroxy(i)     = qsnfroxy(i)
    mpas_noahmp%qsubcxy(i)      = qsubcxy(i)
    mpas_noahmp%qfrocxy(i)      = qfrocxy(i)
    mpas_noahmp%qevacxy(i)      = qevacxy(i)
    mpas_noahmp%qdewcxy(i)      = qdewcxy(i)
    mpas_noahmp%qfrzcxy(i)      = qfrzcxy(i)
    mpas_noahmp%qmeltcxy(i)     = qmeltcxy(i)
    mpas_noahmp%qsnbotxy(i)     = qsnbotxy(i)
    mpas_noahmp%pondingxy(i)    = pondingxy(i)
    mpas_noahmp%fpicexy(i)      = fpicexy(i)
    mpas_noahmp%rainlsm(i)      = rainlsm(i)
    mpas_noahmp%snowlsm(i)      = snowlsm(i)
    mpas_noahmp%forctlsm(i)     = forctlsm(i)
    mpas_noahmp%forcqlsm(i)     = forcqlsm(i)
    mpas_noahmp%forcplsm(i)     = forcplsm(i)
    mpas_noahmp%forczlsm(i)     = forczlsm(i)
    mpas_noahmp%forcwlsm(i)     = forcwlsm(i)
    mpas_noahmp%eflxbxy(i)      = eflxbxy(i)
    mpas_noahmp%soilenergy(i)   = soilenergy(i)
    mpas_noahmp%snowenergy(i)   = snowenergy(i)
    mpas_noahmp%canhsxy(i)      = canhsxy(i)
 enddo

!call mpas_log_write('--- end subroutine lsm_noahmp_fromMPAS.')

 end subroutine lsm_noahmp_fromMPAS

!=================================================================================================================
 subroutine lsm_noahmp_sounding_fromMPAS(mesh,state,time_lev,diag)
!=================================================================================================================

!--- input arguments:
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: diag
 type(mpas_pool_type),intent(in):: state

 integer,intent(in):: time_lev


!--- local variables and arrays:
 integer:: i,its,ite,k,kts,kte
 integer,pointer:: index_qv

 real(kind=RKIND),dimension(:,:),pointer:: zgrid
 real(kind=RKIND),dimension(:,:),pointer:: qv,theta_m,u,v
 real(kind=RKIND),dimension(:,:),pointer:: exner,pressure_b,pressure_p
 real(kind=RKIND),dimension(:,:,:),pointer:: scalars

 real(kind=RKIND):: fzm,fzp,mult,totm,totp
 real(kind=RKIND):: w1,w2,z0,z1,z2

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write('--- enter subroutine lsm_noahmp_sounding_fromMPAS: $i',intArgs=(/time_lev/))


!--- initialization of local dimensions:
 its   = mpas_noahmp%its
 ite   = mpas_noahmp%ite
 kts   = mpas_noahmp%kts
 kte   = mpas_noahmp%kte


!--- initialization of input sounding variables:
 call mpas_pool_get_array(mesh,'zgrid',zgrid)

 call mpas_pool_get_array(diag,'exner'                 ,exner     )
 call mpas_pool_get_array(diag,'pressure_base'         ,pressure_b)
 call mpas_pool_get_array(diag,'pressure_p'            ,pressure_p)
 call mpas_pool_get_array(diag,'uReconstructZonal'     ,u         )
 call mpas_pool_get_array(diag,'uReconstructMeridional',v         )
 
 call mpas_pool_get_array(state,'theta_m',theta_m,time_lev)
 call mpas_pool_get_array(state,'scalars',scalars,time_lev)

 call mpas_pool_get_dimension(state,'index_qv',index_qv)
 qv => scalars(index_qv,:,:)

 do i = its,ite
    do k = kts,kte
       mpas_noahmp%dz8w(i,k)    = zgrid(k+1,i)-zgrid(k,i)
       mpas_noahmp%qv_curr(i,k) = qv(k,i)
       mpas_noahmp%t_phy(i,k)   = (theta_m(k,i)/(1.+R_v/R_d*qv(k,i)))*exner(k,i)
       mpas_noahmp%u_phy(i,k)   = u(k,i)
       mpas_noahmp%v_phy(i,k)   = v(k,i)
    enddo
 enddo


!--- initialization of pressure at interface between layers:
 do i = its,ite
    k = kts
    z0 = zgrid(k,i)
    z1 = 0.5*(zgrid(k,i)+zgrid(k+1,i))
    z2 = 0.5*(zgrid(k+1,i)+zgrid(k+2,i))
    w1 = (z0-z2)/(z1-z2)
    w2 = 1.-w1
    totm = pressure_p(k,i)+pressure_b(k,i)
    totp = pressure_p(k+1,i)+pressure_b(k+1,i)
    mpas_noahmp%p8w(i,k) = w1*totm + w2*totp

    do k = kts+1,kte
       totm = pressure_p(k-1,i)+pressure_b(k-1,i)
       totp = pressure_p(k,i)+pressure_b(k,i)
       mult = 1./(zgrid(k+1,i)-zgrid(k-1,i))
       fzm  = mult*(zgrid(k,i)-zgrid(k-1,i))
       fzp  = mult*(zgrid(k+1,i)-zgrid(k,i))
       mpas_noahmp%p8w(i,k) = fzm*totp + fzp*totm
    enddo
 enddo

!call mpas_log_write('--- end subroutine lsm_noahmp_sounding_fromMPAS:')

 end subroutine lsm_noahmp_sounding_fromMPAS

!=================================================================================================================
 subroutine lsm_noahmp_toMPAS(diag_physics,diag_physics_noahmp,output_noahmp,sfc_input)
!=================================================================================================================

!--- input arguments:
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: diag_physics_noahmp
 type(mpas_pool_type),intent(inout):: output_noahmp
 type(mpas_pool_type),intent(inout):: sfc_input


!--- local variables and arrays:
 integer:: i,its,ite
 integer:: n,ns,nsoil,nsnow,nzsnow


!--- local INOUT pointers (with generic LSM equivalent as defined in WRF):
 real(kind=RKIND),dimension(:),pointer:: acsnom,acsnow,canwat,hfx,qfx,qsfc,lh,grdflx,sfc_albedo,sfc_emiss,  &
                                         sfcrunoff,skintemp,smstav,smstot,udrunoff,snow,snowc,snowh,lai,z0, &
                                         znt
 real(kind=RKIND),dimension(:,:),pointer:: sh2o,smois,tslb,smoiseq


 !--- local INOUT pointers (with no Noah LSM equivalent as defined in WRF):
 integer,dimension(:),pointer:: isnowxy
 integer,dimension(:),pointer:: irnumsi,irnummi,irnumfi,pgsxy
 real(kind=RKIND),dimension(:),pointer:: tvxy,tgxy,canicexy,canliqxy,eahxy,tahxy,cmxy,chxy,fwetxy,sneqvoxy, &
                                         alboldxy,qsnowxy,qrainxy,wslakexy,zwtxy,waxy,wtxy,                 &
                                         lfmassxy,rtmassxy,stmassxy,woodxy,grainxy,gddxy,stblcpxy,          &
                                         fastcpxy,xsaixy,taussxy
 real(kind=RKIND),dimension(:,:),pointer:: tsnoxy,zsnsoxy,snicexy,snliqxy
 real(kind=RKIND),dimension(:),pointer:: irwatsi,irsivol,ireloss,irrsplh,irwatmi,irmivol,irwatfi,irfivol
 real(kind=RKIND),dimension(:),pointer:: smcwtdxy,deeprechxy,rechxy,qrfsxy,qspringsxy,qslatxy, &
                                         rivercondxy,pexpxy
 real(kind=RKIND),dimension(:),pointer:: fsatxy,wsurfxy
 real(kind=RKIND),dimension(:),pointer:: acc_ssoilxy,acc_qinsurxy,acc_qsevaxy,acc_dwaterxy,acc_prcpxy,      &
                                         acc_ecanxy,acc_etranxy,acc_edirxy,acc_glaflwxy
 real(kind=RKIND),dimension(:,:),pointer:: acc_etranixy
 real(kind=RKIND),dimension(:,:),pointer:: albsoildirxy,albsoildifxy
 real(kind=RKIND),dimension(:,:),pointer:: snrdsxy,snfrxy,bcphixy,bcphoxy,ocphixy,ocphoxy,  &
                                           dust1xy,dust2xy,dust3xy,dust4xy,dust5xy,         &
                                           concbcphixy,concbcphoxy,concocphixy,concocphoxy, &
                                           concdust1xy,concdust2xy,concdust3xy,concdust4xy,concdust5xy


!--- local OUT pointers (with no Noah LSM equivalent as defined in WRF):
 real(kind=RKIND),dimension(:),pointer:: t2mvxy,t2mbxy,t2mxy,q2mvxy,q2mbxy,q2mxy,tradxy,neexy,gppxy,nppxy,  &
                                         fvegxy,runsfxy,runsbxy,ecanxy,edirxy,etranxy,fsaxy,firaxy,aparxy,  &
                                         psnxy,savxy,sagxy,rssunxy,rsshaxy,bgapxy,wgapxy,tgvxy,tgbxy,chvxy, &
                                         chbxy,shgxy,shcxy,shbxy,evgxy,evbxy,ghvxy,ghbxy,irgxy,ircxy,irbxy, &
                                         trxy,evcxy,chleafxy,chucxy,chv2xy,chb2xy,rs,qtdrain


!--- local OUT additional variables:
 real(kind=RKIND),dimension(:),pointer:: pahxy,pahgxy,pahbxy,pahvxy,qintsxy,qintrxy,qdripsxy,qdriprxy,         &
                                         qthrosxy,qthrorxy,qsnsubxy,qmeltxy,qsnfroxy,qsubcxy,qfrocxy,          &
                                         qevacxy,qdewcxy,qfrzcxy,qmeltcxy,qsnbotxy,pondingxy,fpicexy,          &
                                         rainlsm,snowlsm,forctlsm,forcqlsm,forcplsm,forczlsm,forcwlsm,         &
                                         eflxbxy,soilenergy,snowenergy,canhsxy

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write('--- enter subroutine lsm_noahmp_toMPAS:')


!--- initialization of local dimensions:
 its    = mpas_noahmp%its
 ite    = mpas_noahmp%ite
 nsoil  = mpas_noahmp%nsoil
 nsnow  = mpas_noahmp%nsnow
 nzsnow = nsnow + nsoil


!--- update of INOUT variables (with generic LSM equivalent as defined in WRF), i.e. see
!    lines 162-184 in module NoahmpIOVarType.F90):
 call mpas_pool_get_array(sfc_input,'skintemp',skintemp)
 call mpas_pool_get_array(sfc_input,'snowc'   ,snowc   )
 call mpas_pool_get_array(sfc_input,'snow'    ,snow    )
 call mpas_pool_get_array(sfc_input,'snowh'   ,snowh   )
 call mpas_pool_get_array(sfc_input,'sh2o'    ,sh2o    )
 call mpas_pool_get_array(sfc_input,'smois'   ,smois   )
 call mpas_pool_get_array(sfc_input,'tslb'    ,tslb    )

 call mpas_pool_get_array(diag_physics,'hfx'       ,hfx       )
 call mpas_pool_get_array(diag_physics,'qfx'       ,qfx       )
 call mpas_pool_get_array(diag_physics,'lh '       ,lh        )
 call mpas_pool_get_array(diag_physics,'grdflx'    ,grdflx    )
 call mpas_pool_get_array(diag_physics,'smstav'    ,smstav    )
 call mpas_pool_get_array(diag_physics,'smstot'    ,smstot    )
 call mpas_pool_get_array(diag_physics,'sfcrunoff' ,sfcrunoff )
 call mpas_pool_get_array(diag_physics,'udrunoff'  ,udrunoff  )
 call mpas_pool_get_array(diag_physics,'sfc_albedo',sfc_albedo)
 call mpas_pool_get_array(diag_physics,'canwat'    ,canwat    )
 call mpas_pool_get_array(diag_physics,'acsnom'    ,acsnom    )
 call mpas_pool_get_array(diag_physics,'acsnow'    ,acsnow    )
 call mpas_pool_get_array(diag_physics,'sfc_emiss' ,sfc_emiss )
 call mpas_pool_get_array(diag_physics,'qsfc'      ,qsfc      )
 call mpas_pool_get_array(diag_physics,'lai'       ,lai       )
 call mpas_pool_get_array(diag_physics,'z0'        ,z0        )
 call mpas_pool_get_array(diag_physics,'znt'       ,znt       )


 do i = its,ite
    skintemp(i)   = mpas_noahmp%tsk(i)
    hfx(i)        = mpas_noahmp%hfx(i)
    qfx(i)        = mpas_noahmp%qfx(i)
    lh(i)         = mpas_noahmp%lh(i)
    grdflx(i)     = mpas_noahmp%grdflx(i)
    smstav(i)     = mpas_noahmp%smstav(i)
    smstot(i)     = mpas_noahmp%smstot(i)
    sfcrunoff(i)  = mpas_noahmp%sfcrunoff(i)
    udrunoff(i)   = mpas_noahmp%udrunoff(i)
    sfc_albedo(i) = mpas_noahmp%albedo(i)
    snowc(i)      = mpas_noahmp%snowc(i)
    snow(i)       = mpas_noahmp%snow(i)
    snowh(i)      = mpas_noahmp%snowh(i)
    canwat(i)     = mpas_noahmp%canwat(i)
    acsnom(i)     = mpas_noahmp%acsnom(i)
    acsnow(i)     = mpas_noahmp%acsnow(i)
    sfc_emiss(i)  = mpas_noahmp%emiss(i)
    qsfc(i)       = mpas_noahmp%qsfc(i)
    lai(i)        = mpas_noahmp%lai(i)
    z0(i)         = mpas_noahmp%z0(i)
    znt(i)        = mpas_noahmp%znt(i)
 enddo

 do ns = 1,nsoil
    do i = its,ite
       sh2o(ns,i)    = mpas_noahmp%sh2o(i,ns)
       smois(ns,i)   = mpas_noahmp%smois(i,ns)
       tslb(ns,i)    = mpas_noahmp%tslb(i,ns)
    enddo
 enddo


!--- update of INOUT variables (with no Noah LSM equivalent as defined in WRF), i.e. see
!    lines 186-222 in module NoahmpIOVarType.F90:
 call mpas_pool_get_array(diag_physics_noahmp,'isnowxy'     ,isnowxy     )
 call mpas_pool_get_array(diag_physics_noahmp,'tvxy'        ,tvxy        )
 call mpas_pool_get_array(diag_physics_noahmp,'tgxy'        ,tgxy        )
 call mpas_pool_get_array(diag_physics_noahmp,'canicexy'    ,canicexy    )
 call mpas_pool_get_array(diag_physics_noahmp,'canliqxy'    ,canliqxy    )
 call mpas_pool_get_array(diag_physics_noahmp,'eahxy'       ,eahxy       )
 call mpas_pool_get_array(diag_physics_noahmp,'tahxy'       ,tahxy       )
 call mpas_pool_get_array(diag_physics_noahmp,'cmxy'        ,cmxy        )
 call mpas_pool_get_array(diag_physics_noahmp,'chxy'        ,chxy        )
 call mpas_pool_get_array(diag_physics_noahmp,'fwetxy'      ,fwetxy      )
 call mpas_pool_get_array(diag_physics_noahmp,'sneqvoxy'    ,sneqvoxy    )
 call mpas_pool_get_array(diag_physics_noahmp,'alboldxy'    ,alboldxy    )
 call mpas_pool_get_array(diag_physics_noahmp,'wslakexy'    ,wslakexy    )
 call mpas_pool_get_array(diag_physics_noahmp,'zwtxy'       ,zwtxy       )
 call mpas_pool_get_array(diag_physics_noahmp,'waxy'        ,waxy        )
 call mpas_pool_get_array(diag_physics_noahmp,'wtxy'        ,wtxy        )
 call mpas_pool_get_array(diag_physics_noahmp,'deeprechxy'  ,deeprechxy  )
 call mpas_pool_get_array(diag_physics_noahmp,'rechxy'      ,rechxy      )
 call mpas_pool_get_array(diag_physics_noahmp,'tsnoxy'      ,tsnoxy      )
 call mpas_pool_get_array(diag_physics_noahmp,'zsnsoxy'     ,zsnsoxy     )
 call mpas_pool_get_array(diag_physics_noahmp,'snicexy'     ,snicexy     )
 call mpas_pool_get_array(diag_physics_noahmp,'snliqxy'     ,snliqxy     )
 call mpas_pool_get_array(diag_physics_noahmp,'lfmassxy'    ,lfmassxy    )
 call mpas_pool_get_array(diag_physics_noahmp,'rtmassxy'    ,rtmassxy    )
 call mpas_pool_get_array(diag_physics_noahmp,'stmassxy'    ,stmassxy    )
 call mpas_pool_get_array(diag_physics_noahmp,'woodxy'      ,woodxy      )
 call mpas_pool_get_array(diag_physics_noahmp,'stblcpxy'    ,stblcpxy    )
 call mpas_pool_get_array(diag_physics_noahmp,'fastcpxy'    ,fastcpxy    )
 call mpas_pool_get_array(diag_physics_noahmp,'xsaixy'      ,xsaixy      )
 call mpas_pool_get_array(diag_physics_noahmp,'taussxy'     ,taussxy     )
 call mpas_pool_get_array(diag_physics_noahmp,'acc_dwaterxy',acc_dwaterxy)
 call mpas_pool_get_array(diag_physics_noahmp,'acc_prcpxy'  ,acc_prcpxy  )
 call mpas_pool_get_array(diag_physics_noahmp,'acc_ecanxy'  ,acc_ecanxy  )
 call mpas_pool_get_array(diag_physics_noahmp,'acc_etranxy' ,acc_etranxy )
 call mpas_pool_get_array(diag_physics_noahmp,'acc_edirxy'  ,acc_edirxy  )
 call mpas_pool_get_array(diag_physics_noahmp,'acc_etranixy',acc_etranixy)
 call mpas_pool_get_array(diag_physics_noahmp,'acc_glaflwxy',acc_glaflwxy)
 call mpas_pool_get_array(diag_physics_noahmp,'acc_ssoilxy' ,acc_ssoilxy )
 call mpas_pool_get_array(diag_physics_noahmp,'acc_qinsurxy',acc_qinsurxy)
 call mpas_pool_get_array(diag_physics_noahmp,'acc_qsevaxy' ,acc_qsevaxy )
 call mpas_pool_get_array(diag_physics_noahmp,'albsoildirxy',albsoildirxy)
 call mpas_pool_get_array(diag_physics_noahmp,'albsoildifxy',albsoildifxy)

 do i = its,ite
    isnowxy(i)      = mpas_noahmp%isnowxy(i)
    tvxy(i)         = mpas_noahmp%tvxy(i)
    tgxy(i)         = mpas_noahmp%tgxy(i)
    canicexy(i)     = mpas_noahmp%canicexy(i)
    canliqxy(i)     = mpas_noahmp%canliqxy(i)
    eahxy(i)        = mpas_noahmp%eahxy(i)
    tahxy(i)        = mpas_noahmp%tahxy(i)
    cmxy(i)         = mpas_noahmp%cmxy(i)
    chxy(i)         = mpas_noahmp%chxy(i)
    fwetxy(i)       = mpas_noahmp%fwetxy(i)
    sneqvoxy(i)     = mpas_noahmp%sneqvoxy(i)
    alboldxy(i)     = mpas_noahmp%alboldxy(i)
    wslakexy(i)     = mpas_noahmp%wslakexy(i)
    zwtxy(i)        = mpas_noahmp%zwtxy(i)
    waxy(i)         = mpas_noahmp%waxy(i)
    wtxy(i)         = mpas_noahmp%wtxy(i)
    deeprechxy(i)   = mpas_noahmp%deeprechxy(i)
    rechxy(i)       = mpas_noahmp%rechxy(i)
    lfmassxy(i)     = mpas_noahmp%lfmassxy(i)
    rtmassxy(i)     = mpas_noahmp%rtmassxy(i)
    stmassxy(i)     = mpas_noahmp%stmassxy(i)
    woodxy(i)       = mpas_noahmp%woodxy(i)
    stblcpxy(i)     = mpas_noahmp%stblcpxy(i)
    fastcpxy(i)     = mpas_noahmp%fastcpxy(i)
    xsaixy(i)       = mpas_noahmp%xsaixy(i)
    taussxy(i)      = mpas_noahmp%taussxy(i)
    acc_dwaterxy(i) = mpas_noahmp%acc_dwaterxy(i)
    acc_prcpxy(i)   = mpas_noahmp%acc_prcpxy(i)
    acc_ecanxy(i)   = mpas_noahmp%acc_ecanxy(i)
    acc_etranxy(i)  = mpas_noahmp%acc_etranxy(i)
    acc_edirxy(i)   = mpas_noahmp%acc_edirxy(i)
    acc_ssoilxy(i)  = mpas_noahmp%acc_ssoilxy(i)
    acc_qinsurxy(i) = mpas_noahmp%acc_qinsurxy(i)
    acc_qsevaxy(i)  = mpas_noahmp%acc_qsevaxy(i)
    acc_glaflwxy(i) = mpas_noahmp%acc_glaflwxy(i)
    do n = 1,mpas_noahmp%numrad
       albsoildirxy(n,i) = mpas_noahmp%albsoildirxy(i,n)
       albsoildifxy(n,i) = mpas_noahmp%albsoildifxy(i,n)
    enddo

    do ns = 1,nsoil
       acc_etranixy(ns,i) = mpas_noahmp%acc_etranixy(i,ns)
    enddo

    do ns = 1,nsnow
       n = ns - nsnow
       tsnoxy(ns,i)  = mpas_noahmp%tsnoxy(i,n)
       snicexy(ns,i) = mpas_noahmp%snicexy(i,n)
       snliqxy(ns,i) = mpas_noahmp%snliqxy(i,n)
    enddo

    do ns = 1,nzsnow
       n = ns - nsnow
       zsnsoxy(ns,i) = mpas_noahmp%zsnsoxy(i,n)
    enddo
 enddo

 if (mpas_noahmp%iopt_crop > 0) then
    call mpas_pool_get_array(diag_physics_noahmp,'gddxy'  ,gddxy  )
    call mpas_pool_get_array(diag_physics_noahmp,'grainxy',grainxy)
    call mpas_pool_get_array(diag_physics_noahmp,'pgsxy'  ,pgsxy  )

    do i = its,ite
       grainxy(i) = mpas_noahmp%grainxy(i)
       gddxy(i)   = mpas_noahmp%gddxy(i)
       pgsxy(i)   = mpas_noahmp%pgsxy(i)
    enddo
 endif

 if (mpas_noahmp%iopt_tdrn > 0) then
    call mpas_pool_get_array(diag_physics_noahmp,'qtdrain',qtdrain)

    do i = its,ite
       qtdrain(i) = mpas_noahmp%qtdrain(i)
    enddo
 endif

 if (mpas_noahmp%iopt_irr > 0) then
    call mpas_pool_get_array(diag_physics_noahmp,'irnumsi',irnumsi)
    call mpas_pool_get_array(diag_physics_noahmp,'irsivol',irsivol)
    call mpas_pool_get_array(diag_physics_noahmp,'irwatsi',irwatsi)
    call mpas_pool_get_array(diag_physics_noahmp,'ireloss',ireloss)
    call mpas_pool_get_array(diag_physics_noahmp,'irrsplh',irrsplh)
    call mpas_pool_get_array(diag_physics_noahmp,'irnummi',irnummi)
    call mpas_pool_get_array(diag_physics_noahmp,'irwatmi',irwatmi)
    call mpas_pool_get_array(diag_physics_noahmp,'irmivol',irmivol)
    call mpas_pool_get_array(diag_physics_noahmp,'irnumfi',irnumfi)
    call mpas_pool_get_array(diag_physics_noahmp,'irwatfi',irwatfi)
    call mpas_pool_get_array(diag_physics_noahmp,'irfivol',irfivol)

    do i = its,ite
       irnumsi(i) = mpas_noahmp%irnumsi(i)
       irwatsi(i) = mpas_noahmp%irwatsi(i)
       irsivol(i) = mpas_noahmp%irsivol(i)
       ireloss(i) = mpas_noahmp%ireloss(i)
       irrsplh(i) = mpas_noahmp%irrsplh(i)
       irnummi(i) = mpas_noahmp%irnummi(i)
       irwatmi(i) = mpas_noahmp%irwatmi(i)
       irmivol(i) = mpas_noahmp%irmivol(i)
       irnumfi(i) = mpas_noahmp%irnumfi(i)
       irwatfi(i) = mpas_noahmp%irwatfi(i)
       irfivol(i) = mpas_noahmp%irfivol(i)
    enddo
 endif

 if (mpas_noahmp%iopt_runsub == 5) then
    call mpas_pool_get_array(diag_physics_noahmp,'smcwtdxy'   ,smcwtdxy   )
    call mpas_pool_get_array(diag_physics_noahmp,'deeprechxy' ,deeprechxy )
    call mpas_pool_get_array(diag_physics_noahmp,'rechxy'     ,rechxy     )
    call mpas_pool_get_array(diag_physics_noahmp,'qrfsxy'     ,qrfsxy     )
    call mpas_pool_get_array(diag_physics_noahmp,'qspringsxy' ,qspringsxy )
    call mpas_pool_get_array(diag_physics_noahmp,'qslatxy'    ,qslatxy    )
    call mpas_pool_get_array(diag_physics_noahmp,'rivercondxy',rivercondxy)
    call mpas_pool_get_array(diag_physics_noahmp,'pexpxy'     ,pexpxy     )
    call mpas_pool_get_array(diag_physics_noahmp,'smoiseq'    ,smoiseq    )

    do i = its,ite
       smcwtdxy(i)    = mpas_noahmp%smcwtdxy(i)
       deeprechxy(i)  = mpas_noahmp%deeprechxy(i)
       rechxy(i)      = mpas_noahmp%rechxy(i)
       qrfsxy(i)      = mpas_noahmp%qrfsxy(i)
       qspringsxy(i)  = mpas_noahmp%qspringsxy(i)
       qslatxy(i)     = mpas_noahmp%qslatxy(i)
       rivercondxy(i) = mpas_noahmp%rivercondxy(i)
       pexpxy(i)      = mpas_noahmp%pexpxy(i)
       do ns = 1,nsoil
          smoiseq(ns,i) = mpas_noahmp%smoiseq(i,ns)
       enddo
    enddo
 endif

if (mpas_noahmp%iopt_wetland > 0) then
    call mpas_pool_get_array(diag_physics_noahmp,'fsatxy' ,fsatxy )
    call mpas_pool_get_array(diag_physics_noahmp,'wsurfxy',wsurfxy)

    do i = its,ite
       fsatxy(i)  = mpas_noahmp%fsatxy(i) 
       wsurfxy(i) = mpas_noahmp%wsurfxy(i)
    enddo
 endif

 if (mpas_noahmp%iopt_alb == 3) then
    call mpas_pool_get_array(diag_physics_noahmp,'snrdsxy'    ,snrdsxy    )
    call mpas_pool_get_array(diag_physics_noahmp,'snfrxy'     ,snfrxy     )
    call mpas_pool_get_array(diag_physics_noahmp,'bcphixy'    ,bcphixy    )
    call mpas_pool_get_array(diag_physics_noahmp,'bcphoxy'    ,bcphoxy    )
    call mpas_pool_get_array(diag_physics_noahmp,'ocphixy'    ,ocphixy    )
    call mpas_pool_get_array(diag_physics_noahmp,'ocphoxy'    ,ocphoxy    )
    call mpas_pool_get_array(diag_physics_noahmp,'dust1xy'    ,dust1xy    )
    call mpas_pool_get_array(diag_physics_noahmp,'dust2xy'    ,dust2xy    )
    call mpas_pool_get_array(diag_physics_noahmp,'dust3xy'    ,dust3xy    )
    call mpas_pool_get_array(diag_physics_noahmp,'dust4xy'    ,dust4xy    )
    call mpas_pool_get_array(diag_physics_noahmp,'dust5xy'    ,dust5xy    )
    call mpas_pool_get_array(diag_physics_noahmp,'concbcphixy',concbcphixy)
    call mpas_pool_get_array(diag_physics_noahmp,'concbcphoxy',concbcphoxy)
    call mpas_pool_get_array(diag_physics_noahmp,'concocphixy',concocphixy)
    call mpas_pool_get_array(diag_physics_noahmp,'concocphoxy',concocphoxy)
    call mpas_pool_get_array(diag_physics_noahmp,'concdust1xy',concdust1xy)
    call mpas_pool_get_array(diag_physics_noahmp,'concdust2xy',concdust2xy)
    call mpas_pool_get_array(diag_physics_noahmp,'concdust3xy',concdust3xy)
    call mpas_pool_get_array(diag_physics_noahmp,'concdust4xy',concdust4xy)
    call mpas_pool_get_array(diag_physics_noahmp,'concdust5xy',concdust5xy)

    do ns = 1,nsnow
       n = ns - nsnow
       do i = its,ite
          snrdsxy(ns,i)     = mpas_noahmp%snrdsxy(i,n)
          snfrxy(ns,i)      = mpas_noahmp%snfrxy(i,n)
          bcphixy(ns,i)     = mpas_noahmp%bcphixy(i,n)
          bcphoxy(ns,i)     = mpas_noahmp%bcphoxy(i,n)
          ocphixy(ns,i)     = mpas_noahmp%ocphixy(i,n)
          ocphoxy(ns,i)     = mpas_noahmp%ocphoxy(i,n)
          dust1xy(ns,i)     = mpas_noahmp%dust1xy(i,n)
          dust2xy(ns,i)     = mpas_noahmp%dust2xy(i,n)
          dust3xy(ns,i)     = mpas_noahmp%dust3xy(i,n)
          dust4xy(ns,i)     = mpas_noahmp%dust4xy(i,n)
          dust5xy(ns,i)     = mpas_noahmp%dust5xy(i,n)
          concbcphixy(ns,i) = mpas_noahmp%massconcbcphixy(i,n)
          concbcphoxy(ns,i) = mpas_noahmp%massconcbcphoxy(i,n)
          concocphixy(ns,i) = mpas_noahmp%massconcocphixy(i,n)
          concocphoxy(ns,i) = mpas_noahmp%massconcocphoxy(i,n)
          concdust1xy(ns,i) = mpas_noahmp%massconcdust1xy(i,n)
          concdust2xy(ns,i) = mpas_noahmp%massconcdust2xy(i,n)
          concdust3xy(ns,i) = mpas_noahmp%massconcdust3xy(i,n)
          concdust4xy(ns,i) = mpas_noahmp%massconcdust4xy(i,n)
          concdust5xy(ns,i) = mpas_noahmp%massconcdust5xy(i,n)
       enddo
    enddo
 endif


!--- update of OUT (with no Noah LSM equivalent as defined in WRF), i.e. see
!    lines 242-290 in module NoahmpIOVarType.F90:
 call mpas_pool_get_array(output_noahmp,'qsnowxy' ,qsnowxy )
 call mpas_pool_get_array(output_noahmp,'qrainxy' ,qrainxy )
 call mpas_pool_get_array(output_noahmp,'t2mvxy'  ,t2mvxy  )
 call mpas_pool_get_array(output_noahmp,'t2mbxy'  ,t2mbxy  )
 call mpas_pool_get_array(output_noahmp,'t2mxy'   ,t2mxy   )
 call mpas_pool_get_array(output_noahmp,'q2mvxy'  ,q2mvxy  )
 call mpas_pool_get_array(output_noahmp,'q2mbxy'  ,q2mbxy  )
 call mpas_pool_get_array(output_noahmp,'q2mxy'   ,q2mxy   )
 call mpas_pool_get_array(output_noahmp,'tradxy'  ,tradxy  )
 call mpas_pool_get_array(output_noahmp,'neexy'   ,neexy   )
 call mpas_pool_get_array(output_noahmp,'gppxy'   ,gppxy   )
 call mpas_pool_get_array(output_noahmp,'nppxy'   ,nppxy   )
 call mpas_pool_get_array(output_noahmp,'fvegxy'  ,fvegxy  )
 call mpas_pool_get_array(output_noahmp,'runsfxy' ,runsfxy )
 call mpas_pool_get_array(output_noahmp,'runsbxy' ,runsbxy )
 call mpas_pool_get_array(output_noahmp,'ecanxy'  ,ecanxy  )
 call mpas_pool_get_array(output_noahmp,'edirxy'  ,edirxy  )
 call mpas_pool_get_array(output_noahmp,'etranxy' ,etranxy )
 call mpas_pool_get_array(output_noahmp,'fsaxy'   ,fsaxy   )
 call mpas_pool_get_array(output_noahmp,'firaxy'  ,firaxy  )
 call mpas_pool_get_array(output_noahmp,'aparxy'  ,aparxy  )
 call mpas_pool_get_array(output_noahmp,'psnxy'   ,psnxy   )
 call mpas_pool_get_array(output_noahmp,'savxy'   ,savxy   )
 call mpas_pool_get_array(output_noahmp,'sagxy'   ,sagxy   )
 call mpas_pool_get_array(output_noahmp,'rssunxy' ,rssunxy )
 call mpas_pool_get_array(output_noahmp,'rsshaxy' ,rsshaxy )
 call mpas_pool_get_array(output_noahmp,'bgapxy'  ,bgapxy  )
 call mpas_pool_get_array(output_noahmp,'wgapxy'  ,wgapxy  )
 call mpas_pool_get_array(output_noahmp,'tgvxy'   ,tgvxy   )
 call mpas_pool_get_array(output_noahmp,'tgbxy'   ,tgbxy   )
 call mpas_pool_get_array(output_noahmp,'chvxy'   ,chvxy   )
 call mpas_pool_get_array(output_noahmp,'chbxy'   ,chbxy   )
 call mpas_pool_get_array(output_noahmp,'shgxy'   ,shgxy   )
 call mpas_pool_get_array(output_noahmp,'shcxy'   ,shcxy   )
 call mpas_pool_get_array(output_noahmp,'shbxy'   ,shbxy   )
 call mpas_pool_get_array(output_noahmp,'evgxy'   ,evgxy   )
 call mpas_pool_get_array(output_noahmp,'evbxy'   ,evbxy   )
 call mpas_pool_get_array(output_noahmp,'ghvxy'   ,ghvxy   )
 call mpas_pool_get_array(output_noahmp,'ghbxy'   ,ghbxy   )
 call mpas_pool_get_array(output_noahmp,'irgxy'   ,irgxy   )
 call mpas_pool_get_array(output_noahmp,'ircxy'   ,ircxy   )
 call mpas_pool_get_array(output_noahmp,'irbxy'   ,irbxy   )
 call mpas_pool_get_array(output_noahmp,'trxy'    ,trxy    )
 call mpas_pool_get_array(output_noahmp,'evcxy'   ,evcxy   )
 call mpas_pool_get_array(output_noahmp,'chleafxy',chleafxy)
 call mpas_pool_get_array(output_noahmp,'chucxy'  ,chucxy  )
 call mpas_pool_get_array(output_noahmp,'chv2xy'  ,chv2xy  )
 call mpas_pool_get_array(output_noahmp,'chb2xy'  ,chb2xy  )
 call mpas_pool_get_array(output_noahmp,'rs'      ,rs      )

 do i = its,ite
    qsnowxy(i)  = mpas_noahmp%qsnowxy(i)
    qrainxy(i)  = mpas_noahmp%qrainxy(i)
    t2mvxy(i)   = mpas_noahmp%t2mvxy(i)
    t2mbxy(i)   = mpas_noahmp%t2mbxy(i)
    t2mxy(i)    = mpas_noahmp%t2mxy(i)
    q2mvxy(i)   = mpas_noahmp%q2mvxy(i)
    q2mbxy(i)   = mpas_noahmp%q2mbxy(i)
    q2mxy(i)    = mpas_noahmp%q2mxy(i)
    tradxy(i)   = mpas_noahmp%tradxy(i)
    neexy(i)    = mpas_noahmp%neexy(i)
    gppxy(i)    = mpas_noahmp%gppxy(i)
    nppxy(i)    = mpas_noahmp%nppxy(i)
    fvegxy(i)   = mpas_noahmp%fvegxy(i)
    runsfxy(i)  = mpas_noahmp%runsfxy(i)
    runsbxy(i)  = mpas_noahmp%runsbxy(i)
    ecanxy(i)   = mpas_noahmp%ecanxy(i)
    edirxy(i)   = mpas_noahmp%edirxy(i)
    etranxy(i)  = mpas_noahmp%etranxy(i)
    fsaxy(i)    = mpas_noahmp%fsaxy(i)
    firaxy(i)   = mpas_noahmp%firaxy(i)
    aparxy(i)   = mpas_noahmp%aparxy(i)
    psnxy(i)    = mpas_noahmp%psnxy(i)
    savxy(i)    = mpas_noahmp%savxy(i)
    sagxy(i)    = mpas_noahmp%sagxy(i)
    rssunxy(i)  = mpas_noahmp%rssunxy(i)
    rsshaxy(i)  = mpas_noahmp%rsshaxy(i)
    bgapxy(i)   = mpas_noahmp%bgapxy(i)
    wgapxy(i)   = mpas_noahmp%wgapxy(i)
    tgvxy(i)    = mpas_noahmp%tgvxy(i)
    tgbxy(i)    = mpas_noahmp%tgbxy(i)
    chvxy(i)    = mpas_noahmp%chvxy(i)
    chbxy(i)    = mpas_noahmp%chbxy(i)
    shgxy(i)    = mpas_noahmp%shgxy(i)
    shcxy(i)    = mpas_noahmp%shcxy(i)
    shbxy(i)    = mpas_noahmp%shbxy(i)
    evgxy(i)    = mpas_noahmp%evgxy(i)
    evbxy(i)    = mpas_noahmp%evbxy(i)
    ghvxy(i)    = mpas_noahmp%ghvxy(i)
    ghbxy(i)    = mpas_noahmp%ghbxy(i)
    irgxy(i)    = mpas_noahmp%irgxy(i)
    ircxy(i)    = mpas_noahmp%ircxy(i)
    irbxy(i)    = mpas_noahmp%irbxy(i)
    trxy(i)     = mpas_noahmp%trxy(i)
    evcxy(i)    = mpas_noahmp%evcxy(i)
    chleafxy(i) = mpas_noahmp%chleafxy(i)
    chucxy(i)   = mpas_noahmp%chucxy(i)
    chv2xy(i)   = mpas_noahmp%chv2xy(i)
    chb2xy(i)   = mpas_noahmp%chb2xy(i)
    rs(i)       = mpas_noahmp%rs(i)
 enddo


!--- update of OUT additional variables, i.e. see lines 292-334 in module NoahmpIOVarType.F90:
 call mpas_pool_get_array(output_noahmp,'pahxy'       ,pahxy        )
 call mpas_pool_get_array(output_noahmp,'pahgxy'      ,pahgxy       )
 call mpas_pool_get_array(output_noahmp,'pahbxy'      ,pahbxy       )
 call mpas_pool_get_array(output_noahmp,'pahvxy'      ,pahvxy       )
 call mpas_pool_get_array(output_noahmp,'qintsxy'     ,qintsxy      )
 call mpas_pool_get_array(output_noahmp,'qintrxy'     ,qintrxy      )
 call mpas_pool_get_array(output_noahmp,'qdripsxy'    ,qdripsxy     )
 call mpas_pool_get_array(output_noahmp,'qdriprxy'    ,qdriprxy     )
 call mpas_pool_get_array(output_noahmp,'qthrosxy'    ,qthrosxy     )
 call mpas_pool_get_array(output_noahmp,'qthrorxy'    ,qthrorxy     )
 call mpas_pool_get_array(output_noahmp,'qsnsubxy'    ,qsnsubxy     )
 call mpas_pool_get_array(output_noahmp,'qmeltxy'     ,qmeltxy      )
 call mpas_pool_get_array(output_noahmp,'qsnfroxy'    ,qsnfroxy     )
 call mpas_pool_get_array(output_noahmp,'qsubcxy'     ,qsubcxy      )
 call mpas_pool_get_array(output_noahmp,'qfrocxy'     ,qfrocxy      )
 call mpas_pool_get_array(output_noahmp,'qevacxy'     ,qevacxy      )
 call mpas_pool_get_array(output_noahmp,'qdewcxy'     ,qdewcxy      )
 call mpas_pool_get_array(output_noahmp,'qfrzcxy'     ,qfrzcxy      )
 call mpas_pool_get_array(output_noahmp,'qmeltcxy'    ,qmeltcxy     )
 call mpas_pool_get_array(output_noahmp,'qsnbotxy'    ,qsnbotxy     )
 call mpas_pool_get_array(output_noahmp,'pondingxy'   ,pondingxy    )
 call mpas_pool_get_array(output_noahmp,'fpicexy'     ,fpicexy      )
 call mpas_pool_get_array(output_noahmp,'rainlsm'     ,rainlsm      )
 call mpas_pool_get_array(output_noahmp,'snowlsm'     ,snowlsm      )
 call mpas_pool_get_array(output_noahmp,'forctlsm'    ,forctlsm     )
 call mpas_pool_get_array(output_noahmp,'forcqlsm'    ,forcqlsm     )
 call mpas_pool_get_array(output_noahmp,'forcplsm'    ,forcplsm     )
 call mpas_pool_get_array(output_noahmp,'forczlsm'    ,forczlsm     )
 call mpas_pool_get_array(output_noahmp,'forcwlsm'    ,forcwlsm     )
 call mpas_pool_get_array(output_noahmp,'eflxbxy'     ,eflxbxy      )
 call mpas_pool_get_array(output_noahmp,'soilenergy'  ,soilenergy   )
 call mpas_pool_get_array(output_noahmp,'snowenergy'  ,snowenergy   )
 call mpas_pool_get_array(output_noahmp,'canhsxy'     ,canhsxy      )

  do i = its,ite
    pahxy(i)        = mpas_noahmp%pahxy(i)
    pahgxy(i)       = mpas_noahmp%pahgxy(i)
    pahbxy(i)       = mpas_noahmp%pahbxy(i)
    pahvxy(i)       = mpas_noahmp%pahvxy(i)
    qintsxy(i)      = mpas_noahmp%qintsxy(i)
    qintrxy(i)      = mpas_noahmp%qintrxy(i)
    qdripsxy(i)     = mpas_noahmp%qdripsxy(i)
    qdriprxy(i)     = mpas_noahmp%qdriprxy(i)
    qthrosxy(i)     = mpas_noahmp%qthrosxy(i)
    qthrorxy(i)     = mpas_noahmp%qthrorxy(i)
    qsnsubxy(i)     = mpas_noahmp%qsnsubxy(i)
    qmeltxy(i)      = mpas_noahmp%qmeltxy(i)
    qsnfroxy(i)     = mpas_noahmp%qsnfroxy(i)
    qsubcxy(i)      = mpas_noahmp%qsubcxy(i)
    qfrocxy(i)      = mpas_noahmp%qfrocxy(i)
    qevacxy(i)      = mpas_noahmp%qevacxy(i)
    qdewcxy(i)      = mpas_noahmp%qdewcxy(i)
    qfrzcxy(i)      = mpas_noahmp%qfrzcxy(i)
    qmeltcxy(i)     = mpas_noahmp%qmeltcxy(i)
    qsnbotxy(i)     = mpas_noahmp%qsnbotxy(i)
    pondingxy(i)    = mpas_noahmp%pondingxy(i)
    fpicexy(i)      = mpas_noahmp%fpicexy(i)
    rainlsm(i)      = mpas_noahmp%rainlsm(i)
    snowlsm(i)      = mpas_noahmp%snowlsm(i)
    forctlsm(i)     = mpas_noahmp%forctlsm(i)
    forcqlsm(i)     = mpas_noahmp%forcqlsm(i)
    forcplsm(i)     = mpas_noahmp%forcplsm(i)
    forczlsm(i)     = mpas_noahmp%forczlsm(i)
    forcwlsm(i)     = mpas_noahmp%forcwlsm(i)
    eflxbxy(i)      = mpas_noahmp%eflxbxy(i)
    soilenergy(i)   = mpas_noahmp%soilenergy(i)
    snowenergy(i)   = mpas_noahmp%snowenergy(i)
    canhsxy(i)      = mpas_noahmp%canhsxy(i)
 enddo

!call mpas_log_write('--- end subroutine lsm_noahmp_toMPAS:')

 end subroutine lsm_noahmp_toMPAS

!=================================================================================================================
 subroutine driver_lsm_noahmp(configs,mesh,state,time_lev,diag,diag_physics,diag_physics_noahmp,output_noahmp, &
                              sfc_input,itimestep,its,ite)
!=================================================================================================================

!--- input arguments:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: diag
 type(mpas_pool_type),intent(in):: state

 integer,intent(in):: itimestep,its,ite
 integer,intent(in):: time_lev

!--- inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: diag_physics_noahmp
 type(mpas_pool_type),intent(inout):: output_noahmp
 type(mpas_pool_type),intent(inout):: sfc_input
 

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write(' ')
!call mpas_log_write('--- enter subroutine driver_lsm_noahmp:')

 call lsm_noahmp_fromMPAS(configs,mesh,diag,diag_physics,diag_physics_noahmp,output_noahmp,sfc_input, &
                          state,time_lev,itimestep)

 call NoahmpDriverMain(mpas_noahmp)

 call lsm_noahmp_toMPAS(diag_physics,diag_physics_noahmp,output_noahmp,sfc_input)

!call mpas_log_write('--- end subroutine driver_lsm_noahmp:')

 end subroutine driver_lsm_noahmp

!=================================================================================================================
 end module mpas_atmphys_driver_lsm_noahmp
!=================================================================================================================
