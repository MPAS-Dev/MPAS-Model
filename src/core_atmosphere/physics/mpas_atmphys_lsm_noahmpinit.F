! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module mpas_atmphys_lsm_noahmpinit

 use mpas_log
 use mpas_pool_routines
 use mpas_timekeeping,       only : mpas_get_timeInterval, mpas_get_clock_timestep

 use mpas_atmphys_constants, only : degrad, grav => gravity, t0 => svpt0
 use mpas_atmphys_utilities, only : physics_error_fatal
 use mpas_atmphys_vars,      only : mpas_noahmp

 use NoahmpInitMainMod,      only : NoahmpInitMain
 use NoahmpIOVarInitMod,     only : NoahmpIOVarInitDefault
 use NoahmpIOVarType,        only : NoahmpIO_type
 use NoahmpReadTableMod,     only : NoahmpReadTable
 use SnowInputSnicarMod,     only : SnowInputSnicar

 private
 public:: init_lsm_noahmp


 contains


!=================================================================================================================
 subroutine init_lsm_noahmp(configs,mesh,clock,diag_physics,diag_physics_noahmp,output_noahmp,sfc_input)
!=================================================================================================================

!--- input arguments:
 type(mpas_pool_type) ,intent(in):: configs
 type(mpas_pool_type) ,intent(in):: mesh
 type(mpas_clock_type),intent(in):: clock

!--- inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: diag_physics_noahmp
 type(mpas_pool_type),intent(inout):: output_noahmp
 type(mpas_pool_type),intent(inout):: sfc_input

!--- local variables and arrays:
 character(len=StrKIND),pointer:: mminlu

 integer:: ns

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write(' ')
!call mpas_log_write('--- enter subroutine init_lsm_noahmp:')


!--- initialize dimensions:
 call noahmp_read_dimensions(mesh)

 
!--- initialize namelist options:
 call noahmp_read_namelist(configs)


!--- allocate Noahmp arrays:
!call mpas_log_write(' ')
!call mpas_log_write('--- enter subroutine NoahmpIOVarInitDefault:')
 call NoahmpIOVarInitDefault(mpas_noahmp)
!call mpas_log_write('--- end subroutine NoahmpIOVarInitDefault:')


!--- read NoahmpTable.TBL:
 call mpas_pool_get_array(sfc_input,'mminlu',mminlu)
 mpas_noahmp%llanduse = mminlu

!call mpas_log_write(' ')
!call mpas_log_write('--- enter subroutine NoahmpReadTable:')
 call NoahmpReadTable(mpas_noahmp)
!call mpas_log_write('--- isbarren_table = $i',intArgs=(/mpas_noahmp%isbarren_table/))
!call mpas_log_write('--- isice_table    = $i',intArgs=(/mpas_noahmp%isice_table/)   )
!call mpas_log_write('--- iswater_table  = $i',intArgs=(/mpas_noahmp%iswater_table/) )
!call mpas_log_write('--- isurban_table  = $i',intArgs=(/mpas_noahmp%isurban_table/) )
!call mpas_log_write('--- urbtype_beg    = $i',intArgs=(/mpas_noahmp%urbtype_beg/)   )
!call mpas_log_write('--- slcats_table   = $i',intArgs=(/mpas_noahmp%slcats_table/)  )
!call mpas_log_write(' ')
!do ns = 1,mpas_noahmp%slcats_table
!   call mpas_log_write('--- BEXP,SMCMAX,PSISAT: $i $r $r $r',intArgs=(/ns/),realArgs= &
!                  (/mpas_noahmp%bexp_table(ns),mpas_noahmp%smcmax_table(ns),mpas_noahmp%psisat_table(ns)/))
!enddo
!call mpas_log_write('--- end subroutine NoahmpReadTable:')

! read in SNICAR parameter netcdif file
 if ( mpas_noahmp%iopt_alb == 3 ) then
    mpas_noahmp%snicar_optic_flnm = "snicar_optics_5bnd_c013122.nc"
    mpas_noahmp%snicar_age_flnm   = "snicar_drdt_bst_fit_60_c070416.nc"
    call SnowInputSnicar(mpas_noahmp)
 endif

!--- initialize noahmp:
 call noahmp_init(configs,mesh,clock,diag_physics,diag_physics_noahmp,output_noahmp,sfc_input)


!call mpas_log_write('--- end subroutine init_lsm_noahmp:')
!call mpas_log_write(' ')

 end subroutine init_lsm_noahmp

!=================================================================================================================
 subroutine noahmp_read_dimensions(mesh)
!=================================================================================================================

!--- input arguments:
 type(mpas_pool_type),intent(in):: mesh

!--- local variables and pointers:
 integer,pointer:: nCellsSolve,nVertLevels
 integer,pointer:: nSoilLevels,nSnowLevels

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write('--- enter subroutine noahmp_read_dimensions:')


 call mpas_pool_get_dimension(mesh,'nCellsSolve',nCellsSolve)
 call mpas_pool_get_dimension(mesh,'nVertLevels',nVertLevels)
 call mpas_pool_get_dimension(mesh,'nSoilLevels',nSoilLevels)
 call mpas_pool_get_dimension(mesh,'nSnowLevels',nSnowLevels)

 mpas_noahmp%its = 1
 mpas_noahmp%ite = nCellsSolve
 mpas_noahmp%kts = 1
 mpas_noahmp%kte = nVertLevels

 mpas_noahmp%nsoil = nSoilLevels
 mpas_noahmp%nsnow = nSnowLevels

!call mpas_log_write('    its = $i   ite = $i', intArgs=(/mpas_noahmp%its,mpas_noahmp%ite/))
!call mpas_log_write('    kts = $i   kte = $i', intArgs=(/mpas_noahmp%kts,mpas_noahmp%kte/))
!call mpas_log_write(' ')
!call mpas_log_write('    nSoilLevels = $i',intArgs=(/mpas_noahmp%nsoil/))
!call mpas_log_write('    nSnowLevels = $i',intArgs=(/mpas_noahmp%nsnow/))


!call mpas_log_write('--- end subroutine noahmp_read_dimensions:')

 end subroutine noahmp_read_dimensions

!=================================================================================================================
 subroutine noahmp_read_namelist(configs)
!=================================================================================================================

!--- input arguments:
 type(mpas_pool_type),intent(in):: configs


!--- local variables and pointers:
 integer,pointer:: iopt_dveg  , iopt_crs  , iopt_btr  , iopt_runsrf , iopt_runsub , iopt_sfc  , iopt_frz  , &
                   iopt_inf   , iopt_rad  , iopt_alb  , iopt_snf    , iopt_tksno  , iopt_tbot , iopt_stc  , &
                   iopt_gla   , iopt_rsf  , iopt_soil , iopt_pedo   , iopt_crop   , iopt_irr  , iopt_irrm , &
                   iopt_infdv , iopt_tdrn , iopt_compact , iopt_scf , iopt_wetland,                         &
                   iopt_snicar_bnd , iopt_snicar_spec , iopt_snicar_snwopts , iopt_snicar_dstopts ,         &
                   iopt_snicar_rt  , iopt_snicar_shp  , iopt_urban_physics
 logical,pointer:: iopt_snicar_aer , iopt_snicar_bcmix, iopt_snicar_dstmix  ,                               &
                   iopt_snicar_oc  , iopt_snicar_tbl

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write(' ')
!call mpas_log_write('--- enter subroutine noahmp_read_namelist:')

 call mpas_pool_get_config(configs,'config_noahmp_iopt_dveg'   ,iopt_dveg   )
 call mpas_pool_get_config(configs,'config_noahmp_iopt_crs'    ,iopt_crs    )
 call mpas_pool_get_config(configs,'config_noahmp_iopt_btr'    ,iopt_btr    )
 call mpas_pool_get_config(configs,'config_noahmp_iopt_runsrf' ,iopt_runsrf )
 call mpas_pool_get_config(configs,'config_noahmp_iopt_runsub' ,iopt_runsub )
 call mpas_pool_get_config(configs,'config_noahmp_iopt_sfc'    ,iopt_sfc    )
 call mpas_pool_get_config(configs,'config_noahmp_iopt_frz'    ,iopt_frz    )
 call mpas_pool_get_config(configs,'config_noahmp_iopt_inf'    ,iopt_inf    )
 call mpas_pool_get_config(configs,'config_noahmp_iopt_rad'    ,iopt_rad    )
 call mpas_pool_get_config(configs,'config_noahmp_iopt_alb'    ,iopt_alb    )
 call mpas_pool_get_config(configs,'config_noahmp_iopt_snf'    ,iopt_snf    )
 call mpas_pool_get_config(configs,'config_noahmp_iopt_tksno'  ,iopt_tksno  )
 call mpas_pool_get_config(configs,'config_noahmp_iopt_tbot'   ,iopt_tbot   )
 call mpas_pool_get_config(configs,'config_noahmp_iopt_stc'    ,iopt_stc    )
 call mpas_pool_get_config(configs,'config_noahmp_iopt_gla'    ,iopt_gla    )
 call mpas_pool_get_config(configs,'config_noahmp_iopt_rsf'    ,iopt_rsf    )
 call mpas_pool_get_config(configs,'config_noahmp_iopt_soil'   ,iopt_soil   )
 call mpas_pool_get_config(configs,'config_noahmp_iopt_pedo'   ,iopt_pedo   )
 call mpas_pool_get_config(configs,'config_noahmp_iopt_crop'   ,iopt_crop   )
 call mpas_pool_get_config(configs,'config_noahmp_iopt_irr'    ,iopt_irr    )
 call mpas_pool_get_config(configs,'config_noahmp_iopt_irrm'   ,iopt_irrm   )
 call mpas_pool_get_config(configs,'config_noahmp_iopt_infdv'  ,iopt_infdv  )
 call mpas_pool_get_config(configs,'config_noahmp_iopt_tdrn'   ,iopt_tdrn   )
 call mpas_pool_get_config(configs,'config_noahmp_iopt_compact',iopt_compact)
 call mpas_pool_get_config(configs,'config_noahmp_iopt_scf'    ,iopt_scf    )
 call mpas_pool_get_config(configs,'config_noahmp_iopt_wetland',iopt_wetland)
 call mpas_pool_get_config(configs,'config_noahmp_iopt_snicar_bnd'    ,iopt_snicar_bnd)
 call mpas_pool_get_config(configs,'config_noahmp_iopt_snicar_spec'   ,iopt_snicar_spec)
 call mpas_pool_get_config(configs,'config_noahmp_iopt_snicar_snwopts',iopt_snicar_snwopts)
 call mpas_pool_get_config(configs,'config_noahmp_iopt_snicar_dstopts',iopt_snicar_dstopts)
 call mpas_pool_get_config(configs,'config_noahmp_iopt_snicar_rt'     ,iopt_snicar_rt)
 call mpas_pool_get_config(configs,'config_noahmp_iopt_snicar_shp'    ,iopt_snicar_shp)
 call mpas_pool_get_config(configs,'config_noahmp_iopt_snicar_aer'    ,iopt_snicar_aer)
 call mpas_pool_get_config(configs,'config_noahmp_iopt_snicar_bcmix'  ,iopt_snicar_bcmix)
 call mpas_pool_get_config(configs,'config_noahmp_iopt_snicar_dstmix' ,iopt_snicar_dstmix)
 call mpas_pool_get_config(configs,'config_noahmp_iopt_snicar_oc'     ,iopt_snicar_oc)
 call mpas_pool_get_config(configs,'config_noahmp_iopt_snicar_tbl'    ,iopt_snicar_tbl)
 call mpas_pool_get_config(configs,'config_urban_physics'             ,iopt_urban_physics)


 mpas_noahmp%iopt_dveg    = iopt_dveg
 mpas_noahmp%iopt_crs     = iopt_crs
 mpas_noahmp%iopt_btr     = iopt_btr
 mpas_noahmp%iopt_runsrf  = iopt_runsrf
 mpas_noahmp%iopt_runsub  = iopt_runsub
 mpas_noahmp%iopt_sfc     = iopt_sfc
 mpas_noahmp%iopt_frz     = iopt_frz
 mpas_noahmp%iopt_inf     = iopt_inf
 mpas_noahmp%iopt_rad     = iopt_rad
 mpas_noahmp%iopt_alb     = iopt_alb
 mpas_noahmp%iopt_snf     = iopt_snf
 mpas_noahmp%iopt_tksno   = iopt_tksno
 mpas_noahmp%iopt_tbot    = iopt_tbot
 mpas_noahmp%iopt_stc     = iopt_stc
 mpas_noahmp%iopt_gla     = iopt_gla
 mpas_noahmp%iopt_rsf     = iopt_rsf
 mpas_noahmp%iopt_soil    = iopt_soil
 mpas_noahmp%iopt_pedo    = iopt_pedo
 mpas_noahmp%iopt_crop    = iopt_crop
 mpas_noahmp%iopt_irr     = iopt_irr
 mpas_noahmp%iopt_irrm    = iopt_irrm
 mpas_noahmp%iopt_infdv   = iopt_infdv
 mpas_noahmp%iopt_tdrn    = iopt_tdrn
 mpas_noahmp%iopt_compact = iopt_compact
 mpas_noahmp%iopt_scf     = iopt_scf
 mpas_noahmp%iopt_wetland = iopt_wetland
 mpas_noahmp%snicar_bandnumber_opt    = iopt_snicar_bnd
 mpas_noahmp%snicar_solarspec_opt     = iopt_snicar_spec
 mpas_noahmp%snicar_snowoptics_opt    = iopt_snicar_snwopts
 mpas_noahmp%snicar_dustoptics_opt    = iopt_snicar_dstopts
 mpas_noahmp%snicar_rtsolver_opt      = iopt_snicar_rt
 mpas_noahmp%snicar_snowshape_opt     = iopt_snicar_shp
 mpas_noahmp%snicar_use_aerosol       = iopt_snicar_aer
 mpas_noahmp%snicar_snowbc_intmix     = iopt_snicar_bcmix
 mpas_noahmp%snicar_snowdust_intmix   = iopt_snicar_dstmix
 mpas_noahmp%snicar_use_oc            = iopt_snicar_oc
 mpas_noahmp%snicar_aerosol_readtable = iopt_snicar_tbl
 mpas_noahmp%sf_urban_physics         = iopt_urban_physics

!--- check options that are not available in MPAS:
 if(iopt_soil == 4)   call physics_error_fatal("NOAHmp: iopt_soil = 4 is not an available option.")
 if(iopt_runsub == 5) call physics_error_fatal("NOAHmp: MMF GW model is not an available option.")
! if(iopt_crop > 0 ) call physics_error_fatal("NOAHmp: crop model is not an available option. set iopt_crop = 0")
! if(iopt_irr > 0  ) call physics_error_fatal("NOAHmp: irrigation is not an available option. set iopt_irr = 0" )
! if(iopt_irrm > 0 ) call physics_error_fatal("NOAHmp: irrigation is not an available option. set iopt_irrm = 0")
! if(iopt_tdrn > 0 ) call physics_error_fatal("NOAHmp: drainage is not an available option. set iopt_tdrn = 0"  )

!call mpas_log_write('--- end subroutine noahmp_read_namelist:')

 end subroutine noahmp_read_namelist

!=================================================================================================================
 subroutine noahmp_init(configs,mesh,clock,diag_physics,diag_physics_noahmp,output_noahmp,sfc_input)
!=================================================================================================================

!--- input arguments:
 type(mpas_pool_type), intent(in):: configs
 type(mpas_pool_type), intent(in):: mesh
 type(mpas_clock_type),intent(in):: clock

!--- inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: diag_physics_noahmp
 type(mpas_pool_type),intent(inout):: output_noahmp
 type(mpas_pool_type),intent(inout):: sfc_input

!local pointers:
 integer,pointer:: nsoilcomps
 integer,pointer:: ncrops
 integer,dimension(:),pointer:: isltyp,ivgtyp
 integer,dimension(:),pointer:: isnowxy
 integer,dimension(:),pointer:: irnumsi,irnummi,irnumfi,cropcat
 
 real(kind=RKIND)        :: dt
 real(kind=RKIND),pointer:: wtddt,stepwtd

 real(kind=RKIND),dimension(:),pointer:: soilcl1,soilcl2,soilcl3,soilcl4
 real(kind=RKIND),dimension(:),pointer:: areaCell,latCell,lonCell
 real(kind=RKIND),dimension(:),pointer:: canwat,lai,skintemp,snow,snowc,snowh,tmn,xice,xland
 real(kind=RKIND),dimension(:),pointer:: alboldxy,canicexy,canliqxy,chxy,cmxy,eahxy,fastcpxy,fwetxy,   &
                                         lfmassxy,qrainxy,qsnowxy,rtmassxy,sneqvoxy,stblcpxy,stmassxy, &
                                         tahxy,tgxy,tvxy,xsaixy,waxy,woodxy,wslakexy,wtxy,zwtxy,       &
                                         t2mbxy,t2mvxy,t2mxy
 real(kind=RKIND),dimension(:),pointer:: irwatsi,irsivol,ireloss,irrsplh,irwatmi,irmivol,irwatfi,irfivol
 real(kind=RKIND),dimension(:),pointer:: fdepthxy,terrain,rechclim,eqzwt,riverbedxy
 real(kind=RKIND),dimension(:),pointer:: irfract,sifract,mifract,fifract
 real(kind=RKIND),dimension(:),pointer:: planting,harvest,season_gdd,td_fraction
 real(kind=RKIND),dimension(:),pointer:: gddxy,grainxy,qtdrain
 real(kind=RKIND),dimension(:),pointer:: smcwtdxy,deeprechxy,rechxy,qrfsxy,qspringsxy,qslatxy, &
                                         rivercondxy,pexpxy
 real(kind=RKIND),dimension(:),pointer:: fsatxy,wsurfxy

 real(kind=RKIND),dimension(:,:),pointer:: dzs,sh2o,smois,tslb,smoiseq
 real(kind=RKIND),dimension(:,:),pointer:: snicexy,snliqxy,tsnoxy,zsnsoxy
 real(kind=RKIND),dimension(:,:),pointer:: snrdsxy,snfrxy,bcphixy,bcphoxy,ocphixy,ocphoxy,  &
                                           dust1xy,dust2xy,dust3xy,dust4xy,dust5xy,         &
                                           concbcphixy,concbcphoxy,concocphixy,concocphoxy, &
                                           concdust1xy,concdust2xy,concdust3xy,concdust4xy, &
                                           concdust5xy
 real(kind=RKIND),dimension(:,:),pointer:: croptype
 real(kind=RKIND),dimension(:,:),pointer:: soilcomp
 real(kind=RKIND),dimension(:,:),pointer:: albsoildirxy,albsoildifxy

!local variables and pointers:
 logical,pointer:: do_restart
 logical,parameter:: fndsnowh = .true.

 integer:: i,its,ite,n,ns,nsoil,nsnow,nzsnow,ierr,jp
 real(kind=RKIND),parameter:: hlice = 3.335E5

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write(' ')
!call mpas_log_write('--- enter subroutine noahmp_init:')


!--- initialization of local dimensions:
 its    = mpas_noahmp%its
 ite    = mpas_noahmp%ite
 nsoil  = mpas_noahmp%nsoil
 nsnow  = mpas_noahmp%nsnow
 nzsnow = nsnow + nsoil


!--- initialization of Noah-MP run parameters:
 call mpas_pool_get_config(configs,'config_do_restart',do_restart)
 call mpas_get_timeInterval(mpas_get_clock_timestep(clock, ierr), dt=dt)

 mpas_noahmp%restart_flag = do_restart

 mpas_noahmp%fndsnowh = fndsnowh
 mpas_noahmp%dtbl     = dt
 ! mpas_noahmp%wtddt  = wtddt ! can be defined as nml_config, frequency of groundwater call [minutes]:
                              ! default: 30min initialized in NoahmpIOVarType.F90

!--- initialization of Noah-MP mesh variables:
 call mpas_pool_get_array(mesh,'areaCell',areaCell)
 call mpas_pool_get_array(mesh,'latCell' ,latCell )
 call mpas_pool_get_array(mesh,'lonCell' ,lonCell )
 call mpas_pool_get_array(sfc_input,'ter',terrain)

 do i = its,ite
    mpas_noahmp%areaxy(i)  = areaCell(i)
    mpas_noahmp%xlat(i)    = latCell(i)/degrad
    mpas_noahmp%xlong(i)   = lonCell(i)/degrad
    mpas_noahmp%dx(i)      = sqrt(areaCell(i))
    mpas_noahmp%dy(i)      = sqrt(areaCell(i))
    mpas_noahmp%terrain(i) = terrain(i)
 enddo

!--- for vertically resolved or detailed soil composition option
 if (mpas_noahmp%iopt_soil > 1) then
    call mpas_pool_get_dimension(mesh,'nSoilComps',nsoilcomps)
    call mpas_pool_get_array(sfc_input,'soilcomp',soilcomp)
    call mpas_pool_get_array(sfc_input,'soilcl1' ,soilcl1 )
    call mpas_pool_get_array(sfc_input,'soilcl2' ,soilcl2 )
    call mpas_pool_get_array(sfc_input,'soilcl3' ,soilcl3 )
    call mpas_pool_get_array(sfc_input,'soilcl4' ,soilcl4 )

    do i = its,ite
       mpas_noahmp%soilcl1(i) = soilcl1(i)
       mpas_noahmp%soilcl2(i) = soilcl2(i)
       mpas_noahmp%soilcl3(i) = soilcl3(i)
       mpas_noahmp%soilcl4(i) = soilcl4(i)
       do ns = 1,nsoilcomps
          mpas_noahmp%soilcomp(i,ns) = soilcomp(ns,i)
       enddo
    enddo
 endif

!--- for crop model
 if (mpas_noahmp%iopt_crop > 0) then
    call mpas_pool_get_dimension(mesh,'nCrops',ncrops)
    call mpas_pool_get_array(sfc_input,'croptype',croptype)
    call mpas_pool_get_array(sfc_input,'planting',planting)
    call mpas_pool_get_array(sfc_input,'harvest',harvest)
    call mpas_pool_get_array(sfc_input,'season_gdd',season_gdd)

    do i = its,ite
       do jp = 1,ncrops
          mpas_noahmp%croptype(i,jp) = croptype(jp,i)
       enddo
       mpas_noahmp%planting(i)   = planting(i)
       mpas_noahmp%harvest(i)    = harvest(i)
       mpas_noahmp%season_gdd(i) = season_gdd(i)
    enddo
 endif

!--- for tile drainage model
 if (mpas_noahmp%iopt_tdrn > 0) then
    call mpas_pool_get_array(sfc_input,'td_fraction',td_fraction)

    do i = its,ite
       mpas_noahmp%td_fraction(i) = td_fraction(i)
    enddo
 endif

!--- for irrigation model
 if (mpas_noahmp%iopt_irr > 0) then
    call mpas_pool_get_array(sfc_input,'irfract',irfract)
    call mpas_pool_get_array(sfc_input,'sifract',sifract)
    call mpas_pool_get_array(sfc_input,'mifract',mifract)
    call mpas_pool_get_array(sfc_input,'fifract',fifract)

    do i = its,ite
       mpas_noahmp%irfract(i) = irfract(i)
       mpas_noahmp%sifract(i) = sifract(i)
       mpas_noahmp%mifract(i) = mifract(i)
       mpas_noahmp%fifract(i) = fifract(i)
    enddo
 endif

!--- for MMF groundwater scheme
 if (mpas_noahmp%iopt_runsub == 5) then
    call mpas_pool_get_array(sfc_input,'fdepthxy',fdepthxy)
    call mpas_pool_get_array(sfc_input,'rechclim',rechclim)
    call mpas_pool_get_array(sfc_input,'eqzwt',eqzwt)
    call mpas_pool_get_array(sfc_input,'riverbedxy',riverbedxy)

    do i = its,ite
       mpas_noahmp%fdepthxy(i)   = fdepthxy(i)
       mpas_noahmp%riverbedxy(i) = riverbedxy(i)
       mpas_noahmp%rechclim(i)   = rechclim(i)
       mpas_noahmp%eqzwt(i)      = eqzwt(i)
    enddo
 endif

!--- initialization of time-invariant surface variables needed in subroutine NoahmpInitMain:
 call mpas_pool_get_array(sfc_input,'dzs'   ,dzs   )
 call mpas_pool_get_array(sfc_input,'isltyp',isltyp)
 call mpas_pool_get_array(sfc_input,'ivgtyp',ivgtyp)

 do i = its, ite
    mpas_noahmp%isltyp(i) = isltyp(i)
    mpas_noahmp%ivgtyp(i) = ivgtyp(i)
 enddo
 do ns = 1, nsoil
    mpas_noahmp%dzs(ns) = dzs(ns,its)
 enddo


 if(mpas_noahmp%restart_flag) return

!--- read in Noah-MP variables used in initialization module NoahmpInitMain
 call mpas_pool_get_array(sfc_input,'skintemp',skintemp)
 call mpas_pool_get_array(sfc_input,'snow'    ,snow    )
 call mpas_pool_get_array(sfc_input,'snowc'   ,snowc   )
 call mpas_pool_get_array(sfc_input,'snowh'   ,snowh   )
 call mpas_pool_get_array(sfc_input,'tmn'     ,tmn     )
 call mpas_pool_get_array(sfc_input,'xice'    ,xice    )
 call mpas_pool_get_array(sfc_input,'xland'   ,xland   )
 call mpas_pool_get_array(sfc_input,'sh2o'    ,sh2o    )
 call mpas_pool_get_array(sfc_input,'smois'   ,smois   )
 call mpas_pool_get_array(sfc_input,'tslb'    ,tslb    )

 call mpas_pool_get_array(diag_physics,'canwat',canwat)
 call mpas_pool_get_array(diag_physics,'lai'   ,lai)

 call mpas_pool_get_array(diag_physics_noahmp,'alboldxy',alboldxy)
 call mpas_pool_get_array(diag_physics_noahmp,'canicexy',canicexy)
 call mpas_pool_get_array(diag_physics_noahmp,'canliqxy',canliqxy)
 call mpas_pool_get_array(diag_physics_noahmp,'chxy'    ,chxy    )
 call mpas_pool_get_array(diag_physics_noahmp,'cmxy'    ,cmxy    )
 call mpas_pool_get_array(diag_physics_noahmp,'eahxy'   ,eahxy   )
 call mpas_pool_get_array(diag_physics_noahmp,'fastcpxy',fastcpxy)
 call mpas_pool_get_array(diag_physics_noahmp,'fwetxy'  ,fwetxy  )
 call mpas_pool_get_array(diag_physics_noahmp,'lfmassxy',lfmassxy)
 call mpas_pool_get_array(diag_physics_noahmp,'rtmassxy',rtmassxy)
 call mpas_pool_get_array(diag_physics_noahmp,'sneqvoxy',sneqvoxy)
 call mpas_pool_get_array(diag_physics_noahmp,'stblcpxy',stblcpxy)
 call mpas_pool_get_array(diag_physics_noahmp,'stmassxy',stmassxy)
 call mpas_pool_get_array(diag_physics_noahmp,'tahxy'   ,tahxy   )
 call mpas_pool_get_array(diag_physics_noahmp,'tgxy'    ,tgxy    )
 call mpas_pool_get_array(diag_physics_noahmp,'tvxy'    ,tvxy    )
 call mpas_pool_get_array(diag_physics_noahmp,'waxy'    ,waxy    )
 call mpas_pool_get_array(diag_physics_noahmp,'woodxy'  ,woodxy  )
 call mpas_pool_get_array(diag_physics_noahmp,'wslakexy',wslakexy)
 call mpas_pool_get_array(diag_physics_noahmp,'wtxy'    ,wtxy    )
 call mpas_pool_get_array(diag_physics_noahmp,'xsaixy'  ,xsaixy  )
 call mpas_pool_get_array(diag_physics_noahmp,'zwtxy'   ,zwtxy   )
 call mpas_pool_get_array(diag_physics_noahmp,'isnowxy' ,isnowxy )
 call mpas_pool_get_array(diag_physics_noahmp,'snicexy' ,snicexy )
 call mpas_pool_get_array(diag_physics_noahmp,'snliqxy' ,snliqxy )
 call mpas_pool_get_array(diag_physics_noahmp,'tsnoxy'  ,tsnoxy  )
 call mpas_pool_get_array(diag_physics_noahmp,'zsnsoxy' ,zsnsoxy )
 call mpas_pool_get_array(diag_physics_noahmp,'albsoildirxy',albsoildirxy)
 call mpas_pool_get_array(diag_physics_noahmp,'albsoildifxy',albsoildifxy)

 call mpas_pool_get_array(output_noahmp      ,'qrainxy' ,qrainxy )
 call mpas_pool_get_array(output_noahmp      ,'qsnowxy' ,qsnowxy )
 call mpas_pool_get_array(output_noahmp      ,'t2mbxy'  ,t2mbxy  )
 call mpas_pool_get_array(output_noahmp      ,'t2mvxy'  ,t2mvxy  )
 call mpas_pool_get_array(output_noahmp      ,'t2mxy'   ,t2mxy   )

 if (mpas_noahmp%iopt_crop > 0) then
    call mpas_pool_get_array(diag_physics_noahmp,'cropcat',cropcat)
    call mpas_pool_get_array(diag_physics_noahmp,'gddxy'  ,gddxy  )
    call mpas_pool_get_array(diag_physics_noahmp,'grainxy',grainxy)
 endif

 if (mpas_noahmp%iopt_tdrn > 0) then
    call mpas_pool_get_array(diag_physics_noahmp,'qtdrain',qtdrain)
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
    call mpas_pool_get_array(diag_physics_noahmp,'stepwtd'    ,stepwtd    )
 endif

 if (mpas_noahmp%iopt_wetland > 0) then
    call mpas_pool_get_array(diag_physics_noahmp,'fsatxy' ,fsatxy )
    call mpas_pool_get_array(diag_physics_noahmp,'wsurfxy',wsurfxy)
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
 endif

!--- initialization of surface input variables needed in subroutine NoahmpInitMain:
 do i = its,ite
    mpas_noahmp%tmn(i)   = tmn(i)
    mpas_noahmp%tsk(i)   = skintemp(i)
    mpas_noahmp%xice(i)  = xice(i)
    mpas_noahmp%xland(i) = xland(i)
    mpas_noahmp%snow(i)  = snow(i)
    mpas_noahmp%snowh(i) = snowh(i)

    do ns = 1,nsoil
       mpas_noahmp%sh2o(i,ns)  = sh2o(ns,i)
       mpas_noahmp%smois(i,ns) = smois(ns,i)
       mpas_noahmp%tslb(i,ns)  = tslb(ns,i)
    enddo
 enddo

!--- Main Noah-MP initialization module
 call NoahmpInitMain(mpas_noahmp)


!--- update of Noah-MP variables with initialized values from NoahmpInitMain
 do i = its,ite
    isnowxy(i) = mpas_noahmp%isnowxy(i)
    snow(i)    = mpas_noahmp%snow(i)   ! in mm (check unit in noahmp driver).
    snowh(i)   = mpas_noahmp%snowh(i)  ! in m  (check unit in noahmp driver).
    snowc(i)   = 0._RKIND
    if(snow(i) > 0._RKIND) snowc(i) = 1._RKIND

    do ns = 1,nsoil
       sh2o(ns,i)  = mpas_noahmp%sh2o(i,ns)
       smois(ns,i) = mpas_noahmp%smois(i,ns)
       tslb(ns,i)  = mpas_noahmp%tslb(i,ns)
   enddo
 enddo

 do ns = 1,nsnow
    n = ns - nsnow
    do i = its,ite
       tsnoxy(ns,i)  = mpas_noahmp%tsnoxy(i,n)
       snicexy(ns,i) = mpas_noahmp%snicexy(i,n)
       snliqxy(ns,i) = mpas_noahmp%snliqxy(i,n)
       zsnsoxy(ns,i) = mpas_noahmp%zsnsoxy(i,n)
    enddo
 enddo
 
 do ns = nsnow+1,nzsnow
    n = ns - nsnow
    do i = its,ite
       zsnsoxy(ns,i) = mpas_noahmp%zsnsoxy(i,n)
    enddo
 enddo

 do i = its,ite
    canwat(i)       = mpas_noahmp%canwat(i)
    lai(i)          = mpas_noahmp%lai(i)
    isnowxy(i)      = mpas_noahmp%isnowxy(i)
    alboldxy(i)     = mpas_noahmp%alboldxy(i)
    canicexy(i)     = mpas_noahmp%canicexy(i)
    canliqxy(i)     = mpas_noahmp%canliqxy(i)
    chxy(i)         = mpas_noahmp%chxy(i)
    cmxy(i)         = mpas_noahmp%cmxy(i)
    eahxy(i)        = mpas_noahmp%eahxy(i)
    fastcpxy(i)     = mpas_noahmp%fastcpxy(i)
    fwetxy(i)       = mpas_noahmp%fwetxy(i)
    lfmassxy(i)     = mpas_noahmp%lfmassxy(i)
    qrainxy(i)      = mpas_noahmp%qrainxy(i)
    qsnowxy(i)      = mpas_noahmp%qsnowxy(i)
    rtmassxy(i)     = mpas_noahmp%rtmassxy(i)
    sneqvoxy(i)     = mpas_noahmp%sneqvoxy(i)
    stblcpxy(i)     = mpas_noahmp%stblcpxy(i)
    stmassxy(i)     = mpas_noahmp%stmassxy(i)
    tahxy(i)        = mpas_noahmp%tahxy(i)
    tgxy(i)         = mpas_noahmp%tgxy(i)
    tvxy(i)         = mpas_noahmp%tvxy(i)
    waxy(i)         = mpas_noahmp%waxy(i)
    woodxy(i)       = mpas_noahmp%woodxy(i)
    wslakexy(i)     = mpas_noahmp%wslakexy(i)
    wtxy(i)         = mpas_noahmp%wtxy(i)
    xsaixy(i)       = mpas_noahmp%xsaixy(i)
    zwtxy(i)        = mpas_noahmp%zwtxy(i)
    t2mbxy(i)       = mpas_noahmp%t2mbxy(i)
    t2mvxy(i)       = mpas_noahmp%t2mvxy(i)
    t2mxy(i)        = mpas_noahmp%t2mxy(i)

    do n = 1,mpas_noahmp%numrad 
       albsoildirxy(n,i) = mpas_noahmp%albsoildirxy(i,n)
       albsoildifxy(n,i) = mpas_noahmp%albsoildifxy(i,n)
    enddo
 enddo

 if (mpas_noahmp%iopt_crop > 0) then
    do i = its,ite
       cropcat(i) = mpas_noahmp%cropcat(i)
       gddxy(i)   = mpas_noahmp%gddxy(i)
       grainxy(i) = mpas_noahmp%grainxy(i)
   enddo
 endif

 if (mpas_noahmp%iopt_tdrn > 0) then
    do i = its,ite
       qtdrain(i) = mpas_noahmp%qtdrain(i)
    enddo
 endif

 if (mpas_noahmp%iopt_irr > 0) then
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
    stepwtd = mpas_noahmp%stepwtd
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
    do i = its,ite
       fsatxy(i)  = mpas_noahmp%fsatxy(i)
       wsurfxy(i) = mpas_noahmp%wsurfxy(i)
    enddo
 endif

 if (mpas_noahmp%iopt_alb == 3) then
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

!call mpas_log_write('--- end subroutine noahmp_init:')

 end subroutine noahmp_init

!=================================================================================================================
 end module mpas_atmphys_lsm_noahmpinit
!=================================================================================================================
