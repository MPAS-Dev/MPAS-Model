! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
#define DM_BCAST_CHAR(A)     call mpas_dmpar_bcast_char(dminfo,A)
#define DM_BCAST_INTEGER(A)  call mpas_dmpar_bcast_int(dminfo,A)
#define DM_BCAST_INTEGERS(A) call mpas_dmpar_bcast_ints(dminfo,size(A),A)
#define DM_BCAST_REAL(A)     call mpas_dmpar_bcast_real(dminfo,A)
#define DM_BCAST_REALS(A)    call mpas_dmpar_bcast_reals(dminfo,size(A),A)

!=================================================================================================================
 module mpas_atmphys_lsm_rucinit
 use mpas_dmpar
 use mpas_kind_types
 use mpas_pool_routines

 use mpas_atmphys_constants
 use mpas_atmphys_utilities
! NOAA physics
 use module_ruc_land

 implicit none
 private
 public:: ruc_init_forMPAS


!Initialization of the RUC land surface scheme. Adapted from NOAH init routine
!
! So far have just renamed routines for RUCLSM, need to pull in the RUC code and adapt.
!
!Laura D. Fowler (send comments to laura@ucar.edu).
!2013-05-01.
!
! subroutines in mpas_atmphys_lsm_noahinit:
! -----------------------------------------
! ruclsm_init_forMPAS: call lsminit from subroutine init_lsm (module mpas_atmphyse_driver_lsm.F).
! lsminit          : main initialization subroutine for the RUC 9-level land surface scheme.
!
! add-ons and modifications to sourcecode:
! ----------------------------------------
! * replaced the variable grav with gravity, for simplicity.
!   Laura D. Fowler (laura@ucar.edu) / 2014-03-21.
! * added "use mpas_kind_types" at the top of the module.
!   Laura D. Fowler (laura@ucar.edu) / 2014-09-18.
! * in subroutine soil_veg_gen_parm, modified reading the updated file VEGPARM.TBL so that we can update the NOAH
!   land surface scheme.added the categories low_density_residential,high_density_residential,and high_intensity_
!   industrial.added the variables ztopvtbl and zbotvtbl.
!   Laura D. Fowler (laura@ucar.edu) / 2017-01-25.


 contains


!=================================================================================================================
 subroutine ruc_init_forMPAS(dminfo,mesh,configs,diag_physics,sfc_input)
!=================================================================================================================

!input arguments:
 type(dm_info):: dminfo
 type(mpas_pool_type):: mesh
 type(mpas_pool_type),intent(in):: configs

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: sfc_input

!-----------------------------------------------------------------------------------------------------------------

!read formatted files needed for land-surface model:
 call ruclsminit(dminfo,mesh,configs,diag_physics,sfc_input)

 end subroutine ruc_init_forMPAS

!=================================================================================================================
 subroutine ruclsminit(dminfo,mesh,configs,diag_physics,sfc_input)
!=================================================================================================================

!input arguments:
 type(dm_info),intent(in):: dminfo
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: configs

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: sfc_input

!local pointers::
 logical,pointer:: input_sfc_albedo,restart,cycling

 character(len=StrKIND),pointer:: mminlu,mminsl

 integer,pointer:: nCells,nSoilLevels
 integer,pointer:: spp_lsm_loc,mosaic_lu,mosaic_soil
 integer,dimension(:),pointer:: ivgtyp,isltyp

 real(kind=RKIND),dimension(:),pointer:: snoalb,snow,snowh,znt,xice,mavail
 real(kind=RKIND),dimension(:,:),pointer:: tslb,smois,sh2o,smfr3d

!local variables:
 logical,parameter:: fndsnowh = .true.

 integer:: iCell, l
 integer:: errflag,ns

 real(kind=RKIND):: bx,fk,smcmax,psisat,free,tln
 real(kind=RKIND):: dqm, ref, psis, qmin, bclh
 real(kind=RKIND), allocatable:: soiliqw(:)
 real(kind=RKIND),parameter:: blim = 5.5
 real(kind=RKIND),parameter:: hlice = 3.335e5
 real(kind=RKIND),parameter:: t0 = 273.15
 real(kind=RKIND),parameter:: xlmelt=3.35E5
 real(kind=RKIND),parameter:: riw=900.E-3

  character*8 :: MMINLURUC

!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_array(sfc_input,'mminlu'              ,mminlu          )
 call mpas_pool_get_config(configs,'input_soil_data'      ,mminsl          )
 call mpas_pool_get_config(configs,'config_sfc_snowalbedo',input_sfc_albedo)
 call mpas_pool_get_config(configs,'config_do_restart'    ,restart         )
 call mpas_pool_get_config(configs,'config_do_DAcycling'  ,cycling         )


 call mpas_pool_get_dimension(mesh,'nCells'     ,nCells     )
 call mpas_pool_get_dimension(mesh,'nSoilLevels',nSoilLevels)

 call mpas_pool_get_array(sfc_input,'isltyp', isltyp)
 call mpas_pool_get_array(sfc_input,'ivgtyp', ivgtyp)
 call mpas_pool_get_array(sfc_input,'sh2o'  , sh2o  )
 call mpas_pool_get_array(sfc_input,'smois' , smois )
 call mpas_pool_get_array(sfc_input,'tslb'  , tslb  )
 call mpas_pool_get_array(sfc_input,'snoalb', snoalb)
 call mpas_pool_get_array(sfc_input,'snow'  , snow  )
 call mpas_pool_get_array(sfc_input,'snowh' , snowh )
 call mpas_pool_get_array(sfc_input,'xice'  , xice  )
 call mpas_pool_get_array(diag_physics,'spp_lsm_loc', spp_lsm_loc)
 call mpas_pool_get_array(diag_physics,'mosaic_lu', mosaic_lu)
 call mpas_pool_get_array(diag_physics,'mosaic_soil', mosaic_soil)

 call mpas_pool_get_array(diag_physics,'mavail', mavail )
 call mpas_pool_get_array(diag_physics,'smfr3d', smfr3d )
 call mpas_pool_get_array(diag_physics,'znt'   , znt    )

 allocate(soiliqw(nSoilLevels))

!hardcode these flags for now
 spp_lsm_loc = 0
 mosaic_lu = 1
 mosaic_soil = 1

!reads the RUC LSM tables:
 call mpas_log_write('')
 call physics_message('--- initialize RUC LSM tables' )

! initialize three  LSM related tables
!   IF ( allowed_to_read ) THEN
     CALL physics_message( 'INITIALIZE THREE LSM RELATED TABLES' )
      if(mminlu == 'USGS') then
        MMINLURUC='USGS-RUC'
      elseif(mminlu == 'MODIS' .OR. &
        &    mminlu == 'MODIFIED_IGBP_MODIS_NOAH') then
        MMINLURUC='MODI-RUC'
      endif
        MMINSL='STAS-RUC'
!     CALL  RUCLSM_PARM_INIT
!   ENDIF

 call RUCLSM_SOILVEGPARM(dminfo,mminluruc,mminsl)
 call physics_message('--- end initialize RUC LSM tables' )
 call mpas_log_write('')

if(.not.restart .and. .not. cycling) then

   errflag = 0
   do iCell = 1, nCells
      if(isltyp(iCell) .lt. 1) then
         errflag = 1
         write(err_message,*) "module_ruc_land.F: lsminit: out of range ISLTYP ", &
                              iCell,isltyp(iCell)
         call physics_message(err_message)
      endif
   enddo
   if(errflag .eq. 1) &
      call physics_error_fatal("module_ruc_land.F: lsminit: out of range value "// &
                           "of ISLTYP. Is this field in the input?" )
   do iCell = 1, nCells
        znt(iCell) = z0tbl(ivgtyp(iCell))

!--- Computation of volumetric content of ice in soil
!--- and initialize MAVAIL
        dqm    = maxsmc(isltyp(iCell)) -&
                 drysmc(isltyp(iCell))
        ref    = refsmc(isltyp(iCell))
        psis   = - satpsi(isltyp(iCell))
        qmin   = drysmc(isltyp(iCell))
        bclh   = bb(isltyp(iCell))

        if(xice(iCell) .gt. 0.) then
          do l=1,nSoilLevels
            smfr3d(l,iCell) = 1.
            sh2o(l,iCell) = 0.
            mavail(iCell) = 1.
          enddo
        else
          if(isltyp(iCell) .ne. 14) then
!-- land
            mavail(iCell) = max(0.00001,min(1.,(smois(1,iCell)-qmin)/(ref-qmin)))
            do l=1,nSoilLevels
              tln = log(tslb(l,iCell) / 273.15 )
              if( tln .lt. 0. ) then
                 soiliqw(l) = (dqm+qmin)*(xlmelt * (tslb(l,iCell)-273.15) &
                        /tslb(l,iCell)/9.81/psis) **(-1./bclh)
                 soiliqw(l)=max(0.,soiliqw(l))
                 soiliqw(l)=min(soiliqw(l),smois(l,iCell))
                 sh2o(l,iCell)=soiliqw(l)
                 smfr3d(l,iCell)=(smois(l,iCell)-soiliqw(l))/RIW

              else
                smfr3d(l,iCell)=0.
                sh2o(l,iCell)=smois(l,iCell)
              endif
            enddo
          else
            do l = 1,nSoilLevels
              smfr3d(l,iCell) = 0.
              sh2o(l,iCell)   = 1.
              mavail(iCell) = 1.
            enddo
          endif
        endif

!initialize physical snow height SNOWH:
       if(.not.fndsnowh)then
!if no snowh do the following:
          call physics_message( 'SNOW HEIGHT NOT FOUND - VALUE DEFINED IN LSMINIT')
          snowh(iCell)=snow(iCell)*0.005 ! snow in mm and snowh in m
       endif
   enddo
endif
 deallocate(soiliqw)
 end subroutine ruclsminit

!=================================================================================================================
 subroutine RUCLSM_SOILVEGPARM(dminfo,mminluruc,mminsl)
!=================================================================================================================

!input arguments:
 type(dm_info),intent(in):: dminfo
 character(len=*),intent(inout):: mminluruc, mminsl

!local variables:
 character*128:: mess , message
 character*256 :: a_string

 integer,parameter:: open_ok = 0
 integer:: lumatch,iindex,lc,num_slope
 integer:: istat,loop_count
 integer,parameter:: loop_max = 10

!-----SPECIFY VEGETATION RELATED CHARACTERISTICS :
!ALBBCK: SFC albedo (in percentage)
!Z0    : Roughness length (m)
!SHDFAC: Green vegetation fraction (in percentage)

!Note: The ALBEDO, Z0, and SHDFAC values read from the following table
!ALBEDO,and Z0 are specified in LAND-USE TABLE. SHDFAC is the monthly green vegetation data.
!CMXTBL: MAX CNPY Capacity (m)
!NROTBL: Rooting depth (layer)
!RSMIN : Mimimum stomatal resistance (s m-1)
!RSMAX : Max. stomatal resistance (s m-1)
!RGL   : Parameters used in radiation stress function
!HS    : Parameter used in vapor pressure deficit function
!TOPT  : Optimum transpiration air temperature. (K)
!CMCMAX: Maximum canopy water capacity
!CFACTR: Parameter used in the canopy inteception calculation
!SNUP  : Threshold snow depth (in water equivalent m) that implies 100% snow cover.
!LAI   : Leaf area index (dimensionless)
!MAXALB: Upper bound on maximum albedo over deep snow
!
!-----READ IN VEGETATION PROPERTIES FROM VEGPARM.TBL

!-----------------------------------------------------------------------------------------------------------------
!call mpas_log_write('')
!call mpas_log_write('       enter subroutine soil_veg_gen_parm:')

!read in the vegetation properties from vegparm.tbl:

 if(dminfo % my_proc_id == IO_NODE) then
    open(16,file='VEGPARM.TBL',form='FORMATTED',status='OLD',iostat=istat)
    if(istat /= open_ok) &
       call physics_error_fatal('subroutine RUCLSM_SOILVEGPARM: ' // &
                                'failure opening VEGPARM.TBL')

    lumatch = 0
    loop_count = 0
    read(16,*,end=2002) a_string
    find_lutype : do while (lumatch == 0)
       read(16,*,end=2002) lutype
       read(16,*) lucats,iindex

       if(lutype.eq.trim(mminluruc))then
          write(mess,*) '   landuse type = ' // trim ( lutype ) // ' found', &
                        lucats, ' categories'
          call physics_message(mess)
          lumatch=1
      ! elseif(trim(lutype) .eq. 'USGS-RUC' .or. lutype .eq. 'MODI-RUC') then
      !   call physics_message('    skipping over lutype = ' // trim ( lutype ))
      !    do lc = 1, lucats+16
      !       read(16,*)
      !    enddo
       else
      !    call physics_message('    skipping over lutype = ' // trim ( lutype ))
      !    do lc = 1, lucats+20
      !       read(16,*)
      !    enddo
         loop_count = loop_count+1
         call physics_message ( "Skipping over LUTYPE = " // TRIM ( LUTYPE ) )
         FIND_VEGETATION_PARAMETER_FLAG : DO
            READ (16,FMT='(A)', END=2002) a_string
            IF ( a_string(1:21) .EQ. 'Vegetation Parameters' ) THEN
               EXIT FIND_VEGETATION_PARAMETER_FLAG
            ELSE IF ( loop_count .GE. loop_max ) THEN
               CALL physics_error_fatal ( 'Too many loops in VEGPARM.TBL')
            ENDIF
         ENDDO FIND_VEGETATION_PARAMETER_FLAG 
       endif
    enddo find_lutype

!prevent possible array overwrite, Bill Bovermann, IBM, May 6, 2008:
!   if(size(albtbl)       < lucats .or. &
!      size(z0tbl)       < lucats .or. &
!      size(lemitbl)        < lucats .or. &
!      size(pctbl)       < lucats .or. &
!      size(shdtbl)        < lucats .or. &
!      size(ifortbl)      < lucats .or. &
!      size(rstbl)       < lucats .or. &
!      size(rgltbl)    < lucats .or. &
!      size(hstbl)    < lucats .or. &
!      size(snuptbl)     < lucats .or. &
!      size(laitbl)     < lucats .or. &
!      size(maxalb) < lucats) then 
!      call physics_error_fatal('table sizes too small for value of lucats in module_sf_rucdrv.f')
!   endif

    if(lutype.eq.trim(mminluruc))then
       call physics_message('Reading ' // trim(lutype) // 'table' )
       do lc = 1, lucats
           READ (16,*)IINDEX,ALBTBL(LC),Z0TBL(LC),LEMITBL(LC),PCTBL(LC), &
                         SHDTBL(LC),IFORTBL(LC),RSTBL(LC),RGLTBL(LC),         &
                         HSTBL(LC),SNUPTBL(LC),LAITBL(LC),MAXALB(LC)

       enddo
       read (16,*)
       read (16,*) topt_data
       read (16,*)
       read (16,*) cmcmax_data
       read (16,*)
       read (16,*) cfactr_data
       read (16,*)
       read (16,*) rsmax_data
       read (16,*)
       read (16,*) bare
       read (16,*)
       read (16,*) natural
       read (16,*)
       read (16,*) crop
       read (16,*)
       read (16,*) urban
!       read (16,*)
!       read (16,*)high_density_residential
!       read (16,*)
!       read (16,*)high_intensity_industrial
    endif

    2002 continue
    close (16)
    if(lumatch == 0) &
       call physics_error_fatal ('land use dataset '''//mminluruc//''' not found in VEGPARM.TBL.')
      
 endif ! end dminfo

!distribute data to all processors: 
DM_BCAST_CHAR(lutype)
DM_BCAST_INTEGER(lucats)
DM_BCAST_INTEGER(iindex)
DM_BCAST_INTEGER(lumatch)
DM_BCAST_REALS(albtbl)
DM_BCAST_REALS(z0tbl)
DM_BCAST_REALS(lemitbl)
DM_BCAST_REALS(pctbl)
DM_BCAST_REALS(shdtbl)
DM_BCAST_INTEGERS(ifortbl)
DM_BCAST_REALS(rstbl)
DM_BCAST_REALS(rgltbl)
DM_BCAST_REALS(hstbl)
DM_BCAST_REALS(snuptbl)
DM_BCAST_REALS(laitbl)
DM_BCAST_REALS(maxalb)
DM_BCAST_REAL(topt_data)
DM_BCAST_REAL(cmcmax_data)
DM_BCAST_REAL(cfactr_data)
DM_BCAST_REAL(rsmax_data)
DM_BCAST_INTEGER(bare)
DM_BCAST_INTEGER(natural)
DM_BCAST_INTEGER(crop)
DM_BCAST_INTEGER(urban)

call mpas_log_write(' LUTYPE  = '//trim(lutype))
call mpas_log_write(' LUCATS  = $i',intArgs=(/lucats/))
call mpas_log_write(' IINDEX  = $i',intArgs=(/iindex/))
call mpas_log_write(' LUMATCH = $i',intArgs=(/lumatch/))

call mpas_log_write(' TOPT_DATA   = $r',realArgs=(/topt_data/))
call mpas_log_write(' CMCMAX_DATA = $r',realArgs=(/cmcmax_data/))
call mpas_log_write(' CFACTR_DATA = $r',realArgs=(/cfactr_data/))
call mpas_log_write(' RSMAX_DATA  = $r',realArgs=(/rsmax_data/))
call mpas_log_write(' BARE        = $i',intArgs=(/bare/))
call mpas_log_write(' NATURAL     = $i',intArgs=(/natural/))
call mpas_log_write(' CROP   = $i ', intArgs=(/crop/))
call mpas_log_write(' URBAN  = $i ', intArgs=(/urban/))
call mpas_log_write('')
do lc = 1, lucats
   call mpas_log_write('$i $r $r $r $r $r $r $r $r $r $r $r $r $r $r $r $r $r', intArgs=(/lc/),     &
                realArgs=(/albtbl(lc),z0tbl(lc),lemitbl(lc),pctbl(lc),shdtbl(lc),real(ifortbl(lc),kind=RKIND), &
                rstbl(lc),rgltbl(lc),hstbl(lc),snuptbl(lc),laitbl(lc),             &
                maxalb(lc)/))
enddo
 call mpas_log_write('    end read VEGPARM.TBL')

!read in soil properties from soilparm.tbl:

 if(dminfo % my_proc_id == IO_NODE) then
    open(16,file='SOILPARM.TBL',form='FORMATTED',status='OLD',iostat=istat)
    if(istat /= open_ok) &
       call physics_error_fatal('mpas_atmphys_lsm_rucinit.F: RUCLSM_SOILVEGPARM: ' // &
                                'failure opening SOILPARM.TBL' )

    write(mess,*) '   input soil texture classification = ', trim (mminsl)
    call physics_message(mess)


    lumatch=0
    do while  (lumatch == 0)
      read(16,*)
      read(16,*,end=2003) sltype
      !2000 format(a4)
      read(16,*)slcats,iindex
      if(trim(sltype).eq.trim(mminsl))then
         write(mess,*) '   soil texture classification = ', trim ( sltype ) , ' found', &
                    slcats,' categories'
         call physics_message ( mess )
         lumatch=1
      else
         call physics_message('    skipping over sltype = ' // trim ( sltype ))
         do lc = 1, slcats
            read(16,*)
         enddo
       endif
    enddo

!prevent possible array overwrite, Bill Bovermann, IBM, May 6, 2008:
!   if(size(bb) < slcats .or. &
!      size(drysmc) < slcats .or. &
!      size(hc    ) < slcats .or. &
!      size(maxsmc) < slcats .or. &
!      size(refsmc) < slcats .or. &
!      size(satpsi) < slcats .or. &
!      size(satdk ) < slcats .or. &
!      size(satdw ) < slcats .or. &
!      size(wltsmc) < slcats .or. &
!      size(qtz   ) < slcats) then
!      call physics_error_fatal('table sizes too small for value of slcats in module_sf_noahdrv.f')
!  endif

    if(sltype.eq.trim(mminsl)) then
       do lc = 1, slcats
          read(16,*) iindex,bb(lc),drysmc(lc),hc(lc),maxsmc(lc),refsmc(lc),satpsi(lc), &
                      satdk(lc),satdw(lc),wltsmc(lc),qtz(lc)
       enddo
    endif

    2003 continue
    close(16)
    if(lumatch.eq.0)then
       call physics_message( 'soil texture in input file does not ' )
       call physics_message( 'match soilparm table'                 )
       call physics_error_fatal( 'inconsistent or missing soilparm file' )
    endif

 endif

!distribute data to all processors: 
DM_BCAST_INTEGER(lumatch)
DM_BCAST_CHAR(sltype)
DM_BCAST_CHAR(mminsl)
DM_BCAST_INTEGER(slcats)
DM_BCAST_INTEGER(iindex)
DM_BCAST_REALS(bb)
DM_BCAST_REALS(drysmc)
DM_BCAST_REALS(hc)
DM_BCAST_REALS(maxsmc)
DM_BCAST_REALS(refsmc)
DM_BCAST_REALS(satpsi)
DM_BCAST_REALS(satdk)
DM_BCAST_REALS(satdw)
DM_BCAST_REALS(wltsmc)
DM_BCAST_REALS(qtz)

call mpas_log_write(' LUMATCH=$i',intArgs=(/lumatch/))
call mpas_log_write(' SLTYPE ='//trim(sltype))
call mpas_log_write(' MMINSL ='//mminsl)
call mpas_log_write(' SLCATS =$i',intArgs=(/slcats/))
call mpas_log_write(' IINDEX =$i',intArgs=(/iindex/))
call mpas_log_write('')
do lc = 1, slcats
   call mpas_log_write('$i $r $r $r $r $r $r $r $r $r $r', intArgs=(/lc/), &
                realArgs=(/bb(lc),drysmc(lc),hc(lc),maxsmc(lc),refsmc(lc),satpsi(lc), &
                satdk(lc),satdw(lc),wltsmc(lc),qtz(lc)/))
enddo
 call mpas_log_write('    end read SOILPARM.TBL')

!read in general parameters from genparm.tbl:

 if(dminfo % my_proc_id == IO_NODE) then
    open(16,file='GENPARM.TBL',form='FORMATTED',status='OLD',iostat=istat)
    if(istat /= open_ok) &
       call physics_error_fatal('module_sf_noahlsm.F: soil_veg_gen_parm: ' // &
                                'failure opening GENPARM.TBL' )
    read(16,*)
    read(16,*)
    read(16,*) num_slope

    slpcats=num_slope
!prevent possible array overwrite, Bill Bovermann, IBM, May 6, 2008:
    if(size(slope_data) < num_slope) &
       call physics_error_fatal('NUM_SLOPE too large for slope_data array' // &
                                'in module_sf_noahdrv')

    do lc = 1, slpcats
       read(16,*)slope_data(lc)
    enddo
    read(16,*)
    read(16,*)sbeta_data
    read(16,*)
    read(16,*)fxexp_data
    read(16,*)
    read(16,*)csoil_data
    read(16,*)
    read(16,*)salp_data
    read(16,*)
    read(16,*)refdk_data
    read(16,*)
    read(16,*)refkdt_data
    read(16,*)
    read(16,*)frzk_data
    read(16,*)
    read(16,*)zbot_data
    read(16,*)
    read(16,*)czil_data
    read(16,*)
    read(16,*)smlow_data
    read(16,*)
    read(16,*)smhigh_data
    close(16)
 endif

 DM_BCAST_INTEGER(num_slope)
 DM_BCAST_INTEGER(slpcats)
 DM_BCAST_REALS(slope_data)
 DM_BCAST_REAL(sbeta_data)
 DM_BCAST_REAL(fxexp_data)
 DM_BCAST_REAL(csoil_data)
 DM_BCAST_REAL(salp_data)
 DM_BCAST_REAL(refdk_data)
 DM_BCAST_REAL(refkdt_data)
 DM_BCAST_REAL(frzk_data)
 DM_BCAST_REAL(zbot_data)
 DM_BCAST_REAL(czil_data)
 DM_BCAST_REAL(smlow_data)
 DM_BCAST_REAL(smhigh_data)

call mpas_log_write('')
write(mess,*) 'input general parameters'
call physics_message(mess)
call mpas_log_write('NUM_SLOPE=$i',intArgs=(/num_slope/))
do lc = 1, slpcats
   call mpas_log_write('$i $r', intArgs=(/lc/),realArgs=(/slope_data(lc)/))
enddo
! call mpas_log_write('       end subroutine soil_veg_gen_parm:')
 call mpas_log_write('    end read GENPARM.TBL')

 end subroutine RUCLSM_SOILVEGPARM

!=================================================================================================================
 end module mpas_atmphys_lsm_rucinit
!=================================================================================================================
