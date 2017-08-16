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
 module mpas_atmphys_lsm_noahinit
 use mpas_dmpar
 use mpas_kind_types
 use mpas_derived_types
 use mpas_pool_routines


 use mpas_atmphys_constants
 use mpas_atmphys_utilities
 use mpas_atmphys_landuse, only: isice, iswater

!wrf physics
 use module_sf_noahlsm


 implicit none
 private
 public:: noah_init_forMPAS


!Initialization of the NOAH 4-layer land surface scheme.
!Laura D. Fowler (send comments to laura@ucar.edu).
!2013-05-01.
!
! subroutines in mpas_atmphys_lsm_noahinit:
! -----------------------------------------
! noah_init_forMPAS: call lsminit from subroutine init_lsm (module mpas_atmphyse_driver_lsm.F).
! lsminit          : main initialization subroutine for the NOAH 4-layer land surface scheme.
!
! add-ons and modifications to sourcecode:
! ----------------------------------------
! * replaced the variable grav with gravity, for simplicity.
!   Laura D. Fowler (laura@ucar.edu) / 2014-03-21.
! * added "use mpas_kind_types" at the top of the module.
!   Laura D. Fowler (laura@ucar.edu) / 2014-09-18.
! * added lsm_mosaic_init subroutine and declarations.
!   Patrick C. Campbell (campbell.patrickc@epa.gov) / 2017-05-18.

 contains


!=================================================================================================================
 subroutine noah_init_forMPAS(dminfo,mesh,configs,diag_physics,sfc_input)
!=================================================================================================================

!input arguments:
 type(dm_info),intent(in):: dminfo
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: configs

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: sfc_input

!-----------------------------------------------------------------------------------------------------------------

!read formatted files needed for land-surface model:

 call lsminit(dminfo,mesh,configs,diag_physics,sfc_input)

 end subroutine noah_init_forMPAS

!=================================================================================================================
 subroutine lsminit(dminfo,mesh,configs,diag_physics,sfc_input)
!=================================================================================================================

!input arguments:
 type(dm_info),intent(in):: dminfo
 type(mpas_pool_type),intent(in):: mesh
 type(mpas_pool_type),intent(in):: configs

!inout arguments:
 type(mpas_pool_type),intent(inout):: diag_physics
 type(mpas_pool_type),intent(inout):: sfc_input

!local pointers::
 logical,pointer:: input_sfc_albedo,restart


 character(len=StrKIND),pointer:: mminlu,mminsl

 integer,pointer:: nCells,nSoilLevels
 integer,dimension(:),pointer:: ivgtyp,isltyp

 real(kind=RKIND),dimension(:),pointer:: snoalb,snow,snowh
 real(kind=RKIND),dimension(:,:),pointer:: tslb,smois,sh2o


!local variables:
 logical,parameter:: fndsnowh = .true.

 integer:: iCell,mosaic_i,NumPairs,lastswap,ice,soil_k,i,temp2,temp5
 integer:: errflag,ns

 real(kind=RKIND):: bx,fk,smcmax,psisat,free,xice_threshold,temp,temp4,temp6
 real(kind=RKIND),parameter:: blim = 5.5
 real(kind=RKIND),parameter:: hlice = 3.335e5
 real(kind=RKIND),parameter:: t0 = 273.15

!-------------------------------------------------
! Noah-mosaic related variables are added to declaration  (danli)
!-------------------------------------------------
!local pointers::
  logical,pointer:: config_surface_mosaic 
  integer,pointer:: config_mosaic_cat
  logical,pointer:: config_frac_seaice
  integer,pointer:: nLandCat

  real(kind=RKIND),dimension(:,:),pointer:: landusef
  real(kind=RKIND),dimension(:,:,:),pointer:: landusef_p
  real(kind=RKIND),dimension(:),pointer:: skintemp
  real(kind=RKIND),dimension(:),pointer:: snowc
  real(kind=RKIND),dimension(:),pointer:: canwat
  real(kind=RKIND),dimension(:),pointer:: xland
  real(kind=RKIND),dimension(:),pointer:: xice
  real(kind=RKIND),dimension(:,:),pointer:: landusef2,TSK_mosaic, CANWAT_mosaic, SNOW_mosaic,SNOWH_mosaic, SNOWC_mosaic, &
                                            ALBEDO_mosaic,ALBBCK_mosaic, EMISS_mosaic, EMBCK_mosaic, ZNT_mosaic, Z0_mosaic
  real(kind=RKIND),dimension(:,:),pointer:: TSLB_mosaic,SMOIS_mosaic,SH2O_mosaic,TRL_URB3D_mosaic, TBL_URB3D_mosaic, TGL_URB3D_mosaic

  real(kind=RKIND),dimension(:,:),pointer:: TR_URB2D_mosaic, TB_URB2D_mosaic,TG_URB2D_mosaic, TC_URB2D_mosaic,QC_URB2D_mosaic, &
                                            SH_URB2D_mosaic,LH_URB2D_mosaic, G_URB2D_mosaic,RN_URB2D_mosaic,TS_URB2D_mosaic, TS_RUL2D_mosaic
  integer,dimension(:,:),pointer:: mosaic_cat_index
      
!-----------------------------------------------------------------------------------------------------------------

 call mpas_pool_get_array(sfc_input,'mminlu'              ,mminlu                        )
 call mpas_pool_get_config(configs,'input_soil_data'      ,mminsl                        )
 call mpas_pool_get_config(configs,'config_sfc_snowalbedo',input_sfc_albedo              )
 call mpas_pool_get_config(configs,'config_do_restart'    ,restart                       )
 call mpas_pool_get_config(configs,'config_surface_mosaic',config_surface_mosaic         ) !for lsm_mosaic_init (PCC)
 call mpas_pool_get_config(configs,'config_mosaic_cat'    ,config_mosaic_cat             ) !for lsm_mosaic_init (PCC)  Number of mosaic tiles
 call mpas_pool_get_config(configs,'config_frac_seaice'   ,config_frac_seaice            ) !for lsm_mosaic_init (PCC)
 call mpas_pool_get_dimension(mesh,'nCells'     ,nCells     )
 call mpas_pool_get_dimension(mesh,'nSoilLevels',nSoilLevels)
 call mpas_pool_get_dimension(mesh,'nLandCat',nLandCat)

 call mpas_pool_get_array(sfc_input,'isltyp'    , isltyp    )
 call mpas_pool_get_array(sfc_input,'ivgtyp'    , ivgtyp    )
 call mpas_pool_get_array(sfc_input,'sh2o'      , sh2o      )
 call mpas_pool_get_array(sfc_input,'smois'     , smois     )
 call mpas_pool_get_array(sfc_input,'tslb'      , tslb      )
 call mpas_pool_get_array(sfc_input,'snoalb'    , snoalb    )
 call mpas_pool_get_array(sfc_input,'snow'      , snow      )
 call mpas_pool_get_array(sfc_input,'snowh'     , snowh     )
 call mpas_pool_get_array(sfc_input,'skintemp'  , skintemp  )  !input...for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'landusef'  , landusef  )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'snowc'     , snowc     )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'xland'     , xland     )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'xice'      , xice      )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(diag_physics,'canwat' , canwat    )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'landusef2' , landusef2 )  !output....for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'TSK_mosaic' , TSK_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'CANWAT_mosaic' , CANWAT_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'SNOW_mosaic' , SNOW_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'SNOWH_mosaic' , SNOWH_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'SNOWC_mosaic' , SNOWC_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'ALBEDO_mosaic' , ALBEDO_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'ALBBCK_mosaic' , ALBBCK_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'EMISS_mosaic' , EMISS_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'EMBCK_mosaic' , EMBCK_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'ZNT_mosaic' , ZNT_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'Z0_mosaic' , Z0_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'TSLB_mosaic' , TSLB_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'SMOIS_mosaic' , SMOIS_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'SH2O_mosaic' , SH2O_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'TRL_URB3D_mosaic' , TRL_URB3D_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'TBL_URB3D_mosaic' , TBL_URB3D_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'TGL_URB3D_mosaic' , TGL_URB3D_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'TR_URB2D_mosaic' , TR_URB2D_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'TB_URB2D_mosaic' , TB_URB2D_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'TG_URB2D_mosaic' , TG_URB2D_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'TC_URB2D_mosaic' , TC_URB2D_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'QC_URB2D_mosaic' , QC_URB2D_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'SH_URB2D_mosaic' , SH_URB2D_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'LH_URB2D_mosaic' , LH_URB2D_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'G_URB2D_mosaic' , G_URB2D_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'RN_URB2D_mosaic' , RN_URB2D_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'TS_URB2D_mosaic' , TS_URB2D_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'TS_RUL2D_mosaic' , TS_RUL2D_mosaic )  !for lsm_mosaic_init (PCC)
 call mpas_pool_get_array(sfc_input,'mosaic_cat_index' , mosaic_cat_index )  !for lsm_mosaic_init (PCC)

 
 

!reads the NOAH LSM tables:
 write(0,*)
 call physics_message('--- initialize NOAH LSM tables' )
 call soil_veg_gen_parm(dminfo,mminlu,mminsl)
 call physics_message('--- end initialize NOAH LSM tables' )
 write(0,*)

 if(.not.restart) then

    errflag = 0
    do iCell = 1, nCells
       if(isltyp(iCell) .lt. 1) then
          errflag = 1
          write(err_message,*) "modunumle_sf_noahlsm.F: lsminit: out of range ISLTYP ", &
                               iCell,isltyp(iCell)
          call physics_message(err_message)
       endif
       if(.not. input_sfc_albedo) snoalb(iCell) = maxalb(ivgtyp(iCell))*0.01
    enddo
    if(errflag .eq. 1) &
       call physics_error_fatal("module_sf_noahlsm.F: lsminit: out of range value "// &
                            "of ISLTYP. Is this field in the input?" )

!initializes soil liquid water content SH2O:
    do iCell = 1, nCells
       bx = bb(isltyp(iCell))
       smcmax = maxsmc(isltyp(iCell))
       psisat = satpsi(isltyp(iCell))
       if((bx > 0.0).and.(smcmax > 0.0).and.(psisat > 0.0)) then
          do ns = 1, nSoilLevels
! ----------------------------------------------------------------------
!SH2O  <= SMOIS for T < 273.149K (-0.001C)
             if(tslb(ns,iCell) < 273.149) then
! ----------------------------------------------------------------------
! first guess following explicit solution for Flerchinger Eqn from Koren
! et al, JGR, 1999, Eqn 17 (KCOUNT=0 in FUNCTION FRH2O).
! ISLTPK is soil type
                bx = bb(isltyp(iCell))
                smcmax = maxsmc(isltyp(iCell))
                psisat = satpsi(isltyp(iCell))
                if(bx >  blim) bx = blim
                fk = (((hlice/(gravity*(-psisat))) * &
                     ((tslb(ns,iCell)-t0)/tslb(ns,iCell)) )**(-1/bx) )*smcmax
                if (fk < 0.02) fk = 0.02
                sh2o(ns,iCell) = min(fk,smois(ns,iCell))
! ----------------------------------------------------------------------
! now use iterative solution for liquid soil water content using
! FUNCTION FRH2O with the initial guess for SH2O from above explicit
! first guess.
                call frh2o(free,tslb(ns,iCell),smois(ns,iCell),sh2o(ns,iCell), &
                           smcmax,bx,psisat)
                sh2o(ns,iCell) = free
             else         ! of if (tslb(i,ns,j)
! ----------------------------------------------------------------------
! SH2O = SMOIS ( for T => 273.149K (-0.001C)
                sh2o(ns,iCell)=smois(ns,iCell)
! ----------------------------------------------------------------------
             endif        ! of if (tslb(i,ns,j)
          enddo
       else                  ! of if ((bx > 0.0)
          do ns = 1, nSoilLevels
             sh2o(ns,iCell)=smois(ns,iCell)
          enddo
       endif ! of if ((bx > 0.0)
    enddo    ! do iCell

!initialize physical snow height SNOWH:
    if(.not.fndsnowh)then
!if no snowh do the following:
       call physics_message( 'SNOW HEIGHT NOT FOUND - VALUE DEFINED IN LSMINIT' )
       do iCell = 1, nCells
          snowh(iCell)=snow(iCell)*0.005 ! snow in mm and snowh in m
       enddo
    endif
    110 continue

!    write(*,*), '--- snowh = (Noah LSM initialize)', snowh
!    write(*,*), '--- sh2o = (Noah LSM initialize)', sh2o
!    write(*,*), '--- tslb = (Noah LSM initialize)', tslb
 IF (config_surface_mosaic) THEN   !lsm_mosaic_init (pcc)

! write(*,*),'--- assign NOAH LSM mosaic from tables, config_surface_mosaic = ',  config_surface_mosaic
! write(*,*),'--- config_frac_seaice= ', config_frac_seaice
 
if(.not. config_frac_seaice) then
    xice_threshold = 0.5_RKIND
 elseif(config_frac_seaice) then
    xice_threshold = 0.02
 endif
 !write(0,*) '--- config_frac_seaice      :', config_frac_seaice
 !write(0,*) '--- xice_threshold          :', xice_threshold


  !===========================================================================   
  ! CHOOSE THE TILES
  !===========================================================================  
  
!  itf=min0(ite,ide-1)
!  jtf=min0(jte,jde-1)

  ! simple test
!  write(*,*),'--- simple test'
  DO iCell = 1, nCells
        IF ((xland(iCell).LT. 1.5 ) .AND. (IVGTYP(iCell) .EQ. ISWATER)) THEN
           PRINT*, 'BEFORE MOSAIC_INIT'
           CALL physics_message("BEFORE MOSAIC_INIT")
           WRITE(err_message,fmt='(a,I6,F8.2,F8.2,I6,I6)') 'iCell,xland,xice,mosaic_cat_index,ivgtyp = ', &
                 iCell,xland(iCell),xice(iCell),nLandCat,IVGTYP(iCell)
           CALL physics_message(err_message)
        ENDIF
  ENDDO

     DO  iCell = 1, nCells
           DO mosaic_i=1,nLandCat
              LANDUSEF2(mosaic_i,iCell)=LANDUSEF(mosaic_i,iCell)
              mosaic_cat_index(mosaic_i,iCell)=mosaic_i
           ENDDO

     ENDDO
      
       DO iCell = 1, nCells
       
          NumPairs=nLandCat-1
          
          DO
               IF (NumPairs == 0) EXIT
                   LastSwap = 1
          DO  mosaic_i=1, NumPairs
            IF(LANDUSEF2(mosaic_i,iCell) < LANDUSEF2(mosaic_i+1,iCell)  ) THEN
               Temp = LANDUSEF2(mosaic_i,iCell)
               LANDUSEF2(mosaic_i,iCell)=LANDUSEF2(mosaic_i+1,iCell)
               LANDUSEF2(mosaic_i+1,iCell)=Temp            
               LastSwap = mosaic_i 
            
               Temp2 =  mosaic_cat_index(mosaic_i,iCell)
               mosaic_cat_index(mosaic_i,iCell)=mosaic_cat_index(mosaic_i+1,iCell)
               mosaic_cat_index(mosaic_i+1,iCell)=Temp2
            ENDIF
          ENDDO
               NumPairs = LastSwap - 1
          ENDDO
          
      ENDDO
 
  !===========================================================================   
  ! For non-seaice grids, eliminate the seaice-tiles
  !=========================================================================== 

     DO iCell = 1, nCells
        
         IF   (XLAND(iCell).LT.1.5)  THEN

             ICE = 0
                 IF( XICE(iCell).GE. XICE_THRESHOLD ) THEN
                   WRITE (err_message,fmt='(a,2I5)') 'sea-ice at point, iCell = ', iCell
                   CALL physics_message(err_message)
                 ICE = 1
                 ENDIF   
           
          IF (ICE == 1)   Then         ! sea-ice case , eliminate sea-ice if they are not the dominant ones

          IF (IVGTYP(iCell) == isice)  THEN    ! if this grid cell is dominanted by ice, then do nothing

          ELSE

                DO mosaic_i=2,config_mosaic_cat
                   IF (mosaic_cat_index(mosaic_i,iCell) == isice ) THEN
                       Temp4=LANDUSEF2(mosaic_i,iCell)
                       Temp5=mosaic_cat_index(mosaic_i,iCell)

                       LANDUSEF2(mosaic_i:nLandCat-1,iCell)=LANDUSEF2(mosaic_i+1:nLandCat,iCell)                       
                       mosaic_cat_index(mosaic_i:nLandCat-1,iCell)=mosaic_cat_index(mosaic_i+1:nLandCat,iCell)

                       LANDUSEF2(nLandCat,iCell)=Temp4
                       mosaic_cat_index(nLandCat,iCell)=Temp5
                   ENDIF 
                 ENDDO

          ENDIF   ! for (IVGTYP(i,j) == isice )
          
          ELSEIF (ICE ==0)  THEN
          
          IF ((mosaic_cat_index(1,iCell) .EQ. ISWATER)) THEN    
          
          ! xland < 1.5 but the dominant land use category based on our calculation is water
	             
           IF (IVGTYP(iCell) .EQ. ISWATER) THEN  
           
           ! xland < 1.5 but the dominant land use category based on the geogrid calculation is water, this must be wrong
           
              CALL physics_message("IN MOSAIC_INIT")
              WRITE(err_message,fmt='(a,I6,I6,F8.2,F8.2)') 'iCell,IVGTYP,XLAND,XICE = ',iCell,IVGTYP(iCell),xland(iCell),xice(iCell) 
              CALL physics_message(err_message)
              CALL physics_message("xland < 1.5 but the dominant land use category based on our calculation is water."//&
                   "In addition, the dominant land use category based on the geogrid calculation is water, this must be wrong")
           
           ENDIF  ! for (IVGTYP(i,j) .EQ. ISWATER)
           
           IF (IVGTYP(iCell) .NE. ISWATER) THEN 
           
           ! xland < 1.5,   the dominant land use category based on our calculation is water, but based on the geogrid calculation is not water, which might be due to the inconsistence between land use data and land-sea mask
           
	       Temp4=LANDUSEF2(1,iCell)
	       Temp5=mosaic_cat_index(1,iCell)
	  
	       LANDUSEF2(1:nLandCat-1,iCell)=LANDUSEF2(2:nLandCat,iCell)                       
	       mosaic_cat_index(1:nLandCat-1,iCell)=mosaic_cat_index(2:nLandCat,iCell)
	  
	       LANDUSEF2(nLandCat,iCell)=Temp4
	       mosaic_cat_index(nLandCat,iCell)=Temp5
	             
              CALL physics_message("IN MOSAIC_INIT")
              WRITE(err_message,fmt='(a,I6,I6,F8.2,F8.2)') 'iCell,IVGTYP,XLAND,XICE = ',iCell,IVGTYP(iCell),xland(iCell),xice(iCell) 
              CALL physics_message(err_message)
              CALL physics_message("xland < 1.5 but the dominant land use category based on our calculation is water."//&
                   "this is fine as long as we change our calculation so that the dominant land use category is"//&
                   "stwiched back to not water.")
              WRITE(err_message,fmt='(a,I6,I6)') 'land use category has been switched, before and after values are ', &
                   temp5,mosaic_cat_index(1,iCell)
              CALL physics_message(err_message)
              WRITE(err_message,fmt='(a,I6,I6)') 'new dominant and second dominant cat are ', mosaic_cat_index(1,iCell),mosaic_cat_index(2,iCell)
              CALL physics_message(err_message)
	             
           ENDIF  ! for (IVGTYP(iCell) .NE. ISWATER)
          
           ELSE    !  for (mosaic_cat_index(1,iCell) .EQ. ISWATER)
           
                     DO mosaic_i=2,config_mosaic_cat
	                IF (mosaic_cat_index(mosaic_i,iCell) == iswater ) THEN
	                   Temp4=LANDUSEF2(mosaic_i,iCell)
	                   Temp5=mosaic_cat_index(mosaic_i,iCell)
	   
	                   LANDUSEF2(mosaic_i:nLandCat-1,iCell)=LANDUSEF2(mosaic_i+1:nLandCat,iCell)                       
	                   mosaic_cat_index(mosaic_i:nLandCat-1,iCell)=mosaic_cat_index(mosaic_i+1:nLandCat,iCell)
	  
	                   LANDUSEF2(nLandCat,iCell)=Temp4
	                   mosaic_cat_index(nLandCat,iCell)=Temp5
	                ENDIF 
	              ENDDO
             
           ENDIF !  for (mosaic_cat_index(1,iCell) .EQ. ISWATER)
             
          ENDIF  !  for ICE == 1
          
      ELSE  ! FOR (XLAND(iCell).LT.1.5)
      
                 ICE = 0
      
                     IF( XICE(iCell).GE. XICE_THRESHOLD ) THEN
                       WRITE (err_message,fmt='(a,2I6)') 'sea-ice at water point, iCell = ', iCell
                       CALL physics_message(err_message)
                       ICE = 1
                     ENDIF  
      
           IF ((mosaic_cat_index(1,iCell) .NE. ISWATER)) THEN    
                
                ! xland > 1.5 and the dominant land use category based on our calculation is not water
      	             
                 IF (IVGTYP(iCell) .NE. ISWATER) THEN  
                 
                 ! xland > 1.5 but the dominant land use category based on the geogrid calculation is not water, this must be wrong
                 CALL physics_message("IN MOSAIC_INIT")
                 WRITE(err_message,fmt='(a,I6,I6,F8.2,F8.2,I6,I6,I6)') 'iCell,IVGTYP,XLAND,XICE,ISWATER,ISICE,mosaic_cat_index = ',iCell,IVGTYP(iCell),xland(iCell),xice(iCell),iswater,isice,mosaic_cat_index(1,iCell)
                 CALL physics_message(err_message)
                 CALL physics_message("xland > 1.5 but the dominant land use category based on our calculation is not water."// &
                      "in addition, the dominant land use category based on the geogrid calculation is not water,"//  &
                      "this must be wrong.")
                 ENDIF  ! for (IVGTYP(iCell) .NE. ISWATER)
                 
                 IF (IVGTYP(iCell) .EQ. ISWATER) THEN 
                 
                 ! xland > 1.5,   the dominant land use category based on our calculation is not water, but based on the geogrid calculation is water, which might be due to the inconsistence between land use data and land-sea mask

                 CALL physics_message("IN MOSAIC_INIT")
                 WRITE(err_message,fmt='(a,I6,I6,F8.2,F8.2,I6,I6,I6)') 'iCell,IVGTYP,XLAND,XICE,ISWATER,ISICE,mosaic_cat_index = ',iCell,IVGTYP(iCell),xland(iCell),xice(iCell),iswater,isice,mosaic_cat_index(1,iCell)
                 CALL physics_message(err_message)
                 CALL physics_message("xland > 1.5 but the dominant land use category based on our calculation is not water."// &
                      "however, the dominant land use category based on the geogrid calculation is water")
                 CALL physics_message("This is fine. We do not need to do anyting because in the noaddrv, "//&
                      "we use xland as a criterion for whether using"// &
                      "mosaic or not when xland > 1.5, no mosaic will be used anyway")
      	             
                 ENDIF  ! for (IVGTYP(iCell) .NE. ISWATER)
                
           ENDIF !  for (mosaic_cat_index(1,iCell) .NE. ISWATER)
      
        ENDIF  ! FOR (XLAND(iCell).LT.1.5)

      ENDDO
      
!      write(*,*),'--- normalize NOAH LSM mosaic' 
  !===========================================================================   
  ! normalize
  !=========================================================================== 

     DO iCell = 1, nCells

          Temp6=0

            DO mosaic_i=1,config_mosaic_cat
               Temp6=Temp6+LANDUSEF2(mosaic_i,iCell)
            ENDDO
            
            if (Temp6 .LT. 1e-5)  then
            
            Temp6 = 1e-5
            WRITE (err_message,fmt='(a,e8.1)') 'the total land surface fraction is less than ', temp6
            CALL physics_message(err_message)
            WRITE (err_message,fmt='(a,2I6,4F8.2)') 'some landusef values at iCell are ', &
                 iCell,landusef2(1,iCell),landusef2(2,iCell),landusef2(3,iCell),landusef2(4,iCell)
            CALL physics_message(err_message)
            WRITE (err_message,fmt='(a,2I6,3I6)') 'some mosaic cat values at iCell are ', &
                 iCell,mosaic_cat_index(1,iCell),mosaic_cat_index(2,iCell),mosaic_cat_index(3,iCell) 
            CALL physics_message(err_message)
            
            endif

            LANDUSEF2(1:config_mosaic_cat,iCell)=LANDUSEF2(1:config_mosaic_cat,iCell)*(1/Temp6)

      ENDDO

!      write(*,*),'--- initialize the NOAH LSM mosaic' 
  !===========================================================================   
  ! initilize the variables
  !===========================================================================   

     DO iCell = 1, nCells
    
             DO mosaic_i=1,config_mosaic_cat
        
            TSK_mosaic(mosaic_i,iCell)=skintemp(iCell)          
            CANWAT_mosaic(mosaic_i,iCell)=CANWAT(iCell) 
            SNOW_mosaic(mosaic_i,iCell)=SNOW(iCell) 
            SNOWH_mosaic(mosaic_i,iCell)=SNOWH(iCell)  
            SNOWC_mosaic(mosaic_i,iCell)=SNOWC(iCell) 
            ALBEDO_mosaic(mosaic_i,iCell)=albedomaxtbl(ivgtyp(iCell))
            ALBBCK_mosaic(mosaic_i,iCell)= albedomaxtbl(ivgtyp(iCell))
            EMISS_mosaic(mosaic_i,iCell)= emissmaxtbl(ivgtyp(iCell))
            EMBCK_mosaic(mosaic_i,iCell)= emissmaxtbl(ivgtyp(iCell))
            ZNT_mosaic(mosaic_i,iCell)=z0maxtbl(ivgtyp(iCell))
            Z0_mosaic(mosaic_i,iCell)=z0maxtbl(ivgtyp(iCell))
              DO soil_k=1,nSoilLevels 
  
              
             TSLB_mosaic(nSoilLevels*(mosaic_i-1)+soil_k,iCell)=TSLB(soil_k,iCell)
             SMOIS_mosaic(nSoilLevels*(mosaic_i-1)+soil_k,iCell)=SMOIS(soil_k,iCell)
             SH2O_mosaic(nSoilLevels*(mosaic_i-1)+soil_k,iCell)=SH2O(soil_k,iCell)

         
              ENDDO
           
           TR_URB2D_mosaic(mosaic_i,iCell)=skintemp(iCell)   
           TB_URB2D_mosaic(mosaic_i,iCell)=skintemp(iCell)   
           TG_URB2D_mosaic(mosaic_i,iCell)=skintemp(iCell)   
           TC_URB2D_mosaic(mosaic_i,iCell)=skintemp(iCell)   
           TS_URB2D_mosaic(mosaic_i,iCell)=skintemp(iCell)   
           TS_RUL2D_mosaic(mosaic_i,iCell)=skintemp(iCell)   
           QC_URB2D_mosaic(mosaic_i,iCell)=0.01
           SH_URB2D_mosaic(mosaic_i,iCell)=0
           LH_URB2D_mosaic(mosaic_i,iCell)=0
           G_URB2D_mosaic(mosaic_i,iCell)=0
           RN_URB2D_mosaic(mosaic_i,iCell)=0
          
          TRL_URB3D_mosaic(4*(mosaic_i-1)+1,iCell)=TSLB(1,iCell)+0.
          TRL_URB3D_mosaic(4*(mosaic_i-1)+2,iCell)=0.5*(TSLB(1,iCell)+TSLB(2,iCell))
          TRL_URB3D_mosaic(4*(mosaic_i-1)+3,iCell)=TSLB(2,iCell)+0.
          TRL_URB3D_mosaic(4*(mosaic_i-1)+4,iCell)=TSLB(2,iCell)+(TSLB(3,iCell)-TSLB(2,iCell))*0.29

          TBL_URB3D_mosaic(4*(mosaic_i-1)+1,iCell)=TSLB(1,iCell)+0.
          TBL_URB3D_mosaic(4*(mosaic_i-1)+2,iCell)=0.5*(TSLB(1,iCell)+TSLB(2,iCell))
          TBL_URB3D_mosaic(4*(mosaic_i-1)+3,iCell)=TSLB(2,iCell)+0.
          TBL_URB3D_mosaic(4*(mosaic_i-1)+4,iCell)=TSLB(2,iCell)+(TSLB(3,iCell)-TSLB(2,iCell))*0.29           
                    
          TGL_URB3D_mosaic(4*(mosaic_i-1)+1,iCell)=TSLB(1,iCell)
          TGL_URB3D_mosaic(4*(mosaic_i-1)+2,iCell)=TSLB(2,iCell)
          TGL_URB3D_mosaic(4*(mosaic_i-1)+3,iCell)=TSLB(3,iCell)
          TGL_URB3D_mosaic(4*(mosaic_i-1)+4,iCell)=TSLB(4,iCell)
         
          
            ENDDO
      ENDDO
!write(*,*),'--- Done initialize the NOAH LSM mosaic'
!write(*,*),'--- TSK Mosaic= ', TSK_mosaic
!write(*,*),'--- CANWAT Mosaic= ', CANWAT_mosaic
!write(*,*),'--- Albedo Mosaic= ', ALBEDO_mosaic
!write(*,*),'--- TSLB Mosaic= ', TSLB_mosaic
!write(*,*),'--- TR_URB2D Mosaic= ', TR_URB2D_mosaic
!write(*,*),'--- TRL_URB3D Mosaic= ', TRL_URB3D_mosaic
   ! simple test
   
       DO iCell = 1, nCells
   
           IF ((xland(iCell).LT. 1.5 ) .AND. (mosaic_cat_index(1,iCell) .EQ. ISWATER)) THEN
             CALL physics_message("After MOSAIC_INIT")
             WRITE (err_message,fmt='(a,I6,F8.2,F8.2,I6,I6)') 'weird xland,xice,mosaic_cat_index and ivgtyp at iCell = ', &
                iCell,xland(iCell),xice(iCell),mosaic_cat_index(1,iCell),IVGTYP(iCell)
             CALL physics_message(err_message)
           ENDIF
           
      ENDDO

          ENDIF  ! For config_lsm_mosaic


 endif !for not restart
!write(*,*),'--- Leaving LSM initialization'
 end subroutine lsminit

!=================================================================================================================
 subroutine soil_veg_gen_parm(dminfo,mminlu,mminsl)
!=================================================================================================================

!input arguments:
 type(dm_info),intent(in):: dminfo
 character(len=*),intent(inout):: mminlu, mminsl

!local variables:
 character*128:: mess , message

 integer,parameter:: open_ok = 0
 integer:: lumatch,iindex,lc,num_slope
 integer:: istat

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
!write(0,*)
!write(0,*) '       enter subroutine soil_veg_gen_parm:'

!read in the vegetation properties from vegparm.tbl:

 if(dminfo % my_proc_id == IO_NODE) then
    open(16,file='VEGPARM.TBL',form='FORMATTED',status='OLD',iostat=istat)
    if(istat /= open_ok) &
       call physics_error_fatal('subroutine soil_veg_gen_arm: ' // &
                                'failure opening VEGPARM.TBL')

    lumatch=0
    find_lutype : do while (lumatch == 0)
       read(16,*,end=2002)
       read(16,*,end=2002) lutype
       read(16,*) lucats,iindex

       if(lutype.eq.trim(mminlu))then
          write(mess,*) '   landuse type = ' // trim ( lutype ) // ' found', &
                        lucats, ' categories'
          call physics_message(mess)
          lumatch=1
       else
          call physics_message('    skipping over lutype = ' // trim ( lutype ))
          do lc = 1, lucats+12
             read(16,*)
          enddo
       endif
    enddo find_lutype

!prevent possible array overwrite, Bill Bovermann, IBM, May 6, 2008:
    if(size(shdtbl)       < lucats .or. &
       size(nrotbl)       < lucats .or. &
       size(rstbl)        < lucats .or. &
       size(rgltbl)       < lucats .or. &
       size(hstbl)        < lucats .or. &
       size(snuptbl)      < lucats .or. &
       size(maxalb)       < lucats .or. &
       size(laimintbl)    < lucats .or. &
       size(laimaxtbl)    < lucats .or. &
       size(z0mintbl)     < lucats .or. &
       size(z0maxtbl)     < lucats .or. &
       size(albedomintbl) < lucats .or. &
       size(albedomaxtbl) < lucats .or. &
       size(emissmintbl ) < lucats .or. &
       size(emissmaxtbl ) < lucats) then
!      call wrf_error_fatal('table sizes too small for value of lucats in module_sf_noahdrv.f')
    endif

    if(lutype.eq.mminlu)then
       do lc = 1, lucats
          read(16,*) iindex,shdtbl(lc),nrotbl(lc),rstbl(lc),rgltbl(lc),hstbl(lc),snuptbl(lc), &
                     maxalb(lc),laimintbl(lc),laimaxtbl(lc),emissmintbl(lc),emissmaxtbl(lc),  &
                     albedomintbl(lc),albedomaxtbl(lc),z0mintbl(lc),z0maxtbl(lc)
       enddo
       read (16,*)
       read (16,*)topt_data
       read (16,*)
       read (16,*)cmcmax_data
       read (16,*)
       read (16,*)cfactr_data
       read (16,*)
       read (16,*)rsmax_data
       read (16,*)
       read (16,*)bare
       read (16,*)
       read (16,*)natural
    endif

    2002 continue
    close (16)
    if(lumatch == 0) &
       call physics_error_fatal ('land use dataset '''//mminlu//''' not found in VEGPARM.TBL.')
      
 endif ! end dminfo

!distribute data to all processors: 
 DM_BCAST_CHAR(lutype)
 DM_BCAST_INTEGER(lucats)
 DM_BCAST_INTEGER(iindex)
 DM_BCAST_INTEGER(lumatch)
 DM_BCAST_REALS(shdtbl)
 DM_BCAST_INTEGERS(nrotbl)
 DM_BCAST_REALS(rstbl)
 DM_BCAST_REALS(rgltbl)
 DM_BCAST_REALS(hstbl)
 DM_BCAST_REALS(snuptbl)
 DM_BCAST_REALS(laimintbl)
 DM_BCAST_REALS(laimaxtbl)
 DM_BCAST_REALS(z0mintbl)
 DM_BCAST_REALS(z0maxtbl)
 DM_BCAST_REALS(emissmintbl)
 DM_BCAST_REALS(emissmaxtbl)
 DM_BCAST_REALS(albedomintbl)
 DM_BCAST_REALS(albedomaxtbl)
 DM_BCAST_REALS(maxalb)
 DM_BCAST_REAL(topt_data)
 DM_BCAST_REAL(cmcmax_data)
 DM_BCAST_REAL(cfactr_data)
 DM_BCAST_REAL(rsmax_data)
 DM_BCAST_INTEGER(bare)
 DM_BCAST_INTEGER(natural)

!write(0,*) ' LUTYPE  = ',trim(lutype)
!write(0,*) ' LUCATS  = ',lucats
!write(0,*) ' IINDEX  = ',iindex
!write(0,*) ' LUMATCH = ',lumatch

!write(0,*) ' TOPT_DATA   = ',topt_data
!write(0,*) ' CMCMAX_DATA = ',cmcmax_data
!write(0,*) ' CFACTR_DATA = ',cfactr_data
!write(0,*) ' RSMAX_DATA  = ',rsmax_data
!write(0,*) ' BARE        = ',bare
!write(0,*) ' NATURAL     = ',natural

!write(0,*)
!do lc = 1, lucats
!   write(0,101) lc,shdtbl(lc),float(nrotbl(lc)),rstbl(lc),rgltbl(lc),hstbl(lc),snuptbl(lc), &
!                maxalb(lc),laimintbl(lc),laimaxtbl(lc),emissmintbl(lc),emissmaxtbl(lc),     &
!                albedomintbl(lc),albedomaxtbl(lc),z0mintbl(lc),z0maxtbl(lc)
!enddo
!101 format(i4,15(3x,f9.5))
 write(0,*) '    end read VEGPARM.TBL'

!read in soil properties from soilparm.tbl:

 if(dminfo % my_proc_id == IO_NODE) then
    open(16,file='SOILPARM.TBL',form='FORMATTED',status='OLD',iostat=istat)
    if(istat /= open_ok) &
       call physics_error_fatal('module_sf_noahlsm.F: soil_veg_gen_parm: ' // &
                                'failure opening SOILPARM.TBL' )

    write(mess,*) '   input soil texture classification = ', trim (mminsl)
    call physics_message(mess)

    lumatch=0
    read(16,*)
    read(16,2000,end=2003) sltype
    2000 format(a4)
    read(16,*)slcats,iindex
    if(sltype.eq.mminsl)then
       write(mess,*) '   soil texture classification = ', trim ( sltype ) , ' found', &
                  slcats,' categories'
       call physics_message ( mess )
       lumatch=1
    endif

!prevent possible array overwrite, Bill Bovermann, IBM, May 6, 2008:
    if(size(bb) < slcats .or. &
       size(drysmc) < slcats .or. &
       size(f11   ) < slcats .or. &
       size(maxsmc) < slcats .or. &
       size(refsmc) < slcats .or. &
       size(satpsi) < slcats .or. &
       size(satdk ) < slcats .or. &
       size(satdw ) < slcats .or. &
       size(wltsmc) < slcats .or. &
       size(qtz   ) < slcats) then
!      call wrf_error_fatal('table sizes too small for value of slcats in module_sf_noahdrv.f')
    endif
    if(sltype.eq.mminsl) then
       do lc = 1, slcats
          read(16,*) iindex,bb(lc),drysmc(lc),f11(lc),maxsmc(lc),refsmc(lc),satpsi(lc), &
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
 DM_BCAST_REALS(f11)
 DM_BCAST_REALS(maxsmc)
 DM_BCAST_REALS(refsmc)
 DM_BCAST_REALS(satpsi)
 DM_BCAST_REALS(satdk)
 DM_BCAST_REALS(satdw)
 DM_BCAST_REALS(wltsmc)
 DM_BCAST_REALS(qtz)

!write(0,*) ' LUMATCH=',lumatch
!write(0,*) ' SLTYPE =',trim(sltype)
!write(0,*) ' MMINSL =',mminsl
!write(0,*) ' SLCATS =',slcats
!write(0,*) ' IINDEX =',iindex
!write(0,*)
!do lc = 1, slcats
!   write(0,101) lc,bb(lc),drysmc(lc),f11(lc),maxsmc(lc),refsmc(lc),satpsi(lc), &
!                satdk(lc),satdw(lc),wltsmc(lc),qtz(lc)
!enddo
 write(0,*) '    end read SOILPARM.TBL'

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
    read(16,*)
    read(16,*)lvcoef_data
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
 DM_BCAST_REAL(lvcoef_data)

!write(0,*)
!write(mess,*) 'input general parameters'
!call physics_message(mess)
!write(0,*) 'NUM_SLOPE=',num_slope
!do lc = 1, slpcats
!   write(0,101) lc,slope_data(lc)
!enddo
!write(0,*) '       end subroutine soil_veg_gen_parm:'
 write(0,*) '    end read GENPARM.TBL'

 end subroutine soil_veg_gen_parm

!=================================================================================================================
 end module mpas_atmphys_lsm_noahinit
!=================================================================================================================
