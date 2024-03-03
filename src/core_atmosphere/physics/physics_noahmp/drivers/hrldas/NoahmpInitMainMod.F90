module NoahmpInitMainMod

!!!  Module to initialize Noah-MP 2-D variables

  use Machine
  use NoahmpIOVarType
  use NoahmpSnowInitMod
 
  implicit none
  
contains

  subroutine NoahmpInitMain(NoahmpIO)

! ------------------------ Code history -------------------------------------
! Original Noah-MP subroutine: NOAHMP_INIT
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ---------------------------------------------------------------------------

    implicit none 
   
    type(NoahmpIO_type), intent(inout) :: NoahmpIO

    ! local variables
    integer                                     :: ide,jde,its,jts,itf,jtf
    integer                                     :: I,J,errflag,NS
    logical                                     :: urbanpt_flag
    real(kind=kind_noahmp)                      :: BEXP, SMCMAX, PSISAT, FK
    real(kind=kind_noahmp), parameter           :: BLIM  = 5.5
    real(kind=kind_noahmp), parameter           :: HLICE = 3.335E5
    real(kind=kind_noahmp), parameter           :: GRAV0 = 9.81
    real(kind=kind_noahmp), parameter           :: T0    = 273.15
! --------------------------------------------------------------------------- 

    ! initialize
    ide = NoahmpIO%ide+1 
    jde = NoahmpIO%jde+1 
    its = NoahmpIO%its
    jts = NoahmpIO%jts

    ! only initialize for non-restart case
    if ( .not. NoahmpIO%restart_flag ) then

       itf = min0(NoahmpIO%ite, ide-1)
       jtf = min0(NoahmpIO%jte, jde-1)

       ! initialize physical snow height SNOWH
       if ( .not. NoahmpIO%FNDSNOWH ) then
          ! If no SNOWH do the following
          print*, 'SNOW HEIGHT NOT FOUND - VALUE DEFINED IN LSMINIT'
          do J = jts, jtf
             do I = its, itf
                NoahmpIO%SNOWH(I,J) = NoahmpIO%SNOW(I,J) * 0.005  ! SNOW in mm and SNOWH in m
             enddo
          enddo
       endif
   
       ! Check if snow/snowh are consistent and cap SWE at 2000mm
       ! the Noah-MP code does it internally but if we don't do it here, problems ensue
       do J = jts, jtf
          do I = its, itf
             if ( NoahmpIO%SNOW(I,J)  < 0.0 ) NoahmpIO%SNOW(I,J)  = 0.0 
             if ( NoahmpIO%SNOWH(I,J) < 0.0 ) NoahmpIO%SNOWH(I,J) = 0.0
             if ( (NoahmpIO%SNOW(I,J) > 0.0) .and. (NoahmpIO%SNOWH(I,J) == 0.0) ) &
                NoahmpIO%SNOWH(I,J) = NoahmpIO%SNOW(I,J) * 0.005
             if ( (NoahmpIO%SNOWH(I,J) > 0.0) .and. (NoahmpIO%SNOW(I,J) == 0.0) ) &
                NoahmpIO%SNOW(I,J)  = NoahmpIO%SNOWH(I,J) / 0.005
             if ( NoahmpIO%SNOW(I,J) > 2000.0 ) then
                NoahmpIO%SNOWH(I,J) = NoahmpIO%SNOWH(I,J) * 2000.0 / NoahmpIO%SNOW(I,J)      ! SNOW in mm and SNOWH in m
                NoahmpIO%SNOW (I,J) = 2000.0                                                 ! cap SNOW at 2000, maintain density
             endif
          enddo
       enddo

       ! check soil type
       errflag = 0
       do J = jts, jtf
          do I = its, itf
             if ( NoahmpIO%ISLTYP(I,J) < 1 ) then
                errflag = 1
                write(*,*) "lsminit: out of range ISLTYP ",I,J,NoahmpIO%ISLTYP(I,J)
                stop
             endif
          enddo
       enddo

       ! initialize soil liquid water content SH2O
       do J = jts , jtf
          do I = its , itf
             if ( (NoahmpIO%IVGTYP(I,J) == NoahmpIO%ISICE_TABLE) .and. &
                  (NoahmpIO%XICE(I,J) <= 0.0) ) then
                do NS = 1, NoahmpIO%NSOIL
                   NoahmpIO%SMOIS(I,NS,J) = 1.0  ! glacier starts all frozen
                   NoahmpIO%SH2O(I,NS,J)  = 0.0
                   NoahmpIO%TSLB(I,NS,J)  = min(NoahmpIO%TSLB(I,NS,J), 263.15) ! set glacier temp to at most -10C
                enddo
                ! NoahmpIO%TMN(I,J) = min(NoahmpIO%TMN(I,J), 263.15)           ! set deep temp to at most -10C
                NoahmpIO%SNOW(I,J)  = max(NoahmpIO%SNOW(I,J), 10.0)            ! set SWE to at least 10mm
                NoahmpIO%SNOWH(I,J) = NoahmpIO%SNOW(I,J) * 0.01                ! SNOW in mm and SNOWH in m
             else
                BEXP   = NoahmpIO%BEXP_TABLE  (NoahmpIO%ISLTYP(I,J))
                SMCMAX = NoahmpIO%SMCMAX_TABLE(NoahmpIO%ISLTYP(I,J))
                PSISAT = NoahmpIO%PSISAT_TABLE(NoahmpIO%ISLTYP(I,J))
                do NS = 1, NoahmpIO%NSOIL
                  if ( NoahmpIO%SMOIS(I,NS,J) > SMCMAX ) NoahmpIO%SMOIS(I,NS,J) = SMCMAX
                enddo
                if ( (BEXP > 0.0) .and. (SMCMAX > 0.0) .and. (PSISAT > 0.0) ) then
                   do NS = 1, NoahmpIO%NSOIL
                      if ( NoahmpIO%TSLB(I,NS,J) < 273.149 ) then
                         FK = (((HLICE / (GRAV0*(-PSISAT))) * &
                                ((NoahmpIO%TSLB(I,NS,J)-T0) / NoahmpIO%TSLB(I,NS,J)))**(-1/BEXP))*SMCMAX
                         FK = max(FK, 0.02)
                         NoahmpIO%SH2O(I,NS,J) = min(FK, NoahmpIO%SMOIS(I,NS,J))
                      else
                         NoahmpIO%SH2O(I,NS,J) = NoahmpIO%SMOIS(I,NS,J)
                      endif
                   enddo
                else
                   do NS = 1, NoahmpIO%NSOIL
                      NoahmpIO%SH2O(I,NS,J) = NoahmpIO%SMOIS(I,NS,J)
                   enddo
                endif
             endif
          enddo ! I
       enddo    ! J

       ! initilize other quantities
       do J = jts, jtf
          do I = its, itf
             NoahmpIO%QTDRAIN(I,J)  = 0.0
             NoahmpIO%TVXY(I,J)     = NoahmpIO%TSK(I,J)
             NoahmpIO%TGXY(I,J)     = NoahmpIO%TSK(I,J)
             if ( (NoahmpIO%SNOW(I,J) > 0.0) .and. (NoahmpIO%TSK(i,j) > 273.15) ) NoahmpIO%TVXY(I,J) = 273.15
             if ( (NoahmpIO%SNOW(I,J) > 0.0) .and. (NoahmpIO%TSK(I,J) > 273.15) ) NoahmpIO%TGXY(I,J) = 273.15
             NoahmpIO%CANWAT(I,J)   = 0.0
             NoahmpIO%CANLIQXY(I,J) = NoahmpIO%CANWAT(I,J)
             NoahmpIO%CANICEXY(I,J) = 0.0
             NoahmpIO%EAHXY(I,J)    = 2000.0
             NoahmpIO%TAHXY(I,J)    = NoahmpIO%TSK(I,J)
             NoahmpIO%T2MVXY(I,J)   = NoahmpIO%TSK(I,J)
             NoahmpIO%T2MBXY(I,J)   = NoahmpIO%TSK(I,J)
             if ( (NoahmpIO%SNOW(I,J) > 0.0) .and. (NoahmpIO%TSK(I,J) > 273.15) ) NoahmpIO%TAHXY(I,J)  = 273.15
             if ( (NoahmpIO%SNOW(I,J) > 0.0) .and. (NoahmpIO%TSK(I,J) > 273.15) ) NoahmpIO%T2MVXY(I,J) = 273.15
             if ( (NoahmpIO%SNOW(I,J) > 0.0) .and. (NoahmpIO%TSK(I,J) > 273.15) ) NoahmpIO%T2MBXY(I,J) = 273.15
             NoahmpIO%CMXY(I,J)     = 0.0
             NoahmpIO%CHXY(I,J)     = 0.0
             NoahmpIO%FWETXY(I,J)   = 0.0
             NoahmpIO%SNEQVOXY(I,J) = 0.0
             NoahmpIO%ALBOLDXY(I,J) = 0.65
             NoahmpIO%QSNOWXY(I,J)  = 0.0
             NoahmpIO%QRAINXY(I,J)  = 0.0
             NoahmpIO%WSLAKEXY(I,J) = 0.0
             if ( NoahmpIO%IOPT_RUNSUB /= 5 ) then 
                NoahmpIO%WAXY(I,J)   = 4900.0 
                NoahmpIO%WTXY(I,J)   = NoahmpIO%WAXY(i,j) 
                NoahmpIO%ZWTXY(I,J)  = (25.0 + 2.0) - NoahmpIO%WAXY(i,j)/1000/0.2
             else
                NoahmpIO%WAXY(I,J)   = 0.0
                NoahmpIO%WTXY(I,J)   = 0.0
                NoahmpIO%AREAXY(I,J) = (NoahmpIO%DX*NoahmpIO%DY) / (NoahmpIO%MSFTX(I,J)*NoahmpIO%MSFTY(I,J))
             endif

             urbanpt_flag = .false.
             if ( (NoahmpIO%IVGTYP(I,J) == NoahmpIO%ISURBAN_TABLE) .or. &
                  (NoahmpIO%IVGTYP(I,J) > NoahmpIO%URBTYPE_beg) ) then
                urbanpt_flag = .true.
             endif

             if ( (NoahmpIO%IVGTYP(I,J) == NoahmpIO%ISBARREN_TABLE) .or. &
                  (NoahmpIO%IVGTYP(I,J) == NoahmpIO%ISICE_TABLE) .or. &
                  ((NoahmpIO%SF_URBAN_PHYSICS == 0) .and. (urbanpt_flag .eqv. .true.)) .or. &
                  (NoahmpIO%IVGTYP(I,J) == NoahmpIO%ISWATER_TABLE) ) then
                NoahmpIO%LAI(I,J)      = 0.0
                NoahmpIO%XSAIXY(I,J)   = 0.0
                NoahmpIO%LFMASSXY(I,J) = 0.0
                NoahmpIO%STMASSXY(I,J) = 0.0
                NoahmpIO%RTMASSXY(I,J) = 0.0
                NoahmpIO%WOODXY(I,J)   = 0.0
                NoahmpIO%STBLCPXY(I,J) = 0.0
                NoahmpIO%FASTCPXY(I,J) = 0.0
                NoahmpIO%GRAINXY(I,J)  = 1.0e-10
                NoahmpIO%GDDXY(I,J)    = 0
                NoahmpIO%CROPCAT(I,J)  = 0
             else
                if ( (NoahmpIO%LAI(I,J) > 100) .or. (NoahmpIO%LAI(I,J) < 0) ) &
                NoahmpIO%LAI(I,J)      = 0.0
                NoahmpIO%LAI(I,J)      = max(NoahmpIO%LAI(I,J), 0.05)                       ! at least start with 0.05 for arbitrary initialization (v3.7)
                NoahmpIO%XSAIXY(I,J)   = max(0.1*NoahmpIO%LAI(I,J), 0.05)                   ! MB: arbitrarily initialize SAI using input LAI (v3.7)
                if ( urbanpt_flag .eqv. .true. ) then
                   NoahmpIO%LFMASSXY(I,J) = NoahmpIO%LAI(I,J) * 1000.0 / &
                                            max(NoahmpIO%SLA_TABLE(NoahmpIO%NATURAL_TABLE),1.0)! use LAI to initialize (v3.7)
                else
                   NoahmpIO%LFMASSXY(I,J) = NoahmpIO%LAI(I,J) * 1000.0 / &
                                            max(NoahmpIO%SLA_TABLE(NoahmpIO%IVGTYP(I,J)),1.0)  ! use LAI to initialize (v3.7)
                endif
                NoahmpIO%STMASSXY(I,J) = NoahmpIO%XSAIXY(I,J) * 1000.0 / 3.0                ! use SAI to initialize (v3.7)
                NoahmpIO%RTMASSXY(I,J) = 500.0                                              ! these are all arbitrary and probably should be
                NoahmpIO%WOODXY(I,J)   = 500.0                                              ! in the table or read from initialization
                NoahmpIO%STBLCPXY(I,J) = 1000.0
                NoahmpIO%FASTCPXY(I,J) = 1000.0
                NoahmpIO%GRAINXY(I,J)  = 1.0e-10
                NoahmpIO%GDDXY(I,J)    = 0    

                ! Initialize crop for crop model
                if ( NoahmpIO%IOPT_CROP == 1 ) then
                   NoahmpIO%CROPCAT(I,J) = NoahmpIO%default_crop_table
                   if ( NoahmpIO%CROPTYPE(I,5,J) >= 0.5 ) then
                      NoahmpIO%RTMASSXY(I,J) = 0.0
                      NoahmpIO%WOODXY  (I,J) = 0.0
                      if ( (NoahmpIO%CROPTYPE(I,1,J) > NoahmpIO%CROPTYPE(I,2,J)) .and. &
                           (NoahmpIO%CROPTYPE(I,1,J) > NoahmpIO%CROPTYPE(I,3,J)) .and. &
                           (NoahmpIO%CROPTYPE(I,1,J) > NoahmpIO%CROPTYPE(I,4,J)) ) then      ! choose corn
                         NoahmpIO%CROPCAT(I,J)  = 1
                         NoahmpIO%LFMASSXY(I,J) = NoahmpIO%LAI(I,J) / 0.015                  ! Initialize lfmass Zhe Zhang 2020-07-13
                         NoahmpIO%STMASSXY(I,J) = NoahmpIO%XSAIXY(I,J) / 0.003
                      elseif ( (NoahmpIO%CROPTYPE(I,2,J) > NoahmpIO%CROPTYPE(I,1,J)) .and. &
                               (NoahmpIO%CROPTYPE(I,2,J) > NoahmpIO%CROPTYPE(I,3,J)) .and. &
                               (NoahmpIO%CROPTYPE(I,2,J) > NoahmpIO%CROPTYPE(I,4,J)) ) then  ! choose soybean
                         NoahmpIO%CROPCAT(I,J)  = 2
                         NoahmpIO%LFMASSXY(I,J) = NoahmpIO%LAI(I,J) / 0.030                  ! Initialize lfmass Zhe Zhang 2020-07-13
                         NoahmpIO%STMASSXY(I,J) = NoahmpIO%XSAIXY(I,J) / 0.003
                      else
                         NoahmpIO%CROPCAT(I,J)  = NoahmpIO%default_crop_table
                         NoahmpIO%LFMASSXY(I,J) = NoahmpIO%LAI(I,J) / 0.035
                         NoahmpIO%STMASSXY(I,J) = NoahmpIO%XSAIXY(I,J) / 0.003
                      endif
                   endif
                endif

                ! Noah-MP irrigation scheme
                if ( (NoahmpIO%IOPT_IRR >= 1) .and. (NoahmpIO%IOPT_IRR <= 3) ) then
                   if ( (NoahmpIO%IOPT_IRRM == 0) .or. (NoahmpIO%IOPT_IRRM ==1) ) then       ! sprinkler
                      NoahmpIO%IRNUMSI(I,J) = 0
                      NoahmpIO%IRWATSI(I,J) = 0.0
                      NoahmpIO%IRELOSS(I,J) = 0.0
                      NoahmpIO%IRRSPLH(I,J) = 0.0    
                   elseif ( (NoahmpIO%IOPT_IRRM == 0) .or. (NoahmpIO%IOPT_IRRM == 2) ) then  ! micro or drip
                      NoahmpIO%IRNUMMI(I,J) = 0
                      NoahmpIO%IRWATMI(I,J) = 0.0
                      NoahmpIO%IRMIVOL(I,J) = 0.0
                   elseif ( (NoahmpIO%IOPT_IRRM == 0) .or. (NoahmpIO%IOPT_IRRM == 3) ) then  ! flood 
                      NoahmpIO%IRNUMFI(I,J) = 0
                      NoahmpIO%IRWATFI(I,J) = 0.0
                      NoahmpIO%IRFIVOL(I,J) = 0.0
                   endif
                endif
             endif
          enddo ! I
       enddo    ! J
       
       ! Given the soil layer thicknesses (in DZS), initialize the soil layer
       ! depths from the surface.
       NoahmpIO%ZSOIL(1) = -NoahmpIO%DZS(1)          ! negative
       do NS = 2, NoahmpIO%NSOIL
          NoahmpIO%ZSOIL(NS) = NoahmpIO%ZSOIL(NS-1) - NoahmpIO%DZS(NS)
       enddo
       
       ! Initialize Noah-MP Snow
       call NoahmpSnowinitMain(NoahmpIO)
 
       !initialize arrays for groundwater dynamics iopt_runsub=5 
       if ( NoahmpIO%IOPT_RUNSUB == 5 ) then
          NoahmpIO%STEPWTD = nint(NoahmpIO%WTDDT * 60.0 / NoahmpIO%DTBL)
          NoahmpIO%STEPWTD = max(NoahmpIO%STEPWTD,1)
       endif

    endif ! NoahmpIO%restart_flag
 
  end subroutine NoahmpInitMain    

end module NoahmpInitMainMod
