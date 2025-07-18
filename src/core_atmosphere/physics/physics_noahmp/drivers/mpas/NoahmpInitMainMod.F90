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
 integer                                     :: its,ite
 integer                                     :: i,ns
 integer                                     :: errorflag
 logical                                     :: urbanpt_flag
 real(kind=kind_noahmp)                      :: bexp, smcmax, psisat, fk
 real(kind=kind_noahmp), parameter           :: hlice = 3.335e5
 real(kind=kind_noahmp), parameter           :: grav0 = 9.81
 real(kind=kind_noahmp), parameter           :: t0    = 273.15
! ---------------------------------------------------------------------------

! only initialize for non-restart case:
 if ( .not. NoahmpIO%restart_flag ) then

    its = NoahmpIO%its
    ite = NoahmpIO%ite

    ! initialize physical snow height SNOWH
    if ( .not. NoahmpIO%fndsnowh ) then
       ! If no SNOWH do the following
       print*, 'SNOW HEIGHT NOT FOUND - VALUE DEFINED IN LSMINIT'
       do i = its, ite
          NoahmpIO%snowh(i) = NoahmpIO%snow(i)*0.005  ! snow in mm and snowh in m.
       enddo
    endif

    ! Check if snow/snowh are consistent and cap SWE at 2000mm
    ! the Noah-MP code does it internally but if we don't do it here, problems ensue
    do i = its, ite 
       if ( NoahmpIO%snow(i)  < 0.0 ) NoahmpIO%snow(i)  = 0.0
       if ( NoahmpIO%snowh(i) < 0.0 ) NoahmpIO%snowh(i) = 0.0
       if ( (NoahmpIO%snow(i) > 0.0) .and. (NoahmpIO%snowh(i) == 0.0) ) &
            NoahmpIO%snowh(i) = NoahmpIO%snow(i) * 0.005
       if ( (NoahmpIO%snowh(i) > 0.0) .and. (NoahmpIO%snow(i) == 0.0) ) &
            NoahmpIO%snow(i)  = NoahmpIO%snowh(i) / 0.005
       if ( NoahmpIO%snow(i) > 2000.0 ) then
            NoahmpIO%snowh(i) = NoahmpIO%snowh(i) * 2000.0 / NoahmpIO%snow(i) !snow in mm and snowh in m.
            NoahmpIO%snow (i) = 2000.0                                        !cap snow at 2000 to maintain
                                                                                 !density.
       endif
    enddo

    ! check soil type:
    errorflag = 0
    do i = its, ite
       if ( NoahmpIO%isltyp(i) < 1 ) then
            errorflag = 1
            write(*,*) "LSMINIT: OUT OF RANGE ISLTYP ",i,NoahmpIO%isltyp(i)
            stop
       endif
    enddo

    ! initialize soil liquid water content SH2O:
    do i = its , ite
       if ( (NoahmpIO%ivgtyp(i) == NoahmpIO%isice_table) .and. &
            (NoahmpIO%xice(i) <= 0.0) ) then
          do ns = 1, NoahmpIO%nsoil
             NoahmpIO%smois(i,ns) = 1.0  ! glacier starts all frozen
             NoahmpIO%sh2o(i,ns)  = 0.0
             NoahmpIO%tslb(i,ns)  = min(NoahmpIO%tslb(i,ns), 263.15)          !set glacier temp to at most -10c
          enddo
          !NoahmpIO%tmn(i) = min(NoahmpIO%tmn(i), 263.15)                     !set deep temp to at most -10C
          NoahmpIO%snow(i)  = max(NoahmpIO%snow(i), 10.0)                     !set swe to at least 10mm
          NoahmpIO%snowh(i) = NoahmpIO%snow(i) * 0.01                         !snow in mm and snowh in m
       else
          bexp   = NoahmpIO%bexp_table  (NoahmpIO%isltyp(i))
          smcmax = NoahmpIO%smcmax_table(NoahmpIO%isltyp(i))
          psisat = NoahmpIO%psisat_table(NoahmpIO%isltyp(i))
          do ns = 1, NoahmpIO%nsoil
             if ( NoahmpIO%smois(i,ns) > smcmax ) NoahmpIO%smois(i,ns) = smcmax
          enddo
          if ( (bexp > 0.0) .and. (smcmax > 0.0) .and. (psisat > 0.0) ) then
             do ns = 1, NoahmpIO%nsoil
                if ( NoahmpIO%tslb(i,ns) < 273.149 ) then
                   fk = (((hlice / (grav0*(-psisat))) * &
                         ((NoahmpIO%tslb(i,ns)-t0) / NoahmpIO%tslb(i,ns)))**(-1/bexp))*smcmax
                   fk = max(fk, 0.02)
                   NoahmpIO%sh2o(i,ns) = min(fk, NoahmpIO%smois(i,ns))
                else
                   NoahmpIO%sh2o(i,ns) = NoahmpIO%smois(i,ns)
                endif
             enddo
          else
             do ns = 1, NoahmpIO%nsoil
                NoahmpIO%sh2o(i,ns) = NoahmpIO%smois(i,ns)
             enddo
          endif
       endif
    enddo

    ! initialize other quantities:
    do i = its, ite
       NoahmpIO%qtdrain(i)  = 0.0
       NoahmpIO%tvxy(i)     = NoahmpIO%tsk(i)
       NoahmpIO%tgxy(i)     = NoahmpIO%tsk(i)
       if ( (NoahmpIO%snow(i) > 0.0) .and. (NoahmpIO%tsk(i) > t0) ) NoahmpIO%tvxy(i) = t0
       if ( (NoahmpIO%snow(i) > 0.0) .and. (NoahmpIO%tsk(i) > t0) ) NoahmpIO%tgxy(i) = t0

       NoahmpIO%canwat(i)   = 0.0
       NoahmpIO%canliqxy(i) = NoahmpIO%canwat(i)
       NoahmpIO%canicexy(i) = 0.0
       NoahmpIO%eahxy(i)    = 2000.0
       NoahmpIO%tahxy(i)    = NoahmpIO%tsk(i)
       NoahmpIO%t2mvxy(i)   = NoahmpIO%tsk(i)
       NoahmpIO%t2mbxy(i)   = NoahmpIO%tsk(i)
       NoahmpIO%t2mxy(i)    = NoahmpIO%tsk(i)
       if ( (NoahmpIO%snow(i) > 0.0) .and. (NoahmpIO%tsk(i) > t0) ) NoahmpIO%tahxy(i)  = t0
       if ( (NoahmpIO%snow(i) > 0.0) .and. (NoahmpIO%tsk(i) > t0) ) NoahmpIO%t2mvxy(i) = t0
       if ( (NoahmpIO%snow(i) > 0.0) .and. (NoahmpIO%tsk(i) > t0) ) NoahmpIO%t2mbxy(i) = t0
       if ( (NoahmpIO%snow(i) > 0.0) .and. (NoahmpIO%tsk(i) > t0) ) NoahmpIO%t2mxy(i)  = t0

       NoahmpIO%cmxy(i)     = 0.0
       NoahmpIO%chxy(i)     = 0.0
       NoahmpIO%fwetxy(i)   = 0.0
       NoahmpIO%sneqvoxy(i) = 0.0
       NoahmpIO%alboldxy(i) = 0.65
       NoahmpIO%qsnowxy(i)  = 0.0
       NoahmpIO%qrainxy(i)  = 0.0
       NoahmpIO%wslakexy(i) = 0.0

       if ( NoahmpIO%iopt_runsub /= 5 ) then
          NoahmpIO%waxy(i)   = 4900.0
          NoahmpIO%wtxy(i)   = NoahmpIO%waxy(i)
          NoahmpIO%zwtxy(i)  = (25.0 + 2.0) - NoahmpIO%waxy(i)/1000/0.2
       else
          NoahmpIO%waxy(i)   = 0.0
          NoahmpIO%wtxy(i)   = 0.0
       endif

       urbanpt_flag = .false.
       if ( (NoahmpIO%ivgtyp(i) == NoahmpIO%isurban_table) .or. &
            (NoahmpIO%ivgtyp(i) > NoahmpIO%urbtype_beg) ) then
            urbanpt_flag = .true.
       endif

       if ( (NoahmpIO%ivgtyp(i) == NoahmpIO%isbarren_table)                      .or. &
            (NoahmpIO%ivgtyp(i) == NoahmpIO%isice_table)                         .or. &
            ((NoahmpIO%sf_urban_physics == 0) .and. (urbanpt_flag .eqv. .true.)) .or. &
            (NoahmpIO%ivgtyp(i) == NoahmpIO%iswater_table )) then
          NoahmpIO%lai(i)      = 0.0
          NoahmpIO%xsaixy(i)   = 0.0
          NoahmpIO%lfmassxy(i) = 0.0
          NoahmpIO%stmassxy(i) = 0.0
          NoahmpIO%rtmassxy(i) = 0.0
          NoahmpIO%woodxy(i)   = 0.0
          NoahmpIO%stblcpxy(i) = 0.0
          NoahmpIO%fastcpxy(i) = 0.0
          NoahmpIO%grainxy(i)  = 1.0e-10
          NoahmpIO%gddxy(i)    = 0
          NoahmpIO%cropcat(i)  = 0
       else
          if ( (NoahmpIO%lai(i) > 100) .or. (NoahmpIO%lai(i) < 0) ) NoahmpIO%lai(i) = 0.0
          NoahmpIO%lai(i)      = max(NoahmpIO%lai(i), 0.05)                      !at least start with 0.05 for arbitrary initialization (v3.7)
          NoahmpIO%xsaixy(i)   = max(0.1*NoahmpIO%lai(i), 0.05)                  !mb: arbitrarily initialize sai using input lai (v3.7)
          NoahmpIO%lfmassxy(i) = NoahmpIO%lai(i) * 1000.0 / &
                                 max(NoahmpIO%sla_table(NoahmpIO%ivgtyp(i)),1.0) !use lai to initialize (v3.7)
          NoahmpIO%stmassxy(i) = NoahmpIO%xsaixy(i) * 1000.0 / 3.0               !use sai to initialize (v3.7)
          NoahmpIO%rtmassxy(i) = 500.0                                           !these are all arbitrary and probably should be
          NoahmpIO%woodxy(i)   = 500.0                                           !in the table or read from initialization
          NoahmpIO%stblcpxy(i) = 1000.0
          NoahmpIO%fastcpxy(i) = 1000.0
          NoahmpIO%grainxy(i)  = 1.0e-10
          NoahmpIO%gddxy(i)    = 0

          ! initialize crop for crop model:
          if ( NoahmpIO%iopt_crop == 1 ) then
             NoahmpIO%cropcat(i) = NoahmpIO%default_crop_table
             if ( NoahmpIO%croptype(i,5) >= 0.5 ) then
                NoahmpIO%rtmassxy(i) = 0.0
                NoahmpIO%woodxy  (i) = 0.0
                if ( (NoahmpIO%croptype(i,1) > NoahmpIO%croptype(i,2)) .and. &
                     (NoahmpIO%croptype(i,1) > NoahmpIO%croptype(i,3)) .and. &
                     (NoahmpIO%croptype(i,1) > NoahmpIO%croptype(i,4)) ) then    !choose corn
                   NoahmpIO%cropcat(i)  = 1
                   NoahmpIO%lfmassxy(i) = NoahmpIO%lai(i) / 0.015                !initialize lfmass zhe zhang 2020-07-13
                   NoahmpIO%stmassxy(i) = NoahmpIO%xsaixy(i) / 0.003
                elseif ( (NoahmpIO%croptype(i,2) > NoahmpIO%croptype(i,1)) .and. &
                         (NoahmpIO%croptype(i,2) > NoahmpIO%croptype(i,3)) .and. &
                         (NoahmpIO%croptype(i,2) > NoahmpIO%croptype(i,4)) ) then!choose soybean
                   NoahmpIO%cropcat(i)  = 2
                   NoahmpIO%lfmassxy(i) = NoahmpIO%lai(i) / 0.030                !initialize lfmass zhe zhang 2020-07-13
                   NoahmpIO%stmassxy(i) = NoahmpIO%xsaixy(i) / 0.003
                else
                   NoahmpIO%cropcat(i)  = NoahmpIO%default_crop_table
                   NoahmpIO%lfmassxy(i) = NoahmpIO%lai(i) / 0.035
                   NoahmpIO%stmassxy(i) = NoahmpIO%xsaixy(i) / 0.003
                endif
             endif
          endif

          ! Noah-MP irrigation scheme:
          if ( (NoahmpIO%iopt_irr >= 1) .and. (NoahmpIO%iopt_irr <= 3) ) then
             if ( (NoahmpIO%iopt_irrm == 0) .or. (NoahmpIO%iopt_irrm ==1) ) then       ! sprinkler
                NoahmpIO%irnumsi(i) = 0
                NoahmpIO%irwatsi(i) = 0.0
                NoahmpIO%ireloss(i) = 0.0
                NoahmpIO%irrsplh(i) = 0.0
             elseif ( (NoahmpIO%iopt_irrm == 0) .or. (NoahmpIO%iopt_irrm == 2) ) then  ! micro or drip
                NoahmpIO%irnummi(i) = 0
                NoahmpIO%irwatmi(i) = 0.0
                NoahmpIO%irmivol(i) = 0.0
             elseif ( (NoahmpIO%iopt_irrm == 0) .or. (NoahmpIO%iopt_irrm == 3) ) then  ! flood
                NoahmpIO%irnumfi(i) = 0
                NoahmpIO%irwatfi(i) = 0.0
                NoahmpIO%irfivol(i) = 0.0
             endif
          endif
       endif
    enddo

    ! Given the soil layer thicknesses (in DZS), initialize the soil layer
    ! depths from the surface:
    NoahmpIO%zsoil(1) = -NoahmpIO%dzs(1)          ! negative
    do ns = 2, NoahmpIO%nsoil
       NoahmpIO%zsoil(ns) = NoahmpIO%zsoil(ns-1) - NoahmpIO%dzs(ns)
    enddo

    ! initialize noah-mp snow
    call NoahmpSnowInitMain(NoahmpIO)

    !initialize arrays for groundwater dynamics iopt_runsub=5
    if ( NoahmpIO%iopt_runsub == 5 ) then
       NoahmpIO%stepwtd = nint(NoahmpIO%wtddt * 60.0 / NoahmpIO%dtbl)
       NoahmpIO%stepwtd = max(NoahmpIO%stepwtd,1)
    endif

 endif ! NoahmpIO%restart_flag

 end subroutine NoahmpInitMain

 end module NoahmpInitMainMod
