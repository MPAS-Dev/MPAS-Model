module NoahmpSnowInitMod

!  Module to initialize Noah-MP Snow variables

  use Machine
  use NoahmpIOVarType
  
  implicit none
  
contains

  subroutine NoahmpSnowInitMain(NoahmpIO)

! ------------------------ Code history -----------------------------------
! Original Noah-MP subroutine: SNOW_INIT
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ------------------------------------------------------------------------- 

    implicit none 
    
    type(NoahmpIO_type), intent(inout) :: NoahmpIO
    
! local variables
    integer                                                               :: I,J,IZ,itf,jtf
    real(kind=kind_noahmp),   dimension(-NoahmpIO%NSNOW+1:             0) :: DZSNO
    real(kind=kind_noahmp),   dimension(-NoahmpIO%NSNOW+1:NoahmpIO%NSOIL) :: DZSNSO    

!------------------------------------------------------------------------------------------    
!   Initialize snow arrays for Noah-MP LSM, based in input SNOWDEP, NSNOW
!   ISNOWXY is an index array, indicating the index of the top snow layer.  Valid indices
!           for snow layers range from 0 (no snow) and -1 (shallow snow) to (-NSNOW)+1 (deep snow).
!   TSNOXY holds the temperature of the snow layer.  Snow layers are initialized with 
!          temperature = ground temperature [?].  Snow-free levels in the array have value 0.0
!   SNICEXY is the frozen content of a snow layer.  Initial estimate based on SNOWH and SNOW
!   SNLIQXY is the liquid content of a snow layer.  Initialized to 0.0
!   ZNSNOXY is the layer depth from the surface.  
!------------------------------------------------------------------------------------------

    itf = min0(NoahmpIO%ite, (NoahmpIO%ide+1)-1)
    jtf = min0(NoahmpIO%jte, (NoahmpIO%jde+1)-1)

    do J = NoahmpIO%jts, jtf
       do I = NoahmpIO%its, itf

          ! initialize snow layers and thickness
          ! no explicit snow layer
          if ( NoahmpIO%SNOWH(I,J) < 0.025 ) then
             NoahmpIO%ISNOWXY(I,J) = 0
             DZSNO(-NoahmpIO%NSNOW+1:0) = 0.0
          else
             ! 1 layer snow
             if ( (NoahmpIO%SNOWH(I,J) >= 0.025) .and. (NoahmpIO%SNOWH(I,J) <= 0.05) ) then
                NoahmpIO%ISNOWXY(I,J) = -1
                DZSNO(0)  = NoahmpIO%SNOWH(I,J)
             ! 2 layer snow
             elseif ( (NoahmpIO%SNOWH(I,J) > 0.05) .and. (NoahmpIO%SNOWH(I,J) <= 0.10) ) then
                NoahmpIO%ISNOWXY(I,J) = -2
                DZSNO(-1) = NoahmpIO%SNOWH(I,J) / 2.0
                DZSNO( 0) = NoahmpIO%SNOWH(I,J) / 2.0
             ! 2 layer thick snow
             elseif ( (NoahmpIO%SNOWH(I,J) > 0.10) .and. (NoahmpIO%SNOWH(I,J) <= 0.25) ) then
                NoahmpIO%ISNOWXY(I,J) = -2
                DZSNO(-1) = 0.05
                DZSNO( 0) = NoahmpIO%SNOWH(I,J) - DZSNO(-1)
             ! 3 layer snow
             elseif ( (NoahmpIO%SNOWH(I,J) > 0.25) .and. (NoahmpIO%SNOWH(I,J) <= 0.45) ) then
                NoahmpIO%ISNOWXY(I,J) = -3
                DZSNO(-2) = 0.05
                DZSNO(-1) = 0.5 * (NoahmpIO%SNOWH(I,J)-DZSNO(-2))
                DZSNO( 0) = 0.5 * (NoahmpIO%SNOWH(I,J)-DZSNO(-2))
             ! 3 layer thick snow
             elseif ( NoahmpIO%SNOWH(I,J) > 0.45 ) then
                NoahmpIO%ISNOWXY(I,J) = -3
                DZSNO(-2) = 0.05
                DZSNO(-1) = 0.20
                DZSNO( 0) = NoahmpIO%SNOWH(I,J) - DZSNO(-1) - DZSNO(-2)
             else
                print*, "Problem with the logic assigning snow layers."
                stop
             endif
          endif

          ! initialize snow temperatuer and ice/liquid content
          NoahmpIO%TSNOXY (I,-NoahmpIO%NSNOW+1:0,J) = 0.0
          NoahmpIO%SNICEXY(I,-NoahmpIO%NSNOW+1:0,J) = 0.0
          NoahmpIO%SNLIQXY(I,-NoahmpIO%NSNOW+1:0,J) = 0.0
          do IZ = NoahmpIO%ISNOWXY(I,J)+1, 0
             NoahmpIO%TSNOXY(I,IZ,J)  = NoahmpIO%TGXY(I,J)
             NoahmpIO%SNLIQXY(I,IZ,J) = 0.0
             NoahmpIO%SNICEXY(I,IZ,J) = 1.0 * DZSNO(IZ) * (NoahmpIO%SNOW(I,J)/NoahmpIO%SNOWH(I,J))
          enddo

          ! Assign local variable DZSNSO, the soil/snow layer thicknesses, for snow layers
          do IZ = NoahmpIO%ISNOWXY(I,J)+1, 0
             DZSNSO(IZ) = -DZSNO(IZ)
          enddo

          ! Assign local variable DZSNSO, the soil/snow layer thicknesses, for soil layers
          DZSNSO(1) = NoahmpIO%ZSOIL(1)
          do IZ = 2, NoahmpIO%NSOIL
             DZSNSO(IZ) = NoahmpIO%ZSOIL(IZ) - NoahmpIO%ZSOIL(IZ-1)
          enddo

          ! Assign ZSNSOXY, the layer depths, for soil and snow layers
          NoahmpIO%ZSNSOXY(I,NoahmpIO%ISNOWXY(I,J)+1,J) = DZSNSO(NoahmpIO%ISNOWXY(I,J)+1)
          do IZ = NoahmpIO%ISNOWXY(I,J)+2, NoahmpIO%NSOIL
             NoahmpIO%ZSNSOXY(I,IZ,J) = NoahmpIO%ZSNSOXY(I,IZ-1,J) + DZSNSO(IZ)
          enddo

       enddo ! I
    enddo    ! J

  end subroutine NoahmpSnowInitMain

end module NoahmpSnowInitMod

