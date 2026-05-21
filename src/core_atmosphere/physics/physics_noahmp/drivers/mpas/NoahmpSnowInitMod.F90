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
    
!local variables
 integer                                                               :: i,its,ite,iz
 real(kind=kind_noahmp),   dimension(-NoahmpIO%nsnow+1:             0) :: dzsno
 real(kind=kind_noahmp),   dimension(-NoahmpIO%nsnow+1:NoahmpIO%nsoil) :: dzsnso

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

 its = NoahmpIO%its
 ite = NoahmpIO%ite

 do i = its, ite

    ! initialize snow layers and thickness
    ! no explicit snow layer
    if ( NoahmpIO%snowh(i) < 0.025 ) then
       NoahmpIO%isnowxy(i) = 0
       dzsno(-NoahmpIO%nsnow+1:0) = 0.0
    else
       ! 1 layer snow
       if ( (NoahmpIO%snowh(i) >= 0.025) .and. (NoahmpIO%snowh(i) <= 0.05) ) then
          NoahmpIO%isnowxy(i) = -1
          dzsno(0)  = NoahmpIO%snowh(i)
       ! 2 layer snow
       elseif ( (NoahmpIO%snowh(i) > 0.05) .and. (NoahmpIO%snowh(i) <= 0.10) ) then
          NoahmpIO%isnowxy(i) = -2
          dzsno(-1) = NoahmpIO%snowh(i) / 2.0
          dzsno( 0) = NoahmpIO%snowh(i) / 2.0
       ! 2 layer thick snow
       elseif ( (NoahmpIO%snowh(i) > 0.10) .and. (NoahmpIO%snowh(i) <= 0.25) ) then
          NoahmpIO%isnowxy(i) = -2
          dzsno(-1) = 0.05
          dzsno( 0) = NoahmpIO%snowh(i) - dzsno(-1)
       ! 3 layer snow
       elseif ( (NoahmpIO%snowh(i) > 0.25) .and. (NoahmpIO%snowh(i) <= 0.45) ) then
          NoahmpIO%isnowxy(i) = -3
          dzsno(-2) = 0.05
          dzsno(-1) = 0.5 * (NoahmpIO%snowh(i)-dzsno(-2))
          dzsno( 0) = 0.5 * (NoahmpIO%snowh(i)-dzsno(-2))
       ! 3 layer thick snow
       elseif ( NoahmpIO%snowh(i) > 0.45 ) then
          NoahmpIO%isnowxy(i) = -3
          dzsno(-2) = 0.05
          dzsno(-1) = 0.20
          dzsno( 0) = NoahmpIO%snowh(i) - dzsno(-1) - dzsno(-2)
       else
          print*, "problem with the logic assigning snow layers."
          stop
       endif
    endif

    ! initialize snow temperatuer and ice/liquid content
    NoahmpIO%tsnoxy (i,-NoahmpIO%nsnow+1:0) = 0.0
    NoahmpIO%snicexy(i,-NoahmpIO%nsnow+1:0) = 0.0
    NoahmpIO%snliqxy(i,-NoahmpIO%nsnow+1:0) = 0.0
    do iz = NoahmpIO%isnowxy(i)+1, 0
       NoahmpIO%tsnoxy(i,iz)  = NoahmpIO%tgxy(i)
       NoahmpIO%snliqxy(i,iz) = 0.0
       NoahmpIO%snicexy(i,iz) = 1.0 * dzsno(iz) * (NoahmpIO%snow(i)/NoahmpIO%snowh(i))
    enddo

    ! assign local variable dzsnso, the soil/snow layer thicknesses, for snow layers
    do iz = NoahmpIO%isnowxy(i)+1, 0
       dzsnso(iz) = -dzsno(iz)
    enddo

    ! assign local variable dzsnso, the soil/snow layer thicknesses, for soil layers
    dzsnso(1) = NoahmpIO%zsoil(1)
    do iz = 2, NoahmpIO%nsoil
       dzsnso(iz) = NoahmpIO%zsoil(iz) - NoahmpIO%zsoil(iz-1)
    enddo

    ! assign zsnsoxy, the layer depths, for soil and snow layers
    NoahmpIO%zsnsoxy(i,NoahmpIO%isnowxy(i)+1) = dzsnso(NoahmpIO%isnowxy(i)+1)
    do iz = NoahmpIO%isnowxy(i)+2, NoahmpIO%nsoil
       NoahmpIO%zsnsoxy(i,iz) = NoahmpIO%zsnsoxy(i,iz-1) + dzsnso(iz)
    enddo

 enddo

 end subroutine NoahmpSnowInitMain

 end module NoahmpSnowInitMod

