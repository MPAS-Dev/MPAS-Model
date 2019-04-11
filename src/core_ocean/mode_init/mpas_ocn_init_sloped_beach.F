! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  ocn_init_sloped_beach
!
!> \brief MPAS ocean initialize case -- Tests radiation stress implementation 
!> \author Steven Brus 
!> \date   August 2019
!> \details
!>  This module contains the routines for initializing the
!>  the sloped_beach test case
!
!-----------------------------------------------------------------------

module ocn_init_sloped_beach

   use mpas_constants
   use mpas_kind_types
   use mpas_io_units
   use mpas_derived_types
   use mpas_pool_routines
   use mpas_dmpar

   use ocn_constants
   use ocn_init_vertical_grids
   use ocn_init_cell_markers

   implicit none
   private
   save

   !--------------------------------------------------------------------
   !
   ! Public parameters
   !
   !--------------------------------------------------------------------

   !--------------------------------------------------------------------
   !
   ! Public member functions
   !
   !--------------------------------------------------------------------

   public :: ocn_init_setup_sloped_beach, &
             ocn_init_validate_sloped_beach

   !--------------------------------------------------------------------
   !
   ! Private module variables
   !
   !--------------------------------------------------------------------


!***********************************************************************

contains

!***********************************************************************
!
!  routine ocn_init_setup_sloped_beach
!
!> \brief   Setup for sloped beach test case 
!> \author  Steven Brus
!> \date    August 2019
!> \details
!>  This routine sets up the initial conditions for the sloped_beach test case.
!
!-----------------------------------------------------------------------
  subroutine ocn_init_setup_sloped_beach(domain, iErr)!{{{

    !--------------------------------------------------------------------

    implicit none

    type (domain_type), intent(inout) :: domain
    integer, intent(out) :: iErr

    type (block_type), pointer :: block_ptr

    type (mpas_pool_type), pointer :: meshPool
    type (mpas_pool_type), pointer :: forcingPool
    type (mpas_pool_type), pointer :: statePool
    type (mpas_pool_type), pointer :: diagnosticsPool
    type (mpas_pool_type), pointer :: verticalMeshPool
    type (mpas_pool_type), pointer :: tracersPool
    type (mpas_pool_type), pointer :: solPool

    integer :: iCell, i
    real(kind=RKIND) :: H0,T,theta,d0
    real(kind=RKIND) :: d,x
    real(kind=RKIND) :: Sxx,Sxy,Syy
    real(kind=RKIND) :: dSxxdx

    ! Define dimensions
    integer, pointer :: nCellsSolve, nEdgesSolve, nVertLevels, nVertLevelsP1
    integer, pointer :: index_temperature, index_salinity

    ! Define arrays
    integer, dimension(:), pointer :: maxLevelCell
    integer, pointer :: npts 
    real (kind=RKIND), dimension(:), pointer :: xCell, refBottomDepth, bottomDepth, vertCoordMovementWeights
    real (kind=RKIND), dimension(:), pointer :: ssh
    real (kind=RKIND), dimension(:), pointer :: sshExact,xTransect
    real (kind=RKIND), dimension(:), pointer :: waveRadiationStressXX, waveRadiationStressYY, waveRadiationStressXY 
    real (kind=RKIND), dimension(:,:), pointer :: layerThickness, restingThickness, zMid
    real (kind=RKIND), dimension(:,:,:), pointer :: activeTracers

    ! Define configs
    character (len=StrKIND), pointer :: config_init_configuration, &
                                        config_vertical_grid
    integer, pointer :: config_sloped_beach_vert_levels

    real (kind=RKIND), dimension(:), pointer :: interfaceLocations


    iErr = 0

    H0 = 0.6_RKIND
    T = 5.0_RKIND
    theta = 0.0_RKIND
    d0 = 2.1_RKIND

    call mpas_pool_get_config(ocnConfigs, 'config_init_configuration', config_init_configuration)

    call mpas_log_write('here')
    if(config_init_configuration .ne. trim('sloped_beach')) return

    call mpas_pool_get_config(ocnConfigs, 'config_vertical_grid', config_vertical_grid)

    call mpas_pool_get_config(ocnConfigs, 'config_sloped_beach_vert_levels', config_sloped_beach_vert_levels)

    call mpas_pool_get_subpool(domain % blocklist % structs, 'mesh', meshPool)
    call mpas_pool_get_dimension(meshPool, 'nVertLevels', nVertLevels)
    call mpas_pool_get_dimension(meshPool, 'nVertLevelsP1', nVertLevelsP1)
    call mpas_pool_get_subpool(domain % blocklist % structs, 'exact_solution', solPool)

    nVertLevels = config_sloped_beach_vert_levels
    nVertLevelsP1 = nVertLevels + 1

    allocate(interfaceLocations(nVertLevelsP1))
    call ocn_generate_vertical_grid(config_vertical_grid, interfaceLocations)

    block_ptr => domain % blocklist
    do while(associated(block_ptr))
      call mpas_pool_get_subpool(block_ptr % structs, 'mesh', meshPool)
      call mpas_pool_get_subpool(block_ptr % structs, 'forcing', forcingPool)
      call mpas_pool_get_subpool(block_ptr % structs, 'state', statePool)
      call mpas_pool_get_subpool(block_ptr % structs, 'diagnostics', diagnosticsPool)
      call mpas_pool_get_subpool(block_ptr % structs, 'verticalMesh', verticalMeshPool)
      call mpas_pool_get_subpool(statePool, 'tracers', tracersPool)

      call mpas_pool_get_array(meshPool, 'xCell', xCell)
      call mpas_pool_get_dimension(meshPool, 'nCellsSolve', nCellsSolve)
      call mpas_pool_get_dimension(meshPool, 'nVertLevels', nVertLevels)
      call mpas_pool_get_array(meshPool, 'maxLevelCell', maxLevelCell)
      call mpas_pool_get_array(meshPool, 'refBottomDepth', refBottomDepth)
      call mpas_pool_get_array(meshPool, 'bottomDepth', bottomDepth)
      call mpas_pool_get_array(meshPool, 'vertCoordMovementWeights', vertCoordMovementWeights)

      call mpas_pool_get_dimension(tracersPool, 'index_temperature', index_temperature)
      call mpas_pool_get_dimension(tracersPool, 'index_salinity', index_salinity)
      call mpas_pool_get_array(tracersPool, 'activeTracers', activeTracers, 1)

      call mpas_pool_get_array(statePool, 'ssh', ssh, 1)
      call mpas_pool_get_array(statePool, 'layerThickness', layerThickness, 1)

      call mpas_pool_get_array(forcingPool, 'waveRadiationStressXX', waveRadiationStressXX, 1)
      call mpas_pool_get_array(forcingPool, 'waveRadiationStressYY', waveRadiationStressYY, 1)
      call mpas_pool_get_array(forcingPool, 'waveRadiationStressXY', waveRadiationStressXY, 1)

      call mpas_pool_get_array(verticalMeshPool, 'restingThickness', restingThickness)

      call mpas_pool_get_array(diagnosticsPool, 'zMid', zMid)


      ! Set refBottomDepth, bottomDepth, and maxLevelCell
      do i = 1, nVertLevels
        refBottomDepth(i) = d0 * interfaceLocations(i+1)
      end do

      do iCell = 1, nCellsSolve
        if (xCell(iCell) >= 80.0_RKIND) then
          bottomDepth(iCell) = d0
        else
          bottomDepth(iCell) = (1.0_RKIND/40.0_RKIND)*xCell(iCell) + 0.1_RKIND
        end if
      end do


      do iCell = 1, nCellsSolve
        maxLevelCell(iCell) = nVertLevels
      end do 
      do iCell = 1, nCellsSolve
        ssh(iCell) = 0d0 
      end do

      do iCell = 1, nCellsSolve
        do i = 1, maxLevelCell(iCell)
          layerThickness(i,iCell) = bottomDepth(iCell)/real(nVertLevels,RKIND)
          restingThickness(i, iCell) = layerThickness(i, iCell)
        end do
      end do

      ! Set vertCoordMovementWeights
      vertCoordMovementWeights(:) = 1.0_RKIND

      ! Set salinity
      do iCell = 1, nCellsSolve
        activeTracers(index_salinity, :, iCell) = 33.0_RKIND 
      end do

      ! Set temperature
      do iCell = 1, nCellsSolve
        activeTracers(index_temperature, :, iCell) = 20.0_RKIND 
      end do

      ! Set wave radiation stress from linear wave theory
      do iCell = 1, nCellsSolve
        
        d = bottomDepth(iCell)
        x = xCell(iCell)

        call compute_radiation_stress(x,d,H0,T,theta,d0,Sxx,Sxy,Syy,dSxxdx)

        waveRadiationStressXX(iCell) = Sxx 
        waveRadiationStressYY(iCell) = Syy
        waveRadiationStressXY(iCell) = Sxy

      enddo

      block_ptr => block_ptr % next
    end do

    deallocate(interfaceLocations)

    call mpas_pool_get_subpool(domain % blocklist % structs, 'exact_solution', solPool)
    call mpas_pool_get_dimension(solPool, 'nSolPts', npts)
    call mpas_pool_get_array(solPool, 'xTransect', xTransect)
    call mpas_pool_get_array(solPool, 'sshExact',sshExact)
    call compute_exact_solution(npts,d0,H0,T,xTransect,sshExact)

    !--------------------------------------------------------------------

  end subroutine ocn_init_setup_sloped_beach!}}}

!***********************************************************************
!
!  routine compute_radiation_stress 
!
!> \brief   Computes wave radiation stress for sloped beach test case
!> \author  Steven Brus 
!> \date    August 2019
!> \details
!>  Computes the wave radiation stresses based on linear wave theory
!
!-----------------------------------------------------------------------
  subroutine compute_radiation_stress(x,d,H0,T,theta,d0,Sxx,Sxy,Syy,dSxxdx) !{{{

    !--------------------------------------------------------------------

    implicit none

    real(kind=RKIND), intent(in) :: x
    real(kind=RKIND), intent(in) :: d
    real(kind=RKIND), intent(in) :: H0
    real(kind=RKIND), intent(in) :: T
    real(kind=RKIND), intent(in) :: theta
    real(kind=RKIND), intent(in) :: d0
    real(kind=RKIND), intent(out) :: Sxx,Sxy,Syy
    real(kind=RKIND), intent(out) :: dSxxdx

    real(kind=RKIND) :: H,Hb,dHdx
    real(kind=RKIND) :: kappa
    real(kind=RKIND) :: sigma
    real(kind=RKIND) :: dddx
    real(kind=RKIND) :: k0,k,kd,dkdx
    real(kind=RKIND) :: C,Cg,Cg0
    real(kind=RKIND) :: n,n0,dndx
    real(kind=RKIND) :: E,dEdx

    sigma = 2.0_RKIND*pii/T
    kappa = 0.78_RKIND

    k0 = wavenumber(T,d0)
    n0 =  0.5_RKIND*(1.0_RKIND + (2.0_RKIND*k0*d0)/sinh(2.0_RKIND*k0*d0))
    Cg0 = n0*sigma/k0

    k = wavenumber(T,d)
    kd = k*d

    n = 0.5_RKIND*(1.0_RKIND + (2.0_RKIND*kd)/sinh(2.0_RKIND*kd))
    C = sigma/k
    Cg = n*C
    H = H0*sqrt(Cg0/Cg)
    Hb = min(kappa*d,H)
    E = (1.0_RKIND/8.0_RKIND)*rho_fw*gravity*Hb**2
    
    Sxx = E*(n*(cos(theta)**2+1.0_RKIND)-0.5_RKIND)
    Syy = E*(n*(sin(theta)**2+1.0_RKIND)-0.5_RKIND)
    Sxy = 0.5_RKIND*E*n*sin(2.0_RKIND*theta)

    if (x <= 80.0_RKIND) then
      dddx = 1.0_RKIND/40.0_RKIND
    else  
      dddx = 0.0_RKIND
    end if

    dkdx = -(k**2*dddx)/(cosh(kd)**2*tanh(kd) + kd)
    dndx = (dkdx*d+k*dddx)*(sinh(2.0_RKIND*kd)-2.0_RKIND*kd*cosh(2.0_RKIND*kd))/sinh(2.0_RKIND*kd)**2
    if (d <= H/kappa) then
      dHdx = kappa*(1.0_RKIND/40.0_RKIND)
    else
      dHdx = 0.5_RKIND*H0/sqrt((n0/k0)*(k/n))*((dkdx*n-dndx*k)/n**2)*(n0/k0)
    end if
    dEdx = 0.25_RKIND*rho_fw*gravity*Hb*dHdx

    dSxxdx = dEdx*(n*(cos(theta)**2+1.0_RKIND)-0.5_RKIND) + E*dndx*(cos(theta)**2+1.0_RKIND)

    !--------------------------------------------------------------------

  end subroutine compute_radiation_stress!}}}

!***********************************************************************
!
!  routine compute_exact_solution 
!
!> \brief   Computes exact ssh solution for sloped beach case 
!> \author  Steven Brus 
!> \date    September 2019
!> \details
!>  Uses linear wave theory for setup/setdown  
!
!-----------------------------------------------------------------------
  subroutine compute_exact_solution(npts,d0,H0,T,xTransect,sshExact)!{{{

    !--------------------------------------------------------------------

    implicit none

    integer, intent(in) :: npts
    real (kind=RKIND), intent(in) :: d0
    real (kind=RKIND), intent(in) :: H0
    real (kind=RKIND), intent(in) :: T 
    real (kind=RKIND), dimension(:), intent(out) :: xTransect    
    real (kind=RKIND), dimension(:), intent(out) :: sshExact    

    integer :: i
    integer :: breaking
    real (kind=RKIND) :: a,d,k,H,n,C,Cg,Cg0,n0,k0
    real (kind=RKIND) :: kappa,sigma
    real (kind=RKIND) :: xb,db,dx
    real (kind=RKIND) :: sshb

    dx = 150.0/real(npts,RKIND)

    ! Compute inital wave parameters
    kappa = 0.78_RKIND
    sigma = 2.0_RKIND*pii/T
    k0 = wavenumber(T,d0)
    n0 =  0.5_RKIND*(1.0_RKIND + (2.0_RKIND*k0*d0)/sinh(2.0_RKIND*k0*d0))
    Cg0 = n0*sigma/k0

    breaking = 0
    do i = 1,npts
      
      ! Compute x location and depth moving toward coast
      xTransect(i) = 150.0_RKIND - real(i-1,RKIND)*dx
      d = xTransect(i)/40.0_RKIND + 0.1_RKIND

      ! Compute wave height
      k = wavenumber(T,d)
      n = 0.5_RKIND*(1.0_RKIND + (2.0_RKIND*k*d)/sinh(2.0_RKIND*k*d))
      C = sigma/k
      Cg = n*C
      H = H0*sqrt(Cg0/Cg)

      ! Check breaking criteria
      if (H >= kappa*d) then
        breaking = 1
      end if

      if (breaking == 1) then
        ! Setup solution
        sshExact(i) = sshb + 3.0_RKIND*kappa**2/8.0_RKIND/(1.0_RKIND + 3.0_RKIND*kappa**2/8.0_RKIND)*(db - d)
      else
        ! Setdown solution
        a = 0.5_RKIND*H
        sshExact(i) = -0.5_RKIND*a**2*k/sinh(2.0_RKIND*d*k) 
        
        ! Keep track of ssh and d at breaking point
        sshb = sshExact(i)
        db = d
      end if 

    end do

    !--------------------------------------------------------------------

  end subroutine compute_exact_solution!}}}

!***********************************************************************
!
!  function wavenumber 
!
!> \brief   Computes wavenumber given period and depth 
!> \author  Steven Brus 
!> \date    August 2019
!> \details
!>  Uses Newton's method iteration to solve linear dispersion relationship 
!
!-----------------------------------------------------------------------
  function wavenumber(T,h) result(k)!{{{
    
    !--------------------------------------------------------------------

    implicit none

    real(kind=RKIND) :: k
    real(kind=RKIND), intent(in) :: T
    real(kind=RKIND), intent(in) :: h

    real(kind=RKIND) :: sigma
    real(kind=RKIND) :: tol
    real(kind=RKIND) :: kh 
    real(kind=RKIND) :: knew
    real(kind=RKIND) :: f,fp 
    integer :: i
    integer :: maxit
    
    ! period to frequency
    sigma = 2.0_RKIND*pii/T

    ! initial guess (Eckart 1951)
    k = sigma**2/(gravity*sqrt(tanh(sigma**2*h/gravity)))
    
    maxit = 100
    tol = 1e-8_RKIND

    do i = 1,maxit

      kh = k*h
      f = gravity*k*tanh(kh)-sigma**2      
      fp = gravity*tanh(kh)+gravity*kh*(1.0_RKIND-tanh(kh)**2)
      knew = k - f/fp
 
      if (abs(knew-k) < tol) then
        exit
      end if

      k = knew

    end do
    
    !--------------------------------------------------------------------

  end function wavenumber!}}}

!***********************************************************************
!
!  routine ocn_init_validate_sloped_beach
!
!> \brief   Validation for radiation stress sloped beach test case
!> \author  Steven Brus 
!> \date    August 2019
!> \details
!>  This routine validates the configuration options for the sloped_beach test case.
!
!-----------------------------------------------------------------------
   subroutine ocn_init_validate_sloped_beach(configPool, packagePool, iocontext, iErr)!{{{

   !--------------------------------------------------------------------

      type (mpas_pool_type), intent(inout) :: configPool, packagePool
      type (mpas_io_context_type), intent(inout) :: iocontext

      integer, intent(out) :: iErr

      character (len=StrKIND), pointer :: config_init_configuration
      integer, pointer :: config_sloped_beach_vert_levels, config_vert_levels

      iErr = 0

      call mpas_pool_get_config(configPool, 'config_init_configuration', config_init_configuration)

      if(config_init_configuration .ne. trim('sloped_beach')) return

      call mpas_pool_get_config(configPool, 'config_vert_levels', config_vert_levels)
      call mpas_pool_get_config(configPool, 'config_sloped_beach_vert_levels', config_sloped_beach_vert_levels)

      if(config_vert_levels <= 0 .and. config_sloped_beach_vert_levels > 0) then
         config_vert_levels = config_sloped_beach_vert_levels
         !call mpas_log_write( 'Using value of $i', intArgs=(/config_vert_levels /))
      else if(config_vert_levels <= 0) then
         call mpas_log_write( 'Validation failed for sloped_beach testcase. ' &
           // 'Not given a usable value for vertical levels.',MPAS_LOG_CRIT)
         iErr = 1
      end if

   !--------------------------------------------------------------------

   end subroutine ocn_init_validate_sloped_beach!}}}


!***********************************************************************

end module ocn_init_sloped_beach

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! vim: foldmethod=marker

