! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  ocn_vel_pressure_grad
!
!> \brief MPAS ocean pressure gradient module
!> \author Mark Petersen, Phil Jones, Rob Aulwes
!> \date   September 2011, updated May 2020
!> \details
!>  This module contains routines for computing velocity
!>  tendency from the horizontal pressure gradient.
!>
!-----------------------------------------------------------------------

module ocn_vel_pressure_grad

   use mpas_timer
   use mpas_derived_types
   use mpas_pool_routines
   use mpas_constants
   use mpas_log

   use ocn_constants
   use ocn_config
   use ocn_mesh

   implicit none
   private
   save

   !--------------------------------------------------------------------
   ! Public parameters
   !--------------------------------------------------------------------

   ! Supported method for pressure gradient
   integer, public, parameter ::         &
      ocnPGradMethodNone            = 0, &!< No press gradient computed
      ocnPGradMethodSSH             = 1, &!< ssh gradient
      ocnPGradMethodPZMid           = 2, &!< pressure and zmid
      ocnPGradMethodMontgomery      = 3, &!< Montgomery potential
      ocnPGradMethodMontgomeryDens  = 4, &!< Montgomery Pot anddensity
      ocnPGradMethodJacobianDens    = 5, &!< Jacobian from density
      ocnPGradMethodJacobianTS      = 6   !< Jacobian from T and S

   integer, public :: &
      ocnPGradMethod  !< Selected method for computing pressure gradient

   !--------------------------------------------------------------------
   ! Public member functions
   !--------------------------------------------------------------------

   public :: ocn_vel_pressure_grad_tend, &
             ocn_vel_pressure_grad_init

   !--------------------------------------------------------------------
   ! Private module variables
   !--------------------------------------------------------------------

   logical :: pgradOn  !< flag to determine if press gradient computed

   real (kind=RKIND) :: density0Inv  !< inverse reference density rho_0
   real (kind=RKIND) :: gdensity0Inv !< gravity (g)/rho_0
   real (kind=RKIND) :: pGradLvlWgt  !< level weighting factor

!***********************************************************************

contains

!***********************************************************************
!
!  routine ocn_vel_pressure_grad_tend
!
!> \brief   Computes tendency term for horizontal pressure gradient
!> \author  Mark Petersen, Phil Jones, Rob Aulwes
!> \date    February 2014, updated May 2020
!> \details
!>  This routine computes the pressure gradient tendency for momentum
!>  based on current state.
!
!-----------------------------------------------------------------------

   subroutine ocn_vel_pressure_grad_tend(ssh, pressure,              &
               montgomeryPotential, zMid, density, potentialDensity, &
               indexT, indexS, tracers, inSituThermalExpansionCoeff, &
               inSituSalineContractionCoeff, tend, err)

      !-----------------------------------------------------------------
      ! input variables
      !-----------------------------------------------------------------

      integer, intent(in) :: indexT !< [in] temperature index in tracer
      integer, intent(in) :: indexS !< [in] salt index in tracer array

      real (kind=RKIND), dimension(:), intent(in) :: &
         ssh             !< [in] Sea surface height

      real (kind=RKIND), dimension(:,:), intent(in) :: &
         pressure,            & !< [in] Pressure field
         montgomeryPotential, & !< [in] Mongomery potential
         zMid,                & !< [in] z-coord at mid-depth of layer
         density,             & !< [in] density
         potentialDensity       !< [in] potentialDensity

      real (kind=RKIND), dimension(:,:), intent(in) :: &
         inSituThermalExpansionCoeff, &!< [in] EOS derivatives
         inSituSalineContractionCoeff

      real (kind=RKIND), dimension(:,:,:), intent(in) :: &
         tracers         !< [in] tracer array

      !-----------------------------------------------------------------
      ! input/output variables
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(:,:), intent(inout) :: &
         tend            !< [inout] velocity tendency

      !-----------------------------------------------------------------
      ! output variables
      !-----------------------------------------------------------------

      integer, intent(out) :: err !< [out] error flag

      !{{{
      !-----------------------------------------------------------------
      ! local variables
      !-----------------------------------------------------------------

      integer ::           &
         iCell, iEdge, k,  &! loop indices for cells, edges, vertical
         kMax,             &! bottom vert level
         cell1, cell2       ! neighbor cell indices

      real (kind=RKIND) :: &! useful scalar temps
         Area,             &! area for Jacobian interpolation
         TL, TR, SL, SR,   &! temporary neighbor T,S values
         rhoL, rhoR,       &! temporary neighbor density values
         zStar, zC, zGamma,&! depth related temporaries
         z1, z2, z1m, z2m, &! depth related interpolation vars
         invdcEdge,        &! 1/dcEdge
         pGrad,            &! accumulated pressure gradiant
         alpha, beta,      &! equation of state coefficients
         JacobianDxDs       ! local Jacobian result

      real (kind=RKIND), dimension(:,:), allocatable :: &
         Temp, Salt,             &! temp and salt temporaries
         pGradLvl,               &! temp for the level contrib to pgrad
         JacobianDxDsZ,          &! Jacobian at every point
         JacobianTz, JacobianSz   ! temps to calc Jacobian

      ! end of preamble
      !----------------
      ! begin code

      ! start up or return if pgrad is not on
      err = 0

      if (.not. pgradOn) return

      call mpas_timer_start("pressure grad")

      !-----------------------------------------------------------------
      ! Compute pressure gradient based on selected method
      !-----------------------------------------------------------------
      select case (ocnPGradMethod)

      !-----------------------------------------------------------------
      case (ocnPGradMethodSSH)   ! ssh gradient

         ! pressure from sea surface height
         ! - g grad ssh

#ifdef MPAS_OPENACC
         !$acc parallel loop &
         !$acc      present(ssh, tend, cellsOnEdge,  &
         !$acc              maxLevelEdgeTop, dcEdge) &
         !$acc      private(cell1, cell2, invdcEdge, k)
#else
         !$omp parallel do schedule(runtime) &
         !$omp    private(cell1, cell2, invdcEdge, k)
#endif
         do iEdge=1,nEdgesOwned
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)
            invdcEdge = gravity / dcEdge(iEdge)

            do k=1,maxLevelEdgeTop(iEdge)
               tend(k,iEdge) = tend(k,iEdge) - invdcEdge* &
                               ( ssh(cell2) - ssh(cell1) )
            end do
         end do
#ifndef MPAS_OPENACC
         !$omp end parallel do
#endif

      !-----------------------------------------------------------------
      case (ocnPGradMethodPZMid)  !  pressure and zmid

         ! pressure for generalized coordinates
         ! -1/density_0 (grad p_k + density g grad z_k^{mid})

#ifdef MPAS_OPENACC
         !$acc parallel loop &
         !$acc      present(tend, pressure, density, zmid, &
         !$acc              cellsOnEdge, dcEdge, maxLevelEdgeTop) &
         !$acc      private(cell1, cell2, invdcEdge, k)
#else
         !$omp parallel do schedule(runtime) &
         !$omp    private(cell1, cell2, invdcEdge, k)
#endif
         do iEdge=1,nEdgesOwned
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)
            invdcEdge = 1.0_RKIND / dcEdge(iEdge)

            do k=1,maxLevelEdgeTop(iEdge)
               tend(k,iEdge) = tend(k,iEdge) + invdcEdge*( &
                             - density0Inv* &
                               (pressure(k,cell2)-pressure(k,cell1)) &
                             - gdensity0Inv*0.5_RKIND* &
                               (density(k,cell1)+density(k,cell2))* &
                               (   zMid(k,cell2)-   zMid(k,cell1)))
            end do
         end do
#ifndef MPAS_OPENACC
         !$omp end parallel do
#endif

      !-----------------------------------------------------------------
      case (ocnPGradMethodMontgomery)  ! Montgomery potential

         ! For pure isopycnal coordinates, this is just grad(M),
         ! the gradient of Montgomery Potential

#ifdef MPAS_OPENACC
         !$acc parallel loop &
         !$acc      present(tend, montgomeryPotential,            &
         !$acc              cellsOnEdge, maxLevelEdgeTop, dcEdge) & 
         !$acc      private(cell1, cell2, invdcEdge, k)
#else
         !$omp parallel do schedule(runtime) &
         !$omp      private(cell1, cell2, invdcEdge, k)
#endif
         do iEdge=1,nEdgesOwned
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)
            invdcEdge = 1.0_RKIND / dcEdge(iEdge)

            do k=1,maxLevelEdgeTop(iEdge)
               tend(k,iEdge) = tend(k,iEdge) + invdcEdge*( &
                             - (montgomeryPotential(k,cell2) - &
                                montgomeryPotential(k,cell1)))
            end do
         end do
#ifndef MPAS_OPENACC
         !$omp end parallel do
#endif

      !-----------------------------------------------------------------
      case (ocnPGradMethodMontgomeryDens) ! Montgomery Pot and density

         ! This formulation has not been extensively tested and is not
         ! supported at this time.

         ! This is -grad(M)+p grad(1/rho)
         ! Where rho is the potential density.
         ! See Bleck (2002) equation 1, and last equation in Appendix A.

#ifdef MPAS_OPENACC
         !$acc parallel loop &
         !$acc    present(tend, montgomeryPotential,  &
         !$acc            pressure, potentialDensity, &
         !$acc            cellsOnEdge, maxLevelEdgeTop, dcEdge) &
         !$acc    private(cell1, cell2, invdcEdge, k)
#else
         !$omp parallel do schedule(runtime) &
         !$omp    private(cell1, cell2, invdcEdge, k)
#endif
         do iEdge=1,nEdgesOwned
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)
            invdcEdge = 1.0_RKIND / dcEdge(iEdge)

            do k=1,maxLevelEdgeTop(iEdge)
               tend(k,iEdge) = tend(k,iEdge) + invdcEdge*( &
                             - (montgomeryPotential(k,cell2) - &
                                montgomeryPotential(k,cell1)) &
                  +  0.5_RKIND*(pressure(k,cell1)+pressure(k,cell2))* &
                               (1.0_RKIND/potentialDensity(k,cell2) &
                              - 1.0_RKIND/potentialDensity(k,cell1)) )
            end do
         end do
#ifndef MPAS_OPENACC
         !$omp end parallel do
#endif

      !-----------------------------------------------------------------
      case (ocnPGradMethodJacobianDens) ! Jacobian from density

         allocate(JacobianDxDsZ(nVertLevels,nEdgesOwned), &
                       pGradLvl(nVertLevels,nEdgesOwned))

         !$acc enter data create(JacobianDxDsZ, pGradLvl)

#ifdef MPAS_OPENACC
         !$acc parallel loop &
         !$acc    present(cellsOnEdge, maxLevelEdgeTop, zMid, &
         !$acc            density, JacobianDxDsZ)             &
         !$acc    private(k, cell1, cell2, z1, z1m, z2, z2m,  &
         !$acc            Area, zStar, zC, zGamma, rhoL, rhoR)
#else
         !$omp parallel do schedule(runtime) &
         !$omp    private(k, cell1, cell2, z1, z1m, z2, z2m,  &
         !$omp            Area, zStar, zC, zGamma, rhoL, rhoR)
#endif
         do iEdge=1,nEdgesOwned
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)

            ! compute J(rho,z) as in Shchepetkin and McWilliams (2003) (7.16)
            ! the following is an in-lined version of the
            ! pGrad_Jacobian_common_level routine
            do k=2,maxLevelEdgeTop(iEdge)

               ! define some vert coordinate values
               z1  = zMid(k  ,Cell1)
               z1m = zMid(k-1,Cell1)
               z2  = zMid(k  ,Cell2)
               z2m = zMid(k-1,Cell2)

               ! eqn 2.7 in Shchepetkin and McWilliams (2003)
               ! Note delta x was removed.  It must be an error in the paper,
               ! as it makes the units incorrect.
               Area = 0.5_RKIND*(z1m - z1 + z2m - z2 )

               ! eqn 2.8
               zStar = (z2m*z1m - z2*z1)/ &
                       (z2m - z2 + z1m - z1)

               ! eqn 3.2
               zC = 0.25_RKIND*( z1 + z1m + z2 + z2m )

               ! eqn 4.1
               zGamma = (1.0_RKIND - pGradLvlWgt)*zStar + &
                                     pGradLvlWgt *zC

               rhoL = (density(k  ,cell1)*(z1m-zGamma) + &
                       density(k-1,cell1)*(zGamma-z1))/(z1m - z1)
               rhoR = (density(k  ,cell2)*(z2m-zGamma) + &
                       density(k-1,cell2)*(zGamma-z2))/(z2m - z2)

               ! eqn 2.6 in Shchepetkin and McWilliams (2003)
               JacobianDxDsZ(k,iEdge) = Area * (rhoL - rhoR)
            end do
         end do
         !$omp end do

#ifdef MPAS_OPENACC
         !$acc parallel loop &
         !$acc    present(cellsOnEdge, edgeMask, dcEdge,     &
         !$acc            pressure, density, zMid, pGradLvl, & 
         !$acc            maxLevelEdgeTop, JacobianDxDsZ)    &
         !$acc    private(k, cell1, cell2, invdcEdge, pGrad)
#else
         !$omp parallel do schedule(runtime) &
         !$omp    private(k, cell1, cell2, invdcEdge, pGrad)
#endif
         do iEdge=1,nEdgesOwned
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)
            invdcEdge = 1.0_RKIND / dcEdge(iEdge)

            ! In layer 1, use pressure for generalized coordinates
            ! pGrad = -1/density_0 (grad p_k + density g grad z_k^{mid})
            k = 1
            pGradLvl(k,iEdge) = edgeMask(k,iEdge) * invdcEdge * ( &
                 - density0Inv * ( pressure(k,cell2) - pressure(k,cell1) ) &
                 - gdensity0Inv * 0.5_RKIND* &
                           (density(k,cell1)+density(k,cell2)) * &
                           ( zMid(k,cell2) - zMid(k,cell1) ) )


            do k=2,maxLevelEdgeTop(iEdge)

               ! note JacobianDxDs includes negative sign, so
               ! pGrad is - g/rho_0 dP/dx

               pGradLvl(k,iEdge) = &
                     gdensity0Inv*JacobianDxDsZ(k,iEdge)*invdcEdge

            end do
         end do
#ifndef MPAS_OPENACC
         !$omp end parallel do
#endif

         ! the final pressure gradient tendency is an integral from the 
         ! surface down of each of the layer contributions
#ifdef MPAS_OPENACC
         !$acc parallel loop &
         !$acc    present(tend, pGradLvl, maxLevelEdgeTop) &
         !$acc    private(k, pGrad)
#else
         !$omp parallel do schedule(runtime)
         !$acc    private(k, pGrad)
#endif
         do iEdge=1,nEdgesOwned
            pGrad = pGradLvl(1,iEdge)
            tend(1,iEdge) = tend(1,iEdge) + pGrad

            do k=2,maxLevelEdgeTop(iEdge)
               pGrad = pGrad + pGradLvl(k,iEdge)
               tend(k,iEdge) = tend(k,iEdge) + pGrad
            end do
         end do
#ifndef MPAS_OPENACC
         !$omp end parallel do
#endif

         !$acc exit data delete(JacobianDxDsZ, pGradLvl)
         deallocate(JacobianDxDsZ, pGradLvl)

      !-----------------------------------------------------------------
      case (ocnPGradMethodJacobianTS) ! Jacobian from T and S

         ! Allocate local vars
         allocate(Temp(nVertLevels,nCellsAll), &
                  Salt(nVertLevels,nCellsAll), &
                  pGradLvl  (nVertLevels,nEdgesOwned), &
                  JacobianTz(nVertLevels,nEdgesOwned), &
                  JacobianSz(nVertLevels,nEdgesOwned))

         !$acc enter data &
         !$acc    create(Temp, Salt, JacobianTz, JacobianSz, pGradLvl)

         ! Extract temperature and salinity
#ifdef MPAS_OPENACC
         !$acc parallel loop collapse(2) &
         !$acc    present(tracers, Temp, Salt)
#else
         !$omp parallel
         !$omp do schedule(runtime), private(k)
#endif
         do iCell=1,nCellsAll
         do k=1,nVertLevels
            Temp(k,iCell) = tracers(indexT,k,iCell)
            Salt(k,iCell) = tracers(indexS,k,iCell)
         end do
         end do
#ifndef MPAS_OPENACC
         !$omp end do
#endif

         ! compute J(T,z) and J(S,z) in Shchepetkin and McWilliams (2003) (7.16)
         ! the following is an in-lined version of the
         ! pGrad_Jacobian_common_level routine
#ifdef MPAS_OPENACC
         !$acc parallel loop &
         !$acc    present(Temp, Salt, JacobianTz, JacobianSz, zMid,   &
         !$acc            cellsOnEdge, maxLevelEdgeTop)  &
         !$acc    private(k, kMax, cell1, cell2, z1, z1m, z2, z2m,    &
         !$acc            Area, zStar, zC, zGamma, TL, TR, SL, SR) 
#else
         !$omp do schedule(runtime) &
         !$omp    private(k, kMax, cell1, cell2, z1, z1m, z2, z2m,    &
         !$omp            Area, zStar, zC, zGamma, TL, TR, SL, SR) 
#endif
         do iEdge=1,nEdgesOwned
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)
            kMax = maxLevelEdgeTop(iEdge)

            ! compute J(T,z) and J(S,z) in Shchepetkin and McWilliams (2003) (7.16)
            ! the following is an in-lined version of the
            ! pGrad_Jacobian_common_level routine
            do k=2,kMax

               ! define some vert coordinate values
               z1  = zMid(k  ,Cell1)
               z1m = zMid(k-1,Cell1)
               z2  = zMid(k  ,Cell2)
               z2m = zMid(k-1,Cell2)

               ! eqn 2.7 in Shchepetkin and McWilliams (2003)
               ! Note delta x was removed.  It must be an error in the paper,
               ! as it makes the units incorrect.
               Area = 0.5_RKIND*(z1m - z1 + z2m - z2 )

               ! eqn 2.8
               zStar = (z2m*z1m - z2*z1)/ &
                       (z2m -z2 + z1m -z1)

               ! eqn 3.2
               zC = 0.25_RKIND*( z1 + z1m + z2 + z2m )

               ! eqn 4.1
               zGamma = (1.0_RKIND - pGradLvlWgt)*zStar + &
                                     pGradLvlWgt *zC

               TL = (Temp(k  ,cell1)*(z1m-zGamma) + &
                     Temp(k-1,cell1)*(zGamma-z1))/(z1m - z1)
               TR = (Temp(k  ,cell2)*(z2m-zGamma) + &
                     Temp(k-1,cell2)*(zGamma-z2))/(z2m - z2)

               SL = (Salt(k  ,cell1)*(z1m-zGamma) + &
                     Salt(k-1,cell1)*(zGamma-z1))/(z1m - z1)
               SR = (Salt(k  ,cell2)*(z2m-zGamma) + &
                     Salt(k-1,cell2)*(zGamma-z2))/(z2m - z2)

               ! eqn 2.6 in Shchepetkin and McWilliams (2003)
               JacobianTz(k,iEdge) = Area * (TL - TR)
               JacobianSz(k,iEdge) = Area * (SL - SR)
            end do
         end do
#ifndef MPAS_OPENACC
         !$omp do schedule(runtime)
#endif

#ifdef MPAS_OPENACC
         !$acc parallel loop &
         !$acc    present(pressure, density, JacobianTz, JacobianSz,  &
         !$acc            pGradLvl, zMid,  dcEdge, edgeMask,          &
         !$acc            cellsOnEdge, maxLevelEdgeTop,  &
         !$acc            inSituThermalExpansionCoeff,   &
         !$acc            inSituSalineContractionCoeff)  &
         !$acc    private(k, kMax, cell1, cell2, invdcEdge, &
         !$acc            alpha, beta, JacobianDxDs)
#else
         !$omp do schedule(runtime) &
         !$omp    private(k, kMax, cell1, cell2, invdcEdge, &
         !$omp            alpha, beta, JacobianDxDs)
#endif
         do iEdge=1,nEdgesOwned
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)
            invdcEdge = 1.0_RKIND / dcEdge(iEdge)
            kMax = maxLevelEdgeTop(iEdge)
            pGradLvl(:,iEdge) = 0.0_RKIND

            ! In layer 1, use pressure for generalized coordinates
            ! pGrad = -1/density_0 (grad p_k + density g grad z_k^{mid})
            k = 1
            pGradLvl(k,iEdge) = edgeMask(k,iEdge) * invdcEdge * ( &
                 -  density0Inv*( pressure(k,cell2) - pressure(k,cell1) ) &
                 - gdensity0Inv*0.5_RKIND*(density(k,cell1)+density(k,cell2))*&
                                          (   zMid(k,cell2)-   zMid(k,cell1)))

            do k=2,kMax
               ! Average alpha and beta over four data points of the Jacobian cell.
               ! Note that inSituThermalExpansionCoeff and 
               ! inSituSalineContractionCoeff include a 1/density factor,
               ! so must multiplied by density here.
               alpha = 0.25_RKIND* &
                     ( density(k  ,cell1)* &
                       inSituThermalExpansionCoeff (k  ,cell1) &
                     + density(k-1,cell1)* &
                       inSituThermalExpansionCoeff (k-1,cell1) &
                     + density(k  ,cell2)* &
                       inSituThermalExpansionCoeff (k,cell2) &
                     + density(k-1,cell2)* &
                       inSituThermalExpansionCoeff (k-1,cell2) )
               beta  = 0.25_RKIND* &
                     ( density(k  ,cell1)* &
                       inSituSalineContractionCoeff(k,cell1) &
                     + density(k-1,cell1)* &
                       inSituSalineContractionCoeff(k-1,cell1) &
                     + density(k  ,cell2)* &
                       inSituSalineContractionCoeff(k,cell2) &
                     + density(k-1,cell2)* &
                       inSituSalineContractionCoeff(k-1,cell2) )

               ! Shchepetkin and McWilliams (2003) (7.16)
               JacobianDxDs = -alpha*JacobianTz(k,iEdge) + &
                                beta*JacobianSz(k,iEdge)

               ! the contribution to the pressure gradient from this lvl
               ! note JacobianDxDs includes negative sign, so
               ! pGrad is - g/rho_0 dP/dx
               pGradLvl(k,iEdge) = gdensity0Inv*JacobianDxDs*invdcEdge
            end do
         end do
#ifndef MPAS_OPENACC
         !$omp end do
#endif

         ! the final pressure gradient tendency is an integral from the 
         ! surface down of each of the layer contributions
#ifdef MPAS_OPENACC
         !$acc parallel loop &
         !$acc    present(pGradLvl, tend, maxLevelEdgeTop)  &
         !$acc    private(k, kMax, pGrad)
#else
         !$omp do schedule(runtime)
#endif
         do iEdge=1,nEdgesOwned
            kMax = maxLevelEdgeTop(iEdge)

            pGrad = pGradLvl(1,iEdge)
            tend(1,iEdge) = tend(1,iEdge) + pGrad

            do k=2,kMax
               pGrad = pGrad + pGradLvl(k,iEdge)
               tend(k,iEdge) = tend(k,iEdge) + pGrad
            end do
         end do
#ifndef MPAS_OPENACC
         !$omp end do
         !$omp end parallel
#endif

#ifdef MPAS_OPENACC
         !$acc exit data &
         !$acc   delete(Temp, Salt, JacobianTz, JacobianSz, pGradLvl)
#endif

         deallocate(Temp, Salt, JacobianTz, JacobianSz, pGradLvl)

      !-----------------------------------------------------------------
      case (ocnPGradMethodNone) ! No press gradient computed

         ! Do not add anything to tendency

      case default

         ! Bad selection - caught during initialization
         err = 1

      end select

      call mpas_timer_stop("pressure grad")

   !--------------------------------------------------------------------

   end subroutine ocn_vel_pressure_grad_tend!}}}

!***********************************************************************
!
!  routine pGrad_Jacobian_common_level
!
!> \brief   Computes density-Jacobian
!> \author  Mark Petersen
!> \date    February 2014
!> \details
!>  This routine computes the density-Jacobian in common_level form.
!>  See Shchepetkin and McWilliams (2003) Ocean Modeling, sections 2-4
!
!-----------------------------------------------------------------------

   subroutine pGrad_Jacobian_common_level(rho1,rho2,z1,z2,kMax,gamma,JacobianDxDs)

      !-----------------------------------------------------------------
      ! input variables
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(:), intent(in) :: &
         rho1, & ! density of column 1
         rho2, & ! density of column 2
         z1,   & ! z-coordinate at middle of cell, column 1
         z2      ! z-coordinate at middle of cell, column 2

      real (kind=RKIND), intent(in) :: &
         gamma   ! weight between zStar (original Jacobian) and z_C (weighted Jacobian)

      integer, intent(in) :: &
         kMax

      !-----------------------------------------------------------------
      ! input/output variables
      !-----------------------------------------------------------------

      !-----------------------------------------------------------------
      ! output variables
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(:), intent(out) :: &
         JacobianDxDs  ! - Delta x Delta s J(rho,z)

      !-----------------------------------------------------------------
      ! local variables
      !-----------------------------------------------------------------

      integer :: k
      real (kind=RKIND) :: Area, zStar, rhoL, rhoR, zC, zGamma

      JacobianDxDs = 0.0_RKIND

      do k=2,kMax

         ! eqn 2.7 in Shchepetkin and McWilliams (2003)
         ! Note delta x was removed.  It must be an error in the paper,
         ! as it makes the units incorrect.
         Area = 0.5_RKIND*(z1(k-1) - z1(k) + z2(k-1) - z2(k) )

         ! eqn 2.8
         zStar = (z2(k-1)*z1(k-1) - z2(k)*z1(k) )/ &
                 (z2(k-1)-z2(k)+z1(k-1)-z1(k))

         ! eqn 3.2
         zC = 0.25_RKIND*( z1(k) + z1(k-1) + z2(k) + z2(k-1) )

         ! eqn 4.1
         zGamma = (1.0_RKIND - gamma)*zStar + gamma*zC

         rhoL = (rho1(k)*(z1(k-1)-zGamma) + rho1(k-1)*(zGamma-z1(k)))/&
                (z1(k-1) - z1(k))
         rhoR = (rho2(k)*(z2(k-1)-zGamma) + rho2(k-1)*(zGamma-z2(k)))/&
                (z2(k-1) - z2(k))

         ! eqn 2.6 in Shchepetkin and McWilliams (2003)
         JacobianDxDs(k) = Area * (rhoL - rhoR)
      end do

   end subroutine pGrad_Jacobian_common_level

!***********************************************************************
!
!  routine pGrad_Jacobian_POM_SCRUM
!
!> \brief   Computes density-Jacobian
!> \author  Mark Petersen
!> \date    February 2014
!> \details
!>  This routine computes the density-Jacobian in POM/SCRUM form.
!>  See Shchepetkin and McWilliams (2003) Ocean Modeling, section 2.
!
!-----------------------------------------------------------------------

   subroutine pGrad_Jacobian_POM_SCRUM(rho1,rho2,z1,z2,kMax,JacobianDxDs)

      !-----------------------------------------------------------------
      !
      ! input variables
      !
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(:), intent(in) :: &
         rho1, & ! density of column 1
         rho2, & ! density of column 2
         z1,   & ! z-coordinate at middle of cell, column 1
         z2      ! z-coordinate at middle of cell, column 2

      integer, intent(in) :: &
         kMax  ! maximum level

      !-----------------------------------------------------------------
      !
      ! input/output variables
      !
      !-----------------------------------------------------------------

      !-----------------------------------------------------------------
      !
      ! output variables
      !
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(:), intent(out) :: &
         JacobianDxDs  ! - Delta x Delta s J(rho,z)

      !-----------------------------------------------------------------
      !
      ! local variables
      !
      !-----------------------------------------------------------------

      integer :: k

      JacobianDxDs = 0.0_RKIND

      do k=2,kMax

         ! eqn 2.3 in Shchepetkin and McWilliams (2003)
         JacobianDxDs(k) = 0.25_RKIND*(&
              (rho1(k) + rho1(k-1) - rho2(k) - rho2(k-1) )*(z1(k-1) - z1(k) + z2(k-1) - z2(k) ) &
            - (rho1(k-1) - rho1(k) + rho2(k-1) - rho2(k) )*(z1(k) + z1(k-1) - z2(k) - z2(k-1) ) )
      end do

   end subroutine pGrad_Jacobian_POM_SCRUM

!***********************************************************************
!
!  routine pGrad_Jacobian_diagonal
!
!> \brief   Computes density-Jacobian
!> \author  Mark Petersen
!> \date    February 2014
!> \details
!>  This routine computes the density-Jacobian in diagonal form.
!>  See Shchepetkin and McWilliams (2003) Ocean Modeling, section 2.
!
!-----------------------------------------------------------------------

   subroutine pGrad_Jacobian_diagonal(rho1,rho2,z1,z2,kMax,JacobianDxDs)

      !-----------------------------------------------------------------
      !
      ! input variables
      !
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(:), intent(in) :: &
         rho1, & ! density of column 1
         rho2, & ! density of column 2
         z1,   & ! z-coordinate at middle of cell, column 1
         z2      ! z-coordinate at middle of cell, column 2

      integer, intent(in) :: &
         kMax

      !-----------------------------------------------------------------
      !
      ! input/output variables
      !
      !-----------------------------------------------------------------

      !-----------------------------------------------------------------
      !
      ! output variables
      !
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(:), intent(out) :: &
         JacobianDxDs  ! - Delta x Delta s J(rho,z)

      !-----------------------------------------------------------------
      !
      ! local variables
      !
      !-----------------------------------------------------------------

      integer :: k

      JacobianDxDs = 0.0_RKIND

      do k=2,kMax

         ! eqn 2.5 in Shchepetkin and McWilliams (2003)
         JacobianDxDs(k) = 0.5_RKIND*( &
              (rho1(k-1) - rho2(k))*(z2(k-1) - z1(k) ) &
            + (rho1(k) - rho2(k-1))*(z1(k-1) - z2(k)) )
      end do

   end subroutine pGrad_Jacobian_diagonal

!***********************************************************************
!
!  routine pGrad_Jacobian_pseudo_flux
!
!> \brief   Computes density-Jacobian
!> \author  Mark Petersen
!> \date    February 2014
!> \details
!>  This routine computes the density-Jacobian in pseudo_flux form.
!>  See Shchepetkin and McWilliams (2003) Ocean Modeling, section 2.
!
!-----------------------------------------------------------------------

   subroutine pGrad_Jacobian_pseudo_flux(rho1,rho2,z1,z2,kMax,JacobianDxDs)

      !-----------------------------------------------------------------
      !
      ! input variables
      !
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(:), intent(in) :: &
         rho1, & ! density of column 1
         rho2, & ! density of column 2
         z1,   & ! z-coordinate at middle of cell, column 1
         z2      ! z-coordinate at middle of cell, column 2

      integer, intent(in) :: &
         kMax

      !-----------------------------------------------------------------
      !
      ! input/output variables
      !
      !-----------------------------------------------------------------

      !-----------------------------------------------------------------
      !
      ! output variables
      !
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(:), intent(out) :: &
         JacobianDxDs  ! - Delta x Delta s J(rho,z)

      !-----------------------------------------------------------------
      !
      ! local variables
      !
      !-----------------------------------------------------------------

      integer :: k
      real (kind=RKIND) :: FLeft, FTop, FRight, FBottom

      JacobianDxDs = 0.0_RKIND

      do k=2,kMax

         FLeft   = 0.5_RKIND*( rho1(k) + rho1(k-1) ) * (z1(k-1) - z1(k))
         FTop    = 0.5_RKIND*( rho1(k-1) + rho2(k-1) ) * (z2(k-1) - z1(k-1))
         FRight  = 0.5_RKIND*( rho2(k) + rho2(k-1) ) * (z2(k-1) - z2(k))
         FBottom = 0.5_RKIND*( rho1(k) + rho2(k) ) * (z2(k) - z1(k))

         ! eqn 2.11 in Shchepetkin and McWilliams (2003)
         JacobianDxDs(k) = FLeft + FTop - FRight - FBottom
      end do

   end subroutine pGrad_Jacobian_pseudo_flux

!***********************************************************************
!
!  routine ocn_vel_pressure_grad_init
!
!> \brief   Initializes ocean momentum horizontal pressure gradient
!> \author  Mark Petersen, Phil Jones, Rob Aulwes
!> \date    September 2011, updated May 2020
!> \details
!>  This routine initializes parameters required for the computation
!>  of the horizontal pressure gradient.
!
!-----------------------------------------------------------------------

   subroutine ocn_vel_pressure_grad_init(err)

      !-----------------------------------------------------------------
      ! Output Variables
      !-----------------------------------------------------------------

      integer, intent(out) :: err !< [out] error flag

      !{{{
      !-----------------------------------------------------------------
      ! Set some default values for module constants
      !-----------------------------------------------------------------

      err = 0

      pgradOn         = .true.
      ocnPGradMethod  = ocnPGradMethodNone

      ! Set some common constants
      density0Inv  = 1.0_RKIND / rho_sw
      gdensity0Inv = gravity / rho_sw
      pGradLvlWgt  = config_common_level_weight

      !-----------------------------------------------------------------
      ! if pressure gradient disabled, turn it off and ignore the rest
      !-----------------------------------------------------------------

      if (config_disable_vel_pgrad) then
         pgradOn = .false.
         ocnPGradMethod  = ocnPGradMethodNone
         call mpas_log_write(' Pressure gradient is disabled ')
         return
      endif

      !-----------------------------------------------------------------
      ! Determine choice of method for computing pressure gradient
      !-----------------------------------------------------------------

      call mpas_log_write( &
         ' Pressure type is: '//trim(config_pressure_gradient_type))

      select case (trim(config_pressure_gradient_type))
      case ('ssh_gradient')
         ocnPGradMethod  = ocnPGradMethodSSH
      case ('pressure_and_zmid')
         ocnPGradMethod  = ocnPGradMethodPZMid
      case ('MontgomeryPotential')
         ocnPGradMethod  = ocnPGradMethodMontgomery
      case ('MontgomeryPotential_and_density')
         ocnPGradMethod  = ocnPGradMethodMontgomeryDens
         call mpas_log_write( &
           ' Montgomery and density for pgrad not fully supported.', &
           MPAS_LOG_WARN)
      case ('Jacobian_from_density')
         ocnPGradMethod  = ocnPGradMethodJacobianDens
      case ('Jacobian_from_TS')
         ocnPGradMethod  = ocnPGradMethodJacobianTS
      case default
         ocnPGradMethod  = ocnPGradMethodNone
         call mpas_log_write( &
            ' Incorrect choice of config_pressure_gradient_type.', &
            MPAS_LOG_CRIT)
      end select

   !--------------------------------------------------------------------

   end subroutine ocn_vel_pressure_grad_init!}}}

!***********************************************************************

end module ocn_vel_pressure_grad

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
