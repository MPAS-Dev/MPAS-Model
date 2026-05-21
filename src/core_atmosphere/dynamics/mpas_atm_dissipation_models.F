! Copyright (c) 2026, The University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!

#define COMMA ,
#define DEBUG_WRITE(M) ! call mpas_log_write(M)

module mpas_atm_dissipation_models

   use mpas_kind_types, only : RKIND
   use mpas_constants
   use mpas_log
   use mpas_timekeeping, only : mpas_get_clock_time, mpas_get_time
   use mpas_derived_types, only : MPAS_Clock_type, MPAS_Time_type, MPAS_NOW, MPAS_LOG_CRIT

   logical, parameter :: les_test = .true., les_sas_test = .false.
   !! real (kind=RKIND), parameter :: tke_heat_flux = 0.03  ! shear case from Moeng et al., first hour
   ! real (kind=RKIND), parameter :: tke_heat_flux = 0.03
   !  real (kind=RKIND), parameter :: tke_heat_flux = 0.0
   !! real (kind=RKIND), parameter :: tke_drag_coefficient = 0.0013 ! ocean roughness length
   ! real (kind=RKIND), parameter :: tke_drag_coefficient = 0.006
   ! real (kind=RKIND), parameter :: tke_drag_coefficient = 0.0
   real (kind=RKIND), parameter :: epsilon_bv = 1.e-06
   ! real (kind=RKIND), parameter :: c_k = 0.1
   real (kind=RKIND), parameter :: c_k = 0.25

   integer, parameter :: LES_INVALID_OPT = -1

   integer, parameter :: LES_MODEL_NONE = 0, &
                         LES_MODEL_3D_SMAGORINSKY = 1, &
                         LES_MODEL_PROGNOSTIC_15_ORDER = 2

   integer, parameter :: LES_SURFACE_NONE = 0, &
                         LES_SURFACE_SPECIFIED = 1, &
                         LES_SURFACE_VARYING = 2

contains


   !-----------------------------------------------------------------------
   !  routine les_model_from_string
   !
   !> \brief Converts an LES model option from a string to an integer parameter
   !> \author Michael Duda
   !> \date   13 February 2026
   !> \details
   !>  Given a string that contains the name of a valid LES model option, this
   !>  routine returns an integer parameter corresponding to that option.
   !>
   !>  If the given string is not recognized as a valid LES model option, the
   !>  integer parameter LES_INVALID_OPT is returned.
   !
   !-----------------------------------------------------------------------
   pure function les_model_from_string(les_model_str) result(les_model_opt)

      implicit none

      ! Arguments
      character(len=*), intent(in) :: les_model_str

      ! Return value
      integer :: les_model_opt


      if (trim(les_model_str) == 'none') then
         les_model_opt = LES_MODEL_NONE
      else if (trim(les_model_str) == '3d_smagorinsky') then
         les_model_opt = LES_MODEL_3D_SMAGORINSKY
      else if (trim(les_model_str) == 'prognostic_1.5_order') then
         les_model_opt = LES_MODEL_PROGNOSTIC_15_ORDER
      else
         les_model_opt = LES_INVALID_OPT
      end if

   end function les_model_from_string


   !-----------------------------------------------------------------------
   !  routine les_surface_from_string
   !
   !> \brief Converts an LES surface option from a string to an integer parameter
   !> \author Michael Duda
   !> \date   13 February 2026
   !> \details
   !>  Given a string that contains the name of a valid LES surface option, this
   !>  routine returns an integer parameter corresponding to that option.
   !>
   !>  If the given string is not recognized as a valid LES surface option, the
   !>  integer parameter LES_INVALID_OPT is returned.
   !
   !-----------------------------------------------------------------------
   pure function les_surface_from_string(les_surface_str) result(les_surface_opt)

      implicit none

      ! Arguments
      character(len=*), intent(in) :: les_surface_str

      ! Return value
      integer :: les_surface_opt


      if (trim(les_surface_str) == 'none') then
         les_surface_opt = LES_SURFACE_NONE
      else if (trim(les_surface_str) == 'specified') then
         les_surface_opt = LES_SURFACE_SPECIFIED
      else if (trim(les_surface_str) == 'varying') then
         les_surface_opt = LES_SURFACE_VARYING
      else
         les_surface_opt = LES_INVALID_OPT
      end if

   end function les_surface_from_string


   subroutine smagorinsky_2d( kdiff, u, v, c_s, config_len_disp,                                 &
                              deformation_coef_c2, deformation_coef_s2, deformation_coef_cs,     &
                              invDt, h_mom_eddy_visc4, config_visc4_2dsmag, h_theta_eddy_visc4,  &
                              cellStart, cellEnd, nEdgesOnCell, edgesOnCell,                     &
                              nCells, nEdges                                                    )

      use mpas_atm_dimensions  !  pull nVertLevels and maxEdges from here

      implicit none

      integer, intent(in) :: cellStart, cellEnd, nCells, nEdges
      real (kind=RKIND), dimension(nVertLevels,nEdges+1), intent(in) :: u
      real (kind=RKIND), dimension(nVertLevels,nEdges+1), intent(in) :: v
      real (kind=RKIND), dimension(maxEdges,nCells+1), intent(in) :: deformation_coef_c2
      real (kind=RKIND), dimension(maxEdges,nCells+1), intent(in) :: deformation_coef_s2
      real (kind=RKIND), dimension(maxEdges,nCells+1), intent(in) :: deformation_coef_cs
      real (kind=RKIND), intent(in) :: c_s, config_len_disp, invDt, config_visc4_2dsmag
      integer, dimension(nCells+1), intent(in) :: nEdgesOnCell
      integer, dimension(maxEdges,nCells+1), intent(in) :: edgesOnCell

      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(out) :: kdiff
      real (kind=RKIND), intent(out) :: h_mom_eddy_visc4, h_theta_eddy_visc4

      ! local variables

      integer :: iCell, iEdge, k
      real (kind=RKIND), dimension(nVertLevels) :: d_11, d_22, d_12, dudx, dudy, dvdx, dvdy


      DEBUG_WRITE(' begin smagorinsky_2d ')

      !$acc enter data create(dudx, dudy, dvdx, dvdy)
      !$acc enter data create(d_11, d_22, d_12)

      !$acc parallel default(present)

      !$acc loop gang worker private(dudx, dudy, dvdx, dvdy, d_11, d_22, d_12)
      do iCell = cellStart,cellEnd

         !$acc loop vector
         do k = 1, nVertLevels
            dudx(k) = 0.0_RKIND
            dudy(k) = 0.0_RKIND
            dvdx(k) = 0.0_RKIND
            dvdy(k) = 0.0_RKIND
         end do

         !$acc loop seq
         do iEdge=1,nEdgesOnCell(iCell)
            !$acc loop vector
            do k=1,nVertLevels
               dudx(k) = dudx(k) + deformation_coef_c2(iEdge,iCell)*u(k,EdgesOnCell(iEdge,iCell)) &
                                 - deformation_coef_cs(iEdge,iCell)*v(k,EdgesOnCell(iEdge,iCell))
               dudy(k) = dudy(k) + deformation_coef_cs(iEdge,iCell)*u(k,EdgesOnCell(iEdge,iCell)) &
                                 - deformation_coef_s2(iEdge,iCell)*v(k,EdgesOnCell(iEdge,iCell))
               dvdx(k) = dvdx(k) + deformation_coef_cs(iEdge,iCell)*u(k,EdgesOnCell(iEdge,iCell)) &
                                 + deformation_coef_c2(iEdge,iCell)*v(k,EdgesOnCell(iEdge,iCell))
               dvdy(k) = dvdy(k) + deformation_coef_s2(iEdge,iCell)*u(k,EdgesOnCell(iEdge,iCell)) &
                                 + deformation_coef_cs(iEdge,iCell)*v(k,EdgesOnCell(iEdge,iCell))
            end do
         end do

!DIR$ IVDEP
         !$acc loop vector
         do k=1, nVertLevels
            ! here is the Smagorinsky formulation,
            ! followed by imposition of an upper bound on the eddy viscosity
            d_11(k) = 2*dudx(k)
            d_22(k) = 2*dvdy(k)
            d_12(k) = dudy(k) + dvdx(k)
            kdiff(k,iCell) = (c_s * config_len_disp)**2 * sqrt(0.25*(d_11(k)-d_22(k))**2 + d_12(k)**2)
            kdiff(k,iCell) = min(kdiff(k,iCell),(0.01*config_len_disp**2) * invDt)
         end do
      end do

      !$acc end parallel

      !$acc exit data delete(dudx, dudy, dvdx, dvdy)
      !$acc exit data delete(d_11, d_22, d_12)

      h_mom_eddy_visc4   = config_visc4_2dsmag * config_len_disp**3
      h_theta_eddy_visc4 = h_mom_eddy_visc4

      DEBUG_WRITE(' exiting smagorinsky_2d ')

   end subroutine smagorinsky_2d

!---------------------------------------

   subroutine les_models( les_model_opt, les_surface_opt, dynamics_substep, eddy_visc_horz, eddy_visc_vert, &
                          u, v, uCell, vCell,                                                &
                          w, c_s, bv_freq2, zgrid, config_len_disp,                          &
                          deformation_coef_c2, deformation_coef_s2, deformation_coef_cs,     &
                          deformation_coef_c, deformation_coef_s, prandtl_3d_inv,            &
                          invDt, h_mom_eddy_visc4, config_visc4_2dsmag, h_theta_eddy_visc4,  &
                          scalars, tend_scalars, index_tke, rho_zz, meshScalingDel2,         &
                          cellStart, cellEnd, nEdgesOnCell, edgesOnCell, cellsOnEdge,        &
                          nCells, nEdges, nVertLevels, maxEdges, num_scalars                )

      implicit none

      integer, intent(in) :: les_model_opt
      integer, intent(in) :: les_surface_opt

      integer, intent(in) :: cellStart, cellEnd, nCells, nEdges, nVertLevels, maxEdges, index_tke, num_scalars
      integer, intent(in) :: dynamics_substep
      real (kind=RKIND), dimension(nVertLevels,nEdges+1), intent(in) :: u
      real (kind=RKIND), dimension(nVertLevels,nEdges+1), intent(in) :: v
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: uCell
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: vCell
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: bv_freq2
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(out) :: prandtl_3d_inv
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: rho_zz
      real (kind=RKIND), dimension(num_scalars,nVertLevels,nCells+1), intent(inout) :: scalars
      real (kind=RKIND), dimension(num_scalars,nVertLevels,nCells+1) :: tend_scalars
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1), intent(in) :: w
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1), intent(in) :: zgrid
      real (kind=RKIND), dimension(maxEdges,nCells+1), intent(in) :: deformation_coef_c2
      real (kind=RKIND), dimension(maxEdges,nCells+1), intent(in) :: deformation_coef_s2
      real (kind=RKIND), dimension(maxEdges,nCells+1), intent(in) :: deformation_coef_cs
      real (kind=RKIND), dimension(maxEdges,nCells+1), intent(in) :: deformation_coef_c
      real (kind=RKIND), dimension(maxEdges,nCells+1), intent(in) :: deformation_coef_s
      real (kind=RKIND), dimension(nEdges+1), intent(in) :: meshScalingDel2
      real (kind=RKIND), intent(in) :: c_s, config_len_disp, invDt, config_visc4_2dsmag
      integer, dimension(nCells+1), intent(in) :: nEdgesOnCell
      integer, dimension(maxEdges,nCells+1), intent(in) :: edgesOnCell
      integer, dimension(2,nEdges+1), intent(in) :: cellsOnEdge

      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(out) :: eddy_visc_horz, eddy_visc_vert
      real (kind=RKIND), intent(out) :: h_mom_eddy_visc4, h_theta_eddy_visc4

      ! local variables

      integer :: iCell, iEdge, k, ie, cell1, cell2
      real (kind=RKIND), dimension(nVertLevels) :: d_11, d_22, d_33, d_12, d_13, d_23
      real (kind=RKIND), dimension(nVertLevels) :: dudx, dudy, dvdx, dvdy
      real (kind=RKIND), dimension(nVertLevels+1) :: dwdx, dwdy
      real (kind=RKIND), dimension(nVertLevels) :: dudz, dvdz, dwdz
      real (kind=RKIND) :: rdz, def2, pr_inv, wk
      real (kind=RKIND) :: shear_production, buoyancy, dissipation, delta_z, delta_s, bv, tke_length, diss_length
      real (kind=RKIND) :: l_horizontal, l_vertical, c_dissipation
      real (kind=RKIND) :: prandtl_horizontal_inv
      real (kind=RKIND) :: eddy_visc_h, eddy_visc_v

      logical, parameter :: test_tke=.true.
      ! real (kind=RKIND), parameter :: epsilon_bv = 1.e-06


      !$acc enter data create(dudx, dudy, dvdx, dvdy, dwdx, dwdy, dudz, dvdz, dwdz)
      !$acc enter data create(d_11, d_22, d_33, d_12, d_13, d_23)

      pr_inv = 1./prandtl

      ! set up coefficients for 4th-order horizontal background filter

      h_mom_eddy_visc4   = config_visc4_2dsmag * config_len_disp**3
      h_theta_eddy_visc4 = h_mom_eddy_visc4

      !$acc parallel default(present)

      !$acc loop gang worker private(dudx, dudy, dvdx, dvdy, dwdx, dwdy, dudz, dvdz, dwdz, d_11, d_22, d_33, d_12, d_13, d_23)
      do iCell = cellStart,cellEnd

         !$acc loop vector
         do k = 1, nVertLevels
            dudx(k) = 0.0_RKIND
            dudy(k) = 0.0_RKIND
            dvdx(k) = 0.0_RKIND
            dvdy(k) = 0.0_RKIND

            dudz(k) = 0.0_RKIND
            dvdz(k) = 0.0_RKIND
            dwdz(k) = 0.0_RKIND
         end do

         !$acc loop vector
         do k = 1, nVertLevels+1
            dwdx(k) = 0.0_RKIND
            dwdy(k) = 0.0_RKIND
         end do

         !$acc loop seq
         do iEdge=1,nEdgesOnCell(iCell)

            ie = EdgesOnCell(iEdge,iCell)
            cell1 = cellsOnEdge(1,ie)
            cell2 = cellsOnEdge(2,ie)

            !$acc loop vector
            do k=1,nVertLevels
               dudx(k) = dudx(k) + deformation_coef_c2(iEdge,iCell)*u(k,ie) &
                                 - deformation_coef_cs(iEdge,iCell)*v(k,ie)
               dudy(k) = dudy(k) + deformation_coef_cs(iEdge,iCell)*u(k,ie) &
                                 - deformation_coef_s2(iEdge,iCell)*v(k,ie)
               dvdx(k) = dvdx(k) + deformation_coef_cs(iEdge,iCell)*u(k,ie) &
                                 + deformation_coef_c2(iEdge,iCell)*v(k,ie)
               dvdy(k) = dvdy(k) + deformation_coef_s2(iEdge,iCell)*u(k,ie) &
                                 + deformation_coef_cs(iEdge,iCell)*v(k,ie)
            end do

            !$acc loop vector
            do k=1,nVertLevels+1
               wk = 0.5*(w(k,cell1)+w(k,cell2))
               dwdx(k) = dwdx(k) + deformation_coef_c(iEdge,iCell)*wk
               dwdy(k) = dwdy(k) + deformation_coef_s(iEdge,iCell)*wk
            end do

         end do

         !$acc loop vector
         do k=1,nVertLevels
            rdz = 1./(zgrid(k+1,iCell)-zgrid(k,iCell))
            dwdz(k) = (w(k+1,iCell)-w(k,iCell))*rdz
         end do

         !$acc loop vector
         do k=2,nVertLevels-1
            rdz = 1./(zgrid(k+2,iCell)+zgrid(k+1,iCell)-zgrid(k,iCell)-zgrid(k-1,iCell))
            dudz(k) = (uCell(k+1,iCell)-uCell(k-1,iCell))*rdz
            dvdz(k) = (vCell(k+1,iCell)-vCell(k-1,iCell))*rdz
         end do

         k = 1
         rdz = 1./(zgrid(k+1,iCell)-zgrid(k,iCell))
         dudz(k) = (uCell(k+1,iCell)-uCell(k,iCell))*rdz
         dvdz(k) = (vCell(k+1,iCell)-vCell(k,iCell))*rdz

         k = nVertLevels-1
         rdz = 1./(zgrid(k+1,iCell)-zgrid(k,iCell))
         dudz(k+1) = (uCell(k+1,iCell)-uCell(k,iCell))*rdz
         dvdz(k+1) = (vCell(k+1,iCell)-vCell(k,iCell))*rdz
 
         !$acc loop vector
         do k=1, nVertLevels
            d_11(k) = 2.*dudx(k)
            d_22(k) = 2.*dvdy(k)
            d_33(k) = 2.*dwdz(k)
            d_12(k) = dudy(k) + dvdx(k)
            d_13(k) = dwdx(k) + dudz(k)
            d_23(k) = dwdy(k) + dvdz(k)
         end do

         if (les_model_opt == LES_MODEL_3D_SMAGORINSKY) then

            !$acc loop vector
            do k=1, nVertLevels
               def2 = 0.5*(d_11(k)**2 + d_22(k)**2 + d_33(k)**2) + d_12(k)**2 + d_13(k)**2 + d_23(k)**2
               eddy_visc_horz(k,iCell) = (c_s * config_len_disp)**2 * sqrt(max(0.,def2 - pr_inv*bv_freq2(k,iCell)))
               eddy_visc_horz(k,iCell) = min(eddy_visc_horz(k,iCell),(0.01*config_len_disp**2) * invDt)
               delta_z = zgrid(k+1,iCell)-zgrid(k,iCell)
               eddy_visc_vert(k,iCell) = (c_s * delta_z)**2 * sqrt(max(0.,def2 - pr_inv*bv_freq2(k,iCell)))
               ! eddy_visc_vert(k,iCell) = eddy_visc_horz(k,iCell)
            end do

         else if (les_model_opt == LES_MODEL_PROGNOSTIC_15_ORDER) then

            !$acc loop vector
            do k=1,nVertLevels  ! bound the tke here, currently hardwired
               ! scalars(index_tke,k,iCell) = max(0.,min(100.,scalars(index_tke,k,iCell)))
               scalars(index_tke,k,iCell) = max(0.,scalars(index_tke,k,iCell))
            end do

            !$acc loop vector
            do k=1,nVertLevels

               delta_z = zgrid(k+1,iCell)-zgrid(k,iCell)
               delta_s = ((config_len_disp**2)*delta_z)**(1./3.)
               bv = max( sqrt(abs(bv_freq2(k,iCell))), epsilon_bv )
               tke_length = delta_s
               ! isentropic mixing formulation
               if(bv_freq2(k,iCell) .gt. 1.e-06) &
                  tke_length = 0.76*sqrt(scalars(index_tke,k,iCell))/bv
                  tke_length = min(tke_length, delta_z)
               diss_length = min(delta_s,max(tke_length,0.01*delta_s))
               if(bv_freq2(k,iCell) <= 0) diss_length = delta_s

               ! non-isotropic mixing

               l_horizontal = config_len_disp
               l_vertical = min(delta_z,tke_length)
               if(bv_freq2(k,iCell) <= 0) diss_length = delta_z

               ! isotropic mixing

               ! l_horizontal = min(delta_s,tke_length)
               ! if(bv_freq2(k,iCell) <= 0) diss_length = delta_s
               ! l_vertical = l_horizontal

               ! eddy viscocities set here if we are running the 1.5 order prognostic tke scheme
               eddy_visc_h = c_k*l_horizontal*sqrt(scalars(index_tke,k,iCell))
               eddy_visc_h = min(eddy_visc_h,(0.01*config_len_disp**2) * invDt)
               eddy_visc_v = c_k*l_vertical*sqrt(scalars(index_tke,k,iCell))
               eddy_visc_v = min(eddy_visc_v,(0.01*delta_z**2) * invDt)

               eddy_visc_horz(k,iCell) = eddy_visc_h
               eddy_visc_vert(k,iCell) = eddy_visc_v

               ! terms for the prognostic tke integration

               shear_production = eddy_visc_h*(d_11(k)**2 + d_22(k)**2 + d_12(k)**2) &
                                 +eddy_visc_v*(d_33(k)**2 + d_13(k)**2 + d_23(k)**2)

               buoyancy = -eddy_visc_v*bv_freq2(k,iCell)

               ! dissipation

               c_dissipation = 1.9*c_k + max( 0.0, 0.93 - 1.9*c_k )*diss_length/delta_s
               !  if( (k.eq. 1) .or. (k.eq.nVertLevels) ) c_dissipation = 3.9

               dissipation = -c_dissipation*(scalars(index_tke,k,iCell)**(1.5))/diss_length

               ! computing eddy viscosities *********

               prandtl_horizontal_inv = 3.
               prandtl_3d_inv(k,iCell) = 1.0+(2.0*l_vertical/delta_z)


               ! RHS term for the subgrid ke.

               if(dynamics_substep == 1) &
                    tend_scalars(index_tke,k,iCell) = rho_zz(k,iCell)*( shear_production + buoyancy + dissipation )

            end do

         else

!MGD            call mpas_log_write(' in les_models, no les scheme for '//trim(config_les_model), messageType=MPAS_LOG_CRIT)

         end if ! end of les_model_opt test

      end do ! loop over all owned cells (columns)

      !$acc end parallel

      !$acc exit data delete(dudx, dudy, dvdx, dvdy, dwdx, dwdy, dudz, dvdz, dwdz)
      !$acc exit data delete(d_11, d_22, d_33, d_12, d_13, d_23)

      DEBUG_WRITE(' les_models ')

   end subroutine les_models

!---------------------------------------

   subroutine calculate_n2( bn2, theta_m, exner, pressure_b, pp, zgrid, scalars, index_qv, index_qc, qtot, &
                            cellStart, cellEnd, nCells)

      use mpas_atm_dimensions  !  pull nVertLevels and num_scalars from here

#ifdef DO_PHYSICS
      use mpas_atmphys_constants, only : svp1, svp2, svp3, svpt0, xlv, R_d, R_v, ep_2
#else
      !
      ! If stand-alone MPAS-Atmosphere physics are not being used, provide
      ! definitions for constants needed in the caculation of the moist
      ! Brunt-Vaisala frequency following those in mpas_atmphys_constants.
      !
      real(kind=RKIND), parameter :: svp1  = 0.6112
      real(kind=RKIND), parameter :: svp2  = 17.67
      real(kind=RKIND), parameter :: svp3  = 29.65
      real(kind=RKIND), parameter :: svpt0 = 273.15
      real(kind=RKIND), parameter :: xlv   = 2.50e6       !latent heat of vaporization [J/kg]
      real (kind=RKIND), parameter :: R_d  = 287.0_RKIND  !< Constant: Gas constant for dry air [J kg-1 K-1]
      real(kind=RKIND), parameter :: R_v   = 461.6        !gas constant for water vapor [J/kg/K]
      real(kind=RKIND), parameter :: ep_2  = R_d/R_v
#endif

      integer, intent(in) :: cellStart, cellEnd, nCells
      integer, intent(in) :: index_qv, index_qc
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(out) :: bn2
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: theta_m, exner, pressure_b, pp, qtot
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1), intent(in) :: zgrid
      real (kind=RKIND), dimension(num_scalars,nVertLevels,nCells+1), intent(in) :: scalars
! local
      integer :: iCell, k
      real (kind=RKIND) :: dz, rdz, esw, p
      real (kind=RKIND), parameter :: qc_cr = 0.00001  ! in kg/kg
      real (kind=RKIND), dimension(nVertLevels) :: theta, qvsw, temp, coefa
      logical :: dry_bv_frequency


      DEBUG_WRITE(' begin BV frequency calculations ')

      !$acc enter data create(theta, temp, qvsw, coefa)

      !$acc parallel default(present)

      !$acc loop gang worker private(theta, temp, qvsw, coefa)
      do iCell = cellStart,cellEnd

!DIR$ IVDEP
         !$acc loop vector
         do k=1, nVertLevels

            theta(k) = theta_m(k,iCell) / (1._RKIND + rvord * scalars(index_qv,k,iCell))

            temp(k) = exner(k,iCell) * theta(k)

            p = pressure_b(k,iCell) + pp(k,iCell)
            esw = 1000. * svp1 * exp(svp2 * (temp(k) - svpt0) / (temp(k) - svp3))
            if (p < esw) esw = p * 0.99     ! fix for pressure < esw
            qvsw(k) = ep_2 * esw / (p - esw)

            coefa(k) = ( 1.0 + xlv * qvsw(k)/ R_d / temp(k) ) / &
               ( 1.0 + xlv * xlv *qvsw(k) / Cp / R_v / temp(k) / temp(k) )

         end do

         !$acc loop vector
         do k=2, nVertLevels-1
            dz = 0.5 * (zgrid(k+2,iCell)+zgrid(k+1,iCell)) - 0.5 * (zgrid(k,iCell)+zgrid(k-1,iCell))
            rdz = 1.0/dz

            ! if ( scalars(index_qc,k,iCell) < qc_cr ) then
            !   ! Dry Brunt-Vaisala frequency
            !   bn2(k,iCell) = gravity * ((theta(k+1) - theta(k-1) ) / theta(k)  / dz  &
            !                + rvord * (scalars(index_qv,k+1,iCell) - scalars(index_qv,k-1,iCell)) / dz &
            !               - ( qtot(k+1, iCell) - qtot(k-1, iCell) ) / dz )
            ! else
            !   ! Moist Brunt-Vaisala frequency according to Durran and Klemp (1982) Eq. 36
            !   bn2(k,iCell) = gravity * ( coefa(k) * ((theta(k+1) - theta(k-1) ) / theta(k) / dz &
            !               + xlv / cp / temp(k) * ( qvsw(k+1) - qvsw(k-1)) / dz ) &
            !               - ( qtot(k+1, iCell) - qtot(k-1, iCell) ) / dz )
            ! endif

            dry_bv_frequency = .true.
            if(index_qc .gt. 0) then  ! if moist simulation, qc exists
               if ( scalars(index_qc,k,iCell) .ge. qc_cr ) dry_bv_frequency = .false.
            end if

            if (dry_bv_frequency) then
               ! Dry Brunt-Vaisala frequency
               bn2(k,iCell) = gravity * ((theta(k+1) - theta(k-1) ) / theta(k)  * rdz  &
                            + rvord * (scalars(index_qv,k+1,iCell) - scalars(index_qv,k-1,iCell)) * rdz &
                           - ( qtot(k+1, iCell) - qtot(k-1, iCell) ) * rdz )
            else
               ! Moist Brunt-Vaisala frequency according to Durran and Klemp (1982) Eq. 36
               bn2(k,iCell) = gravity * ( coefa(k) * ((theta(k+1) - theta(k-1) ) / theta(k) * rdz &
                           + xlv / cp / temp(k) * ( qvsw(k+1) - qvsw(k-1)) * rdz ) &
                           - ( qtot(k+1, iCell) - qtot(k-1, iCell) ) * rdz )
            endif

         end do

         bn2(1,iCell) = bn2(2,iCell)
         bn2(nVertLevels,iCell) = bn2(nVertLevels-1,iCell)

      end do

      !$acc end parallel

      !$acc exit data delete(theta, temp, qvsw, coefa)

      DEBUG_WRITE(' exiting BV frequency calculations ')

   end subroutine calculate_n2

!---------------------------------------

   subroutine u_dissipation_3d( edgeStart, edgeEnd, edgeSolveStart, edgeSolveEnd, vertexStart, vertexEnd,    &
                                cellStart, cellEnd, nCells, nEdges, nVertices, vertexDegree,                 &
                                cellsOnEdge, verticesOnEdge, edgesOnCell, edgesOnVertex,                     &
                                nEdgesOnCell, edgesOnCell_sign, edgesOnVertex_sign,                          &
                                invAreaCell, invAreaTriangle, invDvEdge, invDcEdge,                          &
                                angleEdge, dcEdge, dvEdge, meshScalingDel2, meshScalingDel4,                 &
                                config_mix_full, h_mom_eddy_visc4, v_mom_eddy_visc2,                         &
                                config_del4u_div_factor, zgrid,                                              &
                                eddy_visc_horz, eddy_visc_vert, zz, rdzu, rdzw,                              &
                                fzm, fzp, les_model_opt, les_surface_opt,                                    &
                                config_surface_drag_coefficient,                                             &
                                delsq_u, delsq_vorticity, delsq_divergence,                                  &
                                u, v, divergence, vorticity, rho_edge, rho_zz, u_init, v_init, ustm,         &
                                tend_u_euler                                                                )

      use mpas_atm_dimensions  !  pull nVertLevels and maxEdges from here

      implicit none

      integer, intent(in) :: edgeStart, edgeEnd, edgeSolveStart, edgeSolveEnd
      integer, intent(in) :: vertexStart, vertexEnd, vertexDegree
      integer, intent(in) :: cellStart, cellEnd
      integer, intent(in) :: nCells, nEdges, nVertices
      logical, intent(in) :: config_mix_full

      integer, intent(in) :: les_model_opt
      integer, intent(in) :: les_surface_opt

      integer, dimension(2,nEdges+1), intent(in) :: cellsOnEdge
      integer, dimension(2,nEdges+1), intent(in) :: verticesOnEdge
      integer, dimension(maxEdges,nCells+1), intent(in) :: edgesOnCell
      integer, dimension(nCells+1), intent(in) :: nEdgesOnCell
      integer, dimension(vertexDegree,nVertices+1), intent(in) :: edgesOnVertex

      real (kind=RKIND), intent(in) :: h_mom_eddy_visc4
      real (kind=RKIND), intent(in) :: v_mom_eddy_visc2
      real (kind=RKIND), intent(in) :: config_del4u_div_factor
      real (kind=RKIND), intent(in) :: config_surface_drag_coefficient

      real (kind=RKIND), dimension(maxEdges,nCells+1), intent(in) :: edgesOnCell_sign
      real (kind=RKIND), dimension(vertexDegree,nVertices+1), intent(in) :: edgesOnVertex_sign
      real (kind=RKIND), dimension(nVertices+1), intent(in) :: invAreaTriangle
      real (kind=RKIND), dimension(nCells+1), intent(in) :: invAreaCell
      real (kind=RKIND), dimension(nEdges+1), intent(in) :: invDcEdge
      real (kind=RKIND), dimension(nEdges+1), intent(in) :: invDvEdge
      real (kind=RKIND), dimension(nEdges+1), intent(in) :: angleEdge
      real (kind=RKIND), dimension(nEdges+1), intent(in) :: dcEdge
      real (kind=RKIND), dimension(nEdges+1), intent(in) :: dvEdge
      real (kind=RKIND), dimension(nEdges+1), intent(in) :: meshScalingDel2
      real (kind=RKIND), dimension(nEdges+1), intent(in) :: meshScalingDel4
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1), intent(in) :: zgrid

      real (kind=RKIND), dimension(nVertLevels,nEdges+1), intent(in) :: u
      real (kind=RKIND), dimension(nVertLevels,nEdges+1), intent(in) :: v
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: divergence
      real (kind=RKIND), dimension(nVertLevels,nVertices+1), intent(in) :: vorticity
      real (kind=RKIND), dimension(nVertLevels,nEdges+1), intent(in) :: rho_edge
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: eddy_visc_horz
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: eddy_visc_vert
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: zz
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: rho_zz
      real (kind=RKIND), dimension(nVertLevels), intent(in) :: rdzu
      real (kind=RKIND), dimension(nVertLevels), intent(in) :: rdzw
      real (kind=RKIND), dimension(nVertLevels), intent(in) :: fzm
      real (kind=RKIND), dimension(nVertLevels), intent(in) :: fzp


      !  scratch space from calling routine
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: delsq_u
      real (kind=RKIND), dimension(nVertLevels,nVertices+1) :: delsq_vorticity
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: delsq_divergence

      real (kind=RKIND), dimension(nVertLevels), intent(in) :: u_init, v_init
      real (kind=RKIND), dimension(:), intent(in) :: ustm

      real (kind=RKIND), dimension(nVertLevels,nEdges+1), intent(out) :: tend_u_euler

      ! local variables

      integer :: iEdge, cell1, cell2, vertex1, vertex2, iVertex, iCell, i, k
      real (kind=RKIND) :: r_dc, r_dv, u_diffusion, u_diffusion_les, kdiffu, r, edge_sign, u_mix_scale
      real (kind=RKIND) :: z1, z2, z3, z4, zm, z0, zp
      real (kind=RKIND), dimension(nVertLevels) :: u_mix

      real (kind=RKIND), dimension(nVertLevels+1) :: turb_vflux
      real (kind=RKIND) :: rho_k_cell1, rho_k_cell2, rho_k_at_w
      real (kind=RKIND) :: zz_cell1, zz_cell2, zz_at_w
      real (kind=RKIND) :: ust_edge

      real (kind=RKIND) :: velocity_magnitude
      real (kind=RKIND) :: tau_12_factor


      DEBUG_WRITE(' begin u_dissipation_3d ')
      DEBUG_WRITE(' 4th order hyperviscosity is $r ' COMMA realArgs=(/h_mom_eddy_visc4/))
      DEBUG_WRITE(' 4th order divergence factor is $r ' COMMA realArgs=(/config_del4u_div_factor/))

!$OMP BARRIER

      ! del^4 horizontal filter.  We compute this as del^2 ( del^2 (u) ).
      ! First, storage to hold the result from the first del^2 computation.

      !$acc enter data create(u_mix)
      !$acc enter data create(turb_vflux)

      !$acc parallel default(present)

      tau_12_factor = 0.0
      if(les_model_opt /= LES_MODEL_NONE) tau_12_factor = 1.0

      !$acc loop gang worker
      do iEdge=edgeStart,edgeEnd
         cell1 = cellsOnEdge(1,iEdge)
         cell2 = cellsOnEdge(2,iEdge)
         vertex1 = verticesOnEdge(1,iEdge)
         vertex2 = verticesOnEdge(2,iEdge)
         r_dc = invDcEdge(iEdge)
         r_dv = min(invDvEdge(iEdge), 4*invDcEdge(iEdge))

         !$acc loop vector
         do k = 1, nVertLevels
            delsq_u(k,iEdge) = 0.0_RKIND
         end do

!DIR$ IVDEP
         !$acc loop vector
         do k=1,nVertLevels

            ! Compute diffusion, computed as \nabla divergence - k \times \nabla vorticity
            !                    only valid for h_mom_eddy_visc4 == constant
            u_diffusion =   ( divergence(k,cell2)  - divergence(k,cell1) ) * r_dc  &
                           -( vorticity(k,vertex2) - vorticity(k,vertex1) ) * r_dv
            ! for LES models we need 2 times the gradient of divergence, in contrast to what is
            ! saved and used to calculate the 4th-order horizontal filter
            u_diffusion_les = u_diffusion + tau_12_factor * ( divergence(k,cell2)  - divergence(k,cell1)  ) * r_dc

            delsq_u(k,iEdge) = delsq_u(k,iEdge) + u_diffusion

            kdiffu = 0.5*(eddy_visc_horz(k,cell1)+eddy_visc_horz(k,cell2))

            ! include 2nd-order diffusion here
            tend_u_euler(k,iEdge) = tend_u_euler(k,iEdge) &
                                    + rho_edge(k,iEdge)* kdiffu * u_diffusion_les * meshScalingDel2(iEdge)

         end do
      end do

      !$acc end parallel

      if (h_mom_eddy_visc4 > 0.0) then  ! 4th order mixing is active

!$OMP BARRIER

         !$acc parallel default(present)

         !$acc loop gang worker
         do iVertex=vertexStart,vertexEnd
            !$acc loop vector
            do k = 1, nVertLevels
               delsq_vorticity(k,iVertex) = 0.0_RKIND
            end do

            !$acc loop seq
            do i=1,vertexDegree
               iEdge = edgesOnVertex(i,iVertex)
               edge_sign = invAreaTriangle(iVertex) * dcEdge(iEdge) * edgesOnVertex_sign(i,iVertex)

               !$acc loop vector
               do k=1,nVertLevels
                  delsq_vorticity(k,iVertex) = delsq_vorticity(k,iVertex) + edge_sign * delsq_u(k,iEdge)
               end do
            end do
         end do

         !$acc loop gang worker
         do iCell=cellStart,cellEnd
            !$acc loop vector
            do k = 1, nVertLevels
               delsq_divergence(k,iCell) = 0.0_RKIND
            end do

            r = invAreaCell(iCell)

            !$acc loop seq
            do i=1,nEdgesOnCell(iCell)
               iEdge = edgesOnCell(i,iCell)
               edge_sign = r * dvEdge(iEdge) * edgesOnCell_sign(i,iCell)

               !$acc loop vector
               do k=1,nVertLevels
                  delsq_divergence(k,iCell) = delsq_divergence(k,iCell) + edge_sign * delsq_u(k,iEdge)
               end do
            end do
         end do

         !$acc end parallel

!$OMP BARRIER

         !$acc parallel default(present)

         !$acc loop gang worker
         do iEdge=edgeSolveStart,edgeSolveEnd
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)
            vertex1 = verticesOnEdge(1,iEdge)
            vertex2 = verticesOnEdge(2,iEdge)

            u_mix_scale = meshScalingDel4(iEdge)*h_mom_eddy_visc4
            r_dc = u_mix_scale * config_del4u_div_factor * invDcEdge(iEdge)
            r_dv = u_mix_scale * min(invDvEdge(iEdge), 4*invDcEdge(iEdge))

!DIR$ IVDEP
            !$acc loop vector
            do k=1,nVertLevels

               ! Compute diffusion, computed as \nabla divergence - k \times \nabla vorticity
               !                    only valid for h_mom_eddy_visc4 == constant
               !
               ! Here, we scale the diffusion on the divergence part a factor of config_del4u_div_factor
               !    relative to the rotational part.  The stability constraint on the divergence component is much less
               !    stringent than the rotational part, and this flexibility may be useful.
               !
               u_diffusion =  rho_edge(k,iEdge) *  ( ( delsq_divergence(k,cell2)  - delsq_divergence(k,cell1) ) * r_dc  &
                                                    -( delsq_vorticity(k,vertex2) - delsq_vorticity(k,vertex1) ) * r_dv )
               tend_u_euler(k,iEdge) = tend_u_euler(k,iEdge) - u_diffusion

            end do
         end do

         !$acc end parallel

      end if ! 4th order mixing is active

      !
      !  vertical mixing for u - 2nd order filter in physical (z) space
      !
      if ( v_mom_eddy_visc2 > 0.0 ) then

         if (config_mix_full) then  ! mix full state

            !$acc parallel default(present)

            !$acc loop gang worker
            do iEdge=edgeSolveStart,edgeSolveEnd

               cell1 = cellsOnEdge(1,iEdge)
               cell2 = cellsOnEdge(2,iEdge)

               !$acc loop vector
               do k=2,nVertLevels-1

                  z1 = 0.5*(zgrid(k-1,cell1)+zgrid(k-1,cell2))
                  z2 = 0.5*(zgrid(k  ,cell1)+zgrid(k  ,cell2))
                  z3 = 0.5*(zgrid(k+1,cell1)+zgrid(k+1,cell2))
                  z4 = 0.5*(zgrid(k+2,cell1)+zgrid(k+2,cell2))

                  zm = 0.5*(z1+z2)
                  z0 = 0.5*(z2+z3)
                  zp = 0.5*(z3+z4)

                  tend_u_euler(k,iEdge) = tend_u_euler(k,iEdge) + rho_edge(k,iEdge) * v_mom_eddy_visc2*(  &
                                     (u(k+1,iEdge)-u(k  ,iEdge))/(zp-z0)                      &
                                    -(u(k  ,iEdge)-u(k-1,iEdge))/(z0-zm) )/(0.5*(zp-zm))
               end do
            end do

            !$acc end parallel

         else  ! idealized cases where we mix on the perturbation from the initial 1-D state

            !$acc parallel default(present)

            !$acc loop gang worker private(u_mix)
            do iEdge=edgeSolveStart,edgeSolveEnd

               cell1 = cellsOnEdge(1,iEdge)
               cell2 = cellsOnEdge(2,iEdge)

               !$acc loop vector
               do k=1,nVertLevels
                  u_mix(k) = u(k,iEdge) - u_init(k) * cos( angleEdge(iEdge) ) &
                                        + v_init(k) * sin( angleEdge(iEdge) )
               end do

               !$acc loop vector
               do k=2,nVertLevels-1

                  z1 = 0.5*(zgrid(k-1,cell1)+zgrid(k-1,cell2))
                  z2 = 0.5*(zgrid(k  ,cell1)+zgrid(k  ,cell2))
                  z3 = 0.5*(zgrid(k+1,cell1)+zgrid(k+1,cell2))
                  z4 = 0.5*(zgrid(k+2,cell1)+zgrid(k+2,cell2))

                  zm = 0.5*(z1+z2)
                  z0 = 0.5*(z2+z3)
                  zp = 0.5*(z3+z4)

                  tend_u_euler(k,iEdge) = tend_u_euler(k,iEdge) + rho_edge(k,iEdge) * v_mom_eddy_visc2*(  &
                                     (u_mix(k+1)-u_mix(k  ))/(zp-z0)                      &
                                    -(u_mix(k  )-u_mix(k-1))/(z0-zm) )/(0.5*(zp-zm))
               end do
            end do

            !$acc end parallel

         end if  ! mix perturbation state

      end if  ! vertical mixing of horizontal momentum for les formulation

      if ( les_model_opt /= LES_MODEL_NONE ) then

         !$acc parallel default(present)

         !$acc loop gang worker private(turb_vflux)
         do iEdge=edgeSolveStart,edgeSolveEnd

            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)
            turb_vflux(nVertlevels+1) = 0.0_RKIND  !  no turbulent flux out of the domain
            turb_vflux(1) = 0.0_RKIND  !  lower bc flux handled where ???

            !$acc loop vector
            do k=2,nVertLevels
               rho_k_cell1 =  fzm(k)*rho_zz(k  ,cell1)*zz(k  ,cell1)*eddy_visc_vert(k  ,cell1) &
                             +fzp(k)*rho_zz(k-1,cell1)*zz(k-1,cell1)*eddy_visc_vert(k-1,cell1)
               rho_k_cell2 =  fzm(k)*rho_zz(k  ,cell2)*zz(k  ,cell2)*eddy_visc_vert(k  ,cell2) &
                             +fzp(k)*rho_zz(k-1,cell2)*zz(k-1,cell2)*eddy_visc_vert(k-1,cell2)
               rho_k_at_w = 0.5*(rho_k_cell1+rho_k_cell2)

               zz_cell1 = fzm(k)*zz(k,cell1)+fzp(k)*zz(k-1,cell1)
               zz_cell2 = fzm(k)*zz(k,cell2)+fzp(k)*zz(k-1,cell2)
               zz_at_w = 0.5*(zz_cell1+zz_cell2)
               turb_vflux(k) = - rho_k_at_w*zz_at_w*rdzu(k)*(u(k,iEdge)-u(k-1,iEdge))
            end do

            if( les_surface_opt == LES_SURFACE_SPECIFIED ) then
               velocity_magnitude = sqrt(u(1,iEdge)**2 + v(1,iEdge)**2)
               turb_vflux(1) = -rho_edge(1,iEdge)*config_surface_drag_coefficient*u(1,iEdge)*velocity_magnitude
               turb_vflux(nVertLevels+1) = turb_vflux(nVertLevels)
            else if ( les_surface_opt == LES_SURFACE_VARYING ) then
               ust_edge = 0.5*(ustm(cell1) + ustm(cell2))
               velocity_magnitude = max(sqrt(u(1,iEdge)**2 + v(1,iEdge)**2),0.1)
               turb_vflux(1) = -rho_edge(1,iEdge)*ust_edge*ust_edge*(u(1,iEdge)/velocity_magnitude)
               turb_vflux(nVertLevels+1) = turb_vflux(nVertLevels)
               ! end test conditions
            else
               ! test conditions for supercell case
               turb_vflux(1) = turb_vflux(2)
               turb_vflux(nVertLevels+1) = turb_vflux(nVertLevels)
               ! end test conditions
            end if

            !$acc loop vector
            do k=1,nVertLevels
               tend_u_euler(k,iEdge) = tend_u_euler(k,iEdge) - rdzw(k)*(turb_vflux(k+1)-turb_vflux(k))
            end do

         end do

         !$acc end parallel

      end if

      !$acc exit data delete(turb_vflux)
      !$acc exit data delete(u_mix)

      DEBUG_WRITE(' exiting u_dissipation_3d ')

   end subroutine u_dissipation_3d

!------------------------

   subroutine w_dissipation_3d( cellStart, cellEnd, cellSolveStart, cellSolveEnd,         &
                                nCells, nEdges,                                           &
                                nEdgesOnCell, edgesOnCell, cellsOnEdge, edgesOnCell_sign, &
                                invAreaCell, invDcEdge, dvEdge,                           &
                                meshScalingDel2, meshScalingDel4,                         &
                                rdzw, rdzu,                                               &
                                v_mom_eddy_visc2, h_mom_eddy_visc4,                       &
                                delsq_w,                                                  &
                                w, rho_edge, rho_zz, divergence, zz,                      &
                                eddy_visc_horz, eddy_visc_vert,                           &
                                les_model_opt, les_surface_opt,                           &
                                tend_w_euler                                             )


      ! 3D w dissipation using the 3D smagorinsky eddy viscosities.
      ! This routine also includes the simpler mixing models, and the 4th-order horizontal filter

      use mpas_atm_dimensions  !  pull nVertLevels and maxEdges from here

      implicit none

      integer, intent(in) :: cellStart, cellEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd
      integer, intent(in) :: nCells, nEdges

      integer, dimension(nCells+1), intent(in) :: nEdgesOnCell
      integer, dimension(maxEdges,nCells+1), intent(in) :: EdgesOnCell

      integer, dimension(2,nEdges+1), intent(in) :: cellsOnEdge

      integer, intent(in) :: les_model_opt
      integer, intent(in) :: les_surface_opt

      real (kind=RKIND), intent(in) :: h_mom_eddy_visc4
      real (kind=RKIND), intent(in) :: v_mom_eddy_visc2

      real (kind=RKIND), dimension(maxEdges,nCells+1), intent(in) :: edgesOnCell_sign
      real (kind=RKIND), dimension(nCells+1), intent(in) :: invAreaCell
      real (kind=RKIND), dimension(nEdges+1), intent(in) :: dvEdge
      real (kind=RKIND), dimension(nEdges+1), intent(in) :: invDcEdge
      real (kind=RKIND), dimension(nEdges+1), intent(in) :: meshScalingDel2
      real (kind=RKIND), dimension(nEdges+1), intent(in) :: meshScalingDel4
      real (kind=RKIND), dimension(nVertLevels), intent(in) :: rdzw
      real (kind=RKIND), dimension(nVertLevels), intent(in) :: rdzu

      real (kind=RKIND), dimension(nVertLevels+1,nCells+1), intent(in) :: w
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: eddy_visc_horz
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: eddy_visc_vert
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: rho_zz
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: divergence
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: zz
      real (kind=RKIND), dimension(nVertLevels,nEdges+1), intent(in) :: rho_edge

      real (kind=RKIND), dimension(nVertLevels+1,nCells+1), intent(inout) :: tend_w_euler

      ! storage passed in from calling routine
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: delsq_w
      real (kind=RKIND), dimension(nVertLevels+1) :: turb_vflux

      ! local variables

      integer :: cell1, cell2, iEdge, iCell, i, k
      real (kind=RKIND) :: r_areaCell, edge_sign, w_turb_flux


!  !OMP BARRIER  why is this openmp barrier here???

      ! del^4 horizontal filter.  We compute this as del^2 ( del^2 (w) ).
      !
      ! First, storage to hold the result from the first del^2 computation.
      !  we copied code from the theta mixing, hence the theta* names.


      DEBUG_WRITE(' begin w_dissipation_3d ')
      DEBUG_WRITE(' 4th order hyperviscosity is $r ' COMMA realArgs=(/h_mom_eddy_visc4/))

      !$acc enter data create(turb_vflux)

      !$acc parallel default(present)

      !$acc loop gang worker
      do iCell=cellStart,cellEnd

         !$acc loop vector
         do k = 1, nVertLevels
            delsq_w(k,iCell) = 0.0_RKIND
         end do

         !$acc loop vector
         do k = 1, nVertLevels+1
            tend_w_euler(k,iCell) = 0.0_RKIND
         end do

         r_areaCell = invAreaCell(iCell)

         !$acc loop seq
         do i=1,nEdgesOnCell(iCell)
            iEdge = edgesOnCell(i,iCell)

            edge_sign = 0.5 * r_areaCell*edgesOnCell_sign(i,iCell) * dvEdge(iEdge) * invDcEdge(iEdge)

            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)

!DIR$ IVDEP
            !$acc loop vector
            do k=2,nVertLevels

               w_turb_flux =  edge_sign*(rho_edge(k,iEdge)+rho_edge(k-1,iEdge))*(w(k,cell2) - w(k,cell1))
               delsq_w(k,iCell) = delsq_w(k,iCell) + w_turb_flux
               w_turb_flux = w_turb_flux * meshScalingDel2(iEdge) * 0.25 * &
                               ( eddy_visc_horz(k  ,cell1)+eddy_visc_horz(k  ,cell2)      &
                                +eddy_visc_horz(k-1,cell1)+eddy_visc_horz(k-1,cell2) )
               tend_w_euler(k,iCell) = tend_w_euler(k,iCell) + w_turb_flux
            end do
         end do
      end do

      !$acc end parallel

!$OMP BARRIER

      if (h_mom_eddy_visc4 > 0.0) then  ! 4th order mixing is active

         !$acc parallel default(present)

         !$acc loop gang worker
         do iCell=cellSolveStart,cellSolveEnd    ! Technically updating fewer cells than before...

            r_areaCell = h_mom_eddy_visc4 * invAreaCell(iCell)

            !$acc loop seq
            do i=1,nEdgesOnCell(iCell)
               iEdge = edgesOnCell(i,iCell)
               cell1 = cellsOnEdge(1,iEdge)
               cell2 = cellsOnEdge(2,iEdge)

               edge_sign = meshScalingDel4(iEdge)*r_areaCell*dvEdge(iEdge)*edgesOnCell_sign(i,iCell) * invDcEdge(iEdge)

               !$acc loop vector
               do k=2,nVertLevels
                  tend_w_euler(k,iCell) = tend_w_euler(k,iCell) - edge_sign * (delsq_w(k,cell2) - delsq_w(k,cell1))
               end do

            end do
         end do

         !$acc end parallel

      end if ! 4th order mixing is active

      if ( v_mom_eddy_visc2 > 0.0 ) then  ! vertical mixing

         !$acc parallel default(present)

         !$acc loop gang worker
         do iCell=cellSolveStart,cellSolveEnd
!DIR$ IVDEP
            !$acc loop vector
            do k=2,nVertLevels
               tend_w_euler(k,iCell) = tend_w_euler(k,iCell) + v_mom_eddy_visc2*0.5*(rho_zz(k,iCell)+rho_zz(k-1,iCell))*(  &
                                           (w(k+1,iCell)-w(k  ,iCell))*rdzw(k)                              &
                                          -(w(k  ,iCell)-w(k-1,iCell))*rdzw(k-1) )*rdzu(k)
            end do
         end do

         !$acc end parallel

      end if

      if ( les_model_opt /= LES_MODEL_NONE ) then

         !$acc parallel default(present)

         !$acc loop gang worker private(turb_vflux)
         do iCell = cellSolveStart,cellSolveEnd ! vertical mixing for each column
            ! compute turbulent fluxes
     
            !$acc loop vector
            do k=1,nVertLevels
               turb_vflux(k) = - rho_zz(k,iCell)*eddy_visc_vert(k,iCell)*zz(k,iCell)*(      &
                                          2.0*zz(k,iCell)*rdzw(k)*(w(k+1,iCell)-w(k,iCell))  &
                                              + divergence(k,iCell)                         )
            end do

            turb_vflux(nVertLevels+1) = 0.0

            !$acc loop vector
            do k=2,nVertLevels
               tend_w_euler(k,iCell) = tend_w_euler(k,iCell) &
                                           - rdzu(k)*(turb_vflux(k)-turb_vflux(k-1))
            end do
         end do

         !$acc end parallel

      end if

      !$acc exit data delete(turb_vflux)

      DEBUG_WRITE(' exiting w_dissipation_3d ')

   end subroutine w_dissipation_3d

!-----------------------------------------------------

   subroutine scalar_dissipation_3d_les( cellStart, cellEnd, cellSolveStart, cellSolveEnd,         &
                                         nCells, nEdges,                                           &
                                         nEdgesOnCell, edgesOnCell, cellsOnEdge, edgesOnCell_sign, &
                                         invAreaCell, invDcEdge, dvEdge,                           &
                                         meshScalingDel2, meshScalingDel4,                         &
                                         config_mix_full, t_init, zgrid,                           &
                                         rdzw, rdzu, fzm, fzp,                                     &
                                         v_theta_eddy_visc2, h_theta_eddy_visc4, prandtl_inv,      &
                                         prandtl_3d_inv,                                           &
                                         delsq_theta,                                              &
                                         theta_m, rho_edge, rho_zz, zz,                            &
                                         eddy_visc_horz, eddy_visc_vert,                           &
                                         bv_freq2, config_len_disp, scalars, tend_scalars,         &
                                         index_tke, index_qv, num_scalars_dummy, mix_scalars,      &
                                         les_model_opt, les_surface_opt, clock, dt,                &
                                         config_surface_heat_flux, config_surface_moisture_flux,   &
                                         uReconstructZonal, uReconstructMeridional,                &
                                         hfx, qfx,                                                 &
                                         tend_theta_euler, dynamics_substep                       )


      ! 3D theta_m dissipation using the 3D smagorinsky eddy viscosities.
      ! This routine also includes the simpler mixing models, and the 4th-order horizontal filter

      use mpas_atm_dimensions  !  pull nVertLevels and maxEdges from here

      implicit none

      integer, intent(in) :: cellStart, cellEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd
      integer, intent(in) :: nCells, nEdges
      integer, intent(in) :: num_scalars_dummy
      integer, intent(in) :: index_tke, index_qv
      integer, intent(in) :: dynamics_substep

      real (kind=RKIND), intent(in) :: config_surface_heat_flux
      real (kind=RKIND), intent(in) :: config_surface_moisture_flux

      logical, intent(in) :: config_mix_full, mix_scalars

      integer, intent(in) :: les_model_opt
      integer, intent(in) :: les_surface_opt

      type (MPAS_Clock_type), intent(in) :: clock
      real (kind=RKIND), intent(in) :: dt

      integer, dimension(nCells+1), intent(in) :: nEdgesOnCell
      integer, dimension(maxEdges,nCells+1), intent(in) :: EdgesOnCell

      integer, dimension(2,nEdges+1), intent(in) :: cellsOnEdge

      real (kind=RKIND), intent(in) :: h_theta_eddy_visc4
      real (kind=RKIND), intent(in) :: v_theta_eddy_visc2
      real (kind=RKIND), intent(in) :: prandtl_inv
      real (kind=RKIND), intent(in) :: config_len_disp

      real (kind=RKIND), dimension(maxEdges,nCells+1), intent(in) :: edgesOnCell_sign
      real (kind=RKIND), dimension(nCells+1), intent(in) :: invAreaCell
      real (kind=RKIND), dimension(nEdges+1), intent(in) :: dvEdge
      real (kind=RKIND), dimension(nEdges+1), intent(in) :: invDcEdge
      real (kind=RKIND), dimension(nEdges+1), intent(in) :: meshScalingDel2
      real (kind=RKIND), dimension(nEdges+1), intent(in) :: meshScalingDel4
      real (kind=RKIND), dimension(nVertLevels), intent(in) :: rdzw
      real (kind=RKIND), dimension(nVertLevels), intent(in) :: rdzu
      real (kind=RKIND), dimension(nVertLevels), intent(in) :: fzm
      real (kind=RKIND), dimension(nVertLevels), intent(in) :: fzp
      real (kind=RKIND), dimension(nVertLevels+1, nCells+1), intent(in) :: zgrid
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: zz
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: uReconstructZonal, uReconstructMeridional

      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: t_init

      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: bv_freq2
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: prandtl_3d_inv
      real (kind=RKIND), dimension(num_scalars,nVertLevels,nCells+1), intent(in) :: scalars
      real (kind=RKIND), dimension(num_scalars,nVertLevels,nCells+1), intent(inout) :: tend_scalars
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: theta_m
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: eddy_visc_horz
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: eddy_visc_vert
      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(in) :: rho_zz
      real (kind=RKIND), dimension(nVertLevels,nEdges+1), intent(in) :: rho_edge
      real (kind=RKIND), dimension(:), intent(in) :: hfx, qfx

      real (kind=RKIND), dimension(nVertLevels,nCells+1), intent(inout) :: tend_theta_euler

      ! storage passed in from calling routine
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: delsq_theta

      ! local variables
      integer :: cell1, cell2, iEdge, iCell, i, k, iScalar
      real (kind=RKIND) :: r_areaCell, edge_sign, theta_turb_flux, pr_scale
      real (kind=RKIND) :: z1, z2, z3, z4, zm, z0, zp
      real (kind=RKIND), dimension(nVertLevels+1) :: turb_vflux, prandtl_1d_inverse
      real (kind=RKIND), dimension(num_scalars,nVertLevels+1) :: turb_vflux_scalars
      real (kind=RKIND), dimension(nVertLevels) :: rho_k_at_w, zz_at_w

      real (kind=RKIND) :: moisture_flux, heat_flux, theta_m_flux
      real (kind=RKIND) :: qv_cell, theta_m_cell, theta_cell


      DEBUG_WRITE(' begin scalar_dissipation_3d ')
      DEBUG_WRITE(' 4th order hyperviscosity is $r ' COMMA realArgs=(/h_theta_eddy_visc4/))

      if( mix_scalars .and. (dynamics_substep == 1)) call mpas_log_write(' scalar mixing on ')

      !$acc enter data create(turb_vflux_scalars)
      !$acc enter data create(turb_vflux, prandtl_1d_inverse)

      !$acc parallel default(present)

      !$acc loop gang worker
      do iCell=cellStart,cellEnd

         !$acc loop vector
         do k = 1, nVertLevels
            delsq_theta(k,iCell) = 0.0_RKIND
            tend_theta_euler(k,iCell) = 0.0_RKIND
         end do

         r_areaCell = invAreaCell(iCell)

         !$acc loop seq
         do i=1,nEdgesOnCell(iCell)
            iEdge = edgesOnCell(i,iCell)
            edge_sign = r_areaCell*edgesOnCell_sign(i,iCell) * dvEdge(iEdge) * invDcEdge(iEdge)
            pr_scale = prandtl_inv * meshScalingDel2(iEdge)
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)

!DIR$ IVDEP
            !$acc loop vector
            do k=1,nVertLevels

!  we are computing the Smagorinsky filter at more points than needed here so as to pick up the delsq_theta for 4th order filter below.
!  This is in conservative form.

               theta_turb_flux = edge_sign*(theta_m(k,cell2) - theta_m(k,cell1))*rho_edge(k,iEdge)
               delsq_theta(k,iCell) = delsq_theta(k,iCell) + theta_turb_flux
               theta_turb_flux = theta_turb_flux*0.5*(eddy_visc_horz(k,cell1)+eddy_visc_horz(k,cell2)) * pr_scale
               tend_theta_euler(k,iCell) = tend_theta_euler(k,iCell) + theta_turb_flux

            end do
         end do
      end do

      !$acc end parallel

!$OMP BARRIER

      if (h_theta_eddy_visc4 > 0.0) then  ! 4th order mixing is active

         !$acc parallel default(present)

         !$acc loop gang worker
         do iCell=cellSolveStart,cellSolveEnd    ! Technically updating fewer cells than before...
            r_areaCell = h_theta_eddy_visc4 * prandtl_inv * invAreaCell(iCell)

            !$acc loop seq
            do i=1,nEdgesOnCell(iCell)

               iEdge = edgesOnCell(i,iCell)
               edge_sign = meshScalingDel4(iEdge)*r_areaCell*dvEdge(iEdge)*edgesOnCell_sign(i,iCell)*invDcEdge(iEdge)

               cell1 = cellsOnEdge(1,iEdge)
               cell2 = cellsOnEdge(2,iEdge)

               !$acc loop vector
               do k=1,nVertLevels
                  tend_theta_euler(k,iCell) = tend_theta_euler(k,iCell) - edge_sign*(delsq_theta(k,cell2) - delsq_theta(k,cell1))
               end do
            end do
         end do

         !$acc end parallel

      end if ! 4th order mixing is active

      if(mix_scalars .and. (dynamics_substep == 1)) then  ! dissipation for scalars, including 4th-order filter.  Likely needs optimization

         do iScalar=1,num_scalars

            !$acc parallel default(present)

            !$acc loop gang worker
            do iCell=cellStart,cellEnd
               !$acc loop vector
               do k = 1, nVertLevels
                  delsq_theta(k,iCell) = 0.0_RKIND
               end do

               ! tend_theta_euler(1:nVertLevels,iCell) = 0.0
               r_areaCell = invAreaCell(iCell)

               !$acc loop seq
               do i=1,nEdgesOnCell(iCell)
                  iEdge = edgesOnCell(i,iCell)
                  edge_sign = r_areaCell*edgesOnCell_sign(i,iCell) * dvEdge(iEdge) * invDcEdge(iEdge)
                  pr_scale = prandtl_inv * meshScalingDel2(iEdge)
                  cell1 = cellsOnEdge(1,iEdge)
                  cell2 = cellsOnEdge(2,iEdge)

!DIR$ IVDEP
                  !$acc loop vector
                  do k=1,nVertLevels

!  we are computing the Smagorinsky filter at more points than needed here so as to pick up the delsq_theta for 4th order filter below.
!  This is in conservative form.

                     theta_turb_flux = edge_sign*(scalars(iScalar,k,cell2) - scalars(iScalar,k,cell1))*rho_edge(k,iEdge)
                     delsq_theta(k,iCell) = delsq_theta(k,iCell) + theta_turb_flux
                     theta_turb_flux = theta_turb_flux*0.5*(eddy_visc_horz(k,cell1)+eddy_visc_horz(k,cell2)) * pr_scale
                     tend_scalars(iScalar,k,iCell) = tend_scalars(iScalar,k,iCell) + theta_turb_flux

                  end do
               end do
            end do

            !$acc end parallel

!$OMP BARRIER

            if (h_theta_eddy_visc4 > 0.0) then  ! 4th order mixing is active

               !$acc parallel default(present)

               !$acc loop gang worker
               do iCell=cellSolveStart,cellSolveEnd    ! Technically updating fewer cells than before...

                  r_areaCell = h_theta_eddy_visc4 * prandtl_inv * invAreaCell(iCell)

                  !$acc loop seq
                  do i=1,nEdgesOnCell(iCell)

                     iEdge = edgesOnCell(i,iCell)
                     edge_sign = meshScalingDel4(iEdge)*r_areaCell*dvEdge(iEdge)*edgesOnCell_sign(i,iCell)*invDcEdge(iEdge)

                     cell1 = cellsOnEdge(1,iEdge)
                     cell2 = cellsOnEdge(2,iEdge)

                     !$acc loop vector
                     do k=1,nVertLevels
                        tend_scalars(iScalar,k,iCell) = tend_scalars(iScalar,k,iCell) - edge_sign*(delsq_theta(k,cell2) - delsq_theta(k,cell1))
                     end do
                  end do
               end do

               !$acc end parallel

            end if ! 4th order mixing is active

         end do ! loop over scalars for horizontal mixing

      end if ! horizontal scalar mixing


      !  idealized case vertical mixing.  No scalar mixing here.

      if ( v_theta_eddy_visc2 > 0.0 ) then  ! vertical mixing for theta_m

         if (config_mix_full) then

            !$acc parallel default(present)

            !$acc loop gang worker
            do iCell = cellSolveStart,cellSolveEnd

               !$acc loop vector
               do k=2,nVertLevels-1
                  z1 = zgrid(k-1,iCell)
                  z2 = zgrid(k  ,iCell)
                  z3 = zgrid(k+1,iCell)
                  z4 = zgrid(k+2,iCell)

                  zm = 0.5*(z1+z2)
                  z0 = 0.5*(z2+z3)
                  zp = 0.5*(z3+z4)

                  tend_theta_euler(k,iCell) = tend_theta_euler(k,iCell) + v_theta_eddy_visc2*prandtl_inv*rho_zz(k,iCell)*(&
                                           (theta_m(k+1,iCell)-theta_m(k  ,iCell))/(zp-z0)                 &
                                          -(theta_m(k  ,iCell)-theta_m(k-1,iCell))/(z0-zm) )/(0.5*(zp-zm))
               end do
            end do

            !$acc end parallel

         else  ! idealized cases where we mix on the perturbation from the initial 1-D state

            !$acc parallel default(present)

            !$acc loop gang worker
            do iCell = cellSolveStart,cellSolveEnd

               !$acc loop vector
               do k=2,nVertLevels-1
                  z1 = zgrid(k-1,iCell)
                  z2 = zgrid(k  ,iCell)
                  z3 = zgrid(k+1,iCell)
                  z4 = zgrid(k+2,iCell)

                  zm = 0.5*(z1+z2)
                  z0 = 0.5*(z2+z3)
                  zp = 0.5*(z3+z4)

                  tend_theta_euler(k,iCell) = tend_theta_euler(k,iCell) + v_theta_eddy_visc2*prandtl_inv*rho_zz(k,iCell)*(&
                                           ((theta_m(k+1,iCell)-t_init(k+1,iCell))-(theta_m(k  ,iCell)-t_init(k,iCell)))/(zp-z0)      &
                                          -((theta_m(k  ,iCell)-t_init(k,iCell))-(theta_m(k-1,iCell)-t_init(k-1,iCell)))/(z0-zm) )/(0.5*(zp-zm))
               end do
            end do

            !$acc end parallel

         end if

      end if

      if ( les_model_opt /= LES_MODEL_NONE ) then

         !$acc parallel default(present)

         !$acc loop gang worker private(turb_vflux, turb_vflux_scalars, prandtl_1d_inverse, rho_k_at_w, zz_at_w)
         do iCell = cellSolveStart,cellSolveEnd ! vertical mixing for each column
            ! compute turbulent fluxes

            turb_vflux(nVertlevels+1) = 0.  !  no turbulent flux out of the domain
            turb_vflux(1) = 0.  !  lower bc flux handled where ???

            if ( les_model_opt == LES_MODEL_3D_SMAGORINSKY ) then
               !$acc loop vector
               do k=2,nVertLevels
                  prandtl_1d_inverse(k) = prandtl_inv
               end do
            else   ! prognostic_1.5_order, isentropic mixing length
               ! do k=2,nVertLevels
               !   delta_z = 0.5*(zgrid(k+1,iCell)-zgrid(k-1,iCell))
               !   delta_s = ((config_len_disp**2)*delta_z)**(1./3.)
               !   bv_frequency2 = 0.5*(bv_freq2(k,iCell)+bv_freq2(k-1,iCell))
               !   tke_length = delta_s
               !   if(bv_frequency2 .gt. 1.e-06) &
               !       tke_length = 0.76*sqrt(scalars(index_tke,k,iCell))/sqrt(bv_frequency2)
               !       tke_length = min(delta_z,tke_length)
               !   prandtl_inverse(k) = 1. + 2.*tke_length/delta_z
               ! end do

               !$acc loop vector
               do k=2,nVertLevels
                  ! prandtl_1d_inverse(k) = 0.5*(prandtl_3d_inv(k,iCell)+prandtl_3d_inv(k-1,iCell))
                  prandtl_1d_inverse(k) = fzm(k)*prandtl_3d_inv(k,iCell)+fzp(k)*prandtl_3d_inv(k-1,iCell)
               end do

            end if

            !$acc loop vector
            do k=2,nVertLevels

               ! delta_z = 0.5*(zgrid(k+1,iCell)-zgrid(k-1,iCell))
               ! delta_s = ((config_len_disp**2)*delta_z)**(1./3.)
               ! bv_frequency2 = 0.5*(bv_freq2(k)+bv_freq(k-1))
               ! bv = max( sqrt(abs(bv_frequency2)), epsilon_bv )
               rho_k_at_w(k) =  fzm(k)*rho_zz(k  ,iCell)*zz(k  ,iCell)*zz(k  ,iCell)*eddy_visc_vert(k  ,iCell) &
                             +fzp(k)*rho_zz(k-1,iCell)*zz(k-1,iCell)*zz(k-1,iCell)*eddy_visc_vert(k-1,iCell)
               zz_at_w(k) = fzm(k)*zz(k,iCell)+fzp(k)*zz(k-1,iCell)
               turb_vflux(k) = - prandtl_1d_inverse(k)*rho_k_at_w(k)*zz_at_w(k)*rdzu(k)*(theta_m(k,iCell)-theta_m(k-1,iCell))
            end do

            ! test boundary conditions for supercell and les test cases

            if( les_surface_opt == LES_SURFACE_SPECIFIED .or. les_surface_opt == LES_SURFACE_VARYING ) then

               if( les_surface_opt == LES_SURFACE_SPECIFIED ) then
                  moisture_flux = config_surface_moisture_flux
                  heat_flux = config_surface_heat_flux

!                 place holder routine for time-varying specified
!                 call flux_les_sas( heat_flux, moisture_flux, clock, dt )

               else if ( les_surface_opt == LES_SURFACE_VARYING ) then
                  heat_flux = hfx(iCell)/rho_zz(1,iCell)/cp
                  moisture_flux = qfx(iCell)/rho_zz(1,iCell)
               endif

               qv_cell = scalars(index_qv,1,iCell)
               theta_m_cell = theta_m(1,iCell)
               theta_cell = theta_m_cell/(1.0+(rv/rgas)*qv_cell)

               theta_m_flux = heat_flux*(1.0+(rv/rgas)*qv_cell)+(rv/rgas)*theta_cell*moisture_flux
               turb_vflux(1) = theta_m_flux*rho_zz(1,iCell)
               moisture_flux = moisture_flux*rho_zz(1,iCell)
               turb_vflux(nVertLevels+1) = turb_vflux(nVertLevels)

            else

               turb_vflux(1) = turb_vflux(2)
               turb_vflux(nVertLevels+1) = turb_vflux(nVertLevels)

            end if

            !$acc loop vector
            do k=1,nVertLevels
               tend_theta_euler(k,iCell) = tend_theta_euler(k,iCell) &
                                           - rdzw(k)*(turb_vflux(k+1)-turb_vflux(k))
            end do

            if (mix_scalars ) then

               ! compute turbulent fluxes
               !$acc loop vector
               do iScalar=1,num_scalars
                  turb_vflux_scalars(iScalar,nVertlevels+1) = 0.0_RKIND  !  no turbulent flux out of the domain
                  turb_vflux_scalars(iScalar,1) = 0.0_RKIND  !  lower bc flux handled where ???
               end do

               !$acc loop vector
               do k=2,nVertLevels
                  rho_k_at_w(k) = fzm(k)*rho_zz(k  ,iCell)*zz(k  ,iCell)*zz(k  ,iCell)*eddy_visc_vert(k  ,iCell) &
                                + fzp(k)*rho_zz(k-1,iCell)*zz(k-1,iCell)*zz(k-1,iCell)*eddy_visc_vert(k-1,iCell)
                  zz_at_w(k) = fzm(k)*zz(k,iCell)+fzp(k)*zz(k-1,iCell)
               end do

               !$acc loop vector collapse(2)
               do k=2,nVertLevels
                  do iScalar=1,num_scalars
                     turb_vflux_scalars(iScalar,k) = - prandtl_1d_inverse(k)*rho_k_at_w(k)*zz_at_w(k)*rdzu(k)*  &
                                                      (scalars(iScalar,k,iCell)-scalars(iScalar,k-1,iCell))
                  end do
               end do

               if( les_surface_opt == LES_SURFACE_SPECIFIED .or. les_surface_opt == LES_SURFACE_VARYING ) turb_vflux_scalars(index_qv,1) = moisture_flux ! lower b.c. for qv

               !$acc loop vector collapse(2)
               do k=1,nVertLevels
                  do iScalar=1,num_scalars
                     tend_scalars(iScalar,k,iCell) = tend_scalars(iScalar,k,iCell) &
                          - rdzw(k)*(turb_vflux_scalars(iScalar,k+1)-turb_vflux_scalars(iScalar,k))
                  end do
               end do

            end if ! mix scalars

         end do  ! loop over cells (columns)

         !$acc end parallel

      end if

      !$acc exit data delete(turb_vflux_scalars)
      !$acc exit data delete(turb_vflux, prandtl_1d_inverse)

      DEBUG_WRITE(' exiting scalar_dissipation_3d ')

   end subroutine scalar_dissipation_3d_les

!-----------

!  subroutine flux_les_sas(heat_flux, moisture_flux, clock, dt)

!     implicit none

!     real (kind=RKIND), intent(out) :: heat_flux, moisture_flux
!     type (MPAS_Clock_type), intent(in) :: clock
!     real (kind=RKIND), intent(in) :: dt

!     real (kind=RKIND), parameter:: t_start_t_flux = 3600.*6.0
!     real (kind=RKIND), parameter:: t_end_t_flux = 3600.*19.50
!     real (kind=RKIND), parameter:: t_start_q_flux = 3600.*7.0
!     real (kind=RKIND), parameter:: t_end_q_flux = 3600.*19.50
!     real (kind=RKIND) :: rel_time_t_flux, rel_time_q_flux
!     real (kind=RKIND) :: time_of_day_seconds
!     type (MPAS_Time_type) :: currTime
!     integer :: H, M, S, S_n, S_d
!     integer :: ierr

!     currTime = mpas_get_clock_time(clock, MPAS_NOW, ierr)
!     call mpas_get_time(curr_time=currTime, H=H, M=M, S=S, S_n=S_n, S_d=S_d)
!     time_of_day_seconds = real(H)*3600. + real(M)*60. + real(S) + real(S_n)/real(S_d) + 0.5*dt
!     call mpas_log_write(' les integration, timestep midpoint time of day in seconds, $r ', realArgs=(/time_of_day_seconds/))

!     rel_time_t_flux = max(0.,(time_of_day_seconds - t_start_t_flux)/(t_end_t_flux - t_start_t_flux))
!     rel_time_q_flux = max(0.,(time_of_day_seconds - t_start_q_flux)/(t_end_q_flux - t_start_q_flux))

!     heat_flux = max(0., 0.1*sin(pii*rel_time_t_flux))
!     moisture_flux = max(0., 0.15*sin(pii*rel_time_q_flux))/1000.

!  end subroutine flux_les_sas

end module mpas_atm_dissipation_models
