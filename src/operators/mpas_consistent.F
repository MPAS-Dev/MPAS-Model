!***********************************************************************
!
!  mpas_consistent
!
!> \brief   MPAS  module with routines for consistent scheme
!> \author  Pedro Peixoto
!> \date   Jan/2016
!> \details
!> This module provides routines for performing the consistent scheme methods of Peixoto JCP 2016
!
!-----------------------------------------------------------------------
module mpas_consistent

  use mpas_kind_types
  use mpas_derived_types
  use mpas_pool_routines
  use mpas_constants
  use mpas_vector_operations
  use mpas_log, only : mpas_log_write
  
  implicit none

  private

  public :: mpas_convert_grid_to_hcm, &
            mpas_barycentric_coefs_edge, &
            mpas_barycentric_coefs_vertex, &
            mpas_perot_reconstruct, &
            mpas_consist_tang_vel_comp, &
            mpas_consist_verify_config, &
            mpas_consist_weights_on_edge

  !interface mpas_perot_reconstruct
     !module procedure mpas_reconstruct_1d
  !   module procedure mpas_reconstruct_perot
  !end interface

  contains


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       convert_grid_to_hcm
!
!       > \brief   convert grid to hcm staggering - See Peixoto 2015
!       > \author  Pedro S. Peixoto
!       > \date    10/28/15
!       > \details
!       > Overwrites the edge position to be relative to
!       > the midpoint of the Voronoi cell edges instead of the Triangle edges.
!
!-----------------------------------------------------------------------
  subroutine mpas_convert_grid_to_hcm(meshPool)

  implicit none
    type (mpas_pool_type), intent(in) :: meshPool

  integer, pointer :: nCells, nVertices, nEdges

  real(kind=RKIND) :: pii, omega

  integer :: i

  real (kind=RKIND), dimension(:), pointer :: xCell, yCell, zCell, latCell, lonCell
  real (kind=RKIND), dimension(:), pointer :: xVertex, yVertex, zVertex, latVertex, lonVertex
  real (kind=RKIND), dimension(:), pointer :: xEdge, yEdge, zEdge, latEdge, lonEdge, angleEdge, fEdge, fVertex
  integer, dimension(:,:), pointer :: verticesOnEdge

  !integer, dimension(2) :: onEdgeStart
  integer, dimension(2) :: onEdgeCount

  real (kind=RKIND) :: original_latitude_radians, original_longitude_radians, new_latitude_radians, new_longitude_radians
  real (kind=RKIND) :: thetaLat, thetaLon, thetaBirdsEye
  real (kind=RKIND) :: x0LongitudeAtEquator, y0LongitudeAtEquator, z0LongitudeAtEquator
  real (kind=RKIND) :: uCrossProduct, vCrossProduct, wCrossProduct
  real (kind=RKIND) :: xNew, yNew, zNew

  real (kind=RKIND) :: v
  real (kind=RKIND) :: ax, ay, az
  real (kind=RKIND) :: bx, by, bz
  real (kind=RKIND) :: cx, cy, cz

  character (len=3) :: HCm="HCm"

  pii = 2.*asin(1.0)
  omega = 2.0*pii / 86400.0

  call mpas_pool_get_dimension(meshPool, 'nCells', nCells)
  call mpas_pool_get_dimension(meshPool, 'nEdges', nEdges)
  call mpas_pool_get_dimension(meshPool, 'nVertices', nVertices)

  call mpas_pool_get_array(meshPool, 'xCell', xCell)
  call mpas_pool_get_array(meshPool, 'yCell', yCell)
  call mpas_pool_get_array(meshPool, 'zCell', zCell)
  call mpas_pool_get_array(meshPool, 'latCell', latCell)
  call mpas_pool_get_array(meshPool, 'lonCell', lonCell)

  call mpas_pool_get_array(meshPool, 'xVertex', xVertex)
  call mpas_pool_get_array(meshPool, 'yVertex', yVertex)
  call mpas_pool_get_array(meshPool, 'zVertex', zVertex)
  call mpas_pool_get_array(meshPool, 'latVertex', latVertex)
  call mpas_pool_get_array(meshPool, 'lonVertex', lonVertex)
  call mpas_pool_get_array(meshPool, 'fVertex', fVertex)

  call mpas_pool_get_array(meshPool, 'xEdge', xEdge)
  call mpas_pool_get_array(meshPool, 'yEdge', yEdge)
  call mpas_pool_get_array(meshPool, 'zEdge', zEdge)
  call mpas_pool_get_array(meshPool, 'latEdge', latEdge)
  call mpas_pool_get_array(meshPool, 'lonEdge', lonEdge)
  call mpas_pool_get_array(meshPool, 'angleEdge', angleEdge)
  call mpas_pool_get_array(meshPool, 'fEdge', fEdge)

  call mpas_pool_get_array(meshPool, 'verticesOnEdge', verticesOnEdge)

  !data onEdgeStart /1, 1/
  onEdgeCount(1) = 2
  onEdgeCount(2) = nEdges

  call mpas_log_write( " Converting grid to HCm staggering")

  do i=1,nEdges

!       First vertex coordinates
     ax = xVertex(verticesOnEdge(1,i))
     ay = yVertex(verticesOnEdge(1,i))
     az = zVertex(verticesOnEdge(1,i))

!       Second vertex coordinates
     cx = xVertex(verticesOnEdge(2,i))
     cy = yVertex(verticesOnEdge(2,i))
     cz = zVertex(verticesOnEdge(2,i))

!       Voronoi edge midpoint - cartesian line
!       temporary use bx, by, bz
     bx = (ax+cx)/2.0
     by = (ay+cy)/2.0
     bz = (az+cz)/2.0

!       Normalize to sphere
     v = sqrt(bx**2.0 + by**2.0 + bz**2.0)
     xEdge(i) = bx / v
     yEdge(i) = by / v
     zEdge(i) = bz / v

!       Convert to lat lon
     !print*, "Before:", i, latEdge(i), lonEdge(i)
     call convert_xl(xEdge(i), yEdge(i), zEdge(i), latEdge(i), lonEdge(i))
     !print*, i, xEdge(i), yEdge(i), zEdge(i), latEdge(i), lonEdge(i)
     !print*, "After:", i, latEdge(i), lonEdge(i)

!       compute new angle edge
     bx = -cos(lonEdge(i))*sin(latEdge(i)) + ax
     by = -sin(lonEdge(i))*sin(latEdge(i)) + ay
     bz =  cos(latEdge(i))                 + az

     v = sqrt(bx**2.0 + by**2.0 + bz**2.0)
     bx = bx / v
     by = by / v
     bz = bz / v

!       Compute angle between AB and AC lines
     angleEdge(i) = sphere_angle(ax, ay, az, bx, by, bz, cx, cy, cz)

     fEdge(i)=2*omega*sin(latEdge(i))

  end do

  end subroutine mpas_convert_grid_to_hcm

!***********************************************************************
!
!  routine mpas_consist_verify_config
!
!> \brief   checks if the config_hcm_staggering matches the grid given in the initial
!>           conditions
!> \author  Pedro Peixoto
!> \date    Nov 2015
!> \details
!>  Purpose: Prints a warning if namelist does not match grid staggering
!>  Input: grid meta data and configs
!>  Output: config_hcm_staggering=true/false - write warning
!-----------------------------------------------------------------------
  subroutine mpas_consist_verify_config(mesh, configs) !{{{

      implicit none
      type (mpas_pool_type), intent(in) :: mesh     !< Input: Mesh information
      type (mpas_pool_type), intent(in) :: configs  !< Input: Configs information
      integer :: i, iEdge, iCell1, iCell2
      integer, pointer:: nEdges, nCells, nEdgesSolve, nCellsSolve
      integer, dimension(:,:), pointer :: cellsOnEdge

      real (kind=RKIND), pointer :: sphere_radius
      real (kind=RKIND), dimension(:), pointer :: xCell, yCell, zCell
      real (kind=RKIND), dimension(:), pointer :: xEdge, yEdge, zEdge
      real (kind=RKIND), dimension(:), pointer :: dcEdge

      logical, pointer :: on_a_sphere
      logical, pointer :: config_consistent_scheme
      logical, pointer :: config_hcm_staggering

      real (kind=RKIND) :: xTrEdgeMidpoint, yTrEdgeMidpoint, zTrEdgeMidpoint, ptNorm
      real (kind=RKIND) :: edgeDisp, edgeDispMax
      real (kind=RKIND), dimension(1:3) :: p1, p2, p3, p

      call mpas_pool_get_dimension(mesh, 'nEdges', nEdges)
      call mpas_pool_get_dimension(mesh, 'nEdgesSolve', nEdgesSolve)
      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      call mpas_pool_get_dimension(mesh, 'nCellsSolve', nCellsSolve)
      call mpas_pool_get_array(mesh, 'xCell', xCell)
      call mpas_pool_get_array(mesh, 'yCell', yCell)
      call mpas_pool_get_array(mesh, 'zCell', zCell)
      call mpas_pool_get_array(mesh, 'xEdge', xEdge)
      call mpas_pool_get_array(mesh, 'yEdge', yEdge)
      call mpas_pool_get_array(mesh, 'zEdge', zEdge)
      call mpas_pool_get_array(mesh, 'dcEdge', dcEdge)
      call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
      call mpas_pool_get_config(mesh, 'sphere_radius', sphere_radius)
      call mpas_pool_get_config(mesh, 'on_a_sphere', on_a_sphere)

      call mpas_pool_get_config(configs, 'config_consistent_scheme', config_consistent_scheme)
      call mpas_pool_get_config(configs, 'config_hcm_staggering', config_hcm_staggering)

      edgeDispMax=0.0
      do iEdge=1, nEdges
	      iCell1 = cellsOnEdge(1,iEdge)
	      iCell2 = cellsOnEdge(2,iEdge)

	      !Extreme halo case - improper to calculate here
	      if(iCell1 > nCells .or. iCell2 > nCells)then
	       cycle
	      end if

	      !Calculate midpoint of the triangle edge (the one that intersects the Voronoi cell edges
	      ! There is no need to divide by 2 (average) because it is going to be nromalized anyway
	      xTrEdgeMidpoint = xCell(iCell1) + xCell(iCell2)
	      yTrEdgeMidpoint = yCell(iCell1) + yCell(iCell2)
	      zTrEdgeMidpoint = zCell(iCell1) + zCell(iCell2)
	      ptNorm= xTrEdgeMidpoint**2 + yTrEdgeMidpoint**2 + zTrEdgeMidpoint**2
	      ptNorm=sqrt(ptNorm)
	      if (on_a_sphere)then
	        xTrEdgeMidpoint=sphere_radius*xTrEdgeMidpoint/ptNorm
	        yTrEdgeMidpoint=sphere_radius*yTrEdgeMidpoint/ptNorm
	        zTrEdgeMidpoint=sphere_radius*zTrEdgeMidpoint/ptNorm
	      end if

	      !Distance from edge point given in the mesh and the triangle edge midpoint
	      !  (sanity check for HCm or HCt grid)
	      edgeDisp=arc_length(xTrEdgeMidpoint, yTrEdgeMidpoint, zTrEdgeMidpoint, &
	                          xEdge(iEdge),    yEdge(iEdge),    zEdge(iEdge))
	      !Normalize edgeDisp
	      edgeDisp=edgeDisp/dcEdge(iEdge)
	      edgeDispMax=max(edgeDisp, edgeDispMax)
      end do

      !Write warnings
      
      if(edgeDispMax>0.000000001)then
          if(.not.config_hcm_staggering)then
            call mpas_log_write('WARNING: Inconsistent choice in namelist config_hcm_staggering')
          end if
          config_hcm_staggering = .true.
          call mpas_log_write( 'Using a HCm grid: velocities on the midpoint of Voronoi edges, not triangle edges')
      else
          if(config_hcm_staggering)then
            call mpas_log_write('WARNING: Inconsistent choice in namelist config_hcm_staggering')
          end if
          call mpas_log_write("Using a HCt grid: velocities on the midpoint of triangle edges, not Voronoi edges")
          if(config_consistent_scheme)then
            call mpas_log_write( "WARNING: The consistent scheme has better results using the HCm staggering")
            call mpas_log_write( "          run init_atmosphere with config_use_hcm_staggering=true" )
          end if
      end if

      !On HCt grids there is no need to do barycenctric coordinate interpolation on edges
      if(config_hcm_staggering) then
        !config_bary_interpol_edge=.false.
      end if

      return
  end subroutine mpas_consist_verify_config!}}}

!***********************************************************************
!
!  routine mpas_consist_weights_on_edge
!
!> \brief Calculates weights for the edge reconstruction using
!>           the consistent scheme of Peixoto 2015
!>
!> \author  Pedro Peixoto
!> \date    Nov 2015
!> \details
!>  Purpose:
!>  Input: grid meta data
!>  Output: grid meta data with weights
!>
!> IMPORTANT: this routine overwrites the following mesh arrays:
!>              weightsOnEdge, nEdgesOnEdge, edgesOnEdge
!-----------------------------------------------------------------------
  subroutine mpas_consist_weights_on_edge(mesh) !{{{

      implicit none
      type (mpas_pool_type) :: mesh     !< Input: Mesh information

      integer :: i, j, k, iEdge, iCell, eoe, eoc, cell1, cell2
      integer, pointer:: nEdges, nCells, nEdgesSolve, nCellsSolve, nVertLevels
      integer, dimension(:), pointer :: nEdgesOnCell, nEdgesOnEdge
      integer, dimension(:,:), pointer :: cellsOnEdge
      integer, dimension(:,:), pointer :: edgesOnCell, edgesOnEdge

      real (kind=RKIND), pointer :: sphere_radius
      real (kind=RKIND), dimension(:), pointer :: xCell, yCell, zCell
      real (kind=RKIND), dimension(:), pointer :: xEdge, yEdge, zEdge
      real (kind=RKIND), dimension(:), pointer :: dcEdge, dvEdge, invAreaCell, areaCell
      real (kind=RKIND), dimension(:,:), pointer :: weightsOnEdge
      real (kind=RKIND), dimension(:,:), pointer :: edgeNormalVectors

      logical, pointer :: on_a_sphere

      real (kind=RKIND) :: xTrEdgeMidpoint, yTrEdgeMidpoint, zTrEdgeMidpoint, ptNorm
      real (kind=RKIND) :: edgeDisp, edgeDispMax, signcor
      real (kind=RKIND), dimension(1:3) :: p1, p2, p3, p

      real (kind=RKIND), dimension(1:3) :: vec_tmp, vec_cor, tang_vec, xyzEdge, xyzCell
      real (kind=RKIND), dimension(1:20) :: w
      integer, dimension(1:20):: w_ed

      call mpas_pool_get_dimension(mesh, 'nEdges', nEdges)
      call mpas_pool_get_dimension(mesh, 'nEdgesSolve', nEdgesSolve)
      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      call mpas_pool_get_dimension(mesh, 'nCellsSolve', nCellsSolve)
      call mpas_pool_get_array(mesh, 'xCell', xCell)
      call mpas_pool_get_array(mesh, 'yCell', yCell)
      call mpas_pool_get_array(mesh, 'zCell', zCell)
      call mpas_pool_get_array(mesh, 'xEdge', xEdge)
      call mpas_pool_get_array(mesh, 'yEdge', yEdge)
      call mpas_pool_get_array(mesh, 'zEdge', zEdge)
      call mpas_pool_get_array(mesh, 'dvEdge', dvEdge)
      call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
      call mpas_pool_get_array(mesh, 'weightsOnEdge', weightsOnEdge)
      call mpas_pool_get_array(mesh, 'nEdgesOnEdge', nEdgesOnEdge)
      call mpas_pool_get_array(mesh, 'nEdgesOnCell', nEdgesOnCell)
      call mpas_pool_get_array(mesh, 'edgesOnEdge', edgesOnEdge)
      call mpas_pool_get_array(mesh, 'edgesOnCell', edgesOnCell)
      call mpas_pool_get_array(mesh, 'edgeNormalVectors', edgeNormalVectors)
      call mpas_pool_get_array(mesh, 'invAreaCell', invAreaCell)
      call mpas_pool_get_array(mesh, 'areaCell', areaCell)
      call mpas_pool_get_config(mesh, 'sphere_radius', sphere_radius)
      call mpas_pool_get_config(mesh, 'on_a_sphere', on_a_sphere)

      do iEdge = 1,nEdges !This has extra information because on halos
        !print*, iEdge, edgesOnEdge(1:nEdgesOnEdge(iEdge),iEdge)
        !do iCell=1,2
        !Calculate the edge tangent vector
        xyzEdge(1)=xEdge(iEdge)
        xyzEdge(2)=yEdge(iEdge)
        xyzEdge(3)=zEdge(iEdge)
        !print*, edgeNormalVectors(:,iEdge)
        call mpas_cross_product_in_r3(xyzEdge, edgeNormalVectors(:,iEdge), tang_vec)
        call mpas_unit_vec_in_r3(tang_vec)
        k=1
        nEdgesOnEdge(iEdge)=0
        do i=1,2
          iCell = cellsOnEdge(i,iEdge)
          xyzCell(1)=xCell(iCell)
          xyzCell(2)=yCell(iCell)
          xyzCell(3)=zCell(iCell)

          do j=1, nEdgesOnCell(iCell)
            eoc=edgesOnCell(j,iCell)
            !if( eoc == iEdge )then
            !  cycle
            !end if
            xyzEdge(1)=xEdge(eoc)
            xyzEdge(2)=yEdge(eoc)
            xyzEdge(3)=zEdge(eoc)
            vec_tmp=xyzEdge-xyzCell
            !w(k)=0.5*dvEdge(eoc)*dot_product(vec_tmp, tang_vec)*invAreaCell(iCell)

            !Calculate weight as in Peixoto 2015
            weightsOnEdge(k,iEdge)=0.5*dvEdge(eoc)*dot_product(vec_tmp, tang_vec)*invAreaCell(iCell)


            !Correct the sign to match the orientation of u on edges
            cell1 = cellsOnEdge(1,eoc)
            cell2 = cellsOnEdge(2,eoc)
            vec_cor(1)=xCell(cell2)-xCell(cell1)
            vec_cor(2)=yCell(cell2)-yCell(cell1)
            vec_cor(3)=zCell(cell2)-zCell(cell1)
            !u is stored from  cell1 --> cell2
            signcor=dot_product(vec_cor, vec_tmp)
            !if sign_cor is positive, then u point outwards with respect to the cell, else inwards
            weightsOnEdge(k,iEdge)=sign(1.0, signcor)*weightsOnEdge(k,iEdge)

            !Correct the edeOnEdge to match the weights
            edgesOnEdge(k,iEdge)=eoc

            !print*, iEdge, iCell, eoc, dvEdge(eoc), invAreaCell(iCell), AreaCell(iCell), dot_product(vec_tmp, tang_vec)
            !w_ed(k)=eoc
            k=k+1
          end do
          !Correct the nEdgesOnEdge, since now it also has the edge itself (iEdge)
          nEdgesOnEdge(iEdge)=nEdgesOnEdge(iEdge)+nEdgesOnCell(iCell)
        end do
       ! print*
       !do i=1,nEdgesOnEdge(iEdge)
       !   eoe = edgesOnEdge(i,iEdge)
       !   print*, iEdge, eoe, w_ed(i), weightsOnEdge(i,iEdge),  w(i)
          !weightsOnEdge(i,iEdge)=0.0
       !end do
       !print*
      end do

      return
  end subroutine mpas_consist_weights_on_edge!}}}


!***********************************************************************
!
!  routine mpas_tangential_vel_component
!
!> \brief   2d MPAS Vector reconstruction of the tangential velocity
!> \author  Pedro S. Peixoto
!> \date    11/11/15
!> \details
!>  Purpose: reconstruct tangential velocity using method in Peixoto JCP 2015
!>  Input: grid meta data and vector component data residing at cell edges and field relative to normal of edges (u)
!>  Output: field relative to tangential velocities (v)
!>
!>  Uses the info given in uReconstructXYZ to build the tangential compoent - uReconstruct needs to be precalculated
!-----------------------------------------------------------------------
  subroutine mpas_consist_tang_vel_comp(meshPool, u, uReconstructX, uReconstructY, uReconstructZ, v)!{{{

    implicit none

    type (mpas_pool_type), intent(in) :: meshPool !< Input: Mesh information
    real (kind=RKIND), dimension(:,:), intent(in) :: u !< Input: Velocity field on edges
    real (kind=RKIND), dimension(:,:), intent(in) :: uReconstructX !< Input: X Component of velocity reconstructed to cell centers
    real (kind=RKIND), dimension(:,:), intent(in) :: uReconstructY !< Input: Y Component of velocity reconstructed to cell centers
    real (kind=RKIND), dimension(:,:), intent(in) :: uReconstructZ !< Input: Z Component of velocity reconstructed to cell centers
    real (kind=RKIND), dimension(:,:), intent(out) :: v !< Output: Tangential velocity field on edges

    integer, pointer :: nVertLevels, nEdges, nEdgesSolve
    integer, dimension(:,:), pointer :: cellsOnEdge

    real (kind=RKIND), dimension(:), pointer :: xEdge, yEdge, zEdge
    real (kind=RKIND), dimension(:,:), pointer :: edgeNormalVectors


    integer :: iCell, iEdge, i, k, cell1, cell2
    real (kind=RKIND), dimension(1:3) :: vec_tmp, tang_vec, xyzEdge

    call mpas_pool_get_dimension(meshPool, 'nVertLevels', nVertLevels)
    call mpas_pool_get_dimension(meshPool, 'nEdges', nEdges)
    call mpas_pool_get_dimension(meshPool, 'nEdgesSolve', nEdgesSolve)

    call mpas_pool_get_array(meshPool, 'xEdge', xEdge)
    call mpas_pool_get_array(meshPool, 'yEdge', yEdge)
    call mpas_pool_get_array(meshPool, 'zEdge', zEdge)
    call mpas_pool_get_array(meshPool, 'cellsOnEdge', cellsOnEdge)
    call mpas_pool_get_array(meshPool, 'edgeNormalVectors', edgeNormalVectors)

    v(:,:)=0
    !print*, "In tg recon"
    !Interpolate reconstructed velocities to the edge
    do iEdge = 1,nEdgesSolve !Can only do inner edges, because of halo
      cell1 = cellsOnEdge(1,iEdge)
      cell2 = cellsOnEdge(2,iEdge)
      xyzEdge(1)=xEdge(iEdge)
      xyzEdge(2)=yEdge(iEdge)
      xyzEdge(3)=zEdge(iEdge)
      do k=1,nVertLevels
        vec_tmp(1) = uReconstructX(k, cell1) + uReconstructX(k, cell2)
        vec_tmp(2) = uReconstructY(k, cell1) + uReconstructY(k, cell2)
        vec_tmp(3) = uReconstructZ(k, cell1) + uReconstructZ(k, cell2)
        vec_tmp = 0.5 * vec_tmp

        !Project in the tangential direction
        call mpas_cross_product_in_r3(xyzEdge, edgeNormalVectors(:,iEdge), tang_vec)
        call mpas_unit_vec_in_r3(tang_vec)
        v(k,iEdge)=dot_product(vec_tmp, tang_vec)
        !print*, iEdge, cell1, cell2, k, v(k,iEdge), uReconstructX(k, cell1), uReconstructX(k, cell2)
      end do
    end do

    return
  end subroutine mpas_consist_tang_vel_comp!}}}

!***********************************************************************
!
!  routine mpas_perp_term
!
!> \brief   2d MPAS Vector reconstruction of the perpendicular term
!> \author  Pedro S. Peixoto
!> \date    11/11/15
!> \details
!>  Purpose: Calculate the term rho*pv*v for each edge using the scheme of Peixoto JCP (2015)
!>  Input: grid meta data and data fields ru, pv_edge
!>  Output: perpendicular term for this edge
!>
!> This routine can be highly optimized if edge weights are pre-computed
!>
!-----------------------------------------------------------------------
!  subroutine mpas_perp_term(iEdge, kVert, meshPool, ru, pv_edge, q_perp)!{{{
!
!    implicit none
!
!    integer, intent(in) ::  iEdge, kVert          !< Input: Edge and vertical level
!    type (mpas_pool_type), intent(in) :: meshPool !< Input: Mesh information
!    real (kind=RKIND), dimension(:,:), intent(in) :: ru !< Input: Velocity field on edges
!    real (kind=RKIND), dimension(:,:), intent(in) :: pv_edge !< Input: pv_edge
!    real (kind=RKIND), dimension(:,:), intent(out) :: q_perp !< Output: perpendicular term
!
!    integer, pointer :: nCells, nCells
!    integer, dimension(:,:), pointer :: cellsOnEdge
!
!    real(kind=RKIND), dimension(:), pointer :: xEdge, yEdge, zEdge
!    real(kind=RKIND), dimension(:,:), pointer :: edgeNormalVectors
!
!
!    integer :: iCell, iEdge, i, k, cell1, cell2
!    real (kind=RKIND), dimension(1:3) :: vec_tmp, tang_vec, xyzEdge
!
!    call mpas_pool_get_dimension(meshPool, 'nVertLevels', nVertLevels)
!    call mpas_pool_get_dimension(meshPool, 'nEdges', nEdges)
!    call mpas_pool_get_dimension(meshPool, 'nEdgesSolve', nEdgesSolve)
!
!    call mpas_pool_get_array(meshPool, 'xEdge', xEdge)
!    call mpas_pool_get_array(meshPool, 'yEdge', yEdge)
!    call mpas_pool_get_array(meshPool, 'zEdge', zEdge)
!    call mpas_pool_get_array(meshPool, 'cellsOnEdge', cellsOnEdge)
!    call mpas_pool_get_array(meshPool, 'edgeNormalVectors', edgeNormalVectors)
!
!    v(:,:)=0
!    !print*, "In tg recon"
!    !Interpolate reconstructed velocities to the edge
!    do iEdge = 1,nEdgesSolve !Can only do inner edges, because of halo
!      cell1 = cellsOnEdge(1,iEdge)
!      cell2 = cellsOnEdge(2,iEdge)
!      xyzEdge(1)=xEdge(iEdge)
!      xyzEdge(2)=yEdge(iEdge)
!      xyzEdge(3)=zEdge(iEdge)
!      do k=1,nVertLevels
!        vec_tmp(1) = uReconstructX(k, cell1) + uReconstructX(k, cell2)
!        vec_tmp(2) = uReconstructY(k, cell1) + uReconstructY(k, cell2)
!        vec_tmp(3) = uReconstructZ(k, cell1) + uReconstructZ(k, cell2)
!        vec_tmp = 0.5 * vec_tmp
!
!        !Project in the tangential direction
!        call mpas_cross_product_in_r3(xyzEdge, edgeNormalVectors(:,iEdge), tang_vec)
!        call mpas_unit_vec_in_r3(tang_vec)
!        v(k,iEdge)=dot_product(vec_tmp, tang_vec)
!        !print*, iEdge, cell1, cell2, k, v(k,iEdge), uReconstructX(k, cell1), uReconstructX(k, cell2)
!      end do
!    end do
!
!    return
!  end subroutine mpas_perp_term!}}}

!***********************************************************************
!
!  routine mpas_reconstruct_2d
!
!> \brief   2d MPAS Vector reconstruction routine using Perot scheme
!> \author  Pedro S. Peixoto
!> \date    11/11/15
!> \details
!>  Purpose: reconstruct vector field at cell centers based on Perot's method (see Peixoto and Barros JCP 2014)
!>  Input: grid meta data and vector component data residing at cell edges
!>  Output: reconstructed vector field (measured in X,Y,Z) located at cell centers
!-----------------------------------------------------------------------
  subroutine mpas_perot_reconstruct(meshPool, u, uReconstructX, uReconstructY, uReconstructZ, uReconstructZonal, uReconstructMeridional)!{{{

    implicit none

    type (mpas_pool_type), intent(in) :: meshPool !< Input: Mesh information
    real (kind=RKIND), dimension(:,:), intent(in) :: u !< Input: Velocity field on edges
    real (kind=RKIND), dimension(:,:), intent(out) :: uReconstructX !< Output: X Component of velocity reconstructed to cell centers
    real (kind=RKIND), dimension(:,:), intent(out) :: uReconstructY !< Output: Y Component of velocity reconstructed to cell centers
    real (kind=RKIND), dimension(:,:), intent(out) :: uReconstructZ !< Output: Z Component of velocity reconstructed to cell centers
    real (kind=RKIND), dimension(:,:), intent(out) :: uReconstructZonal !< Output: Zonal Component of velocity reconstructed to cell centers
    real (kind=RKIND), dimension(:,:), intent(out) :: uReconstructMeridional !< Output: Meridional Component of velocity reconstructed to cell centers

    !   temporary arrays needed in the compute procedure
    integer, pointer :: nCellsSolve, nVertLevels, nEdges, nCells
    integer, dimension(:,:), pointer :: edgesOnCell
    integer, dimension(:,:), pointer :: cellsOnEdge
    integer, dimension(:), pointer :: nEdgesOnCell
    real (kind=RKIND), dimension(:), pointer :: dvEdge, areaCell
    real(kind=RKIND), dimension(:), pointer :: xCell, yCell, zCell, xEdge, yEdge, zEdge

    integer :: iCell, iEdge, i, k, cell1, cell2
    real(kind=RKIND), dimension(:), pointer :: latCell, lonCell

    logical, pointer :: on_a_sphere

    real (kind=RKIND) :: clat, slat, clon, slon, r, x_proj, y_proj, z_proj

    call mpas_pool_get_array(meshPool, 'xCell', xCell)
    call mpas_pool_get_array(meshPool, 'yCell', yCell)
    call mpas_pool_get_array(meshPool, 'zCell', zCell)
    call mpas_pool_get_array(meshPool, 'xEdge', xEdge)
    call mpas_pool_get_array(meshPool, 'yEdge', yEdge)
    call mpas_pool_get_array(meshPool, 'zEdge', zEdge)

    call mpas_pool_get_dimension(meshPool, 'nVertLevels', nVertLevels)
    call mpas_pool_get_array(meshPool, 'cellsOnEdge', cellsOnEdge)
    call mpas_pool_get_array(meshPool, 'dvEdge', dvEdge)
    call mpas_pool_get_array(meshPool, 'areaCell', areaCell)

    call mpas_pool_get_array(meshPool, 'nEdgesOnCell', nEdgesOnCell)
    call mpas_pool_get_array(meshPool, 'edgesOnCell', edgesOnCell)
    call mpas_pool_get_dimension(meshPool, 'nEdges', nEdges)
    call mpas_pool_get_dimension(meshPool, 'nCellsSolve', nCellsSolve)
    call mpas_pool_get_dimension(meshPool, 'nCells', nCells)

    call mpas_pool_get_array(meshPool, 'latCell', latCell)
    call mpas_pool_get_array(meshPool, 'lonCell', lonCell)

    call mpas_pool_get_config(meshPool, 'on_a_sphere', on_a_sphere)

    ! init the intent(out)
    uReconstructX = 0.0
    uReconstructY = 0.0
    uReconstructZ = 0.0

    !Apply reconstruction in a similar way as the divergence calculation
    do iEdge=1,nEdges
       cell1 = cellsOnEdge(1,iEdge)
       cell2 = cellsOnEdge(2,iEdge)
       do k=1,nVertLevels
         uReconstructX(k,cell1) = uReconstructX(k,cell1) + u(k,iEdge)*dvEdge(iEdge)*(xEdge(iEdge)-xCell(cell1))
         uReconstructY(k,cell1) = uReconstructY(k,cell1) + u(k,iEdge)*dvEdge(iEdge)*(yEdge(iEdge)-yCell(cell1))
         uReconstructZ(k,cell1) = uReconstructZ(k,cell1) + u(k,iEdge)*dvEdge(iEdge)*(zEdge(iEdge)-zCell(cell1))

         uReconstructX(k,cell2) = uReconstructX(k,cell2) - u(k,iEdge)*dvEdge(iEdge)*(xEdge(iEdge)-xCell(cell2))
         uReconstructY(k,cell2) = uReconstructY(k,cell2) - u(k,iEdge)*dvEdge(iEdge)*(yEdge(iEdge)-yCell(cell2))
         uReconstructZ(k,cell2) = uReconstructZ(k,cell2) - u(k,iEdge)*dvEdge(iEdge)*(zEdge(iEdge)-zCell(cell2))
       end do
    end do

    !print*, nCellsSolve, nCells
    !Normalize vector and project to the sphere
    do iCell = 1,nCells !The original had nCellsSolve
       r = 1.0 / areaCell(iCell)
       do k = 1,nVertLevels
         uReconstructX(k,iCell) = uReconstructX(k,iCell) * r
         uReconstructY(k,iCell) = uReconstructY(k,iCell) * r
         uReconstructZ(k,iCell) = uReconstructZ(k,iCell) * r
         !Project to be tangent to the sphere
         call proj_vec_sphere(uReconstructX(k,iCell), uReconstructY(k,iCell), uReconstructZ(k,iCell), &
                              xCell(iCell), yCell(iCell), zCell(iCell), &
                              x_proj, y_proj, z_proj)
        uReconstructX(k,iCell)=x_proj
        uReconstructY(k,iCell)=y_proj
        uReconstructZ(k,iCell)=z_proj
        !print*, iCell, nCells, nCellsSolve,  k, uReconstructX(k,iCell), uReconstructY(k,iCell), uReconstructZ(k,iCell)
       end do
    end do

    if (on_a_sphere) then
      do iCell = 1, nCells ! The original had nCellsSolve
        clat = cos(latCell(iCell))
        slat = sin(latCell(iCell))
        clon = cos(lonCell(iCell))
        slon = sin(lonCell(iCell))
        uReconstructZonal(:,iCell) = -uReconstructX(:,iCell)*slon + uReconstructY(:,iCell)*clon
        uReconstructMeridional(:,iCell) = -(uReconstructX(:,iCell)*clon &
          + uReconstructY(:,iCell)*slon)*slat &
          + uReconstructZ(:,iCell)*clat
      end do
    else
      uReconstructZonal = uReconstructX
      uReconstructMeridional = uReconstructY
    end if

    return
  end subroutine mpas_perot_reconstruct!}}}


!***********************************************************************
!
!  routine mpas_barycentric_coefs_vertex
!
!> \brief   MPAS Calculate barycentric coordinate coefficients for
!>           scalar interpolation to triangle circumcenters (vertices)
!> \author  Pedro Peixoto
!> \date    Nov 2015
!> \details
!>  Purpose: Compute barycentric coefficients for linear interpolation
!>  Input: grid meta data and an vertex index
!>  Output: barycentric coordinates indexed in the same order as
!>          as the vertices of triangle
!-----------------------------------------------------------------------
  subroutine mpas_barycentric_coefs_vertex(iVertex, mesh, bar_coefs) !{{{

      implicit none
      type (mpas_pool_type), intent(in) :: mesh     !< Input: Mesh information
      integer, intent(in) :: iVertex               !< Input: Triangle/vertex index
      real (kind=RKIND), dimension(1:3), intent(out) :: bar_coefs !< Output: Barycentric coefficients

      integer, pointer:: nCells, nVertices
      real (kind=RKIND), dimension(:), pointer :: xCell, yCell, zCell
      real (kind=RKIND), dimension(:), pointer :: xVertex, yVertex, zVertex
      integer, dimension(:,:), pointer :: cellsOnVertex
      real (kind=RKIND), dimension(1:3) :: p1, p2, p3, p


      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      call mpas_pool_get_dimension(mesh, 'nVertices', nVertices)
      call mpas_pool_get_array(mesh, 'xCell', xCell)
      call mpas_pool_get_array(mesh, 'yCell', yCell)
      call mpas_pool_get_array(mesh, 'zCell', zCell)
      call mpas_pool_get_array(mesh, 'xVertex', xVertex)
      call mpas_pool_get_array(mesh, 'yVertex', yVertex)
      call mpas_pool_get_array(mesh, 'zVertex', zVertex)
      call mpas_pool_get_array(mesh, 'cellsOnVertex', cellsOnVertex)

      bar_coefs=0.0

      !Extreme halo case - improper to calculate here
      if(maxval(cellsOnVertex(1:3,iVertex)) > nCells )then
        return
      end if

      !Edge Point to have scalars interpolated to
      p(1)=xVertex(iVertex)
      p(2)=yVertex(iVertex)
      p(3)=zVertex(iVertex)

      !Vertices of the triangle (cell nodes)
      p1(1)=xCell(cellsOnVertex(1,iVertex))
      p1(2)=yCell(cellsOnVertex(1,iVertex))
      p1(3)=zCell(cellsOnVertex(1,iVertex))
      p2(1)=xCell(cellsOnVertex(2,iVertex))
      p2(2)=yCell(cellsOnVertex(2,iVertex))
      p2(3)=zCell(cellsOnVertex(2,iVertex))
      p3(1)=xCell(cellsOnVertex(3,iVertex))
      p3(2)=yCell(cellsOnVertex(3,iVertex))
      p3(3)=zCell(cellsOnVertex(3,iVertex))

      !Calculate barycentric coordinates
      bar_coefs=bar_coord(p, p1, p2, p3)

     !print*, bary_coord(:)

      return
  end subroutine mpas_barycentric_coefs_vertex!}}}

!***********************************************************************
!
!  routine mpas_barycentric_coefs_edge
!
!> \brief   MPAS Calculate barycentric coordinate coefficients for
!>           scalar interpolation to edge midpoints
!> \author  Pedro Peixoto
!> \date    Nov 2015
!> \details
!>  Purpose: Compute barycentric coefficients for linear interpolation
!>  Input: grid meta data and an edge index
!>  Output: barycentric coordinates indexed in the same order as
!>          as the vertices of triangle edgeTri that contains the edge midpoint
!>          Also outputs the distance between the edge point and the triangle edge midpoint
!>          (this is a sanity check for HCm x HCt grids) - optional
!-----------------------------------------------------------------------
  subroutine mpas_barycentric_coefs_edge(iEdge, mesh, iVertex, bar_coefs,  edgeDisp) !{{{

      implicit none
      integer :: iEdge                              !< Input: An edge index
      type (mpas_pool_type), intent(in) :: mesh     !< Input: Mesh information
      integer, intent(out) :: iVertex               !< Output: Triangle that contains edge midpoint
      real (kind=RKIND), dimension(1:3), intent(out) :: bar_coefs !< Output: Barycentric coefficients
      real (kind=RKIND), intent(out), optional :: edgeDisp     !< Output: Distance from edge midpoint to triangle edge midpoint

      integer :: i, iCell, iCell1, iCell2, iVertex1, iVertex2
      integer, pointer:: nEdges, nCells, nEdgesSolve, nCellsSolve, nVertices, nVerticesSolve
      integer, dimension(:,:), pointer :: cellsOnEdge, cellsOnVertex, verticesOnEdge
      !integer, dimension(:,:), pointer :: edgesOnCell, edgesOnEdge, edgesOnVertex

      real (kind=RKIND), pointer :: sphere_radius
      real (kind=RKIND), dimension(:), pointer :: xCell, yCell, zCell
      real (kind=RKIND), dimension(:), pointer :: xEdge, yEdge, zEdge
      real (kind=RKIND), dimension(:), pointer :: xVertex, yVertex, zVertex
      real (kind=RKIND), dimension(:), pointer :: dcEdge

      logical, pointer :: on_a_sphere

      real (kind=RKIND) :: xTrEdgeMidpoint, yTrEdgeMidpoint, zTrEdgeMidpoint, ptNorm
      real (kind=RKIND) :: distTrEdgeMidToVertex1, distTrEdgeMidToVertex2
      real (kind=RKIND), dimension(1:3) :: p1, p2, p3, p

      call mpas_pool_get_dimension(mesh, 'nEdges', nEdges)
      call mpas_pool_get_dimension(mesh, 'nEdgesSolve', nEdgesSolve)
      call mpas_pool_get_dimension(mesh, 'nCells', nCells)
      call mpas_pool_get_dimension(mesh, 'nCellsSolve', nCellsSolve)
      call mpas_pool_get_dimension(mesh, 'nVertices', nVertices)
      call mpas_pool_get_dimension(mesh, 'nVerticesSolve', nVerticesSolve)
      call mpas_pool_get_array(mesh, 'xCell', xCell)
      call mpas_pool_get_array(mesh, 'yCell', yCell)
      call mpas_pool_get_array(mesh, 'zCell', zCell)
      call mpas_pool_get_array(mesh, 'xEdge', xEdge)
      call mpas_pool_get_array(mesh, 'yEdge', yEdge)
      call mpas_pool_get_array(mesh, 'zEdge', zEdge)
      call mpas_pool_get_array(mesh, 'xVertex', xVertex)
      call mpas_pool_get_array(mesh, 'yVertex', yVertex)
      call mpas_pool_get_array(mesh, 'zVertex', zVertex)
      call mpas_pool_get_array(mesh, 'dcEdge', dcEdge)
      call mpas_pool_get_array(mesh, 'cellsOnEdge', cellsOnEdge)
      call mpas_pool_get_array(mesh, 'cellsOnVertex', cellsOnVertex)
      call mpas_pool_get_array(mesh, 'verticesOnEdge', verticesOnEdge)
      call mpas_pool_get_config(mesh, 'sphere_radius', sphere_radius)
      call mpas_pool_get_config(mesh, 'on_a_sphere', on_a_sphere)

      bar_coefs=0.0

      iCell1 = cellsOnEdge(1,iEdge)
      iCell2 = cellsOnEdge(2,iEdge)
      iVertex1 = verticesOnEdge(1,iEdge)
      iVertex2 = verticesOnEdge(2,iEdge)
      !print*
      !print*, iEdge, nEdges, nEdgesSolve
      !print*, iCell1, iCell2, nCells, nCellsSolve
      !print*, iVertex1, iVertex2, nVertices, nVerticesSolve

      !Extreme halo case - improper to calculate here
      if(iCell1 > nCells .or. iCell2 > nCells .or. iVertex1 > nVertices .or. iVertex2 > nVertices )then
       iVertex=min(iVertex1, iVertex2)
       return
      end if

      !Calculate midpoint of the triangle edge (the one that intersects the Voronoi cell edges
      ! There is no need to divide by 2 (average) because it is going to be nromalized anyway
      xTrEdgeMidpoint = xCell(iCell1) + xCell(iCell2)
      yTrEdgeMidpoint = yCell(iCell1) + yCell(iCell2)
      zTrEdgeMidpoint = zCell(iCell1) + zCell(iCell2)
      ptNorm= xTrEdgeMidpoint**2 + yTrEdgeMidpoint**2 + zTrEdgeMidpoint**2
      ptNorm=sqrt(ptNorm)
      if (on_a_sphere)then
        xTrEdgeMidpoint=sphere_radius*xTrEdgeMidpoint/ptNorm
        yTrEdgeMidpoint=sphere_radius*yTrEdgeMidpoint/ptNorm
        zTrEdgeMidpoint=sphere_radius*zTrEdgeMidpoint/ptNorm
      end if
      !print*, xTrEdgeMidpoint, yTrEdgeMidpoint, zTrEdgeMidpoint
      !print*, xEdge(iEdge), yEdge(iEdge), zEdge(iEdge)

      if(present(edgeDisp))then
	      !Distance from edge point given in the mesh and the triangle edge midpoint
	      !  (sanity check for HCm or HCt grid)
	      edgeDisp=arc_length(xTrEdgeMidpoint, yTrEdgeMidpoint, zTrEdgeMidpoint, &
	                          xEdge(iEdge),    yEdge(iEdge),    zEdge(iEdge))
	      !Normalize edgeDisp
	      edgeDisp=edgeDisp/dcEdge(iEdge)
      end if
      !Calculate distances to vertices
      distTrEdgeMidToVertex1=arc_length(xTrEdgeMidpoint,  yTrEdgeMidpoint,   zTrEdgeMidpoint, &
                                        xVertex(iVertex1),yVertex(iVertex1), zVertex(iVertex1))

      distTrEdgeMidToVertex2=arc_length(xTrEdgeMidpoint,  yTrEdgeMidpoint,   zTrEdgeMidpoint, &
                                        xVertex(iVertex2),yVertex(iVertex2), zVertex(iVertex2))


      !print*, distTrEdgeMidToVertex1, distTrEdgeMidToVertex2, distTrEdgeMidToVertex1+distTrEdgeMidToVertex2, &
      ! dcEdge(iEdge)

      !Case 1: TrEdgeMid closer to v1 ==> Hx Edge midpoint belongs to triangle v2
      !  v1----------TrEdgeMid--------HxMid-----------------------------v2
      !Case 2: TrEdgeMid closer to v2 ==> Hx Edge midpoint belongs to triangle v1
      !  v1---------------------------HxMid-----TrEdgeMid---------------v2
      if(distTrEdgeMidToVertex1<distTrEdgeMidToVertex2)then
         iVertex=iVertex2
      else
         iVertex=iVertex1
      end if
      !print*, iEdge, iVertex, cellsOnEdge(1,iEdge)
      !print*, "Cells Tri:", cellsOnVertex(:,iVertex)
      !print*, maxval(cellsOnVertex(1:3,iVertex)), maxval(cellsOnVertex(1:3,iVertex)) > nVertices

      !Halo cell problem - return zero coefficients and any vertex
      if(maxval(cellsOnVertex(1:3,iVertex)) > nCells )then
       iVertex=min(iVertex1, iVertex2)
       return
      end if

      !Edge Point to have scalars interpolated to
      p(1)=xEdge(iEdge)
      p(2)=yEdge(iEdge)
      p(3)=zEdge(iEdge)

      !Vertices of the triangle (cell nodes) tha contain the Edge Midpoint
      p1(1)=xCell(cellsOnVertex(1,iVertex))
      p1(2)=yCell(cellsOnVertex(1,iVertex))
      p1(3)=zCell(cellsOnVertex(1,iVertex))
      p2(1)=xCell(cellsOnVertex(2,iVertex))
      p2(2)=yCell(cellsOnVertex(2,iVertex))
      p2(3)=zCell(cellsOnVertex(2,iVertex))
      p3(1)=xCell(cellsOnVertex(3,iVertex))
      p3(2)=yCell(cellsOnVertex(3,iVertex))
      p3(3)=zCell(cellsOnVertex(3,iVertex))

      !Calculate coefficients
      bar_coefs=bar_coord(p, p1, p2, p3)

      !print*, bar_coefs(:)

      return
  end subroutine mpas_barycentric_coefs_edge!}}}

  subroutine proj_vec_sphere(vx, vy, vz, px, py, pz, wx, wy, wz)
    !-----------------------------------------------------------
    !  Projects a vector 'v' on the plane tangent to a sphere
    !   Uses the the tangent plane relative to the unit sphere's
    !   point 'p', in cartesian coords
    !  Output in 'w'
    !-----------------------------------------------------------
    real (kind=RKIND), intent(in) :: vx, vy, vz
    real (kind=RKIND), intent(in) :: px, py, pz
    real (kind=RKIND), intent(out) :: wx, wy, wz
    real (kind=RKIND), dimension(1:3) :: v, p, w

    !Init
    v(1)=vx
    v(2)=vy
    v(3)=vz
    p(1)=px
    p(2)=py
    p(3)=pz
    w=0
    p=p/norm(p)
    !Project to the plane tangent to the sphere
    w(1:3)=v(1:3)-dot_product(v,p)*p(1:3)

    !Output
    wx=w(1)
    wy=w(2)
    wz=w(3)

    return
  end subroutine proj_vec_sphere

  function norm(p)
    !-----------------------------------------
    ! NORM
    ! Calculates the euclidian norm of a vector
    !----------------------------------------------
    real(kind=RKIND), intent(in) :: p(:)
    real(kind=RKIND):: norm

    norm=dot_product( p, p)
    norm=sqrt(norm)

    return
  end function norm

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! FUNCTION ARC_LENGTH
   !
   ! Returns the length of the great circle arc from A=(ax, ay, az) to
   !    B=(bx, by, bz). It is assumed that both A and B lie on the surface of the
   !    same sphere centered at the origin.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   real (kind=RKIND) function arc_length(ax, ay, az, bx, by, bz)

      implicit none

      real (kind=RKIND), intent(in) :: ax, ay, az, bx, by, bz

      real (kind=RKIND) :: r, c
      real (kind=RKIND) :: cx, cy, cz

      cx = bx - ax
      cy = by - ay
      cz = bz - az

!      r = ax*ax + ay*ay + az*az
!      c = cx*cx + cy*cy + cz*cz
!
!      arc_length = sqrt(r) * acos(1.0 - c/(2.0*r))

      r = sqrt(ax*ax + ay*ay + az*az)
      c = sqrt(cx*cx + cy*cy + cz*cz)
!      arc_length = sqrt(r) * 2.0 * asin(c/(2.0*r))
      arc_length = r * 2.0 * asin(c/(2.0*r))

   end function arc_length


 function bar_coord(p, p1, p2, p3)
    !----------------------------------------------------------
    ! BARYCENTRIC COORDINATES given 3 points on the sphere
    !
    ! Given a point in cartesian coords and 3 points (p1, p2, p3)
    ! that form a triangle, and are given in the counter-clockwise
    ! ordering, returns the barycentric coords
    !   of the point.
    !
    !   The barycentric coordinates refer to the planar triangle
    !--------------------------------------------------------------
    !Point inside triangle tr and triangle vertices
    real (RKIND), intent(in), dimension(1:3) :: p
    real (RKIND), intent(in), dimension(1:3) :: p1
    real (RKIND), intent(in), dimension(1:3) :: p2
    real (RKIND), intent(in), dimension(1:3) :: p3

    !Barycentric coordinates
    real (RKIND):: bar_coord(1:3)
    real (RKIND):: b(1:3)

    ! 2* Area of the triangle
    real (RKIND):: area

    b(1)=(det(p,p2,p3))
    b(2)=(det(p,p3,p1))
    b(3)=(det(p,p1,p2))
    area=sum(b(1:3)) !abs(det(p1,p2,p3))

    bar_coord=b/area

    return
  end function bar_coord

   function det(p1, p2, p3)
    !-----------------------------------------------------------------------
    !  DET
    !
    !  Returns the determinant of the matrix made of the 3 points p1, p2, p3
    !   as columns
    !-----------------------------------------------------------------------
    real (RKIND), intent(in) :: p1(1:3)
    real (RKIND), intent(in) :: p2(1:3)
    real (RKIND), intent(in) :: p3(1:3)
    real (RKIND):: det

    det=dot_product(cross_product(p1,p2),p3)

    return
  end function det

  function cross_product(a,b)
    !-----------------------------------------------------------------------
    !  CROSS_PRODUCT
    !
    !  Returns the right-handed vector cross product of two 3-vectors:
    !				C = A x B.
    !-----------------------------------------------------------------------
    implicit none

    real (RKIND), intent(in):: a(1:3)
    real (RKIND), intent(in):: b(1:3)
    real (RKIND):: cross_product(1:3)

    cross_product(1) = a(2)*b(3) - a(3)*b(2)
    cross_product(2) = a(3)*b(1) - a(1)*b(3)
    cross_product(3) = a(1)*b(2) - a(2)*b(1)

    return
  end function cross_product





  !Convert degree to radians
  real function degreesToRadians(degAngle)

  implicit none

  real(kind=RKIND) :: degAngle

    degreesToRadians = degAngle * 2 * pii / 360

  end function degreesToRadians


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       SUBROUTINE CONVERT_XL
!
!       Convert (x, y, z) to a (lat, lon) location on a sphere with
!       radius sqrt(x^2 + y^2 + z^2).
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine convert_xl(x, y, z, lat, lon)

  implicit none

  real (kind=RKIND), intent(in) :: x, y, z
  real (kind=RKIND), intent(out) :: lat, lon

  real (kind=RKIND) :: dl, clat
  real (kind=RKIND) :: eps
  parameter (eps=1.e-10)

  dl = sqrt(x*x + y*y + z*z)
  lat = asin(z/dl)

!       check for being close to either pole

  if (abs(x) > eps) then

     if (abs(y) > eps) then

        lon = atan(abs(y/x))

        if ((x <= 0.) .and. (y >= 0.)) then
     lon = pii-lon
        else if ((x <= 0.) .and. (y < 0.)) then
     lon = lon+pii
        else if ((x >= 0.) .and. (y <= 0.)) then
     lon = 2*pii-lon
        end if

     else     ! we're either on longitude 0 or 180

        if (x > 0) then
     lon = 0.
        else
     lon = pii
        end if

     end if

  else if (abs(y) > eps) then

     if (y > 0) then
        lon = pii/2.
     else
        lon = 3.*pii/2.
     end if

  else      ! we are at a pole

     lon = 0.

  end if

  end subroutine convert_xl


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       SUBROUTINE CONVERT_LX
!
!       Convert (lat,lon) to an (x, y, z) location on a sphere with specified radius.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine convert_lx(x, y, z, radius, lat, lon)

  implicit none

  real (kind=RKIND), intent(in) :: radius, lat, lon
  real (kind=RKIND), intent(out) :: x, y, z

  z = radius * sin(lat)
  x = radius * cos(lon) * cos(lat)
  y = radius * sin(lon) * cos(lat)

  end subroutine convert_lx


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       FUNCTION SPHERE_ANGLE
!
!       Computes the angle between arcs AB and AC, given points A, B, and C
!       Equation numbers w.r.t. http://mathworld.wolfram.com/SphericalTrigonometry.html
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  real function sphere_angle(ax, ay, az, bx, by, bz, cx, cy, cz)

  implicit none

  real (kind=RKIND), intent(in) :: ax, ay, az, bx, by, bz, cx, cy, cz

  real (kind=RKIND) :: a, b, c ! Side lengths of spherical triangle ABC

  real (kind=RKIND) :: ABx, ABy, ABz ! The components of the vector AB
  real (kind=RKIND) :: mAB ! The magnitude of AB
  real (kind=RKIND) :: ACx, ACy, ACz ! The components of the vector AC
  real (kind=RKIND) :: mAC ! The magnitude of AC

  real (kind=RKIND) :: Dx ! The i-components of the cross product AB x AC
  real (kind=RKIND) :: Dy ! The j-components of the cross product AB x AC
  real (kind=RKIND) :: Dz ! The k-components of the cross product AB x AC

  real (kind=RKIND) :: s  ! Semiperimeter of the triangle
  real (kind=RKIND) :: sin_angle

  a = acos(max(min(bx*cx + by*cy + bz*cz,1.0_RKIND),-1.0_RKIND)) ! Eqn. (3)
  b = acos(max(min(ax*cx + ay*cy + az*cz,1.0_RKIND),-1.0_RKIND)) ! Eqn. (2)
  c = acos(max(min(ax*bx + ay*by + az*bz,1.0_RKIND),-1.0_RKIND)) ! Eqn. (1)

  ABx = bx - ax
  ABy = by - ay
  ABz = bz - az

  ACx = cx - ax
  ACy = cy - ay
  ACz = cz - az

  Dx =   (ABy * ACz) - (ABz * ACy)
  Dy = -((ABx * ACz) - (ABz * ACx))
  Dz =   (ABx * ACy) - (ABy * ACx)

  s = 0.5*(a + b + c)
!       sin_angle = sqrt((sin(s-b)*sin(s-c))/(sin(b)*sin(c)))   ! Eqn. (28)
  sin_angle = sqrt(min(1.0_RKIND,max(0.0_RKIND,(sin(s-b)*sin(s-c))/(sin(b)*sin(c))))) ! Eqn. (28)

  if ((Dx*ax + Dy*ay + Dz*az) >= 0.0) then
     sphere_angle =  2.0 * asin(max(min(sin_angle,1.0_RKIND),-1.0_RKIND))
  else
     sphere_angle = -2.0 * asin(max(min(sin_angle,1.0_RKIND),-1.0_RKIND))
  end if

  end function sphere_angle



end module mpas_consistent

