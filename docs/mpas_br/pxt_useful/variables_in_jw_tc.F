!---------------------------------------------
!Jablo & Will Baroclinic Test case variables
!---------------------------------------------

!PXT-notation J&W2006T.Rep: zonal wind: u_0 (max)
      real (kind=RKIND), parameter :: u0 = 35.0
!PXT-notation J&W2006T.Rep: grid rotation: \alpha
      real (kind=RKIND), parameter :: alpha_grid = 0.  ! no grid rotation

!      real (kind=RKIND), parameter :: omega_e = 7.29212e-05
      real (kind=RKIND) :: omega_e
!PXT-notation J&W2006T.Rep: Temperature:     ?   ,T0 , \Delta T,      \Gamma (LapseRate), \eta_t (Tropopause_level)
      real (kind=RKIND), parameter :: t0b = 250., t0 = 288., delta_t = 4.8e+05, dtdz = 0.005, eta_t = 0.2
!PXT-notation J&W2006T.Rep: Wind Pert: u_p ,                 a*R,            , \phi_c,             \lambda_c
      real (kind=RKIND), parameter :: u_perturbation = 1., pert_radius = 0.1, latitude_pert = 40., longitude_pert = 20.
!PXT-notation J&W2006T.Rep:        ?      
      real (kind=RKIND), parameter :: theta_c = pii/4.0
      real (kind=RKIND), parameter :: lambda_c = 3.0*pii/2.0
      real (kind=RKIND), parameter :: k_x = 9.           ! Normal mode wave number

!PXT-notation MPAS paper : 1/dzw (1/d\zeta) at u points, d\zeta at w points,  weights for linear interp                     
      real (kind=RKIND), dimension(:), pointer :: rdzw, dzu, rdzu, fzm, fzp
!PXT-notation MPAS paper : diagnostic
      real (kind=RKIND), dimension(:), pointer :: surface_pressure
!PXT-notation MPAS paper : geom height, dz/dx, d(zeta)/dz, h_s
      real (kind=RKIND), dimension(:,:), pointer :: zgrid, zxu, zz, hx
!PXT-notation MPAS paper : p, pb=Exener_base
      real (kind=RKIND), dimension(:,:), pointer :: pressure, ppb, pb, rho_zz, rb, rr, tb, rtb, p, pp, dss, t, rt
!PXT-notation MPAS paper : velocity normal to edge, ? , vert vel at cell face, ? ,  tangent velocity
      real (kind=RKIND), dimension(:,:), pointer :: u, ru, w, rw, v
!PXT-notation MPAS paper : dry air density      
      real (kind=RKIND), dimension(:,:), pointer :: rho, theta
!PXT-notation MPAS paper : omega diagnostics contributions ?
      real (kind=RKIND), dimension(:,:,:), pointer :: zb, zb3
!PXT-notation MPAS paper : weights for transport scheme
      real (kind=RKIND), dimension(:,:,:), pointer :: deriv_two

!.. initialization of moisture:
      integer, pointer :: index_qv
      real (kind=RKIND), parameter :: rh_max = 0.40 ! Maximum relative humidity
!      real (kind=RKIND), parameter :: rh_max = 0.70 ! Maximum relative humidity
      real (kind=RKIND), dimension(nVertLevels, nCells) :: qsat, relhum
      real (kind=RKIND), dimension(:,:,:), pointer :: scalars
!.. end initialization of moisture.

      integer :: iCell, iCell1, iCell2 , iEdge, vtx1, vtx2, ivtx, i, k, nz, itr, itrp, cell1, cell2
      integer, pointer :: nz1, nCellsSolve, nEdges, maxEdges, nVertices

      !This is temporary variable here. It just need when calculate tangential velocity v.
      integer :: eoe, j
      integer, dimension(:), pointer :: nEdgesOnEdge, nEdgesOnCell
      integer, dimension(:,:), pointer :: edgesOnEdge, cellsOnEdge, verticesOnEdge, cellsOnCell
      real (kind=RKIND), dimension(:,:), pointer :: weightsOnEdge

      real (kind=RKIND) :: flux, fluxk, lat1, lat2, eta_v, r_pert, u_pert, lat_pert, lon_pert, r

      real (kind=RKIND) :: ptop, p0, phi
      real (kind=RKIND) :: lon_Edge
!PXT-notation J&W2006T.Rep: a  ,  \eta_v, 
      real (kind=RKIND) :: r_earth, etavs, ztemp, zd, zt, dz, gam, delt, str

      real (kind=RKIND) :: es, qvs, xnutr, znut, ptemp 
      integer :: iter

      real (kind=RKIND), dimension(nVertLevels + 1 ) :: hyai, hybi, znu, znw, znwc, znwv, hyam, hybm
      real (kind=RKIND), dimension(nVertLevels + 1 ) :: znuc, znuv, bn, divh, dpn

      real (kind=RKIND), dimension(nVertLevels + 1 ) :: sh, zw, ah
      real (kind=RKIND), dimension(nVertLevels ) :: zu, dzw, rdzwp, rdzwm
      real (kind=RKIND), dimension(nVertLevels ) :: eta, etav, teta, ppi, tt, temperature_1d

      real (kind=RKIND) :: d1, d2, d3, cof1, cof2, psurf
      real (kind=RKIND), pointer :: cf1, cf2, cf3

      !  storage for (lat,z) arrays for zonal velocity calculation

      logical, parameter :: rebalance = .true.
      integer, parameter :: nlat=721
      real (kind=RKIND), dimension(nVertLevels) :: flux_zonal
      real (kind=RKIND), dimension(nVertLevels + 1, nlat) :: zgrid_2d
      real (kind=RKIND), dimension(nVertLevels, nlat) :: u_2d, pp_2d, rho_2d, qv_2d, etavs_2d, zz_2d
      real (kind=RKIND), dimension(nVertLevels, nlat) :: p_2d, pb_2d, ppb_2d, rr_2d, rb_2d, tb_2d, rtb_2d
      real (kind=RKIND), dimension(nVertLevels, nlat-1) :: zx_2d 
      real (kind=RKIND), dimension(nlat) :: lat_2d
      real (kind=RKIND) :: dlat, hx_1d
      real (kind=RKIND) :: z_edge, z_edge3, d2fdx2_cell1, d2fdx2_cell2