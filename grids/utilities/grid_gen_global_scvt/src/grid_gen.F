program grid_gen
!--------------------------------------------------------------------------------
!GRID_GEN README
!Last modified Oct 2015 - Pedro Peixoto
!--------------------------------------------------------------------------------
!
!The grid_gen utility converts a set of generating points into a complete MPAS
!input file, grid.nc. Additionally, grid_gen creates a graph description file
!suitable for use with METIS, plots of the initial Delaunay triangulation and
!final Delaunay triangulation (after any further adjustment by grid_gen), and
!a list of the final generating points.
!--------------------------------------------------------------------------------
!
!  Modif Oct 2015 - Peixoto:
!      -
!
!--------------------------------------------------------------------------------

   use grid_params
   use grid_constants
   use data_types
   use grid_gen_utils
   use voronoi_utils
   use stripack
   use scvt
   use grid_meta

   implicit none

   real :: dlat, dlon
   real :: dl
   real :: d1, d2, d3, d4
   integer :: p1, p2, p3
   integer :: if
   !character (len=80) :: frame_name

   real :: pi
   real :: area_per_sample, nhexs, sum_nhexs, hex_area
   type (geo_point) :: p

   integer :: i, j, k, nb, ier

   real, allocatable, dimension(:)       :: rlat, rlon, vclat, vclon, x, y, z, xc, yc, zc
   integer, allocatable, dimension(:)    :: list, lptr, listc, lend
   integer, allocatable, dimension(:,:)  :: ltri

   integer :: n, nrow, ntmax, nvc
   integer :: tr1, tr2, tr3, tr4
   integer :: i1, i2, i3, k1, k2

   type (adjacency_list) :: alist, clist

   real :: lat1, lon1, lat2, lon2, lat3, lon3, latc, lonc


   call read_namelist()
   write(0,*) ""
   open(22,file=locs_dat, status='old')
   if (locs_as_xyz) then !xyz file - has the number of nodes on first line
      read(22,*) n
      if (n/=np)then
      	write(0,*) "Number of points in namelist does not match grid file (file, nml)", n, np
      	write(0,*) "Using the number of nodes from grid file", n
      end if
      np=n
   end if


   pi = 4.0*atan(1.0)

   if (min_dx > 0.0) then
      area_per_sample = 4.0 * pi * 6370000**2.0 / 6000000.0
      sum_nhexs = 0.0
      write(0,'(a,f10.1)') 'Computing an estimate for the required number of cells to reach dx=', min_dx
      do if = 1,5
         nhexs = 0.0
         do i=1,6000000
            call random_point(p)
            d1 = density_for_point(p)
            dl = min_dx / (d1 ** 0.25)
            hex_area = sqrt(3.0) / 2.0 * dl**2.0
            nhexs = nhexs + area_per_sample / hex_area
         end do
         sum_nhexs = sum_nhexs + nhexs
         write(0,'(a,i3,a)',advance='no') ' ...',if*20,'%'
      end do
      write(0,*) ' '
      write(0,*) 'Estimated # hexs:', nint(sum_nhexs/5.0)
      write(0,*) ' '
      stop
   end if


   n = np

   ntmax = 6*n
   nvc = ntmax
   nrow = 6
   allocate(rlat(n))
   allocate(rlon(n))
   allocate(x(n))
   allocate(y(n))
   allocate(z(n))
   allocate(ltri(nrow,ntmax))
   allocate(list(nvc))
   allocate(lptr(nvc))
   allocate(lend(n))
   allocate(listc(nvc))
   allocate(vclat(nvc))
   allocate(vclon(nvc))
   allocate(xc(nvc))
   allocate(yc(nvc))
   allocate(zc(nvc))


   write(0,*) 'Reading generating points from a file'
   !open(22,file=locs_dat,form='formatted',status='old')
   if (locs_as_xyz) then !xyz file - has ne number of nodes on first line
      !read(22,*)
      do i=1,n
         read(22,*) x(i), y(i), z(i)
      end do
      call trans_inv(x, y, z, rlat, rlon, n)
   else  !Lat lon grid file
      do i=1,n
         read(22,'(f13.10,1x,f13.10)') rlat(i), rlon(i)
      end do
   end if
   close(22)


   !
   ! Compute Voronoi corners
   !
   write(0,*) ' '
   write(0,*) 'Computing Voronoi corners'
   call compute_vc(rlat, rlon, n, nrow, ntmax, list, lptr, lend, listc, vclat, vclon, nvc)


   !
   ! Form SCVT
   !
   call TRANS (n, rlat, rlon, x, y, z)
   call TRANS (nvc, vclat, vclon, xc, yc, zc)

   ! Plot ps file with initial grid
   !write(frame_name,'(a)') 'scvt_initial.ps'
   open(32,file=plotin,form='formatted',status='unknown')
   call vrplot(32, 8.0, 0.0, 0.0, 90.0 ,N, X,Y,Z, 2*n-4,LISTC,LPTR,LEND,XC,YC,ZC,'(spherical centroidal voronoi tessellation)',.false.,IER)
   close(32)

   !Optimize with SCVT
   if(n_scvt_iterations>0)then
   	  call scvt_solve(n, lend, rlat, rlon, nvc, list, lptr, if)
   end if

   call compute_vc(rlat, rlon, n, nrow, ntmax, list, lptr, lend, listc, vclat, vclon, nvc)
   call TRANS (n, rlat, rlon, x, y, z)
   call TRANS (nvc, vclat, vclon, xc, yc, zc)

   ! Plot ps file with final grid grid
   !write(frame_name,'(a)') 'scvt_final.ps'
   open(32,file=plotout,form='formatted',status='unknown')
   call vrplot(32, 8.0, 0.0, 0.0, 90.0 ,N, X,Y,Z, 2*n-4,LISTC,LPTR,LEND,XC,YC,ZC,'(spherical centroidal voronoi tessellation)',.false.,IER)
   close(32)


   write(0,*) ' '
   write(0,*) 'Deriving grid metadata and writing output'
   call convert_adjacency_list(n, lend, nvc, list, lptr, alist)
   call convert_corner_list(n, lend, nvc, listc, lptr, clist)
   call write_grid(rlat, rlon, n, vclat, vclon, nvc, alist, clist)


   deallocate(rlat)
   deallocate(x)
   deallocate(y)
   deallocate(z)
   deallocate(rlon)
   deallocate(ltri)
   deallocate(list)
   deallocate(lptr)
   deallocate(lend)
   deallocate(listc)
   deallocate(vclat)
   deallocate(vclon)
   deallocate(xc)
   deallocate(yc)
   deallocate(zc)

end program grid_gen
