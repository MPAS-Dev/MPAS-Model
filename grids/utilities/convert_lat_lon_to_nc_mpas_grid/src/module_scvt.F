module scvt

   use data_types
   use sphere_utilities
   use voronoi_utils
   use grid_constants
   use grid_params


   contains


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !  SUBROUTINE SCVT_SOLVE
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine scvt_solve(n, lend, rlat, rlon, nvc, list, lptr, fn)
   
      implicit none

      integer, intent(in) :: n, nvc, fn
      integer, dimension(n), intent(inout) :: lend
      integer, dimension(nvc), intent(inout) :: list, lptr 
      real, dimension(n), intent(inout) :: rlat, rlon

      integer :: maxitr

      integer :: i, j, k, iter
      integer :: ntmax, nrow, nptri
      integer, allocatable, dimension(:) :: listc
      real :: area, density, tot_mass
      real :: x, y, z, new_ctr_x, new_ctr_y, new_ctr_z 
      real, allocatable, dimension(:) :: vclat, vclon
      real, allocatable, dimension(:) :: rlat_2, rlon_2
      type (geo_point) :: p1, p2, p3, pc
      type (geo_point) :: p_n1, p_n2
      type (geo_point), dimension(3,64) :: ptri
      real :: avg_movement, maxmovement, movement
      logical converged

      maxitr = n_scvt_iterations

      maxmovement = 100000


      ntmax = 6*n
      nrow = 6
      nptri = 64
      allocate(listc(nvc))
      allocate(vclat(nvc))
      allocate(vclon(nvc))
      allocate(rlat_2(n))
      allocate(rlon_2(n))

      iter = 1
      converged = .false.

      do while (iter <= maxitr .and. .not.converged)

         write(0,*) 'scvt iteration ',iter

         !
         ! Compute Voronoi corners
         !
         call compute_vc(rlat, rlon, n, nrow, ntmax, list, lptr, lend, listc, vclat, vclon, nvc)
   
         !
         ! Loop over vertices
         !   Within the loop, p0 always refers to the current vertex being processed
         !
!$OMP PARALLEL DO PRIVATE(I, J, K, NEW_CTR_X, NEW_CTR_Y, NEW_CTR_Z, TOT_MASS, P1, P2, P3, PC, AREA, DENSITY, X, Y, Z, PTRI) SHARED(RLAT, RLON, RLAT_2, RLON_2, LPTR, LEND, LISTC, VCLAT, VCLON, NPTRI)
         do i=1,n

            new_ctr_x = 0.0
            new_ctr_y = 0.0
            new_ctr_z = 0.0
            tot_mass = 0.0
   
            !
            ! Compute center of mass of Voronoi cell
            !
            p1%lat = rlat(i)
            p1%lon = rlon(i)
   
            k = lend(i)
            p2%lat = vclat(listc(k))
            p2%lon = vclon(listc(k))
            if (p1%lon - p2%lon > pii)  p2%lon = p2%lon + 2.0*pii
            if (p1%lon - p2%lon < -pii) p2%lon = p2%lon - 2.0*pii
            k = lptr(lend(i))
            p3%lat = vclat(listc(k))
            p3%lon = vclon(listc(k))
            if (p1%lon - p3%lon > pii)  p3%lon = p3%lon + 2.0*pii
            if (p1%lon - p3%lon < -pii) p3%lon = p3%lon - 2.0*pii
   
            call divide_triangle(p1, p2, p3, nptri, ptri)
            do j=1,nptri
               area = triangle_area(ptri(1,j), ptri(2,j), ptri(3,j), 1.0)
               call center_of_mass(ptri(1,j), ptri(2,j), ptri(3,j), pc)
               if (p1%lon - pc%lon > pii)  pc%lon = pc%lon + 2.0*pii
               if (p1%lon - pc%lon < -pii) pc%lon = pc%lon - 2.0*pii
               density = density_for_point(pc)
               tot_mass = tot_mass + area * density

               call convert_lx(x, y, z, 1.0, pc)
               new_ctr_x = new_ctr_x + x*area*density
               new_ctr_y = new_ctr_y + y*area*density
               new_ctr_z = new_ctr_z + z*area*density
            end do
   
            do while (k /= lend(i))
               k = lptr(k)
               p2 = p3
               p3%lat = vclat(listc(k))
               p3%lon = vclon(listc(k))
               if (p1%lon - p3%lon > pii)  p3%lon = p3%lon + 2.0*pii
               if (p1%lon - p3%lon < -pii) p3%lon = p3%lon - 2.0*pii
               if (abs(p2%lat - p3%lat) < 0.00001 .and. abs(p2%lon - p3%lon) < 0.00001) cycle

   
               call divide_triangle(p1, p2, p3, nptri, ptri)
               do j=1,nptri
                  area = triangle_area(ptri(1,j), ptri(2,j), ptri(3,j), 1.0)
                  call center_of_mass(ptri(1,j), ptri(2,j), ptri(3,j), pc)
                  if (p1%lon - pc%lon > pii)  pc%lon = pc%lon + 2.0*pii
                  if (p1%lon - pc%lon < -pii) pc%lon = pc%lon - 2.0*pii
                  density = density_for_point(pc)
                  tot_mass = tot_mass + area * density

                  call convert_lx(x, y, z, 1.0, pc)
                  new_ctr_x = new_ctr_x + x*area*density
                  new_ctr_y = new_ctr_y + y*area*density
                  new_ctr_z = new_ctr_z + z*area*density
               end do
            end do

            new_ctr_x = new_ctr_x / tot_mass
            new_ctr_y = new_ctr_y / tot_mass
            new_ctr_z = new_ctr_z / tot_mass
            call convert_xl(new_ctr_x, new_ctr_y, new_ctr_z, pc)
            rlat_2(i) = pc%lat
            rlon_2(i) = pc%lon

   
         end do
!$OMP END PARALLEL DO

         !Compute movement
         if(mod(iter,100).eq.0) then
           maxmovement = 0.0
           avg_movement = 0.0
           do i = 1,n
            
              p_n1%lat = rlat(i)
              p_n1%lon = rlon(i)
              p_n2%lat = rlat_2(i)
              p_n2%lon = rlon_2(i)
  
              call convert_lx(x,y,z,1.0,p_n1)
              call convert_lx(new_ctr_x, new_ctr_y, new_ctr_z,1.0,p_n2)
  
              !x y z computation
              movement = sqrt((x - new_ctr_x)**2 + (y - new_ctr_y)**2 + (z - new_ctr_z)**2)
  
              if(movement > maxmovement) maxmovement = movement
              avg_movement = avg_movement + movement/n
  
           enddo
           if(avg_movement.lt.eps.and.l2_conv) converged=.true.
           if(avg_movement.lt.eps.and.inf_conv) converged=.true.
           write(6,*) n, iter, maxmovement, avg_movement
         endif

         rlat(:) = rlat_2(:)
         rlon(:) = rlon_2(:)
         iter = iter + 1

         if (mod(iter,restart_interval) == 0) then
            write(0,*) 'Writing restart file'
            open(44,file=restart_dat,form='formatted',status='unknown')
            write(44,*) n
            do i=1,n
               pc % lat = rlat(i)
               pc % lon = rlon(i)
               call convert_lx(x, y, z, 1.0, pc)
               write(44,'(f18.15,1x,f18.15,1x,f18.15)') x, y, z
            end do
            close(44)
         end if
    
      end do

      deallocate(listc)
      deallocate(vclat)
      deallocate(vclon)
      deallocate(rlat_2)
      deallocate(rlon_2)

      if (maxitr > 0) write(0,*) 'Finished SCVT solve'

   end subroutine scvt_solve


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !  SUBROUTINE RANDOM_POINT
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine random_point(p)

      type (geo_point), intent(inout) :: p
      real :: x, y, z, m
      real :: pi

      pi = 4.0*atan(1.0)

      x = 0.0
      y = 0.0
      z = 0.0
      m = 2.0

      do while (m > 1.0 .or. (x == 0.0 .and. y == 0.0 .and. z == 0.0))
         call random_number(x)
         call random_number(y)
         call random_number(z)
         x = x * 2.0 - 1.0
         y = y * 2.0 - 1.0
         z = z * 2.0 - 1.0
         m = x**2 + y**2 + z**2
      end do

      m = 1.0 / sqrt(m)
      x = x * m
      y = y * m
      z = z * m

      call convert_xl(x, y, z, p)

   end subroutine random_point


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !  FUNCTION DENSITY_FOR_POINT
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   real function density_for_point(p)

      implicit none

      type (geo_point), intent(in) :: p

      character (len=256) :: fname
      real :: rx, ry, rz, prx, pry, prz
      type (geo_point) :: p_local
      real :: hgt
      real :: r, norm, t_cent
      real :: r1
      real :: pi
      real :: width, trans_center, min_val

      pi = 4.0*atan(1.0)

      !density_for_point = 1.0 + (1.19*cos(p%lat-3.141592654/4.0))**16.0

      ! Uniform Density Function
      density_for_point = 1.0

      !Target Density Function based on hyperbolic tangent
      !		p_local%lat = latitude (radians) center of high-resolution region
      !         p_local%lon = longitude (radians) center of high-resolution region
      !         width = width of transition zone 
      !         trans_center = width (radians) of high resolution zone
      !         minval = minimum density value. to have grid spacing vary by a factor of 8
      !                  set minval = (1.0 / 8.0)**4

  !   p_local%lat = pii/4.0
  !   p_local%lon = 1.25*pii
  !   call convert_lx(rx, ry, rz, 1.0, p)
  !   call convert_lx(prx, pry, prz, 1.0, p_local)
  !   r = acos(rx*prx + ry*pry + rz*prz)

  !   width = 0.15
  !   trans_center = pi/6.0
  !   min_val = (1.0/8.0)**4
  !   norm = 1.0/(1.0-min_val)
  !   density_for_point = ((tanh((trans_center-r)*(1.0/width))+1.0)/2)/norm + min_val

   end function density_for_point

end module scvt
