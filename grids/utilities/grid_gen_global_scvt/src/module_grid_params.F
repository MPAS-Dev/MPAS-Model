module grid_params

   integer :: np
   logical :: locs_as_xyz
   logical :: l2_conv, inf_conv
   integer :: n_scvt_iterations
   integer :: restart_interval
   real :: eps
   real :: min_dx
   character (len=128) :: work_dir, locs_dat, locs_dat_out, graph_info, grid_nc, plotin, plotout, restart_dat

   contains

   subroutine read_namelist()

      implicit none

      integer :: funit, stat
      real :: pi

      namelist /domains/ np, locs_as_xyz, n_scvt_iterations, restart_interval, eps, l2_conv, inf_conv, min_dx
      namelist /files/ work_dir, locs_dat, locs_dat_out, graph_info, grid_nc, plotin, plotout, restart_dat

      pi = 4.0*atan(1.0)

      funit = 21 

      np = 40962
      locs_as_xyz = .true.
      n_scvt_iterations = 0
      restart_interval = 0
      eps = 0.0
      l2_conv = .true.
      inf_conv = .false.
      min_dx = 0.0
	   work_dir=""
	   locs_dat="locs.dat"
	   locs_dat_out="locs.dat.out"
      graph_info="graph.info"
      grid_nc="grid.nc"
      plotin="scvt_initial.ps"
      plotout="scvt_final.ps"
      restart_dat="restart.dat"

      open(funit,file='namelist.input',status='old',form='formatted')
      read(funit,domains)
      read(funit,files)
      close(funit)

	   locs_dat=trim(work_dir)//trim(locs_dat)
	   locs_dat_out=trim(work_dir)//trim(locs_dat_out)
      graph_info=trim(work_dir)//trim(graph_info)
      grid_nc=trim(work_dir)//trim(grid_nc)
      plotin=trim(work_dir)//trim(plotin)
      plotout=trim(work_dir)//trim(plotout)
      restart_dat=trim(work_dir)//trim(restart_dat)

      if (l2_conv) then
          inf_conv = .false.
      endif

   end subroutine read_namelist

end module grid_params
