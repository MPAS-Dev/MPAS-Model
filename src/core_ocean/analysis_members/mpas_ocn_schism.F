! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  ocn_schism
!
!> \brief MPAS ocean analysis mode member: SCHISM
!> \author Joseph Zhang
!> \date   Oct 2018
!> \details
!>  MPAS ocean analysis mode member: SCHISM
!>  In order to add a new analysis member, do the following:
!>  1. Copy these to your new analysis member name:
!>     cp mpas_ocn_TEMPLATE.F mpas_ocn_your_new_name.F
!>     cp Registry_TEMPLATE.xml Registry_your_new_name.xml
!>
!>  2. In those two new files, replace the following text:
!>     temPlate, TEM_PLATE, FILL_IN_AUTHOR, FILL_IN_DATE
!>     Typically temPlate uses camel case (variable names), like yourNewName,
!>     while TEM_PLATE uses underscores (subroutine names), like your_new_name.
!>     note: do not replace 'filename_template' in Registry_yourNewName.xml
!>
!>  3. Add a #include line for your registry to
!>     Registry_analysis_members.xml
!>
!>  4. In mpas_ocn_analysis_driver.F, add a use statement for your new analysis member.
!>     In addition, add lines for your analysis member, and replace TEM_PLATE
!>     and temPlate as described in step 2. There should be 5 places that need additions:
!>      - Adding the analysis member name to the analysis member list
!>      - Adding an init if test can subroutine call
!>      - Adding a compute if test can subroutine call
!>      - Adding a restart if test can subroutine call
!>      - Adding a finalize if test can subroutine call
!>
!>  5. In src/core_ocean/analysis_members/Makefile, add your
!>     new analysis member to the list of members. See another analysis member
!>     in that file for an example.
!>     NOTE: If your analysis member depends on other files, add a dependency
!>           line for the member and list them there. See okubo weiss for an example.
!>
!-----------------------------------------------------------------------

module ocn_schism

   !Modules in src/framework/
   use mpas_derived_types
   use mpas_pool_routines !get arrays etc
   use mpas_dmpar !MPI exchange routines
   use mpas_timekeeping
   use mpas_stream_manager

   use ocn_constants
   use ocn_diagnostics_routines

   use schism_msgp, only: parallel_init,parallel_finalize,parallel_abort,myrank,nproc
   use schism_glbl, only: np,np_global

   implicit none
   private
   save

#ifdef SINGLE_PRECISION
   integer, parameter :: MPI_REALKIND = MPI_REAL
   integer, parameter :: MPI_2REALKIND = MPI_2REAL
#else
   integer, parameter :: MPI_REALKIND = MPI_DOUBLE_PRECISION
   integer, parameter :: MPI_2REALKIND = MPI_2DOUBLE_PRECISION
#endif

   integer,save :: iths,ntime,npts_mpas_tot,num_divisions_mpas_bnd
   integer,save,allocatable :: npts_mpas_in_rank(:),iparent_in_schism_rank(:),iparent_in_schism_elem(:)
   real (kind=RKIND),save,allocatable :: lonlat_mpas_pts(:,:)

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

   public :: ocn_init_schism, &
             ocn_compute_schism, &
             ocn_restart_schism, &
             ocn_finalize_schism

   !--------------------------------------------------------------------
   !
   ! Private module variables
   !
   !--------------------------------------------------------------------

!***********************************************************************

contains

!***********************************************************************
!
!  routine ocn_init_schism
!
!> \brief   Initialize MPAS-Ocean analysis member
!> \author  FILL_IN_AUTHOR
!> \date    FILL_IN_DATE
!> \details
!>  This routine conducts all initializations required for the
!>  MPAS-Ocean analysis member.
!
!-----------------------------------------------------------------------

   subroutine ocn_init_schism(domain, err)!{{{

      !-----------------------------------------------------------------
      !
      ! input variables
      !
      !-----------------------------------------------------------------

      !-----------------------------------------------------------------
      !
      ! input/output variables
      !
      !-----------------------------------------------------------------

      type (domain_type), intent(inout) :: domain

      !-----------------------------------------------------------------
      !
      ! output variables
      !
      !-----------------------------------------------------------------

      integer, intent(out) :: err !< Output: error flag

      !-----------------------------------------------------------------
      !
      ! local variables
      !
      !-----------------------------------------------------------------
      type (mpas_pool_type), pointer :: schismAMPool
      type (dm_info) :: dminfo
      type (block_type), pointer :: block
      type (mpas_pool_type), pointer :: statePool
      type (mpas_pool_type), pointer :: meshPool
      type (mpas_pool_type), pointer :: scratchPool
      type (mpas_pool_type), pointer :: diagnosticsPool
      type (mpas_pool_type), pointer :: tracersPool

      ! Here are some example variables which may be needed for your analysis member
      integer, pointer :: nVertLevels, nCellsSolve, nEdgesSolve, nVerticesSolve, num_tracers,nCells,nEdges
      integer :: nProcs,iTracer,i,j, k, iCell, iEdge,m,nd1,nd2,ifl,itmp
      integer, dimension(:), pointer :: maxLevelCell, maxLevelEdgeTop, maxLevelVertexBot,nEdgesOnCell
      integer, dimension(:,:), pointer :: edgesOnCell,boundaryCell,boundaryEdge,edgeSignOnCell,verticesOnEdge, &
     &cellsOnEdge
      integer,allocatable :: ibuff1(:)

      real (kind=RKIND), dimension(:), pointer ::  areaCell, dcEdge, dvEdge, angleEdge,lonCell,latCell, &
     &lonVertex,latVertex
      real (kind=RKIND) :: volume,rad_to_deg,pii,tmp,tmp1
      ! pointers to data in pools required for T/S water mass census
      real (kind=RKIND), dimension(:,:),   pointer :: layerThickness
      real (kind=RKIND),allocatable :: rbuff(:,:)

      call parallel_init(domain % dminfo % comm)
      call schism_init(iths,ntime)

      !Calc bounding box of hgrid.ll, assuming SCHISM grid does not cross dateline
      schism_max_lat=maxval(ylat)
      schism_min_lat=minval(ylat)
      schism_max_lon=maxval(xlon)
      schism_min_lon=minval(xlon)
      if(schism_max_lon<0) schism_max_lon=schism_max_lon+360 !convert to MPAS convention of lon
      if(schism_min_lon<0) schism_min_lon=schism_min_lon+360 

      !Get MPAS geometry info
      num_divisions_mpas_bnd=20

      write(12,*)'_init start...:',RKIND
      pii=3.1415926_RKIND
      rad_to_deg=180.0_RKIND/pii

      dminfo = domain % dminfo
      nProcs = domain % dminfo % nprocs
      if(nProcs/=nproc.or.myrank/=domain % dminfo % my_proc_id) call parallel_abort('ocn_init_schism(0.1)')
      allocate(ibuff1(0:nproc-1),npts_mpas_in_rank(0:nproc-1))
      ibuff1=0 !init as 0 for other ranks

      block => domain % blocklist
      icount=0
      do while (associated(block))
        call mpas_pool_get_subpool(block % structs, 'mesh', meshPool)
        call mpas_pool_get_subpool(block % structs, 'scratch', scratchPool)
        call mpas_pool_get_subpool(block % structs, 'schismAM', schismAMPool)

        ! Here are some example variables which may be needed for your analysis member

        call mpas_pool_get_dimension(block % dimensions, 'nVertLevels', nVertLevels)
        call mpas_pool_get_dimension(block % dimensions, 'nCellsSolve', nCellsSolve)
        call mpas_pool_get_dimension(block % dimensions, 'nEdgesSolve', nEdgesSolve)
        call mpas_pool_get_dimension(block % dimensions, 'nVerticesSolve', nVerticesSolve)
        call mpas_pool_get_dimension(block % dimensions, 'nCells', nCells)
        call mpas_pool_get_dimension(block % dimensions, 'nEdges', nEdges)

        call mpas_pool_get_array(meshPool, 'areaCell', areaCell)
        call mpas_pool_get_array(meshPool, 'dcEdge', dcEdge)
        call mpas_pool_get_array(meshPool, 'dvEdge', dvEdge)
        call mpas_pool_get_array(meshPool, 'maxLevelCell', maxLevelCell)
        call mpas_pool_get_array(meshPool, 'maxLevelEdgeTop', maxLevelEdgeTop)
        call mpas_pool_get_array(meshPool, 'maxLevelVertexBot', maxLevelVertexBot)
        call mpas_pool_get_array(meshPool, 'angleEdge', angleEdge)
        call mpas_pool_get_array(meshPool, 'lonCell', lonCell)
        call mpas_pool_get_array(meshPool, 'latCell', latCell)
        call mpas_pool_get_array(meshPool, 'lonVertex', lonVertex)
        call mpas_pool_get_array(meshPool, 'latVertex', latVertex)
        call mpas_pool_get_array(meshPool, 'nEdgesOnCell', nEdgesOnCell)
        call mpas_pool_get_array(meshPool, 'edgesOnCell', edgesOnCell)
        call mpas_pool_get_array(meshPool, 'boundaryEdge', boundaryEdge)
        call mpas_pool_get_array(meshPool, 'boundaryCell', boundaryCell)
        call mpas_pool_get_array(meshPool, 'edgeSignOnCell', edgeSignOnCell)
        call mpas_pool_get_array(meshPool, 'verticesOnEdge', verticesOnEdge)
        call mpas_pool_get_array(meshPool, 'cellsOnEdge', cellsOnEdge)

        !JZ
        !Edge
        do iEdge=1,nEdgesSolve !resident
          nd1=verticesOnEdge(1,iEdge)
          nd2=verticesOnEdge(2,iEdge)
          !Bnd edges have cell (1,) inside, (2,) outside
          if(cellsOnEdge(1,iEdge)<=0.or.cellsOnEdge(1,iEdge)>nCells.or. &
            &cellsOnEdge(2,iEdge)<=0.or.cellsOnEdge(2,iEdge)>nCells) then !bnd edge
            iCell=cellsOnEdge(1,iEdge)
            if(iCell<=0.or.iCell>nCells) call parallel_abort('ocn_init_schism(1)')
            !Find edge index
            ifl=0
            do m=1,nEdgesOnCell(iCell)
              if(iEdge==edgesOnCell(m,iCell)) then
                ifl=m
                exit
              endif
            enddo !m
            if(ifl==0) call parallel_abort('ocn_init_schism(2)')

            !Calc tangential angle
            tmp=atan2(latVertex(nd2)-latVertex(nd1),lonVertex(nd2)-lonVertex(nd1))*rad_to_deg
            write(12,*)'Bnd side:',cellsOnEdge(1:2,iEdge),edgeSignOnCell(ifl,iCell)
            write(12,*)'Bnd side loc:',real(lonVertex(nd1)*rad_to_deg),real(latVertex(nd1)*rad_to_deg), &
     &real(lonVertex(nd2)*rad_to_deg),real(latVertex(nd2)*rad_to_deg),real(angleEdge(iEdge)*rad_to_deg), &
     &real(tmp+90),real(tmp-90)

            !Count pts
            if((lonVertex(nd1)-schism_max_lon)*(lonVertex(nd1)-schism_min_lon)<=0.and. &
              &(lonVertex(nd2)-schism_max_lon)*(lonVertex(nd2)-schism_min_lon)<=0.and. &      
              &(latVertex(nd1)-schism_max_lat)*(latVertex(nd1)-schism_min_lat)<=0.and. &
              &(latVertex(nd2)-schism_max_lat)*(latVertex(nd2)-schism_min_lat)<=0) then
              icount=icount+1
              ibuff1(myrank)=ibuff1(myrank)+num_divisions_mpas_bnd+1
            endif
          endif !bnd edge
        enddo !iEdge

        block => block % next
      end do !while

      call mpi_allreduce(ibuff1,npts_mpas_in_rank,nproc,MPI_INTEGER,MPI_SUM,domain % dminfo % comm,itmp)
      npts_mpas_tot=sum(npts_mpas_in_rank) !can have redundant pts
      write(12,*)'npts_mpas_tot=',npts_mpas_tot
      if(npts_mpas_tot<=0) call parallel_abort('ocn_init_schism: npts_mpas_tot<=0')
      allocate(lonlat_mpas_pts(2,npts_mpas_tot),rbuff(2,npts_mpas_tot))
      allocate(ibuff2(2,npts_mpas_tot),ibuff3(2,npts_mpas_tot),ibuff4(2,npts_mpas_tot))
      rbuff=0 !init for other ranks
      ibuff2=-1; ibuff3=-1; ibuff4=-1 !init

      !Redo to generate list of pts
      icount0=sum(npts_mpas_in_rank(0:myrank-1)) !starting index in the global list rbuff/lonlat_mpas_pts
      icount=icount0
      iblock=0
      block => domain % blocklist
      do while (associated(block))
        iblock=iblock+1
        call mpas_pool_get_subpool(block % structs, 'mesh', meshPool)
        call mpas_pool_get_dimension(block % dimensions, 'nEdgesSolve', nEdgesSolve)
        call mpas_pool_get_dimension(block % dimensions, 'nEdges', nEdges)
        call mpas_pool_get_array(meshPool, 'lonVertex', lonVertex)
        call mpas_pool_get_array(meshPool, 'latVertex', latVertex)
        call mpas_pool_get_array(meshPool, 'nEdgesOnCell', nEdgesOnCell)
        call mpas_pool_get_array(meshPool, 'edgesOnCell', edgesOnCell)
        call mpas_pool_get_array(meshPool, 'verticesOnEdge', verticesOnEdge)
        call mpas_pool_get_array(meshPool, 'cellsOnEdge', cellsOnEdge)

        do iEdge=1,nEdgesSolve !resident
          nd1=verticesOnEdge(1,iEdge)
          nd2=verticesOnEdge(2,iEdge)
          !Bnd edges have cell (1,) inside, (2,) outside
          if(cellsOnEdge(1,iEdge)<=0.or.cellsOnEdge(1,iEdge)>nCells.or. &
            &cellsOnEdge(2,iEdge)<=0.or.cellsOnEdge(2,iEdge)>nCells) then !bnd edge
            iCell=cellsOnEdge(1,iEdge)
            !if(iCell<=0.or.iCell>nCells) call parallel_abort('ocn_init_schism(1)')

            !Count pts
            if((lonVertex(nd1)-schism_max_lon)*(lonVertex(nd1)-schism_min_lon)<=0.and. &
              &(lonVertex(nd2)-schism_max_lon)*(lonVertex(nd2)-schism_min_lon)<=0.and. &      
              &(latVertex(nd1)-schism_max_lat)*(latVertex(nd1)-schism_min_lat)<=0.and. &
              &(latVertex(nd2)-schism_max_lat)*(latVertex(nd2)-schism_min_lat)<=0) then
              do k=1,num_divisions_mpas_bnd+1
                icount=icount+1
                if(icount>npts_mpas_tot) call parallel_abort('ocn_init_schism(1.5)')
                tmp=dble(k-1)/num_divisions_mpas_bnd
                rbuff(1,icount)=lonVertex(nd1)*(1-tmp)+lonVertex(nd2)*tmp
                rbuff(2,icount)=latVertex(nd1)*(1-tmp)+latVertex(nd2)*tmp
                !Save rank, block etc info
                ibuff2(1,icount)=myrank
                ibuff2(2,icount)=iblock
                ibuff3(1,icount)=myrank
                ibuff3(2,icount)=iCell
                ibuff4(1,icount)=myrank
                ibuff4(2,icount)=iEdge
              enddo !k
            endif
          endif !bnd edge
        enddo !iEdge

        block => block % next
      end do !while

      if(icount-icount0/=ibuff1(myrank)) call parallel_abort('ocn_init_schism(1.7)')
      !Build global list
      call mpi_allreduce(rbuff,lonlat_mpas_pts,npts_mpas_tot*2,MPI_REALKIND,MPI_SUM,domain % dminfo % comm,itmp)
      !Use MPI_MAXLOC to get a unique rank etc
      call mpi_allreduce(ibuff2,mpas_rank_block0,npts_mpas_tot*2,MPI_2INTEGER,MPI_MAXLOC,domain % dminfo % comm,itmp)
      call mpi_allreduce(ibuff3,mpas_rank_cell,npts_mpas_tot*2,MPI_2INTEGER,MPI_MAXLOC,domain % dminfo % comm,itmp)
      call mpi_allreduce(ibuff4,mpas_rank_edge,npts_mpas_tot*2,MPI_2INTEGER,MPI_MAXLOC,domain % dminfo % comm,itmp)

      !Find parents in hgrid.ll and reduce the list
      ibuff2=-1 !2: elem ID; 1: rank
      do iCell=1,ne
        do k=1,npts_mpas_tot
          if(ifound(k)==0) then
            ifl=1 !init
            do j=1,i34(iCell)   
             nd1=elnode(j,iCell)
              if(j==i34(iCell)) then
                nd2=elnode(1,iCell)
              else
                nd2=elnode(j+1,iCell)
              endif
              !Error
              tmp1=signa()
              if(tmp1<0) then
                ifl=0
                exit
              endif
            enddo !j
            if(ifl==1) then !parent in this rank
              ibuff2(1,k)=myrank
              ibuff2(2,k)=iCell
            endif !ifl
          endif !ifound
        enddo !k
      enddo !iCell

      !Use MPI_MAXLOC to get unique rank for each pt
      call mpi_allreduce(ibuff2,ibuff3,npts_mpas_tot,MPI_2INTERGER,MPI_MAXLOC,domain % dminfo % comm,itmp)

      allocate(iparent_in_schism(2,npts_mpas_tot),lonlat_edges_of_mpas(2,npts_mpas_tot),bnd_cell_edge_of_mpas(npts_mpas_tot))
      !Build final list
      npts_mpas_final=0
      do k=1,npts_mpas_tot
        if(ibuff3(1,k)<0) cycle
        if(ibuff3(2,k)<=0.or.ibuff3(2,k)>ne) then
          write(12,*)'Fatal MPAS:',ibuff3(2,k),ne
          call parallel_abort('ocn_init_schism: ibuff3<=0')
        endif

        npts_mpas_final=npts_mpas_final+1
        iparent_in_schism(1,npts_mpas_final)=myrank
        iparent_in_schism(2,npts_mpas_final)=ibuff3(2,k) !local elem ID (SCHISM)
        lonlat_edges_of_mpas(1:2,npts_mpas_final)=lonlat_mpas_pts(1:2,k)
        mpas_rank_block(1:2,npts_mpas_final)=mpas_rank_block0(1:2,k) !MPAS rank,block #
        bnd_cell_edge_of_mpas(1,npts_mpas_final)=mpas_rank_cell(2,k) !MPAS elem ID (local)
        bnd_cell_edge_of_mpas(2,npts_mpas_final)=mpas_rank_edge(2,k) !MPAS edge ID (local)

        if(mpas_rank_block(1,npts_mpas_final)<0.or.mpas_rank_block(1,npts_mpas_final)>nproc-1.or. &
         &bnd_cell_edge_of_mpas(1,npts_mpas_final)<=0.or.bnd_cell_edge_of_mpas(2,npts_mpas_final)<=0) then
          write(12,*)'Fatal MPAS:',mpas_rank_block(1:2,npts_mpas_final),bnd_cell_edge_of_mpas(1:2,npts_mpas_final)
          call parallel_abort('ocn_init_schism: final list wrong(1)')
        endif

        !Remember to change lon to SCHISM convention
      enddo !k

      !For example in sedn/rev search 'nProc' in  mpas_ocn_particle_list.F
      !call mpas_dmpar_exch_halo_field(normalVelocity) !seems to work for vertex, cell etc

      deallocate(ibuff1,rbuff)
      err = 0

   end subroutine ocn_init_schism!}}}

!***********************************************************************
!
!  routine ocn_compute_schism
!
!> \brief   Compute MPAS-Ocean analysis member
!> \author  FILL_IN_AUTHOR
!> \date    FILL_IN_DATE
!> \details
!>  This routine conducts all computation required for this
!>  MPAS-Ocean analysis member.
!
!-----------------------------------------------------------------------

   subroutine ocn_compute_schism(domain, timeLevel, err)!{{{

      !-----------------------------------------------------------------
      !
      ! input variables
      !
      !-----------------------------------------------------------------

      integer, intent(in) :: timeLevel

      !-----------------------------------------------------------------
      !
      ! input/output variables
      !
      !-----------------------------------------------------------------

      type (domain_type), intent(inout) :: domain

      !-----------------------------------------------------------------
      !
      ! output variables
      !
      !-----------------------------------------------------------------

      integer, intent(out) :: err !< Output: error flag

      !-----------------------------------------------------------------
      !
      ! local variables
      !
      !-----------------------------------------------------------------

      type (mpas_pool_type), pointer :: schismAMPool
      type (dm_info) :: dminfo
      type (block_type), pointer :: block
      type (mpas_pool_type), pointer :: statePool
      type (mpas_pool_type), pointer :: meshPool
      type (mpas_pool_type), pointer :: scratchPool
      type (mpas_pool_type), pointer :: diagnosticsPool
      type (mpas_pool_type), pointer :: tracersPool

      ! Here are some example variables which may be needed for your analysis member
      integer, pointer :: nVertLevels, nCellsSolve, nEdgesSolve, nVerticesSolve, num_tracers,nCells,nEdges
      integer :: iTracer, k, iCell, iEdge,m,nd1,nd2,ifl
      integer, dimension(:), pointer :: maxLevelCell, maxLevelEdgeTop, maxLevelVertexBot,nEdgesOnCell
      integer, dimension(:,:), pointer :: edgesOnCell,boundaryCell,boundaryEdge,edgeSignOnCell,verticesOnEdge, &
     &cellsOnEdge
      integer, pointer :: index_temperature, index_salinity

      real (kind=RKIND), dimension(:), pointer ::  areaCell, dcEdge, dvEdge, angleEdge,lonCell,latCell, &
     &lonVertex,latVertex
      real (kind=RKIND) :: temperature, salinity,zPosition, volume,rad_to_deg,pii,tmp,tmp1
      ! pointers to data in pools required for T/S water mass census
      real (kind=RKIND), dimension(:,:),   pointer :: layerThickness,normalVelocity,tangentialVelocity
!      real (kind=RKIND), dimension(:,:),   pointer :: potentialDensity
      real (kind=RKIND), dimension(:,:),   pointer :: zMid
      real (kind=RKIND), dimension(:,:,:), pointer :: activeTracers


      integer :: it

      !JZ
      pii=3.1415926_RKIND
      rad_to_deg=180.0_RKIND/pii
      err = 0

      dminfo = domain % dminfo

      block => domain % blocklist
      do while (associated(block))
         call mpas_pool_get_subpool(block % structs, 'state', statePool)
         call mpas_pool_get_subpool(block % structs, 'mesh', meshPool)
         call mpas_pool_get_subpool(block % structs, 'scratch', scratchPool)
         call mpas_pool_get_subpool(block % structs, 'diagnostics', diagnosticsPool)
         call mpas_pool_get_subpool(block % structs, 'schismAM', schismAMPool)

         ! Here are some example variables which may be needed for your analysis member
         call mpas_pool_get_dimension(statePool, 'num_tracers', num_tracers)
         !JZ
         call mpas_pool_get_subpool(statePool, 'tracers', tracersPool)

         call mpas_pool_get_dimension(block % dimensions, 'nVertLevels', nVertLevels)
         call mpas_pool_get_dimension(block % dimensions, 'nCellsSolve', nCellsSolve)
         call mpas_pool_get_dimension(block % dimensions, 'nEdgesSolve', nEdgesSolve)
         call mpas_pool_get_dimension(block % dimensions, 'nVerticesSolve', nVerticesSolve)
         call mpas_pool_get_dimension(block % dimensions, 'nCells', nCells)
         call mpas_pool_get_dimension(block % dimensions, 'nEdges', nEdges)

         call mpas_pool_get_array(meshPool, 'areaCell', areaCell)
         call mpas_pool_get_array(meshPool, 'dcEdge', dcEdge)
         call mpas_pool_get_array(meshPool, 'dvEdge', dvEdge)
         call mpas_pool_get_array(meshPool, 'maxLevelCell', maxLevelCell)
         call mpas_pool_get_array(meshPool, 'maxLevelEdgeTop', maxLevelEdgeTop)
         call mpas_pool_get_array(meshPool, 'maxLevelVertexBot', maxLevelVertexBot)
         call mpas_pool_get_array(meshPool, 'angleEdge', angleEdge)
         call mpas_pool_get_array(meshPool, 'lonCell', lonCell)
         call mpas_pool_get_array(meshPool, 'latCell', latCell)
         call mpas_pool_get_array(meshPool, 'lonVertex', lonVertex)
         call mpas_pool_get_array(meshPool, 'latVertex', latVertex)
         call mpas_pool_get_array(meshPool, 'nEdgesOnCell', nEdgesOnCell)
         call mpas_pool_get_array(meshPool, 'edgesOnCell', edgesOnCell)
         call mpas_pool_get_array(meshPool, 'boundaryEdge', boundaryEdge)
         call mpas_pool_get_array(meshPool, 'boundaryCell', boundaryCell)
         call mpas_pool_get_array(meshPool, 'edgeSignOnCell', edgeSignOnCell)
         call mpas_pool_get_array(meshPool, 'verticesOnEdge', verticesOnEdge)
         call mpas_pool_get_array(meshPool, 'cellsOnEdge', cellsOnEdge)

         ! get indices for T and S
         !JZ
         call mpas_pool_get_dimension(tracersPool, 'index_temperature', index_temperature)
         call mpas_pool_get_dimension(tracersPool, 'index_salinity', index_salinity)
         call mpas_pool_get_array(tracersPool, 'activeTracers', activeTracers, 1)
         call mpas_pool_get_array(statePool, 'layerThickness', layerThickness, timeLevel)
         call mpas_pool_get_array(statePool, 'normalVelocity', normalVelocity, timeLevel)
         call mpas_pool_get_array(diagnosticsPool, 'zMid', zMid)
         call mpas_pool_get_array(diagnosticsPool, 'tangentialVelocity', tangentialVelocity)

         !JZ
         write(12,*)'_compute start...',associated(activeTracers)
         if ( associated(activeTracers) ) then
           do iCell=1,nCells !Solve
!             write(12,*)'Cell:',domain % dminfo % my_proc_id,iCell,lonCell(iCell)*rad_to_deg,latCell(iCell)*rad_to_deg
             do k=1,maxLevelCell(iCell)
               ! make copies of data for convienence
               temperature = activeTracers(index_temperature,k,iCell)
               salinity = activeTracers(index_salinity,k,iCell)
!               density = potentialDensity(k,iCell)
               zPosition = zMid(k,iCell)
               volume = layerThickness(k,iCell) * areaCell(iCell)

               !Output
!               write(12,*)k,temperature,salinity,zPosition,volume,areaCell(iCell)
             enddo !k

             !Edge
             do k=1,nEdgesOnCell(iCell)
               iEdge=edgesOnCell(k,iCell)      
               nd1=verticesOnEdge(1,iEdge)
               nd2=verticesOnEdge(2,iEdge)
!               write(12,*)'Edge info:',k,iEdge,lonVertex(nd1)*rad_to_deg,latVertex(nd1)*rad_to_deg, &
!     &lonVertex(nd2)*rad_to_deg,latVertex(nd2)*rad_to_deg,edgeSignOnCell(k,iCell)
!               write(12,*)'Edge length:',dvEdge(iEdge),angleEdge(iEdge)*rad_to_deg,edgeSignOnCell(k,iCell)
               do m=1,nVertLevels
!                 write(12,*)'Edge normal,tang vel=',m,normalVelocity(m,iEdge),tangentialVelocity(m,iEdge)
                 !bnd edge seems wrong- Xylar said use (1,iEdge)
                 if(boundaryEdge(m,iEdge)/=0) then
!                   write(12,*)'This is a boundary edge:',m
                 endif
               enddo !m
             enddo !k
           enddo !iCell
         endif !associated

         ! Computations which are functions of nCells, nEdges, or nVertices
         ! must be placed within this block loop
         ! Here are some example loops
         do iCell = 1,nCellsSolve
            do k = 1, maxLevelCell(iCell)
               do iTracer = 1, num_tracers
               ! computations on tracers(iTracer,k, iCell)
               end do
            end do
         end do

         block => block % next
      end do !while

      ! mpi gather/scatter calls may be placed here.
      ! Here are some examples.  See mpas_ocn_global_stats.F for further details.
!      call mpas_dmpar_sum_real_array(dminfo, nVariables, sumSquares(1:nVariables), reductions(1:nVariables))
!      call mpas_dmpar_min_real_array(dminfo, nMins, mins(1:nMins), reductions(1:nMins))
!      call mpas_dmpar_max_real_array(dminfo, nMaxes, maxes(1:nMaxes), reductions(1:nMaxes))

      ! Even though some variables do not include an index that is decomposed amongst
      ! domain partitions, we assign them within a block loop so that all blocks have the
      ! correct values for writing output.
      block => domain % blocklist
      do while (associated(block))
         call mpas_pool_get_subpool(block % structs, 'schismAM', schismAMPool)

         ! assignment of final schismAM variables could occur here.

         block => block % next
      end do

      !SCHISM
      !JZ
      do it=iths+1,iths+2 !ntime
        call schism_step(it)
      enddo !it
   end subroutine ocn_compute_schism!}}}

!***********************************************************************
!
!  routine ocn_restart_schism
!
!> \brief   Save restart for MPAS-Ocean analysis member
!> \author  FILL_IN_AUTHOR
!> \date    FILL_IN_DATE
!> \details
!>  This routine conducts computation required to save a restart state
!>  for the MPAS-Ocean analysis member.
!
!-----------------------------------------------------------------------

   subroutine ocn_restart_schism(domain, err)!{{{

      !-----------------------------------------------------------------
      !
      ! input variables
      !
      !-----------------------------------------------------------------

      !-----------------------------------------------------------------
      !
      ! input/output variables
      !
      !-----------------------------------------------------------------

      type (domain_type), intent(inout) :: domain

      !-----------------------------------------------------------------
      !
      ! output variables
      !
      !-----------------------------------------------------------------

      integer, intent(out) :: err !< Output: error flag

      !-----------------------------------------------------------------
      !
      ! local variables
      !
      !-----------------------------------------------------------------

      err = 0

   end subroutine ocn_restart_schism!}}}

!***********************************************************************
!
!  routine ocn_finalize_schism
!
!> \brief   Finalize MPAS-Ocean analysis member
!> \author  FILL_IN_AUTHOR
!> \date    FILL_IN_DATE
!> \details
!>  This routine conducts all finalizations required for this
!>  MPAS-Ocean analysis member.
!
!-----------------------------------------------------------------------

   subroutine ocn_finalize_schism(domain, err)!{{{

      !-----------------------------------------------------------------
      !
      ! input variables
      !
      !-----------------------------------------------------------------

      !-----------------------------------------------------------------
      !
      ! input/output variables
      !
      !-----------------------------------------------------------------

      type (domain_type), intent(inout) :: domain

      !-----------------------------------------------------------------
      !
      ! output variables
      !
      !-----------------------------------------------------------------

      integer, intent(out) :: err !< Output: error flag

      !-----------------------------------------------------------------
      !
      ! local variables
      !
      !-----------------------------------------------------------------

      call schism_finalize
      err = 0

   end subroutine ocn_finalize_schism!}}}

end module ocn_schism

! vim: foldmethod=marker
