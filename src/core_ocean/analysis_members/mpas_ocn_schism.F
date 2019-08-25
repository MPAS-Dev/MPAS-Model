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

#ifdef _MPI
   use mpi
#endif

   !Modules in src/framework/
   use mpas_derived_types
   use mpas_pool_routines !get arrays etc
   use mpas_dmpar !MPI exchange routines
   use mpas_timekeeping
   use mpas_stream_manager

   use ocn_constants
   use ocn_diagnostics_routines

   use schism_msgp, only: parallel_init,parallel_finalize,parallel_abort,myrank,nproc
   use schism_glbl, only: np,np_global,ne,ns,i34,elnode,xlon,ylat,idry_e,ze,kbe,elside, &
           &su2,sv2,tr_el,nvrt,ntracers,dt,errmsg

   implicit none
   private
   save

#ifdef _MPI
#ifdef SINGLE_PRECISION
   integer, parameter :: MPI_REALKIND = MPI_REAL
   integer, parameter :: MPI_2REALKIND = MPI_2REAL
#else
   integer, parameter :: MPI_REALKIND = MPI_DOUBLE_PRECISION
   integer, parameter :: MPI_2REALKIND = MPI_2DOUBLE_PRECISION
#endif
#endif /*_MPI*/

   !Interface arrays. 's2m_': used to force MPAS bnd edges by SCHISM; 'm2s_': used to 
   !force SCHISM near ocean bnd by MPAS
   integer,save :: iths,ntime,s2m_npts_final,num_divisions_mpas_bnd,nsteps_schism_mpas,it_schism_start, &
           &mpas_nvrt
   real (kind=RKIND),save :: schism_max_lat,schism_min_lat,schism_max_lon,schism_min_lon
   integer,save,allocatable :: s2m_schism_rank_elem(:,:),s2m_mpas_rank_block_edge(:,:)
   !s2m_lonlat_edge_pts(1,:) lon in radians in SCHISM convention [-pi,pi)
   real (kind=RKIND),save,allocatable :: s2m_lonlat_edge_pts(:,:) 

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
      integer :: nProcs,iTracer,i,j, k, iCell, iEdge,m,nd1,nd2,ifl,itmp,npts_mpas_tot, &
                &icount,icount0,iblock
      integer, dimension(:), pointer :: maxLevelCell, maxLevelEdgeTop, maxLevelVertexBot,nEdgesOnCell
      integer, dimension(:,:), pointer :: edgesOnCell,boundaryCell,boundaryEdge,edgeSignOnCell,verticesOnEdge, &
     &cellsOnEdge
      integer,allocatable :: npts_mpas_in_rank(:),ibuff1(:),ibuff2(:,:),ibuff3(:,:),mpas_rank_block0(:,:), &
              &mpas_rank_edge(:,:),ifound(:)

      real (kind=RKIND), dimension(:), pointer ::  areaCell, dcEdge, dvEdge, angleEdge,lonCell,latCell, &
     &lonVertex,latVertex
      ! pointers to data in pools required for T/S water mass census
      real (kind=RKIND), dimension(:,:),   pointer :: layerThickness
      real (kind=RKIND),allocatable :: lonlat_mpas_pts(:,:),rbuff(:,:)
      real (kind=RKIND) :: volume,rad_to_deg,pii,tmp,tmp1,tmp2,tmp3,x3,y3

      call parallel_init(domain % dminfo % comm)
      call schism_init(iths,ntime)

      pii=3.1415926_RKIND
      rad_to_deg=180.0_RKIND/pii

      !Put this into param.in 
      num_divisions_mpas_bnd=20
      nsteps_schism_mpas=80

      !Get MPAS geometry info
      dminfo = domain % dminfo
      nProcs = domain % dminfo % nprocs
      if(nProcs/=nproc.or.myrank/=domain % dminfo % my_proc_id) call parallel_abort('ocn_init_schism(0.1)')
      allocate(ibuff1(0:nproc-1),npts_mpas_in_rank(0:nproc-1))
      ibuff1=0 !init as 0 for other ranks

      !Calc bounding box of hgrid.ll, assuming SCHISM grid does not cross dateline
      tmp=maxval(ylat) !radian
      tmp1=minval(ylat)
      tmp2=maxval(xlon)
      tmp3=minval(xlon)

#ifdef _MPI
      call mpi_allreduce(tmp,schism_max_lat,1,MPI_REALKIND,MPI_MAX,domain%dminfo%comm,itmp)
      call mpi_allreduce(tmp1,schism_min_lat,1,MPI_REALKIND,MPI_MIN,domain%dminfo%comm,itmp)
      call mpi_allreduce(tmp2,schism_max_lon,1,MPI_REALKIND,MPI_MAX,domain%dminfo%comm,itmp)
      call mpi_allreduce(tmp3,schism_min_lon,1,MPI_REALKIND,MPI_MIN,domain%dminfo%comm,itmp)
#endif
      write(12,*)'_init start...:',RKIND,schism_max_lon*rad_to_deg,schism_min_lon*rad_to_deg, &
            &schism_max_lat*rad_to_deg,schism_min_lat*rad_to_deg

      if(schism_max_lon<0) schism_max_lon=schism_max_lon+pii*2 !convert to MPAS convention of lon
      if(schism_min_lon<0) schism_min_lon=schism_min_lon+pii*2

      block => domain % blocklist
      do while (associated(block))
        call mpas_pool_get_subpool(block % structs, 'mesh', meshPool)
        call mpas_pool_get_subpool(block % structs, 'scratch', scratchPool)
        call mpas_pool_get_subpool(block % structs, 'schismAM', schismAMPool)

        ! Here are some example variables which may be needed for your analysis member

        call mpas_pool_get_dimension(block % dimensions, 'nVertLevels', nVertLevels)
        mpas_nvrt=nVertLevels
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
            !tmp=atan2(latVertex(nd2)-latVertex(nd1),lonVertex(nd2)-lonVertex(nd1))*rad_to_deg
            !write(12,*)'Bnd side:',cellsOnEdge(1:2,iEdge),edgeSignOnCell(ifl,iCell)
            !write(12,*)'Bnd side loc:',real(lonVertex(nd1)*rad_to_deg),real(latVertex(nd1)*rad_to_deg), &
!     &real(lonVertex(nd2)*rad_to_deg),real(latVertex(nd2)*rad_to_deg),real(angleEdge(iEdge)*rad_to_deg), &
!     &real(tmp+90),real(tmp-90)

            !Count pts
            if(lonVertex(nd1)<=schism_max_lon.and.lonVertex(nd1)>=schism_min_lon.and. &
              &latVertex(nd1)<=schism_max_lat.and.latVertex(nd1)>=schism_min_lat.or. &
              &lonVertex(nd2)<=schism_max_lon.and.lonVertex(nd2)>=schism_min_lon.and. &      
              &latVertex(nd2)<=schism_max_lat.and.latVertex(nd2)>=schism_min_lat) then
              ibuff1(myrank)=ibuff1(myrank)+num_divisions_mpas_bnd+1
            endif
          endif !bnd edge
        enddo !iEdge

        block => block % next
      end do !while

#ifdef _MPI
      call mpi_allreduce(ibuff1,npts_mpas_in_rank,nproc,MPI_INTEGER,MPI_SUM,domain % dminfo % comm,itmp)
#endif
      npts_mpas_tot=sum(npts_mpas_in_rank) !can have redundant pts
      write(12,*)'npts_mpas_tot=',npts_mpas_tot
      if(npts_mpas_tot<=0) call parallel_abort('ocn_init_schism: npts_mpas_tot<=0')
      allocate(lonlat_mpas_pts(2,npts_mpas_tot),rbuff(2,npts_mpas_tot), &
     &ibuff2(2,npts_mpas_tot),ibuff3(2,npts_mpas_tot),mpas_rank_block0(2,npts_mpas_tot), &
     &mpas_rank_edge(2,npts_mpas_tot),ifound(npts_mpas_tot))
      rbuff=0 !init for other ranks
      ibuff2=-1; ibuff3=-1 !init for other ranks

      !Redo to generate list of pts
      icount0=sum(npts_mpas_in_rank(0:myrank-1)) !starting index in the global list rbuff/lonlat_mpas_pts
      icount=icount0
      iblock=0 !block #
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
            if(lonVertex(nd1)<=schism_max_lon.and.lonVertex(nd1)>=schism_min_lon.and. &
              &latVertex(nd1)<=schism_max_lat.and.latVertex(nd1)>=schism_min_lat.or. &
              &lonVertex(nd2)<=schism_max_lon.and.lonVertex(nd2)>=schism_min_lon.and. &
              &latVertex(nd2)<=schism_max_lat.and.latVertex(nd2)>=schism_min_lat) then
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
                ibuff3(2,icount)=iEdge
              enddo !k
            endif
          endif !bnd edge
        enddo !iEdge

        block => block % next
      end do !while

      if(icount-icount0/=ibuff1(myrank)) call parallel_abort('ocn_init_schism(1.7)')
      !Build global list
#ifdef _MPI
      call mpi_allreduce(rbuff,lonlat_mpas_pts,npts_mpas_tot*2,MPI_REALKIND,MPI_SUM,domain % dminfo % comm,itmp)
      !Use MPI_MAXLOC to get a unique rank 
      call mpi_allreduce(ibuff2,mpas_rank_block0,npts_mpas_tot,MPI_2INTEGER,MPI_MAXLOC,domain % dminfo % comm,itmp)
      call mpi_allreduce(ibuff3,mpas_rank_edge,npts_mpas_tot,MPI_2INTEGER,MPI_MAXLOC,domain % dminfo % comm,itmp)
#endif

      !Check rank
      do k=1,npts_mpas_tot
        if(mpas_rank_block0(1,k)/=mpas_rank_edge(1,k)) call parallel_abort('ocn_init_schism(1.8)')
        write(12,*)'List_of_MPAS_edge_raw_ll ',real(lonlat_mpas_pts(1:2,k)*rad_to_deg)
        write(12,*)'MPAS rank/block=',mpas_rank_block0(1:2,k),mpas_rank_edge(2,k)
      enddo !k

      !Find parents in hgrid.ll and reduce the list
      ibuff2=-1 !1: rank; 2: elem ID
      ifound=0
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
              y3=lonlat_mpas_pts(2,k)
              x3=lonlat_mpas_pts(1,k)
              if(x3>pii) x3=x3-pii*2 !SCHISM convention
              tmp1=((xlon(nd1)-x3)*(ylat(nd2)-y3)-(xlon(nd2)-x3)*(ylat(nd1)-y3))/2. !signa()
              if(tmp1<0) then
                ifl=0
                exit
              endif
            enddo !j
            if(ifl==1) then !parent in this rank
              ifound(k)=1
              ibuff2(1,k)=myrank
              ibuff2(2,k)=iCell
            endif !ifl
          endif !ifound
        enddo !k
      enddo !iCell

      !Use MPI_MAXLOC to get unique rank for each pt
#ifdef _MPI
      call mpi_allreduce(ibuff2,ibuff3,npts_mpas_tot,MPI_2INTEGER,MPI_MAXLOC,domain % dminfo % comm,itmp)
#endif

      !Build final list
      allocate(s2m_schism_rank_elem(2,npts_mpas_tot),s2m_lonlat_edge_pts(2,npts_mpas_tot), &
              &s2m_mpas_rank_block_edge(3,npts_mpas_tot))
      s2m_npts_final=0
      do k=1,npts_mpas_tot
        if(ibuff3(1,k)<0) cycle
        if(ibuff3(1,k)>nproc-1.or.ibuff3(2,k)<=0) call parallel_abort('ocn_init_schism: rank over')
!        if(ibuff3(2,k)<=0.or.ibuff3(2,k)>ne) then
!          write(errmsg,*)'Fatal MPAS:',ibuff3(1:2,k),ne
!          call parallel_abort(errmsg)
!        endif

        s2m_npts_final=s2m_npts_final+1
        s2m_schism_rank_elem(1,s2m_npts_final)=ibuff3(1,k) !rank
        s2m_schism_rank_elem(2,s2m_npts_final)=ibuff3(2,k) !local elem ID (SCHISM)
        s2m_lonlat_edge_pts(1:2,s2m_npts_final)=lonlat_mpas_pts(1:2,k) !radians
        s2m_mpas_rank_block_edge(1:2,s2m_npts_final)=mpas_rank_block0(1:2,k) !MPAS rank,block #, edge
        s2m_mpas_rank_block_edge(3,s2m_npts_final)=mpas_rank_edge(2,k) !MPAS edge ID (local)

        if(s2m_mpas_rank_block_edge(1,s2m_npts_final)<0.or.s2m_mpas_rank_block_edge(1,s2m_npts_final)>nproc-1.or. &
         &s2m_mpas_rank_block_edge(3,s2m_npts_final)<=0) then
          write(errmsg,*)'ocn_init_schism, Fatal MPAS:',s2m_mpas_rank_block_edge(1:3,s2m_npts_final)
          call parallel_abort(errmsg)
        endif
      enddo !k

      !Redo grid if there is no overlapping bnd's
      if(s2m_npts_final<=0) call parallel_abort('ocn_init_schism: s2m_npts_final<=0')

      !Change to SCHISM lon convention
      write(12,*)'s2m_npts_final=',s2m_npts_final,mpas_nvrt
      !Final list follows original order 
      do k=1,s2m_npts_final
        write(12,*)'Final_ll ',real(s2m_lonlat_edge_pts(1:2,k)*rad_to_deg)
        write(12,*)'Final rank etc:',s2m_schism_rank_elem(1:2,k),s2m_mpas_rank_block_edge(1:3,k)

        if(s2m_lonlat_edge_pts(1,k)>pii) s2m_lonlat_edge_pts(1,k)=s2m_lonlat_edge_pts(1,k)-2*pii
      enddo !k

      !For example in sedn/rev search 'nProc' in  mpas_ocn_particle_list.F
      !call mpas_dmpar_exch_halo_field(normalVelocity) !seems to work for vertex, cell etc

      !Init SCHISM starting step
      it_schism_start=iths+1

      deallocate(ibuff1,ibuff2,ibuff3,rbuff,mpas_rank_block0,mpas_rank_edge,ifound,npts_mpas_in_rank,lonlat_mpas_pts)
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
      integer :: iTracer, i,j,k,iblock, iCell, iEdge,m,nd1,nd2,ifl,icount,k2,k3,klev,iedge_indx,itmp
      integer, dimension(:), pointer :: maxLevelCell, maxLevelEdgeTop, maxLevelVertexBot,nEdgesOnCell
      integer, dimension(:,:), pointer :: edgesOnCell,boundaryCell,boundaryEdge,edgeSignOnCell,verticesOnEdge, &
     &cellsOnEdge
      integer, pointer :: index_temperature, index_salinity

      real (kind=RKIND), dimension(:), pointer ::  areaCell, dcEdge, dvEdge, angleEdge,lonCell,latCell, &
     &lonVertex,latVertex
      real (kind=RKIND) :: temperature, salinity,zPosition, volume,rad_to_deg,pii,tmp,tmp1,outer_normal,volume2
      ! pointers to data in pools required for T/S water mass census
      real (kind=RKIND), dimension(:,:),   pointer :: layerThickness,tangentialVelocity
!      real (kind=RKIND), dimension(:,:),   pointer :: potentialDensity
      real (kind=RKIND), dimension(:,:),   pointer :: zMid
      !real (kind=RKIND), dimension(:,:,:), pointer :: activeTracers
      type(field3DReal), pointer :: activeTracersField
      type (field2DReal), pointer :: normalVelocity
      real (kind=RKIND), allocatable :: rbuff(:,:,:),s2m_schism_data(:,:,:),rwild(:,:)

      integer :: it

      write(12,*)'_compute start...' !,associated(activeTracers)
      !JZ
      pii=3.1415926_RKIND
      rad_to_deg=180.0_RKIND/pii

      !SCHISM time stepping
      do it=it_schism_start,it_schism_start+nsteps_schism_mpas-1
        call schism_step(it)
      enddo !it
      it_schism_start=it_schism_start+nsteps_schism_mpas

      !Gather SCHISM outputs s2m_schism_data(:,nvrt,s2m_npts_final) (1: zcor; 2:u, 3:v; 4-: tracers)
      allocate(rbuff(3+ntracers,nvrt,s2m_npts_final),s2m_schism_data(3+ntracers,nvrt,s2m_npts_final), &
              &rwild(nvrt,2))
      rbuff=0 !init for other ranks
      do k=1,s2m_npts_final
        if(myrank==s2m_schism_rank_elem(1,k)) then
          i=s2m_schism_rank_elem(2,k) !elem ID
          if(i>ne) call parallel_abort('ocn_compute_schism(1)')
          if(idry_e(i)==1) then
            rbuff(1,:,k)=0 !fill
          else
            rbuff(1,kbe(i):nvrt,k)=ze(kbe(i):nvrt,i)
            rbuff(1,1:kbe(i)-1,k)=ze(kbe(i),i) !fill
          endif
          rwild=0
          do j=1,i34(i)
            k2=elside(j,i)
            if(k2>ns) call parallel_abort('ocn_compute_schism(1.2)')
            rwild(:,1)=rwild(:,1)+su2(:,k2)
            rwild(:,2)=rwild(:,2)+sv2(:,k2)
!            write(12,*)'Side vel:',k,i,j,su2(nvrt,k2),sv2(nvrt,k2)
          enddo !j
          !Could use linear interp
          rbuff(2,:,k)=rwild(:,1)/i34(i)
          rbuff(3,:,k)=rwild(:,2)/i34(i)
          write(12,*)'Side vel:',k,i,rwild(nvrt,:),rbuff(2:3,nvrt,k)

          rbuff(4:3+ntracers,:,k)=tr_el(:,:,i) !no sum
        endif !myrank
      enddo !k

      !List pt only owned by 1 rank
      call mpi_allreduce(rbuff,s2m_schism_data,(3+ntracers)*nvrt*s2m_npts_final,MPI_REALKIND,MPI_SUM,domain%dminfo% comm,itmp)
      !Debug
      do k=1,s2m_npts_final
        write(12,*)'SCHISM reduction:',k,real(s2m_schism_data(1:5,1,k)),real(s2m_schism_data(1:5,nvrt,k))
      enddo !k

!      deallocate(rwild)
!      allocate(rwild(3+ntracers,mpas_nvrt))
      dminfo = domain % dminfo
      iblock=0
      block => domain % blocklist
      do while (associated(block))
         iblock=iblock+1
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
         !call mpas_pool_get_array(tracersPool, 'activeTracers', activeTracers, 1)
         !To exchange halo, must use a type
         call mpas_pool_get_field(tracersPool, 'activeTracers', activeTracersField, timeLevel=timeLevel)
         call mpas_pool_get_array(statePool, 'layerThickness', layerThickness, timeLevel)
         call mpas_pool_get_field(statePool, 'normalVelocity', normalVelocity, timeLevel=timeLevel)
         call mpas_pool_get_array(diagnosticsPool, 'zMid', zMid)
         call mpas_pool_get_array(diagnosticsPool, 'tangentialVelocity', tangentialVelocity)

         !JZ
!         if ( associated(activeTracers) ) then
           do iCell=1,nCellsSolve !resident
!             write(12,*)'Cell:',domain % dminfo % my_proc_id,iCell,lonCell(iCell)*rad_to_deg,latCell(iCell)*rad_to_deg
             do k=1,maxLevelCell(iCell)
               ! make copies of data for convienence
               temperature = activeTracersField%array(index_temperature,k,iCell)
               salinity = activeTracersField%array(index_salinity,k,iCell)
!               density = potentialDensity(k,iCell)
               zPosition = zMid(k,iCell)
               volume = layerThickness(k,iCell) * areaCell(iCell)

               !Output
!               write(12,*)'Cell data:',k,temperature,salinity,zPosition,volume,areaCell(iCell)
             enddo !k
           enddo !iCell

           !Calc SCHISM layers
!           icount=0; k_chunks=0
!           !List in the order of MPAS rank/edge
!           do k=1,s2m_npts_final
!             if(s2m_mpas_rank_block_edge(1,k)/=myrank.or.s2m_mpas_rank_block_edge(2,k)/=iblock) cycle
!
!             if(icount==0) then !init
!               isame_block=iblock; isame_edge=s2m_mpas_rank_block_edge(3,k)
!               k_chunks=k_chunks+1
!               k_chunks_start(k_chunks)=k
!             else if(isame_block/=s2m_mpas_rank_block_edge(2,k).or.isame_edge/=s2m_mpas_rank_block_edge(3,k)) then !new block/edge
!               isame_block=iblock; isame_edge=s2m_mpas_rank_block_edge(3,k)
!               k_chunks=k_chunks+1
!               k_chunks_start(k_chunks)=k
!             endif
!             icount=icount+1
!           enddo !k
!           !Catch up with last chunk
!           k_chunks=k_chunks+1
!           k_chunks_start(k_chunks)=s2m_npts_final+1

           do iEdge=1,nEdgesSolve !resident
             if(cellsOnEdge(2,iEdge)>0.and.cellsOnEdge(2,iEdge)<=nCells) cycle

             !Bnd edge
             nd1=verticesOnEdge(1,iEdge)
             nd2=verticesOnEdge(2,iEdge)

             !Further filtering of irrelevant edges
             if(.not.(lonVertex(nd1)<=schism_max_lon.and.lonVertex(nd1)>=schism_min_lon.and. &
                     &latVertex(nd1)<=schism_max_lat.and.latVertex(nd1)>=schism_min_lat.or. &
                     &lonVertex(nd2)<=schism_max_lon.and.lonVertex(nd2)>=schism_min_lon.and. &
                     &latVertex(nd2)<=schism_max_lat.and.latVertex(nd2)>=schism_min_lat)) cycle

             !Find edge index
             iCell=cellsOnEdge(1,iEdge)
             iedge_indx=0
             do m=1,nEdgesOnCell(iCell)
               if(iEdge==edgesOnCell(m,iCell)) then
                 iedge_indx=m
                 exit
               endif
             enddo !m
             if(iedge_indx==0) call parallel_abort('ocn_compute_schism(2)')

             do k2=1,maxLevelCell(iCell)
               !Average over all legible pts
               icount=0; rbuff(:,1,1)=0
               do k=1,s2m_npts_final
                 if(s2m_mpas_rank_block_edge(1,k)/=myrank.or.s2m_mpas_rank_block_edge(2,k)/=iblock.or. &
                 &iEdge/=s2m_mpas_rank_block_edge(3,k)) cycle

                 icount=icount+1
                 zPosition=zMid(k2,iCell)
                 !Interp vertical
                 if(zPosition>=s2m_schism_data(1,nvrt,k)) then
                   klev=nvrt
                 else if(zPosition<=s2m_schism_data(1,1,k)) then
                   klev=2
                 else
                   klev=0 !init
                   do k3=1,nvrt-1
                     if(zPosition>=s2m_schism_data(1,k3,k).and.zPosition<=s2m_schism_data(1,k3+1,k)) then
                       klev=k3+1; exit
                     endif
                   enddo !k3
                   if(klev==0) then
                     write(errmsg,*)'ocn_compute_schism: klev:',klev,zPosition,s2m_schism_data(1,:,k)
                     call parallel_abort(errmsg)
                   endif
                 endif !zPosition

                 !zcor not used
                 rbuff(:,1,1)=rbuff(:,1,1)+s2m_schism_data(:,klev,k) 
               enddo !k
               if(icount==0) cycle

               rbuff(:,1,1)=rbuff(:,1,1)/icount

               !Debug
               !write(12,*)'Vel,T,S:',icount,iEdge,k2,real(rbuff(2:5,1,1))

               !Outer normal vel
               !Error: I'm not confident the dir is right
               outer_normal=rbuff(2,1,1)*cos(angleEdge(iEdge))+rbuff(3,1,1)*sin(angleEdge(iEdge))
               outer_normal=outer_normal*edgeSignOnCell(iedge_indx,iCell)
               !Error: need to gently nudge or use other approach to inject mom?
!               normalVelocity%array(k2,iEdge)=outer_normal
               !Error: tangential vel?

               if(outer_normal<0) then !Inflow
                 volume=layerThickness(k2,iCell)*areaCell(iCell)
                 !Add scale of the edge
                 volume2=outer_normal*(icount-1)/num_divisions_mpas_bnd* &
     &layerThickness(k2,iCell)*dvEdge(iEdge)*dt*nsteps_schism_mpas
                 if(volume+volume2<=0) call parallel_abort('ocn_compute_schism:volume<=0')

                 !Tracers
                 tmp=activeTracersField%array(index_temperature,k2,iCell)
                 tmp1=activeTracersField%array(index_salinity,k2,iCell)
               
                 activeTracersField%array(index_temperature,k2,iCell)=(volume*tmp+volume2*rbuff(4,1,1))/ &
                        &(volume+volume2) 
                 activeTracersField%array(index_salinity,k2,iCell)=(volume*tmp1+volume2*rbuff(5,1,1))/ &
                        &(volume+volume2)
               endif !Inflow
             enddo !k2
           enddo !iEdge

!         endif !associated

         ! Computations which are functions of nCells, nEdges, or nVertices
         ! must be placed within this block loop
         ! Here are some example loops
!         do iCell = 1,nCellsSolve
!            do k = 1, maxLevelCell(iCell)
!               do iTracer = 1, num_tracers
!               ! computations on tracers(iTracer,k, iCell)
!               end do
!            end do
!         end do

         block => block % next
      end do !while

      ! mpi gather/scatter calls may be placed here.
      ! Here are some examples.  See mpas_ocn_global_stats.F for further details.
!      call mpas_dmpar_sum_real_array(dminfo, nVariables, sumSquares(1:nVariables), reductions(1:nVariables))
!      call mpas_dmpar_min_real_array(dminfo, nMins, mins(1:nMins), reductions(1:nMins))
!      call mpas_dmpar_max_real_array(dminfo, nMaxes, maxes(1:nMaxes), reductions(1:nMaxes))

      call mpas_dmpar_exch_halo_field(normalVelocity)
      call mpas_dmpar_exch_halo_field(activeTracersField)

      ! Even though some variables do not include an index that is decomposed amongst
      ! domain partitions, we assign them within a block loop so that all blocks have the
      ! correct values for writing output.
      block => domain % blocklist
      do while (associated(block))
         call mpas_pool_get_subpool(block % structs, 'schismAM', schismAMPool)

         ! assignment of final schismAM variables could occur here.

         block => block % next
      end do

      deallocate(rbuff,s2m_schism_data,rwild)
      err = 0
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
