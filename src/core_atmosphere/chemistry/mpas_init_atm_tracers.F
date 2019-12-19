!=================================================================================================================
 module mpas_init_atm_tracers
 use mpas_log
 use mpas_kind_types
 use mpas_pool_routines

 implicit none
 private
 public:: init_atm_tracers


 contains


!=================================================================================================================
 subroutine init_atm_tracers(block,mesh,configs,state)
!=================================================================================================================

!input arguments:
 type(mpas_pool_type),intent(in):: configs
 type(mpas_pool_type),intent(in):: mesh

!inout arguments:
 type(block_type),intent(inout),target:: block
 type(mpas_pool_type),intent(inout)   :: state

!local variables and pointers:
 character (len=StrKIND),pointer:: config_start_time

 logical:: lexist

!-----------------------------------------------------------------------------------------------------------------
 call mpas_log_write(' ')
 call mpas_log_write('--- enter subroutine init_atm_tracers:')

!--- lexist is initialized to true (for now):
 lexist = .true.

 if(lexist) then
    call mpas_pool_get_config(configs,'config_start_time',config_start_time)
 
    !--- horizontal interpolation:
    call init_hinterp_tracers(block,mesh,state)

!   !--- vertical interpolation:
!   initial_date = trim(config_start_time)
!   call init_vinterp_gocart(initial_date,mesh,diag,state)
!else
!   call mpas_log_write('QNWFA_QNIFA_Monthly_Sigma.dat was not found in local directory:')
!   call mpas_log_write('nwfa and nifa are set to zero and not interpolated from climatological data.')
 endif

 call mpas_log_write('--- end subroutine init_atm_tracers.')
 call mpas_log_write(' ')
 
 end subroutine init_atm_tracers

!=================================================================================================================
 subroutine init_hinterp_tracers(block,mesh,state)
!=================================================================================================================

!input arguments:
 type (mpas_pool_type),intent(in)  :: mesh

!inout arguments:
 type(block_type),intent(inout),target:: block
 type (mpas_pool_type),intent(inout)  :: state

!local variables and pointers:
 integer,pointer:: nCells,nVertLevels
 integer,pointer:: num_tracers
 integer,pointer:: passive_start,passive_end
 integer,pointer:: index_tr17_7,index_tr17_8

 integer:: iCell,k,n

 real(kind=RKIND),dimension(:,:,:),pointer:: tracers

!-----------------------------------------------------------------------------------------------------------------
 call mpas_log_write('--- enter subroutine init_hinterp_tracers:')

 call mpas_pool_get_dimension(mesh,'nCells',nCells)
 call mpas_pool_get_dimension(mesh,'nVertLevels',nVertLevels)

 call mpas_pool_get_dimension(state,'num_tracers',num_tracers)
 call mpas_pool_get_dimension(state,'passive_start',passive_start)
 call mpas_pool_get_dimension(state,'passive_end',passive_end )
 call mpas_log_write('--- num_tracers   = $i',intArgs=(/num_tracers/))
 call mpas_log_write('--- passive_start = $i',intArgs=(/passive_start/))
 call mpas_log_write('--- passive_end   = $i',intArgs=(/passive_end/))

 call mpas_pool_get_array(state,'tracers',tracers)

!--- initialization:
 do n = passive_start, passive_end
    do k = 1, nVertLevels
       do iCell = 1, nCells
          tracers(n,k,iCell) = 1._RKIND
       enddo
    enddo
 enddo

 call mpas_log_write('--- end subroutine init_hinterp_tracers:')

 end subroutine init_hinterp_tracers

!=================================================================================================================
 end module mpas_init_atm_tracers
!=================================================================================================================
