!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 module SEDIMENT_mod

! !MODULE: SEDIMENT_mod
! !author Zhendong Cao, Mathew Maltrud
! !date 02/04/2021
!
! !DESCRIPTION:
!
 !------------------------------------------------------------------------------
 !
 !------------------------------------------------------------------------------

! !INPUT PARAMETERS:
  !-----------------------------------------------------------------------------
  !   include tracegas and ecosystem parameters
  !   all variables from these modules have a parm_ prefix
  !-----------------------------------------------------------------------------
   use mpas_log
   use SEDIMENT_parms

   implicit none
   save
   private

!-----------------------------------------------------------------------
!  public/private declarations
!-----------------------------------------------------------------------

   public :: &
      SEDIMENT_tracer_cnt,     &
      SEDIMENT_init,           &
      SEDIMENT_SurfaceFluxes,  &
      SEDIMENT_SourceSink

!-----------------------------------------------------------------------
!  module variables
!-----------------------------------------------------------------------

   integer (SEDIMENT_i4), parameter :: &
      SEDIMENT_tracer_cnt = 99

!*****************************************************************************

contains

!*****************************************************************************
! !IROUTINE: SEDIMENT_init
! !INTERFACE:

 subroutine SEDIMENT_init(SEDIMENT_indices)

! !DESCRIPTION:
!  Initialize tracegas tracer module. This involves setting metadata, reading
!  the module namelist, setting initial conditions, setting up forcing,
!  and defining additional tavg variables.
!
! !REVISION HISTORY:
!  same as module

! !INPUT PARAMETERS:

  type(SEDIMENT_indices_type), intent(inout) :: SEDIMENT_indices

! !INPUT/OUTPUT PARAMETERS:

! !OUTPUT PARAMETERS:

!-----------------------------------------------------------------------
!  local variables
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

   SEDIMENT_indices%short_name(SEDIMENT_indices%sedConcen_ind) = 'sedConcen'
   SEDIMENT_indices%long_name(SEDIMENT_indices%sedConcen_ind)  = ' (suspended) sediment Concentration'

   SEDIMENT_indices%units(:)                       = 'kg/m^3'
!-----------------------------------------------------------------------

 end subroutine SEDIMENT_init

!***********************************************************************
!***********************************************************************
! !IROUTINE: SEDIMENT_SourceSink
! !INTERFACE:

 subroutine SEDIMENT_SourceSink(SEDIMENT_indices, SEDIMENT_input, SEDIMENT_forcing, &
                                SEDIMENT_output, SEDIMENT_diagnostic_fields, &
                                numLevelsMax, numColumnsMax, numColumns)

! !DESCRIPTION:
!  Compute time derivatives for tracegas state variables
!
! !REVISION HISTORY:
!  same as module

! !INPUT PARAMETERS:

  type(SEDIMENT_indices_type),     intent(in ) :: SEDIMENT_indices
  type(SEDIMENT_input_type),       intent(in ) :: SEDIMENT_input
  type(SEDIMENT_forcing_type),     intent(in ) :: SEDIMENT_forcing

  integer (SEDIMENT_i4) :: numLevelsMax, numColumnsMax, numColumns

! !OUTPUT PARAMETERS:

  type(SEDIMENT_output_type),      intent(inout) :: SEDIMENT_output
  type(SEDIMENT_diagnostics_type), intent(inout) :: SEDIMENT_diagnostic_fields

!-----------------------------------------------------------------------
!  local variables
!-----------------------------------------------------------------------

  integer(SEDIMENT_i4) ::  &
      column, kmax, k

  integer(SEDIMENT_i4) ::  &
      sedConcen_ind        ! suspended sediment cocentration index

  real(SEDIMENT_r8), allocatable, dimension(:,:) :: & !kg/m^3
         sedConcen_loc     ! local copy of model sedConcen
!---------------------------------------------------------------------------
!---------------------------------------------------------------------------

!-----------------------------------------------------------------------
!  initialize  all tendencies to zero
!-----------------------------------------------------------------------

   SEDIMENT_output%SEDIMENT_tendencies = 0.0_SEDIMENT_r8

!-----------------------------------------------------------------------
!  allocate local copies of tracers
!-----------------------------------------------------------------------

   allocate(sedConcen_loc(numLevelsMax,numColumns))
!-----------------------------------------------------------------------
!  assign indices.  this is not necessary but results in fewer
!    differences between original and new code.
!-----------------------------------------------------------------------

   sedConcen_ind    = SEDIMENT_indices%sedConcen_ind

!-----------------------------------------------------------------------
!  loop over columns
!-----------------------------------------------------------------------

   setup_loop: do column = 1, numColumns

   kmax = SEDIMENT_input%number_of_active_levels(column)
   if (kmax < 1) cycle setup_loop

   do k = 1, kmax

    !---------------------------------------------------------------------------
    !   create local copies of requisite ecotracers
    !   treat negative values as zero and apply mask to locals
    !---------------------------------------------------------------------------

   sedConcen_loc(k,column)      = max(0.0_SEDIMENT_r8, SEDIMENT_input%SEDIMENT_tracers(k,column,sedConcen_ind))

   end do  !  end of setup k loop

   enddo  setup_loop  !  end of setup column loop

!-----------------------------------------------------------------------
!  loop over columns
!-----------------------------------------------------------------------

   column_loop: do column = 1, numColumns

   kmax = SEDIMENT_input%number_of_active_levels(column)
   if (kmax < 1) cycle column_loop

!-----------------------------------------------------------------------
!  various k==1 initializations
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!  loop over levels
!-----------------------------------------------------------------------

   do k = 1, kmax

   enddo ! k loop

   enddo column_loop ! i loop

!-----------------------------------------------------------------------

 end subroutine SEDIMENT_SourceSink

!***********************************************************************
!***********************************************************************
!BOP
! !IROUTINE: SEDIMENT_SurfaceFluxes
! !INTERFACE:

 subroutine SEDIMENT_SurfaceFluxes(SEDIMENT_indices, SEDIMENT_input, SEDIMENT_forcing,   &
                              SEDIMENT_flux_diagnostic_fields, &
                              numColumnsMax, numColumns)

! !DESCRIPTION:
!  Compute surface fluxes for tracegas tracer module.
!
! !REVISION HISTORY:
!  same as module

! !INPUT PARAMETERS:

  type(SEDIMENT_indices_type), intent(in )   :: SEDIMENT_indices
  type(SEDIMENT_input_type),   intent(in )   :: SEDIMENT_input
  type(SEDIMENT_forcing_type), intent(inout) :: SEDIMENT_forcing

  integer (SEDIMENT_i4) :: numColumnsMax, numColumns

! !OUTPUT PARAMETERS:

  type(SEDIMENT_flux_diagnostics_type), intent(inout) :: SEDIMENT_flux_diagnostic_fields

!-----------------------------------------------------------------------
!  local variables
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
 end subroutine SEDIMENT_SurfaceFluxes


!*****************************************************************************
!-----------------------------------------------------------------------
 end module SEDIMENT_mod
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
