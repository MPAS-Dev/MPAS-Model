MODULE SEDIMENT_parms

! ! AUTHOR Zhendong Cao, Mathew Maltrud
! ! DATE 02/04/2021
! ! DESCRIPTION
  !-----------------------------------------------------------------------------
  !   Backfill by imitating analogous module with name ecosys_parms
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !   This module manages the parameter variables for the module SEDIMENT_mod.
  !   Most of the variables are not parameters in the Fortran sense. In the
  !   the Fortran sense, they are vanilla module variables.
  !
  !   This modules handles initializing the variables to default values and
  !   reading them from the namelist SEDIMENT_parms. The values used are echoed
  !   to stdout for record keeping purposes.
  !
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !   variables/subroutines/function used from other modules
  !   The following are used extensively in this tracegas, so are used at
  !   the module level. The use statements for variables that are only needed
  !   locally are located at the module subprogram level.
  !-----------------------------------------------------------------------------

! !USES:

  IMPLICIT NONE

  !-----------------------------------------------------------------------------
  !   public/private declarations
  !   all module variables are public and should have their values preserved
  !-----------------------------------------------------------------------------

  PUBLIC
  SAVE

  !-----------------------------------------------------------------------------
  !   kinds definitions
  !-----------------------------------------------------------------------------
   integer, parameter, public ::               &
      SEDIMENT_char           = 256                    ,&
      SEDIMENT_log            = kind(.true.)           ,&
      SEDIMENT_i4             = selected_int_kind(6)   ,&
      SEDIMENT_i8             = selected_int_kind(13)  ,&
      SEDIMENT_r4             = selected_real_kind(6)  ,&
      SEDIMENT_r8             = selected_real_kind(13)

  !-----------------------------------------------------------------------------
  !   floating point constants used across ecosystem module
  !-----------------------------------------------------------------------------

  real (SEDIMENT_r8), parameter, private :: &
       c0 =   0.0_SEDIMENT_r8,         &
       c1 =   1.0_SEDIMENT_r8,         &
       c2 =   2.0_SEDIMENT_r8,         &
       c10 = 10.0_SEDIMENT_r8,         &
       p5  =  0.5_SEDIMENT_r8

  REAL(KIND=SEDIMENT_r8), PARAMETER :: &
       spd = 86400.0_SEDIMENT_r8,        & ! number of seconds in a day
       dps = c1 / spd,          & ! number of days in a second
       yps = c1 / (365.0_SEDIMENT_r8*spd)  ! number of years in a second

  type, public :: SEDIMENT_indices_type
   integer (SEDIMENT_i4) :: &
      sedConcen_ind       ! SEDIMENT index

   character (SEDIMENT_char), allocatable, dimension(:) ::  &
      short_name,      & ! short name of variable
      long_name,       & ! long name of variable
      units              ! units of variable
  end type SEDIMENT_indices_type

  type, public :: SEDIMENT_input_type
   real (SEDIMENT_r8), allocatable, dimension(:,:,:) :: &
      SEDIMENT_tracers
   real (SEDIMENT_r8), allocatable, dimension(:,:) :: &
      cell_thickness
   integer (SEDIMENT_i4), allocatable, dimension(:) ::  &
      number_of_active_levels
  end type SEDIMENT_input_type

  type, public :: SEDIMENT_forcing_type
   real (SEDIMENT_r8), allocatable, dimension(:) :: &
      SST,                       &
      SSS
  end type SEDIMENT_forcing_type

  type, public :: SEDIMENT_output_type
   real (SEDIMENT_r8), allocatable, dimension(:,:,:) :: &
      SEDIMENT_tendencies
  end type SEDIMENT_output_type

  type, public :: SEDIMENT_flux_diagnostics_type
    real (SEDIMENT_r8), allocatable, dimension(:) :: &
      diag_sedConcen_flux
  end type SEDIMENT_flux_diagnostics_type

  type, public :: SEDIMENT_diagnostics_type
    real (SEDIMENT_r8), allocatable, dimension(:,:) :: &
      diag_sedConcen
  end type SEDIMENT_diagnostics_type

  !-----------------------------------------------------------------------------
  !   floating point constants used across SEDIMENT module
  !-----------------------------------------------------------------------------


  !*****************************************************************************

CONTAINS

  !*****************************************************************************

  SUBROUTINE SEDIMENT_parms_init

    !---------------------------------------------------------------------------
    !   default parameter values
    !---------------------------------------------------------------------------

    !---------------------------------------------------------------------------
  END SUBROUTINE SEDIMENT_parms_init

  !*****************************************************************************

END MODULE SEDIMENT_parms
