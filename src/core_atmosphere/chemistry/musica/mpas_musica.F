! Copyright (c) 2025 The University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at https://mpas-dev.github.io/license.html .
!
!-----------------------------------------------------------------------
!  mpas_musica
!
!> \brief Manages interactions with the MUSICA chemistry package
!> \author CheMPAS-A Developers
!> \date   20 August 2025
!> \details
!>  This module manages the interactions with the MUSICA chemistry package,
!>  including initialization, time-stepping, and finalization for
!>  MICM (ODE solver) and TUV-x (photolysis rate constant calculator).
!--------------------------------------------------------------------------
module mpas_musica

    use musica_micm,  only: micm_t, get_micm_version
    ! get_micm_version is here to avoid an ICE when within musica_init
    use musica_state, only: state_t

    implicit none

    private

    public :: musica_init, musica_step, musica_finalize

    type(micm_t),  pointer :: micm  => null ( )  ! Pointer to the MICM ODE solver instance
    type(state_t), pointer :: state => null ( )  ! Pointer to the state of the MICM solver
    logical :: musica_is_initialized = .false.   ! Flag to track if MUSICA was successfully initialized

    contains

    !------------------------------------------------------------------------
    !  subroutine musica_init
    !
    !> \brief Initializes the MUSICA chemistry package
    !> \author CheMPAS-A Developers
    !> \date   20 August 2025
    !> \details
    !>  This subroutine initializes the MUSICA chemistry package,
    !>  setting up necessary parameters and data structures for the simulation.
    !>  For now, we will load fixed configurations for MICM and TUV-x.
    !>  Later, we will gradually replace the fixed configuration elements
    !>  with runtime updates using the MUSICA API.
    !------------------------------------------------------------------------
    subroutine musica_init(filename_of_micm_configuration, &
            number_of_grid_cells, &
            error_code, error_message)

        use iso_fortran_env, only: real64

        use musica_micm, only : RosenbrockStandardOrder
        use musica_util, only : error_t, string_t

        use mpas_kind_types, only : RKIND
        use mpas_log, only : mpas_log_write

        character(len=*),              intent(in)  :: filename_of_micm_configuration
        integer,                       intent(in)  :: number_of_grid_cells
        integer,                       intent(out) :: error_code
        character(len=:), allocatable, intent(out) :: error_message

        character(len=*), parameter :: INITIAL_CONCENTRATION_PROPERTY = "__initial concentration"

        type(error_t)  :: error
        type(string_t) :: micm_version
        ! TEMPORARY: Hard-coded options for the MICM solver
        integer :: solver_type = RosenbrockStandardOrder
        integer :: i_species
        character(len=:), allocatable :: species_name
        real(real64)                  :: initial_concentration
        real(real64), allocatable     :: initial_profile(:)

        ! Skip MUSICA initialization if no configuration file is provided
        if (len_trim(filename_of_micm_configuration) == 0) then
            call mpas_log_write('MUSICA chemistry disabled: no MICM configuration file specified')
            error_code = 0
            error_message = ''
            return
        end if

        micm_version = get_micm_version()

        call mpas_log_write('Initializing MUSICA chemistry package...')
        call mpas_log_write('MICM configuration file: ' // trim(filename_of_micm_configuration))
        call mpas_log_write('MICM version: ' // micm_version%value_)
        call mpas_log_write('MICM number of grid cells: $i', intArgs=[number_of_grid_cells])

        micm => micm_t(trim(filename_of_micm_configuration), solver_type, error)
        if (has_error_occurred(error, error_message, error_code)) return

        state => micm%get_state(number_of_grid_cells, error)
        if (has_error_occurred(error, error_message, error_code)) return

        allocate(initial_profile(number_of_grid_cells))

        do i_species = 1, state%species_ordering%size()
            species_name = state%species_ordering%name(i_species)
            call mpas_log_write('MICM species: ' // species_name)

            initial_concentration = micm%get_species_property_double(species_name, &
                                                                     INITIAL_CONCENTRATION_PROPERTY, &
                                                                     error)
            if (has_error_occurred(error, error_message, error_code)) return

            initial_profile(:) = initial_concentration
            call set_species_profile(state, species_name, initial_profile, error)
            if (has_error_occurred(error, error_message, error_code)) return

            call mpas_log_write('  initial concentration: $r', &
                 realArgs=[real(initial_concentration, kind=RKIND)])
        end do

        deallocate(initial_profile)

        call assign_rate_parameters(state, number_of_grid_cells)

        musica_is_initialized = .true.

    end subroutine musica_init

    !------------------------------------------------------------------------
    !  subroutine musica_step
    !
    !> \brief Steps the MUSICA chemistry package
    !> \author David Fillmore
    !> \date   17 November 2025
    !> \details
    !>  This subroutine steps the MICM ODE solver.
    !------------------------------------------------------------------------
    subroutine musica_step(time_step, error_code, error_message)

        use iso_fortran_env, only: real64

        use musica_micm, only : solver_stats_t
        use musica_util, only : error_t, string_t

        use mpas_log, only : mpas_log_write

        real(real64),                  intent(in)  :: time_step
        integer,                       intent(out) :: error_code
        character(len=:), allocatable, intent(out) :: error_message

        type(string_t)       :: solver_state
        type(solver_stats_t) :: solver_stats
        type(error_t)        :: error

        if (.not. musica_is_initialized) return

        call mpas_log_write('[MUSICA] Stepping MICM solver...')

        call micm%solve(time_step, state, solver_state, solver_stats, error)
        if (has_error_occurred(error, error_message, error_code)) return

        if (solver_state%get_char_array() /= 'Converged') then
            call mpas_log_write('[MUSICA Warning] MICM solver failure')
        end if

        call log_solver_stats(solver_stats)

        ! TEMPORARY: ABBA specific logging
        call mpas_log_write('[MUSICA] MICM AB concentrations')
        call log_concentrations(state, state%number_of_grid_cells, 'AB', error)
        call mpas_log_write('[MUSICA] MICM A concentrations')
        call log_concentrations(state, state%number_of_grid_cells, 'A', error)
        call mpas_log_write('[MUSICA] MICM B concentrations')
        call log_concentrations(state, state%number_of_grid_cells, 'B', error)

    end subroutine musica_step

    !------------------------------------------------------------------------
    !  subroutine musica_finalize
    !
    !> \brief Finalizes the MUSICA chemistry package
    !> \author CheMPAS-A Developers
    !> \date   20 August 2025
    !> \details
    !>  This subroutine finalizes the MUSICA chemistry package,
    !>  cleaning up any resources and data structures used during the simulation.
    !-------------------------------------------------------------------------
    subroutine musica_finalize()

        use mpas_log, only : mpas_log_write

        if (.not. musica_is_initialized) return

        call mpas_log_write('Finalizing MUSICA chemistry package...')

        ! Here we would typically clean up resources, but for now we do nothing.

    end subroutine musica_finalize

    subroutine assign_rate_parameters(state, number_of_grid_cells)
        ! Provide a default value for any rate parameters (none for ABBA by default).
        use iso_fortran_env, only : real64

        type(state_t), intent(inout) :: state
        integer, intent(in)          :: number_of_grid_cells

        integer :: cell_stride, var_stride, rp_index, cell, idx

        if (state%number_of_rate_parameters == 0) return

        cell_stride = state%rate_parameters_strides%grid_cell
        var_stride  = state%rate_parameters_strides%variable

        do rp_index = 1, state%rate_parameters_ordering%size()
            do cell = 1, number_of_grid_cells
                idx = 1 + (cell - 1) * cell_stride + (rp_index - 1) * var_stride
                state%rate_parameters(idx) = 1.0_real64
            end do
        end do
    end subroutine assign_rate_parameters

    subroutine set_species_profile(state, species_name, values, err)
        ! Write a 1-D profile into the contiguous concentrations array.
        use iso_fortran_env, only : real64
        use musica_util, only : error_t, string_t

        type(state_t), intent(inout) :: state
        character(len=*), intent(in) :: species_name
        real(real64), intent(in)     :: values(:)
        type(error_t), intent(inout) :: err
        integer :: species_index, cell_stride, var_stride, cell, idx

        species_index = state%species_ordering%index(species_name, err)
        if (.not. err%is_success()) return

        cell_stride = state%species_strides%grid_cell
        var_stride  = state%species_strides%variable

        do cell = 1, size(values)
            idx = 1 + (cell - 1) * cell_stride + (species_index - 1) * var_stride
            state%concentrations(idx) = values(cell)
        end do
    end subroutine set_species_profile

    subroutine log_concentrations(state, number_of_grid_cells, species_name, err)
        use iso_fortran_env, only : real64
        use mpas_log, only        : mpas_log_write
        use mpas_kind_types, only : RKIND
        use musica_util, only : error_t, string_t

        type(state_t), intent(in)    :: state
        integer, intent(in)          :: number_of_grid_cells
        character(len=*), intent(in) :: species_name
        type(error_t), intent(inout) :: err

        integer :: species_index, cell_stride, var_stride, cell, idx
        real (kind=RKIND) :: concentration

        species_index = state%species_ordering%index(species_name, err)
        if (.not. err%is_success()) return

        cell_stride = state%species_strides%grid_cell
        var_stride  = state%species_strides%variable

        do cell = 1, number_of_grid_cells
            idx = 1 + (cell - 1) * cell_stride + (species_index - 1) * var_stride
            concentration = state%concentrations(idx)
            call mpas_log_write('  MICM concentration: $r', realArgs=[concentration])
        end do
    end subroutine log_concentrations

    subroutine log_solver_stats(solver_stats)
        use mpas_log, only        : mpas_log_write
        use mpas_kind_types, only : RKIND
        use musica_micm, only     : solver_stats_t

        type(solver_stats_t), intent(in) :: solver_stats
 
        integer :: function_calls, jacobian_updates, number_of_steps
        integer :: accepted, rejected, decompositions, solves

        real (kind=RKIND) :: final_time

        function_calls = solver_stats%function_calls()
        jacobian_updates = solver_stats%jacobian_updates()
        number_of_steps = solver_stats%number_of_steps()
        accepted = solver_stats%accepted()
        rejected = solver_stats%rejected()
        decompositions = solver_stats%decompositions()
        solves = solver_stats%solves()
        final_time = solver_stats%final_time()

        call mpas_log_write('[MUSICA] MICM Solver statistics ...')
        call mpas_log_write('  MICM function calls: $i', intArgs=[function_calls])
        call mpas_log_write('  MICM jacobian updates: $i', intArgs=[jacobian_updates])
        call mpas_log_write('  MICM number of steps: $i', intArgs=[number_of_steps])
        call mpas_log_write('  MICM accepted: $i', intArgs=[accepted])
        call mpas_log_write('  MICM rejected: $i', intArgs=[rejected])
        call mpas_log_write('  MICM LU decompositions: $i', intArgs=[decompositions])
        call mpas_log_write('  MICM linear solves: $i', intArgs=[solves])
        call mpas_log_write('  MICM final time (s): $r', realArgs=[final_time])
    end subroutine log_solver_stats

    !-------------------------------------------------------------------------
    !  function has_error_occurred
    !
    !> \author CheMPAS-A Developers
    !> \date   20 August 2025
    !  \details
    !>  Evaluate a MUSICA error for failure and convert to error data
    !>  @param[in] error The error code to evaluate and convert.
    !>  @param[out] error_message The error message.
    !>  @param[out] error_code The error code.
    !>  @return True for an error, false for success.
    !-------------------------------------------------------------------------
    logical function has_error_occurred(error, error_message, error_code)
        use musica_util, only: error_t

        type(error_t),                 intent(in)  :: error
        character(len=:), allocatable, intent(out) :: error_message
        integer,                       intent(out) :: error_code

        character(len=30) :: error_code_str

        if ( error%is_success( ) ) then
          error_code = 0
          error_message = ''
          has_error_occurred = .false.
          return
        end if
        error_code = error%code( )
        write(error_code_str, '(I30)') error%code( )
        error_message = '[MUSICA Error]: ' // error%category( ) // '[' // &
                        trim( adjustl( error_code_str ) ) // ']: ' // error%message( )
        has_error_occurred = .true.
    end function has_error_occurred

end module mpas_musica
