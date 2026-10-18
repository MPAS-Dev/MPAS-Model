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
!>  including initialization, time-stepping, and finalization for the
!>  MICM ODE solver.
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
    !>  The independent MICM state is initialized from species properties
    !>  in the mechanism configuration.
    !------------------------------------------------------------------------
    subroutine musica_init(filename_of_micm_configuration, &
            number_of_grid_cells, &
            error_code, error_message)

        use iso_fortran_env, only: real64
        use, intrinsic :: ieee_arithmetic, only: ieee_is_finite

        use musica_micm, only : RosenbrockStandardOrder
        use musica_util, only : error_t, string_t

        use mpas_kind_types, only : RKIND
        use mpas_log, only : mpas_log_write

        character(len=*),              intent(in)    :: filename_of_micm_configuration
        integer,                       intent(in)    :: number_of_grid_cells
        integer,                       intent(out)   :: error_code
        character(len=:), allocatable, intent(out)   :: error_message

        character(len=*), parameter :: INITIAL_CONCENTRATION_PROPERTY = '__initial concentration'

        type(error_t)  :: error
        type(string_t) :: micm_version
        ! TEMPORARY: Hard-coded options for the MICM solver
        integer :: solver_type = RosenbrockStandardOrder
        integer :: i_species
        character(len=:), allocatable :: species_name
        real(real64) :: initial_concentration
        real(real64), allocatable :: initial_profile(:)

        error_code = 0
        error_message = ''
        call musica_finalize()

        ! Skip MUSICA initialization if no configuration file is provided
        if (len_trim(filename_of_micm_configuration) == 0) then
            call mpas_log_write('MUSICA chemistry disabled: no MICM configuration file specified')
            return
        end if

        if (number_of_grid_cells < 1) then
            error_code = 1
            error_message = '[MUSICA] MICM requires at least one grid cell'
            return
        end if

        micm_version = get_micm_version()

        call mpas_log_write('Initializing MUSICA chemistry package...')
        call mpas_log_write('MICM configuration file: ' // trim(filename_of_micm_configuration))
        call mpas_log_write('MICM version: ' // micm_version%value_)
        call mpas_log_write('MICM number of grid cells: $i', intArgs=[number_of_grid_cells])

        micm => micm_t(trim(filename_of_micm_configuration), solver_type, error)
        if (has_error_occurred(error, error_message, error_code)) then
            call musica_finalize()
            return
        end if

        state => micm%get_state(number_of_grid_cells, error)
        if (has_error_occurred(error, error_message, error_code)) then
            call musica_finalize()
            return
        end if

        allocate(initial_profile(number_of_grid_cells))
        do i_species = 1, state%species_ordering%size()
            species_name = state%species_ordering%name(i_species)
            call mpas_log_write('MICM species: ' // species_name)

            initial_concentration = micm%get_species_property_double(species_name, &
                INITIAL_CONCENTRATION_PROPERTY, error)
            if (has_error_occurred(error, error_message, error_code)) then
                call musica_finalize()
                return
            end if
            if (.not. ieee_is_finite(initial_concentration) .or. initial_concentration < 0.0_real64) then
                error_code = 1
                error_message = '[MUSICA] Invalid initial concentration for species ' // species_name
                call musica_finalize()
                return
            end if

            initial_profile(:) = initial_concentration
            call set_species_profile(state, species_name, initial_profile, error)
            if (has_error_occurred(error, error_message, error_code)) then
                call musica_finalize()
                return
            end if
            call mpas_log_write('  initial concentration: $r', &
                realArgs=[real(initial_concentration, kind=RKIND)])
        end do

        call assign_rate_parameters(state, error)
        if (has_error_occurred(error, error_message, error_code)) then
            call musica_finalize()
            return
        end if

        musica_is_initialized = .true.

    end subroutine musica_init

    !------------------------------------------------------------------------
    !  subroutine musica_step
    !
    !> \brief Steps the MUSICA chemistry package
    !> \author CheMPAS-A Developers
    !> \date   20 August 2025
    !> \details
    !>  This subroutine advances the independent MICM state through one
    !>  atmospheric time step, re-entering the solver as needed.
    !------------------------------------------------------------------------
    subroutine musica_step(time_step, error_code, error_message)

        use iso_fortran_env, only: real64
        use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
        use musica_micm, only: solver_stats_t
        use musica_util, only: error_t, string_t
        use mpas_kind_types, only: RKIND
        use mpas_log, only : mpas_log_write

        real(real64),                  intent(in)  :: time_step
        integer,                       intent(out) :: error_code
        character(len=:), allocatable, intent(out) :: error_message

        integer, parameter :: MAX_SUB_CALLS = 100
        real(real64), parameter :: TIME_EPS_REL = 1.0e-9_real64
        type(string_t) :: solver_state
        type(solver_stats_t) :: solver_stats
        type(error_t) :: error
        real(real64) :: elapsed, remaining, advanced
        integer :: sub_call

        error_code = 0
        error_message = ''
        if (.not. musica_is_initialized) return

        if (.not. ieee_is_finite(time_step) .or. time_step < 0.0_real64) then
            error_code = 1
            error_message = '[MUSICA] MICM time step must be finite and nonnegative'
            return
        end if
        if (time_step == 0.0_real64) return

        ! MICM may exhaust its internal step budget before completing the
        ! requested interval. Resubmit the remaining time using the actual
        ! advance reported by MICM, with a tolerance for roundoff and a cap
        ! on the number of calls.
        elapsed = 0.0_real64
        sub_call = 0
        do while (time_step - elapsed > TIME_EPS_REL * time_step .and. sub_call < MAX_SUB_CALLS)
            sub_call = sub_call + 1
            remaining = time_step - elapsed
            call micm%solve(remaining, state, solver_state, solver_stats, error)
            if (has_error_occurred(error, error_message, error_code)) return

            select case (solver_state%get_char_array())
            case ('Converged', 'Convergence Exceeded Max Steps')
                ! Re-entry is supported when MICM reaches its internal step limit.
            case default
                error_code = 1
                error_message = '[MUSICA] MICM solve failed with state "' // &
                    solver_state%get_char_array() // '"'
                return
            end select

            advanced = solver_stats%final_time()
            if (.not. ieee_is_finite(advanced) .or. advanced <= 0.0_real64) then
                error_code = 1
                error_message = '[MUSICA] MICM failed to advance with solver state "' // &
                    solver_state%get_char_array() // '"'
                return
            end if
            elapsed = elapsed + advanced
        end do

        if (time_step - elapsed > TIME_EPS_REL * time_step) then
            call mpas_log_write('[MUSICA] MICM exhausted sub-call budget ($i calls); elapsed $r of $r s', &
                intArgs=[MAX_SUB_CALLS], realArgs=[real(elapsed, RKIND), real(time_step, RKIND)])
            error_code = 1
            error_message = '[MUSICA] MICM sub-call budget exhausted'
            return
        end if

        if (sub_call > 1) then
            call mpas_log_write('[MUSICA] MICM completed time step $r s in $i sub-calls', &
                intArgs=[sub_call], realArgs=[real(time_step, RKIND)])
        end if
        call log_solver_stats(solver_stats)

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

        if (musica_is_initialized) call mpas_log_write('Finalizing MUSICA chemistry package...')
        if (associated(state)) deallocate(state)
        if (associated(micm)) deallocate(micm)
        nullify(state, micm)
        musica_is_initialized = .false.

    end subroutine musica_finalize

    subroutine assign_rate_parameters(state, error)
        use iso_fortran_env, only: real64
        use musica_util, only: error_t

        type(state_t), intent(inout) :: state
        type(error_t), intent(inout) :: error
        integer :: rp_index, storage_index, cell, idx
        character(len=:), allocatable :: rp_name
        real(real64) :: rate_value

        do rp_index = 1, state%rate_parameters_ordering%size()
            rp_name = state%rate_parameters_ordering%name(rp_index)
            storage_index = state%rate_parameters_ordering%index(rp_name, error)
            if (.not. error%is_success()) return

            ! USER_DEFINED multipliers let the mechanism's scaling factors
            ! specify fixed rates. Other external rates remain disabled.
            rate_value = 0.0_real64
            if (index(rp_name, 'USER.') == 1) rate_value = 1.0_real64
            do cell = 1, state%number_of_grid_cells
                idx = 1 + (cell - 1) * state%rate_parameters_strides%grid_cell + &
                    (storage_index - 1) * state%rate_parameters_strides%variable
                state%rate_parameters(idx) = rate_value
            end do
        end do
    end subroutine assign_rate_parameters

    subroutine set_species_profile(state, species_name, values, error)
        use iso_fortran_env, only: real64
        use musica_util, only: error_t

        type(state_t), intent(inout) :: state
        character(len=*), intent(in) :: species_name
        real(real64), intent(in) :: values(:)
        type(error_t), intent(inout) :: error
        integer :: species_index, cell, idx

        species_index = state%species_ordering%index(species_name, error)
        if (.not. error%is_success()) return
        do cell = 1, size(values)
            idx = 1 + (cell - 1) * state%species_strides%grid_cell + &
                (species_index - 1) * state%species_strides%variable
            state%concentrations(idx) = values(cell)
        end do
    end subroutine set_species_profile

    subroutine log_solver_stats(solver_stats)
        use musica_micm, only: solver_stats_t
        use mpas_kind_types, only: RKIND
        use mpas_log, only: mpas_log_write

        type(solver_stats_t), intent(in) :: solver_stats

        call mpas_log_write('[MUSICA] MICM statistics for the last solver call:')
        call mpas_log_write('  MICM function calls: $j', int8Args=[solver_stats%function_calls()])
        call mpas_log_write('  MICM jacobian updates: $j', int8Args=[solver_stats%jacobian_updates()])
        call mpas_log_write('  MICM number of steps: $j', int8Args=[solver_stats%number_of_steps()])
        call mpas_log_write('  MICM accepted: $j', int8Args=[solver_stats%accepted()])
        call mpas_log_write('  MICM rejected: $j', int8Args=[solver_stats%rejected()])
        call mpas_log_write('  MICM LU decompositions: $j', int8Args=[solver_stats%decompositions()])
        call mpas_log_write('  MICM linear solves: $j', int8Args=[solver_stats%solves()])
        call mpas_log_write('  MICM final time (s): $r', realArgs=[real(solver_stats%final_time(), RKIND)])
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
