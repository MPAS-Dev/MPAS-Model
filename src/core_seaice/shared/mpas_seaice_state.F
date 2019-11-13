!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  seaice_state
!
!> \brief
!> \author Adrian K. Turner, LANL
!> \date 12th November 2019
!> \details
!>
!
!-----------------------------------------------------------------------

module seaice_state

  use mpas_derived_types
  use mpas_pool_routines

  implicit none

  private
  save

  public :: &
       seaice_init_state

  interface setup_state_array
     module procedure setup_state_array_1DReal
     module procedure setup_state_array_2DReal
     module procedure setup_state_array_3DReal
     module procedure setup_state_array_4DReal
     module procedure setup_state_array_5DReal
     module procedure setup_state_array_1DInteger
     module procedure setup_state_array_2DInteger
     module procedure setup_state_array_3DInteger
  end interface setup_state_array

  ! state variables
  real(kind=RKIND), dimension(:), allocatable, public :: &
       uVelocityState, &
       vVelocityState

contains

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  seaice_init_state
!
!> \brief
!> \author Adrian K. Turner, LANL
!> \date 12th November 2019
!> \details
!>
!
!-----------------------------------------------------------------------

  subroutine seaice_init_state(&
       domain)!{{{

    type(domain_type), intent(inout) :: &
         domain !< Input/Output:

    type(MPAS_pool_type), pointer :: &
         velocitySolverPool

    type(field1DReal), pointer :: &
         uVelocityField, &
         vVelocityField

    call MPAS_pool_get_subpool(domain % blocklist % structs, "velocity_solver", velocitySolverPool)

    call MPAS_pool_get_field(velocitySolverPool, "uVelocity", uVelocityField, 1)
    call MPAS_pool_get_field(velocitySolverPool, "vVelocity", vVelocityField, 1)

    call setup_state_array(uVelocityField, uVelocityState)
    call setup_state_array(vVelocityField, vVelocityState)

  end subroutine seaice_init_state

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  setup_state_array_1DReal
!
!> \brief
!> \author Adrian K. Turner, LANL
!> \date 12th November 2019
!> \details
!>
!
!-----------------------------------------------------------------------

  subroutine setup_state_array_1DReal(field, array)

    type(field1DReal), pointer, intent(inout) :: &
         field

    real(kind=RKIND), dimension(:), allocatable, target, intent(out) :: &
         array

    allocate(array(field % dimSizes(1)))

    array = field % array

    deallocate(field % array)

    field % array => array

  end subroutine setup_state_array_1DReal

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  setup_state_array_2DReal
!
!> \brief
!> \author Adrian K. Turner, LANL
!> \date 12th November 2019
!> \details
!>
!
!-----------------------------------------------------------------------

  subroutine setup_state_array_2DReal(field, array)

    type(field2DReal), pointer, intent(inout) :: &
         field

    real(kind=RKIND), dimension(:,:), allocatable, target, intent(out) :: &
         array

    allocate(array(field % dimSizes(1), field % dimSizes(2)))

    array = field % array

    deallocate(field % array)

    field % array => array

  end subroutine setup_state_array_2DReal

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  setup_state_array_3DReal
!
!> \brief
!> \author Adrian K. Turner, LANL
!> \date 12th November 2019
!> \details
!>
!
!-----------------------------------------------------------------------

  subroutine setup_state_array_3DReal(field, array)

    type(field3DReal), pointer, intent(inout) :: &
         field

    real(kind=RKIND), dimension(:,:,:), allocatable, target, intent(out) :: &
         array

    allocate(array(field % dimSizes(1), field % dimSizes(2), field % dimSizes(3)))

    array = field % array

    deallocate(field % array)

    field % array => array

  end subroutine setup_state_array_3DReal

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  setup_state_array_4DReal
!
!> \brief
!> \author Adrian K. Turner, LANL
!> \date 12th November 2019
!> \details
!>
!
!-----------------------------------------------------------------------

  subroutine setup_state_array_4DReal(field, array)

    type(field4DReal), pointer, intent(inout) :: &
         field

    real(kind=RKIND), dimension(:,:,:,:), allocatable, target, intent(out) :: &
         array

    allocate(array(field % dimSizes(1), field % dimSizes(2), field % dimSizes(3), field % dimSizes(4)))

    array = field % array

    deallocate(field % array)

    field % array => array

  end subroutine setup_state_array_4DReal

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  setup_state_array_5DReal
!
!> \brief
!> \author Adrian K. Turner, LANL
!> \date 12th November 2019
!> \details
!>
!
!-----------------------------------------------------------------------

  subroutine setup_state_array_5DReal(field, array)

    type(field5DReal), pointer, intent(inout) :: &
         field

    real(kind=RKIND), dimension(:,:,:,:,:), allocatable, target, intent(out) :: &
         array

    allocate(array(field % dimSizes(1), field % dimSizes(2), field % dimSizes(3), field % dimSizes(4), field % dimSizes(5)))

    array = field % array

    deallocate(field % array)

    field % array => array

  end subroutine setup_state_array_5DReal

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  setup_state_array_1DInteger
!
!> \brief
!> \author Adrian K. Turner, LANL
!> \date 12th November 2019
!> \details
!>
!
!-----------------------------------------------------------------------

  subroutine setup_state_array_1DInteger(field, array)

    type(field1DInteger), pointer, intent(inout) :: &
         field

    integer, dimension(:), allocatable, target, intent(out) :: &
         array

    allocate(array(field % dimSizes(1)))

    array = field % array

    deallocate(field % array)

    field % array => array

  end subroutine setup_state_array_1DInteger

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  setup_state_array_2DInteger
!
!> \brief
!> \author Adrian K. Turner, LANL
!> \date 12th November 2019
!> \details
!>
!
!-----------------------------------------------------------------------

  subroutine setup_state_array_2DInteger(field, array)

    type(field2DInteger), pointer, intent(inout) :: &
         field

    integer, dimension(:,:), allocatable, target, intent(out) :: &
         array

    allocate(array(field % dimSizes(1), field % dimSizes(2)))

    array = field % array

    deallocate(field % array)

    field % array => array

  end subroutine setup_state_array_2DInteger

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  setup_state_array_3DInteger
!
!> \brief
!> \author Adrian K. Turner, LANL
!> \date 12th November 2019
!> \details
!>
!
!-----------------------------------------------------------------------

  subroutine setup_state_array_3DInteger(field, array)

    type(field3DInteger), pointer, intent(inout) :: &
         field

    integer, dimension(:,:,:), allocatable, target, intent(out) :: &
         array

    allocate(array(field % dimSizes(1), field % dimSizes(2), field % dimSizes(3)))

    array = field % array

    deallocate(field % array)

    field % array => array

  end subroutine setup_state_array_3DInteger

!-----------------------------------------------------------------------

end module seaice_state
