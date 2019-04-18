module mpas_stack
    
   implicit none

   private

   ! Public Subroutines and Structures
   public :: mpas_stack_is_empty
   public :: mpas_stack_push
   public :: mpas_stack_pop
   public :: mpas_stack_free

   public :: node, payload_t

   type payload_t
   end type payload_t

   type node
      type (node), pointer :: next => null()
      class (payload_t), pointer :: payload => null()
   end type node

   !***********************************************************************
   !
   !  module mpas_stack
   !
   !> \brief   MPAS KD-Tree module
   !> \author  Miles A. Curry 
   !> \date    04/04/19
   !> \details
   !>
   !> Introduction
   !> ==============
   !> The MPAS stack is a simple, extensible data stack data structure for use
   !> within the MPAS atmospheric model. It functions as a wrapper around a 
   !> data structure to provide usage in different areas.
   !>
   !>
   !> Creating a Stack
   !> ==================
   !> The stack data structure (`type (node)`) is defined by a single `next` pointer
   !> and a pointer to a `type (payload_t)`, which is defined as a empty derived type.
   !>
   !> To use the stack, create a derived type to fit your usage, this may be a point 
   !> (x, y, z) a string or a more complicated object with multiple variables of 
   !> different types within it. Define the class as the following:
   !>
   !> ```
   !> type, extends(payload_t) :: my_payload_name
   !>    ! Define your type as you wish
   !> end type my_payload_name
   !>
   !> type (my_payload_name) :: item1, item2
   !> ```
   !>
   !> This will enable your type to ride along with the stack. It will also enable
   !> you to push the same payload twice (if need-be).
   !>
   !> You will then need to create a stack (or multiple stacks if you desire) as
   !> the following:
   !>
   !> ```
   !> type (node) :: stack1, stack2
   !> ```
   !> 
   !>  Pushing onto a Stack
   !>  ====================
   !>  You can push your items onto a stack as:
   !> 
   !> ```
   !> stack1 => mpas_satck_push(stack1, item1)
   !> ```
   !> 
   !> Popping an item off of the stack
   !> ================================
   !> Popping an item off of the stack will require a bit more work then pushing.
   !> Because we've done some fancy Fortran class polymorphism, we will need to 
   !> use the select case to get our type (or multiple types) back into a usable
   !> object.
   !> 
   !> 
   !> ```
   !> ! The item to pop items into
   !> class (payload_t), pointer :: top
   !> type (my_payload_name), pointer :: my_item
   !> 
   !> top => mpas_stack_pop(stack1)
   !> select type(top)
   !>    type is(my_payload_name)
   !>       my_item => top
   !> end  select
   !> ```
   !> 
   !> Note: It is recommended to create your own `pop` function so you can limit 
   !> reduce the amount of coded needed. An example is provided at the bottom of
   !> this module as the function `my_pop(..)`
   !
   !-----------------------------------------------------------------------

   contains

   !***********************************************************************
   !
   !  routine mpas_stack_is_empty
   !
   !> \brief   Returns .TRUE. if the stack is empty, otherwise .FALSE.
   !> \author  Miles A. Curry 
   !> \date    04/04/19
   !>
   !
   !-----------------------------------------------------------------------
   function mpas_stack_is_empty(stack) result(is_empty)

      implicit none
      type (node), intent(in), pointer :: stack
      logical :: is_empty

      is_empty = .TRUE.
      if (associated(stack)) then
         is_empty = .FALSE.
         return
      endif

      return
   end function mpas_stack_is_empty

   !***********************************************************************
   !
   !  routine mpas_stack_push
   !
   !> \brief   Push an item onto stack
   !> \author  Miles A. Curry 
   !> \date    04/04/19
   !> \details
   !>
   !> Push a defined type that extends the payload_t type onto the specified
   !> stack.
   !>
   !
   !-----------------------------------------------------------------------
   function mpas_stack_push(stack, payload) result(new_stack)
      
      implicit none

      type(node), intent(inout), pointer :: stack
      class(payload_t), intent(inout), target :: payload

      type(node), pointer :: new_stack
      ! Allocate a new type(node) for the stack: new_stack
      !  Point new_stack % payload => payload

      allocate(new_stack)
      new_stack % payload => payload
      new_stack % next => stack

      return

   end function mpas_stack_push

   !***********************************************************************
   !
   !  function mpas_stack_pop
   !
   !> \brief   Pop off the last item added from a stack 
   !> \author  Miles A. Curry 
   !> \date    04/04/19
   !> \details
   !> Pop off the top of the stack as a payload_t type. Note, that to return
   !> top to a user defined type, a 'select type' statment will need to be
   !> used. See the top of this module for an explanation, or the bottom of
   !> this module for an example function.
   !
   !-----------------------------------------------------------------------
   function mpas_stack_pop(stack) result(top)

      implicit none

      type (node), intent(inout), pointer :: stack
      class(payload_t), pointer :: top

      if ( .NOT. associated(stack)) then
         write(0,*) "I returned here :("
         top => null()
         return
      endif

      top => stack % payload
      stack => stack % next
      return

   end function mpas_stack_pop

   !***********************************************************************
   !
   !  function mpas_stack_free
   !
   !> \brief   Deallocate the entire stack. Optionally deallocate payloads
   !> \author  Miles A. Curry 
   !> \date    04/04/19
   !> \details
   !>  Deallocate the entire stack. If free_payload is set to `.TRUE.` or absent
   !>  then the payload will be deallocated. If not, then the payload will not
   !>  be deallocated.
   !  
   !-----------------------------------------------------------------------
   subroutine mpas_stack_free(stack, free_payload)

      implicit none

      type(node), intent(inout), pointer :: stack
      logical, intent(in), optional :: free_payload
      logical :: fpl

      type(node), pointer :: cur, prev

      if (present(free_payload)) then
         fpl = free_payload
      else
         fpl = .TRUE.
      endif

      cur => stack
      do while(associated(stack)) 
         stack => stack % next
         if ( fpl ) then
            deallocate(cur % payload)
         endif
         deallocate(cur)
         cur => stack
      enddo

   end subroutine mpas_stack_free


   !***********************************************************************
   !
   !  Example user-defined pop function
   !
   !> \brief   Pop off the last item added from a stack and return it as our 
   !>          defined type
   !> \author  Miles A. Curry 
   !> \date    04/04/19
   !> 
   !
   !-----------------------------------------------------------------------
   ! function my_pop(stack) result(item)
   !
   !    use mpas_stack, only : node, payload_t, mpas_stack_pop
   !
   !    implicit none
   !
   !    type(node), intent(inout), pointer :: stack
   !
   !    type(my_item), pointer :: item    ! Our user defined node
   !    class(payload_t), pointer :: top  ! We will need to use the payload_t type to use mpas_stack_pop(...)
   !
   !    !
   !    ! Handle a pop on an empty stack if we want to here
   !    ! Note the stack will return null if it is empty.
   !    !
   !    if (mpas_stack_is_empty(stack)) then
   !       write(0,*) "The stack was empty!"
   !       return
   !    endif
   ! 
   !    top => mpas_stack_pop(stack)
   !    
   !    select type(top)
   !       type is(my_item)
   !          item => top
   !       class default
   !          write(0,*) "We got an Error and we should handle it if we need to!!"
   !          stop
   !    end select



end module mpas_stack

