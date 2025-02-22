module mpas_debug_spec_handler
  use, intrinsic :: iso_c_binding
  implicit none

  interface
    subroutine DebugSpecHandler_ctor              ( pDebugSpecHandler )              &
      bind( c, name="DebugSpecHandler_ctor"               )
      ! get iso_c_binding types
      import
      implicit none
      ! return DebugSpecHandler *
      type( c_ptr ), intent( inout ) :: pDebugSpecHandler
    end subroutine DebugSpecHandler_ctor

    subroutine DebugSpecHandler_dtor              ( pDebugSpecHandler )              &
      bind( c, name="DebugSpecHandler_dtor"               )
      ! get iso_c_binding types
      import
      implicit none
      type( c_ptr ), intent( inout ) :: pDebugSpecHandler
      ! return void
    end subroutine DebugSpecHandler_dtor

    subroutine DebugSpecHandler_setDefaultDebugLevel( pDebugSpecHandler, debugLevel ) &
      bind( c, name="DebugSpecHandler_setDefaultDebugLevel"         )
      ! get iso_c_binding types
      import
      implicit none
      type( c_ptr ), value :: pDebugSpecHandler
      integer(   kind=c_int32_t ), intent( in ), value :: debugLevel
      ! return void
    end subroutine DebugSpecHandler_setDefaultDebugLevel

    subroutine DebugSpecHandler_addDebugSpecs        ( pDebugSpecHandler, specifiers ) &
      bind( c, name="DebugSpecHandler_addDebugSpecs"         )
      ! get iso_c_binding types
      import
      implicit none
      type( c_ptr ), value :: pDebugSpecHandler
      character( kind = c_char ), dimension(*), intent( in ) :: specifiers
      ! return void
    end subroutine DebugSpecHandler_addDebugSpecs

    function DebugSpecHandler_canLog          ( pDebugSpecHandler, debugLevel, tags, file )        &
      bind( c, name="DebugSpecHandler_canLog"           ) result( canLog )
      ! get iso_c_binding types
      import
      implicit none
      type( c_ptr ), value :: pDebugSpecHandler
      integer(   kind=c_int32_t ), intent( in ), value :: debugLevel
      character( kind = c_char  ), dimension(*), intent( in ) :: tags
      character( kind = c_char  ), dimension(*), intent( in ) :: file
      logical(   kind=c_bool    ) :: canLog
      ! return bool
    end function DebugSpecHandler_canLog

  end interface
end module mpas_debug_spec_handler
