#define ESMF_ERR_RETURN(rc) if (ESMF_LogFoundError( \
        rcToCheck=rc, \
        msg=ESMF_LOGERR_PASSTHRU, \
        line=__LINE__, \
        file=__FILE__) \
    ) return

module mpas_nuopc_atm
  !> MPAS NUOPC Cap for Atmosphere

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    modelSS => SetServices

  implicit none

  private

  public SetVM, SetServices

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(model, rc)
    !> Register model entry points:
    !>   Advertise: advertise import and export fields
    !>   Realize: realize connected fields
    !>   SetClock: initialize model clock
    !>   DataInitialize: initialize data in import and export states
    !>   Advance: advance model by a single time step
    !>   Finalize: finalize model and cleanup memory

    ! arguments
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! derive from NUOPC_Model
    call NUOPC_CompDerive(model, modelSS, rc=rc)
    ESMF_ERR_RETURN(rc)

    ! specialize model entry points
    call NUOPC_CompSpecialize(model, specLabel=label_Advertise, &
      specRoutine=Advertise, rc=rc)
    ESMF_ERR_RETURN(rc)
    call NUOPC_CompSpecialize(model, specLabel=label_RealizeProvided, &
      specRoutine=Realize, rc=rc)
    ESMF_ERR_RETURN(rc)
    call NUOPC_CompSpecialize(model, specLabel=label_SetClock, &
      specRoutine=SetClock, rc=rc)
    ESMF_ERR_RETURN(rc)
    call NUOPC_CompSpecialize(model, specLabel=label_DataInitialize, &
      specRoutine=DataInitialize, rc=rc)
    ESMF_ERR_RETURN(rc)
    call NUOPC_CompSpecialize(model, specLabel=label_Advance, &
      specRoutine=Advance, rc=rc)
    ESMF_ERR_RETURN(rc)
    call NUOPC_CompSpecialize(model, specLabel=label_Finalize, &
      specRoutine=Finalize, rc=rc)
    ESMF_ERR_RETURN(rc)

  end subroutine SetServices

  !-----------------------------------------------------------------------------

  subroutine Advertise(model, rc)
    !> Advertise available export fields and desired import fields

    ! arguments
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State) :: importState, exportState

    rc = ESMF_SUCCESS

    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    ESMF_ERR_RETURN(rc)

    call ESMF_LogWrite(logmsgFlag=ESMF_LOGMSG_ERROR, &
      msg="MPAS NUOPC ATM - Advertise has not been implemented", &
      line=__LINE__, &
      file=__FILE__)

  end subroutine Advertise

  !-----------------------------------------------------------------------------

  subroutine Realize(model, rc)
    !> Check field connections and realize connected fields

    ! arguments
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State) :: importState, exportState

    rc = ESMF_SUCCESS

    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    ESMF_ERR_RETURN(rc)

    call ESMF_LogWrite(logmsgFlag=ESMF_LOGMSG_ERROR, &
      msg="MPAS NUOPC ATM - Realize has not been implemented", &
      line=__LINE__, &
      file=__FILE__)

  end subroutine Realize

  !-----------------------------------------------------------------------------

  subroutine SetClock(model, rc)
    !> Adjust model clock and time step during initialization

    ! arguments
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock) :: clock
    type(ESMF_State) :: importState, exportState

    rc = ESMF_SUCCESS

    call NUOPC_ModelGet(model, modelClock=clock, &
      importState=importState, &
      exportState=exportState, rc=rc)
    ESMF_ERR_RETURN(rc)

    call ESMF_LogWrite(logmsgFlag=ESMF_LOGMSG_ERROR, &
      msg="MPAS NUOPC ATM - SetClock has not been implemented", &
      line=__LINE__, &
      file=__FILE__)

  end subroutine SetClock

  !-----------------------------------------------------------------------------

  subroutine DataInitialize(model, rc)
    !> Initialize data in import and export states

    ! arguments
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock) :: clock
    type(ESMF_State) :: importState, exportState

    rc = ESMF_SUCCESS

    call NUOPC_ModelGet(model, modelClock=clock, &
      importState=importState, &
      exportState=exportState, rc=rc)
    ESMF_ERR_RETURN(rc)

    call NUOPC_CompAttributeSet(model, &
      name="InitializeDataComplete", value="true", rc=rc)
    ESMF_ERR_RETURN(rc)

    call ESMF_LogWrite(logmsgFlag=ESMF_LOGMSG_ERROR, &
      msg="MPAS NUOPC ATM - DataInitialize has not been implemented", &
      line=__LINE__, &
      file=__FILE__)

  end subroutine DataInitialize

  !-----------------------------------------------------------------------------

  subroutine Advance(model, rc)
    !> Advance model by a single time step

    ! arguments
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock) :: clock
    type(ESMF_State) :: importState, exportState

    rc = ESMF_SUCCESS

    call NUOPC_ModelGet(model, modelClock=clock, &
      importState=importState, &
      exportState=exportState, rc=rc)
    ESMF_ERR_RETURN(rc)

    call ESMF_LogWrite(logmsgFlag=ESMF_LOGMSG_ERROR, &
      msg="MPAS NUOPC ATM - Advance has not been implemented", &
      line=__LINE__, &
      file=__FILE__)

  end subroutine Advance

  !-----------------------------------------------------------------------------

  subroutine Finalize(model, rc)
    !> Finalize model and cleanup memory allocations

    ! arguments
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)            :: importState, exportState

    rc = ESMF_SUCCESS

    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, rc=rc)
    ESMF_ERR_RETURN(rc)

    call ESMF_LogWrite(logmsgFlag=ESMF_LOGMSG_ERROR, &
      msg="MPAS NUOPC ATM - Finalize has not been implemented", &
      line=__LINE__, &
      file=__FILE__)

  end subroutine Finalize

  !-----------------------------------------------------------------------------

end module mpas_nuopc_atm
