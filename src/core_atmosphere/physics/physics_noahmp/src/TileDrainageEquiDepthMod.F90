module TileDrainageEquiDepthMod

!!! Calculate tile drainage equivalent depth (currently used in Hooghoudt's scheme)

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine TileDrainageEquiDepth(DrainDepthToImp, DrainTubeDist, DrainTubeRadius, DrainWatHgtAbvImp)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: TD_EQUIVALENT_DEPTH
! Original code: P. Valayamkunnath (NCAR)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------

    implicit none

! in & out variables
    real(kind=kind_noahmp), intent(in)    :: DrainDepthToImp    ! tile drainage depth to impermeable layer [m]
    real(kind=kind_noahmp), intent(in)    :: DrainTubeDist      ! distance between two drain tubes or tiles [m]
    real(kind=kind_noahmp), intent(in)    :: DrainTubeRadius    ! effective radius of drains [m]
    real(kind=kind_noahmp), intent(out)   :: DrainWatHgtAbvImp  ! Height of water table in drain Above Impermeable Layer [m]

! local variables
    integer                               :: LoopInd            ! loop index
    real(kind=kind_noahmp)                :: PiMath = 22.0/7.0  ! pi value
    real(kind=kind_noahmp)                :: DrainAspect        ! temporary drain variable
    real(kind=kind_noahmp)                :: DrainFac           ! temporary drain factor
    real(kind=kind_noahmp)                :: DrainExpFac        ! temporary drain exponential factor
    real(kind=kind_noahmp)                :: DrainFacTmp        ! temporary drain factor

! ----------------------------------------------------------------------

    ! initialization
    DrainFac    = 0.0
    DrainExpFac = 0.0
    DrainFacTmp = 0.0
    DrainAspect = (2.0 * PiMath * DrainDepthToImp) / DrainTubeDist

    ! compute tile drainage equivalent depth
    if ( DrainAspect > 0.5 ) then
       do LoopInd = 1, 45, 2
          DrainExpFac = exp(-2.0 * LoopInd * DrainAspect)
          DrainFacTmp = (4.0 * DrainExpFac) / (LoopInd * (1.0-DrainExpFac))
          DrainFac    = DrainFac + DrainFacTmp
          if ( DrainFacTmp < 1.0e-6 ) then
             DrainWatHgtAbvImp = ((PiMath*DrainTubeDist) / 8.0) / &
                                 (log(DrainTubeDist/(PiMath*DrainTubeRadius)) + DrainFac)
             exit
          endif
       enddo
    elseif ( DrainAspect < 1.0e-8 ) then
       DrainWatHgtAbvImp = DrainDepthToImp
    else
       DrainFac          = ((PiMath*PiMath)/(4.0*DrainAspect)) + (log(DrainAspect/(2.0*PiMath)))
       DrainWatHgtAbvImp = ((PiMath*DrainTubeDist) / 8.0) / &
                           (log(DrainTubeDist/(PiMath*DrainTubeRadius)) + DrainFac)
    endif

    if ( (DrainWatHgtAbvImp < 0.0) .and. (LoopInd <= 2) ) DrainWatHgtAbvImp = DrainDepthToImp

  end subroutine TileDrainageEquiDepth

end module TileDrainageEquiDepthMod
