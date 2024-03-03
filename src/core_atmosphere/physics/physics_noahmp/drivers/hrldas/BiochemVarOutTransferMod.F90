module BiochemVarOutTransferMod

!!! Transfer column (1-D) biochemistry variables to 2D NoahmpIO for output

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType
  use NoahmpVarType

  implicit none

contains

!=== Transfer model states to output =====

  subroutine BiochemVarOutTransfer(noahmp, NoahmpIO)

    implicit none

    type(noahmp_type),   intent(inout) :: noahmp
    type(NoahmpIO_type), intent(inout) :: NoahmpIO

! ---------------------------------------------------------------------
    associate(                                         &
              I    => noahmp%config%domain%GridIndexI ,&
              J    => noahmp%config%domain%GridIndexJ  &
             )
! ---------------------------------------------------------------------

    ! biochem state variables
    NoahmpIO%LFMASSXY(I,J) = noahmp%biochem%state%LeafMass
    NoahmpIO%RTMASSXY(I,J) = noahmp%biochem%state%RootMass
    NoahmpIO%STMASSXY(I,J) = noahmp%biochem%state%StemMass
    NoahmpIO%WOODXY  (I,J) = noahmp%biochem%state%WoodMass
    NoahmpIO%STBLCPXY(I,J) = noahmp%biochem%state%CarbonMassDeepSoil
    NoahmpIO%FASTCPXY(I,J) = noahmp%biochem%state%CarbonMassShallowSoil
    NoahmpIO%GDDXY   (I,J) = noahmp%biochem%state%GrowDegreeDay
    NoahmpIO%PGSXY   (I,J) = noahmp%biochem%state%PlantGrowStage
    NoahmpIO%GRAINXY (I,J) = noahmp%biochem%state%GrainMass

    ! biochem flux variables
    NoahmpIO%NEEXY   (I,J) = noahmp%biochem%flux%NetEcoExchange
    NoahmpIO%GPPXY   (I,J) = noahmp%biochem%flux%GrossPriProduction
    NoahmpIO%NPPXY   (I,J) = noahmp%biochem%flux%NetPriProductionTot
    NoahmpIO%PSNXY   (I,J) = noahmp%biochem%flux%PhotosynTotal

    end associate

  end subroutine BiochemVarOutTransfer

end module BiochemVarOutTransferMod
