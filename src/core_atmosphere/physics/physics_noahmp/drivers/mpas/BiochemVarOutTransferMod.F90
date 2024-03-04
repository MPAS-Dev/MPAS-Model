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
              I    => noahmp%config%domain%GridIndexI  &
             )
! ---------------------------------------------------------------------

    ! biochem state variables
    NoahmpIO%LFMASSXY(I) = noahmp%biochem%state%LeafMass
    NoahmpIO%RTMASSXY(I) = noahmp%biochem%state%RootMass
    NoahmpIO%STMASSXY(I) = noahmp%biochem%state%StemMass
    NoahmpIO%WOODXY  (I) = noahmp%biochem%state%WoodMass
    NoahmpIO%STBLCPXY(I) = noahmp%biochem%state%CarbonMassDeepSoil
    NoahmpIO%FASTCPXY(I) = noahmp%biochem%state%CarbonMassShallowSoil
    NoahmpIO%GDDXY   (I) = noahmp%biochem%state%GrowDegreeDay
    NoahmpIO%PGSXY   (I) = noahmp%biochem%state%PlantGrowStage
    NoahmpIO%GRAINXY (I) = noahmp%biochem%state%GrainMass

    ! biochem flux variables
    NoahmpIO%NEEXY   (I) = noahmp%biochem%flux%NetEcoExchange
    NoahmpIO%GPPXY   (I) = noahmp%biochem%flux%GrossPriProduction
    NoahmpIO%NPPXY   (I) = noahmp%biochem%flux%NetPriProductionTot
    NoahmpIO%PSNXY   (I) = noahmp%biochem%flux%PhotosynTotal

    end associate

  end subroutine BiochemVarOutTransfer

end module BiochemVarOutTransferMod
