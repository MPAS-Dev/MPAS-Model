module ConfigVarOutTransferMod

!!! To transfer 1D Noah-MP column Config variables to 2D NoahmpIO for output

! ------------------------ Code history -----------------------------------
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! -------------------------------------------------------------------------

  use Machine
  use NoahmpIOVarType
  use NoahmpVarType

  implicit none

contains

!=== Transfer model states to output=====

  subroutine ConfigVarOutTransfer(noahmp, NoahmpIO)

    implicit none

    type(NoahmpIO_type) , intent(inout) :: NoahmpIO
    type(noahmp_type),    intent(inout) :: noahmp

! ----------------------------------------------------------------------
    associate(                                                         &
              I               => noahmp%config%domain%GridIndexI      ,&
              NumSnowLayerMax => noahmp%config%domain%NumSnowLayerMax ,&
              NumSoilLayer    => noahmp%config%domain%NumSoilLayer     &
             )
! ----------------------------------------------------------------------

    ! config domain variables
    NoahmpIO%ISNOWXY(I)  = noahmp%config%domain%NumSnowLayerNeg
    NoahmpIO%ZSNSOXY(I,-NumSnowLayerMax+1:NumSoilLayer) = &
                            noahmp%config%domain%DepthSnowSoilLayer(-NumSnowLayerMax+1:NumSoilLayer)
    NoahmpIO%FORCZLSM(I) = noahmp%config%domain%RefHeightAboveSfc

    end associate

  end subroutine ConfigVarOutTransfer

end module ConfigVarOutTransferMod
