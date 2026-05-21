module MatrixSolverTriDiagonalMod

!!! Solve tri-diagonal matrix problem

  use Machine
  use NoahmpVarType
  use ConstantDefineMod

  implicit none

contains

  subroutine MatrixSolverTriDiagonal(P, A, B, C, D, Delta, IndTopLayer, NumSoilLayer, NumSnowLayerMax)

! ------------------------ Code history --------------------------------------------------
! Original Noah-MP subroutine: ROSR12
! Original code: Guo-Yue Niu and Noah-MP team (Niu et al. 2011)
! Refactered code: C. He, P. Valayamkunnath, & refactor team (He et al. 2023)
! ----------------------------------------------------------------------------------------
! INVERT (SOLVE) THE TRI-DIAGONAL MATRIX PROBLEM SHOWN BELOW:
! ###                                            ### ###  ###   ###  ###
! #B(1), C(1),  0  ,  0  ,  0  ,   . . .  ,    0   # #      #   #      #
! #A(2), B(2), C(2),  0  ,  0  ,   . . .  ,    0   # #      #   #      #
! # 0  , A(3), B(3), C(3),  0  ,   . . .  ,    0   # #      #   # D(3) #
! # 0  ,  0  , A(4), B(4), C(4),   . . .  ,    0   # # P(4) #   # D(4) #
! # 0  ,  0  ,  0  , A(5), B(5),   . . .  ,    0   # # P(5) #   # D(5) #
! # .                                          .   # #  .   # = #   .  #
! # .                                          .   # #  .   #   #   .  #
! # .                                          .   # #  .   #   #   .  #
! # 0  , . . . , 0 , A(M-2), B(M-2), C(M-2),   0   # #P(M-2)#   #D(M-2)#
! # 0  , . . . , 0 ,   0   , A(M-1), B(M-1), C(M-1)# #P(M-1)#   #D(M-1)#
! # 0  , . . . , 0 ,   0   ,   0   ,  A(M) ,  B(M) # # P(M) #   # D(M) #
! ###                                            ### ###  ###   ###  ###
! ----------------------------------------------------------------------

    implicit none

! in & out variables
    integer               , intent(in) :: IndTopLayer          ! top layer index: soil layer starts from IndTopLayer = 1
    integer               , intent(in) :: NumSoilLayer         ! number of soil layers
    integer               , intent(in) :: NumSnowLayerMax      ! maximum number of snow layers
    real(kind=kind_noahmp), dimension(-NumSnowLayerMax+1:NumSoilLayer), intent(in)    :: A, B, D    ! Tri-diagonal matrix elements
    real(kind=kind_noahmp), dimension(-NumSnowLayerMax+1:NumSoilLayer), intent(inout) :: C,P,Delta  ! Tri-diagonal matrix elements

! local variables
    integer  :: K, KK   ! loop indices
! ----------------------------------------------------------------------

    ! INITIALIZE EQN COEF C FOR THE LOWEST SOIL LAYER
    C (NumSoilLayer) = 0.0
    P (IndTopLayer)  = - C (IndTopLayer) / B (IndTopLayer)

    ! SOLVE THE COEFS FOR THE 1ST SOIL LAYER
    Delta (IndTopLayer) = D (IndTopLayer) / B (IndTopLayer)

    ! SOLVE THE COEFS FOR SOIL LAYERS 2 THRU NumSoilLayer
    do K = IndTopLayer+1, NumSoilLayer
       P (K)     = - C (K) * ( 1.0 / (B (K) + A (K) * P (K -1)) )
       Delta (K) = (D (K) - A (K) * Delta (K -1)) * (1.0 / (B (K) + A (K) * P (K -1)))
    enddo

    ! SET P TO Delta FOR LOWEST SOIL LAYER
    P (NumSoilLayer) = Delta (NumSoilLayer)

    ! ADJUST P FOR SOIL LAYERS 2 THRU NumSoilLayer
    do K = IndTopLayer+1, NumSoilLayer
       KK     = NumSoilLayer - K + (IndTopLayer-1) + 1
       P (KK) = P (KK) * P (KK +1) + Delta (KK)
    enddo

  end subroutine MatrixSolverTriDiagonal

end module MatrixSolverTriDiagonalMod
