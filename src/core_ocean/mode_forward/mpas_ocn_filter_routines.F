!-------------------------------------------------------------------------------!
!                                                                               !
! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)                 !
! and the University Corporation for Atmospheric Research (UCAR).               !
!                                                                               !
! Unless noted otherwise source code is licensed under the BSD license.         !
! Additional copyright and license information can be found in the LICENSE file !
! distributed with this code, or at http://mpas-dev.github.com/license.html.    !
!                                                                               !
!-------------------------------------------------------------------------------!

!-----------------------------------------------------------!
!                                                           !
!  ocn_filter_routines                                      !
!                                                           !
!> \brief MPAS ocean split explicit time integration scheme !
!> \author Sid Bishnu                                       !
!> \date   April 2019                                       !
!> \details                                                 !
!>  This module contains routines for determining the       !
!   primary and secondary weights of various time-averging  ! 
!   filters.                                                !
!                                                           !
!-----------------------------------------------------------!

MODULE ocn_filter_routines

USE mpas_derived_types

IMPLICIT NONE

CONTAINS

   SUBROUTINE WriteTecplot1D(x,phi,filename)
   
   IMPLICIT NONE
   
   REAL(KIND=RKIND), INTENT(IN), DIMENSION(1:) :: x, phi
   CHARACTER(LEN=*), INTENT(IN) :: filename
   
   ! Local variables
   INTEGER :: iX, nX
   
   nX = UBOUND(x,1)
   
   OPEN (UNIT=1, FILE=TRIM(filename)//'.curve', STATUS='REPLACE', ACTION='WRITE', &
         FORM='FORMATTED')
   
   WRITE (1,*) '#phi'
   
   DO iX = 1, nX
      WRITE (1,*)  x(iX), phi(iX)
   END DO
   
   CLOSE (UNIT=1)
   
   END  SUBROUTINE WriteTecplot1D
   !-----------------------------------------------------------------------------------!
   !                                                                                   !
   !-----------------------------------------------------------------------------------!
   FUNCTION VectorNorm(xVector) RESULT(xVectorNorm)
   
   IMPLICIT NONE
   
   REAL(KIND=RKIND), INTENT(IN), DIMENSION(1:) :: xVector
   REAL(KIND=RKIND) :: xVectorNorm
   
   ! Loval variables
   INTEGER :: N, i  
   
   N = UBOUND(xVector,1)
   
   xVectorNorm = 0._RKIND
   
   DO i = 1, N
      xVectorNorm = xVectorNorm + (xVector(i))**2._RKIND
   END DO
   
   xVectorNorm = SQRT(xVectorNorm)
   
   END FUNCTION VectorNorm
   !-----------------------------------------------------------------------------------!
   !                                                                                   !
   !-----------------------------------------------------------------------------------!
   SUBROUTINE WriteMatrixToFile(Matrix,filename)
   
   IMPLICIT NONE
   
   REAL(KIND=RKIND), INTENT(IN), DIMENSION(1:,1:) :: Matrix
   CHARACTER(LEN=*), INTENT(IN) :: filename
   
   ! Local Variables
   INTEGER :: nX, nY, iX, iY
   
   nX = UBOUND(Matrix,1) 
   nY = UBOUND(Matrix,2)
   
   OPEN (UNIT=1, FILE=TRIM(filename)//'.txt', STATUS='REPLACE', ACTION='WRITE', &
         FORM='FORMATTED')
   
   DO iX = 1, nX
      DO iY = 1, nY
         IF (iY == nY) THEN 
            WRITE (1,'(F9.3,2X)') Matrix(iX,iY)
         ELSE
            WRITE (1,'(F9.3,2X)',ADVANCE='NO') Matrix(iX,iY)
         END IF 
      END DO 
   END DO
   
   CLOSE (UNIT=1)
   
   END  SUBROUTINE WriteMatrixToFile
   !-----------------------------------------------------------------------------------!
   !                                                                                   !
   !-----------------------------------------------------------------------------------!
   FUNCTION GetMinor(A,i,j) RESULT(M)
   ! This function returns the minor matrix of the N X N matrix A obtained by excluding 
   ! the elements in the ith row and jth column of A. The minor matrix of A is an 
   ! (N-1) X (N-1) matrix.
   
   IMPLICIT NONE
   
   INTEGER, INTENT(IN) :: i, j
   REAL(KIND=RKIND), INTENT(IN), DIMENSION(1:,1:) :: A
   REAL(KIND=RKIND), DIMENSION(1:UBOUND(A,1)-1,1:UBOUND(A,1)-1) :: M
   
   ! Local variables
   INTEGER :: N, row, column
   INTEGER :: thisRow, thisColumn
   
   N = UBOUND(A,1)
      
   thisRow = 0

   DO row = 1, N ! loop over the rows of A  
      IF (row /= i) THEN
         thisRow = thisRow + 1      
         thisColumn = 0
         DO column = 1, N ! loop over the columns of A
            IF (column /= j) THEN
               thisColumn = thisColumn + 1
               M(thisRow,thisColumn) = A(row,column)
            END IF
         END DO ! column, loop over the columns of A
      END IF 
   END DO ! row, loop over the rows of A

   END FUNCTION GetMinor
   !-----------------------------------------------------------------------------------!
   !                                                                                   !
   !-----------------------------------------------------------------------------------!
   RECURSIVE FUNCTION Determinant(A) RESULT(D)
   ! Computes the determinant of the N X N matrix A
   
   IMPLICIT NONE
   
   REAL(KIND=RKIND), INTENT(IN), DIMENSION(1:,1:) :: A
   REAL(KIND=RKIND) :: D
   
   ! Local variables
   INTEGER :: N, j
   REAL(KIND=RKIND), DIMENSION(1:UBOUND(A,1)-1,1:UBOUND(A,1)-1) :: M
   
   N = UBOUND(A,1)

   IF (N == 2) THEN
      D = A(1,1)*A(2,2) - A(1,2)*A(2,1)
      RETURN
   ELSE
      D = 0._RKIND
      DO j = 1, N
         M = GetMinor(A,1,j)
         D = D + (-1._RKIND)**(1+j)*A(1,j)*Determinant(M)
      END DO
   END IF      
   
   END FUNCTION Determinant
   !-----------------------------------------------------------------------------------!
   !                                                                                   !
   !-----------------------------------------------------------------------------------! 
   FUNCTION Invert2X2Matrix(A) RESULT(AInverse)
   
   IMPLICIT NONE
   
   REAL(KIND=RKIND), INTENT(IN), DIMENSION(1:2,1:2) :: A
   REAL(KIND=RKIND), DIMENSION(1:2,1:2) :: AInverse
   
   ! Local variable
   REAL(KIND=RKIND) :: DetA
   
   DetA = Determinant(A)
   
   AInverse(1,1) = A(2,2)/DetA
   AInverse(2,2) = A(1,1)/DetA
   AInverse(1,2) = -A(1,2)/DetA
   AInverse(2,1) = -A(2,1)/DetA
   
   END FUNCTION Invert2X2Matrix
   !-----------------------------------------------------------------------------------!
   !                                                                                   !
   !-----------------------------------------------------------------------------------!
   FUNCTION InverseMatrix(A) RESULT(AInverse)   
   ! Computes the inverse of N X N matrix A
   
   IMPLICIT NONE
   
   REAL(KIND=RKIND), INTENT(IN), DIMENSION(1:,1:) :: A
   REAL(KIND=RKIND), DIMENSION(1:UBOUND(A,1),1:UBOUND(A,1)) :: AInverse
   
   ! Local variables
   INTEGER :: N, i, j
   REAL(KIND=RKIND) :: DeterminantA
   REAL(KIND=RKIND), DIMENSION(1:UBOUND(A,1)-1,1:UBOUND(A,1)-1) :: M
   REAL(KIND=RKIND), DIMENSION(1:UBOUND(A,1),1:UBOUND(A,1)) :: AdjointA
   
   N = UBOUND(A,1)   
   
   IF (N == 2) THEN
      AInverse = Invert2X2Matrix(A)
      RETURN 
   ELSE
      DO i = 1, N
         DO j = 1, N
            M = GetMinor(A,i,j)
            AdjointA(i,j) = (-1._RKIND)**(i+j)*Determinant(M)
         END DO
      END DO
      AdjointA = TRANSPOSE(AdjointA)
      DeterminantA = Determinant(A)
      AInverse = AdjointA/DeterminantA 
   END IF 
   
   END FUNCTION InverseMatrix   
   !-----------------------------------------------------------------------------------!
   !                                                                                   !
   !-----------------------------------------------------------------------------------!
   SUBROUTINE TestInverseMatrix()
   
   IMPLICIT NONE
   
   REAL(KIND=RKIND), DIMENSION(1:3,1:3) :: A, AInverse
    
   A = 0._RKIND
   A(1,1) = 1._RKIND
   A(2,2) = 1._RKIND
   A(3,3) = 1._RKIND   
   
   AInverse = InverseMatrix(A)
   
   CALL WriteMatrixToFile(A,'TestMatrixInverse1_A')
   CALL WriteMatrixToFile(AInverse,'TestMatrixInverse1_AInverse')
   
   A = TRANSPOSE(RESHAPE((/3._RKIND,0._RKIND,2._RKIND,2._RKIND,0._RKIND,-2._RKIND, &
                           0._RKIND,1._RKIND,1._RKIND/),SHAPE(A)))    
   
   AInverse = InverseMatrix(A)
   
   CALL WriteMatrixToFile(A,'TestMatrixInverse2_A')
   CALL WriteMatrixToFile(AInverse,'TestMatrixInverse2_AInverse')
   
   END SUBROUTINE TestInverseMatrix
   !-----------------------------------------------------------------------------------!
   !                                                                                   !
   !-----------------------------------------------------------------------------------!
   FUNCTION MatrixVectorMultiplication(A,B) RESULT(C)   
   
   IMPLICIT NONE
   
   
   REAL(KIND=RKIND), INTENT(IN), DIMENSION(1:,1:) :: A
   REAL(KIND=RKIND), INTENT(IN), DIMENSION(1:) :: B
   REAL(KIND=RKIND), DIMENSION(1:UBOUND(A,1)) :: C
   
   ! Local variables
   INTEGER :: i, j, M, N
   REAL(KIND=RKIND) :: temp
   
   M = UBOUND(A,1)
   N = UBOUND(A,2)
   
   ! A is of dimension M X N and B is of dimension N X 1. So, C is of dimension M X 1.
        
   DO i = 1, M
      temp = 0._RKIND
      DO j = 1, N
         temp = temp + A(i,j)*B(j)
      END DO
      C(i) = temp
   END DO

   END FUNCTION MatrixVectorMultiplication
   !-----------------------------------------------------------------------------------!
   !                                                                                   ! 
   !-----------------------------------------------------------------------------------!
   FUNCTION GivenTestFunction(xVector) RESULT(fVector)
   
   IMPLICIT NONE
   
   REAL(KIND=RKIND), INTENT(IN), DIMENSION(1:) :: xVector
   REAL(KIND=RKIND), DIMENSION(1:UBOUND(xVector,1)) :: fVector
   
   ! Local variables
   REAL(KIND=RKIND) :: x1, x2
   
   x1 = xVector(1)
   x2 = xVector(2)

   fVector(1) = x1**3._RKIND + x2 - 1._RKIND
   fVector(2) = x2**3._RKIND - x1 + 1._RKIND
   
   END FUNCTION GivenTestFunction
   !-----------------------------------------------------------------------------------!
   !                                                                                   ! 
   !-----------------------------------------------------------------------------------!
   FUNCTION GivenTestFunctionGradient(xVector) RESULT(GradFVector)
   
   IMPLICIT NONE
   
   REAL(KIND=RKIND), INTENT(IN), DIMENSION(1:) :: xVector
   REAL(KIND=RKIND), DIMENSION(1:UBOUND(xVector,1),1:UBOUND(xVector,1)) :: GradFVector
   
   ! Local variables
   REAL(KIND=RKIND) :: x1, x2
   
   x1 = xVector(1)
   x2 = xVector(2)
   
   GradFVector(1,1) = 3._RKIND*x1**2._RKIND
   GradFVector(1,2) = 1._RKIND
   GradFVector(2,1) = -1._RKIND
   GradFVector(2,2) = 3._RKIND*x2**2._RKIND  

   END FUNCTION GivenTestFunctionGradient
   !-----------------------------------------------------------------------------------!
   !                                                                                   !
   !-----------------------------------------------------------------------------------!
   SUBROUTINE NewtonRaphsonSolverForSystem(xVector,nIterationsNewtonRaphson, &
                                           NewtonRaphsonTolerance) 
   
   IMPLICIT NONE
   
   INTEGER, INTENT(IN) :: nIterationsNewtonRaphson
   REAL(KIND=RKIND), INTENT(IN) :: NewtonRaphsonTolerance
   REAL(KIND=RKIND), INTENT(INOUT), DIMENSION(1:) :: xVector
   
   ! Local variables
   INTEGER :: iIteration, nIterationsUptoConvergence
   REAL(KIND=RKIND), DIMENSION(1:UBOUND(xVector,1)) :: fVector, DeltaX
   REAL(KIND=RKIND), DIMENSION(1:UBOUND(xVector,1),1:UBOUND(xVector,1)) &
   :: GradFVector, GradFVectorInverse
   LOGICAL :: converged

   DO iIteration = 1, nIterationsNewtonRaphson    
      fVector = GivenTestFunction(xVector)
      GradFVector = GivenTestFunctionGradient(xVector)
      GradFVectorInverse = InverseMatrix(GradFVector)
      DeltaX = -MatrixVectorMultiplication(GradFVectorInverse,fVector)
!     DeltaX = -MATMUL(GradFVectorInverse,fVector)
      xVector = xVector + DeltaX
      IF (VectorNorm(DeltaX)/VectorNorm(xVector) <= NewtonRaphsonTolerance) THEN
!     IF (NORM2(DeltaX)/NORM2(xVector) <= NewtonRaphsonTolerance) THEN
         converged = .TRUE.
         nIterationsUptoConvergence = iIteration
         EXIT 
      END IF 
   END DO
   
   IF (converged) THEN 
      WRITE (*,'(A,F4.2,A,F4.2,A,I2,A)') &
      'The solution has converged to (', xVector(1), ', ', xVector(2), ') within ', &
      nIterationsUptoConvergence, ' iterations.'
   ELSE
      WRITE (*,'(A,I3,A)') 'The solution has not converged within ', &
      nIterationsNewtonRaphson, ' iterations.'
   END IF 
   
   END SUBROUTINE NewtonRaphsonSolverForSystem
   !-----------------------------------------------------------------------------------!
   !                                                                                   !
   !-----------------------------------------------------------------------------------!   
   SUBROUTINE TestNewtonRaphsonSolverForSystem()
   
   IMPLICIT NONE
   
   INTEGER, PARAMETER :: nIterationsNewtonRaphson = 100
   REAL(KIND=RKIND), PARAMETER :: NewtonRaphsonTolerance = 4._RKIND*EPSILON(1._RKIND)
   REAL(KIND=RKIND), DIMENSION(1:2) :: xVector
   
   WRITE (*,'(A)') ' '
   
   xVector = (/5._RKIND,5._RKIND/)
   
   CALL NewtonRaphsonSolverForSystem(xVector,nIterationsNewtonRaphson, &
                                     NewtonRaphsonTolerance)
   
   END SUBROUTINE TestNewtonRaphsonSolverForSystem   
   !-----------------------------------------------------------------------------------!
   !                                                                                   ! 
   !-----------------------------------------------------------------------------------!
   FUNCTION ShchepetkinShapeFunction(A0,tau0,tau,p,q,r) RESULT(A)
   
   IMPLICIT NONE
   
   REAL(KIND=RKIND), INTENT(IN) :: A0, tau0, tau, p, q, r
   REAL(KIND=RKIND) :: A
   
   A = A0*((tau/tau0)**p*(1._RKIND - (tau/tau0)**q) - r*(tau/tau0))

   END FUNCTION ShchepetkinShapeFunction
   !-----------------------------------------------------------------------------------!
   !                                                                                   ! 
   !-----------------------------------------------------------------------------------!
   FUNCTION ShchepetkinShapeFunctionDerivative(A0,tau0,tau,p,q,r) RESULT(A_tau)
   
   IMPLICIT NONE
   
   REAL(KIND=RKIND), INTENT(IN) :: A0, tau0, tau, p, q, r
   REAL(KIND=RKIND) :: A_tau
   
   A_tau = A0*(p*tau**(p - 1._RKIND)/tau0**p &
               - (p + q)*tau**(p + q - 1._RKIND)/tau0**(p + q) - r/tau0)

   END FUNCTION ShchepetkinShapeFunctionDerivative
   !-----------------------------------------------------------------------------------!
   !                                                                                   ! 
   !-----------------------------------------------------------------------------------!
   FUNCTION GivenFunction(p,q,xVector,ShchepetkinFilterType) RESULT(fVector)
   
   IMPLICIT NONE
   
   CHARACTER(LEN=*), INTENT(IN) :: ShchepetkinFilterType
   REAL(KIND=RKIND), INTENT(IN) :: p, q
   REAL(KIND=RKIND), INTENT(IN), DIMENSION(1:) :: xVector
   REAL(KIND=RKIND), DIMENSION(1:UBOUND(xVector,1)) :: fVector
   
   ! Local variables
   REAL(KIND=RKIND) :: tauStar, tau0, A0, r, ATauStar, I0, I1, I2, I3
   
   tauStar = xVector(1)
   tau0 = xVector(2)
   A0 = xVector(3)
   r = xVector(4)
   
   ATauStar = ShchepetkinShapeFunction(A0,tau0,tauStar,p,q,r)
   I0 = A0*(tauStar**(p+1._RKIND)/(tau0**p*(p+1._RKIND)) &
            - tauStar**(p+q+1._RKIND)/(tau0**(p+q)*(p+q+1._RKIND)) &
            - r*tauStar**2._RKIND/(2._RKIND*tau0))
   I1 = A0*(tauStar**(p+2._RKIND)/(tau0**p*(p+2._RKIND)) &
            - tauStar**(p+q+2._RKIND)/(tau0**(p+q)*(p+q+2._RKIND)) &
            - r*tauStar**3._RKIND/(3._RKIND*tau0))
   I2 = A0*(tauStar**(p+3._RKIND)/(tau0**p*(p+3._RKIND)) &
            - tauStar**(p+q+3._RKIND)/(tau0**(p+q)*(p+q+3._RKIND)) &
            - r*tauStar**4._RKIND/(4._RKIND*tau0))
   I3 = A0*(tauStar**(p+4._RKIND)/(tau0**p*(p+4._RKIND)) &
            - tauStar**(p+q+4._RKIND)/(tau0**(p+q)*(p+q+4._RKIND)) &
            - r*tauStar**5._RKIND/(5._RKIND*tau0))

   fVector(1) = ATauStar - 0._RKIND
   fVector(2) = I0 - 1._RKIND
   fVector(3) = I1 - 1._RKIND
   
   IF (ShchepetkinFilterType == 'SecondOrderAccurate') THEN
      fVector(4) = I2 - 1._RKIND
   ELSE ! IF (ShchepetkinFilterType == 'MinimalDispersion') THEN
      fVector(4) = I3 - 3._RKIND*I2 + 2._RKIND
   END IF 
   
   END FUNCTION GivenFunction
   !-----------------------------------------------------------------------------------!
   !                                                                                   ! 
   !-----------------------------------------------------------------------------------!
   FUNCTION GivenFunctionGradient(p,q,xVector,ShchepetkinFilterType) RESULT(GradFVector)
   
   IMPLICIT NONE
   
   CHARACTER(LEN=*), INTENT(IN) :: ShchepetkinFilterType
   REAL(KIND=RKIND), INTENT(IN) :: p, q
   REAL(KIND=RKIND), INTENT(IN), DIMENSION(1:) :: xVector
   REAL(KIND=RKIND), DIMENSION(1:UBOUND(xVector,1),1:UBOUND(xVector,1)) :: GradFVector
   
   ! Local variables
   REAL(KIND=RKIND) :: tauStar, tau0, A0, r, I2, I3
   REAL(KIND=RKIND), DIMENSION(1:UBOUND(xVector,1)) :: fVector, I2Prime, I3Prime
   
   fVector = GivenFunction(p,q,xVector,ShchepetkinFilterType)
   
   tauStar = xVector(1)
   tau0 = xVector(2)
   A0 = xVector(3)
   r = xVector(4)
   
   GradFVector(1,1) = A0*(p*tauStar**(p-1._RKIND)/tau0**p &
                          - (p+q)*tauStar**(p+q-1._RKIND)/tau0**(p+q) - r/tau0)
   GradFVector(1,2) = A0*(-p*tauStar**p/tau0**(p+1._RKIND) &
                          + (p+q)*tauStar**(p+q)/tau0**(p+q+1._RKIND) &
                          + r*tauStar/tau0**2._RKIND)
   GradFVector(1,3) = ShchepetkinShapeFunction(A0,tau0,tauStar,p,q,r)/A0
   GradFVector(1,4) = -A0*tauStar/tau0
   
   GradFVector(2,1) = ShchepetkinShapeFunction(A0,tau0,tauStar,p,q,r)
   GradFVector(2,2) = A0*(-p*tauStar**(p+1._RKIND)/(tau0**(p+1._RKIND)*(p+1._RKIND)) &
                          + (p+q)*tauStar**(p+q+1._RKIND)/(tau0**(p+q+1._RKIND) &
                            *(p+q+1._RKIND)) &
                          + r*tauStar**2._RKIND/(2._RKIND*tau0**2._RKIND))
   GradFVector(2,3) = tauStar**(p+1._RKIND)/(tau0**p*(p+1._RKIND)) &
                      - tauStar**(p+q+1._RKIND)/(tau0**(p+q)*(p+q+1._RKIND)) &
                      - r*tauStar**2._RKIND/(2._RKIND*tau0)         
   GradFVector(2,4) = -A0*tauStar**2._RKIND/(2._RKIND*tau0)

   GradFVector(3,1) = tauStar*ShchepetkinShapeFunction(A0,tau0,tauStar,p,q,r)
   GradFVector(3,2) = A0*(-p*tauStar**(p+2._RKIND)/(tau0**(p+1._RKIND)*(p+2._RKIND)) &
                    + (p+q)*tauStar**(p+q+2._RKIND)/(tau0**(p+q+1._RKIND) &
                                                     *(p+q+2._RKIND)) &
                    + r*tauStar**3._RKIND/(3._RKIND*tau0**2._RKIND))
   GradFVector(3,3) = tauStar**(p+2._RKIND)/(tau0**p*(p+2._RKIND)) &
                - tauStar**(p+q+2._RKIND)/(tau0**(p+q)*(p+q+2._RKIND)) &
                - r*tauStar**3._RKIND/(3._RKIND*tau0)             
   GradFVector(3,4) = -A0*tauStar**3._RKIND/(3._RKIND*tau0)
   
   I2 = A0*(tauStar**(p+3._RKIND)/(tau0**p*(p+3._RKIND)) &
            - tauStar**(p+q+3._RKIND)/(tau0**(p+q)*(p+q+3._RKIND)) &
            - r*tauStar**4._RKIND/(4._RKIND*tau0))
   I3 = A0*(tauStar**(p+4._RKIND)/(tau0**p*(p+4._RKIND)) &
            - tauStar**(p+q+4._RKIND)/(tau0**(p+q)*(p+q+4._RKIND)) &
            - r*tauStar**5._RKIND/(5._RKIND*tau0))
   
   I2Prime(1) = tauStar**2._RKIND*ShchepetkinShapeFunction(A0,tau0,tauStar,p,q,r)
   I2Prime(2) = A0*(-p*tauStar**(p+3._RKIND)/(tau0**(p+1._RKIND)*(p+3._RKIND)) &
                    + (p+q)*tauStar**(p+q+3._RKIND)/(tau0**(p+q+1._RKIND) &
                                                     *(p+q+3._RKIND)) &
                    + r*tauStar**4._RKIND/(4._RKIND*tau0**2._RKIND))
   I2Prime(3) = tauStar**(p+3._RKIND)/(tau0**p*(p+3._RKIND)) &
                - tauStar**(p+q+3._RKIND)/(tau0**(p+q)*(p+q+3._RKIND)) &
                - r*tauStar**4._RKIND/(4._RKIND*tau0)             
   I2Prime(4) = -A0*tauStar**4._RKIND/(4._RKIND*tau0)   
   
   I3Prime(1) = tauStar**3._RKIND*ShchepetkinShapeFunction(A0,tau0,tauStar,p,q,r)
   I3Prime(2) = A0*(-p*tauStar**(p+4._RKIND)/(tau0**(p+1._RKIND)*(p+4._RKIND)) &
                    + (p+q)*tauStar**(p+q+4._RKIND)/(tau0**(p+q+1._RKIND) &
                                                     *(p+q+4._RKIND)) &
                    + r*tauStar**5._RKIND/(5._RKIND*tau0**2._RKIND))
   I3Prime(3) = tauStar**(p+4._RKIND)/(tau0**p*(p+4._RKIND)) &
                - tauStar**(p+q+4._RKIND)/(tau0**(p+q)*(p+q+4._RKIND)) &
                - r*tauStar**5._RKIND/(5._RKIND*tau0)             
   I3Prime(4) = -A0*tauStar**5._RKIND/(5._RKIND*tau0)    
   
   IF (ShchepetkinFilterType == 'SecondOrderAccurate') THEN
      GradFVector(4,:) = I2Prime(:)
   ELSE ! IF (ShchepetkinFilterType == 'MinimalDispersion') THEN
      GradFVector(4,:) = I3Prime(:) - 3._RKIND*I2Prime(:)
   END IF 

   END FUNCTION GivenFunctionGradient
   !-----------------------------------------------------------------------------------!
   !                                                                                   !
   !-----------------------------------------------------------------------------------!
   SUBROUTINE NewtonRaphsonSolverForDeterminingShchepetkinFilterParameters &
   (p,q,ShchepetkinFilterType) 
   
   IMPLICIT NONE
   
   CHARACTER(LEN=*), INTENT(IN) :: ShchepetkinFilterType
   REAL(KIND=RKIND), INTENT(IN) :: p, q
   
   ! Local variables
   INTEGER, PARAMETER :: nIterationsNewtonRaphson = 10**6
   REAL(KIND=RKIND), PARAMETER :: NewtonRaphsonTolerance = 4._RKIND*EPSILON(1._RKIND)
   INTEGER :: iIteration, nIterationsUptoConvergence 
   LOGICAL :: converged
   REAL(KIND=RKIND) :: tauStar, tau0, A0, r
   REAL(KIND=RKIND), DIMENSION(1:4) :: xVector, fVector, DeltaX
   REAL(KIND=RKIND), DIMENSION(1:4,1:4) :: GradFVector

   IF (ShchepetkinFilterType == 'SecondOrderAccurate') THEN
      tauStar = 1.35_RKIND
   ELSE ! IF (ShchepetkinFilterType == 'MinimalDispersion') THEN
      tauStar = 1.49_RKIND
   END IF 

   tau0 = (p + 2._RKIND)*(p + q + 2._RKIND)/((p + 1._RKIND)*(p + q + 1._RKIND))
   A0 = 1._RKIND
   r = 0._RKIND
   
   ! This Newton-Raphson method is not ideal for solving this nonlinear system of 
   ! equations with the variables being The Shchepetkin filter parameters. This is 
   ! because the problem is not well-posed. It has multiple solutions. If the initial 
   ! guess is not close to our desired solution, the method may not converge at all and 
   ! even if it does, the converged solution will be different one. As a matter of fact, 
   ! the initial guess for tauStar should be chosen very carefully since it is the most
   ! sensitive one. For the second order accurate filter, the method only works for 
   ! initial guess of tauStar equal to 1.35 (not even 1.30 or 1.40). For the filter 
   ! optimized for minimal numerical dispersion, the method only works for initial guess
   ! of tauStar equal to 1.49 (not even 1.48 or 1.50).   
   
   xVector(1) = tauStar
   xVector(2) = tau0
   xVector(3) = A0
   xVector(4) = r
   
   converged = .FALSE.

   DO iIteration = 1, nIterationsNewtonRaphson    
      fVector = GivenFunction(p,q,xVector,ShchepetkinFilterType)
      GradFVector = GivenFunctionGradient(p,q,xVector,ShchepetkinFilterType)
      DeltaX = -MatrixVectorMultiplication(InverseMatrix(GradFVector),fVector)
!     DeltaX = -MATMUL(InverseMatrix(GradFVector),fVector)
      xVector = xVector + DeltaX
      IF (VectorNorm(DeltaX)/VectorNorm(xVector) <= NewtonRaphsonTolerance) THEN
!     IF (NORM2(DeltaX)/NORM2(xVector) <= NewtonRaphsonTolerance) THEN
         converged = .TRUE. 
         nIterationsUptoConvergence = iIteration
         EXIT 
      END IF 
   END DO
   
   IF (converged) THEN
      WRITE (*,'(2A,I7,A)') 'The numerical solution for the Shchepetkin filter ', &
                            'parameters has converged within ', &
                            nIterationsUptoConvergence, ' iterations.' 
      WRITE (*,'(A)') 'They are as follows:'
      WRITE (*,'(A,F8.6)') 'tauStar = ', xVector(1)
      WRITE (*,'(A,F8.6)') 'tau0 = ', xVector(2)
      WRITE (*,'(A,F9.6)') 'A0 = ', xVector(3)
      WRITE (*,'(A,F8.6)') 'r = ', xVector(4)
   ELSE
      WRITE (*,'(2A,I7,A)') 'The numerical solution for the Shchepetkin filter ', &
                            'parameters has not converged within ', &
                            nIterationsNewtonRaphson, ' iterations.' 
      WRITE (*,'(A)') 'The final values of the parameters are as follows:'
      WRITE (*,'(A,F8.6)') 'tauStar = ', xVector(1)
      WRITE (*,'(A,F8.6)') 'tau0 = ', xVector(2)
      WRITE (*,'(A,F9.6)') 'A0 = ', xVector(3)
      WRITE (*,'(A,F8.6)') 'r = ', xVector(4)   
   END IF 

   END SUBROUTINE NewtonRaphsonSolverForDeterminingShchepetkinFilterParameters
   !-----------------------------------------------------------------------------------!
   !                                                                                   !
   !-----------------------------------------------------------------------------------!
   SUBROUTINE RunNewtonRaphsonSolverForDeterminingShchepetkinFilterParameters()
   
   IMPLICIT NONE
   
   CHARACTER(LEN=20) :: ShchepetkinFilterType
   REAL(KIND=RKIND) :: p, q
   
   WRITE (*,'(A)') ' '
   
   p = 2._RKIND
   q = 2._RKIND
   ShchepetkinFilterType = 'SecondOrderAccurate'
      
   CALL NewtonRaphsonSolverForDeterminingShchepetkinFilterParameters &
   (p,q,ShchepetkinFilterType)
   
   WRITE (*,'(A)') ' '
   
   p = 2._RKIND
   q = 2._RKIND
   ShchepetkinFilterType = 'MinimalDispersion'
      
   CALL NewtonRaphsonSolverForDeterminingShchepetkinFilterParameters &
   (p,q,ShchepetkinFilterType)   
   
   WRITE (*,'(A)') ' '

   END SUBROUTINE RunNewtonRaphsonSolverForDeterminingShchepetkinFilterParameters
   !-----------------------------------------------------------------------------------!
   !                                                                                   !
   !-----------------------------------------------------------------------------------!
   SUBROUTINE NewtonRaphsonSolverForDeterminingShchepetkinFilterEndPoint &
   (A0,tau0,p,q,r,tau,printResult,converged) 
   
   IMPLICIT NONE
   
   REAL(KIND=RKIND), INTENT(IN) :: A0, tau0, p, q, r
   REAL(KIND=RKIND), INTENT(INOUT) :: tau
   LOGICAL, INTENT(IN) :: printResult
   LOGICAL, INTENT(OUT) :: converged
   
   ! Local variables
   INTEGER, PARAMETER :: nIterationsNewtonRaphson = 10**2
   REAL(KIND=RKIND), PARAMETER :: NewtonRaphsonTolerance = 4._RKIND*EPSILON(1._RKIND)
   INTEGER :: iIteration, nIterationsUptoConvergence
   REAL(KIND=RKIND) :: FunctionValue, DerivativeValue, Delta
   
   converged = .FALSE. 
  
   DO iIteration = 1, nIterationsNewtonRaphson      
      FunctionValue = ShchepetkinShapeFunction(A0,tau0,tau,p,q,r)
      DerivativeValue = ShchepetkinShapeFunctionDerivative(A0,tau0,tau,p,q,r)
      Delta = -FunctionValue/DerivativeValue
      tau = tau + Delta
      IF (ABS(Delta)/ABS(tau) <= NewtonRaphsonTolerance) THEN
         converged = .TRUE.
         nIterationsUptoConvergence = iIteration
         EXIT 
      END IF 
   END DO
   
   IF (printResult) THEN
      IF (converged) THEN 
         WRITE (*,'(2A,F8.6,A,I3,A)') &
         'The numerical solution for the Shchepetkin filter endpoint has converged ', &
         'to ', tau, ' within ', nIterationsUptoConvergence, ' iterations.'
      ELSE
         WRITE (*,'(2A,I3,A)') &
         'The numerical solution for the Shchepetkin filter endpoint has not ', &
         'converged within ', nIterationsNewtonRaphson, ' iterations.'      
      END IF 
   
   END IF 
   
   END SUBROUTINE NewtonRaphsonSolverForDeterminingShchepetkinFilterEndPoint
   !-----------------------------------------------------------------------------------!
   !                                                                                   !
   !-----------------------------------------------------------------------------------!
   SUBROUTINE TestNewtonRaphsonSolverForDeterminingShchepetkinFilterEndPoint(p,q)
   
   IMPLICIT NONE
   
   REAL(KIND=RKIND), INTENT(IN) :: p, q
   
   ! Local variables
   LOGICAL :: printResult, converged
   REAL(KIND=RKIND) :: A0, tau0, r, tauStar 
   
   A0 = 1._RKIND
   tau0 = (p + 2._RKIND)*(p + q + 2._RKIND)/((p + 1._RKIND)*(p + q + 1._RKIND))
   r = 0.25_RKIND  
   tauStar = 1.5_RKIND
   printResult = .TRUE.
   
   CALL NewtonRaphsonSolverForDeterminingShchepetkinFilterEndPoint &
   (A0,tau0,p,q,r,tauStar,printResult,converged)
   
   WRITE (*,'(A)') ' '
   
   END SUBROUTINE TestNewtonRaphsonSolverForDeterminingShchepetkinFilterEndPoint
   !-----------------------------------------------------------------------------------!
   !                                                                                   ! 
   !-----------------------------------------------------------------------------------!
   SUBROUTINE &
   ShchepetkinFilterParameters_SecondOrderAccurate(p,q, &
                                                   DisplayOutputAtEachIterationLevel, &
                                                   tauStar,tau0,A0,r)
   
   IMPLICIT NONE
   
   LOGICAL, INTENT(IN) :: DisplayOutputAtEachIterationLevel
   REAL(KIND=RKIND), INTENT(IN) :: p, q
   REAL(KIND=RKIND), INTENT(OUT) :: tauStar, tau0, A0, r
   
   ! Local variables
   INTEGER, PARAMETER :: nIterationsNewtonRaphson = 10**3
   REAL(KIND=RKIND), PARAMETER :: NewtonRaphsonTolerance = 4._RKIND*EPSILON(1._RKIND)
   INTEGER :: nIterationsUptoConvergence, iIteration, jIteration, &
              nIterationsTau0UptoConvergence
   LOGICAL :: convergedTauStar, convergedTau0, converged
   REAL(KIND=RKIND) :: rLast, LHSFactor, FunctionValueTau0, DerivativeValueTau0, &
                    DeltaTau0, tau0Last, DeltaR
   
   tauStar = 2._RKIND
   tau0 = (p + 2._RKIND)*(p + q + 2._RKIND)/((p + 1._RKIND)*(p + q + 1._RKIND))
   A0 = 1._RKIND
   r = 0._RKIND   
   rLast = r
   
   converged = .FALSE.
   
   DO iIteration = 1, nIterationsNewtonRaphson
      CALL NewtonRaphsonSolverForDeterminingShchepetkinFilterEndPoint &
           (A0,tau0,p,q,r,tauStar,.FALSE.,convergedTauStar)
      LHSFactor = A0*(tauStar**(p+1._RKIND)/(tau0**p*(p+1._RKIND)) &
                      - tauStar**(p+q+1._RKIND)/(tau0**(p+q)*(p+q+1._RKIND)) &
                      - r*tauStar**2._RKIND/(2._RKIND*tau0))
      A0 = A0/LHSFactor
      convergedTau0 = .FALSE.
      DO jIteration = 1, nIterationsNewtonRaphson
         FunctionValueTau0 &
         = A0*(tauStar**(p+2._RKIND)/(tau0**p*(p+2._RKIND)) &
               - tauStar**(p+q+2._RKIND)/(tau0**(p+q)*(p+q+2._RKIND)) &
               - r*tauStar**3._RKIND/(3._RKIND*tau0)) - 1._RKIND
         DerivativeValueTau0 &
         = A0*(-p*tauStar**(p+2._RKIND)/(tau0**(p+1._RKIND)*(p+2._RKIND)) &
               + (p+q)*tauStar**(p+q+2._RKIND)/(tau0**(p+q+1._RKIND)*(p+q+2._RKIND)) &
               + r*tauStar**3._RKIND/(3._RKIND*tau0**2._RKIND))
         DeltaTau0 = -FunctionValueTau0/DerivativeValueTau0
         tau0 = tau0 + DeltaTau0
         IF (ABS(DeltaTau0)/ABS(tau0) <= NewtonRaphsonTolerance) THEN
            convergedTau0 = .TRUE.
            nIterationsTau0UptoConvergence = jIteration 
            EXIT 
         END IF     
         tau0Last = tau0      
      END DO      
      r = -(4._RKIND*tau0/tauStar**4._RKIND) &
           *(1._RKIND/A0 - tauStar**(p+3._RKIND)/(tau0**p*(p+3._RKIND)) &
             + tauStar**(p+q+3._RKIND)/(tau0**(p+q)*(p+q+3._RKIND)))
      DeltaTau0 = tau0 - tau0Last
      DeltaR = r - rLast
      IF (MAX(ABS(DeltaTau0)/ABS(tau0),ABS(DeltaR)/ABS(r)) <= NewtonRaphsonTolerance) &
      THEN 
         converged = .TRUE.
         nIterationsUptoConvergence = iIteration 
         EXIT 
      END IF 
      IF (DisplayOutputAtEachIterationLevel) THEN
         WRITE (*,'(A,I3,A,4F12.5,A)') &
         'The Shchepetkin filter parameters at iteration level ', iIteration, &
         ' are ', tauStar, tau0, A0, r, '.'
      END IF
      rLast = r      
   END DO 
   IF (converged) THEN  
      WRITE (*,'(2A,I4,A)') &
      'The numerical solution for the Shchepetkin filter parameters has converged ', &
      'within ', nIterationsUptoConvergence, ' iterations.'
      WRITE (*,'(A)') 'They are as follows:'
      WRITE (*,'(A,F8.6)') 'tauStar = ', tauStar
      WRITE (*,'(A,F8.6)') 'tau0 = ', tau0
      WRITE (*,'(A,F9.6)') 'A0 = ', A0
      WRITE (*,'(A,F8.6)') 'r = ', r   
   ELSE 
      WRITE (*,'(2A,I4,A)') &
      'The numerical solution for the Shchepetkin filter parameters has not ', &
      'converged within ', nIterationsNewtonRaphson, ' iterations.'
      WRITE (*,'(A)') 'The final values of the parameters are as follows:'
      WRITE (*,'(A,F8.6)') 'tauStar = ', tauStar
      WRITE (*,'(A,F8.6)') 'tau0 = ', tau0
      WRITE (*,'(A,F9.6)') 'A0 = ', A0
      WRITE (*,'(A,F8.6)') 'r = ', r  
   END IF 
   
   END SUBROUTINE ShchepetkinFilterParameters_SecondOrderAccurate  
   !-----------------------------------------------------------------------------------!
   !                                                                                   ! 
   !-----------------------------------------------------------------------------------!
   SUBROUTINE DetermineShchepetkinFilterParameters_SecondOrderAccurate()
   
   IMPLICIT NONE
   
   LOGICAL :: DisplayOutputAtEachIterationLevel
   REAL(KIND=RKIND) :: p, q, tauStar, tau0, A0, r
   
   p = 2._RKIND
   q = 2._RKIND
   DisplayOutputAtEachIterationLevel = .FALSE.
   
   CALL ShchepetkinFilterParameters_SecondOrderAccurate &
        (p,q, DisplayOutputAtEachIterationLevel,tauStar,tau0,A0,r)
        
   WRITE (*,'(A)') ' '
   
   END SUBROUTINE DetermineShchepetkinFilterParameters_SecondOrderAccurate
   !-----------------------------------------------------------------------------------!
   !                                                                                   ! 
   !-----------------------------------------------------------------------------------!
   SUBROUTINE &
   ShchepetkinFilterParameters_MinimalDispersion(p,q, &
                                                 DisplayOutputAtEachIterationLevel, &
                                                 tauStar,tau0,A0,r)
   
   IMPLICIT NONE
   
   LOGICAL, INTENT(IN) :: DisplayOutputAtEachIterationLevel
   REAL(KIND=RKIND), INTENT(IN) :: p, q
   REAL(KIND=RKIND), INTENT(OUT) :: tauStar, tau0, A0, r
   
   ! Local variables
   INTEGER, PARAMETER :: nIterationsNewtonRaphson = 10**4
   ! It is observed that the iterative method for determining the Shchepetkin parameters
   ! does not converge to our desired tolerance, close to machine precision. However, it
   ! does converge to a solution very close to the one obtained by Shchepetkin. But the 
   ! residual approximated by max(abs(DeltaTau0)/abs(tau0),abs(DeltaR)/abs(r))
   ! is ~O(10._RKIND**(-8._RKIND)) after 1000 iterations and ~O(10._RKIND**(-15._RKIND))
   ! after 10000 iterations. That is why we choose nIterationsNewtonRaphson to be 10**4,
   ! not 10**3.
   REAL(KIND=RKIND), PARAMETER :: NewtonRaphsonTolerance = 4._RKIND*EPSILON(1._RKIND)
   INTEGER :: nIterationsUptoConvergence, iIteration, jIteration, &
              nIterationsTau0UptoConvergence
   LOGICAL :: convergedTauStar, convergedTau0, converged
   REAL(KIND=RKIND) :: rLast, LHSFactor, FunctionValueTau0, DerivativeValueTau0, &
                    DeltaTau0, tau0Last, LHS, RHS, DeltaR
   
   tauStar = 1.5_RKIND
   tau0 = (p + 2._RKIND)*(p + q + 2._RKIND)/((p + 1._RKIND)*(p + q + 1._RKIND))
   A0 = 1._RKIND
   r = 0._RKIND   
   rLast = r
   
   converged = .FALSE.
   
   DO iIteration = 1, nIterationsNewtonRaphson
      CALL NewtonRaphsonSolverForDeterminingShchepetkinFilterEndPoint &
           (A0,tau0,p,q,r,tauStar,.FALSE.,convergedTauStar)
      LHSFactor = A0*(tauStar**(p+1._RKIND)/(tau0**p*(p+1._RKIND)) &
                      - tauStar**(p+q+1._RKIND)/(tau0**(p+q)*(p+q+1._RKIND)) &
                      - r*tauStar**2._RKIND/(2._RKIND*tau0))
      A0 = A0/LHSFactor
      convergedTau0 = .FALSE.
      DO jIteration = 1, nIterationsNewtonRaphson
         FunctionValueTau0 &
         = A0*(tauStar**(p+2._RKIND)/(tau0**p*(p+2._RKIND)) &
               - tauStar**(p+q+2._RKIND)/(tau0**(p+q)*(p+q+2._RKIND)) &
               - r*tauStar**3._RKIND/(3._RKIND*tau0)) - 1._RKIND
         DerivativeValueTau0 &
         = A0*(-p*tauStar**(p+2._RKIND)/(tau0**(p+1._RKIND)*(p+2._RKIND)) &
               + (p+q)*tauStar**(p+q+2._RKIND)/(tau0**(p+q+1._RKIND)*(p+q+2._RKIND)) &
               + r*tauStar**3._RKIND/(3._RKIND*tau0**2._RKIND))
         DeltaTau0 = -FunctionValueTau0/DerivativeValueTau0
         tau0 = tau0 + DeltaTau0
         IF (ABS(DeltaTau0)/ABS(tau0) <= NewtonRaphsonTolerance) THEN
            convergedTau0 = .TRUE.
            nIterationsTau0UptoConvergence = jIteration 
            EXIT 
         END IF     
         tau0Last = tau0      
      END DO   
      LHS = A0*(-tauStar**5._RKIND/(5._RKIND*tau0) &
                + 3._RKIND*tauStar**4._RKIND/(4._RKIND*tau0))
      RHS = 3._RKIND*A0*(tauStar**(p+3._RKIND)/(tau0**p*(p+3._RKIND)) &
            - tauStar**(p+q+3._RKIND)/(tau0**(p+q)*(p+q+3._RKIND))) &
            - A0*(tauStar**(p+4._RKIND)/(tau0**p*(p+4._RKIND)) &
            - tauStar**(p+q+4._RKIND)/(tau0**(p+q)*(p+q+4._RKIND))) - 2._RKIND
      r = RHS/LHS      
      DeltaTau0 = tau0 - tau0Last
      DeltaR = r - rLast
      IF (MAX(ABS(DeltaTau0)/ABS(tau0),ABS(DeltaR)/ABS(r)) <= NewtonRaphsonTolerance) &
      THEN 
         converged = .TRUE.       
         nIterationsUptoConvergence = iIteration
         EXIT 
      END IF 
      IF (DisplayOutputAtEachIterationLevel) THEN
         WRITE (*,'(A,I3,A,4F12.5,A)') &
         'The Shchepetkin filter parameters at iteration level ', iIteration, &
         ' are ', tauStar, tau0, A0, r, '.'
      END IF
      rLast = r      
   END DO 
   IF (converged) THEN  
      WRITE (*,'(2A,I4,A)') &
      'The numerical solution for the Shchepetkin filter parameters has converged ', &
      'within ', nIterationsUptoConvergence, ' iterations.'
      WRITE (*,'(A)') 'They are as follows:'
      WRITE (*,'(A,F8.6)') 'tauStar = ', tauStar
      WRITE (*,'(A,F8.6)') 'tau0 = ', tau0
      WRITE (*,'(A,F9.6)') 'A0 = ', A0
      WRITE (*,'(A,F8.6)') 'r = ', r   
   ELSE 
      WRITE (*,'(2A,I5,A)') &
      'The numerical solution for the Shchepetkin filter parameters has not ', &
      'converged within ', nIterationsNewtonRaphson, ' iterations.'
      WRITE (*,'(A)') 'The final values of the parameters are as follows:'
      WRITE (*,'(A,F8.6)') 'tauStar = ', tauStar
      WRITE (*,'(A,F8.6)') 'tau0 = ', tau0
      WRITE (*,'(A,F9.6)') 'A0 = ', A0
      WRITE (*,'(A,F8.6)') 'r = ', r  
   END IF 
   
   END SUBROUTINE ShchepetkinFilterParameters_MinimalDispersion  
   !-----------------------------------------------------------------------------------!
   !                                                                                   ! 
   !-----------------------------------------------------------------------------------!
   SUBROUTINE DetermineShchepetkinFilterParameters_MinimalDispersion()
   
   IMPLICIT NONE
   
   LOGICAL :: DisplayOutputAtEachIterationLevel
   REAL(KIND=RKIND) :: p, q, tauStar, tau0, A0, r
   
   p = 2._RKIND
   q = 2._RKIND
   DisplayOutputAtEachIterationLevel = .FALSE.
   
   CALL ShchepetkinFilterParameters_MinimalDispersion &
        (p,q, DisplayOutputAtEachIterationLevel,tauStar,tau0,A0,r)
        
   WRITE (*,'(A)') ' '
   
   END SUBROUTINE DetermineShchepetkinFilterParameters_MinimalDispersion
   !-----------------------------------------------------------------------------------!
   !                                                                                   ! 
   !-----------------------------------------------------------------------------------!
   SUBROUTINE DetermineFilterWeights(FilterType,RectangularFilterRange,pShchepetkin, &
                                     qShchepetkin,nBarotropicTimeSteps,tau, &
                                     PrimaryWeights,SecondaryWeights)
   
   IMPLICIT NONE
   
   CHARACTER(LEN=*), INTENT(IN) :: FilterType
   INTEGER, INTENT(IN) :: nBarotropicTimeSteps
   REAL(KIND=RKIND), INTENT(IN) :: RectangularFilterRange, pShchepetkin, qShchepetkin
   REAL(KIND=RKIND), INTENT(OUT), DIMENSION(1:2*nBarotropicTimeSteps) &
   :: tau, PrimaryWeights, SecondaryWeights
   
   ! Local variables
   REAL(KIND=RKIND), PARAMETER :: PI = 4._RKIND*ATAN(1._RKIND)
   INTEGER :: iBarotropicTimeStep, jBarotropicTimeStep
   REAL(KIND=RKIND) :: tauStar, tau0, A0, r
   
   PrimaryWeights = 0._RKIND
   SecondaryWeights = 0._RKIND
   
   DO iBarotropicTimeStep = 1, 2*nBarotropicTimeSteps
      tau(iBarotropicTimeStep) &
      = REAL(iBarotropicTimeStep,KIND=RKIND)/REAL(nBarotropicTimeSteps,KIND=RKIND)
   END DO 
   
   IF (FilterType == 'rectangular') THEN
      DO iBarotropicTimeStep = 1, 2*nBarotropicTimeSteps
         IF (tau(iBarotropicTimeStep) > (1._RKIND - RectangularFilterRange) .AND. &
             tau(iBarotropicTimeStep) <= (1._RKIND + RectangularFilterRange)) THEN
            PrimaryWeights(iBarotropicTimeStep) &
            = 1._RKIND/(2._RKIND*RectangularFilterRange &
                        *REAL(nBarotropicTimeSteps,KIND=RKIND))
         END IF 
      END DO           
   ELSE IF (FilterType == 'ROMS_cosine') THEN
      DO iBarotropicTimeStep = 1, 2*nBarotropicTimeSteps
         IF (tau(iBarotropicTimeStep) > 0.5_RKIND .AND. &
             tau(iBarotropicTimeStep) <= 1.5_RKIND) THEN 
            PrimaryWeights(iBarotropicTimeStep) &
            = 1._RKIND + COS(2._RKIND*PI*(tau(iBarotropicTimeStep) - 1._RKIND))
         END IF 
      END DO  
      PrimaryWeights = PrimaryWeights/REAL(nBarotropicTimeSteps,KIND=RKIND)            
   ELSE IF (FilterType == 'Hamming_window_cosine') THEN
      DO iBarotropicTimeStep = 1, 2*nBarotropicTimeSteps
         IF (tau(iBarotropicTimeStep) > 0.5_RKIND .AND. &
             tau(iBarotropicTimeStep) <= 1.5_RKIND) THEN 
            PrimaryWeights(iBarotropicTimeStep) &
            = 1._RKIND + 0.85_RKIND*COS(2._RKIND*PI*(tau(iBarotropicTimeStep) &
                                                     - 1._RKIND))
         END IF 
      END DO  
      PrimaryWeights = PrimaryWeights/REAL(nBarotropicTimeSteps,KIND=RKIND) 
   ELSE IF (FilterType == 'Shchepetkin_second_order_accurate') THEN
      CALL ShchepetkinFilterParameters_SecondOrderAccurate &
           (pShchepetkin,qShchepetkin,.FALSE.,tauStar,tau0,A0,r)
      DO iBarotropicTimeStep = 1, 2*nBarotropicTimeSteps
         PrimaryWeights(iBarotropicTimeStep) &
         = ShchepetkinShapeFunction(A0,tau0,tau(iBarotropicTimeStep),pShchepetkin, &
                                    qShchepetkin,r)
         IF (tau(iBarotropicTimeStep) > 1._RKIND .AND. &
             PrimaryWeights(iBarotropicTimeStep) < 0._RKIND) THEN 
            PrimaryWeights(iBarotropicTimeStep) = 0._RKIND
         END IF 
      END DO  
      PrimaryWeights = PrimaryWeights/REAL(nBarotropicTimeSteps,KIND=RKIND) 
   ELSE IF (FilterType == 'Shchepetkin_minimal_dispersion') THEN
      CALL ShchepetkinFilterParameters_MinimalDispersion &
           (pShchepetkin,qShchepetkin,.FALSE.,tauStar,tau0,A0,r)
      DO iBarotropicTimeStep = 1, 2*nBarotropicTimeSteps
         PrimaryWeights(iBarotropicTimeStep) &
         = ShchepetkinShapeFunction(A0,tau0,tau(iBarotropicTimeStep),pShchepetkin, &
                                    qShchepetkin,r)
         IF (tau(iBarotropicTimeStep) > 1._RKIND .AND. &
             PrimaryWeights(iBarotropicTimeStep) < 0._RKIND) THEN 
            PrimaryWeights(iBarotropicTimeStep) = 0._RKIND
         END IF 
      END DO  
      PrimaryWeights = PrimaryWeights/REAL(nBarotropicTimeSteps,KIND=RKIND) 
   END IF 

   DO iBarotropicTimeStep = 1, 2*nBarotropicTimeSteps 
      DO jBarotropicTimeStep = iBarotropicTimeStep, 2*nBarotropicTimeSteps
         SecondaryWeights(iBarotropicTimeStep) &
         = SecondaryWeights(iBarotropicTimeStep) + PrimaryWeights(jBarotropicTimeStep)
      END DO 
      SecondaryWeights(iBarotropicTimeStep) &
      = SecondaryWeights(iBarotropicTimeStep)/REAL(nBarotropicTimeSteps,KIND=RKIND)   
   END DO 
   
   END SUBROUTINE DetermineFilterWeights
   !-----------------------------------------------------------------------------------!
   !                                                                                   ! 
   !-----------------------------------------------------------------------------------!
   SUBROUTINE RectangularFilter(RectangularFilterRange)
   
   IMPLICIT NONE
   
   REAL(KIND=RKIND), INTENT(IN) :: RectangularFilterRange
   
   ! Local variables   
   CHARACTER(LEN=50) :: FilterType, Filename
   INTEGER :: nBarotropicTimeSteps
   REAL(KIND=RKIND), ALLOCATABLE, DIMENSION(:) :: tau, PrimaryWeights, SecondaryWeights   
   
   FilterType = 'rectangular'
   nBarotropicTimeSteps = 30
   
   ALLOCATE (tau(1:2*nBarotropicTimeSteps))
   ALLOCATE (PrimaryWeights(1:2*nBarotropicTimeSteps))
   ALLOCATE (SecondaryWeights(1:2*nBarotropicTimeSteps))
   
   CALL DetermineFilterWeights(FilterType,RectangularFilterRange,2._RKIND,2._RKIND, &
                               nBarotropicTimeSteps,tau,PrimaryWeights,SecondaryWeights)
                            
   WRITE (*,'(A,F4.2,A,F8.6,A)') &
   'The sum of all primary weights of the rectangular filter with range ', &
   RectangularFilterRange, ' is ', SUM(PrimaryWeights), '.'
   
   WRITE (*,'(A,F4.2,A,F9.6,A)') &
   'The sum of all secondary weights of the rectangular filter with range ', &
   RectangularFilterRange, ' is ', SUM(SecondaryWeights), '.'
   
   WRITE (*,'(A)') ' '
   
   WRITE (Filename,'(A,F3.1,A)') &
   'RectangularFilter_Range', RectangularFilterRange, '_PrimaryWeights'
   CALL WriteTecplot1D(tau,PrimaryWeights,Filename)
   
   WRITE (Filename,'(A,F3.1,A)') &
   'RectangularFilter_Range', RectangularFilterRange, '_SecondaryWeights'
   CALL WriteTecplot1D(tau,SecondaryWeights,Filename)
      
   DEALLOCATE (tau)
   DEALLOCATE (PrimaryWeights)
   DEALLOCATE (SecondaryWeights)
   
   END SUBROUTINE RectangularFilter
   !-----------------------------------------------------------------------------------!
   !                                                                                   ! 
   !-----------------------------------------------------------------------------------!
   SUBROUTINE ROMSCosineFilter()
   
   IMPLICIT NONE
 
   CHARACTER(LEN=40) :: FilterType
   INTEGER :: nBarotropicTimeSteps
   REAL(KIND=RKIND), ALLOCATABLE, DIMENSION(:) :: tau, PrimaryWeights, SecondaryWeights   
   
   FilterType = 'ROMS_cosine'
   nBarotropicTimeSteps = 30
   
   ALLOCATE (tau(1:2*nBarotropicTimeSteps))
   ALLOCATE (PrimaryWeights(1:2*nBarotropicTimeSteps))
   ALLOCATE (SecondaryWeights(1:2*nBarotropicTimeSteps))
   
   CALL DetermineFilterWeights(FilterType,0.5_RKIND,2._RKIND,2._RKIND, &
                               nBarotropicTimeSteps,tau,PrimaryWeights,SecondaryWeights)
                            
   WRITE (*,'(A,F8.6,A)') &
   'The sum of all primary weights of the ROMS cosine filter is ', &
   SUM(PrimaryWeights), '.'
   
   WRITE (*,'(A,F9.6,A)') &
   'The sum of all secondary weights of the ROMS cosine filter is ', &
   SUM(SecondaryWeights), '.'
   
   WRITE (*,'(A)') ' '
   
   CALL WriteTecplot1D(tau,PrimaryWeights,'ROMSCosineFilterPrimaryWeights')
   CALL WriteTecplot1D(tau,SecondaryWeights,'ROMSCosineFilterSecondaryWeights')
   
   DEALLOCATE (tau)
   DEALLOCATE (PrimaryWeights)
   DEALLOCATE (SecondaryWeights)
   
   END SUBROUTINE ROMSCosineFilter
   !-----------------------------------------------------------------------------------!
   !                                                                                   ! 
   !-----------------------------------------------------------------------------------!
   SUBROUTINE HammingWindowCosineFilter()
   
   IMPLICIT NONE
 
   CHARACTER(LEN=40) :: FilterType
   INTEGER :: nBarotropicTimeSteps
   REAL(KIND=RKIND), ALLOCATABLE, DIMENSION(:) :: tau, PrimaryWeights, SecondaryWeights   
   
   FilterType = 'Hamming_window_cosine'
   nBarotropicTimeSteps = 30
   
   ALLOCATE (tau(1:2*nBarotropicTimeSteps))
   ALLOCATE (PrimaryWeights(1:2*nBarotropicTimeSteps))
   ALLOCATE (SecondaryWeights(1:2*nBarotropicTimeSteps))
   
   CALL DetermineFilterWeights(FilterType,0.5_RKIND,2._RKIND,2._RKIND, &
                               nBarotropicTimeSteps,tau,PrimaryWeights,SecondaryWeights)
                            
   WRITE (*,'(A,F9.6,A)') &
   'The sum of all primary weights of the Hamming window cosine filter is ', &
   SUM(PrimaryWeights), '.'
   
   WRITE (*,'(A,F9.6,A)') &
   'The sum of all secondary weights of the Hamming window cosine filter is ', &
   SUM(SecondaryWeights), '.'
   
   WRITE (*,'(A)') ' '
   
   CALL WriteTecplot1D(tau,PrimaryWeights,'HammingWindowCosineFilterPrimaryWeights')
   CALL WriteTecplot1D(tau,SecondaryWeights,'HammingWindowCosineFilterSecondaryWeights')
   
   DEALLOCATE (tau)
   DEALLOCATE (PrimaryWeights)
   DEALLOCATE (SecondaryWeights)
   
   END SUBROUTINE HammingWindowCosineFilter
   !-----------------------------------------------------------------------------------!
   !                                                                                   ! 
   !-----------------------------------------------------------------------------------!
   SUBROUTINE ShchepetkinFilter_SecondOrderAccurate(pShchepetkin,qShchepetkin)
   
   IMPLICIT NONE
   
   REAL(KIND=RKIND), INTENT(IN) :: pShchepetkin, qShchepetkin
   
   ! Local variables
   CHARACTER(LEN=40) :: FilterType
   INTEGER :: nBarotropicTimeSteps
   REAL(KIND=RKIND), ALLOCATABLE, DIMENSION(:) :: tau, PrimaryWeights, SecondaryWeights   
   
   FilterType = 'Shchepetkin_second_order_accurate'
   nBarotropicTimeSteps = 30
   
   ALLOCATE (tau(1:2*nBarotropicTimeSteps))
   ALLOCATE (PrimaryWeights(1:2*nBarotropicTimeSteps))
   ALLOCATE (SecondaryWeights(1:2*nBarotropicTimeSteps))
   
   CALL DetermineFilterWeights(FilterType,0.5_RKIND,pShchepetkin,qShchepetkin, &
                               nBarotropicTimeSteps,tau,PrimaryWeights,SecondaryWeights)
                            
   WRITE (*,'(2A,F8.6,A)') 'The sum of all primary weights of the second order ', &
   'accurate Shchepetkin filter is ', SUM(PrimaryWeights), '.'
   
   WRITE (*,'(2A,F9.6,A)') 'The sum of all secondary weights of the second order ', &
   'accurate Shchepetkin filter is ', SUM(SecondaryWeights), '.'
   
   WRITE (*,'(A)') ' '
   
   CALL WriteTecplot1D(tau,PrimaryWeights, &
                       'ShchepetkinFilter_SecondOrderAccurate_PrimaryWeights')
   CALL WriteTecplot1D(tau,SecondaryWeights, &
                       'ShchepetkinFilter_SecondOrderAccurate_SecondaryWeights')
   
   DEALLOCATE (tau)
   DEALLOCATE (PrimaryWeights)
   DEALLOCATE (SecondaryWeights)
   
   END SUBROUTINE ShchepetkinFilter_SecondOrderAccurate
   !-----------------------------------------------------------------------------------!
   !                                                                                   ! 
   !-----------------------------------------------------------------------------------!
   SUBROUTINE ShchepetkinFilter_MinimalDispersion(pShchepetkin,qShchepetkin)
   
   IMPLICIT NONE
   
   REAL(KIND=RKIND), INTENT(IN) :: pShchepetkin, qShchepetkin
   
   ! Local variables
   CHARACTER(LEN=40) :: FilterType
   INTEGER :: nBarotropicTimeSteps
   REAL(KIND=RKIND), ALLOCATABLE, DIMENSION(:) :: tau, PrimaryWeights, SecondaryWeights   
   
   FilterType = 'Shchepetkin_minimal_dispersion'
   nBarotropicTimeSteps = 30
   
   ALLOCATE (tau(1:2*nBarotropicTimeSteps))
   ALLOCATE (PrimaryWeights(1:2*nBarotropicTimeSteps))
   ALLOCATE (SecondaryWeights(1:2*nBarotropicTimeSteps))
   
   CALL DetermineFilterWeights(FilterType,0.5_RKIND,pShchepetkin,qShchepetkin, &
                               nBarotropicTimeSteps,tau,PrimaryWeights,SecondaryWeights)
                            
   WRITE (*,'(2A,F8.6,A)') 'The sum of all primary weights of the Shchepetkin ', &
   'filter optimized for minimal dispersion is ', SUM(PrimaryWeights), '.'
   
   WRITE (*,'(2A,F9.6,A)') 'The sum of all secondary weights of the Shchepetkin ', &
   'filter optimized for minimal dispersion is ', SUM(SecondaryWeights), '.'
   
   WRITE (*,'(A)') ' '
   
   CALL WriteTecplot1D(tau,PrimaryWeights, &
                       'ShchepetkinFilter_MinimalDispersion_PrimaryWeights')
   CALL WriteTecplot1D(tau,SecondaryWeights, &
                       'ShchepetkinFilter_MinimalDispersion_SecondaryWeights')
   
   DEALLOCATE (tau)
   DEALLOCATE (PrimaryWeights)
   DEALLOCATE (SecondaryWeights)
   
   END SUBROUTINE ShchepetkinFilter_MinimalDispersion
   !-----------------------------------------------------------------------------------!
   !                                                                                   ! 
   !-----------------------------------------------------------------------------------!
   SUBROUTINE TestFilterRoutines()
   
   IMPLICIT NONE

   CALL TestInverseMatrix()
   CALL TestNewtonRaphsonSolverForSystem()
   CALL RunNewtonRaphsonSolverForDeterminingShchepetkinFilterParameters()
   CALL TestNewtonRaphsonSolverForDeterminingShchepetkinFilterEndPoint(2._RKIND, &
                                                                       2._RKIND)
   CALL DetermineShchepetkinFilterParameters_SecondOrderAccurate()
   CALL DetermineShchepetkinFilterParameters_MinimalDispersion()
   CALL RectangularFilter(1._RKIND)
   CALL RectangularFilter(0.5_RKIND)
   CALL ROMSCosineFilter()
   CALL HammingWindowCosineFilter()
   CALL ShchepetkinFilter_SecondOrderAccurate(2._RKIND,2._RKIND)
   CALL ShchepetkinFilter_SecondOrderAccurate(2._RKIND,4._RKIND)
   CALL ShchepetkinFilter_MinimalDispersion(2._RKIND,2._RKIND)
   CALL ShchepetkinFilter_MinimalDispersion(2._RKIND,4._RKIND)
   
   END SUBROUTINE TestFilterRoutines
   
END MODULE ocn_filter_routines