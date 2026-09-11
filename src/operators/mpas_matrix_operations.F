! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
!
!  mpas_matrix_operations
!
!> \brief MPAS matrix operations
!> \author Mark Petersen
!> \date    April 2013
!> \details
!>  This module contains the routines for matrix operations
!
!-----------------------------------------------------------------------
module mpas_matrix_operations

   use mpas_derived_types
   use mpas_pool_routines
   use mpas_constants
   use mpas_log

   implicit none
   private
   save

   !--------------------------------------------------------------------
   !
   ! Public parameters
   !
   !--------------------------------------------------------------------

   !--------------------------------------------------------------------
   !
   ! Public member functions
   !
   !--------------------------------------------------------------------

   public :: mpas_rotate_2D_matrix_2x2, &
             mpas_rotate_2D_matrix_sym3index, &
             mpas_matrix_sym6index_to_3x3, &
             mpas_matrix_3x3_to_sym6index, &
             mpas_matrix_cell_to_edge, &
             mpas_outer_product, &
             mpas_migs, &
             mpas_legs

   !--------------------------------------------------------------------
   !
   ! Private module variables
   !
   !--------------------------------------------------------------------

!***********************************************************************

contains

!***********************************************************************
!
!  routine mpas_rotate_2D_matrix_2x2
!
!> \brief   Rotate a 2D matrix in 2x2 index format
!> \author  Mark Petersen
!> \date    April 2013
!> \details 
!>  Rotate matrix A about the angle theta using rotation matrix R, where
!>  B = RAR'  and R' is the tranpose.
!
!-----------------------------------------------------------------------

   subroutine mpas_rotate_2D_matrix_2x2(A,theta,B)!{{{

      !-----------------------------------------------------------------
      !
      ! input variables
      !
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(2,2), intent(in) :: &
         A   !< Input: 2x2 matrix

      real (kind=RKIND), intent(in) :: &
         theta   !< Input: rotation angle

      !-----------------------------------------------------------------
      !
      ! input/output variables
      !
      !-----------------------------------------------------------------

      !-----------------------------------------------------------------
      !
      ! output variables
      !
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(2,2), intent(out) :: &
         B   !< Output: 2x2 matrix

      !-----------------------------------------------------------------
      !
      ! local variables
      !
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(2,2) :: &
         R   ! 2x2 rotation matrix

      integer :: i,j,k,l

      real (kind=RKIND) :: sin1, cos1

      cos1 = cos(theta)
      sin1 = sin(theta)

      ! create rotation matrix
      R(1,1) = cos1
      R(1,2) = -sin1
      R(2,1) = sin1
      R(2,2) = cos1

      B = 0.0
      do i=1,2
        do j=1,2
          do k=1,2
            do l=1,2
              ! B = R A R' in matrix format
              ! B(i,j) =  R(i,k) A(k,l) R(j,l) in index format
              ! note indices switched on second R due to transpose.
              B(i,j) = B(i,j) + R(i,k)*A(k,l)*R(j,l)
            enddo
          enddo
        enddo
      enddo

   end subroutine mpas_rotate_2D_matrix_2x2!}}}

!***********************************************************************
!
!  routine mpas_rotate_2D_matrix_sym3index
!
!> \brief   Rotate a 2D matrix in 3-index format
!> \author  Mark Petersen
!> \date    April 2013
!> \details 
!>  Rotate matrix A about the angle theta using rotation matrix R, where
!>  B = RAR'  and R' is the tranpose.  A is a symmetric matrix in 3-index
!>  format, where A(1)=A_{11}, A(2)=A_{22}, and A(3)=A_{12}
!
!-----------------------------------------------------------------------

   subroutine mpas_rotate_2D_matrix_sym3index(A,theta,B)!{{{

      !-----------------------------------------------------------------
      !
      ! input variables
      !
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(3), intent(in) :: &
         A   !< Input: 2x2 symmetric matrix in 3-index format

      real (kind=RKIND), intent(in) :: &
         theta   !< Input: rotation angle

      !-----------------------------------------------------------------
      !
      ! input/output variables
      !
      !-----------------------------------------------------------------

      !-----------------------------------------------------------------
      !
      ! output variables
      !
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(3), intent(out) :: &
         B   !< Input: 2x2 symmetric matrix in 3-index format

      !-----------------------------------------------------------------
      !
      ! local variables
      !
      !-----------------------------------------------------------------

      real (kind=RKIND) :: sin1, cos1, sin2, cos2, cossin

      cos1 = cos(theta)
      sin1 = sin(theta)
      cos2 = cos1**2
      sin2 = sin1**2
      cossin = cos1*sin1

      ! Tensor rotation through angle theta:
      ! R = | cos(theta)    -sin(theta)  |
      !     | sin(theta)     cos(theta)  |
      ! is the 2D rotation matrix.  Tensor rotation is
      ! B = R A R'  where R' is the transpose.

      ! A and B are symmetric, in 3-index format:
      ! A(1)=A_{11}, A(2)=A_{22}, and A(3)=A_{12}

      B(1) = A(1)*cos2 + A(2)*sin2 - A(3)*2*cossin
      B(2) = A(1)*sin2 + A(2)*cos2 + A(3)*2*cossin 
      B(3) = (A(1) - A(2))*cossin + A(3)*(cos2-sin2)

   end subroutine mpas_rotate_2D_matrix_sym3index!}}}


!***********************************************************************
!
!  routine mpas_matrix_sym6index_to_3x3
!
!> \brief   Convert a symetric 6-index matrix to 3x3 format
!> \author  Mark Petersen
!> \date    April 2013
!> \details 
!>  Given a symetric 6-index matrix A, produce a 3x3 matrix B, where
!>  A(1)=B_{11}, A(2)=B_{22}, A(3)=A_{33},
!>  A(4)=B_{12}, A(5)=B_{23}, A(6)=A_{13},
!
!-----------------------------------------------------------------------

   subroutine mpas_matrix_sym6index_to_3x3(A,B)!{{{

      !-----------------------------------------------------------------
      !
      ! input variables
      !
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(6), intent(in) :: &
         A   !< Input: 3x3 symmetric matrix in 6-index format

      !-----------------------------------------------------------------
      !
      ! output variables
      !
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(3,3), intent(out) :: &
         B   !< Output: 3x3 symmetric matrix in 3x3 format


      B(1,1) = A(1)
      B(2,2) = A(2)
      B(3,3) = A(3)
      B(1,2) = A(4)
      B(2,1) = A(4)
      B(2,3) = A(5)
      B(3,2) = A(5)
      B(1,3) = A(6)
      B(3,1) = A(6)

   end subroutine mpas_matrix_sym6index_to_3x3!}}}


!***********************************************************************
!
!  routine mpas_matrix_3x3_to_sym6index
!
!> \brief   Convert a 3x3 format matrix to a symetric 6-index matrix
!> \author  Mark Petersen
!> \date    April 2013
!> \details 
!>  Given a 3x3 matrix B, produce a symetric 6-index matrix A,
!>  B(1)=A(1,1), B(2)=A(2,2), B(3)=A(3,3)
!>  B(4)=(A(1,2)+A(2,1))/2
!>  B(5)=(A(2,3)+A(3,2))/2
!>  B(6)=(A(1,3)+A(3,1))/2
!
!-----------------------------------------------------------------------

   subroutine mpas_matrix_3x3_to_sym6index(A,B)!{{{

      !-----------------------------------------------------------------
      !
      ! input variables
      !
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(3,3), intent(in) :: &
         A   !< Input: 3x3 symmetric matrix in 3x3 format

      !-----------------------------------------------------------------
      !
      ! output variables
      !
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(6), intent(out) :: &
         B   !< Output: 3x3 symmetric matrix in 6-index format


      B(1) = A(1,1)
      B(2) = A(2,2)
      B(3) = A(3,3)
      B(4) = 0.5*(A(1,2)+A(2,1))
      B(5) = 0.5*(A(2,3)+A(3,2))
      B(6) = 0.5*(A(1,3)+A(3,1))

   end subroutine mpas_matrix_3x3_to_sym6index!}}}


!***********************************************************************
!
!  routine mpas_matrix_cell_to_edge
!
!> \brief   Interpolate a matrix from cell to edge
!> \author  Mark Petersen
!> \date    April 2013
!> \details 
!>  This routine interpolates a matrix from cell to edge locations.
!
!-----------------------------------------------------------------------

   subroutine mpas_matrix_cell_to_edge(matrixCell, meshPool, includeHalo, matrixEdge)!{{{

      !-----------------------------------------------------------------
      !
      ! input variables
      !
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(:,:,:), intent(in) :: &
         matrixCell   !< Input: matrix located at Cell

      type (mpas_pool_type), intent(in) :: &
         meshPool          !< Input: grid information

      logical, intent(in) :: & 
         includeHalo !< Input: If true, halo cells and edges are included in computation

      !-----------------------------------------------------------------
      !
      ! output variables
      !
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(:,:,:), intent(out) :: &
         matrixEdge   !< Output: matrix located at Edge

      !-----------------------------------------------------------------
      !
      ! local variables
      !
      !-----------------------------------------------------------------

      integer :: iEdge, cell1, cell2, k
      integer, pointer :: nEdgesCompute, nVertLevels
      integer, dimension(:,:), pointer :: cellsOnEdge

      if (includeHalo) then
         call mpas_pool_get_dimension(meshPool, 'nEdges', nEdgesCompute)
      else 
         call mpas_pool_get_dimension(meshPool, 'nEdgesSolve', nEdgesCompute)
      endif
      call mpas_pool_get_dimension(meshPool, 'nVertLevels', nVertLevels)

      call mpas_pool_get_array(meshPool, 'cellsOnEdge', cellsOnEdge)

      ! error check that index 1 of matrixEdge and matrixCell are same length?

      do iEdge=1,nEdgesCompute
         cell1 = cellsOnEdge(1,iEdge)
         cell2 = cellsOnEdge(2,iEdge)
         do k=1,nVertLevels

            matrixEdge(:,k,iEdge) = 0.5*(matrixCell(:,k,cell1) + matrixCell(:,k,cell2))

         enddo
      enddo

   end subroutine mpas_matrix_cell_to_edge!}}}


!***********************************************************************
!
!  routine mpas_outer_product
!
!> \brief   Compute the outer product of vectors u and v
!> \author  Mark Petersen
!> \date    April 2013
!> \details 
!>  Given two n-length vector u and m-length vector v, compute the 
!>  outer product (tensor product) to compute the nxm matrix A,
!>  A(i,j) = u(i)*v(j)
!
!-----------------------------------------------------------------------

   subroutine mpas_outer_product(u,v,A)!{{{

      !-----------------------------------------------------------------
      !
      ! input variables
      !
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(:), intent(in) :: &
         u, &!< Input: n-length vector
         v   !< Input: m-length vector

      !-----------------------------------------------------------------
      !
      ! output variables
      !
      !-----------------------------------------------------------------

      real (kind=RKIND), dimension(:,:), intent(out) :: &
         A   !< Output: nxm matrix

      !-----------------------------------------------------------------
      !
      ! local variables
      !
      !-----------------------------------------------------------------

      integer :: n,m,i,j

      n = size(u)
      m = size(v)

      ! mrp: find out how to do correct error check.
      if (size(A,1).ne.n.or.size(A,2).ne.m) then
        call mpas_log_write('In subroutine mpas_outer_product(), size of A must match u and v', MPAS_LOG_CRIT)
      endif

      do i=1,n
        do j=1,m
          A(i,j) = u(i)*v(j)
        enddo
      enddo

   end subroutine mpas_outer_product!}}}

!------------------------------------------------------------------------
!
! The LU factorization and solve routines below (mpas_getrf, mpas_getrs)
! are adapted from the LAPACK reference implementation of DGETF2 and
! DGETRS, converted to free-form Fortran at MPAS real precision (RKIND).
!
! Copyright (c) 1992-2025 The University of Tennessee and The University
!                         of Tennessee Research Foundation.  All rights
!                         reserved.
! Copyright (c) 2000-2025 The University of California Berkeley. All
!                         rights reserved.
! Copyright (c) 2006-2025 The University of Colorado Denver.  All rights
!                         reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are
! met:
!
! - Redistributions of source code must retain the above copyright
!   notice, this list of conditions and the following disclaimer.
!
! - Redistributions in binary form must reproduce the above copyright
!   notice, this list of conditions and the following disclaimer listed
!   in this license in the documentation and/or other materials
!   provided with the distribution.
!
! - Neither the name of the copyright holders nor the names of its
!   contributors may be used to endorse or promote products derived from
!   this software without specific prior written permission.
!
! The copyright holders provide no reassurances that the source code
! provided does not infringe any patent, copyright, or any other
! intellectual property rights of third parties.  The copyright holders
! disclaim any liability to any recipient for claims brought against
! recipient by any third party for infringement of that parties
! intellectual property rights.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
! A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
! OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
! SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
! LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
! DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
! THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
!------------------------------------------------------------------------

   subroutine mpas_getrf(N, A, IPIV, INFO)!{{{

      ! LU factorization with partial pivoting: A = P*L*U, in place.

      IMPLICIT NONE

      INTEGER, INTENT(IN):: N
      REAL(KIND=RKIND), DIMENSION(N,N), INTENT(INOUT):: A
      INTEGER, DIMENSION(N), INTENT(OUT):: IPIV
      INTEGER, INTENT(OUT):: INFO

      REAL(KIND=RKIND), PARAMETER:: ZERO = 0.0_RKIND

      INTEGER:: I,J,JP
      REAL(KIND=RKIND):: TMP

      INFO = 0

      DO J = 1, N
         ! Find pivot and test for singularity

         JP = J - 1 + MAXLOC(ABS(A(J:N,J)), DIM=1)
         IPIV(J) = JP
         IF(A(JP,J).NE.ZERO) THEN
            ! Apply the interchange to columns 1:n

            IF (JP.NE.J) THEN
               DO I = 1, N
                  TMP = A(J,I)
                  A(J,I) = A(JP,I)
                  A(JP,I) = TMP
               ENDDO
            ENDIF
         
            ! Compute elements j+1:n of j-th column

            IF (J.LT.N) THEN
               DO I = 1, N-J
                  A(J+I,J) = A(J+I,J) / A(J,J)
               ENDDO
            ENDIF
         
         ELSE IF (INFO.EQ.0) THEN
            INFO = J
         ENDIF

         IF(J.LT.N) THEN
            !Update trailing submatrix

            DO I = J+1, N
               A(J+1:N,I) = A(J+1:N,I) - A(J+1:N,J) * A(J,I)
            ENDDO
         ENDIF
      ENDDO
   end subroutine mpas_getrf!}}}

   subroutine mpas_getrs(N, NRHS,A,IPIV,B)!{{{
      ! Solves A*X = B for nrhs right-hand sides using the P*L*U
      ! factors computed by mpas_getrf

      IMPLICIT NONE

      INTEGER, INTENT(IN):: N,NRHS
      REAL(KIND=RKIND), DIMENSION(N,N), INTENT(IN):: A
      INTEGER, DIMENSION(N), INTENT(IN):: IPIV
      REAL(KIND=RKIND), DIMENSION(N,NRHS), INTENT(INOUT):: B

      INTEGER:: I,K
      REAL(KIND=RKIND):: TMP

      ! Apply row interchanges to the right hand sides
      DO I = 1, N
         IF (IPIV(I).NE.I) THEN 
            DO K = 1, NRHS
               TMP = B(I,K)
               B(I,K) = B(IPIV(I),K)
               B(IPIV(I),K) = TMP
            ENDDO
         ENDIF
      ENDDO

      ! Solve L*X = B, overwriting B with X
      DO K = 1, NRHS
         DO I =2, N
            B(I,K) = B(I,K) - DOT_PRODUCT(A(I,1:I-1), B(1:I-1,K))
         ENDDO
      ENDDO

      ! Solve U*X = B, overwriting B with X
      DO K=1, NRHS
         DO I = N, 1, -1
            B(I,K) = (B(I,K) - DOT_PRODUCT(A(I,I+1:N), B(I+1:N,K)))/A(I,I)
         ENDDO
      ENDDO
   
   end subroutine mpas_getrs!}}}

   subroutine mpas_migs(A,N,X,INDX)!{{{
      ! Invert the N x N matrix A, returning the inverse in X

      IMPLICIT NONE

      INTEGER, INTENT(IN):: N
      REAL(KIND=RKIND), DIMENSION(N,N), INTENT(INOUT):: A
      REAL(KIND=RKIND), DIMENSION(N,N), INTENT(OUT):: X
      INTEGER, DIMENSION(N), INTENT(OUT):: INDX

      INTEGER::K, INFO

      CALL mpas_getrf(N,A,INDX,INFO)

      X(:,:) = 0.0_RKIND
      DO K = 1,N
         X(K,K) = 1.0_RKIND
      ENDDO

      CALL mpas_getrs(N,N,A,INDX,X)
   
   end subroutine mpas_migs!}}}

   subroutine mpas_legs(A,N,B,X,INDX)!{{{
      ! Solve the linear system A*X = B by LU factorization.

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: N
      REAL(KIND=RKIND), DIMENSION(N,N), INTENT(INOUT) :: A
      REAL(KIND=RKIND), DIMENSION(N), INTENT(INOUT) :: B
      REAL(KIND=RKIND), DIMENSION(N), INTENT(OUT) :: X
      INTEGER, DIMENSION(N), INTENT(OUT) :: INDX

      REAL(KIND=RKIND), DIMENSION(N,1) :: RHS
      INTEGER :: INFO

      CALL  mpas_getrf(N,A,INDX,INFO)

      RHS(:,1) = B(:)
      CALL mpas_getrs(N,1,A,INDX,RHS)
      X(:) = RHS(:,1)
   
   end subroutine mpas_legs!}}}

end module mpas_matrix_operations

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
! vim: foldmethod=marker
