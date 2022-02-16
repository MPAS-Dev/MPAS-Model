      SUBROUTINE ADDNOD (NST,K,X,Y,Z, LIST,LPTR,LEND,
     .                   LNEW, IER)
      INTEGER NST, K, LIST(*), LPTR(*), LEND(K), LNEW, IER
      REAL    X(K), Y(K), Z(K)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/08/99
C
C   This subroutine adds node K to a triangulation of the
C convex hull of nodes 1,...,K-1, producing a triangulation
C of the convex hull of nodes 1,...,K.
C
C   The algorithm consists of the following steps:  node K
C is located relative to the triangulation (TRFIND), its
C index is added to the data structure (INTADD or BDYADD),
C and a sequence of swaps (SWPTST and SWAP) are applied to
C the arcs opposite K so that all arcs incident on node K
C and opposite node K are locally optimal (satisfy the cir-
C cumcircle test).  Thus, if a Delaunay triangulation is
C input, a Delaunay triangulation will result.
C
C
C On input:
C
C       NST = Index of a node at which TRFIND begins its
C             search.  Search time depends on the proximity
C             of this node to K.  If NST < 1, the search is
C             begun at node K-1.
C
C       K = Nodal index (index for X, Y, Z, and LEND) of the
C           new node to be added.  K .GE. 4.
C
C       X,Y,Z = Arrays of length .GE. K containing Car-
C               tesian coordinates of the nodes.
C               (X(I),Y(I),Z(I)) defines node I for
C               I = 1,...,K.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR,LEND,LNEW = Data structure associated with
C                             the triangulation of nodes 1
C                             to K-1.  The array lengths are
C                             assumed to be large enough to
C                             add node K.  Refer to Subrou-
C                             tine TRMESH.
C
C On output:
C
C       LIST,LPTR,LEND,LNEW = Data structure updated with
C                             the addition of node K as the
C                             last entry unless IER .NE. 0
C                             and IER .NE. -3, in which case
C                             the arrays are not altered.
C
C       IER = Error indicator:
C             IER =  0 if no errors were encountered.
C             IER = -1 if K is outside its valid range
C                      on input.
C             IER = -2 if all nodes (including K) are col-
C                      linear (lie on a common geodesic).
C             IER =  L if nodes L and K coincide for some
C                      L < K.
C
C Modules required by ADDNOD:  BDYADD, COVSPH, INSERT,
C                                INTADD, JRAND, LSTPTR,
C                                STORE, SWAP, SWPTST,
C                                TRFIND
C
C Intrinsic function called by ADDNOD:  ABS
C
C***********************************************************
C
      INTEGER LSTPTR
      INTEGER I1, I2, I3, IO1, IO2, IN1, IST, KK, KM1, L,
     .        LP, LPF, LPO1, LPO1S
      LOGICAL SWPTST
      REAL    B1, B2, B3, P(3)
C
C Local parameters:
C
C B1,B2,B3 = Unnormalized barycentric coordinates returned
C              by TRFIND.
C I1,I2,I3 = Vertex indexes of a triangle containing K
C IN1 =      Vertex opposite K:  first neighbor of IO2
C              that precedes IO1.  IN1,IO1,IO2 are in
C              counterclockwise order.
C IO1,IO2 =  Adjacent neighbors of K defining an arc to
C              be tested for a swap
C IST =      Index of node at which TRFIND begins its search
C KK =       Local copy of K
C KM1 =      K-1
C L =        Vertex index (I1, I2, or I3) returned in IER
C              if node K coincides with a vertex
C LP =       LIST pointer
C LPF =      LIST pointer to the first neighbor of K
C LPO1 =     LIST pointer to IO1
C LPO1S =    Saved value of LPO1
C P =        Cartesian coordinates of node K
C
      KK = K
      IF (KK .LT. 4) GO TO 3
C
C Initialization:
C
      KM1 = KK - 1
      IST = NST
      IF (IST .LT. 1) IST = KM1
      P(1) = X(KK)
      P(2) = Y(KK)
      P(3) = Z(KK)
C
C Find a triangle (I1,I2,I3) containing K or the rightmost
C   (I1) and leftmost (I2) visible boundary nodes as viewed
C   from node K.
C
      CALL TRFIND (IST,P,KM1,X,Y,Z,LIST,LPTR,LEND, B1,B2,B3,
     .             I1,I2,I3)
C
C   Test for collinear or duplicate nodes.
C
      IF (I1 .EQ. 0) GO TO 4
      IF (I3 .NE. 0) THEN
        L = I1
        IF (P(1) .EQ. X(L)  .AND.  P(2) .EQ. Y(L)  .AND.
     .      P(3) .EQ. Z(L)) GO TO 5
        L = I2
        IF (P(1) .EQ. X(L)  .AND.  P(2) .EQ. Y(L)  .AND.
     .      P(3) .EQ. Z(L)) GO TO 5
        L = I3
        IF (P(1) .EQ. X(L)  .AND.  P(2) .EQ. Y(L)  .AND.
     .      P(3) .EQ. Z(L)) GO TO 5
        CALL INTADD (KK,I1,I2,I3, LIST,LPTR,LEND,LNEW )
      ELSE
        IF (I1 .NE. I2) THEN
          CALL BDYADD (KK,I1,I2, LIST,LPTR,LEND,LNEW )
        ELSE
          CALL COVSPH (KK,I1, LIST,LPTR,LEND,LNEW )
        ENDIF
      ENDIF
      IER = 0
C
C Initialize variables for optimization of the
C   triangulation.
C
      LP = LEND(KK)
      LPF = LPTR(LP)
      IO2 = LIST(LPF)
      LPO1 = LPTR(LPF)
      IO1 = ABS(LIST(LPO1))
C
C Begin loop:  find the node opposite K.
C
    1 LP = LSTPTR(LEND(IO1),IO2,LIST,LPTR)
        IF (LIST(LP) .LT. 0) GO TO 2
        LP = LPTR(LP)
        IN1 = ABS(LIST(LP))
C
C Swap test:  if a swap occurs, two new arcs are
C             opposite K and must be tested.
C
        LPO1S = LPO1
        IF ( .NOT. SWPTST(IN1,KK,IO1,IO2,X,Y,Z) ) GO TO 2
        CALL SWAP (IN1,KK,IO1,IO2, LIST,LPTR,LEND, LPO1)
        IF (LPO1 .EQ. 0) THEN
C
C   A swap is not possible because KK and IN1 are already
C     adjacent.  This error in SWPTST only occurs in the
C     neutral case and when there are nearly duplicate
C     nodes.
C
          LPO1 = LPO1S
          GO TO 2
        ENDIF
        IO1 = IN1
        GO TO 1
C
C No swap occurred.  Test for termination and reset
C   IO2 and IO1.
C
    2   IF (LPO1 .EQ. LPF  .OR.  LIST(LPO1) .LT. 0) RETURN
        IO2 = IO1
        LPO1 = LPTR(LPO1)
        IO1 = ABS(LIST(LPO1))
        GO TO 1
C
C KK < 4.
C
    3 IER = -1
      RETURN
C
C All nodes are collinear.
C
    4 IER = -2
      RETURN
C
C Nodes L and K coincide.
C
    5 IER = L
      RETURN
      END
      REAL FUNCTION AREAS (V1,V2,V3)
      REAL V1(3), V2(3), V3(3)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   09/18/90
C
C   This function returns the area of a spherical triangle
C on the unit sphere.
C
C
C On input:
C
C       V1,V2,V3 = Arrays of length 3 containing the Carte-
C                  sian coordinates of unit vectors (the
C                  three triangle vertices in any order).
C                  These vectors, if nonzero, are implicitly
C                  scaled to have length 1.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       AREAS = Area of the spherical triangle defined by
C               V1, V2, and V3 in the range 0 to 2*PI (the
C               area of a hemisphere).  AREAS = 0 (or 2*PI)
C               if and only if V1, V2, and V3 lie in (or
C               close to) a plane containing the origin.
C
C Modules required by AREAS:  None
C
C Intrinsic functions called by AREAS:  ACOS, DBLE, REAL,
C                                         SQRT
C
C***********************************************************
C
      DOUBLE PRECISION A1, A2, A3, CA1, CA2, CA3, DV1(3),
     .                 DV2(3), DV3(3), S12, S23, S31,
     .                 U12(3), U23(3), U31(3)
      INTEGER          I
C
C Local parameters:
C
C A1,A2,A3 =    Interior angles of the spherical triangle
C CA1,CA2,CA3 = cos(A1), cos(A2), and cos(A3), respectively
C DV1,DV2,DV3 = Double Precision copies of V1, V2, and V3
C I =           DO-loop index and index for Uij
C S12,S23,S31 = Sum of squared components of U12, U23, U31
C U12,U23,U31 = Unit normal vectors to the planes defined by
C                 pairs of triangle vertices
C
      DO 1 I = 1,3
        DV1(I) = DBLE(V1(I))
        DV2(I) = DBLE(V2(I))
        DV3(I) = DBLE(V3(I))
    1   CONTINUE
C
C Compute cross products Uij = Vi X Vj.
C
      U12(1) = DV1(2)*DV2(3) - DV1(3)*DV2(2)
      U12(2) = DV1(3)*DV2(1) - DV1(1)*DV2(3)
      U12(3) = DV1(1)*DV2(2) - DV1(2)*DV2(1)
C
      U23(1) = DV2(2)*DV3(3) - DV2(3)*DV3(2)
      U23(2) = DV2(3)*DV3(1) - DV2(1)*DV3(3)
      U23(3) = DV2(1)*DV3(2) - DV2(2)*DV3(1)
C
      U31(1) = DV3(2)*DV1(3) - DV3(3)*DV1(2)
      U31(2) = DV3(3)*DV1(1) - DV3(1)*DV1(3)
      U31(3) = DV3(1)*DV1(2) - DV3(2)*DV1(1)
C
C Normalize Uij to unit vectors.
C
      S12 = 0.D0
      S23 = 0.D0
      S31 = 0.D0
      DO 2 I = 1,3
        S12 = S12 + U12(I)*U12(I)
        S23 = S23 + U23(I)*U23(I)
        S31 = S31 + U31(I)*U31(I)
    2   CONTINUE
C
C Test for a degenerate triangle associated with collinear
C   vertices.
C
      IF (S12 .EQ. 0.D0  .OR.  S23 .EQ. 0.D0  .OR.
     .    S31 .EQ. 0.D0) THEN
        AREAS = 0.
        RETURN
      ENDIF
      S12 = SQRT(S12)
      S23 = SQRT(S23)
      S31 = SQRT(S31)
      DO 3 I = 1,3
        U12(I) = U12(I)/S12
        U23(I) = U23(I)/S23
        U31(I) = U31(I)/S31
    3   CONTINUE
C
C Compute interior angles Ai as the dihedral angles between
C   planes:
C           CA1 = cos(A1) = -<U12,U31>
C           CA2 = cos(A2) = -<U23,U12>
C           CA3 = cos(A3) = -<U31,U23>
C
      CA1 = -U12(1)*U31(1)-U12(2)*U31(2)-U12(3)*U31(3)
      CA2 = -U23(1)*U12(1)-U23(2)*U12(2)-U23(3)*U12(3)
      CA3 = -U31(1)*U23(1)-U31(2)*U23(2)-U31(3)*U23(3)
      IF (CA1 .LT. -1.D0) CA1 = -1.D0
      IF (CA1 .GT. 1.D0) CA1 = 1.D0
      IF (CA2 .LT. -1.D0) CA2 = -1.D0
      IF (CA2 .GT. 1.D0) CA2 = 1.D0
      IF (CA3 .LT. -1.D0) CA3 = -1.D0
      IF (CA3 .GT. 1.D0) CA3 = 1.D0
      A1 = ACOS(CA1)
      A2 = ACOS(CA2)
      A3 = ACOS(CA3)
C
C Compute AREAS = A1 + A2 + A3 - PI.
C
      AREAS = REAL(A1 + A2 + A3 - ACOS(-1.D0))
      IF (AREAS .LT. 0.) AREAS = 0.
      RETURN
      END
      SUBROUTINE BDYADD (KK,I1,I2, LIST,LPTR,LEND,LNEW )
      INTEGER KK, I1, I2, LIST(*), LPTR(*), LEND(*), LNEW
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/11/96
C
C   This subroutine adds a boundary node to a triangulation
C of a set of KK-1 points on the unit sphere.  The data
C structure is updated with the insertion of node KK, but no
C optimization is performed.
C
C   This routine is identical to the similarly named routine
C in TRIPACK.
C
C
C On input:
C
C       KK = Index of a node to be connected to the sequence
C            of all visible boundary nodes.  KK .GE. 1 and
C            KK must not be equal to I1 or I2.
C
C       I1 = First (rightmost as viewed from KK) boundary
C            node in the triangulation that is visible from
C            node KK (the line segment KK-I1 intersects no
C            arcs.
C
C       I2 = Last (leftmost) boundary node that is visible
C            from node KK.  I1 and I2 may be determined by
C            Subroutine TRFIND.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR,LEND,LNEW = Triangulation data structure
C                             created by Subroutine TRMESH.
C                             Nodes I1 and I2 must be in-
C                             cluded in the triangulation.
C
C On output:
C
C       LIST,LPTR,LEND,LNEW = Data structure updated with
C                             the addition of node KK.  Node
C                             KK is connected to I1, I2, and
C                             all boundary nodes in between.
C
C Module required by BDYADD:  INSERT
C
C***********************************************************
C
      INTEGER K, LP, LSAV, N1, N2, NEXT, NSAV
C
C Local parameters:
C
C K =     Local copy of KK
C LP =    LIST pointer
C LSAV =  LIST pointer
C N1,N2 = Local copies of I1 and I2, respectively
C NEXT =  Boundary node visible from K
C NSAV =  Boundary node visible from K
C
      K = KK
      N1 = I1
      N2 = I2
C
C Add K as the last neighbor of N1.
C
      LP = LEND(N1)
      LSAV = LPTR(LP)
      LPTR(LP) = LNEW
      LIST(LNEW) = -K
      LPTR(LNEW) = LSAV
      LEND(N1) = LNEW
      LNEW = LNEW + 1
      NEXT = -LIST(LP)
      LIST(LP) = NEXT
      NSAV = NEXT
C
C Loop on the remaining boundary nodes between N1 and N2,
C   adding K as the first neighbor.
C
    1 LP = LEND(NEXT)
        CALL INSERT (K,LP, LIST,LPTR,LNEW )
        IF (NEXT .EQ. N2) GO TO 2
        NEXT = -LIST(LP)
        LIST(LP) = NEXT
        GO TO 1
C
C Add the boundary nodes between N1 and N2 as neighbors
C   of node K.
C
    2 LSAV = LNEW
      LIST(LNEW) = N1
      LPTR(LNEW) = LNEW + 1
      LNEW = LNEW + 1
      NEXT = NSAV
C
    3 IF (NEXT .EQ. N2) GO TO 4
        LIST(LNEW) = NEXT
        LPTR(LNEW) = LNEW + 1
        LNEW = LNEW + 1
        LP = LEND(NEXT)
        NEXT = LIST(LP)
        GO TO 3
C
    4 LIST(LNEW) = -N2
      LPTR(LNEW) = LSAV
      LEND(K) = LNEW
      LNEW = LNEW + 1
      RETURN
      END
      SUBROUTINE BNODES (N,LIST,LPTR,LEND, NODES,NB,NA,NT)
      INTEGER N, LIST(*), LPTR(*), LEND(N), NODES(*), NB,
     .        NA, NT
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   06/26/96
C
C   Given a triangulation of N nodes on the unit sphere
C created by Subroutine TRMESH, this subroutine returns an
C array containing the indexes (if any) of the counterclock-
C wise-ordered sequence of boundary nodes -- the nodes on
C the boundary of the convex hull of the set of nodes.  (The
C boundary is empty if the nodes do not lie in a single
C hemisphere.)  The numbers of boundary nodes, arcs, and
C triangles are also returned.
C
C
C On input:
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to Subroutine
C                        TRMESH.
C
C The above parameters are not altered by this routine.
C
C       NODES = Integer array of length at least NB
C               (NB .LE. N).
C
C On output:
C
C       NODES = Ordered sequence of boundary node indexes
C               in the range 1 to N (in the first NB loca-
C               tions).
C
C       NB = Number of boundary nodes.
C
C       NA,NT = Number of arcs and triangles, respectively,
C               in the triangulation.
C
C Modules required by BNODES:  None
C
C***********************************************************
C
      INTEGER K, LP, N0, NN, NST
C
C Local parameters:
C
C K =   NODES index
C LP =  LIST pointer
C N0 =  Boundary node to be added to NODES
C NN =  Local copy of N
C NST = First element of nodes (arbitrarily chosen to be
C         the one with smallest index)
C
      NN = N
C
C Search for a boundary node.
C
      DO 1 NST = 1,NN
        LP = LEND(NST)
        IF (LIST(LP) .LT. 0) GO TO 2
    1   CONTINUE
C
C The triangulation contains no boundary nodes.
C
      NB = 0
      NA = 3*(NN-2)
      NT = 2*(NN-2)
      RETURN
C
C NST is the first boundary node encountered.  Initialize
C   for traversal of the boundary.
C
    2 NODES(1) = NST
      K = 1
      N0 = NST
C
C Traverse the boundary in counterclockwise order.
C
    3 LP = LEND(N0)
        LP = LPTR(LP)
        N0 = LIST(LP)
        IF (N0 .EQ. NST) GO TO 4
        K = K + 1
        NODES(K) = N0
        GO TO 3
C
C Store the counts.
C
    4 NB = K
      NT = 2*N - NB - 2
      NA = NT + N - 1
      RETURN
      END
      SUBROUTINE CIRCUM (V1,V2,V3, C,IER)
      INTEGER IER
      REAL    V1(3), V2(3), V3(3), C(3)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   06/29/95
C
C   This subroutine returns the circumcenter of a spherical
C triangle on the unit sphere:  the point on the sphere sur-
C face that is equally distant from the three triangle
C vertices and lies in the same hemisphere, where distance
C is taken to be arc-length on the sphere surface.
C
C
C On input:
C
C       V1,V2,V3 = Arrays of length 3 containing the Carte-
C                  sian coordinates of the three triangle
C                  vertices (unit vectors) in CCW order.
C
C The above parameters are not altered by this routine.
C
C       C = Array of length 3.
C
C On output:
C
C       C = Cartesian coordinates of the circumcenter unless
C           IER > 0, in which case C is not defined.  C =
C           (V2-V1) X (V3-V1) normalized to a unit vector.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if V1, V2, and V3 lie on a common
C                     line:  (V2-V1) X (V3-V1) = 0.
C             (The vertices are not tested for validity.)
C
C Modules required by CIRCUM:  None
C
C Intrinsic function called by CIRCUM:  SQRT
C
C***********************************************************
C
      INTEGER I
      REAL    CNORM, CU(3), E1(3), E2(3)
C
C Local parameters:
C
C CNORM = Norm of CU:  used to compute C
C CU =    Scalar multiple of C:  E1 X E2
C E1,E2 = Edges of the underlying planar triangle:
C           V2-V1 and V3-V1, respectively
C I =     DO-loop index
C
      DO 1 I = 1,3
        E1(I) = V2(I) - V1(I)
        E2(I) = V3(I) - V1(I)
    1   CONTINUE
C
C Compute CU = E1 X E2 and CNORM**2.
C
      CU(1) = E1(2)*E2(3) - E1(3)*E2(2)
      CU(2) = E1(3)*E2(1) - E1(1)*E2(3)
      CU(3) = E1(1)*E2(2) - E1(2)*E2(1)
      CNORM = CU(1)*CU(1) + CU(2)*CU(2) + CU(3)*CU(3)
C
C The vertices lie on a common line if and only if CU is
C   the zero vector.
C
      IF (CNORM .NE. 0.) THEN
C
C   No error:  compute C.
C
        CNORM = SQRT(CNORM)
        DO 2 I = 1,3
          C(I) = CU(I)/CNORM
    2     CONTINUE
        IER = 0
      ELSE
C
C   CU = 0.
C
        IER = 1
      ENDIF
      RETURN
      END
      SUBROUTINE COVSPH (KK,N0, LIST,LPTR,LEND,LNEW )
      INTEGER KK, N0, LIST(*), LPTR(*), LEND(*), LNEW
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/17/96
C
C   This subroutine connects an exterior node KK to all
C boundary nodes of a triangulation of KK-1 points on the
C unit sphere, producing a triangulation that covers the
C sphere.  The data structure is updated with the addition
C of node KK, but no optimization is performed.  All boun-
C dary nodes must be visible from node KK.
C
C
C On input:
C
C       KK = Index of the node to be connected to the set of
C            all boundary nodes.  KK .GE. 4.
C
C       N0 = Index of a boundary node (in the range 1 to
C            KK-1).  N0 may be determined by Subroutine
C            TRFIND.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR,LEND,LNEW = Triangulation data structure
C                             created by Subroutine TRMESH.
C                             Node N0 must be included in
C                             the triangulation.
C
C On output:
C
C       LIST,LPTR,LEND,LNEW = Data structure updated with
C                             the addition of node KK as the
C                             last entry.  The updated
C                             triangulation contains no
C                             boundary nodes.
C
C Module required by COVSPH:  INSERT
C
C***********************************************************
C
      INTEGER K, LP, LSAV, NEXT, NST
C
C Local parameters:
C
C K =     Local copy of KK
C LP =    LIST pointer
C LSAV =  LIST pointer
C NEXT =  Boundary node visible from K
C NST =   Local copy of N0
C
      K = KK
      NST = N0
C
C Traverse the boundary in clockwise order, inserting K as
C   the first neighbor of each boundary node, and converting
C   the boundary node to an interior node.
C
      NEXT = NST
    1 LP = LEND(NEXT)
        CALL INSERT (K,LP, LIST,LPTR,LNEW )
        NEXT = -LIST(LP)
        LIST(LP) = NEXT
        IF (NEXT .NE. NST) GO TO 1
C
C Traverse the boundary again, adding each node to K's
C   adjacency list.
C
      LSAV = LNEW
    2 LP = LEND(NEXT)
        LIST(LNEW) = NEXT
        LPTR(LNEW) = LNEW + 1
        LNEW = LNEW + 1
        NEXT = LIST(LP)
        IF (NEXT .NE. NST) GO TO 2
C
      LPTR(LNEW-1) = LSAV
      LEND(K) = LNEW - 1
      RETURN
      END
      SUBROUTINE CRLIST (N,NCOL,X,Y,Z,LIST,LEND, LPTR,LNEW,
     .                   LTRI, LISTC,NB,XC,YC,ZC,RC,IER)
      INTEGER  N, NCOL, LIST(*), LEND(N), LPTR(*), LNEW,
     .         LTRI(6,NCOL), LISTC(*), NB, IER
      REAL X(N), Y(N), Z(N), XC(*), YC(*), ZC(*), RC(*)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/05/98
C
C   Given a Delaunay triangulation of nodes on the surface
C of the unit sphere, this subroutine returns the set of
C triangle circumcenters corresponding to Voronoi vertices,
C along with the circumradii and a list of triangle indexes
C LISTC stored in one-to-one correspondence with LIST/LPTR
C entries.
C
C   A triangle circumcenter is the point (unit vector) lying
C at the same angular distance from the three vertices and
C contained in the same hemisphere as the vertices.  (Note
C that the negative of a circumcenter is also equidistant
C from the vertices.)  If the triangulation covers the sur-
C face, the Voronoi vertices are the circumcenters of the
C triangles in the Delaunay triangulation.  LPTR, LEND, and
C LNEW are not altered in this case.
C
C   On the other hand, if the nodes are contained in a sin-
C gle hemisphere, the triangulation is implicitly extended
C to the entire surface by adding pseudo-arcs (of length
C greater than 180 degrees) between boundary nodes forming
C pseudo-triangles whose 'circumcenters' are included in the
C list.  This extension to the triangulation actually con-
C sists of a triangulation of the set of boundary nodes in
C which the swap test is reversed (a non-empty circumcircle
C test).  The negative circumcenters are stored as the
C pseudo-triangle 'circumcenters'.  LISTC, LPTR, LEND, and
C LNEW contain a data structure corresponding to the ex-
C tended triangulation (Voronoi diagram), but LIST is not
C altered in this case.  Thus, if it is necessary to retain
C the original (unextended) triangulation data structure,
C copies of LPTR and LNEW must be saved before calling this
C routine.
C
C
C On input:
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C           Note that, if N = 3, there are only two Voronoi
C           vertices separated by 180 degrees, and the
C           Voronoi regions are not well defined.
C
C       NCOL = Number of columns reserved for LTRI.  This
C              must be at least NB-2, where NB is the number
C              of boundary nodes.
C
C       X,Y,Z = Arrays of length N containing the Cartesian
C               coordinates of the nodes (unit vectors).
C
C       LIST = Integer array containing the set of adjacency
C              lists.  Refer to Subroutine TRMESH.
C
C       LEND = Set of pointers to ends of adjacency lists.
C              Refer to Subroutine TRMESH.
C
C The above parameters are not altered by this routine.
C
C       LPTR = Array of pointers associated with LIST.  Re-
C              fer to Subroutine TRMESH.
C
C       LNEW = Pointer to the first empty location in LIST
C              and LPTR (list length plus one).
C
C       LTRI = Integer work space array dimensioned 6 by
C              NCOL, or unused dummy parameter if NB = 0.
C
C       LISTC = Integer array of length at least 3*NT, where
C               NT = 2*N-4 is the number of triangles in the
C               triangulation (after extending it to cover
C               the entire surface if necessary).
C
C       XC,YC,ZC,RC = Arrays of length NT = 2*N-4.
C
C On output:
C
C       LPTR = Array of pointers associated with LISTC:
C              updated for the addition of pseudo-triangles
C              if the original triangulation contains
C              boundary nodes (NB > 0).
C
C       LNEW = Pointer to the first empty location in LISTC
C              and LPTR (list length plus one).  LNEW is not
C              altered if NB = 0.
C
C       LTRI = Triangle list whose first NB-2 columns con-
C              tain the indexes of a clockwise-ordered
C              sequence of vertices (first three rows)
C              followed by the LTRI column indexes of the
C              triangles opposite the vertices (or 0
C              denoting the exterior region) in the last
C              three rows.  This array is not generally of
C              any use.
C
C       LISTC = Array containing triangle indexes (indexes
C               to XC, YC, ZC, and RC) stored in 1-1 corres-
C               pondence with LIST/LPTR entries (or entries
C               that would be stored in LIST for the
C               extended triangulation):  the index of tri-
C               angle (N1,N2,N3) is stored in LISTC(K),
C               LISTC(L), and LISTC(M), where LIST(K),
C               LIST(L), and LIST(M) are the indexes of N2
C               as a neighbor of N1, N3 as a neighbor of N2,
C               and N1 as a neighbor of N3.  The Voronoi
C               region associated with a node is defined by
C               the CCW-ordered sequence of circumcenters in
C               one-to-one correspondence with its adjacency
C               list (in the extended triangulation).
C
C       NB = Number of boundary nodes unless IER = 1.
C
C       XC,YC,ZC = Arrays containing the Cartesian coordi-
C                  nates of the triangle circumcenters
C                  (Voronoi vertices).  XC(I)**2 + YC(I)**2
C                  + ZC(I)**2 = 1.  The first NB-2 entries
C                  correspond to pseudo-triangles if NB > 0.
C
C       RC = Array containing circumradii (the arc lengths
C            or angles between the circumcenters and associ-
C            ated triangle vertices) in 1-1 correspondence
C            with circumcenters.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if N < 3.
C             IER = 2 if NCOL < NB-2.
C             IER = 3 if a triangle is degenerate (has ver-
C                     tices lying on a common geodesic).
C
C Modules required by CRLIST:  CIRCUM, LSTPTR, SWPTST
C
C Intrinsic functions called by CRLIST:  ABS, ACOS
C
C***********************************************************
C
      INTEGER LSTPTR
      INTEGER I1, I2, I3, I4, IERR, KT, KT1, KT2, KT11,
     .        KT12, KT21, KT22, LP, LPL, LPN, N0, N1, N2,
     .        N3, N4, NM2, NN, NT
      LOGICAL SWPTST
      LOGICAL SWP
      REAL    C(3), T, V1(3), V2(3), V3(3)
C
C Local parameters:
C
C C =         Circumcenter returned by Subroutine CIRCUM
C I1,I2,I3 =  Permutation of (1,2,3):  LTRI row indexes
C I4 =        LTRI row index in the range 1 to 3
C IERR =      Error flag for calls to CIRCUM
C KT =        Triangle index
C KT1,KT2 =   Indexes of a pair of adjacent pseudo-triangles
C KT11,KT12 = Indexes of the pseudo-triangles opposite N1
C               and N2 as vertices of KT1
C KT21,KT22 = Indexes of the pseudo-triangles opposite N1
C               and N2 as vertices of KT2
C LP,LPN =    LIST pointers
C LPL =       LIST pointer of the last neighbor of N1
C N0 =        Index of the first boundary node (initial
C               value of N1) in the loop on boundary nodes
C               used to store the pseudo-triangle indexes
C               in LISTC
C N1,N2,N3 =  Nodal indexes defining a triangle (CCW order)
C               or pseudo-triangle (clockwise order)
C N4 =        Index of the node opposite N2 -> N1
C NM2 =       N-2
C NN =        Local copy of N
C NT =        Number of pseudo-triangles:  NB-2
C SWP =       Logical variable set to TRUE in each optimiza-
C               tion loop (loop on pseudo-arcs) iff a swap
C               is performed
C V1,V2,V3 =  Vertices of triangle KT = (N1,N2,N3) sent to
C               Subroutine CIRCUM
C
      NN = N
      NB = 0
      NT = 0
      IF (NN .LT. 3) GO TO 21
C
C Search for a boundary node N1.
C
      DO 1 N1 = 1,NN
        LP = LEND(N1)
        IF (LIST(LP) .LT. 0) GO TO 2
    1   CONTINUE
C
C The triangulation already covers the sphere.
C
      GO TO 9
C
C There are NB .GE. 3 boundary nodes.  Add NB-2 pseudo-
C   triangles (N1,N2,N3) by connecting N3 to the NB-3
C   boundary nodes to which it is not already adjacent.
C
C   Set N3 and N2 to the first and last neighbors,
C     respectively, of N1.
C
    2 N2 = -LIST(LP)
      LP = LPTR(LP)
      N3 = LIST(LP)
C
C   Loop on boundary arcs N1 -> N2 in clockwise order,
C     storing triangles (N1,N2,N3) in column NT of LTRI
C     along with the indexes of the triangles opposite
C     the vertices.
C
    3 NT = NT + 1
        IF (NT .LE. NCOL) THEN
          LTRI(1,NT) = N1
          LTRI(2,NT) = N2
          LTRI(3,NT) = N3
          LTRI(4,NT) = NT + 1
          LTRI(5,NT) = NT - 1
          LTRI(6,NT) = 0
        ENDIF
        N1 = N2
        LP = LEND(N1)
        N2 = -LIST(LP)
        IF (N2 .NE. N3) GO TO 3
C
      NB = NT + 2
      IF (NCOL .LT. NT) GO TO 22
      LTRI(4,NT) = 0
      IF (NT .EQ. 1) GO TO 7
C
C Optimize the exterior triangulation (set of pseudo-
C   triangles) by applying swaps to the pseudo-arcs N1-N2
C   (pairs of adjacent pseudo-triangles KT1 and KT2 > KT1).
C   The loop on pseudo-arcs is repeated until no swaps are
C   performed.
C
    4 SWP = .FALSE.
      DO 6 KT1 = 1,NT-1
        DO 5 I3 = 1,3
          KT2 = LTRI(I3+3,KT1)
          IF (KT2 .LE. KT1) GO TO 5
C
C   The LTRI row indexes (I1,I2,I3) of triangle KT1 =
C     (N1,N2,N3) are a cyclical permutation of (1,2,3).
C
          IF (I3 .EQ. 1) THEN
            I1 = 2
            I2 = 3
          ELSEIF (I3 .EQ. 2) THEN
            I1 = 3
            I2 = 1
          ELSE
            I1 = 1
            I2 = 2
          ENDIF
          N1 = LTRI(I1,KT1)
          N2 = LTRI(I2,KT1)
          N3 = LTRI(I3,KT1)
C
C   KT2 = (N2,N1,N4) for N4 = LTRI(I,KT2), where
C     LTRI(I+3,KT2) = KT1.
C
          IF (LTRI(4,KT2) .EQ. KT1) THEN
            I4 = 1
          ELSEIF (LTRI(5,KT2) .EQ. KT1) THEN
            I4 = 2
          ELSE
            I4 = 3
          ENDIF
          N4 = LTRI(I4,KT2)
C
C   The empty circumcircle test is reversed for the pseudo-
C     triangles.  The reversal is implicit in the clockwise
C     ordering of the vertices.
C
          IF ( .NOT. SWPTST(N1,N2,N3,N4,X,Y,Z) ) GO TO 5
C
C   Swap arc N1-N2 for N3-N4.  KTij is the triangle opposite
C     Nj as a vertex of KTi.
C
          SWP = .TRUE.
          KT11 = LTRI(I1+3,KT1)
          KT12 = LTRI(I2+3,KT1)
          IF (I4 .EQ. 1) THEN
            I2 = 2
            I1 = 3
          ELSEIF (I4 .EQ. 2) THEN
            I2 = 3
            I1 = 1
          ELSE
            I2 = 1
            I1 = 2
          ENDIF
          KT21 = LTRI(I1+3,KT2)
          KT22 = LTRI(I2+3,KT2)
          LTRI(1,KT1) = N4
          LTRI(2,KT1) = N3
          LTRI(3,KT1) = N1
          LTRI(4,KT1) = KT12
          LTRI(5,KT1) = KT22
          LTRI(6,KT1) = KT2
          LTRI(1,KT2) = N3
          LTRI(2,KT2) = N4
          LTRI(3,KT2) = N2
          LTRI(4,KT2) = KT21
          LTRI(5,KT2) = KT11
          LTRI(6,KT2) = KT1
C
C   Correct the KT11 and KT22 entries that changed.
C
          IF (KT11 .NE. 0) THEN
            I4 = 4
            IF (LTRI(4,KT11) .NE. KT1) THEN
              I4 = 5
              IF (LTRI(5,KT11) .NE. KT1) I4 = 6
            ENDIF
            LTRI(I4,KT11) = KT2
          ENDIF
          IF (KT22 .NE. 0) THEN
            I4 = 4
            IF (LTRI(4,KT22) .NE. KT2) THEN
              I4 = 5
              IF (LTRI(5,KT22) .NE. KT2) I4 = 6
            ENDIF
            LTRI(I4,KT22) = KT1
          ENDIF
    5     CONTINUE
    6   CONTINUE
      IF (SWP) GO TO 4
C
C Compute and store the negative circumcenters and radii of
C   the pseudo-triangles in the first NT positions.
C
    7 DO 8 KT = 1,NT
        N1 = LTRI(1,KT)
        N2 = LTRI(2,KT)
        N3 = LTRI(3,KT)
        V1(1) = X(N1)
        V1(2) = Y(N1)
        V1(3) = Z(N1)
        V2(1) = X(N2)
        V2(2) = Y(N2)
        V2(3) = Z(N2)
        V3(1) = X(N3)
        V3(2) = Y(N3)
        V3(3) = Z(N3)
        CALL CIRCUM (V1,V2,V3, C,IERR)
        IF (IERR .NE. 0) GO TO 23
C
C   Store the negative circumcenter and radius (computed
C     from <V1,C>).
C
        XC(KT) = C(1)
        YC(KT) = C(2)
        ZC(KT) = C(3)
        T = V1(1)*C(1) + V1(2)*C(2) + V1(3)*C(3)
        IF (T .LT. -1.0) T = -1.0
        IF (T .GT. 1.0) T = 1.0
        RC(KT) = ACOS(T)
    8   CONTINUE
C
C Compute and store the circumcenters and radii of the
C   actual triangles in positions KT = NT+1, NT+2, ...
C   Also, store the triangle indexes KT in the appropriate
C   LISTC positions.
C
    9 KT = NT
C
C   Loop on nodes N1.
C
      NM2 = NN - 2
      DO 12 N1 = 1,NM2
        LPL = LEND(N1)
        LP = LPL
        N3 = LIST(LP)
C
C   Loop on adjacent neighbors N2,N3 of N1 for which N2 > N1
C     and N3 > N1.
C
   10   LP = LPTR(LP)
          N2 = N3
          N3 = ABS(LIST(LP))
          IF (N2 .LE. N1  .OR.  N3 .LE. N1) GO TO 11
          KT = KT + 1
C
C   Compute the circumcenter C of triangle KT = (N1,N2,N3).
C
          V1(1) = X(N1)
          V1(2) = Y(N1)
          V1(3) = Z(N1)
          V2(1) = X(N2)
          V2(2) = Y(N2)
          V2(3) = Z(N2)
          V3(1) = X(N3)
          V3(2) = Y(N3)
          V3(3) = Z(N3)
          CALL CIRCUM (V1,V2,V3, C,IERR)
          IF (IERR .NE. 0) GO TO 23
C
C   Store the circumcenter, radius and triangle index.
C
          XC(KT) = C(1)
          YC(KT) = C(2)
          ZC(KT) = C(3)
          T = V1(1)*C(1) + V1(2)*C(2) + V1(3)*C(3)
          IF (T .LT. -1.0) T = -1.0
          IF (T .GT. 1.0) T = 1.0
          RC(KT) = ACOS(T)
C
C   Store KT in LISTC(LPN), where Abs(LIST(LPN)) is the
C     index of N2 as a neighbor of N1, N3 as a neighbor
C     of N2, and N1 as a neighbor of N3.
C
          LPN = LSTPTR(LPL,N2,LIST,LPTR)
          LISTC(LPN) = KT
          LPN = LSTPTR(LEND(N2),N3,LIST,LPTR)
          LISTC(LPN) = KT
          LPN = LSTPTR(LEND(N3),N1,LIST,LPTR)
          LISTC(LPN) = KT
   11     IF (LP .NE. LPL) GO TO 10
   12   CONTINUE
      IF (NT .EQ. 0) GO TO 20
C
C Store the first NT triangle indexes in LISTC.
C
C   Find a boundary triangle KT1 = (N1,N2,N3) with a
C     boundary arc opposite N3.
C
      KT1 = 0
   13 KT1 = KT1 + 1
      IF (LTRI(4,KT1) .EQ. 0) THEN
        I1 = 2
        I2 = 3
        I3 = 1
        GO TO 14
      ELSEIF (LTRI(5,KT1) .EQ. 0) THEN
        I1 = 3
        I2 = 1
        I3 = 2
        GO TO 14
      ELSEIF (LTRI(6,KT1) .EQ. 0) THEN
        I1 = 1
        I2 = 2
        I3 = 3
        GO TO 14
      ENDIF
      GO TO 13
   14 N1 = LTRI(I1,KT1)
      N0 = N1
C
C   Loop on boundary nodes N1 in CCW order, storing the
C     indexes of the clockwise-ordered sequence of triangles
C     that contain N1.  The first triangle overwrites the
C     last neighbor position, and the remaining triangles,
C     if any, are appended to N1's adjacency list.
C
C   A pointer to the first neighbor of N1 is saved in LPN.
C
   15 LP = LEND(N1)
      LPN = LPTR(LP)
      LISTC(LP) = KT1
C
C   Loop on triangles KT2 containing N1.
C
   16 KT2 = LTRI(I2+3,KT1)
      IF (KT2 .NE. 0) THEN
C
C   Append KT2 to N1's triangle list.
C
        LPTR(LP) = LNEW
        LP = LNEW
        LISTC(LP) = KT2
        LNEW = LNEW + 1
C
C   Set KT1 to KT2 and update (I1,I2,I3) such that
C     LTRI(I1,KT1) = N1.
C
        KT1 = KT2
        IF (LTRI(1,KT1) .EQ. N1) THEN
          I1 = 1
          I2 = 2
          I3 = 3
        ELSEIF (LTRI(2,KT1) .EQ. N1) THEN
          I1 = 2
          I2 = 3
          I3 = 1
        ELSE
          I1 = 3
          I2 = 1
          I3 = 2
        ENDIF
        GO TO 16
      ENDIF
C
C   Store the saved first-triangle pointer in LPTR(LP), set
C     N1 to the next boundary node, test for termination,
C     and permute the indexes:  the last triangle containing
C     a boundary node is the first triangle containing the
C     next boundary node.
C
      LPTR(LP) = LPN
      N1 = LTRI(I3,KT1)
      IF (N1 .NE. N0) THEN
        I4 = I3
        I3 = I2
        I2 = I1
        I1 = I4
        GO TO 15
      ENDIF
C
C No errors encountered.
C
   20 IER = 0
      RETURN
C
C N < 3.
C
   21 IER = 1
      RETURN
C
C Insufficient space reserved for LTRI.
C
   22 IER = 2
      RETURN
C
C Error flag returned by CIRCUM: KT indexes a null triangle.
C
   23 IER = 3
      RETURN
      END
      SUBROUTINE DELARC (N,IO1,IO2, LIST,LPTR,LEND,
     .                   LNEW, IER)
      INTEGER N, IO1, IO2, LIST(*), LPTR(*), LEND(N), LNEW,
     .        IER
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/17/96
C
C   This subroutine deletes a boundary arc from a triangula-
C tion.  It may be used to remove a null triangle from the
C convex hull boundary.  Note, however, that if the union of
C triangles is rendered nonconvex, Subroutines DELNOD, EDGE,
C and TRFIND (and hence ADDNOD) may fail.  Also, Function
C NEARND should not be called following an arc deletion.
C
C   This routine is identical to the similarly named routine
C in TRIPACK.
C
C
C On input:
C
C       N = Number of nodes in the triangulation.  N .GE. 4.
C
C       IO1,IO2 = Indexes (in the range 1 to N) of a pair of
C                 adjacent boundary nodes defining the arc
C                 to be removed.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR,LEND,LNEW = Triangulation data structure
C                             created by Subroutine TRMESH.
C
C On output:
C
C       LIST,LPTR,LEND,LNEW = Data structure updated with
C                             the removal of arc IO1-IO2
C                             unless IER > 0.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if N, IO1, or IO2 is outside its valid
C                     range, or IO1 = IO2.
C             IER = 2 if IO1-IO2 is not a boundary arc.
C             IER = 3 if the node opposite IO1-IO2 is al-
C                     ready a boundary node, and thus IO1
C                     or IO2 has only two neighbors or a
C                     deletion would result in two triangu-
C                     lations sharing a single node.
C             IER = 4 if one of the nodes is a neighbor of
C                     the other, but not vice versa, imply-
C                     ing an invalid triangulation data
C                     structure.
C
C Module required by DELARC:  DELNB, LSTPTR
C
C Intrinsic function called by DELARC:  ABS
C
C***********************************************************
C
      INTEGER LSTPTR
      INTEGER LP, LPH, LPL, N1, N2, N3
C
C Local parameters:
C
C LP =       LIST pointer
C LPH =      LIST pointer or flag returned by DELNB
C LPL =      Pointer to the last neighbor of N1, N2, or N3
C N1,N2,N3 = Nodal indexes of a triangle such that N1->N2
C              is the directed boundary edge associated
C              with IO1-IO2
C
      N1 = IO1
      N2 = IO2
C
C Test for errors, and set N1->N2 to the directed boundary
C   edge associated with IO1-IO2:  (N1,N2,N3) is a triangle
C   for some N3.
C
      IF (N .LT. 4  .OR.  N1 .LT. 1  .OR.  N1 .GT. N  .OR.
     .    N2 .LT. 1  .OR.  N2 .GT. N  .OR.  N1 .EQ. N2) THEN
        IER = 1
        RETURN
      ENDIF
C
      LPL = LEND(N2)
      IF (-LIST(LPL) .NE. N1) THEN
        N1 = N2
        N2 = IO1
        LPL = LEND(N2)
        IF (-LIST(LPL) .NE. N1) THEN
          IER = 2
          RETURN
        ENDIF
      ENDIF
C
C Set N3 to the node opposite N1->N2 (the second neighbor
C   of N1), and test for error 3 (N3 already a boundary
C   node).
C
      LPL = LEND(N1)
      LP = LPTR(LPL)
      LP = LPTR(LP)
      N3 = ABS(LIST(LP))
      LPL = LEND(N3)
      IF (LIST(LPL) .LE. 0) THEN
        IER = 3
        RETURN
      ENDIF
C
C Delete N2 as a neighbor of N1, making N3 the first
C   neighbor, and test for error 4 (N2 not a neighbor
C   of N1).  Note that previously computed pointers may
C   no longer be valid following the call to DELNB.
C
      CALL DELNB (N1,N2,N, LIST,LPTR,LEND,LNEW, LPH)
      IF (LPH .LT. 0) THEN
        IER = 4
        RETURN
      ENDIF
C
C Delete N1 as a neighbor of N2, making N3 the new last
C   neighbor.
C
      CALL DELNB (N2,N1,N, LIST,LPTR,LEND,LNEW, LPH)
C
C Make N3 a boundary node with first neighbor N2 and last
C   neighbor N1.
C
      LP = LSTPTR(LEND(N3),N1,LIST,LPTR)
      LEND(N3) = LP
      LIST(LP) = -N1
C
C No errors encountered.
C
      IER = 0
      RETURN
      END
      SUBROUTINE DELNB (N0,NB,N, LIST,LPTR,LEND,LNEW, LPH)
      INTEGER N0, NB, N, LIST(*), LPTR(*), LEND(N), LNEW,
     .        LPH
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/29/98
C
C   This subroutine deletes a neighbor NB from the adjacency
C list of node N0 (but N0 is not deleted from the adjacency
C list of NB) and, if NB is a boundary node, makes N0 a
C boundary node.  For pointer (LIST index) LPH to NB as a
C neighbor of N0, the empty LIST,LPTR location LPH is filled
C in with the values at LNEW-1, pointer LNEW-1 (in LPTR and
C possibly in LEND) is changed to LPH, and LNEW is decremen-
C ted.  This requires a search of LEND and LPTR entailing an
C expected operation count of O(N).
C
C   This routine is identical to the similarly named routine
C in TRIPACK.
C
C
C On input:
C
C       N0,NB = Indexes, in the range 1 to N, of a pair of
C               nodes such that NB is a neighbor of N0.
C               (N0 need not be a neighbor of NB.)
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR,LEND,LNEW = Data structure defining the
C                             triangulation.
C
C On output:
C
C       LIST,LPTR,LEND,LNEW = Data structure updated with
C                             the removal of NB from the ad-
C                             jacency list of N0 unless
C                             LPH < 0.
C
C       LPH = List pointer to the hole (NB as a neighbor of
C             N0) filled in by the values at LNEW-1 or error
C             indicator:
C             LPH > 0 if no errors were encountered.
C             LPH = -1 if N0, NB, or N is outside its valid
C                      range.
C             LPH = -2 if NB is not a neighbor of N0.
C
C Modules required by DELNB:  None
C
C Intrinsic function called by DELNB:  ABS
C
C***********************************************************
C
      INTEGER I, LNW, LP, LPB, LPL, LPP, NN
C
C Local parameters:
C
C I =   DO-loop index
C LNW = LNEW-1 (output value of LNEW)
C LP =  LIST pointer of the last neighbor of NB
C LPB = Pointer to NB as a neighbor of N0
C LPL = Pointer to the last neighbor of N0
C LPP = Pointer to the neighbor of N0 that precedes NB
C NN =  Local copy of N
C
      NN = N
C
C Test for error 1.
C
      IF (N0 .LT. 1  .OR.  N0 .GT. NN  .OR.  NB .LT. 1  .OR.
     .    NB .GT. NN  .OR.  NN .LT. 3) THEN
        LPH = -1
        RETURN
      ENDIF
C
C   Find pointers to neighbors of N0:
C
C     LPL points to the last neighbor,
C     LPP points to the neighbor NP preceding NB, and
C     LPB points to NB.
C
      LPL = LEND(N0)
      LPP = LPL
      LPB = LPTR(LPP)
    1 IF (LIST(LPB) .EQ. NB) GO TO 2
        LPP = LPB
        LPB = LPTR(LPP)
        IF (LPB .NE. LPL) GO TO 1
C
C   Test for error 2 (NB not found).
C
      IF (ABS(LIST(LPB)) .NE. NB) THEN
        LPH = -2
        RETURN
      ENDIF
C
C   NB is the last neighbor of N0.  Make NP the new last
C     neighbor and, if NB is a boundary node, then make N0
C     a boundary node.
C
      LEND(N0) = LPP
      LP = LEND(NB)
      IF (LIST(LP) .LT. 0) LIST(LPP) = -LIST(LPP)
      GO TO 3
C
C   NB is not the last neighbor of N0.  If NB is a boundary
C     node and N0 is not, then make N0 a boundary node with
C     last neighbor NP.
C
    2 LP = LEND(NB)
      IF (LIST(LP) .LT. 0  .AND.  LIST(LPL) .GT. 0) THEN
        LEND(N0) = LPP
        LIST(LPP) = -LIST(LPP)
      ENDIF
C
C   Update LPTR so that the neighbor following NB now fol-
C     lows NP, and fill in the hole at location LPB.
C
    3 LPTR(LPP) = LPTR(LPB)
      LNW = LNEW-1
      LIST(LPB) = LIST(LNW)
      LPTR(LPB) = LPTR(LNW)
      DO 4 I = NN,1,-1
        IF (LEND(I) .EQ. LNW) THEN
          LEND(I) = LPB
          GO TO 5
        ENDIF
    4   CONTINUE
C
    5 DO 6 I = 1,LNW-1
        IF (LPTR(I) .EQ. LNW) THEN
          LPTR(I) = LPB
        ENDIF
    6   CONTINUE
C
C No errors encountered.
C
      LNEW = LNW
      LPH = LPB
      RETURN
      END
      SUBROUTINE DELNOD (K, N,X,Y,Z,LIST,LPTR,LEND,LNEW,LWK,
     .                   IWK, IER)
      INTEGER K, N, LIST(*), LPTR(*), LEND(*), LNEW, LWK,
     .        IWK(2,*), IER
      REAL    X(*), Y(*), Z(*)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   11/30/99
C
C   This subroutine deletes node K (along with all arcs
C incident on node K) from a triangulation of N nodes on the
C unit sphere, and inserts arcs as necessary to produce a
C triangulation of the remaining N-1 nodes.  If a Delaunay
C triangulation is input, a Delaunay triangulation will
C result, and thus, DELNOD reverses the effect of a call to
C Subroutine ADDNOD.
C
C
C On input:
C
C       K = Index (for X, Y, and Z) of the node to be
C           deleted.  1 .LE. K .LE. N.
C
C K is not altered by this routine.
C
C       N = Number of nodes in the triangulation on input.
C           N .GE. 4.  Note that N will be decremented
C           following the deletion.
C
C       X,Y,Z = Arrays of length N containing the Cartesian
C               coordinates of the nodes in the triangula-
C               tion.
C
C       LIST,LPTR,LEND,LNEW = Data structure defining the
C                             triangulation.  Refer to Sub-
C                             routine TRMESH.
C
C       LWK = Number of columns reserved for IWK.  LWK must
C             be at least NNB-3, where NNB is the number of
C             neighbors of node K, including an extra
C             pseudo-node if K is a boundary node.
C
C       IWK = Integer work array dimensioned 2 by LWK (or
C             array of length .GE. 2*LWK).
C
C On output:
C
C       N = Number of nodes in the triangulation on output.
C           The input value is decremented unless 1 .LE. IER
C           .LE. 4.
C
C       X,Y,Z = Updated arrays containing nodal coordinates
C               (with elements K+1,...,N+1 shifted up one
C               position, thus overwriting element K) unless
C               1 .LE. IER .LE. 4.
C
C       LIST,LPTR,LEND,LNEW = Updated triangulation data
C                             structure reflecting the dele-
C                             tion unless 1 .LE. IER .LE. 4.
C                             Note that the data structure
C                             may have been altered if IER >
C                             3.
C
C       LWK = Number of IWK columns required unless IER = 1
C             or IER = 3.
C
C       IWK = Indexes of the endpoints of the new arcs added
C             unless LWK = 0 or 1 .LE. IER .LE. 4.  (Arcs
C             are associated with columns, or pairs of
C             adjacent elements if IWK is declared as a
C             singly-subscripted array.)
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if K or N is outside its valid range
C                     or LWK < 0 on input.
C             IER = 2 if more space is required in IWK.
C                     Refer to LWK.
C             IER = 3 if the triangulation data structure is
C                     invalid on input.
C             IER = 4 if K indexes an interior node with
C                     four or more neighbors, none of which
C                     can be swapped out due to collineari-
C                     ty, and K cannot therefore be deleted.
C             IER = 5 if an error flag (other than IER = 1)
C                     was returned by OPTIM.  An error
C                     message is written to the standard
C                     output unit in this case.
C             IER = 6 if error flag 1 was returned by OPTIM.
C                     This is not necessarily an error, but
C                     the arcs may not be optimal.
C
C   Note that the deletion may result in all remaining nodes
C being collinear.  This situation is not flagged.
C
C Modules required by DELNOD:  DELNB, LEFT, LSTPTR, NBCNT,
C                                OPTIM, SWAP, SWPTST
C
C Intrinsic function called by DELNOD:  ABS
C
C***********************************************************
C
      INTEGER LSTPTR, NBCNT
      INTEGER I, IERR, IWL, J, LNW, LP, LP21, LPF, LPH, LPL,
     .        LPL2, LPN, LWKL, N1, N2, NFRST, NIT, NL, NN,
     .        NNB, NR
      LOGICAL LEFT
      LOGICAL BDRY
      REAL    X1, X2, XL, XR, Y1, Y2, YL, YR, Z1, Z2, ZL, ZR
C
C Local parameters:
C
C BDRY =    Logical variable with value TRUE iff N1 is a
C             boundary node
C I,J =     DO-loop indexes
C IERR =    Error flag returned by OPTIM
C IWL =     Number of IWK columns containing arcs
C LNW =     Local copy of LNEW
C LP =      LIST pointer
C LP21 =    LIST pointer returned by SWAP
C LPF,LPL = Pointers to the first and last neighbors of N1
C LPH =     Pointer (or flag) returned by DELNB
C LPL2 =    Pointer to the last neighbor of N2
C LPN =     Pointer to a neighbor of N1
C LWKL =    Input value of LWK
C N1 =      Local copy of K
C N2 =      Neighbor of N1
C NFRST =   First neighbor of N1:  LIST(LPF)
C NIT =     Number of iterations in OPTIM
C NR,NL =   Neighbors of N1 preceding (to the right of) and
C             following (to the left of) N2, respectively
C NN =      Number of nodes in the triangulation
C NNB =     Number of neighbors of N1 (including a pseudo-
C             node representing the boundary if N1 is a
C             boundary node)
C X1,Y1,Z1 = Coordinates of N1
C X2,Y2,Z2 = Coordinates of N2
C XL,YL,ZL = Coordinates of NL
C XR,YR,ZR = Coordinates of NR
C
C
C Set N1 to K and NNB to the number of neighbors of N1 (plus
C   one if N1 is a boundary node), and test for errors.  LPF
C   and LPL are LIST indexes of the first and last neighbors
C   of N1, IWL is the number of IWK columns containing arcs,
C   and BDRY is TRUE iff N1 is a boundary node.
C
      N1 = K
      NN = N
      IF (N1 .LT. 1  .OR.  N1 .GT. NN  .OR.  NN .LT. 4  .OR.
     .    LWK .LT. 0) GO TO 21
      LPL = LEND(N1)
      LPF = LPTR(LPL)
      NNB = NBCNT(LPL,LPTR)
      BDRY = LIST(LPL) .LT. 0
      IF (BDRY) NNB = NNB + 1
      IF (NNB .LT. 3) GO TO 23
      LWKL = LWK
      LWK = NNB - 3
      IF (LWKL .LT. LWK) GO TO 22
      IWL = 0
      IF (NNB .EQ. 3) GO TO 3
C
C Initialize for loop on arcs N1-N2 for neighbors N2 of N1,
C   beginning with the second neighbor.  NR and NL are the
C   neighbors preceding and following N2, respectively, and
C   LP indexes NL.  The loop is exited when all possible
C   swaps have been applied to arcs incident on N1.
C
      X1 = X(N1)
      Y1 = Y(N1)
      Z1 = Z(N1)
      NFRST = LIST(LPF)
      NR = NFRST
      XR = X(NR)
      YR = Y(NR)
      ZR = Z(NR)
      LP = LPTR(LPF)
      N2 = LIST(LP)
      X2 = X(N2)
      Y2 = Y(N2)
      Z2 = Z(N2)
      LP = LPTR(LP)
C
C Top of loop:  set NL to the neighbor following N2.
C
    1 NL = ABS(LIST(LP))
      IF (NL .EQ. NFRST  .AND.  BDRY) GO TO 3
      XL = X(NL)
      YL = Y(NL)
      ZL = Z(NL)
C
C   Test for a convex quadrilateral.  To avoid an incorrect
C     test caused by collinearity, use the fact that if N1
C     is a boundary node, then N1 LEFT NR->NL and if N2 is
C     a boundary node, then N2 LEFT NL->NR.
C
      LPL2 = LEND(N2)
      IF ( .NOT. ((BDRY  .OR.  LEFT(XR,YR,ZR,XL,YL,ZL,X1,Y1,
     .      Z1))  .AND.  (LIST(LPL2) .LT. 0  .OR.
     .      LEFT(XL,YL,ZL,XR,YR,ZR,X2,Y2,Z2))) ) THEN
C
C   Nonconvex quadrilateral -- no swap is possible.
C
        NR = N2
        XR = X2
        YR = Y2
        ZR = Z2
        GO TO 2
      ENDIF
C
C   The quadrilateral defined by adjacent triangles
C     (N1,N2,NL) and (N2,N1,NR) is convex.  Swap in
C     NL-NR and store it in IWK unless NL and NR are
C     already adjacent, in which case the swap is not
C     possible.  Indexes larger than N1 must be decremented
C     since N1 will be deleted from X, Y, and Z.
C
      CALL SWAP (NL,NR,N1,N2, LIST,LPTR,LEND, LP21)
      IF (LP21 .EQ. 0) THEN
        NR = N2
        XR = X2
        YR = Y2
        ZR = Z2
        GO TO 2
      ENDIF
      IWL = IWL + 1
      IF (NL .LE. N1) THEN
        IWK(1,IWL) = NL
      ELSE
        IWK(1,IWL) = NL - 1
      ENDIF
      IF (NR .LE. N1) THEN
        IWK(2,IWL) = NR
      ELSE
        IWK(2,IWL) = NR - 1
      ENDIF
C
C   Recompute the LIST indexes and NFRST, and decrement NNB.
C
      LPL = LEND(N1)
      NNB = NNB - 1
      IF (NNB .EQ. 3) GO TO 3
      LPF = LPTR(LPL)
      NFRST = LIST(LPF)
      LP = LSTPTR(LPL,NL,LIST,LPTR)
      IF (NR .EQ. NFRST) GO TO 2
C
C   NR is not the first neighbor of N1.
C     Back up and test N1-NR for a swap again:  Set N2 to
C     NR and NR to the previous neighbor of N1 -- the
C     neighbor of NR which follows N1.  LP21 points to NL
C     as a neighbor of NR.
C
      N2 = NR
      X2 = XR
      Y2 = YR
      Z2 = ZR
      LP21 = LPTR(LP21)
      LP21 = LPTR(LP21)
      NR = ABS(LIST(LP21))
      XR = X(NR)
      YR = Y(NR)
      ZR = Z(NR)
      GO TO 1
C
C   Bottom of loop -- test for termination of loop.
C
    2 IF (N2 .EQ. NFRST) GO TO 3
      N2 = NL
      X2 = XL
      Y2 = YL
      Z2 = ZL
      LP = LPTR(LP)
      GO TO 1
C
C Delete N1 and all its incident arcs.  If N1 is an interior
C   node and either NNB > 3 or NNB = 3 and N2 LEFT NR->NL,
C   then N1 must be separated from its neighbors by a plane
C   containing the origin -- its removal reverses the effect
C   of a call to COVSPH, and all its neighbors become
C   boundary nodes.  This is achieved by treating it as if
C   it were a boundary node (setting BDRY to TRUE, changing
C   a sign in LIST, and incrementing NNB).
C
    3 IF (.NOT. BDRY) THEN
        IF (NNB .GT. 3) THEN
          BDRY = .TRUE.
        ELSE
          LPF = LPTR(LPL)
          NR = LIST(LPF)
          LP = LPTR(LPF)
          N2 = LIST(LP)
          NL = LIST(LPL)
          BDRY = LEFT(X(NR),Y(NR),Z(NR),X(NL),Y(NL),Z(NL),
     .                X(N2),Y(N2),Z(N2))
        ENDIF
        IF (BDRY) THEN
C
C   IF a boundary node already exists, then N1 and its
C     neighbors cannot be converted to boundary nodes.
C     (They must be collinear.)  This is a problem if
C     NNB > 3.
C
          DO 4 I = 1,NN
            IF (LIST(LEND(I)) .LT. 0) THEN
              BDRY = .FALSE.
              GO TO 5
            ENDIF
    4       CONTINUE
          LIST(LPL) = -LIST(LPL)
          NNB = NNB + 1
        ENDIF
      ENDIF
    5 IF (.NOT. BDRY  .AND.  NNB .GT. 3) GO TO 24
C
C Initialize for loop on neighbors.  LPL points to the last
C   neighbor of N1.  LNEW is stored in local variable LNW.
C
      LP = LPL
      LNW = LNEW
C
C Loop on neighbors N2 of N1, beginning with the first.
C
    6 LP = LPTR(LP)
        N2 = ABS(LIST(LP))
        CALL DELNB (N2,N1,N, LIST,LPTR,LEND,LNW, LPH)
        IF (LPH .LT. 0) GO TO 23
C
C   LP and LPL may require alteration.
C
        IF (LPL .EQ. LNW) LPL = LPH
        IF (LP .EQ. LNW) LP = LPH
        IF (LP .NE. LPL) GO TO 6
C
C Delete N1 from X, Y, Z, and LEND, and remove its adjacency
C   list from LIST and LPTR.  LIST entries (nodal indexes)
C   which are larger than N1 must be decremented.
C
      NN = NN - 1
      IF (N1 .GT. NN) GO TO 9
      DO 7 I = N1,NN
        X(I) = X(I+1)
        Y(I) = Y(I+1)
        Z(I) = Z(I+1)
        LEND(I) = LEND(I+1)
    7   CONTINUE
C
      DO 8 I = 1,LNW-1
        IF (LIST(I) .GT. N1) LIST(I) = LIST(I) - 1
        IF (LIST(I) .LT. -N1) LIST(I) = LIST(I) + 1
    8   CONTINUE
C
C   For LPN = first to last neighbors of N1, delete the
C     preceding neighbor (indexed by LP).
C
C   Each empty LIST,LPTR location LP is filled in with the
C     values at LNW-1, and LNW is decremented.  All pointers
C     (including those in LPTR and LEND) with value LNW-1
C     must be changed to LP.
C
C  LPL points to the last neighbor of N1.
C
    9 IF (BDRY) NNB = NNB - 1
      LPN = LPL
      DO 13 J = 1,NNB
        LNW = LNW - 1
        LP = LPN
        LPN = LPTR(LP)
        LIST(LP) = LIST(LNW)
        LPTR(LP) = LPTR(LNW)
        IF (LPTR(LPN) .EQ. LNW) LPTR(LPN) = LP
        IF (LPN .EQ. LNW) LPN = LP
        DO 10 I = NN,1,-1
          IF (LEND(I) .EQ. LNW) THEN
            LEND(I) = LP
            GO TO 11
          ENDIF
   10     CONTINUE
C
   11   DO 12 I = LNW-1,1,-1
          IF (LPTR(I) .EQ. LNW) LPTR(I) = LP
   12     CONTINUE
   13   CONTINUE
C
C Update N and LNEW, and optimize the patch of triangles
C   containing K (on input) by applying swaps to the arcs
C   in IWK.
C
      N = NN
      LNEW = LNW
      IF (IWL .GT. 0) THEN
        NIT = 4*IWL
        CALL OPTIM (X,Y,Z,IWL, LIST,LPTR,LEND,NIT,IWK, IERR)
        IF (IERR .NE. 0  .AND.  IERR .NE. 1) GO TO 25
        IF (IERR .EQ. 1) GO TO 26
      ENDIF
C
C Successful termination.
C
      IER = 0
      RETURN
C
C Invalid input parameter.
C
   21 IER = 1
      RETURN
C
C Insufficient space reserved for IWK.
C
   22 IER = 2
      RETURN
C
C Invalid triangulation data structure.  NNB < 3 on input or
C   N2 is a neighbor of N1 but N1 is not a neighbor of N2.
C
   23 IER = 3
      RETURN
C
C N1 is interior but NNB could not be reduced to 3.
C
   24 IER = 4
      RETURN
C
C Error flag (other than 1) returned by OPTIM.
C
   25 IER = 5
      WRITE (*,100) NIT, IERR
  100 FORMAT (//5X,'*** Error in OPTIM (called from ',
     .        'DELNOD):  NIT = ',I4,', IER = ',I1,' ***'/)
      RETURN
C
C Error flag 1 returned by OPTIM.
C
   26 IER = 6
      RETURN
      END
      SUBROUTINE EDGE (IN1,IN2,X,Y,Z, LWK,IWK,LIST,LPTR,
     .                 LEND, IER)
      INTEGER IN1, IN2, LWK, IWK(2,*), LIST(*), LPTR(*),
     .        LEND(*), IER
      REAL    X(*), Y(*), Z(*)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/30/98
C
C   Given a triangulation of N nodes and a pair of nodal
C indexes IN1 and IN2, this routine swaps arcs as necessary
C to force IN1 and IN2 to be adjacent.  Only arcs which
C intersect IN1-IN2 are swapped out.  If a Delaunay triangu-
C lation is input, the resulting triangulation is as close
C as possible to a Delaunay triangulation in the sense that
C all arcs other than IN1-IN2 are locally optimal.
C
C   A sequence of calls to EDGE may be used to force the
C presence of a set of edges defining the boundary of a non-
C convex and/or multiply connected region, or to introduce
C barriers into the triangulation.  Note that Subroutine
C GETNP will not necessarily return closest nodes if the
C triangulation has been constrained by a call to EDGE.
C However, this is appropriate in some applications, such
C as triangle-based interpolation on a nonconvex domain.
C
C
C On input:
C
C       IN1,IN2 = Indexes (of X, Y, and Z) in the range 1 to
C                 N defining a pair of nodes to be connected
C                 by an arc.
C
C       X,Y,Z = Arrays of length N containing the Cartesian
C               coordinates of the nodes.
C
C The above parameters are not altered by this routine.
C
C       LWK = Number of columns reserved for IWK.  This must
C             be at least NI -- the number of arcs that
C             intersect IN1-IN2.  (NI is bounded by N-3.)
C
C       IWK = Integer work array of length at least 2*LWK.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to Subroutine
C                        TRMESH.
C
C On output:
C
C       LWK = Number of arcs which intersect IN1-IN2 (but
C             not more than the input value of LWK) unless
C             IER = 1 or IER = 3.  LWK = 0 if and only if
C             IN1 and IN2 were adjacent (or LWK=0) on input.
C
C       IWK = Array containing the indexes of the endpoints
C             of the new arcs other than IN1-IN2 unless
C             IER > 0 or LWK = 0.  New arcs to the left of
C             IN1->IN2 are stored in the first K-1 columns
C             (left portion of IWK), column K contains
C             zeros, and new arcs to the right of IN1->IN2
C             occupy columns K+1,...,LWK.  (K can be deter-
C             mined by searching IWK for the zeros.)
C
C       LIST,LPTR,LEND = Data structure updated if necessary
C                        to reflect the presence of an arc
C                        connecting IN1 and IN2 unless IER >
C                        0.  The data structure has been
C                        altered if IER >= 4.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if IN1 < 1, IN2 < 1, IN1 = IN2,
C                     or LWK < 0 on input.
C             IER = 2 if more space is required in IWK.
C                     Refer to LWK.
C             IER = 3 if IN1 and IN2 could not be connected
C                     due to either an invalid data struc-
C                     ture or collinear nodes (and floating
C                     point error).
C             IER = 4 if an error flag other than IER = 1
C                     was returned by OPTIM.
C             IER = 5 if error flag 1 was returned by OPTIM.
C                     This is not necessarily an error, but
C                     the arcs other than IN1-IN2 may not
C                     be optimal.
C
C   An error message is written to the standard output unit
C in the case of IER = 3 or IER = 4.
C
C Modules required by EDGE:  LEFT, LSTPTR, OPTIM, SWAP,
C                              SWPTST
C
C Intrinsic function called by EDGE:  ABS
C
C***********************************************************
C
      LOGICAL LEFT
      INTEGER I, IERR, IWC, IWCP1, IWEND, IWF, IWL, LFT, LP,
     .        LP21, LPL, N0, N1, N1FRST, N1LST, N2, NEXT,
     .        NIT, NL, NR
      REAL    DP12, DP1L, DP1R, DP2L, DP2R, X0, X1, X2, Y0,
     .        Y1, Y2, Z0, Z1, Z2
C
C Local parameters:
C
C DPij =     Dot product <Ni,Nj>
C I =        DO-loop index and column index for IWK
C IERR =     Error flag returned by Subroutine OPTIM
C IWC =      IWK index between IWF and IWL -- NL->NR is
C              stored in IWK(1,IWC)->IWK(2,IWC)
C IWCP1 =    IWC + 1
C IWEND =    Input or output value of LWK
C IWF =      IWK (column) index of the first (leftmost) arc
C              which intersects IN1->IN2
C IWL =      IWK (column) index of the last (rightmost) are
C              which intersects IN1->IN2
C LFT =      Flag used to determine if a swap results in the
C              new arc intersecting IN1-IN2 -- LFT = 0 iff
C              N0 = IN1, LFT = -1 implies N0 LEFT IN1->IN2,
C              and LFT = 1 implies N0 LEFT IN2->IN1
C LP =       List pointer (index for LIST and LPTR)
C LP21 =     Unused parameter returned by SWAP
C LPL =      Pointer to the last neighbor of IN1 or NL
C N0 =       Neighbor of N1 or node opposite NR->NL
C N1,N2 =    Local copies of IN1 and IN2
C N1FRST =   First neighbor of IN1
C N1LST =    (Signed) last neighbor of IN1
C NEXT =     Node opposite NL->NR
C NIT =      Flag or number of iterations employed by OPTIM
C NL,NR =    Endpoints of an arc which intersects IN1-IN2
C              with NL LEFT IN1->IN2
C X0,Y0,Z0 = Coordinates of N0
C X1,Y1,Z1 = Coordinates of IN1
C X2,Y2,Z2 = Coordinates of IN2
C
C
C Store IN1, IN2, and LWK in local variables and test for
C   errors.
C
      N1 = IN1
      N2 = IN2
      IWEND = LWK
      IF (N1 .LT. 1  .OR.  N2 .LT. 1  .OR.  N1 .EQ. N2  .OR.
     .    IWEND .LT. 0) GO TO 31
C
C Test for N2 as a neighbor of N1.  LPL points to the last
C   neighbor of N1.
C
      LPL = LEND(N1)
      N0 = ABS(LIST(LPL))
      LP = LPL
    1 IF (N0 .EQ. N2) GO TO 30
        LP = LPTR(LP)
        N0 = LIST(LP)
        IF (LP .NE. LPL) GO TO 1
C
C Initialize parameters.
C
      IWL = 0
      NIT = 0
C
C Store the coordinates of N1 and N2.
C
    2 X1 = X(N1)
      Y1 = Y(N1)
      Z1 = Z(N1)
      X2 = X(N2)
      Y2 = Y(N2)
      Z2 = Z(N2)
C
C Set NR and NL to adjacent neighbors of N1 such that
C   NR LEFT N2->N1 and NL LEFT N1->N2,
C   (NR Forward N1->N2 or NL Forward N1->N2), and
C   (NR Forward N2->N1 or NL Forward N2->N1).
C
C   Initialization:  Set N1FRST and N1LST to the first and
C     (signed) last neighbors of N1, respectively, and
C     initialize NL to N1FRST.
C
      LPL = LEND(N1)
      N1LST = LIST(LPL)
      LP = LPTR(LPL)
      N1FRST = LIST(LP)
      NL = N1FRST
      IF (N1LST .LT. 0) GO TO 4
C
C   N1 is an interior node.  Set NL to the first candidate
C     for NR (NL LEFT N2->N1).
C
    3 IF (LEFT(X2,Y2,Z2,X1,Y1,Z1,X(NL),Y(NL),Z(NL))) GO TO 4
        LP = LPTR(LP)
        NL = LIST(LP)
        IF (NL .NE. N1FRST) GO TO 3
C
C   All neighbors of N1 are strictly left of N1->N2.
C
      GO TO 5
C
C   NL = LIST(LP) LEFT N2->N1.  Set NR to NL and NL to the
C     following neighbor of N1.
C
    4 NR = NL
        LP = LPTR(LP)
        NL = ABS(LIST(LP))
        IF (LEFT(X1,Y1,Z1,X2,Y2,Z2,X(NL),Y(NL),Z(NL)) ) THEN
C
C   NL LEFT N1->N2 and NR LEFT N2->N1.  The Forward tests
C     are employed to avoid an error associated with
C     collinear nodes.
C
          DP12 = X1*X2 + Y1*Y2 + Z1*Z2
          DP1L = X1*X(NL) + Y1*Y(NL) + Z1*Z(NL)
          DP2L = X2*X(NL) + Y2*Y(NL) + Z2*Z(NL)
          DP1R = X1*X(NR) + Y1*Y(NR) + Z1*Z(NR)
          DP2R = X2*X(NR) + Y2*Y(NR) + Z2*Z(NR)
          IF ( (DP2L-DP12*DP1L .GE. 0.  .OR.
     .          DP2R-DP12*DP1R .GE. 0.)  .AND.
     .         (DP1L-DP12*DP2L .GE. 0.  .OR.
     .          DP1R-DP12*DP2R .GE. 0.) ) GO TO 6
C
C   NL-NR does not intersect N1-N2.  However, there is
C     another candidate for the first arc if NL lies on
C     the line N1-N2.
C
          IF ( .NOT. LEFT(X2,Y2,Z2,X1,Y1,Z1,X(NL),Y(NL),
     .                    Z(NL)) ) GO TO 5
        ENDIF
C
C   Bottom of loop.
C
        IF (NL .NE. N1FRST) GO TO 4
C
C Either the triangulation is invalid or N1-N2 lies on the
C   convex hull boundary and an edge NR->NL (opposite N1 and
C   intersecting N1-N2) was not found due to floating point
C   error.  Try interchanging N1 and N2 -- NIT > 0 iff this
C   has already been done.
C
    5 IF (NIT .GT. 0) GO TO 33
      NIT = 1
      N1 = N2
      N2 = IN1
      GO TO 2
C
C Store the ordered sequence of intersecting edges NL->NR in
C   IWK(1,IWL)->IWK(2,IWL).
C
    6 IWL = IWL + 1
      IF (IWL .GT. IWEND) GO TO 32
      IWK(1,IWL) = NL
      IWK(2,IWL) = NR
C
C   Set NEXT to the neighbor of NL which follows NR.
C
      LPL = LEND(NL)
      LP = LPTR(LPL)
C
C   Find NR as a neighbor of NL.  The search begins with
C     the first neighbor.
C
    7 IF (LIST(LP) .EQ. NR) GO TO 8
        LP = LPTR(LP)
        IF (LP .NE. LPL) GO TO 7
C
C   NR must be the last neighbor, and NL->NR cannot be a
C     boundary edge.
C
      IF (LIST(LP) .NE. NR) GO TO 33
C
C   Set NEXT to the neighbor following NR, and test for
C     termination of the store loop.
C
    8 LP = LPTR(LP)
      NEXT = ABS(LIST(LP))
      IF (NEXT .EQ. N2) GO TO 9
C
C   Set NL or NR to NEXT.
C
      IF ( LEFT(X1,Y1,Z1,X2,Y2,Z2,X(NEXT),Y(NEXT),Z(NEXT)) )
     .    THEN
        NL = NEXT
      ELSE
        NR = NEXT
      ENDIF
      GO TO 6
C
C IWL is the number of arcs which intersect N1-N2.
C   Store LWK.
C
    9 LWK = IWL
      IWEND = IWL
C
C Initialize for edge swapping loop -- all possible swaps
C   are applied (even if the new arc again intersects
C   N1-N2), arcs to the left of N1->N2 are stored in the
C   left portion of IWK, and arcs to the right are stored in
C   the right portion.  IWF and IWL index the first and last
C   intersecting arcs.
C
      IWF = 1
C
C Top of loop -- set N0 to N1 and NL->NR to the first edge.
C   IWC points to the arc currently being processed.  LFT
C   .LE. 0 iff N0 LEFT N1->N2.
C
   10 LFT = 0
      N0 = N1
      X0 = X1
      Y0 = Y1
      Z0 = Z1
      NL = IWK(1,IWF)
      NR = IWK(2,IWF)
      IWC = IWF
C
C   Set NEXT to the node opposite NL->NR unless IWC is the
C     last arc.
C
   11 IF (IWC .EQ. IWL) GO TO 21
      IWCP1 = IWC + 1
      NEXT = IWK(1,IWCP1)
      IF (NEXT .NE. NL) GO TO 16
      NEXT = IWK(2,IWCP1)
C
C   NEXT RIGHT N1->N2 and IWC .LT. IWL.  Test for a possible
C     swap.
C
      IF ( .NOT. LEFT(X0,Y0,Z0,X(NR),Y(NR),Z(NR),X(NEXT),
     .                Y(NEXT),Z(NEXT)) ) GO TO 14
      IF (LFT .GE. 0) GO TO 12
      IF ( .NOT. LEFT(X(NL),Y(NL),Z(NL),X0,Y0,Z0,X(NEXT),
     .                Y(NEXT),Z(NEXT)) ) GO TO 14
C
C   Replace NL->NR with N0->NEXT.
C
      CALL SWAP (NEXT,N0,NL,NR, LIST,LPTR,LEND, LP21)
      IWK(1,IWC) = N0
      IWK(2,IWC) = NEXT
      GO TO 15
C
C   Swap NL-NR for N0-NEXT, shift columns IWC+1,...,IWL to
C     the left, and store N0-NEXT in the right portion of
C     IWK.
C
   12 CALL SWAP (NEXT,N0,NL,NR, LIST,LPTR,LEND, LP21)
      DO 13 I = IWCP1,IWL
        IWK(1,I-1) = IWK(1,I)
        IWK(2,I-1) = IWK(2,I)
   13   CONTINUE
      IWK(1,IWL) = N0
      IWK(2,IWL) = NEXT
      IWL = IWL - 1
      NR = NEXT
      GO TO 11
C
C   A swap is not possible.  Set N0 to NR.
C
   14 N0 = NR
      X0 = X(N0)
      Y0 = Y(N0)
      Z0 = Z(N0)
      LFT = 1
C
C   Advance to the next arc.
C
   15 NR = NEXT
      IWC = IWC + 1
      GO TO 11
C
C   NEXT LEFT N1->N2, NEXT .NE. N2, and IWC .LT. IWL.
C     Test for a possible swap.
C
   16 IF ( .NOT. LEFT(X(NL),Y(NL),Z(NL),X0,Y0,Z0,X(NEXT),
     .                Y(NEXT),Z(NEXT)) ) GO TO 19
      IF (LFT .LE. 0) GO TO 17
      IF ( .NOT. LEFT(X0,Y0,Z0,X(NR),Y(NR),Z(NR),X(NEXT),
     .                Y(NEXT),Z(NEXT)) ) GO TO 19
C
C   Replace NL->NR with NEXT->N0.
C
      CALL SWAP (NEXT,N0,NL,NR, LIST,LPTR,LEND, LP21)
      IWK(1,IWC) = NEXT
      IWK(2,IWC) = N0
      GO TO 20
C
C   Swap NL-NR for N0-NEXT, shift columns IWF,...,IWC-1 to
C     the right, and store N0-NEXT in the left portion of
C     IWK.
C
   17 CALL SWAP (NEXT,N0,NL,NR, LIST,LPTR,LEND, LP21)
      DO 18 I = IWC-1,IWF,-1
        IWK(1,I+1) = IWK(1,I)
        IWK(2,I+1) = IWK(2,I)
   18   CONTINUE
      IWK(1,IWF) = N0
      IWK(2,IWF) = NEXT
      IWF = IWF + 1
      GO TO 20
C
C   A swap is not possible.  Set N0 to NL.
C
   19 N0 = NL
      X0 = X(N0)
      Y0 = Y(N0)
      Z0 = Z(N0)
      LFT = -1
C
C   Advance to the next arc.
C
   20 NL = NEXT
      IWC = IWC + 1
      GO TO 11
C
C   N2 is opposite NL->NR (IWC = IWL).
C
   21 IF (N0 .EQ. N1) GO TO 24
      IF (LFT .LT. 0) GO TO 22
C
C   N0 RIGHT N1->N2.  Test for a possible swap.
C
      IF ( .NOT. LEFT(X0,Y0,Z0,X(NR),Y(NR),Z(NR),X2,Y2,Z2) )
     .  GO TO 10
C
C   Swap NL-NR for N0-N2 and store N0-N2 in the right
C     portion of IWK.
C
      CALL SWAP (N2,N0,NL,NR, LIST,LPTR,LEND, LP21)
      IWK(1,IWL) = N0
      IWK(2,IWL) = N2
      IWL = IWL - 1
      GO TO 10
C
C   N0 LEFT N1->N2.  Test for a possible swap.
C
   22 IF ( .NOT. LEFT(X(NL),Y(NL),Z(NL),X0,Y0,Z0,X2,Y2,Z2) )
     .  GO TO 10
C
C   Swap NL-NR for N0-N2, shift columns IWF,...,IWL-1 to the
C     right, and store N0-N2 in the left portion of IWK.
C
      CALL SWAP (N2,N0,NL,NR, LIST,LPTR,LEND, LP21)
      I = IWL
   23 IWK(1,I) = IWK(1,I-1)
      IWK(2,I) = IWK(2,I-1)
      I = I - 1
      IF (I .GT. IWF) GO TO 23
      IWK(1,IWF) = N0
      IWK(2,IWF) = N2
      IWF = IWF + 1
      GO TO 10
C
C IWF = IWC = IWL.  Swap out the last arc for N1-N2 and
C   store zeros in IWK.
C
   24 CALL SWAP (N2,N1,NL,NR, LIST,LPTR,LEND, LP21)
      IWK(1,IWC) = 0
      IWK(2,IWC) = 0
C
C Optimization procedure --
C
      IER = 0
      IF (IWC .GT. 1) THEN
C
C   Optimize the set of new arcs to the left of IN1->IN2.
C
        NIT = 4*(IWC-1)
        CALL OPTIM (X,Y,Z,IWC-1, LIST,LPTR,LEND,NIT,
     .              IWK, IERR)
        IF (IERR .NE. 0  .AND.  IERR .NE. 1) GO TO 34
        IF (IERR .EQ. 1) IER = 5
      ENDIF
      IF (IWC .LT. IWEND) THEN
C
C   Optimize the set of new arcs to the right of IN1->IN2.
C
        NIT = 4*(IWEND-IWC)
        CALL OPTIM (X,Y,Z,IWEND-IWC, LIST,LPTR,LEND,NIT,
     .              IWK(1,IWC+1), IERR)
        IF (IERR .NE. 0  .AND.  IERR .NE. 1) GO TO 34
        IF (IERR .EQ. 1) GO TO 35
      ENDIF
      IF (IER .EQ. 5) GO TO 35
C
C Successful termination (IER = 0).
C
      RETURN
C
C IN1 and IN2 were adjacent on input.
C
   30 IER = 0
      RETURN
C
C Invalid input parameter.
C
   31 IER = 1
      RETURN
C
C Insufficient space reserved for IWK.
C
   32 IER = 2
      RETURN
C
C Invalid triangulation data structure or collinear nodes
C   on convex hull boundary.
C
   33 IER = 3
      WRITE (*,130) IN1, IN2
  130 FORMAT (//5X,'*** Error in EDGE:  Invalid triangula',
     .        'tion or null triangles on boundary'/
     .        9X,'IN1 =',I4,', IN2=',I4/)
      RETURN
C
C Error flag (other than 1) returned by OPTIM.
C
   34 IER = 4
      WRITE (*,140) NIT, IERR
  140 FORMAT (//5X,'*** Error in OPTIM (called from EDGE):',
     .        '  NIT = ',I4,', IER = ',I1,' ***'/)
      RETURN
C
C Error flag 1 returned by OPTIM.
C
   35 IER = 5
      RETURN
      END
      SUBROUTINE GETNP (X,Y,Z,LIST,LPTR,LEND,L, NPTS, DF,
     .                  IER)
      INTEGER LIST(*), LPTR(*), LEND(*), L, NPTS(L), IER
      REAL    X(*), Y(*), Z(*), DF
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/28/98
C
C   Given a Delaunay triangulation of N nodes on the unit
C sphere and an array NPTS containing the indexes of L-1
C nodes ordered by angular distance from NPTS(1), this sub-
C routine sets NPTS(L) to the index of the next node in the
C sequence -- the node, other than NPTS(1),...,NPTS(L-1),
C that is closest to NPTS(1).  Thus, the ordered sequence
C of K closest nodes to N1 (including N1) may be determined
C by K-1 calls to GETNP with NPTS(1) = N1 and L = 2,3,...,K
C for K .GE. 2.
C
C   The algorithm uses the property of a Delaunay triangula-
C tion that the K-th closest node to N1 is a neighbor of one
C of the K-1 closest nodes to N1.
C
C
C On input:
C
C       X,Y,Z = Arrays of length N containing the Cartesian
C               coordinates of the nodes.
C
C       LIST,LPTR,LEND = Triangulation data structure.  Re-
C                        fer to Subroutine TRMESH.
C
C       L = Number of nodes in the sequence on output.  2
C           .LE. L .LE. N.
C
C The above parameters are not altered by this routine.
C
C       NPTS = Array of length .GE. L containing the indexes
C              of the L-1 closest nodes to NPTS(1) in the
C              first L-1 locations.
C
C On output:
C
C       NPTS = Array updated with the index of the L-th
C              closest node to NPTS(1) in position L unless
C              IER = 1.
C
C       DF = Value of an increasing function (negative cos-
C            ine) of the angular distance between NPTS(1)
C            and NPTS(L) unless IER = 1.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if L < 2.
C
C Modules required by GETNP:  None
C
C Intrinsic function called by GETNP:  ABS
C
C***********************************************************
C
      INTEGER I, LM1, LP, LPL, N1, NB, NI, NP
      REAL    DNB, DNP, X1, Y1, Z1
C
C Local parameters:
C
C DNB,DNP =  Negative cosines of the angular distances from
C              N1 to NB and to NP, respectively
C I =        NPTS index and DO-loop index
C LM1 =      L-1
C LP =       LIST pointer of a neighbor of NI
C LPL =      Pointer to the last neighbor of NI
C N1 =       NPTS(1)
C NB =       Neighbor of NI and candidate for NP
C NI =       NPTS(I)
C NP =       Candidate for NPTS(L)
C X1,Y1,Z1 = Coordinates of N1
C
      LM1 = L - 1
      IF (LM1 .LT. 1) GO TO 6
      IER = 0
C
C Store N1 = NPTS(1) and mark the elements of NPTS.
C
      N1 = NPTS(1)
      X1 = X(N1)
      Y1 = Y(N1)
      Z1 = Z(N1)
      DO 1 I = 1,LM1
        NI = NPTS(I)
        LEND(NI) = -LEND(NI)
    1   CONTINUE
C
C Candidates for NP = NPTS(L) are the unmarked neighbors
C   of nodes in NPTS.  DNP is initially greater than -cos(PI)
C   (the maximum distance).
C
      DNP = 2.
C
C Loop on nodes NI in NPTS.
C
      DO 4 I = 1,LM1
        NI = NPTS(I)
        LPL = -LEND(NI)
        LP = LPL
C
C Loop on neighbors NB of NI.
C
    2   NB = ABS(LIST(LP))
          IF (LEND(NB) .LT. 0) GO TO 3
C
C NB is an unmarked neighbor of NI.  Replace NP if NB is
C   closer to N1.
C
          DNB = -(X(NB)*X1 + Y(NB)*Y1 + Z(NB)*Z1)
          IF (DNB .GE. DNP) GO TO 3
          NP = NB
          DNP = DNB
    3     LP = LPTR(LP)
          IF (LP .NE. LPL) GO TO 2
    4   CONTINUE
      NPTS(L) = NP
      DF = DNP
C
C Unmark the elements of NPTS.
C
      DO 5 I = 1,LM1
        NI = NPTS(I)
        LEND(NI) = -LEND(NI)
    5   CONTINUE
      RETURN
C
C L is outside its valid range.
C
    6 IER = 1
      RETURN
      END
      SUBROUTINE INSERT (K,LP, LIST,LPTR,LNEW )
      INTEGER K, LP, LIST(*), LPTR(*), LNEW
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/17/96
C
C   This subroutine inserts K as a neighbor of N1 following
C N2, where LP is the LIST pointer of N2 as a neighbor of
C N1.  Note that, if N2 is the last neighbor of N1, K will
C become the first neighbor (even if N1 is a boundary node).
C
C   This routine is identical to the similarly named routine
C in TRIPACK.
C
C
C On input:
C
C       K = Index of the node to be inserted.
C
C       LP = LIST pointer of N2 as a neighbor of N1.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR,LNEW = Data structure defining the trian-
C                        gulation.  Refer to Subroutine
C                        TRMESH.
C
C On output:
C
C       LIST,LPTR,LNEW = Data structure updated with the
C                        addition of node K.
C
C Modules required by INSERT:  None
C
C***********************************************************
C
      INTEGER LSAV
C
      LSAV = LPTR(LP)
      LPTR(LP) = LNEW
      LIST(LNEW) = K
      LPTR(LNEW) = LSAV
      LNEW = LNEW + 1
      RETURN
      END
      LOGICAL FUNCTION INSIDE (P,LV,XV,YV,ZV,NV,LISTV, IER)
      INTEGER LV, NV, LISTV(NV), IER
      REAL    P(3), XV(LV), YV(LV), ZV(LV)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   12/27/93
C
C   This function locates a point P relative to a polygonal
C region R on the surface of the unit sphere, returning
C INSIDE = TRUE if and only if P is contained in R.  R is
C defined by a cyclically ordered sequence of vertices which
C form a positively-oriented simple closed curve.  Adjacent
C vertices need not be distinct but the curve must not be
C self-intersecting.  Also, while polygon edges are by defi-
C nition restricted to a single hemisphere, R is not so
C restricted.  Its interior is the region to the left as the
C vertices are traversed in order.
C
C   The algorithm consists of selecting a point Q in R and
C then finding all points at which the great circle defined
C by P and Q intersects the boundary of R.  P lies inside R
C if and only if there is an even number of intersection
C points between Q and P.  Q is taken to be a point immedi-
C ately to the left of a directed boundary edge -- the first
C one that results in no consistency-check failures.
C
C   If P is close to the polygon boundary, the problem is
C ill-conditioned and the decision may be incorrect.  Also,
C an incorrect decision may result from a poor choice of Q
C (if, for example, a boundary edge lies on the great cir-
C cle defined by P and Q).  A more reliable result could be
C obtained by a sequence of calls to INSIDE with the ver-
C tices cyclically permuted before each call (to alter the
C choice of Q).
C
C
C On input:
C
C       P = Array of length 3 containing the Cartesian
C           coordinates of the point (unit vector) to be
C           located.
C
C       LV = Length of arrays XV, YV, and ZV.
C
C       XV,YV,ZV = Arrays of length LV containing the Carte-
C                  sian coordinates of unit vectors (points
C                  on the unit sphere).  These values are
C                  not tested for validity.
C
C       NV = Number of vertices in the polygon.  3 .LE. NV
C            .LE. LV.
C
C       LISTV = Array of length NV containing the indexes
C               (for XV, YV, and ZV) of a cyclically-ordered
C               (and CCW-ordered) sequence of vertices that
C               define R.  The last vertex (indexed by
C               LISTV(NV)) is followed by the first (indexed
C               by LISTV(1)).  LISTV entries must be in the
C               range 1 to LV.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       INSIDE = TRUE if and only if P lies inside R unless
C                IER .NE. 0, in which case the value is not
C                altered.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if LV or NV is outside its valid
C                     range.
C             IER = 2 if a LISTV entry is outside its valid
C                     range.
C             IER = 3 if the polygon boundary was found to
C                     be self-intersecting.  This error will
C                     not necessarily be detected.
C             IER = 4 if every choice of Q (one for each
C                     boundary edge) led to failure of some
C                     internal consistency check.  The most
C                     likely cause of this error is invalid
C                     input:  P = (0,0,0), a null or self-
C                     intersecting polygon, etc.
C
C Module required by INSIDE:  INTRSC
C
C Intrinsic function called by INSIDE:  SQRT
C
C***********************************************************
C
      INTEGER I1, I2, IERR, IMX, K, K0, N, NI
      LOGICAL EVEN, LFT1, LFT2, PINR, QINR
      REAL    B(3), BP, BQ, CN(3), D, EPS, PN(3), Q(3),
     .        QN(3), QNRM, V1(3), V2(3), VN(3), VNRM
C
C Local parameters:
C
C B =         Intersection point between the boundary and
C               the great circle defined by P and Q
C BP,BQ =     <B,P> and <B,Q>, respectively, maximized over
C               intersection points B that lie between P and
C               Q (on the shorter arc) -- used to find the
C               closest intersection points to P and Q
C CN =        Q X P = normal to the plane of P and Q
C D =         Dot product <B,P> or <B,Q>
C EPS =       Parameter used to define Q as the point whose
C               orthogonal distance to (the midpoint of)
C               boundary edge V1->V2 is approximately EPS/
C               (2*Cos(A/2)), where <V1,V2> = Cos(A).
C EVEN =      TRUE iff an even number of intersection points
C               lie between P and Q (on the shorter arc)
C I1,I2 =     Indexes (LISTV elements) of a pair of adjacent
C               boundary vertices (endpoints of a boundary
C               edge)
C IERR =      Error flag for calls to INTRSC (not tested)
C IMX =       Local copy of LV and maximum value of I1 and
C               I2
C K =         DO-loop index and LISTV index
C K0 =        LISTV index of the first endpoint of the
C               boundary edge used to compute Q
C LFT1,LFT2 = Logical variables associated with I1 and I2 in
C               the boundary traversal:  TRUE iff the vertex
C               is strictly to the left of Q->P (<V,CN> > 0)
C N =         Local copy of NV
C NI =        Number of intersections (between the boundary
C               curve and the great circle P-Q) encountered
C PINR =      TRUE iff P is to the left of the directed
C               boundary edge associated with the closest
C               intersection point to P that lies between P
C               and Q (a left-to-right intersection as
C               viewed from Q), or there is no intersection
C               between P and Q (on the shorter arc)
C PN,QN =     P X CN and CN X Q, respectively:  used to
C               locate intersections B relative to arc Q->P
C Q =         (V1 + V2 + EPS*VN/VNRM)/QNRM, where V1->V2 is
C               the boundary edge indexed by LISTV(K0) ->
C               LISTV(K0+1)
C QINR =      TRUE iff Q is to the left of the directed
C               boundary edge associated with the closest
C               intersection point to Q that lies between P
C               and Q (a right-to-left intersection as
C               viewed from Q), or there is no intersection
C               between P and Q (on the shorter arc)
C QNRM =      Euclidean norm of V1+V2+EPS*VN/VNRM used to
C               compute (normalize) Q
C V1,V2 =     Vertices indexed by I1 and I2 in the boundary
C               traversal
C VN =        V1 X V2, where V1->V2 is the boundary edge
C               indexed by LISTV(K0) -> LISTV(K0+1)
C VNRM =      Euclidean norm of VN
C
      DATA EPS/1.E-3/
C
C Store local parameters, test for error 1, and initialize
C   K0.
C
      IMX = LV
      N = NV
      IF (N .LT. 3  .OR.  N .GT. IMX) GO TO 11
      K0 = 0
      I1 = LISTV(1)
      IF (I1 .LT. 1  .OR.  I1 .GT. IMX) GO TO 12
C
C Increment K0 and set Q to a point immediately to the left
C   of the midpoint of edge V1->V2 = LISTV(K0)->LISTV(K0+1):
C   Q = (V1 + V2 + EPS*VN/VNRM)/QNRM, where VN = V1 X V2.
C
    1 K0 = K0 + 1
      IF (K0 .GT. N) GO TO 14
      I1 = LISTV(K0)
      IF (K0 .LT. N) THEN
        I2 = LISTV(K0+1)
      ELSE
        I2 = LISTV(1)
      ENDIF
      IF (I2 .LT. 1  .OR.  I2 .GT. IMX) GO TO 12
      VN(1) = YV(I1)*ZV(I2) - ZV(I1)*YV(I2)
      VN(2) = ZV(I1)*XV(I2) - XV(I1)*ZV(I2)
      VN(3) = XV(I1)*YV(I2) - YV(I1)*XV(I2)
      VNRM = SQRT(VN(1)*VN(1) + VN(2)*VN(2) + VN(3)*VN(3))
      IF (VNRM .EQ. 0.) GO TO 1
      Q(1) = XV(I1) + XV(I2) + EPS*VN(1)/VNRM
      Q(2) = YV(I1) + YV(I2) + EPS*VN(2)/VNRM
      Q(3) = ZV(I1) + ZV(I2) + EPS*VN(3)/VNRM
      QNRM = SQRT(Q(1)*Q(1) + Q(2)*Q(2) + Q(3)*Q(3))
      Q(1) = Q(1)/QNRM
      Q(2) = Q(2)/QNRM
      Q(3) = Q(3)/QNRM
C
C Compute CN = Q X P, PN = P X CN, and QN = CN X Q.
C
      CN(1) = Q(2)*P(3) - Q(3)*P(2)
      CN(2) = Q(3)*P(1) - Q(1)*P(3)
      CN(3) = Q(1)*P(2) - Q(2)*P(1)
      IF (CN(1) .EQ. 0.  .AND.  CN(2) .EQ. 0.  .AND.
     .    CN(3) .EQ. 0.) GO TO 1
      PN(1) = P(2)*CN(3) - P(3)*CN(2)
      PN(2) = P(3)*CN(1) - P(1)*CN(3)
      PN(3) = P(1)*CN(2) - P(2)*CN(1)
      QN(1) = CN(2)*Q(3) - CN(3)*Q(2)
      QN(2) = CN(3)*Q(1) - CN(1)*Q(3)
      QN(3) = CN(1)*Q(2) - CN(2)*Q(1)
C
C Initialize parameters for the boundary traversal.
C
      NI = 0
      EVEN = .TRUE.
      BP = -2.
      BQ = -2.
      PINR = .TRUE.
      QINR = .TRUE.
      I2 = LISTV(N)
      IF (I2 .LT. 1  .OR.  I2 .GT. IMX) GO TO 12
      LFT2 = CN(1)*XV(I2) + CN(2)*YV(I2) +
     .       CN(3)*ZV(I2) .GT. 0.
C
C Loop on boundary arcs I1->I2.
C
      DO 2 K = 1,N
        I1 = I2
        LFT1 = LFT2
        I2 = LISTV(K)
        IF (I2 .LT. 1  .OR.  I2 .GT. IMX) GO TO 12
        LFT2 = CN(1)*XV(I2) + CN(2)*YV(I2) +
     .         CN(3)*ZV(I2) .GT. 0.
        IF (LFT1 .EQV. LFT2) GO TO 2
C
C   I1 and I2 are on opposite sides of Q->P.  Compute the
C     point of intersection B.
C
        NI = NI + 1
        V1(1) = XV(I1)
        V1(2) = YV(I1)
        V1(3) = ZV(I1)
        V2(1) = XV(I2)
        V2(2) = YV(I2)
        V2(3) = ZV(I2)
        CALL INTRSC (V1,V2,CN, B,IERR)
C
C   B is between Q and P (on the shorter arc) iff
C     B Forward Q->P and B Forward P->Q       iff
C     <B,QN> > 0 and <B,PN> > 0.
C
        IF (B(1)*QN(1) + B(2)*QN(2) + B(3)*QN(3) .GT. 0.
     .      .AND.
     .      B(1)*PN(1) + B(2)*PN(2) + B(3)*PN(3) .GT. 0.)
     .    THEN
C
C   Update EVEN, BQ, QINR, BP, and PINR.
C
          EVEN = .NOT. EVEN
          D = B(1)*Q(1) + B(2)*Q(2) + B(3)*Q(3)
          IF (D .GT. BQ) THEN
            BQ = D
            QINR = LFT2
          ENDIF
          D = B(1)*P(1) + B(2)*P(2) + B(3)*P(3)
          IF (D .GT. BP) THEN
            BP = D
            PINR = LFT1
          ENDIF
        ENDIF
    2   CONTINUE
C
C Test for consistency:  NI must be even and QINR must be
C   TRUE.
C
      IF (NI .NE. 2*(NI/2)  .OR.  .NOT. QINR) GO TO 1
C
C Test for error 3:  different values of PINR and EVEN.
C
      IF (PINR .NEQV. EVEN) GO TO 13
C
C No error encountered.
C
      IER = 0
      INSIDE = EVEN
      RETURN
C
C LV or NV is outside its valid range.
C
   11 IER = 1
      RETURN
C
C A LISTV entry is outside its valid range.
C
   12 IER = 2
      RETURN
C
C The polygon boundary is self-intersecting.
C
   13 IER = 3
      RETURN
C
C Consistency tests failed for all values of Q.
C
   14 IER = 4
      RETURN
      END
      SUBROUTINE INTADD (KK,I1,I2,I3, LIST,LPTR,LEND,LNEW )
      INTEGER KK, I1, I2, I3, LIST(*), LPTR(*), LEND(*),
     .        LNEW
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/17/96
C
C   This subroutine adds an interior node to a triangulation
C of a set of points on the unit sphere.  The data structure
C is updated with the insertion of node KK into the triangle
C whose vertices are I1, I2, and I3.  No optimization of the
C triangulation is performed.
C
C   This routine is identical to the similarly named routine
C in TRIPACK.
C
C
C On input:
C
C       KK = Index of the node to be inserted.  KK .GE. 1
C            and KK must not be equal to I1, I2, or I3.
C
C       I1,I2,I3 = Indexes of the counterclockwise-ordered
C                  sequence of vertices of a triangle which
C                  contains node KK.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR,LEND,LNEW = Data structure defining the
C                             triangulation.  Refer to Sub-
C                             routine TRMESH.  Triangle
C                             (I1,I2,I3) must be included
C                             in the triangulation.
C
C On output:
C
C       LIST,LPTR,LEND,LNEW = Data structure updated with
C                             the addition of node KK.  KK
C                             will be connected to nodes I1,
C                             I2, and I3.
C
C Modules required by INTADD:  INSERT, LSTPTR
C
C***********************************************************
C
      INTEGER LSTPTR
      INTEGER K, LP, N1, N2, N3
C
C Local parameters:
C
C K =        Local copy of KK
C LP =       LIST pointer
C N1,N2,N3 = Local copies of I1, I2, and I3
C
      K = KK
C
C Initialization.
C
      N1 = I1
      N2 = I2
      N3 = I3
C
C Add K as a neighbor of I1, I2, and I3.
C
      LP = LSTPTR(LEND(N1),N2,LIST,LPTR)
      CALL INSERT (K,LP, LIST,LPTR,LNEW )
      LP = LSTPTR(LEND(N2),N3,LIST,LPTR)
      CALL INSERT (K,LP, LIST,LPTR,LNEW )
      LP = LSTPTR(LEND(N3),N1,LIST,LPTR)
      CALL INSERT (K,LP, LIST,LPTR,LNEW )
C
C Add I1, I2, and I3 as neighbors of K.
C
      LIST(LNEW) = N1
      LIST(LNEW+1) = N2
      LIST(LNEW+2) = N3
      LPTR(LNEW) = LNEW + 1
      LPTR(LNEW+1) = LNEW + 2
      LPTR(LNEW+2) = LNEW
      LEND(K) = LNEW + 2
      LNEW = LNEW + 3
      RETURN
      END
      SUBROUTINE INTRSC (P1,P2,CN, P,IER)
      INTEGER IER
      REAL    P1(3), P2(3), CN(3), P(3)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/19/90
C
C   Given a great circle C and points P1 and P2 defining an
C arc A on the surface of the unit sphere, where A is the
C shorter of the two portions of the great circle C12 assoc-
C iated with P1 and P2, this subroutine returns the point
C of intersection P between C and C12 that is closer to A.
C Thus, if P1 and P2 lie in opposite hemispheres defined by
C C, P is the point of intersection of C with A.
C
C
C On input:
C
C       P1,P2 = Arrays of length 3 containing the Cartesian
C               coordinates of unit vectors.
C
C       CN = Array of length 3 containing the Cartesian
C            coordinates of a nonzero vector which defines C
C            as the intersection of the plane whose normal
C            is CN with the unit sphere.  Thus, if C is to
C            be the great circle defined by P and Q, CN
C            should be P X Q.
C
C The above parameters are not altered by this routine.
C
C       P = Array of length 3.
C
C On output:
C
C       P = Point of intersection defined above unless IER
C           .NE. 0, in which case P is not altered.
C
C       IER = Error indicator.
C             IER = 0 if no errors were encountered.
C             IER = 1 if <CN,P1> = <CN,P2>.  This occurs
C                     iff P1 = P2 or CN = 0 or there are
C                     two intersection points at the same
C                     distance from A.
C             IER = 2 if P2 = -P1 and the definition of A is
C                     therefore ambiguous.
C
C Modules required by INTRSC:  None
C
C Intrinsic function called by INTRSC:  SQRT
C
C***********************************************************
C
      INTEGER I
      REAL    D1, D2, PP(3), PPN, T
C
C Local parameters:
C
C D1 =  <CN,P1>
C D2 =  <CN,P2>
C I =   DO-loop index
C PP =  P1 + T*(P2-P1) = Parametric representation of the
C         line defined by P1 and P2
C PPN = Norm of PP
C T =   D1/(D1-D2) = Parameter value chosen so that PP lies
C         in the plane of C
C
      D1 = CN(1)*P1(1) + CN(2)*P1(2) + CN(3)*P1(3)
      D2 = CN(1)*P2(1) + CN(2)*P2(2) + CN(3)*P2(3)
C
      IF (D1 .EQ. D2) THEN
        IER = 1
        RETURN
      ENDIF
C
C Solve for T such that <PP,CN> = 0 and compute PP and PPN.
C
      T = D1/(D1-D2)
      PPN = 0.
      DO 1 I = 1,3
        PP(I) = P1(I) + T*(P2(I)-P1(I))
        PPN = PPN + PP(I)*PP(I)
    1   CONTINUE
C
C PPN = 0 iff PP = 0 iff P2 = -P1 (and T = .5).
C
      IF (PPN .EQ. 0.) THEN
        IER = 2
        RETURN
      ENDIF
      PPN = SQRT(PPN)
C
C Compute P = PP/PPN.
C
      DO 2 I = 1,3
        P(I) = PP(I)/PPN
    2   CONTINUE
      IER = 0
      RETURN
      END
      INTEGER FUNCTION JRAND (N, IX,IY,IZ )
      INTEGER N, IX, IY, IZ
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/28/98
C
C   This function returns a uniformly distributed pseudo-
C random integer in the range 1 to N.
C
C
C On input:
C
C       N = Maximum value to be returned.
C
C N is not altered by this function.
C
C       IX,IY,IZ = Integer seeds initialized to values in
C                  the range 1 to 30,000 before the first
C                  call to JRAND, and not altered between
C                  subsequent calls (unless a sequence of
C                  random numbers is to be repeated by
C                  reinitializing the seeds).
C
C On output:
C
C       IX,IY,IZ = Updated integer seeds.
C
C       JRAND = Random integer in the range 1 to N.
C
C Reference:  B. A. Wichmann and I. D. Hill, "An Efficient
C             and Portable Pseudo-random Number Generator",
C             Applied Statistics, Vol. 31, No. 2, 1982,
C             pp. 188-190.
C
C Modules required by JRAND:  None
C
C Intrinsic functions called by JRAND:  INT, MOD, REAL
C
C***********************************************************
C
      REAL U, X
C
C Local parameters:
C
C U = Pseudo-random number uniformly distributed in the
C     interval (0,1).
C X = Pseudo-random number in the range 0 to 3 whose frac-
C       tional part is U.
C
      IX = MOD(171*IX,30269)
      IY = MOD(172*IY,30307)
      IZ = MOD(170*IZ,30323)
      X = (REAL(IX)/30269.) + (REAL(IY)/30307.) +
     .    (REAL(IZ)/30323.)
      U = X - INT(X)
      JRAND = REAL(N)*U + 1.
      RETURN
      END
      LOGICAL FUNCTION LEFT (X1,Y1,Z1,X2,Y2,Z2,X0,Y0,Z0)
      REAL X1, Y1, Z1, X2, Y2, Z2, X0, Y0, Z0
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/15/96
C
C   This function determines whether node N0 is in the
C (closed) left hemisphere defined by the plane containing
C N1, N2, and the origin, where left is defined relative to
C an observer at N1 facing N2.
C
C
C On input:
C
C       X1,Y1,Z1 = Coordinates of N1.
C
C       X2,Y2,Z2 = Coordinates of N2.
C
C       X0,Y0,Z0 = Coordinates of N0.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       LEFT = TRUE if and only if N0 is in the closed
C              left hemisphere.
C
C Modules required by LEFT:  None
C
C***********************************************************
C
C LEFT = TRUE iff <N0,N1 X N2> = det(N0,N1,N2) .GE. 0.
C
      LEFT = X0*(Y1*Z2-Y2*Z1) - Y0*(X1*Z2-X2*Z1) +
     .       Z0*(X1*Y2-X2*Y1) .GE. 0.
      RETURN
      END
      INTEGER FUNCTION LSTPTR (LPL,NB,LIST,LPTR)
      INTEGER LPL, NB, LIST(*), LPTR(*)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/15/96
C
C   This function returns the index (LIST pointer) of NB in
C the adjacency list for N0, where LPL = LEND(N0).
C
C   This function is identical to the similarly named
C function in TRIPACK.
C
C
C On input:
C
C       LPL = LEND(N0)
C
C       NB = Index of the node whose pointer is to be re-
C            turned.  NB must be connected to N0.
C
C       LIST,LPTR = Data structure defining the triangula-
C                   tion.  Refer to Subroutine TRMESH.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       LSTPTR = Pointer such that LIST(LSTPTR) = NB or
C                LIST(LSTPTR) = -NB, unless NB is not a
C                neighbor of N0, in which case LSTPTR = LPL.
C
C Modules required by LSTPTR:  None
C
C***********************************************************
C
      INTEGER LP, ND
C
C Local parameters:
C
C LP = LIST pointer
C ND = Nodal index
C
      LP = LPTR(LPL)
    1 ND = LIST(LP)
        IF (ND .EQ. NB) GO TO 2
        LP = LPTR(LP)
        IF (LP .NE. LPL) GO TO 1
C
    2 LSTPTR = LP
      RETURN
      END
      INTEGER FUNCTION NBCNT (LPL,LPTR)
      INTEGER LPL, LPTR(*)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/15/96
C
C   This function returns the number of neighbors of a node
C N0 in a triangulation created by Subroutine TRMESH.
C
C   This function is identical to the similarly named
C function in TRIPACK.
C
C
C On input:
C
C       LPL = LIST pointer to the last neighbor of N0 --
C             LPL = LEND(N0).
C
C       LPTR = Array of pointers associated with LIST.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       NBCNT = Number of neighbors of N0.
C
C Modules required by NBCNT:  None
C
C***********************************************************
C
      INTEGER K, LP
C
C Local parameters:
C
C K =  Counter for computing the number of neighbors
C LP = LIST pointer
C
      LP = LPL
      K = 1
C
    1 LP = LPTR(LP)
        IF (LP .EQ. LPL) GO TO 2
        K = K + 1
        GO TO 1
C
    2 NBCNT = K
      RETURN
      END
      INTEGER FUNCTION NEARND (P,IST,N,X,Y,Z,LIST,LPTR,
     .                         LEND, AL)
      INTEGER IST, N, LIST(*), LPTR(*), LEND(N)
      REAL    P(3), X(N), Y(N), Z(N), AL
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/28/98
C
C   Given a point P on the surface of the unit sphere and a
C Delaunay triangulation created by Subroutine TRMESH, this
C function returns the index of the nearest triangulation
C node to P.
C
C   The algorithm consists of implicitly adding P to the
C triangulation, finding the nearest neighbor to P, and
C implicitly deleting P from the triangulation.  Thus, it
C is based on the fact that, if P is a node in a Delaunay
C triangulation, the nearest node to P is a neighbor of P.
C
C
C On input:
C
C       P = Array of length 3 containing the Cartesian coor-
C           dinates of the point P to be located relative to
C           the triangulation.  It is assumed without a test
C           that P(1)**2 + P(2)**2 + P(3)**2 = 1.
C
C       IST = Index of a node at which TRFIND begins the
C             search.  Search time depends on the proximity
C             of this node to P.
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       X,Y,Z = Arrays of length N containing the Cartesian
C               coordinates of the nodes.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to TRMESH.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       NEARND = Nodal index of the nearest node to P, or 0
C                if N < 3 or the triangulation data struc-
C                ture is invalid.
C
C       AL = Arc length (angular distance in radians) be-
C            tween P and NEARND unless NEARND = 0.
C
C       Note that the number of candidates for NEARND
C       (neighbors of P) is limited to LMAX defined in
C       the PARAMETER statement below.
C
C Modules required by NEARND:  JRAND, LSTPTR, TRFIND, STORE
C
C Intrinsic functions called by NEARND:  ABS, ACOS
C
C***********************************************************
C
      INTEGER   LSTPTR
      INTEGER   LMAX
      PARAMETER (LMAX=25)
      INTEGER   I1, I2, I3, L, LISTP(LMAX), LP, LP1, LP2,
     .          LPL, LPTRP(LMAX), N1, N2, N3, NN, NR, NST
      REAL      B1, B2, B3, DS1, DSR, DX1, DX2, DX3, DY1,
     .          DY2, DY3, DZ1, DZ2, DZ3
C
C Local parameters:
C
C B1,B2,B3 =  Unnormalized barycentric coordinates returned
C               by TRFIND
C DS1 =       (Negative cosine of the) distance from P to N1
C DSR =       (Negative cosine of the) distance from P to NR
C DX1,..DZ3 = Components of vectors used by the swap test
C I1,I2,I3 =  Nodal indexes of a triangle containing P, or
C               the rightmost (I1) and leftmost (I2) visible
C               boundary nodes as viewed from P
C L =         Length of LISTP/LPTRP and number of neighbors
C               of P
C LMAX =      Maximum value of L
C LISTP =     Indexes of the neighbors of P
C LPTRP =     Array of pointers in 1-1 correspondence with
C               LISTP elements
C LP =        LIST pointer to a neighbor of N1 and LISTP
C               pointer
C LP1,LP2 =   LISTP indexes (pointers)
C LPL =       Pointer to the last neighbor of N1
C N1 =        Index of a node visible from P
C N2 =        Index of an endpoint of an arc opposite P
C N3 =        Index of the node opposite N1->N2
C NN =        Local copy of N
C NR =        Index of a candidate for the nearest node to P
C NST =       Index of the node at which TRFIND begins the
C               search
C
C
C Store local parameters and test for N invalid.
C
      NN = N
      IF (NN .LT. 3) GO TO 6
      NST = IST
      IF (NST .LT. 1  .OR.  NST .GT. NN) NST = 1
C
C Find a triangle (I1,I2,I3) containing P, or the rightmost
C   (I1) and leftmost (I2) visible boundary nodes as viewed
C   from P.
C
      CALL TRFIND (NST,P,N,X,Y,Z,LIST,LPTR,LEND, B1,B2,B3,
     .             I1,I2,I3)
C
C Test for collinear nodes.
C
      IF (I1 .EQ. 0) GO TO 6
C
C Store the linked list of 'neighbors' of P in LISTP and
C   LPTRP.  I1 is the first neighbor, and 0 is stored as
C   the last neighbor if P is not contained in a triangle.
C   L is the length of LISTP and LPTRP, and is limited to
C   LMAX.
C
      IF (I3 .NE. 0) THEN
        LISTP(1) = I1
        LPTRP(1) = 2
        LISTP(2) = I2
        LPTRP(2) = 3
        LISTP(3) = I3
        LPTRP(3) = 1
        L = 3
      ELSE
        N1 = I1
        L = 1
        LP1 = 2
        LISTP(L) = N1
        LPTRP(L) = LP1
C
C   Loop on the ordered sequence of visible boundary nodes
C     N1 from I1 to I2.
C
    1   LPL = LEND(N1)
          N1 = -LIST(LPL)
          L = LP1
          LP1 = L+1
          LISTP(L) = N1
          LPTRP(L) = LP1
          IF (N1 .NE. I2  .AND.  LP1 .LT. LMAX) GO TO 1
        L = LP1
        LISTP(L) = 0
        LPTRP(L) = 1
      ENDIF
C
C Initialize variables for a loop on arcs N1-N2 opposite P
C   in which new 'neighbors' are 'swapped' in.  N1 follows
C   N2 as a neighbor of P, and LP1 and LP2 are the LISTP
C   indexes of N1 and N2.
C
      LP2 = 1
      N2 = I1
      LP1 = LPTRP(1)
      N1 = LISTP(LP1)
C
C Begin loop:  find the node N3 opposite N1->N2.
C
    2 LP = LSTPTR(LEND(N1),N2,LIST,LPTR)
        IF (LIST(LP) .LT. 0) GO TO 3
        LP = LPTR(LP)
        N3 = ABS(LIST(LP))
C
C Swap test:  Exit the loop if L = LMAX.
C
        IF (L .EQ. LMAX) GO TO 4
        DX1 = X(N1) - P(1)
        DY1 = Y(N1) - P(2)
        DZ1 = Z(N1) - P(3)
C
        DX2 = X(N2) - P(1)
        DY2 = Y(N2) - P(2)
        DZ2 = Z(N2) - P(3)
C
        DX3 = X(N3) - P(1)
        DY3 = Y(N3) - P(2)
        DZ3 = Z(N3) - P(3)
        IF ( DX3*(DY2*DZ1 - DY1*DZ2) -
     .       DY3*(DX2*DZ1 - DX1*DZ2) +
     .       DZ3*(DX2*DY1 - DX1*DY2) .LE. 0. ) GO TO 3
C
C Swap:  Insert N3 following N2 in the adjacency list for P.
C        The two new arcs opposite P must be tested.
C
        L = L+1
        LPTRP(LP2) = L
        LISTP(L) = N3
        LPTRP(L) = LP1
        LP1 = L
        N1 = N3
        GO TO 2
C
C No swap:  Advance to the next arc and test for termination
C           on N1 = I1 (LP1 = 1) or N1 followed by 0.
C
    3   IF (LP1 .EQ. 1) GO TO 4
        LP2 = LP1
        N2 = N1
        LP1 = LPTRP(LP1)
        N1 = LISTP(LP1)
        IF (N1 .EQ. 0) GO TO 4
        GO TO 2
C
C Set NR and DSR to the index of the nearest node to P and
C   an increasing function (negative cosine) of its distance
C   from P, respectively.
C
    4 NR = I1
      DSR = -(X(NR)*P(1) + Y(NR)*P(2) + Z(NR)*P(3))
      DO 5 LP = 2,L
        N1 = LISTP(LP)
        IF (N1 .EQ. 0) GO TO 5
        DS1 = -(X(N1)*P(1) + Y(N1)*P(2) + Z(N1)*P(3))
        IF (DS1 .LT. DSR) THEN
          NR = N1
          DSR = DS1
        ENDIF
    5   CONTINUE
      DSR = -DSR
      IF (DSR .GT. 1.0) DSR = 1.0
      AL = ACOS(DSR)
      NEARND = NR
      RETURN
C
C Invalid input.
C
    6 NEARND = 0
      RETURN
      END
      SUBROUTINE OPTIM (X,Y,Z,NA, LIST,LPTR,LEND,NIT,
     .                  IWK, IER)
      INTEGER NA, LIST(*), LPTR(*), LEND(*), NIT, IWK(2,NA),
     .        IER
      REAL    X(*), Y(*), Z(*)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/30/98
C
C   Given a set of NA triangulation arcs, this subroutine
C optimizes the portion of the triangulation consisting of
C the quadrilaterals (pairs of adjacent triangles) which
C have the arcs as diagonals by applying the circumcircle
C test and appropriate swaps to the arcs.
C
C   An iteration consists of applying the swap test and
C swaps to all NA arcs in the order in which they are
C stored.  The iteration is repeated until no swap occurs
C or NIT iterations have been performed.  The bound on the
C number of iterations may be necessary to prevent an
C infinite loop caused by cycling (reversing the effect of a
C previous swap) due to floating point inaccuracy when four
C or more nodes are nearly cocircular.
C
C
C On input:
C
C       X,Y,Z = Arrays containing the nodal coordinates.
C
C       NA = Number of arcs in the set.  NA .GE. 0.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to Subroutine
C                        TRMESH.
C
C       NIT = Maximum number of iterations to be performed.
C             NIT = 4*NA should be sufficient.  NIT .GE. 1.
C
C       IWK = Integer array dimensioned 2 by NA containing
C             the nodal indexes of the arc endpoints (pairs
C             of endpoints are stored in columns).
C
C On output:
C
C       LIST,LPTR,LEND = Updated triangulation data struc-
C                        ture reflecting the swaps.
C
C       NIT = Number of iterations performed.
C
C       IWK = Endpoint indexes of the new set of arcs
C             reflecting the swaps.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if a swap occurred on the last of
C                     MAXIT iterations, where MAXIT is the
C                     value of NIT on input.  The new set
C                     of arcs is not necessarily optimal
C                     in this case.
C             IER = 2 if NA < 0 or NIT < 1 on input.
C             IER = 3 if IWK(2,I) is not a neighbor of
C                     IWK(1,I) for some I in the range 1
C                     to NA.  A swap may have occurred in
C                     this case.
C             IER = 4 if a zero pointer was returned by
C                     Subroutine SWAP.
C
C Modules required by OPTIM:  LSTPTR, SWAP, SWPTST
C
C Intrinsic function called by OPTIM:  ABS
C
C***********************************************************
C
      INTEGER I, IO1, IO2, ITER, LP, LP21, LPL, LPP, MAXIT,
     .        N1, N2, NNA
      LOGICAL SWPTST
      LOGICAL SWP
C
C Local parameters:
C
C I =       Column index for IWK
C IO1,IO2 = Nodal indexes of the endpoints of an arc in IWK
C ITER =    Iteration count
C LP =      LIST pointer
C LP21 =    Parameter returned by SWAP (not used)
C LPL =     Pointer to the last neighbor of IO1
C LPP =     Pointer to the node preceding IO2 as a neighbor
C             of IO1
C MAXIT =   Input value of NIT
C N1,N2 =   Nodes opposite IO1->IO2 and IO2->IO1,
C             respectively
C NNA =     Local copy of NA
C SWP =     Flag set to TRUE iff a swap occurs in the
C             optimization loop
C
      NNA = NA
      MAXIT = NIT
      IF (NNA .LT. 0  .OR.  MAXIT .LT. 1) GO TO 7
C
C Initialize iteration count ITER and test for NA = 0.
C
      ITER = 0
      IF (NNA .EQ. 0) GO TO 5
C
C Top of loop --
C   SWP = TRUE iff a swap occurred in the current iteration.
C
    1 IF (ITER .EQ. MAXIT) GO TO 6
      ITER = ITER + 1
      SWP = .FALSE.
C
C   Inner loop on arcs IO1-IO2 --
C
      DO 4 I = 1,NNA
        IO1 = IWK(1,I)
        IO2 = IWK(2,I)
C
C   Set N1 and N2 to the nodes opposite IO1->IO2 and
C     IO2->IO1, respectively.  Determine the following:
C
C     LPL = pointer to the last neighbor of IO1,
C     LP = pointer to IO2 as a neighbor of IO1, and
C     LPP = pointer to the node N2 preceding IO2.
C
        LPL = LEND(IO1)
        LPP = LPL
        LP = LPTR(LPP)
    2   IF (LIST(LP) .EQ. IO2) GO TO 3
          LPP = LP
          LP = LPTR(LPP)
          IF (LP .NE. LPL) GO TO 2
C
C   IO2 should be the last neighbor of IO1.  Test for no
C     arc and bypass the swap test if IO1 is a boundary
C     node.
C
        IF (ABS(LIST(LP)) .NE. IO2) GO TO 8
        IF (LIST(LP) .LT. 0) GO TO 4
C
C   Store N1 and N2, or bypass the swap test if IO1 is a
C     boundary node and IO2 is its first neighbor.
C
    3   N2 = LIST(LPP)
        IF (N2 .LT. 0) GO TO 4
        LP = LPTR(LP)
        N1 = ABS(LIST(LP))
C
C   Test IO1-IO2 for a swap, and update IWK if necessary.
C
        IF ( .NOT. SWPTST(N1,N2,IO1,IO2,X,Y,Z) ) GO TO 4
        CALL SWAP (N1,N2,IO1,IO2, LIST,LPTR,LEND, LP21)
        IF (LP21 .EQ. 0) GO TO 9
        SWP = .TRUE.
        IWK(1,I) = N1
        IWK(2,I) = N2
    4   CONTINUE
      IF (SWP) GO TO 1
C
C Successful termination.
C
    5 NIT = ITER
      IER = 0
      RETURN
C
C MAXIT iterations performed without convergence.
C
    6 NIT = MAXIT
      IER = 1
      RETURN
C
C Invalid input parameter.
C
    7 NIT = 0
      IER = 2
      RETURN
C
C IO2 is not a neighbor of IO1.
C
    8 NIT = ITER
      IER = 3
      RETURN
C
C Zero pointer returned by SWAP.
C
    9 NIT = ITER
      IER = 4
      RETURN
      END
      SUBROUTINE SCOORD (PX,PY,PZ, PLAT,PLON,PNRM)
      REAL PX, PY, PZ, PLAT, PLON, PNRM
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   08/27/90
C
C   This subroutine converts a point P from Cartesian coor-
C dinates to spherical coordinates.
C
C
C On input:
C
C       PX,PY,PZ = Cartesian coordinates of P.
C
C Input parameters are not altered by this routine.
C
C On output:
C
C       PLAT = Latitude of P in the range -PI/2 to PI/2, or
C              0 if PNRM = 0.  PLAT should be scaled by
C              180/PI to obtain the value in degrees.
C
C       PLON = Longitude of P in the range -PI to PI, or 0
C              if P lies on the Z-axis.  PLON should be
C              scaled by 180/PI to obtain the value in
C              degrees.
C
C       PNRM = Magnitude (Euclidean norm) of P.
C
C Modules required by SCOORD:  None
C
C Intrinsic functions called by SCOORD:  ASIN, ATAN2, SQRT
C
C***********************************************************
C
      PNRM = SQRT(PX*PX + PY*PY + PZ*PZ)
      IF (PX .NE. 0.  .OR.  PY .NE. 0.) THEN
        PLON = ATAN2(PY,PX)
      ELSE
        PLON = 0.
      ENDIF
      IF (PNRM .NE. 0.) THEN
        PLAT = ASIN(PZ/PNRM)
      ELSE
        PLAT = 0.
      ENDIF
      RETURN
      END
      REAL FUNCTION STORE (X)
      REAL X
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   05/09/92
C
C   This function forces its argument X to be stored in a
C memory location, thus providing a means of determining
C floating point number characteristics (such as the machine
C precision) when it is necessary to avoid computation in
C high precision registers.
C
C
C On input:
C
C       X = Value to be stored.
C
C X is not altered by this function.
C
C On output:
C
C       STORE = Value of X after it has been stored and
C               possibly truncated or rounded to the single
C               precision word length.
C
C Modules required by STORE:  None
C
C***********************************************************
C
      REAL Y
      COMMON/STCOM/Y
      Y = X
      STORE = Y
      RETURN
      END
      SUBROUTINE SWAP (IN1,IN2,IO1,IO2, LIST,LPTR,
     .                 LEND, LP21)
      INTEGER IN1, IN2, IO1, IO2, LIST(*), LPTR(*), LEND(*),
     .        LP21
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   06/22/98
C
C   Given a triangulation of a set of points on the unit
C sphere, this subroutine replaces a diagonal arc in a
C strictly convex quadrilateral (defined by a pair of adja-
C cent triangles) with the other diagonal.  Equivalently, a
C pair of adjacent triangles is replaced by another pair
C having the same union.
C
C
C On input:
C
C       IN1,IN2,IO1,IO2 = Nodal indexes of the vertices of
C                         the quadrilateral.  IO1-IO2 is re-
C                         placed by IN1-IN2.  (IO1,IO2,IN1)
C                         and (IO2,IO1,IN2) must be trian-
C                         gles on input.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to Subroutine
C                        TRMESH.
C
C On output:
C
C       LIST,LPTR,LEND = Data structure updated with the
C                        swap -- triangles (IO1,IO2,IN1) and
C                        (IO2,IO1,IN2) are replaced by
C                        (IN1,IN2,IO2) and (IN2,IN1,IO1)
C                        unless LP21 = 0.
C
C       LP21 = Index of IN1 as a neighbor of IN2 after the
C              swap is performed unless IN1 and IN2 are
C              adjacent on input, in which case LP21 = 0.
C
C Module required by SWAP:  LSTPTR
C
C Intrinsic function called by SWAP:  ABS
C
C***********************************************************
C
      INTEGER LSTPTR
      INTEGER LP, LPH, LPSAV
C
C Local parameters:
C
C LP,LPH,LPSAV = LIST pointers
C
C
C Test for IN1 and IN2 adjacent.
C
      LP = LSTPTR(LEND(IN1),IN2,LIST,LPTR)
      IF (ABS(LIST(LP)) .EQ. IN2) THEN
        LP21 = 0
        RETURN
      ENDIF
C
C Delete IO2 as a neighbor of IO1.
C
      LP = LSTPTR(LEND(IO1),IN2,LIST,LPTR)
      LPH = LPTR(LP)
      LPTR(LP) = LPTR(LPH)
C
C If IO2 is the last neighbor of IO1, make IN2 the
C   last neighbor.
C
      IF (LEND(IO1) .EQ. LPH) LEND(IO1) = LP
C
C Insert IN2 as a neighbor of IN1 following IO1
C   using the hole created above.
C
      LP = LSTPTR(LEND(IN1),IO1,LIST,LPTR)
      LPSAV = LPTR(LP)
      LPTR(LP) = LPH
      LIST(LPH) = IN2
      LPTR(LPH) = LPSAV
C
C Delete IO1 as a neighbor of IO2.
C
      LP = LSTPTR(LEND(IO2),IN1,LIST,LPTR)
      LPH = LPTR(LP)
      LPTR(LP) = LPTR(LPH)
C
C If IO1 is the last neighbor of IO2, make IN1 the
C   last neighbor.
C
      IF (LEND(IO2) .EQ. LPH) LEND(IO2) = LP
C
C Insert IN1 as a neighbor of IN2 following IO2.
C
      LP = LSTPTR(LEND(IN2),IO2,LIST,LPTR)
      LPSAV = LPTR(LP)
      LPTR(LP) = LPH
      LIST(LPH) = IN1
      LPTR(LPH) = LPSAV
      LP21 = LPH
      RETURN
      END
      LOGICAL FUNCTION SWPTST (N1,N2,N3,N4,X,Y,Z)
      INTEGER N1, N2, N3, N4
      REAL    X(*), Y(*), Z(*)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   03/29/91
C
C   This function decides whether or not to replace a
C diagonal arc in a quadrilateral with the other diagonal.
C The decision will be to swap (SWPTST = TRUE) if and only
C if N4 lies above the plane (in the half-space not contain-
C ing the origin) defined by (N1,N2,N3), or equivalently, if
C the projection of N4 onto this plane is interior to the
C circumcircle of (N1,N2,N3).  The decision will be for no
C swap if the quadrilateral is not strictly convex.
C
C
C On input:
C
C       N1,N2,N3,N4 = Indexes of the four nodes defining the
C                     quadrilateral with N1 adjacent to N2,
C                     and (N1,N2,N3) in counterclockwise
C                     order.  The arc connecting N1 to N2
C                     should be replaced by an arc connec-
C                     ting N3 to N4 if SWPTST = TRUE.  Refer
C                     to Subroutine SWAP.
C
C       X,Y,Z = Arrays of length N containing the Cartesian
C               coordinates of the nodes.  (X(I),Y(I),Z(I))
C               define node I for I = N1, N2, N3, and N4.
C
C Input parameters are not altered by this routine.
C
C On output:
C
C       SWPTST = TRUE if and only if the arc connecting N1
C                and N2 should be swapped for an arc con-
C                necting N3 and N4.
C
C Modules required by SWPTST:  None
C
C***********************************************************
C
      REAL DX1, DX2, DX3, DY1, DY2, DY3, DZ1, DZ2, DZ3,
     .     X4, Y4, Z4
C
C Local parameters:
C
C DX1,DY1,DZ1 = Coordinates of N4->N1
C DX2,DY2,DZ2 = Coordinates of N4->N2
C DX3,DY3,DZ3 = Coordinates of N4->N3
C X4,Y4,Z4 =    Coordinates of N4
C
      X4 = X(N4)
      Y4 = Y(N4)
      Z4 = Z(N4)
      DX1 = X(N1) - X4
      DX2 = X(N2) - X4
      DX3 = X(N3) - X4
      DY1 = Y(N1) - Y4
      DY2 = Y(N2) - Y4
      DY3 = Y(N3) - Y4
      DZ1 = Z(N1) - Z4
      DZ2 = Z(N2) - Z4
      DZ3 = Z(N3) - Z4
C
C N4 lies above the plane of (N1,N2,N3) iff N3 lies above
C   the plane of (N2,N1,N4) iff Det(N3-N4,N2-N4,N1-N4) =
C   (N3-N4,N2-N4 X N1-N4) > 0.
C
      SWPTST = DX3*(DY2*DZ1 - DY1*DZ2)
     .        -DY3*(DX2*DZ1 - DX1*DZ2)
     .        +DZ3*(DX2*DY1 - DX1*DY2) .GT. 0.
      RETURN
      END
      SUBROUTINE TRANS (N,RLAT,RLON, X,Y,Z)
      INTEGER N
      REAL    RLAT(N), RLON(N), X(N), Y(N), Z(N)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   04/08/90
C
C   This subroutine transforms spherical coordinates into
C Cartesian coordinates on the unit sphere for input to
C Subroutine TRMESH.  Storage for X and Y may coincide with
C storage for RLAT and RLON if the latter need not be saved.
C
C
C On input:
C
C       N = Number of nodes (points on the unit sphere)
C           whose coordinates are to be transformed.
C
C       RLAT = Array of length N containing latitudinal
C              coordinates of the nodes in radians.
C
C       RLON = Array of length N containing longitudinal
C              coordinates of the nodes in radians.
C
C The above parameters are not altered by this routine.
C
C       X,Y,Z = Arrays of length at least N.
C
C On output:
C
C       X,Y,Z = Cartesian coordinates in the range -1 to 1.
C               X(I)**2 + Y(I)**2 + Z(I)**2 = 1 for I = 1
C               to N.
C
C Modules required by TRANS:  None
C
C Intrinsic functions called by TRANS:  COS, SIN
C
C***********************************************************
C
      INTEGER I, NN
      REAL    COSPHI, PHI, THETA
C
C Local parameters:
C
C COSPHI = cos(PHI)
C I =      DO-loop index
C NN =     Local copy of N
C PHI =    Latitude
C THETA =  Longitude
C
      NN = N
      DO 1 I = 1,NN
        PHI = RLAT(I)
        THETA = RLON(I)
        COSPHI = COS(PHI)
        X(I) = COSPHI*COS(THETA)
        Y(I) = COSPHI*SIN(THETA)
        Z(I) = SIN(PHI)
    1   CONTINUE
      RETURN
      END
      SUBROUTINE TRFIND (NST,P,N,X,Y,Z,LIST,LPTR,LEND, B1,
     .                   B2,B3,I1,I2,I3)
      INTEGER NST, N, LIST(*), LPTR(*), LEND(N), I1, I2, I3
      REAL    P(3), X(N), Y(N), Z(N), B1, B2, B3
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   11/30/99
C
C   This subroutine locates a point P relative to a triangu-
C lation created by Subroutine TRMESH.  If P is contained in
C a triangle, the three vertex indexes and barycentric coor-
C dinates are returned.  Otherwise, the indexes of the
C visible boundary nodes are returned.
C
C
C On input:
C
C       NST = Index of a node at which TRFIND begins its
C             search.  Search time depends on the proximity
C             of this node to P.
C
C       P = Array of length 3 containing the x, y, and z
C           coordinates (in that order) of the point P to be
C           located.
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       X,Y,Z = Arrays of length N containing the Cartesian
C               coordinates of the triangulation nodes (unit
C               vectors).  (X(I),Y(I),Z(I)) defines node I
C               for I = 1 to N.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to Subroutine
C                        TRMESH.
C
C Input parameters are not altered by this routine.
C
C On output:
C
C       B1,B2,B3 = Unnormalized barycentric coordinates of
C                  the central projection of P onto the un-
C                  derlying planar triangle if P is in the
C                  convex hull of the nodes.  These parame-
C                  ters are not altered if I1 = 0.
C
C       I1,I2,I3 = Counterclockwise-ordered vertex indexes
C                  of a triangle containing P if P is con-
C                  tained in a triangle.  If P is not in the
C                  convex hull of the nodes, I1 and I2 are
C                  the rightmost and leftmost (boundary)
C                  nodes that are visible from P, and
C                  I3 = 0.  (If all boundary nodes are vis-
C                  ible from P, then I1 and I2 coincide.)
C                  I1 = I2 = I3 = 0 if P and all of the
C                  nodes are coplanar (lie on a common great
C                  circle.
C
C Modules required by TRFIND:  JRAND, LSTPTR, STORE
C
C Intrinsic function called by TRFIND:  ABS
C
C***********************************************************
C
      INTEGER JRAND, LSTPTR
      INTEGER IX, IY, IZ, LP, N0, N1, N1S, N2, N2S, N3, N4,
     .        NEXT, NF, NL
      REAL    STORE
      REAL    DET, EPS, PTN1, PTN2, Q(3), S12, TOL, XP, YP,
     .        ZP
      REAL    X0, X1, X2, Y0, Y1, Y2, Z0, Z1, Z2
C
      SAVE    IX, IY, IZ
      DATA    IX/1/, IY/2/, IZ/3/
C
C Local parameters:
C
C EPS =      Machine precision
C IX,IY,IZ = Integer seeds for JRAND
C LP =       LIST pointer
C N0,N1,N2 = Nodes in counterclockwise order defining a
C              cone (with vertex N0) containing P, or end-
C              points of a boundary edge such that P Right
C              N1->N2
C N1S,N2S =  Initially-determined values of N1 and N2
C N3,N4 =    Nodes opposite N1->N2 and N2->N1, respectively
C NEXT =     Candidate for I1 or I2 when P is exterior
C NF,NL =    First and last neighbors of N0, or first
C              (rightmost) and last (leftmost) nodes
C              visible from P when P is exterior to the
C              triangulation
C PTN1 =     Scalar product <P,N1>
C PTN2 =     Scalar product <P,N2>
C Q =        (N2 X N1) X N2  or  N1 X (N2 X N1) -- used in
C              the boundary traversal when P is exterior
C S12 =      Scalar product <N1,N2>
C TOL =      Tolerance (multiple of EPS) defining an upper
C              bound on the magnitude of a negative bary-
C              centric coordinate (B1 or B2) for P in a
C              triangle -- used to avoid an infinite number
C              of restarts with 0 <= B3 < EPS and B1 < 0 or
C              B2 < 0 but small in magnitude
C XP,YP,ZP = Local variables containing P(1), P(2), and P(3)
C X0,Y0,Z0 = Dummy arguments for DET
C X1,Y1,Z1 = Dummy arguments for DET
C X2,Y2,Z2 = Dummy arguments for DET
C
C Statement function:
C
C DET(X1,...,Z0) .GE. 0 if and only if (X0,Y0,Z0) is in the
C                       (closed) left hemisphere defined by
C                       the plane containing (0,0,0),
C                       (X1,Y1,Z1), and (X2,Y2,Z2), where
C                       left is defined relative to an ob-
C                       server at (X1,Y1,Z1) facing
C                       (X2,Y2,Z2).
C
      DET (X1,Y1,Z1,X2,Y2,Z2,X0,Y0,Z0) = X0*(Y1*Z2-Y2*Z1)
     .     - Y0*(X1*Z2-X2*Z1) + Z0*(X1*Y2-X2*Y1)
C
C Initialize variables.
C
      XP = P(1)
      YP = P(2)
      ZP = P(3)
      N0 = NST
      IF (N0 .LT. 1  .OR.  N0 .GT. N)
     .  N0 = JRAND(N, IX,IY,IZ )
C
C Compute the relative machine precision EPS and TOL.
C
      EPS = 1.E0
    1 EPS = EPS/2.E0
        IF (STORE(EPS+1.E0) .GT. 1.E0) GO TO 1
      EPS = 2.E0*EPS
      TOL = 100.E0*EPS
C
C Set NF and NL to the first and last neighbors of N0, and
C   initialize N1 = NF.
C
    2 LP = LEND(N0)
      NL = LIST(LP)
      LP = LPTR(LP)
      NF = LIST(LP)
      N1 = NF
C
C Find a pair of adjacent neighbors N1,N2 of N0 that define
C   a wedge containing P:  P LEFT N0->N1 and P RIGHT N0->N2.
C
      IF (NL .GT. 0) THEN
C
C   N0 is an interior node.  Find N1.
C
    3   IF ( DET(X(N0),Y(N0),Z(N0),X(N1),Y(N1),Z(N1),
     .           XP,YP,ZP) .LT. 0. ) THEN
          LP = LPTR(LP)
          N1 = LIST(LP)
          IF (N1 .EQ. NL) GO TO 6
          GO TO 3
        ENDIF
      ELSE
C
C   N0 is a boundary node.  Test for P exterior.
C
        NL = -NL
        IF ( DET(X(N0),Y(N0),Z(N0),X(NF),Y(NF),Z(NF),
     .           XP,YP,ZP) .LT. 0. ) THEN
C
C   P is to the right of the boundary edge N0->NF.
C
          N1 = N0
          N2 = NF
          GO TO 9
        ENDIF
        IF ( DET(X(NL),Y(NL),Z(NL),X(N0),Y(N0),Z(N0),
     .           XP,YP,ZP) .LT. 0. ) THEN
C
C   P is to the right of the boundary edge NL->N0.
C
          N1 = NL
          N2 = N0
          GO TO 9
        ENDIF
      ENDIF
C
C P is to the left of arcs N0->N1 and NL->N0.  Set N2 to the
C   next neighbor of N0 (following N1).
C
    4 LP = LPTR(LP)
        N2 = ABS(LIST(LP))
        IF ( DET(X(N0),Y(N0),Z(N0),X(N2),Y(N2),Z(N2),
     .           XP,YP,ZP) .LT. 0. ) GO TO 7
        N1 = N2
        IF (N1 .NE. NL) GO TO 4
      IF ( DET(X(N0),Y(N0),Z(N0),X(NF),Y(NF),Z(NF),
     .         XP,YP,ZP) .LT. 0. ) GO TO 6
C
C P is left of or on arcs N0->NB for all neighbors NB
C   of N0.  Test for P = +/-N0.
C
      IF (STORE(ABS(X(N0)*XP + Y(N0)*YP + Z(N0)*ZP))
     .   .LT. 1.0-4.0*EPS) THEN
C
C   All points are collinear iff P Left NB->N0 for all
C     neighbors NB of N0.  Search the neighbors of N0.
C     Note:  N1 = NL and LP points to NL.
C
    5   IF ( DET(X(N1),Y(N1),Z(N1),X(N0),Y(N0),Z(N0),
     .           XP,YP,ZP) .GE. 0. ) THEN
          LP = LPTR(LP)
          N1 = ABS(LIST(LP))
          IF (N1 .EQ. NL) GO TO 14
          GO TO 5
        ENDIF
      ENDIF
C
C P is to the right of N1->N0, or P = +/-N0.  Set N0 to N1
C   and start over.
C
      N0 = N1
      GO TO 2
C
C P is between arcs N0->N1 and N0->NF.
C
    6 N2 = NF
C
C P is contained in a wedge defined by geodesics N0-N1 and
C   N0-N2, where N1 is adjacent to N2.  Save N1 and N2 to
C   test for cycling.
C
    7 N3 = N0
      N1S = N1
      N2S = N2
C
C Top of edge-hopping loop:
C
    8 B3 = DET(X(N1),Y(N1),Z(N1),X(N2),Y(N2),Z(N2),XP,YP,ZP)
      IF (B3 .LT. 0.) THEN
C
C   Set N4 to the first neighbor of N2 following N1 (the
C     node opposite N2->N1) unless N1->N2 is a boundary arc.
C
        LP = LSTPTR(LEND(N2),N1,LIST,LPTR)
        IF (LIST(LP) .LT. 0) GO TO 9
        LP = LPTR(LP)
        N4 = ABS(LIST(LP))
C
C   Define a new arc N1->N2 which intersects the geodesic
C     N0-P.
C
        IF ( DET(X(N0),Y(N0),Z(N0),X(N4),Y(N4),Z(N4),
     .           XP,YP,ZP) .LT. 0. ) THEN
          N3 = N2
          N2 = N4
          N1S = N1
          IF (N2 .NE. N2S  .AND.  N2 .NE. N0) GO TO 8
        ELSE
          N3 = N1
          N1 = N4
          N2S = N2
          IF (N1 .NE. N1S  .AND.  N1 .NE. N0) GO TO 8
        ENDIF
C
C   The starting node N0 or edge N1-N2 was encountered
C     again, implying a cycle (infinite loop).  Restart
C     with N0 randomly selected.
C
        N0 = JRAND(N, IX,IY,IZ )
        GO TO 2
      ENDIF
C
C P is in (N1,N2,N3) unless N0, N1, N2, and P are collinear
C   or P is close to -N0.
C
      IF (B3 .GE. EPS) THEN
C
C   B3 .NE. 0.
C
        B1 = DET(X(N2),Y(N2),Z(N2),X(N3),Y(N3),Z(N3),
     .           XP,YP,ZP)
        B2 = DET(X(N3),Y(N3),Z(N3),X(N1),Y(N1),Z(N1),
     .           XP,YP,ZP)
        IF (B1 .LT. -TOL  .OR.  B2 .LT. -TOL) THEN
C
C   Restart with N0 randomly selected.
C
          N0 = JRAND(N, IX,IY,IZ )
          GO TO 2
        ENDIF
      ELSE
C
C   B3 = 0 and thus P lies on N1->N2. Compute
C     B1 = Det(P,N2 X N1,N2) and B2 = Det(P,N1,N2 X N1).
C
        B3 = 0.
        S12 = X(N1)*X(N2) + Y(N1)*Y(N2) + Z(N1)*Z(N2)
        PTN1 = XP*X(N1) + YP*Y(N1) + ZP*Z(N1)
        PTN2 = XP*X(N2) + YP*Y(N2) + ZP*Z(N2)
        B1 = PTN1 - S12*PTN2
        B2 = PTN2 - S12*PTN1
        IF (B1 .LT. -TOL  .OR.  B2 .LT. -TOL) THEN
C
C   Restart with N0 randomly selected.
C
          N0 = JRAND(N, IX,IY,IZ )
          GO TO 2
        ENDIF
      ENDIF
C
C P is in (N1,N2,N3).
C
      I1 = N1
      I2 = N2
      I3 = N3
      IF (B1 .LT. 0.0) B1 = 0.0
      IF (B2 .LT. 0.0) B2 = 0.0
      RETURN
C
C P Right N1->N2, where N1->N2 is a boundary edge.
C   Save N1 and N2, and set NL = 0 to indicate that
C   NL has not yet been found.
C
    9 N1S = N1
      N2S = N2
      NL = 0
C
C           Counterclockwise Boundary Traversal:
C
   10 LP = LEND(N2)
      LP = LPTR(LP)
      NEXT = LIST(LP)
      IF ( DET(X(N2),Y(N2),Z(N2),X(NEXT),Y(NEXT),Z(NEXT),
     .         XP,YP,ZP) .GE. 0. ) THEN
C
C   N2 is the rightmost visible node if P Forward N2->N1
C     or NEXT Forward N2->N1.  Set Q to (N2 X N1) X N2.
C
        S12 = X(N1)*X(N2) + Y(N1)*Y(N2) + Z(N1)*Z(N2)
        Q(1) = X(N1) - S12*X(N2)
        Q(2) = Y(N1) - S12*Y(N2)
        Q(3) = Z(N1) - S12*Z(N2)
        IF (XP*Q(1) + YP*Q(2) + ZP*Q(3) .GE. 0.) GO TO 11
        IF (X(NEXT)*Q(1) + Y(NEXT)*Q(2) + Z(NEXT)*Q(3)
     .      .GE. 0.) GO TO 11
C
C   N1, N2, NEXT, and P are nearly collinear, and N2 is
C     the leftmost visible node.
C
        NL = N2
      ENDIF
C
C Bottom of counterclockwise loop:
C
      N1 = N2
      N2 = NEXT
      IF (N2 .NE. N1S) GO TO 10
C
C All boundary nodes are visible from P.
C
      I1 = N1S
      I2 = N1S
      I3 = 0
      RETURN
C
C N2 is the rightmost visible node.
C
   11 NF = N2
      IF (NL .EQ. 0) THEN
C
C Restore initial values of N1 and N2, and begin the search
C   for the leftmost visible node.
C
        N2 = N2S
        N1 = N1S
C
C           Clockwise Boundary Traversal:
C
   12   LP = LEND(N1)
        NEXT = -LIST(LP)
        IF ( DET(X(NEXT),Y(NEXT),Z(NEXT),X(N1),Y(N1),Z(N1),
     .           XP,YP,ZP) .GE. 0. ) THEN
C
C   N1 is the leftmost visible node if P or NEXT is
C     forward of N1->N2.  Compute Q = N1 X (N2 X N1).
C
          S12 = X(N1)*X(N2) + Y(N1)*Y(N2) + Z(N1)*Z(N2)
          Q(1) = X(N2) - S12*X(N1)
          Q(2) = Y(N2) - S12*Y(N1)
          Q(3) = Z(N2) - S12*Z(N1)
          IF (XP*Q(1) + YP*Q(2) + ZP*Q(3) .GE. 0.) GO TO 13
          IF (X(NEXT)*Q(1) + Y(NEXT)*Q(2) + Z(NEXT)*Q(3)
     .        .GE. 0.) GO TO 13
C
C   P, NEXT, N1, and N2 are nearly collinear and N1 is the
C     rightmost visible node.
C
          NF = N1
        ENDIF
C
C Bottom of clockwise loop:
C
        N2 = N1
        N1 = NEXT
        IF (N1 .NE. N1S) GO TO 12
C
C All boundary nodes are visible from P.
C
        I1 = N1
        I2 = N1
        I3 = 0
        RETURN
C
C N1 is the leftmost visible node.
C
   13   NL = N1
      ENDIF
C
C NF and NL have been found.
C
      I1 = NF
      I2 = NL
      I3 = 0
      RETURN
C
C All points are collinear (coplanar).
C
   14 I1 = 0
      I2 = 0
      I3 = 0
      RETURN
      END
      SUBROUTINE TRLIST (N,LIST,LPTR,LEND,NROW, NT,LTRI,IER)
      INTEGER N, LIST(*), LPTR(*), LEND(N), NROW, NT,
     .        LTRI(NROW,*), IER
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/20/96
C
C   This subroutine converts a triangulation data structure
C from the linked list created by Subroutine TRMESH to a
C triangle list.
C
C On input:
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       LIST,LPTR,LEND = Linked list data structure defin-
C                        ing the triangulation.  Refer to
C                        Subroutine TRMESH.
C
C       NROW = Number of rows (entries per triangle) re-
C              served for the triangle list LTRI.  The value
C              must be 6 if only the vertex indexes and
C              neighboring triangle indexes are to be
C              stored, or 9 if arc indexes are also to be
C              assigned and stored.  Refer to LTRI.
C
C The above parameters are not altered by this routine.
C
C       LTRI = Integer array of length at least NROW*NT,
C              where NT is at most 2N-4.  (A sufficient
C              length is 12N if NROW=6 or 18N if NROW=9.)
C
C On output:
C
C       NT = Number of triangles in the triangulation unless
C            IER .NE. 0, in which case NT = 0.  NT = 2N-NB-2
C            if NB .GE. 3 or 2N-4 if NB = 0, where NB is the
C            number of boundary nodes.
C
C       LTRI = NROW by NT array whose J-th column contains
C              the vertex nodal indexes (first three rows),
C              neighboring triangle indexes (second three
C              rows), and, if NROW = 9, arc indexes (last
C              three rows) associated with triangle J for
C              J = 1,...,NT.  The vertices are ordered
C              counterclockwise with the first vertex taken
C              to be the one with smallest index.  Thus,
C              LTRI(2,J) and LTRI(3,J) are larger than
C              LTRI(1,J) and index adjacent neighbors of
C              node LTRI(1,J).  For I = 1,2,3, LTRI(I+3,J)
C              and LTRI(I+6,J) index the triangle and arc,
C              respectively, which are opposite (not shared
C              by) node LTRI(I,J), with LTRI(I+3,J) = 0 if
C              LTRI(I+6,J) indexes a boundary arc.  Vertex
C              indexes range from 1 to N, triangle indexes
C              from 0 to NT, and, if included, arc indexes
C              from 1 to NA, where NA = 3N-NB-3 if NB .GE. 3
C              or 3N-6 if NB = 0.  The triangles are or-
C              dered on first (smallest) vertex indexes.
C
C       IER = Error indicator.
C             IER = 0 if no errors were encountered.
C             IER = 1 if N or NROW is outside its valid
C                     range on input.
C             IER = 2 if the triangulation data structure
C                     (LIST,LPTR,LEND) is invalid.  Note,
C                     however, that these arrays are not
C                     completely tested for validity.
C
C Modules required by TRLIST:  None
C
C Intrinsic function called by TRLIST:  ABS
C
C***********************************************************
C
      INTEGER I, I1, I2, I3, ISV, J, KA, KN, KT, LP, LP2,
     .        LPL, LPLN1, N1, N2, N3, NM2
      LOGICAL ARCS
C
C Local parameters:
C
C ARCS =     Logical variable with value TRUE iff are
C              indexes are to be stored
C I,J =      LTRI row indexes (1 to 3) associated with
C              triangles KT and KN, respectively
C I1,I2,I3 = Nodal indexes of triangle KN
C ISV =      Variable used to permute indexes I1,I2,I3
C KA =       Arc index and number of currently stored arcs
C KN =       Index of the triangle that shares arc I1-I2
C              with KT
C KT =       Triangle index and number of currently stored
C              triangles
C LP =       LIST pointer
C LP2 =      Pointer to N2 as a neighbor of N1
C LPL =      Pointer to the last neighbor of I1
C LPLN1 =    Pointer to the last neighbor of N1
C N1,N2,N3 = Nodal indexes of triangle KT
C NM2 =      N-2
C
C
C Test for invalid input parameters.
C
      IF (N .LT. 3  .OR.  (NROW .NE. 6  .AND.  NROW .NE. 9))
     .  GO TO 11
C
C Initialize parameters for loop on triangles KT = (N1,N2,
C   N3), where N1 < N2 and N1 < N3.
C
C   ARCS = TRUE iff arc indexes are to be stored.
C   KA,KT = Numbers of currently stored arcs and triangles.
C   NM2 = Upper bound on candidates for N1.
C
      ARCS = NROW .EQ. 9
      KA = 0
      KT = 0
      NM2 = N-2
C
C Loop on nodes N1.
C
      DO 9 N1 = 1,NM2
C
C Loop on pairs of adjacent neighbors (N2,N3).  LPLN1 points
C   to the last neighbor of N1, and LP2 points to N2.
C
        LPLN1 = LEND(N1)
        LP2 = LPLN1
    1     LP2 = LPTR(LP2)
          N2 = LIST(LP2)
          LP = LPTR(LP2)
          N3 = ABS(LIST(LP))
          IF (N2 .LT. N1  .OR.  N3 .LT. N1) GO TO 8
C
C Add a new triangle KT = (N1,N2,N3).
C
          KT = KT + 1
          LTRI(1,KT) = N1
          LTRI(2,KT) = N2
          LTRI(3,KT) = N3
C
C Loop on triangle sides (I2,I1) with neighboring triangles
C   KN = (I1,I2,I3).
C
          DO 7 I = 1,3
            IF (I .EQ. 1) THEN
              I1 = N3
              I2 = N2
            ELSEIF (I .EQ. 2) THEN
              I1 = N1
              I2 = N3
            ELSE
              I1 = N2
              I2 = N1
            ENDIF
C
C Set I3 to the neighbor of I1 that follows I2 unless
C   I2->I1 is a boundary arc.
C
            LPL = LEND(I1)
            LP = LPTR(LPL)
    2       IF (LIST(LP) .EQ. I2) GO TO 3
              LP = LPTR(LP)
              IF (LP .NE. LPL) GO TO 2
C
C   I2 is the last neighbor of I1 unless the data structure
C     is invalid.  Bypass the search for a neighboring
C     triangle if I2->I1 is a boundary arc.
C
            IF (ABS(LIST(LP)) .NE. I2) GO TO 12
            KN = 0
            IF (LIST(LP) .LT. 0) GO TO 6
C
C   I2->I1 is not a boundary arc, and LP points to I2 as
C     a neighbor of I1.
C
    3       LP = LPTR(LP)
            I3 = ABS(LIST(LP))
C
C Find J such that LTRI(J,KN) = I3 (not used if KN > KT),
C   and permute the vertex indexes of KN so that I1 is
C   smallest.
C
            IF (I1 .LT. I2  .AND.  I1 .LT. I3) THEN
              J = 3
            ELSEIF (I2 .LT. I3) THEN
              J = 2
              ISV = I1
              I1 = I2
              I2 = I3
              I3 = ISV
            ELSE
              J = 1
              ISV = I1
              I1 = I3
              I3 = I2
              I2 = ISV
            ENDIF
C
C Test for KN > KT (triangle index not yet assigned).
C
            IF (I1 .GT. N1) GO TO 7
C
C Find KN, if it exists, by searching the triangle list in
C   reverse order.
C
            DO 4 KN = KT-1,1,-1
              IF (LTRI(1,KN) .EQ. I1  .AND.  LTRI(2,KN) .EQ.
     .            I2  .AND.  LTRI(3,KN) .EQ. I3) GO TO 5
    4         CONTINUE
            GO TO 7
C
C Store KT as a neighbor of KN.
C
    5       LTRI(J+3,KN) = KT
C
C Store KN as a neighbor of KT, and add a new arc KA.
C
    6       LTRI(I+3,KT) = KN
            IF (ARCS) THEN
              KA = KA + 1
              LTRI(I+6,KT) = KA
              IF (KN .NE. 0) LTRI(J+6,KN) = KA
            ENDIF
    7       CONTINUE
C
C Bottom of loop on triangles.
C
    8     IF (LP2 .NE. LPLN1) GO TO 1
    9     CONTINUE
C
C No errors encountered.
C
      NT = KT
      IER = 0
      RETURN
C
C Invalid input parameter.
C
   11 NT = 0
      IER = 1
      RETURN
C
C Invalid triangulation data structure:  I1 is a neighbor of
C   I2, but I2 is not a neighbor of I1.
C
   12 NT = 0
      IER = 2
      RETURN
      END
      SUBROUTINE TRLPRT (N,X,Y,Z,IFLAG,NROW,NT,LTRI,LOUT)
      INTEGER N, IFLAG, NROW, NT, LTRI(NROW,NT), LOUT
      REAL X(N), Y(N), Z(N)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/02/98
C
C   This subroutine prints the triangle list created by Sub-
C routine TRLIST and, optionally, the nodal coordinates
C (either latitude and longitude or Cartesian coordinates)
C on logical unit LOUT.  The numbers of boundary nodes,
C triangles, and arcs are also printed.
C
C
C On input:
C
C       N = Number of nodes in the triangulation.
C           3 .LE. N .LE. 9999.
C
C       X,Y,Z = Arrays of length N containing the Cartesian
C               coordinates of the nodes if IFLAG = 0, or
C               (X and Y only) arrays of length N containing
C               longitude and latitude, respectively, if
C               IFLAG > 0, or unused dummy parameters if
C               IFLAG < 0.
C
C       IFLAG = Nodal coordinate option indicator:
C               IFLAG = 0 if X, Y, and Z (assumed to contain
C                         Cartesian coordinates) are to be
C                         printed (to 6 decimal places).
C               IFLAG > 0 if only X and Y (assumed to con-
C                         tain longitude and latitude) are
C                         to be printed (to 6 decimal
C                         places).
C               IFLAG < 0 if only the adjacency lists are to
C                         be printed.
C
C       NROW = Number of rows (entries per triangle) re-
C              served for the triangle list LTRI.  The value
C              must be 6 if only the vertex indexes and
C              neighboring triangle indexes are stored, or 9
C              if arc indexes are also stored.
C
C       NT = Number of triangles in the triangulation.
C            1 .LE. NT .LE. 9999.
C
C       LTRI = NROW by NT array whose J-th column contains
C              the vertex nodal indexes (first three rows),
C              neighboring triangle indexes (second three
C              rows), and, if NROW = 9, arc indexes (last
C              three rows) associated with triangle J for
C              J = 1,...,NT.
C
C       LOUT = Logical unit number for output.  If LOUT is
C              not in the range 0 to 99, output is written
C              to unit 6.
C
C Input parameters are not altered by this routine.
C
C On output:
C
C   The triangle list and nodal coordinates (as specified by
C IFLAG) are written to unit LOUT.
C
C Modules required by TRLPRT:  None
C
C***********************************************************
C
      INTEGER I, K, LUN, NA, NB, NL, NLMAX, NMAX
      DATA    NMAX/9999/,  NLMAX/58/
C
C Local parameters:
C
C I =     DO-loop, nodal index, and row index for LTRI
C K =     DO-loop and triangle index
C LUN =   Logical unit number for output
C NA =    Number of triangulation arcs
C NB =    Number of boundary nodes
C NL =    Number of lines printed on the current page
C NLMAX = Maximum number of print lines per page (except
C           for the last page which may have two addi-
C           tional lines)
C NMAX =  Maximum value of N and NT (4-digit format)
C
      LUN = LOUT
      IF (LUN .LT. 0  .OR.  LUN .GT. 99) LUN = 6
C
C Print a heading and test for invalid input.
C
C      open(18,file='nodes.dat',STATUS= 'UNKNOWN')
C      do ig = 1,N
C         read(18,*) x(ig),y(ig),z(ig)
C      enddo
C      close(18)

      WRITE (LUN,100) N
      NL = 3
      IF (N .LT. 3  .OR.  N .GT. NMAX  .OR.
     .    (NROW .NE. 6  .AND.  NROW .NE. 9)  .OR.
     .    NT .LT. 1  .OR.  NT .GT. NMAX) THEN
C
C Print an error message and exit.
C
        WRITE (LUN,110) N, NROW, NT
        RETURN
      ENDIF
      IF (IFLAG .EQ. 0) THEN
C
C Print X, Y, and Z.
C
        WRITE (LUN,101)
        NL = 6
        DO 1 I = 1,N
          IF (NL .GE. NLMAX) THEN
            WRITE (LUN,108)
            NL = 0
          ENDIF
          WRITE (LUN,103) I, X(I), Y(I), Z(I)
          NL = NL + 1
    1     CONTINUE
      ELSEIF (IFLAG .GT. 0) THEN
C
C Print X (longitude) and Y (latitude).
C
        WRITE (LUN,102)
        NL = 6
        DO 2 I = 1,N
          IF (NL .GE. NLMAX) THEN
            WRITE (LUN,108)
            NL = 0
          ENDIF
!          WRITE (LUN,104) I, X(I), Y(I)
          WRITE (LUN,103) I, X(I), Y(I), Z(I)
          NL = NL + 1
    2     CONTINUE
      ENDIF
C
C Print the triangulation LTRI.
C
      IF (NL .GT. NLMAX/2) THEN
        WRITE (LUN,108)
        NL = 0
      ENDIF
      IF (NROW .EQ. 6) THEN
        WRITE (LUN,105)
      ELSE
        WRITE (LUN,106)
      ENDIF
      NL = NL + 5
      DO 3 K = 1,NT
        IF (NL .GE. NLMAX) THEN
          WRITE (LUN,108)
          NL = 0
        ENDIF
        WRITE (LUN,107) K, (LTRI(I,K), I = 1,NROW)
        NL = NL + 1
    3   CONTINUE
C
C Print NB, NA, and NT (boundary nodes, arcs, and
C   triangles).
C
      NB = 2*N - NT - 2
      IF (NB .LT. 3) THEN
        NB = 0
        NA = 3*N - 6
      ELSE
        NA = NT + N - 1
      ENDIF
      WRITE (LUN,109) NB, NA, NT
      RETURN
C
C Print formats:
C
  100 FORMAT (///18X,'STRIPACK (TRLIST) Output,  N = ',I4)
  101 FORMAT (//8X,'Node',10X,'X(Node)',10X,'Y(Node)',10X,
     .        'Z(Node)'//)
!  102 FORMAT (//16X,'Node',8X,'Longitude',9X,'Latitude'//)
  102 FORMAT (//8X,'Node',8X,'XXXXXXXXX',9X,'YYYYYYYY',9X,'ZZZZZZZZ'//)

  103 FORMAT (8X,I4,3E17.6)
  104 FORMAT (16X,I4,2E17.6)
  105 FORMAT (//1X,'Triangle',8X,'Vertices',12X,'Neighbors'/
     .        4X,'KT',7X,'N1',5X,'N2',5X,'N3',4X,'KT1',4X,
     .        'KT2',4X,'KT3'/)
  106 FORMAT (//1X,'Triangle',8X,'Vertices',12X,'Neighbors',
     .        14X,'Arcs'/
     .        4X,'KT',7X,'N1',5X,'N2',5X,'N3',4X,'KT1',4X,
     .        'KT2',4X,'KT3',4X,'KA1',4X,'KA2',4X,'KA3'/)
  107 FORMAT (2X,I4,2X,6(3X,I4),3(2X,I5))
  108 FORMAT (///)
  109 FORMAT (/1X,'NB = ',I4,' Boundary Nodes',5X,
     .        'NA = ',I5,' Arcs',5X,'NT = ',I5,
     .        ' Triangles')
  110 FORMAT (//1X,10X,'*** Invalid Parameter:  N =',I5,
     .        ', NROW =',I5,', NT =',I5,' ***')
      END
      SUBROUTINE TRMESH (N,X,Y,Z, LIST,LPTR,LEND,LNEW,NEAR,
     .                   NEXT,DIST,IER)
      INTEGER N, LIST(*), LPTR(*), LEND(N), LNEW, NEAR(N),
     .        NEXT(N), IER
      REAL    X(N), Y(N), Z(N), DIST(N)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/08/99
C
C   This subroutine creates a Delaunay triangulation of a
C set of N arbitrarily distributed points, referred to as
C nodes, on the surface of the unit sphere.  The Delaunay
C triangulation is defined as a set of (spherical) triangles
C with the following five properties:
C
C  1)  The triangle vertices are nodes.
C  2)  No triangle contains a node other than its vertices.
C  3)  The interiors of the triangles are pairwise disjoint.
C  4)  The union of triangles is the convex hull of the set
C        of nodes (the smallest convex set that contains
C        the nodes).  If the nodes are not contained in a
C        single hemisphere, their convex hull is the en-
C        tire sphere and there are no boundary nodes.
C        Otherwise, there are at least three boundary nodes.
C  5)  The interior of the circumcircle of each triangle
C        contains no node.
C
C The first four properties define a triangulation, and the
C last property results in a triangulation which is as close
C as possible to equiangular in a certain sense and which is
C uniquely defined unless four or more nodes lie in a common
C plane.  This property makes the triangulation well-suited
C for solving closest-point problems and for triangle-based
C interpolation.
C
C   Provided the nodes are randomly ordered, the algorithm
C has expected time complexity O(N*log(N)) for most nodal
C distributions.  Note, however, that the complexity may be
C as high as O(N**2) if, for example, the nodes are ordered
C on increasing latitude.
C
C   Spherical coordinates (latitude and longitude) may be
C converted to Cartesian coordinates by Subroutine TRANS.
C
C   The following is a list of the software package modules
C which a user may wish to call directly:
C
C  ADDNOD - Updates the triangulation by appending a new
C             node.
C
C  AREAS  - Returns the area of a spherical triangle.
C
C  BNODES - Returns an array containing the indexes of the
C             boundary nodes (if any) in counterclockwise
C             order.  Counts of boundary nodes, triangles,
C             and arcs are also returned.
C
C  CIRCUM - Returns the circumcenter of a spherical trian-
C             gle.
C
C  CRLIST - Returns the set of triangle circumcenters
C             (Voronoi vertices) and circumradii associated
C             with a triangulation.
C
C  DELARC - Deletes a boundary arc from a triangulation.
C
C  DELNOD - Updates the triangulation with a nodal deletion.
C
C  EDGE   - Forces an arbitrary pair of nodes to be connec-
C             ted by an arc in the triangulation.
C
C  GETNP  - Determines the ordered sequence of L closest
C             nodes to a given node, along with the associ-
C             ated distances.
C
C  INSIDE - Locates a point relative to a polygon on the
C             surface of the sphere.
C
C  INTRSC - Returns the point of intersection between a
C             pair of great circle arcs.
C
C  JRAND  - Generates a uniformly distributed pseudo-random
C             integer.
C
C  LEFT   - Locates a point relative to a great circle.
C
C  NEARND - Returns the index of the nearest node to an
C             arbitrary point, along with its squared
C             distance.
C
C  SCOORD - Converts a point from Cartesian coordinates to
C             spherical coordinates.
C
C  STORE  - Forces a value to be stored in main memory so
C             that the precision of floating point numbers
C             in memory locations rather than registers is
C             computed.
C
C  TRANS  - Transforms spherical coordinates into Cartesian
C             coordinates on the unit sphere for input to
C             Subroutine TRMESH.
C
C  TRLIST - Converts the triangulation data structure to a
C             triangle list more suitable for use in a fin-
C             ite element code.
C
C  TRLPRT - Prints the triangle list created by Subroutine
C             TRLIST.
C
C  TRMESH - Creates a Delaunay triangulation of a set of
C             nodes.
C
C  TRPLOT - Creates a level-2 Encapsulated Postscript (EPS)
C             file containing a triangulation plot.
C
C  TRPRNT - Prints the triangulation data structure and,
C             optionally, the nodal coordinates.
C
C  VRPLOT - Creates a level-2 Encapsulated Postscript (EPS)
C             file containing a Voronoi diagram plot.
C
C
C On input:
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       X,Y,Z = Arrays of length N containing the Cartesian
C               coordinates of distinct nodes.  (X(K),Y(K),
C               Z(K)) is referred to as node K, and K is re-
C               ferred to as a nodal index.  It is required
C               that X(K)**2 + Y(K)**2 + Z(K)**2 = 1 for all
C               K.  The first three nodes must not be col-
C               linear (lie on a common great circle).
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR = Arrays of length at least 6N-12.
C
C       LEND = Array of length at least N.
C
C       NEAR,NEXT,DIST = Work space arrays of length at
C                        least N.  The space is used to
C                        efficiently determine the nearest
C                        triangulation node to each un-
C                        processed node for use by ADDNOD.
C
C On output:
C
C       LIST = Set of nodal indexes which, along with LPTR,
C              LEND, and LNEW, define the triangulation as a
C              set of N adjacency lists -- counterclockwise-
C              ordered sequences of neighboring nodes such
C              that the first and last neighbors of a bound-
C              ary node are boundary nodes (the first neigh-
C              bor of an interior node is arbitrary).  In
C              order to distinguish between interior and
C              boundary nodes, the last neighbor of each
C              boundary node is represented by the negative
C              of its index.
C
C       LPTR = Set of pointers (LIST indexes) in one-to-one
C              correspondence with the elements of LIST.
C              LIST(LPTR(I)) indexes the node which follows
C              LIST(I) in cyclical counterclockwise order
C              (the first neighbor follows the last neigh-
C              bor).
C
C       LEND = Set of pointers to adjacency lists.  LEND(K)
C              points to the last neighbor of node K for
C              K = 1,...,N.  Thus, LIST(LEND(K)) < 0 if and
C              only if K is a boundary node.
C
C       LNEW = Pointer to the first empty location in LIST
C              and LPTR (list length plus one).  LIST, LPTR,
C              LEND, and LNEW are not altered if IER < 0,
C              and are incomplete if IER > 0.
C
C       NEAR,NEXT,DIST = Garbage.
C
C       IER = Error indicator:
C             IER =  0 if no errors were encountered.
C             IER = -1 if N < 3 on input.
C             IER = -2 if the first three nodes are
C                      collinear.
C             IER =  L if nodes L and M coincide for some
C                      M > L.  The data structure represents
C                      a triangulation of nodes 1 to M-1 in
C                      this case.
C
C Modules required by TRMESH:  ADDNOD, BDYADD, COVSPH,
C                                INSERT, INTADD, JRAND,
C                                LEFT, LSTPTR, STORE, SWAP,
C                                SWPTST, TRFIND
C
C Intrinsic function called by TRMESH:  ABS
C
C***********************************************************
C
      INTEGER I, I0, J, K, LP, LPL, NEXTI, NN
      LOGICAL LEFT
      REAL    D, D1, D2, D3
C
C Local parameters:
C
C D =        (Negative cosine of) distance from node K to
C              node I
C D1,D2,D3 = Distances from node K to nodes 1, 2, and 3,
C              respectively
C I,J =      Nodal indexes
C I0 =       Index of the node preceding I in a sequence of
C              unprocessed nodes:  I = NEXT(I0)
C K =        Index of node to be added and DO-loop index:
C              K > 3
C LP =       LIST index (pointer) of a neighbor of K
C LPL =      Pointer to the last neighbor of K
C NEXTI =    NEXT(I)
C NN =       Local copy of N
C
      NN = N
      IF (NN .LT. 3) THEN
        IER = -1
        RETURN
      ENDIF
C
C Store the first triangle in the linked list.
C
      IF ( .NOT. LEFT (X(1),Y(1),Z(1),X(2),Y(2),Z(2),
     .                 X(3),Y(3),Z(3)) ) THEN
C
C   The first triangle is (3,2,1) = (2,1,3) = (1,3,2).
C
        LIST(1) = 3
        LPTR(1) = 2
        LIST(2) = -2
        LPTR(2) = 1
        LEND(1) = 2
C
        LIST(3) = 1
        LPTR(3) = 4
        LIST(4) = -3
        LPTR(4) = 3
        LEND(2) = 4
C
        LIST(5) = 2
        LPTR(5) = 6
        LIST(6) = -1
        LPTR(6) = 5
        LEND(3) = 6
C
      ELSEIF ( .NOT. LEFT(X(2),Y(2),Z(2),X(1),Y(1),Z(1),
     .                    X(3),Y(3),Z(3)) )
     .       THEN
C
C   The first triangle is (1,2,3):  3 Strictly Left 1->2,
C     i.e., node 3 lies in the left hemisphere defined by
C     arc 1->2.
C
        LIST(1) = 2
        LPTR(1) = 2
        LIST(2) = -3
        LPTR(2) = 1
        LEND(1) = 2
C
        LIST(3) = 3
        LPTR(3) = 4
        LIST(4) = -1
        LPTR(4) = 3
        LEND(2) = 4
C
        LIST(5) = 1
        LPTR(5) = 6
        LIST(6) = -2
        LPTR(6) = 5
        LEND(3) = 6
C
      ELSE
C
C   The first three nodes are collinear.
C
        IER = -2
        RETURN
      ENDIF
C
C Initialize LNEW and test for N = 3.
C
      LNEW = 7
      IF (NN .EQ. 3) THEN
        IER = 0
        RETURN
      ENDIF
C
C A nearest-node data structure (NEAR, NEXT, and DIST) is
C   used to obtain an expected-time (N*log(N)) incremental
C   algorithm by enabling constant search time for locating
C   each new node in the triangulation.
C
C For each unprocessed node K, NEAR(K) is the index of the
C   triangulation node closest to K (used as the starting
C   point for the search in Subroutine TRFIND) and DIST(K)
C   is an increasing function of the arc length (angular
C   distance) between nodes K and NEAR(K):  -Cos(a) for arc
C   length a.
C
C Since it is necessary to efficiently find the subset of
C   unprocessed nodes associated with each triangulation
C   node J (those that have J as their NEAR entries), the
C   subsets are stored in NEAR and NEXT as follows:  for
C   each node J in the triangulation, I = NEAR(J) is the
C   first unprocessed node in J's set (with I = 0 if the
C   set is empty), L = NEXT(I) (if I > 0) is the second,
C   NEXT(L) (if L > 0) is the third, etc.  The nodes in each
C   set are initially ordered by increasing indexes (which
C   maximizes efficiency) but that ordering is not main-
C   tained as the data structure is updated.
C
C Initialize the data structure for the single triangle.
C
      NEAR(1) = 0
      NEAR(2) = 0
      NEAR(3) = 0
      DO 1 K = NN,4,-1
        D1 = -(X(K)*X(1) + Y(K)*Y(1) + Z(K)*Z(1))
        D2 = -(X(K)*X(2) + Y(K)*Y(2) + Z(K)*Z(2))
        D3 = -(X(K)*X(3) + Y(K)*Y(3) + Z(K)*Z(3))
        IF (D1 .LE. D2  .AND.  D1 .LE. D3) THEN
          NEAR(K) = 1
          DIST(K) = D1
          NEXT(K) = NEAR(1)
          NEAR(1) = K
        ELSEIF (D2 .LE. D1  .AND.  D2 .LE. D3) THEN
          NEAR(K) = 2
          DIST(K) = D2
          NEXT(K) = NEAR(2)
          NEAR(2) = K
        ELSE
          NEAR(K) = 3
          DIST(K) = D3
          NEXT(K) = NEAR(3)
          NEAR(3) = K
        ENDIF
    1   CONTINUE
C
C Add the remaining nodes
C
      DO 6 K = 4,NN
        CALL ADDNOD (NEAR(K),K,X,Y,Z, LIST,LPTR,LEND,
     .               LNEW, IER)
        IF (IER .NE. 0) RETURN
C
C Remove K from the set of unprocessed nodes associated
C   with NEAR(K).
C
        I = NEAR(K)
        IF (NEAR(I) .EQ. K) THEN
          NEAR(I) = NEXT(K)
        ELSE
          I = NEAR(I)
    2     I0 = I
            I = NEXT(I0)
            IF (I .NE. K) GO TO 2
          NEXT(I0) = NEXT(K)
        ENDIF
        NEAR(K) = 0
C
C Loop on neighbors J of node K.
C
        LPL = LEND(K)
        LP = LPL
    3   LP = LPTR(LP)
          J = ABS(LIST(LP))
C
C Loop on elements I in the sequence of unprocessed nodes
C   associated with J:  K is a candidate for replacing J
C   as the nearest triangulation node to I.  The next value
C   of I in the sequence, NEXT(I), must be saved before I
C   is moved because it is altered by adding I to K's set.
C
          I = NEAR(J)
    4     IF (I .EQ. 0) GO TO 5
          NEXTI = NEXT(I)
C
C Test for the distance from I to K less than the distance
C   from I to J.
C
          D = -(X(I)*X(K) + Y(I)*Y(K) + Z(I)*Z(K))
          IF (D .LT. DIST(I)) THEN
C
C Replace J by K as the nearest triangulation node to I:
C   update NEAR(I) and DIST(I), and remove I from J's set
C   of unprocessed nodes and add it to K's set.
C
            NEAR(I) = K
            DIST(I) = D
            IF (I .EQ. NEAR(J)) THEN
              NEAR(J) = NEXTI
            ELSE
              NEXT(I0) = NEXTI
            ENDIF
            NEXT(I) = NEAR(K)
            NEAR(K) = I
          ELSE
            I0 = I
          ENDIF
C
C Bottom of loop on I.
C
          I = NEXTI
          GO TO 4
C
C Bottom of loop on neighbors J.
C
    5     IF (LP .NE. LPL) GO TO 3
    6   CONTINUE
      RETURN
      END
      SUBROUTINE TRPLOT (LUN,PLTSIZ,ELAT,ELON,A,N,X,Y,Z,
     .                   LIST,LPTR,LEND,TITLE,NUMBR, IER)
      CHARACTER*(*) TITLE
      INTEGER   LUN, N, LIST(*), LPTR(*), LEND(N), IER
      LOGICAL   NUMBR
      REAL      PLTSIZ, ELAT, ELON, A, X(N), Y(N), Z(N)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/16/98
C
C   This subroutine creates a level-2 Encapsulated Post-
C script (EPS) file containing a graphical display of a
C triangulation of a set of nodes on the unit sphere.  The
C visible nodes are projected onto the plane that contains
C the origin and has normal defined by a user-specified eye-
C position.  Projections of adjacent (visible) nodes are
C connected by line segments.
C
C
C On input:
C
C       LUN = Logical unit number in the range 0 to 99.
C             The unit should be opened with an appropriate
C             file name before the call to this routine.
C
C       PLTSIZ = Plot size in inches.  A circular window in
C                the projection plane is mapped to a circu-
C                lar viewport with diameter equal to .88*
C                PLTSIZ (leaving room for labels outside the
C                viewport).  The viewport is centered on the
C                8.5 by 11 inch page, and its boundary is
C                drawn.  1.0 .LE. PLTSIZ .LE. 8.5.
C
C       ELAT,ELON = Latitude and longitude (in degrees) of
C                   the center of projection E (the center
C                   of the plot).  The projection plane is
C                   the plane that contains the origin and
C                   has E as unit normal.  In a rotated
C                   coordinate system for which E is the
C                   north pole, the projection plane con-
C                   tains the equator, and only northern
C                   hemisphere nodes are visible (from the
C                   point at infinity in the direction E).
C                   These are projected orthogonally onto
C                   the projection plane (by zeroing the z-
C                   component in the rotated coordinate
C                   system).  ELAT and ELON must be in the
C                   range -90 to 90 and -180 to 180, respec-
C                   tively.
C
C       A = Angular distance in degrees from E to the boun-
C           dary of a circular window against which the
C           triangulation is clipped.  The projected window
C           is a disk of radius r = Sin(A) centered at the
C           origin, and only visible nodes whose projections
C           are within distance r of the origin are included
C           in the plot.  Thus, if A = 90, the plot includes
C           the entire hemisphere centered at E.  0 .LT. A
C           .LE. 90.
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       X,Y,Z = Arrays of length N containing the Cartesian
C               coordinates of the nodes (unit vectors).
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to Subroutine
C                        TRMESH.
C
C       TITLE = Type CHARACTER variable or constant contain-
C               ing a string to be centered above the plot.
C               The string must be enclosed in parentheses;
C               i.e., the first and last characters must be
C               '(' and ')', respectively, but these are not
C               displayed.  TITLE may have at most 80 char-
C               acters including the parentheses.
C
C       NUMBR = Option indicator:  If NUMBR = TRUE, the
C               nodal indexes are plotted next to the nodes.
C
C Input parameters are not altered by this routine.
C
C On output:
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if LUN, PLTSIZ, or N is outside its
C                     valid range.
C             IER = 2 if ELAT, ELON, or A is outside its
C                     valid range.
C             IER = 3 if an error was encountered in writing
C                     to unit LUN.
C
C   The values in the data statement below may be altered
C in order to modify various plotting options.
C
C Modules required by TRPLOT:  None
C
C Intrinsic functions called by TRPLOT:  ABS, ATAN, COS,
C                                          NINT, REAL, SIN,
C                                          SQRT
C
C***********************************************************
C
      INTEGER IPX1, IPX2, IPY1, IPY2, IR, LP, LPL, N0, N1
      LOGICAL ANNOT
      REAL    CF, CT, EX, EY, EZ, FSIZN, FSIZT, R11, R12,
     .        R21, R22, R23, SF, T, TX, TY, WR, WRS, X0, X1,
     .        Y0, Y1, Z0, Z1
C
      DATA    ANNOT/.TRUE./,  FSIZN/10.0/,  FSIZT/16.0/
C
C Local parameters:
C
C ANNOT =     Logical variable with value TRUE iff the plot
C               is to be annotated with the values of ELAT,
C               ELON, and A
C CF =        Conversion factor for degrees to radians
C CT =        Cos(ELAT)
C EX,EY,EZ =  Cartesian coordinates of the eye-position E
C FSIZN =     Font size in points for labeling nodes with
C               their indexes if NUMBR = TRUE
C FSIZT =     Font size in points for the title (and
C               annotation if ANNOT = TRUE)
C IPX1,IPY1 = X and y coordinates (in points) of the lower
C               left corner of the bounding box or viewport
C               box
C IPX2,IPY2 = X and y coordinates (in points) of the upper
C               right corner of the bounding box or viewport
C               box
C IR =        Half the width (height) of the bounding box or
C               viewport box in points -- viewport radius
C LP =        LIST index (pointer)
C LPL =       Pointer to the last neighbor of N0
C N0 =        Index of a node whose incident arcs are to be
C               drawn
C N1 =        Neighbor of N0
C R11...R23 = Components of the first two rows of a rotation
C               that maps E to the north pole (0,0,1)
C SF =        Scale factor for mapping world coordinates
C               (window coordinates in [-WR,WR] X [-WR,WR])
C               to viewport coordinates in [IPX1,IPX2] X
C               [IPY1,IPY2]
C T =         Temporary variable
C TX,TY =     Translation vector for mapping world coordi-
C               nates to viewport coordinates
C WR =        Window radius r = Sin(A)
C WRS =       WR**2
C X0,Y0,Z0 =  Coordinates of N0 in the rotated coordinate
C               system or label location (X0,Y0)
C X1,Y1,Z1 =  Coordinates of N1 in the rotated coordinate
C               system or intersection of edge N0-N1 with
C               the equator (in the rotated coordinate
C               system)
C
C
C Test for invalid parameters.
C
      IF (LUN .LT. 0  .OR.  LUN .GT. 99  .OR.
     .    PLTSIZ .LT. 1.0  .OR.  PLTSIZ .GT. 8.5  .OR.
     .    N .LT. 3)
     .  GO TO 11
      IF (ABS(ELAT) .GT. 90.0  .OR.  ABS(ELON) .GT. 180.0
     .    .OR.  A .GT. 90.0) GO TO 12
C
C Compute a conversion factor CF for degrees to radians
C   and compute the window radius WR.
C
      CF = ATAN(1.0)/45.0
      WR = SIN(CF*A)
      WRS = WR*WR
C
C Compute the lower left (IPX1,IPY1) and upper right
C   (IPX2,IPY2) corner coordinates of the bounding box.
C   The coordinates, specified in default user space units
C   (points, at 72 points/inch with origin at the lower
C   left corner of the page), are chosen to preserve the
C   square aspect ratio, and to center the plot on the 8.5
C   by 11 inch page.  The center of the page is (306,396),
C   and IR = PLTSIZ/2 in points.
C
      IR = NINT(36.0*PLTSIZ)
      IPX1 = 306 - IR
      IPX2 = 306 + IR
      IPY1 = 396 - IR
      IPY2 = 396 + IR
C
C Output header comments.
C
      WRITE (LUN,100,ERR=13) IPX1, IPY1, IPX2, IPY2
  100 FORMAT ('%!PS-Adobe-3.0 EPSF-3.0'/
     .        '%%BoundingBox:',4I4/
     .        '%%Title:  Triangulation'/
     .        '%%Creator:  STRIPACK'/
     .        '%%EndComments')
C
C Set (IPX1,IPY1) and (IPX2,IPY2) to the corner coordinates
C   of a viewport box obtained by shrinking the bounding box
C   by 12% in each dimension.
C
      IR = NINT(0.88*REAL(IR))
      IPX1 = 306 - IR
      IPX2 = 306 + IR
      IPY1 = 396 - IR
      IPY2 = 396 + IR
C
C Set the line thickness to 2 points, and draw the
C   viewport boundary.
C
      T = 2.0
      WRITE (LUN,110,ERR=13) T
      WRITE (LUN,120,ERR=13) IR
      WRITE (LUN,130,ERR=13)
  110 FORMAT (F12.6,' setlinewidth')
  120 FORMAT ('306 396 ',I3,' 0 360 arc')
  130 FORMAT ('stroke')
C
C Set up an affine mapping from the window box [-WR,WR] X
C   [-WR,WR] to the viewport box.
C
      SF = REAL(IR)/WR
      TX = IPX1 + SF*WR
      TY = IPY1 + SF*WR
      WRITE (LUN,140,ERR=13) TX, TY, SF, SF
  140 FORMAT (2F12.6,' translate'/
     .        2F12.6,' scale')
C
C The line thickness must be changed to reflect the new
C   scaling which is applied to all subsequent output.
C   Set it to 1.0 point.
C
      T = 1.0/SF
      WRITE (LUN,110,ERR=13) T
C
C Save the current graphics state, and set the clip path to
C   the boundary of the window.
C
      WRITE (LUN,150,ERR=13)
      WRITE (LUN,160,ERR=13) WR
      WRITE (LUN,170,ERR=13)
  150 FORMAT ('gsave')
  160 FORMAT ('0 0 ',F12.6,' 0 360 arc')
  170 FORMAT ('clip newpath')
C
C Compute the Cartesian coordinates of E and the components
C   of a rotation R which maps E to the north pole (0,0,1).
C   R is taken to be a rotation about the z-axis (into the
C   yz-plane) followed by a rotation about the x-axis chosen
C   so that the view-up direction is (0,0,1), or (-1,0,0) if
C   E is the north or south pole.
C
C           ( R11  R12  0   )
C       R = ( R21  R22  R23 )
C           ( EX   EY   EZ  )
C
      T = CF*ELON
      CT = COS(CF*ELAT)
      EX = CT*COS(T)
      EY = CT*SIN(T)
      EZ = SIN(CF*ELAT)
      IF (CT .NE. 0.0) THEN
        R11 = -EY/CT
        R12 = EX/CT
      ELSE
        R11 = 0.0
        R12 = 1.0
      ENDIF
      R21 = -EZ*R12
      R22 = EZ*R11
      R23 = CT
C
C Loop on visible nodes N0 that project to points (X0,Y0) in
C   the window.
C
      DO 3 N0 = 1,N
        Z0 = EX*X(N0) + EY*Y(N0) + EZ*Z(N0)
        IF (Z0 .LT. 0.) GO TO 3
        X0 = R11*X(N0) + R12*Y(N0)
        Y0 = R21*X(N0) + R22*Y(N0) + R23*Z(N0)
        IF (X0*X0 + Y0*Y0 .GT. WRS) GO TO 3
        LPL = LEND(N0)
        LP = LPL
C
C Loop on neighbors N1 of N0.  LPL points to the last
C   neighbor of N0.  Copy the components of N1 into P.
C
    1   LP = LPTR(LP)
          N1 = ABS(LIST(LP))
          X1 = R11*X(N1) + R12*Y(N1)
          Y1 = R21*X(N1) + R22*Y(N1) + R23*Z(N1)
          Z1 = EX*X(N1) + EY*Y(N1) + EZ*Z(N1)
          IF (Z1 .LT. 0.) THEN
C
C   N1 is a 'southern hemisphere' point.  Move it to the
C     intersection of edge N0-N1 with the equator so that
C     the edge is clipped properly.  Z1 is implicitly set
C     to 0.
C
            X1 = Z0*X1 - Z1*X0
            Y1 = Z0*Y1 - Z1*Y0
            T = SQRT(X1*X1+Y1*Y1)
            X1 = X1/T
            Y1 = Y1/T
          ENDIF
C
C   If node N1 is in the window and N1 < N0, bypass edge
C     N0->N1 (since edge N1->N0 has already been drawn).
C
          IF ( Z1 .GE. 0.0  .AND.  X1*X1 + Y1*Y1 .LE. WRS
     .         .AND.  N1 .LT. N0 ) GO TO 2
C
C   Add the edge to the path.
C
          WRITE (LUN,180,ERR=13) X0, Y0, X1, Y1
  180     FORMAT (2F12.6,' moveto',2F12.6,' lineto')
C
C Bottom of loops.
C
    2     IF (LP .NE. LPL) GO TO 1
    3   CONTINUE
C
C Paint the path and restore the saved graphics state (with
C   no clip path).
C
      WRITE (LUN,130,ERR=13)
      WRITE (LUN,190,ERR=13)
  190 FORMAT ('grestore')
      IF (NUMBR) THEN
C
C Nodes in the window are to be labeled with their indexes.
C   Convert FSIZN from points to world coordinates, and
C   output the commands to select a font and scale it.
C
        T = FSIZN/SF
        WRITE (LUN,200,ERR=13) T
  200   FORMAT ('/Helvetica findfont'/
     .          F12.6,' scalefont setfont')
C
C Loop on visible nodes N0 that project to points (X0,Y0) in
C   the window.
C
        DO 4 N0 = 1,N
          IF (EX*X(N0) + EY*Y(N0) + EZ*Z(N0) .LT. 0.)
     .      GO TO 4
          X0 = R11*X(N0) + R12*Y(N0)
          Y0 = R21*X(N0) + R22*Y(N0) + R23*Z(N0)
          IF (X0*X0 + Y0*Y0 .GT. WRS) GO TO 4
C
C   Move to (X0,Y0) and draw the label N0.  The first char-
C     acter will will have its lower left corner about one
C     character width to the right of the nodal position.
C
          WRITE (LUN,210,ERR=13) X0, Y0
  210     FORMAT (2F12.6,' moveto')
          WRITE (LUN,220,ERR=13) N0
  220     FORMAT ('(',I3,') show')
    4     CONTINUE
      ENDIF
C
C Convert FSIZT from points to world coordinates, and output
C   the commands to select a font and scale it.
C
      T = FSIZT/SF
      WRITE (LUN,200,ERR=13) T
C
C Display TITLE centered above the plot:
C
C      Y0 = WR + 3.0*T
C      WRITE (LUN,230,ERR=13) TITLE, Y0
C  230 FORMAT (A80/'  stringwidth pop 2 div neg ',F12.6,
C     .        ' moveto')
C      WRITE (LUN,240,ERR=13) TITLE
C  240 FORMAT (A80/'  show')
C      IF (ANNOT) THEN
C
C Display the window center and radius below the plot.
C
C        X0 = -WR
C        Y0 = -WR - 50.0/SF
C        WRITE (LUN,210,ERR=13) X0, Y0
C        WRITE (LUN,250,ERR=13) ELAT, ELON
C        Y0 = Y0 - 2.0*T
C        WRITE (LUN,210,ERR=13) X0, Y0
C        WRITE (LUN,260,ERR=13) A
C  250   FORMAT ('(Window center:  ELAT = ',F7.2,
C     .          ',  ELON = ',F8.2,') show')
C  260   FORMAT ('(Angular extent:  A = ',F5.2,') show')
C      ENDIF
C
C Paint the path and output the showpage command and
C   end-of-file indicator.
C
      WRITE (LUN,270,ERR=13)
  270 FORMAT ('stroke'/
     .        'showpage'/
     .        '%%EOF')
C
C HP's interpreters require a one-byte End-of-PostScript-Job
C   indicator (to eliminate a timeout error message):
C   ASCII 4.
C
      WRITE (LUN,280,ERR=13) CHAR(4)
  280 FORMAT (A1)
C
C No error encountered.
C
      IER = 0
      RETURN
C
C Invalid input parameter LUN, PLTSIZ, or N.
C
   11 IER = 1
      RETURN
C
C Invalid input parameter ELAT, ELON, or A.
C
   12 IER = 2
      RETURN
C
C Error writing to unit LUN.
C
   13 IER = 3
      RETURN
      END
      SUBROUTINE TRPRNT (N,X,Y,Z,IFLAG,LIST,LPTR,LEND,LOUT)
      INTEGER N, IFLAG, LIST(*), LPTR(*), LEND(N), LOUT
      REAL    X(N), Y(N), Z(N)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/25/98
C
C   This subroutine prints the triangulation adjacency lists
C created by Subroutine TRMESH and, optionally, the nodal
C coordinates (either latitude and longitude or Cartesian
C coordinates) on logical unit LOUT.  The list of neighbors
C of a boundary node is followed by index 0.  The numbers of
C boundary nodes, triangles, and arcs are also printed.
C
C
C On input:
C
C       N = Number of nodes in the triangulation.  N .GE. 3
C           and N .LE. 9999.
C
C       X,Y,Z = Arrays of length N containing the Cartesian
C               coordinates of the nodes if IFLAG = 0, or
C               (X and Y only) arrays of length N containing
C               longitude and latitude, respectively, if
C               IFLAG > 0, or unused dummy parameters if
C               IFLAG < 0.
C
C       IFLAG = Nodal coordinate option indicator:
C               IFLAG = 0 if X, Y, and Z (assumed to contain
C                         Cartesian coordinates) are to be
C                         printed (to 6 decimal places).
C               IFLAG > 0 if only X and Y (assumed to con-
C                         tain longitude and latitude) are
C                         to be printed (to 6 decimal
C                         places).
C               IFLAG < 0 if only the adjacency lists are to
C                         be printed.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to Subroutine
C                        TRMESH.
C
C       LOUT = Logical unit for output.  If LOUT is not in
C              the range 0 to 99, output is written to
C              logical unit 6.
C
C Input parameters are not altered by this routine.
C
C On output:
C
C   The adjacency lists and nodal coordinates (as specified
C by IFLAG) are written to unit LOUT.
C
C Modules required by TRPRNT:  None
C
C***********************************************************
C
      INTEGER I, INC, K, LP, LPL, LUN, NA, NABOR(400), NB,
     .        ND, NL, NLMAX, NMAX, NODE, NN, NT
      DATA  NMAX/9999/,  NLMAX/58/
C
C Local parameters:
C
C I =     NABOR index (1 to K)
C INC =   Increment for NL associated with an adjacency list
C K =     Counter and number of neighbors of NODE
C LP =    LIST pointer of a neighbor of NODE
C LPL =   Pointer to the last neighbor of NODE
C LUN =   Logical unit for output (copy of LOUT)
C NA =    Number of arcs in the triangulation
C NABOR = Array containing the adjacency list associated
C           with NODE, with zero appended if NODE is a
C           boundary node
C NB =    Number of boundary nodes encountered
C ND =    Index of a neighbor of NODE (or negative index)
C NL =    Number of lines that have been printed on the
C           current page
C NLMAX = Maximum number of print lines per page (except
C           for the last page which may have two addi-
C           tional lines)
C NMAX =  Upper bound on N (allows 4-digit indexes)
C NODE =  Index of a node and DO-loop index (1 to N)
C NN =    Local copy of N
C NT =    Number of triangles in the triangulation
C
      NN = N
      LUN = LOUT
      IF (LUN .LT. 0  .OR.  LUN .GT. 99) LUN = 6
C
C Print a heading and test the range of N.
C
      WRITE (LUN,100) NN
      IF (NN .LT. 3  .OR.  NN .GT. NMAX) THEN
C
C N is outside its valid range.
C
        WRITE (LUN,110)
        RETURN
      ENDIF
C
C Initialize NL (the number of lines printed on the current
C   page) and NB (the number of boundary nodes encountered).
C
      NL = 6
      NB = 0
      IF (IFLAG .LT. 0) THEN
C
C Print LIST only.  K is the number of neighbors of NODE
C   that have been stored in NABOR.
C
        WRITE (LUN,101)
        DO 2 NODE = 1,NN
          LPL = LEND(NODE)
          LP = LPL
          K = 0
C
    1     K = K + 1
            LP = LPTR(LP)
            ND = LIST(LP)
            NABOR(K) = ND
            IF (LP .NE. LPL) GO TO 1
          IF (ND .LE. 0) THEN
C
C   NODE is a boundary node.  Correct the sign of the last
C     neighbor, add 0 to the end of the list, and increment
C     NB.
C
            NABOR(K) = -ND
            K = K + 1
            NABOR(K) = 0
            NB = NB + 1
          ENDIF
C
C   Increment NL and print the list of neighbors.
C
          INC = (K-1)/14 + 2
          NL = NL + INC
          IF (NL .GT. NLMAX) THEN
            WRITE (LUN,108)
            NL = INC
          ENDIF
          WRITE (LUN,104) NODE, (NABOR(I), I = 1,K)
          IF (K .NE. 14) WRITE (LUN,107)
    2     CONTINUE
      ELSEIF (IFLAG .GT. 0) THEN
C
C Print X (longitude), Y (latitude), and LIST.
C
        WRITE (LUN,102)
        DO 4 NODE = 1,NN
          LPL = LEND(NODE)
          LP = LPL
          K = 0
C
    3     K = K + 1
            LP = LPTR(LP)
            ND = LIST(LP)
            NABOR(K) = ND
            IF (LP .NE. LPL) GO TO 3
          IF (ND .LE. 0) THEN
C
C   NODE is a boundary node.
C
            NABOR(K) = -ND
            K = K + 1
            NABOR(K) = 0
            NB = NB + 1
          ENDIF
C
C   Increment NL and print X, Y, and NABOR.
C
          INC = (K-1)/8 + 2
          NL = NL + INC
          IF (NL .GT. NLMAX) THEN
            WRITE (LUN,108)
            NL = INC
          ENDIF
          WRITE (LUN,105) NODE, (NABOR(I), I = 1,K)
!          WRITE (LUN,105) NODE, X(NODE), Y(NODE),
!     .                    (NABOR(I), I = 1,K)
          IF (K .NE. 8) WRITE (LUN,107)
    4     CONTINUE
      ELSE
C
C Print X, Y, Z, and LIST.
C
        WRITE (LUN,103)
        DO 6 NODE = 1,NN
          LPL = LEND(NODE)
          LP = LPL
          K = 0
C
    5     K = K + 1
            LP = LPTR(LP)
            ND = LIST(LP)
            NABOR(K) = ND
            IF (LP .NE. LPL) GO TO 5
          IF (ND .LE. 0) THEN
C
C   NODE is a boundary node.
C
            NABOR(K) = -ND
            K = K + 1
            NABOR(K) = 0
            NB = NB + 1
          ENDIF
C
C   Increment NL and print X, Y, Z, and NABOR.
C
          INC = (K-1)/5 + 2
          NL = NL + INC
          IF (NL .GT. NLMAX) THEN
            WRITE (LUN,108)
            NL = INC
          ENDIF
          WRITE (LUN,106) NODE, X(NODE), Y(NODE),
     .                    Z(NODE), (NABOR(I), I = 1,K)
          IF (K .NE. 5) WRITE (LUN,107)
    6     CONTINUE
      ENDIF
C
C Print NB, NA, and NT (boundary nodes, arcs, and
C   triangles).
C
      IF (NB .NE. 0) THEN
        NA = 3*NN - NB - 3
        NT = 2*NN - NB - 2
      ELSE
        NA = 3*NN - 6
        NT = 2*NN - 4
      ENDIF
      WRITE (LUN,109) NB, NA, NT
      RETURN
C
C Print formats:
C
  100 FORMAT (///15X,'STRIPACK Triangulation Data ',
     .        'Structure,  N = ',I5//)
  101 FORMAT (1X,'Node',31X,'Neighbors of Node'//)
!  102 FORMAT (1X,'Node',5X,'Longitude',6X,'Latitude',
!     .        18X,'Neighbors of Node'//)
  102 FORMAT (1X,'Node', 8X,'Neighbors of Node'//)
  103 FORMAT (1X,'Node',5X,'X(Node)',8X,'Y(Node)',8X,
     .        'Z(Node)',11X,'Neighbors of Node'//)
  104 FORMAT (1X,I4,4X,14I5/(1X,8X,14I5))
!  105 FORMAT (1X,I4,2E15.6,4X,8I5/(1X,38X,8I5))
  105 FORMAT (1X,I4,4X,8I5/(1X,38X,8I5))
  106 FORMAT (1X,I4,3E15.6,4X,5I5/(1X,53X,5I5))
  107 FORMAT (1X)
  108 FORMAT (///)
  109 FORMAT (/1X,'NB = ',I4,' Boundary Nodes',5X,
     .        'NA = ',I5,' Arcs',5X,'NT = ',I5,
     .        ' Triangles')
  110 FORMAT (1X,10X,'*** N is outside its valid',
     .        ' range ***')
      END
      SUBROUTINE VRPLOT (LUN,PLTSIZ,ELAT,ELON,A,N,X,Y,Z,
     .                   NT,LISTC,LPTR,LEND,XC,YC,ZC,TITLE,
     .                   NUMBR, IER)
      CHARACTER*(*) TITLE
      INTEGER LUN, N, NT, LISTC(*), LPTR(*), LEND(N), IER
      LOGICAL NUMBR
      REAL    PLTSIZ, ELAT, ELON, A, X(N), Y(N), Z(N),
     .        XC(NT), YC(NT), ZC(NT)
C
C***********************************************************
C
C                                              From STRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                           renka@cs.unt.edu
C                                                   07/16/98
C
C   This subroutine creates a level-2 Encapsulated Post-
C script (EPS) file containing a graphical depiction of a
C Voronoi diagram of a set of nodes on the unit sphere.
C The visible vertices are projected onto the plane that
C contains the origin and has normal defined by a user-
C specified eye-position.  Projections of adjacent (visible)
C Voronoi vertices are connected by line segments.
C
C   The parameters defining the Voronoi diagram may be com-
C puted by Subroutine CRLIST.
C
C
C On input:
C
C       LUN = Logical unit number in the range 0 to 99.
C             The unit should be opened with an appropriate
C             file name before the call to this routine.
C
C       PLTSIZ = Plot size in inches.  A circular window in
C                the projection plane is mapped to a circu-
C                lar viewport with diameter equal to .88*
C                PLTSIZ (leaving room for labels outside the
C                viewport).  The viewport is centered on the
C                8.5 by 11 inch page, and its boundary is
C                drawn.  1.0 .LE. PLTSIZ .LE. 8.5.
C
C       ELAT,ELON = Latitude and longitude (in degrees) of
C                   the center of projection E (the center
C                   of the plot).  The projection plane is
C                   the plane that contains the origin and
C                   has E as unit normal.  In a rotated
C                   coordinate system for which E is the
C                   north pole, the projection plane con-
C                   tains the equator, and only northern
C                   hemisphere points are visible (from the
C                   point at infinity in the direction E).
C                   These are projected orthogonally onto
C                   the projection plane (by zeroing the z-
C                   component in the rotated coordinate
C                   system).  ELAT and ELON must be in the
C                   range -90 to 90 and -180 to 180, respec-
C                   tively.
C
C       A = Angular distance in degrees from E to the boun-
C           dary of a circular window against which the
C           Voronoi diagram is clipped.  The projected win-
C           dow is a disk of radius r = Sin(A) centered at
C           the origin, and only visible vertices whose
C           projections are within distance r of the origin
C           are included in the plot.  Thus, if A = 90, the
C           plot includes the entire hemisphere centered at
C           E.  0 .LT. A .LE. 90.
C
C       N = Number of nodes (Voronoi centers) and Voronoi
C           regions.  N .GE. 3.
C
C       X,Y,Z = Arrays of length N containing the Cartesian
C               coordinates of the nodes (unit vectors).
C
C       NT = Number of Voronoi region vertices (triangles,
C            including those in the extended triangulation
C            if the number of boundary nodes NB is nonzero):
C            NT = 2*N-4.
C
C       LISTC = Array of length 3*NT containing triangle
C               indexes (indexes to XC, YC, and ZC) stored
C               in 1-1 correspondence with LIST/LPTR entries
C               (or entries that would be stored in LIST for
C               the extended triangulation):  the index of
C               triangle (N1,N2,N3) is stored in LISTC(K),
C               LISTC(L), and LISTC(M), where LIST(K),
C               LIST(L), and LIST(M) are the indexes of N2
C               as a neighbor of N1, N3 as a neighbor of N2,
C               and N1 as a neighbor of N3.  The Voronoi
C               region associated with a node is defined by
C               the CCW-ordered sequence of circumcenters in
C               one-to-one correspondence with its adjacency
C               list (in the extended triangulation).
C
C       LPTR = Array of length 3*NT = 6*N-12 containing a
C              set of pointers (LISTC indexes) in one-to-one
C              correspondence with the elements of LISTC.
C              LISTC(LPTR(I)) indexes the triangle which
C              follows LISTC(I) in cyclical counterclockwise
C              order (the first neighbor follows the last
C              neighbor).
C
C       LEND = Array of length N containing a set of
C              pointers to triangle lists.  LP = LEND(K)
C              points to a triangle (indexed by LISTC(LP))
C              containing node K for K = 1 to N.
C
C       XC,YC,ZC = Arrays of length NT containing the
C                  Cartesian coordinates of the triangle
C                  circumcenters (Voronoi vertices).
C                  XC(I)**2 + YC(I)**2 + ZC(I)**2 = 1.
C
C       TITLE = Type CHARACTER variable or constant contain-
C               ing a string to be centered above the plot.
C               The string must be enclosed in parentheses;
C               i.e., the first and last characters must be
C               '(' and ')', respectively, but these are not
C               displayed.  TITLE may have at most 80 char-
C               acters including the parentheses.
C
C       NUMBR = Option indicator:  If NUMBR = TRUE, the
C               nodal indexes are plotted at the Voronoi
C               region centers.
C
C Input parameters are not altered by this routine.
C
C On output:
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = 1 if LUN, PLTSIZ, N, or NT is outside
C                     its valid range.
C             IER = 2 if ELAT, ELON, or A is outside its
C                     valid range.
C             IER = 3 if an error was encountered in writing
C                     to unit LUN.
C
C Modules required by VRPLOT:  None
C
C Intrinsic functions called by VRPLOT:  ABS, ATAN, COS,
C                                          NINT, REAL, SIN,
C                                          SQRT
C
C***********************************************************
C
      INTEGER IPX1, IPX2, IPY1, IPY2, IR, KV1, KV2, LP, LPL,
     .        N0
      LOGICAL ANNOT, IN1, IN2
      REAL    CF, CT, EX, EY, EZ, FSIZN, FSIZT, R11, R12,
     .        R21, R22, R23, SF, T, TX, TY, WR, WRS, X0, X1,
     .        X2, Y0, Y1, Y2, Z1, Z2
C
      DATA    ANNOT/.TRUE./,  FSIZN/10.0/,  FSIZT/16.0/
C
C Local parameters:
C
C ANNOT =     Logical variable with value TRUE iff the plot
C               is to be annotated with the values of ELAT,
C               ELON, and A
C CF =        Conversion factor for degrees to radians
C CT =        Cos(ELAT)
C EX,EY,EZ =  Cartesian coordinates of the eye-position E
C FSIZN =     Font size in points for labeling nodes with
C               their indexes if NUMBR = TRUE
C FSIZT =     Font size in points for the title (and
C               annotation if ANNOT = TRUE)
C IN1,IN2 =   Logical variables with value TRUE iff the
C               projections of vertices KV1 and KV2, respec-
C               tively, are inside the window
C IPX1,IPY1 = X and y coordinates (in points) of the lower
C               left corner of the bounding box or viewport
C               box
C IPX2,IPY2 = X and y coordinates (in points) of the upper
C               right corner of the bounding box or viewport
C               box
C IR =        Half the width (height) of the bounding box or
C               viewport box in points -- viewport radius
C KV1,KV2 =   Endpoint indexes of a Voronoi edge
C LP =        LIST index (pointer)
C LPL =       Pointer to the last neighbor of N0
C N0 =        Index of a node
C R11...R23 = Components of the first two rows of a rotation
C               that maps E to the north pole (0,0,1)
C SF =        Scale factor for mapping world coordinates
C               (window coordinates in [-WR,WR] X [-WR,WR])
C               to viewport coordinates in [IPX1,IPX2] X
C               [IPY1,IPY2]
C T =         Temporary variable
C TX,TY =     Translation vector for mapping world coordi-
C               nates to viewport coordinates
C WR =        Window radius r = Sin(A)
C WRS =       WR**2
C X0,Y0 =     Projection plane coordinates of node N0 or
C               label location
C X1,Y1,Z1 =  Coordinates of vertex KV1 in the rotated
C               coordinate system
C X2,Y2,Z2 =  Coordinates of vertex KV2 in the rotated
C               coordinate system or intersection of edge
C               KV1-KV2 with the equator (in the rotated
C               coordinate system)
C
C
C Test for invalid parameters.
C
      IF (LUN .LT. 0  .OR.  LUN .GT. 99  .OR.
     .    PLTSIZ .LT. 1.0  .OR.  PLTSIZ .GT. 8.5  .OR.
     .    N .LT. 3  .OR.  NT .NE. 2*N-4)
     .  GO TO 11
      IF (ABS(ELAT) .GT. 90.0  .OR.  ABS(ELON) .GT. 180.0
     .    .OR.  A .GT. 90.0) GO TO 12
C
C Compute a conversion factor CF for degrees to radians
C   and compute the window radius WR.
C
      CF = ATAN(1.0)/45.0
      WR = SIN(CF*A)
      WRS = WR*WR
C
C Compute the lower left (IPX1,IPY1) and upper right
C   (IPX2,IPY2) corner coordinates of the bounding box.
C   The coordinates, specified in default user space units
C   (points, at 72 points/inch with origin at the lower
C   left corner of the page), are chosen to preserve the
C   square aspect ratio, and to center the plot on the 8.5
C   by 11 inch page.  The center of the page is (306,396),
C   and IR = PLTSIZ/2 in points.
C
      IR = NINT(36.0*PLTSIZ)
      IPX1 = 306 - IR
      IPX2 = 306 + IR
      IPY1 = 396 - IR
      IPY2 = 396 + IR
C
C Output header comments.
C
      WRITE (LUN,100,ERR=13) IPX1, IPY1, IPX2, IPY2
  100 FORMAT ('%!PS-Adobe-3.0 EPSF-3.0'/
     .        '%%BoundingBox:',4I4/
     .        '%%Title:  Voronoi diagram'/
     .        '%%Creator:  STRIPACK'/
     .        '%%EndComments')
C
C Set (IPX1,IPY1) and (IPX2,IPY2) to the corner coordinates
C   of a viewport box obtained by shrinking the bounding box
C   by 12% in each dimension.
C
      IR = NINT(0.88*REAL(IR))
      IPX1 = 306 - IR
      IPX2 = 306 + IR
      IPY1 = 396 - IR
      IPY2 = 396 + IR
C
C Set the line thickness to 2 points, and draw the
C   viewport boundary.
C
      T = 2.0
      WRITE (LUN,110,ERR=13) T
      WRITE (LUN,120,ERR=13) IR
      WRITE (LUN,130,ERR=13)
  110 FORMAT (F12.6,' setlinewidth')
  120 FORMAT ('306 396 ',I3,' 0 360 arc')
  130 FORMAT ('stroke')
C
C Set up an affine mapping from the window box [-WR,WR] X
C   [-WR,WR] to the viewport box.
C
      SF = REAL(IR)/WR
      TX = IPX1 + SF*WR
      TY = IPY1 + SF*WR
      WRITE (LUN,140,ERR=13) TX, TY, SF, SF
  140 FORMAT (2F12.6,' translate'/
     .        2F12.6,' scale')
C
C The line thickness must be changed to reflect the new
C   scaling which is applied to all subsequent output.
C   Set it to 1.0 point.
C
      T = 1.0/SF
      WRITE (LUN,110,ERR=13) T
C
C Save the current graphics state, and set the clip path to
C   the boundary of the window.
C
      WRITE (LUN,150,ERR=13)
      WRITE (LUN,160,ERR=13) WR
      WRITE (LUN,170,ERR=13)
  150 FORMAT ('gsave')
  160 FORMAT ('0 0 ',F12.6,' 0 360 arc')
  170 FORMAT ('clip newpath')
C
C Compute the Cartesian coordinates of E and the components
C   of a rotation R which maps E to the north pole (0,0,1).
C   R is taken to be a rotation about the z-axis (into the
C   yz-plane) followed by a rotation about the x-axis chosen
C   so that the view-up direction is (0,0,1), or (-1,0,0) if
C   E is the north or south pole.
C
C           ( R11  R12  0   )
C       R = ( R21  R22  R23 )
C           ( EX   EY   EZ  )
C
      T = CF*ELON
      CT = COS(CF*ELAT)
      EX = CT*COS(T)
      EY = CT*SIN(T)
      EZ = SIN(CF*ELAT)
      IF (CT .NE. 0.0) THEN
        R11 = -EY/CT
        R12 = EX/CT
      ELSE
        R11 = 0.0
        R12 = 1.0
      ENDIF
      R21 = -EZ*R12
      R22 = EZ*R11
      R23 = CT
C
C Loop on nodes (Voronoi centers) N0.
C   LPL indexes the last neighbor of N0.
C
      DO 3 N0 = 1,N
        LPL = LEND(N0)
C
C Set KV2 to the first (and last) vertex index and compute
C   its coordinates (X2,Y2,Z2) in the rotated coordinate
C   system.
C
        KV2 = LISTC(LPL)
        X2 = R11*XC(KV2) + R12*YC(KV2)
        Y2 = R21*XC(KV2) + R22*YC(KV2) + R23*ZC(KV2)
        Z2 = EX*XC(KV2) + EY*YC(KV2) + EZ*ZC(KV2)
C
C   IN2 = TRUE iff KV2 is in the window.
C
        IN2 = Z2 .GE. 0.  .AND.  X2*X2 + Y2*Y2 .LE. WRS
C
C Loop on neighbors N1 of N0.  For each triangulation edge
C   N0-N1, KV1-KV2 is the corresponding Voronoi edge.
C
        LP = LPL
    1   LP = LPTR(LP)
          KV1 = KV2
          X1 = X2
          Y1 = Y2
          Z1 = Z2
          IN1 = IN2
          KV2 = LISTC(LP)
C
C   Compute the new values of (X2,Y2,Z2) and IN2.
C
          X2 = R11*XC(KV2) + R12*YC(KV2)
          Y2 = R21*XC(KV2) + R22*YC(KV2) + R23*ZC(KV2)
          Z2 = EX*XC(KV2) + EY*YC(KV2) + EZ*ZC(KV2)
          IN2 = Z2 .GE. 0.  .AND.  X2*X2 + Y2*Y2 .LE. WRS
C
C Add edge KV1-KV2 to the path iff both endpoints are inside
C   the window and KV2 > KV1, or KV1 is inside and KV2 is
C   outside (so that the edge is drawn only once).
C
          IF (.NOT. IN1  .OR.  (IN2  .AND.  KV2 .LE. KV1))
     .      GO TO 2
          IF (Z2 .LT. 0.) THEN
C
C   KV2 is a 'southern hemisphere' point.  Move it to the
C     intersection of edge KV1-KV2 with the equator so that
C     the edge is clipped properly.  Z2 is implicitly set
C     to 0.
C
            X2 = Z1*X2 - Z2*X1
            Y2 = Z1*Y2 - Z2*Y1
            T = SQRT(X2*X2+Y2*Y2)
            X2 = X2/T
            Y2 = Y2/T
          ENDIF
          WRITE (LUN,180,ERR=13) X1, Y1, X2, Y2
  180     FORMAT (2F12.6,' moveto',2F12.6,' lineto')
C
C Bottom of loops.
C
    2     IF (LP .NE. LPL) GO TO 1
    3   CONTINUE
C
C Paint the path and restore the saved graphics state (with
C   no clip path).
C
      WRITE (LUN,130,ERR=13)
      WRITE (LUN,190,ERR=13)
  190 FORMAT ('grestore')
      IF (NUMBR) THEN
C
C Nodes in the window are to be labeled with their indexes.
C   Convert FSIZN from points to world coordinates, and
C   output the commands to select a font and scale it.
C
        T = FSIZN/SF
        WRITE (LUN,200,ERR=13) T
  200   FORMAT ('/Helvetica findfont'/
     .          F12.6,' scalefont setfont')
C
C Loop on visible nodes N0 that project to points (X0,Y0) in
C   the window.
C
        DO 4 N0 = 1,N
          IF (EX*X(N0) + EY*Y(N0) + EZ*Z(N0) .LT. 0.)
     .      GO TO 4
          X0 = R11*X(N0) + R12*Y(N0)
          Y0 = R21*X(N0) + R22*Y(N0) + R23*Z(N0)
          IF (X0*X0 + Y0*Y0 .GT. WRS) GO TO 4
C
C   Move to (X0,Y0), and draw the label N0 with the origin
C     of the first character at (X0,Y0).
C
          WRITE (LUN,210,ERR=13) X0, Y0
C          WRITE (LUN,220,ERR=13) 
          WRITE (LUN,220,ERR=13) N0
  210     FORMAT (2F12.6,' moveto')
C  220     FORMAT ('(','.',') show')
  220     FORMAT ('(',I3,') show')
    4     CONTINUE
      ENDIF
C
C Convert FSIZT from points to world coordinates, and output
C   the commands to select a font and scale it.
C
      T = FSIZT/SF
      WRITE (LUN,200,ERR=13) T
C
C Display TITLE centered above the plot:
C
C      Y0 = WR + 3.0*T
C      WRITE (LUN,230,ERR=13) TITLE, Y0
C  230 FORMAT (A80/'  stringwidth pop 2 div neg ',F12.6,
C     .        ' moveto')
C      WRITE (LUN,240,ERR=13) TITLE
C  240 FORMAT (A80/'  show')
C      IF (ANNOT) THEN
C
C Display the window center and radius below the plot.
C
C        X0 = -WR
C        Y0 = -WR - 50.0/SF
C        WRITE (LUN,210,ERR=13) X0, Y0
C        WRITE (LUN,250,ERR=13) ELAT, ELON
C        Y0 = Y0 - 2.0*T
C        WRITE (LUN,210,ERR=13) X0, Y0
C        WRITE (LUN,260,ERR=13) A
C  250   FORMAT ('(Window center:  ELAT = ',F7.2,
C     .          ',  ELON = ',F8.2,') show')
C  260   FORMAT ('(Angular extent:  A = ',F5.2,') show')
C      ENDIF
C
C Paint the path and output the showpage command and
C   end-of-file indicator.
C
      WRITE (LUN,270,ERR=13)
  270 FORMAT ('stroke'/
     .        'showpage'/
     .        '%%EOF')
C
C HP's interpreters require a one-byte End-of-PostScript-Job
C   indicator (to eliminate a timeout error message):
C   ASCII 4.
C
      WRITE (LUN,280,ERR=13) CHAR(4)
  280 FORMAT (A1)
C
C No error encountered.
C
      IER = 0
      RETURN
C
C Invalid input parameter LUN, PLTSIZ, N, or NT.
C
   11 IER = 1
      RETURN
C
C Invalid input parameter ELAT, ELON, or A.
C
   12 IER = 2
      RETURN
C
C Error writing to unit LUN.
C
   13 IER = 3
      RETURN
      END
