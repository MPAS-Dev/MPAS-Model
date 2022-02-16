      	IMPLICIT REAL(A-H,O-Z)
      	INTEGER IER, IFLAG, K, KSUM, KT, LIN, LOUT, LP, LPL,
     .          LPLT, LPLV, LW, LWK, LNEW, N, N0, N1, N2, N3,
     .          NA, NB, NCOL, NMAX, NN, NROW, NT, NT6, NTMX, NV
      	INTEGER NEARND
      	LOGICAL INSIDE, NUMBR
      	REAL    A, AL, AREA, DEL, ELAT, ELON, P(3), PLTSIZ, SC
     .          VLAT, VLON, VNRM
        REAL    V1(3), V2(3), V3(3), V4(3), V5(3), V6(3) 
        REAL    SNW(3), ANGLES(3)

      	PARAMETER (NMAX=1000000, NTMX=2*NMAX, NT6=6*NMAX, LWK=2*NMAX,
     .             NCOL=NMAX, NROW=16)

C  Array storage for the triangulation, work space, and nodal
C  coordinates.

      	INTEGER LIST(NT6), LPTR(NT6), LEND(NMAX), IWK(LWK),
     .          NEIGH(NMAX,20), NEISZ(NMAX), NTRI(6*NMAX,3), 
     .		MARK(NMAX)
      	REAL    DS(NMAX), RLAT(NMAX), RLON(NMAX), HV(NMAX), AV(NMAX)
      	REAL    X(NMAX), Y(NMAX), Z(NMAX), X1(NMAX), Y1(NMAX), Z1(NMAX)

C  Array storage for the Voronoi diagram:  adjacency array,
C  boundary triangle list, triangle circumcenters, and
C  circumradii.

      	INTEGER LISTC(NT6), LBTRI(6,NCOL)
        INTEGER VORTX(NMAX,20), VORSZ(NMAX)
      	REAL    XC(NTMX), YC(NTMX), ZC(NTMX), RC(NTMX)

C  Array storage for the triangle list.

      	INTEGER LTRI(NROW,NTMX)
      	INTEGER I1MACH(2)
      
