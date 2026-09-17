! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!
!=================================================================================================================
 module mpas_atmphys_functions

 use mpas_kind_types, only : RKIND
 use mpas_derived_types, only : MPAS_LOG_ERR
 use mpas_log, only : mpas_log_write

 implicit none
 private
 public:: gammp,wgamma,rslf,rsif


 contains

!=================================================================================================================
!NOTE: functions rslf and rsif are taken from module_mp_thompson temporarily for computing
!      the diagnostic relative humidity. These two functions will be removed from this module
!      when the Thompson cloud microphysics scheme will be restored to MPAS-Dev.
!      Laura D. Fowler (birch.mmm.ucar.edu) / 2013-07-11.

!+---+-----------------------------------------------------------------+
      REAL(KIND=RKIND) FUNCTION GAMMP(A,X)
!     --- COMPUTES THE REGULARIZED LOWER INCOMPLETE GAMMA FUNCTION P(A,X)
!     --- USES GAMIT
      IMPLICIT NONE
      REAL(KIND=RKIND), INTENT(IN):: A,X
      GAMMP = 0.0_RKIND
      IF((X.LT.0.) .OR. (A.LE.0.)) THEN
        CALL MPAS_LOG_WRITE('BAD ARGUMENTS IN GAMMP', MESSAGETYPE=MPAS_LOG_ERR)
        RETURN
      ENDIF
      IF(X.EQ.0.0_RKIND) RETURN
      GAMMP = X**A * GAMIT(A,X)
      END FUNCTION GAMMP

!+---+-----------------------------------------------------------------+
      REAL(KIND=RKIND) FUNCTION GAMIT(A,X)
!     --- COMPUTES TRICOMI'S INCOMPLETE GAMMA FUNCTION
      IMPLICIT NONE
      REAL(KIND=RKIND), INTENT(IN):: A,X

      REAL(KIND=RKIND):: AEPS, AINTA,ALGAP1,ALNEPS,ALNG,ALX,BOT,H,SGA,SGNGAM,T

      ALNEPS = -LOG(EPSILON(1.0_RKIND)/RADIX(1.0_RKIND))
      BOT = LOG(TINY(1.0_RKIND))

      gamit = 0.0_RKIND
      IF (X.LT.0.0_RKIND) THEN
            CALL MPAS_LOG_WRITE('BAD ARGUMENTS IN GAMIT', MESSAGETYPE=MPAS_LOG_ERR)
            RETURN
      ENDIF

      ALX = 0.0_RKIND
      IF (X.NE.0.0_RKIND) ALX = LOG(X)
      SGA = 1.0_RKIND
      IF (A.NE.0.0_RKIND) SGA = SIGN (1.0_RKIND, A)
      AINTA = AINT (A + 0.5_RKIND*SGA)
      AEPS = A - AINTA

      IF (X.EQ.0.0_RKIND) THEN
            IF (AINTA.GT.0.0_RKIND .OR. AEPS.NE.0.0_RKIND) &
                  GAMIT = 1.0_RKIND/GAMMA(A+1.0_RKIND)

      ELSEIF (X.LE.1.0_RKIND) THEN
            ALGAP1 = 0.0_RKIND
            SGNGAM = 1.0_RKIND
            IF (A.GE.(-0.5_RKIND) .OR. AEPS.NE.0.0_RKIND) &
                  ALGAP1 = LOG_GAMMA(A+1.0_RKIND)
            GAMIT = G9GMIT(A,X,ALGAP1,SGNGAM,ALX)
      
      ELSEIF (A.GE.X) THEN
            T = G9LGIT(A,X,LOG_GAMMA(A+1.0_RKIND))
            GAMIT = EXP(T)
      
      ELSE
!     --- EVALUATE GAMIT IN TERMS OF LOG OF THE COMPLEMENTARY INCOMPLETE GAMMA FUNCTION
            ALNG = G9LGIC(A,X,ALX)

            H = 1.0_RKIND
            IF (AEPS.NE.0.0_RKIND .OR. AINTA.GT.0.0_RKIND) THEN
                  ALGAP1 = LOG_GAMMA(A+1.0_RKIND)
                  SGNGAM = 1.0_RKIND
                  T = LOG(ABS(A)) + ALNG - ALGAP1
                  IF (T.GT.ALNEPS) THEN
                        T = T - A*ALX
                        GAMIT = -SGA * SGNGAM * EXP(T)
                        RETURN
                  ENDIF
                  IF (T.GT.(-ALNEPS)) H = 1.0_RKIND - SGA*SGNGAM*EXP(T)
            ENDIF

            T = -A*ALX + LOG(ABS(H))
            GAMIT = SIGN(EXP(T), H)
      END IF
      END FUNCTION GAMIT

!+---+-----------------------------------------------------------------+
! Tricomi's incomplete gamma function for small X (Taylor series).
!+---+-----------------------------------------------------------------+
      REAL(KIND=RKIND) FUNCTION G9GMIT(A,X,ALGAP1,SGNGAM,ALX)
      IMPLICIT NONE
      REAL(KIND=RKIND), INTENT(IN):: A,X,ALGAP1,SGNGAM,ALX

      INTEGER:: K,M,MA
      REAL(KIND=RKIND)::AE,AEPS,ALGS,ALG2,BOT,GEPS,FK,S,SGNG2,T,TE

      GEPS = 0.5_RKIND*EPSILON(1.0_RKIND)/RADIX(1.0_RKIND)
      BOT = LOG(TINY(1.0_RKIND))

      G9GMIT = 0.0_RKIND
      IF (X.LE.0.0_RKIND) THEN
            CALL MPAS_LOG_WRITE('BAD ARGUMENTS IN G9GMIT', MESSAGETYPE=MPAS_LOG_ERR)
            RETURN
      ENDIF

      MA = INT(A+0.5_RKIND)
      IF (A.LT.0.0_RKIND) MA = INT(A - 0.5_RKIND)
      AEPS = A - MA

      AE = A
      IF ( A.LT.(-0.5_RKIND)) AE = AEPS

      T = 1.0_RKIND
      TE = AE
      S = T
      DO K = 1, 200
            FK = REAL(K,RKIND)
            TE = -X*TE/FK
            T = TE/(AE+FK)
            S = S + T 
            IF (ABS(T).LT.GEPS*ABS(S)) EXIT
            IF (K.EQ.200) CALL MPAS_LOG_WRITE( &
                  'NO CONVERGENCE IN 200 TERMS OF TAYLOR SERIES IN G9GMIT', &
                  MESSAGETYPE=MPAS_LOG_ERR)
      ENDDO

      IF (A.GE.(-0.5_RKIND)) THEN
            ALGS = -ALGAP1 + LOG(S)
            G9GMIT = EXP(ALGS)
            RETURN
      ENDIF
      
      ALGS = -LOG_GAMMA(1.0_RKIND+AEPS) + LOG(S)
      S = 1.0_RKIND
      M = -MA - 1
      IF (M.NE.0) THEN
            T = 1.0_RKIND
            DO K = 1, M
                  T = X*T/(AEPS-(M+1-K))
                  S = S + T
                  IF (ABS(T).LT.GEPS*ABS(S)) EXIT
            ENDDO
      ENDIF

      G9GMIT = 0.0_RKIND
      ALGS = -MA*LOG(X) + ALGS
      IF (S.EQ.0.0_RKIND .OR. AEPS.EQ.0.0_RKIND) THEN
            G9GMIT = EXP(ALGS)
            RETURN
      ENDIF

      SGNG2 = SGNGAM * SIGN(1.0_RKIND,S)
      ALG2 = -X-ALGAP1+LOG(ABS(S))

      IF (ALG2.GT.BOT) G9GMIT=SGNG2*EXP(ALG2)
      IF (ALGS.GT.BOT) G9GMIT = G9GMIT+EXP(ALGS)

      END FUNCTION G9GMIT

!+---+-----------------------------------------------------------------+
! Log of Tricomi's incomplete gamma function evaluated with Perron's 
! continued fraction
!+---+-----------------------------------------------------------------+
      REAL(KIND=RKIND) FUNCTION G9LGIT(A,X,ALGAP1)
      IMPLICIT NONE
      REAL(KIND=RKIND), INTENT(IN)::A,X,ALGAP1

      INTEGER:: K 
      REAL(KIND=RKIND)::AX,A1X,GEPS,FK,HSTAR,P,R,S,T 

      GEPS = 0.5_RKIND*EPSILON(1.0_RKIND)/RADIX(1.0_RKIND)

      G9LGIT = 0.0_RKIND
      IF (X.LE.0.0_RKIND .OR. A.LT.X) THEN
            CALL MPAS_LOG_WRITE('X SHOULD BE GT 0.0 AND LE A IN G9LGIT', &
                  MESSAGETYPE=MPAS_LOG_ERR)
            RETURN
      ENDIF

      AX = A + X
      A1X = AX + 1.0_RKIND
      R = 0.0_RKIND
      P = 1.0_RKIND
      S = P 
      DO K = 1,200
            FK = REAL(K,RKIND)
            T = (A+FK)*X*(1.0_RKIND+R)
            R = T/((AX+FK)*(A1X+FK)-T)
            P = R*P
            S = S + P
            IF (ABS(P).LT.GEPS*S) EXIT
            IF (K.EQ.200) CALL MPAS_LOG_WRITE( &
                  'NO CONVERGENCE IN 200 TERMS OF CONTINUED FRACTION IN G9LGIT', &
                  MESSAGETYPE=MPAS_LOG_ERR)
      ENDDO

      HSTAR = 1.0_RKIND - X*S/A1X
      G9LGIT = -X - ALGAP1 - LOG(HSTAR)

      END FUNCTION G9LGIT

!+---+-----------------------------------------------------------------+      
! Log of the complementary incomplete gamma function for large X and
! A <= X
!+---+-----------------------------------------------------------------+      
      REAL (KIND=RKIND) FUNCTION G9LGIC(A,X,ALX)
      IMPLICIT NONE
      REAL(KIND=RKIND), INTENT(IN):: A,X,ALX

      INTEGER::K
      REAL(KIND=RKIND):: GEPS,FK,P,R,S,T,XMA,XPA

      GEPS = 0.5_RKIND*EPSILON(1.0_RKIND)/RADIX(1.0_RKIND)

      XPA = X + 1.0_RKIND - A
      XMA = X - 1.0_RKIND - A

      R = 0.0_RKIND
      P = 1.0_RKIND
      S = P
      DO K = 1,300
            FK = REAL(K,RKIND)
            T = FK*(A-FK)*(1.0_RKIND+R)
            R = -T/((XMA+2.0_RKIND*FK)*(XPA+2.0_RKIND*FK)+T)
            P = R*P 
            S = S + P
            IF (ABS(P).LT.GEPS*S) EXIT
            IF (K.EQ.300) CALL MPAS_LOG_WRITE( &
                  'NO CONVERGENCE IN 300 TERMS OF CONTINUED FRACTION IN G9LGIC', &
                  MESSAGETYPE=MPAS_LOG_ERR)
      ENDDO

      G9LGIC = A*ALX - X + LOG(S/XPA)

      END FUNCTION G9LGIC
      
!+---+-----------------------------------------------------------------+            
      REAL(KIND=RKIND) FUNCTION WGAMMA(y)

      IMPLICIT NONE
      REAL(KIND=RKIND), INTENT(IN):: y

      WGAMMA = EXP(LOG_GAMMA(y))

      END FUNCTION WGAMMA

!+---+-----------------------------------------------------------------+
! THIS FUNCTION CALCULATES THE LIQUID SATURATION VAPOR MIXING RATIO AS
! A FUNCTION OF TEMPERATURE AND PRESSURE
!
      REAL(KIND=RKIND) FUNCTION RSLF(P,T)

      IMPLICIT NONE
      REAL(KIND=RKIND), INTENT(IN):: P, T
      REAL(KIND=RKIND):: ESL,X
      REAL(KIND=RKIND), PARAMETER:: C0= .611583699E03
      REAL(KIND=RKIND), PARAMETER:: C1= .444606896E02
      REAL(KIND=RKIND), PARAMETER:: C2= .143177157E01
      REAL(KIND=RKIND), PARAMETER:: C3= .264224321E-1
      REAL(KIND=RKIND), PARAMETER:: C4= .299291081E-3
      REAL(KIND=RKIND), PARAMETER:: C5= .203154182E-5
      REAL(KIND=RKIND), PARAMETER:: C6= .702620698E-8
      REAL(KIND=RKIND), PARAMETER:: C7= .379534310E-11
      REAL(KIND=RKIND), PARAMETER:: C8=-.321582393E-13

      X=MAX(-80.,T-273.16)

!     ESL=612.2*EXP(17.67*X/(T-29.65))
      ESL=C0+X*(C1+X*(C2+X*(C3+X*(C4+X*(C5+X*(C6+X*(C7+X*C8)))))))
      ESL=MIN(ESL, P*0.15)  ! Even with P=1050mb and T=55C, the sat. vap. pres only contributes to
      RSLF=.622*ESL/(P-ESL) ! ~15% of total pres.

!    ALTERNATIVE
!  ; Source: Murphy and Koop, Review of the vapour pressure of ice and
!             supercooled water for atmospheric applications, Q. J. R.
!             Meteorol. Soc (2005), 131, pp. 1539-1565.
!    ESL = EXP(54.842763 - 6763.22 / T - 4.210 * ALOG(T) + 0.000367 * T
!        + TANH(0.0415 * (T - 218.8)) * (53.878 - 1331.22
!        / T - 9.44523 * ALOG(T) + 0.014025 * T))

      END FUNCTION RSLF
!+---+-----------------------------------------------------------------+
! THIS FUNCTION CALCULATES THE ICE SATURATION VAPOR MIXING RATIO AS A
! FUNCTION OF TEMPERATURE AND PRESSURE
!
      REAL(KIND=RKIND) FUNCTION RSIF(P,T)

      IMPLICIT NONE
      REAL(KIND=RKIND), INTENT(IN):: P, T
      REAL(KIND=RKIND):: ESI,X
      REAL(KIND=RKIND), PARAMETER:: C0= .609868993E03
      REAL(KIND=RKIND), PARAMETER:: C1= .499320233E02
      REAL(KIND=RKIND), PARAMETER:: C2= .184672631E01
      REAL(KIND=RKIND), PARAMETER:: C3= .402737184E-1
      REAL(KIND=RKIND), PARAMETER:: C4= .565392987E-3
      REAL(KIND=RKIND), PARAMETER:: C5= .521693933E-5
      REAL(KIND=RKIND), PARAMETER:: C6= .307839583E-7
      REAL(KIND=RKIND), PARAMETER:: C7= .105785160E-9
      REAL(KIND=RKIND), PARAMETER:: C8= .161444444E-12

      X=MAX(-80.,T-273.16)
      ESI=C0+X*(C1+X*(C2+X*(C3+X*(C4+X*(C5+X*(C6+X*(C7+X*C8)))))))
      ESI=MIN(ESI, P*0.15)
      RSIF=.622*ESI/(P-ESI)

!    ALTERNATIVE
!  ; Source: Murphy and Koop, Review of the vapour pressure of ice and
!             supercooled water for atmospheric applications, Q. J. R.
!             Meteorol. Soc (2005), 131, pp. 1539-1565.
!     ESI = EXP(9.550426 - 5723.265/T + 3.53068*ALOG(T) - 0.00728332*T)

      END FUNCTION RSIF

!=================================================================================================================
 end module mpas_atmphys_functions
!=================================================================================================================