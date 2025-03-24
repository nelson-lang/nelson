      SUBROUTINE SG03BX( DICO, TRANS, A, LDA, E, LDE, B, LDB, U, LDU,
     $                   SCALE, M1, LDM1, M2, LDM2, INFO )
C
C     PURPOSE
C
C     To solve for X = op(U)**T * op(U) either the generalized c-stable
C     continuous-time Lyapunov equation
C
C             T                    T
C        op(A)  * X * op(E) + op(E)  * X * op(A)
C
C                 2        T
C        = - SCALE  * op(B)  * op(B),                                (1)
C
C     or the generalized d-stable discrete-time Lyapunov equation
C
C             T                    T
C        op(A)  * X * op(A) - op(E)  * X * op(E)
C
C                 2        T
C        = - SCALE  * op(B)  * op(B),                                (2)
C
C     where op(K) is either K or K**T for K = A, B, E, U. The Cholesky
C     factor U of the solution is computed without first finding X.
C
C     Furthermore, the auxiliary matrices
C
C                                   -1        -1
C        M1 := op(U) * op(A) * op(E)   * op(U)
C
C                           -1        -1
C        M2 := op(B) * op(E)   * op(U)
C
C     are computed in a numerically reliable way.
C
C     The matrices A, B, E, M1, M2, and U are real 2-by-2 matrices. The
C     pencil A - lambda * E must have a pair of complex conjugate
C     eigenvalues. The eigenvalues must be in the open right half plane
C     (in the continuous-time case) or inside the unit circle (in the
C     discrete-time case). The matrices E and B are upper triangular.
C
C     The resulting matrix U is upper triangular. The entries on its
C     main diagonal are non-negative. SCALE is an output scale factor
C     set to avoid overflow in U.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     DICO    CHARACTER*1
C             Specifies whether the continuous-time or the discrete-time
C             equation is to be solved:
C             = 'C':  Solve continuous-time equation (1);
C             = 'D':  Solve discrete-time equation (2).
C
C     TRANS   CHARACTER*1
C             Specifies whether the transposed equation is to be solved
C             or not:
C             = 'N':  op(K) = K,     K = A, B, E, U;
C             = 'T':  op(K) = K**T,  K = A, B, E, U.
C
C     Input/Output Parameters
C
C     A       (input) DOUBLE PRECISION array, dimension (LDA,2)
C             The leading 2-by-2 part of this array must contain the
C             matrix A.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= 2.
C
C     E       (input) DOUBLE PRECISION array, dimension (LDE,2)
C             The leading 2-by-2 upper triangular part of this array
C             must contain the matrix E.
C
C     LDE     INTEGER
C             The leading dimension of the array E.  LDE >= 2.
C
C     B       (input) DOUBLE PRECISION array, dimension (LDB,2)
C             The leading 2-by-2 upper triangular part of this array
C             must contain the matrix B.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= 2.
C
C     U       (output) DOUBLE PRECISION array, dimension (LDU,2)
C             The leading 2-by-2 part of this array contains the upper
C             triangular matrix U.
C
C     LDU     INTEGER
C             The leading dimension of the array U.  LDU >= 2.
C
C     SCALE   (output) DOUBLE PRECISION
C             The scale factor set to avoid overflow in U.
C             0 < SCALE <= 1.
C
C     M1      (output) DOUBLE PRECISION array, dimension (LDM1,2)
C             The leading 2-by-2 part of this array contains the
C             matrix M1.
C
C     LDM1    INTEGER
C             The leading dimension of the array M1.  LDM1 >= 2.
C
C     M2      (output) DOUBLE PRECISION array, dimension (LDM2,2)
C             The leading 2-by-2 part of this array contains the
C             matrix M2.
C
C     LDM2    INTEGER
C             The leading dimension of the array M2.  LDM2 >= 2.
C
C     Error indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             = 2:  the eigenvalues of the pencil A - lambda * E are not
C                   a pair of complex conjugate numbers;
C             = 3:  the eigenvalues of the pencil A - lambda * E are
C                   not in the open right half plane (in the continuous-
C                   time case) or inside the unit circle (in the
C                   discrete-time case);
C             = 4:  the LAPACK routine ZSTEIN utilized to factorize M3
C                   (see SLICOT Library routine SG03BS) failed to
C                   converge. This error is unlikely to occur.
C
C     METHOD
C
C     The method used by the routine is based on a generalization of the
C     method due to Hammarling ([1], section 6) for Lyapunov equations
C     of order 2. A more detailed description is given in [2].
C
C     REFERENCES
C
C     [1] Hammarling, S.J.
C         Numerical solution of the stable, non-negative definite
C         Lyapunov equation.
C         IMA J. Num. Anal., 2, pp. 303-323, 1982.
C
C     [2] Penzl, T.
C         Numerical solution of generalized Lyapunov equations.
C         Advances in Comp. Math., vol. 8, pp. 33-48, 1998.
C
C     FURTHER COMMENTS
C
C     If the solution matrix U is singular, the matrices M1 and M2 are
C     properly set (see [1], equation (6.21)).
C
C     CONTRIBUTOR
C
C     T. Penzl, Technical University Chemnitz, Germany, Aug. 1998.
C     V. Sima, substantial changes, Dec. 2021, Jan. 2022.
C
C     REVISIONS
C
C     Sep. 1998, Dec. 1998.
C     July 2003 (V. Sima; suggested by Klaus Schnepper).
C     Oct. 2003 (A. Varga).
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ONE, TWO, ZERO, SAFETY
      PARAMETER         ( ONE = 1.0D+0, TWO = 2.0D+0, ZERO = 0.0D+0,
     $                    SAFETY = 1.0D+2 )
C     .. Scalar Arguments ..
      CHARACTER         DICO, TRANS
      DOUBLE PRECISION  SCALE
      INTEGER           INFO, LDA, LDB, LDE, LDM1, LDM2, LDU
C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), B(LDB,*), E(LDE,*), M1(LDM1,*),
     $                  M2(LDM2,*), U(LDU,*)
C     .. Local Scalars ..
      COMPLEX*16        X, ZS
      DOUBLE PRECISION  A11, A12, A21, A22, AI11, AI12, AI21, AI22,
     $                  ALPHA, AR11, AR12, AR21, AR22, B11, B12I, B12R,
     $                  BETAI, BETAR, BI11, BI12, BI21, BI22, BIGNUM,
     $                  BR11, BR12, BR21, BR22, C, CL, CQ, CQB, CQBI,
     $                  CQU, CQUI, CZ, E11, E12, E22, EI12, EI21, ER11,
     $                  ER12, ER22, EPS, LAMI, LAMR, LI, LR, M1I12,
     $                  M1R12, M2I12, M2R12, M2R22, M2S, MI, MR, MX, P,
     $                  S, SCALE1, SCALE2, SI, SIQ, SIQB, SIQU, SIZ, SL,
     $                  SMLNUM, SQTWO, SR, SRQ, SRQB, SRQU, SRZ, T, TMP,
     $                  UI12, UI22, UR11, UR12, UR22, V, VI12, VR12,
     $                  VR22, W, XR, XI, YR, YI
      INTEGER           CT
      LOGICAL           ISCONT, ISTRNS
C     .. Local Arrays ..
      COMPLEX*16        M3(1), M3C(2,1)
      DOUBLE PRECISION  AS(2,2), D(2), DWORK(10), ES(2,2), ET(2), EV(2)
      INTEGER           IWORK(7)
C     .. External Functions ..
      DOUBLE PRECISION  DLAMCH, DLAPY2, DLAPY3
      LOGICAL           LSAME
      EXTERNAL          DLAMCH, DLAPY2, DLAPY3, LSAME
C     .. External Subroutines ..
      EXTERNAL          DLABAD, DLADIV, DLAG2, DLASV2, SG03BR, ZLARFG,
     $                  ZSTEIN
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, DBLE, DCMPLX, DIMAG, MAX, MIN, SQRT
C
C     Decode input parameters.
C
      ISTRNS = LSAME( TRANS, 'T' )
      ISCONT = LSAME( DICO,  'C' )
C
C     Do not check input parameters for errors.
C
C     Set constants to control overflow.
C
      SQTWO  = SQRT( TWO )
      EPS    = DLAMCH( 'P' )
      SMLNUM = DLAMCH( 'S' )/EPS
      BIGNUM = ONE/SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
C
C     Set constant input for ZSTEIN.
C
      IWORK(2) = 1
      IWORK(3) = 0
      IWORK(4) = 2
      IWORK(5) = 0
      EV(1)    = ONE
      EV(2)    = ZERO
C
      INFO  = 0
      SCALE = ONE
C
C     Make copies of A, E, and B.
C
      AS(1,1) = A(1,1)
      AS(2,1) = A(2,1)
      AS(1,2) = A(1,2)
      AS(2,2) = A(2,2)
      ES(1,1) = E(1,1)
      ES(2,1) = ZERO
      ES(1,2) = E(1,2)
      ES(2,2) = E(2,2)
      BR11    = B(1,1)
      BR12    = B(1,2)
      BR22    = B(2,2)
C
C     If the transposed equation (op(K)=K**T, K=A,B,E,U) is to be
C     solved, transpose the matrices A, E, B with respect to the
C     anti-diagonal. This results in a non-transposed equation.
C
      IF ( ISTRNS ) THEN
         V       = AS(1,1)
         AS(1,1) = AS(2,2)
         AS(2,2) = V
         V       = ES(1,1)
         ES(1,1) = ES(2,2)
         ES(2,2) = V
         V       = BR11
         BR11    = BR22
         BR22    = V
      END IF
C
C     Perform QZ-step to transform the pencil A - lambda * E to complex
C     generalized Schur form. The main diagonal of the Schur factor of E
C     is real and positive.
C
C     Compute eigenvalues (LAMR + LAMI * I, LAMR - LAMI * I).
C
      CT = 0
   10 CONTINUE
      CT = CT + 1
      P  = MAX( EPS*MAX( ABS( ES(1,1) ), ABS( ES(1,2) ),
     $                   ABS( ES(2,2) ) ), SMLNUM )
      IF ( MIN( ABS( ES(1,1) ), ABS( ES(2,2) ) ).LT.P ) THEN
         INFO = 2
         RETURN
      END IF
      CALL DLAG2( AS, 2, ES, 2, SMLNUM*EPS*SAFETY, SCALE1, SCALE2, LAMR,
     $            W, LAMI )
      IF ( LAMI.LE.ZERO ) THEN
         INFO = 2
         RETURN
      END IF
C
      IF ( ES(1,2).NE.ZERO ) THEN
C
C        Standardize, that is, rotate so that ES is diagonal with
C        ES(1,1) non-negative.
C
         CALL DLASV2( ES(1,1), ES(1,2), ES(2,2), E22, E11, SR, C, SL,
     $                CL )
C
         IF ( E11.LT.ZERO ) THEN
            C   = -C
            SR  = -SR
            E11 = -E11
            E22 = -E22
         END IF
C
C        Update A using the left and right rotations.
C
         S = CL*AS(1,1) + SL*AS(2,1)
         T = CL*AS(1,2) + SL*AS(2,2)
         V = CL*AS(2,1) - SL*AS(1,1)
         W = CL*AS(2,2) - SL*AS(1,2)
C
         AS(1,1) = S*C + T*SR
         AS(2,1) = V*C + W*SR
         AS(1,2) = T*C - S*SR
         AS(2,1) = W*C - V*SR
C
         ES(1,1) = E11
         ES(2,1) = ZERO
         ES(1,2) = ZERO
C
C        If E22 is negative, negate the second columns.
C
         IF ( E22.LT.ZERO ) THEN
            ES(2,2) = -E22
            AS(1,2) = -AS(1,2)
            AS(2,2) = -AS(2,2)
         ELSE
            ES(2,2) =  E22
         END IF
C
C        Recompute the shift.
C
         CALL DLAG2( AS, 2, ES, 2, SMLNUM*EPS*SAFETY, SCALE1, SCALE2,
     $               LAMR, W, LAMI )
C
C        If standardization has perturbed the shift onto real line,
C        do another (real single-shift) QR step.
C
         IF ( LAMI.EQ.ZERO ) THEN
            IF ( CT.EQ.1 ) THEN
               GO TO 10
            ELSE
               INFO = 2
               RETURN
            END IF
         END IF
      END IF
C
C     Compute left unitary transformation matrix Q.
C
      A11 = AS(1,1)
      A21 = AS(2,1)
      A12 = AS(1,2)
      A22 = AS(2,2)
      E11 = ES(1,1)
      E22 = ES(2,2)
      CALL SG03BR( SCALE1*A11 - E11*LAMR, -E11*LAMI, SCALE1*A21, ZERO,
     $             CQ, SRQ, SIQ, LR, LI )
C
C     A := Q * A.
C
      AR11 =  CQ*A11 + SRQ*A21
      AR21 =  CQ*A21 - SRQ*A11
      AR12 =  CQ*A12 + SRQ*A22
      AR22 =  CQ*A22 - SRQ*A12
      AI11 = SIQ*A21
      AI21 = SIQ*A11
      AI12 = SIQ*A22
      AI22 = SIQ*A12
C
C     E := Q * E.
C
      EI21 = SIQ*E11
      EI12 = SIQ*E22
      TMP  = SRQ*E11
      E11  =  CQ*E11
      E12  = SRQ*E22
C
C     Compute right unitary transformation matrix Z.
C
      CALL SG03BR( CQ*E22, ZERO, TMP, -EI21, CZ, SRZ, SIZ, LR, LI )
C
C     E := E * Z**H.
C
      ER11 =  E11*CZ + E12*SRZ + EI12*SIZ
      ER12 =  E12*CZ - E11*SRZ
      EI12 = EI12*CZ - E11*SIZ
      ER22 = LR
C
C     The structure of the matrices A, E, Q, and Z ensures that the
C     diagonal elements are real and E(2,2) > 0. Make E(1,1) > 0.
C
      IF ( ER11.LT.ZERO )
     $   ER11 = -ER11
C
C     A := A * Z**H.
C
      A11  = AR11
      A12  = AR12
      TMP  = AI11
      AR11 =  A12*SRZ +  A11*CZ  + AI12*SIZ
      AI11 = AI12*SRZ +  TMP*CZ  -  A12*SIZ
      AR12 =  A12*CZ  +  TMP*SIZ -  A11*SRZ
      AI12 = AI12*CZ  -  A11*SIZ -  TMP*SRZ
      AR22 = AR22*CZ  + AI21*SIZ - AR21*SRZ
      AI22 = AI22*CZ  - AR21*SIZ - AI21*SRZ
C
C     End of QZ-step.
C
C     B := B * Z**H.
C
      B11  =  BR11
      BI11 = -BR12*SIZ
      BI21 = -BR22*SIZ
      BI12 =  -B11*SIZ
      BR11 =  BR12*SRZ + B11*CZ
      BR21 =  BR22*SRZ
      BR12 =  BR12*CZ  - B11*SRZ
      BR22 =  BR22*CZ
C
C     Overwrite B with the upper triangular matrix of its
C     QR-factorization. The elements on the main diagonal are real
C     and non-negative.
C
      CALL SG03BR( BR11, BI11, BR21, BI21, CQB, SRQB, SIQB, LR, LI )
      V    = BR12
      T    = BI12
      BR12 = SRQB*BR22 +  CQB*V
      BI12 = SIQB*BR22 +  CQB*T
      BR22 =  CQB*BR22 - SRQB*V - SIQB*T
      BI22 =             SIQB*V - SRQB*T
      BR11 = LR
      BI11 = LI
C
      IF ( LI.NE.ZERO ) THEN
         V = DLAPY2( BR11, BI11 )
         CALL DLADIV( V, ZERO, BR11, BI11, XR, XI )
         BR11 = V
         T    = XR*BR12 - XI*BI12
         BI12 = XR*BI12 + XI*BR12
         BR12 = T
C
         CQBI = XI*CQB
         CQB  = XR*CQB
         T    = XR*SRQB - XI*SIQB
         SIQB = XR*SIQB + XI*SRQB
         SRQB = T
      END IF
C
      IF ( BI22.NE.ZERO ) THEN
         V = DLAPY2( BR22, BI22 )
         IF ( V.GE.MAX( EPS*MAX( BR11, DLAPY2( BR12, BI12 ) ), SMLNUM )
     $         ) THEN
            CALL DLADIV( V, ZERO, BR22, BI22, XR, XI )
            BR22 = V
         ELSE
            BR22 = ZERO
         END IF
      ELSE IF ( BR22.LT.ZERO ) THEN
         BR22 = -BR22
      END IF
C
C     Compute the Cholesky factor of the solution of the reduced
C     equation. The solution may be scaled to avoid overflow.
C
      IF ( ISCONT ) THEN
C
C        Continuous-time equation.
C
C        Step I:  Compute U(1,1). U(2,1) is 0.
C
         V = -AR11
         IF ( V.LE.ZERO ) THEN
            INFO = 3
            RETURN
         END IF
         V = SQRT( V )*SQRT( ER11 )
         T = ( BR11*SMLNUM )/SQTWO
         IF ( T.GT.V ) THEN
            SCALE1 = V/T
            SCALE  = SCALE1*SCALE
            BR11   = SCALE1*BR11
            BR12   = SCALE1*BR12
            BI12   = SCALE1*BI12
            BR22   = SCALE1*BR22
         END IF
         V    = V*SQTWO
         UR11 = BR11/V
C
C        Step II:  Compute U(1,2).
C
         MX = MAX( ABS( AR11 ), ABS( AI11 ), V )
         IF ( ER11.GT.MX*SMLNUM ) THEN
            MR  =  AR11/ER11
            MI  = -AI11/ER11
            M2S =     V/ER11
            XR  =  M2S*BR12
            XI  =  M2S*BI12
            IF ( UR11.NE.ZERO ) THEN
               XR = XR + UR11*( AR12 + MR*ER12 - MI*EI12 )
               XI = XI + UR11*( AI12 + MR*EI12 + MI*ER12 )
            END IF
            YR = AR22 + MR*ER22
            YI = AI22 + MI*ER22
         ELSE
            XR = BR12*V
            XI = BI12*V
            IF ( UR11.NE.ZERO ) THEN
               XR = XR + UR11*( ER11*AR12 + AR11*ER12 + AI11*EI12 )
               XI = XI + UR11*( ER11*AI12 + AR11*EI12 - AI11*ER12 )
            END IF
            YR = ER11*AR22 + AR11*ER22
            YI = ER11*AI22 - AI11*ER22
         END IF
         T = DLAPY2( XR, XI )*SMLNUM
         W = DLAPY2( YR, YI )
         IF ( T.GT.W ) THEN
            SCALE1 = W/T
            SCALE  = SCALE1*SCALE
            BR11   = SCALE1*BR11
            BR12   = SCALE1*BR12
            BR22   = SCALE1*BR22
            BI12   = SCALE1*BI12
            UR11   = SCALE1*UR11
            XR     = SCALE1*XR
            XI     = SCALE1*XI
         END IF
         CALL DLADIV( XR, XI, -YR, -YI, UR12, UI12 )
C
C        Step III:  Compute U(2,2).
C
         VR12 = UR11*ER12 + UR12*ER22
         VI12 = UR11*EI12 + UI12*ER22
         IF ( ER11.GT.MX*SMLNUM ) THEN
            YR = BR12 - M2S*VR12
            YI = BI12 - M2S*VI12
         ELSE
            XR =  VR12*V
            XI = -VI12*V
            T  = DLAPY2( XR, XI )*SMLNUM
            IF ( T.GT.ER11 ) THEN
               SCALE1 = ER11/T
               SCALE  = SCALE1*SCALE
               BR11   = SCALE1*BR11
               BR12   = SCALE1*BR12
               BI12   = SCALE1*BI12
               BR22   = SCALE1*BR22
               UR11   = SCALE1*UR11
               UR12   = SCALE1*UR12
               UI12   = SCALE1*UI12
               XR     = SCALE1*XR
               XI     = SCALE1*XI
            END IF
            YR =  BR12 - XR/ER11
            YI = -BI12 - XI/ER11
         END IF
         CALL SG03BR( BR22, ZERO, YR, YI, C, SR, SI, LR, LI )
         V = -AR22
         IF ( V.LE.ZERO ) THEN
            INFO = 3
            RETURN
         END IF
         V = SQRT( V )*SQRT( ER22 )
         T = ( LR*SMLNUM )/SQTWO
         IF ( T.GT.V ) THEN
            SCALE1 = V/T
            SCALE  = SCALE1*SCALE
            BR11   = SCALE1*BR11
            BR12   = SCALE1*BR12
            BR22   = SCALE1*BR22
            BI12   = SCALE1*BI12
            UR11   = SCALE1*UR11
            UR12   = SCALE1*UR12
            UI12   = SCALE1*UI12
            LR     = SCALE1*LR
         END IF
         V    = V*SQTWO
         UR22 = LR/V
C
C        Compute the needed elements of the matrices M1 and M2 for the
C        reduced equation.
C
         BETAR = AR11/ER11
         BETAI = AI11/ER11
         ALPHA = SQRT( -BETAR )*SQTWO
C
         VR22 = UR22*ER22
         IF ( VR22.NE.ZERO ) THEN
            M2R22 =  BR22/VR22
            M2R12 =  ( BR12 - ALPHA*VR12 )/VR22
            M2I12 =  ( BI12 - ALPHA*VI12 )/VR22
            M1R12 = -ALPHA*M2R12
            M1I12 = -ALPHA*M2I12
         ELSE
            M1R12 = ZERO
            M1I12 = ZERO
            M2R12 = ZERO
            M2R22 = ALPHA
            M2I12 = ZERO
         END IF
C
      ELSE
C
C        Discrete-time equation.
C
C        Step I:  Compute U(1,1). U(2,1) is 0.
C
         V = ER11
         T = DLAPY2( AR11, AI11 )
         IF ( V.LE.T ) THEN
            INFO = 3
            RETURN
         END IF
         T = T/V
         V = SQRT( ONE - T )*SQRT( ONE + T )*V
         T = BR11*SMLNUM
         IF ( T.GT.V ) THEN
            SCALE1 = V/T
            SCALE  = SCALE1*SCALE
            BR11   = SCALE1*BR11
            BR12   = SCALE1*BR12
            BR22   = SCALE1*BR22
            BI12   = SCALE1*BI12
         END IF
         UR11 = BR11/V
C
C        Step II:  Compute U(1,2).
C
         MX = MAX( ABS( AR11 ), ABS( AI11 ), V )
         IF ( ER11.GT.MX*SMLNUM ) THEN
            MR  =  AR11/ER11
            MI  = -AI11/ER11
            M2S =     V/ER11
            XR  =  M2S*BR12
            XI  =  M2S*BI12
            IF ( UR11.NE.ZERO ) THEN
               XR = XR + UR11*( MR*AR12 - MI*AI12 - ER12 )
               XI = XI + UR11*( MR*AI12 + MI*AR12 - EI12 )
            END IF
            YR =  MI*AI22 - MR*AR22 + ER22
            YI = -MR*AI22 - MI*AR22
         ELSE
            XR = -BR12*V
            XI = -BI12*V
            IF ( UR11.NE.ZERO ) THEN
               XR = XR + UR11*( ER11*ER12 - AR11*AR12 - AI11*AI12 )
               XI = XI + UR11*( ER11*EI12 - AR11*AI12 + AI11*AR12 )
            END IF
            YR = AR11*AR22 + AI11*AI22 - ER11*ER22
            YI = AR11*AI22 - AI11*AR22
         END IF
         T = DLAPY2( XR, XI )*SMLNUM
         W = DLAPY2( YR, YI )
         IF ( T.GT.W ) THEN
            SCALE1 = W/T
            SCALE  = SCALE1*SCALE
            BR11   = SCALE1*BR11
            BR12   = SCALE1*BR12
            BR22   = SCALE1*BR22
            BI12   = SCALE1*BI12
            UR11   = SCALE1*UR11
            XR     = SCALE1*XR
            XI     = SCALE1*XI
         END IF
         CALL DLADIV( XR, XI, YR, YI, UR12, UI12 )
C
C        Step III:  Compute U(2,2).
C
         XR = UR11*AR12 + UR12*AR22 - UI12*AI22
         XI = UR11*AI12 + UR12*AI22 + UI12*AR22
C
         IF ( ER11.GT.MX*SMLNUM ) THEN
C
C           Compute auxiliary matrices M3 and Y. The factorization
C           M3 = M3C * M3C**H is found by solving the special symmetric
C           eigenvalue problem. (D below is the diagonal of M3.)
C           It is convenient to use complex arithmetic for M3 and M3C.
C           Only the (1,2) element of M3 is needed.
C
            X     = -M2S*DCMPLX( MR, MI )
            M3(1) =  X
            CALL ZLARFG( 1, X, M3, 1, ZS )
            D(1)  = DLAPY2( MR, MI )**2
            D(2)  = M2S**2
            ET(1) = DBLE( X )
C
            CALL ZSTEIN( 2, D, ET, 1, EV, IWORK(2), IWORK(4), M3C, 2,
     $                   DWORK, IWORK(6), IWORK, INFO )
            IF ( INFO.NE.0 ) THEN
               INFO = 4
               RETURN
            END IF
C
            V  =  DBLE( M3C(1,1) )*( ONE - DBLE( ZS ) )
            W  = -DBLE( M3C(1,1) )*DIMAG( ZS )
            T  =  DBLE( M3C(2,1) )
            YR =  V*BR12 + W*BI12 + T*XR
            YI =  V*BI12 - W*BR12 + T*XI
C
C           Overwrite B(2,2) with the scalar factor R of the
C           QR-factorization of the 2-by-1 vector [ B(2,2); y ].
C
            CALL SG03BR( BR22, ZERO, YR, YI, C, SR, SI, LR, LI )
         ELSE
            T = DLAPY2( AR22, AI22 )
            IF ( ER22.LE.T ) THEN
               INFO = 3
               RETURN
            END IF
            YR = UR11*ER12 + UR12*ER22
            YI = UR11*EI12 + UI12*ER22
            V  = DLAPY2( BR12, BI12 )
            W  = DLAPY2( XR, XI )
            T  = DLAPY2( YR, YI )
            V  = DLAPY3( V, BR22, W )
            IF ( V.LE.T ) THEN
               INFO = 3
               RETURN
            END IF
            T  = T/V
            LR = SQRT( ONE - T )*SQRT( ONE + T )*V
         END IF
C
         V = ER22
         T = DLAPY2( AR22, AI22 )
         IF ( V.LE.T ) THEN
            INFO = 3
            RETURN
         END IF
         T = T/V
         V = SQRT( ONE - T )*SQRT( ONE + T )*V
         T = LR*SMLNUM
         IF ( V.LE.T ) THEN
            SCALE1 = V/T
            SCALE  = SCALE1*SCALE
            BR11   = SCALE1*BR11
            BR12   = SCALE1*BR12
            BR22   = SCALE1*BR22
            BI12   = SCALE1*BI12
            UR11   = SCALE1*UR11
            UR12   = SCALE1*UR12
            UI12   = SCALE1*UI12
            LR     = SCALE1*LR
         END IF
         UR22 = LR/V
C
C        Compute the needed elements of the matrices M1 and M2 for the
C        reduced equation.
C
         B11  = BR11/ER11
         T    = ER11*ER22
         B12R = ( ER11*BR12 - BR11*ER12 )/T
         B12I = ( ER11*BI12 - BR11*EI12 )/T
C
         BETAR = AR11/ER11
         BETAI = AI11/ER11
         V     = DLAPY2( BETAR, BETAI )
         ALPHA = SQRT( ONE - V )*SQRT( ONE + V )
C
         XR = ( AI11*EI12 - AR11*ER12 )/T + AR12/ER22
         XI = ( AR11*EI12 + AI11*ER12 )/T - AI12/ER22
         XR = -TWO*BETAI*B12I - B11*XR
         XI = -TWO*BETAI*B12R - B11*XI
         V  =  ONE + ( BETAI - BETAR )*( BETAI + BETAR )
         W  = -TWO*BETAI*BETAR
         CALL DLADIV( XR, XI, V, W, YR, YI )
         IF ( YR.NE.ZERO .OR. YI.NE.ZERO ) THEN
            M1R12 = -ALPHA*YR/UR22
            M1I12 =  ALPHA*YI/UR22
            M2R12 =  ( YR*BETAR - YI*BETAI )/UR22
            M2I12 = -( YI*BETAR + YR*BETAI )/UR22
            M2R22 =  BR22/ER22/UR22
         ELSE
            M1R12 = ZERO
            M1I12 = ZERO
            M2R12 = ZERO
            M2I12 = ZERO
            M2R22 = ALPHA
         END IF
      END IF
C
C     Transform U back:  U := U * Q.
C
      VR12 = UR12*CQ + UR11*SRQ
      VI12 = UI12*CQ + UR11*SIQ
      VR22 = UR22*CQ
C
C     Overwrite U with the upper triangular matrix of its
C     QR-factorization. The elements on the main diagonal are real
C     and non-negative.
C
      CALL SG03BR( UR11*CQ - UR12*SRQ - UI12*SIQ, UR12*SIQ - UI12*SRQ,
     $             -UR22*SRQ, UR22*SIQ, CQU, SRQU, SIQU, LR, LI )
      U(1,1) = LR
      U(2,1) = ZERO
      U(1,2) = CQU*VR12 + SRQU*VR22
      UI12   = CQU*VI12 + SIQU*VR22
      U(2,2) = CQU*VR22 - SRQU*VR12 - SIQU*VI12
      UI22   =            SIQU*VR12 - SRQU*VI12
      IF ( LI.NE.ZERO ) THEN
         V = DLAPY2( LR, LI )
         CALL DLADIV( V, ZERO, LR, LI, XR, XI )
         CQUI = XI*CQU
         CQU  = XR*CQU
         T    = XR*SRQU - XI*SIQU
         SIQU = XR*SIQU + XI*SRQU
         SRQU = T
C
         U(1,2) = XR*U(1,2) - XI*UI12
         U(1,1) = V
      END IF
C
      U(2,2) = DLAPY2( U(2,2), UI22 )
C
C     Transform the matrices M1 and M2 back.
C
C        M1 := QU * M1 * QU**H,
C        M2 := QB**H * M2 * QU**H.
C
      V = BETAR
      T = ( CQU*SRQU + CQUI*SIQU )*M1R12 +
     $    ( CQU*SIQU - CQUI*SRQU )*M1I12
C
      M1(1,1) = V + T
      M1(2,2) = V - T
      M1(1,2) = M1R12*( CQU -  CQUI )*( CQU +  CQUI ) +
     $           TWO*( BETAI*( SIQU*CQU + SRQU*CQUI ) - M1I12*CQUI*CQU )
      M1(2,1) = SIQU*( M1R12*SIQU - TWO*BETAI*CQU     - M1I12*SRQU ) -
     $          SRQU*( M1R12*SRQU + TWO*BETAI*CQUI    + M1I12*SIQU )
C
      V       = M2R12* CQU - M2I12*CQUI - ALPHA*SRQU
      W       = M2R12*CQUI + M2I12*CQU  - ALPHA*SIQU
      M2(1,1) =  CQB*( ALPHA* CQU + M2R12*SRQU + M2I12*SIQU ) +
     $          CQBI*( M2I12*SRQU - M2R12*SIQU - ALPHA*CQUI ) -
     $                 M2R22*( SRQB*SRQU + SIQB*SIQU )
      M2(2,1) = ZERO
      M2(1,2) =  CQB*V + CQBI*W - M2R22*( SRQB*CQU - SIQB*CQUI )
      M2(2,2) = SRQB*V + SIQB*W + M2R22*(  CQB*CQU - CQBI*CQUI )
C
C     If the transposed equation (op(K)=K**T, K=A,B,E,U) is to be
C     solved, transpose the matrix U with respect to the
C     anti-diagonal and the matrices M1, M2 with respect to the diagonal
C     and the anti-diagonal.
C
      IF ( ISTRNS ) THEN
         V       = U(1,1)
         U(1,1)  = U(2,2)
         U(2,2)  = V
         V       = M1(1,1)
         M1(1,1) = M1(2,2)
         M1(2,2) = V
         V       = M2(1,1)
         M2(1,1) = M2(2,2)
         M2(2,2) = V
      END IF
      U(2,1) = ZERO
C
      RETURN
C *** Last line of SG03BX ***
      END
