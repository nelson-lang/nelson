      SUBROUTINE TG01HU( COMPQ, COMPZ, L, N, M1, M2, P, N1, LBE, A, LDA,
     $                   E, LDE, B, LDB, C, LDC, Q, LDQ, Z, LDZ, NR,
     $                   NRBLCK, RTAU, TOL, IWORK, DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     Given the descriptor system (A-lambda*E,B,C) with the system
C     matrices A, E and B of the form
C
C            ( A1 X1 )        ( E1 Y1 )        ( B1 B2 )
C        A = (       ) ,  E = (       ) ,  B = (       ) ,
C            ( 0  X2 )        ( 0  Y2 )        ( 0  0  )
C
C     where
C          - B is an L-by-(M1+M2) matrix,
C            with B1 an N1-by-M1 submatrix, B2 an N1-by-M2 submatrix,
C          - A is an L-by-N matrix, with A1 an N1-by-N1 submatrix,
C          - E is an L-by-N matrix, with E1 an N1-by-N1 submatrix
C              with LBE nonzero sub-diagonals,
C     this routine reduces the pair (A1-lambda*E1,[B1 B2]) to the form
C
C     Qc'*[B1 B2 A1-lambda*E1 ]*diag(I,Zc) =
C
C                              ( Bc1 Bc2 Ac-lambda*Ec      *         )
C                              (                                     ) ,
C                              (  0   0       0       Anc-lambda*Enc )
C
C     where:
C     1) the pencil ( Bc1 Bc2 Ac-lambda*Ec ) has full row rank NR for
C        all finite lambda and is in a staircase form with
C
C                [ A11 A12  . . .  A1,p-2 A1,p-1 A1p ]
C                [ A21 A22  . . .  A2,p-2 A2,p-1 A2p ]
C                [ A31 A32  . . .  A3,p-2 A3,p-1 A3p ]
C                [  0  A42  . . .  A4,p-2 A4,p-1 A4p ]
C           Ac = [  .   .   . . .    .      .     .  ],              (1)
C                [  .   .     . .    .      .     .  ]
C                [  .   .       .    .      .     .  ]
C                [  0   0   . . .  Ap,p-2 Ap,p-1 App ]
C
C
C                     [ A1,-1 A1,0 ]
C                     [  0    A2,0 ]
C                     [  0     0   ]             ( E11  E12 ...  E1p  )
C                     [  0     0   ]             (  0   E22 ...  E2p  )
C         [Bc1 Bc2] = [  .     .   ],       Ec = (   .   .   .    .   ),
C                     [  .     .   ]             (   .   .   .    .   )
C                     [  .     .   ]             (   0   0  ...  Epp  )
C                     [  0     0   ]
C
C         where the block  Ai,i-2 is an rtau(i)-by-rtau(i-2) full row
C         rank matrix (with rtau(-1) = M1, rtau(0) = M2) and Ei,i is an
C         rtau(i)-by-rtau(i) upper triangular matrix.
C
C      2) the pencil Anc-lambda*Enc is regular of order N1-NR with Enc
C         upper triangular; this pencil contains the uncontrollable
C         finite eigenvalues of the pencil (A1-lambda*E1).
C
C     The transformations are applied to the whole matrices A, E, B
C     and C. The left and/or right orthogonal transformations Qc and Zc,
C     performed to reduce the pencil, can be optionally accumulated in
C     the matrices Q and Z, respectively.
C
C     The reduced order descriptor system (Ac-lambda*Ec,Bc,Cc) has no
C     uncontrollable finite eigenvalues and has the same transfer-
C     function matrix as the original system (A-lambda*E,B,C).
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     COMPQ   CHARACTER*1
C             = 'N':  do not compute Q;
C             = 'I':  Q is initialized to the unit matrix, and the
C                     orthogonal matrix Q is returned;
C             = 'U':  Q must contain an orthogonal matrix Q1 on entry,
C                     and the product Q1*Q is returned.
C
C     COMPZ   CHARACTER*1
C             = 'N':  do not compute Z;
C             = 'I':  Z is initialized to the unit matrix, and the
C                     orthogonal matrix Z is returned;
C             = 'U':  Z must contain an orthogonal matrix Z1 on entry,
C                     and the product Z1*Z is returned.
C
C     Input/Output Parameters
C
C     L       (input) INTEGER
C             The number of descriptor state equations; also the number
C             of rows of the matrices A, E and B.  L >= 0.
C
C     N       (input) INTEGER
C             The dimension of the descriptor state vector; also the
C             number of columns of the matrices A, E and C.  N >= 0.
C
C     M1      (input) INTEGER
C             The number of system inputs in U1, or of columns of B1.
C             M1 >= 0.
C
C     M2      (input) INTEGER
C             The number of system inputs in U2, or of columns of B2.
C             M2 >= 0.
C
C     P       (input) INTEGER
C             The dimension of descriptor system output; also the
C             number of rows of the matrix C.  P >= 0.
C
C     N1      (input) INTEGER
C             The order of the subsystem (A1-lambda*E1,B1,C1) to be
C             reduced.  MIN(L,N) >= N1 >= 0.
C
C     LBE     (input) INTEGER
C             The number of nonzero sub-diagonals of the submatrix E1.
C             MAX(0,N1-1) >= LBE >= 0.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading L-by-N part of this array must
C             contain the L-by-N state matrix A in the partitioned form
C
C                      ( A1 X1 )
C                  A = (       ) ,
C                      ( 0  X2 )
C
C             where A1 is N1-by-N1.
C             On exit, the leading L-by-N part of this array contains
C             the transformed state matrix,
C
C                                          ( Ac  *   * )
C                       Qc'*A*diag(Zc,I) = ( 0  Anc  * ) ,
C                                          ( 0   0   * )
C
C             where Ac is NR-by-NR and Anc is (N1-NR)-by-(N1-NR).
C             The matrix ( Bc Ac ) is in the controllability staircase
C             form (1).
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,L).
C
C     E       (input/output) DOUBLE PRECISION array, dimension (LDE,N)
C             On entry, the leading L-by-N part of this array must
C             contain the L-by-N descriptor matrix E in the partitioned
C             form
C                      ( E1 Y1 )
C                  E = (       ) ,
C                      ( 0  Y2 )
C
C             where E1 is an N1-by-N1 matrix with LBE nonzero
C             sub-diagonals.
C             On exit, the leading L-by-N part of this array contains
C             the transformed descriptor matrix
C
C                                          ( Ec  *   * )
C                       Qc'*E*diag(Zc,I) = ( 0  Enc  * ) ,
C                                          ( 0   0   * )
C
C             where Ec is NR-by-NR and Enc is (N1-NR)-by-(N1-NR).
C             Both Ec and Enc are upper triangular and Enc is
C             nonsingular.
C
C     LDE     INTEGER
C             The leading dimension of the array E.  LDE >= MAX(1,L).
C
C     B       (input/output) DOUBLE PRECISION array, dimension (LDB,M)
C             with M = M1 + M2.
C             On entry, the leading L-by-M part of this array must
C             contain the L-by-M input matrix B in the partitioned form
C
C                      ( Bi )
C                  B = (    ) ,
C                      ( 0  )
C
C             where Bi is N1-by-M.
C             On exit, the leading L-by-M part of this array contains
C             the transformed input matrix
C
C                               ( Bc )
C                       Qc'*B = (    ) ,
C                               ( 0  )
C
C             where Bc is NR-by-M.
C             The matrix ( Bc Ac ) is in the controllability staircase
C             form (1).
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= MAX(1,L).
C
C     C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
C             On entry, the leading P-by-N part of this array must
C             contain the state/output matrix C.
C             On exit, the leading P-by-N part of this array contains
C             the transformed matrix C*Zc.
C
C     LDC     INTEGER
C             The leading dimension of the array C.  LDC >= MAX(1,P).
C
C     Q       (input/output) DOUBLE PRECISION array, dimension (LDQ,L)
C             If COMPQ = 'N': Q is not referenced.
C             If COMPQ = 'I': on entry, Q need not be set;
C                             on exit, the leading L-by-L part of this
C                             array contains the orthogonal matrix Qc,
C                             where Qc' is the product of the
C                             transformations applied to A, E, and B on
C                             the left.
C             If COMPQ = 'U': on entry, the leading L-by-L part of this
C                             array must contain an orthogonal matrix Q;
C                             on exit, the leading L-by-L part of this
C                             array contains the orthogonal matrix
C                             Q*Qc.
C
C     LDQ     INTEGER
C             The leading dimension of the array Q.
C             LDQ >= 1,        if COMPQ = 'N';
C             LDQ >= MAX(1,L), if COMPQ = 'I' or 'U'.
C
C     Z       (input/output) DOUBLE PRECISION array, dimension (LDZ,N)
C             If COMPZ = 'N': Z is not referenced.
C             If COMPZ = 'I': on entry, Z need not be set;
C                             on exit, the leading N-by-N part of this
C                             array contains the orthogonal matrix Zc,
C                             i.e., the product of the transformations
C                             applied to A, E, and C on the right.
C             If COMPZ = 'U': on entry, the leading N-by-N part of this
C                             array must contain an orthogonal matrix Z;
C                             on exit, the leading N-by-N part of this
C                             array contains the orthogonal matrix
C                             Z*Zc.
C
C     LDZ     INTEGER
C             The leading dimension of the array Z.
C             LDZ >= 1,        if COMPZ = 'N';
C             LDZ >= MAX(1,N), if COMPZ = 'I' or 'U'.
C
C     NR      (output) INTEGER
C             The order of the reduced matrices Ac and Ec, and the
C             number of rows of the reduced matrix Bc; also the order of
C             the controllable part of the pair (B, A-lambda*E).
C
C     NRBLCK  (output) INTEGER
C             The number p, of full row rank blocks Ai,i-2 in the
C             staircase form of the pencil (Bc1 Bc2 Ac-lambda*Ec).
C
C     RTAU    (output) INTEGER array, dimension (2*N1)
C             The leading NRBLCK elements of this array contain the
C             orders of the diagonal blocks of Ac. NRBLCK is always
C             an even number, and the NRBLCK/2 odd and even components
C             of RTAU have decreasing values, respectively.
C             Note that some elements of RTAU can be zero.
C
C     Tolerances
C
C     TOL     DOUBLE PRECISION
C             The tolerance to be used in rank determinations when
C             transforming (A-lambda*E, B). If the user sets TOL > 0,
C             then the given value of TOL is used as a lower bound for
C             reciprocal condition numbers in rank determinations; a
C             (sub)matrix whose estimated condition number is less than
C             1/TOL is considered to be of full rank.  If the user sets
C             TOL <= 0, then an implicitly computed, default tolerance,
C             defined by  TOLDEF = L*N*EPS,  is used instead, where
C             EPS is the machine precision (see LAPACK Library routine
C             DLAMCH).  TOL < 1.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (M)
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The length of the array DWORK.
C             LDWORK >= 1, if MIN(N1,M) = 0; otherwise,
C             LDWORK >= MAX(N1+MAX(L,N,M),2*M), if LBE > 0 and N1 > 2;
C             LDWORK >= MAX(1,L,N,2*M),         if LBE = 0 or N1 <= 2.
C             For optimum performance LDWORK should be larger.
C
C             If LDWORK = -1, then a workspace query is assumed;
C             the routine only calculates the optimal size of the
C             DWORK array, returns this value as the first entry of
C             the DWORK array, and no error message related to LDWORK
C             is issued by XERBLA.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value.
C
C     METHOD
C
C     The implemented algorithm [1] represents a specialization of the
C     controllability staircase algorithm of [2] to the special structure
C     of the input matrix B = [B1,B2].
C
C     REFERENCES
C
C     [1] Varga, A.
C         Reliable algorithms for computing minimal dynamic covers for
C         descriptor systems.
C         Proc. of MTNS'04, Leuven, Belgium, 2004.
C
C     [2] Varga, A.
C         Computation of Irreducible Generalized State-Space
C         Realizations.
C         Kybernetika, vol. 26, pp. 89-106, 1990.
C
C     NUMERICAL ASPECTS
C
C     The algorithm is numerically backward stable and requires
C     0( N*N1**2 )  floating point operations.
C
C     CONTRIBUTOR
C
C     A. Varga, German Aerospace Center, DLR Oberpfaffenhofen.
C     April 2003. Based on the SLICOT routine TG01HX.
C
C     REVISIONS
C
C     A. Varga, Dec. 2006.
C     V. Sima, Dec. 2016, Mar. 2019.
C
C     KEYWORDS
C
C     Controllability, minimal realization, orthogonal canonical form,
C     orthogonal transformation.
C
C     ******************************************************************
C
C     .. Parameters ..
      INTEGER            IMAX, IMIN
      PARAMETER          ( IMAX = 1, IMIN = 2 )
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D0, ZERO = 0.0D0 )
C     .. Scalar Arguments ..
      CHARACTER          COMPQ, COMPZ
      INTEGER            INFO, L, LBE, LDA, LDB, LDC, LDE, LDQ, LDWORK,
     $                   LDZ, M1, M2, N, N1, NR, NRBLCK, P
      DOUBLE PRECISION   TOL
C     .. Array Arguments ..
      INTEGER            IWORK( * ), RTAU( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * ),
     $                   DWORK( * ),  E( LDE, * ), Q( LDQ, * ),
     $                   Z( LDZ, * )
C     .. Local Scalars ..
      LOGICAL            B1RED, ILQ, ILZ, LQUERY, ONECOL, WITHC
      INTEGER            I, IC, ICOL, ICOMPQ, ICOMPZ, IROW, ISMAX,
     $                   ISMIN, J, JB2, K, M, MCRT, MCRT1, MCRT2,
     $                   MINWRK, MN, NB, NF, NR1, NX, RANK, WRKOPT
      DOUBLE PRECISION   C1, C2, CO, RCOND, S1, S2, SI, SMAX, SMAXPR,
     $                   SMIN, SMINPR, SVLMAX, SVMA, SVMR, T, TOLZ, TT
C     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            IDAMAX, ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE, DLAPY2, DNRM2
      EXTERNAL           DLAMCH, DLANGE, DLAPY2, DNRM2, IDAMAX, ILAENV,
     $                   LSAME
C     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEQRF, DLACPY, DLAIC1, DLARF, DLARFG,
     $                   DLARTG, DLASET, DORMQR, DROT, DSWAP, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, INT, MAX, MIN, SQRT
C
C     .. Executable Statements ..
C
C     Decode COMPQ.
C
      IF( LSAME( COMPQ, 'N' ) ) THEN
         ILQ = .FALSE.
         ICOMPQ = 1
      ELSE IF( LSAME( COMPQ, 'U' ) ) THEN
         ILQ = .TRUE.
         ICOMPQ = 2
      ELSE IF( LSAME( COMPQ, 'I' ) ) THEN
         ILQ = .TRUE.
         ICOMPQ = 3
      ELSE
         ICOMPQ = 0
      END IF
C
C     Decode COMPZ.
C
      IF( LSAME( COMPZ, 'N' ) ) THEN
         ILZ = .FALSE.
         ICOMPZ = 1
      ELSE IF( LSAME( COMPZ, 'U' ) ) THEN
         ILZ = .TRUE.
         ICOMPZ = 2
      ELSE IF( LSAME( COMPZ, 'I' ) ) THEN
         ILZ = .TRUE.
         ICOMPZ = 3
      ELSE
         ICOMPZ = 0
      END IF
C
C     Test the input scalar parameters.
C
      INFO = 0
      IF( ICOMPQ.LE.0 ) THEN
         INFO = -1
      ELSE IF( ICOMPZ.LE.0 ) THEN
         INFO = -2
      ELSE IF( L.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( M1.LT.0 ) THEN
         INFO = -5
      ELSE IF( M2.LT.0 ) THEN
         INFO = -6
      ELSE IF( P.LT.0 ) THEN
         INFO = -7
      ELSE IF( N1.LT.0 .OR. N1.GT.MIN( L, N ) ) THEN
         INFO = -8
      ELSE IF( LBE.LT.0 .OR. LBE.GT.MAX( 0, N1-1 ) ) THEN
         INFO = -9
      ELSE IF( LDA.LT.MAX( 1, L ) ) THEN
         INFO = -11
      ELSE IF( LDE.LT.MAX( 1, L ) ) THEN
         INFO = -13
      ELSE IF( LDB.LT.MAX( 1, L ) ) THEN
         INFO = -15
      ELSE IF( LDC.LT.MAX( 1, P ) ) THEN
         INFO = -17
      ELSE IF( ( ILQ .AND. LDQ.LT.L ) .OR. LDQ.LT.1 ) THEN
         INFO = -19
      ELSE IF( ( ILZ .AND. LDZ.LT.N ) .OR. LDZ.LT.1 ) THEN
         INFO = -21
      ELSE IF( TOL.GE.ONE ) THEN
         INFO = -25
      ELSE
         M = M1 + M2
         IF( MIN( N1, M ).EQ.0 ) THEN
            MINWRK = 1
         ELSE IF( LBE.GT.0 .AND. N1.GT.2 ) THEN
            MINWRK = MAX( N1 + MAX( L, N, M ), 2*M )
         ELSE
            MINWRK = MAX( 1, L, N, 2*M )
         END IF
C
         LQUERY = LDWORK.EQ.-1
         IF( LQUERY ) THEN
            IF( LBE.GT.0 .AND. N1.GT.2 ) THEN
               CALL DGEQRF( N1, N1, E, LDE, DWORK, DWORK, -1, INFO )
               WRKOPT = MAX( MINWRK, N1 + INT( DWORK(1) ) )
               CALL DORMQR( 'Left', 'Transpose', N1, N, N1, E, LDE,
     $                      DWORK, A, LDA, DWORK, -1, INFO )
               WRKOPT = MAX( WRKOPT, N1 + INT( DWORK(1) ) )
               CALL DORMQR( 'Left', 'Transpose', N1, M, N1, E, LDE,
     $                      DWORK, B, LDB, DWORK, -1, INFO )
               WRKOPT = MAX( WRKOPT, N1 + INT( DWORK(1) ) )
               IF( ILQ ) THEN
                  CALL DORMQR( 'Right', 'NoTranspose', L, N1, N1, E,
     $                         LDE, DWORK, Q, LDQ, DWORK, -1, INFO )
                  WRKOPT = MAX( WRKOPT, N1 + INT( DWORK(1) ) )
               END IF
            ELSE
               WRKOPT = MINWRK
            END IF
         ELSE IF( LDWORK.LT.MINWRK ) THEN
            INFO = -28
         END IF
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'TG01HU', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         DWORK(1) = WRKOPT
         RETURN
      END IF
C
C     Initialize Q and Z if necessary.
C
      IF( ICOMPQ.EQ.3 )
     $   CALL DLASET( 'Full', L, L, ZERO, ONE, Q, LDQ )
      IF( ICOMPZ.EQ.3 )
     $   CALL DLASET( 'Full', N, N, ZERO, ONE, Z, LDZ )
C
C     Initialize output variables.
C
      NR = 0
      NRBLCK = 0
C
C     Quick return if possible.
C
      IF( MIN( N1, M ).EQ.0 ) THEN
         DWORK(1) = ONE
         RETURN
      END IF
C
      TOLZ   = DLAMCH( 'Precision' )
      WITHC  = P.GT.0
      SVLMAX = DLANGE( 'F', L, M, B, LDB, DWORK )
      RCOND  = TOL
      IF ( RCOND.LE.ZERO ) THEN
C
C        Use the default tolerance in controllability determination.
C
         RCOND = DBLE( L*N )*TOLZ
      END IF
      TOLZ = SQRT( TOLZ )
C
      IF ( SVLMAX.LT.RCOND )
     $   SVLMAX = ONE
      SVMR = SVLMAX*RCOND
      SVMA = MAX( ONE, DLANGE( 'F', L, N, A, LDA, DWORK ) )*RCOND
      IF( SVMA.GT.SVMR*TOLZ )
     $    SVMA = DLAPY2( SVMR, SVMA )
      NX = ILAENV( 3, 'DGEQRF', ' ', N1, N1, -1, -1 )
      NB = LDWORK/N1
C
C     Reduce E to upper triangular form if necessary.
C
      IF( LBE.GT.NX/2 .AND. MIN( NB, N1 ).GE.NX ) THEN
C
C        If E1 is a rather full matrix of enough size, use its
C        QR decomposition and apply it to A, B, and Q (if needed).
C        Workspace:  need    2*N1;
C                    prefer  N1 + N1*NB.
C
         CALL DGEQRF( N1, N1, E, LDE, DWORK, DWORK(N1+1), LDWORK-N1,
     $                INFO )
         WRKOPT = MAX( WRKOPT, N1 + INT( DWORK(N1+1) ) )
C
C        Workspace:  need    N1 + N;
C                    prefer  N1 + N*NB.
C
         CALL DORMQR( 'Left', 'Transpose', N1, N, N1, E, LDE, DWORK, A,
     $                LDA, DWORK(N1+1), LDWORK-N1, INFO )
         WRKOPT = MAX( WRKOPT, N1 + INT( DWORK(N1+1) ) )
C
C        Workspace:  need    N1 + M;
C                    prefer  N1 + M*NB.
C
         CALL DORMQR( 'Left', 'Transpose', N1, M, N1, E, LDE, DWORK, B,
     $                LDB, DWORK(N1+1), LDWORK-N1, INFO )
         WRKOPT = MAX( WRKOPT, N1 + INT( DWORK(N1+1) ) )
         IF( ILQ ) THEN
C
C           Workspace:  need    N1 + L;
C                       prefer  N1 + L*NB.
C
            CALL DORMQR( 'Right', 'NoTranspose', L, N1, N1, E, LDE,
     $                   DWORK, Q, LDQ, DWORK(N1+1), LDWORK-N1, INFO )
            WRKOPT = MAX( WRKOPT, N1 + INT( DWORK(N1+1) ) )
         END IF
         CALL DLASET( 'Lower', N1-1, N1-1, ZERO, ZERO, E(2,1), LDE )
      ELSE IF( LBE.GT.0 .AND. N1.GT.1 ) THEN
         DO 10 I = 1, N1 - 1
C
C           Generate elementary reflector H(i) to annihilate
C           E(i+1:i+lbe,i).
C
            K = MIN( LBE, N1-I ) + 1
            CALL DLARFG( K, E(I,I), E(I+1,I), 1, TT )
            T = E(I,I)
            E(I,I) = ONE
C
C           Apply H(i) to E(i:n1,i+1:n) from the left.
C
            CALL DLARF( 'Left', K, N-I, E(I,I), 1, TT,
     $                  E(I,I+1), LDE, DWORK )
C
C           Apply H(i) to A(i:n1,1:n) from the left.
C
            CALL DLARF( 'Left', K, N, E(I,I), 1, TT,
     $                  A(I,1), LDA, DWORK )
C
C           Apply H(i) to B(i:n1,1:m) from the left.
C
            CALL DLARF( 'Left', K, M, E(I,I), 1, TT,
     $                  B(I,1), LDB, DWORK )
            IF( ILQ ) THEN
C
C              Apply H(i) to Q(1:l,i:n1) from the right.
C
               CALL DLARF( 'Right', L, K, E(I,I), 1, TT,
     $                     Q(1,I), LDQ, DWORK )
            END IF
            E(I,I) = T
   10    CONTINUE
         CALL DLASET( 'Lower', N1-1, N1-1, ZERO, ZERO, E(2,1), LDE )
      END IF
C
      MCRT1 = M1
      MCRT2 = M2
      MCRT  = MCRT1
      B1RED = .TRUE.
      ISMIN = 1
      ISMAX = ISMIN + M
C
      IC  = 0
      NF  = N1
      JB2 = M
C
   20 CONTINUE
      IF( NF.EQ.0 .AND. B1RED )
     $   GO TO 120
      NRBLCK = NRBLCK + 1
      RANK   = 0
C
      IF( NF.GT.0 ) THEN
C
C        IROW will point to the current pivot line in B,
C        ICOL+1 will point to the first active columns of A.
C
         ICOL = IC
         IROW = NR
         NR1  = NR + 1
         IF( NRBLCK.EQ.2 ) THEN
            CALL DLACPY( 'Full', NF, M2, B(NR1,M1+1), LDB,
     $                   B(NR1,1), LDB )
            JB2 = MCRT
         ELSEIF( NRBLCK.GT.2 ) THEN
            CALL DLACPY( 'Full', NF, MCRT, A(NR1,IC+1), LDA,
     $                   B(NR1,1), LDB )
            ICOL = IC + MCRT
            SVMR = SVMA
            JB2  = MCRT
         ENDIF
         ONECOL = MCRT.EQ.1
C
C        Perform QR-decomposition with column pivoting on the current B
C        while keeping E upper triangular.
C        The current B is at first iteration B1, at second iteration B2
C        and for subsequent iterations the NF-by-MCRT matrix delimited
C        by rows NR + 1 to N1 and columns IC + 1 to IC + MCRT of A.
C        The rank of current B is computed in RANK.
C
         IF( ONECOL ) THEN
            MN = 1
         ELSE
            MN = MIN( NF, MCRT )
C
C           Compute column norms.
C
            DO 30 J = 1, MCRT
               DWORK(J)   = DNRM2( NF, B(NR1,J), 1 )
               DWORK(M+J) = DWORK(J)
               IWORK(J)   = J
   30       CONTINUE
         END IF
C
   40    CONTINUE
         IF( RANK.LT.MN ) THEN
            J    = RANK + 1
            IROW = IROW + 1
C
C           Pivot if necessary.
C
            IF( J.NE.MCRT ) THEN
               K = ( J - 1 ) + IDAMAX( MCRT-J+1, DWORK(J), 1 )
               IF( K.NE.J ) THEN
                  CALL DSWAP( NF, B(NR1,J), 1, B(NR1,K), 1 )
                  I = IWORK(K)
                  IWORK(K)   = IWORK(J)
                  IWORK(J)   = I
                  DWORK(K)   = DWORK(J)
                  DWORK(M+K) = DWORK(M+J)
               END IF
            END IF
C
C           Zero elements below the current diagonal element of B.
C
            DO 50 I = N1-1, IROW, -1
C
C              Rotate rows I and I+1 to zero B(I+1,J).
C
               T = B(I,J)
               CALL DLARTG( T, B(I+1,J), CO, SI, B(I,J) )
               B(I+1,J) = ZERO
               CALL DROT( N-I+1, E(I,I), LDE, E(I+1,I), LDE, CO, SI )
               IF( J.LT.JB2 )
     $             CALL DROT( JB2-J, B(I,J+1), LDB, B(I+1,J+1), LDB, CO,
     $                        SI )
               CALL DROT( N-ICOL, A(I,ICOL+1), LDA, A(I+1,ICOL+1), LDA,
     $                    CO, SI )
               IF( ILQ )
     $            CALL DROT( L, Q(1,I), 1, Q(1,I+1), 1, CO, SI )
C
C              Rotate columns I, I+1 to zero E(I+1,I).
C
               T = E(I+1,I+1)
               CALL DLARTG( T, E(I+1,I), CO, SI, E(I+1,I+1) )
               E(I+1,I) = ZERO
               CALL DROT( I,  E(1,I+1), 1, E(1,I), 1, CO, SI )
               CALL DROT( N1, A(1,I+1), 1, A(1,I), 1, CO, SI )
               IF( ILZ )
     $            CALL DROT( N, Z(1,I+1), 1, Z(1,I), 1, CO, SI )
               IF( WITHC )
     $            CALL DROT( P, C(1,I+1), 1, C(1,I), 1, CO, SI )
   50       CONTINUE
C
            IF( RANK.EQ.0 ) THEN
C
C              Initialize; exit if matrix is zero (RANK = 0).
C              Short pass if the current B has one column.
C
               SMAX = ABS( B(NR1,1) )
               IF ( SMAX.LE.SVMR ) THEN
                  GO TO 80
                ELSE IF ( ONECOL ) THEN
                  RANK = RANK + 1
                  GO TO 80
               END IF
               SMIN   = SMAX
               SMAXPR = SMAX
               SMINPR = SMIN
               C1 = ONE
               C2 = ONE
            ELSE
C
C              One step of incremental condition estimation.
C
               CALL DLAIC1( IMIN, RANK, DWORK(ISMIN), SMIN, B(NR1,J),
     $                      B(IROW,J), SMINPR, S1, C1 )
               CALL DLAIC1( IMAX, RANK, DWORK(ISMAX), SMAX, B(NR1,J),
     $                      B(IROW,J), SMAXPR, S2, C2 )
            END IF
C
C           Check the rank; finish the loop if rank loss occurs.
C
            IF( SVMR.LE.SMAXPR ) THEN
               IF( SMAXPR*RCOND.LT.SMINPR ) THEN
C
C                 Finish the loop if last row.
C
                  IF( IROW.EQ.N1 ) THEN
                     RANK = RANK + 1
                     GO TO 80
                  END IF
C
C                 Update partial column norms.
C
                  DO 60 I = J + 1, MCRT
                     IF( DWORK(I).NE.ZERO ) THEN
                        T = ABS( B(IROW,I) )/DWORK(I)
                        T = MAX( ( ONE + T )*( ONE - T ), ZERO)
                        TT = T*( DWORK(I)/DWORK(M+I) )**2
                        IF( TT.GT.TOLZ ) THEN
                           DWORK(I) = DWORK(I)*SQRT( T )
                        ELSE
                           DWORK(I) = DNRM2( NF-J, B(IROW+1,I), 1 )
                           DWORK(M+I) = DWORK(I)
                        END IF
                     END IF
   60             CONTINUE
C
                  DO 70 I = 1, RANK
                     DWORK(ISMIN+I-1) = S1*DWORK(ISMIN+I-1)
                     DWORK(ISMAX+I-1) = S2*DWORK(ISMAX+I-1)
   70             CONTINUE
C
                  DWORK(ISMIN+RANK) = C1
                  DWORK(ISMAX+RANK) = C2
                  SMIN = SMINPR
                  SMAX = SMAXPR
                  RANK = RANK + 1
                  GO TO 40
               END IF
            END IF
         END IF
      END IF
C
   80 CONTINUE
C
      IF( RANK.GT.0 ) THEN
         RTAU(NRBLCK) = RANK
C
C        Back permute interchanged columns.
C
         IF( .NOT.ONECOL ) THEN
            DO 100 J = 1, MCRT
               IF( IWORK(J).GT.0 ) THEN
                  K = IWORK(J)
                  IWORK(J) = -K
   90             CONTINUE
                  IF( K.NE.J ) THEN
                     CALL DSWAP( RANK, B(NR1,J), 1, B(NR1,K), 1 )
                     IWORK(K) = -IWORK(K)
                     K = -IWORK(K)
                     GO TO 90
                  END IF
               END IF
  100       CONTINUE
         END IF
      END IF
      IF( NRBLCK.EQ.2 ) THEN
         DO 110 J = M2, 1, -1
            CALL DCOPY( NF, B(NR1,J), 1, B(NR1,M1+J), 1 )
  110    CONTINUE
      ELSEIF( NRBLCK.GT.2 ) THEN
         CALL DLACPY( 'Full', NF, MCRT, B(NR1,1), LDB, A(NR1,IC+1),
     $                LDA )
      END IF		
      IF( RANK.GT.0 ) THEN
         NR = NR + RANK
         NF = NF - RANK
         IF( NRBLCK.GT.2 )
     $      IC = IC + MCRT
         IF( B1RED ) THEN
            MCRT1 = RANK
            MCRT  = MCRT2
         ELSE
            MCRT2 = RANK
            MCRT  = MCRT1
         END IF
         B1RED = .NOT.B1RED
         GO TO 20
      ELSE
         IF( B1RED ) THEN
            IF( MCRT2.GT.0 ) THEN
               B1RED = .NOT.B1RED
               RTAU(NRBLCK) = 0
               IF( NRBLCK.GT.2 )
     $            IC = IC + MCRT
               MCRT1 = 0
               MCRT  = MCRT2
               GO TO 20
            END IF
            NRBLCK = NRBLCK - 1
         ELSE
            IF( MCRT1.GT.0 ) THEN
               B1RED = .NOT.B1RED
               RTAU(NRBLCK) = 0
               IF( NRBLCK.GT.2 )
     $            IC = IC + MCRT
               MCRT2 = 0
               MCRT  = MCRT1
               GO TO 20
            END IF
            NRBLCK = NRBLCK - 2
         END IF
      END IF
C
  120 CONTINUE
C
      IF( NRBLCK.GT.0 ) THEN
C
         RANK = RTAU(1)
         IF( RANK.LT.N1 )
     $      CALL DLASET( 'Full', N1-RANK, M1, ZERO, ZERO, B(RANK+1,1),
     $                   LDB )
         RANK = RANK + RTAU(2)
         IF( RANK.LT.N1 )
     $      CALL DLASET( 'Full', N1-RANK, M2, ZERO, ZERO,
     $                   B(RANK+1,M1+1), LDB )
      END IF
C
      DWORK(1) = WRKOPT
      RETURN
C *** Last line of TG01HU ***
      END
