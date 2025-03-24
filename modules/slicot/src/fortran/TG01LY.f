      SUBROUTINE TG01LY( COMPQ, COMPZ, N, M, P, RANKE, RNKA22, A, LDA,
     $                   E, LDE, B, LDB, C, LDC, Q, LDQ, Z, LDZ, NF,
     $                   NIBLCK, IBLCK, TOL, IWORK, DWORK, LDWORK,
     $                   INFO )
C
C     PURPOSE
C
C     To compute orthogonal transformation matrices Q and Z which reduce
C     the regular pole pencil A-lambda*E of the descriptor system
C     (A-lambda*E,B,C), with the A and E matrices in the form
C
C           ( A11 A12 A13 )             ( E11  0  0 )
C       A = ( A21 A22 A23 ) ,       E = (  0   0  0 ) ,              (1)
C           ( A31  0   0  )             (  0   0  0 )
C
C     where E11 and A22 are nonsingular and upper triangular matrices,
C     to the form
C
C                ( Af  *  )             ( Ef  *  )
C       Q'*A*Z = (        ) ,  Q'*E*Z = (        ) ,
C                ( 0   Ai )             ( 0   Ei )
C
C     where the subpencil Af-lambda*Ef contains the finite eigenvalues
C     and the subpencil Ai-lambda*Ei contains the infinite eigenvalues.
C     The subpencil Ai-lambda*Ei is in a staircase form with the
C     matrices Ai and Ei of form
C
C
C           ( A0,0  A0,k ... A0,1 )         ( 0  E0,k ... E0,1 )
C      Ai = (  0    Ak,k ... Ak,1 ) ,  Ei = ( 0   0   ... Ek,1 ) ,   (2)
C           (  :     :   ...   :  )         ( :   :   ...   :  )
C           (  0     0   ... A1,1 )         ( 0   0   ...   0  )
C
C     where Ai,i, for i = 0, 1, ..., k, are nonsingular upper triangular
C     matrices.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     COMPQ   LOGICAL
C             Specify the option to accumulate or not the performed
C             left transformations:
C             COMPQ = .FALSE. : do not accumulate the transformations;
C             COMPQ = .TRUE.  : accumulate the transformations; in this
C                     case, Q must contain an orthogonal matrix Q1
C                     on entry, and the product Q1*Q is returned.
C
C     COMPZ   LOGICAL
C             Specify the option to accumulate or not the performed
C             right transformations:
C             COMPZ = .FALSE. : do not accumulate the transformations;
C             COMPZ = .TRUE.  : accumulate the transformations; in this
C                     case, Z must contain an orthogonal matrix Z1
C                     on entry, and the product Z1*Z is returned.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The number of rows of the matrix B, the number of columns
C             of the matrix C and the order of the square matrices A
C             and E.  N >= 0.
C
C     M       (input) INTEGER
C             The number of columns of the matrix B.  M >= 0.
C
C     P       (input) INTEGER
C             The number of rows of the matrix C.  P >= 0.
C
C     RANKE   (input) INTEGER
C             The rank of the matrix E; also, the order of the upper
C             triangular matrix E11.  0 <= RANKE <= N. 
C
C     RNKA22  (input) DOUBLE PRECISION
C             The order of the nonsingular submatrix A22 of A.
C             0 <= RNKA22 <= N - RANKE. 
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the N-by-N state matrix A in the form (1).
C             On exit, the leading N-by-N part of this array contains
C             the transformed state matrix Q'*A*Z,
C
C                                ( Af  *  )
C                       Q'*A*Z = (        ) ,
C                                ( 0   Ai )
C
C             where Af is NF-by-NF and Ai is (N-NF)-by-(N-NF).
C             The submatrix Ai is in the staircase form (2), where A0,0
C             is (N-RANKE)-by-(N-RANKE), and Ai,i , for i = 1, ...,
C             NIBLCK is IBLCK(i)-by-IBLCK(i).
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     E       (input/output) DOUBLE PRECISION array, dimension (LDE,N)
C             On entry, the leading N-by-N part of this array must
C             contain the N-by-N descriptor matrix E in the form (1).
C             On exit, the leading N-by-N part of this array contains
C             the transformed descriptor matrix Q'*E*Z,
C
C                                ( Ef  *  )
C                       Q'*E*Z = (        ) ,
C                                ( 0   Ei )
C
C             where Ef is an NF-by-NF nonsingular matrix and Ei is an
C             (N-NF)-by-(N-NF) nilpotent matrix in the staircase
C             form (2).
C
C     LDE     INTEGER
C             The leading dimension of the array E.  LDE >= MAX(1,N).
C
C     B       (input/output) DOUBLE PRECISION array, dimension (LDB,M)
C             On entry, the leading N-by-M part of this array must
C             contain the N-by-M input matrix B.
C             On exit, the leading N-by-M part of this array contains
C             the transformed input matrix Q'*B.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= MAX(1,N).
C
C     C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
C             On entry, the leading P-by-N part of this array must
C             contain the state/output matrix C.
C             On exit, the leading P-by-N part of this array contains
C             the transformed matrix C*Z.
C
C     LDC     INTEGER
C             The leading dimension of the array C.  LDC >= MAX(1,P).
C
C     Q       (input/output) DOUBLE PRECISION array, dimension (LDQ,N)
C             If COMPQ = .FALSE., Q is not referenced.
C             If COMPQ = .TRUE., on entry, the leading N-by-N part of
C                         this array must contain an orthogonal matrix
C                         Q1; on exit, the leading N-by-N part of this
C                         array contains the orthogonal matrix Q1*Q.
C
C     LDQ     INTEGER
C             The leading dimension of the array Q.
C             LDQ >= 1,        if COMPQ = .FALSE.;
C             LDQ >= MAX(1,N), if COMPQ = .TRUE. .
C
C     Z       (input/output) DOUBLE PRECISION array, dimension (LDZ,N)
C             If COMPZ = .FALSE., Z is not referenced.
C             If COMPZ = .TRUE., on entry, the leading N-by-N part of
C                        this array must contain an orthogonal matrix
C                        Z1; on exit, the leading N-by-N part of this
C                        array contains the orthogonal matrix Z1*Z.
C
C     LDZ     INTEGER
C             The leading dimension of the array Z.
C             LDZ >= 1,        if COMPZ = .FALSE.;
C             LDZ >= MAX(1,N), if COMPZ = .TRUE. .
C
C     NF      (output) INTEGER
C             The order of the reduced matrices Af and Ef; also, the
C             number of finite generalized eigenvalues of the pencil
C             A-lambda*E.
C
C     NIBLCK  (output) INTEGER
C             If RANKE < N, the number of infinite blocks minus one.
C             If RANKE = N, NIBLCK = 0.
C
C     IBLCK   (output) INTEGER array, dimension (N)
C             IBLCK(i) contains the dimension of the i-th block in the
C             staircase form (2), where i = 1, 2, ..., NIBLCK.
C
C     Tolerances
C
C     TOL     DOUBLE PRECISION
C             A tolerance used in rank decisions to determine the
C             effective rank, which is defined as the order of the
C             largest leading (or trailing) triangular submatrix in the
C             QR factorization with column pivoting whose estimated
C             condition number is less than 1/TOL.  If the user sets
C             TOL <= 0, then an implicitly computed, default tolerance,
C             TOLDEF = N**2*EPS,  is used instead, where EPS is the
C             machine precision (see LAPACK Library routine DLAMCH).
C             TOL < 1.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (N-RANKE)
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The length of the array DWORK.
C             LDWORK >= 1, if RANKE = N; otherwise,
C             LDWORK >= MAX(4*(N-RANKE)-1, N-RANKE-RNKA22+MAX(N,M)).
C             For optimal performance, LDWORK should be larger.
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
C                   value;
C             = 1:  the pencil A-lambda*E is not regular.
C
C     METHOD
C
C     The subroutine is based on the reduction algorithm of [1].
C
C     REFERENCES
C
C     [1] Misra, P., Van Dooren, P., and Varga, A.
C         Computation of structural invariants of generalized
C         state-space systems.
C         Automatica, 30, pp. 1921-1936, 1994.
C
C     NUMERICAL ASPECTS
C
C     The algorithm is numerically backward stable and requires
C     0( N**3 )  floating point operations.
C
C     FURTHER COMMENTS
C
C     The number of infinite poles is computed as
C
C            NIBLCK
C             Sum  IBLCK(i) = RANKE - NF.
C             i=1
C
C     The multiplicities of infinite poles can be computed as follows:
C     there are IBLCK(k)-IBLCK(k+1) infinite poles of multiplicity
C     k, for k = 1, ..., NIBLCK, where IBLCK(NIBLCK+1) = 0.
C     Note that each infinite pole of multiplicity k corresponds to
C     an infinite eigenvalue of multiplicity k+1.
C
C     CONTRIBUTOR
C
C     A. Varga, German Aerospace Center, DLR Oberpfaffenhofen.
C     July 1999. Based on the RASP routine SRISEP.
C
C     REVISIONS
C
C     A. Varga, German Aerospace Center, DLR Oberpfaffenhofen.
C     July 2001; Nov. 2002
C     V. Sima, Dec. 2016, Feb. 2017, June 2017.
C
C     KEYWORDS
C
C     Generalized eigenvalue problem, system poles, multivariable
C     system, orthogonal transformation, structural invariant.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D0, ZERO = 0.0D0 )
C     .. Scalar Arguments ..
      LOGICAL            COMPQ, COMPZ
      INTEGER            INFO, LDA, LDB, LDC, LDE, LDQ, LDWORK, LDZ, M,
     $                   N, NF, NIBLCK, P, RANKE, RNKA22
      DOUBLE PRECISION   TOL
C     .. Array Arguments ..
      INTEGER            IBLCK( * ), IWORK(*)
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * ),
     $                   DWORK(  * ), E( LDE, * ), Q( LDQ, * ),
     $                   Z( LDZ, * )
C     .. Local Scalars ..
      LOGICAL            FIRST, LQUERY
      INTEGER            I, I0, I1, ICOL, IPIV, IROW, ITAU, J, JWORK1,
     $                   JWORK2, K, MINWRK, MM1, N1, ND, NR, RANK, RO,
     $                   RO1, SIGMA, WRKOPT
      DOUBLE PRECISION   CO, RCOND, SI, SVLMAX, T, TOLDEF
C     .. Local Arrays ..
      DOUBLE PRECISION   DUM(1), SVAL(3)
C     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DLANGE, DLANTR, DLAPY2, DNRM2
      EXTERNAL           DLAMCH, DLANGE, DLANTR, DLAPY2, DNRM2
C     .. External Subroutines ..
      EXTERNAL           DCOPY, DLACPY, DLAPMT, DLARFG, DLARTG, DLASET,
     $                   SLCT_DLATZM, DORMQR, DROT, DSWAP, DTRCON, MB03OY,
     $                   XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC          DBLE, INT, MAX, MIN, MOD
C     .. Executable Statements ..
C
C     Test the input parameters.
C
      INFO = 0
      IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( M.LT.0 ) THEN
         INFO = -4
      ELSE IF( P.LT.0 ) THEN
         INFO = -5
      ELSE IF( RANKE.LT.0 .OR. RANKE.GT.N ) THEN
         INFO = -6
      ELSE IF( RNKA22.LT.0 .OR. RNKA22+RANKE.GT.N ) THEN
         INFO = -7
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE IF( LDE.LT.MAX( 1, N ) ) THEN
         INFO = -11
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -13
      ELSE IF( LDC.LT.MAX( 1, P ) ) THEN
         INFO = -15
      ELSE IF( ( COMPQ .AND. LDQ.LT.N ) .OR. LDQ.LT.1 ) THEN
         INFO = -17
      ELSE IF( ( COMPZ .AND. LDZ.LT.N ) .OR. LDZ.LT.1 ) THEN
         INFO = -19
      ELSE IF( TOL.GE.ONE ) THEN
         INFO = -23
      ELSE
         LQUERY = ( LDWORK.EQ.-1 )
C
         ND  = N  - RANKE
         RO1 = ND
         IF( RANKE.EQ.N ) THEN
            MINWRK = 1
         ELSE
            MINWRK = MAX( 4*ND - 1, RO1 + MAX( N, M ) )
         END IF
         IF( LQUERY ) THEN
            CALL DORMQR( 'Left', 'Transpose', RO1, RANKE, ND,
     $                   A, LDA, DWORK, A, LDA, DWORK, -1, INFO )
            WRKOPT = MAX( MINWRK, INT( DWORK(1) ) + ND )
            CALL DORMQR( 'Left', 'Transpose', RO1, M, ND, A, LDA,
     $                   DWORK, B, LDB, DWORK, -1, INFO )
            WRKOPT = MAX( WRKOPT, INT( DWORK(1) ) + ND )
            IF( COMPQ ) THEN
               CALL DORMQR( 'Right', 'No-Transpose', N, RO1, ND, A,
     $                      LDA, DWORK, Q, LDQ, DWORK, -1, INFO )
               WRKOPT = MAX( WRKOPT, INT( DWORK(1) ) + ND )
            END IF
         ELSE IF( LDWORK.LT.MINWRK ) THEN
            INFO = -26
         END IF
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'TG01LY', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         DWORK(1) = WRKOPT
         RETURN
      END IF
C
C     Initialize output variables.
C
      NF = RANKE
      NIBLCK = 0
C
C     Quick return if possible.
C
      IF( RANKE.EQ.N ) THEN
         DWORK(1) = ONE
         RETURN
      END IF
C
C     Trivial rank check.
C
      IF( RANKE.EQ.0 .AND. RNKA22.EQ.0 ) THEN
         INFO = 1
         RETURN
      END IF
C
C     Skip reduction if A22 has rank N-RANKE.
C
      WRKOPT = MINWRK
      IF( RNKA22.EQ.ND )
     $   GO TO 110
C
      TOLDEF = TOL
      IF( TOLDEF.LE.ZERO ) THEN
C
C        Use the default tolerance in rank determination.
C
         TOLDEF = DBLE( N*N ) * DLAMCH( 'Precision' )
      END IF
C
C     Permute block columns to put E in the form  E = ( 0 E1 )
C                                                     ( 0 0  )
C
C     and define accordingly  A = ( B1 A1 ).
C                                 ( D1 C1 )
C
      MM1 = ND + 1
      CALL DLACPY( 'Full', N, ND, A(1,RANKE+1), LDA, E(1,RANKE+1), LDE )
      IF( RANKE.LE.ND ) THEN
         CALL DLACPY( 'Full', N, RANKE, A, LDA, A(1,MM1), LDA )
      ELSE
         K = MOD( RANKE, ND )
         DO 10 I = N-2*ND+1, K+1, -ND
            CALL DLACPY( 'Full', N, ND, A(1,I), LDA, A(1,I+ND), LDA )
   10    CONTINUE
         IF( K.NE.0 )
     $      CALL DLACPY( 'Full', N, K, A, LDA, A(1,MM1), LDA )
      END IF
      CALL DLACPY( 'Full', N, ND, E(1,RANKE+1), LDE, A, LDA )
      IF( COMPZ ) THEN
         CALL DLACPY( 'Full', N, ND, Z(1,RANKE+1), LDZ, E(1,RANKE+1),
     $                LDE )
         IF( RANKE.LE.ND ) THEN
            CALL DLACPY( 'Full', N, RANKE, Z, LDZ, Z(1,MM1), LDZ )
         ELSE
            DO 20 I = N-2*ND+1, K+1, -ND
               CALL DLACPY( 'Full', N, ND, Z(1,I), LDZ, Z(1,I+ND), LDZ )
   20       CONTINUE
            IF( K.NE.0 )
     $         CALL DLACPY( 'Full', N, K, Z, LDZ, Z(1,MM1), LDZ )
         END IF
         CALL DLACPY( 'Full', N, ND, E(1,RANKE+1), LDE, Z, LDZ )
      END IF
      IF( P.LE.N ) THEN
         CALL DLACPY( 'Full', P, ND, C(1,RANKE+1), LDC, E(1,RANKE+1),
     $                LDE )
         IF( RANKE.LE.ND ) THEN
            CALL DLACPY( 'Full', P, RANKE, C, LDC, C(1,MM1), LDC )
         ELSE
            DO 30 I = N-2*ND+1, K+1, -ND
               CALL DLACPY( 'Full', P, ND, C(1,I), LDC, C(1,I+ND), LDC )
   30       CONTINUE
            IF( K.NE.0 )
     $         CALL DLACPY( 'Full', P, K, C, LDC, C(1,MM1), LDC )
         END IF
         CALL DLACPY( 'Full', P, ND, E(1,RANKE+1), LDE, C, LDC )
      ELSE
         DO 40 I = 1, P
            CALL DCOPY( ND, C(I,RANKE+1), LDC, DWORK, 1 )
            CALL DCOPY( RANKE, C(I,1), LDC, DWORK(ND+1), 1 )
            CALL DCOPY( N, DWORK, 1, C(I,1), LDC )
   40    CONTINUE
      END IF
      IF( RANKE.LE.ND ) THEN
         CALL DLACPY( 'Full', N, RANKE, E, LDE, E(1,MM1), LDE )
      ELSE
         DO 50 I = N-2*ND+1, K+1, -ND
            CALL DLACPY( 'Full', N, ND, E(1,I), LDE, E(1,I+ND), LDE )
   50    CONTINUE
         IF( K.NE.0 )
     $      CALL DLACPY( 'Full', N, K, E, LDE, E(1,MM1), LDE )
      END IF
      CALL DLASET( 'Full', N, ND, ZERO, ZERO, E, LDE )
C
C     Set the estimate of the maximum singular value of A to ||A||_F.
C
      SVAL(1) = DLANGE( 'Frobenius', RANKE, RNKA22, A, LDA, DWORK )
      SVAL(2) = DLANTR( 'Frobenius', 'Upper', 'Non-Unit', RNKA22,
     $                  RNKA22, A(RANKE+1,1), LDA, DWORK )
      SVAL(3) = DLANGE( 'Frobenius', RANKE+RNKA22, ND-RNKA22,
     $                  A(1,RNKA22+1), LDA, DWORK )
      SVLMAX  = DLAPY2( DNRM2( 3, SVAL, 1 ),
     $                  DLANGE( 'Frobenius', N, RANKE, A(1,ND+1), LDA,
     $                          DWORK ) )/DBLE( N )
C
C     The D1 matrix is (RO+SIGMA)-by-(RO+SIGMA), where RO = ND - SIGMA
C     and SIGMA = 0. At each iteration the leading (RO+SIGMA)-by-SIGMA
C     submatrix of D1 has full column rank, with the trailing
C     SIGMA-by-SIGMA submatrix upper triangular.
C
      RO = ND
C
      SIGMA  = 0
      FIRST  = .TRUE.
      ITAU   = 1
      JWORK1 = ITAU   + ND
      JWORK2 = JWORK1 + 1
      DUM(1) = ZERO
C
   60 CONTINUE
      IF( FIRST ) THEN
         RO1 = ND - RNKA22
      ELSE
C
C        (NF+1,1) points to the current position of matrix D1.
C
         RO1 = RO
C
C        Compress columns of D1; first exploit the trapezoidal shape of
C        the (RO+SIGMA)-by-SIGMA matrix in the first SIGMA columns of D1;
C        compress the first SIGMA columns without column pivoting:
C
C              ( x x x x x )       ( x x x x x )
C              ( x x x x x )       ( 0 x x x x )
C              ( x x x x x )  - >  ( 0 0 x x x )
C              ( 0 x x x x )       ( 0 0 0 x x )
C              ( 0 0 x x x )       ( 0 0 0 x x )
C
C        where SIGMA = 3 and RO = 2.
C        Workspace: need  MAX( N, M ).
C
         IROW = NF
         DO 70 ICOL = 1, SIGMA
            IROW = IROW + 1
            CALL DLARFG( RO+1, A(IROW,ICOL), A(IROW+1,ICOL), 1, T )
            CALL SLCT_DLATZM( 'L', RO+1, N-ICOL, A(IROW+1,ICOL), 1, T,
     $                   A(IROW,ICOL+1), A(IROW+1,ICOL+1), LDA, DWORK )
            CALL SLCT_DLATZM( 'L', RO+1, RANKE, A(IROW+1,ICOL), 1, T,
     $                   E(IROW,ND+1), E(IROW+1,ND+1), LDE, DWORK )
            IF( COMPQ )
     $         CALL SLCT_DLATZM( 'R', N, RO+1, A(IROW+1,ICOL), 1, T,
     $                      Q(1, IROW), Q(1, IROW+1), LDQ, DWORK )
            CALL SLCT_DLATZM( 'L', RO+1, M, A(IROW+1,ICOL), 1, T,
     $                   B(IROW,1), B(IROW+1,1), LDB, DWORK )
            CALL DCOPY( ND-ICOL, DUM, 0, A(IROW+1, ICOL), 1 )
   70    CONTINUE
C
C        Continue with Householder with column pivoting.
C
C              ( x x x x x )       ( x x x x x )
C              ( 0 x x x x )       ( 0 x x x x )
C              ( 0 0 x x x )  ->   ( 0 0 x x x ) .
C              ( 0 0 0 x x )       ( 0 0 0 x x )
C              ( 0 0 0 x x )       ( 0 0 0 0 0 )
C
C                             ( D11 D12 )
C        Reduce further D1 to (  0  D22 ), where D22 is full
C                             (  0   0  )
C        row rank upper triangular.
C        Real workspace:    need  4*ND - 1;
C        Integer workspace: need  ND.
C
         IROW = MIN( NF+SIGMA+1, N  )
         ICOL = MIN(    SIGMA+1, ND )
         CALL MB03OY( RO1, ND-SIGMA, A(IROW,ICOL), LDA, TOLDEF, SVLMAX,
     $                RANK, SVAL, IWORK, DWORK(ITAU), DWORK(JWORK1),
     $                INFO )
C
C        Apply the column permutations to D12 and to the corresponding
C        columns of B1.
C
         CALL DLAPMT( .TRUE., NF+SIGMA, ND-SIGMA, A(1,ICOL), LDA,
     $                IWORK )
         CALL DLAPMT( .TRUE., P, ND-SIGMA, C(1,ICOL), LDC, IWORK )
         IF( COMPZ )
     $      CALL DLAPMT( .TRUE., N, ND-SIGMA, Z(1,ICOL), LDZ, IWORK )
C
         IF( RANK.GT.0 ) THEN
C
C           Apply the Householder transformations to the submatrix C1
C                                            ( C11 )
C           and define the transformed C1 as ( C21 ).
C                                            ( C31 )
C           Workspace: need    RANK + MAX(N,M);
C                      prefer  RANK + MAX(N,M)*NB.
C
            CALL DORMQR( 'Left', 'Transpose', RO1, RANKE, RANK,
     $                   A(IROW,ICOL), LDA, DWORK(ITAU), A(IROW,MM1),
     $                   LDA, DWORK(JWORK1), LDWORK-JWORK1+1, INFO )
            WRKOPT = MAX( WRKOPT, INT( DWORK(JWORK1) ) + JWORK1 - 1 )
            CALL DORMQR( 'Left', 'Transpose', RO1, M, RANK,
     $                   A(IROW,ICOL), LDA, DWORK(ITAU), B(IROW,1), LDB,
     $                   DWORK(JWORK1), LDWORK-JWORK1+1, INFO )
            WRKOPT = MAX( WRKOPT, INT( DWORK(JWORK1) ) + JWORK1 - 1 )
            CALL DORMQR( 'Left', 'Transpose', RO1, RANKE, RANK,
     $                   A(IROW,ICOL), LDA, DWORK(ITAU), E(IROW,MM1),
     $                   LDE, DWORK(JWORK1), LDWORK-JWORK1+1, INFO )
            WRKOPT = MAX( WRKOPT, INT( DWORK(JWORK1) ) + JWORK1 - 1 )
            IF( COMPQ ) THEN
               CALL DORMQR( 'Right', 'No-Transpose', N, RO1, RANK,
     $                      A(IROW,ICOL), LDA, DWORK(ITAU), Q(1,IROW),
     $                      LDQ, DWORK(JWORK1), LDWORK-JWORK1+1, INFO )
               WRKOPT = MAX( WRKOPT, INT( DWORK(JWORK1) ) + JWORK1 - 1 )
            END IF
            CALL DLASET( 'Lower', RO1-1, MIN( RO1-1, RANK ), ZERO,
     $                   ZERO, A(MIN( IROW+1, N ),ICOL), LDA )
            RO1 = RO1 - RANK
         END IF
      END IF
C
C     Terminate if D1 has maximal row rank.
C
      IF( RO1.GT.0 ) THEN
C
C        Update SIGMA and number of blocks.
C
         SIGMA  = ND - RO1
         NIBLCK = NIBLCK + 1
C
C        Compress the columns of current C31 to separate a RO1-by-RO1
C        invertible block.
C        Perform RQ-decomposition on the current C31 while keeping E1
C        upper triangular. No row pivoting is necessary since the
C        pencil is assumed regular. Still check regularity at first
C        iteration.
C        The current C31 is the RO1-by-NF matrix delimited by rows
C        NF+SIGMA+1 to NF+SIGMA+RO1 and columns ND+1 to ND+NF of A1.
C        The rank of current C21 is checked only for the first iteration.
C
C        IPIV will point to the current pivot position in C31.
C
         IPIV = NF + ND + 1
         N1   = NF + 1
         DO 90 I = 1, RO1
            IPIV = IPIV - 1
            N1   = N1   - 1
C
C           Zero elements left to A(IPIV,IPIV).
C
            DO 80 K = 1, N1-1
               J = ND + K
C
C              Rotate columns J, J+1 to zero A(IPIV,J).
C
               T = A(IPIV,J+1)
               CALL DLARTG( T, A(IPIV,J), CO, SI, A(IPIV,J+1) )
               A(IPIV,J) = ZERO
               CALL DROT( IPIV-1, A(1,J+1), 1, A(1,J), 1, CO, SI )
               CALL DROT( K+1, E(1,J+1), 1, E(1,J), 1, CO, SI )
               CALL DROT( P, C(1,J+1), 1, C(1,J), 1, CO, SI )
               IF( COMPZ )
     $            CALL DROT( N, Z(1,J+1), 1, Z(1,J), 1, CO, SI )
C
C              Rotate rows K, K+1 to zero E(K+1,J).
C
               T = E(K,J)
               CALL DLARTG( T, E(K+1,J), CO, SI, E(K,J) )
               E(K+1,J) = ZERO
               CALL DROT( N-J, E(K,J+1), LDE, E(K+1,J+1), LDE, CO, SI )
               CALL DROT( N, A(K,1), LDA, A(K+1,1), LDA, CO, SI )
               CALL DROT( M, B(K,1), LDB, B(K+1,1), LDB, CO, SI )
               IF( COMPQ )
     $            CALL DROT( N, Q(1,K), 1, Q(1,K+1), 1, CO, SI )
   80       CONTINUE
C
   90    CONTINUE
C
C        Check regularity of the pencil.
C        Real workspace:    need 3*RO1.
C        Integer workspace: need RO1.
C
         IF( DLANTR( 'Frobenius', 'Upper', 'Non-Unit', RO1, RO1,
     $                A(IPIV,IPIV), LDA, DWORK ) .LE. TOLDEF*SVLMAX )
     $         THEN
            INFO = 1
         ELSE
            CALL DTRCON( '1-norm', 'Upper', 'Non-unit', RO1,
     $                    A(IPIV,IPIV), LDA, RCOND, DWORK, IWORK, INFO )
            IF( RCOND.LE.TOLDEF )
     $         INFO = 1
         END IF
C
C        Return with error for non-regular system.
C
         IF( INFO.NE.0 )
     $      RETURN
C
         NF = NF - RO1
C
C        Set the order of i-th block.
C
         IBLCK(NIBLCK) = RO1
         RO = RO1
         FIRST = .FALSE.
C
         GO TO 60
      END IF
C
      IF( NF.GT.0 ) THEN
C
C        Permute block columns to put A and E in the form
C
C               ( A1 B1  *  )       ( E1 0  *  )
C           A = ( C1 D1  *  ),  E = ( 0  0  *  ) .
C               ( 0   0  Ai )       ( 0  0  Ei )
C
         NR = NF + ND
         DUM(1) = ZERO
         CALL DLACPY( 'Full', NR, ND, A, LDA, E, LDE )
         CALL DLACPY( 'Full', NR, NF, A(1,ND+1), LDA, A, LDA )
         CALL DLACPY( 'Full', NR, ND, E, LDE, A(1,NF+1), LDA )
         IF( COMPZ ) THEN
            CALL DLACPY( 'Full', N, ND, Z, LDZ, E, LDE )
            CALL DLACPY( 'Full', N, NF, Z(1,ND+1), LDZ, Z, LDZ )
            CALL DLACPY( 'Full', N, ND, E, LDE, Z(1,NF+1), LDZ )
         END IF
         IF( P.LE.N ) THEN
            CALL DLACPY( 'Full', P, ND, C, LDC, E, LDE )
            CALL DLACPY( 'Full', P, NF, C(1,ND+1), LDC, C, LDC )
            CALL DLACPY( 'Full', P, ND, E, LDE, C(1,NF+1), LDC )
         ELSE
            DO 100 I = 1, P
               CALL DCOPY( ND, C(I,1), LDC, DWORK, 1 )
               CALL DCOPY( NF, C(I,ND+1), LDC, C(I,1), LDC )
               CALL DCOPY( ND, DWORK, 1, C(I,NF+1), LDC )
  100       CONTINUE
         END IF
         CALL DLACPY( 'Full', N, NF, E(1,ND+1), LDE, E, LDE )
         CALL DLASET( 'Full', N, ND, ZERO, ZERO, E(1,NF+1), LDE )
         CALL DLASET( 'Full', N-NF-ND, NF+ND, ZERO, ZERO,
     $                A(NF+ND+1,1), LDA )
      END IF
C
C     Annihilate C1 keeping E upper triangular, to obtain
C
C             ( Af  *    *  )       ( Ef *  *  )
C         A = ( 0  A0,0  *  ),  E = ( 0  0  *  ) .
C             ( 0   0    Ai )       ( 0  0  Ei )
C
  110 CONTINUE
      I1 = NF + ND
      DO 130 I0 = ND, 1, -1
C
C        Annihilate elements A(I1,1), ..., A(I1,NF) using A(I1,I1)
C        as pivot element; E remains further upper triangular.
C
         K = 1
         DO 120 J = 1, NF
C
C           Rotate columns I1 and J to zero A(I1,J).
C
            T = A(I1,I1)
            CALL DLARTG( T, A(I1,J), CO, SI, A(I1,I1) )
            A(I1,J) = ZERO
            CALL DROT( I1-1, A(1,I1), 1, A(1,J), 1, CO, SI )
            CALL DROT( K, E(1,I1), 1, E(1,J), 1, CO, SI )
            CALL DROT( P, C(1,I1), 1, C(1,J), 1, CO, SI )
            IF( COMPZ )
     $         CALL DROT( N, Z(1,I1), 1, Z(1,J), 1, CO, SI )
            K = K + 1
  120    CONTINUE
         I1 = I1 - 1
  130 CONTINUE
C
      DWORK(1) = WRKOPT
      RETURN
C *** Last line of TG01LY ***
      END

