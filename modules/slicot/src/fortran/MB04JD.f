      SUBROUTINE MB04JD( N, M, P, L, A, LDA, B, LDB, TAU, DWORK, LDWORK,
     $                   INFO )
C
C     PURPOSE
C
C     To compute an LQ factorization of an n-by-m matrix A (A = L * Q),
C     having a min(n,p)-by-p zero triangle in the upper right-hand side
C     corner, as shown below, for n = 8, m = 7, and p = 2:
C
C            [ x x x x x 0 0 ]
C            [ x x x x x x 0 ]
C            [ x x x x x x x ]
C            [ x x x x x x x ]
C        A = [ x x x x x x x ],
C            [ x x x x x x x ]
C            [ x x x x x x x ]
C            [ x x x x x x x ]
C
C     and optionally apply the transformations to an l-by-m matrix B
C     (from the right). The problem structure is exploited. This
C     computation is useful, for instance, in combined measurement and
C     time update of one iteration of the time-invariant Kalman filter
C     (square root covariance filter).
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The number of rows of the matrix A.  N >= 0.
C
C     M       (input) INTEGER
C             The number of columns of the matrix A.  M >= 0.
C
C     P       (input) INTEGER
C             The order of the zero triagle.  P >= 0.
C
C     L       (input) INTEGER
C             The number of rows of the matrix B.  L >= 0.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,M)
C             On entry, the leading N-by-M part of this array must
C             contain the matrix A. The elements corresponding to the
C             zero MIN(N,P)-by-P upper trapezoidal/triangular part
C             (if P > 0) are not referenced.
C             On exit, the elements on and below the diagonal of this
C             array contain the N-by-MIN(N,M) lower trapezoidal matrix
C             L (L is lower triangular, if N <= M) of the LQ
C             factorization, and the relevant elements above the
C             diagonal contain the trailing components (the vectors v,
C             see Method) of the elementary reflectors used in the
C             factorization.
C
C     LDA     INTEGER
C             The leading dimension of array A.  LDA >= MAX(1,N).
C
C     B       (input/output) DOUBLE PRECISION array, dimension (LDB,M)
C             On entry, the leading L-by-M part of this array must
C             contain the matrix B.
C             On exit, the leading L-by-M part of this array contains
C             the updated matrix B.
C             If L = 0, this array is not referenced.
C
C     LDB     INTEGER
C             The leading dimension of array B.  LDB >= MAX(1,L).
C
C     TAU     (output) DOUBLE PRECISION array, dimension MIN(N,M)
C             The scalar factors of the elementary reflectors used.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal value
C             of LDWORK.
C
C     LDWORK  The length of the array DWORK.
C             LDWORK >= MAX(1,N-1,N-P,L).
C             For optimum performance LDWORK should be larger.
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
C     The routine uses min(N,M) Householder transformations exploiting
C     the zero pattern of the matrix.  A Householder matrix has the form
C
C                                     ( 1 ),
C        H  = I - tau *u *u',    u  = ( v )
C         i          i  i  i      i   (  i)
C
C     where v  is an (M-P+I-2)-vector.  The components of v  are stored
C            i                                             i
C     in the i-th row of A, beginning from the location i+1, and tau
C                                                                   i
C     is stored in TAU(i).
C
C     NUMERICAL ASPECTS
C
C     The algorithm is backward stable.
C
C     CONTRIBUTORS
C
C     V. Sima, Katholieke Univ. Leuven, Belgium, Feb. 1997.
C
C     REVISIONS
C
C     -
C
C     KEYWORDS
C
C     Elementary reflector, LQ factorization, orthogonal transformation.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0 )
C     .. Scalar Arguments ..
      INTEGER           INFO, L, LDA, LDB, LDWORK, M, N, P
C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), B(LDB,*), DWORK(*), TAU(*)
C     .. Local Scalars ..
      INTEGER           I
      DOUBLE PRECISION  FIRST, WRKOPT
C     .. External Subroutines ..
      EXTERNAL          DGELQF, DLARF, DLARFG, DORMLQ, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         DBLE, MAX, MIN
C     .. Executable Statements ..
C
C     Test the input scalar arguments.
C
      INFO = 0
      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( M.LT.0 ) THEN
         INFO = -2
      ELSE IF( P.LT.0 ) THEN
         INFO = -3
      ELSE IF( L.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF( LDB.LT.MAX( 1, L ) ) THEN
         INFO = -8
      ELSE IF( LDWORK.LT.MAX( 1, N - 1, N - P, L ) ) THEN
         INFO = -11
      END IF
C
      IF ( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'MB04JD', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( MIN( M, N ).EQ.0 ) THEN
         DWORK(1) = ONE
         RETURN
      ELSE IF( M.LE.P+1 ) THEN
         DO 5 I = 1, MIN( N, M )
            TAU(I) = ZERO
    5    CONTINUE
         DWORK(1) = ONE
         RETURN
      END IF
C
C     Annihilate the superdiagonal elements of A and apply the
C     transformations to B, if L > 0.
C     Workspace: need MAX(N-1,L).
C
C     (Note: Comments in the code beginning "Workspace:" describe the
C     minimal amount of real workspace needed at that point in the
C     code, as well as the preferred amount for good performance.
C     NB refers to the optimal block size for the immediately
C     following subroutine, as returned by ILAENV.)
C
      DO 10 I = 1, MIN( N, P )
C
C        Exploit the structure of the I-th row of A.
C
         CALL DLARFG( M-P, A(I,I), A(I,I+1), LDA, TAU(I) )
         IF( TAU(I).NE.ZERO ) THEN
C
            FIRST = A(I,I)
            A(I,I) = ONE
C
            IF ( I.LT.N ) CALL DLARF( 'Right', N-I, M-P, A(I,I), LDA,
     $                                TAU(I), A(I+1,I), LDA, DWORK )
            IF ( L.GT.0 ) CALL DLARF( 'Right', L, M-P, A(I,I), LDA,
     $                                TAU(I), B(1,I), LDB, DWORK )
C
            A(I,I) = FIRST
         END IF
   10 CONTINUE
C
      WRKOPT = MAX( ONE, DBLE( N - 1 ), DBLE( L ) )
C
C     Fast LQ factorization of the remaining trailing submatrix, if any.
C     Workspace: need N-P;  prefer (N-P)*NB.
C
      IF( N.GT.P ) THEN
         CALL DGELQF( N-P, M-P, A(P+1,P+1), LDA, TAU(P+1), DWORK,
     $                LDWORK, INFO )
         WRKOPT = MAX( WRKOPT, DWORK(1) )
C
         IF ( L.GT.0 ) THEN
C
C           Apply the transformations to B.
C           Workspace: need L;  prefer L*NB.
C
            CALL DORMLQ( 'Right', 'Transpose', L, M-P, MIN(N,M)-P,
     $                   A(P+1,P+1), LDA, TAU(P+1), B(1,P+1), LDB,
     $                   DWORK, LDWORK, INFO )
            WRKOPT = MAX( WRKOPT, DWORK(1) )
         END IF
      END IF
C
      DWORK(1) = WRKOPT
      RETURN
C *** Last line of MB04JD ***
      END
