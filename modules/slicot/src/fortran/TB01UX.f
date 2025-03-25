      SUBROUTINE TB01UX( COMPZ, N, M, P, A, LDA, B, LDB, C, LDC, Z, LDZ,
     $                   NOBSV, NLBLCK, CTAU, TOL, IWORK, DWORK, INFO )
C
C     PURPOSE
C
C     To compute an orthogonal transformation matrix Z which reduces the
C     N-th order system (A,B,C) to the form
C
C                ( Ano  * )               ( Bno )
C       Z'*A*Z = (        ) ,      Z'*B = (     ) ,
C                ( 0   Ao )               ( Bo  )
C
C          C*Z = ( 0   Co ) ,
C
C     where the NOBSV-th order system (Ao,Bo,Co) is observable.
C     The matrix Ano of order N-NOBSV contains the unobservable
C     eigenvalues of A.
C
C     The pencil ( Ao-lambda*I ) has full column rank NOBSV for all
C                (      Co     )
C     lambda, and is in a staircase form, with
C                     _      _            _      _
C                   ( Ak,k   Ak,k-1   ... Ak,2   Ak,1   )
C                   ( _      _            _      _      )
C       ( Ao ) =    ( Ak-1,k Ak-1,k-1 ... Ak-1,2 Ak-1,1 ) ,          (1)
C       ( Co )      (  :       :      ... _ :    _ :    )
C                   (  0       0      ... A1,2   A1,1   )
C                   (                            _      )
C                   (  0       0      ... 0      A0,1   )
C           _
C     where Ai-1,i is a CTAU(i-1)-by-CTAU(i) full column rank matrix
C     (with CTAU(0) = P).
C
C     The orthogonal transformation Z, performed to reduce the system
C     matrices, can be optionally accumulated.
C
C     The reduced order system (Ao,Bo,Co) has the same transfer-function
C     matrix as the original system (A,B,C).
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     COMPZ   CHARACTER*1
C             = 'N':  do not compute Z;
C             = 'I':  Z is initialized to the unit matrix, and the
C                     orthogonal matrix Z is returned.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The dimension of the system state vector; also the order
C             of the square matrix A, the number of rows of the matrix B
C             and the number of columns of the matrix C.  N >= 0.
C
C     M       (input) INTEGER
C             The dimension of system input vector; also the number of
C             columns of the matrix B.  M >= 0.
C
C     P       (input) INTEGER
C             The dimension of system output vector; also the number of
C             rows of the matrix C.  P >= 0.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the N-by-N state matrix A.
C             On exit, the leading N-by-N part of this array contains
C             the transformed state matrix Z'*A*Z,
C
C                                ( Ano  *  )
C                       Z'*A*Z = (         ) ,
C                                ( 0    Ao )
C
C             where Ao is NOBSV-by-NOBSV and Ano is
C             (N-NOBSV)-by-(N-NOBSV).
C             The matrix ( Ao ) is in the observability staircase
C                        ( Co )
C             form (1).
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     B       (input/output) DOUBLE PRECISION array, dimension
C             (LDB,MAX(M,P))
C             On entry, the leading N-by-M part of this array must
C             contain the N-by-M input matrix B.
C             On exit, the leading N-by-M part of this array contains
C             the transformed input matrix Z'*B.
C
C     LDB     INTEGER
C             The leading dimension of the array B.
C             LDB >= MAX(1,N) if M > 0 or  P > 0;
C             LDB >= 1        if M = 0 and P = 0.
C
C     C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
C             On entry, the leading P-by-N part of this array must
C             contain the state/output matrix C.
C             On exit, the leading P-by-N part of this array contains
C             the transformed matrix
C
C                     C*Z = (  0   Co ) ,
C
C             where Co is P-by-NOBSV.          
C             The matrix ( Ao ) is in the observability staircase
C                        ( Co )
C             form (1).
C
C     LDC     INTEGER
C             The leading dimension of the array C.
C             LDC >= MAX(1,M,P) if N > 0;
C             LDC >= 1          if N = 0.
C
C     Z       (input/output) DOUBLE PRECISION array, dimension (LDZ,*)
C             If COMPZ = 'N': Z is not referenced.
C             If COMPZ = 'I': on entry, Z need not be set;
C                             on exit, the leading N-by-N part of this
C                             array contains the orthogonal matrix Z,
C                             i.e., the product of the transformations
C                             applied to A and C on the right.
C
C     LDZ     INTEGER
C             The leading dimension of the array Z.
C             LDZ >= 1,        if COMPZ = 'N';
C             LDZ >= MAX(1,N), if COMPZ = 'I'.
C
C     NOBSV   (output) INTEGER
C             The order of the reduced matrix Ao, and the number of
C             columns of the reduced matrix Co; also, the order of the
C             observable part of the pair (C, A-lambda*I).
C
C     NLBLCK  (output) INTEGER                         _
C             The number k, of full column rank blocks Ai-1,i in the
C             staircase form of the pencil (Ao-lambda*I) (see (1)).
C                                          (     Co    )
C
C     CTAU    (output) INTEGER array, dimension (N)
C             CTAU(i), for i = 1, ..., NLBLCK, is the column dimension
C                                           _
C             of the full column rank block Ai-1,i in the staircase
C             form (1).
C
C     Tolerances
C
C     TOL     DOUBLE PRECISION
C             The tolerance to be used in rank determinations when
C             transforming the pair (A,C). If the user sets TOL > 0,
C             then the given value of TOL is used as a lower bound for
C             reciprocal condition numbers in rank determinations; a
C             (sub)matrix whose estimated condition number is less than
C             1/TOL is considered to be of full rank.  If the user sets
C             TOL <= 0, then an implicitly computed, default tolerance,
C             defined by  TOLDEF = N*N*EPS,  is used instead, where EPS
C             is the machine precision (see LAPACK Library routine
C             DLAMCH).  TOL < 1.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (P)
C
C     DWORK   DOUBLE PRECISION array, dimension (N+MAX(1, N, 3*P, M))
C             On exit, if INFO = 0, DWORK(1) returns the optimal value
C             of LDWORK.
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
C     The subroutine is based on the dual of the reduction
C     algorithms of [1].
C
C     REFERENCES
C
C     [1] Varga, A.
C         Computation of Irreducible Generalized State-Space
C         Realizations.
C         Kybernetika, vol. 26, pp. 89-106, 1990.
C
C     NUMERICAL ASPECTS
C
C     The algorithm is numerically backward stable and requires
C     0( N**3 )  floating point operations.
C
C     FURTHER COMMENTS
C
C     If the system matrices A and C are badly scaled, it is
C     generally recommendable to scale them with the SLICOT routine
C     TB01ID, before calling TG01UX.
C
C     CONTRIBUTOR
C
C     A. Varga, German Aerospace Center, DLR Oberpfaffenhofen.
C     March 2002.
C
C     REVISIONS
C
C     V. Sima, Dec. 2016.
C
C     KEYWORDS
C
C     Observability, minimal realization, orthogonal canonical form,
C     orthogonal transformation.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
C     .. Scalar Arguments ..
      CHARACTER          COMPZ
      INTEGER            INFO, LDA, LDB, LDC, LDZ, M, N, NLBLCK, NOBSV,
     $                   P
      DOUBLE PRECISION   TOL
C     .. Array Arguments ..
      INTEGER            CTAU( * ), IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, *  ),
     $                   DWORK( * ), Z( LDZ, * )
C     .. Local Scalars ..
      LOGICAL            ILZ
      INTEGER            LBA, LDWORK
C     .. Local Arrays ..
      DOUBLE PRECISION   DUM(1)
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     .. External Subroutines ..
      EXTERNAL           AB07MD, MA02BD, TB01UD, TB01XD, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC          MAX
C
C     .. Executable Statements ..
C
C     Decode COMPZ.
C
      ILZ = LSAME( COMPZ, 'I' )
C
C     Test the input scalar parameters.
C
      INFO = 0
      IF( .NOT.ILZ .AND. .NOT.LSAME( COMPZ, 'N' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( P.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF( LDB.LT.1 .OR. ( MAX( M, P ).GT.0 .AND. LDB.LT.N ) ) THEN
         INFO = -8
      ELSE IF( LDC.LT.1 .OR. ( LDC.LT.MAX( M, P ) .AND. N.GT.0 ) ) THEN
         INFO = -10
      ELSE IF( LDZ.LT.1 .OR. ( ILZ .AND. LDZ.LT.N ) ) THEN
         INFO = -12
      ELSE IF( TOL.GE.ONE ) THEN
         INFO = -16
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'TB01UX', -INFO )
         RETURN
      END IF
C
C     Build the dual system.
C
      CALL AB07MD( 'Zero D', N, M, P, A, LDA, B, LDB, C, LDC, DUM, 1,
     $             INFO )
C
      LDWORK = MAX( 1, N, 3*P, M )
      CALL TB01UD( COMPZ, N, P, M, A, LDA, B, LDB, C, LDC, NOBSV,
     $             NLBLCK, CTAU, Z, LDZ, DWORK, TOL, IWORK, DWORK(N+1),
     $             LDWORK, INFO )
      IF( NLBLCK.GT.1 ) THEN
         LBA = CTAU(1) + CTAU(2) - 1
      ELSE IF( NLBLCK.EQ.1 ) THEN
         LBA = CTAU(1) - 1
      ELSE
         LBA = 0
      END IF
C
C     Compute the pertransposed dual system exploiting matrix shapes.
C
      LBA = MAX( LBA, N-NOBSV-1 )
      CALL TB01XD( 'Zero D', N, P, M, LBA, MAX( 0, N-1 ), A, LDA, B,
     $             LDB, C, LDC, DUM, 1, INFO )
      IF( ILZ )
     $   CALL MA02BD( 'Right', N, N, Z, LDZ )
      DWORK(1) = DWORK(N+1)
      RETURN
C *** Last line of TB01UX ***
      END
