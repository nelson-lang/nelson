      SUBROUTINE MB01OO( UPLO, TRANS, N, H, LDH, X, LDX, E, LDE, P, LDP,
     $                   INFO )
C
C     PURPOSE
C
C     To compute either P or P', with P defined by the matrix formula
C
C        P = op( H )*X*op( E )',
C
C     where H is an upper Hessenberg matrix, X is a symmetric matrix,
C     E is an upper triangular matrix, and op( M ) is one of
C
C        op( M ) = M   or   op( M ) = M'.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     UPLO    CHARACTER*1
C             Specifies which triangle of the symmetric matrix X is
C             given as follows:
C             = 'U':  the upper triangular part is given;
C             = 'L':  the lower triangular part is given.
C
C     TRANS   CHARACTER*1
C             Specifies the operation to be performed as follows:
C             = 'N':         compute P  = H*X*E';
C             = 'T' or 'C':  compute P' = E'*X*H.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrices H, X, E, and P.  N >= 0.
C
C     H       (input) DOUBLE PRECISION array, dimension (LDH,N)
C             On entry, the leading N-by-N upper Hessenberg part of this
C             array must contain the upper Hessenberg matrix H.
C             The remaining part of this array is not referenced.
C
C     LDH     INTEGER
C             The leading dimension of the array H.  LDH >= MAX(1,N).
C
C     X       (input) DOUBLE PRECISION array, dimension (LDX,N)
C             On entry, if UPLO = 'U', the leading N-by-N upper
C             triangular part of this array must contain the upper
C             triangular part of the symmetric matrix X and the strictly
C             lower triangular part of the array is not referenced.
C             On entry, if UPLO = 'L', the leading N-by-N lower
C             triangular part of this array must contain the lower
C             triangular part of the symmetric matrix X and the strictly
C             upper triangular part of the array is not referenced.
C
C     LDX     INTEGER
C             The leading dimension of the array X.  LDX >= MAX(1,N).
C
C     E       (input) DOUBLE PRECISION array, dimension (LDE,N)
C             On entry, the leading N-by-N upper triangular part of this
C             array must contain the upper triangular matrix E.
C             The remaining part of this array is not referenced.
C
C     LDE     INTEGER
C             The leading dimension of array E.  LDE >= MAX(1,N).
C
C     P       (output) DOUBLE PRECISION array, dimension (LDP,N)
C             On exit, the leading N-by-N part of this array contains
C             the computed matrix P  = H*X*E', if TRANS = 'N', or
C             the computed matrix P' = E'*X*H, if TRANS = 'T'.
C
C     LDP     INTEGER
C             The leading dimension of the array P.  LDP >= MAX(1,N).
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -k, the k-th argument had an illegal
C                   value.
C
C     METHOD
C
C     The matrix expression is efficiently evaluated taking the
C     structure into account, and using BLAS and SLICOT routines.
C     Let W = H*X, or W = X*H, computed using SLICOT Library routine
C     MB01OS. The result is then obtained calling BLAS 3 routine DTRMM.
C
C     NUMERICAL ASPECTS
C
C     The algorithm requires approximately N**3 operations.
C
C     CONTRIBUTORS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Apr. 2019.
C
C     REVISIONS
C
C     -
C
C     KEYWORDS
C
C     Elementary matrix operations, matrix algebra, matrix operations.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ONE
      PARAMETER         ( ONE = 1.0D0 )
C     .. Scalar Arguments ..
      INTEGER           INFO, LDE, LDH, LDP, LDX, N
      CHARACTER         TRANS, UPLO
C     .. Array Arguments ..
      DOUBLE PRECISION  E(LDE,*), H(LDH,*), P(LDP,*), X(LDX,*)
C     .. Local Scalars ..
      LOGICAL           LTRANS, LUPLO
      CHARACTER         SIDE
C     .. External Functions ..
      LOGICAL           LSAME
      EXTERNAL          LSAME
C     .. External Subroutines ..
      EXTERNAL          DTRMM, MB01OS, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         MAX
C     .. Executable Statements ..
C
C     Test the input scalar arguments.
C
      INFO   = 0
      LUPLO  = LSAME( UPLO, 'U' )
      LTRANS = LSAME( TRANS, 'T' ) .OR. LSAME( TRANS, 'C' )
C
      IF ( ( .NOT.LUPLO ).AND.( .NOT.LSAME( UPLO, 'L' ) ) ) THEN
         INFO = -1
       ELSE IF ( ( .NOT.LTRANS ).AND.( .NOT.LSAME( TRANS, 'N' ) ) ) THEN
         INFO = -2
      ELSE IF ( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDH.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF ( LDX.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF ( LDE.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE IF ( LDP.LT.MAX( 1, N ) ) THEN
         INFO = -11
      END IF
C
      IF ( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'MB01OO', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF ( N.EQ.0 )
     $   RETURN
C
C     Compute W := H*X, if TRANS = 'N'.
C     Compute W := X*H, if TRANS = 'T'.
C
      CALL MB01OS( UPLO, TRANS, N, H, LDH, X, LDX, P, LDP, INFO )
C
C     Compute P = W*E' = H*X*E', if TRANS = 'N', or
C     compute P = E'*W = E'*X*H, if TRANS = 'T'.
C
      IF ( LTRANS ) THEN
         SIDE = 'Left'
      ELSE
         SIDE = 'Right'
      END IF
C
      CALL DTRMM( SIDE, 'Upper', 'Tran', 'NoDiag', N, N, ONE, E, LDE, P,
     $            LDP )
C
      RETURN
C *** Last line of MB01OO ***
      END
