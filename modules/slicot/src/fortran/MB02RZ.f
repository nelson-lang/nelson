      SUBROUTINE MB02RZ( TRANS, N, NRHS, H, LDH, IPIV, B, LDB, INFO )
C
C     PURPOSE
C
C     To solve a system of linear equations
C        H * X = B,  H' * X = B  or  H**H * X = B
C     with a complex upper Hessenberg N-by-N matrix H using the LU
C     factorization computed by MB02SZ.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     TRANS   CHARACTER*1
C             Specifies the form of the system of equations:
C             = 'N':  H * X = B  (No transpose)
C             = 'T':  H'* X = B  (Transpose)
C             = 'C':  H**H * X = B  (Conjugate transpose)
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrix H.  N >= 0.
C
C     NRHS    (input) INTEGER
C             The number of right hand sides, i.e., the number of
C             columns of the matrix B.  NRHS >= 0.
C
C     H       (input) COMPLEX*16 array, dimension (LDH,N)
C             The factors L and U from the factorization H = P*L*U
C             as computed by MB02SZ.
C
C     LDH     INTEGER
C             The leading dimension of the array H.  LDH >= max(1,N).
C
C     IPIV    (input) INTEGER array, dimension (N)
C             The pivot indices from MB02SZ; for 1<=i<=N, row i of the
C             matrix was interchanged with row IPIV(i).
C
C     B       (input/output) COMPLEX*16 array, dimension (LDB,NRHS)
C             On entry, the right hand side matrix B.
C             On exit, the solution matrix X.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= max(1,N).
C
C     INFO    (output) INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value.
C
C     METHOD
C
C     The routine uses the factorization
C        H = P * L * U
C     where P is a permutation matrix, L is lower triangular with unit
C     diagonal elements (and one nonzero subdiagonal), and U is upper
C     triangular.
C
C     REFERENCES
C
C     -
C
C     NUMERICAL ASPECTS
C                                2
C     The algorithm requires 0( N x NRHS ) complex operations.
C
C     CONTRIBUTOR
C
C     Release 3.0: V. Sima, Katholieke Univ. Leuven, Belgium, Dec. 1996.
C     Supersedes Release 2.0 routine TB01FW by A.J. Laub, University of
C     Southern California, United States of America, May 1980.
C
C     REVISIONS
C
C     -
C
C     KEYWORDS
C
C     Frequency response, Hessenberg form, matrix algebra.
C
C     ******************************************************************
C
C     .. Parameters ..
      COMPLEX*16         ONE
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ) )
C     .. Scalar Arguments ..
      CHARACTER          TRANS
      INTEGER            INFO, LDB, LDH, N, NRHS
C     ..
C     .. Array Arguments ..
      INTEGER            IPIV( * )
      COMPLEX*16         B( LDB, * ), H( LDH, * )
C     .. Local Scalars ..
      LOGICAL            NOTRAN
      INTEGER            J, JP
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     .. External Subroutines ..
      EXTERNAL           XERBLA, ZAXPY, ZSWAP, ZTRSM
C     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, MAX
C     .. Executable Statements ..
C
C     Test the input parameters.
C
      INFO = 0
      NOTRAN = LSAME( TRANS, 'N' )
      IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) .AND. .NOT.
     $    LSAME( TRANS, 'C' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDH.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -8
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'MB02RZ', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( N.EQ.0 .OR. NRHS.EQ.0 )
     $   RETURN
C
      IF( NOTRAN ) THEN
C
C        Solve H * X = B.
C
C        Solve L * X = B, overwriting B with X.
C
C        L is represented as a product of permutations and unit lower
C        triangular matrices L = P(1) * L(1) * ... * P(n-1) * L(n-1),
C        where each transformation L(i) is a rank-one modification of
C        the identity matrix.
C
         DO 10 J = 1, N - 1
            JP = IPIV( J )
            IF( JP.NE.J )
     $         CALL ZSWAP( NRHS, B( JP, 1 ), LDB, B( J, 1 ), LDB )
            CALL ZAXPY( NRHS, -H( J+1, J ), B( J, 1 ), LDB, B( J+1, 1 ),
     $                  LDB )
   10    CONTINUE
C
C        Solve U * X = B, overwriting B with X.
C
         CALL ZTRSM( 'Left', 'Upper', 'No transpose', 'Non-unit', N,
     $               NRHS, ONE, H, LDH, B, LDB )
C
      ELSE IF( LSAME( TRANS, 'T' ) ) THEN
C
C        Solve H' * X = B.
C
C        Solve U' * X = B, overwriting B with X.
C
         CALL ZTRSM( 'Left', 'Upper', TRANS, 'Non-unit', N, NRHS, ONE,
     $               H, LDH, B, LDB )
C
C        Solve L' * X = B, overwriting B with X.
C
         DO 20 J = N - 1, 1, -1
            CALL ZAXPY( NRHS, -H( J+1, J ), B( J+1, 1 ), LDB, B( J, 1 ),
     $                  LDB )
            JP = IPIV( J )
            IF( JP.NE.J )
     $         CALL ZSWAP( NRHS, B( JP, 1 ), LDB, B( J, 1 ), LDB )
   20    CONTINUE
C
      ELSE
C
C        Solve H**H * X = B.
C
C        Solve U**H * X = B, overwriting B with X.
C
         CALL ZTRSM( 'Left', 'Upper', TRANS, 'Non-unit', N, NRHS, ONE,
     $               H, LDH, B, LDB )
C
C        Solve L**H * X = B, overwriting B with X.
C
         DO 30 J = N - 1, 1, -1
            CALL ZAXPY( NRHS, -DCONJG( H( J+1, J ) ), B( J+1, 1 ), LDB,
     $                  B( J, 1 ), LDB )
            JP = IPIV( J )
            IF( JP.NE.J )
     $         CALL ZSWAP( NRHS, B( JP, 1 ), LDB, B( J, 1 ), LDB )
   30    CONTINUE
C
      END IF
C
      RETURN
C *** Last line of MB02RZ ***
      END
