      SUBROUTINE MB01RT( UPLO, TRANS, N, ALPHA, BETA, R, LDR, E, LDE,
     $                   X, LDX, DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To compute the matrix formula
C
C        R := alpha*R + beta*op( E )*X*op( E )',
C
C     where alpha and beta are scalars, R and X are symmetric matrices,
C     E is an upper triangular matrix, and op( E ) is one of
C
C        op( E ) = E   or   op( E ) = E'.
C
C     The result is overwritten on R.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     UPLO    CHARACTER*1
C             Specifies which triangles of the symmetric matrices R
C             and X are given as follows:
C             = 'U':  the upper triangular part is given;
C             = 'L':  the lower triangular part is given.
C
C     TRANS   CHARACTER*1
C             Specifies the form of op( E ) to be used in the matrix
C             multiplication as follows:
C             = 'N':  op( E ) = E;
C             = 'T':  op( E ) = E';
C             = 'C':  op( E ) = E'.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrices R, E, and X.  N >= 0.
C
C     ALPHA   (input) DOUBLE PRECISION
C             The scalar alpha. When alpha is zero then R need not be
C             set before entry, except when R is identified with X in
C             the call.
C
C     BETA    (input) DOUBLE PRECISION
C             The scalar beta. When beta is zero then E and X are not
C             referenced.
C
C     R       (input/output) DOUBLE PRECISION array, dimension (LDR,N)
C             On entry with UPLO = 'U', the leading N-by-N upper
C             triangular part of this array must contain the upper
C             triangular part of the symmetric matrix R.
C             On entry with UPLO = 'L', the leading N-by-N lower
C             triangular part of this array must contain the lower
C             triangular part of the symmetric matrix R.
C             In both cases, the other strictly triangular part is not
C             referenced.
C             On exit, the leading N-by-N upper triangular part (if
C             UPLO = 'U'), or lower triangular part (if UPLO = 'L'), of
C             this array contains the corresponding triangular part of
C             the computed matrix R.
C
C     LDR     INTEGER
C             The leading dimension of array R.  LDR >= MAX(1,N).
C
C     E       (input) DOUBLE PRECISION array, dimension (LDE,N)
C             On entry, the leading N-by-N upper triangular part of this
C             array must contain the upper triangular matrix E.
C             The remaining part of this array is not referenced.
C
C     LDE     INTEGER
C             The leading dimension of array E.  LDE >= MAX(1,N).
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
C             The diagonal elements of this array are modified
C             internally, but are restored on exit.
C
C     LDX     INTEGER
C             The leading dimension of array X.  LDX >= MAX(1,N).
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             This array is not referenced when beta = 0, or N = 0.
C
C     LDWORK  The length of the array DWORK.
C             LDWORK >= N*N, if  beta <> 0;
C             LDWORK >= 0,   if  beta =  0.
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
C     The matrix expression is efficiently evaluated taking the symmetry
C     into account. Specifically, let X = U + L, with U and L upper and
C     lower triangular matrices, defined by
C
C        U = triu( X ) - (1/2)*diag( X ),
C        L = tril( X ) - (1/2)*diag( X ),
C
C     where triu, tril, and diag denote the upper triangular part, lower
C     triangular part, and diagonal part of X, respectively. Then,
C     if UPLO = 'U',
C
C        E*X*E' = ( E*U )*E' + E*( E*U )',  for TRANS = 'N',
C        E'*X*E = E'*( U*E ) + ( U*E )'*E,  for TRANS = 'T', or 'C',
C
C     and if UPLO = 'L',
C
C        E*X*E' = ( E*L' )*E' + E*( E*L' )',  for TRANS = 'N',
C        E'*X*E = E'*( L'*E ) + ( L'*E )'*E,  for TRANS = 'T', or 'C',
C
C     which involve operations like in BLAS 2 and 3 (DTRMV and DSYR2K).
C     This approach ensures that the matrices E*U, U*E, E*L', or L'*E
C     are upper triangular.
C
C     NUMERICAL ASPECTS
C
C     The algorithm requires approximately N**3/2 operations.
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
      DOUBLE PRECISION  ZERO, ONE, TWO, HALF
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0,
     $                  HALF = 0.5D0 )
C     .. Scalar Arguments ..
      CHARACTER         TRANS, UPLO
      INTEGER           INFO, LDE, LDR, LDWORK, LDX, N
      DOUBLE PRECISION  ALPHA, BETA
C     .. Array Arguments ..
      DOUBLE PRECISION  DWORK(*), E(LDE,*), R(LDR,*), X(LDX,*)
C     .. Local Scalars ..
      LOGICAL           LTRANS, LUPLO
      INTEGER           I, J
C     .. External Functions ..
      LOGICAL           LSAME
      EXTERNAL          LSAME
C     .. External Subroutines ..
      EXTERNAL          DAXPY, DCOPY, DLASCL, DLASET, DSCAL, DTRMV,
     $                  MB01OT, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         MAX
C     .. Executable Statements ..
C
C     Test the input scalar arguments.
C
      INFO = 0
      LUPLO  = LSAME( UPLO,  'U' )
      LTRANS = LSAME( TRANS, 'T' ) .OR. LSAME( TRANS, 'C' )
C
      IF(      ( .NOT.LUPLO  ).AND.( .NOT.LSAME( UPLO,  'L' ) ) )THEN
         INFO = -1
      ELSE IF( ( .NOT.LTRANS ).AND.( .NOT.LSAME( TRANS, 'N' ) ) )THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDR.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDE.LT.1 .OR. ( LTRANS .AND. LDE.LT.N ) .OR.
     $                  ( .NOT.LTRANS .AND. LDE.LT.N ) ) THEN
         INFO = -9
      ELSE IF( LDX.LT.MAX( 1, N ) ) THEN
         INFO = -11
      ELSE IF( ( BETA.NE.ZERO .AND. LDWORK.LT.N*N )
     $     .OR.( BETA.EQ.ZERO .AND. LDWORK.LT.0 ) ) THEN
         INFO = -13
      END IF
C
      IF ( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'MB01RT', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF ( N.EQ.0 )
     $   RETURN
C
      IF ( BETA.EQ.ZERO ) THEN
         IF ( ALPHA.EQ.ZERO ) THEN
C
C           Special case alpha = 0.
C
            CALL DLASET( UPLO, N, N, ZERO, ZERO, R, LDR )
         ELSE
C
C           Special case beta = 0.
C
            IF ( ALPHA.NE.ONE )
     $         CALL DLASCL( UPLO, 0, 0, ONE, ALPHA, N, N, R, LDR, INFO )
         END IF
         RETURN
      END IF
C
C     General case: beta <> 0.
C     Compute W = E*T or W = T*E in DWORK, and apply the updating
C     formula (see METHOD section).
C     Workspace: need N*N.
C
      CALL DSCAL( N, HALF, X, LDX+1 )
C
      IF ( .NOT.LTRANS ) THEN
C
         IF ( LUPLO ) THEN
C
            DO 10 J = 1, N
               CALL DCOPY( J, X(1,J), 1, DWORK(1+(J-1)*N), 1 )
               CALL DTRMV( UPLO, 'NoTran', 'NoDiag', J, E, LDE,
     $                     DWORK(1+(J-1)*N), 1 )
   10       CONTINUE
C
         ELSE
C
            DO 20 J = 1, N
               CALL DCOPY( J, X(J,1), LDX, DWORK(1+(J-1)*N), 1 )
               CALL DTRMV( 'Upper', 'NoTran', 'NoDiag', J, E, LDE,
     $                     DWORK(1+(J-1)*N), 1 )
   20       CONTINUE
C
         END IF
C
      ELSE
C
         IF ( LUPLO ) THEN
C
            DO 30 J = 1, N
               CALL DCOPY( J, E(1,J), 1, DWORK(1+(J-1)*N), 1 )
               CALL DTRMV( UPLO, 'NoTran', 'NoDiag', J, X, LDX,
     $                     DWORK(1+(J-1)*N), 1 )
   30       CONTINUE
C
         ELSE
C
            DO 40 J = 1, N
               CALL DCOPY( J, E(1,J), 1, DWORK(1+(J-1)*N), 1 )
               CALL DTRMV( UPLO, 'Tran', 'NoDiag', J, X, LDX,
     $                     DWORK(1+(J-1)*N), 1 )
   40       CONTINUE
C
         END IF
C
      END IF
C
      CALL DSCAL( N, TWO, X, LDX+1 )
C
      CALL MB01OT( UPLO, TRANS, N, ALPHA, BETA, R, LDR, E, LDE, DWORK,
     $             N )
C
      RETURN
C *** Last line of MB01RT ***
      END
