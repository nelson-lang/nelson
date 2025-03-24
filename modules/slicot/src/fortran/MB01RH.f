      SUBROUTINE MB01RH( UPLO, TRANS, N, ALPHA, BETA, R, LDR, H, LDH,
     $                   X, LDX, DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To compute the matrix formula
C
C        R := alpha*R + beta*op( H )*X*op( H )',
C
C     where alpha and beta are scalars, R and X are symmetric matrices,
C     H is an upper Hessenberg matrix, and op( H ) is one of
C
C        op( H ) = H   or   op( H ) = H'.
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
C             Specifies the form of op( H ) to be used in the matrix
C             multiplication as follows:
C             = 'N':  op( H ) = H;
C             = 'T':  op( H ) = H';
C             = 'C':  op( H ) = H'.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrices R, H, and X.  N >= 0.
C
C     ALPHA   (input) DOUBLE PRECISION
C             The scalar alpha. When alpha is zero then R need not be
C             set before entry, except when R is identified with X in
C             the call.
C
C     BETA    (input) DOUBLE PRECISION
C             The scalar beta. When beta is zero then H and X are not
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
C     H       (input) DOUBLE PRECISION array, dimension (LDH,N)
C             On entry, the leading N-by-N upper Hessenberg part of this
C             array must contain the upper Hessenberg matrix H.
C             If TRANS = 'N', the entries 3, 4,..., N of the first
C             column are modified internally, but are restored on exit.
C             The remaining part of this array is not referenced.
C
C     LDH     INTEGER
C             The leading dimension of array H.  LDH >= MAX(1,N).
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
C        H*X*H' = ( H*U )*H' + H*( H*U )',  for TRANS = 'N',
C        H'*X*H = H'*( U*H ) + ( U*H )'*H,  for TRANS = 'T', or 'C',
C
C     and if UPLO = 'L',
C
C        H*X*H' = ( H*L' )*H' + H*( H*L' )',  for TRANS = 'N',
C        H'*X*H = H'*( L'*H ) + ( L'*H )'*H,  for TRANS = 'T', or 'C',
C
C     which involve operations like in BLAS 2 and 3 (DTRMV and DSYR2K).
C     This approach ensures that the matrices H*U, U*H, H*L', or L'*H
C     are upper Hessenberg.
C
C     NUMERICAL ASPECTS
C
C     The algorithm requires approximately N**3/2 operations.
C
C     CONTRIBUTORS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Mar. 2019.
C
C     REVISIONS
C
C     V. Sima, Apr. 2019.
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
      INTEGER           INFO, LDH, LDR, LDWORK, LDX, N
      DOUBLE PRECISION  ALPHA, BETA
C     .. Array Arguments ..
      DOUBLE PRECISION  DWORK(*), H(LDH,*), R(LDR,*), X(LDX,*)
C     .. Local Scalars ..
      LOGICAL           LTRANS, LUPLO
      INTEGER           I, J, J1
C     .. External Functions ..
      LOGICAL           LSAME
      EXTERNAL          LSAME
C     .. External Subroutines ..
      EXTERNAL          DAXPY, DCOPY, DLASCL, DLASET, DSCAL, DSWAP,
     $                  DTRMV, MB01OH, XERBLA
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
      ELSE IF( LDH.LT.1 .OR. ( LTRANS .AND. LDH.LT.N ) .OR.
     $                  ( .NOT.LTRANS .AND. LDH.LT.N ) ) THEN
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
         CALL XERBLA( 'MB01RH', -INFO )
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
C     Compute W = H*T or W = T*H in DWORK, and apply the updating
C     formula (see METHOD section).
C     Workspace: need N*N.
C
      CALL DSCAL( N, HALF, X, LDX+1 )
C
      IF ( .NOT.LTRANS ) THEN
C
C        For convenience, swap the subdiagonal entries in H with
C        those in the first column, and finally restore them.
C
         IF ( N.GT.2 )
     $      CALL DSWAP( N-2, H(3,1), 1, H(3,2), LDH+1 )
C
         IF ( LUPLO ) THEN
C
            DO 20 J = 1, N - 1
               J1 = J + 1
               CALL DCOPY( J, X(1,J), 1, DWORK(1+(J-1)*N), 1 )
               CALL DTRMV( UPLO, 'NoTran', 'NoDiag', J, H, LDH,
     $                     DWORK(1+(J-1)*N), 1 )
               DO 10 I = 2, J
                  DWORK(I+(J-1)*N) = DWORK(I+(J-1)*N) + H(I,1)*X(I-1,J)
   10          CONTINUE
               DWORK(J1+(J-1)*N) = H(J1,1)*X(J,J)
   20       CONTINUE
C
            CALL DCOPY( N, X(1,N), 1, DWORK(1+(N-1)*N), 1 )
            CALL DTRMV( UPLO, 'NoTran', 'NoDiag', N, H, LDH,
     $                  DWORK(1+(N-1)*N), 1 )
C
            DO 30 I = 2, N
               DWORK(I+(N-1)*N) = DWORK(I+(N-1)*N) + H(I,1)*X(I-1,N)
   30       CONTINUE
C
         ELSE
C
            DO 50 J = 1, N - 1
               J1 = J + 1
               CALL DCOPY( J, X(J,1), LDX, DWORK(1+(J-1)*N), 1 )
               CALL DTRMV( 'Upper', 'NoTran', 'NoDiag', J, H, LDH,
     $                     DWORK(1+(J-1)*N), 1 )
               DO 40 I = 2, J
                  DWORK(I+(J-1)*N) = DWORK(I+(J-1)*N) + H(I,1)*X(J,I-1)
   40          CONTINUE
               DWORK(J1+(J-1)*N) = H(J1,1)*X(J,J)
   50       CONTINUE
C
            CALL DCOPY( N, X(N,1), LDX, DWORK(1+(N-1)*N), 1 )
            CALL DTRMV( 'Upper', 'NoTran', 'NoDiag', N, H, LDH,
     $                  DWORK(1+(N-1)*N), 1 )
C
            DO 60 I = 2, N
               DWORK(I+(N-1)*N) = DWORK(I+(N-1)*N) + H(I,1)*X(N,I-1)
   60       CONTINUE
C
         END IF
C
         IF ( N.GT.2 )
     $      CALL DSWAP( N-2, H(3,1), 1, H(3,2), LDH+1 )
C
      ELSE
C
         IF ( LUPLO ) THEN
C
            DO 70 J = 1, N - 1
               J1 = J + 1
               CALL DCOPY( J, H(1,J), 1, DWORK(1+(J-1)*N), 1 )
               CALL DTRMV( UPLO, 'NoTran', 'NoDiag', J, X, LDX,
     $                     DWORK(1+(J-1)*N), 1 )
               CALL DAXPY( J, H(J1,J), X(1,J1), 1, DWORK(1+(J-1)*N), 1 )
               DWORK(J1+(J-1)*N) = H(J1,J)*X(J1,J1)
   70       CONTINUE
C
            CALL DCOPY( N, H(1,N), 1, DWORK(1+(N-1)*N), 1 )
            CALL DTRMV( UPLO, 'NoTran', 'NoDiag', N, X, LDX,
     $                  DWORK(1+(N-1)*N), 1 )
C
         ELSE
C
            DO 80 J = 1, N - 1
               J1 = J + 1
               CALL DCOPY( J, H(1,J), 1, DWORK(1+(J-1)*N), 1 )
               CALL DTRMV( UPLO, 'Tran', 'NoDiag', J, X, LDX,
     $                     DWORK(1+(J-1)*N), 1 )
               CALL DAXPY( J, H(J1,J), X(J1,1), LDX, DWORK(1+(J-1)*N),
     $                     1 )
               DWORK(J1+(J-1)*N) = H(J1,J)*X(J1,J1)
   80       CONTINUE
C
            CALL DCOPY( N, H(1,N), 1, DWORK(1+(N-1)*N), 1 )
            CALL DTRMV( UPLO, 'Tran', 'NoDiag', N, X, LDX,
     $                  DWORK(1+(N-1)*N), 1 )
C
         END IF
C
      END IF
C
      CALL DSCAL( N, TWO, X, LDX+1 )
C
      CALL MB01OH( UPLO, TRANS, N, ALPHA, BETA, R, LDR, H, LDH, DWORK,
     $             N )
C
      RETURN
C *** Last line of MB01RH ***
      END
