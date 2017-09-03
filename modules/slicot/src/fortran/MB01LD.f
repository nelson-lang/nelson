      SUBROUTINE MB01LD( UPLO, TRANS, M, N, ALPHA, BETA, R, LDR, A, LDA,
     $                   X, LDX, DWORK, LDWORK, INFO )
C
C     SLICOT RELEASE 5.0.
C
C     Copyright (c) 2002-2010 NICONET e.V.
C
C     This program is free software: you can redistribute it and/or
C     modify it under the terms of the GNU General Public License as
C     published by the Free Software Foundation, either version 2 of
C     the License, or (at your option) any later version.
C
C     This program is distributed in the hope that it will be useful,
C     but WITHOUT ANY WARRANTY; without even the implied warranty of
C     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C     GNU General Public License for more details.
C
C     You should have received a copy of the GNU General Public License
C     along with this program.  If not, see
C     <http://www.gnu.org/licenses/>.
C
C     PURPOSE
C
C     To compute the matrix formula
C        _
C        R = alpha*R + beta*op( A )*X*op( A )',
C                                                 _
C     where alpha and beta are scalars, R, X, and R are skew-symmetric
C     matrices, A is a general matrix, and op( A ) is one of
C
C        op( A ) = A   or   op( A ) = A'.
C
C     The result is overwritten on R.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     UPLO    CHARACTER*1
C             Specifies which triangles of the skew-symmetric matrices R
C             and X are given, as follows:
C             = 'U':  the strictly upper triangular part is given;
C             = 'L':  the strictly lower triangular part is given.
C
C     TRANS   CHARACTER*1
C             Specifies the form of op( A ) to be used in the matrix
C             multiplication, as follows:
C             = 'N':  op( A ) = A;
C             = 'T':  op( A ) = A';
C             = 'C':  op( A ) = A'.
C
C     Input/Output Parameters
C
C     M       (input) INTEGER           _
C             The order of the matrices R and R and the number of rows
C             of the matrix op( A ).  M >= 0.
C
C     N       (input) INTEGER
C             The order of the matrix X and the number of columns of the
C             matrix op( A ).  N >= 0.
C
C     ALPHA   (input) DOUBLE PRECISION
C             The scalar alpha. When alpha is zero then R need not be
C             set before entry, except when R is identified with X in
C             the call.
C
C     BETA    (input) DOUBLE PRECISION
C             The scalar beta. When beta is zero or N <= 1, or M <= 1,
C             then A and X are not referenced.
C
C     R       (input/output) DOUBLE PRECISION array, dimension (LDR,M)
C             On entry with UPLO = 'U', the leading M-by-M strictly
C             upper triangular part of this array must contain the
C             strictly upper triangular part of the skew-symmetric
C             matrix R. The lower triangle is not referenced.
C             On entry with UPLO = 'L', the leading M-by-M strictly
C             lower triangular part of this array must contain the
C             strictly lower triangular part of the skew-symmetric
C             matrix R. The upper triangle is not referenced.
C             On exit, the leading M-by-M strictly upper triangular part
C             (if UPLO = 'U'), or strictly lower triangular part
C             (if UPLO = 'L'), of this array contains the corresponding
C                                                             _
C             strictly triangular part of the computed matrix R.
C
C     LDR     INTEGER
C             The leading dimension of the array R.  LDR >= MAX(1,M).
C
C     A       (input) DOUBLE PRECISION array, dimension (LDA,k)
C             where k is N when TRANS = 'N' and is M when TRANS = 'T' or
C             TRANS = 'C'.
C             On entry with TRANS = 'N', the leading M-by-N part of this
C             array must contain the matrix A.
C             On entry with TRANS = 'T' or TRANS = 'C', the leading
C             N-by-M part of this array must contain the matrix A.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,k),
C             where k is M when TRANS = 'N' and is N when TRANS = 'T' or
C             TRANS = 'C'.
C
C     X       (input or input/output) DOUBLE PRECISION array, dimension
C             (LDX,K), where K = N, if UPLO = 'U' or  LDWORK >= M*(N-1),
C                  or K = MAX(N,M), if UPLO = 'L' and LDWORK <  M*(N-1).
C             On entry, if UPLO = 'U', the leading N-by-N strictly upper
C             triangular part of this array must contain the strictly
C             upper triangular part of the skew-symmetric matrix X and
C             the lower triangular part of the array is not referenced.
C             On entry, if UPLO = 'L', the leading N-by-N strictly lower
C             triangular part of this array must contain the strictly
C             lower triangular part of the skew-symmetric matrix X and
C             the upper triangular part of the array is not referenced.
C             If LDWORK < M*(N-1), this array is overwritten with the
C             matrix op(A)*X, if UPLO = 'U', or X*op(A)', if UPLO = 'L'.
C
C     LDX     INTEGER
C             The leading dimension of the array X.
C             LDX >= MAX(1,N),   if UPLO = 'L' or  LDWORK >= M*(N-1);
C             LDX >= MAX(1,N,M), if UPLO = 'U' and LDWORK <  M*(N-1).
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             This array is not referenced when beta = 0, or M <= 1, or
C             N <= 1.
C
C     LDWORK  The length of the array DWORK.
C             LDWORK >= N, if  beta <> 0, and M > 0, and N >  1;
C             LDWORK >= 0, if  beta =  0, or  M = 0, or  N <= 1.
C             For optimum performance, LDWORK >= M*(N-1), if  beta <> 0,
C             M > 1, and N > 1.
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
C     The matrix expression is efficiently evaluated taking the skew-
C     symmetry into account. If LDWORK >= M*(N-1), a BLAS 3 like
C     implementation is used. Specifically, let X = T - T', with T a
C     strictly upper or strictly lower triangular matrix, defined by
C
C        T = striu( X ),  if UPLO = 'U',
C        T = stril( X ),  if UPLO = 'L',
C
C     where striu and stril denote the strictly upper triangular part
C     and strictly lower triangular part of X, respectively. Then,
C
C        A*X*A' = ( A*T )*A' - A*( A*T )',  for TRANS = 'N',
C        A'*X*A = A'*( T*A ) - ( T*A )'*A,  for TRANS = 'T', or 'C',
C
C     which involve BLAS 3 operations DTRMM and the skew-symmetric
C     correspondent of DSYR2K (with a Fortran implementation available
C     in the SLICOT Library routine MB01KD).
C     If LDWORK < M*(N-1), a BLAS 2 implementation is used.
C
C     NUMERICAL ASPECTS
C
C     The algorithm requires approximately
C
C                   2         2
C        3/2 x M x N + 1/2 x M
C
C     operations.
C
C     CONTRIBUTORS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Jan. 2010.
C     Based on the SLICOT Library routine MB01RU and the HAPACK Library
C     routine DSKUPD.
C
C     REVISIONS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Oct. 2010.
C
C     KEYWORDS
C
C     Elementary matrix operations, matrix algebra, matrix operations.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0 )
C     .. Scalar Arguments ..
      CHARACTER         TRANS, UPLO
      INTEGER           INFO, LDA, LDR, LDWORK, LDX, M, N
      DOUBLE PRECISION  ALPHA, BETA
C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), DWORK(*), R(LDR,*), X(LDX,*)
C     .. Local Scalars ..
      LOGICAL           LTRANS, NOTTRA, UPPER
      INTEGER           I, J, M2
C     .. External Functions ..
      LOGICAL           LSAME
      EXTERNAL          LSAME
C     .. External Subroutines ..
      EXTERNAL          DCOPY, DGEMV, DLACPY, DLASCL, DLASET, DSCAL,
     $                  DTRMM, MB01KD, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         MAX, MIN
C     .. Executable Statements ..
C
C     Test the input scalar arguments.
C
      INFO   = 0
      UPPER  = LSAME( UPLO,  'U' )
      NOTTRA = LSAME( TRANS, 'N' )
      LTRANS = LSAME( TRANS, 'T' ) .OR. LSAME( TRANS, 'C' )
C
      IF(      ( .NOT.UPPER  ).AND.( .NOT.LSAME( UPLO, 'L' ) ) )THEN
         INFO = -1
      ELSE IF( ( .NOT.NOTTRA ).AND.( .NOT.LTRANS ) )THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDR.LT.MAX( 1, M ) ) THEN
         INFO = -8
      ELSE IF( LDA.LT.1 .OR. ( LTRANS .AND. LDA.LT.N ) .OR.
     $                       ( NOTTRA .AND. LDA.LT.M ) ) THEN
         INFO = -10
      ELSE IF( LDX.LT.MAX( 1, N ) .OR.
     $       ( LDX.LT.M .AND. UPPER .AND. LDWORK.LT.M*( N - 1 ) ) ) THEN
         INFO = -12
      ELSE IF( LDWORK.LT.0 .OR. ( BETA.NE.ZERO .AND. M.GT.1 .AND. N.GT.1
     $      .AND. LDWORK.LT.N ) ) THEN
         INFO = -14
      END IF
C
      IF ( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'MB01LD', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF ( M.LE.0 )
     $   RETURN
C
      M2 = MIN( 2, M )
      IF ( BETA.EQ.ZERO .OR. N.LE.1 ) THEN
         IF ( UPPER ) THEN
            I = 1
            J = M2
         ELSE
            I = M2
            J = 1
         END IF
C
         IF ( ALPHA.EQ.ZERO ) THEN
C
C           Special case alpha = 0.
C
            CALL DLASET( UPLO, M-1, M-1, ZERO, ZERO, R(I,J), LDR )
         ELSE
C
C           Special case beta = 0 or N <= 1.
C
            IF ( ALPHA.NE.ONE )
     $         CALL DLASCL( UPLO, 0, 0, ONE, ALPHA, M-1, M-1, R(I,J),
     $                      LDR, INFO )
         END IF
         RETURN
      END IF
C
C     General case: beta <> 0.
C
      IF ( LDWORK.GE.M*( N - 1 ) ) THEN
C
C        Use a BLAS 3 like implementation.
C        Compute W = A*T or W = T*A in DWORK, and apply the updating
C        formula (see METHOD section). Note that column 1 (if
C        UPLO = 'U') or column N (if UPLO = 'L') is zero in the first
C        case, and it is not stored; similarly, row N (if UPLO = 'U') or
C        row 1 (if UPLO = 'L') is zero in the second case, and it is not
C        stored.
C        Workspace: need M*(N-1).
C
         IF ( UPPER ) THEN
            I = 1
            J = M2
         ELSE
            I = M2
            J = 1
         END IF
C
         IF( NOTTRA ) THEN
C
            CALL DLACPY( 'Full', M, N-1, A(1,I), LDA, DWORK, M )
            CALL DTRMM(  'Right', UPLO, 'NoTranspose', 'Non-unit', M,
     $                   N-1, ONE, X(I,J), LDX, DWORK, M )
            CALL MB01KD( UPLO, TRANS, M, N-1, BETA, DWORK, M, A(1,J),
     $                   LDA, ALPHA, R, LDR, INFO )
C
         ELSE
C
            CALL DLACPY( 'Full', N-1, M, A(J,1), LDA, DWORK, N-1 )
            CALL DTRMM(  'Left', UPLO, 'NoTranspose', 'Non-unit', N-1,
     $                   M, ONE, X(I,J), LDX, DWORK, N-1 )
            CALL MB01KD( UPLO, TRANS, M, N-1, BETA, A(I,1), LDA, DWORK,
     $                   N-1, ALPHA, R, LDR, INFO )
C
         END IF
C
      ELSE
C
C        Use a BLAS 2 implementation.
C
C
         IF ( NOTTRA ) THEN
C
C           Compute A*X*A'.
C
            IF ( UPPER ) THEN
C
C              Compute A*X in X (M-by-N).
C
               DO 10 J = 1, N-1
                  CALL DCOPY( J-1, X(1,J), 1, DWORK, 1 )
                  DWORK(J) = ZERO
                  CALL DCOPY( N-J, X(J,J+1), LDX, DWORK(J+1), 1 )
                  CALL DSCAL( N-J, -ONE, DWORK(J+1), 1 )
                  CALL DGEMV( TRANS, M, N, ONE, A, LDA, DWORK, 1, ZERO,
     $                        X(1,J), 1 )
   10          CONTINUE
C
               CALL DCOPY( N-1, X(1,N), 1, DWORK, 1 )
               CALL DGEMV( TRANS, M, N-1, ONE, A, LDA, DWORK, 1, ZERO,
     $                     X(1,N), 1 )
C
C              Compute alpha*striu( R ) + beta*striu( X*A' ) in the
C              strictly upper triangular part of R.
C
               DO 20 I = 1, M-1
                  CALL DCOPY( N, X(I,1), LDX, DWORK, 1 )
                  CALL DGEMV( TRANS, M-I, N, BETA, A(I+1,1), LDA, DWORK,
     $                        1, ALPHA, R(I,I+1), LDR )
   20          CONTINUE
C
            ELSE
C
C              Compute X*A' in X (N-by-M).
C
               DO 30 I = 1, N-1
                  CALL DCOPY( I-1, X(I,1), LDX, DWORK, 1 )
                  DWORK(I) = ZERO
                  CALL DCOPY( N-I, X(I+1,I), 1, DWORK(I+1), 1 )
                  CALL DSCAL( N-I, -ONE, DWORK(I+1), 1 )
                  CALL DGEMV( TRANS, M, N, ONE, A, LDA, DWORK, 1, ZERO,
     $                        X(I,1), LDX )
   30          CONTINUE
C
               CALL DCOPY( N-1, X(N,1), LDX, DWORK, 1 )
               CALL DGEMV( TRANS, M, N-1, ONE, A, LDA, DWORK, 1, ZERO,
     $                     X(N,1), LDX )
C
C              Compute alpha*stril( R ) + beta*stril( A*X ) in the
C              strictly lower triangular part of R.
C
               DO 40 J = 1, M-1
                  CALL DCOPY( N, X(1,J), 1, DWORK, 1 )
                  CALL DGEMV( TRANS, M-J, N, BETA, A(J+1,1), LDA, DWORK,
     $                        1, ALPHA, R(J+1,J), 1 )
   40          CONTINUE
C
            END IF
C
         ELSE
C
C           Compute A'*X*A.
C
            IF ( UPPER ) THEN
C
C              Compute A'*X in X (M-by-N).
C
               DO 50 J = 1, N-1
                  CALL DCOPY( J-1, X(1,J), 1, DWORK, 1 )
                  DWORK(J) = ZERO
                  CALL DCOPY( N-J, X(J,J+1), LDX, DWORK(J+1), 1 )
                  CALL DSCAL( N-J, -ONE, DWORK(J+1), 1 )
                  CALL DGEMV( TRANS, N, M, ONE, A, LDA, DWORK, 1, ZERO,
     $                        X(1,J), 1 )
   50          CONTINUE
C
               CALL DCOPY( N-1, X(1,N), 1, DWORK, 1 )
               CALL DGEMV( TRANS, N-1, M, ONE, A, LDA, DWORK, 1, ZERO,
     $                     X(1,N), 1 )
C
C              Compute alpha*striu( R ) + beta*striu( X*A ) in the
C              strictly upper triangular part of R.
C
               DO 60 I = 1, M-1
                  CALL DCOPY( N, X(I,1), LDX, DWORK, 1 )
                  CALL DGEMV( TRANS, N, M-I, BETA, A(1,I+1), LDA, DWORK,
     $                        1, ALPHA, R(I,I+1), LDR )
   60          CONTINUE
C
            ELSE
C
C              Compute X*A in X (N-by-M).
C
               DO 70 I = 1, N-1
                  CALL DCOPY( I-1, X(I,1), LDX, DWORK, 1 )
                  DWORK(I) = ZERO
                  CALL DCOPY( N-I, X(I+1,I), 1, DWORK(I+1), 1 )
                  CALL DSCAL( N-I, -ONE, DWORK(I+1), 1 )
                  CALL DGEMV( TRANS, N, M, ONE, A, LDA, DWORK, 1, ZERO,
     $                        X(I,1), LDX )
   70          CONTINUE
C
               CALL DCOPY( N-1, X(N,1), LDX, DWORK, 1 )
               CALL DGEMV( TRANS, N-1, M, ONE, A, LDA, DWORK, 1, ZERO,
     $                     X(N,1), LDX )
C
C              Compute alpha*stril( R ) + beta*stril( A'*X ) in the
C              strictly lower triangular part of R.
C
               DO 80 J = 1, M-1
                  CALL DCOPY( N, X(1,J), 1, DWORK, 1 )
                  CALL DGEMV( TRANS, N, M-J, BETA, A(1,J+1), LDA, DWORK,
     $                        1, ALPHA, R(J+1,J), 1 )
   80          CONTINUE
C
            END IF
         END IF
      END IF
C
      RETURN
C *** Last line of MB01LD ***
      END
