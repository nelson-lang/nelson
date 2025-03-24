      SUBROUTINE MA02NZ( UPLO, TRANS, SKEW, N, K, L, A, LDA )
C
C     PURPOSE
C
C     To permute two specified rows and corresponding columns of a
C     (skew-)symmetric/Hermitian complex matrix.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     UPLO    CHARACTER*1
C             Specifies whether the upper or lower triangular part of
C             the (skew-)symmetric/Hermitian matrix A is to be
C             referenced, as follows:
C             = 'U':  Upper triangular part of A is referenced;
C             = 'L':  Lower triangular part of A is referenced.
C
C     TRANS   CHARACTER*1
C             Specifies whether to use transposition or conjugate
C             transposition as follows:
C             = 'T':  Use transposition;
C             = 'C':  Use conjugate transposition.
C
C     SKEW    CHARACTER*1
C             Specifies whether the matrix is symmetric/Hermitian or
C             skew-symmetric/Hermitian as follows:
C             = 'N':  The matrix is symmetric/Hermitian;
C             = 'S':  The matrix is skew-symmetric/skew-Hermitian.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrix A.  N >= 0.
C
C     K       (input) INTEGER
C             The smaller index of the pair of rows and columns to be
C             permuted.  0 <= K <= L. If K = 0, the routine returns.
C
C     L       (input) INTEGER
C             The larger index of the pair of rows and columns to be
C             permuted.  K <= L <= N.
C
C     A       (input/output) COMPLEX*16 array, dimension (LDA,N)
C             On entry, the leading N-by-N upper triangular part
C             (if UPLO = 'U'), or lower triangular part (if UPLO = 'L'),
C             of this array must contain the corresponding upper or
C             lower triangle of the (skew-)symmetric/Hermitian matrix A.
C             On exit, the leading N-by-N upper or lower triangular part
C             of this array (depending on UPLO) contains the
C             corresponding part of the permuted matrix A.
C             Note that a Hermitian matrix has the imaginary parts of
C             the diagonal entries zero. Similarly, a skew-Hermitian
C             matrix has the real parts of the diagonal entries zero.
C             The routine does not check out this conditions.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= max(N,1).
C
C     CONTRIBUTORS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Jan. 2016.
C
C     REVISIONS
C
C     -
C
C     KEYWORDS
C
C     Elementary matrix operations, skew-symmetric matrix.
C
C     ******************************************************************
C
C     .. Scalar Arguments ..
      CHARACTER          SKEW, TRANS, UPLO
      INTEGER            K, L, LDA, N
C     .. Array Arguments ..
      COMPLEX*16         A(LDA,*)
C     .. Local Scalars ..
      INTEGER            I
      COMPLEX*16         T
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     .. External Subroutines ..
      EXTERNAL           ZSWAP
C     ..Intrinsic Functions..
      INTRINSIC          DBLE, DCMPLX, DCONJG, DIMAG
C
C     .. Executable Statements ..
C
C     For efficiency reasons, the parameters are not checked for errors.
C
      IF( N.EQ.0 .OR. K.EQ.0 .OR. K.EQ.L )
     $   RETURN
C
      T      = A(K,K)
      A(K,K) = A(L,L)
      A(L,L) = T
C
      IF( LSAME( UPLO, 'L' ) ) THEN
C
C        Permute the lower triangle of A.
C
         CALL ZSWAP( K-1, A(K,1), LDA, A(L,1), LDA )
C
         IF( LSAME( TRANS, 'T' ) ) THEN
C
            IF( LSAME( SKEW, 'N' ) ) THEN
C
               CALL ZSWAP( L-K-1, A(K+1,K), 1, A(L,K+1), LDA )
            ELSE
C
               A(L,K) = -A(L,K)
               DO 10 I = K+1, L-1
                  T      = -A(L,I)
                  A(L,I) = -A(I,K)
                  A(I,K) = T
   10          CONTINUE
C
            END IF
C
         ELSE
C
            IF( LSAME( SKEW, 'N' ) ) THEN
C
               A(L,K) = DCONJG( A(L,K) )
               DO 20 I = K+1, L-1
                  T      = DCONJG( A(L,I) )
                  A(L,I) = DCONJG( A(I,K) )
                  A(I,K) = T
   20          CONTINUE
C
            ELSE
C
               A(L,K) = DCMPLX( -DBLE( A(L,K) ), DIMAG( A(L,K) ) )
               DO 30 I = K+1, L-1
                  T      = DCMPLX( -DBLE( A(L,I) ), DIMAG( A(L,I) ) )
                  A(L,I) = DCMPLX( -DBLE( A(I,K) ), DIMAG( A(I,K) ) )
                  A(I,K) = T
   30          CONTINUE
C
            END IF
C
         END IF
C
         CALL ZSWAP( N-L, A(L+1,K), 1, A(L+1,L), 1 )
C
      ELSE IF( LSAME( UPLO, 'U' ) ) THEN
C
C        Permute the upper triangle of A.
C
         CALL ZSWAP( K-1, A(1,K), 1, A(1,L), 1 )
C
         IF( LSAME( TRANS, 'T' ) ) THEN
C
            IF( LSAME( SKEW, 'N' ) ) THEN
C
               CALL ZSWAP( L-K-1, A(K,K+1), LDA, A(K+1,L), 1 )
            ELSE
C
               A(K,L) = -A(K,L)
               DO 40 I = K+1, L-1
                  T      = -A(I,L)
                  A(I,L) = -A(K,I)
                  A(K,I) = T
   40          CONTINUE
C
            END IF
C
         ELSE
C
            IF( LSAME( SKEW, 'N' ) ) THEN
C
               A(K,L) = DCONJG( A(K,L) )
               DO 50 I = K+1, L-1
                  T      = DCONJG( A(I,L) )
                  A(I,L) = DCONJG( A(K,I) )
                  A(K,I) = T
   50          CONTINUE
C
            ELSE
C
               A(K,L) = DCMPLX( -DBLE( A(K,L) ), DIMAG( A(K,L) ) )
               DO 60 I = K+1, L-1
                  T      = DCMPLX( -DBLE( A(I,L) ), DIMAG( A(I,L) ) )
                  A(I,L) = DCMPLX( -DBLE( A(K,I) ), DIMAG( A(K,I) ) )
                  A(K,I) = T
   60          CONTINUE
C
            END IF
C
         END IF
C
         CALL ZSWAP( N-L, A(K,L+1), LDA, A(L,L+1), LDA )
C
      END IF
C
      RETURN
C
C *** Last line of MA02NZ ***
      END
