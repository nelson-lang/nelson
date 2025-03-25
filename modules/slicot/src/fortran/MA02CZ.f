      SUBROUTINE MA02CZ( N, KL, KU, A, LDA )
C
C     PURPOSE
C
C     To compute the pertranspose of a central band of a square matrix.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the square matrix A.  N >= 0.
C
C     KL      (input) INTEGER
C             The number of subdiagonals of A to be pertransposed.
C             0 <= KL <= N-1.
C
C     KU      (input) INTEGER
C             The number of superdiagonals of A to be pertransposed.
C             0 <= KU <= N-1.
C
C     A       (input/output) COMPLEX*16 array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain a square matrix whose central band formed from
C             the KL subdiagonals, the main diagonal and the KU
C             superdiagonals will be pertransposed.
C             On exit, the leading N-by-N part of this array contains
C             the matrix A with its central band (the KL subdiagonals,
C             the main diagonal and the KU superdiagonals) pertransposed
C             (that is the elements of each antidiagonal appear in
C             reversed order). This is equivalent to forming P*B'*P,
C             where B is the matrix formed from the central band of A
C             and P is a permutation matrix with ones down the secondary
C             diagonal.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= max(1,N).
C
C     CONTRIBUTOR
C
C     A. Varga, German Aerospace Center,
C     DLR Oberpfaffenhofen, March 1998.
C     Complex version: V. Sima, Research Institute for Informatics,
C     Bucharest, Nov. 2008.
C
C     REVISIONS
C
C     -
C
C    ******************************************************************
C
C     .. Scalar Arguments ..
      INTEGER          KL, KU, LDA, N
C     .. Array Arguments ..
      COMPLEX*16       A(LDA,*)
C     .. Local Scalars ..
      INTEGER          I, I1, LDA1
C     .. External Subroutines ..
      EXTERNAL         ZSWAP
C     .. Intrinsic Functions ..
      INTRINSIC        MIN
C     .. Executable Statements ..
C
C     Quick return if possible.
C
      IF( N.LE.1 )
     $   RETURN
C
      LDA1 = LDA + 1
C
C     Pertranspose the KL subdiagonals.
C
      DO 10 I = 1, MIN( KL, N-2 )
         I1 = (N-I) / 2
         IF( I1.GT.0 )
     $      CALL ZSWAP( I1, A(I+1,1), LDA1, A(N-I1+1,N-I1+1-I), -LDA1 )
   10 CONTINUE
C
C     Pertranspose the KU superdiagonals.
C
      DO 20 I = 1, MIN( KU, N-2 )
         I1 = (N-I) / 2
         IF( I1.GT.0 )
     $      CALL ZSWAP( I1, A(1,I+1), LDA1, A(N-I1+1-I,N-I1+1), -LDA1 )
   20 CONTINUE
C
C     Pertranspose the diagonal.
C
      I1 = N / 2
      IF( I1.GT.0 )
     $   CALL ZSWAP( I1, A(1,1), LDA1, A(N-I1+1,N-I1+1), -LDA1 )
C
      RETURN
C *** Last line of MA02CZ ***
      END
