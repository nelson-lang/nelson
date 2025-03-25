      SUBROUTINE MA02BZ( SIDE, M, N, A, LDA )
C
C     PURPOSE
C
C     To reverse the order of rows and/or columns of a given matrix A
C     by pre-multiplying and/or post-multiplying it, respectively, with
C     a permutation matrix P, where P is a square matrix of appropriate
C     order, with ones down the secondary diagonal.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     SIDE    CHARACTER*1
C             Specifies the operation to be performed, as follows:
C             = 'L': the order of rows of A is to be reversed by
C                    pre-multiplying A with P;
C             = 'R': the order of columns of A is to be reversed by
C                    post-multiplying A with P;
C             = 'B': both the order of rows and the order of columns
C                    of A is to be reversed by pre-multiplying and
C                    post-multiplying A with P.
C
C     Input/Output Parameters
C
C     M       (input) INTEGER
C             The number of rows of the matrix A.  M >= 0.
C
C     N       (input) INTEGER
C             The number of columns of the matrix A.  N >= 0.
C
C     A       (input/output) COMPLEX*16 array, dimension (LDA,N)
C             On entry, the leading M-by-N part of this array must
C             contain the given matrix whose rows and/or columns are to
C             be permuted.
C             On exit, the leading M-by-N part of this array contains
C             the matrix P*A if SIDE = 'L', or A*P if SIDE = 'R', or
C             P*A*P if SIDE = 'B'.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= max(1,M).
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
      CHARACTER          SIDE
      INTEGER            LDA, M, N
C     .. Array Arguments ..
      COMPLEX*16         A(LDA,*)
C     .. Local Scalars ..
      LOGICAL            BSIDES
      INTEGER            I, J, K, M2, N2
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     .. External Subroutines ..
      EXTERNAL           ZSWAP
C     .. Executable Statements ..
C
      BSIDES  = LSAME( SIDE, 'B' )
C
      IF( ( LSAME( SIDE, 'L' ) .OR. BSIDES ) .AND. M.GT.1 ) THEN
C
C        Compute P*A.
C
         M2 = M/2
         K = M - M2 + 1
         DO 10 J = 1, N
            CALL ZSWAP( M2, A(1,J), -1, A(K,J), 1 )
   10    CONTINUE
      END IF
      IF( ( LSAME( SIDE, 'R' ) .OR. BSIDES ) .AND. N.GT.1 ) THEN
C
C        Compute A*P.
C
         N2 = N/2
         K = N - N2 + 1
         DO 20 I = 1, M
            CALL ZSWAP( N2, A(I,1), -LDA, A(I,K), LDA )
   20    CONTINUE
      END IF
C
      RETURN
C *** Last line of MA02BZ ***
      END
