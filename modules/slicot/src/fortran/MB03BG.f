      SUBROUTINE MB03BG( K, N, AMAP, S, SINV, A, LDA1, LDA2, WR, WI )
C
C     PURPOSE
C
C     To compute the eigenvalues of the 2-by-2 trailing submatrix of the
C     matrix product
C
C                  S(1)           S(2)                 S(K)
C          A(:,:,1)     * A(:,:,2)     * ... * A(:,:,K)
C
C     where A(:,:,AMAP(K)) is upper Hessenberg and A(:,:,AMAP(i)),
C     1 <= i < K, is upper triangular. All factors to be inverted
C     (depending on S and SINV) are assumed nonsingular. Moreover,
C     AMAP(K) is either 1 or K.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     K       (input)  INTEGER
C             The number of factors.  K >= 1.
C
C     N       (input)  INTEGER
C             The order of the factors.  N >= 2.
C
C     AMAP    (input)  INTEGER array, dimension (K)
C             The map for accessing the factors, i.e., if AMAP(I) = J,
C             then the factor A_I is stored at the J-th position in A.
C
C     S       (input)  INTEGER array, dimension (K)
C             The signature array. Each entry of S must be 1 or -1.
C
C     SINV    (input)  INTEGER
C             Signature multiplier. Entries of S are virtually
C             multiplied by SINV.
C
C     A       (input)  DOUBLE PRECISION array, dimension (LDA1,LDA2,K)
C             The leading N-by-N-by-K part of this array must contain
C             the product (implicitly represented by its K factors)
C             in upper Hessenberg form.
C
C     LDA1    INTEGER
C             The first leading dimension of the array A.  LDA1 >= N.
C
C     LDA2    INTEGER
C             The second leading dimension of the array A.  LDA2 >= N.
C
C     WR      (output)  DOUBLE PRECISION array, dimension (2)
C     WI      (output)  DOUBLE PRECISION array, dimension (2)
C             The real and imaginary parts, respectively, of the
C             eigenvalues of the 2-by-2 trailing submatrix of the
C             matrix product.
C
C     METHOD
C
C     The 2-by-2 trailing submatrix of the matrix product and its
C     eigenvalues are computed.
C
C     FURTHER COMMENTS
C
C     This routine is intended to be used in a context in which all
C     eigenvalues of the product are needed, hence, the product can be
C     evaluated as prod( A(:,:,AMAP(I)) ), for i = 1 : K. This way, the
C     2-by-2 trailing submatrix depends only on the 2-by-2 trailing
C     submatrices of the factors.
C
C     CONTRIBUTOR
C
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     Jan. 2019.
C
C     REVISIONS
C
C     V. Sima, Mar. 2019. 
C
C     KEYWORDS
C
C     Eigenvalues, QZ algorithm, periodic QZ algorithm, orthogonal
C     transformation.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ONE, ZERO
      PARAMETER         ( ONE = 1.0D0, ZERO = 0.0D0 )
C     .. Scalar Arguments ..
      INTEGER           K, LDA1, LDA2, N, SINV
C     .. Array Arguments ..
      INTEGER           AMAP(*), S(*)
      DOUBLE PRECISION  A(LDA1,LDA2,*), WI(*), WR(*)
C     .. Local Scalars ..
      INTEGER           I, INFO, L, M
      DOUBLE PRECISION  P1, P3, P4
C     .. Local Arrays ..
      DOUBLE PRECISION  DWORK(4), Z(1)
C     .. External Subroutines ..
      EXTERNAL          DLAHQR
C
C     .. Executable Statements ..
C
      M  = N - 1
      P1 = ONE
      P3 = ZERO
      P4 = ONE
C
      DO 10  L = 1, K - 1
         I = AMAP(L)
         IF ( S(I).EQ.SINV ) THEN
            P3 = P1*A(M,N,I) + P3*A(N,N,I)
         ELSE
            P3 = ( P3 - P1*A(M,N,I)/A(M,M,I) )/A(N,N,I)
         END IF
         P1 = P1*A(M,M,I)
         P4 = P4*A(N,N,I)
   10 CONTINUE
C
      I = AMAP(K)
      DWORK(1) = P1*A(M,M,I) + P3*A(N,M,I)
      DWORK(2) = P4*A(N,M,I)
      DWORK(3) = P1*A(M,N,I) + P3*A(N,N,I)
      DWORK(4) = P4*A(N,N,I)
C
C     Compute eigenvalues.
C
      CALL DLAHQR( .FALSE., .FALSE., 2, 1, 2, DWORK, 2, WR, WI, 1, 2, Z,
     $             1, INFO )
C
      RETURN
C *** Last line of MB03BG ***
      END
