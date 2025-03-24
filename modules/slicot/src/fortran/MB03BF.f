      SUBROUTINE MB03BF( K, AMAP, S, SINV, A, LDA1, LDA2, ULP )
C
C     PURPOSE
C
C     To apply at most 20 iterations of a real single shifted
C     periodic QZ algorithm to the 2-by-2 product of matrices stored
C     in the array A. The Hessenberg matrix is the last one of the
C     formal matrix product.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     K       (input)  INTEGER
C             The number of factors.  K >= 1.
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
C     A       (input/output)  DOUBLE PRECISION array, dimension
C                             (LDA1,LDA2,K)
C             On entry, the leading 2-by-2-by-K part of this array must
C             contain a 2-by-2 product (implicitly represented by its K
C             factors) in upper Hessenberg form. The Hessenberg matrix
C             is the last one of the formal matrix product.
C             On exit, the leading 2-by-2-by-K part of this array
C             contains the product after at most 20 iterations of a real
C             shifted periodic QZ algorithm.
C
C     LDA1    INTEGER
C             The first leading dimension of the array A.  LDA1 >= 2.
C
C     LDA2    INTEGER
C             The second leading dimension of the array A.  LDA2 >= 2.
C
C     ULP     INTEGER
C             The machine relation precision.
C
C     METHOD
C
C     Twenty iterations of a real single shifted periodic QZ algorithm
C     are applied to the 2-by-2 matrix product A.
C
C     CONTRIBUTOR
C
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     Dec. 2018.
C
C     REVISIONS
C
C     -
C
C     KEYWORDS
C
C     Eigenvalues, QZ algorithm, periodic QZ algorithm, orthogonal
C     transformation.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO
      PARAMETER         ( ZERO = 0.0D0 )
C     .. Scalar Arguments ..
      INTEGER           K, LDA1, LDA2, SINV
      DOUBLE PRECISION  ULP
C     .. Array Arguments ..
      INTEGER           AMAP(*), S(*)
      DOUBLE PRECISION  A(LDA1,LDA2,*)
C     .. Local Scalars ..
      INTEGER           I, L, AI
      DOUBLE PRECISION  CS, CT, SN, ST, TEMP
C     .. External Subroutines ..
      EXTERNAL          DLARTG, DROT, MB03AF
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, MAX
C
C     .. Executable Statements ..
C
      DO 20  I = 1, 20
         CALL MB03AF( 'Single', K, 2, AMAP, S, SINV, A, LDA1, LDA2,
     $                CS, SN, CT, ST )
         AI = AMAP(K)
         CALL DROT( 2, A(1,1,AI), 1, A(1,2,AI), 1, CS, SN )
C
         DO 10  L = 1, K - 1
            AI = AMAP(L)
            IF ( S(AI).EQ.SINV ) THEN
               CALL DROT( 2, A(1,1,AI), LDA1, A(2,1,AI), LDA1, CS, SN )
               TEMP = A(2,2,AI)
               CALL DLARTG( TEMP, -A(2,1,AI), CS, SN, A(2,2,AI) )
               A(2,1,AI) = ZERO
               TEMP      = CS*A(1,1,AI) + SN*A(1,2,AI)
               A(1,2,AI) = CS*A(1,2,AI) - SN*A(1,1,AI)
               A(1,1,AI) = TEMP
            ELSE
               CALL DROT( 2, A(1,1,AI), 1, A(1,2,AI), 1, CS, SN )
               TEMP = A(1,1,AI)
               CALL DLARTG( TEMP, A(2,1,AI), CS, SN, A(1,1,AI) )
               A(2,1,AI) = ZERO
               TEMP      = CS*A(1,2,AI) + SN*A(2,2,AI)
               A(2,2,AI) = CS*A(2,2,AI) - SN*A(1,2,AI)
               A(1,2,AI) = TEMP
            END IF
   10    CONTINUE
C
         AI = AMAP(K)
         CALL DROT( 2, A(1,1,AI), LDA1, A(2,1,AI), LDA1, CS, SN )
C
         IF ( ABS( A(2,1,AI) ).LT.ULP*( MAX( ABS( A(1,1,AI) ),
     $                                       ABS( A(1,2,AI) ),
     $                                       ABS( A(2,2,AI) ) ) ) )
     $      GO TO 30
   20 CONTINUE
C
   30 CONTINUE
C
      RETURN
C *** Last line of MB03BF ***
      END
