      SUBROUTINE MB03AD( SHFT, K, N, AMAP, S, SINV, A, LDA1, LDA2, C1,
     $                   S1, C2, S2 )
C
C     PURPOSE
C
C     To compute two Givens rotations (C1,S1) and (C2,S2) such that the
C     orthogonal matrix
C
C               [ Q  0 ]        [  C1  S1  0 ]   [ 1  0   0  ]
C           Z = [      ],  Q := [ -S1  C1  0 ] * [ 0  C2  S2 ],
C               [ 0  I ]        [  0   0   1 ]   [ 0 -S2  C2 ]
C
C     makes the first column of the real Wilkinson double shift
C     polynomial of the product of matrices in periodic upper Hessenberg
C     form, stored in the array A, parallel to the first unit vector.
C     Only the rotation defined by C1 and S1 is used for the real
C     Wilkinson single shift polynomial (see SLICOT Library routine
C     MB03BE).
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     SHFT    CHARACTER*1
C             Specifies the number of shifts employed by the shift
C             polynomial, as follows:
C             = 'D':  two shifts (assumes N > 2);
C             = 'S':  one real shift.
C
C     Input/Output Parameters
C
C     K       (input)  INTEGER
C             The number of factors.  K >= 1.
C
C     N       (input)  INTEGER
C             The order of the factors in the array A.
C             N >= 2, for a single shift polynomial;
C             N >= 3, for a double shift polynomial.
C
C     AMAP    (input)  INTEGER array, dimension (K)
C             The map for accessing the factors, i.e., if AMAP(I) = J,
C             then the factor A_I is stored at the J-th position in A.
C             AMAP(1) is the pointer to the Hessenberg matrix.
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
C             in periodic upper Hessenberg form.
C
C     LDA1    INTEGER
C             The first leading dimension of the array A.  LDA1 >= N.
C
C     LDA2    INTEGER
C             The second leading dimension of the array A.  LDA2 >= N.
C
C     C1      (output)  DOUBLE PRECISION
C     S1      (output)  DOUBLE PRECISION
C             On exit, C1 and S1 contain the parameters for the first
C             Givens rotation.
C
C     C2      (output)  DOUBLE PRECISION
C     S2      (output)  DOUBLE PRECISION
C             On exit, if SHFT = 'D' and N > 2, C2 and S2 contain the
C             parameters for the second Givens rotation. Otherwise,
C             C2 = 1, S2 = 0.
C
C     METHOD
C
C     Two Givens rotations are properly computed and applied.
C
C     CONTRIBUTOR
C
C     D. Kressner, Technical Univ. Berlin, Germany, June 2001.
C
C     REVISIONS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     July 2009, SLICOT Library version of the routine PLASHF.
C     V. Sima, Apr. 2018, Oct. 2019, Dec. 2019.
C
C     KEYWORDS
C
C     Eigenvalues, QZ algorithm, periodic QZ algorithm, orthogonal
C     transformation.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE, TWO
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0 )
C     .. Scalar Arguments ..
      CHARACTER         SHFT
      INTEGER           K, LDA1, LDA2, N, SINV
      DOUBLE PRECISION  C1, C2, S1, S2
C     .. Array Arguments ..
      INTEGER           AMAP(*), S(*)
      DOUBLE PRECISION  A(LDA1,LDA2,*)
C     .. Local Scalars ..
      LOGICAL           SGLE
      INTEGER           AI, I
      DOUBLE PRECISION  ALPHA, BETA, C3, DELTA, GAMMA, S3, TEMP
C     .. External Functions ..
      LOGICAL           LSAME
      EXTERNAL          LSAME
C     .. External Subroutines ..
      EXTERNAL          DLARTG
C     .. Intrinsic Functions ..
      INTRINSIC         SQRT
C
C     .. Executable Statements ..
C
      SGLE = LSAME( SHFT, 'S' )
      C1 = ONE
      S1 = ZERO
      C2 = 1/SQRT( TWO )
      S2 = C2
C
      DO 10  I = K, 2, -1
         AI = AMAP(I)
         IF ( S(AI).EQ.SINV ) THEN
            ALPHA = C2 * A(1,1,AI)
            GAMMA = S2 * A(N,N,AI)
            BETA  = S2 * A(N-1,N,AI)
            BETA  = C1 * BETA + S1 * A(N-1,N-1,AI)
            CALL DLARTG( ALPHA, GAMMA, C2, S2, TEMP )
            TEMP = C1 * TEMP
            CALL DLARTG( TEMP, BETA, C1, S1, ALPHA )
         ELSE
            TEMP  = A(1,1,AI)
            BETA  = S2 * TEMP
            TEMP  = C2 * TEMP
            ALPHA = S1 * TEMP
            GAMMA = A(N,N,AI)
            DELTA = C2 * GAMMA
            GAMMA = S2 * GAMMA
            CALL DLARTG( DELTA, BETA, C2, S2, C3 )
            DELTA = C1 * A(N-1,N,AI) - S1 * GAMMA
            ALPHA = C2 * ALPHA - S2 * DELTA
            GAMMA = C1 * A(N-1,N-1,AI)
            CALL DLARTG( GAMMA, ALPHA, C1, S1, TEMP )
         END IF
   10 CONTINUE
C
      AI = AMAP(1)
      ALPHA = A(1,1,AI) * C2 - A(N,N,AI) * S2
      BETA  = C1 * ( C2 * A(2,1,AI) )
      GAMMA = C1 * ( S2 * A(N-1,N,AI) ) + S1 * A(N-1,N-1,AI)
      ALPHA = ALPHA * C1 - A(N,N-1,AI) * S1
      CALL DLARTG( ALPHA, BETA, C1, S1, TEMP )
C
C     This is sufficient for a single real shift.
C
      IF ( SGLE ) THEN
         C2 = ONE
         S2 = ZERO
C
      ELSE
C
         CALL DLARTG( TEMP, GAMMA, C2, S2, ALPHA )
C
C        Rotation 1 is preserved.
C
         ALPHA = C2
         GAMMA = ( A(N-1,N-1,AI) * C1 ) * C2 + A(N,N-1,AI) * S2
         DELTA = ( A(N-1,N-1,AI) * S1 ) * C2
         CALL DLARTG( GAMMA, DELTA, C3, S3, TEMP )
         CALL DLARTG( ALPHA, TEMP, C2, S2, ALPHA )
C
C        Rotation 3 is preserved throughout the following complete loop.
C
         DO 20  I = K, 2, -1
            AI = AMAP(I)
            IF ( S(AI).EQ.SINV ) THEN
               ALPHA = ( A(1,1,AI) * C1 + A(1,2,AI) * S1 ) * C2
               BETA  = ( A(2,2,AI) * S1 ) * C2
               GAMMA = A(N-1,N-1,AI) * S2
               CALL DLARTG( ALPHA, BETA, C1, S1, TEMP )
               CALL DLARTG( TEMP, GAMMA, C2, S2, ALPHA )
            ELSE
               ALPHA =  C1 * A(1,1,AI)
               GAMMA =  S1 * A(1,1,AI)
               BETA  =  C1 * A(1,2,AI) + S1 * A(2,2,AI)
               DELTA = -S1 * A(1,2,AI) + C1 * A(2,2,AI)
               CALL DLARTG( DELTA, GAMMA, C1, S1, TEMP )
               ALPHA = -ALPHA * S2
               BETA  = -BETA  * S2
               ALPHA = C1 * ALPHA + S1 * BETA
               BETA  = C2 * A(N-1,N-1,AI)
               CALL DLARTG( BETA, ALPHA, C2, S2, TEMP )
               S2 = -S2
            END IF
   20    CONTINUE
C
C        Last step: Let the rotations collap into A.
C
         AI = AMAP(1)
         ALPHA = C1 * A(1,1,AI) + S1 * A(1,2,AI)
         BETA  = C1 * A(2,1,AI) + S1 * A(2,2,AI)
         GAMMA = S1 * A(3,2,AI)
         ALPHA = C2 * ALPHA - S2 * C3
         BETA  = C2 * BETA  - S2 * S3
         GAMMA = C2 * GAMMA
         CALL DLARTG( BETA, GAMMA, C2, S2, TEMP )
         CALL DLARTG( ALPHA, TEMP, C1, S1, BETA )
      END IF
      RETURN
C *** Last line of MB03AD ***
      END
