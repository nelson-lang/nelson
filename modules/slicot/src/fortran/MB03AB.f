      SUBROUTINE MB03AB( SHFT, K, N, AMAP, S, SINV, A, LDA1, LDA2, W1,
     $                   W2, C1, S1, C2, S2 )
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
C     Only the rotation defined by C1 and S1 is needed for the real
C     Wilkinson single shift polynomial (see the SLICOT Library routines
C     MB03BE or MB03BF). The shifts are defined based on the eigenvalues
C     (computed externally by the SLICOT Library routine MB03BB) of the
C     trailing 2-by-2 submatrix of the matrix product. See the
C     definitions of the arguments W1 and W2.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     SHFT    CHARACTER*1
C             Specifies the number and type of shifts employed by the
C             shift polynomial, as follows:
C             = 'C':  two complex conjugate shifts;
C             = 'D':  two real identical shifts;
C             = 'R':  two real shifts;
C             = 'S':  one real shift.
C             When the eigenvalues are complex conjugate, this argument
C             must be set to 'C'.
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
C     AMAP    (input) INTEGER array, dimension (K)
C             The map for accessing the factors, i.e., if AMAP(I) = J,
C             then the factor A_I is stored at the J-th position in A.
C             AMAP(1) is the pointer to the Hessenberg matrix, defined
C             by A(:,:,AMAP(1)).
C
C     S       (input)  INTEGER array, dimension (K)
C             The signature array. Each entry of S must be 1 or -1.
C
C     SINV    (input) INTEGER
C             Signature multiplier. Entries of S are virtually
C             multiplied by SINV.
C
C     A       (input)  DOUBLE PRECISION array, dimension (LDA1,LDA2,K)
C             The leading N-by-N-by-K part of this array must contain an
C             n-by-n product (implicitly represented by its K factors)
C             in periodic upper Hessenberg form.
C
C     LDA1    INTEGER
C             The first leading dimension of the array A.  LDA1 >= N.
C
C     LDA2    INTEGER
C             The second leading dimension of the array A.  LDA2 >= N.
C
C     W1      (input)  DOUBLE PRECISION
C             The real part of the first eigenvalue.
C             If SHFT = 'S', this argument is not used.
C
C     W2      (input)  DOUBLE PRECISION
C             The second eigenvalue, if both eigenvalues are real, else
C             the imaginary part of the complex conjugate pair.
C
C     C1      (output)  DOUBLE PRECISION
C     S1      (output)  DOUBLE PRECISION
C             On exit, C1 and S1 contain the parameters for the first
C             Givens rotation.
C
C     C2      (output)  DOUBLE PRECISION
C     S2      (output)  DOUBLE PRECISION
C             On exit, C2 and S2 contain the parameters for the second
C             Givens rotation. If SHFT = 'S', C2 = 1, S2 = 0.
C
C     METHOD
C
C     Givens rotations are properly computed and applied.
C
C     CONTRIBUTOR
C
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     Nov. 2019.
C
C     REVISIONS
C
C     V. Sima, Oct. 2020.
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
      CHARACTER         SHFT
      INTEGER           K, LDA1, LDA2, N, SINV
      DOUBLE PRECISION  C1, C2, S1, S2, W1, W2
C     .. Array Arguments ..
      INTEGER           AMAP(*), S(*)
      DOUBLE PRECISION  A(LDA1,LDA2,*)
C     .. Local Scalars ..
      LOGICAL           ISR, SGLE
      INTEGER           AI, I
      DOUBLE PRECISION  ALPHA, BETA, C23, C3, CX, CY, DELTA, DUM, GAMMA,
     $                  P, S3, SX, SY, TEMP, TMP
C     .. External Functions ..
      LOGICAL           LSAME
      EXTERNAL          LSAME
C     .. External Subroutines ..
      EXTERNAL          DLARTG
C
C     .. Executable Statements ..
C
      SGLE = LSAME( SHFT, 'S' )
      AI   = AMAP(1)
      CALL DLARTG( A(2,1,AI), ONE,  C1, S1, TEMP )
      CALL DLARTG( A(1,1,AI), TEMP, C2, S2, TMP  )
C
      DO 10  I = K, 2, -1
         AI = AMAP(I)
         IF ( S(AI).EQ.SINV ) THEN
            ALPHA = A(1,1,AI)*C2 + A(1,2,AI)*C1*S2
            BETA  = A(2,2,AI)*C1
            GAMMA = S1
            CALL DLARTG( BETA,  GAMMA,   C1, S1, TEMP )
            CALL DLARTG( ALPHA, TEMP*S2, C2, S2, DUM )
        ELSE
            ALPHA = S2*A(1,1,AI)
            BETA  = C1*C2*A(2,2,AI) - S2*A(1,2,AI)
            GAMMA = S1*A(2,2,AI)
            CX    = C1
            SX    = S1
            CALL DLARTG( CX, GAMMA, C1, S1, TEMP )
            TEMP = C1*BETA + SX*C2*S1
            CALL DLARTG( TEMP, ALPHA, C2, S2, DUM )
         END IF
   10 CONTINUE
C
      ISR = .NOT.LSAME( SHFT, 'C' )
      IF ( ISR ) THEN
         CALL DLARTG( C2 - W2*S1*S2, C1*S2, C2, S2, TEMP )
         IF ( SGLE ) THEN
            C1 = C2
            S1 = S2
            C2 = ONE
            S2 = ZERO
C
C           Return.
C
            GO TO 30
         ELSE
C
C           Save C2 and S2 for the final use.
C
            CX  = C2
            SX  = S2
         END IF
      ELSE
         TEMP  = S1*S2
         ALPHA = C2 - W1*TEMP
         BETA  = C1*S2
         GAMMA = W2*TEMP
         CALL DLARTG( BETA, GAMMA, C1, S1, TEMP )
         CALL DLARTG( ALPHA, TEMP, C2, S2, DUM  )
C
C        Save C1, S1, C2, and S2 for the final use.
C
         CX = C1
         SX = S1
         CY = C2
         SY = S2
         S2 = C1*S2
      END IF
C
      I  = 1
      AI = AMAP(I)
C
      ALPHA = A(1,2,AI)*S2 + A(1,1,AI)*C2
      BETA  = A(2,2,AI)*S2 + A(2,1,AI)*C2
      GAMMA = A(3,2,AI)*S2
      CALL DLARTG( GAMMA, ONE,  C1, S1, TEMP )
      CALL DLARTG( BETA,  TEMP, C3, S3, DUM )
      CALL DLARTG( ALPHA, C3*BETA + S3*TEMP, C2, S2, DUM )
C
      DO 20  I = K, 2, -1
         AI = AMAP(I)
         IF ( S(AI).EQ.SINV ) THEN
            TEMP  = C1*S3
            ALPHA = ( A(1,3,AI)*TEMP + A(1,2,AI)*C3 )*S2 + A(1,1,AI)*C2
            BETA  = ( A(2,3,AI)*TEMP + A(2,2,AI)*C3 )*S2
            GAMMA =   A(3,3,AI)*C1
            DELTA =   S1
            CALL DLARTG( GAMMA, DELTA, C1, S1, TEMP )
            TEMP = TEMP*S2*S3
            CALL DLARTG( BETA,  TEMP, C3, S3, TMP )
            CALL DLARTG( ALPHA, TMP,  C2, S2, DUM )
         ELSE
            C23   = C2*C3
            TMP   = C2*S3
            ALPHA = C1*C3*A(3,3,AI)  - S3*A(2,3,AI)
            BETA  = S1*C3
            GAMMA = C1*TMP*A(3,3,AI) + C23*A(2,3,AI) - S2*A(1,3,AI)
            DELTA = S1*TMP
            TMP   = C1
            CALL DLARTG( TMP,  S1*A(3,3,AI), C1, S1, DUM )
            TEMP = ALPHA*C1 + BETA*S1
            CALL DLARTG( TEMP, S3*A(2,2,AI), C3, S3, TMP )
            TEMP = ( C23*A(2,2,AI) - S2*A(1,2,AI) )*C3 +
     $             ( GAMMA*C1 + DELTA*S1 )*S3
            CALL DLARTG( TEMP, S2*A(1,1,AI), C2, S2, DUM )
         END IF
   20 CONTINUE
C
C     Last step: let the rotations collap into the first factor.
C
      IF ( ISR ) THEN
         TEMP  = W1*S1*S3
         ALPHA =   C2 - CX*TEMP*S2
         BETA  = ( C3 - SX*TEMP )*S2
         GAMMA = C1*S2*S3
      ELSE
         P     = S1*S3
         ALPHA = C2 + ( W2*SX*SY - W1*CY )*P*S2
         BETA  = C3 - W1*CX*SY*P
         GAMMA = C1*S3
         P     = S2
      END IF
      CALL DLARTG( BETA, GAMMA, C2, S2, TEMP )
      IF ( .NOT.ISR )
     $   TEMP = TEMP*P
      CALL DLARTG( ALPHA, TEMP, C1, S1, DUM )
C
   30 CONTINUE
      RETURN
C *** Last line of MB03AB ***
      END
