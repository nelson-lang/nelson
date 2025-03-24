      SUBROUTINE MB03BC( K, AMAP, S, SINV, A, LDA1, LDA2, MACPAR, CV,
     $                   SV, DWORK )
C
C     PURPOSE
C
C     To compute the product singular value decomposition of the K-1
C     triangular factors corresponding to a 2-by-2 product of K
C     factors in upper Hessenberg-triangular form.
C     For a general product of 2-by-2 triangular matrices
C
C                        S(2)        S(3)            S(K)
C            A = A(:,:,2)    A(:,:,3)    ... A(:,:,K),
C
C     Givens rotations are computed so that
C                                                          S(i)
C       [  CV(i-1) SV(i-1) ] [ A(1,1,i)(in)  A(1,2,i)(in) ]
C       [ -SV(i-1) CV(i-1) ] [     0         A(2,2,i)(in) ]
C                                      S(i)
C       [ A(1,1,i)(out) A(1,2,i)(out) ]    [  CV(i) SV(i) ]
C     = [     0         A(2,2,i)(out) ]    [ -SV(i) CV(i) ]
C
C     stays upper triangular and
C
C       [  CV(1) SV(1) ]       [ CV(K) -SV(K) ]
C       [ -SV(1) CV(1) ] * A * [ SV(K)  CV(K) ]
C
C     is diagonal.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     K       (input)  INTEGER
C             The number of factors.  K >= 1.
C
C     AMAP    (input) INTEGER array, dimension (K)
C             The map for accessing the factors, i.e., if AMAP(I) = J,
C             then the factor A_I is stored at the J-th position in A.
C
C     S       (input)  INTEGER array, dimension (K)
C             The signature array. Each entry of S must be 1 or -1.
C
C     SINV    (input) INTEGER
C             Signature multiplier. Entries of S are virtually
C             multiplied by SINV.
C
C     A       (input/output)  DOUBLE PRECISION array, dimension
C                             (LDA1,LDA2,K)
C             On entry, the leading 2-by-2-by-K part of this array must
C             contain a 2-by-2 product (implicitly represented by its K
C             factors) in upper Hessenberg-triangular form.
C             On exit, the leading 2-by-2-by-K part of this array
C             contains modified triangular factors such that their
C             product is diagonal.
C
C     LDA1    INTEGER
C             The first leading dimension of the array A.  LDA1 >= 2.
C
C     LDA2    INTEGER
C             The second leading dimension of the array A.  LDA2 >= 2.
C
C     MACPAR  (input)  DOUBLE PRECISION array, dimension (5)
C             Machine parameters:
C             MACPAR(1)  overflow threshold,         DLAMCH( 'O' );
C             MACPAR(2)  underflow threshold,        DLAMCH( 'U' );
C             MACPAR(3)  safe minimum,               DLAMCH( 'S' );
C             MACPAR(4)  relative machine precision, DLAMCH( 'E' );
C             MACPAR(5)  base of the machine,        DLAMCH( 'B' ).
C
C     CV      (output)  DOUBLE PRECISION array, dimension (K)
C             On exit, the first K elements of this array contain the
C             cosines of the Givens rotations.
C
C     SV      (output)  DOUBLE PRECISION array, dimension (K)
C             On exit, the first K elements of this array contain the
C             sines of the Givens rotations.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (3*(K-1))
C
C     METHOD
C
C     The product singular value decomposition of the K-1
C     triangular factors are computed as described in [1].
C
C     REFERENCES
C
C     [1] Bojanczyk, A. and Van Dooren, P.
C         On propagating orthogonal transformations in a product of 2x2
C         triangular matrices.
C         In Reichel, Ruttan and Varga: 'Numerical Linear Algebra',
C         pp. 1-9, 1993.
C
C     CONTRIBUTOR
C
C     D. Kressner, Technical Univ. Berlin, Germany, June 2001.
C
C     REVISIONS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     July 2009, SLICOT Library version of the routine PLAPST.
C     V. Sima, Nov. 2010, Aug. 2011.
C
C     KEYWORDS
C
C     Eigenvalues, orthogonal transformation, singular values,
C     singular value decomposition.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE, HALF, TWO
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0, HALF = 0.5D0,
     $                    TWO = 2.0D0 )
C     .. Scalar Arguments ..
      INTEGER           K, LDA1, LDA2, SINV
C     .. Array Arguments ..
      INTEGER           AMAP(*), S(*)
      DOUBLE PRECISION  A(LDA1,LDA2,*), CV(*), DWORK(*), MACPAR(*),
     $                  SV(*)
C     .. Local Scalars ..
      INTEGER           AI, I, PW, SCL
      DOUBLE PRECISION  A11, A12, A22, B11, B12, B22, BASE, CC, CL, CR,
     $                  EPS, MX, MX2, RMAX, RMIN, RMNS, RMXS, S11, S22,
     $                  SC, SFMN, SL, SR, SSMAX, SSMIN, T11, T12, T22,
     $                  TEMP, TWOS
C     .. External Subroutines ..
      EXTERNAL          DLARTG, DLASV2
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, MAX, SQRT
C
C     .. Executable Statements ..
C
      RMAX = MACPAR(1)
      RMXS = SQRT( RMAX )
      RMIN = MACPAR(2)
      RMNS = SQRT( RMIN )
      SFMN = MACPAR(3)
      EPS  = MACPAR(4)
      BASE = MACPAR(5)
      TWOS = SQRT( TWO )
C
C     Compute the product of the 2-by-2 triangular matrices.
C
      PW  = 1
      T11 = ONE
      T12 = ZERO
      T22 = ONE
C
      DO 60  I = 2, K
         AI  = AMAP(I)
         A11 = A(1,1,AI)
         A12 = A(1,2,AI)
         A22 = A(2,2,AI)
         IF ( S(AI).NE.SINV ) THEN
            TEMP =  A11
            A11  =  A22
            A22  =  TEMP
            A12  = -A12
         END IF
C
C        A and T are scaled so that the elements of the resulting
C        product do not overflow.
C
         MX  = ABS( A11 ) / RMXS
         MX2 = ABS( T11 ) / RMXS
   10    CONTINUE
         IF ( MX*MX2.GE.ONE ) THEN
            IF (  MX.GE.ONE ) THEN
               MX  =  MX / BASE
               A11 = A11 / BASE
               A22 = A22 / BASE
               A12 = A12 / BASE
            END IF
            IF ( MX2.GE.ONE ) THEN
               MX2 = MX2 / BASE
               T11 = T11 / BASE
               T22 = T22 / BASE
               T12 = T12 / BASE
            END IF
            GOTO 10
         END IF
C
         MX  = ABS( A22 ) / RMXS
         MX2 = ABS( T22 ) / RMXS
   20    CONTINUE
         IF ( MX*MX2.GE.ONE ) THEN
            IF (  MX.GE.ONE ) THEN
               MX  = MX  / BASE
               A11 = A11 / BASE
               A22 = A22 / BASE
               A12 = A12 / BASE
            END IF
            IF ( MX2.GE.ONE ) THEN
               MX2 = MX2 / BASE
               T11 = T11 / BASE
               T22 = T22 / BASE
               T12 = T12 / BASE
            END IF
            GOTO 20
         END IF
C
         MX  = ABS( A12 ) / RMXS
         MX2 = ABS( T11 ) / RMXS
   30    CONTINUE
         IF ( MX*MX2.GE.HALF ) THEN
            IF (  MX.GE.HALF ) THEN
               MX  = MX  / BASE
               A11 = A11 / BASE
               A22 = A22 / BASE
               A12 = A12 / BASE
            END IF
            IF ( MX2.GE.HALF ) THEN
               MX2 = MX2 / BASE
               T11 = T11 / BASE
               T22 = T22 / BASE
               T12 = T12 / BASE
            END IF
            GOTO 30
         END IF
C
         MX  = ABS( A22 ) / RMXS
         MX2 = ABS( T12 ) / RMXS
   40    CONTINUE
         IF ( MX*MX2.GE.HALF ) THEN
            IF (  MX.GE.HALF ) THEN
               MX  = MX  / BASE
               A11 = A11 / BASE
               A22 = A22 / BASE
               A12 = A12 / BASE
            END IF
            IF ( MX2.GE.HALF ) THEN
               MX2 = MX2 / BASE
               T11 = T11 / BASE
               T22 = T22 / BASE
               T12 = T12 / BASE
            END IF
            GOTO 40
         END IF
C
C        Avoid underflow if possible.
C
         MX  = MAX( ABS( A11 ), ABS( A22 ), ABS( A12 ) )
         MX2 = MAX( ABS( T11 ), ABS( T22 ), ABS( T12 ) )
         IF ( MX.NE.ZERO .AND. MX2.NE.ZERO ) THEN
   50       CONTINUE
            IF ( ( MX.LE.( ONE/RMNS ) .AND. MX2.LE.RMNS ) .OR.
     $           ( MX.LE.RMNS .AND. MX2.LE.( ONE/RMNS ) ) )
     $      THEN
               IF ( MX.LE.MX2 ) THEN
                  MX  = MX  * BASE
                  A11 = A11 * BASE
                  A22 = A22 * BASE
                  A12 = A12 * BASE
               ELSE
                  MX2 = MX2 * BASE
                  T11 = T11 * BASE
                  T22 = T22 * BASE
                  T12 = T12 * BASE
               END IF
               GOTO 50
            END IF
         END IF
         T12 = T11 * A12 + T12 * A22
         T11 = T11 * A11
         T22 = T22 * A22
         IF ( I.LT.K ) THEN
            DWORK(PW)   = T11
            DWORK(PW+1) = T12
            DWORK(PW+2) = T22
            PW = PW + 3
         END IF
   60 CONTINUE
C
C     Compute the SVD of this product avoiding unnecessary
C     overflow/underflow in the singular values.
C
      TEMP = MAX( ABS( T11 / TWO ) + ABS( T12 / TWO ),
     $            ABS( T22 / TWO ) )
      IF ( TEMP.GT.( RMAX/( TWO * TWOS ) ) ) THEN
         TEMP = TEMP / BASE
         T11  =  T11 / BASE
         T12  =  T12 / BASE
         T22  =  T22 / BASE
      END IF
   70 CONTINUE
      IF ( TEMP.LT.( RMAX/( TWO * BASE * TWOS ) ) .AND.
     $     T11.NE.ZERO .AND. T22.NE.ZERO ) THEN
         SCL = 0
         IF ( ABS( T22 ).LE.TWOS * RMIN ) THEN
            SCL = 1
         ELSE IF ( EPS * ABS( T12 ).GT.ABS( T22 ) ) THEN
            IF ( SQRT( ABS( T11 ) ) * SQRT( ABS( T22 ) ).LE.
     $           ( SQRT( TWOS ) * RMNS ) * SQRT( ABS( T12 ) ) )
     $          SCL = 1
         ELSE
            IF ( ABS( T11 ).LE.TWOS * RMIN *
     $            ( ONE + ABS( T12 / T22 ) ) )
     $         SCL = 1
         END IF
         IF ( SCL.EQ.1 ) THEN
            TEMP = TEMP * BASE
            T11  =  T11 * BASE
            T12  =  T12 * BASE
            T22  =  T22 * BASE
            GOTO 70
         END IF
      END IF
C
      CALL DLASV2( T11, T12, T22, SSMIN, SSMAX, SR, CR, SL, CL )
C
C     Now, the last transformation is propagated to the front as
C     described in [1].
C
      S11 = T11
      S22 = T22
C
      CV(K) = CR
      SV(K) = SR
C
      DO 80  I = K, 2, -1
         AI = AMAP(I)
         IF ( S(AI).EQ.SINV ) THEN
            A11 = A(1,1,AI)
            A12 = A(1,2,AI)
            A22 = A(2,2,AI)
         ELSE
            A11 =  A(2,2,AI)
            A12 = -A(1,2,AI)
            A22 =  A(1,1,AI)
         END IF
         IF ( I.GT.2 ) THEN
            PW = PW - 3
            T11 = DWORK(PW)
            T12 = DWORK(PW+1)
            T22 = DWORK(PW+2)
            IF ( ABS( SR * CL * S22 ).LT.ABS( SL * CR * S11 ) ) THEN
               B11 =  T22
               B22 =  T11
               B12 = -T12
               CC  =  CL
               SC  =  SL
            ELSE
               B11 = A11
               B12 = A12
               B22 = A22
               CC  = CR
               SC  = SR
            END IF
            MX = MAX( ABS( B11 ), ABS( B12 ), ABS( B22 ) )
            IF ( MX.GT.RMAX / TWO ) THEN
               B11 = B11 / TWO
               B22 = B22 / TWO
               B12 = B12 / TWO
            END IF
            CALL DLARTG( B11 * CC + B12 * SC, SC * B22, CC, SC, TEMP )
         ELSE
            CC = CL
            SC = SL
         END IF
         IF ( ABS( SC ).LT.SFMN * ABS( A22 ) ) THEN
            A(1,1,AI) = SC * SR * A22 + CC * ( CR * A11 + SR * A12 )
         ELSE
            A(1,1,AI) = ( A22 / SC ) * SR
         END IF
         IF ( ABS( SR ).LT.SFMN * ABS( A11 ) ) THEN
            A(2,2,AI) = SC * SR * A11 + CR * ( CC * A22 - SC * A12 )
         ELSE
            A(2,2,AI) = ( A11 / SR ) * SC
         END IF
         A(1,2,AI) = ( A12 * CR - A11 * SR ) * CC + A22 * CR * SC
         IF ( S(AI).NE.SINV ) THEN
            TEMP      =  A(1,1,AI)
            A(1,1,AI) =  A(2,2,AI)
            A(2,2,AI) =  TEMP
            A(1,2,AI) = -A(1,2,AI)
         END IF
         CR = CC
         SR = SC
         CV(I-1) = CR
         SV(I-1) = SR
         S11 = T11
         S22 = T22
   80 CONTINUE
C
      CV(1) = CL
      SV(1) = SL
C
      RETURN
C *** Last line of MB03BC ***
      END
