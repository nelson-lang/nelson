      SUBROUTINE MB03BB( BASE, LGBAS, ULP, K, AMAP, S, SINV, A, LDA1,
     $                   LDA2, ALPHAR, ALPHAI, BETA, SCAL, DWORK, INFO )
C
C     PURPOSE
C
C     To compute the eigenvalues of a general 2-by-2 matrix product via
C     a complex single shifted periodic QZ algorithm.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     BASE    (input)  DOUBLE PRECISION
C             Machine base.
C
C     LGBAS   (input)  DOUBLE PRECISION
C             Logarithm of BASE.
C
C     ULP     (input)  DOUBLE PRECISION
C             Machine precision.
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
C     A       (input)  DOUBLE PRECISION array, dimension (LDA1,LDA2,K)
C             On entry, the leading 2-by-2-by-K part of this array must
C             contain a 2-by-2 product (implicitly represented by its K
C             factors) in upper Hessenberg-triangular form.
C
C     LDA1    INTEGER
C             The first leading dimension of the array A.  LDA1 >= 2.
C
C     LDA2    INTEGER
C             The second leading dimension of the array A.  LDA2 >= 2.
C
C     ALPHAR  (output)  DOUBLE PRECISION array, dimension (2)
C             On exit, this array contains the scaled real part of the
C             two eigenvalues. If BETA(I) <> 0, then the I-th eigenvalue
C             (I = 1 : 2) is given by
C                 (ALPHAR(I) + ALPHAI(I)*SQRT(-1) ) * (BASE)**SCAL(I).
C
C     ALPHAI  (output)  DOUBLE PRECISION array, dimension (2)
C             On exit, this array contains the scaled imaginary part of
C             the two eigenvalues. ALPHAI(1) >= 0.
C
C     BETA    (output)  DOUBLE PRECISION array, dimension (2)
C             On exit, this array contains information about infinite
C             eigenvalues. If BETA(I) = 0, then the I-th eigenvalue is
C             infinite. Otherwise, BETA(I) = 1.0.
C
C     SCAL    (output)  INTEGER array, dimension (2)
C             On exit, this array contains the scaling exponents for the
C             two eigenvalues.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (8*K)
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             = 1:  the periodic QZ algorithm did not converge;
C             = 2:  the computed eigenvalues might be inaccurate.
C                   Both values might be taken as warnings, since
C                   approximations of eigenvalues are returned.
C
C     METHOD
C
C     A complex single shifted periodic QZ iteration is applied.
C
C     CONTRIBUTOR
C
C     D. Kressner, Technical Univ. Berlin, Germany, June 2001.
C
C     REVISIONS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     July 2009, SLICOT Library version of the routine PLACP2.
C     V. Sima, June 2010, July 2010, Aug. 2011, Sep. 2011, Oct. 2011.
C
C     KEYWORDS
C
C     Eigenvalues, QZ algorithm, periodic QZ algorithm, orthogonal
C     transformation.
C
C     ******************************************************************
C
C     .. Parameters ..
      COMPLEX*16        CZERO, CONE
      PARAMETER         ( CZERO = ( 0.0D0, 0.0D0 ),
     $                    CONE  = ( 1.0D0, 0.0D0 ) )
      DOUBLE PRECISION  ZERO, ONE, TWO
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0 )
C     .. Scalar Arguments ..
      INTEGER           INFO, K, LDA1, LDA2, SINV
      DOUBLE PRECISION  BASE, LGBAS, ULP
C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA1,LDA2,*), ALPHAI(2), ALPHAR(2), BETA(2),
     $                  DWORK(*)
      INTEGER           AMAP(*), S(*), SCAL(2)
C     .. Local Scalars ..
      INTEGER           AI, I, IITER, J, PDM, PDW, SL
      DOUBLE PRECISION  CS, CST, LHS, MISC, MISR, RHS, TEMPI, TEMPR
      COMPLEX*16        SN, SNT, TEMP
C     .. Local Arrays ..
      COMPLEX*16        T(2,2), Z(3,3)
C     .. External Functions ..
      DOUBLE PRECISION  DLAPY2
      EXTERNAL          DLAPY2
C     .. External Subroutines ..
      EXTERNAL          DLADIV, ZLARTG, ZROT
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, DCMPLX, DCONJG, DBLE, DIMAG, DREAL, INT,
     $                  LOG, MAX, MIN, MOD, SQRT
C
C     .. Executable Statements ..
C
C     Apply a complex single shifted periodic QZ iteration.
C     This might not be efficient but it seems to be reliable.
C
      INFO = 0
      PDW  = 0
C
      DO 10 I = 1, K
         AI = AMAP(I)
         DWORK(PDW+1) = A(1,1,AI)
         DWORK(PDW+2) = ZERO
         DWORK(PDW+3) = A(2,1,AI)
         DWORK(PDW+4) = ZERO
         DWORK(PDW+5) = A(1,2,AI)
         DWORK(PDW+6) = ZERO
         DWORK(PDW+7) = A(2,2,AI)
         DWORK(PDW+8) = ZERO
         PDW = PDW + 8
   10 CONTINUE
C
      PDM = PDW
C
      DO 40  IITER = 1, 80
C
C        Test for deflation.
C
         LHS = DLAPY2( DWORK(3), DWORK(4) )
         RHS = MAX( DLAPY2( DWORK(1), DWORK(2) ),
     $              DLAPY2( DWORK(7), DWORK(8) ) )
         IF ( RHS.EQ.ZERO )
     $      RHS = DLAPY2( DWORK(5), DWORK(6) )
         IF ( LHS.LE.ULP*RHS )
     $      GO TO 50
C
C        Start Iteration.
C
         IF ( IITER.EQ.1 ) THEN
C
C           Compute a randomly chosen initial unitary shift.
C
            CALL ZLARTG( DCMPLX( ONE, -TWO ), DCMPLX( TWO, TWO ), CS,
     $                   SN, TEMP )
         ELSE IF ( MOD( IITER, 40 ).EQ.0 ) THEN
C
C           Ad hoc shift.
C
            CALL ZLARTG( DCMPLX( DBLE( I ), ONE ), DCMPLX( ONE, -TWO ),
     $                   CS, SN, TEMP )
         ELSE
C
C           Compute the shift by a product QR decomposition.
C
            CS = ONE
            SN = CZERO
            CALL ZLARTG( CONE, CONE, CST, SNT, TEMP )
            PDW = PDM
C
            DO 20  I = K, 2, -1
               PDW = PDW - 8
               TEMP = DCMPLX( DWORK(PDW+1), DWORK(PDW+2) )
               Z(1,1) = TEMP
               Z(2,1) = CZERO
               Z(3,1) = CZERO
               Z(1,2) = CZERO
               Z(2,2) = TEMP
               Z(3,2) = DCMPLX( DWORK(PDW+3), DWORK(PDW+4) )
               Z(1,3) = CZERO
               Z(2,3) = DCMPLX( DWORK(PDW+5), DWORK(PDW+6) )
               Z(3,3) = DCMPLX( DWORK(PDW+7), DWORK(PDW+8) )
               IF ( S(AMAP(I)).EQ.SINV ) THEN
                  CALL ZROT( 3, Z(1,1), 1, Z(1,3), 1, CST, DCONJG( SNT )
     $                      )
                  CALL ZROT( 3, Z(1,1), 1, Z(1,2), 1, CS,  DCONJG( SN )
     $                      )
                  CALL ZLARTG( Z(1,1), Z(3,1), CST, SNT, TEMP )
                  CALL ZLARTG( TEMP, Z(2,1), CS, SN, TEMP )
               ELSE
                  CALL ZROT( 3, Z(1,1), 3, Z(3,1), 3, CST, SNT )
                  CALL ZROT( 3, Z(1,1), 3, Z(2,1), 3, CS, SN )
                  TEMP = Z(3,3)
                  CALL ZLARTG( TEMP, Z(3,1), CST, SNT, Z(3,3) )
                  SNT = -SNT
                  CALL ZROT( 2, Z(1,1), 1, Z(1,3), 1, CST, DCONJG( SNT )
     $                      )
                  TEMP = Z(2,2)
                  CALL ZLARTG( TEMP, Z(2,1), CS, SN, Z(2,2) )
                  SN = -SN
               END IF
   20       CONTINUE
C
            PDW = 0
            Z(1,1) =  DCMPLX( DWORK(PDW+1), DWORK(PDW+2) )
            Z(2,1) =  DCMPLX( DWORK(PDW+3), DWORK(PDW+4) )
            Z(1,2) = -DCMPLX( DWORK(PDW+3), DWORK(PDW+4) )
            Z(2,2) =  CZERO
            Z(1,3) = -DCMPLX( DWORK(PDW+7), DWORK(PDW+8) )
            Z(2,3) =  CZERO
            CALL ZROT( 2, Z(1,1), 1, Z(1,3), 1, CST, DCONJG( SNT ) )
            CALL ZROT( 2, Z(1,1), 1, Z(1,2), 1, CS,  DCONJG( SN ) )
            CALL ZLARTG( Z(1,1), Z(2,1), CS, SN, TEMP )
         END IF
         CST = CS
         SNT = SN
         PDW = PDM
C
         DO 30 I = K, 2, -1
            PDW = PDW - 8
            T(1,1) = DCMPLX( DWORK(PDW+1), DWORK(PDW+2) )
            T(2,1) = DCMPLX( DWORK(PDW+3), DWORK(PDW+4) )
            T(1,2) = DCMPLX( DWORK(PDW+5), DWORK(PDW+6) )
            T(2,2) = DCMPLX( DWORK(PDW+7), DWORK(PDW+8) )
            IF ( S(AMAP(I)).EQ.SINV) THEN
               CALL ZROT( 2, T(1,1), 1, T(1,2), 1, CS, DCONJG( SN ) )
               TEMP = T(1,1)
               CALL ZLARTG( TEMP, T(2,1), CS, SN, T(1,1) )
               T(2,1) = CZERO
               CALL ZROT( 1, T(1,2), 2, T(2,2), 2, CS, SN )
            ELSE
               CALL ZROT( 2, T(1,1), 2, T(2,1), 2, CS, SN )
               TEMP = T(2,2)
               CALL ZLARTG( TEMP, T(2,1), CS, SN, T(2,2) )
               T(2,1) = CZERO
               SN = -SN
               CALL ZROT( 1, T(1,1), 1, T(1,2), 1, CS, DCONJG( SN ) )
            END IF
            DWORK(PDW+1) = DREAL( T(1,1) )
            DWORK(PDW+2) = DIMAG( T(1,1) )
            DWORK(PDW+3) = DREAL( T(2,1) )
            DWORK(PDW+4) = DIMAG( T(2,1) )
            DWORK(PDW+5) = DREAL( T(1,2) )
            DWORK(PDW+6) = DIMAG( T(1,2) )
            DWORK(PDW+7) = DREAL( T(2,2) )
            DWORK(PDW+8) = DIMAG( T(2,2) )
   30    CONTINUE
C
         PDW = 0
         T(1,1) = DCMPLX( DWORK(PDW+1), DWORK(PDW+2) )
         T(2,1) = DCMPLX( DWORK(PDW+3), DWORK(PDW+4) )
         T(1,2) = DCMPLX( DWORK(PDW+5), DWORK(PDW+6) )
         T(2,2) = DCMPLX( DWORK(PDW+7), DWORK(PDW+8) )
         CALL ZROT( 2, T(1,1), 2, T(2,1), 2, CST, SNT )
         CALL ZROT( 2, T(1,1), 1, T(1,2), 1, CS, DCONJG( SN ) )
         DWORK(PDW+1) = DREAL( T(1,1) )
         DWORK(PDW+2) = DIMAG( T(1,1) )
         DWORK(PDW+3) = DREAL( T(2,1) )
         DWORK(PDW+4) = DIMAG( T(2,1) )
         DWORK(PDW+5) = DREAL( T(1,2) )
         DWORK(PDW+6) = DIMAG( T(1,2) )
         DWORK(PDW+7) = DREAL( T(2,2) )
         DWORK(PDW+8) = DIMAG( T(2,2) )
   40 CONTINUE
C
C     Not converged. Set INFO = 1, but continue. 
C
      INFO = 1
C
   50 CONTINUE
C
C     Converged.
C
      DO 70  J = 1, 2
         PDW = 0
         IF ( J.EQ.2 )
     $      PDW  = 6
         TEMPI   = ZERO
         TEMPR   = ONE
         BETA(J) = ONE
         SCAL(J) = 0
C
         DO 60  I = 1, K
            RHS = DLAPY2( DWORK(PDW+1), DWORK(PDW+2) )
            IF ( RHS.NE.ZERO ) THEN
               SL = INT( LOG( RHS ) / LGBAS )
               DWORK(PDW+1) = DWORK(PDW+1) / ( BASE**DBLE( SL ) )
               DWORK(PDW+2) = DWORK(PDW+2) / ( BASE**DBLE( SL ) )
            ELSE
               SL = 0
            END IF
            IF ( S(AMAP(I)).EQ.1 ) THEN
               LHS = TEMPI
               TEMPI = TEMPR*DWORK(PDW+2) + TEMPI*DWORK(PDW+1)
               TEMPR = TEMPR*DWORK(PDW+1) -   LHS*DWORK(PDW+2)
               SCAL(J) = SCAL(J) + SL
            ELSE IF ( RHS.EQ.ZERO ) THEN
               BETA(J) = ZERO
            ELSE
               LHS = TEMPR
               RHS = TEMPI
               CALL DLADIV( LHS, RHS, DWORK(PDW+1), DWORK(PDW+2),
     $                      TEMPR, TEMPI )
               SCAL(J) = SCAL(J) - SL
            END IF
            IF ( ( MOD( I, 10 ).EQ.0 ) .OR. ( I.EQ.K ) ) THEN
               RHS = DLAPY2( TEMPR, TEMPI )
               IF ( RHS.EQ.ZERO ) THEN
                  SCAL(J) = 0
               ELSE
                  SL = INT( LOG( RHS ) / LGBAS )
                  TEMPR = TEMPR / ( BASE**DBLE( SL ) )
                  TEMPI = TEMPI / ( BASE**DBLE( SL ) )
                  SCAL(J) = SCAL(J) + SL
               END IF
            END IF
            PDW = PDW + 8
   60    CONTINUE
C
         ALPHAR(J) = TEMPR
         ALPHAI(J) = TEMPI
   70 CONTINUE
C
      IF ( TEMPI.GT.ZERO ) THEN
         ALPHAR(2) = ALPHAR(1)
         ALPHAI(2) = ALPHAI(1)
         ALPHAR(1) = TEMPR
         ALPHAI(1) = TEMPI
         TEMPR     = BETA(2)
         BETA(2)   = BETA(1)
         BETA(1)   = TEMPR
         TEMPR     = SCAL(2)
         SCAL(2)   = SCAL(1)
         SCAL(1)   = TEMPR
      END IF
C
C     Enforce the needed eigenvalue structure for real matrices.
C
      IF ( ALPHAI(1).NE.ZERO .OR. ALPHAI(2).NE.ZERO ) THEN
C
C        Decide if there are two real or complex conjugate eigenvalues.
C
         IF ( SCAL(1).GE.SCAL(2) ) THEN
            SL    = SCAL(1) - SCAL(2)
            TEMPR = ALPHAR(2) / BASE**DBLE( SL )
            TEMPI = ALPHAI(2) / BASE**DBLE( SL )
            LHS   = ALPHAR(1) - TEMPR
            RHS   = ALPHAI(1) + TEMPI
            CST   = ALPHAI(1)
         ELSE
            SL    = SCAL(2) - SCAL(1)
            TEMPR = ALPHAR(1) / BASE**DBLE( SL )
            TEMPI = ALPHAI(1) / BASE**DBLE( SL )
            LHS   = ALPHAR(2) - TEMPR
            RHS   = ALPHAI(2) + TEMPI
            CST   = ALPHAI(2)
         END IF
C
         MISR = DLAPY2( CST, TEMPI )
         MISC = DLAPY2( LHS, RHS ) / TWO
C
         CS = MAX( DLAPY2( ALPHAR(1), ALPHAI(1) ), ONE,
     $             DLAPY2( ALPHAR(2), ALPHAI(2) ) )
         IF ( MIN( MISR, MISC ).GT.CS*SQRT( ULP ) )
     $      INFO = 2
C
         IF ( MISR.GT.MISC ) THEN
C
C           Conjugate eigenvalues.
C
            IF ( SCAL(1).GE.SCAL(2) ) THEN
               ALPHAR(1) =    ( ALPHAR(1) + TEMPR ) / TWO
               ALPHAI(1) = ABS( ALPHAI(1) - TEMPI ) / TWO
               SCAL(2)   = SCAL(1)
            ELSE
               ALPHAR(1) =    ( ALPHAR(2) + TEMPR ) / TWO
               ALPHAI(1) = ABS( ALPHAI(2) - TEMPI ) / TWO
               SCAL(1)   = SCAL(2)
            END IF
            ALPHAR(2) =  ALPHAR(1)
            ALPHAI(2) = -ALPHAI(1)
         ELSE
C
C           Two real eigenvalues.
C
            ALPHAI(1) = ZERO
            ALPHAI(2) = ZERO
         END IF
      END IF
C
      RETURN
C *** Last line of MB03BB ***
      END
