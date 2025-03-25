      SUBROUTINE MB03AI( SHFT, K, N, AMAP, S, SINV, A, LDA1, LDA2, C1,
     $                   S1, C2, S2, DWORK )
C
C     PURPOSE
C
C     To compute two Givens rotations (C1,S1) and (C2,S2)
C     such that the orthogonal matrix
C
C               [ Q  0 ]        [  C1  S1  0 ]   [ 1  0   0  ]
C           Z = [      ],  Q := [ -S1  C1  0 ] * [ 0  C2  S2 ],
C               [ 0  I ]        [  0   0   1 ]   [ 0 -S2  C2 ]
C
C     makes the first column of the real Wilkinson double shift
C     polynomial of the product of matrices in periodic upper Hessenberg
C     form, stored in the array A, parallel to the first unit vector.
C     Only the rotation defined by C1 and S1 is used for the real
C     Wilkinson single shift polynomial (see SLICOT Library routines
C     MB03BE or MB03BF). All factors whose exponents differ from that of 
C     the Hessenberg factor are assumed nonsingular. The matrix product
C     is evaluated.
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
C             The order of the factors.  N >= 2.
C
C     AMAP    (input)  INTEGER array, dimension (K)
C             The map for accessing the factors, i.e., if AMAP(I) = J,
C             then the factor A_I is stored at the J-th position in A.
C             AMAP(K) is the pointer to the Hessenberg matrix.
C             Before calling this routine, AMAP returned by SLICOT
C             Library routine MB03BA should be modified as follows:
C             J = AMAP(1), AMAP(I) = AMAP(I+1), I = 1:K-1, AMAP(K) = J.
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
C             On exit, if SHFT = 'D', C2 and S2 contain the parameters
C             for the second Givens rotation. Otherwise, C2 = 1, S2 = 0.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (N*(N+2))
C
C     METHOD
C
C     The necessary elements of the real Wilkinson double shift
C     polynomial are computed, and suitable Givens rotations are 
C     found. For numerical reasons, this routine should be called
C     when convergence difficulties are encountered for small order
C     matrices and small K, e.g., N, K <= 6. 
C
C     CONTRIBUTOR
C
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     Jan. 2019.
C
C     REVISIONS
C
C     V. Sima, Dec. 2019, Dec. 2020.
C
C     KEYWORDS
C
C     Eigenvalues, QZ algorithm, periodic QZ algorithm, orthogonal
C     transformation.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ONE, TWO, ZERO
      PARAMETER         ( ONE = 1.0D0, TWO = 2.0D0, ZERO = 0.0D0 )
C     .. Scalar Arguments ..
      CHARACTER         SHFT
      INTEGER           K, LDA1, LDA2, N, SINV
      DOUBLE PRECISION  C1, C2, S1, S2
C     .. Array Arguments ..
      INTEGER           AMAP(*), S(*)
      DOUBLE PRECISION  A(LDA1,LDA2,*), DWORK(*)
C     .. Local Scalars ..
      LOGICAL           ISC, SGLE
      INTEGER           I, II, IN1, IN2, IND, IR, J, L, NN
      DOUBLE PRECISION  E1, E2, MD, MXC, MXR, P1, P2, P3, PR, SM
C     .. Local Arrays ..
      DOUBLE PRECISION  Z(1)
C     .. External Functions ..
      LOGICAL           LSAME
      DOUBLE PRECISION  DLAPY2
      EXTERNAL          LSAME, DLAPY2
C     .. External Subroutines ..
      EXTERNAL          DCOPY, DLAHQR, DLARTG, DLASET, DTRMM, DTRMV,
     $                  DTRSM
C     .. Intrinsic Functions ..
      INTRINSIC         ABS
C
C     .. Executable Statements ..
C
C     For efficiency reasons, the parameters are not checked for errors.
C
C     Evaluate the matrix product.
C
      SGLE = LSAME( SHFT, 'S' )
C
      NN = N*N
      IR = NN + 1
      II = IR + N
      CALL DLASET( 'Full', N, N, ZERO, ONE, DWORK, N )
C
      DO 10  J = 1, K - 1
         I = AMAP(J)
         IF ( S(I).EQ.SINV ) THEN
            CALL DTRMM( 'Right', 'Upper', 'NoTran', 'NonUnit', N, N,
     $                  ONE, A(1,1,I), LDA1, DWORK, N )
         ELSE
            CALL DTRSM( 'Right', 'Upper', 'NoTran', 'NonUnit', N, N,
     $                  ONE, A(1,1,I), LDA1, DWORK, N )
         END IF
   10 CONTINUE
C
C     Compute in DWORK(IR:IR+N-1) the last column of the product.
C
      I = AMAP(K)
      CALL DCOPY( N, A(1,N,I), 1, DWORK(IR), 1 )
      CALL DTRMV( 'Upper', 'NoTran', 'NonUnit', N, DWORK, N,
     $            DWORK(IR), 1 )
      J = IR - N
C
      DO 20  L = N - 1, 1, -1
         CALL DCOPY( L+1, A(1,L,I), 1, DWORK(II), 1 )
         CALL DTRMV( 'Upper', 'NoTran', 'NonUnit', L+1, DWORK, N,
     $               DWORK(II), 1 )
         CALL DCOPY( L+1, DWORK(II), 1, DWORK(J), 1 )
         J = J - N
   20 CONTINUE
C
      DO 30  L = 1, N
         CALL DCOPY( L+1, DWORK(J+N), 1, DWORK(J), 1 )
         J = J + N
   30 CONTINUE
C
      IF ( SGLE ) THEN
         CALL DLARTG( DWORK(1) - DWORK(NN), DWORK(2), C1, S1, E1 )
         C2 = ONE
         S2 = ZERO
      ELSE
C
C        Save the needed elements of the product.
C
         E1 = DWORK(1)
         E2 = DWORK(2)
         P1 = DWORK(N+1)
         P2 = DWORK(N+2)
         P3 = DWORK(N+3)
C
C        Compute eigenvalues of the product.
C
         CALL DLAHQR( .FALSE., .FALSE., N, 1, N, DWORK, N, DWORK(IR),
     $                DWORK(II), 1, 1, Z, 1, IND )
C
C        Find two eigenvalues with the largest moduli.
C        If there are complex eigenvalues, selection is based on them.
C
         IND = 0
         IN2 = 0
         ISC = .FALSE.
         MXC = ZERO
         MXR = ZERO
C
         DO 40  I = II, II + N - 1
            IF ( DWORK(I).NE.ZERO ) THEN
               ISC = .TRUE.
               MD  = DLAPY2( DWORK( I-N ), DWORK( I ) )
               IF ( MD.GT.MXC ) THEN
                  MXC = MD
                  IND = I
               END IF
            ELSE
               MD  = ABS( DWORK( I-N ) )
               IN1 = IN2
               IF ( MD.GT.MXR ) THEN
                  MXR = MD
                  IN2 = I - N
               END IF
            END IF
   40    CONTINUE
C
         IF ( ISC ) THEN
            SM = TWO*DWORK(IND-N)
            PR = MXC**2
         ELSE
            IF ( IN1.EQ.IN2 ) THEN
               MXR = ZERO
               SM  = DWORK(IN2)
               DWORK(IN2) = ZERO
C
               DO 50  I = IR, IR + N - 1
                  MD = ABS( DWORK( I ) )
                  IF ( MD.GT.MXR ) THEN
                     MXR = MD
                     IN1 = I
                  END IF
   50          CONTINUE
C
               DWORK(IN2) = SM
            END IF
            SM = DWORK(IN1) + DWORK(IN2)
            PR = DWORK(IN1) * DWORK(IN2)
         END IF
C
C        Compute a multiple of the first column of the real Wilkinson
C        double shift polynomial, having only three nonzero elements.
C
         P1 = P1 + ( ( E1 - SM )*E1 + PR )/E2
         P2 = P2 + E1 - SM
C
C        Compute the rotations to annihilate P2 and P3.
C
         CALL DLARTG( P2, P3, C2, S2, E1 )
         CALL DLARTG( P1, E1, C1, S1, E2 )
      END IF
C
      RETURN
C *** Last line of MB03AI ***
      END
