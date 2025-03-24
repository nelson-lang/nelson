      SUBROUTINE MB03AG( SHFT, K, N, AMAP, S, SINV, A, LDA1, LDA2, C1,
     $                   S1, C2, S2, IWORK, DWORK )
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
C             On exit, if SHFT = 'D', C2 and S2 contain the parameters
C             for the second Givens rotation. Otherwise, C2 = 1, S2 = 0.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (2*N)
C
C     DWORK   DOUBLE PRECISION array, dimension (2*N*N)
C             On exit, DWORK(N*N+1:N*N+N) and DWORK(N*N+N+1:N*N+2*N)
C             contain the real and imaginary parts, respectively, of the
C             eigenvalues of the matrix product.
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
C     V. Sima, Sep. 2019, Dec. 2019.
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
      INTEGER           AMAP(*), IWORK(*), S(*)
      DOUBLE PRECISION  A(LDA1,LDA2,*), DWORK(*)
C     .. Local Scalars ..
      LOGICAL           FC, SGLE
      INTEGER           I, IC, II, IM, IR, IS, J, L, NN
      DOUBLE PRECISION  E1, E2, MC, MN, MX, P1, P2, P3, PR, SM
C     .. Local Arrays ..
      DOUBLE PRECISION  Z(1)
C     .. External Functions ..
      LOGICAL           LSAME
      INTEGER           IDAMAX
      DOUBLE PRECISION  DLAPY2
      EXTERNAL          DLAPY2, IDAMAX, LSAME
C     .. External Subroutines ..
      EXTERNAL          DCOPY, DGESC2, DGETC2, DLAHQR, DLARTG, DLASET,
     $                  DTRMV
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
      I  = AMAP(K)
      IC = 1
C
      Z(1) = ZERO
C
      IF ( K.GT.1 ) THEN
C
         IF ( S(I).EQ.SINV ) THEN
            IS = 1
C
            DO 10  L = 1, N
               CALL DCOPY( L, A(1,L,I), 1, DWORK(IC), 1 )
               CALL DCOPY( N-L, Z, 0, DWORK(IC+L), 1 )
               IC = IC + N
   10       CONTINUE
C
         ELSE
            IS = 0
            CALL DLASET( 'Full', N, N, ZERO, ONE, DWORK, N )
         END IF
C
         DO 50  J = K - IS, 2, -1
            I = AMAP(J)
            IF ( S(I).EQ.SINV ) THEN
               IC = 1
C
               DO 20  L = 1, N
                  CALL DTRMV( 'Upper', 'NoTran', 'NonUnit', L, A(1,1,I),
     $                        LDA1, DWORK(IC), 1 )
                  IC = IC + N
   20          CONTINUE
C
            ELSE
               IC = IR
C
               DO 30  L = 1, N
                  CALL DCOPY( L, A(1,L,I), 1, DWORK(IC), 1 )
                  CALL DCOPY( N-L, Z, 0, DWORK(IC+L), 1 )
                  IC = IC + N
   30          CONTINUE
C
C              Complete pivoting is used for triangular factors whose
C              exponents differ from SINV. It is assumed that SM = 1,
C              i.e., no overflow could appear when calling DGESC2.
C
               CALL DGETC2( N, DWORK(IR), N, IWORK, IWORK(N+1), IM )
C
               DO 40  IC = 1, NN, N
                  CALL DGESC2( N, DWORK(IR), N, DWORK(IC), IWORK,
     $                         IWORK(N+1), SM )
   40          CONTINUE
C
            END IF
   50    CONTINUE
C
         I  = AMAP(1)
         IC = 1
C
         DO 70 J = 1, N - 1
            CALL DCOPY( J, DWORK(IC), 1, DWORK(IR), 1 )
            CALL DTRMV( 'Upper', 'NoTran', 'NoDiag', J, A(1,1,I), LDA1,
     $                  DWORK(IC), 1 )
C
            DO 60 L = 1, J
               DWORK(IC+L) = DWORK(IC+L) + A(L+1,L,I)*DWORK(IR+L-1)
   60       CONTINUE
C
            IC = IC + N
   70    CONTINUE
C
         CALL DCOPY( N, DWORK(IC), 1, DWORK(IR), 1 )
         CALL DTRMV( 'Upper', 'NoTran', 'NoDiag', N, A(1,1,I), LDA1,
     $               DWORK(IC), 1 )
C
         DO 80 L = 1, N
            DWORK(IC+L) = DWORK(IC+L) + A(L+1,L,I)*DWORK(IR+L-1)
   80    CONTINUE
C
      ELSE
         I = AMAP(1)
C
         DO 90  L = 1, N - 1
            CALL DCOPY( L+1, A(1,L,I), 1, DWORK(IC), 1 )
            CALL DCOPY( N-L-1, Z, 0, DWORK(IC+L+1), 1 )
            IC = IC + N
   90    CONTINUE
C
         CALL DCOPY( N, A(1,N,I), 1, DWORK(IC), 1 )
      END IF
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
     $                DWORK(II), 1, 1, Z, 1, IM )
C
C        Find two eigenvalues with the smallest moduli.
C        If there are complex eigenvalues, selection is based on them.
C
         I = IDAMAX( N, DWORK(II), 1 )
         IF ( DWORK(II+I-1).EQ.ZERO ) THEN
            IM = IR + IDAMAX( N, DWORK(IR), 1 ) - 1
            MX = ABS( DWORK(IM) )
            MN = MX
C
            DO 100  I = IR, IR + N - 1
               MC = ABS( DWORK(I) )
               IF ( MC.LT.MN ) THEN
                  MN = MC
                  IM = I
               END IF
  100       CONTINUE
C
            PR = DWORK(IM)
            MN = MX
            DWORK(IM) = MX
            IS = IM
            MX = PR
C
            DO 110  I = IR, IR + N - 1
               MC = ABS( DWORK(I) )
               IF ( MC.LT.MN ) THEN
                  MN = MC
                  IM = I
               END IF
  110       CONTINUE
C
            SM = PR + DWORK(IM)
            PR = PR * DWORK(IM)
            DWORK(IS) = MX
C
         ELSE
C
            I  = II
            FC = .FALSE.
C
C           WHILE ( I <= II+N-1 ) DO
C
  120       CONTINUE
            IF ( I.LE.II + N - 1 ) THEN
               IF ( DWORK(I).NE.ZERO ) THEN
                  MC = DLAPY2( DWORK(I-N), DWORK(I) )
                  IF ( .NOT.FC ) THEN
                     FC = .TRUE.
                     IM = I
                     MN = MC
                  ELSE IF ( MC.LT.MN ) THEN
                     MN = MC
                     IM = I
                  END IF
                  I = I + 2
               ELSE
                  I = I + 1
               END IF
               GO TO 120
            END IF
C
C           END WHILE 120
C
            SM = TWO*DWORK(IM-N)
            PR = MN**2
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
C *** Last line of MB03AG ***
      END
