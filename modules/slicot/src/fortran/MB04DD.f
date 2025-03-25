      SUBROUTINE MB04DD( JOB, N, A, LDA, QG, LDQG, ILO, SCALE, INFO )
C
C     PURPOSE
C
C     To balance a real Hamiltonian matrix,
C
C                   [  A   G  ]
C              H =  [       T ] ,
C                   [  Q  -A  ]
C
C     where A is an N-by-N matrix and G, Q are N-by-N symmetric
C     matrices. This involves, first, permuting H by a symplectic
C     similarity transformation to isolate eigenvalues in the first
C     1:ILO-1 elements on the diagonal of A; and second, applying a
C     diagonal similarity transformation to rows and columns
C     ILO:N, N+ILO:2*N to make the rows and columns as close in 1-norm
C     as possible. Both steps are optional.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOB     CHARACTER*1
C             Specifies the operations to be performed on H:
C             = 'N':  none, set ILO = 1, SCALE(I) = 1.0, I = 1 .. N;
C             = 'P':  permute only;
C             = 'S':  scale only;
C             = 'B':  both permute and scale.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrix A. N >= 0.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the matrix A.
C             On exit, the leading N-by-N part of this array contains
C             the matrix A of the balanced Hamiltonian. In particular,
C             the strictly lower triangular part of the first ILO-1
C             columns of A is zero.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     QG      (input/output) DOUBLE PRECISION array, dimension
C                            (LDQG,N+1)
C             On entry, the leading N-by-N+1 part of this array must
C             contain the lower triangular part of the matrix Q and
C             the upper triangular part of the matrix G.
C             On exit, the leading N-by-N+1 part of this array contains
C             the lower and upper triangular parts of the matrices Q and
C             G, respectively, of the balanced Hamiltonian. In
C             particular, the lower triangular part of the first ILO-1
C             columns of QG is zero.
C
C     LDQG    INTEGER
C             The leading dimension of the array QG.  LDQG >= MAX(1,N).
C
C     ILO     (output) INTEGER
C             ILO-1 is the number of deflated eigenvalues in the
C             balanced Hamiltonian matrix.
C
C     SCALE   (output) DOUBLE PRECISION array of dimension (N)
C             Details of the permutations and scaling factors applied to
C             H.  For j = 1,...,ILO-1 let P(j) = SCALE(j). If P(j) <= N,
C             then rows and columns P(j) and P(j)+N are interchanged
C             with rows and columns j and j+N, respectively. If
C             P(j) > N, then row and column P(j)-N are interchanged with
C             row and column j+N by a generalized symplectic
C             permutation. For j = ILO,...,N the j-th element of SCALE
C             contains the factor of the scaling applied to row and
C             column j.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value.
C
C     REFERENCES
C
C     [1] Benner, P.
C         Symplectic balancing of Hamiltonian matrices.
C         SIAM J. Sci. Comput., 22 (5), pp. 1885-1904, 2001.
C
C     CONTRIBUTORS
C
C     D. Kressner, Technical Univ. Berlin, Germany, and
C     P. Benner, Technical Univ. Chemnitz, Germany, December 2003.
C
C     REVISIONS
C
C     V. Sima, June 2008 (SLICOT version of the HAPACK routine DHABAL).
C     V. Sima, Mar. 2016.
C
C     KEYWORDS
C
C     Balancing, Hamiltonian matrix.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0 )
C     .. Scalar Arguments ..
      CHARACTER         JOB
      INTEGER           ILO, INFO, LDA, LDQG, N
C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), QG(LDQG,*), SCALE(*)
C     .. Local Scalars ..
      LOGICAL           CONV, LPERM, LSCAL
      INTEGER           I, IC, ILOOLD, J
      DOUBLE PRECISION  C, F, GII, MAXC, MAXR, QII, R, SCLFAC,
     $                  SFMAX1, SFMAX2, SFMIN1, SFMIN2, TEMP
C     .. External Functions ..
      LOGICAL           LSAME
      INTEGER           IDAMAX
      DOUBLE PRECISION  DASUM, DLAMCH
      EXTERNAL          DASUM, DLAMCH, IDAMAX, LSAME
C     .. External Subroutines ..
      EXTERNAL          DRSCL, DSCAL, DSWAP, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, DBLE, MAX, MIN
C
C     .. Executable Statements ..
C
C     Check the scalar input parameters.
C
      INFO = 0
      LPERM = LSAME( JOB, 'P' ) .OR. LSAME( JOB, 'B' )
      LSCAL = LSAME( JOB, 'S' ) .OR. LSAME( JOB, 'B' )
C
      IF ( .NOT.LPERM .AND. .NOT.LSCAL
     $     .AND. .NOT.LSAME( JOB, 'N' ) ) THEN
         INFO = -1
      ELSE IF ( N.LT.0 ) THEN
         INFO = -2
      ELSE IF ( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -4
      ELSE IF ( LDQG.LT.MAX( 1, N ) ) THEN
         INFO = -6
      END IF
C
C     Return if there were illegal values.
C
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'MB04DD', -INFO )
         RETURN
      END IF
C
      ILO = 1
C
C     Quick return if possible.
C
      IF ( N.EQ.0 )
     $   RETURN
      IF ( .NOT.LPERM .AND. .NOT.LSCAL ) THEN
         DO 10  I = 1, N
            SCALE(I) = ONE
   10    CONTINUE
         RETURN
      END IF
C
C     Permutations to isolate eigenvalues if possible.
C
      IF ( LPERM ) THEN
         ILOOLD = 0
C        WHILE ( ILO.NE.ILOOLD )
   20    IF ( ILO.NE.ILOOLD ) THEN
            ILOOLD = ILO
C
C           Scan columns ILO .. N.
C
            I = ILO
C           WHILE ( I.LE.N .AND. ILO.EQ.ILOOLD )
   30       IF ( I.LE.N .AND. ILO.EQ.ILOOLD ) THEN
               DO 40  J = ILO, I-1
                  IF ( A(J,I).NE.ZERO ) THEN
                     I = I + 1
                     GOTO 30
                  END IF
   40          CONTINUE
               DO 50  J = I+1, N
                  IF ( A(J,I).NE.ZERO ) THEN
                     I = I + 1
                     GOTO 30
                  END IF
   50          CONTINUE
               DO 60  J = ILO, I
                  IF ( QG(I,J).NE.ZERO ) THEN
                     I = I + 1
                     GOTO 30
                  END IF
   60          CONTINUE
               DO 70  J = I+1, N
                  IF ( QG(J,I).NE.ZERO ) THEN
                     I = I + 1
                     GOTO 30
                  END IF
   70          CONTINUE
C
C              Exchange columns/rows ILO <-> I.
C
               SCALE( ILO ) = DBLE( I )
               IF ( ILO.NE.I ) THEN
C
                  CALL DSWAP( N, A(1,ILO), 1, A(1,I), 1 )
                  CALL DSWAP( N-ILO+1, A(ILO,ILO), LDA, A(I,ILO), LDA )
C
                  CALL DSWAP( 1, QG(I,ILO), LDQG, QG(ILO,ILO), LDQG )
                  CALL DSWAP( N-I+1, QG(I,I), 1, QG(I,ILO), 1 )
                  CALL DSWAP( I-ILO, QG(ILO,ILO), 1, QG(I,ILO), LDQG )
C
                  CALL DSWAP( ILO, QG(1,I+1), 1, QG(1,ILO+1), 1 )
                  CALL DSWAP( N-I+1, QG(I,I+1), LDQG, QG(ILO,I+1),
     $                        LDQG )
                  CALL DSWAP( I-ILO, QG(ILO,ILO+1), LDQG, QG(ILO,I+1),
     $                        1 )
               END IF
               ILO = ILO + 1
            END IF
C           END WHILE 30
C
C           Scan columns N+ILO .. 2*N.
C
            I = ILO
C           WHILE ( I.LE.N .AND. ILO.EQ.ILOOLD )
   80       IF ( I.LE.N .AND. ILO.EQ.ILOOLD ) THEN
               DO 90  J = ILO, I-1
                  IF ( A(I,J).NE.ZERO ) THEN
                     I = I + 1
                     GOTO 80
                  END IF
   90          CONTINUE
               DO 100  J = I+1, N
                  IF ( A(I,J).NE.ZERO ) THEN
                     I = I + 1
                     GOTO 80
                  END IF
  100          CONTINUE
               DO 110  J = ILO, I
                  IF ( QG(J,I+1).NE.ZERO ) THEN
                     I = I + 1
                     GOTO 80
                  END IF
  110          CONTINUE
               DO 120  J = I+1, N
                  IF ( QG(I,J+1).NE.ZERO ) THEN
                     I = I + 1
                     GOTO 80
                  END IF
  120          CONTINUE
               SCALE( ILO ) = DBLE( N+I )
C
C              Exchange columns/rows I <-> I+N with a symplectic
C              generalized permutation.
C
               CALL DSWAP( I-ILO, A(I,ILO), LDA, QG(I,ILO), LDQG )
               CALL DSCAL( I-ILO, -ONE, A(I,ILO), LDA )
               CALL DSWAP( N-I, A(I,I+1), LDA, QG(I+1,I), 1 )
               CALL DSCAL( N-I, -ONE, A(I,I+1), LDA )
               CALL DSWAP( I-1, A(1,I), 1, QG(1,I+1), 1 )
               CALL DSCAL( I-1, -ONE, A(1,I), 1 )
               CALL DSWAP( N-I, A(I+1,I), 1, QG(I,I+2), LDQG )
               CALL DSCAL( N-I, -ONE, A(I+1,I), 1 )
               A(I,I) = -A(I,I)
               TEMP   = QG(I,I)
               QG(I,I) = -QG(I,I+1)
               QG(I,I+1) = -TEMP
C
C              Exchange columns/rows ILO <-> I.
C
               IF ( ILO.NE.I ) THEN
C
                  CALL DSWAP( N, A(1,ILO), 1, A(1,I), 1 )
                  CALL DSWAP( N-ILO+1, A(ILO,ILO), LDA, A(I,ILO), LDA )
C
                  CALL DSWAP( 1, QG(I,ILO), LDQG, QG(ILO,ILO), LDQG )
                  CALL DSWAP( N-I+1, QG(I,I), 1, QG(I,ILO), 1 )
                  CALL DSWAP( I-ILO, QG(ILO,ILO), 1, QG(I,ILO), LDQG )
C
                  CALL DSWAP( ILO, QG(1,I+1), 1, QG(1,ILO+1), 1 )
                  CALL DSWAP( N-I+1, QG(I,I+1), LDQG, QG(ILO,I+1),
     $                        LDQG )
                  CALL DSWAP( I-ILO, QG(ILO,ILO+1), LDQG, QG(ILO,I+1),
     $                        1 )
               END IF
               ILO = ILO + 1
            END IF
C           END WHILE 80
            GOTO 20
         END IF
C        END WHILE 20
      END IF
C
      DO 130  I = ILO, N
         SCALE(I) = ONE
  130 CONTINUE
C
C     Scale to reduce the 1-norm of the remaining blocks.
C
      IF ( LSCAL ) THEN
         SCLFAC = DLAMCH( 'B' )
         SFMIN1 = DLAMCH( 'S' ) / DLAMCH( 'P' )
         SFMAX1 = ONE / SFMIN1
         SFMIN2 = SFMIN1*SCLFAC
         SFMAX2 = ONE / SFMIN2
C
C        Scale the rows and columns one at a time to minimize the
C        1-norm of the remaining Hamiltonian submatrix.
C        Stop when the 1-norm is very roughly minimal.
C
  140    CONTINUE
            CONV = .TRUE.
            DO 170 I = ILO, N
C
C              Compute 1-norm of row and column I without diagonal
C              elements.
C
               R = DASUM( I-ILO, A(I,ILO), LDA ) +
     $             DASUM( N-I,   A(I,I+1), LDA ) +
     $             DASUM( I-ILO, QG(ILO,I+1), 1 ) +
     $             DASUM( N-I,   QG(I,I+2), LDQG )
               C = DASUM( I-ILO, A(ILO,I), 1 ) +
     $             DASUM( N-I,   A(I+1,I), 1 ) +
     $             DASUM( I-ILO, QG(I,ILO), LDQG ) +
     $             DASUM( N-I,   QG(I+1,I), 1 )
               QII = ABS( QG(I,I) )
               GII = ABS( QG(I,I+1) )
C
C              Compute inf-norms of row and column I.
C
               IC = IDAMAX( N-ILO+1, A(I,ILO), LDA )
               MAXR = ABS( A(I,IC+ILO-1) )
               IF ( I.GT.1 ) THEN
                  IC = IDAMAX( I-1, QG(1,I+1), 1 )
                  MAXR = MAX( MAXR, ABS( QG(IC,I+1) ) )
               END IF
               IF ( N.GT.I ) THEN
                  IC = IDAMAX( N-I, QG(I,I+2), LDQG )
                  MAXR = MAX( MAXR, ABS( QG(I,IC+I+1) ) )
               END IF
               IC = IDAMAX( N, A(1,I), 1 )
               MAXC = ABS( A(IC,I) )
               IF ( I.GT.ILO ) THEN
                  IC = IDAMAX( I-ILO, QG(I,ILO), LDQG )
                  MAXC = MAX( MAXC, ABS( QG(I,IC+ILO-1) ) )
               END IF
               IF ( N.GT.I ) THEN
                  IC = IDAMAX( N-I, QG(I+1,I), 1 )
                  MAXC = MAX( MAXC, ABS( QG(IC+I,I) ) )
               END IF
               IF ( ( C + QII ).EQ.ZERO .OR. ( R + GII ).EQ.ZERO )
     $            GO TO 170
C
               F = ONE
  150          CONTINUE
               IF ( ( ( R + GII/SCLFAC )/SCLFAC ).GE.
     $              ( ( C + QII*SCLFAC )*SCLFAC ) .AND.
     $              MAX( F*SCLFAC, C*SCLFAC, MAXC*SCLFAC,
     $                   QII*SCLFAC*SCLFAC ).LT.SFMAX2 .AND.
     $              MIN( ( R + GII/SCLFAC )/SCLFAC, MAX( MAXR/SCLFAC,
     $                   GII/SCLFAC/SCLFAC ) ).GT.SFMIN2 ) THEN
                  F = F*SCLFAC
                  C = C*SCLFAC
                  QII = QII*SCLFAC*SCLFAC
                  R = R / SCLFAC
                  GII = GII/SCLFAC/SCLFAC
                  MAXC = MAXC*SCLFAC
                  MAXR = MAXR / SCLFAC
                  GO TO 150
               END IF
C
  160          CONTINUE
               IF ( ( ( R + GII*SCLFAC )*SCLFAC ).LE.
     $              ( ( C + QII/SCLFAC )/SCLFAC ) .AND.
     $              MAX( R*SCLFAC, MAXR*SCLFAC,
     $                   GII*SCLFAC*SCLFAC ).LT.SFMAX2 .AND.
     $              MIN( F/SCLFAC, ( C + QII/SCLFAC )/SCLFAC,
     $                   MAX( MAXC/SCLFAC, QII/SCLFAC/SCLFAC ) )
     $                   .GT.SFMIN2 ) THEN
                  F = F / SCLFAC
                  C = C / SCLFAC
                  QII = QII/SCLFAC/SCLFAC
                  R = R*SCLFAC
                  GII = GII*SCLFAC*SCLFAC
                  MAXC = MAXC/SCLFAC
                  MAXR = MAXR*SCLFAC
                  GO TO 160
               END IF
C
C              Now balance if necessary.
C
               IF ( F.NE.ONE ) THEN
                  IF ( F.LT.ONE .AND. SCALE(I).LT.ONE ) THEN
                     IF ( F*SCALE(I).LE.SFMIN1 )
     $                  GO TO 170
                  END IF
                  IF ( F.GT.ONE .AND. SCALE(I).GT.ONE ) THEN
                     IF ( SCALE(I).GE.SFMAX1 / F )
     $                  GO TO 170
                  END IF
                  CONV = .FALSE.
                  SCALE(I) = SCALE(I)*F
                  CALL DRSCL( I-ILO, F, A(I,ILO), LDA )
                  CALL DRSCL( N-I, F, A(I,I+1), LDA )
                  CALL DSCAL( I-1, F, A(1,I), 1 )
                  CALL DSCAL( N-I, F, A(I+1,I), 1 )
                  CALL DRSCL( I-1, F, QG(1,I+1), 1 )
                  QG(I,I+1) = QG(I,I+1) / F / F
                  CALL DRSCL( N-I, F, QG(I,I+2), LDQG )
                  CALL DSCAL( I-ILO, F, QG(I,ILO), LDQG )
                  QG(I,I)   = QG(I,I) * F * F
                  CALL DSCAL( N-I, F, QG(I+1,I), 1 )
               END IF
  170       CONTINUE
         IF ( .NOT.CONV ) GO TO 140
      END IF
      RETURN
C *** Last line of MB04DD ***
      END
