      DOUBLE PRECISION FUNCTION MA02JZ( LTRAN1, LTRAN2, N, Q1, LDQ1, Q2,
     $                                  LDQ2, RES, LDRES )
C
C     PURPOSE
C
C     To compute || Q^H Q - I ||_F for a complex matrix of the form
C
C                       [  op( Q1 )  op( Q2 ) ]
C                  Q =  [                     ],
C                       [ -op( Q2 )  op( Q1 ) ]
C
C     where Q1 and Q2 are N-by-N matrices. This residual can be used to
C     test wether Q is numerically a unitary symplectic matrix.
C
C     FUNCTION VALUE
C
C     MA02JZ  DOUBLE PRECISION
C             The computed residual.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     LTRAN1  LOGICAL
C             Specifies the form of op( Q1 ) as follows:
C             = .FALSE.:  op( Q1 ) = Q1;
C             = .TRUE. :  op( Q1 ) = Q1'.
C
C     LTRAN2  LOGICAL
C             Specifies the form of op( Q2 ) as follows:
C             = .FALSE.:  op( Q2 ) = Q2;
C             = .TRUE. :  op( Q2 ) = Q2'.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrices Q1 and Q2.  N >= 0.
C
C     Q1      (input) COMPLEX*16 array, dimension (LDQ1,N)
C             On entry, the leading N-by-N part of this array must
C             contain the matrix op( Q1 ).
C
C     LDQ1    INTEGER
C             The leading dimension of the array Q1.  LDQ1 >= MAX(1,N).
C
C     Q2      (input) COMPLEX*16 array, dimension (LDQ2,N)
C             On entry, the leading N-by-N part of this array must
C             contain the matrix op( Q2 ).
C
C     LDQ2    INTEGER
C             The leading dimension of the array Q2.  LDQ2 >= MAX(1,N).
C
C     Workspace
C
C     RES     DOUBLE PRECISION array, dimension (LDRES,N)
C
C     LDRES   INTEGER
C             The leading dimension of the array RES.
C             LDRES >= MAX(1,N).
C
C     METHOD
C
C     The routine computes the residual by simple elementary operations.
C
C     CONTRIBUTORS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     Sep. 2012. Based on SLICOT Library routine MA02JD.
C
C     REVISIONS
C
C     -
C
C     KEYWORDS
C
C     Elementary operations.
C
C     ******************************************************************
C
C     .. Parameters ..
      COMPLEX*16        ZERO, ONE
      PARAMETER        ( ZERO = (0.0D0,0.0D0), ONE = (1.0D0,0.0D0) )
      DOUBLE PRECISION  TWO
      PARAMETER         ( TWO = 2.0D0 )
C     .. Scalar Arguments ..
      LOGICAL           LTRAN1, LTRAN2
      INTEGER           LDQ1, LDQ2, LDRES, N
C     .. Array Arguments ..
      COMPLEX*16        Q1(LDQ1,*), Q2(LDQ2,*), RES(LDRES,*)
C     .. Local Scalars ..
      INTEGER           I
      DOUBLE PRECISION  TEMP
C     .. Local Arrays ..
      DOUBLE PRECISION  DUMMY(1)
C     .. External Functions ..
      DOUBLE PRECISION  DLAPY2, ZLANGE
      EXTERNAL          DLAPY2, ZLANGE
C     .. External Subroutines ..
      EXTERNAL          ZGEMM
C     .. Intrinsic Functions ..
      INTRINSIC         SQRT
C
C     .. Executable Statements ..
C
      IF ( LTRAN1 ) THEN
         CALL ZGEMM( 'No Transpose', 'Conj Transpose', N, N, N, ONE, Q1,
     $               LDQ1, Q1, LDQ1, ZERO, RES, LDRES )
      ELSE
         CALL ZGEMM( 'Conj Transpose', 'No Transpose', N, N, N, ONE, Q1,
     $               LDQ1, Q1, LDQ1, ZERO, RES, LDRES )
      END IF
      IF ( LTRAN2 ) THEN
         CALL ZGEMM( 'No Transpose', 'Conj Transpose', N, N, N, ONE, Q2,
     $               LDQ2, Q2, LDQ2, ONE, RES, LDRES )
      ELSE
         CALL ZGEMM( 'Conj Transpose', 'No Transpose', N, N, N, ONE, Q2,
     $               LDQ2, Q2, LDQ2, ONE, RES, LDRES )
      END IF
      DO 10 I = 1, N
         RES(I,I) = RES(I,I) - ONE
   10 CONTINUE
      TEMP = ZLANGE( 'Frobenius', N, N, RES, LDRES, DUMMY )
      IF ( LTRAN1 .AND. LTRAN2 ) THEN
         CALL ZGEMM( 'No Transpose', 'Conj Transpose', N, N, N, ONE, Q2,
     $               LDQ2, Q1, LDQ1, ZERO, RES, LDRES )
         CALL ZGEMM( 'No Transpose', 'Conj Transpose', N, N, N, ONE, Q1,
     $               LDQ1, Q2, LDQ2, -ONE, RES, LDRES )
      ELSE IF ( LTRAN1 ) THEN
         CALL ZGEMM( 'Conj Transpose', 'Conj Transpose', N, N, N, ONE,
     $               Q2, LDQ2, Q1, LDQ1, ZERO, RES, LDRES )
         CALL ZGEMM( 'No Transpose', 'No Transpose', N, N, N, ONE, Q1,
     $               LDQ1, Q2, LDQ2, -ONE, RES, LDRES )
      ELSE IF ( LTRAN2 ) THEN
         CALL ZGEMM( 'No Transpose', 'No Transpose', N, N, N, ONE, Q2,
     $               LDQ2, Q1, LDQ1, ZERO, RES, LDRES )
         CALL ZGEMM( 'Conj Transpose', 'Conj Transpose', N, N, N, ONE,
     $               Q1, LDQ1, Q2, LDQ2, -ONE, RES, LDRES )
      ELSE
         CALL ZGEMM( 'Conj Transpose', 'No Transpose', N, N, N, ONE, Q2,
     $               LDQ2, Q1, LDQ1, ZERO, RES, LDRES )
         CALL ZGEMM( 'Conj Transpose', 'No Transpose', N, N, N, ONE, Q1,
     $               LDQ1, Q2, LDQ2, -ONE, RES, LDRES )
      END IF
      TEMP = DLAPY2( TEMP, ZLANGE( 'Frobenius', N, N, RES, LDRES,
     $                             DUMMY ) )
      MA02JZ = SQRT( TWO )*TEMP
      RETURN
C *** Last line of MA02JZ ***
      END
