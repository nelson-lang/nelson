      DOUBLE PRECISION FUNCTION MA02ID( TYP, NORM, N, A, LDA, QG,
     $                                  LDQG, DWORK )
C
C     PURPOSE
C
C     To compute the value of the one norm, or the Frobenius norm, or
C     the infinity norm, or the element of largest absolute value
C     of a real skew-Hamiltonian matrix
C
C                   [  A   G  ]          T         T
C             X  =  [       T ],   G = -G,   Q = -Q,
C                   [  Q   A  ]
C
C     or of a real Hamiltonian matrix
C
C                   [  A   G  ]          T         T
C             X  =  [       T ],   G =  G,   Q =  Q,
C                   [  Q  -A  ]
C
C     where A, G and Q are real n-by-n matrices.
C
C     Note that for this kind of matrices the infinity norm is equal
C     to the one norm.
C
C     FUNCTION VALUE
C
C     MA02ID  DOUBLE PRECISION
C             The computed norm.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     TYP     CHARACTER*1
C             Specifies the type of the input matrix X:
C             = 'S':         X is skew-Hamiltonian;
C             = 'H':         X is Hamiltonian.
C
C     NORM    CHARACTER*1
C             Specifies the value to be returned in MA02ID:
C             = '1' or 'O':  one norm of X;
C             = 'F' or 'E':  Frobenius norm of X;
C             = 'I':         infinity norm of X;
C             = 'M':         max(abs(X(i,j)).
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrix A.  N >= 0.
C
C     A       (input) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the matrix A.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     QG      (input) DOUBLE PRECISION array, dimension (LDQG,N+1)
C             On entry, the leading N-by-N+1 part of this array must
C             contain in columns 1:N the lower triangular part of the
C             matrix Q and in columns 2:N+1 the upper triangular part
C             of the matrix G. If TYP = 'S', the parts containing the
C             diagonal and the first supdiagonal of this array are not
C             referenced.
C
C     LDQG    INTEGER
C             The leading dimension of the array QG.  LDQG >= MAX(1,N).
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             where LDWORK >= 2*N when NORM = '1', NORM = 'I' or
C             NORM = 'O'; otherwise, DWORK is not referenced.
C
C     CONTRIBUTORS
C
C     D. Kressner, Technical Univ. Berlin, Germany, and
C     P. Benner, Technical Univ. Chemnitz, Germany, December 2003.
C
C     REVISIONS
C
C     V. Sima, June 2008 (SLICOT version of the HAPACK routine DLANHA).
C     V. Sima, Jan. 2016 (removed O(N) tests in several loops).
C
C     KEYWORDS
C
C     Elementary matrix operations, Hamiltonian matrix, skew-Hamiltonian
C     matrix.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ONE, TWO, ZERO
      PARAMETER          ( ONE = 1.0D+0, TWO = 2.0D+0, ZERO = 0.0D+0 )
C     .. Scalar Arguments ..
      CHARACTER          NORM, TYP
      INTEGER            LDA, LDQG, N
C     .. Array Arguments ..
      DOUBLE PRECISION   A(LDA,*), DWORK(*), QG(LDQG,*)
C     .. Local Scalars ..
      LOGICAL            LSH
      INTEGER            I, J
      DOUBLE PRECISION   DSCL, DSUM, SCALE, SUM, TEMP, VALUE
C     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLANGE, DLAPY2
      EXTERNAL           DLANGE, DLAPY2, LSAME
C     .. External Subroutines ..
      EXTERNAL           DLASSQ
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SQRT
C
C     .. Executable Statements ..
C
      LSH = LSAME( TYP, 'S' )
C
      IF ( N.EQ.0 ) THEN
         VALUE = ZERO
C
      ELSE IF ( LSAME( NORM, 'M' ) .AND. LSH ) THEN
C
C        Find max(abs(A(i,j))).
C
         VALUE = DLANGE( 'MaxElement', N, N, A, LDA, DWORK )
         IF ( N.GT.1 ) THEN
            DO 30  J = 1, N+1
               DO 10  I = 1, J-2
                  VALUE = MAX( VALUE, ABS( QG(I,J) ) )
   10          CONTINUE
               DO 20  I = J+1, N
                  VALUE = MAX( VALUE, ABS( QG(I,J) ) )
   20          CONTINUE
   30       CONTINUE
         END IF
C
      ELSE IF ( LSAME( NORM, 'M' ) ) THEN
C
C        Find max( abs( A(i,j) ), abs( QG(i,j) ) ).
C
         VALUE = MAX( DLANGE( 'MaxElement', N, N, A, LDA, DWORK ),
     $                DLANGE( 'MaxElement', N, N+1, QG, LDQG,
     $                        DWORK ) )
C
      ELSE IF ( ( LSAME( NORM, 'O' ) .OR. ( NORM.EQ.'1' ) .OR.
     $            LSAME( NORM, 'I' ) ) .AND. LSH ) THEN
C
C        Find the column and row sums of A (in one pass).
C
         VALUE = ZERO
         DO 40 I = 1, N
            DWORK(I) = ZERO
   40    CONTINUE
C
         DO 60 J = 1, N
            SUM = ZERO
            DO 50 I = 1, N
               TEMP = ABS( A(I,J) )
               SUM  = SUM + TEMP
               DWORK(I) = DWORK(I) + TEMP
   50       CONTINUE
            DWORK(N+J) = SUM
   60    CONTINUE
C
C        Compute the maximal absolute column sum.
C
         DO 90 J = 1, N
            DO 70  I = 1, J-2
               TEMP = ABS( QG(I,J) )
               DWORK(I) = DWORK(I) + TEMP
               DWORK(J-1) = DWORK(J-1) + TEMP
   70       CONTINUE
            SUM = DWORK(N+J)
            DO 80  I = J+1, N
               TEMP = ABS( QG(I,J) )
               SUM  = SUM + TEMP
               DWORK(N+I) = DWORK(N+I) + TEMP
   80       CONTINUE
            VALUE = MAX( VALUE, SUM )
   90    CONTINUE
         DO 100  I = 1, N-1
            TEMP = ABS( QG(I,N+1) )
            DWORK(I) = DWORK(I) + TEMP
            DWORK(N) = DWORK(N) + TEMP
  100    CONTINUE
         DO 110 I = 1, N
            VALUE = MAX( VALUE, DWORK(I) )
  110    CONTINUE
C
      ELSE IF ( LSAME( NORM, 'O' ) .OR. ( NORM.EQ.'1' ) .OR.
     $          LSAME( NORM, 'I' ) ) THEN
C
C        Find the column and row sums of A (in one pass).
C
         VALUE = ZERO
         DO 120 I = 1, N
            DWORK(I) = ZERO
  120   CONTINUE
C
         DO 140 J = 1, N
            SUM = ZERO
            DO 130 I = 1, N
               TEMP = ABS( A(I,J) )
               SUM  = SUM + TEMP
               DWORK(I) = DWORK(I) + TEMP
  130       CONTINUE
            DWORK(N+J) = SUM
  140    CONTINUE
C
C        Compute the maximal absolute column sum.
C
         SUM = DWORK(N+1) + ABS( QG(1,1) )
         DO 150 I = 2, N
            TEMP = ABS( QG(I,1) )
            SUM  = SUM + TEMP
            DWORK(N+I) = DWORK(N+I) + TEMP
  150    CONTINUE
         VALUE = MAX( VALUE, SUM )
         DO 180 J = 2, N
            DO 160  I = 1, J-2
               TEMP = ABS( QG(I,J) )
               DWORK(I) = DWORK(I) + TEMP
               DWORK(J-1) = DWORK(J-1) + TEMP
  160       CONTINUE
            DWORK(J-1) = DWORK(J-1) + ABS( QG(J-1,J) )
            SUM = DWORK(N+J) + ABS( QG(J,J) )
            DO 170 I = J+1, N
               TEMP = ABS( QG(I,J) )
               SUM  = SUM + TEMP
               DWORK(N+I) = DWORK(N+I) + TEMP
  170       CONTINUE
            VALUE = MAX( VALUE, SUM )
  180    CONTINUE
         DO 190  I = 1, J-2
            TEMP = ABS( QG(I,N+1) )
            DWORK(I) = DWORK(I) + TEMP
            DWORK(N) = DWORK(N) + TEMP
  190    CONTINUE
         DWORK(N) = DWORK(N) + ABS( QG(N,N+1) )
         DO 200 I = 1, N
            VALUE = MAX( VALUE, DWORK(I) )
  200    CONTINUE
C
      ELSE IF ( ( LSAME( NORM, 'F' ) .OR.
     $            LSAME( NORM, 'E' ) ) .AND. LSH ) THEN
C
C        Find normF(A).
C
         SCALE = ZERO
         SUM = ONE
         DO 210 J = 1, N
            CALL DLASSQ( N, A(1,J), 1, SCALE, SUM )
  210    CONTINUE
C
C        Add normF(G) and normF(Q).
C
         IF ( N.GT.1 )
     $      CALL DLASSQ( N-1, QG(2,1), 1, SCALE, SUM )
         IF ( N.GT.2 )
     $      CALL DLASSQ( N-2, QG(3,2), 1, SCALE, SUM )
         DO 220 J = 3, N-1
            CALL DLASSQ( J-2, QG(1,J),   1, SCALE, SUM )
            CALL DLASSQ( N-J, QG(J+1,J), 1, SCALE, SUM )
  220    CONTINUE
         CALL DLASSQ( N-2, QG(1,N),   1, SCALE, SUM )
         CALL DLASSQ( N-1, QG(1,N+1), 1, SCALE, SUM )
         VALUE = SQRT( TWO )*SCALE*SQRT( SUM )
C
      ELSE IF ( LSAME( NORM, 'F' ) .OR. LSAME( NORM, 'E' ) ) THEN
C
         SCALE = ZERO
         SUM = ONE
         DO 230 J = 1, N
            CALL DLASSQ( N, A(1,J), 1, SCALE, SUM )
  230    CONTINUE
C
         DSCL = ZERO
         DSUM = ONE
         CALL DLASSQ( 1, QG(1,1), 1, DSCL, DSUM )
         IF ( N.GT.1 )
     $      CALL DLASSQ( N-1, QG(2,1), 1, SCALE, SUM )
         DO 240 J = 2, N
            CALL DLASSQ( J-2, QG(1,J),   1, SCALE, SUM )
            CALL DLASSQ( 2,   QG(J-1,J), 1, DSCL, DSUM )
            CALL DLASSQ( N-J, QG(J+1,J), 1, SCALE, SUM )
  240    CONTINUE
         CALL DLASSQ( N-1, QG(1,N+1), 1, SCALE, SUM )
         CALL DLASSQ( 1,   QG(N,N+1), 1, DSCL, DSUM )
         VALUE = DLAPY2( SQRT( TWO )*SCALE*SQRT( SUM ),
     $                   DSCL*SQRT( DSUM ) )
      END IF
C
      MA02ID = VALUE
      RETURN
C *** Last line of MA02ID ***
      END
