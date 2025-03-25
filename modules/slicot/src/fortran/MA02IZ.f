      DOUBLE PRECISION FUNCTION MA02IZ( TYP, NORM, N, A, LDA, QG,
     $                                  LDQG, DWORK )
C
C     PURPOSE
C
C     To compute the value of the one norm, or the Frobenius norm, or
C     the infinity norm, or the element of largest absolute value
C     of a complex skew-Hamiltonian matrix
C
C                   [  A   G  ]          H         H
C             X  =  [       H ],   G = -G,   Q = -Q,
C                   [  Q   A  ]
C
C     or of a complex Hamiltonian matrix
C
C                   [  A   G  ]          H         H
C             X  =  [       H ],   G =  G,   Q =  Q,
C                   [  Q  -A  ]
C
C     where A, G and Q are complex n-by-n matrices.
C
C     Note that for this kind of matrices the infinity norm is equal
C     to the one norm.
C
C     FUNCTION VALUE
C
C     MA02IZ  DOUBLE PRECISION
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
C             Specifies the value to be returned in MA02IZ:
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
C     A       (input) COMPLEX*16 array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the matrix A.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     QG      (input) COMPLEX*16 array, dimension (LDQG,N+1)
C             On entry, the leading N-by-N+1 part of this array must
C             contain in columns 1:N the lower triangular part of the
C             matrix Q and in columns 2:N+1 the upper triangular part
C             of the matrix G. If TYP = 'S', the real parts of the
C             entries on the diagonal and the first superdiagonal of
C             this array, which should be zero, need not be set, since
C             they are not used. Similarly, if TYP = 'H', the imaginary
C             parts of the entries on the diagonal and the first
C             superdiagonal of this array, which should be zero, need
C             not be set.
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
C     V. Sima, Research Institute for Informatics, Bucharest, Dec. 2011.
C              Based on the SLICOT Library routine MA02ID.
C
C     REVISIONS
C
C     V. Sima, Oct. 2012, Dec. 2015, Jan. 2016.
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
      COMPLEX*16         A(LDA,*), QG(LDQG,*)
      DOUBLE PRECISION   DWORK(*)
C     .. Local Scalars ..
      LOGICAL            LSH
      INTEGER            I, J
      DOUBLE PRECISION   DSCL, DSUM, SCALE, SUM, TEMP, VALUE
C     .. Local Arrays ..
      DOUBLE PRECISION   DUM(2)
C     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAPY2, ZLANGE
      EXTERNAL           DLAPY2, LSAME, ZLANGE
C     .. External Subroutines ..
      EXTERNAL           DLASSQ, ZLASSQ
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DIMAG, MAX, SQRT
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
C        Find max( abs( A(i,j) ), abs( QG(i,j) ) ).
C
         VALUE = ZLANGE( 'MaxElement', N, N, A, LDA, DUM )
         DO 30  J = 1, N
            DO 10  I = 1, J-2
               VALUE = MAX( VALUE, ABS( QG(I,J) ) )
   10       CONTINUE
            VALUE = MAX( VALUE, ABS( DIMAG( QG(J,J) ) ) )
            DO 20  I = J+1, N
               VALUE = MAX( VALUE, ABS( QG(I,J) ) )
   20       CONTINUE
            VALUE = MAX( VALUE, ABS( DIMAG( QG(J,J+1) ) ) )
   30    CONTINUE
         DO 40  I = 1, N-1
            VALUE = MAX( VALUE, ABS( QG(I,N+1) ) )
   40    CONTINUE
C
      ELSE IF ( LSAME( NORM, 'M' ) ) THEN
C
C        Find max( abs( A(i,j) ), abs( QG(i,j) ) ).
C
         VALUE = ZLANGE( 'MaxElement', N, N, A, LDA, DUM )
         DO 70  J = 1, N
            DO 50  I = 1, J-2
               VALUE = MAX( VALUE, ABS( QG(I,J) ) )
   50       CONTINUE
            VALUE = MAX( VALUE, ABS( DBLE( QG(J,J) ) ) )
            DO 60  I = J+1, N
               VALUE = MAX( VALUE, ABS( QG(I,J) ) )
   60       CONTINUE
            VALUE = MAX( VALUE, ABS( DBLE( QG(J,J+1) ) ) )
   70    CONTINUE
         DO 80  I = 1, N-1
            VALUE = MAX( VALUE, ABS( QG(I,N+1) ) )
   80    CONTINUE
C
      ELSE IF ( ( LSAME( NORM, 'O' ) .OR. ( NORM.EQ.'1' ) .OR.
     $            LSAME( NORM, 'I' ) ) .AND. LSH ) THEN
C
C        Find the column and row sums of A (in one pass).
C
         VALUE = ZERO
         DO 90 I = 1, N
            DWORK(I) = ZERO
   90    CONTINUE
C
         DO 110 J = 1, N
            SUM = ZERO
            DO 100 I = 1, N
               TEMP = ABS( A(I,J) )
               SUM  = SUM + TEMP
               DWORK(I) = DWORK(I) + TEMP
  100       CONTINUE
            DWORK(N+J) = SUM
  110    CONTINUE
C
C        Compute the maximal absolute column sum.
C
         SUM = DWORK(N+1) + ABS( DIMAG( QG(1,1) ) )
         DO 120 I = 2, N
            TEMP = ABS( QG(I,1) )
            SUM  = SUM + TEMP
            DWORK(N+I) = DWORK(N+I) + TEMP
  120    CONTINUE
         VALUE = MAX( VALUE, SUM )
         DO 150 J = 2, N
            DO 130  I = 1, J-2
               TEMP = ABS( QG(I,J) )
               DWORK(I) = DWORK(I) + TEMP
               DWORK(J-1) = DWORK(J-1) + TEMP
  130       CONTINUE
            DWORK(J-1) = DWORK(J-1) + ABS( DIMAG( QG(J-1,J) ) )
            SUM = DWORK(N+J) + ABS( DIMAG( QG(J,J) ) )
            DO 140 I = J+1, N
               TEMP = ABS( QG(I,J) )
               SUM  = SUM + TEMP
               DWORK(N+I) = DWORK(N+I) + TEMP
  140       CONTINUE
            VALUE = MAX( VALUE, SUM )
  150    CONTINUE
         DO 160  I = 1, N-1
            TEMP = ABS( QG(I,N+1) )
            DWORK(I) = DWORK(I) + TEMP
            DWORK(N) = DWORK(N) + TEMP
  160    CONTINUE
         DWORK(N) = DWORK(N) + ABS( DIMAG( QG(N,N+1) ) )
         DO 170 I = 1, N
            VALUE = MAX( VALUE, DWORK(I) )
  170    CONTINUE
C
      ELSE IF ( LSAME( NORM, 'O' ) .OR. ( NORM.EQ.'1' ) .OR.
     $          LSAME( NORM, 'I' ) ) THEN
C
C        Find the column and row sums of A (in one pass).
C
         VALUE = ZERO
         DO 180 I = 1, N
            DWORK(I) = ZERO
  180    CONTINUE
C
         DO 200 J = 1, N
            SUM = ZERO
            DO 190 I = 1, N
               TEMP = ABS( A(I,J) )
               SUM  = SUM + TEMP
               DWORK(I) = DWORK(I) + TEMP
  190       CONTINUE
            DWORK(N+J) = SUM
  200    CONTINUE
C
C        Compute the maximal absolute column sum.
C
         SUM = DWORK(N+1) + ABS( DBLE( QG(1,1) ) )
         DO 210 I = 2, N
            TEMP = ABS( QG(I,1) )
            SUM  = SUM + TEMP
            DWORK(N+I) = DWORK(N+I) + TEMP
  210    CONTINUE
         VALUE = MAX( VALUE, SUM )
         DO 240 J = 2, N
            DO 220  I = 1, J-2
               TEMP = ABS( QG(I,J) )
               DWORK(I) = DWORK(I) + TEMP
               DWORK(J-1) = DWORK(J-1) + TEMP
  220       CONTINUE
            DWORK(J-1) = DWORK(J-1) + ABS( DBLE( QG(J-1,J) ) )
            SUM = DWORK(N+J) + ABS( DBLE( QG(J,J) ) )
            DO 230 I = J+1, N
               TEMP = ABS( QG(I,J) )
               SUM  = SUM + TEMP
               DWORK(N+I) = DWORK(N+I) + TEMP
  230       CONTINUE
            VALUE = MAX( VALUE, SUM )
  240    CONTINUE
         DO 250  I = 1, N-1
            TEMP = ABS( QG(I,N+1) )
            DWORK(I) = DWORK(I) + TEMP
            DWORK(N) = DWORK(N) + TEMP
  250    CONTINUE
         DWORK(N) = DWORK(N) + ABS( DBLE( QG(N,N+1) ) )
         DO 260 I = 1, N
            VALUE = MAX( VALUE, DWORK(I) )
  260    CONTINUE
C
      ELSE IF ( ( LSAME( NORM, 'F' ) .OR.
     $            LSAME( NORM, 'E' ) ) .AND. LSH ) THEN
C
C        Find normF(A).
C
         SCALE = ZERO
         SUM   = ONE
         DO 270 J = 1, N
            CALL ZLASSQ( N, A(1,J), 1, SCALE, SUM )
  270    CONTINUE
C
C        Add normF(G) and normF(Q).
C
         DSCL = ABS( DIMAG( QG(1,1) ) )
         DSUM = ONE
         IF ( N.GT.1 ) THEN
            CALL ZLASSQ( N-1, QG(2,1), 1, SCALE, SUM )
            DUM(1) = DIMAG( QG(1,2) )
            DUM(2) = DIMAG( QG(2,2) )
            CALL DLASSQ( 2, DUM, 1, DSCL, DSUM )
         END IF
         IF ( N.GT.2 )
     $      CALL ZLASSQ( N-2, QG(3,2), 1, SCALE, SUM )
         DO 280 J = 3, N
            CALL ZLASSQ( J-2, QG(1,J), 1, SCALE, SUM )
            DUM(1) = DIMAG( QG(J-1,J) )
            DUM(2) = DIMAG( QG(J,  J) )
            CALL DLASSQ(   2, DUM,       1, DSCL, DSUM )
            CALL ZLASSQ( N-J, QG(J+1,J), 1, SCALE, SUM )
  280    CONTINUE
         IF ( N.GT.1 )
     $      CALL ZLASSQ( N-1, QG(1,N+1), 1, SCALE, SUM )
         DUM(1) = DIMAG( QG(N,N+1) )
         CALL DLASSQ( 1, DUM, 1, DSCL, DSUM )
         VALUE = DLAPY2( SQRT( TWO )*SCALE*SQRT( SUM ),
     $                   DSCL*SQRT( DSUM ) )
C
      ELSE IF ( LSAME( NORM, 'F' ) .OR. LSAME( NORM, 'E' ) ) THEN
C
C        Find normF(A).
C
         SCALE = ZERO
         SUM   = ONE
         DO 290 J = 1, N
            CALL ZLASSQ( N, A(1,J), 1, SCALE, SUM )
  290    CONTINUE
C
C        Add normF(G) and normF(Q).
C
         DSCL = ABS( DBLE( QG(1,1) ) )
         DSUM = ONE
         IF ( N.GT.1 ) THEN
            CALL ZLASSQ( N-1, QG(2,1), 1, SCALE, SUM )
            DUM(1) = DBLE( QG(1,2) )
            DUM(2) = DBLE( QG(2,2) )
            CALL DLASSQ( 2, DUM, 1, DSCL, DSUM )
         END IF
         IF ( N.GT.2 )
     $      CALL ZLASSQ( N-2, QG(3,2), 1, SCALE, SUM )
         DO 300 J = 3, N
            CALL ZLASSQ( J-2, QG(1,J), 1, SCALE, SUM )
            DUM(1) = DBLE( QG(J-1,J) )
            DUM(2) = DBLE( QG(J,  J) )
            CALL DLASSQ(   2, DUM,       1, DSCL, DSUM )
            CALL ZLASSQ( N-J, QG(J+1,J), 1, SCALE, SUM )
  300    CONTINUE
         IF ( N.GT.1 )
     $      CALL ZLASSQ( N-1, QG(1,N+1), 1, SCALE, SUM )
         DUM(1) = DBLE( QG(N,N+1) )
         CALL DLASSQ( 1, DUM, 1, DSCL, DSUM )
         VALUE = DLAPY2( SQRT( TWO )*SCALE*SQRT( SUM ),
     $                   DSCL*SQRT( DSUM ) )
      END IF
C
      MA02IZ = VALUE
      RETURN
C *** Last line of MA02IZ ***
      END
