      INTEGER FUNCTION MA02OZ( SKEW, M, A, LDA, DE, LDDE )
C
C     PURPOSE
C
C     To compute the number of zero rows (and zero columns) of a complex
C     (skew-)Hamiltonian matrix,
C
C           (  A    D   )
C       H = (           ).
C           (  E  +/-A' )
C
C     FUNCTION VALUE
C
C     MA02OZ  INTEGER
C             The number of zero rows.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     SKEW    CHARACTER*1
C             Specifies whether the matrix is Hamiltonian or skew-
C             Hamiltonian as follows:
C             = 'H':  The matrix is Hamiltonian;
C             = 'S':  The matrix is skew-Hamiltonian.
C
C     Input/Output Parameters
C
C     M       (input) INTEGER
C             The order of the matrices A, D, and E.  M >= 0.
C
C     A       (input) COMPLEX*16 array, dimension (LDA,M)
C             The leading M-by-M part of this array must contain the
C             matrix A.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= max(1,M).
C
C     DE      (input) COMPLEX*16 array, dimension (LDDE,M+1)
C             The leading M-by-M lower triangular part of this array
C             must contain the lower triangular part of the (skew-)
C             Hermitian matrix E, and the M-by-M upper triangular
C             part of the submatrix in the columns 2 to M+1 of this
C             array must contain the upper triangular part of the
C             (skew-)Hermitian matrix D. If S is skew-Hamiltonian, the
C             real parts of the entries on the diagonal and the first
C             superdiagonal of this array, which should be zero, are
C             not used. If S is Hamiltonian, the imaginary parts of the
C             entries on the diagonal and the first superdiagonal of
C             this array, which should be zero, are not used.
C
C     LDDE    INTEGER
C             The leading dimension of the array DE.  LDDE >= MAX(1,M).
C
C     CONTRIBUTORS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Sep. 2016.
C
C     REVISIONS
C
C     -
C
C     KEYWORDS
C
C     Elementary matrix operations, skew-Hamiltonian matrix.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
      COMPLEX*16         CZERO
      PARAMETER          ( CZERO = ( 0.0D+0, 0.0D+0 ) )
C     ..
C     .. Scalar Arguments ..
      CHARACTER          SKEW
      INTEGER            LDA, LDDE, M
C     ..
C     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), DE( LDDE, * )
C     ..
C     .. Local Scalars ..
      LOGICAL            ISSKEW
      INTEGER            I, J, NZ
C     ..
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          DBLE, DIMAG
C     ..
C     .. Executable Statements ..
C
C     For efficiency reasons, the parameters are not checked.
C
      NZ = 0
C
      IF( M.GT.0 ) THEN
         ISSKEW = LSAME( SKEW, 'S' )
C
C        Scan columns 1 .. M.
C
         I = 0
C        WHILE ( I.LE.M ) DO
   10    CONTINUE
         I = I + 1
         IF( I.LE.M ) THEN
            DO 20  J = 1, M
               IF( A( J, I ).NE.CZERO )
     $            GO TO 10
   20       CONTINUE
            DO 30  J = 1, I - 1
               IF( DE( I, J ).NE.CZERO )
     $            GO TO 10
   30       CONTINUE
            IF( ISSKEW ) THEN
               IF( DIMAG( DE( I, I ) ).NE.ZERO )
     $            GO TO 10
            ELSE
               IF( DBLE( DE( I, I ) ).NE.ZERO )
     $            GO TO 10
            END IF
            DO 40  J = I + 1, M
               IF( DE( J, I ).NE.CZERO )
     $            GO TO 10
   40       CONTINUE
C
            NZ = NZ + 1
            GO TO 10
C
C           END WHILE 10
         END IF
C
C        Scan columns M+1 .. 2*M.
C
         I = 0
C        WHILE ( I.LE.M ) DO
   50    CONTINUE
         I = I + 1
         IF( I.LE.M ) THEN
            DO 60  J = 1, M
               IF( A( I, J ).NE.CZERO )
     $            GO TO 50
   60       CONTINUE
            DO 70  J = 1, I - 1
               IF( DE( J, I+1 ).NE.CZERO )
     $            GO TO 50
   70       CONTINUE
            IF( ISSKEW ) THEN
               IF( DIMAG( DE( I, I+1 ) ).NE.ZERO )
     $            GO TO 50
            ELSE
               IF( DBLE( DE( I, I+1 ) ).NE.ZERO )
     $            GO TO 50
            END IF
            DO 80  J = I + 1, M
               IF( DE( I, J+1 ).NE.CZERO )
     $            GO TO 50
   80       CONTINUE
C
            NZ = NZ + 1
            GO TO 50
C
         END IF
C        END WHILE 50
      END IF
C
      MA02OZ = NZ
      RETURN
C
C *** Last line of MA02OZ ***
      END
