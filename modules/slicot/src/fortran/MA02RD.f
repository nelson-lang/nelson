      SUBROUTINE MA02RD( ID, N, D, E, INFO )
C
C     Purpose
C
C     To sort the elements of an n-vector D in increasing order (if
C     ID = 'I') or in decreasing order (if ID = 'D'), and to rearrange
C     the elements of an n-vector E using the same permutations.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     ID      CHARACTER*1
C             Specifies the desired order, as follows:
C             = 'I': sort D in increasing order;
C             = 'D': sort D in decreasing order.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The length of the array D.
C
C     D       (input/output) DOUBLE PRECISION array, dimension (N)
C             On entry, this array must contain the vector to be sorted.
C             On exit, this array contains the vector D sorted into
C             increasing order (D(1) <= ... <= D(N) ) or into decreasing
C             order (D(1) >= ... >= D(N) ), depending on ID.
C
C     E       (input/output) DOUBLE PRECISION array, dimension (N)
C             On entry, this array must contain the vector to be
C             rearranged.
C             On exit, this array contains the vector E with rearranged
C             elements: if, on exit, D(i) contains D(j), E(i) will
C             contain E(j), for all i = 1, 2, ... N.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value.
C
C     METHOD
C
C     The routine uses Quick Sort, but reverts to Insertion sort on
C     arrays of size of length at most 20. Dimension of the local STACK
C     array limits N to about 2**32.
C
C     FURTHER COMMENTS
C
C     The routine is based on the LAPACK code DLASRT, but applies to E
C     the same interchanges used for D.
C
C     CONTRIBUTORS
C
C     V. Sima, Oct. 2023.
C
C     REVISIONS
C
C     -
C
C     KEYWORDS
C
C     insertion sort, Quick Sort, vector operations.
C
C     ******************************************************************
C
C     .. Parameters ..
      INTEGER            SELECT
      PARAMETER          ( SELECT = 20 )
C
C     .. Scalar Arguments ..
      CHARACTER          ID
      INTEGER            INFO, N
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E( * )
C     ..
C     ..
C     .. Local Scalars ..
      INTEGER            DIR, ENDD, I, J, START, STKPNT
      DOUBLE PRECISION   D1, D2, D3, DMNMX, TMP
C     ..
C     .. Local Arrays ..
      INTEGER            STACK( 2, 32 )
C     ..
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     ..
C     .. External Subroutines ..
      EXTERNAL           XERBLA
C     ..
C     .. Executable Statements ..
C
C     Test the input parameters.
C
      INFO = 0
      DIR = -1
      IF( LSAME( ID, 'D' ) ) THEN
         DIR = 0
      ELSE IF( LSAME( ID, 'I' ) ) THEN
         DIR = 1
      END IF
      IF( DIR.EQ.-1 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'MA02RD', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( N.LE.1 )
     $   RETURN
C
      STKPNT = 1
      STACK( 1, 1 ) = 1
      STACK( 2, 1 ) = N
   10 CONTINUE
      START = STACK( 1, STKPNT )
      ENDD  = STACK( 2, STKPNT )
      STKPNT = STKPNT - 1
      IF( ENDD-START.LE.SELECT .AND. ENDD-START.GT.0 ) THEN
C
C        Do Insertion sort on D( START:ENDD ).
C
         IF( DIR.EQ.0 ) THEN
C
C           Sort into decreasing order.
C
            DO 30 I = START + 1, ENDD
               DO 20 J = I, START + 1, -1
                  IF( D( J ).GT.D( J-1 ) ) THEN
                     DMNMX = D( J )
                     D( J ) = D( J-1 )
                     D( J-1 ) = DMNMX
                     DMNMX = E( J )
                     E( J ) = E( J-1 )
                     E( J-1 ) = DMNMX
                  ELSE
                     GO TO 30
                  END IF
   20          CONTINUE
   30       CONTINUE
C
         ELSE
C
C           Sort into increasing order.
C
            DO 50 I = START + 1, ENDD
               DO 40 J = I, START + 1, -1
                  IF( D( J ).LT.D( J-1 ) ) THEN
                     DMNMX = D( J )
                     D( J ) = D( J-1 )
                     D( J-1 ) = DMNMX
                     DMNMX = E( J )
                     E( J ) = E( J-1 )
                     E( J-1 ) = DMNMX
                  ELSE
                     GO TO 50
                  END IF
   40          CONTINUE
   50       CONTINUE
C
         END IF
C
      ELSE IF( ENDD-START.GT.SELECT ) THEN
C
C        Partition D( START:ENDD ) and stack parts, largest one first.
C
C        Choose partition entry as median of 3.
C
         D1 = D( START )
         D2 = D( ENDD )
         I  = ( START+ENDD ) / 2
         D3 = D( I )
         IF( D1.LT.D2 ) THEN
            IF( D3.LT.D1 ) THEN
               DMNMX = D1
            ELSE IF( D3.LT.D2 ) THEN
               DMNMX = D3
            ELSE
               DMNMX = D2
            END IF
         ELSE
            IF( D3.LT.D2 ) THEN
               DMNMX = D2
            ELSE IF( D3.LT.D1 ) THEN
               DMNMX = D3
            ELSE
               DMNMX = D1
            END IF
         END IF
C
         IF( DIR.EQ.0 ) THEN
C
C           Sort into decreasing order.
C
            I = START - 1
            J = ENDD  + 1
   60       CONTINUE
   70       CONTINUE
            J = J - 1
            IF( D( J ).LT.DMNMX )
     $         GO TO 70
   80       CONTINUE
            I = I + 1
            IF( D( I ).GT.DMNMX )
     $         GO TO 80
            IF( I.LT.J ) THEN
               TMP = D( I )
               D( I ) = D( J )
               D( J ) = TMP
               TMP = E( I )
               E( I ) = E( J )
               E( J ) = TMP
               GO TO 60
            END IF
            IF( J-START.GT.ENDD-J-1 ) THEN
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = START
               STACK( 2, STKPNT ) = J
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = J + 1
               STACK( 2, STKPNT ) = ENDD
            ELSE
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = J + 1
               STACK( 2, STKPNT ) = ENDD
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = START
               STACK( 2, STKPNT ) = J
            END IF
         ELSE
C
C           Sort into increasing order.
C
            I = START - 1
            J = ENDD  + 1
   90       CONTINUE
  100       CONTINUE
            J = J - 1
            IF( D( J ).GT.DMNMX )
     $         GO TO 100
  110       CONTINUE
            I = I + 1
            IF( D( I ).LT.DMNMX )
     $         GO TO 110
            IF( I.LT.J ) THEN
               TMP = D( I )
               D( I ) = D( J )
               D( J ) = TMP
               TMP = E( I )
               E( I ) = E( J )
               E( J ) = TMP
               GO TO 90
            END IF
            IF( J-START.GT.ENDD-J-1 ) THEN
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = START
               STACK( 2, STKPNT ) = J
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = J + 1
               STACK( 2, STKPNT ) = ENDD
            ELSE
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = J + 1
               STACK( 2, STKPNT ) = ENDD
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = START
               STACK( 2, STKPNT ) = J
            END IF
         END IF
      END IF
      IF( STKPNT.GT.0 )
     $   GO TO 10
      RETURN
C
C *** Last line of MA02RD ***
      END
