      SUBROUTINE TB01TY( MODE, IOFF, JOFF, NROW, NCOL, SIZE, X, LDX,
     $                   BVECT )
C
C     PURPOSE
C
C     Balances the rows (MODE .EQ. 0) or columns (MODE .NE. 0) of the
C     (NROW x NCOL) block of the matrix X with offset (IOFF,JOFF), i.e.
C     with first (top left) element (IOFF + 1,JOFF + 1).  Each non-
C     zero row (column) is balanced in the sense that it is multiplied
C     by that integer power of the base of the machine floating-point
C     representation for which the sum of the absolute values of its
C     entries (i.e. its 1-norm) satisfies
C
C        (SIZE / BASE) .LT. ABSSUM .LE. SIZE
C
C     for SIZE as input.  (Note that this form of scaling does not
C     introduce any rounding errors.)  The vector BVECT then contains
C     the appropriate scale factors in rows (IOFF + 1)...(IOFF + NROW)
C     (columns (JOFF + 1)...(JOFF + NCOL) ).  In particular, if the
C     I-th row (J-th column) of the block is 'numerically' non-zero
C     with 1-norm given by BASE**(-EXPT) for some real EXPT, then the
C     desired scale factor (returned as element IOFF + I (JOFF + J) of
C     BVECT) is BASE**IEXPT, where IEXPT is the largest integer .LE.
C     EXPT: this integer is precisely the truncation INT(EXPT) except
C     for negative non-integer EXPT, in which case this value is too
C     high by 1 and so must be adjusted accordingly.  Finally, note
C     that the element of BVECT corresponding to a 'numerically' zero
C     row (column) is simply set equal to 1.0.
C
C     For efficiency, no tests of the input scalar parameters are
C     performed.
C
C     REVISIONS
C
C     -
C
C     ******************************************************************
C
      DOUBLE PRECISION  ONE
      PARAMETER         ( ONE = 1.0D0 )
C     .. Scalar Arguments ..
      INTEGER           IOFF, JOFF, LDX, MODE, NCOL, NROW
      DOUBLE PRECISION  SIZE
C     .. Array Arguments ..
      DOUBLE PRECISION  BVECT(*), X(LDX,*)
C     .. Local Scalars ..
      DOUBLE PRECISION  ABSSUM, DIV, EPS, EXPT, SCALE, TEST
      INTEGER           BASE, I, IEXPT, J
C     .. External Functions ..
      DOUBLE PRECISION  DASUM, DLAMCH
      EXTERNAL          DASUM, DLAMCH
C     .. External Subroutines ..
      EXTERNAL          DSCAL
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, DBLE, INT, LOG
C     .. Executable Statements ..
C
      BASE = DLAMCH( 'Base' )
      EPS  = DLAMCH( 'Epsilon' )
C
      DIV = ONE/LOG( DBLE( BASE ) )
      IF ( MODE.NE.0 ) THEN
C
C        Balance one column at a time using its column-sum norm.
C
         DO 10 J = JOFF + 1, JOFF + NCOL
            ABSSUM = DASUM( NROW, X(IOFF+1,J), 1 )/ABS( SIZE )
            TEST = ABSSUM/DBLE( NROW )
            IF ( TEST.GT.EPS ) THEN
C
C              Non-zero column: calculate (and apply) correct scale
C              factor.
C
               EXPT = -DIV*LOG( ABSSUM )
               IEXPT = INT( EXPT )
               IF ( ( IEXPT.LT.0 ) .AND. ( DBLE( IEXPT ).NE.EXPT ) )
     $             IEXPT = IEXPT - 1
               SCALE = DBLE( BASE )**IEXPT
               BVECT(J) = SCALE
               CALL DSCAL( NROW, SCALE, X(IOFF+1,J), 1 )
            ELSE
C
C              'Numerically' zero column: do not rescale.
C
               BVECT(J) = ONE
            END IF
   10    CONTINUE
C
      ELSE
C
C        Balance one row at a time using its row-sum norm.
C
         DO 20 I = IOFF + 1, IOFF + NROW
            ABSSUM = DASUM( NCOL, X(I,JOFF+1), LDX )/ABS( SIZE )
            TEST = ABSSUM/DBLE( NCOL )
            IF ( TEST.GT.EPS ) THEN
C
C              Non-zero row: calculate (and apply) correct scale factor.
C
               EXPT = -DIV*LOG( ABSSUM )
               IEXPT = INT( EXPT )
               IF ( ( IEXPT.LT.0 ) .AND. ( DBLE( IEXPT ).NE.EXPT ) )
     $             IEXPT = IEXPT - 1
C
               SCALE = DBLE( BASE )**IEXPT
               BVECT(I) = SCALE
               CALL DSCAL( NCOL, SCALE, X(I,JOFF+1), LDX )
            ELSE
C
C              'Numerically' zero row: do not rescale.
C
               BVECT(I) = ONE
            END IF
   20    CONTINUE
C
      END IF
C
      RETURN
C *** Last line of TB01TY ***
      END
