      SUBROUTINE SG03BR( XR, XI, YR, YI, C, SR, SI, ZR, ZI )
C
C     PURPOSE
C
C     To compute the parameters for the complex Givens rotation
C
C        (    C      SR+SI*I )   ( XR+XI*I )   ( ZR+ZI*I )
C        (                   ) * (         ) = (         )
C        ( -SR+SI*I     C    )   ( YR+YI*I )   (    0    )
C
C     where C, SR, SI, XR, XI, YR, YI, ZR, ZI are real numbers, I is the
C     imaginary unit, I = SQRT(-1), and C**2 + |SR+SI*I|**2 = 1.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     XR, XI, (input) DOUBLE PRECISION
C     YR, YI  (input) DOUBLE PRECISION
C             The given real scalars XR, XI, YR, YI.
C
C     C,      (output) DOUBLE PRECISION
C     SR, SI, (output) DOUBLE PRECISION
C     ZR, ZI  (output) DOUBLE PRECISION
C             The computed real scalars C, SR, SI, ZR, ZI defining the
C             complex Givens rotation and Z = ZR+ZI*I.
C
C     NUMERICAL ASPECTS
C
C     The subroutine avoids unnecessary overflow.
C
C     FURTHER COMMENTS
C
C     In the interest of speed, this routine does not check the input
C     for errors.
C
C     CONTRIBUTOR
C
C     V. Sima, Dec. 2021.
C     This is an adaptation for real data of the LAPACK routine ZLARTG.
C
C     REVISIONS
C
C     V. Sima, Jan. 2022.
C
C     ******************************************************************
C
C      .. Parameters ..
       DOUBLE PRECISION  ONE, TWO, ZERO
       PARAMETER         ( ONE = 1.0D+0, TWO = 2.0D+0, ZERO = 0.0D+0 )
C      .. Scalar Arguments ..
       DOUBLE PRECISION  C, SI, SR, XI, XR, YI, YR, ZI, ZR
C     .. Local Scalars ..
      INTEGER            COUNT, I
      DOUBLE PRECISION   D, DI, DR, EPS, SAFMIN, SAFMN2, SAFMX2, SCALE,
     $                   TI, TR, X2, X2S, XIS, XRS, Y2, Y2S, YIS, YRS
C     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DLAPY2
      EXTERNAL           DLAMCH, DLAPY2
C      .. Intrinsic Functions ..
       DOUBLE PRECISION  ABS, INT, LOG, MAX, SQRT
C
C     Do not check input parameters for errors.
C
      SAFMIN = DLAMCH( 'S' )
      EPS    = DLAMCH( 'E' )
      SAFMN2 = DLAMCH( 'B' )**INT( LOG( SAFMIN / EPS ) /
     $         LOG( DLAMCH( 'B' ) ) / TWO )
      SAFMX2 = ONE / SAFMN2
C
      SCALE = MAX( ABS( XR ), ABS( XI ), ABS( YR ), ABS( YI ) )
C
      XRS   = XR
      XIS   = XI
      YRS   = YR
      YIS   = YI
      COUNT = 0
C
      IF( SCALE.GE.SAFMX2 ) THEN
C
   10    CONTINUE
         COUNT = COUNT + 1
         XRS   =   XRS*SAFMN2
         XIS   =   XIS*SAFMN2
         YRS   =   YRS*SAFMN2
         YIS   =   YIS*SAFMN2
         SCALE = SCALE*SAFMN2
         IF( SCALE.GE.SAFMX2 )
     $      GO TO 10
C
      ELSE IF( SCALE.LE.SAFMN2 ) THEN
C
         IF( YR.EQ.ZERO .AND. YI.EQ.ZERO ) THEN
            C  = ONE
            SR = ZERO
            SI = ZERO
            ZR = XR
            ZI = XI
            RETURN
         END IF
C
   20    CONTINUE
         COUNT = COUNT - 1
         XRS   =   XRS*SAFMX2
         XIS   =   XIS*SAFMX2
         YRS   =   YRS*SAFMX2
         YIS   =   YIS*SAFMX2
         SCALE = SCALE*SAFMX2
         IF( SCALE.LE.SAFMN2 )
     $      GO TO 20
C
      END IF
C
      X2 = XRS**2 + XIS**2
      Y2 = YRS**2 + YIS**2
C
      IF( X2.LE.MAX( Y2, ONE )*SAFMIN ) THEN
C
C        This is a rare case: XR + I*XI is very small.
C
         IF( XR.EQ.ZERO .AND. XI.EQ.ZERO ) THEN
            C  = ZERO
            ZR = DLAPY2( YR, YI )
            ZI = ZERO
C
C           Do complex/real division explicitly with two real divisions.
C
            D  =  DLAPY2( YRS, YIS )
            SR =  YRS / D
            SI = -YIS / D
            RETURN
         END IF
C
         X2S = DLAPY2( XRS, XIS )
C
C        Y2 and Y2S are accurate.
C        Y2 is at least SAFMIN, and Y2S is at least SAFMN2.
C
         Y2S = SQRT( Y2 )
         C   = X2S / Y2S
C
C        Make sure abs(XR+iXI) = 1.
C        Do complex/real division explicitly with two real divisions.
C
         IF( MAX( ABS( XR ), ABS( XI ) ).GT.ONE ) THEN
            D  = DLAPY2( XR, XI )
            TR = XR / D
            TI = XI / D
         ELSE
            DR = SAFMX2*XR
            DI = SAFMX2*XI
            D  = DLAPY2( DR, DI )
            TR = DR / D
            TI = DI / D
         END IF
C
         SR = TR*( YRS / Y2S ) + TI*( YIS / Y2S )
         SI = TI*( YRS / Y2S ) - TR*( YIS / Y2S )
         ZR = C*XR + SR*YR - SI*YI
         ZI = C*XI + SI*YR + SR*YI
C
      ELSE
C
C        This is the most common case.
C        Neither X2 nor X2/Y2 are less than SAFMIN.
C        X2S cannot overflow, and it is accurate.
C
         X2S = SQRT( ONE + Y2 / X2 )
C
C        Do the X2S*XS multiply with two real multiplies.
C
         ZR = X2S*XRS
         ZI = X2S*XIS
         C  = ONE / X2S
         D  = X2 + Y2
C
C        Do complex/real division explicitly with two real divisions.
C
         SR = ZR / D
         SI = ZI / D
         DR = SR*YRS + SI*YIS
         SI = SI*YRS - SR*YIS
         SR = DR
C
         IF( COUNT.NE.0 ) THEN
            IF( COUNT.GT.0 ) THEN
C
               DO 30 I = 1, COUNT
                  ZR = ZR*SAFMX2
                  ZI = ZI*SAFMX2
   30          CONTINUE
C
            ELSE
C
               DO 40 I = 1, -COUNT
                  ZR = ZR*SAFMN2
                  ZI = ZI*SAFMN2
   40          CONTINUE
C
            END IF
         END IF
C
      END IF
C
      RETURN
C
C *** Last line of SG03BR ***
      END
