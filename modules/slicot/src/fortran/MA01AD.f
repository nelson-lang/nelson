      SUBROUTINE MA01AD( XR, XI, YR, YI )
C
C     PURPOSE
C
C     To compute the complex square root YR + i*YI of a complex number
C     XR + i*XI  in real arithmetic.  The returned result is so that
C     YR >= 0.0  and  SIGN(YI) = SIGN(XI).
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     XR      (input) DOUBLE PRECISION
C     XI      (input) DOUBLE PRECISION
C             These scalars define the real and imaginary part of the
C             complex number of which the square root is sought.
C
C     YR      (output) DOUBLE PRECISION
C     YI      (output) DOUBLE PRECISION
C             These scalars define the real and imaginary part of the
C             complex square root.
C
C     METHOD
C
C     The complex square root YR + i*YI of the complex number XR + i*XI
C     is computed in real arithmetic, taking care to avoid overflow.
C
C     REFERENCES
C
C     Adapted from EISPACK subroutine CSROOT.
C
C     CONTRIBUTOR
C
C     P. Benner, Universitaet Bremen, Germany, and
C     R. Byers, University of Kansas, Lawrence, USA,
C     Aug. 1998, routine DCROOT.
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     Oct. 1998, SLICOT Library version.
C
C     REVISIONS
C
C     -
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, HALF
      PARAMETER         ( ZERO = 0.0D0, HALF = 1.0D0/2.0D0 )
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION  XR, XI, YR, YI
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION  S
C     ..
C     .. External Functions ..
      DOUBLE PRECISION  DLAPY2
      EXTERNAL          DLAPY2
C
C     .. Intrinsic functions ..
      INTRINSIC         ABS, SQRT
C     ..
C     .. Executable Statements ..
C
      S = SQRT( HALF*( DLAPY2( XR, XI ) + ABS( XR ) ) )
      IF ( XR.GE.ZERO ) YR =  S
      IF ( XI.LT.ZERO ) S  = -S
      IF ( XR.LE.ZERO ) THEN
         YI =  S
         IF ( XR.LT.ZERO ) YR =  HALF*( XI/S )
      ELSE
         YI =  HALF*( XI/YR )
      END IF
C
      RETURN
C     *** Last line of MA01AD ***
      END
