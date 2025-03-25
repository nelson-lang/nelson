      SUBROUTINE SG03BY( XR, XI, YR, YI, CR, CI, SR, SI, Z )
C
C     PURPOSE
C
C     To compute the parameters for the complex Givens rotation
C
C        (  CR-CI*I   SR-SI*I )   ( XR+XI*I )   ( Z )
C        (                    ) * (         ) = (   ),
C        ( -SR-SI*I   CR+CI*I )   ( YR+YI*I )   ( 0 )
C
C     where CR, CI, SR, SI, XR, XI, YR, YI are real numbers and I is the
C     imaginary unit, I = SQRT(-1). Z is a non-negative real number.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     XR, XI, (input) DOUBLE PRECISION
C     YR, YI  (input) DOUBLE PRECISION
C             The given real scalars XR, XI, YR, YI.
C
C     CR, CI, (output) DOUBLE PRECISION
C     SR, SI, (output) DOUBLE PRECISION
C     Z       (output) DOUBLE PRECISION
C             The computed real scalars CR, CI, SR, SI, Z, defining the
C             complex Givens rotation and Z.
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
C     T. Penzl, Technical University Chemnitz, Germany, Aug. 1998.
C
C     REVISIONS
C
C     Sep. 1998 (V. Sima).
C
C     ******************************************************************
C
C      .. Parameters ..
       DOUBLE PRECISION  ONE, ZERO
       PARAMETER         ( ONE = 1.0D+0, ZERO = 0.0D+0 )
C      .. Scalar Arguments ..
       DOUBLE PRECISION  CI, CR, SI, SR, XI, XR, YI, YR, Z
C      .. Intrinsic Functions ..
       DOUBLE PRECISION  ABS, MAX, SQRT
C      .. Executable Statements ..
C
       Z = MAX( ABS( XR ), ABS( XI ), ABS( YR ), ABS( YI ) )
C
       IF ( Z .EQ. ZERO ) THEN
          CR = ONE
          CI = ZERO
          SR = ZERO
          SI = ZERO
       ELSE
          Z = Z*SQRT( ( XR/Z )**2 + ( XI/Z )**2 +
     $                ( YR/Z )**2 + ( YI/Z )**2 )
          CR = XR/Z
          CI = XI/Z
          SR = YR/Z
          SI = YI/Z
       END IF
C
       RETURN
C
C *** Last line of SG03BY ***
       END
