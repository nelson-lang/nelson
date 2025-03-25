      SUBROUTINE MD03BA( N, IPAR, LIPAR, FNORM, J, LDJ, E, JNORMS,
     $                   GNORM, IPVT, DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To compute the QR factorization with column pivoting of an
C     m-by-n Jacobian matrix J (m >= n), that is, J*P = Q*R, where Q is
C     a matrix with orthogonal columns, P a permutation matrix, and
C     R an upper trapezoidal matrix with diagonal elements of
C     nonincreasing magnitude, and to apply the transformation Q' on
C     the error vector e (in-situ). The 1-norm of the scaled gradient
C     is also returned.
C
C     This routine is an interface to SLICOT Library routine MD03BX,
C     for solving standard nonlinear least squares problems using SLICOT
C     routine MD03BD.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The number of columns of the Jacobian matrix J.  N >= 0.
C
C     IPAR    (input) INTEGER array, dimension (LIPAR)
C             The integer parameters describing the structure of the
C             matrix J, as follows:
C             IPAR(1) must contain the number of rows M of the Jacobian
C                     matrix J.  M >= N.
C             IPAR is provided for compatibility with SLICOT Library
C             routine MD03BD.
C
C     LIPAR   (input) INTEGER
C             The length of the array IPAR.  LIPAR >= 1.
C
C     FNORM   (input) DOUBLE PRECISION
C             The Euclidean norm of the vector e.  FNORM >= 0.
C
C     J       (input/output) DOUBLE PRECISION array, dimension (LDJ, N)
C             On entry, the leading M-by-N part of this array must
C             contain the Jacobian matrix J.
C             On exit, the leading N-by-N upper triangular part of this
C             array contains the upper triangular factor R of the
C             Jacobian matrix. Note that for efficiency of the later
C             calculations, the matrix R is delivered with the leading
C             dimension MAX(1,N), possibly much smaller than the value
C             of LDJ on entry.
C
C     LDJ     (input/output) INTEGER
C             The leading dimension of array J.
C             On entry, LDJ >= MAX(1,M).
C             On exit,  LDJ >= MAX(1,N).
C
C     E       (input/output) DOUBLE PRECISION array, dimension (M)
C             On entry, this array must contain the error vector e.
C             On exit, this array contains the updated vector Q'*e.
C
C     JNORMS  (output) DOUBLE PRECISION array, dimension (N)
C             This array contains the Euclidean norms of the columns
C             of the Jacobian matrix, considered in the initial order.
C
C     GNORM   (output) DOUBLE PRECISION
C             If FNORM > 0, the 1-norm of the scaled vector
C             J'*Q'*e/FNORM, with each element i further divided
C             by JNORMS(i) (if JNORMS(i) is nonzero).
C             If FNORM = 0, the returned value of GNORM is 0.
C
C     IPVT    (output) INTEGER array, dimension (N)
C             This array defines the permutation matrix P such that
C             J*P = Q*R. Column j of P is column IPVT(j) of the identity
C             matrix.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The length of the array DWORK.
C             LDWORK >= 1,      if N = 0 or  M = 1;
C             LDWORK >= 4*N+1,  if N > 1.
C             For optimum performance LDWORK should be larger.
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
C     This routine calls SLICOT Library routine MD03BX to perform the
C     calculations.
C
C     FURTHER COMMENTS
C
C     For efficiency, the arguments are not checked. This is done in
C     the routine MD03BX (except for LIPAR).
C
C     CONTRIBUTORS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Dec. 2001.
C
C     REVISIONS
C
C     -
C
C     KEYWORDS
C
C     Elementary matrix operations, Jacobian matrix, matrix algebra,
C     matrix operations.
C
C     ******************************************************************
C
C     .. Scalar Arguments ..
      INTEGER           INFO, LDJ, LDWORK, LIPAR, N
      DOUBLE PRECISION  FNORM, GNORM
C     .. Array Arguments ..
      INTEGER           IPAR(*), IPVT(*)
      DOUBLE PRECISION  DWORK(*), E(*), J(*), JNORMS(*)
C     .. External Subroutines ..
      EXTERNAL          MD03BX
C     ..
C     .. Executable Statements ..
C
      CALL MD03BX( IPAR(1), N, FNORM, J, LDJ, E, JNORMS, GNORM, IPVT,
     $             DWORK, LDWORK, INFO )
      RETURN
C
C *** Last line of MD03BA ***
      END
