      SUBROUTINE MB03QV( N, S, LDS, T, LDT, ALPHAR, ALPHAI, BETA, INFO )
C
C     PURPOSE
C
C     To compute the eigenvalues of an upper quasi-triangular matrix
C     pencil.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrices S and T.  N >= 0.
C
C     S       (input) DOUBLE PRECISION array, dimension(LDS,N)
C             The upper quasi-triangular matrix S.
C
C     LDS     INTEGER
C             The leading dimension of the array S.  LDS >= max(1,N).
C
C     T       (input) DOUBLE PRECISION array, dimension(LDT,N)
C             The upper triangular matrix T.
C
C     LDT     INTEGER
C             The leading dimension of the array T.  LDT >= max(1,N).
C
C     ALPHAR  (output) DOUBLE PRECISION array, dimension (N)
C     ALPHAI  (output) DOUBLE PRECISION array, dimension (N)
C     BETA    (output) DOUBLE PRECISION array, dimension (N)
C             On exit, (ALPHAR(j) + ALPHAI(j)*i)/BETA(j), j=1,...,N,
C             are the generalized eigenvalues.
C             ALPHAR(j) + ALPHAI(j)*i, and  BETA(j),j=1,...,N, are the
C             diagonals of the complex Schur form (S,T) that would
C             result if the 2-by-2 diagonal blocks of the real Schur
C             form of (A,B) were further reduced to triangular form
C             using 2-by-2 complex unitary transformations.
C             If ALPHAI(j) is zero, then the j-th eigenvalue is real; if
C             positive, then the j-th and (j+1)-st eigenvalues are a
C             complex conjugate pair, with ALPHAI(j+1) negative.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value.
C
C     CONTRIBUTOR
C
C     A. Varga, German Aerospace Center, DLR Oberpfaffenhofen,
C     October 2002.
C
C     REVISIONS
C
C     V. Sima, Dec. 2016.
C
C     ******************************************************************
C     .. Parameters ..
      DOUBLE PRECISION ZERO
      PARAMETER        ( ZERO = 0.0D0 )
C     .. Scalar Arguments ..
      INTEGER          INFO, LDS, LDT, N
C     .. Array Arguments ..
      DOUBLE PRECISION ALPHAI(*), ALPHAR(*), BETA(*), S(LDS,*), T(LDT,*)
C     .. Local Scalars ..
      INTEGER          I, INEXT
      DOUBLE PRECISION SAFMIN
C     .. External Functions ..
      EXTERNAL         DLAMCH
      DOUBLE PRECISION DLAMCH
C     .. External Subroutines ..
      EXTERNAL         DLAG2, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC        MAX
C     .. Executable Statements ..
C
      INFO = 0
C
C     Test the input scalar arguments.
C
      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( LDS.LT.MAX( 1, N ) ) THEN
         INFO = -3
      ELSE IF( LDT.LT.MAX( 1, N ) ) THEN
         INFO = -5
      END IF
C
      IF( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'MB03QV', -INFO )
         RETURN
      END IF
C
      SAFMIN = DLAMCH( 'S' )
      INEXT  = 1
      DO 10 I = 1, N
         IF( I.LT.INEXT )
     $      GO TO 10
         IF( I.NE.N ) THEN
            IF( S(I+1,I).NE.ZERO ) THEN
C
C              A pair of eigenvalues.
C
               INEXT = I + 2
               CALL DLAG2( S(I,I), LDS, T(I,I), LDT, SAFMIN, BETA(I),
     $                     BETA(I+1), ALPHAR(I), ALPHAR(I+1),
     $                     ALPHAI(I) )
               ALPHAI(I+1) = -ALPHAI(I)
               GO TO 10
            END IF
         END IF
C
C        Simple eigenvalue.
C
         INEXT = I + 1
         ALPHAR(I) = S(I,I)
         ALPHAI(I) = ZERO
         BETA(I)   = T(I,I)
   10 CONTINUE
C
      RETURN
C *** Last line of MB03QV ***
      END
