      SUBROUTINE MB03VY( N, P, ILO, IHI, A, LDA1, LDA2, TAU, LDTAU,
     $                   DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To generate the real orthogonal matrices Q_1, Q_2, ..., Q_p,
C     which are defined as the product of ihi-ilo elementary reflectors
C     of order n, as returned by SLICOT Library routine MB03VD:
C
C        Q_j = H_j(ilo) H_j(ilo+1) . . . H_j(ihi-1).
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrices Q_1, Q_2, ..., Q_p.  N >= 0.
C
C     P       (input) INTEGER
C             The number p of transformation matrices.  P >= 1.
C
C     ILO     (input) INTEGER
C     IHI     (input) INTEGER
C             The values of the indices ilo and ihi, respectively, used
C             in the previous call of the SLICOT Library routine MB03VD.
C             1 <= ILO <= max(1,N); min(ILO,N) <= IHI <= N.
C
C     A       (input/output) DOUBLE PRECISION array, dimension
C             (LDA1,LDA2,N)
C             On entry, the leading N-by-N strictly lower triangular
C             part of A(*,*,j) must contain the vectors which define the
C             elementary reflectors used for reducing A_j, as returned
C             by SLICOT Library routine MB03VD, j = 1, ..., p.
C             On exit, the leading N-by-N part of A(*,*,j) contains the
C             N-by-N orthogonal matrix Q_j, j = 1, ..., p.
C
C     LDA1    INTEGER
C             The first leading dimension of the array A.
C             LDA1 >= max(1,N).
C
C     LDA2    INTEGER
C             The second leading dimension of the array A.
C             LDA2 >= max(1,N).
C
C     TAU     (input) DOUBLE PRECISION array, dimension (LDTAU,P)
C             The leading N-1 elements in the j-th column must contain
C             the scalar factors of the elementary reflectors used to
C             form the matrix Q_j, as returned by SLICOT Library routine
C             MB03VD.
C
C     LDTAU   INTEGER
C             The leading dimension of the array TAU.
C             LDTAU >= max(1,N-1).
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The length of the array DWORK.  LDWORK >= MAX(1,N).
C             For optimum performance LDWORK should be larger.
C
C             If LDWORK = -1, then a workspace query is assumed;
C             the routine only calculates the optimal size of the
C             DWORK array, returns this value as the first entry of
C             the DWORK array, and no error message related to LDWORK
C             is issued by XERBLA.
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
C     Each matrix Q_j is generated as the product of the elementary
C     reflectors used for reducing A_j. Standard LAPACK routines for
C     Hessenberg and QR decompositions are used.
C
C     REFERENCES
C
C     [1] Bojanczyk, A.W., Golub, G. and Van Dooren, P.
C         The periodic Schur decomposition: algorithms and applications.
C         Proc. of the SPIE Conference (F.T. Luk, Ed.), 1770, pp. 31-42,
C         1992.
C
C     [2] Sreedhar, J. and Van Dooren, P.
C         Periodic Schur form and some matrix equations.
C         Proc. of the Symposium on the Mathematical Theory of Networks
C         and Systems (MTNS'93), Regensburg, Germany (U. Helmke,
C         R. Mennicken and J. Saurer, Eds.), Vol. 1, pp. 339-362, 1994.
C
C     NUMERICAL ASPECTS
C
C     The algorithm is numerically stable.
C
C     CONTRIBUTOR
C
C     V. Sima, Katholieke Univ. Leuven, Belgium, and A. Varga,
C     German Aerospace Center, DLR Oberpfaffenhofen, February 1999.
C     Partly based on the routine PSHTR by A. Varga
C     (DLR Oberpfaffenhofen), November 26, 1995.
C
C     REVISIONS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Feb. 2004,
C     July 2012, June 2022.
C
C     KEYWORDS
C
C     Hessenberg form, orthogonal transformation, periodic systems,
C     similarity transformation, triangular form.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE
      PARAMETER         ( ZERO = 0.0D+0, ONE = 1.0D+0 )
C
C     .. Scalar Arguments ..
      INTEGER           IHI, ILO, INFO, LDA1, LDA2, LDTAU, LDWORK, N, P
C     ..
C     .. Array Arguments ..
      DOUBLE PRECISION  A( LDA1, LDA2, * ), DWORK( * ), TAU( LDTAU, * )
C     ..
C     .. Local Scalars ..
      LOGICAL           LQUERY
      INTEGER           J, NH, WRKOPT
C     ..
C     .. External Subroutines ..
      EXTERNAL          DLASET, DORGHR, DORGQR, XERBLA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC         INT, MAX, MIN
C     ..
C     .. Executable Statements ..
C
C     Test the input scalar arguments.
C
      INFO = 0
      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( P.LT.1 ) THEN
         INFO = -2
      ELSE IF( ILO.LT.1 .OR. ILO.GT.MAX( 1, N ) ) THEN
         INFO = -3
      ELSE IF( IHI.LT.MIN( ILO, N ) .OR. IHI.GT.N ) THEN
         INFO = -4
      ELSE IF( LDA1.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF( LDA2.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDTAU.LT.MAX( 1, N-1 ) ) THEN
         INFO = -9
      ELSE
         NH = IHI - ILO + 1
         LQUERY = LDWORK.EQ.-1
         IF( LQUERY ) THEN
            CALL DORGHR( N, ILO, IHI, A, LDA1, TAU, DWORK, -1, INFO )
            WRKOPT = MAX( 1, N, INT( DWORK( 1 ) ) )
            IF ( NH.GT.1 ) THEN
               CALL DORGQR( NH, NH, NH-1, A, LDA1, TAU, DWORK, -1, INFO)
               WRKOPT = MAX( WRKOPT, INT( DWORK( 1 ) ) )
            END IF
         END IF
         IF( LDWORK.LT.MAX( 1, N ) .AND. .NOT. LQUERY )
     $      INFO = -11
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'MB03VY', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF ( N.EQ.0 ) THEN
         DWORK( 1 ) = ONE
         RETURN
      ELSE IF( LQUERY ) THEN
         DWORK( 1 ) = WRKOPT
         RETURN
      END IF
C
C     Generate the orthogonal matrix Q_1.
C
      CALL DORGHR( N, ILO, IHI, A, LDA1, TAU, DWORK, LDWORK, INFO )
      WRKOPT = INT( DWORK( 1 ) )
C
      DO 20 J = 2, P
C
C        Generate the orthogonal matrix Q_j.
C        Set the first ILO-1 and the last N-IHI rows and columns of Q_j
C        to those of the unit matrix.
C
         CALL DLASET( 'Full', N, ILO-1, ZERO, ONE, A( 1, 1, J ), LDA1 )
         CALL DLASET( 'Full', ILO-1, NH, ZERO, ZERO, A( 1, ILO, J ),
     $                LDA1 )
         IF ( NH.GT.1 ) THEN
            CALL DORGQR( NH, NH, NH-1, A( ILO, ILO, J ), LDA1,
     $                   TAU( ILO, J ), DWORK, LDWORK, INFO )
         ELSE
            A( ILO, ILO, J ) = ONE
         END IF
         IF ( IHI.LT.N ) THEN
            CALL DLASET( 'Full', N-IHI, NH, ZERO, ZERO,
     $                   A( IHI+1, ILO, J ), LDA1 )
            CALL DLASET( 'Full', IHI, N-IHI, ZERO, ZERO,
     $                   A( 1, IHI+1, J ), LDA1 )
            CALL DLASET( 'Full', N-IHI, N-IHI, ZERO, ONE,
     $                   A( IHI+1, IHI+1, J ), LDA1 )
         END IF
   20 CONTINUE
C
      DWORK( 1 ) = MAX( WRKOPT, INT( DWORK( 1 ) ) )
      RETURN
C
C *** Last line of MB03VY ***
      END
