      SUBROUTINE MB04RD( JOBX, JOBY, SORT, N, PMAX, A, LDA, B, LDB, X,
     $                   LDX, Y, LDY, NBLCKS, BLSIZE, ALPHAR, ALPHAI,
     $                   BETA, TOL, IWORK, DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To reduce a real matrix pair (A,B) in generalized real Schur form
C     to a block-diagonal form using well-conditioned non-orthogonal
C     equivalence transformations. The condition numbers of the left and
C     right transformations used for the reduction are roughly bounded
C     by PMAX, where PMAX is a given value. The transformations are
C     optionally postmultiplied in the given matrices X and Y. The
C     generalized Schur form is optionally ordered, so that clustered
C     eigenvalues are grouped in the same pair of blocks.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOBX    CHARACTER*1
C             Specifies whether or not the left transformations are
C             accumulated, as follows:
C             = 'N':  The left transformations are not accumulated;
C             = 'U':  The left transformations are accumulated in X
C                     (the given matrix X is updated).
C
C     JOBY    CHARACTER*1
C             Specifies whether or not the right transformations are
C             accumulated, as follows:
C             = 'N':  The right transformations are not accumulated;
C             = 'U':  The right transformations are accumulated in Y
C                     (the given matrix Y is updated).
C
C     SORT    CHARACTER*1
C             Specifies whether or not the diagonal blocks of the
C             generalized real Schur form are reordered, as follows:
C             = 'N':  The diagonal blocks are not reordered;
C             = 'S':  The diagonal blocks are reordered before each
C                     step of reduction, so that clustered eigenvalues
C                     appear in the same pair of blocks.
C             = 'C':  The diagonal blocks are not reordered, but the
C                     "closest-neighbour" strategy is used instead of
C                     the standard "closest to the mean" strategy (see
C                     METHOD);
C             = 'B':  The diagonal blocks are reordered before each
C                     step of reduction, and the "closest-neighbour"
C                     strategy is used (see METHOD).
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrices A, B, X and Y.  N >= 0.
C
C     PMAX    (input) DOUBLE PRECISION
C             An upper bound for the absolute value of the elements of
C             the individual transformations used for reduction
C             (see METHOD and FURTHER COMMENTS).  PMAX >= 1.0D0.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N upper quasi-triangular part
C             of this array must contain the upper quasi-triangular
C             matrix A in the generalized real Schur form, as returned
C             by the LAPACK Library routine DGGES. The lower triangular
C             part below the Schur matrix is used as workspace.
C             On exit, the leading N-by-N upper quasi-triangular part of
C             this array contains the computed block-diagonal matrix, in
C             real Schur canonical form, corresponding to the given
C             matrix A. The remaining part is set to zero.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     B       (input/output) DOUBLE PRECISION array, dimension (LDB,N)
C             On entry, the leading N-by-N upper triangular part of this
C             array must contain the upper triangular matrix B in the
C             generalized real Schur form, as returned by the LAPACK
C             Library routine DGGES. The diagonal elements of B are
C             non-negative. The strictly lower triangular part is used
C             as workspace. The matrix B is assumed nonzero.
C             On exit, the leading N-by-N upper triangular part of this
C             array contains the computed upper triangular block-
C             diagonal matrix, corresponding to the given matrix B. The
C             remaining part is set to zero. The diagonal elements of B
C             are non-negative.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= MAX(1,N).
C
C     X       (input/output) DOUBLE PRECISION array, dimension (LDX,*)
C             On entry, if JOBX = 'U', the leading N-by-N part of this
C             array must contain a given matrix X, for instance the left
C             transformation matrix VSL returned by the LAPACK Library
C             routine DGGES.
C             On exit, if JOBX = 'U', the leading N-by-N part of this
C             array contains the product of the given matrix X and the
C             left transformation matrix that reduced (A,B) to block-
C             diagonal form. The local transformation matrix is itself a
C             product of non-orthogonal equivalence transformations
C             having elements with magnitude less than or equal to PMAX.
C             If JOBX = 'N', this array is not referenced.
C
C     LDX     INTEGER
C             The leading dimension of the array X.
C             LDX >= 1,        if JOBX = 'N';
C             LDX >= MAX(1,N), if JOBX = 'U'.
C
C     Y       (input/output) DOUBLE PRECISION array, dimension (LDY,*)
C             On entry, if JOBY = 'U', the leading N-by-N part of this
C             array must contain a given matrix Y, for instance the
C             right transformation matrix VSR returned by the LAPACK
C             Library routine DGGES.
C             On exit, if JOBY = 'U', the leading N-by-N part of this
C             array contains the product of the given matrix Y and the
C             right transformation matrix that reduced (A,B) to block-
C             diagonal form. The local transformation matrix is itself a
C             product of non-orthogonal equivalence transformations
C             having elements with magnitude less than or equal to PMAX.
C             If JOBY = 'N', this array is not referenced.
C
C     LDY     INTEGER
C             The leading dimension of the array Y.
C             LDY >= 1,        if JOBY = 'N';
C             LDY >= MAX(1,N), if JOBY = 'U'.
C
C     NBLCKS  (output) INTEGER
C             The number of diagonal blocks of the matrices A and B.
C
C     BLSIZE  (output) INTEGER array, dimension (N)
C             The first NBLCKS elements of this array contain the orders
C             of the resulting diagonal blocks of the matrices A and B.
C
C     ALPHAR  (output) DOUBLE PRECISION array, dimension (N)
C     ALPHAI  (output) DOUBLE PRECISION array, dimension (N)
C     BETA    (output) DOUBLE PRECISION array, dimension (N)
C             On exit, if INFO = 0, (ALPHAR(j) + ALPHAI(j)*i)/BETA(j),
C             j = 1, ..., N, will be the generalized eigenvalues.
C             ALPHAR(j) + ALPHAI(j)*i, and BETA(j), j = 1, ..., N, are
C             the diagonals of the complex Schur form (S,T) that would
C             result if the 2-by-2 diagonal blocks of the real Schur
C             form of (A,B) were further reduced to triangular form
C             using 2-by-2 complex unitary transformations.
C             If ALPHAI(j) is zero, then the j-th eigenvalue is real; if
C             positive, then the j-th and (j+1)-st eigenvalues are a
C             complex conjugate pair, with ALPHAI(j+1) negative.
C             All BETA(j) are non-negative real numbers.
C             The quotients ALPHAR(j)/BETA(j) and ALPHAI(j)/BETA(j) may
C             easily over- or underflow, and BETA(j) may even be zero.
C             Thus, the user should avoid naively computing the ratio.
C             If A and B are obtained from general matrices using DGGES,
C             ALPHA will be always less than and usually comparable with
C             norm(A) in magnitude, and BETA always less than and
C             usually comparable with norm(B).
C
C     Tolerances
C
C     TOL     DOUBLE PRECISION
C             If SORT = 'S' or 'B', the tolerance to be used in the
C             ordering of the diagonal blocks of the upper triangular
C             matrix pair.
C             If the user sets TOL > 0, then the given value of TOL is
C             used as an absolute tolerance: a pair of blocks i and a
C             temporarily fixed pair of blocks 1 (the first pair of
C             blocks of the current trailing pair of submatrices to be
C             reduced) are considered to belong to the same cluster if
C             their eigenvalues satisfy the following "distance"
C             condition
C
C               | lambda_1 - lambda_i | <= TOL.
C
C             If the user sets TOL < 0, then the given value of TOL is
C             used as a relative tolerance: a pair of blocks i and a
C             temporarily fixed pair of blocks 1 are considered to
C             belong to the same cluster if their eigenvalues satisfy,
C             for finite lambda_j, j = 1, ..., N,
C
C               | lambda_1 - lambda_i | <= | TOL | * max | lambda_j |.
C
C             If the user sets TOL = 0, then an implicitly computed,
C             default tolerance, defined by TOL = SQRT( SQRT( EPS ) )
C             is used instead, as a relative tolerance, where EPS is
C             the machine precision (see LAPACK Library routine DLAMCH).
C             The approximate symmetric chordal metric is used as
C             "distance" of two complex, possibly infinite numbers, x
C             and y. This metric is given by the formula
C
C               d(x,y) = min( |x-y|, |1/x-1/y| ),
C
C             taking into account the special cases of infinite or NaN
C             values.
C             If SORT = 'N' or 'C', this parameter is not referenced.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (N+6)
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) contains the optimal value
C             of LDWORK.
C             On exit, if INFO = -22, DWORK(1) returns the minimum
C             value of LDWORK. When LDWORK = 0 is set on entry, the
C             routine will return this value for INFO, and also set
C             DWORK(1), but no error message related to LDWORK is
C             issued by XERBLA.
C
C     LDWORK  INTEGER
C             The dimension of the array DWORK.
C             LDWORK >= 1,         if N <= 1;
C             LDWORK >= 4*N + 16,  if N >  1.
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
C                   value;
C             = 1:  the matrix pencil defined by A and B is singular.
C
C     METHOD
C
C     Consider first that SORT = 'N'. Let
C
C            ( A    A   )       ( B    B   )
C            (  11   12 )       (  11   12 )
C        A = (          ),  B = (          ),
C            ( 0    A   )       ( 0    B   )
C            (       22 )       (       22 )
C
C     be the given matrix pair in generalized real Schur form, where
C     initially A   and B   are the first pair of diagonal blocks of
C                11      11
C     dimension 1-by-1 or 2-by-2. An attempt is made to compute the
C     transformation matrices X and Y of the form
C
C            ( I   V )       ( I   W )
C        X = (       ),  Y = (       )                               (1)
C            ( 0   I )       ( 0   I )
C
C     (partitioned as A and B ), so that (' denotes the transpose)
C
C                 ( A     0  )            ( B     0  )
C                 (  11      )            (  11      )
C        X' A Y = (          ),  X' B Y = (          ),
C                 ( 0    A   )            ( 0    B   )
C                 (       22 )            (       22 )
C
C     and the elements of V and W do not exceed the value PMAX in
C     magnitude. An adaptation of the standard method for solving
C     generalized Sylvester equations [1], which controls the magnitude
C     of the individual elements of the computed solution [2], is used
C     to obtain V and W. When this attempt fails, a 1-by-1 (or 2-by-2)
C     pair of diagonal blocks of (A  , B  ), whose eigenvalue(s) is
C                                  22   22
C     (are) the closest to the mean of those of (A  , B  ) is selected,
C                                                 11   11
C     and moved by orthogonal equivalence transformations in the leading
C     position of (A  , B  ); the moved diagonal blocks in A and B are
C                   22   22
C     then added to the blocks A   and B  , respectively, increasing
C                               11      11
C     their order by 1 (or 2). Another attempt is made to compute
C     suitable transformation matrices X and Y with the new definitions
C     of the blocks A  , A  , B  , and B  . After successful
C                    11   22   11       22
C     transformation matrices X and Y have been obtained, they
C     postmultiply the current transformation matrices (if JOBX = 'U'
C     and/or JOBY = 'U') and the whole procedure is repeated for the new
C     blocks A   and B  .
C             22      22
C
C     When SORT = 'S', the diagonal blocks of the generalized real Schur
C     form are reordered before each step of the reduction, so that each
C     cluster of generalized eigenvalues, defined as specified in the
C     definition of TOL, appears in adjacent blocks. The blocks for
C     each cluster are merged together, and the procedure described
C     above is applied to the larger blocks. Using the option SORT = 'S'
C     will usually provide better efficiency than the standard option
C     (SORT = 'N'), proposed in [2], because there could be no or few
C     unsuccessful attempts to compute individual transformation
C     matrices X and Y of the form (1). However, the resulting
C     dimensions of the blocks are usually larger; this could make
C     subsequent calculations less efficient.
C
C     When SORT = 'C' or 'B', the procedure is similar to that for
C     SORT = 'N' or 'S', respectively, but the blocks of A   and B
C                                                         22      22
C     whose eigenvalue(s) is (are) the closest to those of (A  , B  )
C                                                            11   11
C     (not to their mean) are selected and moved to the leading position
C     of A   and B  . This is called the "closest-neighbour" strategy.
C         22      22
C
C     REFERENCES
C
C     [1] Kagstrom, B. and Westin, L.
C         Generalized Schur Methods with Condition Estimators for
C         Solving the Generalized Sylvester Equation.
C         IEEE Trans. Auto. Contr., 34, pp. 745-751, 1989.
C
C     [2] Bavely, C. and Stewart, G.W.
C         An Algorithm for Computing Reducing Subspaces by Block
C         Diagonalization.
C         SIAM J. Numer. Anal., 16, pp. 359-367, 1979.
C
C     [3] Demmel, J.
C         The Condition Number of Equivalence Transformations that
C         Block Diagonalize Matrix Pencils.
C         SIAM J. Numer. Anal., 20, pp. 599-610, 1983.
C
C     NUMERICAL ASPECTS
C                                       3                     4
C     The algorithm usually requires 0(N ) operations, but 0(N ) are
C     possible in the worst case, when the matrix pencil cannot be
C     diagonalized by well-conditioned transformations.
C
C     FURTHER COMMENTS
C
C     The individual non-orthogonal transformation matrices used in the
C     reduction of A and B to a block-diagonal form have condition
C     numbers of the order PMAX. This does not guarantee that their
C     product is well-conditioned enough. The routine can be easily
C     modified to provide estimates for the condition numbers of the
C     clusters of generalized eigenvalues.
C
C     CONTRIBUTOR
C
C     V. Sima, Nov. 2022.
C
C     REVISIONS
C
C     V. Sima, Dec. 2022, Feb. 2023, Mar. 2023, Apr. 2023, Jan. 24.
C
C     KEYWORDS
C
C     Diagonalization, orthogonal transformation, Schur form, Sylvester
C     equation.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE, TEN
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0, TEN = 1.0D1 )
C     .. Scalar Arguments ..
      CHARACTER         JOBX, JOBY, SORT
      INTEGER           INFO, LDA, LDB, LDWORK, LDX, LDY, N, NBLCKS
      DOUBLE PRECISION  PMAX, TOL
C     .. Array Arguments ..
      INTEGER           BLSIZE(*), IWORK(*)
      DOUBLE PRECISION  A(LDA,*), ALPHAI(*), ALPHAR(*), B(LDB,*),
     $                  BETA(*), DWORK(*), X(LDX,*), Y(LDY,*)
C     .. Local Scalars ..
      LOGICAL           GOON, LQUERY, LSORN, LSORS, LSORT, PINF, WANTX,
     $                  WANTY
      INTEGER           DA11, DA22, I, IERR, K, KF, L, L11, L22, L22M1,
     $                  MAXWRK, MINWRK
      DOUBLE PRECISION  ABSA, AVI, AVR, BIGNUM, BIR, BKR, BLR, C, D,
     $                  DAV, DC, EII, EIR, EKI, EKR, ELI, ELR, EPS,
     $                  MXEV, NRMB, SAFEMN, SC, SCALE, THRESH, TOLB
C     .. Local Arrays ..
      DOUBLE PRECISION  DUM(1)
C     .. External Functions ..
      LOGICAL           LSAME
      DOUBLE PRECISION  DLAMCH, DLANHS, DLANTR, DLAPY2, DNRM2
      EXTERNAL          DLAMCH, DLANHS, DLANTR, DLAPY2, DNRM2, LSAME
C     .. External Subroutines ..
      EXTERNAL          DGEMM, DLASET, DSCAL, DTGEXC, MA01DZ, MA02AD,
     $                  MB03QV, MB04RT, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, MAX, SIGN, SQRT
C     .. Executable Statements ..
C
C     Test the input scalar arguments.
C
      INFO   = 0
      WANTX  = LSAME( JOBX, 'U' )
      WANTY  = LSAME( JOBY, 'U' )
      LSORN  = LSAME( SORT, 'N' )
      LSORS  = LSAME( SORT, 'S' )
      LSORT  = LSAME( SORT, 'B' ) .OR. LSORS
      LQUERY = LDWORK.EQ.-1
C
      IF( N.LE.1 ) THEN
         MINWRK = 1
      ELSE
         MINWRK = 4*N + 16
      END IF
C
      IF(      .NOT.WANTX .AND. .NOT.LSAME( JOBX, 'N' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.WANTY .AND. .NOT.LSAME( JOBY, 'N' ) ) THEN
         INFO = -2
      ELSE IF( .NOT.LSORN .AND. .NOT.LSORT .AND.
     $         .NOT.LSAME( SORT, 'C' ) ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( PMAX.LT.ONE ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE IF( LDX.LT.1 .OR. ( WANTX .AND. LDX.LT.N ) ) THEN
         INFO = -11
      ELSE IF( LDY.LT.1 .OR. ( WANTY .AND. LDY.LT.N ) ) THEN
         INFO = -13
      ELSE
         IF( LQUERY ) THEN
            CALL DTGEXC( WANTX, WANTY, N, A, LDA, B, LDB, X, LDX, Y,
     $                   LDY, 1, N, DWORK, -1, IERR )
            MAXWRK = DWORK(1)
            RETURN
         ELSE
            IF( LDWORK.LT.MINWRK ) THEN
               INFO = -22
               DWORK(1) = MINWRK
               IF( LDWORK.EQ.0 )
     $            RETURN
            ELSE
               MAXWRK = MINWRK
            END IF
         END IF
      END IF
C
      IF( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'MB04RD', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      NBLCKS = 0
      IF( N.EQ.0 ) THEN
         DWORK(1)  = ONE
         RETURN
      ELSE IF( N.EQ.1 ) THEN
         NBLCKS    = 1
         BLSIZE(1) = 1
         ALPHAR(1) = A(1,1)
         ALPHAI(1) = ZERO
         BETA(1)   = B(1,1)
         DWORK(1)  = ONE
         RETURN
      END IF
C
C     Set the "safe" minimum positive number with representable
C     reciprocal.
C
      EPS    = DLAMCH( 'Precision' )
      SAFEMN = DLAMCH( 'Safe minimum' )
      BIGNUM = ONE / SAFEMN
C
C     Set the scaling factor as an approximation of the expected
C     magnitude of eigenvalues. The matrix B is assumed nonzero.
C     Set also a tolerance for taking an eigenvalue as infinite after
C     it was perturbed into a finite one.
C
      NRMB  = DLANTR( 'Froben', 'Upper', 'NoDiag', N, N, B, LDB, DUM )
      SCALE = DLANHS( 'Froben', N, A, LDA, DUM ) / NRMB
      TOLB  = TEN*EPS*NRMB
C
C     Set the scaled eigenvalues of (A,B) and the tolerance for
C     reordering the eigenvalues in clusters, if needed.
C     The LAPACK routine DLAG2, called by MB03QV, assumes that the
C     2-by-2 pairs A(ii:ii+1,ii:ii+1) and B(ii:ii+1,ii:ii+1) have their
C     1-norms less than 1/SAFEMN, and the diagonal entries of B are at
C     least sqrt(SAFEMN).
C
      CALL MB03QV( N, A, LDA, B, LDB, ALPHAR, ALPHAI, BETA, IERR )
      CALL DSCAL(  N, SCALE, BETA, 1 )
C
      IF( LSORT ) THEN
         THRESH = ABS( TOL )
         IF( THRESH.EQ.ZERO ) THEN
C
C           Use the default tolerance in ordering the eigenvalues.
C
            THRESH = SQRT( SQRT( EPS ) )
         END IF
C
         IF( TOL.LE.ZERO ) THEN
            MXEV = ZERO
C
C           Use a relative tolerance.
C           Find max | lambda_j |, for finite lambda_j, j = 1 : N.
C
            DO 10 I = 1, N
               ABSA = DLAPY2( ALPHAR(I), ALPHAI(I) )
               BLR  = BETA(I)
               IF( BLR.GE.ONE ) THEN
                  MXEV = MAX( MXEV, ABSA / BLR )
               ELSE IF( ABSA.LT.BIGNUM*BLR ) THEN
                  MXEV = MAX( MXEV, ABSA / BLR )
               END IF
   10       CONTINUE
C
            IF( THRESH.LE.ONE ) THEN
               IF( MXEV.GE.ONE ) THEN
                  THRESH = THRESH*MXEV
               ELSE
                  THRESH = MAX( THRESH*MXEV, EPS )
               END IF
            ELSE IF( MXEV.LT.BIGNUM / THRESH ) THEN
               THRESH = THRESH*MXEV
            ELSE
               THRESH = BIGNUM
            END IF
         ELSE
            THRESH = THRESH / SCALE
         END IF
      END IF
C
C     Define the following submatrices of A:
C     A11, the DA11-by-DA11 block in position (L11,L11);
C     A22, the DA22-by-DA22 block in position (L22,L22);
C     A12, the DA11-by-DA22 block in position (L11,L22);
C     A21, the DA22-by-DA11 block in position (L22,L11) (null initially
C                                                        and finally).
C     Define similarly the submatrices of B, B11, B22, B12, B21.
C     The following loop uses L11 as loop variable and try to separate a
C     pair in position (L11,L11) of (A,B), with possibly clustered
C     eigenvalues, separated by the other eigenvalues (in the pair
C     (A22,B22)).
C
      L11 = 1
C
C     WHILE ( L11.LE.N ) DO
C
   20 CONTINUE
C
      IF( L11.LE.N ) THEN
         NBLCKS = NBLCKS + 1
         IF( ALPHAI(L11).GT.ZERO ) THEN
            DA11 = 2
         ELSE
            DA11 = 1
         END IF
         L22 = L11 + DA11
C
         IF( LSORT .AND. L11.LT.N ) THEN
C
            ELR = ALPHAR(L11)
            ELI = ALPHAI(L11)
            BLR =   BETA(L11)
C
C           The following loop, using K as loop variable, finds the
C           diagonal blocks whose eigenvalues are close to those of
C           (A11,B11) and moves these blocks (if any) to the leading
C           position of (A22,B22).
C
            K  = L22
            KF = L22
            IF( ALPHAI(K).GT.ZERO )
     $         KF = KF + 1
C
C           WHILE ( K.LE.N ) DO
   30       CONTINUE
C
            PINF = .FALSE.
            IF( K.LE.N ) THEN
               EKR = ALPHAR(K)
               EKI = ALPHAI(K)
               BKR =   BETA(K)
               IF( BKR.EQ.ZERO ) THEN
C
C                 Positive infinite eigenvalue.
C
                  PINF = A(K,K).GT.ZERO
               END IF
C
               CALL MA01DZ( ELR, ELI, BLR, EKR, EKI, BKR, EPS, SAFEMN,
     $                      C, DC, INFO )
               IF( INFO.EQ.1 ) THEN
C
C                 Error return: singular pencil.
C
                  RETURN
               END IF
C
               IF( DC.NE.ZERO .AND. C.LE.THRESH ) THEN
C
C                 A 1x1 or a 2x2 pair of blocks of (A22,B22) has been
C                 found so that
C
C                    abs( lambda_1 - lambda_k ) <= THRESH,
C
C                 where lambda_1 denotes an eigenvalue of (A11,B11).
C                 Move that pair of blocks to the leading position
C                 of (A22,B22).
C
C                 Workspace: need    4*N + 16, if N > 1; 1, otherwise;
C                            prefer  larger.
C
                  IF( K.GT.L22 ) THEN
                     CALL DTGEXC( WANTX, WANTY, N, A, LDA, B, LDB, X,
     $                            LDX, Y, LDY, K, L22, DWORK, LDWORK,
     $                            IERR )
C
                     IF( K.LT.N ) THEN
                        IF( A(K+1,K).NE.ZERO )
     $                     KF = K + 1
                     END IF
C
C                    Make B(I,I) >= 0, I = L22, ..., KF.
C                    Also, set the correct sign of an infinite
C                    eigenvalue.
C
                     IF( BKR.EQ.ZERO .AND. ABS( B(L22,L22) ).LT.TOLB )
     $                     THEN
                        B(L22,L22) = ZERO
                        IF( PINF .AND. A(L22,L22).LT.ZERO ) THEN
                           CALL DSCAL( L22-1, -ONE, B(1,L22), 1 )
                           CALL DSCAL(   L22, -ONE, A(1,L22), 1 )
                           IF( WANTY )
     $                        CALL DSCAL( N, -ONE, Y(1,L22), 1 )
                        END IF
                     END IF
C
                     DO 40 I = L22, KF
                        IF( B(I,I).LT.ZERO ) THEN
                           CALL DSCAL( I, -ONE, B(1,I), 1 )
                           CALL DSCAL( I, -ONE, A(1,I), 1 )
                           IF( I.LT.N ) THEN
                              IF( A(I+1,I).NE.ZERO )
     $                           A(I+1,I) = -A(I+1,I)
                           END IF
                           IF( WANTY )
     $                        CALL DSCAL( N, -ONE, Y(1,I), 1 )
                        END IF
   40                CONTINUE
C
                     CALL MB03QV( KF-L22+1, A(L22,L22), LDA, B(L22,L22),
     $                            LDB, ALPHAR(L22), ALPHAI(L22),
     $                            BETA(L22), IERR )
C
                     CALL DSCAL( KF-L22+1, SCALE, BETA(L22), 1 )
                  END IF
C
C                 Extend (A11,B11) with the leading 1-by-1 or 2-by-2
C                 pair of blocks of (A22,B22).
C
                  IF( ALPHAI(L22).GT.ZERO ) THEN
                     DA11 = DA11 + 2
                  ELSE
                     DA11 = DA11 + 1
                  END IF
                  L22 = L11 + DA11
               END IF
               IF( ALPHAI(K).GT.ZERO ) THEN
                  K = K + 2
               ELSE
                  K = K + 1
               END IF
               KF = K
               GO TO 30
            END IF
C           END WHILE 30
C
         END IF
C
C        The following loop uses L22 as loop variable and forms a
C        separable DA11-by-DA11 pair (A11,B11) in position (L11,L11).
C
C        WHILE ( L22.LE.N ) DO
C
   50    CONTINUE
C
         IF( L22.LE.N ) THEN
            L22M1 = L22 - 1
            DA22  = N - L22M1
C
C           Try to separate the pair (A11,B11) of order DA11 by using a
C           well-conditioned equivalence transformation.
C
C           First save A12' in the block A21, containing zeros only.
C           Similarly, save B12' in the block B21.
C
            CALL MA02AD( 'Full', DA11, DA22, A(L11,L22), LDA,
     $                   A(L22,L11), LDA )
            CALL MA02AD( 'Full', DA11, DA22, B(L11,L22), LDB,
     $                   B(L22,L11), LDB )
C
C           Solve the generalized Sylvester equation
C              A11*W - V*A22 = -sc*A12,
C              B11*W - V*B22 = -sc*B12.
C
C           Integer workspace:  need    N+6.
C
            CALL MB04RT( DA11, DA22, PMAX, A(L11,L11), LDA, A(L22,L22),
     $                   LDA, A(L11,L22), LDA, B(L11,L11), LDB,
     $                   B(L22,L22), LDB, B(L11,L22), LDB, SC, IWORK,
     $                   IERR )
C
            IF( IERR.GE.1 ) THEN
C
C              The annihilation of A12, B12 failed. Restore A12 and B12.
C
               CALL MA02AD( 'Full', DA22, DA11, A(L22,L11), LDA,
     $                      A(L11,L22), LDA )
               CALL DLASET( 'Full', DA22, DA11, ZERO, ZERO, A(L22,L11),
     $                      LDA )
               CALL MA02AD( 'Full', DA22, DA11, B(L22,L11), LDB,
     $                      B(L11,L22), LDB )
               CALL DLASET( 'Full', DA22, DA11, ZERO, ZERO, B(L22,L11),
     $                      LDB )
C
               GOON = ( L22.EQ.N .AND. DA11.EQ.1 ) .OR. L22.LT.N-1
               IF( ( LSORN .OR. LSORS ) .AND. GOON ) THEN
C
C                 Extend (A11,B11) with a 1-by-1 or 2-by-2 pair of
C                 blocks of (A22,B22) having the nearest eigenvalue to
C                 the mean of eigenvalues of (A11,B11), and resume the
C                 loop. First compute the mean.
C
                  AVR = ZERO
                  AVI = ZERO
C
                  DO 60 I = L11, L22M1
                     EIR = ALPHAR(I)
                     EII = ALPHAI(I)
                     BIR =   BETA(I)
                     IF( BIR.GE.ONE ) THEN
                        EIR = EIR / BIR
                        EII = EII / BIR
                        AVR = AVR + EIR
                        AVI = AVI + EII
                     ELSE IF( MAX( ABS( EIR ), ABS( EII ) ).LT.
     $                             BIGNUM*BIR ) THEN
                        EIR = EIR / BIR
                        EII = EII / BIR
                        AVR = AVR + EIR
                        AVI = AVI + EII
                     ELSE
                        AVR = SIGN( ONE, EIR )
                        AVI = ZERO
                        DAV = ZERO
                        GO TO 70
                     END IF
   60             CONTINUE
C
                  AVR = AVR / DA11
                  AVI = AVI / DA11
                  DAV = ONE
C
   70             CONTINUE
C
C                 Loop to find the eigenvalue(s) of (A22,B22) nearest to
C                 the above computed mean.
C
                  D = BIGNUM
                  K = L22
                  L = L22
C
C                 WHILE ( L.LE.N ) DO
   80             CONTINUE
C
                  PINF = .FALSE.
                  IF( L.LE.N ) THEN
                     ELR = ALPHAR(L)
                     ELI = ALPHAI(L)
                     BLR =   BETA(L)
                     IF( MAX( BLR, DAV ).EQ.ZERO ) THEN
                        D = ZERO
                        K = L
C
C                       Positive infinite eigenvalue.
C
                        PINF = A(L,L).GT.ZERO
                        GO TO 90
                     ELSE
                        CALL MA01DZ( ELR, ELI, BLR, AVR, AVI, DAV,
     $                               EPS, SAFEMN, C, DC, INFO )
                        IF( INFO.EQ.1 )
     $                     RETURN
                        IF( DC.NE.ZERO .AND. C.LT.D ) THEN
                           D = C
                           K = L
                        END IF
                        IF( ALPHAI(L).GT.ZERO ) THEN
                           L = L + 2
                        ELSE
                           L = L + 1
                        END IF
                     END IF
C
                     GO TO 80
                  END IF
C                 END WHILE 80
C
   90             CONTINUE
C
                  IF( ALPHAI(K).GT.ZERO ) THEN
                     KF = K + 1
                  ELSE
                     KF = K
                  END IF
C
               ELSE
C
C                 Extend (A11,B11) with a 1-by-1 or 2-by-2 pair of
C                 blocks of (A22,B22) having the nearest eigenvalues to
C                 the cluster of eigenvalues of (A11,B11), and resume
C                 the loop.
C
C                 Loop to find the eigenvalue(s) of (A22,B22) of minimum
C                 distance to the cluster of eigenvalues of (A11,B11).
C
                  D = BIGNUM
                  K = L22
                  L = L22
                  I = L22M1
C
                  EIR = ALPHAR(I)
                  EII = ALPHAI(I)
                  BIR =   BETA(I)
C
C                 WHILE ( L.LE.N ) DO
C
  100             CONTINUE
C
                  PINF = .FALSE.
                  IF( L.LE.N ) THEN
                     ELR = ALPHAR(L)
                     ELI = ALPHAI(L)
                     BLR =   BETA(L)
C
                     IF( MAX( BIR, BLR ).EQ.ZERO ) THEN
                        D = ZERO
                        K = L
C
C                       Positive infinite eigenvalue.
C
                        PINF = A(L,L).GT.ZERO
                        GO TO 110
                     ELSE
                        CALL MA01DZ( EIR, EII, BIR, ELR, ELI, BLR,
     $                               EPS, SAFEMN, C, DC, INFO )
                        IF( INFO.EQ.1 )
     $                     RETURN
                        IF( DC.NE.ZERO .AND. C.LT.D ) THEN
                           D = C
                           K = L
                        END IF
                        IF( ALPHAI(L).GT.ZERO ) THEN
                           L = L + 2
                        ELSE
                           L = L + 1
                        END IF
                     END IF
                     GO TO 100
                  END IF
C                 END WHILE 100
C
  110             CONTINUE
C
                  IF( ALPHAI(K).GT.ZERO ) THEN
                     KF = K + 1
                  ELSE
                     KF = K
                  END IF
C
               END IF
C
C              Try to move the 1-by-1 or 2-by-2 pair found to the
C              leading position of (A22,B22).
C
               IF( K.GT.L22 ) THEN
                  CALL DTGEXC( WANTX, WANTY, N, A, LDA, B, LDB, X, LDX,
     $                         Y, LDY, K, L22, DWORK, LDWORK, IERR )
C
                  IF( K.LT.N ) THEN
                     IF( A(K+1,K).NE.ZERO )
     $                  KF = K + 1
                  END IF
C
C                 Make B(I,I) >= 0, I = L22, ..., K.
C                 Also, set the correct sign of an infinite eigenvalue.
C
                  IF( BLR.EQ.ZERO .AND. ABS( B(L22,L22) ).LT.TOLB ) THEN
                     B(L22,L22) = ZERO
                     IF( PINF .AND. A(L22,L22).LT.ZERO ) THEN
                        CALL DSCAL( L22M1, -ONE, B(1,L22), 1 )
                        CALL DSCAL(   L22, -ONE, A(1,L22), 1 )
                        IF( WANTY )
     $                     CALL DSCAL( N, -ONE, Y(1,L22), 1 )
                     END IF
                  END IF
C
                  DO 120 I = L22, KF
                     IF( B(I,I).LT.ZERO ) THEN
                        CALL DSCAL( I, -ONE, B(1,I), 1 )
                        CALL DSCAL( I, -ONE, A(1,I), 1 )
                        IF( I.LT.N ) THEN
                           IF( A(I+1,I).NE.ZERO )
     $                        A(I+1,I) = -A(I+1,I)
                        END IF
                        IF( WANTY )
     $                     CALL DSCAL( N, -ONE, Y(1,I), 1 )
                     END IF
  120             CONTINUE
C
                  CALL MB03QV( KF-L22+1, A(L22,L22), LDA, B(L22,L22),
     $                         LDB, ALPHAR(L22), ALPHAI(L22), BETA(L22),
     $                         IERR )
C
                  CALL DSCAL( KF-L22+1, SCALE, BETA(L22), 1 )
C
               END IF
C
C              Extend (A11,B11) with the leading 1-by-1 block of
C              (A22,B22).
C
               IF( ALPHAI(L22).GT.ZERO ) THEN
                  DA11 = DA11 + 2
               ELSE
                  DA11 = DA11 + 1
               END IF
               L22 = L11 + DA11
               GO TO 50
            END IF
         END IF
C        END WHILE 50
C
         IF( L22.LE.N ) THEN
C
C           Accumulate the transformation in X and/or Y.
C           Only rows L11, ..., L22-1 in X, and columns L22, ..., N
C           in Y, are modified.
C           Also, scale to unity the (non-zero) columns of Y which will
C           be no more modified and transform A11 and B11 accordingly.
C           Scaling to unity the columns of X is done at the end.
C
            IF( WANTX ) THEN
               CALL DGEMM( 'NoTran', 'Trans', N, DA11, DA22, ONE,
     $                     X(1,L22), LDX, B(L11,L22), LDB, ONE,
     $                     X(1,L11), LDX )
            END IF
C
            IF( WANTY ) THEN
               CALL DGEMM( 'NoTran', 'NoTran', N, DA22, DA11, -ONE,
     $                     Y(1,L11), LDY, A(L11,L22), LDA, ONE,
     $                     Y(1,L22), LDY )
C
               DO 130 I = L11, L22M1
                  SC = DNRM2( N, Y(1,I), 1 )
                  IF( ABS( SC - ONE ).GT.EPS .AND. SC.GT.SAFEMN ) THEN
                     SC = ONE / SC
                     CALL DSCAL( DA11, SC, A(L11,I), 1 )
                     CALL DSCAL( DA11, SC, B(L11,I), 1 )
                     CALL DSCAL(    N, SC,   Y(1,I), 1 )
                  END IF
  130          CONTINUE
C
            END IF
C
C           Set A12, A21, B12 and B21 to zero.
C
            CALL DLASET( 'Full', DA11, DA22, ZERO, ZERO, A(L11,L22),
     $                   LDA )
            CALL DLASET( 'Full', DA22, DA11, ZERO, ZERO, A(L22,L11),
     $                   LDA )
            CALL DLASET( 'Full', DA11, DA22, ZERO, ZERO, B(L11,L22),
     $                   LDB )
            CALL DLASET( 'Full', DA22, DA11, ZERO, ZERO, B(L22,L11),
     $                   LDB )
         END IF
C
C        Store the orders of the diagonal blocks in BLSIZE.
C
         BLSIZE(NBLCKS) = DA11
         L11 = L22
         GO TO 20
C
      END IF
C
C     END WHILE 20
C
      IF( WANTX ) THEN
C
C        Scale to unity the (non-zero) columns of X and update A and B.
C
         L11 = 1
C
         DO 150 L = 1, NBLCKS
            DA11 = BLSIZE(L)
            L22  = L11 + DA11
C
            DO 140 I = L11, L22 - 1
               SC = DNRM2( N, X(1,I), 1 )
               IF( ABS( SC - ONE ).GT.EPS .AND. SC.GT.SAFEMN ) THEN
                  SC = ONE / SC
                  CALL DSCAL( DA11, SC, A(I,L11), LDA )
                  CALL DSCAL( DA11, SC, B(I,L11), LDB )
                  CALL DSCAL(    N, SC,   X(1,I),   1 )
               END IF
  140       CONTINUE
C
            L11 = L22
  150    CONTINUE
C
      END IF
C
      IF( WANTY ) THEN
C
C        Scale to unity the remaining (non-zero) columns of Y and update
C        A and B.
C
         L11 = N - DA11 + 1
C
         DO 160 I = L11, N
            SC = DNRM2( N, Y(1,I), 1 )
            IF( ABS( SC - ONE ).GT.EPS .AND. SC.GT.SAFEMN ) THEN
               SC = ONE / SC
               CALL DSCAL( DA11, SC, A(L11,I), 1 )
               CALL DSCAL( DA11, SC, B(L11,I), 1 )
               CALL DSCAL(    N, SC,   Y(1,I), 1 )
            END IF
  160    CONTINUE
C
      END IF
C
C     Undo scaling of eigenvalues.
C
      CALL DSCAL( N, ONE / SCALE, BETA, 1 )
C
      DWORK(1) = MAXWRK
C
      RETURN
C *** Last line of MB04RD ***
      END
