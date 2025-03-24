      SUBROUTINE MB04RZ( JOBX, JOBY, SORT, N, PMAX, A, LDA, B, LDB, X,
     $                   LDX, Y, LDY, NBLCKS, BLSIZE, ALPHA, BETA, TOL,
     $                   IWORK, INFO )
C
C     PURPOSE
C
C     To reduce a complex matrix pair (A,B) in generalized complex
C     Schur form to a block-diagonal form using well-conditioned
C     non-unitary equivalence transformations. The condition numbers
C     of the left and right transformations used for the reduction
C     are roughly bounded by PMAX, where PMAX is a given value.
C     The transformations are optionally postmultiplied in the given
C     matrices X and Y. The generalized Schur form is optionally
C     ordered, so that clustered eigenvalues are grouped in the same
C     pair of blocks.
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
C             Specifies whether or not the diagonal elements of the
C             generalized Schur form are reordered, as follows:
C             = 'N':  The diagonal elements are not reordered;
C             = 'S':  The diagonal elements are reordered before each
C                     step of reduction, so that clustered eigenvalues
C                     appear in the same pair of blocks.
C             = 'C':  The diagonal elements are not reordered, but the
C                     "closest-neighbour" strategy is used instead of
C                     the standard "closest to the mean" strategy (see
C                     METHOD);
C             = 'B':  The diagonal elements are reordered before each
C                     step of reduction, and the "closest-neighbour"
C                     strategy is used (see METHOD).
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrices A, B, X and Y.  N >= 0.
C
C     PMAX    (input) DOUBLE PRECISION
C             An upper bound for the "absolute value" of the elements of
C             the individual transformations used for reduction
C             (see METHOD and FURTHER COMMENTS).  PMAX >= 1.0D0.
C
C     A       (input/output) COMPLEX*16 array, dimension (LDA,N)
C             On entry, the leading N-by-N upper triangular part of this
C             array must contain the upper triangular matrix A in the
C             generalized complex Schur form, as returned by the LAPACK
C             Library routine ZGGES. The strictly lower triangular part
C             is used as workspace.
C             On exit, the leading N-by-N upper triangular part of
C             this array contains the computed block-diagonal matrix,
C             corresponding to the given matrix A. The remaining part
C             is set to zero.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     B       (input/output) COMPLEX*16 array, dimension (LDB,N)
C             On entry, the leading N-by-N upper triangular part of this
C             array must contain the upper triangular matrix B in the
C             generalized complex Schur form, as returned by the LAPACK
C             Library routine ZGGES. The diagonal elements of B are real
C             non-negative, hence the imaginary parts of these elements
C             are zero. The strictly lower triangular part is used as
C             workspace. The matrix B is assumed nonzero.
C             On exit, the leading N-by-N upper triangular part of this
C             array contains the computed upper triangular block-
C             diagonal matrix, corresponding to the given matrix B. The
C             remaining part is set to zero. The diagonal elements of B
C             are real non-negative.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= MAX(1,N).
C
C     X       (input/output) COMPLEX*16 array, dimension (LDX,*)
C             On entry, if JOBX = 'U', the leading N-by-N part of this
C             array must contain a given matrix X, for instance the left
C             transformation matrix VSL returned by the LAPACK Library
C             routine ZGGES.
C             On exit, if JOBX = 'U', the leading N-by-N part of this
C             array contains the product of the given matrix X and the
C             left transformation matrix that reduced (A,B) to block-
C             diagonal form. The local transformation matrix is itself a
C             product of non-unitary equivalence transformations having
C             elements with magnitude less than or equal to PMAX.
C             If JOBX = 'N', this array is not referenced.
C
C     LDX     INTEGER
C             The leading dimension of the array X.
C             LDX >= 1,        if JOBX = 'N';
C             LDX >= MAX(1,N), if JOBX = 'U'.
C
C     Y       (input/output) COMPLEX*16 array, dimension (LDY,*)
C             On entry, if JOBY = 'U', the leading N-by-N part of this
C             array must contain a given matrix Y, for instance the
C             right transformation matrix VSR returned by the LAPACK
C             Library routine ZGGES.
C             On exit, if JOBY = 'U', the leading N-by-N part of this
C             array contains the product of the given matrix Y and the
C             right transformation matrix that reduced (A,B) to block-
C             diagonal form. The local transformation matrix is itself a
C             product of non-unitary equivalence transformations having
C             elements with magnitude less than or equal to PMAX.
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
C     ALPHA   (output) COMPLEX*16 array, dimension (N)
C     BETA    (output) COMPLEX*16 array, dimension (N)
C             If INFO = 0, then ALPHA(j)/BETA(j), j = 1, ..., N, are
C             the generalized eigenvalues of the matrix pair (A, B)
C             (the diagonals of the complex Schur form).
C             All BETA(j) are non-negative real numbers.
C             The quotients ALPHA(j)/BETA(j) may easily over- or
C             underflow, and BETA(j) may even be zero. Thus, the user
C             should avoid naively computing the ratio.
C             If A and B are obtained from general matrices using ZGGES,
C             ALPHA will be always less than and usually comparable with
C             norm(A) in magnitude, and BETA always less than and
C             usually comparable with norm(B).
C
C     Tolerances
C
C     TOL     DOUBLE PRECISION
C             If SORT = 'S' or 'B', the tolerance to be used in the
C             ordering of the diagonal elements of the upper triangular
C             matrix pair.
C             If the user sets TOL > 0, then the given value of TOL is
C             used as an absolute tolerance: an eigenvalue i and a
C             temporarily fixed eigenvalue 1 (represented by the first
C             element of the current trailing pair of submatrices to be
C             reduced) are considered to belong to the same cluster if
C             they satisfy the following "distance" condition
C
C               | lambda_1 - lambda_i | <= TOL.
C
C             If the user sets TOL < 0, then the given value of TOL is
C             used as a relative tolerance: an eigenvalue i and a
C             temporarily fixed eigenvalue 1 are considered to belong to
C             the same cluster if they satisfy, for finite lambda_j,
C             j = 1, ..., N,
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
C     IWORK   INTEGER array, dimension (N+2)
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
C     be the given matrix pair in generalized Schur form, where
C     initially A   and B   are the first diagonal elements.
C                11      11
C     An attempt is made to compute the transformation matrices X and Y
C     of the form
C
C            ( I   V )       ( I   W )
C        X = (       ),  Y = (       )                               (1)
C            ( 0   I )       ( 0   I )
C
C     (partitioned as A and B ), so that (' denotes conjugate-transpose)
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
C     to obtain V and W. When this attempt fails, an eigenvalue of
C     (A  , B  ) closest to the mean of those of (A  , B  ) is selected,
C       22   22                                    11   11
C     and moved by unitary equivalence transformations in the leading
C     position of (A  , B  ); the moved diagonal elements in A and B are
C                   22   22
C     then added to the blocks A   and B  , respectively, increasing
C                               11      11
C     their order by 1. Another attempt is made to compute suitable
C     transformation matrices X and Y with the new definitions of the
C
C     blocks A  , A  , B  , and B  . After successful transformation
C             11   22   11       22
C     matrices X and Y have been obtained, they postmultiply the current
C     transformation matrices (if JOBX = 'U' and/or JOBY = 'U') and the
C     whole procedure is repeated for the new blocks A   and B  .
C                                                     22      22
C
C     When SORT = 'S', the diagonal elements of the generalized Schur
C     form are reordered before each step of the reduction, so that each
C     cluster of generalized eigenvalues, defined as specified in the
C     definition of TOL, appears in adjacent elements. The elements for
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
C     The individual non-unitary transformation matrices used in the
C     reduction of A and B to a block-diagonal form have condition
C     numbers of the order PMAX. This does not guarantee that their
C     product is well-conditioned enough. The routine can be easily
C     modified to provide estimates for the condition numbers of the
C     clusters of generalized eigenvalues.
C     For efficiency reasons, the "absolute value" (or "magnitude") of
C     the complex elements x of the transformation matrices is computed
C     as |real(x)| + |imag(x)|.
C
C     CONTRIBUTOR
C
C     V. Sima, Oct. 2022.
C
C     REVISIONS
C
C     V. Sima, Nov. 2022, Dec. 2022, Feb. 2023, Mar. 2023, Apr. 2023.
C
C     KEYWORDS
C
C     Diagonalization, unitary transformation, Schur form, Sylvester
C     equation.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE, TEN
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0, TEN = 1.0D1 )
      COMPLEX*16        CZERO, CONE
      PARAMETER         ( CZERO = ( 0.0D+0, 0.0D+0 ),
     $                    CONE  = ( 1.0D+0, 0.0D+0 ) )
C     .. Scalar Arguments ..
      CHARACTER         JOBX, JOBY, SORT
      INTEGER           INFO, LDA, LDB, LDX, LDY, N, NBLCKS
      DOUBLE PRECISION  PMAX, TOL
C     .. Array Arguments ..
      INTEGER           BLSIZE(*), IWORK(*)
      COMPLEX*16        A(LDA,*), ALPHA(*), B(LDB,*), BETA(*), X(LDX,*),
     $                  Y(LDY,*)
C     .. Local Scalars ..
      LOGICAL           LSORN, LSORS, LSORT, WANTX, WANTY
      INTEGER           DA11, DA22, I, IERR, K, L, L11, L22, L22M1
      DOUBLE PRECISION  ABSA, ABSB, AVI, AVR, BIGNUM, BIR, BKR, BLR, C,
     $                  D, DAV, DC, EII, EIR, EKI, EKR, ELI, ELR, EPS,
     $                  MXEV, NRMB, SAFEMN, SC, SCALE, THRESH, TOLB
      COMPLEX*16        AV, EI, SC1, SC2
C     .. Local Arrays ..
      DOUBLE PRECISION  DUM(1)
C     .. External Functions ..
      LOGICAL           LSAME
      DOUBLE PRECISION  DLAMCH, DZNRM2, ZLANTR
      EXTERNAL          DLAMCH, DZNRM2, LSAME, ZLANTR
C     .. External Subroutines ..
      EXTERNAL          MA01DZ, MA02AZ, MB04RW, XERBLA, ZCOPY, ZDSCAL,
     $                  ZGEMM, ZLASET, ZSCAL, ZTGEXC
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, DBLE, DCONJG, DIMAG, MAX, SIGN, SQRT
C     .. Executable Statements ..
C
C     Test the input scalar arguments.
C
      INFO  = 0
      WANTX = LSAME( JOBX, 'U' )
      WANTY = LSAME( JOBY, 'U' )
      LSORN = LSAME( SORT, 'N' )
      LSORS = LSAME( SORT, 'S' )
      LSORT = LSAME( SORT, 'B' ) .OR. LSORS
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
      END IF
C
      IF( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'MB04RZ', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      NBLCKS = 0
      IF( N.EQ.0 ) THEN
         RETURN
      ELSE IF( N.EQ.1 ) THEN
         NBLCKS    = 1
         BLSIZE(1) = 1
         ALPHA(1)  = A(1,1)
         BETA(1)   = B(1,1)
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
      NRMB  = ZLANTR( 'Froben', 'Upper', 'NoDiag', N, N, B, LDB, DUM )
      SCALE = ZLANTR( 'Froben', 'Upper', 'NoDiag', N, N, A, LDA, DUM ) /
     $        NRMB
      TOLB  = TEN*EPS*NRMB
C
C     Set the scaled eigenvalues of (A,B) and the tolerance for
C     reordering the eigenvalues in clusters, if needed.
C
      CALL ZCOPY(  N, A, LDA+1, ALPHA, 1 )
      CALL ZCOPY(  N, B, LDB+1, BETA,  1 )
      CALL ZDSCAL( N, SCALE,    BETA,  1 )
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
               ABSA =  ABS( ALPHA(I) )
               BLR  = DBLE(  BETA(I) )
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
         DA11   = 1
         L22    = L11 + DA11
C
         IF( LSORT .AND. L11.LT.N ) THEN
C
            ELR =  DBLE( ALPHA(L11) )
            ELI = DIMAG( ALPHA(L11) )
            BLR =  DBLE(  BETA(L11) )
C
C           The following loop, using K as loop variable, finds the
C           generalized eigenvalues which are close to those of the pair
C           (A11,B11) and moves these eigenvalues (if any) to the
C           leading position of (A22,B22).
C
            DO 40 K = L22, N
               EKR =  DBLE( ALPHA(K) )
               EKI = DIMAG( ALPHA(K) )
               BKR =  DBLE(  BETA(K) )
C
               CALL MA01DZ( ELR, ELI, BLR, EKR, EKI, BKR, EPS, SAFEMN,
     $                      D, DC, INFO )
               IF( INFO.EQ.1 ) THEN
C
C                 Error return: singular pencil.
C
                  RETURN
               END IF
C
               IF( DC.NE.ZERO .AND. D.LE.THRESH ) THEN
C
C                 An eigenvalue lambda_k of (A22,B22) has been found so
C                 that
C
C                    abs( lambda_1 - lambda_k ) <= THRESH,
C
C                 where lambda_1 denotes an eigenvalue of (A11,B11).
C                 Move lambda_k to the leading position of (A22,B22).
C
                  IF( K.GT.L22 ) THEN
                     CALL ZTGEXC( WANTX, WANTY, N, A, LDA, B, LDB, X,
     $                            LDX, Y, LDY, K, L22, IERR )
C
C                    Make B(I,I) real non-negative, I = L22, ..., K.
C                    Also, set the correct sign of an infinite
C                    eigenvalue.
C
                     IF( BKR.EQ.ZERO .AND. ABS( B(L22,L22) ).LT.TOLB )
     $                  B(L22,L22) = ZERO
                     DO 30 I = L22, K
                        IF( DIMAG( B(I,I) ).NE.ZERO .OR.
     $                       DBLE( B(I,I) ).LT.ZERO ) THEN
                           ABSB = ABS( B(I,I) )
                           IF( ABSB.GT.SAFEMN ) THEN
                              SC2    = B(I,I) / ABSB
                              SC1    = DCONJG( SC2 )
                              B(I,I) = ABSB
                              CALL ZSCAL( N-I,   SC1, B(I,I+1), LDB )
                              CALL ZSCAL( N-I+1, SC1, A(I,I),   LDA )
                              IF( WANTX )
     $                           CALL ZSCAL( N, SC2, X(1,I), 1 )
                           ELSE
                              B(I,I) = CZERO
                           END IF
                        END IF
C
                        ALPHA(I) = A(I,I)
                        BETA(I)  = B(I,I) * SCALE
C
   30                CONTINUE
C
                  END IF
C
C                 Extend (A11,B11) with the leading 1-by-1 pair of
C                 blocks of (A22,B22).
C
                  DA11 = DA11 + 1
                  L22  = L11  + DA11
               END IF
   40       CONTINUE
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
C           First save A12.' in the block A21, containing zeros only.
C           Similarly, save B12.' in the block B21. (M.' denotes the
C           usual matrix transposition.)
C
            CALL MA02AZ( 'Transpose', 'Full', DA11, DA22, A(L11,L22),
     $                   LDA, A(L22,L11), LDA )
            CALL MA02AZ( 'Transpose', 'Full', DA11, DA22, B(L11,L22),
     $                   LDB, B(L22,L11), LDB )
C
C           Solve the generalized Sylvester equation
C              A11*W - V*A22 = -sc*A12,
C              B11*W - V*B22 = -sc*B12.
C
C           Integer workspace:  need    N+2.
C
            CALL MB04RW( DA11, DA22, PMAX, A(L11,L11), LDA, A(L22,L22),
     $                   LDA, A(L11,L22), LDA, B(L11,L11), LDB,
     $                   B(L22,L22), LDB, B(L11,L22), LDB, SC, IWORK,
     $                   IERR )
C
            IF( IERR.GE.1 ) THEN
C
C              The annihilation of A12, B12 failed. Restore A12 and B12.
C
               CALL MA02AZ( 'Transpose', 'Full', DA22, DA11, A(L22,L11),
     $                      LDA, A(L11,L22), LDA )
               CALL ZLASET( 'Full', DA22, DA11, CZERO, CZERO,
     $                      A(L22,L11), LDA )
               CALL MA02AZ( 'Transpose', 'Full', DA22, DA11, B(L22,L11),
     $                      LDB, B(L11,L22), LDB )
               CALL ZLASET( 'Full', DA22, DA11, CZERO, CZERO,
     $                      B(L22,L11), LDB )
C
               IF( LSORN .OR. LSORS ) THEN
C
C                 Extend (A11,B11) with a 1-by-1 pair of (A22,B22)
C                 having the nearest eigenvalue to the mean of
C                 eigenvalues of (A11,B11), and resume the loop.
C                 First compute the mean of eigenvalues of (A11,B11).
C
                  AV = CZERO
C
                  DO 60 I = L11, L22M1
                     EI  = ALPHA(I)
                     BIR = DBLE( BETA(I) )
                     IF( BIR.GE.ONE ) THEN
                        EI = EI / BIR
                        AV = AV + EI
                     ELSE IF( MAX( ABS( DBLE( EI ) ), ABS( DIMAG( EI ) )
     $                           ).LT.BIGNUM*BIR ) THEN
                        EI = EI / BIR
                        AV = AV + EI
                     ELSE
                        AVR = SIGN( ONE, DBLE( EI ) )
                        AVI = ZERO
                        DAV = ZERO
                        GO TO 70
                     END IF
   60             CONTINUE
C
                  AVR =  DBLE( AV ) / DA11
                  AVI = DIMAG( AV ) / DA11
                  DAV = ONE
C
   70             CONTINUE
C
C                 Loop to find the eigenvalue of (A22,B22) nearest to
C                 the above computed mean.
C
                  D = BIGNUM
                  K = L22
C
                  DO 80 L = L22, N
                     ELR =  DBLE( ALPHA(L) )
                     ELI = DIMAG( ALPHA(L) )
                     BLR =  DBLE(  BETA(L) )
C
                     IF( MAX( BLR, DAV ).EQ.ZERO ) THEN
                        D = ZERO
                        K = L
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
                     END IF
   80             CONTINUE
C
   90             CONTINUE
C
               ELSE
C
C                 Extend (A11,B11) with a 1-by-1 pair of (A22,B22)
C                 corresponding to the eigenvalue nearest to the cluster
C                 of eigenvalues of (A11,B11), and resume the loop.
C
C                 Loop to find the eigenvalue of (A22,B22) of minimum
C                 distance to the cluster of eigenvalues of (A11,B11).
C
                  D = BIGNUM
                  I = L22M1
                  K = L22
C
                  EIR =  DBLE( ALPHA(I) )
                  EII = DIMAG( ALPHA(I) )
                  BIR =  DBLE(  BETA(I) )
C
                  DO 100 L = L22, N
                     ELR =  DBLE( ALPHA(L) )
                     ELI = DIMAG( ALPHA(L) )
                     BLR =  DBLE(  BETA(L) )
C
                     IF( MAX( BIR, BLR ).EQ.ZERO ) THEN
                        D = ZERO
                        K = L
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
                     END IF
  100             CONTINUE
C
  110             CONTINUE
C
               END IF
C
C              Try to move the 1-by-1 pair found to the leading position
C              of (A22,B22).
C
               IF( K.GT.L22 ) THEN
                  CALL ZTGEXC( WANTX, WANTY, N, A, LDA, B, LDB, X, LDX,
     $                         Y, LDY, K, L22, IERR )
C
C                 Make B(I,I) real non-negative, I = L22, ..., K.
C                    Also, set the correct sign of an infinite
C                    eigenvalue.
C
                  IF( BKR.EQ.ZERO .AND. ABS( B(L22,L22) ).LT.TOLB )
     $               B(L22,L22) = ZERO
C
                  DO 120 I = L22, K
                     IF( DIMAG( B(I,I) ).NE.ZERO .OR.
     $                    DBLE( B(I,I) ).LT.ZERO ) THEN
                        ABSB = ABS( B(I,I) )
                        IF( ABSB.GT.SAFEMN ) THEN
                           SC2    = B(I,I) / ABSB
                           SC1    = DCONJG( SC2 )
                           B(I,I) = ABSB
                           CALL ZSCAL( N-I,   SC1, B(I,I+1), LDB )
                           CALL ZSCAL( N-I+1, SC1, A(I,I),   LDA )
                           IF( WANTX )
     $                        CALL ZSCAL( N, SC2, X(1,I), 1 )
                        ELSE
                           B(I,I) = CZERO
                        END IF
                     END IF
C
                     ALPHA(I) = A(I,I)
                     BETA(I)  = B(I,I) * SCALE
C
  120             CONTINUE
C
               END IF
C
C              Extend (A11,B11) with the leading 1-by-1 block of
C              (A22,B22).
C
               DA11 = DA11 + 1
               L22  = L11  + DA11
               GO TO 50
            END IF
         END IF
C
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
               CALL ZGEMM( 'NoTran', 'CTrans', N, DA11, DA22, CONE,
     $                     X(1,L22), LDX, B(L11,L22), LDB, CONE,
     $                     X(1,L11), LDX )
            END IF
C
            IF( WANTY ) THEN
               CALL ZGEMM( 'NoTran', 'NoTran', N, DA22, DA11, -CONE,
     $                     Y(1,L11), LDY, A(L11,L22), LDA, CONE,
     $                     Y(1,L22), LDY )
C
               DO 130 I = L11, L22M1
                  SC = DZNRM2( N, Y(1,I), 1 )
                  IF( ABS( SC - ONE ).GT.EPS .AND. SC.GT.SAFEMN ) THEN
                     SC = ONE / SC
                     CALL ZDSCAL( DA11, SC, A(L11,I), 1 )
                     CALL ZDSCAL( DA11, SC, B(L11,I), 1 )
                     CALL ZDSCAL(    N, SC,   Y(1,I), 1 )
                  END IF
  130          CONTINUE
C
            END IF
C
C           Set A12, A21, B12 and B21 to zero.
C
            CALL ZLASET( 'Full', DA11, DA22, CZERO, CZERO, A(L11,L22),
     $                   LDA )
            CALL ZLASET( 'Full', DA22, DA11, CZERO, CZERO, A(L22,L11),
     $                   LDA )
            CALL ZLASET( 'Full', DA11, DA22, CZERO, CZERO, B(L11,L22),
     $                   LDB )
            CALL ZLASET( 'Full', DA22, DA11, CZERO, CZERO, B(L22,L11),
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
               SC = DZNRM2( N, X(1,I), 1 )
               IF( ABS( SC - ONE ).GT.EPS .AND. SC.GT.SAFEMN ) THEN
                  SC = ONE / SC
                  CALL ZDSCAL( DA11, SC, A(I,L11), LDA )
                  CALL ZDSCAL( DA11, SC, B(I,L11), LDB )
                  CALL ZDSCAL(    N, SC,   X(1,I),   1 )
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
            SC = DZNRM2( N, Y(1,I), 1 )
            IF( ABS( SC - ONE ).GT.EPS .AND. SC.GT.SAFEMN ) THEN
               SC = ONE / SC
               CALL ZDSCAL( DA11, SC, A(L11,I), 1 )
               CALL ZDSCAL( DA11, SC, B(L11,I), 1 )
               CALL ZDSCAL(    N, SC,   Y(1,I), 1 )
            END IF
  160    CONTINUE
C
      END IF
C
C     Undo scaling of eigenvalues.
C
      CALL ZDSCAL( N, ONE / SCALE, BETA, 1 )
C
      RETURN
C *** Last line of MB04RZ ***
      END
