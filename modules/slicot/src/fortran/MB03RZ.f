      SUBROUTINE MB03RZ( JOBX, SORT, N, PMAX, A, LDA, X, LDX, NBLCKS,
     $                   BLSIZE, W, TOL, INFO )
C     PURPOSE
C
C     To reduce an upper triangular complex matrix A (Schur form) to a
C     block-diagonal form using well-conditioned non-unitary similarity
C     transformations. The condition numbers of the transformations used
C     for reduction are roughly bounded by PMAX, where PMAX is a given
C     value. The transformations are optionally postmultiplied in a
C     given matrix X. The Schur form is optionally ordered, so that
C     clustered eigenvalues are grouped in the same block.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOBX    CHARACTER*1
C             Specifies whether or not the transformations are
C             accumulated, as follows:
C             = 'N':  The transformations are not accumulated;
C             = 'U':  The transformations are accumulated in X (the
C                     given matrix X is updated).
C
C     SORT    CHARACTER*1
C             Specifies whether or not the diagonal elements of the
C             Schur form are reordered, as follows:
C             = 'N':  The diagonal elements are not reordered;
C             = 'S':  The diagonal elements are reordered before each
C                     step of reduction, so that clustered eigenvalues
C                     appear in the same block;
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
C             The order of the matrices A and X.  N >= 0.
C
C     PMAX    (input) DOUBLE PRECISION
C             An upper bound for the absolute value of the elements of
C             the individual transformations used for reduction
C             (see METHOD). PMAX >= 1.0D0.
C
C     A       (input/output) COMPLEX*16 array, dimension (LDA,N)
C             On entry, the leading N-by-N upper triangular part of this
C             array must contain the upper triangular matrix A to be
C             block-diagonalized.
C             On exit, the leading N-by-N upper triangular part of this
C             array contains the computed block-diagonal matrix, in
C             Schur form.
C             The strictly lower triangular part is used as workspace,
C             but it is set to zero before exit.
C
C     LDA     INTEGER
C             The leading dimension of array A.  LDA >= MAX(1,N).
C
C     X       (input/output) COMPLEX*16 array, dimension (LDX,*)
C             On entry, if JOBX = 'U', the leading N-by-N part of this
C             array must contain a given matrix X.
C             On exit, if JOBX = 'U', the leading N-by-N part of this
C             array contains the product of the given matrix X and the
C             transformation matrix that reduced A to block-diagonal
C             form. The transformation matrix is itself a product of
C             non-unitary similarity transformations having elements
C             with magnitude less than or equal to PMAX.
C             If JOBX = 'N', this array is not referenced.
C
C     LDX     INTEGER
C             The leading dimension of array X.
C             LDX >= 1,        if JOBX = 'N';
C             LDX >= MAX(1,N), if JOBX = 'U'.
C
C     NBLCKS  (output) INTEGER
C             The number of diagonal blocks of the matrix A.
C
C     BLSIZE  (output) INTEGER array, dimension (N)
C             The first NBLCKS elements of this array contain the orders
C             of the resulting diagonal blocks of the matrix A.
C
C     W       (output) COMPLEX*16 array, dimension (N)
C             This array contains the eigenvalues of the matrix A.
C
C     Tolerances
C
C     TOL     DOUBLE PRECISION
C             The tolerance to be used in the ordering of the diagonal
C             elements of the upper triangular matrix.
C             If the user sets TOL > 0, then the given value of TOL is
C             used as an absolute tolerance: an eigenvalue i and a
C             temporarily fixed eigenvalue 1 (the first element of the
C             current trailing submatrix to be reduced) are considered
C             to belong to the same cluster if they satisfy
C
C               | lambda_1 - lambda_i | <= TOL.
C
C             If the user sets TOL < 0, then the given value of TOL is
C             used as a relative tolerance: an eigenvalue i and a
C             temporarily fixed eigenvalue 1 are considered to belong to
C             the same cluster if they satisfy, for j = 1, ..., N,
C
C               | lambda_1 - lambda_i | <= | TOL | * max | lambda_j |.
C
C             If the user sets TOL = 0, then an implicitly computed,
C             default tolerance, defined by TOL = SQRT( SQRT( EPS ) )
C             is used instead, as a relative tolerance, where EPS is
C             the machine precision (see LAPACK Library routine DLAMCH).
C             If SORT = 'N' or 'C', this parameter is not referenced.
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
C     Consider first that SORT = 'N'. Let
C
C            ( A    A   )
C            (  11   12 )
C        A = (          ),
C            ( 0    A   )
C            (       22 )
C
C     be the given matrix in Schur form, where initially A   is the
C                                                         11
C     first diagonal element. An attempt is made to compute a
C     transformation matrix X of the form
C
C            ( I   P )
C        X = (       )                                               (1)
C            ( 0   I )
C
C     (partitioned as A), so that
C
C                 ( A     0  )
C         -1      (  11      )
C        X  A X = (          ),
C                 ( 0    A   )
C                 (       22 )
C
C     and the elements of P do not exceed the value PMAX in magnitude.
C     An adaptation of the standard method for solving Sylvester
C     equations [1], which controls the magnitude of the individual
C     elements of the computed solution [2], is used to obtain matrix P.
C     When this attempt failed, a diagonal element of A  , closest to
C                                                      22
C     the mean of those of A   is selected, and moved by unitary
C                           11
C     similarity transformations in the leading position of A  ; the
C                                                            22
C     moved diagonal element is then added to the block A  , increasing
C                                                        11
C     its order by 1. Another attempt is made to compute a suitable
C     transformation matrix X with the new definitions of the blocks A
C                                                                     11
C     and A  . After a successful transformation matrix X has been
C          22
C     obtained, it postmultiplies the current transformation matrix
C     (if JOBX = 'U'), and the whole procedure is repeated for the
C     block A  .
C            22
C
C     When SORT = 'S', the diagonal elements of the Schur form are
C     reordered before each step of the reduction, so that each cluster
C     of eigenvalues, defined as specified in the definition of TOL,
C     appears in adjacent elements. The elements for each cluster are
C     merged together, and the procedure described above is applied to
C     the larger blocks. Using the option SORT = 'S' will usually
C     provide better efficiency than the standard option (SORT = 'N'),
C     proposed in [2], because there could be no or few unsuccessful
C     attempts to compute individual transformation matrices X of the
C     form (1). However, the resulting dimensions of the blocks are
C     usually larger; this could make subsequent calculations less
C     efficient.
C
C     When SORT = 'C' or 'B', the procedure is similar to that for
C     SORT = 'N' or 'S', respectively, but the block of A   whose
C                                                        22
C     eigenvalue(s) is (are) the closest to those of A   (not to their
C                                                     11
C     mean) is selected and moved to the leading position of A  . This
C                                                             22
C     is called the "closest-neighbour" strategy.
C
C     REFERENCES
C
C     [1] Bartels, R.H. and Stewart, G.W.  T
C         Solution of the matrix equation A X + XB = C.
C         Comm. A.C.M., 15, pp. 820-826, 1972.
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
C     possible in the worst case, when the matrix cannot be diagonalized
C     by well-conditioned transformations.
C
C     FURTHER COMMENTS
C
C     The individual non-unitary transformation matrices used in the
C     reduction of A to a block-diagonal form have condition numbers of
C     the order PMAX. This does not guarantee that their product is
C     well-conditioned enough. The routine can be easily modified to
C     provide estimates for the condition numbers of the clusters of
C     eigenvalues.
C
C     CONTRIBUTOR
C
C     V. Sima, June 2021.
C
C     REVISIONS
C
C     V. Sima, Feb. 2022.
C
C     KEYWORDS
C
C     Diagonalization, unitary transformation, Schur form, Sylvester
C     equation.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0 )
      COMPLEX*16        CZERO, CONE
      PARAMETER         ( CZERO = ( 0.0D+0, 0.0D+0 ),
     $                    CONE  = ( 1.0D+0, 0.0D+0 ) )
C     .. Scalar Arguments ..
      CHARACTER         JOBX, SORT
      INTEGER           INFO, LDA, LDX, N, NBLCKS
      DOUBLE PRECISION  PMAX, TOL
C     .. Array Arguments ..
      INTEGER           BLSIZE(*)
      COMPLEX*16        A(LDA,*), W(*), X(LDX,*)
C     .. Local Scalars ..
      LOGICAL           LJOBX, LSORN, LSORS, LSORT
      CHARACTER         JOBV
      INTEGER           DA11, DA22, I, IERR, J, K, L, L11, L22, L22M1
      DOUBLE PRECISION  BIGNUM, C, D, EDIF, SAFEMN, THRESH
      COMPLEX*16        AV, SC
C     .. External Functions ..
      INTEGER           IZAMAX
      LOGICAL           LSAME
      DOUBLE PRECISION  DLAMCH, DZNRM2
      EXTERNAL          DLAMCH, DZNRM2, IZAMAX, LSAME
C     .. External Subroutines ..
      EXTERNAL          DLABAD, MA02AZ, MB03RW, XERBLA, ZCOPY, ZGEMM,
     $                  ZLASET, ZSCAL, ZTREXC
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, DCMPLX, MAX, SQRT
C     .. Executable Statements ..
C
C     Test the input scalar arguments.
C
      INFO  = 0
      LJOBX = LSAME( JOBX, 'U' )
      LSORN = LSAME( SORT, 'N' )
      LSORS = LSAME( SORT, 'S' )
      LSORT = LSAME( SORT, 'B' ) .OR. LSORS
      IF( .NOT.LJOBX .AND. .NOT.LSAME( JOBX, 'N' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.LSORN .AND. .NOT.LSORT .AND.
     $         .NOT.LSAME( SORT, 'C' ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( PMAX.LT.ONE ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF( ( LDX.LT.1 ) .OR. ( LJOBX .AND. LDX.LT.N ) ) THEN
         INFO = -8
      END IF
C
      IF( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'MB03RZ', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      NBLCKS = 0
      IF( N.EQ.0 )
     $   RETURN
C
C     Set the "safe" minimum positive number with representable
C     reciprocal, and set JOBV parameter for ZTREXC routine.
C
      SAFEMN = DLAMCH( 'Safe minimum' )
      BIGNUM = ONE / SAFEMN
      CALL DLABAD( SAFEMN, BIGNUM )
      SAFEMN = SAFEMN / DLAMCH( 'Precision' )
      JOBV   = JOBX
      IF( LJOBX )
     $   JOBV = 'V'
C
C     Set the eigenvalues of A and the tolerance for reordering the
C     eigenvalues in clusters, if needed.
C
      CALL ZCOPY( N, A, LDA+1, W, 1 )
C
      IF( LSORT ) THEN
         THRESH = ABS( TOL )
         IF( THRESH.EQ.ZERO ) THEN
C
C           Use the default tolerance in ordering the elements.
C
            THRESH = SQRT( SQRT( DLAMCH( 'Epsilon' ) ) )
         END IF
C
         IF( TOL.LE.ZERO ) THEN
C
C           Use a relative tolerance. Find max | lambda_j |, j = 1 : N.
C
            L = IZAMAX( N, W, 1 )
            THRESH = THRESH * ABS( W(L) )
         END IF
      END IF
C
C     Define the following submatrices of A:
C     A11, the DA11-by-DA11 block in position (L11,L11);
C     A22, the DA22-by-DA22 block in position (L22,L22);
C     A12, the DA11-by-DA22 block in position (L11,L22);
C     A21, the DA22-by-DA11 block in position (L22,L11) (null initially
C                                                        and finally).
C     The following loop uses L11 as loop variable and try to separate a
C     block in position (L11,L11), with possibly clustered eigenvalues,
C     separated by the other eigenvalues (in the block A22).
C
      L11 = 1
C
C     WHILE ( L11.LE.N ) DO
C
   10 CONTINUE
      IF( L11.LE.N ) THEN
         NBLCKS = NBLCKS + 1
         DA11   = 1
C
         IF( LSORT ) THEN
C
C           The following loop, using K as loop variable, finds the
C           diagonal elements which are close to those of A11 and moves
C           these elements (if any) to the leading position of A22.
C
            L22 = L11 + DA11
            K   = L22
C
C           WHILE ( K.LE.N ) DO
C
   20       CONTINUE
            IF( K.LE.N ) THEN
               EDIF = ABS( W(L11) - W(K) )
               IF( EDIF.LE.THRESH ) THEN
C
C                 A diagonal element of A22 has been found so that
C
C                    abs( lambda_1 - lambda_k ) <= THRESH
C
C                 where lambda_1 and lambda_k denote an eigenvalue of
C                 A11 and of the leading element in A22, respectively.
C                 Move that element to the leading position of A22.
C
                  IF( K.GT.L22 ) THEN
                     CALL ZTREXC( JOBV, N, A, LDA, X, LDX, K, L22, IERR)
                     CALL ZCOPY ( K-L22+1, A(L22,L22), LDA+1, W(L22), 1)
                  END IF
C
C                 Extend A11 with the leading element of A22.
C
                  DA11 = DA11 + 1
                  L22  = L11  + DA11
               END IF
               K = K + 1
               GO TO 20
            END IF
C
C           END WHILE 20
C
         END IF
C
C        The following loop uses L22 as loop variable and forms a
C        separable DA11-by-DA11 block A11 in position (L11,L11).
C
         L22   = L11 + DA11
         L22M1 = L22 - 1
C
C        WHILE ( L22.LE.N ) DO
C
   30    CONTINUE
         IF( L22.LE.N ) THEN
            DA22 = N - L22M1
C
C           Try to separate the block A11 of order DA11 by using a
C           well-conditioned similarity transformation.
C
C           First save A12' in the block A21, containing zeros only.
C
            CALL MA02AZ( 'Transpose', 'Full', DA11, DA22, A(L11,L22),
     $                   LDA, A(L22,L11), LDA )
C
C           Solve  -A11*P + P*A22 = A12.
C
            CALL MB03RW( DA11, DA22, PMAX, A(L11,L11), LDA, A(L22,L22),
     $                   LDA, A(L11,L22), LDA, IERR )
C
            IF( IERR.EQ.1 ) THEN
C
C              The annihilation of A12 failed. Restore A12 and A21.
C
               CALL MA02AZ( 'Transpose', 'Full', DA22, DA11, A(L22,L11),
     $                      LDA, A(L11,L22), LDA )
               CALL ZLASET( 'Full', DA22, DA11, CZERO, CZERO,
     $                      A(L22,L11), LDA )
C
               IF( LSORN .OR. LSORS ) THEN
C
C                 Extend A11 with an element of A22 having the nearest
C                 eigenvalues to the mean of eigenvalues of A11 and
C                 resume the loop.
C                 First compute the mean of eigenvalues of A11.
C
                  AV = CZERO
C
                  DO 40 I = L11, L22M1
                     AV = AV + W(I)
   40             CONTINUE
C
                  AV = AV/DA11
C
C                 Loop to find the eigenvalue of A22 nearest to the
C                 above computed mean.
C
                  D = ABS( AV - W(L22) )
                  K = L22
                  L = L22 + 1
C
C                 WHILE ( L.LE.N ) DO
C
   50             CONTINUE
                  IF( L.LE.N ) THEN
                     C = ABS( AV - W(L) )
                     IF( C.LT.D ) THEN
                        D = C
                        K = L
                     END IF
                     L = L + 1
                     GO TO 50
                  END IF
C
C                 END WHILE 50
C
               ELSE
C
C                 Extend A11 with an element of A22 having the nearest
C                 eigenvalues to the cluster of eigenvalues of A11 and
C                 resume the loop.
C
C                 Loop to find the eigenvalue of A22 of minimum distance
C                 to the cluster.
C
                  D = BIGNUM
                  L = L22
                  K = L22
C
C                 WHILE ( L.LE.N ) DO
C
   60             CONTINUE
                  IF( L.LE.N ) THEN
                     I = L11
C
C                    WHILE ( I.LE.L22M1 ) DO
C
   70                CONTINUE
                     IF( I.LE.L22M1 ) THEN
                        C = ABS( W(I) - W(L) )
                        IF( C.LT.D ) THEN
                           D = C
                           K = L
                        END IF
                        I = I + 1
                        GO TO 70
                     END IF
C
C                    END WHILE 70
C
                     L = L + 1
                     GO TO 60
                  END IF
C
C                 END WHILE 60
C
               END IF
C
C              Try to move element found to the leading position of A22.
C
               IF( K.GT.L22 ) THEN
                  CALL ZTREXC( JOBV, N, A, LDA, X, LDX, K, L22, IERR )
                  CALL ZCOPY ( K-L22+1, A(L22,L22), LDA+1, W(L22), 1 )
               END IF
C
C              Extend A11 with the leading element of A22.
C
               DA11  = DA11 + 1
               L22   = L11  + DA11
               L22M1 = L22  - 1
               GO TO 30
            END IF
         END IF
C
C        END WHILE 30
C
         IF( LJOBX ) THEN
C
C           Accumulate the transformation in X.
C           Only columns L22, ..., N are modified.
C
            IF( L22.LE.N )
     $         CALL ZGEMM( 'No transpose', 'No transpose', N, DA22,
     $                     DA11, CONE, X(1,L11), LDX, A(L11,L22), LDA,
     $                     CONE, X(1,L22), LDX )
C
C           Scale to unity the (non-zero) columns of X which will be
C           no more modified and transform A11 accordingly.
C
            DO 80 J = L11, L22M1
               C  = DZNRM2( N, X(1,J), 1 )
               SC = DCMPLX( C, ZERO )
               IF( C.GT.SAFEMN ) THEN
                  CALL ZSCAL( DA11, SC, A(J,L11), LDA )
                  SC = CONE/SC
                  CALL ZSCAL( N, SC, X(1,J), 1 )
                  CALL ZSCAL( DA11, SC, A(L11,J), 1 )
               END IF
   80       CONTINUE
C
         END IF
C
         IF( L22.LE.N ) THEN
C
C           Set A12 and A21 to zero.
C
            CALL ZLASET( 'Full', DA11, DA22, CZERO, CZERO, A(L11,L22),
     $                   LDA )
            CALL ZLASET( 'Full', DA22, DA11, CZERO, CZERO, A(L22,L11),
     $                   LDA )
         END IF
C
C        Store the orders of the diagonal blocks in BLSIZE.
C
         BLSIZE(NBLCKS) = DA11
         L11 = L22
         GO TO 10
      END IF
C
C     END WHILE 10
C
      RETURN
C *** Last line of MB03RZ ***
      END
