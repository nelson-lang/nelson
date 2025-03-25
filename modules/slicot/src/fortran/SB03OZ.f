      SUBROUTINE SB03OZ( DICO, FACT, TRANS, N, M, A, LDA, Q, LDQ, B,
     $                   LDB, SCALE, W, DWORK, ZWORK, LZWORK, INFO )
C
C     PURPOSE
C                           H
C     To solve for X = op(U) *op(U) either the stable non-negative
C     definite continuous-time Lyapunov equation
C             H                     2      H
C        op(A) *X + X*op(A) = -scale *op(B) *op(B),                  (1)
C
C     or the convergent non-negative definite discrete-time Lyapunov
C     equation
C             H                     2      H
C        op(A) *X*op(A) - X = -scale *op(B) *op(B),                  (2)
C
C     where op(K) = K or K**H  (i.e., the conjugate transpose of the
C     matrix K), A is an N-by-N matrix, op(B) is an M-by-N matrix, U is
C     an upper triangular matrix containing the Cholesky factor of the
C     solution matrix X, and scale is an output scale factor, set less
C     than or equal to 1 to avoid overflow in X. If matrix B has full
C     rank, then the solution matrix X will be positive definite and
C     hence the Cholesky factor U will be nonsingular, but if B is rank
C     deficient, then X may be only positive semi-definite and U will be
C     singular.
C
C     In the case of equation (1) the matrix A must be stable (that is,
C     all the eigenvalues of A must have negative real parts), and for
C     equation (2) the matrix A must be convergent (that is, all the
C     eigenvalues of A must lie inside the unit circle).
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     DICO    CHARACTER*1
C             Specifies the type of Lyapunov equation to be solved, as
C             follows:
C             = 'C':  Equation (1), continuous-time case;
C             = 'D':  Equation (2), discrete-time case.
C
C     FACT    CHARACTER*1
C             Specifies whether or not the Schur factorization of the
C             matrix A is supplied on entry, as follows:
C             = 'F':  On entry, A and Q contain the factors from the
C                     Schur factorization of the matrix A;
C             = 'N':  The Schur factorization of A will be computed
C                     and the factors will be stored in A and Q.
C
C     TRANS   CHARACTER*1
C             Specifies the form of op(K) to be used, as follows:
C             = 'N':  op(K) = K    (No transpose);
C             = 'C':  op(K) = K**H (Conjugate transpose).
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrix A and the number of columns of
C             the matrix op(B).  N >= 0.
C
C     M       (input) INTEGER
C             The number of rows of the matrix op(B).  M >= 0.
C             If M = 0, A is unchanged on exit, and Q and W are not set.
C
C     A       (input/output) COMPLEX*16 array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the matrix A. If FACT = 'F', then A contains
C             an upper triangular matrix S in Schur form; the elements
C             below the diagonal of the array A are then not referenced.
C             On exit, the leading N-by-N upper triangular part of this
C             array contains the upper triangle of the matrix S.
C             The contents of the array A is not modified if FACT = 'F'.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     Q       (input or output) COMPLEX*16 array, dimension (LDQ,N)
C             On entry, if FACT = 'F', then the leading N-by-N part of
C             this array must contain the unitary matrix Q of the Schur
C             factorization of A.
C             Otherwise, Q need not be set on entry.
C             On exit, the leading N-by-N part of this array contains
C             the unitary matrix Q of the Schur factorization of A.
C             The contents of the array Q is not modified if FACT = 'F'.
C
C     LDQ     INTEGER
C             The leading dimension of the array Q.  LDQ >= MAX(1,N).
C
C     B       (input/output) COMPLEX*16 array, dimension (LDB,N)
C             if TRANS = 'N', and dimension (LDB,max(M,N)), if
C             TRANS = 'C'.
C             On entry, if TRANS = 'N', the leading M-by-N part of this
C             array must contain the coefficient matrix B of the
C             equation.
C             On entry, if TRANS = 'C', the leading N-by-M part of this
C             array must contain the coefficient matrix B of the
C             equation.
C             On exit, the leading N-by-N part of this array contains
C             the upper triangular Cholesky factor U of the solution
C             matrix X of the problem, X = op(U)**H * op(U).
C             If M = 0 and N > 0, then U is set to zero.
C
C     LDB     INTEGER
C             The leading dimension of the array B.
C             LDB >= MAX(1,N,M), if TRANS = 'N';
C             LDB >= MAX(1,N),   if TRANS = 'C'.
C
C     SCALE   (output) DOUBLE PRECISION
C             The scale factor, scale, set less than or equal to 1 to
C             prevent the solution overflowing.
C
C     W       (output) COMPLEX*16 array, dimension (N)
C             If INFO >= 0 and INFO <= 3, W contains the eigenvalues of
C             the matrix A.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (N)
C
C     ZWORK   COMPLEX*16 array, dimension (LZWORK)
C             On exit, if INFO = 0 or INFO = 1, ZWORK(1) returns the
C             optimal value of LZWORK.
C             On exit, if INFO = -16, ZWORK(1) returns the minimum value
C             of LZWORK.
C
C     LZWORK  INTEGER
C             The length of the array ZWORK.
C             If M > 0, LZWORK >= MAX(1,2*N+MAX(MIN(N,M)-2,0));
C             If M = 0, LZWORK >= 1.
C             For optimum performance LZWORK should sometimes be larger.
C
C             If LZWORK = -1, then a workspace query is assumed; the
C             routine only calculates the optimal size of the ZWORK
C             array, returns this value as the first entry of the ZWORK
C             array, and no error message related to LZWORK is issued by
C             XERBLA.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value;
C             = 1:  if the Lyapunov equation is (nearly) singular
C                   (warning indicator);
C                   if DICO = 'C' this means that while the matrix A
C                   (or the factor S) has computed eigenvalues with
C                   negative real parts, it is only just stable in the
C                   sense that small perturbations in A can make one or
C                   more of the eigenvalues have a non-negative real
C                   part;
C                   if DICO = 'D' this means that while the matrix A
C                   (or the factor S) has computed eigenvalues inside
C                   the unit circle, it is nevertheless only just
C                   convergent, in the sense that small perturbations
C                   in A can make one or more of the eigenvalues lie
C                   outside the unit circle;
C                   perturbed values were used to solve the equation;
C             = 2:  if FACT = 'N' and DICO = 'C', but the matrix A is
C                   not stable (that is, one or more of the eigenvalues
C                   of A has a non-negative real part), or DICO = 'D',
C                   but the matrix A is not convergent (that is, one or
C                   more of the eigenvalues of A lies outside the unit
C                   circle); however, A will still have been factored
C                   and the eigenvalues of A returned in W;
C             = 3:  if FACT = 'F' and DICO = 'C', but the Schur factor S
C                   supplied in the array A is not stable (that is, one
C                   or more of the eigenvalues of S has a non-negative
C                   real part), or DICO = 'D', but the Schur factor S
C                   supplied in the array A is not convergent (that is,
C                   one or more of the eigenvalues of S lies outside the
C                   unit circle); the eigenvalues of A are still
C                   returned in W;
C             = 6:  if FACT = 'N' and the LAPACK Library routine ZGEES
C                   has failed to converge. This failure is not likely
C                   to occur. The matrix B will be unaltered but A will
C                   be destroyed.
C
C     METHOD
C
C     The method used by the routine is based on the Bartels and Stewart
C     method [1], except that it finds the upper triangular matrix U
C     directly without first finding X and without the need to form the
C     normal matrix op(B)**H * op(B).
C
C     The Schur factorization of a square matrix A is given by
C               H
C        A = QSQ ,
C
C     where Q is unitary and S is an N-by-N upper triangular matrix.
C     If A has already been factored prior to calling the routine, then
C     the factors Q and S may be supplied and the initial factorization
C     omitted.
C
C     If TRANS = 'N' and 6*M > 7*N, the matrix B is factored as
C     (QR factorization)
C            _   _
C        B = P ( R ), 
C              ( 0 )
C           _                                 _
C     where P is an M-by-M unitary matrix and R is a square upper
C                                         _   _
C     triangular matrix. Then, the matrix B = RQ is factored as
C        _
C        B = PR.
C
C     If TRANS = 'N' and 6*M <= 7*N, the matrix BQ is factored as
C
C        BQ = P ( R ),   M >= N,   BQ = P ( R  Z ),   M < N.
C               ( 0 ) 
C
C     If TRANS = 'C' and 6*M > 7*N, the matrix B is factored as
C     (RQ factorization)
C                 _   _
C        B = ( 0  R ) P,
C           _                                 _
C     where P is an M-by-M unitary matrix and R is a square upper
C                                         _    H _
C     triangular matrix. Then, the matrix B = Q  R is factored as
C        _
C        B = RP.
C                                                H
C     If TRANS = 'C' and 6*M <= 7*N, the matrix Q  B is factored as
C
C         H                              H     ( Z )
C        Q  B = ( 0  R ) P,   M >= N,   Q  B = (   ) P,   M < N.
C                                              ( R )
C
C     These factorizations are utilised to either transform the
C     continuous-time Lyapunov equation to the canonical form
C            H      H              H                     2      H
C       op(S) *op(V) *op(V) + op(V) *op(V)*op(S) = -scale *op(F) *op(F),
C
C     or the discrete-time Lyapunov equation to the canonical form
C            H      H                    H               2      H
C       op(S) *op(V) *op(V)*op(S) - op(V) *op(V) = -scale *op(F) *op(F),
C
C     where V and F are upper triangular, and
C
C        F = R,  M >= N,   F = ( R  Z ),  M < N,  if TRANS = 'N';
C                              ( 0  0 )
C
C        F = R,  M >= N,   F = ( 0  Z ),  M < N,  if TRANS = 'C'.
C                              ( 0  R )
C
C     The transformed equation is then solved for V, from which U is
C     obtained via the QR factorization of V*Q**H, if TRANS = 'N', or
C     via the RQ factorization of Q*V, if TRANS = 'C'.
C
C     REFERENCES
C
C     [1] Bartels, R.H. and Stewart, G.W.
C         Solution of the matrix equation  A'X + XB = C.
C         Comm. A.C.M., 15, pp. 820-826, 1972.
C
C     [2] Hammarling, S.J.
C         Numerical solution of the stable, non-negative definite
C         Lyapunov equation.
C         IMA J. Num. Anal., 2, pp. 303-325, 1982.
C
C     NUMERICAL ASPECTS
C                               3
C     The algorithm requires 0(N ) operations and is backward stable.
C
C     FURTHER COMMENTS
C
C     The Lyapunov equation may be very ill-conditioned. In particular,
C     if A is only just stable (or convergent) then the Lyapunov
C     equation will be ill-conditioned.  A symptom of ill-conditioning
C     is "large" elements in U relative to those of A and B, or a
C     "small" value for scale.
C
C     SB03OZ routine can be also used for solving "unstable" Lyapunov
C     equations, i.e., when matrix A has all eigenvalues with positive
C     real parts, if DICO = 'C', or with moduli greater than one,
C     if DICO = 'D'. Specifically, one may solve for X = op(U)**H*op(U)
C     either the continuous-time Lyapunov equation
C             H                    2      H
C        op(A) *X + X*op(A) = scale *op(B) *op(B),                   (3)
C
C     or the discrete-time Lyapunov equation
C             H                    2      H
C        op(A) *X*op(A) - X = scale *op(B) *op(B),                   (4)
C
C     provided, for equation (3), the given matrix A is replaced by -A,
C     or, for equation (4), the given matrices A and B are replaced by
C     inv(A) and B*inv(A), if TRANS = 'N' (or inv(A)*B, if TRANS = 'C'),
C     respectively. Although the inversion generally can rise numerical
C     problems, in case of equation (4) it is expected that the matrix A
C     is enough well-conditioned, having only eigenvalues with moduli
C     greater than 1.
C
C     CONTRIBUTOR
C
C     V. Sima, March 2022.
C
C     REVISIONS
C
C     V. Sima, April 2022, Apr. 2023.
C
C     KEYWORDS
C
C     Lyapunov equation, unitary transformation, Schur form, Sylvester
C     equation.
C
C     ******************************************************************
C
C     .. Parameters ..
      COMPLEX*16        CZERO, CONE
      PARAMETER         ( CZERO = ( 0.0D0, 0.0D0 ),
     $                    CONE  = ( 1.0D0, 0.0D0 ) )
      DOUBLE PRECISION  ZERO, ONE, P95
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0, P95 = 0.95D0 )
C     .. Scalar Arguments ..
      CHARACTER         DICO, FACT, TRANS
      INTEGER           INFO, LDA, LDB, LDQ, LZWORK, M, N
      DOUBLE PRECISION  SCALE
C     .. Array Arguments ..
      COMPLEX*16        A(LDA,*), B(LDB,*), Q(LDQ,*), W(*), ZWORK(*)
      DOUBLE PRECISION  DWORK(*)
C     .. Local Scalars ..
      DOUBLE PRECISION  BIGNMS, BIGNUM, EMAX, EPS, MA, MATO, MB, MBTO,
     $                  MN, MX, SAFMIN, SMLNUM, T, TMP
      INTEGER           BL, I, IFAIL, INFORM, ITAU, J, JWORK, K, L,
     $                  MAXMN, MINMN, MINWRK, NC, NM, NR, SDIM, WRKOPT
      LOGICAL           CONT, ISTRAN, LASCL, LBSCL, LQUERY, LSCL,
     $                  NOFACT, NUNITQ, SCALB, SMALLM
C     .. Local Arrays ..
      LOGICAL           BWORK(1)
C     .. External Functions ..
      LOGICAL           LSAME, MA02HZ, SELECT
      DOUBLE PRECISION  DLAMCH, ZLANGE, ZLANTR
      EXTERNAL          DLAMCH, LSAME, MA02HZ, SELECT, ZLANGE, ZLANTR
C     .. External Subroutines ..
      EXTERNAL          DLABAD, MB01UZ, SB03OS, XERBLA, ZCOPY, ZDSCAL,
     $                  ZGEES, ZGEMM, ZGEMV, ZGEQRF, ZGERQF, ZLACGV,
     $                  ZLACPY, ZLASCL, ZLASET, ZSWAP, ZTRMM
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, DBLE, INT, MAX, MIN, SQRT
C     .. Executable Statements ..
C
C     Test the input scalar arguments.
C
      CONT   = LSAME( DICO,  'C' )
      NOFACT = LSAME( FACT,  'N' )
      ISTRAN = LSAME( TRANS, 'C' )
      LQUERY = LZWORK.EQ.-1
      MINMN  = MIN( M, N )
      MAXMN  = MAX( M, N )
C
      INFO = 0
      IF ( .NOT.CONT .AND. .NOT.LSAME( DICO, 'D' ) ) THEN
         INFO = -1
      ELSE IF ( .NOT.NOFACT .AND. .NOT.LSAME( FACT, 'F' ) ) THEN
         INFO = -2
      ELSE IF ( .NOT.ISTRAN .AND. .NOT.LSAME( TRANS, 'N' ) ) THEN
         INFO = -3
      ELSE IF ( N.LT.0 ) THEN
         INFO = -4
      ELSE IF ( M.LT.0 ) THEN
         INFO = -5
      ELSE IF ( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF ( LDQ.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE IF ( ( ISTRAN .AND. ( LDB.LT.MAX( 1, N ) ) ) .OR.
     $     ( .NOT.ISTRAN .AND. ( LDB.LT.MAX( 1, MAXMN ) ) ) ) THEN
         INFO = -11
      ELSE
         IF ( MINMN.EQ.0 ) THEN
            MINWRK = 1
         ELSE
            MINWRK = 2*N + MAX( MINMN - 2, 0 )
         END IF
         SMALLM = 6*M.LE.7*N
         IF ( LQUERY ) THEN
            IF ( NOFACT ) THEN
               CALL ZGEES( 'Vectors', 'Not ordered', SELECT, N, A, LDA,
     $                     SDIM, W, Q, LDQ, ZWORK, -1, DWORK, BWORK,
     $                     IFAIL )
               WRKOPT = MAX( MINWRK, INT( ZWORK(1) ) )
            ELSE
               WRKOPT = MINWRK
            END IF
            IF ( ISTRAN ) THEN
               CALL ZGERQF( N, MAXMN, B, LDB, ZWORK, ZWORK, -1, IFAIL )
            ELSE
               CALL ZGEQRF( MAXMN, N, B, LDB, ZWORK, ZWORK, -1, IFAIL )
            END IF
            WRKOPT = MAX( WRKOPT, INT( ZWORK(1) ) + N )
         ELSE IF ( LZWORK.LT.MINWRK ) THEN
            ZWORK(1) = MINWRK
            INFO = -16
         END IF
      END IF
C
      IF ( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'SB03OZ', -INFO )
         RETURN
      ELSE IF ( LQUERY ) THEN
         ZWORK(1) = WRKOPT
         RETURN
      END IF
C
      SCALE = ONE
C
C     Quick return if possible.
C
      IF ( ISTRAN ) THEN
         K = N
         L = M
      ELSE
         K = M
         L = N
      END IF
      MB = ZLANGE( 'Max', K, L, B, LDB, DWORK )
      IF ( MB.EQ.ZERO ) THEN
         IF ( N.GT.0 )
     $      CALL ZLASET( 'Full', N, N, CZERO, CZERO, B, LDB )
         ZWORK(1) = CONE
         RETURN
      END IF
C
C     Set constants to control overflow.
C
      EPS    = DLAMCH( 'Precision' )
      SAFMIN = DLAMCH( 'Safe minimum' )
      SMLNUM = SAFMIN
      BIGNMS = ONE/SMLNUM
      CALL DLABAD( SMLNUM, BIGNMS )
      SMLNUM = SQRT( SMLNUM )/EPS
      BIGNUM = ONE/SMLNUM
C
C     Start the solution.
C
C     (Note: Comments in the code beginning "Workspace:" describe the
C     minimal amount of real workspace needed at that point in the
C     code, as well as the preferred amount for good performance.
C     NB refers to the optimal block size for the immediately
C     following subroutine, as returned by ILAENV.)
C
      IF ( NOFACT ) THEN
C
C        Find the Schur factorization of A,   A = Q*S*Q'.
C        Workspace:  need   2*N;
C                    prefer larger.
C
         CALL ZGEES( 'Vectors', 'Not ordered', SELECT, N, A, LDA, SDIM,
     $               W, Q, LDQ, ZWORK, LZWORK, DWORK, BWORK, INFORM )
         IF ( INFORM.NE.0 ) THEN
            INFO = 6
            RETURN
         END IF
         WRKOPT = ZWORK(1)
      ELSE
C
C        Set the eigenvalues of the matrix A.
C
         CALL ZCOPY( N, A, LDA+1, W, 1 )
         WRKOPT = 0
      END IF
C
C     Check for identity matrix Q.
C
      NUNITQ = .NOT.MA02HZ( 'All', N, N, CONE, Q, LDQ )
C
C     Check the eigenvalues for stability.
C
      IF ( CONT ) THEN
         EMAX = DBLE( W(1) )
C
         DO 10 J = 2, N
            TMP = DBLE( W(J) )
            IF ( TMP.GT.EMAX )
     $         EMAX = TMP
   10    CONTINUE
C
      ELSE
         EMAX = ABS( W(1) )
C
         DO 20 J = 2, N
            TMP = ABS( W(J) )
            IF ( TMP.GT.EMAX )
     $         EMAX = TMP
   20    CONTINUE
C
      END IF
C
      IF (    ( CONT ) .AND. ( EMAX.GE.ZERO ) .OR.
     $   ( .NOT.CONT ) .AND. ( EMAX.GE.ONE  ) ) THEN
         IF ( NOFACT ) THEN
            INFO = 2
         ELSE
            INFO = 3
         END IF
         RETURN
      END IF
C
C     Scale A if the maximum absolute value of its elements is outside
C     the range [SMLNUM,BIGNUM]. Scale similarly B. Scaling of B is done
C     before further processing if the maximum absolute value of its
C     elements is greater than BIGNMS; otherwise, it is postponed.
C     For continuous-time equations, scaling is also performed if the
C     maximum absolute values of A and B differ too much, or their
C     minimum (maximum) is too large (small).
C
      MA = MIN( ZLANTR( 'Max', 'Upper', 'NoDiag', N, N, A, LDA, DWORK ),
     $          BIGNMS )
      MN = MIN( MA, MB )
      MX = MAX( MA, MB )
C
      IF ( CONT ) THEN
         LSCL = MN.LT.MX*SMLNUM .OR. MX.LT.SMLNUM .OR. MN.GT.BIGNUM
      ELSE
         LSCL = .FALSE.
      END IF
C
      IF ( LSCL ) THEN
         MATO  = ONE
         MBTO  = ONE
         LASCL = .TRUE.
         LBSCL = .TRUE.
      ELSE
         IF ( MA.GT.ZERO .AND. MA.LT.SMLNUM ) THEN
            MATO  = SMLNUM
            LASCL = .TRUE.
         ELSE IF ( MA.GT.BIGNUM ) THEN
            MATO  = BIGNUM
            LASCL = .TRUE.
         ELSE
            LASCL = .FALSE.
         END IF
C
         IF ( MB.GT.ZERO .AND. MB.LT.SMLNUM ) THEN
            MBTO  = SMLNUM
            LBSCL = .TRUE.
         ELSE IF ( MB.GT.BIGNUM ) THEN
            MBTO  = BIGNUM
            LBSCL = .TRUE.
         ELSE
            MBTO  = ONE
            LBSCL = .FALSE.
         END IF
      END IF
C
      IF ( .NOT.CONT .AND. MATO.EQ.ONE )
     $   MATO = P95
      IF ( LASCL )
     $   CALL ZLASCL( 'Upper', 0, 0, MA, MATO, N, N, A, LDA, INFO )
C
      SCALB = MB.GT.BIGNMS
      MB    = MIN( MB, BIGNMS )
      IF ( LBSCL .AND. SCALB )
     $   CALL ZLASCL( 'Gen', 0, 0, MB, MBTO, K, L, B, LDB, INFO )
C
C     Transformation of the right hand side, involving one or two RQ or
C     QR factorizations. Also, do scaling, if it was postponed.
C
C     Workspace:  need   MIN(M,N) + N;
C                 prefer MIN(M,N) + N*NB.
C
      ITAU  = 1
      JWORK = ITAU + MINMN
C
      IF ( ISTRAN ) THEN
         NM = M
         IF ( NUNITQ ) THEN
            IF ( SMALLM ) THEN
C                      _
C              Compute B := Q**H * B.
C
               NC = INT( LZWORK / N )
C
               DO 30 J = 1, M, NC
                  BL = MIN( M-J+1, NC )
                  CALL ZGEMM(  'CTrans', 'NoTran', N, BL, N, CONE, Q,
     $                         LDQ, B(1,J), LDB, CZERO, ZWORK, N )
                  CALL ZLACPY( 'All', N, BL, ZWORK, N, B(1,J), LDB )
   30          CONTINUE
C
            ELSE
C
C              If M > 7*N/6, perform the RQ factorization of B,
C                          _   _
C                 B = ( 0  R ) P.
C
               NM = N
               CALL ZGERQF( N, M, B, LDB, ZWORK(ITAU), ZWORK(JWORK),
     $                      LZWORK-JWORK+1, IFAIL )
               WRKOPT = MAX( WRKOPT, INT( ZWORK(JWORK) ) + JWORK - 1,
     $                       MINMN*N )
C
C              Form in B
C                 _     H _        _
C                 B := Q  R,  with B an N-by-MIN(M,N) matrix.
C              Use a BLAS 3 operation if enough workspace, and BLAS 2,
C                         _
C              otherwise: B is formed column by column.
C
               IF ( LZWORK.GE.MINMN*N ) THEN
                  J = 1
C
                  DO 40 I = 1, MINMN
                     CALL ZCOPY(  N, Q(N-MINMN+I,1), LDQ, ZWORK(J), 1 )
                     CALL ZLACGV( N, ZWORK(J), 1 )
                     J = J + N
   40             CONTINUE
C
                  CALL ZTRMM(  'Right', 'Upper', 'NoTran', 'NoUnit', N,
     $                         MINMN, CONE, B(N-MINMN+1,M-MINMN+1), LDB,
     $                         ZWORK, N )
                  CALL ZLACPY( 'Full', N, MINMN, ZWORK, N, B, LDB )
               ELSE
C
                  DO 50 J = 1, MINMN
                     CALL ZCOPY( J, B(1,M-MINMN+J), 1, ZWORK, 1 )
                     CALL ZGEMV( 'CTrans', J, N, CONE, Q, LDQ, ZWORK, 1,
     $                           CZERO, B(1,J), 1 )
   50             CONTINUE
C
               END IF
            END IF
         END IF
C                                        _
C        Perform the RQ factorization of B to get the factor F.
C        Note that if M <= 7*N/6, the factorization is
C           _                          _       H   H  H
C           B := ( 0  F ) P,  M >= N,  B := ( Z   F  )  P,  M < N.
C        Then, do scaling, if it was postponed.
C        Make the entries on the main diagonal are non-negative.
C
         CALL ZGERQF( N, NM, B, LDB, ZWORK(ITAU), ZWORK(JWORK),
     $                LZWORK-JWORK+1, IFAIL )
         IF ( N.GT.NM ) THEN
            IF ( LBSCL .AND. .NOT.SCALB ) THEN
               CALL ZLASCL( 'Gen',   0, 0, MB, MBTO, N-M, M, B, LDB,
     $                      INFO )
               CALL ZLASCL( 'Upper', 0, 0, MB, MBTO, M, M, B(N-M+1,1),
     $                      LDB, INFO )
            END IF
C   
            DO 60 I = M, 1, -1
               CALL ZCOPY( N-M+I, B(1,I), 1, B(1,N-M+I), 1 )
   60       CONTINUE
C
            CALL ZLASET( 'Full', N, N-M, CZERO, CZERO, B, LDB )
            IF ( M.GT.1 )
     $         CALL ZLASET( 'Lower', M-1, M-1, CZERO, CZERO,
     $                      B(N-M+2,N-M+1), LDB )
         ELSE
            IF ( M.GT.N .AND. M.EQ.NM )
     $         CALL ZLACPY( 'Upper', N, N, B(1,M-N+1), LDB, B, LDB )
            IF ( LBSCL .AND. .NOT.SCALB )
     $         CALL ZLASCL( 'Upper', 0, 0, MB, MBTO, N, N, B, LDB,
     $                      INFO )
         END IF
C
         DO 70 I = N - MINMN + 1, N
            IF ( DBLE( B(I,I) ).LT.ZERO )
     $         CALL ZDSCAL( I, -ONE, B(1,I), 1 )
   70    CONTINUE
C
      ELSE
C
         NM = M
         IF ( NUNITQ ) THEN
            IF ( SMALLM ) THEN
C                      _
C              Compute B := B * Q.
C
               NR = INT( LZWORK / N )
C
               DO 80 I = 1, M, NR
                  BL = MIN( M-I+1, NR )
                  CALL ZGEMM(  TRANS, 'NoTran', BL, N, N, CONE, B(I,1),
     $                         LDB, Q, LDQ, CZERO, ZWORK, BL )
                  CALL ZLACPY( 'All', BL, N, ZWORK, BL, B(I,1), LDB )
   80          CONTINUE
C
            ELSE
C
C              If M > 7*N/6, perform the QR factorization of B,
C                     _   _
C                 B = P ( R ).
C                       ( 0 )
C
               CALL ZGEQRF( M, N, B, LDB, ZWORK(ITAU), ZWORK(JWORK),
     $                      LZWORK-JWORK+1, IFAIL )
               WRKOPT = MAX( WRKOPT, INT( ZWORK(JWORK) ) + JWORK - 1,
     $                       N*N )
C
C              Form in B
C                 _    _          _
C                 B := RQ,   with B an n-by-n matrix.
C              Use a BLAS 3 operation if enough workspace, and BLAS 2,
C                         _
C              otherwise: B is formed row by row.
C
               IF ( LZWORK.GE.N*N ) THEN
                  CALL ZLACPY( 'Full', N, N, Q, LDQ, ZWORK, N )
                  CALL ZTRMM(  'Left', 'Upper', 'NoTran', 'NoUnit', N,
     $                         N, CONE, B, LDB, ZWORK, N )
                  CALL ZLACPY( 'Full', N, N, ZWORK, MINMN, B, LDB )
               ELSE
                  CALL MB01UZ( 'Left', 'Upper', 'NoTrans', N, N, CONE,
     $                         B, LDB, Q, LDQ, ZWORK, LZWORK, INFO )
               END IF
               NM = N
            END IF
         END IF
C                                        _
C        Perform the QR factorization of B to get the factor F.
C           _                        _
C           B = P ( F ),   M >= N,   B = P ( F  Z ),   M < N.
C                 ( 0 )
C
         CALL ZGEQRF( NM, N, B, LDB, ZWORK(ITAU), ZWORK(JWORK),
     $                LZWORK-JWORK+1, IFAIL )
         IF ( LBSCL .AND. .NOT.SCALB )
     $      CALL ZLASCL( 'Upper', 0, 0, MB, MBTO, NM, N, B, LDB, INFO )
C
         IF ( M.LT.N )
     $      CALL ZLASET( 'Upper', N-M, N-M, CZERO, CZERO, B(M+1,M+1),
     $                   LDB )
C
C        Make the entries on the main diagonal of F non-negative. 
C
         DO 90 I = 1, MINMN
            IF ( DBLE( B(I,I) ).LT.ZERO )
     $         CALL ZDSCAL( N+1-I, -ONE, B(I,I), LDB )
   90    CONTINUE
C
      END IF
      IF ( MINMN.GT.1 )
     $   CALL ZLASET( 'Lower', MINMN-1, MINMN-1, CZERO, CZERO, B(2,1),
     $                LDB )
C
C     Solve for U the transformed Lyapunov equation
C          H      H              H                     2      H
C     op(S) *op(U) *op(U) + op(U) *op(U)*op(S) = -scale *op(F) *op(F),
C
C     or
C          H      H                    H               2      H
C     op(S) *op(U) *op(U)*op(S) - op(U) *op(U) = -scale *op(F) *op(F).
C
C     Workspace:  need   2*N - 2.
C
      CALL SB03OS( .NOT.CONT, ISTRAN, N, A, LDA, B, LDB, SCALE, DWORK,
     $             ZWORK, INFO )
C
C                    H
C     Form  U :=  F*Q  or U := Q*F in the array B, if Q is not identity.
C
      IF ( ISTRAN ) THEN
C
         IF ( NUNITQ ) THEN
C
C           Workspace:  need   N;
C                       prefer larger.
C
            CALL MB01UZ( 'Right', 'Upper', 'NoTran', N, N, CONE, B, LDB,
     $                   Q, LDQ, ZWORK, LZWORK, INFO )
C
C           Overwrite U with the triangular matrix of its
C           RQ-factorization and make the entries on the main diagonal
C           non-negative.
C
C           Workspace:  need   2*N;
C                       prefer N + N*NB.
C
            CALL ZGERQF( N, N, B, LDB, ZWORK, ZWORK(N+1), LZWORK-N,
     $                   IFAIL )
            IF ( N.GT.1 )
     $         CALL ZLASET( 'Lower', N-1, N-1, CZERO, CZERO, B(2,1),
     $                      LDB )
C
            DO 100 I = 1, N
               IF ( DBLE( B(I,I) ).LT.ZERO )
     $            CALL ZDSCAL( I, -ONE, B(1,I), 1 )
  100       CONTINUE
C
         END IF
C
      ELSE
C
         IF ( NUNITQ ) THEN
C
C           Workspace:  need   N;
C                       prefer larger.
C
            CALL MB01UZ( 'Right', 'Upper', 'CTrans', N, N, CONE, B, LDB,
     $                   Q, LDQ, ZWORK, LZWORK, INFO )
C
            DO 110 I = 1, N
               CALL ZSWAP( I, B(I,1), LDB, B(1,I), 1 )
  110       CONTINUE
C
            DO 120 I = 1, N
               CALL ZLACGV( N, B(1,I), 1 )
  120       CONTINUE
C
C           Overwrite U with the triangular matrix of its
C           QR-factorization and make the entries on the main diagonal
C           non-negative.
C
C           Workspace:  2*N;
C                       prefer N + N*NB.
C
            CALL ZGEQRF( N, N, B, LDB, ZWORK, ZWORK(N+1), LZWORK-N,
     $                   IFAIL )
            IF ( N.GT.1 )
     $         CALL ZLASET( 'Lower', N-1, N-1, CZERO, CZERO, B(2,1),
     $                      LDB )
C
            DO 130 I = 1, N
               IF ( DBLE( B(I,I) ).LT.ZERO )
     $            CALL ZDSCAL( N+1-I, -ONE, B(I,I), LDB )
  130       CONTINUE
C
         END IF
C
      END IF
C
C     Undo the scaling of A and B and update SCALE.
C
      TMP = ONE
      IF ( LASCL ) THEN
         CALL ZLASCL( 'Upper', 0, 0, MATO, MA, N, N, A, LDA, INFO )
         TMP = SQRT( MATO/MA )
      END IF
      IF ( LBSCL ) THEN
         MX = ZLANTR( 'Max', 'Upper', 'NoDiag', N, N, B, LDB, DWORK )
         MN = MIN( TMP, MB )
         T  = MAX( TMP, MB )
         IF ( T.GT.ONE ) THEN
            IF ( MN.GT.BIGNMS/T ) THEN
               SCALE = SCALE/T
               TMP   =   TMP/T
            END IF
         END IF
         TMP = TMP*MB
         IF ( TMP.GT.ONE ) THEN
            IF ( MX.GT.BIGNMS/TMP ) THEN
               SCALE = SCALE/MX
               TMP   =   TMP/MX
            END IF
         END IF
      END IF
      IF ( LASCL .OR. LBSCL )
     $   CALL ZLASCL( 'Upper', 0, 0, MBTO, TMP, N, N, B, LDB, INFO )
C
C     Set the optimal workspace.
C
      ZWORK(1) = WRKOPT
C
      RETURN
C *** Last line of SB03OZ ***
      END
