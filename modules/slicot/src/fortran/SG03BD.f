      SUBROUTINE SG03BD( DICO, FACT, TRANS, N, M, A, LDA, E, LDE, Q,
     $                   LDQ, Z, LDZ, B, LDB, SCALE, ALPHAR, ALPHAI,
     $                   BETA, DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To compute the Cholesky factor U of the matrix X,
C
C                 T
C        X = op(U)  * op(U),
C
C     which is the solution of either the generalized c-stable
C     continuous-time Lyapunov equation
C
C             T                    T
C        op(A)  * X * op(E) + op(E)  * X * op(A)
C
C                 2        T
C        = - SCALE  * op(B)  * op(B),                                (1)
C
C     or the generalized d-stable discrete-time Lyapunov equation
C
C             T                    T
C        op(A)  * X * op(A) - op(E)  * X * op(E)
C
C                 2        T
C        = - SCALE  * op(B)  * op(B),                                (2)
C
C     without first finding X and without the need to form the matrix
C     op(B)**T * op(B).
C
C     op(K) is either K or K**T for K = A, B, E, U. A and E are N-by-N
C     matrices, op(B) is an M-by-N matrix. The resulting matrix U is an
C     N-by-N upper triangular matrix with non-negative entries on its
C     main diagonal. SCALE is an output scale factor set to avoid
C     overflow in U.
C
C     In the continuous-time case (1) the pencil A - lambda * E must be
C     c-stable (that is, all eigenvalues must have negative real parts).
C     In the discrete-time case (2) the pencil A - lambda * E must be
C     d-stable (that is, the moduli of all eigenvalues must be smaller
C     than one).
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     DICO    CHARACTER*1
C             Specifies which type of the equation is considered:
C             = 'C':  Continuous-time equation (1);
C             = 'D':  Discrete-time equation (2).
C
C     FACT    CHARACTER*1
C             Specifies whether the generalized real Schur
C             factorization of the pencil A - lambda * E is supplied on
C             entry or not:
C             = 'N':  Factorization is not supplied;
C             = 'F':  Factorization is supplied.
C
C     TRANS   CHARACTER*1
C             Specifies whether the transposed equation is to be solved
C             or not:
C             = 'N':  op(A) = A,    op(E) = E;
C             = 'T':  op(A) = A**T, op(E) = E**T.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrix A.  N >= 0.
C
C     M       (input) INTEGER
C             The number of rows in the matrix op(B).  M >= 0.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, if FACT = 'F', then the leading N-by-N upper
C             Hessenberg part of this array must contain the generalized
C             Schur factor A_s of the matrix A (see definition (3) in
C             section METHOD). A_s must be an upper quasitriangular
C             matrix. The elements below the upper Hessenberg part of
C             the array A are not referenced.
C             If FACT = 'N', then the leading N-by-N part of this array
C             must contain the matrix A.
C             On exit, if FACT = 'N', the leading N-by-N upper
C             Hessenberg part of this array contains the generalized
C             Schur factor A_s of the matrix A. (A_s is an upper
C             quasitriangular matrix.) If FACT = 'F', the leading N-by-N
C             upper triangular part of this array is unchanged.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     E       (input/output) DOUBLE PRECISION array, dimension (LDE,N)
C             On entry, if FACT = 'F', then the leading N-by-N upper
C             triangular part of this array must contain the generalized
C             Schur factor E_s of the matrix E (see definition (4) in
C             section METHOD). E_s must be an upper triangular matrix.
C             The elements below the upper triangular part of the array
C             E are not referenced.
C             If FACT = 'N', then the leading N-by-N part of this array
C             must contain the coefficient matrix E of the equation.
C             On exit, if FACT = 'N', the leading N-by-N upper
C             triangular part of this array contains the generalized
C             Schur factor E_s of the matrix E. (E_s is an upper
C             triangular matrix.) If FACT = 'F', the leading N-by-N
C             upper triangular part of this array is unchanged.
C
C     LDE     INTEGER
C             The leading dimension of the array E.  LDE >= MAX(1,N).
C
C     Q       (input/output) DOUBLE PRECISION array, dimension (LDQ,N)
C             On entry, if FACT = 'F', then the leading N-by-N part of
C             this array must contain the orthogonal matrix Q from the
C             generalized Schur factorization (see definitions (3) and
C             (4) in section METHOD), or an identity matrix (if the
C             original equation has upper triangular matrices A and E).
C             If FACT = 'N', Q need not be set on entry.
C             On exit, if FACT = 'N', the leading N-by-N part of this
C             array contains the orthogonal matrix Q from the
C             generalized Schur factorization. If FACT = 'F', this array
C             is unchanged.
C
C     LDQ     INTEGER
C             The leading dimension of the array Q.  LDQ >= MAX(1,N).
C
C     Z       (input/output) DOUBLE PRECISION array, dimension (LDZ,N)
C             On entry, if FACT = 'F', then the leading N-by-N part of
C             this array must contain the orthogonal matrix Z from the
C             generalized Schur factorization (see definitions (3) and
C             (4) in section METHOD), or an identity matrix (if the
C             original equation has upper triangular matrices A and E).
C             If FACT = 'N', Z need not be set on entry.
C             On exit, if FACT = 'N', the leading N-by-N part of this
C             array contains the orthogonal matrix Z from the
C             generalized Schur factorization. If FACT = 'F', this array
C             is unchanged.
C
C     LDZ     INTEGER
C             The leading dimension of the array Z.  LDZ >= MAX(1,N).
C
C     B       (input/output) DOUBLE PRECISION array, dimension (LDB,N1)
C             On entry, if TRANS = 'T', the leading N-by-M part of this
C             array must contain the matrix B and N1 >= MAX(M,N).
C             If TRANS = 'N', the leading M-by-N part of this array
C             must contain the matrix B and N1 >= N.
C             On exit, if INFO = 0, the leading N-by-N part of this
C             array contains the Cholesky factor U of the solution
C             matrix X of the problem, X = op(U)**T * op(U).
C             If M = 0 and N > 0, then U is set to zero.
C
C     LDB     INTEGER
C             The leading dimension of the array B.
C             If TRANS = 'T',  LDB >= MAX(1,N).
C             If TRANS = 'N',  LDB >= MAX(1,M,N).
C
C     SCALE   (output) DOUBLE PRECISION
C             The scale factor set to avoid overflow in U.
C             0 < SCALE <= 1.
C
C     ALPHAR  (output) DOUBLE PRECISION array, dimension (N)
C     ALPHAI  (output) DOUBLE PRECISION array, dimension (N)
C     BETA    (output) DOUBLE PRECISION array, dimension (N)
C             If INFO = 0, 3, 5, 6, or 7, then
C             (ALPHAR(j) + ALPHAI(j)*i)/BETA(j), j = 1, ... ,N, are the
C             eigenvalues of the matrix pencil A - lambda * E.
C             All BETA(j) are non-negative numbers.
C             ALPHAR and ALPHAI will be always less than and usually
C             comparable with norm(A) in magnitude, and BETA always less
C             than and usually comparable with norm(B).
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal value
C             of LDWORK.
C             On exit, if INFO = -21, DWORK(1) returns the minimum value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The dimension of the array DWORK.
C             LDWORK >= MAX(1,8*N+16),     if FACT = 'N';
C             LDWORK >= MAX(1,2*N,6*N-6),  if FACT = 'F'.
C             For good performance, LDWORK should be larger.
C
C             If LDWORK = -1, then a workspace query is assumed; the
C             routine only calculates the optimal size of the DWORK
C             array, returns this value as the first entry of the DWORK
C             array, and no error message related to LDWORK is issued by
C             XERBLA.
C
C     Error indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value;
C             = 1:  the pencil A - lambda * E is (nearly) singular;
C                   perturbed values were used to solve the equation
C                   (but the reduced (quasi)triangular matrices A and E
C                   are unchanged);
C             = 2:  FACT = 'F' and the matrix contained in the upper
C                   Hessenberg part of the array A is not in upper
C                   quasitriangular form;
C             = 3:  FACT = 'F' and there is a 2-by-2 block on the main
C                   diagonal of the pencil A_s - lambda * E_s whose
C                   eigenvalues are not conjugate complex;
C             = 4:  FACT = 'N' and the pencil A - lambda * E cannot be
C                   reduced to generalized Schur form: LAPACK routine
C                   DGGES has failed to converge;
C             = 5:  DICO = 'C' and the pencil A - lambda * E is not
C                   c-stable;
C             = 6:  DICO = 'D' and the pencil A - lambda * E is not
C                   d-stable;
C             = 7:  the LAPACK routine DSYEVX utilized to factorize M3
C                   failed to converge in the discrete-time case (see
C                   section METHOD for SLICOT Library routine SG03BU).
C                   This error is unlikely to occur.
C
C     METHOD
C
C     An extension [2] of Hammarling's method [1] to generalized
C     Lyapunov equations is utilized to solve (1) or (2).
C
C     First the pencil A - lambda * E is reduced to real generalized
C     Schur form A_s - lambda * E_s by means of orthogonal
C     transformations (QZ-algorithm):
C
C        A_s = Q**T * A * Z   (upper quasitriangular),               (3)
C
C        E_s = Q**T * E * Z   (upper triangular).                    (4)
C
C     If the pencil A - lambda * E has already been factorized prior to
C     calling the routine, however, then the factors A_s, E_s, Q and Z
C     may be supplied and the initial factorization omitted.
C
C     Depending on the parameters TRANS and M, the N-by-N upper
C     triangular matrix B_s is defined as follows. In any case Q_B is
C     an M-by-M orthogonal matrix, which need not be accumulated.
C
C     1. If TRANS = 'N' and M < N, B_s is the upper triangular matrix
C        from the QR-factorization
C
C           ( Q_B  O )           ( B * Z )
C           (        ) * B_s  =  (       ),
C           (  O   I )           (   O   )
C
C        where the O's are zero matrices of proper size and I is the
C        identity matrix of order N-M.
C
C     2. If TRANS = 'N' and M >= N, B_s is the upper triangular matrix
C        from the (rectangular) QR-factorization
C
C                 ( B_s )
C           Q_B * (     )  =  B * Z,
C                 (  O  )
C
C        where O is the (M-N)-by-N zero matrix.
C
C     3. If TRANS = 'T' and M < N, B_s is the upper triangular matrix
C        from the RQ-factorization
C
C                       ( Q_B  O )
C           (B_s  O ) * (        )  =  ( Q**T * B   O ).
C                       (  O   I )
C
C     4. If TRANS = 'T' and M >= N, B_s is the upper triangular matrix
C        from the (rectangular) RQ-factorization
C
C           ( B_s   O ) * Q_B  =  Q**T * B,
C
C        where O is the N-by-(M-N) zero matrix.
C
C     Assuming SCALE = 1, the transformation of A, E and B described
C     above leads to the reduced continuous-time equation
C
C                 T        T
C          op(A_s)  op(U_s)  op(U_s) op(E_s)
C
C                 T        T
C        + op(E_s)  op(U_s)  op(U_s) op(A_s)
C
C                    T
C        =  - op(B_s)  op(B_s)                                       (5)
C
C     or to the reduced discrete-time equation
C
C                 T        T
C          op(A_s)  op(U_s)  op(U_s) op(A_s)
C
C                 T        T
C        - op(E_s)  op(U_s)  op(U_s) op(E_s)
C
C                    T
C        =  - op(B_s)  op(B_s).                                      (6)
C
C     For brevity we restrict ourself to equation (5) and the case
C     TRANS = 'N'. The other three cases can be treated in a similar
C     fashion.
C
C     We use the following partitioning for the matrices A_s, E_s, B_s,
C     and U_s
C
C                 ( A11   A12 )          ( E11   E12 )
C           A_s = (           ),   E_s = (           ),
C                 (   0   A22 )          (   0   E22 )
C
C                 ( B11   B12 )          ( U11   U12 )
C           B_s = (           ),   U_s = (           ).              (7)
C                 (   0   B22 )          (   0   U22 )
C
C     The size of the (1,1)-blocks is 1-by-1 (iff A_s(2,1) = 0.0) or
C     2-by-2.
C
C     We compute U11, U12**T, and U22 in three steps.
C
C     Step I:
C
C        From (5) and (7) we get the 1-by-1 or 2-by-2 equation
C
C                T      T                   T      T
C             A11  * U11  * U11 * E11  + E11  * U11  * U11 * A11
C
C                    T
C             = - B11  * B11.
C
C        For brevity, details are omitted here. See [2]. The technique
C        for computing U11 is similar to those applied to standard
C        Lyapunov equations in Hammarling's algorithm ([1], section 6).
C
C        Furthermore, the auxiliary matrices M1 and M2 defined as
C        follows
C
C                               -1      -1
C           M1 = U11 * A11 * E11   * U11
C
C                         -1      -1
C           M2 = B11 * E11   * U11
C
C        are computed in a numerically reliable way.
C
C     Step II:
C
C        The generalized Sylvester equation
C
C              T      T      T      T
C           A22  * U12  + E22  * U12  * M1  =
C
C                T           T      T      T      T
C           - B12  * M2 - A12  * U11  - E12  * U11  * M1
C
C        is solved for U12**T.
C
C     Step III:
C
C        It can be shown that
C
C              T      T                  T      T
C           A22  * U22  * U22 * E22 + E22  * U22  * U22 * A22  =
C
C                T              T
C           - B22  * B22 - y * y                                     (8)
C
C        holds, where y is defined as
C
C                  T        T      T      T      T       T
C           y = B12  - ( E12  * U11  + E22  * U12  ) * M2 .
C
C        If B22_tilde is the square triangular matrix arising from the
C        (rectangular) QR-factorization
C
C                       ( B22_tilde )     ( B22  )
C           Q_B_tilde * (           )  =  (      ),
C                       (     O     )     ( y**T )
C
C        where Q_B_tilde is an orthogonal matrix of order N, then
C
C                T              T                T
C           - B22  * B22 - y * y   =  - B22_tilde  * B22_tilde.
C
C        Replacing the right hand side in (8) by the term
C        - B22_tilde**T * B22_tilde leads to a reduced generalized
C        Lyapunov equation of lower dimension compared to (5).
C
C     The recursive application of the steps I to III yields the
C     solution U_s of the equation (5).
C
C     It remains to compute the solution matrix U of the original
C     problem (1) or (2) from the matrix U_s. To this end we transform
C     the solution back (with respect to the transformation that led
C     from (1) to (5) (from (2) to (6)) and apply the QR-factorization
C     (RQ-factorization). The upper triangular solution matrix U is
C     obtained by
C
C        Q_U * U  =  U_s * Q**T     (if TRANS = 'N'),
C
C     or
C
C        U * Q_U  =  Z * U_s        (if TRANS = 'T'),
C
C     where Q_U is an N-by-N orthogonal matrix. Again, the orthogonal
C     matrix Q_U need not be accumulated.
C
C     REFERENCES
C
C     [1] Hammarling, S.J.
C         Numerical solution of the stable, non-negative definite
C         Lyapunov equation.
C         IMA J. Num. Anal., 2, pp. 303-323, 1982.
C
C     [2] Penzl, T.
C         Numerical solution of generalized Lyapunov equations.
C         Advances in Comp. Math., vol. 8, pp. 33-48, 1998.
C
C     NUMERICAL ASPECTS
C
C     The number of flops required by the routine is given by the
C     following table. Note that we count a single floating point
C     arithmetic operation as one flop.
C
C                 |           FACT = 'F'                  FACT = 'N'
C        ---------+--------------------------------------------------
C         M <= N  |     (13*N**3+6*M*N**2         (211*N**3+6*M*N**2
C                 |   +6*M**2*N-2*M**3)/3        +6*M**2*N-2*M**3)/3
C                 |
C          M > N  | (11*N**3+12*M*N**2)/3     (209*N**3+12*M*N**2)/3
C
C     FURTHER COMMENTS
C
C     The Lyapunov equation may be very ill-conditioned. In particular,
C     if DICO = 'D' and the pencil A - lambda * E has a pair of almost
C     reciprocal eigenvalues, or DICO = 'C' and the pencil has an almost
C     degenerate pair of eigenvalues, then the Lyapunov equation will be
C     ill-conditioned. Perturbed values were used to solve the equation.
C     A condition estimate can be obtained from the routine SG03AD.
C     When setting the error indicator INFO, the routine does not test
C     for near instability in the equation but only for exact
C     instability.
C
C     CONTRIBUTOR
C
C     T. Penzl, Technical University Chemnitz, Germany, Aug. 1998.
C
C     REVISIONS
C
C     Sep. 1998, May 1999 (V. Sima).
C     March 2002 (A. Varga).
C     Feb. 2004, July 2011, Dec. 2021 - Feb. 2022 (V. Sima).
C
C     KEYWORDS
C
C     Lyapunov equation
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  MONE, ONE, ZERO
      PARAMETER         ( MONE = -1.0D+0, ONE = 1.0D+0, ZERO = 0.0D+0 )
C     .. Scalar Arguments ..
      DOUBLE PRECISION  SCALE
      INTEGER           INFO, LDA, LDB, LDE, LDQ, LDWORK, LDZ, M, N
      CHARACTER         DICO, FACT, TRANS
C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), ALPHAI(*), ALPHAR(*), B(LDB,*),
     $                  BETA(*), DWORK(*), E(LDE,*), Q(LDQ,*), Z(LDZ,*)
C     .. Local Scalars ..
      DOUBLE PRECISION  BIGNMS, BIGNUM, EPS, MA, MATO, MB, MBTO, ME,
     $                  METO, MN, MX, S1, S2, SAFMIN, SMLNUM, T, TMP,
     $                  WI, WR1, WR2
      INTEGER           BL, I, INFO1, J, K, L, MAXMN, MINGG, MINMN,
     $                  MINWRK, NC, NR, OPTWRK
      LOGICAL           ISDISC, ISFACT, ISTRAN, LASCL, LBSCL, LESCL,
     $                  LQUERY, LSCL, NUNITQ, NUNITZ, SCALB
C     .. Local Arrays ..
      DOUBLE PRECISION  E1(2,2)
      LOGICAL           BWORK(1)
C     .. External Functions ..
      DOUBLE PRECISION  DLAMCH, DLANGE, DLANHS, DLANTR, DLAPY2
      LOGICAL           DELCTG, LSAME, MA02HD
      EXTERNAL          DELCTG, DLAMCH, DLANGE, DLANHS, DLANTR, DLAPY2,
     $                  LSAME, MA02HD
C     .. External Subroutines ..
      EXTERNAL          DCOPY, DGEMM, DGEQRF, DGERQF, DGGES,
     $                  DLABAD, DLACPY, DLAG2, DLASCL, DLASET, DSCAL,
     $                  DSWAP, MB01UY, SG03BU, SG03BV, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         DBLE, INT, MAX, MIN, SIGN, SQRT
C     .. Executable Statements ..
C
C     Decode input parameters.
C
      ISDISC = LSAME( DICO,  'D' )
      ISFACT = LSAME( FACT,  'F' )
      ISTRAN = LSAME( TRANS, 'T' )
      LQUERY = LDWORK.EQ.-1
C
C     Check the scalar input parameters.
C
      INFO = 0
      IF (     .NOT.( ISDISC .OR. LSAME( DICO,  'C' ) ) ) THEN
         INFO = -1
      ELSEIF ( .NOT.( ISFACT .OR. LSAME( FACT,  'N' ) ) ) THEN
         INFO = -2
      ELSEIF ( .NOT.( ISTRAN .OR. LSAME( TRANS, 'N' ) ) ) THEN
         INFO = -3
      ELSEIF ( N.LT.0 ) THEN
         INFO = -4
      ELSEIF ( M.LT.0 ) THEN
         INFO = -5
      ELSEIF ( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSEIF ( LDE.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSEIF ( LDQ.LT.MAX( 1, N ) ) THEN
         INFO = -11
      ELSEIF ( LDZ.LT.MAX( 1, N ) ) THEN
         INFO = -13
      ELSEIF ( ( ISTRAN .AND. ( LDB.LT.MAX( 1, N ) ) ) .OR.
     $    ( .NOT.ISTRAN .AND. ( LDB.LT.MAX( 1, M, N ) ) ) ) THEN
         INFO = -15
      ELSE
C
C        Compute minimal and optimal workspace.
C
         IF ( ISFACT ) THEN
            MINWRK = MAX( 1, 2*N, 6*N-6 )
         ELSE
            MINWRK = MAX( 1, 8*N + 16 )
         END IF
         MINGG = MAX( MINWRK, 8*N + 16 )
         MAXMN = MAX( M, N )
         IF ( LQUERY ) THEN
            OPTWRK = MINWRK
            IF ( .NOT.ISFACT ) THEN
               CALL DGGES( 'Vectors', 'Vectors', 'Not ordered', DELCTG,
     $                     N, A, LDA, E, LDE, I, ALPHAR, ALPHAI, BETA,
     $                     Q, LDQ, Z, LDZ, DWORK, -1, BWORK, INFO1 )
               OPTWRK = MAX( MINGG, INT( DWORK(1) ) )
            END IF
            IF ( ISTRAN ) THEN
               CALL DGERQF( N, MAXMN, B, LDB, DWORK, DWORK, -1, INFO1 )
            ELSE
               CALL DGEQRF( MAXMN, N, B, LDB, DWORK, DWORK, -1, INFO1 )
            END IF
            OPTWRK = MAX( OPTWRK, INT( DWORK(1) ) + N )
         ELSEIF ( LDWORK.LT.MINWRK ) THEN
            DWORK(1) = MINWRK
            INFO = -21
         END IF
      END IF
C
      IF ( INFO.NE.0 ) THEN
         CALL XERBLA( 'SG03BD', -INFO )
         RETURN
      ELSE IF ( LQUERY ) THEN
         DWORK(1) = OPTWRK
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
      MB = DLANGE( 'Max', K, L, B, LDB, DWORK )
      IF ( MB.EQ.ZERO ) THEN
         IF ( N.GT.0 )
     $      CALL DLASET( 'Full', N, N, ZERO, ZERO, B, LDB )
         DWORK(1) = ONE
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
      IF ( .NOT.ISFACT ) THEN
C
C        Reduce the pencil A - lambda * E to generalized Schur form.
C
C           A := Q**T * A * Z   (upper quasitriangular),
C           E := Q**T * E * Z   (upper triangular).
C
C        The diagonal elements of E are non-negative real numbers.
C
C           Use DGGES. The workspace is increased to avoid an error
C           return, while it should not really be larger than above.
C           Workspace: >= MAX(1,8*N+16)
         CALL DGGES( 'Vectors', 'Vectors', 'Not ordered', DELCTG, N,
     $               A, LDA, E, LDE, I, ALPHAR, ALPHAI, BETA, Q, LDQ,
     $               Z, LDZ, DWORK, LDWORK, BWORK, INFO1 )
         IF ( INFO1 .NE. 0 ) THEN
            INFO = 4
            RETURN
         END IF
         NUNITQ = .TRUE.
         NUNITZ = .TRUE.
         OPTWRK = INT( DWORK(1) )
C
      ELSE
C
C        Make sure the upper Hessenberg part of A is quasitriangular.
C
         DO 10 I = 1, N-2
            IF ( A(I+1,I).NE.ZERO .AND. A(I+2,I+1).NE.ZERO ) THEN
               INFO = 2
               RETURN
            END IF
   10    CONTINUE
C
C        Compute the eigenvalues of the matrix pencil A - lambda * E.
C
         E1(2,1) = ZERO
         I = 1
C        WHILE ( I.LE.N ) DO
   20    CONTINUE
         IF ( I.LT.N ) THEN
            IF ( A(I+1,I).EQ.ZERO ) THEN
               ALPHAR(I) = A(I,I)
               ALPHAI(I) = ZERO
               BETA(I)   = E(I,I)
            ELSE
               E1(1,1) = E(I,I)
               E1(1,2) = E(I,I+1)
               E1(2,2) = E(I+1,I+1)
               CALL DLAG2( A(I,I), LDA, E1, 2, SAFMIN, S1, S2, WR1, WR2,
     $                     WI )
               IF ( WI.EQ.ZERO ) THEN
                  INFO = 3
                  RETURN
               END IF
               ALPHAR(I) = WR1
               ALPHAI(I) = WI
               BETA(I)   = S1
               I = I + 1
               ALPHAR(I) = WR2
               ALPHAI(I) = -WI
               BETA(I)   = S2
            END IF
            I = I + 1
            GOTO 20
         ELSE IF ( I.EQ.N ) THEN
            ALPHAR(N) = A(N,N)
            ALPHAI(N) = ZERO
            BETA(N)   = E(N,N)
         END IF
C        END WHILE 20
C
C        Check for identity matrices Q and/or Z.
C
         NUNITQ = .NOT.MA02HD( 'All', N, N, ONE, Q, LDQ )
         NUNITZ = .NOT.MA02HD( 'All', N, N, ONE, Z, LDZ )
         OPTWRK = MINWRK
      END IF
C
C     Check on the stability of the matrix pencil A - lambda * E.
C
      DO 30 I = 1, N
         IF ( ISDISC ) THEN
            IF ( DLAPY2( ALPHAR(I), ALPHAI(I) ).GE.BETA(I) ) THEN
               INFO = 6
               RETURN
            END IF
         ELSE
            IF ( ( ALPHAR(I).EQ.ZERO ) .OR. ( BETA(I).EQ.ZERO ) .OR.
     $         ( SIGN( ONE,ALPHAR(I) )*SIGN( ONE, BETA(I) ).GE.ZERO ) )
     $         THEN
               INFO = 5
               RETURN
            END IF
         END IF
   30 CONTINUE
C
C     Scale A if the maximum absolute value of its elements is outside
C     the range [SMLNUM,BIGNUM]. Scale similarly E and B. The scaling
C     factors of E may be set equal to those for A, to preserve
C     stability in the discrete-time case. Scaling of B is done before
C     further processing if the maximum absolute value of its elements
C     is greater than BIGNMS; otherwise, it is postponed. Scaling is
C     also performed if the maximum absolute values of A, E, B differ
C     too much, or their minimum (maximum) is too large (small).
C
      MA = MIN( DLANHS( 'Max', N, A, LDA, DWORK ), BIGNMS )
      ME = MIN( DLANTR( 'Max', 'Upper', 'NoDiag', N, N, E, LDE, DWORK ),
     $          BIGNMS )
      MN = MIN( MA, ME, MB )
      MX = MAX( MA, ME, MB )
C
      LSCL = MN.LT.MX*SMLNUM .OR. MX.LT.SMLNUM .OR. MN.GT.BIGNUM
      IF ( LSCL ) THEN
         MATO  = ONE
         METO  = ONE
         MBTO  = ONE
         LASCL = .TRUE.
         LESCL = .TRUE.
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
         IF ( ME.GT.ZERO .AND. ME.LT.SMLNUM ) THEN
            METO  = SMLNUM
            LESCL = .TRUE.
         ELSE IF ( ME.GT.BIGNUM ) THEN
            METO  = BIGNUM
            LESCL = .TRUE.
         ELSE
            LESCL = .FALSE.
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
      IF ( ISDISC .AND. LASCL .AND. LESCL ) THEN
         IF ( MATO/MA.GT.METO/ME ) THEN
            ME   = MA
            METO = MATO
         END IF
      END IF
C
      IF ( LASCL )
     $   CALL DLASCL( 'Hess',  0, 0, MA, MATO, N, N, A, LDA, INFO )
      IF ( LESCL )
     $   CALL DLASCL( 'Upper', 0, 0, ME, METO, N, N, E, LDE, INFO )
C
      SCALB = MB.GT.BIGNMS
      MB    = MIN( MB, BIGNMS )
      IF ( LBSCL .AND. SCALB )
     $   CALL DLASCL( 'Gen', 0, 0, MB, MBTO, K, L, B, LDB, INFO )
C
C     Transformation of the right hand side:
C
C        B := Q**T * B  or  B := B * Z.
C
C     Workspace:  need max(1,2*N);  prefer larger.
C
      IF ( ISTRAN ) THEN
C
         IF ( NUNITQ ) THEN
            NC = INT( LDWORK / N )
C
            DO 40 J = 1, M, NC
               BL = MIN( M-J+1, NC )
               CALL DGEMM(  'Trans', 'NoTrans', N, BL, N, ONE, Q, LDQ,
     $                      B(1,J), LDB, ZERO, DWORK, N )
               CALL DLACPY( 'All', N, BL, DWORK, N, B(1,J), LDB )
   40       CONTINUE
C
         END IF
C
      ELSE
C
         IF ( NUNITQ ) THEN
            NR = INT( LDWORK / N )
C
            DO 50 I = 1, M, NR
               BL = MIN( M-I+1, NR )
               CALL DGEMM(  TRANS, 'NoTrans', BL, N, N, ONE, B(I,1),
     $                      LDB, Z, LDZ, ZERO, DWORK, BL )
               CALL DLACPY( 'All', BL, N, DWORK, BL, B(I,1), LDB )
   50       CONTINUE
C
         END IF
C
      END IF
C
C     Overwrite B with the triangular matrix of its RQ-factorization
C     or its QR-factorization.
C     (The entries on the main diagonal are non-negative.)
C     Then, do scaling, if it was postponed.
C
C     Workspace:  need max(1,MIN(M,N)+N);  prefer larger.
C
      MINMN = MIN( M, N )
      IF ( ISTRAN ) THEN
C
         CALL DGERQF( N, M, B, LDB, DWORK, DWORK(N+1), LDWORK-N, INFO1 )
         IF ( N.GE.M ) THEN
            IF ( LBSCL .AND. .NOT.SCALB ) THEN
               CALL DLASCL( 'Gen', 0, 0, MB, MBTO, N-M, M, B, LDB,
     $                      INFO )
               CALL DLASCL( 'Upper', 0, 0, MB, MBTO, M, M, B(N-M+1,1),
     $                      LDB, INFO )
            END IF
            IF ( N.GT.M ) THEN
C
               DO 60 I = M, 1, -1
                  CALL DCOPY( I+N-M, B(1,I), 1, B(1,I+N-M), 1 )
   60          CONTINUE
C
               CALL DLASET( 'All', N, N-M, ZERO, ZERO, B, LDB )
            END IF
            IF ( M.GT.1 )
     $         CALL DLASET( 'Lower', M-1, M-1, ZERO, ZERO,
     $                      B(N-M+2,N-M+1), LDB )
         ELSE
            IF ( LBSCL .AND. .NOT.SCALB )
     $         CALL DLASCL( 'Upper', 0, 0, MB, MBTO, N, M, B, LDB,
     $                      INFO )
C
            DO  70 I = 1, N
               CALL DCOPY( I, B(1,M-N+I), 1, B(1,I), 1 )
   70       CONTINUE
C
            IF ( N.GT.1 )
     $         CALL DLASET( 'Lower', N-1, N-1, ZERO, ZERO, B(2,1), LDB )
         END IF
C
         DO 80 I = N - MINMN + 1, N
            IF ( B(I,I).LT.ZERO )
     $         CALL DSCAL( I, MONE, B(1,I), 1 )
   80    CONTINUE
C
      ELSE
C
         CALL DGEQRF( M, N, B, LDB, DWORK, DWORK(N+1), LDWORK-N, INFO1 )
         IF ( LBSCL .AND. .NOT.SCALB )
     $      CALL DLASCL( 'Upper', 0, 0, MB, MBTO, M, N, B, LDB, INFO )
         IF ( MAXMN.GT.1 )
     $      CALL DLASET( 'Lower', MAXMN-1, MINMN, ZERO, ZERO, B(2,1),
     $                   LDB )
         IF ( N.GT.M )
     $      CALL DLASET( 'All', N-M, N, ZERO, ZERO, B(M+1,1), LDB )
C
         DO 90 I = 1, MINMN
            IF ( B(I,I).LT.ZERO )
     $         CALL DSCAL( N+1-I, MONE, B(I,I), LDB )
   90    CONTINUE
C
      END IF
C
C     Solve the reduced generalized Lyapunov equation.
C
C     Workspace: 6*N-6
C
      IF ( ISDISC ) THEN
         CALL SG03BU( TRANS, N, A, LDA, E, LDE, B, LDB, SCALE, DWORK,
     $                INFO1 )
         IF ( INFO1.NE.0 ) THEN
            IF ( INFO1.EQ.1 )
     $         INFO = 1
            IF ( INFO1.EQ.2 )
     $         INFO = 3
            IF ( INFO1.EQ.3 )
     $         INFO = 6
            IF ( INFO1.EQ.4 )
     $         INFO = 7
            IF ( INFO.NE.1 )
     $         RETURN
         END IF
      ELSE
         CALL SG03BV( TRANS, N, A, LDA, E, LDE, B, LDB, SCALE, DWORK,
     $                INFO1 )
         IF ( INFO1.NE.0 ) THEN
            IF ( INFO1.EQ.1 )
     $         INFO = 1
            IF ( INFO1.GE.2 )
     $         INFO = 3
            IF ( INFO1.EQ.3 )
     $         INFO = 5
            IF ( INFO.NE.1 )
     $         RETURN
         END IF
      END IF
C
C     Transform the solution matrix back, if Z and/or Q are not unit:
C
C        U := Z * U  or  U := U * Q**T ( U**T := Q * U**T).
C
      IF ( ISTRAN ) THEN
C
         IF ( NUNITZ ) THEN
C
C           Workspace:  max(1,N);  prefer larger.
C
            CALL MB01UY( 'Right', 'Upper', 'NoTrans', N, N, ONE, B, LDB,
     $                   Z, LDZ, DWORK, LDWORK, INFO )
C
C           Overwrite U with the triangular matrix of its
C           RQ-factorization and make the entries on the main diagonal
C           non-negative.
C
C           Workspace:  >= max(1,2*N);  prefer larger.
C
            CALL DGERQF( N, N, B, LDB, DWORK, DWORK(N+1), LDWORK-N,
     $                   INFO1 )
            IF ( N.GT.1 )
     $         CALL DLASET( 'Lower', N-1, N-1, ZERO, ZERO, B(2,1), LDB )
C
            DO 100 I = 1, N
               IF ( B(I,I).LT.ZERO )
     $            CALL DSCAL( I, MONE, B(1,I), 1 )
  100       CONTINUE
C
         END IF
C
      ELSE
C
         IF ( NUNITQ ) THEN
C
C           Workspace:  max(1,N);  prefer larger.
C
            CALL MB01UY( 'Right', 'Upper', 'Trans', N, N, ONE, B, LDB,
     $                   Q, LDQ, DWORK, LDWORK, INFO )
C
            DO 110 I = 1, N
               CALL DSWAP( I, B(I,1), LDB, B(1,I), 1 )
  110       CONTINUE
C
C           Overwrite U with the triangular matrix of its
C           QR-factorization and make the entries on the main diagonal
C           non-negative.
C
C           Workspace:  >= max(1,2*N);  prefer larger.
C
            CALL DGEQRF( N, N, B, LDB, DWORK, DWORK(N+1), LDWORK-N,
     $                   INFO1 )
            IF ( N.GT.1 )
     $         CALL DLASET( 'Lower', N-1, N-1, ZERO, ZERO, B(2,1), LDB )
C
            DO 120 I = 1, N
               IF ( B(I,I).LT.ZERO )
     $            CALL DSCAL( N+1-I, MONE, B(I,I), LDB )
  120       CONTINUE
C
         END IF
C
      END IF
C
C     Undo the scaling of A, E, and B and update SCALE.
C
      TMP = ONE
      IF ( LASCL ) THEN
         CALL DLASCL( 'Hess', 0, 0, MATO, MA, N, N, A, LDA, INFO )
         TMP = SQRT( MATO/MA )
      END IF
      IF ( LESCL ) THEN
         CALL DLASCL( 'Upper', 0, 0, METO, ME, N, N, E, LDE, INFO )
         TMP = TMP*SQRT( METO/ME )
      END IF
      IF ( LBSCL ) THEN
         MX = DLANTR( 'Max', 'Upper', 'NoDiag', N, N, B, LDB, DWORK )
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
      IF ( LASCL .OR. LESCL .OR. LBSCL )
     $   CALL DLASCL( 'Upper', 0, 0, MBTO, TMP, N, N, B, LDB, INFO )
C
      OPTWRK = MAX( OPTWRK, INT( DWORK(N+1) ) + N )
C
      DWORK(1) = DBLE( MAX( OPTWRK, MINWRK ) )
      RETURN
C *** Last line of SG03BD ***
      END
   