      SUBROUTINE SG03BZ( DICO, FACT, TRANS, N, M, A, LDA, E, LDE, Q,
     $                   LDQ, Z, LDZ, B, LDB, SCALE, ALPHA, BETA, DWORK,
     $                   ZWORK, LZWORK, INFO )
C
C     PURPOSE
C
C     To compute the Cholesky factor U of the matrix X,
C
C                 H
C        X = op(U)  * op(U),
C
C     which is the solution of either the generalized c-stable
C     continuous-time Lyapunov equation
C
C             H                    H
C        op(A)  * X * op(E) + op(E)  * X * op(A)
C
C                 2        H
C        = - SCALE  * op(B)  * op(B),                                (1)
C
C     or the generalized d-stable discrete-time Lyapunov equation
C
C             H                    H
C        op(A)  * X * op(A) - op(E)  * X * op(E)
C
C                 2        H
C        = - SCALE  * op(B)  * op(B),                                (2)
C
C     without first finding X and without the need to form the matrix
C     op(B)**H * op(B).
C
C     op(K) is either K or K**H for K = A, B, E, U. A and E are N-by-N
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
C             Specifies whether the generalized (complex) Schur
C             factorization of the pencil A - lambda * E is supplied on
C             entry or not:
C             = 'N':  Factorization is not supplied;
C             = 'F':  Factorization is supplied.
C
C     TRANS   CHARACTER*1
C             Specifies whether the conjugate transposed equation is to
C             be solved or not:
C             = 'N':  op(A) = A,    op(E) = E;
C             = 'C':  op(A) = A**H, op(E) = E**H.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrix A.  N >= 0.
C
C     M       (input) INTEGER
C             The number of rows in the matrix op(B).  M >= 0.
C             If M = 0, A and E are unchanged on exit, and Q, Z, ALPHA
C             and BETA are not set.
C
C     A       (input/output) COMPLEX*16 array, dimension (LDA,N)
C             On entry, if FACT = 'F', then the leading N-by-N upper
C             triangular part of this array must contain the generalized
C             Schur factor A_s of the matrix A (see definition (3) in
C             section METHOD). A_s must be an upper triangular matrix.
C             The elements below the upper triangular part of the array
C             A are used as workspace.
C             If FACT = 'N', then the leading N-by-N part of this array
C             must contain the matrix A.
C             On exit, if FACT = 'N', the leading N-by-N upper
C             triangular part of this array contains the generalized
C             Schur factor A_s of the matrix A. (A_s is an upper
C             triangular matrix.) If FACT = 'F', the leading N-by-N
C             upper triangular part of this array is unchanged.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     E       (input/output) COMPLEX*16 array, dimension (LDE,N)
C             On entry, if FACT = 'F', then the leading N-by-N upper
C             triangular part of this array must contain the generalized
C             Schur factor E_s of the matrix E (see definition (4) in
C             section METHOD). E_s must be an upper triangular matrix.
C             The elements below the upper triangular part of the array
C             E are used as workspace.
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
C     Q       (input/output) COMPLEX*16 array, dimension (LDQ,N)
C             On entry, if FACT = 'F', then the leading N-by-N part of
C             this array must contain the unitary matrix Q from the
C             generalized Schur factorization (see definitions (3) and
C             (4) in section METHOD), or an identity matrix (if the
C             original equation has upper triangular matrices A and E).
C             If FACT = 'N', Q need not be set on entry.
C             On exit, if FACT = 'N', the leading N-by-N part of this
C             array contains the unitary matrix Q from the generalized
C             Schur factorization. If FACT = 'F', this array is
C             unchanged.
C
C     LDQ     INTEGER
C             The leading dimension of the array Q.  LDQ >= MAX(1,N).
C
C     Z       (input/output) COMPLEX*16 array, dimension (LDZ,N)
C             On entry, if FACT = 'F', then the leading N-by-N part of
C             this array must contain the unitary matrix Z from the
C             generalized Schur factorization (see definitions (3) and
C             (4) in section METHOD), or an identity matrix (if the
C             original equation has upper triangular matrices A and E).
C             If FACT = 'N', Z need not be set on entry.
C             On exit, if FACT = 'N', the leading N-by-N part of this
C             array contains the unitary matrix Z from the generalized
C             Schur factorization. If FACT = 'F', this array is
C             unchanged.
C
C     LDZ     INTEGER
C             The leading dimension of the array Z.  LDZ >= MAX(1,N).
C
C     B       (input/output) COMPLEX*16 array, dimension (LDB,N1)
C             On entry, if TRANS = 'C', the leading N-by-M part of this
C             array must contain the matrix B and N1 >= MAX(M,N).
C             If TRANS = 'N', the leading M-by-N part of this array
C             must contain the matrix B and N1 >= N.
C             On exit, if INFO = 0, the leading N-by-N part of this
C             array contains the Cholesky factor U of the solution
C             matrix X of the problem, X = op(U)**H * op(U).
C             If M = 0 and N > 0, then U is set to zero.
C
C     LDB     INTEGER
C             The leading dimension of the array B.
C             If TRANS = 'C',  LDB >= MAX(1,N).
C             If TRANS = 'N',  LDB >= MAX(1,M,N).
C
C     SCALE   (output) DOUBLE PRECISION
C             The scale factor set to avoid overflow in U.
C             0 < SCALE <= 1.
C
C     ALPHA   (output) COMPLEX*16 arrays, dimension (N)
C     BETA    If INFO = 0, 5, 6, or 7, then ALPHA(j)/BETA(j),
C             j = 1, ... , N, are the eigenvalues of the matrix pencil
C             A - lambda * E (the diagonals of the complex Schur form).
C             All BETA(j) are non-negative real numbers.
C             ALPHA will be always less than and usually comparable with
C             norm(A) in magnitude, and BETA always less than and
C             usually comparable with norm(B).
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK), where
C             LDWORK = 0,           if MIN(M,N) = 0 or
C                                      FACT = 'F' and N <= 1; else,
C             LDWORK = N-1,         if FACT = 'F' and DICO = 'C';
C             LDWORK = MAX(N-1,10), if FACT = 'F' and DICO = 'D';
C             LDWORK = 8*N,         if FACT = 'N'.
C
C     ZWORK   COMPLEX*16 array, dimension (LZWORK)
C             On exit, if INFO = 0, ZWORK(1) returns the optimal value
C             of LZWORK.
C             On exit, if INFO = -21, ZWORK(1) returns the minimum value
C             of LZWORK.
C
C     LZWORK  INTEGER
C             The dimension of the array ZWORK.
C             LZWORK >= MAX(1,3*N-3,2*N).
C             For good performance, LZWORK should be larger.
C
C             If LZWORK = -1, then a workspace query is assumed; the
C             routine only calculates the optimal size of the ZWORK
C             array, returns this value as the first entry of the ZWORK
C             array, and no error message related to LZWORK is issued by
C             XERBLA.
C
C     Error indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value;
C             = 4:  FACT = 'N' and the pencil A - lambda * E cannot be
C                   reduced to generalized Schur form: LAPACK routine
C                   ZGGES has failed to converge;
C             = 5:  DICO = 'C' and the pencil A - lambda * E is not
C                   c-stable;
C             = 6:  DICO = 'D' and the pencil A - lambda * E is not
C                   d-stable;
C             = 7:  the LAPACK routine ZSTEIN utilized to factorize M3
C                   failed to converge in the discrete-time case (see
C                   section METHOD for SLICOT Library routine SG03BS).
C                   This error is unlikely to occur.
C
C     METHOD
C
C     An extension [2] of Hammarling's method [1] to generalized
C     Lyapunov equations is utilized to solve (1) or (2).
C
C     First the pencil A - lambda * E is reduced to complex generalized
C     Schur form A_s - lambda * E_s by means of unitary transformations
C     (QZ-algorithm):
C
C        A_s = Q**H * A * Z   (upper triangular),                    (3)
C
C        E_s = Q**H * E * Z   (upper triangular).                    (4)
C
C     If the pencil A - lambda * E has already been factorized prior to
C     calling the routine, however, then the factors A_s, E_s, Q and Z
C     may be supplied and the initial factorization omitted.
C
C     Depending on the parameters TRANS and M, the N-by-N upper
C     triangular matrix B_s is defined as follows. In any case Q_B is
C     an M-by-M unitary matrix, which need not be accumulated.
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
C     3. If TRANS = 'C' and M < N, B_s is the upper triangular matrix
C        from the RQ-factorization
C
C                       ( Q_B  O )
C           (B_s  O ) * (        )  =  ( Q**H * B   O ).
C                       (  O   I )
C
C     4. If TRANS = 'C' and M >= N, B_s is the upper triangular matrix
C        from the (rectangular) RQ-factorization
C
C           ( B_s   O ) * Q_B  =  Q**H * B,
C
C        where O is the N-by-(M-N) zero matrix.
C
C     Assuming SCALE = 1, the transformation of A, E and B described
C     above leads to the reduced continuous-time equation
C
C                 H        H
C          op(A_s)  op(U_s)  op(U_s) op(E_s)
C
C                 H        H
C        + op(E_s)  op(U_s)  op(U_s) op(A_s)
C
C                    H
C        =  - op(B_s)  op(B_s)                                       (5)
C
C     or to the reduced discrete-time equation
C
C                 H        H
C          op(A_s)  op(U_s)  op(U_s) op(A_s)
C
C                 H        H
C        - op(E_s)  op(U_s)  op(U_s) op(E_s)
C
C                    H
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
C     The size of the (1,1)-blocks is 1-by-1.
C
C     We compute U11, U12**H, and U22 in three steps.
C
C     Step I:
C
C        From (5) and (7) we get the 1-by-1 equation
C
C                H      H                   H      H
C             A11  * U11  * U11 * E11  + E11  * U11  * U11 * A11
C
C                    H
C             = - B11  * B11.
C
C        For brevity, details are omitted here. See [2]. The technique
C        for computing U11 is similar to those applied to standard
C        Lyapunov equations in Hammarling's algorithm ([1], section 5).
C
C        Furthermore, the auxiliary scalars M1 and M2 defined as follows
C
C           M1 = A11 / E11 ,
C
C           M2 = B11 / E11 / U11 ,
C
C        are computed in a numerically reliable way.
C
C     Step II:
C
C        The generalized Sylvester equation
C
C              H      H      H      H
C           A22  * U12  + E22  * U12  * M1  =
C
C                H           H      H      H      H
C           - B12  * M2 - A12  * U11  - E12  * U11  * M1
C
C        is solved for U12**H, as a linear system of order N-1.
C
C     Step III:
C
C        It can be shown that
C
C              H      H                  H      H
C           A22  * U22  * U22 * E22 + E22  * U22  * U22 * A22  =
C
C                H              H
C           - B22  * B22 - y * y                                     (8)
C
C        holds, where y is defined as
C
C                  H        H      H      H      H
C           y = B12  - ( E12  * U11  + E22  * U12  ) * M2 .
C
C        If B22_tilde is the square triangular matrix arising from the
C        (rectangular) QR-factorization
C
C                       ( B22_tilde )     ( B22  )
C           Q_B_tilde * (           )  =  (      ),
C                       (     O     )     ( y**H )
C
C        where Q_B_tilde is a unitary matrix of order N, then
C
C                H              H                H
C           - B22  * B22 - y * y   =  - B22_tilde  * B22_tilde.
C
C        Replacing the right hand side in (8) by the term
C        - B22_tilde**H * B22_tilde leads to a reduced generalized
C        Lyapunov equation like (5), but of dimension N-1.
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
C        Q_U * U  =  U_s * Q**H     (if TRANS = 'N'),
C
C     or
C
C        U * Q_U  =  Z * U_s        (if TRANS = 'C'),
C
C     where Q_U is an N-by-N unitary matrix. Again, the unitary matrix
C     Q_U need not be accumulated.
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
C
C     CONTRIBUTOR
C
C     V. Sima, June 2021.
C
C     REVISIONS
C
C     V. Sima, July 2021, Oct. 2021 - Feb. 2022.
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
      COMPLEX*16        CONE, CZERO
      PARAMETER         ( CONE  = ( 1.0D+0, 0.0D+0 ),
     $                    CZERO = ( 0.0D+0, 0.0D+0 ) )
C     .. Scalar Arguments ..
      DOUBLE PRECISION  SCALE
      INTEGER           INFO, LDA, LDB, LDE, LDQ, LDZ, LZWORK, M, N
      CHARACTER         DICO, FACT, TRANS
C     .. Array Arguments ..
      COMPLEX*16        A(LDA,*), ALPHA(*), B(LDB,*), BETA(*), E(LDE,*),
     $                  Q(LDQ,*), Z(LDZ,*), ZWORK(*)
      DOUBLE PRECISION  DWORK(*)
C     .. Local Scalars ..
      DOUBLE PRECISION  BIGNMS, BIGNUM, EPS, MA, MATO, MB, MBTO, ME,
     $                  METO, MN, MX, SMLNUM, T, TMP
      INTEGER           BL, I, INFO1, J, K, L, MAXMN, MINMN, MINWRK, NC,
     $                  NR, OPTWRK
      LOGICAL           ISDISC, ISFACT, ISTRAN, LASCL, LBSCL, LESCL,
     $                  LQUERY, LSCL, NUNITQ, NUNITZ, SCALB
C     .. Local Arrays ..
      LOGICAL           BWORK(1)
C     .. External Functions ..
      DOUBLE PRECISION  DLAMCH, ZLANGE, ZLANTR
      LOGICAL           DELCTG, LSAME, MA02HZ
      EXTERNAL          DELCTG, DLAMCH, LSAME, MA02HZ, ZLANGE, ZLANTR
C     .. External Subroutines ..
      EXTERNAL          DLABAD, MB01UZ, SG03BS, SG03BT, XERBLA, ZCOPY,
     $                  ZDSCAL, ZGEMM, ZGEQRF, ZGERQF, ZGGES, ZLACGV,
     $                  ZLACPY, ZLASCL, ZLASET, ZSWAP
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, DBLE, INT, MAX, MIN, SIGN, SQRT
C     .. Executable Statements ..
C
C     Decode input parameters.
C
      ISDISC = LSAME( DICO,  'D' )
      ISFACT = LSAME( FACT,  'F' )
      ISTRAN = LSAME( TRANS, 'C' )
      LQUERY = LZWORK.EQ.-1
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
         MINWRK = MAX( 1, 2*N, 3*N-3 )
         MAXMN  = MAX( M, N )
         IF ( LQUERY ) THEN
            OPTWRK = MINWRK
            IF ( .NOT.ISFACT ) THEN
               CALL ZGGES( 'Vectors', 'Vectors', 'Not ordered', DELCTG,
     $                     N, A, LDA, E, LDE, I, ALPHA, BETA, Q, LDQ, Z,
     $                     LDZ, ZWORK, -1, DWORK, BWORK, INFO1 )
               OPTWRK = MAX( OPTWRK, INT( ZWORK(1) ) )
            END IF
            IF ( ISTRAN ) THEN
               CALL ZGERQF( N, MAXMN, B, LDB, ZWORK, ZWORK, -1, INFO1 )
            ELSE
               CALL ZGEQRF( MAXMN, N, B, LDB, ZWORK, ZWORK, -1, INFO1 )
            END IF
            OPTWRK = MAX( OPTWRK, INT( ZWORK(1) ) + N )
         ELSEIF ( LZWORK.LT.MINWRK ) THEN
            ZWORK(1) = MINWRK
            INFO = -21
         END IF
      END IF
C
      IF ( INFO.NE.0 ) THEN
         CALL XERBLA( 'SG03BZ', -INFO )
         RETURN
      ELSE IF ( LQUERY ) THEN
         ZWORK(1) = OPTWRK
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
      EPS    = DLAMCH( 'P' )
      SMLNUM = DLAMCH( 'S' )
      BIGNMS = ONE/SMLNUM
      CALL DLABAD( SMLNUM, BIGNMS )
      SMLNUM = SQRT( SMLNUM )/EPS
      BIGNUM = ONE/SMLNUM
C
      IF ( .NOT.ISFACT ) THEN
C
C        Reduce the pencil A - lambda * E to generalized Schur form.
C
C           A := Q**H * A * Z   (upper triangular),
C           E := Q**H * E * Z   (upper triangular).
C
C        The diagonal elements of E are non-negative real numbers.
C
C        Workspace:  complex >= MAX(1,2*N);  prefer larger;
C                       real  = 8*N.
C
         CALL ZGGES( 'Vectors', 'Vectors', 'Not ordered', DELCTG, N, A,
     $               LDA, E, LDE, I, ALPHA, BETA, Q, LDQ, Z, LDZ, ZWORK,
     $               LZWORK, DWORK, BWORK, INFO1 )
         IF ( INFO1.NE.0 ) THEN
            INFO = 4
            RETURN
         END IF
C
         OPTWRK = INT( ZWORK(1) )
C
      ELSE
C
C        Set the eigenvalues of the matrix pencil A - lambda * E.
C
         CALL ZCOPY( N, A, LDA+1, ALPHA, 1 )
         CALL ZCOPY( N, E, LDE+1, BETA,  1 )
         OPTWRK = MINWRK
      END IF
C
C     Check for identity matrices Q and/or Z.
C
      NUNITQ = .NOT.MA02HZ( 'All', N, N, CONE, Q, LDQ )
      NUNITZ = .NOT.MA02HZ( 'All', N, N, CONE, Z, LDZ )
C
C     Check on the stability of the matrix pencil A - lambda * E.
C
      IF ( ISDISC ) THEN
C
         DO 10 I = 1, N
            IF ( ABS( ALPHA(I) ).GE.DBLE( BETA(I) ) ) THEN
               INFO = 6
               RETURN
            END IF
   10    CONTINUE
C
      ELSE
C
         DO 20 I = 1, N
            IF ( ( ALPHA(I).EQ.CZERO ) .OR. ( BETA(I).EQ.CZERO ) .OR.
     $         ( SIGN( ONE, DBLE( ALPHA(I) ) )*
     $           SIGN( ONE, DBLE(  BETA(I) ) ).GE.ZERO ) ) THEN
               INFO = 5
               RETURN
            END IF
   20    CONTINUE
C
      END IF
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
      MA = MIN( ZLANTR( 'Max', 'Upper', 'NoDiag', N, N, A, LDA, DWORK ),
     $          BIGNMS )
      ME = MIN( ZLANTR( 'Max', 'Upper', 'NoDiag', N, N, E, LDE, DWORK ),
     $          BIGNMS )
C
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
     $   CALL ZLASCL( 'Upper', 0, 0, MA, MATO, N, N, A, LDA, INFO )
      IF ( LESCL )
     $   CALL ZLASCL( 'Upper', 0, 0, ME, METO, N, N, E, LDE, INFO )
      SCALB = MB.GT.BIGNMS
      MB    = MIN( MB, BIGNMS )
      IF ( LBSCL .AND. SCALB )
     $   CALL ZLASCL( 'Gen', 0, 0, MB, MBTO, K, L, B, LDB, INFO )
C
C     Transformation of the right hand side:
C
C        B := Q**H * B  or  B := B * Z.
C
C     Workspace:  need max(1,2*N);  prefer larger.
C
      IF ( ISTRAN ) THEN
C
         IF ( NUNITQ ) THEN
            NC = INT( LZWORK / N )
C
            DO 30 J = 1, M, NC
               BL = MIN( M-J+1, NC )
               CALL ZGEMM(  'ConjTrans', 'NoTrans', N, BL, N, CONE, Q,
     $                      LDQ, B(1,J), LDB, CZERO, ZWORK, N )
               CALL ZLACPY( 'All', N, BL, ZWORK, N, B(1,J), LDB )
   30       CONTINUE
C
         END IF
C
      ELSE
C
         IF ( NUNITQ ) THEN
            NR = INT( LZWORK / N )
C
            DO 40 I = 1, M, NR
               BL = MIN( M-I+1, NR )
               CALL ZGEMM(  TRANS, 'NoTrans', BL, N, N, CONE, B(I,1),
     $                      LDB, Z, LDZ, CZERO, ZWORK, BL )
               CALL ZLACPY( 'All', BL, N, ZWORK, BL, B(I,1), LDB )
   40       CONTINUE
C
         END IF
C
      END IF
C
C     Overwrite B with the triangular matrix of its RQ-factorization
C     or its QR-factorization. Then, do scaling, if it was postponed.
C     Make sure that the entries on the main diagonal are non-negative.
C
C     Workspace:  need max(1,MIN(M,N)+N);  prefer larger.
C
      MINMN = MIN( M, N )
      IF ( ISTRAN ) THEN
C
         CALL ZGERQF( N, M, B, LDB, ZWORK, ZWORK(N+1), LZWORK-N, INFO1 )
         IF ( N.GE.M ) THEN
            IF ( LBSCL .AND. .NOT.SCALB ) THEN
               CALL ZLASCL( 'Gen', 0, 0, MB, MBTO, N-M, M, B, LDB,
     $                      INFO )
               CALL ZLASCL( 'Upper', 0, 0, MB, MBTO, M, M, B(N-M+1,1),
     $                      LDB, INFO )
            END IF
            IF ( N.GT.M ) THEN
C
               DO 50 I = M, 1, -1
                  CALL ZCOPY( I+N-M, B(1,I), 1, B(1,I+N-M), 1 )
   50          CONTINUE
C
               CALL ZLASET( 'All', N, N-M, CZERO, CZERO, B, LDB )
            END IF
            IF ( M.GT.1 )
     $         CALL ZLASET( 'Lower', M-1, M-1, CZERO, CZERO,
     $                      B(N-M+2,N-M+1), LDB )
         ELSE
C
            DO  60 I = 1, N
               CALL ZCOPY( I, B(1,M-N+I), 1, B(1,I), 1 )
   60       CONTINUE
C
            IF ( LBSCL .AND. .NOT.SCALB )
     $         CALL ZLASCL( 'Upper', 0, 0, MB, MBTO, N, N, B, LDB,
     $                      INFO )
            IF ( N.GT.1 )
     $         CALL ZLASET( 'Lower', N-1, N-1, CZERO, CZERO, B(2,1),
     $                      LDB )
         END IF
C
         DO 70 I = N - MINMN + 1, N
            IF ( DBLE( B(I,I) ).LT.ZERO )
     $         CALL ZDSCAL( I, MONE, B(1,I), 1 )
   70    CONTINUE
C
      ELSE
C
         CALL ZGEQRF( M, N, B, LDB, ZWORK, ZWORK(N+1), LZWORK-N, INFO1 )
         IF ( LBSCL .AND. .NOT.SCALB )
     $      CALL ZLASCL( 'Upper', 0, 0, MB, MBTO, M, N, B, LDB, INFO )
         IF ( MAXMN.GT.1 )
     $      CALL ZLASET( 'Lower', MAXMN-1, MINMN, CZERO, CZERO, B(2,1),
     $                   LDB )
         IF ( N.GT.M )
     $      CALL ZLASET( 'All', N-M, N, CZERO, CZERO, B(M+1,1), LDB )
C
         DO 80 I = 1, MINMN
            IF ( DBLE( B(I,I) ).LT.ZERO )
     $         CALL ZDSCAL( N+1-I, MONE, B(I,I), LDB )
   80    CONTINUE
C
      END IF
C
C     Solve the reduced generalized Lyapunov equation.
C
C     Workspace:  complex MAX(3*N-3,0);
C                    real MAX(N-1,0),   if DICO = 'C';
C                         0,            if DICO = 'D' and N <= 1;
C                         MAX(N-1,10),  if DICO = 'D' and N >  1.
C
      IF ( ISDISC ) THEN
         CALL SG03BS( TRANS, N, A, LDA, E, LDE, B, LDB, SCALE, DWORK,
     $                ZWORK, INFO1 )
         IF ( INFO1.NE.0 ) THEN
            IF ( INFO1.EQ.3 )
     $         INFO = 6
            IF ( INFO1.EQ.4 )
     $         INFO = 7
            RETURN
         END IF
      ELSE
         CALL SG03BT( TRANS, N, A, LDA, E, LDE, B, LDB, SCALE, DWORK,
     $                ZWORK, INFO1 )
         IF ( INFO1.NE.0 ) THEN
            IF ( INFO1.EQ.3 )
     $         INFO = 5
            RETURN
         END IF
      END IF
C
C     Transform the solution matrix back, if Z and/or Q are not unit:
C
C        U := Z * U  or  U := U * Q**H ( U**H := Q * U**H).
C
      IF ( ISTRAN ) THEN
C
         IF ( NUNITZ ) THEN
C
C           Workspace:  max(1,N);  prefer larger.
C
            CALL MB01UZ( 'Right', 'Upper', 'NoTrans', N, N, CONE, B,
     $                   LDB, Z, LDZ, ZWORK, LZWORK, INFO )
C
C           Overwrite U with the triangular matrix of its
C           RQ-factorization and make the entries on the main diagonal
C           non-negative.
C
C           Workspace:  >= max(1,2*N);  prefer larger.
C
            CALL ZGERQF( N, N, B, LDB, ZWORK, ZWORK(N+1), LZWORK-N,
     $                   INFO1 )
            IF ( N.GT.1 )
     $         CALL ZLASET( 'Lower', N-1, N-1, CZERO, CZERO, B(2,1),
     $                      LDB )
C
            DO 90 I = 1, N
               IF ( DBLE( B(I,I) ).LT.ZERO )
     $            CALL ZDSCAL( I, MONE, B(1,I), 1 )
   90       CONTINUE
C
         END IF
C
      ELSE
C
         IF ( NUNITQ ) THEN
C
C           Workspace:  max(1,N);  prefer larger.
C
            CALL MB01UZ( 'Right', 'Upper', 'CTrans', N, N, CONE, B, LDB,
     $                   Q, LDQ, ZWORK, LZWORK, INFO )
C
            DO 100 I = 1, N
               CALL ZSWAP( I, B(I,1), LDB, B(1,I), 1 )
  100       CONTINUE
C
            DO 110 I = 1, N
               CALL ZLACGV( N, B(1,I), 1 )
  110       CONTINUE
C
C           Overwrite U with the triangular matrix of its
C           QR-factorization and make the entries on the main diagonal
C           non-negative.
C
C           Workspace:  >= max(1,2*N);  prefer larger.
C
            CALL ZGEQRF( N, N, B, LDB, ZWORK, ZWORK(N+1), LZWORK-N,
     $                   INFO1 )
            IF ( N.GT.1 )
     $         CALL ZLASET( 'Lower', N-1, N-1, CZERO, CZERO, B(2,1),
     $                      LDB )
C
            DO 120 I = 1, N
               IF ( DBLE( B(I,I) ).LT.ZERO )
     $            CALL ZDSCAL( N+1-I, MONE, B(I,I), LDB )
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
         CALL ZLASCL( 'Upper', 0, 0, MATO, MA, N, N, A, LDA, INFO )
         TMP = SQRT( MATO/MA )
      END IF
      IF ( LESCL ) THEN
         CALL ZLASCL( 'Upper', 0, 0, METO, ME, N, N, E, LDE, INFO )
         TMP = TMP*SQRT( METO/ME )
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
      CALL ZLASCL( 'Upper', 0, 0, MBTO, TMP, N, N, B, LDB, INFO )
C
      OPTWRK = MAX( OPTWRK, INT( ZWORK(N+1) ) + N )
C
      ZWORK(1) = DBLE( MAX( OPTWRK, MINWRK ) )
      RETURN
C *** Last line of SG03BZ ***
      END
