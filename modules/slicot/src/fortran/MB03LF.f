      SUBROUTINE MB03LF( COMPQ, COMPU, ORTH, N, Z, LDZ, B, LDB, FG,
     $                   LDFG, NEIG, Q, LDQ, U, LDU, ALPHAR, ALPHAI,
     $                   BETA, IWORK, LIWORK, DWORK, LDWORK, BWORK,
     $                   IWARN, INFO )
C
C     PURPOSE
C
C     To compute the relevant eigenvalues of a real N-by-N skew-
C     Hamiltonian/Hamiltonian pencil aS - bH, with
C
C                                   (  B  F  )      (  0  I  )
C       S = T Z = J Z' J' Z and H = (        ), J = (        ),      (1)
C                                   (  G -B' )      ( -I  0  )
C
C     where the notation M' denotes the transpose of the matrix M.
C     Optionally, if COMPQ = 'C', an orthogonal basis of the right
C     deflating subspace of aS - bH corresponding to the eigenvalues
C     with strictly negative real part is computed. Optionally, if
C     COMPU = 'C', an orthonormal basis of the companion subspace,
C     range(P_U) [1], which corresponds to the eigenvalues with strictly
C     negative real part, is computed.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     COMPQ   CHARACTER*1
C             Specifies whether to compute the right deflating subspace
C             corresponding to the eigenvalues of aS - bH with strictly
C             negative real part.
C             = 'N':  do not compute the deflating subspace;
C             = 'C':  compute the deflating subspace and store it in the
C                     leading subarray of Q.
C
C     COMPU   CHARACTER*1
C             Specifies whether to compute the companion subspace
C             corresponding to the eigenvalues of aS - bH with strictly
C             negative real part.
C             = 'N': do not compute the companion subspace;
C             = 'C': compute the companion subspace and store it in the
C                    leading subarray of U.
C
C     ORTH    CHARACTER*1
C             If COMPQ = 'C' and/or COMPU = 'C', specifies the technique
C             for computing the orthogonal basis of the deflating
C             subspace, and/or of the companion subspace, as follows:
C             = 'P':  QR factorization with column pivoting;
C             = 'S':  singular value decomposition.
C             If COMPQ = 'N' and COMPU = 'N', the ORTH value is not
C             used.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the pencil aS - bH.  N >= 0, even.
C
C     Z       (input/output) DOUBLE PRECISION array, dimension (LDZ, N)
C             On entry, the leading N-by-N part of this array must
C             contain the non-trivial factor Z in the factorization
C             S = J Z' J' Z of the skew-Hamiltonian matrix S.
C             On exit, if COMPQ = 'C' or COMPU = 'C', the leading
C             N-by-N part of this array contains the transformed upper
C                               ~
C             triangular matrix Z11 (see METHOD), after moving the
C             eigenvalues with strictly negative real part to the top
C             of the pencil (3). The strictly lower triangular part is
C             not zeroed.
C             If COMPQ = 'N' and COMPU = 'N', the leading N-by-N part of
C             this array contains the matrix Z obtained by the SLICOT
C             Library routine MB04AD just before the application of the
C             periodic QZ algorithm. The elements of the (2,1) block,
C             i.e., in the rows N/2+1 to N and in the columns 1 to N/2
C             are not set to zero, but are unchanged on exit.
C
C     LDZ     INTEGER
C             The leading dimension of the array Z.  LDZ >= MAX(1, N).
C
C     B       (input) DOUBLE PRECISION array, dimension (LDB, N/2)
C             On entry, the leading N/2-by-N/2 part of this array must
C             contain the matrix B.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= MAX(1, N/2).
C
C     FG      (input) DOUBLE PRECISION array, dimension (LDFG, N/2+1)
C             On entry, the leading N/2-by-N/2 lower triangular part of
C             this array must contain the lower triangular part of the
C             symmetric matrix G, and the N/2-by-N/2 upper triangular
C             part of the submatrix in the columns 2 to N/2+1 of this
C             array must contain the upper triangular part of the
C             symmetric matrix F.
C
C     LDFG    INTEGER
C             The leading dimension of the array FG.
C             LDFG >= MAX(1, N/2).
C
C     NEIG    (output) INTEGER
C             If COMPQ = 'C' or COMPU = 'C', the number of eigenvalues
C             in aS - bH with strictly negative real part.
C
C     Q       (output) DOUBLE PRECISION array, dimension (LDQ, 2*N)
C             On exit, if COMPQ = 'C', the leading N-by-NEIG part of
C             this array contains an orthogonal basis of the right
C             deflating subspace corresponding to the eigenvalues of
C             aS - bH with strictly negative real part. The remaining
C             part of this array is used as workspace.
C             If COMPQ = 'N', this array is not referenced.
C
C     LDQ     INTEGER
C             The leading dimension of the array Q.
C             LDQ >= 1,           if COMPQ = 'N';
C             LDQ >= MAX(1, 2*N), if COMPQ = 'C'.
C
C     U       (output) DOUBLE PRECISION array, dimension (LDU, 2*N)
C             On exit, if COMPU = 'C', the leading N-by-NEIG part of
C             this array contains an orthogonal basis of the companion
C             subspace corresponding to the eigenvalues of aS - bH with
C             strictly negative real part. The remaining part of this
C             array is used as workspace.
C             If COMPU = 'N', this array is not referenced.
C
C     LDU     INTEGER
C             The leading dimension of the array U.
C             LDU >= 1,         if COMPU = 'N';
C             LDU >= MAX(1, N), if COMPU = 'C'.
C
C     ALPHAR  (output) DOUBLE PRECISION array, dimension (N/2)
C             The real parts of each scalar alpha defining an eigenvalue
C             of the pencil aS - bH.
C
C     ALPHAI  (output) DOUBLE PRECISION array, dimension (N/2)
C             The imaginary parts of each scalar alpha defining an
C             eigenvalue of the pencil aS - bH.
C             If ALPHAI(j) is zero, then the j-th eigenvalue is real.
C
C     BETA    (output) DOUBLE PRECISION array, dimension (N/2)
C             The scalars beta that define the eigenvalues of the pencil
C             aS - bH.
C             Together, the quantities alpha = (ALPHAR(j),ALPHAI(j)) and
C             beta = BETA(j) represent the j-th eigenvalue of the pencil
C             aS - bH, in the form lambda = alpha/beta. Since lambda may
C             overflow, the ratios should not, in general, be computed.
C             Due to the skew-Hamiltonian/Hamiltonian structure of the
C             pencil, for every eigenvalue lambda, -lambda is also an
C             eigenvalue, and thus it has only to be saved once in
C             ALPHAR, ALPHAI and BETA.
C             Specifically, only eigenvalues with imaginary parts
C             greater than or equal to zero are stored; their conjugate
C             eigenvalues are not stored. If imaginary parts are zero
C             (i.e., for real eigenvalues), only positive eigenvalues
C             are stored. The remaining eigenvalues have opposite signs.
C             As a consequence, pairs of complex eigenvalues, stored in
C             consecutive locations, are not complex conjugate.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (LIWORK)
C             On exit, if INFO = -20, IWORK(1) returns the minimum value
C             of LIWORK.
C
C     LIWORK  INTEGER
C             The dimension of the array IWORK.
C             LIWORK >= N + 18,      if COMPQ = 'N' and COMPU = 'N';
C             LIWORK >= MAX( 2*N+1, 48 ), otherwise.
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal value
C             of LDWORK.
C             On exit, if INFO = -22, DWORK(1) returns the minimum value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The dimension of the array DWORK.
C             LDWORK >= b*N*N + 3*N*N/2 + MAX( 6*N, 54 ),
C                                       if COMPQ = 'N' and COMPU = 'N';
C             LDWORK >= d*N*N + MAX( N/2+252, 432 ), otherwise, where
C                       b = a,   d = c,   if COMPU = 'N',
C                       b = a+1, d = c+1, if COMPU = 'C', and
C                       a = 2,   c = 7,   if COMPQ = 'N',
C                       a = 4,   c = 10,  if COMPQ = 'C'.
C             For good performance LDWORK should be generally larger.
C
C             If LDWORK = -1  a workspace query is assumed; the
C             routine only calculates the optimal size of the DWORK
C             array, returns this value as the first entry of the DWORK
C             array, and no error message is issued by XERBLA.
C
C     BWORK   LOGICAL array, dimension (N/2)
C
C     Warning Indicator
C
C     IWARN   INTEGER
C             = 0: no warning;
C             = 1: some eigenvalues might be unreliable. More details
C                  can be obtained by running the SLICOT routine MB04AD.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0: succesful exit;
C             < 0: if INFO = -i, the i-th argument had an illegal value;
C             = 1: periodic QZ iteration failed in the SLICOT Library
C                  routines MB04AD, MB04CD or MB03BB (QZ iteration did
C                  not converge or computation of the shifts failed);
C             = 2: standard QZ iteration failed in the SLICOT Library
C                  routines MB04CD or MB03CD (called by MB03ID);
C             = 3: a numerically singular matrix was found in the SLICOT
C                  Library routine MB03GD (called by MB03ID);
C             = 4: the singular value decomposition failed in the LAPACK
C                  routine DGESVD (for ORTH = 'S').
C
C     METHOD
C
C     First, the decompositions of S and H are computed via orthogonal
C     matrices Q1 and Q2 and orthogonal symplectic matrices U1 and U2,
C     such that
C
C                                   ( T11  T12 )
C       Q1' T U1 = Q1' J Z' J' U1 = (          ),
C                                   (  0   T22 )
C
C                  ( Z11  Z12 )
C       U2' Z Q2 = (          ),                                     (2)
C                  (  0   Z22 )
C
C                  ( H11  H12 )
C       Q1' H Q2 = (          ),
C                  (  0   H22 )
C
C     where T11, T22', Z11, Z22', H11 are upper triangular and H22' is
C     upper quasi-triangular.
C
C     Then, orthogonal matrices Q3, Q4 and U3 are found, for the
C     matrices
C
C       ~     ( T22'  0  )  ~     ( T11'  0  )  ~   (   0   H11 )
C       Z11 = (          ), Z22 = (          ), H = (           ),
C             (  0   Z11 )        (  0   Z22 )      ( -H22'  0  )
C
C               ~          ~       ~          ~
C     such that Z11 := U3' Z11 Q4, Z22 := U3' Z22 Q3 are upper
C                    ~          ~
C     triangular and H11 := Q3' H Q4 is upper quasi-triangular. The
C     following matrices are computed:
C
C       ~          ( -T12'  0  )        ~          (  0   H12 )
C       Z12 := U3' (           ) Q3 and H12 := Q3' (          ) Q3.
C                  (  0    Z12 )                   ( H12'  0  )
C
C     Then, an orthogonal matrix Q and an orthogonal symplectic matrix U
C     are found such that the eigenvalues with strictly negative real
C     parts of the pencil
C
C             ~    ~          ~    ~           ~    ~
C           ( Z11  Z12 )'   ( Z11  Z12 )     ( H11  H12  )
C       a J (      ~   ) J' (      ~   ) - b (      ~    )           (3)
C           (  0   Z22 )    (  0   Z22 )     (  0  -H11' )
C
C     are moved to the top of this pencil.
C
C     Finally, an orthogonal basis of the right deflating subspace
C     and an orthogonal basis of the companion subspace corresponding to
C     the eigenvalues with strictly negative real part are computed.
C     See also page 11 in [1] for more details.
C
C     REFERENCES
C
C     [1] Benner, P., Byers, R., Losse, P., Mehrmann, V. and Xu, H.
C         Numerical Solution of Real Skew-Hamiltonian/Hamiltonian
C         Eigenproblems.
C         Tech. Rep., Technical University Chemnitz, Germany,
C         Nov. 2007.
C
C     NUMERICAL ASPECTS
C                                                               3
C     The algorithm is numerically backward stable and needs O(N )
C     floating point operations.
C
C     FURTHER COMMENTS
C
C     This routine does not perform any scaling of the matrices. Scaling
C     might sometimes be useful, and it should be done externally.
C
C     CONTRIBUTOR
C
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     Jan. 2011.
C
C     REVISIONS
C
C     V. Sima, Feb. 2011, Aug. 2011, Nov. 2011, Oct. 2012, July 2013,
C     July 2014, May 2020.
C     M. Voigt, Jan. 2012, July 2013.
C
C     KEYWORDS
C
C     Deflating subspace, embedded pencil, skew-Hamiltonian/Hamiltonian
C     pencil, structured Schur form.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TWO = 2.0D+0 )
C
C     .. Scalar Arguments ..
      CHARACTER          COMPQ, COMPU, ORTH
      INTEGER            INFO, IWARN, LDB, LDFG, LDQ, LDU, LDWORK, LDZ,
     $                   LIWORK, N, NEIG
C
C     .. Array Arguments ..
      LOGICAL            BWORK( * )
      INTEGER            IWORK( * )
      DOUBLE PRECISION   ALPHAI( * ), ALPHAR( * ), B( LDB, * ),
     $                   BETA( * ), DWORK( * ), FG( LDFG, * ),
     $                   Q( LDQ, * ), U( LDU, * ), Z( LDZ, * )
C
C     .. Local Scalars ..
      LOGICAL            LCMP, LCMPQ, LCMPU, LQUERY, QR, QRP, SVD
      CHARACTER*14       CMPI, CMQI, CMUI, JOB
      INTEGER            I, IH, IH12, IQ1, IQ2, IQ3, IQ4, IS, IT, IU11,
     $                   IU12, IU21, IU22, IU3, IW, IWRK, IZ12, J, M,
     $                   MINDB, MINDW, MINIW, MM, MP1, N2, NM, NMM, NN,
     $                   NP1, OPTDW
C
C     .. Local Arrays ..
      DOUBLE PRECISION   DUM( 7 )
C
C     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH, LSAME
C
C     .. External Subroutines ..
      EXTERNAL           DAXPY, DCOPY, DGEMM, DGEQP3, DGEQRF, DGESVD,
     $                   DLACPY, DORGQR, DSWAP, MA02ED, MB03ID, MB04AD,
     $                   MB04CD, XERBLA
C
C     .. Intrinsic Functions ..
      INTRINSIC          INT, MAX, MOD, SQRT
C
C     .. Executable Statements ..
C
C     Decode the input arguments.
C     Using ORTH = 'Q' is not safe, but sometimes gives better results.
C
      M  = N/2
      N2 = N*2
      NN = N*N
C
      NEIG  = 0
      LCMPQ = LSAME( COMPQ, 'C' )
      LCMPU = LSAME( COMPU, 'C' )
      LCMP  = LCMPQ .OR. LCMPU
      IF( LCMP ) THEN
         QR    = LSAME( ORTH, 'Q' )
         QRP   = LSAME( ORTH, 'P' )
         SVD   = LSAME( ORTH, 'S' )
         MINIW = MAX( N2 + 1, 48 )
      ELSE
         QR    = .FALSE.
         QRP   = .FALSE.
         SVD   = .FALSE.
         MINIW = N + 18
      END IF
      IF( N.EQ.0 ) THEN
         MINIW = 1
         MINDW = 1
      ELSE
         IF( LCMPQ ) THEN
            I = 4
            J = 10
         ELSE
            I = 2
            J = 7
         END IF
         IF( LCMPU ) THEN
            I = I + 1
            J = J + 1
         END IF
         MINDB = I*NN
         IF( LCMP ) THEN
            MINDW = J*NN + MAX( M + 252, 432 )
         ELSE
            MINDW = MINDB + 3*( NN/2 ) + MAX( 6*N, 54 )
         END IF
      END IF
      LQUERY = LDWORK.EQ.-1
C
C     Test the input arguments.
C
      INFO = 0
      IF( .NOT.( LSAME( COMPQ, 'N' ) .OR. LCMPQ ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( LSAME( COMPU, 'N' ) .OR. LCMPU ) ) THEN
         INFO = -2
      ELSE IF( LCMP .AND. .NOT. ( QR .OR. QRP .OR. SVD ) ) THEN
         INFO = -3
      ELSE IF( N.LT.0 .OR. MOD( N, 2 ).NE.0 ) THEN
         INFO = -4
      ELSE IF(  LDZ.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF(  LDB.LT.MAX( 1, M ) ) THEN
         INFO = -8
      ELSE IF( LDFG.LT.MAX( 1, M ) ) THEN
         INFO = -10
      ELSE IF(  LDQ.LT.1 .OR. ( LCMPQ .AND. LDQ.LT.N2 ) ) THEN
         INFO = -13
      ELSE IF(  LDU.LT.1 .OR. ( LCMPU .AND. LDU.LT.N  ) ) THEN
         INFO = -15
      ELSE IF( LIWORK.LT.MINIW ) THEN
         IWORK( 1 ) = MINIW
         INFO = -20
      ELSE IF( .NOT.LQUERY ) THEN
         IF( LDWORK.LT.MINDW ) THEN
            DWORK( 1 ) = MINDW
            INFO = -22
         END IF
      END IF
C
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'MB03LF', -INFO )
         RETURN
      ELSE IF( N.GT.0 ) THEN
C
C        Compute optimal workspace.
C
         CMPI = 'Initialize'
         IF( LCMPQ ) THEN
            CMQI = CMPI
         ELSE
            CMQI = 'No Computation'
         END IF
C
         IF( LCMPU ) THEN
            CMUI = CMPI
         ELSE
            CMUI = 'No Computation'
         END IF
C
         IF( LCMP ) THEN
            JOB = 'Triangularize'
         ELSE
            JOB = 'Eigenvalues'
         END IF
C
         IF( LQUERY ) THEN
            CALL MB04AD( JOB, CMQI, CMQI, CMUI, CMUI, N, DWORK, N,
     $                   DWORK, N, DWORK, N, DWORK, N, DWORK, M, DWORK,
     $                   M, DWORK, M, DWORK, M, DWORK, N, DWORK, DWORK,
     $                   DWORK, IWORK, LIWORK, DUM, -1, INFO )
C
            IF( LCMP ) THEN
               IW    = MINDB
               MINDB = MINDB + 2*NN
               IF( LCMPQ ) THEN
                  IW    = IW    + NN
                  MINDB = MINDB + NN
               ELSE
                  IW    = 0
               END IF
               CALL MB04CD( CMQI, CMPI, CMPI, N, DWORK, N, DWORK, N,
     $                      DWORK, N, DWORK, N, DWORK, N, DWORK, N,
     $                      IWORK, LIWORK, DUM( 2 ), -1, BWORK, INFO )
               IF( SVD ) THEN
                  CALL DGESVD( 'O', 'N', N, N, Q, LDQ, DWORK, DWORK,
     $                         LDQ, DWORK, 1, DUM( 3 ), -1, INFO )
                  J = N + INT( DUM( 3 ) )
               ELSE
                  I = MAX( LDQ, LDU )
                  IF( QR ) THEN
                     CALL DGEQRF( N, M, Q, I, DWORK, DUM( 3 ), -1,
     $                            INFO )
                     J = M
                  ELSE
                     CALL DGEQP3( N, N, Q, I, IWORK, DWORK, DUM( 3 ),
     $                            -1, INFO )
                     J = N
                  END IF
                  CALL DORGQR( N, J, J, Q, I, DWORK, DUM( 4 ), -1,
     $                         INFO )
                  J = J + IW + MAX( INT( DUM( 3 ) ), INT( DUM( 4 ) ) )
               END IF
               OPTDW = MAX( MINDB + INT( DUM( 2 ) ), J )
            ELSE
               OPTDW = 0
            END IF
            OPTDW = MAX( MINDW, OPTDW, MINDB + INT( DUM( 1 ) ) )
            DWORK( 1 ) = OPTDW
            RETURN
         END IF
      END IF
C
C     Quick return if possible.
C
      IWARN = 0
      IF( N.EQ.0 ) THEN
         DWORK( 1 ) = ONE
         RETURN
      END IF
C
C     STEP 1: Apply MB04AD to compute the generalized symplectic
C             URV decomposition.
C
C     Set the pointers for the inputs and outputs of MB04AD.
C
C     Real workspace:     need   w1 + w, where
C                         w1 = 2*N**2, if COMPQ = 'N' and COMPU = 'N';
C                         w1 = 3*N**2, if COMPQ = 'N' and COMPU = 'C';
C                         w1 = 4*N**2, if COMPQ = 'C' and COMPU = 'N';
C                         w1 = 5*N**2, if COMPQ = 'C' and COMPU = 'C';
C                         w  = 3/2*N**2+MAX(6*N, 54), if  COMPQ = 'N'
C                                                     and COMPU = 'N';
C                         w  =   3*N**2+MAX(6*N, 54), otherwise;
C                         prefer larger.
C     Integer workspace:  need   N + 18.
C
      MM  = M*M
      NM  = N*M
      NMM = NM + M
      NP1 = N  + 1
      MP1 = M  + 1
      IQ1 = 1
      IF( LCMPQ ) THEN
         IQ2  = IQ1 + NN
         IU11 = IQ2 + NN
         IH   = IU11
      ELSE
         IQ2  = 1
         IU11 = 1
         IH   = 1
      END IF
C
      IF( LCMPU ) THEN
         IU12 = IU11 + MM
         IU21 = IU12 + MM
         IU22 = IU21 + MM
         IH   = IU22 + MM
      ELSE
         IU12 = 1
         IU21 = 1
         IU22 = 1
      END IF
C
C     Build the matrix H.
C
      IW = IH
      IS = IH + M + N
      DO 10 J = 1, M
         CALL DCOPY( M, B( 1, J ), 1, DWORK( IW ), 1 )
         IW = IW + M + J - 1
         CALL DCOPY( M-J+1, FG( J, J ),  1, DWORK( IW ), 1 )
         CALL DCOPY( M-J, DWORK( IW+1 ), 1, DWORK( IS ), N )
         IW = IW + MP1 - J
         IS = IS + NP1
   10 CONTINUE
C
      IW = IH + NM
      IS = IW
      DO 30 J = 1, M
         CALL DCOPY( J, FG( 1, J+1 ),  1, DWORK( IW ), 1 )
         CALL DCOPY( J-1, DWORK( IW ), 1, DWORK( IS ), N )
         IW = IW + M
         IS = IS + 1
         DO 20 I = 1, M
            DWORK( IW ) = -B( J, I )
            IW = IW + 1
   20    CONTINUE
   30 CONTINUE
C
      IT   = IH + NN
      IWRK = IT + NN
C
      CALL MB04AD( JOB, CMQI, CMQI, CMUI, CMUI, N, Z, LDZ, DWORK( IH ),
     $             N, DWORK( IQ1 ), N, DWORK( IQ2 ), N, DWORK( IU11 ),
     $             M, DWORK( IU12 ), M, DWORK( IU21 ), M, DWORK( IU22 ),
     $             M, DWORK( IT ), N, ALPHAR, ALPHAI, BETA, IWORK,
     $             LIWORK, DWORK( IWRK ), LDWORK-IWRK+1, INFO )
C
      IF( INFO.EQ.3 ) THEN
         IWARN = 1
      ELSE IF( INFO.GT.0 ) THEN
         INFO = 1
         RETURN
      END IF
      OPTDW = MAX( MINDW, INT( DWORK( IWRK ) ) + IWRK - 1 )
C
      IF( .NOT.LCMP ) THEN
         DWORK( 1 ) = OPTDW
         RETURN
      END IF
C                                                    ~    ~        ~
C     STEP 2: Build the needed parts of the matrices Z11, Z22' and H,
C     and compute the transformed matrices and the orthogonal matrices
C     Q3, Q4 and U3. (Q4 might not be required.)
C
C     Real workspace:     need   w1 + w2 + 3*N*N + MAX( M+252, 432 ),
C                                w2 = 2*N**2, if COMPQ = 'N';
C                                w2 = 3*N**2, if COMPQ = 'C';
C                         prefer larger.
C     Integer workspace:  need   MAX( N/2+1, 48 ).
C     Logical workspace:  need   M.
C
C     Save Z12, T12, and H12, since they are overwritten by MB04CD.
C
      IW = IT + NM
      IS = IH + NM
      IF( LCMPU ) THEN
         DO 40 J = 1, M
            CALL DCOPY( M, Z( 1, M+J ), 1, U(   1, J ), 1 )
            CALL DCOPY( M, DWORK( IW ), 1, U( MP1, J ), 1 )
            IW = IW + N
   40    CONTINUE
         DO 50 J = 1, M
            CALL DCOPY( M, DWORK( IS ), 1, U( 1, M+J ), 1 )
            IS = IS + N
   50    CONTINUE
      ELSE
         DO 60 J = 1, M
            CALL DCOPY( M, Z( 1, M+J ), 1, Q(   1, J ), 1 )
            CALL DCOPY( M, DWORK( IW ), 1, Q( MP1, J ), 1 )
            IW = IW + N
   60    CONTINUE
         DO 70 J = 1, M
            CALL DCOPY( M, DWORK( IS ), 1, Q( 1, M+J ), 1 )
            IS = IS + N
   70    CONTINUE
      END IF
C
      IU3 = IWRK
      IQ3 = IU3 + NN
      IQ4 = IQ3 + NN
      IF( LCMPQ ) THEN
         IWRK = IQ4 + NN
      ELSE
         IWRK = IQ4
      END IF
C
      IW = IH
      DO 90 J = 1, M
         IS = IW + NM
         CALL DCOPY( M, DWORK( IW ), 1, DWORK( IS ), 1 )
         IW = IW + M
         IS = IH + NMM + J - 1
         DO 80 I = 1, M
            DWORK( IW ) = -DWORK( IS )
            IW = IW + 1
            IS = IS + N
   80    CONTINUE
   90 CONTINUE
C
      IS = IT + NMM
      DO 100 J = 1, M
         CALL DSWAP( M, Z( 1, J ), 1, Z( MP1, M+J ), 1 )
         CALL DSWAP( M, Z( 1, J ), 1, DWORK( IS ), N )
         IS = IS + 1
  100 CONTINUE
C
      CALL MB04CD( CMQI, CMPI, CMPI, N, DWORK( IT ), N, Z, LDZ,
     $             DWORK( IH ), N, DWORK( IQ4 ), N, DWORK( IU3 ), N,
     $             DWORK( IQ3 ), N, IWORK, LIWORK, DWORK( IWRK ),
     $             LDWORK-IWRK+1, BWORK, INFO )
      IF( INFO.GT.0 ) THEN
         IF( INFO.GT.2 )
     $      INFO = 2
         RETURN
      END IF
      OPTDW = MAX( OPTDW, INT( DWORK( IWRK ) ) + IWRK - 1 )
C
C                     ~                                    ~
C     STEP 3: Compute Z12 and the upper triangular part of H12,
C     exploiting the structure.
C
C     Real workspace:     need   w1 + w2 + w3, where
C                                w3 = 2*N**2.
C
      IZ12 = IWRK
      IH12 = IZ12 + NN
      IWRK = IH12 + NN
C
C             ~           [ -T12'  0  ]                   [ Qa  Qc ]
C     Compute Z12 = U3' * [           ] * Q3, where Q3 =: [        ].
C                         [   0   Z12 ]                   [ Qb  Qd ]
C
C     Part of the arrays U or Q and DWORK(IH12) are used as workspace.
C
      IF( LCMPU ) THEN
         CALL DGEMM( 'Transpose', 'No Transpose', M, N, M, -ONE,
     $               U( MP1, 1 ), LDU, DWORK( IQ3 ), N, ZERO,
     $               DWORK( IH12 ), N )
         CALL DGEMM( 'No Transpose', 'No Transpose', M, N, M, ONE,
     $               U, LDU, DWORK( IQ3+M ), N, ZERO, DWORK( IH12+M ),
     $               N )
      ELSE
         CALL DGEMM( 'Transpose', 'No Transpose', M, N, M, -ONE,
     $               Q( MP1, 1 ), LDQ, DWORK( IQ3 ), N, ZERO,
     $               DWORK( IH12 ), N )
         CALL DGEMM( 'No Transpose', 'No Transpose', M, N, M, ONE,
     $               Q, LDQ, DWORK( IQ3+M ), N, ZERO, DWORK( IH12+M ),
     $               N )
      END IF
      CALL DGEMM( 'Transpose', 'No Transpose', N, N, N, ONE,
     $            DWORK( IU3 ), N, DWORK( IH12 ), N, ZERO,
     $            DWORK( IZ12 ), N )
C
C             ~           [  0   H12 ]
C     Compute H12 = Q3' * [          ] * Q3.
C                         [ H12'  0  ]
C
C     The (2,1) block of Z is used as workspace.
C
C     Compute Qb'*H12'*Qa + Qa'*H12*Qb.
C
      IF( LCMPU ) THEN
         CALL DGEMM( 'No Transpose', 'No Transpose', M, M, M, ONE,
     $               U( 1, MP1 ), LDU, DWORK( IQ3+M ), N, ZERO,
     $               Z( MP1, 1 ), LDZ )
      ELSE
         CALL DGEMM( 'No Transpose', 'No Transpose', M, M, M, ONE,
     $               Q( 1, MP1 ), LDQ, DWORK( IQ3+M ), N, ZERO,
     $               Z( MP1, 1 ), LDZ )
      END IF
      CALL DGEMM( 'Transpose', 'No Transpose', M, M, M, ONE,
     $            DWORK( IQ3 ), N, Z( MP1, 1 ), LDZ, ZERO,
     $            DWORK( IH12 ), N )
      IS = 0
      DO 110 J = 0, M - 1
         CALL DAXPY( J+1, ONE, DWORK( IH12+J ), N, DWORK( IH12+IS ), 1 )
         IS = IS + N
  110 CONTINUE
C
C     Compute Qb'*H12'*Qc + Qa'*H12*Qd.
C
      CALL DGEMM( 'Transpose', 'No Transpose', M, M, M, ONE,
     $            Z( MP1, 1 ), LDZ, DWORK( IQ3+NM ), N, ZERO,
     $            DWORK( IH12+NM ), N )
      IF( LCMPU ) THEN
         CALL DGEMM( 'No Transpose', 'No Transpose', M, M, M, ONE,
     $              U( 1, MP1 ), LDU, DWORK( IQ3+NMM ), N, ZERO,
     $              Z( MP1, 1 ), LDZ )
      ELSE
         CALL DGEMM( 'No Transpose', 'No Transpose', M, M, M, ONE,
     $              Q( 1, MP1 ), LDQ, DWORK( IQ3+NMM ), N, ZERO,
     $              Z( MP1, 1 ), LDZ )
      END IF
      CALL DGEMM( 'Transpose', 'No Transpose', M, M, M, ONE,
     $            DWORK( IQ3 ), N, Z( MP1, 1 ), LDZ, ONE,
     $            DWORK( IH12+NM ), N )
C
C     Compute Qd'*H12'*Qc + Qc'*H12*Qd.
C
      CALL DGEMM( 'Transpose', 'No Transpose', M, M, M, ONE,
     $            Z( MP1, 1 ), LDZ, DWORK( IQ3+NM ), N, ZERO,
     $            DWORK( IH12+NMM ), N )
      IS = 0
      DO 120 J = 0, M - 1
         CALL DAXPY( J+1, ONE, DWORK( IH12+NMM+J ), N,
     $               DWORK( IH12+NMM+IS ), 1 )
         IS = IS + N
  120 CONTINUE
C
C     STEP 4: Apply MB03ID to reorder the eigenvalues with strictly
C             negative real part to the top.
C
C     Real workspace:     need   w1 + w2 + w3 + w4;
C                                w4 = MAX(4*N+48,171), if COMPQ = 'N';
C                                w4 = MAX(8*N+48,171), if COMPQ = 'C'.
C     Integer workspace:  need   2*N + 1.
C
      CALL MA02ED( 'Upper', N, DWORK( IT ), N )
C
      CALL MB03ID( CMQI, CMUI, N2, Z, LDZ, DWORK( IT ), N,
     $             DWORK( IZ12 ), N, DWORK( IH ), N, DWORK( IH12 ), N,
     $             Q, LDQ, U, LDU, U( 1, NP1 ), LDU, NEIG, IWORK,
     $             LIWORK, DWORK( IWRK ), LDWORK-IWRK+1, INFO )
      IF( INFO.GT.0 )
     $   RETURN
C
      IF( QR )
     $   NEIG = NEIG/2
C
      IWRK = IZ12
C
      IF( LCMPQ ) THEN
C
C        STEP 5: Compute the deflating subspace corresponding to the
C                eigenvalues with strictly negative real part.
C
C        Real workspace:     need   w1 + w2 +   N**2, if ORTH = 'QR'.
C                                   w1 + w2 + 2*N**2, otherwise.
C
C        The workspace used before for storing H and T is reused.
C
C        Compute [ J*Q1*J' Q2 ].
C
         CALL DLACPY( 'Full', M, M, DWORK( IQ1+NMM ), N, DWORK( IH ),
     $                N )
         IW = IH  + M
         IS = IQ1 + NM
         DO 140 J = 1, M
            DO 130 I = 1, M
               DWORK( IW ) = -DWORK( IS )
               IW = IW + 1
               IS = IS + 1
  130       CONTINUE
            IW = IW + M
            IS = IS + M
  140    CONTINUE
C
         IW = IH  + NM
         IS = IQ1 + M
         DO 160 J = 1, M
            DO 150 I = 1, M
               DWORK( IW ) = -DWORK( IS )
               IW = IW + 1
               IS = IS + 1
  150       CONTINUE
            IW = IW + M
            IS = IS + M
  160    CONTINUE
         CALL DLACPY( 'Full', M, M, DWORK( IQ1 ), N, DWORK( IH+NMM ),
     $                N )
C
         CALL DLACPY( 'Full', N, N, DWORK( IQ2 ), N, DWORK( IT ), N )
C
C        Compute the first NEIG columns of P*[ Q4  0; 0 Q3 ]*Q.
C
         CALL DGEMM( 'No Transpose', 'No Transpose', M, NEIG, N, ONE,
     $               DWORK( IQ4 ), N, Q, LDQ, ZERO, DWORK( IWRK ), N2 )
         CALL DGEMM( 'No Transpose', 'No Transpose', M, NEIG, N, ONE,
     $               DWORK( IQ3 ), N, Q( NP1, 1 ), LDQ, ZERO,
     $               DWORK( IWRK+M ), N2 )
         CALL DGEMM( 'No Transpose', 'No Transpose', M, NEIG, N, ONE,
     $               DWORK( IQ4+M ), N, Q, LDQ, ZERO, DWORK( IWRK+N ),
     $               N2 )
         CALL DGEMM( 'No Transpose', 'No Transpose', M, NEIG, N, ONE,
     $               DWORK( IQ3+M ), N, Q( NP1, 1 ), LDQ, ZERO,
     $               DWORK( IWRK+N+M ), N2 )
C
C        Compute the deflating subspace.
C
         CALL DGEMM( 'No Transpose', 'No Transpose', N, NEIG, N2,
     $               SQRT( TWO )/TWO, DWORK( IH ), N, DWORK( IWRK ), N2,
     $               ZERO, Q, LDQ )
C
C        Orthogonalize the basis given in Q(1:n,1:neig).
C
         IF( LCMPU ) THEN
            IWRK = IQ3
         ELSE
            IWRK = NEIG + 1
         END IF
         IF( SVD ) THEN
C
C           Real workspace:     need   w5 + N + MAX(1,5*N);
C                                      w5 = 0,        if COMPU = 'N';
C                                      w5 = w1 + N*N, if COMPU = 'C'.
C                               prefer larger.
C
            CALL DGESVD( 'Overwrite', 'No V', N, NEIG, Q, LDQ, DWORK,
     $                   DWORK, 1,  DWORK, 1, DWORK( IWRK ),
     $                   LDWORK-IWRK+1, INFO )
            IF( INFO.GT.0 ) THEN
               INFO = 4
               RETURN
            END IF
            OPTDW = MAX( OPTDW, INT( DWORK( IWRK ) ) + IWRK - 1 )
            IF( .NOT.LCMPU )
     $         NEIG = NEIG/2
C
         ELSE
            IF( QR ) THEN
C
C              Real workspace:     need   w5 + N;
C                                  prefer w5 + M + M*NB, where NB is the
C                                                  optimal blocksize.
C
               CALL DGEQRF( N, NEIG, Q, LDQ, DWORK, DWORK( IWRK ),
     $                      LDWORK-IWRK+1, INFO )
            ELSE
C
C              Real workspace:     need   w5 + 4*N + 1;
C                                  prefer w5 + 3*N + (N+1)*NB.
C
               DO 170 J = 1, NEIG
                  IWORK( J ) = 0
  170          CONTINUE
               CALL DGEQP3( N, NEIG, Q, LDQ, IWORK, DWORK,
     $                      DWORK( IWRK ), LDWORK-IWRK+1, INFO )
            END IF
            OPTDW = MAX( OPTDW, INT( DWORK( IWRK ) ) + IWRK - 1 )
C
C           Real workspace:     need   w5 + 2*NEIG;
C                               prefer w5 + NEIG + NEIG*NB.
C
            CALL DORGQR( N, NEIG, NEIG, Q, LDQ, DWORK, DWORK( IWRK ),
     $                   LDWORK-IWRK+1, INFO )
            OPTDW = MAX( OPTDW, INT( DWORK( IWRK ) ) + IWRK - 1 )
            IF( QRP .AND. .NOT.LCMPU )
     $         NEIG = NEIG/2
         END IF
C
      END IF
C
      IF( LCMPU ) THEN
C
C        STEP 6: Compute the companion subspace corresponding to the
C                eigenvalues with strictly negative real part.
C
C        Real workspace:     need   w1 + w2 +   N**2, if ORTH = 'QR'.
C                                   w1 + w2 + 2*N**2, otherwise.
C
C        The workspace used before for storing H and T is reused.
C
C        Set [ U1 U2 ].
C
         CALL DLACPY( 'Full', M, N, DWORK( IU11 ), M, DWORK( IH ), N )
         IW = IH + M
         IS = IU12
         DO 190 J = 1, M
            DO 180 I = 1, M
               DWORK( IW ) = -DWORK( IS )
               IW = IW + 1
               IS = IS + 1
  180       CONTINUE
            IW = IW + M
  190    CONTINUE
         CALL DLACPY( 'Full', M, M, DWORK( IU11 ), M, DWORK( IH+NMM ),
     $                N )
         CALL DLACPY( 'Full', M, N, DWORK( IU21 ), M, DWORK( IT ), N )
         IW = IT + M
         IS = IU22
         DO 210 J = 1, M
            DO 200 I = 1, M
               DWORK( IW ) = -DWORK( IS )
               IW = IW + 1
               IS = IS + 1
  200       CONTINUE
            IW = IW + M
  210    CONTINUE
         CALL DLACPY( 'Full', M, M, DWORK( IU21 ), M, DWORK( IT+NMM ),
     $                N )
C
C        Compute the first NEIG columns of P*[ U3  0; 0 U3 ]*U.
C
         CALL DGEMM( 'No Transpose', 'No Transpose', M, NEIG, N, ONE,
     $               DWORK( IU3 ), N, U, LDU, ZERO, DWORK( IWRK ), N2 )
         CALL DGEMM( 'No Transpose', 'No Transpose', M, NEIG, N, -ONE,
     $               DWORK( IU3 ), N, U( 1, NP1 ), LDU, ZERO,
     $               DWORK( IWRK+M ), N2 )
         CALL DGEMM( 'No Transpose', 'No Transpose', M, NEIG, N, ONE,
     $               DWORK( IU3+M ), N, U, LDU, ZERO, DWORK( IWRK+N ),
     $               N2 )
         CALL DGEMM( 'No Transpose', 'No Transpose', M, NEIG, N, -ONE,
     $               DWORK( IU3+M ), N, U( 1, NP1 ), LDU, ZERO,
     $               DWORK( IWRK+N+M ), N2 )
C
C        Compute the companion subspace.
C
         CALL DGEMM( 'No Transpose', 'No Transpose', N, NEIG, N2,
     $               SQRT( TWO )/TWO, DWORK( IH ), N, DWORK( IWRK ), N2,
     $               ZERO, U, LDU )
C
C        Orthogonalize the basis given in U(1:n,1:neig).
C
         IWRK = NEIG + 1
         IF( SVD ) THEN
C
C           Real workspace:     need   N + MAX(1,5*N);
C                               prefer larger.
C
            CALL DGESVD( 'Overwrite', 'No V', N, NEIG, U, LDU, DWORK,
     $                   DWORK, 1,  DWORK, 1, DWORK( IWRK ),
     $                   LDWORK-IWRK+1, INFO )
            IF( INFO.GT.0 ) THEN
               INFO = 4
               RETURN
            END IF
            OPTDW = MAX( OPTDW, INT( DWORK( IWRK ) ) + IWRK - 1 )
            NEIG = NEIG/2
C
         ELSE
            IF( QR ) THEN
C
C              Real workspace:     need   N;
C                                  prefer M + M*NB, where NB is the
C                                                   optimal blocksize.
C
               CALL DGEQRF( N, NEIG, U, LDU, DWORK, DWORK( IWRK ),
     $                      LDWORK-IWRK+1, INFO )
            ELSE
C
C              Real workspace:     need   4*N + 1;
C                                  prefer 3*N + (N+1)*NB.
C
               DO 220 J = 1, NEIG
                  IWORK( J ) = 0
  220          CONTINUE
               CALL DGEQP3( N, NEIG, U, LDU, IWORK, DWORK,
     $                      DWORK( IWRK ), LDWORK-IWRK+1, INFO )
            END IF
            OPTDW = MAX( OPTDW, INT( DWORK( IWRK ) ) + IWRK - 1 )
C
C           Real workspace:     need   2*NEIG;
C                               prefer NEIG + NEIG*NB.
C
            CALL DORGQR( N, NEIG, NEIG, U, LDU, DWORK, DWORK( IWRK ),
     $                   LDWORK-IWRK+1, INFO )
            OPTDW = MAX( OPTDW, INT( DWORK( IWRK ) ) + IWRK - 1 )
            IF( QRP )
     $         NEIG = NEIG/2
         END IF
C
      END IF
C
      DWORK( 1 ) = OPTDW
      RETURN
C *** Last line of MB03LF ***
      END
