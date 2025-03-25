      SUBROUTINE MB03LD( COMPQ, ORTH, N, A, LDA, DE, LDDE, B, LDB, FG,
     $                   LDFG, NEIG, Q, LDQ, ALPHAR, ALPHAI, BETA,
     $                   IWORK, LIWORK, DWORK, LDWORK, BWORK, INFO )
C
C     PURPOSE
C
C     To compute the relevant eigenvalues of a real N-by-N skew-
C     Hamiltonian/Hamiltonian pencil aS - bH, with
C
C           (  A  D  )         (  B  F  )
C       S = (        ) and H = (        ),                           (1)
C           (  E  A' )         (  G -B' )
C
C     where the notation M' denotes the transpose of the matrix M.
C     Optionally, if COMPQ = 'C', an orthogonal basis of the right
C     deflating subspace of aS - bH corresponding to the eigenvalues
C     with strictly negative real part is computed.
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
C     ORTH    CHARACTER*1
C             If COMPQ = 'C', specifies the technique for computing an
C             orthogonal basis of the deflating subspace, as follows:
C             = 'P':  QR factorization with column pivoting;
C             = 'S':  singular value decomposition.
C             If COMPQ = 'N', the ORTH value is not used.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the pencil aS - bH.  N >= 0, even.
C
C     A       (input/output) DOUBLE PRECISION array, dimension
C                            (LDA, N/2)
C             On entry, the leading N/2-by-N/2 part of this array must
C             contain the matrix A.
C             On exit, if COMPQ = 'C', the leading N/2-by-N/2 part of
C             this array contains the upper triangular matrix Aout
C             (see METHOD); otherwise, it contains the upper triangular
C             matrix A obtained just before the application of the
C             periodic QZ algorithm (see SLICOT Library routine MB04BD).
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1, N/2).
C
C     DE      (input/output) DOUBLE PRECISION array, dimension
C                            (LDDE, N/2+1)
C             On entry, the leading N/2-by-N/2 lower triangular part of
C             this array must contain the lower triangular part of the
C             skew-symmetric matrix E, and the N/2-by-N/2 upper
C             triangular part of the submatrix in the columns 2 to N/2+1
C             of this array must contain the upper triangular part of the
C             skew-symmetric matrix D.
C             The entries on the diagonal and the first superdiagonal of
C             this array need not be set, but are assumed to be zero.
C             On exit, if COMPQ = 'C', the leading N/2-by-N/2 lower
C             triangular part and the first superdiagonal contain the
C             transpose of the upper quasi-triangular matrix C2out (see
C             METHOD), and the (N/2-1)-by-(N/2-1) upper triangular part
C             of the submatrix in the columns 3 to N/2+1 of this array
C             contains the strictly upper triangular part of the
C             skew-symmetric matrix Dout (see METHOD), without the main
C             diagonal, which is zero.
C             On exit, if COMPQ = 'N', the leading N/2-by-N/2 lower
C             triangular part and the first superdiagonal contain the
C             transpose of the upper Hessenberg matrix C2, and the
C             (N/2-1)-by-(N/2-1) upper triangular part of the submatrix
C             in the columns 3 to N/2+1 of this array contains the
C             strictly upper triangular part of the skew-symmetric
C             matrix D (without the main diagonal) just before the
C             application of the periodic QZ algorithm.
C
C     LDDE    INTEGER
C             The leading dimension of the array DE.
C             LDDE >= MAX(1, N/2).
C
C     B       (input/output) DOUBLE PRECISION array, dimension
C                            (LDB, N/2)
C             On entry, the leading N/2-by-N/2 part of this array must
C             contain the matrix B.
C             On exit, if COMPQ = 'C', the leading N/2-by-N/2 part of
C             this array contains the upper triangular matrix C1out
C             (see METHOD); otherwise, it contains the upper triangular
C             matrix C1 obtained just before the application of the
C             periodic QZ algorithm.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= MAX(1, N/2).
C
C     FG      (input/output) DOUBLE PRECISION array, dimension
C                            (LDFG, N/2+1)
C             On entry, the leading N/2-by-N/2 lower triangular part of
C             this array must contain the lower triangular part of the
C             symmetric matrix G, and the N/2-by-N/2 upper triangular
C             part of the submatrix in the columns 2 to N/2+1 of this
C             array must contain the upper triangular part of the
C             symmetric matrix F.
C             On exit, if COMPQ = 'C', the leading N/2-by-N/2 part of
C             the submatrix in the columns 2 to N/2+1 of this array
C             contains the matrix Vout (see METHOD); otherwise, it
C             contains the matrix V obtained just before the application
C             of the periodic QZ algorithm.
C
C     LDFG    INTEGER
C             The leading dimension of the array FG.
C             LDFG >= MAX(1, N/2).
C
C     NEIG    (output) INTEGER
C             If COMPQ = 'C', the number of eigenvalues in aS - bH with
C             strictly negative real part.
C
C     Q       (output) DOUBLE PRECISION array, dimension (LDQ, 2*N)
C             On exit, if COMPQ = 'C', the leading N-by-NEIG part of
C             this array contains an orthogonal basis of the right
C             deflating subspace corresponding to the eigenvalues of
C             aA - bB with strictly negative real part. The remaining
C             part of this array is used as workspace.
C             If COMPQ = 'N', this array is not referenced.
C
C     LDQ     INTEGER
C             The leading dimension of the array Q.
C             LDQ >= 1,           if COMPQ = 'N';
C             LDQ >= MAX(1, 2*N), if COMPQ = 'C'.
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
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (LIWORK)
C             On exit, if INFO = -19, IWORK(1) returns the minimum value
C             of LIWORK.
C
C     LIWORK  INTEGER
C             The dimension of the array IWORK.  LIWORK = 1, if N = 0,
C             LIWORK >= MAX( N + 12, 2*N + 3 ),     if COMPQ = 'N',
C             LIWORK >= MAX( 32, 2*N + 3 ),         if COMPQ = 'C'.
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal LDWORK.
C             On exit, if INFO = -21, DWORK(1) returns the minimum value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The dimension of the array DWORK.  LDWORK = 1, if N = 0,
C             LDWORK >= 3*(N/2)**2 + N**2 + MAX( L, 36 ),
C                                                        if COMPQ = 'N',
C             where     L = 4*N + 4, if N/2 is even, and
C                       L = 4*N    , if N/2 is odd; 
C             LDWORK >= 8*N**2 + MAX( 8*N + 32, 272 ),   if COMPQ = 'C'.
C             For good performance LDWORK should be generally larger.
C
C             If LDWORK = -1  a workspace query is assumed; the
C             routine only calculates the optimal size of the DWORK
C             array, returns this value as the first entry of the DWORK
C             array, and no error message is issued by XERBLA.
C
C     BWORK   LOGICAL array, dimension (N/2)
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0: succesful exit;
C             < 0: if INFO = -i, the i-th argument had an illegal value;
C             = 1: periodic QZ iteration failed in the SLICOT Library
C                  routines MB04BD or MB04HD (QZ iteration did not
C                  converge or computation of the shifts failed);
C             = 2: standard QZ iteration failed in the SLICOT Library
C                  routines MB04HD or MB03DD (called by MB03JD);
C             = 3: a numerically singular matrix was found in the SLICOT
C                  Library routine MB03HD (called by MB03JD);
C             = 4: the singular value decomposition failed in the LAPACK
C                  routine DGESVD (for ORTH = 'S');
C             = 5: some eigenvalues might be inaccurate. This is a
C                  warning.
C
C     METHOD
C
C     First, the decompositions of S and H are computed via orthogonal
C     transformations Q1 and Q2 as follows:
C
C                       (  Aout  Dout  )
C       Q1' S J Q1 J' = (              ),
C                       (   0    Aout' )
C
C                       (  Bout  Fout  )
C       J' Q2' J S Q2 = (              ) =: T,                       (2)
C                       (   0    Bout' )
C
C                  (  C1out  Vout  )            (  0  I  )
C       Q1' H Q2 = (               ), where J = (        ),
C                  (  0     C2out' )            ( -I  0  )
C
C     and Aout, Bout, C1out are upper triangular, C2out is upper quasi-
C     triangular and Dout and Fout are skew-symmetric.
C
C     Then, orthogonal matrices Q3 and Q4 are found, for the extended
C     matrices
C
C            (  Aout   0  )          (    0   C1out )
C       Se = (            ) and He = (              ),
C            (   0   Bout )          ( -C2out   0   )
C
C     such that S11 := Q4' Se Q3 is upper triangular and
C     H11 := Q4' He Q3 is upper quasi-triangular. The following matrices
C     are computed:
C
C                  (  Dout   0  )                   (   0   Vout )
C       S12 := Q4' (            ) Q4 and H12 := Q4' (            ) Q4.
C                  (   0   Fout )                   ( Vout'   0  )
C
C     Then, an orthogonal matrix Q is found such that the eigenvalues
C     with strictly negative real parts of the pencil
C
C         (  S11  S12  )     (  H11  H12  )
C       a (            ) - b (            )
C         (   0   S11' )     (   0  -H11' )
C
C     are moved to the top of this pencil.
C
C     Finally, an orthogonal basis of the right deflating subspace
C     corresponding to the eigenvalues with strictly negative real part
C     is computed. See also page 12 in [1] for more details.
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
C     Oct. 2010.
C
C     REVISIONS
C
C     V. Sima, Nov. 2010, Dec. 2010, Mar. 2011, Aug. 2011, Nov. 2011,
C     Oct. 2012, July 2013, July 2014, Jan. 2017, May 2020.
C     M. Voigt, Jan. 2012.
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
      CHARACTER          COMPQ, ORTH
      INTEGER            INFO, LDA, LDB, LDDE, LDFG, LDQ, LDWORK,
     $                   LIWORK, N, NEIG
C
C     .. Array Arguments ..
      LOGICAL            BWORK( * )
      INTEGER            IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), ALPHAI( * ), ALPHAR( * ),
     $                   B( LDB, * ), BETA( * ), DE( LDDE, * ),
     $                   DWORK( * ), FG( LDFG, * ), Q( LDQ, * )
C
C     .. Local Scalars ..
      LOGICAL            LINIQ, LQUERY, QR, QRP, SVD
      CHARACTER*14       CMPQ
      INTEGER            IB, IC2, IFO, IH11, IH12, IQ1, IQ2, IQ3, IQ4,
     $                   IRT, IS11, IS12, IW, IWRK, J, M, MINDW, MINIW,
     $                   MM, N2, NM, NMM, NN, OPTDW
C
C     .. Local Arrays ..
      DOUBLE PRECISION   DUM( 4 )
C
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C
C     .. External Subroutines ..
      EXTERNAL           DAXPY, DCOPY, DGEMM, DGEQP3, DGEQRF, DGESVD,
     $                   DLACPY, DORGQR, DSCAL, DSYR2K, DTRMM, MA02AD,
     $                   MB01KD, MB01LD, MB03JD, MB04BD, MB04HD, XERBLA
C
C     .. Intrinsic Functions ..
      INTRINSIC          INT, MAX, MOD, SQRT
C
C     .. Executable Statements ..
C
C     Decode the input arguments.
C     Using ORTH = 'Q' is not safe, but sometimes gives better results.
C
      M   = N/2
      N2  = N*2
      NN  = N*N
      MM  = M*M
      NEIG  = 0
      LINIQ = LSAME( COMPQ, 'C' )
      IF( LINIQ ) THEN
         QR  = LSAME( ORTH, 'Q' )
         QRP = LSAME( ORTH, 'P' )
         SVD = LSAME( ORTH, 'S' )
      END IF
      IF( N.EQ.0 ) THEN
         MINIW = 1
         MINDW = 1
      ELSE IF( LINIQ ) THEN
         MINIW = MAX( 32, N2 + 3 )
         MINDW = 8*NN + MAX( 8*N + 32, 272 )
      ELSE
         IF( MOD( M, 2 ).EQ.0 ) THEN
            J = MAX( 4*N, 32 ) + 4
         ELSE
            J = MAX( 4*N, 36 )
         END IF
         MINIW = MAX( N + 12, N2 + 3 )
         MINDW = 3*M**2 + NN + J
      END IF
      LQUERY = LDWORK.EQ.-1
C
C     Test the input arguments.
C
      INFO = 0
      IF( .NOT.( LSAME( COMPQ, 'N' ) .OR. LINIQ ) ) THEN
         INFO = -1
      ELSE IF( LINIQ ) THEN
         IF( .NOT.( QR .OR. QRP .OR. SVD ) )
     $      INFO = -2
      ELSE IF( N.LT.0 .OR. MOD( N, 2 ).NE.0 ) THEN
         INFO = -3
      ELSE IF(  LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LDDE.LT.MAX( 1, M ) ) THEN
         INFO = -7
      ELSE IF(  LDB.LT.MAX( 1, M ) ) THEN
         INFO = -9
      ELSE IF( LDFG.LT.MAX( 1, M ) ) THEN
         INFO = -11
      ELSE IF( LDQ.LT.1 .OR. ( LINIQ .AND. LDQ.LT.N2 ) ) THEN
         INFO = -14
      ELSE IF( LIWORK.LT.MINIW ) THEN
         IWORK( 1 ) = MINIW
         INFO = -19
      ELSE IF( .NOT. LQUERY .AND. LDWORK.LT.MINDW ) THEN
         DWORK( 1 ) = MINDW
         INFO = -21
      END IF
C
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'MB03LD', -INFO )
         RETURN
      ELSE IF( N.GT.0 ) THEN
C
C        Compute optimal workspace.
C
         IF( LQUERY ) THEN
            IF( LINIQ ) THEN
               CALL MB04HD( 'I', 'I', N, DWORK, N, DWORK, N, DWORK, N,
     $                      DWORK, N, IWORK, LIWORK, DUM, -1, BWORK,
     $                      INFO )
               IF( SVD ) THEN
                  CALL DGESVD( 'O', 'N', N, N, Q, LDQ, DWORK, DWORK,
     $                         LDQ, DWORK, 1, DUM( 2 ), -1, INFO )
                  J = N + INT( DUM( 2 ) )
               ELSE
                  IF( QR ) THEN
                     CALL DGEQRF( N, M, Q, LDQ, DWORK, DUM( 2 ), -1,
     $                            INFO )
                     J = M
                  ELSE
                     CALL DGEQP3( N, N, Q, LDQ, IWORK, DWORK, DUM( 2 ),
     $                            -1, INFO )
                     J = N
                  END IF
                  CALL DORGQR( N, J, J, Q, LDQ, DWORK, DUM( 3 ), -1,
     $                         INFO )
                  J = J + MAX( INT( DUM( 2 ) ), INT( DUM( 3 ) ) )
               END IF
               OPTDW = MAX( MINDW, 6*NN + INT( DUM( 1 ) ), J )
            ELSE
               OPTDW = MINDW
            END IF
            DWORK( 1 ) = OPTDW
            RETURN
         END IF
      END IF
C
C     Quick return if possible.
C
      IF( N.EQ.0 ) THEN
         DWORK( 1 ) = ONE
         RETURN
      END IF
C
      IFO = 1
C
C     STEP 1: Apply MB04BD to transform the pencil to real
C             skew-Hamiltonian/Hamiltonian Schur form.
C
C     Set the computation option and pointers for the inputs and outputs
C     of MB04BD. If possible, array Q is used as vectorized workspace.
C
C     Real workspace:     need   w1 + w0 + MAX( L, 36 ),  where
C                                w1 = 2*N**2, w0 = 2*N**2, if COMPQ = 'C';
C                                w1 = 3*M**2, w0 =   N**2, if COMPQ = 'N';
C                                L  = 4*N + 4, if N/2 is even, and
C                                L  = 4*N    , if N/2 is odd.
C     Integer workspace:  need   MAX(N+12,2*N+3).
C
      IF( LINIQ ) THEN
         CMPQ = 'Initialize'
         IQ1  = 1
         IQ2  = IQ1 + NN
         IWRK = IQ2 + NN
         IF( MOD( M, 4 ).EQ.0 ) THEN
            IC2 = M/4
         ELSE
            IC2 = INT( M/4 ) + 1
         END IF
         IB   = 2*IC2 + 1
         IC2  =   IC2 + 1
         CALL MB04BD( 'Triangularize', CMPQ, CMPQ, N, A, LDA, DE, LDDE,
     $                B, LDB, FG, LDFG, DWORK( IQ1 ), N, DWORK( IQ2 ),
     $                N, Q( 1, IB ), M, Q( 1, IFO ), M, Q( 1, IC2 ), M,
     $                ALPHAR, ALPHAI, BETA, IWORK, LIWORK,
     $                DWORK( IWRK ), LDWORK-IWRK+1, INFO )
      ELSE
         CMPQ = 'No Computation'
         IB   = IFO + MM
         IC2  = IB  + MM
         IWRK = IC2 + MM
         CALL MB04BD( 'Eigenvalues', CMPQ, CMPQ, N, A, LDA, DE, LDDE, B,
     $                LDB, FG, LDFG, DWORK, N, DWORK, N, DWORK( IB ), M,
     $                DWORK( IFO ), M, DWORK( IC2 ), M, ALPHAR, ALPHAI,
     $                BETA, IWORK, LIWORK, DWORK( IWRK ), LDWORK-IWRK+1,
     $                INFO )
      END IF
      OPTDW = MAX( MINDW, INT( DWORK( IWRK ) ) + IWRK - 1 )
C
      IF( INFO.GT.0 .AND. INFO.LT.3 ) THEN
         INFO = 1
         RETURN
      ELSE IF( INFO.EQ.3 ) THEN
         IW = 5
      ELSE
         IW = 0
      END IF
C
      IF( .NOT.LINIQ ) THEN
         CALL MA02AD( 'Upper', M, M, DWORK( IC2 ), M, DE, LDDE )
         CALL DCOPY(  M-1, DWORK( IC2+1 ), M+1, DE( 1, 2 ), LDDE+1 )
         DWORK( 1 ) = OPTDW
         INFO = IW
         RETURN
      END IF
C
C     STEP 2: Build the needed parts of the extended matrices Se and He,
C     and compute the transformed matrices and the orthogonal matrices
C     Q3 and Q4.
C
C     Real workspace:     need   w1 + w2 + 2*N**2 + MAX(M+168,272), with
C                                w2 = 4*N**2 (COMPQ = 'C');
C                         prefer larger.
C     Integer workspace:  need   MAX(M+1,32).
C
      NM   = N*M
      NMM  = NM + M
      IQ3  = IWRK
      IQ4  = IQ3  + NN
      IS11 = IQ4  + NN
      IH11 = IS11 + NN
      IWRK = IH11 + NN
C
      CALL DLACPY( 'Full', M, M, A, LDA, DWORK( IS11 ), N )
      CALL DLACPY( 'Full', M, M, Q( 1, IB ), M, DWORK( IS11+NMM ), N )
      CALL DSCAL(  MM, -ONE, Q( 1, IC2 ), 1 )
      CALL DLACPY( 'Full', M, M, Q( 1, IC2 ), M, DWORK( IH11+M ), N )
      CALL DLACPY( 'Full', M, M, B, LDB, DWORK( IH11+NM ), N )
C
      CALL MB04HD( CMPQ, CMPQ, N, DWORK( IS11 ), N, DWORK( IH11 ), N,
     $             DWORK( IQ3 ), N, DWORK( IQ4 ), N, IWORK, LIWORK,
     $             DWORK( IWRK ), LDWORK-IWRK+1, BWORK, INFO )
      IF( INFO.GT.0 ) THEN
         IF( INFO.GT.2 )
     $      INFO = 2
         RETURN
      END IF
      OPTDW = MAX( OPTDW, INT( DWORK( IWRK ) ) + IWRK - 1 )
C
C     STEP 3: Update S12 and H12, building the upper triangular parts,
C     and exploiting the structure. Note that S12 is skew-symmetric and
C     H12 is symmetric.
C
C     Real workspace:     need   w1 + w2 + w3, where
C                                w3 = N**2 + M**2.
C
      IS12 = IWRK
      IH12 = IS12 + NN
      IWRK = IH12
C
      IF( M.GT.1 ) THEN
C
C                                                   [ Qa  Qc ]
C        Compute Qa'*Do*Qc + Qb'*Fo*Qd, where Q4 =: [        ],
C                                                   [ Qb  Qd ]
C        with Do := Dout, etc.
C        Compute also Qc'*Do*Qc + Qd'*Fo*Qd, using MB01KD.
C        Part of the array Q and DWORK(IS12) are used as workspace.
C
         CALL DLACPY( 'Full', M-1, M, DWORK( IQ4+NM+1 ), N,
     $                DWORK( IS12 ), M )
         CALL DLACPY( 'Full', M-1, M, DWORK( IQ4+NM ), N, Q( 2, IB ),
     $                M )
         CALL DTRMM(  'Left', 'Upper', 'No Transpose', 'Non-Unit', M-1,
     $                M, ONE, DE( 1, 3 ), LDDE, DWORK( IS12 ), M )
C
         CALL MB01KD( 'Upper', 'Transpose', M, M-1, ONE,
     $                DWORK( IQ4+NM ), N, DWORK( IS12 ), M, ZERO,
     $                DWORK( IS12+NMM ), N, INFO )
C
         CALL DTRMM(  'Left', 'Upper', 'Transpose', 'Non-Unit', M-1, M,
     $                -ONE, DE( 1, 3 ), LDDE, Q( 2, IB ), M )
         DUM( 1 ) = ZERO
         CALL DCOPY( M, DUM, 0, DWORK( IS12+M-1 ), M )
         CALL DCOPY( M, DUM, 0, Q( 1, IB ), M )
         CALL DAXPY( MM, ONE, Q( 1, IB ), 1, DWORK( IS12 ), 1 )
C
         CALL DLACPY( 'Full', M-1, M, DWORK( IQ4+NMM+1 ), N,
     $                DWORK( IWRK ), M )
         CALL DLACPY( 'Full', M-1, M, DWORK( IQ4+NMM ), N, Q( 2, IB ),
     $                M )
         CALL DTRMM(  'Left', 'Upper', 'No Transpose', 'Non-Unit', M-1,
     $                M, ONE, Q( M+1, IFO ), M, DWORK( IWRK ), M )
C
         CALL MB01KD( 'Upper', 'Transpose', M, M-1, ONE,
     $                DWORK(  IQ4+NMM ), N, DWORK( IWRK ), M, ONE,
     $                DWORK( IS12+NMM ), N, INFO )
C
         CALL DTRMM(  'Left', 'Upper', 'Transpose', 'Non-Unit', M-1,
     $                M, -ONE, Q( M+1, IFO ), M, Q( 2, IB ), M )
         CALL DCOPY( M, DUM, 0, DWORK( IWRK+M-1 ), M )
         CALL DAXPY( MM, ONE, Q( 1, IB ), 1, DWORK( IWRK ), 1 )
C
         CALL DGEMM( 'Transpose', 'No Transpose', M, M, M, ONE,
     $               DWORK( IQ4 ), N, DWORK( IS12 ), M, ZERO,
     $               DWORK( IS12+NM ), N )
         CALL DGEMM( 'Transpose', 'No Transpose', M, M, M, ONE,
     $               DWORK( IQ4+M ), N, DWORK( IWRK ), M, ONE,
     $               DWORK( IS12+NM ), N )
C
C        Compute Qa'*Do*Qa + Qb'*Fo*Qb.
C
         CALL MB01LD( 'Upper', 'Transpose', M, M, ZERO, ONE,
     $                DWORK( IS12 ), N, DWORK( IQ4 ), N, DE( 1, 2 ),
     $                LDDE, DWORK( IWRK ), LDWORK-IWRK+1, INFO )
         CALL MB01LD( 'Upper', 'Transpose', M, M, ONE, ONE,
     $                DWORK( IS12 ), N, DWORK( IQ4+M ), N, Q( 1, IFO ),
     $                M, DWORK( IWRK ), LDWORK-IWRK+1, INFO )
      END IF
C
C     Compute Qb'*Vo'*Qc + Qa'*Vo*Qd.
C     Real workspace:     need   w1 + w2 + w3, where
C                                w3 = 2*N**2.
C
      CALL DGEMM( 'Transpose', 'No Transpose', M, M, M, ONE,
     $            FG( 1, 2 ), LDFG, DWORK( IQ4+NM ), N, ZERO,
     $            Q( 1, IFO ), M )
      CALL DGEMM( 'Transpose', 'No Transpose', M, M, M, ONE,
     $            FG( 1, 2 ), LDFG, DWORK( IQ4 ), N, ZERO,
     $            DWORK( IH12+NMM ), N )
      CALL DGEMM( 'Transpose', 'No Transpose', M, M, M, ONE,
     $            DWORK( IQ4+M ), N, Q( 1, IFO ), M, ZERO,
     $            DWORK( IH12+NM ), N )
      CALL DGEMM( 'Transpose', 'No Transpose', M, M, M, ONE,
     $            DWORK( IH12+NMM ), N, DWORK( IQ4+NMM ), N, ONE,
     $            DWORK( IH12+NM ), N )
C
C     Compute the upper triangle of Qa'*Vo*Qb + (Qa'*Vo*Qb)'.
C
      CALL DSYR2K( 'Upper', 'Transpose', M, M, ONE, DWORK( IH12+NMM ),
     $             N, DWORK( IQ4+M ), N, ZERO, DWORK( IH12 ), N )
C
C     Compute the upper triangle of Qc'*Vo*Qd + (Qc'*Vo*Qd)'.
C
      CALL DSYR2K( 'Upper', 'Transpose', M, M, ONE, Q( 1, IFO ), M,
     $             DWORK( IQ4+NMM ), N, ZERO, DWORK( IH12+NMM ), N )
C
C     Return C2out.
C
      CALL DSCAL(  MM, -ONE, Q( 1, IC2 ), 1 )
      CALL MA02AD( 'Upper', M, M, Q( 1, IC2 ), M, DE, LDDE )
      CALL DCOPY(  M-1, Q( 2, IC2 ), M+1, DE( 1, 2 ), LDDE+1 )
C
C     STEP 4: Apply MB03JD to reorder the eigenvalues with strictly
C             negative real part to the top.
C
C     Real workspace:     need   w1 + w2 + w3 + MAX(8*N+32,108),
C                                w3 = 2*N**2.
C     Integer workspace:  need   2*N + 1.
C
      IWRK = IH12 + NN
C
      CALL MB03JD( CMPQ, N2, DWORK( IS11 ), N, DWORK( IS12 ), N,
     $             DWORK( IH11 ), N, DWORK( IH12 ), N, Q, LDQ, NEIG,
     $             IWORK, LIWORK, DWORK( IWRK ), LDWORK-IWRK+1, INFO )
      IF( INFO.GT.0 ) THEN
         INFO = INFO + 1
         RETURN
      END IF
C
C     STEP 5: Compute the deflating subspace corresponding to the
C             eigenvalues with strictly negative real part.
C
C     Real workspace:     need   w2 + 3*N**2, if ORTH = 'QR';
C                                w2 + 4*N**2, otherwise.
C
      IWRK = IS11
      IF( QR )
     $   NEIG = NEIG/2
C
C     Compute [ J*Q1*J' Q2 ].
C
      CALL DLACPY( 'Full', M, M, DWORK( IQ1+NMM ), N, DWORK( IWRK ), N )
      CALL DLACPY( 'Full', M, M, DWORK( IQ1+NM ), N, DWORK( IWRK+M ),
     $             N )
      DO 10 J = 1, M
         CALL DSCAL( M, -ONE, DWORK( IWRK+M+(J-1)*N ), 1 )
   10 CONTINUE
      CALL DLACPY( 'Full', M, M, DWORK( IQ1+M ), N, DWORK( IWRK+NM ),
     $             N )
      DO 20 J = 1, M
         CALL DSCAL( M, -ONE, DWORK( IWRK+NM+(J-1)*N ), 1 )
   20 CONTINUE
      CALL DLACPY( 'Full', M, M, DWORK( IQ1 ), N, DWORK( IWRK+NMM ), N )
C
      CALL DLACPY( 'Full', N, N, DWORK( IQ2 ), N, DWORK( IWRK+NN ), N )
C
C     Compute the first NEIG columns of P*[ Q3  0; 0 Q4 ]*Q.
C
      IRT = IWRK + N*N2
      CALL DGEMM( 'No Transpose', 'No Transpose', M, NEIG, N, ONE,
     $            DWORK( IQ3 ), N, Q, LDQ, ZERO, DWORK( IRT ), N2 )
      CALL DGEMM( 'No Transpose', 'No Transpose', M, NEIG, N, ONE,
     $            DWORK( IQ4 ), N, Q( N+1, 1 ), LDQ, ZERO,
     $            DWORK( IRT+M ), N2 )
      CALL DGEMM( 'No Transpose', 'No Transpose', M, NEIG, N, ONE,
     $            DWORK( IQ3+M ), N, Q, LDQ, ZERO, DWORK( IRT+N ), N2 )
      CALL DGEMM( 'No Transpose', 'No Transpose', M, NEIG, N, ONE,
     $            DWORK( IQ4+M ), N, Q( N+1, 1 ), LDQ, ZERO,
     $            DWORK( IRT+N+M ), N2 )
C
C     Compute the deflating subspace.
C
      CALL DGEMM( 'No Transpose', 'No Transpose', N, NEIG, N2,
     $            SQRT( TWO )/TWO, DWORK( IWRK ), N, DWORK( IRT ), N2,
     $            ZERO, Q, LDQ )
C
C     Orthogonalize the basis given in Q(1:n,1:neig).
C
      IWRK = NEIG + 1
      IF( SVD ) THEN
C
C        Real workspace:     need   N + MAX(1,5*N);
C                            prefer larger.
C
         CALL DGESVD( 'Overwrite', 'No V', N, NEIG, Q, LDQ, DWORK,
     $                DWORK, 1,  DWORK, 1, DWORK( IWRK ), LDWORK-IWRK+1,
     $                INFO )
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
C           Real workspace:     need   N;
C                               prefer M+M*NB, where NB is the optimal
C                                              blocksize.
C
            CALL DGEQRF( N, NEIG, Q, LDQ, DWORK, DWORK( IWRK ),
     $                   LDWORK-IWRK+1, INFO )
         ELSE
C
C           Real workspace:     need   4*N+1;
C                               prefer 3*N+(N+1)*NB.
C
            DO 30 J = 1, NEIG
               IWORK( J ) = 0
   30       CONTINUE
            CALL DGEQP3( N, NEIG, Q, LDQ, IWORK, DWORK, DWORK( IWRK ),
     $                   LDWORK-IWRK+1, INFO )
         END IF
         OPTDW = MAX( OPTDW, INT( DWORK( IWRK ) ) + IWRK - 1 )
C
C        Real workspace:     need   2*NEIG;
C                            prefer NEIG + NEIG*NB.
C
         CALL DORGQR( N, NEIG, NEIG, Q, LDQ, DWORK, DWORK( IWRK ),
     $                LDWORK-IWRK+1, INFO )
         OPTDW = MAX( OPTDW, INT( DWORK( IWRK ) ) + IWRK - 1 )
         IF( QRP )
     $      NEIG = NEIG/2
      END IF
C
      DWORK( 1 ) = OPTDW
      INFO = IW
      RETURN
C *** Last line of MB03LD ***
      END
