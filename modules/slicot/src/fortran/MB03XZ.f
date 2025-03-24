      SUBROUTINE MB03XZ( BALANC, JOB, JOBU, N, A, LDA, QG, LDQG, U1,
     $                   LDU1, U2, LDU2, WR, WI, ILO, SCALE, DWORK,
     $                   LDWORK, ZWORK, LZWORK, BWORK, INFO )
C
C     PURPOSE
C
C     To compute the eigenvalues of a Hamiltonian matrix,
C
C                   [  A   G  ]         H        H
C             H  =  [       H ],   G = G ,  Q = Q ,                  (1)
C                   [  Q  -A  ]
C
C     where A, G and Q are complex n-by-n matrices.
C
C     Due to the structure of H, if lambda is an eigenvalue, then
C     -conjugate(lambda) is also an eigenvalue. This does not mean that
C     purely imaginary eigenvalues are necessarily multiple. The routine
C     computes the eigenvalues of H using an embedding to a real skew-
C     Hamiltonian matrix He,
C
C                    [  Ae   Ge  ]            T            T
C             He  =  [         T ],   Ge = -Ge ,   Qe = -Qe ,        (2)
C                    [  Qe   Ae  ]
C
C     where Ae, Ge, and Qe are real 2*n-by-2*n matrices, defined by
C
C                    [   Im(A)   Re(A)  ]
C             Ae  =  [                  ],
C                    [  -Re(A)   Im(A)  ]
C
C                    [  triu(Im(G))     Re(G)     ]
C        triu(Ge) =  [                            ],
C                    [       0       triu(Im(G))  ]
C
C                    [  tril(Im(Q))       0       ]
C        tril(Qe) =  [                            ], 
C                    [     -Re(Q)    tril(Im(Q))  ]
C
C     and triu and tril denote the upper and lower triangle,
C     respectively. Then, an orthogonal symplectic matrix Ue is used to
C     reduce He to the structured real Schur form
C
C           T          [  Se   De ]            T
C          Ue He Ue =  [        T ],   De = -De ,                    (3)
C                      [  0    Se ]
C
C     where Ue is a 4n-by-4n real symplectic matrix, and Se is upper
C     quasi-triangular (real Schur form).
C
C     Optionally, if JOB = 'S', or JOB = 'G', the matrix i*He is further
C     transformed to the structured complex Schur form
C
C           H            [  Sc  Gc ]           H
C          U (i*He) U =  [       H ],   Gc = Gc ,                    (4)
C                        [  0  -Sc ]
C
C     where U is a 4n-by-4n unitary symplectic matrix, and Sc is upper
C     triangular (Schur form).
C
C     The algorithm is backward stable and preserves the spectrum
C     structure in finite precision arithmetic.
C
C     Optionally, a symplectic balancing transformation to improve the
C     conditioning of eigenvalues is computed (see the SLICOT Library
C     routine MB04DZ). In this case, the matrix He in decompositions (3)
C     and (4) must be replaced by the balanced matrix.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     BALANC  CHARACTER*1
C             Indicates how H should be diagonally scaled and/or
C             permuted to reduce its norm.
C             = 'N': Do not diagonally scale or permute;
C             = 'P': Perform symplectic permutations to make the matrix
C                    closer to skew-Hamiltonian Schur form. Do not
C                    diagonally scale;
C             = 'S': Diagonally scale the matrix, i.e., replace A, G and
C                    Q by D*A*D**(-1), D*G*D and D**(-1)*Q*D**(-1) where
C                    D is a diagonal matrix chosen to make the rows and
C                    columns of H more equal in norm. Do not permute;
C             = 'B': Both diagonally scale and permute A, G and Q.
C             Permuting does not change the norm of H, but scaling does.
C
C     JOB     CHARACTER*1
C             Indicates whether the user wishes to compute the full
C             decomposition (4) or the eigenvalues only, as follows:
C             = 'E': compute the eigenvalues only;
C             = 'S': compute the matrix Sc of (4);
C             = 'G': compute the matrices Sc and Gc of (4).
C
C     JOBU    CHARACTER*1
C             Indicates whether or not the user wishes to compute the
C             symplectic matrix Ue of (3), if JOB = 'E', or U of (4),
C             if JOB = 'S' or JOB = 'G', as follows:
C             = 'N': the matrix Ue or U is not computed;
C             = 'U': the matrix Ue or U is computed.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrix A.  N >= 0.
C
C     A       (input/output) COMPLEX*16 array, dimension (LDA,K)
C             where K = N, if JOB = 'E', and K = 2*N, if JOB <> 'E'.
C             On entry, the leading N-by-N part of this array must
C             contain the matrix A.
C             On exit, if JOB = 'E', the leading N-by-N part of this
C             array is unchanged, if BALANC = 'N', or it contains the
C             balanced (permuted and/or scaled) matrix A, if
C             BALANC <> 'N'.
C             On exit, if JOB = 'S' or JOB = 'G', the leading 2*N-by-2*N
C             upper triangular part of this array contains the matrix Sc
C             (complex Schur form) of decomposition (4).
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= max(1,K).
C
C     QG      (input/output) COMPLEX*16 array, dimension
C                            (LDQG,min(K+1,2*N))
C             On entry, the leading N-by-N+1 part of this array must
C             contain in columns 1:N the lower triangular part of the
C             matrix Q and in columns 2:N+1 the upper triangular part
C             of the matrix G.
C             On exit, if JOB <> 'G', the leading N-by-N+1 part of this
C             array is unchanged, if BALANC = 'N', or it contains the
C             balanced (permuted and/or scaled) parts of the matrices
C             Q and G (as above), if BALANC <> 'N'.
C             On exit, JOB = 'G', the leading 2*N-by-2*N upper
C             triangular part of this array contains the upper
C             triangular part of the matrix Gc in the decomposition (4).
C
C     LDQG    INTEGER
C             The leading dimension of the array QG.  LDQG >= max(1,K).
C
C     U1      (output) COMPLEX*16 array, dimension (LDU1,2*N)
C             On exit, if JOB = 'S' or JOB = 'G', and JOBU = 'U', the
C             leading 2*N-by-2*N part of this array contains the (1,1)
C             block of the unitary symplectic matrix U of the
C             decomposition (4).
C             If JOB = 'E' or JOBU = 'N', this array is not referenced.
C
C     LDU1    INTEGER
C             The leading dimension of the array U1.  LDU1 >= 1.
C             LDU1 >= 2*N,    if JOBU = 'U'.
C
C     U2      (output) COMPLEX*16 array, dimension (LDU2,2*N)
C             On exit, if JOB = 'S' or JOB = 'G', and JOBU = 'U', the
C             leading 2*N-by-2*N part of this array contains the (1,2)
C             block of the unitary symplectic matrix U of the
C             decomposition (4).
C             If JOB = 'E' or JOBU = 'N', this array is not referenced.
C
C     LDU2    INTEGER
C             The leading dimension of the array U2.  LDU2 >= 1.
C             LDU2 >= 2*N,    if JOBU = 'U'.
C
C     WR      (output) DOUBLE PRECISION array, dimension (2*N)
C     WI      (output) DOUBLE PRECISION array, dimension (2*N)
C             On exit, the leading 2*N elements of WR and WI contain the
C             real and imaginary parts, respectively, of the eigenvalues
C             of the Hamiltonian matrix H.
C
C     ILO     (output) INTEGER
C             ILO is an integer value determined when H was balanced.
C             The balanced A(I,J) = 0 if I > J and J = 1,...,ILO-1.
C             The balanced Q(I,J) = 0 if J = 1,...,ILO-1 or
C             I = 1,...,ILO-1.
C
C     SCALE   (output) DOUBLE PRECISION array, dimension (N)
C             On exit, if BALANC <> 'N', the leading N elements of this
C             array contain details of the permutation and/or scaling
C             factors applied when balancing H, see MB04DZ.
C             This array is not referenced if BALANC = 'N'.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0,  DWORK(1)  returns the optimal
C             value of LDWORK, and   DWORK(2)  returns the 1-norm of the
C             (scaled, if BALANC = 'S' or 'B') Hamiltonian matrix.
C             Moreover, the next locations of this array have the
C             following content:
C             - The leading 2*N-by-2*N upper Hessenberg part in the
C             locations 3:2+4*N*N contains the upper Hessenberg part of
C             the real Schur matrix Se in the decomposition (3);
C             - the leading 2*N-by-2*N upper triangular part in the
C             locations 3+4*N*N+2*N:2+8*N*N+2*N contains the upper
C             triangular part of the skew-symmetric matrix De in the
C             decomposition (3).
C             - If JOBU = 'U', the leading 2*N-by-2*N part in the
C             locations 3+8*N*N+2*N:2+12*N*N+2*N contains the (1,1)
C             block of the orthogonal symplectic matrix Ue of
C             decomposition (3).
C             - the leading 2*N-by-2*N part in the locations
C             3+12*N*N+2*N:2+16*N*N+2*N contains the (2,1) block of the
C             orthogonal symplectic matrix Ue.
C             On exit, if  INFO = -18,  DWORK(1)  returns the minimum
C             value of LDWORK.
C
C     LDWORK  INTEGER
C             The dimension of the array DWORK.
C             LDWORK >= MAX( 12*N**2 + 4*N, 8*N**2 + 12*N ) + 2,
C                                    if JOB = 'E' and JOBU = 'N';
C             LDWORK >= MAX( 2, 12*N**2 + 4*N, 8*N**2 + 12*N ),
C                                    if JOB = 'S' or 'G' and JOBU = 'N';
C             LDWORK >= 20*N**2 + 12*N + 2,
C                                    if JOB = 'E' and JOBU = 'U';
C             LDWORK >= MAX( 2, 20*N**2 + 12*N ),
C                                    if JOB = 'S' or 'G' and JOBU = 'U'.
C             For good performance, LDWORK must generally be larger.
C
C             If LDWORK = -1, then a workspace query is assumed;
C             the routine only calculates the optimal size of the
C             DWORK array, returns this value as the first entry of
C             the DWORK array, and no error message related to LDWORK
C             is issued by XERBLA.
C
C     ZWORK   COMPLEX*16 array, dimension (LZWORK)
C             On exit, if INFO = 0,  ZWORK(1)  returns the optimal
C             value of LZWORK.
C             On exit, if  INFO = -20,  ZWORK(1)  returns the minimum
C             value of LZWORK.
C
C     LZWORK  INTEGER
C             The dimension of the array ZWORK.
C             LZWORK >= 1,                  if JOB = 'E';
C             LZWORK >= MAX( 1, 12*N - 6 ), if JOB = 'S' and JOBU = 'N';
C             LZWORK >= MAX( 1, 12*N - 2 ), if JOB = 'G' or  JOBU = 'U'.
C
C             If LZWORK = -1, then a workspace query is assumed;
C             the routine only calculates the optimal size of the
C             ZWORK array, returns this value as the first entry of
C             the ZWORK array, and no error message related to LZWORK
C             is issued by XERBLA.
C
C     BWORK   LOGICAL array, dimension (LBWORK)
C             LBWORK >= 0,     if JOB = 'E';
C             LBWORK >= 2*N-1, if JOB = 'S' or JOB = 'G'.
C
C     Error Indicator
C
C     INFO     INTEGER
C              = 0:  successful exit;
C              < 0:  if INFO = -i, the i-th argument had an illegal
C                    value;
C              > 0:  if INFO = i, the QR algorithm failed to compute
C                    all the eigenvalues; elements i+1:2*N of WR and
C                    WI contain eigenvalues which have converged;
C              = 2*N+1:  the QR algorithm failed to compute the
C                    eigenvalues of a 2-by-2 real block.
C
C     METHOD
C
C     First, the extended matrix He in (2) is built. Then, the
C     structured real Schur form in (3) is computed, using the SLICOT
C     Library routine MB03XS. The eigenvalues of Se immediately give
C     the eigenvalues of H. Finally, if required, Se is further
C     transformed by using the complex QR algorithm to triangularize
C     its 2-by-2 blocks, and Ge and U are updated, to obtain (4).
C
C     REFERENCES
C
C     [1] Benner, P., Mehrmann, V. and Xu, H.
C         A note on the numerical solution of complex Hamiltonian and
C         skew-Hamiltonian eigenvalue problems.
C         Electr. Trans. Num. Anal., 8, pp. 115-126, 1999.
C
C     [2] Van Loan, C.F.
C         A symplectic method for approximating all the eigenvalues of
C         a Hamiltonian matrix.
C         Linear Algebra and its Applications, 61, pp. 233-251, 1984.
C
C     CONTRIBUTORS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Nov. 2011.
C
C     REVISIONS
C
C     V. Sima, Dec. 2011, Sep. 2012, Oct. 2012.
C
C     KEYWORDS
C
C     Schur form, eigenvalues, skew-Hamiltonian matrix.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0 )
      COMPLEX*16         CZERO, CONE
      PARAMETER          ( CZERO = ( 0.0D0, 0.0D0 ),
     $                     CONE  = ( 1.0D0, 0.0D0 ) )
C     .. Scalar Arguments ..
      CHARACTER          BALANC, JOB, JOBU
      INTEGER            ILO, INFO, LDA, LDQG, LDU1, LDU2, LDWORK,
     $                   LZWORK, N
C     .. Array Arguments ..
      LOGICAL            BWORK( * )
      DOUBLE PRECISION   DWORK( * ), SCALE( * ), WI( * ), WR( * )
      COMPLEX*16         A( LDA, * ), QG( LDQG, * ), U1( LDU1, * ),
     $                   U2( LDU2, * ), ZWORK( * )
C     .. Local Scalars ..
      LOGICAL            LQUERY, LSCAL, SCALEH, WANTG, WANTS, WANTU,
     $                   WANTUS
      INTEGER            I, I1, IA, IERR, IEV, IQG, IS, IU, IU1, IU2,
     $                   IUB, IW, IW1, IWRK, J, J1, J2, JM1, JP2, K,
     $                   MINDB, MINDW, MINZW, N2, NB, NC, NC1, NN, NN2,
     $                   OPTDW, OPTZW
      DOUBLE PRECISION   BIGNUM, CSCALE, EPS, HNR1, HNRM, NRMB, SMLNUM
      COMPLEX*16         TMP
C     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, MA02IZ
      EXTERNAL           DLAMCH, LSAME, MA02IZ
C     .. External Subroutines ..
      EXTERNAL           DCOPY, DLABAD, DLACPY, DLASCL, MB03XS, MB04DZ,
     $                   XERBLA, ZGEMM, ZGEQRF, ZLACPY, ZLAHQR, ZLASCL,
     $                   ZLASET
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCMPLX, DIMAG, INT, MAX, MIN, SQRT
C
C     .. Executable Statements ..
C
C     Decode the scalar input parameters.
C
      NN    = N*N
      N2    = 2*N
      NN2   = N2*N2
      INFO  = 0
      LSCAL = LSAME( BALANC, 'P' ) .OR. LSAME( BALANC, 'S' ) .OR.
     $        LSAME( BALANC, 'B' )
      WANTG = LSAME( JOB,    'G' )
      WANTS = LSAME( JOB,    'S' ) .OR. WANTG
      WANTU = LSAME( JOBU,   'U' )
C
      WANTUS = WANTS .AND. WANTU
C
      IF ( WANTS ) THEN
         K = N2
      ELSE
         K = N
      END IF
C
      IF ( N.EQ.0 ) THEN
         MINDW = 2
      ELSE IF ( WANTU ) THEN
         MINDB = 4*NN2 + N2
         IF ( WANTS ) THEN
            MINDW = MAX( 2, 20*NN + 12*N )
         ELSE
            MINDW = 20*NN + 12*N + 2
         END IF
      ELSE
         MINDB = 2*NN2 + N2
         IF ( WANTS ) THEN
            MINDW = MAX( 2, 12*NN + 4*N, 8*NN + 12*N )
         ELSE
            MINDW = MAX( 12*NN + 4*N, 8*NN + 12*N ) + 2
         END IF
      END IF
      IF ( WANTG .OR. WANTU ) THEN
         MINZW = MAX( 1, 12*N - 2 )
      ELSE IF ( WANTS ) THEN
         MINZW = MAX( 1, 12*N - 6 )
      ELSE
         MINZW = 1
      END IF
      LQUERY = LDWORK.EQ.-1 .OR. LZWORK.EQ.-1
C
C     Test the scalar input parameters.
C
      IF ( .NOT.LSCAL .AND. .NOT.LSAME( BALANC, 'N' ) ) THEN
         INFO = -1
      ELSE IF ( .NOT.WANTS .AND. .NOT.LSAME( JOB,  'E' ) ) THEN
         INFO = -2
      ELSE IF ( .NOT.WANTU .AND. .NOT.LSAME( JOBU, 'N' ) ) THEN
         INFO = -3
      ELSE IF ( N.LT.0 ) THEN
         INFO = -4
      ELSE IF (  LDA.LT.MAX( 1, K ) ) THEN
         INFO = -6
      ELSE IF ( LDQG.LT.MAX( 1, K ) ) THEN
         INFO = -8
      ELSE IF ( LDU1.LT.1 .OR. ( WANTUS .AND. LDU1.LT.N2 ) ) THEN
         INFO = -10
      ELSE IF ( LDU2.LT.1 .OR. ( WANTUS .AND. LDU2.LT.N2 ) ) THEN
         INFO = -12
      ELSE IF ( .NOT. LQUERY ) THEN
         IF ( LDWORK.LT.MINDW ) THEN
            DWORK( 1 ) = MINDW
            INFO = -18
         ELSE IF ( LZWORK.LT.MINZW ) THEN
            ZWORK( 1 ) = MINZW
            INFO = -20
         END IF
      END IF
C
C     Return if there were illegal values.
C
      IF ( INFO.NE.0 ) THEN
         CALL XERBLA( 'MB03XZ', -INFO )
         RETURN
      ELSE IF ( N.GT.0 ) THEN
C
C        Set the pointers for the inputs and outputs of MB03XS.
C
         IF ( WANTS ) THEN
            IA = 1
         ELSE
            IA = 3
         END IF
         IQG = IA + NN2
         IF ( WANTU ) THEN
            IU1  = IQG + NN2 + N2
            IU2  = IU1 + NN2
            IWRK = IU2 + NN2
         ELSE
            IU1  = IQG
            IU2  = IQG
            IWRK = IQG + NN2 + N2
         END IF
C
C        Compute optimal workspace.
C
         OPTZW = MINZW
         IF ( WANTS ) THEN
            CALL ZGEQRF( N2, N2, ZWORK, N2, ZWORK, ZWORK, -1, INFO )
            I  = INT( ZWORK( 1 ) )
            NB = MAX( I/N2, 2 )
         END IF
C
         IF ( LQUERY ) THEN
            CALL MB03XS( JOBU, N2, DWORK, N2, DWORK, N2, DWORK, N2,
     $                   DWORK, N2, WI, WR, DWORK, -1, INFO )
            OPTDW = MAX( MINDW, MINDB + INT( DWORK( 1 ) ) )
            DWORK( 1 ) = OPTDW
            ZWORK( 1 ) = OPTZW
            RETURN
         END IF
      END IF
C
C     Quick return if possible.
C
      ILO = 1
      IF ( N.EQ.0 ) THEN
         DWORK( 1 ) = TWO
         DWORK( 2 ) = ZERO
         ZWORK( 1 ) = CONE
         RETURN
      END IF
C
      EPS    = DLAMCH( 'P' )
      SMLNUM = DLAMCH( 'S' )
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
      SMLNUM = SQRT( SMLNUM ) / EPS
      BIGNUM = ONE / SMLNUM
C
C     Scale H if maximal element is outside range [SMLNUM,BIGNUM].
C
      HNRM = MA02IZ( 'Hamiltonian', 'MaxElement', N, A, LDA, QG, LDQG,
     $               DWORK )
      SCALEH = .FALSE.
      IF ( HNRM.GT.ZERO .AND. HNRM.LT.SMLNUM ) THEN
         SCALEH = .TRUE.
         CSCALE = SMLNUM
      ELSE IF ( HNRM.GT.BIGNUM ) THEN
         SCALEH = .TRUE.
         CSCALE = BIGNUM
      END IF
      IF ( SCALEH ) THEN
        CALL ZLASCL( 'General', 0, 0, HNRM, CSCALE, N, N, A, LDA, IERR )
        CALL ZLASCL( 'General', 0, 0, HNRM, CSCALE, N, N+1, QG, LDQG,
     $               IERR )
      END IF
C
C     Balance the matrix and compute the 1-norm.
C
      IF ( LSCAL ) THEN
         CALL MB04DZ( BALANC, N, A, LDA, QG, LDQG, ILO, SCALE, IERR )
      ELSE
         ILO = 1
      END IF
C
C     Real workspace:    need  2*N.
C
      HNR1 = MA02IZ( 'Hamiltonian', '1-norm', N, A, LDA, QG, LDQG,
     $               DWORK )
C
C     Set up the embeddings of the matrix H.
C
C     Real workspace:    need  w1 =  8*N**2 + 2*N, if JOBU = 'N';
C                              w1 = 16*N**2 + 2*N, if JOBU = 'U'.
C
C     Build the embedding of A.
C
      IW = IA
      IS = IW + N2*N
      DO 30 J = 1, N
         IW1 = IW
         DO 10 I = 1, N
            DWORK( IW ) = DIMAG( A( I, J ) )
            IW = IW + 1
   10    CONTINUE
C
         DO 20 I = 1, N
            DWORK( IW ) = -DBLE( A( I, J ) )
            DWORK( IS ) = -DWORK( IW )
            IW = IW + 1
            IS = IS + 1
   20    CONTINUE
         CALL DCOPY( N, DWORK( IW1 ), 1, DWORK( IS ), 1 )
         IS = IS + N
   30 CONTINUE
C
C     Build the embedding of G and Q.
C
      IW = IQG
      DO 60 J = 1, N + 1
         DO 40 I = 1, N
            DWORK( IW ) = DIMAG( QG( I, J ) )
            IW = IW + 1
   40    CONTINUE
C
         IW = IW + J - 1
         IS = IW
         DO 50 I = J, N
            DWORK( IW ) = -DBLE( QG( I, J ) )
            DWORK( IS ) =  DWORK( IW )
            IW = IW + 1
            IS = IS + N2
   50    CONTINUE
   60 CONTINUE
C
      IW1 = IW
      I1  = IW
      DO 80 J = 2, N + 1
         IS = I1
         I1 = I1 + 1
         DO 70 I = 1, J - 1
            DWORK( IW ) = DBLE( QG( I, J ) )
            DWORK( IS ) = DWORK( IW )
            IW = IW + 1
            IS = IS + N2
   70    CONTINUE
         IW = IW + N2 - J + 1
   80 CONTINUE
      CALL DLACPY( 'Full', N, N+1, DWORK( IQG ), N2, DWORK( IW1-N ),
     $             N2 )
C
C     Compute the eigenvalues and real skew-Hamiltonian Schur form of
C     the embedded skew-Hamiltonian matrix.
C
C     Real workspace:    need  w1 + w2, where
C                              w2 = max( 4*N**2 + 2*N,
C                                        10*N ),       if JOBU = 'N';
C                              w2 = 4*N**2 + 10*N,     if JOBU = 'U'.
C                        prefer larger.
C
      CALL MB03XS( JOBU, N2, DWORK( IA ), N2, DWORK( IQG ), N2,
     $             DWORK( IU1 ), N2, DWORK( IU2 ), N2, WI, WR,
     $             DWORK( IWRK ), LDWORK-IWRK+1, INFO )
      IF ( INFO.NE.0 )
     $   RETURN
C
      OPTDW = MAX( MINDW, INT( DWORK(IWRK) ) + IWRK - 1 )
C
C     Return if further reduction is not required.
C
      IF ( .NOT.WANTS ) THEN
         IF ( SCALEH ) THEN
C
C           Undo scaling.
C
            CALL DLASCL( 'Hessenberg', 0, 0, CSCALE, HNRM, N2, N2,
     $                   DWORK( IA ), N2, IERR )
            IF ( WANTG )
     $         CALL DLASCL( 'General', 0, 0, CSCALE, HNRM, N2, N2,
     $                      DWORK( IQG+N2 ), N2, IERR )
            CALL DLASCL( 'General', 0, 0, CSCALE, HNRM, N2, 1, WR, N2,
     $                   IERR )
            CALL DLASCL( 'General', 0, 0, CSCALE, HNRM, N2, 1, WI, N2,
     $                   IERR )
            HNR1 = HNR1 * HNRM / CSCALE
         END IF 
         GO TO 190
      END IF 
C
C     Convert the results to complex datatype. G starts now in the
C     first column of QG.
C     Only the upper triangular part of G is used below.
C
      IW = IA
      DO 100 J = 1, N2
         DO 90 I = 1, MIN( J+1, N2 )
            A( I, J ) = DCMPLX( ZERO, DWORK( IW ) )
            IW = IW + 1
   90    CONTINUE
         IW = IW + N2 - MIN( J+1, N2 )
  100 CONTINUE
C
      IF ( WANTG ) THEN
         IW = IQG + N2
         DO 120 J = 1, N2
            DO 110 I = 1, J - 1
               QG( I, J ) = DCMPLX( ZERO, DWORK( IW ) )
               IW = IW + 1
  110       CONTINUE
            QG( J, J ) = CZERO
            IW = IW + N2 - J + 1
  120    CONTINUE
      END IF 
C
      IF ( WANTU ) THEN
C
C        Set the transformation matrix.
C
         IW = IU1
         DO 140 J = 1, N2
            DO 130 I = 1, N2
               U1( I, J ) = DCMPLX( DWORK( IW ) )
               IW = IW + 1
  130       CONTINUE
  140    CONTINUE
C
         DO 160 J = 1, N2
            DO 150 I = 1, N2
               U2( I, J ) = DCMPLX( DWORK( IW ) )
               IW = IW + 1
  150       CONTINUE
  160    CONTINUE
      END IF
C
C     Triangularize the 2-by-2 diagonal blocks in Se using the complex
C     version of the QR algorithm.
C
C     Set up pointers on the outputs of ZLAHQR.
C     A block algorithm is used for large N2.
C
      IEV  = 1
      IU   = 3
      IWRK = IU + 4*( N2 - 1 )
C
      J  = 1
      J2 = MIN( N2, NB )
C     WHILE( J.LT.N2 ) DO
  170 CONTINUE
      IF ( J.LT.N2 ) THEN
         NRMB = ABS( A( J, J ) ) + ABS( A( J+1, J+1 ) )
         IF ( ABS( A( J+1, J ) ).GT.NRMB*EPS ) THEN
C
C           Triangularization step.
C           Workspace:    need   8*N - 2 (complex).
C
            NC  = MAX( J2-J-1, 0 )
            NC1 = MAX( J2-J+1, 0 )
            JM1 = MAX( J-1, 1 )
            JP2 = MIN( J+2, N2 )
            CALL ZLASET( 'Full', 2, 2, CZERO, CONE, ZWORK( IU ), 2 )
            CALL ZLAHQR( .TRUE., .TRUE., 2, 1, 2, A( J, J ), LDA,
     $                   ZWORK( IEV ), 1, 2, ZWORK( IU ), 2, INFO )
            IF ( INFO.GT.0 ) THEN
               INFO = N2 + 1
               RETURN
            END IF
C
C           Update A.
C           Workspace:    need   12*N - 6.
C
            CALL ZGEMM(  'No Transpose', 'No Transpose', J-1, 2, 2,
     $                   CONE, A( 1, J ), LDA, ZWORK( IU ), 2, CZERO,
     $                   ZWORK( IWRK ), JM1 )
            CALL ZLACPY( 'Full', J-1, 2, ZWORK( IWRK ), JM1, A( 1, J ),
     $                   LDA )
            CALL ZGEMM(  'Conjugate Transpose', 'No Transpose', 2, NC,
     $                   2, CONE, ZWORK( IU ), 2, A( J, JP2 ), LDA,
     $                   CZERO, ZWORK( IWRK ), 2 )
            CALL ZLACPY( 'Full', 2, NC, ZWORK( IWRK ), 2, A( J, JP2 ),
     $                   LDA )
C
            IF ( WANTG ) THEN
C
C              Update G.
C              Workspace:    need   12*N - 2.
C
               TMP          =  QG( J+1, J )
               QG( J+1, J ) = -QG( J, J+1 )
               CALL ZGEMM(  'No Transpose', 'No Transpose', J+1, 2, 2,
     $                      CONE, QG( 1, J ), LDQG, ZWORK( IU ), 2,
     $                      CZERO, ZWORK( IWRK ), J+1 )
               CALL ZLACPY( 'Full', J+1, 2, ZWORK( IWRK ), J+1,
     $                      QG( 1, J ), LDQG )
               CALL ZGEMM(  'Conjugate Transpose', 'No Transpose', 2,
     $                      NC1, 2, CONE, ZWORK( IU ), 2, QG( J, J ),
     $                      LDQG, CZERO, ZWORK( IWRK ), 2 )
               CALL ZLACPY( 'Full', 2, NC1, ZWORK( IWRK ), 2,
     $                      QG( J, J ), LDQG )
               QG( J+1, J ) = TMP
            END IF
C
            IF ( WANTU ) THEN
C
C              Update U.
C              Workspace:    need   12*N - 2.
C
               CALL ZGEMM(  'No Transpose', 'No Transpose', N2, 2, 2,
     $                      CONE, U1( 1, J ), LDU1, ZWORK( IU ), 2,
     $                      CZERO, ZWORK( IWRK ), N2 )
               CALL ZLACPY( 'Full', N2, 2, ZWORK( IWRK ), N2,
     $                      U1( 1, J ), LDU1 )
               CALL ZGEMM(  'No Transpose', 'No Transpose', N2, 2, 2,
     $                      CONE, U2( 1, J ), LDU2, ZWORK( IU ), 2,
     $                      CZERO, ZWORK( IWRK ), N2 )
               CALL ZLACPY( 'Full', N2, 2, ZWORK( IWRK ), N2,
     $                      U2( 1, J ), LDU2 )
            END IF
C
            BWORK( J ) = .TRUE.
            J  = J  + 2
            IU = IU + 4
         ELSE
            BWORK( J )  = .FALSE.
            A( J+1, J ) = CZERO
            J = J + 1
         END IF
C
         IF ( J.GE.J2 .AND. J.LT.N2 ) THEN
            J1 = J2 + 1
            J2 = MIN( N2, J1 + NB - 1 )
            NC = J2 - J1 + 1
C
C           Update the columns J1 to J2 of A and QG for previous
C           transformations.
C
            I   = 1
            IUB = 3
C           WHILE( I.LT.J ) DO
  180       CONTINUE
            IF ( I.LT.J ) THEN
               IF ( BWORK( I ) ) THEN
                  CALL ZGEMM(  'Conjugate Transpose', 'No Transpose', 2,
     $                         NC, 2, CONE, ZWORK( IUB ), 2, A( I, J1 ),
     $                         LDA, CZERO, ZWORK( IWRK ), 2 )
                  CALL ZLACPY( 'Full', 2, NC, ZWORK( IWRK ), 2,
     $                         A( I, J1 ), LDA )
C
                  IF ( WANTG ) THEN
                     CALL ZGEMM(  'Conjugate Transpose', 'No Transpose',
     $                            2, NC, 2, CONE, ZWORK( IUB ), 2,
     $                            QG( I, J1 ), LDQG, CZERO,
     $                            ZWORK( IWRK ), 2 )
                     CALL ZLACPY( 'Full', 2, NC, ZWORK( IWRK ), 2,
     $                            QG( I, J1 ), LDQG )
                  END IF
C
                  IUB = IUB + 4
C
                  I = I + 2
               ELSE
                  I = I + 1
               END IF
               GO TO 180
            END IF
C           END WHILE 180
         END IF
         GO TO 170
      END IF
C     END WHILE 170
C
      IF ( SCALEH ) THEN
C
C        Undo scaling.
C
         CALL ZLASCL( 'Hessenberg', 0, 0, CSCALE, HNRM, N2, N2, A, LDA,
     $                IERR )
         If ( WANTG )
     $      CALL ZLASCL( 'General', 0, 0, CSCALE, HNRM, N2, N2, QG(1,2),
     $                   LDQG, IERR )
         CALL DLASCL( 'General', 0, 0, CSCALE, HNRM, N2, 1, WR, N2,
     $                IERR )
         CALL DLASCL( 'General', 0, 0, CSCALE, HNRM, N2, 1, WI, N2,
     $                IERR )
         HNR1 = HNR1 * HNRM / CSCALE
      END IF 
C
  190 CONTINUE
      DWORK( 1 ) = DBLE( OPTDW )
      DWORK( 2 ) = HNR1
      ZWORK( 1 ) = OPTZW
      RETURN
C *** Last line of MB03XZ ***
      END
