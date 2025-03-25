      SUBROUTINE MB3LZP( COMPQ, ORTH, N, A, LDA, DE, LDDE, B, LDB, FG,
     $                   LDFG, NEIG, Q, LDQ, ALPHAR, ALPHAI, BETA,
     $                   IWORK, DWORK, LDWORK, ZWORK, LZWORK, BWORK,
     $                   INFO )
C
C     PURPOSE
C
C     To compute the eigenvalues of a complex N-by-N skew-Hamiltonian/
C     Hamiltonian pencil aS - bH, with
C
C           (  A  D  )         (  B  F  )
C       S = (        ) and H = (        ).                           (1)
C           (  E  A' )         (  G -B' )
C
C     The structured Schur form of the embedded real skew-Hamiltonian/
C     skew-Hamiltonian pencil aB_S - bB_T, defined as
C
C             (  Re(A)  -Im(A)  |  Re(D)  -Im(D)  )
C             (                 |                 )
C             (  Im(A)   Re(A)  |  Im(D)   Re(D)  )
C             (                 |                 )
C       B_S = (-----------------+-----------------) , and
C             (                 |                 )
C             (  Re(E)  -Im(E)  |  Re(A')  Im(A') )
C             (                 |                 )
C             (  Im(E)   Re(E)  | -Im(A')  Re(A') )
C                                                                    (2)
C             ( -Im(B)  -Re(B)  | -Im(F)  -Re(F)  )
C             (                 |                 )
C             (  Re(B)  -Im(B)  |  Re(F)  -Im(F)  )
C             (                 |                 )
C       B_T = (-----------------+-----------------) ,  T = i*H,
C             (                 |                 )
C             ( -Im(G)  -Re(G)  | -Im(B')  Re(B') )
C             (                 |                 )
C             (  Re(G)  -Im(G)  | -Re(B') -Im(B') )
C
C     is determined and used to compute the eigenvalues. The notation M'
C     denotes the conjugate transpose of the matrix M. Optionally,
C     if COMPQ = 'C', an orthonormal basis of the right deflating
C     subspace of the pencil aS - bH, corresponding to the eigenvalues
C     with strictly negative real part, is computed. Namely, after
C     transforming aB_S - bB_H by unitary matrices, we have
C
C                ( BA  BD  )              ( BB  BF  )
C       B_Sout = (         ) and B_Hout = (         ),               (3)
C                (  0  BA' )              (  0 -BB' )
C
C     and the eigenvalues with strictly negative real part of the
C     complex pencil aB_Sout - bB_Hout are moved to the top. The
C     embedding doubles the multiplicities of the eigenvalues of the
C     pencil aS - bH.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     COMPQ   CHARACTER*1
C             Specifies whether to compute the deflating subspace
C             corresponding to the eigenvalues of aS - bH with strictly
C             negative real part.
C             = 'N': do not compute the deflating subspace; compute the
C                    eigenvalues only;
C             = 'C': compute the deflating subspace and store it in the
C                    leading subarray of Q.
C
C     ORTH    CHARACTER*1
C             If COMPQ = 'C', specifies the technique for computing an
C             orthonormal basis of the deflating subspace, as follows:
C             = 'P':  QR factorization with column pivoting;
C             = 'S':  singular value decomposition.
C             If COMPQ = 'N', the ORTH value is not used.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the pencil aS - bH.  N >= 0, even.
C
C     A       (input/output) COMPLEX*16 array, dimension (LDA, N)
C             On entry, the leading N/2-by-N/2 part of this array must
C             contain the matrix A.
C             On exit, if COMPQ = 'C', the leading N-by-N part of this
C             array contains the upper triangular matrix BA in (3) (see
C             also METHOD). The strictly lower triangular part is not
C             zeroed; it is preserved in the leading N/2-by-N/2 part.
C             If COMPQ = 'N', this array is unchanged on exit.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1, N).
C
C     DE      (input/output) COMPLEX*16 array, dimension (LDDE, N)
C             On entry, the leading N/2-by-N/2 lower triangular part of
C             this array must contain the lower triangular part of the
C             skew-Hermitian matrix E, and the N/2-by-N/2 upper
C             triangular part of the submatrix in the columns 2 to N/2+1
C             of this array must contain the upper triangular part of
C             the skew-Hermitian matrix D.
C             On exit, if COMPQ = 'C', the leading N-by-N part of this
C             array contains the skew-Hermitian matrix BD in (3) (see
C             also METHOD). The strictly lower triangular part of the
C             input matrix is preserved.
C             If COMPQ = 'N', this array is unchanged on exit.
C
C     LDDE    INTEGER
C             The leading dimension of the array DE.  LDDE >= MAX(1, N).
C
C     B       (input/output) COMPLEX*16 array, dimension (LDB, N)
C             On entry, the leading N/2-by-N/2 part of this array must
C             contain the matrix B.
C             On exit, if COMPQ = 'C', the leading N-by-N part of this
C             array contains the upper triangular matrix BB in (3) (see
C             also METHOD). The strictly lower triangular part is not
C             zeroed; the elements below the first subdiagonal of the
C             input matrix are preserved.
C             If COMPQ = 'N', this array is unchanged on exit.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= MAX(1, N).
C
C     FG      (input/output) COMPLEX*16 array, dimension (LDFG, N)
C             On entry, the leading N/2-by-N/2 lower triangular part of
C             this array must contain the lower triangular part of the
C             Hermitian matrix G, and the N/2-by-N/2 upper triangular
C             part of the submatrix in the columns 2 to N/2+1 of this
C             array must contain the upper triangular part of the
C             Hermitian matrix F.
C             On exit, if COMPQ = 'C', the leading N-by-N part of this
C             array contains the Hermitian matrix BF in (3) (see also
C             METHOD). The strictly lower triangular part of the input
C             matrix is preserved. The diagonal elements might have tiny
C             imaginary parts.
C             If COMPQ = 'N', this array is unchanged on exit.
C
C     LDFG    INTEGER
C             The leading dimension of the array FG.  LDFG >= MAX(1, N).
C
C     NEIG    (output) INTEGER
C             If COMPQ = 'C', the number of eigenvalues in aS - bH with
C             strictly negative real part.
C
C     Q       (output) COMPLEX*16 array, dimension (LDQ, 2*N)
C             On exit, if COMPQ = 'C', the leading N-by-NEIG part of
C             this array contains an orthonormal basis of the right
C             deflating subspace corresponding to the eigenvalues of the
C             pencil aS - bH with strictly negative real part.
C             The remaining entries are meaningless.
C             If COMPQ = 'N', this array is not referenced.
C
C     LDQ     INTEGER
C             The leading dimension of the array Q.
C             LDQ >= 1,           if COMPQ = 'N';
C             LDQ >= MAX(1, 2*N), if COMPQ = 'C'.
C
C     ALPHAR  (output) DOUBLE PRECISION array, dimension (N)
C             The real parts of each scalar alpha defining an eigenvalue
C             of the pencil aS - bH.
C
C     ALPHAI  (output) DOUBLE PRECISION array, dimension (N)
C             The imaginary parts of each scalar alpha defining an
C             eigenvalue of the pencil aS - bH.
C             If ALPHAI(j) is zero, then the j-th eigenvalue is real.
C
C     BETA    (output) DOUBLE PRECISION array, dimension (N)
C             The scalars beta that define the eigenvalues of the pencil
C             aS - bH.
C             Together, the quantities alpha = (ALPHAR(j),ALPHAI(j)) and
C             beta = BETA(j) represent the j-th eigenvalue of the pencil
C             aS - bH, in the form lambda = alpha/beta. Since lambda may
C             overflow, the ratios should not, in general, be computed.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (N+1)
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal LDWORK.
C             On exit, if INFO = -20, DWORK(1) returns the minimum value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The dimension of the array DWORK.
C             LDWORK >= MAX( 4*N*N + 2*N + MAX(3,N) ), if COMPQ = 'N';
C             LDWORK >= MAX( 1, 11*N*N + 2*N ),        if COMPQ = 'C'.
C             For good performance LDWORK should be generally larger.
C
C             If LDWORK = -1, then a workspace query is assumed;
C             the routine only calculates the optimal size of the
C             DWORK array, returns this value as the first entry of
C             the DWORK array, and no error message related to LDWORK
C             is issued by XERBLA.
C
C     ZWORK   COMPLEX*16 array, dimension (LZWORK)
C             On exit, if INFO = 0, ZWORK(1) returns the optimal LZWORK.
C             On exit, if INFO = -22, ZWORK(1) returns the minimum value
C             of LZWORK.
C
C     LZWORK  INTEGER
C             The dimension of the array ZWORK.
C             LZWORK >= 1,       if COMPQ = 'N';
C             LZWORK >= 8*N + 4, if COMPQ = 'C'.
C             For good performance LZWORK should be generally larger.
C
C             If LZWORK = -1, then a workspace query is assumed;
C             the routine only calculates the optimal size of the
C             ZWORK array, returns this value as the first entry of
C             the ZWORK array, and no error message related to LZWORK
C             is issued by XERBLA.
C
C     BWORK   LOGICAL array, dimension (LBWORK)
C             LBWORK >= 0,     if COMPQ = 'N';
C             LBWORK >= N - 1, if COMPQ = 'C'.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0: succesful exit;
C             < 0: if INFO = -i, the i-th argument had an illegal value;
C             = 1: QZ iteration failed in the SLICOT Library routine
C                  MB04FP (QZ iteration did not converge or computation
C                  of the shifts failed);
C             = 2: QZ iteration failed in the LAPACK routine ZHGEQZ when
C                  trying to triangularize the 2-by-2 blocks;
C             = 3: the singular value decomposition failed in the LAPACK
C                  routine ZGESVD (for ORTH = 'S');
C             = 4: warning: the pencil is numerically singular.
C
C     METHOD
C
C     First, T = i*H is set. Then, the embeddings, B_S and B_T, of the
C     matrices S and T, are determined and, subsequently, the SLICOT
C     Library routine MB04FP is applied to compute the structured Schur
C     form, i.e., the factorizations
C
C     ~                     (  S11  S12  )
C     B_S = J Q' J' B_S Q = (            ) and
C                           (   0   S11' )
C
C     ~                     (  T11  T12  )           (  0  I  )
C     B_T = J Q' J' B_T Q = (            ), with J = (        ),
C                           (   0   T11' )           ( -I  0  )
C
C     where Q is real orthogonal, S11 is upper triangular, and T11 is
C     upper quasi-triangular.
C
C     Second, the SLICOT Library routine MB3JZP is applied, to compute a
C                    ~
C     unitary matrix Q, such that
C
C                        ~    ~
C       ~     ~   ~   (  S11  S12  )
C     J Q' J' B_S Q = (       ~    ) =: B_Sout,
C                     (   0   S11' )
C
C       ~        ~    ~   (  H11  H12  )
C     J Q' J'(-i*B_T) Q = (            ) =: B_Hout,
C                         (   0  -H11' )
C          ~                                               ~       ~
C     with S11, H11 upper triangular, and such that Spec_-(B_S, -i*B_T)
C     is contained in the spectrum of the 2*NEIG-by-2*NEIG leading
C                          ~
C     principal subpencil aS11 - bH11.
C
C     Finally, the right deflating subspace is computed.
C     See also page 22 in [1] for more details.
C
C     REFERENCES
C
C     [1] Benner, P., Byers, R., Mehrmann, V. and Xu, H.
C         Numerical Computation of Deflating Subspaces of Embedded
C         Hamiltonian Pencils.
C         Tech. Rep. SFB393/99-15, Technical University Chemnitz,
C         Germany, June 1999.
C
C     NUMERICAL ASPECTS
C                                                               3
C     The algorithm is numerically backward stable and needs O(N )
C     complex floating point operations.
C
C     FURTHER COMMENTS
C
C     This routine does not perform any scaling of the matrices. Scaling
C     might sometimes be useful, and it should be done externally.
C     For large values of N, the routine applies the transformations on
C     panels of columns. The user may specify in INFO the desired number
C     of columns. If on entry INFO <= 0, then the routine estimates a
C     suitable value of this number.
C
C     CONTRIBUTOR
C
C     V. Sima, Research Institute for Informatics, Bucharest, Jan. 2011.
C     M. Voigt, Max Planck Institute for Dynamics of Complex Technical
C     Systems, Dec. 2011.
C
C     REVISIONS
C
C     V. Sima, Mar. 2011, Aug. 2011, Nov. 2011, July 2013, June 2014,
C     Nov. 2014, Jan. 2017, Apr. 2020.
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
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
      COMPLEX*16         CZERO, CONE, CIMAG
      PARAMETER          ( CZERO = ( 0.0D+0, 0.0D+0 ),
     $                      CONE = ( 1.0D+0, 0.0D+0 ),
     $                     CIMAG = ( 0.0D+0, 1.0D+0 ) )
C
C     .. Scalar Arguments ..
      CHARACTER          COMPQ, ORTH
      INTEGER            INFO, LDA, LDB, LDDE, LDFG, LDQ, LDWORK,
     $                   LZWORK, N, NEIG
C
C     .. Array Arguments ..
      LOGICAL            BWORK( * )
      INTEGER            IWORK( * )
      DOUBLE PRECISION   ALPHAI( * ), ALPHAR( * ), BETA( * ), DWORK( * )
      COMPLEX*16         A( LDA, * ), B( LDB, * ), DE( LDDE, * ),
     $                   FG( LDFG, * ), Q( LDQ, * ), ZWORK( * )
C
C     .. Local Scalars ..
      LOGICAL            LCMPQ, LQUERY, QR, QRP, SVD
      CHARACTER*14       CMPQ, JOB
      INTEGER            I, I1, IA, IB, IDE, IEV, IFG, IQ, IQ2, IQB, IS,
     $                   ITAU, IW, IW1, IWA, IWRK, J, J1, J2, JM1, JP2,
     $                   M, MINDB, MINDW, MINZW, N2, NB, NBL, NC, NN,
     $                   OPTDW, OPTZW
      DOUBLE PRECISION   EPS, NRMB, TOL
      COMPLEX*16         TMP
C
C     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH, LSAME
C
C     .. External Subroutines ..
      EXTERNAL           DCOPY, DLACPY, DSCAL, MB3JZP, MB04FP, XERBLA,
     $                   ZAXPY, ZGEMM, ZGEQP3, ZGEQRF, ZGESVD, ZHGEQZ,
     $                   ZLACPY, ZSCAL, ZUNGQR
C
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCMPLX, DIMAG, INT, MAX, MIN, MOD,
     $                   SQRT
C
C     .. Executable Statements ..
C
C     Decode the input arguments.
C     Using ORTH = 'Q' is not safe, but sometimes gives better results.
C
      NBL  = INFO
      M    = N/2
      NN   = N*N
      N2   = 2*N
      NEIG = 0
      LCMPQ = LSAME( COMPQ, 'C' )
      IF( LCMPQ ) THEN
         QR  = LSAME( ORTH, 'Q' )
         QRP = LSAME( ORTH, 'P' )
         SVD = LSAME( ORTH, 'S' )
      ELSE
         QR  = .FALSE.
         QRP = .FALSE.
         SVD = .FALSE.
      END IF
C
      IF( N.EQ.0 ) THEN
         MINDW = 1
         MINZW = 1
      ELSE IF( LCMPQ ) THEN
         MINDB =  8*NN + N2
         MINDW = 11*NN + N2
         MINZW =  8*N  + 4
      ELSE
         MINDB = 4*NN + N2
         MINDW = 4*NN + N2 + MAX( 3, N )
         MINZW = 1
      END IF
      LQUERY = LDWORK.EQ.-1 .OR. LZWORK.EQ.-1
C
C     Test the input arguments.
C
      INFO = 0
      IF( .NOT.( LSAME( COMPQ, 'N' ) .OR. LCMPQ ) ) THEN
         INFO = -1
      ELSE IF( LCMPQ ) THEN
         IF( .NOT.( QR .OR. QRP .OR. SVD ) )
     $      INFO = -2
      ELSE IF( N.LT.0 .OR. MOD( N, 2 ).NE.0 ) THEN
         INFO = -3
      ELSE IF(  LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LDDE.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF(  LDB.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE IF( LDFG.LT.MAX( 1, N ) ) THEN
         INFO = -11
      ELSE IF( LDQ.LT.1 .OR. ( LCMPQ .AND. LDQ.LT.N2 ) ) THEN
         INFO = -14
      ELSE IF( .NOT. LQUERY ) THEN
         IF( LDWORK.LT.MINDW ) THEN
            DWORK( 1 ) = MINDW
            INFO = -20
         ELSE IF( LZWORK.LT.MINZW ) THEN
            ZWORK( 1 ) = MINZW
            INFO = -22
         END IF
      END IF
C
      IF( INFO.NE.0) THEN
         CALL XERBLA( 'MB3LZP', -INFO )
         RETURN
      ELSE IF( N.GT.0 ) THEN
C
C        Compute optimal workspace.
C
         IF( LCMPQ ) THEN
            JOB  = 'Triangularize'
            CMPQ = 'Initialize'
            CALL ZGEQRF( N, N, Q, LDQ, ZWORK, ZWORK, -1, INFO )
            I  = INT( ZWORK( 1 ) )
            NB = MAX( I/N, 2 )
         ELSE
            JOB  = 'Eigenvalues'
            CMPQ = 'No Computation'
         END IF
C
         IF( LQUERY ) THEN
            CALL MB04FP( JOB, CMPQ, N2, DWORK, N, DWORK, N, DWORK, N,
     $                   DWORK, N, DWORK, N2, ALPHAI, ALPHAR, BETA,
     $                   IWORK, DWORK, -1, INFO )
            OPTDW = MAX( MINDW, MINDB + INT( DWORK( 1 ) ) )
C
            IF( LCMPQ ) THEN
               IF( SVD ) THEN
                  CALL ZGESVD( 'O', 'N', N, N, Q, LDQ, DWORK, ZWORK, 1,
     $                         ZWORK, 1, ZWORK, -1, DWORK, INFO )
                  J = INT( ZWORK( 1 ) )
               ELSE
                  IF( QR ) THEN
                     J = M
                     CALL ZGEQRF( N, J, Q, LDQ, ZWORK, ZWORK, -1, INFO )
                  ELSE
                     J = N
                     CALL ZGEQP3( N, J, Q, LDQ, IWORK, ZWORK, ZWORK, -1,
     $                            DWORK, INFO )
                  END IF
                  CALL ZUNGQR( N, J, J, Q, LDQ, ZWORK, ZWORK( 2 ), -1,
     $                         INFO )
                  J = J + MAX( INT( ZWORK( 1 ) ), INT( ZWORK( 2 ) ) )
               END IF
               OPTZW = MAX( MINZW, I, J )
            ELSE
               OPTZW = MINZW
            END IF
            DWORK( 1 ) = OPTDW
            ZWORK( 1 ) = OPTZW
            RETURN
         ELSE
            OPTZW = MINZW
         END IF
      END IF
C
C     Quick return if possible.
C
      IF( N.EQ.0 ) THEN
         DWORK( 1 ) = ONE
         ZWORK( 1 ) = CONE
         RETURN
      END IF
C
C     Determine machine constants.
C
      EPS = DLAMCH( 'Precision' )
      TOL = SQRT( EPS )
C
C     Set up the embeddings of the matrices S and H.
C
C     Set the pointers for the inputs and outputs of MB04FP.
C     Real workspace:    need   4*N**2+2*N, if COMPQ = 'N';
C                               8*N**2+2*N, if COMPQ = 'C'.
C
      IQ = 1
      IF( LCMPQ ) THEN
         IA = IQ + N2*N2
      ELSE
         IA = 1
      END IF
C
      IDE  = IA  + NN
      IB   = IDE + NN + N
      IFG  = IB  + NN
      IWRK = IFG + NN + N
C
C     Build the embedding of A.
C
      IW = IA
      IS = IW + N*M
      DO 30 J = 1, M
         IW1 = IW
         DO 10 I = 1, M
            DWORK( IW ) = DBLE( A( I, J ) )
            IW = IW + 1
   10    CONTINUE
C
         DO 20 I = 1, M
            DWORK( IW ) =  DIMAG( A( I, J ) )
            DWORK( IS ) = -DWORK( IW )
            IW = IW + 1
            IS = IS + 1
   20    CONTINUE
         CALL DCOPY( M, DWORK( IW1 ), 1, DWORK( IS ), 1 )
         IS = IS + M
   30 CONTINUE
C
C     Build the embedding of D and E.
C
      IW = IDE
      DO 60 J = 1, M + 1
         DO 40 I = 1, M
            DWORK( IW ) = DBLE( DE( I, J ) )
            IW = IW + 1
   40    CONTINUE
C
         IW = IW + J - 1
         IS = IW
         DO 50 I = J, M
            DWORK( IW ) = DIMAG( DE( I, J ) )
            DWORK( IS ) = DWORK( IW )
            IW = IW + 1
            IS = IS + N
   50    CONTINUE
   60 CONTINUE
C
      IW1 = IW
      I1  = IW
      DO 80 J = 2, M + 1
         IS = I1
         I1 = I1 + 1
         DO 70 I = 1, J - 1
            DWORK( IW ) = -DIMAG( DE( I, J ) )
            DWORK( IS ) =  DWORK( IW )
            IW = IW + 1
            IS = IS + N
   70    CONTINUE
         IW = IW + N - J + 1
   80 CONTINUE
      CALL DLACPY( 'Full', M, M+1, DWORK( IDE ), N, DWORK( IW1-M ), N )
C
C     Build the embedding of B.
C
      IW = IB
      IS = IW + N*M
      DO 110 J = 1, M
         IW1 = IW
         DO 90 I = 1, M
            DWORK( IW ) = -DIMAG( B( I, J ) )
            IW = IW + 1
   90    CONTINUE
C
         DO 100 I = 1, M
            DWORK( IW ) =   DBLE( B( I, J ) )
            DWORK( IS ) = -DWORK( IW )
            IW = IW + 1
            IS = IS + 1
  100    CONTINUE
         CALL DCOPY( M, DWORK( IW1 ), 1, DWORK( IS ), 1 )
         IS = IS + M
  110 CONTINUE
C
C     Build the embedding of F and G.
C
      IW = IFG
      DO 140 J = 1, M + 1
         DO 120 I = 1, M
            DWORK( IW ) = -DIMAG( FG( I, J ) )
            IW = IW + 1
  120    CONTINUE
C
         IW = IW + J - 1
         IS = IW
         DO 130 I = J, M
            DWORK( IW ) =  DBLE( FG( I, J ) )
            DWORK( IS ) = DWORK( IW )
            IW = IW + 1
            IS = IS + N
  130    CONTINUE
  140 CONTINUE
C
      IW1 = IW
      I1  = IW
      DO 160 J = 2, M + 1
         IS = I1
         I1 = I1 + 1
         DO 150 I = 1, J - 1
            DWORK( IW ) = -DBLE( FG( I, J ) )
            DWORK( IS ) = DWORK( IW )
            IW = IW + 1
            IS = IS + N
  150    CONTINUE
         IW = IW + N - J + 1
  160 CONTINUE
      CALL DLACPY( 'Full', M, M+1, DWORK( IFG ), N, DWORK( IW1-M ), N )
C
C     STEP 1: Apply MB04FP to transform the extended pencil to real
C             skew-Hamiltonian/skew-Hamiltonian Schur form.
C
C             Real workspace:
C                  need   4*N*N + 2*N + MAX( 3, N ), if COMPQ = 'N';
C                         8*N*N + 2*N + 3*N*N,       if COMPQ = 'C'.
C                  prefer larger.
C
      INFO = NBL
      CALL MB04FP( JOB, CMPQ, N2, DWORK( IA ), N, DWORK( IDE ), N,
     $             DWORK( IB ), N, DWORK( IFG ), N, DWORK( IQ ), N2,
     $             ALPHAI, ALPHAR, BETA, IWORK, DWORK( IWRK ),
     $             LDWORK-IWRK+1, INFO )
      IF( INFO.EQ.2 ) THEN
         IWA = 4
      ELSE IF( INFO.GT.0 ) THEN
         RETURN
      ELSE
         IWA = 0
      END IF
      OPTDW = MAX( MINDW, MINDB + INT( DWORK( IWRK ) ) )
C
C     Scale the eigenvalues.
C
      CALL DSCAL( N, -ONE, ALPHAI, 1 )
C
C     Return if only the eigenvalues are desired.
C
      IF( .NOT.LCMPQ ) THEN
         DWORK( 1 ) = OPTDW
         ZWORK( 1 ) = OPTZW
         INFO = IWA
         RETURN
      END IF
C
C     Convert the results to complex datatype. D and F start in the
C     first column of DE and FG, respectively.
C
      IW = IA
      DO 180 J = 1, N
         DO 170 I = 1, J
            A( I, J ) = DCMPLX( DWORK( IW ) )
            IW = IW + 1
  170    CONTINUE
         IF( J.GE.M .AND. J.LT.N )
     $      A( J+1, J ) = CZERO
         IW = IW + N - J
  180 CONTINUE
C
      IW = IDE + N
      DO 200 J = 1, N
         DO 190 I = 1, J - 1
            DE( I, J ) = DCMPLX( DWORK( IW ) )
            IW = IW + 1
  190    CONTINUE
         DE( J, J ) = CZERO
         IF( J.GE.M .AND. J.LT.N )
     $      DE( J+1, J ) = CZERO
         IW = IW + N - J + 1
  200 CONTINUE
C
      IW = IB
      DO 220 J = 1, N
         DO 210 I = 1, MIN( J + 1, N )
            B( I, J ) = DCMPLX( DWORK( IW ) )
            IW = IW + 1
  210    CONTINUE
         IW = IW + N - J - 1
  220 CONTINUE
C
      IW = IFG + N
      DO 240 J = 1, N
         DO 230 I = 1, J - 1
            FG( I, J ) = DCMPLX( DWORK( IW ) )
            IW = IW + 1
  230    CONTINUE
         FG( J, J ) = CZERO
         IF( J.GE.M .AND. J.LT.N )
     $      FG( J+1, J ) = CZERO
         IW = IW + N - J + 1
  240 CONTINUE
C
      IW = IQ
      DO 260 J = 1, N2
         DO 250 I = 1, N2
            Q( I, J ) = DCMPLX( DWORK( IW ) )
            IW = IW + 1
  250    CONTINUE
  260 CONTINUE
C
C     Triangularize the 2-by-2 diagonal blocks in B using the complex
C     version of the QZ algorithm.
C
C     Set up pointers on the outputs of ZHGEQZ.
C     A block algorithm is used for large N.
C
      IQ2  = 1
      IEV  = 5
      IQ   = 9
      IWRK = IQ + 4*( N - 1 )
C
      J  = 1
      J1 = 1
      J2 = MIN( N, J1 + NB - 1 )
C     WHILE( J.LT.N ) DO
  270 CONTINUE
      IF( J.LT.N ) THEN
         NRMB = ABS( B( J, J ) ) + ABS( B( J+1, J+1 ) )
         IF( ABS( B( J+1, J ) ).GT.NRMB*EPS ) THEN
C
C           Triangularization step.
C           Workspace:    need   8*N + 4.
C
            NC  = MAX( J2-J-1, 0 )
            JM1 = MAX( J-1, 1 )
            JP2 = MIN( J+2, N )
            TMP         =  A( J+1, J )
            A( J+1, J ) = CZERO
            CALL ZHGEQZ( 'Schur Form', 'Initialize', 'Initialize', 2, 1,
     $                   2, B( J, J ), LDB, A( J, J ), LDA,
     $                   ZWORK( IEV ), ZWORK( IEV+2 ), ZWORK( IQ ), 2,
     $                   ZWORK( IQ2 ), 2, ZWORK( IWRK ), LZWORK-IWRK+1,
     $                   DWORK, INFO )
            A( J+1, J ) = TMP
            IF( INFO.GT.0 ) THEN
               INFO = 2
               RETURN
            END IF
C
C           Update A.
C
            CALL ZGEMM(  'No Transpose', 'No Transpose', J-1, 2, 2,
     $                   CONE, A( 1, J ), LDA, ZWORK( IQ2 ), 2, CZERO,
     $                   ZWORK( IWRK ), JM1 )
            CALL ZLACPY( 'Full', J-1, 2, ZWORK( IWRK ), JM1, A( 1, J ),
     $                   LDA )
            CALL ZGEMM(  'Conjugate Transpose', 'No Transpose', 2, NC,
     $                   2, CONE, ZWORK( IQ ), 2, A( J, JP2 ), LDA,
     $                   CZERO, ZWORK( IWRK ), 2 )
            CALL ZLACPY( 'Full', 2, NC, ZWORK( IWRK ), 2, A( J, JP2 ),
     $                   LDA )
C
C           Update DE.
C
            TMP          =  DE( J+1, J )
            DE( J+1, J ) = -DE( J, J+1 )
            CALL ZGEMM(  'No Transpose', 'No Transpose', J+1, 2, 2,
     $                   CONE, DE( 1, J ), LDDE, ZWORK( IQ ), 2, CZERO,
     $                   ZWORK( IWRK ), J+1 )
            CALL ZLACPY( 'Full', J+1, 2, ZWORK( IWRK ), J+1, DE( 1, J ),
     $                   LDDE )
            CALL ZGEMM(  'Conjugate Transpose', 'No Transpose', 2,
     $                   J2-J+1, 2, CONE, ZWORK( IQ ), 2, DE( J, J ),
     $                   LDDE, CZERO, ZWORK( IWRK ), 2 )
            CALL ZLACPY( 'Full', 2, J2-J+1, ZWORK( IWRK ), 2,
     $                   DE( J, J ), LDDE )
            DE( J+1, J ) = TMP
C
C           Update B.
C
            CALL ZGEMM(  'No Transpose', 'No Transpose', J-1, 2, 2,
     $                   CONE, B( 1, J ), LDB, ZWORK( IQ2 ), 2, CZERO,
     $                   ZWORK( IWRK ), JM1 )
            CALL ZLACPY( 'Full', J-1, 2, ZWORK( IWRK ), JM1, B( 1, J ),
     $                   LDB )
            CALL ZGEMM(  'Conjugate Transpose', 'No Transpose', 2, NC,
     $                   2, CONE, ZWORK( IQ ), 2, B( J, JP2 ), LDB,
     $                   CZERO, ZWORK( IWRK ), 2 )
            CALL ZLACPY( 'Full', 2, NC, ZWORK( IWRK ), 2, B( J, JP2 ),
     $                   LDB )
C
C           Update FG.
C
            TMP          =  FG( J+1, J )
            FG( J+1, J ) = -FG( J, J+1 )
            CALL ZGEMM(  'No Transpose', 'No Transpose', J+1, 2, 2,
     $                   CONE, FG( 1, J ), LDFG, ZWORK( IQ ), 2, CZERO,
     $                   ZWORK( IWRK ), J+1 )
            CALL ZLACPY( 'Full', J+1, 2, ZWORK( IWRK ), J+1, FG( 1, J ),
     $                   LDFG )
            CALL ZGEMM(  'Conjugate Transpose', 'No Transpose', 2,
     $                   J2-J+1, 2, CONE, ZWORK( IQ ), 2, FG( J, J ),
     $                   LDFG, CZERO, ZWORK( IWRK ), 2 )
            CALL ZLACPY( 'Full', 2, J2-J+1, ZWORK( IWRK ), 2,
     $                   FG( J, J ), LDFG )
            FG( J+1, J ) = TMP
C
C           Update Q.
C
            CALL ZGEMM(  'No Transpose', 'No Transpose', N2, 2, 2, CONE,
     $                   Q( 1, J ), LDQ, ZWORK( IQ2 ), 2, CZERO,
     $                   ZWORK( IWRK ), N2 )
            CALL ZLACPY( 'Full', N2, 2, ZWORK( IWRK ), N2, Q( 1, J ),
     $                   LDQ )
            CALL ZGEMM(  'No Transpose', 'No Transpose', N2, 2, 2, CONE,
     $                   Q( 1, N+J ), LDQ, ZWORK( IQ ), 2, CZERO,
     $                   ZWORK( IWRK ), N2 )
            CALL ZLACPY( 'Full', N2, 2, ZWORK( IWRK ), N2, Q( 1, N+J ),
     $                   LDQ )
C
            BWORK( J ) = .TRUE.
            J  = J  + 2
            IQ = IQ + 4
         ELSE
            BWORK( J )  = .FALSE.
            B( J+1, J ) = CZERO
            J = J + 1
         END IF
C
         IF( J.GE.J2 ) THEN
            J1 = J2 + 1
            J2 = MIN( N, J1 + NB - 1 )
            NC = J2 - J1 + 1
C
C           Update the columns J1 to J2 of A, DE, B, and FG for previous
C           transformations.
C
            I   = 1
            IQB = 9
C           WHILE( I.LT.J ) DO
  280       CONTINUE
            IF( I.LT.J ) THEN
               IF( BWORK( I ) ) THEN
                  CALL ZGEMM(  'Conjugate Transpose', 'No Transpose', 2,
     $                         NC, 2, CONE, ZWORK( IQB ), 2, A( I, J1 ),
     $                         LDA, CZERO, ZWORK( IWRK ), 2 )
                  CALL ZLACPY( 'Full', 2, NC, ZWORK( IWRK ), 2,
     $                         A( I, J1 ), LDA )
C
                  CALL ZGEMM(  'Conjugate Transpose', 'No Transpose', 2,
     $                         NC, 2, CONE, ZWORK( IQB ), 2,
     $                         DE( I, J1 ), LDDE, CZERO, ZWORK( IWRK ),
     $                         2 )
                  CALL ZLACPY( 'Full', 2, NC, ZWORK( IWRK ), 2,
     $                         DE( I, J1 ), LDDE )
C
                  CALL ZGEMM(  'Conjugate Transpose', 'No Transpose', 2,
     $                         NC, 2, CONE, ZWORK( IQB ), 2, B( I, J1 ),
     $                         LDB, CZERO, ZWORK( IWRK ), 2 )
                  CALL ZLACPY( 'Full', 2, NC, ZWORK( IWRK ), 2,
     $                         B( I, J1 ), LDB )
C
                  CALL ZGEMM(  'Conjugate Transpose', 'No Transpose', 2,
     $                         NC, 2, CONE, ZWORK( IQB ), 2,
     $                         FG( I, J1 ), LDFG, CZERO, ZWORK( IWRK ),
     $                         2 )
                  CALL ZLACPY( 'Full', 2, NC, ZWORK( IWRK ), 2,
     $                         FG( I, J1 ), LDFG )
                  IQB = IQB + 4
C
                  I = I + 2
               ELSE
                  I = I + 1
               END IF
               GO TO 280
            END IF
C           END WHILE 280
         END IF
         GO TO 270
      END IF
C     END WHILE 270
C
C     Scale B and FG by -i.
C
      DO 290 I = 1, N
         CALL ZSCAL( I, -CIMAG, B( 1, I ), 1 )
  290 CONTINUE
C
      DO 300 I = 1, N
         CALL ZSCAL( I, -CIMAG, FG( 1, I ), 1 )
  300 CONTINUE
C
C     STEP 2: Apply MB3JZP to reorder the eigenvalues with strictly
C             negative real part to the top.
C
      CMPQ = 'Update'
      INFO = NBL
      CALL MB3JZP( CMPQ, N2, A, LDA, DE, LDDE, B, LDB, FG, LDFG, Q, LDQ,
     $             NEIG, TOL, DWORK, ZWORK, INFO )
C
      IF( QR )
     $   NEIG = NEIG/2
      ITAU = 1
      IWRK = NEIG + 1
C
C     STEP 3: Compute the right deflating subspace corresponding to
C             the eigenvalues with strictly negative real part.
C
      IF( NEIG.LE.M ) THEN
         DO 310 I = 1, NEIG
            CALL ZAXPY( M, CIMAG, Q( M+1, I ), 1, Q( 1, I ), 1 )
  310    CONTINUE
         CALL ZLACPY( 'Full', M, NEIG, Q( N+1, 1 ), LDQ, Q( M+1, 1 ),
     $                LDQ )
         DO 320 I = 1, NEIG
            CALL ZAXPY( M, CIMAG, Q( M+N+1, I ), 1, Q( M+1, I ), 1 )
  320    CONTINUE
      ELSE
         DO 330 I = 1, M
            CALL ZAXPY( M, CIMAG, Q( M+1, I ), 1, Q( 1, I ), 1 )
  330    CONTINUE
         CALL ZLACPY( 'Full', M, M, Q( N+1, 1 ), LDQ, Q( M+1, 1 ), LDQ )
         DO 340 I = 1, M
            CALL ZAXPY( M, CIMAG, Q( M+N+1, I ), 1, Q( M+1, I ), 1 )
  340    CONTINUE
C
         DO 350 I = 1, NEIG - M
            CALL ZAXPY( M, CIMAG, Q( M+1, M+I ), 1, Q( 1, M+I ), 1 )
  350    CONTINUE
         CALL ZLACPY( 'Full', M, NEIG-M, Q( N+1, M+1 ), LDQ,
     $                Q( M+1, M+1 ), LDQ )
         DO 360 I = 1, NEIG - M
            CALL ZAXPY( M, CIMAG, Q( M+N+1, M+I ), 1, Q( M+1, M+I ), 1 )
  360    CONTINUE
      END IF
C
C     Orthogonalize the basis given in Q(1:n,1:neig).
C
      IF( SVD ) THEN
C
C        Workspace:          need   3*N;
C                            prefer larger.
C        Real workspace:     need   6*N.
C
         CALL ZGESVD( 'Overwrite', 'No V', N, NEIG, Q, LDQ, DWORK,
     $                ZWORK, 1, ZWORK, 1, ZWORK, LZWORK,
     $                DWORK( IWRK ), INFO )
         IF( INFO.GT.0 ) THEN
            INFO = 3
            RETURN
         END IF
         OPTZW = MAX( OPTZW, INT( ZWORK( 1 ) ) )
         NEIG  = NEIG/2
C
      ELSE
         IF( QR ) THEN
C
C           Workspace:          need   N;
C                               prefer M+M*NB, where NB is the optimal
C                                              blocksize.
C
            CALL ZGEQRF( N, NEIG, Q, LDQ, ZWORK( ITAU ), ZWORK( IWRK ),
     $                   LZWORK-IWRK+1, INFO )
            OPTZW = MAX( OPTZW, INT( ZWORK( IWRK ) ) + IWRK - 1 )
         ELSE
C
C           Workspace:          need   2*N+1;
C                               prefer N+(N+1)*NB.
C           Real workspace:     need   2*N.
C
            DO 370 J = 1, NEIG
               IWORK( J ) = 0
  370       CONTINUE
            CALL ZGEQP3( N, NEIG, Q, LDQ, IWORK, ZWORK, ZWORK( IWRK ),
     $                   LZWORK-IWRK+1, DWORK, INFO )
            OPTZW = MAX( OPTZW, INT( ZWORK( IWRK ) ) + IWRK - 1 )
         END IF
C
C        Workspace:     need   2*N;
C                       prefer N+N*NB.
C
         CALL ZUNGQR( N, NEIG, NEIG, Q, LDQ, ZWORK( ITAU ),
     $                ZWORK( IWRK ), LZWORK-IWRK+1, INFO )
         OPTZW = MAX( OPTZW, INT( ZWORK( IWRK ) ) + IWRK - 1 )
         IF( QRP )
     $      NEIG = NEIG/2
      END IF
C
      DWORK( 1 ) = OPTDW
      ZWORK( 1 ) = OPTZW
      INFO = IWA
      RETURN
C *** Last line of MB3LZP ***
      END
