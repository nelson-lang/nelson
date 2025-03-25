      SUBROUTINE MB04BZ( JOB, COMPQ, N, A, LDA, DE, LDDE, B, LDB, FG,
     $                   LDFG, Q, LDQ, ALPHAR, ALPHAI, BETA, IWORK,
     $                   DWORK, LDWORK, ZWORK, LZWORK, BWORK, INFO )
C
C     PURPOSE
C
C     To compute the eigenvalues of a complex N-by-N skew-Hamiltonian/
C     Hamiltonian pencil aS - bH, with
C
C           (  A  D  )         (  B  F  )
C       S = (      H ) and H = (      H ).                           (1)
C           (  E  A  )         (  G -B  )
C
C     This routine computes the eigenvalues using an embedding to a real
C     skew-Hamiltonian/skew-Hamiltonian pencil aB_S - bB_T, defined as
C
C             (  Re(A)  -Im(A)  |  Re(D)  -Im(D)  )
C             (                 |                 )
C             (  Im(A)   Re(A)  |  Im(D)   Re(D)  )
C             (                 |                 )
C       B_S = (-----------------+-----------------) , and
C             (                 |      T       T  )
C             (  Re(E)  -Im(E)  |  Re(A )  Im(A ) )
C             (                 |      T       T  )
C             (  Im(E)   Re(E)  | -Im(A )  Re(A ) )
C                                                                    (2)
C             ( -Im(B)  -Re(B)  | -Im(F)  -Re(F)  )
C             (                 |                 )
C             (  Re(B)  -Im(B)  |  Re(F)  -Im(F)  )
C             (                 |                 )
C       B_T = (-----------------+-----------------) ,  T = i*H.
C             (                 |      T       T  )
C             ( -Im(G)  -Re(G)  | -Im(B )  Re(B ) )
C             (                 |      T       T  )
C             (  Re(G)  -Im(G)  | -Re(B ) -Im(B ) )
C
C     Optionally, if JOB = 'T', the pencil aB_S - bB_H (B_H = -i*B_T) is
C     transformed by a unitary matrix Q to the structured Schur form
C
C                ( BA  BD  )              ( BB  BF  )
C       B_Sout = (       H ) and B_Hout = (       H ),               (3)
C                (  0  BA  )              (  0 -BB  )
C
C     where BA and BB are upper triangular, BD is skew-Hermitian, and
C     BF is Hermitian. The embedding doubles the multiplicities of the
C     eigenvalues of the pencil aS - bH. Optionally, if COMPQ = 'C', the
C     unitary matrix Q is computed.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOB     CHARACTER*1
C             Specifies the computation to be performed, as follows:
C             = 'E': compute the eigenvalues only; S and H will not
C                    necessarily be transformed as in (3).
C             = 'T': put S and H into the forms in (3) and return the
C                    eigenvalues in ALPHAR, ALPHAI and BETA.
C
C     COMPQ   CHARACTER*1
C             Specifies whether to compute the unitary transformation
C             matrix Q, as follows:
C             = 'N':  Q is not computed;
C             = 'C':  compute the unitary transformation matrix Q.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the pencil aS - bH.  N >= 0, even.
C
C     A       (input/output) COMPLEX*16 array, dimension (LDA, K)
C             where K = N/2, if JOB = 'E', and K = N, if JOB = 'T'.
C             On entry, the leading N/2-by-N/2 part of this array must
C             contain the matrix A.
C             On exit, if JOB = 'T', the leading N-by-N part of this
C             array contains the upper triangular matrix BA in (3) (see
C             also METHOD). The strictly lower triangular part is not
C             zeroed, but it is preserved.
C             If JOB = 'E', this array is unchanged on exit.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1, K).
C
C     DE      (input/output) COMPLEX*16 array, dimension
C                            (LDDE, MIN(K+1,N))
C             On entry, the leading N/2-by-N/2 lower triangular part of
C             this array must contain the lower triangular part of the
C             skew-Hermitian matrix E, and the N/2-by-N/2 upper
C             triangular part of the submatrix in the columns 2 to N/2+1
C             of this array must contain the upper triangular part of
C             the skew-Hermitian matrix D.
C             On exit, if JOB = 'T', the leading N-by-N part of this
C             array contains the skew-Hermitian matrix BD in (3) (see
C             also METHOD). The strictly lower triangular part of the
C             input matrix is preserved.
C             If JOB = 'E', this array is unchanged on exit.
C
C     LDDE    INTEGER
C             The leading dimension of the array DE.  LDDE >= MAX(1, K).
C
C     B       (input/output) COMPLEX*16 array, dimension (LDB, K)
C             On entry, the leading N/2-by-N/2 part of this array must
C             contain the matrix B.
C             On exit, if JOB = 'T', the leading N-by-N part of this
C             array contains the upper triangular matrix BB in (3) (see
C             also METHOD). The strictly lower triangular part is not
C             zeroed; the elements below the first subdiagonal of the
C             input matrix are preserved.
C             If JOB = 'E', this array is unchanged on exit.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= MAX(1, K).
C
C     FG      (input/output) COMPLEX*16 array, dimension
C                            (LDFG, MIN(K+1,N))
C             On entry, the leading N/2-by-N/2 lower triangular part of
C             this array must contain the lower triangular part of the
C             Hermitian matrix G, and the N/2-by-N/2 upper triangular
C             part of the submatrix in the columns 2 to N/2+1 of this
C             array must contain the upper triangular part of the
C             Hermitian matrix F.
C             On exit, if JOB = 'T', the leading N-by-N part of this
C             array contains the Hermitian matrix BF in (3) (see also
C             METHOD). The strictly lower triangular part of the input
C             matrix is preserved. The diagonal elements might have tiny
C             imaginary parts.
C             If JOB = 'E', this array is unchanged on exit.
C
C     LDFG    INTEGER
C             The leading dimension of the array FG.  LDFG >= MAX(1, K).
C
C     Q       (output) COMPLEX*16 array, dimension (LDQ, 2*N)
C             On exit, if COMPQ = 'C', the leading 2*N-by-2*N part of
C             this array contains the unitary transformation matrix Q
C             that reduced the matrices B_S and B_H to the form in (3).
C             However, if JOB = 'E', the reduction was possibly not
C             completed: the matrix B_H may have 2-by-2 diagonal blocks,
C             and the array Q returns the orthogonal matrix that
C             performed the partial reduction.
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
C     IWORK   INTEGER array, dimension (2*N+4)
C             On exit, IWORK(1) contains the number, q, of unreliable,
C             possibly inaccurate (pairs of) eigenvalues, and the
C             absolute values in IWORK(2), ..., IWORK(q+1) are their
C             indices, as well as of the corresponding 1-by-1 and 2-by-2
C             diagonal blocks of the arrays B and A on exit, if
C             JOB = 'T'. Specifically, a positive value is an index of
C             a real or purely imaginary eigenvalue, corresponding to a
C             1-by-1 block, while the absolute value of a negative entry
C             in IWORK is an index to the first eigenvalue in a pair of
C             consecutively stored eigenvalues, corresponding to a
C             2-by-2 block. Moreover, IWORK(q+2),..., IWORK(2*q+1)
C             contain pointers to the starting elements in DWORK where
C             each block pair is stored. Specifically, if IWORK(i+1) > 0
C             then DWORK(r) and DWORK(r+1) store corresponding diagonal
C             elements of T11 and S11, respectively, and if
C             IWORK(i+1) < 0, then DWORK(r:r+3) and DWORK(r+4:r+7) store
C             the elements of the block in T11 and S11, respectively
C             (see Section METHOD), where r = IWORK(q+1+i). Moreover,
C             IWORK(2*q+2) contains the number of the 1-by-1 blocks, and
C             IWORK(2*q+3) contains the number of the 2-by-2 blocks,
C             corresponding to unreliable eigenvalues. IWORK(2*q+4)
C             contains the total number t of the 2-by-2 blocks.
C             If INFO = 0, then q = 0, therefore IWORK(1) = 0.
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0 or INFO = 3, DWORK(1) returns the
C             optimal LDWORK, and DWORK(2) and DWORK(3) contain the
C             Frobenius norms of the matrices B_S and B_T. These norms
C             are used in the tests to decide that some eigenvalues are
C             considered as numerically unreliable. Moreover, DWORK(4),
C             ..., DWORK(3+2*s) contain the s pairs of values of the
C             1-by-1 diagonal elements of T11 and S11. The eigenvalue of
C             such a block pair is obtained from -i*T11(i,i)/S11(i,i).
C             Similarly, DWORK(4+2*s), ..., DWORK(3+2*s+8*t) contain the
C             t groups of pairs of 2-by-2 diagonal submatrices of T11
C             and S11, stored column-wise. The spectrum of such a block
C             pair is obtained from -i*ev, where ev are the eigenvalues
C             of (T11(i:i+1,i:i+1),S11(i:i+1,i:i+1)).
C             On exit, if INFO = -19, DWORK(1) returns the minimum value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The dimension of the array DWORK. If COMPQ = 'N',
C             LDWORK >= MAX( 3,  4*N*N + 3*N ), if   JOB = 'E';
C             LDWORK >= MAX( 3,  5*N*N + 3*N ), if   JOB = 'T';
C             LDWORK >= MAX( 3, 11*N*N + 2*N ), if COMPQ = 'C'.
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
C             On exit, if INFO = -21, ZWORK(1) returns the minimum value
C             of LZWORK.
C
C     LZWORK  INTEGER
C             The dimension of the array ZWORK.
C             LZWORK >= 1,       if JOB = 'E'; otherwise,
C             LZWORK >= 6*N + 4, if COMPQ = 'N';
C             LZWORK >= 8*N + 4, if COMPQ = 'C'.
C
C             If LZWORK = -1, then a workspace query is assumed;
C             the routine only calculates the optimal size of the
C             ZWORK array, returns this value as the first entry of
C             the ZWORK array, and no error message related to LZWORK
C             is issued by XERBLA.
C
C     BWORK   LOGICAL array, dimension (LBWORK)
C             LBWORK >= 0, if JOB = 'E';
C             LBWORK >= N, if JOB = 'T'.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0: succesful exit;
C             < 0: if INFO = -i, the i-th argument had an illegal value;
C             = 1: QZ iteration failed in the SLICOT Library routine
C                  MB04FD (QZ iteration did not converge or computation
C                  of the shifts failed);
C             = 2: QZ iteration failed in the LAPACK routine ZHGEQZ when
C                  trying to triangularize the 2-by-2 blocks;
C             = 3: warning: the pencil is numerically singular.
C
C     METHOD
C
C     First, T = i*H is set. Then, the embeddings, B_S and B_T, of the
C     matrices S and T, are determined and, subsequently, the SLICOT
C     Library routine MB04FD is applied to compute the structured Schur
C     form, i.e., the factorizations
C
C     ~        T  T         (  S11  S12  )
C     B_S = J Q  J  B_S Q = (          T ) and
C                           (   0   S11  )
C
C     ~        T  T         (  T11  T12  )
C     B_T = J Q  J  B_T Q = (          T ),
C                           (   0   T11  )
C
C     where Q is real orthogonal, S11 is upper triangular, and T11 is
C     upper quasi-triangular. If JOB = 'T', then the matrices above are
C                                                        ~
C     further transformed so that the 2-by-2 blocks in i*B_T are split
C     into 1-by-1 blocks.  If COMPQ = 'C', the transformations are
C     accumulated in the unitary matrix Q.
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
C     The returned eigenvalues are those of the pencil (-i*T11,S11),
C     where i is the purely imaginary unit.
C
C     If JOB = 'E', the returned matrix T11 is quasi-triangular. Note
C     that the off-diagonal elements of the 2-by-2 blocks of S11 are
C     zero by construction.
C
C     If JOB = 'T', the returned eigenvalues correspond to the diagonal
C     elements of BB and BA.
C
C     This routine does not perform any scaling of the matrices. Scaling
C     might sometimes be useful, and it should be done externally.
C
C     CONTRIBUTOR
C
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     Nov. 2011.
C
C     REVISIONS
C
C     V. Sima, July 2012, Sep. 2016, Nov. 2016, Apr. 2020.
C
C     KEYWORDS
C
C     Deflating subspace, embedded pencil, skew-Hamiltonian/Hamiltonian
C     pencil, structured Schur form.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ONE, THREE, ZERO
      PARAMETER          ( ONE = 1.0D+0, THREE = 3.0D+0, ZERO = 0.0D+0 )
      COMPLEX*16         CZERO, CONE, CIMAG
      PARAMETER          ( CZERO = ( 0.0D+0, 0.0D+0 ),
     $                      CONE = ( 1.0D+0, 0.0D+0 ),
     $                     CIMAG = ( 0.0D+0, 1.0D+0 ) )
C
C     .. Scalar Arguments ..
      CHARACTER          COMPQ, JOB
      INTEGER            INFO, LDA, LDB, LDDE, LDFG, LDQ, LDWORK,
     $                   LZWORK, N
C
C     .. Array Arguments ..
      LOGICAL            BWORK( * )
      INTEGER            IWORK( * )
      DOUBLE PRECISION   ALPHAI( * ), ALPHAR( * ), BETA( * ), DWORK( * )
      COMPLEX*16         A( LDA, * ), B( LDB, * ), DE( LDDE, * ),
     $                   FG( LDFG, * ), Q( LDQ, * ), ZWORK( * )
C
C     .. Local Scalars ..
      LOGICAL            LCMPQ, LQUERY, LTRI, UNREL
      CHARACTER*14       CMPQ, JOBF
      INTEGER            I, I1, IA, IB, IDE, IEV, IFG, IQ, IQ2, IQB, IS,
     $                   IW, IW1, IWRK, J, J1, J2, JM1, JP2, K, L, M,
     $                   MINDB, MINDW, MINZW, N2, NB, NC, NN, OPTDW,
     $                   OPTZW
      DOUBLE PRECISION   NRMBS, NRMBT
      COMPLEX*16         TMP
C
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C
C     .. External Subroutines ..
      EXTERNAL           DCOPY, DLACPY, DSCAL, MB04FD, XERBLA, ZGEMM,
     $                   ZGEQRF, ZHGEQZ, ZLACPY, ZSCAL
C
C     .. Intrinsic Functions ..
      INTRINSIC          DBLE, DCMPLX, DIMAG, INT, MAX, MIN, MOD
C
C     .. Executable Statements ..
C
C     Decode the input arguments.
C
      M     = N/2
      NN    = N*N
      N2    = 2*N
      LTRI  = LSAME( JOB,   'T' )
      LCMPQ = LSAME( COMPQ, 'C' )
C
      IF( LTRI ) THEN
         K = N
      ELSE
         K = M
      END IF
C
      IF( N.EQ.0 ) THEN
         MINDW = 3
         MINZW = 1
      ELSE IF( LCMPQ ) THEN
         MINDB =  8*NN + N2
         MINDW = 11*NN + N2
         MINZW =  8*N  + 4
      ELSE
         MINDB =  4*NN + N2
         IF( LTRI ) THEN
            MINDW = 5*NN + 3*N
         ELSE
            MINDW = 4*NN + 3*N
         END IF
         MINZW = 6*N + 4
      END IF
C
      LQUERY = LDWORK.EQ.-1 .OR. LZWORK.EQ.-1
C
C     Test the input arguments.
C
      INFO = 0
      IF( .NOT.( LSAME( JOB, 'E' ) .OR. LTRI ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( LSAME( COMPQ, 'N' ) .OR. LCMPQ ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 .OR. MOD( N, 2 ).NE.0 ) THEN
         INFO = -3
      ELSE IF(  LDA.LT.MAX( 1, K ) ) THEN
         INFO = -5
      ELSE IF( LDDE.LT.MAX( 1, K ) ) THEN
         INFO = -7
      ELSE IF(  LDB.LT.MAX( 1, K ) ) THEN
         INFO = -9
      ELSE IF( LDFG.LT.MAX( 1, K ) ) THEN
         INFO = -11
      ELSE IF( LDQ.LT.1 .OR. ( LCMPQ .AND. LDQ.LT.N2 ) ) THEN
         INFO = -13
      ELSE IF( .NOT. LQUERY ) THEN
         IF( LDWORK.LT.MINDW ) THEN
            DWORK( 1 ) = MINDW
            INFO = -19
         ELSE IF( LZWORK.LT.MINZW ) THEN
            ZWORK( 1 ) = MINZW
            INFO = -21
         END IF
      END IF
C
      IF( INFO.NE.0) THEN
         CALL XERBLA( 'MB04BZ', -INFO )
         RETURN
      ELSE IF( N.GT.0 ) THEN
C
C        Compute optimal workspace.
C
         OPTZW = MINZW
         IF( LCMPQ ) THEN
            CMPQ = 'Initialize'
         ELSE
            CMPQ = 'No Computation'
         END IF
C
         IF( LTRI ) THEN
            JOBF = 'Triangularize'
            CALL ZGEQRF( N, N, ZWORK, N, ZWORK, ZWORK, -1, INFO )
            I  = INT( ZWORK( 1 ) )
            NB = MAX( I/N, 2 )
         ELSE
            JOBF = 'Eigenvalues'
         END IF
C
         IF( LQUERY ) THEN
            CALL MB04FD( JOBF, CMPQ, N2, DWORK, N, DWORK, N, DWORK, N,
     $                   DWORK, N, DWORK, N2, ALPHAI, ALPHAR, BETA,
     $                   IWORK, DWORK, -1, INFO )
            OPTDW = MAX( MINDW, MINDB + INT( DWORK( 1 ) ) )
            DWORK( 1 ) = OPTDW
            ZWORK( 1 ) = OPTZW
            RETURN
         END IF
      ELSE IF( LQUERY ) THEN
         DWORK( 1 ) = MINDW
         ZWORK( 1 ) = MINZW
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( N.EQ.0 ) THEN
         IWORK( 1 ) = 0
         DWORK( 1 ) = THREE
         DWORK( 2 ) = ZERO
         DWORK( 3 ) = ZERO
         ZWORK( 1 ) = CONE
         RETURN
      END IF
C
C     Set up the embeddings of the matrices S and H.
C
C     Set the pointers for the inputs and outputs of MB04FD.
C     Real workspace:    need   4*N**2 + 2*N, if COMPQ = 'N';
C                               8*N**2 + 2*N, if COMPQ = 'C'.
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
C     STEP 1: Apply MB04FD to transform the extended pencil to real
C             skew-Hamiltonian/skew-Hamiltonian Schur form.
C
C     Real workspace:
C     need    4*N*N + 2*N + max(3,N),     if JOB = 'E' and COMPQ = 'N';
C             4*N*N + 2*N + max(3,N*N+N), if JOB = 'T' and COMPQ = 'N';
C            11*N*N + 2*N,                if               COMPQ = 'C'.
C     prefer larger.
C
      CALL MB04FD( JOBF, CMPQ, N2, DWORK( IA ), N, DWORK( IDE ), N,
     $             DWORK( IB ), N, DWORK( IFG ), N, DWORK( IQ ), N2,
     $             ALPHAI, ALPHAR, BETA, IWORK, DWORK( IWRK ),
     $             LDWORK-IWRK+1, INFO )
      IF( INFO.EQ.1 ) THEN
         RETURN
      ELSE IF( INFO.EQ.2 ) THEN
         INFO = 3
      END IF
      OPTDW = MAX( MINDW, MINDB + INT( DWORK( IWRK ) ) )
      NRMBS = DWORK( IWRK+1 )
      NRMBT = DWORK( IWRK+2 )
C
C     Scale the eigenvalues.
C
      CALL DSCAL( N, -ONE, ALPHAI, 1 )
C
      IF( LCMPQ ) THEN
C
C        Set the transformation matrix.
C
         IW = IQ
         DO 180 J = 1, N2
            DO 170 I = 1, N2
               Q( I, J ) = DCMPLX( DWORK( IW ) )
               IW = IW + 1
  170       CONTINUE
  180    CONTINUE
      END IF
C
C     Count the numbers of pairs of diagonal 1-by-1 and 2-by-2 blocks of
C     B and A with possibly unreliable eigenvalues.
C
      I  = IWORK( 1 )
      IS = 0
      IW = 0
      DO 190 J = 1, I
         IF( IWORK( J+1 ).GT.0 ) THEN
            IS = IS + 1
         ELSE IF( IWORK( J+1 ).LT.0 ) THEN
            IW = IW + 1
         END IF
  190 CONTINUE
C
      I = 2*I + 2
      IWORK( I )   = IS
      IWORK( I+1 ) = IW
C
      IF( LTRI ) THEN
C
C        Convert the results to complex datatype. D and F start in the
C        first column of DE and FG, respectively.
C
         IW = IA
         DO 210 J = 1, N
            DO 200 I = 1, J
               A( I, J ) = DCMPLX( DWORK( IW ) )
               IW = IW + 1
  200       CONTINUE
            IW = IW + N - J
  210    CONTINUE
C
         IW = IDE + N
         DO 230 J = 1, N
            DO 220 I = 1, J - 1
               DE( I, J ) = DCMPLX( DWORK( IW ) )
               IW = IW + 1
  220       CONTINUE
            DE( J, J ) = CZERO
            IW = IW + N - J + 1
  230    CONTINUE
C
         IW = IB
         DO 250 J = 1, N
            DO 240 I = 1, MIN( J + 1, N )
               B( I, J ) = DCMPLX( DWORK( IW ) )
               IW = IW + 1
  240       CONTINUE
            IW = IW + N - J - 1
  250    CONTINUE
C
         IW = IFG + N
         DO 270 J = 1, N
            DO 260 I = 1, J - 1
               FG( I, J ) = DCMPLX( DWORK( IW ) )
               IW = IW + 1
  260       CONTINUE
            FG( J, J ) = CZERO
            IW = IW + N - J + 1
  270    CONTINUE
C
      END IF
C
C     Count the number of diagonal 2-by-2 blocks of B and A.
C
      I1 = 0
      I  = 1
C     WHILE( I.LT.N ) DO
  280 CONTINUE
      IF ( I.LT.N ) THEN
         IF( ALPHAR( I ).NE.ZERO .AND. BETA( I ).NE.ZERO .AND.
     $       ALPHAR( I ).NE.ZERO ) THEN
            I1 = I1 + 1
            I  = I  + 2
         ELSE
            I  = I  + 1
         END IF
         GO TO 280
      END IF
C     END WHILE 280
C
      I = 2*IWORK( 1 ) + 4
      IWORK( I ) = I1
C
C     Save in DWORK the pairs of diagonal 1-by-1 and 2-by-2 blocks of
C     B and A. Also, save in IWORK the pointers to the starting element
C     of each pair corresponding to unreliable eigenvalues.
C
      CALL DCOPY( N, DWORK( IA ), N+1, DWORK( IFG ), 1 )
      DWORK( 1 ) = OPTDW
      DWORK( 2 ) = NRMBS
      DWORK( 3 ) = NRMBT
C
      K  = 4
      I  = 1
      IW = IWORK( 1 )
      J  = 1
      L  = ( N - 2*I1 )*2 + K
C     WHILE( I.LE.N ) DO
  290 CONTINUE
      IF ( I.LE.N ) THEN
         IF ( J.LE.IW )
     $      UNREL = I.EQ.ABS( IWORK( J+1 ) )
         IF( ALPHAR( I ).NE.ZERO .AND. BETA( I ).NE.ZERO .AND.
     $       ALPHAI( I ).NE.ZERO ) THEN
            IF ( UNREL ) THEN
               J = J + 1
               IWORK( J+IW ) = L
               UNREL = .FALSE.
            END IF
            CALL DLACPY( 'Full', 2, 2, DWORK( IB+(I-1)*(N+1) ), N,
     $                   DWORK( L ), 2 )
            L = L + 4
            CALL DCOPY( 2, DWORK( IFG+I-1 ), 1, DWORK( L ), 3 )
            DWORK( L+1 ) = ZERO
            DWORK( L+2 ) = ZERO
            L = L + 4
            I = I + 2
         ELSE
            IF ( UNREL ) THEN
               J = J + 1
               IWORK( J+IW ) = K
               UNREL = .FALSE.
            END IF
            DWORK( K   ) = DWORK( IB+(I-1)*(N+1) )
            DWORK( K+1 ) = DWORK( IFG+I-1 )
            K = K + 2
            I = I + 1
         END IF
         GO TO 290
      END IF
C     END WHILE 290
C
      IF( .NOT.LTRI ) THEN
C
C        Return.
C
         ZWORK( 1 ) = OPTZW
         RETURN
      END IF
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
      J2 = MIN( N, NB )
C     WHILE( J.LT.N ) DO
  300 CONTINUE
      IF( J.LT.N ) THEN
         IF( B( J+1, J ).NE.CZERO ) THEN
C
C           Triangularization step.
C           Complex workspace:    need   4*N + 6.
C           Real    workspace:    need   4*N + 5.
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
     $                   DWORK( 4*N+4 ), IW )
            A( J+1, J ) = TMP
            IF( IW.GT.0 ) THEN
               INFO = 2
               RETURN
            END IF
C
C           Update A, DE, B, and FG.
C           Complex workspace:    need   6*N + 4.
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
            IF( LCMPQ ) THEN
C
C              Update Q.
C              Complex workspace:    need   8*N + 4.
C
               CALL ZGEMM(  'No Transpose', 'No Transpose', N2, 2, 2,
     $                      CONE, Q( 1, J ), LDQ, ZWORK( IQ2 ), 2,
     $                      CZERO, ZWORK( IWRK ), N2 )
               CALL ZLACPY( 'Full', N2, 2, ZWORK( IWRK ), N2, Q( 1, J ),
     $                      LDQ )
               CALL ZGEMM(  'No Transpose', 'No Transpose', N2, 2, 2,
     $                      CONE, Q( 1, N+J ), LDQ, ZWORK( IQ ), 2,
     $                      CZERO, ZWORK( IWRK ), N2 )
               CALL ZLACPY( 'Full', N2, 2, ZWORK( IWRK ), N2,
     $                      Q( 1, N+J ), LDQ )
            END IF
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
  310       CONTINUE
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
               GO TO 310
            END IF
C           END WHILE 310
         END IF
         GO TO 300
      END IF
C     END WHILE 300
C
C     Scale B and FG by -i.
C
      DO 320 I = 1, N
         CALL ZSCAL( I, -CIMAG, B( 1, I ), 1 )
  320 CONTINUE
C
      DO 330 I = 1, N
         CALL ZSCAL( I, -CIMAG, FG( 1, I ), 1 )
  330 CONTINUE
C
      ZWORK( 1 ) = OPTZW
      RETURN
C *** Last line of MB04BZ ***
      END
