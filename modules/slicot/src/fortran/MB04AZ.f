      SUBROUTINE MB04AZ( JOB, COMPQ, COMPU, N, Z, LDZ, B, LDB, FG,
     $                   LDFG, D, LDD, C, LDC, Q, LDQ, U, LDU, ALPHAR,
     $                   ALPHAI, BETA, IWORK, LIWORK, DWORK, LDWORK,
     $                   ZWORK, LZWORK, BWORK, INFO )
C     PURPOSE
C
C     To compute the eigenvalues of a complex N-by-N skew-Hamiltonian/
C     Hamiltonian pencil aS - bH, with
C
C              H  T           (  B  F  )       (  Z11  Z12  )
C       S = J Z  J  Z and H = (      H ), Z =: (            ).       (1)
C                             (  G -B  )       (  Z21  Z22  )
C
C     The structured Schur form of the embedded real skew-Hamiltonian/
C                                                           H  T
C     skew-Hamiltonian pencil, aB_S - bB_T, with B_S = J B_Z  J  B_Z,
C
C             (  Re(Z11)  -Im(Z11)  |  Re(Z12)  -Im(Z12)  )
C             (                     |                     )
C             (  Im(Z11)   Re(Z11)  |  Im(Z12)   Re(Z12)  )
C             (                     |                     )
C       B_Z = (---------------------+---------------------) ,
C             (                     |                     )
C             (  Re(Z21)  -Im(Z21)  |  Re(Z22)  -Im(Z22)  )
C             (                     |                     )
C             (  Im(Z21)   Re(Z21)  |  Im(Z22)   Re(Z22)  )
C                                                                    (2)
C             ( -Im(B)  -Re(B)  | -Im(F)  -Re(F)  )
C             (                 |                 )
C             (  Re(B)  -Im(B)  |  Re(F)  -Im(F)  )
C             (                 |                 )
C       B_T = (-----------------+-----------------) ,  T = i*H,
C             (                 |      T       T  )
C             ( -Im(G)  -Re(G)  | -Im(B )  Re(B ) )
C             (                 |      T       T  )
C             (  Re(G)  -Im(G)  | -Re(B ) -Im(B ) )
C
C     is determined and used to compute the eigenvalues. Optionally,
C     if JOB = 'T', the pencil aB_S - bB_H is transformed by a unitary
C     matrix Q and a unitary symplectic matrix U to the structured Schur
C                                                   H  T
C     form aB_Sout - bB_Hout, with B_Sout = J B_Zout  J  B_Zout,
C
C                ( BA  BD  )              ( BB  BF  )
C       B_Zout = (         ) and B_Hout = (       H ),               (3)
C                (  0  BC  )              (  0 -BB  )
C
C     where BA and BB are upper triangular, BC is lower triangular,
C     and BF is Hermitian. B_H above is defined as B_H = -i*B_T.
C     The embedding doubles the multiplicities of the eigenvalues of
C     the pencil aS - bH.
C     Optionally, if COMPQ = 'C', the unitary matrix Q is computed.
C     Optionally, if COMPU = 'C', the unitary symplectic matrix U is
C     computed.
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
C             = 'N': do not compute the unitary matrix Q;
C             = 'C': the array Q is initialized internally to the unit
C                    matrix, and the unitary matrix Q is returned.
C
C     COMPU   CHARACTER*1
C             Specifies whether to compute the unitary symplectic
C             transformation matrix U, as follows:
C             = 'N': do not compute the unitary symplectic matrix U;
C             = 'C': the array U is initialized internally to the unit
C                    matrix, and the unitary symplectic matrix U is
C                    returned.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             Order of the pencil aS - bH.  N >= 0, even.
C
C     Z       (input/output) COMPLEX*16 array, dimension (LDZ, N)
C             On entry, the leading N-by-N part of this array must
C             contain the non-trivial factor Z in the factorization
C                    H  T
C             S = J Z  J  Z of the skew-Hamiltonian matrix S.
C             On exit, if JOB = 'T', the leading N-by-N part of this
C             array contains the upper triangular matrix BA in (3)
C             (see also METHOD). The strictly lower triangular part is
C             not zeroed. The submatrix in the rows N/2+1 to N and the
C             first N/2 columns is unchanged, except possibly for the
C             entry (N/2+1,N/2), which might be set to zero.
C             If JOB = 'E', this array is unchanged on exit.
C
C     LDZ     INTEGER
C             The leading dimension of the array Z.  LDZ >= MAX(1, N).
C
C     B       (input/output) COMPLEX*16 array, dimension (LDB, K), where
C             K = N, if JOB = 'T', and K = M, if JOB = 'E'.
C             On entry, the leading N/2-by-N/2 part of this array must
C             contain the matrix B.
C             On exit, if JOB = 'T', the leading N-by-N part of this
C             array contains the upper triangular matrix BB in (3)
C             (see also METHOD).
C             The strictly lower triangular part is not zeroed.
C             If JOB = 'E', this array is unchanged on exit.
C
C     LDB     INTEGER
C             The leading dimension of the array B.
C             LDB >= MAX(1, M), if JOB = 'E';
C             LDB >= MAX(1, N), if JOB = 'T'.
C
C     FG      (input/output) COMPLEX*16 array, dimension (LDFG, P),
C             where P = MAX(M+1,N), if JOB = 'T', and
C                   P = M+1,        if JOB = 'E'.
C             On entry, the leading N/2-by-N/2 lower triangular part of
C             this array must contain the lower triangular part of the
C             Hermitian matrix G, and the N/2-by-N/2 upper triangular
C             part of the submatrix in the columns 2 to N/2+1 of this
C             array must contain the upper triangular part of the
C             Hermitian matrix F. Accidental nonzero imaginary parts on
C             the main diagonals of F and G do not perturb the results.
C             On exit, if JOB = 'T', the leading N-by-N part of this
C             array contains the Hermitian matrix BF in (3) (see also
C             METHOD). The strictly lower triangular part of the input
C             matrix is preserved. The diagonal elements might have tiny
C             imaginary parts, since they have not been annihilated.
C             If JOB = 'E', this array is unchanged on exit.
C
C     LDFG    INTEGER
C             The leading dimension of the array FG.
C             LDFG >= MAX(1, M), if JOB = 'E';
C             LDFG >= MAX(1, N), if JOB = 'T'.
C
C     D       (output) COMPLEX*16 array, dimension (LDD, N)
C             If JOB = 'T', the leading N-by-N part of this array
C             contains the matrix BD in (3) (see also METHOD).
C             If JOB = 'E', this array is not referenced.
C
C     LDD     INTEGER
C             The leading dimension of the array D.
C             LDD >= 1,         if JOB = 'E';
C             LDD >= MAX(1, N), if JOB = 'T'.
C
C     C       (output) COMPLEX*16 array, dimension (LDC, N)
C             If JOB = 'T', the leading N-by-N part of this array
C             contains the lower triangular matrix BC in (3) (see also
C             METHOD). The part over the first superdiagonal is not set.
C             If JOB = 'E', this array is not referenced.
C
C     LDC     INTEGER
C             The leading dimension of the array C.
C             LDC >= 1,         if JOB = 'E';
C             LDC >= MAX(1, N), if JOB = 'T'.
C
C     Q       (output) COMPLEX*16 array, dimension (LDQ, 2*N)
C             On exit, if COMPQ = 'C' and JOB = 'T', then the leading
C             2*N-by-2*N part of this array contains the unitary
C             transformation matrix Q.
C             If COMPQ = 'C' and JOB = 'E', this array contains the
C             orthogonal transformation which reduced B_Z and B_T
C             in the first step of the algorithm (see METHOD).
C             If COMPQ = 'N', this array is not referenced.
C
C     LDQ     INTEGER
C             The leading dimension of the array Q.
C             LDQ >= 1,           if COMPQ = 'N';
C             LDQ >= MAX(1, 2*N), if COMPQ = 'C'.
C
C     U       (output) COMPLEX*16 array, dimension (LDU, 2*N)
C             On exit, if COMPU = 'C' and JOB = 'T', then the leading
C             N-by-2*N part of this array contains the leading N-by-2*N
C             part of the unitary symplectic transformation matrix U.
C             If COMPU = 'C' and JOB = 'E', this array contains the
C             first N rows of the transformation U which reduced B_Z
C             and B_T in the first step of the algorithm (see METHOD).
C             If COMPU = 'N', this array is not referenced.
C
C     LDU     INTEGER
C             The leading dimension of the array U.
C             LDU >= 1,         if COMPU = 'N';
C             LDU >= MAX(1, N), if COMPU = 'C'.
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
C     IWORK   INTEGER array, dimension (LIWORK)
C             On exit, if INFO = 3, IWORK(1) contains the number of
C             (pairs of) possibly inaccurate eigenvalues, q <= N/2, and
C             IWORK(2), ..., IWORK(q+1) indicate their indices.
C             Specifically, a positive value is an index of a real or
C             purely imaginary eigenvalue, corresponding to a 1-by-1
C             block, while the absolute value of a negative entry in
C             IWORK is an index to the first eigenvalue in a pair of
C             consecutively stored eigenvalues, corresponding to a
C             2-by-2 block. A 2-by-2 block may have two complex, two
C             real, two purely imaginary, or one real and one purely
C             imaginary eigenvalue. The blocks are those in B_T and B_S.
C             For i = q+2, ..., 2*q+1, IWORK(i) contains a pointer to
C             the starting location in DWORK of the i-th triplet of
C             1-by-1 blocks, if IWORK(i-q) > 0, or 2-by-2 blocks,
C             if IWORK(i-q) < 0, defining unreliable eigenvalues.
C             IWORK(2*q+2) contains the number of the 1-by-1 blocks, and
C             IWORK(2*q+3) contains the number of the 2-by-2 blocks,
C             corresponding to unreliable eigenvalues. IWORK(2*q+4)
C             contains the total number t of the 2-by-2 blocks.
C             If INFO = 0, then q = 0, therefore IWORK(1) = 0.
C
C     LIWORK  INTEGER
C             The dimension of the array IWORK.  LIWORK >= 2*N+9.
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0 or INFO = 3, DWORK(1) returns the
C             optimal LDWORK, and DWORK(2), ..., DWORK(4) contain the
C             Frobenius norms of the factors of the formal matrix
C             product used by the algorithm. In addition, DWORK(5), ...,
C             DWORK(4+3*s) contain the s triplet values corresponding
C             to the 1-by-1 blocks. Their eigenvalues are real or purely
C             imaginary. Such an eigenvalue is obtained as -a1/a2/a3*i,
C             where a1, ..., a3 are the corresponding triplet values,
C             and i is the purely imaginary unit.
C             Moreover, DWORK(5+3*s), ..., DWORK(4+3*s+12*t) contain the
C             t groups of triplet 2-by-2 matrices corresponding to the
C             2-by-2 blocks. Their eigenvalue pairs are either complex,
C             or placed on the real and imaginary axes. Such an
C             eigenvalue pair is given by imag( ev ) - real( ev )*i,
C             where ev is the spectrum of the matrix product
C             A1*inv(A2)*inv(A3), and A1, ..., A3 define the
C             corresponding 2-by-2 matrix triplet.
C             On exit, if INFO = -25, DWORK(1) returns the minimum
C             value of LDWORK.
C
C     LDWORK  INTEGER
C             The dimension of the array DWORK.
C             LDWORK >= c*N**2 + N + MAX(6*N, 27), where
C                       c = 18, if                 COMPU = 'C';
C                       c = 16, if COMPQ = 'C' and COMPU = 'N';
C                       c = 13, if COMPQ = 'N' and COMPU = 'N'.
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
C             On exit, if INFO = -27, ZWORK(1) returns the minimum
C             value of LZWORK.
C
C     LZWORK  INTEGER
C             The dimension of the array ZWORK.
C             LZWORK >= 8*N + 28, if JOB = 'T' and COMPQ = 'C';
C             LZWORK >= 6*N + 28, if JOB = 'T' and COMPQ = 'N';
C             LZWORK >= 1,        if JOB = 'E'.
C             For good performance LZWORK should be generally larger.
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
C             = 1: the algorithm was not able to reveal information
C                  about the eigenvalues from the 2-by-2 blocks in the
C                  SLICOT Library routine MB03BD (called by MB04ED);
C             = 2: periodic QZ iteration failed in the SLICOT Library
C                  routines MB03BD or MB03BZ when trying to
C                  triangularize the 2-by-2 blocks;
C             = 3: some eigenvalues might be inaccurate. This is a
C                  warning.
C
C     METHOD
C
C     First T = i*H is set. Then, the embeddings, B_Z and B_T, of the
C     matrices S and T, are determined and, subsequently, the SLICOT
C     Library routine MB04ED is applied to compute the structured Schur
C     form, i.e., the factorizations
C
C     ~      T         (  BZ11  BZ12  )
C     B_Z = U  B_Z Q = (              ) and
C                      (    0   BZ22  )
C
C     ~        T  T         (  T11  T12  )
C     B_T = J Q  J  B_T Q = (          T ),
C                           (   0   T11  )
C
C     where Q is real orthogonal, U is real orthogonal symplectic, BZ11,
C     BZ22' are upper triangular and T11 is upper quasi-triangular.
C     If JOB = 'T', the 2-by-2 blocks are triangularized using the
C     periodic QZ algorithm.
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
C
C     CONTRIBUTOR
C
C     Matthias Voigt, Fakultaet fuer Mathematik, Technische Universitaet
C     Chemnitz, April 30, 2009.
C     V. Sima, Aug. 2009 (SLICOT version of the routine ZHAFDF).
C
C     REVISIONS
C     V. Sima, Jan. 2011, Mar. 2011, Aug. 2011, Nov. 2011, July 2012,
C     July 2013, May 2020.
C
C     KEYWORDS
C
C     Deflating subspace, embedded pencil, skew-Hamiltonian/Hamiltonian
C     pencil, structured Schur form.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, FOUR
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, FOUR = 4.0D+0 )
      COMPLEX*16         CZERO, CONE, CIMAG
      PARAMETER          ( CZERO = ( 0.0D+0, 0.0D+0 ),
     $                      CONE = ( 1.0D+0, 0.0D+0 ),
     $                     CIMAG = ( 0.0D+0, 1.0D+0 ) )
C
C     .. Scalar Arguments ..
      CHARACTER          COMPQ, COMPU, JOB
      INTEGER            INFO, LDB, LDC, LDD, LDFG, LDQ, LDU, LDWORK,
     $                   LDZ, LIWORK, LZWORK, N
C
C     .. Array Arguments ..
      LOGICAL            BWORK( * )
      INTEGER            IWORK( * )
      DOUBLE PRECISION   ALPHAI( * ), ALPHAR( * ), BETA( * ), DWORK( * )
      COMPLEX*16         B( LDB, * ), C( LDC, * ), D( LDD, * ),
     $                   FG( LDFG, * ), Q( LDQ, * ), U( LDU, * ),
     $                   Z( LDZ, * ), ZWORK( * )
C
C     .. Local Scalars ..
      LOGICAL            LCMPQ, LCMPU, LQUERY, LTRI
      CHARACTER*10       CMPQ, CMPU
      INTEGER            I, I1, IB, IEV, IFG, IQ, IQ2, IQB, IS, IU, IUB,
     $                   IW, IW1, IWRK, IZ11, IZ22, J, J1, J2, J3, JM1,
     $                   JP2, K, M, MINDB, MINDW, MINZW, N2, NB, NC,
     $                   NJ1, NN, OPTDW, OPTZW
      DOUBLE PRECISION   EPS, NRMB
      COMPLEX*16         TMP
C
C     .. Local Arrays ..
      INTEGER            DWORKZ( 2 ), IWORKZ( 5 )
C
C     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH, LSAME
C
C     .. External Subroutines ..
      EXTERNAL           DCOPY, DLACPY, DSCAL, MB03BZ, MB04ED, XERBLA,
     $                   ZGEMM, ZGEQRF, ZLACPY, ZSCAL
C
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCMPLX, DCONJG, DIMAG, INT, MAX,
     $                   MIN, MOD
C
C     .. Executable Statements ..
C
C     Decode the input arguments.
C
      M  = N/2
      NN = N*N
      N2 = 2*N
C
      LTRI  = LSAME( JOB,   'T' )
      LCMPQ = LSAME( COMPQ, 'C' )
      LCMPU = LSAME( COMPU, 'C' )
C
      IF( LTRI ) THEN
         K = MAX( 1, N )
      ELSE
         K = MAX( 1, M )
      END IF
      IF( N.EQ.0 ) THEN
         MINDW = 4
         MINZW = 1
      ELSE
         IF( LTRI ) THEN
            IF( LCMPQ ) THEN
               MINZW = 4*N2 + 28
            ELSE
               MINZW = 3*N2 + 28
            END IF
         ELSE
            MINZW = 1
         END IF
         IF( LCMPU ) THEN
            I = 12
            J = 18
         ELSE
            I = 10
            IF( LCMPQ ) THEN
               J = 16
            ELSE
               J = 13
            END IF
         END IF
         MINDB = I*NN + N
         MINDW = J*NN + N + MAX( 3*N2, 27 )
      END IF
      LQUERY = LDWORK.EQ.-1 .OR. LZWORK.EQ.-1
C
C     Test the input arguments.
C
      INFO = 0
      IF( .NOT.( LSAME( JOB, 'E' ) .OR. LTRI ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( LSAME( COMPQ, 'N' ) .OR. LCMPQ ) ) THEN
         INFO = -2
      ELSE IF( .NOT.( LSAME( COMPU, 'N' ) .OR. LCMPU ) ) THEN
         INFO = -3
      ELSE IF( N.LT.0 .OR. MOD( N, 2 ).NE.0 ) THEN
         INFO = -4
      ELSE IF(  LDZ.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF(  LDB.LT.K ) THEN
         INFO = -8
      ELSE IF( LDFG.LT.K ) THEN
         INFO = -10
      ELSE IF(  LDD.LT.1 .OR. (  LTRI .AND. LDD.LT.N  ) ) THEN
         INFO = -12
      ELSE IF(  LDC.LT.1 .OR. (  LTRI .AND. LDC.LT.N  ) ) THEN
         INFO = -14
      ELSE IF(  LDQ.LT.1 .OR. ( LCMPQ .AND. LDQ.LT.N2 ) ) THEN
         INFO = -16
      ELSE IF(  LDU.LT.1 .OR. ( LCMPU .AND. LDU.LT.N  ) ) THEN
         INFO = -18
      ELSE IF( LIWORK.LT.N2+9 ) THEN
         INFO = -23
      ELSE IF( .NOT. LQUERY ) THEN
         IF( LDWORK.LT.MINDW ) THEN
            DWORK( 1 ) = MINDW
            INFO = -25
         ELSE IF( LZWORK.LT.MINZW ) THEN
            ZWORK( 1 ) = MINZW
            INFO = -27
         END IF
      END IF
C
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'MB04AZ', -INFO )
         RETURN
      ELSE IF( N.GT.0 ) THEN
C
C        Compute optimal workspace.
C
         IF( LTRI ) THEN
            CALL ZGEQRF( N, N, Z, LDZ, ZWORK, ZWORK, -1, INFO )
            I  = INT( ZWORK( 1 ) )
            NB = MAX( I/N, 2 )
         END IF
C
         IF( LCMPQ ) THEN
            CMPQ = 'Initialize'
         ELSE
            CMPQ = COMPQ
         END IF
         IF( LCMPU ) THEN
            CMPU = 'Initialize'
         ELSE
            CMPU = COMPU
         END IF
C
         IF( LQUERY ) THEN
            CALL MB04ED( JOB, CMPQ, CMPU, N2, DWORK, N2, DWORK, N,
     $                   DWORK, N, DWORK, N2, DWORK, N, DWORK, N,
     $                   ALPHAI, ALPHAR, BETA, IWORK, LIWORK, DWORK,
     $                   -1, INFO )
            OPTDW = MAX( MINDW, MINDB + INT( DWORK( 1 ) ) )
C
            IF( LTRI ) THEN
               OPTZW = MAX( MINZW, I )
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
         IWORK( 1 ) = 0
         DWORK( 1 ) = FOUR
         DWORK( 2 ) = ZERO
         DWORK( 3 ) = ZERO
         DWORK( 4 ) = ZERO
         ZWORK( 1 ) = CONE
         RETURN
      END IF
C
C     Determine machine constants.
C
      EPS = DLAMCH( 'Precision' )
C
C     Set up the embeddings of the matrices Z and H.
C     Real workspace:    need   w1, where
C                        w1 = 12*N**2+N, if COMPU = 'C';
C                        w1 = 10*N**2+N, if COMPU = 'N'.
C
      IQ = 1
      IF( LCMPU ) THEN
         IU   = IQ + N2*N2
         IZ11 = IU + N2*N
      ELSE
         IU   = 1
         IZ11 = IQ + N2*N2
      END IF
      IB   = IZ11 + N2*N2
      IFG  = IB   + NN
      IWRK = IFG  + NN + N
C
C     Build the embedding of Z.
C
      IW = IZ11
      IS = IW + N2*M
      DO 50 J = 1, N
         IW1 = IW
         DO 10 I = 1, M
            DWORK( IW ) = DBLE( Z( I, J ) )
            IW = IW + 1
   10    CONTINUE
C
         DO 20 I = 1, M
            DWORK( IW ) =  DIMAG( Z( I, J ) )
            DWORK( IS ) = -DWORK( IW )
            IW = IW + 1
            IS = IS + 1
   20    CONTINUE
         CALL DCOPY( M, DWORK( IW1 ), 1, DWORK( IS ), 1 )
         IW1 = IW
         IS  = IS + M
C
         DO 30 I = M + 1, N
            DWORK( IW ) = DBLE( Z( I, J ) )
            IW = IW + 1
   30    CONTINUE
C
         DO 40 I = M + 1, N
            DWORK( IW ) =  DIMAG( Z( I, J ) )
            DWORK( IS ) = -DWORK( IW )
            IW = IW + 1
            IS = IS + 1
   40    CONTINUE
C
         CALL DCOPY( M, DWORK( IW1 ), 1, DWORK( IS ), 1 )
         IW1 = IW
         IS  = IS + M
         IF( MOD( J, M ).EQ.0 ) THEN
            IW = IW + N2*M
            IS = IS + N2*M
         END IF
   50 CONTINUE
C
C     Build the embedding of B.
C
      IW = IB
      IS = IW + N*M
      DO 80 J = 1, M
         IW1 = IW
         DO 60 I = 1, M
            DWORK( IW ) = -DIMAG( B( I, J ) )
            IW = IW + 1
   60    CONTINUE
C
         DO 70 I = 1, M
            DWORK( IW ) =   DBLE( B( I, J ) )
            DWORK( IS ) = -DWORK( IW )
            IW = IW + 1
            IS = IS + 1
   70    CONTINUE
         CALL DCOPY( M, DWORK( IW1 ), 1, DWORK( IS ), 1 )
         IS = IS + M
   80 CONTINUE
C
C     Build the embeddings of F and G.
C
      IW = IFG
      DO 110 J = 1, M + 1
         DO 90 I = 1, M
            DWORK( IW ) = -DIMAG( FG( I, J ) )
            IW = IW + 1
   90    CONTINUE
C
         IW = IW + J - 1
         IS = IW
         DO 100 I = J, M
            DWORK( IW ) =  DBLE( FG( I, J ) )
            DWORK( IS ) = DWORK( IW )
            IW = IW + 1
            IS = IS + N
  100    CONTINUE
  110 CONTINUE
C
      IW1 = IW
      I1  = IW
      DO 130 J = 2, M + 1
         IS = I1
         I1 = I1 + 1
         DO 120 I = 1, J - 1
            DWORK( IW ) = -DBLE( FG( I, J ) )
            DWORK( IS ) = DWORK( IW )
            IW = IW + 1
            IS = IS + N
  120    CONTINUE
         IW = IW + N - J + 1
  130 CONTINUE
      CALL DLACPY( 'Full', M, M+1, DWORK( IFG ), N, DWORK( IW1-M ), N )
C
C     Apply MB04ED to transform the extended pencil to real
C     skew-Hamiltonian/skew-Hamiltonian Schur form.
C
C     Real workspace:    need   w1 + w2, where
C                        w2 = 6*N**2+MAX(6*N, 27),
C                                        if COMPQ = 'C' or  COMPU = 'C';
C                        w2 = 3*N**2+MAX(6*N, 27),
C                                        if COMPQ = 'N' and COMPU = 'N';
C                        prefer larger.
C     Integer workspace: need   2*N+9.
C
      CALL MB04ED( JOB, CMPQ, CMPU, N2, DWORK( IZ11 ), N2, DWORK( IB ),
     $             N, DWORK( IFG ), N, DWORK( IQ ), N2, DWORK( IU ), N,
     $             DWORK( IU+NN ), N, ALPHAI, ALPHAR, BETA, IWORK,
     $             LIWORK, DWORK( IWRK ), LDWORK-IWRK+1, INFO )
      IF( INFO.GT.0 .AND. INFO.LT.3 )
     $   RETURN
      OPTDW = MAX( MINDW, MINDB + INT( DWORK( IWRK ) ) )
C
C     Scale the eigenvalues.
C
      CALL DSCAL( N, -ONE, ALPHAI, 1 )
C
C     Convert the transformation matrices to complex datatype.
C
      IF( LCMPQ ) THEN
         IW = IQ
         DO 150 J = 1, N2
            DO 140 I = 1, N2
               Q( I, J ) = DCMPLX( DWORK( IW ) )
               IW = IW + 1
  140       CONTINUE
  150    CONTINUE
      END IF
C
      IF( LCMPU ) THEN
         IW = IU
         DO 170 J = 1, N2
            DO 160 I = 1, N
               U( I, J ) = DCMPLX( DWORK( IW ) )
               IW = IW + 1
  160       CONTINUE
  170    CONTINUE
      END IF
C
C     Return if only the eigenvalues are desired.
C
      IF( .NOT.LTRI ) THEN
         ZWORK( 1 ) = OPTZW
         DWORK( 1 ) = OPTDW
         IW1 = IWORK( 1 )
         IW  = IWORK( 2*IW1+4 )
         K   = 3*( N - 2*IW + 1 ) + 12*IW
         CALL DCOPY( K, DWORK( IWRK+1 ), 1, DWORK( 2 ), 1 )
         RETURN
      END IF
C
C     Convert the other results to complex datatype.
C
      IW = IZ11
      DO 190 J = 1, N
         DO 180 I = 1, J
            Z( I, J ) = DCMPLX( DWORK( IW ) )
            IW = IW + 1
  180    CONTINUE
         IW = IW + N2 - J
  190 CONTINUE
C
      IW = IZ11 + N2*N
      DO 220 J = 1, N
         DO 200 I = 1, N
            D( I, J ) = DCMPLX( DWORK( IW ) )
            IW = IW + 1
  200    CONTINUE
         IW = IW + J - 1
C
         DO 210 I = J, N
            C( I, J ) = DCMPLX( DWORK( IW ) )
            IW = IW + 1
  210    CONTINUE
  220 CONTINUE
C
      IW = IB
      DO 240 J = 1, N
         DO 230 I = 1, MIN( J + 1, N )
            B( I, J ) = DCMPLX( DWORK( IW ) )
            IW = IW + 1
  230    CONTINUE
         IW = IW + N - J - 1
  240 CONTINUE
C
      IW = IFG + N
      DO 260 J = 1, N
         DO 250 I = 1, J - 1
            FG( I, J ) = DCMPLX( DWORK( IW ) )
            IW = IW + 1
  250    CONTINUE
         FG( J, J ) = CZERO
         IW = IW + N - J + 1
  260 CONTINUE
C
C     Triangularize the 2-by-2 diagonal blocks in B using the complex
C     version of the periodic QZ algorithm.
C
C     Set up pointers on the inputs and outputs of MB03BZ.
C     A block algorithm is used for updating the matrices for large N.
C
      IS   = IWRK
      IQ2  = 1
      IQ   = IQ2  + 4
      IU   = IQ   + 4
      IB   = IU   + 4
      IZ11 = IB   + 4
      IZ22 = IZ11 + 4
      IEV  = IZ22 + 4
      IQB  = IEV  + 4
      IUB  = IQB  + 4*M
      IWRK = IUB  + 4*M
C
C     Set the signatures of the input matrices of MB03BZ.
C
      IWORKZ( 1 ) =  1
      IWORKZ( 2 ) = -1
      IWORKZ( 3 ) = -1
C
      J  = 1
      J1 = 1
      J2 = MIN( N, NB )
C     WHILE( J.LT.N ) DO
  270 CONTINUE
      IF( J.LT.N ) THEN
         NRMB = ABS( B( J, J ) ) + ABS( B( J+1, J+1 ) )
         IF( ABS( B( J+1, J ) ).GT.NRMB*EPS ) THEN
C
C           Triangularization step. Row transformations are blocked.
C           Complex workspace:    need   8*N + 28, if COMPQ = 'C';
C                                        6*N + 28, if COMPQ = 'N'.
C           Real workspace:       need   2.
C           Integer workspace:    need   5.
C
            NC  = MAX( J2-J-1,  0 )
            J3  = MIN( J2-J1+1, J-1 )
            JM1 = MAX( J-1, 1 )
            JP2 = MIN( J+2, N )
            NJ1 = MAX( N-J-1, 1 )
            CALL ZLACPY( 'Full',  2, 2, B( J, J ), LDB, ZWORK( IB ), 2 )
            CALL ZLACPY( 'Upper', 2, 2, Z( J, J ), LDZ, ZWORK( IZ11 ),
     $                   2 )
            ZWORK( IZ11+1 ) = CZERO
            ZWORK( IZ22 )   = DCONJG( C( J, J ) )
            ZWORK( IZ22+1 ) = CZERO
            ZWORK( IZ22+2 ) = DCONJG( C( J+1, J ) )
            ZWORK( IZ22+3 ) = DCONJG( C( J+1, J+1 ) )
C
            CALL MB03BZ( 'Schur form', 'Initialize', 3, 2, 1, 2, IWORKZ,
     $                   ZWORK( IB ), 2, 2, ZWORK( IQ2 ), 2, 2,
     $                   ZWORK( IEV ), ZWORK( IEV+2 ), IWORKZ( 4 ),
     $                   DWORKZ, 2, ZWORK( IWRK ), LZWORK-IWRK+1, INFO )
            IF( INFO.GT.0 ) THEN
               INFO = 2
               RETURN
            END IF
C
C           Update a panel of Z.
C
            CALL ZGEMM(  'No Transpose', 'No Transpose', J-1, 2, 2,
     $                   CONE, Z( 1, J ), LDZ, ZWORK( IQ ), 2, CZERO,
     $                   ZWORK( IWRK ), JM1 )
            CALL ZLACPY( 'Full', J-1, 2, ZWORK( IWRK ), JM1, Z( 1, J ),
     $                   LDZ )
            CALL ZLACPY( 'Upper', 2, 2, ZWORK( IZ11 ), 2, Z( J, J ),
     $                   LDZ )
            Z( J+1, J ) = CZERO
            CALL ZGEMM(  'Conjugate Transpose', 'No Transpose', 2, NC,
     $                   2, CONE, ZWORK( IU ), 2, Z( J, JP2 ), LDZ,
     $                   CZERO, ZWORK( IWRK ), 2 )
            CALL ZLACPY( 'Full', 2, NC, ZWORK( IWRK ), 2, Z( J, JP2 ),
     $                   LDZ )
C
C           Update the columns J and J+1 of D.
C           The transformations on rows are made outside this loop.
C
            CALL ZGEMM(  'No Transpose', 'No Transpose', N, 2, 2, CONE,
     $                   D( 1, J ), LDD, ZWORK( IQ2 ), 2, CZERO,
     $                   ZWORK( IWRK ), N )
            CALL ZLACPY( 'Full', N, 2, ZWORK( IWRK ), N, D( 1, J ),
     $                   LDD )
C
C           Similarly, update C.
C
            C( J,   J   ) = DCONJG( ZWORK( IZ22 ) )
            C( J+1, J   ) = DCONJG( ZWORK( IZ22+2 ) )
            C( J,   J+1 ) = CZERO
            C( J+1, J+1 ) = DCONJG( ZWORK( IZ22+3 ) )
            CALL ZGEMM(  'No Transpose', 'No Transpose', N-J-1, 2, 2,
     $                   CONE, C( JP2, J ), LDC, ZWORK( IQ2 ), 2, CZERO,
     $                   ZWORK( IWRK ), NJ1 )
            CALL ZLACPY( 'Full', N-J-1, 2, ZWORK( IWRK ), NJ1,
     $                   C( JP2, J ), LDC )
C
C           Update a panel of B.
C
            CALL ZGEMM(  'No Transpose', 'No Transpose', J-1, 2, 2,
     $                   CONE, B( 1, J ), LDB, ZWORK( IQ ), 2, CZERO,
     $                   ZWORK( IWRK ), JM1 )
            CALL ZLACPY( 'Full', J-1, 2, ZWORK( IWRK ), JM1, B( 1, J ),
     $                   LDB )
            CALL ZLACPY( 'Upper', 2, 2, ZWORK( IB ), 2, B( J, J ),
     $                   LDB )
            B( J+1, J ) = CZERO
            CALL ZGEMM(  'Conjugate Transpose', 'No Transpose', 2, NC,
     $                   2, CONE, ZWORK( IQ2 ), 2, B( J, JP2 ), LDB,
     $                   CZERO, ZWORK( IWRK ), 2 )
            CALL ZLACPY( 'Full', 2, NC, ZWORK( IWRK ), 2, B( J, JP2 ),
     $                   LDB )
C
C           Update a panel of F.
C
            TMP          =  FG( J+1, J )
            FG( J+1, J ) = -FG( J, J+1 )
            CALL ZGEMM(  'No Transpose', 'No Transpose', J+1, 2, 2,
     $                   CONE, FG( 1, J ), LDFG, ZWORK( IQ2 ), 2, CZERO,
     $                   ZWORK( IWRK ), J+1 )
            CALL ZLACPY( 'Full', J+1, 2, ZWORK( IWRK ), J+1, FG( 1, J ),
     $                   LDFG )
            CALL ZGEMM(  'Conjugate Transpose', 'No Transpose', 2,
     $                   J2-J+1, 2, CONE, ZWORK( IQ2 ), 2, FG( J, J ),
     $                   LDFG, CZERO, ZWORK( IWRK ), 2 )
            CALL ZLACPY( 'Full', 2, J2-J+1, ZWORK( IWRK ), 2,
     $                   FG( J, J ), LDFG )
            FG( J+1, J ) = TMP
C
            IF( LCMPQ ) THEN
C
C              Update Q.
C
               CALL ZGEMM(  'No Transpose', 'No Transpose', N2, 2, 2,
     $                      CONE, Q( 1, J ), LDQ, ZWORK( IQ ), 2, CZERO,
     $                      ZWORK( IWRK ), N2 )
               CALL ZLACPY( 'Full', N2, 2, ZWORK( IWRK ), N2,
     $                      Q( 1, J ), LDQ )
               CALL ZGEMM(  'No Transpose', 'No Transpose', N2, 2, 2,
     $                      CONE, Q( 1, N+J ), LDQ, ZWORK( IQ2 ), 2,
     $                      CZERO, ZWORK( IWRK ), N2 )
               CALL ZLACPY( 'Full', N2, 2, ZWORK( IWRK ), N2,
     $                      Q( 1, N+J ), LDQ )
            END IF
C
            IF( LCMPU ) THEN
C
C              Update U.
C
               CALL ZGEMM(  'No Transpose', 'No Transpose', N, 2, 2,
     $                      CONE, U( 1, J ), LDU, ZWORK( IU ), 2, CZERO,
     $                      ZWORK( IWRK ), N )
               CALL ZLACPY( 'Full', N, 2, ZWORK( IWRK ), N, U( 1, J ),
     $                      LDU )
               CALL ZGEMM(  'No Transpose', 'No Transpose', N, 2, 2,
     $                      CONE, U( 1, N+J ), LDU, ZWORK( IU ), 2,
     $                      CZERO, ZWORK( IWRK ), N )
               CALL ZLACPY( 'Full', N, 2, ZWORK( IWRK ), N, U( 1, N+J ),
     $                      LDU )
            END IF
C
C           Save the needed transformations.
C
            BWORK( J ) = .TRUE.
            J = J + 2
            CALL ZLACPY( 'Full', 2, 2, ZWORK( IQ2 ), 2, ZWORK( IQB ),
     $                   2 )
            CALL ZLACPY( 'Full', 2, 2, ZWORK( IU ), 2, ZWORK( IUB ), 2 )
            IQB = IQB + 4
            IUB = IUB + 4
         ELSE
            BWORK( J )  = .FALSE.
            B( J+1, J ) = CZERO
            J = J + 1
         END IF
C
         IF( J.GE.J2 .AND. J.LE.N ) THEN
            IQB = IEV + 4
            IUB = IQB + 4*M
C
C           Start to update the next panel of Z, B, and F for previous
C           transformations on rows.
C
            I  = 1
            J1 = J2 + 1
            J2 = MIN( N, J1 + NB - 1 )
            NC = J2 - J1 + 1
C           WHILE( I.LT.J-1 ) DO
  280       CONTINUE
            IF( I.LT.J-1 ) THEN
               IF( BWORK( I ) ) THEN
                  CALL ZGEMM(  'Conjugate Transpose', 'No Transpose', 2,
     $                         NC, 2, CONE, ZWORK( IUB ), 2, Z( I, J1 ),
     $                         LDZ, CZERO, ZWORK( IWRK ), 2 )
                  CALL ZLACPY( 'Full', 2, NC, ZWORK( IWRK ), 2,
     $                         Z( I, J1 ), LDZ )
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
                  IUB = IUB + 4
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
      J1 = 1
      J2 = MIN( N, NB )
C     WHILE( MAX( J1, J2 ).LE.N ) DO
  290 CONTINUE
      IF( MAX( J1, J2 ).LE.N ) THEN
         IQB = IEV + 4
         IUB = IQB + 4*M
C
C        Update the panel of columns J1 to J2 of D and C for the
C        transformations on rows.
C
         I  = 1
         NC = J2 - J1 + 1
C        WHILE( I.LT.N ) DO
  300    CONTINUE
         IF( I.LT.N ) THEN
            IF( BWORK( I ) ) THEN
               CALL ZGEMM(  'Conjugate Transpose', 'No Transpose', 2,
     $                     NC, 2, CONE, ZWORK( IUB ), 2, D( I, J1 ),
     $                     LDD, CZERO, ZWORK( IWRK ), 2 )
               CALL ZLACPY( 'Full', 2, NC, ZWORK( IWRK ), 2, D( I, J1 ),
     $                      LDD )
C
               IF( I.GT.J1 ) THEN
                  J3 = MIN( NC, I - J1 )
                  CALL ZGEMM(  'Conjugate Transpose', 'No Transpose', 2,
     $                         J3, 2, CONE, ZWORK( IUB ), 2, C( I, J1 ),
     $                         LDC, CZERO, ZWORK( IWRK ), 2 )
                  CALL ZLACPY( 'Full', 2, J3, ZWORK( IWRK ), 2,
     $                         C( I, J1 ), LDC )
               END IF
C
               IQB = IQB + 4
               IUB = IUB + 4
C
               I = I + 2
            ELSE
               I = I + 1
            END IF
            GO TO 300
         END IF
C        END WHILE 300
         J1 = J2 + 1
         J2 = MIN( N, J1 + NB - 1 )
         GO TO 290
C        END WHILE 290
      END IF
C
C     Scale B and F by -i.
C
      DO 310 I = 1, N
         CALL ZSCAL( I, -CIMAG, B( 1, I ), 1 )
  310 CONTINUE
C
      DO 320 I = 1, N
         CALL ZSCAL( I, -CIMAG, FG( 1, I ), 1 )
  320 CONTINUE
C
      ZWORK( 1 ) = OPTZW
      DWORK( 1 ) = OPTDW
      IW1 = IWORK( 1 )
      IW  = IWORK( 2*IW1+4 )
      K   = 3*( N - 2*IW + 1 ) + 12*IW
      CALL DCOPY( K, DWORK( IS+1 ), 1, DWORK( 2 ), 1 )
C
      RETURN
C *** Last line of MB04AZ ***
      END
