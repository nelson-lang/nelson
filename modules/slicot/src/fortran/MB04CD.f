      SUBROUTINE MB04CD( COMPQ1, COMPQ2, COMPQ3, N, A, LDA, B, LDB, D,
     $                   LDD, Q1, LDQ1, Q2, LDQ2, Q3, LDQ3, IWORK,
     $                   LIWORK, DWORK, LDWORK, BWORK, INFO )
C
C     PURPOSE
C
C     To compute the transformed matrices A, B and D, using orthogonal
C     matrices Q1, Q2 and Q3 for a real N-by-N regular pencil
C
C                     ( A11   0  ) ( B11   0  )     (  0   D12 )
C       aA*B - bD = a (          ) (          ) - b (          ),    (1)
C                     (  0   A22 ) (  0   B22 )     ( D21   0  )
C
C     where A11, A22, B11, B22 and D12 are upper triangular, D21 is
C     upper quasi-triangular and the generalized matrix product 
C        -1        -1    -1        -1
C     A11   D12 B22   A22   D21 B11   is upper quasi-triangular, such
C     that Q3' A Q2, Q2' B Q1 are upper triangular, Q3' D Q1 is upper
C     quasi-triangular and the transformed pencil
C     a(Q3' A B Q1) - b(Q3' D Q1) is in generalized Schur form. The
C     notation M' denotes the transpose of the matrix M.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     COMPQ1  CHARACTER*1
C             Specifies whether to compute the orthogonal transformation
C             matrix Q1, as follows:
C             = 'N':  Q1 is not computed;
C             = 'I':  the array Q1 is initialized internally to the unit
C                     matrix, and the orthogonal matrix Q1 is returned;
C             = 'U':  the array Q1 contains an orthogonal matrix Q01 on
C                     entry, and the matrix Q01*Q1 is returned, where Q1
C                     is the product of the orthogonal transformations
C                     that are applied on the right to the pencil
C                     aA*B - bD in (1).
C
C     COMPQ2  CHARACTER*1
C             Specifies whether to compute the orthogonal transformation
C             matrix Q2, as follows:
C             = 'N':  Q2 is not computed;
C             = 'I':  the array Q2 is initialized internally to the unit
C                     matrix, and the orthogonal matrix Q2 is returned;
C             = 'U':  the array Q2 contains an orthogonal matrix Q02 on
C                     entry, and the matrix Q02*Q2 is returned, where Q2
C                     is the product of the orthogonal transformations
C                     that are applied on the left to the pencil
C                     aA*B - bD in (1).
C
C     COMPQ3  CHARACTER*1
C             Specifies whether to compute the orthogonal transformation
C             matrix Q3, as follows:
C             = 'N':  Q3 is not computed;
C             = 'I':  the array Q3 is initialized internally to the unit
C                     matrix, and the orthogonal matrix Q3 is returned;
C             = 'U':  the array Q3 contains an orthogonal matrix Q01 on
C                     entry, and the matrix Q03*Q3 is returned, where Q3
C                     is the product of the orthogonal transformations
C                     that are applied on the right to the pencil
C                     aA*B - bD in (1).
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             Order of the pencil aA*B - bD.  N >= 0, even.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
C             On entry, the leading N-by-N block diagonal part of this
C             array must contain the matrix A in (1). The off-diagonal
C             blocks need not be set to zero.
C             On exit, the leading N-by-N part of this array contains
C             the transformed upper triangular matrix.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1, N).
C
C     B       (input/output) DOUBLE PRECISION array, dimension (LDB, N)
C             On entry, the leading N-by-N block diagonal part of this
C             array must contain the matrix B in (1). The off-diagonal
C             blocks need not be set to zero.
C             On exit, the leading N-by-N part of this array contains
C             the transformed upper triangular matrix.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= MAX(1, N).
C
C     D       (input/output) DOUBLE PRECISION array, dimension (LDD, N)
C             On entry, the leading N-by-N block anti-diagonal part of
C             this array must contain the matrix D in (1). The diagonal
C             blocks need not be set to zero.
C             On exit, the leading N-by-N part of this array contains
C             the transformed upper quasi-triangular matrix.
C
C     LDD     INTEGER
C             The leading dimension of the array D.  LDD >= MAX(1, N).
C
C     Q1      (input/output) DOUBLE PRECISION array, dimension (LDQ1, N)
C             On entry, if COMPQ1 = 'U', then the leading N-by-N part of
C             this array must contain a given matrix Q01, and on exit,
C             the leading N-by-N part of this array contains the product
C             of the input matrix Q01 and the transformation matrix Q1
C             used to transform the matrices A, B, and D.
C             On exit, if COMPQ1 = 'I', then the leading N-by-N part of
C             this array contains the orthogonal transformation matrix
C             Q1.
C             If COMPQ1 = 'N' this array is not referenced.
C
C     LDQ1    INTEGER
C             LDQ1 >= 1,         if COMPQ1 = 'N';
C             LDQ1 >= MAX(1, N), if COMPQ1 = 'I' or COMPQ1 = 'U'.
C
C     Q2      (input/output) DOUBLE PRECISION array, dimension (LDQ2, N)
C             On entry, if COMPQ2 = 'U', then the leading N-by-N part of
C             this array must contain a given matrix Q02, and on exit,
C             the leading N-by-N part of this array contains the product
C             of the input matrix Q02 and the transformation matrix Q2
C             used to transform the matrices A, B, and D.
C             On exit, if COMPQ2 = 'I', then the leading N-by-N part of
C             this array contains the orthogonal transformation matrix
C             Q2.
C             If COMPQ2 = 'N' this array is not referenced.
C
C     LDQ2    INTEGER
C             The leading dimension of the array Q2.
C             LDQ2 >= 1,         if COMPQ2 = 'N';
C             LDQ2 >= MAX(1, N), if COMPQ2 = 'I' or COMPQ2 = 'U'.
C
C     Q3      (input/output) DOUBLE PRECISION array, dimension (LDQ3, N)
C             On entry, if COMPQ3 = 'U', then the leading N-by-N part of
C             this array must contain a given matrix Q03, and on exit,
C             the leading N-by-N part of this array contains the product
C             of the input matrix Q03 and the transformation matrix Q3
C             used to transform the matrices A, B and D.
C             On exit, if COMPQ3 = 'I', then the leading N-by-N part of
C             this array contains the orthogonal transformation matrix
C             Q3.
C             If COMPQ3 = 'N' this array is not referenced.
C
C     LDQ3    INTEGER
C             The leading dimension of the array Q3.
C             LDQ3 >= 1,         if COMPQ3 = 'N';
C             LDQ3 >= MAX(1, N), if COMPQ3 = 'I' or COMPQ3 = 'U'.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (LIWORK)
C
C     LIWORK  INTEGER
C             The dimension of the array IWORK.
C             LIWORK >= MAX( N/2+1, 48 ).
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal LDWORK.
C             On exit, if INFO = -20, DWORK(1) returns the minimum value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The dimension of the array DWORK.
C             LDWORK >= 3*N*N + MAX( N/2 + 252, 432 ).
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
C             = 1: the periodic QZ algorithm failed to reorder the
C                  eigenvalues (the problem is very ill-conditioned) in
C                  the SLICOT Library routine MB03KD;
C             = 2: the standard QZ algorithm failed in the LAPACK
C                  routine DGGEV, called by the SLICOT routine MB03CD;
C             = 3: the standard QZ algorithm failed in the LAPACK
C                  routines DGGES, called by the SLICOT routines MB03CD
C                  or MB03ED;
C             = 4: the standard QZ algorithm failed to reorder the
C                  eigenvalues in the LAPACK routine DTGSEN, called by
C                  the SLICOT routine MB03CD.
C
C     METHOD
C
C     First, the periodic QZ algorithm (see also [2] and [3]) is applied
C                                     -1        -1    -1        -1
C     to the formal matrix product A11   D12 B22   A22   D21 B11   to
C     reorder the eigenvalues, i.e., orthogonal matrices V1, V2, V3, V4,
C     V5 and V6 are computed such that V2' A11 V1, V2' D12 V3,
C     V4' B22 V3, V5' A22 V4, V5' D21 V6 and V1' B11 V6 keep the
C     triangular form, but they can be partitioned into 2-by-2 block
C     forms and the last diagonal blocks correspond to all nonpositive
C     real eigenvalues of the formal product, and the first diagonal
C     blocks correspond to the remaining eigenvalues.
C
C     Second, Q1 = diag(V6, V3), Q2 = diag(V1, V4), Q3 = diag(V2, V5)
C     and
C
C                      ( AA11 AA12   0    0  )
C                      (                     )
C                      (   0  AA22   0    0  )
C     A := Q3' A Q2 =: (                     ),
C                      (   0    0  AA33 AA34 )
C                      (                     )
C                      (   0    0    0  AA44 )
C
C                      ( BB11 BB12   0    0  )
C                      (                     )
C                      (   0  BB22   0    0  )
C     B := Q2' B Q1 =: (                     ),
C                      (   0    0  BB33 BB34 )
C                      (                     )
C                      (   0    0    0  BB44 )
C
C                      (   0    0  DD13 DD14 )
C                      (                     )
C                      (   0    0    0  DD24 )
C     D := Q3' D Q1 =: (                     ),
C                      ( DD31 DD32   0    0  )
C                      (                     )
C                      (   0  DD42   0    0  )
C
C                            -1          -1     -1          -1
C     are set, such that AA22   DD24 BB44   AA44   DD42 BB22   has only
C     nonpositive real eigenvalues.
C
C     Third, the permutation matrix
C
C         (  I  0  0  0  )
C         (              )
C         (  0  0  I  0  )
C     P = (              ),
C         (  0  I  0  0  )
C         (              )
C         (  0  0  0  I  )
C
C     where I denotes the identity matrix of appropriate size is used to
C     transform aA*B - bD to block upper triangular form
C
C                   ( AA11   0  | AA12   0  )
C                   (           |           )
C                   (   0  AA33 |   0  AA34 )   ( AA1  *  )
C     A := P' A P = (-----------+-----------) = (         ),
C                   (   0    0  | AA22   0  )   (  0  AA2 )
C                   (           |           )
C                   (   0    0  |   0  AA44 )
C
C                   ( BB11   0  | BB12   0  )
C                   (           |           )
C                   (   0  BB33 |   0  BB34 )   ( BB1  *  )
C     B := P' B P = (-----------+-----------) = (         ),
C                   (   0    0  | BB22   0  )   (  0  BB2 )
C                   (           |           )
C                   (   0    0  |   0  BB44 )
C
C                   (   0  DD13 |   0  DD14 )
C                   (           |           )
C                   ( DD31   0  | DD32   0  )   ( DD1  *  )
C     D := P' D P = (-----------+-----------) = (         ).
C                   (   0    0  |   0  DD24 )   (  0  DD2 )
C                   (           |           )
C                   (   0    0  | DD42   0  )
C
C     Then, further orthogonal transformations that are provided by the
C     SLICOT Library routines MB03ED and MB03CD are used to
C     triangularize the subpencil aAA1 BB1 - bDD1.
C
C     Finally, the subpencil aAA2 BB2 - bDD2 is triangularized by
C     applying a special permutation matrix.
C
C     See also page 22 in [1] for more details.
C
C     REFERENCES
C
C     [1] Benner, P., Byers, R., Losse, P., Mehrmann, V. and Xu, H.
C         Numerical Solution of Real Skew-Hamiltonian/Hamiltonian
C         Eigenproblems.
C         Tech. Rep., Technical University Chemnitz, Germany,
C         Nov. 2007.
C
C     [2] Bojanczyk, A., Golub, G. H. and Van Dooren, P.
C         The periodic Schur decomposition: algorithms and applications.
C         In F.T. Luk (editor), Advanced Signal Processing Algorithms,
C         Architectures, and Implementations III, Proc. SPIE Conference,
C         vol. 1770, pp. 31-42, 1992.
C
C     [3] Hench, J. J. and Laub, A. J.
C         Numerical Solution of the discrete-time periodic Riccati
C         equation. IEEE Trans. Automat. Control, 39, 1197-1210, 1994.
C
C     NUMERICAL ASPECTS
C                                                               3
C     The algorithm is numerically backward stable and needs O(N ) real
C     floating point operations.
C
C     CONTRIBUTOR
C
C     Matthias Voigt, Fakultaet fuer Mathematik, Technische Universitaet
C     Chemnitz, January 21, 2009.
C     V. Sima, Aug. 2009 (SLICOT version of the routine DBTFMT).
C
C     REVISIONS
C
C     V. Sima, Jan. 2011, Aug. 2011, July 2014.
C     M. Voigt, Jan. 2012, July 2013.
C
C     KEYWORDS
C
C     Eigenvalue reordering, matrix pencil, periodic QZ algorithm,
C     upper (quasi-)triangular matrix.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, HUND2
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, HUND2 = 2.0D+2 )
C
C     .. Scalar Arguments ..
      CHARACTER          COMPQ1, COMPQ2, COMPQ3
      INTEGER            INFO, LDA, LDB, LDD, LDQ1, LDQ2, LDQ3, LDWORK,
     $                   LIWORK, N
C
C     .. Array Arguments ..
      LOGICAL            BWORK( * )
      INTEGER            IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), D( LDD, * ),
     $                   DWORK( * ), Q1( LDQ1, * ), Q2( LDQ2, * ),
     $                   Q3( LDQ3, * )
C
C     .. Local Scalars ..
      LOGICAL            LCMPQ1, LCMPQ2, LCMPQ3, LINIQ1, LINIQ2, LINIQ3,
     $                   LQUERY, LUPDQ1, LUPDQ2, LUPDQ3
      INTEGER            DIM1, DIM2, I, I1, I1LOLE, I1LORI, I1UPLE,
     $                   I1UPRI, I2, I2LOLE, I2LORI, I2UPLE, I2UPRI,
     $                   I3, I3LOLE, I3LORI, I3UPLE, I3UPRI, IA, IA11,
     $                   IA22, IALOLE, IALORI, IAUPLE, IAUPRI, IB, IB1,
     $                   IB11, IB2, IB22, IBLOLE, IBLORI, IBUPLE,
     $                   IBUPRI, ID12, ID21, IDLOLE, IDLORI, IDUPLE,
     $                   IDUPRI, IJ1, IJ2, ITMP, ITMP2, ITMP3, IV1, IV2,
     $                   IV3, IV4, IV5, IV6, IWRK, J, K, KSCHUR, M, M1,
     $                   M2, M4, MINWRK, MM, MP1, NR, NROW, OPTWRK, R,
     $                   SDIM
      DOUBLE PRECISION   BASE, LGBAS, TMP2, TMP3, ULP
C
C     .. Local Arrays ..
      LOGICAL            BW(   4 )
      INTEGER            IDUM( 1 )
      DOUBLE PRECISION   DUM(  1 )
C
C     .. External Functions ..
      LOGICAL            LSAME, SB02OW
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH, LSAME, SB02OW
C
C     .. External Subroutines ..
      EXTERNAL           DAXPY, DCOPY, DGEMM, DGEMV, DGGES, DGGEV,
     $                   DLACPY, DLASET, DSCAL, DTGSEN, MA01BD, MB03BA,
     $                   MB03CD, MB03ED, MB03KD, XERBLA
C
C     .. Intrinsic Functions ..
      INTRINSIC          INT, LOG, MAX, MIN, MOD
C
C     .. Executable Statements ..
C
C     Decode the input arguments.
C
      M  = N/2
      MM = M*M
      LINIQ1 = LSAME( COMPQ1, 'I' )
      LUPDQ1 = LSAME( COMPQ1, 'U' )
      LINIQ2 = LSAME( COMPQ2, 'I' )
      LUPDQ2 = LSAME( COMPQ2, 'U' )
      LINIQ3 = LSAME( COMPQ3, 'I' )
      LUPDQ3 = LSAME( COMPQ3, 'U' )
      LCMPQ1 = LINIQ1 .OR. LUPDQ1
      LCMPQ2 = LINIQ2 .OR. LUPDQ2
      LCMPQ3 = LINIQ3 .OR. LUPDQ3
      LQUERY = LDWORK.EQ.-1
      MINWRK = 12*MM + MAX( M + 252, 432 )
C
C     Test the input arguments.
C
      INFO = 0
      IF( .NOT.( LSAME( COMPQ1, 'N' ) .OR. LCMPQ1 ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( LSAME( COMPQ2, 'N' ) .OR. LCMPQ2 ) ) THEN
         INFO = -2
      ELSE IF( .NOT.( LSAME( COMPQ3, 'N' ) .OR. LCMPQ3 ) ) THEN
         INFO = -3
      IF( N.LT.0 .OR. MOD( N, 2 ).NE.0 ) THEN
         INFO = -4
      ELSE IF(  LDA.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF(  LDB.LT.MAX( 1, N ) ) THEN
         INFO = -8
      ELSE IF(  LDD.LT.MAX( 1, N ) ) THEN
         INFO = -10
      ELSE IF( LDQ1.LT.MAX( 1, N ) ) THEN
         INFO = -12
      ELSE IF( LDQ2.LT.MAX( 1, N ) ) THEN
         INFO = -14
      ELSE IF( LDQ3.LT.MAX( 1, N ) ) THEN
         INFO = -16
      ELSE IF( LIWORK.LT.MAX( M + 1, 48 ) ) THEN
         INFO = -18
      ELSE IF( .NOT. LQUERY .AND. LDWORK.LT.MINWRK ) THEN
         DWORK( 1 ) = MINWRK
         INFO = -20
      END IF
C
      END IF
      IF( INFO.NE.0) THEN
         CALL XERBLA( 'MB04CD', -INFO )
         RETURN
      ELSE IF( N.GT.0 ) THEN
C
C        Compute optimal workspace.
C
         I = MIN( 4, N )
         DO 5 J = 1, I
            BW( I ) = .TRUE.
    5    CONTINUE
         CALL DGGES(  'Vectors', 'Vectors', 'Sorted', SB02OW, I, A, LDA,
     $                B, LDB, IDUM( 1 ), DWORK, DWORK, DWORK, Q1, I, Q2,
     $                I, DWORK, -1, BW, INFO )
         CALL DGGES(  'Vectors', 'Vectors', 'Not sorted', SB02OW, I, A,
     $                LDA, B, LDB, IDUM( 1 ), DWORK, DWORK, DWORK, Q1,
     $                I, Q2, I, DWORK( 2 ), -1, BW, INFO )
         CALL DGGEV(  'No Vector', 'No Vector', 2, A, LDA, B, LDB,
     $                DWORK, DWORK, DWORK, DUM, 1, DUM, 1, DWORK( 3 ),
     $                -1, INFO )
         CALL DTGSEN( 0, .TRUE., .TRUE., BW, I, A, LDA, B, LDB, DWORK,
     $                DWORK, DWORK, Q1, I, Q2, I, IDUM( 1 ), TMP2,
     $                TMP2, DUM, DWORK( 4 ), -1, IDUM, 1, INFO )
C
         OPTWRK = MAX( 96 + MAX( 28 + INT( DWORK( 1 ) ), 4*M + 8, 4*N,
     $                           24 + INT( DWORK( 2 ) ),
     $                            6 + INT( DWORK( 3 ) ),
     $                           12 + INT( DWORK( 4 ) ), 4*N ), MINWRK )
         IF( LQUERY ) THEN
            DWORK( 1 ) = OPTWRK
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
C     Computations. Note that MB03KD needs reverse ordering of the
C     factors in the formal matrix product, compared to MA01BD, MB03BA.
C     In addition, V6 is interchanged with V2, and V5 with V3, compared
C     to the notation used in section METHOD.
C
      IA11 = 1
      ID12 = IA11 + MM
      IB22 = ID12 + MM
      IA22 = IB22 + MM
      ID21 = IA22 + MM
      IB11 = ID21 + MM
      IV1  = IB11 + MM
      IV2  = IV1  + MM
      IV3  = IV2  + MM
      IV4  = IV3  + MM
      IV5  = IV4  + MM
      IV6  = IV5  + MM
      MP1  = M    + 1
C
C     Get the machine parameters.
C
      ULP   = DLAMCH( 'Precision' )
      BASE  = DLAMCH( 'Base' )
      LGBAS = LOG( BASE )
C
C     Compute maps to access the factors of the formal matrix product.
C
      K = 6
      KSCHUR = 5
      IWORK( 2*K+1 ) = -1
      IWORK( 2*K+2 ) =  1
      IWORK( 2*K+3 ) = -1
      IWORK( 2*K+4 ) = -1
      IWORK( 2*K+5 ) =  1
      IWORK( 2*K+6 ) = -1
      CALL MB03BA( K, KSCHUR, IWORK( 2*K+1 ), I, IWORK, IWORK( K+1 ) )
C
C     Store the factors of the formal matrix product.
C
      DUM( 1 ) = ZERO
      CALL DCOPY(  MM*K, DUM, 0, DWORK, 1 )
      CALL DLACPY( 'Upper', M, M, A, LDA, DWORK, M )
      CALL DLACPY( 'Upper', M, M, A( MP1, MP1 ), LDA, DWORK( IA22 ), M )
      CALL DLACPY( 'Upper', M, M, B, LDB, DWORK( IB11 ), M )
      CALL DLACPY( 'Upper', M, M, B( MP1, MP1 ), LDB, DWORK( IB22 ), M )
      CALL DLACPY( 'Upper', M, M, D( 1, MP1 ),   LDD, DWORK( ID12 ), M )
      CALL DLACPY( 'Upper', M, M, D( MP1, 1 ),   LDD, DWORK( ID21 ), M )
      IF( M.GT.1 )
     $   CALL DCOPY( M-1, D( M+2, 1 ), LDD+1, DWORK( ID21+1 ), MP1 )
C
C     Set BWORK according to the eigenvalues of the formal matrix
C     product in Schur-triangular form.
C     Workspace:   need   6*M*M + 2.
C
      J  = 1
      IA = IV1
      IB = IA + 1
C
C     WHILE( J.LE.M ) DO
   10 CONTINUE
      IF( J.LT.M ) THEN
         IF( DWORK( ID21+J+(J-1)*M ).EQ.ZERO ) THEN
            CALL MA01BD( BASE, LGBAS, K, IWORK( 2*K+1 ),
     $                   DWORK( (J-1)*M+J ), MM, DWORK( IA ),
     $                   DWORK( IB ), IWORK( 3*K+1 ) )
            BWORK( J ) = DWORK( IA ).GT.ZERO .OR. DWORK( IB ).EQ.ZERO
            J = J + 1
            GO TO 10
         ELSE
            BWORK( J   ) = .TRUE.
            BWORK( J+1 ) = .TRUE.
            J = J + 2
            GO TO 10
         END IF
      ELSE IF ( J.EQ.M ) THEN
         CALL MA01BD( BASE, LGBAS, K, IWORK( 2*K+1 ), DWORK( MM ), MM,
     $                DWORK( IA ), DWORK( IB ), IWORK( 3*K+1 ) )
         BWORK( J ) = DWORK( IA ).GT.ZERO .OR. DWORK( IB ).EQ.ZERO
      END IF
C     END WHILE 10
C
C     Check if BWORK(J) = .TRUE. for all J.
C
      J = 1
C     WHILE( J.LE.M and BWORK(J) ) DO
   20 CONTINUE
      IF( J.LE.M .AND. BWORK(J) ) THEN
         J = J + 1
         GO TO 20
      END IF
C     END WHILE 20
C
      IF( J.NE.MP1 ) THEN
C
C        Apply periodic QZ algorithm for reordering the eigenvalues.
C        Workspace:   need   12*M*M + MAX(42*K + M, 80*K - 48), K = 6,
C                     if there is at least a pair of adjacent blocks
C                     of order 2 involved in reordering, and M > 10.
C                     Otherwise, the MAX term is slightly smaller.
C
         IWRK = 2*IV1 - 1
         IB11 = 1
         ID21 = IB11 + MM
         IA22 = ID21 + MM
         IB22 = IA22 + MM
         ID12 = IB22 + MM
         IA11 = ID12 + MM
C
         KSCHUR = 2
C
         DO 30 I = 1, K
            IWORK(   I   ) = M
            IWORK(   K+I ) = 0
            IWORK( 3*K+I ) = 1 + ( I - 1 )*MM
   30    CONTINUE
C
         CALL DCOPY( MM*K, DUM, 0, DWORK( IB11 ), 1 )
         CALL DLACPY( 'Upper', M, M, D( MP1, 1 ), LDD, DWORK( ID21 ),
     $                M )
         CALL DLACPY( 'Upper', M, M, D( 1, MP1 ), LDD, DWORK( ID12 ),
     $                M )
         CALL DLACPY( 'Upper', M, M, A, LDA, DWORK( IA11 ), M )
         CALL DLACPY( 'Upper', M, M, A( MP1, MP1 ), LDA, DWORK( IA22 ),
     $                M )
         CALL DLACPY( 'Upper', M, M, B, LDB, DWORK( IB11 ), M )
         CALL DLACPY( 'Upper', M, M, B( MP1, MP1 ), LDB, DWORK( IB22 ),
     $                M )
         IF( M.GT.1 )
     $      CALL DCOPY( M-1, D( M+2, 1 ), LDD+1, DWORK( ID21+1 ), MP1 )
C
         CALL MB03KD( 'Initialize', IDUM, 'NotStrong', K, M, KSCHUR,
     $                 IWORK, IWORK( K+1 ), IWORK( 2*K+1 ), BWORK,
     $                 DWORK, IWORK, IWORK( 3*K+1 ), DWORK( IV1 ),
     $                 IWORK, IWORK( 3*K+1 ), M1, HUND2, IWORK( 4*K+1 ),
     $                 DWORK( IWRK ), LDWORK-IWRK+1, INFO )
         IF( INFO.GT.0 )
     $      RETURN
C
         M2 = M  - M1
         I1 = M1 + 1
         I2 = I1 + M1
         I3 = I2 + M2
         M4 = 2*M2
C
C        If Q1, Q2 and/or Q3 are user-initialized, update them.
C        The (2,1) block of A is used as workspace.
C
         IF( LUPDQ1 ) THEN
            CALL DGEMM(  'No Transpose', 'No Transpose', M, M, M, ONE,
     $                   Q1, LDQ1, DWORK( IV2 ), M, ZERO, A( MP1, 1 ),
     $                   LDA )
            CALL DLACPY( 'Full', M, M, A( MP1, 1 ), LDA, Q1, LDQ1 )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, M, M, ONE,
     $                   Q1( MP1, 1 ), LDQ1, DWORK( IV2 ), M, ZERO,
     $                   A( MP1, 1 ), LDA )
            CALL DLACPY( 'Full', M, M, A( MP1, 1 ), LDA, Q1( MP1, 1 ),
     $                   LDQ1 )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, M, M, ONE,
     $                   Q1( 1, MP1 ), LDQ1, DWORK( IV5 ), M, ZERO,
     $                   A( MP1, 1 ), LDA )
            CALL DLACPY( 'Full', M, M, A( MP1, 1 ), LDA, Q1( 1, MP1 ),
     $                   LDQ1 )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, M, M, ONE,
     $                   Q1( MP1, MP1 ), LDQ1, DWORK( IV5 ), M, ZERO,
     $                   A( MP1, 1 ), LDA )
            CALL DLACPY( 'Full', M, M, A( MP1, 1 ), LDA, Q1( MP1, MP1 ),
     $                   LDQ1 )
C
            IF( M2.GT.0 ) THEN
               CALL DLACPY( 'Full', M, M, Q1( 1, I1 ), LDQ1,
     $                      A( MP1, 1 ), LDA )
               CALL DLACPY( 'Full', M, M1, A( MP1, M2+1 ), LDA,
     $                      Q1( 1, I1 ), LDQ1 )
               CALL DLACPY( 'Full', M, M2, A( MP1, 1 ), LDA,
     $                      Q1( 1, I2 ), LDQ1 )
               CALL DLACPY( 'Full', M, M, Q1( MP1, I1 ), LDQ1,
     $                      A( MP1, 1 ), LDA )
               CALL DLACPY( 'Full', M, M1, A( MP1, M2+1 ), LDA,
     $                      Q1( MP1, I1 ), LDQ1 )
               CALL DLACPY( 'Full', M, M2, A( MP1, 1 ), LDA,
     $                      Q1( MP1, I2 ), LDQ1 )
            END IF
         END IF
C
         IF( LUPDQ2 ) THEN
            CALL DGEMM(  'No Transpose', 'No Transpose', M, M, M, ONE,
     $                   Q2, LDQ2, DWORK( IV1 ), M, ZERO, A( MP1, 1 ),
     $                   LDA )
            CALL DLACPY( 'Full', M, M, A( MP1, 1 ), LDA, Q2, LDQ2 )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, M, M, ONE,
     $                   Q2( MP1, 1 ), LDQ2, DWORK( IV1 ), M, ZERO,
     $                   A( MP1, 1 ), LDA )
            CALL DLACPY( 'Full', M, M, A( MP1, 1 ), LDA, Q2( MP1, 1 ),
     $                   LDQ2 )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, M, M, ONE,
     $                   Q2( 1, MP1 ), LDQ2, DWORK( IV4 ), M, ZERO,
     $                   A( MP1, 1 ), LDA )
            CALL DLACPY( 'Full', M, M, A( MP1, 1 ), LDA, Q2( 1, MP1 ),
     $                   LDQ2 )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, M, M, ONE,
     $                   Q2( MP1, MP1 ), LDQ2, DWORK( IV4 ), M, ZERO,
     $                   A( MP1, 1 ), LDA )
            CALL DLACPY( 'Full', M, M, A( MP1, 1 ), LDA, Q2( MP1, MP1 ),
     $                   LDQ2 )
C
            IF( M2.GT.0 ) THEN
               CALL DLACPY( 'Full', M, M, Q2( 1, I1 ), LDQ2,
     $                      A( MP1, 1 ), LDA )
               CALL DLACPY( 'Full', M, M1, A( MP1, M2+1 ), LDA,
     $                      Q2( 1, I1 ), LDQ2 )
               CALL DLACPY( 'Full', M, M2, A( MP1, 1 ), LDA,
     $                      Q2( 1, I2 ), LDQ2 )
               CALL DLACPY( 'Full', M, M, Q2( MP1, I1 ), LDQ2,
     $                      A( MP1, 1 ), LDA )
               CALL DLACPY( 'Full', M, M1, A( MP1, M2+1 ), LDA,
     $                      Q2( MP1, I1 ), LDQ2 )
               CALL DLACPY( 'Full', M, M2, A( MP1, 1 ), LDA,
     $                      Q2( MP1, I2 ), LDQ2 )
            END IF
         END IF
C
         IF( LUPDQ3 ) THEN
            CALL DGEMM(  'No Transpose', 'No Transpose', M, M, M, ONE,
     $                   Q3, LDQ3, DWORK( IV6 ), M, ZERO, A( MP1, 1 ),
     $                   LDA )
            CALL DLACPY( 'Full', M, M, A( MP1, 1 ), LDA, Q3, LDQ3 )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, M, M, ONE,
     $                   Q3( MP1, 1 ), LDQ3, DWORK( IV6 ), M, ZERO,
     $                   A( MP1, 1 ), LDA )
            CALL DLACPY( 'Full', M, M, A( MP1, 1 ), LDA, Q3( MP1, 1 ),
     $                   LDQ3 )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, M, M, ONE,
     $                   Q3( 1, MP1 ), LDQ3, DWORK( IV3 ), M, ZERO,
     $                   A( MP1, 1 ), LDA )
            CALL DLACPY( 'Full', M, M, A( MP1, 1 ), LDA, Q3( 1, MP1 ),
     $                   LDQ3 )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, M, M, ONE,
     $                   Q3( MP1, MP1 ), LDQ3, DWORK( IV3 ), M, ZERO,
     $                   A( MP1, 1 ), LDA )
            CALL DLACPY( 'Full', M, M, A( MP1, 1 ), LDA, Q3( MP1, MP1 ),
     $                   LDQ3 )
C
            IF( M2.GT.0 ) THEN
               CALL DLACPY( 'Full', M, M, Q3( 1, I1 ), LDQ3,
     $                      A( MP1, 1 ), LDA )
               CALL DLACPY( 'Full', M, M1, A( MP1, M2+1 ), LDA,
     $                      Q3( 1, I1 ), LDQ3 )
               CALL DLACPY( 'Full', M, M2, A( MP1, 1 ), LDA,
     $                      Q3( 1, I2 ), LDQ3 )
               CALL DLACPY( 'Full', M, M, Q3( MP1, I1 ), LDQ3,
     $                      A( MP1, 1 ), LDA )
               CALL DLACPY( 'Full', M, M1, A( MP1, M2+1 ), LDA,
     $                      Q3( MP1, I1 ), LDQ3 )
               CALL DLACPY( 'Full', M, M2, A( MP1, 1 ), LDA,
     $                      Q3( MP1, I2 ), LDQ3 )
            END IF
         END IF
C
C        Make permutations of the corresponding matrices.
C
         IF( M2.GT.0 ) THEN
            CALL DLASET( 'Full', M, M, ZERO, ZERO, A( MP1, 1 ), LDA )
            CALL DLACPY( 'Upper', M1, M1, DWORK( IA11 ), M, A, LDA )
            CALL DLASET( 'Full', M1, M1, ZERO, ZERO, A( 1, I1 ), LDA )
            CALL DLACPY( 'Upper', M1, M1, DWORK( IA22 ), M, A( I1, I1 ),
     $                   LDA )
            CALL DLACPY( 'Full', M1, M2, DWORK( IA11+M*M1 ), M,
     $                   A( 1, I2 ), LDA )
            CALL DLASET( 'Full', M1, M2, ZERO, ZERO, A( I1, I2 ), LDA )
            CALL DLACPY( 'Upper', M2, M2, DWORK( IA11+M*M1+M1 ), M,
     $                   A( I2, I2 ), LDA )
            CALL DLASET( 'Full', M1, M2, ZERO, ZERO, A( 1, I3 ), LDA )
            CALL DLACPY( 'Full', M1, M2, DWORK( IA22+M*M1 ), M,
     $                   A( I1, I3 ), LDA )
            CALL DLASET( 'Full', M2, M2, ZERO, ZERO, A( I2, I3 ), LDA )
            CALL DLACPY( 'Upper', M2, M2, DWORK( IA22+M*M1+M1 ), M,
     $                   A( I3, I3 ), LDA )
C
            CALL DLASET( 'Full', M, M, ZERO, ZERO, B( MP1, 1 ), LDB )
            CALL DLACPY( 'Upper', M1, M1, DWORK( IB11 ), M, B, LDB )
            CALL DLASET( 'Full', M1, M1, ZERO, ZERO, B( 1, I1 ), LDB )
            CALL DLACPY( 'Upper', M1, M1, DWORK( IB22 ), M, B( I1, I1 ),
     $                   LDB )
            CALL DLACPY( 'Full', M1, M2, DWORK( IB11+M*M1 ), M,
     $                   B( 1, I2 ), LDB )
            CALL DLASET( 'Full', M1, M2, ZERO, ZERO, B( I1, I2 ), LDB )
            CALL DLACPY( 'Upper', M2, M2, DWORK( IB11+M*M1+M1 ), M,
     $                   B( I2, I2 ), LDB )
            CALL DLASET( 'Full', M1, M2, ZERO, ZERO, B( 1, I3 ), LDB )
            CALL DLACPY( 'Full', M1, M2, DWORK( IB22+M*M1 ), M,
     $                   B( I1, I3 ), LDB )
            CALL DLASET( 'Full', M2, M2, ZERO, ZERO, B( I2, I3 ), LDB )
            CALL DLACPY( 'Upper', M2, M2, DWORK( IB22+M*M1+M1 ), M,
     $                   B( I3, I3 ), LDB )
C
            CALL DLASET( 'Full', M1, M1, ZERO, ZERO, D, LDD )
            CALL DLACPY( 'Upper', M1, M1, DWORK( ID21 ), M, D( I1, 1 ),
     $                   LDD )
            CALL DCOPY(  M1-1, DWORK( ID21+1 ), MP1, D( I1+1, 1 ),
     $                   LDD+1 )
            IF( M1.GT.2 )
     $         CALL DLASET( 'Lower', M1-2, M1-2, ZERO, ZERO,
     $                      D( I1+2, 1 ), LDD )
            CALL DLASET( 'Full', M4, M1, ZERO, ZERO, D( I2, 1 ), LDD )
            CALL DLACPY( 'Upper', M1, M1, DWORK( ID12 ), M, D( 1, I1 ),
     $                   LDD )
            IF( M1.GT.1 )
     $         CALL DLASET( 'Lower', M1-1, M1-1, ZERO, ZERO, D( 2, I1 ),
     $                      LDD )
            CALL DLASET( 'Full', N-M1, M1, ZERO, ZERO, D( I1, I1 ),
     $                   LDD )
            CALL DLASET( 'Full', M1, M2, ZERO, ZERO, D( 1, I2 ), LDD )
            CALL DLACPY( 'Full', M1, M2, DWORK( ID21+M*M1 ), M,
     $                   D( I1, I2 ), LDD )
            CALL DLASET( 'Full', M2, M2, ZERO, ZERO, D( I2, I2 ), LDD )
            CALL DLACPY( 'Upper', M2, M2, DWORK( ID21+M*M1+M1 ), M,
     $                   D( I3, I2 ), LDD )
            IF( I3.LT.N )
     $         CALL DCOPY( M2-1, DWORK( ID21+M*M1+I1 ), MP1,
     $                     D( I3+1, I2 ), LDD+1 )
            IF( M2.GT.2 )
     $         CALL DLASET( 'Lower', M2-2, M2-2, ZERO, ZERO,
     $                      D( I3+2, I2 ), LDD )
            CALL DLACPY( 'Full', M1, M2, DWORK( ID12+M*M1 ), M,
     $                   D( 1, I3 ), LDD )
            CALL DLASET( 'Full', M1, M2, ZERO, ZERO, D( I1, I3 ), LDD )
            CALL DLACPY( 'Full', M2, M2, DWORK( ID12+M*M1+M1 ), M,
     $                   D( I2, I3 ), LDD )
            CALL DLASET( 'Full', M2, M2, ZERO, ZERO, D( I3, I3 ), LDD )
         ELSE
            CALL DLASET( 'Full', M, M, ZERO, ZERO, A( MP1, 1 ), LDA )
            CALL DLASET( 'Full', M, M, ZERO, ZERO, A( 1, MP1 ), LDA )
            CALL DLASET( 'Full', M, M, ZERO, ZERO, B( MP1, 1 ), LDB )
            CALL DLASET( 'Full', M, M, ZERO, ZERO, B( 1, MP1 ), LDB )
            CALL DLASET( 'Full', M, M, ZERO, ZERO, D, LDD )
            CALL DLASET( 'Full', M, M, ZERO, ZERO, D( MP1, MP1 ), LDD )
         END IF
C
         IF( LINIQ1 ) THEN
            CALL DLACPY( 'Full', M, M1, DWORK( IV2 ), M, Q1, LDQ1 )
            CALL DLASET( 'Full', M, M1, ZERO, ZERO, Q1( MP1, 1 ), LDQ1 )
            CALL DLASET( 'Full', M, M1, ZERO, ZERO, Q1( 1, I1 ),  LDQ1 )
            CALL DLACPY( 'Full', M, M1, DWORK( IV5 ), M, Q1( MP1, I1 ),
     $                   LDQ1 )
            IF( M2.GT.0 ) THEN
               CALL DLACPY( 'Full', M, M2, DWORK( IV2+M*M1 ), M,
     $                      Q1( 1, I2 ), LDQ1 )
               CALL DLASET( 'Full', M, M2, ZERO, ZERO, Q1( MP1, I2 ),
     $                      LDQ1 )
               CALL DLASET( 'Full', M, M2, ZERO, ZERO, Q1( 1, I3 ),
     $                      LDQ1 )
               CALL DLACPY( 'Full', M, M2, DWORK( IV5+M*M1 ), M,
     $                      Q1( MP1, I3 ), LDQ1 )
            END IF
         END IF
C
         IF( LINIQ2 ) THEN
            CALL DLACPY( 'Full', M, M1, DWORK( IV1 ), M, Q2, LDQ2 )
            CALL DLASET( 'Full', M, M1, ZERO, ZERO, Q2( MP1, 1 ), LDQ2 )
            CALL DLASET( 'Full', M, M1, ZERO, ZERO, Q2( 1, I1 ),  LDQ2 )
            CALL DLACPY( 'Full', M, M1, DWORK( IV4 ), M, Q2( MP1, I1 ),
     $                   LDQ2 )
            IF( M2.GT.0 ) THEN
               CALL DLACPY( 'Full', M, M2, DWORK( IV1+M*M1 ), M,
     $                      Q2( 1, I2 ), LDQ2 )
               CALL DLASET( 'Full', M, M2, ZERO, ZERO, Q2( MP1, I2 ),
     $                      LDQ2 )
               CALL DLASET( 'Full', M, M2, ZERO, ZERO, Q2( 1, I3 ),
     $                      LDQ2 )
               CALL DLACPY( 'Full', M, M2, DWORK( IV4+M*M1 ), M,
     $                      Q2( MP1, I3 ), LDQ2 )
            END IF
         END IF
C
         IF( LINIQ3 ) THEN
            CALL DLACPY( 'Full', M, M1, DWORK( IV6 ), M, Q3, LDQ3 )
            CALL DLASET( 'Full', M, M1, ZERO, ZERO, Q3( MP1, 1 ), LDQ3 )
            CALL DLASET( 'Full', M, M1, ZERO, ZERO, Q3( 1, I1 ),  LDQ3 )
            CALL DLACPY( 'Full', M, M1, DWORK( IV3 ), M, Q3( MP1, I1 ),
     $                   LDQ3 )
            IF( M2.GT.0 ) THEN
               CALL DLACPY( 'Full', M, M2, DWORK( IV6+M*M1 ), M,
     $                      Q3( 1, I2 ), LDQ3 )
               CALL DLASET( 'Full', M, M2, ZERO, ZERO, Q3( MP1, I2 ),
     $                      LDQ3 )
               CALL DLASET( 'Full', M, M2, ZERO, ZERO, Q3( 1, I3 ),
     $                      LDQ3 )
               CALL DLACPY( 'Full', M, M2, DWORK( IV3+M*M1 ), M,
     $                      Q3( MP1, I3 ), LDQ3 )
            END IF
         END IF
C
      ELSE
         M1 = M
         M2 = 0
         I1 = M1 + 1
         I2 = I1 + M1
         I3 = I2
         M4 = 2*M2
         CALL DLASET( 'Full', M, M, ZERO, ZERO, A( MP1, 1 ), LDA )
         CALL DLASET( 'Full', M, M, ZERO, ZERO, A( 1, MP1 ), LDA )
         CALL DLASET( 'Full', M, M, ZERO, ZERO, B( MP1, 1 ), LDB )
         CALL DLASET( 'Full', M, M, ZERO, ZERO, B( 1, MP1 ), LDB )
         CALL DLASET( 'Full', M, M, ZERO, ZERO, D, LDD )
         CALL DLASET( 'Full', M, M, ZERO, ZERO, D( MP1, MP1 ), LDD )
         IF( LINIQ1 )
     $      CALL DLASET( 'Full', N, N, ZERO, ONE, Q1, LDQ1 )
         IF( LINIQ2 )
     $      CALL DLASET( 'Full', N, N, ZERO, ONE, Q2, LDQ2 )
         IF( LINIQ3 )
     $      CALL DLASET( 'Full', N, N, ZERO, ONE, Q3, LDQ3 )
      END IF
C
C     Count the number of blocks in DD31.
C
      R = 0
      J = 1
C     WHILE( J.LE.M1 ) DO
   40 CONTINUE
      IF( J.LT.M1 ) THEN
         R = R + 1
         IWORK( R ) = J
         IF( D( M1+J+1, J ).EQ.ZERO ) THEN
            J = J + 1
         ELSE
            J = J + 2
         END IF
         GO TO 40
      ELSE IF ( J.EQ.M1 ) THEN
         R = R + 1
         IWORK( R ) = J
         J = J + 1
      END IF
C     END WHILE 40
      IWORK( R+1 ) = J
C
C     Triangularize the upper left subpencil aAA1 BB1 - bDD1.
C
      DO 60 K = 1, R
C
C        Calculate position of submatrices in DWORK.
C        IB1 and IB2 are pointers to 2 consecutive blocks.
C
         IB1  = IWORK( K )
         IB2  = IWORK( K+1 )
         DIM1 = IB2 - IB1
         SDIM = 2*DIM1
C
         IAUPLE = 1
         IALOLE = IAUPLE + DIM1
         IAUPRI = DIM1*SDIM + 1
         IALORI = IAUPRI + DIM1
         IBUPLE = SDIM*SDIM + 1
         IBLOLE = IBUPLE + DIM1
         IBUPRI = 3*DIM1*SDIM + 1
         IBLORI = IBUPRI + DIM1
         IDUPLE = 2*SDIM*SDIM + 1
         IDLOLE = IDUPLE + DIM1
         IDUPRI = 5*DIM1*SDIM + 1
         IDLORI = IDUPRI + DIM1
         I1UPLE = 3*SDIM*SDIM + 1
         I1LOLE = I1UPLE + DIM1
         I1UPRI = 7*DIM1*SDIM + 1
         I1LORI = I1UPRI + DIM1
         I2UPLE = 4*SDIM*SDIM + 1
         I2LOLE = I2UPLE + DIM1
         I2UPRI = 9*DIM1*SDIM + 1
         I2LORI = I2UPRI + DIM1
         I3UPLE = 5*SDIM*SDIM + 1
         I3LOLE = I3UPLE + DIM1
         I3UPRI = 11*DIM1*SDIM + 1
         I3LORI = I3UPRI + DIM1
C
C        Generate input matrices for MB03ED built of submatrices of A, B
C        and D.
C        Workspace:   need    48.
C
         IF( DIM1.EQ.1 ) THEN
            CALL DCOPY( SDIM, A( IB1, IB1 ), ( LDA+1 )*M1,
     $                  DWORK( IAUPLE ), SDIM+1 )
            CALL DCOPY( SDIM, B( IB1, IB1 ), ( LDB+1 )*M1,
     $                  DWORK( IBUPLE ), SDIM+1 )
            CALL DCOPY( SDIM, D( M1+IB1, IB1 ), ( LDD-1 )*M1,
     $                  DWORK( IDLOLE ), 1 )
         ELSE
            CALL DLACPY( 'Full', DIM1, DIM1, A( IB1, IB1 ), LDA,
     $                   DWORK( IAUPLE ), SDIM )
            CALL DLASET( 'Full', DIM1, DIM1, ZERO, ZERO,
     $                   DWORK( IALOLE ), SDIM )
            CALL DLASET( 'Full', DIM1, DIM1, ZERO, ZERO,
     $                   DWORK( IAUPRI ), SDIM )
            CALL DLACPY( 'Full', DIM1, DIM1, A( M1+IB1, M1+IB1 ), LDA,
     $                   DWORK( IALORI ), SDIM )
C
            CALL DLACPY( 'Full', DIM1, DIM1, B( IB1, IB1 ), LDB,
     $                   DWORK( IBUPLE ), SDIM )
            CALL DLASET( 'Full', DIM1, DIM1, ZERO, ZERO,
     $                   DWORK( IBLOLE ), SDIM )
            CALL DLASET( 'Full', DIM1, DIM1, ZERO, ZERO,
     $                   DWORK( IBUPRI ), SDIM )
            CALL DLACPY( 'Full', DIM1, DIM1, B( M1+IB1, M1+IB1 ), LDB,
     $                   DWORK( IBLORI ), SDIM )
C
            CALL DLASET( 'Full', DIM1, DIM1, ZERO, ZERO,
     $                   DWORK( IDUPLE ), SDIM )
            CALL DLACPY( 'Full', DIM1, DIM1, D( M1+IB1, IB1 ), LDD,
     $                   DWORK( IDLOLE ), SDIM )
            CALL DLACPY( 'Full', DIM1, DIM1, D( IB1, M1+IB1 ), LDD,
     $                   DWORK( IDUPRI ), SDIM )
            CALL DLASET( 'Full', DIM1, DIM1, ZERO, ZERO,
     $                   DWORK( IDLORI ), SDIM )
         END IF
C
C        Perform eigenvalue exchange.
C        Workspace:   need   96 + max( 79, 4*N ).
C
         IWRK  = 6*SDIM*SDIM + 1
         ITMP  = IWRK  + DIM1*M
         ITMP2 = ITMP  + DIM1*M
         ITMP3 = ITMP2 + DIM1*DIM1
C
         CALL MB03ED( SDIM, ULP, DWORK( IAUPLE ), SDIM, DWORK( IBUPLE ),
     $                SDIM, DWORK( IDUPLE ), SDIM, DWORK( I1UPLE ),
     $                SDIM, DWORK( I2UPLE ), SDIM, DWORK( I3UPLE ),
     $                SDIM, DWORK( IWRK ), LDWORK-IWRK+1, INFO )
         IF( INFO.GT.0 ) THEN
            INFO = 3
            RETURN
         END IF
C
         NR = IB2 - 1
C
         IF( DIM1.EQ.2 ) THEN
C
C           Update A.
C
            CALL DLACPY( 'Full', NR, DIM1, A( 1, IB1 ), LDA,
     $                   DWORK( IWRK ), NR )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1, DIM1,
     $                   ONE, DWORK( IWRK ), NR, DWORK( I2UPLE ), SDIM,
     $                   ZERO, A( 1, IB1 ), LDA )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1, DIM1,
     $                   ONE, A( 1, M1+IB1 ), LDA, DWORK( I2LOLE ),
     $                   SDIM, ONE, A( 1, IB1 ), LDA )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1, DIM1,
     $                   ONE, DWORK( IWRK ), NR, DWORK( I2UPRI ), SDIM,
     $                   ZERO, DWORK( ITMP ), NR )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1, DIM1,
     $                   ONE, A( 1, M1+IB1 ), LDA, DWORK( I2LORI ),
     $                   SDIM, ONE, DWORK( ITMP ), NR )
            CALL DLACPY( 'Full', NR, DIM1, DWORK( ITMP ), NR,
     $                   A( 1, M1+IB1 ), LDA )
C
            CALL DLACPY( 'Full', NR, DIM1, A( I1, IB1 ), LDA,
     $                   DWORK( IWRK ), NR )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1, DIM1,
     $                   ONE, DWORK( IWRK ), NR, DWORK( I2UPLE ), SDIM,
     $                   ZERO, A( I1, IB1 ), LDA )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1, DIM1,
     $                   ONE, A( I1, M1+IB1 ), LDA, DWORK( I2LOLE ),
     $                   SDIM, ONE, A( I1, IB1 ), LDA )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1, DIM1,
     $                   ONE, DWORK( IWRK ), NR, DWORK( I2UPRI ), SDIM,
     $                   ZERO, DWORK( ITMP ), NR )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1, DIM1,
     $                   ONE, A( I1, M1+IB1 ), LDA, DWORK( I2LORI ),
     $                   SDIM, ONE, DWORK( ITMP ), NR )
            CALL DLACPY( 'Full', NR, DIM1, DWORK( ITMP ), NR,
     $                   A( I1, M1+IB1 ), LDA )
C
            CALL DLACPY( 'Full', DIM1, DIM1, A( M1+IB1, IB1 ), LDA,
     $                   DWORK( ITMP2 ), DIM1 )
            CALL DLACPY( 'Full', DIM1, DIM1, A( IB1, M1+IB1 ), LDA,
     $                   DWORK( ITMP3 ), DIM1 )
            CALL DLASET( 'Full', DIM1, DIM1, ZERO, ZERO,
     $                   A( M1+IB1, IB1 ), LDA )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB2+1,
     $                   DIM1, ONE, DWORK( I3UPRI ), SDIM,
     $                   A( IB1, IB2 ), LDA, ZERO, A( M1+IB1, IB2 ),
     $                   LDA )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                   DIM1, ONE, DWORK( I3UPLE ), SDIM,
     $                   A( IB1, IB1 ), LDA, ZERO, DWORK( ITMP ), DIM1 )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                   ONE, DWORK( I3LOLE ), SDIM, DWORK( ITMP2 ),
     $                   DIM1, ONE, DWORK( ITMP ), DIM1 )
            CALL DLACPY( 'Full', DIM1, M1-IB1+1, DWORK( ITMP ), DIM1,
     $                   A( IB1, IB1 ), LDA )
C     
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                   DIM1, ONE, DWORK( I3LOLE ), SDIM,
     $                   A( M1+IB1, M1+IB1 ), LDA, ZERO,
     $                   A( IB1, M1+IB1 ), LDA )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                   ONE, DWORK( I3UPLE ), SDIM, DWORK( ITMP3 ),
     $                   DIM1, ONE, A( IB1, M1+IB1 ), LDA )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                   DIM1, ONE, DWORK( I3LORI ), SDIM,
     $                   A( M1+IB1, M1+IB1 ), LDA, ZERO, DWORK( ITMP ),
     $                   DIM1 )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                   ONE, DWORK( I3UPRI ), SDIM, DWORK( ITMP3 ),
     $                   DIM1, ONE, DWORK( ITMP ), DIM1 )
            CALL DLACPY( 'Full', DIM1, M1-IB1+1, DWORK( ITMP ), DIM1,
     $                   A( M1+IB1, M1+IB1 ), LDA )
C
            IF( M2.GT.0 ) THEN
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M2, DIM1,
     $                      ONE, DWORK( I3UPRI ), SDIM, A( IB1, I2 ),
     $                      LDA, ZERO, A( M1+IB1, I2), LDA )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M2, DIM1,
     $                      ONE, DWORK( I3UPLE ), SDIM, A( IB1, I2 ),
     $                      LDA, ZERO, DWORK( ITMP ), DIM1 )
               CALL DLACPY( 'Full', DIM1, M2, DWORK( ITMP ), DIM1,
     $                      A( IB1, I2 ), LDA )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M2, DIM1,
     $                      ONE, DWORK( I3LOLE ), SDIM, A( M1+IB1, I3 ),
     $                      LDA, ZERO, A( IB1, I3 ), LDA )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M2, DIM1,
     $                      ONE, DWORK( I3LORI ), SDIM, A( M1+IB1, I3 ),
     $                      LDA, ZERO, DWORK( ITMP ), DIM1 )
               CALL DLACPY( 'Full', DIM1, M2, DWORK( ITMP ), DIM1,
     $                      A( M1+IB1, I3 ), LDA )
            END IF
C
C           Update B.
C
            CALL DLACPY( 'Full', NR, DIM1, B( 1, IB1 ), LDB,
     $                   DWORK( IWRK ), NR )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1, DIM1,
     $                   ONE, DWORK( IWRK ), NR, DWORK( I1UPLE ), SDIM,
     $                   ZERO, B( 1, IB1 ), LDB )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1, DIM1,
     $                   ONE, B( 1, M1+IB1 ), LDB, DWORK( I1LOLE ),
     $                   SDIM, ONE, B( 1, IB1 ), LDB )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1, DIM1,
     $                   ONE, DWORK( IWRK ), NR, DWORK( I1UPRI ), SDIM,
     $                   ZERO, DWORK( ITMP ), NR )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1, DIM1,
     $                   ONE, B( 1, M1+IB1 ), LDB, DWORK( I1LORI ),
     $                   SDIM, ONE, DWORK( ITMP ), NR )
            CALL DLACPY( 'Full', NR, DIM1, DWORK( ITMP ), NR,
     $                   B( 1, M1+IB1 ), LDB )
C
            CALL DLACPY( 'Full', NR, DIM1, B( I1, IB1 ), LDB,
     $                   DWORK( IWRK ), NR )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1, DIM1,
     $                   ONE, DWORK( IWRK ), NR, DWORK( I1UPLE ), SDIM,
     $                   ZERO, B( I1, IB1 ), LDB )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1, DIM1,
     $                   ONE, B( I1, M1+IB1 ), LDB, DWORK( I1LOLE ),
     $                   SDIM, ONE, B( I1, IB1 ), LDB )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1, DIM1,
     $                   ONE, DWORK( IWRK ), NR, DWORK( I1UPRI ), SDIM,
     $                   ZERO, DWORK( ITMP ), NR )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1, DIM1,
     $                   ONE, B( I1, M1+IB1 ), LDB, DWORK( I1LORI ),
     $                   SDIM, ONE, DWORK( ITMP ), NR )
            CALL DLACPY( 'Full', NR, DIM1, DWORK( ITMP ), NR,
     $                   B( I1, M1+IB1 ), LDB )
C
            CALL DLACPY( 'Full', DIM1, DIM1, B( M1+IB1, IB1 ), LDB,
     $                   DWORK( ITMP2 ), DIM1 )
            CALL DLACPY( 'Full', DIM1, DIM1, B( IB1, M1+IB1 ), LDB,
     $                   DWORK( ITMP3 ), DIM1 )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB2+1,
     $                   DIM1, ONE, DWORK( I2UPRI ), SDIM,
     $                   B( IB1, IB2 ), LDB, ZERO, B( M1+IB1, IB2 ),
     $                   LDB )
            CALL DLASET( 'Full', DIM1, DIM1, ZERO, ZERO,
     $                   B( M1+IB1, IB1 ), LDB )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                   DIM1, ONE, DWORK( I2UPLE ), SDIM,
     $                   B( IB1, IB1 ), LDB, ZERO, DWORK( ITMP ), DIM1 )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                   ONE, DWORK( I2LOLE ), SDIM, DWORK( ITMP2 ),
     $                   DIM1, ONE, DWORK( ITMP ), DIM1 )
            CALL DLACPY( 'Full', DIM1, M1-IB1+1, DWORK( ITMP ), DIM1,
     $                   B( IB1, IB1 ), LDB )
C     
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                   DIM1, ONE, DWORK( I2LOLE ), SDIM,
     $                   B( M1+IB1, M1+IB1 ), LDB, ZERO,
     $                   B( IB1, M1+IB1 ), LDB )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                   ONE, DWORK( I2UPLE ), SDIM, DWORK( ITMP3 ),
     $                   DIM1, ONE, B( IB1, M1+IB1 ), LDB )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                   DIM1, ONE, DWORK( I2LORI ), SDIM,
     $                   B( M1+IB1, M1+IB1 ), LDB, ZERO, DWORK( ITMP ),
     $                   DIM1 )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                   ONE, DWORK( I2UPRI ), SDIM, DWORK( ITMP3 ),
     $                   DIM1, ONE, DWORK( ITMP ), DIM1 )
            CALL DLACPY( 'Full', DIM1, M1-IB1+1, DWORK( ITMP ), DIM1,
     $                   B( M1+IB1, M1+IB1 ), LDB )
C
            IF( M2.GT.0 ) THEN
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M2, DIM1,
     $                      ONE, DWORK( I2UPRI ), SDIM, B( IB1, I2 ),
     $                      LDB, ZERO, B( M1+IB1, I2), LDB )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M2, DIM1,
     $                      ONE, DWORK( I2UPLE ), SDIM, B( IB1, I2 ),
     $                      LDB, ZERO, DWORK( ITMP ), DIM1 )
               CALL DLACPY( 'Full', DIM1, M2, DWORK( ITMP ), DIM1,
     $                      B( IB1, I2 ), LDB )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M2, DIM1,
     $                      ONE, DWORK( I2LOLE ), SDIM, B( M1+IB1, I3 ),
     $                      LDB, ZERO, B( IB1, I3 ), LDB )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M2, DIM1,
     $                      ONE, DWORK( I2LORI ), SDIM, B( M1+IB1, I3 ),
     $                      LDB, ZERO, DWORK( ITMP ), DIM1 )
               CALL DLACPY( 'Full', DIM1, M2, DWORK( ITMP ), DIM1,
     $                      B( M1+IB1, I3 ), LDB )
            END IF
C
C           Update D.
C
            CALL DLACPY( 'Full', NR, DIM1, D( 1, IB1 ), LDD,
     $                   DWORK( IWRK ), NR )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1, DIM1,
     $                   ONE, DWORK( IWRK ), NR, DWORK( I1UPLE ), SDIM,
     $                   ZERO, D( 1, IB1 ), LDD )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1, DIM1,
     $                   ONE, D( 1, M1+IB1 ), LDD, DWORK( I1LOLE ),
     $                   SDIM, ONE, D( 1, IB1 ), LDD )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1, DIM1,
     $                   ONE, DWORK( IWRK ), NR, DWORK( I1UPRI ), SDIM,
     $                   ZERO, DWORK( ITMP ), NR ) 
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1, DIM1,
     $                   ONE, D( 1, M1+IB1 ), LDD, DWORK( I1LORI ),
     $                   SDIM, ONE, DWORK( ITMP ), NR )
            CALL DLACPY( 'Full', NR, DIM1, DWORK( ITMP ), NR,
     $                   D( 1, M1+IB1 ), LDD )
C
            CALL DLACPY( 'Full', NR, DIM1, D( I1, IB1 ), LDD,
     $                   DWORK( IWRK ), NR )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1, DIM1,
     $                   ONE, DWORK( IWRK ), NR, DWORK( I1UPLE ), SDIM,
     $                   ZERO, D( I1, IB1 ), LDD )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1, DIM1,
     $                   ONE, D( I1, M1+IB1 ), LDD, DWORK( I1LOLE ),
     $                   SDIM, ONE, D( I1, IB1 ), LDD )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1, DIM1,
     $                   ONE, DWORK( IWRK ), NR, DWORK( I1UPRI ), SDIM,
     $                   ZERO, DWORK( ITMP ), NR ) 
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1, DIM1,
     $                   ONE, D( I1, M1+IB1 ), LDD, DWORK( I1LORI ),
     $                   SDIM, ONE, DWORK( ITMP ), NR )
            CALL DLACPY( 'Full', NR, DIM1, DWORK( ITMP ), NR,
     $                   D( I1, M1+IB1 ), LDD )
C
            CALL DLACPY( 'Full', DIM1, DIM1, D( IB1, IB1 ), LDD,
     $                   DWORK( ITMP2 ), DIM1 )
            CALL DLACPY( 'Full', DIM1, DIM1, D( M1+IB1, M1+IB1 ), LDD,
     $                   DWORK( ITMP3 ), DIM1 )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                   DIM1, ONE, DWORK( I3LOLE ), SDIM,
     $                   D( M1+IB1, IB1 ), LDD, ZERO, D( IB1, IB1 ),
     $                   LDD )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                   ONE, DWORK( I3UPLE ), SDIM, DWORK( ITMP2 ),
     $                   DIM1, ONE, D( IB1, IB1 ), LDD )
            CALL DLASET( 'Full', DIM1, DIM1, ZERO, ZERO,
     $                   D( M1+IB1, IB1 ), LDD )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                   DIM1, ONE, DWORK( I3LORI ), SDIM,
     $                   D( M1+IB1, IB1+1 ), LDD, ZERO, DWORK( ITMP ),
     $                   DIM1 )
            CALL DLACPY( 'Full', DIM1, M1-IB1+1, DWORK( ITMP ), DIM1,
     $                   D( M1+IB1, IB1+1 ), LDD )
C
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                   DIM1, ONE, DWORK( I3UPRI ), SDIM,
     $                   D( IB1, M1+IB1 ), LDD, ZERO,
     $                   D( M1+IB1, M1+IB1 ), LDD )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                   ONE, DWORK( I3LORI ), SDIM, DWORK( ITMP3 ),
     $                   DIM1, ONE, D( M1+IB1, M1+IB1 ), LDD )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                   DIM1, ONE, DWORK( I3UPLE ), SDIM,
     $                   D( IB1, M1+IB1 ), LDD, ZERO, DWORK( ITMP ),
     $                   DIM1 )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                   ONE, DWORK( I3LOLE ), SDIM, DWORK( ITMP3 ),
     $                   DIM1, ONE, DWORK( ITMP ), DIM1 )
            CALL DLACPY( 'Full', DIM1, M1-IB1+1, DWORK( ITMP ), DIM1,
     $                   D( IB1, M1+IB1 ), LDD )
C
            IF( M2.GT.0 ) THEN
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M2, DIM1,
     $                      ONE, DWORK( I3LOLE ), SDIM, D( M1+IB1, I2 ),
     $                      LDD, ZERO, D( IB1, I2), LDD )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M2, DIM1,
     $                      ONE, DWORK( I3LORI ), SDIM, D( M1+IB1, I2 ),
     $                      LDD, ZERO, DWORK( ITMP ), DIM1 )
               CALL DLACPY( 'Full', DIM1, M2, DWORK( ITMP ), DIM1,
     $                      D( M1+IB1, I2 ), LDD )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M2, DIM1,
     $                      ONE, DWORK( I3UPRI ), SDIM, D( IB1, I3 ),
     $                      LDD, ZERO, D( M1+IB1, I3), LDD )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M2, DIM1,
     $                      ONE, DWORK( I3UPLE ), SDIM, D( IB1, I3 ),
     $                      LDD, ZERO, DWORK( ITMP ), DIM1 )
               CALL DLACPY( 'Full', DIM1, M2, DWORK( ITMP ), DIM1,
     $                      D( IB1, I3 ), LDD )
            END IF
C
            ITMP = IWRK + N*DIM1
C
            IF( LCMPQ1 ) THEN
C
C              Update Q1.
C
               CALL DLACPY( 'Full', N, DIM1, Q1( 1, IB1 ), LDQ1,
     $                      DWORK( IWRK ), N )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), N,
     $                      DWORK( I1UPLE ), SDIM, ZERO, Q1( 1, IB1 ),
     $                      LDQ1 )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, Q1( 1, M1+IB1 ), LDQ1,
     $                      DWORK( I1LOLE ), SDIM, ONE, Q1( 1, IB1 ),
     $                      LDQ1 )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), N,
     $                      DWORK( I1UPRI ), SDIM, ZERO, DWORK( ITMP ),
     $                      N )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, Q1( 1, M1+IB1 ), LDQ1,
     $                      DWORK( I1LORI ), SDIM, ONE, DWORK( ITMP ),
     $                      N )
               CALL DLACPY( 'Full', N, DIM1, DWORK( ITMP ), N,
     $                      Q1( 1, M1+IB1 ), LDQ1 )
            END IF
C
            IF( LCMPQ2 ) THEN
C
C              Update Q2.
C
               CALL DLACPY( 'Full', N, DIM1, Q2( 1, IB1 ), LDQ2,
     $                      DWORK( IWRK ), N )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), N,
     $                      DWORK( I2UPLE ), SDIM, ZERO, Q2( 1, IB1 ),
     $                      LDQ2 )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, Q2( 1, M1+IB1 ), LDQ2,
     $                      DWORK( I2LOLE ), SDIM, ONE, Q2( 1, IB1 ),
     $                      LDQ2 )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), N,
     $                      DWORK( I2UPRI ), SDIM, ZERO, DWORK( ITMP ),
     $                      N )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, Q2( 1, M1+IB1 ), LDQ2,
     $                      DWORK( I2LORI ), SDIM, ONE, DWORK( ITMP ),
     $                      N )
               CALL DLACPY( 'Full', N, DIM1, DWORK( ITMP ), N,
     $                      Q2( 1, M1+IB1 ), LDQ2 )
            END IF
C
            IF( LCMPQ3 ) THEN
C
C              Update Q3.
C
               CALL DLACPY( 'Full', N, DIM1, Q3( 1, IB1 ), LDQ3,
     $                      DWORK( IWRK ), N )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), N,
     $                      DWORK( I3UPLE ), SDIM, ZERO, Q3( 1, IB1 ),
     $                      LDQ3 )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, Q3( 1, M1+IB1 ), LDQ3,
     $                      DWORK( I3LOLE ), SDIM, ONE, Q3( 1, IB1 ),
     $                      LDQ3 )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), N,
     $                      DWORK( I3UPRI ), SDIM, ZERO, DWORK( ITMP ),
     $                      N )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, Q3( 1, M1+IB1 ), LDQ3,
     $                      DWORK( I3LORI ), SDIM, ONE, DWORK( ITMP ),
     $                      N )
               CALL DLACPY( 'Full', N, DIM1, DWORK( ITMP ), N,
     $                      Q3( 1, M1+IB1 ), LDQ3 )
            END IF
C
         ELSE
C
C           Update A.
C
            CALL DCOPY( NR, A( 1, IB1 ), 1, DWORK( IWRK ), 1 )
            CALL DSCAL( NR, DWORK( I2UPLE ), A( 1, IB1 ), 1 )
            CALL DAXPY( NR, DWORK( I2LOLE ), A( 1, M1+IB1 ), 1,
     $                  A( 1, IB1 ), 1 )
            CALL DSCAL( NR, DWORK( I2LORI ), A( 1, M1+IB1 ), 1 )
            CALL DAXPY( NR, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                  A( 1, M1+IB1 ), 1 )
C
            CALL DCOPY( NR, A( I1, IB1 ), 1, DWORK( IWRK ), 1 )
            CALL DSCAL( NR, DWORK( I2UPLE ), A( I1, IB1 ), 1 )
            CALL DAXPY( NR, DWORK( I2LOLE ), A( I1, M1+IB1 ), 1,
     $                  A( I1, IB1 ), 1 )
            CALL DSCAL( NR, DWORK( I2LORI ), A( I1, M1+IB1 ), 1 )
            CALL DAXPY( NR, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                  A( I1, M1+IB1 ), 1 )
C
            TMP2 = A( M1+IB1, IB1 )
            TMP3 = A( IB1, M1+IB1 )
            IF( M1.GT.IB1 ) THEN
               CALL DCOPY( M1-IB1, A( IB1, IB1+1 ), LDA,
     $                     A( M1+IB1, IB1+1 ), LDA )
               CALL DSCAL( M1-IB1, DWORK( I3UPRI ), A( M1+IB1, IB1+1 ),
     $                     LDA )
            END IF
            A( M1+IB1, IB1 ) = ZERO
            CALL DSCAL( M1-IB1+1, DWORK( I3UPLE ), A( IB1, IB1 ), LDA )
            A( IB1, IB1 ) = A( IB1, IB1 ) + DWORK( I3LOLE )*TMP2
C
            CALL DCOPY( M1-IB1+1, A( M1+IB1, M1+IB1 ), LDA,
     $                  A( IB1, M1+IB1 ), LDA )
            CALL DSCAL( M1-IB1+1, DWORK( I3LOLE ), A( IB1, M1+IB1 ),
     $                  LDA )
            A( IB1, M1+IB1 ) = A( IB1, M1+IB1 ) + DWORK( I3UPLE )*TMP3
            CALL DSCAL( M1-IB1+1, DWORK( I3LORI ), A( M1+IB1, M1+IB1 ),
     $                  LDA )
            A( M1+IB1, M1+IB1 ) = A( M1+IB1, M1+IB1 ) +
     $                            DWORK( I3UPRI )*TMP3
C
            IF( M2.GT.0 ) THEN
               CALL DCOPY( M2, A( IB1, I2 ), LDA, A( M1+IB1, I2 ), LDA )
               CALL DSCAL( M2, DWORK( I3UPRI ),   A( M1+IB1, I2 ), LDA )
               CALL DSCAL( M2, DWORK( I3UPLE ), A( IB1, I2 ), LDA )
               CALL DCOPY( M2, A( M1+IB1, I3 ), LDA, A( IB1, I3 ), LDA )
               CALL DSCAL( M2, DWORK( I3LOLE ), A( IB1, I3 ), LDA )
               CALL DSCAL( M2, DWORK( I3LORI ), A( M1+IB1, I3 ), LDA )
            END IF
C
C           Update B.
C
            CALL DCOPY( NR, B( 1, IB1 ), 1, DWORK( IWRK ), 1 )
            CALL DSCAL( NR, DWORK( I1UPLE ), B( 1, IB1 ), 1 )
            CALL DAXPY( NR, DWORK( I1LOLE ), B( 1, M1+IB1 ), 1,
     $                  B( 1, IB1 ), 1 )
            CALL DSCAL( NR, DWORK( I1LORI ), B( 1, M1+IB1 ), 1 )
            CALL DAXPY( NR, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                  B( 1, M1+IB1 ), 1 )
C
            CALL DCOPY( NR, B( I1, IB1 ), 1, DWORK( IWRK ), 1 )
            CALL DSCAL( NR, DWORK( I1UPLE ), B( I1, IB1 ), 1 )
            CALL DAXPY( NR, DWORK( I1LOLE ), B( I1, M1+IB1 ), 1,
     $                  B( I1, IB1 ), 1 )
            CALL DSCAL( NR, DWORK( I1LORI ), B( I1, M1+IB1 ), 1 )
            CALL DAXPY( NR, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                  B( I1, M1+IB1 ), 1 )
C
            TMP2 = B( M1+IB1, IB1 )
            TMP3 = B( IB1, M1+IB1 )
            IF( M1.GT.IB1 ) THEN
               CALL DCOPY( M1-IB1, B( IB1, IB1+1 ), LDB,
     $                     B( M1+IB1, IB1+1 ), LDB )
               CALL DSCAL( M1-IB1, DWORK( I2UPRI ), B( M1+IB1, IB1+1 ),
     $                     LDB )
            END IF
            B( M1+IB1, IB1 ) = ZERO
            CALL DSCAL( M1-IB1+1, DWORK( I2UPLE ), B( IB1, IB1 ), LDB )
            B( IB1, IB1 ) = B( IB1, IB1 ) + DWORK( I2LOLE )*TMP2
C
            CALL DCOPY( M1-IB1+1, B( M1+IB1, M1+IB1 ), LDB,
     $                  B( IB1, M1+IB1 ), LDB )
            CALL DSCAL( M1-IB1+1, DWORK( I2LOLE ), B( IB1, M1+IB1 ),
     $                  LDB )
            B( IB1, M1+IB1 ) = B( IB1, M1+IB1 ) + DWORK( I2UPLE )*TMP3
            CALL DSCAL( M1-IB1+1, DWORK( I2LORI ), B( M1+IB1, M1+IB1 ),
     $                  LDB )
            B( M1+IB1, M1+IB1 ) = B( M1+IB1, M1+IB1 ) +
     $                            DWORK( I2UPRI )*TMP3
C
            IF( M2.GT.0 ) THEN
               CALL DCOPY( M2, B( IB1, I2 ), LDB, B( M1+IB1, I2 ), LDB )
               CALL DSCAL( M2, DWORK( I2UPRI ),   B( M1+IB1, I2 ), LDB )
               CALL DSCAL( M2, DWORK( I2UPLE ), B( IB1, I2 ), LDB )
               CALL DCOPY( M2, B( M1+IB1, I3 ), LDB, B( IB1, I3 ), LDB )
               CALL DSCAL( M2, DWORK( I2LOLE ), B( IB1, I3 ), LDB )
               CALL DSCAL( M2, DWORK( I2LORI ), B( M1+IB1, I3 ), LDB )
            END IF
C
C           Update D.
C
            CALL DCOPY( NR, D( 1, IB1 ), 1, DWORK( IWRK ), 1 )
            CALL DSCAL( NR, DWORK( I1UPLE ), D( 1, IB1 ), 1 )
            CALL DAXPY( NR, DWORK( I1LOLE ), D( 1, M1+IB1 ), 1,
     $                  D( 1, IB1 ), 1 )
            CALL DSCAL( NR, DWORK( I1LORI ), D( 1, M1+IB1 ), 1 )
            CALL DAXPY( NR, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                  D( 1, M1+IB1 ), 1 )
C
            CALL DCOPY( NR, D( I1, IB1 ), 1, DWORK( IWRK ), 1 )
            CALL DSCAL( NR, DWORK( I1UPLE ), D( I1, IB1 ), 1 )
            CALL DAXPY( NR, DWORK( I1LOLE ), D( I1, M1+IB1 ), 1,
     $                  D( I1, IB1 ), 1 )
            CALL DSCAL( NR, DWORK( I1LORI ), D( I1, M1+IB1 ), 1 )
            CALL DAXPY( NR, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                  D( I1, M1+IB1 ), 1 )
C
            TMP2 = D( IB1, IB1 )
            TMP3 = D( M1+IB1, M1+IB1 )
            CALL DCOPY( M1-IB1+1, D( M1+IB1, IB1 ), LDD, D( IB1, IB1 ),
     $                  LDD )
            CALL DSCAL( M1-IB1+1, DWORK( I3LOLE ), D( IB1, IB1 ), LDD )
            D( IB1, IB1 ) = D( IB1, IB1 ) + DWORK( I3UPLE )*TMP2
            D( M1+IB1, IB1 ) = ZERO
            CALL DSCAL( M1-IB1+1, DWORK( I3LORI ), D( M1+IB1, IB1+1 ),
     $                  LDD )
C
            CALL DCOPY( M1-IB1+1, D( IB1, M1+IB1 ), LDD,
     $                  D( M1+IB1, M1+IB1 ), LDD )
            CALL DSCAL( M1-IB1+1, DWORK( I3UPRI ), D( M1+IB1, M1+IB1 ),
     $                  LDD )
            D( M1+IB1, M1+IB1 ) = D( M1+IB1, M1+IB1 ) +
     $                            DWORK( I3LORI )*TMP3
            CALL DSCAL( M1-IB1+1, DWORK( I3UPLE ), D( IB1, M1+IB1 ),
     $                  LDD )
            D( IB1, M1+IB1 ) = D( IB1, M1+IB1 ) + DWORK( I3LOLE )*TMP3
C
            IF( M2.GT.0 ) THEN
               CALL DCOPY( M2, D( M1+IB1, I2 ), LDD, D( IB1, I2 ), LDD )
               CALL DSCAL( M2, DWORK( I3LOLE ), D( IB1, I2 ), LDD )
               CALL DSCAL( M2, DWORK( I3LORI ), D( M1+IB1, I2 ),  LDD )
               CALL DCOPY( M2, D( IB1, I3 ), LDD, D( M1+IB1, I3 ), LDD )
               CALL DSCAL( M2, DWORK( I3UPRI ), D( M1+IB1, I3 ), LDD )
               CALL DSCAL( M2, DWORK( I3UPLE ), D( IB1, I3 ),   LDD )
            END IF
C
            ITMP = IWRK + N
C
            IF( LCMPQ1 ) THEN
C
C              Update Q1.
C
               CALL DCOPY( N, Q1( 1, IB1 ), 1, DWORK( IWRK ), 1 )
               CALL DSCAL( N, DWORK( I1UPLE ), Q1( 1, IB1 ),  1 )
               CALL DAXPY( N, DWORK( I1LOLE ), Q1( 1, M1+IB1 ), 1,
     $                     Q1( 1, IB1 ), 1 )
               CALL DSCAL( N, DWORK( I1LORI ), Q1( 1, M1+IB1 ), 1 )
               CALL DAXPY( N, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                     Q1( 1, M1+IB1 ), 1 )
            END IF
C
            IF( LCMPQ2 ) THEN
C
C              Update Q2.
C
               CALL DCOPY( N, Q2( 1, IB1 ), 1, DWORK( IWRK ), 1 )
               CALL DSCAL( N, DWORK( I2UPLE ), Q2( 1, IB1 ),  1 )
               CALL DAXPY( N, DWORK( I2LOLE ), Q2( 1, M1+IB1 ), 1,
     $                     Q2( 1, IB1 ), 1 )
               CALL DSCAL( N, DWORK( I2LORI ), Q2( 1, M1+IB1 ), 1 )
               CALL DAXPY( N, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                     Q2( 1, M1+IB1 ), 1 )
            END IF
C
            IF( LCMPQ3 ) THEN
C
C              Update Q3.
C
               CALL DCOPY( N, Q3( 1, IB1 ), 1, DWORK( IWRK ), 1 )
               CALL DSCAL( N, DWORK( I3UPLE ), Q3( 1, IB1 ),  1 )
               CALL DAXPY( N, DWORK( I3LOLE ), Q3( 1, M1+IB1 ), 1,
     $                     Q3( 1, IB1 ), 1 )
               CALL DSCAL( N, DWORK( I3LORI ), Q3( 1, M1+IB1 ), 1 )
               CALL DAXPY( N, DWORK( I3UPRI ), DWORK( IWRK ), 1,
     $                     Q3( 1, M1+IB1 ), 1 )
            END IF
C
         END IF
C
         DO 50 J = K - 1, 1, -1
C
C           Calculate position of submatrices in DWORK.
C
            IJ1  = IWORK( J )
            IJ2  = IWORK( J+1 )
            DIM1 = IWORK( K+1 ) - IWORK( K )
            DIM2 = IJ2 - IJ1
            SDIM = DIM1 + DIM2
C
            IAUPLE = 1
            IALOLE = IAUPLE + DIM1
            IAUPRI = DIM1*SDIM + 1
            IALORI = IAUPRI + DIM1
            IBUPLE = SDIM*SDIM + 1
            IBLOLE = IBUPLE + DIM1
            IBUPRI = SDIM*SDIM + DIM1*SDIM + 1
            IBLORI = IBUPRI + DIM1
            IDUPLE = 2*SDIM*SDIM + 1
            IDLOLE = IDUPLE + DIM1
            IDUPRI = 2*SDIM*SDIM + DIM1*SDIM + 1
            IDLORI = IDUPRI + DIM1
            I1UPLE = 3*SDIM*SDIM + 1
            I1LOLE = I1UPLE + DIM1
            I1UPRI = 3*SDIM*SDIM + DIM1*SDIM + 1
            I1LORI = I1UPRI + DIM1
            I2UPLE = 4*SDIM*SDIM + 1
            I2LOLE = I2UPLE + DIM1
            I2UPRI = 4*SDIM*SDIM + DIM1*SDIM + 1
            I2LORI = I2UPRI + DIM1
            I3UPLE = 5*SDIM*SDIM + 1
            I3LOLE = I3UPLE + DIM1
            I3UPRI = 5*SDIM*SDIM + DIM1*SDIM + 1
            I3LORI = I3UPRI + DIM1
C
C           Generate input matrices for MB03CD built of submatrices of A,
C           B and D.
C           Workspace:   need   48.
C
            IF( DIM1.EQ.2 .AND. DIM2.EQ.2 ) THEN
               CALL DLACPY( 'Full', DIM1, DIM1, A( IB1, IB1 ), LDA,
     $                      DWORK( IAUPLE ), SDIM )
               CALL DLACPY( 'Full', DIM2, DIM1, A( M1+IJ1, IB1 ), LDA,
     $                      DWORK( IALOLE ), SDIM )
               CALL DLASET( 'Full', DIM1, DIM2, ZERO, ZERO,
     $                      DWORK( IAUPRI ), SDIM )
               CALL DLACPY( 'Full', DIM2, DIM2, A( M1+IJ1, M1+IJ1 ),
     $                      LDA, DWORK( IALORI ), SDIM )
C
               CALL DLACPY( 'Full', DIM1, DIM1, B( IB1, IB1 ), LDB,
     $                      DWORK( IBUPLE ), SDIM )
               CALL DLACPY( 'Full', DIM2, DIM1, B( M1+IJ1, IB1 ), LDB,
     $                      DWORK( IBLOLE ), SDIM )
               CALL DLASET( 'Full', DIM1, DIM2, ZERO, ZERO,
     $                      DWORK( IBUPRI ), SDIM )
               CALL DLACPY( 'Full', DIM2, DIM2, B( M1+IJ1, M1+IJ1 ),
     $                      LDB, DWORK( IBLORI ), SDIM )
C
               CALL DLACPY( 'Full', DIM1, DIM1, D( IB1, IB1 ), LDD,
     $                      DWORK( IDUPLE ), SDIM )
               CALL DLACPY( 'Full', DIM2, DIM1, D( M1+IJ1, IB1 ), LDD,
     $                      DWORK( IDLOLE ), SDIM )
               CALL DLASET( 'Full', DIM1, DIM2, ZERO, ZERO,
     $                      DWORK( IDUPRI ), SDIM )
               CALL DLACPY( 'Full', DIM2, DIM2, D( M1+IJ1, M1+IJ1 ),
     $                      LDD, DWORK( IDLORI ), SDIM )
C
            ELSE IF( DIM1.EQ.1 .AND. DIM2.EQ.2 ) THEN
               DWORK( IAUPLE ) = A( IB1, IB1 )
               CALL DCOPY( DIM2, A( M1+IJ1, IB1 ), 1, DWORK( IALOLE ),
     $                     1 )
               CALL DCOPY( DIM2, DUM, 0, DWORK( IAUPRI ), SDIM )
               CALL DLACPY( 'Full', DIM2, DIM2, A( M1+IJ1, M1+IJ1 ),
     $                      LDA, DWORK( IALORI ), SDIM )
C
               DWORK( IBUPLE ) = B( IB1, IB1 )
               CALL DCOPY( DIM2, B( M1+IJ1, IB1 ), 1, DWORK( IBLOLE ),
     $                     1 )
               CALL DCOPY( DIM2, DUM, 0, DWORK( IBUPRI ), SDIM )
               CALL DLACPY( 'Full', DIM2, DIM2, B( M1+IJ1, M1+IJ1 ),
     $                      LDB, DWORK( IBLORI ), SDIM )
C
               DWORK( IDUPLE ) = D( IB1, IB1 )
               CALL DCOPY( DIM2, D( M1+IJ1, IB1 ), 1, DWORK( IDLOLE ),
     $                     1 )
               CALL DCOPY( DIM2, DUM, 0, DWORK( IDUPRI ), SDIM )
               CALL DLACPY( 'Full', DIM2, DIM2, D( M1+IJ1, M1+IJ1 ),
     $                      LDD, DWORK( IDLORI ), SDIM )
C
            ELSE IF( DIM1.EQ.2 .AND. DIM2.EQ.1 ) THEN
               CALL DLACPY( 'Full', DIM1, DIM1, A( IB1, IB1 ), LDA,
     $                      DWORK( IAUPLE ), SDIM )
               CALL DCOPY( DIM1, A( M1+IJ1, IB1 ), LDA, DWORK( IALOLE ),
     $                     SDIM )
               CALL DCOPY( DIM1, DUM, 0, DWORK( IAUPRI ), 1 )
               DWORK( IALORI ) = A( M1+IJ1, M1+IJ1 )
C
               CALL DLACPY( 'Full', DIM1, DIM1, B( IB1, IB1 ), LDB,
     $                      DWORK( IBUPLE ), SDIM )
               CALL DCOPY( DIM1, B( M1+IJ1, IB1 ), LDB, DWORK( IBLOLE ),
     $                     SDIM )
               CALL DCOPY( DIM1, DUM, 0, DWORK( IBUPRI ), 1 )
               DWORK( IBLORI ) = B( M1+IJ1, M1+IJ1 )
C
               CALL DLACPY( 'Full', DIM1, DIM1, D( IB1, IB1 ), LDD,
     $                      DWORK( IDUPLE ), SDIM )
               CALL DCOPY( DIM1, D( M1+IJ1, IB1 ), LDD, DWORK( IDLOLE ),
     $                     SDIM )
               CALL DCOPY( DIM1, DUM, 0, DWORK( IDUPRI ), 1 )
               DWORK( IDLORI ) = D( M1+IJ1, M1+IJ1 )
C
            ELSE
               DWORK( IAUPLE ) = A( IB1, IB1 )
               DWORK( IALOLE ) = A( M1+IJ1, IB1 )
               DWORK( IAUPRI ) = ZERO
               DWORK( IALORI ) = A( M1+IJ1, M1+IJ1 )
C
               DWORK( IBUPLE ) = B( IB1, IB1 )
               DWORK( IBLOLE ) = B( M1+IJ1, IB1 )
               DWORK( IBUPRI ) = ZERO
               DWORK( IBLORI ) = B( M1+IJ1, M1+IJ1 )
C
               DWORK( IDUPLE ) = D( IB1, IB1 )
               DWORK( IDLOLE ) = D( M1+IJ1, IB1 )
               DWORK( IDUPRI ) = ZERO
               DWORK( IDLORI ) = D( M1+IJ1, M1+IJ1 )
C
            END IF
C
C           Perform upper triangularization.
C           Workspace:   need   96 + max( 75, 4*N ).
C
            IWRK = 6*SDIM*SDIM + 1
            ITMP = IWRK + 2*N
C
            CALL MB03CD( 'Lower', DIM1, DIM2, ULP, DWORK( IAUPLE ),
     $                   SDIM, DWORK( IBUPLE ), SDIM, DWORK( IDUPLE ),
     $                   SDIM, DWORK( I1UPLE ), SDIM, DWORK( I2UPLE ),
     $                   SDIM, DWORK( I3UPLE ), SDIM, DWORK( IWRK ),
     $                   LDWORK-IWRK+1, INFO )
            IF( INFO.GT.0 ) THEN
               IF( INFO.LE.2 ) THEN
                  INFO = 2
               ELSE IF( INFO.LE.4 ) THEN
                  INFO = 3
               ELSE
                  INFO = 4
               END IF
               RETURN
            END IF
C
            NROW = IJ2 - 1
C
            IF( DIM1.EQ.2 .AND. DIM2.EQ.2 ) THEN
C
C              Update A.
C
               CALL DLACPY( 'Full', NR, DIM1, A( 1, IB1 ), LDA,
     $                      DWORK( IWRK ), NR )
               CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), NR,
     $                      DWORK( I2UPLE ), SDIM, ZERO, A( 1, IB1 ),
     $                      LDA )
               CALL DGEMM(  'No Transpose', 'No Transpose', NR-DIM1,
     $                      DIM1, DIM2, ONE, A( 1, M1+IJ1 ), LDA,
     $                      DWORK( I2LOLE ), SDIM, ONE, A( 1, IB1 ),
     $                      LDA )    
               CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM2,
     $                      DIM1, ONE, DWORK( IWRK ), NR,
     $                      DWORK( I2UPRI ), SDIM, ZERO, DWORK( ITMP ),
     $                      NR )
               CALL DGEMM(  'No Transpose', 'No Transpose', NR-DIM1,
     $                      DIM2, DIM2, ONE, A( 1, M1+IJ1 ), LDA,
     $                      DWORK( I2LORI ), SDIM, ONE, DWORK( ITMP ),
     $                      NR )
               CALL DLACPY( 'Full', NR, DIM2, DWORK( ITMP ), NR,
     $                      A( 1, M1+IJ1 ), LDA )
C
               CALL DLACPY( 'Full', NROW, DIM1, A( I1, IB1 ), LDA,
     $                      DWORK( IWRK ), NROW )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), NROW,
     $                      DWORK( I2UPLE ), SDIM, ZERO, A( I1, IB1 ),
     $                      LDA )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM1,
     $                      DIM2, ONE, A( I1, M1+IJ1 ), LDA,
     $                      DWORK( I2LOLE ), SDIM, ONE, A( I1, IB1 ),
     $                      LDA )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM2,
     $                      DIM1, ONE, DWORK( IWRK ), NROW,
     $                      DWORK( I2UPRI ), SDIM, ZERO, DWORK( ITMP ),
     $                      NROW )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM2,
     $                      DIM2, ONE, A( I1, M1+IJ1 ), LDA,
     $                      DWORK( I2LORI ), SDIM, ONE, DWORK( ITMP ),
     $                      NROW )
               CALL DLACPY( 'Full', NROW, DIM2, DWORK( ITMP ), NROW,
     $                      A( I1, M1+IJ1 ), LDA )
C
               CALL DLACPY( 'Full', DIM1, M1-IB1+1, A( IB1, IB1 ), LDA,
     $                      DWORK( IWRK ), DIM1 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                      DIM1, ONE, DWORK( I3UPLE ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, A( IB1, IB1 ),
     $                      LDA )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                      DIM2, ONE, DWORK( I3LOLE ), SDIM,
     $                      A( M1+IJ1, IB1 ), LDA, ONE, A( IB1, IB1 ),
     $                      LDA )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IB1+1,
     $                      DIM1, ONE, DWORK( I3UPRI ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, DWORK( ITMP ),
     $                      DIM2 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IB1+1,
     $                      DIM2, ONE, DWORK( I3LORI ), SDIM,
     $                      A( M1+IJ1, IB1 ), LDA, ONE, DWORK( ITMP ),
     $                      DIM2 )
               CALL DLACPY( 'Full', DIM2, M1-IB1+1, DWORK( ITMP ), DIM2,
     $                      A( M1+IJ1, IB1 ), LDA )
C
               CALL DLACPY( 'Full', DIM1, M1-IJ1+1, A( IB1, M1+IJ1 ),
     $                      LDA, DWORK( IWRK ), DIM1 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IJ1+1,
     $                      DIM1, ONE, DWORK( I3UPLE ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, A( IB1, M1+IJ1 ),
     $                      LDA )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IJ1+1,
     $                      DIM2, ONE, DWORK( I3LOLE ), SDIM,
     $                      A( M1+IJ1, M1+IJ1 ), LDA, ONE,
     $                      A( IB1, M1+IJ1 ), LDA )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IJ1+1,
     $                      DIM1, ONE, DWORK( I3UPRI ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, DWORK( ITMP ),
     $                      DIM2 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IJ1+1,
     $                      DIM2, ONE, DWORK( I3LORI ), SDIM,
     $                      A( M1+IJ1, M1+IJ1 ), LDA, ONE,
     $                      DWORK( ITMP ), DIM2 )
               CALL DLACPY( 'Full', DIM2, M1-IJ1+1, DWORK( ITMP ), DIM2,
     $                      A( M1+IJ1, M1+IJ1 ), LDA )
C
               IF( M2.GT.0 ) THEN
                  CALL DLACPY( 'Full', DIM1, M4, A( IB1, I2 ), LDA,
     $                         DWORK( IWRK ), DIM1 )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M4,
     $                         DIM1, ONE, DWORK( I3UPLE ), SDIM,
     $                         DWORK( IWRK ), DIM1, ZERO, A( IB1, I2 ),
     $                         LDA )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M4,
     $                         DIM2, ONE, DWORK( I3LOLE ), SDIM,
     $                         A( M1+IJ1, I2 ), LDA, ONE,
     $                         A( IB1, I2 ), LDA )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M4,
     $                         DIM1, ONE, DWORK( I3UPRI ), SDIM,
     $                         DWORK( IWRK ), DIM1, ZERO, DWORK( ITMP ),
     $                         DIM2 )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M4,
     $                         DIM2, ONE, DWORK( I3LORI ), SDIM,
     $                         A( M1+IJ1, I2 ), LDA, ONE, DWORK( ITMP ),
     $                         DIM2 )
                  CALL DLACPY( 'Full', DIM2, M4, DWORK( ITMP ), DIM2,
     $                         A( M1+IJ1, I2 ), LDA )
               END IF
C
C              Update B.
C
               CALL DLACPY( 'Full', NR, DIM1, B( 1, IB1 ), LDB,
     $                      DWORK( IWRK ), NR )
               CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), NR,
     $                      DWORK( I1UPLE ), SDIM, ZERO, B( 1, IB1 ),
     $                      LDB )
               CALL DGEMM(  'No Transpose', 'No Transpose', NR-DIM1,
     $                      DIM1, DIM2, ONE, B( 1, M1+IJ1 ), LDB,
     $                      DWORK( I1LOLE ), SDIM, ONE, B( 1, IB1 ),
     $                      LDB )
               CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM2,
     $                      DIM1, ONE, DWORK( IWRK ), NR,
     $                      DWORK( I1UPRI ), SDIM, ZERO, DWORK( ITMP ),
     $                      NR )
               CALL DGEMM(  'No Transpose', 'No Transpose', NR-DIM1,
     $                      DIM2, DIM2, ONE, B( 1, M1+IJ1 ), LDB,
     $                      DWORK( I1LORI ), SDIM, ONE, DWORK( ITMP ),
     $                      NR )
               CALL DLACPY( 'Full', NR, DIM2, DWORK( ITMP ), NR,
     $                      B( 1, M1+IJ1 ), LDB )
C
               CALL DLACPY( 'Full', NROW, DIM1, B( I1, IB1 ), LDB,
     $                      DWORK( IWRK ), NROW )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), NROW,
     $                      DWORK( I1UPLE ), SDIM, ZERO, B( I1, IB1 ),
     $                      LDB )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM1,
     $                      DIM2, ONE, B( I1, M1+IJ1 ), LDB,
     $                      DWORK( I1LOLE ), SDIM, ONE, B( I1, IB1 ),
     $                      LDB )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM2,
     $                      DIM1, ONE, DWORK( IWRK ), NROW,
     $                      DWORK( I1UPRI ), SDIM, ZERO, DWORK( ITMP ),
     $                      NROW )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM2,
     $                      DIM2, ONE, B( I1, M1+IJ1 ), LDB,
     $                      DWORK( I1LORI ), SDIM, ONE, DWORK( ITMP ),
     $                      NROW )
               CALL DLACPY( 'Full', NROW, DIM2, DWORK( ITMP ), NROW,
     $                      B( I1, M1+IJ1 ), LDB )
C
               CALL DLACPY( 'Full', DIM1, M1-IB1+1, B( IB1, IB1 ), LDB,
     $                      DWORK( IWRK ), DIM1 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                      DIM1, ONE, DWORK( I2UPLE ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, B( IB1, IB1 ),
     $                      LDB )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                      DIM2, ONE, DWORK( I2LOLE ), SDIM,
     $                      B( M1+IJ1, IB1 ), LDB, ONE, B( IB1, IB1 ),
     $                      LDB )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IB1+1,
     $                      DIM1, ONE, DWORK( I2UPRI ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, DWORK( ITMP ),
     $                      DIM2 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IB1+1,
     $                      DIM2, ONE, DWORK( I2LORI ), SDIM,
     $                      B( M1+IJ1, IB1 ), LDB, ONE, DWORK( ITMP ),
     $                      DIM2 )
               CALL DLACPY( 'Full', DIM2, M1-IB1+1, DWORK( ITMP ), DIM2,
     $                      B( M1+IJ1, IB1 ), LDB )
C
               CALL DLACPY( 'Full', DIM1, M1-IJ1+1, B( IB1, M1+IJ1 ),
     $                      LDB, DWORK( IWRK ), DIM1 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IJ1+1,
     $                      DIM1, ONE, DWORK( I2UPLE ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, B( IB1, M1+IJ1 ),
     $                      LDB )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IJ1+1,
     $                      DIM2, ONE, DWORK( I2LOLE ), SDIM,
     $                      B( M1+IJ1, M1+IJ1 ), LDB, ONE,
     $                      B( IB1, M1+IJ1 ), LDB )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IJ1+1,
     $                      DIM1, ONE, DWORK( I2UPRI ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, DWORK( ITMP ),
     $                      DIM2 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IJ1+1,
     $                      DIM2, ONE, DWORK( I2LORI ), SDIM,
     $                      B( M1+IJ1, M1+IJ1 ), LDB, ONE,
     $                      DWORK( ITMP ), DIM2 )
               CALL DLACPY( 'Full', DIM2, M1-IJ1+1, DWORK( ITMP ), DIM2,
     $                      B( M1+IJ1, M1+IJ1 ), LDB )
C
               IF( M2.GT.0 ) THEN
                  CALL DLACPY( 'Full', DIM1, M4, B( IB1, I2 ), LDB,
     $                         DWORK( IWRK ), DIM1 )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M4,
     $                         DIM1, ONE, DWORK( I2UPLE ), SDIM,
     $                         DWORK( IWRK ), DIM1, ZERO, B( IB1, I2 ),
     $                         LDB )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M4,
     $                         DIM2, ONE, DWORK( I2LOLE ), SDIM,
     $                         B( M1+IJ1, I2 ), LDB, ONE,
     $                         B( IB1, I2 ), LDB )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M4,
     $                         DIM1, ONE, DWORK( I2UPRI ), SDIM,
     $                         DWORK( IWRK ), DIM1, ZERO, DWORK( ITMP ),
     $                         DIM2 )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M4,
     $                         DIM2, ONE, DWORK( I2LORI ), SDIM,
     $                         B( M1+IJ1, I2 ), LDB, ONE, DWORK( ITMP ),
     $                         DIM2 )
                  CALL DLACPY( 'Full', DIM2, M4, DWORK( ITMP ), DIM2,
     $                         B( M1+IJ1, I2 ), LDB )
               END IF
C
C              Update D.
C
               CALL DLACPY( 'Full', NR, DIM1, D( 1, IB1 ), LDD,
     $                      DWORK( IWRK ), NR )
               CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), NR,
     $                      DWORK( I1UPLE ), SDIM, ZERO, D( 1, IB1 ),
     $                      LDD )
               CALL DGEMM(  'No Transpose', 'No Transpose', NR-DIM1,
     $                      DIM1, DIM2, ONE, D( 1, M1+IJ1 ), LDD,
     $                      DWORK( I1LOLE ), SDIM, ONE, D( 1, IB1 ),
     $                      LDD )
               CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM2,
     $                      DIM1, ONE, DWORK( IWRK ), NR,
     $                      DWORK( I1UPRI ), SDIM, ZERO, DWORK( ITMP ),
     $                      NR )
               CALL DGEMM(  'No Transpose', 'No Transpose', NR-DIM1,
     $                      DIM2, DIM2, ONE, D( 1, M1+IJ1 ), LDD,
     $                      DWORK( I1LORI ), SDIM, ONE, DWORK( ITMP ),
     $                      NR )
               CALL DLACPY( 'Full', NR, DIM2, DWORK( ITMP ), NR,
     $                      D( 1, M1+IJ1 ), LDD )
C
               CALL DLACPY( 'Full', NROW, DIM1, D( I1, IB1 ), LDD,
     $                      DWORK( IWRK ), NROW )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), NROW,
     $                      DWORK( I1UPLE ), SDIM, ZERO, D( I1, IB1 ),
     $                      LDD )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM1,
     $                      DIM2, ONE, D( I1, M1+IJ1 ), LDD,
     $                      DWORK( I1LOLE ), SDIM, ONE, D( I1, IB1 ),
     $                      LDD )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM2,
     $                      DIM1, ONE, DWORK( IWRK ), NROW,
     $                      DWORK( I1UPRI ), SDIM, ZERO, DWORK( ITMP ),
     $                      NROW )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM2,
     $                      DIM2, ONE, D( I1, M1+IJ1 ), LDD,
     $                      DWORK( I1LORI ), SDIM, ONE, DWORK( ITMP ),
     $                      NROW )
               CALL DLACPY( 'Full', NROW, DIM2, DWORK( ITMP ), NROW,
     $                      D( I1, M1+IJ1 ), LDD )
C
               CALL DLACPY( 'Full', DIM1, M1-IB1+1, D( IB1, IB1 ), LDD,
     $                      DWORK( IWRK ), DIM1 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                      DIM1, ONE, DWORK( I3UPLE ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, D( IB1, IB1 ),
     $                      LDD )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                      DIM2, ONE, DWORK( I3LOLE ), SDIM,
     $                      D( M1+IJ1, IB1 ), LDD, ONE, D( IB1, IB1 ),
     $                      LDD )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IB1+1,
     $                      DIM1, ONE, DWORK( I3UPRI ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, DWORK( ITMP ),
     $                      DIM2 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IB1+1,
     $                      DIM2, ONE, DWORK( I3LORI ), SDIM,
     $                      D( M1+IJ1, IB1 ), LDD, ONE, DWORK( ITMP ),
     $                      DIM2 )
               CALL DLACPY( 'Full', DIM2, M1-IB1+1, DWORK( ITMP ), DIM2,
     $                      D( M1+IJ1, IB1 ), LDD )
C
               CALL DLACPY( 'Full', DIM1, M1-IJ1+1, D( IB1, M1+IJ1 ),
     $                      LDD, DWORK( IWRK ), DIM1 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IJ1+1,
     $                      DIM1, ONE, DWORK( I3UPLE ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, D( IB1, M1+IJ1 ),
     $                      LDD )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IJ1+1,
     $                      DIM2, ONE, DWORK( I3LOLE ), SDIM,
     $                      D( M1+IJ1, M1+IJ1 ), LDD, ONE,
     $                      D( IB1, M1+IJ1 ), LDD )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IJ1+1,
     $                      DIM1, ONE, DWORK( I3UPRI ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, DWORK( ITMP ),
     $                      DIM2 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IJ1+1,
     $                      DIM2, ONE, DWORK( I3LORI ), SDIM,
     $                      D( M1+IJ1, M1+IJ1 ), LDD, ONE,
     $                       DWORK( ITMP ),DIM2 )
               CALL DLACPY( 'Full', DIM2, M1-IJ1+1, DWORK( ITMP ), DIM2,
     $                      D( M1+IJ1, M1+IJ1 ), LDD )
C
               IF( M2.GT.0 ) THEN
                  CALL DLACPY( 'Full', DIM1, M4, D( IB1, I2 ), LDD,
     $                         DWORK( IWRK ), DIM1 )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M4,
     $                         DIM1, ONE, DWORK( I3UPLE ), SDIM,
     $                         DWORK( IWRK ), DIM1, ZERO, D( IB1, I2 ),
     $                         LDD )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M4,
     $                         DIM2, ONE, DWORK( I3LOLE ), SDIM,
     $                         D( M1+IJ1, I2 ), LDD, ONE,
     $                         D( IB1, I2 ), LDD )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M4,
     $                         DIM1, ONE, DWORK( I3UPRI ), SDIM,
     $                         DWORK( IWRK ), DIM1, ZERO, DWORK( ITMP ),
     $                         DIM2 )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M4,
     $                         DIM2, ONE, DWORK( I3LORI ), SDIM,
     $                         D( M1+IJ1, I2 ), LDD, ONE, DWORK( ITMP ),
     $                         DIM2 )
                  CALL DLACPY( 'Full', DIM2, M4, DWORK( ITMP ), DIM2,
     $                         D( M1+IJ1, I2 ), LDD )
               END IF
C
               IF( LCMPQ1 ) THEN
C
C                 Update Q1.
C
                  CALL DLACPY( 'Full', N, DIM1, Q1( 1, IB1 ), LDQ1,
     $                         DWORK( IWRK ), N )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                         DIM1, ONE, DWORK( IWRK ), N,
     $                         DWORK( I1UPLE ), SDIM, ZERO,
     $                         Q1( 1, IB1 ), LDQ1 )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                         DIM2, ONE, Q1( 1, M1+IJ1 ), LDQ1,
     $                         DWORK( I1LOLE ), SDIM, ONE, Q1( 1, IB1 ),
     $                         LDQ1 )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM2,
     $                         DIM1, ONE, DWORK( IWRK ), N,
     $                         DWORK( I1UPRI ), SDIM, ZERO,
     $                         DWORK( ITMP ), N )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM2,
     $                         DIM2, ONE, Q1( 1, M1+IJ1 ), LDQ1,
     $                         DWORK( I1LORI ), SDIM, ONE,
     $                         DWORK( ITMP ), N )
                  CALL DLACPY( 'Full', N, DIM2, DWORK( ITMP ), N,
     $                         Q1( 1, M1+IJ1 ), LDQ1 )
               END IF
C
               IF( LCMPQ2 ) THEN
C
C                 Update Q2.
C
                  CALL DLACPY( 'Full', N, DIM1, Q2( 1, IB1 ), LDQ2,
     $                         DWORK( IWRK ), N )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                         DIM1, ONE, DWORK( IWRK ), N,
     $                         DWORK( I2UPLE ), SDIM, ZERO,
     $                         Q2( 1, IB1 ), LDQ2 )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                         DIM2, ONE, Q2( 1, M1+IJ1 ), LDQ2,
     $                         DWORK( I2LOLE ), SDIM, ONE, Q2( 1, IB1 ),
     $                         LDQ2 )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM2,
     $                         DIM1, ONE, DWORK( IWRK ), N,
     $                         DWORK( I2UPRI ), SDIM, ZERO,
     $                         DWORK( ITMP ), N )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM2,
     $                         DIM2, ONE, Q2( 1, M1+IJ1 ), LDQ2,
     $                         DWORK( I2LORI ), SDIM, ONE,
     $                         DWORK( ITMP ), N )
                  CALL DLACPY( 'Full', N, DIM2, DWORK( ITMP ), N,
     $                         Q2( 1, M1+IJ1 ), LDQ2 )
               END IF
C
               IF( LCMPQ3 ) THEN
C
C                 Update Q3.
C
                  CALL DLACPY( 'Full', N, DIM1, Q3( 1, IB1 ), LDQ3,
     $                         DWORK( IWRK ), N )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                         DIM1, ONE, DWORK( IWRK ), N,
     $                         DWORK( I3UPLE ), SDIM, ZERO,
     $                         Q3( 1, IB1 ), LDQ3 )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                         DIM2, ONE, Q3( 1, M1+IJ1 ), LDQ3,
     $                         DWORK( I3LOLE ), SDIM, ONE, Q3( 1, IB1 ),
     $                         LDQ3 )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM2,
     $                         DIM1, ONE, DWORK( IWRK ), N,
     $                         DWORK( I3UPRI ), SDIM, ZERO,
     $                         DWORK( ITMP ), N )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM2,
     $                         DIM2, ONE, Q3( 1, M1+IJ1 ), LDQ3,
     $                         DWORK( I3LORI ), SDIM, ONE,
     $                         DWORK( ITMP ), N )
                  CALL DLACPY( 'Full', N, DIM2, DWORK( ITMP ), N,
     $                         Q3( 1, M1+IJ1 ), LDQ3 )
               END IF
C
            ELSE IF( DIM1.EQ.1 .AND. DIM2.EQ.2 ) THEN
C
C              Update A.
C
               CALL DCOPY( NR, A( 1, IB1 ), 1, DWORK( IWRK ), 1 )
               CALL DGEMV( 'No Transpose', NR-1, DIM2, ONE,
     $                     A( 1, M1+IJ1 ), LDA, DWORK( I2LOLE ), 1,
     $                     DWORK( I2UPLE ), A( 1, IB1 ), 1 )
               A( NR, IB1 ) = DWORK( I2UPLE )*A( NR, IB1 )
               CALL DGEMM( 'No Transpose', 'No Transpose', NR-1, DIM2,
     $                     DIM2, ONE, A( 1, M1+IJ1 ), LDA,
     $                     DWORK( I2LORI ), SDIM, ZERO, DWORK( ITMP ),
     $                     NR )
               DWORK( ITMP+  NR-1 ) = ZERO
               DWORK( ITMP+2*NR-1 ) = ZERO
               CALL DAXPY(  NR, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                      DWORK( ITMP ), 1 )
               CALL DAXPY(  NR, DWORK( I2UPRI+SDIM ), DWORK( IWRK ), 1,
     $                      DWORK( ITMP+NR ), 1 )
               CALL DLACPY( 'Full', NR, DIM2, DWORK( ITMP ), NR,
     $                      A( 1, M1+IJ1 ), LDA )
C
               CALL DCOPY(  NROW, A( I1, IB1 ), 1, DWORK( IWRK ), 1 )
               CALL DGEMV(  'No Transpose', NROW, DIM2, ONE,
     $                      A( I1, M1+IJ1 ), LDA, DWORK( I2LOLE ), 1,
     $                      DWORK( I2UPLE ), A( I1, IB1 ), 1 )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM2,
     $                      DIM2, ONE, A( I1, M1+IJ1 ), LDA,
     $                      DWORK( I2LORI ), SDIM, ZERO, DWORK( ITMP ),
     $                      NROW )
               CALL DAXPY(  NROW, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                      DWORK( ITMP ), 1 )
               CALL DAXPY(  NROW, DWORK( I2UPRI+SDIM ), DWORK( IWRK ),
     $                      1, DWORK( ITMP+NROW ), 1 )
               CALL DLACPY( 'Full', NROW, DIM2, DWORK( ITMP ), NROW,
     $                      A( I1, M1+IJ1 ), LDA )
C
               CALL DCOPY(  M1-IB1+1, A( IB1, IB1 ), LDA, DWORK( IWRK ),
     $                      1 )
               CALL DGEMV(  'Transpose', DIM2, M1-IB1+1, ONE,
     $                      A( M1+IJ1, IB1 ), LDA, DWORK( I3LOLE ), 1,
     $                      DWORK( I3UPLE ), A( IB1, IB1 ), LDA )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IB1+1,
     $                      DIM2, ONE, DWORK( I3LORI ), SDIM,
     $                      A( M1+IJ1, IB1 ), LDA, ZERO, DWORK( ITMP ),
     $                      DIM2 )
               CALL DAXPY(  M1-IB1+1, DWORK( I3UPRI ), DWORK( IWRK ), 1,
     $                      DWORK( ITMP ), DIM2 )
               CALL DAXPY(  M1-IB1+1, DWORK( I3UPRI+SDIM ),
     $                      DWORK( IWRK ), 1, DWORK( ITMP+1 ), DIM2 )
               CALL DLACPY( 'Full', DIM2, M1-IB1+1, DWORK( ITMP ), DIM2,
     $                      A( M1+IJ1, IB1 ), LDA )
C
               CALL DCOPY(  M1-IJ1+1, A( IB1, M1+IJ1 ), LDA,
     $                      DWORK( IWRK ), 1 )
               CALL DGEMV(  'Transpose', DIM2, M1-IJ1+1, ONE,
     $                      A( M1+IJ1, M1+IJ1 ), LDA, DWORK( I3LOLE ),
     $                      1, DWORK( I3UPLE ), A( IB1, M1+IJ1 ), LDA )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IJ1+1,
     $                      DIM2, ONE, DWORK( I3LORI ), SDIM,
     $                      A( M1+IJ1, M1+IJ1 ), LDA, ZERO,
     $                      DWORK( ITMP ), DIM2 )
               CALL DAXPY(  M1-IJ1+1, DWORK( I3UPRI ), DWORK( IWRK ), 1,
     $                      DWORK( ITMP ), DIM2 )
               CALL DAXPY(  M1-IJ1+1, DWORK( I3UPRI+SDIM ),
     $                      DWORK( IWRK ), 1, DWORK( ITMP+1 ), DIM2 )
               CALL DLACPY( 'Full', DIM2, M1-IJ1+1, DWORK( ITMP ), DIM2,
     $                      A( M1+IJ1, M1+IJ1 ), LDA )
C
               IF( M2.GT.0 ) THEN
                  CALL DCOPY(  M4, A( IB1, I2 ), LDA, DWORK( IWRK ), 1 )
                  CALL DGEMV(  'Transpose', DIM2, M4, ONE,
     $                         A( M1+IJ1, I2 ), LDA, DWORK( I3LOLE ), 1,
     $                         DWORK( I3UPLE ), A( IB1, I2 ), LDA )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M4,
     $                         DIM2, ONE, DWORK( I3LORI ), SDIM,
     $                         A( M1+IJ1, I2 ), LDA, ZERO,
     $                         DWORK( ITMP ), DIM2 )
                  CALL DAXPY(  M4, DWORK( I3UPRI ), DWORK( IWRK ), 1,
     $                         DWORK( ITMP ), DIM2 )
                  CALL DAXPY(  M4, DWORK( I3UPRI+SDIM ), DWORK( IWRK ),
     $                         1, DWORK( ITMP+1 ), DIM2 )
                  CALL DLACPY( 'Full', DIM2, M4, DWORK( ITMP ), DIM2,
     $                         A( M1+IJ1, I2 ), LDA )
               END IF
C
C              Update B.
C
               CALL DCOPY( NR, B( 1, IB1 ), 1, DWORK( IWRK ), 1 )
               CALL DGEMV( 'No Transpose', NR-1, DIM2, ONE,
     $                     B( 1, M1+IJ1 ), LDB, DWORK( I1LOLE ), 1,
     $                     DWORK( I1UPLE ), B( 1, IB1 ), 1 )
               B( NR, IB1 ) = DWORK( I1UPLE )*B( NR, IB1 )
               CALL DGEMM( 'No Transpose', 'No Transpose', NR-1, DIM2,
     $                     DIM2, ONE, B( 1, M1+IJ1 ), LDB,
     $                     DWORK( I1LORI ), SDIM, ZERO, DWORK( ITMP ),
     $                     NR )
               DWORK( ITMP+  NR-1 ) = ZERO
               DWORK( ITMP+2*NR-1 ) = ZERO
               CALL DAXPY(  NR, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                      DWORK( ITMP ), 1 )
               CALL DAXPY(  NR, DWORK( I1UPRI+SDIM ), DWORK( IWRK ), 1,
     $                      DWORK( ITMP+NR ), 1 )
               CALL DLACPY( 'Full', NR, DIM2, DWORK( ITMP ), NR,
     $                      B( 1, M1+IJ1 ), LDB )
C
               CALL DCOPY(  NROW, B( I1, IB1 ), 1, DWORK( IWRK ), 1 )
               CALL DGEMV(  'No Transpose', NROW, DIM2, ONE,
     $                      B( I1, M1+IJ1 ), LDB, DWORK( I1LOLE ), 1,
     $                      DWORK( I1UPLE ), B( I1, IB1 ), 1 )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM2,
     $                      DIM2, ONE, B( I1, M1+IJ1 ), LDB,
     $                      DWORK( I1LORI ), SDIM, ZERO, DWORK( ITMP ),
     $                      NROW )
               CALL DAXPY(  NROW, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                      DWORK( ITMP ), 1 )
               CALL DAXPY(  NROW, DWORK( I1UPRI+SDIM ), DWORK( IWRK ),
     $                      1, DWORK( ITMP+NROW ), 1 )
               CALL DLACPY( 'Full', NROW, DIM2, DWORK( ITMP ), NROW,
     $                      B( I1, M1+IJ1 ), LDB )
C
               CALL DCOPY(  M1-IB1+1, B( IB1, IB1 ), LDB, DWORK( IWRK ),
     $                      1 )
               CALL DGEMV(  'Transpose', DIM2, M1-IB1+1, ONE,
     $                      B( M1+IJ1, IB1 ), LDB, DWORK( I2LOLE ), 1,
     $                      DWORK( I2UPLE ), B( IB1, IB1 ), LDB )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IB1+1,
     $                      DIM2, ONE, DWORK( I2LORI ), SDIM,
     $                      B( M1+IJ1, IB1 ), LDB, ZERO, DWORK( ITMP ),
     $                      DIM2 )
               CALL DAXPY(  M1-IB1+1, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                      DWORK( ITMP ), DIM2 )
               CALL DAXPY(  M1-IB1+1, DWORK( I2UPRI+SDIM ),
     $                      DWORK( IWRK ), 1, DWORK( ITMP+1 ), DIM2 )
               CALL DLACPY( 'Full', DIM2, M1-IB1+1, DWORK( ITMP ), DIM2,
     $                      B( M1+IJ1, IB1 ), LDB )
C
               CALL DCOPY(  M1-IJ1+1, B( IB1, M1+IJ1 ), LDB,
     $                      DWORK( IWRK ), 1 )
               CALL DGEMV(  'Transpose', DIM2, M1-IJ1+1, ONE,
     $                      B( M1+IJ1, M1+IJ1 ), LDB, DWORK( I2LOLE ),
     $                      1, DWORK( I2UPLE ), B( IB1, M1+IJ1 ), LDB )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IJ1+1,
     $                      DIM2, ONE, DWORK( I2LORI ), SDIM,
     $                      B( M1+IJ1, M1+IJ1 ), LDB, ZERO,
     $                      DWORK( ITMP ), DIM2 )
               CALL DAXPY(  M1-IJ1+1, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                      DWORK( ITMP ), DIM2 )
               CALL DAXPY(  M1-IJ1+1, DWORK( I2UPRI+SDIM ),
     $                      DWORK( IWRK ), 1, DWORK( ITMP+1 ), DIM2 )
               CALL DLACPY( 'Full', DIM2, M1-IJ1+1, DWORK( ITMP ), DIM2,
     $                      B( M1+IJ1, M1+IJ1 ), LDB )
C
               IF( M2.GT.0 ) THEN
                  CALL DCOPY(  M4, B( IB1, I2 ), LDB, DWORK( IWRK ), 1 )
                  CALL DGEMV(  'Transpose', DIM2, M4, ONE,
     $                         B( M1+IJ1, I2 ), LDB, DWORK( I2LOLE ), 1,
     $                         DWORK( I2UPLE ), B( IB1, I2 ), LDB )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M4,
     $                         DIM2, ONE, DWORK( I2LORI ), SDIM,
     $                         B( M1+IJ1, I2 ), LDB, ZERO,
     $                         DWORK( ITMP ), DIM2 )
                  CALL DAXPY(  M4, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                         DWORK( ITMP ), DIM2 )
                  CALL DAXPY(  M4, DWORK( I2UPRI+SDIM ), DWORK( IWRK ),
     $                         1, DWORK( ITMP+1 ), DIM2 )
                  CALL DLACPY( 'Full', DIM2, M4, DWORK( ITMP ), DIM2,
     $                         B( M1+IJ1, I2 ), LDB )
               END IF
C
C              Update D.
C
               CALL DCOPY( NR, D( 1, IB1 ), 1, DWORK( IWRK ), 1 )
               CALL DGEMV( 'No Transpose', NR-1, DIM2, ONE,
     $                     D( 1, M1+IJ1 ), LDD, DWORK( I1LOLE ), 1,
     $                     DWORK( I1UPLE ), D( 1, IB1 ), 1 )
               D( NR, IB1 ) = DWORK( I1UPLE )*D( NR, IB1 )
               CALL DGEMM( 'No Transpose', 'No Transpose', NR-1, DIM2,
     $                     DIM2, ONE, D( 1, M1+IJ1 ), LDD,
     $                     DWORK( I1LORI ), SDIM, ZERO, DWORK( ITMP ),
     $                     NR )
               DWORK( ITMP+  NR-1 ) = ZERO
               DWORK( ITMP+2*NR-1 ) = ZERO
               CALL DAXPY(  NR, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                      DWORK( ITMP ), 1 )
               CALL DAXPY(  NR, DWORK( I1UPRI+SDIM ), DWORK( IWRK ), 1,
     $                      DWORK( ITMP+NR ), 1 )
               CALL DLACPY( 'Full', NR, DIM2, DWORK( ITMP ), NR,
     $                      D( 1, M1+IJ1 ), LDD )
C
               CALL DCOPY(  NROW, D( I1, IB1 ), 1, DWORK( IWRK ), 1 )
               CALL DGEMV(  'No Transpose', NROW, DIM2, ONE,
     $                      D( I1, M1+IJ1 ), LDD, DWORK( I1LOLE ), 1,
     $                      DWORK( I1UPLE ), D( I1, IB1 ), 1 )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM2,
     $                      DIM2, ONE, D( I1, M1+IJ1 ), LDD,
     $                      DWORK( I1LORI ), SDIM, ZERO, DWORK( ITMP ),
     $                      NROW )
               CALL DAXPY(  NROW, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                      DWORK( ITMP ), 1 )
               CALL DAXPY(  NROW, DWORK( I1UPRI+SDIM ), DWORK( IWRK ),
     $                      1, DWORK( ITMP+NROW ), 1 )
               CALL DLACPY( 'Full', NROW, DIM2, DWORK( ITMP ), NROW,
     $                      D( I1, M1+IJ1 ), LDD )
C
               CALL DCOPY(  M1-IB1+1, D( IB1, IB1 ), LDD, DWORK( IWRK ),
     $                      1 )
               CALL DGEMV(  'Transpose', DIM2, M1-IB1+1, ONE,
     $                      D( M1+IJ1, IB1 ), LDD, DWORK( I3LOLE ), 1,
     $                      DWORK( I3UPLE ), D( IB1, IB1 ), LDD )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IB1+1,
     $                      DIM2, ONE, DWORK( I3LORI ), SDIM,
     $                      D( M1+IJ1, IB1 ), LDD, ZERO, DWORK( ITMP ),
     $                      DIM2 )
               CALL DAXPY(  M1-IB1+1, DWORK( I3UPRI ), DWORK( IWRK ), 1,
     $                      DWORK( ITMP ), DIM2 )
               CALL DAXPY(  M1-IB1+1, DWORK( I3UPRI+SDIM ),
     $                      DWORK( IWRK ), 1, DWORK( ITMP+1 ), DIM2 )
               CALL DLACPY( 'Full', DIM2, M1-IB1+1, DWORK( ITMP ), DIM2,
     $                      D( M1+IJ1, IB1 ), LDD )
C
               CALL DCOPY(  M1-IJ1+1, D( IB1, M1+IJ1 ), LDD,
     $                      DWORK( IWRK ), 1 )
               CALL DGEMV(  'Transpose', DIM2, M1-IJ1+1, ONE,
     $                      D( M1+IJ1, M1+IJ1 ), LDD, DWORK( I3LOLE ),
     $                      1, DWORK( I3UPLE ), D( IB1, M1+IJ1 ), LDD )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IJ1+1,
     $                      DIM2, ONE, DWORK( I3LORI ), SDIM,
     $                      D( M1+IJ1, M1+IJ1 ), LDD, ZERO,
     $                      DWORK( ITMP ), DIM2 )
               CALL DAXPY(  M1-IJ1+1, DWORK( I3UPRI ), DWORK( IWRK ), 1,
     $                      DWORK( ITMP ), DIM2 )
               CALL DAXPY(  M1-IJ1+1, DWORK( I3UPRI+SDIM ),
     $                      DWORK( IWRK ), 1, DWORK( ITMP+1 ), DIM2 )
               CALL DLACPY( 'Full', DIM2, M1-IJ1+1, DWORK( ITMP ), DIM2,
     $                      D( M1+IJ1, M1+IJ1 ), LDD )
C
               IF( M2.GT.0 ) THEN
                  CALL DCOPY(  M4, D( IB1, I2 ), LDD, DWORK( IWRK ), 1 )
                  CALL DGEMV(  'Transpose', DIM2, M4, ONE,
     $                         D( M1+IJ1, I2 ), LDD, DWORK( I3LOLE ), 1,
     $                         DWORK( I3UPLE ), D( IB1, I2 ), LDD )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M4,
     $                         DIM2, ONE, DWORK( I3LORI ), SDIM,
     $                         D( M1+IJ1, I2 ), LDD, ZERO,
     $                         DWORK( ITMP ), DIM2 )
                  CALL DAXPY(  M4, DWORK( I3UPRI ), DWORK( IWRK ), 1,
     $                         DWORK( ITMP ), DIM2 )
                  CALL DAXPY(  M4, DWORK( I3UPRI+SDIM ), DWORK( IWRK ),
     $                         1, DWORK( ITMP+1 ), DIM2 )
                  CALL DLACPY( 'Full', DIM2, M4, DWORK( ITMP ), DIM2,
     $                         D( M1+IJ1, I2 ), LDD )
               END IF
C
C              Update Q1.
C
               IF( LCMPQ1 ) THEN
                  CALL DCOPY(  N, Q1( 1, IB1 ), 1, DWORK( IWRK ), 1 )
                  CALL DGEMV(  'No Transpose', N, DIM2, ONE,
     $                         Q1( 1, M1+IJ1 ), LDQ1, DWORK( I1LOLE ),
     $                         1, DWORK( I1UPLE ), Q1( 1, IB1 ), 1 )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM2,
     $                         DIM2, ONE, Q1( 1, M1+IJ1 ), LDQ1,
     $                         DWORK( I1LORI ), SDIM, ZERO,
     $                         DWORK( ITMP ), N )
                  CALL DAXPY(  N, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                         DWORK( ITMP ), 1 )
                  CALL DAXPY(  N, DWORK( I1UPRI+SDIM ), DWORK( IWRK ),
     $                         1, DWORK( ITMP+N ), 1 )
                  CALL DLACPY( 'Full', N, DIM2, DWORK( ITMP ), N,
     $                         Q1( 1, M1+IJ1 ), LDQ1 )
               END IF
C
C              Update Q2.
C
               IF( LCMPQ2 ) THEN
                  CALL DCOPY(  N, Q2( 1, IB1 ), 1, DWORK( IWRK ), 1 )
                  CALL DGEMV(  'No Transpose', N, DIM2, ONE,
     $                         Q2( 1, M1+IJ1 ), LDQ2, DWORK( I2LOLE ),
     $                         1, DWORK( I2UPLE ), Q2( 1, IB1 ), 1 )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM2,
     $                         DIM2, ONE, Q2( 1, M1+IJ1 ), LDQ2,
     $                         DWORK( I2LORI ), SDIM, ZERO,
     $                         DWORK( ITMP ), N )
                  CALL DAXPY(  N, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                         DWORK( ITMP ), 1 )
                  CALL DAXPY(  N, DWORK( I2UPRI+SDIM ), DWORK( IWRK ),
     $                         1, DWORK( ITMP+N ), 1 )
                  CALL DLACPY( 'Full', N, DIM2, DWORK( ITMP ), N,
     $                         Q2( 1, M1+IJ1 ), LDQ2 )
               END IF
C
C              Update Q3.
C
               IF( LCMPQ3 ) THEN
                  CALL DCOPY(  N, Q3( 1, IB1 ), 1, DWORK( IWRK ), 1 )
                  CALL DGEMV(  'No Transpose', N, DIM2, ONE,
     $                         Q3( 1, M1+IJ1 ), LDQ3, DWORK( I3LOLE ),
     $                         1, DWORK( I3UPLE ), Q3( 1, IB1 ), 1 )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM2,
     $                         DIM2, ONE, Q3( 1, M1+IJ1 ), LDQ3,
     $                         DWORK( I3LORI ), SDIM, ZERO,
     $                         DWORK( ITMP ), N )
                  CALL DAXPY(  N, DWORK( I3UPRI ), DWORK( IWRK ), 1,
     $                         DWORK( ITMP ), 1 )
                  CALL DAXPY(  N, DWORK( I3UPRI+SDIM ), DWORK( IWRK ),
     $                         1, DWORK( ITMP+N ), 1 )
                  CALL DLACPY( 'Full', N, DIM2, DWORK( ITMP ), N,
     $                         Q3( 1, M1+IJ1 ), LDQ3 )
               END IF
C
            ELSE IF( DIM1.EQ.2 .AND. DIM2.EQ.1 ) THEN
C
C              Update A.
C
               CALL DLACPY( 'Full', NR, DIM1, A( 1, IB1 ), LDA,
     $                      DWORK( IWRK ), NR )
               CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), NR,
     $                      DWORK( I2UPLE ), SDIM, ZERO, A( 1, IB1 ),
     $                      LDA )
               CALL DAXPY(  NR-1, DWORK( I2LOLE ), A( 1, M1+IJ1 ), 1,
     $                      A( 1, IB1 ), 1 )
               CALL DAXPY(  NR-1, DWORK( I2LOLE+SDIM ), A( 1, M1+IJ1 ),
     $                      1, A( 1, IB1+1 ), 1 )
               A( NR, M1+IJ1 ) = ZERO
               CALL DGEMV(  'No Transpose', NR, DIM1, ONE,
     $                      DWORK( IWRK ), NR, DWORK( I2UPRI ), 1,
     $                      DWORK( I2LORI ), A( 1, M1+IJ1 ), 1 )
C
               CALL DLACPY( 'Full', NROW, DIM1, A( I1, IB1 ), LDA,
     $                      DWORK( IWRK ), NROW )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), NROW,
     $                      DWORK( I2UPLE ), SDIM, ZERO, A( I1, IB1 ),
     $                      LDA )
               CALL DAXPY(  NROW, DWORK( I2LOLE ), A( I1, M1+IJ1 ), 1,
     $                      A( I1, IB1 ), 1 )
               CALL DAXPY(  NROW, DWORK( I2LOLE+SDIM ), A( I1, M1+IJ1 ),
     $                      1, A( I1, IB1+1 ), 1 )
               CALL DGEMV(  'No Transpose', NROW, DIM1, ONE,
     $                      DWORK( IWRK ), NROW, DWORK( I2UPRI ), 1,
     $                      DWORK( I2LORI ), A( I1, M1+IJ1 ), 1 )
C
               CALL DLACPY( 'Full', DIM1, M1-IB1+1, A( IB1, IB1 ), LDA,
     $                      DWORK( IWRK ), DIM1 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                      DIM1, ONE, DWORK( I3UPLE ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, A( IB1, IB1 ),
     $                      LDA )
               CALL DAXPY(  M1-IB1+1, DWORK( I3LOLE ), A( M1+IJ1, IB1 ),
     $                      LDA, A( IB1, IB1 ), LDA )
               CALL DAXPY(  M1-IB1+1, DWORK( I3LOLE+SDIM ),
     $                      A( M1+IJ1, IB1 ), LDA, A( IB1+1, IB1 ),
     $                      LDA )
               CALL DGEMV(  'Transpose', DIM1, M1-IB1+1, ONE,
     $                      DWORK( IWRK ), DIM1, DWORK( I3UPRI ), 1,
     $                      DWORK( I3LORI ), A( M1+IJ1, IB1 ), LDA )
C
               CALL DLACPY( 'Full', DIM1, M1-IJ1+1, A( IB1, M1+IJ1 ),
     $                      LDA, DWORK( IWRK ), DIM1 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IJ1+1,
     $                      DIM1, ONE, DWORK( I3UPLE ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, A( IB1, M1+IJ1 ),
     $                      LDA )
               CALL DAXPY(  M1-IJ1+1, DWORK( I3LOLE ),
     $                      A( M1+IJ1, M1+IJ1 ), LDA, A( IB1, M1+IJ1 ),
     $                      LDA )
               CALL DAXPY(  M1-IJ1+1, DWORK( I3LOLE+SDIM ),
     $                      A( M1+IJ1, M1+IJ1 ), LDA,
     $                      A( IB1+1, M1+IJ1 ), LDA )
               CALL DGEMV(  'Transpose', DIM1, M1-IJ1+1, ONE,
     $                      DWORK( IWRK ), DIM1, DWORK( I3UPRI ), 1,
     $                      DWORK( I3LORI ), A( M1+IJ1, M1+IJ1 ), LDA )
C
               IF( M2.GT.0 ) THEN
                  CALL DLACPY( 'Full', DIM1, M4, A( IB1, I2 ), LDA,
     $                         DWORK( IWRK ), DIM1 )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M4,
     $                         DIM1, ONE, DWORK( I3UPLE ), SDIM,
     $                         DWORK( IWRK ), DIM1, ZERO, A( IB1, I2 ),
     $                         LDA )
                  CALL DAXPY(  M4, DWORK( I3LOLE ), A( M1+IJ1, I2 ),
     $                         LDA, A( IB1, I2 ), LDA )
                  CALL DAXPY(  M4, DWORK( I3LOLE+SDIM ),
     $                         A( M1+IJ1, I2 ), LDA, A( IB1+1, I2 ),
     $                         LDA )
                  CALL DGEMV(  'Transpose', DIM1, M4, ONE,
     $                         DWORK( IWRK ), DIM1, DWORK( I3UPRI ), 1,
     $                         DWORK( I3LORI ), A( M1+IJ1, I2 ), LDA )
               END IF
C
C              Update B.
C
               CALL DLACPY( 'Full', NR, DIM1, B( 1, IB1 ), LDB,
     $                      DWORK( IWRK ), NR )
               CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), NR,
     $                      DWORK( I1UPLE ), SDIM, ZERO, B( 1, IB1 ),
     $                      LDB )
               CALL DAXPY(  NR-1, DWORK( I1LOLE ), B( 1, M1+IJ1 ), 1,
     $                      B( 1, IB1 ), 1 )
               CALL DAXPY(  NR-1, DWORK( I1LOLE+SDIM ), B( 1, M1+IJ1 ),
     $                      1, B( 1, IB1+1 ), 1 )
               B( NR, M1+IJ1 ) = ZERO
               CALL DGEMV(  'No Transpose', NR, DIM1, ONE,
     $                      DWORK( IWRK ), NR, DWORK( I1UPRI ), 1,
     $                      DWORK( I1LORI ), B( 1, M1+IJ1 ), 1 )
C
               CALL DLACPY( 'Full', NROW, DIM1, B( I1, IB1 ), LDB,
     $                      DWORK( IWRK ), NROW )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), NROW,
     $                      DWORK( I1UPLE ), SDIM, ZERO, B( I1, IB1 ),
     $                      LDB )
               CALL DAXPY(  NROW, DWORK( I1LOLE ), B( I1, M1+IJ1 ), 1,
     $                      B( I1, IB1 ), 1 )
               CALL DAXPY(  NROW, DWORK( I1LOLE+SDIM ), B( I1, M1+IJ1 ),
     $                      1, B( I1, IB1+1 ), 1 )
               CALL DGEMV(  'No Transpose', NROW, DIM1, ONE,
     $                      DWORK( IWRK ), NROW, DWORK( I1UPRI ), 1,
     $                      DWORK( I1LORI ), B( I1, M1+IJ1 ), 1 )
C
               CALL DLACPY( 'Full', DIM1, M1-IB1+1, B( IB1, IB1 ), LDB,
     $                      DWORK( IWRK ), DIM1 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                      DIM1, ONE, DWORK( I2UPLE ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, B( IB1, IB1 ),
     $                      LDB )
               CALL DAXPY(  M1-IB1+1, DWORK( I2LOLE ), B( M1+IJ1, IB1 ),
     $                      LDB, B( IB1, IB1 ), LDB )
               CALL DAXPY(  M1-IB1+1, DWORK( I2LOLE+SDIM ),
     $                      B( M1+IJ1, IB1 ), LDB, B( IB1+1, IB1 ),
     $                      LDB )
               CALL DGEMV(  'Transpose', DIM1, M1-IB1+1, ONE,
     $                      DWORK( IWRK ), DIM1, DWORK( I2UPRI ), 1,
     $                      DWORK( I2LORI ), B( M1+IJ1, IB1 ), LDB )
C
               CALL DLACPY( 'Full', DIM1, M1-IJ1+1, B( IB1, M1+IJ1 ),
     $                      LDB, DWORK( IWRK ), DIM1 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IJ1+1,
     $                      DIM1, ONE, DWORK( I2UPLE ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, B( IB1, M1+IJ1 ),
     $                      LDB )
               CALL DAXPY(  M1-IJ1+1, DWORK( I2LOLE ),
     $                      B( M1+IJ1, M1+IJ1 ), LDB, B( IB1, M1+IJ1 ),
     $                      LDB )
               CALL DAXPY(  M1-IJ1+1, DWORK( I2LOLE+SDIM ),
     $                      B( M1+IJ1, M1+IJ1 ), LDB,
     $                      B( IB1+1, M1+IJ1 ), LDB )
               CALL DGEMV(  'Transpose', DIM1, M1-IJ1+1, ONE,
     $                      DWORK( IWRK ), DIM1, DWORK( I2UPRI ), 1,
     $                      DWORK( I2LORI ), B( M1+IJ1, M1+IJ1 ), LDB )
C
               IF( M2.GT.0 ) THEN
                  CALL DLACPY( 'Full', DIM1, M4, B( IB1, I2 ), LDB,
     $                         DWORK( IWRK ), DIM1 )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M4,
     $                         DIM1, ONE, DWORK( I2UPLE ), SDIM,
     $                         DWORK( IWRK ), DIM1, ZERO, B( IB1, I2 ),
     $                         LDB )
                  CALL DAXPY(  M4, DWORK( I2LOLE ), B( M1+IJ1, I2 ),
     $                         LDB, B( IB1, I2 ), LDB )
                  CALL DAXPY(  M4, DWORK( I2LOLE+SDIM ),
     $                         B( M1+IJ1, I2 ), LDB, B( IB1+1, I2 ),
     $                         LDB )
                  CALL DGEMV(  'Transpose', DIM1, M4, ONE,
     $                         DWORK( IWRK ), DIM1, DWORK( I2UPRI ), 1,
     $                         DWORK( I2LORI ), B( M1+IJ1, I2 ), LDB )
               END IF
C
C              Update D.
C
               CALL DLACPY( 'Full', NR, DIM1, D( 1, IB1 ), LDD,
     $                      DWORK( IWRK ), NR )
               CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), NR,
     $                      DWORK( I1UPLE ), SDIM, ZERO, D( 1, IB1 ),
     $                      LDD )
               CALL DAXPY(  NR-1, DWORK( I1LOLE ), D( 1, M1+IJ1 ), 1,
     $                      D( 1, IB1 ), 1 )
               CALL DAXPY(  NR-1, DWORK( I1LOLE+SDIM ), D( 1, M1+IJ1 ),
     $                      1, D( 1, IB1+1 ), 1 )
               D( NR, M1+IJ1 ) = ZERO
               CALL DGEMV(  'No Transpose', NR, DIM1, ONE,
     $                      DWORK( IWRK ), NR, DWORK( I1UPRI ), 1,
     $                      DWORK( I1LORI ), D( 1, M1+IJ1 ), 1 )
C
               CALL DLACPY( 'Full', NROW, DIM1, D( I1, IB1 ), LDD,
     $                      DWORK( IWRK ), NROW )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), NROW,
     $                      DWORK( I1UPLE ), SDIM, ZERO, D( I1, IB1 ),
     $                      LDD )
               CALL DAXPY(  NROW, DWORK( I1LOLE ), D( I1, M1+IJ1 ), 1,
     $                      D( I1, IB1 ), 1 )
               CALL DAXPY(  NROW, DWORK( I1LOLE+SDIM ), D( I1, M1+IJ1 ),
     $                      1, D( I1, IB1+1 ), 1 )
               CALL DGEMV(  'No Transpose', NROW, DIM1, ONE,
     $                      DWORK( IWRK ), NROW, DWORK( I1UPRI ), 1,
     $                      DWORK( I1LORI ), D( I1, M1+IJ1 ), 1 )
C
               CALL DLACPY( 'Full', DIM1, M1-IB1+1, D( IB1, IB1 ), LDD,
     $                      DWORK( IWRK ), DIM1 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                      DIM1, ONE, DWORK( I3UPLE ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, D( IB1, IB1 ),
     $                      LDD )
               CALL DAXPY(  M1-IB1+1, DWORK( I3LOLE ), D( M1+IJ1, IB1 ),
     $                      LDD, D( IB1, IB1 ), LDD )
               CALL DAXPY(  M1-IB1+1, DWORK( I3LOLE+SDIM ),
     $                      D( M1+IJ1, IB1 ), LDD, D( IB1+1, IB1 ),
     $                      LDD )
               CALL DGEMV(  'Transpose', DIM1, M1-IB1+1, ONE,
     $                      DWORK( IWRK ), DIM1, DWORK( I3UPRI ), 1,
     $                      DWORK( I3LORI ), D( M1+IJ1, IB1 ), LDD )
C
               CALL DLACPY( 'Full', DIM1, M1-IJ1+1, D( IB1, M1+IJ1 ),
     $                      LDD, DWORK( IWRK ), DIM1 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IJ1+1,
     $                      DIM1, ONE, DWORK( I3UPLE ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, D( IB1, M1+IJ1 ),
     $                      LDD )
               CALL DAXPY(  M1-IJ1+1, DWORK( I3LOLE ),
     $                      D( M1+IJ1, M1+IJ1 ), LDD, D( IB1, M1+IJ1 ),
     $                      LDD )
               CALL DAXPY(  M1-IJ1+1, DWORK( I3LOLE+SDIM ),
     $                      D( M1+IJ1, M1+IJ1 ), LDD,
     $                      D( IB1+1, M1+IJ1 ), LDD )
               CALL DGEMV(  'Transpose', DIM1, M1-IJ1+1, ONE,
     $                      DWORK( IWRK ), DIM1, DWORK( I3UPRI ), 1,
     $                      DWORK( I3LORI ), D( M1+IJ1, M1+IJ1 ), LDD )
C
               IF( M2.GT.0 ) THEN
                  CALL DLACPY( 'Full', DIM1, M4, D( IB1, I2 ), LDD,
     $                         DWORK( IWRK ), DIM1 )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M4,
     $                         DIM1, ONE, DWORK( I3UPLE ), SDIM,
     $                         DWORK( IWRK ), DIM1, ZERO, D( IB1, I2 ),
     $                         LDD )
                  CALL DAXPY(  M4, DWORK( I3LOLE ), D( M1+IJ1, I2 ),
     $                         LDD, D( IB1, I2 ), LDD )
                  CALL DAXPY(  M4, DWORK( I3LOLE+SDIM ),
     $                         D( M1+IJ1, I2 ), LDD, D( IB1+1, I2 ),
     $                         LDD )
                  CALL DGEMV(  'Transpose', DIM1, M4, ONE,
     $                         DWORK( IWRK ), DIM1, DWORK( I3UPRI ), 1,
     $                         DWORK( I3LORI ), D( M1+IJ1, I2 ), LDD )
               END IF
C
C              Update Q1.
C
               IF( LCMPQ1 ) THEN
                  CALL DLACPY( 'Full', N, DIM1, Q1( 1, IB1 ), LDQ1,
     $                         DWORK( IWRK ), N )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                         DIM1, ONE, DWORK( IWRK ), N,
     $                         DWORK( I1UPLE ), SDIM, ZERO,
     $                         Q1( 1, IB1 ), LDQ1 )
                  CALL DAXPY(  N, DWORK( I1LOLE ), Q1( 1, M1+IJ1 ), 1,
     $                         Q1( 1, IB1 ), 1 )
                  CALL DAXPY(  N, DWORK( I1LOLE+SDIM ), Q1( 1, M1+IJ1 ),
     $                         1, Q1( 1, IB1+1 ), 1 )
                  CALL DGEMV(  'No Transpose', N, DIM1, ONE,
     $                         DWORK( IWRK ), N, DWORK( I1UPRI ), 1, 
     $                         DWORK( I1LORI ), Q1( 1, M1+IJ1 ), 1 )
               END IF
C
C              Update Q2.
C
               IF( LCMPQ2 ) THEN
                  CALL DLACPY( 'Full', N, DIM1, Q2( 1, IB1 ), LDQ2,
     $                         DWORK( IWRK ), N )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                         DIM1, ONE, DWORK( IWRK ), N,
     $                         DWORK( I2UPLE ), SDIM, ZERO,
     $                         Q2( 1, IB1 ), LDQ2 )
                  CALL DAXPY(  N, DWORK( I2LOLE ), Q2( 1, M1+IJ1 ), 1,
     $                         Q2( 1, IB1 ), 1 )
                  CALL DAXPY(  N, DWORK( I2LOLE+SDIM ), Q2( 1, M1+IJ1 ),
     $                         1, Q2( 1, IB1+1 ), 1 )
                  CALL DGEMV(  'No Transpose', N, DIM1, ONE,
     $                         DWORK( IWRK ), N, DWORK( I2UPRI ), 1, 
     $                         DWORK( I2LORI ), Q2( 1, M1+IJ1 ), 1 )
               END IF
C
C              Update Q3.
C
               IF( LCMPQ3 ) THEN
                  CALL DLACPY( 'Full', N, DIM1, Q3( 1, IB1 ), LDQ3,
     $                         DWORK( IWRK ), N )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                         DIM1, ONE, DWORK( IWRK ), N,
     $                         DWORK( I3UPLE ), SDIM, ZERO,
     $                         Q3( 1, IB1 ), LDQ3 )
                  CALL DAXPY(  N, DWORK( I3LOLE ), Q3( 1, M1+IJ1 ), 1,
     $                         Q3( 1, IB1 ), 1 )
                  CALL DAXPY(  N, DWORK( I3LOLE+SDIM ), Q3( 1, M1+IJ1 ),
     $                         1, Q3( 1, IB1+1 ), 1 )
                  CALL DGEMV(  'No Transpose', N, DIM1, ONE,
     $                         DWORK( IWRK ), N, DWORK( I3UPRI ), 1, 
     $                         DWORK( I3LORI ), Q3( 1, M1+IJ1 ), 1 )
               END IF
C
            ELSE
C
C              Update A.
C
               CALL DCOPY( NR, A( 1, IB1 ), 1, DWORK( IWRK ), 1 )
               CALL DSCAL( NR, DWORK( I2UPLE ), A( 1, IB1 ), 1 )
               CALL DAXPY( NR-1, DWORK( I2LOLE ), A( 1, M1+IJ1 ), 1,
     $                     A( 1, IB1 ), 1 )
               CALL DSCAL( NR-1, DWORK( I2LORI ), A( 1, M1+IJ1 ), 1 )
               CALL DAXPY( NR-1, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                     A( 1, M1+IJ1 ), 1 )
               A( NR, M1+IJ1 ) = DWORK( I2UPRI )*DWORK( IWRK+NR-1 )
C
               CALL DCOPY( NROW, A( I1, IB1 ), 1, DWORK( IWRK ), 1 )
               CALL DSCAL( NROW, DWORK( I2UPLE ), A( I1, IB1 ), 1 )
               CALL DAXPY( NROW, DWORK( I2LOLE ), A( I1, M1+IJ1 ), 1,
     $                     A( I1, IB1 ), 1 )
               CALL DSCAL( NROW, DWORK( I2LORI ), A( I1, M1+IJ1 ), 1 )
               CALL DAXPY( NROW, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                     A( I1, M1+IJ1 ), 1 )
C
               CALL DCOPY( M1-IB1+1, A( IB1, IB1 ), LDA, DWORK( IWRK ),
     $                     1 )
               CALL DSCAL( M1-IB1+1, DWORK( I3UPLE ), A( IB1, IB1 ),
     $                     LDA )
               CALL DAXPY( M1-IB1+1, DWORK( I3LOLE ), A( M1+IJ1, IB1 ),
     $                     LDA, A( IB1, IB1 ), LDA )
               CALL DSCAL( M1-IB1+1, DWORK( I3LORI ), A( M1+IJ1, IB1 ),
     $                     LDA )
               CALL DAXPY( M1-IB1+1, DWORK( I3UPRI ), DWORK( IWRK ), 1,
     $                     A( M1+IJ1, IB1 ), LDA )
C
               CALL DCOPY( M1-IJ1+1, A( IB1, M1+IJ1 ), LDA,
     $                     DWORK( IWRK ), 1 )
               CALL DSCAL( M1-IJ1+1, DWORK( I3UPLE ), A( IB1, M1+IJ1 ),
     $                     LDA )
               CALL DAXPY( M1-IJ1+1, DWORK( I3LOLE ),
     $                     A( M1+IJ1, M1+IJ1 ), LDA, A( IB1, M1+IJ1 ),
     $                     LDA )
               CALL DSCAL( M1-IJ1+1, DWORK( I3LORI ),
     $                     A( M1+IJ1, M1+IJ1 ), LDA )
               CALL DAXPY( M1-IJ1+1, DWORK( I3UPRI ), DWORK( IWRK ), 1,
     $                     A( M1+IJ1, M1+IJ1 ), LDA )
C
               IF( M2.GT.0 ) THEN
                  CALL DCOPY( M4, A( IB1, I2 ), LDA, DWORK( IWRK ), 1 )
                  CALL DSCAL( M4, DWORK( I3UPLE ), A( IB1, I2 ), LDA )
                  CALL DAXPY( M4, DWORK( I3LOLE ), A( M1+IJ1, I2 ), LDA,
     $                        A( IB1, I2 ), LDA )
                  CALL DSCAL( M4, DWORK( I3LORI ), A( M1+IJ1, I2 ),
     $                        LDA )
                  CALL DAXPY( M4, DWORK( I3UPRI ), DWORK( IWRK ), 1,
     $                        A( M1+IJ1, I2 ), LDA )
               END IF
C
C              Update B.
C
               CALL DCOPY( NR, B( 1, IB1 ), 1, DWORK( IWRK ), 1 )
               CALL DSCAL( NR, DWORK( I1UPLE ), B( 1, IB1 ), 1 )
               CALL DAXPY( NR-1, DWORK( I1LOLE ), B( 1, M1+IJ1 ), 1,
     $                     B( 1, IB1 ), 1 )
               CALL DSCAL( NR-1, DWORK( I1LORI ), B( 1, M1+IJ1 ), 1 )
               CALL DAXPY( NR-1, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                     B( 1, M1+IJ1 ), 1 )
               B( NR, M1+IJ1 ) = DWORK( I1UPRI )*DWORK( IWRK+NR-1 )
C
               CALL DCOPY( NROW, B( I1, IB1 ), 1, DWORK( IWRK ), 1 )
               CALL DSCAL( NROW, DWORK( I1UPLE ), B( I1, IB1 ), 1 )
               CALL DAXPY( NROW, DWORK( I1LOLE ), B( I1, M1+IJ1 ), 1,
     $                     B( I1, IB1 ), 1 )
               CALL DSCAL( NROW, DWORK( I1LORI ), B( I1, M1+IJ1 ), 1 )
               CALL DAXPY( NROW, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                     B( I1, M1+IJ1 ), 1 )
C
               CALL DCOPY( M1-IB1+1, B( IB1, IB1 ), LDB, DWORK( IWRK ),
     $                     1 )
               CALL DSCAL( M1-IB1+1, DWORK( I2UPLE ), B( IB1, IB1 ),
     $                     LDB )
               CALL DAXPY( M1-IB1+1, DWORK( I2LOLE ), B( M1+IJ1, IB1 ),
     $                     LDB, B( IB1, IB1 ), LDB )
               CALL DSCAL( M1-IB1+1, DWORK( I2LORI ), B( M1+IJ1, IB1 ),
     $                     LDB )
               CALL DAXPY( M1-IB1+1, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                     B( M1+IJ1, IB1 ), LDB )
C
               CALL DCOPY( M1-IJ1+1, B( IB1, M1+IJ1 ), LDB,
     $                     DWORK( IWRK ), 1 )
               CALL DSCAL( M1-IJ1+1, DWORK( I2UPLE ), B( IB1, M1+IJ1 ),
     $                     LDB )
               CALL DAXPY( M1-IJ1+1, DWORK( I2LOLE ),
     $                     B( M1+IJ1, M1+IJ1 ), LDB, B( IB1, M1+IJ1 ),
     $                     LDB )
               CALL DSCAL( M1-IJ1+1, DWORK( I2LORI ),
     $                     B( M1+IJ1, M1+IJ1 ), LDB )
               CALL DAXPY( M1-IJ1+1, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                     B( M1+IJ1, M1+IJ1 ), LDB )
C
               IF( M2.GT.0 ) THEN
                  CALL DCOPY( M4, B( IB1, I2 ), LDB, DWORK( IWRK ), 1 )
                  CALL DSCAL( M4, DWORK( I2UPLE ), B( IB1, I2 ), LDB )
                  CALL DAXPY( M4, DWORK( I2LOLE ), B( M1+IJ1, I2 ), LDB,
     $                        B( IB1, I2 ), LDB )
                  CALL DSCAL( M4, DWORK( I2LORI ), B( M1+IJ1, I2 ),
     $                        LDB )
                  CALL DAXPY( M4, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                        B( M1+IJ1, I2 ), LDB )
               END IF
C
C              Update D.
C
               CALL DCOPY( NR, D( 1, IB1 ), 1, DWORK( IWRK ), 1 )
               CALL DSCAL( NR, DWORK( I1UPLE ), D( 1, IB1 ), 1 )
               CALL DAXPY( NR-1, DWORK( I1LOLE ), D( 1, M1+IJ1 ), 1,
     $                     D( 1, IB1 ), 1 )
               CALL DSCAL( NR-1, DWORK( I1LORI ), D( 1, M1+IJ1 ), 1 )
               CALL DAXPY( NR-1, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                     D( 1, M1+IJ1 ), 1 )
               D( NR, M1+IJ1 ) = DWORK( I1UPRI )*DWORK( IWRK+NR-1 )
C
               CALL DCOPY( NROW, D( I1, IB1 ), 1, DWORK( IWRK ), 1 )
               CALL DSCAL( NROW, DWORK( I1UPLE ), D( I1, IB1 ), 1 )
               CALL DAXPY( NROW, DWORK( I1LOLE ), D( I1, M1+IJ1 ), 1,
     $                     D( I1, IB1 ), 1 )
               CALL DSCAL( NROW, DWORK( I1LORI ), D( I1, M1+IJ1 ), 1 )
               CALL DAXPY( NROW, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                     D( I1, M1+IJ1 ), 1 )
C
               CALL DCOPY( M1-IB1+1, D( IB1, IB1 ), LDD, DWORK( IWRK ),
     $                     1 )
               CALL DSCAL( M1-IB1+1, DWORK( I3UPLE ), D( IB1, IB1 ),
     $                     LDD )
               CALL DAXPY( M1-IB1+1, DWORK( I3LOLE ), D( M1+IJ1, IB1 ),
     $                     LDD, D( IB1, IB1 ), LDD )
               CALL DSCAL( M1-IB1+1, DWORK( I3LORI ), D( M1+IJ1, IB1 ),
     $                     LDD )
               CALL DAXPY( M1-IB1+1, DWORK( I3UPRI ), DWORK( IWRK ), 1,
     $                     D( M1+IJ1, IB1 ), LDD )
C
               CALL DCOPY( M1-IJ1+1, D( IB1, M1+IJ1 ), LDD,
     $                     DWORK( IWRK ), 1 )
               CALL DSCAL( M1-IJ1+1, DWORK( I3UPLE ), D( IB1, M1+IJ1 ),
     $                     LDD )
               CALL DAXPY( M1-IJ1+1, DWORK( I3LOLE ),
     $                     D( M1+IJ1, M1+IJ1 ), LDD, D( IB1, M1+IJ1 ),
     $                     LDD )
               CALL DSCAL( M1-IJ1+1, DWORK( I3LORI ),
     $                     D( M1+IJ1, M1+IJ1 ), LDD )
               CALL DAXPY( M1-IJ1+1, DWORK( I3UPRI ), DWORK( IWRK ), 1,
     $                     D( M1+IJ1, M1+IJ1 ), LDD )
C
               IF( M2.GT.0 ) THEN
                  CALL DCOPY( M4, D( IB1, I2 ), LDD, DWORK( IWRK ), 1 )
                  CALL DSCAL( M4, DWORK( I3UPLE ), D( IB1, I2 ), LDD )
                  CALL DAXPY( M4, DWORK( I3LOLE ), D( M1+IJ1, I2 ), LDD,
     $                        D( IB1, I2 ), LDD )
                  CALL DSCAL( M4, DWORK( I3LORI ), D( M1+IJ1, I2 ),
     $                        LDD )
                  CALL DAXPY( M4, DWORK( I3UPRI ), DWORK( IWRK ), 1,
     $                        D( M1+IJ1, I2 ), LDD )
               END IF
C
C              Update Q1.
C
               IF( LCMPQ1 ) THEN
                  CALL DCOPY( N, Q1( 1, IB1 ), 1, DWORK( IWRK ), 1 )
                  CALL DSCAL( N, DWORK( I1UPLE ), Q1( 1, IB1 ),  1 )
                  CALL DAXPY( N, DWORK( I1LOLE ), Q1( 1, M1+IJ1 ), 1,
     $                        Q1( 1, IB1 ), 1 )
                  CALL DSCAL( N, DWORK( I1LORI ), Q1( 1, M1+IJ1 ), 1 )
                  CALL DAXPY( N, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                        Q1( 1, M1+IJ1 ), 1 )
               END IF
C
C              Update Q2.
C
               IF( LCMPQ2 ) THEN
                  CALL DCOPY( N, Q2( 1, IB1 ), 1, DWORK( IWRK ), 1 )
                  CALL DSCAL( N, DWORK( I2UPLE ), Q2( 1, IB1 ),  1 )
                  CALL DAXPY( N, DWORK( I2LOLE ), Q2( 1, M1+IJ1 ), 1,
     $                        Q2( 1, IB1 ), 1 )
                  CALL DSCAL( N, DWORK( I2LORI ), Q2( 1, M1+IJ1 ), 1 )
                  CALL DAXPY( N, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                        Q2( 1, M1+IJ1 ), 1 )
               END IF
C
C              Update Q3.
C
               IF( LCMPQ3 ) THEN
                  CALL DCOPY( N, Q3( 1, IB1 ), 1, DWORK( IWRK ), 1 )
                  CALL DSCAL( N, DWORK( I3UPLE ), Q3( 1, IB1 ),  1 )
                  CALL DAXPY( N, DWORK( I3LOLE ), Q3( 1, M1+IJ1 ), 1,
     $                        Q3( 1, IB1 ), 1 )
                  CALL DSCAL( N, DWORK( I3LORI ), Q3( 1, M1+IJ1 ), 1 )
                  CALL DAXPY( N, DWORK( I3UPRI ), DWORK( IWRK ), 1,
     $                        Q3( 1, M1+IJ1 ), 1 )
               END IF
            END IF
   50    CONTINUE
   60 CONTINUE
C
C     Triangularize the lower right subpencil aAA2 BB2 - bDD2.
C
      IF( M2.GT.1 ) THEN
         CALL DLACPY( 'Full', N, M4-2, A( 1, I2+1 ), LDA, DWORK, N )
         DO 70 I = 1, M2 - 1
            CALL DCOPY( N, DWORK( N*( I-1 )+1 ), 1,
     $                  A( 1, 2*( M1+I )+1 ), 1 )
            CALL DCOPY( N, DWORK( N*( M2+I-2 )+1 ), 1,
     $                  A( 1, 2*( M1+I ) ), 1 )
   70    CONTINUE
C
         CALL DLACPY( 'Full', M4-2, M4, A( I2+1, I2 ), LDA, DWORK,
     $                M4-2 )
         DO 80 I = 1, M2 - 1
            CALL DCOPY( M4, DWORK( I ), M4-2, A( 2*( M1+I )+1, I2 ),
     $                  LDA )
            CALL DCOPY( M4, DWORK( M2+I-1 ), M4-2, A( 2*( M1+I ), I2 ),
     $                  LDA )
   80    CONTINUE
C
         CALL DLACPY( 'Full', N, M4-2, B( 1, I2+1 ), LDB, DWORK, N )
         DO 90 I = 1, M2 - 1
            CALL DCOPY( N, DWORK( N*( I-1 )+1 ), 1,
     $                  B( 1, 2*( M1+I )+1 ), 1 )
            CALL DCOPY( N, DWORK( N*( M2+I-2 )+1 ), 1,
     $                  B( 1, 2*( M1+I ) ), 1 )
   90    CONTINUE
C
         CALL DLACPY( 'Full', M4-2, M4, B( I2+1, I2 ), LDB, DWORK,
     $                M4-2 )
         DO 100 I = 1, M2 - 1
            CALL DCOPY( M4, DWORK( I ), M4-2, B( 2*( M1+I )+1, I2 ),
     $                  LDB )
            CALL DCOPY( M4, DWORK( M2+I-1 ), M4-2, B( 2*( M1+I ), I2 ),
     $                  LDB )
  100    CONTINUE
C
         CALL DLACPY( 'Full', N, M4-2, D( 1, I2+1 ), LDD, DWORK, N )
         DO 110 I = 1, M2 - 1
            CALL DCOPY( N, DWORK( N*( I-1 )+1 ), 1,
     $                  D( 1, 2*( M1+I )+1 ), 1 )
            CALL DCOPY( N, DWORK( N*( M2+I-2 )+1 ), 1,
     $                  D( 1, 2*( M1+I ) ), 1 )
  110    CONTINUE
C
         CALL DLACPY( 'Full', M4-2, M4, D( I2+1, I2 ), LDD, DWORK,
     $                M4-2 )
         DO 120 I = 1, M2 - 1
            CALL DCOPY( M4, DWORK( I ), M4-2, D( 2*( M1+I )+1, I2 ),
     $                  LDD )
            CALL DCOPY( M4, DWORK( M2+I-1 ), M4-2, D( 2*( M1+I ), I2 ),
     $                  LDD )
  120    CONTINUE
C
         IF( LCMPQ1 ) THEN
            CALL DLACPY( 'Full', N, M4-2, Q1( 1, I2+1 ), LDQ1, DWORK,
     $                    N )
            DO 130 I = 1, M2 - 1
               CALL DCOPY( N, DWORK( N*( I-1 )+1 ), 1,
     $                     Q1( 1, 2*( M1+I )+1 ), 1 )
               CALL DCOPY( N, DWORK( N*( M2+I-2 )+1 ), 1,
     $                     Q1( 1, 2*( M1+I ) ), 1 )
  130       CONTINUE
         END IF
C
         IF( LCMPQ2 ) THEN
            CALL DLACPY( 'Full', N, M4-2, Q2( 1, I2+1 ), LDQ2, DWORK,
     $                    N )
            DO 140 I = 1, M2 - 1
               CALL DCOPY( N, DWORK( N*( I-1 )+1 ), 1,
     $                     Q2( 1, 2*( M1+I )+1 ), 1 )
               CALL DCOPY( N, DWORK( N*( M2+I-2 )+1 ), 1,
     $                     Q2( 1, 2*( M1+I ) ), 1 )
  140       CONTINUE
         END IF
C
         IF( LCMPQ3 ) THEN
            CALL DLACPY( 'Full', N, M4-2, Q3( 1, I2+1 ), LDQ3, DWORK,
     $                    N )
            DO 150 I = 1, M2 - 1
               CALL DCOPY( N, DWORK( N*( I-1 )+1 ), 1,
     $                     Q3( 1, 2*( M1+I )+1 ), 1 )
               CALL DCOPY( N, DWORK( N*( M2+I-2 )+1 ), 1,
     $                     Q3( 1, 2*( M1+I ) ), 1 )
  150       CONTINUE
         END IF
      END IF
C
      RETURN
C *** Last line of MB04CD ***
      END
 
