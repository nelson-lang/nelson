      SUBROUTINE TG01PD( DICO, STDOM, JOBAE, COMPQ, COMPZ, N, M, P,
     $                   NLOW, NSUP, ALPHA, A, LDA, E, LDE, B, LDB,
     $                   C, LDC, Q, LDQ, Z, LDZ, NDIM, ALPHAR, ALPHAI,
     $                   BETA, DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To compute orthogonal transformation matrices Q and Z which
C     reduce the regular pole pencil A-lambda*E of the descriptor system
C     (A-lambda*E,B,C) to the generalized real Schur form with ordered
C     generalized eigenvalues. The pair (A,E) is reduced to the form
C
C                ( *  *  *  * )             ( *  *  *  * )
C                (            )             (            )
C                ( 0  A1 *  * )             ( 0  E1 *  * )
C       Q'*A*Z = (            ) ,  Q'*E*Z = (            ) ,
C                ( 0  0  A2 * )             ( 0  0  E2 * )
C                (            )             (            )
C                ( 0  0  0  * )             ( 0  0  0  * )
C
C     where the subpencil A1-lambda*E1 contains the eigenvalues which
C     belong to a suitably defined domain of interest and the subpencil
C     A2-lambda*E2 contains the eigenvalues which are outside of the
C     domain of interest.
C     If JOBAE = 'S', the pair (A,E) is assumed to be already in a
C     generalized real Schur form and the reduction is performed only
C     on the subpencil A12 - lambda*E12 defined by rows and columns
C     NLOW to NSUP of A - lambda*E.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     DICO    CHARACTER*1
C             Specifies the type of the descriptor system as follows:
C             = 'C':  continuous-time system;
C             = 'D':  discrete-time system.
C
C     STDOM   CHARACTER*1
C             Specifies whether the domain of interest is of stability
C             type (left part of complex plane or inside of a circle)
C             or of instability type (right part of complex plane or
C             outside of a circle) as follows:
C             = 'S':  stability type domain;
C             = 'U':  instability type domain.
C
C     JOBAE   CHARACTER*1
C             Specifies the shape of the matrix pair (A,E) on entry
C             as follows:
C             = 'S':  (A,E) is in a generalized real Schur form;
C             = 'G':  A and E are general square dense matrices.
C
C     COMPQ   CHARACTER*1
C             = 'I':  Q is initialized to the unit matrix, and the
C                     orthogonal matrix Q is returned;
C             = 'U':  Q must contain an orthogonal matrix Q1 on entry,
C                     and the product Q1*Q is returned.
C                     This option can not be used when JOBAE = 'G'.
C
C     COMPZ   CHARACTER*1
C             = 'I':  Z is initialized to the unit matrix, and the
C                     orthogonal matrix Z is returned;
C             = 'U':  Z must contain an orthogonal matrix Z1 on entry,
C                     and the product Z1*Z is returned.
C                     This option can not be used when JOBAE = 'G'.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The number of rows of the matrix B, the number of columns
C             of the matrix C, and the order of the square matrices A
C             and E.  N >= 0.
C
C     M       (input) INTEGER
C             The number of columns of the matrix B.  M >= 0.
C
C     P       (input) INTEGER
C             The number of rows of the matrix C.  P >= 0.
C
C     NLOW,   (input) INTEGER
C     NSUP    (input) INTEGER
C             NLOW and NSUP specify the boundary indices for the rows
C             and columns of the principal subpencil of A - lambda*E
C             whose diagonal blocks are to be reordered.
C             0 <= NLOW <= NSUP <= N,       if JOBAE = 'S'.
C             NLOW = MIN( 1, N ), NSUP = N, if JOBAE = 'G'.
C
C     ALPHA   (input) DOUBLE PRECISION
C             The boundary of the domain of interest for the generalized
C             eigenvalues of the pair (A,E). For a continuous-time
C             system (DICO = 'C'), ALPHA is the boundary value for the
C             real parts of the generalized eigenvalues, while for a
C             discrete-time system (DICO = 'D'), ALPHA >= 0 represents
C             the boundary value for the moduli of the generalized
C             eigenvalues.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the state dynamics matrix A.
C             If JOBAE = 'S' then A must be a matrix in real Schur form.
C             On exit, the leading N-by-N part of this array contains
C             the matrix Q'*A*Z in real Schur form, with the elements
C             below the first subdiagonal set to zero.
C             The leading NDIM-by-NDIM part of the principal subpencil
C             A12 - lambda*E12, defined by A12 := A(NLOW:NSUP,NLOW:NSUP)
C             and E12 := E(NLOW:NSUP,NLOW:NSUP), has generalized
C             eigenvalues in the domain of interest, and the trailing
C             part of this subpencil has generalized eigenvalues outside
C             the domain of interest.
C             The domain of interest for eig(A12,E12), the generalized
C             eigenvalues of the pair (A12,E12), is defined by the
C             parameters ALPHA, DICO and STDOM as follows:
C               For DICO = 'C':
C                  Real(eig(A12,E12)) < ALPHA if STDOM = 'S';
C                  Real(eig(A12,E12)) > ALPHA if STDOM = 'U'.
C               For DICO = 'D':
C                  Abs(eig(A12,E12)) < ALPHA if STDOM = 'S';
C                  Abs(eig(A12,E12)) > ALPHA if STDOM = 'U'.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     E       (input/output) DOUBLE PRECISION array, dimension (LDE,N)
C             On entry, the leading N-by-N part of this array must
C             contain the descriptor matrix E.
C             If JOBAE = 'S', then E must be an upper triangular matrix.
C             On exit, the leading N-by-N part of this array contains an
C             upper triangular matrix Q'*E*Z, with the elements below
C             the diagonal set to zero.
C             The leading NDIM-by-NDIM part of the principal subpencil
C             A12 - lambda*E12 (see description of A) has generalized
C             eigenvalues in the domain of interest, and the trailing
C             part of this subpencil has generalized eigenvalues outside
C             the domain of interest.
C
C     LDE     INTEGER
C             The leading dimension of the array E.  LDE >= MAX(1,N).
C
C     B       (input/output) DOUBLE PRECISION array, dimension (LDB,M)
C             On entry, the leading N-by-M part of this array must
C             contain the input matrix B.
C             On exit, the leading N-by-M part of this array contains
C             the transformed input matrix Q'*B.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= MAX(1,N).
C
C     C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
C             On entry, the leading P-by-N part of this array must
C             contain the output matrix C.
C             On exit, the leading P-by-N part of this array contains
C             the transformed output matrix C*Z.
C
C     LDC     INTEGER
C             The leading dimension of the array C.  LDC >= MAX(1,P).
C
C     Q       (input/output) DOUBLE PRECISION array, dimension (LDQ,N)
C             If COMPQ = 'I':  on entry, Q need not be set;
C                              on exit, the leading N-by-N part of this
C                              array contains the orthogonal matrix Q,
C                              where Q' is the product of orthogonal
C                              transformations which are applied to A,
C                              E, and B on the left.
C             If COMPQ = 'U':  on entry, the leading N-by-N part of this
C                              array must contain an orthogonal matrix
C                              Q1;
C                              on exit, the leading N-by-N part of this
C                              array contains the orthogonal matrix
C                              Q1*Q.
C
C     LDQ     INTEGER
C             The leading dimension of the array Q. LDQ >= MAX(1,N).
C
C     Z       (input/output) DOUBLE PRECISION array, dimension (LDZ,N)
C             If COMPZ = 'I':  on entry, Z need not be set;
C                              on exit, the leading N-by-N part of this
C                              array contains the orthogonal matrix Z,
C                              which is the product of orthogonal
C                              transformations applied to A, E, and C
C                              on the right.
C             If COMPZ = 'U':  on entry, the leading N-by-N part of this
C                              array must contain an orthogonal matrix
C                              Z1;
C                              on exit, the leading N-by-N part of this
C                              array contains the orthogonal matrix
C                              Z1*Z.
C
C     LDZ     INTEGER
C             The leading dimension of the array Z. LDZ >= MAX(1,N).
C
C     NDIM    (output) INTEGER
C             The number of generalized eigenvalues of the principal
C             subpencil A12 - lambda*E12 (see description of A) lying
C             inside the domain of interest for eigenvalues.
C
C     ALPHAR  (output) DOUBLE PRECISION array, dimension (N)
C     ALPHAI  (output) DOUBLE PRECISION array, dimension (N)
C     BETA    (output) DOUBLE PRECISION array, dimension (N)
C             On exit, (ALPHAR(j) + ALPHAI(j)*i)/BETA(j), j=1,...,N,
C             are the generalized eigenvalues.
C             ALPHAR(j) + ALPHAI(j)*i, and BETA(j), j = 1,...,N, are the
C             diagonals of the complex Schur form (S,T) that would
C             result if the 2-by-2 diagonal blocks of the real Schur
C             form of (A,B) were further reduced to triangular form
C             using 2-by-2 complex unitary transformations.
C             If ALPHAI(j) is zero, then the j-th eigenvalue is real;
C             if positive, then the j-th and (j+1)-st eigenvalues are a
C             complex conjugate pair, with ALPHAI(j+1) negative.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The length of the array DWORK.
C             LDWORK >= 8*N+16, if JOBAE = 'G';
C             LDWORK >= 4*N+16, if JOBAE = 'S'.
C             For optimum performance LDWORK should be larger.
C
C             If LDWORK = -1, then a workspace query is assumed; the
C             routine only calculates the optimal size of the DWORK
C             array, returns this value as the first entry of the DWORK
C             array, and no error message related to LDWORK is issued by
C             XERBLA.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value;
C             = 1:  the QZ algorithm failed to compute all generalized
C                   eigenvalues of the pair (A,E);
C             = 2:  a failure occured during the ordering of the
C                   generalized real Schur form of the pair (A,E).
C
C     METHOD
C
C     If JOBAE = 'G', the pair (A,E) is reduced to an ordered
C     generalized real Schur form using an orthogonal equivalence
C     transformation A <-- Q'*A*Z and E <-- Q'*E*Z. This transformation
C     is determined so that the leading diagonal blocks of the resulting
C     pair (A,E) have generalized eigenvalues in a suitably defined
C     domain of interest. Then, the transformations are applied to the
C     matrices B and C: B <-- Q'*B and C <-- C*Z.
C     If JOBAE = 'S', then the diagonal blocks of the subpencil
C     A12 - lambda*E12, defined by A12 := A(NLOW:NSUP,NLOW:NSUP)
C     and E12 := E(NLOW:NSUP,NLOW:NSUP), are reordered using orthogonal
C     equivalence transformations, such that the leading blocks have
C     generalized eigenvalues in a suitably defined domain of interest.
C
C     NUMERICAL ASPECTS
C                                     3
C     The algorithm requires about 25N  floating point operations.
C
C     CONTRIBUTOR
C
C     A. Varga, German Aerospace Center,
C     DLR Oberpfaffenhofen, October 2002.
C     Based on the RASP routine SRSFOD.
C
C     REVISIONS
C
C     V. Sima, Dec. 2016.
C
C     KEYWORDS
C
C     Deflating subspace, orthogonal transformation,
C     generalized real Schur form, equivalence transformation.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION ZERO, ONE
      PARAMETER        ( ZERO = 0.0D0, ONE = 1.0D0 )
C     .. Scalar Arguments ..
      CHARACTER        COMPQ, COMPZ, DICO, JOBAE, STDOM
      INTEGER          INFO, LDA, LDB, LDC, LDE, LDQ, LDWORK, LDZ, M, N,
     $                 NDIM, NLOW, NSUP, P
      DOUBLE PRECISION ALPHA
C     .. Array Arguments ..
      DOUBLE PRECISION A(LDA,*), ALPHAI(*), ALPHAR(*), B(LDB,*),
     $                 BETA(*),  C(LDC,*),  DWORK(*),  E(LDE,*),
     $                 Q(LDQ,*), Z(LDZ,*)
C     .. Local Scalars ..
      LOGICAL          DISCR, LJOBG, LQUERY
      INTEGER          I, ICOMPQ, ICOMPZ, LW, MINWRK, NB, NBC, NC, NR,
     $                 SDIM, WRKOPT
C     .. Local Arrays ..
      LOGICAL          BWORK(1)
C     .. External Functions ..
      LOGICAL          DELCTG, LSAME
      EXTERNAL         DELCTG, LSAME
C     .. External Subroutines ..
      EXTERNAL         DCOPY, DGEMM, DGEQRF, DGGES, DLACPY, DLASET,
     $                 MB03QG, MB03QV, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC        INT, MAX, MIN
C
C     .. Executable Statements ..
C
C     Decode COMPQ.
C
      IF( LSAME( COMPQ, 'I' ) ) THEN
         ICOMPQ = 1
      ELSE IF( LSAME( COMPQ, 'U' ) ) THEN
         ICOMPQ = 2
      ELSE
         ICOMPQ = 0
      END IF
C
C     Decode COMPZ.
C
      IF( LSAME( COMPZ, 'I' ) ) THEN
         ICOMPZ = 1
      ELSE IF( LSAME( COMPZ, 'U' ) ) THEN
         ICOMPZ = 2
      ELSE
         ICOMPZ = 0
      END IF
C
      INFO  = 0
      DISCR = LSAME( DICO,  'D' )
      LJOBG = LSAME( JOBAE, 'G' )
C
C     Check input scalar arguments.
C
      IF( .NOT. ( LSAME( DICO, 'C' ) .OR. DISCR ) ) THEN
         INFO = -1
      ELSE IF( .NOT. ( LSAME( STDOM, 'S' ) .OR.
     $                 LSAME( STDOM, 'U' ) ) ) THEN
         INFO = -2
      ELSE IF( .NOT. ( LSAME( JOBAE, 'S' ) .OR. LJOBG ) ) THEN
         INFO = -3
      ELSE IF( ICOMPQ.LE.0 .OR. ( LJOBG .AND. ICOMPQ.EQ.2 ) ) THEN
         INFO = -4
      ELSE IF( ICOMPZ.LE.0 .OR. ( LJOBG .AND. ICOMPZ.EQ.2 ) ) THEN
         INFO = -5
      ELSE IF( N.LT.0 ) THEN
         INFO = -6
      ELSE IF( M.LT.0 ) THEN
         INFO = -7
      ELSE IF( P.LT.0 ) THEN
         INFO = -8
      ELSE IF(    ( LJOBG .AND. NLOW.NE.MIN( 1, N ) ) .OR.
     $       ( .NOT.LJOBG .AND. NLOW.LT.0 ) )  THEN
         INFO = -9
      ELSE IF( ( LJOBG .AND. NSUP.NE.N )  .OR. ( .NOT.LJOBG .AND.
     $         ( NSUP.LT.NLOW .OR. N.LT.NSUP ) ) ) THEN
         INFO = -10
      ELSE IF( DISCR .AND. ALPHA.LT.ZERO ) THEN
         INFO = -11
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -13
      ELSE IF( LDE.LT.MAX( 1, N ) ) THEN
         INFO = -15
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -17
      ELSE IF( LDC.LT.MAX( 1, P ) ) THEN
         INFO = -19
      ELSE IF( LDQ.LT.MAX( 1, N ) ) THEN
         INFO = -21
      ELSE IF( LDZ.LT.MAX( 1, N ) ) THEN
         INFO = -23
      ELSE
         IF( N.EQ.0 ) THEN
            MINWRK = 1
         ELSE IF( LJOBG ) THEN
            MINWRK = 8*N + 16
         ELSE
            MINWRK = 4*N + 16
         END IF
         LQUERY = LDWORK.EQ.-1
C
C        Estimate the optimal block size.
C
         CALL DGEQRF( N, MAX( M, P ), A, LDA, DWORK, DWORK, -1, INFO )
         NB = INT( DWORK(1) )/MAX( 1, M, P )
         LW = MIN( NB*NB, N*MAX( M, P ) )
C
         IF( LQUERY ) THEN
            WRKOPT = MINWRK
            IF( LJOBG ) THEN
               CALL DGGES( 'Vectors', 'Vectors', 'Not ordered', DELCTG,
     $                     N, A, LDA, E, LDE, SDIM, ALPHAR, ALPHAI,
     $                     BETA, Q, LDQ, Z, LDZ, DWORK, -1, BWORK,
     $                     INFO )
               WRKOPT = MAX( WRKOPT, INT( DWORK(1) ) )
            END IF
            CALL MB03QG( DICO, STDOM, 'Update', 'Update', N, NLOW, NSUP,
     $                   ALPHA, A, LDA, E, LDE, Q, LDQ, Z, LDZ, NDIM,
     $                   DWORK, -1, INFO )
            WRKOPT = MAX( WRKOPT, LW, INT( DWORK(1) ) )
         ELSE IF( LDWORK.LT.MINWRK ) THEN
            INFO = -29
         END IF
      END IF
C
      IF( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'TG01PD', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         DWORK(1) = WRKOPT
         RETURN
      END IF
C
C     Quick return if possible.
C
      NDIM = 0
      IF( N.EQ.0 ) THEN
         DWORK(1) = ONE
         RETURN
      END IF
C
      IF( LJOBG ) THEN
C
C        Reduce (A,E) to real generalized Schur form using an orthogonal
C        equivalence transformation (A,E) <- (Q'*A*Z,Q'*E*Z), accumulate
C        the transformations in Q and Z, and compute the generalized
C        eigenvalues of the pair (A,E) in (ALPHAR, ALPHAI, BETA).
C
C        Workspace:  need   8*N+16;
C                    prefer larger.
C
         CALL DGGES( 'Vectors', 'Vectors', 'Not ordered', DELCTG, N,
     $               A, LDA, E, LDE, SDIM, ALPHAR, ALPHAI, BETA, Q, LDQ,
     $               Z, LDZ, DWORK, LDWORK, BWORK, INFO )
         IF( INFO.NE.0 ) THEN
            INFO = 1
            RETURN
         END IF
         WRKOPT = MAX( MINWRK, INT( DWORK(1) ) )
      ELSE
C
C        Initialize Q and Z if necessary.
C
         IF( ICOMPQ.EQ.1 )
     $      CALL DLASET( 'Full', N, N, ZERO, ONE, Q, LDQ )
         IF( ICOMPZ.EQ.1 )
     $      CALL DLASET( 'Full', N, N, ZERO, ONE, Z, LDZ )
         WRKOPT = MINWRK
      END IF
C
C     Separate the spectrum of (A,E). The leading NDIM-by-NDIM subpencil
C     of A12-lambda*E12 corresponds to the generalized eigenvalues of
C     interest.
C     Workspace:  need   4*N+16.
C
      CALL MB03QG( DICO, STDOM, 'Update', 'Update', N, NLOW, NSUP,
     $             ALPHA, A, LDA, E, LDE, Q, LDQ, Z, LDZ, NDIM, DWORK,
     $             LDWORK, INFO )
      IF( INFO.NE.0 ) THEN
         INFO = 2
         RETURN
      END IF
      WRKOPT = MAX( WRKOPT, INT( DWORK(1) ) )
C
C     Compute the generalized eigenvalues.
C
      CALL MB03QV( N, A, LDA, E, LDE, ALPHAR, ALPHAI, BETA, INFO )
C
C     Apply the transformation: B <-- Q'*B.
C
      NBC = MAX( 1, MIN( LDWORK/N, M ) )
      DO 10 I = 1, M, NBC
         NC = MIN( NBC, M-I+1 )
         CALL DGEMM( 'Transpose', 'No transpose', N, NC, N, ONE, Q, LDQ,
     $               B(1,I), LDB, ZERO, DWORK, N )
         CALL DLACPY( 'All', N, NC, DWORK, N, B(1,I), LDB )
   10 CONTINUE
C
C     Apply the transformation: C <-- C*Z.
C
      NBC = MAX( 1, MIN( LDWORK/N, P ) )
      DO 20 I = 1, P, NBC
         NR = MIN( NBC, P-I+1 )
         CALL DGEMM( 'No Transpose', 'No transpose', NR, N, N, ONE,
     $              C(I,1), LDC, Z, LDZ, ZERO, DWORK, NR )
         CALL DLACPY( 'All', NR, N, DWORK, NR, C(I,1), LDC )
   20 CONTINUE
C
      DWORK( 1 ) = MAX( WRKOPT, LW )
C
      RETURN
C *** Last line of TG01PD ***
      END
