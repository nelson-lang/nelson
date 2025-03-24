      SUBROUTINE TG01LD( JOB, JOBA, COMPQ, COMPZ, N, M, P, A, LDA,
     $                   E, LDE, B, LDB, C, LDC, Q, LDQ, Z, LDZ, NF, ND,
     $                   NIBLCK, IBLCK, TOL, IWORK, DWORK, LDWORK,
     $                   INFO )
C
C     PURPOSE
C
C     To compute orthogonal transformation matrices Q and Z which
C     reduce the regular pole pencil A-lambda*E of the descriptor system
C     (A-lambda*E,B,C) to the form (if JOB = 'F')
C
C                ( Af  *  )             ( Ef  *  )
C       Q'*A*Z = (        ) ,  Q'*E*Z = (        ) ,                 (1)
C                ( 0   Ai )             ( 0   Ei )
C
C     or to the form (if JOB = 'I')
C
C                ( Ai  *  )             ( Ei  *  )
C       Q'*A*Z = (        ) ,  Q'*E*Z = (        ) ,                 (2)
C                ( 0   Af )             ( 0   Ef )
C
C     where the subpencil Af-lambda*Ef, with Ef nonsingular and upper
C     triangular, contains the finite eigenvalues, and the subpencil
C     Ai-lambda*Ei, with Ai nonsingular and upper triangular, contains
C     the infinite eigenvalues. The subpencil Ai-lambda*Ei is in a
C     staircase form (see METHOD). If JOBA = 'H', the submatrix Af
C     is further reduced to an upper Hessenberg form.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOB     CHARACTER*1
C             = 'F':  perform the finite-infinite separation;
C             = 'I':  perform the infinite-finite separation.
C
C     JOBA    CHARACTER*1
C             = 'H':  reduce Af further to an upper Hessenberg form;
C             = 'N':  keep Af unreduced.
C
C     COMPQ   CHARACTER*1
C             = 'N':  do not compute Q;
C             = 'I':  Q is initialized to the unit matrix, and the
C                     orthogonal matrix Q is returned;
C             = 'U':  Q must contain an orthogonal matrix Q1 on entry,
C                     and the product Q1*Q is returned.
C
C     COMPZ   CHARACTER*1
C             = 'N':  do not compute Z;
C             = 'I':  Z is initialized to the unit matrix, and the
C                     orthogonal matrix Z is returned;
C             = 'U':  Z must contain an orthogonal matrix Z1 on entry,
C                     and the product Z1*Z is returned.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The number of rows of the matrix B, the number of columns
C             of the matrix C and the order of the square matrices A
C             and E.  N >= 0.
C
C     M       (input) INTEGER
C             The number of columns of the matrix B.  M >= 0.
C
C     P       (input) INTEGER
C             The number of rows of the matrix C.  P >= 0.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the N-by-N state matrix A.
C             On exit, the leading N-by-N part of this array contains
C             the transformed state matrix Q'*A*Z,
C
C                                ( Af  *  )                 ( Ai  *  )
C                       Q'*A*Z = (        ) ,  or  Q'*A*Z = (        ) ,
C                                ( 0   Ai )                 ( 0   Af )
C
C             depending on JOB, with Af an NF-by-NF matrix, and Ai an
C             (N-NF)-by-(N-NF) nonsingular and upper triangular matrix.
C             If JOBA = 'H', Af is in an upper Hessenberg form.
C             Otherwise, Af is unreduced.
C             Ai has a block structure as in (3) or (4), where A0,0 is
C             ND-by-ND and Ai,i , for i = 1, ..., NIBLCK, is
C             IBLCK(i)-by-IBLCK(i).
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     E       (input/output) DOUBLE PRECISION array, dimension (LDE,N)
C             On entry, the leading N-by-N part of this array must
C             contain the N-by-N descriptor matrix E.
C             On exit, the leading N-by-N part of this array contains
C             the transformed descriptor matrix Q'*E*Z,
C
C                                ( Ef  *  )                 ( Ei  *  )
C                       Q'*E*Z = (        ) ,  or  Q'*E*Z = (        ) ,
C                                ( 0   Ei )                 ( 0   Ef )
C
C             depending on JOB, with Ef an NF-by-NF nonsingular matrix,
C             and Ei an (N-NF)-by-(N-NF) nilpotent matrix in an upper
C             block triangular form, as in (3) or (4).
C
C     LDE     INTEGER
C             The leading dimension of the array E.  LDE >= MAX(1,N).
C
C     B       (input/output) DOUBLE PRECISION array, dimension (LDB,K),
C             where K = M if JOB = 'F', and K = MAX(M,P) if JOB = 'I'.
C             On entry, the leading N-by-M part of this array must
C             contain the N-by-M input matrix B.
C             On exit, the leading N-by-M part of this array contains
C             the transformed input matrix Q'*B.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= MAX(1,N).
C
C     C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
C             On entry, the leading P-by-N part of this array must
C             contain the state/output matrix C.
C             On exit, the leading P-by-N part of this array contains
C             the transformed matrix C*Z.
C
C     LDC     INTEGER
C             The leading dimension of the array C.  LDC >= MAX(1,K),
C             where K = P if JOB = 'F', and K = MAX(M,P) if JOB = 'I'.
C
C     Q       (input/output) DOUBLE PRECISION array, dimension (LDQ,N)
C             If COMPQ = 'N':  Q is not referenced.
C             If COMPQ = 'I':  on entry, Q need not be set;
C                              on exit, the leading N-by-N part of this
C                              array contains the orthogonal matrix Q,
C                              where Q' is the product of Householder
C                              transformations applied to A, E, and B on
C                              the left.
C             If COMPQ = 'U':  on entry, the leading N-by-N part of this
C                              array must contain an orthogonal matrix
C                              Q1;
C                              on exit, the leading N-by-N part of this
C                              array contains the orthogonal matrix
C                              Q1*Q.
C
C     LDQ     INTEGER
C             The leading dimension of the array Q.
C             LDQ >= 1,        if COMPQ = 'N';
C             LDQ >= MAX(1,N), if COMPQ = 'I' or 'U'.
C
C     Z       (input/output) DOUBLE PRECISION array, dimension (LDZ,N)
C             If COMPZ = 'N':  Z is not referenced.
C             If COMPZ = 'I':  on entry, Z need not be set;
C                              on exit, the leading N-by-N part of this
C                              array contains the orthogonal matrix Z,
C                              which is the product of Householder
C                              transformations applied to A, E, and C on
C                              the right.
C             If COMPZ = 'U':  on entry, the leading N-by-N part of this
C                              array must contain an orthogonal matrix
C                              Z1;
C                              on exit, the leading N-by-N part of this
C                              array contains the orthogonal matrix
C                              Z1*Z.
C
C     LDZ     INTEGER
C             The leading dimension of the array Z.
C             LDZ >= 1,        if COMPZ = 'N';
C             LDZ >= MAX(1,N), if COMPZ = 'I' or 'U'.
C
C     NF      (output) INTEGER.
C             The order of the reduced matrices Af and Ef; also, the
C             number of finite generalized eigenvalues of the pencil
C             A-lambda*E.
C
C     ND      (output) INTEGER.
C             The number of non-dynamic infinite eigenvalues of the
C             pair (A,E). Note: N-ND is the rank of the matrix E.
C
C     NIBLCK  (output) INTEGER
C             If ND > 0, the number of infinite blocks minus one.
C             If ND = 0, then NIBLCK = 0.
C
C     IBLCK   (output) INTEGER array, dimension (N)
C             IBLCK(i) contains the dimension of the i-th block in the
C             staircase form (3) or (4), with i = 1,2, ..., NIBLCK.
C
C     Tolerances
C
C     TOL     DOUBLE PRECISION
C             A tolerance used in rank decisions to determine the
C             effective rank, which is defined as the order of the
C             largest leading (or trailing) triangular submatrix in the
C             QR factorization with column pivoting whose estimated
C             condition number is less than 1/TOL. If the user sets
C             TOL <= 0, then an implicitly computed, default tolerance,
C             TOLDEF = N**2*EPS,  is used instead, where EPS is the
C             machine precision (see LAPACK Library routine DLAMCH).
C             TOL < 1.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (N)
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The length of the array DWORK.  LDWORK >= 1, and if N > 0,
C             LDWORK >= N + MAX(3*N,M,P).
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
C             = 1:  the pencil A-lambda*E is not regular.
C
C     METHOD
C
C     The subroutine is based on the reduction algorithm of [1].
C     If JOB = 'F', the matrices Ai and Ei have the form
C
C           ( A0,0  A0,k ... A0,1 )         ( 0  E0,k ... E0,1 )
C      Ai = (  0    Ak,k ... Ak,1 ) ,  Ei = ( 0   0   ... Ek,1 ) ;   (3)
C           (  :     :    .    :  )         ( :   :    .    :  )
C           (  0     0   ... A1,1 )         ( 0   0   ...   0  )
C
C     if JOB = 'I', the matrices Ai and Ei have the form
C
C           ( A1,1 ... A1,k  A1,0 )         ( 0 ... E1,k  E1,0 )
C      Ai = (  :    .    :    :   ) ,  Ei = ( :  .    :    :   ) ,   (4)
C           (  :   ... Ak,k  Ak,0 )         ( : ...   0   Ek,0 )
C           (  0   ...   0   A0,0 )         ( 0 ...   0     0  )
C
C     where Ai,i , for i = 0, 1, ..., k, are nonsingular upper
C     triangular matrices. A0,0 corresponds to the non-dynamic infinite
C     modes of the system.
C
C     REFERENCES
C
C     [1] Misra, P., Van Dooren, P., and Varga, A.
C         Computation of structural invariants of generalized
C         state-space systems.
C         Automatica, 30, pp. 1921-1936, 1994.
C
C     NUMERICAL ASPECTS
C
C     The algorithm is numerically backward stable and requires
C     0( N**3 )  floating point operations.
C
C     FURTHER COMMENTS
C
C     The number of infinite poles is computed as
C
C                   NIBLCK
C        NINFP =     Sum  IBLCK(i) = N - ND - NF.
C                    i=1
C
C     The multiplicities of infinite poles can be computed as follows:
C     there are IBLCK(k)-IBLCK(k+1) infinite poles of multiplicity
C     k, for k = 1, ..., NIBLCK, where IBLCK(NIBLCK+1) = 0.
C     Note that each infinite pole of multiplicity k corresponds to
C     an infinite eigenvalue of multiplicity k+1.
C
C     CONTRIBUTOR
C
C     A. Varga, German Aerospace Center, DLR Oberpfaffenhofen.
C     July 1999. Based on the RASP routines SRISEP and RPDSGH.
C
C     REVISIONS
C
C     A. Varga, 3-11-2002.
C     V. Sima, Dec. 2016, June 2017.
C
C     KEYWORDS
C
C     Generalized eigenvalue problem, system poles, multivariable
C     system, orthogonal transformation, structural invariant.
C
C     ******************************************************************
C
C     ..
C     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
C     .. Scalar Arguments ..
      CHARACTER          COMPQ, COMPZ, JOB, JOBA
      INTEGER            INFO, LDA, LDB, LDC, LDE, LDQ, LDWORK, LDZ, M,
     $                   N, ND, NF, NIBLCK, P
      DOUBLE PRECISION   TOL
C     .. Array Arguments ..
      INTEGER            IBLCK( * ), IWORK(*)
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * ),
     $                   DWORK( * ),  E( LDE, * ), Q( LDQ, * ),
     $                   Z( LDZ, * )
C     .. Local Scalars ..
      CHARACTER          JOBQ, JOBZ
      LOGICAL            ILQ, ILZ, LQUERY, REDA, REDIF
      INTEGER            I, ICOMPQ, ICOMPZ, IHI, ILO, MINWRK, RANKE,
     $                   RNKA22, WRKOPT
C     .. Local Arrays ..
      DOUBLE PRECISION   DUM(1)
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     .. External Subroutines ..
      EXTERNAL           AB07MD, DSWAP, MA02BD, MA02CD, TB01XD, TG01BD,
     $                   TG01FD, TG01LY, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC          INT, MAX
C     .. Executable Statements ..
C
C     Decode COMPQ.
C
      IF( LSAME( COMPQ, 'N' ) ) THEN
         ILQ = .FALSE.
         ICOMPQ = 1
      ELSE IF( LSAME( COMPQ, 'U' ) ) THEN
         ILQ = .TRUE.
         ICOMPQ = 2
      ELSE IF( LSAME( COMPQ, 'I' ) ) THEN
         ILQ = .TRUE.
         ICOMPQ = 3
      ELSE
         ICOMPQ = 0
      END IF
C
C     Decode COMPZ.
C
      IF( LSAME( COMPZ, 'N' ) ) THEN
         ILZ = .FALSE.
         ICOMPZ = 1
      ELSE IF( LSAME( COMPZ, 'U' ) ) THEN
         ILZ = .TRUE.
         ICOMPZ = 2
      ELSE IF( LSAME( COMPZ, 'I' ) ) THEN
         ILZ = .TRUE.
         ICOMPZ = 3
      ELSE
         ICOMPZ = 0
      END IF
      REDIF = LSAME( JOB,  'I' )
      REDA  = LSAME( JOBA, 'H' )
C
C     Test the input parameters.
C
      INFO = 0
      IF( .NOT.LSAME( JOB, 'F' ) .AND. .NOT.REDIF ) THEN
         INFO = -1
      ELSE IF( .NOT.LSAME( JOBA, 'N' ) .AND. .NOT.REDA ) THEN
         INFO = -2
      ELSE IF( ICOMPQ.LE.0 ) THEN
         INFO = -3
      ELSE IF( ICOMPZ.LE.0 ) THEN
         INFO = -4
      ELSE IF( N.LT.0 ) THEN
         INFO = -5
      ELSE IF( M.LT.0 ) THEN
         INFO = -6
      ELSE IF( P.LT.0 ) THEN
         INFO = -7
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE IF( LDE.LT.MAX( 1, N ) ) THEN
         INFO = -11
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -13
      ELSE IF( ( .NOT.REDIF .AND. LDC.LT.MAX( 1, P ) ) .OR.
     $         ( REDIF .AND. LDC.LT.MAX( 1, M, P ) ) )THEN
         INFO = -15
      ELSE IF( ( ILQ .AND. LDQ.LT.N ) .OR. LDQ.LT.1 ) THEN
         INFO = -17
      ELSE IF( ( ILZ .AND. LDZ.LT.N ) .OR. LDZ.LT.1 ) THEN
         INFO = -19
      ELSE IF( TOL.GE.ONE ) THEN
         INFO = -24
      ELSE
         LQUERY = LDWORK.EQ.-1
         IF( N.EQ.0 ) THEN
            MINWRK = 1
         ELSE
            MINWRK = N + MAX( 3*N, M, P )
         END IF
         IF( LQUERY ) THEN
            RANKE  = MAX( 1, INT( N/2 ) )
            RNKA22 = N - RANKE
C
            IF( REDIF ) THEN
               CALL TG01FD( COMPZ, COMPQ, 'Trapezoidal', N, N, P, M, A,
     $                      LDA, E, LDE, B, LDB, C, LDC, Z, LDZ, Q, LDQ,
     $                      RANKE, RNKA22, TOL, IWORK, DWORK, -1, INFO )
               WRKOPT = MAX( MINWRK, INT( DWORK(1) ) )
               CALL TG01LY( ILZ, ILQ, N, P, M, RANKE, RNKA22, A, LDA,
     $                      E, LDE, B, LDB, C, LDC, Z, LDZ, Q, LDQ, NF,
     $                      NIBLCK, IBLCK, TOL, IWORK, DWORK, -1, INFO )
            ELSE
               CALL TG01FD( COMPQ, COMPZ, 'Trapezoidal', N, N, M, P, A,
     $                      LDA, E, LDE, B, LDB, C, LDC, Q, LDQ, Z, LDZ,
     $                      RANKE, RNKA22, TOL, IWORK, DWORK, -1, INFO )
               WRKOPT = MAX( MINWRK, INT( DWORK(1) ) )
               CALL TG01LY( ILQ, ILZ, N, M, P, RANKE, RNKA22, A, LDA,
     $                      E, LDE, B, LDB, C, LDC, Q, LDQ, Z, LDZ, NF,
     $                      NIBLCK, IBLCK, TOL, IWORK, DWORK, -1, INFO )
            END IF
            WRKOPT = MAX( WRKOPT, INT( DWORK(1) ) )
         END IF
         IF( LDWORK.LT.MINWRK .AND. .NOT.LQUERY ) THEN
            INFO = -27
         END IF
      END IF
C
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'TG01LD', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         DWORK(1) = WRKOPT
         RETURN
      END IF
C
C     Quick return if possible
C
      IF( N.EQ.0 ) THEN
         NF = 0
         ND = 0
         NIBLCK = 0
         DWORK(1) = ONE
         RETURN
      END IF
C
      IF( REDIF ) THEN
C
C       Build the dual system.
C
         CALL AB07MD( 'Z', N, M, P, A, LDA, B, LDB, C, LDC, DUM, 1,
     $                INFO )
         DO 10 I = 2, N
            CALL DSWAP( I-1, E(I,1), LDE, E(1,I), 1 )
   10    CONTINUE
C
C        Reduce to SVD-like form with A22 in QR-form.
C
         CALL TG01FD( COMPZ, COMPQ, 'Trapezoidal', N, N, P, M, A, LDA,
     $                E, LDE, B, LDB, C, LDC, Z, LDZ, Q, LDQ, RANKE,
     $                RNKA22, TOL, IWORK, DWORK, LDWORK, INFO )
         WRKOPT = MAX( MINWRK, INT( DWORK(1) ) )
C
C        Perform finite-infinite separation.
C
         CALL TG01LY( ILZ, ILQ, N, P, M, RANKE, RNKA22, A, LDA,
     $                E, LDE, B, LDB, C, LDC, Z, LDZ, Q, LDQ, NF,
     $                NIBLCK, IBLCK, TOL, IWORK, DWORK, LDWORK, INFO )
         ILO = N - NF + 1
         IHI = N
      ELSE
C
C        Reduce to SVD-like form with A22 in QR-form.
C
         CALL TG01FD( COMPQ, COMPZ, 'Trapezoidal', N, N, M, P, A, LDA,
     $                E, LDE, B, LDB, C, LDC, Q, LDQ, Z, LDZ, RANKE,
     $                RNKA22, TOL, IWORK, DWORK, LDWORK, INFO )
         WRKOPT = MAX( MINWRK, INT( DWORK(1) ) )
C
C        Perform finite-infinite separation.
C
         CALL TG01LY( ILQ, ILZ, N, M, P, RANKE, RNKA22, A, LDA,
     $                E, LDE, B, LDB, C, LDC, Q, LDQ, Z, LDZ, NF,
     $                NIBLCK, IBLCK, TOL, IWORK, DWORK, LDWORK, INFO )
         ILO = 1
         IHI = NF
      END IF
C
      IF( INFO.NE.0 )
     $   RETURN
      WRKOPT = MAX( WRKOPT, INT( DWORK(1) ) )
C
      ND = N - RANKE
C
      IF( REDIF ) THEN
C
C        Compute the pertransposed dual system exploiting matrix shapes.
C
         CALL TB01XD( 'Z', N, P, M, MAX( 0, NF-1 ), MAX( 0, N-1 ),
     $                A, LDA, B, LDB, C, LDC, DUM, 1, INFO )
         CALL MA02CD( N, 0, MAX( 0, N-1 ), E, LDE )
         IF( ILQ )
     $      CALL MA02BD( 'Right', N, N, Q, LDQ )
         IF( ILZ )
     $      CALL MA02BD( 'Right', N, N, Z, LDZ )
      END IF
C
C     If required, reduce (A,E) to generalized Hessenberg form.
C
      IF( REDA ) THEN
         IF( ILQ) THEN
            JOBQ = 'V'
         ELSE
            JOBQ = 'N'
         END IF
         IF( ILZ) THEN
            JOBZ = 'V'
         ELSE
            JOBZ = 'N'
         END IF
         CALL TG01BD( 'Upper', JOBQ, JOBZ, N, M, P, ILO, IHI, A, LDA,
     $                E, LDE, B, LDB, C, LDC, Q, LDQ, Z, LDZ, DWORK,
     $                LDWORK, INFO )
      END IF
C
      DWORK(1) = WRKOPT
C
      RETURN
C *** Last line of TG01LD ***
      END
