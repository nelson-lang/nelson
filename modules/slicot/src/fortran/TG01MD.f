      SUBROUTINE TG01MD( JOB, N, M, P, A, LDA, E, LDE, B, LDB, C, LDC,
     $                   ALPHAR, ALPHAI, BETA, Q, LDQ, Z, LDZ, NF, ND,
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
C     where the pair (Af,Ef) is in a generalized real Schur form, with
C     Ef nonsingular and upper triangular and Af in real Schur form.
C     The subpencil Af-lambda*Ef contains the finite eigenvalues.
C     The pair (Ai,Ei) is in a generalized real Schur form with
C     both Ai and Ei upper triangular. The subpencil Ai-lambda*Ei,
C     with Ai nonsingular and Ei nilpotent contains the infinite
C     eigenvalues and is in a block staircase form (see METHOD).
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOB     CHARACTER*1
C             = 'F':  perform the finite-infinite separation;
C             = 'I':  perform the infinite-finite separation.
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
C             the transformed state matrix Q'*A*Z in the form
C
C             ( Af  *  )                    ( Ai  *  )
C             (        ) for JOB = 'F', or  (        )  for JOB = 'I',
C             ( 0   Ai )                    ( 0   Af )
C
C             where Af is an NF-by-NF matrix in real Schur form, and Ai
C             is an (N-NF)-by-(N-NF) nonsingular and upper triangular
C             matrix. Ai has a block structure as in (3) or (4), where
C             A0,0 is ND-by-ND and Ai,i , for i = 1, ..., NIBLCK, is
C             IBLCK(i)-by-IBLCK(i). (See METHOD.)
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     E       (input/output) DOUBLE PRECISION array, dimension (LDE,N)
C             On entry, the leading N-by-N part of this array must
C             contain the N-by-N descriptor matrix E.
C             On exit, the leading N-by-N part of this array contains
C             the transformed descriptor matrix Q'*E*Z in the form
C
C             ( Ef  *  )                    ( Ei  *  )
C             (        ) for JOB = 'F', or  (        )  for JOB = 'I',
C             ( 0   Ei )                    ( 0   Ef )
C
C             where Ef is an NF-by-NF nonsingular and upper triangular
C             matrix, and Ei is an (N-NF)-by-(N-NF) nilpotent matrix in
C             an upper triangular block form as in (3) or (4).
C
C     LDE     INTEGER
C             The leading dimension of the array E.  LDE >= MAX(1,N).
C
C     B       (input/output) DOUBLE PRECISION array, dimension (LDB,M)
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
C             The leading dimension of the array C.  LDC >= MAX(1,P).
C
C     ALPHAR  (output) DOUBLE PRECISION array, dimension (N)
C             ALPHAR(1:NF) will be set to the real parts of the diagonal
C             elements of Af that would result from reducing A and E to
C             the Schur form, and then further reducing both of them to
C             triangular form using unitary transformations, subject to
C             having the diagonal of E positive real.  Thus, if Af(j,j)
C             is in a 1-by-1 block (i.e., Af(j+1,j) = Af(j,j+1) = 0),
C             then ALPHAR(j) = Af(j,j). Note that the (real or complex)
C             values (ALPHAR(j) + i*ALPHAI(j))/BETA(j), j=1,...,NF, are
C             the finite generalized eigenvalues of the matrix pencil
C             A - lambda*E.
C
C     ALPHAI  (output) DOUBLE PRECISION array, dimension (N)
C             ALPHAI(1:NF) will be set to the imaginary parts of the
C             diagonal elements of Af that would result from reducing A
C             and E to Schur form, and then further reducing both of
C             them to triangular form using unitary transformations,
C             subject to having the diagonal of E positive real. Thus,
C             if Af(j,j) is in a 1-by-1 block (see above), then
C             ALPHAI(j) = 0. Note that the (real or complex) values
C             (ALPHAR(j) + i*ALPHAI(j))/BETA(j), j=1,...,NF, are the
C             finite generalized eigenvalues of the matrix pencil
C             A - lambda*E.
C
C     BETA    (output) DOUBLE PRECISION array, dimension (N)
C             BETA(1:NF) will be set to the (real) diagonal elements of
C             Ef that would result from reducing A and E to Schur form,
C             and then further reducing both of them to triangular form
C             using unitary transformations, subject to having the
C             diagonal of E positive real. Thus, if Af(j,j) is in a
C             1-by-1 block (see above), then BETA(j) = Ef(j,j).
C             Note that the (real or complex) values
C             (ALPHAR(j) + i*ALPHAI(j))/BETA(j), j=1,...,NF, are the
C             finite generalized eigenvalues of the matrix pencil
C             A - lambda*E.
C
C     Q       (output) DOUBLE PRECISION array, dimension (LDQ,N)
C             The leading N-by-N part of this array contains the
C             orthogonal matrix Q, which is the accumulated product of
C             the transformations applied to A, E, and B on the left.
C
C     LDQ     INTEGER
C             The leading dimension of the array Q.  LDQ >= MAX(1,N).
C
C     Z       (output) DOUBLE PRECISION array, dimension (LDZ,N)
C             The leading N-by-N part of this array contains the
C             orthogonal matrix Z, which is the accumulated product of
C             the transformations applied to A, E, and C on the right.
C
C     LDZ     INTEGER
C             The leading dimension of the array Z.  LDZ >= MAX(1,N).
C
C     NF      (output) INTEGER
C             The order of the reduced matrices Af and Ef; also, the
C             number of finite generalized eigenvalues of the pencil
C             A-lambda*E.
C
C     ND      (output) INTEGER
C             The number of non-dynamic infinite eigenvalues of the
C             matrix pair (A,E). Note: N-ND is the rank of the matrix E.
C
C     NIBLCK  (output) INTEGER
C             If ND > 0, the number of infinite blocks minus one.
C             If ND = 0, then NIBLCK = 0.
C
C     IBLCK   (output) INTEGER array, dimension (N)
C             IBLCK(i) contains the dimension of the i-th block in the
C             staircase form (3), where i = 1,2,...,NIBLCK.
C
C     Tolerances
C
C     TOL     DOUBLE PRECISION
C             A tolerance used in rank decisions to determine the
C             effective rank, which is defined as the order of the
C             largest leading (or trailing) triangular submatrix in the
C             QR factorization with column pivoting whose estimated
C             condition number is less than 1/TOL. If the user sets
C             TOL <= 0, then an implicitly computed, default tolerance
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
C             LDWORK >= 4*N.
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
C             = 1:  the pencil A-lambda*E is not regular;
C             = 2:  the QZ iteration did not converge.
C
C     METHOD
C
C     For the separation of infinite structure, the reduction algorithm
C     of [1] is employed.
C     If JOB = 'F', the matrices Ai and Ei have the form
C
C           ( A0,0  A0,k ... A0,1 )         ( 0  E0,k ... E0,1 )
C      Ai = (  0    Ak,k ... Ak,1 ) ,  Ei = ( 0   0   ... Ek,1 ) ;   (3)
C           (  :     :    .    :  )         ( :   :    .    :  )
C           (  0     0   ... A1,1 )         ( 0   0   ...   0  )
C
C     if JOB = 'I' the matrices Ai and Ei have the form
C
C           ( A1,1 ... A1,k  A1,0 )         ( 0 ... E1,k  E1,0 )
C      Ai = (  :    .    :    :   ) ,  Ei = ( :  .    :    :   ) ,   (4)
C           (  :   ... Ak,k  Ak,0 )         ( : ...   0   Ek,0 )
C           (  0   ...   0   A0,0 )         ( 0 ...   0     0  )
C
C     where Ai,i, for i = 0, 1, ..., k, are nonsingular upper triangular
C     matrices. A0,0 corresponds to the non-dynamic infinite modes of
C     the system.
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
C     V. Sima, Dec. 2016.
C
C     KEYWORDS
C
C     Generalized eigenvalue problem, system poles, multivariable
C     system, orthogonal transformation, structural invariant.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D0, ZERO = 0.0D0 )
C     .. Scalar Arguments ..
      CHARACTER          JOB
      INTEGER            INFO, LDA, LDB, LDC, LDE, LDQ, LDWORK, LDZ, M,
     $                   N, ND, NF, NIBLCK, P
      DOUBLE PRECISION   TOL
C     .. Array Arguments ..
      INTEGER            IBLCK( * ), IWORK(*)
      DOUBLE PRECISION   A(LDA,*), ALPHAI(*), ALPHAR(*), B(LDB,*),
     $                   BETA(*),  C(LDC,*),  DWORK(*),  E(LDE,*),
     $                   Q(LDQ,*), Z(LDZ,*)
C     .. Local Scalars ..
      LOGICAL            LQUERY, TRINF
      INTEGER            I, IHI, ILO, MINWRK, NBC, NC, NR, WRKOPT
C     .. Local Arrays ..
      DOUBLE PRECISION   DUM(1)
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     .. External Subroutines ..
      EXTERNAL           DGEMM, DHGEQZ, DLACPY, TG01LD, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC          INT, MAX, MIN
C     .. Executable Statements ..
C
C     Test the input parameters.
C
      INFO  =  0
      TRINF =  LSAME( JOB, 'I' )
      IF( .NOT.LSAME( JOB, 'F' ) .AND. .NOT.TRINF ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( P.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF( LDE.LT.MAX( 1, N ) ) THEN
         INFO = -8
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -10
      ELSE IF( LDC.LT.MAX( 1, P ) ) THEN
         INFO = -12
      ELSE IF( LDQ.LT.MAX( 1, N ) ) THEN
         INFO = -17
      ELSE IF( LDZ.LT.MAX( 1, N ) ) THEN
         INFO = -19
      ELSE IF( TOL.GE.ONE ) THEN
         INFO = -24
      ELSE
         LQUERY = LDWORK.EQ.-1
         IF( N.EQ.0 ) THEN
            MINWRK = 1
         ELSE
            MINWRK = 4*N
         END IF
         IF( LQUERY ) THEN
            ILO = 1
            IHI = N
C
            CALL TG01LD( JOB, 'Hessenberg', 'Identity', 'Identity', N,
     $                   0, 0, A, LDA, E, LDE, DUM, LDB, DUM, LDC, Q,
     $                   LDQ, Z, LDZ, NF, ND, NIBLCK, IBLCK, TOL, IWORK,
     $                   DWORK, -1, INFO )
            WRKOPT = MAX( MINWRK, INT( DWORK(1) ) )
            CALL DHGEQZ( 'Schur', 'Vector', 'Vector', N, ILO, IHI, A,
     $                   LDA, E, LDE, ALPHAR, ALPHAI, BETA, Q, LDQ, Z,
     $                   LDZ, DWORK, -1, INFO )
            WRKOPT = MAX( WRKOPT, INT( DWORK(1) ) )
         END IF
         IF( LDWORK.LT.MINWRK .AND. .NOT.LQUERY ) THEN
            INFO = -27
         END IF
      END IF
C
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'TG01MD', -INFO )
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
         NIBLCK   = 0
         DWORK(1) = ONE
         RETURN
      END IF
C
C     Compute the finite-infinite separation with A in Hessenberg form
C     and E upper triangular.
C     Workspace: need  4*N.
C
      CALL TG01LD( JOB, 'Hessenberg', 'Identity', 'Identity', N, 0, 0,
     $             A, LDA, E, LDE, DUM, LDB, DUM, LDC, Q, LDQ, Z, LDZ,
     $             NF, ND, NIBLCK, IBLCK, TOL, IWORK, DWORK, LDWORK,
     $             INFO )
C
      IF( INFO.NE.0 )
     $   RETURN
      WRKOPT = MAX( MINWRK, INT( DWORK(1) ) )
C
C     Reduce the finite part to generalized real Schur form.
C     Workspace: need  N;
C                prefer larger.
C
      IF( TRINF ) THEN
         ILO = N - NF + 1
         IHI = N
      ELSE
         ILO = 1
         IHI = NF
      END IF
      CALL DHGEQZ( 'Schur', 'Vector', 'Vector', N, ILO, IHI, A, LDA,
     $             E, LDE, ALPHAR, ALPHAI, BETA, Q, LDQ, Z, LDZ, DWORK,
     $             LDWORK, INFO )
      IF( INFO.NE.0 ) THEN
         INFO = 2
         RETURN
      END IF
      WRKOPT = MAX( WRKOPT, INT( DWORK(1) ) )
C
C     B <-- Q' * B.
C     Workspace: need   N;
C                prefer N*M.
C
      NBC = MAX( 1, MIN( LDWORK/N, M ) )
      DO 10 I = 1, M, NBC
         NC = MIN( NBC, M-I+1 )
         CALL DGEMM( 'Transpose', 'No transpose', N, NC, N, ONE, Q, LDQ,
     $               B(1,I), LDB, ZERO, DWORK, N )
         CALL DLACPY( 'All', N, NC, DWORK, N, B(1,I), LDB )
   10 CONTINUE
C
C     C <-- C * Z
C     Workspace: need   N;
C                prefer P*N.
C
      NBC = MAX( 1, MIN( LDWORK/N, P ) )
      DO 20 I = 1, P, NBC
         NR = MIN( NBC, P-I+1 )
         CALL DGEMM( 'No Transpose', 'No transpose', NR, N, N, ONE,
     $              C(I,1), LDC, Z, LDZ, ZERO, DWORK, NR )
         CALL DLACPY( 'All', NR, N, DWORK, NR, C(I,1), LDC )
   20 CONTINUE
C
      DWORK(1) = WRKOPT
C
      RETURN
C *** Last line of TG01MD ***
      END
