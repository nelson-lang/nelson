      SUBROUTINE TG01QD( DICO, STDOM, JOBFI, N, M, P, ALPHA, A, LDA,
     $                   E, LDE, B, LDB, C, LDC, N1, N2, N3, ND, NIBLCK,
     $                   IBLCK, Q, LDQ, Z, LDZ, ALPHAR, ALPHAI, BETA,
     $                   TOL, IWORK, DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To compute orthogonal transformation matrices Q and Z which
C     reduce the regular pole pencil A-lambda*E of the descriptor system
C     (A-lambda*E,B,C) to the generalized real Schur form with ordered
C     generalized eigenvalues. The pair (A,E) is reduced to the form
C
C                ( A1  *   *  )             ( E1  *   *  )
C       Q'*A*Z = ( 0   A2  *  ) ,  Q'*E*Z = ( 0   E2  *  ) ,         (1)
C                ( 0   0   A3 )             ( 0   0   E2 )
C
C     where the subpencils Ak-lambda*Ek, for k = 1, 2, 3, contain the
C     generalized eigenvalues which belong to certain domains of
C     interest.
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
C             Specifies the type of the domain of interest for the
C             generalized eigenvalues, as follows:
C             = 'S':  stability type domain (i.e., left part of complex
C                     plane or inside of a circle);
C             = 'U':  instability type domain (i.e., right part of complex
C                     plane or outside of a circle);
C             = 'N':  whole complex domain, excepting infinity.
C
C     JOBFI   CHARACTER*1
C             Specifies the type of generalized eigenvalues in the
C             leading diagonal block(s) as follows:
C             = 'F':  finite generalized eigenvalues are in the
C                     leading diagonal blocks (Af,Ef), and the resulting
C                     transformed pair has the form
C
C                              ( Af  *  )             ( Ef  *  )
C                     Q'*A*Z = (        ) ,  Q'*E*Z = (        ) ;
C                              ( 0   Ai )             ( 0   Ei )
C
C             = 'I':  infinite generalized eigenvalues are in the
C                     leading diagonal blocks (Ai,Ei), and the resulting
C                     transformed pair has the form
C
C                              ( Ai  *  )             ( Ei  *  )
C                     Q'*A*Z = (        ) ,  Q'*E*Z = (        ) .
C                              ( 0   Af )             ( 0   Ef )
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
C     ALPHA   (input) DOUBLE PRECISION
C             The boundary of the domain of interest for the finite
C             generalized eigenvalues of the pair (A,E). For a
C             continuous-time system (DICO = 'C'), ALPHA is the boundary
C             value for the real parts of the generalized eigenvalues,
C             while for a discrete-time system (DICO = 'D'), ALPHA >= 0
C             represents the boundary value for the moduli of the
C             generalized eigenvalues.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the state dynamics matrix A.
C             On exit, the leading N-by-N part of this array contains
C             the matrix Q'*A*Z in real Schur form, with the elements
C             below the first subdiagonal set to zero.
C             If JOBFI = 'I', the N1-by-N1 pair (A1,E1) contains the
C             infinite spectrum, the N2-by-N2 pair (A2,E2) contains the
C             finite spectrum in the domain of interest, and the
C             N3-by-N3 pair (A3,E3) contains the finite spectrum ouside
C             of the domain of interest.
C             If JOBFI = 'F', the N1-by-N1 pair (A1,E1) contains the
C             finite spectrum in the domain of interest, the N2-by-N2
C             pair (A2,E2) contains the finite spectrum ouside of the
C             domain of interest, and the N3-by-N3 pair (A3,E3) contains
C             the infinite spectrum.
C             Ai has a block structure as in (2), where A0,0 is ND-by-ND
C             and Ai,i is IBLCK(i)-by-IBLCK(i), for i = 1, ..., NIBLCK.
C             The domain of interest for the pair (Af,Ef), containing
C             the finite generalized eigenvalues, is defined by the
C             parameters ALPHA, DICO and STDOM as follows:
C               For DICO = 'C':
C                  Real(eig(Af,Ef)) < ALPHA if STDOM = 'S';
C                  Real(eig(Af,Ef)) > ALPHA if STDOM = 'U'.
C               For DICO = 'D':
C                  Abs(eig(Af,Ef))  < ALPHA if STDOM = 'S';
C                  Abs(eig(Af,Ef))  > ALPHA if STDOM = 'U'.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     E       (input/output) DOUBLE PRECISION array, dimension (LDE,N)
C             On entry, the leading N-by-N part of this array must
C             contain the descriptor matrix E.
C             On exit, the leading N-by-N part of this array contains
C             the matrix Q'*E*Z in upper triangular form, with the
C             elements below the diagonal set to zero. Its structure
C             corresponds to the block structure of the matrix Q'*A*Z
C             (see description of A).
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
C     N1      (output) INTEGER
C     N2      (output) INTEGER
C     N3      (output) INTEGER
C             The number of the generalized eigenvalues of the pairs
C             (A1,E1), (A2,E2) and (A3,E3), respectively.
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
C             staircase form (2), where i = 1,2,...,NIBLCK.
C
C     Q       (output) DOUBLE PRECISION array, dimension (LDQ,N)
C             The leading N-by-N part of this array contains the
C             orthogonal matrix Q, where Q' is the product of orthogonal
C             transformations applied to A, E, and B on the left.
C
C     LDQ     INTEGER
C             The leading dimension of the array Q.  LDQ >= MAX(1,N).
C
C     Z       (output) DOUBLE PRECISION array, dimension (LDZ,N)
C             The leading N-by-N part of this array contains the
C             orthogonal matrix Z, which is the product of orthogonal
C             transformations applied to A, E, and C on the right.
C
C     LDZ     INTEGER
C             The leading dimension of the array Z.  LDZ >= MAX(1,N).
C
C     ALPHAR  (output) DOUBLE PRECISION array, dimension (N)
C     ALPHAI  (output) DOUBLE PRECISION array, dimension (N)
C     BETA    (output) DOUBLE PRECISION array, dimension (N)
C             On exit, (ALPHAR(j) + ALPHAI(j)*i)/BETA(j), j = 1, ..., N,
C             are the generalized eigenvalues.
C             ALPHAR(j) + ALPHAI(j)*i, and BETA(j), j = 1, ..., N, are
C             the diagonals of the complex Schur form (S,T) that would
C             result if the 2-by-2 diagonal blocks of the real Schur
C             form of (A,E) were further reduced to triangular form
C             using 2-by-2 complex unitary transformations.
C             If ALPHAI(j) is zero, then the j-th eigenvalue is real;
C             if positive, then the j-th and (j+1)-st eigenvalues are a
C             complex conjugate pair, with ALPHAI(j+1) negative.
C
C     Tolerances
C
C     TOL     DOUBLE PRECISION
C             A tolerance used in rank decisions to determine the
C             effective rank, which is defined as the order of the
C             largest leading (or trailing) triangular submatrix in the
C             QR factorization with column pivoting whose estimated
C             condition number is less than 1/TOL.  If the user sets
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
C             The length of the array DWORK.  LDWORK >= 1, and if N = 0,
C             LDWORK >= 4*N,    if STDOM = 'N';
C             LDWORK >= 4*N+16, if STDOM = 'S' or 'U'.
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
C             = 1:  the pencil A-lambda*E is not regular;
C             = 2:  the QZ algorithm failed to compute all generalized
C                   eigenvalues of the pair (A,E);
C             = 3:  a failure occured during the ordering of the
C                   generalized real Schur form of the pair (A,E).
C
C     METHOD
C
C     The separation of the finite and infinite parts is based on the
C     reduction algorithm of [1].
C     If JOBFI = 'F', the matrices of the pair (Ai,Ei), containing the
C     infinite generalized eigenvalues, have the form
C
C           ( A0,0  A0,k ... A0,1 )         ( 0  E0,k ... E0,1 )
C      Ai = (  0    Ak,k ... Ak,1 ) ,  Ei = ( 0   0   ... Ek,1 ) ;   (2)
C           (  :     :    .    :  )         ( :   :    .    :  )
C           (  0     0   ... A1,1 )         ( 0   0   ...   0  )
C
C     if JOBFI = 'I', the matrices Ai and Ei have the form
C
C           ( A1,1 ... A1,k  A1,0 )         ( 0 ... E1,k  E1,0 )
C      Ai = (  :    .    :    :   ) ,  Ei = ( :  .    :    :   ) ,   (3)
C           (  :   ... Ak,k  Ak,0 )         ( : ...   0   Ek,0 )
C           (  0   ...   0   A0,0 )         ( 0 ...   0     0  )
C
C     where Ai,i , for i = 0, 1, ..., k, are nonsingular upper
C     triangular matrices, and A0,0 corresponds to the non-dynamic
C     infinite modes of the system.
C
C     REFERENCES
C
C     [1] Misra, P., Van Dooren, P., and Varga, A.
C         Computation of structural invariants of generalized
C         state-space systems.
C         Automatica, 30, pp. 1921-1936, 1994.
C
C     NUMERICAL ASPECTS
C                                     3
C     The algorithm requires about 25N  floating point operations.
C
C     FURTHER COMMENTS
C
C     The number of infinite poles is computed as
C
C                   NIBLCK
C        NINFP =     Sum  IBLCK(i) = N - ND - NF,
C                    i=1
C
C     where NF is the number of finite generalized eigenvalues.
C     The multiplicities of infinite poles can be computed as follows:
C     there are IBLCK(k)-IBLCK(k+1) infinite poles of multiplicity k,
C     for k = 1, ..., NIBLCK, where IBLCK(NIBLCK+1) = 0.
C     Note that each infinite pole of multiplicity k corresponds to an
C     infinite eigenvalue of multiplicity k+1.
C
C     CONTRIBUTOR
C
C     A. Varga, German Aerospace Center,
C     DLR Oberpfaffenhofen, October 2002.
C     Based on the RASP routine SRSFOD.
C
C     REVISIONS
C
C     V. Sima, Dec. 2016, June 2017.
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
      CHARACTER        DICO, JOBFI, STDOM
      INTEGER          INFO, LDA, LDB, LDC, LDE, LDQ, LDWORK, LDZ, M, N,
     $                 N1, N2, N3, ND, NIBLCK, P
      DOUBLE PRECISION ALPHA, TOL
C     .. Array Arguments ..
      INTEGER          IBLCK( * ), IWORK(*)
      DOUBLE PRECISION A(LDA,*), ALPHAI(*), ALPHAR(*), B(LDB,*),
     $                 BETA(*), C(LDC,*), DWORK(*), E(LDE,*), Q(LDQ,*),
     $                 Z(LDZ,*)
C     .. Local Scalars ..
      LOGICAL          DISCR, LQUERY, ORDER, REDIF, STAB
      INTEGER          I, LW, MINWRK, NB, NBC, NC, NDIM, NF, NI, NLOW,
     $                 NR, NSUP, WRKOPT
C     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
C     .. External Subroutines ..
      EXTERNAL         DGEMM, DGEQRF, DLACPY, MB03QG, MB03QV, TG01MD,
     $                 XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC        INT, MAX, MIN
C
C     .. Executable Statements ..
C
      INFO  = 0
      DISCR = LSAME( DICO,  'D' )
      REDIF = LSAME( JOBFI, 'I' )
      STAB  = LSAME( STDOM, 'S' )
      ORDER = STAB .OR. LSAME( STDOM, 'U' )
C
C     Check input scalar arguments.
C
      IF(      .NOT.DISCR .AND. .NOT.LSAME( DICO,  'C' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.ORDER .AND. .NOT.LSAME( STDOM, 'N' ) ) THEN
         INFO = -2
      ELSE IF( .NOT.REDIF .AND. .NOT.LSAME( JOBFI, 'F' ) ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( M.LT.0 ) THEN
         INFO = -5
      ELSE IF( P.LT.0 ) THEN
         INFO = -6
      ELSE IF( DISCR .AND. ALPHA.LT.ZERO ) THEN
         INFO = -7
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE IF( LDE.LT.MAX( 1, N ) ) THEN
         INFO = -11
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -13
      ELSE IF( LDC.LT.MAX( 1, P ) ) THEN
         INFO = -15
      ELSE IF( LDQ.LT.MAX( 1, N ) ) THEN
         INFO = -23
      ELSE IF( LDZ.LT.MAX( 1, N ) ) THEN
         INFO = -25
      ELSE IF( TOL.GE.ONE ) THEN
         INFO = -29
      ELSE
         IF( N.EQ.0 ) THEN
            MINWRK = 1
         ELSE IF( ORDER ) THEN
            MINWRK = 4*N + 16
         ELSE
            MINWRK = 4*N
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
            CALL TG01MD( JOBFI, N, 0, 0, A, LDA, E, LDE, B, LDB, C, LDC,
     $                   ALPHAR, ALPHAI, BETA, Q, LDQ, Z, LDZ, NF, ND,
     $                   NIBLCK, IBLCK, TOL, IWORK, DWORK, -1, INFO )
            WRKOPT = MAX( MINWRK, LW, INT( DWORK(1) ) )
            IF( ORDER ) THEN
               NLOW = 1
               NSUP = N
               CALL MB03QG( DICO, STDOM, 'Update', 'Update', N, NLOW,
     $                      NSUP, ALPHA, A, LDA, E, LDE, Q, LDQ, Z, LDZ,
     $                      NDIM, DWORK, -1, INFO )
               WRKOPT = MAX( WRKOPT, INT( DWORK(1) ) )
            END IF
         ELSE IF( LDWORK.LT.MINWRK ) THEN
            INFO = -32
         END IF
      END IF
C
      IF( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'TG01QD', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         DWORK(1) = WRKOPT
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( N.EQ.0 ) THEN
         N1 = 0
         N2 = 0
         N3 = 0
         ND = 0
         NIBLCK   = 0
         DWORK(1) = ONE
         RETURN
      END IF
C
C     Finite-infinite separation in generalized real Schur form.
C
C     Workspace:  need   4*N;
C                 prefer larger.
C
      CALL TG01MD( JOBFI, N, 0, 0, A, LDA, E, LDE, B, LDB, C, LDC,
     $             ALPHAR, ALPHAI, BETA, Q, LDQ, Z, LDZ, NF, ND, NIBLCK,
     $             IBLCK, TOL, IWORK, DWORK, LDWORK, INFO )
C
      IF( INFO.NE.0 )
     $   RETURN
C
      WRKOPT = MAX( MINWRK, INT( DWORK(1) ) )
C
      NI = N - NF
      IF( ORDER ) THEN
         IF( REDIF ) THEN
            NLOW = NI + 1
            NSUP = N
         ELSE
            NLOW = 1
            NSUP = MAX( 1, NF )
         END IF
C
C        Separate the spectrum of (A,E). The leading NDIM-by-NDIM subpencil
C        of Af-lambda*Ef corresponds to the generalized eigenvalues in the
C        domain of interest.
C
C        Workspace:  need   4*N+16.
C
         CALL MB03QG( DICO, STDOM, 'Update', 'Update', N, NLOW, NSUP,
     $                ALPHA, A, LDA, E, LDE, Q, LDQ, Z, LDZ, NDIM,
     $                DWORK, LDWORK, INFO )
         IF( INFO.NE.0 ) THEN
            INFO = 3
            RETURN
         END IF
C
         WRKOPT = MAX( WRKOPT, INT( DWORK(1) ) )
C
         IF( REDIF ) THEN
            N1 = NI
            N2 = NDIM
            N3 = N - N1 - N2
         ELSE
            N1 = NDIM
            N3 = NI
            N2 = N - N1 - N3
         END IF
C
C        Compute the generalized eigenvalues.
C
         CALL MB03QV( N, A, LDA, E, LDE, ALPHAR, ALPHAI, BETA, INFO )
      ELSE
         IF( REDIF ) THEN
            N1 = NI
            N3 = NF
         ELSE
            N1 = NF
            N3 = NI
         END IF
         N2 = 0
      END IF
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
C *** Last line of TG01QD ***
      END
