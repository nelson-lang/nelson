      SUBROUTINE AB08NY( FIRST, N, M, P, SVLMAX, ABCD, LDABCD, NINFZ,
     $                   NR, PR, DINFZ, NKRONL, INFZ, KRONL, TOL, IWORK,
     $                   DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To extract from the (N+P)-by-(M+N) system pencil
C                  ( B  A-lambda*I )
C                  ( D      C      )
C     an (NR+PR)-by-(M+NR) "reduced" system pencil,
C                  ( Br Ar-lambda*I ),
C                  ( Dr     Cr      )
C     having the same transmission zeros, but with Dr of full row rank.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     FIRST   LOGICAL
C             Specifies if AB08NY is called first time, or it is called
C             for an already reduced system, with D of full column rank,
C             with the last M rows in upper triangular form:
C             FIRST = .TRUE.  :  first time called;
C             FIRST = .FALSE. :  not first time called.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The number of rows of the matrix B, the number of columns
C             of the matrix C, and the order of the square matrix A.
C             N >= 0.
C
C     M       (input) INTEGER
C             The number of columns of the matrices B and D.  M >= 0.
C             M <= P, if FIRST = .FALSE.
C
C     P       (input) INTEGER
C             The number of rows of the matrices C and D.  P >= 0.
C
C     SVLMAX  (input) DOUBLE PRECISION
C             An estimate of the largest singular value of the original
C             matrix ABCD (for instance, the Frobenius norm of ABCD).
C             SVLMAX >= 0.
C
C     ABCD    (input/output) DOUBLE PRECISION array, dimension
C             (LDABCD,M+N)
C             On entry, the leading (N+P)-by-(M+N) part of this array
C             must contain the compound matrix
C                      (  B   A  ),
C                      (  D   C  )
C             where A is an N-by-N matrix, B is an N-by-M matrix,
C             C is a P-by-N matrix, and D is a P-by-M matrix.
C             If FIRST = .FALSE., then D must be a full column rank
C             matrix, with the last M rows in an upper triangular form.
C             On exit, the leading (NR+PR)-by-(M+NR) part of this array
C             contains the reduced compound matrix
C                       (  Br  Ar ),
C                       (  Dr  Cr )
C             where Ar is an NR-by-NR matrix, Br is an NR-by-M matrix,
C             Cr is a PR-by-NR matrix, and Dr is a PR-by-M full row rank
C             left upper-trapezoidal matrix, with the first PR columns
C             in an upper triangular form.
C
C     LDABCD  INTEGER
C             The leading dimension of the array ABCD.
C             LDABCD >= MAX(1,N+P).
C
C     NINFZ   (input/output) INTEGER
C             On entry, the currently computed number of infinite zeros.
C             It should be initialized to zero on the first call.
C             NINFZ >= 0.
C             If FIRST = .FALSE., then NINFZ is not modified.
C             On exit, the number of infinite zeros.
C
C     NR      (output) INTEGER
C             The order of the reduced matrix Ar; also, the number of
C             rows of the reduced matrix Br and the number of columns of
C             the reduced matrix Cr.
C             If Dr is invertible, NR is also the number of finite Smith
C             zeros.
C
C     PR      (output) INTEGER
C             The normal rank of the transfer-function matrix of the
C             original system; also, the number of rows of the reduced
C             matrices Cr and Dr.
C
C     DINFZ   (output) INTEGER
C             The maximal multiplicity of infinite zeros.
C             DINFZ = 0 if FIRST = .FALSE. .
C
C     NKRONL  (output) INTEGER
C             The maximal dimension of left elementary Kronecker blocks.
C
C     INFZ    (output) INTEGER array, dimension (N)
C             INFZ(i) contains the number of infinite zeros of degree i,
C             where i = 1,2,...,DINFZ.
C             INFZ is not referenced if FIRST = .FALSE. .
C
C     KRONL   (output) INTEGER array, dimension (N+1)
C             KRONL(i) contains the number of left elementary Kronecker
C             blocks of dimension i-by-(i-1), where i = 1,2,...,NKRONL.
C
C     Tolerances
C
C     TOL     DOUBLE PRECISION
C             A tolerance used in rank decisions to determine the
C             effective rank, which is defined as the order of the
C             largest leading (or trailing) triangular submatrix in the
C             QR (or RQ) factorization with column (or row) pivoting
C             whose estimated condition number is less than 1/TOL.
C             NOTE that when SVLMAX > 0, the estimated ranks could be
C             less than those defined above (see SVLMAX).
C             If the user sets TOL to be less than or equal to zero,
C             then the tolerance is taken as (N+P)*(N+M)*EPS, where EPS
C             is the machine precision (see LAPACK Library Routine
C             DLAMCH).  TOL < 1.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (MAX(M,P))
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The length of the array DWORK.
C             LDWORK >= 1, if MIN(P, MAX(N,M)) = 0; otherwise,
C             LDWORK >= MAX( MIN(P,M) + M + MAX(2*M,N) - 1,
C                            MIN(P,N) + MAX(N + MAX( P, M), 3*P - 1 ) ).
C             For optimum performance LDWORK should be larger.
C
C             If LDWORK = -1, then a workspace query is assumed;
C             the routine only calculates the optimal size of the
C             DWORK array, returns this value as the first entry of
C             the DWORK array, and no error message related to LDWORK
C             is issued by XERBLA.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value.
C
C     REFERENCES
C
C     [1] Svaricek, F.
C         Computation of the Structural Invariants of Linear
C         Multivariable Systems with an Extended Version of the
C         Program ZEROS.
C         System & Control Letters, 6, pp. 261-266, 1985.
C
C     [2] Emami-Naeini, A. and Van Dooren, P.
C         Computation of Zeros of Linear Multivariable Systems.
C         Automatica, 18, pp. 415-430, 1982.
C
C     NUMERICAL ASPECTS
C
C     The algorithm is numerically backward stable and requires
C     0( (P+N)*(M+N)*N )  floating point operations.
C
C     FURTHER COMMENTS
C
C     The number of infinite zeros is computed (if FIRST = .TRUE.) as
C
C                   DINFZ
C        NINFZ =     Sum  (INFZ(i)*i .
C                    i=1
C
C     Note that each infinite zero of multiplicity k corresponds to an
C     infinite eigenvalue of multiplicity k+1.
C     The multiplicities of the infinite eigenvalues can be determined
C     from PR, DINFZ and INFZ(i), i = 1, ..., DINFZ, as follows:
C
C                     DINFZ
C     - there are PR - Sum (INFZ(i)) simple infinite eigenvalues;
C                      i=1
C
C     - there are INFZ(i) infinite eigenvalues with multiplicity i+1,
C       for i = 1, ..., DINFZ.
C
C     The left Kronecker indices are:
C
C     [ 0  0 ...  0  | 1  1  ...  1 |  .... | NKRONL  ...  NKRONL ]
C     |<- KRONL(1) ->|<- KRONL(2) ->|       |<-  KRONL(NKRONL)  ->|
C
C     CONTRIBUTOR
C
C     V. Sima, Katholieke Univ. Leuven, Belgium.
C     A. Varga, DLR Oberpfaffenhofen, Germany, May 1999.
C     Supersedes Release 3.0 routine AB08BX.
C
C     REVISIONS
C
C     A. Varga, DLR Oberpfaffenhofen, Germany, March 2002.
C     V. Sima, Dec. 2016, Jan. 2017, Feb. 2018.
C
C     KEYWORDS
C
C     Generalized eigenvalue problem, Kronecker indices, multivariable
C     system, orthogonal transformation, structural invariant.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0 )
C     .. Scalar Arguments ..
      LOGICAL           FIRST
      INTEGER           DINFZ, INFO, LDABCD, LDWORK, M, N, NINFZ,
     $                  NKRONL, NR, P, PR
      DOUBLE PRECISION  SVLMAX, TOL
C     .. Array Arguments ..
      INTEGER           INFZ(*), IWORK(*), KRONL(*)
      DOUBLE PRECISION  ABCD(LDABCD,*), DWORK(*)
C     .. Local Scalars ..
      LOGICAL           LQUERY
      INTEGER           I, I1, ICOL, IRC, IROW, ITAU, JWORK, K, MN, MNR,
     $                  MNTAU, MP1, MPM, MPN, MUI, MUIM1, NBLCKS, PN,
     $                  RANK, RO, RO1, SIGMA, TAUI, WRKOPT
      DOUBLE PRECISION  RCOND
C     .. Local Arrays ..
      DOUBLE PRECISION  SVAL(3)
C     .. External Functions ..
      DOUBLE PRECISION  DLAMCH
      EXTERNAL          DLAMCH
C     .. External Subroutines ..
      EXTERNAL          DLAPMT, DLASET, DORMQR, DORMRQ, MB03OY, MB03PY,
     $                  MB04ID, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         DBLE, INT, MAX, MIN
C     .. Executable Statements ..
C
      INFO = 0
      PN   = P + N
      MN   = M + N
      MPN  = MIN( P, N )
      MPM  = MIN( P, M )
C
C     Test the input scalar arguments.
C
      IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( M.LT.0 .OR. (.NOT.FIRST .AND. M.GT.P ) ) THEN
         INFO = -3
      ELSE IF( P.LT.0 ) THEN
         INFO = -4
      ELSE IF( SVLMAX.LT.ZERO ) THEN
         INFO = -5
      ELSE IF( LDABCD.LT.MAX( 1, PN ) ) THEN
         INFO = -7
      ELSE IF( NINFZ.LT.0 .OR. ( FIRST .AND. NINFZ.GT.0 ) ) THEN
         INFO = -8
      ELSE IF( TOL.GE.ONE ) THEN
         INFO = -15
      ELSE
         LQUERY = ( LDWORK.EQ.-1 )
         IF( MIN( P, MAX( N, M ) ).EQ.0 ) THEN
            JWORK = 1
         ELSE
            JWORK = MAX( MPM + M + MAX( 2*M, N ) - 1,
     $                   MPN + MAX( N + MAX( P, M) , 3*P - 1 ) )
         END IF
         IF( LQUERY ) THEN
            IF( M.GT.0 ) THEN
               CALL MB04ID( P, MPM, M-1, N, ABCD, LDABCD, ABCD, LDABCD,
     $                      DWORK, DWORK, -1, INFO )
               WRKOPT = MAX( JWORK, MPM + INT( DWORK(1) ) )
               CALL DORMQR( 'Left', 'Transpose', P, N, MPM, ABCD,
     $                      LDABCD, DWORK, ABCD, LDABCD, DWORK, -1,
     $                      INFO )
               WRKOPT = MAX( WRKOPT, MPM + INT( DWORK(1) ) )
            ELSE
               WRKOPT = JWORK
            END IF
            CALL DORMRQ( 'Right', 'Transpose', PN, N, MPN, ABCD, LDABCD,
     $                   DWORK, ABCD, LDABCD, DWORK, -1, INFO )
            WRKOPT = MAX( WRKOPT, MPN + INT( DWORK(1) ) )
            CALL DORMRQ( 'Left', 'NoTranspose', N, MN, MPN, ABCD,
     $                   LDABCD, DWORK, ABCD, LDABCD, DWORK, -1, INFO )
            WRKOPT = MAX( WRKOPT, MPN + INT( DWORK(1) ) )
         ELSE IF( LDWORK.LT.JWORK ) THEN
            INFO = -18
         END IF
      END IF
C
      IF ( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'AB08NY', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         DWORK(1) = WRKOPT
         RETURN
      END IF
C
C     Initialize output variables.
C
      PR = P
      NR = N
C
      DINFZ  = 0
      NKRONL = 0
C
C     Quick return if possible.
C
      IF( P.EQ.0 ) THEN
         DWORK(1) = ONE
         RETURN
      END IF
C
      IF( MAX( N, M ).EQ.0 ) THEN
         PR       = 0
         NKRONL   = 1
         KRONL(1) = P
         DWORK(1) = ONE
         RETURN
      END IF
C
      WRKOPT = 1
      RCOND  = TOL
      IF( RCOND.LE.ZERO ) THEN
C
C        Use the default tolerance in rank determination.
C
         RCOND = DBLE( PN*MN )*DLAMCH( 'EPSILON' )
      END IF
C
C     The D matrix is (RO+SIGMA)-by-M, where RO = P - SIGMA and
C     SIGMA = 0, for FIRST = .TRUE., and SIGMA = M, for FIRST = .FALSE..
C     The leading (RO+SIGMA)-by-SIGMA submatrix of D has full column
C     rank, with the trailing SIGMA-by-SIGMA submatrix upper triangular.
C
      IF( FIRST ) THEN
         SIGMA = 0
      ELSE
         SIGMA = M
      END IF
      RO  = P - SIGMA
      MP1 = M + 1
      MUI = 0
C
      NBLCKS = 0
      ITAU   = 1
C
   10 CONTINUE
C
C     Main reduction loop:
C
C            M   NR                  M     NR
C      NR  [ B   A ]           NR  [ B     A ]
C      PR  [ D   C ]  -->    SIGMA [ RD   C1 ]   (SIGMA = rank(D) =
C                             TAU  [ 0    C2 ]    row size of RD)
C
C                                    M   NR-MUI MUI
C                           NR-MUI [ B1   A11   A12 ]
C                     -->     MUI  [ B2   A21   A22 ]  (MUI = rank(C2) =
C                            SIGMA [ RD   C11   C12 ]   col size of LC)
C                             TAU  [ 0     0    LC  ]
C
C                                    M   NR-MUI
C                           NR-MUI [ B1   A11 ]     NR := NR - MUI
C                                  [----------]     PR := MUI + SIGMA
C                     -->     MUI  [ B2   A21 ]      D := [B2;RD]
C                            SIGMA [ RD   C11 ]      C := [A21;C11]
C
      IF ( PR.EQ.0 )
     $   GO TO 20
C
C     (Note: Comments in the code beginning "Workspace:" describe the
C     minimal amount of real workspace needed at that point in the
C     code, as well as the preferred amount for good performance.)
C
      RO1  = RO
      MNR  = M + NR
C
      IF ( M.GT.0 ) THEN
C
C        Compress columns of D; first, exploit the trapezoidal shape of
C        the (RO+SIGMA)-by-SIGMA matrix in the first SIGMA columns of D;
C        compress the first SIGMA columns without column pivoting:
C
C              ( x x x x x )       ( x x x x x )
C              ( x x x x x )       ( 0 x x x x )
C              ( x x x x x )  - >  ( 0 0 x x x )
C              ( 0 x x x x )       ( 0 0 0 x x )
C              ( 0 0 x x x )       ( 0 0 0 x x )
C
C        where SIGMA = 3 and RO = 2.
C
         IROW = NR + 1
         IF ( SIGMA.GT.0 ) THEN
            JWORK = ITAU + SIGMA
C
C           Compress rows of D.  First, exploit the triangular shape.
C           Workspace: need   min(P,M) + M+N-1;
C                      prefer larger.
C
            CALL MB04ID( RO+SIGMA, SIGMA, SIGMA-1, MNR-SIGMA,
     $                   ABCD(IROW,1), LDABCD, ABCD(IROW,SIGMA+1),
     $                   LDABCD, DWORK(ITAU), DWORK(JWORK),
     $                   LDWORK-JWORK+1, INFO )
            CALL DLASET( 'Lower', RO+SIGMA-1, SIGMA, ZERO, ZERO,
     $                   ABCD(IROW+1,1), LDABCD )
            WRKOPT = MAX( WRKOPT, INT( DWORK(JWORK) ) + JWORK - 1 )
         END IF
C
         IF( FIRST ) THEN
C
C           Continue with Householder with column pivoting.
C
C              ( x x x x x )       ( x x x x x )
C              ( 0 x x x x )       ( 0 x x x x )
C              ( 0 0 x x x )  ->   ( 0 0 x x x )
C              ( 0 0 0 x x )       ( 0 0 0 x x )
C              ( 0 0 0 x x )       ( 0 0 0 0 0 )
C
C           Workspace: need   min(P,M) + 3*M-1.
C           Int.work.  need   M.
C
            JWORK = ITAU + MIN( RO1, M-SIGMA )
C
            IROW = MIN( NR+SIGMA+1, PN )
            ICOL = MIN( SIGMA+1,M )
            CALL MB03OY( RO1, M-SIGMA, ABCD(IROW,ICOL), LDABCD, RCOND,
     $                   SVLMAX, RANK, SVAL, IWORK, DWORK(ITAU),
     $                   DWORK(JWORK), INFO )
            WRKOPT = MAX( WRKOPT, JWORK + 3*( M-SIGMA ) - 1 )
C
C           Apply the column permutations to B and part of D.
C
            CALL DLAPMT( .TRUE., NR+SIGMA, M-SIGMA, ABCD(1,ICOL),
     $                   LDABCD, IWORK )
C
            IF ( RANK.GT.0 ) THEN
C
C              Apply the Householder transformations to the submatrix C.
C              Workspace: need   min(P,M) + N.
C                         prefer min(P,M) + N*NB.
C
               CALL DORMQR( 'Left', 'Transpose', RO1, NR, RANK,
     $                      ABCD(IROW,ICOL), LDABCD, DWORK(ITAU),
     $                      ABCD(IROW,MP1),  LDABCD, DWORK(JWORK),
     $                      LDWORK-JWORK+1, INFO )
               WRKOPT = MAX( WRKOPT, INT( DWORK(JWORK) ) + JWORK - 1 ) 
               CALL DLASET( 'Lower', RO1-1, MIN( RO1-1, RANK ), ZERO,
     $                      ZERO, ABCD(MIN( PN, IROW+1 ),ICOL), LDABCD )
               RO1 = RO1 - RANK
            END IF
         END IF
C
C        Terminate if Dr has maximal row rank.
C
         IF( RO1.EQ.0 )
     $      GO TO 30
C
      END IF
C
C     Update SIGMA.
C
      SIGMA  = PR - RO1
C
      NBLCKS = NBLCKS + 1
      TAUI   = RO1
C
      IF ( NR.LE.0 ) THEN
         PR   = SIGMA
         RANK = 0
      ELSE
C
C        Compress the columns of C using RQ factorization with row
C        pivoting, P * C = R * Q.
C        The current C is the TAUI-by-NR matrix delimited by rows
C        IRC+1 to IRC+TAUI and columns M+1 to M+NR of ABCD.
C        The rank of the current C is computed in MUI.
C        Workspace: need   min(P,N) + 3*P-1.
C        Int.work.  need   P.
C
         IRC   = NR  + SIGMA
         I1    = IRC + 1
         MNTAU = MIN( TAUI, NR )
         JWORK = ITAU + MNTAU
C
         CALL MB03PY( TAUI, NR, ABCD(I1,MP1), LDABCD, RCOND, SVLMAX,
     $                RANK, SVAL, IWORK, DWORK(ITAU), DWORK(JWORK),
     $                INFO )
         WRKOPT = MAX( WRKOPT, JWORK + 3*TAUI - 1 )
C
         IF ( RANK.GT.0 ) THEN
            IROW = I1 + TAUI - RANK
C
C           Apply Q' to the first NR columns of [A; C1] from the right.
C           Workspace: need   min(P,N) +  N + SIGMA; SIGMA <= P;
C                      prefer min(P,N) + (N + SIGMA)*NB.
C
            CALL DORMRQ( 'Right', 'Transpose', IRC, NR, RANK,
     $                   ABCD(IROW,MP1), LDABCD, DWORK(MNTAU-RANK+1),
     $                   ABCD(1,MP1), LDABCD, DWORK(JWORK),
     $                   LDWORK-JWORK+1, INFO )
            WRKOPT = MAX( WRKOPT, INT( DWORK(JWORK) ) + JWORK - 1 )
C
C           Apply Q to the first NR rows and M + NR columns of [ B  A ]
C           from the left.
C           Workspace: need   min(P,N) +  M + N;
C                      prefer min(P,N) + (M + N)*NB.
C
            CALL DORMRQ( 'Left', 'NoTranspose', NR, MNR, RANK,
     $                   ABCD(IROW,MP1), LDABCD, DWORK(MNTAU-RANK+1),
     $                   ABCD, LDABCD, DWORK(JWORK), LDWORK-JWORK+1,
     $                   INFO )
            WRKOPT = MAX( WRKOPT, INT( DWORK(JWORK) ) + JWORK - 1 )
C
            CALL DLASET( 'Full', RANK, NR-RANK, ZERO, ZERO,
     $                   ABCD(IROW,MP1), LDABCD )
            IF ( RANK.GT.1 )
     $         CALL DLASET( 'Lower', RANK-1, RANK-1, ZERO, ZERO,
     $                      ABCD(IROW+1,MP1+NR-RANK), LDABCD )
         END IF
      END IF
C
   20 CONTINUE
      MUI = RANK
      NR  = NR    - MUI
      PR  = SIGMA + MUI
C
C     Set number of left Kronecker blocks of order (i-1)-by-i.
C
      KRONL(NBLCKS) = TAUI - MUI
C
C     Set number of infinite divisors of order i-1.
C
      IF( FIRST .AND. NBLCKS.GT.1 )
     $   INFZ(NBLCKS-1) = MUIM1 - TAUI
      MUIM1 = MUI
      RO    = MUI
C
C     Continue reduction if rank of current C is positive.
C
      IF( MUI.GT.0 )
     $   GO TO 10
C
C     Determine the maximal degree of infinite zeros and the number of
C     infinite zeros.
C
   30 CONTINUE
      IF( FIRST ) THEN
         IF( MUI.EQ.0 ) THEN
            DINFZ = MAX( 0, NBLCKS - 1 )
         ELSE
            DINFZ = NBLCKS
            INFZ(NBLCKS) = MUI
         END IF
         K = DINFZ
C
         DO 40 I = K, 1, -1
            IF( INFZ(I).NE.0 )
     $         GO TO 50
            DINFZ = DINFZ - 1
   40    CONTINUE
C
   50    CONTINUE
C
         DO 60 I = 1, DINFZ
            NINFZ = NINFZ + INFZ(I)*I
   60    CONTINUE
C
      END IF
C
C     Determine the maximal order of left elementary Kronecker blocks.
C
      NKRONL = NBLCKS
C
      DO 70 I = NBLCKS, 1, -1
         IF( KRONL(I).NE.0 )
     $      GO TO 80
         NKRONL = NKRONL - 1
   70 CONTINUE
C
   80 CONTINUE
C
      DWORK(1) = WRKOPT
      RETURN
C *** Last line of AB08NY ***
      END
