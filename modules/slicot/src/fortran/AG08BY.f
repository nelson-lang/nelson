      SUBROUTINE AG08BY( FIRST, N, M, P, SVLMAX, ABCD, LDABCD, E, LDE,
     $                   NR, PR, NINFZ, DINFZ, NKRONL, INFZ, KRONL,
     $                   TOL, IWORK, DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To extract from the (N+P)-by-(M+N) descriptor system pencil
C
C        S(lambda) = ( B   A - lambda*E  )
C                    ( D        C        )
C
C     with E nonsingular and upper triangular a
C     (NR+PR)-by-(M+NR) "reduced" descriptor system pencil
C
C                           ( Br  Ar-lambda*Er )
C              Sr(lambda) = (                  )
C                           ( Dr     Cr        )
C
C     having the same finite Smith zeros as the pencil
C     S(lambda) but with Dr, a PR-by-M full row rank
C     left upper trapezoidal matrix, and Er, an NR-by-NR
C     upper triangular nonsingular matrix.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     FIRST   LOGICAL
C             Specifies if AG08BY is called first time or it is called
C             for an already reduced system, with D full column rank
C             with the last M rows in upper triangular form:
C             FIRST = .TRUE.,  first time called;
C             FIRST = .FALSE., not first time called.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The number of rows of matrix B, the number of columns of
C             matrix C and the order of square matrices A and E.
C             N >= 0.
C
C     M       (input) INTEGER
C             The number of columns of matrices B and D.  M >= 0.
C             M <= P if FIRST = .FALSE. .
C
C     P       (input) INTEGER
C             The number of rows of matrices C and D.  P >= 0.
C
C     SVLMAX  (input) DOUBLE PRECISION
C             During each reduction step, the rank-revealing QR
C             factorization of a matrix stops when the estimated minimum
C             singular value is smaller than TOL * MAX(SVLMAX,EMSV),
C             where EMSV is the estimated maximum singular value.
C             SVLMAX >= 0.
C
C     ABCD    (input/output) DOUBLE PRECISION array, dimension
C             (LDABCD,M+N)
C             On entry, the leading (N+P)-by-(M+N) part of this array
C             must contain the compound matrix
C                      (  B   A  ) ,
C                      (  D   C  )
C             where A is an N-by-N matrix, B is an N-by-M matrix,
C             C is a P-by-N matrix and D is a P-by-M matrix.
C             If FIRST = .FALSE., then D must be a full column
C             rank matrix with the last M rows in upper triangular form.
C             On exit, the leading (NR+PR)-by-(M+NR) part of ABCD
C             contains the reduced compound matrix
C                       (  Br  Ar ) ,
C                       (  Dr  Cr )
C             where Ar is an NR-by-NR matrix, Br is an NR-by-M matrix,
C             Cr is a PR-by-NR matrix, Dr is a PR-by-M full row rank
C             left upper trapezoidal matrix with the first PR columns
C             in upper triangular form.
C
C     LDABCD  INTEGER
C             The leading dimension of array ABCD.
C             LDABCD >= MAX(1,N+P).
C
C     E       (input/output) DOUBLE PRECISION array, dimension (LDE,N)
C             On entry, the leading N-by-N part of this array must
C             contain the upper triangular nonsingular matrix E.
C             On exit, the leading NR-by-NR part contains the reduced
C             upper triangular nonsingular matrix Er.
C
C     LDE     INTEGER
C             The leading dimension of array E.  LDE >= MAX(1,N).
C
C     NR      (output) INTEGER
C             The order of the reduced matrices Ar and Er; also the
C             number of rows of the reduced matrix Br and the number
C             of columns of the reduced matrix Cr.
C             If Dr is invertible, NR is also the number of finite
C             Smith zeros.
C
C     PR      (output) INTEGER
C             The rank of the resulting matrix Dr; also the number of
C             rows of reduced matrices Cr and Dr.
C
C     NINFZ   (output) INTEGER
C             Number of infinite zeros.  NINFZ = 0 if FIRST = .FALSE. .
C
C     DINFZ   (output) INTEGER
C             The maximal multiplicity of infinite zeros.
C             DINFZ = 0 if FIRST = .FALSE. .
C
C     NKRONL  (output) INTEGER
C             The maximal dimension of left elementary Kronecker blocks.
C
C     INFZ    (output) INTEGER array, dimension (N)
C             INFZ(i) contains the number of infinite zeros of
C             degree i, where i = 1,2,...,DINFZ.
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
C             If the user sets TOL <= 0, then an implicitly computed,
C             default tolerance TOLDEF = (N+P)*(N+M)*EPS,  is used
C             instead, where EPS is the machine precision
C             (see LAPACK Library routine DLAMCH).
C             NOTE that when SVLMAX > 0, the estimated ranks could be
C             less than those defined above (see SVLMAX).  TOL <= 1.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (M)
C             If FIRST = .FALSE., IWORK is not referenced.
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The length of the array DWORK.
C             LDWORK >= 1, if P = 0; otherwise
C             LDWORK >= MAX( 1, N+M-1, MIN(P,M) + MAX(3*M-1,N), 5*P ),
C                                             if FIRST = .TRUE.;
C             LDWORK >= MAX( 1, N+M-1, 5*P ), if FIRST = .FALSE. .
C             The second term is not needed if M = 0.
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
C     METHOD
C
C     The subroutine is based on the reduction algorithm of [1].
C
C     REFERENCES
C
C     [1] P. Misra, P. Van Dooren and A. Varga.
C         Computation of structural invariants of generalized
C         state-space systems.
C         Automatica, 30, pp. 1921-1936, 1994.
C
C     NUMERICAL ASPECTS
C
C     The algorithm is numerically backward stable and requires
C     0( (P+N)*(M+N)*N )  floating point operations.
C
C     FURTHER COMMENTS
C
C     The number of infinite zeros is computed as
C
C                   DINFZ
C        NINFZ =     Sum  (INFZ(i)*i) .
C                    i=1
C     Note that each infinite zero of multiplicity k corresponds to
C     an infinite eigenvalue of multiplicity k+1.
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
C     A. Varga, German Aerospace Center, DLR Oberpfaffenhofen.
C     May 1999. Based on the RASP routine SRISEP.
C
C     REVISIONS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Sep. 1999,
C     Jan. 2009, Apr. 2009.
C     A. Varga, DLR Oberpfaffenhofen, March 2002.
C     V. Sima, Jan. 2010, following Bujanovic and Drmac's suggestion.
C     V. Sima, Apr. 2011, Mar. 2019.
C
C     KEYWORDS
C
C     Generalized eigenvalue problem, Kronecker indices, multivariable
C     system, orthogonal transformation, structural invariant.
C
C     ******************************************************************
C
C     .. Parameters ..
      INTEGER            IMAX, IMIN
      PARAMETER          ( IMAX = 1, IMIN = 2 )
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D0, ZERO = 0.0D0 )
C     .. Scalar Arguments ..
      INTEGER            DINFZ, INFO, LDABCD, LDE, LDWORK, M, N, NINFZ,
     $                   NKRONL, NR, P, PR
      DOUBLE PRECISION   SVLMAX, TOL
      LOGICAL            FIRST
C     .. Array Arguments ..
      INTEGER            INFZ( * ), IWORK(*), KRONL( * )
      DOUBLE PRECISION   ABCD( LDABCD, * ), DWORK( * ), E( LDE, * )
C     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            I, ICOL, ILAST, IRC, IROW, ISMAX, ISMIN, ITAU,
     $                   J, JLAST, JWORK1, JWORK2, K, MN, MN1, MNR,
     $                   MNTAU, MP1, MPM, MUI, MUIM1, N1, NBLCKS, PN,
     $                   RANK, RO, RO1, SIGMA, TAUI, WRKOPT
      DOUBLE PRECISION   C, C1, C2, RCOND, S, S1, S2, SMAX, SMAXPR,
     $                   SMIN, SMINPR, T, TOLZ, TT
C     .. Local Arrays ..
      DOUBLE PRECISION   DUM(1), SVAL(3)
C     .. External Functions ..
      INTEGER            IDAMAX
      DOUBLE PRECISION   DLAMCH, DNRM2
      EXTERNAL           DLAMCH, DNRM2, IDAMAX
C     .. External Subroutines ..
      EXTERNAL           DCOPY, DLAIC1, DLAPMT, DLARFG, DLARTG, DLASET,
     $                   SLCT_DLATZM, DORMQR, DROT, DSWAP, MB03OY,
     $                   XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, INT, MAX, MIN, SQRT
C     .. Executable Statements ..
C
C     Test the input parameters.
C
      LQUERY = ( LDWORK.EQ.-1 )
      INFO = 0
      PN   = P + N
      MN   = M + N
      MPM  = MIN( P, M )
      IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( M.LT.0 .OR. ( .NOT.FIRST .AND. M.GT.P ) ) THEN
         INFO = -3
      ELSE IF( P.LT.0 ) THEN
         INFO = -4
      ELSE IF( SVLMAX.LT.ZERO ) THEN
         INFO = -5
      ELSE IF( LDABCD.LT.MAX( 1, PN ) ) THEN
         INFO = -7
      ELSE IF( LDE.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE IF( TOL.GT.ONE ) THEN
         INFO = -17
      ELSE
         WRKOPT = MAX( 1, 5*P )
         IF( P.GT.0 ) THEN
            IF( M.GT.0 ) THEN
               WRKOPT = MAX( WRKOPT, MN-1 )
               IF( FIRST ) THEN
                  WRKOPT = MAX( WRKOPT, MPM + MAX( 3*M-1, N ) )
                  IF( LQUERY ) THEN
                     CALL DORMQR( 'Left', 'Transpose', P, N, MPM, ABCD,
     $                            LDABCD, DWORK, ABCD, LDABCD, DWORK,
     $                            -1, INFO )
                     WRKOPT = MAX( WRKOPT, MPM + INT( DWORK(1) ) )
                  END IF
               END IF
            END IF
         END IF
         IF( LDWORK.LT.WRKOPT .AND. .NOT.LQUERY ) THEN
            INFO = -20
         END IF
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'AG08BY', -INFO )
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
      DINFZ  = 0
      NINFZ  = 0
      NKRONL = 0
C
C     Quick return if possible.
C
      IF( P.EQ.0 ) THEN
         DWORK(1) = ONE
         RETURN
      END IF
      IF( N.EQ.0 .AND. M.EQ.0 ) THEN
         PR = 0
         NKRONL   = 1
         KRONL(1) = P
         DWORK(1) = ONE
         RETURN
      END IF
C
      TOLZ  = SQRT( DLAMCH( 'Epsilon' ) )
      RCOND = TOL
      IF( RCOND.LE.ZERO ) THEN
C
C        Use the default tolerance in rank determination.
C
         RCOND = DBLE( PN*MN )*DLAMCH( 'EPSILON' )
      END IF
C
C     The D matrix is (RO+SIGMA)-by-M, where RO = P - SIGMA and
C     SIGMA = 0 for FIRST = .TRUE. and SIGMA = M for FIRST = .FALSE..
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
      DUM(1) = ZERO
C
      ITAU   = 1
      JWORK1 = ITAU + MPM
      ISMIN  = 2*P + 1
      ISMAX  = ISMIN + P
      JWORK2 = ISMAX + P
      NBLCKS = 0
      WRKOPT = 1
C
   10 IF( PR.EQ.0 ) GO TO 90
C
C     (NR+1,ICOL+1) points to the current position of matrix D.
C
      RO1 = RO
      MNR = M + NR
      IF( M.GT.0 ) THEN
C
C        Compress rows of D; first exploit the trapezoidal shape of the
C        (RO+SIGMA)-by-SIGMA matrix in the first SIGMA columns of D;
C        compress the first SIGMA columns without column pivoting:
C
C              ( x x x x x )       ( x x x x x )
C              ( x x x x x )       ( 0 x x x x )
C              ( x x x x x )  - >  ( 0 0 x x x )
C              ( 0 x x x x )       ( 0 0 0 x x )
C              ( 0 0 x x x )       ( 0 0 0 x x )
C
C        where SIGMA = 3 and RO = 2.
C        Workspace: need maximum M+N-1.
C
         IROW = NR
         DO 20 ICOL = 1, SIGMA
            IROW = IROW + 1
            CALL DLARFG( RO+1, ABCD(IROW,ICOL), ABCD(IROW+1,ICOL), 1,
     $                   T )
            CALL SLCT_DLATZM( 'L', RO+1, MNR-ICOL, ABCD(IROW+1,ICOL),
     $                   1, T,
     $                   ABCD(IROW,ICOL+1), ABCD(IROW+1,ICOL+1),
     $                   LDABCD, DWORK )
            CALL DCOPY( PR-ICOL, DUM, 0, ABCD(IROW+1,ICOL), 1 )
   20    CONTINUE
         WRKOPT = MAX( WRKOPT, MN - 1 )
C
         IF( FIRST ) THEN
C
C           Continue with Householder with column pivoting.
C
C              ( x x x x x )        ( x x x x x )
C              ( 0 x x x x )        ( 0 x x x x )
C              ( 0 0 x x x )  - >   ( 0 0 x x x )
C              ( 0 0 0 x x )        ( 0 0 0 x x )
C              ( 0 0 0 x x )        ( 0 0 0 0 0 )
C
C           Real workspace:    need maximum min(P,M)+3*M-1;
C           Integer workspace: need maximum M.
C
            IROW = MIN( NR+SIGMA+1, PN )
            ICOL = MIN( SIGMA+1, M )
            CALL MB03OY( RO1, M-SIGMA, ABCD(IROW,ICOL), LDABCD,
     $                   RCOND, SVLMAX, RANK, SVAL, IWORK, DWORK(ITAU),
     $                   DWORK(JWORK1), INFO )
            WRKOPT = MAX( WRKOPT, JWORK1 + 3*M - 2 )
C
C           Apply the column permutations to B and part of D.
C
            CALL DLAPMT( .TRUE., NR+SIGMA, M-SIGMA, ABCD(1,ICOL),
     $                   LDABCD, IWORK )
C
            IF( RANK.GT.0 ) THEN
C
C              Apply the Householder transformations to the submatrix C.
C              Workspace: need   maximum min(P,M) + N;
C                         prefer maximum min(P,M) + N*NB.
C
               CALL DORMQR( 'Left', 'Transpose', RO1, NR, RANK,
     $                      ABCD(IROW,ICOL), LDABCD, DWORK(ITAU),
     $                      ABCD(IROW,MP1), LDABCD, DWORK(JWORK1),
     $                      LDWORK-JWORK1+1, INFO )
               WRKOPT = MAX( WRKOPT, JWORK1 + INT( DWORK(JWORK1) ) - 1 )
               CALL DLASET( 'Lower', RO1-1, MIN( RO1-1, RANK ), ZERO,
     $                      ZERO, ABCD(MIN( IROW+1, PN ),ICOL), LDABCD )
               RO1 = RO1 - RANK
            END IF
         END IF
C
C        Terminate if Dr has maximal row rank.
C
         IF( RO1.EQ.0 ) GO TO 90
C
      END IF
C
C     Update SIGMA.
C
      SIGMA = PR - RO1
C
      NBLCKS = NBLCKS + 1
      TAUI = RO1
C
C     Compress the columns of current C to separate a TAUI-by-MUI
C     full column rank block.
C
      IF( NR.EQ.0 ) THEN
C
C        Finish for zero state dimension.
C
         PR = SIGMA
         RANK = 0
      ELSE
C
C        Perform RQ-decomposition with row pivoting on the current C
C        while keeping E upper triangular.
C        The current C is the TAUI-by-NR matrix delimited by rows
C        IRC+1 to IRC+TAUI and columns M+1 to M+NR of ABCD.
C        The rank of current C is computed in MUI.
C        Workspace: need maximum 5*P.
C
         IRC = NR + SIGMA
         N1  = NR
         IF( TAUI.GT.1 ) THEN
C
C           Compute norms.
C
            DO 30 I = 1, TAUI
               DWORK(I) = DNRM2( NR, ABCD(IRC+I,MP1), LDABCD )
               DWORK(P+I) = DWORK(I)
   30       CONTINUE
         END IF
C
         RANK  = 0
         MNTAU = MIN( TAUI, NR )
C
C        ICOL and IROW will point to the current pivot position in C.
C
         ILAST = NR + PR
         JLAST = M  + NR
         IROW = ILAST
         ICOL = JLAST
         I = TAUI
   40    IF( RANK.LT.MNTAU ) THEN
            MN1 = M + N1
C
C           Pivot if necessary.
C
            IF( I.NE.1 ) THEN
               J = IDAMAX( I, DWORK, 1 )
               IF( J.NE.I ) THEN
                  DWORK(J) = DWORK(I)
                  DWORK(P+J) = DWORK(P+I)
                  CALL DSWAP( N1, ABCD(IROW,MP1), LDABCD,
     $                        ABCD(IRC+J,MP1), LDABCD )
               END IF
            END IF
C
C           Zero elements left to ABCD(IROW,ICOL).
C
            DO 50 K = 1, N1-1
               J = M + K
C
C              Rotate columns J, J+1 to zero ABCD(IROW,J).
C
               T = ABCD(IROW,J+1)
               CALL DLARTG( T, ABCD(IROW,J), C, S, ABCD(IROW,J+1) )
               ABCD(IROW,J) = ZERO
               CALL DROT( IROW-1, ABCD(1,J+1), 1, ABCD(1,J), 1, C, S )
               CALL DROT( K+1, E(1,K+1), 1, E(1,K), 1, C, S )
C
C              Rotate rows K, K+1 to zero E(K+1,K).
C
               T = E(K,K)
               CALL DLARTG( T, E(K+1,K), C, S, E(K,K) )
               E(K+1,K) = ZERO
               CALL DROT( N1-K, E(K,K+1), LDE, E(K+1,K+1), LDE, C, S )
               CALL DROT( MN1, ABCD(K,1), LDABCD, ABCD(K+1,1), LDABCD,
     $                    C, S )
   50       CONTINUE
C
            IF( RANK.EQ.0 ) THEN
C
C              Initialize; exit if matrix is zero (RANK = 0).
C
               SMAX = ABS( ABCD(ILAST,JLAST) )
               IF ( SMAX.EQ.ZERO ) GO TO 80
               SMIN   = SMAX
               SMAXPR = SMAX
               SMINPR = SMIN
               C1 = ONE
               C2 = ONE
            ELSE
C
C              One step of incremental condition estimation.
C
               CALL DCOPY( RANK, ABCD(IROW,ICOL+1), LDABCD,
     $                     DWORK(JWORK2), 1 )
               CALL DLAIC1( IMIN, RANK, DWORK( ISMIN ), SMIN,
     $                      DWORK(JWORK2), ABCD(IROW,ICOL), SMINPR, S1,
     $                      C1 )
               CALL DLAIC1( IMAX, RANK, DWORK( ISMAX ), SMAX,
     $                      DWORK(JWORK2), ABCD(IROW,ICOL), SMAXPR, S2,
     $                      C2 )
               WRKOPT = MAX( WRKOPT, 5*P )
            END IF
C
C           Check the rank; finish the loop if rank loss occurs.
C
            IF( SVLMAX*RCOND.LE.SMAXPR ) THEN
               IF( SVLMAX*RCOND.LE.SMINPR ) THEN
                  IF( SMAXPR*RCOND.LT.SMINPR ) THEN
C
C                    Finish the loop if last row.
C
                     IF( N1.EQ.0 ) THEN
                        RANK = RANK + 1
                        GO TO 80
                     END IF
C
                     IF( N1.GT.1 ) THEN
C
C                       Update norms.
C
                        IF( I-1.GT.1 ) THEN
                           DO 60 J = 1, I - 1
                              IF( DWORK(J).NE.ZERO ) THEN
                                 T = ABS( ABCD(IRC+J,ICOL) ) / DWORK(J)
                                 T = MAX( ( ONE + T )*( ONE - T ), ZERO)
                                 TT = T*( DWORK(J)/DWORK(P+J) )**2
                                 IF( TT.GT.TOLZ ) THEN
                                    DWORK(J) = DWORK(J)*SQRT( T )
                                 ELSE
                                    DWORK(J) = DNRM2( N1-1,
     $                                         ABCD(IRC+J,MP1), LDABCD )
                                    DWORK(P+J) = DWORK(J)
                                 END IF
                              END IF
   60                      CONTINUE
                        END IF
                     END IF
C
                     DO 70 J = 1, RANK
                        DWORK( ISMIN+J-1 ) = S1*DWORK( ISMIN+J-1 )
                        DWORK( ISMAX+J-1 ) = S2*DWORK( ISMAX+J-1 )
   70                CONTINUE
C
                     DWORK( ISMIN+RANK ) = C1
                     DWORK( ISMAX+RANK ) = C2
                     SMIN = SMINPR
                     SMAX = SMAXPR
                     RANK = RANK + 1
                     ICOL = ICOL - 1
                     IROW = IROW - 1
                     N1 = N1 - 1
                     I  = I  - 1
                     GO TO 40
                  END IF
               END IF
            END IF
         END IF
      END IF
C
   80 CONTINUE
      MUI = RANK
      NR  = NR - MUI
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
      RO = MUI
C
C     Continue reduction if rank of current C is positive.
C
      IF( MUI.GT.0 )
     $   GO TO 10
C
C     Determine the maximal degree of infinite zeros and
C     the number of infinite zeros.
C
   90 CONTINUE
      IF( FIRST ) THEN
         IF( MUI.EQ.0 ) THEN
            DINFZ = MAX( 0, NBLCKS - 1 )
         ELSE
            DINFZ = NBLCKS
            INFZ(NBLCKS) = MUI
         END IF
         K = DINFZ
         DO 100 I = K, 1, -1
            IF( INFZ(I).NE.0 ) GO TO 110
            DINFZ = DINFZ - 1
  100    CONTINUE
  110    CONTINUE
         DO 120 I = 1, DINFZ
            NINFZ = NINFZ + INFZ(I)*I
  120    CONTINUE
      END IF
C
C     Determine the maximal order of left elementary Kronecker blocks.
C
      NKRONL = NBLCKS
      DO 130 I = NBLCKS, 1, -1
         IF( KRONL(I).NE.0 ) GO TO 140
         NKRONL = NKRONL - 1
  130 CONTINUE
  140 CONTINUE
C
      DWORK(1) = WRKOPT
      RETURN
C *** Last line of AG08BY ***
      END
