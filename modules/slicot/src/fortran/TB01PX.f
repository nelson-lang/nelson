      SUBROUTINE TB01PX( JOB, EQUIL, N, M, P, A, LDA, B, LDB, C, LDC,
     $                   NR, INFRED, TOL, IWORK, DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To find a reduced (controllable, observable, or minimal) state-
C     space representation (Ar,Br,Cr) for any original state-space
C     representation (A,B,C). The matrix Ar is in an upper block
C     Hessenberg staircase form.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOB     CHARACTER*1
C             Indicates whether the user wishes to remove the
C             uncontrollable and/or unobservable parts as follows:
C             = 'M':  Remove both the uncontrollable and unobservable
C                     parts to get a minimal state-space representation;
C             = 'C':  Remove the uncontrollable part only to get a
C                     controllable state-space representation;
C             = 'O':  Remove the unobservable part only to get an
C                     observable state-space representation.
C
C     EQUIL   CHARACTER*1
C             Specifies whether the user wishes to preliminarily balance
C             the triplet (A,B,C) as follows:
C             = 'S':  Perform balancing (scaling);
C             = 'N':  Do not perform balancing.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the original state-space representation, i.e.
C             the order of the matrix A.  N >= 0.
C
C     M       (input) INTEGER
C             The number of system inputs.  M >= 0.
C
C     P       (input) INTEGER
C             The number of system outputs.   P >= 0.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the original state dynamics matrix A.
C             On exit, if INFRED(1) >= 0 and/or INFRED(2) >= 0, the
C             leading NR-by-NR part of this array contains the upper
C             block Hessenberg state dynamics matrix Ar of a minimal,
C             controllable, or observable realization for the original
C             system, depending on the value of JOB, JOB = 'M',
C             JOB = 'C', or JOB = 'O', respectively.
C             The block structure of the resulting staircase form is
C             contained in the leading INFRED(4) elements of IWORK.
C             If INFRED(1:2) < 0, then A contains the original matrix.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     B       (input/output) DOUBLE PRECISION array, dimension (LDB,M),
C             if JOB = 'C', or (LDB,MAX(M,P)), otherwise.
C             On entry, the leading N-by-M part of this array must
C             contain the original input/state matrix B; if JOB = 'M',
C             or JOB = 'O', the remainder of the leading N-by-MAX(M,P)
C             part is used as internal workspace.
C             On exit, if INFRED(1) >= 0 and/or INFRED(2) >= 0, the
C             leading NR-by-M part of this array contains the
C             input/state matrix Br of a minimal, controllable, or
C             observable realization for the original system, depending
C             on the value of JOB, JOB = 'M', JOB = 'C', or JOB = 'O',
C             respectively. If JOB = 'C', only the first IWORK(1) rows
C             of B are nonzero.
C             If INFRED(1:2) < 0, then B contains the original matrix.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= MAX(1,N).
C
C     C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
C             On entry, the leading P-by-N part of this array must
C             contain the original state/output matrix C; if JOB = 'M',
C             or JOB = 'O', the remainder of the leading MAX(M,P)-by-N
C             part is used as internal workspace.
C             On exit, if INFRED(1) >= 0 and/or INFRED(2) >= 0, the
C             leading P-by-NR part of this array contains the
C             state/output matrix Cr of a minimal, controllable, or
C             observable realization for the original system, depending
C             on the value of JOB, JOB = 'M', JOB = 'C', or JOB = 'O',
C             respectively. If JOB = 'M', or JOB = 'O', only the last
C             IWORK(1) columns (in the first NR columns) of C are
C             nonzero.
C             If INFRED(1:2) < 0, then C contains the original matrix.
C
C     LDC     INTEGER
C             The leading dimension of the array C.
C             LDC >= MAX(1,M,P), if N > 0.
C             LDC >= 1,          if N = 0.
C
C     NR      (output) INTEGER
C             The order of the reduced state-space representation
C             (Ar,Br,Cr) of a minimal, controllable, or observable
C             realization for the original system, depending on
C             JOB = 'M', JOB = 'C', or JOB = 'O'.
C
C     INFRED  (output) INTEGER array, dimension 4
C             This array contains information on the performed reduction
C             and on structure of resulting system matrices, as follows:
C             INFRED(k) >= 0 (k = 1 or 2) if Phase k of the reduction
C                            (see METHOD) has been performed. In this
C                            case, INFRED(k) is the achieved order
C                            reduction in Phase k.
C             INFRED(k) < 0  (k = 1 or 2) if Phase k was not performed.
C                            This can also appear when Phase k was
C                            tried, but did not reduce the order, if
C                            enough workspace is provided for saving the
C                            system matrices (see LDWORK description).
C             INFRED(3)  -   the number of nonzero subdiagonals of A.
C             INFRED(4)  -   the number of blocks in the resulting
C                            staircase form at the last performed
C                            reduction phase. The block dimensions are
C                            contained in the first INFRED(4) elements
C                            of IWORK.
C
C     Tolerances
C
C     TOL     DOUBLE PRECISION
C             The tolerance to be used in rank determinations when
C             transforming (A, B, C). If the user sets TOL > 0, then
C             the given value of TOL is used as a lower bound for the
C             reciprocal condition number (see the description of the
C             argument RCOND in the SLICOT routine MB03OD);  a
C             (sub)matrix whose estimated condition number is less than
C             1/TOL is considered to be of full rank.  If the user sets
C             TOL <= 0, then an implicitly computed, default tolerance
C             (determined by the SLICOT routine TB01UD) is used instead.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (c*N+MAX(M,P)), where
C             c = 2, if JOB = 'M', and c = 1, otherwise.
C             On exit, if INFO = 0, the first INFRED(4) elements of
C             IWORK return the orders of the diagonal blocks of A.
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The length of the array DWORK.  LDWORK >= 1, and if N > 0,
C             LDWORK >= N + MAX(N, 3*M, 3*P).
C             For optimum performance LDWORK should be larger.
C             If LDWORK >= MAX(1, N + MAX(N, 3*M, 3*P) + N*(N+M+P) ),
C             then more accurate results are to be expected by accepting
C             only those reductions phases (see METHOD), where effective
C             order reduction occurs. This is achieved by saving the
C             system matrices before each phase and restoring them if
C             no order reduction took place in that phase.
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
C     The order reduction is performed in two phases:
C
C     Phase 1:
C     If JOB = 'M' or 'C', the pair (A,B) is reduced by orthogonal
C     similarity transformations to the controllability staircase form
C     (see [1]) and a controllable realization (Ac,Bc,Cc) is extracted.
C     Ac results in an upper block Hessenberg form.
C
C     Phase 2:
C     If JOB = 'M' or 'O', the same algorithm is applied to the dual
C     of the controllable realization (Ac,Bc,Cc), or to the dual of
C     the original system, respectively, to extract an observable
C     realization (Ar,Br,Cr). If JOB = 'M', the resulting realization
C     is also controllable, and thus minimal.
C     Ar results in an upper block Hessenberg form.
C
C     REFERENCES
C
C     [1] Van Dooren, P.
C         The Generalized Eigenstructure Problem in Linear System
C         Theory. (Algorithm 1)
C         IEEE Trans. Auto. Contr., AC-26, pp. 111-129, 1981.
C
C     NUMERICAL ASPECTS
C                               3
C     The algorithm requires 0(N ) operations and is backward stable.
C
C     CONTRIBUTOR
C
C     V. Sima, Katholieke Univ. Leuven, Belgium.
C     A. Varga, DLR - Oberpfaffenhofen, April 1999.
C
C     REVISIONS
C
C     A. Varga, DLR - Oberpfaffenhofen, March 2002.
C     V.Sima, Dec. 2016, Mar. 2017.
C
C     KEYWORDS
C
C     Hessenberg form, minimal realization, multivariable system,
C     orthogonal transformation, state-space model, state-space
C     representation.
C
C     ******************************************************************
C
C     .. Parameters ..
      INTEGER           LDIZ
      PARAMETER         ( LDIZ = 1 )
      DOUBLE PRECISION  ONE, HUNDR
      PARAMETER         ( ONE = 1.0D0, HUNDR = 100.0D0 )
C     .. Scalar Arguments ..
      CHARACTER         EQUIL, JOB
      INTEGER           INFO, LDA, LDB, LDC, LDWORK, M, N, NR, P
      DOUBLE PRECISION  TOL
C     .. Array Arguments ..
      INTEGER           INFRED(*), IWORK(*)
      DOUBLE PRECISION  A(LDA,*), B(LDB,*), C(LDC,*), DWORK(*)
C     .. Local Scalars ..
      LOGICAL           LEQUIL, LNJOBC, LNJOBO, LSPACE
      INTEGER           I, IB, ICON, INDCON, ITAU, IZ, JWORK, KL, KWA,
     $                  KWB, KWC, LDWMIN, MAXMP, NCONT, WRKOPT
      DOUBLE PRECISION  ANORM, BNORM, CNORM, MAXRED
C     .. External Functions ..
      LOGICAL           LSAME
      DOUBLE PRECISION  DLANGE
      EXTERNAL          DLANGE, LSAME
C     .. External Subroutines ..
      EXTERNAL          AB07MD, DLACPY, TB01ID, TB01UD, TB01XD, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         INT, MAX
C     .. Executable Statements ..
C
      INFO   = 0
      MAXMP  = MAX( M, P )
      LDWMIN = N + MAX( N, 3*MAXMP )
      LNJOBC = .NOT.LSAME( JOB,   'C' )
      LNJOBO = .NOT.LSAME( JOB,   'O' )
      LEQUIL =      LSAME( EQUIL, 'S' )
C
C     Test the input scalar arguments.
C
      IF( LNJOBC .AND. LNJOBO .AND. .NOT.LSAME( JOB, 'M' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.LEQUIL .AND. .NOT.LSAME( EQUIL, 'N' ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( M.LT.0 ) THEN
         INFO = -4
      ELSE IF( P.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE IF( LDC.LT.1 .OR. ( N.GT.0 .AND. LDC.LT.MAXMP ) ) THEN
         INFO = -11
      ELSE IF( LDWORK.LT.1 .OR. ( N.GT.0 .AND. LDWORK.LT.LDWMIN ) ) THEN
         INFO = -17
      END IF
C
      IF ( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'TB01PX', -INFO )
         RETURN
      END IF
C
      INFRED(1) = -1
      INFRED(2) = -1
      INFRED(4) = 0
C
C     Quick return if possible.
C
      IF ( N.EQ.0 .OR. ( LNJOBC .AND. P.EQ.0 ) .OR.
     $                 ( LNJOBO .AND. M.EQ.0 ) ) THEN
         NR = 0
         INFRED(3) = 0
         DWORK(1)  = ONE
         RETURN
      END IF
      ANORM = DLANGE( '1-norm', N, N, A, LDA, DWORK )
      BNORM = DLANGE( '1-norm', N, M, B, LDB, DWORK )
      CNORM = DLANGE( '1-norm', P, N, C, LDC, DWORK )
*
*     In case of absolute error control, the following
*     squence can be employed:
*
*     IF( .NOT. LEQUIL .AND. BNORM.LE.TOL ) THEN
*        NR = 0
*        INFRED(1) = N
*        DWORK(1)  = ONE
*        RETURN
*     END IF
*     IF( .NOT. LEQUIL .AND. CNORM.LE.TOL ) THEN
*        NR = 0
*        INFRED(2) = N
*        DWORK(1)  = ONE
*        RETURN
*     END IF
C
C     If required, balance the triplet (A,B,C).
C     Workspace: need N.
C
C     (Note: Comments in the code beginning "Workspace:" describe the
C     minimal amount of real workspace needed at that point in the code,
C     as well as the preferred amount for good performance.)
C
      IF ( LEQUIL ) THEN
         MAXRED = MAX( HUNDR, ANORM, BNORM, CNORM )
         CALL TB01ID( 'A', N, M, P, MAXRED, A, LDA, B, LDB, C, LDC,
     $                DWORK, INFO )
      END IF
C
C     Set large workspace option and determine offsets.
C
      LSPACE = LDWORK.GE.N*( N + M + P ) + LDWMIN
      IF ( LSPACE ) THEN
         KWA = 1
         KWB = KWA + N*N
         KWC = KWB + N*M
         IZ  = KWC + P*N
      ELSE
         IZ  = 1
      END IF
C
      ITAU  = IZ
      JWORK = ITAU + N
      KL = MAX( 0, N-1 )
      IB = 1
C
      IF ( LNJOBO ) THEN
C
C        Phase 1: Eliminate uncontrolable eigenvalues.
C
         IF( LSPACE ) THEN
C
C           Save system matrices.
C
            CALL DLACPY( 'Full', N, N, A, LDA, DWORK(KWA), N )
            CALL DLACPY( 'Full', N, M, B, LDB, DWORK(KWB), N )
            CALL DLACPY( 'Full', P, N, C, LDC, DWORK(KWC), MAX( 1, P ) )
         END IF
C
C        Separate out controllable subsystem (of order NCONT):
C        A <-- Z'*A*Z,  B <-- Z'*B,  C <-- C*Z.
C
C        Workspace: need   N + MAX(N, 3*M, P).
C                   prefer larger.
C
         CALL TB01UD( 'No Z', N, M, P, A, LDA, B, LDB, C, LDC, NCONT,
     $                ICON, IWORK, DWORK(IZ), LDIZ, DWORK(ITAU), TOL,
     $                IWORK(N+1), DWORK(JWORK), LDWORK-JWORK+1, INFO )
C
         WRKOPT = INT( DWORK(JWORK) ) + JWORK - 1
C
         IF( NCONT.LT.N .OR. .NOT.LSPACE ) THEN
            IF( ICON.GT.1 ) THEN
               KL = IWORK(1) + IWORK(2) - 1
            ELSE IF( ICON.EQ.1 ) THEN
               KL = IWORK(1) - 1
            ELSE
               KL = 0
            END IF
            INFRED(1) = N - NCONT
            INFRED(4) = ICON
            IF ( LNJOBC )
     $         IB = N + 1
         ELSE
C
C           Restore system matrices.
C
            CALL DLACPY( 'Full', N, N, DWORK(KWA), N, A, LDA )
            CALL DLACPY( 'Full', N, M, DWORK(KWB), N, B, LDB )
            CALL DLACPY( 'Full', P, N, DWORK(KWC), MAX( 1, P ), C, LDC )
         END IF
      ELSE
         NCONT = N
      END IF
C
      IF ( LNJOBC ) THEN
C
C        Phase 2: Eliminate unobservable eigenvalues.
C
         IF( LSPACE .AND. ( ( LNJOBO .AND. NCONT.LT.N ) .OR.
     $                   .NOT.LNJOBO ) ) THEN
C
C           Save system matrices.
C
            CALL DLACPY( 'Full', NCONT, NCONT, A, LDA, DWORK(KWA), N )
            CALL DLACPY( 'Full', NCONT, M,     B, LDB, DWORK(KWB), N )
            CALL DLACPY( 'Full', P,     NCONT, C, LDC, DWORK(KWC),
     $                   MAX( 1, P ) )
         END IF
C
C        Separate out the observable subsystem (of order NR):
C        Form the dual of the subsystem of order NCONT (which is
C        controllable, if JOB = 'M'), leaving the rest as it is.
C
         CALL AB07MD( 'Z', NCONT, M, P, A, LDA, B, LDB, C, LDC, DWORK,
     $                1, INFO )
C
C        And separate out the controllable part of this dual subsystem.
C
C        Workspace: need   NCONT + MAX(NCONT, 3*P, M).
C                   prefer larger.
C
         CALL TB01UD( 'No Z', NCONT, P, M, A, LDA, B, LDB, C, LDC, NR,
     $                INDCON, IWORK(IB), DWORK(IZ), LDIZ, DWORK(ITAU),
     $                TOL, IWORK(IB+N), DWORK(JWORK), LDWORK-JWORK+1,
     $                INFO )
C
         WRKOPT = MAX( WRKOPT, INT( DWORK(JWORK) ) + JWORK - 1 )
C
C        Transpose and reorder (to get a block upper Hessenberg
C        matrix A), giving, for JOB = 'M', the controllable and
C        observable (i.e., minimal) part of original system.
C
         IF( NR.LT.NCONT .OR. .NOT.LSPACE ) THEN
            IF( INDCON.GT.1 ) THEN
               KL = IWORK(IB) + IWORK(IB+1) - 1
            ELSE IF( INDCON.EQ.1 ) THEN
               KL = IWORK(IB) - 1
            ELSE
               KL = 0
            END IF
            CALL TB01XD( 'Zero D', NR, P, M, KL, MAX( 0, NR-1 ),
     $                   A, LDA, B, LDB, C, LDC, DWORK, 1, INFO )
            INFRED(2) = NCONT - NR
            INFRED(4) = INDCON
            IF ( LNJOBO ) THEN
               DO 10 I = 1, INDCON
                  IWORK(I) = IWORK(IB+I-1)
   10          CONTINUE
            END IF
         ELSE
C
C           Restore system matrices.
C
            CALL DLACPY( 'Full', NCONT, NCONT, DWORK(KWA), N, A, LDA )
            CALL DLACPY( 'Full', NCONT, M,     DWORK(KWB), N, B, LDB )
            CALL DLACPY( 'Full', P,     NCONT, DWORK(KWC), MAX( 1, P ),
     $                   C, LDC )
         END IF
      ELSE
         NR = NCONT
      END IF
C
C     Set structure information and optimal workspace dimension.
C
      INFRED(3) = KL
      DWORK(1)  = MAX( WRKOPT, LDWMIN + N*( N + M + P ) )
      RETURN
C *** Last line of TB01PX ***
      END
