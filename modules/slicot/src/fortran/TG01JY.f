      SUBROUTINE TG01JY( JOB, SYSTYP, EQUIL, CKSING, RESTOR, N, M, P, A,
     $                   LDA, E, LDE, B, LDB, C, LDC, NR, INFRED, TOL,
     $                   IWORK, DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To find a reduced (controllable, observable, or irreducible)
C     descriptor representation (Ar-lambda*Er,Br,Cr) for an original
C     descriptor representation (A-lambda*E,B,C).
C     The pencil Ar-lambda*Er is in an upper block Hessenberg form, with
C     either Ar or Er upper triangular.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOB     CHARACTER*1
C             Indicates whether the user wishes to remove the
C             uncontrollable and/or unobservable parts as follows:
C             = 'I':  Remove both the uncontrollable and unobservable
C                     parts to get an irreducible descriptor
C                     representation;
C             = 'C':  Remove the uncontrollable part only to get a
C                     controllable descriptor representation;
C             = 'O':  Remove the unobservable part only to get an
C                     observable descriptor representation.
C
C     SYSTYP  CHARACTER*1
C             Indicates the type of descriptor system algorithm
C             to be applied according to the assumed
C             transfer-function matrix as follows:
C             = 'R':  Rational transfer-function matrix;
C             = 'S':  Proper (standard) transfer-function matrix;
C             = 'P':  Polynomial transfer-function matrix.
C
C     EQUIL   CHARACTER*1
C             Specifies whether the user wishes to preliminarily scale
C             the system (A-lambda*E,B,C) as follows:
C             = 'S':  Perform scaling;
C             = 'N':  Do not perform scaling.
C
C     CKSING  CHARACTER*1
C             Specifies whether the user wishes to check if the pencil
C             (A-lambda*E) is singular as follows:
C             = 'C':  Check singularity;
C             = 'N':  Do not check singularity.
C             If the pencil is singular, the reduced system computed for
C             CKSING = 'N' can be wrong.
C
C     RESTOR  CHARACTER*1
C             Specifies whether the user wishes to save the system
C             matrices before each phase and restore them if no order
C             reduction took place as follows:
C             = 'R':  Save and restore;
C             = 'N':  Do not save the matrices.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The dimension of the descriptor state vector; also the
C             order of square matrices A and E, the number of rows of
C             matrix B, and the number of columns of matrix C.  N >= 0.
C
C     M       (input) INTEGER
C             The dimension of descriptor system input vector; also the
C             number of columns of matrix B.  M >= 0.
C
C     P       (input) INTEGER
C             The dimension of descriptor system output vector; also the
C             number of rows of matrix C.  P >= 0.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the original state matrix A.
C             On exit, the leading NR-by-NR part of this array contains
C             the reduced order state matrix Ar of an irreducible,
C             controllable, or observable realization for the original
C             system, depending on the value of JOB, JOB = 'I',
C             JOB = 'C', or JOB = 'O', respectively.
C             The matrix Ar is upper triangular if SYSTYP = 'P'.
C             If SYSTYP = 'S' and JOB = 'C', the matrix [Br Ar]
C             is in a controllable staircase form (see SLICOT Library
C             routine TG01HD).
C             If SYSTYP = 'S' and JOB = 'I' or 'O', the matrix ( Ar )
C                                                              ( Cr )
C             is in an observable staircase form (see TG01HD).
C             The resulting Ar has INFRED(5) nonzero sub-diagonals.
C             The block structure of staircase forms is contained
C             in the leading INFRED(7) elements of IWORK.
C
C     LDA     INTEGER
C             The leading dimension of array A.  LDA >= MAX(1,N).
C
C     E       (input/output) DOUBLE PRECISION array, dimension (LDE,N)
C             On entry, the leading N-by-N part of this array must
C             contain the original descriptor matrix E.
C             On exit, the leading NR-by-NR part of this array contains
C             the reduced order descriptor matrix Er of an irreducible,
C             controllable, or observable realization for the original
C             system, depending on the value of JOB, JOB = 'I',
C             JOB = 'C', or JOB = 'O', respectively.
C             The resulting Er has INFRED(6) nonzero sub-diagonals.
C             If at least for one k = 1,...,4, INFRED(k) >= 0, then the
C             resulting Er is structured being either upper triangular
C             or block Hessenberg, in accordance to the last
C             performed order reduction phase (see METHOD).
C             The block structure of staircase forms is contained
C             in the leading INFRED(7) elements of IWORK.
C
C     LDE     INTEGER
C             The leading dimension of array E.  LDE >= MAX(1,N).
C
C     B       (input/output) DOUBLE PRECISION array, dimension (LDB,M),
C             if JOB = 'C', or (LDB,MAX(M,P)), otherwise.
C             On entry, the leading N-by-M part of this array must
C             contain the original input matrix B; if JOB = 'I',
C             or JOB = 'O', the remainder of the leading N-by-MAX(M,P)
C             part is used as internal workspace.
C             On exit, the leading NR-by-M part of this array contains
C             the reduced input matrix Br of an irreducible,
C             controllable, or observable realization for the original
C             system, depending on the value of JOB, JOB = 'I',
C             JOB = 'C', or JOB = 'O', respectively.
C             If JOB = 'C', only the first IWORK(1) rows of B are
C             nonzero.
C
C     LDB     INTEGER
C             The leading dimension of array B.  LDB >= MAX(1,N).
C
C     C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
C             On entry, the leading P-by-N part of this array must
C             contain the original output matrix C; if JOB = 'I',
C             or JOB = 'O', the remainder of the leading MAX(M,P)-by-N
C             part is used as internal workspace.
C             On exit, the leading P-by-NR part of this array contains
C             the transformed state/output matrix Cr of an irreducible,
C             controllable, or observable realization for the original
C             system, depending on the value of JOB, JOB = 'I',
C             JOB = 'C', or JOB = 'O', respectively.
C             If JOB = 'I', or JOB = 'O', only the last IWORK(1) columns
C             (in the first NR columns) of C are nonzero.
C
C     LDC     INTEGER
C             The leading dimension of array C.
C             LDC >= MAX(1,M,P) if N > 0.
C             LDC >= 1          if N = 0.
C
C     NR      (output) INTEGER
C             The order of the reduced descriptor representation
C             (Ar-lambda*Er,Br,Cr) of an irreducible, controllable,
C             or observable realization for the original system,
C             depending on JOB = 'I', JOB = 'C', or JOB = 'O',
C             respectively.
C
C     INFRED  (output) INTEGER array, dimension 7
C             This array contains information on performed reduction
C             and on structure of resulting system matrices as follows:
C             INFRED(k) >= 0 (k = 1, 2, 3, or 4) if Phase k of reduction
C                            (see METHOD) has been performed. In this
C                            case, INFRED(k) is the achieved order
C                            reduction in Phase k.
C             INFRED(k) < 0  (k = 1, 2, 3, or 4) if Phase k was not
C                            performed.
C             INFRED(5)  -   the number of nonzero sub-diagonals of A.
C             INFRED(6)  -   the number of nonzero sub-diagonals of E.
C             INFRED(7)  -   the number of blocks in the resulting
C                            staircase form at last performed reduction
C                            phase. The block dimensions are contained
C                            in the first INFRED(7) elements of IWORK.
C
C     Tolerances
C
C     TOL     DOUBLE PRECISION array, dimension 3
C             TOL(1) is the tolerance to be used in rank determinations
C             when transforming (A-lambda*E,B,C). If the user sets
C             TOL(1) > 0, then the given value of TOL(1) is used as a
C             lower bound for reciprocal condition numbers in rank
C             determinations; a (sub)matrix whose estimated condition
C             number is less than 1/TOL(1) is considered to be of full
C             rank.  If the user sets TOL(1) <= 0, then an implicitly
C             computed, default tolerance, defined by TOLDEF1 = N*N*EPS,
C             is used instead, where EPS is the machine precision (see
C             LAPACK Library routine DLAMCH).  TOL(1) < 1.
C             TOL(2) is the tolerance to be used for checking pencil
C             singularity when CKSING = 'C', or singularity of the
C             matrices A and E when CKSING = 'N'. If the user sets
C             TOL(2) > 0, then the given value of TOL(2) is used.
C             If the user sets TOL(2) <= 0, then an implicitly
C             computed, default tolerance, defined by  TOLDEF2 = 10*EPS,
C             is used instead.  TOL(2) < 1.
C             TOL(3) is the threshold value for magnitude of the matrix
C             elements, if EQUIL = 'S': elements with magnitude less
C             than or equal to TOL(3) are ignored for scaling. If the
C             user sets TOL(3) >= 0, then the given value of TOL(3) is
C             used. If the user sets TOL(3) < 0, then an implicitly
C             computed, default threshold, defined by  THRESH = c*EPS,
C             where c = MAX(norm_1(A,E,B,C)) is used instead.
C             TOL(3) = 0 is not always a good choice.  TOL(3) < 1.
C             TOL(3) is not used if EQUIL = 'N'.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (2*N+MAX(M,P))
C             On exit, if INFO = 0, the leading INFRED(7) elements of
C             IWORK contain the orders of the diagonal blocks of
C             Ar-lambda*Er.
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if  INFO = 0,  DWORK(1) returns the optimal value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The length of the array DWORK.
C             LDWORK >= MAX(1,x,y,8*N), if EQUIL = 'S',
C             LDWORK >= MAX(1,x,y),     if EQUIL = 'N',
C             where x = MAX(2*(z+MAX(M,P)+N-1),N*N+4*N), if RESTOR = 'R'
C                   x = MAX(  2*(MAX(M,P)+N-1),N*N+4*N), if RESTOR = 'N'
C                   y = 2*N*N+10*N+MAX(N,23), if CKSING = 'C',
C                   y = 0,                    if CKSING = 'N',
C                   z = 2*N*N+N*M+N*P, if JOB  = 'I',
C                   z = 0,             if JOB <> 'I'.
C             For good performance, LDWORK should be generally larger.
C             If RESTOR = 'R', or
C             LDWORK >= MAX(1,2*N*N+N*M+N*P+2*(MAX(M,P)+N-1),
C             more accurate results are to be expected by considering
C             only those reductions phases (see METHOD), where effective
C             order reduction occurs. This is achieved by saving the
C             system matrices before each phase and restoring them if no
C             order reduction took place. Actually, if JOB = 'I' and
C             RESTOR = 'N', then the saved matrices are those obtained
C             after orthogonally triangularizing the matrix A (if
C             SYSTYP = 'R' or 'P'), or the matrix E (if SYSTYP = 'R'
C             or 'S').
C
C             If LDWORK = -1, then a workspace query is assumed;
C             the routine only calculates the optimal size of the
C             DWORK array, returns this value as the first entry of
C             the DWORK array, and no error message related to LDWORK
C             is issued by XERBLA. The optimal workspace includes the
C             extra space for improving the accuracy.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value;
C             = 1:  the given pencil A - lambda*E is numerically
C                   singular and the reduced system is not computed.
C                   This error can be returned only if CKSING = 'C'.
C
C     METHOD
C
C     The subroutine is based on the reduction algorithms of [1], but
C     with a different ordering of the phases.
C     The order reduction is performed in 4 phases:
C     Phase 1: Eliminate all infinite and finite nonzero uncontrollable
C              eigenvalues. The resulting matrix ( Br Er ) is in a
C              controllable staircase form (see TG01HD), and Ar is
C              upper triangular.
C              This phase is performed if JOB = 'I' or 'C' and
C              SYSTYP = 'R' or 'P'.
C     Phase 2: Eliminate all infinite and finite nonzero unobservable
C              eigenvalues. The resulting matrix ( Er ) is in an
C                                                ( Cr )
C              observable staircase form (see SLICOT Library routine
C              TG01ID), and Ar is upper triangular.
C              This phase is performed if JOB = 'I' or 'O' and
C              SYSTYP = 'R' or 'P'.
C     Phase 3: Eliminate all finite uncontrollable eigenvalues.
C              The resulting matrix ( Br Ar ) is in a controllable
C              staircase form (see TG01HD), and Er is upper triangular.
C              This phase is performed if JOB = 'I' or 'C' and
C              SYSTYP = 'R' or 'S'.
C     Phase 4: Eliminate all finite unobservable eigenvalues.
C              The resulting matrix ( Ar ) is in an observable
C                                   ( Cr )
C              staircase form (see TG01ID), and Er is upper triangular.
C              This phase is performed if JOB = 'I' or 'O' and
C              SYSTYP = 'R' or 'S'.
C     The routine checks the singularity of the matrices A and/or E
C     (depending on JOB and SYSTYP) and skips the unnecessary phases.
C     See FURTHER COMMENTS.
C
C     REFERENCES
C
C     [1] A. Varga
C         Computation of Irreducible Generalized State-Space
C         Realizations.
C         Kybernetika, vol. 26, pp. 89-106, 1990.
C
C     NUMERICAL ASPECTS
C
C     The algorithm is numerically backward stable and requires
C     0( N**3 )  floating point operations.
C
C     FURTHER COMMENTS
C
C     If the pencil A-lambda*E has no zero eigenvalues, then an
C     irreducible realization is computed skipping Phases 3 and 4
C     (equivalent to setting: JOB = 'I' and SYSTYP = 'P').
C
C     CONTRIBUTOR
C
C     V. Sima, Research Institute for Informatics, Bucharest, Feb. 2012.
C     Based on the SLICOT Library routine TG01JD by A. Varga.
C
C     REVISIONS
C
C     V. Sima, March 2012, April 2012, June 2012.
C
C     KEYWORDS
C
C     Controllability, irreducible realization, observability,
C     orthogonal canonical form, orthogonal transformation.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ONE, ZERO, TEN, TOLRC
      PARAMETER         ( ONE = 1.0D0, ZERO = 0.0D0, TEN = 10.0D0,
     $                    TOLRC = 1.0D-10 )
C     .. Scalar Arguments ..
      CHARACTER         CKSING, EQUIL, JOB, RESTOR, SYSTYP
      INTEGER           INFO, LDA, LDB, LDC, LDE, LDWORK, M, N, NR, P
C     .. Array Arguments ..
      INTEGER           INFRED(*), IWORK(*)
      DOUBLE PRECISION  A(LDA,*), B(LDB,*), C(LDC,*), DWORK(*),
     $                  E(LDE,*), TOL(*)
C     .. Local Scalars ..
      CHARACTER         JOBQ, JOBZ
      LOGICAL           FINCON, FINOBS, INFCON, INFOBS, LEQUIL, LJOBC,
     $                  LJOBIR, LJOBO, LQUERY, LSING, LSPACE, LSYSP,
     $                  LSYSR, LSYSS, MAXACC, SINGA, SINGE
      INTEGER           I, J, K, KWA, KWB, KWC, KWE, KWR, LBA, LBAS,
     $                  LBE, LBES, LDQ, LDZ, LWA, LWB, LWC, LWE, M1,
     $                  MAXMP, MAXWRK, MINWRK, N1, NB, NBLCK, NC, NN,
     $                  NX, P1
      DOUBLE PRECISION  ANORM, ENORM, RCOND, T, TL, TT, TZER
C     .. Local Arrays ..
      LOGICAL           BWORK(1)
      DOUBLE PRECISION  DUM(1)
C     .. External Functions ..
      LOGICAL           DELCTG, LSAME
      INTEGER           ILAENV
      DOUBLE PRECISION  DLAMCH, DLANGE, DLAPY2
      EXTERNAL          DELCTG, DLAMCH, DLANGE, DLAPY2, ILAENV, LSAME
C     .. External Subroutines ..
      EXTERNAL          DGECON, DGEQRF, DGETRF, DGGES, DLACPY, DLARF,
     $                  DLARFG, DLASET, DORMQR, MA02CD, TB01XD, TG01AD,
     $                  TG01HY, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, INT, MAX, MIN
C     .. Executable Statements ..
C
      INFO  = 0
      MAXMP = MAX( M, P )
      N1    = MAX( 1, N )
C
C     Decode JOB.
C
      LJOBIR = LSAME( JOB, 'I' )
      LJOBC  = LJOBIR .OR. LSAME( JOB, 'C' )
      LJOBO  = LJOBIR .OR. LSAME( JOB, 'O' )
C
C     Decode SYSTYP.
C
      LSYSR  = LSAME( SYSTYP, 'R' )
      LSYSS  = LSYSR .OR. LSAME( SYSTYP, 'S' )
      LSYSP  = LSYSR .OR. LSAME( SYSTYP, 'P' )
C
      LEQUIL = LSAME( EQUIL,  'S' )
C
      LSING  = LSAME( CKSING, 'C' )
C
      MAXACC = LSAME( RESTOR, 'R' )
C
C     Test the input scalar arguments.
C
      IF( .NOT.LJOBC .AND. .NOT.LJOBO ) THEN
         INFO = -1
      ELSE IF( .NOT.LSYSS .AND. .NOT.LSYSP ) THEN
         INFO = -2
      ELSE IF( .NOT.LEQUIL .AND. .NOT.LSAME( EQUIL,  'N' ) ) THEN
         INFO = -3
      ELSE IF( .NOT.LSING  .AND. .NOT.LSAME( CKSING, 'N' ) ) THEN
         INFO = -4
      ELSE IF( .NOT.MAXACC .AND. .NOT.LSAME( RESTOR, 'N' ) ) THEN
         INFO = -5
      ELSE IF( N.LT.0 ) THEN
         INFO = -6
      ELSE IF( M.LT.0 ) THEN
         INFO = -7
      ELSE IF( P.LT.0 ) THEN
         INFO = -8
      ELSE IF( LDA.LT.N1 ) THEN
         INFO = -10
      ELSE IF( LDE.LT.N1 ) THEN
         INFO = -12
      ELSE IF( LDB.LT.N1 ) THEN
         INFO = -14
      ELSE IF( LDC.LT.1 .OR. ( N.GT.0 .AND. LDC.LT.MAXMP ) ) THEN
         INFO = -16
      ELSE IF( TOL(1).GE.ONE ) THEN
         INFO = -19
      ELSE IF( TOL(2).GE.ONE ) THEN
         INFO = -19
      ELSE IF( TOL(3).GE.ONE ) THEN
         INFO = -19
      ELSE
         NN = N*N
         K  = N*( 2*N + M + P )
         IF( MAXACC ) THEN
            MINWRK = MAX( 1, 2*( K + MAXMP + N - 1 ), NN + 4*N )
         ELSE
            MINWRK = MAX( 1, 2*( MAXMP + N - 1 ), NN + 4*N )
         END IF
         IF( LEQUIL )
     $      MINWRK = MAX( MINWRK, 8*N )
         IF( LSING )
     $      MINWRK = MAX( MINWRK, 2*NN + 10*N + MAX( N, 23 ) )
C
C        Set controllability/observability determination options.
C
         FINCON = LJOBC .AND. LSYSS
         INFCON = LJOBC .AND. LSYSP
         FINOBS = LJOBO .AND. LSYSS
         INFOBS = LJOBO .AND. LSYSP
C
C        Set large workspace option and determine offsets.
C
         MAXWRK = K + 2*( MAXMP + N - 1 )
         LSPACE = LDWORK.GE.MAXWRK
         IF( LJOBIR )
     $      MAXWRK = MAXWRK + K
         LQUERY = LDWORK.EQ.-1
         MAXWRK = MAX( MAXWRK, MINWRK )
         JOBQ = 'N'
         JOBZ = 'N'
         TL   = TOL(1)
         IF( LQUERY ) THEN
C
C           Compute optimal workspace.
C
            IF( LSING ) THEN
               CALL DGGES( 'No Q', 'No Z', 'No sort', DELCTG, N, DWORK,
     $                     N1, DWORK, N1, KWA, DWORK, DWORK, DWORK,
     $                     DWORK, 1, DWORK, 1, DWORK, -1, BWORK, INFO )
               MAXWRK = MAX( MAXWRK, INT( DWORK(1) ) + 2*NN + 3*N )
            END IF
            IF( N.GT.1 ) THEN
               IF( FINCON .OR. INFCON ) THEN
                  CALL TG01HY( JOBQ, JOBZ, N, N, M, P, N, N-1, A, LDA,
     $                         E, LDE, B, LDB, C, LDC, DUM, 1, DUM, 1,
     $                         NR, NBLCK, IWORK, TL, IWORK, DWORK, -1,
     $                         INFO )
                  MAXWRK = MAX( MAXWRK, INT( DWORK(1) ) + K )
               END IF
               IF( FINOBS .OR. INFOBS ) THEN
                  CALL TG01HY( JOBQ, JOBZ, N, N, P, M, N, N-1, A, LDA,
     $                         E, LDE, B, LDB, C, LDC, DUM, 1, DUM, 1,
     $                         NR, NBLCK, IWORK, TL, IWORK, DWORK, -1,
     $                         INFO )
                  MAXWRK = MAX( MAXWRK, INT( DWORK(1) ) + K )
               END IF
            END IF
         ELSE IF( LDWORK.LT.MINWRK ) THEN
            INFO = -22
         END IF
      END IF
C
      IF ( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'TG01JY', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         DWORK(1) = MAXWRK
         RETURN
      END IF
C
C     Quick return if possible.
C
      INFRED(1) = -1
      INFRED(2) = -1
      INFRED(3) = -1
      INFRED(4) = -1
      INFRED(5) =  0
      INFRED(6) =  0
      INFRED(7) =  0
C
      IF( MAX( N, MAXMP ).EQ.0 ) THEN
         NR = 0
         DWORK(1) = ONE
         RETURN
      END IF
C
      LBA  = MAX( 0, N-1 )
      LBE  = LBA
      TZER = TOL(2)
      IF ( TL.LE.ZERO .OR. TZER.LE.ZERO .OR. LEQUIL ) THEN
         T = DLAMCH( 'Precision' )
         IF( TL.LE.ZERO )
     $      TL = NN*T
         IF( TZER.LE.ZERO )
     $      TZER = TEN*T
         IF( LEQUIL ) THEN
            THRESH = TOL(3)
            IF( THRESH.LT.ZERO ) THEN
               ANORM = DLANGE( '1-norm', N, N, A, LDA, DWORK )
               ENORM = DLANGE( '1-norm', N, N, E, LDE, DWORK )
               THRESH = MAX( ANORM, ENORM, 
     $                       DLANGE( '1-norm', N, M, B, LDB, DWORK ),
     $                       DLANGE( '1-norm', P, N, C, LDC, DWORK ) )*T
            END IF
         END IF
      END IF
C
C     Check if A and/or E are singular.
C     Workspace:  need   N*N + 4*N.
C
      SINGA = .FALSE.
      SINGE = .FALSE.
C
      J = NN + 1
C
      IF( LSING .OR. FINCON .OR. FINOBS ) THEN
         CALL DLACPY( 'Full', N, N, A, LDA, DWORK, N1 )
         CALL DGETRF( N, N, DWORK, N1, IWORK, I )
         IF( I.GT.0 ) THEN
            SINGA = .TRUE.
         ELSE
            IF( .NOT.LEQUIL .OR. TOL(3).GE.ZERO )
     $         ANORM = DLANGE( '1-norm', N, N, A, LDA, DWORK )
            CALL DGECON( '1-norm', N, DWORK, N1, ANORM, RCOND,
     $                   DWORK(J), IWORK, I )
            IF( RCOND.GT.TOLRC ) THEN
               FINCON = .FALSE.
               FINOBS = .FALSE.
            END IF
            IF( RCOND.LE.TZER )
     $         SINGA = .TRUE.
         END IF
C
      END IF
C
      IF( LSING .OR. INFCON .OR. INFOBS ) THEN
         CALL DLACPY( 'Full', N, N, E, LDE, DWORK, N1 )
         CALL DGETRF( N, N, DWORK, N1, IWORK, I )
         IF( I.GT.0 ) THEN
            SINGE = .TRUE.
         ELSE
            IF( .NOT.LEQUIL .OR. TOL(3).GE.ZERO )
     $         ENORM = DLANGE( '1-norm', N, N, E, LDE, DWORK )
            CALL DGECON( '1-norm', N, DWORK, N1, ENORM, RCOND,
     $                   DWORK(J), IWORK, I )
            IF( RCOND.LE.TZER )
     $         SINGE = .TRUE.
         END IF
C
         IF( .NOT.SINGE ) THEN
            INFCON = .FALSE.
            INFOBS = .FALSE.
         END IF
      END IF
C
      IF( LSING .AND. SINGA .AND. SINGE ) THEN
C
C        Check pencil regularity.
C        Workspace:  need   2*N*N + 10*N + MAX(N,23);
C                    prefer larger.
C        A variation of this routine can apply the QZ algorithm directly
C        to A and E, and update B and C. Then, the reduction can be done
C        on the transformed system.
C
         K = J + NN
         CALL DLACPY( 'Full', N, N, A, LDA, DWORK, N1 )
         CALL DLACPY( 'Full', N, N, E, LDE, DWORK(J), N1 )
         CALL DGGES( 'No Q', 'No Z', 'No sort', DELCTG, N, DWORK, N1,
     $               DWORK(J), N1, KWA, DWORK(K), DWORK(K+N),
     $               DWORK(K+2*N), DWORK, 1, DWORK, 1, DWORK(K+3*N),
     $               LDWORK-(K+3*N)+1, BWORK, I )
         MAXWRK = MAX( MAXWRK, INT( DWORK(K+3*N) ) + K + 3*N - 1 )
C
         DO 10 I = K, K+N-1
            IF( ABS( DWORK(I+2*N) ).LE.TZER ) THEN
               IF( DLAPY2( DWORK(I), DWORK(I+N) ).LE.TZER ) THEN
                  INFO = 1
                  RETURN
               END IF
            END IF
   10    CONTINUE
      END IF
C
      M1 = MAX( 1, M )
      P1 = MAX( 1, P )
C
      IF( LSPACE ) THEN
C
C        Determine offsets for large workspace option.
C
         KWA = 1
         KWE = J
         KWB = KWE + NN
         KWC = KWB + N*M
         KWR = KWC + N*P
      ELSE
         KWR = 1
      END IF
C
      IF( MAXACC ) THEN
C
C        Determine offsets for large workspace option.
C
         LWA = KWR
         LWE = LWA + NN
         LWB = LWE + NN
         LWC = LWB + N*M
         KWR = LWC + N*P
      END IF
C
C     If required, scale the system (A-lambda*E,B,C).
C     Workspace: need 8*N.
C
      IF( LEQUIL ) THEN
         CALL TG01AD( 'All', N, N, M, P, THRESH, A, LDA, E, LDE, B, LDB,
     $                C, LDC, DWORK, DWORK(N+1), DWORK(2*N+1), INFO )
      END IF
C
      LDQ = 1
      LDZ = 1
      NC  = N
      NR  = N
C
      IF( INFCON ) THEN
C
C        Phase 1: Eliminate all infinite and all finite nonzero
C                 uncontrollable eigenvalues.
C
         IF( MAXACC ) THEN
C
C           Save system matrices.
C
            CALL DLACPY( 'Full', NC, NC, A, LDA, DWORK(LWA), N1 )
            CALL DLACPY( 'Full', NC, NC, E, LDE, DWORK(LWE), N1 )
            CALL DLACPY( 'Full', NC, M,  B, LDB, DWORK(LWB), N1 )
            CALL DLACPY( 'Full', P,  NC, C, LDC, DWORK(LWC), P1 )
         END IF
C
         IF( LSPACE ) THEN
C
C           Reduce A to upper triangular form if necessary.
C           Check if block algorithms should be used.
C
            IF( LBA.GT.0 .AND. INFOBS ) THEN
               CALL DGEQRF( NC, NC, A, LDA, DWORK, DWORK, -1, INFO )
               NB = INT( DWORK(1)/NC )
               IF( LDWORK.LT.NC*NB )
     $            NB = INT( LDWORK/NC )
C
               NX = ILAENV( 3, 'DGEQRF', ' ', NC, NC, -1, -1 )
               IF( LBA.LT.NX/2 .OR. NB.LT.NX .OR. NC.LT.NX ) THEN
C
                  DO 20 I = 1, NC-1
C
C                    Generate elementary reflector H(i) to annihilate
C                    A(i+1:i+lba,i).
C
                     K = MIN( LBA, NC-I ) + 1
                     CALL DLARFG( K, A(I,I), A(I+1,I), 1, TT )
                     T = A(I,I)
                     A(I,I) = ONE
C
C                    Apply H(i) to A(i:nc,i+1:n) from the left.
C
                     CALL DLARF( 'Left', K, N-I, A(I,I), 1, TT,
     $                           A(I,I+1), LDA, DWORK )
C
C                    Apply H(i) to E(i:nc,1:n) from the left.
C
                     CALL DLARF( 'Left', K, N, A(I,I), 1, TT, E(I,1),
     $                           LDE, DWORK )
C
C                    Apply H(i) to B(i:nc,1:m) from the left.
C
                     CALL DLARF( 'Left', K, M, A(I,I), 1, TT, B(I,1),
     $                           LDB, DWORK )
                     A(I,I) = T
   20             CONTINUE
C
               ELSE
C
                  CALL DGEQRF( NC, NC, A, LDA, DWORK, DWORK(NC+1),
     $                         LDWORK-NC, INFO )
                  MAXWRK = MAX( MAXWRK, INT( DWORK(NC+1) )+NC )
                  CALL DORMQR( 'Left', 'Transpose', NC, N, NC, A, LDA,
     $                         DWORK, E, LDE, DWORK(NC+1), LDWORK-NC,
     $                         INFO )
                  MAXWRK = MAX( MAXWRK, INT( DWORK(NC+1) )+NC )
                  CALL DORMQR( 'Left', 'Transpose', NC, M, NC, A, LDA,
     $                         DWORK, B, LDB, DWORK(NC+1), LDWORK-NC,
     $                         INFO )
                  MAXWRK = MAX( MAXWRK, INT( DWORK(NC+1) )+NC )
               END IF
               IF( NC.GT.1 )
     $            CALL DLASET( 'Lower', NC-1, NC-1, ZERO, ZERO, A(2,1),
     $                         LDA )
               LBA = 0
C
            END IF
C
C           Save system matrices.
C
            CALL DLACPY( 'Full', NC, NC, A, LDA, DWORK(KWA), N1 )
            CALL DLACPY( 'Full', NC, NC, E, LDE, DWORK(KWE), N1 )
            CALL DLACPY( 'Full', NC, M,  B, LDB, DWORK(KWB), N1 )
            CALL DLACPY( 'Full', P,  NC, C, LDC, DWORK(KWC), P1 )
         END IF
C
C        Perform infinite controllability form reduction.
C        Workspace: need   2*(M+N-1);
C                   prefer larger.
C
         CALL TG01HY( JOBQ, JOBZ, NC, NC, M, P, NC, LBA, E, LDE, A, LDA,
     $                B, LDB, C, LDC, DUM, LDQ, DUM, LDZ, NR, NBLCK,
     $                IWORK, TL, IWORK(N+1), DWORK(KWR), LDWORK-KWR+1,
     $                INFO )
         MAXWRK = MAX( MAXWRK, INT( DWORK(KWR) )+KWR-1 )
         INFRED(1) = NC - NR
         INFRED(7) = NBLCK
         IF( NR.LT.NC .OR. .NOT.LSPACE ) THEN
            IF( NBLCK.GT.1 ) THEN
               LBE = IWORK(1) + IWORK(2) - 1
            ELSE IF( NBLCK.EQ.1 ) THEN
               LBE = IWORK(1) - 1
            ELSE
               LBE = 0
            END IF
            LBA = 0
            NC  = NR
         ELSE IF ( .NOT.MAXACC ) THEN
C
C           Restore orthogonally transformed system matrices.
C
            CALL DLACPY( 'Full', NC, NC, DWORK(KWA), N1, A, LDA )
            CALL DLACPY( 'Full', NC, NC, DWORK(KWE), N1, E, LDE )
            CALL DLACPY( 'Full', NC, M,  DWORK(KWB), N1, B, LDB )
            CALL DLACPY( 'Full', P,  NC, DWORK(KWC), P1, C, LDC )
         ELSE
C
C           Restore system matrices.
C
            CALL DLACPY( 'Full', NC, NC, DWORK(LWA), N1, A, LDA )
            CALL DLACPY( 'Full', NC, NC, DWORK(LWE), N1, E, LDE )
            CALL DLACPY( 'Full', NC, M,  DWORK(LWB), N1, B, LDB )
            CALL DLACPY( 'Full', P,  NC, DWORK(LWC), P1, C, LDC )
            LBA = MAX( 0, N-1 )
         END IF
      END IF
C
      IF( INFOBS ) THEN
C
C        Phase 2: Eliminate all infinite and all finite nonzero
C                 unobservable eigenvalues.
C
C        Compute the pertransposed dual system exploiting matrix shapes.
C
         CALL TB01XD( 'Z', NC, M, P, LBA, MAX( 0, NC-1 ), A, LDA,
     $                B, LDB, C, LDC, DUM, 1, INFO )
         CALL MA02CD( NC, LBE, MAX( 0, NC-1 ), E, LDE )
C
         IF( LSPACE) THEN
C
C           Save system matrices.
C
            CALL DLACPY( 'Full', NC, NC, A, LDA, DWORK(KWA), N1 )
            CALL DLACPY( 'Full', NC, NC, E, LDE, DWORK(KWE), N1 )
            CALL DLACPY( 'Full', NC, P,  B, LDB, DWORK(KWC), N1 )
            CALL DLACPY( 'Full', M,  NC, C, LDC, DWORK(KWB), M1 )
         END IF
C
C        Perform infinite observability form reduction.
C        Workspace: need   2*(P+N-1);
C                   prefer larger.
C
         CALL TG01HY( JOBZ, JOBQ, NC, NC, P, M, NC, LBA, E, LDE, A, LDA,
     $                B, LDB, C, LDC, DUM, LDZ, DUM, LDQ, NR, NBLCK,
     $                IWORK(N+1), TL, IWORK(2*N+1), DWORK(KWR),
     $                LDWORK-KWR+1, INFO )
         MAXWRK = MAX( MAXWRK, INT( DWORK(KWR) )+KWR-1 )
         INFRED(2) = NC - NR
         IF( NR.LT.NC .OR. .NOT.LSPACE ) THEN
            INFRED(7) =  NBLCK
            DO 30 I = 1, NBLCK
               IWORK(I) = IWORK(N+I)
   30       CONTINUE
            IF( NBLCK.GT.1 ) THEN
               LBE = IWORK(1) + IWORK(2) - 1
            ELSE IF( NBLCK.EQ.1 ) THEN
               LBE = IWORK(1) - 1
            ELSE
               LBE = 0
            END IF
            LBA = 0
            NC  = NR
         ELSE
C
C           Restore orthogonally transformed system matrices.
C
            CALL DLACPY( 'Full', NC, NC, DWORK(KWA), N1, A, LDA )
            CALL DLACPY( 'Full', NC, NC, DWORK(KWE), N1, E, LDE )
            CALL DLACPY( 'Full', NC, P,  DWORK(KWC), N1, B, LDB )
            CALL DLACPY( 'Full', M,  NC, DWORK(KWB), M1, C, LDC )
         END IF
C
         IF( FINCON .OR. .NOT. FINOBS ) THEN
C
C           Compute the pertransposed dual system exploiting matrix
C           shapes.
C
            CALL TB01XD( 'Z', NC, P, M, LBA, MAX( 0, NC-1 ), A, LDA,
     $                   B, LDB, C, LDC, DUM, 1, INFO )
            CALL MA02CD( NC, LBE, MAX( 0, NC-1 ), E, LDE )
         END IF
      END IF
C
      IF( FINCON ) THEN
C
C        Phase 3: Eliminate all finite uncontrollable eigenvalues.
C
         IF( MAXACC ) THEN
C
C           Save system matrices.
C
            CALL DLACPY( 'Full', NC, NC, A, LDA, DWORK(LWA), N1 )
            CALL DLACPY( 'Full', NC, NC, E, LDE, DWORK(LWE), N1 )
            CALL DLACPY( 'Full', NC, M,  B, LDB, DWORK(LWB), N1 )
            CALL DLACPY( 'Full', P,  NC, C, LDC, DWORK(LWC), P1 )
            LBAS = LBA
            LBES = LBE
         END IF
C
         IF( LSPACE ) THEN
C
C           Reduce E to upper triangular form if necessary.
C           Check if block algorithms should be used.
C
            IF( LBE.GT.0 .AND. FINOBS ) THEN
               CALL DGEQRF( NC, NC, E, LDE, DWORK, DWORK, -1, INFO )
               NB = INT( DWORK(1)/NC )
               IF( LDWORK.LT.NC*NB )
     $            NB = INT( LDWORK/NC )
C
               NX = ILAENV( 3, 'DGEQRF', ' ', NC, NC, -1, -1 )
               IF( LBE.LT.NX/2 .OR. NB.LT.NX .OR. NC.LT.NX ) THEN
C
                  DO 40 I = 1, NC-1
C
C                    Generate elementary reflector H(i) to annihilate
C                    E(i+1:i+lbe,i).
C
                     K = MIN( LBE, NC-I ) + 1
                     CALL DLARFG( K, E(I,I), E(I+1,I), 1, TT )
                     T = E(I,I)
                     E(I,I) = ONE
C
C                    Apply H(i) to E(i:nc,i+1:n) from the left.
C
                     CALL DLARF( 'Left', K, N-I, E(I,I), 1, TT,
     $                           E(I,I+1), LDE, DWORK )
C
C                    Apply H(i) to A(i:nc,1:n) from the left.
C
                     CALL DLARF( 'Left', K, N, E(I,I), 1, TT, A(I,1),
     $                           LDA, DWORK )
C
C                    Apply H(i) to B(i:nc,1:m) from the left.
C
                     CALL DLARF( 'Left', K, M, E(I,I), 1, TT, B(I,1),
     $                           LDB, DWORK )
                     E(I,I) = T
   40             CONTINUE
C
               ELSE
C
                  CALL DGEQRF( NC, NC, E, LDE, DWORK, DWORK(NC+1),
     $                         LDWORK-NC, INFO )
                  MAXWRK = MAX( MAXWRK, INT( DWORK(NC+1) )+NC )
                  CALL DORMQR( 'Left', 'Transpose', NC, N, NC, E, LDE,
     $                         DWORK, A, LDA, DWORK(NC+1), LDWORK-NC,
     $                         INFO )
                  MAXWRK = MAX( MAXWRK, INT( DWORK(NC+1) )+NC )
                  CALL DORMQR( 'Left', 'Transpose', NC, M, NC, E, LDE,
     $                         DWORK, B, LDB, DWORK(NC+1), LDWORK-NC,
     $                         INFO )
                  MAXWRK = MAX( MAXWRK, INT( DWORK(NC+1) )+NC )
               END IF
               IF( NC.GT.1 )
     $            CALL DLASET( 'Lower', NC-1, NC-1, ZERO, ZERO, E(2,1),
     $                         LDE )
               LBE = 0
               LBA = MAX( 0, NC-1 )
C
            END IF
C
C           Save system matrices.
C
            CALL DLACPY( 'Full', NC, NC, A, LDA, DWORK(KWA), N1 )
            CALL DLACPY( 'Full', NC, NC, E, LDE, DWORK(KWE), N1 )
            CALL DLACPY( 'Full', NC, M,  B, LDB, DWORK(KWB), N1 )
            CALL DLACPY( 'Full', P,  NC, C, LDC, DWORK(KWC), P1 )
         END IF
C
C        Perform finite controllability form reduction.
C        Workspace: need   2*(M+N-1);
C                   prefer larger.
C
         CALL TG01HY( JOBQ, JOBZ, NC, NC, M, P, NC, LBE, A, LDA, E, LDE,
     $                B, LDB, C, LDC, DUM, LDQ, DUM, LDZ, NR, NBLCK,
     $                IWORK(N+1), TL, IWORK(2*N+1), DWORK(KWR),
     $                LDWORK-KWR+1, INFO )
         MAXWRK = MAX( MAXWRK, INT( DWORK(KWR) )+KWR-1 )
         INFRED(3) = NC - NR
         IF( NR.LT.NC .OR. .NOT.LSPACE ) THEN
            INFRED(7) =  NBLCK
            DO 50 I = 1, NBLCK
               IWORK(I) = IWORK(N+I)
   50       CONTINUE
            IF( NBLCK.GT.1 ) THEN
               LBA = IWORK(1) + IWORK(2) - 1
            ELSE IF( NBLCK.EQ.1 ) THEN
               LBA = IWORK(1) - 1
            ELSE
               LBA = 0
            END IF
            LBE = 0
            NC  = NR
         ELSE IF ( .NOT.MAXACC ) THEN
C
C           Restore orthogonally transformed system matrices.
C
            CALL DLACPY( 'Full', NC, NC, DWORK(KWA), N1, A, LDA )
            CALL DLACPY( 'Full', NC, NC, DWORK(KWE), N1, E, LDE )
            CALL DLACPY( 'Full', NC, M,  DWORK(KWB), N1, B, LDB )
            CALL DLACPY( 'Full', P,  NC, DWORK(KWC), P1, C, LDC )
         ELSE
C
C           Restore system matrices.
C
            CALL DLACPY( 'Full', NC, NC, DWORK(LWA), N1, A, LDA )
            CALL DLACPY( 'Full', NC, NC, DWORK(LWE), N1, E, LDE )
            CALL DLACPY( 'Full', NC, M,  DWORK(LWB), N1, B, LDB )
            CALL DLACPY( 'Full', P,  NC, DWORK(LWC), P1, C, LDC )
            LBA = LBAS
            LBE = LBES
         END IF
C
         IF( FINOBS ) THEN
C
C           Compute the pertransposed dual system exploiting matrix
C           shapes.
C
            CALL TB01XD( 'Z', NC, M, P, LBA, MAX( 0, NC-1 ), A, LDA,
     $                   B, LDB, C, LDC, DUM, 1, INFO )
            CALL MA02CD( NC, LBE, MAX( 0, NC-1 ), E, LDE )
         END IF
      END IF
C
      IF( FINOBS ) THEN
C
C        Phase 4: Eliminate all finite unobservable eigenvalues.
C
         IF( LSPACE ) THEN
C
C           Save system matrices.
C
            CALL DLACPY( 'Full', NC, NC, A, LDA, DWORK(KWA), N1 )
            CALL DLACPY( 'Full', NC, NC, E, LDE, DWORK(KWE), N1 )
            CALL DLACPY( 'Full', NC, P,  B, LDB, DWORK(KWC), N1 )
            CALL DLACPY( 'Full', M,  NC, C, LDC, DWORK(KWB), M1 )
         END IF
C
C        Perform finite observability form reduction.
C        Workspace: need   2*(P+N-1);
C                   prefer larger.
C
         CALL TG01HY( JOBZ, JOBQ, NC, NC, P, M, NC, LBE, A, LDA, E, LDE,
     $                B, LDB, C, LDC, DUM, LDZ, DUM, LDQ, NR, NBLCK,
     $                IWORK(N+1), TL, IWORK(2*N+1), DWORK(KWR),
     $                LDWORK-KWR+1, INFO )
         MAXWRK = MAX( MAXWRK, INT( DWORK(KWR) )+KWR-1 )
         INFRED(4) = NC - NR
         IF( NR.LT.NC .OR. .NOT.LSPACE ) THEN
            INFRED(7) =  NBLCK
            DO 60 I = 1, NBLCK
               IWORK(I) = IWORK(N+I)
   60       CONTINUE
            IF( NBLCK.GT.1 ) THEN
               LBA = IWORK(1) + IWORK(2) - 1
            ELSE IF( NBLCK.EQ.1 ) THEN
               LBA = IWORK(1) - 1
            ELSE
               LBA = 0
            END IF
            LBE = 0
            NC  = NR
         ELSE
C
C           Restore orthogonally transformed system matrices.
C
            CALL DLACPY( 'Full', NC, NC, DWORK(KWA), N1, A, LDA )
            CALL DLACPY( 'Full', NC, NC, DWORK(KWE), N1, E, LDE )
            CALL DLACPY( 'Full', NC, P,  DWORK(KWC), N1, B, LDB )
            CALL DLACPY( 'Full', M,  NC, DWORK(KWB), M1, C, LDC )
         END IF
C
C        Compute the pertransposed dual system exploiting matrix shapes.
C
         CALL TB01XD( 'Z', NC, P, M, LBA, MAX( 0, NC-1 ), A, LDA,
     $                B, LDB, C, LDC, DUM, 1, INFO )
         CALL MA02CD( NC, LBE, MAX( 0, NC-1 ), E, LDE )
      END IF
C
C     Set structural information on A and E.
C
      INFRED(5) = LBA
      INFRED(6) = LBE
      DWORK(1)  = MAXWRK
C
      RETURN
C *** Last line of TG01JY ***
      END
