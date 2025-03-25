      SUBROUTINE MB03VW( COMPQ, QIND, TRIU, N, K, H, ILO, IHI, S, A,
     $                   LDA1, LDA2, Q, LDQ1, LDQ2, IWORK, LIWORK,
     $                   DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To reduce the generalized matrix product
C
C                  S(1)           S(2)                 S(K)
C          A(:,:,1)     * A(:,:,2)     * ... * A(:,:,K)
C
C     to upper Hessenberg-triangular form, where A is N-by-N-by-K and S
C     is the signature array with values 1 or -1. The H-th matrix of A
C     is reduced to upper Hessenberg form while the other matrices are
C     triangularized. Unblocked version.
C
C     If COMPQ = 'U' or COMPZ = 'I', then the orthogonal factors are
C     computed and stored in the array Q so that for S(I) = 1,
C                                                              T
C           Q(:,:,I)(in)   A(:,:,I)(in)   Q(:,:,MOD(I,K)+1)(in)
C                                                               T    (1)
C        =  Q(:,:,I)(out)  A(:,:,I)(out)  Q(:,:,MOD(I,K)+1)(out) ,
C
C     and for S(I) = -1,
C                                                              T
C           Q(:,:,MOD(I,K)+1)(in)   A(:,:,I)(in)   Q(:,:,I)(in)
C                                                               T    (2)
C        =  Q(:,:,MOD(I,K)+1)(out)  A(:,:,I)(out)  Q(:,:,I)(out) .
C
C     A partial generation of the orthogonal factors can be realized via
C     the array QIND.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     COMPQ   CHARACTER*1
C             Specifies whether or not the orthogonal transformations
C             should be accumulated in the array Q, as follows:
C             = 'N': do not modify Q;
C             = 'U': modify (update) the array Q by the orthogonal
C                    transformations that are applied to the matrices in
C                    the array A to reduce them to periodic Hessenberg-
C                    triangular form;
C             = 'I': like COMPQ = 'U', except that each matrix in the
C                    array Q will be first initialized to the identity
C                    matrix;
C             = 'P': use the parameters as encoded in QIND.
C
C     QIND    INTEGER array, dimension (K)
C             If COMPQ = 'P', then this array describes the generation
C             of the orthogonal factors as follows:
C                If QIND(I) > 0, then the array Q(:,:,QIND(I)) is
C             modified by the transformations corresponding to the
C             i-th orthogonal factor in (1) and (2).
C                If QIND(I) < 0, then the array Q(:,:,-QIND(I)) is
C             initialized to the identity and modified by the
C             transformations corresponding to the i-th orthogonal
C             factor in (1) and (2).
C                If QIND(I) = 0, then the transformations corresponding
C             to the i-th orthogonal factor in (1), (2) are not applied.
C
C     TRIU    CHARACTER*1
C             Indicates how many matrices are reduced to upper
C             triangular form in the first stage of the algorithm,
C             as follows
C             = 'N':  only matrices with negative signature;
C             = 'A':  all possible N - 1 matrices.
C             The first choice minimizes the computational costs of the
C             algorithm, whereas the second is more cache efficient and
C             therefore faster on modern architectures.
C
C     Input/Output Parameters
C
C     N       (input)  INTEGER
C             The order of each factor in the array A.  N >= 0.
C
C     K       (input) INTEGER
C             The number of factors.  K >= 0.
C
C     H       (input/output) INTEGER
C             On entry, if H is in the interval [1,K] then the H-th
C             factor of A will be transformed to upper Hessenberg form.
C             Otherwise the most efficient H is chosen.
C             On exit, H indicates the factor of A which is in upper
C             Hessenberg form.
C
C     ILO     (input)  INTEGER
C     IHI     (input)  INTEGER
C             It is assumed that each factor in A is already upper
C             triangular in rows and columns 1:ILO-1 and IHI+1:N.
C             1 <= ILO <= IHI <= N, if N > 0;
C             ILO = 1 and IHI  = 0, if N = 0.
C             If ILO = IHI, all factors are upper triangular.
C
C     S       (input)  INTEGER array, dimension (K)
C             The leading K elements of this array must contain the
C             signatures of the factors. Each entry in S must be either
C             1 or -1.
C
C     A       (input/output) DOUBLE PRECISION array, dimension
C                            (LDA1,LDA2,K)
C             On entry, the leading N-by-N-by-K part of this array must
C             contain the factors of the general product to be reduced.
C             On exit, A(:,:,H) is overwritten by an upper Hessenberg
C             matrix and each A(:,:,I), for I not equal to H, is
C             overwritten by an upper triangular matrix.
C
C     LDA1    INTEGER
C             The first leading dimension of the array A.
C             LDA1 >= MAX(1,N).
C
C     LDA2    INTEGER
C             The second leading dimension of the array A.
C             LDA2 >= MAX(1,N).
C
C     Q       (input/output) DOUBLE PRECISION array, dimension
C                            (LDQ1,LDQ2,K)
C             On entry, if COMPQ = 'U', the leading N-by-N-by-K part
C             of this array must contain the initial orthogonal factors
C             as described in (1) and (2).
C             On entry, if COMPQ = 'P', only parts of the leading
C             N-by-N-by-K part of this array must contain some
C             orthogonal factors as described by the parameters QIND.
C             If COMPQ = 'I', this array should not be set on entry.
C             On exit, if COMPQ = 'U' or COMPQ = 'I', the leading
C             N-by-N-by-K part of this array contains the modified
C             orthogonal factors as described in (1) and (2).
C             On exit, if COMPQ = 'P', only parts of the leading
C             N-by-N-by-K part contain some modified orthogonal factors
C             as described by the parameters QIND.
C             This array is not referenced if COMPQ = 'N'.
C
C     LDQ1    INTEGER
C             The first leading dimension of the array Q.  LDQ1 >= 1,
C             and, if COMPQ <> 'N', LDQ1 >= MAX(1,N).
C
C     LDQ2    INTEGER
C             The second leading dimension of the array Q.  LDQ2 >= 1,
C             and, if COMPQ <> 'N', LDQ2 >= MAX(1,N).
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (LIWORK)
C             On exit, if  INFO = -17,  IWORK(1)  returns the needed
C             value of LIWORK.
C
C     LIWORK  INTEGER
C             The length of the array IWORK.  LIWORK >= MAX(1,3*K).
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal value
C             of LDWORK.
C             On exit, if  INFO = -19,  DWORK(1)  returns the minimum
C             value of LIWORK.
C
C     LDWORK  INTEGER
C             The length of the array DWORK. 
C             LDWORK >= 1, if MIN(N,K) = 0, or N = 1 or ILO = IHI;
C             LDWORK >= M+MAX(IHI,N-ILO+1)), otherwise, where
C                       M = IHI-ILO+1.
C             For optimum performance LDWORK should be larger.
C
C             If LDWORK = -1  a workspace query is assumed; the
C             routine only calculates the optimal size of the DWORK
C             array, returns this value as the first entry of the DWORK
C             array, and no error message is issued by XERBLA.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value.
C
C     CONTRIBUTOR
C
C     V. Sima, Feb. 2022.
C     Based on an unfinished version of the routine PGGHRD, developed by
C     D. Kressner, Technical Univ. Chemnitz, Germany, June 1998.
C
C     REVISIONS
C
C     V. Sima, Mar. 2022.
C
C     KEYWORDS
C
C     Eigenvalues, QZ algorithm, periodic QZ algorithm, orthogonal
C     transformation.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0 )
C     .. Scalar Arguments ..
      CHARACTER         COMPQ, TRIU
      INTEGER           H, IHI, ILO, INFO, K, LDA1, LDA2, LDQ1, LDQ2,
     $                  LDWORK, LIWORK, N
C     .. Array Arguments ..
      INTEGER           IWORK(*), QIND(*), S(*)
      DOUBLE PRECISION  A(LDA1,LDA2,*), DWORK(LDWORK), Q(LDQ1,LDQ2,*)
C     .. Local Scalars ..
      LOGICAL           ALLTRI, LCMPQ, LINDQ, LINIQ, LPARQ, LQUERY
      INTEGER           AIND, AINDP, I, I2, I3, IER, INDQ, IWRK, J, L,
     $                  LT, M, MAPA, MAPQ, MAXSET, MINWRK, OPTWRK, POS,
     $                  SMULT, UPIDX, WMAX
      DOUBLE PRECISION  ALPHA, TAU, TEMP
C     .. Local Arrays ..
      DOUBLE PRECISION  DUM(3)
C     .. External Functions ..
      LOGICAL           LSAME
      EXTERNAL          LSAME
C     .. External Subroutines ..
      EXTERNAL          DCOPY, DGEQRF, DGERQF, DLACPY, DLARF, DLARFG,
     $                  DLARTG, DLASET, DORGQR, DORMQR, DORMRQ, DROT,
     $                  MB03BA, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, INT, MAX, MIN
C
C     .. Executable Statements ..
C
      INFO   = 0
      LINIQ  = LSAME( COMPQ, 'I' )
      LCMPQ  = LSAME( COMPQ, 'U' ) .OR. LINIQ
      LPARQ  = LSAME( COMPQ, 'P' )
      ALLTRI = LSAME( TRIU,  'A' )
      LQUERY = LDWORK.EQ.-1
C
C     Test the input scalar arguments.
C
      IF ( .NOT.LCMPQ .AND. .NOT.LPARQ .AND. .NOT.LSAME( COMPQ, 'N' ) )
     $      THEN
         INFO = -1
      ELSE IF ( .NOT.ALLTRI .AND. .NOT.LSAME( TRIU, 'N') ) THEN
         INFO = -3
      ELSE IF ( N.LT.0 ) THEN
         INFO = -4
      ELSE IF ( K.LT.0 ) THEN
         INFO = -5
      ELSE IF ( ILO.LT.1 ) THEN
         INFO = -7
      ELSE IF ( IHI.GT.N .OR. ( N.GT.0 .AND. IHI.LT.ILO ) .OR.
     $          IHI.LT.0 ) THEN
         INFO = -8
      ELSE IF ( LDA1.LT.MAX( 1, N ) ) THEN
         INFO = -11
      ELSE IF ( LDA2.LT.MAX( 1, N ) ) THEN
         INFO = -12
      ELSE IF ( LDQ1.LT.1 .OR. ( ( LCMPQ .OR. LPARQ )
     $                             .AND. LDQ1.LT.N ) ) THEN
         INFO = -14
      ELSE IF ( LDQ2.LT.1 .OR. ( ( LCMPQ .OR. LPARQ )
     $                             .AND. LDQ2.LT.N ) ) THEN
         INFO = -15
      ELSE IF ( LIWORK.LT.MAX( 1, 3*K ) ) THEN
         INFO = -17
         IWORK(1) = MAX( 1, 3*K )
      ELSE
         IF ( MIN( N, K ).EQ.0 .OR. N.EQ.1 .OR. ILO.EQ.IHI ) THEN
            MINWRK = 1
         ELSE
            M = IHI - ILO + 1
            MINWRK = M + MAX( IHI, N - ILO + 1 )
         END IF
         OPTWRK = MINWRK
         IF ( .NOT.LQUERY .AND. LDWORK.LT.MINWRK ) THEN
            INFO = -19
            DWORK(1) = MINWRK
         ELSE IF ( N.GT.2 ) THEN
            CALL DGEQRF( M, M, A, LDA1, DUM, DUM(1), -1, IER )
            CALL DGERQF( M, M, A, LDA1, DUM, DUM(2), -1, IER )
            OPTWRK = MAX( INT( DUM(1) ), INT( DUM(2) ) )
            IF ( IHI.LT.N ) THEN
               CALL DORMQR( 'Left', 'Trans', M, N-IHI, M, A, LDA1, DUM,
     $                      A, LDA1, DUM, -1, IER )
               OPTWRK = MAX( OPTWRK, INT( DUM(1) ) )
            END IF
            CALL DORMQR( 'Right', 'NoTran', IHI, M, M, A, LDA1, DUM, A,
     $                   LDA1, DUM, -1, IER )
            CALL DORMQR( 'Left', 'Trans', M, N-ILO+1, M, A, LDA1, DUM,
     $                   A, LDA1, DUM(2), -1, IER )
            OPTWRK = MAX( OPTWRK, INT( DUM(1) ), INT( DUM(2) ) )
            CALL DORMRQ( 'Right', 'Trans', IHI, M, M, A, LDA1, DUM, A,
     $                   LDA1, DUM, -1, IER )
            CALL DORMRQ( 'Left', 'NoTran', M, N-ILO+1, M, A, LDA1, DUM,
     $                   A, LDA1, DUM(2), -1, IER )
            OPTWRK = MAX( OPTWRK, INT( DUM(1) ), INT( DUM(2) ) )
            IF ( LINIQ ) THEN
               CALL DORGQR( M, M, M, Q, LDQ1, DUM, DUM, -1, IER )
               OPTWRK = MAX( OPTWRK, INT( DUM(1) ) )
            END IF
            OPTWRK = MAX( MINWRK, M + OPTWRK )
         END IF
      END IF
C
      IF ( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'MB03VW', -INFO )
         RETURN
      ELSE IF ( LQUERY ) THEN
         DWORK(1) = OPTWRK
         RETURN
      END IF
C
C     (Note: Comments in the code beginning "Workspace:" describe the
C     minimal amount of real workspace needed at that point in the
C     code, as well as the preferred amount for good performance.
C     NB refers to the optimal block size for the immediately
C     following subroutine.)
C
C     Set H, if not in the proper interval.
C
      IF ( K.EQ.0 ) THEN
         H = 0
      ELSE IF ( H.LT.1 .OR. H.GT.K ) THEN
         H = 1
      END IF
C
C     Quick return if possible.
C
      IF ( MIN( N, K ).EQ.0 ) THEN
         DWORK(1) = ONE
         RETURN
      END IF
C
C     Initialize Q, if needed.
C
      DO 10  I = 1, K
         J = 0
         IF ( LINIQ ) THEN
            J = I
         ELSE IF ( LPARQ ) THEN
            J = -QIND(I)
         END IF
         IF ( J.GT.0 )
     $      CALL DLASET( 'Full', N, N, ZERO, ONE, Q(1,1,J), LDQ1 )
   10 CONTINUE
C
C     If all factors are already upper triangular, return.
C
      IF ( ILO.EQ.IHI ) THEN
         DWORK(1) = ONE
         RETURN
      END IF
C
C     Compute maps for accessing A and Q.
C
      MAPA = K
      MAPQ = 2*K
      CALL MB03BA( K, H, S, SMULT, IWORK(MAPA+1), IWORK(MAPQ+1) )
C
C     Compute a certain subset of the set ( i : s(i) = smult ).
C
      DO 20  I = 1, K
         AIND = IWORK(MAPA+I)
         IF ( S(AIND).NE.SMULT ) THEN
            IWORK(AIND) = 0
         ELSE IF ( ALLTRI .AND. AIND.NE.H ) THEN
            IWORK(AIND) = 0
         ELSE
            IWORK(AIND) = 1
         END IF
   20 CONTINUE
C
C     Find the maximal element in this set.
C
      MAXSET = 0
C
      DO 30  I = K, 1, -1
         AIND = IWORK(MAPA+I)
         IF ( MAXSET.EQ.0 .AND. IWORK(AIND).EQ.1 )
     $      MAXSET = I
   30 CONTINUE
C
C     Transform all matrices which are not in the set to upper
C     triangular form.
C
      I2    = MIN( N, ILO+1 )
      I3    = MIN( N, IHI+1 )
      IWRK  = M + 1
      WMAX  = LDWORK - M
      LINDQ = .FALSE.
C
      DO 40  I = K, 1, -1
         AIND = IWORK(MAPA+I)
C
         IF ( IWORK(AIND).EQ.0 ) THEN
            INDQ = IWORK(MAPQ+I)
            IF ( LPARQ ) THEN
               INDQ  = ABS( QIND(INDQ) )
               LINDQ = INDQ.GT.0
            END IF
            AINDP = IWORK(MAPA+I-1)
C
            IF ( S(AIND).EQ.SMULT ) THEN
C
C              Do a QR Decomposition of A_AIND.
C
C              Workspace: need   2*M,  M = IHI - ILO + 1;
C                         prefer M + M*NB.
C
               CALL DGEQRF( M, M, A(ILO,ILO,AIND), LDA1, DWORK,
     $                      DWORK(IWRK), WMAX, IER )
C
C              Update the rows ILO:IHI in columns IHI+1:N of A_AIND.
C
C              Workspace: need   M +  N-IHI;
C                         prefer M + (N-IHI)*NB.
C
               IF ( IHI.LT.N )
     $            CALL DORMQR( 'Left', 'Trans', M, N-IHI, M,
     $                         A(ILO,ILO,AIND), LDA1, DWORK,
     $                         A(ILO,IHI+1,AIND), LDA1, DWORK(IWRK),
     $                         WMAX, IER )
C
C              Update A_AINDP.
C
C              Workspace: need   M + MAX( IHI, N-ILO+1 );
C                         prefer M + MAX( IHI, N-ILO+1 )*NB.
C
               IF ( S(AINDP).EQ.SMULT ) THEN
                  CALL DORMQR( 'Right', 'NoTran', IHI, M, M,
     $                         A(ILO,ILO,AIND), LDA1, DWORK,
     $                         A(1,ILO,AINDP),  LDA1, DWORK(IWRK), WMAX,
     $                         IER )
               ELSE
                  CALL DORMQR( 'Left', 'Trans', M, N-ILO+1, M,
     $                         A(ILO,ILO,AIND),  LDA1, DWORK,
     $                         A(ILO,ILO,AINDP), LDA1, DWORK(IWRK),
     $                         WMAX, IER )
               END IF
C
C              Update the transformation matrix.
C
               IF ( LINIQ ) THEN
C
C                 Workspace: need   2*M;
C                            prefer M + M*NB.
C
                  CALL DLASET( 'Full', N, ILO-1, ZERO, ONE, Q(1,1,INDQ),
     $                         LDQ1 )
                  CALL DLASET( 'Full', ILO-1, N-ILO+1, ZERO, ZERO,
     $                         Q(1,ILO,INDQ), LDQ1 )
                  CALL DLACPY( 'Lower', M-1, M-1, A(I2,ILO,AIND), LDA1,
     $                         Q(I2,ILO,INDQ), LDQ1 )
                  CALL DORGQR( M, M, M, Q(ILO,ILO,INDQ), LDQ1, DWORK,
     $                         DWORK(IWRK), WMAX, IER )
                  CALL DLASET( 'Full', N-IHI, IHI, ZERO, ZERO,
     $                         Q(I3,ILO,INDQ), LDQ1 )
                  CALL DLASET( 'Full', IHI, N-IHI, ZERO, ZERO,
     $                         Q(1,I3,INDQ), LDQ1 )
                  CALL DLASET( 'Full', N-IHI, N-IHI, ZERO, ONE,
     $                         Q(I3,I3,INDQ), LDQ1 )
               ELSE IF ( LCMPQ .OR. LINDQ ) THEN
C
C                 Workspace: need   M + IHI;
C                            prefer M + IHI*NB.
C
                  CALL DORMQR( 'Right', 'NoTran', IHI, M, M,
     $                         A(ILO,ILO,AIND), LDA1, DWORK,
     $                         Q(1,ILO,INDQ),   LDQ1, DWORK(IWRK), WMAX,
     $                         IER )
               END IF
C
            ELSE
C
C              Do an RQ Decomposition of A_AIND.
C
C              Workspace: need   2*M;
C                         prefer M + M*NB.
C
               CALL DGERQF( M, M, A(ILO,ILO,AIND), LDA1, DWORK,
     $                      DWORK(IWRK), WMAX, IER )
C
C              Update the rows 1:ILO-1 in columns ILO:IHI of A_AIND.
C
C              Workspace: need   M +  ILO - 1;
C                         prefer M + (ILO - 1)*NB.
C
               IF ( ILO.GT.1 )
     $            CALL DORMRQ( 'Right', 'Trans', ILO-1, M, M,
     $                         A(ILO,ILO,AIND), LDA1, DWORK,
     $                         A(1,ILO,AIND),   LDA1, DWORK(IWRK), WMAX,
     $                         IER )
C
C              Update A_AINDP.
C
C              Workspace: need   M + MAX( IHI, N-ILO+1 );
C                         prefer M + MAX( IHI, N-ILO+1 )*NB.
C
               IF ( S(AINDP).EQ.SMULT ) THEN
                  CALL DORMRQ( 'Right', 'Trans', IHI, M, M,
     $                         A(ILO,ILO,AIND), LDA1, DWORK,
     $                         A(1,ILO,AINDP),  LDA1, DWORK(IWRK), WMAX,
     $                         IER )
               ELSE
                  CALL DORMRQ( 'Left', 'NoTran', M, N-ILO+1, M,
     $                         A(ILO,ILO,AIND),  LDA1, DWORK,
     $                         A(ILO,ILO,AINDP), LDA1, DWORK(IWRK),
     $                         WMAX, IER )
               END IF
C
C	         Update the transformation matrix.
C
C              Workspace: need   M + IHI;
C                         prefer M + IHI*NB.
C
               IF ( LCMPQ .OR. LINDQ )
     $            CALL DORMRQ( 'Right', 'Trans', IHI, M, M,
     $                         A(ILO,ILO,AIND), LDA1, DWORK,
     $                         Q(1,ILO,INDQ),   LDQ1, DWORK(IWRK), WMAX,
     $                         IER )
            END IF
C
            CALL DLASET( 'Lower', M-1, M-1, ZERO, ZERO, A(I2,ILO,AIND),
     $                   LDA1 )
         END IF
C
   40 CONTINUE
C
C     Reduce A_1 to upper Hessenberg form and the other matrices of the
C     set to upper triangular form.
C
      DUM(1) = ZERO
      LINDQ  = .FALSE.
C
      DO 120  J = ILO, IHI - 1
C
C        UPIDX denotes the last remaining nonzero element in the
C        j-th column.
C
         UPIDX = J
C
         DO 110  LT = K + MAXSET, MAXSET + 1, -1
            L = LT
            IF ( L.GT.K )
     $         L = L - K
            IF ( L.EQ.1 )
     $         UPIDX = J + 1
C
            IF ( UPIDX.LT.N ) THEN
               AIND = IWORK(MAPA+L)
               INDQ = IWORK(MAPQ+L)
               IF ( LPARQ ) THEN
                  INDQ  = ABS( QIND(INDQ) )
                  LINDQ = INDQ.GT.0
               END IF
               IF ( L.LE.1 ) THEN
                  AINDP = IWORK(MAPA+K)
               ELSE
                  AINDP = IWORK(MAPA+L-1)
               END IF
C
               IF ( IWORK(AIND).EQ.1 .AND. IWORK(AINDP).EQ.1 ) THEN
C
C                 Case 1: AIND and AINDP are in the set.
C                 Annihilate A(UPIDX+1:ihi,j,AIND) with Householder.
C
                  CALL DLARFG( IHI-UPIDX+1, A(UPIDX,J,AIND),
     $                         A(UPIDX+1,J,AIND), 1, TAU )
                  ALPHA = A(UPIDX,J,AIND)
                  A(UPIDX,J,AIND) = ONE
C
C                 Update the affected matrices.
C
C                 Workspace: need  MAX( N - ILO, IHI ), if COMPQ =  'N';
C                 Workspace: need  N,                   if COMPQ <> 'N'.
C
                  CALL DLARF( 'Left', IHI-UPIDX+1, N-J, A(UPIDX,J,AIND),
     $                        1, TAU, A(UPIDX,J+1,AIND), LDA1, DWORK )
                  CALL DLARF( 'Right', IHI, N-UPIDX+1, A(UPIDX,J,AIND),
     $                        1, TAU, A(1,UPIDX,AINDP), LDA1, DWORK )
                  IF ( LCMPQ .OR. LINDQ )
     $               CALL DLARF( 'Right', N, N-UPIDX+1, A(UPIDX,J,AIND),
     $                           1, TAU, Q(1,UPIDX,INDQ), LDQ1, DWORK )
                  A(UPIDX,J,AIND) = ALPHA
                  CALL DCOPY( IHI-UPIDX, DUM, 0, A(UPIDX+1,J,AIND), 1 )
C
               ELSE IF ( IWORK(AIND).EQ.1 ) THEN
                  POS = 1
C
C                 Case 2: AIND is in the set, but AINDP is not.
C                 Annihilate A(UPIDX+1:n,j,AIND) with Givens rotations.
C
C                 Workspace: need   2*(M - 1).
C
                  DO 50  I = IHI, UPIDX + 1, -1
                     TEMP = A(I-1,J,AIND)
                     CALL DLARTG( TEMP, A(I,J,AIND), DWORK(POS),
     $                            DWORK(POS+1), A(I-1,J,AIND) )
                     A(I,J,AIND) = ZERO
                     CALL DROT( N-J, A(I-1,J+1,AIND), LDA1,
     $                          A(I,J+1,AIND), LDA1, DWORK(POS),
     $                          DWORK(POS+1) )
                     POS = POS + 2
   50             CONTINUE
C
C                 Update the corresponding transformation matrix.
C
                  IF ( LCMPQ .OR. LINDQ ) THEN
                     POS = 1
C
                     DO 60  I = IHI, UPIDX + 1, -1
                        CALL DROT( N, Q(1,I-1,INDQ), 1, Q(1,I,INDQ), 1,
     $                             DWORK(POS), DWORK(POS+1) )
                        POS = POS + 2
   60                CONTINUE
C
                  END IF
C
               ELSE
C
C                 Case 3: Neither AIND nor AINDP are in the set.
C                 Propagate rotations over upper triangular matrices.
C
C                 Workspace: need   2*(M - 1).
C
                  POS = 1
                  IF ( S(AIND).EQ.SMULT ) THEN
C
                     DO 70  I = IHI, UPIDX + 1, -1
                        CALL DROT( I, A(1,I-1,AIND), 1, A(1,I,AIND), 1,
     $                             DWORK(POS), DWORK(POS+1) )
                        TEMP = A(I-1,I-1,AIND)
                        CALL DLARTG( TEMP, A(I,I-1,AIND), DWORK(POS),
     $                               DWORK(POS+1), A(I-1,I-1,AIND) )
                        A(I,I-1,AIND) = ZERO
                        CALL DROT( N-I+1, A(I-1,I,AIND), LDA1,
     $                             A(I,I,AIND), LDA1, DWORK(POS),
     $                             DWORK(POS+1) )
                        POS = POS + 2
   70                CONTINUE
C
                  ELSE
C
                     DO 80  I = IHI, UPIDX + 1, -1
                        CALL DROT( N-I+2, A(I-1,I-1,AIND), LDA1,
     $                             A(I,I-1,AIND), LDA1, DWORK(POS),
     $                             DWORK(POS+1) )
                        TEMP = A(I,I,AIND)
C
C                       Use a transposed rotation to get a unified
C                       treatment when applying the transformations.
C
                        CALL DLARTG( TEMP, -A(I,I-1,AIND), DWORK(POS),
     $                               DWORK(POS+1), A(I,I,AIND) )
                        A(I,I-1,AIND) = ZERO
                        CALL DROT( I-1, A(1,I-1,AIND), 1, A(1,I,AIND),
     $                             1, DWORK(POS), DWORK(POS+1) )
                        POS = POS + 2
   80                CONTINUE
C
                  END IF
C
C                 Update the corresponding transformation matrix.
C
                  IF ( LCMPQ .OR. LINDQ ) THEN
                     POS = 1
C
                     DO 90  I = IHI, UPIDX + 1, -1
                        CALL DROT( N, Q(1,I-1,INDQ), 1, Q(1,I,INDQ), 1,
     $                             DWORK(POS), DWORK(POS+1) )
                        POS = POS + 2
   90                CONTINUE
C
                  END IF
C
C                 If AINDP is in the set, then apply all rotations on
C                 this matrix.
C
                  IF ( IWORK(AINDP).EQ.1 ) THEN
                     POS = 1
C
                     DO 100  I = IHI, UPIDX + 1, -1
                        CALL DROT( IHI, A(1,I-1,AINDP), 1, A(1,I,AINDP),
     $                             1, DWORK(POS), DWORK(POS+1) )
                        POS = POS + 2
  100                CONTINUE
C
                  END IF
C
               END IF
C
            END IF
C
  110    CONTINUE
C
  120 CONTINUE
C
      DWORK(1) = OPTWRK
      RETURN
C *** Last line of MB03VW ***
      END

