      SUBROUTINE MB03BD( JOB, DEFL, COMPQ, QIND, K, N, H, ILO, IHI, S,
     $                   A, LDA1, LDA2, Q, LDQ1, LDQ2, ALPHAR, ALPHAI,
     $                   BETA, SCAL, IWORK, LIWORK, DWORK, LDWORK,
     $                   IWARN, INFO )
C
C     PURPOSE
C
C     To find the eigenvalues of the generalized matrix product
C
C                  S(1)           S(2)                 S(K)
C          A(:,:,1)     * A(:,:,2)     * ... * A(:,:,K)
C
C     where A(:,:,H) is upper Hessenberg and A(:,:,i), i <> H, is upper
C     triangular, using a double-shift version of the periodic
C     QZ method. In addition, A may be reduced to periodic Schur form:
C     A(:,:,H) is upper quasi-triangular and all the other factors
C     A(:,:,I) are upper triangular. Optionally, the 2-by-2 triangular
C     matrices corresponding to 2-by-2 diagonal blocks in A(:,:,H)
C     are so reduced that their product is a 2-by-2 diagonal matrix.
C
C     If COMPQ = 'U' or COMPQ = 'I', then the orthogonal factors are
C     computed and stored in the array Q so that for S(I) = 1,
C
C                         T
C             Q(:,:,I)(in)   A(:,:,I)(in)   Q(:,:,MOD(I,K)+1)(in)
C                                                                 T  (1)
C         =   Q(:,:,I)(out)  A(:,:,I)(out)  Q(:,:,MOD(I,K)+1)(out),
C
C     and for S(I) = -1,
C
C                                  T
C             Q(:,:,MOD(I,K)+1)(in)   A(:,:,I)(in)   Q(:,:,I)(in)
C                                                                 T  (2)
C         =   Q(:,:,MOD(I,K)+1)(out)  A(:,:,I)(out)  Q(:,:,I)(out).
C
C     A partial generation of the orthogonal factors can be realized
C     via the array QIND.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOB     CHARACTER*1
C             Specifies the computation to be performed, as follows:
C             = 'E': compute the eigenvalues only; A will not
C                    necessarily be put into periodic Schur form;
C             = 'S': put A into periodic Schur form, and return the
C                    eigenvalues in ALPHAR, ALPHAI, BETA, and SCAL;
C             = 'T': as JOB = 'S', but A is put into standardized
C                    periodic Schur form, that is, the general product
C                    of the 2-by-2 triangular matrices corresponding to
C                    a complex eigenvalue is diagonal.
C
C     DEFL    CHARACTER*1
C             Specifies the deflation strategy to be used, as follows:
C             = 'C': apply a careful deflation strategy, that is,
C                    the criteria are based on the magnitudes of
C                    neighboring elements and infinite eigenvalues are
C                    only deflated at the top; this is the recommended
C                    option;
C             = 'A': apply a more aggressive strategy, that is,
C                    elements on the subdiagonal or diagonal are set
C                    to zero as soon as they become smaller in magnitude
C                    than eps times the norm of the corresponding
C                    factor; this option is only recommended if
C                    balancing is applied beforehand and convergence
C                    problems are observed.
C
C     COMPQ   CHARACTER*1
C             Specifies whether or not the orthogonal transformations
C             should be accumulated in the array Q, as follows:
C             = 'N': do not modify Q;
C             = 'U': modify (update) the array Q by the orthogonal
C                    transformations that are applied to the matrices in
C                    the array A to reduce them to periodic Schur form;
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
C     Input/Output Parameters
C
C     K       (input)  INTEGER
C             The number of factors.  K >= 1.
C
C     N       (input)  INTEGER
C             The order of each factor in the array A.  N >= 0.
C
C     H       (input)  INTEGER
C             Hessenberg index. The factor A(:,:,H) is on entry in upper
C             Hessenberg form.  1 <= H <= K.
C
C     ILO     (input)  INTEGER
C     IHI     (input)  INTEGER
C             It is assumed that each factor in A is already upper
C             triangular in rows and columns 1:ILO-1 and IHI+1:N.
C             1 <= ILO <= IHI <= N, if N > 0;
C             ILO = 1 and IHI  = 0, if N = 0.
C
C     S       (input)  INTEGER array, dimension (K)
C             The leading K elements of this array must contain the
C             signatures of the factors. Each entry in S must be either
C             1 or -1.
C
C     A       (input/output)  DOUBLE PRECISION array, dimension
C                             (LDA1,LDA2,K)
C             On entry, the leading N-by-N-by-K part of this array
C             must contain the factors in upper Hessenberg-triangular
C             form, that is, A(:,:,H) is upper Hessenberg and the other
C             factors are upper triangular.
C             On exit, if JOB = 'S' and INFO = 0, the leading
C             N-by-N-by-K part of this array contains the factors of
C             A in periodic Schur form, that is, A(:,:,H) is upper quasi
C             triangular and the other factors are upper triangular.
C             On exit, if JOB = 'T' and INFO = 0, the leading
C             N-by-N-by-K part of this array contains the factors of
C             A as for the option JOB = 'S', but the product of the
C             triangular factors corresponding to a 2-by-2 block in
C             A(:,:,H) is diagonal.
C             On exit, if JOB = 'E', then the leading N-by-N-by-K part
C             of this array contains meaningless elements in the off-
C             diagonal blocks. Consequently, the formulas (1) and (2)
C             do not hold for the returned A and Q (if COMPQ <> 'N')
C             in this case.
C
C     LDA1    INTEGER
C             The first leading dimension of the array A.
C             LDA1 >= MAX(1,N).
C
C     LDA2    INTEGER
C             The second leading dimension of the array A.
C             LDA2 >= MAX(1,N).
C
C     Q       (input/output)  DOUBLE PRECISION array, dimension
C                             (LDQ1,LDQ2,K)
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
C     ALPHAR  (output) DOUBLE PRECISION array, dimension (N)
C             On exit, if INFO = 0, the leading N elements of this array
C             contain the scaled real parts of the eigenvalues of the
C             matrix product A. The i-th eigenvalue of A is given by
C
C             (ALPHAR(I) + ALPHAI(I)*SQRT(-1))/BETA(I) * BASE**SCAL(I),
C
C             where BASE is the machine base (often 2.0). Complex
C             conjugate eigenvalues appear in consecutive locations.
C
C     ALPHAI  (output) DOUBLE PRECISION array, dimension (N)
C             On exit, if INFO = 0, the leading N elements of this array
C             contain the scaled imaginary parts of the eigenvalues
C             of A.
C
C     BETA    (output) DOUBLE PRECISION array, dimension (N)
C             On exit, if INFO = 0, the leading N elements of this array
C             contain indicators for infinite eigenvalues. That is, if
C             BETA(I) = 0.0, then the i-th eigenvalue is infinite.
C             Otherwise BETA(I) is set to 1.0.
C
C     SCAL    (output) INTEGER array, dimension (N)
C             On exit, if INFO = 0, the leading N elements of this array
C             contain the scaling parameters for the eigenvalues of A.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (LIWORK)
C             On exit, if INFO = 0, IWORK(1) returns the optimal LIWORK,
C             and if IWARN > N, the nonzero absolute values in IWORK(2),
C             ..., IWORK(N+1) are indices of the possibly inaccurate
C             eigenvalues, as well as of the corresponding 1-by-1 or
C             2-by-2 diagonal blocks of the factors in the array A.
C             The 2-by-2 blocks correspond to negative values in IWORK.
C             One negative value is stored for each such eigenvalue
C             pair. Its modulus indicates the starting index of a
C             2-by-2 block. This is also done for any value of IWARN,
C             if a 2-by-2 block is found to have two real eigenvalues.
C             On exit, if INFO = -22, IWORK(1) returns the minimum value
C             of LIWORK.
C
C     LIWORK  INTEGER
C             The length of the array IWORK.  LIWORK  >= 2*K+N.
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal LDWORK,
C             and DWORK(2), ..., DWORK(1+K) contain the Frobenius norms
C             of the factors of the formal matrix product used by the
C             algorithm.
C             On exit, if INFO = -24, DWORK(1) returns the minimum value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The length of the array DWORK.
C             LDWORK >= K + MAX( 2*N, 8*K ).
C
C     Warning Indicator
C
C     IWARN   INTEGER
C             = 0        :  no warnings;
C             = 1,..,N-1 :  A is in periodic Schur form, but the
C                           algorithm was not able to reveal information
C                           about the eigenvalues from the 2-by-2
C                           blocks.
C                           ALPHAR(i), ALPHAI(i), BETA(i) and SCAL(i),
C                           can be incorrect for i = 1, ..., IWARN+1;
C             = N        :  some eigenvalues might be inaccurate;
C             = N+1      :  some eigenvalues might be inaccurate, and
C                           details can be found in IWORK.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0      :  succesful exit;
C             < 0      :  if INFO = -i, the i-th argument had an illegal
C                         value;
C             = 1,..,N :  the periodic QZ iteration did not converge.
C                         A is not in periodic Schur form, but
C                         ALPHAR(i), ALPHAI(i), BETA(i) and SCAL(i), for
C                         i = INFO+1,...,N should be correct.
C
C     METHOD
C
C     A modified version of the periodic QZ algorithm is used [1], [2].
C
C     REFERENCES
C
C     [1] Bojanczyk, A., Golub, G. H. and Van Dooren, P.
C         The periodic Schur decomposition: algorithms and applications.
C         In F.T. Luk (editor), Advanced Signal Processing Algorithms,
C         Architectures, and Implementations III, Proc. SPIE Conference,
C         vol. 1770, pp. 31-42, 1992.
C
C     [2] Kressner, D.
C         An efficient and reliable implementation of the periodic QZ
C         algorithm. IFAC Workshop on Periodic Control Systems (PSYCO
C         2001), Como (Italy), August 27-28 2001. Periodic Control
C         Systems 2001 (IFAC Proceedings Volumes), Pergamon.
C
C     NUMERICAL ASPECTS
C
C     The implemented method is numerically backward stable.
C                                 3
C     The algorithm requires 0(K N ) floating point operations.
C
C     CONTRIBUTOR
C
C     D. Kressner, Technical Univ. Berlin, Germany, June 2001.
C
C     REVISIONS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     July 2009, SLICOT Library version of the routine PHGEQZ.
C     V. Sima, June 2010, July 2010, Nov. 2010, Sep. 2011, Oct. 2011,
C     Jan. 2013, Feb. 2013, July 2013, Sep. 2016, Nov. 2016, Apr. 2018.
C     Dec. 2018, Jan. 2019, Feb. 2019, Mar. 2019, Aug.-Sep. 2019, Dec.
C     2019, Jan.-Apr. 2020.
C
C     KEYWORDS
C
C     Eigenvalues, QZ algorithm, periodic QZ algorithm, orthogonal
C     transformation.
C
C     ******************************************************************
C
C     .. Parameters ..
C     .. NITER is the number of consecutive iterations for a deflated ..
C     .. subproblem before switching from implicit to explicit shifts...
C     .. MCOUNT is, similarly, the maximum number of consecutive ..
C     .. iterations before switching from explicit to implicit shifts...
C
      INTEGER           MCOUNT, NITER
      PARAMETER         ( MCOUNT = 1, NITER = 10 )
      DOUBLE PRECISION  ZERO, ONE, TEN
      PARAMETER         ( ZERO = 0.0D+0, ONE = 1.0D+0, TEN = 1.0D+1 )
C     .. Scalar Arguments ..
      CHARACTER         COMPQ, DEFL, JOB
      INTEGER           H, IHI, ILO, INFO, IWARN, K, LDA1, LDA2, LDQ1,
     $                  LDQ2, LDWORK, LIWORK, N
C     .. Array Arguments ..
      INTEGER           IWORK(*), QIND(*), S(*), SCAL(*)
      DOUBLE PRECISION  A(LDA1,LDA2,*), ALPHAI(*), ALPHAR(*), BETA(*),
     $                  DWORK(*), Q(LDQ1,LDQ2,*)
C     .. Local Arrays ..
      DOUBLE PRECISION  MACPAR(5)
C     .. Local Scalars ..
      LOGICAL           ADEFL, ISINF, LCMPQ, LINIQ, LPARQ, LSCHR, LSVD
      CHARACTER         SHFT
      INTEGER           AIND, COUNT, COUNTE, I, IERR, IFIRST, IFRSTM,
     $                  IITER, ILAST, ILASTM, IN, IO, J, J1, JDEF,
     $                  JITER, JLO, L, LDEF, LM, MAXIT, NTRA, OPTDW,
     $                  OPTIW, QI, SINV, TITER, ZITER
      DOUBLE PRECISION  A1, A2, A3, A4, BASE, CS, CS1, CS2, LGBAS, NRM,
     $                  SAFMAX, SAFMIN, SDET, SMLNUM, SN, SN1, SN2,
     $                  SVMN, TEMP, TEMP2, TOL, TOLL, ULP, W1, W2
C     .. Workspace Pointers ..
      INTEGER           MAPA, MAPH, MAPQ, PDW, PFREE, PNORM
C     .. External Functions ..
      LOGICAL           LSAME
      DOUBLE PRECISION  DLAMCH, DLANHS, DLAPY2, DLAPY3
      EXTERNAL          DLAMCH, DLANHS, DLAPY2, DLAPY3, LSAME
C     .. External Subroutines ..
      EXTERNAL          DLABAD, DLADIV, DLARTG, DLAS2, DLASET, DROT,
     $                  MA01BD, MB03AB, MB03AF, MB03BA, MB03BB, MB03BC,
     $                  MB03BF, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, DBLE, INT, LOG, MAX, MIN, MOD, SIGN, SQRT
C
C     .. Executable Statements ..
C
C     Decode the scalar input parameters.
C
      LSVD  = LSAME( JOB,   'T' )
      LSCHR = LSAME( JOB,   'S' ) .OR. LSVD
      LINIQ = LSAME( COMPQ, 'I' )
      LCMPQ = LSAME( COMPQ, 'U' ) .OR. LINIQ
      LPARQ = LSAME( COMPQ, 'P' )
      ADEFL = LSAME( DEFL,  'A' )
      IWARN = 0
      OPTDW = K + MAX( 2*N, 8*K )
      OPTIW = 2*K + N
C
C     Check the scalar input parameters.
C
      INFO = 0
      IF ( .NOT. ( LSCHR .OR. LSAME( JOB, 'E' ) ) ) THEN
         INFO = -1
      ELSE IF ( .NOT.( ADEFL .OR. LSAME( DEFL, 'C' ) ) ) THEN
         INFO = -2
      ELSE IF ( .NOT.( LCMPQ .OR. LPARQ .OR. LSAME( COMPQ, 'N' ) ) )
     $      THEN
         INFO = -3
      ELSE IF ( K.LT.1 ) THEN
         INFO = -5
      ELSE IF ( N.LT.0 ) THEN
         INFO = -6
      ELSE IF ( H.LT.1 .OR. H.GT.K ) THEN
         INFO = -7
      ELSE IF ( ILO.LT.1 ) THEN
         INFO = -8
      ELSE IF ( IHI.GT.N .OR. IHI.LT.ILO-1 ) THEN
         INFO = -9
      ELSE IF ( LDA1.LT.MAX( 1, N ) ) THEN
         INFO = -12
      ELSE IF ( LDA2.LT.MAX( 1, N ) ) THEN
         INFO = -13
      ELSE IF ( LDQ1.LT.1 .OR. ( ( LCMPQ .OR. LPARQ )
     $                             .AND. LDQ1.LT.N ) ) THEN
         INFO = -15
      ELSE IF ( LDQ2.LT.1 .OR. ( ( LCMPQ .OR. LPARQ )
     $                             .AND. LDQ2.LT.N ) ) THEN
         INFO = -16
      ELSE IF ( LIWORK.LT.OPTIW ) THEN
         IWORK(1) = OPTIW
         INFO = -22
      ELSE IF ( LDWORK.LT.OPTDW ) THEN
         DWORK(1) = DBLE( OPTDW )
         INFO = -24
      END IF
C
C     Return if there were illegal values.
C
      IF ( INFO.NE.0 ) THEN
         CALL XERBLA( 'MB03BD', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF ( N.EQ.0 ) THEN
         DWORK(1) = ONE
         IWORK(1) = 1
         RETURN
      END IF
C
C     Compute Maps for accessing A and Q.
C
      MAPA = 0
      MAPH = 2
      MAPQ = K
      QI   = 0
      CALL MB03BA( K, H, S, SINV, IWORK(MAPA+1), IWORK(MAPQ+1) )
C
C     Machine Constants.
C
      IN = IHI + 1 - ILO
      SAFMIN = DLAMCH( 'SafeMinimum' )
      SAFMAX = ONE / SAFMIN
      ULP    = DLAMCH( 'Precision' )
      TOLL   = TEN*ULP
      CALL DLABAD( SAFMIN, SAFMAX )
      SMLNUM = SAFMIN*( IN / ULP )
      BASE   = DLAMCH( 'Base' )
      LGBAS  = LOG( BASE )
C
      MACPAR(2) = DLAMCH( 'Underflow' )
      IF ( LSVD ) THEN
         MACPAR(1) = DLAMCH( 'ORmax' )
         MACPAR(3) = SAFMIN
         MACPAR(4) = DLAMCH( 'Epsilon' )
         MACPAR(5) = BASE
      END IF
      IF ( K.GE.INT( LOG( MACPAR(2) ) / LOG( ULP ) ) ) THEN
C
C        Start Iteration with a controlled zero shift.
C
         ZITER = -1
      ELSE
         ZITER = 0
      END IF
C
C     Initialize IWORK (needed in case of loosing accuracy).
C
      DO 10  I = 2*K + 1, 2*K + N
         IWORK(I) = 0
   10 CONTINUE
C
C     Compute norms and initialize Q.
C
      PNORM = 0
      PFREE = K
      DO 20  I = 1, K
         AIND = IWORK(MAPA+I)
         DWORK(I) = DLANHS( 'Frobenius', IN, A(ILO,ILO,AIND), LDA1,
     $                      DWORK )
         J = 0
         IF ( LINIQ ) THEN
            J = I
         ELSE IF ( LPARQ ) THEN
            J = -QIND(I)
         END IF
         IF ( J.NE.0 )
     $      CALL DLASET( 'Full', N, N, ZERO, ONE, Q(1,1,J), LDQ1 )
   20 CONTINUE
C
C     Set Eigenvalues IHI+1:N.
C
      DO 30  J = IHI + 1, N
         CALL MA01BD( BASE, LGBAS, K, S, A(J,J,1), LDA1*LDA2, ALPHAR(J),
     $                BETA(J), SCAL(J) )
         ALPHAI(J) = ZERO
   30 CONTINUE
C
C     If IHI < ILO, skip QZ steps.
C
      IF ( IHI.LT.ILO )
     $   GO TO 550
C
C     MAIN PERIODIC QZ ITERATION LOOP.
C
C     Initialize dynamic indices.
C
C     Eigenvalues ILAST+1:N have been found.
C        Column operations modify rows IFRSTM:whatever.
C        Row operations modify columns whatever:ILASTM.
C
C     If only eigenvalues are being computed, then
C        IFRSTM is the row of the last splitting row above row ILAST;
C        this is always at least ILO.
C     IITER counts iterations since the last eigenvalue was found,
C        to tell when to use an observed zero or exceptional shift.
C     MAXIT is the maximum number of QZ sweeps allowed.
C
      ILAST = IHI
      IF ( LSCHR ) THEN
         IFRSTM = 1
         ILASTM = N
      ELSE
         IFRSTM = ILO
         ILASTM = IHI
      END IF
      IITER  = 0
      TITER  = 0
      COUNT  = 0
      COUNTE = 0
      MAXIT  = 120 * IN
C
      DO  540 JITER = 1, MAXIT
C
C        Special Case: ILAST = ILO.
C
         IF ( ILAST.EQ.ILO )
     $      GO TO 390
C
C        **************************************************************
C        *                     CHECK FOR DEFLATION                    *
C        **************************************************************
C
C        Test 1:  Deflation in the Hessenberg matrix.
C
         IF ( ADEFL )
     $      TOL = MAX( SAFMIN, DWORK(PNORM+1)*ULP )
         AIND = IWORK(MAPA+1)
         JLO = ILO
         DO 40  J = ILAST, ILO + 1, -1
            IF ( .NOT.ADEFL ) THEN
               TOL = ABS( A(J-1,J-1,AIND) ) + ABS( A(J,J,AIND) )
               IF ( TOL.EQ.ZERO )
     $             TOL = DLANHS( '1', J-ILO+1, A(ILO,ILO,AIND), LDA1,
     $                           DWORK )
               TOL = MAX( ULP*TOL, SMLNUM )
            END IF
            IF ( ABS( A(J,J-1,AIND) ).LE.TOL ) THEN
               A(J,J-1,AIND) = ZERO
               JLO = J
               IF ( J.EQ.ILAST )
     $            GO TO 390
               GO TO 50
            END IF
   40    CONTINUE
C
   50    CONTINUE
C
C        Test 2:  Deflation in the triangular matrices with index 1.
C
         DO 70  LDEF = 2, K
            AIND = IWORK(MAPA+LDEF)
            IF ( S(AIND).EQ.SINV ) THEN
               IF ( ADEFL )
     $            TOL = MAX( SAFMIN, DWORK(PNORM+LDEF)*ULP )
               DO 60  J = ILAST, JLO, -1
                  IF ( .NOT.ADEFL ) THEN
                     IF ( J.EQ.ILAST ) THEN
                        TOL = ABS( A(J-1,J,AIND) )
                     ELSE IF ( J.EQ.JLO ) THEN
                        TOL = ABS( A(J,J+1,AIND) )
                     ELSE
                        TOL = ABS( A(J-1,J,AIND) )
     $                      + ABS( A(J,J+1,AIND) )
                     END IF
                     IF ( TOL.EQ.ZERO )
     $                  TOL = DLANHS( '1', J-JLO+1, A(JLO,JLO,AIND),
     $                                LDA1, DWORK )
                     TOL = MAX( ULP*TOL, SMLNUM )
                  END IF
                  IF ( ABS( A(J,J,AIND) ).LE.TOL ) THEN
                     A(J,J,AIND) = ZERO
                     GO TO 170
                  END IF
   60          CONTINUE
            END IF
   70    CONTINUE
C
C        Test 3:  Deflation in the triangular matrices with index -1.
C
         DO 90  LDEF = 2, K
            AIND = IWORK(MAPA+LDEF)
            IF ( S(AIND).NE.SINV ) THEN
               IF ( ADEFL )
     $            TOL = MAX( SAFMIN, DWORK(PNORM+LDEF)*ULP )
               DO 80  J = ILAST, JLO, -1
                  IF ( .NOT.ADEFL ) THEN
                     IF ( J.EQ.ILAST ) THEN
                        TOL = ABS( A(J-1,J,AIND) )
                     ELSE IF ( J.EQ.JLO ) THEN
                        TOL = ABS( A(J,J+1,AIND) )
                     ELSE
                        TOL = ABS( A(J-1,J,AIND) )
     $                      + ABS( A(J,J+1,AIND) )
                     END IF
                     IF ( TOL.EQ.ZERO )
     $                  TOL = DLANHS( '1', J-JLO+1, A(JLO,JLO,AIND),
     $                                LDA1, DWORK )
                     TOL = MAX( ULP*TOL, SMLNUM )
                  END IF
                  IF ( ABS( A(J,J,AIND) ).LE.TOL ) THEN
                     A(J,J,AIND) = ZERO
                     GO TO 320
                  END IF
   80          CONTINUE
            END IF
   90    CONTINUE
C
C        Test 4:  Controlled zero shift.
C
         IF ( ZITER.GE.7 .OR. ZITER.LT.0 ) THEN
C
C           Make Hessenberg matrix upper triangular.
C
            AIND = IWORK(MAPA+1)
            PDW  = PFREE + 1
            DO 100 J = JLO, ILAST - 1
               TEMP = A(J,J,AIND)
               CALL DLARTG( TEMP, A(J+1,J,AIND), CS, SN, A(J,J,AIND) )
               A(J+1,J,AIND) = ZERO
               CALL DROT( ILASTM-J, A(J,J+1,AIND), LDA1,
     $                    A(J+1,J+1,AIND), LDA1, CS, SN )
               DWORK(PDW)   = CS
               DWORK(PDW+1) = SN
               PDW = PDW + 2
  100       CONTINUE
            IF ( LCMPQ ) THEN
               QI = IWORK(MAPQ+1)
            ELSE IF ( LPARQ ) THEN
               QI = ABS( QIND(IWORK(MAPQ+1)) )
            END IF
            IF ( QI.NE.0 ) THEN
               PDW = PFREE + 1
               DO 110  J = JLO, ILAST - 1
                  CS = DWORK(PDW)
                  SN = DWORK(PDW+1)
                  PDW = PDW + 2
                  CALL DROT( N, Q(1,J,QI), 1, Q(1,J+1,QI), 1, CS, SN )
  110          CONTINUE
            END IF
C
C           Propagate transformations back to A_1.
C
            DO 150  L = K, 2, -1
               AIND = IWORK(MAPA+L)
               PDW  = PFREE + 1
               IF ( ADEFL )
     $            TOL = MAX( SAFMIN, DWORK(PNORM+L)*ULP )
               IF ( S(AIND).EQ.SINV ) THEN
                  DO 120  J = JLO, ILAST - 1
                     CS = DWORK(PDW)
                     SN = DWORK(PDW+1)
                     IF ( SN.NE.ZERO ) THEN
                        CALL DROT( J+2-IFRSTM, A(IFRSTM,J,AIND), 1,
     $                             A(IFRSTM,J+1,AIND), 1, CS, SN )
C
C                       Check for deflation.
C
                        IF ( .NOT.ADEFL ) THEN
                           TOL = ABS( A(J,J,AIND) ) +
     $                           ABS( A(J+1,J+1,AIND) )
                           IF ( TOL.EQ.ZERO )
     $                        TOL = DLANHS( '1', J-JLO+2,
     $                                      A(JLO,JLO,AIND), LDA1,
     $                                      DWORK )
                           TOL = MAX( ULP*TOL, SMLNUM )
                        END IF
                        IF ( ABS( A(J+1,J,AIND) ).LE.TOL ) THEN
                           CS = ONE
                           SN = ZERO
                           A(J+1,J,AIND) = ZERO
                        END IF
                     END IF
                     IF ( SN.NE.ZERO ) THEN
                        TEMP = A(J,J,AIND)
                        CALL DLARTG( TEMP, A(J+1,J,AIND), CS, SN,
     $                               A(J,J,AIND) )
                        A(J+1,J,AIND) = ZERO
                        CALL DROT( ILASTM-J, A(J,J+1,AIND), LDA1,
     $                             A(J+1,J+1,AIND), LDA1, CS, SN )
                     END IF
                     DWORK(PDW)   = CS
                     DWORK(PDW+1) = SN
                     PDW = PDW + 2
  120             CONTINUE
               ELSE
                  DO 130  J = JLO, ILAST - 1
                     CS = DWORK(PDW)
                     SN = DWORK(PDW+1)
                     IF ( SN.NE.ZERO ) THEN
                        CALL DROT( ILASTM-J+1, A(J,J,AIND), LDA1,
     $                             A(J+1,J,AIND), LDA1, CS, SN )
C
C                       Check for deflation.
C
                        IF ( .NOT.ADEFL ) THEN
                           TOL = ABS( A(J,J,AIND) ) +
     $                           ABS( A(J+1,J+1,AIND) )
                           IF ( TOL.EQ.ZERO )
     $                        TOL = DLANHS( '1', J-JLO+2,
     $                                      A(JLO,JLO,AIND), LDA1,
     $                                      DWORK )
                           TOL = MAX( ULP*TOL, SMLNUM )
                        END IF
                        IF ( ABS( A(J+1,J,AIND) ).LE.TOL ) THEN
                           CS = ONE
                           SN = ZERO
                           A(J+1,J,AIND) = ZERO
                        END IF
                     END IF
                     IF ( SN.NE.ZERO ) THEN
                        TEMP = A(J+1,J+1,AIND)
                        CALL DLARTG( TEMP, -A(J+1,J,AIND), CS, SN,
     $                               A(J+1,J+1,AIND) )
                        A(J+1,J,AIND) = ZERO
                        CALL DROT( J+1-IFRSTM, A(IFRSTM,J,AIND), 1,
     $                             A(IFRSTM,J+1,AIND), 1, CS, SN )
                     END IF
                     DWORK(PDW)   = CS
                     DWORK(PDW+1) = SN
                     PDW = PDW + 2
  130             CONTINUE
               END IF
               IF ( LCMPQ ) THEN
                  QI = IWORK(MAPQ+L)
               ELSE IF ( LPARQ ) THEN
                  QI = ABS( QIND(IWORK(MAPQ+L)) )
               END IF
               IF ( QI.NE.0 ) THEN
                  PDW = PFREE + 1
                  DO 140  J = JLO, ILAST - 1
                     CS  = DWORK(PDW)
                     SN  = DWORK(PDW+1)
                     PDW = PDW + 2
                     IF ( SN.NE.ZERO )
     $                  CALL DROT( N, Q(1,J,QI), 1, Q(1,J+1,QI), 1, CS,
     $                             SN )
  140             CONTINUE
               END IF
  150       CONTINUE
C
C           Apply the transformations to the right hand side of the
C           Hessenberg factor.
C
            AIND = IWORK(MAPA+1)
            PDW  = PFREE + 1
            ZITER = 0
            DO 160  J = JLO, ILAST - 1
               CS = DWORK(PDW)
               SN = DWORK(PDW+1)
               PDW = PDW + 2
               IF ( SN.NE.ZERO ) THEN
                  CALL DROT( J+2-IFRSTM, A(IFRSTM,J,AIND), 1,
     $                       A(IFRSTM,J+1,AIND), 1, CS, SN )
               ELSE
                  ZITER = -1
               END IF
  160       CONTINUE
C
C           No QZ iteration.
C
            GO TO 530
         END IF
C
C        **************************************************************
C        *                     HANDLE DEFLATIONS                      *
C        **************************************************************
C
C        Case I: Deflation occurs in the Hessenberg matrix. The QZ
C                iteration is only applied to the JLO:ILAST part.
C
         IFIRST = JLO
C
C        Go to the periodic QZ steps.
C
         GO TO 420
C
C        Case II: Deflation occurs in a triangular matrix with index 1.
C
C        Do an unshifted periodic QZ step.
C
  170    CONTINUE
         JDEF = J
         AIND = IWORK(MAPA+1)
         PDW  = PFREE + 1
         DO 180  J = JLO, JDEF - 1
            TEMP = A(J,J,AIND)
            CALL DLARTG( TEMP, A(J+1,J,AIND), CS, SN, A(J,J,AIND) )
            A(J+1,J,AIND) = ZERO
            CALL DROT( ILASTM-J, A(J,J+1,AIND), LDA1, A(J+1,J+1,AIND),
     $                 LDA1, CS, SN )
            DWORK(PDW)   = CS
            DWORK(PDW+1) = SN
            PDW = PDW + 2
  180    CONTINUE
         IF ( LCMPQ ) THEN
            QI = IWORK(MAPQ+1)
         ELSE IF ( LPARQ ) THEN
            QI = ABS( QIND(IWORK(MAPQ+1)) )
         END IF
         IF ( QI.NE.0 ) THEN
            PDW = PFREE + 1
            DO 190  J = JLO, JDEF - 1
               CS  = DWORK(PDW)
               SN  = DWORK(PDW+1)
               PDW = PDW + 2
               CALL DROT( N, Q(1,J,QI), 1, Q(1,J+1,QI), 1, CS, SN )
  190       CONTINUE
         END IF
C
C        Propagate the transformations through the triangular matrices.
C        Due to the zero element on the diagonal of the LDEF-th factor,
C        the number of transformations drops by one.
C
         DO 230  L = K, 2, -1
            AIND = IWORK(MAPA+L)
            IF ( L.LT.LDEF ) THEN
               NTRA = JDEF - 2
            ELSE
               NTRA = JDEF - 1
            END IF
            PDW = PFREE + 1
            IF ( S(AIND).EQ.SINV ) THEN
               DO 200  J = JLO, NTRA
                  CS = DWORK(PDW)
                  SN = DWORK(PDW+1)
                  CALL DROT( J+2-IFRSTM, A(IFRSTM,J,AIND), 1,
     $                       A(IFRSTM,J+1,AIND), 1, CS, SN )
                  TEMP = A(J,J,AIND)
                  CALL DLARTG( TEMP, A(J+1,J,AIND), CS, SN,
     $                         A(J,J,AIND) )
                  A(J+1,J,AIND) = ZERO
                  CALL DROT( ILASTM-J, A(J,J+1,AIND), LDA1,
     $                       A(J+1,J+1,AIND), LDA1, CS, SN )
                  DWORK(PDW)   = CS
                  DWORK(PDW+1) = SN
                  PDW = PDW + 2
  200          CONTINUE
            ELSE
               DO 210  J = JLO, NTRA
                  CS = DWORK(PDW)
                  SN = DWORK(PDW+1)
                  CALL DROT( ILASTM-J+1, A(J,J,AIND), LDA1,
     $                       A(J+1,J,AIND), LDA1, CS, SN )
                  TEMP = A(J+1,J+1,AIND)
                  CALL DLARTG( TEMP, -A(J+1,J,AIND), CS, SN,
     $                         A(J+1,J+1,AIND) )
                  A(J+1,J,AIND) = ZERO
                  CALL DROT( J+1-IFRSTM, A(IFRSTM,J,AIND), 1,
     $                       A(IFRSTM,J+1,AIND), 1, CS, SN )
                  DWORK(PDW)   = CS
                  DWORK(PDW+1) = SN
                  PDW = PDW + 2
  210          CONTINUE
            END IF
            IF ( LCMPQ ) THEN
               QI = IWORK(MAPQ+L)
            ELSE IF ( LPARQ ) THEN
               QI = ABS( QIND(IWORK(MAPQ+L)) )
            END IF
            IF ( QI.NE.0 ) THEN
               PDW = PFREE + 1
               DO 220  J = JLO, NTRA
                  CS  = DWORK(PDW)
                  SN  = DWORK(PDW+1)
                  PDW = PDW + 2
                  CALL DROT( N, Q(1,J,QI), 1, Q(1,J+1,QI), 1, CS, SN )
  220          CONTINUE
            END IF
  230    CONTINUE
C
C        Apply the transformations to the right hand side of the
C        Hessenberg factor.
C
         AIND = IWORK(MAPA+1)
         PDW = PFREE + 1
         DO 240  J = JLO, JDEF - 2
            CS = DWORK(PDW)
            SN = DWORK(PDW+1)
            PDW = PDW + 2
            CALL DROT( J+2-IFRSTM, A(IFRSTM,J,AIND), 1,
     $                 A(IFRSTM,J+1,AIND), 1, CS, SN )
  240    CONTINUE
C
C        Do an unshifted periodic QZ step.
C
         PDW = PFREE + 1
         DO 250  J = ILAST, JDEF + 1, -1
            TEMP = A(J,J,AIND)
            CALL DLARTG( TEMP, -A(J,J-1,AIND), CS, SN, A(J,J,AIND) )
            A(J,J-1,AIND) = ZERO
            CALL DROT( J-IFRSTM, A(IFRSTM,J-1,AIND), 1,
     $                 A(IFRSTM,J,AIND), 1, CS, SN )
            DWORK(PDW)   = CS
            DWORK(PDW+1) = SN
            PDW = PDW + 2
  250    CONTINUE
         IF ( LCMPQ ) THEN
            QI = IWORK(MAPQ+2)
         ELSE IF ( LPARQ ) THEN
            QI = ABS( QIND(IWORK(MAPQ+2)) )
         END IF
         IF ( QI.NE.0 ) THEN
            PDW = PFREE + 1
            DO 260  J = ILAST, JDEF + 1, -1
               CS  = DWORK(PDW)
               SN  = DWORK(PDW+1)
               PDW = PDW + 2
               CALL DROT( N, Q(1,J-1,QI), 1, Q(1,J,QI), 1, CS, SN )
  260       CONTINUE
         END IF
C
C        Propagate the transformations through the triangular matrices.
C
         DO 300  L = 2, K
            AIND = IWORK(MAPA+L)
            IF ( L.GT.LDEF ) THEN
               NTRA = JDEF + 2
            ELSE
               NTRA = JDEF + 1
            END IF
            PDW = PFREE + 1
            IF ( S(AIND).NE.SINV ) THEN
               DO 270  J = ILAST, NTRA, -1
                  CS = DWORK(PDW)
                  SN = DWORK(PDW+1)
                  CALL DROT( J+1-IFRSTM, A(IFRSTM,J-1,AIND), 1,
     $                       A(IFRSTM,J,AIND), 1, CS, SN )
                  TEMP = A(J-1,J-1,AIND)
                  CALL DLARTG( TEMP, A(J,J-1,AIND), CS, SN,
     $                         A(J-1,J-1,AIND) )
                  A(J,J-1,AIND) = ZERO
                  CALL DROT( ILASTM-J+1, A(J-1,J,AIND), LDA1,
     $                       A(J,J,AIND), LDA1, CS, SN )
                  DWORK(PDW)   = CS
                  DWORK(PDW+1) = SN
                  PDW = PDW + 2
  270          CONTINUE
            ELSE
               DO 280  J = ILAST, NTRA, -1
                  CS = DWORK(PDW)
                  SN = DWORK(PDW+1)
                  CALL DROT( ILASTM-J+2, A(J-1,J-1,AIND), LDA1,
     $                       A(J,J-1,AIND), LDA1, CS, SN )
                  TEMP = A(J,J,AIND)
                  CALL DLARTG( TEMP, -A(J,J-1,AIND), CS, SN,
     $                         A(J,J,AIND) )
                  A(J,J-1,AIND) = ZERO
                  CALL DROT( J-IFRSTM, A(IFRSTM,J-1,AIND), 1,
     $                       A(IFRSTM,J,AIND), 1, CS, SN )
                  DWORK(PDW)   = CS
                  DWORK(PDW+1) = SN
                  PDW = PDW + 2
  280          CONTINUE
            END IF
            LM = MOD( L, K ) + 1
            IF ( LCMPQ ) THEN
               QI = IWORK(MAPQ+LM)
            ELSE IF ( LPARQ ) THEN
               QI = ABS( QIND(IWORK(MAPQ+LM)) )
            END IF
            IF ( QI.NE.0 ) THEN
               PDW = PFREE + 1
               DO 290  J = ILAST, NTRA, -1
                  CS  = DWORK(PDW)
                  SN  = DWORK(PDW+1)
                  PDW = PDW + 2
                  CALL DROT( N, Q(1,J-1,QI), 1, Q(1,J,QI), 1, CS, SN )
  290          CONTINUE
            END IF
  300    CONTINUE
C
C        Apply the transformations to the left hand side of the
C        Hessenberg factor.
C
         AIND = IWORK(MAPA+1)
         PDW  = PFREE + 1
         DO 310  J = ILAST, JDEF + 2, -1
            CS  = DWORK(PDW)
            SN  = DWORK(PDW+1)
            PDW = PDW + 2
            CALL DROT( ILASTM-J+2, A(J-1,J-1,AIND), LDA1, A(J,J-1,AIND),
     $                 LDA1, CS, SN )
  310    CONTINUE
C
C        No QZ iteration.
C
         GO TO 530
C
C        Case III: Deflation occurs in a triangular matrix with
C                  index -1.
C
  320    CONTINUE
         JDEF = J
         IF ( JDEF.GT.( ( ILAST - JLO + 1 )/2 ) ) THEN
C
C           Chase the zero downwards to the last position.
C
            DO 340  J1 = JDEF, ILAST - 1
               J = J1
               AIND = IWORK(MAPA+LDEF)
               TEMP = A(J,J+1,AIND)
               CALL DLARTG( TEMP, A(J+1,J+1,AIND), CS, SN,
     $                      A(J,J+1,AIND) )
               A(J+1,J+1,AIND) = ZERO
               CALL DROT( ILASTM-J-1, A(J,J+2,AIND), LDA1,
     $                    A(J+1,J+2,AIND), LDA1, CS, SN )
               LM = MOD( LDEF, K ) + 1
               IF ( LCMPQ ) THEN
                  QI = IWORK(MAPQ+LM)
               ELSE IF ( LPARQ ) THEN
                  QI = ABS( QIND(IWORK(MAPQ+LM)) )
               END IF
               IF ( QI.NE.0 )
     $            CALL DROT( N, Q(1,J,QI), 1, Q(1,J+1,QI), 1, CS, SN )
               DO 330  L = 1, K - 1
                  AIND = IWORK(MAPA+LM)
                  IF ( LM.EQ.1 ) THEN
                     CALL DROT( ILASTM-J+2, A(J,J-1,AIND), LDA1,
     $                          A(J+1,J-1,AIND), LDA1, CS, SN )
                     TEMP = A(J+1,J,AIND)
                     CALL DLARTG( TEMP, -A(J+1,J-1,AIND), CS, SN,
     $                            A(J+1,J,AIND) )
                     A(J+1,J-1,AIND) = ZERO
                     CALL DROT( J-IFRSTM+1, A(IFRSTM,J-1,AIND), 1,
     $                          A(IFRSTM,J,AIND), 1, CS, SN )
                     J = J - 1
                  ELSE IF ( S(AIND).EQ.SINV ) THEN
                     CALL DROT( ILASTM-J+1, A(J,J,AIND), LDA1,
     $                          A(J+1,J,AIND), LDA1, CS, SN )
                     TEMP = A(J+1,J+1,AIND)
                     CALL DLARTG( TEMP, -A(J+1,J,AIND), CS, SN,
     $                            A(J+1,J+1,AIND) )
                     A(J+1,J,AIND) = ZERO
                     CALL DROT( J-IFRSTM+1, A(IFRSTM,J,AIND), 1,
     $                          A(IFRSTM,J+1,AIND), 1, CS, SN )
                  ELSE
                     CALL DROT( J-IFRSTM+2, A(IFRSTM,J,AIND), 1,
     $                          A(IFRSTM,J+1,AIND), 1, CS, SN )
                     TEMP = A(J,J,AIND)
                     CALL DLARTG( TEMP, A(J+1,J,AIND), CS, SN,
     $                            A(J,J,AIND) )
                     A(J+1,J,AIND) = ZERO
                     CALL DROT( ILASTM-J, A(J,J+1,AIND), LDA1,
     $                          A(J+1,J+1,AIND), LDA1, CS, SN )
                  END IF
                  LM = MOD( LM, K ) + 1
                  IF ( LCMPQ ) THEN
                     QI = IWORK(MAPQ+LM)
                  ELSE IF ( LPARQ ) THEN
                     QI = ABS( QIND(IWORK(MAPQ+LM)) )
                  END IF
                  IF ( QI.NE.0 )
     $               CALL DROT( N, Q(1,J,QI), 1, Q(1,J+1,QI), 1, CS,
     $                          SN )
  330          CONTINUE
               AIND = IWORK(MAPA+LDEF)
               CALL DROT( J-IFRSTM+1, A(IFRSTM,J,AIND), 1,
     $                    A(IFRSTM,J+1,AIND), 1, CS, SN )
  340       CONTINUE
C
C           Deflate the last element in the Hessenberg matrix.
C
            AIND = IWORK(MAPA+1)
            J = ILAST
            TEMP = A(J,J,AIND)
            CALL DLARTG( TEMP, -A(J,J-1,AIND), CS, SN, A(J,J,AIND) )
            A(J,J-1,AIND) = ZERO
            CALL DROT( J-IFRSTM, A(IFRSTM,J-1,AIND), 1,
     $                 A(IFRSTM,J,AIND), 1, CS, SN )
            IF ( LCMPQ ) THEN
               QI = IWORK(MAPQ+2)
            ELSE IF ( LPARQ ) THEN
               QI = ABS( QIND(IWORK(MAPQ+2)) )
            END IF
            IF ( QI.NE.0 )
     $         CALL DROT( N, Q(1,J-1,QI), 1, Q(1,J,QI), 1, CS, SN )
            DO 350  L = 2, LDEF - 1
               AIND = IWORK(MAPA+L)
               IF ( S(AIND).NE.SINV ) THEN
                  CALL DROT( J+1-IFRSTM, A(IFRSTM,J-1,AIND), 1,
     $                       A(IFRSTM,J,AIND), 1, CS, SN )
                  TEMP = A(J-1,J-1,AIND)
                  CALL DLARTG( TEMP, A(J,J-1,AIND), CS, SN,
     $                         A(J-1,J-1,AIND) )
                  A(J,J-1,AIND) = ZERO
                  CALL DROT( ILASTM-J+1, A(J-1,J,AIND), LDA1,
     $                       A(J,J,AIND), LDA1, CS, SN )
               ELSE
                  CALL DROT( ILASTM-J+2, A(J-1,J-1,AIND), LDA1,
     $                       A(J,J-1,AIND), LDA1, CS, SN )
                  TEMP = A(J,J,AIND)
                  CALL DLARTG( TEMP, -A(J,J-1,AIND), CS, SN,
     $                         A(J,J,AIND) )
                  A(J,J-1,AIND) = ZERO
                  CALL DROT( J-IFRSTM, A(IFRSTM,J-1,AIND), 1,
     $                       A(IFRSTM,J,AIND), 1, CS, SN )
               END IF
               LM = L + 1
               IF ( LCMPQ ) THEN
                  QI = IWORK(MAPQ+LM)
               ELSE IF ( LPARQ ) THEN
                  QI = ABS( QIND(IWORK(MAPQ+LM)) )
               END IF
               IF ( QI.NE.0 )
     $            CALL DROT( N, Q(1,J-1,QI), 1, Q(1,J,QI), 1, CS, SN )
  350       CONTINUE
            AIND = IWORK(MAPA+LDEF)
            CALL DROT( J+1-IFRSTM, A(IFRSTM,J-1,AIND), 1,
     $                 A(IFRSTM,J,AIND), 1, CS, SN )
         ELSE
C
C           Chase the zero upwards to the first position.
C
            DO 370  J1 = JDEF, JLO + 1, -1
               J = J1
               AIND = IWORK(MAPA+LDEF)
               TEMP = A(J-1,J,AIND)
               CALL DLARTG( TEMP, -A(J-1,J-1,AIND), CS, SN,
     $                      A(J-1,J,AIND) )
               A(J-1,J-1,AIND) = ZERO
               CALL DROT( J-IFRSTM-1, A(IFRSTM,J-1,AIND), 1,
     $                    A(IFRSTM,J,AIND), 1, CS, SN )
               IF ( LCMPQ ) THEN
                  QI = IWORK(MAPQ+LDEF)
               ELSE IF ( LPARQ ) THEN
                  QI = ABS( QIND(IWORK(MAPQ+LDEF)) )
               END IF
               IF ( QI.NE.0 )
     $            CALL DROT( N, Q(1,J-1,QI), 1, Q(1,J,QI), 1, CS, SN )
               LM = LDEF - 1
               DO 360  L = 1, K - 1
                  AIND = IWORK(MAPA+LM)
                  IF ( LM.EQ.1 ) THEN
                     CALL DROT( J-IFRSTM+2, A(IFRSTM,J-1,AIND), 1,
     $                          A(IFRSTM,J,AIND), 1, CS, SN )
                     TEMP = A(J,J-1,AIND)
                     CALL DLARTG( TEMP, A(J+1,J-1,AIND), CS, SN,
     $                            A(J,J-1,AIND) )
                     A(J+1,J-1,AIND) = ZERO
                     CALL DROT( ILASTM-J+1, A(J,J,AIND), LDA1,
     $                          A(J+1,J,AIND), LDA1, CS, SN )
                     J = J + 1
                  ELSE IF ( S(AIND).NE.SINV ) THEN
                     CALL DROT( ILASTM-J+2, A(J-1,J-1,AIND), LDA1,
     $                          A(J,J-1,AIND), LDA1, CS, SN )
                     TEMP = A(J,J,AIND)
                     CALL DLARTG( TEMP, -A(J,J-1,AIND), CS, SN,
     $                            A(J,J,AIND) )
                     A(J,J-1,AIND) = ZERO
                     CALL DROT( J-IFRSTM, A(IFRSTM,J-1,AIND), 1,
     $                          A(IFRSTM,J,AIND), 1, CS, SN )
                  ELSE
                     CALL DROT( J-IFRSTM+1, A(IFRSTM,J-1,AIND), 1,
     $                          A(IFRSTM,J,AIND), 1, CS, SN )
                     TEMP = A(J-1,J-1,AIND)
                     CALL DLARTG( TEMP, A(J,J-1,AIND), CS, SN,
     $                            A(J-1,J-1,AIND) )
                     A(J,J-1,AIND) = ZERO
                     CALL DROT( ILASTM-J+1, A(J-1,J,AIND), LDA1,
     $                          A(J,J,AIND), LDA1, CS, SN )
                  END IF
                  IF ( LCMPQ ) THEN
                     QI = IWORK(MAPQ+LM)
                  ELSE IF ( LPARQ ) THEN
                     QI = ABS( QIND(IWORK(MAPQ+LM)) )
                  END IF
                  IF ( QI.NE.0 )
     $               CALL DROT( N, Q(1,J-1,QI), 1, Q(1,J,QI), 1, CS,
     $                          SN )
                  LM = LM - 1
                  IF ( LM.LE.0 )
     $               LM = K
  360          CONTINUE
               AIND = IWORK(MAPA+LDEF)
               CALL DROT( ILASTM-J+1, A(J-1,J,AIND), LDA1, A(J,J,AIND),
     $                    LDA1, CS, SN )
  370       CONTINUE
C
C           Deflate the first element in the Hessenberg matrix.
C
            AIND = IWORK(MAPA+1)
            J = JLO
            TEMP = A(J,J,AIND)
            CALL DLARTG( TEMP, A(J+1,J,AIND), CS, SN, A(J,J,AIND) )
            A(J+1,J,AIND) = ZERO
            CALL DROT( ILASTM-J, A(J,J+1,AIND), LDA1, A(J+1,J+1,AIND),
     $                 LDA1, CS, SN )
            IF ( LCMPQ ) THEN
               QI = IWORK(MAPQ+1)
            ELSE IF ( LPARQ ) THEN
               QI = ABS( QIND(IWORK(MAPQ+1)) )
            END IF
            IF ( QI.NE.0 )
     $         CALL DROT( N, Q(1,J,QI), 1, Q(1,J+1,QI), 1, CS, SN )
            DO 380  L = K, LDEF + 1, -1
               AIND = IWORK(MAPA+L)
               IF ( S(AIND).EQ.SINV ) THEN
                  CALL DROT( J+2-IFRSTM, A(IFRSTM,J,AIND), 1,
     $                       A(IFRSTM,J+1,AIND), 1, CS, SN )
                  TEMP = A(J,J,AIND)
                  CALL DLARTG( TEMP, A(J+1,J,AIND), CS, SN,
     $                         A(J,J,AIND) )
                  A(J+1,J,AIND) = ZERO
                  CALL DROT( ILASTM-J, A(J,J+1,AIND), LDA1,
     $                       A(J+1,J+1,AIND), LDA1, CS, SN )
               ELSE
                  CALL DROT( ILASTM-J+1, A(J,J,AIND), LDA1,
     $                       A(J+1,J,AIND), LDA1, CS, SN )
                  TEMP = A(J+1,J+1,AIND)
                  CALL DLARTG( TEMP, -A(J+1,J,AIND), CS, SN,
     $                         A(J+1,J+1,AIND) )
                  A(J+1,J,AIND) = ZERO
                  CALL DROT( J+1-IFRSTM, A(IFRSTM,J,AIND), 1,
     $                       A(IFRSTM,J+1,AIND), 1, CS, SN )
               END IF
               IF ( LCMPQ ) THEN
                  QI = IWORK(MAPQ+L)
               ELSE IF ( LPARQ ) THEN
                  QI = ABS( QIND(IWORK(MAPQ+L)) )
               END IF
               IF ( QI.NE.0 )
     $            CALL DROT( N, Q(1,J,QI), 1, Q(1,J+1,QI), 1, CS, SN )
  380       CONTINUE
            AIND = IWORK(MAPA+LDEF)
            CALL DROT( ILASTM-J, A(J,J+1,AIND), LDA1, A(J+1,J+1,AIND),
     $                 LDA1, CS, SN )
         END IF
C
C        No QZ iteration.
C
         GO TO 530
C
C        Special case: A 1x1 block splits off at the bottom.
C
  390    CONTINUE
         CALL MA01BD( BASE, LGBAS, K, S, A(ILAST,ILAST,1), LDA1*LDA2,
     $                ALPHAR(ILAST), BETA(ILAST), SCAL(ILAST) )
         ALPHAI(ILAST) = ZERO
C
C        Check for possible loss of accuracy.
C
         IF ( BETA(ILAST).NE.ZERO ) THEN
            DO 400  L = 1, K
               AIND = IWORK(MAPA+L)
               TEMP = A(ILAST,ILAST,AIND)
               IF ( TEMP.NE.ZERO ) THEN
                  IF ( ABS( TEMP ).LT.DWORK(L)*TOLL ) THEN
                     IWARN = N + 1
                     IWORK(2*K+ILAST) = ILAST
                     GO TO 410
                  END IF
               END IF
  400       CONTINUE
         END IF
C
C        Go to next block - exit if finished.
C
  410    CONTINUE
         ILAST = ILAST - 1
         IF ( ILAST.LT.ILO )
     $      GO TO 550
C
C        Reset iteration counters.
C
         IITER  = 0
         TITER  = 0
         COUNT  = 0
         COUNTE = 0
         IF ( ZITER.NE.-1 )
     $      ZITER = 0
         IF ( .NOT.LSCHR ) THEN
            ILASTM = ILAST
            IF ( IFRSTM.GT.ILAST )
     $         IFRSTM = ILO
         END IF
C
C        No QZ iteration.
C
         GO TO 530
C
C        **************************************************************
C        *                      PERIODIC QZ STEP                      *
C        **************************************************************
C
C        It is assumed that IFIRST < ILAST.
C
  420    CONTINUE
C
         IITER = IITER + 1
         ZITER = ZITER + 1
         IF( .NOT.LSCHR )
     $      IFRSTM = IFIRST
         IF ( IFIRST+1.EQ.ILAST ) THEN
C
C           Special case -- 2x2 block.
C
            J = ILAST - 1
            IF ( TITER.LT.2 ) THEN
               TITER = TITER + 1
C
C              Try to deflate the 2-by-2 problem.
C
               PDW = PFREE + 1
               DO 430  L = 1, K
                  DWORK(PDW  ) = A(J,J,L)
                  DWORK(PDW+1) = A(J+1,J,L)
                  DWORK(PDW+2) = A(J,J+1,L)
                  DWORK(PDW+3) = A(J+1,J+1,L)
                  PDW = PDW + 4
  430          CONTINUE
               IF ( SINV.LT.0 ) THEN
                  I = IWORK(MAPQ+1)
                  IWORK(MAPQ+1) = IWORK(MAPA+1)
               END IF
               CALL MB03BF( K, IWORK(MAPH), S, SINV, DWORK(PFREE+1),
     $                      2, 2, ULP )
               IF ( SINV.LT.0 )
     $            IWORK(MAPQ+1) = I
               I = PFREE + 4*( H - 1 )
               IF ( ABS( DWORK(I+2) ).LT.
     $              ULP*( MAX( ABS( DWORK(I+1) ), ABS( DWORK(I+3) ),
     $                         ABS( DWORK(I+4) ) ) ) ) THEN
C
C                 Construct a perfect shift polynomial. This may fail,
C                 so we try it twice (indicated by TITER).
C
                  CS1 = ONE
                  SN1 = ONE
                  DO 440  L = K, 2, -1
                     AIND = IWORK(MAPA+L)
                     TEMP = DWORK(PFREE+AIND*4)
                     IF ( S(AIND).EQ.SINV ) THEN
                        CALL DLARTG( CS1*A(J,J,AIND), SN1*TEMP, CS1,
     $                               SN1, TEMP )
                     ELSE
                        CALL DLARTG( CS1*TEMP, SN1*A(J,J,AIND), CS1,
     $                               SN1, TEMP )
                     END IF
  440             CONTINUE
                  AIND = IWORK(MAPA+1)
                  TEMP = DWORK(PFREE+AIND*4)
                  CALL DLARTG( A(J,J,AIND)*CS1-TEMP*SN1,
     $                         A(J+1,J,AIND)*CS1, CS1, SN1, TEMP )
                  GO TO 510
               END IF
            END IF
C
C           Looks like a complex block.
C           1. Compute the product SVD of the triangular matrices
C             (optionally).
C
            IF ( LSVD ) THEN
               CALL MB03BC( K, IWORK(MAPA+1), S, SINV, A(J,J,1), LDA1,
     $                      LDA2, MACPAR, DWORK(PFREE+1),
     $                      DWORK(PFREE+K+1), DWORK(PFREE+2*K+1) )
C
C              Update factors and transformations.
C
               AIND = IWORK(MAPA+1)
               CS2  = DWORK(PFREE+1)
               SN2  = DWORK(PFREE+K+1)
               CALL DROT( ILASTM-IFRSTM+1, A(IFRSTM,J,AIND), 1,
     $                    A(IFRSTM,J+1,AIND), 1, CS2, SN2 )
               DO 450  L = 2, K
                  AIND = IWORK(MAPA+L)
                  IF ( LCMPQ ) THEN
                     QI = IWORK(MAPQ+L)
                  ELSE IF ( LPARQ ) THEN
                     QI = ABS( QIND(IWORK(MAPQ+L)) )
                  END IF
                  IF ( QI.NE.0 )
     $               CALL DROT( N, Q(1,J,QI), 1, Q(1,J+1,QI), 1, CS2,
     $                          SN2 )
                  CS1 = CS2
                  SN1 = SN2
                  CS2 = DWORK(PFREE+L)
                  SN2 = DWORK(PFREE+K+L)
                  IF (S(AIND).EQ.SINV) THEN
                     CALL DROT( ILASTM-J-1, A(J,J+2,AIND), LDA1,
     $                          A(J+1,J+2,AIND), LDA1, CS1, SN1 )
                     CALL DROT( J-IFRSTM, A(IFRSTM,J,AIND), 1,
     $                          A(IFRSTM,J+1,AIND), 1, CS2, SN2 )
                  ELSE
                     CALL DROT( ILASTM-J-1, A(J,J+2,AIND), LDA1,
     $                          A(J+1,J+2,AIND), LDA1, CS2, SN2 )
                     CALL DROT( J-IFRSTM, A(IFRSTM,J,AIND), 1,
     $                          A(IFRSTM,J+1,AIND), 1, CS1, SN1 )
                  END IF
  450          CONTINUE
               IF ( LCMPQ ) THEN
                  QI = IWORK(MAPQ+1)
               ELSE IF ( LPARQ ) THEN
                  QI = ABS( QIND(IWORK(MAPQ+1)) )
               END IF
               IF ( QI.NE.0 )
     $            CALL DROT( N, Q(1,J,QI), 1, Q(1,J+1,QI), 1, CS2, SN2 )
               AIND = IWORK(MAPA+1)
               CALL DROT( ILASTM-J+1, A(J,J,AIND), LDA1,
     $                    A(J+1,J,AIND), LDA1, CS2, SN2 )
            END IF
C
C           2. Compute complex eigenvalues.
C
            CALL MB03BB( BASE, LGBAS, ULP, K, IWORK(MAPA+1), S, SINV,
     $                   A(J,J,1), LDA1, LDA2, ALPHAR(J), ALPHAI(J),
     $                   BETA(J), SCAL(J), DWORK(PFREE+1), IERR )
            IF ( IERR.EQ.1 ) THEN
C
C              The single shift periodic QZ did not converge, set
C              IWARN = J to indicate that the eigenvalues are not
C              assigned.
C
               IWARN = MAX( J, IWARN )
            ELSE IF ( IERR.EQ.2 ) THEN
C
C              Some computed eigenvalues might be inaccurate.
C
               IF ( IWARN.EQ.0 )
     $            IWARN = N
            END IF
C
C           Check for real eigenvalues and possible loss of accuracy.
C           Also, set zero or infinite eigenvalues where appropriate.
C
            DO 460  L = 1, K
               AIND = IWORK(MAPA+L)
               IF ( ALPHAI(J).EQ.ZERO .AND. BETA(J).NE.ZERO ) THEN
                  IF ( ABS( A(J,J,AIND) ).LT.DWORK(L)*TOLL ) THEN
                     IWARN = N + 1
                     IWORK(2*K+J) = -J
                     GO TO 470
                  END IF
               ELSE
                  A1  = A(J,J,AIND)
                  A3  = A(J,J+1,AIND)
                  A4  = A(J+1,J+1,AIND)
                  NRM = DLAPY3( A1, A3, A4 )
                  IF ( L.EQ.IWORK(MAPA+1) ) THEN
                     A2  = A(J+1,J,L)
                     NRM = DLAPY2( NRM, A2 )
                  END IF
                  SDET = ( MAX( ABS( A1 ), ABS( A4 ) )/NRM )
     $                    *MIN( ABS( A1 ), ABS( A4 ) )*
     $                    SIGN( ONE, A1 )*SIGN( ONE, A4 )
                  IF ( L.EQ.IWORK(MAPA+1) )
     $               SDET = SDET - ( MAX( ABS( A2 ), ABS( A3 ) )/NRM )
     $                              *MIN( ABS( A2 ), ABS( A3 ) )*
     $                              SIGN( ONE, A2 )*SIGN( ONE, A3 )
                  IF ( ABS( SDET ).LT.DWORK(L)*TOLL ) THEN
C
C                    Make a more accurate singularity test using SVD.
C
                     IF ( L.EQ.IWORK(MAPA+1) ) THEN
                        IF ( ABS( A1 ).GE.ABS( A4 ) ) THEN
                           CALL DLARTG( A1, A2, CS, SN, TEMP )
                           A1   = TEMP
                           TEMP = CS*A3 + SN*A4
                           A4   = CS*A4 - SN*A3
                           A3   = TEMP
                        ELSE
                           CALL DLARTG( A4, A2, CS, SN, TEMP )
                           A4   = TEMP
                           TEMP = CS*A3 + SN*A1
                           A1   = CS*A1 - SN*A3
                           A3   = TEMP
                        END IF
                     END IF
                     CALL DLAS2( A1, A3, A4, SVMN, TEMP )
                     IF ( SVMN.LT.DWORK(L)*TOLL ) THEN
                        IWARN = N + 1
                        IWORK(2*K+J) = -J
                        GO TO 470
                     END IF
                  END IF
               END IF
  460       CONTINUE
C
C           Go to next block and reset counters.
C
  470       CONTINUE
            ILAST = IFIRST - 1
            IF ( ILAST.LT.ILO )
     $         GO TO 550
            IITER  = 0
            TITER  = 0
            COUNT  = 0
            COUNTE = 0
            IF ( ZITER.NE.-1 )
     $         ZITER = 0
            IF ( .NOT.LSCHR ) THEN
               ILASTM = ILAST
               IF ( IFRSTM.GT.ILAST )
     $           IFRSTM = ILO
            END IF
            GO TO 530
         END IF
C
C        Now, it is assumed that ILAST-IFIRST+1 >= 3.
C
         IF ( COUNT.LT.NITER ) THEN
C
C           Use the normal periodic QZ step routine.
C           Note that the pointer to IWORK is increased by 1.
C           The fact that, for SINV = 1, IWORK(MAPQ+1) = IWORK(MAPA+1)
C           is used.
C
            COUNT = COUNT + 1
            IF ( SINV.LT.0 ) THEN
               I = IWORK(MAPQ+1)
               IWORK(MAPQ+1) = IWORK(MAPA+1)
            END IF
            CALL MB03AF( 'Double', K, ILAST-IFIRST+1, IWORK(MAPH), S,
     $                   SINV, A(IFIRST,IFIRST,1), LDA1, LDA2, CS1,
     $                   SN1, CS2, SN2 )
            IF ( SINV.LT.0 )
     $         IWORK(MAPQ+1) = I
         ELSE IF ( COUNTE.LT.MCOUNT ) THEN
C
C           Compute the two trailing eigenvalues for finding the shifts.
C           Deal with special case of infinite eigenvalues, if needed.
C
            I = ILAST - 1
            IF ( SINV.LT.0 ) THEN
               AIND = IWORK(MAPA+1)
               A1   = A(I,I,AIND)
               A2   = A(I+1,I,AIND)
               A3   = A(I,I+1,AIND)
               A4   = A(I+1,I+1,AIND)
               NRM  = DLANHS( 'Frobenius', 2, A(ILO,ILO,AIND), LDA1,
     $                        DWORK )
               SDET = (  MAX( ABS( A1 ), ABS( A4 ) )/NRM )
     $                  *MIN( ABS( A1 ), ABS( A4 ) )*
     $                  SIGN( ONE, A1 )*SIGN( ONE, A4 ) -
     $                (  MAX( ABS( A2 ), ABS( A3 ) )/NRM )
     $                  *MIN( ABS( A2 ), ABS( A3 ) )*
     $                  SIGN( ONE, A2 )*SIGN( ONE, A3 )
               ISINF = ABS( SDET ).LT.DWORK(AIND)*TOLL
               IF ( ISINF ) THEN
                  ALPHAR(I)     = ONE/DWORK(PNORM+1)
                  ALPHAR(ILAST) = ONE/DWORK(PNORM+1)
                  SCAL(I)       = 1
                  SCAL(ILAST)   = 1
               END IF
               IERR = 0
            ELSE
               ISINF = .FALSE.
            END IF
            IF ( .NOT.ISINF ) THEN
               CALL MB03BB( BASE, LGBAS, ULP, K, IWORK(MAPA+1), S, SINV,
     $                      A(I,I,1), LDA1, LDA2, ALPHAR(I), ALPHAI(I),
     $                      BETA(I), SCAL(I), DWORK(PFREE+1), IERR )
               IF ( SINV.LT.0 ) THEN
C
C                 Use the reciprocals of the eigenvalues returned above.
C
                  IF ( ALPHAI(I).EQ.ZERO ) THEN
                     ALPHAR(I)     = SIGN( ONE, ALPHAR(I) )/
     $                               MAX( SAFMIN, ABS( ALPHAR(I) ) )
                     ALPHAR(ILAST) = SIGN( ONE, ALPHAR(ILAST) )/
     $                               MAX( SAFMIN, ABS( ALPHAR(ILAST) ) )
                     SCAL(I)       = -SCAL(I)
                     SCAL(ILAST)   = -SCAL(ILAST)
                  ELSE
                     CALL DLADIV( ONE, ZERO, ALPHAR(ILAST),
     $                            -ALPHAI(ILAST), ALPHAR(I), ALPHAI(I) )
                     SCAL(I) = -SCAL(I)
                  END IF
               END IF
C
               IF ( IERR.NE.0 ) THEN
C
C                 Try an exceptional transformation if MB03BB does not
C                 converge on some special cases.
C
                  TEMP2 = BASE**SCAL(I)
                  IF ( ALPHAI(I).NE.ZERO ) THEN
                     TEMP = ( ABS( ALPHAR(I) ) + ABS( ALPHAI(I) ) )*
     $                      TEMP2
                  ELSE
                     TEMP = MAX( ABS( ALPHAR(ILAST) )*BASE**SCAL(ILAST),
     $                           ABS( ALPHAR(I) )*TEMP2 )
                  END IF
                  IF ( TEMP.LE.SQRT( ULP )*DWORK(PNORM+1) ) THEN
                     ALPHAR(I) = DWORK(PNORM+1)
                     SCAL(I)   = 1
                     ALPHAR(ILAST) = DWORK(PNORM+1)
                     SCAL(ILAST)   = 1
                     IERR = 0
                  END IF
               END IF
            END IF
C
            IF ( IERR.NE.0 ) THEN
C
C              Use the normal periodic QZ step routine.
C
               IERR = 0
               IN   = ILAST - IFIRST + 1
               IF ( SINV.LT.0 ) THEN
                  J1 = IWORK(MAPQ+1)
                  IWORK(MAPQ+1) = IWORK(MAPA+1)
               END IF
               CALL MB03AF( 'Double', K, IN, IWORK(MAPH), S, SINV, 
     $                      A(IFIRST,IFIRST,1), LDA1, LDA2, CS1, SN1,
     $                      CS2, SN2 )
               IF ( SINV.LT.0 )
     $            IWORK(MAPQ+1) = J1
               COUNT  = 0
               COUNTE = 0
            ELSE
C
C              Use explict shifts.
C
               COUNTE = COUNTE + 1
               W1     = ALPHAR(I)*BASE**SCAL(I)
C
               IF ( ALPHAI(I).NE.ZERO ) THEN
C
C                 Use complex conjugate shifts.
C
                  SHFT = 'C'
                  W2   = ALPHAI(I)*BASE**SCAL(I)
C
               ELSE
C
C                 Two identical real shifts are tried first. If there is
C                 no convergence after MCOUNT/2 consecutive iterations,
C                 a single shift is applied. The eigenvalue closer to
C                 the last element of the current product is used.
C
                  W2 = ALPHAR(ILAST)*BASE**SCAL(ILAST)
C
                  CALL MA01BD( BASE, LGBAS, K, S, A(ILAST,ILAST,1),
     $                         LDA1*LDA2, TEMP, TEMP2, I )
                  TEMP = TEMP*BASE**I
                  A1   = ABS( TEMP - W1 )
                  A2   = ABS( TEMP - W2 )
C
                  IF ( COUNTE.LE.MAX( 1, MCOUNT/2 ) ) THEN
                     SHFT = 'D'
                     IF ( A1.LT.A2 ) THEN
                        W2 = W1
                     ELSE
                        W1 = W2
                     END IF
                  ELSE
                     SHFT = 'S'
                     IF ( A1.LT.A2 )
     $                  W2 = W1
                  END IF
C
               END IF
C
C              Compute an initial transformation using the selected
C              shifts.
C
               CALL MB03AB( SHFT, K, ILAST-IFIRST+1, IWORK(MAPA+1), S,
     $                      SINV, A(IFIRST,IFIRST,1), LDA1, LDA2, W1,
     $                      W2, CS1, SN1, CS2, SN2 )
            END IF
C
            IF ( COUNT+COUNTE.GE.NITER+MCOUNT ) THEN
C
C              Reset the two counters.
C
               COUNT  = 0
               COUNTE = 0
            END IF
         END IF
C
C        Do the sweeps.
C
         IF ( K.GT.1 ) THEN
C
C           The propagation of the initial transformation is processed
C           here separately.
C
            IN   = IFIRST + 1
            IO   = ILAST  - 2
            J    = IFIRST
            AIND = IWORK(MAPA+1)
            CALL DROT( ILAST-IFRSTM+1, A(IFRSTM,J+1,AIND), 1,
     $                 A(IFRSTM,J+2,AIND), 1, CS2, SN2 )
            CALL DROT( ILAST-IFRSTM+1, A(IFRSTM,J,AIND), 1,
     $                 A(IFRSTM,J+1,AIND), 1, CS1, SN1 )
            IF ( LCMPQ ) THEN
               QI = IWORK(MAPQ+2)
            ELSE IF ( LPARQ ) THEN
               QI = ABS( QIND(IWORK(MAPQ+2)) )
            END IF
            IF ( QI.NE.0 ) THEN
               CALL DROT( N, Q(1,J+1,QI), 1, Q(1,J+2,QI), 1, CS2, SN2 )
               CALL DROT( N, Q(1,J,QI), 1, Q(1,J+1,QI), 1, CS1, SN1 )
            END IF
C
C           Propagate information from the right to A_k.
C
            DO 480 L = 2, K
               AIND = IWORK(MAPA+L)
               IF ( S(AIND).EQ.SINV ) THEN
                  CALL DROT( ILASTM-J+1, A(J+1,J,AIND), LDA1,
     $                       A(J+2,J,AIND), LDA1, CS2, SN2 )
                  TEMP = A(J+2,J+2,AIND)
                  CALL DLARTG( TEMP, -A(J+2,J+1,AIND), CS2, SN2,
     $                         A(J+2,J+2,AIND) )
                  A(J+2,J+1,AIND) = ZERO
                  CALL DROT( J-IFRSTM+2, A(IFRSTM,J+1,AIND), 1,
     $                       A(IFRSTM,J+2,AIND), 1, CS2, SN2 )
C
                  CALL DROT( ILASTM-J+1, A(J,J,AIND), LDA1,
     $                       A(J+1,J,AIND), LDA1, CS1, SN1 )
                  TEMP = A(J+1,J+1,AIND)
                  CALL DLARTG( TEMP, -A(J+1,J,AIND), CS1, SN1,
     $                         A(J+1,J+1,AIND) )
                  A(J+1,J,AIND) = ZERO
                  CALL DROT( J-IFRSTM+1, A(IFRSTM,J,AIND), 1,
     $                       A(IFRSTM,J+1,AIND), 1, CS1, SN1 )
C
               ELSE
C
                  CALL DROT( J+3-IFRSTM, A(IFRSTM,J+1,AIND), 1,
     $                       A(IFRSTM,J+2,AIND), 1, CS2, SN2 )
                  TEMP = A(J+1,J+1,AIND)
                  CALL DLARTG( TEMP, A(J+2,J+1,AIND), CS2, SN2,
     $                         A(J+1,J+1,AIND) )
                  A(J+2,J+1,AIND) = ZERO
                  CALL DROT( ILASTM-J-1, A(J+1,J+2,AIND), LDA1,
     $                       A(J+2,J+2,AIND), LDA1, CS2, SN2 )
C
                  CALL DROT( J+2-IFRSTM, A(IFRSTM,J,AIND), 1,
     $                       A(IFRSTM,J+1,AIND), 1, CS1, SN1 )
                  TEMP = A(J,J,AIND)
                  CALL DLARTG( TEMP, A(J+1,J,AIND), CS1, SN1,
     $                         A(J,J,AIND) )
                  A(J+1,J,AIND) = ZERO
                  CALL DROT( ILASTM-J, A(J,J+1,AIND), LDA1,
     $                       A(J+1,J+1,AIND), LDA1, CS1, SN1 )
               END IF
               IF ( LCMPQ ) THEN
                  QI = IWORK(MAPQ+MOD(L,K)+1)
               ELSE IF ( LPARQ ) THEN
                  QI = ABS( QIND(IWORK(MAPQ+MOD(L,K)+1)) )
               END IF
               IF ( QI.NE.0 ) THEN
                  CALL DROT( N, Q(1,J+1,QI), 1, Q(1,J+2,QI), 1, CS2,
     $                       SN2 )
                  CALL DROT( N, Q(1,J,QI), 1, Q(1,J+1,QI), 1, CS1, SN1 )
               END IF
  480       CONTINUE
C
            AIND = IWORK(MAPA+1)
            CALL DROT( ILASTM-IFIRST+1, A(J+1,IFIRST,AIND), LDA1,
     $                 A(J+2,IFIRST,AIND), LDA1, CS2, SN2 )
            CALL DROT( ILASTM-IFIRST+1, A(J,IFIRST,AIND), LDA1,
     $                 A(J+1,IFIRST,AIND), LDA1, CS1, SN1 )
         ELSE
            IN = IFIRST - 1
            IO = ILAST  - 3
         END IF
C
         DO 500  J1 = IN, IO
            AIND = IWORK(MAPA+1)
            IF ( LCMPQ ) THEN
               QI = IWORK(MAPQ+1)
            ELSE IF ( LPARQ ) THEN
               QI = ABS( QIND(IWORK(MAPQ+1)) )
            END IF
C
C           Create a bulge if J1 = IFIRST - 1, otherwise chase the
C           bulge.
C
            IF ( J1.LT.IFIRST ) THEN
               J = J1 + 1
               CALL DROT( ILASTM-J+1, A(J+1,J,AIND), LDA1,
     $                    A(J+2,J,AIND), LDA1, CS2, SN2 )
               CALL DROT( ILASTM-J+1, A(J,J,AIND), LDA1,
     $                    A(J+1,J,AIND), LDA1, CS1, SN1 )
            ELSE
               IF ( K.EQ.1 ) THEN
                  J = J + 1
               ELSE
                  J = J1
               END IF
               TEMP = A(J+1,J-1,AIND)
               CALL DLARTG( TEMP, A(J+2,J-1,AIND), CS2, SN2,
     $                      TEMP2 )
               TEMP = A(J,J-1,AIND)
               CALL DLARTG( TEMP, TEMP2, CS1, SN1, A(J,J-1,AIND) )
               A(J+1,J-1,AIND) = ZERO
               A(J+2,J-1,AIND) = ZERO
               CALL DROT( ILASTM-J+1, A(J+1,J,AIND), LDA1,
     $                    A(J+2,J,AIND), LDA1, CS2, SN2 )
               CALL DROT( ILASTM-J+1, A(J,J,AIND), LDA1,
     $                    A(J+1,J,AIND), LDA1, CS1, SN1 )
            END IF
            IF ( QI.NE.0 ) THEN
               CALL DROT( N, Q(1,J+1,QI), 1, Q(1,J+2,QI), 1, CS2, SN2 )
               CALL DROT( N, Q(1,J,  QI), 1, Q(1,J+1,QI), 1, CS1, SN1 )
            END IF
C
C           Propagate information from the right to A_1.
C
            DO 490  L = K, 2, -1
               AIND = IWORK(MAPA+L)
               IF ( S(AIND).EQ.SINV ) THEN
                  CALL DROT( J+3-IFRSTM, A(IFRSTM,J+1,AIND), 1,
     $                       A(IFRSTM,J+2,AIND), 1, CS2, SN2 )
                  TEMP = A(J+1,J+1,AIND)
                  CALL DLARTG( TEMP, A(J+2,J+1,AIND), CS2, SN2,
     $                         A(J+1,J+1,AIND) )
                  A(J+2,J+1,AIND) = ZERO
                  CALL DROT( ILASTM-J-1, A(J+1,J+2,AIND), LDA1,
     $                       A(J+2,J+2,AIND), LDA1, CS2, SN2 )
                  CALL DROT( J+2-IFRSTM, A(IFRSTM,J,AIND), 1,
     $                       A(IFRSTM,J+1,AIND), 1, CS1, SN1 )
                  TEMP = A(J,J,AIND)
                  CALL DLARTG( TEMP, A(J+1,J,AIND), CS1, SN1,
     $                         A(J,J,AIND) )
                  A(J+1,J,AIND) = ZERO
                  CALL DROT( ILASTM-J, A(J,J+1,AIND), LDA1,
     $                       A(J+1,J+1,AIND), LDA1, CS1, SN1 )
               ELSE
                  CALL DROT( ILASTM-J+1, A(J+1,J,AIND), LDA1,
     $                       A(J+2,J,AIND), LDA1, CS2, SN2 )
                  TEMP = A(J+2,J+2,AIND)
                  CALL DLARTG( TEMP, -A(J+2,J+1,AIND), CS2, SN2,
     $                         A(J+2,J+2,AIND) )
                  A(J+2,J+1,AIND) = ZERO
                  CALL DROT( J+2-IFRSTM, A(IFRSTM,J+1,AIND), 1,
     $                       A(IFRSTM,J+2,AIND), 1, CS2, SN2 )
                  CALL DROT( ILASTM-J+1, A(J,J,AIND), LDA1,
     $                       A(J+1,J,AIND), LDA1, CS1, SN1 )
                  TEMP = A(J+1,J+1,AIND)
                  CALL DLARTG( TEMP, -A(J+1,J,AIND), CS1, SN1,
     $                         A(J+1,J+1,AIND) )
                  A(J+1,J,AIND) = ZERO
                  CALL DROT( J+1-IFRSTM, A(IFRSTM,J,AIND), 1,
     $                       A(IFRSTM,J+1,AIND), 1, CS1, SN1 )
               END IF
               IF ( LCMPQ ) THEN
                  QI = IWORK(MAPQ+L)
               ELSE IF ( LPARQ ) THEN
                  QI = ABS( QIND(IWORK(MAPQ+L)) )
               END IF
               IF ( QI.NE.0 ) THEN
                  CALL DROT( N, Q(1,J+1,QI), 1, Q(1,J+2,QI), 1, CS2,
     $                       SN2 )
                  CALL DROT( N, Q(1,J,QI), 1, Q(1,J+1,QI), 1, CS1, SN1 )
               END IF
  490       CONTINUE
            AIND = IWORK(MAPA+1)
            LM = MIN( J+3, ILASTM ) - IFRSTM + 1
            CALL DROT( LM, A(IFRSTM,J+1,AIND), 1,
     $                 A(IFRSTM,J+2,AIND), 1, CS2, SN2 )
            CALL DROT( LM, A(IFRSTM,J,AIND), 1,
     $                 A(IFRSTM,J+1,AIND), 1, CS1, SN1 )
  500    CONTINUE
C
C        To avoid IF statements, there is an extra piece of code for
C        the last step.
C
         J = ILAST - 1
         TEMP = A(J,J-1,AIND)
         CALL DLARTG( TEMP, A(J+1,J-1,AIND), CS1, SN1, A(J,J-1,AIND) )
         A(J+1,J-1,AIND) = ZERO
C
  510    CONTINUE
C
         CALL DROT( ILASTM-J+1, A(J,J,AIND), LDA1,
     $              A(J+1,J,AIND), LDA1, CS1, SN1 )
         IF ( LCMPQ ) THEN
            QI = IWORK(MAPQ+1)
         ELSE IF ( LPARQ ) THEN
            QI = ABS( QIND(IWORK(MAPQ+1)) )
         END IF
         IF ( QI.NE.0 )
     $      CALL DROT( N, Q(1,J,QI), 1, Q(1,J+1,QI), 1, CS1, SN1 )
C
C        Propagate information from the right to A_1.
C
         DO 520  L = K, 2, -1
            AIND = IWORK(MAPA+L)
            IF ( S(AIND).EQ.SINV ) THEN
               CALL DROT( J+2-IFRSTM, A(IFRSTM,J,AIND), 1,
     $                    A(IFRSTM,J+1,AIND), 1, CS1, SN1 )
               TEMP = A(J,J,AIND)
               CALL DLARTG( TEMP, A(J+1,J,AIND), CS1, SN1,
     $                      A(J,J,AIND) )
               A(J+1,J,AIND) = ZERO
               CALL DROT( ILASTM-J, A(J,J+1,AIND), LDA1,
     $                    A(J+1,J+1,AIND), LDA1, CS1, SN1 )
            ELSE
               CALL DROT( ILASTM-J+1, A(J,J,AIND), LDA1,
     $                    A(J+1,J,AIND), LDA1, CS1, SN1 )
               TEMP = A(J+1,J+1,AIND)
               CALL DLARTG( TEMP, -A(J+1,J,AIND), CS1, SN1,
     $                      A(J+1,J+1,AIND) )
               A(J+1,J,AIND) = ZERO
               CALL DROT( J+1-IFRSTM, A(IFRSTM,J,AIND), 1,
     $                    A(IFRSTM,J+1,AIND), 1, CS1, SN1 )
            END IF
            IF ( LCMPQ ) THEN
               QI = IWORK(MAPQ+L)
            ELSE IF ( LPARQ ) THEN
               QI = ABS( QIND(IWORK(MAPQ+L)) )
            END IF
            IF ( QI.NE.0 )
     $         CALL DROT( N, Q(1,J,QI), 1, Q(1,J+1,QI), 1, CS1, SN1 )
  520    CONTINUE
         AIND = IWORK(MAPA+1)
         CALL DROT( ILASTM-IFRSTM+1, A(IFRSTM,J,AIND), 1,
     $              A(IFRSTM,J+1,AIND), 1, CS1, SN1 )
C
C        End of iteration loop.
C
  530    CONTINUE
  540 CONTINUE
C
C     Drop through = non-convergence.
C
      INFO = ILAST
      GO TO 580
C
C     Successful completion of all QZ steps.
C
  550 CONTINUE
C
C     Set eigenvalues 1:ILO-1.
C
      DO 560  J = 1, ILO - 1
         CALL MA01BD( BASE, LGBAS, K, S, A(J,J,1), LDA1*LDA2, ALPHAR(J),
     $                BETA(J), SCAL(J) )
         ALPHAI(J) = ZERO
  560 CONTINUE
C
C     Store information about the splitted 2-by-2 blocks and possible
C     loss of accuracy.
C
      DO 570  I = 2, N + 1
         IWORK(I) = IWORK(2*K+I-1)
  570 CONTINUE
C
  580 CONTINUE
C
      DO 590  L = K + 1, 2, -1
         DWORK(PNORM+L) = DWORK(PNORM+L-1)
  590 CONTINUE
C
      DWORK(1) = DBLE( OPTDW )
      IWORK(1) = OPTIW
C
      RETURN
C *** Last line of MB03BD ***
      END
