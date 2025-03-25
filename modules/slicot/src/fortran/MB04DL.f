      SUBROUTINE MB04DL( JOB, N, THRESH, A, LDA, B, LDB, ILO, IHI,
     $                   LSCALE, RSCALE, DWORK, IWARN, INFO )
C
C     PURPOSE
C
C     To balance a pair of N-by-N real matrices (A,B). This involves,
C     first, permuting A and B by equivalence transformations to isolate
C     eigenvalues in the first 1 to ILO-1 and last IHI+1 to N elements
C     on the diagonal of A and B; and second, applying a diagonal
C     equivalence transformation to rows and columns ILO to IHI to make
C     the rows and columns as close in 1-norm as possible. Both steps
C     are optional. Balancing may reduce the 1-norms of the matrices,
C     and improve the accuracy of the computed eigenvalues and/or
C     eigenvectors in the generalized eigenvalue problem
C     A*x = lambda*B*x.
C
C     This routine may optionally improve the conditioning of the
C     scaling transformation compared to the LAPACK routine DGGBAL.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOB     CHARACTER*1
C             Specifies the operations to be performed on A and B:
C             = 'N':  none:  simply set ILO = 1, LSCALE(I) = 1.0 and
C                     RSCALE(I) = 1.0 for I = 1,...,N.
C             = 'P':  permute only;
C             = 'S':  scale only;
C             = 'B':  both permute and scale.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of matrices A and B.  N >= 0.
C
C     THRESH  (input) DOUBLE PRECISION
C             If JOB = 'S' or JOB = 'B', and THRESH >= 0, threshold
C             value for magnitude of the elements to be considered in
C             the scaling process: elements with magnitude less than or
C             equal to THRESH*MXNORM are ignored for scaling, where
C             MXNORM is the maximum of the 1-norms of the original
C             submatrices A(s,s) and B(s,s), with s = ILO:IHI.
C             If THRESH < 0, the subroutine finds the scaling factors
C             for which some conditions, detailed below, are fulfilled.
C             A sequence of increasing strictly positive threshold
C             values is used.
C             If THRESH = -1, the condition is that
C                max( norm(A(s,s),1)/norm(B(s,s),1),
C                     norm(B(s,s),1)/norm(S(s,s),1) )                (1)
C             has the smallest value, for the threshold values used,
C             where A(s,s) and B(s,s) are the scaled submatrices.
C             If THRESH = -2, the norm ratio reduction (1) is tried, but
C             the subroutine may return IWARN = 1 and reset the scaling
C             factors to 1, if this seems suitable. See the description
C             of the argument IWARN and FURTHER COMMENTS.
C             If THRESH = -3, the condition is that
C                norm(A(s,s),1)*norm(B(s,s),1)                       (2)
C             has the smallest value for the scaled submatrices.
C             If THRESH = -4, the norm reduction in (2) is tried, but
C             the subroutine may return IWARN = 1 and reset the scaling
C             factors to 1, as for THRESH = -2 above.
C             If THRESH = -VALUE, with VALUE >= 10, the condition
C             numbers of the left and right scaling transformations will
C             be bounded by VALUE, i.e., the ratios between the largest
C             and smallest entries in LSCALE(s) and RSCALE(s), will be
C             at most VALUE. VALUE should be a power of 10.
C             If JOB = 'N' or JOB = 'P', the value of THRESH is
C             irrelevant.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the matrix A.
C             On exit, the leading N-by-N part of this array contains
C             the balanced matrix A.
C             In particular, the strictly lower triangular part of the
C             first ILO-1 columns and the last N-IHI rows of A is zero.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     B       (input/output) DOUBLE PRECISION array, dimension (LDB, N)
C             On entry, the leading N-by-N part of this array must
C             contain the matrix B.
C             On exit, the leading N-by-N part of this array contains
C             the balanced matrix B.
C             In particular, the strictly lower triangular part of the
C             first ILO-1 columns and the last N-IHI rows of B is zero.
C             If JOB = 'N', the arrays A and B are not referenced.
C
C     LDB    INTEGER
C             The leading dimension of the array B.  LDB >= MAX(1, N).
C
C     ILO     (output) INTEGER
C     IHI     (output) INTEGER
C             ILO and IHI are set to integers such that on exit
C             A(i,j) = 0 and B(i,j) = 0 if i > j and
C             j = 1,...,ILO-1 or i = IHI+1,...,N.
C             If JOB = 'N' or 'S', ILO = 1 and IHI = N.
C
C     LSCALE  (output) DOUBLE PRECISION array, dimension (N)
C             Details of the permutations and scaling factors applied
C             to the left side of A and B.  If P(j) is the index of the
C             row interchanged with row j, and D(j) is the scaling
C             factor applied to row j, then
C               LSCALE(j) = P(j)    for j = 1,...,ILO-1
C                         = D(j)    for j = ILO,...,IHI
C                         = P(j)    for j = IHI+1,...,N.
C             The order in which the interchanges are made is N to
C             IHI+1, then 1 to ILO-1.
C
C     RSCALE  (output) DOUBLE PRECISION array, dimension (N)
C             Details of the permutations and scaling factors applied
C             to the right side of A and B.  If P(j) is the index of the
C             column interchanged with column j, and D(j) is the scaling
C             factor applied to column j, then
C               RSCALE(j) = P(j)    for j = 1,...,ILO-1
C                         = D(j)    for j = ILO,...,IHI
C                         = P(j)    for j = IHI+1,...,N.
C             The order in which the interchanges are made is N to
C             IHI+1, then 1 to ILO-1.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK) where
C             LDWORK = 0,   if  JOB = 'N' or JOB = 'P', or N = 0;
C             LDWORK = 6*N, if (JOB = 'S' or JOB = 'B') and THRESH >= 0;
C             LDWORK = 8*N, if (JOB = 'S' or JOB = 'B') and THRESH <  0.
C             On exit, if JOB = 'S' or JOB = 'B', DWORK(1) and DWORK(2)
C             contain the initial 1-norms of A(s,s) and B(s,s), and
C             DWORK(3) and DWORK(4) contain their final 1-norms,
C             respectively. Moreover, DWORK(5) contains the THRESH value
C             used (irrelevant if IWARN = 1 or ILO = IHI).
C
C     Warning Indicator
C
C     IWARN   INTEGER
C             = 0:  no warning;
C             = 1:  scaling has been requested, for THRESH = -2 or
C                   THRESH = -4, but it most probably would not improve
C                   the accuracy of the computed solution for a related
C                   eigenproblem (since maximum norm increased
C                   significantly compared to the original pencil
C                   matrices and (very) high and/or small scaling
C                   factors occurred). The returned scaling factors have
C                   been reset to 1, but information about permutations,
C                   if requested, has been preserved.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit.
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value.
C
C     METHOD
C
C     Balancing consists of applying an equivalence transformation
C     to isolate eigenvalues and/or to make the 1-norms of the rows
C     and columns ILO,...,IHI of A and B nearly equal. If THRESH < 0,
C     a search is performed to find those scaling factors giving the
C     smallest norm ratio or product defined above (see the description
C     of the parameter THRESH).
C
C     Assuming JOB = 'S', let Dl and Dr be diagonal matrices containing
C     the vectors LSCALE and RSCALE, respectively. The returned matrices
C     are obtained using the equivalence transformation
C
C        Dl*A*Dr and Dl*B*Dr.
C
C     For THRESH = 0, the routine returns essentially the same results
C     as the LAPACK subroutine DGGBAL [1]. Setting THRESH < 0, usually
C     gives better results than DGGBAL for badly scaled matrix pencils.
C
C     REFERENCES
C
C     [1] Anderson, E., Bai, Z., Bischof, C., Demmel, J., Dongarra, J.,
C         Du Croz, J., Greenbaum, A., Hammarling, S., McKenney, A.,
C         Ostrouchov, S., and Sorensen, D.
C         LAPACK Users' Guide: Second Edition.
C         SIAM, Philadelphia, 1995.
C
C     NUMERICAL ASPECTS
C
C     No rounding errors appear if JOB = 'P'.
C
C     FURTHER COMMENTS
C
C     If THRESH = -2, the increase of the maximum norm of the scaled
C     submatrices, compared to the maximum norm of the initial
C     submatrices, is bounded by MXGAIN = 100.
C     If THRESH = -2, or THRESH = -4, the maximum condition number of
C     the scaling transformations is bounded by MXCOND = 1/SQRT(EPS),
C     where EPS is the machine precision (see LAPACK Library routine
C     DLAMCH).
C
C     CONTRIBUTOR
C
C     V. Sima, Research Institute for Informatics, Bucharest, Dec. 2015.
C
C     REVISIONS
C
C     V. Sima, Jan. 2016, Jan. 2017, Feb. 2017.
C
C     KEYWORDS
C
C     Balancing, eigenvalue, equivalence transformation, matrix algebra,
C     matrix operations.
C
C  *********************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   HALF, ONE, TEN, THREE, ZERO
      PARAMETER          ( HALF  = 0.5D+0, ONE  = 1.0D+0, TEN = 1.0D+1,
     $                     THREE = 3.0D+0, ZERO = 0.0D+0 )
      DOUBLE PRECISION   MXGAIN, SCLFAC
      PARAMETER          ( MXGAIN = 1.0D+2, SCLFAC = 1.0D+1 )
C     .. Scalar Arguments ..
      CHARACTER          JOB
      INTEGER            IHI, ILO, INFO, IWARN, LDA, LDB, N
      DOUBLE PRECISION   THRESH
C     .. Array Arguments ..
      DOUBLE PRECISION   A(LDA,*), B(LDB,*), DWORK(*), LSCALE(*),
     $                   RSCALE(*)
C     .. Local Scalars ..
      LOGICAL            EVNORM, LOOP, LPERM, LSCAL, STORMN
      INTEGER            I, ICAB, IFLOW, IP1, IR, IRAB, IT, ITER, ITH,
     $                   J, JC, JP1, K, KOUNT, KS, KW1, KW2, KW3, KW4,
     $                   KW5, KW6, KW7, L, LM1, LRAB, LSFMAX, LSFMIN, M,
     $                   NR, NRP2
      DOUBLE PRECISION   AB, ALPHA, BASL, BETA, CAB, CMAX, COEF, COEF2,
     $                   COEF5, COR, DENOM, EPS, EW, EWC, GAMMA, GAP,
     $                   MINPRO, MINRAT, MN, MX, MXCOND, MXNORM, MXS,
     $                   NA, NA0, NAS, NB, NB0, NBS, PGAMMA, PROD, RAB,
     $                   RATIO, SFMAX, SFMIN, SUM, T, TA, TB, TC, TH,
     $                   TH0, THS
C     .. Local Arrays ..
      DOUBLE PRECISION   DUM(1)
C     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            IDAMAX
      DOUBLE PRECISION   DDOT, DLAMCH, DLANGE
      EXTERNAL           DDOT, DLAMCH, DLANGE, IDAMAX, LSAME
C     .. External Subroutines ..
      EXTERNAL           DAXPY, DCOPY, DSCAL, DSWAP, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, INT, LOG10, MAX, MIN, SIGN, SQRT
C
C     .. Executable Statements ..
C
C     Test the input parameters.
C
      INFO  = 0
      IWARN = 0
      LPERM = LSAME( JOB, 'P' ) .OR. LSAME( JOB, 'B' )
      LSCAL = LSAME( JOB, 'S' ) .OR. LSAME( JOB, 'B' )
C
      IF( .NOT.LPERM .AND. .NOT.LSCAL .AND. .NOT.LSAME( JOB, 'N' ) )
     $   THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'MB04DL', -INFO )
         RETURN
      END IF
C
      ILO = 1
      IHI = N
C
C     Quick return if possible.
C
      IF( N.EQ.0 )
     $   RETURN
      IF( ( .NOT.LPERM .AND. .NOT.LSCAL ) .OR. N.EQ.1 ) THEN
         DUM(1) = ONE
         CALL DCOPY( N, DUM, 0, LSCALE, 1 )
         CALL DCOPY( N, DUM, 0, RSCALE, 1 )
         IF( N.EQ.1 .AND. LSCAL ) THEN
            NA0 = ABS( A(1,1) )
            NB0 = ABS( B(1,1) )
            DWORK(1) = NA0
            DWORK(2) = NB0
            DWORK(3) = NA0
            DWORK(4) = NB0
            DWORK(5) = THRESH
         END IF
         RETURN
      END IF
C
      K = 1
      L = N
C
      IF( LPERM ) THEN
C
C        Permute the matrices A and B to isolate the eigenvalues.
C
C        Find row with one nonzero in columns 1 through L.
C
   10    CONTINUE
         LM1 = L - 1
         DO 60 I = L, 1, -1
            DO 20 J = 1, LM1
               JP1 = J + 1
               IF( A(I,J).NE.ZERO .OR. B(I,J).NE.ZERO )
     $            GO TO 30
   20       CONTINUE
            J = L
            GO TO 50
C
   30       CONTINUE
            DO 40 J = JP1, L
               IF( A(I,J).NE.ZERO .OR. B(I,J).NE.ZERO )
     $            GO TO 60
   40       CONTINUE
            J = JP1 - 1
C
   50       CONTINUE
            M = L
            IFLOW = 1
            GO TO 130
   60    CONTINUE
C
C        Find column with one nonzero in rows K through N.
C
   70    CONTINUE
         DO 120 J = K, L
            DO 80 I = K, LM1
               IP1 = I + 1
               IF( A(I,J).NE.ZERO .OR. B(I,J).NE.ZERO )
     $            GO TO 90
   80       CONTINUE
            I = L
            GO TO 110
C
   90       CONTINUE
            DO 100 I = IP1, L
               IF( A(I,J).NE.ZERO .OR. B(I,J).NE.ZERO )
     $            GO TO 120
  100       CONTINUE
            I = IP1 - 1
C
  110       CONTINUE
            M = K
            IFLOW = 2
            GO TO 130
  120    CONTINUE
         GO TO 140
C
C        Permute rows M and I.
C
  130    CONTINUE
         LSCALE(M) = I
         IF( I.NE.M ) THEN
            CALL DSWAP( N-K+1, A(I,K), LDA, A(M,K), LDA )
            CALL DSWAP( N-K+1, B(I,K), LDB, B(M,K), LDB )
         END IF
C
C        Permute columns M and J.
C
         RSCALE(M) = J
         IF( J.NE.M ) THEN
            CALL DSWAP( L, A(1,J), 1, A(1,M), 1 )
            CALL DSWAP( L, B(1,J), 1, B(1,M), 1 )
         END IF
C
         IF( IFLOW.EQ.1 ) THEN
            L = LM1
            IF( L.NE.1 )
     $         GO TO 10
C
            RSCALE(1) = ONE
            LSCALE(1) = ONE
         ELSE
            K = K + 1
            GO TO 70
         END IF
      END IF
C
  140 CONTINUE
      ILO = K
      IHI = L
C
      IF( .NOT.LSCAL ) THEN
         DO 150 I = ILO, IHI
            LSCALE(I) = ONE
            RSCALE(I) = ONE
  150    CONTINUE
         RETURN
      END IF
C
      NR = IHI - ILO + 1
C
C     Compute initial 1-norms and return if ILO = N.
C
      NA0 = DLANGE( '1-norm', NR, NR, A(ILO,ILO), LDA, DWORK )
      NB0 = DLANGE( '1-norm', NR, NR, B(ILO,ILO), LDB, DWORK )
C
      IF( ILO.EQ.IHI ) THEN
         DWORK(1) = NA0
         DWORK(2) = NB0
         DWORK(3) = NA0
         DWORK(4) = NB0
         DWORK(5) = THRESH
         RETURN
      END IF
C
C     Balance the submatrices in rows ILO to IHI.
C
C     Initialize balancing and allocate work storage.
C
      KW1 = N
      KW2 = KW1 + N
      KW3 = KW2 + N
      KW4 = KW3 + N
      KW5 = KW4 + N
      DUM(1) = ZERO
C
C     Prepare for scaling.
C
      SFMIN  = DLAMCH( 'Safe minimum' )
      SFMAX  = ONE / SFMIN
      BASL   = LOG10( SCLFAC )
      LSFMIN = INT( LOG10( SFMIN ) / BASL + ONE )
      LSFMAX = INT( LOG10( SFMAX ) / BASL )
      MXNORM = MAX( NA0, NB0 )
      LOOP   = THRESH.LT.ZERO
C
      IF( LOOP ) THEN
C
C        Compute relative threshold.
C
         NA  = NA0
         NAS = NA0
         NB  = NB0
         NBS = NB0
C
         ITH = THRESH
         MXS = MXNORM
         MX  = ZERO
         MN  = SFMAX
         IF( ITH.GE.-2 ) THEN
            IF( NA.LT.NB ) THEN
               RATIO = MIN( NB/NA, SFMAX )
            ELSE
               RATIO = MIN( NA/NB, SFMAX )
            END IF
            MINRAT = RATIO
         ELSE IF( ITH.LE.-10 ) THEN
            MXCOND = -THRESH
         ELSE
            DENOM  = MAX( ONE, MXNORM )
            PROD   = ( NA/DENOM )*( NB/DENOM )
            MINPRO = PROD
         END IF
         STORMN = .FALSE.
         EVNORM = .FALSE.
C
C        Find maximum order of magnitude of the differences in sizes of
C        the nonzero entries, not considering diag(A) and diag(B).
C
         DO 170 J = ILO, IHI
            DO 160 I = ILO, IHI
               IF( I.NE.J ) THEN
                  AB = ABS( A(I,J) )
                  IF( AB.NE.ZERO )
     $               MN = MIN( MN, AB )
                  MX = MAX( MX, AB )
               END IF
  160       CONTINUE
  170    CONTINUE
C
         DO 190 J = ILO, IHI
            DO 180 I = ILO, IHI
               IF( I.NE.J ) THEN
                  AB = ABS( B(I,J) )
                  IF( AB.NE.ZERO )
     $               MN = MIN( MN, AB )
                  MX = MAX( MX, AB )
               END IF
  180       CONTINUE
  190    CONTINUE
C
         IF( MX*SFMIN.LE.MN ) THEN
            GAP = MX/MN
         ELSE
            GAP = SFMAX
         END IF
         EPS  = DLAMCH( 'Precision' )
         ITER = MIN( INT( LOG10( GAP ) ), -INT( LOG10( EPS ) ) ) + 1
         TH   = MAX( MN, MX*EPS )/MAX( MXNORM, SFMIN )
         THS  = TH
         KW6  = KW5 + N + ILO
         KW7  = KW6 + N
         CALL DCOPY( NR, LSCALE(ILO), 1, DWORK(KW6), 1 )
         CALL DCOPY( NR, RSCALE(ILO), 1, DWORK(KW7), 1 )
C
C        Set the maximum condition number of the transformations.
C
         IF( ITH.GT.-10 )
     $      MXCOND = ONE/SQRT( EPS )
      ELSE
         TH     = MXNORM*THRESH
         ITER   = 1
         EVNORM = .TRUE.
      END IF
      TH0 = TH
C
      COEF  = ONE / DBLE( 2*NR )
      COEF2 = COEF*COEF
      COEF5 = HALF*COEF2
      NRP2  = NR + 2
      BETA  = ZERO
C
C     If THRESH < 0, use a loop to reduce the norm ratio.
C
      DO 400 K = 1, ITER
C
C        Compute right side vector in resulting linear equations.
C
         CALL DCOPY( 6*N, DUM, 0, DWORK, 1 )
         CALL DCOPY(  NR, DUM, 0, LSCALE(ILO), 1 )
         CALL DCOPY(  NR, DUM, 0, RSCALE(ILO), 1 )
         DO 210 I = ILO, IHI
            DO 200 J = ILO, IHI
               TA = ABS( A(I,J) )
               TB = ABS( B(I,J) )
               IF( TA.GT.TH ) THEN
                  TA = LOG10( TA ) / BASL
               ELSE
                  TA = ZERO
               END IF
               IF( TB.GT.TH ) THEN
                  TB = LOG10( TB ) / BASL
               ELSE
                  TB = ZERO
               END IF
               DWORK(I+KW4) = DWORK(I+KW4) - TA - TB
               DWORK(J+KW5) = DWORK(J+KW5) - TA - TB
  200       CONTINUE
  210    CONTINUE
C
         IT = 1
C
C        Start generalized conjugate gradient iteration.
C
  220    CONTINUE
C
         GAMMA = DDOT( NR, DWORK(ILO+KW4), 1, DWORK(ILO+KW4), 1 ) +
     $           DDOT( NR, DWORK(ILO+KW5), 1, DWORK(ILO+KW5), 1 )
C
         EW  = ZERO
         EWC = ZERO
         DO 230 I = ILO, IHI
            EW  = EW  + DWORK(I+KW4)
            EWC = EWC + DWORK(I+KW5)
  230    CONTINUE
C
         GAMMA = COEF*GAMMA - COEF2*( EW**2 + EWC**2 ) -
     $                        COEF5*( EW - EWC  )**2
         IF( GAMMA.EQ.ZERO )
     $      GO TO 300
         IF( IT.NE.1 )
     $      BETA = GAMMA / PGAMMA
         T  = COEF5*( EWC - THREE*EW  )
         TC = COEF5*( EW  - THREE*EWC )
C
         CALL DSCAL( NR, BETA, DWORK(ILO), 1 )
         CALL DSCAL( NR, BETA, DWORK(ILO+KW1), 1 )
C
         CALL DAXPY( NR, COEF, DWORK(ILO+KW4), 1, DWORK(ILO+KW1), 1 )
         CALL DAXPY( NR, COEF, DWORK(ILO+KW5), 1, DWORK(ILO), 1 )
C
         DO 240 J = ILO, IHI
            DWORK(J)     = DWORK(J)     + TC
            DWORK(J+KW1) = DWORK(J+KW1) + T
  240    CONTINUE
C
C        Apply matrix to vector.
C
         DO 260 I = ILO, IHI
            KOUNT = 0
            SUM   = ZERO
            DO 250 J = ILO, IHI
               KS = KOUNT
               IF( A(I,J).NE.ZERO )
     $            KOUNT = KOUNT + 1
               IF( B(I,J).NE.ZERO )
     $            KOUNT = KOUNT + 1
               SUM = SUM + DBLE( KOUNT - KS )*DWORK(J)
  250       CONTINUE
            DWORK(I+KW2) = DBLE( KOUNT )*DWORK(I+KW1) + SUM
  260    CONTINUE
C
         DO 280 J = ILO, IHI
            KOUNT = 0
            SUM   = ZERO
            DO 270 I = ILO, IHI
               KS = KOUNT
               IF( A(I,J).NE.ZERO )
     $            KOUNT = KOUNT + 1
               IF( B(I,J).NE.ZERO )
     $            KOUNT = KOUNT + 1
               SUM = SUM + DBLE( KOUNT - KS )*DWORK(I+KW1)
  270       CONTINUE
            DWORK(J+KW3) = DBLE( KOUNT )*DWORK(J) + SUM
  280    CONTINUE
C
         SUM = DDOT( NR, DWORK(ILO+KW1), 1, DWORK(ILO+KW2), 1 ) +
     $         DDOT( NR, DWORK(ILO),     1, DWORK(ILO+KW3), 1 )
         ALPHA = GAMMA / SUM
C
C        Determine correction to current iteration.
C
         CMAX = ZERO
         DO 290 I = ILO, IHI
            COR = ALPHA*DWORK(I+KW1)
            IF( ABS( COR ).GT.CMAX )
     $         CMAX = ABS( COR )
            LSCALE(I) = LSCALE(I) + COR
            COR = ALPHA*DWORK(I)
            IF( ABS( COR ).GT.CMAX )
     $         CMAX = ABS( COR )
            RSCALE(I) = RSCALE(I) + COR
  290    CONTINUE
C
         IF( CMAX.GE.HALF ) THEN
C
            CALL DAXPY( NR, -ALPHA, DWORK(ILO+KW2), 1, DWORK(ILO+KW4),
     $                  1 )
            CALL DAXPY( NR, -ALPHA, DWORK(ILO+KW3), 1, DWORK(ILO+KW5),
     $                  1 )
C
            PGAMMA = GAMMA
            IT = IT + 1
            IF( IT.LE.NRP2 )
     $         GO TO 220
         END IF
C
C        End generalized conjugate gradient iteration.
C
  300    CONTINUE
C
C        Compute diagonal scaling matrices.
C
         DO 310 I = ILO, IHI
            IRAB = IDAMAX( N-ILO+1, A(I,ILO), LDA )
            RAB  = ABS( A(I,ILO+IRAB-1) )
            IRAB = IDAMAX( N-ILO+1, B(I,ILO), LDB )
            RAB  = MAX( RAB, ABS( B(I,ILO+IRAB-1) ) )
            LRAB = INT( LOG10( RAB+SFMIN ) / BASL + ONE )
            IR   = LSCALE(I) + SIGN( HALF, LSCALE(I) )
            IR   = MIN( MAX( IR, LSFMIN ), LSFMAX, LSFMAX-LRAB )
            LSCALE(I) = SCLFAC**IR
C
            ICAB = IDAMAX( IHI, A(1,I), 1 )
            CAB  = ABS( A(ICAB,I) )
            ICAB = IDAMAX( IHI, B(1,I), 1 )
            CAB  = MAX( CAB, ABS( B(ICAB,I) ) )
            LRAB = INT( LOG10( CAB+SFMIN ) / BASL + ONE )
            JC = RSCALE(I) + SIGN( HALF, RSCALE(I) )
            JC = MIN( MAX( JC, LSFMIN ), LSFMAX, LSFMAX-LRAB )
            RSCALE(I) = SCLFAC**JC
  310    CONTINUE
C
         DO 320 I = ILO, IHI
            IF( LSCALE(I).NE.ONE .OR. RSCALE(I).NE.ONE )
     $         GO TO 330
  320    CONTINUE
C
C        Finish the procedure for all scaling factors equal to 1.
C
         NAS = NA0
         NBS = NB0
         THS = TH0
         GO TO 460
C
  330    CONTINUE
C
         IF( LOOP ) THEN
            IF( ITH.LE.-10 ) THEN
C
C              Compute the reciprocal condition number of the left and
C              right transformations. Continue the loop if it is too
C              small.
C
               IR = IDAMAX( NR, LSCALE(ILO), 1 )
               JC = IDAMAX( NR, RSCALE(ILO), 1 )
               T  = LSCALE(ILO+IR-1)
               MN = T
               DO 340 I = ILO, IHI
                  IF( LSCALE(I).LT.MN )
     $               MN = LSCALE(I)
  340          CONTINUE
               T  = MN/T
               TA = RSCALE(ILO+JC-1)
               MN = TA
               DO 350 I = ILO, IHI
                  IF( RSCALE(I).LT.MN )
     $               MN = RSCALE(I)
  350          CONTINUE
               T = MIN( T, MN/TA )
               IF( T.LT.ONE/MXCOND ) THEN
                  TH = TH*TEN
                  GO TO 400
               ELSE
                  THS    = TH
                  EVNORM = .TRUE.
                  GO TO 430
               END IF
            END IF
C
C           Compute the 1-norms of the scaled submatrices,
C           without actually scaling them.
C
            NA = ZERO
            DO 370 J = ILO, IHI
               T = ZERO
               DO 360 I = ILO, IHI
                  T = T + ABS( A(I,J) )*LSCALE(I)*RSCALE(J)
  360          CONTINUE
               IF( T.GT.NA )
     $            NA = T
  370       CONTINUE
C
            NB = ZERO
            DO 390 J = ILO, IHI
               T = ZERO
               DO 380 I = ILO, IHI
                  T = T + ABS( B(I,J) )*LSCALE(I)*RSCALE(J)
  380          CONTINUE
               IF( T.GT.NB )
     $            NB = T
  390       CONTINUE
C
            IF( ITH.GE.-4 .AND. ITH.LT.-2 ) THEN
               PROD = ( NA/DENOM )*( NB/DENOM )
               IF( MINPRO.GT.PROD ) THEN
                  MINPRO = PROD
                  STORMN = .TRUE.
                  CALL DCOPY( NR, LSCALE(ILO), 1, DWORK(KW6), 1 )
                  CALL DCOPY( NR, RSCALE(ILO), 1, DWORK(KW7), 1 )
                  NAS = NA
                  NBS = NB
                  THS = TH
               END IF
            ELSE IF( ITH.GE.-2 ) THEN
               IF( NA.LT.NB ) THEN
                  RATIO = MIN( NB/NA, SFMAX )
               ELSE
                  RATIO = MIN( NA/NB, SFMAX )
               END IF
               IF( MINRAT.GT.RATIO ) THEN
                  MINRAT = RATIO
                  STORMN = .TRUE.
                  CALL DCOPY( NR, LSCALE(ILO), 1, DWORK(KW6), 1 )
                  CALL DCOPY( NR, RSCALE(ILO), 1, DWORK(KW7), 1 )
                  MXS = MAX( NA, NB )
                  NAS = NA
                  NBS = NB
                  THS = TH
               END IF
            END IF
            TH = TH*TEN
         END IF
  400 CONTINUE
C
C     Prepare for scaling.
C
      IF( LOOP ) THEN
         IF( ITH.LE.-10 ) THEN
C
C           Could not find enough well conditioned transformations
C           for THRESH <= -10. Set scaling factors to 1 and return.
C
            DUM(1) = ONE
            CALL DCOPY( NR, DUM(1), 0, LSCALE(ILO), 1 )
            CALL DCOPY( NR, DUM(1), 0, RSCALE(ILO), 1 )
            IWARN = 1
            GO TO 460
         END IF
C
C        Check if scaling might reduce the accuracy when solving related
C        eigenproblems, and set the scaling factors to 1 in this case,
C        if THRESH = -2 or THRESH = -4.
C
         IF( ( MXNORM.LT.MXS .AND. MXNORM.LT.MXS/MXGAIN .AND. ITH.EQ.-2)
     $         .OR. ITH.EQ.-4 ) THEN
            IR = IDAMAX( NR, DWORK(KW6), 1 )
            JC = IDAMAX( NR, DWORK(KW7), 1 )
            T  = DWORK(KW6+IR-1)
            MN = T
            DO 410 I = KW6, KW6+NR-1
               IF( DWORK(I).LT.MN )
     $            MN = DWORK(I)
  410       CONTINUE
            T  = MN/T
            TA = DWORK(KW7+JC-1)
            MN = TA
            DO 420 I = KW7, KW7+NR-1
               IF( DWORK(I).LT.MN )
     $            MN = DWORK(I)
  420       CONTINUE
            T = MIN( T, MN/TA )
            IF( T.LT.ONE/MXCOND ) THEN
               DUM(1) = ONE
               CALL DCOPY( NR, DUM(1), 0, LSCALE(ILO), 1 )
               CALL DCOPY( NR, DUM(1), 0, RSCALE(ILO), 1 )
               IWARN = 1
               NAS = NA0
               NBS = NB0
               THS = TH0
               GO TO 460
            END IF
         END IF
         IF( STORMN ) THEN
            CALL DCOPY( NR, DWORK(KW6), 1, LSCALE(ILO), 1 )
            CALL DCOPY( NR, DWORK(KW7), 1, RSCALE(ILO), 1 )
         ELSE
            NAS = NA
            NBS = NB
            THS = TH
         END IF
      END IF
C
  430 CONTINUE
C
C     Row scaling.
C
      DO 440 I = ILO, IHI
         CALL DSCAL( N-ILO+1, LSCALE(I), A(I,ILO), LDA )
         CALL DSCAL( N-ILO+1, LSCALE(I), B(I,ILO), LDB )
  440 CONTINUE
C
C     Column scaling.
C
      DO 450 J = ILO, IHI
         CALL DSCAL( IHI, RSCALE(J), A(1,J), 1 )
         CALL DSCAL( IHI, RSCALE(J), B(1,J), 1 )
  450 CONTINUE
C
C     Set DWORK(1:5).
C
  460 CONTINUE
      IF( EVNORM ) THEN
         NAS = DLANGE( '1-norm', NR, NR, A(ILO,ILO), LDA, DWORK )
         NBS = DLANGE( '1-norm', NR, NR, B(ILO,ILO), LDB, DWORK )
      END IF
C
      DWORK(1) = NA0
      DWORK(2) = NB0
      DWORK(3) = NAS
      DWORK(4) = NBS
      IF( LOOP ) THEN
         DWORK(5) = THS/MAX( MXNORM, SFMIN )
      ELSE
         DWORK(5) = THRESH
      END IF
C
      RETURN
C *** Last line of MB04DL ***
      END
