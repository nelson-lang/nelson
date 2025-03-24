      SUBROUTINE MB04DP( JOB, N, THRESH, A, LDA, DE, LDDE, C, LDC, VW,
     $                   LDVW, ILO, LSCALE, RSCALE, DWORK, IWARN, INFO )
C
C     PURPOSE
C
C     To balance the 2*N-by-2*N skew-Hamiltonian/Hamiltonian pencil
C     aS - bH, with
C
C           (  A  D  )         (  C  V  )
C       S = (        ) and H = (        ),  A, C N-by-N,             (1)
C           (  E  A' )         (  W -C' )
C
C     where D and E are skew-symmetric, and V and W are symmetric
C     matrices. This involves, first, permuting aS - bH by a symplectic
C     equivalence transformation to isolate eigenvalues in the first
C     1:ILO-1 elements on the diagonal of A and C; and second, applying
C     a diagonal equivalence transformation to make the pairs of rows
C     and columns ILO:N and N+ILO:2*N as close in 1-norm as possible.
C     Both steps are optional. Balancing may reduce the 1-norms of the
C     matrices S and H.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOB     CHARACTER*1
C             Specifies the operations to be performed on S and H:
C             = 'N':  none:  simply set ILO = 1, LSCALE(I) = 1.0 and
C                     RSCALE(I) = 1.0 for i = 1,...,N.
C             = 'P':  permute only;
C             = 'S':  scale only;
C             = 'B':  both permute and scale.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of matrices A, D, E, C, V, and W.  N >= 0.
C
C     THRESH  (input) DOUBLE PRECISION
C             If JOB = 'S' or JOB = 'B', and THRESH >= 0, threshold
C             value for magnitude of the elements to be considered in
C             the scaling process: elements with magnitude less than or
C             equal to THRESH*MXNORM are ignored for scaling, where
C             MXNORM is the maximum of the 1-norms of the original
C             submatrices S(s,s) and H(s,s), with s = [ILO:N,N+ILO:2*N].
C             If THRESH < 0, the subroutine finds the scaling factors
C             for which some conditions, detailed below, are fulfilled.
C             A sequence of increasing strictly positive threshold
C             values is used.
C             If THRESH = -1, the condition is that
C                max( norm(H(s,s),1)/norm(S(s,s),1),
C                     norm(S(s,s),1)/norm(H(s,s),1) )                (1)
C             has the smallest value, for the threshold values used,
C             where S(s,s) and H(s,s) are the scaled submatrices.
C             If THRESH = -2, the norm ratio reduction (1) is tried, but
C             the subroutine may return IWARN = 1 and reset the scaling
C             factors to 1, if this seems suitable. See the description
C             of the argument IWARN and FURTHER COMMENTS.
C             If THRESH = -3, the condition is that
C                norm(H(s,s),1)*norm(S(s,s),1)                       (2)
C             has the smallest value for the scaled submatrices.
C             If THRESH = -4, the norm reduction in (2) is tried, but
C             the subroutine may return IWARN = 1 and reset the scaling
C             factors to 1, as for THRESH = -2 above.
C             If THRESH = -VALUE, with VALUE >= 10, the condition
C             numbers of the left and right scaling transformations will
C             be bounded by VALUE, i.e., the ratios between the largest
C             and smallest entries in [LSCALE(ILO:N); RSCALE(ILO:N)]
C             will be at most VALUE. VALUE should be a power of 10.
C             If JOB = 'N' or JOB = 'P', the value of THRESH is
C             irrelevant.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the matrix A.
C             On exit, the leading N-by-N part of this array contains
C             the matrix A of the balanced skew-Hamiltonian matrix S.
C             In particular, the strictly lower triangular part of the
C             first ILO-1 columns of A is zero.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     DE      (input/output) DOUBLE PRECISION array, dimension
C                            (LDDE, N+1)
C             On entry, the leading N-by-N strictly lower triangular
C             part of this array must contain the strictly lower
C             triangular part of the skew-symmetric matrix E, and the
C             N-by-N strictly upper triangular part of the submatrix
C             in the columns 2 to N+1 of this array must contain the
C             strictly upper triangular part of the skew-symmetric
C             matrix D.
C             The entries on the diagonal and the first superdiagonal of
C             this array need not be set, but are assumed to be zero.
C             On exit, the leading N-by-N strictly lower triangular
C             part of this array contains the strictly lower triangular
C             part of the balanced matrix E, and the N-by-N strictly
C             upper triangular part of the submatrix in the columns 2 to
C             N+1 of this array contains the strictly upper triangular
C             part of the balanced matrix D. In particular, the strictly
C             lower triangular part of the first ILO-1 columns of DE is
C             zero.
C
C     LDDE    INTEGER
C             The leading dimension of the array DE.  LDDE >= MAX(1, N).
C
C     C       (input/output) DOUBLE PRECISION array, dimension (LDC, N)
C             On entry, the leading N-by-N part of this array must
C             contain the matrix C.
C             On exit, the leading N-by-N part of this array contains
C             the matrix C of the balanced Hamiltonian matrix H.
C             In particular, the strictly lower triangular part of the
C             first ILO-1 columns of C is zero.
C
C     LDC     INTEGER
C             The leading dimension of the array C.  LDC >= MAX(1, N).
C
C     VW      (input/output) DOUBLE PRECISION array, dimension
C                            (LDVW, N+1)
C             On entry, the leading N-by-N lower triangular part of
C             this array must contain the lower triangular part of the
C             symmetric matrix W, and the N-by-N upper triangular
C             part of the submatrix in the columns 2 to N+1 of this
C             array must contain the upper triangular part of the
C             symmetric matrix V.
C             On exit, the leading N-by-N lower triangular part of this
C             array contains the lower triangular part of the balanced
C             matrix W, and the N-by-N upper triangular part of the
C             submatrix in the columns 2 to N+1 of this array contains
C             the upper triangular part of the balanced matrix V. In
C             particular, the lower triangular part of the first ILO-1
C             columns of VW is zero.
C
C     LDVW    INTEGER
C             The leading dimension of the array VW.  LDVW >= MAX(1, N).
C
C     ILO     (output) INTEGER
C             ILO-1 is the number of deflated eigenvalues in the
C             balanced skew-Hamiltonian/Hamiltonian matrix pencil.
C             ILO is set to 1 if JOB = 'N' or JOB = 'S'.
C
C     LSCALE  (output) DOUBLE PRECISION array, dimension (N)
C             Details of the permutations of S and H and scaling applied
C             to A, D, C, and V from the left. For j = 1,...,ILO-1 let
C             P(j) = LSCALE(j). If P(j) <= N, then rows and columns P(j)
C             and P(j)+N are interchanged with rows and columns j and
C             j+N, respectively. If P(j) > N, then row and column P(j)-N
C             are interchanged with row and column j+N by a generalized
C             symplectic permutation. For j = ILO,...,N the j-th element
C             of LSCALE contains the factor of the scaling applied to
C             row j of the matrices A, D, C, and V.
C
C     RSCALE  (output) DOUBLE PRECISION array, dimension (N)
C             Details of the permutations of S and H and scaling applied
C             to A, E, C, and W from the right. For j = 1,...,ILO-1 let
C             P(j) = RSCALE(j). If P(j) <= N, then rows and columns P(j)
C             and P(j)+N are interchanged with rows and columns j and
C             j+N, respectively. If P(j) > N, then row and column P(j)-N
C             are interchanged with row and column j+N by a generalized
C             symplectic permutation. For j = ILO,...,N the j-th element
C             of RSCALE contains the factor of the scaling applied to
C             column j of the matrices A, E, C, and W.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK) where
C             LDWORK = 0,   if  JOB = 'N' or JOB = 'P', or N = 0;
C             LDWORK = 6*N, if (JOB = 'S' or JOB = 'B') and THRESH >= 0;
C             LDWORK = 8*N, if (JOB = 'S' or JOB = 'B') and THRESH <  0.
C             On exit, if JOB = 'S' or JOB = 'B', DWORK(1) and DWORK(2)
C             contain the initial 1-norms of S(s,s) and H(s,s), and
C             DWORK(3) and DWORK(4) contain their final 1-norms,
C             respectively. Moreover, DWORK(5) contains the THRESH value
C             used (irrelevant if IWARN = 1 or ILO = N).
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
C     Balancing consists of applying a (symplectic) equivalence
C     transformation to isolate eigenvalues and/or to make the 1-norms
C     of each pair of rows and columns indexed by s of S and H nearly
C     equal. If THRESH < 0, a search is performed to find those scaling
C     factors giving the smallest norm ratio or product defined above
C     (see the description of the parameter THRESH).
C
C     Assuming JOB = 'S', let Dl and Dr be diagonal matrices containing
C     the vectors LSCALE and RSCALE, respectively. The returned matrices
C     are obtained using the equivalence transformation
C
C       ( Dl  0 ) ( A  D  ) ( Dr  0 )   ( Dl  0 ) ( C  V  ) ( Dr  0 )
C       (       ) (       ) (       ),  (       ) (       ) (       ).
C       ( 0  Dr ) ( E  A' ) ( 0  Dl )   ( 0  Dr ) ( W -C' ) ( 0  Dl )
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
C     [2] Benner, P.
C         Symplectic balancing of Hamiltonian matrices.
C         SIAM J. Sci. Comput., 22 (5), pp. 1885-1904, 2001.
C
C     NUMERICAL ASPECTS
C
C     The transformations used preserve the skew-Hamiltonian/Hamiltonian
C     structure and do not introduce significant rounding errors.
C     No rounding errors appear if JOB = 'P'. If T is the global
C     transformation matrix applied to the right, then J'*T*J is the
C     global transformation matrix applied to the left, where
C     J = [ 0 I; -I 0 ], with blocks of order N.
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
C     V. Sima, Research Institute for Informatics, Bucharest, Sep. 2015.
C
C     REVISIONS
C
C     V. Sima, Oct. 2015, Nov. 2015, Dec. 2015, Feb. 2016, Jan. 2017,
C     Feb. 2017.
C
C     KEYWORDS
C
C     Balancing, eigenvalue, equivalence transformation, matrix algebra,
C     matrix operations, symplectic equivalence transformation.
C
C  *********************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   HALF, ONE, TEN, TWO, ZERO
      PARAMETER          ( HALF = 0.5D+0, ONE  = 1.0D+0, TEN = 1.0D+1,
     $                     TWO  = 2.0D+0, ZERO = 0.0D+0 )
      DOUBLE PRECISION   MXGAIN, SCLFAC
      PARAMETER          ( MXGAIN = 1.0D+2, SCLFAC = 1.0D+1 )
C     .. Scalar Arguments ..
      CHARACTER          JOB
      INTEGER            ILO, INFO, IWARN, LDA, LDC, LDDE, LDVW, N
      DOUBLE PRECISION   THRESH
C     .. Array Arguments ..
      DOUBLE PRECISION   A(LDA,*), C(LDC,*), DE(LDDE,*), DWORK(*),
     $                   LSCALE(*), RSCALE(*), VW(LDVW,*)
C     .. Local Scalars ..
      LOGICAL            EVNORM, LOOP, LPERM, LSCAL, STORMN
      INTEGER            I, ICAB, ILOOLD, IR, IRAB, IT, ITER, ITH, J,
     $                   JC, K, KOUNT, KS, KW1, KW2, KW3, KW4, KW5, KW6,
     $                   KW7, LRAB, LSFMAX, LSFMIN, NR, NRP2
      DOUBLE PRECISION   AB, ALPHA, BASL, BETA, CAB, CMAX, COEF, COEF2,
     $                   COEF5, COR, DENOM, EPS, EW, GAMMA, GAP, MINPRO,
     $                   MINRAT, MN, MX, MXCOND, MXNORM, MXS, NH, NH0,
     $                   NHS, NS, NS0, NSS, PGAMMA, PROD, RAB, RATIO,
     $                   SFMAX, SFMIN, SUM, T, TA, TC, TD, TE, TH, TH0,
     $                   THS, TV, TW
C     .. Local Arrays ..
      DOUBLE PRECISION   DUM(1)
C     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            IDAMAX
      DOUBLE PRECISION   DDOT, DLAMCH, MA02ID
      EXTERNAL           DDOT, DLAMCH, IDAMAX, LSAME, MA02ID
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
      ELSE IF( LDA .LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LDDE.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDC .LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE IF( LDVW.LT.MAX( 1, N ) ) THEN
         INFO = -11
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'MB04DP', -INFO )
         RETURN
      END IF
C
      ILO = 1
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
            NS0 = MA02ID( 'skew-Hamiltonian', '1-norm', N, A, LDA, DE,
     $                     LDDE, DWORK )
            NH0 = MA02ID( 'Hamiltonian', '1-norm', N, C, LDC, VW, LDVW,
     $                     DWORK )
            DWORK(1) = NS0
            DWORK(2) = NH0
            DWORK(3) = NS0
            DWORK(4) = NH0
            DWORK(5) = THRESH
         END IF
         RETURN
      END IF
C
      IF( LPERM ) THEN
C
C        Permute the matrices S and H to isolate the eigenvalues.
C
         ILOOLD = 0
C        WHILE ( ILO.NE.ILOOLD )
   10    CONTINUE
         IF( ILO.NE.ILOOLD ) THEN
            ILOOLD = ILO
C
C           Scan columns ILO .. N.
C
            I = ILO
C           WHILE ( I.LE.N .AND. ILO.EQ.ILOOLD )
   20       CONTINUE
            IF( I.LE.N .AND. ILO.EQ.ILOOLD ) THEN
               DO 30  J = ILO, I-1
                  IF( A(J,I).NE.ZERO .OR. C(J,I).NE.ZERO ) THEN
                     I = I + 1
                     GOTO 20
                  END IF
   30          CONTINUE
               DO 40  J = I+1, N
                  IF( A(J,I).NE.ZERO .OR. C(J,I).NE.ZERO ) THEN
                     I = I + 1
                     GOTO 20
                  END IF
   40          CONTINUE
               DO 50  J = ILO, I-1
                  IF( DE(I,J).NE.ZERO .OR. VW(I,J).NE.ZERO ) THEN
                     I = I + 1
                     GOTO 20
                  END IF
   50          CONTINUE
               IF( VW(I,I).NE.ZERO ) THEN
                  I = I + 1
                  GOTO 20
               END IF
               DO 60  J = I+1, N
                  IF( DE(J,I).NE.ZERO .OR. VW(J,I).NE.ZERO ) THEN
                     I = I + 1
                     GOTO 20
                  END IF
   60          CONTINUE
C
C              Exchange columns/rows ILO <-> I.
C
               LSCALE(ILO) = I
               RSCALE(ILO) = I
C
               IF( ILO.NE.I ) THEN
C
                  CALL DSWAP( N, A(1,ILO), 1, A(1,I), 1 )
                  CALL DSWAP( N-ILO+1, A(ILO,ILO), LDA, A(I,ILO), LDA )
C
                  IF( I.LT.N )
     $               CALL DSWAP( N-I, DE(I+1,ILO), 1, DE(I+1,I), 1 )
                  IF( I.GT.ILO+1 ) THEN
                     CALL DSCAL( I-ILO-1, -ONE, DE(ILO+1,ILO), 1 )
                     CALL DSWAP( I-ILO-1, DE(ILO+1,ILO), 1, DE(I,ILO+1),
     $                           LDDE )
                  END IF
C
                  CALL DSWAP( ILO-1, DE(1,ILO+1), 1, DE(1,I+1), 1 )
                  IF( N.GT.I )
     $               CALL DSWAP( N-I, DE(I,I+2), LDDE, DE(ILO,I+2),
     $                           LDDE )
                  IF( I.GT.ILO+1 ) THEN
                     CALL DSCAL( I-ILO-1, -ONE, DE(ILO+1,I+1), 1 )
                     CALL DSWAP( I-ILO-1, DE(ILO,ILO+2), LDDE,
     $                           DE(ILO+1,I+1), 1 )
                  END IF
                  CALL DSCAL( I-ILO, -ONE, DE(ILO,I+1), 1 )
C
                  CALL DSWAP( N, C(1,ILO), 1, C(1,I), 1 )
                  CALL DSWAP( N-ILO+1, C(ILO,ILO), LDC, C(I,ILO), LDC )
C
                  T           = VW(I,ILO)
                  VW(I,ILO)   = VW(ILO,ILO)
                  VW(ILO,ILO) = T
                  CALL DSWAP( N-I+1, VW(I,ILO), 1, VW(I,I), 1 )
                  CALL DSWAP( I-ILO, VW(ILO,ILO), 1, VW(I,ILO), LDVW )
C
                  CALL DSWAP( ILO, VW(1,ILO+1), 1, VW(1,I+1), 1 )
                  CALL DSWAP( N-I+1, VW(I,I+1), LDVW, VW(ILO,I+1),
     $                        LDVW )
                  CALL DSWAP( I-ILO, VW(ILO,ILO+1), LDVW, VW(ILO,I+1),
     $                        1 )
               END IF
               ILO = ILO + 1
            END IF
C           END WHILE 20
C
C           Scan columns N+ILO .. 2*N.
C
            I = ILO
C           WHILE ( I.LE.N .AND. ILO.EQ.ILOOLD )
   70       CONTINUE
            IF( I.LE.N .AND. ILO.EQ.ILOOLD ) THEN
               DO 80  J = ILO, I-1
                  IF( A(I,J).NE.ZERO .OR. C(I,J).NE.ZERO ) THEN
                     I = I + 1
                     GOTO 70
                  END IF
   80          CONTINUE
               DO 90  J = I+1, N
                  IF( A(I,J).NE.ZERO .OR. C(I,J).NE.ZERO ) THEN
                     I = I + 1
                     GOTO 70
                  END IF
   90          CONTINUE
               DO 100  J = ILO, I-1
                  IF( DE(J,I+1).NE.ZERO .OR. VW(J,I+1).NE.ZERO ) THEN
                     I = I + 1
                     GOTO 70
                  END IF
  100          CONTINUE
               IF( VW(I,I+1).NE.ZERO ) THEN
                  I = I + 1
                  GOTO 70
               END IF
               DO 110  J = I+1, N
                  IF( DE(I,J+1).NE.ZERO .OR. VW(I,J+1).NE.ZERO ) THEN
                     I = I + 1
                     GOTO 70
                  END IF
  110          CONTINUE
C
C              Exchange columns/rows I <-> I+N with a symplectic
C              generalized permutation.
C
               LSCALE(ILO) = N + I
               RSCALE(ILO) = N + I
C
               CALL DSWAP( I-ILO, A(I,ILO), LDA, DE(I,ILO), LDDE )
               CALL DSCAL( I-ILO, -ONE, A(I,ILO), LDA )
               IF( N.GT.I ) THEN
                  CALL DSWAP( N-I, A(I,I+1), LDA, DE(I+1,I), 1 )
                  CALL DSCAL( N-I, -ONE, DE(I+1,I), 1 )
               END IF
               CALL DSWAP( I-1, A(1,I), 1, DE(1,I+1), 1 )
               CALL DSCAL( I-1, -ONE, A(1,I), 1 )
               IF( N.GT.I ) THEN
                  CALL DSCAL( N-I, -ONE, A(I+1,I), 1 )
                  CALL DSWAP( N-I, A(I+1,I), 1, DE(I,I+2), LDDE )
               END IF
C
               CALL DSWAP( I-ILO, C(I,ILO), LDC, VW(I,ILO), LDVW )
               CALL DSCAL( I-ILO, -ONE, C(I,ILO), LDC )
               IF( N.GT.I ) THEN
                  CALL DSWAP( N-I, C(I,I+1), LDC, VW(I+1,I), 1 )
                  CALL DSCAL( N-I, -ONE, C(I,I+1), LDC )
               END IF
               CALL DSWAP( I-1, C(1,I), 1, VW(1,I+1), 1 )
               CALL DSCAL( I-1, -ONE, C(1,I), 1 )
               IF( N.GT.I ) THEN
                  CALL DSWAP( N-I, C(I+1,I), 1, VW(I,I+2), LDVW )
                  CALL DSCAL( N-I, -ONE, C(I+1,I), 1 )
               END IF
               C(I,I)    = -C(I,I)
               T         =  VW(I,I)
               VW(I,I)   = -VW(I,I+1)
               VW(I,I+1) = -T
C
C              Exchange columns/rows ILO <-> I.
C
               IF( ILO.NE.I ) THEN
C
                  CALL DSWAP( N, A(1,ILO), 1, A(1,I), 1 )
                  CALL DSWAP( N-ILO+1, A(ILO,ILO), LDA, A(I,ILO), LDA )
C
                  IF( I.LT.N )
     $               CALL DSWAP( N-I, DE(I+1,ILO), 1, DE(I+1,I), 1 )
                  IF( I.GT.ILO+1 ) THEN
                     CALL DSCAL( I-ILO-1, -ONE, DE(ILO+1,ILO), 1 )
                     CALL DSWAP( I-ILO-1, DE(ILO+1,ILO), 1, DE(I,ILO+1),
     $                           LDDE )
                  END IF
C
                  CALL DSWAP( ILO-1, DE(1,ILO+1), 1, DE(1,I+1), 1 )
                  IF( N.GT.I )
     $               CALL DSWAP( N-I, DE(I,I+2), LDDE, DE(ILO,I+2),
     $                           LDDE )
                  IF( I.GT.ILO+1 ) THEN
                     CALL DSCAL( I-ILO-1, -ONE, DE(ILO+1,I+1), 1 )
                     CALL DSWAP( I-ILO-1, DE(ILO,ILO+2), LDDE,
     $                           DE(ILO+1,I+1), 1 )
                  END IF
                  CALL DSCAL( I-ILO, -ONE, DE(ILO,I+1), 1 )
C
                  CALL DSWAP( N, C(1,ILO), 1, C(1,I), 1 )
                  CALL DSWAP( N-ILO+1, C(ILO,ILO), LDC, C(I,ILO), LDC )
C
                  T           = VW(I,ILO)
                  VW(I,ILO)   = VW(ILO,ILO)
                  VW(ILO,ILO) = T
                  CALL DSWAP( N-I+1, VW(I,ILO), 1, VW(I,I), 1 )
                  CALL DSWAP( I-ILO, VW(ILO,ILO), 1, VW(I,ILO), LDVW )
C
                  CALL DSWAP( ILO, VW(1,ILO+1), 1, VW(1,I+1), 1 )
                  CALL DSWAP( N-I+1, VW(I,I+1), LDVW, VW(ILO,I+1),
     $                        LDVW )
                  CALL DSWAP( I-ILO, VW(ILO,ILO+1), LDVW, VW(ILO,I+1),
     $                        1 )
               END IF
               ILO = ILO + 1
            END IF
C           END WHILE 70
            GOTO 10
         END IF
C        END WHILE 10
C
         DO 120 I = ILO, N
            LSCALE( I ) = ONE
            RSCALE( I ) = ONE
  120    CONTINUE
         IF( .NOT.LSCAL )
     $      RETURN
      END IF
C
      NR = N - ILO + 1
C
C     Compute initial 1-norms and return if ILO = N.
C
      NS0 = MA02ID( 'skew-Hamiltonian', '1-norm', NR, A(ILO,ILO), LDA,
     $               DE(ILO,ILO), LDDE, DWORK )
      NH0 = MA02ID( 'Hamiltonian', '1-norm', NR, C(ILO,ILO), LDC,
     $               VW(ILO,ILO), LDVW, DWORK )
C
      IF( ILO.EQ.N ) THEN
         DWORK(1) = NS0
         DWORK(2) = NH0
         DWORK(3) = NS0
         DWORK(4) = NH0
         DWORK(5) = THRESH
         RETURN
      END IF
C
C     Balance the submatrices in rows ILO to N.
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
      MXNORM = MAX( NS0, NH0 )
      LOOP   = THRESH.LT.ZERO
C
      IF( LOOP ) THEN
C
C        Compute relative threshold.
C
         NS  = NS0
         NSS = NS0
         NH  = NH0
         NHS = NH0
C
         ITH = THRESH
         MXS = MXNORM
         MX  = ZERO
         MN  = SFMAX
         IF( ITH.GE.-2 ) THEN
            IF( NS.LT.NH ) THEN
               RATIO = MIN( NH/NS, SFMAX )
            ELSE
               RATIO = MIN( NS/NH, SFMAX )
            END IF
            MINRAT = RATIO
         ELSE IF( ITH.LE.-10 ) THEN
            MXCOND = -THRESH
         ELSE
            DENOM  = MAX( ONE, MXNORM )
            PROD   = ( NS/DENOM )*( NH/DENOM )
            MINPRO = PROD
         END IF
         STORMN = .FALSE.
         EVNORM = .FALSE.
C
C        Find maximum order of magnitude of the differences in sizes of
C        the nonzero entries, not considering diag(A) and diag(C).
C
         DO 140 J = ILO, N
            DO 130 I = ILO, N
               IF( I.NE.J ) THEN
                  AB = ABS( A(I,J) )
                  IF( AB.NE.ZERO )
     $               MN = MIN( MN, AB )
                  MX = MAX( MX, AB )
               END IF
  130       CONTINUE
  140    CONTINUE
C
         DO 160 J = ILO, N
            DO 150 I = ILO, N
               IF( I.NE.J ) THEN
                  AB = ABS( DE(I,J) )
                  IF( AB.NE.ZERO )
     $               MN = MIN( MN, AB )
                  MX = MAX( MX, AB )
                  AB = ABS( DE(I,J+1) )
                  IF( AB.NE.ZERO )
     $               MN = MIN( MN, AB )
                  MX = MAX( MX, AB )
               END IF
  150       CONTINUE
  160    CONTINUE
C
         DO 180 J = ILO, N
            DO 170 I = ILO, N
               IF( I.NE.J ) THEN
                  AB = ABS( C(I,J) )
                  IF( AB.NE.ZERO )
     $               MN = MIN( MN, AB )
                  MX = MAX( MX, AB )
               END IF
  170       CONTINUE
  180    CONTINUE
C
         DO 200 J = ILO, N
            DO 190 I = ILO, N
               AB = ABS( VW(I,J) )
               IF( AB.NE.ZERO )
     $            MN = MIN( MN, AB )
               MX = MAX( MX, AB )
               AB = ABS( VW(I,J+1) )
               IF( AB.NE.ZERO )
     $            MN = MIN( MN, AB )
               MX = MAX( MX, AB )
  190       CONTINUE
  200    CONTINUE
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
      COEF  = HALF / DBLE( 2*NR )
      COEF2 = COEF*COEF
      COEF5 = HALF*COEF2
      NRP2  = NR + 2
      BETA  = ZERO
C
C     If THRESH < 0, use a loop to reduce the norm ratio.
C
      DO 450 K = 1, ITER
C
C        Compute right side vector in resulting linear equations.
C
         CALL DCOPY( 6*N, DUM, 0, DWORK, 1 )
         CALL DCOPY(  NR, DUM, 0, LSCALE(ILO), 1 )
         CALL DCOPY(  NR, DUM, 0, RSCALE(ILO), 1 )
         DO 220 I = ILO, N
            DO 210 J = ILO, N
               TA = ABS( A(I,J) )
               TC = ABS( C(I,J) )
               IF( J.GT.I ) THEN
                  TD = ABS( DE(I,J+1) )
               ELSE IF( J.LT.I ) THEN
                  TD = ABS( DE(J,I+1) )
               ELSE
                  TD = ZERO
               END IF
               IF( J.GT.I ) THEN
                  TE = ABS( DE(J,I) )
               ELSE IF( J.LT.I ) THEN
                  TE = ABS( DE(I,J) )
               ELSE
                  TE = ZERO
               END IF
               IF( J.GT.I ) THEN
                  TV = ABS( VW(I,J+1) )
               ELSE
                  TV = ABS( VW(J,I+1) )
               END IF
               IF( J.GT.I ) THEN
                  TW = ABS( VW(J,I) )
               ELSE
                  TW = ABS( VW(I,J) )
               END IF
               IF( TA.GT.TH ) THEN
                  TA = LOG10( TA ) / BASL
               ELSE
                  TA = ZERO
               END IF
               IF( TC.GT.TH ) THEN
                  TC = LOG10( TC ) / BASL
               ELSE
                  TC = ZERO
               END IF
               IF( TD.GT.TH ) THEN
                  TD = LOG10( TD ) / BASL
               ELSE
                  TD = ZERO
               END IF
               IF( TE.GT.TH ) THEN
                  TE = LOG10( TE ) / BASL
               ELSE
                  TE = ZERO
               END IF
               IF( TV.GT.TH ) THEN
                  TV = LOG10( TV ) / BASL
               ELSE
                  TV = ZERO
               END IF
               IF( TW.GT.TH ) THEN
                  TW = LOG10( TW ) / BASL
               ELSE
                  TW = ZERO
               END IF
               DWORK(I+KW4) = DWORK(I+KW4) - TA - TC - TD - TV
               DWORK(J+KW5) = DWORK(J+KW5) - TA - TC - TE - TW
  210       CONTINUE
  220    CONTINUE
C
         IT = 1
C
C        Start generalized conjugate gradient iteration.
C
  230    CONTINUE
C
         GAMMA = ( DDOT( NR, DWORK(ILO+KW4), 1, DWORK(ILO+KW4), 1 ) +
     $             DDOT( NR, DWORK(ILO+KW5), 1, DWORK(ILO+KW5), 1 ) )*
     $           TWO
C
         EW = ZERO
         DO 240 I = ILO, N
            EW = EW + DWORK(I+KW4) + DWORK(I+KW5)
  240    CONTINUE
C
         GAMMA = COEF*GAMMA - TWO*COEF2*EW**2
         IF( GAMMA.EQ.ZERO )
     $      GO TO 310
         IF( IT.NE.1 )
     $      BETA = GAMMA / PGAMMA
         T = -TWO*COEF5*EW
C
         CALL DSCAL( NR, BETA, DWORK(ILO), 1 )
         CALL DSCAL( NR, BETA, DWORK(ILO+KW1), 1 )
C
         CALL DAXPY( NR, COEF, DWORK(ILO+KW4), 1, DWORK(ILO+KW1), 1 )
         CALL DAXPY( NR, COEF, DWORK(ILO+KW5), 1, DWORK(ILO), 1 )
C
         DO 250 J = ILO, N
            DWORK(J)     = DWORK(J)     + T
            DWORK(J+KW1) = DWORK(J+KW1) + T
  250    CONTINUE
C
C        Apply matrix to vector.
C
         DO 270 I = ILO, N
            KOUNT = 0
            SUM   = ZERO
            DO 260 J = ILO, N
               KS = KOUNT
               IF( A(I,J).NE.ZERO )
     $            KOUNT = KOUNT + 1
               IF( C(I,J).NE.ZERO )
     $            KOUNT = KOUNT + 1
               SUM = SUM + DBLE( KOUNT - KS )*DWORK(J)
C
               KS  = KOUNT
               IF( J.GT.I ) THEN
                  IF( DE(I,J+1).NE.ZERO )
     $               KOUNT = KOUNT + 1
               ELSE IF( J.LT.I ) THEN
                  IF( DE(J,I+1).NE.ZERO )
     $               KOUNT = KOUNT + 1
               END IF
               IF( J.GE.I ) THEN
                  IF( VW(I,J+1).NE.ZERO )
     $               KOUNT = KOUNT + 1
               ELSE
                  IF( VW(J,I+1).NE.ZERO )
     $               KOUNT = KOUNT + 1
               END IF
               SUM = SUM + DBLE( KOUNT - KS )*DWORK(J+KW1)
  260       CONTINUE
            DWORK(I+KW2) = DBLE( KOUNT )*DWORK(I+KW1) + SUM
  270    CONTINUE
C
         DO 290 J = ILO, N
            KOUNT = 0
            SUM   = ZERO
            DO 280 I = ILO, N
               KS = KOUNT
               IF( A(I,J).NE.ZERO )
     $            KOUNT = KOUNT + 1
               IF( C(I,J).NE.ZERO )
     $            KOUNT = KOUNT + 1
               SUM = SUM + DBLE( KOUNT - KS )*DWORK(I+KW1)
C
               KS  = KOUNT
               IF( J.GT.I ) THEN
                  IF( DE(J,I).NE.ZERO )
     $               KOUNT = KOUNT + 1
               ELSE IF( J.LT.I ) THEN
                  IF( DE(I,J).NE.ZERO )
     $               KOUNT = KOUNT + 1
               END IF
               IF( J.GE.I ) THEN
                  IF( VW(J,I).NE.ZERO )
     $               KOUNT = KOUNT + 1
               ELSE
                  IF( VW(I,J).NE.ZERO )
     $               KOUNT = KOUNT + 1
               END IF
               SUM = SUM + DBLE( KOUNT - KS )*DWORK(I)
  280       CONTINUE
            DWORK(J+KW3) = DBLE( KOUNT )*DWORK(J) + SUM
  290    CONTINUE
C
         SUM = ( DDOT( NR, DWORK(ILO+KW1), 1, DWORK(ILO+KW2), 1 ) +
     $           DDOT( NR, DWORK(ILO),     1, DWORK(ILO+KW3), 1 ) )*TWO
         ALPHA = GAMMA / SUM
C
C        Determine correction to current iteration.
C
         CMAX = ZERO
         DO 300 I = ILO, N
            COR = ALPHA*DWORK(I+KW1)
            IF( ABS( COR ).GT.CMAX )
     $         CMAX = ABS( COR )
            LSCALE(I) = LSCALE(I) + COR
            COR = ALPHA*DWORK(I)
            IF( ABS( COR ).GT.CMAX )
     $         CMAX = ABS( COR )
            RSCALE(I) = RSCALE(I) + COR
  300    CONTINUE
C
         IF( CMAX.GE.HALF ) THEN
C
            CALL DAXPY( N, -ALPHA, DWORK(ILO+KW2), 1, DWORK(ILO+KW4), 1)
            CALL DAXPY( N, -ALPHA, DWORK(ILO+KW3), 1, DWORK(ILO+KW5), 1)
C
            PGAMMA = GAMMA
            IT = IT + 1
            IF( IT.LE.NRP2 )
     $         GO TO 230
         END IF
C
C        End generalized conjugate gradient iteration.
C
  310    CONTINUE
C
C        Compute diagonal scaling matrices.
C
         DO 320 I = ILO, N
            IRAB = IDAMAX( NR, A(I,ILO), LDA )
            RAB  = ABS( A(I,ILO+IRAB-1) )
            IRAB = IDAMAX( NR, C(I,ILO), LDC )
            RAB  = MAX( RAB, ABS( C(I,ILO+IRAB-1) ) )
            IF( I.GT.ILO ) THEN
               IRAB = IDAMAX( I-1, DE(1,I+1), 1 )
               RAB  = MAX( RAB, ABS( DE(IRAB,I+1) ) )
            END IF
            IF( N.GT.I ) THEN
               IRAB = IDAMAX( N-I, DE(I,I+2), LDDE )
               RAB  = MAX( RAB, ABS( DE(I,I+IRAB+1) ) )
            END IF
            IRAB = IDAMAX( I, VW(1,I+1), 1 )
            RAB  = MAX( RAB, ABS( VW(IRAB,I+1) ) )
            IF( N.GT.I+1 ) THEN
               IRAB = IDAMAX( N-I-1, VW(I,I+2), LDVW )
               RAB  = MAX( RAB, ABS( VW(I,I+IRAB+1) ) )
            END IF
C
            LRAB = INT( LOG10( RAB+SFMIN ) / BASL + ONE )
            IR   = LSCALE(I) + SIGN( HALF, LSCALE(I) )
            IR   = MIN( MAX( IR, LSFMIN ), LSFMAX, LSFMAX-LRAB )
            LSCALE(I) = SCLFAC**IR
C
            ICAB = IDAMAX( N, A(1,I), 1 )
            CAB  = ABS( A(ICAB,I) )
            ICAB = IDAMAX( N, C(1,I), 1 )
            CAB  = MAX( CAB, ABS( C(ICAB,I) ) )
            IF( I.GT.1 ) THEN
               ICAB = IDAMAX( I-1, DE(I,1), LDDE )
               CAB  = MAX( CAB, ABS( DE(I,ICAB) ) )
            END IF
            IF( N.GT.I ) THEN
               ICAB = IDAMAX( N-I, DE(I+1,I), 1 )
               CAB  = MAX( CAB, ABS( DE(I+ICAB,I) ) )
            END IF
            ICAB = IDAMAX( I, VW(I,1), LDVW )
            CAB  = MAX( CAB, ABS( VW(I,ICAB) ) )
            IF( N.GT.I ) THEN
               ICAB = IDAMAX( N-I, VW(I+1,I), 1 )
               CAB  = MAX( CAB, ABS( VW(I+ICAB,I) ) )
            END IF
C
            LRAB = INT( LOG10( CAB+SFMIN ) / BASL + ONE )
            JC = RSCALE(I) + SIGN( HALF, RSCALE(I) )
            JC = MIN( MAX( JC, LSFMIN ), LSFMAX, LSFMAX-LRAB )
            RSCALE(I) = SCLFAC**JC
  320    CONTINUE
C
         DO 330 I = ILO, N
            IF( LSCALE(I).NE.ONE .OR. RSCALE(I).NE.ONE )
     $         GO TO 340
  330    CONTINUE
C
C        Finish the procedure for all scaling factors equal to 1.
C
         NSS = NS0
         NHS = NH0
         THS = TH0
         GO TO 510
C
  340    CONTINUE
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
               T  = MAX( LSCALE(ILO+IR-1), RSCALE(ILO+JC-1) )
               MN = T
               DO 350 I = ILO, N
                  IF( LSCALE(I).LT.MN )
     $               MN = LSCALE(I)
  350          CONTINUE
               DO 360 I = ILO, N
                  IF( RSCALE(I).LT.MN )
     $               MN = RSCALE(I)
  360          CONTINUE
               T = MN/T
               IF( T.LT.ONE/MXCOND ) THEN
                  TH = TH*TEN
                  GO TO 450
               ELSE
                  THS    = TH
                  EVNORM = .TRUE.
                  GO TO 480
               END IF
            END IF
C
C           Compute the 1-norms of the scaled submatrices,
C           without actually scaling them.
C
            NS = ZERO
            DO 380 J = ILO, N
               T = ZERO
               DO 370 I = ILO, N
                  T = T + ABS( A(I,J) )*LSCALE(I)*RSCALE(J)
                  IF( I.LT.J ) THEN
                     T = T + ABS( DE(J,I) )*RSCALE(I)*RSCALE(J)
                  ELSE IF( I.GT.J ) THEN
                     T = T + ABS( DE(I,J) )*RSCALE(I)*RSCALE(J)
                  END IF
  370          CONTINUE
               IF( T.GT.NS )
     $            NS = T
  380       CONTINUE
C
            DO 400 J = ILO, N
               T = ZERO
               DO 390 I = ILO, N
                  T = T + ABS( A(J,I) )*LSCALE(J)*RSCALE(I)
                  IF( I.LT.J ) THEN
                     T = T + ABS( DE(I,J+1) )*LSCALE(I)*LSCALE(J)
                  ELSE IF( I.GT.J ) THEN
                     T = T + ABS( DE(J,I+1) )*LSCALE(I)*LSCALE(J)
                  END IF
  390          CONTINUE
               IF( T.GT.NS )
     $            NS = T
  400       CONTINUE
C
            NH = ZERO
            DO 420 J = ILO, N
               T = ZERO
               DO 410 I = ILO, N
                  T = T + ABS( C(I,J) )*LSCALE(I)*RSCALE(J)
                  IF( I.LE.J ) THEN
                     T = T + ABS( VW(J,I) )*RSCALE(I)*RSCALE(J)
                  ELSE IF( I.GT.J ) THEN
                     T = T + ABS( VW(I,J) )*RSCALE(I)*RSCALE(J)
                  END IF
  410          CONTINUE
               IF( T.GT.NH )
     $            NH = T
  420       CONTINUE
C
            DO 440 J = ILO, N
               T = ZERO
               DO 430 I = ILO, N
                  T = T + ABS( C(J,I) )*LSCALE(J)*RSCALE(I)
                  IF( I.LE.J ) THEN
                     T = T + ABS( VW(I,J+1) )*LSCALE(I)*LSCALE(J)
                  ELSE IF( I.GT.J ) THEN
                     T = T + ABS( VW(J,I+1) )*LSCALE(I)*LSCALE(J)
                  END IF
  430          CONTINUE
               IF( T.GT.NH )
     $            NH = T
  440       CONTINUE
C
            IF( ITH.GE.-4 .AND. ITH.LT.-2 ) THEN
               PROD = ( NS/DENOM )*( NH/DENOM )
               IF( MINPRO.GT.PROD ) THEN
                  MINPRO = PROD
                  STORMN = .TRUE.
                  CALL DCOPY( NR, LSCALE(ILO), 1, DWORK(KW6), 1 )
                  CALL DCOPY( NR, RSCALE(ILO), 1, DWORK(KW7), 1 )
                  NSS = NS
                  NHS = NH
                  THS = TH
               END IF
            ELSE IF( ITH.GE.-2 ) THEN
               IF( NS.LT.NH ) THEN
                  RATIO = MIN( NH/NS, SFMAX )
               ELSE
                  RATIO = MIN( NS/NH, SFMAX )
               END IF
               IF( MINRAT.GT.RATIO ) THEN
                  MINRAT = RATIO
                  STORMN = .TRUE.
                  CALL DCOPY( NR, LSCALE(ILO), 1, DWORK(KW6), 1 )
                  CALL DCOPY( NR, RSCALE(ILO), 1, DWORK(KW7), 1 )
                  MXS = MAX( NS, NH )
                  NSS = NS
                  NHS = NH
                  THS = TH
               END IF
            END IF
            TH = TH*TEN
         END IF
  450 CONTINUE
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
            GO TO 510
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
            T  = MAX( DWORK(KW6+IR-1), DWORK(KW7+JC-1) )
            MN = T
            DO 460 I = KW6, KW6+NR-1
               IF( DWORK(I).LT.MN )
     $            MN = DWORK(I)
  460       CONTINUE
            DO 470 I = KW7, KW7+NR-1
               IF( DWORK(I).LT.MN )
     $            MN = DWORK(I)
  470       CONTINUE
            T = MN/T
            IF( T.LT.ONE/MXCOND ) THEN
               DUM(1) = ONE
               CALL DCOPY( NR, DUM(1), 0, LSCALE(ILO), 1 )
               CALL DCOPY( NR, DUM(1), 0, RSCALE(ILO), 1 )
               IWARN = 1
               NSS = NS0
               NHS = NH0
               THS = TH0
               GO TO 510
            END IF
         END IF
         IF( STORMN ) THEN
            CALL DCOPY( NR, DWORK(KW6), 1, LSCALE(ILO), 1 )
            CALL DCOPY( NR, DWORK(KW7), 1, RSCALE(ILO), 1 )
         ELSE
            NSS = NS
            NHS = NH
            THS = TH
         END IF
      END IF
C
  480 CONTINUE
C
C     Row scaling.
C
      DO 490 I = ILO, N
         CALL DSCAL(  NR, LSCALE(I), A(I,ILO),  LDA )
         CALL DSCAL(  NR, LSCALE(I), C(I,ILO),  LDC )
         CALL DSCAL( I-1, LSCALE(I), DE(1,I+1), 1   )
         IF( N.GT.I )
     $      CALL DSCAL( N-I, LSCALE(I), DE(I,I+2), LDDE )
         CALL DSCAL(      I, LSCALE(I), VW(1,I+1), 1    )
         CALL DSCAL(  N-I+1, LSCALE(I), VW(I,I+1), LDVW )
  490 CONTINUE
C
C     Column scaling.
C
      DO 500 J = ILO, N
         CALL DSCAL(   N, RSCALE(J), A(1,J),  1    )
         CALL DSCAL(   N, RSCALE(J), C(1,J),  1    )
         CALL DSCAL( J-1, RSCALE(J), DE(J,1), LDDE )
         IF( N.GT.J )
     $      CALL DSCAL( N-J, RSCALE(J), DE(J+1,J), 1    )
         CALL DSCAL(      J, RSCALE(J), VW(J,1),   LDVW )
         CALL DSCAL(  N-J+1, RSCALE(J), VW(J,J),   1    )
  500 CONTINUE
C
C     Set DWORK(1:5).
C
  510 CONTINUE
      IF( EVNORM ) THEN
         NSS = MA02ID( 'skew-Hamiltonian', '1-norm', NR, A(ILO,ILO),
     $                  LDA, DE(ILO,ILO), LDDE, DWORK )
         NHS = MA02ID( 'Hamiltonian', '1-norm', NR, C(ILO,ILO), LDC,
     $                 VW(ILO,ILO), LDVW, DWORK )
      END IF
C
      DWORK(1) = NS0
      DWORK(2) = NH0
      DWORK(3) = NSS
      DWORK(4) = NHS
      IF( LOOP ) THEN
         DWORK(5) = THS/MAX( MXNORM, SFMIN )
      ELSE
         DWORK(5) = THRESH
      END IF
C
      RETURN
C *** Last line of MB04DP ***
      END
