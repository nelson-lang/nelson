      SUBROUTINE SG02CX( JOBE, FLAG, JOBG, UPLO, TRANS, N, M, E, LDE, R,
     $                   LDR, S, LDS, G, LDG, ALPHA, RNORM, DWORK,
     $                   LDWORK, IWARN, INFO )
C
C     PURPOSE
C
C     To find the line search parameter alpha minimizing the Frobenius
C     norm (in a MATLAB-style notation)
C
C        P(alpha) := norm(R(X+alpha*S), 'fro')
C                  = norm((1-alpha)*R(X) +/- alpha^2*V, 'fro'),
C
C     where R(X) is the residual of a (generalized) continuous-time
C     algebraic Riccati equation
C
C        0 = op(A)'*X + X*op(A) +/- X*G*X + Q  =:  R(X),
C     or
C        0 = op(A)'*X*op(E) + op(E)'*X*op(A) +/- op(E)'*X*G*X*op(E) + Q
C          =:  R(X),
C
C     V = op(E)'*S*G*S*op(E), and op(W) is either W or W'. The matrix S
C     is the Newton step.
C                                                     _-1
C     Instead of the symmetric N-by-N matrix G, G = B*R  *B', the N-by-M
C                      -1
C     matrix D, D = B*L  , such that G = D*D', may be given on entry.
C                _  _
C     The matrix R, R = L'*L, is a weighting matrix of the optimal
C     problem, and L is its (Cholesky) factor.
C
C     Optionally, V is specified as V = H*K, or V = F*F', but F or H and
C     K must be evaluated in S. See the SLICOT Library routine SG02CW
C     description for more details.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOBE    CHARACTER*1
C             Specifies whether E is a general or an identity matrix,
C             as follows:
C             = 'G':  The matrix E is general and is given;
C             = 'I':  The matrix E is assumed identity and is not given.
C
C     FLAG    CHARACTER*1
C             Specifies which sign is used, as follows:
C             = 'P':  The plus sign is used;
C             = 'M':  The minus sign is used.
C
C     JOBG    CHARACTER*1
C             Specifies how the matrix product V is defined, as follows:
C             = 'G':  The matrix G is given:  V = op(E)'*S*G*S*op(E);
C             = 'D':  The matrix D is given:  V = op(E)'*S*D*D'*S*op(E);
C             = 'F':  The matrix F is given:           V = F*F';
C             = 'H':  The matrices H and K are given:  V = H*K.
C
C     UPLO    CHARACTER*1
C             Specifies which triangles of the symmetric matrices R, G,
C             if JOBG = 'G', and S, if JOBG = 'G' or JOBG = 'D', are
C             given, as follows:
C             = 'U':  The upper triangular part is given;
C             = 'L':  The lower triangular part is given.
C
C     TRANS   CHARACTER*1
C             Specifies the form of op(W) to be used in the matrix
C             multiplication, as follows:
C             = 'N':  op(W) = W;
C             = 'T':  op(W) = W';
C             = 'C':  op(W) = W'.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrices E, R, and S.  N >= 0.
C
C     M       (input) INTEGER
C             If JOBG <> 'G', the number of columns of the matrices D,
C             F, or K'.  M >= 0.
C             If JOBG = 'G', the value of M is meaningless.
C
C     E       (input) DOUBLE PRECISION array, dimension (LDE,*)
C             If JOBE = 'G' and (JOBG = 'G' or JOBG = 'D'), the leading
C             N-by-N part of this array must contain the matrix E.
C             If JOBE = 'I' or JOBG = 'F' or JOBG = 'H', this array is
C             not referenced.
C
C     LDE     INTEGER
C             The leading dimension of array E.
C             LDE >= MAX(1,N), if JOBE = 'G' and (JOBG = 'G' or
C                                                 JOBG = 'D');
C             LDE >= 1,        if JOBE = 'I'  or  JOBG = 'F' or
C                                                 JOBG = 'H'.
C
C     R       (input) DOUBLE PRECISION array, dimension (LDR,N)
C             The leading N-by-N upper or lower triangular part
C             (depending on UPLO) of this array must contain the upper
C             or lower triangular part, respectively, of the matrix
C             R(X), the residual of the algebraic Riccati equation.
C             The other strictly triangular part is not referenced.
C
C     LDR     INTEGER
C             The leading dimension of array R.  LDR >= MAX(1,N).
C
C     S       (input) DOUBLE PRECISION array, dimension (LDS,*)
C             If JOBG = 'G' or JOBG = 'D', the leading N-by-N part of
C             this array must contain the symmetric Newton step
C             matrix S. If JOBE = 'I', the full matrix must be given.
C             Otherwise, it is sufficient to input only the triangular
C             part specified by UPLO, and the remaining strictly
C             triangular part is not referenced.
C             If JOBG = 'F', this array is not referenced.
C             If JOBG = 'H', the leading M-by-N part of this array must
C             contain the matrix K.
C
C     LDS     INTEGER
C             The leading dimension of array S.
C             LDS >= MAX(1,N), if JOBG =  'G' or JOBG =  'D';
C             LDS >= 1,        if JOBG =  'F';
C             LDS >= MAX(1,M), if JOBG =  'H'.
C
C     G       (input/works.) DOUBLE PRECISION array, dimension (LDG,*)
C             If JOBG = 'G', the leading N-by-N upper or lower
C             triangular part (depending on UPLO) of this array must
C             contain the upper or lower triangular part, respectively,
C             of the matrix G. The other strictly triangular part is not
C             referenced. The diagonal elements of this array are
C             modified internally, but are restored on exit.
C             If JOBG = 'D', the leading N-by-M part of this array must
C             contain the matrix D, so that G = D*D'.
C             If JOBG = 'F', the leading N-by-M part of this array must
C             contain the matrix F.
C             If JOBG = 'H', leading N-by-M part of this array must
C             contain the matrix H.
C
C     LDG     INTEGER
C             The leading dimension of array G.  LDG >= MAX(1,N).
C
C     ALPHA   (output) DOUBLE PRECISION
C             If INFO = 0, ALPHA contains the real number alpha which
C             minimizes  P(alpha) = norm(R(X+alpha*S), 'fro') in the
C             interval [0,2].
C             If INFO = 1 or IWARN = 2, ALPHA is set equal to 1.
C
C     RNORM   (output) DOUBLE PRECISION
C             On exit, if INFO >= 0, RNORM contains the Frobenius norm
C             of the residual R(X+alpha*S).
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if LDWORK = -1 on input, then DWORK(1) returns
C             the optimal value of LDWORK.
C             On exit, if LDWORK = -2 on input or INFO = -19, then
C             DWORK(1) returns the minimal value of LDWORK.
C             On exit, if INFO = 0, the leading N-by-N upper or lower
C             triangular part (depending on UPLO) of this array contains
C             the corresponding triangular part of the matrix V.
C
C     LDWORK  The length of the array DWORK.
C             LDWORK >= N*N + MAX( 2*N*N, 51 ),
C                                       if JOBG = 'G' and JOBE = 'G';
C             LDWORK >= N*N + MAX( N*N, 51 ),
C                                       if JOBG = 'G' and JOBE = 'I',
C                                       or JOBG = 'F', or JOBG = 'H';
C             LDWORK >= N*N + MAX( MAX( N*N, 51 ), MIN( 2*N*N, N*M ) ),
C                                       if JOBG = 'D' and JOBE = 'G';
C             LDWORK >= N*N + MAX( N*N, N*M, 51 ),
C                                       if JOBG = 'D' and JOBE = 'I'.
C             For M <= N, the last two formulas simplify to
C             LDWORK >= N*N + MAX( N*N, 51).
C
C             If LDWORK = -1, an optimal workspace query is assumed; the
C             routine only calculates the optimal size of the DWORK
C             array, returns this value as the first entry of the DWORK
C             array, and no error message is issued by XERBLA.
C
C             If LDWORK = -2, a minimal workspace query is assumed; the
C             routine only calculates the minimal size of the DWORK
C             array, returns this value as the first entry of the DWORK
C             array, and no error message is issued by XERBLA.
C
C     Warning Indicator
C
C     IWARN   INTEGER
C             = 0:  no warnings;
C             = 2:  no optimal line search parameter t := alpha in [0,2]
C                   was found; t = 1 was set.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -k, the k-th argument had an illegal
C                   value;
C             = 1:  an error occurred during the call of the SLICOT
C                   Library routine MC01XD: the eigenvalues computation
C                   for the 3-by-3 (generalized) eigenproblem failed.
C                   ALPHA and RNORM are set as specified above.
C
C     METHOD
C
C     The matrix V is computed with the suitable formula, and used to
C     set up a third order polynomial whose roots in [0,2], if any, are
C     candidates for the solution of the minimum residual problem.
C     The roots of the polynomial are computed by solving an equivalent
C     3-by-3 (generalized) eigenproblem.
C
C     REFERENCES
C
C     [1] Benner, P.
C         Contributions to the Numerical Solution of Algebraic
C         Riccati Equations and Related Eigenvalue Problems.
C         Fakultat fur Mathematik, Technische Universitat Chemnitz-
C         Zwickau, D-09107 Chemnitz, Germany, Feb. 1997.
C
C     NUMERICAL ASPECTS
C
C     The calculations are backward stable. The computational effort is
C     of the order of c*N**2*M operations. Here, M = N, if JOBG = 'G',
C     and the coefficient c varies between 0.5 and 2.5, depending on
C     JOBE and JOBG. (An "operation" includes a multiplication, an
C     addition, and some address calculations.)
C     The computed value of norm(R(X+alpha*S),'fro'), returned in RNORM,
C     could be inaccurate if R(X) is small, since then subtraction
C     cancellation could appear in the updating formula which is used.
C     (This can happen, e.g., when solving a Riccati equation by
C     Newton's method with line search, since then the sequence of R(.)
C     tends to zero.) In such a case, it is better to recompute the
C     residual from the data.
C
C     FURTHER COMMENTS
C
C     The routine does not ckeck if the matrix S is zero, when a quick
C     return with ALPHA = 1 is possible.
C     With suitable input arguments E and G, this routine may also be
C     used for discrete-time algebraic Riccati equations. (Matrix E
C     must be the current closed-loop matrix.)
C
C     CONTRIBUTORS
C
C     M. Slowik, Institut fur Mathematik, TU Berlin, July 2005.
C     P. Benner, Institut fur Mathematik, TU Chemnitz, July 2005.
C     V. Sima, Research Institute for Informatics, Bucharest, Nov. 2005.
C
C     REVISIONS
C
C     V. Sima, Jan. 2013, Feb. 2013, Jun. 2013, Dec. 2013, Feb. 2014.
C
C     KEYWORDS
C
C     Algebraic Riccati equation, eigenvalues, generalized eigenproblem.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE, TWO, THREE, FOUR, SIX
      PARAMETER         ( ZERO  = 0.0D0, ONE  = 1.0D0, TWO = 2.0D0,
     $                    THREE = 3.0D0, FOUR = 4.0D0, SIX = 6.0D0 )
C     .. Scalar Arguments ..
      CHARACTER         FLAG, JOBE, JOBG, TRANS, UPLO
      INTEGER           INFO, IWARN, LDE, LDG, LDR, LDS, LDWORK, M, N
      DOUBLE PRECISION  ALPHA, RNORM
C     .. Array Arguments ..
      DOUBLE PRECISION  DWORK(*), E(LDE,*), G(LDG,*), R(LDR,*), S(LDS,*)
C     .. Local Scalars ..
      CHARACTER         NT, NTRANS, SIDE, TR
      LOGICAL           LCND, LFLAG, LJOBE, LJOBF, LJOBG, LJOBH, LJOBL,
     $                  LQUERY, LTRANS, LUPLO, USE1, WWT
      INTEGER           CRITNR, EVIPOS, EVQPOS, EVRPOS, I, J, NM, NMIN,
     $                  NN, NOPT, RPOS, SP
      DOUBLE PRECISION  BETA, DELTA, GAMMA, MX, PA, PB, PC, VNORM
C     .. Local Arrays ..
      DOUBLE PRECISION  CRD(2), CRN(2)
C     .. External Functions ..
      LOGICAL           LSAME
      DOUBLE PRECISION  DDOT, DLANSY
      EXTERNAL          DDOT, DLANSY, LSAME
C     .. External Subroutines ..
      EXTERNAL          DAXPY, DGEMM, DLACPY, DLASCL, DSCAL, DSYMM,
     $                  DSYRK, MB01RB, MB01RU, MC01XD, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, MAX, MIN
C     .. Executable Statements ..
C
C     Test the input scalar arguments.
C
      IWARN = 0
      INFO  = 0
      NN    = N*N
C
      LJOBE  = LSAME( JOBE,  'G' )
      LFLAG  = LSAME( FLAG,  'M' )
      LJOBG  = LSAME( JOBG,  'G' )
      LJOBF  = LSAME( JOBG,  'F' )
      LJOBH  = LSAME( JOBG,  'H' )
      LUPLO  = LSAME( UPLO,  'U' )
      LTRANS = LSAME( TRANS, 'T' ) .OR. LSAME( TRANS, 'C' )
      LJOBL  = LJOBF .OR. LJOBH
C
      IF (      .NOT.LJOBE  .AND. .NOT.LSAME( JOBE,  'I' ) ) THEN
         INFO = -1
      ELSE IF ( .NOT.LFLAG  .AND. .NOT.LSAME( FLAG,  'P' ) ) THEN
         INFO = -2
      ELSE IF ( .NOT.LJOBG  .AND. .NOT.LSAME( JOBG,  'D' ) .AND. 
     $          .NOT.LJOBL ) THEN
         INFO = -3
      ELSE IF ( .NOT.LUPLO  .AND. .NOT.LSAME( UPLO,  'L' ) ) THEN
         INFO = -4
      ELSE IF ( .NOT.LTRANS .AND. .NOT.LSAME( TRANS, 'N' ) ) THEN
         INFO = -5
      ELSE IF ( N.LT.0 ) THEN
         INFO = -6
      ELSE IF ( .NOT.LJOBG .AND. M.LT.0 ) THEN
         INFO = -7
      ELSE IF ( LDE.LT.1 .OR. ( LJOBE .AND. .NOT.LJOBL .AND. LDE.LT.N )
     $        ) THEN
         INFO = -9
      ELSE IF ( LDR.LT.MAX( 1, N ) ) THEN
         INFO = -11
      ELSE IF ( LDS.LT.1 .OR. ( LJOBH .AND. LDS.LT.M ) .OR.
     $                   ( .NOT.LJOBL .AND. LDS.LT.N ) ) THEN
         INFO = -13
      ELSE IF ( LDG.LT.MAX( 1, N ) ) THEN
         INFO = -15
      ELSE
C
C        Check the needed size of the workspace.
C
         LQUERY = LDWORK.EQ.-1
         IF ( LJOBL ) THEN
            NMIN = NN + MAX( NN, 51 )
            IF ( LQUERY )
     $         NOPT = NMIN
         ELSE IF ( LJOBG ) THEN
            IF ( LJOBE ) THEN
               NMIN = NN + MAX( 2*NN, 51 )
            ELSE
               NMIN = NN + MAX( NN, 51 )
            END IF
            IF ( LQUERY )
     $         NOPT = NMIN
         ELSE
            NM = N*M
            IF ( LJOBE ) THEN
               NMIN = NN + MAX( MAX( NN, 51 ), MIN( 2*NN, NM ) )
               LCND = 2*M.GT.3*N
               IF ( LQUERY ) THEN
                  IF ( LCND ) THEN
                     NOPT = 3*NN
                  ELSE
                     NOPT = NN + NM
                  END IF
               END IF
            ELSE
               NMIN = NN + MAX( NN, NM, 51 )
               LCND = M.GT.3*N
               IF ( LQUERY ) THEN
                  IF ( LCND ) THEN
                     NOPT = 2*NN
                  ELSE
                     NOPT = NN + NM
                  END IF
               END IF
            END IF
            IF ( LQUERY )
     $         NOPT = MAX( NOPT, NMIN )
         END IF
         IF ( LQUERY ) THEN
C
C           The array DWORK should have at least 3 entries here.
C
            CALL MC01XD( ALPHA, BETA, GAMMA, DELTA, DWORK, DWORK, DWORK,
     $                   DWORK, -1, INFO )
            NOPT = MAX( NOPT, NN + 9 + INT( DWORK(1) ) )
            DWORK(1) = NOPT
            RETURN
         ELSE IF ( LDWORK.EQ.-2 ) THEN
            DWORK(1) = NMIN
            RETURN
         ELSE IF ( LDWORK.LT.NMIN ) THEN
            INFO = -19
            DWORK(1) = NMIN
         END IF
      END IF
C
      IF ( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'SG02CX', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF ( N.EQ.0 .OR. ( .NOT.LJOBG .AND. M.EQ.0 ) ) THEN
         ALPHA = ONE
         RNORM = ZERO
         RETURN
      END IF
C
      RNORM = DLANSY( 'F-norm', UPLO, N, R, LDR, DWORK )
      IF ( RNORM.EQ.ZERO ) THEN
         ALPHA = ZERO
         RETURN
      END IF
C
C     Initialize scalar arguments for the called routines.
C
      NT = 'No transpose'
      IF ( LJOBE ) THEN
         TR = 'Transpose'
         IF ( LTRANS ) THEN
            SIDE   = 'Right'
            NTRANS = NT
         ELSE
            SIDE   = 'Left'
            NTRANS = TR
         END IF
      END IF
C
C     Initialize DWORK positions.
C
      SP = NN + 1
C
C     Compute the matrix
C
C       V = op(E)'*S*G*S*op(E), or
C       V = op(E)'*S*D*D'*S*op(E), or
C       V = F*F', or V = H*K.
C
      IF ( LJOBL ) THEN
C
C        Compute F*F' or H*K.
C
C        Workspace: N*N.
C
         IF ( LJOBF ) THEN
            CALL DSYRK( UPLO, NT, N, M, ONE, G, LDG, ZERO, DWORK, N )
         ELSE
            CALL MB01RB( 'Left', UPLO, NT, N, M, ZERO, ONE, DWORK, N, G,
     $                   LDG, S, LDS, INFO )
        END IF
C
      ELSE IF ( LJOBG ) THEN
         IF ( LJOBE ) THEN
C
C           Compute the following steps, if TRANS = 'N'
C           1.  W = S*E       (stored in DWORK(SP));
C           2.  V = W'*G*W    (stored in DWORK).
C
C           Compute the following steps, if TRANS = 'T'
C           1.  W = E*S       (stored in DWORK(SP));
C           2.  V = W*G*W'    (stored in DWORK).
C
C           Workspace: 3*N*N.
C
            CALL DSYMM(  SIDE, UPLO, N, N, ONE, S, LDS, E, LDE,
     $                   ZERO, DWORK(SP), N )
            CALL MB01RU( UPLO, NTRANS, N, N, ZERO, ONE, DWORK, N,
     $                   DWORK(SP), N, G, LDG, DWORK(SP+NN), NN, INFO )
         ELSE
C
C           Compute V = S*G*S    (stored in DWORK).
C
C           Workspace: 2*N*N.
C
            CALL MB01RU( UPLO, NT, N, N, ZERO, ONE, DWORK, N, S, LDS, G,
     $                   LDG, DWORK(SP), NN, INFO )
         END IF
C
      ELSE
C
         WWT = N.GE.M
         IF ( LJOBE ) THEN
            USE1 = LCND .AND. LDWORK.GE.3*NN
C
            IF ( USE1 ) THEN
C
C              Compute
C                 Y = D*D'                 in DWORK,
C                 W = S*E or W = E*S       in DWORK(SP),
C                 V = W'*Y*W or V = W*Y*W' in DWORK.
C
C              Workspace: 3*N*N.
C
               CALL DSYMM(  SIDE, UPLO, N, N, ONE, S, LDS, E, LDE, ZERO,
     $                      DWORK(SP), N )
               CALL DSYRK(  UPLO, NT, N, M, ONE, G, LDG, ZERO, DWORK,
     $                      N )
               CALL MB01RU( UPLO, NTRANS, N, N, ZERO, ONE, DWORK, N,
     $                      DWORK(SP), N, DWORK, N, DWORK(SP+NN), NN,
     $                      INFO )
               CALL DSCAL(  N, ONE/TWO, DWORK, N+1 )
C
            ELSE IF ( WWT ) THEN
C
C              Compute W = S*D               (stored in DWORK);
C              Compute W = E'*W or W = E*W   (stored in DWORK(SP)).
C
C              Workspace (including for W*W'):  N*N + N*M.
C
               CALL DSYMM( 'Left', UPLO, N, M, ONE, S, LDS, G, LDG,
     $                     ZERO, DWORK, N )
C
               CALL DGEMM( NTRANS, NT, N, M, N, ONE, E, LDE, DWORK, N,
     $                     ZERO, DWORK(SP), N )
            ELSE
C
C              Compute W = S*E  or W = E*S   (stored in DWORK);
C              Compute W = D'*W or W = W*D   (stored in DWORK(SP)).
C
C              Workspace (including for W'*W):  N*N + N*M.
C
               CALL DSYMM( SIDE, UPLO, N, N, ONE, S, LDS, E, LDE,
     $                     ZERO, DWORK, N )
C
               IF ( LTRANS ) THEN
                  CALL DGEMM( NT, NT, N, M, N, ONE, DWORK, N, G, LDG,
     $                        ZERO, DWORK(SP), N )
               ELSE
                  CALL DGEMM( TR, NT, M, N, N, ONE, G, LDG, DWORK, N,
     $                        ZERO, DWORK(SP), M )
               END IF
            END IF
C
         ELSE
            USE1 = LCND .OR. LDWORK.LT.NN + NM
C
            IF ( USE1 ) THEN
C
C              Compute
C                 Y = D*D'   in DWORK,
C                 W = S*Y*S  in DWORK.
C
C              Workspace: 2*N*N.
C
               CALL DSYRK(  UPLO, NT, N, M, ONE, G, LDG, ZERO, DWORK,
     $                      N )
               CALL MB01RU( UPLO, NT, N, N, ZERO, ONE, DWORK, N, S, LDS,
     $                      DWORK, N, DWORK(SP), NN, INFO )
               CALL DSCAL(  N, ONE/TWO, DWORK, N+1 )
C
            ELSE
C
C              Compute W = S*D        (stored in DWORK(SP)).
C
C              Workspace (including for W*W'): N*M + N*N.
C
               CALL DSYMM( 'Left', UPLO, N, M, ONE, S, LDS, G, LDG,
     $                     ZERO, DWORK(SP), N )
            END IF
         END IF
C
C        Compute V = W*W' or V = W'*W       (stored in DWORK).
C
         IF ( .NOT.USE1 ) THEN
            IF ( WWT .OR. .NOT.LJOBE .OR. LTRANS ) THEN
               CALL DSYRK( UPLO, NT, N, M, ONE, DWORK(SP), N, ZERO,
     $                     DWORK, N )
            ELSE
               CALL DSYRK( UPLO, NTRANS, N, M, ONE, DWORK(SP), M, ZERO,
     $                     DWORK, N )
            END IF
         END IF
      END IF
C
C     Initialize scalar values.
C
      PB = ZERO
      CRITNR = 0
C
C     Compute the parameters alpha, beta, gamma.
C
      VNORM = DLANSY( 'F-norm', UPLO, N, DWORK, N, DWORK )
C
      MX = MAX( ONE, RNORM, VNORM )
      PA = ( RNORM/MX )*( RNORM/MX )
C
      I = 1
C
      IF ( LUPLO ) THEN
C
         DO 10 J = 1, N
            PB = PB + TWO*DDOT( J-1, R(1,J), 1, DWORK(I), 1 )/MX/MX +
     $           ( R(J,J)/MX )*( DWORK(I+J-1)/MX )
            I  = I + N
   10    CONTINUE
C
      ELSE
C
         DO 20 J = 1, N - 1
            PB = PB + ( R(J,J)/MX )*( DWORK(I)/MX ) +
     $           TWO*DDOT( N-J, R(J+1,J), 1, DWORK(I+1), 1 )/MX/MX
            I  = I + N + 1
   20    CONTINUE
C
         PB = PB + ( R(N,N)/MX )*( DWORK(I)/MX )
      END IF
C
C     Compute the coefficients of the derivative polynomial.
C
      ALPHA = -TWO*PA
      IF ( LFLAG ) THEN
         BETA  =  TWO*( PA - TWO*PB )
         GAMMA =  SIX*PB
      ELSE
         BETA  =  TWO*( PA + TWO*PB )
         GAMMA = -SIX*PB
      END IF
      DELTA = FOUR*( VNORM/MX )*( VNORM/MX )
C
C     Compute the roots of the polynomial.
C
C     Workspace: N*N + 51.
C
      EVRPOS = SP
      EVIPOS = EVRPOS + 3
      EVQPOS = EVIPOS + 3
      RPOS   = EVQPOS + 3
C
      CALL MC01XD( ALPHA, BETA, GAMMA, DELTA, DWORK(EVRPOS),
     $             DWORK(EVIPOS), DWORK(EVQPOS), DWORK(RPOS),
     $             LDWORK-RPOS+1, INFO )
C
      IF ( INFO.NE.0 ) THEN
         INFO  = 1
         ALPHA = ONE
         RNORM = VNORM
         RETURN
      END IF
C
C     Find the global minimum of the polynomial in [0,2], if it exists.
C
      DO 30 J = 0, 2
C
C        Check if EVR(J+1)/EVQ(J+1) is a critical value.
C
         IF ( DWORK(EVIPOS+J).EQ.ZERO )  THEN
C
C           Check if EVR(J+1)/EVQ(J+1) is in [0,2].
C
            PA = DWORK(EVRPOS+J)
            PB = DWORK(EVQPOS+J)
            IF ( PA.GE.ZERO .AND. PB.GT.ZERO .AND. PA.LE.TWO*PB ) THEN
C
C              Check the second derivative (min or max).
C
               MX = MAX( ABS( BETA ), ABS( GAMMA ), DELTA, ABS( PA ),
     $                   ABS( PB ) )
               IF ( MX.GT.ZERO ) THEN
                  PA = PA/MX
                  PB = PB/MX
                  PC = ( BETA/MX )*PB*PB + TWO*( GAMMA/MX )*PA*PB +
     $                                   THREE*( DELTA/MX )*PA*PA
                  IF ( PC.GT.ZERO ) THEN
                     CRITNR = CRITNR + 1
                     CRN(CRITNR) = DWORK(EVRPOS+J)
                     CRD(CRITNR) = DWORK(EVQPOS+J)
                  END IF
               END IF
            END IF
         END IF
   30 CONTINUE
C
      IF ( CRITNR.EQ.0 ) THEN
C
C        No minimum is found in [0,2].
C
         IWARN = 2
         ALPHA = ONE
         RNORM = VNORM
         RETURN
      END IF
C
C     Compute the norm of R(X+ALPHA*S).
C
C     Workspace: 2*N*N.
C
      ALPHA = CRN(1)/CRD(1)
      PA    = ONE - ALPHA
C
      CALL DLACPY( UPLO, N, N, R, LDR, DWORK(SP), N )
      CALL DLASCL( UPLO, 1, 1, ONE, PA, N, N, DWORK(SP), N, INFO )
C
      IF ( LFLAG ) THEN
         PA = -ALPHA*ALPHA
      ELSE
         PA =  ALPHA*ALPHA
      END IF
      I  = 0
C
      IF ( LUPLO ) THEN
C
         DO 40 J = 1, N
            CALL DAXPY( J, PA, DWORK(I+1), 1, DWORK(SP+I), 1 )
            I = I + N
   40    CONTINUE
C
      ELSE
C
         DO 50 J = 1, N
            CALL DAXPY( N-J+1, PA, DWORK(I+1), 1, DWORK(SP+I), 1 )
            I = I + N + 1
   50    CONTINUE
C
      END IF
C
      RNORM = DLANSY( 'F-norm', UPLO, N, DWORK(SP), N, DWORK )
C
C     If two local minima are found in [0,2], choose the one for which
C     the norm of R(X+t*S) is minimal.
C
      IF ( CRITNR.EQ.2 ) THEN
         BETA = CRN(2)/CRD(2)
         PB   = ONE - BETA
C
         CALL DLACPY( UPLO, N, N, R, LDR, DWORK(SP), N )
         CALL DLASCL( UPLO, 1, 1, ONE, PB, N, N, DWORK(SP), N, INFO )
C
         IF ( LFLAG ) THEN
            PB = -BETA*BETA
         ELSE
            PB =  BETA*BETA
         END IF
         I  = 0
C
         IF ( LUPLO ) THEN
C
            DO 60 J = 1, N
               CALL DAXPY( J, PB, DWORK(I+1), 1, DWORK(SP+I), 1 )
               I = I + N
   60       CONTINUE
C
         ELSE
C
            DO 70 J = 1, N
               CALL DAXPY( N-J+1, PB, DWORK(I+1), 1, DWORK(SP+I), 1 )
               I = I + N + 1
   70       CONTINUE
C
         END IF
C
         VNORM = DLANSY( 'F-norm', UPLO, N, DWORK(SP), N, DWORK )
C
         IF ( VNORM.LT.RNORM ) THEN
            ALPHA = BETA
            RNORM = VNORM
         END IF
      END IF
C
      RETURN
C *** Last line of SG02CX ***
      END
