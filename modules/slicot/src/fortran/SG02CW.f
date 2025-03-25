      SUBROUTINE SG02CW( DICO, JOB, JOBE, FLAG, JOBG, UPLO, TRANS, N, M,
     $                   A, LDA, E, LDE, G, LDG, X, LDX, F, LDF, K, LDK,
     $                   XE, LDXE, R, LDR, C, LDC, NORMS, DWORK, LDWORK,
     $                   INFO )
C
C     PURPOSE
C
C     To compute the residual matrix R for a continuous-time or
C     discrete-time Riccati equation and/or the "closed-loop system"
C     matrix op(C), using the formulas
C
C        R = op(A)'*X + X*op(A) +/- X*G*X + Q,
C        C = op(A) +/- G*X,
C     or
C        R = op(A)'*X*op(E) + op(E)'*X*op(A) +/- op(E)'*X*G*X*op(E) + Q,
C        C = op(A) +/- G*X*op(E),
C     or
C        R = op(A)'*X*op(E) + op(E)'*X*op(A) +/- H*K + Q,
C        C = op(A) +/- B*K,
C
C     in the continuous-time case, or the formulas
C
C        R = op(A)'*X*op(A) - X +/- op(A)'*X*G*X*op(A) + Q,
C        C = op(A) +/- G*X*op(A),
C     or
C        R = op(A)'*X*op(A) - op(E)'*X*op(E) +/- op(A)'*X*G*X*op(A) + Q,
C        C = op(A) +/- G*X*op(A),
C     or
C        R = op(A)'*X*op(A) - op(E)'*X*op(E) +/- H*K + Q,
C        C = op(A) +/- B*K,
C
C     in the discrete-time case, where X, G, and Q are symmetric
C     matrices, A, E, H, K, B are general matrices, and op(W) is one of
C
C        op(W) = W   or   op(W) = W'.
C                                                     _-1
C     Instead of the symmetric N-by-N matrix G, G = B*R  *B', the N-by-M
C                     _-1
C     matrix D, D = B*L  , such that G = D*D', may be given on entry.
C                _       _   _  _
C     The matrix R, with R = L'*L, is a weighting matrix of the optimal
C                                      _            _
C     problem, if DICO = 'C', or it is R = B'*X*B + Rd, if DICO = 'D',
C          _                              _                         _
C     with Rd a similar weighting matrix; L is a Cholesky factor of R,
C        _                          _
C     if R is positive definite. If R is not positive definite, which
C     may happen in the discrete-time case, a UdU' or LdL' factorization
C     is used to compute the matrices H and K. If M = 0, the residual
C     matrix of a (generalized) Lyapunov or Stein equation is computed.
C     To this end, set JOBG = 'D' and JOB = 'R' (since op(C) = A in this
C     case).
C
C     Optionally, the quadratic term in the formulas for R is specified
C     as H*K, where
C
C        H = L + op(E)'*X*B,  if DICO = 'C', or
C        H = L + op(A)'*X*B,  if DICO = 'D', and
C            _-1
C        K = R *H',
C
C     with L an N-by-M matrix. This is useful, e.g., for DICO = 'D',
C                        _                     _
C     when L <> 0 and/or Rd is singular, hence R might be numerically
C     indefinite; it might be indefinite in the first iterations of
C     Newton's algorithm. Depending on JOB, part or all of the matrices
C     H, K, and B should be given in such a case.
C        _
C     If R is positive definite, the quadratic term can be specified
C     as F*F', and the second term in the formulas for C is D*F', where
C              _-1
C        F = H*L .
C
C     The matrices F and/or D should be given. This option is not useful
C     when L = 0, unless F and D are available. If DICO = 'C', the
C     computational problem with L <> 0 is equivalent with one with
C     L = 0 after replacing
C                   _-1                 _-1
C        A := A - B*R* L',   Q := Q - L*R* L'.
C                          _             _
C     These formulas, with R replaced by Rd, can also be used in the
C                            _
C     discrete-time case, if Rd is nonsingular and well-conditioned with
C     respect to inversion.
C
C     Optionally, the Frobenius norms of the product terms defining the
C     denominator of the relative residual are also computed. The norms
C     of Q and X are not computed.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     DICO    CHARACTER*1
C             Specifies the type of the Riccati equation, as follows:
C             = 'C':  continuous-time algebraic Riccati equation;
C             = 'D':  discrete-time algebraic Riccati equation.
C
C     JOB     CHARACTER*1
C             Specifies which results must be computed, as follows:
C             = 'A':  Both (all) matrices R and C must be computed;
C             = 'R':  The matrix R only must be computed;
C             = 'C':  The matrix C only must be computed;
C             = 'N':  The matrices R and C and the norms must be
C                     computed;
C             = 'B':  The matrix R and the norms must be computed.
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
C             Specifies how the quadratic term in the formulas for R is
C             defined, as follows:
C             = 'G':  The matrix G is given;
C             = 'D':  The matrix D is given;
C             = 'F':  The matrix F is given;
C             = 'H':  The matrices H and K are given.
C
C     UPLO    CHARACTER*1
C             Specifies which triangles of the symmetric matrices X, G
C             (if JOBG = 'G'), and Q (if JOB <> 'C') are given, as
C             follows:
C             = 'U':  The upper triangular part is given;
C             = 'L':  The lower triangular part is given.
C
C     TRANS   CHARACTER*1
C             Specifies the form of op(W) to be used in the formulas
C             above, as follows:
C             = 'N':  op(W) = W;
C             = 'T':  op(W) = W';
C             = 'C':  op(W) = W'.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrices A, E, Q, X, C and R.  N >= 0.
C
C     M       (input) INTEGER
C             If JOBG <> 'G', the number of columns of the matrices D,
C             F, and/or B, H, and K'.  M >= 0.
C             If JOBG = 'G', the value of M is meaningless.
C
C     A       (input) DOUBLE PRECISION array, dimension (LDA,N)
C             The leading N-by-N part of this array must contain the
C             matrix A.
C
C     LDA     INTEGER
C             The leading dimension of array A.  LDA >= MAX(1,N).
C
C     E       (input) DOUBLE PRECISION array, dimension (LDE,*)
C             If JOBE = 'G' and (JOB <> 'C' or (DICO = 'C' and
C             (JOBG = 'G' or JOBG = 'D'))), the leading N-by-N part of
C             this array must contain the matrix E.
C             If JOBE = 'I' or (JOB = 'C' and (DICO = 'D' or
C             JOBG = 'F' or JOBG = 'H')), this array is not referenced.
C
C     LDE     INTEGER
C             The leading dimension of array E.
C             LDE >= MAX(1,N), if JOBE = 'G' and (JOB <> 'C' or
C                                (DICO = 'C' and (JOBG = 'G' or
C                                                 JOBG = 'D')));
C             LDE >= 1,        if JOBE = 'I'  or (JOB  = 'C' and
C                                (DICO = 'D'  or  JOBG = 'F' or
C                                                 JOBG = 'H')).
C
C     G       (input/works.) DOUBLE PRECISION array, dimension (LDG,*)
C             If JOBG = 'G', the leading N-by-N upper or lower
C             triangular part (depending on UPLO) of this array must
C             contain the upper or lower triangular part, respectively,
C             of the matrix G. The other strictly triangular part is not
C             referenced. If DICO = 'D', (JOB = 'R' or JOB = 'B'), and
C             JOBG = 'G', the diagonal elements of this array are
C             modified internally, but are restored on exit.
C             If JOBG = 'D' or (JOBG = 'F' and JOB <> 'R' and
C             JOB <> 'B'), the leading N-by-M part of this array must
C             contain the matrix D, so that G = D*D'.
C             If JOBG = 'H' and JOB <> 'R' and JOB <> 'B', the leading
C             N-by-M part of this array must contain the matrix B.
C             If (JOBG = 'F' or JOBG = 'H') and JOB = 'R' or JOB = 'B',
C             this array is not referenced.
C
C     LDG     INTEGER
C             The leading dimension of array G.
C             LDG >= MAX(1,N), if  JOBG = 'G' or  JOBG = 'D' or
C                                 (JOB <> 'R' and JOB <> 'B');
C             LDG >= 1,        if (JOBG = 'F' or JOBG = 'H') and
C                                 (JOB  = 'R' or JOB  = 'B').
C
C     X       (input/works.) DOUBLE PRECISION array, dimension (LDX,N)
C             The leading N-by-N part of this array must contain the
C             symmetric matrix X, and it is unchanged on exit.
C             If DICO = 'D', JOBE = 'G' and JOB <> 'C', the diagonal
C             elements of this array are modified internally, but they
C             are restored on exit.
C             The full matrix X should be input if DICO = 'C',
C             JOBE = 'I', and the conditions in the lines of the table
C             below are satisfied
C
C                JOBG     JOB             LDWORK
C             ----------------------------------------------
C               'F','H'  'A','R'          LDWORK <   N*N
C                 'G'    'A','R','N'      LDWORK < 2*N*N
C                 'G'      'C'            LDWORK <   N*N
C                 'G'      'B'            LDWORK < 3*N*N
C                 'D'      'R'     (M<=N, LDWORK <   N*N) or
C                                  (M> N, LDWORK < 3*N*N)
C                 'D'      'A'     (M<=N, LDWORK <   N*N) or
C                                        (LDWORK >=  N*N and
C                                         LDWORK < 2*N*N)
C             ----------------------------------------------
C
C             For all the other cases, including when the optimal length
C             of the workspace array DWORK is used, only the relevant
C             upper or lower triangular part (depending on UPLO) of this
C             array must be input, and the other strictly triangular
C             part is not referenced.
C
C     LDX     INTEGER
C             The leading dimension of array X.  LDX >= MAX(1,N).
C
C     F       (input) DOUBLE PRECISION array, dimension (LDF,*)
C             If JOBG = 'F', the leading N-by-M part of this array must
C             contain the matrix F.
C             If JOBG = 'H', the leading N-by-M part of this array must
C             contain the matrix H.
C             If JOBG = 'G' or JOBG = 'D', this array is not referenced.
C
C     LDF     INTEGER
C             The leading dimension of array F.
C             LDF >= MAX(1,N), if JOBG = 'F' or JOBG = 'H';
C             LDF >= 1,        if JOBG = 'G' or JOBG = 'D'.
C
C     K       (input) DOUBLE PRECISION array, dimension (LDK,*)
C             If JOBG = 'H', the leading M-by-N part of this array must
C             contain the matrix K.
C             If JOBG <> 'H', this array is not referenced.
C
C     LDK     INTEGER
C             The leading dimension of array K.
C             LDK >= MAX(1,M), if JOBG =  'H';
C             LDK >= 1,        if JOBG <> 'H'.
C
C     XE      (input) DOUBLE PRECISION array, dimension (LDXE,*)
C             If (JOBG = 'F' or JOBG = 'H'), JOB <> 'C', DICO = 'C', and
C             JOBE = 'G', the leading N-by-N part of this array must
C             contain the matrix product X*E, if TRANS = 'N', or E*X, if
C             TRANS = 'T' or 'C'.
C             If (JOBG = 'F' or JOBG = 'H'), JOB <> 'C', and DICO = 'D',
C             the leading N-by-N part of this array must contain the
C             matrix product X*A, if TRANS = 'N', or A*X, if TRANS = 'T'
C             or 'C'.
C             These matrix products are needed for computing F or H.
C             If JOBG = 'G' or JOBG = 'D' or JOB = 'C' or (DICO = 'C'
C             and JOBE = 'I') this array is not referenced.
C
C     LDXE    INTEGER
C             The leading dimension of array XE.
C             LDXE >= MAX(1,N), if (JOBG = 'F' or JOBG = 'H'),
C                               JOB <> 'C', and either DICO = 'C' and
C                               JOBE = 'G', or DICO = 'D';
C             LDXE >= 1,        if JOBG = 'G' or JOBG = 'D' or JOB = 'C'
C                               or (DICO = 'C' and JOBE = 'I').
C
C     R       (input/output) DOUBLE PRECISION array, dimension (LDR,*)
C             On entry, if JOB <> 'C', the leading N-by-N upper or lower
C             triangular part (depending on UPLO) of this array must
C             contain the upper or lower triangular part, respectively,
C             of the matrix Q. The other strictly triangular part is not
C             referenced.
C             On exit, if JOB <> 'C' and INFO = 0, the leading N-by-N
C             upper or lower triangular part (depending on UPLO) of this
C             array contains the upper or lower triangular part,
C             respectively, of the matrix R.
C             If JOB = 'C', this array is not referenced.
C
C     LDR     INTEGER
C             The leading dimension of array R.
C             LDR >= MAX(1,N), if JOB <> 'C';
C             LDR >= 1,        if JOB =  'C'.
C
C     C       (output) DOUBLE PRECISION array, dimension (LDC,*)
C             If JOB <> 'R' and JOB <> 'B' and INFO = 0, the leading
C             N-by-N part of this array contains the matrix op(C).
C             If JOB = 'R' or JOB = 'B', this array is not referenced.
C
C     LDC     INTEGER
C             The leading dimension of array C.
C             LDC >= MAX(1,N), if JOB <> 'R' and JOB <> 'B';
C             LDC >= 1,        if JOB =  'R' or  JOB =  'B'.
C
C     NORMS   (output) DOUBLE PRECISION array, dimension (LN)
C             If JOB = 'N' or JOB = 'B', LN = 2 or 3, if (DICO = 'C' or
C             JOBE = 'I'), or (DICO = 'D' and JOBE = 'G'), respectively.
C             If DICO = 'C',
C             NORMS(1) contains the Frobenius norm of the matrix
C             op(A)'*X (or of X*op(A)), if JOBE = 'I', or of the matrix
C             op(A)'*X*op(E) (or of op(E)'*X*op(A)), if JOBE = 'G';
C             NORMS(2) contains the Frobenius norm of the matrix
C             product X*G*X, if JOBE = 'I', or of the matrix product
C             V = op(E)'*X*G*X*op(E), if JOBE = 'G' (for JOBG = 'G' or
C             JOBG = 'D'), or of V = F*F', if JOBG = 'F', or of V = H*K,
C             if JOBG = 'H'.
C             If DICO = 'D',
C             NORMS(1) contains the Frobenius norm of the matrix
C             op(A)'*X*op(A);
C             NORMS(2) contains the Frobenius norm of the matrix product
C             V = op(A)'*X*G*X*op(A), if JOBG = 'G' or JOBG = 'D', or of
C             V = F*F', if JOBG = 'F', or of V = H*K, if JOBG = 'H';
C             if JOBE = 'G', NORMS(3) contains the Frobenius norm of the
C             matrix product op(E)'*X*op(E).
C             If JOB <> 'N' and JOB <> 'B', this array is not
C             referenced.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = -30, or if LDWORK = -2 on input, then
C             DWORK(1) returns the minimum value of LDWORK.
C             On exit, if INFO = 0, or if LDWORK = -1 on input, then
C             DWORK(1) returns the optimal value of LDWORK.
C
C     LDWORK  The length of the array DWORK. LDWORK >= MAX(v,1), with v
C             specified in the following table, where
C                a = 1, if JOBE = 'G';
C                a = 0, if JOBE = 'I'.
C
C             DICO     JOBG     JOB             v
C             -----------------------------------------------
C             'C'    'F','H' 'A','C','R'         0
C             'C'    'F','H'    'N'           a*N*N
C             'C'    'F','H'    'B'             N*N
C             'C'      'G'    'A','C'         a*N*N
C             'C'      'G'    'N','R'     (a+1)*N*N
C             'C'      'G'      'B'       (a+2)*N*N
C             'C'      'D'      'A'       N*MIN(M,(a+1)*N)
C             'C'      'D'      'C'         N*MIN(N,M)
C             'C'      'D'      'N'       N*(N+MIN(a*N,M))
C             'C'      'D'      'B'     N*(N+MIN(N+a*N,M))
C             'C'      'D'      'R'     N*MIN(a*N+M,(a+2)*N)
C             -----------------------------------------------
C             'D'    'F','H'  'A','C'           0
C             'D'    'F','H'  'N','R'         a*N*N
C             'D'    'F','H'    'B'       (a+1)*N*N
C             'D'      'G'    'A','C'           N*N
C             'D'      'G'    'N','R'         2*N*N
C             'D'      'G'      'B'           3*N*N
C             'D'      'D'    'A','N'   N*MIN(MAX(N,M),2*N)
C             'D'      'D'      'B'      N*(N+MAX(N,M))
C             'D'      'D'      'C'         N*MIN(N,M)
C             'D'      'D'      'R'       N*MIN(3*N,N+M)
C             -----------------------------------------------
C
C             If LDWORK = -1, an optimal workspace query is assumed; the
C             routine only calculates the optimal size of the DWORK
C             array, returns this value as the first entry of the DWORK
C             array, and no error message is issued by XERBLA.
C             This evaluation assumes that only the specified triangle
C             of the array X is always used, and the other strict
C             triangle is not referenced.
C
C             If LDWORK = -2, a minimal workspace query is assumed; the
C             routine only calculates the minimal size of the DWORK
C             array, returns this value as the first entry of the DWORK
C             array, and no error message is issued by XERBLA.
C             This evaluation assumes that full matrix is given in the
C             array X, when needed (see the table at the description of
C             the array X).
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
C     The matrix expressions are efficiently evaluated, using symmetry,
C     common matrix subexpressions, and proper order of matrix
C     multiplications.
C     If JOB = 'N' or JOB = 'B', then:
C     If DICO = 'C', the matrices op(op(A)'*X*op(E)) or op(X*op(A)), and
C     V = op(E)'*X*G*X*op(E) or V = F*F' or V = H*K, are efficiently
C     computed.
C     If DICO = 'D', the matrices op(A)'*X*op(A), V = op(A)'*X*G*X*op(A)
C     or V = F*F' or V = H*K, and op(E)'*X*op(E), if JOBE = 'G', are
C     efficiently computed. The results are used to evaluate R, op(C)
C     (if JOB = 'N'), and the norms.
C     If JOB <> 'N', then the needed parts of the intermediate results
C     are obtained and used to evaluate R and/or op(C).
C
C     NUMERICAL ASPECTS
C
C     The calculations are backward stable.
C                                                3     2
C     The algorithm requires approximately (a+b)N  + cN M operations,
C     where,
C       a = 0,    if JOBE = 'I',
C       a = 1,    if JOBE = 'G' and (DICO = 'C' or
C                                   (DICO = 'D' and JOB = 'C')),
C       a = 1.5,  if JOBE = 'G' and  DICO = 'D',
C     and b and c are implicitly defined below. Specifically, the effort
C     is approximately as follows (using ^ to denote the power operator)
C
C     For DICO = 'C':
C
C     JOBG                      JOB
C               C                R                 A, N, B
C     'G'  (a+1)*N^3    (a+2)*N^3              (a+2)*N^3,(a+2.5)*N^3
C     'D'  (a+2)*N^2*M  (a+1)*N^3+1.5*N^2*M    (a+1)*N^3+2.5*N^2*M
C     'F','H'    N^2*M        N^3+0.5*N^2*M        N^3+1.5*N^2*M
C
C     For DICO = 'D':
C
C     JOBG                      JOB
C               C                R                 A, N, B
C     'G'      2*N^3    (a+3  )*N^3            (a+3  )*N^3
C     'D'      3*N^2*M  (a+1.5)*N^3+1.5*N^2*M  (a+1.5)*N^3+2.5*N^2*M
C     'F','H'  N^2*M    (a+0.5)*N^3+0.5*N^2*M  (a+0.5)*N^3+1.5*N^2*M
C
C     For JOBG <> 'G' and JOB = 'B', the effort reduces by N^2*M in
C     both tables.
C
C     An "operation" includes a multiplication, an addition, and some
C     address calculations.
C
C     CONTRIBUTORS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Feb. 2013.
C
C     REVISIONS
C
C     V. Sima, Mar. 2013, Sep.-Dec. 2013, June 2014, May 2015.
C
C     KEYWORDS
C
C     Algebraic Riccati equation, elementary matrix operations, matrix
C     algebra, matrix operations.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE, TWO
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0 )
C     .. Scalar Arguments ..
      CHARACTER         DICO, FLAG, JOB, JOBE, JOBG, TRANS, UPLO
      INTEGER           INFO, LDA, LDC, LDE, LDF, LDG, LDK, LDR, LDWORK,
     $                  LDX, LDXE, M, N
C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), C(LDC,*), DWORK(*), E(LDE,*),
     $                  F(LDF,*), G(LDG,*), K(LDK,*), NORMS(*),
     $                  R(LDR,*), X(LDX,*), XE(LDXE,*)
C     .. Local Scalars ..
      CHARACTER         NSIDE, NT, NTRANS, SIDE, TR
      LOGICAL           DISCR, FULLX, KEEPX, LCNDS, LCNDT, LCOND, LFLAG,
     $                  LJOBA, LJOBB, LJOBC, LJOBE, LJOBF, LJOBG, LJOBH,
     $                  LJOBL, LJOBN, LJOBR, LTRANS, LUPLO, NLJOBC,
     $                  NLJOBR, UNITE, USE1, USEATW, USEC, USEOPT,
     $                  WITHD, WITHE
      INTEGER           I, IA, IB, IW, J, L, MINWRK, NM, NN, OPTWRK, WP,
     $                  YP
      DOUBLE PRECISION  ALPHA, BETA
C     .. External Functions ..
      LOGICAL           LSAME
      DOUBLE PRECISION  DLANGE, DLANSY
      EXTERNAL          DLANGE, DLANSY, LSAME
C     .. External Subroutines ..
      EXTERNAL          DAXPY, DCOPY, DGEMM, DLACPY, DSCAL, DSYMM,
     $                  DSYR2K, DSYRK, MA02ED, MB01RB, MB01RU, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         MAX, MIN
C     .. Executable Statements ..
C
C     Test the input scalar arguments.
C
      INFO = 0
C
      DISCR  = LSAME( DICO,  'D' )
      LJOBA  = LSAME( JOB,   'A' )
      LJOBC  = LSAME( JOB,   'C' )
      LJOBN  = LSAME( JOB,   'N' )
      LJOBB  = LSAME( JOB,   'B' )
      LJOBR  = LSAME( JOB,   'R' )
      LJOBE  = LSAME( JOBE,  'G' )
      LFLAG  = LSAME( FLAG,  'M' )
      LJOBG  = LSAME( JOBG,  'G' )
      LJOBF  = LSAME( JOBG,  'F' )
      LJOBH  = LSAME( JOBG,  'H' )
      LUPLO  = LSAME( UPLO,  'U' )
      LTRANS = LSAME( TRANS, 'T' ) .OR. LSAME( TRANS, 'C' )
      LJOBL  = LJOBF .OR. LJOBH
      NLJOBC = .NOT.LJOBC
      NLJOBR = .NOT.LJOBR .AND. .NOT.LJOBB
      WITHD  = .NOT.LJOBL .OR. NLJOBR
      UNITE  = .NOT.LJOBE
      WITHE  = LJOBE .AND. ( NLJOBC .OR. .NOT. ( DISCR .OR. LJOBL ) )
C
      IF (     .NOT.DISCR  .AND. .NOT.LSAME( DICO,  'C' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.LJOBA  .AND.  NLJOBC .AND.
     $         .NOT.LJOBN  .AND.  NLJOBR ) THEN
         INFO = -2
      ELSE IF(      UNITE  .AND. .NOT.LSAME( JOBE,  'I' ) ) THEN
         INFO = -3
      ELSE IF( .NOT.LFLAG  .AND. .NOT.LSAME( FLAG,  'P' ) ) THEN
         INFO = -4
      ELSE IF( .NOT.LJOBG  .AND. .NOT.LSAME( JOBG,  'D' ) .AND.
     $         .NOT.LJOBL ) THEN
         INFO = -5
      ELSE IF( .NOT.LUPLO  .AND. .NOT.LSAME( UPLO,  'L' ) ) THEN
         INFO = -6
      ELSE IF( .NOT.LTRANS .AND. .NOT.LSAME( TRANS, 'N' ) ) THEN
         INFO = -7
      ELSE IF( N.LT.0 ) THEN
         INFO = -8
      ELSE IF( .NOT.LJOBG .AND. M.LT.0 ) THEN
         INFO = -9
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -11
      ELSE IF( LDE.LT.1  .OR. ( WITHE .AND. LDE.LT.N ) ) THEN
         INFO = -13
      ELSE IF( LDG.LT.1  .OR. ( WITHD .AND. LDG.LT.N ) ) THEN
         INFO = -15
      ELSE IF( LDX.LT.MAX( 1, N ) ) THEN
         INFO = -17
      ELSE IF( LDF.LT.1  .OR. ( LJOBL .AND. LDF.LT.N ) ) THEN
         INFO = -19
      ELSE IF( LDK.LT.1  .OR. ( LJOBH .AND. LDK.LT.M ) ) THEN
         INFO = -21
      ELSE IF( LDXE.LT.1 .OR. ( LJOBL .AND. NLJOBC .AND. LDXE.LT.N .AND.
     $      ( DISCR .OR. ( .NOT.DISCR .AND. LJOBE ) ) ) ) THEN
         INFO = -23
      ELSE IF( LDR.LT.1  .OR. ( NLJOBC .AND. LDR.LT.N ) ) THEN
         INFO = -25
      ELSE IF( LDC.LT.1  .OR. ( NLJOBR .AND. LDC.LT.N ) ) THEN
         INFO = -27
      ELSE
         NN = N*N
         IF( .NOT.LJOBG )
     $      NM = N*M
         IF ( LJOBE .OR. DISCR ) THEN
            IA = 1
         ELSE
            IA = 0
         END IF
C
         IF ( LJOBN .OR. LJOBB ) THEN
            IF ( LJOBB ) THEN
               IB = 1
            ELSE
               IB = 0
            END IF
            IF ( LJOBL ) THEN
               IF ( LJOBB .AND. DISCR ) THEN
                  MINWRK = ( IA + 1 )*NN
               ELSE IF ( LJOBE .OR. LJOBB ) THEN
                  MINWRK = NN
               ELSE
                  MINWRK = 0
               END IF
               OPTWRK = MINWRK
               USE1   = .FALSE.
            ELSE IF ( LJOBG ) THEN
               MINWRK = ( IA + IB + 1 )*NN
               OPTWRK = ( IB + 2 )*NN
               USE1   = .FALSE.
            ELSE
               IF ( LJOBN ) THEN
                  LCNDS = 4*M.LE.3*N
               ELSE
                  LCNDS = 2*M.LE.3*N
               END IF
               IF ( DISCR ) THEN
                  IF ( LJOBN ) THEN
                     MINWRK = MIN( MAX( NN, NM ), 2*NN )
                  ELSE
                     MINWRK = NN + MIN( MAX( NN, NM ), 2*NN )
                  END IF
                  IF ( LCNDS ) THEN
                     IF ( LJOBN ) THEN
                        OPTWRK = MAX( NN, NM )
                     ELSE
                        OPTWRK = NN + MAX( NN, NM )
                     END IF
                  ELSE
                     OPTWRK = ( IB + 2 )*NN
                  END IF
                  IF ( LJOBB )
     $               OPTWRK = MAX( OPTWRK, MINWRK )
                  USE1 = LCNDS .OR. LDWORK.LT.( IB + 2 )*NN
               ELSE
                  I = NN + MAX( IA*IB*NN, NM )
                  J = NN + ( IA + IB )*NN
                  MINWRK = MIN( I, J )
                  IF ( LCNDS ) THEN
                     OPTWRK = I
                  ELSE
                     OPTWRK = ( IA + IB + 1 )*NN
                  END IF
                  USE1 = LCNDS .AND. LDWORK.GE.I
                  IF ( LJOBN ) THEN
                     USE1 = LJOBE .AND. ( LCNDS .OR. LDWORK.LT.OPTWRK )
     $                 .OR. UNITE .AND. USE1
                  ELSE
                     USE1 = USE1 .OR. LDWORK.LT.( IA + IB + 1 )*NN
                  END IF
               END IF
            END IF
C
         ELSE
C
            LCOND = ( 3 + 2*IA )*M.LE.( 2 + 2*IA )*N
C
            IF ( DISCR ) THEN
               IF ( LJOBL ) THEN
                  IF ( LJOBE .AND. LJOBR ) THEN
                     MINWRK = NN
                  ELSE
                     MINWRK = 0
                  END IF
                  OPTWRK = MINWRK
               ELSE
                  LCNDS = 4*M.LE.3*N
                  LCNDT = 2*M.LE.3*N
                  IF ( LJOBG ) THEN
                     IF ( LJOBR ) THEN
                        MINWRK = 2*NN
                     ELSE
                        MINWRK = NN
                     END IF
                     OPTWRK = MINWRK
                     USE1 = .FALSE.
                  ELSE
                     IF ( LJOBC ) THEN
                        MINWRK = MIN( NN, NM )
                        IF ( LCOND ) THEN
                           OPTWRK = NM
                        ELSE
                           OPTWRK = 2*NN
                        END IF
                        USE1 = LCOND .OR. LDWORK.LT.NN
                     ELSE IF ( LJOBR ) THEN
                        MINWRK = MIN( NN + NM, 3*NN )
                        IF ( LCNDT ) THEN
                           OPTWRK = NN + NM
                        ELSE
                           OPTWRK = 3*NN
                        END IF
                        USE1 = LCNDT .OR. LDWORK.LT.OPTWRK
                     ELSE
                        MINWRK = MIN( MAX( NN, NM ), 2*NN )
                        IF ( LCNDS ) THEN
                           OPTWRK = MINWRK
                        ELSE
                           OPTWRK = 2*NN
                        END IF
                        USE1 = LCNDS .OR. LDWORK.LT.OPTWRK
                     END IF
                  END IF
               END IF
C
            ELSE
C
               IF ( LJOBL ) THEN
                  MINWRK = 0
                  IF ( UNITE .AND. LJOBR ) THEN
                     OPTWRK = NN
                  ELSE
                     OPTWRK = 0
                  END IF
                  USE1 = .FALSE.
               ELSE
                  LCNDS = 3*M.LE.2*N
                  LCNDT =   M.LE.N
                  IF ( LJOBG ) THEN
                     IF ( LJOBR ) THEN
                        MINWRK = ( IA + 1 )*NN
                        OPTWRK = 2*NN
                     ELSE
                        MINWRK = IA*NN
                        IF ( LJOBC ) THEN
                           OPTWRK = NN
                        ELSE
                           OPTWRK = 2*NN
                        END IF
                     END IF
                     USE1 = .FALSE.
                  ELSE
                     IF ( LJOBC ) THEN
                        MINWRK = MIN( NN, NM )
                        IF ( LCOND ) THEN
                           OPTWRK = NM
                        ELSE
                           OPTWRK = NN
                        END IF
                        USE1 = LCOND .OR. LDWORK.LT.OPTWRK
                     ELSE IF ( LJOBR ) THEN
                        MINWRK = MIN( IA*NN + NM, ( IA + 2 )*NN )
                        IF ( LCNDT ) THEN
                           OPTWRK = NN + IA*NM
                        ELSE
                           OPTWRK = 3*NN
                        END IF
                        USE1 = LCNDT .OR. LDWORK.LT.( IA + 2 )*NN
                     ELSE
                        MINWRK = MIN( NM, ( IA + 1 )*NN )
                        OPTWRK = 2*NN
                        USE1   = MINWRK.EQ.NM .AND. LDWORK.LT.OPTWRK
                     END IF
                  END IF
               END IF
C
               IF ( NLJOBC ) THEN
                  USEATW = USE1 .OR. M.EQ.0
                  IF ( LJOBA )
     $               USEATW = USEATW .OR.
     $                        ( LJOBG .AND. LDWORK.LT.OPTWRK )
               ELSE
                  USEATW = .FALSE.
               END IF
            END IF
         END IF
C
         IF ( LDWORK.EQ.-2 ) THEN
            DWORK(1) = MAX( 1, MINWRK )
            RETURN
         ELSE IF ( LDWORK.EQ.-1 ) THEN
            DWORK(1) = MAX( 1, OPTWRK )
            RETURN
         END IF
C
         IF( LDWORK.LT.MINWRK ) THEN
            INFO = -30
            DWORK(1) = MAX( 1, MINWRK )
         END IF
      END IF
C
      IF ( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'SG02CW', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF ( N.EQ.0 ) THEN
         IF ( LJOBN .OR. LJOBB ) THEN
            NORMS(1) = ZERO
            NORMS(2) = ZERO
            IF ( DISCR .AND. LJOBE )
     $         NORMS(3) = ZERO
         END IF
         RETURN
      END IF
C
C     Initialize DWORK positions.
C
      IF ( LJOBN .OR. LJOBB ) THEN
         IF ( DISCR .OR. LJOBE .OR. LJOBG .OR. USE1 ) THEN
            WP = NN + 1
         ELSE
            WP = 1
         END IF
         IF ( .NOT.( DISCR .OR. LJOBE ) .AND. LJOBG ) THEN
            IF ( LJOBN ) THEN
               KEEPX = LDWORK.GE.2*NN
            ELSE
               KEEPX = LDWORK.GE.3*NN
            END IF
         ELSE
            KEEPX = .FALSE.
         END IF
      ELSE IF ( DISCR ) THEN
         WP = NN + 1
         IF ( LJOBC .AND. .NOT.LJOBG .AND. .NOT.LCOND ) THEN
            USEOPT = LDWORK.GE.2*NN
         ELSE
            USEOPT = .FALSE.
         END IF
      ELSE IF ( LJOBL .OR. LJOBC ) THEN
         WP    = 1
         KEEPX = UNITE .AND. LDWORK.GE.OPTWRK
      ELSE
         IF ( LJOBG ) THEN
            KEEPX = UNITE .AND. ( LDWORK.GE.OPTWRK .OR.
     $            ( LJOBA .AND.   LDWORK.GE.NN ) )
            IF ( LDWORK.LT.2*NN ) THEN
               WP = 1
            ELSE
               WP = NN + 1
            END IF
         ELSE
            IF ( LDWORK.LT.OPTWRK ) THEN
               KEEPX = .FALSE.
               IF ( LJOBR ) THEN
                  WP = IA*NM + 1
                  YP = WP + NN
               ELSE
                  WP = NN + 1
               END IF
            ELSE
               KEEPX = .TRUE.
               IF ( LJOBA ) THEN
                  WP = NN + 1
               ELSE
                  IF ( USE1 ) THEN
                     WP = IA*NM + 1
                  ELSE
                     WP = NN + 1
                     YP = WP + NN
                  END IF
               END IF
            END IF
         END IF
      END IF
C
      IF ( LFLAG ) THEN
         BETA = -ONE
      ELSE
         BETA =  ONE
      END IF
C
      NT = 'No transpose'
      TR = 'Transpose'
      IF ( LTRANS ) THEN
         SIDE   = 'Right'
         NSIDE  = 'Left'
         NTRANS = NT
      ELSE
         SIDE   = 'Left'
         NSIDE  = 'Right'
         NTRANS = TR
      END IF
C
      IF ( LJOBN ) THEN
C
C        JOB = 'N'.
C
         IF ( DISCR ) THEN
C
            IF ( LJOBE ) THEN
C
C              Compute op(E)'*X*op(E) in C and its norm.
C
C              Workspace: N*N.
C
               CALL MB01RU( UPLO, NTRANS, N, N, ZERO, ONE, C, LDC, E,
     $                      LDE, X, LDX, DWORK, NN, INFO )
C
               NORMS(3) = DLANSY( 'F-norm', UPLO, N, C, LDC, DWORK )
C
C              Compute Q - op(E)'*X*op(E) in R.
C
               IF ( LUPLO ) THEN
C
                  DO 10 J = 1, N
                     CALL DAXPY( J, -ONE, C(1,J), 1, R(1,J), 1 )
   10             CONTINUE
C
               ELSE
C
                  DO 20 J = 1, N
                     CALL DAXPY( N-J+1, -ONE, C(J,J), 1, R(J,J), 1 )
   20             CONTINUE
C
               END IF
C
            ELSE
C
C              Compute Q - X in R.
C
               IF ( LUPLO ) THEN
C
                  DO 30 J = 1, N
                     CALL DAXPY( J, -ONE, X(1,J), 1, R(1,J), 1 )
   30             CONTINUE
C
               ELSE
C
                  DO 40 J = 1, N
                     CALL DAXPY( N-J+1, -ONE, X(J,J), 1, R(J,J), 1 )
   40             CONTINUE
C
               END IF
C
            END IF
C
C           Compute op(A)'*X*op(A) and its norm.
C
            IF ( LJOBL ) THEN
C
C              Compute in C the symmetric matrix
C                 T = A'*XE,  if TRANS = 'N';
C                 T = XE*A',  if TRANS = 'T'.
C
               CALL MB01RB( SIDE, UPLO, TR, N, N, ZERO, ONE, C, LDC, A,
     $                      LDA, XE, LDXE, INFO )
            ELSE
C
C              First, compute in DWORK
C                 W = X*A,  if TRANS = 'N';
C                 W = A*X,  if TRANS = 'T'.
C
C              Workspace: N*N.
C
               CALL DSYMM( SIDE, UPLO, N, N, ONE, X, LDX, A, LDA, ZERO,
     $                     DWORK, N )
C
C              Compute in C the symmetric matrix
C                 T = A'*W,  if TRANS = 'N';
C                 T = W*A',  if TRANS = 'T'.
C
               CALL MB01RB( SIDE, UPLO, TR, N, N, ZERO, ONE, C, LDC, A,
     $                      LDA, DWORK, N, INFO )
            END IF
C
            NORMS(1) = DLANSY( 'F-norm', UPLO, N, C, LDC, DWORK )
C
C           Update R := R + op(A)'*X*op(A).
C
            IF ( LUPLO ) THEN
C
               DO 50 J = 1, N
                  CALL DAXPY( J, ONE, C(1,J), 1, R(1,J), 1 )
   50          CONTINUE
C
            ELSE
C
               DO 60 J = 1, N
                  CALL DAXPY( N-J+1, ONE, C(J,J), 1, R(J,J), 1 )
   60          CONTINUE
C
            END IF
C
            IF ( LJOBL ) THEN
C
C              Compute F*F' or H*K in C and its norm.
C
               IF ( LJOBF ) THEN
C
                  CALL DSYRK( UPLO, NT, N, M, ONE, F, LDF, ZERO, C,
     $                        LDC )
C
               ELSE
C
                  CALL MB01RB( 'Left', UPLO, NT, N, M, ZERO, ONE, C,
     $                         LDC, F, LDF, K, LDK, INFO )
               END IF
C
               NORMS(2) = DLANSY( 'F-norm', UPLO, N, C, LDC, DWORK )
C
C              Update R := R +/- F*F' or R := R +/- H*K.
C
               IF ( LUPLO ) THEN
C
                  DO 70 J = 1, N
                     CALL DAXPY( J, BETA, C(1,J), 1, R(1,J), 1 )
   70             CONTINUE
C
               ELSE
C
                  DO 80 J = 1, N
                     CALL DAXPY( N-J+1, BETA, C(J,J), 1, R(J,J), 1 )
   80             CONTINUE
C
               END IF
C
C              Compute op(C) = A +/- op(D*F') or op(C) = A +/- op(B*K).
C
               CALL DLACPY( 'All', N, N, A, LDA, C, LDC )
C
               IF ( LJOBF ) THEN
                  IF ( LTRANS ) THEN
                     CALL DGEMM( NT, TR, N, N, M, BETA, F, LDF, G, LDG,
     $                           ONE, C, LDC )
                  ELSE
                     CALL DGEMM( NT, TR, N, N, M, BETA, G, LDG, F, LDF,
     $                           ONE, C, LDC )
                  END IF
C
               ELSE
                  IF ( LTRANS ) THEN
                     CALL DGEMM( TR, TR, N, N, M, BETA, K, LDK, G, LDG,
     $                           ONE, C, LDC )
                  ELSE
                     CALL DGEMM( NT, NT, N, N, M, BETA, G, LDG, K, LDK,
     $                           ONE, C, LDC )
                  END IF
C
               END IF
C
            ELSE IF ( LJOBG ) THEN
C
C              Compute V = op(A)'*X*G*X*op(A) and its norm.
C              First, compute in C the following product:
C                 Z = +/- G*W  ( TRANS = 'N' ) or
C                 Z = +/- W*G  ( TRANS = 'T' ).
C              Then, compute V in DWORK(WP), with
C                 V = W'*Z ( TRANS = 'N' )
C                 V = Z*W' ( TRANS = 'T' ).
C
C              Workspace: 2*N*N.
C
               CALL DSYMM(  SIDE, UPLO, N, N, BETA, G, LDG, DWORK, N,
     $                      ZERO, C, LDC )
               CALL MB01RB( SIDE, UPLO, TR, N, N, ZERO, ONE, DWORK(WP),
     $                      N, DWORK, N, C, LDC, INFO )
C
               NORMS(2) = DLANSY( 'F-norm', UPLO, N, DWORK(WP), N, DWORK
     $                          )
C
C              Update R := R +/- op(A)'*X*G*X*op(A) = R + V.
C
               I = WP
               IF ( LUPLO ) THEN
C
                  DO 90 J = 1, N
                     CALL DAXPY( J, ONE, DWORK(I), 1, R(1,J), 1 )
                     I = I + N
   90             CONTINUE
C
               ELSE
C
                  DO 100 J = 1, N
                     CALL DAXPY( N-J+1, ONE, DWORK(I), 1, R(J,J), 1 )
                     I = I + N + 1
  100             CONTINUE
C
               END IF
C
C              Compute
C                 op(C) = A +/- G*W, if TRANS = 'N';
C                 op(C) = A +/- W*G, if TRANS = 'T'.
C
               DO 110 J = 1, N
                  CALL DAXPY( N, ONE, A(1,J), 1, C(1,J), 1 )
  110          CONTINUE
C
            ELSE IF ( M.GT.0 ) THEN
C
               IF ( USE1 ) THEN
C
C                 To reduce memory requirements, save W in C.
C                 Compute S = D'*W or S = W*D   (stored in DWORK).
C                 Then, compute
C                    V = op(A)'*X*D*D'*X*op(A) (i.e., S'*S or S*S')
C                 in C, and compute its norm; D in stored in array G.
C
C                 Workspace: MAX(N*N,N*M).
C
                  CALL DLACPY( 'All', N, N, DWORK, N, C, LDC )
                  IF ( LTRANS ) THEN
                     CALL DGEMM( NT, NT, N, M, N, ONE, C, LDC, G, LDG,
     $                           ZERO, DWORK, N )
                     CALL DSYRK( UPLO, NT, N, M, ONE, DWORK, N, ZERO,
     $                           C, LDC )
                  ELSE
                     CALL DGEMM( TR, NT, M, N, N, ONE, G, LDG, C, LDC,
     $                           ZERO, DWORK, M )
                     CALL DSYRK( UPLO, TR, N, M, ONE, DWORK, M, ZERO,
     $                           C, LDC )
                  END IF
C
                  NORMS(2) = DLANSY( 'F-norm', UPLO, N, C, LDC, DWORK )
C
C                 Update R := R +/- op(A)'*X*D*D'*X*op(A) = R +/- V, and
C                 compute
C                    op(C) = A +/- D*D'*W = A +/- D*S,  if TRANS = 'N';
C                    op(C) = A +/- W*D*D' = A +/- S*D', if TRANS = 'T'.
C
                  IF ( LUPLO ) THEN
C
                     DO 120 J = 1, N
                        CALL DAXPY( J, BETA, C(1,J), 1, R(1,J), 1 )
  120                CONTINUE
C
                  ELSE
C
                     DO 130 J = 1, N
                        CALL DAXPY( N-J+1, BETA, C(J,J), 1, R(J,J), 1 )
  130                CONTINUE
C
                  END IF
C
                  CALL DLACPY( 'All', N, N, A, LDA, C, LDC )
                  IF ( LTRANS ) THEN
                     CALL DGEMM( NT, TR, N, N, M, BETA, DWORK, N, G,
     $                           LDG, ONE, C, LDC )
                  ELSE
                     CALL DGEMM( NT, NT, N, N, M, BETA, G, LDG, DWORK,
     $                           M, ONE, C, LDC )
                  END IF
C
               ELSE
C
C                 Compute Y = D*D' in DWORK(WP), and Z in C,
C                    Z = +/- Y*W  ( TRANS = 'N' ) or
C                    Z = +/- W*Y  ( TRANS = 'T' ).
C
C                 Workspace: 2*N*N.
C
                  CALL DSYRK( UPLO, NT, N, M, ONE, G, LDG, ZERO,
     $                        DWORK(WP), N )
C
                  CALL DSYMM( SIDE, UPLO, N, N, BETA, DWORK(WP), N,
     $                        DWORK, N, ZERO, C, LDC )
C
C                 Compute in DWORK(WP) a triangle of symmetric matrix
C                    T = W'*Z,  if TRANS = 'N';
C                    T = Z*W',  if TRANS = 'T'.
C                 and its norm.
C
                  CALL MB01RB( SIDE, UPLO, TR, N, N, ZERO, ONE,
     $                         DWORK(WP), N, DWORK, N, C, LDC, INFO )
C
                  NORMS(2) = DLANSY( 'F-norm', UPLO, N, DWORK(WP), N,
     $                               DWORK )
C
C                 Update R, R = R + T, and compute  op(C) = A + Z.
C
                  I = WP
                  IF ( LUPLO ) THEN
C
                     DO 140 J = 1, N
                        CALL DAXPY( J, ONE, DWORK(I), 1, R(1,J), 1 )
                        I = I + N
  140                CONTINUE
C
                  ELSE
C
                     DO 150 J = 1, N
                        CALL DAXPY( N-J+1, ONE, DWORK(I), 1, R(J,J), 1 )
                        I = I + N + 1
  150                CONTINUE
C
                  END IF
C
                  DO 160 J = 1, N
                     CALL DAXPY( N, ONE, A(1,J), 1, C(1,J), 1 )
  160             CONTINUE
C
               END IF
C
            ELSE
C
               NORMS(2) = ZERO
               CALL DLACPY( 'All', N, N, A, LDA, C, LDC )
C
            END IF
C
         ELSE
C
C           Continuous-time case.
C
            IF ( LJOBE ) THEN
C
C              Compute
C                 A'*X*E,  if TRANS = 'N';
C                 E*X*A',  if TRANS = 'T'.
C
               IF ( LJOBL ) THEN
C
C                 Compute in C
C                    T = A'*XE,  if TRANS = 'N';
C                    T = XE*A',  if TRANS = 'T'.
C
                  IF ( LTRANS ) THEN
                     CALL DGEMM( NT, TR, N, N, N, ONE, XE, LDXE, A, LDA,
     $                           ZERO, C, LDC )
                  ELSE
                     CALL DGEMM( TR, NT, N, N, N, ONE, A, LDA, XE, LDXE,
     $                           ZERO, C, LDC )
                  END IF
C
               ELSE
C
C                 First, compute in DWORK
C                    W = X*E,  if TRANS = 'N';
C                    W = E*X,  if TRANS = 'T'.
C
C                 Workspace: N*N.
C
                  CALL DSYMM( SIDE, UPLO, N, N, ONE, X, LDX, E, LDE,
     $                        ZERO, DWORK, N )
C
C                 Compute in C
C                    T = A'*W,  if TRANS = 'N';
C                    T = W*A',  if TRANS = 'T'.
C
                  IF ( LTRANS ) THEN
                     CALL DGEMM( NT, TR, N, N, N, ONE, DWORK, N, A, LDA,
     $                           ZERO, C, LDC )
                  ELSE
                     CALL DGEMM( TR, NT, N, N, N, ONE, A, LDA, DWORK, N,
     $                           ZERO, C, LDC )
                  END IF
C
               END IF
C
            ELSE
C
C              Compute in C
C                 T = X*A,  if TRANS = 'N';
C                 T = A*X,  if TRANS = 'T'.
C
               CALL DSYMM( SIDE, UPLO, N, N, ONE, X, LDX, A, LDA, ZERO,
     $                     C, LDC )
C
            END IF
C
C           Compute the norm of T.
C
            NORMS(1) = DLANGE( 'F-norm', N, N, C, LDC, DWORK )
C
C           Compute Q + T + T' in R.
C
            IF ( LUPLO ) THEN
C
               DO 170 J = 1, N
                  CALL DAXPY( J, ONE, C(1,J), 1,   R(1,J), 1 )
                  CALL DAXPY( J, ONE, C(J,1), LDC, R(1,J), 1 )
  170          CONTINUE
C
            ELSE
C
               DO 180 J = 1, N
                  CALL DAXPY( N-J+1, ONE, C(J,J), 1,   R(J,J), 1 )
                  CALL DAXPY( N-J+1, ONE, C(J,J), LDC, R(J,J), 1 )
  180          CONTINUE
C
            END IF
C
            IF ( LJOBL ) THEN
C
C              Compute F*F' or H*K in C and its norm.
C
               IF ( LJOBF ) THEN
C
                  CALL DSYRK( UPLO, NT, N, M, ONE, F, LDF, ZERO, C,
     $                        LDC )
               ELSE
C
                  CALL MB01RB( 'Left', UPLO, NT, N, M, ZERO, ONE, C,
     $                         LDC, F, LDF, K, LDK, INFO )
               END IF
C
               NORMS(2) = DLANSY( 'F-norm', UPLO, N, C, LDC, DWORK )
C
C              Update R := R +/- F*F' or R := R +/- op(H*K).
C
               IF ( LUPLO ) THEN
C
                  DO 190 J = 1, N
                     CALL DAXPY( J, BETA, C(1,J), 1, R(1,J), 1 )
  190             CONTINUE
C
               ELSE
C
                  DO 200 J = 1, N
                     CALL DAXPY( N-J+1, BETA, C(J,J), 1, R(J,J), 1 )
  200             CONTINUE
C
               END IF
C
C              Compute op(C) = A +/- op(D*F') or op(C) = A +/- op(B*K).
C
               CALL DLACPY( 'All', N, N, A, LDA, C, LDC )
C
               IF ( LJOBF ) THEN
                  IF ( LTRANS ) THEN
                     CALL DGEMM( NT, TR, N, N, M, BETA, F, LDF, G, LDG,
     $                           ONE, C, LDC )
                  ELSE
                     CALL DGEMM( NT, TR, N, N, M, BETA, G, LDG, F, LDF,
     $                           ONE, C, LDC )
                  END IF
C
               ELSE
C
                  IF ( LTRANS ) THEN
                     CALL DGEMM( TR, TR, N, N, M, BETA, K, LDK, G, LDG,
     $                           ONE, C, LDC )
                  ELSE
                     CALL DGEMM( NT, NT, N, N, M, BETA, G, LDG, K, LDK,
     $                           ONE, C, LDC )
                  END IF
C
               END IF
C
            ELSE IF ( LJOBG ) THEN
C
               IF ( LJOBE ) THEN
C
C                 Compute V = op(E)'*X*G*X*op(E) in C,
C                    V = W'*G*W,  if TRANS = 'N';
C                    V = W*G*W',  if TRANS = 'T'.
C                 First, compute in DWORK(WP)
C                    Z = G*W,     if TRANS = 'N';
C                    Z = W*G,     if TRANS = 'T'.
C
C                 Workspace: 2*N*N.
C
                  CALL DSYMM( SIDE, UPLO, N, N, ONE, G, LDG, DWORK, N,
     $                        ZERO, DWORK(WP), N )
C
C                 Then, compute in C the symmetric matrix V.
C
                  CALL MB01RB( SIDE, UPLO, TR, N, N, ZERO, ONE, C, LDC,
     $                         DWORK, N, DWORK(WP), N, INFO )
                  I = WP
C
               ELSE
C
C                 Compute V = X*G*X in C.
C                 First, compute in DWORK
C                    Z = G*X,  if TRANS = 'N';
C                    Z = X*G,  if TRANS = 'T'.
C                 Then, compute in C the symmetric matrix V.
C
                  IF ( KEEPX ) THEN
C
C                    Copy the matrix X to DWORK(WP).
C
C                    Workspace: 2*N*N.
C
                     CALL DLACPY( UPLO, N, N, X, LDX, DWORK(WP), N )
                     CALL MA02ED( UPLO, N, DWORK(WP), N )
                     CALL DSYMM(  SIDE, UPLO, N, N, ONE, G, LDG,
     $                            DWORK(WP), N, ZERO, DWORK, N )
                     CALL MB01RB( SIDE, UPLO, TR, N, N, ZERO, ONE, C,
     $                            LDC, DWORK(WP), N, DWORK, N, INFO )
                  ELSE
C
C                    Workspace: N*N.
C
                     CALL DSYMM(  SIDE, UPLO, N, N, ONE, G, LDG, X, LDX,
     $                            ZERO, DWORK, N )
                     CALL MB01RB( SIDE, UPLO, TR, N, N, ZERO, ONE, C,
     $                            LDC, X, LDX, DWORK, N, INFO )
                  END IF
C
                  I = 1
               END IF
C
C              Compute the norm of V.
C
               NORMS(2) = DLANSY( 'F-norm', UPLO, N, C, LDC, DWORK )
C
C              Update R := R +/- V.
C
               IF ( LUPLO ) THEN
C
                  DO 210 J = 1, N
                     CALL DAXPY( J, BETA, C(1,J), 1, R(1,J), 1 )
  210             CONTINUE
C
               ELSE
C
                  DO 220 J = 1, N
                     CALL DAXPY( N-J+1, BETA, C(J,J), 1, R(J,J), 1 )
  220             CONTINUE
C
               END IF
C
C              Compute op(C), op(C) = A +/- Z.
C
               DO 230 J = 1, N
                  CALL DCOPY( N, A(1,J), 1, C(1,J), 1 )
                  CALL DAXPY( N, BETA, DWORK(I), 1, C(1,J), 1 )
                  I = I + N
  230          CONTINUE
C
            ELSE IF ( M.GT.0 ) THEN
C
C              Compute the matrix
C                V = op(E)'*X*D*D'*X*op(E), with D in array G.
C
               IF ( USE1 ) THEN
C
C                 Workspace N*N + N*M (including for S'*S or S*S').
C
                  IF ( LJOBE ) THEN
C
C                    Compute S = D'*W or S = W*D  (stored in DWORK(WP)).
C
                     IF ( LTRANS ) THEN
                        CALL DGEMM( NT, NT, N, M, N, ONE, DWORK, N, G,
     $                              LDG, ZERO, DWORK(WP), N )
                     ELSE
                        CALL DGEMM( TR, NT, M, N, N, ONE, G, LDG, DWORK,
     $                              N, ZERO, DWORK(WP), M )
                     END IF
C
                  ELSE
C
C                    Compute S = X*D        (stored in DWORK(WP)).
C
                     CALL DSYMM( 'Left', UPLO, N, M, ONE, X, LDX, G,
     $                           LDG, ZERO, DWORK(WP), N )
                  END IF
C
C                 Compute V = S'*S or V = S*S' (in DWORK), and op(C).
C
                  CALL DLACPY( 'All', N, N, A, LDA, C, LDC )
                  IF ( UNITE .OR. LTRANS ) THEN
                     CALL DSYRK( UPLO, NT, N, M, ONE, DWORK(WP), N,
     $                           ZERO, DWORK, N )
                     IF ( LTRANS ) THEN
                        CALL DGEMM( NT, TR, N, N, M, BETA, DWORK(WP), N,
     $                              G, LDG, ONE, C, LDC )
                     ELSE
                        CALL DGEMM( NT, TR, N, N, M, BETA, G, LDG,
     $                              DWORK(WP), N, ONE, C, LDC )
                     END IF
                  ELSE
                     CALL DSYRK( UPLO, TR, N, M, ONE, DWORK(WP), M,
     $                           ZERO, DWORK, N )
                     CALL DGEMM( NT, NT, N, N, M, BETA, G, LDG,
     $                           DWORK(WP), M, ONE, C, LDC )
                  END IF
C
                  NORMS(2) = DLANSY( 'F-norm', UPLO, N, DWORK, N,
     $                               DWORK )
C
C                 Update R := R +/- op(E)'*X*D*D'*X*op(E).
C
                  I = 1
                  IF ( LUPLO ) THEN
C
                     DO 240 J = 1, N
                        CALL DAXPY( J, BETA, DWORK(I), 1, R(1,J), 1 )
                        I = I + N
  240                CONTINUE
C
                  ELSE
C
                     DO 250 J = 1, N
                        CALL DAXPY( N-J+1, BETA, DWORK(I), 1, R(J,J),
     $                              1 )
                        I = I + N + 1
  250                CONTINUE
C
                  END IF
C
               ELSE
C
C                 Compute Y = D*D' in DWORK(WP) and compute in C
C                 the following product (depending on TRANS):
C                     Z = +/- Y*W | Z = +/- W*Y,  if JOBE = 'G', or
C                     Z = +/- op(Y*X),            if JOBE = 'I'.
C                 Then, compute T in DWORK(WP),
C                     T = W'*Z | T = Z*W',  if JOBE = 'G', or
C                     T =  X*Z | T = Z*X,   if JOBE = 'I',
C                 and its norm. Finally, update R and set op(C),
C                     R = R + T;  op(C) = A + Z.
C
C                 Workspace: (IA+1)*N*N.
C
                  CALL DSYRK( UPLO, NT, N, M, ONE, G, LDG, ZERO,
     $                        DWORK(WP), N )
C
                  IF ( LJOBE ) THEN
                     CALL DSYMM( SIDE, UPLO, N, N, BETA, DWORK(WP), N,
     $                           DWORK, N, ZERO, C, LDC )
                     IF ( LTRANS ) THEN
                        CALL DGEMM( NT, TR, N, N, N, ONE, C, LDC, DWORK,
     $                              N, ZERO, DWORK(WP), N )
                     ELSE
                        CALL DGEMM( TR, NT, N, N, N, ONE, DWORK, N, C,
     $                              LDC, ZERO, DWORK(WP), N )
                     END IF
                  ELSE
                     CALL MA02ED( UPLO, N, DWORK, N )
                     CALL DSYMM(  NSIDE, UPLO, N, N, BETA, X, LDX,
     $                            DWORK, N, ZERO, C, LDC )
                     CALL DSYMM(  SIDE, UPLO, N, N, ONE, X, LDX, C, LDC,
     $                            ZERO, DWORK, N )
                  END IF
C
                  NORMS(2) = DLANSY( 'F-norm', UPLO, N, DWORK(WP), N,
     $                               DWORK )
                  I = WP
C
                  IF ( LUPLO ) THEN
C
                     DO 260 J = 1, N
                        CALL DAXPY( J, ONE, DWORK(I), 1, R(1,J), 1 )
                        I = I + N
  260                CONTINUE
C
                  ELSE
C
                     DO 270 J = 1, N
                        CALL DAXPY( N-J+1, ONE, DWORK(I), 1, R(J,J), 1 )
                        I = I + N + 1
  270                CONTINUE
C
                  END IF
C
                  DO 280 J = 1, N
                     CALL DAXPY( N, ONE, A(1,J), 1, C(1,J), 1 )
  280             CONTINUE
C
               END IF
C
            ELSE
C
               NORMS(2) = ZERO
               CALL DLACPY( 'All', N, N, A, LDA, C, LDC )
C
            END IF
C
         END IF
C
      ELSE IF ( LJOBB ) THEN
C
C        JOB = 'B'.
C
         IF ( DISCR ) THEN
C
            IF ( LJOBE ) THEN
C
C              Compute op(E)'*X*op(E) in DWORK and its norm.
C
C              Workspace: 2*N*N.
C
               CALL MB01RU( UPLO, NTRANS, N, N, ZERO, ONE, DWORK, N, E,
     $                      LDE, X, LDX, DWORK(NN+1), NN, INFO )
C
               NORMS(3) = DLANSY( 'F-norm', UPLO, N, DWORK, N, DWORK )
C
C              Compute Q - op(E)'*X*op(E) in R.
C
               I = 1
               IF ( LUPLO ) THEN
C
                  DO 290 J = 1, N
                     CALL DAXPY( J, -ONE, DWORK(I), 1, R(1,J), 1 )
                     I = I + N
  290             CONTINUE
C
               ELSE
C
                  DO 300 J = 1, N
                     CALL DAXPY( N-J+1, -ONE, DWORK(I), 1, R(J,J), 1 )
                     I = I + N + 1
  300             CONTINUE
C
               END IF
C
            ELSE
C
C              Compute Q - X in R.
C
               IF ( LUPLO ) THEN
C
                  DO 310 J = 1, N
                     CALL DAXPY( J, -ONE, X(1,J), 1, R(1,J), 1 )
  310             CONTINUE
C
               ELSE
C
                  DO 320 J = 1, N
                     CALL DAXPY( N-J+1, -ONE, X(J,J), 1, R(J,J), 1 )
  320             CONTINUE
C
               END IF
C
            END IF
C
C           Compute op(A)'*X*op(A) in DWORK and its norm.
C
            IF ( LJOBL ) THEN
C
C              Compute in DWORK the symmetric matrix
C                 T = A'*XE,  if TRANS = 'N';
C                 T = XE*A',  if TRANS = 'T'.
C
C              Workspace: N*N.
C
               I = 1
               CALL MB01RB( SIDE, UPLO, TR, N, N, ZERO, ONE, DWORK, N,
     $                      A, LDA, XE, LDXE, INFO )
            ELSE
C
               IF ( USE1 ) THEN
                  IW = 1
                  I  = WP
               ELSE
                  IW = WP
                  I  = 1
               END IF
C
C              First, compute W in DWORK(IW)
C                 W = X*A,  if TRANS = 'N';
C                 W = A*X,  if TRANS = 'T'.
C
C              Workspace: 2*N*N.
C
               CALL DSYMM( SIDE, UPLO, N, N, ONE, X, LDX, A, LDA, ZERO,
     $                     DWORK(IW), N )
C
C              Compute in DWORK(I) the symmetric matrix
C                 T = A'*W,  if TRANS = 'N';
C                 T = W*A',  if TRANS = 'T'.
C
               CALL MB01RB( SIDE, UPLO, TR, N, N, ZERO, ONE, DWORK(I),
     $                      N, A, LDA, DWORK(IW), N, INFO )
            END IF
C
            NORMS(1) = DLANSY( 'F-norm', UPLO, N, DWORK(I), N, DWORK )
C
C           Update R := R + op(A)'*X*op(A).
C
            IF ( LUPLO ) THEN
C
               DO 330 J = 1, N
                  CALL DAXPY( J, ONE, DWORK(I), 1, R(1,J), 1 )
                  I = I + N
  330          CONTINUE
C
            ELSE
C
               DO 340 J = 1, N
                  CALL DAXPY( N-J+1, ONE, DWORK(I), 1, R(J,J), 1 )
                  I = I + N + 1
  340          CONTINUE
C
            END IF
C
            IF ( LJOBL ) THEN
C
C              Compute F*F' or H*K in DWORK and its norm.
C
C              Workspace: N*N.
C
              IF ( LJOBF ) THEN
C
                  CALL DSYRK( UPLO, NT, N, M, ONE, F, LDF, ZERO, DWORK,
     $                        N )
C
               ELSE
C
                  CALL MB01RB( 'Left', UPLO, NT, N, M, ZERO, ONE, DWORK,
     $                         N, F, LDF, K, LDK, INFO )
               END IF
C
               NORMS(2) = DLANSY( 'F-norm', UPLO, N, DWORK, N, DWORK )
C
C              Update R := R +/- F*F' or R := R +/- H*K.
C
               I = 1
               IF ( LUPLO ) THEN
C
                  DO 350 J = 1, N
                     CALL DAXPY( J, BETA, DWORK(I), 1, R(1,J), 1 )
                     I = I + N
  350             CONTINUE
C
               ELSE
C
                  DO 360 J = 1, N
                     CALL DAXPY( N-J+1, BETA, DWORK(I), 1, R(J,J), 1 )
                     I = I + N + 1
  360             CONTINUE
C
               END IF
C
            ELSE IF ( LJOBG ) THEN
C
C              Compute V = op(A)'*X*G*X*op(A) in DWORK and its norm,
C              using W in DWORK(IW).
C
C              Workspace: 3*N*N.
C
               CALL MB01RU( UPLO, NTRANS, N, N, ZERO, ONE, DWORK, N,
     $                      DWORK(IW), N, G, LDG, DWORK(2*NN+1), NN,
     $                      INFO )
C
               NORMS(2) = DLANSY( 'F-norm', UPLO, N, DWORK, N, DWORK )
C
C              Update R := R +/- op(A)'*X*G*X*op(A) = R + V.
C
               I = 1
               IF ( LUPLO ) THEN
C
                  DO 370 J = 1, N
                     CALL DAXPY( J, BETA, DWORK(I), 1, R(1,J), 1 )
                     I = I + N
  370             CONTINUE
C
               ELSE
C
                  DO 380 J = 1, N
                     CALL DAXPY( N-J+1, BETA, DWORK(I), 1, R(J,J), 1 )
                     I = I + N + 1
  380             CONTINUE
C
               END IF
C
            ELSE IF ( M.GT.0 ) THEN
C
               IF ( USE1 ) THEN
C
C                 Compute S = D'*W or S = W*D   (stored in DWORK(WP)).
C                 Then, compute
C                    V = op(A)'*X*D*D'*X*op(A) (i.e., S'*S or S*S')
C                 in DWORK, and compute its norm; D in stored in
C                 array G.
C
C                 Workspace: N*N + N*M.
C
                  IF ( LTRANS ) THEN
                     CALL DGEMM( NT, NT, N, M, N, ONE, DWORK, N, G,
     $                           LDG, ZERO, DWORK(WP), N )
                     CALL DSYRK( UPLO, NT, N, M, ONE, DWORK(WP), N,
     $                           ZERO, DWORK, N )
                  ELSE
                     CALL DGEMM( TR, NT, M, N, N, ONE, G, LDG,
     $                           DWORK, N, ZERO, DWORK(WP), M )
                     CALL DSYRK( UPLO, TR, N, M, ONE, DWORK(WP), M,
     $                           ZERO, DWORK, N )
                  END IF
C
                  NORMS(2) = DLANSY( 'F-norm', UPLO, N, DWORK, N,
     $                               DWORK )
C
C                 Update R := R +/- op(A)'*X*D*D'*X*op(A) = R +/- V.
C
                  I = 1
                  IF ( LUPLO ) THEN
C
                     DO 390 J = 1, N
                        CALL DAXPY( J, BETA, DWORK(I), 1, R(1,J), 1 )
                        I = I + N
  390                CONTINUE
C
                  ELSE
C
                     DO 400 J = 1, N
                        CALL DAXPY( N-J+1, BETA, DWORK(I), 1, R(J,J), 1)
                        I = I + N + 1
  400                CONTINUE
C
                  END IF
C
               ELSE
C
C                 Compute Y = D*D' in DWORK.
C
                  CALL DSYRK( UPLO, NT, N, M, ONE, G, LDG, ZERO,
     $                        DWORK, N )
C
C                 Compute V = op(A)'*X*Y*X*op(A), also in DWORK, and
C                 its norm, using W in DWORK(IW).
C
C                 Workspace: 3*N*N.
C
                  CALL MB01RU( UPLO, NTRANS, N, N, ZERO, ONE, DWORK, N,
     $                         DWORK(IW), N, DWORK, N, DWORK(2*NN+1),
     $                         NN, INFO )
                  CALL DSCAL( N, ONE/TWO, DWORK, N+1 )
C
                  NORMS(2) = DLANSY( 'F-norm', UPLO, N, DWORK, N,
     $                               DWORK )
C
C                 Update R, R = R +/- V.
C
                  I = 1
                  IF ( LUPLO ) THEN
C
                     DO 410 J = 1, N
                        CALL DAXPY( J, BETA, DWORK(I), 1, R(1,J), 1 )
                        I = I + N
  410                CONTINUE
C
                  ELSE
C
                     DO 420 J = 1, N
                        CALL DAXPY( N-J+1, BETA, DWORK(I), 1, R(J,J), 1)
                        I = I + N + 1
  420                CONTINUE
C
                  END IF
C
               END IF
C
            ELSE
C
               NORMS(2) = ZERO
C
            END IF
C
         ELSE
C
C           Continuous-time case.
C
            IF ( LJOBE ) THEN
C
C              Compute
C                 A'*X*E,  if TRANS = 'N';
C                 E*X*A',  if TRANS = 'T'.
C
               IF ( LJOBL ) THEN
C
C                 Compute in DWORK
C                    T = A'*XE,  if TRANS = 'N';
C                    T = XE*A',  if TRANS = 'T'.
C
C                 Workspace: N*N.
C
                  I = 1
                  IF ( LTRANS ) THEN
                     CALL DGEMM( NT, TR, N, N, N, ONE, XE, LDXE, A, LDA,
     $                           ZERO, DWORK, N )
                  ELSE
                     CALL DGEMM( TR, NT, N, N, N, ONE, A, LDA, XE, LDXE,
     $                           ZERO, DWORK, N )
                  END IF
C
               ELSE
C
                  IF ( USE1 ) THEN
                     IW = 1
                     I  = WP
                  ELSE
                     IW = WP
                     I  = 1
                  END IF
C
C                 First, compute in DWORK(IW)
C                    W = X*E,  if TRANS = 'N';
C                    W = E*X,  if TRANS = 'T'.
C
C                 Workspace: 2*N*N.
C
                  CALL DSYMM( SIDE, UPLO, N, N, ONE, X, LDX, E, LDE,
     $                        ZERO, DWORK(IW), N )
C
C                 Compute in DWORK(I)
C                    T = A'*W,  if TRANS = 'N';
C                    T = W*A',  if TRANS = 'T'.
C
                  IF ( LTRANS ) THEN
                     CALL DGEMM( NT, TR, N, N, N, ONE, DWORK(IW), N, A,
     $                           LDA, ZERO, DWORK(I), N )
                  ELSE
                     CALL DGEMM( TR, NT, N, N, N, ONE, A, LDA,
     $                           DWORK(IW), N, ZERO, DWORK(I), N )
                  END IF
C
               END IF
C
            ELSE
C
C              Compute in DWORK
C                 T = X*A,  if TRANS = 'N';
C                 T = A*X,  if TRANS = 'T'.
C
C              Workspace: N*N.
C
               I = 1
               CALL DSYMM( SIDE, UPLO, N, N, ONE, X, LDX, A, LDA, ZERO,
     $                     DWORK, N )
C
            END IF
C
C           Compute the norm of T.
C
            NORMS(1) = DLANGE( 'F-norm', N, N, DWORK(I), N, DWORK )
C
C           Compute Q + T + T' in R.
C
            IF ( LUPLO ) THEN
               L = I
C
               DO 430 J = 1, N
                  CALL DAXPY( J, ONE, DWORK(I), 1, R(1,J), 1 )
                  CALL DAXPY( J, ONE, DWORK(L), N, R(1,J), 1 )
                  I = I + N
                  L = L + 1
  430          CONTINUE
C
            ELSE
C
               DO 440 J = 1, N
                  CALL DAXPY( N-J+1, ONE, DWORK(I), 1, R(J,J), 1 )
                  CALL DAXPY( N-J+1, ONE, DWORK(I), N, R(J,J), 1 )
                  I = I + N + 1
  440          CONTINUE
C
            END IF
C
            IF ( LJOBL ) THEN
C
C              Compute F*F' or H*K in DWORK and its norm.
C
C              Workspace: N*N.
C
               IF ( LJOBF ) THEN
C
                  CALL DSYRK( UPLO, NT, N, M, ONE, F, LDF, ZERO, DWORK,
     $                        N )
               ELSE
C
                  CALL MB01RB( 'Left', UPLO, NT, N, M, ZERO, ONE, DWORK,
     $                         N, F, LDF, K, LDK, INFO )
               END IF
C
               NORMS(2) = DLANSY( 'F-norm', UPLO, N, DWORK, N, DWORK )
C
C              Update R := R +/- F*F' or R := R +/- op(H*K).
C
               I = 1
               IF ( LUPLO ) THEN
C
                  DO 450 J = 1, N
                     CALL DAXPY( J, BETA, DWORK(I), 1, R(1,J), 1 )
                     I = I + N
  450             CONTINUE
C
               ELSE
C
                  DO 460 J = 1, N
                     CALL DAXPY( N-J+1, BETA, DWORK(I), 1, R(J,J), 1 )
                     I = I + N + 1
  460             CONTINUE
C
               END IF
C
            ELSE IF ( LJOBG ) THEN
C
               IF ( LJOBE ) THEN
C
C                 Compute V = op(E)'*X*G*X*op(E) in DWORK, using W
C                 in DWORK(IW).
C
C                 Workspace: 3*N*N.
C
                  CALL MB01RU( UPLO, NTRANS, N, N, ZERO, ONE, DWORK,
     $                         N, DWORK(IW), N, G, LDG, DWORK(2*NN+1),
     $                         NN, INFO )
C
               ELSE
C
C                 Compute V = X*G*X in DWORK.
C
                  IF ( KEEPX ) THEN
C
C                    Copy the matrix X to DWORK(WP) and compute V.
C
C                    Workspace: 3*N*N.
C
                     CALL DLACPY( UPLO, N, N, X, LDX, DWORK(WP), N )
                     CALL MA02ED( UPLO, N, DWORK(WP), N )
                     CALL MB01RU( UPLO, NTRANS, N, N, ZERO, ONE, DWORK,
     $                            N, DWORK(WP), N, G, LDG,
     $                            DWORK(2*NN+1), NN, INFO )
                  ELSE
C
C                    Compute in DWORK the symmetric matrix V.
C
C                    Workspace: 2*N*N.
C
                     CALL MB01RU( UPLO, NTRANS, N, N, ZERO, ONE, DWORK,
     $                            N, X, LDX, G, LDG, DWORK(NN+1), NN,
     $                            INFO )
                  END IF
C
               END IF
C
C              Compute the norm of V.
C
               NORMS(2) = DLANSY( 'F-norm', UPLO, N, DWORK, N, DWORK )
C
C              Update R := R +/- V.
C
               I = 1
               IF ( LUPLO ) THEN
C
                  DO 470 J = 1, N
                     CALL DAXPY( J, BETA, DWORK(I), 1, R(1,J), 1 )
                     I = I + N
  470             CONTINUE
C
               ELSE
C
                  DO 480 J = 1, N
                     CALL DAXPY( N-J+1, BETA, DWORK(I), 1, R(J,J), 1 )
                     I = I + N + 1
  480             CONTINUE
C
               END IF
C
            ELSE IF ( M.GT.0 ) THEN
C
C              Compute the matrix
C                V = op(E)'*X*D*D'*X*op(E), with D in array G.
C
               IF ( USE1 ) THEN
C
C                 Workspace N*N + N*M (including for S'*S or S*S').
C
                  IF ( LJOBE ) THEN
C
C                    Compute S = D'*W or S = W*D  (stored in DWORK(WP)).
C
                     IF ( LTRANS ) THEN
                        CALL DGEMM( NT, NT, N, M, N, ONE, DWORK, N, G,
     $                              LDG, ZERO, DWORK(WP), N )
                     ELSE
                        CALL DGEMM( TR, NT, M, N, N, ONE, G, LDG,
     $                              DWORK, N, ZERO, DWORK(WP), M )
                     END IF
C
                  ELSE
C
C                    Compute S = X*D        (stored in DWORK(WP)).
C
                     CALL DSYMM( 'Left', UPLO, N, M, ONE, X, LDX, G,
     $                           LDG, ZERO, DWORK(WP), N )
                  END IF
C
C                 Compute V = S'*S or V = S*S' (in DWORK).
C
                  IF ( UNITE .OR. LTRANS ) THEN
                     CALL DSYRK( UPLO, NT, N, M, ONE, DWORK(WP), N,
     $                           ZERO, DWORK, N )
                  ELSE
                     CALL DSYRK( UPLO, TR, N, M, ONE, DWORK(WP), M,
     $                           ZERO, DWORK, N )
                  END IF
C
                  NORMS(2) = DLANSY( 'F-norm', UPLO, N, DWORK, N,
     $                               DWORK )
C
C                 Update R := R +/- op(E)'*X*D*D'*X*op(E).
C
                  I = 1
                  IF ( LUPLO ) THEN
C
                     DO 490 J = 1, N
                        CALL DAXPY( J, BETA, DWORK(I), 1, R(1,J), 1 )
                        I = I + N
  490                CONTINUE
C
                  ELSE
C
                     DO 500 J = 1, N
                        CALL DAXPY( N-J+1, BETA, DWORK(I), 1, R(J,J),
     $                              1 )
                        I = I + N + 1
  500                CONTINUE
C
                  END IF
C
               ELSE
C
C                 Compute Y = D*D' in DWORK and compute also in DWORK
C                 the following product (depending on TRANS):
C                     T = +/- W'*Y*W | T = +/- W*Y*W',  if JOBE = 'G', or
C                     T = +/-  X*Y*X,                   if JOBE = 'I',
C                 and its norm. Finally, update R,
C                     R = R + T.
C
                  CALL DSYRK( UPLO, NT, N, M, ONE, G, LDG, ZERO, DWORK,
     $                        N )
C
                  IF ( LJOBE ) THEN
C
C                    Workspace: 3*N*N.
C
                     CALL MB01RU( UPLO, NTRANS, N, N, ZERO, BETA, DWORK,
     $                            N, DWORK(IW), N, DWORK, N,
     $                            DWORK(2*NN+1), NN, INFO )
                     CALL DSCAL(  N, ONE/TWO, DWORK, N+1 )
C
                  ELSE IF ( LDWORK.GE.3*NN ) THEN
C
C                    Workspace: 3*N*N.
C
                     CALL DLACPY( UPLO, N, N, X, LDX, DWORK(NN+1), N )
                     CALL MA02ED( UPLO, N, DWORK(NN+1), N )
                     CALL MB01RU( UPLO, NTRANS, N, N, ZERO, BETA, DWORK,
     $                            N, DWORK(NN+1), N, DWORK, N,
     $                            DWORK(2*NN+1), NN, INFO )
                     CALL DSCAL(  N, ONE/TWO, DWORK, N+1 )
C
                  ELSE
C
C                    Workspace: 2*N*N.
C
                     CALL MA02ED( UPLO, N, DWORK, N )
                     CALL DSYMM(  NSIDE, UPLO, N, N, BETA, X, LDX,
     $                            DWORK, N, ZERO, DWORK(NN+1), N )
                     CALL DSYMM(  SIDE, UPLO, N, N, ONE, X, LDX,
     $                            DWORK(NN+1), N, ZERO, DWORK, N )
                  END IF
C
                  NORMS(2) = DLANSY( 'F-norm', UPLO, N, DWORK, N,
     $                               DWORK )
                  I = 1
C
                  IF ( LUPLO ) THEN
C
                     DO 510 J = 1, N
                        CALL DAXPY( J, ONE, DWORK(I), 1, R(1,J), 1 )
                        I = I + N
  510                CONTINUE
C
                  ELSE
C
                     DO 520 J = 1, N
                        CALL DAXPY( N-J+1, ONE, DWORK(I), 1, R(J,J), 1 )
                        I = I + N + 1
  520                CONTINUE
C
                  END IF
C
               END IF
C
            ELSE
C
               NORMS(2) = ZERO
C
            END IF
C
         END IF
C
      ELSE
C
C        JOB <> 'N' and JOB <> 'B'.
C
         IF ( DISCR ) THEN
C
C           Discrete-time case.
C
            IF ( LJOBL ) THEN
C
C              Using F or H and K (JOBG = 'F' or 'H').
C
               IF ( NLJOBC ) THEN
C
C                 Start to compute the residual:
C                 R  = Q + op(A)'*X*op(A);
C                 R := R - op(E)'*X*op(E), if JOBE = 'G'.
C
                  CALL MB01RB( SIDE, UPLO, TR, N, N, ONE, ONE, R, LDR,
     $                         A, LDA, XE, LDXE, INFO )
C
                  IF ( LJOBE ) THEN
                     IF ( LJOBR ) THEN
C
C                       Workspace: N*N.
C
                        CALL MB01RU( UPLO, NTRANS, N, N, ONE, -ONE, R,
     $                               LDR, E, LDE, X, LDX, DWORK, NN,
     $                               INFO )
                     ELSE
C
C                       Use C as workspace.
C
                        CALL MB01RU( UPLO, NTRANS, N, N, ONE, -ONE, R,
     $                               LDR, E, LDE, X, LDX, C, NN, INFO )
                     END IF
                  END IF
C
                  IF ( LJOBF ) THEN
C
C                    Add/subtract F*F' to/from R.
C
                     CALL DSYRK( UPLO, NT, N, M, BETA, F, LDF, ONE, R,
     $                           LDR )
                  ELSE
C
C                    Add/subtract H*K to/from R.
C
                     CALL MB01RB( 'Left', UPLO, NT, N, M, ONE, BETA, R,
     $                            LDR, F, LDF, K, LDK, INFO )
                  END IF
               END IF
C
               IF ( NLJOBR ) THEN
C
C                 Compute op(C) = A +/- op(D*F') or
C                         op(C) = A +/- op(B*K).
C
                  CALL DLACPY( 'All', N, N, A, LDA, C, LDC )
C
                  IF ( LJOBF ) THEN
                     IF ( LTRANS ) THEN
                        CALL DGEMM( NT, TR, N, N, M, BETA, F, LDF, G,
     $                              LDG, ONE, C, LDC )
                     ELSE
                        CALL DGEMM( NT, TR, N, N, M, BETA, G, LDG, F,
     $                              LDF, ONE, C, LDC )
                     END IF
C
                  ELSE
                     IF ( LTRANS ) THEN
                        CALL DGEMM( TR, TR, N, N, M, BETA, K, LDK, G,
     $                              LDG, ONE, C, LDC )
                     ELSE
                        CALL DGEMM( NT, NT, N, N, M, BETA, G, LDG, K,
     $                              LDK, ONE, C, LDC )
                     END IF
                  END IF
C
               END IF
C
            ELSE
C
C              Usual case (JOBG = 'G' or JOBG = 'D').
C
               USEC = LJOBA .AND. USE1
C
               IF ( USEC ) THEN
C
C                 Compute in C
C                    W = X*A,  if TRANS = 'N';
C                    W = A*X,  if TRANS = 'T'.
C
                  CALL DSYMM( SIDE, UPLO, N, N, ONE, X, LDX, A, LDA,
     $                        ZERO, C, LDC )
C
               ELSE IF ( NLJOBC .OR. LJOBG .OR. USEOPT ) THEN
C
C                 Compute in DWORK
C                    W = X*A,  if TRANS = 'N';
C                    W = A*X,  if TRANS = 'T'.
C
C                 Workspace: N*N.
C
                  CALL DSYMM( SIDE, UPLO, N, N, ONE, X, LDX, A, LDA,
     $                        ZERO, DWORK, N )
               END IF
C
               IF ( LJOBC ) THEN
C
                  IF ( LJOBG ) THEN
C
C                    Compute op(C) = A +/- op(G*W):
C
C                    Workspace: N*N.
C
                     CALL DLACPY( 'All', N, N, A, LDA, C, LDC )
                     CALL DSYMM(  SIDE, UPLO, N, N, BETA, G, LDG, DWORK,
     $                            N, ONE, C, LDC )
C
                  ELSE IF ( M.EQ.0 ) THEN
C
                     CALL DLACPY( 'All', N, N, A, LDA, C, LDC )
C
                  ELSE IF ( USE1 ) THEN
C
C                    Compute X*D in C.
C
                     CALL DSYMM( 'Left', UPLO, N, M, ONE, X, LDX, G,
     $                           LDG, ZERO, C, LDC )
C
C                    Compute in DWORK
C                       D'*X*A  ( TRANS = 'N' ) or
C                       A*X*D   ( TRANS = 'T' ).
C                    Then, compute op(C).
C
C                    Workspace: N*M.
C
                     IF ( LTRANS ) THEN
                        CALL DGEMM(  NT, NT, N, M, N, ONE, A, LDA, C,
     $                               LDC, ZERO, DWORK, N )
                        CALL DLACPY( 'All', N, N, A, LDA, C, LDC )
                        CALL DGEMM(  NT, TR, N, N, M, BETA, DWORK, N, G,
     $                               LDG, ONE, C, LDC )
                     ELSE
                        CALL DGEMM(  TR, NT, M, N, N, ONE, C, LDC, A,
     $                               LDA, ZERO, DWORK, M )
                        CALL DLACPY( 'All', N, N, A, LDA, C, LDC )
                        CALL DGEMM(  NT, NT, N, N, M, BETA, G, LDG,
     $                               DWORK, M, ONE, C, LDC )
                     END IF
C
                  ELSE IF ( USEOPT ) THEN
C
C                    Compute D*D' in DWORK(WP) and then compute op(C).
C
C                    Workspace: 2*N*N.
C
                     CALL DSYRK(  UPLO, NT, N, M, ONE, G, LDG, ZERO,
     $                            DWORK(WP), N )
                     CALL DLACPY( 'All', N, N, A, LDA, C, LDC )
                     CALL DSYMM(  SIDE, UPLO, N, N, BETA, DWORK(WP), N,
     $                            DWORK, N, ONE, C, LDC )
                  ELSE
C
C                    Compute D*D' in C, and
C                       T = D*D'*X  ( TRANS = 'N' ) or
C                       T = X*D*D'  ( TRANS = 'T' ),
C                    in DWORK. Then, compute op(C).
C
C                    Workspace: N*N.
C
                     CALL DSYRK(  UPLO, NT, N, M, ONE, G, LDG, ZERO, C,
     $                            LDC )
                     CALL MA02ED( UPLO, N, C, LDC )
                     CALL DSYMM(  NSIDE, UPLO, N, N, ONE, X, LDX, C,
     $                            LDC, ZERO, DWORK, N )
C
                     CALL DLACPY( 'All', N, N, A, LDA, C, LDC )
                     IF ( LTRANS ) THEN
                        CALL DGEMM( NT, NT, N, N, N, BETA, A, LDA,
     $                              DWORK, N, ONE, C, LDC )
                     ELSE
                        CALL DGEMM( NT, NT, N, N, N, BETA, DWORK, N, A,
     $                              LDA, ONE, C, LDC )
                     END IF
C
                  END IF
C
               ELSE
C
C                 Compute in R the symmetric matrix
C                    R = Q + A'*W,  if TRANS = 'N';
C                    R = Q + W*A',  if TRANS = 'T'.
C
                  IF ( USEC ) THEN
C
                     CALL MB01RB( SIDE, UPLO, TR, N, N, ONE, ONE, R,
     $                            LDR, A, LDA, C, LDC, INFO )
                  ELSE
C
C                    Workspace: N*N.
C
                     CALL MB01RB( SIDE, UPLO, TR, N, N, ONE, ONE, R,
     $                            LDR, A, LDA, DWORK, N, INFO )
                  END IF
C
                  IF ( LJOBG ) THEN
C
                     IF ( LJOBR ) THEN
C
C                       Add/subtract V = op(A)'*X*G*X*op(A) to/from R.
C
C                       Workspace: 2*N*N.
C
                        CALL MB01RU( UPLO, NTRANS, N, N, ONE, BETA, R,
     $                               LDR, DWORK, N, G, LDG, DWORK(WP),
     $                               NN, INFO )
                     ELSE
C
C                       Compute in C the following product:
C                          Z = +/- G*W  ( TRANS = 'N' ) or
C                          Z = +/- W*G  ( TRANS = 'T' ).
C
C                       Workspace: N*N.
C
                        CALL DSYMM( SIDE, UPLO, N, N, BETA, G, LDG,
     $                              DWORK, N, ZERO, C, LDC )
C
C                       Add V to R, with
C                          V = W'*Z ( TRANS = 'N' )
C                          V = Z*W' ( TRANS = 'T' ).
C
                        CALL MB01RB( SIDE, UPLO, TR, N, N, ONE, ONE, R,
     $                               LDR, DWORK, N, C, LDC, INFO )
C
C                       Compute  op(C) = A + Z.
C
                        DO 530 J = 1, N
                           CALL DAXPY( N, ONE, A(1,J), 1, C(1,J), 1 )
  530                   CONTINUE
C
                     END IF
C
                  ELSE IF ( M.GT.0 ) THEN
C
                     IF ( LJOBR ) THEN
C
                        IF ( USE1 ) THEN
C
C                          Compute in DWORK(WP) the following product:
C                             S = D'*W  ( TRANS = 'N' ) or
C                             S = W*D   ( TRANS = 'T' ).
C                          Then add/subtract V = S'*S or S*S' to/from R.
C
C                          Workspace: N*N + N*M.
C
                           IF ( LTRANS ) THEN
                              CALL DGEMM( NT, NT, N, M, N, ONE, DWORK,
     $                                    N, G, LDG, ZERO, DWORK(WP),
     $                                    N )
                              CALL DSYRK( UPLO, NT, N, M, BETA,
     $                                    DWORK(WP), N, ONE, R, LDR )
                           ELSE
                              CALL DGEMM( TR, NT, M, N, N, ONE, G, LDG,
     $                                    DWORK, N, ZERO, DWORK(WP), M )
                              CALL DSYRK( UPLO, TR, N, M, BETA,
     $                                    DWORK(WP), M, ONE, R, LDR )
                           END IF
C
                        ELSE
C
C                          Compute D*D' in DWORK(WP) and then update R.
C
C                          Workspace: 3*N*N.
C
                           CALL DSYRK(  UPLO, NT, N, M, ONE, G, LDG,
     $                                  ZERO, DWORK(WP), N )
                           CALL MB01RU( UPLO, NTRANS, N, N, ONE, BETA,
     $                                  R, LDR, DWORK, N, DWORK(WP), N,
     $                                  DWORK(WP+NN), NN, INFO )
                        END IF
C
                     ELSE
C
                        IF ( USEC ) THEN
C
C                          Compute in DWORK the following product:
C                             S = D'*W  ( TRANS = 'N' ) or
C                             S = W*D   ( TRANS = 'T' ).
C                          Then, add/subtract V = S'*S or V = S*S'
C                          to/from R, and compute
C                             op(C) = A +/- D*S   ( TRANS = 'N' ) or
C                             op(C) = A +/- S*D'  ( TRANS = 'T' ).
C
C                          Workspace: N*M.
C
                           IF ( LTRANS ) THEN
                              CALL DGEMM(  NT, NT, N, M, N, ONE, C, LDC,
     $                                     G, LDG, ZERO, DWORK, N )
                              CALL DSYRK(  UPLO, NT, N, M, BETA, DWORK,
     $                                     N, ONE, R, LDR )
                              CALL DLACPY( 'All', N, N, A, LDA, C, LDC )
                              CALL DGEMM(  NT, TR, N, N, M, BETA, DWORK,
     $                                     N, G, LDG, ONE, C, LDC )
                           ELSE
                              CALL DGEMM(  TR, NT, M, N, N, ONE, G, LDG,
     $                                     C, LDC, ZERO, DWORK, M )
                              CALL DSYRK(  UPLO, TR, N, M, BETA, DWORK,
     $                                     M, ONE, R, LDR )
                              CALL DLACPY( 'All', N, N, A, LDA, C, LDC )
                              CALL DGEMM(  NT, NT, N, N, M, BETA, G,
     $                                     LDG, DWORK, M, ONE, C, LDC )
                           END IF
C
                        ELSE
C
C                          Compute Y = D*D' in DWORK(WP), and Z in C,
C                             Z = +/- Y*W  ( TRANS = 'N' ) or
C                             Z = +/- W*Y  ( TRANS = 'T' ).
C
C                          Workspace: 2*N*N.
C
                           CALL DSYRK( UPLO, NT, N, M, ONE, G, LDG,
     $                                 ZERO, DWORK(WP), N )
C
                           CALL DSYMM( SIDE, UPLO, N, N, BETA,
     $                                 DWORK(WP), N, DWORK, N, ZERO, C,
     $                                 LDC )
C
C                          Update R,
C                             R = R + W'*Z,  if TRANS = 'N';
C                             R = R + Z*W',  if TRANS = 'T'.
C
                           CALL MB01RB( SIDE, UPLO, TR, N, N, ONE, ONE,
     $                                  R, LDR, DWORK, N, C, LDC, INFO )
C
C                          Compute  op(C) = A + Z.
C
                           DO 540 J = 1, N
                              CALL DAXPY( N, ONE, A(1,J), 1, C(1,J), 1 )
  540                      CONTINUE
C
                        END IF
C
                     END IF
C
                  ELSE IF ( NLJOBR ) THEN
C
                     CALL DLACPY( 'All', N, N, A, LDA, C, LDC )
C
                  END IF
C
               END IF
C
            END IF
C
            IF ( NLJOBC ) THEN
C
               IF ( UNITE ) THEN
C
C                 Subtract X from R.
C
                  IF ( LUPLO ) THEN
C
                     DO 550 J = 1, N
                        CALL DAXPY( J, -ONE, X(1,J), 1, R(1,J), 1 )
  550                CONTINUE
C
                  ELSE
C
                     DO 560 J = 1, N
                        CALL DAXPY( N-J+1, -ONE, X(J,J), 1, R(J,J), 1 )
  560                CONTINUE
C
                  END IF
C
               ELSE IF ( .NOT.LJOBL ) THEN
C
C                 Subtract op(E)'*X*op(E) from R.
C
C                 Workspace: N*N.
C
                  CALL MB01RU( UPLO, NTRANS, N, N, ONE, -ONE, R, LDR,
     $                         E, LDE, X, LDX, DWORK, NN, INFO )
               END IF
C
            END IF
C
         ELSE
C
C           Continuous-time case.
C
            USEC  = LJOBA .AND. USE1
            FULLX = UNITE .AND. .NOT.KEEPX
C
            IF ( LJOBE .AND. .NOT.LJOBL ) THEN
C
               IF ( USEC ) THEN
C
C                 Compute the following product, stored in C:
C                    W = X*E ( TRANS = 'N' ) or W = E*X ( TRANS = 'T' ).
C
                  CALL DSYMM( SIDE, UPLO, N, N, ONE, X, LDX, E, LDE,
     $                        ZERO, C, LDC )
C
               ELSE IF ( LJOBG .OR. NLJOBC ) THEN
C
C                 Compute W above in DWORK(WP).
C
C                 Workspace: WP+N*N-1.
C
                  CALL DSYMM( SIDE, UPLO, N, N, ONE, X, LDX, E, LDE,
     $                        ZERO, DWORK(WP), N )
               END IF
C
            ELSE IF ( USEC ) THEN
               IF ( KEEPX ) THEN
                  CALL DLACPY( UPLO, N, N, X, LDX, C, LDC )
                  CALL MA02ED( UPLO, N, C, LDC )
               END IF
            END IF
C
            IF ( LJOBL ) THEN
C
               IF ( NLJOBC ) THEN
C
C                 Compute Q + T + T' in the array R, where
C                    T = A'*W  or  T = A'*X             ( TRANS = 'N' )
C                 or
C                    T = A*W'  or  T = A*X              ( TRANS = 'T' ).
C
                  IF ( FULLX ) THEN
C
                     CALL DSYR2K( UPLO, NTRANS, N, N, ONE, A, LDA, X,
     $                            LDX, ONE, R, LDR )
C
                  ELSE IF ( UNITE ) THEN
C
                     IF ( LJOBR ) THEN
C
C                       Copy the matrix X to DWORK.
C
C                       Workspace: N*N.
C
                        CALL DLACPY( UPLO, N, N, X, LDX, DWORK, N )
                        CALL MA02ED( UPLO, N, DWORK, N )
                        CALL DSYR2K( UPLO, NTRANS, N, N, ONE, A, LDA,
     $                               DWORK, N, ONE, R, LDR )
                     ELSE
C
C                       Copy the matrix X to C.
C
                        CALL DLACPY( UPLO, N, N, X, LDX, C, LDC )
                        CALL MA02ED( UPLO, N, C, LDC )
                        CALL DSYR2K( UPLO, NTRANS, N, N, ONE, A, LDA, C,
     $                               LDC, ONE, R, LDR )
                     END IF
C
                  ELSE
C
                     CALL DSYR2K( UPLO, NTRANS, N, N, ONE, A, LDA, XE,
     $                            LDXE, ONE, R, LDR )
                  END IF
C
C                 Add/subtract F*F' or H*K to/from R.
C
                  IF ( LJOBF ) THEN
                     CALL DSYRK( UPLO, NT, N, M, BETA, F, LDF, ONE, R,
     $                           LDR )
                  ELSE
                     CALL MB01RB( 'Left', UPLO, NT, N, M, ONE, BETA, R,
     $                            LDR, F, LDF, K, LDK, INFO )
                  END IF
               END IF
C
               IF ( NLJOBR ) THEN
C
C                 Compute op(C) = A +/- op(D*F') or
C                         op(C) = A +/- op(B*K).
C
                  CALL DLACPY( 'All', N, N, A, LDA, C, LDC )
C
                  IF ( LJOBF ) THEN
                     IF ( LTRANS ) THEN
                        CALL DGEMM( NT, TR, N, N, M, BETA, F, LDF, G,
     $                              LDG, ONE, C, LDC )
                     ELSE
                        CALL DGEMM( NT, TR, N, N, M, BETA, G, LDG, F,
     $                              LDF, ONE, C, LDC )
                     END IF
C
                  ELSE
C
                     IF ( LTRANS ) THEN
                        CALL DGEMM( TR, TR, N, N, M, BETA, K, LDK, G,
     $                              LDG, ONE, C, LDC )
                     ELSE
                        CALL DGEMM( NT, NT, N, N, M, BETA, G, LDG, K,
     $                              LDK, ONE, C, LDC )
                     END IF
C
                  END IF
C
               END IF
C
            ELSE
C
               IF ( LJOBC ) THEN
C
C                 Compute op(C) = A +/- op(G*W) or op(C) = A +/- op(G*X)
C
                  IF ( LJOBG ) THEN
C
                     CALL DLACPY( 'All', N, N, A, LDA, C, LDC )
C
                     IF ( FULLX ) THEN
C
                        CALL DSYMM(  SIDE, UPLO, N, N, BETA, G, LDG, X,
     $                               LDX, ONE, C, LDC )
                     ELSE
C
                        IF ( UNITE ) THEN
C
C                          Copy the matrix X to DWORK.
C
C                          Workspace: N*N.
C
                           CALL DLACPY( UPLO, N, N, X, LDX, DWORK, N )
                           CALL MA02ED( UPLO, N, DWORK, N )
                        END IF
C
                        CALL DSYMM( SIDE, UPLO, N, N, BETA, G, LDG,
     $                              DWORK, N, ONE, C, LDC )
                     END IF
C
                  ELSE IF ( M.EQ.0 ) THEN
C
                     CALL DLACPY( 'All', N, N, A, LDA, C, LDC )
C
                  ELSE IF ( LJOBE ) THEN
C
                     IF ( USE1 ) THEN
C
C                       Compute X*D in C.
C
                        CALL DSYMM( 'Left', UPLO, N, M, ONE, X, LDX, G,
     $                              LDG, ZERO, C, LDC )
C
C                       Compute in DWORK
C                          W = D'*X*E  ( TRANS = 'N' ) or
C                          W = E*X*D   ( TRANS = 'T' ).
C                       Then, compute
C                          op(C) = A +/- D*W   ( TRANS = 'N' ) or
C                          op(C) = A +/- W*D'  ( TRANS = 'T' ).
C
C                       Workspace: N*M.
C
                        IF ( LTRANS ) THEN
                           CALL DGEMM( NT, NT, N, M, N, ONE, E, LDE, C,
     $                                 LDC, ZERO, DWORK, N )
                           CALL DLACPY( 'All', N, N, A, LDA, C, LDC )
                           CALL DGEMM( NT, TR, N, N, M, BETA, DWORK, N,
     $                                 G, LDG, ONE, C, LDC )
                        ELSE
                           CALL DGEMM( TR, NT, M, N, N, ONE, C, LDC, E,
     $                                 LDE, ZERO, DWORK, M )
                           CALL DLACPY( 'All', N, N, A, LDA, C, LDC )
                           CALL DGEMM( NT, NT, N, N, M, BETA, G, LDG,
     $                                 DWORK, M, ONE, C, LDC )
                        END IF
C
                     ELSE
C
C                       Compute Y = D*D' in C, Y*X or X*Y in DWORK, and
C                       then compute op(C).
C
C                       Workspace: N*N.
C
                        CALL DSYRK(  UPLO, NT, N, M, ONE, G, LDG, ZERO,
     $                               C, LDC )
                        CALL MA02ED( UPLO, N, C, LDC )
                        CALL DSYMM(  NSIDE, UPLO, N, N, ONE, X, LDX, C,
     $                               LDC, ZERO, DWORK, N )
                        CALL DLACPY( 'All', N, N, A, LDA, C, LDC )
                        IF ( LTRANS ) THEN
                           CALL DGEMM( NT, NT, N, N, N, BETA, E, LDE,
     $                                 DWORK, N, ONE, C, LDC )
                        ELSE
                           CALL DGEMM( NT, NT, N, N, N, BETA, DWORK, N,
     $                                 E, LDE, ONE, C, LDC )
                        END IF
C
                     END IF
C
                  ELSE
C
                     CALL DLACPY( 'All', N, N, A, LDA, C, LDC )
C
                     IF ( USE1 ) THEN
C
C                       Compute S = X*D in DWORK.
C
C                       Workspace: N*M.
C
                        CALL DSYMM( 'Left', UPLO, N, M, ONE, X, LDX, G,
     $                              LDG, ZERO, DWORK, N )
C
C                       Compute
C                          op(C) = A +/- D*S'  ( TRANS = 'N' ) or
C                          op(C) = A +/- S*D'  ( TRANS = 'T' ).
C
                        IF ( LTRANS ) THEN
                           CALL DGEMM( NT, TR, N, N, M, BETA, DWORK, N,
     $                                 G, LDG, ONE, C, LDC )
                        ELSE
                           CALL DGEMM( NT, TR, N, N, M, BETA, G, LDG,
     $                                 DWORK, N, ONE, C, LDC )
                        END IF
C
                     ELSE
C
C                       Compute D*D' in DWORK and then compute op(C).
C
C                       Workspace: N*N.
C
                        CALL DSYRK(  UPLO, NT, N, M, ONE, G, LDG, ZERO,
     $                               DWORK, N )
                        CALL MA02ED( UPLO, N, DWORK, N )
                        CALL DSYMM(  NSIDE, UPLO, N, N, BETA, X, LDX,
     $                               DWORK, N, ONE, C, LDC )
                     END IF
C
                  END IF
C
               ELSE
C
C                 Compute R (and op(C)). Start computation of R.
C
                  IF ( USEATW ) THEN
C
C                    Compute Q + T' + T in the array R, where
C                       T = A'*W  or  T = A'*X          ( TRANS = 'N' )
C                    or
C                       T = A*W'  or  T = A*X           ( TRANS = 'T' ).
C
                     IF ( FULLX ) THEN
C
                        CALL DSYR2K( UPLO, NTRANS, N, N, ONE, A, LDA, X,
     $                               LDX, ONE, R, LDR )
C
                     ELSE IF ( USEC ) THEN
C
                        CALL DSYR2K( UPLO, NTRANS, N, N, ONE, A, LDA, C,
     $                               LDC, ONE, R, LDR )
                     ELSE
C
                        IF ( UNITE ) THEN
C
C                          Copy the matrix X to DWORK(WP).
C
C                          Workspace: WP+N*N-1.
C
                           CALL DLACPY( UPLO, N, N, X, LDX, DWORK(WP),
     $                                  N )
                           CALL MA02ED( UPLO, N, DWORK(WP), N )
                        END IF
C
                        CALL DSYR2K( UPLO, NTRANS, N, N, ONE, A, LDA,
     $                               DWORK(WP), N, ONE, R, LDR )
                     END IF
C
                  END IF
C
                  ALPHA = BETA/TWO
C
                  IF ( LJOBG ) THEN
C
                     IF ( LJOBR ) THEN
C
C                       Compute R only.
C                       Compute in DWORK the following matrix:
C                          Z = A +/- 0.5*G*W | Z = A +/- 0.5*W*G
C                                                  if JOBE = 'G',  or
C                          Z = A +/- 0.5*op(G*X),  if JOBE = 'I'.
C                       Then, similarly compute R:
C                          R = Q + W'*Z + Z'*W | R = Q + W*Z' + Z*W' or
C                          R = Q + Z'*X +  X*Z | R = Q + Z*X  + X*Z'.
C
                        CALL DLACPY( 'All', N, N, A, LDA, DWORK, N )
C
                        IF ( FULLX ) THEN
C
C                          Workspace: N*N.
C
                           CALL DSYMM(  SIDE, UPLO, N, N, ALPHA, G, LDG,
     $                                  X, LDX, ONE, DWORK, N )
                           CALL DSYR2K( UPLO, NTRANS, N, N, ONE, DWORK,
     $                                  N, X, LDX, ONE, R, LDR )
                        ELSE
C
                           IF ( UNITE ) THEN
C
C                             Copy the matrix X to DWORK(WP).
C
C                             Workspace: 2*N*N.
C
                              CALL DLACPY( UPLO, N, N, X, LDX,
     $                                     DWORK(WP), N )
                              CALL MA02ED( UPLO, N, DWORK(WP), N )
                           END IF
C
                           CALL DSYMM(  SIDE, UPLO, N, N, ALPHA, G, LDG,
     $                                  DWORK(WP), N, ONE, DWORK, N )
                           CALL DSYR2K( UPLO, NTRANS, N, N, ONE, DWORK,
     $                                  N, DWORK(WP), N, ONE, R, LDR )
                        END IF
C
                     ELSE
C
C                       Compute R and op(C).
C
                        IF ( LDWORK.LT.OPTWRK ) THEN
C
                           IF ( FULLX ) THEN
C
                              CALL DSYMM(  SIDE, UPLO, N, N, BETA, G,
     $                                     LDG, X, LDX, ZERO, C, LDC )
                              CALL MB01RB( SIDE, UPLO, NT, N, N, ONE,
     $                                     ONE, R, LDR, X, LDX, C, LDC,
     $                                     INFO )
                           ELSE
C
                              IF ( UNITE ) THEN
C
C                                Copy the matrix X to DWORK(WP).
C
C                                Workspace: 2*N*N.
C
                                 CALL DLACPY( UPLO, N, N, X, LDX,
     $                                        DWORK(WP), N )
                                 CALL MA02ED( UPLO, N, DWORK(WP), N )
                              END IF
C
                              CALL DSYMM( SIDE, UPLO, N, N, BETA, G,
     $                                    LDG, DWORK(WP), N, ZERO, C,
     $                                    LDC )
                              CALL MB01RB( SIDE, UPLO, TR, N, N, ONE,
     $                                     ONE, R, LDR, DWORK(WP), N, C,
     $                                     LDC, INFO )
                           END IF
C
C                          Compute  op(C) = A + C.
C
                           DO 570 J = 1, N
                              CALL DAXPY( N, ONE, A(1,J), 1, C(1,J), 1 )
  570                      CONTINUE
C
                        ELSE
C
C                          Compute in DWORK the following matrix:
C                             Z = A +/- 0.5*G*W | Z = A +/- 0.5*W*G or
C                             Z = A +/- 0.5*op(G*X).
C                          This is done in two steps, allowing to
C                          get op(C). Then, compute R:
C                             R = Q + W'*Z + Z'*W | R = Q + W*Z' + Z*W';
C                             R = Q + Z'*X +  X*Z | R = Q + Z*X  + X*Z'.
C
                           I = 1
                           IF ( FULLX ) THEN
C
C                             Workspace: N*N.
C
                              CALL DSYMM(  SIDE, UPLO, N, N, ALPHA, G,
     $                                     LDG, X, LDX, ZERO, DWORK, N )
C
C                             Compute  op(C) = A + 2*Z and Z := A + Z.
C
                              DO 580 J = 1, N
                                 CALL DCOPY( N, A(1,J), 1, C(1,J), 1 )
                                 CALL DAXPY( N, TWO, DWORK(I), 1,
     $                                       C(1,J), 1 )
                                 CALL DAXPY( N, ONE, A(1,J), 1,
     $                                       DWORK(I), 1 )
                                 I = I + N
  580                         CONTINUE
C
                              CALL DSYR2K( UPLO, NTRANS, N, N, ONE,
     $                                     DWORK, N, X, LDX, ONE, R,
     $                                     LDR )
                           ELSE
C
                              IF ( UNITE ) THEN
C
C                                Copy the matrix X to DWORK(WP).
C
C                                Workspace: 2*N*N.
C
                                 CALL DLACPY( UPLO, N, N, X, LDX,
     $                                        DWORK(WP), N )
                                 CALL MA02ED( UPLO, N, DWORK(WP), N )
                              END IF
C
                              CALL DSYMM( SIDE, UPLO, N, N, ALPHA, G,
     $                                    LDG, DWORK(WP), N, ZERO,
     $                                    DWORK, N )
C
C                             Compute  op(C) = A + 2*Z and Z := A + Z.
C
                              DO 590 J = 1, N
                                 CALL DCOPY( N, A(1,J), 1, C(1,J), 1 )
                                 CALL DAXPY( N, TWO, DWORK(I), 1,
     $                                       C(1,J), 1 )
                                 CALL DAXPY( N, ONE, A(1,J), 1,
     $                                       DWORK(I), 1 )
                                 I = I + N
  590                         CONTINUE
C
                              CALL DSYR2K( UPLO, NTRANS, N, N, ONE,
     $                                     DWORK, N, DWORK(WP), N, ONE,
     $                                     R, LDR )
                           END IF
C
                        END IF
C
                     END IF
C
                  ELSE IF ( M.EQ.0 ) THEN
C
                     IF ( NLJOBR )
     $                  CALL DLACPY( 'All', N, N, A, LDA, C, LDC )
C
                  ELSE
C
C                    Use D.
C
                     IF ( LJOBR ) THEN
C
C                       Compute R only.
C
                        IF ( USEATW ) THEN
C
                           IF ( LJOBE ) THEN
C
C                             Compute in DWORK
C                             S = D'*W or S = W*D  (TRANS = 'N' or 'T').
C
C                             Workspace: N*N + N*M.
C
                              IF ( LTRANS ) THEN
                                 CALL DGEMM( NT, NT, N, M, N, ONE,
     $                                       DWORK(WP), N, G, LDG, ZERO,
     $                                       DWORK, N )
                                 J = N
                              ELSE
                                 CALL DGEMM( TR, NT, M, N, N, ONE, G,
     $                                       LDG, DWORK(WP), N, ZERO,
     $                                       DWORK, M )
                                 J = M
                              END IF
C
C                             Update R = R +/- S'*S or R = R +/- S*S'.
C
                              CALL DSYRK( UPLO, NTRANS, N, M, BETA,
     $                                    DWORK, J, ONE, R, LDR )
                           ELSE
C
C                             Compute S = X*D in DWORK.
C
C                             Workspace: N*M.
C
                              CALL DSYMM( 'Left', UPLO, N, M, ONE, X,
     $                                    LDX, G, LDG, ZERO, DWORK, N )
C
C                             Update R = R +/- S*S'.
C
                              CALL DSYRK( UPLO, NT, N, M, BETA, DWORK,
     $                                    N, ONE, R, LDR )
                           END IF
C
                        ELSE
C
C                          Compute Y = D*D' in DWORK(YP) and compute
C                          in DWORK the following matrix:
C                             Z = A +/- 0.5*Y*W | Z = A +/- 0.5*W*Y,  or
C                             Z = A +/- 0.5*op(Y*X).
C                          Then, compute R:
C                             R = Q + W'*Z + Z'*W | R = Q + W*Z' + Z*W';
C                             R = Q + Z'*X +  X*Z | R = Q + Z*X  + X*Z'.
C
                           CALL DSYRK(  UPLO, NT, N, M, ONE, G, LDG,
     $                                  ZERO, DWORK(YP), N )
                           CALL DLACPY( 'All', N, N, A, LDA, DWORK, N )
C
                           IF ( FULLX ) THEN
C
C                             Workspace: 2*N*N.
C
                              CALL DSYMM(  SIDE, UPLO, N, N, ALPHA,
     $                                     DWORK(YP), N, X, LDX, ONE,
     $                                     DWORK, N )
                              CALL DSYR2K( UPLO, NTRANS, N, N, ONE,
     $                                     DWORK, N, X, LDX, ONE, R,
     $                                     LDR )
                           ELSE
C
C                             Workspace: 3*N*N.
C
                              IF ( UNITE ) THEN
C
C                                Copy the matrix X to DWORK(WP).
C
                                 CALL DLACPY( UPLO, N, N, X, LDX,
     $                                        DWORK(WP), N )
                                 CALL MA02ED( UPLO, N, DWORK(WP), N )
                              END IF
C
                              CALL DSYMM(  SIDE, UPLO, N, N, ALPHA,
     $                                     DWORK(YP), N, DWORK(WP), N,
     $                                     ONE, DWORK, N )
                              CALL DSYR2K( UPLO, NTRANS, N, N, ONE,
     $                                     DWORK, N, DWORK(WP), N, ONE,
     $                                     R, LDR )
                           END IF
C
                        END IF
C
                     ELSE
C
C                       Compute R and op(C).
C
                        IF ( USE1 ) THEN
C
C                          Workspace: N*M.
C
                           IF ( LJOBE ) THEN
C
C                             Compute S in DWORK
C                             S = D'*C | S = C*D  (TRANS = 'N' or 'T'),
C                             op(C) = A +/- D*S | op(C) = A +/- S*D',
C                             and update R,
C                             R = R +/- S'*S | R = R +/- S*S'.
C
                              IF ( LTRANS ) THEN
                                 CALL DGEMM(  NT, NT, N, M, N, ONE, C,
     $                                        LDC, G, LDG, ZERO, DWORK,
     $                                        N )
                                 CALL DLACPY( 'All', N, N, A, LDA, C,
     $                                        LDC )
                                 CALL DGEMM(  NT, TR, N, N, M, BETA,
     $                                        DWORK, N, G, LDG, ONE, C,
     $                                        LDC )
                                 CALL DSYRK(  UPLO, NT, N, M, BETA,
     $                                        DWORK, N, ONE, R, LDR )
                              ELSE
                                 CALL DGEMM(  TR, NT, M, N, N, ONE, G,
     $                                        LDG, C, LDC, ZERO, DWORK,
     $                                        M )
                                 CALL DLACPY( 'All', N, N, A, LDA, C,
     $                                        LDC )
                                 CALL DGEMM(  NT, NT, N, N, M, BETA, G,
     $                                        LDG, DWORK, M, ONE, C,
     $                                        LDC )
                                 CALL DSYRK(  UPLO, TR, N, M, BETA,
     $                                        DWORK, M, ONE, R, LDR )
                              END IF
C
                           ELSE
C
C                             Compute S = X*D in DWORK,
C                             op(C) = A +/- op(D*S'),
C                             and update R, R = R +/- S*S'.
C
                              CALL DSYMM( 'Left', UPLO, N, M, ONE, X,
     $                                    LDX, G, LDG, ZERO, DWORK, N )
                              CALL DLACPY( 'All', N, N, A, LDA, C, LDC )
                              IF ( LTRANS ) THEN
                                 CALL DGEMM( NT, TR, N, N, M, BETA,
     $                                       DWORK, N, G, LDG, ONE, C,
     $                                       LDC )
                              ELSE
                                 CALL DGEMM( NT, TR, N, N, M, BETA, G,
     $                                       LDG, DWORK, N, ONE, C,
     $                                       LDC )
                              END IF
                              CALL DSYRK( UPLO, NT, N, M, BETA, DWORK,
     $                                    N, ONE, R, LDR )
                           END IF
C
                        ELSE IF ( LCNDS ) THEN
C
C                          Compute in C
C                             S = D'*W | S = W*D  (JOBE  = 'G'), or
C                             S = X*D             (JOBE  = 'I'),
C                          and similarly compute in DWORK the product
C                             Z = +/- 0.5*D*S | Z = +/- 0.5*S*D', or
C                             Z = +/- 0.5*op(D*S').
C                          Then, compute
C                             op(C) = A + 2*Z;
C                             Z = A + Z;
C                             R = Q + W'*Z + Z'*W | R = Q + W*Z' + Z*W';
C                             R = Q + Z'*X +  X*Z | R = Q + Z*X  + X*Z'.
C
C                          Workspace: need    (IA+1)*N*N;
C                                     prefer       2*N*N.
C
                           IF ( LJOBE ) THEN
C
                              IF ( LTRANS ) THEN
                                 CALL DGEMM( NT, NT, N, M, N, ONE,
     $                                       DWORK(WP), N, G, LDG, ZERO,
     $                                       C, LDC )
                                 CALL DGEMM( NT, TR, N, N, M, ALPHA, C,
     $                                       LDC, G, LDG, ZERO, DWORK,
     $                                       N )
                              ELSE
                                 CALL DGEMM( TR, NT, M, N, N, ONE, G,
     $                                       LDG, DWORK(WP), N, ZERO,
     $                                       C, LDC )
                                 CALL DGEMM( NT, NT, N, N, M, ALPHA, G,
     $                                       LDG, C, LDC, ZERO, DWORK,
     $                                       N )
                              END IF
C
                           ELSE
C
                              CALL DSYMM( 'Left', UPLO, N, M, ONE, X,
     $                                    LDX, G, LDG, ZERO, C, LDC )
                              IF ( LTRANS ) THEN
                                 CALL DGEMM( NT, TR, N, N, M, ALPHA, C,
     $                                       LDC, G, LDG, ZERO, DWORK,
     $                                       N )
                              ELSE
                                 CALL DGEMM( NT, TR, N, N, M, ALPHA, G,
     $                                       LDG, C, LDC, ZERO, DWORK,
     $                                       N )
                              END IF
C
                           END IF
C
                           I = 1
C
                           DO 600 J = 1, N
                              CALL DCOPY( N, A(1,J), 1, C(1,J), 1 )
                              CALL DAXPY( N, TWO, DWORK(I), 1, C(1,J),
     $                                    1 )
                              CALL DAXPY( N, ONE, A(1,J), 1, DWORK(I),
     $                                    1 )
                              I = I + N
  600                      CONTINUE
C
                           IF ( FULLX ) THEN
C
C                             Workspace: N*N.
C
                              CALL DSYR2K( UPLO, NTRANS, N, N, ONE,
     $                                     DWORK, N, X, LDX, ONE, R,
     $                                     LDR )
                           ELSE
C
                              IF ( UNITE ) THEN
C
C                                Copy the matrix X to DWORK(WP).
C
C                                Workspace: 2*N*N.
C
                                 CALL DLACPY( UPLO, N, N, X, LDX,
     $                                        DWORK(WP), N )
                                 CALL MA02ED( UPLO, N, DWORK(WP), N )
                              END IF
C
                              CALL DSYR2K( UPLO, NTRANS, N, N, ONE,
     $                                     DWORK, N, DWORK(WP), N, ONE,
     $                                     R, LDR )
                           END IF
C
                        ELSE
C
C                          Compute Y = D*D' in C and compute in DWORK
C                          the following product:
C                             Z = +/- 0.5*Y*W | Z = +/- 0.5*W*Y,
C                                                   if JOBE = 'G', or
C                             Z = +/- 0.5*op(Y*X),  if JOBE = 'I'.
C                          Then, compute
C                             op(C) = A + 2*Z;
C                             Z = A + Z,
C                             R = Q + W'*Z + Z'*W | R = Q + W*Z' + Z*W';
C                             R = Q + Z'*X +  X*Z | R = Q + Z*X  + X*Z'.
C
C                          Workspace: need    (IA+1)*N*N;
C                                     prefer       2*N*N.
C
                           CALL DSYRK( UPLO, NT, N, M, ONE, G, LDG,
     $                                 ZERO, C, LDC )
C
                           IF ( FULLX ) THEN
C
C                             Workspace: N*N.
C
                              CALL DSYMM(  SIDE, UPLO, N, N, ALPHA, C,
     $                                     LDC, X, LDX, ZERO, DWORK, N )
                           ELSE
C
                              IF ( UNITE ) THEN
C
C                                Copy the matrix X to DWORK(WP).
C
C                                Workspace: 2*N*N.
C
                                 CALL DLACPY( UPLO, N, N, X, LDX,
     $                                        DWORK(WP), N )
                                 CALL MA02ED( UPLO, N, DWORK(WP), N )
                              END IF
C
                              CALL DSYMM( SIDE, UPLO, N, N, ALPHA, C,
     $                                    LDC, DWORK(WP), N, ZERO,
     $                                    DWORK, N )
                           END IF
C
                           I = 1
C
                           DO 610 J = 1, N
                              CALL DCOPY( N, A(1,J), 1, C(1,J), 1 )
                              CALL DAXPY( N, TWO, DWORK(I), 1, C(1,J),
     $                                    1 )
                              CALL DAXPY( N, ONE, A(1,J), 1, DWORK(I),
     $                                    1 )
                              I = I + N
  610                      CONTINUE
C
                           IF ( FULLX ) THEN
C
                              CALL DSYR2K( UPLO, NTRANS, N, N, ONE,
     $                                     DWORK, N, X, LDX, ONE, R,
     $                                     LDR )
                           ELSE
C
                              CALL DSYR2K( UPLO, NTRANS, N, N, ONE,
     $                                     DWORK, N, DWORK(WP), N, ONE,
     $                                     R, LDR )
                           END IF
C
                        END IF
                     END IF
                  END IF
               END IF
            END IF
         END IF
      END IF
C
      DWORK(1) = MAX( 1, OPTWRK )
C
      RETURN
C *** Last line of SG02CW ***
      END
