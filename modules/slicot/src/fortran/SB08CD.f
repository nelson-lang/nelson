      SUBROUTINE SB08CD( DICO, N, M, P, A, LDA, B, LDB, C, LDC, D, LDD,
     $                   NQ, NR, BR, LDBR, DR, LDDR, TOL, DWORK, LDWORK,
     $                   IWARN, INFO )
C
C     PURPOSE
C
C     To construct, for a given system G = (A,B,C,D), an output
C     injection matrix H, an orthogonal transformation matrix Z, and a
C     gain matrix V, such that the systems
C
C          Q = (Z'*(A+H*C)*Z, Z'*(B+H*D), V*C*Z, V*D)
C     and
C          R = (Z'*(A+H*C)*Z, Z'*H, V*C*Z, V)
C
C     provide a stable left coprime factorization of G in the form
C                   -1
C              G = R  * Q,
C
C     where G, Q and R are the corresponding transfer-function matrices
C     and the denominator R is co-inner, that is, R(s)*R'(-s) = I in
C     the continuous-time case, or R(z)*R'(1/z) = I in the discrete-time
C     case. The Z matrix is not explicitly computed.
C
C     Note: G must have no observable poles on the imaginary axis
C     for a continuous-time system, or on the unit circle for a
C     discrete-time system. If the given state-space representation
C     is not detectable, the undetectable part of the original
C     system is automatically deflated and the order of the systems
C     Q and R is accordingly reduced.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     DICO    CHARACTER*1
C             Specifies the type of the original system as follows:
C             = 'C':  continuous-time system;
C             = 'D':  discrete-time system.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The dimension of the state vector, i.e. the order of the
C             matrix A, and also the number of rows of the matrices B
C             and BR, and the number of columns of the matrix C.
C             N >= 0.
C
C     M       (input) INTEGER
C             The dimension of input vector, i.e. the number of columns
C             of the matrices B and D.  M >= 0.
C
C     P       (input) INTEGER
C             The dimension of output vector, i.e. the number of rows
C             of the matrices C, D and DR, and the number of columns
C             of the matrices BR and DR.  P >= 0.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the state dynamics matrix A. The matrix A must not
C             have observable eigenvalues on the imaginary axis, if
C             DICO = 'C', or on the unit circle, if DICO = 'D'.
C             On exit, the leading NQ-by-NQ part of this array contains
C             the leading NQ-by-NQ part of the matrix Z'*(A+H*C)*Z, the
C             state dynamics matrix of the numerator factor Q, in a
C             real Schur form. The leading NR-by-NR part of this matrix
C             represents the state dynamics matrix of a minimal
C             realization of the denominator factor R.
C
C     LDA     INTEGER
C             The leading dimension of array A.  LDA >= MAX(1,N).
C
C     B       (input/output) DOUBLE PRECISION array, dimension
C             (LDB,MAX(M,P))
C             On entry, the leading N-by-M part of this array must
C             contain the input/state matrix.
C             On exit, the leading NQ-by-M part of this array contains
C             the leading NQ-by-M part of the matrix Z'*(B+H*D), the
C             input/state matrix of the numerator factor Q.
C             The remaining part of this array is needed as workspace.
C
C     LDB     INTEGER
C             The leading dimension of array B.  LDB >= MAX(1,N).
C
C     C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
C             On entry, the leading P-by-N part of this array must
C             contain the state/output matrix C.
C             On exit, the leading P-by-NQ part of this array contains
C             the leading P-by-NQ part of the matrix V*C*Z, the
C             state/output matrix of the numerator factor Q.
C             The first NR columns of this array represent the
C             state/output matrix of a minimal realization of the
C             denominator factor R.
C             The remaining part of this array is needed as workspace.
C
C     LDC     INTEGER
C             The leading dimension of array C.
C             LDC >= MAX(1,M,P), if N > 0.
C             LDC >= 1,          if N = 0.
C
C     D       (input/output) DOUBLE PRECISION array, dimension
C             (LDD,MAX(M,P))
C             On entry, the leading P-by-M part of this array must
C             contain the input/output matrix.
C             On exit, the leading P-by-M part of this array contains
C             the matrix V*D representing the input/output matrix
C             of the numerator factor Q.
C             The remaining part of this array is needed as workspace.
C
C     LDD     INTEGER
C             The leading dimension of array D.  LDD >= MAX(1,M,P).
C
C     NQ      (output) INTEGER
C             The order of the resulting factors Q and R.
C             Generally, NQ = N - NS, where NS is the number of
C             unobservable eigenvalues outside the stability region.
C
C     NR      (output) INTEGER
C             The order of the minimal realization of the factor R.
C             Generally, NR is the number of observable eigenvalues
C             of A outside the stability region (the number of modified
C             eigenvalues).
C
C     BR      (output) DOUBLE PRECISION array, dimension (LDBR,P)
C             The leading NQ-by-P part of this array contains the
C             leading NQ-by-P part of the output injection matrix
C             Z'*H, which reflects the eigenvalues of A lying outside
C             the stable region to values which are symmetric with
C             respect to the imaginary axis (if DICO = 'C') or the unit
C             circle (if DICO = 'D'). The first NR rows of this matrix
C             form the input/state matrix of a minimal realization of
C             the denominator factor R.
C
C     LDBR    INTEGER
C             The leading dimension of array BR.  LDBR >= MAX(1,N).
C
C     DR      (output) DOUBLE PRECISION array, dimension (LDDR,P)
C             The leading P-by-P part of this array contains the lower
C             triangular matrix V representing the input/output matrix
C             of the denominator factor R.
C
C     LDDR    INTEGER
C             The leading dimension of array DR.  LDDR >= MAX(1,P).
C
C     Tolerances
C
C     TOL     DOUBLE PRECISION
C             The absolute tolerance level below which the elements of
C             C are considered zero (used for observability tests).
C             If the user sets TOL <= 0, then an implicitly computed,
C             default tolerance, defined by  TOLDEF = N*EPS*NORM(C),
C             is used instead, where EPS is the machine precision
C             (see LAPACK Library routine DLAMCH) and NORM(C) denotes
C             the infinity-norm of C.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The dimension of working array DWORK.
C             LDWORK >= MAX( 1, P*N + MAX( N*(N+5),P*(P+2),4*P,4*M ) ).
C             For optimum performance LDWORK should be larger.
C
C     Warning Indicator
C
C     IWARN   INTEGER
C             = 0:  no warning;
C             = K:  K violations of the numerical stability condition
C                   NORM(H) <= 10*NORM(A)/NORM(C) occured during the
C                   assignment of eigenvalues.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value;
C             = 1:  the reduction of A to a real Schur form failed;
C             = 2:  a failure was detected during the ordering of the
C                   real Schur form of A, or in the iterative process
C                   for reordering the eigenvalues of Z'*(A + H*C)*Z
C                   along the diagonal;
C             = 3:  if DICO = 'C' and the matrix A has an observable
C                   eigenvalue on the imaginary axis, or DICO = 'D' and
C                   A has an observable eigenvalue on the unit circle.
C
C     METHOD
C
C     The subroutine uses the right coprime factorization algorithm with
C     inner denominator of [1] applied to G'.
C
C     REFERENCES
C
C     [1] Varga A.
C         A Schur method for computing coprime factorizations with
C         inner denominators and applications in model reduction.
C         Proc. ACC'93, San Francisco, CA, pp. 2130-2131, 1993.
C
C     NUMERICAL ASPECTS
C                                            3
C     The algorithm requires no more than 14N  floating point
C     operations.
C
C     CONTRIBUTOR
C
C     A. Varga, German Aerospace Center,
C     DLR Oberpfaffenhofen, July 1998.
C     Based on the RASP routine LCFID.
C
C     REVISIONS
C
C     Nov. 1998, V. Sima, Research Institute for Informatics, Bucharest.
C     Dec. 1998, V. Sima, Katholieke Univ. Leuven, Leuven.
C     May  2003, A. Varga, DLR Oberpfaffenhofen.
C     Nov  2003, A. Varga, DLR Oberpfaffenhofen.
C
C     KEYWORDS
C
C     Coprime factorization, eigenvalue, eigenvalue assignment,
C     feedback control, pole placement, state-space model.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ONE, ZERO
      PARAMETER         ( ONE = 1.0D0, ZERO = 0.0D0 )
C     .. Scalar Arguments ..
      CHARACTER         DICO
      INTEGER           INFO, IWARN, LDA, LDB, LDBR, LDC, LDD, LDDR,
     $                  LDWORK, M, N, NQ, NR, P
      DOUBLE PRECISION  TOL
C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), B(LDB,*), BR(LDBR,*), C(LDC,*),
     $                  D(LDD,*), DR(LDDR,*), DWORK(*)
C     .. Local Scalars ..
      INTEGER           I, KBR, KW
C     .. External Functions ..
      LOGICAL           LSAME
      EXTERNAL          LSAME
C     .. External subroutines ..
      EXTERNAL          AB07MD, DLASET, DSWAP, MA02AD, MA02BD, SB08DD,
     $                  TB01XD, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         DBLE, MAX, MIN
C     .. Executable Statements ..
C
      IWARN = 0
      INFO  = 0
C
C     Check the scalar input parameters.
C
      IF( .NOT.LSAME( DICO, 'C' ) .AND.
     $    .NOT.LSAME( DICO, 'D' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( P.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -8
      ELSE IF( LDC.LT.1 .OR. ( N.GT.0 .AND. LDC.LT.MAX( M, P ) ) )
     $      THEN
         INFO = -10
      ELSE IF( LDD.LT.MAX( 1, M, P ) ) THEN
         INFO = -12
      ELSE IF( LDBR.LT.MAX( 1, N ) ) THEN
         INFO = -16
      ELSE IF( LDDR.LT.MAX( 1, P ) ) THEN
         INFO = -18
      ELSE IF( LDWORK.LT.MAX( 1, P*N + MAX( N*(N+5), P*(P+2), 4*P,
     $                        4*M ) ) ) THEN
         INFO = -21
      END IF
      IF( INFO.NE.0 )THEN
C
C        Error return.
C
         CALL XERBLA( 'SB08CD', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( MIN( N, P ).EQ.0 ) THEN
         NQ = 0
         NR = 0
         DWORK(1) = ONE
         CALL DLASET( 'Full', P, P, ZERO, ONE, DR, LDDR )
         RETURN
      END IF
C
C     Compute the dual system G' = (A',C',B',D').
C
      CALL AB07MD( 'D', N, M, P, A, LDA, B, LDB, C, LDC, D, LDD,
     $             INFO )
C
C     Compute the right coprime factorization with inner
C     denominator of G'.
C
C     Workspace needed:      P*N;
C     Additional workspace:  need  MAX( N*(N+5), P*(P+2), 4*P, 4*M );
C                            prefer larger.
C
      KBR = 1
      KW  = KBR + P*N
      CALL SB08DD( DICO, N, P, M, A, LDA, B, LDB, C, LDC, D, LDD,
     $             NQ, NR, DWORK(KBR), P, DR, LDDR, TOL, DWORK(KW),
     $             LDWORK-KW+1, IWARN, INFO )
      IF( INFO.EQ.0 ) THEN
C
C        Determine the elements of the left coprime factorization from
C        those of the computed right coprime factorization and make the
C        state-matrix upper real Schur.
C
         CALL TB01XD( 'D', NQ, P, M, MAX( 0, NQ-1 ), MAX( 0, NQ-1 ),
     $                A, LDA, B, LDB, C, LDC, D, LDD, INFO )
C
         CALL MA02AD( 'Full', P, NQ, DWORK(KBR), P, BR, LDBR )
         CALL MA02BD( 'Left', NQ, P, BR, LDBR )
C
         DO 10 I = 2, P
            CALL DSWAP( I-1, DR(I,1), LDDR, DR(1,I), 1 )
   10    CONTINUE
C
      END IF
C
      DWORK(1) = DWORK(KW) + DBLE( KW-1 )
C
      RETURN
C *** Last line of SB08CD ***
      END
