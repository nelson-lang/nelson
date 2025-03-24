      SUBROUTINE SG02CV( DICO, JOB, JOBE, UPLO, TRANS, N, A, LDA, E,
     $                   LDE, X, LDX, R, LDR, NORMS, DWORK, LDWORK,
     $                   INFO )
C
C     PURPOSE
C
C     To compute the residual matrix R for a continuous-time or
C     discrete-time "reduced" Lyapunov equation, using the formulas
C
C        R = op(A)'*X + X*op(A) + Q,
C     or
C        R = op(A)'*X*op(E) + op(E)'*X*op(A) + Q,
C
C     in the continuous-time case, or the formulas
C
C        R = op(A)'*X*op(A) - X + Q,
C     or
C        R = op(A)'*X*op(A) - op(E)'*X*op(E) + Q,
C
C     in the discrete-time case, where X and Q are symmetric matrices,
C     A is in upper real Schur form, E is upper triangular, and op(W) is
C
C        op(W) = W   or   op(W) = W'.
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
C             Specifies the type of the Lyapunov equation, as follows:
C             = 'C':  continuous-time Lyapunov equation;
C             = 'D':  discrete-time Lyapunov equation.
C
C     JOB     CHARACTER*1
C             Specifies which results must be computed, as follows:
C             = 'R':  The matrix R only must be computed;
C             = 'N':  The matrix R and the norms must be computed;
C             = 'B':  The matrix R and the norms must be computed.
C
C     JOBE    CHARACTER*1
C             Specifies whether E is a general or an identity matrix,
C             as follows:
C             = 'G':  The matrix E is general and is given;
C             = 'I':  The matrix E is assumed identity and is not given.
C
C     UPLO    CHARACTER*1
C             Specifies which triangles of the symmetric matrices X and
C             Q are given, as follows:
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
C             The order of the matrices A, E, Q, X, and R.  N >= 0.
C
C     A       (input) DOUBLE PRECISION array, dimension (LDA,N)
C             The leading N-by-N upper part of this array must contain
C             the upper real Schur matrix A.
C             If TRANS = 'N' and (DICO = 'D' or (JOB = 'R' and
C             JOBE = 'G')), the entries 3, 4,..., N of the first column
C             are modified internally, but are restored on exit.
C             Otherwise, the part of this array below the first
C             subdiagonal is not referenced.
C
C     LDA     INTEGER
C             The leading dimension of array A.  LDA >= MAX(1,N).
C
C     E       (input) DOUBLE PRECISION array, dimension (LDE,*)
C             If JOBE = 'G', the leading N-by-N upper triangular part of
C             this array must contain the upper triangular matrix E.
C             The strictly lower triangular part of this array is not
C             referenced.
C             If JOBE = 'I', this array is not referenced.
C
C     LDE     INTEGER
C             The leading dimension of array E.
C             LDE >= MAX(1,N), if JOBE = 'G';
C             LDE >= 1,        if JOBE = 'I'.
C
C     X       (input/works.) DOUBLE PRECISION array, dimension (LDX,N)
C             On entry, if UPLO = 'U', the leading N-by-N upper
C             triangular part of this array must contain the upper
C             triangular part of the symmetric matrix X and the strictly
C             lower triangular part of the array is not referenced.
C             On entry, if UPLO = 'L', the leading N-by-N lower
C             triangular part of this array must contain the lower
C             triangular part of the symmetric matrix X and the strictly
C             upper triangular part of the array is not referenced.
C             If DICO = 'D' or (JOB = 'R' and JOBE = 'G'), the diagonal
C             elements of this array are modified internally, but they
C             are restored on exit.
C
C     LDX     INTEGER
C             The leading dimension of array X.  LDX >= MAX(1,N).
C
C     R       (input/output) DOUBLE PRECISION array, dimension (LDR,*)
C             On entry, the leading N-by-N upper or lower triangular
C             part (depending on UPLO) of this array must contain the
C             upper or lower triangular part, respectively, of the
C             matrix Q. The other strictly triangular part is not
C             referenced.
C             On exit, the leading N-by-N upper or lower triangular
C             part (depending on UPLO) of this array contains the upper
C             or lower triangular part, respectively, of the matrix R.
C
C     LDR     INTEGER
C             The leading dimension of array R.  LDR >= MAX(1,N).
C
C     NORMS   (output) DOUBLE PRECISION array, dimension (LN)
C             If JOB = 'N' or JOB = 'B', LN = 1 or 2, if (DICO = 'C' or
C             JOBE = 'I'), or (DICO = 'D' and JOBE = 'G'), respectively.
C             If DICO = 'C',
C             NORMS(1) contains the Frobenius norm of the matrix
C             op(A)'*X (or of X*op(A)), if JOBE = 'I', or of the matrix
C             op(A)'*X*op(E) (or of op(E)'*X*op(A)), if JOBE = 'G'.
C             If DICO = 'D',
C             NORMS(1) contains the Frobenius norm of the matrix
C             op(A)'*X*op(A);
C             if JOBE = 'G', NORMS(2) contains the Frobenius norm of the
C             matrix op(E)'*X*op(E).
C             If JOB <> 'N', this array is not referenced.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = -17 or if LDWORK = -2 on input, then
C             DWORK(1) returns the minimum value of LDWORK.
C             On exit, if INFO = 0, or if LDWORK = -1 on input, then
C             DWORK(1) returns the optimal value of LDWORK.
C
C     LDWORK  The length of the array DWORK. LDWORK >= MAX(v,1), with v
C             specified in the following table, where
C                a = 1, if JOBE = 'G';
C                a = 0, if JOBE = 'I'.
C
C             DICO   JOB             v
C             ----------------------------
C             'C'    'R'           a*N*N
C             'C'    'N','B'         N*N
C             ----------------------------
C             'D'    'R'             N*N
C             'D'    'N','B'       2*N*N
C             ----------------------------
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
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value.
C
C     METHOD
C
C     The matrix expressions are efficiently evaluated, using symmetry.
C     If JOB = 'N' or JOB = 'B', then:
C     If DICO = 'C', the matrices op(op(A)'*X*op(E)) or op(X*op(A)), are
C     efficiently computed.
C     If DICO = 'D', the matrices op(A)'*X*op(A) and op(E)'*X*op(E), if
C     JOBE = 'G', are efficiently computed. The results are used to
C     evaluate R and the norms.
C     If JOB = 'R', then the needed parts of the intermediate results
C     are obtained and used to evaluate R.
C
C     NUMERICAL ASPECTS
C
C     The calculations are backward stable.
C
C     The algorithm requires approximately a*N^3 operations, where ^
C     denotes the power operator, and
C
C        a = 1,      if DICO = 'C' and JOB <> 'R' and JOBE = 'G';
C        a = 1/2,    otherwise.
C
C     An "operation" includes a multiplication, an addition, and some
C     address calculations.
C
C     CONTRIBUTORS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Feb. 2019.
C
C     REVISIONS
C
C     V. Sima, Apr. 2019.
C
C     KEYWORDS
C
C     Algebraic Lyapunov equation, elementary matrix operations, matrix
C     algebra, matrix operations.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0 )
C     .. Scalar Arguments ..
      CHARACTER         DICO, JOB, JOBE, TRANS, UPLO
      INTEGER           INFO, LDA, LDE, LDR, LDWORK, LDX, N
C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), DWORK(*), E(LDE,*), NORMS(*),
     $                  R(LDR,*), X(LDX,*)
C     .. Local Scalars ..
      CHARACTER         NTRANS
      LOGICAL           DISCR, LJOBE, LJOBN, LJOBR, LTRANS, LUPLO,
     $                  UNITE
      INTEGER           J, MINWRK, NN, OPTWRK
C     .. External Functions ..
      LOGICAL           LSAME
      DOUBLE PRECISION  DLANGE, DLANSY
      EXTERNAL          DLANGE, DLANSY, LSAME
C     .. External Subroutines ..
      EXTERNAL          DAXPY, MB01OC, MB01OD, MB01OO, MB01OS, MB01RH,
     $                  MB01RT, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         MAX
C     .. Executable Statements ..
C
C     Test the input scalar arguments.
C
      INFO = 0
C
      DISCR  = LSAME( DICO,  'D' )
      LJOBN  = LSAME( JOB,   'N' ) .OR. LSAME( JOB, 'B' )
      LJOBR  = LSAME( JOB,   'R' )
      LJOBE  = LSAME( JOBE,  'G' )
      LUPLO  = LSAME( UPLO,  'U' )
      LTRANS = LSAME( TRANS, 'T' ) .OR. LSAME( TRANS, 'C' )
      UNITE  = .NOT.LJOBE
C
      IF (     .NOT.DISCR  .AND. .NOT.LSAME( DICO,  'C' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.LJOBN  .AND. .NOT.LJOBR ) THEN
         INFO = -2
      ELSE IF(      UNITE  .AND. .NOT.LSAME( JOBE,  'I' ) ) THEN
         INFO = -3
      ELSE IF( .NOT.LUPLO  .AND. .NOT.LSAME( UPLO,  'L' ) ) THEN
         INFO = -4
      ELSE IF( .NOT.LTRANS .AND. .NOT.LSAME( TRANS, 'N' ) ) THEN
         INFO = -5
      ELSE IF( N.LT.0 ) THEN
         INFO = -6
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -8
      ELSE IF( LDE.LT.1  .OR. ( LJOBE .AND. LDE.LT.N ) ) THEN
         INFO = -10
      ELSE IF( LDX.LT.MAX( 1, N ) ) THEN
         INFO = -12
      ELSE IF( LDR.LT.MAX( 1, N ) ) THEN
         INFO = -14
      ELSE
         NN = N*N
C
         IF ( LJOBN ) THEN
            IF ( DISCR ) THEN
               MINWRK = 2*NN
            ELSE
               MINWRK = NN
            END IF
         ELSE IF ( .NOT.DISCR .AND. UNITE ) THEN
            MINWRK = 0
         ELSE
            MINWRK = NN
         END IF
C
         OPTWRK = MINWRK
C
         IF ( LDWORK.EQ.-2 ) THEN
            DWORK(1) = MAX( 1, MINWRK )
            RETURN
         ELSE IF ( LDWORK.EQ.-1 ) THEN
            DWORK(1) = MAX( 1, OPTWRK )
            RETURN
         END IF
C
         IF ( LDWORK.LT.MINWRK ) THEN
            INFO = -17
            DWORK(1) = MAX( 1, MINWRK )
         END IF
      END IF
C
      IF ( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'SG02CV', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF ( N.EQ.0 ) THEN
         IF ( .NOT.LJOBR ) THEN
            NORMS(1) = ZERO
            IF ( DISCR .AND. LJOBE )
     $         NORMS(2) = ZERO
         END IF
         RETURN
      END IF
C
      IF ( LTRANS ) THEN
         NTRANS = 'NoTran'
      ELSE
         NTRANS = 'Tran'
      END IF
C
      IF ( LJOBR ) THEN
C
C        JOB = 'R'.
C
         IF ( DISCR ) THEN
C
C           Discrete-time case.
C
C           Compute in R a triangle of the symmetric matrix
C              R = Q + A'*X*A,  if TRANS = 'N';
C              R = Q + A*X*A',  if TRANS = 'T'.
C
C           Workspace: N*N.
C
            CALL MB01RH( UPLO, NTRANS, N, ONE, ONE, R, LDR, A, LDA, X,
     $                   LDX, DWORK, LDWORK, INFO )
C
            IF ( UNITE ) THEN
C
C              Subtract the triangle of X from R.
C
               IF ( LUPLO ) THEN
C
                  DO 10 J = 1, N
                     CALL DAXPY( J, -ONE, X(1,J), 1, R(1,J), 1 )
   10             CONTINUE
C
               ELSE
C
                  DO 20 J = 1, N
                     CALL DAXPY( N-J+1, -ONE, X(J,J), 1, R(J,J), 1 )
   20             CONTINUE
C
               END IF
C
            ELSE
C
C              Subtract the triangle of op(E)'*X*op(E) from R.
C
C              Workspace: N*N.
C
               CALL MB01RT( UPLO, NTRANS, N, ONE, -ONE, R, LDR, E, LDE,
     $                      X, LDX, DWORK, LDWORK, INFO )
            END IF
C
         ELSE
C
C           Continuous-time case.
C
            IF ( LJOBE ) THEN
C
C              Compute in R a triangle of
C                 R = op(A)'*X*op(E) + op(E)'*X*op(A) + Q.
C
C              Workspace N*N.
C
               CALL MB01OD( UPLO, NTRANS, N, ONE, ONE, R, LDR, A, LDA,
     $                      X, LDX, E, LDE, DWORK, NN, INFO )
C
            ELSE
C
C              Compute in R a triangle of
C                 R = op(A)'*X + X*op(A) + Q.
C
               CALL MB01OC( UPLO, NTRANS, N, ONE, ONE, R, LDR, A, LDA,
     $                      X, LDX, INFO )
            END IF
C
         END IF
C
      ELSE
C
C        JOB = 'B' or 'N'.
C
         IF ( DISCR ) THEN
C
C           Discrete-time case.
C
            IF ( LJOBE ) THEN
C
C              Compute in DWORK a triangle of op(E)'*X*op(E) and its
C              norm.
C
C              Workspace: 2*N*N.
C
               CALL MB01RT( UPLO, NTRANS, N, ZERO, ONE, DWORK, N, E,
     $                      LDE, X, LDX, DWORK(NN+1), LDWORK, INFO )
C
               NORMS(2) = DLANSY( 'F-norm', UPLO, N, DWORK, N, DWORK )
C
C              Compute in R the triangle of Q - op(E)'*X*op(E).
C
               IF ( LUPLO ) THEN
C
                  DO 30 J = 1, N
                     CALL DAXPY( J, -ONE, DWORK(1+(J-1)*N), 1, R(1,J),
     $                           1 )
   30             CONTINUE
C
               ELSE
C
                  DO 40 J = 1, N
                     CALL DAXPY( N-J+1, -ONE, DWORK(J+(J-1)*N), 1,
     $                           R(J,J), 1 )
   40             CONTINUE
C
               END IF
C
            ELSE
C
C              Compute in R the triangle of Q - X.
C
               IF ( LUPLO ) THEN
C
                  DO 50 J = 1, N
                     CALL DAXPY( J, -ONE, X(1,J), 1, R(1,J), 1 )
   50             CONTINUE
C
               ELSE
C
                  DO 60 J = 1, N
                     CALL DAXPY( N-J+1, -ONE, X(J,J), 1, R(J,J), 1 )
   60             CONTINUE
C
               END IF
C
            END IF
C
C           Compute in DWORK a triangle of op(A)'*X*op(A) and its norm.
C
C           Workspace: 2*N*N.
C
            CALL MB01RH( UPLO, NTRANS, N, ZERO, ONE, DWORK, N, A, LDA,
     $                   X, LDX, DWORK(NN+1), NN, INFO )
C
            NORMS(1) = DLANSY( 'F-norm', UPLO, N, DWORK, N, DWORK )
C
C           Update the triangle of R := R + op(A)'*X*op(A).
C
            IF ( LUPLO ) THEN
C
               DO 70 J = 1, N
                  CALL DAXPY( J, ONE, DWORK(1+(J-1)*N), 1, R(1,J), 1 )
   70          CONTINUE
C
            ELSE
C
               DO 80 J = 1, N
                  CALL DAXPY( N-J+1, ONE, DWORK(J+(J-1)*N), 1, R(J,J),
     $                        1 )
   80          CONTINUE
C
            END IF
C
         ELSE
C
C           Continuous-time case.
C
            IF ( LJOBE ) THEN
C
C              Compute in DWORK
C                 E'*X*A,  if TRANS = 'N';
C                 A*X*E',  if TRANS = 'T'.
C
C              Workspace: N*N.
C
               CALL MB01OO( UPLO, NTRANS, N, A, LDA, X, LDX, E, LDE,
     $                      DWORK, N, INFO )
C
            ELSE
C
C              Compute in DWORK
C                 A*X,  if TRANS = 'N';
C                 X*A,  if TRANS = 'T'.
C
C              Workspace: N*N.
C
               CALL MB01OS( UPLO, NTRANS, N, A, LDA, X, LDX, DWORK, N,
     $                      INFO )
C
            END IF
C
C           Compute the norm of the product P.
C
            NORMS(1) = DLANGE( 'F-norm', N, N, DWORK, N, DWORK )
C
C           Compute in R a triangle of Q + P + P'.
C
            IF ( LUPLO ) THEN
C
               DO 90 J = 1, N
                  CALL DAXPY( J, ONE, DWORK(1+(J-1)*N), 1, R(1,J), 1 )
                  CALL DAXPY( J, ONE, DWORK(J), N, R(1,J), 1 )
   90          CONTINUE
C
            ELSE
C
               DO 100 J = 1, N
                  CALL DAXPY( N-J+1, ONE, DWORK(J+(J-1)*N), 1, R(J,J),
     $                        1 )
                  CALL DAXPY( N-J+1, ONE, DWORK(J+(J-1)*N), N, R(J,J),
     $                        1 )
  100          CONTINUE
C
            END IF
C
         END IF
C
      END IF
C
      DWORK(1) = MAX( 1, OPTWRK )
C
      RETURN
C *** Last line of SG02CV ***
      END
