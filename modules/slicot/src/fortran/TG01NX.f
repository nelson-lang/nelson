      SUBROUTINE TG01NX( JOBT, N, M, P, NDIM, A, LDA, E, LDE, B, LDB,
     $                   C, LDC, Q, LDQ, Z, LDZ, IWORK, INFO )
C
C     PURPOSE
C
C     To compute equivalence transformation matrices Q and Z which
C     reduce the regular pole pencil A-lambda*E of the descriptor system
C     (A-lambda*E,B,C), with (A,E) in a generalized real Schur form, to
C     the block-diagonal form
C
C                ( A1  0  )             ( E1  0  )
C        Q*A*Z = (        ) ,   Q*E*Z = (        ) ,                 (1)
C                ( 0   A2 )             ( 0   E2 )
C
C     where the pair (Q*A*Z,Q*E*Z) is in a generalized real Schur form,
C     with (A1,E1) and (A2,E2) having no common generalized eigenvalues.
C     This decomposition corresponds to an additive spectral
C     decomposition of the transfer-function matrix of the descriptor
C     system as the sum of two terms containing the generalized
C     eigenvalues of (A1,E1) and (A2,E2), respectively.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOBT    CHARACTER*1
C             = 'D':  compute the direct transformation matrices;
C             = 'I':  compute the inverse transformation matrices
C                     inv(Q) and inv(Z).
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The number of rows of the matrix B, the number of columns
C             of the matrix C and the order of the square matrices A
C             and E.  N >= 0.
C
C     M       (input) INTEGER
C             The number of columns of the matrix B.  M >= 0.
C
C     P       (input) INTEGER
C             The number of rows of the matrix C.  P >= 0.
C
C     NDIM    (input) INTEGER
C             The dimension of the leading diagonal blocks of (A,E)
C             having generalized eigenvalues distinct from those of the
C             trailing diagonal block.  0 <= NDIM <= N.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the N-by-N state matrix A in a real Schur form.
C             On exit, the leading N-by-N part of this array contains
C             the transformed state matrix Q*A*Z (if JOBT = 'D') or
C             inv(Q)*A*inv(Z) (if JOBT = 'I'), in the form (1), where
C             A1 is a NDIM-by-NDIM matrix.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     E       (input/output) DOUBLE PRECISION array, dimension (LDE,N)
C             On entry, the leading N-by-N part of this array must
C             contain the N-by-N descriptor matrix E in upper triangular
C             form.
C             On exit, the leading N-by-N part of this array contains
C             the transformed descriptor matrix Q*E*Z (if JOBT = 'D') or
C             inv(Q)*E*inv(Z) (if JOBT = 'I'), in the form (1), where
C             E1 is an NDIM-by-NDIM matrix.
C
C     LDE     INTEGER
C             The leading dimension of the array E.  LDE >= MAX(1,N).
C
C     B       (input/output) DOUBLE PRECISION array, dimension (LDB,M)
C             On entry, the leading N-by-M part of this array must
C             contain the N-by-M input matrix B.
C             On exit, the leading N-by-M part of this array contains
C             the transformed input matrix Q*B (if JOBT = 'D') or
C             inv(Q)*B (if JOBT = 'I').
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= MAX(1,N).
C
C     C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
C             On entry, the leading P-by-N part of this array must
C             contain the state/output matrix C.
C             On exit, the leading P-by-N part of this array contains
C             the transformed matrix C*Z (if JOBT = 'D') or C*inv(Z)
C             (if JOBT = 'I').
C
C     LDC     INTEGER
C             The leading dimension of the array C.  LDC >= MAX(1,P).
C
C     Q       (input/output) DOUBLE PRECISION array, dimension (LDQ,N)
C             On entry, the leading N-by-N part of this array contains
C             Q1, the orthogonal left transformation matrix Q used to
C             reduce the pair (A,E) to the generalized real Schur form.
C             On exit, the leading N-by-N part of this array contains
C             the left transformation matrix Q = Q2*Q1, if JOBT = 'D',
C             or its inverse inv(Q), if JOBT = 'I'.
C
C     LDQ     INTEGER
C             The leading dimension of the array Q.  LDQ >= MAX(1,N).
C
C     Z       (input/output) DOUBLE PRECISION array, dimension (LDZ,N)
C             On entry, the leading N-by-N part of this array contains
C             the orthogonal right transformation matrix Z1 used to
C             reduce the pair (A,E) to the generalized real Schur form.
C             On exit, the leading N-by-N part of this array contains
C             the right transformation matrix Z = Z1*Z2, if JOBT = 'D',
C             or its inverse inv(Z), if JOBT = 'I'.
C
C     LDZ     INTEGER
C             The leading dimension of the array Z.  LDZ >= MAX(1,N).
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (N+6)
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value;
C             = 1:  the separation of the two diagonal blocks failed
C                   because of very close eigenvalues.
C
C     METHOD
C
C     For the separation, transformation matrices Q2 and Z2 of the form
C
C             ( I -X )          ( I  Y )
C        Q2 = (      ) ,   Z2 = (      )
C             ( 0  I )          ( 0  I )
C
C     are determined, such that Q2*A*Z2 and Q2*E*Z2 are block diagonal
C     as in (1). X and Y are computed by solving generalized Sylvester
C     equations.
C
C     If we partition Q2*B and C*Z2 according to (1) in the form ( B1 )
C                                                                ( B2 )
C     and ( C1 C2 ), then (A1-lambda*E1,B1,C1) and (A2-lambda*E2,B2,C2)
C     represent an additive spectral decomposition of the system
C     transfer-function matrix.
C
C     REFERENCES
C
C     [1] Kagstrom, B. and Van Dooren, P.
C         Additive decomposition of a transfer function with respect
C         to a specified region.
C         Proc. MTNS Symp., Brussels, 1989.
C
C     NUMERICAL ASPECTS
C
C     The algorithm is numerically backward stable and requires
C     0( N**3 )  floating point operations.
C
C     CONTRIBUTOR
C
C     A. Varga, German Aerospace Center, DLR Oberpfaffenhofen.
C     November 2002.
C
C     REVISIONS
C
C     V. Sima, Dec. 2016.
C
C     KEYWORDS
C
C     Generalized eigenvalue problem, system poles, multivariable
C     system, additive decomposition.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D0, ZERO = 0.0D0 )
C     .. Scalar Arguments ..
      CHARACTER          JOBT
      INTEGER            INFO, LDA, LDB, LDC, LDE, LDQ, LDZ, M, N, NDIM,
     $                   P
C     .. Array Arguments ..
      INTEGER            IWORK(*)
      DOUBLE PRECISION   A(LDA,*), B(LDB,*), C(LDC,*), E(LDE,*),
     $                   Q(LDQ,*), Z(LDZ,*)
C     .. Local Scalars ..
      LOGICAL            TRINV
      DOUBLE PRECISION   DIF, SCALE
      INTEGER            I, N1, N11, N2
C     .. Local Arrays ..
      DOUBLE PRECISION   DUM(1)
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     .. External Subroutines ..
      EXTERNAL           DGEMM, DLASET, DSWAP, DTGSYL, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
C     .. Executable Statements ..
C
C     Test the input parameters.
C
      INFO  =  0
      TRINV =  LSAME( JOBT, 'I' )
      IF( .NOT.LSAME( JOBT, 'D' ) .AND. .NOT.TRINV  ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( P.LT.0 ) THEN
         INFO = -4
      ELSE IF( NDIM.LT.0 .OR. NDIM.GT.N ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDE.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -11
      ELSE IF( LDC.LT.MAX( 1, P ) ) THEN
         INFO = -13
      ELSE IF( LDQ.LT.MAX( 1, N ) ) THEN
         INFO = -15
      ELSE IF( LDZ.LT.MAX( 1, N ) ) THEN
         INFO = -17
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'TG01NX', -INFO )
         RETURN
      END IF
C
C     Quick return if possible
C
      IF( N.EQ.0 )
     $   RETURN
C
      IF( TRINV ) THEN
C
C        Transpose Z in-situ.
C
         DO 10 I = 2, N
            CALL DSWAP( I-1, Z(1,I), 1, Z(I,1), LDZ )
   10    CONTINUE
C
C        Transpose Q in-situ.
C
         DO 20 I = 2, N
            CALL DSWAP( I-1, Q(1,I), 1, Q(I,1), LDQ )
   20    CONTINUE
      END IF
C
C     Let be A and E partitioned as ( A11 A12 ) and ( E11 E12 ).
C                                   (  0  A22 )     (  0  E22 )
C     Split the pairs (A11,E11) and (A22,E22) by using the following
C     left and right transformation matrices
C             ( I -X )          ( I  Y )
C        Q2 = (      ) ,   Z2 = (      ) ,
C             ( 0  I )          ( 0  I )
C     where X and Y are computed by solving the generalized
C     Sylvester equations
C
C              A11 * Y - X * A22 = scale * A12
C              E11 * Y - X * E22 = scale * E12.
C
C     -Y is computed in A12 and -X is computed in E12.
C
C     Integer workspace: need  N+6.
C
      N1  = NDIM
      N2  = N - NDIM
      N11 = MIN( N1 + 1, N )
C
      IF( N1.GT.0 .AND. N2.GT.0 ) THEN
         CALL DTGSYL( 'No transpose', 0, N1, N2, A, LDA, A(N11,N11),
     $                LDA, A(1,N11), LDA, E, LDE, E(N11,N11), LDE,
     $                E(1,N11), LDE, SCALE, DIF, DUM, 1, IWORK, INFO )
         IF( INFO.NE.0 ) THEN
            INFO = 1
            RETURN
         END IF
C
C        Transform B = ( B1 ) and C = ( C1 C2 ).
C                      ( B2 )
C
         IF( SCALE.GT.0 )
     $       SCALE = ONE/SCALE
C
C        B1 <- B1 - X*B2.
C
         CALL DGEMM( 'N', 'N', N1, M, N2, SCALE, E(1,N11), LDE,
     $               B(N11,1), LDB, ONE, B, LDB )
C
C        C2 <- C2 + C1*Y.
C
         CALL DGEMM( 'N', 'N', P, N2, N1, -SCALE, C, LDC, A(1,N11),
     $               LDA, ONE, C(1,N11), LDC )
C
         IF( TRINV ) THEN
C
C           Transform Q1 = ( Q11 Q12 ) and Z1 = ( Z11 ).
C                                               ( Z21 )
C
C           Q12 <- Q12 + Q11*X.
C
            CALL DGEMM( 'N', 'N', N, N2, N1, -SCALE, Q, LDQ, E(1,N11),
     $                  LDE, ONE, Q(1,N11), LDQ )
C
C           Z11 <- Z11 - Y*Z21.
C
            CALL DGEMM( 'N', 'N', N1, N, N2, SCALE, A(1,N11), LDA,
     $                  Z(N11,1), LDZ, ONE, Z, LDZ )
         ELSE
C
C           Transform Q1 = ( Q11 ) and Z1 = ( Z11 Z12 ).
C                          ( Q21 )
C
C           Q11 <- Q11 - X*Q21.
C
            CALL DGEMM( 'N', 'N', N1, N, N2, SCALE, E(1,N11), LDE,
     $                  Q(N11,1), LDQ, ONE, Q, LDQ )
C
C           Z12 <- Z12 + Z11*Y.
C
            CALL DGEMM( 'N', 'N', N, N2, N1, -SCALE, Z, LDZ, A(1,N11),
     $                  LDA, ONE, Z(1,N11), LDZ )
         END IF
C
C        Set A12 and E12 to zero.
C
         CALL DLASET( 'Full', N1, N2, ZERO, ZERO, A(1,N11), LDA )
         CALL DLASET( 'Full', N1, N2, ZERO, ZERO, E(1,N11), LDE )
      END IF
C
      RETURN
C *** Last line of TG01NX ***
      END
