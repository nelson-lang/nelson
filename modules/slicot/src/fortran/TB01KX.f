      SUBROUTINE TB01KX( N, M, P, NDIM, A, LDA, B, LDB, C, LDC, U, LDU,
     $                   V, LDV, INFO )
C
C     PURPOSE
C
C     To compute an additive spectral decomposition of the transfer-
C     function matrix of the system (A,B,C) by reducing the system
C     state-matrix A to a block-diagonal form. It is assumed that A is
C     in a real Schur form, and the leading diagonal block of order NDIM
C     has eigenvalues distinct from those of the trailing diagonal
C     block. The system matrices are transformed as
C        A <-- V*A*U, B <--V*B and C <-- C*U, where V = inv(U),
C     preserving the spectra of the two diagonal blocks.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the state-space representation, i.e., the
C             order of the matrix A.  N >= 0.
C
C     M       (input) INTEGER
C             The number of system inputs, or of columns of B.  M >= 0.
C
C     P       (input) INTEGER
C             The number of system outputs, or of rows of C.  P >= 0.
C
C     NDIM    (input) INTEGER
C             The dimension of the leading diagonal block of A having
C             eigenvalues distinct from those of the trailing diagonal
C             block.  0 <= NDIM <= N.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the state dynamics matrix A in real Schur form.
C             On exit, the leading N-by-N part of this array contains a
C             block diagonal matrix inv(U) * A * U with two diagonal
C             blocks in real Schur form, with the elements below the
C             first subdiagonal set to zero. The leading block has
C             dimension NDIM-by-NDIM.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     B       (input/output) DOUBLE PRECISION array, dimension (LDB,M)
C             On entry, the leading N-by-M part of this array must
C             contain the input matrix B.
C             On exit, the leading N-by-M part of this array contains
C             the transformed input matrix inv(U) * B.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= MAX(1,N).
C
C     C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
C             On entry, the leading P-by-N part of this array must
C             contain the output matrix C.
C             On exit, the leading P-by-N part of this array contains
C             the transformed output matrix C * U.
C
C     LDC     INTEGER
C             The leading dimension of the array C.  LDC >= MAX(1,P).
C
C     U       (input/output) DOUBLE PRECISION array, dimension (LDU,N)
C             On entry, the leading N-by-N part of this array must
C             contain an initial transformation matrix U.
C             On exit, the leading N-by-N part of this array contains
C             the transformation matrix used to reduce A to the block-
C             diagonal form. The first NDIM columns of U span the
C             invariant subspace of A corresponding to the eigenvalues
C             of its leading diagonal block. The last N-NDIM columns of
C             U span the reducing subspace of A corresponding to the
C             eigenvalues of the trailing diagonal block of A.
C
C     LDU     INTEGER
C             The leading dimension of the array U.  LDU >= max(1,N).
C
C     V       (output) DOUBLE PRECISION array, dimension (LDV,N)
C             The leading N-by-N part of this array contains the
C             inverse of the transformation matrix U used to reduce A
C             to the block-diagonal form.
C
C     LDV     INTEGER
C             The leading dimension of the array V.  LDV >= max(1,N).
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0: successful exit;
C             < 0: if INFO = -i, the i-th argument had an illegal value;
C             = 1: the separation of the two diagonal blocks failed
C                  because of very close eigenvalues.
C
C     METHOD
C
C     A similarity transformation U is determined that reduces the given
C     system state-matrix A to a block-diagonal form (with two diagonal
C     blocks), so that the eigenvalues of the leading diagonal block of
C     the resulting A are preserved. The determined transformation is
C     applied to the system (A,B,C) as
C        A <-- inv(U)*A*U, B <-- inv(U)*B and C <-- C*U.
C
C     REFERENCES
C
C     [1] Safonov, M.G., Jonckheere, E.A., Verma, M., Limebeer, D.J.N.
C         Synthesis of positive real multivariable feedback systems.
C         Int. J. Control, pp. 817-842, 1987.
C
C     NUMERICAL ASPECTS
C                                   3
C     The algorithm requires about N /2 + NDIM*(N-NDIM)*(2*N+M+P)
C     floating point operations.
C
C     CONTRIBUTOR
C
C     A. Varga, German Aerospace Center,
C     DLR Oberpfaffenhofen, March 1998.
C     Based on the RASP routine SADSDC.
C
C     REVISIONS
C
C     V. Sima, Dec. 2016.
C
C     KEYWORDS
C
C     Invariant subspace, real Schur form, similarity transformation,
C     spectral factorization.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION ZERO, ONE
      PARAMETER        ( ZERO = 0.0D0, ONE = 1.0D0 )
C     .. Scalar Arguments ..
      INTEGER          INFO, LDA, LDB, LDC, LDU, LDV, M, N, NDIM, P
C     .. Array Arguments ..
      DOUBLE PRECISION A(LDA,*), B(LDB,*), C(LDC,*), U(LDU,*), V(LDV,*)
C     .. Local Scalars ..
      INTEGER          NDIM1, NR
      DOUBLE PRECISION SCALE
C     .. External Subroutines ..
      EXTERNAL         DGEMM, DLASET, DTRSYL, MA02AD, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC        MAX
C
C     .. Executable Statements ..
C
      INFO = 0
C
C     Check input scalar arguments.
C
      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( M.LT.0 ) THEN
         INFO = -2
      ELSE IF( P.LT.0 ) THEN
         INFO = -3
      ELSE IF( NDIM.LT.0 .OR. NDIM.GT.N ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -8
      ELSE IF( LDC.LT.MAX( 1, P ) ) THEN
         INFO = -10
      ELSE IF( LDU.LT.MAX( 1, N ) ) THEN
         INFO = -12
      ELSE IF( LDV.LT.MAX( 1, N ) ) THEN
         INFO = -14
      END IF
C
      IF( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'TB01KX', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( N.EQ.0 )
     $   RETURN
C
C     Form U' in V.
C
      CALL MA02AD( 'Full', N, N, U, LDU, V, LDV )
C
      IF( NDIM.GT.0 .AND. NDIM.LT.N ) THEN
C
C        Reduce A to a block-diagonal form by a similarity
C        transformation of the form
C                 -1                  ( I -X )
C           A <- T  AT,  where    T = (      )  and X satisfies the
C                                     ( 0  I )
C        Sylvester equation
C
C           A11*X - X*A22 = A12.
C
         NR = N - NDIM
         NDIM1 = NDIM + 1
         CALL DTRSYL( 'N', 'N', -1, NDIM, NR, A, LDA, A(NDIM1,NDIM1),
     $                LDA, A(1,NDIM1), LDA, SCALE, INFO )
         IF ( INFO.NE.0 )
     $      RETURN
C                      -1                                -1
C        Compute B <- T  *B,  C <- C*T,  U <- U*T, V <- T  *V.
C
         SCALE = ONE/SCALE
         CALL DGEMM( 'N', 'N', NDIM, M, NR, SCALE, A(1,NDIM1), LDA,
     $               B(NDIM1,1), LDB, ONE, B, LDB )
         CALL DGEMM( 'N', 'N', P, NR, NDIM, -SCALE, C, LDC, A(1,NDIM1),
     $               LDA, ONE, C(1,NDIM1), LDC )
         CALL DGEMM( 'N', 'N', N, NR, NDIM, -SCALE, U, LDU, A(1,NDIM1),
     $               LDA, ONE, U(1,NDIM1), LDU )
         CALL DGEMM( 'N', 'N', NDIM, N, NR, SCALE, A(1,NDIM1), LDA,
     $               V(NDIM1,1), LDV, ONE, V, LDV )
C
C        Set A12 to zero.
C
         CALL DLASET( 'Full', NDIM, NR, ZERO, ZERO, A(1,NDIM1), LDA )
      END IF
C
C     Set to zero the lower triangular part under the first subdiagonal
C     of A.
C
      IF ( N.GT.2 )
     $   CALL DLASET( 'L', N-2, N-2, ZERO, ZERO, A( 3, 1 ), LDA )
      RETURN
C *** Last line of TB01KX ***
      END
