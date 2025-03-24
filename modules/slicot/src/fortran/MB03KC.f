      SUBROUTINE MB03KC( K, KHESS, N, R, S, A, LDA, V, TAU )
C
C     PURPOSE
C
C     To reduce a 2-by-2 general, formal matrix product A of length K,
C
C        A_K^s(K) * A_K-1^s(K-1) * ... * A_1^s(1),
C
C     to the periodic Hessenberg-triangular form using a K-periodic
C     sequence of elementary reflectors (Householder matrices). The
C     matrices A_k, k = 1, ..., K, are stored in the N-by-N-by-K array A
C     starting in the R-th row and column, and N can be 3 or 4.
C
C     Each elementary reflector H_k is represented as
C
C        H_k = I - tau_k * v_k * v_k',                               (1)
C
C     where I is the 2-by-2 identity, tau_k is a real scalar, and v_k is
C     a vector of length 2, k = 1,...,K, and it is constructed such that
C     the following holds for k = 1,...,K:
C
C            H_{k+1} * A_k * H_k = T_k, if s(k) = 1,
C                                                                    (2)
C            H_k * A_k * H_{k+1} = T_k, if s(k) = -1,
C
C     with H_{K+1} = H_1 and all T_k upper triangular except for
C     T_{khess} which is full. Clearly,
C
C        T_K^s(K) *...* T_1^s(1) = H_1 * A_K^s(K) *...* A_1^s(1) * H_1.
C
C     The reflectors are suitably applied to the whole, extended N-by-N
C     matrices Ae_k, not only to the submatrices A_k, k = 1, ..., K.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     K       (input) INTEGER
C             The number of matrices in the sequence A_k.  K >= 2.
C
C     KHESS   (input) INTEGER
C             The index for which the returned matrix A_khess should be
C             in the Hessenberg form on output.  1 <= KHESS <= K.
C
C     N       (input) INTEGER
C             The order of the extended matrices.  N = 3 or N = 4.
C
C     R       (input) INTEGER
C             The starting row and column index for the
C             2-by-2 submatrices.  R = 1, or R = N-1.
C
C     S       (input) INTEGER array, dimension (K)
C             The leading K elements of this array must contain the
C             signatures of the factors. Each entry in S must be either
C             1 or -1; the value S(k) = -1 corresponds to using the
C             inverse of the factor A_k.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (*)
C             On entry, this array must contain at position IXA(k) =
C             (k-1)*N*LDA+1 the N-by-N matrix Ae_k stored with leading
C             dimension LDA.
C             On exit, this array contains at position IXA(k) the
C             N-by-N matrix Te_k stored with leading dimension LDA.
C
C     LDA     INTEGER
C             Leading dimension of the matrices Ae_k and Te_k in the
C             one-dimensional array A.  LDA >= N.
C
C     V       (output) DOUBLE PRECISION array, dimension (2*K)
C             On exit, this array contains the K vectors v_k,
C             k = 1,...,K, defining the elementary reflectors H_k as
C             in (1). The k-th reflector is stored in V(2*k-1:2*k).
C
C     TAU     (output) DOUBLE PRECISION array, dimension (K)
C             On exit, this array contains the K values of tau_k,
C             k = 1,...,K, defining the elementary reflectors H_k
C             as in (1).
C
C     METHOD
C
C     A K-periodic sequence of elementary reflectors (Householder
C     matrices) is used. The computations start for k = khess with the
C     left reflector in (1), which is the identity matrix.
C
C     NUMERICAL ASPECTS
C
C     The implemented method is numerically backward stable.
C
C     CONTRIBUTOR
C
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     Mar. 2010, an essentially new version of the PEP routine
C     PEP_DGEHR2, by R. Granat, Umea University, Sweden, Apr. 2008.
C
C     REVISIONS
C
C     V. Sima, Apr. 2010, May 2010.
C
C     KEYWORDS
C
C     Orthogonal transformation, periodic QZ algorithm, QZ algorithm.
C
C     ******************************************************************
C
C  .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
C     ..
C     .. Scalar Arguments ..
      INTEGER            K, KHESS, LDA, N, R
C     ..
C     .. Array Arguments ..
      INTEGER            S( * )
      DOUBLE PRECISION   A( * ), TAU( * ), V( * )
C  ..
C  .. Local Scalars ..
      INTEGER            I, I1, I2, IC, INC, IP1, IR, IX, NO
C  ..
C  .. Local Arrays ..
      DOUBLE PRECISION   TMP( 1 ), WORK( 2 )
C  ..
C  .. External Subroutines ..
      EXTERNAL           DLARFG, DLARFX
C  ..
C     .. Intrinsic Functions ..
      INTRINSIC          MOD
C  ..
C  .. Executable Statements ..
C
C     For efficiency reasons, the parameters are not checked.
C
C     Compute the periodic Hessenberg form of A with the Hessenberg
C     matrix at position KHESS - start construction from I = KHESS,
C     i.e., to the left of (and including) the Hessenberg matrix in the
C     corresponding matrix product.
C
C     Since the problem is 2-by-2, the orthogonal matrix working on
C     A_{khess} from the left, if s(khess) = 1, or from the right,
C     if s(khess) = -1, hence H_{khess+1}, will be the identity.
C
      IR  = ( R - 1 )*LDA
      IC  = IR + R - 1
      NO  = N - R
      INC = N*LDA
      I1  = KHESS*INC + 1
      IP1 = MOD( KHESS, K ) + 1
C
      TAU( IP1   ) = ZERO
      V( 2*IP1-1 ) = ZERO
      V( 2*IP1   ) = ZERO
C
      DO 10 I = KHESS + 1, K
         IP1 = MOD( I, K )
         IX  = I1 + IC
         I2  = IP1*INC + 1
         IP1 = IP1 + 1
C
C        Compute and apply the reflector H_{i+1} working on A_i^s(i)
C        from the left.
C
         IF( S( I ).EQ.1 ) THEN
            WORK( 1 ) = ONE
            WORK( 2 ) = A( IX+1 )
            CALL DLARFG( 2, A( IX ), WORK( 2 ), 1, TAU( IP1 ) )
            V( 2*IP1-1 ) = ONE
            V( 2*IP1   ) = WORK( 2 )
            CALL DLARFX( 'Left', 2, NO, WORK, TAU( IP1 ), A( IX+LDA ),
     $                   LDA, TMP )
         ELSE
            WORK( 1 ) = A( IX+1 )
            WORK( 2 ) = ONE
            CALL DLARFG( 2, A( IX+LDA+1 ), WORK, 1, TAU( IP1 ) )
            V( 2*IP1-1 ) = WORK( 1 )
            V( 2*IP1   ) = ONE
            CALL DLARFX( 'Right', R, 2, WORK, TAU( IP1 ), A( I1+IR ),
     $                   LDA, TMP )
         END IF
         A( IX+1 ) = ZERO
C
C        Apply the reflector to A_{mod(i,K)+1}.
C
         IF( S( IP1 ).EQ.1 ) THEN
            CALL DLARFX( 'Right', R+1, 2, WORK, TAU( IP1 ), A( I2+IR ),
     $                   LDA, TMP )
         ELSE
            CALL DLARFX( 'Left', 2, NO+1, WORK, TAU( IP1 ), A( I2+IC ),
     $                   LDA, TMP )
         END IF
         I1 = I1 + INC
   10 CONTINUE
C
C     Continue to the right of the Hessenberg matrix.
C
      I1 = 1
C
      DO 20 I = 1, KHESS - 1
         IP1 = MOD( I, K )
         IX  = I1 + IC
         I2  = IP1*INC + 1
         IP1 = IP1 + 1
C
C        Compute and apply the reflector H_{i+1} working on A_i^s(i)
C        from the left.
C
         IF( S( I ).EQ.1 ) THEN
            WORK( 1 ) = ONE
            WORK( 2 ) = A( IX+1 )
            CALL DLARFG( 2, A( IX ), WORK( 2 ), 1, TAU( IP1 ) )
            V( 2*IP1-1 ) = ONE
            V( 2*IP1   ) = WORK( 2 )
            CALL DLARFX( 'Left', 2, NO, WORK, TAU( IP1 ), A( IX+LDA ),
     $                   LDA, TMP )
         ELSE
            WORK( 1 ) = A( IX+1 )
            WORK( 2 ) = ONE
            CALL DLARFG( 2, A( IX+LDA+1 ), WORK, 1, TAU( IP1 ) )
            V( 2*IP1-1 ) = WORK( 1 )
            V( 2*IP1   ) = ONE
            CALL DLARFX( 'Right', R, 2, WORK, TAU( IP1 ), A( I1+IR ),
     $                   LDA, TMP )
         END IF
         A( IX+1 ) = ZERO
C
C        Apply the reflector to A_{mod(i,K)+1}.
C
         IF( S( IP1 ).EQ.1 ) THEN
            CALL DLARFX( 'Right', R+1, 2, WORK, TAU( IP1 ), A( I2+IR ),
     $                   LDA, TMP )
         ELSE
            CALL DLARFX( 'Left', 2, NO+1, WORK, TAU( IP1 ), A( I2+IC ),
     $                   LDA, TMP )
         END IF
         I1 = I1 + INC
   20 CONTINUE
C
C     The periodic Hessenberg-triangular form has been computed.
C
      RETURN
C
C *** Last line of MB03KC ***
      END
