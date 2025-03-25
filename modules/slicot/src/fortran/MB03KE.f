      SUBROUTINE MB03KE( TRANA, TRANB, ISGN, K, M, N, PREC, SMIN, S, A,
     $                   B, C, SCALE, DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To solve small periodic Sylvester-like equations (PSLE)
C
C      op(A(i))*X( i ) + isgn*X(i+1)*op(B(i)) = -scale*C(i), S(i) =  1,
C      op(A(i))*X(i+1) + isgn*X( i )*op(B(i)) = -scale*C(i), S(i) = -1.
C
C     i = 1, ..., K, where op(A) means A or A**T, for the K-periodic
C     matrix sequence X(i) = X(i+K), where A, B and C are K-periodic
C     matrix sequences and A and B are in periodic real Schur form. The
C     matrices A(i) are M-by-M and B(i) are N-by-N, with 1 <= M, N <= 2.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     TRANA   LOGICAL
C             Specifies the form of op(A) to be used, as follows:
C             = .FALSE.:  op(A) = A,
C             = .TRUE. :  op(A) = A**T.
C
C     TRANB   LOGICAL
C             Specifies the form of op(B) to be used, as follows:
C             = .FALSE.:  op(B) = B,
C             = .TRUE. :  op(B) = B**T.
C
C     ISGN    INTEGER
C             Specifies which sign variant of the equations to solve.
C             ISGN = 1 or ISGN = -1.
C
C     Input/Output Parameters
C
C     K       (input) INTEGER
C             The period of the periodic matrix sequences A, B, C and X.
C             K >= 2. (For K = 1, a standard Sylvester equation is
C             obtained.)
C
C     M       (input) INTEGER
C             The order of the matrices A(i) and the number of rows of
C             the matrices C(i) and X(i), i = 1, ..., K.  1 <= M <= 2.
C
C     N       (input) INTEGER
C             The order of the matrices B(i) and the number of columns
C             of the matrices C(i) and X(i), i = 1, ..., K.
C             1 <= N <= 2.
C
C     PREC    (input) DOUBLE PRECISION
C             The relative machine precision. See the LAPACK Library
C             routine DLAMCH.
C
C     SMIN    (input) DOUBLE PRECISION
C             The machine safe minimum divided by PREC.
C
C     S       (input) INTEGER array, dimension (K)
C             The leading K elements of this array must contain the
C             signatures (exponents) of the factors in the K-periodic
C             matrix sequences for A and B. Each entry in S must be
C             either 1 or -1. Notice that it is assumed that the same
C             exponents are tied to both A and B on reduction to the
C             periodic Schur form.
C
C     A       (input) DOUBLE PRECISION array, dimension (M*M*K)
C             On entry, this array must contain the M-by-M matrices
C             A(i), for i = 1, ..., K, stored with the leading dimension
C             M. Matrix A(i) is stored starting at position M*M*(i-1)+1.
C
C     B       (input) DOUBLE PRECISION array, dimension (N*N*K)
C             On entry, this array must contain the N-by-N matrices
C             B(i), for i = 1, ..., K, stored with the leading dimension
C             N. Matrix B(i) is stored starting at position N*N*(i-1)+1.
C
C     C       (input/output) DOUBLE PRECISION array, dimension (M*N*K)
C             On entry, this array must contain the M-by-N matrices
C             C(i), for i = 1, ..., K, stored with the leading dimension
C             M. Matrix C(i) is stored starting at position M*N*(i-1)+1.
C             On exit, the matrices C(i) are overwritten by the solution
C             sequence X(i).
C
C     SCALE   (output) DOUBLE PRECISION
C             The scale factor, scale, set less than or equal to 1 to
C             avoid overflow in X.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal LDWORK.
C             On exit, if INFO = -21, DWORK(1) returns the minimum value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The dimension of the array DWORK.
C             LDWORK >= (4*K-3) * (M*N)**2 + K * M*N.
C
C             If LDWORK = -1  a workspace query is assumed; the
C             routine only calculates the optimal size of the DWORK
C             array, returns this value as the first entry of the DWORK
C             array, and no error message is issued by XERBLA.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -21, then LDWORK is too small; appropriate
C                   value for LDWORK is returned in DWORK(1); the other
C                   arguments are not tested, for efficiency;
C             = 1:  the solution would overflow with scale = 1, so
C                   SCALE was set less than 1. This is a warning, not
C                   an error.
C
C     METHOD
C
C     A version of the algorithm described in [1] is used. The routine
C     uses a sparse Kronecker product representation Z of the PSLE and
C     solves for X(i) from an associated linear system Z*x = c using
C     structured (overlapping) variants of QR factorization and backward
C     substitution.
C
C     REFERENCES
C
C     [1] Granat, R., Kagstrom, B. and Kressner, D.
C         Computing periodic deflating subspaces associated with a
C         specified set of eigenvalues.
C         BIT Numerical Mathematics, vol. 47, 763-791, 2007.
C
C     NUMERICAL ASPECTS
C
C     The implemented method is numerically backward stable.
C
C     CONTRIBUTOR
C
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     Mar. 2010, an essentially new version of the PEP routine
C     PEP_DGESY2, by R. Granat, Umea University, Sweden, Apr. 2008.
C
C     REVISIONS
C
C     V. Sima, Apr. 2010, Oct. 2010, Aug. 2011.
C
C     KEYWORDS
C
C     Orthogonal transformation, periodic QZ algorithm, periodic
C     Sylvester-like equations, QZ algorithm.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TWO = 2.0D+0 )
C     ..
C     .. Scalar Arguments ..
      LOGICAL            TRANA, TRANB
      INTEGER            INFO, ISGN, K, LDWORK, M, N
      DOUBLE PRECISION   PREC, SCALE, SMIN
C     ..
C     .. Array Arguments ..
      INTEGER            S( * )
      DOUBLE PRECISION   A( * ), B( * ), C( * ), DWORK( * )
C     ..
C     .. Local Scalars ..
      LOGICAL            DOSCAL, LQUERY
      INTEGER            CB, I, IA1, IA3, IB1, IB3, IC1, II, IM1, IXA,
     $                   IXB, IXC, IZ, J, KM2, KM3, KMN, L, LDW, LEN,
     $                   MINWRK, MM, MN, MN6, MN7, NN, ZC, ZD, ZI, ZI2,
     $                   ZIS
      DOUBLE PRECISION   AC, AD, BETA, BIGNUM, DMIN, ELEM, SCALOC, SGN,
     $                   SPIV, TAU, TEMP
C     ..
C     .. External Functions ..
      INTEGER            IDAMAX
C     ..
C     .. External Subroutines ..
      EXTERNAL           DAXPY, DSCAL, XERBLA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, MIN, MOD
C     ..
C     .. Executable Statements ..
C
C     Decode the input parameters.
C     For efficiency reasons, the parameters are not checked.
C
      INFO   = 0
      LQUERY = LDWORK.EQ.-1
C
      MN  = M*N
      KMN = K*MN
C
      MINWRK = ( 4*K - 3 ) * MN**2 + KMN
      IF( .NOT. LQUERY .AND. LDWORK.LT.MINWRK )
     $   INFO = -21
C
C     Quick return if possible.
C
      DWORK( 1 ) = DBLE( MINWRK )
      IF( LQUERY ) THEN
         RETURN
      ELSE IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'MB03KE', -INFO )
         RETURN
      END IF
C
C     Find the overflow threshold.
C
      BIGNUM = PREC / SMIN
C
C     --- Use QR-factorizations and backward substitution ---
C
C     This variant does not utilize the sparsity structure of the
C     individual blocks of the matrix Z - storage of each block Z_i,i
C     is compatible with the BLAS. Numerics is stable since excessive
C     pivot growth is avoided.
C
      MM  = M*M
      NN  = N*N
      SGN = DBLE( ISGN )
      LDW = 3*MN
      IF( M.EQ.2 .AND. N.EQ.2 ) THEN
         MN6 = LDW + LDW
         MN7 = MN6 + LDW
         KM2 = KMN + KMN
         KM3 = KM2 + KMN
      END IF
C
C     Divide workspace for superdiagonal + diagonal + subdiagonal blocks
C     and right-most block column stored in a "block-packed" format. For
C     simplicity, an additional block Z_{0,1} appears in the first block
C     column in Z.
C
      ZD = 1
      ZC = ZD + LDW*MN*( K - 1 )
C
C     Also give workspace for right hand side in CB.
C
      CB = ZC + MN*KMN
C
C     Fill the Z part of the workspace with zeros.
C
      DO 10 J = 1, CB - 1
         DWORK( J ) = ZERO
   10 CONTINUE
C
C     Build matrix Z in ZD and ZC.
C
      IXA = 1
      IXB = 1
      IXC = 1
      IM1 = K
      ZI  = ZD + MN
C
      DO 20 I = 1, K - 1
C
C        Build Z_{i,i}, i = 1,...,K-1.
C
         IF( S( IM1 ).EQ.-1 ) THEN
C
            IA1 = ( IM1 - 1 )*MM + 1
            DWORK( ZI ) = A( IA1 )
            IF( M.EQ.2 ) THEN
               IA3 = IA1 + 2
               IF( .NOT. TRANA ) THEN
                  DWORK( ZI + 1   )  = A( IA1 + 1 )
                  DWORK( ZI + LDW )  = A( IA3     )
               ELSE
                  DWORK( ZI + 1   )  = A( IA3     )
                  DWORK( ZI + LDW )  = A( IA1 + 1 )
               END IF
               DWORK( ZI + LDW + 1 ) = A( IA3 + 1 )
            END IF
            IF( N.EQ.2 ) THEN
               ZI2 = ZI + ( LDW + 1 )*M
               DWORK( ZI2 ) = DWORK( ZI )
               IF( M.EQ.2 ) THEN
                  DWORK( ZI2 + 1 )       = DWORK( ZI + 1 )
                  DWORK( ZI2 + LDW     ) = DWORK( ZI + LDW     )
                  DWORK( ZI2 + LDW + 1 ) = DWORK( ZI + LDW + 1 )
               END IF
            END IF
C
         ELSE
C
            IB1 = ( IM1 - 1 )*NN + 1
            DWORK( ZI ) = SGN*B( IB1 )
            IF( .NOT. TRANB ) THEN
               IF( M.EQ.2 ) THEN
                  DWORK( ZI + LDW + 1 ) = DWORK( ZI )
                  IF( N.EQ.2 ) THEN
                     IB3 = IB1 + 2
                     DWORK( ZI + 2 )       = SGN*B( IB3 )
                     DWORK( ZI + LDW + 3 ) = DWORK( ZI  + 2 )
                     DWORK( ZI + MN6     ) = SGN*B( IB1 + 1 )
                     DWORK( ZI + MN6 + 2 ) = SGN*B( IB3 + 1 )
                     DWORK( ZI + MN7 + 1 ) = DWORK( ZI  + MN6 )
                     DWORK( ZI + MN7 + 3 ) = DWORK( ZI  + MN6 + 2 )
                  END IF
               ELSE IF( N.EQ.2 ) THEN
                  IB3 = IB1 + 2
                  DWORK( ZI + 1 )       = SGN*B( IB3 )
                  DWORK( ZI + LDW     ) = SGN*B( IB1 + 1 )
                  DWORK( ZI + LDW + 1 ) = SGN*B( IB3 + 1 )
               END IF
            ELSE
               IF( M.EQ.2 ) THEN
                  DWORK( ZI + LDW + 1 ) = DWORK( ZI )
                  IF( N.EQ.2 ) THEN
                     IB3 = IB1 + 2
                     DWORK( ZI + 2 )       = SGN*B( IB1 + 1 )
                     DWORK( ZI + LDW + 3 ) = DWORK( ZI  + 2 )
                     DWORK( ZI + MN6     ) = SGN*B( IB3     )
                     DWORK( ZI + MN6 + 2 ) = SGN*B( IB3 + 1 )
                     DWORK( ZI + MN7 + 1 ) = DWORK( ZI  + MN6 )
                     DWORK( ZI + MN7 + 3 ) = DWORK( ZI  + MN6 + 2 )
                  END IF
               ELSE IF( N.EQ.2 ) THEN
                  IB3 = IB1 + 2
                  DWORK( ZI + 1 )       = SGN*B( IB1 + 1 )
                  DWORK( ZI + LDW     ) = SGN*B( IB3     )
                  DWORK( ZI + LDW + 1 ) = SGN*B( IB3 + 1 )
               END IF
            END IF
C
         END IF
C
C        Build Z_{i+1,i}, i = 1,...,K-1.
C
         ZI = ZI + MN
         IF( S( I ).EQ.1 ) THEN
C
            IA1 = IXA
            DWORK( ZI ) = A( IA1 )
            IF( M.EQ.2 ) THEN
               IA3 = IA1 + 2
               IF( .NOT. TRANA ) THEN
                  DWORK( ZI + 1   )  = A( IA1 + 1 )
                  DWORK( ZI + LDW )  = A( IA3     )
               ELSE
                  DWORK( ZI + 1   )  = A( IA3     )
                  DWORK( ZI + LDW )  = A( IA1 + 1 )
               END IF
               DWORK( ZI + LDW + 1 ) = A( IA3 + 1 )
            END IF
            IF( N.EQ.2 ) THEN
               ZI2 = ZI + ( LDW + 1 )*M
               DWORK( ZI2 ) = DWORK( ZI )
               IF( M.EQ.2 ) THEN
                  DWORK( ZI2 + 1 )       = DWORK( ZI + 1 )
                  DWORK( ZI2 + LDW     ) = DWORK( ZI + LDW     )
                  DWORK( ZI2 + LDW + 1 ) = DWORK( ZI + LDW + 1 )
               END IF
            END IF
C
         ELSE
C
            IB1 = IXB
            DWORK( ZI ) = SGN*B( IB1 )
            IF( .NOT. TRANB ) THEN
               IF( M.EQ.2 ) THEN
                  DWORK( ZI + LDW + 1 ) = DWORK( ZI )
                  IF( N.EQ.2 ) THEN
                     IB3 = IB1 + 2
                     DWORK( ZI + 2 )       = SGN*B( IB3 )
                     DWORK( ZI + LDW + 3 ) = DWORK( ZI  + 2 )
                     DWORK( ZI + MN6     ) = SGN*B( IB1 + 1 )
                     DWORK( ZI + MN6 + 2 ) = SGN*B( IB3 + 1 )
                     DWORK( ZI + MN7 + 1 ) = DWORK( ZI  + MN6     )
                     DWORK( ZI + MN7 + 3 ) = DWORK( ZI  + MN6 + 2 )
                  END IF
               ELSE IF( N.EQ.2 ) THEN
                  IB3 = IB1 + 2
                  DWORK( ZI + 1 )       = SGN*B( IB3 )
                  DWORK( ZI + LDW     ) = SGN*B( IB1 + 1 )
                  DWORK( ZI + LDW + 1 ) = SGN*B( IB3 + 1 )
               END IF
            ELSE
               IF( M.EQ.2 ) THEN
                  DWORK( ZI + LDW + 1 ) = DWORK( ZI )
                  IF( N.EQ.2 ) THEN
                     IB3 = IB1 + 2
                     DWORK( ZI + 2 )       = SGN*B( IB1 + 1 )
                     DWORK( ZI + LDW + 3 ) = DWORK( ZI  + 2 )
                     DWORK( ZI + MN6     ) = SGN*B( IB3     )
                     DWORK( ZI + MN6 + 2 ) = SGN*B( IB3 + 1 )
                     DWORK( ZI + MN7 + 1 ) = DWORK( ZI  + MN6     )
                     DWORK( ZI + MN7 + 3 ) = DWORK( ZI  + MN6 + 2 )
                  END IF
               ELSE IF( N.EQ.2 ) THEN
                  IB3 = IB1 + 2
                  DWORK( ZI + 1 )       = SGN*B( IB1 + 1 )
                  DWORK( ZI + LDW     ) = SGN*B( IB3     )
                  DWORK( ZI + LDW + 1 ) = SGN*B( IB3 + 1 )
               END IF
            END IF
         END IF
C
         IXA = IXA + MM
         IXB = IXB + NN
         IM1 = I
         ZI  = ZI + MN*( LDW - 1 )
   20 CONTINUE
C
C     Build Z_{K,K}.
C
      IXA = IXA - MM
      IXB = IXB - NN
      ZI  = ZC + KMN - MN
      IF( S( K - 1 ).EQ.-1 ) THEN
C
         IA1 = IXA
         DWORK( ZI ) = A( IA1 )
         IF( M.EQ.2 ) THEN
            IA3 = IA1 + 2
            IF( .NOT. TRANA ) THEN
               DWORK( ZI + 1   )  = A( IA1 + 1 )
               DWORK( ZI + KMN )  = A( IA3     )
            ELSE
               DWORK( ZI + 1   )  = A( IA3     )
               DWORK( ZI + KMN )  = A( IA1 + 1 )
            END IF
            DWORK( ZI + KMN + 1 ) = A( IA3 + 1 )
         END IF
         IF( N.EQ.2 ) THEN
            ZI2 = ZI + ( KMN + 1 )*M
            DWORK( ZI2 ) = DWORK( ZI )
            IF( M.EQ.2 ) THEN
               DWORK( ZI2 + 1 )       = DWORK( ZI + 1 )
               DWORK( ZI2 + KMN     ) = DWORK( ZI + KMN     )
               DWORK( ZI2 + KMN + 1 ) = DWORK( ZI + KMN + 1 )
            END IF
         END IF
C
      ELSE
C
         IB1 = IXB
         DWORK( ZI ) = SGN*B( IB1 )
         IF( .NOT. TRANB ) THEN
            IF( M.EQ.2 ) THEN
               DWORK( ZI + KMN + 1 ) = DWORK( ZI )
               IF( N.EQ.2 ) THEN
                  IB3 = IB1 + 2
                  DWORK( ZI + 2 )       = SGN*B( IB3 )
                  DWORK( ZI + KMN + 3 ) = DWORK( ZI  + 2 )
                  DWORK( ZI + KM2     ) = SGN*B( IB1 + 1 )
                  DWORK( ZI + KM2 + 2 ) = SGN*B( IB3 + 1 )
                  DWORK( ZI + KM3 + 1 ) = DWORK( ZI  + KM2     )
                  DWORK( ZI + KM3 + 3 ) = DWORK( ZI  + KM2 + 2 )
               END IF
            ELSE IF( N.EQ.2 ) THEN
               IB3 = IB1 + 2
               DWORK( ZI + 1 )       = SGN*B( IB3 )
               DWORK( ZI + KMN     ) = SGN*B( IB1 + 1 )
               DWORK( ZI + KMN + 1 ) = SGN*B( IB3 + 1 )
            END IF
         ELSE
            IF( M.EQ.2 ) THEN
               DWORK( ZI + KMN + 1 ) = DWORK( ZI )
               IF( N.EQ.2 ) THEN
                  IB3 = IB1 + 2
                  DWORK( ZI + 2 )       = SGN*B( IB1 + 1 )
                  DWORK( ZI + KMN + 3 ) = DWORK( ZI  + 2 )
                  DWORK( ZI + KM2     ) = SGN*B( IB3     )
                  DWORK( ZI + KM2 + 2 ) = SGN*B( IB3 + 1 )
                  DWORK( ZI + KM3 + 1 ) = DWORK( ZI  + KM2     )
                  DWORK( ZI + KM3 + 3 ) = DWORK( ZI  + KM2 + 2 )
               END IF
            ELSE IF( N.EQ.2 ) THEN
               IB3 = IB1 + 2
               DWORK( ZI + 1 )       = SGN*B( IB1 + 1 )
               DWORK( ZI + KMN     ) = SGN*B( IB3     )
               DWORK( ZI + KMN + 1 ) = SGN*B( IB3 + 1 )
            END IF
         END IF
      END IF
C
C     Build Z_{1,K}.
C
      IF( S( K ).EQ.1 ) THEN
C
         IA1 = IA1 + MM
         DWORK( ZC ) = A( IA1 )
         IF( M.EQ.2 ) THEN
            IA3 = IA1 + 2
            IF( .NOT. TRANA ) THEN
               DWORK( ZC + 1   )  = A( IA1 + 1 )
               DWORK( ZC + KMN )  = A( IA3     )
            ELSE
               DWORK( ZC + 1   )  = A( IA3     )
               DWORK( ZC + KMN )  = A( IA1 + 1 )
            END IF
            DWORK( ZC + KMN + 1 ) = A( IA3 + 1 )
         END IF
         IF( N.EQ.2 ) THEN
            ZI2 = ZC + ( KMN + 1 )*M
            DWORK( ZI2 ) = DWORK( ZC )
            IF( M.EQ.2 ) THEN
               DWORK( ZI2 + 1 )       = DWORK( ZC + 1 )
               DWORK( ZI2 + KMN     ) = DWORK( ZC + KMN     )
               DWORK( ZI2 + KMN + 1 ) = DWORK( ZC + KMN + 1 )
            END IF
         END IF
C
      ELSE
C
         IB1 = IB1 + NN
         DWORK( ZC ) = SGN*B( IB1 )
         IF( .NOT. TRANB ) THEN
            IF( M.EQ.2 ) THEN
               DWORK( ZC + KMN + 1 ) = DWORK( ZC )
               IF( N.EQ.2 ) THEN
                  IB3 = IB1 + 2
                  DWORK( ZC + 2 )       = SGN*B( IB3 )
                  DWORK( ZC + KMN + 3 ) = DWORK( ZC  + 2 )
                  DWORK( ZC + KM2     ) = SGN*B( IB1 + 1 )
                  DWORK( ZC + KM2 + 2 ) = SGN*B( IB3 + 1 )
                  DWORK( ZC + KM3 + 1 ) = DWORK( ZC  + KM2     )
                  DWORK( ZC + KM3 + 3 ) = DWORK( ZC  + KM2 + 2 )
               END IF
            ELSE IF( N.EQ.2 ) THEN
               IB3 = IB1 + 2
               DWORK( ZC + 1 )       = SGN*B( IB3 )
               DWORK( ZC + KMN     ) = SGN*B( IB1 + 1 )
               DWORK( ZC + KMN + 1 ) = SGN*B( IB3 + 1 )
            END IF
         ELSE
            IF( M.EQ.2 ) THEN
               DWORK( ZC + KMN + 1 ) = DWORK( ZC )
               IF( N.EQ.2 ) THEN
                  IB3 = IB1 + 2
                  DWORK( ZC + 2 )       = SGN*B( IB1 + 1 )
                  DWORK( ZC + KMN + 3 ) = DWORK( ZC  + 2 )
                  DWORK( ZC + KM2     ) = SGN*B( IB3     )
                  DWORK( ZC + KM2 + 2 ) = SGN*B( IB3 + 1 )
                  DWORK( ZC + KM3 + 1 ) = DWORK( ZC  + KM2     )
                  DWORK( ZC + KM3 + 3 ) = DWORK( ZC  + KM2 + 2 )
               END IF
            ELSE IF( N.EQ.2 ) THEN
               IB3 = IB1 + 2
               DWORK( ZC + 1 )       = SGN*B( IB1 + 1 )
               DWORK( ZC + KMN     ) = SGN*B( IB3     )
               DWORK( ZC + KMN + 1 ) = SGN*B( IB3 + 1 )
            END IF
         END IF
      END IF
C
C     Prepare right hand side in CB.
C
      ZI = CB + MN
      DO 30 L = 1, K - 1
         IC1 = IXC
         DWORK( ZI ) = -C( IC1 )
         IF( M.EQ.1 ) THEN
            IF( N.EQ.2 )
     $         DWORK( ZI + 1 ) = -C( IC1 + 1 )
         ELSE
            DWORK( ZI + 1 )    = -C( IC1 + 1 )
            IF( N.EQ.2 ) THEN
               DWORK( ZI + 2 ) = -C( IC1 + 2 )
               DWORK( ZI + 3 ) = -C( IC1 + 3 )
            END IF
         END IF
         IXC = IXC + MN
         ZI  = ZI  + MN
   30 CONTINUE
C
      ZI  = CB
      IC1 = IXC
      DWORK( ZI ) = -C( IC1 )
      IF( M.EQ.1 ) THEN
         IF( N.EQ.2 )
     $      DWORK( ZI + 1 ) = -C( IC1 + 1 )
      ELSE
         DWORK( ZI + 1 )    = -C( IC1 + 1 )
         IF( N.EQ.2 ) THEN
            DWORK( ZI + 2 ) = -C( IC1 + 2 )
            DWORK( ZI + 3 ) = -C( IC1 + 3 )
         END IF
      END IF
C
C     Solve the Kronecker product system for X_i, i = 1,...,K
C     using overlapping (structured) QR-factorization and
C     backward substitution.
C
C     Step 1: Reduce the system to triangular form via overlapping
C             QR-factorizations.
C
C             The method here is based on successively formed
C             Householder reflections which are applied one by one
C             to the matrix Z and the right hand side c. The size
C             of each reflection is chosen as the number of elements
C             in each column from the last non-zero element up to
C             the diagonal.
C
C             Notation:
C             L   = current position of the column to work with;
C             I   = corresponding block column in Z;
C             II  = corresponding row and column position in Z-block;
C             LEN = length of the current Householder reflection.
C
      I   = 1
      II  = 0
      ZIS = ZD + MN
      ZI2 = ZD + MN*LDW
C
C     Treat Z_{K,K} separately from [Z_{i,i}',Z_{i+1,i}']' (see below).
C     DMIN is the minimum modulus of the final diagonal values.
C
      DMIN = BIGNUM
C
      DO 50 L = 1, KMN - MN
         II  = II   + 1
         ZI  = ZIS  + 2*MN
         LEN = 2*MN - II + 1
C
C        REPEAT
   40    CONTINUE
         ZI = ZI - 1
         ELEM = DWORK( ZI )
         IF( ELEM.EQ.ZERO ) THEN
            LEN = LEN - 1
            GO TO 40
         END IF
C        UNTIL ELEM.NE.ZERO.
C
         IF( LEN.GT.1 ) THEN
C
C           Generate Householder reflection to zero out the current
C           column. The new main diagonal value is stored temporarily
C           in BETA.
C
            ZI = ZI - LEN + 1
            CALL DLARFG( LEN, DWORK( ZI ), DWORK( ZI + 1 ), 1, TAU )
            BETA = DWORK( ZI )
            DWORK( ZI ) = ONE
C
C           Apply reflection to Z and c: first to the rest of the
C           corresponding rows and columns of [Z_{i,i}',Z_{i+1,i}']'
C           of size LEN-by-(MN-II) ...
C
            CALL DLARFX( 'Left', LEN, MN - II, DWORK( ZI ), TAU,
     $                   DWORK( ZI + LDW ), LDW, DWORK )
C
C           ... then to the corresponding part of
C           [Z_{i,i+1}',Z_{i+1,i+1}']' of size LEN-by-MN ...
C
            IF( I.LT.K - 1 )
     $         CALL DLARFX( 'Left', LEN, MN, DWORK( ZI ), TAU,
     $                      DWORK( ZI2 ), LDW, DWORK )
C
C           ... next to the corresponding part of
C           [Z_{i,K}',Z_{i+1,K}']' of size LEN-by-MN ...
C
            CALL DLARFX( 'Left', LEN, MN, DWORK( ZI ), TAU,
     $                   DWORK( ZC + L - 1 ), KMN, DWORK )
C
C           ... and finally to c(L:L+LEN-1).
C
            CALL DLARFX( 'Left', LEN, 1, DWORK( ZI ), TAU,
     $                   DWORK( CB + L - 1 ), KMN, DWORK )
C
C           Store the new diagonal value.
C
            DWORK( ZI ) = BETA
            DMIN = MIN( DMIN, ABS( BETA ) )
         END IF
C
         ZIS = ZIS + LDW
         ZI2 = ZI2 + 1
         IF( MOD( L, MN ).EQ.0 ) THEN
            I   =  I + 1
            II  =  0
            ZI2 = ZD + I*MN*LDW
         END IF
   50 CONTINUE
C
      II = 0
      ZI = ZC + KMN - MN
C
C     Z_{K,K} is treated separately.
C
      DO 60 L = KMN - MN + 1, KMN
         II  = II + 1
         LEN = MN - II + 1
         IF( LEN.GT.1 ) THEN
C
C           Generate Householder reflection.
C
            CALL DLARFG( LEN, DWORK( ZI ), DWORK( ZI + 1 ), 1, TAU )
            BETA = DWORK( ZI )
            DWORK( ZI ) = ONE
C
C           Apply reflection to Z and c: first to Z_{i,i} ...
C
            CALL DLARFX( 'Left', LEN, MN - II, DWORK( ZI ), TAU,
     $                   DWORK( ZI + KMN ), KMN, DWORK )
C
C           ... and finally to c(L:L+LEN-1).
C
            CALL DLARFX( 'Left', LEN, 1, DWORK( ZI ), TAU,
     $                   DWORK( CB + L - 1 ), KMN, DWORK )
C
C           Store the new diagonal value.
C
            DWORK( ZI ) = BETA
            DMIN = MIN( DMIN, ABS( BETA ) )
         END IF
         ZI = ZI + KMN + 1
C
   60 CONTINUE
C
C     Step 2: Use backward substitution on the computed triangular
C             system.
C
C             Here, we take the possible irregularities above the
C             diagonal of the resulting R-factor into account by
C             checking the number of elements from the main diagonal
C             to the last non-zero element above the diagonal that
C             resides in the current column.
C             Pivots less than SPIV = MAX( PREC*DMIN, SMIN ) are set
C             to SPIV.
C
      SCALE  = ONE
      DOSCAL = .FALSE.
      DMIN   = MAX( DMIN, SMIN )
      SPIV   = MAX( PREC*DMIN, SMIN )
C
C     Check for scaling.
C
      I  = IDAMAX( KMN, DWORK( CB ), 1 )
      AC = ABS( DWORK( CB + I - 1 ) )
      IF( TWO*SMIN*AC.GT.DMIN ) THEN
         TEMP = ( ONE / TWO ) / AC
         CALL DSCAL( KMN, TEMP, DWORK( CB ), 1 )
         SCALE = SCALE*TEMP
      END IF
C
      ZI = CB - 1
C
      DO 70 I = KMN, KMN - MN + 1, -1
C
         AD = ABS( DWORK( ZI ) )
         AC = ABS( DWORK( CB + I - 1 ) )
         IF( AD.LT.SPIV ) THEN
            AD = SPIV
            DWORK( ZI ) = SPIV
         END IF
         SCALOC = ONE
         IF( AD.LT.ONE .AND. AC.GT.ONE ) THEN
            IF( AC.GT.BIGNUM*AD ) THEN
               INFO   = 1
               SCALOC = BIGNUM*AD / AC
               DOSCAL = .TRUE.
               SCALE  = SCALE * SCALOC
            END IF
         END IF
         TEMP = ( DWORK( CB + I - 1 ) * SCALOC ) / DWORK( ZI )
         IF( DOSCAL ) THEN
            DOSCAL = .FALSE.
            CALL DSCAL( KMN, SCALOC, DWORK( CB ), 1 )
         END IF
         DWORK( CB + I - 1 ) = TEMP
C
         CALL DAXPY( I - 1, -TEMP, DWORK( ZI - I + 1 ), 1, DWORK( CB ),
     $               1 )
C
         ZI = ZI - KMN - 1
   70 CONTINUE
C
      ZIS = ZC - LDW
      ZI  = ZIS + 2*MN - 1
      IZ  = 0
C
      DO 90 I = KMN - MN, 1, -1
         AD = ABS( DWORK( ZI ) )
         AC = ABS( DWORK( CB + I - 1 ) )
         IF( AD.LT.SPIV ) THEN
            AD = SPIV
            DWORK( ZI ) = SPIV
         END IF
         SCALOC = ONE
         IF( AD.LT.ONE .AND. AC.GT.ONE ) THEN
            IF( AC.GT.BIGNUM*AD ) THEN
               INFO   = 1
               SCALOC = BIGNUM*AD / AC
               DOSCAL = .TRUE.
               SCALE  = SCALE * SCALOC
            END IF
         END IF
         TEMP = ( DWORK( CB + I - 1 ) * SCALOC ) / DWORK( ZI )
         IF( DOSCAL ) THEN
            DOSCAL = .FALSE.
            CALL DSCAL( KMN, SCALOC, DWORK( CB ), 1 )
         END IF
         DWORK( CB + I - 1 ) = TEMP
         LEN = MN + MOD( I - 1, MN ) + 1
         ZI2 = ZIS
   80    CONTINUE
         IF( DWORK( ZI2 ).EQ.ZERO ) THEN
            LEN = LEN - 1
            ZI2 = ZI2 + 1
            GO TO 80
         END IF
C
         J = MAX( 1, I - LEN + 1 )
         CALL DAXPY( I - J, -TEMP, DWORK( ZI - I + J ), 1,
     $               DWORK( CB + J - 1 ), 1 )
C
         IF( MN.GT.1 ) THEN
            IF( MOD( I, MN ).EQ.1 ) THEN
               IZ = 1 - MN
            ELSE
               IZ = 1
            END IF
         END IF
         ZI  = ZI  - LDW - IZ
         ZIS = ZIS - LDW
   90 CONTINUE
C
C     Reshape the solution into C.
C
      IC1 = 1
      ZI  = CB
C
      DO 100 L = 1, K
         C( IC1 ) = DWORK( ZI )
         IF( M.EQ.1 ) THEN
            IF( N.EQ.2 )
     $         C( IC1 + 1 ) = DWORK( ZI + 1 )
         ELSE
            C( IC1 + 1 ) = DWORK( ZI + 1 )
            IF( N.EQ.2 ) THEN
               C( IC1 + 2 ) = DWORK( ZI + 2 )
               C( IC1 + 3 ) = DWORK( ZI + 3 )
            END IF
         END IF
         IC1 = IC1 + MN
         ZI  = ZI  + MN
  100 CONTINUE
C
C     Store the minimal workspace on output.
C
      DWORK( 1 ) = DBLE( MINWRK )
      RETURN
C
C *** Last line of MB03KE ***
      END
