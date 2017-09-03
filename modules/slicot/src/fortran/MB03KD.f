      SUBROUTINE MB03KD( COMPQ, WHICHQ, STRONG, K, NC, KSCHUR, N, NI, S,
     $                   SELECT, T, LDT, IXT, Q, LDQ, IXQ, M, TOL,
     $                   IWORK, DWORK, LDWORK, INFO )
C
C     SLICOT RELEASE 5.0.
C
C     Copyright (c) 2002-2010 NICONET e.V.
C
C     This program is free software: you can redistribute it and/or
C     modify it under the terms of the GNU General Public License as
C     published by the Free Software Foundation, either version 2 of
C     the License, or (at your option) any later version.
C
C     This program is distributed in the hope that it will be useful,
C     but WITHOUT ANY WARRANTY; without even the implied warranty of
C     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C     GNU General Public License for more details.
C
C     You should have received a copy of the GNU General Public License
C     along with this program.  If not, see
C     <http://www.gnu.org/licenses/>.
C
C     PURPOSE
C
C     To reorder the diagonal blocks of the formal matrix product
C
C        T22_K^S(K) * T22_K-1^S(K-1) * ... * T22_1^S(1),             (1)
C
C     of length K, in the generalized periodic Schur form,
C
C              [  T11_k  T12_k  T13_k  ]
C        T_k = [    0    T22_k  T23_k  ],    k = 1, ..., K,          (2)
C              [    0      0    T33_k  ]
C
C     where
C
C     - the submatrices T11_k are NI(k+1)-by-NI(k), if S(k) = 1, or
C       NI(k)-by-NI(k+1), if S(k) = -1, and contain dimension-induced
C       infinite eigenvalues,
C     - the submatrices T22_k are NC-by-NC and contain core eigenvalues,
C       which are generically neither zero nor infinite,
C     - the submatrices T33_k contain dimension-induced zero
C       eigenvalues,
C
C     such that the M selected eigenvalues pointed to by the logical
C     vector SELECT end up in the leading part of the matrix sequence
C     T22_k.
C
C     Given that N(k) = N(k+1) for all k where S(k) = -1, the T11_k are
C     void and the first M columns of the updated orthogonal
C     transformation matrix sequence Q_1, ..., Q_K span a periodic
C     deflating subspace corresponding to the same eigenvalues.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     COMPQ   CHARACTER*1
C             Specifies whether to compute the orthogonal transformation
C             matrices Q_k, as follows:
C             = 'N': do not compute any of the matrices Q_k;
C             = 'I': each coefficient of Q is initialized internally to
C                    the identity matrix, and the orthogonal matrices
C                    Q_k are returned, where Q_k, k = 1, ..., K,
C                    performed the reordering;
C             = 'U': each coefficient of Q must contain an orthogonal
C                    matrix Q1_k on entry, and the products Q1_k*Q_k are
C                    returned;
C             = 'W': the computation of each Q_k is specified
C                    individually in the array WHICHQ.
C
C     WHICHQ  INTEGER array, dimension (K)
C             If COMPQ = 'W', WHICHQ(k) specifies the computation of Q_k
C             as follows:
C             = 0:   do not compute Q_k;
C             = 1:   the kth coefficient of Q is initialized to the
C                    identity matrix, and the orthogonal matrix Q_k is
C                    returned;
C             = 2:   the kth coefficient of Q must contain an orthogonal
C                    matrix Q1_k on entry, and the product Q1_k*Q_k is
C                    returned.
C             This array is not referenced if COMPQ <> 'W'.
C
C     STRONG  CHARACTER*1
C             Specifies whether to perform the strong stability tests,
C             as follows:
C             = 'N': do not perform the strong stability tests;
C             = 'S': perform the strong stability tests; often, this is
C                    not needed, and omitting them can save some
C                    computations.
C
C     Input/Output Parameters
C
C     K       (input) INTEGER
C             The period of the periodic matrix sequences T and Q (the
C             number of factors in the matrix product).  K >= 2.
C             (For K = 1, a standard eigenvalue reordering problem is
C             obtained.)
C
C     NC      (input) INTEGER
C             The number of core eigenvalues.  0 <= NC <= min(N).
C
C     KSCHUR  (input) INTEGER
C             The index for which the matrix T22_kschur is upper quasi-
C             triangular. All other T22 matrices are upper triangular.
C
C     N       (input) INTEGER array, dimension (K)
C             The leading K elements of this array must contain the
C             dimensions of the factors of the formal matrix product T,
C             such that the k-th coefficient T_k is an N(k+1)-by-N(k)
C             matrix, if S(k) = 1, or an N(k)-by-N(k+1) matrix,
C             if S(k) = -1, k = 1, ..., K, where N(K+1) = N(1).
C
C     NI      (input) INTEGER array, dimension (K)
C             The leading K elements of this array must contain the
C             dimensions of the factors of the matrix sequence T11_k.
C             N(k) >= NI(k) + NC >= 0.
C
C     S       (input) INTEGER array, dimension (K)
C             The leading K elements of this array must contain the
C             signatures (exponents) of the factors in the K-periodic
C             matrix sequence. Each entry in S must be either 1 or -1;
C             the value S(k) = -1 corresponds to using the inverse of
C             the factor T_k.
C
C     SELECT  (input) LOGICAL array, dimension (NC)
C             SELECT specifies the eigenvalues in the selected cluster.
C             To select a real eigenvalue w(j), SELECT(j) must be set to
C             .TRUE.. To select a complex conjugate pair of eigenvalues
C             w(j) and w(j+1), corresponding to a 2-by-2 diagonal block,
C             either SELECT(j) or SELECT(j+1) or both must be set to
C             .TRUE.; a complex conjugate pair of eigenvalues must be
C             either both included in the cluster or both excluded.
C
C     T       (input/output) DOUBLE PRECISION array, dimension (*)
C             On entry, this array must contain at position IXT(k) the
C             matrix T_k, which is at least N(k+1)-by-N(k), if S(k) = 1,
C             or at least N(k)-by-N(k+1), if S(k) = -1, in periodic
C             Schur form.
C             On exit, the matrices T_k are overwritten by the reordered
C             periodic Schur form.
C
C     LDT     INTEGER array, dimension (K)
C             The leading dimensions of the matrices T_k in the one-
C             dimensional array T.
C             LDT(k) >= max(1,N(k+1)),  if S(k) =  1,
C             LDT(k) >= max(1,N(k)),    if S(k) = -1.
C
C     IXT     INTEGER array, dimension (K)
C             Start indices of the matrices T_k in the one-dimensional
C             array T.
C
C     Q       (input/output) DOUBLE PRECISION array, dimension (*)
C             On entry, this array must contain at position IXQ(k) a
C             matrix Q_k of size at least N(k)-by-N(k), provided that
C             COMPQ = 'U', or COMPQ = 'W' and WHICHQ(k) = 2.
C             On exit, if COMPQ = 'I' or COMPQ = 'W' and WHICHQ(k) = 1,
C             Q_k contains the orthogonal matrix that performed the
C             reordering. If COMPQ = 'U', or COMPQ = 'W' and
C             WHICHQ(k) = 2, Q_k is post-multiplied with the orthogonal
C             matrix that performed the reordering.
C             This array is not referenced if COMPQ = 'N'.
C
C     LDQ     INTEGER array, dimension (K)
C             The leading dimensions of the matrices Q_k in the one-
C             dimensional array Q.
C             LDQ(k) >= max(1,N(k)), if COMPQ = 'I', or COMPQ = 'U', or
C                                       COMPQ = 'W' and WHICHQ(k) > 0;
C             This array is not referenced if COMPQ = 'N'.
C
C     IXQ     INTEGER array, dimension (K)
C             Start indices of the matrices Q_k in the one-dimensional
C             array Q.
C             This array is not referenced if COMPQ = 'N'.
C
C     M       (output) INTEGER
C             The number of selected core eigenvalues which were
C             reordered to the top of T22_k.
C
C     Tolerances
C
C     TOL     DOUBLE PRECISION
C             The tolerance parameter c. The weak and strong stability
C             tests performed for checking the reordering use a
C             threshold computed by the formula  MAX(c*EPS*NRM, SMLNUM),
C             where NRM is the varying Frobenius norm of the matrices
C             formed by concatenating K pairs of adjacent diagonal
C             blocks of sizes 1 and/or 2 in the T22_k submatrices from
C             (2), which are swapped, and EPS and SMLNUM are the machine
C             precision and safe minimum divided by EPS, respectively
C             (see LAPACK Library routine DLAMCH). The value c should
C             normally be at least 10.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (4*K)
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal LDWORK.
C
C     LDWORK  INTEGER
C             The dimension of the array DWORK.
C             LDWORK >= 10*K + MN, if all blocks involved in reordering
C                                  have order 1;
C             LDWORK >= 25*K + MN, if there is at least a block of
C                                  order 2, but no adjacent blocks of
C                                  order 2 are involved in reordering;
C             LDWORK >= MAX(42*K + MN, 80*K - 48), if there is at least
C                                  a pair of adjacent blocks of order 2
C                                  involved in reordering;
C             where MN = MXN, if MXN > 10, and MN = 0, otherwise, with
C             MXN = MAX(N(k),k=1,...,K).
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
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value;
C             = 1:  the reordering of T failed because some eigenvalues
C                   are too close to separate (the problem is very ill-
C                   conditioned); T may have been partially reordered.
C
C     METHOD
C
C     An adaptation of the LAPACK Library routine DTGSEN is used.
C
C     NUMERICAL ASPECTS
C
C     The implemented method is numerically backward stable.
C
C     CONTRIBUTOR
C
C     R. Granat, Umea University, Sweden, Apr. 2008.
C
C     REVISIONS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     Mar. 2010, SLICOT Library version of the PEP routine PEP_DTGSEN.
C     V. Sima, July, 2010.
C
C     KEYWORDS
C
C     Orthogonal transformation, periodic QZ algorithm, periodic
C     Sylvester-like equations, QZ algorithm.
C
C     ******************************************************************
C
C .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
C     ..
C     .. Scalar Arguments ..
      CHARACTER          COMPQ, STRONG
      INTEGER            INFO, K, KSCHUR, LDWORK, M, NC
      DOUBLE PRECISION   TOL
C     ..
C     .. Array Arguments ..
      LOGICAL            SELECT( * )
      INTEGER            IWORK( * ), IXQ( * ), IXT( * ), LDQ( * ),
     $                   LDT( * ), N( * ), NI( * ), S( * ), WHICHQ( * )
      DOUBLE PRECISION   DWORK( * ), Q( * ), T( * )
C     ..
C     .. Local Scalars ..
      CHARACTER          COMPQC
      LOGICAL            INITQ, PAIR, SPECQ, SWAP, WANTQ, WANTQL, WS
      INTEGER            I, IP1, IT, L, LL, LS, MAXN, MINK, MINN,
     $                   MINSUM, MNWORK, NKP1, SUMD
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION   TOLA( 3 )
C     ..
C     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH, LSAME
C     ..
C     .. External Subroutines ..
      EXTERNAL           DLASET, MB03KA, XERBLA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, INT, MAX, MOD
C     ..
C     .. Local Functions ..
      INTEGER            INDP1
      INDP1( I, K ) = MOD( I, K ) + 1
C     ..
C     .. Executable Statements ..
C
C     Decode and test the input parameters.
C
      INFO  = 0
      INITQ = LSAME( COMPQ,  'I' )
      WANTQ = LSAME( COMPQ,  'U' ) .OR. INITQ
      SPECQ = LSAME( COMPQ,  'W' )
      WS    = LSAME( STRONG, 'S' )
C
C     Test all input arguments.
C
      IF( K.LT.2 ) THEN
         INFO = -4
C
C     Check options for generating orthogonal factors.
C
      ELSE IF( .NOT.( LSAME( COMPQ, 'N' ) .OR. WANTQ .OR. SPECQ ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( LSAME( STRONG, 'N' ) .OR. WS ) ) THEN
         INFO = -3
      ELSE IF( TOL.LE.ZERO ) THEN
         INFO = -18
      END IF
      IF( INFO.EQ.0 .AND. SPECQ ) THEN
         DO 10 L = 1, K
            IF( WHICHQ(L).LT.0 .OR. WHICHQ(L).GT.2 )
     $         INFO = -2
   10    CONTINUE
      END IF
C
C     Check whether any of the dimensions is negative.
C     At the same time the sequence of consecutive sums of dimension
C     differences is formed and its minimum is determined.
C     Also, the maximum of all dimensions is computed.
C
      IF( INFO.EQ.0 ) THEN
         SUMD   = 0
         MINK   = K
         MINSUM = 0
         MAXN   = 0
         MINN   = N(K)
C
         DO 20 L = 1, K
            IF( L.LT.K .AND. N(L).LT.MINN )
     $         MINN = N(L)
            NKP1 = N(INDP1(L,K))
            IF ( N(L).LT.0 )
     $         INFO = -7
            IF ( S(L).EQ.-1 )
     $         SUMD = SUMD + ( NKP1 - N(L) )
            IF ( SUMD.LT.MINSUM ) THEN
               MINSUM = SUMD
               MINK = L
            END IF
            MAXN = MAX( MAXN, N(L) )
C
C           Check the condition N(l) >= NI(l) + NC >= 0.
C
            IF( INFO.EQ.0 .AND. ( N(L).LT.NI(L)+NC .OR. NI(L).LT.0 ) )
     $         INFO = -8
   20    CONTINUE
      END IF
C
C     Check the condition 0 <= NC <= min(N).
C
      IF( INFO.EQ.0 .AND. ( NC.LT.0 .OR. NC.GT.MINN ) )
     $   INFO = -5
C
C     Check KSCHUR.
C
      IF( INFO.EQ.0 .AND. ( KSCHUR.LT.1 .OR. KSCHUR.GT.K ) )
     $   INFO = -6
C
C     Check that the complete sum is zero; otherwise T is singular.
C
      IF( INFO.EQ.0 .AND. SUMD.NE.0 )
     $   INFO = -7
C
C     Check signatures.
C
      IF( INFO.EQ.0 ) THEN
         DO 30 L = 1, K
            IF( ABS( S(L) ).NE.1 )
     $         INFO = -9
   30    CONTINUE
      END IF
C
C     Check the leading dimensions of T_k.
C
      IF( INFO.EQ.0 ) THEN
         DO 40 L = 1, K
            NKP1 = N(INDP1(L,K))
            IF ( S(L).EQ.1 ) THEN
               IF ( LDT(L).LT.MAX( 1, NKP1 ) )
     $            INFO = -12
            ELSE
               IF ( LDT(L).LT.MAX( 1, N(L) ) )
     $            INFO = -12
            END IF
   40    CONTINUE
      END IF
C
C     Check the leading dimensions of Q_k.
C
      IF( INFO.EQ.0 .AND. ( WANTQ .OR. SPECQ ) ) THEN
         DO 50 L = 1, K
            WANTQL = WANTQ
            IF ( SPECQ )
     $         WANTQL = WHICHQ(L).NE.0
            IF ( WANTQL ) THEN
               IF ( LDQ( L ).LT.MAX( 1, N(L) ) )
     $            INFO = -15
            END IF
   50    CONTINUE
      END IF
C
C     Set M to the dimension of the specified periodic invariant
C     subspace.
C
      M = 0
      I = KSCHUR
      PAIR = .FALSE.
      IP1  = INDP1( I, K )
      DO 70 L = 1, NC
         IF( PAIR ) THEN
            PAIR = .FALSE.
         ELSE
            IF( L.LT.NC ) THEN
               IF( S(I).EQ.1 ) THEN
                  IT = IXT(I) + ( NI(I) + L - 1 )*LDT(I) + NI(IP1) + L
               ELSE
                  IT = IXT(I) + ( NI(IP1) + L - 1 )*LDT(I) + NI(I) + L
               END IF
               IF( T(IT).EQ.ZERO ) THEN
                  IF( SELECT( L ) )
     $               M = M + 1
               ELSE
                  PAIR = .TRUE.
                  IF( SELECT( L ) .OR. SELECT( L+1 ) )
     $               M = M + 2
               END IF
            ELSE
               IF( SELECT( NC ) )
     $            M = M + 1
            END IF
         END IF
   70 CONTINUE
C
C     Set COMPQ for MB03KA, if needed.
C
      IF( INITQ ) THEN
         COMPQC = 'U'
      ELSE
         COMPQC = COMPQ
      END IF
C
C     Check workspace.
C
      IF( INFO.EQ.0 ) THEN
         CALL MB03KA( COMPQC, WHICHQ, WS, K, NC, KSCHUR, 1, 1, N, NI, S,
     $                T, LDT, IXT, Q, LDQ, IXQ, DWORK, IWORK, DWORK, -1,
     $                INFO )
         MNWORK = MAX( 1, INT( DWORK(1) ) )
         IF( LDWORK.NE.-1 .AND. LDWORK.LT.MNWORK )
     $      INFO = -21
      END IF
C
C     Quick return if possible.
C
      IF( LDWORK.EQ.-1 ) THEN
         DWORK(1) = DBLE( MNWORK )
         RETURN
      ELSE IF( INFO.LT.0 ) THEN
         CALL XERBLA( 'MB03KD', -INFO )
         RETURN
      END IF
C
C     Compute some machine-dependent parameters.
C
      TOLA( 1 ) = TOL
      TOLA( 2 ) = DLAMCH( 'Precision' )
      TOLA( 3 ) = DLAMCH( 'Safe minimum' ) / TOLA( 2 )
C
C     Initialization of orthogonal factors.
C
      DO 80 L = 1, K
         IF ( SPECQ )
     $      INITQ = WHICHQ(L).EQ.1
         IF ( INITQ )
     $      CALL DLASET( 'All', N(L), N(L), ZERO, ONE, Q( IXQ( L ) ),
     $                   LDQ( L ) )
   80 CONTINUE
C
C     Collect the selected blocks at the top-left corner of T22_k.
C
      LS   = 0
      PAIR = .FALSE.
      I    = KSCHUR
      IP1  = INDP1( I, K )
      DO 90 L = 1, NC
         IF( PAIR ) THEN
            PAIR = .FALSE.
         ELSE
            SWAP = SELECT( L )
            IF( L.LT.NC ) THEN
               IF( S(I).EQ.1 ) THEN
                  IT = IXT(I) + ( NI(I) + L - 1 )*LDT(I) + NI(IP1) + L
               ELSE
                  IT = IXT(I) + ( NI(IP1) + L - 1 )*LDT(I) + NI(I) + L
               END IF
               IF( T(IT).NE.ZERO ) THEN
                  PAIR = .TRUE.
                  SWAP = SWAP .OR. SELECT( L+1 )
               END IF
            END IF
            IF( SWAP ) THEN
               LS = LS + 1
C
C              Swap the L-th block to position LS in T22_k.
C
               LL = L
               IF( L.NE.LS ) THEN
                  CALL MB03KA( COMPQC, WHICHQ, WS, K, NC, KSCHUR, LL,
     $                         LS, N, NI, S, T, LDT, IXT, Q, LDQ, IXQ,
     $                         TOLA, IWORK, DWORK, LDWORK, INFO )
                  IF( INFO.NE.0 ) THEN
C
C                    Blocks too close to swap; exit.
C
                     GO TO 100
                  END IF
               END IF
               IF( PAIR )
     $            LS = LS + 1
            END IF
         END IF
   90 CONTINUE
C
  100 CONTINUE
C
C     Store optimal workspace length and return.
C
      DWORK(1) = DBLE( MNWORK )
      RETURN
C
C *** Last line of MB03KD ***
      END
