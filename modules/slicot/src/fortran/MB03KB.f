      SUBROUTINE MB03KB( COMPQ, WHICHQ, WS, K, NC, KSCHUR, J1, N1, N2,
     $                   N, NI, S, T, LDT, IXT, Q, LDQ, IXQ, TOL, IWORK,
     $                   DWORK, LDWORK, INFO )
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
C        T22_K^S(K) * T22_K-1^S(K-1) * ... * T22_1^S(1)              (1)
C
C     of length K in the generalized periodic Schur form
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
C     such that pairs of adjacent diagonal blocks of sizes 1 and/or 2 in
C     the product (1) are swapped.
C
C     Optionally, the transformation matrices Q_1,...,Q_K from the
C     reduction into generalized periodic Schur form are updated with
C     respect to the performed reordering.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     COMPQ   CHARACTER*1
C             = 'N': do not compute any of the matrices Q_k;
C             = 'U': each coefficient of Q must contain an orthogonal
C                    matrix Q1_k on entry, and the products Q1_k*Q_k are
C                    returned, where Q_k, k = 1, ..., K, performed the
C                    reordering;
C             = 'W': the computation of each Q_k is specified
C                    individually in the array WHICHQ.
C
C     WHICHQ  INTEGER array, dimension (K)
C             If COMPQ = 'W', WHICHQ(k) specifies the computation of Q_k
C             as follows:
C             = 0:   do not compute Q_k;
C             > 0:   the kth coefficient of Q must contain an orthogonal
C                    matrix Q1_k on entry, and the product Q1_k*Q_k is
C                    returned.
C             This array is not referenced if COMPQ <> 'W'.
C
C     WS      LOGICAL
C             = .FALSE. : do not perform the strong stability tests;
C             = .TRUE.  : perform the strong stability tests; often,
C                         this is not needed, and omitting them can save
C                         some computations.
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
C             triangular.
C
C     J1      (input) INTEGER
C             The index of the first row and column of the first block
C             to swap in T22_k.
C             1 <= J1 <= NC-N1-N2+1.
C
C     N1      (input) INTEGER
C             The order of the first block to swap.   N1 = 0, 1 or 2.
C
C     N2      (input) INTEGER
C             The order of the second block to swap.  N2 = 0, 1 or 2.
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
C             COMPQ = 'U', or COMPQ = 'W' and WHICHQ(k) > 0.
C             On exit, if COMPQ = 'U', or COMPQ = 'W' and WHICHQ(k) > 0,
C             Q_k is post-multiplied with the orthogonal matrix that
C             performed the reordering.
C             This array is not referenced if COMPQ = 'N'.
C
C     LDQ     INTEGER array, dimension (K)
C             The leading dimensions of the matrices Q_k in the one-
C             dimensional array Q.  LDQ(k) >= 1, and
C             LDQ(k) >= max(1,N(k)), if COMPQ = 'U', or COMPQ = 'W' and
C                                                       WHICHQ(k) > 0;
C             This array is not referenced if COMPQ = 'N'.
C
C     IXQ     INTEGER array, dimension (K)
C             Start indices of the matrices Q_k in the one-dimensional
C             array Q.
C             This array is not referenced if COMPQ = 'N'.
C
C     Tolerances
C
C     TOL     DOUBLE PRECISION array, dimension (3)
C             This array contains tolerance parameters. The weak and
C             strong stability tests use a threshold computed by the
C             formula  MAX( c*EPS*NRM, SMLNUM ),  where c is a constant,
C             NRM is the Frobenius norm of the matrix formed by
C             concatenating K pairs of adjacent diagonal blocks of sizes
C             1 and/or 2 in the T22_k submatrices from (2), which are
C             swapped, and EPS and SMLNUM are the machine precision and
C             safe minimum divided by EPS, respectively (see LAPACK
C             Library routine DLAMCH). The norm NRM is computed by this
C             routine; the other values are stored in the array TOL.
C             TOL(1), TOL(2), and TOL(3) contain c, EPS, and SMLNUM,
C             respectively. TOL(1) should normally be at least 10.
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
C             LDWORK >= 10*K + MN,                 if N1 = 1, N2 = 1;
C             LDWORK >= 25*K + MN,                 if N1 = 1, N2 = 2;
C             LDWORK >= MAX(23*K + MN, 25*K - 12), if N1 = 2, N2 = 1;
C             LDWORK >= MAX(42*K + MN, 80*K - 48), if N1 = 2, N2 = 2;
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
C             < 0:  if INFO = -22, then LDWORK is too small; appropriate
C                   value for LDWORK is returned in DWORK(1); the other
C                   arguments are not tested, for efficiency;
C             = 1:  the swap was rejected from stability reasons; the
C                   blocks are not swapped and T and Q are unchanged.
C
C     METHOD
C
C     The algorithm described in [1] is used. Both weak and strong
C     stability tests are performed.
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
C                                  3
C     The algorithm requires 0(K NC ) floating point operations.
C
C     CONTRIBUTOR
C
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     Mar. 2010, an essentially new version of the PEP routine
C     PEP_DLAEXC, by R. Granat, Umea University, Sweden, Apr. 2008.
C
C     REVISIONS
C
C     V. Sima, Apr. 2010, May 2010, July 2010.
C
C     KEYWORDS
C
C     Eigenvalues, QZ algorithm, periodic QZ algorithm, orthogonal
C     transformation.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
C     ..
C     .. Scalar Arguments ..
      CHARACTER          COMPQ
      LOGICAL            WS
      INTEGER            INFO, J1, K, KSCHUR, LDWORK, N1, N2, NC
C     ..
C     .. Array Arguments ..
      INTEGER            IWORK( * ), IXQ( * ), IXT( * ), LDQ( * ),
     $                   LDT( * ), N( * ), NI( * ), S( * ), WHICHQ( * )
      DOUBLE PRECISION   DWORK( * ), Q( * ), T( * ), TOL( * )
C     ..
C     .. Local Scalars ..
      LOGICAL            FILL21, FILL43, FILLIN, SPECQ, WANTQ, WANTQL
      INTEGER            A, B, C, I, I11, I12, I21, I22, IA, IB, IC,
     $                   II, INDF1, INDF2, INDTAU, INDTT, INDV1, INDV2,
     $                   INDVF, INDVP1, INDXC, INDXV, IP1, IPP, IQ, IS,
     $                   IT, IT2, ITAU1, ITAU2, ITAUF, ITAUF1, ITAUF2,
     $                   ITAUP1, IV1P1, IV2P1, J2, J3, J4, L, LTAU,
     $                   LTAU1, LTAU2, LTT, MINWRK, MN, ND, ND2, TAU,
     $                   TAU1, TAU1P1, TAU2, TAU2P1, TT, V, V1, V2,
     $                   VLOC, VLOC1, VLOC2, W, WE
      DOUBLE PRECISION   DNRM, DTAU1, DTAU2, EPS, SCALOC, SMLNUM,
     $                   STRONG, TAULOC, THRESH, TMP, TMP1, TMP2, V_1,
     $                   V_2, V_3, W_2, W_3, X_11, X_12, X_21, X_22
C     ..
C     .. Local Arrays ..
      DOUBLE PRECISION   TAUS( 2 ), TEMP( 16 ), TEMPM1( 16 )
C     ..
C     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLANGE, DLANTR, DLAPY2
      EXTERNAL           DLANGE, DLANTR, DLAPY2, LSAME
C     ..
C     .. External Subroutines ..
      EXTERNAL           DAXPY, DLACPY, DLARFG, DLARFX, DLASCL, MB03KC,
     $                   MB03KE, XERBLA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, INT, MAX, MOD
C     ..
C     .. Executable Statements ..
C
C     Decode the input parameters
C
      INFO  = 0
      WANTQ = LSAME( COMPQ, 'U' )
      SPECQ = LSAME( COMPQ, 'W' )
C
C     Set the machine-dependent parameters.
C
      EPS    = TOL( 2 )
      SMLNUM = TOL( 3 )
C
C     For efficiency reasons, the parameters are not checked.
C
C     Set integer pointers to correct subsequences in T22_k and check
C     workspace. For simplicity, below these subsequences are denoted
C     by T11, T22 and T12 and are not to be confused with the T11_k,
C     T22_k and T12_k in (2). Also set integer pointers to be used in
C     Sylvester solver.
C
      J2  = J1  + N1
      I11 = 0
      I21 = I11 + K
      I12 = I21 + K
      I22 = I12 + K
      MN  = 0
C
      DO 10 I = 1, K
         MN  = MAX( MN, N( I ) )
         IP1 = MOD( I, K ) + 1
         IF( S( I ).EQ.1 ) THEN
            II = IXT( I ) + NI( I )*LDT( I ) + NI( IP1 ) - 1
         ELSE
            II = IXT( I ) + NI( IP1 )*LDT( I ) + NI( I ) - 1
         END IF
         IWORK( I11+I ) = II + ( J1 - 1 )*LDT( I ) + J1
         IWORK( I21+I ) = IWORK( I11+I ) + N1
         IWORK( I12+I ) = IWORK( I11+I ) + N1*LDT( I )
         IWORK( I22+I ) = IWORK( I12+I ) + N1
   10 CONTINUE
C
C     Divide workspace into different arrays and submatrices.
C
      A = 1
      IF( N1.EQ.1 .AND. N2.EQ.1 ) THEN
         B     = A     + K
         C     = B     + K
         TAU   = C     + K
         V     = TAU   + K
         TT    = V     + K * 2
         W     = TT    + K * 4
         WE    = TAU
         MN    = MN    + K * 10
      ELSE IF( N1.EQ.1 .AND. N2.EQ.2 ) THEN
         B     = A     + K
         C     = B     + K * 4
         TAU1  = C     + K * 2
         V1    = TAU1  + K
         TAU2  = V1    + K * 2
         V2    = TAU2  + K
         TT    = V2    + K * 2
         LTAU  = TT    + K * 9
         VLOC  = LTAU  + K
         W     = VLOC  + K * 2
         WE    = TAU1
         MN    = MN    + K * 25
      ELSE IF( N1.EQ.2 .AND. N2.EQ.1 ) THEN
         B     = A     + K * 4
         C     = B     + K
         TAU   = C     + K * 2
         V     = TAU   + K
         TT    = V     + K * 3
         LTAU  = TT    + K * 9
         VLOC  = LTAU  + K
         W     = VLOC  + K * 2
         WE    = TAU
         MN    = MN    + K * 23
      ELSE IF( N1.EQ.2 .AND. N2.EQ.2 ) THEN
         B     = A     + K * 4
         C     = B     + K * 4
         TAU1  = C     + K * 4
         V1    = TAU1  + K
         TAU2  = V1    + K * 3
         V2    = TAU2  + K
         TT    = V2    + K * 3
         LTAU1 = TT    + K * 16
         VLOC1 = LTAU1 + K
         LTAU2 = VLOC1 + K * 2
         VLOC2 = LTAU2 + K
         W     = VLOC2 + K * 2
         WE    = TAU1
         MN    = MN    + K * 42
      END IF
C
      CALL MB03KE( .FALSE., .FALSE., -1, K, N1, N2, EPS, SMLNUM, S, T,
     $             T, T, SCALOC, DWORK, -1, INFO )
      MINWRK = MAX( INT( DWORK( 1 ) ) + WE - 1, MN )
C
C     Quick return if possible.
C
      DWORK( 1 ) = DBLE( MINWRK )
      IF( LDWORK.EQ.-1 ) THEN
         RETURN
      ELSE IF( LDWORK.LT.MINWRK ) THEN
         INFO = -22
         CALL XERBLA( 'MB03KB', -INFO )
         RETURN
      ELSE IF( NC.LE.1 .OR. N1.LE.0 .OR. N2.LE.0 .OR. N1.GT.NC .OR.
     $        J2.GT.NC .OR. J2 + N2 - 1.GT.NC ) THEN
         RETURN
      END IF
C
C     Compute some local indices.
C
      J2 = J1 + 1
      J3 = J1 + 2
      J4 = J1 + 3
C
C     Solve the periodic Sylvester-like equation associated with
C     the swap.
C
C     Copy T11, T22 and T12 to workspace. Apply scaling to all of T22_k
C     for numerical stability.
C
      IA  = A
      IB  = B
      IC  = C
      ND  = N1 + N2
      ND2 = ND**2
C
      DNRM = ZERO
C
      DO 20 I = 1, K
         IT  = IWORK( I11+I )
         IS  = IWORK( I12+I )
         IQ  = IWORK( I22+I )
         TMP = DLANTR( 'Frobenius', 'Upper', 'NonUnit', ND, ND, T( IT ),
     $                 LDT( I ), DWORK )
         IF( I.EQ.KSCHUR ) THEN
            IF( N1.EQ.2 )
     $         TMP = DLAPY2( T( IT+1 ), TMP )
            IF( N2.EQ.2 )
     $         TMP = DLAPY2( T( IQ+1 ), TMP )
         END IF
         DNRM = DLAPY2( DNRM, TMP )
         TMP  = MAX( TMP, SMLNUM )
         IF( N1.EQ.1 ) THEN
            DWORK( IA ) = T( IT ) / TMP
            DWORK( IC ) = T( IS ) / TMP
            IF( N2.EQ.1 ) THEN
               DWORK( IB ) = T( IQ ) / TMP
            ELSE
               CALL DLACPY( 'All', N2, N2, T( IQ ), LDT( I ),
     $                      DWORK( IB ), N2 )
               CALL DLASCL( 'General', 0, 0, TMP, ONE, N2, N2,
     $                      DWORK( IB ), N2, INFO )
               DWORK( IC+1 ) = T( IS+LDT(I) ) / TMP
            END IF
         ELSE
            CALL DLACPY( 'All', N1, N1, T( IT ), LDT( I ), DWORK( IA ),
     $                   N1 )
            CALL DLASCL( 'General', 0, 0, TMP, ONE, N1, N1, DWORK( IA ),
     $                   N1, INFO )
            IF( N2.EQ.1 ) THEN
               DWORK( IB   ) = T( IQ   ) / TMP
               DWORK( IC   ) = T( IS   ) / TMP
               DWORK( IC+1 ) = T( IS+1 ) / TMP
            ELSE
               CALL DLACPY( 'All', N2, N2, T( IQ ), LDT( I ),
     $                      DWORK( IB ), N2 )
               CALL DLASCL( 'General', 0, 0, TMP, ONE, N2, N2,
     $                      DWORK( IB ), N2, INFO )
               CALL DLACPY( 'All', N1, N2, T( IS ), LDT( I ),
     $                      DWORK( IC ), N1 )
               CALL DLASCL( 'General', 0, 0, TMP, ONE, N1, N2,
     $                      DWORK( IC ), N1, INFO )
            END IF
         END IF
         IA = IA + N1**2
         IB = IB + N2**2
         IC = IC + N1*N2
   20 CONTINUE
C
C     Compute a machine-dependent threshold of the test for accepting
C     a swap.
C
      THRESH = MAX( TOL( 1 )*EPS*DNRM, SMLNUM )
C
C     Call the periodic Sylvester-like equation solver.
C     Workspace: need   WE - 1 + (4*K-3)*(N1*N2)**2 + K*N1*N2.
C
      CALL MB03KE( .FALSE., .FALSE., -1, K, N1, N2, EPS, SMLNUM, S,
     $             DWORK( A ), DWORK( B ), DWORK( C ), SCALOC,
     $             DWORK( WE ), LDWORK-WE+1, INFO )
C
C     Swap the adjacent diagonal blocks.
C
      L = N1 + N1 + N2 - 2
      GO TO ( 30, 70, 140, 210 ) L
C
   30 CONTINUE
C
C     Direct swap with N1 = 1 and N2 = 1.
C
C     Generate elementary reflectors H_i such that:
C
C     H_i( X_11_i ) = ( * ).
C        ( scale  )   ( 0 )
C
      INDXC = C
      INDXV = V
C
      DO 40 INDTAU = TAU, TAU + K - 1
         X_11 = DWORK( INDXC )
         DWORK( INDXV   ) = X_11
         DWORK( INDXV+1 ) = SCALOC
         CALL DLARFG( 2, DWORK( INDXV ), DWORK( INDXV+1 ), 1,
     $                DWORK( INDTAU ) )
         DWORK( INDXV ) = ONE
C
C        Next, do weak stability test.
C
         TAULOC = DWORK( INDTAU )
         TMP    = SCALOC * ( ONE - TAULOC ) +
     $            TAULOC * DWORK( INDXV+1 ) * X_11
         IF( ABS( TMP ).GT.THRESH )
     $      GO TO 300
         INDXC = INDXC + 1
         INDXV = INDXV + 2
   40 CONTINUE
C
      IF( WS ) THEN
C
C        The swap passed weak stability test - move on and perform the
C        swapping temporarily into TT (workspace) and perform strong
C        stability test.
C
         INDTAU = TAU
         INDXV  = V
         INDTT  = TT
C
         DO 50 I  = 1, K
            IP1    = MOD( I, K )
            INDVP1 = V   + IP1 * 2
            ITAUP1 = TAU + IP1
            CALL DLACPY( 'All', 2, 2, T( IWORK( I11+I ) ), LDT( I ),
     $                   TEMP, 2 )
            CALL DLACPY( 'All', 2, 2, TEMP, 2, DWORK( INDTT ), 2 )
            IF( S( I ).EQ.1 ) THEN
               CALL DLARFX( 'Left', 2, 2, DWORK( INDVP1 ),
     $                      DWORK( ITAUP1 ), DWORK( INDTT ), 2,
     $                      DWORK( W ) )
               CALL DLARFX( 'Right', 2, 2, DWORK( INDXV ),
     $                      DWORK( INDTAU ), DWORK( INDTT ), 2,
     $                      DWORK( W ) )
            ELSE
               CALL DLARFX( 'Right', 2, 2, DWORK( INDVP1 ),
     $                      DWORK( ITAUP1 ), DWORK( INDTT ), 2,
     $                      DWORK( W ) )
               CALL DLARFX( 'Left', 2, 2, DWORK( INDXV ),
     $                      DWORK( INDTAU ), DWORK( INDTT ), 2,
     $                      DWORK( W ) )
            END IF
C
            CALL DLACPY( 'All', 2, 2, DWORK( INDTT ), 2, TEMPM1, 2 )
            IF( S( I ).EQ.1 ) THEN
               CALL DLARFX( 'Left', 2, 2, DWORK( INDVP1 ),
     $                      DWORK( ITAUP1 ), TEMPM1, 2, DWORK( W ) )
               CALL DLARFX( 'Right', 2, 2, DWORK( INDXV ),
     $                      DWORK( INDTAU ), TEMPM1, 2, DWORK( W ) )
            ELSE
               CALL DLARFX( 'Right', 2, 2, DWORK( INDVP1 ),
     $                      DWORK( ITAUP1 ), TEMPM1, 2, DWORK( W ) )
               CALL DLARFX( 'Left', 2, 2, DWORK( INDXV ),
     $                      DWORK( INDTAU ), TEMPM1, 2, DWORK( W ) )
            END IF
            CALL DAXPY( ND2, -ONE, TEMP, 1, TEMPM1, 1 )
            STRONG = DLANGE( 'Frobenius', 2, 2, TEMPM1, 2, DWORK )
            IF( STRONG.GT.THRESH )
     $         GO TO 300
C
            INDTAU = INDTAU + 1
            INDXV  = INDXV  + 2
            INDTT  = INDTT  + 4
   50    CONTINUE
C
      END IF
C
C     The swap was accepted - now update T and Q with respect to the
C     swapping.
C
      INDTAU = TAU
      INDXV  = V
C
      DO 60 I = 1, K
         IP1 = MOD( I, K )
C
C        Apply Householder transformations from left or right depending
C        on S and accumulate transformations in matrices Q_i, i = 1:K.
C
         INDVP1 = V   + IP1 * 2
         ITAUP1 = TAU + IP1
C
         IP1 = IP1 + 1
         IT  = IWORK( I11+I ) - J1 + 1
         IF( S( I ).EQ.1 ) THEN
            CALL DLARFX( 'Left', 2, N( I )-J1+1, DWORK( INDVP1 ),
     $                   DWORK( ITAUP1 ), T( IWORK( I11+I ) ), LDT( I ),
     $                   DWORK( W ) )
            CALL DLARFX( 'Right', NI( IP1 )+J2, 2, DWORK( INDXV ),
     $                   DWORK( INDTAU ), T( IT-NI( IP1 ) ), LDT( I ),
     $                   DWORK( W ) )
         ELSE
            CALL DLARFX( 'Left', 2, N( IP1 )-J1+1, DWORK( INDXV ),
     $                   DWORK( INDTAU ), T( IWORK( I11+I ) ), LDT( I ),
     $                   DWORK( W ) )
            CALL DLARFX( 'Right', NI( I )+J2, 2, DWORK( INDVP1 ),
     $                   DWORK( ITAUP1 ), T( IT-NI( I ) ), LDT( I ),
     $                   DWORK( W ) )
         END IF
C
C        Set to zero the fill-in element T(J2,J1,I).
C
         T( IWORK( I21+I ) ) = ZERO
C
         WANTQL = WANTQ
         IF ( SPECQ )
     $      WANTQL = WHICHQ( I ).NE.0
         IF ( WANTQL ) THEN
            IQ = IXQ( I ) + ( J1 - 1 )*LDQ( I )
            CALL DLARFX( 'Right', N( I ), 2, DWORK( INDXV ),
     $                   DWORK( INDTAU ), Q( IQ ), LDQ( I ),
     $                   DWORK( W ) )
         END IF
         INDTAU = INDTAU + 1
         INDXV  = INDXV  + 2
   60 CONTINUE
C
C     Exit direct swap N1 = 1 and N2 = 1.
C
      GOTO 290
C
C     Direct swap with N1 = 1 and N2 = 2.
C
   70 CONTINUE
C
C     Generate elementary reflectors H(1)_i and H(2)_i such that
C
C     H(2)_i H(1)_i  ( X_11_i X_12_i ) = ( * * ).
C                    (  scale    0   )   ( 0 * )
C                    (   0     scale )   ( 0 0 )
C
      ITAU2 = TAU2
      INDXC = C
      INDV1 = V1
      INDV2 = V2
C
      DO 80 ITAU1 = TAU1, TAU1 + K - 1
C
C        Compute elementary reflector H(1)_i.
C
         X_11 = DWORK( INDXC )
         X_12 = DWORK( INDXC+1 )
         DWORK( INDV1   ) = X_11
         DWORK( INDV1+1 ) = SCALOC
         CALL DLARFG( 2, DWORK( INDV1 ), DWORK( INDV1+1 ), 1,
     $                DWORK( ITAU1 ) )
         DWORK( INDV1 ) = ONE
C
C        Compute elementary reflector H(2)_i.
C
         DWORK( INDV2   ) = X_12
         DWORK( INDV2+1 ) = ZERO
         CALL DLARFX( 'Left', 2, 1, DWORK( INDV1 ), DWORK( ITAU1 ),
     $                DWORK( INDV2 ), 2, DWORK( W ) )
         DWORK( INDV2   ) = DWORK( INDV2+1 )
         DWORK( INDV2+1 ) = SCALOC
         CALL DLARFG( 2, DWORK( INDV2 ), DWORK( INDV2+1 ), 1,
     $                DWORK( ITAU2 ) )
         DWORK( INDV2 ) = ONE
C
C        Next, do weak stability test.
C
         TAUS( 1 ) = DWORK( ITAU1 )
         TAUS( 2 ) = DWORK( ITAU2 )
         V_1  = DWORK( INDV1+1 )
         TMP1 = SCALOC * ( ONE - TAUS( 1 ) ) + TAUS( 1 ) * V_1 * X_11
         TMP2 = -( SCALOC * TAUS( 1 ) * V_1 + X_11 *
     $              ( ONE - TAUS( 1 ) * V_1**2 ) ) * ( ONE - TAUS( 2 ) )
     $          + TAUS( 2 ) * DWORK( INDV2+1 ) * X_12
         IF( DLAPY2( TMP1, TMP2 ).GT.THRESH )
     $      GO TO 300
         ITAU2 = ITAU2 + 1
         INDXC = INDXC + 2
         INDV1 = INDV1 + 2
         INDV2 = INDV2 + 2
   80 CONTINUE
C
C     The swap passed weak stability test - move on and perform the
C     swapping temporarily into TT (workspace).
C
      ITAU1 = TAU1
      ITAU2 = TAU2
      INDV1 = V1
      INDV2 = V2
      INDTT = TT
      LTT   = 3
C
      DO 90 I  = 1, K
         IP1    = MOD( I, K )
         IV1P1  = V1   + IP1 * 2
         IV2P1  = V2   + IP1 * 2
         TAU1P1 = TAU1 + IP1
         TAU2P1 = TAU2 + IP1
         CALL DLACPY( 'All', 3, 3, T( IWORK( I11+I ) ), LDT( I ),
     $                DWORK( INDTT ), 3 )
C
         IF( S( I ).EQ.1 ) THEN
            CALL DLARFX( 'Left', 2, 3, DWORK( IV1P1 ), DWORK( TAU1P1 ),
     $                   DWORK( INDTT ), 3, DWORK( W ) )
            CALL DLARFX( 'Left', 2, 3, DWORK( IV2P1 ), DWORK( TAU2P1 ),
     $                   DWORK( INDTT+1 ), 3, DWORK( W ) )
            CALL DLARFX( 'Right', 3, 2, DWORK( INDV1 ), DWORK( ITAU1 ),
     $                   DWORK( INDTT ), 3, DWORK( W ) )
            CALL DLARFX( 'Right', 3, 2, DWORK( INDV2 ), DWORK( ITAU2 ),
     $                   DWORK( INDTT+3 ), 3, DWORK( W ) )
         ELSE
            CALL DLARFX( 'Right', 3, 2, DWORK( IV1P1 ), DWORK( TAU1P1 ),
     $                   DWORK( INDTT ), 3, DWORK( W ) )
            CALL DLARFX( 'Right', 3, 2, DWORK( IV2P1 ), DWORK( TAU2P1 ),
     $                   DWORK( INDTT+3 ), 3, DWORK( W ) )
            CALL DLARFX( 'Left', 2, 3, DWORK( INDV1 ), DWORK( ITAU1 ),
     $                   DWORK( INDTT ), 3, DWORK( W ) )
            CALL DLARFX( 'Left', 2, 3, DWORK( INDV2 ), DWORK( ITAU2 ),
     $                   DWORK( INDTT+1 ), 3, DWORK( W ) )
         END IF
         ITAU1 = ITAU1 + 1
         ITAU2 = ITAU2 + 1
         INDV1 = INDV1 + 2
         INDV2 = INDV2 + 2
         INDTT = INDTT + 9
   90 CONTINUE
C
C     Check for fill-in elements in the new 2-by-2 block.
C
      FILLIN = .FALSE.
      INDTT  = TT + 1
      DO 100 I = 1, K
         IF( I.NE.KSCHUR .AND. ABS( DWORK( INDTT ) ).GT.THRESH )
     $      FILLIN = .TRUE.
         INDTT = INDTT + 9
  100 CONTINUE
C
C     Found fill-in elements?
C
      IF( FILLIN ) THEN
C
C        Restore periodic Schur form.
C
         CALL MB03KC( K, KSCHUR, LTT, 1, S, DWORK( TT ), LTT,
     $                DWORK( VLOC ), DWORK( LTAU ) )
      END IF
C
      IF( WS ) THEN
C
C        Perform strong stability test.
C
         ITAU1 = TAU1
         ITAU2 = TAU2
         ITAUF = LTAU
         INDV1 = V1
         INDV2 = V2
         INDVF = VLOC
         INDTT = TT
C
         DO 110 I = 1, K
            IP1 = MOD( I, K )
            CALL DLACPY( 'All', 3, 3, DWORK( INDTT ), 3, TEMPM1, 3 )
C
C           Apply possible transformations from fill-in removal.
C
            IF( FILLIN ) THEN
               INDVP1 = VLOC + IP1 * 2
               ITAUP1 = LTAU + IP1
C
C              Apply on top-left 2-by-2 block.
C
               IF( S( I ).EQ.1 ) THEN
                  CALL DLARFX( 'Left', 2, 3, DWORK( INDVP1 ),
     $                         DWORK( ITAUP1 ), TEMPM1, 3, DWORK( W ) )
                  CALL DLARFX( 'Right', 3, 2, DWORK( INDVF ),
     $                         DWORK( ITAUF ),  TEMPM1, 3, DWORK( W ) )
               ELSE
                  CALL DLARFX( 'Right', 3, 2, DWORK( INDVP1 ),
     $                         DWORK( ITAUP1 ), TEMPM1, 3, DWORK( W ) )
                  CALL DLARFX( 'Left', 2, 3, DWORK( INDVF ),
     $                         DWORK( ITAUF ),  TEMPM1, 3, DWORK( W ) )
               END IF
            END IF
C
C           Take the "large" transformations.
C
            IV1P1  = V1   + IP1 * 2
            IV2P1  = V2   + IP1 * 2
            TAU1P1 = TAU1 + IP1
            TAU2P1 = TAU2 + IP1
C
C           Apply H(1)_i+1 * H(2)_i+1 from left or right depending on S.
C           Apply H(2)_i * H(1)_i from right or left depending on S.
C
            IF( S( I ).EQ.1 ) THEN
               CALL DLARFX( 'Left', 2, 3, DWORK( IV2P1 ),
     $                      DWORK( TAU2P1 ), TEMPM1( 2 ), 3, DWORK( W )
     $                    )
               CALL DLARFX( 'Left', 2, 3, DWORK( IV1P1 ),
     $                      DWORK( TAU1P1 ), TEMPM1, 3, DWORK( W ) )
               CALL DLARFX( 'Right', 3, 2, DWORK( INDV2 ),
     $                      DWORK( ITAU2 ), TEMPM1( 4 ), 3, DWORK( W ) )
               CALL DLARFX( 'Right', 3, 2, DWORK( INDV1 ),
     $                      DWORK( ITAU1 ), TEMPM1, 3, DWORK( W ) )
            ELSE
               CALL DLARFX( 'Right', 3, 2, DWORK( IV2P1 ),
     $                      DWORK( TAU2P1 ), TEMPM1( 4 ), 3, DWORK( W )
     $                    )
               CALL DLARFX( 'Right', 3, 2, DWORK( IV1P1 ),
     $                      DWORK( TAU1P1 ), TEMPM1, 3, DWORK( W ) )
               CALL DLARFX( 'Left', 2, 3, DWORK( INDV2 ),
     $                      DWORK( ITAU2 ), TEMPM1( 2 ), 3, DWORK( W ) )
               CALL DLARFX( 'Left', 2, 3, DWORK( INDV1 ),
     $                      DWORK( ITAU1 ), TEMPM1, 3, DWORK( W ) )
            END IF
C
C           Compute residual norm.
C
            CALL DLACPY( 'All', 3, 3, T( IWORK( I11+I ) ), LDT( I ),
     $                   TEMP, 3 )
            CALL DAXPY( ND2, -ONE, TEMP, 1, TEMPM1, 1 )
            STRONG = DLANGE( 'Frobenius', 3, 3, TEMPM1, 3, DWORK )
            IF( STRONG.GT.THRESH )
     $         GO TO 300
C
            ITAU1 = ITAU1 + 1
            ITAU2 = ITAU2 + 1
            ITAUF = ITAUF + 1
            INDV1 = INDV1 + 2
            INDV2 = INDV2 + 2
            INDVF = INDVF + 2
            INDTT = INDTT + 9
  110    CONTINUE
C
      END IF
C
C     The swap was accepted - now update T and Q with respect to the
C     swapping.

      ITAU1 = TAU1
      ITAU2 = TAU2
      ITAUF = LTAU
      INDV1 = V1
      INDV2 = V2
      INDVF = VLOC
C
      DO 120 I = 1, K
         IP1 = MOD( I, K )
         IT  = IWORK( I11+I ) - J1 + 1
C
C        Apply Householder transformations from left or right depending
C        on S and accumulate transformations in matrices Q_i, i = 1:K.
C
         IV1P1  = V1   + IP1 * 2
         IV2P1  = V2   + IP1 * 2
         TAU1P1 = TAU1 + IP1
         TAU2P1 = TAU2 + IP1
         IP1    = IP1  + 1
C
         IF( S( I ).EQ.1 ) THEN
            IT  = IT - NI( IP1 )
            IT2 = IT + LDT( I )
            CALL DLARFX( 'Left', 2, N( I )-J1+1, DWORK( IV1P1 ),
     $                   DWORK( TAU1P1 ), T( IWORK( I11+I ) ), LDT( I ),
     $                   DWORK( W ) )
            CALL DLARFX( 'Left', 2, N( I )-J1+1, DWORK( IV2P1 ),
     $                   DWORK( TAU2P1 ), T( IWORK( I21+I ) ), LDT( I ),
     $                   DWORK( W ) )
            CALL DLARFX( 'Right', NI( IP1 )+J3, 2, DWORK( INDV1 ),
     $                   DWORK( ITAU1 ), T( IT ), LDT( I ), DWORK( W ) )
            CALL DLARFX( 'Right', NI( IP1 )+J3, 2, DWORK( INDV2 ),
     $                   DWORK( ITAU2 ), T( IT2 ), LDT( I ), DWORK( W )
     $                 )
         ELSE
            IT  = IT -  NI( I )
            IT2 = IT + LDT( I )
            CALL DLARFX( 'Left', 2, N( IP1 )-J1+1, DWORK( INDV1 ),
     $                   DWORK( ITAU1 ), T( IWORK( I11+I ) ), LDT( I ),
     $                   DWORK( W ) )
            CALL DLARFX( 'Left', 2, N( IP1 )-J1+1, DWORK( INDV2 ),
     $                   DWORK( ITAU2 ), T( IWORK( I21+I ) ), LDT( I ),
     $                   DWORK( W ) )
            CALL DLARFX( 'Right', NI( I )+J3, 2, DWORK( IV1P1 ),
     $                   DWORK( TAU1P1 ), T( IT ), LDT( I ), DWORK( W )
     $                 )
            CALL DLARFX( 'Right', NI( I )+J3, 2, DWORK( IV2P1 ),
     $                   DWORK( TAU2P1 ), T( IT2 ), LDT( I ), DWORK( W )
     $                 )
         END IF
C
         WANTQL = WANTQ
         IF ( SPECQ )
     $      WANTQL = WHICHQ( I ).NE.0
         IF ( WANTQL ) THEN
            IQ = IXQ( I ) + ( J1 - 1 )*LDQ( I )
            CALL DLARFX( 'Right', N( I ), 2, DWORK( INDV1 ),
     $                   DWORK( ITAU1 ), Q( IQ ), LDQ( I ),
     $                   DWORK( W ) )
            IQ = IQ + LDQ( I )
            CALL DLARFX( 'Right', N( I ), 2, DWORK( INDV2 ),
     $                   DWORK( ITAU2 ), Q( IQ ), LDQ( I ),
     $                   DWORK( W ) )
         END IF
C
C        Apply Householder transformations from fill-in removal and
C        accumulate transformations in matrices Q_i, i=1,...,K.
C
         IF ( FILLIN ) THEN
            IV1P1  = VLOC + ( IP1 - 1 ) * 2
            TAU1P1 = LTAU + ( IP1 - 1 )
C
            IF( S( I ).EQ.1 ) THEN
               CALL DLARFX( 'Left', 2, N( I )-J1+1, DWORK( IV1P1 ),
     $                      DWORK( TAU1P1 ), T( IWORK( I11+I ) ),
     $                      LDT( I ), DWORK( W ) )
               CALL DLARFX( 'Right', NI( IP1 )+J2, 2, DWORK( INDVF ),
     $                      DWORK( ITAUF ), T( IT ), LDT( I ),
     $                      DWORK( W ) )
            ELSE
               CALL DLARFX( 'Right', NI( I )+J2, 2, DWORK( IV1P1 ),
     $                      DWORK( TAU1P1 ), T( IT ), LDT( I ),
     $                      DWORK( W ) )
               CALL DLARFX( 'Left', 2, N( IP1 )-J1+1, DWORK( INDVF ),
     $                      DWORK( ITAUF ), T( IWORK( I11+I ) ),
     $                      LDT( I ), DWORK( W ) )
            END IF
C
            WANTQL = WANTQ
            IF ( SPECQ )
     $         WANTQL = WHICHQ( I ).NE.0
            IF ( WANTQL ) THEN
               IQ = IXQ( I ) + ( J1 - 1 )*LDQ( I )
               CALL DLARFX( 'Right', N( I ), 2, DWORK( INDVF ),
     $                      DWORK( ITAUF ), Q( IQ ), LDQ( I ),
     $                      DWORK( W ) )
            END IF
         END IF
C
         ITAU1 = ITAU1 + 1
         ITAU2 = ITAU2 + 1
         ITAUF = ITAUF + 1
         INDV1 = INDV1 + 2
         INDV2 = INDV2 + 2
         INDVF = INDVF + 2
  120 CONTINUE
C
C     Set to zero the fill-in elements.
C
      DO 130 I = 1, K
         T( IWORK( I21+I )+1 ) = ZERO
         T( IWORK( I22+I )+1 ) = ZERO
         IF( I.NE.KSCHUR )
     $       T( IWORK( I21+I ) ) = ZERO
  130 CONTINUE
C
C     Exit direct swap N1 = 1 and N2 = 2.
C
      GOTO 290
C
C     Direct swap with N1 = 2 and N2 = 1.
C
  140 CONTINUE
C
C     Generate elementary reflectors H_i such that:
C
C     H_i( X_11_i ) = ( * ).
C        ( X_21_i )   ( 0 )
C        (  scale )   ( 0 )
C
      INDXC = C
      INDXV = V
C
      DO 150 INDTAU = TAU, TAU + K - 1
         X_11 = DWORK( INDXC )
         X_21 = DWORK( INDXC+1 )
         DWORK( INDXV   ) = X_11
         DWORK( INDXV+1 ) = X_21
         DWORK( INDXV+2 ) = SCALOC
C
         CALL DLARFG( 3, DWORK( INDXV ), DWORK( INDXV+1 ), 1,
     $                DWORK( INDTAU ) )
         DWORK( INDXV ) = ONE
C
C        Next, do weak stability test: check that
C        ||H_11_i - X_i * H_21_i||_F <= tol, i = 1, ..., K.
C
         V_2    = DWORK( INDXV+2 )
         TAULOC = DWORK( INDTAU )
         TMP1 = SCALOC * ( ONE - TAULOC ) + TAULOC * V_2 * X_11
         TMP2 = TAULOC * ( V_2 * X_21 - SCALOC * DWORK( INDXV+1 ) )
         IF( DLAPY2( TMP1, TMP2 ).GT.THRESH )
     $      GO TO 300
         INDXC = INDXC + 2
         INDXV = INDXV + 3
  150 CONTINUE
C
C     The swap passed weak stability test - move on and perform the
C     swapping temporarily into TT (workspace).
C
      INDTAU = TAU
      INDXV  = V
      INDTT  = TT
      LTT    = 3
C
      DO 160 I  = 1, K
         IP1    = MOD( I, K )
         INDVP1 = V   + IP1 * 3
         ITAUP1 = TAU + IP1
         CALL DLACPY( 'All', 3, 3, T( IWORK( I11+I ) ), LDT( I ),
     $                DWORK( INDTT ), 3 )
C
         IF( S( I ).EQ.1 ) THEN
            CALL DLARFX( 'Left', 3, 3, DWORK( INDVP1 ), DWORK( ITAUP1 ),
     $                   DWORK( INDTT ), 3, DWORK( W ) )
            CALL DLARFX( 'Right', 3, 3, DWORK( INDXV ), DWORK( INDTAU ),
     $                   DWORK( INDTT ), 3, DWORK( W ) )
         ELSE
            CALL DLARFX( 'Right', 3, 3, DWORK( INDVP1 ),
     $                   DWORK( ITAUP1 ), DWORK( INDTT ), 3,
     $                   DWORK( W ) )
            CALL DLARFX( 'Left', 3, 3, DWORK( INDXV ),
     $                   DWORK( INDTAU ), DWORK( INDTT ), 3,
     $                   DWORK( W ) )
         END IF
         INDTAU = INDTAU + 1
         INDXV  = INDXV  + 3
         INDTT  = INDTT  + 9
  160 CONTINUE
C
C     Check for fill-in elements.
C
      FILLIN = .FALSE.
      INDTT  = TT + 5
      DO 170 I = 1, K
         IF( I.NE.KSCHUR .AND. ABS( DWORK( INDTT ) ).GT.THRESH )
     $      FILLIN = .TRUE.
         INDTT = INDTT + 9
  170 CONTINUE
C
C     Found fill-in elements?
C
      IF( FILLIN ) THEN
C
C        Restore periodic Schur form.
C
         CALL MB03KC( K, KSCHUR, LTT, 2, S, DWORK( TT ), LTT,
     $                DWORK( VLOC ), DWORK( LTAU ) )
      END IF
C
      IF( WS ) THEN
C
C        Perform strong stability test.
C
         INDTAU = TAU
         INDXV  = V
         ITAUF  = LTAU
         INDVF  = VLOC
         INDTT  = TT
C
         DO 180 I = 1, K
            IP1 = MOD( I, K )
            CALL DLACPY( 'All', 3, 3, DWORK( INDTT ), 3, TEMPM1, 3 )
            IF( FILLIN ) THEN
               INDVP1 = VLOC + IP1 * 2
               ITAUP1 = LTAU + IP1
               IF( S( I ).EQ.1 ) THEN
                  CALL DLARFX( 'Left', 2, 2, DWORK( INDVP1 ),
     $                         DWORK( ITAUP1 ), TEMPM1( 5 ), 3,
     $                         DWORK( W ) )
                  CALL DLARFX( 'Right', 3, 2, DWORK( INDVF ),
     $                         DWORK( ITAUF ), TEMPM1( 4 ), 3,
     $                         DWORK( W ) )
               ELSE
                  CALL DLARFX( 'Right', 3, 2, DWORK( INDVP1 ),
     $                         DWORK( ITAUP1 ), TEMPM1( 4 ), 3,
     $                         DWORK( W ) )
                  CALL DLARFX( 'Left', 2, 2, DWORK( INDVF ),
     $                         DWORK( ITAUF ), TEMPM1( 5 ), 3,
     $                         DWORK( W ) )
               END IF
            END IF
C
            INDVP1 = V   + IP1 * 3
            ITAUP1 = TAU + IP1
            IF( S( I ).EQ.1 ) THEN
               CALL DLARFX( 'Left', 3, 3, DWORK( INDVP1 ),
     $                      DWORK( ITAUP1 ), TEMPM1, 3, DWORK( W ) )
               CALL DLARFX( 'Right', 3, 3, DWORK( INDXV ),
     $                      DWORK( INDTAU ), TEMPM1, 3, DWORK( W ) )
            ELSE
               CALL DLARFX( 'Right', 3, 3, DWORK( INDVP1 ),
     $                      DWORK( ITAUP1 ), TEMPM1, 3, DWORK( W ) )
               CALL DLARFX( 'Left', 3, 3, DWORK( INDXV ),
     $                      DWORK( INDTAU ), TEMPM1, 3, DWORK( W ) )
            END IF
            CALL DLACPY( 'All', 3, 3, T( IWORK( I11+I ) ), LDT( I ),
     $                   TEMP, 3 )
            CALL DAXPY( ND2, -ONE, TEMP, 1, TEMPM1, 1 )
            STRONG = DLANGE( 'Frobenius', 3, 3, TEMPM1, 3, DWORK )
            IF( STRONG.GT.THRESH )
     $         GO TO 300
C
            INDTAU = INDTAU + 1
            INDXV  = INDXV  + 3
            ITAUF  = ITAUF  + 1
            INDVF  = INDVF  + 2
            INDTT  = INDTT  + 9
  180    CONTINUE
C
      END IF
C
C     The swap was accepted - now update T and Q with respect to the
C     swapping.
C
      INDTAU = TAU
      INDXV  = V
      ITAUF  = LTAU
      INDVF  = VLOC
C
      DO 190 I = 1, K
         IP1 = MOD( I, K )
         IT  = IWORK( I11+I ) - J1 + 1
C
C        Apply Householder transformations from left or right depending
C        on S and accumulate transformations in matrices Q_i, i = 1:K.
C
         INDVP1 = V   + IP1 * 3
         ITAUP1 = TAU + IP1
         IP1    = IP1 + 1
         IF( S( I ).EQ.1 ) THEN
            IT  = IT - NI( IP1 )
            CALL DLARFX( 'Left', 3, N( I )-J1+1, DWORK( INDVP1 ),
     $                  DWORK( ITAUP1 ), T( IWORK( I11+I ) ), LDT( I ),
     $                  DWORK( W ) )
            CALL DLARFX( 'Right', NI( IP1 )+J3, 3, DWORK( INDXV ),
     $                   DWORK( INDTAU ), T( IT ), LDT( I ), DWORK( W )
     $                 )
         ELSE
            IT  = IT - NI( I )
            CALL DLARFX( 'Left', 3, N( IP1 )-J1+1, DWORK( INDXV ),
     $                  DWORK( INDTAU ), T( IWORK( I11+I ) ), LDT( I ),
     $                  DWORK( W ) )
            CALL DLARFX( 'Right', NI( I )+J3, 3, DWORK( INDVP1 ),
     $                   DWORK( ITAUP1 ), T( IT ), LDT( I ), DWORK( W )
     $                 )
         END IF
         WANTQL = WANTQ
         IF ( SPECQ )
     $      WANTQL = WHICHQ( I ).NE.0
         IF ( WANTQL ) THEN
            IQ = IXQ( I ) + ( J1 - 1 )*LDQ( I )
            CALL DLARFX( 'Right', N(I), 3, DWORK( INDXV ),
     $                   DWORK( INDTAU ), Q( IQ ), LDQ( I ), DWORK( W )
     $                 )
         END IF
C
C        Apply Householder transformations from fill-in removal and
C        accumulate transformations in matrices Q_i, i=1,...,K.
C
         IF ( FILLIN ) THEN
            INDVP1 = VLOC + ( IP1 - 1 ) * 2
            ITAUP1 = LTAU +   IP1 - 1
            IT2    = IT   + LDT( I )
            IF( S( I ).EQ.1 ) THEN
               CALL DLARFX( 'Left', 2, N( I )-J1, DWORK( INDVP1 ),
     $                      DWORK( ITAUP1 ), T( IT2+J1 ), LDT( I ),
     $                      DWORK( W ) )
               CALL DLARFX( 'Right', NI( IP1 )+J3, 2, DWORK( INDVF ),
     $                      DWORK( ITAUF ), T( IT2 ), LDT( I ),
     $                      DWORK( W ) )
            ELSE
               CALL DLARFX( 'Left', 2, N( IP1 )-J1, DWORK( INDVF ),
     $                      DWORK( ITAUF ), T( IT2+J1 ), LDT( I ),
     $                      DWORK( W ) )
               CALL DLARFX( 'Right', NI( I )+J3, 2, DWORK( INDVP1 ),
     $                      DWORK( ITAUP1 ), T( IT2 ), LDT( I ),
     $                      DWORK( W ) )
            END IF
            WANTQL = WANTQ
            IF ( SPECQ )
     $         WANTQL = WHICHQ( I ).NE.0
            IF ( WANTQL ) THEN
               IQ = IXQ( I ) + J1*LDQ( I )
               CALL DLARFX( 'Right', N( I ), 2, DWORK( INDVF ),
     $                      DWORK( ITAUF ), Q( IQ ), LDQ( I ),
     $                      DWORK( W ) )
            END IF
         END IF
         INDTAU = INDTAU + 1
         INDXV  = INDXV  + 3
         ITAUF  = ITAUF  + 1
         INDVF  = INDVF  + 2
  190 CONTINUE
C
C     Set to zero the fill-in elements below the main diagonal.
C
      DO 200 I = 1, K
         IT = IWORK( I11+I ) + 1
         T( IT   ) = ZERO
         T( IT+1 ) = ZERO
         IF( I.NE.KSCHUR )
     $      T( IT+LDT( I )+1 ) = ZERO
  200 CONTINUE
C
C     Exit direct swap N1 = 2 and N2 = 1.
C
      GOTO 290
C
C     Direct swap with N1 = 2 and N2 = 2.
C
  210 CONTINUE
C
C     Generate elementary reflectors H(1)_i and H(2)_i such that
C
C     H(2)_i H(1)_i  ( X_11_i X_12_i ) = ( * * ).
C                    ( X_21_i X_22_i )   ( 0 * )
C                    (  scale    0   )   ( 0 0 )
C                    (   0     scale )   ( 0 0 )
C
      INDXC = C
      ITAU2 = TAU2
      INDV1 = V1
      INDV2 = V2
C
      DO 220 ITAU1 = TAU1, TAU1 + K - 1
         X_11  = DWORK( INDXC   )
         X_21  = DWORK( INDXC+1 )
         X_12  = DWORK( INDXC+2 )
         X_22  = DWORK( INDXC+3 )
C
C        Compute elementary reflector H(1)_i.
C
         DWORK( INDV1   ) = X_11
         DWORK( INDV1+1 ) = X_21
         DWORK( INDV1+2 ) = SCALOC
         CALL DLARFG( 3, DWORK( INDV1 ), DWORK( INDV1+1 ), 1,
     $                DWORK( ITAU1 ) )
         DWORK( INDV1 ) = ONE
C
C        Compute elementary reflector H(2)_i.
C
         DWORK( INDV2   ) = X_12
         DWORK( INDV2+1 ) = X_22
         DWORK( INDV2+2 ) = ZERO
         CALL DLARFX( 'Left', 3, 1, DWORK( INDV1 ), DWORK( ITAU1 ),
     $                DWORK( INDV2 ), 3, DWORK( W ) )
         DWORK( INDV2   ) = DWORK( INDV2+1 )
         DWORK( INDV2+1 ) = DWORK( INDV2+2 )
         DWORK( INDV2+2 ) = SCALOC
         CALL DLARFG( 3, DWORK( INDV2 ), DWORK( INDV2+1 ), 1,
     $                DWORK( ITAU2 ) )
         DWORK( INDV2 ) = ONE
C
C        Next, do weak stability test: check that
C        ||QQ_11_i - X_i * QQ_21_i||_F <= tol, i = 1, ...,K,
C        where QQ_i = H(1)_i * H(2)_i.
C
         V_2   = DWORK( INDV1+1 )
         V_3   = DWORK( INDV1+2 )
         W_2   = DWORK( INDV2+1 )
         W_3   = DWORK( INDV2+2 )
         DTAU1 = DWORK( ITAU1   )
         DTAU2 = DWORK( ITAU2   )
         TEMP( 1 ) = SCALOC*( ONE - DTAU1 ) + X_11*DTAU1*V_3
         TEMP( 3 ) = SCALOC*( DTAU2*W_2*DTAU1*V_3 -
     $                        DTAU1*V_2*( ONE - DTAU2 ) ) -
     $               X_11*( -DTAU1*V_2*V_3*( ONE - DTAU2 ) -
     $               ( ONE - DTAU1*V_3**2 )*DTAU2*W_2 ) + X_12*DTAU2*W_3
         TEMP( 2 ) = -SCALOC*DTAU1*V_2 + X_21*DTAU1*V_3
         TEMP( 4 ) =  SCALOC*( ( ONE - DTAU1*V_2**2 )*( ONE - DTAU2 ) +
     $               DTAU1*V_2*V_3*DTAU2*W_2 ) -
     $               X_21*( -DTAU1*V_2*V_3*( ONE - DTAU2 ) -
     $               ( ONE - DTAU1*V_3**2 )*DTAU2*W_2 ) + X_22*DTAU2*W_3
         IF( DLANGE( 'Frobenius', 2, 2, TEMP, 2, DWORK ).GT.THRESH )
     $       GO TO 300
         INDXC = INDXC + 4
         ITAU2 = ITAU2 + 1
         INDV1 = INDV1 + 3
         INDV2 = INDV2 + 3
  220 CONTINUE
C
C     The swap passed weak stability test - move on and perform the
C     swapping temporarily into TT (workspace).
C
      ITAU1 = TAU1
      ITAU2 = TAU2
      INDV1 = V1
      INDV2 = V2
      INDTT = TT
      LTT   = 4
C
      DO 230 I = 1, K
         IP1    = MOD( I, K )
         IV1P1  = V1   + IP1 * 3
         IV2P1  = V2   + IP1 * 3
         TAU1P1 = TAU1 + IP1
         TAU2P1 = TAU2 + IP1
         CALL DLACPY( 'All', 4, 4, T( IWORK( I11+I ) ), LDT( I ),
     $                DWORK( INDTT ), 4 )
C
         IF( S( I ).EQ.1 ) THEN
            CALL DLARFX( 'Left', 3, 4, DWORK( IV1P1 ), DWORK( TAU1P1 ),
     $                   DWORK( INDTT ), 4, DWORK( W ) )
            CALL DLARFX( 'Left', 3, 4, DWORK( IV2P1 ), DWORK( TAU2P1 ),
     $                   DWORK( INDTT+1 ), 4, DWORK( W ) )
            CALL DLARFX( 'Right', 4, 3, DWORK( INDV1 ), DWORK( ITAU1 ),
     $                   DWORK( INDTT ), 4, DWORK( W ) )
            CALL DLARFX( 'Right', 4, 3, DWORK( INDV2 ), DWORK( ITAU2 ),
     $                   DWORK( INDTT+4 ), 4, DWORK( W ) )
         ELSE
            CALL DLARFX( 'Right', 4, 3, DWORK( IV1P1 ), DWORK( TAU1P1 ),
     $                   DWORK( INDTT ), 4, DWORK( W ) )
            CALL DLARFX( 'Right', 4, 3, DWORK( IV2P1 ), DWORK( TAU2P1 ),
     $                   DWORK( INDTT+4 ), 4, DWORK( W ) )
            CALL DLARFX( 'Left', 3, 4, DWORK( INDV1 ), DWORK( ITAU1 ),
     $                   DWORK( INDTT ), 4, DWORK( W ) )
            CALL DLARFX( 'Left', 3, 4, DWORK( INDV2 ), DWORK( ITAU2 ),
     $                   DWORK( INDTT+1), 4, DWORK( W ) )
         END IF
         ITAU1 = ITAU1 + 1
         ITAU2 = ITAU2 + 1
         INDV1 = INDV1 + 3
         INDV2 = INDV2 + 3
         INDTT = INDTT + 16
  230 CONTINUE
C
C     Check for fill-in elements.
C
      FILLIN = .FALSE.
      FILL21 = .FALSE.
      INDTT  = TT + 1
      DO 240 I = 1, K
         IF( I.NE.KSCHUR .AND. ABS( DWORK( INDTT ) ).GT.THRESH ) THEN
            FILLIN = .TRUE.
            FILL21 = .TRUE.
         END IF
         INDTT = INDTT + 16
  240 CONTINUE
C
C     Found fill-in elements?
C
      IF( FILLIN ) THEN
C
C        Restore periodic Schur form.
C
         CALL MB03KC( K, KSCHUR, LTT, 1, S, DWORK( TT ), LTT,
     $                DWORK( VLOC1 ), DWORK( LTAU1 ) )
      END IF
C
C     Check for fill-in elements again.
C
      FILLIN = .FALSE.
      FILL43 = .FALSE.
      INDTT  = TT + 11
      DO 250 I = 1, K
         IF( I.NE.KSCHUR .AND. ABS( DWORK( INDTT ) ).GT.EPS ) THEN
            FILLIN = .TRUE.
            FILL43 = .TRUE.
         END IF
         INDTT = INDTT + 16
  250 CONTINUE
C
C     Found fill-in elements?
C
      IF( FILLIN ) THEN
C
C        Restore periodic Schur form.
C
         CALL MB03KC( K, KSCHUR, LTT, 3, S, DWORK( TT ), LTT,
     $                DWORK( VLOC2 ), DWORK( LTAU2 ) )
      END IF
C
      IF( WS ) THEN
C
C        Perform strong stability test.
C
         ITAU1 = TAU1
         ITAU2 = TAU2
         INDV1 = V1
         INDV2 = V2
         INDTT = TT
         IF( FILLIN ) THEN
            ITAUF1 = LTAU1
            ITAUF2 = LTAU2
            INDF1  = VLOC1
            INDF2  = VLOC2
         END IF
C
         DO 260 I = 1, K
            IP1 = MOD( I, K )
            CALL DLACPY( 'All', 4, 4, DWORK( INDTT ), 4, TEMPM1, 4 )
C
C           Apply possible transformations from fill-in removal.
C
            IF( FILLIN ) THEN
               IV1P1  = VLOC1 + IP1 * 2
               IV2P1  = VLOC2 + IP1 * 2
               TAU1P1 = LTAU1 + IP1
               TAU2P1 = LTAU2 + IP1
C
C              Apply on top-left 2-by-2 block.
C
               IF( FILL21 ) THEN
                  IF( S( I ).EQ.1 ) THEN
                     CALL DLARFX( 'Left', 2, 4, DWORK( IV1P1 ),
     $                            DWORK( TAU1P1 ), TEMPM1, 4, DWORK( W )
     $                          )
                     CALL DLARFX( 'Right', 2, 2, DWORK( INDF1 ),
     $                            DWORK( ITAUF1 ), TEMPM1, 4, DWORK( W )
     $                          )
                  ELSE
                     CALL DLARFX( 'Right', 2, 2, DWORK( IV1P1 ),
     $                            DWORK( TAU1P1 ), TEMPM1, 4, DWORK( W )
     $                          )
                     CALL DLARFX( 'Left', 2, 4, DWORK( INDF1 ),
     $                            DWORK( ITAUF1 ), TEMPM1, 4, DWORK( W )
     $                          )
                  END IF
               END IF
C
C              Apply on down-right 2-by-2 block.
C
               IF( FILL43 ) THEN
                  IF( S( I ).EQ.1 ) THEN
                     CALL DLARFX( 'Left', 2, 2, DWORK( IV2P1 ),
     $                            DWORK( TAU2P1 ), TEMPM1( 11 ), 4,
     $                            DWORK( W ) )
                     CALL DLARFX( 'Right', 4, 2, DWORK( INDF2 ),
     $                            DWORK( ITAUF2 ), TEMPM1( 9 ), 4,
     $                            DWORK( W ) )
                  ELSE
                     CALL DLARFX( 'Right', 4, 2, DWORK( IV2P1 ),
     $                            DWORK( TAU2P1 ), TEMPM1( 9 ), 4,
     $                            DWORK( W ) )
                     CALL DLARFX( 'Left', 2, 2, DWORK( INDF2 ),
     $                            DWORK( ITAUF2 ), TEMPM1( 11 ), 4,
     $                            DWORK( W ) )
                  END IF
               END IF
            END IF
C
C           Take the "large" transformations.
C
            IV1P1  = V1   + IP1 * 3
            IV2P1  = V2   + IP1 * 3
            TAU1P1 = TAU1 + IP1
            TAU2P1 = TAU2 + IP1
C
C           Apply H(2)_i+1, H(1)_i+1, H(2)_i, H(1)_i from left or right
C           depending on S.
C
            IF( S( I ).EQ.1 ) THEN
               CALL DLARFX( 'Left', 3, 4, DWORK( IV2P1 ),
     $                      DWORK( TAU2P1 ), TEMPM1( 2 ), 4, DWORK( W )
     $                    )
               CALL DLARFX( 'Left', 3, 4, DWORK( IV1P1 ),
     $                      DWORK( TAU1P1 ), TEMPM1, 4, DWORK( W ) )
               CALL DLARFX( 'Right', 4, 3, DWORK( INDV2 ),
     $                      DWORK( ITAU2 ), TEMPM1( 5 ), 4, DWORK( W ) )
               CALL DLARFX( 'Right', 4, 3, DWORK( INDV1 ),
     $                      DWORK( ITAU1 ), TEMPM1, 4, DWORK( W ) )
            ELSE
               CALL DLARFX( 'Right', 4, 3, DWORK( IV2P1 ),
     $                      DWORK( TAU2P1 ), TEMPM1( 5 ), 4, DWORK( W )
     $                    )
               CALL DLARFX( 'Right', 4, 3, DWORK( IV1P1 ),
     $                      DWORK( TAU1P1 ), TEMPM1, 4, DWORK( W ) )
               CALL DLARFX( 'Left', 3, 4, DWORK( INDV2 ),
     $                      DWORK( ITAU2 ), TEMPM1( 2 ), 4, DWORK( W ) )
               CALL DLARFX( 'Left', 3, 4, DWORK( INDV1 ),
     $                      DWORK( ITAU1 ), TEMPM1, 4, DWORK( W ) )
            END IF
C
C           Compute residual norm.
C
            CALL DLACPY( 'All', 4, 4, T( IWORK( I11+I ) ), LDT( I ),
     $                   TEMP, 4 )
            CALL DAXPY( ND2, -ONE, TEMP, 1, TEMPM1, 1 )
            STRONG = DLANGE( 'Frobenius', 4, 4, TEMPM1, 4, DWORK )
            IF( STRONG.GT.THRESH )
     $         GO TO 300
C
            ITAU1 = ITAU1 + 1
            ITAU2 = ITAU2 + 1
            INDV1 = INDV1 + 3
            INDV2 = INDV2 + 3
            INDTT = INDTT + 16
            IF( FILLIN ) THEN
               ITAUF1 = ITAUF1 + 1
               ITAUF2 = ITAUF2 + 1
               INDF1  = INDF1  + 2
               INDF2  = INDF2  + 2
            END IF
  260    CONTINUE
C
      END IF
C
C     The swap was accepted - now update T and Q with respect to the
C     swapping.
C
      ITAU1 = TAU1
      ITAU2 = TAU2
      INDV1 = V1
      INDV2 = V2
      IF( FILLIN ) THEN
         ITAUF1 = LTAU1
         ITAUF2 = LTAU2
         INDF1  = VLOC1
         INDF2  = VLOC2
      END IF
C
      DO 270 I = 1, K
         IP1 = MOD( I, K )
         IPP = IP1 + 1
         IT  = IWORK( I11+I ) - J1 + 1
C
C        Apply Householder transformations from left or right depending
C        on S and accumulate transformations in matrices Q_i, i = 1:K.
C
         IV1P1  = V1   + IP1 * 3
         IV2P1  = V2   + IP1 * 3
         TAU1P1 = TAU1 + IP1
         TAU2P1 = TAU2 + IP1
C
         IF( S( I ).EQ.1 ) THEN
            IT  = IT - NI( IPP )
            IT2 = IT + LDT( I )
            CALL DLARFX( 'Left', 3, N( I )-J1+1, DWORK( IV1P1 ),
     $                   DWORK( TAU1P1 ), T( IWORK( I11+I ) ), LDT( I ),
     $                   DWORK( W ) )
            CALL DLARFX( 'Left', 3, N( I )-J1+1, DWORK( IV2P1 ),
     $                   DWORK( TAU2P1 ), T( IWORK( I11+I )+1 ),
     $                   LDT( I ), DWORK( W ) )
            CALL DLARFX( 'Right', NI( IPP )+J4, 3, DWORK( INDV1 ),
     $                   DWORK( ITAU1 ), T( IT ), LDT( I ), DWORK( W ) )
            CALL DLARFX( 'Right', NI( IPP )+J4, 3, DWORK( INDV2 ),
     $                   DWORK( ITAU2 ), T( IT2 ), LDT( I ), DWORK( W )
     $                 )
         ELSE
            IT  = IT - NI( I )
            IT2 = IT + LDT( I )
            CALL DLARFX( 'Right', NI( I )+J4, 3, DWORK( IV1P1 ),
     $                   DWORK( TAU1P1 ), T( IT ), LDT( I ), DWORK( W )
     $                 )
            CALL DLARFX( 'Right', NI( I )+J4, 3, DWORK( IV2P1 ),
     $                   DWORK( TAU2P1 ), T( IT2 ), LDT( I ), DWORK( W )
     $                 )
            CALL DLARFX( 'Left', 3, N( IPP )-J1+1, DWORK( INDV1 ),
     $                   DWORK( ITAU1 ), T( IWORK( I11+I ) ), LDT( I ),
     $                   DWORK( W ) )
            CALL DLARFX( 'Left', 3, N( IPP )-J1+1, DWORK( INDV2 ),
     $                   DWORK( ITAU2 ), T( IWORK( I11+I )+1 ),
     $                   LDT( I ), DWORK( W ) )
         END IF
C
         WANTQL = WANTQ
         IF ( SPECQ )
     $      WANTQL = WHICHQ( I ).NE.0
         IF ( WANTQL ) THEN
            IQ = IXQ( I ) + ( J1 - 1 )*LDQ( I )
            CALL DLARFX( 'Right', N( I ), 3, DWORK( INDV1 ),
     $                   DWORK( ITAU1 ), Q( IQ ), LDQ( I ),
     $                   DWORK( W ) )
            IQ = IQ + LDQ( I )
            CALL DLARFX( 'Right', N( I ), 3, DWORK( INDV2 ),
     $                   DWORK( ITAU2 ), Q( IQ ), LDQ( I ),
     $                   DWORK( W ) )
         END IF
C
C        Apply Householder transformations from fill-in removal and
C        accumulate transformations.
C
         IF ( FILLIN ) THEN
            IV1P1  = VLOC1 + IP1 * 2
            IV2P1  = VLOC2 + IP1 * 2
            TAU1P1 = LTAU1 + IP1
            TAU2P1 = LTAU2 + IP1
C
            IF( FILL21 ) THEN
               IF( S( I ).EQ.1 ) THEN
                  CALL DLARFX( 'Left', 2, N( I )-J1+1, DWORK( IV1P1 ),
     $                         DWORK( TAU1P1 ), T( IWORK( I11+I ) ),
     $                         LDT( I ), DWORK( W ) )
                  CALL DLARFX( 'Right', NI( IPP )+J2, 2, DWORK( INDF1 ),
     $                         DWORK( ITAUF1 ), T( IT ), LDT( I ),
     $                         DWORK( W ) )
               ELSE
                  CALL DLARFX( 'Right', NI( I )+J2, 2, DWORK( IV1P1 ),
     $                         DWORK( TAU1P1 ), T( IT ), LDT( I ),
     $                         DWORK( W ) )
                  CALL DLARFX( 'Left', 2, N( IPP )-J1+1, DWORK( INDF1 ),
     $                         DWORK( ITAUF1 ), T( IWORK( I11+I ) ),
     $                         LDT( I ), DWORK( W ) )
               END IF
            END IF
C
            IF( FILL43 ) THEN
               IT  = IWORK( I22+I )
               IT2 = IT2 + LDT( I )
               IF( S( I ).EQ.1 ) THEN
                  CALL DLARFX( 'Left', 2, N( I )-J2, DWORK( IV2P1 ),
     $                         DWORK( TAU2P1 ), T( IT ), LDT( I ),
     $                         DWORK( W ) )
                  CALL DLARFX( 'Right', NI( IPP )+J4, 2, DWORK( INDF2 ),
     $                         DWORK( ITAUF2 ), T( IT2 ), LDT( I ),
     $                         DWORK( W ) )
               ELSE
                  CALL DLARFX( 'Right', NI( I )+J4, 2, DWORK( IV2P1 ),
     $                         DWORK( TAU2P1 ), T( IT2 ), LDT( I ),
     $                         DWORK( W ) )
                  CALL DLARFX( 'Left', 2, N( IPP )-J2, DWORK( INDF2 ),
     $                         DWORK( ITAUF2 ), T( IT ), LDT( I ),
     $                         DWORK( W ) )
               END IF
            END IF
C
            WANTQL = WANTQ
            IF ( SPECQ )
     $         WANTQL = WHICHQ( I ).NE.0
            IF ( WANTQL ) THEN
               IF( FILL21 ) THEN
                  IQ = IXQ( I ) + ( J1 - 1 )*LDQ( I )
                  CALL DLARFX( 'Right', N( I ), 2, DWORK( INDF1 ),
     $                         DWORK( ITAUF1 ), Q( IQ ), LDQ( I ),
     $                         DWORK( W ) )
               END IF
               IF( FILL43 ) THEN
                  IQ = IXQ( I ) + J2*LDQ( I )
                  CALL DLARFX( 'Right', N( I ), 2, DWORK( INDF2 ),
     $                         DWORK( ITAUF2 ), Q( IQ ), LDQ( I ),
     $                         DWORK( W ) )
               END IF
            END IF
         END IF
         ITAU1 = ITAU1 + 1
         ITAU2 = ITAU2 + 1
         INDV1 = INDV1 + 3
         INDV2 = INDV2 + 3
         IF( FILLIN ) THEN
            ITAUF1 = ITAUF1 + 1
            ITAUF2 = ITAUF2 + 1
            INDF1  = INDF1  + 2
            INDF2  = INDF2  + 2
         END IF
  270 CONTINUE
C
C     Set to zero the fill-in elements below the main diagonal.
C
      DO 280 I = 1, K
         IT = IWORK( I21+I )
         T( IT   ) = ZERO
         T( IT+1 ) = ZERO
         IT = IT + LDT( I )
         T( IT   ) = ZERO
         T( IT+1 ) = ZERO
         IF( I.NE.KSCHUR ) THEN
            T( IWORK( I11+I )+1 ) = ZERO
            T( IWORK( I22+I )+1 ) = ZERO
         END IF
  280 CONTINUE
C
C     Exit direct swap N1 = 2 and N2 = 2.
C
C     Normal exit.
C
  290 CONTINUE
C
C     Store optimal workspace values and return.
C
      DWORK( 1 ) = DBLE( MINWRK )
      RETURN
C
C     Exit with INFO = 1 if swap was rejected.
C
  300 CONTINUE
      INFO = 1
      RETURN
C
C *** Last line of MB03KB ***
      END
