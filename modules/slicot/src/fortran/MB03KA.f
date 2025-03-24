      SUBROUTINE MB03KA( COMPQ, WHICHQ, WS, K, NC, KSCHUR, IFST, ILST,
     $                   N, NI, S, T, LDT, IXT, Q, LDQ, IXQ, TOL, IWORK,
     $                   DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To reorder the diagonal blocks of the formal matrix product
C
C        T22_K^S(K) * T22_K-1^S(K-1) * ... * T22_1^S(1),             (1)
C
C     of length K, in the generalized periodic Schur form
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
C     such that the block with starting row index IFST in (1) is moved
C     to row index ILST. The indices refer to the T22_k submatrices.
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
C             triangular. All other T22 matrices are upper triangular.
C
C     IFST    (input/output) INTEGER
C     ILST    (input/output) INTEGER
C             Specify the reordering of the diagonal blocks, as follows:
C             The block with starting row index IFST in (1) is moved to
C             row index ILST by a sequence of direct swaps between adjacent
C             blocks in the product.
C             On exit, if IFST pointed on entry to the second row of a
C             2-by-2 block in the product, it is changed to point to the
C             first row; ILST always points to the first row of the block
C             in its final position in the product (which may differ from
C             its input value by +1 or -1).
C             1 <= IFST <= NC, 1 <= ILST <= NC.
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
C             dimensional array Q.
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
C             NRM is the Frobenius norm of the current matrix formed by
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
C             LDWORK >= 10*K + MN, if all blocks between IFST and ILST
C                                  have order 1;
C             LDWORK >= 25*K + MN, if there is at least a block of
C                                  order 2, but no adjacent blocks of
C                                  order 2 can appear between IFST and
C                                  ILST during reordering;
C             LDWORK >= MAX(42*K + MN, 80*K - 48), if at least a pair of
C                                  adjacent blocks of order 2 can appear
C                                  between IFST and ILST during
C                                  reordering;
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
C             < 0:  if INFO = -21, the LDWORK argument was too small;
C             = 1:  the reordering of T failed because some eigenvalues
C                   are too close to separate (the problem is very ill-
C                   conditioned); T may have been partially reordered.
C                   The returned value of ILST is the index where this
C                   was detected.
C
C     METHOD
C
C     An adaptation of the LAPACK Library routine DTGEXC is used.
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
C     Mar. 2010, SLICOT Library version of the PEP routine PEP_DTGEXC.
C     V. Sima, July 2010.
C
C     KEYWORDS
C
C     Orthogonal transformation, periodic QZ algorithm, periodic
C     Sylvester-like equations, QZ algorithm.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
C     ..
C     .. Scalar Arguments ..
      CHARACTER          COMPQ
      LOGICAL            WS
      INTEGER            IFST, ILST, INFO, K, KSCHUR, LDWORK, NC
C     ..
C     .. Array Arguments ..
      INTEGER            IWORK( * ), IXQ( * ), IXT( * ), LDQ( * ),
     $                   LDT( * ), N( * ), NI( * ), S( * ), WHICHQ( * )
      DOUBLE PRECISION   DWORK( * ), Q( * ), T( * ), TOL( * )
C     ..
C     .. Local Scalars ..
      INTEGER            HERE, I, IP1, IT, MINWRK, NBF, NBL, NBNEXT
C     ..
C     .. External Subroutines ..
      EXTERNAL           MB03KB, XERBLA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          DBLE, INT, MAX, MOD
C     ..
C     .. Executable Statements ..
C
C     For efficiency reasons the parameters are not checked, except for
C     workspace.
C
      IF( NC.EQ.2 ) THEN
         NBF = 1
         NBL = 1
      ELSE IF( NC.EQ.3 ) THEN
         NBF = 1
         NBL = 2
      ELSE
         NBF = 2
         NBL = 2
      END IF
      CALL MB03KB( COMPQ, WHICHQ, WS, K, NC, KSCHUR, 1, NBF, NBL, N, NI,
     $             S, T, LDT, IXT, Q, LDQ, IXQ, TOL, IWORK, DWORK, -1,
     $             INFO )
      MINWRK = MAX( 1, INT( DWORK(1) ) )
      IF( LDWORK.NE.-1 .AND. LDWORK.LT.MINWRK )
     $   INFO = -21
C
C     Quick return if possible
C
      IF( LDWORK.EQ.-1 ) THEN
         DWORK(1) = DBLE( MINWRK )
         RETURN
      ELSE IF( INFO.LT.0 ) THEN
         CALL XERBLA( 'MB03KA', -INFO )
         RETURN
      END IF
C
C     Set I and IP1 to point to KSCHUR and KSCHUR+1 to simplify
C     indices below.
C
      I   = KSCHUR
      IP1 = MOD( I, K ) + 1
C
C     Determine the first row of the block in T22_kschur corresponding
C     to the first block in the product and find out if it is 1-by-1 or
C     2-by-2.
C
      IF( IFST.GT.1 ) THEN
         IF( S(I).EQ.1 ) THEN
            IT = IXT(I) + ( NI(I) + IFST - 2 )*LDT(I) + NI(IP1) + IFST
     $                  - 1
         ELSE
            IT = IXT(I) + ( NI(IP1) + IFST - 2 )*LDT(I) + NI(I) + IFST
     $                  - 1
         END IF
         IF( T( IT ).NE.ZERO )
     $      IFST = IFST - 1
      END IF
      NBF = 1
      IF( IFST.LT.NC ) THEN
         IF( S(I).EQ.1 ) THEN
            IT = IXT(I) + ( NI(I) + IFST - 1 )*LDT(I) + NI(IP1) + IFST
         ELSE
            IT = IXT(I) + ( NI(IP1) + IFST - 1 )*LDT(I) + NI(I) + IFST
         END IF
         IF( T( IT ).NE.ZERO )
     $      NBF = 2
      END IF
C
C     Determine the first row of the block in T_kschur corresponding
C     to the last block in the product and find out it is 1-by-1 or
C     2-by-2.
C
      IF( ILST.GT.1 ) THEN
         IF( S(I).EQ.1 ) THEN
            IT = IXT(I) + ( NI(I) + ILST - 2 )*LDT(I) + NI(IP1) + ILST
     $                  - 1
         ELSE
            IT = IXT(I) + ( NI(IP1) + ILST - 2 )*LDT(I) + NI(I) + ILST
     $                  - 1
         END IF
         IF( T( IT ).NE.ZERO )
     $      ILST = ILST - 1
      END IF
      NBL = 1
      IF( ILST.LT.NC ) THEN
         IF( S(I).EQ.1 ) THEN
            IT = IXT(I) + ( NI(I) + ILST - 1 )*LDT(I) + NI(IP1) + ILST
         ELSE
            IT = IXT(I) + ( NI(IP1) + ILST - 1 )*LDT(I) + NI(I) + ILST
         END IF
         IF( T( IT ).NE.ZERO )
     $      NBL = 2
      END IF
C
C     If the specified and last block in the product were the same,
C     return.
C
      IF( IFST.EQ.ILST )
     $   RETURN
C
C     If the specified block lies above the last block on the diagonal
C     of the product and the blocks have unequal sizes, update ILST.
C
      IF( IFST.LT.ILST ) THEN
C
C        Update ILST.
C
         IF( NBF.EQ.2 .AND. NBL.EQ.1 )
     $      ILST = ILST - 1
         IF( NBF.EQ.1 .AND. NBL.EQ.2 )
     $      ILST = ILST + 1
C
         HERE = IFST
C
   10    CONTINUE
C
C        Swap a block with next one below.
C
         IF( NBF.EQ.1 .OR. NBF.EQ.2 ) THEN
C
C           Current next block is either 1-by-1 or 2-by-2.
C
            NBNEXT = 1
            IF( HERE+NBF+1.LE.NC ) THEN
               IF( S(I).EQ.1 ) THEN
                  IT = IXT(I) + ( NI(I) + HERE + NBF - 1 )*LDT(I) +
     $                          NI(IP1) + HERE + NBF
               ELSE
                  IT = IXT(I) + ( NI(IP1) + HERE + NBF - 1 )*LDT(I) +
     $                              NI(I) + HERE + NBF
               END IF
               IF( T( IT ).NE.ZERO )
     $            NBNEXT = 2
            END IF
            CALL MB03KB( COMPQ, WHICHQ, WS, K, NC, KSCHUR, HERE, NBF,
     $                   NBNEXT, N, NI, S, T, LDT, IXT, Q, LDQ, IXQ,
     $                   TOL, IWORK, DWORK, LDWORK, INFO )
            IF( INFO.NE.0 ) THEN
               ILST = HERE
               RETURN
            END IF
            HERE = HERE + NBNEXT
C
C           Test if a 2-by-2 block breaks into two 1-by-1 blocks.
C
            IF( NBF.EQ.2 ) THEN
               IF( S(I).EQ.1 ) THEN
                  IT = IXT(I) + ( NI(I) + HERE - 1 )*LDT(I) + NI(IP1)
     $                        + HERE
               ELSE
                  IT = IXT(I) + ( NI(IP1) + HERE - 1 )*LDT(I) + NI(I)
     $                        + HERE
               END IF
               IF( T( IT ).EQ.ZERO )
     $            NBF = 3
            END IF
         ELSE
C
C           Current next block consists of two 1-by-1 blocks each of
C           which must be swapped individually.
C
            NBNEXT = 1
            IF( HERE+3.LE.NC ) THEN
               IF( S(I).EQ.1 ) THEN
                  IT = IXT(I) + ( NI(I) + HERE + 1 )*LDT(I) + NI(IP1) +
     $                                    HERE + 2
               ELSE
                  IT = IXT(I) + ( NI(IP1) + HERE + 1 )*LDT(I) + NI(I) +
     $                                      HERE + 2
               END IF
               IF( T( IT ).NE.ZERO )
     $            NBNEXT = 2
            END IF
            CALL MB03KB( COMPQ, WHICHQ, WS, K, NC, KSCHUR, HERE+1, 1,
     $                   NBNEXT, N, NI, S, T, LDT, IXT, Q, LDQ, IXQ,
     $                   TOL, IWORK, DWORK, LDWORK, INFO )
            IF( INFO.NE.0 ) THEN
               ILST = HERE
               RETURN
            END IF
            IF( NBNEXT.EQ.1 ) THEN
C
C              Swap two 1-by-1 blocks.
C
               CALL MB03KB( COMPQ, WHICHQ, WS, K, NC, KSCHUR, HERE, 1,
     $                      NBNEXT, N, NI, S, T, LDT, IXT, Q, LDQ, IXQ,
     $                      TOL, IWORK, DWORK, LDWORK, INFO )
               IF( INFO.NE.0 ) THEN
                  ILST = HERE
                  RETURN
               END IF
               HERE = HERE + 1
            ELSE
C
C              Recompute NBNEXT in case 2-by-2 split.
C
               IF( S(I).EQ.1 ) THEN
                  IT = IXT(I) + ( NI(I) + HERE )*LDT(I) + NI(IP1) + HERE
     $                        + 1
               ELSE
                  IT = IXT(I) + ( NI(IP1) + HERE )*LDT(I) + NI(I) + HERE
     $                                    + 1
               END IF
               IF( T( IT ).EQ.ZERO )
     $            NBNEXT = 1
               IF( NBNEXT.EQ.2 ) THEN
C
C                 The 2-by-2 block did not split.
C
                  CALL MB03KB( COMPQ, WHICHQ, WS, K, NC, KSCHUR, HERE,
     $                         1, NBNEXT, N, NI, S, T, LDT, IXT, Q, LDQ,
     $                         IXQ, TOL, IWORK, DWORK, LDWORK, INFO )
                  IF( INFO.NE.0 ) THEN
                     ILST = HERE
                     RETURN
                  END IF
                  HERE = HERE + 2
               ELSE
C
C                 The 2-by-2 block did split.
C
                  CALL MB03KB( COMPQ, WHICHQ, WS, K, NC, KSCHUR, HERE,
     $                         1, 1, N, NI, S, T, LDT, IXT, Q, LDQ, IXQ,
     $                         TOL, IWORK, DWORK, LDWORK, INFO )
                  IF( INFO.NE.0 ) THEN
                     ILST = HERE
                     RETURN
                  END IF
                  CALL MB03KB( COMPQ, WHICHQ, WS, K, NC, KSCHUR, HERE+1,
     $                         1, 1, N, NI, S, T, LDT, IXT, Q, LDQ, IXQ,
     $                         TOL, IWORK, DWORK, LDWORK, INFO )
                  IF( INFO.NE.0 ) THEN
                     ILST = HERE + 1
                     RETURN
                  END IF
                  HERE = HERE + 2
               END IF
            END IF
         END IF
         IF( HERE.LT.ILST )
     $      GO TO 10
C
      ELSE
C
         HERE = IFST
   20    CONTINUE
C
C        Swap a block with next one above.
C
         IF( NBF.EQ.1 .OR. NBF.EQ.2 ) THEN
C
C           Current block is either 1-by-1 or 2-by-2.
C
            NBNEXT = 1
            IF( HERE.GE.3 ) THEN
               IF( S(I).EQ.1 ) THEN
                  IT = IXT(I) + ( NI(I) + HERE - 3 )*LDT(I) + NI(IP1)
     $                                  + HERE - 2
               ELSE
                  IT = IXT(I) + ( NI(IP1) + HERE - 3 )*LDT(I) + NI(I)
     $                                    + HERE - 2
               END IF
               IF( T( IT ).NE.ZERO )
     $            NBNEXT = 2
            END IF
            CALL MB03KB( COMPQ, WHICHQ, WS, K, NC, KSCHUR, HERE-NBNEXT,
     $                   NBNEXT, NBF, N, NI, S, T, LDT, IXT, Q, LDQ,
     $                   IXQ, TOL, IWORK, DWORK, LDWORK, INFO )
            IF( INFO.NE.0 ) THEN
               ILST = HERE
               RETURN
            END IF
            HERE = HERE - NBNEXT
C
C           Test if a 2-by-2 block breaks into two 1-by-1 blocks.
C
            IF( NBF.EQ.2 ) THEN
               IF( S(I).EQ.1 ) THEN
                  IT = IXT(I) + ( NI(I) + HERE - 1 )*LDT(I) + NI(IP1)
     $                                  + HERE
               ELSE
                  IT = IXT(I) + ( NI(IP1) + HERE - 1 )*LDT(I) + NI(I)
     $                                    + HERE
               END IF
               IF( T( IT ).EQ.ZERO )
     $            NBF = 3
            END IF
C
         ELSE
C
C           Current block consists of two 1-by-1 blocks each of which
C           must be swapped individually.
C
            NBNEXT = 1
            IF( HERE.GE.3 ) THEN
               IF( S(I).EQ.1 ) THEN
                  IT = IXT(I) + ( NI(I) + HERE - 3 )*LDT(I) + NI(IP1)
     $                                  + HERE - 2
               ELSE
                  IT = IXT(I) + ( NI(IP1) + HERE - 3 )*LDT(I) + NI(I)
     $                                    + HERE - 2
               END IF
               IF( T( IT ).NE.ZERO )
     $            NBNEXT = 2
            END IF
            CALL MB03KB( COMPQ, WHICHQ, WS, K, NC, KSCHUR, HERE-NBNEXT,
     $                   NBNEXT, 1, N, NI, S, T, LDT, IXT, Q, LDQ, IXQ,
     $                   TOL, IWORK, DWORK, LDWORK, INFO )
            IF( INFO.NE.0 ) THEN
               ILST = HERE
               RETURN
            END IF
            IF( NBNEXT.EQ.1 ) THEN
C
C              Swap two 1-by-1 blocks.
C
               CALL MB03KB( COMPQ, WHICHQ, WS, K, NC, KSCHUR, HERE,
     $                      NBNEXT, 1, N, NI, S, T, LDT, IXT, Q, LDQ,
     $                      IXQ, TOL, IWORK, DWORK, LDWORK, INFO )
               IF( INFO.NE.0 ) THEN
                  ILST = HERE
                  RETURN
               END IF
               HERE = HERE - 1
            ELSE
C
C              Recompute NBNEXT in case 2-by-2 split.
C
               IF( S(I).EQ.1 ) THEN
                  IT = IXT(I) + ( NI(I) + HERE - 2 )*LDT(I) + NI(IP1)
     $                                  + HERE - 1
               ELSE
                  IT = IXT(I) + ( NI(IP1) + HERE - 2 )*LDT(I) + NI(I)
     $                                    + HERE - 1
               END IF
               IF( T( IT ).EQ.ZERO )
     $            NBNEXT = 1
               IF( NBNEXT.EQ.2 ) THEN
C
C                 The 2-by-2 block did not split.
C
                  CALL MB03KB( COMPQ, WHICHQ, WS, K, NC, KSCHUR, HERE-1,
     $                         2, 1, N, NI, S, T, LDT, IXT, Q, LDQ, IXQ,
     $                         TOL, IWORK, DWORK, LDWORK, INFO )
                  IF( INFO.NE.0 ) THEN
                     ILST = HERE
                     RETURN
                  END IF
                  HERE = HERE - 2
               ELSE

C                 The 2-by-2 block did split.
C
                  CALL MB03KB( COMPQ, WHICHQ, WS, K, NC, KSCHUR, HERE,
     $                         1, 1, N, NI, S, T, LDT, IXT, Q, LDQ, IXQ,
     $                         TOL, IWORK, DWORK, LDWORK, INFO )
                  IF( INFO.NE.0 ) THEN
                     ILST = HERE
                     RETURN
                  END IF
                  CALL MB03KB( COMPQ, WHICHQ, WS, K, NC, KSCHUR, HERE-1,
     $                         1, 1, N, NI, S, T, LDT, IXT, Q, LDQ, IXQ,
     $                         TOL, IWORK, DWORK, LDWORK, INFO )
                  IF( INFO.NE.0 ) THEN
                     ILST = HERE - 1
                     RETURN
                  END IF
                  HERE = HERE - 2
               END IF
            END IF
         END IF
         IF( HERE.GT.ILST )
     $      GO TO 20
      END IF
      ILST = HERE
C
C     Store optimal workspace values and return.
C
      DWORK(1) = DBLE( MINWRK )
      RETURN
C
C *** Last line of MB03KA ***
      END
