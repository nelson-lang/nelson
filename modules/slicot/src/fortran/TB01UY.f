      SUBROUTINE TB01UY( JOBZ, N, M1, M2, P, A, LDA, B, LDB, C, LDC,
     $                   NCONT, INDCON, NBLK, Z, LDZ, TAU, TOL, IWORK,
     $                   DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To find a controllable realization for the linear time-invariant
C     multi-input system
C
C             dX/dt = A * X + B1 * U1 + B2 * U2,
C                Y  = C * X,
C
C     where A, B1, B2 and C are N-by-N, N-by-M1, N-by-M2, and P-by-N
C     matrices, respectively, and A and [B1,B2] are reduced by this
C     routine to orthogonal canonical form using (and optionally
C     accumulating) orthogonal similarity transformations, which are
C     also applied to C.  Specifically, the system (A, [B1,B2], C) is
C     reduced to the triplet (Ac, [Bc1,Bc2], Cc), where
C     Ac = Z' * A * Z, [Bc1,Bc2] = Z' * [B1,B2], Cc = C * Z,  with
C
C             [ Acont     *    ]                [ Bcont1, Bcont2 ]
C        Ac = [                ],   [Bc1,Bc1] = [                ],
C             [   0    Auncont ]                [   0        0   ]
C
C        and
C
C                [ A11 A12  . . .  A1,p-2 A1,p-1 A1p ]
C                [ A21 A22  . . .  A2,p-2 A2,p-1 A2p ]
C                [ A31 A32  . . .  A3,p-2 A3,p-1 A3p ]
C                [  0  A42  . . .  A4,p-2 A4,p-1 A4p ]
C        Acont = [  .   .   . . .    .      .     .  ],
C                [  .   .     . .    .      .     .  ]
C                [  .   .       .    .      .     .  ]
C                [  0   0   . . .  Ap,p-2 Ap,p-1 App ]
C
C                    [ B11 B12 ]
C                    [  0  B22 ]
C                    [  0   0  ]
C                    [  0   0  ]
C        [Bc1,Bc2] = [  .   .  ],
C                    [  .   .  ]
C                    [  .   .  ]
C                    [  0   0  ]
C
C     where the blocks  B11, B22, A31, ..., Ap,p-2  have full row ranks and
C     p is the controllability index of the pair (A,[B1,B2]).  The size of the
C     block  Auncont  is equal to the dimension of the uncontrollable
C     subspace of the pair (A,[B1,B2]).
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOBZ    CHARACTER*1
C             Indicates whether the user wishes to accumulate in a
C             matrix Z the orthogonal similarity transformations for
C             reducing the system, as follows:
C             = 'N':  Do not form Z and do not store the orthogonal
C                     transformations;
C             = 'F':  Do not form Z, but store the orthogonal
C                     transformations in the factored form;
C             = 'I':  Z is initialized to the unit matrix and the
C                     orthogonal transformation matrix Z is returned.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the original state-space representation,
C             i.e., the order of the matrix A.  N >= 0.
C
C     M1      (input) INTEGER
C             The number of system inputs in U1, or of columns of B1.
C             M1 >= 0.
C
C     M2      (input) INTEGER
C             The number of system inputs in U2, or of columns of B2.
C             M2 >= 0.
C
C     P       (input) INTEGER
C             The number of system outputs, or of rows of C.  P >= 0.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the original state dynamics matrix A.
C             On exit, the leading N-by-N part of this array contains
C             the transformed state dynamics matrix Ac = Z'*A*Z. The
C             leading NCONT-by-NCONT diagonal block of this matrix,
C             Acont, is the state dynamics matrix of a controllable
C             realization for the original system. The elements below
C             the second block-subdiagonal are set to zero.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     B       (input/output) DOUBLE PRECISION array, dimension
C             (LDB,M1+M2)
C             On entry, the leading N-by-(M1+M2) part of this array must
C             contain the compound input matrix B = [B1,B2], where B1 is
C             N-by-M1 and B2 is N-by-M2.
C             On exit, the leading N-by-(M1+M2) part of this array
C             contains the transformed compound input matrix [Bc1,Bc2] =
C             Z'*[B1,B2]. The leading NCONT-by-(M1+M2) part of this
C             array, [Bcont1, Bcont2], is the compound input matrix of
C             a controllable realization for the original system.
C             All elements below the first block-diagonal are set to
C             zero.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= MAX(1,N).
C
C     C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
C             On entry, the leading P-by-N part of this array must
C             contain the output matrix C.
C             On exit, the leading P-by-N part of this array contains
C             the transformed output matrix Cc, given by C * Z.
C
C     LDC     INTEGER
C             The leading dimension of the array C.  LDC >= MAX(1,P).
C
C     NCONT   (output) INTEGER
C             The order of the controllable state-space representation.
C
C     INDCON  (output) INTEGER
C             The controllability index of the controllable part of the
C             system representation.
C
C     NBLK    (output) INTEGER array, dimension (2*N)
C             The leading INDCON elements of this array contain the
C             orders of the diagonal blocks of Acont. INDCON is always
C             an even number, and the INDCON/2 odd and even components
C             of NBLK have decreasing values, respectively.
C             Note that some elements of NBLK can be zero.
C
C     Z       (output) DOUBLE PRECISION array, dimension (LDZ,N)
C             If JOBZ = 'I', then the leading N-by-N part of this
C             array contains the matrix of accumulated orthogonal
C             similarity transformations which reduces the given system
C             to orthogonal canonical form.
C             If JOBZ = 'F', the elements below the diagonal, with the
C             array TAU, represent the orthogonal transformation matrix
C             as a product of elementary reflectors. The transformation
C             matrix can then be obtained by calling the LAPACK Library
C             routine DORGQR.
C             If JOBZ = 'N', the array Z is not referenced and can be
C             supplied as a dummy array (i.e., set parameter LDZ = 1 and
C             declare this array to be Z(1,1) in the calling program).
C
C     LDZ     INTEGER
C             The leading dimension of the array Z. If JOBZ = 'I' or
C             JOBZ = 'F', LDZ >= MAX(1,N); if JOBZ = 'N', LDZ >= 1.
C
C     TAU     (output) DOUBLE PRECISION array, dimension (MIN(N,M1+M2))
C             The elements of TAU contain the scalar factors of the
C             elementary reflectors used in the reduction of [B1,B2]
C             and A.
C
C     Tolerances
C
C     TOL     DOUBLE PRECISION
C             The tolerance to be used in rank determinations when
C             transforming (A, [B1,B2]). If the user sets TOL > 0, then
C             the given value of TOL is used as a lower bound for the
C             reciprocal condition number (see the description of the
C             argument RCOND in the SLICOT routine MB03OD);  a
C             (sub)matrix whose estimated condition number is less than
C             1/TOL is considered to be of full rank.  If the user sets
C             TOL <= 0, then an implicitly computed, default tolerance,
C             defined by  TOLDEF = N*N*EPS,  is used instead, where EPS
C             is the machine precision (see LAPACK Library routine
C             DLAMCH).
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (MAX(M1,M2))
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The length of the array DWORK.  LDWORK >= 1, and
C             LDWORK >= MAX(N, 3*MAX(M1,M2), P), if MIN(N,M1+M2) > 0.
C             For optimum performance LDWORK should be larger.
C
C             If LDWORK = -1, then a workspace query is assumed; the
C             routine only calculates the optimal size of the DWORK
C             array, returns this value as the first entry of the DWORK
C             array, and no error message related to LDWORK is issued by
C             XERBLA.
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
C     The implemented algorithm [1] represents a specialization of the
C     controllability staircase algorithm of [2] to the special structure
C     of the input matrix B = [B1,B2].
C
C     REFERENCES
C
C     [1] Varga, A.
C         Reliable algorithms for computing minimal dynamic covers.
C         Proc. CDC'2003, Hawaii, 2003.
C
C     [2] Varga, A.
C         Numerically stable algorithm for standard controllability
C         form determination.
C         Electronics Letters, vol. 17, pp. 74-75, 1981.
C
C     NUMERICAL ASPECTS
C                               3
C     The algorithm requires 0(N ) operations and is backward stable.
C
C     FURTHER COMMENTS
C
C     If the system matrices A and B are badly scaled, it would be
C     useful to scale them with SLICOT routine TB01ID, before calling
C     the routine.
C
C     CONTRIBUTOR
C
C     A. Varga, DLR Oberpfaffenhofen, March 2003.
C
C     REVISIONS
C
C     A. Varga, DLR Oberpfaffenhofen, April 2003, December 2006.
C     V. Sima, December 2016, April 2017.
C
C     KEYWORDS
C
C     Controllability, minimal realization, orthogonal canonical form,
C     orthogonal transformation.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0 )
C     .. Scalar Arguments ..
      CHARACTER         JOBZ
      INTEGER           INDCON, INFO, LDA, LDB, LDC, LDWORK, LDZ, M1,
     $                  M2, N, NCONT, P
      DOUBLE PRECISION  TOL
C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), B(LDB,*), C(LDC,*), DWORK(*), TAU(*),
     $                  Z(LDZ,*)
      INTEGER           IWORK(*), NBLK(*)
C     .. Local Scalars ..
      LOGICAL           B1RED, LJOBF, LJOBI, LJOBZ, LQUERY
      INTEGER           IQR, ITAU, J, JB2, JQR, M, MCRT, MCRT1,
     $                  MCRT2, MINWRK, NCRT, NI, NJ, RANK, WRKOPT
      DOUBLE PRECISION  ANORM, BNORM, FNRM, FNRM2, FNRMA, TOLDEF
C     .. Local Arrays ..
      DOUBLE PRECISION  SVAL(3)
C     .. External Functions ..
      LOGICAL           LSAME
      DOUBLE PRECISION  DLAMCH, DLANGE
      EXTERNAL          DLAMCH, DLANGE, LSAME
C     .. External Subroutines ..
      EXTERNAL          DCOPY, DLACPY, DLAPMT, DLASET, DORGQR, DORMQR,
     $                  MB01PD, MB03OY, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         DBLE, INT, MAX, MIN
C     ..
C     .. Executable Statements ..
C
      INFO  = 0
      LJOBF = LSAME( JOBZ, 'F' )
      LJOBI = LSAME( JOBZ, 'I' )
      LJOBZ = LJOBF.OR.LJOBI
      M = M1 + M2
C
C     Test the input scalar arguments.
C
      IF( .NOT.LJOBZ .AND. .NOT.LSAME( JOBZ, 'N' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( M1.LT.0 ) THEN
         INFO = -3
      ELSE IF( M2.LT.0 ) THEN
         INFO = -4
      ELSE IF( P.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE IF( LDC.LT.MAX( 1, P ) ) THEN
         INFO = -11
      ELSE IF( LDZ.LT.1 .OR. ( LJOBZ .AND. LDZ.LT.N ) ) THEN
         INFO = -16
      ELSE
         IF( MIN( N, M ).EQ.0 ) THEN
            MINWRK = 1
         ELSE
            MINWRK = MAX( N, 3*MAX( M1, M2 ), P )
         END IF
C
         LQUERY = LDWORK.LT.0
         IF( LQUERY ) THEN
            MCRT = MAX( M1, M2 )
            RANK = MIN( N, MCRT )
            CALL DORMQR( 'Left', 'Transpose', N, MCRT, RANK, B, LDB,
     $                   TAU, B, LDB, DWORK, -1, INFO )
            WRKOPT = MAX( MINWRK, INT( DWORK(1) ) )
            CALL DORMQR( 'Left', 'Transpose', N, N, RANK, B, LDB, TAU,
     $                   A, LDA, DWORK, -1, INFO )
            WRKOPT = MAX( WRKOPT, INT( DWORK(1) ) )
            CALL DORMQR( 'Right', 'No transpose', P, N, RANK, B, LDB,
     $                   TAU, C, LDC, DWORK, -1, INFO )
            WRKOPT = MAX( WRKOPT, INT( DWORK(1) ) )
            IF( LJOBI .AND. N.GT.0 ) THEN
               CALL DORGQR( N, N, N-1, Z, LDZ, TAU, DWORK, -1, INFO )
               WRKOPT = MAX( WRKOPT, INT( DWORK(1) ) )
            END IF
         ELSE IF( LDWORK.LT.MINWRK ) THEN
            INFO = -21
         END IF
      END IF
C
      IF( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'TB01UY', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         DWORK(1) = WRKOPT
         RETURN
      END IF
C
      NCONT  = 0
      INDCON = 0
C
C     Quick return if possible.
C
      IF( N.EQ.0 ) THEN
         DWORK(1) = ONE
         RETURN
      END IF
C
C     Calculate the absolute norms of A and B (used for scaling).
C
      ANORM = DLANGE( 'M', N, N, A, LDA, DWORK )
      BNORM = DLANGE( 'M', N, M, B, LDB, DWORK )
C
C     Return if matrix B is zero.
C
      IF( BNORM.EQ.ZERO ) THEN
         IF( LJOBI ) THEN
            CALL DLASET( 'Full', N, N, ZERO, ONE, Z, LDZ )
         ELSE IF( LJOBF .AND. N.GT.1 ) THEN
            CALL DLASET( 'Lower', N-1, N-1, ZERO, ZERO, Z(2,1), LDZ )
            CALL DLASET( 'Full', MIN( N, M ), 1, ZERO, ZERO, TAU, N )
         END IF
         DWORK(1) = ONE
         RETURN
      END IF
C
C     Scale (if needed) the matrices A and B.
C
      CALL MB01PD( 'S', 'G', N, N, 0, 0, ANORM, 0, NBLK, A, LDA, INFO )
      CALL MB01PD( 'S', 'G', N, M, 0, 0, BNORM, 0, NBLK, B, LDB, INFO )
C
C     Compute the Frobenius norm of B1 (used for rank estimation of B1).
C     In the loop, then use the Frobenius norm of B2 and then of A.
C
      FNRM  = DLANGE( 'F', N, M1, B, LDB, DWORK )
      FNRMA = DLANGE( 'F', N, N,  A, LDA, DWORK )
      IF( M2.GT.0 ) THEN
         FNRM2 = DLANGE( 'F', N, M2, B(1,M1+1), LDB, DWORK )
      ELSE
         FNRM2 = ZERO
      END IF
C
      TOLDEF = TOL
      IF( TOLDEF.LE.ZERO ) THEN
C
C        Use the default tolerance in controllability determination.
C
         TOLDEF = DBLE( N*N )*DLAMCH( 'EPSILON' )
      END IF
C
      WRKOPT = 1
      NI = 0
      NJ = 0
      ITAU = 1
      NCRT = N
      MCRT1 = M1
      MCRT2 = M2
      MCRT = MCRT1
      IQR  = 1
      JQR = 1
      B1RED = .TRUE.
      JB2 = MIN( M1+1, M )
C
C     (Note: Comments in the code beginning "Workspace:" describe the
C     minimal amount of real workspace needed at that point in the
C     code, as well as the preferred amount for good performance.
C     NB refers to the optimal block size for the immediately
C     following subroutine, as returned by ILAENV.)
C
   10 CONTINUE
C
C        Rank-revealing QR decomposition with column pivoting.
C        The calculation is performed in NCRT rows of B starting from
C        the row IQR.
C        Workspace: 3*MAX(MCRT1,MCRT2).
C
         CALL MB03OY( NCRT, MCRT, B(IQR,JQR), LDB, TOLDEF, FNRM, RANK,
     $                SVAL, IWORK, TAU(ITAU), DWORK, INFO )
C
         IF( RANK.EQ.0 ) THEN
            IF( B1RED ) THEN
               IF( MCRT2.GT.0 ) THEN
                  B1RED  = .NOT.B1RED
                  INDCON = INDCON + 1
                  NBLK(INDCON) = 0
                  IF( INDCON.EQ.1 ) THEN
                     FNRM = FNRM2
                  ELSE IF( INDCON.GT.2 ) THEN
                     NJ = NJ + MCRT
                  END IF
                  MCRT1 = 0
                  MCRT  = MCRT2
                  JQR   = JB2
                  IF( INDCON.GE.2 ) THEN
                     IF( INDCON.EQ.2 )
     $                  FNRM = FNRMA
                     CALL DLACPY( 'G', NCRT, MCRT, A(NCONT+1,NJ+1),
     $                            LDA, B(IQR,JQR), LDB )
                     CALL DLASET( 'G', NCRT, MCRT, ZERO, ZERO,
     $                            A(NCONT+1,NJ+1), LDA )
                  END IF
                  GO TO 10
               END IF
            ELSE
               IF( MCRT1.GT.0 ) THEN
                  B1RED  = .NOT.B1RED
                  INDCON = INDCON + 1
                  NBLK(INDCON) = 0
                  MCRT2 = 0
                  MCRT  = MCRT1
                  JQR   = 1
                  IF( INDCON.GE.2 ) THEN
                     IF( INDCON.EQ.2 ) THEN
                        FNRM = FNRMA
                     ELSE IF( INDCON.GT.2 ) THEN
                        NJ = NJ + MCRT
                     END IF
                     CALL DLACPY( 'G', NCRT, MCRT, A(NCONT+1,NJ+1),
     $                            LDA, B(IQR,JQR), LDB )
                     CALL DLASET( 'G', NCRT, MCRT, ZERO, ZERO,
     $                            A(NCONT+1,NJ+1), LDA )
                  END IF
                  GO TO 10
               END IF
               INDCON = INDCON - 1
            END IF
         ELSE
            NI     = NCONT
            NCONT  = NCONT + RANK
            INDCON = INDCON + 1
            NBLK(INDCON) = RANK
C
C           Premultiply the appropriate block row of B2 by Q'.
C           Workspace: need   MCRT2;
C                      prefer MCRT2*NB.
C
            IF( INDCON.LT.2 ) THEN
               FNRM = FNRM2
               CALL DORMQR( 'Left', 'Transpose', NCRT, MCRT2, RANK,
     $                      B(IQR,JQR), LDB, TAU(ITAU),
     $                      B(NI+1,JB2), LDB, DWORK, LDWORK, INFO )
               WRKOPT = MAX( WRKOPT, INT( DWORK(1) ) )
            END IF
C
C           Premultiply and postmultiply the appropriate block row
C           and block column of A by Q' and Q, respectively.
C           Workspace: need   N;
C                      prefer N*NB.
C
            CALL DORMQR( 'Left', 'Transpose', NCRT, N-NJ, RANK,
     $                   B(IQR,JQR), LDB, TAU(ITAU), A(NI+1,NJ+1), LDA,
     $                   DWORK, LDWORK, INFO )
            WRKOPT = MAX( WRKOPT, INT( DWORK(1) ) )
C
C           Workspace: need   N;
C                      prefer N*NB.
C
            CALL DORMQR( 'Right', 'No transpose', N, NCRT, RANK,
     $                   B(IQR,JQR), LDB, TAU(ITAU), A(1,NI+1), LDA,
     $                   DWORK, LDWORK, INFO )
            WRKOPT = MAX( WRKOPT, INT( DWORK(1) ) )
C
C           Postmultiply the appropriate block column of C by Q.
C           Workspace: need   P;
C                      prefer P*NB.
C
            CALL DORMQR( 'Right', 'No transpose', P, NCRT, RANK,
     $                   B(IQR,JQR), LDB, TAU(ITAU), C(1,NI+1), LDC,
     $                   DWORK, LDWORK, INFO )
            WRKOPT = MAX( WRKOPT, INT( DWORK(1) ) )
C
C           If required, save transformations.
C
            IF( LJOBZ .AND. NCRT.GT.1 ) THEN
               CALL DLACPY( 'L', NCRT-1, MIN( RANK, NCRT-1 ),
     $                      B(IQR+1,JQR), LDB, Z(NI+2,ITAU), LDZ )
            END IF
C
C           Zero the subdiagonal elements of the current matrix.
C
            IF( RANK.GT.1 )
     $         CALL DLASET( 'L', RANK-1, RANK-1, ZERO, ZERO,
     $                      B(IQR+1,JQR), LDB )
C
C           Backward permutation of the columns of B or A.
C
            IF( INDCON.LE.2 ) THEN
               IF( INDCON.EQ.2 )
     $            FNRM = FNRMA
               CALL DLAPMT( .FALSE., RANK, MCRT, B(IQR,JQR), LDB,
     $                      IWORK )
               IQR = IQR + RANK
            ELSE
               DO 20 J = 1, MCRT
                  CALL DCOPY( RANK, B(IQR,JQR+J-1), 1,
     $                        A(NI+1,NJ+IWORK(J)), 1 )
   20          CONTINUE
            END IF
C
            ITAU = ITAU + RANK
            IF( RANK.NE.NCRT ) THEN
               IF( INDCON.GT.2 )
     $            NJ = NJ + MCRT
               IF( B1RED ) THEN
                  MCRT1 = RANK
                  MCRT  = MCRT2
                  JQR   = JB2
               ELSE
                  MCRT2 = RANK
                  MCRT  = MCRT1
                  JQR   = 1
               END IF
               NCRT = NCRT - RANK
               IF( INDCON.GE.2 ) THEN
                  CALL DLACPY( 'G', NCRT, MCRT, A(NCONT+1,NJ+1), LDA,
     $                         B(IQR,JQR), LDB )
                  CALL DLASET( 'G', NCRT, MCRT, ZERO, ZERO,
     $                         A(NCONT+1,NJ+1), LDA )
               END IF
               B1RED = .NOT.B1RED
               GO TO 10
            ELSE
               IF( B1RED ) THEN
                  INDCON = INDCON + 1
                  NBLK(INDCON) = 0
               END IF
            END IF
         END IF
C     End loop 10.
C
C     If required, accumulate transformations.
C     Workspace: need N;  prefer N*NB.
C
      IF( LJOBI ) THEN
         CALL DORGQR( N, N, MAX( 1, ITAU-1 ), Z, LDZ, TAU, DWORK,
     $                LDWORK, INFO )
         WRKOPT = MAX( WRKOPT, INT( DWORK(1) ) )
      END IF
C
C     Annihilate the trailing blocks of B1 and B2.
C
      IF( NBLK(1).LT.N )
     $   CALL DLASET( 'G', N-NBLK(1), M1, ZERO, ZERO,
     $                B(NBLK(1)+1,1), LDB )
      IF( IQR.LE.N )
     $   CALL DLASET( 'G', N-IQR+1, M2, ZERO, ZERO, B(IQR,JB2), LDB )
C
C     Annihilate the trailing elements of TAU, if JOBZ = 'F'.
C
      IF( LJOBF ) THEN
         DO 30 J = ITAU, MIN( N, M )
            TAU(J) = ZERO
   30    CONTINUE
      END IF
C
C     Undo scaling of A and B.
C
      CALL MB01PD( 'U', 'G', N, N, 0, 0, ANORM, 0, NBLK, A, LDA, INFO )
      CALL MB01PD( 'U', 'G', NBLK(1)+NBLK(2), M, 0, 0, BNORM, 0, NBLK,
     $             B, LDB, INFO )
C
C     Set optimal workspace dimension.
C
      DWORK(1) = WRKOPT
      RETURN
C *** Last line of TB01UY ***
      END
