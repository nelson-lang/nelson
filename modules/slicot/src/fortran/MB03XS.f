      SUBROUTINE MB03XS( JOBU, N, A, LDA, QG, LDQG, U1, LDU1, U2, LDU2,
     $                   WR, WI, DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To compute the eigenvalues and real skew-Hamiltonian Schur form of
C     a skew-Hamiltonian matrix,
C
C                   [  A   G  ]
C             W  =  [       T ],
C                   [  Q   A  ]
C
C     where A is an N-by-N matrix and G, Q are N-by-N skew-symmetric
C     matrices. Specifically, an orthogonal symplectic matrix U is
C     computed so that
C
C               T       [  Aout  Gout  ]
C              U W U =  [            T ] ,
C                       [    0   Aout  ]
C
C     where Aout is in Schur canonical form (as returned by the LAPACK
C     routine DHSEQR). That is, Aout is block upper triangular with
C     1-by-1 and 2-by-2 diagonal blocks; each 2-by-2 diagonal block has
C     its diagonal elements equal and its off-diagonal elements of
C     opposite sign.
C
C     Optionally, the matrix U is returned in terms of its first N/2
C     rows
C
C                      [  U1   U2 ]
C                  U = [          ].
C                      [ -U2   U1 ]
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOBU    CHARACTER*1
C             Specifies whether matrix U is computed or not, as follows:
C             = 'N': transformation matrix U is not computed;
C             = 'U': transformation matrix U is computed.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrix A.  N >= 0.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the matrix A.
C             On exit, the leading N-by-N part of this array contains
C             the matrix Aout in Schur canonical form.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     QG      (input/output) DOUBLE PRECISION array, dimension
C                            (LDQG,N+1)
C             On entry, the leading N-by-N+1 part of this array must
C             contain in columns 1:N the strictly lower triangular part
C             of the matrix Q and in columns 2:N+1 the strictly upper
C             triangular part of the matrix G.
C             On exit, the leading N-by-N+1 part of this array contains
C             in columns 2:N+1 the strictly upper triangular part of the
C             skew-symmetric matrix Gout. The part which contained the
C             matrix Q is set to zero.
C             Note that the parts containing the diagonal and the first
C             superdiagonal of this array are not overwritten by zeros
C             only if JOBU = 'U' or LDWORK >= 2*N*N - N.
C
C     LDQG    INTEGER
C             The leading dimension of the array QG.  LDQG >= MAX(1,N).
C
C     U1      (output) DOUBLE PRECISION array, dimension (LDU1,N)
C             On exit, if JOBU = 'U', the leading N-by-N part of this
C             array contains the matrix U1.
C             If JOBU = 'N', this array is not referenced.
C
C     LDU1    INTEGER
C             The leading dimension of the array U1.
C             LDU1 >= MAX(1,N),  if JOBU = 'U';
C             LDU1 >= 1,         if JOBU = 'N'.
C
C     U2      (output) DOUBLE PRECISION array, dimension (LDU2,N)
C             On exit, if JOBU = 'U', the leading N-by-N part of this
C             array contains the matrix U2.
C             If JOBU = 'N', this array is not referenced.
C
C     LDU2    INTEGER
C             The leading dimension of the array U2.
C             LDU2 >= MAX(1,N),  if JOBU = 'U';
C             LDU2 >= 1,         if JOBU = 'N'.
C
C     WR      (output) DOUBLE PRECISION array, dimension (N)
C     WI      (output) DOUBLE PRECISION array, dimension (N)
C             The real and imaginary parts, respectively, of the
C             eigenvalues of Aout, which are half of the eigenvalues
C             of W. The eigenvalues are stored in the same order as on
C             the diagonal of Aout, with WR(i) = Aout(i,i) and, if
C             Aout(i:i+1,i:i+1) is a 2-by-2 diagonal block, WI(i) > 0
C             and WI(i+1) = -WI(i).
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0,  DWORK(1)  returns the optimal value
C             of LDWORK.
C             On exit, if  INFO = -14,  DWORK(1)  returns the minimum
C             value of LDWORK.
C
C     LDWORK  INTEGER
C             The length of the array DWORK.
C             LDWORK >= MAX(1,(N+5)*N),      if JOBU = 'U';
C             LDWORK >= MAX(1,5*N,(N+1)*N),  if JOBU = 'N'.
C
C             If LDWORK = -1, then a workspace query is assumed;
C             the routine only calculates the optimal size of the
C             DWORK array, returns this value as the first entry of
C             the DWORK array, and no error message related to LDWORK
C             is issued by XERBLA.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value;
C             > 0:  if INFO = i, DHSEQR failed to compute all of the
C                   eigenvalues.  Elements 1:ILO-1 and i+1:N of WR
C                   and WI contain those eigenvalues which have been
C                   successfully computed. The matrix A (and QG) has
C                   been partially reduced; namely, A is upper
C                   Hessenberg in the rows and columns ILO through i.
C                   (See DHSEQR for details.)
C
C     METHOD
C
C     First, using the SLICOT Library routine MB04RB, an orthogonal
C     symplectic matrix UP is computed so that
C
C                T      [  AP  GP  ]
C            UP W UP =  [        T ]
C                       [  0   AP  ]
C
C     is in Paige/Van Loan form. Next, the LAPACK routine DHSEQR is
C     applied to the matrix AP to compute an orthogonal matrix V so
C     that Aout = V'*AP*V is in Schur canonical form.
C     Finally, the transformations
C
C                       [ V  0 ]
C             U =  UP * [      ],     Gout = V'*G*V,
C                       [ 0  V ]
C
C     using the SLICOT Library routine MB01LD for the latter, are
C     performed.
C
C     REFERENCES
C
C     [1] Van Loan, C.F.
C         A symplectic method for approximating all the eigenvalues of
C         a Hamiltonian matrix.
C         Linear Algebra and its Applications, 61, pp. 233-251, 1984.
C
C     [2] Kressner, D.
C         Block algorithms for orthogonal symplectic factorizations.
C         BIT, 43 (4), pp. 775-790, 2003.
C
C     CONTRIBUTORS
C
C     D. Kressner (Technical Univ. Berlin, Germany) and
C     P. Benner (Technical Univ. Chemnitz, Germany), December 2003.
C
C     REVISIONS
C
C     V. Sima, Nov. 2011 (SLICOT version of the HAPACK routine DSHES).
C     V. Sima, Oct. 2012.
C
C     KEYWORDS
C
C     Schur form, eigenvalues, skew-Hamiltonian matrix.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0 )
C     .. Scalar Arguments ..
      CHARACTER         JOBU
      INTEGER           INFO, LDA, LDQG, LDU1, LDU2, LDWORK, N
C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), DWORK(*), QG(LDQG,*), U1(LDU1,*),
     $                  U2(LDU2,*), WI(*), WR(*)
C     .. Local Scalars ..
      LOGICAL           COMPU, LQUERY, SCALEW
      INTEGER           I, I1, I2, IERR, ILO, INXT, NN, PBAL, PCS, PDV,
     $                  PDW, PHO, PTAU, WRKMIN, WRKOPT
      DOUBLE PRECISION  BIGNUM, CSCALE, EPS, SMLNUM, WNRM
C     .. External Function ..
      LOGICAL           LSAME
      DOUBLE PRECISION  DLAMCH, MA02ID
      EXTERNAL          DLAMCH, LSAME, MA02ID
C     .. External Subroutines ..
      EXTERNAL          DCOPY, DHSEQR, DLABAD, DLACPY, DLASCL, DLASET,
     $                  DSCAL, DSWAP, MB01LD, MB04DI, MB04DS, MB04QS,
     $                  MB04RB, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         DBLE, INT, MAX, MIN, SQRT
C
C     .. Executable Statements ..
C
C     Check the scalar input parameters.
C
      INFO  = 0
      COMPU = LSAME( JOBU, 'U' )
C
      NN = N*N
      IF ( COMPU ) THEN
         WRKMIN = MAX( 1, NN + 5*N )
      ELSE
         WRKMIN = MAX( 1, 5*N, NN + N )
      END IF
      WRKOPT = WRKMIN
C
      IF ( .NOT.COMPU .AND. .NOT.LSAME( JOBU, 'N' ) ) THEN
         INFO = -1
      ELSE IF ( N.LT.0 ) THEN
         INFO = -2
      ELSE IF (  LDA.LT.MAX( 1,N ) ) THEN
         INFO = -4
      ELSE IF ( LDQG.LT.MAX( 1,N ) ) THEN
         INFO = -6
      ELSE IF ( LDU1.LT.1 .OR. ( COMPU .AND. LDU1.LT.N ) ) THEN
         INFO = -8
      ELSE IF ( LDU2.LT.1 .OR. ( COMPU .AND. LDU2.LT.N ) ) THEN
         INFO = -10
      ELSE
         LQUERY = LDWORK.EQ.-1
         IF ( LDWORK.LT.WRKMIN .AND. .NOT.LQUERY ) THEN
            DWORK(1) = DBLE( WRKMIN )
            INFO = -14
         ELSE IF( LQUERY ) THEN
            IF ( N.EQ.0 ) THEN
               DWORK(1) = ONE
            ELSE
               I = 4*N
               CALL MB04RB( N, 1, A, LDA, QG, LDQG, DWORK, DWORK, DWORK,
     $                      -1, IERR )
               WRKOPT = MAX( WRKOPT, INT( DWORK(1) ) + I )
               CALL DHSEQR( 'Schur', 'Initialize', N, 1, N, A, LDA, WR,
     $                      WI, DWORK, N, DWORK, -1, IERR )
               IF ( COMPU ) THEN
                  I = I + NN
                  WRKOPT = MAX( WRKOPT, INT( DWORK(1) ) + I )
                  CALL MB04QS( 'N', 'N', 'N', N, N, 1, DWORK, N, QG,
     $                         LDQG, U1, LDU1, U2, LDU2, DWORK, DWORK,
     $                         DWORK, -1, IERR )
               ELSE
                  I = NN
                  WRKOPT = MAX( WRKOPT, 2*NN - N )
               END IF
               DWORK(1) = MAX( WRKMIN, WRKOPT, INT( DWORK(1) ) + I )
            END IF
            RETURN
         END IF
      END IF
C
C     Return if there were illegal values.
C
      IF ( INFO.NE.0 ) THEN
         CALL XERBLA( 'MB03XS', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF ( N.EQ.0 ) THEN
         DWORK(1) = ONE
         RETURN
      END IF
C
C     Get machine constants.
C
      EPS    = DLAMCH( 'P' )
      SMLNUM = DLAMCH( 'S' )
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
      SMLNUM = SQRT( SMLNUM ) / EPS
      BIGNUM = ONE / SMLNUM
C
C     Scale W if max element outside range [SMLNUM,BIGNUM].
C
      WNRM = MA02ID( 'Skew-Hamiltonian', 'Max-Norm', N, A, LDA, QG,
     $               LDQG, DWORK )
      SCALEW = .FALSE.
      IF ( WNRM.GT.ZERO .AND. WNRM.LT.SMLNUM ) THEN
         SCALEW = .TRUE.
         CSCALE = SMLNUM
      ELSE IF ( WNRM.GT.BIGNUM ) THEN
         SCALEW = .TRUE.
         CSCALE = BIGNUM
      END IF
      IF( SCALEW ) THEN
         CALL DLASCL( 'General', 0, 0, WNRM, CSCALE, N, N, A, LDA,
     $                IERR )
         IF ( N.GT.1 ) THEN
            CALL DLASCL( 'Lower', 0, 0, WNRM, CSCALE, N-1, N-1, QG(2,1),
     $                   LDQG, IERR )
            CALL DLASCL( 'Upper', 0, 0, WNRM, CSCALE, N-1, N-1, QG(1,3),
     $                   LDQG, IERR )
         END IF
      END IF
C
C     Permute to make W closer to skew-Hamiltonian Schur form.
C     Workspace:  need   N.
C
      PBAL = 1
      CALL MB04DS( 'Permute', N, A, LDA, QG, LDQG, ILO, DWORK(PBAL),
     $             IERR )
C
C     Reduce to Paige/Van Loan form.
C     Workspace:  need   5*N-1.
C
      PCS  =   N + PBAL
      PTAU = 2*N + PCS
      PDW  =   N + PTAU
      CALL MB04RB( N, ILO, A, LDA, QG, LDQG, DWORK(PCS), DWORK(PTAU),
     $             DWORK(PDW), LDWORK-PDW+1, IERR )
      WRKOPT = MAX( WRKOPT, INT( DWORK(PDW) ) + PDW - 1 )
      IF ( COMPU ) THEN
C
C        Copy information about Householder vectors to workspace.
C
         PHO = PDW
         PDW = PDW + NN
         CALL DLACPY( 'L', N, N, A, LDA, DWORK(PHO), N )
C
C        Perform QR iteration, accumulating Schur vectors in U1.
C        Workspace:  need   N*N + 5*N;
C                    prefer larger.
C
         CALL DHSEQR( 'Schur', 'Initialize', N, MIN( ILO, N ), N, A,
     $                LDA, WR, WI, U1, LDU1, DWORK(PDW), LDWORK-PDW+1,
     $                INFO )
         WRKOPT = MAX( WRKOPT, INT( DWORK(PDW) ) + PDW - 1 )
C
C        Update G = V'*G*V.
C
         CALL MB01LD( 'Upper', 'Transpose', N, N, ZERO, ONE, QG(1,2),
     $                 LDQG, U1, LDU1, QG(1,2), LDQG, U2, NN, IERR )
C
C        Apply orthogonal symplectic matrix from PVL reduction to [V;0].
C        Workspace:  need   N*N + 5*N;
C                    prefer larger.
C
         CALL DSCAL(  N-1, -ONE, DWORK(PCS+1), 2 )
         CALL DLASET( 'All', N, N, ZERO, ZERO, U2, LDU2 )
         CALL MB04QS( 'No Transpose', 'No Transpose', 'No Transpose', N,
     $                N, ILO, DWORK(PHO), N, QG, LDQG, U1, LDU1, U2,
     $                LDU2, DWORK(PCS), DWORK(PTAU), DWORK(PDW),
     $                LDWORK-PDW+1, IERR )
         WRKOPT = MAX( WRKOPT, INT( DWORK(PDW) ) + PDW - 1 )
C
C        Annihilate Q.
C
         IF ( N.GT.1 )
     $      CALL DLASET( 'Lower', N-1, N-1, ZERO, ZERO, QG(2,1), LDQG )
C
C        Undo balancing.
C
         CALL MB04DI( 'Permute', 'Positive', N, ILO, DWORK(PBAL), N, U1,
     $                LDU1, U2, LDU2, IERR )
      ELSE
C
C        Perform QR iteration, accumulating Schur vectors in DWORK.
C        Workspace:  need   N*N + N;
C                    prefer larger.
C
         PDV = 1
         PDW = NN + PDV
         CALL DHSEQR( 'Schur', 'Initialize', N, MIN( ILO, N ), N, A,
     $                LDA, WR, WI, DWORK(PDV), N, DWORK(PDW),
     $                LDWORK-PDW+1, INFO )
         WRKOPT = MAX( WRKOPT, INT( DWORK(PDW) ) + PDW - 1 )
C
C        Update G = V'*G*V.
C        Workspace:  need   N*N + N;
C                    prefer N*N + N*(N-1).
C
         CALL MB01LD( 'Upper', 'Transpose', N, N, ZERO, ONE, QG(1,2),
     $                LDQG, DWORK(PDV), N, QG(1,2), LDQG, DWORK(PDW),
     $                LDWORK-PDW+1, IERR )
         WRKOPT = MAX( WRKOPT, 2*NN - N )
C
C        Annihilate Q.
C
         IF ( N.GT.1 )
     $      CALL DLASET( 'Lower', N-1, N-1, ZERO, ZERO, QG(2,1), LDQG )
      END IF
C
      IF ( SCALEW ) THEN
C
C        Undo scaling for the skew-Hamiltonian Schur form.
C
         CALL DLASCL( 'Hessenberg', 0, 0, CSCALE, WNRM, N, N, A, LDA,
     $                IERR )
         IF ( N.GT.1 ) THEN
            CALL DLASCL( 'Upper', 0, 0, CSCALE, WNRM, N-1, N-1, QG(1,3),
     $                   LDQG, IERR )
         END IF
         CALL DCOPY( N, A, LDA+1, WR, 1 )
C
         IF ( CSCALE.EQ.SMLNUM ) THEN
C
C           If scaling back towards underflow, adjust WI if an
C           offdiagonal element of a 2-by-2 block in the Schur form
C           underflows.
C
            IF( INFO.GT.0 ) THEN
               I1 = INFO + 1
               CALL DLASCL( 'General', 0, 0, CSCALE, WNRM, ILO-1, 1, WI,
     $                      MAX( ILO-1, 1 ), IERR )
            ELSE
               I1 = ILO
            END IF
            I2   = N - 1
            INXT = I1 - 1
            DO 10 I = I1, I2
               IF ( I.LT.INXT )
     $            GO TO 10
               IF ( WI(I).EQ.ZERO ) THEN
                  INXT = I + 1
               ELSE
                  IF ( A(I+1,I).EQ.ZERO ) THEN
                     WI(I)   = ZERO
                     WI(I+1) = ZERO
                  ELSE IF ( A(I,I+1).EQ.ZERO )
     $                  THEN
                     WI(I)   = ZERO
                     WI(I+1) = ZERO
                     IF ( I.GT.1 )
     $                  CALL DSWAP( I-1, A(1,I), 1, A(1,I+1), 1 )
                     IF( N.GT.I+1 )
     $                  CALL DSWAP( N-I-1, A(I,I+2), LDA,
     $                              A(I+1,I+2), LDA )
                     A(I,I+1)  = A(I+1,I)
                     A(I+1,I ) = ZERO
C
                     CALL DSWAP( I-1, QG(1,I+2), 1, QG(1,I+1), 1 )
                     IF ( N.GT.I+1 )
     $                  CALL DSWAP( N-I-1, QG(I+1,I+3), LDQG, QG(I,I+3),
     $                              LDQG )
                     QG(I,I+2) = -QG(I,I+2)
                     IF ( COMPU ) THEN
                        CALL DSWAP( N, U1(1,I), 1, U1(1,I+1), 1 )
                        CALL DSWAP( N, U2(1,I), 1, U2(1,I+1), 1 )
                     END IF
                  END IF
                  INXT = I + 2
               END IF
   10       CONTINUE
         END IF
C
C        Undo scaling for imaginary parts of the eigenvalues.
C
         CALL DLASCL( 'General', 0, 0, CSCALE, WNRM, N-INFO, 1,
     $                WI(INFO+1), MAX( N-INFO, 1 ), IERR )
      END IF
C
      DWORK(1) = DBLE( WRKOPT )
C
      RETURN
C *** Last line of MB03XS ***
      END
