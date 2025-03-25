      SUBROUTINE TG01OD( JOBE, N, DCBA, LDDCBA, E, LDE, NZ, G, TOL,
     $                   DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To compute for a single-input single-output descriptor system,
C     given by the system matrix
C
C        [ D     C    ]
C        [ B  A - s*E ],
C
C     with E nonsingular, a reduced system matrix,
C
C        [ d     c    ]
C        [ b  a - s*e ],
C
C     such that d has a "sufficiently" large magnitude.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOBE    CHARACTER*1
C             Specifies whether E is a general or an identity matrix,
C             as follows:
C             = 'G':  The matrix E is a general matrix;
C             = 'I':  The matrix E is assumed identity and is not given.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The dimension of the descriptor state vector; also the
C             order of square matrices A and E, the number of rows of
C             matrix B, and the number of columns of matrix C.  N >= 0.
C
C     DCBA    (input/output) DOUBLE PRECISION array, dimension
C             (LDDCBA,N+1)
C             On entry, the leading (N+1)-by-(N+1) part of this array
C             must contain the original system matrices A, B, C, and D,
C             stored as follows
C
C                [ D  C ]
C                [ B  A ].
C
C             On exit, the leading (NZ+1)-by-(NZ+1) part of this array
C             contains the reduced system matrices a, b, c, and d.
C
C     LDDCBA  INTEGER
C             The leading dimension of the array DCBA.  LDDCBA >= N+1.
C
C     E       (input/output) DOUBLE PRECISION array, dimension (LDE,*)
C             On entry, if JOBE = 'G', the leading N-by-N part of this
C             array must contain the nonsingular descriptor matrix E.
C             On exit, if JOBE = 'G', the leading NZ-by-NZ part of this
C             array contains the reduced descriptor matrix e.
C             If JOBE = 'I', this array is not referenced.
C
C     LDE     INTEGER
C             The leading dimension of the array E.
C             LDE >= MAX(1,N), if JOBE = 'G';
C             LDE >= 1,        if JOBE = 'I'.
C
C     NZ      (output) INTEGER
C             The order of the reduced system.
C
C     G       (output) DOUBLE PRECISION
C             The gain of the reduced system.
C
C     Tolerances
C
C     TOL     DOUBLE PRECISION
C             The tolerance to be used in determining if the transformed
C             d has a "sufficiently" large magnitude. If the user sets
C             TOL > 0, then the given value of TOL is used. If the user
C             sets TOL <= 0, then an implicitly computed, default
C             tolerance, defined by TOLDEF = EPS**(3/4), is used
C             instead, where EPS is the machine precision (see LAPACK
C             Library routine DLAMCH).
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal value
C             of LDWORK.
C             On exit, if INFO = -11, DWORK(1) returns the minimum value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The length of the array DWORK.
C             LDWORK >= 2*N+1,  if JOBE = 'G';
C             LDWORK >=   N+1,  if JOBE = 'I'.
C             For good performance when JOBE = 'G', LDWORK should be
C             larger. Specifically,
C                LDWORK >= MAX( N*NB(DGEQRF), (N+1)*NB(DORMQR) ),
C             where NB(X) is the optimal block sizes for the LAPACK
C             Library routine X.
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
C                   value.
C
C     METHOD
C
C     Householder transformations and Givens rotations are used to
C     process the matrices. If E is a general matrix, it is first
C     triangularized using the QR decomposition, and the triangular form
C     is preserved during the remaining computations.
C
C     NUMERICAL ASPECTS
C
C     The algorithm is numerically backward stable.
C
C     CONTRIBUTOR
C
C     V. Sima, May 2021.
C
C     REVISIONS
C
C     V. Sima, June 2021, Nov. 2021.
C
C     KEYWORDS
C
C     Givens rotation, orthogonal transformation.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ONE, THREE, FOUR, ZERO
      PARAMETER         ( ONE  = 1.0D0, THREE = 3.0D0, FOUR = 4.0D0,
     $                    ZERO = 0.0D0 )
C     .. Scalar Arguments ..
      CHARACTER         JOBE
      INTEGER           INFO, LDDCBA, LDWORK, LDE, N, NZ
      DOUBLE PRECISION  G, TOL
C     .. Array Arguments ..
      DOUBLE PRECISION  DCBA(LDDCBA,*), DWORK(*), E(LDE,*)
C     .. Local Scalars ..
      CHARACTER         JOBT
      LOGICAL           DESCR, LQUERY
      INTEGER           I, IMAX, ITAU, IWRK, J, JF, MAXWRK, MINWRK, N1,
     $                  NC
      DOUBLE PRECISION  ABSD, MAXA, NRMB, NRMC, TAU, TOLDEF
C     .. External Functions ..
      LOGICAL           LSAME
      INTEGER           IDAMAX
      DOUBLE PRECISION  DLAMCH, DLANGE, DNRM2
      EXTERNAL          DLAMCH, DLANGE, DNRM2, IDAMAX, LSAME
C     .. External Subroutines ..
      EXTERNAL          DCOPY, DGEQRF, DLARF, DLARFG, DLASET, DORMQR,
     $                  DSWAP, TG01OA, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, DBLE, INT, MAX, MIN
C     .. Executable Statements ..
C
      DESCR = LSAME( JOBE, 'G' )
      INFO  = 0
      N1    = N + 1
C
C     Test the input scalar arguments.
C
      IF ( .NOT.DESCR .AND. .NOT.LSAME( JOBE, 'I' ) ) THEN
         INFO = -1
      ELSE IF ( N.LT.0 ) THEN
         INFO = -2
      ELSE IF ( LDDCBA.LT.N1 ) THEN
         INFO = -4
      ELSE IF ( LDE.LT.1 .OR. ( DESCR .AND. LDE.LT.MAX( 1, N ) ) ) THEN
         INFO = -6
      ELSE
         IF( DESCR ) THEN
            MINWRK = 2*N + 1
         ELSE IF( N.EQ.0 ) THEN
            MINWRK = 1
         ELSE
            MINWRK = N1
         END IF
         MAXWRK = MINWRK
         LQUERY = LDWORK.EQ.-1
         IF ( LQUERY ) THEN
            IF ( DESCR ) THEN
               CALL DGEQRF( N, N, E, LDE, DCBA, DWORK, -1, INFO )
               MAXWRK = MAX( MAXWRK, INT( DWORK(1) ) )
               CALL DORMQR( 'Left', 'Transpose', N, N1, N, E, LDE, DCBA,
     $                      DCBA, LDDCBA, DWORK, -1, INFO )
               DWORK(1) = DBLE( MAX( MAXWRK, INT( DWORK(1) ) ) )
            ELSE
               DWORK(1) = DBLE( MAXWRK )
            END IF
            RETURN
         ELSE IF( LDWORK.LT.MINWRK ) THEN
            DWORK(1) = DBLE( MINWRK )
            INFO = -11
         END IF
      END IF
C
      IF( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'TG01OD', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      NZ = N
      IF( N.EQ.0 ) THEN
         G = DCBA(1,1)
         DWORK(1) = ONE
         RETURN
      END IF
C
      TOLDEF = TOL
      IF ( TOLDEF.LE.ZERO ) THEN
C
C        Use the default tolerance.
C
         TOLDEF = DLAMCH( 'Precision' )**( THREE/FOUR )
      END IF
C
C     Check if the reduction is needed.
C
      G    = ONE
      MAXA = DLANGE( 'MAX', N, N, DCBA(2,2), LDDCBA, DWORK )
      NRMB = DNRM2 ( N, DCBA(2,1), 1 )
      NRMC = DNRM2 ( N, DCBA(1,2), N1 )
C
      IF( ABS( DCBA(1,1) )*( ONE + MAXA ).LE.TOLDEF*NRMB*NRMC ) THEN
         IF( DESCR ) THEN
C
C           Triangularize E.
C           Workspace: need   2*N + 1;
C                      prefer MAX( N*NB(DGEQRF), (N+1)*NB(DORMQR) ).
C
            ITAU = 1
            IWRK = ITAU + N
            CALL DGEQRF( N, N, E, LDE, DWORK(ITAU), DWORK(IWRK),
     $                   LDWORK-N, INFO )
            MAXWRK = MAX( MAXWRK, INT( DWORK(IWRK) ) )
            CALL DORMQR( 'Left', 'Transpose', N, N1, N, E, LDE,
     $                   DWORK(ITAU), DCBA(2,1), LDDCBA, DWORK(IWRK),
     $                   LDWORK-N, INFO )
            MAXWRK = MAX( MAXWRK, INT( DWORK(IWRK) ) )
            IF( N.GT.1 )
     $         CALL DLASET( 'Lower', N-1, N-1, ZERO, ZERO, E(2,1), LDE )
            JOBT = 'Upper'
         ELSE
            JOBT = JOBE
         END IF
C
         DO 10 I = 1, N
C
C           Perform one-step deflation of [ D C; B A-s*E ] with D = 0.
C
            NC = NZ + 1
            IF( .NOT.DESCR ) THEN
C
C              Ensure that the currently first entry of B is nonzero,
C              to maximize the identity portion of the Householder
C              transformation.
C
               IF( DCBA(I+1,I).EQ.ZERO ) THEN
C
C                 Bring the largest entry of B in the first position.
C
                  IMAX = IDAMAX( NZ, DCBA(I+1,I), 1 ) + I
                  CALL DSWAP( NC, DCBA(I+1,I), LDDCBA, DCBA(IMAX,I),
     $                        LDDCBA )
                  CALL DSWAP( NC, DCBA(I,I+1), 1, DCBA(I,IMAX), 1 )
               END IF
C
C              Find and apply the Householder transformation setting
C              to zero all entries of the current B, but the first.
C
               CALL DLARFG( NZ, DCBA(I+1,I), DCBA(MIN(I+2,N1),I), 1,
     $                      TAU )
               G = G*DCBA(I+1,I)
               DCBA(I+1,I) = ONE
               CALL DLARF( 'Left',  NZ, NZ, DCBA(I+1,I), 1, TAU,
     $                      DCBA(I+1,I+1), LDDCBA, DWORK )
               CALL DLARF( 'Right', NC, NZ, DCBA(I+1,I), 1, TAU,
     $                      DCBA(I,I+1), LDDCBA, DWORK )
            ELSE
               CALL TG01OA( JOBT, NZ, DCBA(I,I), LDDCBA, E(I,I), LDE,
     $                      INFO )
               G = G*DCBA(I+1,I)/E(I,I)
            END IF
C
C           Reduce DCBA (delete the second row and first column of the
C           current DCBA matrix). Actually, the first row is copied over
C           the second, and then the first row and column are removed.
C
            CALL DCOPY( NZ, DCBA(I,I+1), LDDCBA, DCBA(I+1,I+1), LDDCBA )
C
C           Terminate when [ D; B ] = 0, [ D C ] = 0, or D is large
C           enough.
C
            NZ   = NZ - 1
            ABSD = ABS( DCBA(I+1,I+1) )
            NRMB = DNRM2( NZ, DCBA(I+2,I+1), 1 )
            NRMC = DNRM2( NZ, DCBA(I+1,I+2), N1 )
            IF( ABSD.EQ.ZERO .AND. ( NRMB.EQ.ZERO .OR. NRMC.EQ.ZERO ) )
     $            THEN 
               NZ = 0
               GO TO 20
            END IF
            MAXA = DLANGE( 'MAX', NZ, NZ, DCBA(I+2,I+2), LDDCBA, DWORK )
            IF( ABSD*( ONE + MAXA ).GT.TOLDEF*NRMB*NRMC ) THEN
               GO TO 20
            END IF
   10    CONTINUE
C
         I = N
C
   20    CONTINUE
C
C        Move the results in the leading positions.
C
         JF = 1
C
         DO 30 J = I + 1, N1
            CALL DCOPY( NZ+1, DCBA(I+1,J), 1, DCBA(1,JF), 1 )
            JF = JF + 1
   30    CONTINUE
C
         IF( DESCR ) THEN
            JF = 1
C
            DO 40 J = I + 1, N
               CALL DCOPY( NZ, E(I+1,J), 1, E(1,JF), 1 )
               JF = JF + 1
   40       CONTINUE
C
         END IF
C
      END IF
C
      G = G*DCBA(1,1)
      DWORK(1) = DBLE( MAXWRK )
C
      RETURN
C *** Last line of TG01OD ***
      END
