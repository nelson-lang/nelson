      SUBROUTINE TG01OZ( JOBE, N, DCBA, LDDCBA, E, LDE, NZ, G, TOL,
     $                   ZWORK, LZWORK, INFO )
C
C     PURPOSE
C
C     To compute for a single-input single-output descriptor system,
C     given by the system matrix with complex elements
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
C     DCBA    (input/output) COMPLEX*16 array, dimension (LDDCBA,N+1)
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
C     E       (input/output) COMPLEX*16 array, dimension (LDE,*)
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
C     G       (output) COMPLEX*16
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
C     ZWORK   COMPLEX*16 array, dimension (LZWORK)
C             On exit, if INFO = 0, ZWORK(1) returns the optimal value
C             of LZWORK.
C             On exit, if INFO = -11, ZWORK(1) returns the minimum value
C             of LZWORK.
C
C     LZWORK  INTEGER
C             The length of the array ZWORK.
C             LZWORK >= 2*N+1,  if JOBE = 'G';
C             LZWORK >=   N+1,  if JOBE = 'I'.
C             For good performance when JOBE = 'G', LZWORK should be
C             larger. Specifically,
C                LZWORK >= MAX( N*NB(ZGEQRF), (N+1)*NB(ZUNMQR) ),
C             where NB(X) is the optimal block sizes for the LAPACK
C             Library routine X.
C
C             If LZWORK = -1, then a workspace query is assumed;
C             the routine only calculates the optimal size of the
C             ZWORK array, returns this value as the first entry of
C             the ZWORK array, and no error message related to LZWORK
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
      COMPLEX*16        CONE, CZERO
      PARAMETER         ( CONE  = ( 1.0D+0, 0.0D+0 ),
     $                    CZERO = ( 0.0D+0, 0.0D+0 ) )
C     .. Scalar Arguments ..
      CHARACTER         JOBE
      INTEGER           INFO, LDDCBA, LDE, LZWORK, N, NZ
      DOUBLE PRECISION  TOL
      COMPLEX*16        G
C     .. Array Arguments ..
      COMPLEX*16        DCBA(LDDCBA,*), E(LDE,*), ZWORK(*)
C     .. Local Scalars ..
      CHARACTER         JOBT
      LOGICAL           DESCR, LQUERY
      INTEGER           I, IMAX, ITAU, IWRK, J, JF, MAXWRK, MINWRK, N1,
     $                  NC
      DOUBLE PRECISION  ABSD, MAXA, NRMB, NRMC, TOLDEF
      COMPLEX*16        TAU
C     .. Local Arrays ..
      DOUBLE PRECISION  DWORK(1)
C     .. External Functions ..
      LOGICAL           LSAME
      INTEGER           IZAMAX
      DOUBLE PRECISION  DLAMCH, DZNRM2, ZLANGE
      COMPLEX*16        ZLADIV
      EXTERNAL          DLAMCH, DZNRM2, IZAMAX, LSAME, ZLADIV, ZLANGE
C     .. External Subroutines ..
      EXTERNAL          TG01OB, XERBLA, ZCOPY, ZGEQRF, ZLARF, ZLARFG,
     $                  ZLASET, ZSWAP, ZUNMQR
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, DCMPLX, DCONJG, INT, MAX, MIN
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
         LQUERY = LZWORK.EQ.-1
         IF ( LQUERY ) THEN
            IF ( DESCR ) THEN
               CALL ZGEQRF( N, N, E, LDE, DCBA, ZWORK, -1, INFO )
               MAXWRK = MAX( MAXWRK, INT( ZWORK(1) ) )
               CALL ZUNMQR( 'Left', 'Conjugate', N, N1, N, E, LDE, DCBA,
     $                      DCBA, LDDCBA, ZWORK, -1, INFO )
               ZWORK(1) = DCMPLX( MAX( MAXWRK, INT( ZWORK(1) ) ) )
            ELSE
               ZWORK(1) = DCMPLX( MAXWRK )
            END IF
            RETURN
         ELSE IF( LZWORK.LT.MINWRK ) THEN
            ZWORK(1) = DCMPLX( MINWRK )
            INFO = -11
         END IF
      END IF
C
      IF( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'TG01OZ', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      NZ = N
      IF( N.EQ.0 ) THEN
         G = DCBA(1,1)
         ZWORK(1) = CONE
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
      G    = CONE
      MAXA = ZLANGE( 'MAX', N, N, DCBA(2,2), LDDCBA, DWORK )
      NRMB = DZNRM2( N, DCBA(2,1), 1 )
      NRMC = DZNRM2( N, DCBA(1,2), N1 )
C
      IF( ABS( DCBA(1,1) )*( ONE + MAXA ).LE.TOLDEF*NRMB*NRMC ) THEN
         IF( DESCR ) THEN
C
C           Triangularize E.
C           Workspace: need   2*N + 1;
C                      prefer MAX( N*NB(ZGEQRF), (N+1)*NB(ZUNMQR) ).
C
            ITAU = 1
            IWRK = ITAU + N
            CALL ZGEQRF( N, N, E, LDE, ZWORK(ITAU), ZWORK(IWRK),
     $                   LZWORK-N, INFO )
            MAXWRK = MAX( MAXWRK, INT( ZWORK(IWRK) ) )
            CALL ZUNMQR( 'Left', 'Conjugate', N, N1, N, E, LDE,
     $                   ZWORK(ITAU), DCBA(2,1), LDDCBA, ZWORK(IWRK),
     $                   LZWORK-N, INFO )
            MAXWRK = MAX( MAXWRK, INT( ZWORK(IWRK) ) )
            IF( N.GT.1 )
     $         CALL ZLASET( 'Lower', N-1, N-1, CZERO, CZERO, E(2,1),
     $                      LDE )
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
               IF( DCBA(I+1,I).EQ.CZERO ) THEN
C
C                 Bring the largest entry of B in the first position.
C
                  IMAX = IZAMAX( NZ, DCBA(I+1,I), 1 ) + I
                  CALL ZSWAP( NC, DCBA(I+1,I), LDDCBA, DCBA(IMAX,I),
     $                        LDDCBA )
                  CALL ZSWAP( NC, DCBA(I,I+1), 1, DCBA(I,IMAX), 1 )
               END IF
C
C              Find and apply the Householder transformation setting
C              to zero all entries of the current B, but the first.
C
               CALL ZLARFG( NZ, DCBA(I+1,I), DCBA(MIN(I+2,N1),I), 1,
     $                      TAU )
               G = G*DCBA(I+1,I)
               DCBA(I+1,I) = CONE
               CALL ZLARF( 'Left',  NZ, NZ, DCBA(I+1,I), 1,
     $                      DCONJG( TAU ), DCBA(I+1,I+1), LDDCBA,
     $                      ZWORK )
               CALL ZLARF( 'Right', NC, NZ, DCBA(I+1,I), 1, TAU,
     $                      DCBA(I,I+1), LDDCBA, ZWORK )
            ELSE
               CALL TG01OB( JOBT, NZ, DCBA(I,I), LDDCBA, E(I,I), LDE,
     $                      INFO )
               G = ZLADIV( G*DCBA(I+1,I), E(I,I) )
            END IF
C
C           Reduce DCBA (delete the second row and first column of the
C           current DCBA matrix). Actually, the first row is copied over
C           the second, and then the first row and column are removed.
C
            CALL ZCOPY( NZ, DCBA(I,I+1), LDDCBA, DCBA(I+1,I+1), LDDCBA )
C
C           Terminate when [ D; B ] = 0, [ D C ] = 0, or D is large
C           enough.
C
            NZ   = NZ - 1
            ABSD = ABS( DCBA(I+1,I+1) )
            NRMB = DZNRM2( NZ, DCBA(I+2,I+1), 1 )
            NRMC = DZNRM2( NZ, DCBA(I+1,I+2), N1 )
            IF( ABSD.EQ.ZERO .AND. ( NRMB.EQ.ZERO .OR. NRMC.EQ.ZERO ) )
     $            THEN 
               NZ = 0
               GO TO 20
            END IF
            MAXA = ZLANGE( 'MAX', NZ, NZ, DCBA(I+2,I+2), LDDCBA, DWORK )
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
            CALL ZCOPY( NZ+1, DCBA(I+1,J), 1, DCBA(1,JF), 1 )
            JF = JF + 1
   30    CONTINUE
C
         IF( DESCR ) THEN
            JF = 1
C
            DO 40 J = I + 1, N
               CALL ZCOPY( NZ, E(I+1,J), 1, E(1,JF), 1 )
               JF = JF + 1
   40       CONTINUE
C
         END IF
C
      END IF
C
      G = G*DCBA(1,1)
      ZWORK(1) = DCMPLX( MAXWRK )
C
      RETURN
C *** Last line of TG01OZ ***
      END
