      SUBROUTINE TB01WX( COMPU, N, M, P, A, LDA, B, LDB, C, LDC, U, LDU,
     $                   DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To reduce the system state matrix A to an upper Hessenberg form
C     by using an orthogonal similarity transformation A <-- U'*A*U and
C     to apply the transformation to the matrices B and C: B <-- U'*B
C     and C <-- C*U.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     COMPU   CHARACTER*1
C             = 'N':  do not compute U;
C             = 'I':  U is initialized to the unit matrix, and the
C                     orthogonal matrix U is returned;
C             = 'U':  U must contain an orthogonal matrix U1 on entry,
C                     and the product U1*U is returned.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the original state-space representation,
C             i.e., the order of the matrix A.  N >= 0.
C
C     M       (input) INTEGER
C             The number of system inputs, or of columns of B.  M >= 0.
C
C     P       (input) INTEGER
C             The number of system outputs, or of rows of C.  P >= 0.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the original state dynamics matrix A.
C             On exit, the leading N-by-N part of this array contains
C             the matrix U' * A * U in Hessenberg form. The elements
C             below the first subdiagonal are set to zero.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     B       (input/output) DOUBLE PRECISION array, dimension (LDB,M)
C             On entry, the leading N-by-M part of this array must
C             contain the input matrix B.
C             On exit, the leading N-by-M part of this array contains
C             the transformed input matrix U' * B.
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
C     U       (input/output) DOUBLE PRECISION array, dimension (LDU,*)
C             On entry, if COMPU = 'U', the leading N-by-N part of this
C             array must contain the given matrix U1. Otherwise, this
C             array need not be set on input.
C             On exit, if COMPU <> 'N', the leading N-by-N part of this
C             array contains the orthogonal transformation matrix used
C             to reduce A to the Hessenberg form (U1*U if COMPU = 'U').
C             If COMPU = 'N', this array is not referenced.
C
C     LDU     INTEGER
C             The leading dimension of the array U.
C             LDU >= 1,        if COMPU =  'N';
C             LDU >= max(1,N), if COMPU <> 'N'.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The length of the array DWORK.  LDWORK >= 1, and if N > 0,
C             LDWORK >= N - 1 + MAX(N,M,P).
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
C     Matrix A is reduced to the Hessenberg form using an orthogonal
C     similarity transformation A <- U'*A*U. Then, the transformation
C     is applied to the matrices B and C: B <-- U'*B and C <-- C*U.
C
C     NUMERICAL ASPECTS
C                                    3      2
C     The algorithm requires about 5N /3 + N (M+P) floating point
C                                              3
C     operations, if COMPU = 'N'. Otherwise, 2N /3 additional operations
C     are needed.
C
C     CONTRIBUTOR
C
C     A. Varga, German Aerospace Center,
C     DLR Oberpfaffenhofen, April 2002.
C
C     REVISIONS
C
C     V. Sima, Dec. 2016.
C
C     KEYWORDS
C
C     Orthogonal transformation, Hessenberg form, similarity
C     transformation.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION ZERO, ONE
      PARAMETER        ( ZERO = 0.0D0, ONE = 1.0D0 )
C     .. Scalar Arguments ..
      CHARACTER        COMPU
      INTEGER          INFO, LDA, LDB, LDC, LDU, LDWORK, M, N, P
C     .. Array Arguments ..
      DOUBLE PRECISION A(LDA,*), B(LDB,*), C(LDC,*), DWORK(*), U(LDU,*)
C     .. Local Scalars ..
      LOGICAL          ILU, LQUERY
      INTEGER          ICOMPU, ITAU, JWORK, MINWRK, WRKOPT
C     .. External Functions ..
      LOGICAL          LSAME
      EXTERNAL         LSAME
C     .. External Subroutines ..
      EXTERNAL         DGEHRD, DLACPY, DORGHR, DORMHR, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC        INT, MAX
C
C     .. Executable Statements ..
C
C     Decode COMPU.
C
      IF( LSAME( COMPU, 'N' ) ) THEN
         ILU = .FALSE.
         ICOMPU = 1
      ELSE IF( LSAME( COMPU, 'U' ) ) THEN
         ILU = .TRUE.
         ICOMPU = 2
      ELSE IF( LSAME( COMPU, 'I' ) ) THEN
         ILU = .TRUE.
         ICOMPU = 3
      ELSE
         ICOMPU = 0
      END IF
C
      INFO = 0
C
C     Check input parameters.
C
      IF( ICOMPU.LE.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( P.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -8
      ELSE IF( LDC.LT.MAX( 1, P ) ) THEN
         INFO = -10
      ELSE IF( LDU.LT.1 .OR. ( ILU .AND. LDU.LT.MAX( 1, N ) ) ) THEN
         INFO = -12
      ELSE
         LQUERY = LDWORK.LT.0
         IF( N.EQ.0 ) THEN
            MINWRK = 1
         ELSE
            MINWRK = N - 1 + MAX( N, M, P )
         END IF
         IF( LQUERY ) THEN
            CALL DGEHRD( N, 1, N, A, LDA, DWORK, DWORK, -1, INFO )
            WRKOPT = MAX( MINWRK, N - 1 + INT( DWORK(1) ) )
            CALL DORMHR( 'Left', 'Transpose', N, M, 1, N, A, LDA,
     $                   DWORK, B, LDB, DWORK, -1, INFO )
            WRKOPT = MAX( WRKOPT, N - 1 + INT( DWORK(1) ) )
            CALL DORMHR( 'Right', 'No transpose', P, N, 1, N, A, LDA,
     $             DWORK, C, LDC, DWORK, -1, INFO )
            WRKOPT = MAX( WRKOPT, N - 1 + INT( DWORK(1) ) )
            IF( ILU ) THEN
               IF( ICOMPU.EQ.3 ) THEN
                  CALL DORGHR( N, 1, N, U, LDU, DWORK, DWORK, -1, INFO )
               ELSE
                  CALL DORMHR( 'Right', 'No transpose', N, N, 1, N, A,
     $                         LDA, DWORK, U, LDU, DWORK, -1, INFO )
               END IF
               WRKOPT = MAX( WRKOPT, N - 1 + INT( DWORK(1) ) )
            END IF
         ELSE IF( LDWORK.LT.MINWRK ) THEN
            INFO = -14
         END IF
      END IF
C
      IF( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'TB01WX', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         DWORK(1) = WRKOPT
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( N.EQ.0 ) THEN
         DWORK(1) = ONE
         RETURN
      END IF
C
C     Reduce A to Hessenberg form using an orthogonal similarity
C     transformation, A <- U'*A*U, and apply the orthogonal
C     transformations to B and C such that B <- U'*B, C <- C*U.
C
C     Workspace:  need   N-1+MAX(N,M,P);
C                 prefer N - 1 + MAX(N,M,P)*NB.
C
      ITAU  = 1
      JWORK = ITAU + N - 1
      CALL DGEHRD( N, 1, N, A, LDA, DWORK(ITAU), DWORK(JWORK),
     $             LDWORK-JWORK+1, INFO )
      WRKOPT = INT( DWORK(JWORK) )+JWORK-1
C
      CALL DORMHR( 'Left', 'Transpose', N, M, 1, N, A, LDA,
     $             DWORK(ITAU), B, LDB, DWORK(JWORK), LDWORK-JWORK+1,
     $             INFO )
      WRKOPT = MAX( WRKOPT, INT( DWORK(JWORK) )+JWORK-1 )
C
      CALL DORMHR( 'Right', 'No transpose', P, N, 1, N, A, LDA,
     $             DWORK(ITAU), C, LDC, DWORK(JWORK), LDWORK-JWORK+1,
     $             INFO )
      WRKOPT = MAX( WRKOPT, INT( DWORK(JWORK) )+JWORK-1 )
C
      IF( ILU ) THEN
         IF( ICOMPU.EQ.3 ) THEN
C
C           Accumulate the transformation in U.
C           Copy Householder vectors to U.
C
            CALL DLACPY( 'Lower', N, N, A, LDA, U, LDU )
C
C           Generate orthogonal matrix in U.
C           Workspace: need 2*N-1, prefer 2*N+(N-1)*NB.
C
            CALL DORGHR( N, 1, N, U, LDU, DWORK( ITAU ), DWORK(JWORK),
     $                   LDWORK-JWORK+1, INFO )
         ELSE
C
C           Apply the transformation to U1.
C
            CALL DORMHR( 'Right', 'No transpose', N, N, 1, N, A, LDA,
     $                   DWORK(ITAU), U, LDU, DWORK(JWORK),
     $                   LDWORK-JWORK+1, INFO )
         END IF
         WRKOPT = MAX( WRKOPT, INT( DWORK(JWORK) )+JWORK-1 )
      END IF
C
      IF( N.GT.2 )
     $   CALL DLASET( 'L', N-2, N-2, ZERO, ZERO, A(3,1), LDA )
C
      DWORK(1) = WRKOPT
C
      RETURN
C *** Last line of TB01WX ***
      END
