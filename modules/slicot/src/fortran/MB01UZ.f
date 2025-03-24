      SUBROUTINE MB01UZ( SIDE, UPLO, TRANS, M, N, ALPHA, T, LDT, A, LDA,
     $                   ZWORK, LZWORK, INFO )
C
C     PURPOSE
C
C     To compute one of the matrix products
C
C       T : = alpha*op( T ) * A, or T : = alpha*A * op( T ),
C
C     where alpha is a scalar, A is an M-by-N matrix, T is a triangular
C     matrix, and op( T ) is one of
C
C        op( T ) = T,  or  op( T ) = T',  the transpose of T, or
C        op( T ) = conj(T'),              the conjugate transpose of T.
C
C     A block-row/column algorithm is used, if possible. The result
C     overwrites the array T.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     SIDE    CHARACTER*1
C             Specifies whether the triangular matrix T appears on the
C             left or right in the matrix product, as follows:
C             = 'L':  T := alpha * op( T ) * A;
C             = 'R':  T := alpha * A * op( T ).
C
C     UPLO    CHARACTER*1.
C             Specifies whether the matrix T is an upper or lower
C             triangular matrix, as follows:
C             = 'U':  T is an upper triangular matrix;
C             = 'L':  T is a lower triangular matrix.
C
C     TRANS   CHARACTER*1
C             Specifies the form of op( T ) to be used in the matrix
C             multiplication as follows:
C             = 'N':  op( T ) = T;
C             = 'T':  op( T ) = T';
C             = 'C':  op( T ) = conj(T').
C
C     Input/Output Parameters
C
C     M       (input) INTEGER
C             The number of rows of the matrix A.  M >= 0.
C
C     N       (input) INTEGER
C             The number of columns of the matrix A.  N >= 0.
C
C     ALPHA   (input) COMPLEX*16
C             The scalar alpha. When alpha is zero then T and A need not
C             be set before entry.
C
C     T       (input/output) COMPLEX*16 array, dimension
C             (LDT,max(K,N)), when SIDE = 'L', and
C             (LDT,K),        when SIDE = 'R',
C             where K is M if SIDE = 'L' and is N if SIDE = 'R'.
C             On entry with UPLO = 'U', the leading K-by-K upper
C             triangular part of this array must contain the upper
C             triangular matrix T. The elements below the diagonal
C             do not need to be zero.
C             On entry with UPLO = 'L', the leading K-by-K lower
C             triangular part of this array must contain the lower
C             triangular matrix T. The elements above the diagonal
C             do not need to be zero.
C             On exit, the leading M-by-N part of this array contains
C             the corresponding product defined by SIDE, UPLO, and
C             TRANS.
C
C     LDT     INTEGER
C             The leading dimension of the array T.
C             LDT >= max(1,M),    if SIDE = 'L';
C             LDT >= max(1,M,N),  if SIDE = 'R'.
C
C     A       (input) COMPLEX*16 array, dimension (LDA,N)
C             The leading M-by-N part of this array must contain the
C             matrix A.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= max(1,M).
C
C     Workspace
C
C     ZWORK   COMPLEX*16 array, dimension (LZWORK)
C             On exit, if INFO = 0,  ZWORK(1)  returns the optimal value
C             of LZWORK.
C             On exit, if  INFO = -12,  ZWORK(1)  returns the minimum
C             value of LZWORK.
C
C     LZWORK  The length of the array ZWORK.
C             LZWORK >= 1, if alpha =  0 or MIN(M,N) = 0;
C             LZWORK >= M, if SIDE  = 'L';
C             LZWORK >= N, if SIDE  = 'R'.
C             For good performance, LZWORK should be larger.
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
C     A block-row/column size is found based on the available workspace.
C     BLAS 3 gemm and trmm are used if possible.
C
C     CONTRIBUTORS
C
C     V. Sima, June 2021.
C
C     REVISIONS
C
C     V. Sima, August 2021.
C
C     KEYWORDS
C
C     Elementary matrix operations.
C
C     ******************************************************************
C
C     .. Parameters ..
      COMPLEX*16        ZERO, ONE
      PARAMETER         ( ZERO = ( 0.0D0, 0.0D0 ),
     $                    ONE  = ( 1.0D0, 0.0D0 ) )
C     .. Scalar Arguments ..
      CHARACTER         SIDE, TRANS, UPLO
      INTEGER           INFO, LDA, LDT, LZWORK, M, N
      COMPLEX*16        ALPHA
C     .. Array Arguments ..
      COMPLEX*16        A(LDA,*), T(LDT,*), ZWORK(*)
C     .. Local Scalars ..
      CHARACTER         SKEW, TRANC, UPLOC
      LOGICAL           FILLIN, LQUERY, LSIDE, LTRAN, LUPLO, TTRAN
      INTEGER           BL, I, II, IJ, J, K, L, MN, NB, NC, NR, WRKMIN,
     $                  WRKOPT
      COMPLEX*16        TEMP
C     .. External Functions ..
      LOGICAL           LSAME
      EXTERNAL          LSAME
C     .. External Subroutines ..
      EXTERNAL          MA02EZ, XERBLA, ZCOPY, ZGEMM, ZGEMV, ZGEQRF,
     $                  ZLACGV, ZLACPY, ZLASET, ZTRMM
C     .. Intrinsic Functions ..
      INTRINSIC         DBLE, DCONJG, INT, MAX, MIN
C
C     .. Executable Statements ..
C
C     Decode and test the input scalar arguments.
C
      INFO  = 0
      LSIDE = LSAME( SIDE,  'L' )
      LUPLO = LSAME( UPLO,  'U' )
      TTRAN = LSAME( TRANS, 'T' )
      LTRAN = LSAME( TRANS, 'C' ) .OR. TTRAN
      IF ( LSIDE ) THEN
         K = M
         L = N
      ELSE
         K = N
         L = M
      END IF
      MN = MIN( M, N )
C
C     Ensure that at least two rows or columns of A fit into the
C     workspace, if optimal workspace is required.
C
      WRKMIN = 1
      IF ( ALPHA.NE.ZERO .AND. MN.GT.0 )
     $   WRKMIN = MAX( WRKMIN, K )
      LQUERY = LZWORK.EQ.-1
C
      IF ( (      .NOT.LSIDE ).AND.( .NOT.LSAME( SIDE,  'R' ) ) ) THEN
         INFO = -1
      ELSE IF ( ( .NOT.LUPLO ).AND.( .NOT.LSAME( UPLO,  'L' ) ) ) THEN
         INFO = -2
      ELSE IF ( ( .NOT.LTRAN ).AND.( .NOT.LSAME( TRANS, 'N' ) ) ) THEN
         INFO = -3
      ELSE IF ( M.LT.0 ) THEN
         INFO = -4
      ELSE IF ( N.LT.0 ) THEN
         INFO = -5
      ELSE IF ( LDT.LT.MAX( 1, M ) .OR. ( .NOT.LSIDE .AND. LDT.LT.N ) )
     $      THEN
         INFO = -8
      ELSE IF ( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -10
      ELSE IF( LQUERY ) THEN
         IF ( ALPHA.NE.ZERO .AND. MN.GT.0 ) THEN
            CALL ZGEQRF( M, MAX( M,N ), A, LDA, ZWORK, ZWORK, -1, INFO )
            WRKOPT = MAX( WRKMIN, 2*L, INT( ZWORK(1) ) )
            ZWORK(1) = DBLE( WRKOPT )
         ELSE
            ZWORK(1) = ONE
         END IF
         RETURN
      ELSE IF ( LZWORK.LT.WRKMIN ) THEN
         ZWORK(1) = DBLE( WRKMIN )
         INFO = -12
      END IF
C
      IF ( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'MB01UZ', -INFO )
         RETURN
      END IF
C
C     Quick return, if possible.
C
      IF ( MN.EQ.0 )
     $   RETURN
C
      IF ( ALPHA.EQ.ZERO ) THEN
C
C        Set T to zero and return.
C
         CALL ZLASET( 'Full', M, N, ZERO, ZERO, T, LDT )
         RETURN
      END IF
C
C     Set the panel (block-row/column) size NB.
C
      NB = MAX( 1, MIN( K, INT( LZWORK/L ) ) )
C
      IF ( LZWORK.GE.M*N ) THEN
C
C        Enough workspace for a fast BLAS 3 calculation.
C        Save A in the workspace and compute one of the matrix products
C          T : = alpha * op( triu( T ) ) * A, or
C          T : = alpha * A * op( triu( T ) ),
C        involving the upper/lower triangle of T.
C
         CALL ZLACPY( 'All', M, N, A, LDA, ZWORK, M )
         CALL ZTRMM(  SIDE, UPLO, TRANS, 'NonUnit', M, N, ALPHA, T, LDT,
     $                ZWORK, M )
         CALL ZLACPY( 'All', M, N, ZWORK, M, T, LDT )
C
      ELSE IF ( NB.GT.1 ) THEN
C
C        Use BLAS 3 calculations in a loop. BL is the number of panels.
C
C        If UPLO = 'L' and TRANS <> 'N', change the format so that to
C        correspond to UPLO = 'U' and TRANS = 'N'.
C        If UPLO = 'U' and TRANS <> 'N', change the format so that to
C        correspond to UPLO = 'L' and TRANS = 'N'.
C
         IF ( LTRAN ) THEN
            CALL MA02EZ( UPLO, TRANS, 'General', K, T, LDT )
            IF ( LUPLO ) THEN 
               UPLOC = 'Lower'
            ELSE
               UPLOC = 'Upper'
            END IF
            TRANC = 'NoTran'
            LUPLO = .NOT.LUPLO
            LTRAN = .NOT.LTRAN
         ELSE
            UPLOC = UPLO
            TRANC = TRANS
         END IF
C
         BL = MAX( 1, INT( K/NB ) )
         J  = MIN( K, NB*BL )
C
         IF ( LSIDE ) THEN
C
            IF ( LUPLO ) THEN
C
C              Compute the last rows.
C
               IF ( J.EQ.M ) THEN
                  NR = NB
                  II = M  - NB + 1
                  BL = BL - 1
               ELSE
                  NR = M - J
                  II = J + 1
               END IF
               CALL ZLACPY( 'All', NR, N, A(II,1), LDA, ZWORK, NR )
               CALL ZTRMM(  SIDE, UPLOC, TRANC, 'NonUnit', NR, N, ALPHA,
     $                      T(II,II), LDT, ZWORK, NR )
               CALL ZLACPY( 'All', NR, N, ZWORK, NR, T(II,1), LDT )
C
               DO 10 I = 1, BL
                  IJ = II
                  II = II - NB
                  CALL ZLACPY( 'All', NB, N, A(II,1), LDA, ZWORK, NB )
                  CALL ZTRMM(  SIDE, UPLOC, TRANC, 'NonUnit', NB, N,
     $                         ALPHA, T(II,II), LDT, ZWORK, NB )
                  CALL ZGEMM(  TRANC, 'NoTrans', NB, N, M-IJ+1, ALPHA,
     $                         T(II,IJ), LDT, A(IJ,1), LDA, ONE, ZWORK,
     $                         NB )
                  CALL ZLACPY( 'All', NB, N, ZWORK, NB, T(II,1), LDT )
   10          CONTINUE
C
            ELSE
C
C              Compute the first rows.
C
               IF ( J.EQ.M ) THEN
                  NR = NB
                  BL = BL - 1
               ELSE
                  NR = M - J
               END IF
               CALL ZLACPY( 'All', NR, N, A, LDA, ZWORK, NR )
               CALL ZTRMM(  SIDE, UPLOC, TRANC, 'NonUnit', NR, N, ALPHA,
     $                      T, LDT, ZWORK, NR )
               CALL ZLACPY( 'All', NR, N, ZWORK, NR, T, LDT )
               II = NR + 1
C
               DO 20 I = 1, BL
                  CALL ZLACPY( 'All', NB, N, A(II,1), LDA, ZWORK, NB )
                  CALL ZTRMM(  SIDE, UPLOC, TRANC, 'NonUnit', NB, N,
     $                         ALPHA, T(II,II), LDT, ZWORK, NB )
                  CALL ZGEMM(  TRANC, 'NoTrans', NB, N, II-1, ALPHA,
     $                         T(II,1), LDT, A, LDA, ONE, ZWORK, NB )
                  CALL ZLACPY( 'All', NB, N, ZWORK, NB, T(II,1), LDT )
                  II = II + NB
   20          CONTINUE
C
            END IF
C
         ELSE
C
            IF ( LUPLO ) THEN
C
C              Compute the first columns.
C
               II = 1
               IF ( J.EQ.N ) THEN
                  NC = NB
                  BL = BL - 1
               ELSE
                  NC = N - J
               END IF
               CALL ZLACPY( 'All', M, NC, A, LDA, ZWORK, M )
               CALL ZTRMM(  SIDE, UPLOC, TRANC, 'NonUnit', M, NC, ALPHA,
     $                      T, LDT, ZWORK, M )
               CALL ZLACPY( 'All', M, NC, ZWORK, M, T, LDT )
               II = II + NC
C
               DO 30 I = 1, BL
                  IJ = II - 1
                  CALL ZLACPY( 'All', M, NB, A(1,II), LDA, ZWORK, M )
                  CALL ZTRMM(  SIDE, UPLOC, TRANC, 'NonUnit', M, NB,
     $                         ALPHA, T(II,II), LDT, ZWORK, M )
                  CALL ZGEMM(  TRANC, 'NoTrans', M, NB, IJ, ALPHA, A,
     $                         LDA, T(1,II), LDT, ONE, ZWORK, M )
                  CALL ZLACPY( 'All', M, NB, ZWORK, M, T(1,II), LDT )
                  II = II + NB
   30          CONTINUE
C
            ELSE
C
C              Compute the last columns.
C
               IF ( J.EQ.N ) THEN
                  NC = NB
                  II = N  - NB + 1
                  BL = BL - 1
               ELSE
                  NC = N - J
                  II = J + 1
               END IF
               CALL ZLACPY( 'All', M, NC, A(1,II), LDA, ZWORK, M )
               CALL ZTRMM(  SIDE, UPLOC, TRANC, 'NonUnit', M, NC, ALPHA,
     $                      T(II,II), LDT, ZWORK, M )
               CALL ZLACPY( 'All', M, NC, ZWORK, M, T(1,II), LDT )
C
               DO 40 I = 1, BL
                  IJ = II
                  II = II - NB
                  CALL ZLACPY( 'All', M, NB, A(1,II), LDA, ZWORK, M )
                  CALL ZTRMM(  SIDE, UPLOC, TRANC, 'NonUnit', M, NB,
     $                         ALPHA, T(II,II), LDT, ZWORK, M )
                  CALL ZGEMM(  TRANC, 'NoTrans', M, NB, NC, ALPHA,
     $                         A(1,IJ), LDA, T(IJ,II), LDT, ONE, ZWORK,
     $                         M )
                  CALL ZLACPY( 'All', M, NB, ZWORK, M, T(1,II), LDT )
                  NC = NC + NB
   40          CONTINUE
C
            END IF
C
         END IF
C
      ELSE
C
C        Use BLAS 2 calculations in a loop.
C
         FILLIN = LTRAN .AND. ( ( LSIDE .AND.      LUPLO ) .OR.
     $                     ( .NOT.LSIDE .AND. .NOT.LUPLO ) )
         IF ( FILLIN ) THEN
C
C           Fill-in the other part of T.
C
            IF ( TTRAN ) THEN
               SKEW = 'NotSkew'
            ELSE
               SKEW = 'General'
            END IF
            CALL MA02EZ( UPLO, TRANS, SKEW, K, T, LDT )
         END IF
C
         IF ( LSIDE ) THEN
C
            IF ( LUPLO ) THEN
C
               IF ( .NOT.LTRAN ) THEN
                  TEMP = DCONJG( ALPHA )
C
                  DO 50 I = 1, M
                     CALL ZCOPY(  M-I+1, T(I,I), LDT, ZWORK, 1 )
                     CALL ZLACGV( M-I+1, ZWORK, 1 )
                     CALL ZGEMV(  'CTrans', M-I+1, N, TEMP, A(I,1), LDA,
     $                            ZWORK, 1, ZERO, T(I,1), LDT )
                     CALL ZLACGV( N, T(I,1), LDT )
   50             CONTINUE
C
               ELSE IF ( TTRAN ) THEN
C
                  DO 60 I = 1, M
                     CALL ZCOPY( I, T(I,1), LDT, ZWORK, 1 )
                     CALL ZGEMV( TRANS, I, N, ALPHA, A, LDA, ZWORK, 1,
     $                           ZERO, T(I,1), LDT )
   60             CONTINUE
C
               ELSE
                  TEMP = DCONJG( ALPHA )
C
                  DO 70 I = 1, M
                     CALL ZCOPY(  I, T(I,1), LDT, ZWORK, 1 )
                     CALL ZLACGV( I, ZWORK, 1 )
                     CALL ZGEMV(  TRANS, I, N, TEMP, A, LDA, ZWORK, 1,
     $                            ZERO, T(I,1), LDT )
                     CALL ZLACGV( N, T(I,1), LDT )
   70             CONTINUE
C
               END IF
C
            ELSE
C
               IF ( .NOT.LTRAN ) THEN
                  TEMP = DCONJG( ALPHA )
C
                  DO 80 I = 1, M
                     CALL ZCOPY(  I, T(I,1), LDT, ZWORK, 1 )
                     CALL ZLACGV( I, ZWORK, 1 )
                     CALL ZGEMV(  'CTrans', I, N, TEMP, A, LDA, ZWORK,
     $                            1, ZERO, T(I,1), LDT )
                     CALL ZLACGV( N, T(I,1), LDT )
   80             CONTINUE
C
               ELSE IF ( TTRAN ) THEN
C
                  DO 90 I = 1, M
                     CALL ZCOPY( M-I+1, T(I,I), 1, ZWORK, 1 )
                     CALL ZGEMV( TRANS, M-I+1, N, ALPHA, A(I,1), LDA,
     $                           ZWORK, 1, ZERO, T(I,1), LDT )
   90             CONTINUE
C
               ELSE
                  TEMP = DCONJG( ALPHA )
C
                  DO 100 I = 1, M
                     CALL ZCOPY(  M-I+1, T(I,I), 1, ZWORK, 1 )
                     CALL ZGEMV(  TRANS, M-I+1, N, TEMP, A(I,1), LDA,
     $                            ZWORK, 1, ZERO, T(I,1), LDT )
                     CALL ZLACGV( N, T(I,1), LDT )
  100             CONTINUE
C
               END IF
C
            END IF
C
         ELSE
C
            IF ( LUPLO ) THEN
C
               IF ( TTRAN ) THEN
C
                  DO 110 I = 1, N
                     CALL ZCOPY( N-I+1, T(I,I), LDT, ZWORK, 1 )
                     CALL ZGEMV( 'NoTran', M, N-I+1, ALPHA, A(1,I), LDA,
     $                           ZWORK, 1, ZERO, T(1,I), 1 )
  110             CONTINUE
C
               ELSE IF ( LTRAN ) THEN
C
                  DO 120 I = 1, N
                     CALL ZCOPY(  N-I+1, T(I,I), LDT, ZWORK, 1 )
                     CALL ZLACGV( N-I+1, ZWORK, 1 )
                     CALL ZGEMV(  'NoTran', M, N-I+1, ALPHA, A(1,I),
     $                            LDA, ZWORK, 1, ZERO, T(1,I), 1 )
  120             CONTINUE
C
               ELSE
C
                  DO 130 I = 1, N
                     CALL ZCOPY( I, T(1,I), 1, ZWORK, 1 )
                     CALL ZGEMV( 'NoTran', M, I, ALPHA, A, LDA, ZWORK,
     $                           1, ZERO, T(1,I), 1 )
  130             CONTINUE
C
               END IF
C
            ELSE
C
               IF ( LTRAN ) THEN
C
                  DO 140 I = 1, N
                     CALL ZCOPY( I, T(1,I), 1, ZWORK, 1 )
                     CALL ZGEMV( 'NoTran', M, I, ALPHA, A, LDA, ZWORK,
     $                           1, ZERO, T(1,I), 1 )
  140             CONTINUE
C
               ELSE
C
                  DO 150 I = 1, N
                     CALL ZCOPY( N-I+1, T(I,I), 1, ZWORK, 1 )
                     CALL ZGEMV( 'NoTran', M, N-I+1, ALPHA, A(1,I), LDA,
     $                           ZWORK, 1, ZERO, T(1,I), 1 )
  150             CONTINUE
C
               END IF
C
            END IF
C
         END IF
C
      END IF
C
      ZWORK(1) = DBLE( MAX( WRKMIN, WRKOPT ) )
      RETURN
C *** Last line of MB01UZ ***
      END
