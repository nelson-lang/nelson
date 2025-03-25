      SUBROUTINE MB01KD( UPLO, TRANS, N, K, ALPHA, A, LDA, B, LDB, BETA,
     $                   C, LDC, INFO )
C
C     PURPOSE
C
C     To perform one of the skew-symmetric rank 2k operations
C
C         C := alpha*A*B' - alpha*B*A' + beta*C,
C
C     or
C
C         C := alpha*A'*B - alpha*B'*A + beta*C,
C
C     where alpha and beta are scalars, C is a real N-by-N skew-
C     symmetric matrix and A, B are N-by-K matrices in the first case
C     and K-by-N matrices in the second case.
C
C     This is a modified version of the vanilla implemented BLAS
C     routine DSYR2K written by Jack Dongarra, Iain Duff,
C     Jeremy Du Croz and Sven Hammarling.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     UPLO    CHARACTER*1
C             Specifies whether the upper or lower triangular part of
C             the array C is to be referenced, as follows:
C             = 'U':  only the strictly upper triangular part of C is to
C                     be referenced;
C             = 'L':  only the striclty lower triangular part of C is to
C                     be referenced.
C
C     TRANS   CHARACTER*1
C             Specifies the operation to be performed, as follows:
C             = 'N':         C := alpha*A*B' - alpha*B*A' + beta*C;
C             = 'T' or 'C':  C := alpha*A'*B - alpha*B'*A + beta*C.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrix C.  N >= 0.
C
C     K       (input) INTEGER
C             If TRANS = 'N' the number of columns of A and B; and if
C             TRANS = 'T' or TRANS = 'C' the number of rows of A and B.
C             K >= 0.
C
C     ALPHA   (input) DOUBLE PRECISION
C             The scalar alpha. If alpha is zero, or N <= 1, or K = 0,
C             A and B are not referenced.
C
C     A       (input)  DOUBLE PRECISION array, dimension (LDA,KA),
C             where KA is K when TRANS = 'N', and is N otherwise.
C             On entry with TRANS = 'N', the leading N-by-K part of
C             of this array must contain the matrix A.
C             On entry with TRANS = 'T' or TRANS = 'C', the leading
C             K-by-N part of this array must contain the matrix A.
C
C     LDA     INTEGER
C             The leading dimension of the array A.
C             LDA >= MAX(1,N),  if TRANS = 'N';
C             LDA >= MAX(1,K),  if TRANS = 'T' or TRANS = 'C'.
C
C     B       (input)  DOUBLE PRECISION array, dimension (LDB,KB),
C             where KB is K when TRANS = 'N', and is N otherwise.
C             On entry with TRANS = 'N', the leading N-by-K part of
C             of this array must contain the matrix B.
C             On entry with TRANS = 'T' or TRANS = 'C', the leading
C             K-by-N part of this array must contain the matrix B.
C
C     LDB     INTEGER
C             The leading dimension of the array B.
C             LDB >= MAX(1,N),  if TRANS = 'N';
C             LDB >= MAX(1,K),  if TRANS = 'T' or TRANS = 'C'.
C
C     BETA    (input) DOUBLE PRECISION
C             The scalar beta. If beta is zero C need not be set before
C             entry.
C
C     C       (input/output)  DOUBLE PRECISION array, dimension (LDC,N)
C             On entry with UPLO = 'U', the leading N-by-N part of this
C             array must contain the strictly upper triangular part of
C             the matrix C. The lower triangular part of this array is
C             not referenced.
C             On entry with UPLO = 'L', the leading N-by-N part of this
C             array must contain the strictly lower triangular part of
C             the matrix C. The upper triangular part of this array is
C             not referenced.
C             On exit with UPLO = 'U', the leading N-by-N part of this
C             array contains the strictly upper triangular part of the
C             updated matrix C.
C             On exit with UPLO = 'L', the leading N-by-N part of this
C             array contains the strictly lower triangular part of the
C             updated matrix C.
C
C     LDC     INTEGER
C             The leading dimension of the array C.  LDC >= MAX(1,N)
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value.
C
C     NUMERICAL ASPECTS
C
C     Though being almost identical with the vanilla implementation
C     of the BLAS routine DSYR2K the performance of this routine could
C     be significantly lower in the case of vendor supplied, highly
C     optimized BLAS.
C
C     CONTRIBUTORS
C
C     D. Kressner (Technical Univ. Berlin, Germany) and
C     P. Benner (Technical Univ. Chemnitz, Germany), December 2003.
C
C     REVISIONS
C
C     V. Sima, Jan. 2010 (SLICOT version of the HAPACK routine DSKR2K).
C
C     KEYWORDS
C
C     Elementary matrix operations,
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0 )
C     .. Scalar Arguments ..
      CHARACTER         UPLO, TRANS
      INTEGER           INFO, K, LDA, LDB, LDC, N
      DOUBLE PRECISION  ALPHA, BETA
C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), B(LDB,*), C(LDC,*)
C     .. Local Scalars ..
      LOGICAL           LUP, LTRAN
      INTEGER           I, J, L
      DOUBLE PRECISION  TEMP1, TEMP2
C     .. External Functions ..
      LOGICAL           LSAME
      EXTERNAL          LSAME
C     .. External Subroutines ..
      EXTERNAL          XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         MAX
C
C     .. Executable Statements ..
C
C     Decode the scalar input parameters.
C
      INFO  = 0
      LUP   = LSAME( UPLO,  'U' )
      LTRAN = LSAME( TRANS, 'T' ) .OR. LSAME( TRANS, 'C' )
C
C     Check the scalar input parameters.
C
      IF ( .NOT.( LUP .OR. LSAME( UPLO, 'L' ) ) ) THEN
         INFO = -1
      ELSE IF ( .NOT.( LTRAN .OR. LSAME( TRANS, 'N' ) ) ) THEN
         INFO = -2
      ELSE IF ( N.LT.0 ) THEN
         INFO = -3
      ELSE IF ( K.LT.0 ) THEN
         INFO = -4
      ELSE IF ( ( .NOT.LTRAN .AND. LDA.LT.N ) .OR. LDA.LT.1 .OR.
     $          (      LTRAN .AND. LDA.LT.K ) ) THEN
         INFO = -7
      ELSE IF ( ( .NOT.LTRAN .AND. LDB.LT.N ) .OR. LDB.LT.1 .OR.
     $          (      LTRAN .AND. LDB.LT.K ) ) THEN
         INFO = -9
      ELSE IF ( LDC.LT.MAX( 1, N ) ) THEN
         INFO = -12
      END IF
C
C     Return if there were illegal values.
C
      IF ( INFO.NE.0 ) THEN
         CALL XERBLA( 'MB01KD', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF ( ( N.LE.1 ) .OR.
     $   ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
C
C     Special case ALPHA = 0.
C
      IF ( ALPHA.EQ.ZERO ) THEN
         IF ( LUP ) THEN
            IF ( BETA.EQ.ZERO ) THEN
               DO 20  J = 2, N
                  DO 10  I = 1, J-1
                     C(I,J) = ZERO
   10             CONTINUE
   20          CONTINUE
            ELSE
               DO 40  J = 2, N
                  DO 30  I = 1, J-1
                     C(I,J) = BETA * C(I,J)
   30             CONTINUE
   40          CONTINUE
            END IF
         ELSE
            IF( BETA.EQ.ZERO ) THEN
               DO 60  J = 1, N-1
                  DO 50  I = J+1, N
                     C(I,J) = ZERO
   50             CONTINUE
   60          CONTINUE
            ELSE
               DO 80  J = 1, N-1
                  DO 70  I = J+1, N
                     C(I,J) = BETA * C(I,J)
   70             CONTINUE
   80          CONTINUE
            END IF
         END IF
         RETURN
      END IF
C
C     Normal case.
C
      IF ( .NOT.LTRAN ) THEN
C
C        Update C := alpha*A*B' - alpha*B*A' + beta*C.
C
         IF ( LUP ) THEN
            DO 130  J = 2, N
               IF ( BETA.EQ.ZERO ) THEN
                  DO 90  I = 1, J-1
                     C(I,J) = ZERO
   90             CONTINUE
               ELSE IF ( BETA.NE.ONE ) THEN
                  DO 100  I = 1, J-1
                     C(I,J) = BETA * C(I,J)
  100             CONTINUE
               END IF
               DO 120  L = 1, K
                  IF ( ( A(J,L).NE.ZERO ) .OR.
     $                 ( B(J,L).NE.ZERO ) ) THEN
                     TEMP1 = ALPHA * B(J,L)
                     TEMP2 = ALPHA * A(J,L)
                     DO 110  I = 1, J-1
                        C(I,J) = C(I,J) + A(I,L)*TEMP1 - B(I,L)*TEMP2
  110                CONTINUE
                  END IF
  120          CONTINUE
  130       CONTINUE
         ELSE
            DO 180  J = 1, N-1
               IF ( BETA.EQ.ZERO ) THEN
                  DO 140  I = J+1, N
                     C(I,J) = ZERO
  140             CONTINUE
               ELSE IF ( BETA.NE.ONE ) THEN
                  DO 150  I = J+1, N
                     C(I,J) = BETA * C(I,J)
  150             CONTINUE
               END IF
               DO 170  L = 1, K
                  IF ( ( A(J,L).NE.ZERO ) .OR.
     $                 ( B(J,L).NE.ZERO ) ) THEN
                     TEMP1 = ALPHA * B(J,L)
                     TEMP2 = ALPHA * A(J,L)
                     DO 160  I = J+1, N
                        C(I,J) = C(I,J) + A(I,L)*TEMP1 - B(I,L)*TEMP2
  160                CONTINUE
                  END IF
  170          CONTINUE
  180       CONTINUE
         END IF
      ELSE
C
C        Update C := alpha*A'*B - alpha*B'*A + beta*C.
C
         IF ( LUP ) THEN
            DO 210  J = 2, N
               DO 200  I = 1, J-1
                  TEMP1 = ZERO
                  TEMP2 = ZERO
                  DO 190  L = 1, K
                     TEMP1 = TEMP1 + A(L,I)*B(L,J)
                     TEMP2 = TEMP2 + B(L,I)*A(L,J)
  190             CONTINUE
                  IF ( BETA.EQ.ZERO ) THEN
                     C(I,J) = ALPHA*TEMP1 - ALPHA*TEMP2
                  ELSE
                     C(I,J) = BETA*C(I,J) + ALPHA*TEMP1 - ALPHA*TEMP2
                  END IF
  200          CONTINUE
  210       CONTINUE
         ELSE
            DO 240  J = 1, N-1
               DO 230  I = J+1, N
                  TEMP1 = ZERO
                  TEMP2 = ZERO
                  DO 220, L = 1, K
                     TEMP1 = TEMP1 + A(L,I)*B(L,J)
                     TEMP2 = TEMP2 + B(L,I)*A(L,J)
  220             CONTINUE
                  IF ( BETA.EQ.ZERO ) THEN
                     C(I,J) = ALPHA*TEMP1 - ALPHA*TEMP2
                  ELSE
                     C(I,J) = BETA*C(I,J) + ALPHA*TEMP1 - ALPHA*TEMP2
                  END IF
  230          CONTINUE
  240       CONTINUE
         END IF
      END IF
      RETURN
C *** Last line of MB01KD ***
      END
