      SUBROUTINE MA02AZ( TRANS, JOB, M, N, A, LDA, B, LDB )
C
C     PURPOSE
C
C     To (conjugate) transpose all or part of a two-dimensional complex
C     matrix A into another matrix B.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     TRANS   CHARACTER*1
C             Specifies if a transpose or conjugate transpose operation
C             should be performed as follows:
C             = 'T':  transpose operation;
C             = 'C':  conjugate transpose operation.
C
C     JOB     CHARACTER*1
C             Specifies the part of the matrix A to be transposed into B
C             as follows:
C             = 'U': Upper triangular part;
C             = 'L': Lower triangular part;
C             Otherwise:  All of the matrix A.
C
C     Input/Output Parameters
C
C     M      (input) INTEGER
C            The number of rows of the matrix A.  M >= 0.
C
C     N      (input) INTEGER
C            The number of columns of the matrix A.  N >= 0.
C
C     A      (input) COMPLEX*16 array, dimension (LDA,N)
C            The m-by-n matrix A.  If JOB = 'U', only the upper
C            triangle or trapezoid is accessed; if JOB = 'L', only the
C            lower triangle or trapezoid is accessed.
C
C     LDA    INTEGER
C            The leading dimension of the array A.  LDA >= max(1,M).
C
C     B      (output) COMPLEX*16 array, dimension (LDB,M)
C            B = A' in the locations specified by JOB, where ' denotes
C            the transpose or conjugate transpose operation, as
C            as specified by TRANS.
C
C     LDB    INTEGER
C            The leading dimension of the array B.  LDB >= max(1,N).
C
C     CONTRIBUTOR
C
C     V. Sima, June 2021.
C
C     REVISIONS
C
C     -
C
C     ******************************************************************
C
C     .. Scalar Arguments ..
      CHARACTER          JOB, TRANS
      INTEGER            LDA, LDB, M, N
C     .. Array Arguments ..
      COMPLEX*16         A(LDA,*), B(LDB,*)
C     .. Local Scalars ..
      INTEGER            I, J
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, MIN
C
C     .. Executable Statements ..
C
      IF( LSAME( TRANS, 'T' ) ) THEN
         IF( LSAME( JOB, 'U' ) ) THEN
            DO 20 J = 1, N
               DO 10 I = 1, MIN( J, M )
                  B(J,I) = A(I,J)
   10          CONTINUE
   20       CONTINUE
         ELSE IF( LSAME( JOB, 'L' ) ) THEN
            DO 40 J = 1, N
               DO 30 I = J, M
                  B(J,I) = A(I,J)
   30          CONTINUE
   40       CONTINUE
         ELSE
            DO 60 J = 1, N
               DO 50 I = 1, M
                  B(J,I) = A(I,J)
   50          CONTINUE
   60       CONTINUE
         END IF
      ELSE
         IF( LSAME( JOB, 'U' ) ) THEN
            DO 80 J = 1, N
               DO 70 I = 1, MIN( J, M )
                  B(J,I) = DCONJG( A(I,J) )
   70          CONTINUE
   80       CONTINUE
         ELSE IF( LSAME( JOB, 'L' ) ) THEN
            DO 100 J = 1, N
               DO 90 I = J, M
                  B(J,I) = DCONJG( A(I,J) )
   90          CONTINUE
  100       CONTINUE
         ELSE
            DO 120 J = 1, N
               DO 110 I = 1, M
                  B(J,I) = DCONJG( A(I,J) )
  110          CONTINUE
  120       CONTINUE
         END IF
      END IF
C
      RETURN
C *** Last line of MA02AZ ***
      END
