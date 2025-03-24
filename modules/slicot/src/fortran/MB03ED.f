      SUBROUTINE MB03ED( N, PREC, A, LDA, B, LDB, D, LDD, Q1, LDQ1, Q2,
     $                   LDQ2, Q3, LDQ3, DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To compute orthogonal matrices Q1, Q2, Q3 for a real 2-by-2 or
C     4-by-4 regular pencil
C
C                    ( A11  0  ) ( B11  0  )     (  0  D12 )
C       aAB - bD = a (         ) (         ) - b (         ),        (1)
C                    (  0  A22 ) (  0  B22 )     ( D21  0  )
C
C     such that Q3' A Q2 and Q2' B Q1 are upper triangular, Q3' D Q1 is
C     upper quasi-triangular, and the eigenvalues with negative real
C     parts (if there are any) are allocated on the top. The notation M'
C     denotes the transpose of the matrix M. The submatrices A11, A22,
C     B11, B22 and D12 are upper triangular. If D21 is 2-by-2, then all
C     other blocks are nonsingular and the product
C        -1        -1    -1        -1
C     A22   D21 B11   A11   D12 B22   has a pair of complex conjugate
C     eigenvalues.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the input pencil, N = 2 or N = 4.
C
C     PREC    (input) DOUBLE PRECISION
C             The machine precision, (relative machine precision)*base.
C             See the LAPACK Library routine DLAMCH.
C
C     A       (input) DOUBLE PRECISION array, dimension (LDA, N)
C             The leading N-by-N upper triangular part of this array
C             must contain the upper triangular matrix A of the pencil
C             aAB - bD. The strictly lower triangular part and the
C             entries of the (1,2) block are not referenced.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= N.
C
C     B       (input) DOUBLE PRECISION array, dimension (LDB, N)
C             The leading N-by-N upper triangular part of this array
C             must contain the upper triangular matrix B of the pencil
C             aAB - bD. The strictly lower triangular part and the
C             entries of the (1,2) block are not referenced.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= N.
C
C     D       (input/output) DOUBLE PRECISION array, dimension (LDD, N)
C             On entry, the leading N-by-N part of this array must
C             contain the matrix D of the pencil aAB - bD.
C             On exit, if N = 4, the leading N-by-N part of this array
C             contains the transformed matrix D in real Schur form.
C             If N = 2, this array is unchanged on exit.
C
C     LDD     INTEGER
C             The leading dimension of the array D.  LDD >= N.
C
C     Q1      (output) DOUBLE PRECISION array, dimension (LDQ1, N)
C             The leading N-by-N part of this array contains the first
C             orthogonal transformation matrix.
C
C     LDQ1    INTEGER
C             The leading dimension of the array Q1.  LDQ1 >= N.
C
C     Q2      (output) DOUBLE PRECISION array, dimension (LDQ2, N)
C             The leading N-by-N part of this array contains the second
C             orthogonal transformation matrix.
C
C     LDQ2    INTEGER
C             The leading dimension of the array Q2.  LDQ2 >= N.
C
C     Q3      (output) DOUBLE PRECISION array, dimension (LDQ3, N)
C             The leading N-by-N part of this array contains the third
C             orthogonal transformation matrix.
C
C     LDQ3    INTEGER
C             The leading dimension of the array Q3.  LDQ3 >= N.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             If N = 2, then DWORK is not referenced.
C
C     LDWORK  INTEGER
C             The dimension of the array DWORK.
C             If N = 4, then LDWORK >= 79. For good performance LDWORK
C             should be generally larger.
C             If N = 2, then LDWORK >= 0.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0: succesful exit;
C             = 1: the QZ iteration failed in the LAPACK routine DGGES;
C             = 2: another error occured during execution of DGGES.
C
C     METHOD
C
C     The algorithm uses orthogonal transformations as described on page
C     20 in [2].
C
C     REFERENCES
C
C     [1] Benner, P., Byers, R., Mehrmann, V. and Xu, H.
C         Numerical computation of deflating subspaces of skew-
C         Hamiltonian/Hamiltonian pencils.
C         SIAM J. Matrix Anal. Appl., 24 (1), pp. 165-190, 2002.
C
C     [2] Benner, P., Byers, R., Losse, P., Mehrmann, V. and Xu, H.
C         Numerical Solution of Real Skew-Hamiltonian/Hamiltonian
C         Eigenproblems.
C         Tech. Rep., Technical University Chemnitz, Germany,
C         Nov. 2007.
C
C     NUMERICAL ASPECTS
C
C     The algorithm is numerically backward stable.
C
C     CONTRIBUTOR
C
C     Matthias Voigt, Fakultaet fuer Mathematik, Technische Universitaet
C     Chemnitz, October 22, 2008.
C
C     REVISIONS
C
C     V. Sima, Aug. 2009 (SLICOT version of the routine DBTFSX).
C     V. Sima, Oct. 2009, Nov. 2009, Oct. 2010, Nov. 2010.
C     M. Voigt, Jan. 2012.
C
C     KEYWORDS
C
C     Eigenvalue exchange, matrix pencil, upper (quasi-)triangular
C     matrix.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
C
C     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LDB, LDD, LDQ1, LDQ2, LDQ3, LDWORK,
     $                   N
      DOUBLE PRECISION   PREC
C
C     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), D( LDD, * ),
     $                   DWORK( * ), Q1( LDQ1, * ), Q2( LDQ2, * ),
     $                   Q3( LDQ3, * )
C
C     .. Local Scalars ..
      LOGICAL            COMPG
      INTEGER            IDUM, IEVS, IWRK
      DOUBLE PRECISION   A11, A22, B11, B22, CO, D12, D21, SI, TMP
C
C     .. Local Arrays ..
      LOGICAL            BWORK( 4 )
      DOUBLE PRECISION   DUM( 1 )
C
C     .. External Functions ..
      LOGICAL            SB02OW
      EXTERNAL           SB02OW
C
C     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEQR2, DGGES, DLACPY, DLARTG, DORG2R,
     $                   DTRMM
C
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, SIGN, SQRT
C
C     .. Executable Statements ..
C
C     For efficiency, the input arguments are not tested.
C
      INFO = 0
C
C     Computations.
C
      IF( N.EQ.4 ) THEN
         DUM( 1 ) = ZERO
         CALL DCOPY( 16, DUM, 0, DWORK, 1 )
         DWORK(  1 ) = B( 1, 1 )
         DWORK(  5 ) = B( 1, 2 )
         DWORK(  6 ) = B( 2, 2 )
         DWORK( 11 ) = B( 3, 3 )
         DWORK( 15 ) = B( 3, 4 )
         DWORK( 16 ) = B( 4, 4 )
         CALL DTRMM( 'Left', 'Upper', 'No Transpose', 'NonUnit', 2, 4,
     $               ONE, A, LDA, DWORK, N )
         CALL DTRMM( 'Left', 'Upper', 'No Transpose', 'NonUnit', 2, 2,
     $               ONE, A( 3, 3 ), LDA, DWORK( 11 ), N )
         IEVS = N*N  + 1
         IWRK = IEVS + 3*N
         CALL DGGES( 'Vector Computation', 'Vector Computation',
     $               'Sorted', SB02OW, N, D, LDD, DWORK, N, IDUM,
     $               DWORK( IEVS ), DWORK( IEVS+N ), DWORK( IEVS+2*N ),
     $               Q3, LDQ3, Q1, LDQ1, DWORK( IWRK ), LDWORK-IWRK+1,
     $               BWORK, INFO )
         IF( INFO.NE.0 ) THEN
            IF( INFO.GE.1 .AND. INFO.LE.4 ) THEN
               INFO = 1
               RETURN
            ELSE IF ( INFO.NE.6 ) THEN
               INFO = 2
               RETURN
            ELSE
               INFO = 0
            END IF
         END IF
         CALL DLACPY( 'Full', N, N, Q1, LDQ1, Q2, LDQ2 )
         CALL DTRMM(  'Left', 'Upper', 'No Transpose', 'NonUnit', 2, 4,
     $                ONE, B, LDB, Q2, LDQ2 )
         CALL DTRMM(  'Left', 'Upper', 'No Transpose', 'NonUnit', 2, 4,
     $                ONE, B( 3, 3 ), LDB, Q2( 3, 1 ), LDQ2 )
         CALL DGEQR2( N, N, Q2, LDQ2, DWORK, DWORK( N+1 ), INFO )
         CALL DORG2R( N, N, N, Q2, LDQ2, DWORK, DWORK( N+1 ), INFO )
      ELSE
C
C        The pencil has infinite eigenvalues. The code decides this when
C        A or B is (numerically) singular. Although the numerical
C        singularity of A*B with respect to PREC is detected, the
C        eigenvalues will not be infinite, but large, when neither A
C        nor B is (numerically) singular. This allows a more accurate
C        computation of the transformed A and B (using Q1, Q2, and Q3),
C        as well as of the eigenvalues.
C
         A11   = ABS( A( 1, 1 ) )
         A22   = ABS( A( 2, 2 ) )
         B11   = ABS( B( 1, 1 ) )
         B22   = ABS( B( 2, 2 ) )
         D21   = ABS( D( 2, 1 ) )
         D12   = ABS( D( 1, 2 ) )
         COMPG = .FALSE.
         IF( A11*B11.LE.PREC*A22*B22 ) THEN
            IF(  A11.LE.PREC*A22 ) THEN
               Q1( 1, 1 ) = ONE
               Q1( 2, 1 ) = ZERO
               Q1( 1, 2 ) = ZERO
               Q1( 2, 2 ) = ONE
               Q2( 1, 1 ) = ONE
               Q2( 2, 1 ) = ZERO
               Q2( 1, 2 ) = ZERO
               Q2( 2, 2 ) = ONE
               Q3( 1, 1 ) = ZERO
               Q3( 2, 1 ) = -ONE
               Q3( 1, 2 ) = -ONE
               Q3( 2, 2 ) = ZERO
            ELSE IF( B11.LE.PREC*B22 ) THEN
               Q1( 1, 1 ) = -ONE
               Q1( 2, 1 ) = ZERO
               Q1( 1, 2 ) = ZERO
               Q1( 2, 2 ) = -ONE
               Q2( 1, 1 ) = ZERO
               Q2( 2, 1 ) = ONE
               Q2( 1, 2 ) = ONE
               Q2( 2, 2 ) = ZERO
               Q3( 1, 1 ) = ZERO
               Q3( 2, 1 ) = ONE
               Q3( 1, 2 ) = ONE
               Q3( 2, 2 ) = ZERO
            ELSE
               COMPG = .TRUE.
            END IF
         ELSE IF( A22*B22.LE.PREC*A11*B11 ) THEN
            IF( A22.LE.PREC*A11 ) THEN
               Q1( 1, 1 ) = ZERO
               Q1( 2, 1 ) = ONE
               Q1( 1, 2 ) = ONE
               Q1( 2, 2 ) = ZERO
               Q2( 1, 1 ) = ZERO
               Q2( 2, 1 ) = ONE
               Q2( 1, 2 ) = ONE
               Q2( 2, 2 ) = ZERO
               Q3( 1, 1 ) = -ONE
               Q3( 2, 1 ) = ZERO
               Q3( 1, 2 ) = ZERO
               Q3( 2, 2 ) = -ONE
            ELSE IF( B22.LE.PREC*B11 ) THEN
               Q1( 1, 1 ) = ZERO
               Q1( 2, 1 ) = -ONE
               Q1( 1, 2 ) = -ONE
               Q1( 2, 2 ) = ZERO
               Q2( 1, 1 ) = ONE
               Q2( 2, 1 ) = ZERO
               Q2( 1, 2 ) = ZERO
               Q2( 2, 2 ) = ONE
               Q3( 1, 1 ) = ONE
               Q3( 2, 1 ) = ZERO
               Q3( 1, 2 ) = ZERO
               Q3( 2, 2 ) = ONE
            ELSE
               COMPG = .TRUE.
            END IF
C
C        The pencil has a double zero eigenvalue.
C
         ELSE IF( D21.LE.PREC*D12 ) THEN
            Q1( 1, 1 ) = ONE
            Q1( 2, 1 ) = ZERO
            Q1( 1, 2 ) = ZERO
            Q1( 2, 2 ) = ONE
            Q2( 1, 1 ) = ONE
            Q2( 2, 1 ) = ZERO
            Q2( 1, 2 ) = ZERO
            Q2( 2, 2 ) = ONE
            Q3( 1, 1 ) = ONE
            Q3( 2, 1 ) = ZERO
            Q3( 1, 2 ) = ZERO
            Q3( 2, 2 ) = ONE
         ELSE IF( D12.LE.PREC*D21 ) THEN
            Q1( 1, 1 ) = ZERO
            Q1( 2, 1 ) = ONE
            Q1( 1, 2 ) = ONE
            Q1( 2, 2 ) = ZERO
            Q2( 1, 1 ) = ZERO
            Q2( 2, 1 ) = ONE
            Q2( 1, 2 ) = ONE
            Q2( 2, 2 ) = ZERO
            Q3( 1, 1 ) = ZERO
            Q3( 2, 1 ) = ONE
            Q3( 1, 2 ) = ONE
            Q3( 2, 2 ) = ZERO
         ELSE
            COMPG = .TRUE.
         END IF
C
         IF( COMPG ) THEN
C
C           The pencil has real eigenvalues.
C
            CALL DLARTG( SIGN( ONE, A( 1, 1 )*B( 1, 1 )*A( 2, 2 )*
     $                   B( 2, 2 ) )*SQRT( A22*B22*D12 ),
     $                   SQRT( A11*B11*D21 ), CO, SI, TMP )
            Q1( 1, 1 ) =  CO
            Q1( 2, 1 ) = -SI
            Q1( 1, 2 ) =  SI
            Q1( 2, 2 ) =  CO
            CALL DLARTG( SIGN( ONE, A( 1, 1 )*A( 2, 2 ) )*
     $                   SQRT( A22*B11*D12 ), SQRT( A11*B22*D21 ), CO,
     $                   SI, TMP )
            Q2( 1, 1 ) =  CO
            Q2( 2, 1 ) = -SI
            Q2( 1, 2 ) =  SI
            Q2( 2, 2 ) =  CO
            CALL DLARTG( SQRT( A11*B11*D12 ), SQRT( A22*B22*D21 ), CO,
     $                   SI, TMP )
            Q3( 1, 1 ) =  CO
            Q3( 2, 1 ) = -SI
            Q3( 1, 2 ) =  SI
            Q3( 2, 2 ) =  CO
         END IF
      END IF
C
      RETURN
C *** Last line of MB03ED ***
      END
