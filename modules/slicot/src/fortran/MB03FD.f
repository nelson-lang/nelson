      SUBROUTINE MB03FD( N, PREC, A, LDA, B, LDB, Q1, LDQ1, Q2, LDQ2,
     $                   DWORK, LDWORK, INFO )
C
C     SLICOT RELEASE 5.0.
C
C     Copyright (c) 2002-2010 NICONET e.V.
C
C     This program is free software: you can redistribute it and/or
C     modify it under the terms of the GNU General Public License as
C     published by the Free Software Foundation, either version 2 of
C     the License, or (at your option) any later version.
C
C     This program is distributed in the hope that it will be useful,
C     but WITHOUT ANY WARRANTY; without even the implied warranty of
C     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C     GNU General Public License for more details.
C
C     You should have received a copy of the GNU General Public License
C     along with this program.  If not, see
C     <http://www.gnu.org/licenses/>.
C
C     PURPOSE
C
C     To compute orthogonal matrices Q1 and Q2 for a real 2-by-2 or
C     4-by-4 regular pencil
C
C                   ( A11  0  )     (  0  B12 )
C       aA - bB = a (         ) - b (         ),                     (1)
C                   (  0  A22 )     ( B21  0  )
C
C     such that Q2' A Q1 is upper triangular, Q2' B Q1 is upper quasi-
C     triangular, and the eigenvalues with negative real parts (if there
C     are any) are allocated on the top. The submatrices A11, A22, and
C     B12 are upper triangular. If B21 is 2-by-2, then all the other
C     blocks are nonsingular and the product
C
C        -1        -1
C     A11   B12 A22   B21 has a pair of complex conjugate eigenvalues.
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
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
C             On entry, the leading N-by-N part of this array must
C             contain the matrix A of the pencil aA - bB.
C             If N = 2, the diagonal elements only are referenced.
C             On exit, if N = 4, the leading N-by-N part of this array
C             contains the transformed upper triangular matrix of the
C             generalized real Schur form of the pencil aA - bB.
C             If N = 2, this array is unchanged on exit.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= N.
C
C     B       (input/output) DOUBLE PRECISION array, dimension (LDB, N)
C             On entry, the leading N-by-N part of this array must
C             contain the matrix B of the pencil aA - bB.
C             If N = 2, the anti-diagonal elements only are referenced.
C             On exit, if N = 4, the leading N-by-N part of this array
C             contains the transformed real Schur matrix of the
C             generalized real Schur form of the pencil aA - bB.
C             If N = 2, this array is unchanged on exit.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= N.
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
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             If N = 2, then DWORK is not referenced.
C
C     LDWORK  INTEGER
C             The dimension of the array DWORK.
C             If N = 4, then LDWORK >= 63. For good performance LDWORK
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
C     29 in [2].
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
C     Chemnitz, October 16, 2008.
C
C     REVISIONS
C
C     V. Sima, Aug. 2009 (SLICOT version of the routine MB03FD).
C     V. Sima, Oct. 2009, Nov. 2009, Oct. 2010, Nov. 2010.
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
      INTEGER            INFO, LDA, LDB, LDQ1, LDQ2, LDWORK, N
      DOUBLE PRECISION   PREC
C
C     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), DWORK( * ),
     $                   Q1( LDQ1, * ), Q2( LDQ2, * )
C
C     .. Local Scalars ..
      LOGICAL            COMPG
      INTEGER            IDUM
      DOUBLE PRECISION   A11, A22, B12, B21, CO, SI, TMP
C
C     .. Local Arrays ..
      LOGICAL            BWORK( 4 )
C
C     .. External Functions ..
      LOGICAL            SB02OW
      EXTERNAL           SB02OW
C
C     .. External Subroutines ..
      EXTERNAL           DGGES, DLARTG
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
         CALL DGGES( 'Vector Computation', 'Vector Computation',
     $               'Sorted', SB02OW, N, B, LDB, A, LDA, IDUM, DWORK,
     $               DWORK( N+1 ), DWORK( 2*N+1 ), Q2, LDQ2, Q1, LDQ1,
     $               DWORK( 3*N+1 ), LDWORK-3*N, BWORK, INFO )
         IF( INFO.NE.0 ) THEN
            IF( INFO.GE.1 .AND. INFO.LE.4 ) THEN
               INFO = 1
            ELSE IF ( INFO.NE.6 ) THEN
               INFO = 2
            ELSE
               INFO = 0
            END IF
         END IF
         RETURN
      ELSE
C
C        The pencil has infinite eigenvalues. The code decides this when
C        A is (numerically) singular.
C
         A11   = ABS( A( 1, 1 ) )
         A22   = ABS( A( 2, 2 ) )
         B21   = ABS( B( 2, 1 ) )
         B12   = ABS( B( 1, 2 ) )
         COMPG = .FALSE.
         IF( A11.LE.PREC*A22 ) THEN
            Q1( 1, 1 ) = ONE
            Q1( 2, 1 ) = ZERO
            Q1( 1, 2 ) = ZERO
            Q1( 2, 2 ) = ONE
            Q2( 1, 1 ) = ZERO
            Q2( 2, 1 ) = ONE
            Q2( 1, 2 ) = ONE
            Q2( 2, 2 ) = ZERO
         ELSE IF( A22.LE.PREC*A11 ) THEN
            Q1( 1, 1 ) = ZERO
            Q1( 2, 1 ) = ONE
            Q1( 1, 2 ) = ONE
            Q1( 2, 2 ) = ZERO
            Q2( 1, 1 ) = ONE
            Q2( 2, 1 ) = ZERO
            Q2( 1, 2 ) = ZERO
            Q2( 2, 2 ) = ONE
         ELSE
            COMPG = .TRUE.
         END IF
         IF( COMPG ) THEN
C
C           The pencil has a double zero eigenvalue.
C
            IF( B21.LE.PREC*B12 ) THEN
               Q1( 1, 1 ) = ONE
               Q1( 2, 1 ) = ZERO
               Q1( 1, 2 ) = ZERO
               Q1( 2, 2 ) = ONE
               Q2( 1, 1 ) = ONE
               Q2( 2, 1 ) = ZERO
               Q2( 1, 2 ) = ZERO
               Q2( 2, 2 ) = ONE
            ELSE IF( B12.LE.PREC*B21 ) THEN
               Q1( 1, 1 ) = ZERO
               Q1( 2, 1 ) = ONE
               Q1( 1, 2 ) = ONE
               Q1( 2, 2 ) = ZERO
               Q2( 1, 1 ) = ZERO
               Q2( 2, 1 ) = ONE
               Q2( 1, 2 ) = ONE
               Q2( 2, 2 ) = ZERO
            ELSE
               COMPG = .TRUE.
            END IF
         END IF
         IF( COMPG ) THEN
C
C           The pencil has real eigenvalues.
C
            CALL DLARTG( SIGN( ONE, A( 1, 1 )*A( 2, 2 ) )*
     $                   SQRT( A22*B12 ), SQRT( A11*B21 ), CO, SI, TMP )
            Q1( 1, 1 ) =  CO
            Q1( 2, 1 ) = -SI
            Q1( 1, 2 ) =  SI
            Q1( 2, 2 ) =  CO
            CALL DLARTG( SQRT( A11*B12 ), SQRT( A22*B21 ), CO, SI, TMP )
            Q2( 1, 1 ) =  CO
            Q2( 2, 1 ) = -SI
            Q2( 1, 2 ) =  SI
            Q2( 2, 2 ) =  CO
         END IF
      END IF
C
      RETURN
C *** Last line of MB03FD ***
      END
