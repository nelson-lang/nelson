      SUBROUTINE MB03HD( N, A, LDA, B, LDB, MACPAR, Q, LDQ, DWORK,
     $                   INFO )
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
C     To determine an orthogonal matrix Q, for a real regular 2-by-2 or
C     4-by-4 skew-Hamiltonian/Hamiltonian pencil
C
C                     ( A11 A12  )     ( B11  B12  )
C         aA - bB = a (        T ) - b (         T )
C                     (  0  A11  )     (  0  -B11  )
C
C                                             T  T
C     in structured Schur form, such that  J Q  J  (aA - bB) Q  is still
C     in structured Schur form but the eigenvalues are exchanged.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the pencil aA - bB.  N = 2 or N = 4.
C
C     A       (input) DOUBLE PRECISION array, dimension (LDA, N)
C             If N = 4, the leading N/2-by-N upper trapezoidal part of
C             this array must contain the first block row of the skew-
C             Hamiltonian matrix A of the pencil aA - bB in structured
C             Schur form. Only the entries (1,1), (1,2), (1,4), and
C             (2,2) are referenced.
C             If N = 2, this array is not referenced.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= N/2.
C
C     B       (input) DOUBLE PRECISION array, dimension (LDB, N)
C             The leading N/2-by-N part of this array must contain the
C             first block row of the Hamiltonian matrix B of the
C             pencil aA - bB in structured Schur form. The entry (2,3)
C             is not referenced.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= N/2.
C
C     MACPAR  (input)  DOUBLE PRECISION array, dimension (2)
C             Machine parameters:
C             MACPAR(1)  (machine precision)*base, DLAMCH( 'P' );
C             MACPAR(2)  safe minimum,             DLAMCH( 'S' ).
C             This argument is not used for N = 2.
C
C     Q       (output) DOUBLE PRECISION array, dimension (LDQ, N)
C             The leading N-by-N part of this array contains the
C             orthogonal transformation matrix Q.
C
C     LDQ     INTEGER
C             The leading dimension of the array Q.  LDQ >= N.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (24)
C             If N = 2, then DWORK is not referenced.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0: succesful exit;
C             = 1: the leading N/2-by-N/2 block of the matrix B is
C                  numerically singular.
C
C     METHOD
C
C     The algorithm uses orthogonal transformations as described on page
C     31 in [2]. The structure is exploited.
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
C     V. Sima, Sep. 2009 (SLICOT version of the routine DHAUEX).
C
C     REVISIONS
C
C     V. Sima, Nov. 2009, Nov. 2010.
C
C     KEYWORDS
C
C     Eigenvalue exchange, skew-Hamiltonian/Hamiltonian pencil,
C     structured Schur form.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TWO = 2.0D+0 )
C
C     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LDB, LDQ, N
C
C     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), DWORK( * ),
     $                   MACPAR( * ), Q( LDQ, * )
C
C     .. Local Scalars ..
      INTEGER            ITAU, IWRK
      DOUBLE PRECISION   CO, D, NRM, S, SI, SMIN, SMLN, T
C
C     .. Local Arrays ..
      DOUBLE PRECISION   PAR( 3 )
C
C     .. External Subroutines ..
      EXTERNAL           DGEMM, DGEQR2, DLACPY, DLARTG, DLASCL, DORG2R,
     $                   DROT, MB02UW
C
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SQRT
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
C
C        Set machine constants.
C
         PAR( 1 ) = MACPAR( 1 )
         PAR( 2 ) = MACPAR( 2 )
C
C        Compute si*inv(B11)*[ A11 A12 B12 ], using blocks of A and B.
C        X11 = si*inv(B11)*A11.
C        Also, set SMIN to avoid overflows in matrix multiplications.
C
         DWORK(  1 ) =  A( 1, 1 )
         DWORK(  2 ) =  ZERO
         DWORK(  5 ) =  A( 1, 2 )
         DWORK(  6 ) =  A( 2, 2 )
         DWORK(  9 ) =  ZERO
         DWORK( 10 ) = -A( 1, 4 )
         DWORK( 11 ) = -DWORK(  1 )
         DWORK( 12 ) = -DWORK(  5 )
         DWORK( 13 ) = -DWORK( 10 )
         DWORK( 14 ) =  ZERO
         DWORK( 15 ) =  ZERO
         DWORK( 16 ) = -DWORK(  6 )
         DWORK( 17 ) =  B( 1, 3 )
         DWORK( 18 ) =  B( 1, 4 )
         DWORK( 21 ) =  B( 1, 4 )
         DWORK( 22 ) =  B( 2, 4 )
C
         SMLN = TWO*PAR( 2 ) / PAR( 1 )
         SMIN = SQRT( SMLN ) /
     $          MAX( ABS( DWORK(  1 ) ), SMLN, ABS( DWORK( 10 ) ),
     $               ABS( DWORK(  5 ) ) + ABS( DWORK( 6 ) ),
     $               ABS( DWORK( 18 ) ) +
     $               MAX( ABS( DWORK( 17 ) ), ABS( DWORK( 22 ) ) ) )
         PAR( 3 ) = SMIN
         CALL MB02UW( .FALSE., 2, 6, PAR, B, LDB, DWORK, 4, SI, INFO )
         IF( INFO.NE.0 )
     $      RETURN
C
C        Compute X22 = -d*inv(B11')*A11'.
C
         CALL MB02UW( .TRUE.,  2, 2, PAR, B, LDB, DWORK( 11 ), 4, D,
     $                INFO )
C
C        Take si = min( si, d ) as unique scaling factor.
C
         IF( SI.LT.D ) THEN
            CALL DLASCL( 'G', 0, 0, D, SI, 2, 2, DWORK( 11 ), 4, INFO )
         ELSE IF( SI.GT.D ) THEN
            CALL DLASCL( 'G', 0, 0, SI, D, 2, 6, DWORK, 4, INFO )
         END IF
C
C        Compute X12 = si*( inv(B11)*A12 - ( inv(B11)*B12 )*X22 ).
C
         CALL DGEMM( 'No Transpose', 'No Transpose', 2, 2, 2, -ONE,
     $               DWORK( 17 ), 4, DWORK( 11 ), 4, ONE, DWORK( 9 ),
     $               4 )
C
C        Scale X11, X12, and X22, so that 1-norm of X11 is 1.
C
         NRM = MAX( ABS( DWORK( 1 ) ) + ABS( DWORK( 2 ) ),
     $              ABS( DWORK( 5 ) ) + ABS( DWORK( 6 ) ), SMLN )
         IF ( NRM.GT.ONE ) THEN
            CALL DLASCL( 'G', 0, 0, NRM, ONE, 2, 4, DWORK, 4, INFO )
            CALL DLASCL( 'G', 0, 0, NRM, ONE, 2, 2, DWORK( 11 ), 4,
     $                   INFO )
         END IF
C
C        Compute s = trace(X11).
C
         S = DWORK( 1 ) + DWORK( 6 )
C
C        Compute Y2, the last two columns of Y = X*X - s*X + t*I4,
C        where X = ( Xij ), i,j = 1,2, X21 = 0, t = det(X11).
C
         T = DWORK( 1 )*DWORK( 6 ) - DWORK( 2 )*DWORK( 5 )
C
         CALL DLACPY( 'Full', 4, 2, DWORK( 9 ), 4, Q, LDQ )
         CALL DGEMM( 'No Transpose', 'No Transpose', 2, 2, 4, ONE,
     $               DWORK, 4, DWORK( 9 ), 4, -S, Q, LDQ )
         CALL DGEMM( 'No Transpose', 'No Transpose', 2, 2, 2, ONE,
     $               DWORK( 11 ), 4, DWORK( 11 ), 4, -S, Q( 3, 1 ),
     $               LDQ )
         Q( 3, 1 ) = Q( 3, 1 ) + T
         Q( 4, 2 ) = Q( 4, 2 ) + T
C
         ITAU = 1
         IWRK = 3
C
C        Triangularize Y2 and compute the orthogonal transformation
C        matrix.
C
         CALL DGEQR2( 4, 2, Q, LDQ, DWORK( ITAU ), DWORK( IWRK ), INFO )
         CALL DORG2R( 4, 4, 2, Q, LDQ, DWORK( ITAU ), DWORK( IWRK ),
     $                INFO )
C
C        Use the last two columns of Q to build a 2-by-4 matrix W.
C        Postmultiply A with the first column of Q, and premultiply
C        by W. Then, annihilate the second element of the result.
C
         DWORK( 21 ) = A( 1, 1 )*Q( 1, 1 ) + A( 1, 2 )*Q( 2, 1 ) +
     $                 A( 1, 4 )*Q( 4, 1 )
         DWORK( 22 ) = A( 2, 2 )*Q( 2, 1 ) - A( 1, 4 )*Q( 3, 1 )
         DWORK( 23 ) = A( 1, 1 )*Q( 3, 1 )
         DWORK( 24 ) = A( 1, 2 )*Q( 3, 1 ) + A( 2, 2 )*Q( 4, 1 )
         DWORK(  9 ) = Q( 3, 3 )*DWORK( 21 ) + Q( 4, 3 )*DWORK( 22 )
     $               - Q( 1, 3 )*DWORK( 23 ) - Q( 2, 3 )*DWORK( 24 )
         DWORK( 10 ) = Q( 3, 4 )*DWORK( 21 ) + Q( 4, 4 )*DWORK( 22 )
     $               - Q( 1, 4 )*DWORK( 23 ) - Q( 2, 4 )*DWORK( 24 )
         CALL DLARTG( DWORK( 9 ), DWORK( 10 ), CO, SI, T )
         CALL DROT( 4, Q( 1, 3 ), 1, Q( 1, 4 ), 1, CO, SI )
C
      ELSE
         CALL DLARTG( B( 1, 2 ), TWO*B( 1, 1 ), CO, SI, T )
         Q( 1, 1 ) =  CO
         Q( 2, 1 ) = -SI
         Q( 1, 2 ) =  SI
         Q( 2, 2 ) =  CO
      END IF
C
      RETURN
C *** Last line of MB03HD ***
      END
