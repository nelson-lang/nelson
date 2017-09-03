      SUBROUTINE MB04SU( M, N, A, LDA, B, LDB, CS, TAU, DWORK, LDWORK,
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
C     To compute a symplectic QR decomposition of a real 2M-by-N matrix
C     [A; B],
C
C               [ A ]             [ R11  R12 ]
C               [   ] = Q * R = Q [          ],
C               [ B ]             [ R21  R22 ]
C
C     where Q is a symplectic orthogonal matrix, R11 is upper triangular
C     and R21 is strictly upper triangular.
C     If [A; B] is symplectic then, theoretically, R21 = 0 and
C     R22 = inv(R11)^T. Unblocked version.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     M       (input) INTEGER
C             The number of rows of A and B. M >= 0.
C
C     N       (input) INTEGER
C             The number of columns of A and B. N >= 0.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading M-by-N part of this array must
C             contain the matrix A.
C             On exit, the leading M-by-N part of this array contains
C             the matrix [ R11  R12 ] and, in the zero parts of R,
C             information about the elementary reflectors used to
C             compute the symplectic QR decomposition.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,M).
C
C     B       (input/output) DOUBLE PRECISION array, dimension (LDB,N)
C             On entry, the leading M-by-N part of this array must
C             contain the matrix B.
C             On exit, the leading M-by-N part of this array contains
C             the matrix [ R21  R22 ] and, in the zero parts of B,
C             information about the elementary reflectors used to
C             compute the symplectic QR decomposition.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= MAX(1,M).
C
C     CS      (output) DOUBLE PRECISION array, dimension (2 * min(M,N))
C             On exit, the first 2*min(M,N) elements of this array
C             contain the cosines and sines of the symplectic Givens
C             rotations used to compute the symplectic QR decomposition.
C
C     TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
C             On exit, the first min(M,N) elements of this array
C             contain the scalar factors of some of the elementary
C             reflectors.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0,  DWORK(1)  returns the optimal
C             value of LDWORK.
C             On exit, if  INFO = -10,  DWORK(1)  returns the minimum
C             value of LDWORK.
C
C     LDWORK  INTEGER
C             The length of the array DWORK.  LDWORK >= MAX(1,N).
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
C     The matrix Q is represented as a product of symplectic reflectors
C     and Givens rotators
C
C     Q = diag( H(1),H(1) ) G(1) diag( F(1),F(1) )
C         diag( H(2),H(2) ) G(2) diag( F(2),F(2) )
C                           ....
C         diag( H(k),H(k) ) G(k) diag( F(k),F(k) ),
C
C     where k = min(m,n).
C
C     Each H(i) has the form
C
C           H(i) = I - tau * w * w'
C
C     where tau is a real scalar, and w is a real vector with
C     w(1:i-1) = 0 and w(i) = 1; w(i+1:m) is stored on exit in
C     B(i+1:m,i), and tau in B(i,i).
C
C     Each F(i) has the form
C
C           F(i) = I - nu * v * v'
C
C     where nu is a real scalar, and v is a real vector with
C     v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in
C     A(i+1:m,i), and nu in TAU(i).
C
C     Each G(i) is a Givens rotator acting on rows i of A and B,
C     where the cosine is stored in CS(2*i-1) and the sine in
C     CS(2*i).
C
C     REFERENCES
C
C     [1] Bunse-Gerstner, A.
C         Matrix factorizations for symplectic QR-like methods.
C         Linear Algebra Appl., 83, pp. 49-77, 1986.
C
C     [2] Byers, R.
C         Hamiltonian and Symplectic Algorithms for the Algebraic
C         Riccati Equation.
C         Ph.D. Dissertation, Center for Applied Mathematics,
C         Cornell University, Ithaca, NY, 1983.
C
C     NUMERICAL ASPECTS
C
C     The algorithm requires
C        8*M*N*N - 8/3*N*N*N +  2*M*N + 6*N*N + 8/3*N,  if M >= N,
C        8*M*M*N - 8/3*M*M*M + 14*M*N - 6*M*M + 8/3*N,  if M <= N,
C     floating point operations and is numerically backward stable.
C
C     CONTRIBUTORS
C
C     D. Kressner, Technical Univ. Berlin, Germany, and
C     P. Benner, Technical Univ. Chemnitz, Germany, December 2003.
C
C     REVISIONS
C
C     V. Sima, June 2008 (SLICOT version of the HAPACK routine DGESQR).
C
C     KEYWORDS
C
C     Elementary matrix operations, orthogonal symplectic matrix.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ONE
      PARAMETER         ( ONE = 1.0D0 )
C     .. Scalar Arguments ..
      INTEGER           INFO, LDA, LDB, LDWORK, M, N
C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), B(LDB,*), CS(*), DWORK(*), TAU(*)
C     .. Local Scalars ..
      INTEGER           I, K
      DOUBLE PRECISION  ALPHA, NU, TEMP
C     .. External Subroutines ..
      EXTERNAL          DLARF, DLARFG, DLARTG, DROT, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         DBLE, MAX, MIN
C
C     .. Executable Statements ..
C
C     Check the scalar input parameters.
C
      INFO  = 0
      IF ( M.LT.0 ) THEN
         INFO = -1
      ELSE IF ( N.LT.0 ) THEN
         INFO = -2
      ELSE IF ( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      ELSE IF ( LDB.LT.MAX( 1, M ) ) THEN
         INFO = -6
      ELSE IF ( LDWORK.LT.MAX( 1, N ) ) THEN
         DWORK(1) = DBLE( MAX( 1, N ) )
         INFO = -10
      END IF
C
C     Return if there were illegal values.
C
      IF ( INFO.NE.0 ) THEN
         CALL XERBLA( 'MB04SU', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      K = MIN( M,N )
      IF ( K.EQ.0 ) THEN
         DWORK(1) = ONE
         RETURN
      END IF
C
      DO 10 I = 1, K
C
C        Generate elementary reflector H(i) to annihilate B(i+1:m,i).
C
         ALPHA = B(I,I)
         CALL DLARFG( M-I+1, ALPHA, B(MIN( I+1,M ),I), 1, NU )
C
C        Apply H(i) to A(i:m,i:n) and B(i:m,i+1:n) from the left.
C
         B(I,I) = ONE
         CALL DLARF( 'Left', M-I+1, N-I+1, B(I,I), 1, NU, A(I,I), LDA,
     $               DWORK )
         IF ( I.LT.N )
     $      CALL DLARF( 'Left', M-I+1, N-I, B(I,I), 1, NU, B(I,I+1),
     $                  LDB, DWORK )
         B(I,I) = NU
C
C        Generate symplectic Givens rotator G(i) to annihilate
C        B(i,i).
C
         TEMP = A(I,I)
         CALL DLARTG( TEMP, ALPHA, CS(2*I-1), CS(2*I), A(I,I) )
         IF ( I.LT.N ) THEN
C
C           Apply G(i) to [ A(i,i+1:n); B(i,i+1:n) ] from the left.
C
            CALL DROT( N-I, A(I,I+1), LDA, B(I,I+1), LDB, CS(2*I-1),
     $                 CS(2*I) )
         END IF
C
C        Generate elementary reflector F(i) to annihilate A(i+1:m,i).
C
         CALL DLARFG( M-I+1, A(I,I), A(MIN( I+1,M ),I), 1, TAU(I) )
         IF ( I.LT.N ) THEN
C
C           Apply F(i) to A(i:m,i+1:n) and B(i:m,i+1:n) from the
C           left.
C
            TEMP = A(I,I)
            A(I,I) = ONE
            CALL DLARF( 'Left', M-I+1, N-I, A(I,I), 1, TAU(I), A(I,I+1),
     $                  LDA, DWORK )
            CALL DLARF( 'Left', M-I+1, N-I, A(I,I), 1, TAU(I), B(I,I+1),
     $                  LDB, DWORK )
            A(I,I) = TEMP
         END IF
   10 CONTINUE
      DWORK(1) = DBLE(MAX( 1, N ))
      RETURN
C *** Last line of MB04SU ***
      END
