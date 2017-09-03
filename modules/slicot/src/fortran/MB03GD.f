      SUBROUTINE MB03GD( N, B, LDB, D, LDD, MACPAR, Q, LDQ, U, LDU,
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
C     To compute an orthogonal matrix Q and an orthogonal symplectic
C     matrix U for a real regular 2-by-2 or 4-by-4 skew-Hamiltonian/
C     Hamiltonian pencil a J B' J' B - b D with
C
C           ( B11  B12 )      (  D11  D12  )
C       B = (          ), D = (            ),
C           (  0   B22 )      (   0  -D11' )
C
C     such that J Q' J' D Q and U' B Q keep block triangular form, but
C     the eigenvalues are reordered.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the pencil a J B' J' B - b D. N = 2 or N = 4.
C
C     B       (input) DOUBLE PRECISION array, dimension (LDB, N)
C             The leading N-by-N part of this array must contain the
C             non-trivial factor of the decomposition of the
C             skew-Hamiltonian input matrix J B' J' B. The (2,1) block
C             is not referenced.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= N.
C
C     D       (input) DOUBLE PRECISION array, dimension (LDD, N)
C             The leading N/2-by-N part of this array must contain the
C             first block row of the second matrix of a J B' J' B - b D.
C             The matrix D has to be Hamiltonian. The strict lower
C             triangle of the (1,2) block is not referenced.
C
C     LDD     INTEGER
C             The leading dimension of the array D.  LDD >= N/2.
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
C     U       (output) DOUBLE PRECISION array, dimension (LDU, N)
C             The leading N-by-N part of this array contains the
C             orthogonal symplectic transformation matrix U.
C
C     LDU     INTEGER
C             The leading dimension of the array U.  LDU >= N.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             If N = 2 then DWORK is not referenced.
C
C     LDWORK  INTEGER
C             The length of the array DWORK.
C             If N = 2 then LDWORK >= 0; if N = 4 then LDWORK >= 12.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0: succesful exit;
C             = 1: B11 or B22 is a (numerically) singular matrix.
C
C     METHOD
C
C     The algorithm uses orthogonal transformations as described on page
C     22 in [1], but with an improved implementation.
C
C     REFERENCES
C
C     [1] Benner, P., Byers, R., Losse, P., Mehrmann, V. and Xu, H.
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
C     Chemnitz, October 29, 2008.
C     V. Sima, Aug. 2009 (SLICOT version of the routine DHAFEX).
C
C     REVISIONS
C
C     V. Sima, Nov. 2009, July 2010, Nov. 2010.
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
      INTEGER            INFO, LDB, LDD, LDQ, LDU, LDWORK, N
C
C     .. Array Arguments ..
      DOUBLE PRECISION   B( LDB, * ), D( LDD, * ), DWORK( * ),
     $                   MACPAR( * ), Q( LDQ, * ), U( LDU, * )
C
C     .. Local Scalars ..
      INTEGER            I, ICS, IR, ITAU, IWRK1, IWRK2
      DOUBLE PRECISION   CL1, CL2, CO, CO2, CR1, CR2, EPS, F, F1, G, G1,
     $                   R, R1, S, SFMIN, SI, SI2, SL1, SL2, SMAX1,
     $                   SMAX2, SMIN1, SMIN2, SR1, SR2, T, T1
C
C     .. External Subroutines ..
      EXTERNAL           DGEMM, DGEQR2, DGERQ2, DLACPY, DLARTG, DLASV2,
     $                   DORGR2, DORM2R, DSWAP, DSYR2K, MB01RU, MB04SU,
     $                   MB04WU
C
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX
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
         EPS   = MACPAR( 1 )
         SFMIN = MACPAR( 2 )
C
C        Compute the first two columns of H = inv( B' )*J'*D*inv( B )*J
C        in U, using the singular value decompositions of B11 and B22.
C
         CALL DLARTG( B( 1, 1 ), B( 2, 1 ), CO, SI, R )
         F = CO*B( 1, 2 ) + SI*B( 2, 2 )
         G = CO*B( 2, 2 ) - SI*B( 1, 2 )
         CALL DLASV2( R, F, G, SMIN1, SMAX1, SR1, CR1, SL1, CL1 )
         IF( ABS( SMIN1 ).LT.MAX( SFMIN, EPS*ABS( SMAX1 ) ) ) THEN
            INFO = 1
            RETURN
         END IF
C
         CALL DLARTG( B( 3, 3 ), B( 4, 3 ), CO2, SI2, R )
         F = CO2*B( 3, 4 ) + SI2*B( 4, 4 )
         G = CO2*B( 4, 4 ) - SI2*B( 3, 4 )
         CALL DLASV2( R, F, G, SMIN2, SMAX2, SR2, CR2, SL2, CL2 )
         IF( ABS( SMIN2 ).LT.MAX( SFMIN, EPS*ABS( SMAX2 ) ) ) THEN
            INFO = 1
            RETURN
         END IF
C
C        Compute inv( B11' )*D11' and copy it in U12.
C
         R = ( CR1*D( 1, 1 ) + SR1*D( 1, 2 ) )/SMAX1
         F = ( CR1*D( 2, 1 ) + SR1*D( 2, 2 ) )/SMAX1
         T = ( CR1*D( 1, 2 ) - SR1*D( 1, 1 ) )/SMIN1
         G = ( CR1*D( 2, 2 ) - SR1*D( 2, 1 ) )/SMIN1
C
         R1 = CL1*R - SL1*T
         F1 = CL1*F - SL1*G
         T1 = CL1*T + SL1*R
         G1 = CL1*G + SL1*F
C
         U( 1, 3 ) = CO*R1 - SI*T1
         U( 2, 3 ) = CO*T1 + SI*R1
         U( 1, 4 ) = CO*F1 - SI*G1
         U( 2, 4 ) = CO*G1 + SI*F1
C
C        Compute D11*inv( B11 )*B12 + B12'*inv( B11' )*D11' - D12 in Q.
C
         Q( 1, 1 ) = D( 1, 3 )
         Q( 1, 2 ) = D( 1, 4 )
         Q( 2, 2 ) = D( 2, 4 )
C
         CALL DSYR2K( 'Upper', 'Transpose', 2, 2, ONE, U( 1, 3 ), LDU,
     $                B( 1, 3 ), LDB, -ONE, Q, LDQ )
C
C        Compute inv( B22 ) in U22.
C
         R =  CR2/SMAX2
         T =  SR2/SMAX2
         F = -SR2/SMIN2
         G =  CR2/SMIN2
C
         R1 = CL2*R - SL2*F
         T1 = CL2*T - SL2*G
         F1 = CL2*F + SL2*R
         G1 = CL2*G + SL2*T
C
         U( 3, 3 ) = CO2*R1 - SI2*F1
         U( 4, 3 ) = CO2*T1 - SI2*G1
         U( 3, 4 ) = CO2*F1 + SI2*R1
         U( 4, 4 ) = CO2*G1 + SI2*T1
C
C        Compute H11 = -inv( B11' )*D11'*inv( B22 ) in U11.
C
         CALL DGEMM( 'No Transpose', 'No Transpose', 2, 2, 2, -ONE,
     $               U( 1, 3 ), LDU, U( 3, 3 ), LDU, ZERO, U, LDU )
C
C        Compute H21 = inv( B22' )*Q*inv( B22 ) in U21.
C
         CALL MB01RU( 'Upper', 'Transpose', 2, 2, ZERO, ONE, U( 3, 1 ),
     $                LDU, U( 3, 3 ), LDU, Q, LDQ, DWORK, 4, INFO )
         U( 4, 1 ) = U( 3, 2 )
C
         S = -( U( 1, 1 ) + U( 2, 2 ) )
C
C        Compute Y1, the first two columns of Y = H*H - s*H + t*I4,
C        where H = ( Hij ), i,j = 1,2, H12 = 0, t = det(H22).
C        H is real lower Hamiltonian block triangular with the
C        desired eigenvalues in the leading positions.
C
         T = U( 1, 1 )*U( 2, 2 ) - U( 2, 1 )*U( 1, 2 )
C
         CALL DLACPY( 'Full', 4, 2, U, LDU, Q, LDQ )
         Q( 1, 3 ) = U( 1, 1 ) - S
         Q( 2, 3 ) = U( 2, 1 )
         Q( 3, 3 ) = U( 1, 1 )
         Q( 4, 3 ) = U( 2, 1 )
         Q( 1, 4 ) = U( 1, 2 )
         Q( 2, 4 ) = U( 2, 2 ) - S
         Q( 3, 4 ) = U( 1, 2 )
         Q( 4, 4 ) = U( 2, 2 )
         CALL DGEMM( 'No Transpose', 'No Transpose', 4, 2, 2, ONE,
     $               Q, LDQ, Q( 1, 3 ), LDQ, ZERO, U, LDU )
         CALL DGEMM( 'Transpose', 'No Transpose', 2, 2, 2, -ONE,
     $               Q( 3, 3 ), LDQ, Q( 3, 1 ), LDQ, ONE, U( 3, 1 ),
     $               LDU )
         U( 1, 1 ) = U( 1, 1 ) + T
         U( 2, 2 ) = U( 2, 2 ) + T
C
C        Compute the relevant part of the orthogonal symplectic
C        matrix U performing the symplectic QR factorization of Y1.
C        Workspace: need   10.
C
         ICS   = 1
         ITAU  = ICS  + 4
         IWRK2 = ITAU + 2
         CALL MB04SU( 2, 2, U( 1, 1 ), LDU, U( 3, 1 ), LDU,
     $                DWORK( ICS ), DWORK( ITAU ), DWORK( IWRK2 ),
     $                LDWORK-IWRK2+1, INFO )
         CALL MB04WU( 'No Transpose', 'No Transpose', 2, 2, 2,
     $                U( 1, 1 ), LDU, U( 3, 1 ), LDU, DWORK( ICS ),
     $                DWORK( ITAU ), DWORK( IWRK2 ), LDWORK-IWRK2+1,
     $                INFO )
C
C        Compute J*U in U.
C
         U( 1, 3 ) =  U( 1, 1 )
         U( 2, 3 ) =  U( 2, 1 )
         U( 1, 4 ) =  U( 1, 2 )
         U( 2, 4 ) =  U( 2, 2 )
C
         U( 1, 1 ) = -U( 3, 1 )
         U( 2, 1 ) = -U( 4, 1 )
         U( 1, 2 ) = -U( 3, 2 )
         U( 2, 2 ) = -U( 4, 2 )
C
         U( 3, 1 ) = -U( 1, 3 )
         U( 4, 1 ) = -U( 2, 3 )
         U( 3, 2 ) = -U( 1, 4 )
         U( 4, 2 ) = -U( 2, 4 )
C
         U( 3, 3 ) =  U( 1, 1 )
         U( 4, 3 ) =  U( 2, 1 )
         U( 3, 4 ) =  U( 1, 2 )
         U( 4, 4 ) =  U( 2, 2 )
C
C        Compute U'*B using structure.
C
         CALL DGEMM( 'Transpose', 'No Transpose', 4, 2, 2, ONE, U,
     $               LDU, B, LDB, ZERO, Q, LDQ )
         CALL DGEMM( 'Transpose', 'No Transpose', 4, 2, 4, ONE, U,
     $               LDU, B( 1, 3 ), LDB, ZERO, Q( 1, 3 ), LDQ )
C
C        Determine Q using different elimination orders in the RQ and
C        QR factorizations of U'*B.
C        Workspace: need   12.
C
         ITAU  = 1
         IWRK1 = ITAU + N
         CALL DGERQ2( N, N, Q, LDQ, DWORK( ITAU ), DWORK( IWRK1 ),
     $                INFO )
         IR    = IWRK1
         IWRK2 = IR + 4
         DWORK( IR )   = Q( 3, 3 )
         DWORK( IR+1 ) = Q( 3, 4 )
         DWORK( IR+2 ) = ZERO
         DWORK( IR+3 ) = Q( 4, 4 )
         CALL DORGR2( N, N, N, Q, LDQ, DWORK( ITAU ), DWORK( IWRK2 ),
     $                INFO )
C
         DO 20 I = 2, N
            CALL DSWAP( N-I+1, Q( I, I-1 ), 1, Q( I-1, I ), LDQ )
   20    CONTINUE
C
         CALL DGEQR2( 2, 2, DWORK( IR ), 2, DWORK( ITAU ),
     $                DWORK( IWRK2 ), INFO )
         CALL DORM2R( 'Right', 'No Transpose', N, 2, 2, DWORK( IR ), 2,
     $                DWORK( ITAU ), Q( 1, 3 ), LDQ, DWORK( IWRK2 ),
     $                INFO )
      ELSE
C
         G = TWO*B( 1, 1 )*B( 2, 2 )*D( 1, 1 )
         CALL DLARTG( B( 1, 1 )*B( 2, 2 )*D( 1, 2 ), G, CO, SI, R )
         Q( 1, 1 ) =  CO
         Q( 2, 1 ) = -SI
         Q( 1, 2 ) =  SI
         Q( 2, 2 ) =  CO
         CALL DLARTG( B( 1, 1 )*Q( 1, 1 ) + B( 1, 2 )*Q( 2, 1 ),
     $                B( 2, 2 )*Q( 2, 1 ), CO, SI, R )
         U( 1, 1 ) =  CO
         U( 2, 1 ) =  SI
         U( 1, 2 ) = -SI
         U( 2, 2 ) =  CO
      END IF
C
      RETURN
C *** Last line of MB03GD ***
      END
