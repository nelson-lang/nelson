      SUBROUTINE MB03GZ( Z11, Z12, Z22, H11, H12, CO1, SI1, CO2, SI2 )
C
C     PURPOSE
C
C     To compute a unitary matrix Q and a unitary symplectic matrix U
C     for a complex regular 2-by-2 skew-Hamiltonian/Hamiltonian pencil
C     aS - bH with S = J Z' J' Z, where
C
C            (  Z11  Z12  )         (  H11  H12  )
C        Z = (            ) and H = (            ),
C            (   0   Z22  )         (   0  -H11' )
C
C     such that U' Z Q, (J Q J' )' H Q are both upper triangular, but the  
C     eigenvalues of (J Q J')' ( aS - bH ) Q are in reversed order.
C     The matrices Q and U are represented by
C
C            (  CO1  SI1  )         (  CO2  SI2  )
C        Q = (            ) and U = (            ), respectively.
C            ( -SI1' CO1  )         ( -SI2' CO2  )
C
C     The notation M' denotes the conjugate transpose of the matrix M.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     Z11     (input) COMPLEX*16
C             Upper left element of the non-trivial factor Z in the
C             factorization of S.
C
C     Z12     (input) COMPLEX*16
C             Upper right element of the non-trivial factor Z in the
C             factorization of S.
C
C     Z22     (input) COMPLEX*16
C             Lower right element of the non-trivial factor Z in the
C             factorization of S.
C
C     H11     (input) COMPLEX*16
C             Upper left element of the Hamiltonian matrix H.
C
C     H12     (input) COMPLEX*16
C             Upper right element of the Hamiltonian matrix H.
C
C     CO1     (output) DOUBLE PRECISION
C             Upper left element of Q.
C
C     SI1     (output) COMPLEX*16
C             Upper right element of Q.
C
C     CO2     (output) DOUBLE PRECISION
C             Upper left element of U.
C
C     SI2     (output) COMPLEX*16
C             Upper right element of U.
C
C     METHOD
C
C     The algorithm uses unitary and unitary symplectic transformations
C     as described on page 37 in [1].
C
C     REFERENCES
C
C     [1] Benner, P., Byers, R., Mehrmann, V. and Xu, H.
C         Numerical Computation of Deflating Subspaces of Embedded
C         Hamiltonian Pencils.
C         Tech. Rep. SFB393/99-15, Technical University Chemnitz,
C         Germany, June 1999.
C
C     NUMERICAL ASPECTS
C
C     The algorithm is numerically backward stable.
C
C     CONTRIBUTOR
C
C     Matthias Voigt, Fakultaet fuer Mathematik, Technische Universitaet
C     Chemnitz, April 21, 2009.
C
C     REVISIONS
C
C     V. Sima, Aug. 2009 (SLICOT version of the routine ZHAFEX).
C     V. Sima, Dec. 2010.
C     M. Voigt, Jan. 2012.
C
C     KEYWORDS
C
C     Eigenvalue exchange, skew-Hamiltonian/Hamiltonian pencil, upper
C     triangular matrix.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   TWO
      PARAMETER          ( TWO = 2.0D+0 )
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION   CO1, CO2
      COMPLEX*16         H11, H12, SI1, SI2, Z11, Z12, Z22
C
C     .. Local Scalars ..
      COMPLEX*16         D, G, TMP
C
C     .. External Subroutines ..
      EXTERNAL           ZLARTG
C
C     .. Intrinsic Functions ..
      INTRINSIC          DBLE, DCONJG
C
C     .. Executable Statements ..
C
C     Computations.
C
      G = TWO*DBLE( H11*DCONJG( Z11 )*Z22 )
      D = Z22*DCONJG( Z11 )*H12 +
     $      ( DCONJG( Z22 )*Z12 - DCONJG( Z12 )*Z22 )*DCONJG( H11 )
      CALL ZLARTG( D, G, CO1, SI1, TMP )
      D =  Z11*CO1 - Z12*DCONJG( SI1 )
      G = -Z22*DCONJG( SI1 )
      CALL ZLARTG( D, G, CO2, SI2, TMP )
      SI2 = -SI2
C
      RETURN
C *** Last line of MB03GZ ***
      END 
