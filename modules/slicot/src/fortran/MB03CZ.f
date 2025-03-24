      SUBROUTINE MB03CZ( A, LDA, B, LDB, D, LDD, CO1, SI1, CO2, SI2,
     $                   CO3, SI3 )
C
C     PURPOSE
C
C     To compute unitary matrices Q1, Q2, and Q3 for a complex 2-by-2
C     regular pencil aAB - bD, with A, B, D upper triangular, such that
C     Q3' A Q2, Q2' B Q1, Q3' D Q1 are still upper triangular, but the
C     eigenvalues are in reversed order. The matrices Q1, Q2, and Q3 are
C     represented by
C
C          (  CO1  SI1  )       (  CO2  SI2  )       (  CO3  SI3  )
C     Q1 = (            ), Q2 = (            ), Q3 = (            ).
C          ( -SI1' CO1  )       ( -SI2' CO2  )       ( -SI3' CO3  )
C
C     The notation M' denotes the conjugate transpose of the matrix M.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     A       (input) COMPLEX*16 array, dimension (LDA, 2)
C             On entry, the leading 2-by-2 upper triangular part of
C             this array must contain the matrix A of the pencil.
C             The (2,1) entry is not referenced.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= 2.
C
C     B       (input) COMPLEX*16 array, dimension (LDB, 2)
C             On entry, the leading 2-by-2 upper triangular part of
C             this array must contain the matrix B of the pencil.
C             The (2,1) entry is not referenced.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= 2.
C
C     D       (input) COMPLEX*16 array, dimension (LDD, 2)
C             On entry, the leading 2-by-2 upper triangular part of
C             this array must contain the matrix D of the pencil.
C             The (2,1) entry is not referenced.
C
C     LDD     INTEGER
C             The leading dimension of the array D.  LDD >= 2.
C
C     CO1     (output) DOUBLE PRECISION
C             The upper left element of the unitary matrix Q1.
C
C     SI1     (output) COMPLEX*16
C             The upper right element of the unitary matrix Q1.
C
C     CO2     (output) DOUBLE PRECISION
C             The upper left element of the unitary matrix Q2.
C
C     SI2     (output) COMPLEX*16
C             The upper right element of the unitary matrix Q2.
C
C     CO3     (output) DOUBLE PRECISION
C             The upper left element of the unitary matrix Q3.
C
C     SI3     (output) COMPLEX*16
C             The upper right element of the unitary matrix Q3.
C
C     METHOD
C
C     The algorithm uses unitary transformations as described on page 37
C     in [1].
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
C     V. Sima, Aug. 2009 (SLICOT version of the routine ZBTFEX).
C     V. Sima, Nov. 2009, Nov. 2010, Dec. 2010.
C     M. Voigt, Jan. 2012.
C
C     KEYWORDS
C
C     Eigenvalue exchange, matrix pencil, upper triangular matrix.
C
C     ******************************************************************
C
C     .. Scalar Arguments ..
      INTEGER            LDA, LDB, LDD
      DOUBLE PRECISION   CO1, CO2, CO3
      COMPLEX*16         SI1, SI2, SI3
C
C     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), B( LDB, * ), D( LDD, * )
C
C     .. Local Scalars ..
      COMPLEX*16         F, G, TMP
C
C     .. External Subroutines ..
      EXTERNAL           ZLARTG
C
C     .. Executable Statements ..
C
C     For efficiency, the input arguments are not tested.
C
C     Computations.
C
      G = A( 1, 1 )*B( 1, 1 )*D( 2, 2 ) - A( 2, 2 )*B( 2, 2 )*D( 1, 1 )
      F = ( A( 1, 1 )*B( 1, 2 ) + A( 1, 2 )*B( 2, 2 ) )*D( 2, 2 ) -
     $      A( 2, 2 )*B( 2, 2 )*D( 1, 2 )
      CALL ZLARTG( F, G, CO1, SI1, TMP )
      F = ( A( 1, 2 )*D( 2, 2 ) - A( 2, 2 )*D( 1, 2 ) )*B( 1, 1 ) +
     $      A( 2, 2 )*D( 1, 1 )*B( 1, 2 )
      CALL ZLARTG( F, G, CO2, SI2, TMP )
      F = ( B( 1, 2 )*D( 1, 1 ) - B( 1, 1 )*D( 1, 2 ) )*A( 1, 1 ) +
     $      A( 1, 2 )*B( 2, 2 )*D( 1, 1 )
      CALL ZLARTG( F, G, CO3, SI3, TMP )
C
      RETURN
C *** Last line of MB03CZ ***
      END
