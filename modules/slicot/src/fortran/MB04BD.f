      SUBROUTINE MB04BD( JOB, COMPQ1, COMPQ2, N, A, LDA, DE, LDDE, C1,
     $                   LDC1, VW, LDVW, Q1, LDQ1, Q2, LDQ2, B, LDB, F,
     $                   LDF, C2, LDC2, ALPHAR, ALPHAI, BETA, IWORK,
     $                   LIWORK, DWORK, LDWORK, INFO )
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
C     To compute the eigenvalues of a real N-by-N skew-Hamiltonian/
C     Hamiltonian pencil aS - bH with
C
C           (  A  D  )         (  C  V  )
C       S = (        ) and H = (        ).                           (1)
C           (  E  A' )         (  W -C' )
C
C     Optionally, if JOB = 'T', decompositions of S and H will be
C     computed via orthogonal transformations Q1 and Q2 as follows:
C
C                       (  Aout  Dout  )
C       Q1' S J Q1 J' = (              ),
C                       (   0    Aout' )
C
C                       (  Bout  Fout  )
C       J' Q2' J S Q2 = (              ) =: T,                       (2)
C                       (   0    Bout' )
C
C                  (  C1out  Vout  )            (  0  I  )
C       Q1' H Q2 = (               ), where J = (        )
C                  (  0     C2out' )            ( -I  0  )
C
C     and Aout, Bout, C1out are upper triangular, C2out is upper quasi-
C     triangular and Dout and Fout are skew-symmetric. The notation M'
C     denotes the transpose of the matrix M.
C     Optionally, if COMPQ1 = 'I', the orthogonal transformation matrix
C     Q1 will be computed.
C     Optionally, if COMPQ2 = 'I', the orthogonal transformation matrix
C     Q2 will be computed.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOB     CHARACTER*1
C             Specifies the computation to be performed, as follows:
C             = 'E': compute the eigenvalues only; S and H will not
C                    necessarily be transformed as in (2).
C             = 'T': put S and H into the forms in (2) and return the
C                    eigenvalues in ALPHAR, ALPHAI and BETA.
C
C     COMPQ1  CHARACTER*1
C             Specifies whether to compute the orthogonal transformation
C             matrix Q1, as follows:
C             = 'N':  Q1 is not computed;
C             = 'I':  the array Q1 is initialized internally to the unit
C                     matrix, and the orthogonal matrix Q1 is returned;
C             = 'U':  the array Q1 contains an orthogonal matrix Q on
C                     entry, and the product Q*Q1 is returned, where Q1
C                     is the product of the orthogonal transformations
C                     that are applied to the pencil aS - bH to reduce
C                     S and H to the forms in (2), for COMPQ1 = 'I'.
C
C     COMPQ2  CHARACTER*1
C             Specifies whether to compute the orthogonal transformation
C             matrix Q2, as follows:
C             = 'N':  Q2 is not computed;
C             = 'I':  on exit, the array Q2 contains the orthogonal
C                     matrix Q2;
C             = 'U':  on exit, the array Q2 contains the matrix product
C                     J*Q*J'*Q2, where Q2 is the product of the
C                     orthogonal transformations that are applied to
C                     the pencil aS - bH to reduce S and H to the forms
C                     in (2), for COMPQ2 = 'I'.
C                     Setting COMPQ2 <> 'N' assumes COMPQ2 = COMPQ1.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the pencil aS - bH.  N has to be even.
C
C     A       (input/output) DOUBLE PRECISION array, dimension
C                            (LDA, N/2)
C             On entry, the leading N/2-by-N/2 part of this array must
C             contain the matrix A.
C             On exit, if JOB = 'T', the leading N/2-by-N/2 part of this
C             array contains the matrix Aout; otherwise, it contains the
C             upper triangular matrix A obtained just before the
C             application of the periodic QZ algorithm.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1, N/2).
C
C     DE      (input/output) DOUBLE PRECISION array, dimension
C                            (LDDE, N/2+1)
C             On entry, the leading N/2-by-N/2 strictly lower triangular
C             part of this array must contain the strictly lower
C             triangular part of the skew-symmetric matrix E, and the
C             N/2-by-N/2 strictly upper triangular part of the submatrix
C             in the columns 2 to N/2+1 of this array must contain the
C             strictly upper triangular part of the skew-symmetric
C             matrix D.
C             The entries on the diagonal and the first superdiagonal of
C             this array need not be set, but are assumed to be zero.
C             On exit, if JOB = 'T', the leading N/2-by-N/2 strictly
C             upper triangular part of the submatrix in the columns
C             2 to N/2+1 of this array contains the strictly upper
C             triangular part of the skew-symmetric matrix Dout.
C             If JOB = 'E', the leading N/2-by-N/2 strictly upper
C             triangular part of the submatrix in the columns 2 to N/2+1
C             of this array contains the strictly upper triangular part
C             of the skew-symmetric matrix D just before the application
C             of the periodic QZ algorithm. The remaining entries are
C             meaningless.
C
C     LDDE    INTEGER
C             The leading dimension of the array DE.
C             LDDE >= MAX(1, N/2).
C
C     C1      (input/output) DOUBLE PRECISION array, dimension
C                           (LDC1, N/2)
C             On entry, the leading N/2-by-N/2 part of this array must
C             contain the matrix C1 = C.
C             On exit, if JOB = 'T', the leading N/2-by-N/2 part of this
C             array contains the matrix C1out; otherwise, it contains the
C             upper triangular matrix C1 obtained just before the
C             application of the periodic QZ algorithm.
C
C     LDC1    INTEGER
C             The leading dimension of the array C1.
C             LDC1 >= MAX(1, N/2).
C
C     VW      (input/output) DOUBLE PRECISION array, dimension
C                            (LDVW, N/2+1)
C             On entry, the leading N/2-by-N/2 lower triangular part of
C             this array must contain the lower triangular part of the
C             symmetric matrix W, and the N/2-by-N/2 upper triangular
C             part of the submatrix in the columns 2 to N/2+1 of this
C             array must contain the upper triangular part of the
C             symmetric matrix V.
C             On exit, if JOB = 'T', the N/2-by-N/2 part in the columns
C             2 to N/2+1 of this array contains the matrix Vout.
C             If JOB = 'E', the N/2-by-N/2 part in the columns 2 to
C             N/2+1 of this array contains the matrix V just before the
C             application of the periodic QZ algorithm.
C
C     LDVW    INTEGER
C             The leading dimension of the array VW.
C             LDVW >= MAX(1, N/2).
C
C     Q1      (input/output) DOUBLE PRECISION array, dimension (LDQ1, N)
C             On entry, if COMPQ1 = 'U', then the leading N-by-N part of
C             this array must contain a given matrix Q, and on exit,
C             the leading N-by-N part of this array contains the product
C             of the input matrix Q and the transformation matrix Q1
C             used to transform the matrices S and H.
C             On exit, if COMPQ1 = 'I', then the leading N-by-N part of
C             this array contains the orthogonal transformation matrix
C             Q1.
C             If COMPQ1 = 'N', this array is not referenced.
C
C     LDQ1    INTEGER
C             The leading dimension of the array Q1.
C             LDQ1 >= 1,         if COMPQ1 = 'N';
C             LDQ1 >= MAX(1, N), if COMPQ1 = 'I' or COMPQ1 = 'U'.
C
C     Q2      (output) DOUBLE PRECISION array, dimension (LDQ2, N)
C             On exit, if COMPQ2 = 'U', then the leading N-by-N part of
C             this array contains the product of the matrix J*Q*J' and
C             the transformation matrix Q2 used to transform the
C             matrices S and H.
C             On exit, if COMPQ2 = 'I', then the leading N-by-N part of
C             this array contains the orthogonal transformation matrix
C             Q2.
C             If COMPQ2 = 'N', this array is not referenced.
C
C     LDQ2    INTEGER
C             The leading dimension of the array Q2.
C             LDQ2 >= 1,         if COMPQ2 = 'N';
C             LDQ2 >= MAX(1, N), if COMPQ2 = 'I' or COMPQ2 = 'U'.
C
C     B       (output) DOUBLE PRECISION array, dimension (LDB, N/2)
C             On exit, if JOB = 'T', the leading N/2-by-N/2 part of this
C             array contains the matrix Bout; otherwise, it contains the
C             upper triangular matrix B obtained just before the
C             application of the periodic QZ algorithm.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= MAX(1, N/2).
C
C     F       (output) DOUBLE PRECISION array, dimension (LDF, N/2)
C             On exit, if JOB = 'T', the leading N/2-by-N/2 strictly
C             upper triangular part of this array contains the strictly
C             upper triangular part of the skew-symmetric matrix Fout.
C             If JOB = 'E', the leading N/2-by-N/2 strictly upper
C             triangular part of this array contains the strictly upper
C             triangular part of the skew-symmetric matrix F just before
C             the application of the periodic QZ algorithm.
C             The entries on the leading N/2-by-N/2 lower triangular
C             part of this array are not referenced.
C
C     LDF     INTEGER
C             The leading dimension of the array F.  LDF >= MAX(1, N/2).
C
C     C2      (output) DOUBLE PRECISION array, dimension (LDC2, N/2)
C             On exit, if JOB = 'T', the leading N/2-by-N/2 part of this
C             array contains the matrix C2out; otherwise, it contains
C             the upper Hessenberg matrix C2 obtained just before the
C             application of the periodic QZ algorithm.
C
C     LDC2    INTEGER
C             The leading dimension of the array C2.
C             LDC2 >= MAX(1, N/2).
C
C     ALPHAR  (output) DOUBLE PRECISION array, dimension (N/2)
C             The real parts of each scalar alpha defining an eigenvalue
C             of the pencil aS - bH.
C
C     ALPHAI  (output) DOUBLE PRECISION array, dimension (N/2)
C             The imaginary parts of each scalar alpha defining an
C             eigenvalue of the pencil aS - bH.
C             If ALPHAI(j) is zero, then the j-th eigenvalue is real.
C
C     BETA    (output) DOUBLE PRECISION array, dimension (N/2)
C             The scalars beta that define the eigenvalues of the pencil
C             aS - bH.
C             Together, the quantities alpha = (ALPHAR(j),ALPHAI(j)) and
C             beta = BETA(j) represent the j-th eigenvalue of the pencil
C             aS - bH, in the form lambda = alpha/beta. Since lambda may
C             overflow, the ratios should not, in general, be computed.
C             Due to the skew-Hamiltonian/Hamiltonian structure of the
C             pencil, for every eigenvalue lambda, -lambda is also an
C             eigenvalue, and thus it has only to be saved once in
C             ALPHAR, ALPHAI and BETA.
C             Specifically, only eigenvalues with imaginary parts
C             greater than or equal to zero are stored; their conjugate
C             eigenvalues are not stored. If imaginary parts are zero
C             (i.e., for real eigenvalues), only positive eigenvalues
C             are stored.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (LIWORK)
C
C     LIWORK  INTEGER
C             The dimension of the array IWORK.
C             LIWORK >= N/2+12.
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal LDWORK.
C             On exit, if INFO = -27, DWORK(1) returns the minimum
C             value of LDWORK.
C
C     LDWORK  INTEGER
C             The dimension of the array DWORK.
C             If JOB = 'E' and COMPQ1 = 'N' and COMPQ2 = 'N',
C                LDWORK >= N**2 + MAX(N,32);
C             if JOB = 'T' or COMPQ1 <> 'N' or COMPQ2 <> 'N',
C                LDWORK >= 2*N**2 + MAX(N,32).
C             For good performance LDWORK should generally be larger.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0: succesful exit;
C             < 0: if INFO = -i, the i-th argument had an illegal value;
C             = 1: problem during computation of the eigenvalues;
C             = 2: periodic QZ algorithm did not converge in the SLICOT
C                  Library subroutine MB03BD.
C
C     METHOD
C
C     The algorithm uses Givens rotations and Householder reflections to
C     annihilate elements in S, T, and H such that A, B, and C1 are
C     upper triangular and C2 is upper Hessenberg. Finally, the periodic
C     QZ algorithm is applied to transform C2 to upper quasi-triangular
C     form while A, B, and C1 stay in upper triangular form.
C     See also page 27 in [1] for more details.
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
C                                                               3
C     The algorithm is numerically backward stable and needs O(N ) real
C     floating point operations.
C
C     CONTRIBUTOR
C
C     Matthias Voigt, Fakultaet fuer Mathematik, Technische Universitaet
C     Chemnitz, October 16, 2008.
C
C     REVISIONS
C
C     V. Sima, Oct. 2009 (SLICOT version of the routine DHAUTR).
C     V. Sima, Nov. 2010.
C
C     KEYWORDS
C
C     periodic QZ algorithm, upper (quasi-)triangular matrix,
C     skew-Hamiltonian/Hamiltonian pencil.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ZERO, HALF, ONE, TWO
      PARAMETER          ( ZERO = 0.0D+0, HALF = 0.5D+0, ONE = 1.0D+0,
     $                     TWO  = 2.0D+0 )
C
C     .. Scalar Arguments ..
      CHARACTER          COMPQ1, COMPQ2, JOB
      INTEGER            INFO, LDA, LDB, LDC1, LDC2, LDDE, LDF, LDQ1,
     $                   LDQ2, LDVW, LDWORK, LIWORK, N
C
C     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), ALPHAI( * ), ALPHAR( * ),
     $                   B( LDB, * ), BETA( * ), C1( LDC1, * ),
     $                   C2( LDC2, * ), DE( LDDE, * ), DWORK( * ),
     $                   F( LDF, * ), Q1( LDQ1, * ), Q2( LDQ2, * ),
     $                   VW( LDVW, * )
C
C     .. Local Scalars ..
      LOGICAL            LCMPQ1, LCMPQ2, LINIQ1, LINIQ2, LTRI, LUPDQ1,
     $                   LUPDQ2
      CHARACTER*16       CMPQ, CMPSC
      INTEGER            I, IMAT, IWARN, IWRK, J, K, M, MJ1, MJ2, MJ3,
     $                   MK1, MK2, MK3, MM, OPTDW
      DOUBLE PRECISION   BASE, CO, EMAX, EMIN, MU, NU, PREC, SI, TMP1,
     $                   TMP2
      COMPLEX*16         EIG
C
C     .. Local Arrays ..
      INTEGER            IDUM( 1 )
      DOUBLE PRECISION   DUM( 1 )
C
C     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DDOT, DLAMCH, DLAPY2
      EXTERNAL           DDOT, DLAMCH, DLAPY2, LSAME
C
C     .. External Subroutines ..
      EXTERNAL           DAXPY, DCOPY, DGEMM, DLACPY, DLARF, DLARFG,
     $                   DLARTG, DLASET, DROT, DSYMV, DSYR2, MA02AD,
     $                   MB01LD, MB01MD, MB01ND, MB03BD, XERBLA
C
C     ... Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCMPLX, DIMAG, INT, MAX, MIN, MOD,
     $                   SQRT
C
C     .. Executable Statements ..
C
C     Decode the input arguments.
C
      M  = N/2
      MM = M*M
      LTRI   = LSAME( JOB,    'T' )
      LINIQ1 = LSAME( COMPQ1, 'I' )
      LINIQ2 = LSAME( COMPQ2, 'I' )
      LUPDQ1 = LSAME( COMPQ1, 'U' )
      LUPDQ2 = LSAME( COMPQ2, 'U' )
      LCMPQ1 = LUPDQ1 .OR. LINIQ1
      LCMPQ2 = LUPDQ2 .OR. LINIQ2
      IF( LTRI .OR. LCMPQ1 .OR. LCMPQ2 ) THEN
         OPTDW = 8*MM + MAX( N, 32 )
      ELSE
         OPTDW = 4*MM + MAX( N, 32 )
      END IF
C
C     Test the input arguments.
C
      INFO = 0
      IF( .NOT.( LSAME( JOB, 'E' ) .OR. LTRI ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( LSAME( COMPQ1, 'N' ) .OR. LCMPQ1 ) ) THEN
         INFO = -2
      ELSE IF( .NOT.( LSAME( COMPQ2, 'N' ) .OR. LCMPQ2 ) ) THEN
         INFO = -3
      ELSE IF( ( LINIQ2 .AND. .NOT.LINIQ1 ) .OR.
     $         ( LUPDQ2 .AND. .NOT.LUPDQ1 ) ) THEN
         INFO = -3
      ELSE IF( N.LT.0 .OR. MOD( N, 2 ).NE.0 ) THEN
         INFO = -4
      ELSE IF(  LDA.LT.MAX( 1, M ) ) THEN
         INFO = -6
      ELSE IF( LDDE.LT.MAX( 1, M ) ) THEN
         INFO = -8
      ELSE IF( LDC1.LT.MAX( 1, M ) ) THEN
         INFO = -10
      ELSE IF( LDVW.LT.MAX( 1, M ) ) THEN
         INFO = -12
      ELSE IF( LDQ1.LT.1 .OR. ( LCMPQ1 .AND. LDQ1.LT.N ) ) THEN
         INFO = -14
      ELSE IF( LDQ2.LT.1 .OR. ( LCMPQ2 .AND. LDQ2.LT.N ) ) THEN
         INFO = -16
      ELSE IF(  LDB.LT.MAX( 1, M ) ) THEN
         INFO = -18
      ELSE IF(  LDF.LT.MAX( 1, M ) ) THEN
         INFO = -20
      ELSE IF( LDC2.LT.MAX( 1, M ) ) THEN
         INFO = -22
      ELSE IF( LIWORK.LT.M+12 ) THEN
         INFO = -27
      ELSE IF( LDWORK.LT.OPTDW ) THEN
         INFO = -29
      END IF
      IF( INFO.NE.0) THEN
         CALL XERBLA( 'MB04BD', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( N.EQ.0 ) THEN
         DWORK( 1 ) = ONE
         RETURN
      END IF
C
C     Determine machine constants.
C
      BASE = DLAMCH( 'Base' )
      EMIN = DLAMCH( 'Minimum Exponent' )
      EMAX = DLAMCH( 'Largest Exponent' )
      PREC = DLAMCH( 'Precision' )
C
C     STEP 1: Reduce S to skew-Hamiltonian triangular form.
C
      IF( LINIQ1 )
     $   CALL DLASET( 'Full', N, N, ZERO, ONE, Q1, LDQ1 )
C
      DUM( 1 ) = ZERO
C
      DO 10 K = 1, M - 1
C
C        Generate elementary reflector H(k) = I - nu * v * v' to
C        annihilate E(k+2:m,k).
C
         MK2  = MIN( K+2, M )
         MK3  = MK2 + 1
         TMP1 = DE( K+1, K )
         CALL DLARFG( M-K, TMP1, DE( MK2, K ), 1, NU )
         IF( NU.NE.ZERO ) THEN
            DE( K+1, K ) = ONE
C
C           Apply H(k) from both sides to E(k+1:m,k+1:m).
C           Compute  x := nu * E(k+1:m,k+1:m) * v.
C
            CALL MB01MD( 'Lower', M-K, NU, DE( K+1, K+1 ), LDDE,
     $                   DE( K+1, K ), 1, ZERO, DWORK, 1 )
C
C           Compute  w := x - 1/2 * nu * (x'*v) * v in x.
C
            MU = -HALF*NU*DDOT( M-K, DWORK, 1, DE( K+1, K ), 1 )
            CALL DAXPY( M-K, MU, DE( K+1, K ), 1, DWORK, 1 )
C
C           Apply the transformation as a skew-symmetric rank-2 update:
C                E := E + v * w' - w * v'.
C
            CALL MB01ND( 'Lower', M-K, ONE, DE( K+1, K ), 1, DWORK, 1,
     $                   DE( K+1, K+1 ), LDDE )
C
C           Apply H(k) to W(k+1:m,1:k) from the left (and implicitly to
C           W(1:k,k+1:m) from the right).
C
            CALL DLARF( 'Left', M-K, K, DE( K+1, K ), 1, NU,
     $                  VW( K+1, 1 ), LDVW, DWORK )
C
C           Apply H(k) from both sides to W(k+1:m,k+1:m).
C           Compute  x := nu * W(k+1:m,k+1:m) * v.
C
            CALL DSYMV( 'Lower', M-K, NU, VW( K+1, K+1 ), LDVW,
     $                  DE( K+1, K ), 1, ZERO, DWORK, 1 )
C
C           Compute  w := x - 1/2 * nu * (x'*v) * v.
C
            MU = -HALF*NU*DDOT( M-K, DWORK, 1, DE( K+1, K ), 1 )
            CALL DAXPY( M-K, MU, DE( K+1, K ), 1, DWORK, 1 )
C
C           Apply the transformation as a rank-2 update:
C                W := W - v * w' - w * v'.
C
            CALL DSYR2( 'Lower', M-K, -ONE, DE( K+1, K ), 1, DWORK, 1,
     $                  VW( K+1, K+1 ), LDVW )
C
C           Apply H(k) from the right hand side to A(1:m,k+1:m) and
C           C1(1:m,k+1:m).
C
            CALL DLARF( 'Right', M, M-K, DE( K+1, K ), 1, NU,
     $                  A( 1, K+1 ), LDA, DWORK )
            CALL DLARF( 'Right', M, M-K, DE( K+1, K ), 1, NU,
     $                  C1( 1, K+1 ), LDC1, DWORK )
C
            IF( LCMPQ1 ) THEN
C
C              Apply H(k) from the right hand side to Q1(1:n,m+k+1:n).
C
               CALL DLARF( 'Right', N, M-K, DE( K+1, K ), 1, NU,
     $                     Q1( 1, M+K+1 ), LDQ1, DWORK )
            END IF
            DE( K+1, K ) = TMP1
         END IF
C
C        Determine a Givens rotation to annihilate E(k+1,k) from the
C        left.
C
         TMP2 = A( K+1, K )
         CALL DLARTG( TMP2, DE( K+1, K ), CO, SI, A( K+1, K ) )
C
C        Update A, E and D.
C
         CALL DROT( M-K-1, DE( MK2, K+1 ), 1, A( K+1, MK2 ), LDA, CO,
     $              SI )
         CALL DROT( K, A( 1, K+1 ), 1, DE( 1, K+2 ), 1, CO, SI )
         CALL DROT( M-K-1, DE( K+1, MK3 ), LDDE, A( MK2, K+1 ), 1, CO,
     $              SI )
C
C        Update C1, W and V.
C
         CALL DROT( K, VW( K+1, 1 ), LDVW, C1( K+1, 1 ), LDC1, CO, -SI )
         CALL DROT( M-K-1, VW( MK2, K+1 ), 1, C1( K+1, MK2 ), LDC1, CO,
     $              -SI )
         CALL DROT( K, C1( 1, K+1 ), 1, VW( 1, K+2 ), 1, CO, SI )
         CALL DROT( M-K-1, VW( K+1, MK3 ), LDVW, C1( MK2, K+1 ), 1, CO,
     $              -SI )
C
C        Fix the diagonal part.
C
         TMP1 = C1( K+1, K+1 )
         TMP2 = VW( K+1, K+2 )
         C1( K+1, K+1 ) = ( CO - SI )*( CO + SI )*TMP1 +
     $                    CO*SI*( VW( K+1, K+1 ) + TMP2 )
         TMP1 = TWO*CO*SI*TMP1
         VW( K+1, K+2 ) = CO**2*TMP2 - SI**2*VW( K+1, K+1 ) - TMP1
         VW( K+1, K+1 ) = CO**2*VW( K+1, K+1 ) - SI**2*TMP2 - TMP1
C
         IF( LCMPQ1 ) THEN
C
C           Update Q1.
C
            CALL DROT( N, Q1( 1, K+1 ), 1, Q1( 1, M+K+1 ), 1, CO, SI )
         END IF
C
C        Generate elementary reflector P(k) to annihilate A(k+1:m,k).
C
         TMP1 = A( K, K )
         CALL DLARFG( M-K+1, TMP1, A( K+1, K ), 1, NU )
         IF( NU.NE.ZERO ) THEN
            A( K, K ) = ONE
C
C           Apply P(k) from the left hand side to A(k:m,k+1:m).
C
            CALL DLARF( 'Left', M-K+1, M-K, A( K, K ), 1, NU,
     $                  A( K, K+1 ), LDA, DWORK )
C
C           Apply P(k) to D(1:k-1,k:m) from the right (and implicitly
C           to D(k:m,1:k-1) from the left).
C
            CALL DLARF( 'Right', K-1, M-K+1, A( K, K ), 1, NU,
     $                  DE( 1, K+1 ), LDDE, DWORK )
C
C           Apply P(k) from both sides to D(k:m,k:m).
C           Compute  x := nu * D(k:m,k:m) * v.
C
            CALL MB01MD( 'Upper', M-K+1, NU, DE( K, K+1 ), LDDE,
     $                   A( K, K ), 1, ZERO, DWORK, 1 )
C
C           Compute  w := x - 1/2 * nu * (x'*v) * v in x.
C
            MU = -HALF*NU*DDOT( M-K+1, DWORK, 1, A( K, K ), 1 )
            CALL DAXPY( M-K+1, MU, A( K, K ), 1, DWORK, 1 )
C
C           Apply the transformation as a skew-symmetric rank-2 update:
C                D := D + v * w' - w * v'.
C
            CALL MB01ND( 'Upper', M-K+1, ONE, A( K, K ), 1, DWORK, 1,
     $                   DE( K, K+1 ), LDDE )
C
C           Apply P(k) from the left hand side to C1(k:m,k+1:m).
C
            CALL DLARF( 'Left', M-K+1, M, A( K, K ), 1, NU, C1( K, 1 ),
     $                  LDC1, DWORK )
C
C           Apply P(k) to V(1:k-1,k:m) from the right (and implicitly
C           to V(k:m,1:k-1) from the left).
C
            CALL DLARF( 'Right', K-1, M-K+1, A( K, K ), 1, NU,
     $                  VW( 1, K+1 ), LDVW, DWORK )
C
C           Apply P(k) from both sides to V(k:m,k:m).
C           Compute  x := nu * V(k:m,k:m) * v.
C
            CALL DSYMV( 'Upper', M-K+1, NU, VW( K, K+1 ), LDVW,
     $                  A( K, K ), 1, ZERO, DWORK, 1 )
C
C           Compute  w := x - 1/2 * nu * (x'*v) * v.
C
            MU = -HALF*NU*DDOT( M-K+1, DWORK, 1, A( K, K ), 1 )
            CALL DAXPY( M-K+1, MU, A( K, K ), 1, DWORK, 1 )
C
C           Apply the transformation as a rank-2 update:
C                V := V - v * w' - w * v'.
C
            CALL DSYR2( 'Upper', M-K+1, -ONE, A( K, K ), 1, DWORK, 1,
     $                  VW( K, K+1 ), LDVW )
C
            IF( LCMPQ1 ) THEN
C
C              Apply P(k) from the right hand side to Q1(1:n,k:m).
C
               CALL DLARF( 'Right', N, M-K+1, A( K, K ), 1, NU,
     $                     Q1( 1, K ), LDQ1, DWORK )
            END IF
            A( K, K ) = TMP1
         END IF
C
C        Set A(k+1:m,k) to zero in order to be able to apply MB03BD.
C
         CALL DCOPY( M-K, DUM, 0, A( K+1, K ), 1 )
   10 CONTINUE
C
C     The following operations do not preserve the Hamiltonian structure
C     of H. -C1 is copied to C2. The lower triangular part of W(1:m,1:m)
C     and its transpose are stored in DWORK. Then, the transpose of the
C     upper triangular part of V(1:m,1:m) is saved in the lower
C     triangular part of VW(1:m,2:m+1).
C
      CALL DLACPY( 'Full', M, M, A, LDA, B, LDB )
      CALL DLACPY( 'Upper', M, M, DE( 1, 2 ), LDDE, F, LDF )
C
      DO 30 J = 1, M
         DO 20 I = 1, M
            C2( I, J ) = -C1( I, J )
   20    CONTINUE
   30 CONTINUE
C
      CALL DLACPY( 'Lower', M, M, VW, LDVW, DWORK, M )
      CALL MA02AD( 'Lower', M, M, VW, LDVW, DWORK, M )
C
      CALL MA02AD( 'Upper', M, M, VW( 1, 2 ), LDVW, VW( 1, 2 ), LDVW )
C
      IF ( LCMPQ2 ) THEN
         CALL DLACPY( 'Full', M, M, Q1( M+1, M+1 ), LDQ1, Q2, LDQ2 )
C
         DO 50 J = 1, M
            DO 40 I = M + 1, N
               Q2( I, J ) = -Q1( I-M, J+M )
   40       CONTINUE
   50    CONTINUE
C
         DO 70 J = M + 1, N
            DO 60 I = 1, M
               Q2( I, J ) = -Q1( I+M, J-M )
   60       CONTINUE
   70    CONTINUE
C
         CALL DLACPY( 'Full', M, M, Q1, LDQ1, Q2( M+1, M+1 ), LDQ2 )
      END IF
C
C     STEP 2: Eliminations in H.
C
      DO 120 K = 1, M
         MK1 = MIN( K+1, M )
C
C        I. Annihilate W(k:m-1,k).
C
         DO 80 J = K, M - 1
            MJ3  = MIN( J+3, M+1 )
C
C           Determine a Givens rotation to annihilate W(j,k) from the
C           left.
C
            CALL DLARTG( DWORK( ( K-1 )*M+J+1 ), DWORK( ( K-1 )*M+J ),
     $                   CO, SI, TMP1 )
C
C           Update C2 and W.
C
            CALL DROT( M, C2( 1, J+1 ), 1, C2( 1, J ), 1, CO, SI )
            DWORK( ( K-1 )*M+J+1 ) = TMP1
            DWORK( ( K-1 )*M+J )   = ZERO
            CALL DROT( M-K, DWORK( K*M+J+1 ), M, DWORK( K*M+J ), M, CO,
     $                 SI )
C
C           Update A.
C
            CALL DROT( J, A( 1, J+1 ), 1, A( 1, J ), 1, CO, SI )
            TMP1          = -SI*A( J+1, J+1 )
            A( J+1, J+1 ) =  CO*A( J+1, J+1 )
C
            IF( LCMPQ1 ) THEN
C
C              Update Q1.
C
               CALL DROT( N, Q1( 1, M+J+1 ), 1, Q1( 1, M+J ), 1, CO, SI
     $                     )
            END IF
C
C           Determine a Givens rotation to annihilate A(j+1,j) from the
C           left.
C
            CALL DLARTG( A( J, J ), TMP1, CO, SI, TMP2 )
C
C           Update A and D.
C
            A( J, J ) = TMP2
            CALL DROT( M-J, A( J, J+1 ), LDA, A( J+1, J+1 ), LDA, CO, SI
     $                  )
            CALL DROT( J-1, DE( 1, J+1 ), 1, DE( 1, J+2 ), 1, CO, SI )
            CALL DROT( M-J-1, DE( J, MJ3 ), LDDE, DE( J+1, MJ3 ), LDDE,
     $                 CO, SI )
C
C           Update C1 and V.
C
            CALL DROT( M-K+1, C1( J, K ), LDC1, C1( J+1, K ), LDC1, CO,
     $                 SI )
            CALL DROT( M, VW( J, 2 ), LDVW, VW( J+1, 2 ), LDVW, CO, SI )
C
            IF( LCMPQ1 ) THEN
C
C              Update Q1.
C
               CALL DROT( N, Q1( 1, J ), 1, Q1( 1, J+1 ), 1, CO, SI )
            END IF
   80    CONTINUE
C
C        II. Annihilate W(m,k).
C
C        Determine a Givens rotation to annihilate W(m,k) from the left.
C
         CALL DLARTG( C1( M, K ), DWORK( M*K ), CO, SI, TMP1 )
C
C        Update C1 and W.
C
         C1( M, K )   = TMP1
         DWORK( M*K ) = ZERO
         CALL DROT( M-K, C1( M, MK1 ), LDC1, DWORK( M*MK1 ), M, CO, SI )
         CALL DROT( M, VW( M, 2 ), LDVW, C2( 1, M ), 1, CO, SI )
C
C        Update A and D.
C
         CALL DROT( M-1, A( 1, M ), 1, DE( 1, M+1 ), 1, CO, SI )
C
         IF( LCMPQ1 ) THEN
C
C           Update Q1.
C
            CALL DROT( N, Q1( 1, M ), 1, Q1( 1, N ), 1, CO, SI )
         END IF
C
C        III. Annihilate C1(k+1:m,k).
C
         DO 90 J = M, K + 1, -1
            MJ2  = MIN( J+2, M+1 )
C
C           Determine a Givens rotation to annihilate C1(j,k) from the
C           left.
C
            CALL DLARTG( C1( J-1, K ), C1( J, K ), CO, SI, TMP1 )
C
C           Update C1 and V.
C
            C1( J-1, K ) = TMP1
            C1( J, K )   = ZERO
            CALL DROT( M-K, C1( J-1, MK1 ), LDC1, C1( J, MK1 ), LDC1,
     $                 CO, SI )
            CALL DROT( M, VW( J-1, 2 ), LDVW, VW( J, 2 ), LDVW , CO, SI
     $                  )
C
C           Update A and D.
C
            TMP1          = -SI*A( J-1, J-1 )
            A( J-1, J-1 ) =  CO*A( J-1, J-1 )
            CALL DROT( M-J+1, A( J-1, J ), LDA, A( J, J ), LDA, CO, SI )
            CALL DROT( J-2, DE( 1, J ), 1, DE( 1, J+1 ), 1, CO, SI )
            CALL DROT( M-J, DE( J-1, MJ2 ), LDDE, DE( J, MJ2 ), LDDE,
     $                 CO, SI )
C
            IF( LCMPQ1 ) THEN
C
C              Update Q1.
C
               CALL DROT( N, Q1( 1, J-1 ), 1, Q1( 1, J ), 1, CO, SI )
            END IF
C
C           Determine a Givens rotation to annihilate A(j,j-1) from the
C           right.
C
            CALL DLARTG( A( J, J ), TMP1, CO, SI, TMP2 )
C
C           Update A.
C
            A( J, J ) = TMP2
            CALL DROT( J-1, A( 1, J ), 1, A( 1, J-1 ), 1, CO, SI )
C
C           Update C2 and W.
C
            CALL DROT( M, C2( 1, J ), 1, C2( 1, J-1 ), 1, CO, SI )
            CALL DROT( M-K+1, DWORK( ( K-1 )*M+J ), M,
     $                 DWORK( ( K-1)*M+J-1 ), M, CO, SI )
C
            IF( LCMPQ1 ) THEN
C
C              Update Q1.
C
               CALL DROT( N, Q1( 1, M+J ), 1, Q1( 1, M+J-1 ), 1, CO, SI
     $                     )
            END IF
   90    CONTINUE
C
C        IV. Annihilate W(k,k+1:m-1).
C
         DO 100 J = K + 1, M - 1
            MJ2  = MIN( J+2, M )
C
C           Determine a Givens rotation to annihilate W(k,j) from the
C           right.
C
            CALL DLARTG( DWORK( J*M+K ), DWORK( ( J-1 )*M+K ), CO,
     $                   SI, TMP1 )
C
C           Update C1 and W.
C
            CALL DROT( M, C1( 1, J+1 ), 1, C1( 1, J ), 1, CO, SI )
            DWORK( ( J-1 )*M+K ) = ZERO
            DWORK( J*M+K )       = TMP1
            CALL DROT( M-K, DWORK( J*M+MK1 ), 1, DWORK( ( J-1 )*M+K+1 ),
     $                 1, CO, SI )
C
C           Update B.
C
            CALL DROT( J, B( 1, J+1 ), 1, B( 1, J ), 1, CO, SI )
            TMP1          = -SI*B( J+1, J+1 )
            B( J+1, J+1 ) =  CO*B( J+1, J+1 )
C
            IF( LCMPQ2 ) THEN
C
C              Update Q2.
C
               CALL DROT( N, Q2( 1, J+1 ), 1, Q2( 1, J ), 1, CO, SI )
            END IF
C
C           Determine a Givens rotation to annihilate B(j+1,j) from the
C           left.
C
            CALL DLARTG( B( J, J ), TMP1, CO, SI, TMP2 )
C
C           Update B and F.
C
            B( J, J ) = TMP2
            CALL DROT( M-J, B( J, J+1 ), LDB, B( J+1, J+1 ), LDB, CO, SI
     $                  )
            CALL DROT( J-1, F( 1, J ), 1, F( 1, J+1 ), 1, CO, SI )
            CALL DROT( M-J-1, F( J, MJ2 ), LDF, F( J+1, MJ2 ), LDF, CO,
     $                 SI )
C
C           Update C2 and V.
C
            CALL DROT( M-K+1, C2( J, K ), LDC2, C2( J+1, K ), LDC2, CO,
     $                 SI )
            CALL DROT( M, VW( 1, J+1 ), 1, VW( 1, J+2 ), 1, CO, SI )
C
            IF( LCMPQ2 ) THEN
C
C              Update Q2.
C
               CALL DROT( N, Q2( 1, M+J ), 1, Q2( 1, M+J+1 ), 1, CO, SI
     $                     )
            END IF
  100    CONTINUE
C
C        V. Annihilate W(k,m).
C
         IF( K.LT.M ) THEN
C
C           Determine a Givens rotation to annihilate W(k,m) from the
C           right.
C
            CALL DLARTG( C2( M, K ), DWORK( ( M-1 )*M+K ), CO, SI, TMP1
     $                    )
C
C           Update C1, C2, W and V.
C
            CALL DROT( M, VW( 1, M+1 ), 1, C1( 1, M ), 1, CO, SI )
            C2( M, K )           = TMP1
            DWORK( ( M-1 )*M+K ) = ZERO
            CALL DROT( M-K, C2( M, K+1 ), LDC2, DWORK( ( M-1 )*M+K+1 ),
     $                 1, CO, SI )
C
C           Update B and F.
C
            CALL DROT( M-1, F( 1, M ), 1, B( 1, M ), 1, CO, SI )
C
            IF( LCMPQ2 ) THEN
C
C               Update Q2.
C
                CALL DROT( N, Q2( 1, N ), 1, Q2( 1, M ), 1, CO, SI )
            END IF
         ELSE
C
C           Determine a Givens rotation to annihilate W(m,m) from the
C           left.
C
            CALL DLARTG( C1( M, M ), DWORK( MM ), CO, SI, TMP1 )
C
C           Update C1, C2, W and V.
C
            C1( M, M )  = TMP1
            DWORK( MM ) = ZERO
            CALL DROT( M, VW( M, 2 ), LDVW, C2( 1, M ), 1, CO, SI )
C
C           Update A and D.
C
            CALL DROT( M-1, A( 1, M ), 1, DE( 1, M+1 ), 1, CO, SI )
C
            IF( LCMPQ1 ) THEN
C
C              Update Q1.
C
               CALL DROT( N, Q1( 1, M ), 1, Q1( 1, N ), 1, CO, SI )
            END IF
         END IF
C
C        VI. Annihilate C2(k+2:m,k).
C
         DO 110 J = M, K + 2, -1
            MJ1 = MIN( J+1, M )
C
C           Determine a Givens rotation to annihilate C2(j,k) from the
C           left.
C
            CALL DLARTG( C2( J-1, K ), C2( J, K ), CO, SI, TMP1 )
C
C           Update C2 and V.
C
            C2( J-1, K ) = TMP1
            C2( J, K )   = ZERO
            CALL DROT( M-K, C2( J-1, MK1 ), LDC2, C2( J, MK1 ), LDC2,
     $                 CO, SI )
            CALL DROT( M, VW( 1, J ), 1, VW( 1, J+1 ), 1, CO, SI )
C
C           Update B and F.
C
            CALL DROT( M-J+1, B( J-1, J ), LDB, B( J, J ), LDB, CO, SI )
            TMP1          = -SI*B( J-1, J-1 )
            B( J-1, J-1 ) =  CO*B( J-1, J-1 )
            CALL DROT( J-2, F( 1, J-1 ), 1, F( 1, J ), 1, CO, SI )
            CALL DROT( M-J, F( J-1, MJ1 ), LDF, F( J, MJ1 ), LDF, CO, SI
     $                  )
C
            IF( LCMPQ2 ) THEN
C
C              Update Q2.
C
               CALL DROT( N, Q2( 1, M+J-1 ), 1, Q2( 1, M+J ), 1, CO, SI
     $                     )
            END IF
C
C           Determine a Givens rotation to annihilate B(j,j-1) from the
C           right.
C
            CALL DLARTG( B( J, J ), TMP1, CO, SI, TMP2 )
            B( J, J ) = TMP2
C
C           Update B.
C
            CALL DROT( J-1, B( 1, J ), 1, B( 1, J-1 ), 1, CO, SI )
C
C           Update C1 and W.
C
            CALL DROT( M, C1( 1, J ), 1, C1( 1, J-1 ), 1, CO, SI )
            CALL DROT( M-K+1, DWORK( ( J-1 )*M+K ), 1,
     $                 DWORK( ( J-2 )*M+K ), 1, CO, SI )
C
            IF( LCMPQ2 ) THEN
C
C              Update Q2.
C
               CALL DROT( N, Q2( 1, J ), 1, Q2( 1, J-1 ), 1, CO, SI )
            END IF
  110    CONTINUE
  120 CONTINUE
C
C                     (  A1  D1  )      (  B1  F1  )      (  C11  V1  )
C     Now we have S = (          ), T = (          ), H = (           ),
C                     (   0  A1' )      (   0  B1' )      (   0  C21' )
C
C     where A1, B1, and C11 are upper triangular, C21 is upper
C     Hessenberg, and D1 and F1 are skew-symmetric.
C
C     STEP 3: Apply the periodic QZ algorithm to the generalized matrix
C
C                           -1       -1
C             product C21 A1   C11 B1   in order to make C21 upper
C             quasi-triangular.
C
C     Determine the mode of computations.
C
      IF( LTRI .OR. LCMPQ1 .OR. LCMPQ2 ) THEN
         CMPQ = 'Initialize'
         IMAT = 4*MM + 1
         IWRK = 8*MM + 1
      ELSE
         CMPQ = 'No Computation'
         IMAT = 1
         IWRK = 4*MM + 1
      END IF
C
      IF( LTRI ) THEN
         CMPSC = 'Schur Form'
      ELSE
         CMPSC = 'Eigenvalues Only'
      END IF
C
C     Save matrices in the form that is required by MB03BD.
C
      CALL DLACPY( 'Full', M, M, C2, LDC2, DWORK( IMAT ),      M )
      CALL DLACPY( 'Full', M, M, A,  LDA,  DWORK( IMAT+MM ),   M )
      CALL DLACPY( 'Full', M, M, C1, LDC1, DWORK( IMAT+2*MM ), M )
      CALL DLACPY( 'Full', M, M, B,  LDB,  DWORK( IMAT+3*MM ), M )
      IWORK( 1 ) =  1
      IWORK( 2 ) = -1
      IWORK( 3 ) =  1
      IWORK( 4 ) = -1
C
C     Apply periodic QZ algorithm.
C     Workspace:    need   OPTDW;
C                   prefer larger.
C
      CALL MB03BD( CMPSC, 'Careful', CMPQ, IDUM, 4, M, 1, 1, M, IWORK,
     $             DWORK( IMAT ), M, M, DWORK, M, M, ALPHAR, ALPHAI,
     $             BETA, IWORK( 5 ), IWORK( M+5 ), LIWORK-( M+4 ),
     $             DWORK( IWRK ), LDWORK-IWRK+1, IWARN, INFO )
      IF( IWARN.GT.0 ) THEN
         INFO = 1
         RETURN
      ELSE IF( INFO.GT.0 ) THEN
         INFO = 2
         RETURN
      END IF
      OPTDW = MAX( OPTDW, INT( DWORK( IWRK ) ) + IWRK - 1 )
C
C     Compute the eigenvalues with nonnegative imaginary parts of the
C     pencil aS - bH.
C
      DO 130 I = 1, M
         IF( IWORK( I+4 ).GE.2*EMIN .AND. IWORK( I+4 ).LE.2*EMAX ) THEN
C
C           B = SQRT(BASE**IWORK(i+4)) is between underflow and overflow
C           threshold, BETA(i) is divided by B.
C           Set to zero negligible real and imaginary parts.
C
            BETA( I ) = BETA( I )/BASE**( HALF*IWORK( I+4 ) )
            EIG = SQRT( DCMPLX( ALPHAR( I ), ALPHAI( I ) ) )
            ALPHAR( I ) = DIMAG( EIG )
            ALPHAI( I ) = DBLE(  EIG )
            TMP2 = PREC*DLAPY2( ALPHAR( I ), ALPHAI( I ) )
            IF( ABS( ALPHAR( I ) ).LT.TMP2 ) THEN
               ALPHAR( I ) = ZERO
               IF( ALPHAI( I ).LT.ZERO )
     $             ALPHAI( I ) = -ALPHAI( I )
            END IF
            IF( ABS( ALPHAI( I ) ).LT.TMP2 ) THEN
               ALPHAI( I ) = ZERO
               IF( ALPHAR( I ).LT.ZERO )
     $             ALPHAR( I ) = -ALPHAR( I )
            END IF
         ELSE
C
C           Error.
C
            INFO = 1
            RETURN
         END IF
  130 CONTINUE
C
      IF( LTRI ) THEN
C
C        Update C1 and C2.
C
         CALL DLACPY( 'Upper', M, M, DWORK( IMAT+2*MM ), M, C1, LDC1 )
         CALL DLACPY( 'Full',  M, M, DWORK( IMAT ), M, C2, LDC2 )
C
C        Update V.
C
         CALL DGEMM( 'Transpose', 'No Transpose', M, M, M, ONE,
     $               DWORK( 2*MM+1 ), M, VW( 1, 2 ), LDVW, ZERO,
     $               DWORK( IMAT ), M )
         CALL DGEMM( 'No Transpose', 'No Transpose', M, M, M, ONE,
     $               DWORK( IMAT ), M, DWORK, M, ZERO, VW( 1, 2 ),
     $               LDVW )
C
C        Update A.
C
         CALL DLACPY( 'Upper', M, M, DWORK( IMAT+MM ), M, A, LDA )
C
C        Skew-symmetric update of D.
C
         CALL MB01LD( 'Upper', 'Transpose', M, M, ZERO, ONE, DE( 1, 2 ),
     $                LDDE, DWORK( 2*MM+1 ), M, DE( 1, 2 ), LDDE,
     $                DWORK( IMAT ), LDWORK-IMAT+1, INFO )
C
C        Update B.
C
         CALL DLACPY( 'Upper', M, M, DWORK( IMAT+3*MM ), M, B, LDB )
C
C        Skew-symmetric update of F.
C
         CALL MB01LD( 'Upper', 'Transpose', M, M, ZERO, ONE, F, LDF,
     $                DWORK, M, F, LDF, DWORK( IMAT ), LDWORK-IMAT+1,
     $                INFO )
C
         IF( LCMPQ1 ) THEN
C
C           Update Q1.
C
            CALL DGEMM( 'No Transpose', 'No Transpose', N, M, M, ONE,
     $                  Q1, LDQ1, DWORK( 2*MM+1 ), M, ZERO,
     $                  DWORK( IMAT ), N )
            CALL DLACPY( 'Full', N, M, DWORK( IMAT ), N, Q1, LDQ1 )
            CALL DGEMM( 'No Transpose', 'No Transpose', N, M, M, ONE,
     $                  Q1( 1, M+1 ), LDQ1, DWORK( MM+1 ), M, ZERO,
     $                  DWORK( IMAT ), N )
            CALL DLACPY( 'Full', N, M, DWORK( IMAT ), N, Q1( 1, M+1 ),
     $                   LDQ1 )
         END IF
C
         IF( LCMPQ2 ) THEN
C
C           Update Q2.
C
            CALL DGEMM( 'No Transpose', 'No Transpose', N, M, M, ONE,
     $                  Q2, LDQ2, DWORK( 3*MM+1 ), M, ZERO,
     $                  DWORK( IMAT ), N )
            CALL DLACPY( 'Full', N, M, DWORK( IMAT ), N, Q2, LDQ2 )
            CALL DGEMM( 'No Transpose', 'No Transpose', N, M, M, ONE,
     $                  Q2( 1, M+1 ), LDQ2, DWORK, M, ZERO,
     $                  DWORK( IMAT ), N )
            CALL DLACPY( 'Full', N, M, DWORK( IMAT ), N, Q2( 1, M+1 ),
     $                   LDQ2 )
         END IF
      END IF
C
      DWORK( 1 ) = OPTDW
      RETURN
C *** Last line of MB04BD ***
      END
