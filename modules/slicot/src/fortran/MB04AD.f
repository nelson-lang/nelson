      SUBROUTINE MB04AD( JOB, COMPQ1, COMPQ2, COMPU1, COMPU2, N, Z, LDZ,
     $                   H, LDH, T, LDT, Q1, LDQ1, Q2, LDQ2, U11, LDU11,
     $                   U12, LDU12, U21, LDU21, U22, LDU22, ALPHAR,
     $                   ALPHAI, BETA, IWORK, LIWORK, DWORK, LDWORK,
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
C     To compute the eigenvalues of a real N-by-N skew-Hamiltonian/
C     Hamiltonian pencil aS - bH with
C
C                                      (  0  I  )
C       S = T Z = J Z' J' Z, where J = (        ),                   (1)
C                                      ( -I  0  )
C
C     via generalized symplectic URV decomposition. That is, orthogonal
C     matrices Q1 and Q2 and orthogonal symplectic matrices U1 and U2
C     are computed such that
C
C                                   (  T11  T12 )
C       Q1' T U1 = Q1' J Z' J' U1 = (           ) = Tout,
C                                   (   0   T22 )
C
C                  (  Z11  Z12 )
C       U2' Z Q2 = (           ) = Zout,                             (2)
C                  (   0   Z22 )
C
C                  ( H11  H12 )
C       Q1' H Q2 = (          ) = Hout,
C                  (  0   H22 )
C
C     where T11, T22', Z11, Z22', H11 are upper triangular and H22' is
C     upper quasi-triangular.
C     Optionally, if COMPQ1 = 'C' the orthogonal transformation matrix
C     Q1 will be computed.
C     Optionally, if COMPQ2 = 'C' the orthogonal transformation matrix
C     Q2 will be computed.
C     Optionally, if COMPU1 = 'C' the orthogonal symplectic
C     transformation matrix
C
C            (  U11  U12  )
C       U1 = (            )
C            ( -U12  U11  )
C
C     will be computed.
C     Optionally, if COMPU2 = 'C' the orthogonal symplectic
C     transformation matrix
C
C            (  U21  U22  )
C       U2 = (            )
C            ( -U22  U21  )
C
C     will be computed.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOB     CHARACTER*1
C             Specifies the computation to be performed, as follows:
C             = 'E': compute the eigenvalues only; Z, T, and H will not
C                    necessarily be put into the forms in (2); H22' is
C                    upper Hessenberg;
C             = 'T': put Z, T, and H into the forms in (2), and return
C                    the eigenvalues in ALPHAR, ALPHAI and BETA.
C
C     COMPQ1  CHARACTER*1
C             Specifies whether to compute the orthogonal transformation
C             matrix Q1, as follows:
C             = 'N': Q1 is not computed;
C             = 'C': compute the matrix Q1 of the orthogonal
C                    transformations applied on the left to the pencil
C                    aTZ - bH to reduce its matrices to the form (2).
C                    The array Q1 is initialized internally to the
C                    identity matrix.
C
C     COMPQ2  CHARACTER*1
C             Specifies whether to compute the orthogonal transformation
C             matrix Q2, as follows:
C             = 'N': Q2 is not computed;
C             = 'C': compute the matrix Q2 of the orthogonal
C                    transformations applied on the right to the pencil
C                    aTZ - bH to reduce its matrices to the form (2).
C                    The array Q2 is initialized internally to the
C                    identity matrix.
C
C     COMPU1  CHARACTER*1
C             Specifies whether to compute the orthogonal symplectic
C             transformation matrix U1, as follows:
C             = 'N': U1 is not computed;
C             = 'C': compute the matrices U11 and U12 of the orthogonal
C                    symplectic transformations applied to the pencil
C                    aTZ - bT to reduce its matrices to the form (2).
C                    The arrays U11 and U12 are initialized internally
C                    to correspond to an identity matrix U1.
C
C     COMPU2  CHARACTER*1
C             Specifies whether to compute the orthogonal symplectic
C             transformation matrix U2, as follows:
C             = 'N': U2 is not computed;
C             = 'C': compute the matrices U21 and U22 of the orthogonal
C                    symplectic transformations applied to the pencil
C                    aTZ - bT to reduce its matrices to the form (2).
C                    The arrays U21 and U22 are initialized internally
C                    to correspond to an identity matrix U2.
C
C     Input/output Parameters
C
C     N       (input) INTEGER
C             The order of the pencil aS - bH.  N >= 0, even.
C
C     Z       (input/output) DOUBLE PRECISION array, dimension (LDZ, N)
C             On entry, the leading N-by-N part of this array must
C             contain the matrix Z.
C             On exit, if JOB = 'T', the leading N-by-N part of this
C             array contains the matrix Zout; otherwise, it contains the
C             matrix Z obtained just before the application of the
C             periodic QZ algorithm.
C             The elements of the (2,1) block, i.e., in the rows N/2+1
C             to N and in the columns 1 to N/2 are not set to zero, but
C             are unchanged on exit.
C
C     LDZ     INTEGER
C             The leading dimension of the array Z.  LDZ >= MAX(1, N).
C
C     H       (input/output) DOUBLE PRECISION array, dimension (LDH, N)
C             On entry, the leading N-by-N part of this array must
C             contain the Hamiltonian matrix H (H22 = -H11', H12 = H12',
C             H21 = H21').
C             On exit, if JOB = 'T', the leading N-by-N part of this
C             array contains the matrix Hout; otherwise, it contains the
C             matrix H obtained just before the application of the
C             periodic QZ algorithm.
C
C     LDH     INTEGER
C             The leading dimension of the array H.  LDH >= MAX(1, N).
C
C     T       (output) DOUBLE PRECISION array, dimension (LDT, N)
C             If JOB = 'T', the leading N-by-N part of this array
C             contains the matrix Tout; otherwise, it contains the
C             matrix T obtained just before the application of the
C             periodic QZ algorithm.
C
C     LDT     INTEGER
C             The leading dimension of the array T.  LDT >= MAX(1, N).
C
C     Q1      (output) DOUBLE PRECISION array, dimension (LDQ1, N)
C             On exit, if COMPQ1 = 'C', the leading N-by-N part of this
C             array contains the orthogonal transformation matrix Q1.
C             If COMPQ1 = 'N', this array is not referenced.
C
C     LDQ1    INTEGER
C             The leading dimension of the array Q1.
C             LDQ1 >= 1,         if COMPQ1 = 'N';
C             LDQ1 >= MAX(1, N), if COMPQ1 = 'C'.
C
C     Q2      (output) DOUBLE PRECISION array, dimension (LDQ2, N)
C             On exit, if COMPQ2 = 'C', the leading N-by-N part of this
C             array contains the orthogonal transformation matrix Q2.
C             If COMPQ2 = 'N', this array is not referenced.
C
C     LDQ2    INTEGER
C             The leading dimension of the array Q2.
C             LDQ2 >= 1,         if COMPQ2 = 'N';
C             LDQ2 >= MAX(1, N), if COMPQ2 = 'C'.
C
C     U11     (output) DOUBLE PRECISION array, dimension (LDU11, N/2)
C             On exit, if COMPU1 = 'C', the leading N/2-by-N/2 part of
C             this array contains the upper left block U11 of the
C             orthogonal symplectic transformation matrix U1.
C             If COMPU1 = 'N', this array is not referenced.
C
C     LDU11   INTEGER
C             The leading dimension of the array U11.
C             LDU11 >= 1,           if COMPU1 = 'N';
C             LDU11 >= MAX(1, N/2), if COMPU1 = 'C'.
C
C     U12     (output) DOUBLE PRECISION array, dimension (LDU12, N/2)
C             On exit, if COMPU1 = 'C', the leading N/2-by-N/2 part of
C             this array contains the upper right block U12 of the
C             orthogonal symplectic transformation matrix U1.
C             If COMPU1 = 'N', this array is not referenced.
C
C     LDU12   INTEGER
C             The leading dimension of the array U12.
C             LDU12 >= 1,           if COMPU1 = 'N';
C             LDU12 >= MAX(1, N/2), if COMPU1 = 'C'.
C
C     U21     (output) DOUBLE PRECISION array, dimension (LDU21, N/2)
C             On exit, if COMPU2 = 'C', the leading N/2-by-N/2 part of
C             this array contains the upper left block U21 of the
C             orthogonal symplectic transformation matrix U2.
C             If COMPU2 = 'N', this array is not referenced.
C
C     LDU21   INTEGER
C             The leading dimension of the array U21.
C             LDU21 >= 1,           if COMPU2 = 'N';
C             LDU21 >= MAX(1, N/2), if COMPU2 = 'C'.
C
C     U22     (output) DOUBLE PRECISION array, dimension (LDU22, N/2)
C             On exit, if COMPU2 = 'C', the leading N/2-by-N/2 part of
C             this array contains the upper right block U22 of the
C             orthogonal symplectic transformation matrix U2.
C             If COMPU2 = 'N', this array is not referenced.
C
C     LDU22   INTEGER
C             The leading dimension of the array U22.
C             LDU22 >= 1,           if COMPU2 = 'N';
C             LDU22 >= MAX(1, N/2), if COMPU2 = 'C'.
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
C             The scalars beta defining the eigenvalues of the pencil
C             aS - bH.
C             If INFO = 0, the quantities alpha = (ALPHAR(j),ALPHAI(j)),
C             and beta = BETA(j) represent together the j-th eigenvalue
C             of the pencil aS - bH, in the form lambda = alpha/beta.
C             Since lambda may overflow, the ratios should not, in
C             general, be computed. Due to the skew-Hamiltonian/
C             Hamiltonian structure of the pencil, only half of the
C             spectrum is saved in ALPHAR, ALPHAI and BETA.
C             Specifically, the eigenvalues with positive real parts or
C             with non-negative imaginary parts, when real parts are
C             zero, are returned. The remaining eigenvalues have
C             opposite signs.
C             If INFO = 3, one or more BETA(j) is not representable, and
C             the eigenvalues are returned as described below.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (LIWORK)
C             On exit, if INFO = 3, IWORK(1), ..., IWORK(N/2) return the
C             scaling parameters for the eigenvalues of the pencil
C             aS - bH (see INFO = 3).
C
C     LIWORK  INTEGER
C             The dimension of the array IWORK.
C             LIWORK >= N/2+18.
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal value
C             of LDWORK, and DWORK(2) returns the machine base, b.
C             On exit, if INFO = -31, DWORK(1) returns the minimum value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The dimension of the array DWORK.
C             If JOB = 'E' and COMPQ1 = 'N' and COMPQ2 = 'N' and
C             COMPU1 = 'N' and COMPU2 = 'N', then
C                   LDWORK >= 3/2*N**2+MAX(N, 48);
C             else, LDWORK >=   3*N**2+MAX(N, 48).
C             For good performance LDWORK should generally be larger.
C
C             If LDWORK = -1, then a workspace query is assumed; the
C             routine only calculates the optimal size of the DWORK
C             array, returns this value as the first entry of the DWORK
C             array, and no error message related to LDWORK is issued by
C             XERBLA.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0: succesful exit.
C             < 0: if INFO = -i, the i-th argument had an illegal value.
C             = 1: the periodic QZ algorithm was not able to reveal
C                  information about the eigenvalues from the 2-by-2
C                  blocks in the SLICOT Library routine MB03BD;
C             = 2: the periodic QZ algorithm did not converge in the
C                  SLICOT Library routine MB03BD;
C             = 3: the eigenvalues will under- or overflow if evaluated;
C                  therefore, the j-th eigenvalue is represented by
C                  the quantities alpha = (ALPHAR(j),ALPHAI(j)),
C                  beta = BETA(j), and gamma = IWORK(j) in the form
C                  lambda = (alpha/beta) * b**gamma, where b is the
C                  machine base (often 2.0). This is not an error.
C
C     METHOD
C
C     The algorithm uses Givens rotations and Householder reflections to
C     annihilate elements in T, Z, and H such that T11, T22', Z11, Z22'
C     and H11 are upper triangular and H22' is upper Hessenberg. Finally
C     the periodic QZ algorithm is applied to transform H22' to upper
C     quasi-triangular form while T11, T22', Z11, Z22', and H11 stay in
C     upper triangular form.
C     See also page 17 in [1] for more details.
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
C     Chemnitz, December 03, 2008.
C     V. Sima, Dec. 2009 (SLICOT version of the routine DGEURV).
C
C     REVISIONS
C
C     V. Sima, Feb. 2010, Nov. 2010.
C
C     KEYWORDS
C
C     generalized symplectic URV decomposition, periodic QZ algorithm,
C     upper (quasi-)triangular matrix.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ZERO, HALF, ONE
      PARAMETER          ( ZERO = 0.0D+0, HALF= 0.5D+0, ONE = 1.0D+0 )
C
C     .. Scalar Arguments ..
      CHARACTER          COMPQ1, COMPQ2, COMPU1, COMPU2, JOB
      INTEGER            INFO, LDH, LDQ1, LDQ2, LDT, LDU11, LDU12,
     $                   LDU21, LDU22, LDWORK, LDZ, LIWORK, N
C
C     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   ALPHAI( * ), ALPHAR( * ), BETA( * ),
     $                   DWORK( * ), H( LDH, * ), Q1( LDQ1, * ),
     $                   Q2( LDQ2, * ), T( LDT, * ), U11( LDU11, * ),
     $                   U12( LDU12, * ), U21( LDU21, * ),
     $                   U22( LDU22, * ), Z( LDZ, * )
C
C     .. Local Scalars ..
      LOGICAL            LCMPQ1, LCMPQ2, LCMPU1, LCMPU2, LINIQ1, LINIQ2,
     $                   LINIU1, LINIU2, LQUERY, LTRI, LUPDQ1, LUPDQ2,
     $                   LUPDU1, LUPDU2
      CHARACTER*16       CMPQ, CMPSC
      INTEGER            I, IMAT, IQ, ITAU, IWARN, IWRK, J, K, M, MINDW,
     $                   MM, NB, OPTDW
      DOUBLE PRECISION   BASE, CO, EMAX, EMIN, PREC, SI, SQRB, TMP1,
     $                   TMP2
      COMPLEX*16         EIG
C
C     .. Local Arrays ..
      INTEGER            IDUM( 1 )
      DOUBLE PRECISION   DUM( 1 )
C
C     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLAPY2
      EXTERNAL           DLAMCH, DLAPY2, ILAENV, LSAME
C
C     .. External Subroutines ..
      EXTERNAL           DGEMM, DGEQRF, DGERQF, DLACPY, DLARTG, DLASET,
     $                   DORMQR, DORMRQ, DROT, DSCAL, DSWAP, MA02AD,
     $                   MB03BD, XERBLA
C
C     ... Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCMPLX, DIMAG, MAX, MIN, MOD, SQRT
C
C     .. Executable Statements ..
C
C     Decode the input arguments.
C     The code is able to update some given matrices Q1, Q2, U1, and U2,
C     but this feature is not documented.
C
      M  = N/2
      MM = M*M
C
      LTRI   = LSAME( JOB,    'T' )
      LINIQ1 = LSAME( COMPQ1, 'C' )
      LUPDQ1 = LSAME( COMPQ1, 'U' )
      LINIQ2 = LSAME( COMPQ2, 'C' )
      LUPDQ2 = LSAME( COMPQ2, 'U' )
      LINIU1 = LSAME( COMPU1, 'C' )
      LUPDU1 = LSAME( COMPU1, 'U' )
      LINIU2 = LSAME( COMPU2, 'C' )
      LUPDU2 = LSAME( COMPU2, 'U' )
      LCMPQ1 = LINIQ1 .OR. LUPDQ1
      LCMPQ2 = LINIQ2 .OR. LUPDQ2
      LCMPU1 = LINIU1 .OR. LUPDU1
      LCMPU2 = LINIU2 .OR. LUPDU2
      LQUERY = ( LDWORK.EQ.-1 )
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
      ELSE IF( .NOT.( LSAME( COMPU1, 'N' ) .OR. LCMPU1 ) ) THEN
         INFO = -4
      ELSE IF( .NOT.( LSAME( COMPU2, 'N' ) .OR. LCMPU2 ) ) THEN
         INFO = -5
      ELSE IF( N.LT.0 .OR. MOD( N, 2 ).NE.0 ) THEN
         INFO = -6
      ELSE IF( LDZ.LT.MAX( 1, N ) ) THEN
         INFO = -8
      ELSE IF( LDH.LT.MAX( 1, N ) ) THEN
         INFO = -10
      ELSE IF( LDT.LT.MAX( 1, N ) ) THEN
         INFO = -12
      ELSE IF( LDQ1.LT.1  .OR. ( LCMPQ1 .AND.  LDQ1.LT.N ) ) THEN
         INFO = -14
      ELSE IF( LDQ2.LT.1  .OR. ( LCMPQ2 .AND.  LDQ2.LT.N ) ) THEN
         INFO = -16
      ELSE IF( LDU11.LT.1 .OR. ( LCMPU1 .AND. LDU11.LT.M ) ) THEN
         INFO = -18
      ELSE IF( LDU12.LT.1 .OR. ( LCMPU1 .AND. LDU12.LT.M ) ) THEN
         INFO = -20
      ELSE IF( LDU21.LT.1 .OR. ( LCMPU2 .AND. LDU21.LT.M ) ) THEN
         INFO = -22
      ELSE IF( LDU22.LT.1 .OR. ( LCMPU2 .AND. LDU22.LT.M ) ) THEN
         INFO = -24
      ELSE IF( LIWORK.LT.M+18 ) THEN
         INFO = -29
      ELSE
         IF( LTRI .OR. LCMPQ1 .OR. LCMPQ2 .OR. LCMPU1 .OR. LCMPU2 ) THEN
            MINDW = 12*MM + MAX( N, 48 )
         ELSE
            MINDW =  6*MM + MAX( N, 48 )
         END IF
         IWRK  = M
         NB    = MIN( 64, ILAENV( 1, 'DORMQR', 'LT', N, N, M, -1 ) )
         OPTDW = MAX( MINDW, IWRK + N*NB )
         NB    = ILAENV( 1, 'DGERQF', ' ', M, M, -1, -1 )
         OPTDW = MAX( OPTDW, IWRK + MM + M*NB )
         NB    = MIN( 64, ILAENV( 1, 'DORMRQ', 'LN', M, N, M, -1 ) )
         OPTDW = MAX( OPTDW, IWRK + M*M + N*NB )
         IF( LCMPQ1 ) THEN
            NB    = MIN( 64, ILAENV( 1, 'DORMRQ', 'RT', N, M, M, -1 ) )
            OPTDW = MAX( OPTDW, IWRK + M*M + N*NB )
         END IF
         NB    = ILAENV( 1, 'DGEQRF', ' ', N, M, -1, -1 )
         OPTDW = MAX( OPTDW, IWRK + M*N + M*NB )
         NB    = MIN( 64, ILAENV( 1, 'DORMQR', 'RN', N, N, M, -1 ) )
         OPTDW = MAX( OPTDW, IWRK + M*N + N*NB )
         IF( LCMPQ2 ) THEN
            NB    = MIN( 64, ILAENV( 1, 'DORMQR', 'LN', N, N, M, -1 ) )
            OPTDW = MAX( OPTDW, IWRK + M*N + N*NB )
         END IF
         IF( LDWORK.LT.MINDW .AND. .NOT.LQUERY ) THEN
            DWORK( 1 ) = MINDW
            INFO = -31
         END IF
      END IF
C
      IF( INFO.NE.0) THEN
         CALL XERBLA( 'MB04AD', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         DWORK( 1 ) = OPTDW
         RETURN
      END IF
C
C     Determine machine constants.
C
      BASE = DLAMCH( 'Base' )
      EMIN = DLAMCH( 'Minimum Exponent' )
      EMAX = DLAMCH( 'Largest Exponent' )
      PREC = DLAMCH( 'Precision' )
      SQRB = SQRT( BASE )
C
C     Quick return if possible.
C
      IF( N.EQ.0 ) THEN
         DWORK( 1 ) = 2
         DWORK( 2 ) = BASE
         RETURN
      END IF
C
C     Initializations.
C
C     Set T = J Z' J'.
C
      CALL MA02AD( 'Full', M, M, Z( M+1, M+1 ), LDZ, T, LDT )
      CALL MA02AD( 'Full', M, M, Z( 1, M+1 ), LDZ, T( 1, M+1 ), LDT )
C
      DO 10 I = 1, M
         CALL DSCAL( M, -ONE, T( 1, M+I ), 1 )
   10 CONTINUE
C
      CALL MA02AD( 'Full', M, M, Z( M+1, 1 ), LDZ, T( M+1, 1 ), LDT )
C
      DO 20 I = 1, M
         CALL DSCAL( M, -ONE, T( M+1, I ), 1 )
   20 CONTINUE
C
      CALL MA02AD( 'Full', M, M, Z, LDZ, T( M+1, M+1 ), LDT )
C
      IF( LINIQ1 )
     $   CALL DLASET( 'Full', N, N, ZERO, ONE, Q1, LDQ1 )
C
      IF( LINIQ2 ) THEN
         CALL DLASET( 'Full', M, M, ZERO, ZERO, Q2, LDQ2 )
         CALL DLASET( 'Full', M, M, ZERO, ONE,  Q2( M+1, 1 ),   LDQ2 )
         CALL DLASET( 'Full', M, M, ZERO, ONE,  Q2( 1, M+1 ),   LDQ2 )
         CALL DLASET( 'Full', M, M, ZERO, ZERO, Q2( M+1, M+1 ), LDQ2 )
      END IF
C
      IF( LINIU1 ) THEN
         CALL DLASET( 'Full', M, M, ZERO,  ONE, U11, LDU11 )
         CALL DLASET( 'Full', M, M, ZERO, ZERO, U12, LDU12 )
      END IF
C
      IF( LINIU2 ) THEN
         CALL DLASET( 'Full', M, M, ZERO,  ONE, U21, LDU21 )
         CALL DLASET( 'Full', M, M, ZERO, ZERO, U22, LDU22 )
      END IF
C
C     STEP 1: Block triangularize T and Z.
C
C     (Note: Comments in the code beginning "Workspace:" describe the
C     minimal amount of real workspace needed at that point in the
C     code, as well as the preferred amount for good performance.
C     NB refers to the optimal block size for the immediately
C     following subroutine, as returned by ILAENV.)
C
      ITAU = 1
      IWRK = ITAU + M
C
C                                 ( T11 )
C     Perform a QR decomposition, (     ) = Q1*R1.
C                                 ( T21 )
C     Workspace:    need   IWRK + M - 1;
C                   prefer IWRK + M*NB - 1.
C
      CALL DGEQRF( N, M, T, LDT, DWORK( ITAU ), DWORK( IWRK ),
     $             LDWORK-IWRK+1, INFO )
C
C            ( T12 )
C     Update (     ).
C            ( T22 )
C
C     Workspace:    need   IWRK + M - 1;
C                   prefer IWRK + M*NB - 1.
C
      CALL DORMQR( 'Left', 'Transpose', N, M, M, T, LDT, DWORK( ITAU ),
     $             T( 1, M+1 ), LDT, DWORK( IWRK ), LDWORK-IWRK+1, INFO
     $           )
C
C     Update H.
C     Workspace:    need   IWRK + N - 1;
C                   prefer IWRK + N*NB - 1.
C
      CALL DORMQR( 'Left', 'Transpose', N, N, M, T, LDT, DWORK( ITAU ),
     $             H, LDH, DWORK( IWRK ), LDWORK-IWRK+1, INFO )
C
      IF( LCMPQ1 ) THEN
C
C        Update Q1.
C
         CALL DORMQR( 'Right', 'No Transpose', N, N, M, T, LDT,
     $                DWORK( ITAU ), Q1, LDQ1, DWORK( IWRK ),
     $                LDWORK-IWRK+1, INFO )
      END IF
C
C     Set the strictly lower triangular part of [ T11; T21 ] to zero.
C
      CALL DLASET( 'Lower', N-1, M, ZERO, ZERO, T( 2, 1 ), LDT )
C
C     Perform an RQ decomposition, T22 = R2*Q2.
C     Workspace:    need   IWRK + M - 1;
C                   prefer IWRK + M*NB - 1.
C
      ITAU = MM   + 1
      IWRK = ITAU + M
      CALL MA02AD( 'Full', M, M, T( M+1, M+1 ), LDT, DWORK, M )
      CALL DGERQF( M, M, DWORK, M, DWORK( ITAU ), DWORK( IWRK ),
     $             LDWORK-IWRK+1, INFO )
      CALL MA02AD( 'Upper', M, M, DWORK, M, T( M+1, M+1 ), LDT )
C
C     Set the strictly upper triangular part of T22 to zero.
C
      IF( M.GT.1 )
     $   CALL DLASET( 'Upper', M-1, M-1, ZERO, ZERO, T( M+1, M+2 ), LDT
     $              )
C
C     Update H.
C     Workspace:    need   IWRK + N - 1;
C                   prefer IWRK + N*NB - 1.
C
      CALL DORMRQ( 'Left', 'No Transpose', M, N, M, DWORK, M,
     $             DWORK( ITAU ), H( M+1, 1 ), LDH, DWORK( IWRK ),
     $             LDWORK-IWRK+1, INFO )
C
      IF( LCMPQ1 ) THEN
C
C        Update Q1.
C
         CALL DORMRQ( 'Right', 'Transpose', N, M, M, DWORK, M,
     $                DWORK( ITAU ), Q1( 1, M+1 ), LDQ1, DWORK( IWRK ),
     $                LDWORK-IWRK+1, INFO )
      END IF
C
C     Perform a QR decomposition, ( Z21  Z22 )' = Q3*R3.
C     Workspace:    need   IWRK + M - 1;
C                   prefer IWRK + M*NB - 1.
C
      ITAU = M*N  + 1
      IWRK = ITAU + M
      CALL MA02AD( 'Full', M, N, Z( M+1, 1 ), LDZ, DWORK, N )
      CALL DGEQRF( N, M, DWORK, N, DWORK( ITAU ), DWORK( IWRK ),
     $             LDWORK-IWRK+1, INFO )
C
C     Update ( Z11  Z12 ).
C
      CALL DORMQR( 'Right', 'No Transpose', M, N, M, DWORK, N,
     $             DWORK( ITAU ), Z, LDZ, DWORK( IWRK ),
     $             LDWORK-IWRK+1, INFO )
      CALL MA02AD( 'Upper', M, M, DWORK, N, Z( M+1, M+1 ), LDZ )
C
C     Set the strictly upper triangular part of Z22 to zero.
C
      IF( M.GT.1 )
     $   CALL DLASET( 'Upper', M-1, M-1, ZERO, ZERO, Z( M+1, M+2 ), LDZ
     $              )
C
C     Update H.
C
      CALL DORMQR( 'Right', 'No Transpose', N, N, M, DWORK, N,
     $             DWORK( ITAU ), H, LDH, DWORK( IWRK ), LDWORK-IWRK+1,
     $             INFO )
C
      DO 30 I = 1, M
         CALL DSWAP( N, H( 1, I ), 1, H( 1, M+I ), 1 )
  30  CONTINUE
C
      IF( LCMPQ2 ) THEN
C
C        Update Q2.
C
         CALL DORMQR( 'Left', 'No Transpose', N, N, M, DWORK, N,
     $                DWORK( ITAU ), Q2, LDQ2, DWORK( IWRK ),
     $                LDWORK-IWRK+1, INFO )
      END IF
C
C     Perform an RQ decomposition Z12 = R4*Q4.
C
      ITAU = 1
      IWRK = ITAU + M
      CALL DGERQF( M, M, Z( 1, M+1 ), LDZ, DWORK( ITAU ), DWORK( IWRK ),
     $             LDWORK-IWRK+1, INFO )
C
C     Update H.
C
      CALL DORMRQ( 'Right', 'Transpose', N, M, M, Z( 1, M+1 ), LDZ,
     $             DWORK( ITAU ), H, LDH, DWORK( IWRK ), LDWORK-IWRK+1,
     $             INFO )
C
      IF( LCMPQ2 ) THEN
C
C        Update Q2.
C
         CALL DORMRQ( 'Right', 'Transpose', N, M, M, Z( 1, M+1 ), LDZ,
     $                DWORK( ITAU ), Q2, LDQ2, DWORK( IWRK ),
     $                LDWORK-IWRK+1, INFO )
      END IF
C
C     Exchange Z11 and Z12 and set the strictly lower triangular part
C     of Z11 to zero.
C
      DUM( 1 ) = ZERO
C
      DO 40 I = 1, M - 1
         CALL DSWAP(  M, Z( 1, I ), 1, Z( 1, M+I ), 1 )
         CALL DCOPY( M-I, DUM, 0, Z( I+1, I ), 1 )
   40 CONTINUE
C
      CALL DSWAP( M, Z( 1, M ), 1, Z( 1, N ), 1 )
C
C     STEP 2: Eliminations in H.
C
      DO 90 K = 1, M
C
C        I. Annihilate H(m+k:n-1,k).
C
         DO 50 J = K, M-1
C
C           Determine a Givens rotation to annihilate H(m+j,k) from the
C           left.
C
            CALL DLARTG( H( M+J+1, K ), H( M+J, K ), CO, SI, TMP1 )
C
C           Update H.
C
            H( M+J+1, K ) = TMP1
            H(   M+J, K ) = ZERO
            CALL DROT( N-K, H( M+J+1, K+1 ), LDH, H( M+J, K+1 ), LDH,
     $                 CO, SI )
C
C           Update T.
C
            CALL DROT( J+1, T( M+J+1, M+1 ), LDT, T( M+J, M+1 ), LDT,
     $                 CO, SI )
C
            IF( LCMPQ1 ) THEN
C
C              Update Q1.
C
               CALL DROT( N, Q1( 1, M+J+1 ), 1, Q1( 1, M+J ), 1, CO, SI
     $                  )
            END IF
C
C           Determine a Givens rotation to annihilate T(m+j,m+j+1) from
C           the right.
C
            CALL DLARTG( T( M+J, M+J ), T( M+J, M+J+1 ), CO, SI, TMP1 )
C
C           Update T.
C
            CALL DROT( M, T( 1, M+J ), 1, T( 1, M+J+1 ), 1, CO , SI )
            T( M+J, M+J   ) = TMP1
            T( M+J, M+J+1 ) = ZERO
            CALL DROT( M-J, T( M+J+1, M+J ), 1, T( M+J+1, M+J+1 ), 1,
     $                 CO, SI )
            CALL DROT( J+1, T( 1, J ), 1, T( 1, J+1 ), 1, CO, SI )
C
            IF( LCMPU1 ) THEN
C
C              Update U11 and U12.
C
               CALL DROT( M, U11( 1, J ), 1, U11( 1, J+1 ), 1, CO, SI )
               CALL DROT( M, U12( 1, J ), 1, U12( 1, J+1 ), 1, CO, SI )
            END IF
C
C           Determine a Givens rotation to annihilate T(j+1,j) from the
C           left.
C
            CALL DLARTG( T( J, J ), T( J+1, J ), CO, SI, TMP1 )
C
C           Update T.
C
            T(   J, J ) = TMP1
            T( J+1, J ) = ZERO
            CALL DROT( N-J, T( J, J+1 ), LDT, T( J+1, J+1 ), LDT, CO, SI
     $               )
C
C           Update H.
C
            CALL DROT( N-K+1, H( J, K ), LDH, H( J+1, K ), LDH, CO, SI )
C
            IF( LCMPQ1 ) THEN
C
C              Update Q1.
C
               CALL DROT( N, Q1( 1, J ), 1, Q1( 1, J+1 ), 1, CO, SI )
            END IF
   50    CONTINUE
C
C        II. Annihilate H(n,k).
C
C        Determine a Givens rotation to annihilate H(n,k) form the left.
C
         CALL DLARTG( H( M, K ), H( N, K ), CO, SI, TMP1 )
C
C        Update H.
C
         H( M, K ) = TMP1
         H( N, K ) = ZERO
         CALL DROT( N-K, H( M, K+1 ), LDH, H( N, K+1 ), LDH, CO, SI )
C
C        Update T.
C
         CALL DROT( M, T( M, M+1 ), LDT, T( N, M+1 ), LDT, CO, SI )
         TMP1 =      -SI*T( M, M )
         T( M, M ) =  CO*T( M, M )
C
         IF( LCMPQ1 ) THEN
C
C           Update Q1.
C
            CALL DROT( N, Q1( 1, M ), 1, Q1( 1, N ), 1, CO, SI )
         END IF
C
C        Determine a Givens rotation to annihilate T(n,m) from the
C        right.
C
         CALL DLARTG( T( N, N ), TMP1, CO, SI, TMP2 )
C
C        Update T.
C
         CALL DROT( M, T( 1, N ), 1, T( 1, M ), 1, CO, SI )
         T( N, N ) = TMP2
C
         IF( LCMPU1 ) THEN
C
C           Update U11 and U12.
C
            CALL DROT( M, U12( 1, M ), 1, U11( 1, M ), 1, CO, SI )
         END IF
C
C        III. Annihilate H(k+1:m,k).
C
         DO 60 J = M, K+1, -1
C
C           Determine a Givens rotation to annihilate H(j,k) from the
C           left.
C
            CALL DLARTG( H( J-1, K ), H( J, K ), CO, SI, TMP1 )
C
C           Update H.
C
            H( J-1, K ) = TMP1
            H( J,   K ) = ZERO
            CALL DROT( N-K, H( J-1, K+1 ), LDH, H( J, K+1 ), LDH, CO, SI
     $               )
C
C           Update T.
C
            CALL DROT( N-J+2, T( J-1, J-1 ), LDT, T( J, J-1 ), LDT, CO,
     $                 SI )
C
            IF( LCMPQ1 ) THEN
C
C              Update Q1.
C
               CALL DROT( N, Q1( 1, J-1 ), 1, Q1( 1, J ), 1, CO, SI )
            END IF
C
C           Determine a Givens rotation to annihilate T(j,j-1) from the
C           right.
C
            CALL DLARTG( T( J, J ), T( J, J-1 ), CO, SI, TMP1 )
C
C           Update T.
C
            CALL DROT( M, T( 1, M+J ), 1, T( 1, M+J-1 ), 1, CO, SI )
            CALL DROT( M-J+2, T( M+J-1, M+J ), 1, T( M+J-1, M+J-1 ), 1,
     $                 CO, SI )
            T( J, J   ) = TMP1
            T( J, J-1 ) = ZERO
            CALL DROT( J-1, T( 1, J ), 1, T( 1, J-1 ), 1, CO, SI )
C
            IF( LCMPU1 ) THEN
C
C              Update U11 and U12.
C
               CALL DROT( M, U11( 1, J ), 1, U11( 1, J-1 ), 1, CO, SI )
               CALL DROT( M, U12( 1, J ), 1, U12( 1, J-1 ), 1, CO, SI )
            END IF
C
C           Determine a Givens rotation to annihilate T(m+j-1,m-j) from
C           the left.
C
            CALL DLARTG( T( M+J, M+J ), T( M+J-1, M+J ), CO, SI, TMP1 )
C
C           Update T.
C
            T( M+J,   M+J ) = TMP1
            T( M+J-1, M+J ) = ZERO
            CALL DROT( J-1, T( M+J, M+1 ), LDT, T( M+J-1, M+1 ), LDT,
     $                 CO, SI )
C
C           Update H.
C
            CALL DROT( N-K+1, H( M+J, K ), LDH, H( M+J-1, K ), LDH, CO,
     $                 SI )
C
            IF( LCMPQ1 ) THEN
C
C              Update Q1.
C
               CALL DROT( N, Q1( 1, M+J ), 1, Q1( 1, M+J-1 ), 1, CO, SI
     $                  )
            END IF
   60    CONTINUE
C
C        IV. Annihilate H(m+k,k+1:m-1).
C
         DO 70 J = K+1, M-1
C
C           Determine a Givens rotation to annihilate H(m+k,j) from the
C           right.
C
            CALL DLARTG( H( M+K, J+1 ), H( M+K, J ), CO, SI, TMP1 )
C
C           Update H.
C
            CALL DROT( M, H( 1, J+1 ), 1, H( 1, J ), 1, CO, SI )
            H( M+K, J+1 ) = TMP1
            H( M+K, J   ) = ZERO
            CALL DROT( M-K, H( M+K+1, J+1 ), 1, H( M+K+1, J ), 1, CO, SI
     $               )
C
C           Update Z.
C
            CALL DROT( J+1, Z( 1, J+1 ), 1, Z( 1, J ), 1, CO, SI )
C
            IF( LCMPQ2 ) THEN
C
C              Update Q2.
C
               CALL DROT( N, Q2( 1, J+1 ), 1, Q2( 1, J ), 1, CO, SI )
            END IF
C
C           Determine a Givens rotation to annihilate Z(j+1,j) from the
C           left.
C
            CALL DLARTG( Z( J, J ), Z( J+1, J ), CO, SI, TMP1 )
C
C           Update Z.
C
            Z( J,   J ) = TMP1
            Z( J+1, J ) = ZERO
            CALL DROT( N-J, Z( J, J+1 ), LDZ, Z( J+1, J+1 ), LDZ, CO, SI
     $               )
            CALL DROT( J+1, Z( M+J, M+1 ), LDZ, Z( M+J+1, M+1 ), LDZ,
     $                 CO, SI )
C
            IF( LCMPU2 ) THEN
C
C              Update U21 and U22.
C
               CALL DROT( M, U21( 1, J ), 1, U21( 1, J+1 ), 1, CO, SI )
               CALL DROT( M, U22( 1, J ), 1, U22( 1, J+1 ), 1, CO, SI )
            END IF
C
C           Determine a Givens rotation to annihilate Z(m+j,m+j+1) from
C           the right.
C
            CALL DLARTG( Z( M+J, M+J ), Z( M+J, M+J+1 ), CO, SI, TMP1 )
C
C           Update Z.
C
            Z( M+J, M+J   ) = TMP1
            Z( M+J, M+J+1 ) = ZERO
            CALL DROT( M, Z( 1, M+J ), 1, Z( 1, M+J+1 ), 1, CO, SI )
            CALL DROT( M-J, Z( M+J+1, M+J ), 1, Z( M+J+1, M+J+1 ), 1,
     $                 CO, SI )
C
C           Update H.
C
            CALL DROT( M, H( 1, M+J ), 1, H( 1, M+J+1 ), 1, CO, SI )
            CALL DROT( M-K+1, H( M+K, M+J ), 1, H( M+K, M+J+1 ), 1, CO,
     $                 SI )
C
            IF( LCMPQ2 ) THEN
C
C              Update Q2.
C
               CALL DROT( N, Q2( 1, M+J ), 1, Q2( 1, M+J+1 ), 1, CO, SI
     $                  )
            END IF
   70    CONTINUE
C
C        V. Annihilate H(m+k,m).
C
         IF( K.LT.M ) THEN
C
C           Determine a Givens rotation to annihilate H(m+k,m) from the
C           right.
C
            CALL DLARTG( H( M+K, N ), H( M+K, M ), CO, SI, TMP1 )
C
C           Update H.
C
            H( M+K, N ) = TMP1
            H( M+K, M ) = ZERO
            CALL DROT( M, H( 1, N ), 1, H( 1, M ), 1, CO, SI )
            CALL DROT( M-K, H( M+K+1, N ), 1, H( M+K+1, M ), 1, CO, SI )
C
C           Update Z.
C
            CALL DROT( M, Z( 1, N ), 1, Z( 1, M ), 1, CO, SI )
            TMP1 = -SI*Z( N, N )
            Z( N, N ) = CO*Z( N, N )
C
            IF( LCMPQ2 ) THEN
C
C              Update Q2.
C
               CALL DROT( N, Q2( 1, N ), 1, Q2( 1, M ), 1, CO, SI )
            END IF
C
C           Determine a Givens rotation to annihilate Z(n,m) from the
C           left.
C
            CALL DLARTG( Z( M, M ), TMP1, CO, SI, TMP2 )
C
C           Update Z.
C
            CALL DROT( M, Z( M, M+1 ), LDZ, Z( N, M+1 ), LDZ, CO, SI )
            Z( M, M ) = TMP2
C
            IF( LCMPU2 ) THEN
C
C              Update U2.
C
               CALL DROT( M, U21( 1, M ), 1, U22( 1, M ), 1, CO, SI )
            END IF
         ELSE
C
C           Determine a Givens rotation to annihilate H(n,m) from the
C           left.
C
            CALL DLARTG( H( M, M ), H( N, M ), CO, SI, TMP1 )
C
C           Update H.
C
            H( M, M ) = TMP1
            H( N, M ) = ZERO
            CALL DROT( M, H( M, M+1 ), LDH, H( N, M+1 ), LDH, CO, SI )
C
C           Update T.
C
            CALL DROT( M, T( M, M+1 ), LDT, T( N, M+1 ), LDT, CO, SI )
            T( M, M ) = CO*T( M, M )
C
            IF( LCMPQ1 ) THEN
C
C              Update Q1.
C
               CALL DROT( N, Q1( 1, M ), 1, Q1( 1, N ), 1, CO, SI )
            END IF
C
C           Determine a Givens rotation to annihilate T( N, M ) from the
C           right.
C
            CALL DLARTG( T( N, N ), -SI*T( M, M ), CO, SI, TMP2 )
C
C           Update T.
C
            CALL DROT( M, T( 1, N ), 1, T( 1, M ), 1, CO, SI )
            T( N, N ) = TMP2
C
            IF( LCMPU1 ) THEN
C
C              Update U1.
C
               CALL DROT( M, U12( 1, M ), 1, U11( 1, M ), 1, CO, SI )
            END IF
         END IF
C
C        VI. Annihilate H(m+k,m+k+2:n).
C
         DO 80 J = M, K+2, -1
C
C           Determine a Givens rotation to annihilate H(m+k,m+j) from
C           the right.
C
            CALL DLARTG( H( M+K, M+J-1 ), H( M+K, M+J ), CO, SI, TMP1 )
C
C           Update H.
C
            CALL DROT( M, H( 1, M+J-1 ), 1, H( 1, M+J ), 1, CO, SI )
            H( M+K, M+J-1 ) = TMP1
            H( M+K, M+J   ) = ZERO
            CALL DROT( M-K, H( M+K+1, M+J-1 ), 1, H( M+K+1, M+J ), 1,
     $                 CO, SI )
C
C           Update Z.
C
            CALL DROT( M, Z( 1, M+J-1 ), 1, Z( 1, M+J ), 1, CO, SI )
            CALL DROT( M-J+2, Z( M+J-1, M+J-1 ), 1, Z( M+J-1, M+J ), 1,
     $                 CO, SI )
C
            IF( LCMPQ2 ) THEN
C
C              Update Q2.
C
               CALL DROT( N, Q2( 1, M+J-1 ), 1, Q2( 1, M+J ), 1, CO, SI
     $                  )
            END IF
C
C           Determine a Givens rotation to annihilate Z(m+j-1,m+j) from
C           the left.
C
            CALL DLARTG( Z( M+J, M+J ), Z( M+J-1, M+J ), CO, SI, TMP1 )
C
C           Update Z.
C
            Z( M+J,   M+J ) = TMP1
            Z( M+J-1, M+J ) = ZERO
            CALL DROT( J-1, Z( M+J, M+1 ), LDZ, Z( M+J-1, M+1 ), LDZ,
     $                 CO, SI )
            CALL DROT( N-J+2, Z( J, J-1 ), LDZ, Z( J-1, J-1 ), LDZ, CO,
     $                 SI )
C
            IF( LCMPU2 ) THEN
C
C              Update U2.
C
               CALL DROT( M, U21( 1, J ), 1, U21( 1, J-1 ), 1, CO, SI )
               CALL DROT( M, U22( 1, J ), 1, U22( 1, J-1 ), 1, CO, SI )
            END IF
C
C           Determine a Givens rotation to annihilate Z(j,j-1) from the
C           right.
C
            CALL DLARTG( Z( J, J ), Z( J, J-1 ), CO, SI, TMP1 )
C
C           Update Z.
C
            Z( J, J   ) = TMP1
            Z( J, J-1 ) = ZERO
            CALL DROT( J-1, Z( 1, J ), 1, Z( 1, J-1 ), 1, CO, SI )
C
C           Update H.
C
            CALL DROT( M, H( 1, J ), 1, H( 1, J-1 ), 1, CO, SI )
            CALL DROT( M-K+1, H( M+K, J ), 1, H( M+K, J-1 ), 1, CO, SI )
C
            IF( LCMPQ2 ) THEN
C
C              Update Q2.
C
               CALL DROT( N, Q2( 1, J ), 1, Q2( 1, J-1 ), 1, CO, SI )
            END IF
   80    CONTINUE
C
   90 CONTINUE
C
C     Now T, Z, H are in block forms (1) and H22' is upper Hessenberg.
C
C     STEP 3: Apply periodic QZ algorithm to the generalized matrix
C
C                            -1    -1        -1    -1
C             product H22 T22   T11   H11 Z11   Z22   to transform H22'
C             to upper quasi-triangular form while T11, T22', Z11, Z22',
C             and H11 stay in upper triangular form.
C
C     Determine mode of computations.
C
      IF( LTRI .OR. LCMPQ1 .OR. LCMPQ2 .OR. LCMPU1 .OR. LCMPU2 ) THEN
         CMPQ = 'Initialize'
         IQ   =  1
         IMAT =  6*MM + 1
         IWRK = 12*MM + 1
      ELSE
         CMPQ = 'No Computation'
         IMAT = 1
         IWRK = 6*MM + 1
      END IF
C
      IF( LTRI ) THEN
         CMPSC = 'Schur Form'
      ELSE
         CMPSC = 'Eigenvalues Only'
      END IF
C
C     Save matrices in structure that is required by MB03BD.
C
      CALL MA02AD( 'Lower', M, M, H( M+1, M+1 ), LDH, DWORK( IMAT ), M )
      CALL DCOPY(  M-1, H( M+1, M+2 ), LDH+1, DWORK( IMAT+1 ), M+1 )
      CALL DLASET( 'Lower', M-2, M-2, ZERO, ZERO, DWORK( IMAT+2 ), M )
      CALL MA02AD( 'Lower', M, M, T( M+1, M+1 ), LDT, DWORK( IMAT+MM ),
     $             M )
      CALL DLASET( 'Lower', M-1, M-1, ZERO, ZERO, DWORK( IMAT+MM+1 ),
     $             M )
      CALL DLACPY( 'Upper', M, M, T, LDT, DWORK( IMAT+2*MM ), M )
      CALL DLASET( 'Lower', M-1, M-1, ZERO, ZERO, DWORK( IMAT+2*MM+1 ),
     $             M )
      CALL DLACPY( 'Upper', M, M, H, LDH, DWORK( IMAT+3*MM ), M )
      CALL DLASET( 'Lower', M-1, M-1, ZERO, ZERO, DWORK( IMAT+3*MM+1 ),
     $             M )
      CALL DLACPY( 'Upper', M, M, Z, LDZ, DWORK( IMAT+4*MM ), M )
      CALL DLASET( 'Lower', M-1, M-1, ZERO, ZERO, DWORK( IMAT+4*MM+1 ),
     $             M )
      CALL MA02AD( 'Lower', M, M, Z( M+1, M+1 ), LDZ,
     $             DWORK( IMAT+5*MM ), M )
      CALL DLASET( 'Lower', M-1, M-1, ZERO, ZERO, DWORK( IMAT+5*MM+1 ),
     $             M )
C
      IWORK( M+1 ) =  1
      IWORK( M+2 ) = -1
      IWORK( M+3 ) = -1
      IWORK( M+4 ) =  1
      IWORK( M+5 ) = -1
      IWORK( M+6 ) = -1
C
C     Apply periodic QZ algorithm.
C     Workspace:          need   IWRK + MAX( 2*M, 48 ) - 1.
C     Integer workspace:  need   M + 18.
C
      CALL MB03BD( CMPSC, 'Careful', CMPQ, IDUM, 6, M, 1, 1, M,
     $             IWORK( M+1 ), DWORK( IMAT ), M, M, DWORK( IQ ), M, M,
     $             ALPHAR, ALPHAI, BETA, IWORK, IWORK( M+7 ),
     $             LIWORK-( M+6 ), DWORK( IWRK ), LDWORK-IWRK+1, IWARN,
     $             INFO )
      IF( IWARN.GT.0 ) THEN
         INFO = 1
         RETURN
      ELSE IF( INFO.GT.0 ) THEN
         INFO = 2
         RETURN
      END IF
C
C     Compute the "non-negative" eigenvalues of the pencil aTZ - bH.
C     These are the eigenvalues with positive real parts or with
C     non-negative imaginary parts, when real parts are zero.
C
      DO 100 I = 1, M
         EIG = SQRT( DCMPLX( ALPHAR( I ), ALPHAI( I ) ) )
         ALPHAR( I ) = DIMAG( EIG )
         ALPHAI( I ) = DBLE(  EIG )
         TMP2 = PREC*DLAPY2( ALPHAR( I ), ALPHAI( I ) )
         IF( ABS( ALPHAR( I ) ).LT.TMP2 ) THEN
            ALPHAR( I ) = ZERO
            IF( ALPHAI( I ).LT.ZERO )
     $          ALPHAI( I ) = -ALPHAI( I )
         END IF
         IF( ABS( ALPHAI( I ) ).LT.TMP2 ) THEN
            ALPHAI( I ) = ZERO
            IF( ALPHAR( I ).LT.ZERO )
     $          ALPHAR( I ) = -ALPHAR( I )
         END IF
         IF( IWORK( I ).GE.2*EMIN .AND. IWORK( I ).LE.2*EMAX ) THEN
C
C           If B = SQRT(BASE**IWORK(i)) is between underflow and
C           overflow threshold, BETA(i) is divided by B.
C
            BETA(  I ) = BETA( I )/BASE**( HALF*IWORK( I ) )
            IWORK( I ) = 0
         ELSE
C
C           The eigenvalues are defined by ALPHAR, ALPHAI, BETA, and
C           IWORK, as for the SLICOT Library routine MB03BD.
C
            INFO = 3
            IF( MOD( IWORK( I ), 2 ).NE.0 )
     $         BETA( I ) =  BETA( I )/SQRB
            IWORK( I )   = IWORK( I )/2
         END IF
  100 CONTINUE
C
      IF( LTRI ) THEN
C
C        Update H.
C
         CALL DLACPY( 'Upper', M, M, DWORK( IMAT+3*MM ), M, H, LDH )
         CALL MA02AD( 'Full', M, M, DWORK( IMAT ), M, H( M+1, M+1 ),
     $                LDH )
         CALL DGEMM( 'Transpose', 'No Transpose', M, M, M, ONE,
     $               DWORK( IQ+3*MM ), M, H( 1, M+1 ), LDH, ZERO,
     $               DWORK( IMAT ), M )
         CALL DGEMM( 'No Transpose', 'No Transpose', M, M, M, ONE,
     $               DWORK( IMAT ), M, DWORK, M, ZERO, H( 1, M+1 ),
     $               LDH )
C
C        Update T.
C
         CALL DLACPY( 'Upper', M, M, DWORK( IMAT+2*MM ), M, T, LDT )
         CALL DGEMM( 'Transpose', 'No Transpose', M, M, M, ONE,
     $               DWORK( IQ+3*MM ), M, T( 1, M+1 ), LDT, ZERO,
     $               DWORK( IMAT ), M )
         CALL DGEMM( 'No Transpose', 'No Transpose', M, M, M, ONE,
     $               DWORK( IMAT ), M, DWORK( IQ+2*MM ), M, ZERO,
     $               T( 1, M+1 ), LDT )
         CALL MA02AD( 'Upper', M, M, DWORK( IMAT+MM ), M, T( M+1, M+1 ),
     $                LDT )
C
C        Update Z.
C
         CALL DLACPY( 'Upper', M, M, DWORK( IMAT+4*MM ), M, Z, LDZ )
         CALL DGEMM( 'Transpose', 'No Transpose', M, M, M, ONE,
     $               DWORK( IQ+5*MM ), M, Z( 1, M+1 ), LDZ, ZERO,
     $               DWORK( IMAT ), M )
         CALL DGEMM( 'No Transpose', 'No Transpose', M, M, M, ONE,
     $               DWORK( IMAT ), M, DWORK, M, ZERO, Z( 1, M+1 ),
     $               LDZ )
         CALL MA02AD( 'Upper', M, M, DWORK( IMAT+5*MM ), M,
     $                Z( M+1, M+1 ), LDZ )
C
         IF( LCMPQ1 ) THEN
C
C           Update Q1.
C
            CALL DGEMM( 'No Transpose', 'No Transpose', N, M, M, ONE,
     $                  Q1, LDQ1, DWORK( IQ+3*MM ), M, ZERO,
     $                  DWORK( IMAT ), N )
            CALL DLACPY( 'Full', N, M, DWORK( IMAT ), N, Q1, LDQ1 )
            CALL DGEMM( 'No Transpose', 'No Transpose', N, M, M, ONE,
     $                  Q1( 1, M+1 ), LDQ1, DWORK( IQ+MM ), M, ZERO,
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
     $                  Q2, LDQ2, DWORK( IQ+4*MM ), M, ZERO,
     $                  DWORK( IMAT ), N )
            CALL DLACPY( 'Full', N, M, DWORK( IMAT ), N, Q2, LDQ2 )
            CALL DGEMM( 'No Transpose', 'No Transpose', N, M, M, ONE,
     $                  Q2( 1, M+1 ), LDQ2, DWORK, M, ZERO,
     $                  DWORK( IMAT ), N )
            CALL DLACPY( 'Full', N, M, DWORK( IMAT ), N, Q2( 1, M+1 ),
     $                   LDQ2 )
         END IF
C
         IF( LCMPU1 ) THEN
C
C           Update U11 and U12.
C
            CALL DGEMM( 'No Transpose', 'No Transpose', M, M, M, ONE,
     $                  U11, LDU11, DWORK( IQ+2*MM ), M, ZERO,
     $                  DWORK( IMAT ), M )
            CALL DLACPY( 'Full', M, M, DWORK( IMAT ), M, U11, LDU11 )
            CALL DGEMM( 'No Transpose', 'No Transpose', M, M, M, ONE,
     $                  U12, LDU12, DWORK( IQ+2*MM ), M, ZERO,
     $                  DWORK( IMAT ), M )
            CALL DLACPY( 'Full', M, M, DWORK( IMAT ), M, U12, LDU12 )
         END IF
C
         IF( LCMPU2 ) THEN
C
C           Update U21 and U22.
C
            CALL DGEMM( 'No Transpose', 'No Transpose', M, M, M, ONE,
     $                  U21, LDU21, DWORK( IQ+5*MM ), M, ZERO,
     $                  DWORK( IMAT ), M )
            CALL DLACPY( 'Full', M, M, DWORK( IMAT ), M, U21, LDU21 )
            CALL DGEMM( 'No Transpose', 'No Transpose', M, M, M, ONE,
     $                  U22, LDU22, DWORK( IQ+5*MM ), M, ZERO,
     $                  DWORK( IMAT ), M )
            CALL DLACPY( 'Full', M, M, DWORK( IMAT ), M, U22, LDU22 )
         END IF
      END IF
C
      DWORK( 1 ) = OPTDW
      DWORK( 2 ) = BASE
      RETURN
C *** Last line of MB04AD ***
      END
