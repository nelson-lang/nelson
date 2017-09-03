      SUBROUTINE MB04HD( COMPQ1, COMPQ2, N, A, LDA, B, LDB, Q1, LDQ1,
     $                   Q2, LDQ2, BWORK, IWORK, LIWORK, DWORK, LDWORK,
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
C     To compute the transformed matrices A and B, using orthogonal
C     matrices Q1 and Q2 for a real N-by-N regular pencil
C
C                    ( A11   0  )     (  0   B12 )
C       aA - bB =  a (          ) - b (          ),                  (1)
C                    (  0   A22 )     ( B21   0  )
C
C     where A11, A22 and B12 are upper triangular and the generalized
C                       -1        -1
C     matrix product A11   B12 A22   B21 is upper quasi-triangular,
C     such that the matrix Q2' A Q1 is upper triangular and Q2' B Q1 is
C     upper quasi-triangular.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     COMPQ1  CHARACTER*1
C             Specifies whether to compute the orthogonal transformation
C             matrix Q1, as follows:
C             = 'N':  Q1 is not computed;
C             = 'I':  the array Q1 is initialized internally to the unit
C                     matrix, and the orthogonal matrix Q1 is returned;
C             = 'U':  the array Q1 contains an orthogonal matrix Q01 on
C                     entry, and the matrix Q01*Q1 is returned, where Q1
C                     is the product of the orthogonal transformations
C                     that are applied on the right to the pencil
C                     aA - bB in (1).
C
C     COMPQ2  CHARACTER*1
C             Specifies whether to compute the orthogonal transformation
C             matrix Q2, as follows:
C             = 'N':  Q2 is not computed;
C             = 'I':  the array Q2 is initialized internally to the unit
C                     matrix, and the orthogonal matrix Q2 is returned;
C             = 'U':  the array Q2 contains an orthogonal matrix Q02 on
C                     entry, and the matrix Q02*Q2 is returned, where Q2
C                     is the product of the orthogonal transformations
C                     that are applied on the left to the pencil aA - bB
C                     in (1).
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             Order of the pencil aA - bB, N has to be even.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
C             On entry, the leading N-by-N block diagonal part of this
C             array must contain the matrix A in (1). The off-diagonal
C             blocks need not be set to zero.
C             On exit, the leading N-by-N part of this array contains
C             the transformed upper triangular matrix.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1, N).
C
C     B       (input/output) DOUBLE PRECISION array, dimension (LDB, N)
C             On entry, the leading N-by-N block anti-diagonal part of
C             this array must contain the matrix B in (1). The diagonal
C             blocks need not be set to zero.
C             On exit, the leading N-by-N part of this array contains
C             the transformed upper quasi-triangular matrix.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= MAX(1, N).
C
C     Q1      (input/output) DOUBLE PRECISION array, dimension (LDQ1, N)
C             On entry, if COMPQ1 = 'U', then the leading N-by-N part of
C             this array must contain a given matrix Q01, and on exit,
C             the leading N-by-N part of this array contains the product
C             of the input matrix Q01 and the transformation matrix Q1
C             used to transform the matrices A and B.
C             On exit, if COMPQ1 = 'I', then the leading N-by-N part of
C             this array contains the orthogonal transformation matrix
C             Q1.
C             If COMPQ1 = 'N' this array is not referenced.
C
C     LDQ1    INTEGER
C             The leading dimension of the array Q1.
C             LDQ1 >= 1,         if COMPQ1 = 'N';
C             LDQ1 >= MAX(1, N), if COMPQ1 = 'I' or COMPQ1 = 'U'.
C
C     Q2      (input/output) DOUBLE PRECISION array, dimension (LDQ2, N)
C             On entry, if COMPQ2 = 'U', then the leading N-by-N part of
C             this array must contain a given matrix Q02, and on exit,
C             the leading N-by-N part of this array contains the product
C             of the input matrix Q02 and the transformation matrix Q2
C             used to transform the matrices A and B.
C             On exit, if COMPQ2 = 'I', then the leading N-by-N part of
C             this array contains the orthogonal transformation matrix
C             Q2.
C             If COMPQ2 = 'N' this array is not referenced.
C
C     LDQ2    INTEGER
C             The leading dimension of the array Q2.
C             LDQ2 >= 1,         if COMPQ2 = 'N';
C             LDQ2 >= MAX(1, N), if COMPQ2 = 'I' or COMPQ2 = 'U'.
C
C     Workspace
C
C     BWORK   LOGICAL array, dimension (N/2)
C
C     IWORK   INTEGER array, dimension (LIWORK)
C
C     LIWORK  INTEGER
C             The dimension of the array IWORK.
C             LIWORK >= N/2 + 32.
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal LDWORK.
C             On exit, if INFO = -16, DWORK(1) returns the minimum value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The dimension of the array DWORK.
C             LDWORK >= 2*N*N + MAX( N/2 + 168, 272 ).
C             For good performance LDWORK should be generally larger.
C
C             If LDWORK = -1  a workspace query is assumed; the 
C             routine only calculates the optimal size of the DWORK
C             array, returns this value as the first entry of the DWORK
C             array, and no error message is issued by XERBLA. 
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0: succesful exit;
C             < 0: if INFO = -i, the i-th argument had an illegal value;
C             = 1: the periodic QZ algorithm failed to reorder the
C                  eigenvalues (the problem is very ill-conditioned) in
C                  the SLICOT Library routine MB03KD;
C             = 2: the standard QZ algorithm failed in the LAPACK
C                  routine DGGEV, called by the SLICOT routine MB03DD;
C             = 3: the standard QZ algorithm failed in the LAPACK
C                  routines DGGES or DHGEQZ, called by the SLICOT
C                  routines MB03DD or MB03FD;
C             = 4: the standard QZ algorithm failed to reorder the
C                  eigenvalues in the LAPACK routine DTGSEN, called by
C                  the SLICOT routine MB03DD.
C
C     METHOD
C
C     First, the periodic QZ algorithm (see also [2] and [3]) is applied
C                                     -1        -1
C     to the formal matrix product A11   B12 A22   B21 to reorder the
C     eigenvalues, i.e., orthogonal matrices V1, V2, V3 and V4 are
C     computed such that V2' A11 V1, V2' B12 V3, V4' A22 V3 and
C     V4' B21 V1 keep the triangular form, but they can be partitioned
C     into 2-by-2 block forms and the last diagonal blocks correspond to
C     all nonpositive real eigenvalues of the formal product, and the
C     first diagonal blocks correspond to the remaining eigenvalues.
C
C     Second, Q1 = diag(V1, V3), Q2 = diag(V2, V4) and
C
C                      ( AA11 AA12   0    0  )
C                      (                     )
C                      (   0  AA22   0    0  )
C     A := Q2' A Q1 =: (                     ),
C                      (   0    0  AA33 AA34 )
C                      (                     )
C                      (   0    0    0  AA44 )
C
C                      (   0    0  BB13 BB14 )
C                      (                     )
C                      (   0    0    0  BB24 )
C     B := Q2' B Q1 =: (                     ),
C                      ( BB31 BB32   0    0  )
C                      (                     )
C                      (   0  BB42   0    0  )
C
C                            -1          -1
C     are set, such that AA22   BB24 AA44   BB42 has only nonpositive
C     real eigenvalues.
C
C     Third, the permutation matrix
C
C         (  I  0  0  0  )
C         (              )
C         (  0  0  I  0  )
C     P = (              ),
C         (  0  I  0  0  )
C         (              )
C         (  0  0  0  I  )
C
C     where I denotes the identity matrix of appropriate size, is used
C     to transform aA - bB to block upper triangular form
C
C                   ( AA11   0  | AA12   0  )
C                   (           |           )
C                   (   0  AA33 |   0  AA34 )   ( AA1  *  )
C     A := P' A P = (-----------+-----------) = (         ),
C                   (   0    0  | AA22   0  )   (  0  AA2 )
C                   (           |           )
C                   (   0    0  |   0  AA44 )
C
C                   (   0  BB13 |   0  BB14 )
C                   (           |           )
C                   ( BB31   0  | BB32   0  )   ( BB1  *  )
C     B := P' B P = (-----------+-----------) = (         ).
C                   (   0    0  |   0  BB24 )   (  0  BB2 )
C                   (           |           )
C                   (   0    0  | BB42   0  )
C
C     Then, further orthogonal transformations that are provided by
C     MB03FD and MB03DD are used to triangularize the subpencil
C     aAA1 - bBB1.
C
C     Finally, the subpencil aAA2 - bBB2 is triangularized by applying a
C     special permutation matrix.
C
C     See also page 31 in [1] for more details.
C
C     REFERENCES
C
C     [1] Benner, P., Byers, R., Losse, P., Mehrmann, V. and Xu, H.
C         Numerical Solution of Real Skew-Hamiltonian/Hamiltonian
C         Eigenproblems.
C         Tech. Rep., Technical University Chemnitz, Germany,
C         Nov. 2007.
C
C     [2] Bojanczyk, A., Golub, G. H. and Van Dooren, P.
C         The periodic Schur decomposition: algorithms and applications.
C         In F.T. Luk (editor), Advanced Signal Processing Algorithms,
C         Architectures, and Implementations III, Proc. SPIE Conference,
C         vol. 1770, pp. 31-42, 1992.
C
C     [3] Hench, J. J. and Laub, A. J.
C         Numerical Solution of the discrete-time periodic Riccati
C         equation. IEEE Trans. Automat. Control, 39, 1197-1210, 1994.
C
C     NUMERICAL ASPECTS
C                                                               3
C     The algorithm is numerically backward stable and needs O(N ) real
C     floating point operations.
C
C     CONTRIBUTOR
C
C     Matthias Voigt, Fakultaet fuer Mathematik, Technische Universitaet
C     Chemnitz, December 08, 2008.
C     V. Sima, Dec. 2009 (SLICOT version of the routine DBTUMT).
C
C     REVISIONS
C
C     V. Sima, Aug. 2009, Feb. 2010, Jul. 2010, Sep.-Nov. 2010.
C
C     KEYWORDS
C
C     Eigenvalue reordering, matrix pencil, periodic QZ algorithm,
C     upper (quasi-)triangular matrix.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, HUND2
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, HUND2 = 2.0D+2 )
C
C     .. Scalar Arguments ..
      CHARACTER          COMPQ1, COMPQ2
      INTEGER            INFO, LDA, LDB, LDQ1, LDQ2, LDWORK, LIWORK, N
C
C     .. Array Arguments ..
      LOGICAL            BWORK( * )
      INTEGER            IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), DWORK( * ),
     $                   Q1( LDQ1, * ), Q2( LDQ2, * )
C
C     .. Local Scalars ..
      LOGICAL            LCMPQ1, LCMPQ2, LINIQ1, LINIQ2, LQUERY, LUPDQ1,
     $                   LUPDQ2
      INTEGER            DIM1, DIM2, I, I1, I1LOLE, I1LORI, I1UPLE,
     $                   I1UPRI, I2, I2LOLE, I2LORI, I2UPLE, I2UPRI, I3,
     $                   IA, IA11, IA22, IALOLE, IALORI, IAUPLE, IAUPRI,
     $                   IB, IB1, IB12, IB2, IB21, IBLOLE, IBLORI,
     $                   IBUPLE, IBUPRI, IJ1, IJ2, ITMP, ITMP2, ITMP3,
     $                   IV1, IV2, IV3, IV4, IWRK, J, K, KSCHUR, M, M1,
     $                   M2, M4, MINWRK, MM, MP1, NR, NROW, OPTWRK, R,
     $                   SDIM
      DOUBLE PRECISION   BASE, LGBAS, TMP2, TMP3, ULP
C
C     .. Local Arrays ..
      INTEGER            IDUM( 1 )
      DOUBLE PRECISION   DUM( 1 )
C
C     .. External Functions ..
      LOGICAL            LSAME, SB02OW
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH, LSAME, SB02OW
C
C     .. External Subroutines ..
      EXTERNAL           DAXPY, DCOPY, DGEMM, DGEMV, DGGES, DGGEV,
     $                   DLACPY, DLASET, DSCAL, DTGSEN, MA01BD, MB03BA,
     $                   MB03DD, MB03FD, MB03KD, XERBLA
C
C     .. Intrinsic Functions ..
      INTRINSIC          DBLE, INT, LOG, MAX, MIN, MOD
C
C     .. Executable Statements ..
C
C     Decode the input arguments.
C
      M  = N/2
      MM = M*M
      LINIQ1 = LSAME( COMPQ1, 'I' )
      LUPDQ1 = LSAME( COMPQ1, 'U' )
      LINIQ2 = LSAME( COMPQ2, 'I' )
      LUPDQ2 = LSAME( COMPQ2, 'U' )
      LCMPQ1 = LINIQ1 .OR. LUPDQ1
      LCMPQ2 = LINIQ2 .OR. LUPDQ2
      LQUERY = LDWORK.EQ.-1
      MINWRK = 2*N*N + MAX( M + 168, 272 )
C
C     Test the input arguments.
C
      INFO = 0
      IF( .NOT.( LSAME( COMPQ1, 'N' ) .OR. LCMPQ1 ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( LSAME( COMPQ2, 'N' ) .OR. LCMPQ2 ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 .OR. MOD( N, 2 ).NE.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDQ1.LT.1 .OR. LCMPQ1 .AND. LDQ1.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE IF( LDQ2.LT.1 .OR. LCMPQ2 .AND. LDQ2.LT.MAX( 1, N ) ) THEN
         INFO = -11
      ELSE IF( LIWORK.LT.M + 32 ) THEN
         INFO = -14
      ELSE IF( .NOT. LQUERY .AND. LDWORK.LT.MINWRK ) THEN
         DWORK( 1 ) = MINWRK
         INFO = -16
      END IF
C
      IF( INFO.NE.0) THEN
         CALL XERBLA( 'MB04HD', -INFO )
         RETURN
      ELSE
C
C        Compute optimal workspace.
C
         I = MAX( 1, MIN( 4, N ) )
         CALL DGGES(  'Vectors', 'Vectors', 'Sorted', SB02OW, I, A, LDA,
     $                B, LDB, IDUM, DWORK, DWORK, DWORK, Q1, I, Q2, I,
     $                DWORK, -1, BWORK, INFO )
         CALL DGGES(  'Vectors', 'Vectors', 'Not sorted', SB02OW, I, A,
     $                LDA, B, LDB, IDUM, DWORK, DWORK, DWORK, Q1, I, Q2,
     $                I, DWORK( 2 ), -1, BWORK, INFO )
         CALL DGGEV(  'No Vector', 'No Vector', 2, A, LDA, B, LDB,
     $                DWORK, DWORK, DWORK, DUM, 1, DUM, 1, DWORK( 3 ),
     $                -1, INFO )
         CALL DTGSEN( 0, .TRUE., .TRUE., BWORK, I, A, LDA, B, LDB,
     $                DWORK, DWORK, DWORK, Q1, I, Q2, I, IDUM, TMP2,
     $                TMP2, DUM, DWORK( 4 ), -1, IDUM, 1, INFO )
C
         OPTWRK = MAX( 64 + MAX( 12 + INT( DWORK( 1 ) ), 4*M + 8,
     $                           24 + INT( DWORK( 2 ) ),
     $                            6 + INT( DWORK( 3 ) ),
     $                           12 + INT( DWORK( 4 ) ), 4*N ), MINWRK )
         IF( LQUERY ) THEN
            DWORK( 1 ) = OPTWRK
            RETURN
         END IF
      END IF
C
C     Quick return if possible.
C
      IF( N.EQ.0 ) THEN
         DWORK( 1 ) = ONE
         RETURN
      END IF
C
C     Computations. Note that MB03KD needs reverse ordering of the
C     factors in the formal matrix product, compared to MA01BD, MB03BA.
C
      IA11 = 1
      IB12 = IA11 + MM
      IA22 = IB12 + MM
      IB21 = IA22 + MM
      IV1  = IB21 + MM
      IV2  = IV1  + MM
      IV3  = IV2  + MM
      IV4  = IV3  + MM
      MP1  = M    + 1
C
C     Get the machine parameters.
C
      ULP   = DLAMCH( 'Precision' )
      BASE  = DLAMCH( 'Base' )
      LGBAS = LOG( BASE )
C
C     Compute maps to access the factors of the formal matrix product.
C
      K = 4
      KSCHUR = 4
      IWORK( 2*K+1 ) = -1
      IWORK( 2*K+2 ) =  1
      IWORK( 2*K+3 ) = -1
      IWORK( 2*K+4 ) =  1
      CALL MB03BA( K, KSCHUR, IWORK( 2*K+1 ), I, IWORK, IWORK( K+1 ) )
C
C     Store the factors of the formal matrix product.
C
      DUM( 1 ) = ZERO
      CALL DCOPY(  4*MM, DUM, 0, DWORK, 1 )
      CALL DLACPY( 'Upper', M, M, A, LDA, DWORK, M )
      CALL DLACPY( 'Upper', M, M, A( MP1, MP1 ), LDA, DWORK( IA22 ), M )
      CALL DLACPY( 'Upper', M, M, B( 1, MP1 ),   LDB, DWORK( IB12 ), M )
      CALL DLACPY( 'Upper', M, M, B( MP1, 1 ),   LDB, DWORK( IB21 ), M )
      IF( M.GT.1 )
     $   CALL DCOPY( M-1, B( M+2, 1 ), LDB+1, DWORK( IB21+1 ), MP1 )
C
C     Set BWORK according to the eigenvalues of the formal matrix
C     product in Schur-triangular form.
C     Workspace:   need   4*M*M + 2.
C
      J  = 1
      IA = IV1
      IB = IA + 1
C
C     WHILE( J.LE.M ) DO
   10 CONTINUE
      IF( J.LT.M ) THEN
         IF( DWORK( IB21+J+(J-1)*M ).EQ.ZERO ) THEN
            CALL MA01BD( BASE, LGBAS, K, IWORK( 2*K+1 ),
     $                   DWORK( (J-1)*M+J ), MM, DWORK( IA ),
     $                   DWORK( IB ), IWORK( 3*K+1 ) )
            BWORK( J ) = DWORK( IA ).GT.ZERO
            J = J + 1
            GO TO 10
         ELSE
            BWORK( J   ) = .TRUE.
            BWORK( J+1 ) = .TRUE.
            J = J + 2
            GO TO 10
         END IF
      ELSE IF ( J.EQ.M ) THEN
         CALL MA01BD( BASE, LGBAS, K, IWORK( 2*K+1 ), DWORK( MM ), MM,
     $                DWORK( IA ), DWORK( IB ), IWORK( 3*K+1 ) )
         BWORK( J ) = DWORK( IA ).GT.ZERO
      END IF
C     END WHILE 10
C
C     Check if BWORK(J) = .TRUE. for all J.
C
      J = 1
C     WHILE( J.LE.M and BWORK(J) ) DO
   20 CONTINUE
      IF( J.LE.M .AND. BWORK(J) ) THEN
         J = J + 1
         GO TO 20
      END IF
C     END WHILE 20
C
      IF( J.NE.MP1 ) THEN
C
C        Apply periodic QZ algorithm for reordering the eigenvalues.
C        Workspace:   need   8*M*M + MAX(42*K + M, 80*K - 48), K = 4,
C                     if there is at least a pair of adjacent blocks
C                     of order 2 involved in reordering, and M > 10.
C                     Otherwise, the MAX term is slightly smaller.
C
         IWRK = 2*IV1 - 1
         IB21 = 1
         IA22 = IB21 + MM
         IB12 = IA22 + MM
         IA11 = IB12 + MM
C
         KSCHUR = 1
         IWORK( 2*K+1 ) =  1
         IWORK( 2*K+2 ) = -1
         IWORK( 2*K+3 ) =  1
         IWORK( 2*K+4 ) = -1
C
         DO 30 I = 1, K
            IWORK(   I   ) = M
            IWORK(   K+I ) = 0
            IWORK( 3*K+I ) = 1 + ( I - 1 )*MM
   30    CONTINUE
C
         CALL DCOPY( MM*K, DUM, 0, DWORK( IB21 ), 1 )
         CALL DLACPY( 'Upper', M, M, B( MP1, 1 ), LDB, DWORK( IB21 ),
     $                M )
         CALL DLACPY( 'Upper', M, M, B( 1, MP1 ), LDB, DWORK( IB12 ),
     $                M )
         CALL DLACPY( 'Upper', M, M, A, LDA, DWORK( IA11 ), M )
         CALL DLACPY( 'Upper', M, M, A( MP1, MP1 ), LDA, DWORK( IA22 ),
     $                M )
         IF( M.GT.1 )
     $      CALL DCOPY( M-1, B( M+2, 1 ), LDB+1, DWORK( IB21+1 ), MP1 )
C
         CALL MB03KD( 'Initialize', IDUM, 'NotStrong', K, M, KSCHUR,
     $                 IWORK, IWORK( K+1 ), IWORK( 2*K+1 ), BWORK,
     $                 DWORK, IWORK, IWORK( 3*K+1 ), DWORK( IV1 ),
     $                 IWORK, IWORK( 3*K+1 ), M1, HUND2, IWORK( 4*K+1 ),
     $                 DWORK( IWRK ), LDWORK-IWRK+1, INFO )
         IF( INFO.GT.0 )
     $      RETURN
C
         M2 = M  - M1
         I1 = M1 + 1
         I2 = I1 + M1
         I3 = I2 + M2
         M4 = 2*M2
C
C        If Q1 and/or Q2 are user-initialized, update them.
C        The (2,1) block of A is used as workspace.
C
         IF( LUPDQ1 ) THEN
            CALL DGEMM(  'No Transpose', 'No Transpose', M, M, M, ONE,
     $                   Q1, LDQ1, DWORK( IV1 ), M, ZERO, A( MP1, 1 ),
     $                   LDA )
            CALL DLACPY( 'Full', M, M, A( MP1, 1 ), LDA, Q1, LDQ1 )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, M, M, ONE,
     $                   Q1( MP1, 1 ), LDQ1, DWORK( IV1 ), M, ZERO,
     $                   A( MP1, 1 ), LDA )
            CALL DLACPY( 'Full', M, M, A( MP1, 1 ), LDA, Q1( MP1, 1 ),
     $                   LDQ1 )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, M, M, ONE,
     $                   Q1( 1, MP1 ), LDQ1, DWORK( IV3 ), M, ZERO,
     $                   A( MP1, 1 ), LDA )
            CALL DLACPY( 'Full', M, M, A( MP1, 1 ), LDA, Q1( 1, MP1 ),
     $                   LDQ1 )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, M, M, ONE,
     $                   Q1( MP1, MP1 ), LDQ1, DWORK( IV3 ), M, ZERO,
     $                   A( MP1, 1 ), LDA )
            CALL DLACPY( 'Full', M, M, A( MP1, 1 ), LDA, Q1( MP1, MP1 ),
     $                   LDQ1 )
C
            IF( M2.GT.0 ) THEN
               CALL DLACPY( 'Full', M, M, Q1( 1, I1 ), LDQ1,
     $                      A( MP1, 1 ), LDA )
               CALL DLACPY( 'Full', M, M1, A( MP1, M2+1 ), LDA,
     $                      Q1( 1, I1 ), LDQ1 )
               CALL DLACPY( 'Full', M, M2, A( MP1, 1 ), LDA,
     $                      Q1( 1, I2 ), LDQ1 )
               CALL DLACPY( 'Full', M, M, Q1( MP1, I1 ), LDQ1,
     $                      A( MP1, 1 ), LDA )
               CALL DLACPY( 'Full', M, M1, A( MP1, M2+1 ), LDA,
     $                      Q1( MP1, I1 ), LDQ1 )
               CALL DLACPY( 'Full', M, M2, A( MP1, 1 ), LDA,
     $                      Q1( MP1, I2 ), LDQ1 )
            END IF
         END IF
C
         IF( LUPDQ2 ) THEN
            CALL DGEMM(  'No Transpose', 'No Transpose', M, M, M, ONE,
     $                   Q2, LDQ2, DWORK( IV4 ), M, ZERO, A( MP1, 1 ),
     $                   LDA )
            CALL DLACPY( 'Full', M, M, A( MP1, 1 ), LDA, Q2, LDQ2 )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, M, M, ONE,
     $                   Q2( MP1, 1 ), LDQ2, DWORK( IV4 ), M, ZERO,
     $                   A( MP1, 1 ), LDA )
            CALL DLACPY( 'Full', M, M, A( MP1, 1 ), LDA, Q2( MP1, 1 ),
     $                   LDQ2 )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, M, M, ONE,
     $                   Q2( 1, MP1 ), LDQ2, DWORK( IV2 ), M, ZERO,
     $                   A( MP1, 1 ), LDA )
            CALL DLACPY( 'Full', M, M, A( MP1, 1 ), LDA, Q2( 1, MP1 ),
     $                   LDQ2 )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, M, M, ONE,
     $                   Q2( MP1, MP1 ), LDQ2, DWORK( IV2 ), M, ZERO,
     $                   A( MP1, 1 ), LDA )
            CALL DLACPY( 'Full', M, M, A( MP1, 1 ), LDA, Q2( MP1, MP1 ),
     $                   LDQ2 )
C
            IF( M2.GT.0 ) THEN
               CALL DLACPY( 'Full', M, M, Q2( 1, I1 ), LDQ2,
     $                      A( MP1, 1 ), LDA )
               CALL DLACPY( 'Full', M, M1, A( MP1, M2+1 ), LDA,
     $                      Q2( 1, I1 ), LDQ2 )
               CALL DLACPY( 'Full', M, M2, A( MP1, 1 ), LDA,
     $                      Q2( 1, I2 ), LDQ2 )
               CALL DLACPY( 'Full', M, M, Q2( MP1, I1 ), LDQ2,
     $                      A( MP1, 1 ), LDA )
               CALL DLACPY( 'Full', M, M1, A( MP1, M2+1 ), LDA,
     $                      Q2( MP1, I1 ), LDQ2 )
               CALL DLACPY( 'Full', M, M2, A( MP1, 1 ), LDA,
     $                      Q2( MP1, I2 ), LDQ2 )
            END IF
         END IF
C
C        Make permutations of the corresponding matrices.
C
         IF( M2.GT.0 ) THEN
            CALL DLASET( 'Full', M, M, ZERO, ZERO, A( MP1, 1 ), LDA )
            CALL DLACPY( 'Upper', M1, M1, DWORK( IA11 ), M, A, LDA )
            CALL DLASET( 'Full', M1, M1, ZERO, ZERO, A( 1, I1 ), LDA )
            CALL DLACPY( 'Upper', M1, M1, DWORK( IA22 ), M, A( I1, I1 ),
     $                   LDA )
            CALL DLACPY( 'Full', M1, M2, DWORK( IA11+M*M1 ), M,
     $                   A( 1, I2 ), LDA )
            CALL DLASET( 'Full', M1, M2, ZERO, ZERO, A( I1, I2 ), LDA )
            CALL DLACPY( 'Upper', M2, M2, DWORK( IA11+M*M1+M1 ), M,
     $                   A( I2, I2 ), LDA )
            CALL DLASET( 'Full', M1, M2, ZERO, ZERO, A( 1, I3 ), LDA )
            CALL DLACPY( 'Full', M1, M2, DWORK( IA22+M*M1 ), M,
     $                   A( I1, I3 ), LDA )
            CALL DLASET( 'Full', M2, M2, ZERO, ZERO, A( I2, I3 ), LDA )
            CALL DLACPY( 'Upper', M2, M2, DWORK( IA22+M*M1+M1 ), M,
     $                   A( I3, I3 ), LDA )
C
            CALL DLASET( 'Full', M1, M1, ZERO, ZERO, B, LDB )
            CALL DLACPY( 'Upper', M1, M1, DWORK( IB21 ), M, B( I1, 1 ),
     $                   LDB )
            CALL DCOPY(  M1-1, DWORK( IB21+1 ), MP1, B( I1+1, 1 ),
     $                   LDB+1 )
            IF( M1.GT.2 )
     $         CALL DLASET( 'Lower', M1-2, M1-2, ZERO, ZERO,
     $                      B( I1+2, 1 ), LDB )
            CALL DLASET( 'Full', M4, M1, ZERO, ZERO, B( I2, 1 ), LDB )
            CALL DLACPY( 'Upper', M1, M1, DWORK( IB12 ), M, B( 1, I1 ),
     $                   LDB )
            IF( M1.GT.1 )
     $         CALL DLASET( 'Lower', M1-1, M1-1, ZERO, ZERO, B( 2, I1 ),
     $                      LDB )
            CALL DLASET( 'Full', N-M1, M1, ZERO, ZERO, B( I1, I1 ),
     $                   LDB )
            CALL DLASET( 'Full', M1, M2, ZERO, ZERO, B( 1, I2 ), LDB )
            CALL DLACPY( 'Full', M1, M2, DWORK( IB21+M*M1 ), M,
     $                   B( I1, I2 ), LDB )
            CALL DLASET( 'Full', M2, M2, ZERO, ZERO, B( I2, I2 ), LDB )
            CALL DLACPY( 'Upper', M2, M2, DWORK( IB21+M*M1+M1 ), M,
     $                   B( I3, I2 ), LDB )
            CALL DCOPY( M2-1, DWORK( IB21+M*M1+I1 ), MP1, B( I3+1, I2 ),
     $                  LDB+1 )
            IF( M2.GT.2 )
     $         CALL DLASET( 'Lower', M2-2, M2-2, ZERO, ZERO,
     $                      B( I3+2, I2 ), LDB )
            CALL DLACPY( 'Full', M1, M2, DWORK( IB12+M*M1 ), M,
     $                   B( 1, I3 ), LDB )
            CALL DLASET( 'Full', M1, M2, ZERO, ZERO, B( I1, I3 ), LDB )
            CALL DLACPY( 'Full', M2, M2, DWORK( IB12+M*M1+M1 ), M,
     $                   B( I2, I3 ), LDB )
            CALL DLASET( 'Full', M2, M2, ZERO, ZERO, B( I3, I3 ), LDB )
         ELSE
            CALL DLASET( 'Full', M, M, ZERO, ZERO, A( MP1, 1 ), LDA )
            CALL DLASET( 'Full', M, M, ZERO, ZERO, A( 1, MP1 ), LDA )
            CALL DLASET( 'Full', M, M, ZERO, ZERO, B, LDB )
            CALL DLASET( 'Full', M, M, ZERO, ZERO, B( MP1, MP1 ), LDB )
         END IF
C
         IF( LINIQ1 ) THEN
            CALL DLACPY( 'Full', M, M1, DWORK( IV1 ), M, Q1, LDQ1 )
            CALL DLASET( 'Full', M, M1, ZERO, ZERO, Q1( MP1, 1 ), LDQ1 )
            CALL DLASET( 'Full', M, M1, ZERO, ZERO, Q1( 1, I1 ),  LDQ1 )
            CALL DLACPY( 'Full', M, M1, DWORK( IV3 ), M, Q1( MP1, I1 ),
     $                   LDQ1 )
            IF( M2.GT.0 ) THEN
               CALL DLACPY( 'Full', M, M2, DWORK( IV1+M*M1 ), M,
     $                      Q1( 1, I2 ), LDQ1 )
               CALL DLASET( 'Full', M, M2, ZERO, ZERO, Q1( MP1, I2 ),
     $                      LDQ1 )
               CALL DLASET( 'Full', M, M2, ZERO, ZERO, Q1( 1, I3 ),
     $                      LDQ1 )
               CALL DLACPY( 'Full', M, M2, DWORK( IV3+M*M1 ), M,
     $                      Q1( MP1, I3 ), LDQ1 )
            END IF
         END IF
C
         IF( LINIQ2 ) THEN
            CALL DLACPY( 'Full', M, M1, DWORK( IV4 ), M, Q2, LDQ2 )
            CALL DLASET( 'Full', M, M1, ZERO, ZERO, Q2( MP1, 1 ), LDQ2 )
            CALL DLASET( 'Full', M, M1, ZERO, ZERO, Q2( 1, I1 ),  LDQ2 )
            CALL DLACPY( 'Full', M, M1, DWORK( IV2 ), M, Q2( MP1, I1 ),
     $                   LDQ2 )
            IF( M2.GT.0 ) THEN
               CALL DLACPY( 'Full', M, M2, DWORK( IV4+M*M1 ), M,
     $                      Q2( 1, I2 ), LDQ2 )
               CALL DLASET( 'Full', M, M2, ZERO, ZERO, Q2( MP1, I2 ),
     $                      LDQ2 )
               CALL DLASET( 'Full', M, M2, ZERO, ZERO, Q2( 1, I3 ),
     $                      LDQ2 )
               CALL DLACPY( 'Full', M, M2, DWORK( IV2+M*M1 ), M,
     $                      Q2( MP1, I3 ), LDQ2 )
            END IF
         END IF
      ELSE
         M1 = M
         M2 = 0
         I1 = M1 + 1
         I2 = I1 + M1
         I3 = I2
         M4 = 2*M2
         CALL DLASET( 'Full', M, M, ZERO, ZERO, A( MP1, 1 ), LDA )
         CALL DLASET( 'Full', M, M, ZERO, ZERO, A( 1, MP1 ), LDA )
         CALL DLASET( 'Full', M, M, ZERO, ZERO, B, LDB )
         CALL DLASET( 'Full', M, M, ZERO, ZERO, B( MP1, MP1 ), LDB )
         IF( LINIQ1 )
     $      CALL DLASET( 'Full', N, N, ZERO, ONE, Q1, LDQ1 )
         IF( LINIQ2 )
     $      CALL DLASET( 'Full', N, N, ZERO, ONE, Q2, LDQ2 )
      END IF
C
C     Count the number of blocks in BB31.
C
      R = 0
      J = 1
C     WHILE( J.LE.M1 ) DO
   40 CONTINUE
      IF( J.LT.M1 ) THEN
         R = R + 1
         IWORK( R ) = J
         IF( B( M1+J+1, J ).EQ.ZERO ) THEN
            J = J + 1
         ELSE
            J = J + 2
         END IF
         GO TO 40
      ELSE IF ( J.EQ.M1 ) THEN
         R = R + 1
         IWORK( R ) = J
         J = J + 1
      END IF
C     END WHILE 40
      IWORK( R+1 ) = J
C
C     Triangularize the upper left subpencil aAA1 - bBB1.
C
      DO 60 I = 1, R
C
C        Calculate position of submatrices in DWORK.
C        IB1 and IB2 are pointers to 2 consecutive blocks.
C
         IB1  = IWORK( I )
         IB2  = IWORK( I+1 )
         DIM1 = IB2 - IB1
         SDIM = 2*DIM1
C
         IAUPLE = 1
         IALOLE = IAUPLE + DIM1
         IAUPRI = DIM1*SDIM + 1
         IALORI = IAUPRI + DIM1
         IBUPLE = SDIM*SDIM + 1
         IBLOLE = IBUPLE + DIM1
         IBUPRI = 3*DIM1*SDIM + 1
         IBLORI = IBUPRI + DIM1
         I1UPLE = 2*SDIM*SDIM + 1
         I1LOLE = I1UPLE + DIM1
         I1UPRI = 5*DIM1*SDIM + 1
         I1LORI = I1UPRI + DIM1
         I2UPLE = 3*SDIM*SDIM + 1
         I2LOLE = I2UPLE + DIM1
         I2UPRI = 7*DIM1*SDIM + 1
         I2LORI = I2UPRI + DIM1
C
C        Generate input matrices for MB03FD, built of submatrices of A
C        and B.
C        Workspace:   need    32.
C
         IF( DIM1.EQ.1 ) THEN
            CALL DCOPY( SDIM, A( IB1, IB1 ), ( LDA+1 )*M1,
     $                  DWORK( IAUPLE ), SDIM+1 )
            CALL DCOPY( SDIM, B( M1+IB1, IB1 ), ( LDB-1 )*M1,
     $                  DWORK( IBLOLE ), 1 )
         ELSE
            CALL DLACPY( 'Upper', DIM1, DIM1, A( IB1, IB1 ), LDA,
     $                   DWORK( IAUPLE ), SDIM )
            CALL DLASET( 'Lower', SDIM-1, SDIM-1, ZERO, ZERO,
     $                   DWORK( IAUPLE+1 ), SDIM )
            CALL DLASET( 'Full', DIM1, DIM1, ZERO, ZERO,
     $                   DWORK( IAUPRI ), SDIM )
            CALL DLACPY( 'Upper', DIM1, DIM1, A( M1+IB1, M1+IB1 ), LDA,
     $                   DWORK( IALORI ), SDIM )
            DWORK( IALORI+1 ) = ZERO
C
            CALL DLASET( 'Full', DIM1, DIM1, ZERO, ZERO,
     $                   DWORK( IBUPLE ), SDIM )
            CALL DLACPY( 'Full', DIM1, DIM1, B( M1+IB1, IB1 ), LDB,
     $                   DWORK( IBLOLE ), SDIM )
            CALL DLACPY( 'Full', DIM1, DIM1, B( IB1, M1+IB1 ), LDB,
     $                   DWORK( IBUPRI ), SDIM )
            CALL DLASET( 'Full', DIM1, DIM1, ZERO, ZERO,
     $                   DWORK( IBLORI ), SDIM )
         END IF
C
C        Perform eigenvalue exchange.
C        Workspace:   need   64 + max( 63, 4*M + 8 ).
C
         IWRK  = 4*SDIM*SDIM + 1
         ITMP  = IWRK  + M*DIM1
         ITMP2 = ITMP  + M*DIM1
         ITMP3 = ITMP2 + DIM1*DIM1
         CALL MB03FD( SDIM, ULP, DWORK( IAUPLE ), SDIM, DWORK( IBUPLE ),
     $                SDIM, DWORK( I1UPLE ), SDIM, DWORK( I2UPLE ),
     $                SDIM, DWORK( IWRK ), LDWORK-IWRK+1, INFO )
         IF( INFO.GT.0 ) THEN
            INFO = 3
            RETURN
         END IF
C
         NR = IB2 - 1
C
         IF( DIM1.EQ.2 ) THEN
C
C           Update A.
C
            CALL DLACPY( 'Full', NR, DIM1, A( 1, IB1 ), LDA,
     $                   DWORK( IWRK ), NR )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                   DIM1, ONE, DWORK( IWRK ), NR, DWORK( I1UPLE ),
     $                   SDIM, ZERO, A( 1, IB1 ), LDA )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                   DIM1, ONE, A( 1, M1+IB1 ), LDA,
     $                   DWORK( I1LOLE ), SDIM, ONE, A( 1, IB1 ), LDA )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                   DIM1, ONE, DWORK( IWRK ), NR, DWORK( I1UPRI ),
     $                   SDIM, ZERO, DWORK( ITMP ), NR )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                   DIM1, ONE, A( 1, M1+IB1 ), LDA,
     $                   DWORK( I1LORI ), SDIM, ONE, DWORK( ITMP ), NR )
            CALL DLACPY( 'Full', NR, DIM1, DWORK( ITMP ), NR,
     $                   A( 1, M1+IB1 ), LDA )
C
            CALL DLACPY( 'Full', NR, DIM1, A( I1, IB1 ), LDA,
     $                   DWORK( IWRK ), NR )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                   DIM1, ONE, DWORK( IWRK ), NR, DWORK( I1UPLE ),
     $                   SDIM, ZERO, A( I1, IB1 ), LDA )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                   DIM1, ONE, A( I1, M1+IB1 ), LDA,
     $                   DWORK( I1LOLE ), SDIM, ONE, A( I1, IB1 ), LDA )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                   DIM1, ONE, DWORK( IWRK ), NR, DWORK( I1UPRI ),
     $                   SDIM, ZERO, DWORK( ITMP ), NR )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                   DIM1, ONE, A( I1, M1+IB1 ), LDA,
     $                   DWORK( I1LORI ), SDIM, ONE, DWORK( ITMP ), NR )
            CALL DLACPY( 'Full', NR, DIM1, DWORK( ITMP ), NR,
     $                   A( I1, M1+IB1 ), LDA )
C
            CALL DLACPY( 'Full', DIM1, DIM1, A( M1+IB1, IB1 ), LDA,
     $                   DWORK( ITMP2 ), DIM1 )
            CALL DLACPY( 'Full', DIM1, DIM1, A( IB1, M1+IB1 ), LDA,
     $                   DWORK( ITMP3 ), DIM1 )
            CALL DLASET( 'Full', DIM1, DIM1, ZERO, ZERO,
     $                   A( M1+IB1, IB1 ), LDA )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB2+1,
     $                   DIM1, ONE, DWORK( I2UPRI ), SDIM,
     $                   A( IB1, IB2 ), LDA, ZERO, A( M1+IB1, IB2 ),
     $                   LDA )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                   DIM1, ONE, DWORK( I2UPLE ), SDIM,
     $                   A( IB1, IB1 ), LDA, ZERO, DWORK( ITMP ), DIM1 )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                   ONE, DWORK( I2LOLE ), SDIM, DWORK( ITMP2 ),
     $                   DIM1, ONE, DWORK( ITMP ), DIM1 )
            CALL DLACPY( 'Full', DIM1, M1-IB1+1, DWORK( ITMP ), DIM1,
     $                   A( IB1, IB1 ), LDA )
C
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                   DIM1, ONE, DWORK( I2LOLE ), SDIM,
     $                   A( M1+IB1, M1+IB1 ), LDA, ZERO,
     $                   A( IB1, M1+IB1 ), LDA )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                   ONE, DWORK( I2UPLE ), SDIM, DWORK( ITMP3 ),
     $                   DIM1, ONE, A( IB1, M1+IB1 ), LDA )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                   DIM1, ONE, DWORK( I2LORI ), SDIM,
     $                   A( M1+IB1, M1+IB1 ), LDA, ZERO, DWORK( ITMP ),
     $                   DIM1 )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                   ONE, DWORK( I2UPRI ), SDIM, DWORK( ITMP3 ),
     $                   DIM1, ONE, DWORK( ITMP ), DIM1 )
            CALL DLACPY( 'Full', DIM1, M1-IB1+1, DWORK( ITMP ), DIM1,
     $                   A( M1+IB1, M1+IB1 ), LDA )
C
            IF( M2.GT.0 ) THEN
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M2, DIM1,
     $                      ONE, DWORK( I2UPRI ), SDIM, A( IB1, I2 ),
     $                      LDA, ZERO, A( M1+IB1, I2 ), LDA )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M2, DIM1,
     $                      ONE, DWORK( I2UPLE ), SDIM, A( IB1, I2 ),
     $                      LDA, ZERO, DWORK( ITMP ), DIM1 )
               CALL DLACPY( 'Full', DIM1, M2, DWORK( ITMP ), DIM1,
     $                      A( IB1, I2 ), LDA )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M2, DIM1,
     $                      ONE, DWORK( I2LOLE ), SDIM, A( M1+IB1, I3 ),
     $                      LDA, ZERO, A( IB1, I3 ), LDA )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M2, DIM1,
     $                      ONE, DWORK( I2LORI ), SDIM, A( M1+IB1, I3 ),
     $                      LDA, ZERO, DWORK( ITMP ), DIM1 )
               CALL DLACPY( 'Full', DIM1, M2, DWORK( ITMP ), DIM1,
     $                      A( M1+IB1, I3 ), LDA )
            END IF
C
C           Update B.
C
            CALL DLACPY( 'Full', NR, DIM1, B( 1, IB1 ), LDB,
     $                   DWORK( IWRK ), NR )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                   DIM1, ONE, DWORK( IWRK ), NR, DWORK( I1UPLE ),
     $                   SDIM, ZERO, B( 1, IB1 ), LDB )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                   DIM1, ONE, B( 1, M1+IB1 ), LDB,
     $                   DWORK( I1LOLE ), SDIM, ONE, B( 1, IB1 ), LDB )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                   DIM1, ONE, DWORK( IWRK ), NR, DWORK( I1UPRI ),
     $                   SDIM, ZERO, DWORK( ITMP ), NR )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                   DIM1, ONE, B( 1, M1+IB1 ), LDB,
     $                   DWORK( I1LORI ), SDIM, ONE, DWORK( ITMP ), NR )
            CALL DLACPY( 'Full', NR, DIM1, DWORK( ITMP ), NR,
     $                   B( 1, M1+IB1 ), LDB )
C
            CALL DLACPY( 'Full', NR, DIM1, B( I1, IB1 ), LDB,
     $                   DWORK( IWRK ), NR )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                   DIM1, ONE, DWORK( IWRK ), NR, DWORK( I1UPLE ),
     $                   SDIM, ZERO, B( I1, IB1 ), LDB )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                   DIM1, ONE, B( I1, M1+IB1 ), LDB,
     $                   DWORK( I1LOLE ), SDIM, ONE, B( I1, IB1 ), LDB )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                   DIM1, ONE, DWORK( IWRK ), NR, DWORK( I1UPRI ),
     $                   SDIM, ZERO, DWORK( ITMP ), NR )
            CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                   DIM1, ONE, B( I1, M1+IB1 ), LDB,
     $                   DWORK( I1LORI ), SDIM, ONE, DWORK( ITMP ), NR )
            CALL DLACPY( 'Full', NR, DIM1, DWORK( ITMP ), NR,
     $                   B( I1, M1+IB1 ), LDB )
C
            CALL DLACPY( 'Full', DIM1, DIM1, B( IB1, IB1 ), LDB,
     $                   DWORK( ITMP2 ), DIM1 )
            CALL DLACPY( 'Full', DIM1, DIM1, B( M1+IB1, M1+IB1 ), LDB,
     $                   DWORK( ITMP3 ), DIM1 )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                   DIM1, ONE, DWORK( I2LOLE ), SDIM,
     $                   B( M1+IB1, IB1 ), LDB, ZERO, B( IB1, IB1 ), LDB
     $                   )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                   ONE, DWORK( I2UPLE ), SDIM, DWORK( ITMP2 ),
     $                   DIM1, ONE, B( IB1, IB1 ), LDB )
            CALL DLASET( 'Full', DIM1, DIM1, ZERO, ZERO,
     $                   B( M1+IB1, IB1 ), LDB )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                   DIM1, ONE, DWORK( I2LORI ), SDIM,
     $                   B( M1+IB1, IB1+1 ), LDB, ZERO, DWORK( ITMP ),
     $                   DIM1 )
            CALL DLACPY( 'Full', DIM1, M1-IB1+1, DWORK( ITMP ), DIM1,
     $                   B( M1+IB1, IB1+1 ), LDB )
C
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                   DIM1, ONE, DWORK( I2UPRI ), SDIM,
     $                   B( IB1, M1+IB1 ), LDB, ZERO,
     $                   B( M1+IB1, M1+IB1 ), LDB )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                   ONE, DWORK( I2LORI ), SDIM, DWORK( ITMP3 ),
     $                   DIM1, ONE, B( M1+IB1, M1+IB1 ), LDB )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                   DIM1, ONE, DWORK( I2UPLE ), SDIM,
     $                   B( IB1, M1+IB1 ), LDB, ZERO, DWORK( ITMP ),
     $                   DIM1 )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                   ONE, DWORK( I2LOLE ), SDIM, DWORK( ITMP3 ),
     $                   DIM1, ONE, DWORK( ITMP ), DIM1 )
            CALL DLACPY( 'Full', DIM1, M1-IB1+1, DWORK( ITMP ), DIM1,
     $                   B( IB1, M1+IB1 ), LDB )
C
            IF( M2.GT.0 ) THEN
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M2, DIM1,
     $                      ONE, DWORK( I2LOLE ), SDIM, B( M1+IB1, I2 ),
     $                      LDB, ZERO, B( IB1, I2 ), LDB )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M2, DIM1,
     $                      ONE, DWORK( I2LORI ), SDIM, B( M1+IB1, I2 ),
     $                      LDB, ZERO, DWORK( ITMP ), DIM1 )
               CALL DLACPY( 'Full', DIM1, M2, DWORK( ITMP ), DIM1,
     $                      B( M1+IB1, I2 ), LDB )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M2, DIM1,
     $                      ONE, DWORK( I2UPRI ), SDIM, B( IB1, I3 ),
     $                      LDB, ZERO, B( M1+IB1, I3 ), LDB )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M2, DIM1,
     $                      ONE, DWORK( I2UPLE ), SDIM, B( IB1, I3 ),
     $                      LDB, ZERO, DWORK( ITMP ), DIM1 )
               CALL DLACPY( 'Full', DIM1, M2, DWORK( ITMP ), DIM1,
     $                      B( IB1, I3 ), LDB )
            END IF
C
            ITMP = IWRK + N*DIM1
C
            IF( LCMPQ1 ) THEN
C
C              Update Q1.
C
               CALL DLACPY( 'Full', N, DIM1, Q1( 1, IB1 ), LDQ1,
     $                      DWORK( IWRK ), N )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), N,
     $                      DWORK( I1UPLE ), SDIM, ZERO, Q1( 1, IB1 ),
     $                      LDQ1 )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, Q1( 1, M1+IB1 ), LDQ1,
     $                      DWORK( I1LOLE ), SDIM, ONE, Q1( 1, IB1 ),
     $                      LDQ1 )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), N,
     $                      DWORK( I1UPRI ), SDIM, ZERO, DWORK( ITMP ),
     $                      N )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, Q1( 1, M1+IB1 ), LDQ1,
     $                      DWORK( I1LORI ), SDIM, ONE, DWORK( ITMP ),
     $                      N )
               CALL DLACPY( 'Full', N, DIM1, DWORK( ITMP ), N,
     $                      Q1( 1, M1+IB1 ), LDQ1 )
            END IF
C
            IF( LCMPQ2 ) THEN
C
C              Update Q2.
C
               CALL DLACPY( 'Full', N, DIM1, Q2( 1, IB1 ), LDQ2,
     $                      DWORK( IWRK ), N )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), N,
     $                      DWORK( I2UPLE ), SDIM, ZERO, Q2( 1, IB1 ),
     $                      LDQ2 )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, Q2( 1, M1+IB1 ), LDQ2,
     $                      DWORK( I2LOLE ), SDIM, ONE, Q2( 1, IB1 ),
     $                      LDQ2 )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), N,
     $                      DWORK( I2UPRI ), SDIM, ZERO, DWORK( ITMP ),
     $                      N )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, Q2( 1, M1+IB1 ), LDQ2,
     $                      DWORK( I2LORI ), SDIM, ONE, DWORK( ITMP ),
     $                      N )
               CALL DLACPY( 'Full', N, DIM1, DWORK( ITMP ), N,
     $                      Q2( 1, M1+IB1 ), LDQ2 )
            END IF
C
         ELSE
C
C           Update A.
C
            CALL DCOPY( NR, A( 1, IB1 ), 1, DWORK( IWRK ), 1 )
            CALL DSCAL( NR, DWORK( I1UPLE ), A( 1, IB1 ), 1 )
            CALL DAXPY( NR, DWORK( I1LOLE ), A( 1, M1+IB1 ), 1,
     $                  A( 1, IB1 ), 1 )
            CALL DSCAL( NR, DWORK( I1LORI ), A( 1, M1+IB1 ), 1 )
            CALL DAXPY( NR, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                  A( 1, M1+IB1 ), 1 )
C
            CALL DCOPY( NR, A( I1, IB1 ), 1, DWORK( IWRK ), 1 )
            CALL DSCAL( NR, DWORK( I1UPLE ), A( I1, IB1 ), 1 )
            CALL DAXPY( NR, DWORK( I1LOLE ), A( I1, M1+IB1 ), 1,
     $                  A( I1, IB1 ), 1 )
            CALL DSCAL( NR, DWORK( I1LORI ), A( I1, M1+IB1 ), 1 )
            CALL DAXPY( NR, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                  A( I1, M1+IB1 ), 1 )
C
            TMP2 = A( M1+IB1, IB1 )
            TMP3 = A( IB1, M1+IB1 )
            IF( M1.GT.IB1 ) THEN
               CALL DCOPY( M1-IB1, A( IB1, IB1+1 ), LDA,
     $                     A( M1+IB1, IB1+1 ), LDA )
               CALL DSCAL( M1-IB1, DWORK( I2UPRI ), A( M1+IB1, IB1+1 ),
     $                     LDA )
            END IF
            A( M1+IB1, IB1 ) = ZERO
            CALL DSCAL( M1-IB1+1, DWORK( I2UPLE ), A( IB1, IB1 ), LDA )
            A( IB1, IB1 ) = A( IB1, IB1 ) + DWORK( I2LOLE )*TMP2
C
            CALL DCOPY( M1-IB1+1, A( M1+IB1, M1+IB1 ), LDA,
     $                  A( IB1, M1+IB1 ), LDA )
            CALL DSCAL( M1-IB1+1, DWORK( I2LOLE ), A( IB1, M1+IB1 ),
     $                  LDA )
            A( IB1, M1+IB1 ) = A( IB1, M1+IB1 ) + DWORK( I2UPLE )*TMP3
            CALL DSCAL( M1-IB1+1, DWORK( I2LORI ), A( M1+IB1, M1+IB1 ),
     $                  LDA )
            A( M1+IB1, M1+IB1 ) = A( M1+IB1, M1+IB1 ) +
     $                            DWORK( I2UPRI )*TMP3
C
            IF( M2.GT.0 ) THEN
               CALL DCOPY( M2, A( IB1, I2 ), LDA, A( M1+IB1, I2 ), LDA )
               CALL DSCAL( M2, DWORK( I2UPRI ),   A( M1+IB1, I2 ), LDA )
               CALL DSCAL( M2, DWORK( I2UPLE ), A( IB1, I2 ), LDA )
               CALL DCOPY( M2, A( M1+IB1, I3 ), LDA, A( IB1, I3 ), LDA )
               CALL DSCAL( M2, DWORK( I2LOLE ), A( IB1, I3 ), LDA )
               CALL DSCAL( M2, DWORK( I2LORI ), A( M1+IB1, I3 ), LDA )
            END IF
C
C           Update B.
C
            CALL DCOPY( NR, B( 1, IB1 ), 1, DWORK( IWRK ), 1 )
            CALL DSCAL( NR, DWORK( I1UPLE ), B( 1, IB1 ), 1 )
            CALL DAXPY( NR, DWORK( I1LOLE ), B( 1, M1+IB1 ), 1,
     $                  B( 1, IB1 ), 1 )
            CALL DSCAL( NR, DWORK( I1LORI ), B( 1, M1+IB1 ), 1 )
            CALL DAXPY( NR, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                  B( 1, M1+IB1 ), 1 )
C
            CALL DCOPY( NR, B( I1, IB1 ), 1, DWORK( IWRK ), 1 )
            CALL DSCAL( NR, DWORK( I1UPLE ), B( I1, IB1 ), 1 )
            CALL DAXPY( NR, DWORK( I1LOLE ), B( I1, M1+IB1 ), 1,
     $                  B( I1, IB1 ), 1 )
            CALL DSCAL( NR, DWORK( I1LORI ), B( I1, M1+IB1 ), 1 )
            CALL DAXPY( NR, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                  B( I1, M1+IB1 ), 1 )
C
            TMP2 = B( IB1, IB1 )
            TMP3 = B( M1+IB1, M1+IB1 )
            CALL DCOPY( M1-IB1+1, B( M1+IB1, IB1 ), LDB, B( IB1, IB1 ),
     $                  LDB )
            CALL DSCAL( M1-IB1+1, DWORK( I2LOLE ), B( IB1, IB1 ), LDB )
            B( IB1, IB1 ) = B( IB1, IB1 ) + DWORK( I2UPLE )*TMP2
            B( M1+IB1, IB1 ) = ZERO
            CALL DSCAL( M1-IB1+1, DWORK( I2LORI ), B( M1+IB1, IB1+1 ),
     $                  LDB )
C
            CALL DCOPY( M1-IB1+1, B( IB1, M1+IB1 ), LDB,
     $                  B( M1+IB1, M1+IB1 ), LDB )
            CALL DSCAL( M1-IB1+1, DWORK( I2UPRI ), B( M1+IB1, M1+IB1 ),
     $                  LDB )
            B( M1+IB1, M1+IB1 ) = B( M1+IB1, M1+IB1 ) +
     $                            DWORK( I2LORI )*TMP3
            CALL DSCAL( M1-IB1+1, DWORK( I2UPLE ), B( IB1, M1+IB1 ),
     $                  LDB )
            B( IB1, M1+IB1 ) = B( IB1, M1+IB1 ) + DWORK( I2LOLE )*TMP3
C
            IF( M2.GT.0 ) THEN
               CALL DCOPY( M2, B( M1+IB1, I2 ), LDB, B( IB1, I2 ), LDB )
               CALL DSCAL( M2, DWORK( I2LOLE ), B( IB1, I2 ), LDB )
               CALL DSCAL( M2, DWORK( I2LORI ), B( M1+IB1, I2 ),  LDB )
               CALL DCOPY( M2, B( IB1, I3 ), LDB, B( M1+IB1, I3 ), LDB )
               CALL DSCAL( M2, DWORK( I2UPRI ), B( M1+IB1, I3 ), LDB )
               CALL DSCAL( M2, DWORK( I2UPLE ), B( IB1, I3 ),   LDB )
            END IF
C
            ITMP = IWRK + N
C
            IF( LCMPQ1 ) THEN
C
C              Update Q1.
C
               CALL DCOPY( N, Q1( 1, IB1 ), 1, DWORK( IWRK ), 1 )
               CALL DSCAL( N, DWORK( I1UPLE ), Q1( 1, IB1 ),  1 )
               CALL DAXPY( N, DWORK( I1LOLE ), Q1( 1, M1+IB1 ), 1,
     $                     Q1( 1, IB1 ), 1 )
               CALL DSCAL( N, DWORK( I1LORI ), Q1( 1, M1+IB1 ), 1 )
               CALL DAXPY( N, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                     Q1( 1, M1+IB1 ), 1 )
            END IF
C
            IF( LCMPQ2 ) THEN
C
C              Update Q2.
C
               CALL DCOPY( N, Q2( 1, IB1 ), 1, DWORK( IWRK ), 1 )
               CALL DSCAL( N, DWORK( I2UPLE ), Q2( 1, IB1 ),  1 )
               CALL DAXPY( N, DWORK( I2LOLE ), Q2( 1, M1+IB1 ), 1,
     $                     Q2( 1, IB1 ), 1 )
               CALL DSCAL( N, DWORK( I2LORI ), Q2( 1, M1+IB1 ), 1 )
               CALL DAXPY( N, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                     Q2( 1, M1+IB1 ), 1 )
            END IF
C
         END IF
C
         DO 50 J = I - 1, 1, -1
C
C           Calculate position of submatrices in DWORK.
C
            IJ1  = IWORK( J )
            IJ2  = IWORK( J+1 )
            DIM1 = IWORK( I+1 ) - IWORK( I )
            DIM2 = IJ2 - IJ1
            SDIM = DIM1 + DIM2
C
            IALOLE = IAUPLE + DIM1
            IAUPRI = DIM1*SDIM + 1
            IALORI = IAUPRI + DIM1
            IBUPLE = SDIM*SDIM + 1
            IBLOLE = IBUPLE + DIM1
            IBUPRI = SDIM*SDIM + DIM1*SDIM + 1
            IBLORI = IBUPRI + DIM1
            I1UPLE = 2*SDIM*SDIM + 1
            I1LOLE = I1UPLE + DIM1
            I1UPRI = 2*SDIM*SDIM + DIM1*SDIM + 1
            I1LORI = I1UPRI + DIM1
            I2UPLE = 3*SDIM*SDIM + 1
            I2LOLE = I2UPLE + DIM1
            I2UPRI = 3*SDIM*SDIM + DIM1*SDIM + 1
            I2LORI = I2UPRI + DIM1
C
C           Generate input matrices for MB03DD built of submatrices of A
C           and B.
C           Workspace:   need   32.
C
            IF( DIM1.EQ.2 .AND. DIM2.EQ.2 ) THEN
               CALL DLACPY( 'Full', DIM1, DIM1, A( IB1, IB1 ), LDA,
     $                      DWORK( IAUPLE ), SDIM )
               CALL DLACPY( 'Full', DIM2, DIM1, A( M1+IJ1, IB1 ), LDA,
     $                      DWORK( IALOLE ), SDIM )
               CALL DLASET( 'Full', DIM1, DIM2, ZERO, ZERO,
     $                      DWORK( IAUPRI ), SDIM )
               CALL DLACPY( 'Full', DIM2, DIM2, A( M1+IJ1, M1+IJ1 ),
     $                      LDA, DWORK( IALORI ), SDIM )
C
               CALL DLACPY( 'Full', DIM1, DIM1, B( IB1, IB1 ), LDB,
     $                      DWORK( IBUPLE ), SDIM )
               CALL DLACPY( 'Full', DIM2, DIM1, B( M1+IJ1, IB1 ), LDB,
     $                      DWORK( IBLOLE ), SDIM )
               CALL DLASET( 'Full', DIM1, DIM2, ZERO, ZERO,
     $                      DWORK( IBUPRI ), SDIM )
               CALL DLACPY( 'Full', DIM2, DIM2, B( M1+IJ1, M1+IJ1 ),
     $                      LDB, DWORK( IBLORI ), SDIM )
C
            ELSE IF( DIM1.EQ.1 .AND. DIM2.EQ.2 ) THEN
               DWORK( IAUPLE ) = A( IB1, IB1 )
               CALL DCOPY( DIM2, A( M1+IJ1, IB1 ), 1, DWORK( IALOLE ),
     $                     1 )
               CALL DCOPY( DIM2, DUM, 0, DWORK( IAUPRI ), SDIM )
               CALL DLACPY( 'Full', DIM2, DIM2, A( M1+IJ1, M1+IJ1 ),
     $                      LDA, DWORK( IALORI ), SDIM )
C
               DWORK( IBUPLE ) = B( IB1, IB1 )
               CALL DCOPY( DIM2, B( M1+IJ1, IB1 ), 1, DWORK( IBLOLE ),
     $                     1 )
               CALL DCOPY( DIM2, DUM, 0, DWORK( IBUPRI ), SDIM )
               CALL DLACPY( 'Full', DIM2, DIM2, B( M1+IJ1, M1+IJ1 ),
     $                      LDB, DWORK( IBLORI ), SDIM )
C
            ELSE IF( DIM1.EQ.2 .AND. DIM2.EQ.1 ) THEN
               CALL DLACPY( 'Full', DIM1, DIM1, A( IB1, IB1 ), LDA,
     $                      DWORK( IAUPLE ), SDIM )
               CALL DCOPY( DIM1, A( M1+IJ1, IB1 ), LDA, DWORK( IALOLE ),
     $                     SDIM )
               CALL DCOPY( DIM1, DUM, 0, DWORK( IAUPRI ), 1 )
               DWORK( IALORI ) = A( M1+IJ1, M1+IJ1 )
C
               CALL DLACPY( 'Full', DIM1, DIM1, B( IB1, IB1 ), LDB,
     $                      DWORK( IBUPLE ), SDIM )
               CALL DCOPY( DIM1, B( M1+IJ1, IB1 ), LDB, DWORK( IBLOLE ),
     $                     SDIM )
               CALL DCOPY( DIM1, DUM, 0, DWORK( IBUPRI ), 1 )
               DWORK( IBLORI ) = B( M1+IJ1, M1+IJ1 )
C
            ELSE
               DWORK( IAUPLE ) = A( IB1, IB1 )
               DWORK( IALOLE ) = A( M1+IJ1, IB1 )
               DWORK( IAUPRI ) = ZERO
               DWORK( IALORI ) = A( M1+IJ1, M1+IJ1 )
C
               DWORK( IBUPLE ) = B( IB1, IB1 )
               DWORK( IBLOLE ) = B( M1+IJ1, IB1 )
               DWORK( IBUPRI ) = ZERO
               DWORK( IBLORI ) = B( M1+IJ1, M1+IJ1 )
C
            END IF
C
C           Perform upper triangularization.
C           Workspace:   need   64 + max( 75, 4*N ).
C
            IWRK = 4*SDIM*SDIM + 1
            ITMP = IWRK + 2*N
            CALL MB03DD( 'Lower', DIM1, DIM2, ULP, DWORK( IBUPLE ),
     $                   SDIM, DWORK( IAUPLE ), SDIM, DWORK( I1UPLE ),
     $                   SDIM, DWORK( I2UPLE ), SDIM, DWORK( IWRK ),
     $                   LDWORK-IWRK+1, INFO )
            IF( INFO.GT.0 ) THEN
               IF( INFO.LE.2 ) THEN
                  INFO = 2
               ELSE IF( INFO.LE.4 ) THEN
                  INFO = 3
               ELSE
                  INFO = 4
               END IF
               RETURN
            END IF
C
            NROW = IJ2 - 1
C
            IF( DIM1.EQ.2 .AND. DIM2.EQ.2 ) THEN
C
C              Update A.
C
               CALL DLACPY( 'Full', NR, DIM1, A( 1, IB1 ), LDA,
     $                      DWORK( IWRK ), NR )
               CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), NR,
     $                      DWORK( I1UPLE ), SDIM, ZERO, A( 1, IB1 ),
     $                      LDA )
               CALL DGEMM(  'No Transpose', 'No Transpose', NR-DIM1,
     $                      DIM1, DIM2, ONE, A( 1, M1+IJ1 ), LDA,
     $                      DWORK( I1LOLE ), SDIM, ONE, A( 1, IB1 ),
     $                      LDA )
               CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM2,
     $                      DIM1, ONE, DWORK( IWRK ), NR,
     $                      DWORK( I1UPRI ), SDIM, ZERO, DWORK( ITMP ),
     $                      NR )
               CALL DGEMM(  'No Transpose', 'No Transpose', NR-DIM1,
     $                      DIM2, DIM2, ONE, A( 1, M1+IJ1 ), LDA,
     $                      DWORK( I1LORI ), SDIM, ONE, DWORK( ITMP ),
     $                      NR )
               CALL DLACPY( 'Full', NR, DIM2, DWORK( ITMP ), NR,
     $                      A( 1, M1+IJ1 ), LDA )
C
               CALL DLACPY( 'Full', NROW, DIM1, A( I1, IB1 ), LDA,
     $                      DWORK( IWRK ), NROW )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), NROW,
     $                      DWORK( I1UPLE ), SDIM, ZERO, A( I1, IB1 ),
     $                      LDA )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM1,
     $                      DIM2, ONE, A( I1, M1+IJ1 ), LDA,
     $                      DWORK( I1LOLE ), SDIM, ONE, A( I1, IB1 ),
     $                      LDA )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM2,
     $                      DIM1, ONE, DWORK( IWRK ), NROW,
     $                      DWORK( I1UPRI ), SDIM, ZERO, DWORK( ITMP ),
     $                      NROW )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM2,
     $                      DIM2, ONE, A( I1, M1+IJ1 ), LDA,
     $                      DWORK( I1LORI ), SDIM, ONE, DWORK( ITMP ),
     $                      NROW )
               CALL DLACPY( 'Full', NROW, DIM2, DWORK( ITMP ), NROW,
     $                      A( I1, M1+IJ1 ), LDA )
C
               CALL DLACPY( 'Full', DIM1, M1-IB1+1, A( IB1, IB1 ), LDA,
     $                      DWORK( IWRK ), DIM1 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                      DIM1, ONE, DWORK( I2UPLE ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, A( IB1, IB1 ),
     $                      LDA )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                      DIM2, ONE, DWORK( I2LOLE ), SDIM,
     $                      A( M1+IJ1, IB1 ), LDA, ONE, A( IB1, IB1 ),
     $                      LDA )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IB1+1,
     $                      DIM1, ONE, DWORK( I2UPRI ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, DWORK( ITMP ),
     $                      DIM2 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IB1+1,
     $                      DIM2, ONE, DWORK( I2LORI ), SDIM,
     $                      A( M1+IJ1, IB1 ), LDA, ONE, DWORK( ITMP ),
     $                      DIM2 )
               CALL DLACPY( 'Full', DIM2, M1-IB1+1, DWORK( ITMP ), DIM2,
     $                      A( M1+IJ1, IB1 ), LDA )
C
               CALL DLACPY( 'Full', DIM1, M1-IJ1+1, A( IB1, M1+IJ1 ),
     $                      LDA, DWORK( IWRK ), DIM1 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IJ1+1,
     $                      DIM1, ONE, DWORK( I2UPLE ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, A( IB1, M1+IJ1 ),
     $                      LDA )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IJ1+1,
     $                      DIM2, ONE, DWORK( I2LOLE ), SDIM,
     $                      A( M1+IJ1, M1+IJ1 ), LDA, ONE,
     $                      A( IB1, M1+IJ1 ), LDA )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IJ1+1,
     $                      DIM1, ONE, DWORK( I2UPRI ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, DWORK( ITMP ),
     $                      DIM2 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IJ1+1,
     $                      DIM2, ONE, DWORK( I2LORI ), SDIM,
     $                      A( M1+IJ1, M1+IJ1 ), LDA, ONE,
     $                      DWORK( ITMP ), DIM2 )
               CALL DLACPY( 'Full', DIM2, M1-IJ1+1, DWORK( ITMP ), DIM2,
     $                      A( M1+IJ1, M1+IJ1 ), LDA )
C
               IF( M2.GT.0 ) THEN
                  CALL DLACPY( 'Full', DIM1, M4, A( IB1, I2 ), LDA,
     $                         DWORK( IWRK ), DIM1 )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M4,
     $                         DIM1, ONE, DWORK( I2UPLE ), SDIM,
     $                         DWORK( IWRK ), DIM1, ZERO, A( IB1, I2 ),
     $                         LDA )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M4,
     $                         DIM2, ONE, DWORK( I2LOLE ), SDIM,
     $                         A( M1+IJ1, I2 ), LDA, ONE, A( IB1, I2 ),
     $                         LDA )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M4,
     $                         DIM1, ONE, DWORK( I2UPRI ), SDIM,
     $                         DWORK( IWRK ), DIM1, ZERO, DWORK( ITMP ),
     $                         DIM2 )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M4,
     $                         DIM2, ONE, DWORK( I2LORI ), SDIM,
     $                         A( M1+IJ1, I2 ), LDA, ONE, DWORK( ITMP ),
     $                         DIM2 )
                  CALL DLACPY( 'Full', DIM2, M4, DWORK( ITMP ), DIM2,
     $                         A( M1+IJ1, I2 ), LDA )
               END IF
C
C              Update B.
C
               CALL DLACPY( 'Full', NR, DIM1, B( 1, IB1 ), LDB,
     $                      DWORK( IWRK ), NR )
               CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), NR,
     $                      DWORK( I1UPLE ), SDIM, ZERO, B( 1, IB1 ),
     $                      LDB )
               CALL DGEMM(  'No Transpose', 'No Transpose', NR-DIM1,
     $                      DIM1, DIM2, ONE, B( 1, M1+IJ1 ), LDB,
     $                      DWORK( I1LOLE ), SDIM, ONE, B( 1, IB1 ),
     $                      LDB )
               CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM2,
     $                      DIM1, ONE, DWORK( IWRK ), NR,
     $                      DWORK( I1UPRI ), SDIM, ZERO, DWORK( ITMP ),
     $                      NR )
               CALL DGEMM(  'No Transpose', 'No Transpose', NR-DIM1,
     $                      DIM2, DIM2, ONE, B( 1, M1+IJ1 ), LDB,
     $                      DWORK( I1LORI ), SDIM, ONE, DWORK( ITMP ),
     $                      NR )
               CALL DLACPY( 'Full', NR, DIM2, DWORK( ITMP ), NR,
     $                      B( 1, M1+IJ1 ), LDB )
C
               CALL DLACPY( 'Full', NROW, DIM1, B( I1, IB1 ), LDB,
     $                      DWORK( IWRK ), NROW )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), NROW,
     $                      DWORK( I1UPLE ), SDIM, ZERO, B( I1, IB1 ),
     $                      LDB )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM1,
     $                      DIM2, ONE, B( I1, M1+IJ1 ), LDB,
     $                      DWORK( I1LOLE ), SDIM, ONE, B( I1, IB1 ),
     $                      LDB )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM2,
     $                      DIM1, ONE, DWORK( IWRK ), NROW,
     $                      DWORK( I1UPRI ), SDIM, ZERO, DWORK( ITMP ),
     $                      NROW )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM2,
     $                      DIM2, ONE, B( I1, M1+IJ1 ), LDB,
     $                      DWORK( I1LORI ), SDIM, ONE, DWORK( ITMP ),
     $                      NROW )
               CALL DLACPY( 'Full', NROW, DIM2, DWORK( ITMP ), NROW,
     $                      B( I1, M1+IJ1 ), LDB )
C
               CALL DLACPY( 'Full', DIM1, M1-IB1+1, B( IB1, IB1 ), LDB,
     $                      DWORK( IWRK ), DIM1 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                      DIM1, ONE, DWORK( I2UPLE ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, B( IB1, IB1 ),
     $                      LDB )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                      DIM2, ONE, DWORK( I2LOLE ), SDIM,
     $                      B( M1+IJ1, IB1 ), LDB, ONE, B( IB1, IB1 ),
     $                      LDB )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IB1+1,
     $                      DIM1, ONE, DWORK( I2UPRI ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, DWORK( ITMP ),
     $                      DIM2 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IB1+1,
     $                      DIM2, ONE, DWORK( I2LORI ), SDIM,
     $                      B( M1+IJ1, IB1 ), LDB, ONE, DWORK( ITMP ),
     $                      DIM2 )
               CALL DLACPY( 'Full', DIM2, M1-IB1+1, DWORK( ITMP ), DIM2,
     $                      B( M1+IJ1, IB1 ), LDB )
C
               CALL DLACPY( 'Full', DIM1, M1-IJ1+1, B( IB1, M1+IJ1 ),
     $                      LDB, DWORK( IWRK ), DIM1 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IJ1+1,
     $                      DIM1, ONE, DWORK( I2UPLE ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, B( IB1, M1+IJ1 ),
     $                      LDB )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IJ1+1,
     $                      DIM2, ONE, DWORK( I2LOLE ), SDIM,
     $                      B( M1+IJ1, M1+IJ1 ), LDB, ONE,
     $                      B( IB1, M1+IJ1 ), LDB )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IJ1+1,
     $                      DIM1, ONE, DWORK( I2UPRI ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, DWORK( ITMP ),
     $                      DIM2 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IJ1+1,
     $                      DIM2, ONE, DWORK( I2LORI ), SDIM,
     $                      B( M1+IJ1, M1+IJ1 ), LDB, ONE,
     $                      DWORK( ITMP ), DIM2 )
               CALL DLACPY( 'Full', DIM2, M1-IJ1+1, DWORK( ITMP ), DIM2,
     $                      B( M1+IJ1, M1+IJ1 ), LDB )
C
               IF( M2.GT.0 ) THEN
                  CALL DLACPY( 'Full', DIM1, M4, B( IB1, I2 ), LDB,
     $                         DWORK( IWRK ), DIM1 )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M4,
     $                         DIM1, ONE, DWORK( I2UPLE ), SDIM,
     $                         DWORK( IWRK ), DIM1, ZERO, B( IB1, I2 ),
     $                         LDB )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M4,
     $                         DIM2, ONE, DWORK( I2LOLE ), SDIM,
     $                         B( M1+IJ1, I2 ), LDB, ONE, B( IB1, I2 ),
     $                         LDB )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M4,
     $                         DIM1, ONE, DWORK( I2UPRI ), SDIM,
     $                         DWORK( IWRK ), DIM1, ZERO, DWORK( ITMP ),
     $                         DIM2 )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M4,
     $                         DIM2, ONE, DWORK( I2LORI ), SDIM,
     $                         B( M1+IJ1, I2 ), LDB, ONE, DWORK( ITMP ),
     $                         DIM2 )
                  CALL DLACPY( 'Full', DIM2, M4, DWORK( ITMP ), DIM2,
     $                         B( M1+IJ1, I2 ), LDB )
               END IF
C
C              Update Q1.
C
               IF( LCMPQ1 ) THEN
                  CALL DLACPY( 'Full', N, DIM1, Q1( 1, IB1 ), LDQ1,
     $                         DWORK( IWRK ), N )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                         DIM1, ONE, DWORK( IWRK ), N,
     $                         DWORK( I1UPLE ), SDIM, ZERO,
     $                         Q1( 1, IB1 ), LDQ1 )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                         DIM2, ONE, Q1( 1, M1+IJ1 ), LDQ1,
     $                         DWORK( I1LOLE ), SDIM, ONE, Q1( 1, IB1 ),
     $                         LDQ1 )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM2,
     $                         DIM1, ONE, DWORK( IWRK ), N,
     $                         DWORK( I1UPRI ), SDIM, ZERO,
     $                         DWORK( ITMP ), N )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM2,
     $                         DIM2, ONE, Q1( 1, M1+IJ1 ), LDQ1,
     $                         DWORK( I1LORI ), SDIM, ONE,
     $                         DWORK( ITMP ), N )
                  CALL DLACPY( 'Full', N, DIM2, DWORK( ITMP ), N,
     $                         Q1( 1, M1+IJ1 ), LDQ1 )
               END IF
C
C              Update Q2.
C
               IF( LCMPQ2 ) THEN
                  CALL DLACPY( 'Full', N, DIM1, Q2( 1, IB1 ), LDQ2,
     $                         DWORK( IWRK ), N )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                         DIM1, ONE, DWORK( IWRK ), N,
     $                         DWORK( I2UPLE ), SDIM, ZERO,
     $                         Q2( 1, IB1 ), LDQ2 )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                         DIM2, ONE, Q2( 1, M1+IJ1 ), LDQ2,
     $                         DWORK( I2LOLE ), SDIM, ONE, Q2( 1, IB1 ),
     $                         LDQ2 )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM2,
     $                         DIM1, ONE, DWORK( IWRK ), N,
     $                         DWORK( I2UPRI ), SDIM, ZERO,
     $                         DWORK( ITMP ), N )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM2,
     $                         DIM2, ONE, Q2( 1, M1+IJ1 ), LDQ2,
     $                         DWORK( I2LORI ), SDIM, ONE,
     $                         DWORK( ITMP ), N )
                  CALL DLACPY( 'Full', N, DIM2, DWORK( ITMP ), N,
     $                         Q2( 1, M1+IJ1 ), LDQ2 )
               END IF
C
            ELSE IF( DIM1.EQ.1 .AND. DIM2.EQ.2 ) THEN
C
C              Update A.
C
               CALL DCOPY( NR, A( 1, IB1 ), 1, DWORK( IWRK ), 1 )
               CALL DGEMV( 'No Transpose', NR-1, DIM2, ONE,
     $                     A( 1, M1+IJ1 ), LDA, DWORK( I1LOLE ), 1,
     $                     DWORK( I1UPLE ), A( 1, IB1 ), 1 )
               A( NR, IB1 ) = DWORK( I1UPLE )*A( NR, IB1 )
               CALL DGEMM( 'No Transpose', 'No Transpose', NR-1, DIM2,
     $                     DIM2, ONE, A( 1, M1+IJ1 ), LDA,
     $                     DWORK( I1LORI ), SDIM, ZERO, DWORK( ITMP ),
     $                     NR )
               DWORK( ITMP+  NR-1 ) = ZERO
               DWORK( ITMP+2*NR-1 ) = ZERO
               CALL DAXPY(  NR, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                      DWORK( ITMP ), 1 )
               CALL DAXPY(  NR, DWORK( I1UPRI+SDIM ), DWORK( IWRK ), 1,
     $                      DWORK( ITMP+NR ), 1 )
               CALL DLACPY( 'Full', NR, DIM2, DWORK( ITMP ), NR,
     $                      A( 1, M1+IJ1 ), LDA )
C
               CALL DCOPY(  NROW, A( I1, IB1 ), 1, DWORK( IWRK ), 1 )
               CALL DGEMV(  'No Transpose', NROW, DIM2, ONE,
     $                      A( I1, M1+IJ1 ), LDA, DWORK( I1LOLE ), 1,
     $                      DWORK( I1UPLE ), A( I1, IB1 ), 1 )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM2,
     $                      DIM2, ONE, A( I1, M1+IJ1 ), LDA,
     $                      DWORK( I1LORI ), SDIM, ZERO, DWORK( ITMP ),
     $                      NROW )
               CALL DAXPY(  NROW, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                      DWORK( ITMP ), 1 )
               CALL DAXPY(  NROW, DWORK( I1UPRI+SDIM ), DWORK( IWRK ),
     $                      1, DWORK( ITMP+NROW ), 1 )
               CALL DLACPY( 'Full', NROW, DIM2, DWORK( ITMP ), NROW,
     $                      A( I1, M1+IJ1 ), LDA )
C
               CALL DCOPY(  M1-IB1+1, A( IB1, IB1 ), LDA, DWORK( IWRK ),
     $                      1 )
               CALL DGEMV(  'Transpose', DIM2, M1-IB1+1, ONE,
     $                      A( M1+IJ1, IB1 ), LDA, DWORK( I2LOLE ), 1,
     $                      DWORK( I2UPLE ), A( IB1, IB1 ), LDA )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IB1+1,
     $                      DIM2, ONE, DWORK( I2LORI ), SDIM,
     $                      A( M1+IJ1, IB1 ), LDA, ZERO, DWORK( ITMP ),
     $                      DIM2 )
               CALL DAXPY(  M1-IB1+1, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                      DWORK( ITMP ), DIM2 )
               CALL DAXPY(  M1-IB1+1, DWORK( I2UPRI+SDIM ),
     $                      DWORK( IWRK ), 1, DWORK( ITMP+1 ), DIM2 )
               CALL DLACPY( 'Full', DIM2, M1-IB1+1, DWORK( ITMP ), DIM2,
     $                      A( M1+IJ1, IB1 ), LDA )
C
               CALL DCOPY(  M1-IJ1+1, A( IB1, M1+IJ1 ), LDA,
     $                      DWORK( IWRK ), 1 )
               CALL DGEMV(  'Transpose', DIM2, M1-IJ1+1, ONE,
     $                      A( M1+IJ1, M1+IJ1 ), LDA, DWORK( I2LOLE ),
     $                      1, DWORK( I2UPLE ), A( IB1, M1+IJ1 ), LDA )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IJ1+1,
     $                      DIM2, ONE, DWORK( I2LORI ), SDIM,
     $                      A( M1+IJ1, M1+IJ1 ), LDA, ZERO,
     $                      DWORK( ITMP ), DIM2 )
               CALL DAXPY(  M1-IJ1+1, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                      DWORK( ITMP ), DIM2 )
               CALL DAXPY(  M1-IJ1+1, DWORK( I2UPRI+SDIM ),
     $                      DWORK( IWRK ), 1, DWORK( ITMP+1 ), DIM2 )
               CALL DLACPY( 'Full', DIM2, M1-IJ1+1, DWORK( ITMP ), DIM2,
     $                      A( M1+IJ1, M1+IJ1 ), LDA )
C
               IF( M2.GT.0 ) THEN
                  CALL DCOPY(  M4, A( IB1, I2 ), LDA, DWORK( IWRK ), 1 )
                  CALL DGEMV(  'Transpose', DIM2, M4, ONE,
     $                         A( M1+IJ1, I2 ), LDA, DWORK( I2LOLE ), 1,
     $                         DWORK( I2UPLE ), A( IB1, I2 ), LDA )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M4,
     $                         DIM2, ONE, DWORK( I2LORI ), SDIM,
     $                         A( M1+IJ1, I2 ), LDA, ZERO,
     $                         DWORK( ITMP ), DIM2 )
                  CALL DAXPY(  M4, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                         DWORK( ITMP ), DIM2 )
                  CALL DAXPY(  M4, DWORK( I2UPRI+SDIM ), DWORK( IWRK ),
     $                         1, DWORK( ITMP+1 ), DIM2 )
                  CALL DLACPY( 'Full', DIM2, M4, DWORK( ITMP ), DIM2,
     $                         A( M1+IJ1, I2 ), LDA )
               END IF
C
C              Update B.
C
               CALL DCOPY( NR, B( 1, IB1 ), 1, DWORK( IWRK ), 1 )
               CALL DGEMV( 'No Transpose', NR-1, DIM2, ONE,
     $                     B( 1, M1+IJ1 ), LDB, DWORK( I1LOLE ), 1,
     $                     DWORK( I1UPLE ), B( 1, IB1 ), 1 )
               B( NR, IB1 ) = DWORK( I1UPLE )*B( NR, IB1 )
               CALL DGEMM( 'No Transpose', 'No Transpose', NR-1, DIM2,
     $                     DIM2, ONE, B( 1, M1+IJ1 ), LDB,
     $                     DWORK( I1LORI ), SDIM, ZERO, DWORK( ITMP ),
     $                     NR )
               DWORK( ITMP+  NR-1 ) = ZERO
               DWORK( ITMP+2*NR-1 ) = ZERO
               CALL DAXPY(  NR, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                      DWORK( ITMP ), 1 )
               CALL DAXPY(  NR, DWORK( I1UPRI+SDIM ), DWORK( IWRK ), 1,
     $                      DWORK( ITMP+NR ), 1 )
               CALL DLACPY( 'Full', NR, DIM2, DWORK( ITMP ), NR,
     $                      B( 1, M1+IJ1 ), LDB )
C
               CALL DCOPY(  NROW, B( I1, IB1 ), 1, DWORK( IWRK ), 1 )
               CALL DGEMV(  'No Transpose', NROW, DIM2, ONE,
     $                      B( I1, M1+IJ1 ), LDB, DWORK( I1LOLE ), 1,
     $                      DWORK( I1UPLE ), B( I1, IB1 ), 1 )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM2,
     $                      DIM2, ONE, B( I1, M1+IJ1 ), LDB,
     $                      DWORK( I1LORI ), SDIM, ZERO, DWORK( ITMP ),
     $                      NROW )
               CALL DAXPY(  NROW, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                      DWORK( ITMP ), 1 )
               CALL DAXPY(  NROW, DWORK( I1UPRI+SDIM ), DWORK( IWRK ),
     $                      1, DWORK( ITMP+NROW ), 1 )
               CALL DLACPY( 'Full', NROW, DIM2, DWORK( ITMP ), NROW,
     $                      B( I1, M1+IJ1 ), LDB )
C
               CALL DCOPY(  M1-IB1+1, B( IB1, IB1 ), LDB, DWORK( IWRK ),
     $                      1 )
               CALL DGEMV(  'Transpose', DIM2, M1-IB1+1, ONE,
     $                      B( M1+IJ1, IB1 ), LDB, DWORK( I2LOLE ), 1,
     $                      DWORK( I2UPLE ), B( IB1, IB1 ), LDB )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IB1+1,
     $                      DIM2, ONE, DWORK( I2LORI ), SDIM,
     $                      B( M1+IJ1, IB1 ), LDB, ZERO, DWORK( ITMP ),
     $                      DIM2 )
               CALL DAXPY(  M1-IB1+1, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                      DWORK( ITMP ), DIM2 )
               CALL DAXPY(  M1-IB1+1, DWORK( I2UPRI+SDIM ),
     $                      DWORK( IWRK ), 1, DWORK( ITMP+1 ), DIM2 )
               CALL DLACPY( 'Full', DIM2, M1-IB1+1, DWORK( ITMP ), DIM2,
     $                      B( M1+IJ1, IB1 ), LDB )
C
               CALL DCOPY(  M1-IJ1+1, B( IB1, M1+IJ1 ), LDB,
     $                      DWORK( IWRK ), 1 )
               CALL DGEMV(  'Transpose', DIM2, M1-IJ1+1, ONE,
     $                      B( M1+IJ1, M1+IJ1 ), LDB, DWORK( I2LOLE ),
     $                      1, DWORK( I2UPLE ), B( IB1, M1+IJ1 ), LDB )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M1-IJ1+1,
     $                      DIM2, ONE, DWORK( I2LORI ), SDIM,
     $                      B( M1+IJ1, M1+IJ1 ), LDB, ZERO,
     $                      DWORK( ITMP ), DIM2 )
               CALL DAXPY(  M1-IJ1+1, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                      DWORK( ITMP ), DIM2 )
               CALL DAXPY(  M1-IJ1+1, DWORK( I2UPRI+SDIM ),
     $                      DWORK( IWRK ), 1, DWORK( ITMP+1 ), DIM2 )
               CALL DLACPY( 'Full', DIM2, M1-IJ1+1, DWORK( ITMP ), DIM2,
     $                      B( M1+IJ1, M1+IJ1 ), LDB )
C
               IF( M2.GT.0 ) THEN
                  CALL DCOPY(  M4, B( IB1, I2 ), LDB, DWORK( IWRK ), 1 )
                  CALL DGEMV(  'Transpose', DIM2, M4, ONE,
     $                         B( M1+IJ1, I2 ), LDB, DWORK( I2LOLE ), 1,
     $                         DWORK( I2UPLE ), B( IB1, I2 ), LDB )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM2, M4,
     $                         DIM2, ONE, DWORK( I2LORI ), SDIM,
     $                         B( M1+IJ1, I2 ), LDB, ZERO,
     $                         DWORK( ITMP ), DIM2 )
                  CALL DAXPY(  M4, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                         DWORK( ITMP ), DIM2 )
                  CALL DAXPY(  M4, DWORK( I2UPRI+SDIM ), DWORK( IWRK ),
     $                         1, DWORK( ITMP+1 ), DIM2 )
                  CALL DLACPY( 'Full', DIM2, M4, DWORK( ITMP ), DIM2,
     $                         B( M1+IJ1, I2 ), LDB )
               END IF
C
C              Update Q1.
C
               IF( LCMPQ1 ) THEN
                  CALL DCOPY(  N, Q1( 1, IB1 ), 1, DWORK( IWRK ), 1 )
                  CALL DGEMV(  'No Transpose', N, DIM2, ONE,
     $                         Q1( 1, M1+IJ1 ), LDQ1, DWORK( I1LOLE ),
     $                         1, DWORK( I1UPLE ), Q1( 1, IB1 ), 1 )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM2,
     $                         DIM2, ONE, Q1( 1, M1+IJ1 ), LDQ1,
     $                         DWORK( I1LORI ), SDIM, ZERO,
     $                         DWORK( ITMP ), N )
                  CALL DAXPY(  N, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                         DWORK( ITMP ), 1 )
                  CALL DAXPY(  N, DWORK( I1UPRI+SDIM ), DWORK( IWRK ),
     $                         1, DWORK( ITMP+N ), 1 )
                  CALL DLACPY( 'Full', N, DIM2, DWORK( ITMP ), N,
     $                         Q1( 1, M1+IJ1 ), LDQ1 )
               END IF
C
C              Update Q2.
C
               IF( LCMPQ2 ) THEN
                  CALL DCOPY(  N, Q2( 1, IB1 ), 1, DWORK( IWRK ), 1 )
                  CALL DGEMV(  'No Transpose', N, DIM2, ONE,
     $                         Q2( 1, M1+IJ1 ), LDQ2, DWORK( I2LOLE ),
     $                         1, DWORK( I2UPLE ), Q2( 1, IB1 ), 1 )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM2,
     $                         DIM2, ONE, Q2( 1, M1+IJ1 ), LDQ2,
     $                         DWORK( I2LORI ), SDIM, ZERO,
     $                         DWORK( ITMP ), N )
                  CALL DAXPY(  N, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                         DWORK( ITMP ), 1 )
                  CALL DAXPY(  N, DWORK( I2UPRI+SDIM ), DWORK( IWRK ),
     $                         1, DWORK( ITMP+N ), 1 )
                  CALL DLACPY( 'Full', N, DIM2, DWORK( ITMP ), N,
     $                         Q2( 1, M1+IJ1 ), LDQ2 )
               END IF
C
            ELSE IF( DIM1.EQ.2 .AND. DIM2.EQ.1 ) THEN
C
C              Update A.
C
               CALL DLACPY( 'Full', NR, DIM1, A( 1, IB1 ), LDA,
     $                      DWORK( IWRK ), NR )
               CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), NR,
     $                      DWORK( I1UPLE ), SDIM, ZERO, A( 1, IB1 ),
     $                      LDA )
               CALL DAXPY(  NR-1, DWORK( I1LOLE ), A( 1, M1+IJ1 ), 1,
     $                      A( 1, IB1 ), 1 )
               CALL DAXPY(  NR-1, DWORK( I1LOLE+SDIM ), A( 1, M1+IJ1 ),
     $                      1, A( 1, IB1+1 ), 1 )
               A( NR, M1+IJ1 ) = ZERO
               CALL DGEMV(  'No Transpose', NR, DIM1, ONE,
     $                      DWORK( IWRK ), NR, DWORK( I1UPRI ), 1,
     $                      DWORK( I1LORI ), A( 1, M1+IJ1 ), 1 )
C
               CALL DLACPY( 'Full', NROW, DIM1, A( I1, IB1 ), LDA,
     $                      DWORK( IWRK ), NROW )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), NROW,
     $                      DWORK( I1UPLE ), SDIM, ZERO, A( I1, IB1 ),
     $                      LDA )
               CALL DAXPY(  NROW, DWORK( I1LOLE ), A( I1, M1+IJ1 ), 1,
     $                      A( I1, IB1 ), 1 )
               CALL DAXPY(  NROW, DWORK( I1LOLE+SDIM ), A( I1, M1+IJ1 ),
     $                      1, A( I1, IB1+1 ), 1 )
               CALL DGEMV(  'No Transpose', NROW, DIM1, ONE,
     $                      DWORK( IWRK ), NROW, DWORK( I1UPRI ), 1,
     $                      DWORK( I1LORI ), A( I1, M1+IJ1 ), 1 )
C
               CALL DLACPY( 'Full', DIM1, M1-IB1+1, A( IB1, IB1 ), LDA,
     $                      DWORK( IWRK ), DIM1 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                      DIM1, ONE, DWORK( I2UPLE ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, A( IB1, IB1 ),
     $                      LDA )
               CALL DAXPY(  M1-IB1+1, DWORK( I2LOLE ), A( M1+IJ1, IB1 ),
     $                      LDA, A( IB1, IB1 ), LDA )
               CALL DAXPY(  M1-IB1+1, DWORK( I2LOLE+SDIM ),
     $                      A( M1+IJ1, IB1 ), LDA, A( IB1+1, IB1 ),
     $                      LDA )
               CALL DGEMV(  'Transpose', DIM1, M1-IB1+1, ONE,
     $                      DWORK( IWRK ), DIM1, DWORK( I2UPRI ), 1,
     $                      DWORK( I2LORI ), A( M1+IJ1, IB1 ), LDA )
C
               CALL DLACPY( 'Full', DIM1, M1-IJ1+1, A( IB1, M1+IJ1 ),
     $                      LDA, DWORK( IWRK ), DIM1 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IJ1+1,
     $                      DIM1, ONE, DWORK( I2UPLE ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, A( IB1, M1+IJ1 ),
     $                      LDA )
               CALL DAXPY(  M1-IJ1+1, DWORK( I2LOLE ),
     $                      A( M1+IJ1, M1+IJ1 ), LDA, A( IB1, M1+IJ1 ),
     $                      LDA )
               CALL DAXPY(  M1-IJ1+1, DWORK( I2LOLE+SDIM ),
     $                      A( M1+IJ1, M1+IJ1 ), LDA,
     $                      A( IB1+1, M1+IJ1 ), LDA )
               CALL DGEMV(  'Transpose', DIM1, M1-IJ1+1, ONE,
     $                      DWORK( IWRK ), DIM1, DWORK( I2UPRI ), 1,
     $                      DWORK( I2LORI ), A( M1+IJ1, M1+IJ1 ), LDA )
C
               IF( M2.GT.0 ) THEN
                  CALL DLACPY( 'Full', DIM1, M4, A( IB1, I2 ), LDA,
     $                         DWORK( IWRK ), DIM1 )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M4,
     $                         DIM1, ONE, DWORK( I2UPLE ), SDIM,
     $                         DWORK( IWRK ), DIM1, ZERO, A( IB1, I2 ),
     $                         LDA )
                  CALL DAXPY(  M4, DWORK( I2LOLE ), A( M1+IJ1, I2 ),
     $                         LDA, A( IB1, I2 ), LDA )
                  CALL DAXPY(  M4, DWORK( I2LOLE+SDIM ),
     $                         A( M1+IJ1, I2 ), LDA, A( IB1+1, I2 ),
     $                         LDA )
                  CALL DGEMV(  'Transpose', DIM1, M4, ONE,
     $                         DWORK( IWRK ), DIM1, DWORK( I2UPRI ), 1,
     $                         DWORK( I2LORI ), A( M1+IJ1, I2 ), LDA )
               END IF
C
C              Update B.
C
               CALL DLACPY( 'Full', NR, DIM1, B( 1, IB1 ), LDB,
     $                      DWORK( IWRK ), NR )
               CALL DGEMM(  'No Transpose', 'No Transpose', NR, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), NR,
     $                      DWORK( I1UPLE ), SDIM, ZERO, B( 1, IB1 ),
     $                      LDB )
               CALL DAXPY(  NR-1, DWORK( I1LOLE ), B( 1, M1+IJ1 ), 1,
     $                      B( 1, IB1 ), 1 )
               CALL DAXPY(  NR-1, DWORK( I1LOLE+SDIM ), B( 1, M1+IJ1 ),
     $                      1, B( 1, IB1+1 ), 1 )
               B( NR, M1+IJ1 ) = ZERO
               CALL DGEMV(  'No Transpose', NR, DIM1, ONE,
     $                      DWORK( IWRK ), NR, DWORK( I1UPRI ), 1,
     $                      DWORK( I1LORI ), B( 1, M1+IJ1 ), 1 )
C
               CALL DLACPY( 'Full', NROW, DIM1, B( I1, IB1 ), LDB,
     $                      DWORK( IWRK ), NROW )
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, DIM1,
     $                      DIM1, ONE, DWORK( IWRK ), NROW,
     $                      DWORK( I1UPLE ), SDIM, ZERO, B( I1, IB1 ),
     $                      LDB )
               CALL DAXPY(  NROW, DWORK( I1LOLE ), B( I1, M1+IJ1 ), 1,
     $                      B( I1, IB1 ), 1 )
               CALL DAXPY(  NROW, DWORK( I1LOLE+SDIM ), B( I1, M1+IJ1 ),
     $                      1, B( I1, IB1+1 ), 1 )
               CALL DGEMV(  'No Transpose', NROW, DIM1, ONE,
     $                      DWORK( IWRK ), NROW, DWORK( I1UPRI ), 1,
     $                      DWORK( I1LORI ), B( I1, M1+IJ1 ), 1 )
C
               CALL DLACPY( 'Full', DIM1, M1-IB1+1, B( IB1, IB1 ), LDB,
     $                      DWORK( IWRK ), DIM1 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IB1+1,
     $                      DIM1, ONE, DWORK( I2UPLE ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, B( IB1, IB1 ),
     $                      LDB )
               CALL DAXPY(  M1-IB1+1, DWORK( I2LOLE ), B( M1+IJ1, IB1 ),
     $                      LDB, B( IB1, IB1 ), LDB )
               CALL DAXPY(  M1-IB1+1, DWORK( I2LOLE+SDIM ),
     $                      B( M1+IJ1, IB1 ), LDB, B( IB1+1, IB1 ),
     $                      LDB )
               CALL DGEMV(  'Transpose', DIM1, M1-IB1+1, ONE,
     $                      DWORK( IWRK ), DIM1, DWORK( I2UPRI ), 1,
     $                      DWORK( I2LORI ), B( M1+IJ1, IB1 ), LDB )
C
               CALL DLACPY( 'Full', DIM1, M1-IJ1+1, B( IB1, M1+IJ1 ),
     $                      LDB, DWORK( IWRK ), DIM1 )
               CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M1-IJ1+1,
     $                      DIM1, ONE, DWORK( I2UPLE ), SDIM,
     $                      DWORK( IWRK ), DIM1, ZERO, B( IB1, M1+IJ1 ),
     $                      LDB )
               CALL DAXPY(  M1-IJ1+1, DWORK( I2LOLE ),
     $                      B( M1+IJ1, M1+IJ1 ), LDB, B( IB1, M1+IJ1 ),
     $                      LDB )
               CALL DAXPY(  M1-IJ1+1, DWORK( I2LOLE+SDIM ),
     $                      B( M1+IJ1, M1+IJ1 ), LDB,
     $                      B( IB1+1, M1+IJ1 ), LDB )
               CALL DGEMV(  'Transpose', DIM1, M1-IJ1+1, ONE,
     $                      DWORK( IWRK ), DIM1, DWORK( I2UPRI ), 1,
     $                      DWORK( I2LORI ), B( M1+IJ1, M1+IJ1 ), LDB )
C
               IF( M2.GT.0 ) THEN
                  CALL DLACPY( 'Full', DIM1, M4, B( IB1, I2 ), LDB,
     $                         DWORK( IWRK ), DIM1 )
                  CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M4,
     $                         DIM1, ONE, DWORK( I2UPLE ), SDIM,
     $                         DWORK( IWRK ), DIM1, ZERO, B( IB1, I2 ),
     $                         LDB )
                  CALL DAXPY(  M4, DWORK( I2LOLE ), B( M1+IJ1, I2 ),
     $                         LDB, B( IB1, I2 ), LDB )
                  CALL DAXPY(  M4, DWORK( I2LOLE+SDIM ),
     $                         B( M1+IJ1, I2 ), LDB, B( IB1+1, I2 ),
     $                         LDB )
                  CALL DGEMV(  'Transpose', DIM1, M4, ONE,
     $                         DWORK( IWRK ), DIM1, DWORK( I2UPRI ), 1,
     $                         DWORK( I2LORI ), B( M1+IJ1, I2 ), LDB )
               END IF
C
C              Update Q1.
C
               IF( LCMPQ1 ) THEN
                  CALL DLACPY( 'Full', N, DIM1, Q1( 1, IB1 ), LDQ1,
     $                         DWORK( IWRK ), N )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                         DIM1, ONE, DWORK( IWRK ), N,
     $                         DWORK( I1UPLE ), SDIM, ZERO,
     $                         Q1( 1, IB1 ), LDQ1 )
                  CALL DAXPY(  N, DWORK( I1LOLE ), Q1( 1, M1+IJ1 ), 1,
     $                         Q1( 1, IB1 ), 1 )
                  CALL DAXPY(  N, DWORK( I1LOLE+SDIM ), Q1( 1, M1+IJ1 ),
     $                         1, Q1( 1, IB1+1 ), 1 )
                  CALL DGEMV(  'No Transpose', N, DIM1, ONE,
     $                         DWORK( IWRK ), N, DWORK( I1UPRI ), 1, 
     $                         DWORK( I1LORI ), Q1( 1, M1+IJ1 ), 1 )
               END IF
C
C              Update Q2.
C
               IF( LCMPQ2 ) THEN
                  CALL DLACPY( 'Full', N, DIM1, Q2( 1, IB1 ), LDQ2,
     $                         DWORK( IWRK ), N )
                  CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                         DIM1, ONE, DWORK( IWRK ), N,
     $                         DWORK( I2UPLE ), SDIM, ZERO,
     $                         Q2( 1, IB1 ), LDQ2 )
                  CALL DAXPY(  N, DWORK( I2LOLE ), Q2( 1, M1+IJ1 ), 1,
     $                         Q2( 1, IB1 ), 1 )
                  CALL DAXPY(  N, DWORK( I2LOLE+SDIM ), Q2( 1, M1+IJ1 ),
     $                         1, Q2( 1, IB1+1 ), 1 )
                  CALL DGEMV(  'No Transpose', N, DIM1, ONE,
     $                         DWORK( IWRK ), N, DWORK( I2UPRI ), 1, 
     $                         DWORK( I2LORI ), Q2( 1, M1+IJ1 ), 1 )
               END IF
C
            ELSE
C
C              Update A.
C
               CALL DCOPY( NR, A( 1, IB1 ), 1, DWORK( IWRK ), 1 )
               CALL DSCAL( NR, DWORK( I1UPLE ), A( 1, IB1 ), 1 )
               CALL DAXPY( NR-1, DWORK( I1LOLE ), A( 1, M1+IJ1 ), 1,
     $                     A( 1, IB1 ), 1 )
               CALL DSCAL( NR-1, DWORK( I1LORI ), A( 1, M1+IJ1 ), 1 )
               CALL DAXPY( NR-1, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                     A( 1, M1+IJ1 ), 1 )
               A( NR, M1+IJ1 ) = DWORK( I1UPRI )*DWORK( IWRK+NR-1 )
C
               CALL DCOPY( NROW, A( I1, IB1 ), 1, DWORK( IWRK ), 1 )
               CALL DSCAL( NROW, DWORK( I1UPLE ), A( I1, IB1 ), 1 )
               CALL DAXPY( NROW, DWORK( I1LOLE ), A( I1, M1+IJ1 ), 1,
     $                     A( I1, IB1 ), 1 )
               CALL DSCAL( NROW, DWORK( I1LORI ), A( I1, M1+IJ1 ), 1 )
               CALL DAXPY( NROW, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                     A( I1, M1+IJ1 ), 1 )
C
               CALL DCOPY( M1-IB1+1, A( IB1, IB1 ), LDA, DWORK( IWRK ),
     $                     1 )
               CALL DSCAL( M1-IB1+1, DWORK( I2UPLE ), A( IB1, IB1 ),
     $                     LDA )
               CALL DAXPY( M1-IB1+1, DWORK( I2LOLE ), A( M1+IJ1, IB1 ),
     $                     LDA, A( IB1, IB1 ), LDA )
               CALL DSCAL( M1-IB1+1, DWORK( I2LORI ), A( M1+IJ1, IB1 ),
     $                     LDA )
               CALL DAXPY( M1-IB1+1, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                     A( M1+IJ1, IB1 ), LDA )
C
               CALL DCOPY( M1-IJ1+1, A( IB1, M1+IJ1 ), LDA,
     $                     DWORK( IWRK ), 1 )
               CALL DSCAL( M1-IJ1+1, DWORK( I2UPLE ), A( IB1, M1+IJ1 ),
     $                     LDA )
               CALL DAXPY( M1-IJ1+1, DWORK( I2LOLE ),
     $                     A( M1+IJ1, M1+IJ1 ), LDA, A( IB1, M1+IJ1 ),
     $                     LDA )
               CALL DSCAL( M1-IJ1+1, DWORK( I2LORI ),
     $                     A( M1+IJ1, M1+IJ1 ), LDA )
               CALL DAXPY( M1-IJ1+1, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                     A( M1+IJ1, M1+IJ1 ), LDA )
C
               IF( M2.GT.0 ) THEN
                  CALL DCOPY( M4, A( IB1, I2 ), LDA, DWORK( IWRK ), 1 )
                  CALL DSCAL( M4, DWORK( I2UPLE ), A( IB1, I2 ), LDA )
                  CALL DAXPY( M4, DWORK( I2LOLE ), A( M1+IJ1, I2 ), LDA,
     $                        A( IB1, I2 ), LDA )
                  CALL DSCAL( M4, DWORK( I2LORI ), A( M1+IJ1, I2 ),
     $                        LDA )
                  CALL DAXPY( M4, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                        A( M1+IJ1, I2 ), LDA )
               END IF
C
C              Update B.
C
               CALL DCOPY( NR, B( 1, IB1 ), 1, DWORK( IWRK ), 1 )
               CALL DSCAL( NR, DWORK( I1UPLE ), B( 1, IB1 ), 1 )
               CALL DAXPY( NR-1, DWORK( I1LOLE ), B( 1, M1+IJ1 ), 1,
     $                     B( 1, IB1 ), 1 )
               CALL DSCAL( NR-1, DWORK( I1LORI ), B( 1, M1+IJ1 ), 1 )
               CALL DAXPY( NR-1, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                     B( 1, M1+IJ1 ), 1 )
               B( NR, M1+IJ1 ) = DWORK( I1UPRI )*DWORK( IWRK+NR-1 )
C
               CALL DCOPY( NROW, B( I1, IB1 ), 1, DWORK( IWRK ), 1 )
               CALL DSCAL( NROW, DWORK( I1UPLE ), B( I1, IB1 ), 1 )
               CALL DAXPY( NROW, DWORK( I1LOLE ), B( I1, M1+IJ1 ), 1,
     $                     B( I1, IB1 ), 1 )
               CALL DSCAL( NROW, DWORK( I1LORI ), B( I1, M1+IJ1 ), 1 )
               CALL DAXPY( NROW, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                     B( I1, M1+IJ1 ), 1 )
C
               CALL DCOPY( M1-IB1+1, B( IB1, IB1 ), LDB, DWORK( IWRK ),
     $                     1 )
               CALL DSCAL( M1-IB1+1, DWORK( I2UPLE ), B( IB1, IB1 ),
     $                     LDB )
               CALL DAXPY( M1-IB1+1, DWORK( I2LOLE ), B( M1+IJ1, IB1 ),
     $                     LDB, B( IB1, IB1 ), LDB )
               CALL DSCAL( M1-IB1+1, DWORK( I2LORI ), B( M1+IJ1, IB1 ),
     $                     LDB )
               CALL DAXPY( M1-IB1+1, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                     B( M1+IJ1, IB1 ), LDB )
C
               CALL DCOPY( M1-IJ1+1, B( IB1, M1+IJ1 ), LDB,
     $                     DWORK( IWRK ), 1 )
               CALL DSCAL( M1-IJ1+1, DWORK( I2UPLE ), B( IB1, M1+IJ1 ),
     $                     LDB )
               CALL DAXPY( M1-IJ1+1, DWORK( I2LOLE ),
     $                     B( M1+IJ1, M1+IJ1 ), LDB, B( IB1, M1+IJ1 ),
     $                     LDB )
               CALL DSCAL( M1-IJ1+1, DWORK( I2LORI ),
     $                     B( M1+IJ1, M1+IJ1 ), LDB )
               CALL DAXPY( M1-IJ1+1, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                     B( M1+IJ1, M1+IJ1 ), LDB )
C
               IF( M2.GT.0 ) THEN
                  CALL DCOPY( M4, B( IB1, I2 ), LDB, DWORK( IWRK ), 1 )
                  CALL DSCAL( M4, DWORK( I2UPLE ), B( IB1, I2 ), LDB )
                  CALL DAXPY( M4, DWORK( I2LOLE ), B( M1+IJ1, I2 ), LDB,
     $                        B( IB1, I2 ), LDB )
                  CALL DSCAL( M4, DWORK( I2LORI ), B( M1+IJ1, I2 ),
     $                        LDB )
                  CALL DAXPY( M4, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                        B( M1+IJ1, I2 ), LDB )
               END IF
C
C              Update Q1.
C
               IF( LCMPQ1 ) THEN
                  CALL DCOPY( N, Q1( 1, IB1 ), 1, DWORK( IWRK ), 1 )
                  CALL DSCAL( N, DWORK( I1UPLE ), Q1( 1, IB1 ),  1 )
                  CALL DAXPY( N, DWORK( I1LOLE ), Q1( 1, M1+IJ1 ), 1,
     $                        Q1( 1, IB1 ), 1 )
                  CALL DSCAL( N, DWORK( I1LORI ), Q1( 1, M1+IJ1 ), 1 )
                  CALL DAXPY( N, DWORK( I1UPRI ), DWORK( IWRK ), 1,
     $                        Q1( 1, M1+IJ1 ), 1 )
               END IF
C
C              Update Q2.
C
               IF( LCMPQ2 ) THEN
                  CALL DCOPY( N, Q2( 1, IB1 ), 1, DWORK( IWRK ), 1 )
                  CALL DSCAL( N, DWORK( I2UPLE ), Q2( 1, IB1 ),  1 )
                  CALL DAXPY( N, DWORK( I2LOLE ), Q2( 1, M1+IJ1 ), 1,
     $                        Q2( 1, IB1 ), 1 )
                  CALL DSCAL( N, DWORK( I2LORI ), Q2( 1, M1+IJ1 ), 1 )
                  CALL DAXPY( N, DWORK( I2UPRI ), DWORK( IWRK ), 1,
     $                        Q2( 1, M1+IJ1 ), 1 )
               END IF
            END IF
   50    CONTINUE
   60 CONTINUE
C
C     Triangularize the lower right subpencil aAA2 - bBB2.
C
      IF( M2.GT.1 ) THEN
         CALL DLACPY( 'Full', N, M4-2, A( 1, I2+1 ), LDA, DWORK, N )
         DO 70 I = 1, M2 - 1
            CALL DCOPY( N, DWORK( N*( I-1 )+1 ), 1,
     $                  A( 1, 2*( M1+I )+1 ), 1 )
            CALL DCOPY( N, DWORK( N*( M2+I-2 )+1 ), 1,
     $                  A( 1, 2*( M1+I ) ), 1 )
   70    CONTINUE
C
         CALL DLACPY( 'Full', M4-2, M4, A( I2+1, I2 ), LDA, DWORK,
     $                M4-2 )
         DO 80 I = 1, M2 - 1
            CALL DCOPY( M4, DWORK( I ), M4-2, A( 2*( M1+I )+1, I2 ),
     $                  LDA )
            CALL DCOPY( M4, DWORK( M2+I-1 ), M4-2, A( 2*( M1+I ), I2 ),
     $                  LDA )
   80    CONTINUE
C
         CALL DLACPY( 'Full', N, M4-2, B( 1, I2+1 ), LDB, DWORK, N )
         DO 90 I = 1, M2 - 1
            CALL DCOPY( N, DWORK( N*( I-1 )+1 ), 1,
     $                  B( 1, 2*( M1+I )+1 ), 1 )
            CALL DCOPY( N, DWORK( N*( M2+I-2 )+1 ), 1,
     $                  B( 1, 2*( M1+I ) ), 1 )
   90    CONTINUE
C
         CALL DLACPY( 'Full', M4-2, M4, B( I2+1, I2 ), LDB, DWORK,
     $                M4-2 )
         DO 100 I = 1, M2 - 1
            CALL DCOPY( M4, DWORK( I ), M4-2, B( 2*( M1+I )+1, I2 ),
     $                  LDB )
            CALL DCOPY( M4, DWORK( M2+I-1 ), M4-2, B( 2*( M1+I ), I2 ),
     $                  LDB )
  100    CONTINUE
C
         IF( LCMPQ1 ) THEN
            CALL DLACPY( 'Full', N, M4-2, Q1( 1, I2+1 ), LDQ1, DWORK,
     $                    N )
            DO 110 I = 1, M2 - 1
               CALL DCOPY( N, DWORK( N*( I-1 )+1 ), 1,
     $                     Q1( 1, 2*( M1+I )+1 ), 1 )
               CALL DCOPY( N, DWORK( N*( M2+I-2 )+1 ), 1,
     $                     Q1( 1, 2*( M1+I ) ), 1 )
  110       CONTINUE
         END IF
C
         IF( LCMPQ2 ) THEN
            CALL DLACPY( 'Full', N, M4-2, Q2( 1, I2+1 ), LDQ2, DWORK,
     $                    N )
            DO 120 I = 1, M2 - 1
               CALL DCOPY( N, DWORK( N*( I-1 )+1 ), 1,
     $                     Q2( 1, 2*( M1+I )+1 ), 1 )
               CALL DCOPY( N, DWORK( N*( M2+I-2 )+1 ), 1,
     $                     Q2( 1, 2*( M1+I ) ), 1 )
  120       CONTINUE
         END IF
      END IF
C
      DWORK( 1 ) = OPTWRK
      RETURN
C *** Last line of MB04HD ***
      END
