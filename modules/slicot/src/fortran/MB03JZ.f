      SUBROUTINE MB03JZ( COMPQ, N, A, LDA, D, LDD, B, LDB, F, LDF, Q,
     $                   LDQ, NEIG, TOL, INFO )
C
C     PURPOSE
C
C     To move the eigenvalues with strictly negative real parts of an
C     N-by-N complex skew-Hamiltonian/Hamiltonian pencil aS - bH in
C     structured Schur form to the leading principal subpencil, while
C     keeping the triangular form. On entry, we have
C
C           (  A  D  )      (  B  F  )
C       S = (        ), H = (        ),
C           (  0  A' )      (  0 -B' )
C
C     where A and B are upper triangular.
C     S and H are transformed by a unitary matrix Q such that
C
C                            (  Aout  Dout  )
C       Sout = J Q' J' S Q = (              ), and
C                            (    0   Aout' )
C                                                                    (1)
C                            (  Bout  Fout  )           (  0  I  )
C       Hout = J Q' J' H Q = (              ), with J = (        ),
C                            (    0  -Bout' )           ( -I  0  )
C
C     where Aout and Bout remain in upper triangular form. The notation
C     M' denotes the conjugate transpose of the matrix M.
C     Optionally, if COMPQ = 'I' or COMPQ = 'U', the unitary matrix Q
C     that fulfills (1) is computed.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     COMPQ   CHARACTER*1
C             Specifies whether or not the unitary transformations
C             should be accumulated in the array Q, as follows:
C             = 'N':  Q is not computed;
C             = 'I':  the array Q is initialized internally to the unit
C                     matrix, and the unitary matrix Q is returned;
C             = 'U':  the array Q contains a unitary matrix Q0 on
C                     entry, and the matrix Q0*Q is returned, where Q
C                     is the product of the unitary transformations
C                     that are applied to the pencil aS - bH to reorder
C                     the eigenvalues.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the pencil aS - bH.  N >= 0, even.
C
C     A       (input/output) COMPLEX*16 array, dimension (LDA, N/2)
C             On entry, the leading N/2-by-N/2 part of this array must
C             contain the upper triangular matrix A.
C             On exit, the leading  N/2-by-N/2 part of this array
C             contains the transformed matrix Aout.
C             The strictly lower triangular part of this array is not
C             referenced.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1, N/2).
C
C     D       (input/output) COMPLEX*16 array, dimension (LDD, N/2)
C             On entry, the leading N/2-by-N/2 part of this array must
C             contain the upper triangular part of the skew-Hermitian
C             matrix D.
C             On exit, the leading  N/2-by-N/2 part of this array
C             contains the transformed matrix Dout.
C             The strictly lower triangular part of this array is not
C             referenced.
C
C     LDD     INTEGER
C             The leading dimension of the array D.  LDD >= MAX(1, N/2).
C
C     B       (input/output) COMPLEX*16 array, dimension (LDB, N/2)
C             On entry, the leading N/2-by-N/2 part of this array must
C             contain the upper triangular matrix B.
C             On exit, the leading  N/2-by-N/2 part of this array
C             contains the transformed matrix Bout.
C             The strictly lower triangular part of this array is not
C             referenced.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= MAX(1, N/2).
C
C     F       (input/output) COMPLEX*16 array, dimension (LDF, N/2)
C             On entry, the leading N/2-by-N/2 part of this array must
C             contain the upper triangular part of the Hermitian matrix
C             F.
C             On exit, the leading  N/2-by-N/2 part of this array
C             contains the transformed matrix Fout.
C             The strictly lower triangular part of this array is not
C             referenced.
C
C     LDF     INTEGER
C             The leading dimension of the array F.  LDF >= MAX(1, N/2).
C
C     Q       (input/output) COMPLEX*16 array, dimension (LDQ, N)
C             On entry, if COMPQ = 'U', then the leading N-by-N part of
C             this array must contain a given matrix Q0, and on exit,
C             the leading N-by-N part of this array contains the product
C             of the input matrix Q0 and the transformation matrix Q
C             used to transform the matrices S and H.
C             On exit, if COMPQ = 'I', then the leading N-by-N part of
C             this array contains the unitary transformation matrix Q.
C             If COMPQ = 'N' this array is not referenced.
C
C     LDQ     INTEGER
C             The leading dimension of the array Q.
C             LDQ >= 1,         if COMPQ = 'N';
C             LDQ >= MAX(1, N), if COMPQ = 'I' or COMPQ = 'U'.
C
C     NEIG    (output) INTEGER
C             The number of eigenvalues in aS - bH with strictly
C             negative real part.
C
C     Tolerances
C
C     TOL     DOUBLE PRECISION
C             The tolerance used to decide the sign of the eigenvalues.
C             If the user sets TOL > 0, then the given value of TOL is
C             used. If the user sets TOL <= 0, then an implicitly
C             computed, default tolerance, defined by MIN(N,10)*EPS, is
C             used instead, where EPS is the machine precision (see
C             LAPACK Library routine DLAMCH). A larger value might be
C             needed for pencils with multiple eigenvalues.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0: succesful exit;
C             < 0: if INFO = -i, the i-th argument had an illegal value.
C
C     METHOD
C
C     The algorithm reorders the eigenvalues like the following scheme:
C
C     Step 1: Reorder the eigenvalues in the subpencil aA - bB.
C          I. Reorder the eigenvalues with negative real parts to the
C             top.
C         II. Reorder the eigenvalues with positive real parts to the
C             bottom.
C
C     Step 2: Reorder the remaining eigenvalues with negative real parts.
C          I. Exchange the eigenvalues between the last diagonal block
C             in aA - bB and the last diagonal block in aS - bH.
C         II. Move the eigenvalues in the N/2-th place to the (MM+1)-th
C             place, where MM denotes the current number of eigenvalues
C             with negative real parts in aA - bB.
C
C     The algorithm uses a sequence of unitary transformations as
C     described on page 43 in [1]. To achieve those transformations the
C     elementary SLICOT Library subroutines MB03DZ and MB03HZ are called
C     for the corresponding matrix structures.
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
C                                                               3
C     The algorithm is numerically backward stable and needs O(N )
C     complex floating point operations.
C
C     CONTRIBUTOR
C
C     Matthias Voigt, Fakultaet fuer Mathematik, Technische Universitaet
C     Chemnitz, April 6, 2009.
C     V. Sima, Aug. 2009 (SLICOT version of the routine ZHAUNX).
C
C     REVISIONS
C
C     V. Sima, Dec. 2010, Jan. 2011, Aug. 2014.
C     M. Voigt, Jan. 2012.
C
C     KEYWORDS
C
C     Eigenvalue reordering, upper triangular matrix,
C     skew-Hamiltonian/Hamiltonian pencil, structured Schur form.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ZERO, TEN
      PARAMETER          ( ZERO = 0.0D+0, TEN = 1.0D+1 )
      COMPLEX*16         CZERO, CONE
      PARAMETER          ( CZERO = ( 0.0D+0, 0.0D+0 ), CONE = ( 1.0D+0,
     $                     0.0D+0 ) )
C
C     .. Scalar Arguments ..
      CHARACTER          COMPQ
      INTEGER            INFO, LDA, LDB, LDD, LDF, LDQ, N, NEIG
      DOUBLE PRECISION   TOL
C
C     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), B( LDB, * ), D( LDD, * ),
     $                   F( LDF, * ), Q( LDQ, * ) 
C
C     .. Local Scalars ..
      LOGICAL            LCMPQ, LINIQ, LUPDQ
      INTEGER            IUPD, J, K, M, MM, MP, UPDS
      DOUBLE PRECISION   CO1, CO2, EPS
      COMPLEX*16         CJF, SI1, SI2, TMP
C
C     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH, LSAME
C
C     .. External Subroutines ..
      EXTERNAL           MB03DZ, MB03HZ, XERBLA, ZLASET, ZROT
C
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCONJG, DIMAG, MAX, MIN, MOD
C
C     .. Executable Statements ..
C
C     Decode the input arguments.
C
      M     = N/2
      NEIG  = 0
      LINIQ = LSAME( COMPQ, 'I' )
      LUPDQ = LSAME( COMPQ, 'U' )
      LCMPQ = LINIQ .OR. LUPDQ
C
C     Test the input arguments.
C
      INFO = 0
      IF( .NOT.( LSAME( COMPQ, 'N' ) .OR. LCMPQ ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 .OR. MOD( N, 2 ).NE.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      ELSE IF( LDD.LT.MAX( 1, M ) ) THEN
         INFO = -6
      ELSE IF( LDB.LT.MAX( 1, M ) ) THEN
         INFO = -8
      ELSE IF( LDF.LT.MAX( 1, M ) ) THEN
         INFO = -10
      ELSE IF( LDQ.LT.1 .OR. ( LCMPQ .AND. LDQ.LT.N ) ) THEN
         INFO = -12
      END IF
      IF( INFO.NE.0) THEN
         CALL XERBLA( 'MB03JZ', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( N.EQ.0 )
     $   RETURN
C
      EPS = TOL
      IF ( EPS.LE.ZERO ) THEN
C
C        Use the default tolerance.
C
         EPS = MIN( DBLE( N ), TEN )*DLAMCH( 'Precision' )
      END IF
C
C     STEP 0. Initializations.
C
      IF( LINIQ ) THEN
         IUPD = M + 1
         UPDS = M
         CALL ZLASET( 'Full', N, N, CZERO, CONE, Q, LDQ )
      ELSE IF( LUPDQ ) THEN
         IUPD = 1
         UPDS = N
      END IF
C
C     STEP 1. Reorder the eigenvalues in the subpencil aA - bB.
C
      MM = 0
      MP = M + 1
C
C     I. Reorder the eigenvalues with negative real parts to the top.
C
      DO 20 K = 1, M
         IF( DBLE(  A( K, K ) )*DBLE(  B( K, K ) ) +
     $       DIMAG( A( K, K ) )*DIMAG( B( K, K ) ).LT.
     $       -ABS(  A( K, K ) )*ABS(   B( K, K ) )*EPS ) THEN
C
            DO 10 J = K - 1, MM + 1, -1
C
C              Perform eigenvalue exchange.
C
               CALL MB03DZ( A( J, J ), LDA, B( J, J ), LDB, CO1, SI1,
     $                      CO2, SI2 )
C
C              Update A and D.
C
               CALL ZROT( J, A( 1, J+1 ), 1, A( 1, J ), 1, CO1, SI1 )
               A( J, J )     = CO2*A( J, J ) +
     $                         SI2*A( J+1, J+1 )*DCONJG( SI1 )
               A( J+1, J+1 ) = CO1*A( J+1, J+1 )
               CALL ZROT( M-J, A( J, J+1 ), LDA, A( J+1, J+1 ), LDA,
     $                    CO2, -SI2 )
C
               CJF = -DCONJG( D( J, J+1 ) )
               TMP = CO2*CJF - DCONJG( SI2 )*D( J+1, J+1 )
               CALL ZROT( J, D( 1, J+1 ), 1, D( 1, J ), 1, CO2, SI2 )
               D( J, J )     = CO2*D( J, J )     - SI2*TMP
               D( J+1, J+1 ) = CO2*D( J+1, J+1 ) + SI2*CJF
               CALL ZROT( M-J, D( J, J+1 ), LDD, D( J+1, J+1 ), LDD,
     $                    CO2, -SI2 )
C
C              Update B and F.
C
               CALL ZROT( J, B( 1, J+1 ), 1, B( 1, J ), 1, CO1, SI1 )
               B( J, J )     = CO2*B( J, J ) +
     $                         SI2*B( J+1, J+1 )*DCONJG( SI1 )
               B( J+1, J+1 ) = CO1*B( J+1, J+1 )
               CALL ZROT( M-J, B( J, J+1 ), LDB, B( J+1, J+1 ), LDB,
     $                    CO2, -SI2 )
C
               CJF = DCONJG( F( J, J+1 ) )
               TMP = CO2*CJF - DCONJG( SI2 )*F( J+1, J+1 )
               CALL ZROT( J, F( 1, J+1 ), 1, F( 1, J ), 1, CO2, SI2 )
               F( J, J )     = CO2*F( J, J )     - SI2*TMP
               F( J+1, J+1 ) = CO2*F( J+1, J+1 ) + SI2*CJF
               CALL ZROT( M-J, F( J, J+1 ), LDF, F( J+1, J+1 ), LDF,
     $                    CO2, -SI2 )
C
               IF( LCMPQ ) THEN
C
C                 Update Q.
C
                  CALL ZROT( UPDS, Q( 1, J+1 ), 1, Q( 1, J ), 1, CO1,
     $                       SI1 )
                  CALL ZROT( UPDS, Q( IUPD, M+J+1 ), 1, Q( IUPD, M+J ),
     $                       1, CO2, SI2 )
               END IF
   10       CONTINUE
            MM = MM + 1
         END IF
   20 CONTINUE
C
C     II. Reorder the eigenvalues with positive real parts to the bottom.
C
      DO 40 K = M, MM + 1, -1
         IF( DBLE(  A( K, K ) )*DBLE(  B( K, K ) ) +
     $       DIMAG( A( K, K ) )*DIMAG( B( K, K ) ).GT.
     $       ABS(   A( K, K ) )*ABS(   B( K, K ) )*EPS ) THEN
C
            DO 30 J = K, MP - 2
C
C              Perform eigenvalue exchange.
C
               CALL MB03DZ( A( J, J ), LDA, B( J, J ), LDB, CO1, SI1,
     $                      CO2, SI2 )
C
C              Update A and D.
C
               CALL ZROT( J, A( 1, J+1 ), 1, A( 1, J ), 1, CO1, SI1 )
               A( J, J )     = CO2*A( J, J ) +
     $                         SI2*A( J+1, J+1 )*DCONJG( SI1 )
               A( J+1, J+1 ) = CO1*A( J+1, J+1 )
               CALL ZROT( M-J, A( J, J+1 ), LDA, A( J+1, J+1 ), LDA,
     $                    CO2, -SI2 )
C
               CJF = -DCONJG( D( J, J+1 ) )
               TMP = CO2*CJF - DCONJG( SI2 )*D( J+1, J+1 )
               CALL ZROT( J, D( 1, J+1 ), 1, D( 1, J ), 1, CO2, SI2 )
               D( J, J )     = CO2*D( J, J )     - SI2*TMP
               D( J+1, J+1 ) = CO2*D( J+1, J+1 ) + SI2*CJF
               CALL ZROT( M-J, D( J, J+1 ), LDD, D( J+1, J+1 ), LDD,
     $                    CO2, -SI2 )
C
C              Update B and F.
C
               CALL ZROT( J, B( 1, J+1 ), 1, B( 1, J ), 1, CO1, SI1 )
               B( J, J )     = CO2*B( J, J ) +
     $                         SI2*B( J+1, J+1 )*DCONJG( SI1 )
               B( J+1, J+1 ) = CO1*B( J+1, J+1 )
               CALL ZROT( M-J, B( J, J+1 ), LDB, B( J+1, J+1 ), LDB,
     $                    CO2, -SI2 )
C
               CJF = DCONJG( F( J, J+1 ) )
               TMP = CO2*CJF - DCONJG( SI2 )*F( J+1, J+1 )
               CALL ZROT( J, F( 1, J+1 ), 1, F( 1, J ), 1, CO2, SI2 )
               F( J, J )     = CO2*F( J, J )     - SI2*TMP
               F( J+1, J+1 ) = CO2*F( J+1, J+1 ) + SI2*CJF
               CALL ZROT( M-J, F( J, J+1 ), LDF, F( J+1, J+1 ), LDF,
     $                    CO2, -SI2 )
C
               IF( LCMPQ ) THEN
C
C                 Update Q.
C
                  CALL ZROT( UPDS, Q( 1, J+1 ), 1, Q( 1, J ), 1, CO1,
     $                       SI1 )
                  CALL ZROT( UPDS, Q( IUPD, M+J+1 ), 1, Q( IUPD, M+J ),
     $                       1, CO2, SI2 )
               END IF
   30       CONTINUE
            MP = MP - 1
         END IF
   40 CONTINUE
C
C     The remaining M-MP+1 eigenvalues with negative real part are now in
C     the bottom right subpencil of aS - bH.
C
C     STEP 2. Reorder the remaining M-MP+1 eigenvalues.
C
      DO 60 K = M, MP, -1
C
C        I. Exchange the eigenvalues between two diagonal blocks.
C
C        Perform eigenvalue exchange.
C
         CALL MB03HZ( A( M, M ), D( M, M ), B( M, M ), F( M, M ), CO1,
     $                SI1 )
C
C        Update A and D.
C
         TMP = DCONJG( A( M, M ) )
         CALL ZROT( M, D( 1, M ), 1, A( 1, M ), 1, CO1, SI1 )
         A( M, M ) = A( M, M )*CO1 + TMP*DCONJG( SI1 )**2
         D( M, M ) = D( M, M )*CO1 - TMP*DCONJG( SI1 )*CO1
C
C        Update B and F.
C
         TMP = -DCONJG( B( M, M ) )
         CALL ZROT( M, F( 1, M ), 1, B( 1, M ), 1, CO1, SI1 )
         B( M, M ) = B( M, M )*CO1 + TMP*DCONJG( SI1 )**2
         F( M, M ) = F( M, M )*CO1 - TMP*DCONJG( SI1 )*CO1
C
         IF( LCMPQ ) THEN
C
C           Update Q.
C
            CALL ZROT( N, Q( 1, N ), 1, Q( 1, M ), 1, CO1, SI1 )
         END IF
C
C        II. Move the eigenvalue in the M-th diagonal position to the
C            (MM+1)-th position.
C
         MM = MM + 1
         DO 50 J = M - 1, MM, -1
C
C           Perform eigenvalue exchange.
C
            CALL MB03DZ( A( J, J ), LDA, B( J, J ), LDB, CO1, SI1, CO2,
     $                   SI2 )
C
C           Update A and D.
C
            CALL ZROT( J, A( 1, J+1 ), 1, A( 1, J ), 1, CO1, SI1 )
            A( J, J )     = CO2*A( J, J ) +
     $                      SI2*A( J+1, J+1 )*DCONJG( SI1 )
            A( J+1, J+1 ) = CO1*A( J+1, J+1 )
            CALL ZROT( M-J, A( J, J+1 ), LDA, A( J+1, J+1 ), LDA, CO2,
     $                 -SI2 )
C
            CJF = -DCONJG( D( J, J+1 ) )
            TMP = CO2*CJF - DCONJG( SI2 )*D( J+1, J+1 )
            CALL ZROT( J, D( 1, J+1 ), 1, D( 1, J ), 1, CO2, SI2 )
            D( J, J )     = CO2*D( J, J )     - SI2*TMP
            D( J+1, J+1 ) = CO2*D( J+1, J+1 ) + SI2*CJF
            CALL ZROT( M-J, D( J, J+1 ), LDD, D( J+1, J+1 ), LDD, CO2,
     $                 -SI2 )
C
C           Update B and F.
C
            CALL ZROT( J, B( 1, J+1 ), 1, B( 1, J ), 1, CO1, SI1 )
            B( J, J )     = CO2*B( J, J ) +
     $                      SI2*B( J+1, J+1 )*DCONJG( SI1 )
            B( J+1, J+1 ) = CO1*B( J+1, J+1 )
            CALL ZROT( M-J, B( J, J+1 ), LDB, B( J+1, J+1 ), LDB, CO2,
     $                 -SI2 )
C
            CJF = DCONJG( F( J, J+1 ) )
            TMP = CO2*CJF - DCONJG( SI2 )*F( J+1, J+1 )
            CALL ZROT( J, F( 1, J+1 ), 1, F( 1, J ), 1, CO2, SI2 )
            F( J, J )     = CO2*F( J, J )     - SI2*TMP
            F( J+1, J+1 ) = CO2*F( J+1, J+1 ) + SI2*CJF
            CALL ZROT( M-J, F( J, J+1 ), LDF, F( J+1, J+1 ), LDF, CO2,
     $                 -SI2 )
C
            IF( LCMPQ ) THEN
C
C              Update Q.
C
               CALL ZROT( N, Q( 1, J+1 ), 1, Q( 1, J ), 1, CO1, SI1 )
               CALL ZROT( N, Q( 1, M+J+1 ), 1, Q( 1, M+J ), 1, CO2, SI2
     $                     )
            END IF
   50    CONTINUE
   60 CONTINUE
C
      NEIG = MM
C
      RETURN
C *** Last line of MB03JZ ***
      END
