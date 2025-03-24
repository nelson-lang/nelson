      SUBROUTINE MB3JZP( COMPQ, N, A, LDA, D, LDD, B, LDB, F, LDF, Q,
     $                   LDQ, NEIG, TOL, DWORK, ZWORK, INFO )
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
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (N/2)
C
C     ZWORK   COMPLEX*16 array, dimension (N/2)
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
C     FURTHER COMMENTS
C
C     For large values of N, the routine applies the transformations on
C     panels of columns. The user may specify in INFO the desired number
C     of columns. If on entry INFO <= 0, then the routine estimates a
C     suitable value of this number.
C
C     CONTRIBUTOR
C
C     V. Sima, Research Institute for Informatics, Bucharest, Jan. 2011.
C     M. Voigt, Max Planck Institute for Dynamics of Complex Technical
C     Systems, Dec. 2011.
C
C     REVISIONS
C
C     V. Sima, June 2013, June 2014, Nov. 2014.
C     M. Voigt, July 2013, July 2014. 
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
     $                   F( LDF, * ), Q( LDQ, * ), ZWORK( * ) 
      DOUBLE PRECISION   DWORK( * )
C
C     .. Local Scalars ..
      LOGICAL            LCMPQ, LINIQ, LUPDQ
      INTEGER            IC, ICS, IUPD, J, JE, JS, K, M, M1, MM, MP, NB,
     $                   NC, UPDS
      DOUBLE PRECISION   CO1, CO2, EPS
      COMPLEX*16         CJF, SI1, SI2, TMP
C
C     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH, LSAME
C
C     .. External Subroutines ..
      EXTERNAL           MB03DZ, MB03HZ, XERBLA, ZGEQRF, ZLASET, ZROT
C
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCONJG, DIMAG, MAX, MIN, MOD
C
C     .. Executable Statements ..
C
C     Decode the input arguments.
C
      NB    = INFO
      M     = N/2
      M1    = MAX( 1, M )
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
      ELSE IF( LDA.LT.M1 ) THEN
         INFO = -4
      ELSE IF( LDD.LT.M1 ) THEN
         INFO = -6
      ELSE IF( LDB.LT.M1 ) THEN
         INFO = -8
      ELSE IF( LDF.LT.M1 ) THEN
         INFO = -10
      ELSE IF( LDQ.LT.1 .OR. ( LCMPQ .AND. LDQ.LT.N ) ) THEN
         INFO = -12
      END IF
      IF( INFO.NE.0) THEN
         CALL XERBLA( 'MB3JZP', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( N.EQ.0 )
     $   RETURN
C
C     A block algorithm is used for large M.
C
      IF( NB.LE.0 ) THEN
         CALL ZGEQRF( M, M, A, LDA, ZWORK, ZWORK, -1, INFO )
         NB = MIN( MAX( INT( ZWORK( 1 ) )/M1, 2 ), M )          
      END IF
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
      DO 180 K = 1, M
         IF(  DBLE( A( K, K ) )* DBLE( B( K, K ) ) +
     $       DIMAG( A( K, K ) )*DIMAG( B( K, K ) ).LT.
     $        -ABS( A( K, K ) )*  ABS( B( K, K ) )*EPS ) THEN
C
            JS = K
            JE = MIN( M, JS+NB-1 )
            IC = 1
            DO 10 J = K - 1, MM + 1, -1
C
C              Perform eigenvalue exchange.
C
               CALL MB03DZ( A( J, J ), LDA, B( J, J ), LDB, CO1, SI1,
     $                      CO2, SI2 )
               DWORK( IC ) =  CO2
               ZWORK( IC ) = -SI2
               IC = IC + 1
C
C              Update A and D.
C
               CALL ZROT( J, A( 1, J+1 ), 1, A( 1, J ), 1, CO1, SI1 )
               A( J, J )     = CO2*A( J, J ) +
     $                         SI2*A( J+1, J+1 )*DCONJG( SI1 )
               A( J+1, J+1 ) = CO1*A( J+1, J+1 )
C
               CJF = -DCONJG( D( J, J+1 ) )
               TMP = CO2*CJF - DCONJG( SI2 )*D( J+1, J+1 )
               CALL ZROT( J, D( 1, J+1 ), 1, D( 1, J ), 1, CO2, SI2 )
               D( J, J )     = CO2*D( J, J )     - SI2*TMP
               D( J+1, J+1 ) = CO2*D( J+1, J+1 ) + SI2*CJF
C
C              Update B and F.
C
               CALL ZROT( J, B( 1, J+1 ), 1, B( 1, J ), 1, CO1, SI1 )
               B( J, J )     = CO2*B( J, J ) +
     $                         SI2*B( J+1, J+1 )*DCONJG( SI1 )
               B( J+1, J+1 ) = CO1*B( J+1, J+1 )
C
               CJF = DCONJG( F( J, J+1 ) )
               TMP = CO2*CJF - DCONJG( SI2 )*F( J+1, J+1 )
               CALL ZROT( J, F( 1, J+1 ), 1, F( 1, J ), 1, CO2, SI2 )
               F( J, J )     = CO2*F( J, J )     - SI2*TMP
               F( J+1, J+1 ) = CO2*F( J+1, J+1 ) + SI2*CJF
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
C
C           Panel Updates.
C
C           Update A.
C
            ICS = 1
            JE  = K - 1
C
C           WHILE( JE.GT.2 ) DO
   20       CONTINUE
            IF( JE.GE.MM+1 ) THEN
               NC  = 0
               IC  = ICS
               ICS = ICS + NB
               DO 30 J = JE, MM + 1, -1
                  NC = MIN( NC+1, NB )
                  JS = JE - NC + 1    
                  CALL ZROT( NC, A( J, JS+1 ), LDA, A( J+1, JS+1 ), LDA,
     $                       DWORK( IC ), ZWORK( IC ) )
                  IC = IC + 1
   30          CONTINUE
               JE = JE - NB
               GO TO 20
            END IF
C           END WHILE 20
C
            DO 50 JS = K, M-1, NB
               JE = MIN( M-1, JS+NB-1 )
               NC = JE - JS + 1
               IC = 1
               DO 40 J = K - 1, MM + 1, -1
                  CALL ZROT( NC, A( J, JS+1 ), LDA, A( J+1, JS+1 ), LDA,
     $                       DWORK( IC ), ZWORK( IC ) )
                  IC = IC + 1
   40          CONTINUE
   50       CONTINUE
C
C           Update B.
C
            ICS = 1
            JE  = K - 1
C
C           WHILE( JE.GT.2 ) DO
   60       CONTINUE
            IF( JE.GE.MM+1 ) THEN
               NC  = 0
               IC  = ICS
               ICS = ICS + NB
               DO 70 J = JE, MM + 1, -1
                  NC = MIN( NC+1, NB )
                  JS = JE - NC + 1              
                  CALL ZROT( NC, B( J, JS+1 ), LDB, B( J+1, JS+1 ), LDB,
     $                       DWORK( IC ), ZWORK( IC ) )
                  IC = IC + 1
   70          CONTINUE
               JE = JE - NB
               GO TO 60
            END IF
C           END WHILE 60
C
            DO 90 JS = K, M-1, NB
               JE = MIN( M-1, JS+NB-1 )
               NC = JE - JS + 1
               IC = 1
               DO 80 J = K - 1, MM + 1, -1
                  CALL ZROT( NC, B( J, JS+1 ), LDB, B( J+1, JS+1 ), LDB,
     $                       DWORK( IC ), ZWORK( IC ) )
                  IC = IC + 1
   80          CONTINUE
   90       CONTINUE
C
C           Update D.
C
            ICS = 1
            JE  = K - 1
C
C           WHILE( JE.GT.2 ) DO
  100       CONTINUE
            IF( JE.GE.MM+1 ) THEN
               NC  = 0
               IC  = ICS
               ICS = ICS + NB
               DO 110 J = JE, MM + 1, -1
                  NC = MIN( NC+1, NB )
                  JS = JE - NC + 1              
                  CALL ZROT( NC, D( J, JS+1 ), LDD, D( J+1, JS+1 ), LDD,
     $                       DWORK( IC ), ZWORK( IC ) )
                  IC = IC + 1
  110          CONTINUE
               JE = JE - NB
               GO TO 100
            END IF
C           END WHILE 100
C
            DO 130 JS = K, M-1, NB
               JE = MIN( M-1, JS+NB-1 )
               NC = JE - JS + 1
               IC = 1
               DO 120 J = K - 1, MM + 1, -1
                  CALL ZROT( NC, D( J, JS+1 ), LDD, D( J+1, JS+1 ), LDD,
     $                       DWORK( IC ), ZWORK( IC ) )
                  IC = IC + 1
  120          CONTINUE
  130       CONTINUE
C
C           Update F.
C
            ICS = 1
            JE  = K - 1
C
C           WHILE( JE.GT.2 ) DO
  140       CONTINUE
            IF( JE.GE.MM+1 ) THEN
               NC  = 0
               IC  = ICS
               ICS = ICS + NB
               DO 150 J = JE, MM + 1, -1
                  NC = MIN( NC+1, NB )
                  JS = JE - NC + 1              
                  CALL ZROT( NC, F( J, JS+1 ), LDF, F( J+1, JS+1 ), LDF,
     $                       DWORK( IC ), ZWORK( IC ) )
                  IC = IC + 1
  150          CONTINUE
               JE = JE - NB
               GO TO 140
            END IF
C           END WHILE 140
C
            DO 170 JS = K, M-1, NB
               JE = MIN( M-1, JS+NB-1 )
               NC = JE - JS + 1
               IC = 1
               DO 160 J = K - 1, MM + 1, -1
                  CALL ZROT( NC, F( J, JS+1 ), LDF, F( J+1, JS+1 ), LDF,
     $                       DWORK( IC ), ZWORK( IC ) )
                  IC = IC + 1
  160          CONTINUE
  170       CONTINUE
C
            MM = MM + 1
         END IF
  180 CONTINUE
C
C     II. Reorder the eigenvalues with positive real parts to the bottom.
C
      DO 280 K = M, MM + 1, -1
         IF(  DBLE( A( K, K ) )* DBLE( B( K, K ) ) +
     $       DIMAG( A( K, K ) )*DIMAG( B( K, K ) ).GT.
     $         ABS( A( K, K ) )*  ABS( B( K, K ) )*EPS ) THEN
C
            IC = 1
            DO 190 J = K, MP - 2
C
C              Perform eigenvalue exchange.
C
               CALL MB03DZ( A( J, J ), LDA, B( J, J ), LDB, CO1, SI1,
     $                      CO2, SI2 )
               DWORK( IC ) =  CO2
               ZWORK( IC ) = -SI2
               IC = IC + 1
C
C              Update A and D.
C
               CALL ZROT( J, A( 1, J+1 ), 1, A( 1, J ), 1, CO1, SI1 )
               A( J, J )     = CO2*A( J, J ) +
     $                         SI2*A( J+1, J+1 )*DCONJG( SI1 )
               A( J+1, J+1 ) = CO1*A( J+1, J+1 )
               CALL ZROT( MP-J-1, A( J, J+1 ), LDA, A( J+1, J+1 ), LDA,
     $                    CO2, -SI2 )
C
               CJF = -DCONJG( D( J, J+1 ) )
               TMP = CO2*CJF - DCONJG( SI2 )*D( J+1, J+1 )
               CALL ZROT( J, D( 1, J+1 ), 1, D( 1, J ), 1, CO2, SI2 )
               D( J, J )     = CO2*D( J, J )     - SI2*TMP
               D( J+1, J+1 ) = CO2*D( J+1, J+1 ) + SI2*CJF
               CALL ZROT( MP-J-1, D( J, J+1 ), LDD, D( J+1, J+1 ), LDD,
     $                    CO2, -SI2 )
C
C              Update B and F.
C
               CALL ZROT( J, B( 1, J+1 ), 1, B( 1, J ), 1, CO1, SI1 )
               B( J, J )     = CO2*B( J, J ) +
     $                         SI2*B( J+1, J+1 )*DCONJG( SI1 )
               B( J+1, J+1 ) = CO1*B( J+1, J+1 )
               CALL ZROT( MP-J-1, B( J, J+1 ), LDB, B( J+1, J+1 ), LDB,
     $                    CO2, -SI2 )
C
               CJF = DCONJG( F( J, J+1 ) )
               TMP = CO2*CJF - DCONJG( SI2 )*F( J+1, J+1 )
               CALL ZROT( J, F( 1, J+1 ), 1, F( 1, J ), 1, CO2, SI2 )
               F( J, J )     = CO2*F( J, J )     - SI2*TMP
               F( J+1, J+1 ) = CO2*F( J+1, J+1 ) + SI2*CJF
               CALL ZROT( MP-J-1, F( J, J+1 ), LDF, F( J+1, J+1 ), LDF,
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
  190       CONTINUE
C
C           Panel Updates.
C
C           Update A.
C
            ICS = 1
            DO 210 JS = MP, M, NB
               IC = ICS
               JE = MIN( JS+NB, M )
               NC = MIN( NB, JE-JS+1 )
               DO 200 J = K, MP-2
                  CALL ZROT( NC, A( J, JS ), LDA, A( J+1, JS ),
     $                       LDA, DWORK( IC ), ZWORK( IC ) )
                  IC = IC + 1
  200          CONTINUE
  210       CONTINUE
C
C           Update D.
C
            ICS = 1
            DO 230 JS = MP, M, NB
               IC = ICS
               JE = MIN( JS+NB, M )
               NC = MIN( NB, JE-JS+1 )
               DO 220 J = K, MP-2
                  CALL ZROT( NC, D( J, JS ), LDD, D( J+1, JS ),
     $                       LDD, DWORK( IC ), ZWORK( IC ) )
                  IC = IC + 1
  220          CONTINUE
  230       CONTINUE
C
C           Update B.
C
            ICS = 1
            DO 250 JS = MP, M, NB
               IC = ICS
               JE = MIN( JS+NB, M )
               NC = MIN( NB, JE-JS+1 )
               DO 240 J = K, MP-2
                  CALL ZROT( NC, B( J, JS ), LDB, B( J+1, JS ),
     $                       LDB, DWORK( IC ), ZWORK( IC ) )
                  IC = IC + 1
  240          CONTINUE
  250       CONTINUE
C
C           Update F.
C
            ICS = 1
            DO 270 JS = MP, M, NB
               IC = ICS
               JE = MIN( JS+NB, M )
               NC = MIN( NB, JE-JS+1 )
               DO 260 J = K, MP-2
                  CALL ZROT( NC, F( J, JS ), LDF, F( J+1, JS ),
     $                       LDF, DWORK( IC ), ZWORK( IC ) )
                  IC = IC + 1
  260          CONTINUE
  270       CONTINUE
C
            MP = MP - 1
         END IF
  280 CONTINUE
C
C
C     The remaining M-MP+1 eigenvalues with negative real part are now in
C     the bottom right subpencil of aS - bH.
C
C     STEP 2. Reorder the remaining M-MP+1 eigenvalues.
C
      DO 380 K = M, MP, -1
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
         IC = 1
         DO 290 J = M - 1, MM, -1
C
C           Perform eigenvalue exchange.
C
            CALL MB03DZ( A( J, J ), LDA, B( J, J ), LDB, CO1, SI1, CO2,
     $                   SI2 )
            DWORK( IC ) =  CO2
            ZWORK( IC ) = -SI2
            IC = IC + 1
C
C           Update A and D.
C
            CALL ZROT( J, A( 1, J+1 ), 1, A( 1, J ), 1, CO1, SI1 )
            A( J, J )     = CO2*A( J, J ) +
     $                      SI2*A( J+1, J+1 )*DCONJG( SI1 )
            A( J+1, J+1 ) = CO1*A( J+1, J+1 )
C
            CJF = -DCONJG( D( J, J+1 ) )
            TMP = CO2*CJF - DCONJG( SI2 )*D( J+1, J+1 )
            CALL ZROT( J, D( 1, J+1 ), 1, D( 1, J ), 1, CO2, SI2 )
            D( J, J )     = CO2*D( J, J )     - SI2*TMP
            D( J+1, J+1 ) = CO2*D( J+1, J+1 ) + SI2*CJF
C
C           Update B and F.
C
            CALL ZROT( J, B( 1, J+1 ), 1, B( 1, J ), 1, CO1, SI1 )
            B( J, J )     = CO2*B( J, J ) +
     $                      SI2*B( J+1, J+1 )*DCONJG( SI1 )
            B( J+1, J+1 ) = CO1*B( J+1, J+1 )
C
            CJF = DCONJG( F( J, J+1 ) )
            TMP = CO2*CJF - DCONJG( SI2 )*F( J+1, J+1 )
            CALL ZROT( J, F( 1, J+1 ), 1, F( 1, J ), 1, CO2, SI2 )
            F( J, J )     = CO2*F( J, J )     - SI2*TMP
            F( J+1, J+1 ) = CO2*F( J+1, J+1 ) + SI2*CJF
C
            IF( LCMPQ ) THEN
C
C              Update Q.
C
               CALL ZROT( N, Q( 1, J+1 ), 1, Q( 1, J ), 1, CO1, SI1 )
               CALL ZROT( N, Q( 1, M+J+1 ), 1, Q( 1, M+J ), 1, CO2, SI2
     $                     )
            END IF
  290    CONTINUE
C
C        Panel Updates.
C
C        Update A.
C
         ICS = 1
         JE  = M - 1
C
C        WHILE( JE.GT.2 ) DO
  300    CONTINUE
         IF( JE.GE.MM ) THEN
            NC  = 0
            IC  = ICS
            ICS = ICS + NB
            DO 310 J = JE, MM, -1
               NC = MIN( NC+1, NB )
               JS = JE - NC + 1           
               CALL ZROT( NC, A( J, JS+1 ), LDA, A( J+1, JS+1 ), LDA,
     $                    DWORK( IC ), ZWORK( IC ) )
               IC = IC + 1
  310       CONTINUE
            JE = JE - NB
            GO TO 300
         END IF
C        END WHILE 300
C
C        Update B.
C
         ICS = 1
         JE  = M - 1
C
C        WHILE( JE.GT.2 ) DO
  320    CONTINUE
         IF( JE.GE.MM ) THEN
            NC  = 0
            IC  = ICS
            ICS = ICS + NB
            DO 330 J = JE, MM, -1
               NC = MIN( NC+1, NB )
               JS = JE - NC + 1            
               CALL ZROT( NC, B( J, JS+1 ), LDB, B( J+1, JS+1 ), LDB,
     $                    DWORK( IC ), ZWORK( IC ) )
               IC = IC + 1
  330       CONTINUE
            JE = JE - NB
            GO TO 320
         END IF
C        END WHILE 320
C
C        Update D.
C
         ICS = 1
         JE  = M - 1
C
C        WHILE( JE.GT.2 ) DO
  340    CONTINUE
         IF( JE.GE.MM ) THEN
            NC  = 0
            IC  = ICS
            ICS = ICS + NB
            DO 350 J = JE, MM, -1
               NC = MIN( NC+1, NB )
               JS = JE - NC + 1            
               CALL ZROT( NC, D( J, JS+1 ), LDD, D( J+1, JS+1 ), LDD,
     $                    DWORK( IC ), ZWORK( IC ) )
               IC = IC + 1
  350       CONTINUE
            JE = JE - NB
            GO TO 340
         END IF
C        END WHILE 340
C
C        Update F.
C
         ICS = 1
         JE  = M - 1
C
C        WHILE( JE.GT.2 ) DO
  360    CONTINUE
         IF( JE.GE.MM ) THEN
            NC  = 0
            IC  = ICS
            ICS = ICS + NB
            DO 370 J = JE, MM, -1
               NC = MIN( NC+1, NB )
               JS = JE - NC + 1            
               CALL ZROT( NC, F( J, JS+1 ), LDF, F( J+1, JS+1 ), LDF,
     $                    DWORK( IC ), ZWORK( IC ) )
               IC = IC + 1
  370       CONTINUE
            JE = JE - NB
            GO TO 360
         END IF
C        END WHILE 360
C
  380 CONTINUE
C
      NEIG = MM
C
      RETURN
C *** Last line of MB3JZP ***
      END
