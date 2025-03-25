      SUBROUTINE MB03IZ( COMPQ, COMPU, N, A, LDA, C, LDC, D, LDD, B,
     $                   LDB, F, LDF, Q, LDQ, U1, LDU1, U2, LDU2, NEIG,
     $                   TOL, INFO )
C
C     PURPOSE
C
C     To move the eigenvalues with strictly negative real parts of an
C     N-by-N complex skew-Hamiltonian/Hamiltonian pencil aS - bH in
C     structured Schur form, with
C
C                                (  0  I  )
C       S = J Z' J' Z, where J = (        ),
C                                ( -I  0  )
C
C     to the leading principal subpencil, while keeping the triangular
C     form. On entry, we have
C
C           (  A  D  )      (  B  F  )
C       Z = (        ), H = (        ),
C           (  0  C  )      (  0 -B' )
C
C     where A and B are upper triangular and C is lower triangular.
C     Z and H are transformed by a unitary symplectic matrix U and a
C     unitary matrix Q such that
C
C                       (  Aout  Dout  )
C       Zout = U' Z Q = (              ), and
C                       (    0   Cout  )
C                                                                    (1)
C                            (  Bout  Fout  )
C       Hout = J Q' J' H Q = (              ), 
C                            (    0  -Bout' )
C
C     where Aout, Bout and Cout remain in triangular form. The notation
C     M' denotes the conjugate transpose of the matrix M.
C     Optionally, if COMPQ = 'I' or COMPQ = 'U', the unitary matrix Q
C     that fulfills (1) is computed.
C     Optionally, if COMPU = 'I' or COMPU = 'U', the unitary symplectic
C     matrix 
C
C           (  U1  U2  )
C       U = (          )
C           ( -U2  U1  )   
C
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
C     COMPU   CHARACTER*1
C             Specifies whether or not the unitary symplectic
C             transformations should be accumulated in the arrays U1 and
C             U2, as follows:
C             = 'N':  U1 and U2 are not computed;
C             = 'I':  the arrays U1 and U2 are initialized internally,
C                     and the submatrices U1 and U2 defining the
C                     unitary symplectic matrix U are returned;
C             = 'U':  the arrays U1 and U2 contain the corresponding
C                     submatrices of a unitary symplectic matrix U0
C                     on entry, and the updated submatrices U1 and U2
C                     of the matrix product U0*U are returned, where U
C                     is the product of the unitary symplectic
C                     transformations that are applied to the pencil
C                     aS - bH to reorder the eigenvalues.
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
C     C       (input/output) COMPLEX*16 array, dimension (LDC, N/2)
C             On entry, the leading N/2-by-N/2 part of this array must
C             contain the lower triangular matrix C.
C             On exit, the leading  N/2-by-N/2 part of this array
C             contains the transformed matrix Cout.
C             The strictly upper triangular part of this array is not
C             referenced.
C
C     LDC     INTEGER
C             The leading dimension of the array C.  LDC >= MAX(1, N/2).
C
C     D       (input/output) COMPLEX*16 array, dimension (LDD, N/2)
C             On entry, the leading N/2-by-N/2 part of this array must
C             contain the matrix D.
C             On exit, the leading  N/2-by-N/2 part of this array
C             contains the transformed matrix Dout.
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
C     U1      (input/output) COMPLEX*16 array, dimension (LDU1, N/2)
C             On entry, if COMPU = 'U', then the leading N/2-by-N/2 part
C             of this array must contain the upper left block of a
C             given matrix U0, and on exit, the leading N/2-by-N/2 part
C             of this array contains the updated upper left block U1 of
C             the product of the input matrix U0 and the transformation
C             matrix U used to transform the matrices S and H.
C             On exit, if COMPU = 'I', then the leading N/2-by-N/2 part
C             of this array contains the upper left block U1 of the
C             unitary symplectic transformation matrix U.
C             If COMPU = 'N' this array is not referenced.
C
C     LDU1    INTEGER
C             The leading dimension of the array U1.
C             LDU1 >= 1,           if COMPU = 'N';
C             LDU1 >= MAX(1, N/2), if COMPU = 'I' or COMPU = 'U'.
C
C     U2      (input/output) COMPLEX*16 array, dimension (LDU2, N/2)
C             On entry, if COMPU = 'U', then the leading N/2-by-N/2 part
C             of this array must contain the upper right block of a
C             given matrix U0, and on exit, the leading N/2-by-N/2 part
C             of this array contains the updated upper right block U2 of
C             the product of the input matrix U0 and the transformation
C             matrix U used to transform the matrices S and H.
C             On exit, if COMPU = 'I', then the leading N/2-by-N/2 part
C             of this array contains the upper right block U2 of the
C             unitary symplectic transformation matrix U.
C             If COMPU = 'N' this array is not referenced.
C
C     LDU2    INTEGER
C             The leading dimension of the array U2.
C             LDU2 >= 1,           if COMPU = 'N';
C             LDU2 >= MAX(1, N/2), if COMPU = 'I' or COMPU = 'U'.
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
C     Step 1: Reorder the eigenvalues in the subpencil aC'*A - bB.
C          I. Reorder the eigenvalues with negative real parts to the
C             top.
C         II. Reorder the eigenvalues with positive real parts to the
C             bottom.
C
C     Step 2: Reorder the remaining eigenvalues with negative real
C             parts.
C          I. Exchange the eigenvalues between the last diagonal block
C             in aC'*A - bB and the last diagonal block in aS - bH.
C         II. Move the eigenvalues in the N/2-th place to the (MM+1)-th
C             place, where MM denotes the current number of eigenvalues
C             with negative real parts in aC'*A - bB.
C
C     The algorithm uses a sequence of unitary transformations as
C     described on page 38 in [1]. To achieve those transformations the
C     elementary SLICOT Library subroutines MB03CZ and MB03GZ are called
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
C     Chemnitz, April 29, 2009.
C     V. Sima, Aug. 2009 (SLICOT version of the routine ZHAFNX).
C
C     REVISIONS
C
C     V. Sima, Dec. 2010, Jan. 2011.
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
      PARAMETER          ( CZERO = ( 0.0D+0, 0.0D+0 ),
     $                     CONE  = ( 1.0D+0, 0.0D+0 ) )
C
C     .. Scalar Arguments ..
      CHARACTER          COMPQ, COMPU
      INTEGER            INFO, LDA, LDB, LDC, LDD, LDF, LDQ, LDU1, LDU2,
     $                   N, NEIG
      DOUBLE PRECISION   TOL
C
C     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), B( LDB, * ), C( LDC, * ),
     $                   D( LDD, * ), F( LDF, * ), Q( LDQ, * ),
     $                   U1( LDU1, * ), U2( LDU2, * )
C
C     .. Local Scalars ..
      LOGICAL            LCMPQ, LCMPU, LINIQ, LINIU, LUPDQ, LUPDU
      INTEGER            IUPD, J, K, M, MM, MP, UPDS
      DOUBLE PRECISION   CO1, CO2, CO3, EPS, NRMA, NRMB
      COMPLEX*16         CJF, SI1, SI2, SI3, TMP
C
C     .. Local Arrays ..
      DOUBLE PRECISION   DUM( 1 )
      COMPLEX*16         HLP( 2, 2 )
C
C     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, ZLANTR
      EXTERNAL           DLAMCH, LSAME, ZLANTR
C
C     .. External Subroutines ..
      EXTERNAL           MB03CZ, MB03GZ, XERBLA, ZLASET, ZROT
C
C     .. Intrinsic Functions ..
      INTRINSIC          DBLE, DCONJG, MAX, MIN, MOD
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
      LINIU = LSAME( COMPU, 'I' )
      LUPDU = LSAME( COMPU, 'U' )
      LCMPU = LINIU .OR. LUPDU
C
C     Test the input arguments.
C
      INFO = 0
      IF( .NOT.( LSAME( COMPQ, 'N' ) .OR. LCMPQ ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( LSAME( COMPU, 'N' ) .OR. LCMPU ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 .OR. MOD( N, 2 ).NE.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -7
      ELSE IF( LDD.LT.MAX( 1, M ) ) THEN
         INFO = -9
      ELSE IF( LDB.LT.MAX( 1, M ) ) THEN
         INFO = -11
      ELSE IF( LDF.LT.MAX( 1, M ) ) THEN
         INFO = -13
      ELSE IF( LDQ.LT.1  .OR. ( LCMPQ .AND. LDQ.LT.N  ) ) THEN
         INFO = -15
      ELSE IF( LDU1.LT.1 .OR. ( LCMPU .AND. LDU1.LT.M ) ) THEN
         INFO = -17
      ELSE IF( LDU2.LT.1 .OR. ( LCMPU .AND. LDU2.LT.M ) ) THEN
         INFO = -19
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'MB03IZ', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( N.EQ.0 ) THEN
         RETURN
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
      IF( LINIU ) THEN
         CALL ZLASET( 'Full', M, M, CZERO, CONE,  U1, LDU1 )
         CALL ZLASET( 'Full', M, M, CZERO, CZERO, U2, LDU2 )
      END IF
C                                                        
C     STEP 1. Reorder the eigenvalues in the subpencil aC'*A - bB.
C
      MM = 0
      MP = M + 1
      NRMA = ZLANTR( 'One', 'Upper', 'Non-Unit', M, M, A, LDA, DUM )*
     $       ZLANTR( 'One', 'Lower', 'Non-Unit', M, M, C, LDC, DUM )
      NRMB = ZLANTR( 'One', 'Upper', 'Non-Unit', M, M, B, LDB, DUM )
C
C     I. Reorder the eigenvalues with negative real parts to the top.
C
      DO 20 K = 1, M
         IF( DBLE( B( K, K )*C( K, K )*DCONJG( A( K, K ) ) )*NRMB.LE.
     $      -EPS*NRMA ) THEN
            DO 10 J = K - 1, MM + 1, -1
C
C              Perform eigenvalue exchange.
C
               HLP( 1, 1 ) = DCONJG( C( J, J ) )
               HLP( 1, 2 ) = DCONJG( C( J+1, J ) )
               HLP( 2, 2 ) = DCONJG( C( J+1, J+1 ) )
C
               CALL MB03CZ( HLP, 2, A( J, J ), LDA, B( J, J ),
     $                      LDB, CO1, SI1, CO2, SI2, CO3, SI3 )
C
C              Update A, C, and D.
C
               CALL ZROT( J, A( 1, J+1 ), 1, A( 1, J ), 1, CO1, SI1 )
               A( J, J )     = CO2*A( J, J ) +
     $                         SI2*A( J+1, J+1 )*DCONJG( SI1 )
               A( J+1, J+1 ) = CO1*A( J+1, J+1 )
               CALL ZROT( M-J, A( J, J+1 ), LDA, A( J+1, J+1 ), LDA,
     $                    CO2, -SI2 )
C
               CALL ZROT( M, D( 1, J+1 ), 1, D( 1, J ), 1, CO3, SI3 )
               CALL ZROT( M, D( J, 1 ), LDD, D( J+1, 1 ), LDD, CO2, -SI2
     $                     )
C
               CALL ZROT( M-J, C( J+1, J+1 ), 1, C( J+1, J ), 1, CO3,
     $                    SI3 )
               C( J+1, J+1 ) = CO2*C( J+1, J+1 ) +
     $                         SI3*C( J, J )*DCONJG( SI2 )
               C( J, J )     = CO3*C( J, J )
               CALL ZROT( J, C( J, 1 ), LDC, C( J+1, 1 ), LDC, CO2,
     $                    -SI2 )
C
C              Update B and F.
C
               CALL ZROT( J, B( 1, J+1 ), 1, B( 1, J ), 1, CO1, SI1 )
               B( J, J )     = CO3*B( J, J ) +
     $                         SI3*B( J+1, J+1 )*DCONJG( SI1 )
               B( J+1, J+1 ) = CO1*B( J+1, J+1 )
               CALL ZROT( M-J, B( J, J+1 ), LDB, B( J+1, J+1 ), LDB,
     $                    CO3, -SI3 )
C
               CJF = DCONJG( F( J, J+1 ) )
               TMP = CO3*CJF - DCONJG( SI3 )*F( J+1, J+1 )
               CALL ZROT( J, F( 1, J+1 ), 1, F( 1, J ), 1, CO3, SI3 )
               F( J, J )     = CO3*F( J, J )     - SI3*TMP
               F( J+1, J+1 ) = CO3*F( J+1, J+1 ) + SI3*CJF
               CALL ZROT( M-J, F( J, J+1 ), LDF, F( J+1, J+1 ), LDF,
     $                    CO3, -SI3 )
C
               IF( LCMPQ ) THEN
C
C                 Update Q.
C
                  CALL ZROT( UPDS, Q( 1, J+1 ), 1, Q( 1, J ), 1, CO1,
     $                       SI1 )
                  CALL ZROT( UPDS, Q( IUPD, M+J+1 ), 1, Q( IUPD, M+J ),
     $                       1, CO3, SI3 )
               END IF
C
               IF( LCMPU ) THEN
C
C                 Update U.
C
                  CALL ZROT( M, U1( 1, J+1 ), 1, U1( 1, J ), 1, CO2, SI2
     $                        )
                  IF( LUPDU ) THEN
                     CALL ZROT( M, U2( 1, J+1 ), 1, U2( 1, J ), 1, CO2,
     $                          SI2 )
                  END IF
               END IF
   10       CONTINUE
            MM = MM + 1
         END IF
   20 CONTINUE
C
C     II. Reorder the eigenvalues with positive real parts to the bottom.
C
      DO 40 K = M, MM + 1, -1
         IF( DBLE( B( K, K )*C( K, K )*DCONJG( A( K, K ) ) )*NRMB.GE.
     $       EPS*NRMA ) THEN
            DO 30 J = K, MP - 2
C
C              Perform eigenvalue exchange.
C
               HLP( 1, 1 ) = DCONJG( C( J, J ) )
               HLP( 1, 2 ) = DCONJG( C( J+1, J ) )
               HLP( 2, 2 ) = DCONJG( C( J+1, J+1 ) )
C
               CALL MB03CZ( HLP, 2, A( J, J ), LDA, B( J, J ),
     $                      LDB, CO1, SI1, CO2, SI2, CO3, SI3 )
C
C              Update A, C, and D.
C
               CALL ZROT( J, A( 1, J+1 ), 1, A( 1, J ), 1, CO1, SI1 )
               A( J, J )     = CO2*A( J, J ) +
     $                         SI2*A( J+1, J+1 )*DCONJG( SI1 )
               A( J+1, J+1 ) = CO1*A( J+1, J+1 )
               CALL ZROT( M-J, A( J, J+1 ), LDA, A( J+1, J+1 ), LDA,
     $                    CO2, -SI2 )
C
               CALL ZROT( M, D( 1, J+1 ), 1, D( 1, J ), 1, CO3, SI3 )
               CALL ZROT( M, D( J, 1 ), LDD, D( J+1, 1 ), LDD, CO2, -SI2
     $                     )
C
               CALL ZROT( M-J, C( J+1, J+1 ), 1, C( J+1, J ), 1, CO3,
     $                    SI3 )
               C( J+1, J+1 ) = CO2*C( J+1, J+1 ) +
     $                         SI3*C( J, J )*DCONJG( SI2 )
               C( J, J )     = CO3*C( J, J )
               CALL ZROT( J, C( J, 1 ), LDC, C( J+1, 1 ), LDC, CO2,
     $                    -SI2 )
C
C              Update B and F.
C
               CALL ZROT( J, B( 1, J+1 ), 1, B( 1, J ), 1, CO1, SI1 )
               B( J, J )     = CO3*B( J, J ) +
     $                         SI3*B( J+1, J+1 )*DCONJG( SI1 )
               B( J+1, J+1 ) = CO1*B( J+1, J+1 )
               CALL ZROT( M-J, B( J, J+1 ), LDB, B( J+1, J+1 ), LDB,
     $                    CO3, -SI3 )
C
               CJF = DCONJG( F( J, J+1 ) )
               TMP = CO3*CJF - DCONJG( SI3 )*F( J+1, J+1 )
               CALL ZROT( J, F( 1, J+1 ), 1, F( 1, J ), 1, CO3, SI3 )
               F( J, J )     = CO3*F( J, J )     - SI3*TMP
               F( J+1, J+1 ) = CO3*F( J+1, J+1 ) + SI3*CJF
               CALL ZROT( M-J, F( J, J+1 ), LDF, F( J+1, J+1 ), LDF,
     $                    CO3, -SI3 )
C
               IF( LCMPQ ) THEN
C
C                 Update Q.
C
                  CALL ZROT( UPDS, Q( 1, J+1 ), 1, Q( 1, J ), 1, CO1,
     $                       SI1 )
                  CALL ZROT( UPDS, Q( IUPD, M+J+1 ), 1, Q( IUPD, M+J ),
     $                       1, CO3, SI3 )
               END IF
C
               IF( LCMPU ) THEN
C
C                 Update U.
C
                  CALL ZROT( M, U1( 1, J+1 ), 1, U1( 1, J ), 1, CO2, SI2
     $                        )
                  IF( LUPDU ) THEN
                     CALL ZROT( M, U2( 1, J+1 ), 1, U2( 1, J ), 1, CO2,
     $                          SI2 )
                  END IF
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
         CALL MB03GZ( A( M, M ), D( M, M ), C( M, M ), B( M, M ),
     $                F( M, M ), CO1, SI1, CO2, SI2 )
C
C        Update A, C, and D.
C
         CALL ZROT( M, D( 1, M ), 1, A( 1, M ), 1, CO1, SI1 )
         TMP = -DCONJG( SI1 )*C( M, M )
         C( M, M ) = CO1*C( M, M )
         CALL ZROT( M, D( M, 1 ), LDD, C( M, 1 ), LDC, CO2,
     $              -DCONJG( SI2 ) )
         A( M, M ) = CO2*A( M, M ) - DCONJG( SI2 )*TMP
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
         IF( LCMPU ) THEN
C
C           Update U.
C
            CALL ZROT( M, U2( 1, M ), 1, U1( 1, M ), 1, CO2, SI2 )
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
            HLP( 1, 1 ) = DCONJG( C( J, J ) )
            HLP( 1, 2 ) = DCONJG( C( J+1, J ) )
            HLP( 2, 2 ) = DCONJG( C( J+1, J+1 ) )
C
            CALL MB03CZ( HLP, 2, A( J, J ), LDA, B( J, J ), LDB, CO1,
     $                   SI1, CO2, SI2, CO3, SI3 )
C
C           Update A, C, and D.
C
            CALL ZROT( J, A( 1, J+1 ), 1, A( 1, J ), 1, CO1, SI1 )
            A( J, J )     = CO2*A( J, J ) +
     $                      SI2*A( J+1, J+1 )*DCONJG( SI1 )
            A( J+1, J+1 ) = CO1*A( J+1, J+1 )
            CALL ZROT( M-J, A( J, J+1 ), LDA, A( J+1, J+1 ), LDA, CO2,
     $                 -SI2 )
C
            CALL ZROT( M, D( 1, J+1 ), 1, D( 1, J ), 1, CO3, SI3 )
            CALL ZROT( M, D( J, 1 ), LDD, D( J+1, 1 ), LDD, CO2, -SI2 )
C
            CALL ZROT( M-J, C( J+1, J+1 ), 1, C( J+1, J ), 1, CO3, SI3 )
            C( J+1, J+1 ) = CO2*C( J+1, J+1 ) +
     $                      SI3*C( J, J )*DCONJG( SI2 )
            C( J, J )     = CO3*C( J, J )
            CALL ZROT( J, C( J, 1 ), LDC, C( J+1, 1 ), LDC, CO2, -SI2 )
C
C           Update B and F.
C
            CALL ZROT( J, B( 1, J+1 ), 1, B( 1, J ), 1, CO1, SI1 )
            B( J, J )     = CO3*B( J, J ) +
     $                      SI3*B( J+1, J+1 )*DCONJG( SI1 )
            B( J+1, J+1 ) = CO1*B( J+1, J+1 )
            CALL ZROT( M-J, B( J, J+1 ), LDB, B( J+1, J+1 ), LDB, CO3,
     $                 -SI3 )
C
            CJF = DCONJG( F( J, J+1 ) )
            TMP = CO3*CJF - DCONJG( SI3 )*F( J+1, J+1 )
            CALL ZROT( J, F( 1, J+1 ), 1, F( 1, J ), 1, CO3, SI3 )
            F( J, J )     = CO3*F( J, J )     - SI3*TMP
            F( J+1, J+1 ) = CO3*F( J+1, J+1 ) + SI3*CJF
            CALL ZROT( M-J, F( J, J+1 ), LDF, F( J+1, J+1 ), LDF, CO3,
     $                 -SI3 )
C
            IF( LCMPQ ) THEN
C
C              Update Q.
C
               CALL ZROT( N, Q( 1, J+1 ), 1, Q( 1, J ), 1, CO1, SI1 )
               CALL ZROT( N, Q( 1, M+J+1 ), 1, Q( 1, M+J ), 1, CO3, SI3
     $                     )
            END IF
C
            IF( LCMPU ) THEN
C
C              Update U.
C
               CALL ZROT( M, U1( 1, J+1 ), 1, U1( 1, J ), 1, CO2, SI2 )
               CALL ZROT( M, U2( 1, J+1 ), 1, U2( 1, J ), 1, CO2, SI2 )
            END IF
   50    CONTINUE
   60 CONTINUE
C
      NEIG = MM
C
      RETURN
C *** Last line of MB03IZ ***
      END
