      SUBROUTINE MB03CD( UPLO, N1, N2, PREC, A, LDA, B, LDB, D, LDD, Q1,
     $                   LDQ1, Q2, LDQ2, Q3, LDQ3, DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To compute orthogonal matrices Q1, Q2, Q3 for a real 2-by-2,
C     3-by-3, or 4-by-4 regular block upper triangular pencil
C
C                    ( A11 A12 ) ( B11 B12 )     ( D11 D12 )
C       aAB - bD = a (         ) (         ) - b (         ),        (1)
C                    (  0  A22 ) (  0  B22 )     (  0  D22 )
C
C     such that the pencil a(Q3' A Q2 )(Q2' B Q1 ) - b(Q3' D Q1) is
C     still in block upper triangular form, but the eigenvalues in
C     Spec(A11 B11, D11), Spec(A22 B22, D22) are exchanged, where
C     Spec(X,Y) denotes the spectrum of the matrix pencil (X,Y), and M'
C     denotes the transpose of the matrix M.
C
C     Optionally, to upper triangularize the real regular pencil in
C     block lower triangular form
C
C                  ( A11  0  ) ( B11  0  )     ( D11  0  )
C     aAB - bD = a (         ) (         ) - b (         ),          (2)
C                  ( A21 A22 ) ( B21 B22 )     ( D21 D22 )
C
C     while keeping the eigenvalues in the same diagonal position.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     UPLO    CHARACTER*1
C             Specifies if the pencil is in lower or upper block
C             triangular form on entry, as follows:
C             = 'U': Upper block triangular, eigenvalues are exchanged
C                    on exit;
C             = 'L': Lower block triangular, eigenvalues are not
C                    exchanged on exit.
C
C     Input/Output Parameters
C
C     N1      (input/output) INTEGER
C             Size of the upper left block, N1 <= 2.
C             If UPLO = 'U' and INFO = 0, or UPLO = 'L' and INFO <> 0,
C             N1 and N2 are exchanged on exit; otherwise, N1 is
C             unchanged on exit.
C
C     N2      (input/output) INTEGER
C             Size of the lower right block, N2 <= 2.
C             If UPLO = 'U' and INFO = 0, or UPLO = 'L' and INFO <> 0,
C             N1 and N2 are exchanged on exit; otherwise, N2 is
C             unchanged on exit.
C
C     PREC    (input) DOUBLE PRECISION
C             The machine precision, (relative machine precision)*base.
C             See the LAPACK Library routine DLAMCH.
C
C     A       (input or input/output) DOUBLE PRECISION array, dimension
C                (LDA, N1+N2)
C             On entry, the leading (N1+N2)-by-(N1+N2) part of this
C             array must contain the matrix A of the pencil aAB - bD.
C             The (2,1) block, if UPLO = 'U', or the (1,2) block, if
C             UPLO = 'L', need not be set to zero.
C             On exit, if N1 = N2 = 1, this array contains the matrix
C                               [  0 1 ]
C             J' A J, where J = [ -1 0 ]; otherwise, this array is
C             unchanged on exit.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= N1+N2.
C
C     B       (input or input/output) DOUBLE PRECISION array, dimension
C                (LDB, N1+N2)
C             On entry, the leading (N1+N2)-by-(N1+N2) part of this
C             array must contain the matrix B of the pencil aAB - bD.
C             The (2,1) block, if UPLO = 'U', or the (1,2) block, if
C             UPLO = 'L', need not be set to zero.
C             On exit, if N1 = N2 = 1, this array contains the matrix
C             J' B J; otherwise, this array is unchanged on exit.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= N1+N2.
C
C     D       (input/output) DOUBLE PRECISION array, dimension
C                (LDD, N1+N2)
C             On entry, the leading (N1+N2)-by-(N1+N2) part of this
C             array must contain the matrix D of the pencil aAB - bD.
C             On exit, if N1 = 2 or N2 = 2, the leading
C             (N1+N2)-by-(N1+N2) part of this array contains the
C             transformed matrix D in real Schur form. If N1 = 1 and
C             N2 = 1, this array contains the matrix J' D J.
C
C     LDD     INTEGER
C             The leading dimension of the array D.  LDD >= N1+N2.
C
C     Q1      (output) DOUBLE PRECISION array, dimension (LDQ1, N1+N2)
C             The leading (N1+N2)-by-(N1+N2) part of this array contains
C             the first orthogonal transformation matrix.
C
C     LDQ1    INTEGER
C             The leading dimension of the array Q1.  LDQ1 >= N1+N2.
C
C     Q2      (output) DOUBLE PRECISION array, dimension (LDQ2, N1+N2)
C             The leading (N1+N2)-by-(N1+N2) part of this array contains
C             the second orthogonal transformation matrix.
C
C     LDQ2    INTEGER
C             The leading dimension of the array Q2.  LDQ2 >= N1+N2.
C
C     Q3      (output) DOUBLE PRECISION array, dimension (LDQ3, N1+N2)
C             The leading (N1+N2)-by-(N1+N2) part of this array contains
C             the third orthogonal transformation matrix.
C
C     LDQ3    INTEGER
C             The leading dimension of the array Q3.  LDQ3 >= N1+N2.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             If N1+N2 = 2 then DWORK is not referenced.
C
C     LDWORK  INTEGER
C             The dimension of the array DWORK.
C             If N1+N2 = 2, then LDWORK = 0; otherwise,
C             LDWORK >= 16*N1 + 10*N2 + 23, UPLO = 'U';
C             LDWORK >= 10*N1 + 16*N2 + 23, UPLO = 'L'.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0: succesful exit;
C             = 1: the QZ iteration failed in the LAPACK routine DGGEV;
C             = 2: another error occured while executing a routine in
C                  DGGEV;
C             = 3: the QZ iteration failed in the LAPACK routine DGGES;
C             = 4: another error occured during execution of DGGES;
C             = 5: reordering of aA*B - bD in the LAPACK routine DTGSEN
C                  failed because the transformed matrix pencil
C                  aA*B - bD would be too far from generalized Schur
C                  form; the problem is very ill-conditioned.
C
C     METHOD
C
C     The algorithm uses orthogonal transformations as described in [2]
C     (page 21). The QZ algorithm is used for N1 = 2 or N2 = 2, but it
C     always acts on an upper block triangular pencil.
C
C     REFERENCES
C
C     [1] Benner, P., Byers, R., Mehrmann, V. and Xu, H.
C         Numerical computation of deflating subspaces of skew-
C         Hamiltonian/Hamiltonian pencils.
C         SIAM J. Matrix Anal. Appl., 24 (1), pp. 165-190, 2002.
C
C     [2] Benner, P., Byers, R., Losse, P., Mehrmann, V. and Xu, H.
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
C     Chemnitz, October 21, 2008.
C
C     REVISIONS
C
C     V. Sima, July 2009 (SLICOT version of the routine DBTFEX).
C     V. Sima, Nov. 2009, Oct. 2010, Nov. 2010.
C     M. Voigt, Jan. 2012.
C
C     KEYWORDS
C
C     Block triangular pencil, eigenvalue exchange.
C
C     ******************************************************************
C
      DOUBLE PRECISION   ZERO, ONE, TEN, HUND
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TEN = 1.0D+1,
     $                     HUND = 1.0D+2 )
C
C     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDA, LDB, LDD, LDQ1, LDQ2, LDQ3, LDWORK,
     $                   N1, N2
      DOUBLE PRECISION   PREC
C
C     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), D( LDD, * ),
     $                   DWORK( * ), Q1( LDQ1, * ), Q2( LDQ2, * ),
     $                   Q3( LDQ3, * )
C
C     .. Local Scalars ..
      LOGICAL            AEVINF, EVINF, LUPLO
      INTEGER            CNT, EVSEL, I, IAEV, IDUM, IEVS, ITMP, J, M
      DOUBLE PRECISION   ABSAEV, ABSEV, ADIF, CO1, CO2, CO3, E, G, SI1,
     $                   SI2, SI3, TMP, TOL, TOLB
C
C     .. Local Arrays ..
      LOGICAL            BWORK( 1 ), OUT( 2 ), SLCT( 4 )
      INTEGER            IDM( 1 )
      DOUBLE PRECISION   DUM( 2 )
C
C     .. External Functions ..
      LOGICAL            LSAME, SB02OW
      EXTERNAL           LSAME, SB02OW
C
C     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEMM, DGEQR2, DGGES, DGGEV, DLACPY,
     $                   DLARTG, DLASET, DORG2R, DSWAP, DTGSEN
C
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX
C
C     .. Executable Statements ..
C
C     Decode the input arguments.
C
      LUPLO = LSAME( UPLO, 'U' )
C
C     For efficiency, the input arguments are not tested.
C
      INFO = 0
C
C     Computations.
C
      M = N1 + N2
      IF( M.GT.2 ) THEN
C
C        Compute A*B, and, if UPLO = 'L', make the pencil upper block
C        triangular. Array Q2 is used as workspace.
C
         IF( LUPLO ) THEN
            CALL DGEMM( 'No Transpose', 'No Transpose', N1, N1, N1, ONE,
     $                  A, LDA, B, LDB, ZERO, Q2, LDQ2 )
            CALL DLASET( 'Full', N2, N1, ZERO, ZERO, Q2( N1+1, 1 ), LDQ2
     $                 )
            CALL DGEMM( 'No Transpose', 'No Transpose', N1, N2, M, ONE,
     $                  A, LDA, B( 1, N1+1 ), LDB, ZERO, Q2( 1, N1+1 ),
     $                  LDQ2 )
            CALL DGEMM( 'No Transpose', 'No Transpose', N2, N2, N2, ONE,
     $                  A( N1+1, N1+1 ), LDA, B( N1+1, N1+1 ), LDB,
     $                  ZERO, Q2( N1+1, N1+1 ), LDQ2 )
         ELSE
C
            CALL DGEMM( 'No Transpose', 'No Transpose', N2, N2, N2, ONE,
     $                  A( N1+1, N1+1 ), LDA, B( N1+1, N1+1 ), LDB,
     $                  ZERO, Q2, LDQ2 )
            CALL DLASET( 'Full', N1, N2, ZERO, ZERO, Q2( N2+1, 1 ), LDQ2
     $                 )
            CALL DGEMM( 'No Transpose', 'No Transpose', N2, N1, M, ONE,
     $                  A( N1+1, 1 ), LDA, B, LDB, ZERO, Q2( 1, N2+1 ),
     $                  LDQ2 )
            CALL DGEMM( 'No Transpose', 'No Transpose', N1, N1, N1, ONE,
     $                  A, LDA, B, LDB, ZERO, Q2( N2+1, N2+1 ), LDQ2 )
            IF( N1.EQ.1 ) THEN
               DUM( 1 )  = D( 1, 1 )
               DUM( 2 )  = D( 2, 1 )
               D( 1, 1 ) = D( 2, 2 )
               D( 2, 1 ) = D( 3, 2 )
               D( 1, 2 ) = D( 2, 3 )
               D( 2, 2 ) = D( 3, 3 )
               D( 1, 3 ) = DUM( 2 )
               D( 2, 3 ) = D( 3, 1 )
               D( 3, 3 ) = DUM( 1 )
               D( 3, 1 ) = ZERO
               D( 3, 2 ) = ZERO
            ELSE IF( N2.EQ.1 ) THEN
               DUM( 1 )  = D( 3, 2 )
               DUM( 2 )  = D( 3, 3 )
               D( 2, 3 ) = D( 1, 2 )
               D( 3, 3 ) = D( 2, 2 )
               D( 2, 2 ) = D( 1, 1 )
               D( 3, 2 ) = D( 2, 1 )
               D( 1, 1 ) = DUM( 2 )
               D( 1, 2 ) = D( 3, 1 )
               D( 1, 3 ) = DUM( 1 )
               D( 2, 1 ) = ZERO
               D( 3, 1 ) = ZERO
            ELSE
C
               DO 10 J = 1, N1
                  CALL DSWAP( N1, D( 1,    J ), 1, D( N1+1, N1+J ), 1 )
                  CALL DSWAP( N1, D( 1, N1+J ), 1, D( N1+1,    J ), 1 )
   10          CONTINUE
C
            END IF
            ITMP = N1
            N1 = N2
            N2 = ITMP
         END IF
C
C        Apply the QZ algorithm and order the eigenvalues in
C        DWORK(1:3*N1) to the top.
C
C        Workspace: need   11*N1.
C                   Note that N1 and N2 are interchanged for UPLO = 'L'.
C
         IEVS = 3*N1 + 1
         IAEV = IEVS + 3*N1
         CALL DLACPY( 'Full', M, M, D, LDD, Q1, LDQ1 )
         CALL DLACPY( 'Full', M, M, Q2, LDQ2, Q3, LDQ3 )
         CALL DGGEV( 'No Vector', 'No Vector', N1, Q1, LDQ1, Q3, LDQ3,
     $               DWORK, DWORK( N1+1 ), DWORK( 2*N1+1 ), DUM, 1, DUM,
     $               1, DWORK( IEVS ), LDWORK-IEVS+1, INFO )
         IF( INFO.GE.1 .AND. INFO.LE.N1 ) THEN
            INFO = 1
            RETURN
         ELSE IF( INFO.GT.N1 ) THEN
            INFO = 2
            RETURN
         END IF
C
C        Workspace: need   16*N1 + 10*N2 + 23.
C                   Note that N1 and N2 are interchanged for UPLO = 'L'.
C
         ITMP = IAEV + 3*M
         CALL DCOPY( 3*N1, DWORK, 1, DWORK( IEVS ), 1 )
         CALL DGGES( 'Vector Computation', 'Vector Computation',
     $               'Not sorted', SB02OW, M, D, LDD, Q2, LDQ2, IDUM,
     $               DWORK( IAEV ), DWORK( IAEV+M ), DWORK( IAEV+2*M ),
     $               Q3, LDQ3, Q1, LDQ1, DWORK( ITMP ), LDWORK-ITMP+1,
     $               BWORK, INFO )
         IF( INFO.NE.0 ) THEN
            IF( INFO.GE.1 .AND. INFO.LE.M ) THEN
               INFO = 3
               RETURN
            ELSE IF( INFO.NE.M+2 ) THEN
               INFO = 4
               RETURN
            ELSE
               INFO = 0
            END IF
         END IF
C
         TOL   = PREC
         TOLB  = TEN*PREC
         EVSEL = 0
         DO 20 I = 1, M
            SLCT( I ) = .TRUE.
   20    CONTINUE
C
C        WHILE( EVSEL.EQ.0 ) DO
C
   30    CONTINUE
         IF( EVSEL.EQ.0 ) THEN
            CNT = 0
            OUT( 1 ) = .FALSE.
            OUT( 2 ) = .FALSE.
C
            DO 50 I = IAEV, IAEV + M - 1
               AEVINF = ABS( DWORK( 2*M+I ) ).LT.PREC*
     $                ( ABS( DWORK( I ) ) + ABS( DWORK( M+I ) ) )
               DO 40 J = 1, N1
C
C                 Check if an eigenvalue is selected and check if it
C                 is infinite.
C
                  EVINF = ABS( DWORK( 2*N1+J ) ).LT.PREC*
     $                  ( ABS( DWORK( J ) ) + ABS( DWORK( N1+J ) ) )
                  IF( ( .NOT. EVINF .OR. AEVINF ) .AND.
     $                ( .NOT.AEVINF .OR.  EVINF ) .AND.
     $                  .NOT. OUT( J ) ) THEN
                     IF( .NOT.EVINF .OR. .NOT.AEVINF ) THEN
                        ADIF   = ABS( DWORK( J    )/DWORK( 2*N1+J ) -
     $                                DWORK( I    )/DWORK( 2*M+I  ) ) +
     $                           ABS( DWORK( N1+J )/DWORK( 2*N1+J ) -
     $                                DWORK( M+I  )/DWORK( 2*M+I  ) )
                        ABSEV  = ABS( DWORK( J    )/DWORK( 2*N1+J ) ) +
     $                           ABS( DWORK( N1+J )/DWORK( 2*N1+J ) )
                        ABSAEV = ABS( DWORK( I    )/DWORK( 2*M+I  ) ) +
     $                           ABS( DWORK( M+I  )/DWORK( 2*M+I  ) )
                        IF( ADIF.LE.TOL*MAX( TOLB, ABSEV, ABSAEV ) )
     $                        THEN
                           SLCT( I-IAEV+1 ) = .FALSE.
                           OUT( J ) = .TRUE.
                           CNT = CNT + 1
                        END IF
                     ELSE
                        SLCT( I-IAEV+1 ) = .FALSE.
                        OUT( J ) = .TRUE.
                        CNT = CNT + 1
                     END IF
                  END IF
   40          CONTINUE
   50       CONTINUE
C
            IF( CNT.EQ.N1 ) THEN
               EVSEL = 1
            ELSE
C
C              CNT < N1, too few eigenvalues selected.
C
               TOL = TEN*TOL
               CALL DCOPY( 3*N1, DWORK( IEVS ), 1, DWORK, 1 )
            END IF
            GO TO 30
         END IF
C        END WHILE 30
C
C        Workspace: need   7*N1 + 7*N2 + 16.
C
         ITMP = 3*M + 1
         CALL DTGSEN( 0, .TRUE., .TRUE., SLCT, M, D, LDD, Q2, LDQ2,
     $                DWORK, DWORK( M+1 ), DWORK( 2*M+1 ),
     $                Q3, LDQ3, Q1, LDQ1, IDUM, TMP, TMP, DUM,
     $                DWORK( ITMP ), LDWORK-ITMP+1, IDM, 1, INFO )
         IF( INFO.EQ.1 ) THEN
            INFO = 5
            RETURN
         END IF
C
C        Interchange N1 and N2.
C
         ITMP = N1
         N1 = N2
         N2 = ITMP
C
         IF( .NOT.LUPLO ) THEN
C
C           Permute the rows of Q1 and Q3.
C
            IF( N1.EQ.1 ) THEN
C
               DO 60 J = 1, M
                  TMP = Q1( 3, J )
                  Q1( 3, J ) = Q1( 2, J )
                  Q1( 2, J ) = Q1( 1, J )
                  Q1( 1, J ) = TMP
                  TMP = Q3( 3, J )
                  Q3( 3, J ) = Q3( 2, J )
                  Q3( 2, J ) = Q3( 1, J )
                  Q3( 1, J ) = TMP
   60          CONTINUE
C
            ELSE IF( N2.EQ.1 ) THEN
C
               DO 70 J = 1, M
                  TMP = Q1( 1, J )
                  Q1( 1, J ) = Q1( 2, J )
                  Q1( 2, J ) = Q1( 3, J )
                  Q1( 3, J ) = TMP
                  TMP = Q3( 1, J )
                  Q3( 1, J ) = Q3( 2, J )
                  Q3( 2, J ) = Q3( 3, J )
                  Q3( 3, J ) = TMP
   70          CONTINUE
C
            ELSE
C
               DO 80 J = 1, M
                  CALL DSWAP( N1, Q1( 1, J ), 1, Q1( N1+1, J ), 1 )
                  CALL DSWAP( N1, Q3( 1, J ), 1, Q3( N1+1, J ), 1 )
   80          CONTINUE
C
            END IF
         END IF
C
C        Workspace: need   2*N1 + 2*N2.
C
         IF( LUPLO ) THEN
            CALL DGEMM( 'No Transpose', 'No Transpose', N2, M, M, ONE,
     $                  B, LDB, Q1, LDQ1, ZERO, Q2, LDQ2 )
            CALL DGEMM( 'No Transpose', 'No Transpose', N1, M, N1, ONE,
     $                  B( N2+1, N2+1 ), LDB, Q1( N2+1, 1 ), LDQ1, ZERO,
     $                  Q2( N2+1, 1 ), LDQ2 )
         ELSE
            CALL DGEMM( 'No Transpose', 'No Transpose', N1, M, N1, ONE,
     $                  B, LDB, Q1, LDQ1, ZERO, Q2, LDQ2 )
            CALL DGEMM( 'No Transpose', 'No Transpose', N2, M, M, ONE,
     $                  B ( N1+1, 1 ), LDB, Q1, LDQ1, ZERO,
     $                  Q2( N1+1, 1 ), LDQ2 )
         END IF
         CALL DGEQR2( M, M, Q2, LDQ2, DWORK, DWORK( M+1 ), INFO )
         CALL DORG2R( M, M, M, Q2, LDQ2, DWORK, DWORK( M+1 ), INFO )
C
      ELSE
C
C        2-by-2 case.
C
         IF( .NOT.LUPLO ) THEN
            TMP = A( 1, 1 )
            A( 1, 1 ) =  A( 2, 2 )
            A( 2, 2 ) = TMP
            A( 1, 2 ) = -A( 2, 1 )
            A( 2, 1 ) = ZERO
            TMP = B( 1, 1 )
            B( 1, 1 ) =  B( 2, 2 )
            B( 2, 2 ) = TMP
            B( 1, 2 ) = -B( 2, 1 )
            B( 2, 1 ) = ZERO
            TMP = D( 1, 1 )
            D( 1, 1 ) =  D( 2, 2 )
            D( 2, 2 ) = TMP
            D( 1, 2 ) = -D( 2, 1 )
            D( 2, 1 ) = ZERO
         END IF
C
         TMP = A( 2, 2 )*B( 2, 2 )*D( 1, 1 )
         G   = A( 1, 1 )*B( 1, 1 )*D( 2, 2 ) - TMP
         IF( ABS( G ).LT.HUND*PREC*ABS( TMP ) ) THEN
C
C           The eigenvalues might be too close to interchange them.
C
            IF( LUPLO ) THEN
               CALL DLASET( 'Full', 2, 2, ZERO, ONE, Q1, LDQ1 )
               CALL DLASET( 'Full', 2, 2, ZERO, ONE, Q2, LDQ2 )
               CALL DLASET( 'Full', 2, 2, ZERO, ONE, Q3, LDQ3 )
            ELSE
               Q1( 1, 1 ) = ZERO
               Q1( 2, 1 ) = -ONE
               Q1( 1, 2 ) =  ONE
               Q1( 2, 2 ) = ZERO
               Q2( 1, 1 ) = ZERO
               Q2( 2, 1 ) = -ONE
               Q2( 1, 2 ) =  ONE
               Q2( 2, 2 ) = ZERO
               Q3( 1, 1 ) = ZERO
               Q3( 2, 1 ) = -ONE
               Q3( 1, 2 ) =  ONE
               Q3( 2, 2 ) = ZERO
            END IF
         ELSE
            E = ( A( 1, 1 )*B( 1, 2 ) + A( 1, 2 )*B( 2, 2 ) )*D( 2, 2 )
     $          - A( 2, 2 )*B( 2, 2 )*D( 1, 2 )
            CALL DLARTG( E, G, CO1, SI1, TMP )
            E = ( A( 1, 2 )*D( 2, 2 ) - A( 2, 2 )*D( 1, 2 ) )*B( 1, 1 )
     $          + A( 2, 2 )*D( 1, 1 )*B( 1, 2 )
            CALL DLARTG( E, G, CO2, SI2, TMP )
            E = ( B( 1, 2 )*D( 1, 1 ) - B( 1, 1 )*D( 1, 2 ) )*A( 1, 1 )
     $          + A( 1, 2 )*B( 2, 2 )*D( 1, 1 )
            CALL DLARTG( E, G, CO3, SI3, TMP )
C
            IF( LUPLO ) THEN
               Q1( 1, 1 ) =  CO1
               Q1( 2, 1 ) = -SI1
               Q1( 1, 2 ) =  SI1
               Q1( 2, 2 ) =  CO1
               Q2( 1, 1 ) =  CO2
               Q2( 2, 1 ) = -SI2
               Q2( 1, 2 ) =  SI2
               Q2( 2, 2 ) =  CO2
               Q3( 1, 1 ) =  CO3
               Q3( 2, 1 ) = -SI3
               Q3( 1, 2 ) =  SI3
               Q3( 2, 2 ) =  CO3
            ELSE
               Q1( 1, 1 ) = -SI1
               Q1( 2, 1 ) = -CO1
               Q1( 1, 2 ) =  CO1
               Q1( 2, 2 ) = -SI1
               Q2( 1, 1 ) = -SI2
               Q2( 2, 1 ) = -CO2
               Q2( 1, 2 ) =  CO2
               Q2( 2, 2 ) = -SI2
               Q3( 1, 1 ) = -SI3
               Q3( 2, 1 ) = -CO3
               Q3( 1, 2 ) =  CO3
               Q3( 2, 2 ) = -SI3
            END IF
         END IF
      END IF
C
      RETURN
C *** Last line of MB03CD ***
      END
