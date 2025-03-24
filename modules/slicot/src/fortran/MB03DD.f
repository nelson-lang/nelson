      SUBROUTINE MB03DD( UPLO, N1, N2, PREC, A, LDA, B, LDB, Q1, LDQ1,
     $                   Q2, LDQ2, DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To compute orthogonal matrices Q1 and Q2 for a real 2-by-2,
C     3-by-3, or 4-by-4 regular block upper triangular pencil
C
C                    ( A11 A12 )     ( B11 B12 )
C       aA - bB =  a (         ) - b (         ),                    (1)
C                    (  0  A22 )     (  0  B22 )
C
C     such that the pencil a(Q2' A Q1) - b(Q2' B Q1) is still in block
C     upper triangular form, but the eigenvalues in Spec(A11, B11),
C     Spec(A22, B22) are exchanged, where Spec(X,Y) denotes the spectrum
C     of the matrix pencil (X,Y) and the notation M' denotes the
C     transpose of the matrix M.
C
C     Optionally, to upper triangularize the real regular pencil in
C     block lower triangular form
C
C                   ( A11  0  )     ( B11  0  )
C       aA - bB = a (         ) - b (         ),                     (2)
C                   ( A21 A22 )     ( B21 B22 )
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
C             = 'T': Upper block triangular, B triangular, eigenvalues
C                    are exchanged on exit;
C             = 'L': Lower block triangular, eigenvalues are not
C                    exchanged on exit.
C
C     Input/Output Parameters
C
C     N1      (input/output) INTEGER
C             Size of the upper left block, N1 <= 2.
C             If UPLO = 'U' or UPLO = 'T' and INFO = 0, or UPLO = 'L'
C             and INFO <> 0, N1 and N2 are exchanged on exit; otherwise,
C             N1 is unchanged on exit.
C
C     N2      (input/output) INTEGER
C             Size of the lower right block, N2 <= 2.
C             If UPLO = 'U' or UPLO = 'T' and INFO = 0, or UPLO = 'L'
C             and INFO <> 0, N1 and N2 are exchanged on exit; otherwise,
C             N2 is unchanged on exit.
C
C     PREC    (input) DOUBLE PRECISION
C             The machine precision, (relative machine precision)*base.
C             See the LAPACK Library routine DLAMCH.
C
C     A       (input/output) DOUBLE PRECISION array, dimension
C                (LDA, N1+N2)
C             On entry, the leading (N1+N2)-by-(N1+N2) part of this
C             array must contain the matrix A of the pencil aA - bB.
C             On exit, if N1 = N2 = 1, this array is unchanged, if
C             UPLO = 'U' or UPLO = 'T', but, if UPLO = 'L', it contains
C                                          [  0 1 ]
C             the matrix J' A J, where J = [ -1 0 ]; otherwise, this
C             array contains the transformed quasi-triangular matrix in
C             generalized real Schur form.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= N1+N2.
C
C     B       (input/output) DOUBLE PRECISION array, dimension
C                (LDB, N1+N2)
C             On entry, the leading (N1+N2)-by-(N1+N2) part of this
C             array must contain the matrix B of the pencil aA - bB.
C             On exit, if N1 = N2 = 1, this array is unchanged, if
C             UPLO = 'U' or UPLO = 'T', but, if UPLO = 'L', it contains
C             the matrix J' B J; otherwise, this array contains the
C             transformed upper triangular matrix in generalized real
C             Schur form.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= N1+N2.
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
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             If N1+N2 = 2 then DWORK is not referenced.
C
C     LDWORK  INTEGER
C             The dimension of the array DWORK.
C             If N1+N2 = 2, then LDWORK = 0; otherwise,
C             LDWORK >= 16*N1 + 10*N2 + 23, if UPLO = 'U';
C             LDWORK >=  7*N1 +  7*N2 + 16, if UPLO = 'T';
C             LDWORK >= 10*N1 + 16*N2 + 23, if UPLO = 'L'.
C             For good performance LDWORK should be generally larger.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0: succesful exit;
C             = 3: the QZ iteration failed in the LAPACK routine DGGES
C                  (if UPLO <> 'T') or DHGEQZ (if UPLO = 'T');
C             = 4: another error occured during execution of DHGEQZ;
C             = 5: reordering of aA - bB in the LAPACK routine DTGSEN
C                  failed because the transformed matrix pencil aA - bB
C                  would be too far from generalized Schur form;
C                  the problem is very ill-conditioned.
C
C     METHOD
C
C     The algorithm uses orthogonal transformations as described in [2]
C     (page 30). The QZ algorithm is used for N1 = 2 or N2 = 2, but it
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
C     Chemnitz, October 16, 2008.
C
C     REVISIONS
C
C     V. Sima, July 2009 (SLICOT version of the routine DBTUEX).
C     V. Sima, Nov. 2009, Oct. 2010, Nov. 2010, Mar. 2016, Apr. 2016,
C     May 2016.
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
      INTEGER            INFO, LDA, LDB, LDQ1, LDQ2, LDWORK, N1, N2
      DOUBLE PRECISION   PREC
C
C     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), DWORK( * ),
     $                   Q1( LDQ1, * ), Q2( LDQ2, * )
C
C     .. Local Scalars ..
      LOGICAL            AEVINF, EVINF, LTRIU, LUPLO
      INTEGER            CNT, EVSEL, I, IAEV, IDUM, IEVS, ITMP, J, M
      DOUBLE PRECISION   A11, A22, ABSAEV, ABSEV, ADIF, B11, B22, CO,
     $                   CO1, E, G, MX, NRA, NRB, SFMIN, SI, SI1, TMP,
     $                   TOL, TOLB
C
C     .. Local Arrays ..
      LOGICAL            BWORK( 1 ), OUT( 2 ), SLCT( 4 )
      INTEGER            IDM( 2 )
      DOUBLE PRECISION   AS( 2, 2 ), BS( 2, 2 ), DUM( 8 )
C
C     .. External Functions ..
      LOGICAL            LSAME,  SB02OW
      DOUBLE PRECISION   DLAMCH, DLANGE, DLANHS
      EXTERNAL           DLAMCH, DLANGE, DLANHS, LSAME, SB02OW
C
C     .. External Subroutines ..
      EXTERNAL           DCOPY, DGGES, DHGEQZ, DLACPY, DLAG2, DLARTG,
     $                   DLASET, DROT, DSWAP, DTGEX2, DTGSEN, MB01QD
C
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SIGN
C
C     .. Executable Statements ..
C
C     Decode the input arguments.
C
      LTRIU = LSAME( UPLO, 'T' )
      LUPLO = LSAME( UPLO, 'U' ) .OR. LTRIU
C
C     For efficiency, the input arguments are not tested.
C
      INFO = 0
C
C     Computations.
C
C     (Note: Comments in the code beginning "Workspace:" describe the
C     minimal amount of real workspace needed at that point in the
C     code, as well as the preferred amount for good performance.)
C
      M = N1 + N2
      IF( M.GT.2 ) THEN
         IF( .NOT.LUPLO ) THEN
C
C           Make the pencil upper block triangular.
C           Quick return if A21 = 0 and B21 = 0.
C
            IF( DLANGE( '1-norm', N2, N1, A( N1+1, 1 ), LDA, DWORK )
     $            .EQ.ZERO .AND.
     $          DLANGE( '1-norm', N2, N1, B( N1+1, 1 ), LDB, DWORK )
     $            .EQ.ZERO ) THEN
               IF( N1.EQ.2 ) THEN
                  CALL DGGES( 'Vectors', 'Vectors', 'Not sorted',
     $                        SB02OW, N1, A, LDA, B, LDB, IDUM, DWORK,
     $                        DWORK( M+1 ), DWORK( 2*M+1 ), Q2, LDQ2,
     $                        Q1, LDQ1, DWORK( 3*M+1 ), LDWORK-2*M,
     $                        BWORK, INFO )
                  IF( INFO.NE.0 ) THEN
                     IF( INFO.GE.1 .AND. INFO.LE.N1 ) THEN
                        INFO = 3
                        RETURN
                     ELSE
                        INFO = 4
                        RETURN
                     END IF
                  END IF
                  IF( N2.EQ.1 ) THEN
                     Q1( 3, 3 ) = ONE
                     Q2( 3, 3 ) = ONE
                  END IF
               END IF
               IF( N2.EQ.2 ) THEN
                  CALL DGGES( 'Vectors', 'Vectors', 'Not sorted',
     $                        SB02OW, N2, A( N1+1, N1+1 ), LDA,
     $                        B( N1+1, N1+1 ), LDB, IDUM, DWORK( N1+1 ),
     $                        DWORK( M+N1+1 ), DWORK( 2*M+N1+1 ),
     $                        Q2( N1+1, N1+1 ), LDQ2, Q1( N1+1, N1+1 ),
     $                        LDQ1, DWORK( 3*M+1 ), LDWORK-2*M, BWORK,
     $                        INFO )
                  IF( INFO.NE.0 ) THEN
                     IF( INFO.GE.1 .AND. INFO.LE.N2 ) THEN
                        INFO = 3
                        RETURN
                     ELSE
                        INFO = 4
                        RETURN
                     END IF
                  END IF
                  IF( N1.EQ.1 ) THEN
                     Q1( 1, 1 ) = ONE
                     Q2( 1, 1 ) = ONE
                  END IF
               END IF
               CALL DLASET( 'Full', N2, N1, ZERO, ZERO, Q1( N1+1, 1 ),
     $                      LDQ1 )
               CALL DLASET( 'Full', N1, N2, ZERO, ZERO, Q1( 1, N1+1 ),
     $                      LDQ1 )
               CALL DLASET( 'Full', N2, N1, ZERO, ZERO, Q2( N1+1, 1 ),
     $                      LDQ2 )
               CALL DLASET( 'Full', N1, N2, ZERO, ZERO, Q2( 1, N1+1 ),
     $                      LDQ2 )
               RETURN
            END IF
            IF( N1.EQ.1 ) THEN
               DUM( 1 )  = A( 1, 1 )
               DUM( 2 )  = A( 2, 1 )
               A( 1, 1 ) = A( 2, 2 )
               A( 2, 1 ) = A( 3, 2 )
               A( 1, 2 ) = A( 2, 3 )
               A( 2, 2 ) = A( 3, 3 )
               A( 1, 3 ) = DUM( 2 )
               A( 2, 3 ) = A( 3, 1 )
               A( 3, 3 ) = DUM( 1 )
               A( 3, 1 ) = ZERO
               A( 3, 2 ) = ZERO
               DUM( 1 )  = B( 1, 1 )
               DUM( 2 )  = B( 2, 1 )
               B( 1, 1 ) = B( 2, 2 )
               B( 2, 1 ) = B( 3, 2 )
               B( 1, 2 ) = B( 2, 3 )
               B( 2, 2 ) = B( 3, 3 )
               B( 1, 3 ) = DUM( 2 )
               B( 2, 3 ) = B( 3, 1 )
               B( 3, 3 ) = DUM( 1 )
               B( 3, 1 ) = ZERO
               B( 3, 2 ) = ZERO
            ELSE IF( N2.EQ.1 ) THEN
               DUM( 1 )  = A( 3, 2 )
               DUM( 2 )  = A( 3, 3 )
               A( 2, 3 ) = A( 1, 2 )
               A( 3, 3 ) = A( 2, 2 )
               A( 2, 2 ) = A( 1, 1 )
               A( 3, 2 ) = A( 2, 1 )
               A( 1, 1 ) = DUM( 2 )
               A( 1, 2 ) = A( 3, 1 )
               A( 1, 3 ) = DUM( 1 )
               A( 2, 1 ) = ZERO
               A( 3, 1 ) = ZERO
               DUM( 1 )  = B( 3, 2 )
               DUM( 2 )  = B( 3, 3 )
               B( 2, 3 ) = B( 1, 2 )
               B( 3, 3 ) = B( 2, 2 )
               B( 2, 2 ) = B( 1, 1 )
               B( 3, 2 ) = B( 2, 1 )
               B( 1, 1 ) = DUM( 2 )
               B( 1, 2 ) = B( 3, 1 )
               B( 1, 3 ) = DUM( 1 )
               B( 2, 1 ) = ZERO
               B( 3, 1 ) = ZERO
            ELSE
C
               DO 10 J = 1, N1
                  CALL DSWAP( N1, A( 1,    J ), 1, A( N1+1, N1+J ), 1 )
                  CALL DSWAP( N1, A( 1, N1+J ), 1, A( N1+1,    J ), 1 )
                  CALL DSWAP( N1, B( 1,    J ), 1, B( N1+1, N1+J ), 1 )
                  CALL DSWAP( N1, B( 1, N1+J ), 1, B( N1+1,    J ), 1 )
   10          CONTINUE
C
            END IF
C
            ITMP = N1
            N1 = N2
            N2 = ITMP
         END IF
C
C        Apply the QZ algorithm and order the eigenvalues in
C        DWORK(1:3*N1) to the top.
C        Note that N1 and N2 are interchanged for UPLO = 'L'.
C
         IEVS = 3*N1 + 1
         IAEV = IEVS + 3*N1
         IF( N1.EQ.1 ) THEN
            DWORK( 1 ) = A( 1, 1 )*SIGN( ONE, B( 1, 1 ) )
            DWORK( 2 ) = ZERO
            DWORK( 3 ) = ABS( B( 1, 1 ) )
         ELSE
            SFMIN = DLAMCH( 'Safemin' )
            Q1( 1, 1 ) = A( 1, 1 )
            Q1( 2, 1 ) = A( 2, 1 )
            Q1( 1, 2 ) = A( 1, 2 )
            Q1( 2, 2 ) = A( 2, 2 )
            Q2( 1, 1 ) = B( 1, 1 )
            Q2( 2, 1 ) = B( 2, 1 )
            Q2( 1, 2 ) = B( 1, 2 )
            Q2( 2, 2 ) = B( 2, 2 )
            IF( .NOT.LTRIU .AND. B( 2, 1 ).NE.ZERO ) THEN
C
C              Triangularize B11 and update A11.
C
               A11 = ABS( Q1( 1, 1 ) )
               A22 = ABS( Q1( 2, 2 ) )
               B11 = ABS( Q2( 1, 1 ) )
               B22 = ABS( Q2( 2, 2 ) )
               MX  = MAX( A11 + ABS( Q1( 2, 1 ) ),
     $                    A22 + ABS( Q1( 1, 2 ) ),
     $                    B11 + ABS( Q2( 2, 1 ) ),
     $                    B22 + ABS( Q2( 1, 2 ) ), SFMIN )
               Q1( 1, 1 ) = Q1( 1, 1 ) / MX
               Q1( 2, 1 ) = Q1( 2, 1 ) / MX
               Q1( 1, 2 ) = Q1( 1, 2 ) / MX
               Q1( 2, 2 ) = Q1( 2, 2 ) / MX
               Q2( 1, 1 ) = Q2( 1, 1 ) / MX
               Q2( 2, 1 ) = Q2( 2, 1 ) / MX
               Q2( 1, 2 ) = Q2( 1, 2 ) / MX
               Q2( 2, 2 ) = Q2( 2, 2 ) / MX
               CALL DLARTG( Q2( 1, 1 ), Q2( 2, 1 ), CO,  SI,  E )
               CALL DLARTG( Q2( 2, 2 ), Q2( 2, 1 ), CO1, SI1, G )
               IF( ABS( CO *B( 2, 1 ) - SI *B( 1, 1 ) ).LE.
     $             ABS( CO1*B( 2, 1 ) - SI1*B( 2, 2 ) ) ) THEN
                  CALL DROT( 2, Q1( 1, 1 ), LDQ1, Q1( 2, 1 ), LDQ1, CO,
     $                       SI )
                  Q2( 1, 1 ) = E
                  TMP = Q2( 1, 2 )
                  Q2( 1, 2 ) = SI*Q2( 2, 2 ) + CO*TMP
                  Q2( 2, 2 ) = CO*Q2( 2, 2 ) - SI*TMP
               ELSE
                  CALL DROT( 2, Q1( 1, 2 ), 1, Q1( 1, 1 ), 1, CO1, SI1 )
                  Q2( 2, 2 ) = G
                  TMP = Q2( 1, 2 )
                  Q2( 1, 2 ) = SI1*Q2( 1, 1 ) + CO1*TMP
                  Q2( 1, 1 ) = CO1*Q2( 1, 1 ) - SI1*TMP
               END IF
               Q2( 2, 1 ) = ZERO
            END IF
            CALL DLAG2( Q1, LDQ1, Q2, LDQ2, SFMIN*HUND, DWORK( 2*N1+1 ),
     $                  DWORK( 2*N1+2 ), DWORK( 1 ), DWORK( 2 ),
     $                  DWORK( N1+1 ) )
            DWORK( N1+2 ) = -DWORK( N1+1 )
         END IF
C
         ITMP = IAEV + 3*M
         CALL DCOPY( 3*N1, DWORK, 1, DWORK( IEVS ), 1 )
         IF( LTRIU ) THEN
C
C           Workspace: need   10*N1 + 4*N2.
C
            CALL DHGEQZ( 'Schur', 'Identity', 'Identity', M, 1, M, A,
     $                   LDA, B, LDB, DWORK( IAEV ), DWORK( IAEV+M ),
     $                   DWORK( IAEV+2*M ), Q2, LDQ2, Q1, LDQ1,
     $                   DWORK( ITMP ), LDWORK-ITMP+1, INFO )
            IF( INFO.GE.1 .AND. INFO.LE.M ) THEN
               INFO = 3
               RETURN
            ELSE IF( INFO.NE.0 ) THEN
               INFO = 4
               RETURN
            END IF
         ELSE
C
C           Workspace: need   16*N1 + 10*N2 + 23;
C                      prefer larger.
C
            CALL DGGES(  'Vectors', 'Vectors', 'Not sorted', SB02OW, M,
     $                   A, LDA, B, LDB, IDUM, DWORK( IAEV ),
     $                   DWORK( IAEV+M ), DWORK( IAEV+2*M ), Q2, LDQ2,
     $                   Q1, LDQ1, DWORK( ITMP ), LDWORK-ITMP+1, BWORK,
     $                   INFO )
            IF( INFO.NE.0 ) THEN
               IF( INFO.GE.1 .AND. INFO.LE.M ) THEN
                  INFO = 3
                  RETURN
               ELSE
                  INFO = 4
                  RETURN
               END IF
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
C        Workspace: need   7*N1 + 7*N2 + 16;
C                   prefer larger.
C
         ITMP = 3*M + 1
         NRA = DLANHS( '1-norm', M, A, LDA, DWORK )
         NRB = DLANHS( '1-norm', M, B, LDB, DWORK )
         IDM( 1 ) = 2
         IDM( 2 ) = 2
         CALL MB01QD( 'Hess', M, M, 0, 0, NRA, ONE, 2, IDM, A, LDA,
     $                INFO )
         CALL MB01QD( 'Hess', M, M, 0, 0, NRB, ONE, 2, IDM, B, LDB,
     $                INFO )
         CALL DTGSEN( 0, .TRUE., .TRUE., SLCT, M, A, LDA, B, LDB, DWORK,
     $                DWORK( M+1 ), DWORK( 2*M+1 ), Q2, LDQ2, Q1, LDQ1,
     $                IDUM, TMP, TMP, DUM, DWORK( ITMP ), LDWORK-ITMP+1,
     $                IDM, 1, INFO )
         IF( INFO.EQ.1 ) THEN
            INFO = 5
            RETURN
         END IF
C
         CALL MB01QD( 'Hess', M, M, 0, 0, ONE, NRA, 0, IDM, A, LDA,
     $                INFO )
         CALL MB01QD( 'Hess', M, M, 0, 0, ONE, NRB, 0, IDM, B, LDB,
     $                INFO )
C
C        Interchange N1 and N2.
C
         ITMP = N1
         N1 = N2
         N2 = ITMP
C
         IF( .NOT.LUPLO ) THEN
C
C           Permute the rows of Q1 and Q2.
C
            IF( N1.EQ.1 ) THEN
C
               DO 60 J = 1, M
                  TMP = Q1( 3, J )
                  Q1( 3, J ) = Q1( 2, J )
                  Q1( 2, J ) = Q1( 1, J )
                  Q1( 1, J ) = TMP
                  TMP = Q2( 3, J )
                  Q2( 3, J ) = Q2( 2, J )
                  Q2( 2, J ) = Q2( 1, J )
                  Q2( 1, J ) = TMP
   60          CONTINUE
C
            ELSE IF( N2.EQ.1 ) THEN
C
               DO 70 J = 1, M
                  TMP = Q1( 1, J )
                  Q1( 1, J ) = Q1( 2, J )
                  Q1( 2, J ) = Q1( 3, J )
                  Q1( 3, J ) = TMP
                  TMP = Q2( 1, J )
                  Q2( 1, J ) = Q2( 2, J )
                  Q2( 2, J ) = Q2( 3, J )
                  Q2( 3, J ) = TMP
   70          CONTINUE
C
            ELSE
C
               DO 80 J = 1, M
                  CALL DSWAP( N1, Q1( 1, J ), 1, Q1( N1+1, J ), 1 )
                  CALL DSWAP( N1, Q2( 1, J ), 1, Q2( N1+1, J ), 1 )
   80          CONTINUE
C
            END IF
         END IF
      ELSE
C
C        2-by-2 case.
C
         IF( .NOT.LUPLO .AND. A( 2, 1 ).EQ.ZERO
     $                  .AND. B( 2, 1 ).EQ.ZERO ) THEN
            CALL DLASET( 'Full', M, M, ZERO, ONE, Q1, LDQ1 )
            CALL DLASET( 'Full', M, M, ZERO, ONE, Q2, LDQ2 )
            RETURN
         ELSE IF( LUPLO ) THEN
            CALL DLASET( 'Full', M, M, ZERO, ONE, Q1, LDQ1 )
            CALL DLASET( 'Full', M, M, ZERO, ONE, Q2, LDQ2 )
         ELSE
            TMP        =  A( 1, 1 )
            A( 1, 1 )  =  A( 2, 2 )
            A( 2, 2 )  =  TMP
            A( 1, 2 )  = -A( 2, 1 )
            A( 2, 1 )  =  ZERO
            TMP        =  B( 1, 1 )
            B( 1, 1 )  =  B( 2, 2 )
            B( 2, 2 )  =  TMP
            B( 1, 2 )  = -B( 2, 1 )
            B( 2, 1 )  =  ZERO
            Q1( 1, 1 ) =  ZERO
            Q1( 2, 1 ) = -ONE
            Q1( 1, 2 ) =  ONE
            Q1( 2, 2 ) =  ZERO
            Q2( 1, 1 ) =  ZERO
            Q2( 2, 1 ) = -ONE
            Q2( 1, 2 ) =  ONE
            Q2( 2, 2 ) =  ZERO
         END IF
         A11 = A( 1, 1 )
         A22 = A( 2, 2 )
         B11 = B( 1, 1 )
         B22 = B( 2, 2 )
         MX  = MAX( ABS( A11 ), ABS( A22 ), ABS( A( 1, 2 ) ),
     $              ABS( B11 ), ABS( B22 ), ABS( B( 1, 2 ) ),
     $              DLAMCH( 'Safemin' ) )
         AS( 1, 1 ) = A11 / MX
         AS( 2, 1 ) = ZERO
         AS( 1, 2 ) = A( 1, 2 ) / MX
         AS( 2, 2 ) = A22 / MX
         BS( 1, 1 ) = B11 / MX
         BS( 2, 2 ) = B22 / MX
         BS( 1, 2 ) = B( 1, 2 ) / MX 
         CALL DLACPY( 'Full', M, M, A, LDA, AS, 2 )
         CALL DLACPY( 'Full', M, M, B, LDB, BS, 2 )
         CALL DTGEX2( .TRUE., .TRUE., M, AS, 2, BS, 2, Q2, LDQ2, Q1,
     $                LDQ1, 1, 1, 1, DUM, 8, ITMP )
C
      END IF
C
      RETURN
C *** Last line of MB03DD ***
      END
