      SUBROUTINE TG01GD( JOBS, L, N, M, P, A, LDA, E, LDE, B, LDB,
     $                   C, LDC, D, LDD, LR, NR, RANKE, INFRED, TOL,
     $                   IWORK, DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To find a reduced descriptor representation (Ar-lambda*Er,Br,Cr)
C     without non-dynamic modes for a descriptor representation
C     (A-lambda*E,B,C). Optionally, the reduced descriptor system can
C     be put into a standard form with the leading diagonal block
C     of Er identity.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOBS    CHARACTER*1
C             Indicates whether the user wishes to transform the leading
C             diagonal block of Er to an identity matrix, as follows:
C             = 'S':  make Er with leading diagonal identity;
C             = 'D':  keep Er unreduced or upper triangular.
C
C     Input/Output Parameters
C
C     L       (input) INTEGER
C             The number of rows of the matrices A, E, and B;
C             also the number of differential equations.  L >= 0.
C
C     N       (input) INTEGER
C             The number of columns of the matrices A, E, and C;
C             also the dimension of descriptor state vector.  N >= 0.
C
C     M       (input) INTEGER
C             The number of columns of the matrix B;
C             also the dimension of the input vector.  M >= 0.
C
C     P       (input) INTEGER
C             The number of rows of the matrix C.
C             also the dimension of the output vector.  P >= 0.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading L-by-N part of this array must
C             contain the state dynamics matrix A.
C             On exit, if NR < N, the leading LR-by-NR part of this
C             array contains the reduced order state matrix Ar of a
C             descriptor realization without non-dynamic modes.
C             Array A contains the original state dynamics matrix if
C             INFRED < 0.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,L).
C
C     E       (input/output) DOUBLE PRECISION array, dimension (LDE,N)
C             On entry, the leading L-by-N part of this array must
C             contain the descriptor matrix E.
C             On exit, if INFRED >= 0, the leading LR-by-NR part of this
C             array contains the reduced order descriptor matrix Er of a
C             descriptor realization without non-dynamic modes.
C             In this case, only the leading RANKE-by-RANKE submatrix
C             of Er is nonzero and this submatrix is nonsingular and
C             upper triangular. Array E contains the original descriptor
C             matrix if INFRED < 0. If JOBS = 'S', then the leading
C             RANKE-by-RANKE submatrix results in an identity matrix.
C
C     LDE     INTEGER
C             The leading dimension of the array E.  LDE >= MAX(1,L).
C
C     B       (input/output) DOUBLE PRECISION array, dimension (LDB,M)
C             On entry, the leading L-by-M part of this array must
C             contain the input matrix B.
C             On exit, the leading LR-by-M part of this array contains
C             the reduced order input matrix Br of a descriptor
C             realization without non-dynamic modes. Array B contains
C             the original input matrix if INFRED < 0.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= MAX(1,L).
C
C     C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
C             On entry, the leading P-by-N part of this array must
C             contain the output matrix C.
C             On exit, the leading P-by-NR part of this array contains
C             the reduced order output matrix Cr of a descriptor
C             realization without non-dynamic modes. Array C contains
C             the original output matrix if INFRED < 0.
C
C     LDC     INTEGER
C             The leading dimension of the array C.  LDC >= MAX(1,P).
C
C     D       (input/output) DOUBLE PRECISION array, dimension (LDD,M)
C             On entry, the leading P-by-M part of this array must
C             contain the original feedthrough matrix D.
C             On exit, the leading P-by-M part of this array contains
C             the feedthrough matrix Dr of a reduced descriptor
C             realization without non-dynamic modes.
C
C     LDD     INTEGER
C             The leading dimension of the array D.  LDD >= MAX(1,P).
C
C     LR      (output) INTEGER
C             The number of reduced differential equations.
C
C     NR      (output) INTEGER
C             The dimension of the reduced descriptor state vector.
C
C     RANKE   (output) INTEGER
C             The estimated rank of the matrix E.
C
C     INFRED  (output) INTEGER
C             This parameter contains information on performed reduction
C             and on structure of resulting system matrices, as follows:
C             INFRED >= 0 the reduced system is in an SVD-like
C                         coordinate form with Er upper triangular;
C                         INFRED is the achieved order reduction.
C             INFRED  < 0 no reduction achieved and the original
C                         system has been restored.
C
C     Tolerances
C
C     TOL     DOUBLE PRECISION
C             The tolerance to be used in rank determinations when
C             transforming (A-lambda*E). If the user sets TOL > 0,
C             then the given value of TOL is used as a lower bound for
C             reciprocal condition numbers in rank determinations; a
C             (sub)matrix whose estimated condition number is less than
C             1/TOL is considered to be of full rank.  If the user sets
C             TOL <= 0, then an implicitly computed, default tolerance,
C             defined by  TOLDEF = L*N*EPS,  is used instead, where EPS
C             is the machine precision (see LAPACK Library routine
C             DLAMCH).  TOL < 1.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (N)
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The length of the array DWORK.
C             LDWORK >= 1, if MIN(L,N) = 0; otherwise,
C             LDWORK >= MAX( N+P, MIN(L,N)+MAX(3*N-1,M,L) ).
C             If LDWORK >= 2*L*N+L*M+N*P+
C                          MAX( 1, N+P, MIN(L,N)+MAX(3*N-1,M,L) ) then
C             the original matrices are restored if no order reduction
C             is possible. This is achieved by saving system matrices
C             before reduction and restoring them if no order reduction
C             took place.
C
C             If LDWORK = -1, then a workspace query is assumed; the
C             routine only calculates the optimal size of the DWORK
C             array, returns this value as the first entry of the DWORK
C             array, and no error message related to LDWORK is issued by
C             XERBLA. The optimal size does not necessarily include the 
C             space needed for saving the original system matrices.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value.
C
C     METHOD
C
C     The subroutine elliminates the non-dynamics modes in two steps:
C
C     Step 1: Reduce the system to the SVD-like coordinate form
C     (Q'*A*Z-lambda*Q'*E*Z, Q'*B, C*Z) , where
C
C              ( A11 A12 A13 )           ( E11 0 0 )         ( B1 )
C     Q'*A*Z = ( A21 A22  0  ), Q'*E*Z = (  0  0 0 ), Q'*B = ( B2 ),
C              ( A31  0   0  )           (  0  0 0 )         ( B3 )
C
C        C*Z = ( C1  C2  C3 ),
C
C     where E11 and A22 are upper triangular invertible matrices.
C
C     Step 2: Compute the reduced system as (Ar-lambda*Er,Br,Cr,Dr),
C     where
C          ( A11 - A12*inv(A22)*A21, A13 )        ( E11 0 )
C     Ar = (                             ),  Er = (       ),
C          (     A31                  0  )        (  0  0 )
C
C          ( B1 - A12*inv(A22)*B2 )
C     Br = (                      ),  Cr = ( C1 - C2*inv(A22)*A21, C3 ),
C          (        B3            )
C
C     Dr = D - C2*inv(A22)*B2.
C
C     Step 3: If desired (JOBS = 'S'), reduce the descriptor system to
C     the standard form
C
C     Ar <- diag(inv(E11),I)*Ar;  Br <- diag(inv(E11),I)*Br;
C     Er  = diag(I,0).
C
C     If L = N and LR = NR = RANKE, then if Step 3 is performed,
C     the resulting system is a standard state space system.
C
C     NUMERICAL ASPECTS
C
C     If L = N, the algorithm requires 0( N**3 ) floating point
C     operations.
C
C     CONTRIBUTOR
C
C     A. Varga, German Aerospace Center, DLR Oberpfaffenhofen.
C     May 1999.
C
C     REVISIONS
C
C     July 1999, V. Sima, Research Institute for Informatics, Bucharest.
C     A. Varga, DLR Oberpfaffenhofen, March 2002.
C     V. Sima, Dec. 2016, Feb. 2017.
C
C     KEYWORDS
C
C     Minimal realization, orthogonal transformation.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D0, ZERO = 0.0D0 )
C     .. Scalar Arguments ..
      CHARACTER         JOBS
      INTEGER           INFO, INFRED, L, LDA, LDB, LDC, LDD, LDE,
     $                  LDWORK, LR, M, N, NR, P, RANKE
      DOUBLE PRECISION  TOL
C     .. Array Arguments ..
      INTEGER           IWORK(*)
      DOUBLE PRECISION  A(LDA,*), B(LDB,*), C(LDC,*), D(LDD,*),
     $                  DWORK(*), E(LDE,*)
C     .. Local Scalars ..
      LOGICAL           LQUERY, LSPACE, SSTYPE
      INTEGER           K, K1, KWA, KWB, KWC, KWE, KWR, LS, LWRMIN, NS,
     $                  RNKA22, WRKOPT
C     .. Local Arrays ..
      DOUBLE PRECISION  DUM(1)
C     .. External Functions ..
      LOGICAL           LSAME
      EXTERNAL          LSAME
C     .. External Subroutines ..
      EXTERNAL          DGEMM, DLACPY, DLASET, DTRSM, TG01FD, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         INT, MAX, MIN
C     .. Executable Statements ..
C
C     Decode JOBS.
C
      SSTYPE = LSAME( JOBS, 'S' )
C
      INFO = 0
C
C     Test the input scalar arguments.
C
      IF( .NOT.SSTYPE .AND. .NOT.LSAME( JOBS, 'D' )  ) THEN
         INFO = -1
      ELSE IF( L.LT.0 ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( M.LT.0 ) THEN
         INFO = -4
      ELSE IF( P.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, L) ) THEN
         INFO = -7
      ELSE IF( LDE.LT.MAX( 1, L ) ) THEN
         INFO = -9
      ELSE IF( LDB.LT.MAX( 1, L ) ) THEN
         INFO = -11
      ELSE IF( LDC.LT.MAX( 1, P ) ) THEN
         INFO = -13
      ELSE IF( LDD.LT.MAX( 1, P ) ) THEN
         INFO = -15
      ELSE IF( TOL.GE.ONE ) THEN
         INFO = -20
      ELSE
         IF( MIN( L, N ).EQ.0 ) THEN
            LWRMIN = 1
         ELSE
            LWRMIN = MAX( N + P, MIN( L, N ) + MAX( 3*N - 1, M, L ) )
         END IF
         LQUERY = LDWORK.EQ.-1
C
         IF( LQUERY ) THEN
            CALL TG01FD( 'Not Q', 'Not Z', 'Reduce A', L, N, M, P, A,
     $                   LDA, E, LDE, B, LDB, C, LDC, DUM, 1, DUM, 1,
     $                   RANKE, RNKA22, TOL, IWORK, DWORK, -1, INFO )
            WRKOPT = MAX( LWRMIN, INT( DWORK(1) ) )
         ELSE IF( LDWORK.LT.LWRMIN ) THEN
            INFO = -23
         END IF
      END IF
C
      IF ( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'TG01GD', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         DWORK(1) = WRKOPT
         RETURN
       END IF
C
C     Quick return if possible.
C
      LR = L
      NR = N
      IF( MIN( L, N ).EQ.0 ) THEN
         DWORK(1) = ONE
         RANKE    =  0
         INFRED   = -1
         RETURN
      END IF
C
C     Set large workspace option.
C
      LSPACE = LDWORK.GE.( LWRMIN + L*( 2*N + M) + P*N ) .AND.
     $         .NOT.SSTYPE
C
      IF( LSPACE) THEN
C
C        Determine offsets and save system matrices.
C
         KWA = 1
         KWE = KWA + L*N
         KWB = KWE + L*N
         KWC = KWB + L*M
         KWR = KWC + P*N
         CALL DLACPY( 'Full', L, N, A, LDA, DWORK(KWA), L )
         CALL DLACPY( 'Full', L, N, E, LDE, DWORK(KWE), L )
         CALL DLACPY( 'Full', L, M, B, LDB, DWORK(KWB), L )
         CALL DLACPY( 'Full', P, N, C, LDC, DWORK(KWC), MAX( 1, P ) )
      ELSE
         KWR = 1
      END IF
C
C     Reduce the descriptor system to the SVD-like coordinate form
C     (At-lambda*Et, Bt, Ct) , where
C
C             ( A11 A12 A13 )       ( E11 0 0 )       ( B1 )
C        At = ( A21 A22  0  ), Et = (  0  0 0 ), Bt = ( B2 ),
C             ( A31  0   0  )       (  0  0 0 )       ( B3 )
C
C        Ct = ( C1  C2  C3  ),
C
C     and E11 and A22 are RANKE-by-RANKE and RNKA22-by-RNKA22
C     upper triangular invertible matrices, respectively.
C     Workspace: needed real MAX( N+P, MIN(L,N)+MAX(3*N-1,M,L) );
C                needed integer N.
C
      CALL TG01FD( 'Not Q', 'Not Z', 'Reduce A', L, N, M, P, A, LDA,
     $              E, LDE, B, LDB, C, LDC, DUM, 1, DUM, 1, RANKE,
     $              RNKA22, TOL, IWORK, DWORK(KWR), LDWORK-KWR+1, INFO )
C
      IF( INFO.EQ.0 ) THEN
         INFRED = RNKA22
         IF( RNKA22.GT.0 ) THEN
C
C           Apply residualization formulas.
C
            K  = RANKE + 1
            K1 = MIN( L, N, K + RNKA22 )
            LR = L  - RNKA22
            NR = N  - RNKA22
            LS = LR - RANKE
            NS = NR - RANKE
C
C           Compute A21 <- INV(A22)*A21.
C
            CALL DTRSM( 'Left', 'Upper', 'No Transpose', 'Non-unit',
     $                  RNKA22, RANKE, ONE, A(K,K), LDA, A(K,1), LDA )
C
C           Compute B2 <- INV(A22)*B2.
C
            CALL DTRSM( 'Left', 'Upper', 'No Transpose', 'Non-unit',
     $                  RNKA22, M, ONE, A(K,K), LDA, B(K,1), LDB )
C
C           Compute the residualized systems matrices.
C
C           Dr = D - C2*INV(A22)*B2.
C
            CALL DGEMM( 'No Transpose', 'No Transpose', P, M, RNKA22,
     $                  -ONE, C(1,K), LDC, B(K,1), LDB, ONE, D, LDD )
C
C           Br = ( B1 - A12*INV(A22)*B2 ).
C                (         B3           )
C
            CALL DGEMM( 'No Transpose', 'No Transpose', RANKE, M,
     $                  RNKA22, -ONE, A(1,K), LDA, B(K,1), LDB, ONE,
     $                  B, LDB )
            CALL DLACPY( 'Full', LS, M, B(K1,1), LDB, B(K,1), LDB )
C
C           Cr = ( C1 - C2*INV(A22)*A21 C3 ).
C
            CALL DGEMM( 'NoTranspose', 'NoTranspose', P, RANKE, RNKA22,
     $                  -ONE, C(1,K), LDC, A(K,1), LDA, ONE, C, LDC )
            CALL DLACPY( 'Full', P, NS, C(1,K1), LDC, C(1,K), LDC )
C
C           Ar = ( A11 - A12*INV(A22)*A21  A13 ).
C                (       A31                0  )
C
            CALL DGEMM( 'No Transpose', 'No Transpose', RANKE, RANKE,
     $                  RNKA22, -ONE, A(1,K), LDA, A(K,1), LDA, ONE, A,
     $                  LDA )
C
            CALL DLACPY( 'Full', LS,    NR, A(K1,1), LDA, A(K,1), LDA )
            CALL DLACPY( 'Full', RANKE, NS, A(1,K1), LDA, A(1,K), LDA )
         ELSE
            IF( LSPACE ) THEN
C
C              Restore system matrices.
C
               CALL DLACPY( 'Full', L, N, DWORK(KWA), L, A, LDA )
               CALL DLACPY( 'Full', L, N, DWORK(KWE), L, E, LDE )
               CALL DLACPY( 'Full', L, M, DWORK(KWB), L, B, LDB )
               CALL DLACPY( 'Full', P, N, DWORK(KWC), MAX( 1, P ), C,
     $                      LDC )
               INFRED = -1
            END IF
         END IF
C
         IF( SSTYPE ) THEN
C
C           Ar <- diag(inv(E11),I)*Ar.
C
            CALL DTRSM( 'Left', 'Upper', 'No Transpose', 'Non-unit',
     $                  RANKE, NR, ONE, E, LDE, A, LDA )
C
C           Br <- diag(inv(E11),I)*Br.
C
            CALL DTRSM( 'Left', 'Upper', 'No Transpose', 'Non-unit',
     $                  RANKE, M, ONE, E, LDE, B, LDB )
C
C           E11 = I.
C
            CALL DLASET( 'Full', RANKE, RANKE, ZERO, ONE, E, LDE )
         END IF
         DWORK(1) = DWORK(KWR)
      END IF
C
      RETURN
C *** Last line of TG01GD ***
      END
