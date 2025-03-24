      SUBROUTINE AB08NW( EQUIL, N, M, P, A, LDA, B, LDB, C, LDC, D, LDD,
     $                   NFZ, NRANK, NIZ, DINFZ, NKROR, NINFE, NKROL,
     $                   INFZ, KRONR, INFE, KRONL, E, LDE, TOL, IWORK,
     $                   DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To extract from the system pencil
C
C                       ( A-lambda*I B )
C           S(lambda) = (              )
C                       (      C     D )
C
C     a regular pencil Af-lambda*Ef which has the finite Smith zeros of
C     S(lambda) as generalized eigenvalues. The routine also computes
C     the orders of the infinite Smith zeros and determines the singular
C     and infinite Kronecker structure of the system pencil, i.e., the
C     right and left Kronecker indices, and the multiplicities of the
C     infinite eigenvalues.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     EQUIL   CHARACTER*1
C             Specifies whether the user wishes to balance the system
C             matrix as follows:
C             = 'S':  Perform balancing (scaling);
C             = 'N':  Do not perform balancing.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER.
C             The order of the square matrix A, the number of rows of
C             the matrix B, and number of columns of the matrix C.
C             N >= 0.
C
C     M       (input) INTEGER.
C             The number of columns of the matrix B.  M >= 0.
C
C     P       (input) INTEGER.
C             The number of rows of the matrix C.  P >= 0.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the state dynamics matrix A of the system.
C             On exit, the leading NFZ-by-NFZ part of this array
C             contains the matrix Af of the reduced pencil.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     B       (input/output) DOUBLE PRECISION array, dimension (LDB,M)
C             On entry, the leading N-by-M part of this array must
C             contain the input/state matrix B of the system.
C             On exit, this matrix does not contain useful information.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= 1, and
C             LDB >= MAX(1,N), if M > 0.
C
C     C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
C             On entry, the leading P-by-N part of this array must
C             contain the state/output matrix C of the system.
C             On exit, this matrix does not contain useful information.
C
C     LDC     INTEGER
C             The leading dimension of the array C.  LDC >= MAX(1,P).
C
C     D       (input) DOUBLE PRECISION array, dimension (LDD,M)
C             The leading P-by-M part of this array must contain the
C             direct transmission matrix D of the system.
C
C     LDD     INTEGER
C             The leading dimension of the array D.  LDD >= MAX(1,P).
C
C     NFZ     (output) INTEGER
C             The number of finite zeros.
C
C     NRANK   (output) INTEGER
C             The normal rank of the system pencil.
C
C     NIZ     (output) INTEGER
C             The number of infinite zeros.
C
C     DINFZ   (output) INTEGER
C             The maximal multiplicity of infinite Smith zeros.
C
C     NKROR   (output) INTEGER
C             The number of right Kronecker indices.
C
C     NINFE   (output) INTEGER
C             The number of elementary infinite blocks.
C
C     NKROL   (output) INTEGER
C             The number of left Kronecker indices.
C
C     INFZ    (output) INTEGER array, dimension (N+1)
C             The leading DINFZ elements of INFZ contain information
C             on the infinite elementary divisors as follows:
C             the system has INFZ(i) infinite elementary divisors in
C             the Smith form of degree i, where i = 1,2,...,DINFZ.
C
C     KRONR   (output) INTEGER array, dimension (N+1)
C             The leading NKROR elements of this array contain the
C             right Kronecker (column) indices.
C
C     INFE    (output) INTEGER array, dimension (N+1)
C             The leading NINFE elements of INFE contain the
C             multiplicities of infinite eigenvalues.
C
C     KRONL   (output) INTEGER array, dimension (N+1)
C             The leading NKROL elements of this array contain the
C             left Kronecker (row) indices.
C
C     E       (output) DOUBLE PRECISION array, dimension (LDE,N)
C             The leading NFZ-by-NFZ part of this array contains the
C             matrix Ef of the reduced pencil.
C
C     LDE     INTEGER
C             The leading dimension of the array E.  LDE >= MAX(1,N).
C
C     Tolerances
C
C     TOL     DOUBLE PRECISION
C             A tolerance used in rank decisions to determine the
C             effective rank, which is defined as the order of the
C             largest leading (or trailing) triangular submatrix in the
C             QR (or RQ) factorization with column (or row) pivoting
C             whose estimated condition number is less than 1/TOL.
C             If the user sets TOL <= 0, then an implicitly computed,
C             default tolerance  TOLDEF = MAX(N+P,N+M)**2*EPS,  is used
C             instead, where EPS is the machine precision (see LAPACK
C             Library routine DLAMCH).  TOL < 1.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (MAX(M,P))
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The length of the array DWORK.
C             LDWORK >= 1, if MAX(N,M,P) = 0; otherwise,
C             LDWORK >= MAX( MIN(P,M) + M + MAX(2*M,N) - 1,
C                            MIN(P,N) + MAX(N + MAX(P,M), 3*P - 1 ) ) +
C                            MAX(P+N,M+N)*MAX(P+N,M+N).
C
C             If LDWORK = -1, then a workspace query is assumed;
C             the routine only calculates the optimal size of the
C             DWORK array, returns this value as the first entry of
C             the DWORK array, and no error message related to LDWORK
C             is issued by XERBLA.
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
C     The routine extracts from the system matrix of a state space
C     system, (A-lambda*I,B,C,D), a regular pencil Af-lambda*Ef, which
C     has the finite zeros of the system as generalized eigenvalues.
C     The procedure has the following main computational steps:
C
C        (a) construct the (N+P)-by-(M+N) system pencil
C
C             S(lambda) = (B  A)-lambda*( 0  I );
C                         (D  C)        ( 0  0 )
C
C        (b) reduce S(lambda) to S1(lambda) with the same finite zeros
C            and right Kronecker structure, but with D of full row rank;
C
C        (c) reduce the pencil S1(lambda) to S2(lambda) with the same
C            finite zeros and with D square invertible;
C
C        (d) perform a unitary transformation on the columns of
C            S2(lambda) = (A-lambda*I   B), in order to reduce it to
C                         (     C       D)
C
C            (Af-lambda*Ef   X), with Y and Ef square invertible;
C            (     0         Y)
C
C        (e) compute the right and left Kronecker indices of the system
C            matrix, which, together with the multiplicities of the
C            finite and infinite eigenvalues, constitute the complete
C            set of structural invariants under strict equivalence
C            transformations of a linear system.
C
C     REFERENCES
C
C     [1] Svaricek, F.
C         Computation of the Structural Invariants of Linear
C         Multivariable Systems with an Extended Version of the
C         Program ZEROS.
C         System & Control Letters, 6, pp. 261-266, 1985.
C
C     [2] Emami-Naeini, A. and Van Dooren, P.
C         Computation of Zeros of Linear Multivariable Systems.
C         Automatica, 18, pp. 415-430, 1982.
C
C     NUMERICAL ASPECTS
C
C     The algorithm is backward stable (see [2] and [1]).
C
C     FURTHER COMMENTS
C
C     In order to compute the finite Smith zeros of the system
C     explicitly, a call to this routine may be followed by a call to
C     the LAPACK Library routine DGGEV.
C
C     CONTRIBUTOR
C
C     A. Varga, German Aerospace Center, DLR Oberpfaffenhofen.
C     V. Sima, Katholieke Univ. Leuven, Belgium, May 1999.
C
C     REVISIONS
C
C     A. Varga, May 2003, German Aerospace Center, DLR Oberpfaffenhofen.
C     V. Sima, Dec. 2016, Jan. 2017, Apr. 2017.
C
C     KEYWORDS
C
C     Generalized eigenvalue problem, Kronecker indices, multivariable
C     system, orthogonal transformation, structural invariant.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ONE, ZERO
      PARAMETER         ( ONE = 1.0D0, ZERO = 0.0D0 )
C     .. Scalar Arguments ..
      CHARACTER         EQUIL
      INTEGER           DINFZ, INFO, LDA, LDB, LDC, LDD, LDE, LDWORK,
     $                  M, N, NFZ, NINFE, NIZ, NKROL, NKROR, NRANK, P
      DOUBLE PRECISION  TOL
C     .. Array Arguments ..
      INTEGER           INFE(*),  INFZ(*),  IWORK(*), KRONL(*), KRONR(*)
      DOUBLE PRECISION  A(LDA,*), B(LDB,*), C(LDC,*), D(LDD,*),
     $                  DWORK(*), E(LDE,*)
C     .. Local Scalars ..
      LOGICAL           LEQUIL, LQUERY, QRET
      INTEGER           I, I0, I1, II, ITAU, J, JWORK, KABCD, LABCD2,
     $                  LDABCD, MM, MPM, MPN, MU, NN, NSINFE, NU, NU1,
     $                  PP, WRKOPT
      DOUBLE PRECISION  MAXRED, SVLMAX, TOLER
C     .. External Functions ..
      LOGICAL           LSAME
      DOUBLE PRECISION  DLAMCH, DLANGE
      EXTERNAL          DLAMCH, DLANGE, LSAME
C     .. External Subroutines ..
      EXTERNAL          AB08NY, DLACPY, DLASET, DORMRZ, DTZRZF, MA02BD,
     $                  TB01ID, TB01XD, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         DBLE, INT, MAX, MIN
C     .. Executable Statements ..
C
      INFO   = 0
      LDABCD = N + MAX( P, M )
      LABCD2 = LDABCD*LDABCD
      LEQUIL = LSAME( EQUIL, 'S' )
C
C     Test the input scalar arguments.
C
      IF( .NOT.LEQUIL .AND. .NOT.LSAME( EQUIL, 'N' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( P.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF( LDB.LT.1 .OR. ( M.GT.0 .AND. LDB.LT.N ) ) THEN
         INFO = -8
      ELSE IF( LDC.LT.MAX( 1, P ) ) THEN
         INFO = -10
      ELSE IF( LDD.LT.MAX( 1, P ) ) THEN
         INFO = -12
      ELSE IF( LDE.LT.MAX( 1, N ) ) THEN
         INFO = -25
      ELSE IF( TOL.GE.ONE ) THEN
         INFO = -26
      ELSE
         MPN  = MIN( P, N )
         MPM  = MIN( P, M )
         QRET = MAX( N, M, P ).EQ.0
         LQUERY = ( LDWORK.EQ.-1 )
         IF( QRET ) THEN
            JWORK = 1
         ELSE
            JWORK = MAX( MPM + M + MAX( 2*M, N ) - 1,
     $                   MPN + MAX( LDABCD, 3*P  - 1 ) ) + LABCD2
         END IF
         IF( LQUERY ) THEN
            IF( QRET ) THEN
               WRKOPT = 1
            ELSE
               SVLMAX = ZERO
               NIZ    = 0
               CALL AB08NY( .TRUE., N, M, P, SVLMAX, DWORK, LDABCD, NIZ,
     $                      NU, MU, DINFZ, NKROL, INFZ, KRONL, TOL,
     $                      IWORK, DWORK, -1, INFO )
               WRKOPT = MAX( JWORK, LABCD2 + INT( DWORK(1) ) )
               CALL AB08NY( .FALSE., N, M, M, SVLMAX, DWORK, LDABCD,
     $                      NIZ, NU, MU, I1, NKROR, IWORK, KRONR, TOL,
     $                      IWORK, DWORK, -1, INFO )
               WRKOPT = MAX( WRKOPT, LABCD2 + INT( DWORK(1) ) )
               CALL DTZRZF( MPM, N+MPM, DWORK, LDABCD, DWORK, DWORK, -1,
     $                      INFO )
               WRKOPT = MAX( WRKOPT, LABCD2 + MPM + INT( DWORK(1) ) )
               CALL DORMRZ( 'Right', 'Transpose', N, N+MPM, MPM, N,
     $                      DWORK, LDABCD, DWORK, DWORK, LDABCD, DWORK,
     $                      -1, INFO )
               WRKOPT = MAX( WRKOPT, LABCD2 + MPM + INT( DWORK(1) ) )
            END IF
         ELSE IF( LDWORK.LT.JWORK ) THEN
            INFO = -29
         END IF
      END IF
C
      IF( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'AB08NW', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         DWORK(1) = WRKOPT
         RETURN
      END IF
C
      NIZ   = 0
      NKROL = 0
      NKROR = 0
      NINFE = 0
C
C     Quick return if possible.
C
      IF( QRET ) THEN
         DINFZ = 0
         NFZ   = 0
         NRANK = 0
         DWORK(1) = ONE
         RETURN
      END IF
C
C     (Note: Comments in the code beginning "Workspace:" describe the
C     minimal amount of real workspace needed at that point in the
C     code, as well as the preferred amount for good performance.)
C
      WRKOPT = 1
      KABCD  = 1
      JWORK  = KABCD + LABCD2
C
C     If required, balance the system pencil.
C     Workspace: need   N.
C
      IF( LEQUIL ) THEN
         MAXRED = ZERO
         CALL TB01ID( 'A', N, M, P, MAXRED, A, LDA, B, LDB, C, LDC,
     $                DWORK, INFO )
         WRKOPT = N
      END IF
C
C     Construct the system pencil
C
C                      ( B A-lambda*I )
C         S(lambda) =  (              )
C                      ( D     C      )
C
C     of dimension (N+P)-by-(M+N).
C
      NN = N
      MM = M
      PP = P
C
      CALL DLACPY( 'Full', N, M, B, LDB, DWORK(KABCD),          LDABCD )
      CALL DLACPY( 'Full', P, M, D, LDD, DWORK(KABCD+N),        LDABCD )
      CALL DLACPY( 'Full', N, N, A, LDA, DWORK(KABCD+LDABCD*M), LDABCD )
      CALL DLACPY( 'Full', P, N, C, LDC, DWORK(KABCD+LDABCD*M+N),
     $              LDABCD )
C
C     If required, set tolerance.
C
      TOLER = TOL
      IF( TOLER.LE.ZERO ) THEN
         TOLER = DBLE( LABCD2 ) * DLAMCH( 'Precision' )
      END IF
      SVLMAX = DLANGE( 'Frobenius', NN+PP, NN+MM, DWORK(KABCD), LDABCD,
     $                 DWORK(JWORK) )
C
C     Extract the reduced pencil S1(lambda)
C
C             ( Bc  Ac-lambda*I ),
C             ( Dc      Cc      )
C
C     having the same finite Smith zeros as the system pencil S(lambda),
C     but with Dc, a MU-by-MM full row rank left upper-trapezoidal
C     matrix, with the first MU columns in an upper triangular form.
C
C     Workspace: need   MAX( MIN(P,M) + M + MAX(2*M,N) - 1,
C                            MIN(P,N) + MAX(N + MAX(P,M), 3*P - 1 ) ).
C                prefer larger.
C     Int.work.  need   MAX(M,P).
C
      CALL AB08NY( .TRUE., NN, MM, PP, SVLMAX, DWORK(KABCD), LDABCD,
     $             NIZ, NU, MU, DINFZ, NKROL, INFZ, KRONL, TOLER, IWORK,
     $             DWORK(JWORK), LDWORK-JWORK+1, INFO )
C
      WRKOPT = MAX( WRKOPT, INT( DWORK(JWORK) ) + JWORK - 1 )
C
C     Set the number of simple (non-dynamic) infinite eigenvalues, and
C     the normal rank of the system pencil.
C
      NSINFE = MU
      NRANK  = NN + MU
C
C     Pertranspose the system.
C
      CALL TB01XD( 'D', NU, MM, MM, MAX( 0, NU-1 ), MAX( 0, NU-1 ),
     $              DWORK(KABCD+LDABCD*MM),    LDABCD,
     $              DWORK(KABCD),              LDABCD,
     $              DWORK(KABCD+LDABCD*MM+NU), LDABCD,
     $              DWORK(KABCD+NU),           LDABCD, INFO )
      CALL MA02BD( 'Right', NU+MM, MM, DWORK(KABCD),    LDABCD )
      CALL MA02BD( 'Left',  MM, NU+MM, DWORK(KABCD+NU), LDABCD )
C
      IF( MU.NE.MM ) THEN
         NN = NU
         PP = MM
         MM = MU
         KABCD = KABCD + ( PP - MM )*LDABCD
C
C        Extract the reduced pencil S2(lambda) to
C
C             ( Br  Ar-lambda*I ),
C             ( Dr      Cr      )
C
C        having the same finite Smith zeros as the pencil S(lambda),
C        but with Dr, a MU-by-MU invertible upper-triangular matrix.
C
C        Workspace: need  MAX( MIN(P,M) + M + MAX(2*M,N) - 1,
C                              MIN(P,N) + MAX(N + MAX(P,M), 3*P-1 ) ).
C                   prefer larger.
C
         CALL AB08NY( .FALSE., NN, MM, PP, SVLMAX, DWORK(KABCD), LDABCD,
     $                I0, NU, MU, I1, NKROR, IWORK, KRONR, TOLER,
     $                IWORK, DWORK(JWORK), LDWORK-JWORK+1, INFO )
C
         WRKOPT = MAX( WRKOPT, INT( DWORK(JWORK) ) + JWORK - 1 )
      END IF
C
      IF( MIN( NU, MU ).NE.0 ) THEN
C
C        Perform a unitary transformation on the columns of
C                     ( Br Ar-lambda*I  ),
C                     ( Dr     Cr       )
C        in order to reduce it to
C                     ( Af-lambda*Ef  * ),
C                     (      0        Y )
C        with Y and Ef square invertible.
C
         NU1   = NU + KABCD
         I1    = NU + MU
         ITAU  = JWORK
         JWORK = ITAU + MU
C
C        Workspace: need   2*MIN(P,M);
C                   prefer MIN(P,M) + MIN(P,M)*NB.
C
         CALL DTZRZF( MU, I1, DWORK(NU1), LDABCD, DWORK(ITAU),
     $                DWORK(JWORK), LDWORK-JWORK+1, INFO )
         WRKOPT = MAX( WRKOPT, INT( DWORK(JWORK) ) + JWORK - 1 )
C
C        Compute and save Af.
C
C        Workspace: need   MIN(P,M) + N;
C                   prefer MIN(P,M) + N*NB.
C
         CALL DORMRZ( 'Right', 'Transpose', NU, I1, MU, NU,
     $                DWORK(NU1), LDABCD, DWORK(ITAU), DWORK(KABCD),
     $                LDABCD, DWORK(JWORK), LDWORK-JWORK+1, INFO )
         WRKOPT = MAX( WRKOPT, INT( DWORK(JWORK) ) + JWORK - 1 )
C
         CALL DLACPY( 'Full', NU, NU, DWORK(KABCD+MU*LDABCD), LDABCD, A,
     $                LDA )
C
C        Compute and save Ef.
C
         CALL DLASET( 'Full', NU, MU, ZERO, ZERO, DWORK(KABCD), LDABCD )
         CALL DLASET( 'Full', NU, NU, ZERO, ONE, DWORK(KABCD+MU*LDABCD),
     $                LDABCD )
         CALL DORMRZ( 'Right', 'Transpose', NU, I1, MU, NU,
     $                DWORK(NU1), LDABCD, DWORK(ITAU), DWORK(KABCD),
     $                LDABCD, DWORK(JWORK), LDWORK-JWORK+1, INFO )
         WRKOPT = MAX( WRKOPT, INT( DWORK(JWORK) ) + JWORK - 1 )
C
         CALL DLACPY( 'Full', NU, NU, DWORK(KABCD+MU*LDABCD), LDABCD, E,
     $                LDE )
      ELSE
C
C        Save Af and set Ef.
C
         CALL DLACPY( 'Full', NU, NU, DWORK(KABCD+MU*LDABCD), LDABCD, A,
     $                LDA )
         CALL DLASET( 'Full', NU, NU, ZERO, ONE, E, LDE )
      END IF
C
      NFZ = NU
C
C     Set right Kronecker indices (column indices).
C
      DO 30 I = 1, NKROR
         IWORK(I) = KRONR(I)
   30 CONTINUE
C
      J = 0
C
      DO 50 I = 1, NKROR
         DO 40 II = J + 1, J + IWORK(I)
            KRONR(II) = I - 1
   40    CONTINUE
C
         J = J + IWORK(I)
   50 CONTINUE
C
      NKROR = J
C
C     Set left Kronecker indices (row indices).
C
      DO 60 I = 1, NKROL
         IWORK(I) = KRONL(I)
   60 CONTINUE
C
      J = 0
C
      DO 80 I = 1, NKROL
         DO 70 II = J + 1, J + IWORK(I)
            KRONL(II) = I - 1
   70    CONTINUE
C
         J = J + IWORK(I)
   80 CONTINUE
C
      NKROL = J
C
C     Determine the number of simple infinite blocks as the difference
C     between the order of Dr and the number of infinite blocks of order
C     greater than one.
C
      DO 90 I = 1, DINFZ
         NINFE = NINFE + INFZ(I)
   90 CONTINUE
C
      NINFE = NSINFE - NINFE
C
      DO 100 I = 1, NINFE
         INFE(I) = 1
  100 CONTINUE
C
C     Set the structure of infinite eigenvalues.
C
      DO 120 I = 1, DINFZ
C
         DO 110 II = NINFE + 1, NINFE + INFZ(I)
            INFE(II) = I + 1
  110    CONTINUE
C
         NINFE = NINFE + INFZ(I)
  120 CONTINUE
C
      DWORK(1) = WRKOPT
      RETURN
C *** Last line of AB08NW ***
      END
