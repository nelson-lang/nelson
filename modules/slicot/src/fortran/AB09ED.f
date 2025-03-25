      SUBROUTINE AB09ED( DICO, EQUIL, ORDSEL, N, M, P, NR, ALPHA,
     $                   A, LDA, B, LDB, C, LDC, D, LDD, NS, HSV, TOL1,
     $                   TOL2, IWORK, DWORK, LDWORK, IWARN, INFO )
C
C     PURPOSE
C
C     To compute a reduced order model (Ar,Br,Cr,Dr) for an original
C     state-space representation (A,B,C,D) by using the optimal
C     Hankel-norm approximation method in conjunction with square-root
C     balancing for the ALPHA-stable part of the system.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     DICO    CHARACTER*1
C             Specifies the type of the original system as follows:
C             = 'C':  continuous-time system;
C             = 'D':  discrete-time system.
C
C     EQUIL   CHARACTER*1
C             Specifies whether the user wishes to preliminarily
C             equilibrate the triplet (A,B,C) as follows:
C             = 'S':  perform equilibration (scaling);
C             = 'N':  do not perform equilibration.
C
C     ORDSEL  CHARACTER*1
C             Specifies the order selection method as follows:
C             = 'F':  the resulting order NR is fixed;
C             = 'A':  the resulting order NR is automatically determined
C                     on basis of the given tolerance TOL1.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the original state-space representation, i.e.
C             the order of the matrix A.  N >= 0.
C
C     M       (input) INTEGER
C             The number of system inputs.  M >= 0.
C
C     P       (input) INTEGER
C             The number of system outputs.  P >= 0.
C
C     NR      (input/output) INTEGER
C             On entry with ORDSEL = 'F', NR is the desired order of
C             the resulting reduced order system.  0 <= NR <= N.
C             On exit, if INFO = 0, NR is the order of the resulting
C             reduced order model. For a system with NU ALPHA-unstable
C             eigenvalues and NS ALPHA-stable eigenvalues (NU+NS = N),
C             NR is set as follows: if ORDSEL = 'F', NR is equal to
C             NU+MIN(MAX(0,NR-NU-KR+1),NMIN), where KR is the
C             multiplicity of the Hankel singular value HSV(NR-NU+1),
C             NR is the desired order on entry, and NMIN is the order
C             of a minimal realization of the ALPHA-stable part of the
C             given system; NMIN is determined as the number of Hankel
C             singular values greater than NS*EPS*HNORM(As,Bs,Cs), where
C             EPS is the machine precision (see LAPACK Library Routine
C             DLAMCH) and HNORM(As,Bs,Cs) is the Hankel norm of the
C             ALPHA-stable part of the given system (computed in
C             HSV(1));
C             if ORDSEL = 'A', NR is the sum of NU and the number of
C             Hankel singular values greater than
C             MAX(TOL1,NS*EPS*HNORM(As,Bs,Cs)).
C
C     ALPHA   (input) DOUBLE PRECISION
C             Specifies the ALPHA-stability boundary for the eigenvalues
C             of the state dynamics matrix A. For a continuous-time
C             system (DICO = 'C'), ALPHA <= 0 is the boundary value for
C             the real parts of eigenvalues, while for a discrete-time
C             system (DICO = 'D'), 0 <= ALPHA <= 1 represents the
C             boundary value for the moduli of eigenvalues.
C             The ALPHA-stability domain does not include the boundary.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the state dynamics matrix A.
C             On exit, if INFO = 0, the leading NR-by-NR part of this
C             array contains the state dynamics matrix Ar of the
C             reduced order system in a real Schur form.
C             The resulting A has a block-diagonal form with two blocks.
C             For a system with NU ALPHA-unstable eigenvalues and
C             NS ALPHA-stable eigenvalues (NU+NS = N), the leading
C             NU-by-NU block contains the unreduced part of A
C             corresponding to ALPHA-unstable eigenvalues.
C             The trailing (NR+NS-N)-by-(NR+NS-N) block contains
C             the reduced part of A corresponding to ALPHA-stable
C             eigenvalues.
C
C     LDA     INTEGER
C             The leading dimension of array A.  LDA >= MAX(1,N).
C
C     B       (input/output) DOUBLE PRECISION array, dimension (LDB,M)
C             On entry, the leading N-by-M part of this array must
C             contain the original input/state matrix B.
C             On exit, if INFO = 0, the leading NR-by-M part of this
C             array contains the input/state matrix Br of the reduced
C             order system.
C
C     LDB     INTEGER
C             The leading dimension of array B.  LDB >= MAX(1,N).
C
C     C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
C             On entry, the leading P-by-N part of this array must
C             contain the original state/output matrix C.
C             On exit, if INFO = 0, the leading P-by-NR part of this
C             array contains the state/output matrix Cr of the reduced
C             order system.
C
C     LDC     INTEGER
C             The leading dimension of array C.  LDC >= MAX(1,P).
C
C     D       (input/output) DOUBLE PRECISION array, dimension (LDD,M)
C             On entry, the leading P-by-M part of this array must
C             contain the original input/output matrix D.
C             On exit, if INFO = 0, the leading P-by-M part of this
C             array contains the input/output matrix Dr of the reduced
C             order system.
C
C     LDD     INTEGER
C             The leading dimension of array D.  LDD >= MAX(1,P).
C
C     NS      (output) INTEGER
C             The dimension of the ALPHA-stable subsystem.
C
C     HSV     (output) DOUBLE PRECISION array, dimension (N)
C             If INFO = 0, the leading NS elements of HSV contain the
C             Hankel singular values of the ALPHA-stable part of the
C             original system ordered decreasingly.
C             HSV(1) is the Hankel norm of the ALPHA-stable subsystem.
C
C     Tolerances
C
C     TOL1    DOUBLE PRECISION
C             If ORDSEL = 'A', TOL1 contains the tolerance for
C             determining the order of reduced system.
C             For model reduction, the recommended value is
C             TOL1 = c*HNORM(As,Bs,Cs), where c is a constant in the
C             interval [0.00001,0.001], and HNORM(As,Bs,Cs) is the
C             Hankel-norm of the ALPHA-stable part of the given system
C             (computed in HSV(1)).
C             If TOL1 <= 0 on entry, the used default value is
C             TOL1 = NS*EPS*HNORM(As,Bs,Cs), where NS is the number of
C             ALPHA-stable eigenvalues of A and EPS is the machine
C             precision (see LAPACK Library Routine DLAMCH).
C             This value is appropriate to compute a minimal realization
C             of the ALPHA-stable part.
C             If ORDSEL = 'F', the value of TOL1 is ignored.
C
C     TOL2    DOUBLE PRECISION
C             The tolerance for determining the order of a minimal
C             realization of the ALPHA-stable part of the given system.
C             The recommended value is TOL2 = NS*EPS*HNORM(As,Bs,Cs).
C             This value is used by default if TOL2 <= 0 on entry.
C             If TOL2 > 0, then TOL2 <= TOL1.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (LIWORK)
C             LIWORK = MAX(1,M),   if DICO = 'C';
C             LIWORK = MAX(1,N,M), if DICO = 'D'.
C             On exit, if INFO = 0, IWORK(1) contains NMIN, the order of
C             the computed minimal realization.
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The length of the array DWORK.
C             LDWORK >= MAX( LDW1, LDW2 ), where
C             LDW1 = N*(2*N + MAX(N,M,P) + 5) + N*(N+1)/2,
C             LDW2 = N*(M+P+2) + 2*M*P + MIN(N,M) +
C                    MAX( 3*M+1, MIN(N,M)+P ).
C             For optimum performance LDWORK should be larger.
C
C     Warning Indicator
C
C     IWARN   INTEGER
C             = 0:  no warning;
C             = 1:  with ORDSEL = 'F', the selected order NR is greater
C                   than NSMIN, the sum of the order of the
C                   ALPHA-unstable part and the order of a minimal
C                   realization of the ALPHA-stable part of the given
C                   system. In this case, the resulting NR is set equal
C                   to NSMIN.
C             = 2:  with ORDSEL = 'F', the selected order NR is less
C                   than the order of the ALPHA-unstable part of the
C                   given system. In this case NR is set equal to the
C                   order of the ALPHA-unstable part.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value;
C             = 1:  the computation of the ordered real Schur form of A
C                   failed;
C             = 2:  the separation of the ALPHA-stable/unstable diagonal
C                   blocks failed because of very close eigenvalues;
C             = 3:  the computed ALPHA-stable part is just stable,
C                   having stable eigenvalues very near to the imaginary
C                   axis (if DICO = 'C') or to the unit circle
C                   (if DICO = 'D');
C             = 4:  the computation of Hankel singular values failed;
C             = 5:  the computation of stable projection in the
C                   Hankel-norm approximation algorithm failed;
C             = 6:  the order of computed stable projection in the
C                   Hankel-norm approximation algorithm differs
C                   from the order of Hankel-norm approximation.
C
C     METHOD
C
C     Let be the following linear system
C
C          d[x(t)] = Ax(t) + Bu(t)
C          y(t)    = Cx(t) + Du(t)                           (1)
C
C     where d[x(t)] is dx(t)/dt for a continuous-time system and x(t+1)
C     for a discrete-time system. The subroutine AB09ED determines for
C     the given system (1), the matrices of a reduced order system
C
C          d[z(t)] = Ar*z(t) + Br*u(t)
C          yr(t)   = Cr*z(t) + Dr*u(t)                       (2)
C
C     such that
C
C     HSV(NR+NS-N) <= INFNORM(G-Gr) <= 2*[HSV(NR+NS-N+1)+...+HSV(NS)],
C
C     where G and Gr are transfer-function matrices of the systems
C     (A,B,C,D) and (Ar,Br,Cr,Dr), respectively, and INFNORM(G) is the
C     infinity-norm of G.
C
C     The following procedure is used to reduce a given G:
C
C     1) Decompose additively G as
C
C          G = G1 + G2
C
C        such that G1 = (As,Bs,Cs,D) has only ALPHA-stable poles and
C        G2 = (Au,Bu,Cu,0) has only ALPHA-unstable poles.
C
C     2) Determine G1r, a reduced order approximation of the
C        ALPHA-stable part G1.
C
C     3) Assemble the reduced model Gr as
C
C           Gr = G1r + G2.
C
C     To reduce the ALPHA-stable part G1, the optimal Hankel-norm
C     approximation method of [1], based on the square-root
C     balancing projection formulas of [2], is employed.
C
C     REFERENCES
C
C     [1] Glover, K.
C         All optimal Hankel norm approximation of linear
C         multivariable systems and their L-infinity error bounds.
C         Int. J. Control, Vol. 36, pp. 1145-1193, 1984.
C
C     [2] Tombs M.S. and Postlethwaite I.
C         Truncated balanced realization of stable, non-minimal
C         state-space systems.
C         Int. J. Control, Vol. 46, pp. 1319-1330, 1987.
C
C     NUMERICAL ASPECTS
C
C     The implemented methods rely on an accuracy enhancing square-root
C     technique.
C                                         3
C     The algorithms require less than 30N  floating point operations.
C
C     CONTRIBUTOR
C
C     A. Varga, German Aerospace Center, DLR Oberpfaffenhofen,
C     July 1998.
C     Based on the RASP routines SADSDC and OHNAP.
C
C     REVISIONS
C
C     Nov. 1998, V. Sima, Research Institute for Informatics, Bucharest.
C     Dec. 1998, V. Sima, Katholieke Univ. Leuven, Leuven.
C     Nov. 2000, A. Varga, DLR Oberpfaffenhofen.
C     March 26, 2005, V. Sima, Research Institute for Informatics.
C
C     KEYWORDS
C
C     Balancing, Hankel-norm approximation, model reduction,
C     multivariable system, state-space model.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  C100, ONE, ZERO
      PARAMETER         ( C100 = 100.0D0, ONE = 1.0D0, ZERO = 0.0D0 )
C     .. Scalar Arguments ..
      CHARACTER         DICO, EQUIL, ORDSEL
      INTEGER           INFO, IWARN, LDA, LDB, LDC, LDD, LDWORK,
     $                  M, N, NR, NS, P
      DOUBLE PRECISION  ALPHA, TOL1, TOL2
C     .. Array Arguments ..
      INTEGER           IWORK(*)
      DOUBLE PRECISION  A(LDA,*), B(LDB,*), C(LDC,*), D(LDD,*),
     $                  DWORK(*), HSV(*)
C     .. Local Scalars ..
      LOGICAL           DISCR, FIXORD
      INTEGER           IERR, IWARNL, KI, KL, KU, KW, NRA, NU, NU1
      DOUBLE PRECISION  ALPWRK, MAXRED, WRKOPT
C     .. External Functions ..
      LOGICAL           LSAME
      DOUBLE PRECISION  DLAMCH
      EXTERNAL          DLAMCH, LSAME
C     .. External Subroutines ..
      EXTERNAL          AB09CX, TB01ID, TB01KD, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         MAX, MIN, SQRT
C     .. Executable Statements ..
C
      INFO   = 0
      IWARN  = 0
      DISCR  = LSAME( DICO,   'D' )
      FIXORD = LSAME( ORDSEL, 'F' )
C
C     Check the input scalar arguments.
C
      IF( .NOT. ( LSAME( DICO, 'C' ) .OR. DISCR ) ) THEN
         INFO = -1
      ELSE IF( .NOT. ( LSAME( EQUIL, 'S' ) .OR.
     $                 LSAME( EQUIL, 'N' ) ) ) THEN
         INFO = -2
      ELSE IF( .NOT. ( FIXORD .OR. LSAME( ORDSEL, 'A' ) ) ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( M.LT.0 ) THEN
         INFO = -5
      ELSE IF( P.LT.0 ) THEN
         INFO = -6
      ELSE IF( FIXORD .AND. ( NR.LT.0 .OR. NR.GT.N ) ) THEN
         INFO = -7
      ELSE IF( ( DISCR .AND. ( ALPHA.LT.ZERO .OR. ALPHA.GT.ONE ) ) .OR.
     $    ( .NOT.DISCR .AND.   ALPHA.GT.ZERO ) ) THEN
         INFO = -8
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -10
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -12
      ELSE IF( LDC.LT.MAX( 1, P ) ) THEN
         INFO = -14
      ELSE IF( LDD.LT.MAX( 1, P ) ) THEN
         INFO = -16
      ELSE IF( TOL2.GT.ZERO .AND. TOL2.GT.TOL1 ) THEN
         INFO = -20
      ELSE IF( LDWORK.LT.MAX( N*( 2*N + MAX( N, M, P ) + 5 ) +
     $                        ( N*( N + 1 ) )/2,
     $                        N*( M + P + 2 ) + 2*M*P + MIN( N, M ) +
     $                        MAX ( 3*M + 1, MIN( N, M ) + P ) ) ) THEN
         INFO = -23
      END IF
C
      IF( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'AB09ED', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( MIN( N, M, P ).EQ.0 ) THEN
         NR = 0
         NS = 0
         IWORK(1) = 0
         DWORK(1) = ONE
         RETURN
      END IF
C
      IF( LSAME( EQUIL, 'S' ) ) THEN
C
C        Scale simultaneously the matrices A, B and C:
C        A <- inv(D)*A*D,  B <- inv(D)*B and C <- C*D, where D is a
C        diagonal matrix.
C        Workspace: N.
C
         MAXRED = C100
         CALL TB01ID( 'All', N, M, P, MAXRED, A, LDA, B, LDB, C, LDC,
     $                DWORK, INFO )
      END IF
C
C     Correct the value of ALPHA to ensure stability.
C
      ALPWRK = ALPHA
      IF( DISCR ) THEN
         IF( ALPHA.EQ.ONE ) ALPWRK = ONE - SQRT( DLAMCH( 'E' ) )
      ELSE
         IF( ALPHA.EQ.ZERO ) ALPWRK = -SQRT( DLAMCH( 'E' ) )
      END IF
C
C     Allocate working storage.
C
      KU = 1
      KL = KU + N*N
      KI = KL + N
      KW = KI + N
C
C     Reduce A to a block-diagonal real Schur form, with the
C     ALPHA-unstable part in the leading diagonal position, using a
C     non-orthogonal similarity transformation A <- inv(T)*A*T and
C     apply the transformation to B and C: B <- inv(T)*B and C <- C*T.
C
C     Workspace needed:      N*(N+2);
C     Additional workspace:  need   3*N;
C                            prefer larger.
C
      CALL TB01KD( DICO, 'Unstable', 'General', N, M, P, ALPWRK, A, LDA,
     $             B, LDB, C, LDC, NU, DWORK(KU), N, DWORK(KL),
     $             DWORK(KI), DWORK(KW), LDWORK-KW+1, IERR )
C
      IF( IERR.NE.0 ) THEN
         IF( IERR.NE.3 ) THEN
            INFO = 1
         ELSE
            INFO = 2
         END IF
         RETURN
      END IF
C
      WRKOPT = DWORK(KW) + DBLE( KW-1 )
C
C     Determine a reduced order approximation of the ALPHA-stable part.
C
C     Workspace: need   MAX( LDW1, LDW2 ),
C                LDW1 = N*(2*N + MAX(N,M,P) + 5) + N*(N+1)/2,
C                LDW2 = N*(M+P+2) + 2*M*P + MIN(N,M) +
C                       MAX( 3*M+1, MIN(N,M)+P );
C                prefer larger.
C
      IWARNL = 0
      NS = N - NU
      IF( FIXORD ) THEN
         NRA = MAX( 0, NR-NU )
         IF( NR.LT.NU )
     $      IWARNL = 2
      ELSE
         NRA = 0
      END IF
C
C     Finish if only unstable part is present.
C
      IF( NS.EQ.0 ) THEN
         NR = NU
         DWORK(1) = WRKOPT
         RETURN
      END IF
C
      NU1 = NU + 1
      CALL AB09CX( DICO, ORDSEL, NS, M, P, NRA, A(NU1,NU1), LDA,
     $             B(NU1,1), LDB, C(1,NU1), LDC, D, LDD, HSV, TOL1,
     $             TOL2, IWORK, DWORK, LDWORK, IWARN, IERR )
C
      IWARN = MAX( IWARN, IWARNL )
      IF( IERR.NE.0 ) THEN
         INFO = IERR + 2
         RETURN
      END IF
C
      NR = NRA + NU
C
      DWORK(1) = MAX( WRKOPT, DWORK(1) )
C
      RETURN
C *** Last line of AB09ED ***
      END
