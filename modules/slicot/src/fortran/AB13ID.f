      LOGICAL FUNCTION AB13ID( JOBSYS, JOBEIG, EQUIL, CKSING, RESTOR,
     $                         UPDATE, N, M, P, A, LDA, E, LDE, B, LDB,
     $                         C, LDC, NR, RANKE, TOL, IWORK, DWORK,
     $                         LDWORK, IWARN, INFO )
C
C     PURPOSE
C
C     To check whether the transfer function
C
C                                     -1
C       G(lambda) := C*( lambda*E - A ) *B
C
C     of a given linear time-invariant descriptor system with
C     generalized state space realization (lambda*E-A,B,C) is proper.
C     Optionally, if JOBEIG = 'A', the system (lambda*E-A,B,C) is
C     reduced to an equivalent one (lambda*Er-Ar,Br,Cr) with only
C     controllable and observable eigenvalues in order to use it for a
C     subsequent L_inf-norm computation; if JOBEIG = 'I', the system is
C     reduced to an equivalent one (lambda*Er-Ar,Br,Cr) without
C     uncontrollable and unobservable infinite eigenvalues. In this
C     case, intended mainly for checking the properness, the returned
C     system is not fully reduced, unless UPDATE = 'U'.
C
C     FUNCTION VALUE
C
C     AB13ID  LOGICAL
C             Indicates whether the transfer function is proper.
C             If AB13ID = .TRUE., the transfer function is proper;
C             otherwise, it is improper.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOBSYS  CHARACTER*1
C             Indicates whether the system (lambda*E-A,B,C) is already
C             in the reduced form which is obtained as stated in
C             JOBEIG, as follows.
C             = 'R': The system is not in a reduced form, the reduction
C                    step is performed;
C             = 'N': The system is in a reduced form; the reduction step
C                    is omitted.
C
C     JOBEIG  CHARACTER*1
C             Indicates which kind of eigenvalues of the matrix pencil
C             lambda*E-A should be removed if JOBSYS = 'R', as follows:
C             = 'A': All uncontrollable and unobservable eigenvalues
C                    are removed; the reduced system is returned in
C                    the arrays A, E, B, C;
C             = 'I': Only all uncontrollable and unobservable infinite
C                    eigenvalues are removed; the returned system is not
C                    fully reduced if UPDATE = 'N'.
C
C     EQUIL   CHARACTER*1
C             Specifies whether the user wishes to preliminarily scale
C             the system (lambda*E-A,B,C) as follows:
C             = 'S': Perform scaling;
C             = 'N': Do not perform scaling.
C
C     CKSING  CHARACTER*1
C             Specifies whether the user wishes to check if the pencil
C             (lambda*E-A) is singular as follows:
C             = 'C':  Check singularity;
C             = 'N':  Do not check singularity.
C             If the pencil is singular, the reduced system computed for
C             CKSING = 'N' may have completely different eigenvalues
C             than the given system.
C             The test is performed only if JOBSYS = 'R'.
C
C     RESTOR  CHARACTER*1
C             Specifies whether the user wishes to save the system
C             matrices before each reduction phase (if JOBSYS = 'R') and
C             restore them if no order reduction took place as follows:
C             = 'R':  Save and restore;
C             = 'N':  Do not save the matrices.
C             This option is ineffective if JOBSYS = 'N'.
C
C     UPDATE  CHARACTER*1
C             Specifies whether the user wishes to update the matrices
C             A, B, and C if JOBEIG = 'I' as follows:
C             = 'U':  Update the matrices A, B and C;
C             = 'N':  Do not update the matrices A, B and C when
C                     performing URV decomposition of the matrix E
C                     (see METHOD).
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The dimension of the descriptor state vector; also the
C             order of square matrices A and E, the number of rows of
C             matrix B, and the number of columns of matrix C.  N >= 0.
C
C     M       (input) INTEGER
C             The dimension of the descriptor system input vector; also
C             the number of columns of matrix B.  M >= 0.
C
C     P       (input) INTEGER
C             The dimension of the descriptor system output vector; also
C             the number of rows of matrix C.  P >= 0.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the state matrix A.
C             On exit, if JOBSYS = 'R' and JOBEIG = 'A', the leading
C             NR-by-NR part of this array contains the reduced order
C             state matrix Ar of a fully controllable and observable
C             realization for the original system. If JOBSYS = 'R' and
C             JOBEIG = 'I', the leading NR-by-NR part of this array
C             contains the reduced order state matrix Ar of a
C             transformed system without uncontrollable and unobservable
C             infinite poles. In this case, the matrix Ar does not
C             correspond to the returned matrix Er (obtained after a
C             URV decomposition), unless UPDATE = 'U' or RANKE < NR.
C             On exit, if JOBSYS = 'N' and (JOBEIG = 'A' or UPDATE = 'U'
C             or RANKE < N), the leading N-by-N part of this array
C             contains the transformed matrix A corresponding to the
C             URV decomposition of E (see (2) in METHOD), and if
C             JOBEIG = 'I' and UPDATE = 'N', the submatrix A22 in (2) is
C             further transformed to estimate its rank.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     E       (input/output) DOUBLE PRECISION array, dimension (LDE,N)
C             On entry, the leading N-by-N part of this array must
C             contain the descriptor matrix E.
C             On exit, if JOBSYS = 'R' and JOBEIG = 'A', the leading
C             NR-by-NR part of this array contains the reduced order
C             descriptor matrix Er of a completely controllable and
C             observable realization for the original system. The
C             reduced matrix Er is in upper triangular form.
C             If JOBSYS = 'R' and JOBEIG = 'I', the leading NR-by-NR
C             part of this array contains the reduced order descriptor
C             matrix Er of a transformed system without uncontrollable
C             and unobservable infinite poles. The reduced matrix Er is
C             upper triangular. In both cases, or if JOBSYS = 'N', the
C             matrix Er results from a URV decomposition of the matrix E
C             (see METHOD).
C
C     LDE     INTEGER
C             The leading dimension of the array E.  LDE >= MAX(1,N).
C
C     B       (input/output) DOUBLE PRECISION array, dimension
C             (LDB,MAX(M,P))
C             On entry, the leading N-by-M part of this array must
C             contain the input matrix B; the remainder of the leading
C             N-by-MAX(M,P) part is used as internal workspace.
C             On exit, if JOBSYS = 'R' and JOBEIG = 'A', the leading
C             NR-by-M part of this array contains the reduced input
C             matrix Br of a completely controllable and observable
C             realization for the original system. If JOBSYS = 'R' and
C             JOBEIG = 'I', the leading NR-by-M part of this array
C             contains the transformed input matrix Br obtained after
C             removing the uncontrollable and unobservable infinite
C             poles; the transformations for the URV decomposition of
C             the matrix E are not applied if UPDATE = 'N'.
C             On exit, if JOBSYS = 'N' and (JOBEIG = 'A' or
C             UPDATE = 'U'), the leading N-by-M part of this array
C             contains the transformed matrix B corresponding to the
C             URV decomposition of E, but if JOBEIG = 'I', EQUIL = 'N'
C             and UPDATE = 'N', the array B is unchanged on exit.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= MAX(1,N).
C
C     C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
C             On entry, the leading P-by-N part of this array must
C             contain the output matrix C; the remainder of the leading
C             MAX(M,P)-by-N part is used as internal workspace.
C             On exit, if JOBSYS = 'R' and JOBEIG = 'A', the leading
C             P-by-NR part of this array contains the transformed output
C             matrix Cr of a completely controllable and observable
C             realization for the original system. If JOBSYS = 'R' and
C             JOBEIG = 'I', the leading P-by-NR part of this array
C             contains the transformed output matrix Cr obtained after
C             removing the uncontrollable and unobservable infinite
C             poles; the transformations for the URV decomposition of
C             the matrix E are not applied if UPDATE = 'N'.
C             On exit, if JOBSYS = 'N' and (JOBEIG = 'A' or
C             UPDATE = 'U'), the leading P-by-N part of this array
C             contains the transformed matrix C corresponding to the
C             URV decomposition of E, but if JOBEIG = 'I', EQUIL = 'N'
C             and UPDATE = 'N', the array C is unchanged on exit.
C
C     LDC     INTEGER
C             The leading dimension of the array C.
C             LDC >= MAX(1,M,P) if N > 0;
C             LDC >= 1          if N = 0.
C
C     NR      (output) INTEGER
C             The order of the reduced generalized state space
C             representation (lambda*Er-Ar,Br,Cr) as stated in JOBEIG.
C             If JOBEIG = 'A', NR denotes the order of a reduced system
C             without any uncontrollable or unobservable eigenvalues; if
C             JOBEIG = 'I', NR denotes the order of the reduced system
C             without any uncontrollable or unobservable infinite
C             eigenvalues. If JOBSYS = 'N', then NR = N.
C
C     RANKE   (output) INTEGER
C             The effective (estimated) rank of the reduced matrix Er.
C
C     Tolerances
C
C     TOL     DOUBLE PRECISION array, dimension 3
C             TOL(1) is the tolerance to be used in rank determinations
C             when transforming (lambda*E-A,B,C). If the user sets
C             TOL(1) > 0, then the given value of TOL(1) is used as a
C             lower bound for reciprocal condition numbers in rank
C             determinations; a (sub)matrix whose estimated condition
C             number is less than 1/TOL(1) is considered to be of full
C             rank.  If the user sets TOL(1) <= 0, then an implicitly
C             computed, default tolerance, defined by TOLDEF1 = N*N*EPS,
C             is used instead, where EPS is the machine precision (see
C             LAPACK Library routine DLAMCH).  TOL(1) < 1.
C             TOL(2) is the tolerance to be used for checking pencil
C             singularity when CKSING = 'C', or singularity of the
C             matrices A and E when CKSING = 'N'. If the user sets
C             TOL(2) > 0, then the given value of TOL(2) is used.
C             If the user sets TOL(2) <= 0, then an implicitly
C             computed, default tolerance, defined by  TOLDEF2 = 10*EPS,
C             is used instead.  TOL(2) < 1.
C             TOL(3) is the threshold value for magnitude of the matrix
C             elements, if EQUIL = 'S': elements with magnitude less
C             than or equal to TOL(3) are ignored for scaling. If the
C             user sets TOL(3) >= 0, then the given value of TOL(3) is
C             used. If the user sets TOL(3) < 0, then an implicitly
C             computed, default threshold, defined by  THRESH = c*EPS,
C             where c = MAX(||A||,||E||,||B||,||C||) is used instead and
C             1-norm is used. TOL(3) = 0 is not always a good choice.
C             TOL(3) < 1.
C             TOL(3) is not used if EQUIL = 'N'.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (LIWORK)
C             If JOBSYS = 'R',  LIWORK >= 2*N+MAX(M,P)+7;
C             If JOBSYS = 'N',  LIWORK >= N.
C             If JOBSYS = 'R', the first 7 elements of IWORK contain
C             information on performed reduction and on structure of
C             resulting system matrices after removing the specified
C             eigenvalues (see the description of the parameter INFRED
C             of the SLICOT Library routine TG01JY).
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if  INFO = 0,  DWORK(1) returns the optimal value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The length of the array DWORK.
C             If JOBSYS = 'R', and EQUIL = 'S',
C                LDWORK >= MAX(w+4*N+4,8*N,x,y),
C             where w = N*N,                  if JOBEIG = 'A',
C                   w = 0,                    if JOBEIG = 'I',
C                   x = MAX(2*(z+MAX(M,P)+N-1),N*N+4*N), if RESTOR = 'R'
C                   x = MAX(  2*(MAX(M,P)+N-1),N*N+4*N), if RESTOR = 'N'
C                   y = 2*N*N+10*N+MAX(N,23), if CKSING = 'C',
C                   y = 0,                    if CKSING = 'N',
C                   z = 2*N*N+N*M+N*P;
C             if JOBSYS = 'R', and EQUIL = 'N',
C                LDWORK >= MAX(w+4*N+4,x,y);
C             if JOBSYS = 'N', and JOBEIG = 'A'  or UPDATE = 'U',
C                              and EQUIL  = 'S',
C                LDWORK >= MAX(N*N+4*N+4,8*N,N+M,N+P);
C             if JOBSYS = 'N', and JOBEIG = 'A'  or UPDATE = 'U',
C                              and EQUIL  = 'N',
C                LDWORK >= MAX(N*N+4*N+4,N+M,N+P);
C             if JOBSYS = 'N', and JOBEIG = 'I' and UPDATE = 'N',
C                              and EQUIL  = 'S',
C                LDWORK >= MAX(4*N+4,8*N);
C             if JOBSYS = 'N', and JOBEIG = 'I' and UPDATE = 'N',
C                              and EQUIL  = 'N',
C                LDWORK >= 4*N+4.
C             If JOBSYS = 'R' and ( RESTOR = 'R' or
C             LDWORK >= MAX(1,2*N*N+N*M+N*P+2*(MAX(M,P)+N-1) ),
C             then more accurate results are to be expected by
C             considering only those reduction phases in the SLICOT
C             Library routine TG01JY, where effective order reduction
C             occurs. This is achieved by saving the system matrices
C             before each phase (after orthogonally triangularizing the
C             matrix A or the matrix E, if RESTOR = 'N') and restoring
C             them if no order reduction took place. However, higher
C             global accuracy is not guaranteed.
C             For good performance, LDWORK should be generally larger.
C
C             If LDWORK = -1, then a workspace query is assumed;
C             the routine only calculates the optimal size of the
C             DWORK array, returns this value as the first entry of
C             the DWORK array, and no error message related to LDWORK
C             is issued by XERBLA. The optimal workspace includes the
C             extra space for improving the accuracy.
C
C     Warning Indicator
C
C     IWARN   INTEGER
C             = 0: When determining the rank of a matrix, the rank can
C                  be safely determined: a small decrease of TOL(1) will
C                  not increase the rank.
C             = 1: The computed rank is possibly incorrect: a small
C                  decrease of TOL(1) might increase the rank.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0: succesful exit;
C             < 0: if INFO = -i, the i-th argument had an illegal value;
C             = 1: the given pencil A - lambda*E is numerically
C                  singular and the reduced system is not computed.
C                  However, the system is considered improper, and
C                  AB13ID is set to .FALSE.
C                  This error can be returned only if CKSING = 'C'.
C
C     METHOD
C
C     If JOBSYS = 'R', the routine first removes uncontrollable and
C     unobservable infinite eigenvalues of the pencil lambda*E-A. If, in
C     addition, JOBEIG = 'A', uncontrollable and unobservable zero
C     eigenvalues are also removed. Then, or if JOBSYS = 'N', a
C     URV decomposition of the matrix E is performed, i.e., orthogonal
C     matrices U and V are computed, such that
C
C               ( T  0 )
C       U*E*V = (      ) with a full-rank matrix T.                  (1)
C               ( 0  0 )
C
C     Then the matrix A (or a copy of A if JOBEIG = 'A' or UPDATE = 'U')
C     is updated and partioned as in (1), i.e.,
C
C               ( A11  A12 )
C       U*A*V = (          ) ,                                       (2)
C               ( A21  A22 )
C
C     and the rank of A22 is computed. If A22 is invertible, the
C     transfer function is proper, otherwise it is improper. If
C     required (i.e., JOBEIG = 'A' or UPDATE = 'U'), the matrices B and
C     C are updated as well in order to obtain an equivalent reduced
C     system with the same transfer function. See also Chapter 3 in [1],
C     [2] for more details.
C
C     REFERENCES
C
C     [1] Voigt, M.
C         L_inf-Norm Computation for Descriptor Systems.
C         Diploma Thesis, Chemnitz University of Technology, Department
C         of Mathematics, Germany, July 2010.
C
C     [2] Benner, P., Sima, V., Voigt, M.
C         L_infinity-norm computation for continuous-time descriptor
C         systems using structured matrix pencils.
C         IEEE Trans. Automat. Contr., vol. 57, pp. 233-238, 2012.
C
C     NUMERICAL ASPECTS
C
C     The algorithm requires O(N**3) floating point operations. During
C     the algorithm it is necessary to determine the rank of certain
C     matrices. Therefore it is crucial to use an appropriate tolerance
C     TOL(1) to make correct rank decisions.
C
C     CONTRIBUTORS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     Jan. 2012.
C     Based on the subroutine DGEISP by Matthias Voigt, Chemnitz
C     University of Technology, Department of Mathematics, Feb. 2010.
C
C     REVISIONS
C
C     V. Sima, Feb. 2012, March 2012, April 2012, June 2012, Feb. 2021,
C     Oct. 2022, June 2023.
C
C     KEYWORDS
C
C     Descriptor system, proper transfer function, L_inf-norm
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TEN
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TEN = 1.0D+1 )
C
C     .. Scalar Arguments ..
      CHARACTER          CKSING, EQUIL, JOBEIG, JOBSYS, RESTOR, UPDATE
      INTEGER            INFO, IWARN, LDA, LDB, LDC, LDE, LDWORK, M, N,
     $                   NR, P, RANKE
C
C     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * ),
     $                   DWORK( * ), E( LDE, * ), TOL( * )
C
C     .. Local Scalars ..
      CHARACTER          SYSTYP
      LOGICAL            LEQUIL, LQUERY, LREDC, LREMA, LRUPD, LSING,
     $                   LUPD, MAXACC
      INTEGER            I, ISV, ITAU, IWRK, IWS, J, K, MAXMP, MAXWRK,
     $                   MINWRK, N1, NA, RANKA
      DOUBLE PRECISION   PREC, SVLMAX, THRESH, TOLDEF
C
C     .. Local Arrays ..
      DOUBLE PRECISION   DUM( 2 ), TOLV( 3 )
C
C     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANGE
      EXTERNAL           DLAMCH, DLANGE, LSAME
C
C     .. External Subroutines ..
      EXTERNAL           DLACPY, DLASET, DORMQR, DORMRZ, DSWAP, DTZRZF,
     $                   MB03OD, TG01AD, TG01JY, XERBLA
C
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, INT, MAX
C
C     .. Executable Statements ..
C
      INFO  = 0
      IWARN = 0
      MAXMP = MAX( M, P )
      N1    = MAX( 1, N )
C
C     Decode the input arguments.
C
      LREDC  = LSAME( JOBSYS, 'R' )
      LREMA  = LSAME( JOBEIG, 'A' )
      LEQUIL = LSAME( EQUIL,  'S' )
      LSING  = LSAME( CKSING, 'C' )
      MAXACC = LSAME( RESTOR, 'R' )
      LUPD   = LSAME( UPDATE, 'U' )
      LRUPD  = LREMA .OR. LUPD
C
C     Test the input arguments.
C
      IF( .NOT.LREDC .AND. .NOT.LSAME( JOBSYS, 'N' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.LREMA  .AND. .NOT.LSAME( JOBEIG, 'I' ) ) THEN
         INFO = -2
      ELSE IF( .NOT.LEQUIL .AND. .NOT.LSAME( EQUIL,  'N' ) ) THEN
         INFO = -3
      ELSE IF( .NOT.LSING  .AND. .NOT.LSAME( CKSING, 'N' ) ) THEN
         INFO = -4
      ELSE IF( .NOT.MAXACC .AND. .NOT.LSAME( RESTOR, 'N' ) ) THEN
         INFO = -5
      ELSE IF( .NOT.LUPD   .AND. .NOT.LSAME( UPDATE, 'N' ) ) THEN
         INFO = -6
      ELSE IF( N.LT.0 ) THEN
         INFO = -7
      ELSE IF( M.LT.0 ) THEN
         INFO = -8
      ELSE IF( P.LT.0 ) THEN
         INFO = -9
      ELSE IF( LDA.LT.N1 ) THEN
         INFO = -11
      ELSE IF( LDE.LT.N1 ) THEN
         INFO = -13
      ELSE IF( LDB.LT.N1 ) THEN
         INFO = -15
      ELSE IF( LDC.LT.1 .OR. ( N.GT.0 .AND. LDC.LT.MAXMP ) ) THEN
         INFO = -17
      ELSE IF( TOL( 1 ).GE.ONE ) THEN
         INFO = -20
      ELSE IF( TOL( 2 ).GE.ONE ) THEN
         INFO = -20
      ELSE IF( LEQUIL ) THEN
         THRESH = TOL( 3 )
         IF( THRESH.GE.ONE )
     $      INFO = -20
      END IF
      IF( INFO.EQ.0 ) THEN
C
C        Compute minimal workspace.
C
         IF( LREDC ) THEN
            K = N*( 2*N + M + P )
            IF( MAXACC ) THEN
               MINWRK = MAX( 1, 2*( K + MAXMP + N - 1 ) )
            ELSE
               MINWRK = MAX( 1, 2*( MAXMP + N - 1 ) )
            END IF
            MINWRK = MAX( MINWRK, N*N + 4*N )
            IF( LSING )
     $         MINWRK = MAX( MINWRK, 2*N*N + 10*N + MAX( N, 23 ) )
C
            IF( LREMA ) THEN
               SYSTYP = 'R'
            ELSE
               SYSTYP = 'P'
            END IF
         ELSE
            MINWRK = 0
         END IF
         IF( LRUPD ) THEN
            MINWRK = MAX( MINWRK, N*N + 4*N + 4, N + MAXMP )
         ELSE
            MINWRK = MAX( MINWRK, 4*N + 4 )
         END IF
         IF( LEQUIL )
     $      MINWRK = MAX( MINWRK, 8*N )
C
         MAXWRK = MINWRK
         LQUERY = LDWORK.EQ.-1
         IF( LQUERY ) THEN
C
C           Compute optimal workspace.
C
            IF( LREDC ) THEN
               CALL TG01JY( 'Irreducible', SYSTYP, 'No Scaling', CKSING,
     $                      RESTOR, N, M, P, A, LDA, E, LDE, B, LDB, C,
     $                      LDC, NR, IWORK, TOL, IWORK, DWORK, -1,
     $                      INFO )
               MAXWRK = MAX( MAXWRK, INT( DWORK( 1 ) ) )
            END IF
            CALL MB03OD( 'QR Decomposition', N, N, E, LDE, IWORK, ZERO,
     $                   ZERO, DWORK, RANKE, DWORK, DUM, -1, INFO )
            MAXWRK = MAX( MAXWRK, INT( DUM( 1 ) ) + N + 3 )
            CALL DORMQR( 'Left', 'Transpose', N, N, N, E, LDE, DWORK, A,
     $                   LDA, DUM, -1, INFO )
            MAXWRK = MAX( MAXWRK, INT( DUM( 1 ) ) + N )
            IF( LRUPD ) THEN
               CALL DORMQR( 'Left', 'Transpose', N, M, N, E, LDE, DWORK,
     $                      B, LDB, DUM( 2 ), -1, INFO )
               MAXWRK = MAX( MAXWRK, INT( DUM( 2 ) ) + N )
            END IF
            CALL DTZRZF( N, N, E, LDE, DWORK, DUM, -1, INFO )
            CALL DORMRZ( 'Right', 'Transpose', N, N, N, N, E, LDE,
     $                   DWORK, A, LDA, DUM( 2 ), -1, INFO )
            MAXWRK = MAX( MAXWRK, MAX( INT( DUM( 1 ) ),
     $                                 INT( DUM( 2 ) ) ) + N )
            IF( LRUPD ) THEN
               CALL DORMRZ( 'Right', 'Transpose', P, N, N, N, E, LDE,
     $                      DWORK, C, LDC, DUM, -1, INFO )
               MAXWRK = MAX( MAXWRK, INT( DUM( 1 ) ) + N )
            END IF
         ELSE IF( LDWORK.LT.MINWRK ) THEN
            INFO = -23
         END IF
      END IF
C
      IF( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'AB13ID', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         DWORK( 1 ) = MAXWRK
         RETURN
      END IF
C
C     Quick return if possible.
C
      NR = N
      IF( N.EQ.0 ) THEN
         RANKE  = 0
         AB13ID = .TRUE.
         DWORK( 1 ) = ONE
         RETURN
      END IF
C
C     Set the tolerances.
C
      TOLDEF = TOL( 1 )
      IF( TOLDEF.LE.ZERO .OR. LEQUIL ) THEN
         PREC = DLAMCH( 'Precision' )
         IF( LEQUIL ) THEN
            IF( THRESH.LT.ZERO )
     $         THRESH = MAX( DLANGE( '1-norm', N, N, A, LDA, DWORK ),
     $                       DLANGE( '1-norm', N, N, E, LDE, DWORK ),
     $                       DLANGE( '1-norm', N, M, B, LDB, DWORK ),
     $                       DLANGE( '1-norm', P, N, C, LDC, DWORK ) )*
     $                  PREC
         ELSE
            TOLDEF = N*N*PREC
         END IF
      END IF
      TOLV( 1 ) = TOLDEF
      TOLV( 2 ) = TOL( 2 )
      TOLV( 3 ) = TOL( 3 )
C
C     Computations.
C
C     (Note: Comments in the code beginning "Workspace:" describe the
C     minimal amount of real workspace needed at that point in the
C     code, as well as the preferred amount for good performance.
C     NB refers to the optimal block size for the immediately
C     following subroutine, as returned by ILAENV.)
C
C     Equilibrate the system, if required.
C
C     Workspace: need   8*N.
C
      IF( LEQUIL ) THEN
         CALL TG01AD( 'All', N, N, M, P, THRESH, A, LDA, E, LDE, B, LDB,
     $                C, LDC, DWORK, DWORK( N+1 ), DWORK( 2*N+1 ),
     $                INFO )
         MAXWRK = MAX( MAXWRK, 8*N )
      END IF
C
      IF( LREDC ) THEN
C
C        Step 1: If JOBEIG = 'A' remove all uncontrollable and
C                unobservable eigenvalues of the system; otherwise
C                remove only the uncontrollable and unobservable
C                infinite eigenvalues explicitely, uncontrollable or
C                unobservable zero eigenvalues are not removed.
C
C        Workspace: need   MAX(2*(MAX(M,P)+N-1),N*N+4*N,y),
C                   prefer MAX(2*N*N+N*M+N*P+2*(MAX(M,P)+N-1),y), or
C                          even larger (see, optimal space above).
C
         IWS = 8
         CALL TG01JY( 'Irreducible', SYSTYP, 'No Scaling', CKSING,
     $                RESTOR, N, M, P, A, LDA, E, LDE, B, LDB, C, LDC,
     $                NR, IWORK, TOLV, IWORK( IWS ), DWORK, LDWORK,
     $                INFO )
         MAXWRK = MAX( MAXWRK, INT( DWORK( 1 ) ) )
         IF( INFO.EQ.1 ) THEN
            AB13ID = .FALSE.
            RETURN
         END IF
      ELSE
         IWS = 1
      END IF
C
C     Step 2: Compute a URV decomposition of E(1:NR,1:NR).
C
      DO 10 I = IWS, IWS + NR - 1
         IWORK( I ) = 0
   10 CONTINUE
C
C     Perform a rank-revealing QR decomposition of E(1:NR,1:NR) with
C     respect to the tolerance TOLDEF.
C
C     Workspace: need   4*NR+4,
C                prefer 3*NR+( NR+1 )*NB+3, with NB for DGEQP3.
C
      SVLMAX = DLANGE( 'Frobenius', N, N, E, LDE, DWORK )
      CALL MB03OD( 'QR Decomposition', NR, NR, E, LDE, IWORK( IWS ),
     $             TOLDEF, SVLMAX, DWORK, RANKE, DWORK( NR+1 ),
     $             DWORK( NR+4 ), LDWORK-( NR+3 ), INFO )
      MAXWRK = MAX( MAXWRK, INT( DWORK( NR+4 ) ) + NR + 3 )
C
C     Check whether the estimated rank might be incorrect.
C
      IF( ABS( DWORK( NR+3 )/ DWORK( NR+1 ) - TOLDEF ).LT.TOLDEF/TEN )
C
C        The estimated rank might be incorrect, warning returned.
C
     $   IWARN = 1
C
      IF( RANKE.LT.NR .OR. LRUPD ) THEN
C
C        Perform the same transformations on A(1:NR,1:NR).
C
C        Workspace: need   2*NR,
C                   prefer NR+NR*( NB+1 ).
C
         CALL DORMQR( 'Left', 'Transpose', NR, NR, NR, E, LDE, DWORK, A,
     $                LDA, DWORK( NR+1 ), LDWORK-NR, INFO )
         MAXWRK = MAX( MAXWRK, INT( DWORK( NR+1 ) ) + NR )
C
         DO 20 I = IWS, IWS + NR - 1
            IWORK( I ) = -IWORK( I )
   20    CONTINUE
C
         DO 40 I = IWS, IWS + NR - 1
            IF( IWORK( I ).LT.0 ) THEN
               J = I
               IWORK( J ) = -IWORK( J )
   30          CONTINUE
               K = IWORK( J ) + IWS - 1
               IF( IWORK( K ).LT.0 ) THEN
                  CALL DSWAP( NR, A( 1, J-IWS+1 ), 1, A( 1, K-IWS+1 ),
     $                        1 )
                  IWORK( K ) = -IWORK( K )
                  J = K
                  GO TO 30
               END IF
            END IF
   40    CONTINUE
C
         IF( LRUPD ) THEN
C
C           Update B and C.
C
C           Workspace: need   NR+M,
C                      prefer NR+M*NB.
C
            CALL DORMQR( 'Left', 'Transpose', NR, M, NR, E, LDE, DWORK,
     $                   B, LDB, DWORK( NR+1 ), LDWORK-NR, INFO )
            MAXWRK = MAX( MAXWRK, INT( DWORK( NR+1 ) ) + NR )
C
            DO 50 I = IWS, IWS + NR - 1
               IWORK( I ) = -IWORK( I )
   50       CONTINUE
C
            DO 70 I = IWS, IWS + NR - 1
               IF( IWORK( I ).LT.0 ) THEN
                  J = I
                  IWORK( J ) = -IWORK( J )
   60             CONTINUE
                  K = IWORK( J ) + IWS - 1
                  IF( IWORK( K ).LT.0 ) THEN
                     CALL DSWAP( P, C( 1, J-IWS+1 ), 1, C( 1, K-IWS+1 ),
     $                           1 )
                     IWORK( K ) = -IWORK( K )
                     J = K
                     GO TO 60
                  END IF
               END IF
   70       CONTINUE
         END IF
      END IF
C
C                                        ( R11 R12 )
C     Logically partition E(1:NR,1:NR) = (         ),
C                                        (  0  R22 )
C
C     where R11 = R(1:RANKE,1:RANKE).
C
C     Neglect R22 and annihilate R12.
C
      IF( RANKE.LT.NR ) THEN
C
C        The block R22 is non-empty, further computations required.
C
C        Workspace: need   2*RANKE,
C                   prefer RANKE+RANKE*NB.
C
         CALL DTZRZF( RANKE, NR, E, LDE, DWORK, DWORK( RANKE+1 ),
     $                LDWORK-RANKE, INFO )
         MAXWRK = MAX( MAXWRK, INT( DWORK( RANKE+1 ) ) + RANKE )
C
C        Perform the same transformations on A(1:NR,1:NR).
C
C        Workspace: need   RANKE+NR,
C                   prefer RANKE+NR*NB.
C
         NA = NR - RANKE
         CALL DORMRZ( 'Right', 'Transpose', NR, NR, RANKE, NA, E, LDE,
     $                DWORK, A, LDA, DWORK( RANKE+1 ), LDWORK-RANKE,
     $                INFO )
         MAXWRK = MAX( MAXWRK, INT( DWORK( RANKE+1 ) ) + RANKE )
C
         IF( LRUPD ) THEN
C
C           Update C.
C
C           Workspace: need   RANKE+P,
C                      prefer RANKE+P*NB.
C
            CALL DORMRZ( 'Right', 'Transpose', P, NR, RANKE, NA, E, LDE,
     $                   DWORK, C, LDC, DWORK( RANKE+1 ), LDWORK-RANKE,
     $                   INFO )
            MAXWRK = MAX( MAXWRK, INT( DWORK( RANKE+1 ) ) + RANKE )
         END IF
C
C        Step 3: Determine the rank of A(RANKE+1:NR,RANKE+1:NR).
C
         DO 80 I = IWS, IWS + NA - 1
            IWORK( I ) = 0
   80    CONTINUE
C
C        Perform a rank-revealing QR decomposition of
C        A(RANKE+1:NR,RANKE+1:NR) with respect to the tolerance TOLDEF.
C
         SVLMAX = DLANGE( 'Frobenius', NR, NR, A, LDA, DWORK )
C
         IF( LRUPD ) THEN
C
C           IF JOBEIG = 'A' or UPDATE = 'U', then copy
C           A(RANKE+1:NR,RANKE+1:NR) to DWORK and perform the operations
C           on DWORK.
C
C           Workspace: need   NA*NA+4*NA+4,
C                      prefer NA*NA+3*NA+( NA+1 )*NB+3.
C
            ITAU = NA*NA + 1
            ISV  = ITAU  + NA
            IWRK = ISV   + 3
            CALL DLACPY( 'Full', NA, NA, A( RANKE+1, RANKE+1 ), LDA,
     $                   DWORK, NA )
            CALL MB03OD( 'QR Decomposition', NA, NA, DWORK, NA,
     $                   IWORK( IWS ), TOLDEF, SVLMAX, DWORK( ITAU ),
     $                   RANKA, DWORK( ISV ), DWORK( IWRK ),
     $                   LDWORK-IWRK+1, INFO )
         ELSE
C
C           Workspace: need   4*NA+4,
C                      prefer 3*NA+( NA+1 )*NB+3.
C
            ISV  = NA  + 1
            IWRK = ISV + 3
            CALL MB03OD( 'QR Decomposition', NA, NA,
     $                   A( RANKE+1, RANKE+1 ), LDA, IWORK( IWS ),
     $                   TOLDEF, SVLMAX, DWORK, RANKA, DWORK( ISV ),
     $                   DWORK( IWRK ), LDWORK-IWRK+1, INFO )
         END IF
C
         MAXWRK = MAX( MAXWRK, INT( DWORK( IWRK ) ) + IWRK - 1 )
C
C
C        Check whether the estimated rank might be incorrect.
C
         IF( ABS( DWORK( ISV+2 )/ DWORK( ISV ) - TOLDEF ).LT.TOLDEF/TEN)
     $      IWARN = 1
C
C        Set R12 and R22 to 0.
C
         IF( NR.GT.1 ) THEN
            CALL DLASET( 'Lowr', NR-1, RANKE, ZERO, ZERO, E( 2, 1 ),
     $                   LDE )
            CALL DLASET( 'Full', NR, NA, ZERO, ZERO, E( 1, RANKE+1 ),
     $                   LDE )
         END IF
      ELSE
C
C        The block R22 is empty, no computation required.
C
         RANKA = 0
         NA    = 0
         IF( RANKE.GT.1 )
     $      CALL DLASET( 'Lowr', RANKE-1, RANKE-1, ZERO, ZERO,
     $                   E( 2, 1 ), LDE )
      END IF
C
C     Step 4: Determine if the transfer function is proper.
C
      IF( NA.EQ.RANKA ) THEN
C
C        The transfer function is proper.
C
         AB13ID = .TRUE.
      ELSE
C
C        The transfer function is improper.
C
         AB13ID = .FALSE.
      END IF
C
      DWORK( 1 ) = MAXWRK
      RETURN
C *** Last line of AB13ID ***
      END
