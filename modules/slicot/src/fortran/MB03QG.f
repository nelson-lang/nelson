      SUBROUTINE MB03QG( DICO, STDOM, JOBU, JOBV, N, NLOW, NSUP, ALPHA,
     $                   A, LDA, E, LDE, U, LDU, V, LDV, NDIM, DWORK,
     $                   LDWORK, INFO )
C
C     PURPOSE
C
C     To reorder the diagonal blocks of a principal subpencil of an
C     upper quasi-triangular matrix pencil A-lambda*E together with
C     their generalized eigenvalues, by constructing orthogonal
C     similarity transformations UT and VT.
C     After reordering, the leading block of the selected subpencil of
C     A-lambda*E has generalized eigenvalues in a suitably defined
C     domain of interest, usually related to stability/instability in a
C     continuous- or discrete-time sense.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     DICO    CHARACTER*1
C             Specifies the type of the spectrum separation to be
C             performed, as follows:
C             = 'C':  continuous-time sense;
C             = 'D':  discrete-time sense.
C
C     STDOM   CHARACTER*1
C             Specifies whether the domain of interest is of stability
C             type (left part of complex plane or inside of a circle)
C             or of instability type (right part of complex plane or
C             outside of a circle), as follows:
C             = 'S':  stability type domain;
C             = 'U':  instability type domain.
C
C     JOBU    CHARACTER*1
C             Indicates how the performed orthogonal transformations UT
C             are accumulated, as follows:
C             = 'I':  U is initialized to the unit matrix and the matrix
C                     UT is returned in U;
C             = 'U':  the given matrix U is updated and the matrix U*UT
C                     is returned in U.
C
C     JOBV    CHARACTER*1
C             Indicates how the performed orthogonal transformations VT
C             are accumulated, as follows:
C             = 'I':  V is initialized to the unit matrix and the matrix
C                     VT is returned in V;
C             = 'U':  the given matrix V is updated and the matrix V*VT
C                     is returned in V.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrices A, E, U, and V.  N >= 0.
C
C     NLOW,   (input) INTEGER
C     NSUP    (input) INTEGER
C             NLOW and NSUP specify the boundary indices for the rows
C             and columns of the principal subpencil  of A - lambda*E
C             whose diagonal blocks are to be reordered.
C             0 <= NLOW <= NSUP <= N.
C
C     ALPHA   (input) DOUBLE PRECISION
C             The boundary of the domain of interest for the eigenvalues
C             of A. If DICO = 'C', ALPHA is the boundary value for the
C             real parts of the generalized eigenvalues, while for
C             DICO = 'D', ALPHA >= 0 represents the boundary value for
C             the moduli of the generalized eigenvalues.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain a matrix in a real Schur form whose 1-by-1 and
C             2-by-2 diagonal blocks between positions NLOW and NSUP
C             are to be reordered.
C             On exit, the leading N-by-N part of this array contains
C             a real Schur matrix UT' * A * VT, with the elements below
C             the first subdiagonal set to zero.
C             The leading NDIM-by-NDIM part of the principal subpencil
C             B - lambda*C, defined with B := A(NLOW:NSUP,NLOW:NSUP),
C             C := E(NLOW:NSUP,NLOW:NSUP), has generalized eigenvalues
C             in the domain of interest and the trailing part of this
C             subpencil has generalized eigenvalues outside the domain
C             of interest.
C             The domain of interest for eig(B,C), the generalized
C             eigenvalues of the pair (B,C), is defined by the
C             parameters ALPHA, DICO and STDOM as follows:
C               For DICO = 'C':
C                  Real(eig(B,C)) < ALPHA if STDOM = 'S';
C                  Real(eig(B,C)) > ALPHA if STDOM = 'U'.
C               For DICO = 'D':
C                  Abs(eig(B,C)) < ALPHA if STDOM = 'S';
C                  Abs(eig(B,C)) > ALPHA if STDOM = 'U'.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     E       (input/output) DOUBLE PRECISION array, dimension (LDE,N)
C             On entry, the leading N-by-N part of this array must
C             contain a matrix in an upper triangular form.
C             On exit, the leading N-by-N part of this array contains an
C             upper triangular matrix UT' * E * VT, with the elements
C             below the diagonal set to zero.
C             The leading NDIM-by-NDIM part of the principal subpencil
C             B - lambda*C, defined with B := A(NLOW:NSUP,NLOW:NSUP)
C             C := E(NLOW:NSUP,NLOW:NSUP) has generalized eigenvalues
C             in the domain of interest and the trailing part of this
C             subpencil has generalized eigenvalues outside the domain
C             of interest (see description of A).
C
C     LDE     INTEGER
C             The leading dimension of the array E.  LDE >= MAX(1,N).
C
C     U       (input/output) DOUBLE PRECISION array, dimension (LDU,N)
C             On entry with JOBU = 'U', the leading N-by-N part of this
C             array must contain a transformation matrix (e.g., from a
C             previous call to this routine).
C             On exit, if JOBU = 'U', the leading N-by-N part of this
C             array contains the product of the input matrix U and the
C             orthogonal matrix UT used to reorder the diagonal blocks
C             of A - lambda*E.
C             On exit, if JOBU = 'I', the leading N-by-N part of this
C             array contains the matrix UT of the performed orthogonal
C             transformations.
C             Array U need not be set on entry if JOBU = 'I'.
C
C     LDU     INTEGER
C             The leading dimension of the array U.  LDU >= MAX(1,N).
C
C     V       (input/output) DOUBLE PRECISION array, dimension (LDV,N)
C             On entry with JOBV = 'U', the leading N-by-N part of this
C             array must contain a transformation matrix (e.g., from a
C             previous call to this routine).
C             On exit, if JOBV = 'U', the leading N-by-N part of this
C             array contains the product of the input matrix V and the
C             orthogonal matrix VT used to reorder the diagonal blocks
C             of A - lambda*E.
C             On exit, if JOBV = 'I', the leading N-by-N part of this
C             array contains the matrix VT of the performed orthogonal
C             transformations.
C             Array V need not be set on entry if JOBV = 'I'.
C
C     LDV     INTEGER
C             The leading dimension of the array V.  LDV >= MAX(1,N).
C
C     NDIM    (output) INTEGER
C             The number of generalized eigenvalues of the selected
C             principal subpencil lying inside the domain of interest.
C             If NLOW = 1, NDIM is also the dimension of the deflating
C             subspace corresponding to the generalized eigenvalues of
C             the leading NDIM-by-NDIM subpencil. In this case, if U and
C             V are the orthogonal transformation matrices used to
C             compute and reorder the generalized real Schur form of the
C             pair (A,E), then the first NDIM columns of V form an
C             orthonormal basis for the above deflating subspace.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) returns the optimal value
C             of LDWORK.
C
C     LDWORK  INTEGER
C             The length of the array DWORK.  LDWORK >= 1, and if N > 1,
C             LDWORK >= 4*N + 16.
C
C             If LDWORK = -1, then a workspace query is assumed; the
C             routine only calculates the optimal size of the DWORK
C             array, returns this value as the first entry of the DWORK
C             array, and no error message related to LDWORK is issued by
C             XERBLA.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value;
C             = 1:  A(NLOW,NLOW-1) is nonzero, i.e., A(NLOW,NLOW) is not
C                   the leading element of a 1-by-1 or 2-by-2 diagonal
C                   block of A, or A(NSUP+1,NSUP) is nonzero, i.e.,
C                   A(NSUP,NSUP) is not the bottom element of a 1-by-1
C                   or 2-by-2 diagonal block of A;
C             = 2:  two adjacent blocks are too close to swap (the
C                   problem is very ill-conditioned).
C
C     METHOD
C
C     Given an upper quasi-triangular matrix pencil A - lambda*E with
C     1-by-1 or 2-by-2 diagonal blocks, the routine reorders its
C     diagonal blocks along with its eigenvalues by performing an
C     orthogonal equivalence transformation UT'*(A - lambda*E)* VT.
C     The column transformations UT and VT are also performed on the
C     given (initial) transformations U and V (resulted from a
C     possible previous step or initialized as identity matrices).
C     After reordering, the generalized eigenvalues inside the region
C     specified by the parameters ALPHA, DICO and STDOM appear at the
C     top of the selected diagonal subpencil between positions NLOW and
C     NSUP. In other words, lambda(A(Select,Select),E(Select,Select))
C     are ordered such that lambda(A(Inside,Inside),E(Inside,Inside))
C     are inside, and lambda(A(Outside,Outside),E(Outside,Outside)) are
C     outside the domain of interest, where Select = NLOW:NSUP,
C     Inside = NLOW:NLOW+NDIM-1, and Outside = NLOW+NDIM:NSUP.
C     If NLOW = 1, the first NDIM columns of V*VT span the corresponding
C     right deflating subspace of (A,E).
C
C     REFERENCES
C
C     [1] Stewart, G.W.
C         HQR3 and EXCHQZ: FORTRAN subroutines for calculating and
C         ordering the eigenvalues of a real upper Hessenberg matrix.
C         ACM TOMS, 2, pp. 275-280, 1976.
C
C     NUMERICAL ASPECTS
C                                         3
C     The algorithm requires less than 4*N  operations.
C
C     CONTRIBUTOR
C
C     A. Varga, German Aerospace Center, DLR Oberpfaffenhofen,
C     October 2002. Based on the RASP/BIMASC routine GSEOR1.
C
C     REVISIONS
C
C     V. Sima, Dec. 2016.
C
C     KEYWORDS
C
C     Eigenvalues, invariant subspace, orthogonal transformation, real
C     Schur form, equivalence transformation.
C
C    ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION ONE, ZERO
      PARAMETER        ( ONE = 1.0D0, ZERO = 0.0D0 )
C     .. Scalar Arguments ..
      CHARACTER        DICO, JOBU, JOBV, STDOM
      INTEGER          INFO, LDA, LDE, LDU, LDV, LDWORK, N, NDIM, NLOW,
     $                 NSUP
      DOUBLE PRECISION ALPHA
C     .. Array Arguments ..
      DOUBLE PRECISION A(LDA,*), DWORK(*), E(LDE,*), U(LDU,*), V(LDV,*)
C     .. Local Scalars ..
      LOGICAL          DISCR, LQUERY, LSTDOM
      INTEGER          IB, L, LM1, MINWRK, NUP
      DOUBLE PRECISION ALPHAI(2), ALPHAR(2), BETA(2)
      DOUBLE PRECISION TLAMBD, TOLE, X
C     .. External Functions ..
      LOGICAL          LSAME
      DOUBLE PRECISION DLAMCH, DLANTR, DLAPY2
      EXTERNAL         DLAMCH, DLANTR, DLAPY2, LSAME
C     .. External Subroutines ..
      EXTERNAL         DLASET, DTGEXC, MB03QW, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC        ABS, MAX
C     .. Executable Statements ..
C
      INFO   = 0
      DISCR  = LSAME( DICO,  'D' )
      LSTDOM = LSAME( STDOM, 'S' )
C
C     Check input scalar arguments.
C
      IF( .NOT. ( LSAME( DICO, 'C' ) .OR. DISCR ) ) THEN
         INFO = -1
      ELSE IF( .NOT. ( LSTDOM .OR. LSAME( STDOM, 'U' ) ) ) THEN
         INFO = -2
      ELSE IF( .NOT. ( LSAME( JOBU, 'I' ) .OR.
     $                 LSAME( JOBU, 'U' ) ) ) THEN
         INFO = -3
      ELSE IF( .NOT. ( LSAME( JOBV, 'I' ) .OR.
     $                 LSAME( JOBV, 'U' ) ) ) THEN
         INFO = -4
      ELSE IF( N.LT.0 ) THEN
         INFO = -5
      ELSE IF( NLOW.LT.0 ) THEN
         INFO = -6
      ELSE IF( NSUP.LT.NLOW .OR. N.LT.NSUP ) THEN
         INFO = -7
      ELSE IF( DISCR .AND. ALPHA.LT.ZERO ) THEN
         INFO = -8
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -10
      ELSE IF( LDE.LT.MAX( 1, N ) ) THEN
         INFO = -12
      ELSE IF( LDU.LT.MAX( 1, N ) ) THEN
         INFO = -14
      ELSE IF( LDV.LT.MAX( 1, N ) ) THEN
         INFO = -16
      ELSE
         LQUERY = LDWORK.EQ.-1
         IF( N.LE.1 ) THEN
            MINWRK = 1
         ELSE
            MINWRK = 4*N + 16
         END IF
         IF( LDWORK.LT.MINWRK .AND. .NOT.LQUERY ) THEN
            INFO = -19
         END IF
      END IF
C
      IF( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'MB03QG', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         DWORK(1) = MINWRK
         RETURN
      END IF
C
C     Quick return if possible.
C
      NDIM = 0
      IF( NSUP.EQ.0 )
     $   RETURN
C
      IF( NLOW.GT.1 ) THEN
         IF( A(NLOW,NLOW-1).NE.ZERO )
     $      INFO = 1
      END IF
      IF( NSUP.LT.N ) THEN
         IF( A(NSUP+1,NSUP).NE.ZERO )
     $      INFO = 1
      END IF
      IF( INFO.NE.0 )
     $   RETURN
C
C     Initialize U with an identity matrix if necessary.
C
      IF( LSAME( JOBU, 'I' ) )
     $   CALL DLASET( 'Full', N, N, ZERO, ONE, U, LDU )
C
C     Initialize V with an identity matrix if necessary.
C
      IF( LSAME( JOBV, 'I' ) )
     $   CALL DLASET( 'Full', N, N, ZERO, ONE, V, LDV )
C
C     Compute zero tolerance for the diagonal elements of matrix E.
C
      TOLE = DLAMCH( 'Epsilon' ) *
     $       DLANTR( '1', 'Upper', 'Non-unit', N, N, E, LDE, DWORK )
C
      L   = NSUP
      NUP = NSUP
C
C     NUP is the minimal value such that the subpencil
C     A(i,j)-lambda*E(i,j),  with NUP+1 <= i,j <= NSUP contains no
C     generalized eigenvalues inside the domain of interest.
C     L is such that all generalized eigenvalues of the subpencil
C     A(i,j)-lambda*E(i,j) with L <= i,j <= NUP lie inside the
C     domain of interest.
C
C     WHILE( L >= NLOW ) DO
C
   10 CONTINUE
      IF( L.GE.NLOW ) THEN
         IB = 1
         IF( L.GT.NLOW ) THEN
            LM1 = L - 1
            IF( A(L,LM1).NE.ZERO ) THEN
               CALL MB03QW( N, LM1, A, LDA, E, LDE, U, LDU, V, LDV,
     $                      ALPHAR, ALPHAI, BETA, INFO )
               IF( A(L,LM1).NE.ZERO )
     $            IB = 2
            END IF
         END IF
         IF( DISCR ) THEN
            IF( IB.EQ.1 ) THEN
               TLAMBD = ABS( A(L,L) )
               X = ABS( E(L,L) )
            ELSE
               TLAMBD = DLAPY2( ALPHAR(1), ALPHAI(1) )
               X = ABS( BETA(1) )
            END IF
         ELSE
            IF( IB.EQ.1 ) THEN
               X = E(L,L)
               IF( X.LT.ZERO ) THEN
                  TLAMBD = -A(L,L)
                  X = -X
               ELSE
                  TLAMBD = A(L,L)
               END IF
            ELSE
               TLAMBD = ALPHAR(1)
               X = BETA(1)
               IF( X.LT.ZERO ) THEN
                  TLAMBD = -TLAMBD
                  X = -X
               END IF
            END IF
         END IF
         IF((      LSTDOM .AND. TLAMBD.LT.ALPHA*X .AND. X.GT.TOLE ) .OR.
     $      ( .NOT.LSTDOM .AND. TLAMBD.GT.ALPHA*X ) )
     $      THEN
            NDIM = NDIM + IB
            L = L - IB
         ELSE
            IF( NDIM.NE.0 ) THEN
               CALL DTGEXC( .TRUE., .TRUE., N, A, LDA, E, LDE, U, LDU,
     $                      V, LDV, L, NUP, DWORK, LDWORK, INFO )
               IF( INFO.NE.0 ) THEN
                  INFO = 2
                  RETURN
               END IF
               NUP = NUP - 1
               L   = L - 1
            ELSE
               NUP = NUP - IB
               L   = L - IB
            END IF
         END IF
         GO TO 10
      END IF
C
C     END WHILE 10
C
      DWORK(1) = MINWRK
      RETURN
C *** Last line of MB03QG ***
      END
