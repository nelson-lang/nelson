      SUBROUTINE MB04QS( TRANC, TRAND, TRANU, M, N, ILO, V, LDV, W, LDW,
     $                   C, LDC, D, LDD, CS, TAU, DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     Overwrites general real m-by-n/n-by-m matrices C and D with
C
C               [ op(C) ]
C         U  *  [       ]   if TRANU = 'N', or
C               [ op(D) ]
C
C          T    [ op(C) ]
C         U  *  [       ]   if TRANU = 'T',
C               [ op(D) ]
C
C     where U is defined as the product of symplectic reflectors and
C     Givens rotations,
C
C         U = diag( H(1),H(1) ) G(1) diag( F(1),F(1) )
C             diag( H(2),H(2) ) G(2) diag( F(2),F(2) )
C                               ....
C             diag( H(k),H(k) ) G(k) diag( F(k),F(k) ),
C
C     with k = m-1, as returned by the SLICOT Library routines MB04PU
C     or MB04RU.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     TRANC   CHARACTER*1
C             Specifies the form of op( C ) as follows:
C             = 'N':  op( C ) = C; 
C             = 'T':  op( C ) = C';
C             = 'C':  op( C ) = C'.
C
C     TRAND   CHARACTER*1
C             Specifies the form of op( D ) as follows:
C             = 'N':  op( D ) = D; 
C             = 'T':  op( D ) = D';
C             = 'C':  op( D ) = D'.
C
C     TRANU   CHARACTER*1
C             Specifies whether U or U' is applied as follows:
C             = 'N':  apply U;
C             = 'T':  apply U'.
C
C     Input/Output Parameters
C
C     M       (input) INTEGER
C             The number of rows of the matrices op(C) and op(D).
C             M >= 0.
C
C     N       (input) INTEGER
C             The number of columns of the matrices op(C) and op(D).
C             N >= 0.
C
C     ILO     (input) INTEGER
C             ILO must have the same value as in the previous call of
C             MB04PU or MB04RU. U is equal to the unit matrix except in
C             the submatrix
C             U([ilo+1:m m+ilo+1:2*m], [ilo+1:m m+ilo+1:2*m]).
C             1 <= ILO <= M+1.
C
C     V       (input) DOUBLE PRECISION array, dimension (LDV,M)
C             On entry, the leading M-by-M part of this array must
C             contain in its columns the vectors which define the
C             elementary reflectors H(i).
C
C     LDV     INTEGER
C             The leading dimension of the array V.  LDV >= MAX(1,M).
C
C     W       (input) DOUBLE PRECISION array, dimension (LDW,M)
C             On entry, the leading M-by-M part of this array must
C             contain in its columns the vectors which define the
C             elementary reflectors F(i).
C
C     LDW     INTEGER
C             The leading dimension of the array W.  LDW >= MAX(1,M).
C
C     C       (input/output) DOUBLE PRECISION array, dimension
C                     (LDC,N) if TRANC = 'N',
C                     (LDC,M) if TRANC = 'T' or TRANC = 'C'.
C             On entry with TRANC = 'N', the leading M-by-N part of this
C             array must contain the matrix C.
C             On entry with TRANC = 'T' or TRANC = 'C', the leading
C             N-by-M part of this array must contain the transpose of
C             the matrix C.
C             On exit with TRANC = 'N', the leading M-by-N part of this
C             array contains the updated matrix C.
C             On exit with TRANC = 'T' or TRANC = 'C', the leading
C             N-by-M part of this array contains the transpose of the
C             updated matrix C.
C
C     LDC     INTEGER
C             The leading dimension of the array C.
C             LDC >= MAX(1,M),  if TRANC = 'N';
C             LDC >= MAX(1,N),  if TRANC = 'T' or TRANC = 'C'.
C
C     D       (input/output) DOUBLE PRECISION array, dimension
C                     (LDD,N) if TRAND = 'N',
C                     (LDD,M) if TRAND = 'T' or TRAND = 'C'.
C             On entry with TRAND = 'N', the leading M-by-N part of this
C             array must contain the matrix D.
C             On entry with TRAND = 'T' or TRAND = 'C', the leading
C             N-by-M part of this array must contain the transpose of
C             the matrix D.
C             On exit with TRAND = 'N', the leading M-by-N part of this
C             array contains the updated matrix D.
C             On exit with TRAND = 'T' or TRAND = 'C', the leading
C             N-by-M part of this array contains the transpose of the
C             updated matrix D.
C
C     LDD     INTEGER
C             The leading dimension of the array D.
C             LDD >= MAX(1,M),  if TRAND = 'N';
C             LDD >= MAX(1,N),  if TRAND = 'T' or TRAND = 'C'.
C
C     CS      (input) DOUBLE PRECISION array, dimension (2*N-2)
C             On entry, the first 2*N-2 elements of this array must
C             contain the cosines and sines of the symplectic Givens
C             rotations G(i), as returned by MB04PU or MB04RU.
C
C     TAU     (input) DOUBLE PRECISION array, dimension (N-1)
C             On entry, the first N-1 elements of this array must
C             contain the scalar factors of the elementary reflectors
C             F(i), as returned by MB04PU or MB04RU.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0,  DWORK(1)  returns the optimal value
C             of LDWORK.
C             On exit, if  INFO = -18,  DWORK(1)  returns the minimum
C             value of LDWORK.
C
C     LDWORK  INTEGER
C             The length of the array DWORK.  LDWORK >= MAX(1,N).
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
C     CONTRIBUTORS
C
C     D. Kressner (Technical Univ. Berlin, Germany) and
C     P. Benner (Technical Univ. Chemnitz, Germany), December 2003.
C
C     REVISIONS
C
C     V. Sima, Nov. 2011 (SLICOT version of the HAPACK routine DOSMPV).
C
C     KEYWORDS
C
C     Elementary matrix operations, orthogonal symplectic matrix.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ONE
      PARAMETER         ( ONE = 1.0D+0 )
C     .. Scalar Arguments ..
      CHARACTER         TRANC, TRAND, TRANU
      INTEGER           ILO, INFO, LDC, LDD, LDV, LDW, LDWORK, M, N
C     .. Array Arguments ..
      DOUBLE PRECISION  C(LDC,*), CS(*), D(LDD,*), DWORK(*), TAU(*),
     $                  V(LDV,*), W(LDW,*)
C     .. Local Scalars ..
      LOGICAL           LQUERY, LTRC, LTRD, LTRU
      INTEGER           IC, ID, IERR, JC, JD, MH, MINWRK
C     .. External Functions ..
      LOGICAL           LSAME
      EXTERNAL          LSAME
C     .. External Subroutines ..
      EXTERNAL          MB04QB, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         DBLE, INT, MAX
C
C     .. Executable Statements ..
C
C     Decode the scalar input parameters.
C
      INFO  = 0
      LTRC = LSAME( TRANC, 'T' ) .OR. LSAME( TRANC, 'C' )
      LTRD = LSAME( TRAND, 'T' ) .OR. LSAME( TRAND, 'C' )
      LTRU = LSAME( TRANU, 'T' )
      MH = MAX( 0, M - ILO )
C
C     Check the scalar input parameters.
C
      IF (      .NOT.( LTRC .OR. LSAME( TRANC, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF ( .NOT.( LTRD .OR. LSAME( TRAND, 'N' ) ) ) THEN
         INFO = -2
      ELSE IF ( .NOT.( LTRU .OR. LSAME( TRANU, 'N' ) ) ) THEN
         INFO = -3
      ELSE IF ( M.LT.0 ) THEN
         INFO = -4
      ELSE IF ( N.LT.0 ) THEN
         INFO = -5
      ELSE IF ( ILO.LT.1 .OR. ILO.GT.M+1 ) THEN
         INFO = -6
      ELSE IF ( LDV.LT.MAX( 1, M ) ) THEN
         INFO = -8
      ELSE IF ( LDW.LT.MAX( 1, M ) ) THEN
         INFO = -10
      ELSE IF ( ( LTRC .AND. LDC.LT.MAX( 1, N ) ).OR.
     $     ( .NOT.LTRC .AND. LDC.LT.MAX( 1, M ) ) ) THEN
         INFO = -12
      ELSE IF ( ( LTRD .AND. LDD.LT.MAX( 1, N ) ).OR.
     $     ( .NOT.LTRD .AND. LDD.LT.MAX( 1, M ) ) ) THEN
         INFO = -14
      ELSE
         LQUERY = LDWORK.EQ.-1
         MINWRK = MAX( 1, N )
         IF ( LDWORK.LT.MINWRK .AND. .NOT.LQUERY ) THEN
            DWORK(1) = DBLE( MINWRK )
            INFO = -18
         ELSE IF( LQUERY ) THEN
            IF ( M.LE.ILO .OR. N.EQ.0 ) THEN
               DWORK(1) = ONE
            ELSE
               CALL MB04QB( TRANC, TRAND, TRANU, 'C', 'C', MH, N, MH, V,
     $                      LDV, W, LDW, C, LDC, D, LDD, CS, TAU, DWORK,
     $                      -1, IERR )
               DWORK(1) = MAX( MINWRK, INT( DWORK(1) ) )
            END IF
            RETURN
         END IF
      END IF
C
C     Return if there were illegal values.
C
      IF ( INFO.NE.0 ) THEN
         CALL XERBLA( 'MB04QS', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF ( M.LE.ILO .OR. N.EQ.0 ) THEN
         DWORK(1) = ONE
         RETURN
      END IF
C
      IF ( LTRC ) THEN
         IC = 1
         JC = ILO + 1
      ELSE
         IC = ILO + 1
         JC = 1
      END IF
      IF ( LTRD ) THEN
         ID = 1
         JD = ILO + 1
      ELSE
         ID = ILO + 1
         JD = 1
      END IF
C
      CALL MB04QB( TRANC, TRAND, TRANU, 'Columnwise', 'Columnwise', MH,
     $             N, MH, V(ILO+1,ILO), LDV, W(ILO+1,ILO), LDW,
     $             C(IC,JC), LDC, D(ID,JD), LDD, CS(2*ILO-1), TAU(ILO),
     $             DWORK, LDWORK, IERR )
      RETURN
C *** Last line of MB04QS ***
      END
