      SUBROUTINE MB03RW( M, N, PMAX, A, LDA, B, LDB, C, LDC, INFO )
C
C     PURPOSE
C
C     To solve the Sylvester equation -AX + XB = C, where A and B are
C     complex M-by-M and N-by-N matrices, respectively, in Schur form.
C
C     This routine is intended to be called only by SLICOT Library
C     routine MB03RZ. For efficiency purposes, the computations are
C     aborted when the absolute value of an element of X is greater than
C     a given value PMAX.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     M       (input) INTEGER
C             The order of the matrix A and the number of rows of the
C             matrices C and X.  M >= 0.
C
C     N       (input) INTEGER
C             The order of the matrix B and the number of columns of the
C             matrices C and X.  N >= 0.
C
C     PMAX    (input) DOUBLE PRECISION
C             An upper bound for the absolute value of the elements of X
C             (see METHOD).
C
C     A       (input) COMPLEX*16 array, dimension (LDA,M)
C             The leading M-by-M upper triangular part of this array
C             must contain the matrix A of the Sylvester equation.
C             The elements below the diagonal are not referenced.
C
C     LDA     INTEGER
C             The leading dimension of array A.  LDA >= MAX(1,M).
C
C     B       (input) COMPLEX*16 array, dimension (LDB,N)
C             The leading N-by-N upper triangular part of this array
C             must contain the matrix B of the Sylvester equation.
C             The elements below the diagonal are not referenced.
C
C     LDB     INTEGER
C             The leading dimension of array B.  LDB >= MAX(1,N).
C
C     C       (input/output) COMPLEX*16 array, dimension (LDC,N)
C             On entry, the leading M-by-N part of this array must
C             contain the matrix C of the Sylvester equation.
C             On exit, if INFO = 0, the leading M-by-N part of this
C             array contains the solution matrix X of the Sylvester
C             equation, and each element of X (see METHOD) has the
C             absolute value less than or equal to PMAX.
C             On exit, if INFO = 1, the solution matrix X has not been
C             computed completely, because an element of X had the
C             absolute value greater than PMAX. Part of the matrix C has
C             possibly been overwritten with the corresponding part
C             of X.
C
C     LDC     INTEGER
C             The leading dimension of array C.  LDC >= MAX(1,M).
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             = 1:  an element of X had the absolute value greater than
C                   the given value PMAX.
C             = 2:  A and B have common or very close eigenvalues;
C                   perturbed values were used to solve the equation
C                   (but the matrices A and B are unchanged). This is a
C                   warning.
C
C     METHOD
C
C     The routine uses an adaptation of the standard method for solving
C     Sylvester equations [1], which controls the magnitude of the
C     individual elements of the computed solution [2]. The equation
C     -AX + XB = C can be rewritten as
C                                  m            l-1
C       -A  X   + X  B   = C   +  sum  A  X   - sum  X  B
C         kk kl    kl ll    kl   i=k+1  ki il   j=1   kj jl
C
C     for l = 1:n, and k = m:-1:1, where A  , B  , C  , and X  , are the
C                                         kk   ll   kl       kl
C     elements defined by the partitioning induced by the Schur form
C     of A and B. So, the elements of X are found column by column,
C     starting from the bottom. If any such element has the absolute
C     value greater than the given value PMAX, the calculations are
C     ended.
C
C     REFERENCES
C
C     [1] Bartels, R.H. and Stewart, G.W.  T
C         Solution of the matrix equation A X + XB = C.
C         Comm. A.C.M., 15, pp. 820-826, 1972.
C
C     [2] Bavely, C. and Stewart, G.W.
C         An Algorithm for Computing Reducing Subspaces by Block
C         Diagonalization.
C         SIAM J. Numer. Anal., 16, pp. 359-367, 1979.
C
C     NUMERICAL ASPECTS
C                               2      2
C     The algorithm requires 0(M N + MN ) operations.
C
C     FURTHER COMMENTS
C
C     Let
C
C            ( A   C )       ( I   X )
C        M = (       ),  Y = (       ).
C            ( 0   B )       ( 0   I )
C
C     Then
C
C         -1      ( A   0 )
C        Y  M Y = (       ),
C                 ( 0   B )
C
C     hence Y is a non-unitary transformation matrix which performs the
C     reduction of M to a block-diagonal form. Bounding a norm of X is
C     equivalent to setting an upper bound to the condition number of
C     the transformation matrix Y.
C
C     CONTRIBUTOR
C
C     V. Sima, June 2021.
C
C     REVISIONS
C
C     -
C
C     KEYWORDS
C
C     Diagonalization, Schur form, Sylvester equation.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ONE
      PARAMETER         ( ONE = 1.0D+0 )
      COMPLEX*16        CONE
      PARAMETER         ( CONE = ( 1.0D0, 0.0D0 ) )
C     .. Scalar Arguments ..
      INTEGER           INFO, LDA, LDB, LDC, M, N
      DOUBLE PRECISION  PMAX
C     .. Array Arguments ..
      COMPLEX*16        A(LDA,*), B(LDB,*), C(LDC,*)
C     .. Local Scalars ..
      INTEGER           K, K1, L, LM1
      DOUBLE PRECISION  AA11, AC11, BIGNUM, EPS, SMIN, SMLNUM
      COMPLEX*16        A11, C11, X11
C     .. Local Arrays ..
      DOUBLE PRECISION  DUM( 1 )
C     .. External Functions ..
      DOUBLE PRECISION  DLAMCH, ZLANTR
      COMPLEX*16        ZDOTU,  ZLADIV
      EXTERNAL          DLAMCH, ZDOTU, ZLADIV, ZLANTR
C     .. External Subroutines ..
      EXTERNAL          DLABAD, ZGEMV
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, DBLE, DIMAG, MAX
C     .. Executable Statements ..
C
C     For efficiency reasons, this routine does not check the input
C     parameters for errors.
C
      INFO = 0
C
C     Quick return if possible.
C
      IF( M.EQ.0 .OR. N.EQ.0 )
     $   RETURN
C
C     Set constants to control overflow.
C
      EPS    = DLAMCH( 'P' )
      SMLNUM = DLAMCH( 'S' )
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
      SMLNUM = SMLNUM*DBLE( M*N ) / EPS
      BIGNUM = ONE / SMLNUM
      SMIN   = MAX(  SMLNUM,
     $               EPS*ZLANTR( 'M', 'U', 'N', M, M, A, LDA, DUM ),
     $               EPS*ZLANTR( 'M', 'U', 'N', N, N, B, LDB, DUM ) )
C
C     Column loop indexed by L.
C
      DO 20 L = 1, N
         LM1 = L - 1
C
         IF ( LM1.GT.0 ) THEN
C
C           Update column L of C.
C
            CALL ZGEMV( 'No transpose', M, LM1, -CONE, C, LDC, B(1,L),
     $                  1, CONE, C(1,L), 1 )
         ENDIF
C                                  m            l-1
C       -A  X   + X  B   = C   +  sum  A  X   - sum  X  B
C         kk kl    kl ll    kl   i=k+1  ki il   j=1   kj jl
C
C        Row loop indexed by K.
C
         DO 10 K = M, 1, -1
            K1 = K + 1
            C11 = C(K,L)
            IF ( K.LT.M ) THEN
C
C              Update C(K,L).
C
               C11 = C11 + ZDOTU( M-K, A(K,K1), LDA, C(K1,L), 1 )
            ENDIF
            A11  = B( L, L ) - A( K, K )
            AA11 = ABS( DBLE( A11 ) ) + ABS( DIMAG( A11 ) )
            IF( AA11.LE.SMIN ) THEN
               A11  = SMIN
               AA11 = SMIN
               INFO = 2
            END IF
            AC11 = ABS( DBLE( C11 ) ) + ABS( DIMAG( C11 ) )
            IF( AA11.LT.ONE .AND. AC11.GT.ONE ) THEN
               IF( AC11.GT.BIGNUM*AA11 ) THEN
                  INFO = 1
                  RETURN
               END IF
            END IF
            X11 = ZLADIV( C11, A11 )
            IF( ABS( X11 ).GT.PMAX ) THEN
                INFO = 1
                RETURN
            END IF
            C(K,L) = X11
   10    CONTINUE
   20 CONTINUE
C
      RETURN
C *** Last line of MB03RW ***
      END
