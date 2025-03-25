      SUBROUTINE MB03JD( COMPQ, N, A, LDA, D, LDD, B, LDB, F, LDF, Q,
     $                   LDQ, NEIG, IWORK, LIWORK, DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     To move the eigenvalues with strictly negative real parts of an
C     N-by-N real skew-Hamiltonian/Hamiltonian pencil aS - bH in
C     structured Schur form,
C
C           (  A  D  )      (  B  F  )
C       S = (        ), H = (        ),
C           (  0  A' )      (  0 -B' )
C
C     with A upper triangular and B upper quasi-triangular, to the
C     leading principal subpencil, while keeping the triangular form.
C     The notation M' denotes the transpose of the matrix M.
C     The matrices S and H are transformed by an orthogonal matrix Q
C     such that
C
C                            (  Aout  Dout  )  
C       Sout = J Q' J' S Q = (              ),
C                            (    0   Aout' )  
C                                                                    (1)
C                            (  Bout  Fout  )           (  0  I  )
C       Hout = J Q' J' H Q = (              ), with J = (        ),
C                            (  0    -Bout' )           ( -I  0  )
C
C     where Aout is upper triangular and Bout is upper quasi-triangular.
C     Optionally, if COMPQ = 'I' or COMPQ = 'U', the orthogonal matrix Q
C     that fulfills (1), is computed.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     COMPQ   CHARACTER*1
C             Specifies whether or not the orthogonal transformations
C             should be accumulated in the array Q, as follows:
C             = 'N':  Q is not computed;
C             = 'I':  the array Q is initialized internally to the unit
C                     matrix, and the orthogonal matrix Q is returned;
C             = 'U':  the array Q contains an orthogonal matrix Q0 on
C                     entry, and the matrix Q0*Q is returned, where Q
C                     is the product of the orthogonal transformations
C                     that are applied to the pencil aS - bH to reorder
C                     the eigenvalues.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the pencil aS - bH.  N >= 0, even.
C
C     A       (input/output) DOUBLE PRECISION array, dimension
C                            (LDA, N/2)
C             On entry, the leading N/2-by-N/2 part of this array must
C             contain the upper triangular matrix A. The elements of the
C             strictly lower triangular part of this array are not used.
C             On exit, the leading  N/2-by-N/2 part of this array
C             contains the transformed matrix Aout.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1, N/2).
C
C     D       (input/output) DOUBLE PRECISION array, dimension
C                           (LDD, N/2)
C             On entry, the leading N/2-by-N/2 part of this array must
C             contain the upper triangular part of the skew-symmetric
C             matrix D. The diagonal need not be set to zero.
C             On exit, the leading  N/2-by-N/2 part of this array
C             contains the transformed upper triangular part of the
C             matrix Dout.
C             The strictly lower triangular part of this array is
C             not referenced, except for the element D(N/2,N/2-1), but
C             its initial value is preserved.
C
C     LDD     INTEGER
C             The leading dimension of the array D.  LDD >= MAX(1, N/2).
C
C     B       (input/output) DOUBLE PRECISION array, dimension
C                            (LDB, N/2)
C             On entry, the leading N/2-by-N/2 part of this array must
C             contain the upper quasi-triangular matrix B.
C             On exit, the leading  N/2-by-N/2 part of this array
C             contains the transformed upper quasi-triangular part of
C             the matrix Bout.
C             The part below the first subdiagonal of this array is
C             not referenced.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= MAX(1, N/2).
C
C     F       (input/output) DOUBLE PRECISION array, dimension
C                           (LDF, N/2)
C             On entry, the leading N/2-by-N/2 part of this array must
C             contain the upper triangular part of the symmetric matrix
C             F.
C             On exit, the leading  N/2-by-N/2 part of this array
C             contains the transformed upper triangular part of the
C             matrix Fout.
C             The strictly lower triangular part of this array is not
C             referenced, except for the element F(N/2,N/2-1), but its
C             initial value is preserved.
C
C     LDF     INTEGER
C             The leading dimension of the array F.  LDF >= MAX(1, N/2).
C
C     Q       (input/output) DOUBLE PRECISION array, dimension (LDQ, N)
C             On entry, if COMPQ = 'U', then the leading N-by-N part of
C             this array must contain a given matrix Q0, and on exit,
C             the leading N-by-N part of this array contains the product
C             of the input matrix Q0 and the transformation matrix Q
C             used to transform the matrices S and H.
C             On exit, if COMPQ = 'I', then the leading N-by-N part of
C             this array contains the orthogonal transformation matrix
C             Q.
C             If COMPQ = 'N' this array is not referenced.
C
C     LDQ     INTEGER
C             The leading dimension of the array Q.
C             LDQ >= 1,         if COMPQ = 'N';
C             LDQ >= MAX(1, N), if COMPQ = 'I' or COMPQ = 'U'.
C
C     NEIG    (output) INTEGER
C             The number of eigenvalues in aS - bH with strictly
C             negative real part.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (LIWORK)
C
C     LIWORK  INTEGER
C             The dimension of the array IWORK.
C             LIWORK >= N+1.
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C
C     LDWORK  INTEGER
C             The dimension of the array DWORK.
C             If COMPQ = 'N',
C                LDWORK >= MAX(2*N+32,108);
C             if COMPQ = 'I' or COMPQ = 'U',
C                LDWORK >= MAX(4*N+32,108).
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0: succesful exit;
C             < 0: if INFO = -i, the i-th argument had an illegal value;
C             = 1: error occured during execution of MB03DD;
C             = 2: error occured during execution of MB03HD.
C
C     METHOD
C
C     The algorithm reorders the eigenvalues like the following scheme:
C
C     Step 1: Reorder the eigenvalues in the subpencil aA - bB.
C          I. Reorder the eigenvalues with negative real parts to the
C             top.
C         II. Reorder the eigenvalues with positive real parts to the
C             bottom.
C
C     Step 2: Reorder the remaining eigenvalues with negative real
C             parts in the pencil aS - bH.
C          I. Exchange the eigenvalues between the last diagonal block
C             in aA - bB and the last diagonal block in aS - bH.
C         II. Move the eigenvalues of the R-th block to the (MM+1)-th
C             block, where R denotes the number of upper quasi-
C             triangular blocks in aA - bB and MM denotes the current
C             number of blocks in aA - bB with eigenvalues with negative
C             real parts.
C
C     The algorithm uses a sequence of orthogonal transformations as
C     described on page 33 in [1]. To achieve those transformations the
C     elementary subroutines MB03DD and MB03HD are called for the
C     corresponding matrix structures.
C
C     REFERENCES
C
C     [1] Benner, P., Byers, R., Losse, P., Mehrmann, V. and Xu, H.
C         Numerical Solution of Real Skew-Hamiltonian/Hamiltonian
C         Eigenproblems.
C         Tech. Rep., Technical University Chemnitz, Germany,
C         Nov. 2007.
C
C     NUMERICAL ASPECTS
C                                                               3
C     The algorithm is numerically backward stable and needs O(N ) real
C     floating point operations.
C
C     CONTRIBUTOR
C
C     Matthias Voigt, Fakultaet fuer Mathematik, Technische Universitaet
C     Chemnitz, October 16, 2008.
C     V. Sima, Dec. 2009 (SLICOT version of the routine DHAUNX).
C
C     REVISIONS
C
C     V. Sima, Aug. 2009; Jan. 2010, Oct. 2010, Nov. 2010.
C     M. Voigt, Jan. 2012.
C
C     KEYWORDS
C
C     Eigenvalue reordering, upper (quasi-)triangular matrix,
C     skew-Hamiltonian/Hamiltonian pencil, structured Schur form.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, HALF, TEN
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, HALF = 0.5D+0,
     $                     TEN = 1.0D+1 )
C
C     .. Scalar Arguments ..
      CHARACTER          COMPQ
      INTEGER            INFO, LDA, LDB, LDD, LDF, LDQ, LDWORK, LIWORK,
     $                   N, NEIG
C
C     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), D( LDD, * ),
     $                   DWORK( * ),  F( LDF, * ), Q( LDQ, * )
C
C     .. Local Scalars ..
      LOGICAL            LCMPQ, LINIQ, LUPDQ
      INTEGER            DIM1, DIM2, HLP, I, IA, IAUPLE, IB, IB1, IB2,
     $                   IB3, IBUPLE, IBUPRI, IC, ICS, IQ1, IQ2, IQLOLE,
     $                   IQLORI, IQUPLE, IQUPRI, IR, IS, ITMP1, ITMP2,
     $                   ITMP3, IUPD, IWRK1, IWRK2, IWRK3, IWRK4, IWRK5,
     $                   J, K, LDW, M, MM, MP, NCOL, NCOLS, NROW, NROWS,
     $                   OPTDW, R, SDIM, UPDS
      DOUBLE PRECISION   A2, D1, D2, D3, F2, NRMA, NRMB, PREC, Q11, Q12,
     $                   Q21, Q22, TMP, TOL
C
C     .. Local Arrays ..
      DOUBLE PRECISION   PAR( 2 )
C
C     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DDOT, DLAMCH, DLANHS, DLANTR
      EXTERNAL           DDOT, DLAMCH, DLANHS, DLANTR, LSAME
C
C     .. External Subroutines ..
      EXTERNAL           DAXPY, DCOPY, DGEMM, DGEMV, DLACPY, DLASET,
     $                   DSCAL, MB01LD, MB01RU, MB01RX, MB03DD, MB03HD,
     $                   XERBLA
C
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, INT, MAX, MIN, MOD, SIGN
C
C     .. Executable Statements ..
C
C     Decode the input arguments.
C
      M = N/2
      LINIQ = LSAME( COMPQ, 'I' )
      LUPDQ = LSAME( COMPQ, 'U' )
      LCMPQ = LINIQ .OR. LUPDQ
      IF( LCMPQ ) THEN
         OPTDW = MAX( 4*N+32, 108 )
      ELSE
         OPTDW = MAX( 2*N+32, 108 )
      END IF
C
C     Test the input arguments.
C
      INFO = 0
      IF( .NOT.( LSAME( COMPQ, 'N' ) .OR. LCMPQ ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 .OR. MOD( N, 2 ).NE.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      ELSE IF( LDD.LT.MAX( 1, M ) ) THEN
         INFO = -6
      ELSE IF( LDB.LT.MAX( 1, M ) ) THEN
         INFO = -8
      ELSE IF( LDF.LT.MAX( 1, M ) ) THEN
         INFO = -10
      ELSE IF( LDQ.LT.1 .OR. ( LCMPQ .AND. LDQ.LT.N ) ) THEN
         INFO = -12
      ELSE IF( LIWORK.LT.N+1 ) THEN
         INFO = -15
      ELSE IF( LDWORK.LT.OPTDW ) THEN
         INFO = -17
      END IF
      IF( INFO.NE.0) THEN
         CALL XERBLA( 'MB03JD', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( N.EQ.0 ) THEN
         NEIG = 0
         RETURN
      END IF
C
C     Determine machine constants.
C
      PREC = DLAMCH( 'Precision' )
      TOL  = MIN( DBLE( N ), TEN )*PREC
C
      PAR( 1 ) = PREC
      PAR( 2 ) = DLAMCH( 'Safe minimum' )
C
C     STEP 0: Determine location and size of diagonal blocks.
C             IWORK(J) and IWORK(IS+J) are used to indicate the
C             beginning index and the kind of eigenvalues of the
C             J-th diagonal block of the subpencil aA - bB. For a
C             2-by-2 block, it is assumed that both eigenvalues have
C             real parts with the same sign (true for a structured
C             Schur form).
C
      I  = 1
      J  = 1
      IS = M + 1
C
      NRMA = DLANTR( 'One', 'Upper', 'Non-diag', M, M, A, LDA, DWORK )
      NRMB = DLANHS( 'One', M, B, LDB, DWORK )
C
C     Partition blocks.
C
C     WHILE( I.LE.M-1 ) DO
C
   10 CONTINUE
      IF( I.LE.M-1 ) THEN
         IWORK( J ) = I
         IF( ABS( B( I+1, I ) ).LE.TOL*NRMB ) THEN
C
C           1-by-1 block.
C
            B( I+1, I ) = ZERO
            IF( ABS( A( I, I ) ).LE.TOL*NRMA .OR.
     $          ABS( B( I, I ) ).LE.TOL*NRMB ) THEN
C
C              Eigenvalue is infinite, 0, or 0/0.
C
               IWORK( IS+J ) = 0
            ELSE
               IWORK( IS+J ) = INT( SIGN( ONE, A( I, I )*B( I, I ) ) )
            END IF
            I = I + 1
         ELSE
C
C           2-by-2 block.
C
            IF( A( I, I ).EQ.ZERO .OR. A( I+1, I+1 ).EQ.ZERO ) THEN
C
C              Eigenvalue is infinite.
C
               IWORK( IS+J ) = 0
            ELSE
               TMP = ( B( I, I ) - ( B( I+1, I )   / A( I+1, I+1 ) )*
     $                               A( I, I+1 ) ) / A( I, I )  +
     $                               B( I+1, I+1 ) / A( I+1, I+1 )
               IF( TMP.EQ.ZERO ) THEN
                  IWORK( IS+J ) = 0
               ELSE
                  IWORK( IS+J ) = INT( SIGN( ONE, TMP ) )
               END IF
            END IF
            I = I + 2
         END IF
         J = J + 1
      GO TO 10
C
C     END WHILE 10
C
      END IF
C
      IF( I.EQ.M ) THEN
         IWORK( J ) = I
         IF( ABS( A( I, I ) ).LE.TOL*NRMA .OR.
     $       ABS( B( I, I ) ).LE.TOL*NRMB ) THEN
C
C           Eigenvalue is infinite or zero.
C
            IWORK( IS+J ) = 0
         ELSE
            IWORK( IS+J ) = INT( SIGN( ONE, A( I, I )*B( I, I ) ) )
         END IF
         J = J + 1
      END IF
C
      R = J - 1
C
C     Initialize Q if appropriate.
C
      IF( LINIQ ) THEN
         IUPD = M + 1
         UPDS = M
         CALL DLASET( 'Full', N, N, ZERO, ONE, Q, LDQ )
      ELSE IF( LUPDQ ) THEN
         IUPD = 1
         UPDS = N
      END IF
C
      IF( M.GT.1 ) THEN
C
C        Save the lower triangle of the submatrix D(M-1:M,M-1:M) and the
C        elements A(M,M-1), F(M,M-1), which might be overwritten.
C
         D1 = D( M-1, M-1 )
         D2 = D(   M, M-1 )
         D3 = D(   M,   M )
         A2 = A(   M, M-1 )
         F2 = F(   M, M-1 )
      END IF
C
C     STEP 1: Reorder the eigenvalues in the subpencil aA - bB.
C
      MM = 0
      MP = J
C
C     I. Reorder the eigenvalues with negative real parts to the top.
C
C     Set pointers for the inputs and outputs of MB03DD.
C
      IQ1   = 1
      IQ2   = IQ1 + 16
      IA    = IQ2 + 16
      IB    = IA  + 16
      IWRK1 = IB  + 16
      IWRK2 = IA
C
      K   = 1
      IB3 = M + 1
      IWORK( R+1 ) = IB3
C
C     WHILE( K.LE.R ) DO
C
   20 CONTINUE
      IF( K.LE.R ) THEN
         IF( IWORK( IS+K ).LT.0 ) THEN
            DO 30 J = K - 1, MM + 1, -1
C
C              IB1, IB2, and IB3 are pointers to 3 consecutive blocks.
C
               IB1  = IWORK( J )
               IB2  = IWORK( J+1 )
               IB3  = IWORK( J+2 )
               DIM1 = IB2  - IB1
               DIM2 = IB3  - IB2
               SDIM = DIM1 + DIM2
C
C              Copy the relevant part of A(ib1:ib3-1,ib1:ib3-1) and
C              B(ib1:ib3-1,ib1:ib3-1) to DWORK as inputs for MB03DD.
C              Also, set the additional zero elements.
C
               CALL DLACPY( 'Upper', SDIM, SDIM, A( IB1, IB1 ), LDA,
     $                      DWORK( IA ), SDIM )
               CALL DLASET( 'Lower', SDIM-1, SDIM-1, ZERO, ZERO,
     $                      DWORK( IA+1 ), SDIM )
               CALL DLACPY( 'Upper', SDIM, SDIM, B( IB1, IB1 ), LDB,
     $                      DWORK( IB ), SDIM )
               CALL DCOPY(  SDIM-1, B( IB1+1, IB1 ), LDB+1,
     $                      DWORK( IB+1 ), SDIM+1 )
               CALL DLASET( 'Lower', SDIM-2, SDIM-2, ZERO, ZERO,
     $                      DWORK( IB+2 ), SDIM )
C
C              Perform eigenvalue/matrix block exchange.
C              Workspace: IWRK1 + 43.
C
               CALL MB03DD( 'Triangular', DIM1, DIM2, PREC, DWORK( IB ),
     $                      SDIM, DWORK( IA ), SDIM, DWORK( IQ1 ), SDIM,
     $                      DWORK( IQ2 ), SDIM, DWORK( IWRK1 ),
     $                      LDWORK-IWRK1+1, INFO )
               IF( INFO.GT.0 ) THEN
                  INFO = 1
                  RETURN
               END IF
C
C              Copy the transformed diagonal blocks, if sdim > 2.
C
               NROWS = IB1 - 1
               NCOLS = M - IB3 + 1
               ICS   = IB3
               IF( SDIM.GT.2 ) THEN
                  CALL DLACPY( 'Upper', SDIM, SDIM, DWORK( IA ), SDIM,
     $                         A( IB1, IB1 ), LDA )
                  CALL DLACPY( 'Upper', SDIM, SDIM, DWORK( IB ), SDIM,
     $                         B( IB1, IB1 ), LDB )
                  CALL DCOPY(  SDIM-1, DWORK( IB+1 ), SDIM+1,
     $                         B( IB1+1, IB1 ), LDB+1 )
                  NROW = NROWS
                  NCOL = NCOLS
                  IC   = ICS
                  LDW  = MAX( 1, NROW )
               ELSE
                  TMP = A( IB1+1, IB1 )
                  A( IB1+1, IB1 ) = ZERO
                  NROW = IB3 - 1
                  NCOL = M - IB1 + 1
                  IC   = IB1
                  LDW  = NROW
               END IF
C
C              Update A.
C              Workspace: IWRK2 + 2*N - 1.
C
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, SDIM,
     $                      SDIM, ONE, A( 1, IB1 ), LDA, DWORK( IQ1 ),
     $                      SDIM, ZERO, DWORK( IWRK2 ), LDW )
               CALL DLACPY( 'Full', NROW, SDIM, DWORK( IWRK2 ), LDW,
     $                      A( 1, IB1 ), LDA )
               CALL DGEMM(  'Transpose', 'No Transpose', SDIM, NCOL,
     $                      SDIM, ONE, DWORK( IQ2 ), SDIM, A( IB1, IC ),
     $                      LDA, ZERO, DWORK( IWRK2 ), SDIM )
               CALL DLACPY( 'Full', SDIM, NCOL, DWORK( IWRK2 ), SDIM,
     $                      A( IB1, IC ), LDA )
               IF( SDIM.EQ.2 )
     $            A( IB1+1, IB1 ) = TMP
C
C              Update D.
C
               CALL DGEMM(  'No Transpose', 'No Transpose', NROWS, SDIM,
     $                      SDIM, ONE, D( 1, IB1 ), LDD, DWORK( IQ2 ),
     $                      SDIM, ZERO, DWORK( IWRK2 ), LDW )
               CALL DLACPY( 'Full', NROWS, SDIM, DWORK( IWRK2 ), LDW,
     $                      D( 1, IB1 ), LDD )
               CALL DGEMM(  'Transpose', 'No Transpose', SDIM, NCOLS,
     $                      SDIM, ONE, DWORK( IQ2 ), SDIM,
     $                      D( IB1, ICS ), LDD, ZERO, DWORK( IWRK2 ),
     $                      SDIM )
               CALL DLACPY( 'Full', SDIM, NCOLS, DWORK( IWRK2 ), SDIM,
     $                      D( IB1, ICS ), LDD )
               CALL MB01LD( 'Upper', 'Transpose', SDIM, SDIM, ZERO, ONE,
     $                      D( IB1, IB1 ), LDD, DWORK( IQ2 ), SDIM,
     $                      D( IB1, IB1 ), LDD, DWORK( IWRK2 ),
     $                      LDWORK-IWRK2+1, INFO )
C
C              Update B.
C
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, SDIM,
     $                      SDIM, ONE, B( 1, IB1 ), LDB, DWORK( IQ1 ),
     $                      SDIM, ZERO, DWORK( IWRK2 ), LDW )
               CALL DLACPY( 'Full', NROW, SDIM, DWORK( IWRK2 ), LDW,
     $                      B( 1, IB1 ), LDB )
               CALL DGEMM(  'Transpose', 'No Transpose', SDIM, NCOL,
     $                      SDIM, ONE, DWORK( IQ2 ), SDIM, B( IB1, IC ),
     $                      LDB, ZERO, DWORK( IWRK2 ), SDIM )
               CALL DLACPY( 'Full', SDIM, NCOL, DWORK( IWRK2 ), SDIM,
     $                      B( IB1, IC ), LDB )
C
C              Update F.
C
               CALL DGEMM(  'No Transpose', 'No Transpose', NROWS, SDIM,
     $                      SDIM, ONE, F( 1, IB1 ), LDF, DWORK( IQ2 ),
     $                      SDIM, ZERO, DWORK( IWRK2 ), LDW )
               CALL DLACPY( 'Full', NROWS, SDIM, DWORK( IWRK2 ), LDW,
     $                      F( 1, IB1 ), LDF )
               CALL DGEMM(  'Transpose', 'No Transpose', SDIM, NCOLS,
     $                      SDIM, ONE, DWORK( IQ2 ), SDIM,
     $                      F( IB1, ICS ), LDF, ZERO, DWORK( IWRK2 ),
     $                      SDIM )
               CALL DLACPY( 'Full', SDIM, NCOLS, DWORK( IWRK2 ), SDIM,
     $                      F( IB1, ICS ), LDF )
               CALL MB01RU( 'Upper', 'Transpose', SDIM, SDIM, ZERO, ONE,
     $                      F( IB1, IB1 ), LDF, DWORK( IQ2 ), SDIM,
     $                      F( IB1, IB1 ), LDF, DWORK( IWRK2 ),
     $                      LDWORK-IWRK2+1, INFO )
               CALL DSCAL( SDIM, HALF, F( IB1, IB1 ), LDF+1 )
C
               IF( LCMPQ ) THEN
C
C                 Update Q.
C                 Workspace: IWRK2 + 2*N - 1, if COMPQ = 'I';
C                            IWRK2 + 4*N - 1, if COMPQ = 'U'.
C
                  CALL DGEMM(  'No Transpose', 'No Transpose', UPDS,
     $                         SDIM, SDIM, ONE, Q( 1, IB1 ), LDQ,
     $                         DWORK( IQ1 ), SDIM, ZERO, DWORK( IWRK2 ),
     $                         UPDS )
                  CALL DLACPY( 'Full', UPDS, SDIM, DWORK( IWRK2 ), UPDS,
     $                         Q( 1, IB1 ), LDQ )
                  CALL DGEMM(  'No Transpose', 'No Transpose', UPDS,
     $                         SDIM, SDIM, ONE, Q( IUPD, M+IB1 ), LDQ,
     $                         DWORK( IQ2 ), SDIM, ZERO, DWORK( IWRK2 ),
     $                         UPDS )
                  CALL DLACPY( 'Full', UPDS, SDIM, DWORK( IWRK2 ), UPDS,
     $                         Q( IUPD, M+IB1 ), LDQ )
               END IF
C
C              Update index lists IWORK(1:M) and IWORK(M+2:N+1) if a
C              1-by-1 and 2-by-2 block have been swapped.
C
               HLP = DIM2 - DIM1
               IF( HLP.EQ.1 ) THEN
C
C                 First block was 2-by-2.
C
                  IWORK( J+1 ) = IB1 + 1
               ELSE IF( HLP.EQ.-1 ) THEN
C
C                 Second block was 2-by-2.
C
                  IWORK( J+1 ) = IB1 + 2
               END IF
C
C              Update IWORK(M+2:N+1).
C
               HLP = IWORK( IS+J )
               IWORK( IS+J )   = IWORK( IS+J+1 )
               IWORK( IS+J+1 ) = HLP
   30       CONTINUE
            MM = MM + 1
         END IF
         K = K + 1
      GO TO 20
C
C     END WHILE 20
C
      END IF
C
C     II. Reorder the eigenvalues with positive real parts to the bottom.
C
      K = R
C
C     WHILE( K.GE.MM+1 ) DO
C
   40 CONTINUE
      IF( K.GE.MM + 1 ) THEN
         IF( IWORK( IS+K ).GT.0 ) THEN
            DO 50 J = K, MP - 2
               IB1  = IWORK( J )
               IB2  = IWORK( J+1 )
               IB3  = IWORK( J+2 )
               DIM1 = IB2  - IB1
               DIM2 = IB3  - IB2
               SDIM = DIM1 + DIM2
C
C              Copy the relevant part of A(ib1:ib3-1,ib1:ib3-1) and
C              B(ib1:ib3-1,ib1:ib3-1) to DWORK as inputs for MB03DD.
C              Also, set the additional zero elements.
C
               CALL DLACPY( 'Upper', SDIM, SDIM, A( IB1, IB1 ), LDA,
     $                      DWORK( IA ), SDIM )
               CALL DLASET( 'Lower', SDIM-1, SDIM-1, ZERO, ZERO,
     $                      DWORK( IA+1 ), SDIM )
               CALL DLACPY( 'Upper', SDIM, SDIM, B( IB1, IB1 ), LDB,
     $                      DWORK( IB ), SDIM )
               CALL DCOPY(  SDIM-1, B( IB1+1, IB1 ), LDB+1,
     $                      DWORK( IB+1 ), SDIM+1 )
               CALL DLASET( 'Lower', SDIM-2, SDIM-2, ZERO, ZERO,
     $                      DWORK( IB+2 ), SDIM )
C
C              Perform eigenvalue/matrix block exchange.
C
               CALL MB03DD( 'Triangular', DIM1, DIM2, PREC, DWORK( IB ),
     $                      SDIM, DWORK( IA ), SDIM, DWORK( IQ1 ), SDIM,
     $                      DWORK( IQ2 ), SDIM, DWORK( IWRK1 ),
     $                      LDWORK-IWRK1+1, INFO )
               IF( INFO.GT.0 ) THEN
                  INFO = 1
                  RETURN
               END IF
C
C              Copy the transformed diagonal blocks, if sdim > 2.
C
               NROWS = IB1 - 1
               NCOLS = M - IB3 + 1
               ICS   = IB3
               IF( SDIM.GT.2 ) THEN
                  CALL DLACPY( 'Upper', SDIM, SDIM, DWORK( IA ), SDIM,
     $                         A( IB1, IB1 ), LDA )
                  CALL DLACPY( 'Upper', SDIM, SDIM, DWORK( IB ), SDIM,
     $                         B( IB1, IB1 ), LDB )
                  CALL DCOPY(  SDIM-1, DWORK( IB+1 ), SDIM+1,
     $                         B( IB1+1, IB1 ), LDB+1 )
                  NROW = NROWS
                  NCOL = NCOLS
                  IC   = ICS
                  LDW  = MAX( 1, NROW )
               ELSE
                  TMP = A( IB1+1, IB1 )
                  A( IB1+1, IB1 ) = ZERO
                  NROW = IB3 - 1
                  NCOL = M - IB1 + 1
                  IC   = IB1
                  LDW  = NROW
               END IF
C
C              Update A.
C
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, SDIM,
     $                      SDIM, ONE, A( 1, IB1 ), LDA, DWORK( IQ1 ),
     $                      SDIM, ZERO, DWORK( IWRK2 ), LDW )
               CALL DLACPY( 'Full', NROW, SDIM, DWORK( IWRK2 ), LDW,
     $                      A( 1, IB1 ), LDA )
               CALL DGEMM(  'Transpose', 'No Transpose', SDIM, NCOL,
     $                      SDIM, ONE, DWORK( IQ2 ), SDIM, A( IB1, IC ),
     $                      LDA, ZERO, DWORK( IWRK2 ), SDIM )
               CALL DLACPY( 'Full', SDIM, NCOL, DWORK( IWRK2 ), SDIM,
     $                      A( IB1, IC ), LDA )
               IF( SDIM.EQ.2 )
     $            A( IB1+1, IB1 ) = TMP
C
C              Update D.
C
               CALL DGEMM(  'No Transpose', 'No Transpose', NROWS, SDIM,
     $                      SDIM, ONE, D( 1, IB1 ), LDD, DWORK( IQ2 ),
     $                      SDIM, ZERO, DWORK( IWRK2 ), LDW )
               CALL DLACPY( 'Full', NROWS, SDIM, DWORK( IWRK2 ), LDW,
     $                      D( 1, IB1 ), LDD )
               CALL DGEMM(  'Transpose', 'No Transpose', SDIM, NCOLS,
     $                      SDIM, ONE, DWORK( IQ2 ), SDIM,
     $                      D( IB1, ICS ), LDD, ZERO, DWORK( IWRK2 ),
     $                      SDIM )
               CALL DLACPY( 'Full', SDIM, NCOLS, DWORK( IWRK2 ), SDIM,
     $                      D( IB1, ICS ), LDD )
               CALL MB01LD( 'Upper', 'Transpose', SDIM, SDIM, ZERO, ONE,
     $                      D( IB1, IB1 ), LDD, DWORK( IQ2 ), SDIM,
     $                      D( IB1, IB1 ), LDD, DWORK( IWRK2 ),
     $                      LDWORK-IWRK2+1, INFO )
C
C              Update B.
C
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, SDIM,
     $                      SDIM, ONE, B( 1, IB1 ), LDB, DWORK( IQ1 ),
     $                      SDIM, ZERO, DWORK( IWRK2 ), LDW )
               CALL DLACPY( 'Full', NROW, SDIM, DWORK( IWRK2 ), LDW,
     $                      B( 1, IB1 ), LDB )
               CALL DGEMM(  'Transpose', 'No Transpose', SDIM, NCOL,
     $                      SDIM, ONE, DWORK( IQ2 ), SDIM, B( IB1, IC ),
     $                      LDB, ZERO, DWORK( IWRK2 ), SDIM )
               CALL DLACPY( 'Full', SDIM, NCOL, DWORK( IWRK2 ), SDIM,
     $                      B( IB1, IC ), LDB )
C
C              Update F.
C
               CALL DGEMM(  'No Transpose', 'No Transpose', NROWS, SDIM,
     $                      SDIM, ONE, F( 1, IB1 ), LDF, DWORK( IQ2 ),
     $                      SDIM, ZERO, DWORK( IWRK2 ), LDW )
               CALL DLACPY( 'Full', NROWS, SDIM, DWORK( IWRK2 ), LDW,
     $                      F( 1, IB1 ), LDF )
               CALL DGEMM(  'Transpose', 'No Transpose', SDIM, NCOLS,
     $                      SDIM, ONE, DWORK( IQ2 ), SDIM,
     $                      F( IB1, ICS ), LDF, ZERO, DWORK( IWRK2 ),
     $                      SDIM )
               CALL DLACPY( 'Full', SDIM, NCOLS, DWORK( IWRK2 ), SDIM,
     $                      F( IB1, ICS ), LDF )
               CALL MB01RU( 'Upper', 'Transpose', SDIM, SDIM, ZERO, ONE,
     $                      F( IB1, IB1 ), LDF, DWORK( IQ2 ), SDIM,
     $                      F( IB1, IB1 ), LDF, DWORK( IWRK2 ),
     $                      LDWORK-IWRK2+1, INFO )
               CALL DSCAL( SDIM, HALF, F( IB1, IB1 ), LDF+1 )
C
               IF( LCMPQ ) THEN
C
C                 Update Q.
C
                  CALL DGEMM(  'No Transpose', 'No Transpose', UPDS,
     $                         SDIM, SDIM, ONE, Q( 1, IB1 ), LDQ,
     $                         DWORK( IQ1 ), SDIM, ZERO, DWORK( IWRK2 ),
     $                         UPDS )
                  CALL DLACPY( 'Full', UPDS, SDIM, DWORK( IWRK2 ), UPDS,
     $                         Q( 1, IB1 ), LDQ )
                  CALL DGEMM(  'No Transpose', 'No Transpose', UPDS,
     $                         SDIM, SDIM, ONE, Q( IUPD, M+IB1 ), LDQ,
     $                         DWORK( IQ2 ), SDIM, ZERO, DWORK( IWRK2 ),
     $                         UPDS )
                  CALL DLACPY( 'Full', UPDS, SDIM, DWORK( IWRK2 ), UPDS,
     $                         Q( IUPD, M+IB1 ), LDQ )
               END IF
C
C              Update index list IWORK(1:M) if a 1-by-1 and 2-by-2 block
C              have been swapped. IWORK(M+2:N+1) is not needed anymore,
C              so it is not necessary to update it.
C
               HLP = DIM2 - DIM1
               IF( HLP.EQ.1 ) THEN
C
C                 First block was 2-by-2.
C
                  IWORK( J+1 ) = IB1 + 1
               ELSE IF( HLP.EQ.-1 ) THEN
C
C                 Second block was 2-by-2.
C
                  IWORK( J+1 ) = IB1 + 2
               END IF
   50       CONTINUE
            MP = MP - 1
         END IF
         K = K - 1
      GO TO 40
C
C     END WHILE 40
C
      END IF
C
C     STEP 2: Reorder the remaining eigenvalues with negative real parts.
C
C     Set pointers for the inputs and outputs of MB03HD.
C
      IQUPLE = 1
      IAUPLE = IQUPLE + 16
      IBUPLE = IAUPLE +  8
      IWRK5  = IBUPLE +  8
      IWRK3  = IAUPLE
      IWRK4  = IWRK3  + 2*N
      ITMP1  = IWRK3  + N
      ITMP2  = ITMP1  + 4
      ITMP3  = ITMP2  + 4
C
      DO 70 K = R, MP, -1
C
C        I. Exchange the eigenvalues between two diagonal blocks.
C
         IR   = IWORK( R )
         DIM1 = IWORK( R+1 ) - IR
         SDIM = 2*DIM1
C
         IF( DIM1.EQ.2 ) THEN
            A( M, IR ) = ZERO
C
C           Build the (small) full skew-symmetric matrix D(M-1:M,M-1:M)
C           and the (small) symmetric matrix F(M-1:M,M-1:M).
C
            D( IR, IR ) =  ZERO
            D(  M, IR ) = -D( IR, M )
            D(  M,  M ) =  ZERO
            F(  M, IR ) =  F( IR, M )
         END IF
C
C        Calculate position of submatrices in DWORK.
C
         IBUPRI = IBUPLE + DIM1*DIM1
         IQLOLE = IQUPLE + DIM1
         IQUPRI = IQUPLE + DIM1*SDIM
         IQLORI = IQUPRI + DIM1
C
C        Generate input matrices for MB03HD built of submatrices of A,
C        D, B, and F.
C
         IF( DIM1.EQ.2 ) THEN
            CALL DLACPY( 'Upper', DIM1, DIM1, A( IR, IR ), LDA,
     $                   DWORK( IAUPLE ), DIM1 )
            DWORK( IAUPLE+6 ) = D( IR, IR+1 )
            CALL DLACPY( 'Full', DIM1, DIM1, B( IR, IR ), LDB,
     $                   DWORK( IBUPLE ), DIM1 )
            CALL DLACPY( 'Upper', DIM1, DIM1, F( IR, IR ), LDF,
     $                   DWORK( IBUPRI ), DIM1 )
         ELSE
            DWORK( IBUPLE ) = B( IR, IR )
            DWORK( IBUPRI ) = F( IR, IR )
         END IF
C
C        Perform eigenvalue exchange.
C        Workspace: IWRK5 + 23, if SDIM = 4.
C
         CALL MB03HD( SDIM, DWORK( IAUPLE ), DIM1, DWORK( IBUPLE ),
     $                DIM1, PAR, DWORK( IQUPLE ), SDIM, DWORK( IWRK5 ),
     $                INFO )
         IF( INFO.GT.0 ) THEN
            INFO = 2
            RETURN
         END IF
C
         IF( DIM1.EQ.2 ) THEN
C
C           Update A by transformations from the right.
C           Workspace: IWRK3 + N - 1.
C
            CALL DLACPY( 'Full', M, DIM1, A( 1, IR ), LDA,
     $                   DWORK( IWRK3 ), M )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, DIM1, DIM1,
     $                   ONE, DWORK( IWRK3 ), M, DWORK( IQUPLE ), SDIM,
     $                   ZERO, A( 1, IR ), LDA )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, DIM1, DIM1,
     $                   ONE, D( 1, IR ), LDD, DWORK( IQLOLE ), SDIM,
     $                   ONE, A( 1, IR ), LDA )
C
C           Update D by transformations from the right.
C
            CALL DGEMM(  'No Transpose', 'No Transpose', M, DIM1, DIM1,
     $                   ONE, DWORK( IWRK3 ), M, DWORK( IQUPRI ), SDIM,
     $                   ZERO, DWORK( ITMP1 ), M )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, DIM1, DIM1,
     $                   ONE, D( 1, IR ), LDD, DWORK( IQLORI ), SDIM,
     $                   ONE, DWORK( ITMP1 ), M )
            CALL DLACPY( 'Full', M, DIM1, DWORK( ITMP1 ), M, D( 1, IR ),
     $                   LDD )
C
C           Compute the intermediate product Af'*Q21 and the second
C           column of Af'*Q22, with Af = A(M-1:M,M-1:M).
C
            CALL DGEMM( 'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                  ONE, DWORK( IWRK3+M-DIM1 ), M, DWORK( IQLOLE ),
     $                  SDIM, ZERO, DWORK( ITMP1 ), DIM1 )
            CALL DGEMV( 'Transpose', DIM1, DIM1, ONE,
     $                  DWORK( IWRK3+M-DIM1 ), M, DWORK( IQLORI+SDIM ),
     $                  1, ZERO, DWORK( ITMP2 ), 1 )
C
C           Update A by transformations from the left.
C
            CALL DLACPY( 'Full', DIM1, DIM1, A( IR, IR ), LDA,
     $                   DWORK( IWRK3 ), DIM1 )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                   -ONE, DWORK( IQUPRI ), SDIM, DWORK( ITMP1 ),
     $                   DIM1, ZERO, A( IR, IR ), LDA )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                   ONE, DWORK( IQLORI ), SDIM, DWORK( IWRK3 ),
     $                   DIM1, ONE, A( IR, IR ), LDA )
C
C           Update D by transformations from the left.
C
            D( IR, M ) = DDOT( DIM1, DWORK( IQLORI ), 1, D( IR, M ), 1 )
     $                 - DDOT( DIM1, DWORK( IQUPRI ), 1, DWORK( ITMP2 ),
     $                         1 )
C
C           Update B by transformations from the right.
C
            CALL DLACPY( 'Full', M, DIM1, B( 1, IR ), LDB,
     $                   DWORK( IWRK3 ), M )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, DIM1, DIM1,
     $                   ONE, DWORK( IWRK3 ), M, DWORK( IQUPLE ), SDIM,
     $                   ZERO, B( 1, IR ), LDB )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, DIM1, DIM1,
     $                   ONE, F( 1, IR ), LDF, DWORK( IQLOLE ), SDIM,
     $                   ONE, B( 1, IR ), LDB )
C
C           Update F by transformations from the right.
C
            CALL DGEMM(  'No Transpose', 'No Transpose', M, DIM1, DIM1,
     $                   ONE, DWORK( IWRK3 ), M, DWORK( IQUPRI ), SDIM,
     $                   ZERO, DWORK( ITMP1 ), M )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, DIM1, DIM1,
     $                   ONE, F( 1, IR ), LDF, DWORK( IQLORI ), SDIM,
     $                   ONE, DWORK( ITMP1 ), M )
            CALL DLACPY( 'Full', M, DIM1, DWORK( ITMP1 ), M, F( 1, IR ),
     $                   LDF )
C
C           Compute intermediate products Bf'*Q21 and Bf'*Q22, with
C           Bf = B(M-1:M,M-1:M).
C
            CALL DGEMM( 'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                  ONE, DWORK( IWRK3+M-DIM1 ), M, DWORK( IQLOLE ),
     $                  SDIM, ZERO, DWORK( ITMP1 ), DIM1 )
C
            CALL DGEMM( 'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                  ONE, DWORK( IWRK3+M-DIM1 ), M, DWORK( IQLORI ),
     $                  SDIM, ZERO, DWORK( ITMP2 ), DIM1 )
C
C           Update B by transformations from the left.
C
            CALL DLACPY( 'Full', DIM1, DIM1, B( IR, IR ), LDB,
     $                   DWORK( ITMP3 ), DIM1 )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                   ONE, DWORK( IQUPRI ), SDIM, DWORK( ITMP1 ),
     $                   DIM1, ZERO, B( IR, IR ), LDB )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                   ONE, DWORK( IQLORI ), SDIM, DWORK( ITMP3 ),
     $                   DIM1, ONE, B( IR, IR ), LDB )
C
C           Update F by transformations from the left.
C
            CALL MB01RX( 'Left', 'Upper', 'Transpose', DIM1, DIM1, ZERO,
     $                   ONE, DWORK( ITMP1 ), DIM1, DWORK( IQLORI ),
     $                   SDIM, F( IR, IR ), LDF, INFO )
            CALL MB01RX( 'Left', 'Upper', 'Transpose', DIM1, DIM1, ONE,
     $                   ONE, DWORK( ITMP1 ), DIM1, DWORK( IQUPRI ),
     $                   SDIM, DWORK( ITMP2 ), DIM1, INFO )
            CALL DLACPY( 'Upper', DIM1, DIM1, DWORK( ITMP1 ), DIM1,
     $                   F( IR, IR ), LDF )
C
            IF( LCMPQ ) THEN
C
C              Update Q.
C              Workspace: IWRK4 + 2*N - 1.
C
               CALL DLACPY( 'Full', N, DIM1, Q( 1, IR ), LDQ,
     $                      DWORK( IWRK4 ), N )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, DWORK( IWRK4 ), N,
     $                      DWORK( IQUPLE ), SDIM, ZERO, Q( 1, IR ),
     $                      LDQ )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, Q( 1, M+IR ), LDQ,
     $                      DWORK( IQLOLE ), SDIM, ONE, Q( 1, IR ),
     $                      LDQ )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, DWORK( IWRK4 ), N,
     $                      DWORK( IQUPRI ), SDIM, ZERO, DWORK( IWRK3 ),
     $                      N )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, Q( 1, M+IR ), LDQ,
     $                      DWORK( IQLORI ), SDIM, ONE, DWORK( IWRK3 ),
     $                      N )
               CALL DLACPY( 'Full', N, DIM1, DWORK( IWRK3 ), N,
     $                      Q( 1, M+IR ), LDQ )
            END IF
         ELSE
            Q11 = DWORK( IQUPLE )
            Q21 = DWORK( IQLOLE )
            Q12 = DWORK( IQUPRI )
            Q22 = DWORK( IQLORI )
C
C           Update A by transformations from the right.
C
            CALL DCOPY( M-1, A( 1, IR ), 1, DWORK( IWRK3 ), 1 )
            CALL DSCAL( M-1, Q11, A( 1, IR ), 1 )
            CALL DAXPY( M-1, Q21, D( 1, IR ), 1, A( 1, IR ), 1 )
C
C           Update D by transformations from the right.
C
            CALL DSCAL( M-1, Q22, D( 1, IR ), 1 )
            CALL DAXPY( M-1, Q12, DWORK( IWRK3 ), 1, D( 1, IR ), 1 )
C
C           Update B by transformations from the right.
C
            CALL DCOPY( M-1, B( 1, IR ), 1, DWORK( IWRK3 ), 1 )
            CALL DSCAL( M-1, Q11, B( 1, IR ), 1 )
            CALL DAXPY( M-1, Q21, F( 1, IR ), 1, B( 1, IR ), 1 )
C
C           Update F by transformations from the right.
C
            CALL DSCAL( M-1, Q22, F( 1, IR ), 1 )
            CALL DAXPY( M-1, Q12, DWORK( IWRK3 ), 1, F( 1, IR ), 1 )
C
C           Update B by transformations from the left.
C
            B( M, M ) = -B( M, M )
C
            IF( LCMPQ ) THEN
C
C              Update Q.
C
               CALL DCOPY( N, Q( 1, IR ), 1, DWORK( IWRK4 ), 1 )
               CALL DSCAL( N, Q11, Q( 1, IR ), 1 )
               CALL DAXPY( N, Q21, Q( 1, IR+M ), 1, Q( 1, IR ), 1 )
               CALL DSCAL( N, Q22, Q( 1, IR+M ), 1 )
               CALL DAXPY( N, Q12, DWORK( IWRK4 ), 1, Q( 1, IR+M ), 1 )
            END IF
C
         END IF
C
         MM = MM + 1
         DO 60 J = R - 1, MM, -1
            IB1  = IWORK( J )
            IB2  = IWORK( J+1 )
            IB3  = IWORK( J+2 )
            DIM1 = IB2  - IB1
            DIM2 = IB3  - IB2
            SDIM = DIM1 + DIM2
C
C           Copy the relevant part of A(ib1:ib3-1,ib1:ib3-1) and
C           B(ib1:ib3-1,ib1:ib3-1) to DWORK as inputs for MB03DD.
C           Also, set the additional zero elements.
C
            CALL DLACPY( 'Upper', SDIM, SDIM, A( IB1, IB1 ), LDA,
     $                   DWORK( IA ), SDIM )
            CALL DLASET( 'Lower', SDIM-1, SDIM-1, ZERO, ZERO,
     $                   DWORK( IA+1 ), SDIM )
            CALL DLACPY( 'Upper', SDIM, SDIM, B( IB1, IB1 ), LDB,
     $                   DWORK( IB ), SDIM )
            CALL DCOPY(  SDIM-1, B( IB1+1, IB1 ), LDB+1, DWORK( IB+1 ),
     $                   SDIM+1 )
            CALL DLASET( 'Lower', SDIM-2, SDIM-2, ZERO, ZERO,
     $                   DWORK( IB+2 ), SDIM )
C
C           Perform eigenvalue/matrix block exchange.
C
            CALL MB03DD( 'Triangular', DIM1, DIM2, PREC, DWORK( IB ),
     $                   SDIM, DWORK( IA ), SDIM, DWORK( IQ1 ), SDIM,
     $                   DWORK( IQ2 ), SDIM, DWORK( IWRK1 ),
     $                   LDWORK-IWRK1+1, INFO )
            IF( INFO.GT.0 ) THEN
               INFO = 1
               RETURN
            END IF
C
C           Copy the transformed diagonal blocks, if sdim > 2.
C
            NROWS = IB1 - 1
            NCOLS = M - IB3 + 1
            ICS   = IB3
            IF( SDIM.GT.2 ) THEN
               CALL DLACPY( 'Upper', SDIM, SDIM, DWORK( IA ), SDIM,
     $                      A( IB1, IB1 ), LDA )
               CALL DLACPY( 'Upper', SDIM, SDIM, DWORK( IB ), SDIM,
     $                      B( IB1, IB1 ), LDB )
               CALL DCOPY(  SDIM-1, DWORK( IB+1 ), SDIM+1,
     $                      B( IB1+1, IB1 ), LDB+1 )
               NROW = NROWS
               NCOL = NCOLS
               IC   = ICS
               LDW  = MAX( 1, NROW )
            ELSE
               TMP = A( IB1+1, IB1 )
               A( IB1+1, IB1 ) = ZERO
               NROW = IB3 - 1
               NCOL = M - IB1 + 1
               IC   = IB1
               LDW  = NROW
            END IF
C
C           Update A.
C
            CALL DGEMM(  'No Transpose', 'No Transpose', NROW, SDIM,
     $                   SDIM, ONE, A( 1, IB1 ), LDA, DWORK( IQ1 ),
     $                   SDIM, ZERO, DWORK( IWRK2 ), LDW )
            CALL DLACPY( 'Full', NROW, SDIM, DWORK( IWRK2 ), LDW,
     $                   A( 1, IB1 ), LDA )
            CALL DGEMM(  'Transpose', 'No Transpose', SDIM, NCOL,
     $                   SDIM, ONE, DWORK( IQ2 ), SDIM, A( IB1, IC ),
     $                   LDA, ZERO, DWORK( IWRK2 ), SDIM )
            CALL DLACPY( 'Full', SDIM, NCOL, DWORK( IWRK2 ), SDIM,
     $                   A( IB1, IC ), LDA )
            IF( SDIM.EQ.2 )
     $         A( IB1+1, IB1 ) = TMP
C
C           Update D.
C
            CALL DGEMM(  'No Transpose', 'No Transpose', NROWS, SDIM,
     $                   SDIM, ONE, D( 1, IB1 ), LDD, DWORK( IQ2 ),
     $                   SDIM, ZERO, DWORK( IWRK2 ), LDW )
            CALL DLACPY( 'Full', NROWS, SDIM, DWORK( IWRK2 ), LDW,
     $                   D( 1, IB1 ), LDD )
            CALL DGEMM(  'Transpose', 'No Transpose', SDIM, NCOLS,
     $                   SDIM, ONE, DWORK( IQ2 ), SDIM,
     $                   D( IB1, ICS ), LDD, ZERO, DWORK( IWRK2 ),
     $                   SDIM )
            CALL DLACPY( 'Full', SDIM, NCOLS, DWORK( IWRK2 ), SDIM,
     $                   D( IB1, ICS ), LDD )
            CALL MB01LD( 'Upper', 'Transpose', SDIM, SDIM, ZERO, ONE,
     $                   D( IB1, IB1 ), LDD, DWORK( IQ2 ), SDIM,
     $                   D( IB1, IB1 ), LDD, DWORK( IWRK2 ),
     $                   LDWORK-IWRK2+1, INFO )
C
C           Update B.
C
            CALL DGEMM(  'No Transpose', 'No Transpose', NROW, SDIM,
     $                   SDIM, ONE, B( 1, IB1 ), LDB, DWORK( IQ1 ),
     $                   SDIM, ZERO, DWORK( IWRK2 ), LDW )
            CALL DLACPY( 'Full', NROW, SDIM, DWORK( IWRK2 ), LDW,
     $                   B( 1, IB1 ), LDB )
            CALL DGEMM(  'Transpose', 'No Transpose', SDIM, NCOL,
     $                   SDIM, ONE, DWORK( IQ2 ), SDIM, B( IB1, IC ),
     $                   LDB, ZERO, DWORK( IWRK2 ), SDIM )
            CALL DLACPY( 'Full', SDIM, NCOL, DWORK( IWRK2 ), SDIM,
     $                   B( IB1, IC ), LDB )
C
C           Update F.
C
            CALL DGEMM(  'No Transpose', 'No Transpose', NROWS, SDIM,
     $                   SDIM, ONE, F( 1, IB1 ), LDF, DWORK( IQ2 ),
     $                   SDIM, ZERO, DWORK( IWRK2 ), LDW )
            CALL DLACPY( 'Full', NROWS, SDIM, DWORK( IWRK2 ), LDW,
     $                   F( 1, IB1 ), LDF )
            CALL DGEMM(  'Transpose', 'No Transpose', SDIM, NCOLS,
     $                   SDIM, ONE, DWORK( IQ2 ), SDIM,
     $                   F( IB1, ICS ), LDF, ZERO, DWORK( IWRK2 ),
     $                   SDIM )
            CALL DLACPY( 'Full', SDIM, NCOLS, DWORK( IWRK2 ), SDIM,
     $                   F( IB1, ICS ), LDF )
            CALL MB01RU( 'Upper', 'Transpose', SDIM, SDIM, ZERO, ONE,
     $                   F( IB1, IB1 ), LDF, DWORK( IQ2 ), SDIM,
     $                   F( IB1, IB1 ), LDF, DWORK( IWRK2 ),
     $                   LDWORK-IWRK2+1, INFO )
            CALL DSCAL( SDIM, HALF, F( IB1, IB1 ), LDF+1 )
C
            IF( LCMPQ ) THEN
C
C              Update Q.
C              Workspace: IWRK2 + 4*N - 1.
C
               CALL DGEMM(  'No Transpose', 'No Transpose', N, SDIM,
     $                      SDIM, ONE, Q( 1, IB1 ), LDQ, DWORK( IQ1 ),
     $                      SDIM, ZERO, DWORK( IWRK2 ), N )
               CALL DLACPY( 'Full', N, SDIM, DWORK( IWRK2 ), N,
     $                      Q( 1, IB1 ), LDQ )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, SDIM,
     $                      SDIM, ONE, Q( 1, M+IB1 ), LDQ, DWORK( IQ2 ),
     $                      SDIM, ZERO, DWORK( IWRK2 ), N )
               CALL DLACPY( 'Full', N, SDIM, DWORK( IWRK2 ), N,
     $                      Q( 1, M+IB1 ), LDQ )
            END IF
C
C           Update index list IWORK(1:M) if a 1-by-1 and 2-by-2 block
C           have been swapped.
C
            HLP = DIM2 - DIM1
            IF( HLP.EQ.1 ) THEN
C
C              First block was 2-by-2.
C
               IWORK( J+1 ) = IB1 + 1
C
            ELSE IF( HLP.EQ.-1 ) THEN
C
C              Second block was 2-by-2.
C
               IWORK( J+1 ) = IB1 + 2
            END IF
   60    CONTINUE
   70 CONTINUE
C
      IF( M.GT.1 ) THEN
C
C        Restore the lower triangle of the submatrix D(M-1:M,M-1:M) and
C        the elements A(M,M-1) and F(M,M-1).
C
         D( M-1, M-1 ) = D1
         D(   M, M-1 ) = D2
         D(   M,   M ) = D3
         A(   M, M-1 ) = A2
         F(   M, M-1 ) = F2
      END IF
C
      IF( MM.GT.0 ) THEN
         NEIG = IWORK( MM+1 ) - 1
      ELSE
         NEIG = 0
      END IF
C
      RETURN
C *** Last line of MB03JD ***
      END
