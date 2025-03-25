      SUBROUTINE SG03BS( TRANS, N, A, LDA, E, LDE, B, LDB, SCALE, DWORK,
     $                   ZWORK, INFO )
C
C     PURPOSE
C
C     To compute the Cholesky factor U of the matrix X, X = U**H * U or
C     X = U * U**H, which is the solution of the generalized d-stable
C     discrete-time Lyapunov equation
C
C         H            H                  2    H
C        A  * X * A - E  * X * E = - SCALE  * B  * B,                (1)
C
C     or the conjugate transposed equation
C
C                 H            H          2        H
C        A * X * A  - E * X * E  = - SCALE  * B * B ,                (2)
C
C     respectively, where A, E, B, and U are complex N-by-N matrices.
C     The Cholesky factor U of the solution is computed without first
C     finding X. The pencil A - lambda * E must be in complex
C     generalized Schur form (A and E are upper triangular and the
C     diagonal elements of E are non-negative real numbers). Moreover,
C     it must be d-stable, i.e., the moduli of its eigenvalues must be
C     less than one. B must be an upper triangular matrix with real
C     non-negative entries on its main diagonal.
C
C     The resulting matrix U is upper triangular. The entries on its
C     main diagonal are non-negative. SCALE is an output scale factor
C     set to avoid overflow in U.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     TRANS   CHARACTER*1
C             Specifies whether equation (1) or equation (2) is to be
C             solved:
C             = 'N':  Solve equation (1);
C             = 'C':  Solve equation (2).
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrices.  N >= 0.
C
C     A       (input/workspace) COMPLEX*16 array, dimension (LDA,N)
C             The leading N-by-N upper triangular part of this array
C             must contain the triangular matrix A. The lower triangular
C             part is used as workspace, but the diagonal is restored.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     E       (input/workspace) COMPLEX*16 array, dimension (LDE,N)
C             The leading N-by-N upper triangular part of this array
C             must contain the triangular matrix E. If TRANS = 'N', the
C             strictly lower triangular part is used as workspace.
C
C     LDE     INTEGER
C             The leading dimension of the array E.  LDE >= MAX(1,N).
C
C     B       (input/output) COMPLEX*16 array, dimension (LDB,N)
C             On entry, the leading N-by-N upper triangular part of this
C             array must contain the matrix B.
C             On exit, the leading N-by-N upper triangular part of this
C             array contains the solution matrix U.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= MAX(1,N).
C
C     SCALE   (output) DOUBLE PRECISION
C             The scale factor set to avoid overflow in U.
C             0 < SCALE <= 1.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension LDWORK, where
C             LDWORK = 0,            if N <= 1;
C             LDWORK = MAX(N-1,10),  if N >  1.
C
C     ZWORK   COMPLEX*16, dimension MAX(3*N-3,0)
C
C     Error indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value;
C             = 3:  the pencil A - lambda * E is not stable, i.e., there
C                   there are eigenvalues outside the open unit circle;
C             = 4:  the LAPACK routine ZSTEIN utilized to factorize M3
C                   failed to converge. This error is unlikely to occur.
C
C     METHOD
C
C     The method used by the routine is an extension of Hammarling's
C     algorithm [1] to generalized Lyapunov equations. The real case is
C     described in [2].
C
C     We present the method for solving equation (1). Equation (2) can
C     be treated in a similar fashion. For simplicity, assume SCALE = 1.
C
C     Since all matrices A, E, B, and U are upper triangular, we use the
C     following partitioning
C
C               ( A11   A12 )        ( E11   E12 )
C           A = (           ),   E = (           ),
C               (   0   A22 )        (   0   E22 )
C
C               ( B11   B12 )        ( U11   U12 )
C           B = (           ),   U = (           ),                  (3)
C               (   0   B22 )        (   0   U22 )
C
C     where the size of the (1,1)-blocks is 1-by-1.
C
C     We compute U11, U12**H and U22 in three steps.
C
C     Step I:
C
C        From (1) and (3) we get the 1-by-1 equation
C
C              H     H                 H     H                  H
C           A11 * U11 * U11 * A11 - E11 * U11 * U11 * E11 = -B11 * B11.
C
C        For brevity, details are omitted here. The technique for
C        computing U11 is similar to those applied to standard Lyapunov
C        equations in Hammarling's algorithm ([1], section 5).
C
C        Furthermore, the auxiliary scalars M1 and M2 defined as follows
C
C           M1 = A11 / E11 ,   M2 = B11 / E11 / U11 ,
C
C        are computed in a numerically reliable way.
C
C     Step II:
C
C        We solve for U12**H the linear system of equations, with
C        scaling to prevent overflow,
C
C                     H      H      H
C           ( M1 * A22  - E22  ) U12  =
C
C                       H              H           H
C           = - M2 * B12  + U11 * ( E12  - M1 * A12  ) .
C
C     Step III:
C
C        One can show that
C
C              H      H                  H      H
C           A22  * U22  * U22 * A22 - E22  * U22  * U22 * E22  =
C
C                H              H
C           - B22  * B22 - y * y                                     (4)
C
C        holds, where y is defined as follows
C
C                    H           H      H      H
C           y = ( B12   U11 * A12  + A22  * U12  ) * M3EV,
C
C        where M3EV is a matrix which fulfils
C
C                ( I - M2*M2     -M2*M1**H )              H
C           M3 = (                         ) = M3EV * M3EV .
C                (    -M1*M2  I - M1*M1**H )
C
C        M3 is positive semidefinite and its rank is equal to 1.
C        Therefore, a matrix M3EV can be found by solving the Hermitian
C        eigenvalue problem for M3 such that y consists of one column.
C
C        If B22_tilde is the square triangular matrix arising from the
C        QR-factorization
C
C               ( B22_tilde )     ( B22  )
C           Q * (           )  =  (      ),
C               (     0     )     ( y**H )
C
C        then
C
C                H              H                H
C           - B22  * B22 - y * y   =  - B22_tilde  * B22_tilde.
C
C        Replacing the right hand side in (4) by the term
C        - B22_tilde**H * B22_tilde leads to a generalized Lyapunov
C        equation like (1), but of dimension N-1.
C
C     The solution U of the equation (1) can be obtained by recursive
C     application of the steps I to III.
C
C     REFERENCES
C
C     [1] Hammarling, S.J.
C         Numerical solution of the stable, non-negative definite
C         Lyapunov equation.
C         IMA J. Num. Anal., 2, pp. 303-323, 1982.
C
C     [2] Penzl, T.
C         Numerical solution of generalized Lyapunov equations.
C         Advances in Comp. Math., vol. 8, pp. 33-48, 1998.
C
C     NUMERICAL ASPECTS
C
C     The routine requires 2*N**3 flops. Note that we count a single
C     floating point arithmetic operation as one flop.
C
C     FURTHER COMMENTS
C
C     The Lyapunov equation may be very ill-conditioned. In particular,
C     if the pencil A - lambda * E has a pair of almost reciprocal
C     eigenvalues, then the Lyapunov equation will be ill-conditioned.
C     Perturbed values were used to solve the equation.
C     A condition estimate can be obtained from the routine SG03AD.
C
C     CONTRIBUTOR
C
C     V. Sima, June 2021.
C
C     REVISIONS
C
C     V. Sima, July 2021, Oct. 2021, Nov. 2021.
C
C     KEYWORDS
C
C     Lyapunov equation
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  MONE, ONE, ZERO
      PARAMETER         ( MONE = -1.0D+0, ONE = 1.0D+0, ZERO = 0.0D+0 )
      COMPLEX*16        CONE
      PARAMETER         ( CONE = ( 1.0D+0, 0.0D+0 ) )
C     .. Scalar Arguments ..
      CHARACTER         TRANS
      DOUBLE PRECISION  SCALE
      INTEGER           INFO, LDA, LDB, LDE, N
C     .. Array Arguments ..
      DOUBLE PRECISION  DWORK(*)
      COMPLEX*16        A(LDA,*), B(LDB,*), E(LDE,*), ZWORK(*)
C     .. Local Scalars ..
      COMPLEX*16        M1, R, S, X, Z
      DOUBLE PRECISION  BIGNUM, C, DELTA1, EPS, M2, SCALE1, SMLNUM, T,
     $                  UII
      INTEGER           APT, I, J, KL, KL1, UPT, WPT
      LOGICAL           NOTRNS
C     .. Local Arrays ..
      COMPLEX*16        M3(2,2), M3C(2,1)
      DOUBLE PRECISION  D(2), ES(2), W(2)
      INTEGER           IWORK(7)
C     .. External Functions ..
      DOUBLE PRECISION  DLAMCH
      LOGICAL           LSAME
      EXTERNAL          DLAMCH, LSAME
C     .. External Subroutines ..
      EXTERNAL          DLABAD, MA02EZ, XERBLA, ZAXPY, ZCOPY, ZDSCAL,
     $                  ZLACGV, ZLARFG, ZLARTG, ZLASCL, ZLATRS, ZROT,
     $                  ZSCAL, ZSTEIN, ZTRMV
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, DBLE, DCMPLX, DCONJG, MAX, SQRT
C     .. Executable Statements ..
C
C     Decode input parameter.
C
      NOTRNS = LSAME( TRANS, 'N' )
C
C     Check the scalar input parameters.
C
      IF ( .NOT.( NOTRNS .OR. LSAME( TRANS, 'C' ) ) ) THEN
         INFO = -1
      ELSEIF ( N.LT.0 ) THEN
         INFO = -2
      ELSEIF ( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -4
      ELSEIF ( LDE.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSEIF ( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -8
      ELSE
         INFO = 0
      END IF
      IF ( INFO.NE.0 ) THEN
         CALL XERBLA( 'SG03BS', -INFO )
         RETURN
      END IF
C
      SCALE = ONE
C
C     Quick return if possible.
C
      IF ( N.EQ.0 )
     $    RETURN
C
C     Set constants to control overflow.
C
      EPS    = DLAMCH( 'P' )
      SMLNUM = DLAMCH( 'S' )/EPS
      BIGNUM = ONE/SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
C
C     Set workspace pointers.
C
      UPT = 1
      WPT = N
      APT = 2*N - 1
C
C     Set constant input for ZSTEIN.
C
      IWORK(2) = 1
      IWORK(3) = 0
      IWORK(4) = 2
      IWORK(5) = 0
      W(1)     = ONE
      W(2)     = ZERO
C
      IF ( NOTRNS ) THEN
C
C        Solve equation (1).
C
C        Store the last N-1 diagonal elements of A.
C        Fill-in the strictly lower triangular part of E with the
C        conjugate transpose of the strictly upper triangular part.
C
         IF ( N.GT.1 )
     $      CALL ZCOPY( N-1, A(2,2), LDA+1, ZWORK(APT), 1 )
         CALL MA02EZ( 'Upper', 'Conj', 'NoSkew', N, E, LDE )
C
C        Main Loop. Compute the row elements U(KL,KL:N).
C
         DO 60 KL = 1, N
C
C           STEP I: Compute U(KL,KL) and the auxiliary scalars M1 and
C                   M2. (For the moment the result U(KL,KL) is stored
C                   in UII).
C
            DELTA1 = DBLE( E(KL,KL) )
            T      =  ABS( A(KL,KL) )
            M2     =  MAX( DELTA1, T )
            DELTA1 = DELTA1/M2
            T      =      T/M2
            IF ( DELTA1.LE.T ) THEN
               INFO = 3
               RETURN
            END IF
            DELTA1 = SQRT( ONE - T )*SQRT( ONE + T )*M2
            T = DBLE( B(KL,KL) )*SMLNUM
            IF ( T.GT.DELTA1 ) THEN
               SCALE1 = DELTA1/T
               SCALE  = SCALE1*SCALE
               CALL ZLASCL( 'Upper', 0, 0, ONE, SCALE1, N, N, B, LDB,
     $                      INFO )
            END IF
C
            UII = DBLE( B(KL,KL) )/DELTA1
C
            IF ( KL.LT.N ) THEN
C
               M1 = A(KL,KL)/DBLE( E(KL,KL) )
               M2 =   DELTA1/DBLE( E(KL,KL) )
C
C              STEP II: Compute U(KL,KL+1:N) by solving a linear system
C                       of equations. (For the moment the result is
C                       stored in the workspace.)
C
C              Fill-in the lower triangular part of A22 with the
C              conjugate transpose of the upper triangular part.
C
               CALL MA02EZ( 'Upper', 'Conj', 'General', N-KL+1,
     $                      A(KL,KL), LDA )
C
C              Form right hand side of the system of equations.
C
               KL1 = KL + 1
               CALL ZCOPY( N-KL, E(KL1,KL), 1, ZWORK(UPT), 1 )
               CALL ZAXPY( N-KL, -M1, A(KL1,KL), 1, ZWORK(UPT), 1 )
               I = UPT
C
               DO 10 J = KL1, N
                  ZWORK(I) = DCMPLX( UII )*ZWORK(I) -
     $                       DCMPLX( M2  )*DCONJG( B(KL,J) )
                  I = I + 1
   10          CONTINUE
C
C              Form the coefficient matrix.
C
               DO 30 J = KL1, N
                  DO 20 I = J, N
                     A(I,J) = M1*A(I,J) - E(I,J)
   20             CONTINUE
   30          CONTINUE
C
C              Solve the system, with scaling to prevent overflow.
C
               CALL ZLATRS( 'Lower', 'NoConj', 'NoDiag', 'NoNorm', N-KL,
     $                      A(KL1,KL1), LDA, ZWORK(UPT), SCALE1, DWORK,
     $                      INFO )
               IF ( SCALE1.NE.ONE ) THEN
                  SCALE = SCALE1*SCALE
                  UII   = SCALE1*UII
                  CALL ZLASCL( 'Upper', 0, 0, ONE, SCALE1, N, N, B, LDB,
     $                         INFO )
               END IF
C
C              Restore the diagonal of A22.
C
               A(KL,KL) = DCONJG( A(KL,KL) )
               CALL ZCOPY( N-KL, ZWORK(APT+KL-1), 1, A(KL1,KL1), LDA+1 )
C
C              STEP III: Form the right hand side matrix
C                        B(KL+1:N,KL+1:N) of the (smaller) Lyapunov
C                        equation to be solved during the next pass of
C                        the main loop.
C
C              Compute auxiliary matrices M3 and Y. The factorization
C              M3 = M3C * M3C**H is found by solving the special
C              symmetric eigenvalue problem. (D is the diagonal of M3.)
C
               M3(1,2) = -M2*DCONJG( M1 )
C
               X = M3(1,2)
               CALL ZLARFG( 1, X, M3(1,2), 1, Z )
               D(1)  = ABS( M1 )**2
               D(2)  = M2**2
               ES(1) = DBLE( X )
C
               CALL ZSTEIN( 2, D, ES, 1, W, IWORK(2), IWORK(4), M3C, 2,
     $                      DWORK, IWORK(6), IWORK, INFO )
               IF ( INFO.NE.0 ) THEN
                  INFO = 4
                  RETURN
               END IF
               M3C(1,1) = ( CONE - Z )*M3C(1,1)
C
C              Compute auxiliary vector Y in ZWORK(WPT).
C
               CALL ZCOPY(  N-KL, ZWORK(UPT), 1, ZWORK(WPT), 1 )
               CALL ZTRMV(  'Upper', 'Conj', 'NonUnit', N-KL,
     $                      A(KL1,KL1), LDA, ZWORK(WPT), 1 )
               CALL ZAXPY(  N-KL, DCMPLX( UII ), A(KL1,KL), 1,
     $                      ZWORK(WPT), 1 )
               CALL ZSCAL(  N-KL, M3C(2,1), ZWORK(WPT), 1 )
               CALL ZLACGV( N-KL, ZWORK(WPT), 1 )
               CALL ZAXPY(  N-KL, DCONJG( M3C(1,1) ), B(KL,KL1), LDB,
     $                      ZWORK(WPT), 1 )
C
C              Overwrite B(KL+1:N,KL+1:N) with the triangular matrix
C              from the QR-factorization of the (N-KL+1)-by-(N-KL)
C              matrix
C
C                       (  B(KL+1:N,KL+1:N)  )
C                       (                    ) .
C                       (       Y**H         )
C
               DO 40 I = 1, N-KL
                  X = B(KL+I,KL+I)
                  Z = ZWORK(WPT+I-1)
                  CALL ZLARTG( X, Z, C, S, R )
                  B(KL+I,KL+I) = R
                  IF ( I.LT.N-KL )
     $               CALL ZROT(  N-KL-I, B(KL+I,KL1+I), LDB,
     $                           ZWORK(WPT+I), 1, C, S )
   40          CONTINUE
C
C              Make main diagonal elements of B(KL+1:N,KL+1:N) positive.
C
               DO 50 I = KL1, N
                  IF ( DBLE( B(I,I) ).LT.ZERO )
     $               CALL ZDSCAL( N-I+1, MONE, B(I,I), LDB )
   50          CONTINUE
C
C              Overwrite right hand side with the part of the solution
C              computed in step II.
C
               CALL ZLACGV( N-KL, ZWORK(UPT), 1 )
               CALL ZCOPY(  N-KL, ZWORK(UPT), 1, B(KL,KL1), LDB )
C
            END IF
C
C           Overwrite right hand side with the part of the solution
C           computed in step I.
C
            B(KL,KL) = UII
C
   60    CONTINUE
C
      ELSE
C
C        Solve equation (2).
C
C        Store the first N-1 diagonal elements of A.
C
         IF ( N.GT.1 )
     $      CALL ZCOPY( N-1, A, LDA+1, ZWORK(APT), 1 )
C
C        Main Loop. Compute the column elements U(1:KL,KL).
C
         DO 110 KL = N, 1, -1
C
C           STEP I: Compute U(KL,KL) and the auxiliary scalars M1 and
C                   M2. (For the moment the result U(KL,KL) is stored
C                   in UII).
C
            DELTA1 = DBLE( E(KL,KL) )
            T      =  ABS( A(KL,KL) )
            M2     =  MAX( DELTA1, T )
            DELTA1 = DELTA1/M2
            T      =      T/M2
            IF ( DELTA1.LE.T ) THEN
               INFO = 3
               RETURN
            END IF
            DELTA1 = SQRT( ONE - T )*SQRT( ONE + T )*M2
            T = DBLE( B(KL,KL) )*SMLNUM
            IF ( T.GT.DELTA1 ) THEN
               SCALE1 = DELTA1/T
               SCALE  = SCALE1*SCALE
               CALL ZLASCL( 'Upper', 0, 0, ONE, SCALE1, N, N, B, LDB,
     $                      INFO )
            END IF
C
            UII = DBLE( B(KL,KL) )/DELTA1
C
            IF ( KL.GT.1 ) THEN
C
               M1 = DCONJG( A(KL,KL) )/DBLE( E(KL,KL) )
               M2 =             DELTA1/DBLE( E(KL,KL) )
C
C              STEP II: Compute U(1:KL,KL) by solving a linear system
C                       of equations. (For the moment the result is
C                       stored in the workspace.)
C
C              Fill-in the strictly lower triangular part of A22 with
C              the transpose of the strictly upper triangular part.
C
               KL1 = KL - 1
               CALL MA02EZ( 'Upper', 'Trans', 'General', KL1, A, LDA )
C
C              Form right hand side of the system of equations.
C
               CALL ZCOPY(  KL1, E(1,KL), 1, ZWORK(UPT), 1 )
               CALL ZAXPY(  KL1, -M1, A(1,KL), 1, ZWORK(UPT), 1 )
               CALL ZDSCAL( KL1, UII, ZWORK(UPT), 1 )
               CALL ZAXPY(  KL1, -DCMPLX( M2 ), B(1,KL), 1, ZWORK(UPT),
     $                      1 )
C
C              Form the coefficient matrix.
C
               DO 80 J = 1, KL1
                  DO 70 I = 1, J
                     A(I,J) = M1*A(I,J) - E(I,J)
   70             CONTINUE
   80          CONTINUE
C
C              Solve the system, with scaling to prevent overflow.
C
               CALL ZLATRS( 'Upper', 'NoConj', 'NoDiag', 'NoNorm', KL1,
     $                      A, LDA, ZWORK(UPT), SCALE1, DWORK, INFO )
               IF ( SCALE1.NE.ONE ) THEN
                  SCALE = SCALE1*SCALE
                  UII   = SCALE1*UII
                  CALL ZLASCL( 'Upper', 0, 0, ONE, SCALE1, N, N, B, LDB,
     $                         INFO )
               END IF
C
C              Restore the upper triangular part of A22.
C
               CALL MA02EZ( 'Lower', 'Trans', 'General', KL1, A, LDA )
               CALL ZCOPY(  KL1, ZWORK(APT), 1, A, LDA+1 )
C
C              STEP III: Form the right hand side matrix
C                        B(1:KL-1,1:KL-1) of the (smaller) Lyapunov
C                        equation to be solved during the next pass of
C                        the main loop.
C
C              Compute auxiliary matrices M3 and Y. The factorization
C              M3 = M3C * M3C**H is found by solving the special
C              symmetric eigenvalue problem. (D is the diagonal of M3.)
C
               M3(1,2) = -M2*DCONJG( M1 )
C
               X = M3(1,2)
               CALL ZLARFG( 1, X, M3(1,2), 1, Z )
               D(1)  = ABS( M1 )**2
               D(2)  = M2**2
               ES(1) = DBLE( X )
C
               CALL ZSTEIN( 2, D, ES, 1, W, IWORK(2), IWORK(4), M3C, 2,
     $                      DWORK, IWORK(6), IWORK, INFO )
               IF ( INFO.NE.0 ) THEN
                  INFO = 4
                  RETURN
               END IF
               M3C(1,1) = ( CONE - Z )*M3C(1,1)
C
C              Compute auxiliary vector Y in B(1:KL,KL).
C
               CALL ZSCAL(  KL1, M3C(1,1), B(1,KL), 1 )
               CALL ZCOPY(  KL1, ZWORK(UPT), 1, ZWORK(WPT), 1 )
               CALL ZTRMV(  'Upper', 'NoTrans', 'NonUnit', KL1, A, LDA,
     $                      ZWORK(WPT), 1 )
               CALL ZAXPY(  KL1, DCMPLX( UII ), A(1,KL), 1, ZWORK(WPT),
     $                      1 )
               CALL ZAXPY(  KL1, M3C(2,1), ZWORK(WPT), 1, B(1,KL), 1 )
C
C              Overwrite B(1:KL-1,1:KL-1) with the triangular matrix
C              from the RQ-factorization of the (KL-1)-by-KL matrix
C
C                       (                        )
C                       (  B(1:KL-1,1:KL-1)   Y  ) .
C                       (                        )
C
               DO 90 I = KL1, 1, -1
                  X = B(I,I)
                  Z = B(I,KL)
                  CALL ZLARTG( X, Z, C, S, R )
                  B(I,I) = R
                  IF ( I.GT.1 )
     $               CALL ZROT( I-1, B(1,I), 1, B(1,KL), 1,  C, S )
   90          CONTINUE
C
C              Make main diagonal elements of B(1:KL-1,1:KL-1) positive.
C
               DO 100 I = 1, KL1
                  IF ( DBLE( B(I,I) ).LT.ZERO )
     $               CALL ZDSCAL( I, MONE, B(1,I), 1 )
  100          CONTINUE
C
C              Overwrite right hand side with the part of the solution
C              computed in step II.
C
               CALL ZCOPY( KL1, ZWORK(UPT), 1, B(1,KL), 1 )
C
            END IF
C
C           Overwrite right hand side with the part of the solution
C           computed in step I.
C
            B(KL,KL) = UII
C
  110    CONTINUE
C
      END IF
C
      RETURN
C *** Last line of SG03BS ***
      END
