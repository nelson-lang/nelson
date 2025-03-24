      SUBROUTINE SB03OS( DISCR, LTRANS, N, S, LDS, R, LDR, SCALE, DWORK,
     $                   ZWORK, INFO )
C
C     PURPOSE
C                           H
C     To solve for X = op(U) *op(U) either the stable non-negative
C     definite continuous-time Lyapunov equation
C             H                     2      H
C        op(S) *X + X*op(S) = -scale *op(R) *op(R),                  (1)
C
C     or the convergent non-negative definite discrete-time Lyapunov
C     equation
C             H                     2      H
C        op(S) *X*op(S) - X = -scale *op(R) *op(R),                  (2)
C
C     where op(K) = K or K**H (i.e., the conjugate transpose of the
C     matrix K), S and R are complex N-by-N upper triangular matrices,
C     and scale is an output scale factor, set less than or equal to 1
C     to avoid overflow in X. The diagonal elements of the matrix R must
C     be real non-negative.
C
C     In the case of equation (1) the matrix S must be stable (that is,
C     all the eigenvalues of S must have negative real parts), and for
C     equation (2) the matrix S must be convergent (that is, all the
C     eigenvalues of S must lie inside the unit circle).
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     DISCR   LOGICAL
C             Specifies the type of Lyapunov equation to be solved as
C             follows:
C             = .TRUE. :  Equation (2), discrete-time case;
C             = .FALSE.:  Equation (1), continuous-time case.
C
C     LTRANS  LOGICAL
C             Specifies the form of op(K) to be used, as follows:
C             = .FALSE.:  op(K) = K    (No transpose);
C             = .TRUE. :  op(K) = K**H (Conjugate transpose).
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrices S and R.  N >= 0.
C
C     S       (input) COMPLEX*16 array of dimension (LDS,N)
C             The leading N-by-N upper triangular part of this array
C             must contain the upper triangular matrix.
C             The elements below the upper triangular part of the array
C             S are not referenced.
C
C     LDS     INTEGER
C             The leading dimension of array S.  LDS >= MAX(1,N).
C
C     R       (input/output) COMPLEX*16 array of dimension (LDR,N)
C             On entry, the leading N-by-N upper triangular part of this
C             array must contain the upper triangular matrix R, with
C             real non-negative entries on its main diagonal.
C             On exit, the leading N-by-N upper triangular part of this
C             array contains the upper triangular matrix U, with real
C             non-negative entries on its main diagonal.
C             The strictly lower triangle of R is not referenced.
C
C     LDR     INTEGER
C             The leading dimension of array R.  LDR >= MAX(1,N).
C
C     SCALE   (output) DOUBLE PRECISION
C             The scale factor, scale, set less than or equal to 1 to
C             prevent the solution overflowing.
C
C     Workspace
C
C     DWORK   DOUBLE PRECISION array, dimension (N-1)
C
C     ZWORK   COMPLEX*16 array, dimension (2*N-2)
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value;
C             = 3:  if the matrix S is not stable (that is, one or more
C                   of the eigenvalues of S has a non-negative real
C                   part), if DISCR = .FALSE., or not convergent (that
C                   is, one or more of the eigenvalues of S lies outside
C                   the unit circle), if DISCR = .TRUE..
C
C     METHOD
C
C     The method used by the routine is based on a variant of the
C     Bartels and Stewart backward substitution method [1], that finds
C     the Cholesky factor op(U) directly without first finding X and
C     without the need to form the normal matrix op(R)'*op(R) [2].
C
C     The continuous-time Lyapunov equation in the canonical form
C            H      H              H                     2      H
C       op(S) *op(U) *op(U) + op(U) *op(U)*op(S) = -scale *op(R) *op(R),
C
C     or the discrete-time Lyapunov equation in the canonical form
C            H      H                    H               2      H
C       op(S) *op(U) *op(U)*op(S) - op(U) *op(U) = -scale *op(R) *op(R),
C
C     where U and R are upper triangular, is solved for U.
C
C     REFERENCES
C
C     [1] Bartels, R.H. and Stewart, G.W.
C         Solution of the matrix equation  A'X + XB = C.
C         Comm. A.C.M., 15, pp. 820-826, 1972.
C
C     [2] Hammarling, S.J.
C         Numerical solution of the stable, non-negative definite
C         Lyapunov equation.
C         IMA J. Num. Anal., 2, pp. 303-325, 1982.
C
C     NUMERICAL ASPECTS
C                               3
C     The algorithm requires 0(N ) operations and is backward stable.
C
C     FURTHER COMMENTS
C
C     The Lyapunov equation may be very ill-conditioned. In particular
C     if S is only just stable (or convergent) then the Lyapunov
C     equation will be ill-conditioned. "Large" elements in U relative
C     to those of S and R, or a "small" value for scale, is a symptom
C     of ill-conditioning. A condition estimate can be computed using
C     SLICOT Library routine SB03MD.
C
C     CONTRIBUTOR
C
C     V. Sima, March 2022.
C
C     REVISIONS
C
C     V. Sima, April 2022.
C
C     KEYWORDS
C
C     Lyapunov equation, orthogonal transformation, real Schur form.
C
C     ******************************************************************
C
C     .. Parameters ..
      COMPLEX*16        CZERO, CONE
      PARAMETER         ( CZERO = ( 0.0D0, 0.0D0 ),
     $                    CONE  = ( 1.0D0, 0.0D0 ) )
      DOUBLE PRECISION  ZERO, ONE, TWO
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0 )
C     .. Scalar Arguments ..
      DOUBLE PRECISION  SCALE
      INTEGER           INFO, LDR, LDS, N
      LOGICAL           DISCR, LTRANS
C     .. Array Arguments ..
      COMPLEX*16        R(LDR,*), S(LDS,*), ZWORK(*)
      DOUBLE PRECISION  DWORK(*)
C     .. Local Scalars ..
      COMPLEX*16        ALPHA, SN, TMP, X, Z
      DOUBLE PRECISION  ABSSKK, BIGNUM, C, DR, EPS, SCALOC, SMLNUM,
     $                  SQTWO, TEMP
      INTEGER           I, J, K, K1, KOUNT, KP1, KSZ
      LOGICAL           SLV
C     .. External Functions ..
      DOUBLE PRECISION  DLAMCH
      EXTERNAL          DLAMCH
C     .. External Subroutines ..
      EXTERNAL          DLABAD, XERBLA, ZAXPY, ZCOPY, ZDSCAL, ZLACGV,
     $                  ZLARTG, ZLASCL, ZLATRS, ZROT, ZSCAL, ZSWAP,
     $                  ZTRMV
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, DBLE, DCONJG, MAX, SQRT
C     .. Executable Statements ..
C
      INFO = 0
C
C     Test the input scalar arguments.
C
      IF ( N.LT.0 ) THEN
         INFO = -3
      ELSE IF ( LDS.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF ( LDR.LT.MAX( 1, N ) ) THEN
         INFO = -7
      END IF
C
      IF ( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'SB03OS', -INFO )
         RETURN
      END IF
C
      SCALE = ONE
C
C     Quick return if possible.
C
      IF ( N.EQ.0 )
     $   RETURN
C
C     Set constants to control overflow.
C
      EPS    = DLAMCH( 'P' )
      SMLNUM = DLAMCH( 'S' )
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
      SMLNUM = SMLNUM*DBLE( N*N ) / EPS
      BIGNUM = ONE / SMLNUM
C
      SQTWO  = SQRT( TWO )
C
C     Start the solution. Most of the comments refer to notation and
C     equations in sections 5 and 10 of the second reference above.
C
      IF ( .NOT.LTRANS ) THEN
C
C        Case op(M) = M.
C
         KOUNT = 1
C
   10    CONTINUE
C        WHILE( KOUNT.LE.N )LOOP
         IF ( KOUNT.LE.N ) THEN
            K     = KOUNT
            KP1   = K + 1
            KOUNT = KP1
C
C           Make sure S is stable or convergent and find u11 in
C           equation (5.13) or (10.15).
C
            IF ( DISCR ) THEN
               ABSSKK = ABS( S(K,K) )
               IF ( ABSSKK.GE.ONE ) THEN
                  INFO = 3
                  RETURN
               END IF
               TEMP = SQRT( ONE - ABSSKK )*SQRT( ONE + ABSSKK )
            ELSE
               TEMP = DBLE( S(K,K) )
               IF ( TEMP.GE.ZERO ) THEN
                  INFO = 3
                  RETURN
               END IF
               TEMP = SQTWO*SQRT( -TEMP )
            END IF
C
            SCALOC = ONE
            DR     = DBLE( R(K,K) )
            IF ( TEMP.LT.ONE .AND. DR.GT.ONE ) THEN
               IF ( DR.GT.BIGNUM*TEMP )
     $            SCALOC = ONE / DR
            END IF
            ALPHA  = TEMP
            R(K,K) = R(K,K) / ALPHA
            IF ( SCALOC.NE.ONE ) THEN
               SCALE = SCALE*SCALOC
               CALL ZLASCL( 'Upper', 0, 0, ONE, SCALOC, N, N, R, LDR,
     $                      INFO )
            END IF
C
C           If we are not at the end of  S  then set up and solve
C           equation (5.14) or (10.16).  ksz is the order of the
C           remainder of  S.
C
            IF ( KOUNT.LE.N ) THEN
               KSZ = N - K
               K1  = KSZ + 1
C
C              Form the right-hand side in ZWORK( 1 ),..., ZWORK( n-k ).
C
               Z = DCONJG( S(K,K) )
               CALL ZCOPY( KSZ, R(K,KP1), LDR, ZWORK, 1 )
               CALL ZSCAL( KSZ, -ALPHA, ZWORK, 1 )
               IF ( DISCR ) THEN
                  CALL ZAXPY( KSZ, -Z*R(K,K), S(K,KP1), LDS, ZWORK, 1 )
               ELSE
                  CALL ZAXPY( KSZ,   -R(K,K), S(K,KP1), LDS, ZWORK, 1 )
               END IF
               CALL ZLACGV( KSZ, ZWORK, 1 )
C
C              Form the coefficient matrix.
C
               SLV = .TRUE.
               IF ( DISCR ) THEN
                  IF ( Z.EQ.CZERO ) THEN
                     SLV = .FALSE.
                     CALL ZSCAL( KSZ, -CONE, ZWORK, 1 )
                  ELSE
                     I = 0
C
                     DO 20 J = KP1, N
                        I = I + 1
                        CALL ZSCAL( I, Z, S(KP1,J), 1 )
                        S(J,J) = S(J,J) - CONE
   20                CONTINUE
C
                  END IF
               ELSE
C
                  DO 30 J = KP1, N
                     S(J,J) = S(J,J) + Z
   30             CONTINUE
C
               END IF
C
               IF ( SLV ) THEN
C
C                 Solve the system, with scaling to prevent overflow.
C
                  CALL ZLATRS( 'Upper', 'CTran', 'NoDiag', 'NoNorm',
     $                         KSZ, S(KP1,KP1), LDS, ZWORK, SCALOC,
     $                         DWORK, INFO )
                  IF ( SCALOC.NE.ONE ) THEN
                     SCALE = SCALE*SCALOC
                     CALL ZLASCL( 'Upper', 0, 0, ONE, SCALOC, N, N, R,
     $                            LDR, INFO )
                  END IF
               END IF
C
C              Restore the upper triangle or diagonal of the trailing S.
C              Then, copy the solution into the next ( n - k ) elements
C              of ZWORK, and swap it with the corresponding part of the
C              row of R.
C
               IF ( DISCR ) THEN
                  IF ( Z.NE.CZERO ) THEN
                     Z = CONE / Z
                     I = 0
C
                     DO 40 J = KP1, N
                        S(J,J) = S(J,J) + CONE
                        I = I + 1
                        CALL ZSCAL( I, Z, S(KP1,J), 1 )
   40                CONTINUE
C
                  END IF
                  CALL ZCOPY(  KSZ, ZWORK, 1, ZWORK(K1), 1 )
                  CALL ZLACGV( KSZ, ZWORK, 1 )
               ELSE
C
                  DO 50 J = KP1, N
                     S(J,J) = S(J,J) - Z
   50             CONTINUE
C
                  CALL ZLACGV( KSZ, ZWORK, 1 )
                  CALL ZCOPY(  KSZ, ZWORK, 1, ZWORK(K1), 1 )
               END IF
C
               CALL ZSWAP( KSZ, ZWORK, 1, R(K,KP1), LDR )
C
C              Now form the matrix  Rhat  of equation (5.15) or
C              (10.17), first computing  y  in  ZWORK,  and then
C              updating  R1.
C
               IF ( DISCR ) THEN
C
C                 First form  -lambda( 1 )*r  and then add in
C                 alpha*u11*s.
C
                  CALL ZSCAL(  KSZ, -S(K,K), ZWORK, 1 )
                  CALL ZAXPY(  KSZ, ALPHA*R(K,K), S(K,KP1), LDS, ZWORK,
     $                         1 )
                  CALL ZLACGV( KSZ, ZWORK, 1 )
C
C                 Now form  S1'*u  in ZWORK(K1), where S1 is upper
C                 triangular, and then add  alpha*S1'*u  to ZWORK.
C
                  CALL ZTRMV(  'Upper', 'CTrans', 'NoUnit', KSZ,
     $                         S(KP1,KP1), LDS, ZWORK(K1), 1 )
                  CALL ZAXPY(  KSZ, ALPHA, ZWORK(K1), 1, ZWORK, 1 )
                  CALL ZLACGV( KSZ, ZWORK, 1 )
               ELSE
                  CALL ZAXPY( KSZ, -ALPHA, ZWORK(K1), 1, ZWORK, 1 )
               END IF
C
C              Overwrite R(K+1:N,K+1:N) with the triangular matrix
C              from the QR-factorization of the (N-K+1)-by-(N-K)
C              matrix
C
C                       (  R(KP1:N,KP1:N)  )
C                       (                  ) .
C                       (       Y**H       )
C
               DO 60 I = 1, KSZ
                  X = R(K+I,K+I)
                  Z = ZWORK(I)
                  CALL ZLARTG( X, Z, C, SN, TMP )
                  R(K+I,K+I) = TMP
                  IF ( I.LT.KSZ )
     $               CALL ZROT( KSZ-I, R(K+I,K+I+1), LDR, ZWORK(I+1), 1,
     $                          C, SN )
   60          CONTINUE
C
C              Make main diagonal elements of R(K+1:N,K+1:N) positive.
C
               DO 70 I = KP1, N
                  IF ( DBLE( R(I,I) ).LT.ZERO )
     $               CALL ZDSCAL( N-I+1, -ONE, R(I,I), LDR )
   70          CONTINUE
C
            END IF
            GO TO 10
         END IF
C        END WHILE 10
C
      ELSE
C
C        Case op(M) = M'.
C
         KOUNT = N
C
   80    CONTINUE
C        WHILE( KOUNT.GE.1 )LOOP
         IF ( KOUNT.GE.1 ) THEN
            K     = KOUNT
            KOUNT = KOUNT - 1
C
C           Make sure S is stable or convergent and find u11 in
C           equation corresponding to (5.13) or (10.15).
C
            IF ( DISCR ) THEN
               ABSSKK = ABS( S(K,K) )
               IF ( ABSSKK.GE.ONE ) THEN
                  INFO = 3
                  RETURN
               END IF
               TEMP = SQRT( ONE - ABSSKK )*SQRT( ONE + ABSSKK )
            ELSE
               TEMP = DBLE( S(K,K) )
               IF ( TEMP.GE.ZERO ) THEN
                  INFO = 3
                  RETURN
               END IF
               TEMP = SQTWO*SQRT( -TEMP )
            END IF
C
            SCALOC = ONE
            DR     = DBLE( R(K,K) )
            IF ( TEMP.LT.ONE .AND. DR.GT.ONE ) THEN
               IF ( DR.GT.BIGNUM*TEMP )
     $            SCALOC = ONE / DR
            END IF
            ALPHA  = TEMP
            R(K,K) = R(K,K) / ALPHA
            IF ( SCALOC.NE.ONE ) THEN
               SCALE = SCALE*SCALOC
               CALL ZLASCL( 'Upper', 0, 0, ONE, SCALOC, N, N, R, LDR,
     $                      INFO )
            END IF
C
C           If we are not at the front of  S  then set up and solve
C           equation corresponding to (5.14) or (10.16).  ksz is
C           the order of the remainder leading part of  S.
C
            IF ( KOUNT.GT.0 ) THEN
               KSZ = K - 1
               K1  = KSZ
C
C              Form the right-hand side in ZWORK( 1 ),...,
C              ZWORK( k - 1 ).
C
               Z = DCONJG( S(K,K) )
               CALL ZCOPY( KSZ, R(1,K), 1, ZWORK, 1 )
               CALL ZSCAL( KSZ, -ALPHA, ZWORK, 1 )
               IF ( DISCR ) THEN
                  CALL ZAXPY( KSZ, -Z*R(K,K), S(1,K), 1, ZWORK, 1 )
               ELSE
                  CALL ZAXPY( KSZ,   -R(K,K), S(1,K), 1, ZWORK, 1 )
               END IF
C
C              Form the coefficient matrix.
C
               SLV = .TRUE.
               IF ( DISCR ) THEN
                  IF ( Z.EQ.CZERO ) THEN
                     SLV = .FALSE.
                     CALL ZSCAL( KSZ, -CONE, ZWORK, 1 )
                  ELSE
C
                     DO 90 J = 1, K1
                        CALL ZSCAL( J, Z, S(1,J), 1 )
                        S(J,J) = S(J,J) - CONE
   90                CONTINUE
C
                  END IF
               ELSE
C
                  DO 100 J = 1, K1
                     S(J,J) = S(J,J) + Z
  100             CONTINUE
C
               END IF
C
               IF ( SLV ) THEN
C
C                 Solve the system, with scaling to prevent overflow.
C
                  CALL ZLATRS( 'Upper', 'NoTran', 'NoDiag', 'NoNorm',
     $                         KSZ, S, LDS, ZWORK, SCALOC, DWORK, INFO )
                  IF ( SCALOC.NE.ONE ) THEN
                     SCALE = SCALE*SCALOC
                     CALL ZLASCL( 'Upper', 0, 0, ONE, SCALOC, N, N, R,
     $                            LDR, INFO )
                  END IF
               END IF
C
C              Restore the upper triangle or diagonal of the leading S.
C              Then, copy the solution into the next ( k - 1 ) elements
C              of ZWORK, and swap it with the corresponding part of the
C              row of R.
C
               IF ( DISCR ) THEN
                  IF ( Z.NE.CZERO ) THEN
                     Z = CONE / Z
C
                     DO 110 J = 1, K1
                        S(J,J) = S(J,J) + CONE
                        CALL ZSCAL( J, Z, S(1,J), 1 )
  110                CONTINUE
C
                  END IF
               ELSE
C
                  DO 120 J = 1, K1
                     S(J,J) = S(J,J) - Z
  120             CONTINUE
C
               END IF
C
               CALL ZCOPY( KSZ, ZWORK, 1, ZWORK(K), 1 )
               CALL ZSWAP( KSZ, ZWORK, 1, R(1,K), 1 )
C
C              Now form the matrix  Rhat  of equation corresponding
C              to (5.15) or (10.17), first computing  y  in  ZWORK,
C              and then updating  R1.
C
               IF ( DISCR ) THEN
C
C                 First form  -lambda( 1 )*r  and then add in
C                 alpha*u11*s.
C
                  CALL ZSCAL( KSZ, -S(K,K), ZWORK, 1 )
                  CALL ZAXPY( KSZ, ALPHA*R(K,K), S(1,K), 1, ZWORK, 1 )
C
C                 Now form  alpha*S1*u  in ZWORK(K), where  S1  is upper
C                 triangular.
C
                  CALL ZTRMV( 'Upper', 'NoTran', 'NoUnit', KSZ, S, LDS,
     $                        ZWORK(K), 1 )
                  CALL ZAXPY( KSZ,  ALPHA, ZWORK(K), 1, ZWORK, 1 )
               ELSE
                  CALL ZAXPY( KSZ, -ALPHA, ZWORK(K), 1, ZWORK, 1 )
               END IF
C
C              Overwrite  R(1:K-1,1:K-1)  with the triangular matrix
C              from the RQ-factorization of the  (K-1)-by-K  matrix
C
C                       (                      )
C                       (  R(1:K-1,1:K-1)   Y  ) .
C                       (                      )
C
               DO 130 I = KSZ, 1, -1
                  X = R(I,I)
                  Z = DCONJG( ZWORK(I) )
                  CALL ZLARTG( X, Z, C, SN, TMP )
                  R(I,I) = TMP
                  IF ( I.GT.1 )
     $               CALL ZROT( I-1, R(1,I), 1, ZWORK, 1,  C,
     $                          DCONJG( SN ) )
  130          CONTINUE
C
C              Make main diagonal elements of  R(1:K-1,1:K-1)  positive.
C
               DO 140 I = 1, KSZ
                  IF ( DBLE( R(I,I) ).LT.ZERO )
     $               CALL ZDSCAL( I, -ONE, R(1,I), 1 )
  140          CONTINUE
            END IF
            GO TO 80
         END IF
C        END WHILE 80
C
      END IF
      RETURN
C *** Last line of SB03OS ***
      END
