      SUBROUTINE MB03AH( SHFT, K, N, AMAP, S, SINV, A, LDA1, LDA2, C1,
     $                   S1, C2, S2 )
C
C     PURPOSE
C
C     To compute two Givens rotations (C1,S1) and (C2,S2) such that the
C     orthogonal matrix
C
C               [ Q  0 ]        [  C1  S1  0 ]   [ 1  0   0  ]
C           Z = [      ],  Q := [ -S1  C1  0 ] * [ 0  C2  S2 ],
C               [ 0  I ]        [  0   0   1 ]   [ 0 -S2  C2 ]
C
C     makes the first column of the real Wilkinson double shift
C     polynomial of the product of matrices in periodic upper Hessenberg
C     form, stored in the array A, parallel to the first unit vector.
C     Only the rotation defined by C1 and S1 is used for the real
C     Wilkinson single shift polynomial (see SLICOT Library routines
C     MB03BE or MB03BF). All factors whose exponents differ from that of 
C     the Hessenberg factor are assumed nonsingular. The trailing 2-by-2
C     submatrix and the five nonzero elements in the first two columns
C     of the matrix product are evaluated when a double shift is used.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     SHFT    CHARACTER*1
C             Specifies the number of shifts employed by the shift
C             polynomial, as follows:
C             = 'D':  two shifts (assumes N > 2);
C             = 'S':  one real shift.
C
C     Input/Output Parameters
C
C     K       (input)  INTEGER
C             The number of factors.  K >= 1.
C
C     N       (input)  INTEGER
C             The order of the factors.  N >= 2.
C
C     AMAP    (input)  INTEGER array, dimension (K)
C             The map for accessing the factors, i.e., if AMAP(I) = J,
C             then the factor A_I is stored at the J-th position in A.
C             AMAP(K) is the pointer to the Hessenberg matrix.
C             Before calling this routine, AMAP returned by SLICOT
C             Library routine MB03BA should be modified as follows:
C             J = AMAP(1), AMAP(I) = AMAP(I+1), I = 1:K-1, AMAP(K) = J.
C
C     S       (input)  INTEGER array, dimension (K)
C             The signature array. Each entry of S must be 1 or -1.
C
C     SINV    (input)  INTEGER
C             Signature multiplier. Entries of S are virtually
C             multiplied by SINV.
C
C     A       (input)  DOUBLE PRECISION array, dimension (LDA1,LDA2,K)
C             The leading N-by-N-by-K part of this array must contain
C             the product (implicitly represented by its K factors)
C             in periodic upper Hessenberg form.
C
C     LDA1    INTEGER
C             The first leading dimension of the array A.  LDA1 >= N.
C
C     LDA2    INTEGER
C             The second leading dimension of the array A.  LDA2 >= N.
C
C     C1      (output)  DOUBLE PRECISION
C     S1      (output)  DOUBLE PRECISION
C             On exit, C1 and S1 contain the parameters for the first
C             Givens rotation.
C
C     C2      (output)  DOUBLE PRECISION
C     S2      (output)  DOUBLE PRECISION
C             On exit, if SHFT = 'D' and N > 2, C2 and S2 contain the
C             parameters for the second Givens rotation. Otherwise,
C             C2 = 1, S2 = 0.
C
C     METHOD
C
C     The necessary elements of the real Wilkinson double/single shift
C     polynomial are computed, and suitable Givens rotations are found.
C     For numerical reasons, this routine should be called when
C     convergence difficulties are encountered. For a double shift, if
C     there are two real eigenvalues of the trailing 2-by-2 part of the
C     product, both shifts are chosen equal to the eigenvalue with
C     minimum modulus. The trailing element of the product is used as a
C     single shift. If SINV is negative, the shift(s) correspond to the
C     reciprocals of the eigenvalues of the product, as required by the
C     SLICOT Library routine MB03BD.
C
C     CONTRIBUTOR
C
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     Aug. 2019, Dec. 2020.
C
C     REVISIONS
C
C     V. Sima, Sep. 2019, Dec. 2019, Jan. 2020, Feb. 2020, Mar. 2020.
C
C     KEYWORDS
C
C     Eigenvalues, QZ algorithm, periodic QZ algorithm, orthogonal
C     transformation.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ONE, TWO, ZERO
      PARAMETER         ( ONE = 1.0D0, TWO = 2.0D0, ZERO = 0.0D0 )
C     .. Scalar Arguments ..
      CHARACTER         SHFT
      INTEGER           K, LDA1, LDA2, N, SINV
      DOUBLE PRECISION  C1, C2, S1, S2
C     .. Array Arguments ..
      INTEGER           AMAP(*), S(*)
      DOUBLE PRECISION  A(LDA1,LDA2,*)
C     .. Local Scalars ..
      LOGICAL           SGLE
      INTEGER           I, IC, IND, J, M, MM
      DOUBLE PRECISION  E1, E2, P1, P2, P3, PR, SCL, SM, T
C     .. Local Arrays ..
      INTEGER           IP(3), JP(3)
      DOUBLE PRECISION  DWORK(9), WI(2), WR(2), Y(9), Z(2,2)
C     .. External Functions ..
      LOGICAL           LSAME
      EXTERNAL          LSAME
C     .. External Subroutines ..
      EXTERNAL          DCOPY, DGESC2, DGETC2, DLACPY, DLANV2, DLARTG,
     $                  DLASET, DTRMV
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, MIN
C
C     .. Executable Statements ..
C
C     For efficiency reasons, the parameters are not checked for errors.
C
C     Evaluate the needed part of the matrix product.
C
      SGLE = LSAME( SHFT, 'S' ) .OR. N.EQ.2
C
      M  = MIN( N, 3 )
      MM = M*M
C
      CALL DLASET( 'Full', M, M, ZERO, ONE, DWORK, M )
C
      DO 30  J = K - 1, 1, -1
         I = AMAP(J)
         IF ( S(I).EQ.SINV ) THEN
C
            DO 10  IC = 1, MM, M
               CALL DTRMV( 'Upper', 'NoTran', 'NonUnit', M, A(1,1,I),
     $                     LDA1, DWORK(IC), 1 )
   10       CONTINUE
C
         ELSE
C
C           Complete pivoting is used for triangular factors whose
C           exponents differ from SINV. It is assumed that no overflow
C           could appear when solving linear systems, hence SCL = 1.
C
            CALL DLACPY( 'Upper', M, M, A(1,1,I), LDA1, Y, M )
            CALL DLASET( 'Lower', M-1, M-1, ZERO, ZERO, Y(2), M )
            CALL DGETC2( M, Y, M, IP, JP, IND )
C
            DO 20  IC = 1, MM, M
               CALL DGESC2( M, Y, M, DWORK(IC), IP, JP, SCL )
   20       CONTINUE
C
         END IF
   30 CONTINUE
C
C     Compute successively in Y(1:3) the nonzero elements of the first
C     two columns of the product, and save the results.
C
      I = AMAP(K)
      CALL DCOPY( 2, A(1,1,I), 1, Y, 1 )
      CALL DTRMV( 'Upper', 'NoTran', 'NonUnit', 2, DWORK, M, Y, 1 )
      E1 = Y(1)
      E2 = Y(2)
C
      IF ( SGLE ) THEN
         P1 = ONE
C
C        Compute the (N,N) element of the product and the rotations.
C        This element is used as shift.
C
         DO 40  J = 1, K
            I = AMAP(J)
            IF ( S(I).EQ.SINV ) THEN
               P1 = P1*A(N,N,I)
            ELSE
               P1 = P1/A(N,N,I)
            END IF
   40    CONTINUE
C
         CALL DLARTG( E1 - P1, E2, C1, S1, E1 )
         C2 = ONE
         S2 = ZERO
C
      ELSE
C
         CALL DCOPY( M, A(1,2,I), 1, Y, 1 )
         CALL DTRMV( 'Upper', 'NoTran', 'NonUnit', M, DWORK, M, Y, 1)
         P1 = Y(1)
         P2 = Y(2)
         P3 = Y(3)
C
C        Compute the bottom 2-by-2 part using complete pivoting.
C
         CALL DLASET( 'Full', 2, 2, ZERO, ONE, Z, 2 )
C
         M = N - 1
C
         DO 50  J = K - 1, 1, -1
            I = AMAP(J)
            IF ( S(I).EQ.SINV ) THEN
               Z(1,1) = A(M,M,I)*Z(1,1)
               Z(1,2) = A(M,M,I)*Z(1,2) + A(M,N,I)*Z(2,2)
               Z(2,2) = A(N,N,I)*Z(2,2)
            ELSE
               Y(1) = A(M,M,I)
               Y(2) = ZERO
               CALL DCOPY(  2, A(M,N,I), 1, Y(3), 1 )
               CALL DGETC2( 2, Y, 2, IP, JP, IND )
               CALL DGESC2( 2, Y, 2, Z, IP, JP, SCL )
               CALL DGESC2( 2, Y, 2, Z(1,2), IP, JP, SCL )
            END IF
   50    CONTINUE
C
         I = AMAP(K)
         T      = Z(1,1)*A(M,M,I) + Z(1,2)*A(N,M,I)
         Z(1,2) = Z(1,1)*A(M,N,I) + Z(1,2)*A(N,N,I)
         Z(1,1) = T
         Z(2,1) = Z(2,2)*A(N,M,I)
         Z(2,2) = Z(2,2)*A(N,N,I)
C
C        Compute the eigenvalues of the bottom 2-by-2 part.
C        If there are two real eigenvalues, both shifts are chosen equal
C        to the eigenvalue with minimum modulus. Only the sum and
C        product of the shifts are needed.
C
         CALL DLANV2( Z(1,1), Z(1,2), Z(2,1), Z(2,2), WR(1), WI(1),
     $                WR(2), WI(2), C1, S1 )
         IF ( WI(1).EQ.ZERO ) THEN
            IF ( ABS( WR(1) ).LT.ABS( WR(2) ) ) THEN
               T = WR(1)
            ELSE
               T = WR(2)
            END IF
            SM = TWO*T
            PR = T**2
         ELSE
            SM = TWO*WR(1)
            PR = WR(1)**2 + WI(1)**2
         END IF
C
C        Compute a multiple of the first column of the real Wilkinson
C        double shift polynomial, having only three nonzero elements.
C
         P1 = P1 + ( ( E1 - SM )*E1 + PR )/E2
         P2 = P2 + E1 - SM
C
C        Compute the rotations to annihilate P2 and P3.
C
         CALL DLARTG( P2, P3, C2, S2, E1 )
         CALL DLARTG( P1, E1, C1, S1, E2 )
      END IF
C
      RETURN
C *** Last line of MB03AH ***
      END
