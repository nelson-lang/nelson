      SUBROUTINE MB03AF( SHFT, K, N, AMAP, S, SINV, A, LDA1, LDA2, C1,
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
C     MB03BE or MB03BF).
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     SHFT    CHARACTER*1
C             Specifies the number of shifts employed by the shift
C             polynomial, as follows:
C             = 'D':  two real shifts;
C             = 'S':  one real shift.
C
C     Input/Output Parameters
C
C     K       (input)  INTEGER
C             The number of factors.  K >= 1.
C
C     N       (input)  INTEGER
C             The order of the factors in the array A.
C             N >= 2, for a single shift polynomial;
C             N >= 3, for a double shift polynomial.
C
C     AMAP    (input) INTEGER array, dimension (K)
C             The map for accessing the factors, i.e., if AMAP(I) = J,
C             then the factor A_I is stored at the J-th position in A.
C             AMAP(K) is the pointer to the Hessenberg matrix.
C             Before calling this routine, AMAP returned by SLICOT
C             Library routine MB03BA should be modified as follows:
C             J = AMAP(1), AMAP(I) = AMAP(I+1), I = 1:K-1, AMAP(K) = J.
C
C     S       (input)  INTEGER array, dimension (K)
C             The signature array. Each entry of S must be 1 or -1.
C             S(K) is not used, but assumed to be 1.
C
C     SINV    (input) INTEGER
C             Signature multiplier. Entries of S are virtually
C             multiplied by SINV.
C
C     A       (input)  DOUBLE PRECISION array, dimension (LDA1,LDA2,K)
C             On entry, the leading N-by-N-by-K part of this array must
C             contain a n-by-n product (implicitly represented by its K
C             factors) in periodic upper Hessenberg form.
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
C             On exit, if SHFT = 'D', C2 and S2 contain the parameters
C             for the second Givens rotation. Otherwise, C2 = 1, S2 = 0.
C
C     METHOD
C
C     Givens rotations are properly computed and applied.
C
C     CONTRIBUTOR
C
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     Dec. 2018, Dec. 2020.
C
C     REVISIONS
C
C     V. Sima, Dec. 2019.
C
C     KEYWORDS
C
C     Eigenvalues, QZ algorithm, periodic QZ algorithm, orthogonal
C     transformation.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ONE
      PARAMETER         ( ONE = 1.0D0 )
C     .. Scalar Arguments ..
      CHARACTER         SHFT
      INTEGER           K, LDA1, LDA2, N, SINV
      DOUBLE PRECISION  C1, C2, S1, S2
C     .. Array Arguments ..
      INTEGER           AMAP(*), S(*)
      DOUBLE PRECISION  A(LDA1,LDA2,*)
C     .. Local Scalars ..
      LOGICAL           SGLE
      INTEGER           AI, I, M
      DOUBLE PRECISION  ALPHA, BETA, C2R, C3, C3R, C4, C4R, C5, C5R, C6,
     $                  C6R, CS, CX, DELTA, EPSIL, ETA, GAMMA, S2R, S3,
     $                  S3R, S4, S4R, S5, S5R, S6, S6R, SS, SSS, SSSS,
     $                  SX, TEMP, THETA, VAL1, VAL2, VAL3, VAL4, VAL5,
     $                  ZETA
C     .. External Functions ..
      LOGICAL           LSAME
      EXTERNAL          LSAME
C     .. External Subroutines ..
      EXTERNAL          DLARTG
C
C     .. Executable Statements ..
C
      SGLE = LSAME( SHFT, 'S' )
      M = N - 1
      AI = AMAP(K)
      CALL DLARTG( A(1,1,AI), A(2,1,AI), C1, S1, TEMP )
	CALL DLARTG( TEMP,      ONE,       C2, S2, TEMP )
C
      DO 10  I = K - 1, 1, -1
         AI = AMAP(I)
         IF ( S(AI).EQ.SINV ) THEN
            ALPHA = C2 * ( C1 * A(1,1,AI) + S1 * A(1,2,AI) )
            BETA  = S1 * C2 * A(2,2,AI)
            GAMMA = S2 * A(N,N,AI)
            CALL DLARTG( ALPHA, BETA,  C1, S1, TEMP )
            CALL DLARTG( TEMP,  GAMMA, C2, S2, VAL1 )
         ELSE
            ALPHA = C1 * S2 * A(1,1,AI)
            GAMMA = S1 * A(1,1,AI)
            BETA  = S2 * ( C1 * A(1,2,AI) + S1 * A(2,2,AI) )
            DELTA = C1 * A(2,2,AI) - S1 * A(1,2,AI)
            CALL DLARTG( DELTA, GAMMA, C1, S1, TEMP )
            ALPHA = C1 * ALPHA + S1 * BETA
            BETA  = C2 * A(N,N,AI)
            CALL DLARTG( BETA, ALPHA, C2, S2, TEMP )
         END IF
   10 CONTINUE
C
      I  = K
      AI = AMAP(I)
      ALPHA =  S2 * A(N,N,AI) - C1 * C2
      BETA  = -S1 * C2
C
      IF ( SGLE ) THEN
C
C        This is sufficient for single real shifts.
C
         CALL DLARTG( ALPHA, BETA, C1, S1, TEMP )
C
      ELSE
C
         GAMMA = -S2 * A(N,M,AI)
         CALL DLARTG( ALPHA, GAMMA, C2, S2, TEMP )
         CALL DLARTG( TEMP,  BETA,  C1, S1, TEMP )
         CX = C1 * C2
         SX = C1 * S2
         BETA  = S1 * A(N,M,AI)
         ALPHA = CX * A(N,M,AI) + SX * A(N,N,AI)
         GAMMA = S1 * A(M,M,AI)
         DELTA = CX * A(M,M,AI) + SX * A(M,N,AI)
         VAL1  = S1 * A(3,2,AI)
         VAL2  = CX * A(2,1,AI) + S1 * A(2,2,AI)
         VAL3  = CX * A(1,1,AI) + S1 * A(1,2,AI)
         CALL DLARTG( ALPHA, BETA, C1, S1, TEMP )
         CALL DLARTG( GAMMA, TEMP, C2, S2, TEMP )
         CALL DLARTG( DELTA, TEMP, C3, S3, TEMP )
         CALL DLARTG( VAL1,  TEMP, C4, S4, TEMP )
         CALL DLARTG( VAL2,  TEMP, C5, S5, TEMP )
         CALL DLARTG( VAL3,  TEMP, C6, S6, TEMP )
C
         DO 20  I = K - 1, 1, -1
            AI = AMAP(I)
C
            IF ( S(AI).EQ.SINV ) THEN
               SS    = S3 * S4
               SSS   = S2 * SS
               SSSS  = S1 * SSS
               VAL1  = C4 * A(1,3,AI)
               VAL2  = C4 * A(2,3,AI)
               VAL3  = C4 * A(3,3,AI)
               ALPHA = S4 * C3 * A(M,M,AI) + SSS  * C1 * A(M,N,AI)
               BETA  = SS * C2 * A(M,M,AI) + SSSS * A(M,N,AI)
               GAMMA = SSS  * C1 * A(N,N,AI)
               DELTA = SSSS * A(N,N,AI)
C
               SS    = S5 * S6
               CS    = C5 * S6
               VAL1  = SS * VAL1 + CS * A(1,2,AI) + C6 * A(1,1,AI)
               VAL2  = SS * VAL2 + CS * A(2,2,AI)
               VAL3  = SS * VAL3
               ALPHA = SS * ALPHA
               BETA  = SS * BETA
               GAMMA = SS * GAMMA
               DELTA = SS * DELTA
C
               CALL DLARTG( GAMMA, DELTA, C1, S1, TEMP )
               CALL DLARTG( BETA,  TEMP,  C2, S2, TEMP )
               CALL DLARTG( ALPHA, TEMP,  C3, S3, TEMP )
               CALL DLARTG( VAL3,  TEMP,  C4, S4, TEMP )
               CALL DLARTG( VAL2,  TEMP,  C5, S5, TEMP )
               CALL DLARTG( VAL1,  TEMP,  C6, S6, TEMP )
C
            ELSE
C
               DELTA =  C1 * A(N,N,AI)
               EPSIL =  S1 * A(N,N,AI)
C
               ALPHA =  C2 * A(M,M,AI)
               BETA  =  S2 * DELTA
               GAMMA = -S2 * A(M,M,AI)
               ZETA  =  C2 * A(M,N,AI) + S2 * EPSIL
               ETA   = -S2 * A(M,N,AI) + C2 * EPSIL
C
C              Update the entry (2n+1,2n+1) for G1'.
C
               DELTA = C1 * C2 * DELTA + S1*ETA
C
C              Compute the new, right rotation G2.
C
               CALL DLARTG( DELTA, -GAMMA,  C2R, S2R, TEMP )
C
C              Apply G3 to the 2-by-4 submatrix in
C              (n+1:n+2,[n+1:n+2 2n+1:2n+1]).
C
               DELTA =  C3 * A(M,M,AI)
               EPSIL =  S3 * ALPHA
               ETA   =  C3 * A(M,N,AI) + S3 * BETA
               THETA =  S3 * ZETA
               GAMMA = -S3 * A(M,M,AI)
               BETA  = -S3 * A(M,N,AI) + C3 * BETA
C
C              Update the entry (n+2,n+2) for G1' and G2R'.
C
               ALPHA = C2R * C3 * ALPHA + S2R * ( C1 * BETA +
     $                                            S1 * C3 * ZETA )
C
C              Compute the new G3.
C
               CALL DLARTG( ALPHA, -GAMMA, C3R, S3R, TEMP )
C
C              Apply G4 to the 2-by-5 submatrix in
C              ([3 n+1],[3 n+1:n+2 2n+1:2n+1]).
C
               VAL1  =  C4 * A(3,3,AI)
               VAL2  =  S4 * DELTA
               VAL3  =  S4 * EPSIL
               VAL4  =  S4 * ETA
               VAL5  =  S4 * THETA
               BETA  = -S4 * A(3,3,AI)
               DELTA =  C4 * DELTA
               EPSIL =  C4 * EPSIL
               ZETA  =  C4 * ETA
               ETA   =  C4 * THETA
C
C              Update the entry (n+1,n+1) for G1', G2R', and G3R'.
C
               ALPHA = C3R * DELTA + S3R * ( C2R * EPSIL + S2R *
     $                                      ( C1 * ZETA  + S1 * ETA ) )
C
C              Compute the new G4.
C
               CALL DLARTG( ALPHA, -BETA, C4R, S4R, TEMP )
C
C              Apply G5 to the 2-by-6 submatrix in
C              (2:3,[2:3 n+1:n+2 2n+1:2n+2]).
C
               BETA  =  C5 * A(2,2,AI)
               DELTA =  C5 * A(2,3,AI) + S5 * VAL1
               EPSIL =  S5 * VAL2
               ZETA  =  S5 * VAL3
               ETA   =  S5 * VAL4
               THETA =  S5 * VAL5
               GAMMA = -S5 * A(2,2,AI)
               VAL1  =  C5 * VAL1 - S5 * A(2,3,AI)
               VAL2  =  C5 * VAL2
               VAL3  =  C5 * VAL3
               VAL4  =  C5 * VAL4
               VAL5  =  C5 * VAL5
C
C              Update the entry (3,3) for G1', G2R', G3R', and G4R'.
C
               ALPHA = C4R * VAL1 + S4R * ( C3R * VAL2 + S3R *
     $                                    ( C2R * VAL3 + S2R *
     $                                    ( C1 * VAL4 + S1 * VAL5 ) ) )
C
C              Compute the new G5.
C
               CALL DLARTG( ALPHA, -GAMMA, C5R, S5R, TEMP )
C
C              Apply G6 to the 2-by-7 submatrix in
C              (1:2,[1:3 n+1:n+2 2n+1:2n+2]).
C
               GAMMA = -S6 * A(1,1,AI)
               BETA  =  C6 * BETA  - S6 * A(1,2,AI)
               DELTA =  C6 * DELTA - S6 * A(1,3,AI)
               EPSIL =  C6 * EPSIL
               ZETA  =  C6 * ZETA
               ETA   =  C6 * ETA
               THETA =  C6 * THETA
C
C              Update the entry (2,2) for G1', G2R', G3R', G4R', and
C              G5R'.
C
               ALPHA = C5R * BETA + S5R * ( C4R * DELTA + S4R *
     $                                    ( C3R * EPSIL + S3R *
     $                                    ( C2R * ZETA  + S2R *
     $                                    ( C1  * ETA   + S1  * THETA )
     $                                ) ) )
C
C              Compute the new G5.
C
               CALL DLARTG( ALPHA, -GAMMA, C6R, S6R, TEMP )
C
               C2 = C2R
               S2 = S2R
               C3 = C3R
               S3 = S3R
               C4 = C4R
               S4 = S4R
               C5 = C5R
               S5 = S5R
               C6 = C6R
               S6 = S6R
C
            END IF
C
   20    CONTINUE
C
C        Last step: let the rotations collap into the first factor.
C
         VAL1  =  S5 * S6
         VAL2  =  S4 * VAL1
         VAL3  =  S3 * VAL2
         ALPHA =  C3 * VAL2 - C6
         BETA  =  C2 * VAL3 - C5 * S6
         GAMMA = -C4 * VAL1
         CALL DLARTG( BETA,  GAMMA, C2, S2, TEMP )
         CALL DLARTG( ALPHA, TEMP,  C1, S1, VAL1 )
C
      END IF
C
      RETURN
C *** Last line of MB03AF ***
      END
