      SUBROUTINE MB03BA( K, H, S, SMULT, AMAP, QMAP )
C
C     PURPOSE
C
C     To compute the suitable maps for Hessenberg index H and
C     signature array S. Auxiliary routine for the periodic QZ
C     algorithms.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     K       (input)  INTEGER
C             The number of factors.  K >= 1.
C
C     H       (input)  INTEGER
C             Index which corresponds to A_1.
C
C     S       (input)  INTEGER array, dimension (K)
C             The signature array. Each entry of S must be 1 or -1.
C
C     SMULT   (output)  INTEGER
C             Signature multiplier. Entries of S are virtually
C             multiplied by SMULT.
C
C     AMAP    (output)  INTEGER array, dimension (K)
C             The map for accessing the factors, that is,
C             if AMAP(I) = J, then the factor A_I is stored at the J-th
C             position in A.
C
C     QMAP    (output)  INTEGER array, dimension (K)
C             The map for accessing the orthognal transformation
C             matrices, that is, if QMAP(I) = J, then the matrix Q_I is
C             stored at the J-th position in Q.
C
C     CONTRIBUTOR
C
C     D. Kressner, Technical Univ. Berlin, Germany, June 2001.
C
C     REVISIONS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     July 2009, SLICOT Library version of the routine PLAIND.
C
C     KEYWORDS
C
C     Hessenberg matrix, QZ algorithm, periodic QZ algorithm.
C
C     ******************************************************************
C
C     .. Scalar Arguments ..
      INTEGER           K, H, SMULT
C     .. Array Arguments ..
      INTEGER           AMAP(*), QMAP(*), S(*)
C     .. Local Scalars ..
      INTEGER           I, TEMP
C     .. Intrinsic Functions ..
      INTRINSIC         MOD
C
C     .. Executable Statements ..
C
      IF ( S(H).EQ.-1 ) THEN
         SMULT = -1
         DO 10  I = 1, H
            AMAP(I) = H-I+1
   10    CONTINUE
         DO 20  I = H+1, K
            AMAP(I) = H+1-I+K
   20    CONTINUE
         TEMP = MOD( H, K ) + 1
         DO 30  I = TEMP, 1, -1
            QMAP(TEMP-I+1) = I
   30    CONTINUE
         DO 40  I = K, TEMP + 1, -1
            QMAP(TEMP+K-I+1) = I
   40    CONTINUE
      ELSE
         SMULT = 1
         DO 50  I = H, K
            AMAP(I-H+1) = I
            QMAP(I-H+1) = I
   50    CONTINUE
         DO 60  I = 1, H-1
            AMAP(K-H+I+1) = I
            QMAP(K-H+I+1) = I
   60    CONTINUE
      END IF
C
      RETURN
C *** Last line of MB03BA ***
      END
