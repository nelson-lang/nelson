      SUBROUTINE MB03BE( K, AMAP, S, SINV, A, LDA1, LDA2 )
C
C     SLICOT RELEASE 5.0.
C
C     Copyright (c) 2002-2010 NICONET e.V.
C
C     This program is free software: you can redistribute it and/or
C     modify it under the terms of the GNU General Public License as
C     published by the Free Software Foundation, either version 2 of
C     the License, or (at your option) any later version.
C
C     This program is distributed in the hope that it will be useful,
C     but WITHOUT ANY WARRANTY; without even the implied warranty of
C     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C     GNU General Public License for more details.
C
C     You should have received a copy of the GNU General Public License
C     along with this program.  If not, see
C     <http://www.gnu.org/licenses/>.
C
C     PURPOSE
C
C     To apply 10 iterations of a real single shifted periodic QZ
C     algorithm to the 2-by-2 product of matrices stored in the array A.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     K       (input)  INTEGER
C             The number of factors.  K >= 1.
C
C     AMAP    (input)  INTEGER array, dimension (K)
C             The map for accessing the factors, i.e., if AMAP(I) = J,
C             then the factor A_I is stored at the J-th position in A.
C
C     S       (input)  INTEGER array, dimension (K)
C             The signature array. Each entry of S must be 1 or -1.
C
C     SINV    (input)  INTEGER
C             Signature multiplier. Entries of S are virtually
C             multiplied by SINV.
C
C     A       (input/output)  DOUBLE PRECISION array, dimension
C                             (LDA1,LDA2,K)
C             On entry, the leading 2-by-2-by-K part of this array must
C             contain a 2-by-2 product (implicitly represented by its K
C             factors) in upper Hessenberg form.
C             On exit, the leading 2-by-2-by-K part of this array
C             contains the product after 10 iterations of a real shifted
C             periodic QZ algorithm.
C
C     LDA1    INTEGER
C             The first leading dimension of the array A.  LDA1 >= 2.
C
C     LDA2    INTEGER
C             The second leading dimension of the array A.  LDA2 >= 2.
C
C     METHOD
C
C     Ten iterations of a real single shifted periodic QZ algorithm are
C     applied to the 2-by-2 matrix product A.
C
C     CONTRIBUTOR
C
C     D. Kressner, Technical Univ. Berlin, Germany, June 2001.
C
C     REVISIONS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     July 2009, SLICOT Library version of the routine PLARL2.
C
C     KEYWORDS
C
C     Eigenvalues, QZ algorithm, periodic QZ algorithm, orthogonal
C     transformation.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO
      PARAMETER         ( ZERO = 0.0D0 )
C     .. Scalar Arguments ..
      INTEGER           K, LDA1, LDA2, SINV
C     .. Array Arguments ..
      INTEGER           AMAP(*), S(*)
      DOUBLE PRECISION  A(LDA1,LDA2,*)
C     .. Local Scalars ..
      INTEGER           I, L, AI
      DOUBLE PRECISION  CS, SN, CT, ST, TEMP
C     .. External Subroutines ..
      EXTERNAL          DLARTG, DROT, MB03AD
C
C     .. Executable Statements ..
C
      DO 20  I = 1, 10
         CALL MB03AD( 'Single', K, 2, AMAP, S, SINV, A, LDA1, LDA2,
     $                CS, SN, CT, ST )
         AI = AMAP(1)
         CALL DROT( 2, A(1,1,AI), LDA1, A(2,1,AI), LDA1, CS, SN )
C
         DO 10  L = K, 2, -1
            AI = AMAP(L)
            IF ( S(AI).EQ.SINV ) THEN
               CALL DROT( 2, A(1,1,AI), 1, A(1,2,AI), 1, CS, SN )
               TEMP = A(1,1,AI)
               CALL DLARTG( TEMP, A(2,1,AI), CS, SN, A(1,1,AI) )
               A(2,1,AI) = ZERO
               TEMP      = CS*A(1,2,AI) + SN*A(2,2,AI)
               A(2,2,AI) = CS*A(2,2,AI) - SN*A(1,2,AI)
               A(1,2,AI) = TEMP
            ELSE
               CALL DROT( 2, A(1,1,AI), LDA1, A(2,1,AI), LDA1, CS, SN )
               TEMP = A(2,2,AI)
               CALL DLARTG( TEMP, A(2,1,AI), CS, SN, A(2,2,AI) )
               A(2,1,AI) = ZERO
               SN = -SN
               TEMP      = CS*A(1,1,AI) + SN*A(1,2,AI)
               A(1,2,AI) = CS*A(1,2,AI) - SN*A(1,1,AI)
               A(1,1,AI) = TEMP
            END IF
   10    CONTINUE
C
         AI = AMAP(1)
         CALL DROT( 2, A(1,1,AI), 1, A(1,2,AI), 1, CS, SN )
   20 CONTINUE
C
      RETURN
C *** Last line of MB03BE ***
      END
