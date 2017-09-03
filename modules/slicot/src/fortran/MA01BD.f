      SUBROUTINE MA01BD( BASE, LGBAS, K, S, A, INCA, ALPHA, BETA, SCAL )
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
C     To compute the general product of K real scalars without over-
C     or underflow.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     BASE    (input)  DOUBLE PRECISION
C             Machine base.
C
C     LGBAS   (input)  DOUBLE PRECISION
C             Logarithm of BASE.
C
C     K       (input)  INTEGER
C             The number of scalars.  K >= 1.
C
C     S       (input)  INTEGER array, dimension (K)
C             The signature array. Each entry of S must be 1 or -1.
C
C     A       (input)  DOUBLE PRECISION array, dimension (K)
C             Vector of real scalars.
C
C     INCA    (input)  INTEGER
C             Increment for the array A. INCA <> 0.
C
C     ALPHA   (output)  DOUBLE PRECISION
C             ALPHA is a real scalar such that
C
C                ALPHA / BETA * BASE**(SCAL)
C
C             is the general product of the scalars in the array A.
C
C     BETA    (output)  DOUBLE PRECISION
C             BETA is either 0.0 or 1.0.
C             See also the description of ALPHA.
C
C     SCAL    (output)  INTEGER
C             Scaling factor exponent, see ALPHA.
C
C     CONTRIBUTOR
C
C     D. Kressner, Technical Univ. Berlin, Germany, June 2001.
C
C     REVISIONS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Romania,
C     July 2009, SLICOT Library version of the routine PLAPR1.
C
C     KEYWORDS
C
C     Computer arithmetic, overflow, underflow.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE
      PARAMETER         ( ZERO = 0.0D0, ONE = 1.0D0 )
C     .. Scalar Arguments ..
      INTEGER           INCA, K, SCAL
      DOUBLE PRECISION  ALPHA, BASE, BETA, LGBAS
C     .. Array Arguments ..
      INTEGER           S(*)
      DOUBLE PRECISION  A(*)
C     .. Local Scalars ..
      INTEGER           I, SL
      DOUBLE PRECISION  TEMP
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, DBLE, INT, LOG, MOD
C
C     .. Executable Statements ..
C
      ALPHA = ONE
      BETA  = ONE
      SCAL  = 0
C
      DO 10  I = 1, K
         TEMP = A( 1 + ( I - 1 )*INCA )
         IF ( TEMP.NE.ZERO ) THEN
            SL   = INT( LOG( ABS( TEMP ) ) / LGBAS )
            TEMP = TEMP / ( BASE**DBLE( SL ) )
         END IF
         IF ( S(I).EQ.1 ) THEN
            ALPHA = ALPHA * TEMP
            SCAL  = SCAL + SL
         ELSE
            BETA = BETA * TEMP
            SCAL = SCAL - SL
         END IF
         IF ( MOD( I, 10 ).EQ.0 ) THEN
            IF ( ALPHA.NE.ZERO ) THEN
               SL    = INT( LOG( ABS( ALPHA ) ) / LGBAS )
               SCAL  = SCAL + SL
               ALPHA = ALPHA / ( BASE**DBLE( SL ) )
            END IF
            IF ( BETA.NE.ZERO ) THEN
               SL   = INT( LOG( ABS( BETA ) ) / LGBAS )
               SCAL = SCAL - SL
               BETA = BETA / ( BASE**DBLE( SL ) )
            END IF
         END IF
   10 CONTINUE
C
      IF ( BETA.NE.ZERO ) THEN
         ALPHA = ALPHA / BETA
         BETA  = ONE
      END IF
      IF ( ALPHA.EQ.ZERO ) THEN
         SCAL = 0
      ELSE
         SL    = INT( LOG( ABS( ALPHA ) ) / LGBAS )
         ALPHA = ALPHA / ( BASE**DBLE( SL ) )
         SCAL  = SCAL + SL
      END IF
C
      RETURN
C *** Last line of MA01BD ***
      END
