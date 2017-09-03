      SUBROUTINE MB02UW( LTRANS, N, M, PAR, A, LDA, B, LDB, SCALE,
     $                   IWARN )
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
C     To solve a system of the form  A X = s B  or  A' X = s B  with
C     possible scaling ("s") and perturbation of A.  (A' means
C     A-transpose.)  A is an N-by-N real matrix, and X and B are
C     N-by-M matrices.  N may be 1 or 2.  The scalar "s" is a scaling
C     factor (.LE. 1), computed by this subroutine, which is so chosen
C     that X can be computed without overflow.  X is further scaled if
C     necessary to assure that norm(A)*norm(X) is less than overflow.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     LTRANS  LOGICAL
C             Specifies if A or A-transpose is to be used, as follows:
C             =.TRUE. :  A-transpose will be used;
C             =.FALSE.:  A will be used (not transposed).
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrix A.  It may (only) be 1 or 2.
C
C     M       (input) INTEGER
C             The number of right hand size vectors.
C
C     PAR     (input) DOUBLE PRECISION array, dimension (3)
C             Machine related parameters:
C             PAR(1) =: PREC  (machine precision)*base, DLAMCH( 'P' );
C             PAR(2) =: SFMIN safe minimum,             DLAMCH( 'S' );
C             PAR(3) =: SMIN  The desired lower bound on the singular
C                             values of A.  This should be a safe
C                             distance away from underflow or overflow,
C                             say, between (underflow/machine precision)
C                             and (machine precision * overflow).
C
C     A       (input) DOUBLE PRECISION array, dimension (LDA,N)
C             The leading N-by-N part of this array must contain the
C             matrix A.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= N.
C
C     B       (input/output) DOUBLE PRECISION array, dimension (LDB,M)
C             On entry, the leading N-by-M part of this array must
C             contain the matrix B (right-hand side).
C             On exit, the leading N-by-M part of this array contains
C             the N-by-M matrix X (unknowns).
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= N.
C
C     SCALE   (output) DOUBLE PRECISION
C             The scale factor that B must be multiplied by to insure
C             that overflow does not occur when computing X.  Thus,
C             A X  will be SCALE*B, not B (ignoring perturbations of A).
C             SCALE will be at most 1.
C
C     Warning Indicator
C
C     IWARN   INTEGER
C             = 0:  no warnings (A did not have to be perturbed);
C             = 1:  A had to be perturbed to make its smallest (or only)
C                   singular value greater than SMIN (see below).
C
C     METHOD
C
C     Gaussian elimination with complete pivoting is used. The matrix A
C     is slightly perturbed if it is (close to being) singular.
C
C     FURTHER COMMENTS
C
C     If both singular values of A are less than SMIN, SMIN*identity
C     will be used instead of A.  If only one singular value is less
C     than SMIN, one element of A will be perturbed enough to make the
C     smallest singular value roughly SMIN.  If both singular values
C     are at least SMIN, A will not be perturbed.  In any case, the
C     perturbation will be at most some small multiple of
C     max( SMIN, EPS*norm(A) ), where EPS is the machine precision
C     (see LAPACK Library routine DLAMCH).  The singular values are
C     computed by infinity-norm approximations, and thus will only be
C     correct to a factor of 2 or so.
C
C     Note: all input quantities are assumed to be smaller than overflow
C     by a reasonable factor.  (See BIGNUM.)  In the interests of speed,
C     this routine does not check the inputs for errors.
C
C     CONTRIBUTOR
C
C     V. Sima, Research Institute for Informatics, Bucharest, Aug. 2009.
C     Based on the LAPACK Library routine DLALN2.
C
C     REVISIONS
C
C     V. Sima, Nov. 2010.
C
C     KEYWORDS
C
C     Linear system of equations, matrix operations, matrix algebra.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0 )
C
C     .. Scalar Arguments ..
      LOGICAL            LTRANS
      INTEGER            IWARN, LDA, LDB, N, M
      DOUBLE PRECISION   SCALE, SMIN
C
C     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), PAR( * )
C
C     .. Local Scalars ..
      INTEGER            I, ICMAX, J
      DOUBLE PRECISION   BBND, BIGNUM, BNORM, B1, B2, CMAX, C21, C22,
     $                   CS, EPS, L21, SCALEP, SMINI, SMLNUM, TEMP, U11,
     $                   U11R, U12, U22, XNORM, X1, X2
C
C     .. Local Arrays ..
      LOGICAL            RSWAP( 4 ), ZSWAP( 4 )
      INTEGER            IPIVOT( 4, 4 )
      DOUBLE PRECISION   C( 2, 2 ), CV( 4 )
C
C     ..External Functions ..
      INTEGER            IDAMAX
      DOUBLE PRECISION   DLANGE
      EXTERNAL           DLANGE, IDAMAX
C
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
C     ..
C     .. Equivalences ..
      EQUIVALENCE        ( C( 1, 1 ), CV( 1 ) )
C     ..
C     .. Data statements ..
      DATA               ZSWAP / .FALSE., .FALSE., .TRUE., .TRUE. /
      DATA               RSWAP / .FALSE., .TRUE., .FALSE., .TRUE. /
      DATA               IPIVOT / 1, 2, 3, 4, 2, 1, 4, 3, 3, 4, 1, 2, 4,
     $                   3, 2, 1 /
C
C     .. Executable Statements ..
C
C     For efficiency, the input arguments are not tested.
C
      IWARN = 0
C
C     Compute BIGNUM.
C
      SMIN   = PAR( 3 )
      EPS    = PAR( 1 )
      SMLNUM = TWO*PAR( 2 ) / EPS
      BIGNUM = ONE / SMLNUM
C
C     Standard initializations.
C
      SCALE = ONE
C
      IF( N.EQ.1 ) THEN
C
C        1-by-1  (i.e., scalar) systems  C X = B.
C
         CS    = A( 1, 1 )
         CMAX  = ABS( CS )
         SMINI = MAX( SMIN, SMLNUM, EPS*CMAX )
C
C        If | C | < SMINI, use C = SMINI.
C
         IF( CMAX.LT.SMINI ) THEN
            CS    = SMINI
            CMAX  = SMINI
            IWARN = 1
         END IF
C
C        Check scaling for  X = B / C.
C
         BNORM = ABS( B( 1, IDAMAX( M, B, LDB ) ) )
         IF( CMAX.LT.ONE .AND. BNORM.GT.ONE ) THEN
            IF( BNORM.GT.BIGNUM*CMAX )
     $         SCALE = ONE / BNORM
         END IF
C
C        Compute X.
C
         DO 10 I = 1, M
            B( 1, I ) = ( B( 1, I )*SCALE ) / CS
   10    CONTINUE
C
      ELSE
C
C        2x2 systems.
C
C        Compute C = A  (or  A').
C
         C( 1, 1 ) = A( 1, 1 )
         C( 2, 2 ) = A( 2, 2 )
         IF( LTRANS ) THEN
            C( 1, 2 ) = A( 2, 1 )
            C( 2, 1 ) = A( 1, 2 )
         ELSE
            C( 2, 1 ) = A( 2, 1 )
            C( 1, 2 ) = A( 1, 2 )
         END IF
C
         BNORM = DLANGE( 'M', N, M, B, LDB, CV )
C
C        Find the largest element in C.
C
         CMAX  = ZERO
         ICMAX = 0
C
         DO 20 J = 1, 4
            IF( ABS( CV( J ) ).GT.CMAX ) THEN
               CMAX  = ABS( CV( J ) )
               ICMAX = J
            END IF
   20    CONTINUE
C
         SMINI = MAX( SMIN, SMLNUM, EPS*CMAX )
C
C        If norm(C) < SMINI, use SMINI*identity.
C
         IF( CMAX.LT.SMINI ) THEN
            IF( SMINI.LT.ONE .AND. BNORM.GT.ONE ) THEN
               IF( BNORM.GT.BIGNUM*SMINI )
     $            SCALE = ONE / BNORM
            END IF
            TEMP = SCALE / SMINI
C
            DO 30 I = 1, M
               B( 1, I ) = TEMP*B( 1, I )
               B( 2, I ) = TEMP*B( 2, I )
   30       CONTINUE
C
            IWARN = 1
            RETURN
         END IF
C
C        Gaussian elimination with complete pivoting.
C
         U11  = CV( ICMAX )
         C21  = CV( IPIVOT( 2, ICMAX ) )
         U12  = CV( IPIVOT( 3, ICMAX ) )
         C22  = CV( IPIVOT( 4, ICMAX ) )
         U11R = ONE / U11
         L21  = U11R*C21
         U22  = C22 - U12*L21
C
C        If smaller pivot < SMINI, use SMINI.
C
         IF( ABS( U22 ).LT.SMINI ) THEN
            U22   = SMINI
            IWARN = 1
         END IF
C
         SCALEP = ONE
C
         DO 50 I = 1, M
            IF( RSWAP( ICMAX ) ) THEN
               B1 = B( 2, I )
               B2 = B( 1, I )
            ELSE
               B1 = B( 1, I )
               B2 = B( 2, I )
            END IF
            B2   = B2 - L21*B1
            BBND = MAX( ABS( B1*( U22*U11R ) ), ABS( B2 ) )
            IF( BBND.GT.ONE .AND. ABS( U22 ).LT.ONE ) THEN
               IF( BBND.GE.BIGNUM*ABS( U22 ) )
     $            SCALE = ONE / BBND
            END IF
            SCALE = MIN( SCALE, SCALEP )
            IF( SCALE.LT.SCALEP ) THEN
               SCALEP = SCALE / SCALEP
C
               DO 40 J = 1, I - 1
                  B( 1, J ) = B( 1, J )*SCALEP
                  B( 2, J ) = B( 2, J )*SCALEP
   40          CONTINUE
C
            END IF
C
            X2 = ( B2*SCALE ) / U22
            X1 = ( SCALE*B1 )*U11R - X2*( U11R*U12 )
            IF( ZSWAP( ICMAX ) ) THEN
               B( 1, I ) = X2
               B( 2, I ) = X1
            ELSE
               B( 1, I ) = X1
               B( 2, I ) = X2
            END IF
            XNORM = MAX( ABS( X1 ), ABS( X2 ) )
C
C           Further scaling if  norm(A) norm(X) > overflow.
C
            IF( XNORM.GT.ONE .AND. CMAX.GT.ONE ) THEN
               IF( XNORM.GT.BIGNUM / CMAX ) THEN
                  TEMP  = CMAX / BIGNUM
                  B( 1, I ) = TEMP*B( 1, I )
                  B( 2, I ) = TEMP*B( 2, I )
                  SCALE = TEMP*SCALE
               END IF
            END IF
            SCALEP = SCALE
   50    CONTINUE
C
      END IF
C
      RETURN
C
C *** Last line of MB02UW ***
      END
