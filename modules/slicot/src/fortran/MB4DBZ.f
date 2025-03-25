      SUBROUTINE MB4DBZ( JOB, SGN, N, ILO, LSCALE, RSCALE, M, V1, LDV1,
     $                   V2, LDV2, INFO )
C
C     PURPOSE
C
C     To apply from the left the inverse of a balancing transformation,
C     computed by the SLICOT Library routine MB4DPZ, to the complex
C     matrix
C
C               [   V1   ]
C               [        ],
C               [ sgn*V2 ]
C
C     where sgn is either +1 or -1.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOB     CHARACTER*1
C             Specifies the type of inverse transformation required:
C             = 'N':  do nothing, return immediately;
C             = 'P':  do inverse transformation for permutation only;
C             = 'S':  do inverse transformation for scaling only;
C             = 'B':  do inverse transformations for both permutation
C                     and scaling.
C             JOB must be the same as the argument JOB supplied to
C             MB4DPZ.
C
C     SGN     CHARACTER*1
C             Specifies the sign to use for V2:
C             = 'P':  sgn = +1;
C             = 'N':  sgn = -1.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The number of rows of the matrices V1 and V2. N >= 0.
C
C     ILO     (input) INTEGER
C             The integer ILO determined by MB4DPZ.
C             1 <= ILO <= N+1.
C
C     LSCALE  (input) DOUBLE PRECISION array, dimension (N)
C             Details of the permutation and scaling factors applied
C             from the left, as returned by MB4DPZ.
C
C     RSCALE  (input) DOUBLE PRECISION array, dimension (N)
C             Details of the permutation and scaling factors applied
C             from the right, as returned by MB4DPZ.
C
C     M       (input) INTEGER
C             The number of columns of the matrices V1 and V2.  M >= 0.
C
C     V1      (input/output) COMPLEX*16 array, dimension (LDV1,M)
C             On entry, the leading N-by-M part of this array must
C             contain the matrix V1.
C             On exit, the leading N-by-M part of this array is
C             overwritten by the updated matrix V1 of the transformed
C             matrix.
C
C     LDV1    INTEGER
C             The leading dimension of the array V1. LDV1 >= max(1,N).
C
C     V2      (input/output) COMPLEX*16 array, dimension (LDV2,M)
C             On entry, the leading N-by-M part of this array must
C             contain the matrix V2.
C             On exit, the leading N-by-M part of this array is
C             overwritten by the updated matrix V2 of the transformed
C             matrix.
C
C     LDV2    INTEGER
C             The leading dimension of the array V2. LDV2 >= max(1,N).
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value.
C
C     REFERENCES
C
C     [1] Anderson, E., Bai, Z., Bischof, C., Demmel, J., Dongarra, J.,
C         Du Croz, J., Greenbaum, A., Hammarling, S., McKenney, A.,
C         Ostrouchov, S., and Sorensen, D.
C         LAPACK Users' Guide: Second Edition.
C         SIAM, Philadelphia, 1995.
C
C     [2] Benner, P.
C         Symplectic balancing of Hamiltonian matrices.
C         SIAM J. Sci. Comput., 22 (5), pp. 1885-1904, 2001.
C
C     CONTRIBUTORS
C
C     V. Sima, Research Institute for Informatics, Bucharest, Dec. 2015.
C
C     REVISIONS
C
C     V. Sima, Jan. 2016.
C
C     KEYWORDS
C
C     Balancing, Hamiltonian matrix, skew-Hamiltonian matrix,
C     symplectic equivalence transformation.
C
C     ******************************************************************
C
C     .. Parameters ..
      COMPLEX*16        ONE
      PARAMETER         ( ONE = ( 1.0D0, 0.0D0 ) )
C     .. Scalar Arguments ..
      CHARACTER         JOB, SGN
      INTEGER           ILO, INFO, LDV1, LDV2, M, N
C     .. Array Arguments ..
      DOUBLE PRECISION  LSCALE(*), RSCALE(*)
      COMPLEX*16        V1(LDV1,*), V2(LDV2,*)
C     .. Local Scalars ..
      LOGICAL           LPERM, LSCAL, LSGN, SYSW
      INTEGER           I, K
C     .. External Functions ..
      LOGICAL           LSAME
      EXTERNAL          LSAME
C     .. External Subroutines ..
      EXTERNAL          XERBLA, ZDRSCL, ZSCAL, ZSWAP
C     .. Intrinsic Functions ..
      INTRINSIC         MAX
C
C     .. Executable Statements ..
C
C     Check the scalar input parameters.
C
      INFO  = 0
      LPERM = LSAME( JOB, 'P' ) .OR. LSAME( JOB, 'B' )
      LSCAL = LSAME( JOB, 'S' ) .OR. LSAME( JOB, 'B' )
      LSGN  = LSAME( SGN, 'N' )
      IF ( .NOT.LPERM .AND. .NOT.LSCAL
     $     .AND. .NOT.LSAME( JOB, 'N' ) ) THEN
         INFO = -1
      ELSE IF ( .NOT.LSGN .AND. .NOT.LSAME( SGN, 'P' ) ) THEN
         INFO = -2
      ELSE IF ( N.LT.0 ) THEN
         INFO = -3
      ELSE IF ( ILO.LT.1 .OR. ILO.GT.N+1 ) THEN
         INFO = -4
      ELSE IF ( M.LT.0 ) THEN
         INFO = -7
      ELSE IF ( LDV1.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE IF ( LDV2.LT.MAX( 1, N ) ) THEN
         INFO = -11
      END IF
C
C     Return if there were illegal values.
C
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'MB4DBZ', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF ( N.EQ.0 .OR. M.EQ.0 .OR. LSAME( JOB, 'N' ) )
     $   RETURN
C
C     Inverse scaling.
C
      IF ( LSCAL ) THEN
         DO 10 I = ILO, N
            CALL ZDRSCL( M, LSCALE(I), V1(I,1), LDV1 )
   10    CONTINUE
         DO 20 I = ILO, N
            CALL ZDRSCL( M, RSCALE(I), V2(I,1), LDV2 )
   20    CONTINUE
      END IF
C
C     Inverse permutation.
C
      IF ( LPERM ) THEN
         DO 30 I = ILO-1, 1, -1
            K = LSCALE(I)
            SYSW = K.GT.N
            IF ( SYSW )
     $         K = K - N
C
            IF ( K.NE.I ) THEN
C
C              Exchange rows k <-> i.
C
               CALL ZSWAP( M, V1(I,1), LDV1, V1(K,1), LDV1 )
               CALL ZSWAP( M, V2(I,1), LDV2, V2(K,1), LDV2 )
            END IF
C
            IF ( SYSW ) THEN
C
C              Exchange V1(k,:) <-> V2(k,:).
C
               CALL ZSWAP( M, V1(K,1), LDV1, V2(K,1), LDV2 )
C
               IF ( LSGN ) THEN
                  CALL ZSCAL( M, -ONE, V1(K,1), LDV1 )
               ELSE
                  CALL ZSCAL( M, -ONE, V2(K,1), LDV2 )
               END IF
            END IF
   30    CONTINUE
      END IF
C
      RETURN
C *** Last line of MB4DBZ ***
      END
