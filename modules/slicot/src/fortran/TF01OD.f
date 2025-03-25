      SUBROUTINE TF01OD( NH1, NH2, NR, NC, H, LDH, T, LDT, INFO )
C
C     PURPOSE
C
C     To construct the block Hankel expansion T of a multivariable
C     parameter sequence M(1),...,M(NR+NC-1), where each parameter M(k)
C     is an NH1-by-NH2 block matrix and k = 1,2,...,(NR+NC-1).
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     NH1     (input) INTEGER
C             The number of rows in each parameter M(k).  NH1 >= 0.
C
C     NH2     (input) INTEGER
C             The number of columns in each parameter M(k).  NH2 >= 0.
C
C     NR      (input) INTEGER
C             The number of parameters required in each column of the
C             block Hankel expansion matrix T.  NR >= 0.
C
C     NC      (input) INTEGER
C             The number of parameters required in each row of the
C             block Hankel expansion matrix T.  NC >= 0.
C
C     H       (input) DOUBLE PRECISION array, dimension
C             (LDH,(NR+NC-1)*NH2)
C             The leading NH1-by-(NR+NC-1)*NH2 part of this array must
C             contain the multivariable sequence M(k), where k = 1,2,
C             ...,(NR+NC-1). Specifically, each parameter M(k) is an
C             NH1-by-NH2 matrix whose (i,j)-th element must be stored in
C             H(i,(k-1)*NH2+j) for i = 1,2,...,NH1 and j = 1,2,...,NH2.
C
C     LDH     INTEGER
C             The leading dimension of array H.  LDH >= MAX(1,NH1).
C
C     T       (output) DOUBLE PRECISION array, dimension (LDT,NH2*NC)
C             The leading NH1*NR-by-NH2*NC part of this array contains
C             the block Hankel expansion of the multivariable sequence
C             M(k).
C
C     LDT     INTEGER
C             The leading dimension of array T.  LDT >= MAX(1,NH1*NR).
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value.
C
C     METHOD
C
C     The NH1-by-NH2 dimensional parameters M(k) of a multivariable
C     sequence are arranged into a matrix T in Hankel form such that
C
C
C              | M(1)   M(2)    M(3)    . . .  M(NC)     |
C              |                                         |
C              | M(2)   M(3)    M(4)    . . .  M(NC+1)   |
C         T =  |  .      .       .              .        |.
C              |  .      .       .              .        |
C              |  .      .       .              .        |
C              |                                         |
C              | M(NR)  M(NR+1) M(NR+2) . . .  M(NR+NC-1)|
C
C     REFERENCES
C
C     [1] Johvidov, J.S.
C         Hankel and Toeplitz Matrices and Forms: Algebraic Theory,
C         (translated by G.P.A. Thijsse, I. Gohberg, ed.).
C         Birkhaeuser, Boston, 1982.
C
C     NUMERICAL ASPECTS
C
C     The time taken is approximately proportional to
C     NH1 x NH2 x NR x NC.
C
C     CONTRIBUTOR
C
C     Release 3.0: V. Sima, Katholieke Univ. Leuven, Belgium, Dec. 1996.
C     Supersedes Release 2.0 routine TF01CD by S. Van Huffel, Katholieke
C     Univ. Leuven, Belgium.
C
C     REVISIONS
C
C     -
C
C     KEYWORDS
C
C     Hankel matrix, multivariable system.
C
C     ******************************************************************
C
C     .. Scalar Arguments ..
      INTEGER           INFO, LDH, LDT, NC, NH1, NH2, NR
C     .. Array Arguments ..
      DOUBLE PRECISION  H(LDH,*), T(LDT,*)
C     .. Local Scalars ..
      INTEGER           IH, IT, JT, NROW
C     .. External Subroutines ..
      EXTERNAL          DLACPY, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         MAX
C     .. Executable Statements ..
C
      INFO = 0
C
C     Test the input scalar arguments.
C
      IF( NH1.LT.0 ) THEN
         INFO = -1
      ELSE IF( NH2.LT.0 ) THEN
         INFO = -2
      ELSE IF( NR.LT.0 ) THEN
         INFO = -3
      ELSE IF( NC.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDH.LT.MAX( 1, NH1 ) ) THEN
         INFO = -6
      ELSE IF( LDT.LT.MAX( 1, NH1*NR ) ) THEN
         INFO = -8
      END IF
C
      IF ( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'TF01OD', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF ( MAX( NH1, NH2, NR, NC ).EQ.0 )
     $   RETURN
C
C     Construct the first block column of T.
C
      IH = 1
      NROW = (NR-1)*NH1
C
      DO 10 IT = 1, NROW+NH1, NH1
         CALL DLACPY( 'Full', NH1, NH2, H(1,IH), LDH, T(IT,1), LDT )
         IH = IH + NH2
   10 CONTINUE
C
C     Construct the remaining block columns of T.
C
      DO 20 JT = NH2+1, NC*NH2, NH2
         CALL DLACPY( 'Full', NROW, NH2, T(NH1+1,JT-NH2), LDT, T(1,JT),
     $                LDT )
         CALL DLACPY( 'Full', NH1, NH2, H(1,IH), LDH, T(NROW+1,JT),
     $                LDT )
         IH = IH + NH2
   20 CONTINUE
C
      RETURN
C *** Last line of TF01OD ***
      END
