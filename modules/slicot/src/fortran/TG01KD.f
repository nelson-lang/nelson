      SUBROUTINE TG01KD( JOBE, COMPC, COMPQ, COMPZ, N, A, LDA, E, LDE,
     $                   B, C, INCC, Q, LDQ, Z, LDZ, INFO )
C
C     PURPOSE
C
C     To compute for a single-input single-output descriptor system,
C     (A, E, B, C), with E upper triangular, a transformed system,
C     (Q'*A*Z, Q'*E*Z, Q'*B, C*Z), via an orthogonal equivalence
C     transformation, so that Q'*B has only the first element nonzero
C     and Q'*E*Z remains upper triangular.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOBE    CHARACTER*1
C             Specifies whether E is an upper triangular or an identity
C             matrix, as follows:
C             = 'U':  The matrix E is an upper triangular matrix;
C             = 'I':  The matrix E is assumed identity and is not given.
C
C     COMPC   CHARACTER*1
C             Indicates whether the user wishes to transform the system
C             output matrix C, as follows:
C             = 'C':  Transform the system output matrix C;
C             = 'N':  Do not transform the system output matrix C.
C
C     COMPQ   CHARACTER*1
C             Indicates whether the user wishes to accumulate in a
C             matrix Q the orthogonal row transformations, as follows:
C             = 'N':  Do not form Q;
C             = 'I':  Q is initialized to the unit matrix and the
C                     orthogonal transformation matrix Q is returned;
C             = 'U':  The given matrix Q is updated by the orthogonal
C                     transformations used.
C
C     COMPZ   CHARACTER*1
C             Indicates whether the user wishes to accumulate in a
C             matrix Z the orthogonal column transformations, as
C             follows:
C             = 'N':  Do not form Z;
C             = 'I':  Z is initialized to the unit matrix and the
C                     orthogonal transformation matrix Z is returned;
C             = 'U':  The given matrix Z is updated by the orthogonal
C                     transformations used.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The dimension of the descriptor state vector; also the
C             order of square matrices A and E, the number of rows of
C             matrix B, and the number of columns of matrix C.  N >= 0.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the original state matrix A.
C             On exit, the leading N-by-N part of this array contains
C             the transformed state matrix Q'*A*Z.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1,N).
C
C     E       (input/output) DOUBLE PRECISION array, dimension (LDE,*)
C             On entry, if JOBE = 'U', the leading N-by-N upper
C             triangular part of this array must contain the upper
C             triangular part of the descriptor matrix E. The lower
C             triangular part under the first subdiagonal is not
C             referenced.
C             On exit, if JOBE = 'U', the leading N-by-N upper
C             triangular part of this array contains the upper
C             triangular part of the transformed descriptor matrix,
C             Q'*E*Z.
C             If JOBE = 'I', this array is not referenced.
C
C     LDE     INTEGER
C             The leading dimension of the array E.
C             LDE >= MAX(1,N), if JOBE = 'U';
C             LDE >= 1,        if JOBE = 'I'.
C
C     B       (input/output) DOUBLE PRECISION array, dimension (N)
C             On entry, the leading N part of this array must contain
C             the original input matrix B.
C             On exit, the leading N part of this array contains the
C             transformed input matrix Q'*B with all elements but the
C             first set to zero.
C
C     C       (input/output) DOUBLE PRECISION array, dimension
C             ((N-1)*INCC+1)
C             On entry, if COMPC = 'C', the elements 1, INCC+1, ...,
C             (N-1)*INCC+1 of this array must contain the original
C             output vector C.
C             On exit, if COMPC = 'C', the elements 1, INCC+1, ...,
C             (N-1)*INCC+1 of this array contain the transformed output
C             vector C*Z.
C             If COMPC = 'N', this array is not referenced.
C
C     INCC    INTEGER
C             If COMPC = 'C', the increment between successive values
C             of C.  INCC > 0.
C             If COMPC = 'N', INCC is not used.
C
C     Q       (input/output) DOUBLE PRECISION array, dimension (LDQ,*)
C             On entry, if COMPQ = 'U', the leading N-by-N part of this
C             array must contain the given matrix Q1. Otherwise, this
C             array need not be set on input.
C             On exit, if COMPU <> 'N', the leading N-by-N part of this
C             array contains the orthogonal transformation matrix used
C             (Q1*Q if COMPQ = 'U').
C             If COMPQ = 'N', this array is not referenced.
C
C     LDQ     INTEGER
C             The leading dimension of the array Q.
C             LDQ >= 1,        if COMPQ =  'N';
C             LDQ >= max(1,N), if COMPQ <> 'N'.
C
C     Z       (input/output) DOUBLE PRECISION array, dimension (LDZ,*)
C             On entry, if COMPZ = 'U', the leading N-by-N part of this
C             array must contain the given matrix Z1. Otherwise, this
C             array need not be set on input.
C             On exit, if COMPZ <> 'N', the leading N-by-N part of this
C             array contains the orthogonal transformation matrix used
C             (Z1*Z if COMPZ = 'U').
C             If COMPZ = 'N', this array is not referenced.
C
C     LDZ     INTEGER
C             The leading dimension of the array Z.
C             LDZ >= 1,        if COMPZ =  'N';
C             LDZ >= max(1,N), if COMPZ <> 'N'.
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
C     Givens rotations are used to annihilate the last N-1 elements of B
C     in reverse order, but preserve the form of E.
C
C     NUMERICAL ASPECTS
C
C     The algorithm is numerically backward stable.
C
C     CONTRIBUTOR
C
C     V. Sima, Research Institute for Informatics, Bucharest, Aug. 2020.
C
C     REVISIONS
C
C     V. Sima, April 2021, May 2021.
C
C     KEYWORDS
C
C     Controllability, orthogonal transformation.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION  ONE, ZERO
      PARAMETER         ( ONE = 1.0D0, ZERO = 0.0D0 )
C     .. Scalar Arguments ..
      CHARACTER         COMPC, COMPQ, COMPZ, JOBE
      INTEGER           INCC, INFO, LDA, LDE, LDQ, LDZ, N
C     .. Array Arguments ..
      DOUBLE PRECISION  A(LDA,*), B(*), C(*), E(LDE,*), Q(LDQ,*),
     $                  Z(LDZ,*)
C     .. Local Scalars ..
      LOGICAL           LINIQ, LINIZ, LUPDQ, LUPDZ, UNITE, WITHC, WITHQ,
     $                  WITHZ
      INTEGER           IC, K
      DOUBLE PRECISION  CS, SN, TEMP
C     .. External Functions ..
      LOGICAL           LSAME
      EXTERNAL          LSAME
C     .. External Subroutines ..
      EXTERNAL          DLACPY, DLARTG, DLASET, DROT, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         MAX
C     .. Executable Statements ..
C
      UNITE = LSAME( JOBE,  'I' )
      WITHC = LSAME( COMPC, 'C' )
      LINIQ = LSAME( COMPQ, 'I' )
      LUPDQ = LSAME( COMPQ, 'U' )
      LINIZ = LSAME( COMPZ, 'I' )
      LUPDZ = LSAME( COMPZ, 'U' )
      WITHQ = LINIQ .OR. LUPDQ
      WITHZ = LINIZ .OR. LUPDZ
      INFO  = 0
C
C     Test the input scalar arguments.
C
      IF (      .NOT.UNITE .AND. .NOT.LSAME( JOBE,  'U' ) ) THEN
         INFO = -1
      ELSE IF ( .NOT.WITHC .AND. .NOT.LSAME( COMPC, 'N' ) ) THEN
         INFO = -2
      ELSE IF ( .NOT.WITHQ .AND. .NOT.LSAME( COMPQ, 'N' ) ) THEN
         INFO = -3
      ELSE IF ( .NOT.WITHZ .AND. .NOT.LSAME( COMPZ, 'N' ) ) THEN
         INFO = -4
      ELSE IF ( N.LT.0 ) THEN
         INFO = -5
      ELSE IF ( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF ( LDE.LT.1 .OR. ( .NOT.UNITE .AND. LDE.LT.MAX( 1, N ) ) )
     $      THEN
         INFO = -9
      ELSE IF ( WITHC .AND. INCC.LE.0 ) THEN
         INFO = -12
      ELSE IF ( LDQ.LT.1 .OR. ( WITHQ .AND. LDQ.LT.MAX( 1, N ) ) ) THEN
         INFO = -14
      ELSE IF ( LDZ.LT.1 .OR. ( WITHZ .AND. LDZ.LT.MAX( 1, N ) ) ) THEN
         INFO = -16
      END IF
C
      IF( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'TG01KD', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( N.EQ.0 )
     $   RETURN
C
      IF ( LINIQ .OR. ( N.EQ.1 .AND. .NOT.LUPDQ ) )
     $   CALL DLASET( 'Full', N, N, ZERO, ONE, Q, LDQ )
      IF ( LINIZ .OR. ( N.EQ.1 .AND. .NOT.LUPDQ ) )
     $   CALL DLASET( 'Full', N, N, ZERO, ONE, Z, LDZ )
      IF( N.EQ.1 )
     $   RETURN
C
      IF ( WITHC )
     $   IC = ( N - 1 )*INCC + 1
C
      DO 10 K = N, 2, -1
         IF ( B(K).NE.ZERO ) THEN
            CALL DLARTG( B(K-1), B(K), CS, SN, TEMP )
            B(K-1) = TEMP
            B(K)   = ZERO
            CALL DROT( N, A(K-1,1), LDA, A(K,1), LDA, CS, SN )
            IF ( WITHQ )
     $         CALL DROT( N, Q(1,K-1), 1, Q(1,K), 1, CS, SN )
            IF ( UNITE ) THEN
               CALL DROT( N, A(1,K-1), 1, A(1,K), 1, CS, SN )
               IF ( WITHC ) THEN
                  TEMP  = C(IC)*SN + C(IC-INCC)*CS
                  C(IC) = C(IC)*CS - C(IC-INCC)*SN
                  IC    = IC - INCC
                  C(IC) = TEMP
               END IF
               IF ( WITHZ ) THEN
                  IF ( WITHQ .AND. ( LINIQ.EQV.LINIZ .OR.
     $                               LUPDQ.EQV.LUPDZ ) ) THEN
                     CALL DLACPY( 'Full', N, 2, Q(1,K-1), LDQ, Z(1,K-1),
     $                            LDZ )
                  ELSE
                     CALL DROT( N, Z(1,K-1), 1, Z(1,K), 1, CS, SN )
                  END IF
               END IF
            ELSE
               E(K,K-1)   = SN*E(K-1,K-1)
               E(K-1,K-1) = CS*E(K-1,K-1)
               CALL DROT( N-K+1, E(K-1,K), LDE, E(K,K), LDE, CS, SN )
               IF ( E(K,K-1).NE.ZERO ) THEN
                  CALL DLARTG( E(K,K), E(K,K-1), CS, SN, TEMP )
                  E(K,K)     = TEMP
                  E(K,K-1)   = ZERO
                  TEMP       = E(K-1,K)*SN + E(K-1,K-1)*CS
                  E(K-1,K)   = E(K-1,K)*CS - E(K-1,K-1)*SN
                  E(K-1,K-1) = TEMP
                  CALL DROT( K-2, E(1,K-1), 1, E(1,K), 1, CS, SN )
                  CALL DROT(   N, A(1,K-1), 1, A(1,K), 1, CS, SN )
                  IF ( WITHC ) THEN
                     TEMP  = C(IC)*SN + C(IC-INCC)*CS
                     C(IC) = C(IC)*CS - C(IC-INCC)*SN
                     IC    = IC - INCC
                     C(IC) = TEMP
                  END IF
                  IF ( WITHZ )
     $               CALL DROT( N, Z(1,K-1), 1, Z(1,K), 1, CS, SN )
               END IF
            END IF
         END IF
   10 CONTINUE
C
      RETURN
C *** Last line of TG01KD ***
      END
