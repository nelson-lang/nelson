      SUBROUTINE MB03QW( N, L, A, LDA, E, LDE, U, LDU, V, LDV,
     $                   ALPHAR, ALPHAI, BETA, INFO )
C
C     PURPOSE
C
C     To compute the eigenvalues of a selected 2-by-2 diagonal block
C     pair of an upper quasi-triangular pencil, to reduce the selected
C     block pair to the standard form and to split it in the case of
C     real eigenvalues, by constructing orthogonal matrices UT and VT.
C     The transformations UT and VT are applied to the pair (A,E) by
C     computing (UT'*A*VT, UT'*E*VT ), to the matrices U and V,
C     by computing U*UT and V*VT.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the matrices A, E, UT and VT.  N >= 2.
C
C     L       (input) INTEGER
C             Specifies the position of the block.  1 <= L < N.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the upper quasi-triangular matrix A whose
C             selected 2-by-2 diagonal block is to be processed.
C             On exit, the leading N-by-N part of this array contains
C             the upper quasi-triangular matrix A after its selected
C             block has been split and/or put in the LAPACK standard
C             form.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= N.
C
C     E       (input/output) DOUBLE PRECISION array, dimension (LDE,N)
C             On entry, the leading N-by-N part of this array must
C             contain the upper triangular matrix E whose selected
C             2-by-2 diagonal block is to be processed.
C             On exit, the leading N-by-N part of this array contains
C             the transformed upper triangular matrix E (in the LAPACK
C             standard form).
C
C     LDE     INTEGER
C             The leading dimension of the array E.  LDE >= N.
C
C     U       (input/output) DOUBLE PRECISION array, dimension (LDU,N)
C             On entry, the leading N-by-N part of this array must
C             contain a transformation matrix U.
C             On exit, the leading N-by-N part of this array contains
C             U*UT, where UT is the transformation matrix used to
C             split and/or standardize the selected block pair.
C
C     LDU     INTEGER
C             The leading dimension of the array U.  LDU >= N.
C
C     V       (input/output) DOUBLE PRECISION array, dimension (LDV,N)
C             On entry, the leading N-by-N part of this array must
C             contain a transformation matrix V.
C             On exit, the leading N-by-N part of this array contains
C             V*VT, where VT is the transformation matrix used to
C             split and/or standardize the selected block pair.
C
C     LDV     INTEGER
C             The leading dimension of the array V.  LDV >= N.
C
C     ALPHAR  (output) DOUBLE PRECISION array, dimension (2)
C     ALPHAI  (output) DOUBLE PRECISION array, dimension (2)
C     BETA    (output) DOUBLE PRECISION array, dimension (2)
C             (ALPHAR(k)+i*ALPHAI(k))/BETA(k) are the eigenvalues of the
C             block pair of the pencil (A,B), k=1,2, i = sqrt(-1).
C             Note that BETA(k) may be zero.
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
C     Let A1 = ( A(L,L)    A(L,L+1)   ), E1 = ( E(L,L)    E(L,L+1)   ),
C              ( A(L+1,L)  A(L+1,L+1) )       ( E(L+1,L)  E(L+1,L+1) )
C     be the specified 2-by-2 diagonal block pair of the pencil (A,B).
C     If the eigenvalues of (A1,E1) are complex, then they are computed
C     and stored in ER and EI, where the real part is stored in ER and
C     the positive imaginary part in EI. The 2-by-2 block pair is
C     reduced if necessary to the standard form, such that
C     A(L,L) = A(L+1,L+1), and A(L,L+1) and A(L+1,L) have oposite signs.
C     If the eigenvalues are real, the 2-by-2 block pair is reduced to
C     upper triangular form such that ABS(A(L,L)) >= ABS(A(L+1,L+1)).
C     In both cases, an orthogonal rotation U1' is constructed such that
C     U1'*A1*U1 has the appropriate form. Let UT be an extension of U1
C     to an N-by-N orthogonal matrix, using identity submatrices. Then A
C     and E are replaced by UT'*A*UT and UT'*E*UT, and the contents of
C     the arrays U and V are U * UT and V * VT, respectively.
C
C     CONTRIBUTOR
C
C     A. Varga, German Aerospace Center, DLR Oberpfaffenhofen,
C     March 1998. Based on the RASP routine SPLITB.
C
C     REVISIONS
C
C     V. Sima, Dec. 2016.
C
C     KEYWORDS
C
C     Eigenvalues, orthogonal transformation, real Schur form,
C     equivalence transformation.
C
C     ******************************************************************
C
C     .. Scalar Arguments ..
      INTEGER          INFO, L, LDA, LDE, LDU, LDV, N
      DOUBLE PRECISION ALPHAI(*), ALPHAR(*), BETA(*)
C     .. Array Arguments ..
      DOUBLE PRECISION A(LDA,*), E(LDE,*), U(LDU,*), V(LDV,*)
C     .. Local Scalars ..
      INTEGER          L1
      DOUBLE PRECISION CSL, CSR, SNL, SNR
C     .. External Subroutines ..
      EXTERNAL         DLAGV2, DROT, XERBLA
C     .. Executable Statements ..
C
      INFO = 0
C
C     Test the input scalar arguments.
C
      IF( N.LT.2 ) THEN
         INFO = -1
      ELSE IF( L.LT.1 .OR. L.GE.N ) THEN
         INFO = -2
      ELSE IF( LDA.LT.N ) THEN
         INFO = -4
      ELSE IF( LDE.LT.N ) THEN
         INFO = -6
      ELSE IF( LDU.LT.N ) THEN
         INFO = -8
      ELSE IF( LDV.LT.N ) THEN
         INFO = -10
      END IF
C
      IF( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'MB03QW', -INFO )
         RETURN
      END IF
C
C     Compute the generalized eigenvalues and the elements of the Givens
C     transformations.
C
      L1 = L + 1
      CALL DLAGV2( A(L,L), LDA, E(L,L), LDE, ALPHAR, ALPHAI, BETA,
     $             CSL, SNL, CSR, SNR )
C
C     Apply the transformations to A and E.
C
      IF( L1.LT.N ) THEN
         CALL DROT( N-L1, A(L,L1+1), LDA, A(L1,L1+1), LDA, CSL, SNL )
         CALL DROT( N-L1, E(L,L1+1), LDE, E(L1,L1+1), LDE, CSL, SNL )
      END IF
      CALL DROT( L-1, A(1,L), 1, A(1,L1), 1, CSR, SNR )
      CALL DROT( L-1, E(1,L), 1, E(1,L1), 1, CSR, SNR )
C
C     Accumulate the transformations in U and V.
C
      CALL DROT( N, U(1,L), 1, U(1,L1), 1, CSL, SNL )
      CALL DROT( N, V(1,L), 1, V(1,L1), 1, CSR, SNR )
C
      RETURN
C *** Last line of MB03QW ***
      END
