      SUBROUTINE MB03ID( COMPQ, COMPU, N, A, LDA, C, LDC, D, LDD, B,
     $                   LDB, F, LDF, Q, LDQ, U1, LDU1, U2, LDU2, NEIG,
     $                   IWORK, LIWORK, DWORK, LDWORK, INFO )
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
C     To move the eigenvalues with strictly negative real parts of an
C     N-by-N real skew-Hamiltonian/Hamiltonian pencil aS - bH in
C     structured Schur form, with
C
C                          (  0  I  )      (  A  D  )      (  B  F  )
C       S = J Z' J' Z, J = (        ), Z = (        ), H = (        ),
C                          ( -I  0  )      (  0  C  )      (  0 -B' )
C
C     to the leading principal subpencil, while keeping the triangular
C     form. Above, A is upper triangular, B is upper quasi-triangular,
C     and C is lower triangular.
C     The matrices Z and H are transformed by an orthogonal symplectic
C     matrix U and an orthogonal matrix Q such that
C
C                       (  Aout  Dout  )
C       Zout = U' Z Q = (              ), and
C                       (    0   Cout  )
C                                                                    (1)
C                            (  Bout  Fout  )
C       Hout = J Q' J' H Q = (              ),
C                            (    0  -Bout' )
C
C     where Aout, Bout and Cout remain in triangular form.
C     Optionally, if COMPQ = 'I' or COMPQ = 'U', the orthogonal matrix Q
C     that fulfills (1) is computed.
C     Optionally, if COMPU = 'I' or COMPU = 'U', the orthogonal
C     symplectic matrix
C
C           (  U1  U2  )
C       U = (          )
C           ( -U2  U1  )
C
C     that fulfills (1) is computed.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     COMPQ   CHARACTER*1
C             Specifies whether or not the orthogonal transformations
C             should be accumulated in the array Q, as follows:
C             = 'N':  Q is not computed;
C             = 'I':  the array Q is initialized internally to the unit
C                     matrix, and the orthogonal matrix Q is returned;
C             = 'U':  the array Q contains an orthogonal matrix Q0 on
C                     entry, and the matrix Q0*Q is returned, where Q
C                     is the product of the orthogonal transformations
C                     that are applied to the pencil aS - bH to reorder
C                     the eigenvalues.
C
C     COMPU   CHARACTER*1
C             Specifies whether or not the orthogonal symplectic
C             transformations should be accumulated in the arrays U1 and
C             U2, as follows:
C             = 'N':  U1 and U2 are not computed;
C             = 'I':  the arrays U1 and U2 are initialized internally,
C                     and the submatrices U1 and U2 defining the
C                     orthogonal symplectic matrix U are returned;
C             = 'U':  the arrays U1 and U2 contain the corresponding
C                     submatrices of an orthogonal symplectic matrix U0
C                     on entry, and the updated submatrices U1 and U2
C                     of the matrix product U0*U are returned, where U
C                     is the product of the orthogonal symplectic
C                     transformations that are applied to the pencil
C                     aS - bH to reorder the eigenvalues.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the pencil aS - bH.  N >= 0, even.
C
C     A       (input/output) DOUBLE PRECISION array, dimension
C                            (LDA, N/2)
C             On entry, the leading N/2-by-N/2 part of this array must
C             contain the upper triangular matrix A. The elements of the
C             strictly lower triangular part of this array are not used.
C             On exit, the leading  N/2-by-N/2 part of this array
C             contains the transformed matrix Aout.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= MAX(1, N/2).
C
C     C       (input/output) DOUBLE PRECISION array, dimension
C                            (LDC, N/2)
C             On entry, the leading N/2-by-N/2 part of this array must
C             contain the lower triangular matrix C. The elements of the
C             strictly upper triangular part of this array are not used.
C             On exit, the leading  N/2-by-N/2 part of this array
C             contains the transformed matrix Cout.
C
C     LDC     INTEGER
C             The leading dimension of the array C.  LDC >= MAX(1, N/2).
C
C     D       (input/output) DOUBLE PRECISION array, dimension
C                            (LDD, N/2)
C             On entry, the leading N/2-by-N/2 part of this array must
C             contain the matrix D.
C             On exit, the leading  N/2-by-N/2 part of this array
C             contains the transformed matrix Dout.
C
C     LDD     INTEGER
C             The leading dimension of the array D.  LDD >= MAX(1, N/2).
C
C     B       (input/output) DOUBLE PRECISION array, dimension
C                            (LDB, N/2)
C             On entry, the leading N/2-by-N/2 part of this array must
C             contain the upper quasi-triangular matrix B.
C             On exit, the leading  N/2-by-N/2 part of this array
C             contains the transformed upper quasi-triangular part of
C             the matrix Bout.
C             The part below the first subdiagonal of this array is
C             not referenced.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= MAX(1, N/2).
C
C     F       (input/output) DOUBLE PRECISION array, dimension
C                            (LDF, N/2)
C             On entry, the leading N/2-by-N/2 part of this array must
C             contain the upper triangular part of the symmetric matrix
C             F.
C             On exit, the leading  N/2-by-N/2 part of this array
C             contains the transformed upper triangular part of the
C             matrix Fout.
C             The strictly lower triangular part of this array is not
C             referenced, except for the element F(N/2,N/2-1), but its
C             initial value is preserved.
C
C     LDF     INTEGER
C             The leading dimension of the array F.  LDF >= MAX(1, N/2).
C
C     Q       (input/output) DOUBLE PRECISION array, dimension (LDQ, N)
C             On entry, if COMPQ = 'U', then the leading N-by-N part of
C             this array must contain a given matrix Q0, and on exit,
C             the leading N-by-N part of this array contains the product
C             of the input matrix Q0 and the transformation matrix Q
C             used to transform the matrices S and H.
C             On exit, if COMPQ = 'I', then the leading N-by-N part of
C             this array contains the orthogonal transformation matrix
C             Q.
C             If COMPQ = 'N' this array is not referenced.
C
C     LDQ     INTEGER
C             The leading dimension of of the array Q.
C             LDQ >= 1,         if COMPQ = 'N';
C             LDQ >= MAX(1, N), if COMPQ = 'I' or COMPQ = 'U'.
C
C     U1      (input/output) DOUBLE PRECISION array, dimension
C                            (LDU1, N/2)
C             On entry, if COMPU = 'U', then the leading N/2-by-N/2 part
C             of this array must contain the upper left block of a
C             given matrix U0, and on exit, the leading N/2-by-N/2 part
C             of this array contains the updated upper left block U1 of
C             the product of the input matrix U0 and the transformation
C             matrix U used to transform the matrices S and H.
C             On exit, if COMPU = 'I', then the leading N/2-by-N/2 part
C             of this array contains the upper left block U1 of the
C             orthogonal symplectic transformation matrix U.
C             If COMPU = 'N' this array is not referenced.
C
C     LDU1    INTEGER
C             The leading dimension of the array U1.
C             LDU1 >= 1,           if COMPU = 'N';
C             LDU1 >= MAX(1, N/2), if COMPU = 'I' or COMPU = 'U'.
C
C     U2      (input/output) DOUBLE PRECISION array, dimension
C                            (LDU2, N/2)
C             On entry, if COMPU = 'U', then the leading N/2-by-N/2 part
C             of this array must contain the upper right block of a
C             given matrix U0, and on exit, the leading N/2-by-N/2 part
C             of this array contains the updated upper right block U2 of
C             the product of the input matrix U0 and the transformation
C             matrix U used to transform the matrices S and H.
C             On exit, if COMPU = 'I', then the leading N/2-by-N/2 part
C             of this array contains the upper right block U2 of the
C             orthogonal symplectic transformation matrix U.
C             If COMPU = 'N' this array is not referenced.
C
C     LDU2    INTEGER
C             The leading dimension of the array U2.
C             LDU2 >= 1,           if COMPU = 'N';
C             LDU2 >= MAX(1, N/2), if COMPU = 'U' or COMPU = 'I'.
C
C     NEIG    (output) INTEGER
C             The number of eigenvalues in aS - bH with strictly
C             negative real part.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (LIWORK)
C
C     LIWORK  INTEGER
C             The dimension of the array IWORK.
C             LIWORK >= N+1.
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C
C     LDWORK  INTEGER
C             The dimension of the array DWORK.
C             If COMPQ = 'N',
C                LDWORK >= MAX(2*N+48,171);
C             if COMPQ = 'I' or COMPQ = 'U',
C                LDWORK >= MAX(4*N+48,171).
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0: succesful exit;
C             < 0: if INFO = -i, the i-th argument had an illegal value;
C             = 1: the periodic QZ algorithm did not converge in SLICOT
C                  Library routine MB03BB;
C             = 2: an error occured during the execution of MB03CD;
C             = 3: an error occured during the execution of MB03GD.
C
C     METHOD
C
C     The algorithm reorders the eigenvalues like the following scheme:
C
C     Step 1: Reorder the eigenvalues in the subpencil aA - bB.
C          I. Reorder the eigenvalues with negative real parts to the
C             top.
C         II. Reorder the eigenvalues with positive real parts to the
C             bottom.
C
C     Step 2: Reorder the remaining eigenvalues with negative real
C             parts in the pencil aS - bH.
C          I. Exchange the eigenvalues between the last diagonal block
C             in aA - bB and the last diagonal block in aS - bH.
C         II. Move the eigenvalues of the R-th block to the (MM+1)-th
C             block, where R denotes the number of upper quasi-
C             triangular blocks in aA - bB and MM denotes the current
C             number of blocks in aA - bB with eigenvalues with negative
C             real parts.
C
C     The algorithm uses a sequence of orthogonal transformations as
C     described on page 25 in [1]. To achieve those transformations the
C     elementary subroutines MB03CD and MB03GD are called for the
C     corresponding matrix structures.
C
C     REFERENCES
C
C     [1] Benner, P., Byers, R., Losse, P., Mehrmann, V. and Xu, H.
C         Numerical Solution of Real Skew-Hamiltonian/Hamiltonian
C         Eigenproblems.
C         Tech. Rep., Technical University Chemnitz, Germany,
C         Nov. 2007.
C
C     NUMERICAL ASPECTS
C                                                               3
C     The algorithm is numerically backward stable and needs O(N ) real
C     floating point operations.
C
C     CONTRIBUTOR
C
C     Matthias Voigt, Fakultaet fuer Mathematik, Technische Universitaet
C     Chemnitz, November 21, 2008.
C     V. Sima, Dec. 2009 (SLICOT version of the routine DHAFNX).
C
C     REVISIONS
C
C     V. Sima, Aug. 2009; Feb. 2010; Oct. 2010; Nov. 2010.
C
C     KEYWORDS
C
C     Eigenvalue reordering, upper (quasi-)triangular matrix,
C     skew-Hamiltonian/Hamiltonian pencil, structured Schur form.
C
C     ******************************************************************
C
C     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, HALF, TEN
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, HALF = 0.5D+0,
     $                     TEN = 1.0D+1 )
C
C     .. Scalar Arguments ..
      CHARACTER          COMPQ, COMPU
      INTEGER            INFO, LDA, LDB, LDC, LDD, LDF, LDQ, LDU1, LDU2,
     $                   LDWORK, LIWORK, N, NEIG
C
C     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * ),
     $                   D( LDD, * ), DWORK( *  ), F( LDF, * ),
     $                   Q( LDQ, * ), U1( LDU1, * ), U2( LDU2, * )
C
C     .. Local Scalars ..
      LOGICAL            LCMPQ, LCMPU, LINIQ, LINIU, LUPDQ, LUPDU
      INTEGER            DIM1, DIM2, HLP, I, I1, IA, IB, IB1, IB2, IB3,
     $                   IBS, IC, IHUPLE, IQ1, IQ2, IQ3, IQLOLE, IQLORI,
     $                   IQUPLE, IQUPRI, IR, IS, ITMP1, ITMP2, ITMP3,
     $                   IUPD, IUUPLE, IUUPRI, IWRK1, IWRK2, IWRK3,
     $                   IWRK4, IWRK5, IZLORI, IZUPLE, IZUPRI, J, K,
     $                   LDW, M, MM, MP, NCOL, NCOLS, NROW, NROWS,
     $                   OPTDW, R, SDIM, UPDS
      DOUBLE PRECISION   A2, BASE, C2, F2, LGBAS, NRMB, PREC, Q11, Q12,
     $                   Q21, Q22, TMPA, TMPC, TOL, U11, U12
C
C     .. Local Arrays ..
      INTEGER            IDUM( 8 )
      DOUBLE PRECISION   DUM( 3, 4 ), PAR( 2 ), PRD( 2, 2, 3 )
C
C     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            MA01CD
      DOUBLE PRECISION   DLAMCH, DLANHS
      EXTERNAL           DLAMCH, DLANHS, LSAME, MA01CD
C
C     .. External Subroutines ..
      EXTERNAL           DAXPY, DCOPY, DGEMM, DLACPY, DLASET, DSCAL,
     $                   MA02AD, MB01RU, MB01RX, MB03BB, MB03CD, MB03GD,
     $                   XERBLA
C
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, LOG, MAX, MIN, MOD, SIGN
C
C     .. Executable Statements ..
C
C     Decode the input arguments.
C
      M     = N/2
      LINIQ = LSAME( COMPQ, 'I' )
      LUPDQ = LSAME( COMPQ, 'U' )
      LINIU = LSAME( COMPU, 'I' )
      LUPDU = LSAME( COMPU, 'U' )
      LCMPQ = LINIQ .OR. LUPDQ
      LCMPU = LINIU .OR. LUPDU
C
      IF( LCMPQ ) THEN
         OPTDW = MAX( 4*N + 48, 171 )
      ELSE
         OPTDW = MAX( 2*N + 48, 171 )
      END IF
C
C     Test the input arguments.
C
      INFO = 0
      IF( .NOT.( LSAME( COMPQ, 'N' ) .OR. LCMPQ ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( LSAME( COMPU, 'N' ) .OR. LCMPU ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 .OR. MOD( N, 2 ).NE.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -7
      ELSE IF( LDD.LT.MAX( 1, M ) ) THEN
         INFO = -9
      ELSE IF( LDB.LT.MAX( 1, M ) ) THEN
         INFO = -11
      ELSE IF( LDF.LT.MAX( 1, M ) ) THEN
         INFO = -13
      ELSE IF( LDQ.LT.1  .OR. ( LCMPQ .AND.  LDQ.LT.N ) ) THEN
         INFO = -15
      ELSE IF( LDU1.LT.1 .OR. ( LCMPU .AND. LDU1.LT.M ) ) THEN
         INFO = -17
      ELSE IF( LDU2.LT.1 .OR. ( LCMPU .AND. LDU2.LT.M ) ) THEN
         INFO = -19
      ELSE IF( LIWORK.LT.N+1 ) THEN
         INFO = -22
      ELSE IF( LDWORK.LT.OPTDW ) THEN
         INFO = -24
      END IF
      IF( INFO.NE.0) THEN
         CALL XERBLA( 'MB03ID', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( N.EQ.0 ) THEN
         NEIG = 0
         RETURN
      END IF
C
C     Determine machine constants.
C
      PREC  = DLAMCH( 'Precision' )
      BASE  = DLAMCH( 'Base' )
      LGBAS = LOG( BASE )
      TOL   = MIN( DBLE( M ), TEN )*PREC
C
      PAR( 1 ) = PREC
      PAR( 2 ) = DLAMCH( 'Safe minimum' )
C
C     STEP 0: Determine location and size of diagonal blocks.
C             IWORK(J) and IWORK(IS+J) are used to indicate the
C             beginning index and the kind of eigenvalues of the
C             J-th diagonal block of the subpencil aA - bB.
C             To find IWORK(IS+J) for the block J of size dim, compute
C                                      -T                      -1
C                sign( trace(C(rng,rng)  *B(rng,rng)*A(rng,rng)  ) ),
C
C             where rng = J:J+dim-1. For dim = 2, it is assumed that
C             both eigenvalues of the matrix above have real parts with
C             the same sign (true for a structured Schur form).
C
      I  = 1
      J  = 1
      IS = M + 1
C
C     Partition blocks.
C
      NRMB = DLANHS( 'One', M, B, LDB, DWORK )
C
C     WHILE( I.LE.M-1 ) DO
C
   10 CONTINUE
      IF( I.LE.M-1 ) THEN
         IWORK( J ) = I
         IF( ABS( B( I+1, I ) ).LE.TOL*NRMB ) THEN
C
C           1-by-1 block.
C
            B( I+1, I )   = ZERO
            IWORK( IS+J ) = SIGN( ONE, A( I, I )*B( I, I )*C( I, I ) )
            I = I + 1
         ELSE
C
C           2-by-2 block.
C
            U11  = B( I+1, I )*A( I, I+1 )
            U12  = B( I+1, I )*C( I+1, I )
            TMPA = B( I+1, I+1 )*A( I, I ) - U11
            TMPC = B( I, I )*C( I+1, I+1 ) - U12
            IF( ABS( TMPA ).LE.PREC*ABS( U11 ) .AND.
     $          ABS( TMPC ).LE.PREC*ABS( U12 ) ) THEN
C
C              Severe cancellation. Use the periodic QZ algorithm.
C              Workspace: 30.
C
               IDUM( 1 ) =  1
               IDUM( 2 ) =  2
               IDUM( 3 ) =  3
               IDUM( 4 ) =  1
               IDUM( 5 ) = -1
               IDUM( 6 ) = -1
               CALL DLACPY( 'Full',  2, 2, B( I, I ), LDB, PRD, 2 )
               CALL DLACPY( 'Upper', 2, 2, A( I, I ), LDA,
     $                      PRD( 1, 1, 2 ), 2 )
               CALL MA02AD( 'Lower', 2, 2, C( I, I ), LDC,
     $                      PRD( 1, 1, 3 ), 2 )
               PRD( 2, 1, 2 ) = ZERO
               PRD( 2, 1, 3 ) = ZERO
               CALL MB03BB( BASE, LGBAS, PREC, 3, IDUM, IDUM( 4 ), 1,
     $                      PRD, 2, 2, DWORK, DWORK( 3 ), DWORK( 5 ),
     $                      IDUM( 7 ), DWORK( 7 ), INFO )
               IF( INFO.NE.0 )
     $            RETURN
               IF( DWORK( 5 ).EQ.ZERO .OR. DWORK( 6 ).EQ.ZERO ) THEN
                  IWORK( IS+J ) = 0
               ELSE
                  IWORK( IS+J ) = MA01CD( DWORK( 1 ), IDUM( 7 ),
     $                                    DWORK( 2 ), IDUM( 8 ) )
               END IF
            ELSE IF( C( I, I ).EQ.ZERO .OR. A( I+1, I+1 ).EQ.ZERO ) THEN
C
C              The pencil has infinite eigenvalues or it is singular.
C
               IWORK( IS+J ) = 0
            ELSE
               U11 = TMPA/A( I+1, I+1 ) + TMPC/C( I, I )
               IF( U11.EQ.ZERO ) THEN
                  IWORK( IS+J ) = 0
               ELSE
                  IWORK( IS+J ) = SIGN( ONE, U11 )*
     $                            SIGN( ONE, A( I, I )*C( I+1, I+1 ) )
               END IF
            END IF
            I = I + 2
         END IF
         J = J + 1
      GO TO 10
C
C     END WHILE 10
C
      END IF
C
      IF( I.EQ.M ) THEN
C
C        1-by-1 block
C
         IWORK( J ) = I
         IWORK( IS+J ) = SIGN( ONE, A( I, I )*B( I, I )*C( I, I ) )
         J = J + 1
      END IF
C
      R = J - 1
C
C     Initialize Q if appropriate.
C
      IF( LINIQ ) THEN
         IUPD = M + 1
         UPDS = M
         CALL DLASET( 'Full', N, N, ZERO, ONE, Q, LDQ )
      ELSE IF( LUPDQ ) THEN
         IUPD = 1
         UPDS = N
      END IF
C
C     Initialize U1 and U2 if appropriate.
C
      IF( LINIU ) THEN
         CALL DLASET( 'Full', M, M, ZERO, ONE,  U1, LDU1 )
         CALL DLASET( 'Full', M, M, ZERO, ZERO, U2, LDU2 )
      END IF
C
      IF( M.GT.1 ) THEN
C
C        Save the elements A(M,M-1), C(M-1,M), and F(M,M-1), which might
C        be overwritten.
C
         A2 = A( M, M-1 )
         C2 = C( M-1, M )
         F2 = F( M, M-1 )
      END IF
C
C     STEP 1: Reorder the eigenvalues in the subpencil aA - bB.
C
      MM = 0
      MP = J
C
C     I. Reorder the eigenvalues with negative real parts to the top.
C
C     Set pointers for the inputs and outputs of MB03CD.
C
      IQ1   = 1
      IQ2   = IQ1 + 16
      IQ3   = IQ2 + 16
      IA    = IQ3 + 16
      IB    = IA  + 16
      IC    = IB  + 16
      IWRK1 = IC  + 16
      IWRK2 = IA
C
      K   = 1
      IB3 = M + 1
      IWORK( R+1 ) = IB3
C
C     WHILE( K.LE.R ) DO
C
   20 CONTINUE
      IF( K.LE.R ) THEN
         IF( IWORK( IS+K ).LT.0 ) THEN
            DO 30 J = K - 1, MM + 1, -1
C
C              IB1, IB2, and IB3 are pointers to 3 consecutive blocks.
C
               IB1  = IWORK( J )
               IB2  = IWORK( J+1 )
               IB3  = IWORK( J+2 )
               DIM1 = IB2  - IB1
               DIM2 = IB3  - IB2
               SDIM = DIM1 + DIM2
C
C              Copy the relevant part of A(ib1:ib3-1,ib1:ib3-1),
C              C(ib1:ib3-1,ib1:ib3-1), and B(ib1:ib3-1,ib1:ib3-1) to
C              DWORK as inputs for MB03CD. Also, set the additional
C              zero elements.
C
               CALL DLACPY( 'Upper', SDIM, SDIM, A( IB1, IB1 ), LDA,
     $                      DWORK( IA ), SDIM )
               CALL MA02AD( 'Lower', SDIM, SDIM, C( IB1, IB1 ), LDC,
     $                      DWORK( IC ), SDIM )
               CALL DLACPY( 'Upper', SDIM, SDIM, B( IB1, IB1 ), LDB,
     $                      DWORK( IB ), SDIM )
               CALL DCOPY(  SDIM-1, B( IB1+1, IB1 ), LDB+1,
     $                      DWORK( IB+1 ), SDIM+1 )
               IF( DIM1.EQ.2 ) THEN
                  DWORK( IA+1 ) = ZERO
                  DWORK( IC+1 ) = ZERO
               END IF
               IF( DIM2.EQ.2 ) THEN
                  I1 = SDIM*( SDIM - 1 ) - 1
                  DWORK( IA+I1 ) = ZERO
                  DWORK( IC+I1 ) = ZERO
               END IF
               DWORK( IB+SDIM-1 ) = ZERO
               IF( SDIM.EQ.4 ) THEN
                  DWORK( IB+2 ) = ZERO
                  DWORK( IB+7 ) = ZERO
               END IF
C
C              Perform eigenvalue/matrix block exchange.
C              Workspace: IWRK1 + 16*DIM1 + 10*DIM2 + 22 <= IWRK1 + 74,
C              if SDIM > 2, and IWRK1 - 1, otherwise.
C
               CALL MB03CD( 'Upper', DIM1, DIM2, PREC, DWORK( IC ),
     $                      SDIM, DWORK( IA ),  SDIM,  DWORK( IB ),
     $                      SDIM, DWORK( IQ1 ), SDIM, DWORK( IQ2 ),
     $                      SDIM, DWORK( IQ3 ), SDIM, DWORK( IWRK1 ),
     $                      LDWORK-IWRK1+1, INFO )
               IF( INFO.GT.0 ) THEN
                  INFO = 2
                  RETURN
               END IF
C
C              Copy the transformed diagonal block of B, if sdim > 2.
C
               IF( SDIM.GT.2 ) THEN
                  CALL DLACPY( 'Upper', SDIM, SDIM, DWORK( IB ), SDIM,
     $                         B( IB1, IB1 ), LDB )
                  CALL DCOPY(  SDIM-1, DWORK( IB+1 ), SDIM+1,
     $                         B( IB1+1, IB1 ), LDB+1 )
               END IF
C
               NROWS = IB1 - 1
               NCOLS = M - IB3 + 1
               NROW  = IB3 - 1
               NCOL  = M - IB1 + 1
               CALL DLACPY( 'Lower', SDIM-1, SDIM-1, A( IB1+1, IB1 ),
     $                      LDA, DUM, 3 )
               CALL DLASET( 'Lower', SDIM-1, SDIM-1, ZERO, ZERO,
     $                      A( IB1+1, IB1 ), LDA )
               CALL DLACPY( 'Upper', SDIM-1, SDIM-1, C( IB1, IB1+1 ),
     $                      LDC, DUM( 1, 2 ), 3 )
               CALL DLASET( 'Upper', SDIM-1, SDIM-1, ZERO, ZERO,
     $                      C( IB1, IB1+1 ), LDC )
C
C              Update A.
C              Workspace: IWRK2 + 2*N - 1.
C
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, SDIM,
     $                      SDIM, ONE, A( 1, IB1 ), LDA, DWORK( IQ1 ),
     $                      SDIM, ZERO, DWORK( IWRK2 ), NROW )
               CALL DLACPY( 'Full', NROW, SDIM, DWORK( IWRK2 ), NROW,
     $                      A( 1, IB1 ), LDA )
               CALL DGEMM(  'Transpose', 'No Transpose', SDIM, NCOL,
     $                      SDIM, ONE, DWORK( IQ2 ), SDIM,
     $                      A( IB1, IB1 ), LDA, ZERO, DWORK( IWRK2 ),
     $                      SDIM )
               CALL DLACPY( 'Full', SDIM, NCOL, DWORK( IWRK2 ), SDIM,
     $                      A( IB1, IB1 ), LDA )
               CALL DLACPY( 'Lower', SDIM-1, SDIM-1, DUM, 3,
     $                      A( IB1+1, IB1 ), LDA )
C
C              Update C.
C
               CALL DGEMM(  'No Transpose', 'No Transpose', NCOL, SDIM,
     $                      SDIM, ONE, C( IB1, IB1 ), LDC, DWORK( IQ3 ),
     $                      SDIM, ZERO, DWORK( IWRK2 ), NCOL )
               CALL DLACPY( 'Full', NCOL, SDIM, DWORK( IWRK2 ), NCOL,
     $                      C( IB1, IB1 ), LDC )
               CALL DGEMM(  'Transpose', 'No Transpose', SDIM, NROW,
     $                      SDIM, ONE, DWORK( IQ2 ), SDIM, C( IB1, 1 ),
     $                      LDC, ZERO, DWORK( IWRK2 ), SDIM )
               CALL DLACPY( 'Full', SDIM, NROW, DWORK( IWRK2 ), SDIM,
     $                      C( IB1, 1 ), LDC )
               CALL DLACPY( 'Upper', SDIM-1, SDIM-1, DUM( 1, 2 ), 3,
     $                      C( IB1, IB1+1 ), LDC )
C
C              Update D.
C
               CALL DGEMM(  'No Transpose', 'No Transpose', M, SDIM,
     $                      SDIM, ONE, D( 1, IB1 ), LDD, DWORK( IQ3 ),
     $                      SDIM, ZERO, DWORK( IWRK2 ), M )
               CALL DLACPY( 'Full', M, SDIM, DWORK( IWRK2 ), M,
     $                      D( 1, IB1 ), LDD )
               CALL DGEMM(  'Transpose', 'No Transpose', SDIM, M, SDIM,
     $                      ONE, DWORK( IQ2 ), SDIM, D( IB1, 1 ), LDD,
     $                      ZERO, DWORK( IWRK2 ), SDIM )
               CALL DLACPY( 'Full', SDIM, M, DWORK( IWRK2 ), SDIM,
     $                      D( IB1, 1 ), LDD )
C
C              Update B.
C
               IF( SDIM.GT.2 ) THEN
                  NROW = NROWS
                  NCOL = NCOLS
                  IBS  = IB3
                  LDW  = MAX( 1, NROW )
               ELSE
                  IBS  = IB1
                  LDW  = NROW
               END IF
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, SDIM,
     $                      SDIM, ONE, B( 1, IB1 ), LDB, DWORK( IQ1 ),
     $                      SDIM, ZERO, DWORK( IWRK2 ), LDW )
               CALL DLACPY( 'Full', NROW, SDIM, DWORK( IWRK2 ), LDW,
     $                      B( 1, IB1 ), LDB )
               CALL DGEMM(  'Transpose', 'No Transpose', SDIM, NCOL,
     $                      SDIM, ONE, DWORK( IQ3 ), SDIM,
     $                      B( IB1, IBS ), LDB, ZERO, DWORK( IWRK2 ),
     $                      SDIM )
               CALL DLACPY( 'Full', SDIM, NCOL, DWORK( IWRK2 ), SDIM,
     $                      B( IB1, IBS ), LDB )
C
C              Update F.
C
               CALL DGEMM(  'No Transpose', 'No Transpose', NROWS, SDIM,
     $                      SDIM, ONE, F( 1, IB1 ), LDF, DWORK( IQ3 ),
     $                      SDIM, ZERO, DWORK( IWRK2 ), LDW )
               CALL DLACPY( 'Full', NROWS, SDIM, DWORK( IWRK2 ), LDW,
     $                      F( 1, IB1 ), LDF )
               CALL DGEMM(  'Transpose', 'No Transpose', SDIM, NCOLS,
     $                      SDIM, ONE, DWORK( IQ3 ), SDIM,
     $                      F( IB1, IB3 ), LDF, ZERO, DWORK( IWRK2 ),
     $                      SDIM )
               CALL DLACPY( 'Full', SDIM, NCOLS, DWORK( IWRK2 ), SDIM,
     $                      F( IB1, IB3 ), LDF )
               CALL MB01RU( 'Upper', 'Transpose', SDIM, SDIM, ZERO, ONE,
     $                      F( IB1, IB1 ), LDF, DWORK( IQ3 ), SDIM,
     $                      F( IB1, IB1 ), LDF, DWORK( IWRK2 ),
     $                      LDWORK-IWRK2+1, INFO )
               CALL DSCAL( SDIM, HALF, F( IB1, IB1 ), LDF+1 )
C
               IF( LCMPQ ) THEN
C
C                 Update Q.
C                 Workspace: IWRK2 + 2*N - 1, if COMPQ = 'I';
C                            IWRK2 + 4*N - 1, if COMPQ = 'U'.
C
                  CALL DGEMM(  'No Transpose', 'No Transpose', UPDS,
     $                         SDIM, SDIM, ONE, Q( 1, IB1 ), LDQ,
     $                         DWORK( IQ1 ), SDIM, ZERO, DWORK( IWRK2 ),
     $                         UPDS )
                  CALL DLACPY( 'Full', UPDS, SDIM, DWORK( IWRK2 ), UPDS,
     $                         Q( 1, IB1 ), LDQ )
                  CALL DGEMM(  'No Transpose', 'No Transpose', UPDS,
     $                         SDIM, SDIM, ONE, Q( IUPD, M+IB1 ), LDQ,
     $                         DWORK( IQ3 ), SDIM, ZERO, DWORK( IWRK2 ),
     $                         UPDS )
                  CALL DLACPY( 'Full', UPDS, SDIM, DWORK( IWRK2 ), UPDS,
     $                         Q( IUPD, M+IB1 ), LDQ )
               END IF
C
               IF( LCMPU ) THEN
C
C                 Update U1.
C
                  CALL DGEMM(  'No Transpose', 'No Transpose', M, SDIM,
     $                         SDIM, ONE, U1( 1, IB1 ), LDU1,
     $                         DWORK( IQ2 ), SDIM, ZERO, DWORK( IWRK2 ),
     $                         M )
                  CALL DLACPY( 'Full', M, SDIM, DWORK( IWRK2 ), M,
     $                         U1( 1, IB1 ), LDU1 )
               END IF
C
               IF( LUPDU ) THEN
C
C                 Update U2.
C
                  CALL DGEMM(  'No Transpose', 'No Transpose', M, SDIM,
     $                         SDIM, ONE, U2( 1, IB1 ), LDU2,
     $                         DWORK( IQ2 ), SDIM, ZERO, DWORK( IWRK2 ),
     $                         M )
                  CALL DLACPY( 'Full', M, SDIM, DWORK( IWRK2 ), M,
     $                         U2( 1, IB1 ), LDU2 )
               END IF
C
C              Update index lists IWORK(1:M) and IWORK(M+2:N+1) if a
C              1-by-1 and 2-by-2 block have been swapped.
C
               HLP = DIM2 - DIM1
               IF( HLP.EQ.1 ) THEN
C
C                 First block was 2-by-2.
C
                  IWORK( J+1 ) = IB1 + 1
               ELSE IF( HLP.EQ.-1 ) THEN
C
C                 Second block was 2-by-2.
C
                  IWORK( J+1 ) = IB1 + 2
               END IF
C
C              Update IWORK(M+2:N+1).
C
               HLP = IWORK( IS+J )
               IWORK( IS+J )   = IWORK( IS+J+1 )
               IWORK( IS+J+1 ) = HLP
   30       CONTINUE
            MM = MM + 1
         END IF
         K = K + 1
      GO TO 20
C
C     END WHILE 20
C
      END IF
C
C     II. Reorder the eigenvalues with positive real parts to the bottom.
C
      K = R
C
C     WHILE( K.GE.MM+1 ) DO
C
   40 CONTINUE
      IF( K.GE.MM+1 ) THEN
         IF( IWORK( IS+K ).GT.0 ) THEN
            DO 50 J = K, MP - 2
               IB1  = IWORK( J )
               IB2  = IWORK( J+1 )
               IB3  = IWORK( J+2 )
               DIM1 = IB2  - IB1
               DIM2 = IB3  - IB2
               SDIM = DIM1 + DIM2
C
C              Copy the relevant part of A(ib1:ib3-1,ib1:ib3-1),
C              C(ib1:ib3-1,ib1:ib3-1), and B(ib1:ib3-1,ib1:ib3-1) to
C              DWORK as inputs for MB03CD. Also, set the additional
C              zero elements.
C
               CALL DLACPY( 'Upper', SDIM, SDIM, A( IB1, IB1 ), LDA,
     $                      DWORK( IA ), SDIM )
               CALL MA02AD( 'Lower', SDIM, SDIM, C( IB1, IB1 ), LDC,
     $                      DWORK( IC ), SDIM )
               CALL DLACPY( 'Upper', SDIM, SDIM, B( IB1, IB1 ), LDB,
     $                      DWORK( IB ), SDIM )
               CALL DCOPY(  SDIM-1, B( IB1+1, IB1 ), LDB+1,
     $                      DWORK( IB+1 ), SDIM+1 )
               IF( DIM1.EQ.2 ) THEN
                  DWORK( IA+1 ) = ZERO
                  DWORK( IC+1 ) = ZERO
               END IF
               IF( DIM2.EQ.2 ) THEN
                  I1 = SDIM*( SDIM - 1 ) - 1
                  DWORK( IA+I1 ) = ZERO
                  DWORK( IC+I1 ) = ZERO
               END IF
               DWORK( IB+SDIM-1 ) = ZERO
               IF( SDIM.EQ.4 ) THEN
                  DWORK( IB+2 ) = ZERO
                  DWORK( IB+7 ) = ZERO
               END IF
C
C              Perform eigenvalue/matrix block exchange.
C
               CALL MB03CD( 'Upper', DIM1, DIM2, PREC, DWORK( IC ),
     $                      SDIM, DWORK( IA ),  SDIM,  DWORK( IB ),
     $                      SDIM, DWORK( IQ1 ), SDIM, DWORK( IQ2 ),
     $                      SDIM, DWORK( IQ3 ), SDIM, DWORK( IWRK1 ),
     $                      LDWORK-IWRK1+1, INFO )
               IF( INFO.GT.0 ) THEN
                  INFO = 2
                  RETURN
               END IF
C
C              Copy the transformed diagonal block of B, if sdim > 2.
C
               IF( SDIM.GT.2 ) THEN
                  CALL DLACPY( 'Upper', SDIM, SDIM, DWORK( IB ), SDIM,
     $                         B( IB1, IB1 ), LDB )
                  CALL DCOPY(  SDIM-1, DWORK( IB+1 ), SDIM+1,
     $                         B( IB1+1, IB1 ), LDB+1 )
               END IF
C
               NROWS = IB1 - 1
               NCOLS = M - IB3 + 1
               NROW  = IB3 - 1
               NCOL  = M - IB1 + 1
               CALL DLACPY( 'Lower', SDIM-1, SDIM-1, A( IB1+1, IB1 ),
     $                      LDA, DUM, 3 )
               CALL DLASET( 'Lower', SDIM-1, SDIM-1, ZERO, ZERO,
     $                      A( IB1+1, IB1 ), LDA )
               CALL DLACPY( 'Upper', SDIM-1, SDIM-1, C( IB1, IB1+1 ),
     $                      LDC, DUM( 1, 2 ), 3 )
               CALL DLASET( 'Upper', SDIM-1, SDIM-1, ZERO, ZERO,
     $                      C( IB1, IB1+1 ), LDC )
C
C              Update A.
C
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, SDIM,
     $                      SDIM, ONE, A( 1, IB1 ), LDA, DWORK( IQ1 ),
     $                      SDIM, ZERO, DWORK( IWRK2 ), NROW )
               CALL DLACPY( 'Full', NROW, SDIM, DWORK( IWRK2 ), NROW,
     $                      A( 1, IB1 ), LDA )
               CALL DGEMM(  'Transpose', 'No Transpose', SDIM, NCOL,
     $                      SDIM, ONE, DWORK( IQ2 ), SDIM,
     $                      A( IB1, IB1 ), LDA, ZERO, DWORK( IWRK2 ),
     $                      SDIM )
               CALL DLACPY( 'Full', SDIM, NCOL, DWORK( IWRK2 ), SDIM,
     $                      A( IB1, IB1 ), LDA )
               CALL DLACPY( 'Lower', SDIM-1, SDIM-1, DUM, 3,
     $                      A( IB1+1, IB1 ), LDA )
C
C              Update C.
C
               CALL DGEMM(  'No Transpose', 'No Transpose', NCOL, SDIM,
     $                      SDIM, ONE, C( IB1, IB1 ), LDC, DWORK( IQ3 ),
     $                      SDIM, ZERO, DWORK( IWRK2 ), NCOL )
               CALL DLACPY( 'Full', NCOL, SDIM, DWORK( IWRK2 ), NCOL,
     $                      C( IB1, IB1 ), LDC )
               CALL DGEMM(  'Transpose', 'No Transpose', SDIM, NROW,
     $                      SDIM, ONE, DWORK( IQ2 ), SDIM, C( IB1, 1 ),
     $                      LDC, ZERO, DWORK( IWRK2 ), SDIM )
               CALL DLACPY( 'Full', SDIM, NROW, DWORK( IWRK2 ), SDIM,
     $                      C( IB1, 1 ), LDC )
               CALL DLACPY( 'Upper', SDIM-1, SDIM-1, DUM( 1, 2 ), 3,
     $                      C( IB1, IB1+1 ), LDC )
C
C              Update D.
C
               CALL DGEMM(  'No Transpose', 'No Transpose', M, SDIM,
     $                      SDIM, ONE, D( 1, IB1 ), LDD, DWORK( IQ3 ),
     $                      SDIM, ZERO, DWORK( IWRK2 ), M )
               CALL DLACPY( 'Full', M, SDIM, DWORK( IWRK2 ), M,
     $                      D( 1, IB1 ), LDD )
               CALL DGEMM(  'Transpose', 'No Transpose', SDIM, M, SDIM,
     $                      ONE, DWORK( IQ2 ), SDIM, D( IB1, 1 ), LDD,
     $                      ZERO, DWORK( IWRK2 ), SDIM )
               CALL DLACPY( 'Full', SDIM, M, DWORK( IWRK2 ), SDIM,
     $                      D( IB1, 1 ), LDD )
C
C              Update B.
C
               IF( SDIM.GT.2 ) THEN
                  NROW = NROWS
                  NCOL = NCOLS
                  IBS  = IB3
                  LDW  = MAX( 1, NROW )
               ELSE
                  IBS  = IB1
                  LDW  = NROW
               END IF
               CALL DGEMM(  'No Transpose', 'No Transpose', NROW, SDIM,
     $                      SDIM, ONE, B( 1, IB1 ), LDB, DWORK( IQ1 ),
     $                      SDIM, ZERO, DWORK( IWRK2 ), LDW )
               CALL DLACPY( 'Full', NROW, SDIM, DWORK( IWRK2 ), LDW,
     $                      B( 1, IB1 ), LDB )
               CALL DGEMM(  'Transpose', 'No Transpose', SDIM, NCOL,
     $                      SDIM, ONE, DWORK( IQ3 ), SDIM,
     $                      B( IB1, IBS ), LDB, ZERO, DWORK( IWRK2 ),
     $                      SDIM )
               CALL DLACPY( 'Full', SDIM, NCOL, DWORK( IWRK2 ), SDIM,
     $                      B( IB1, IBS ), LDB )
C
C              Update F.
C
               CALL DGEMM(  'No Transpose', 'No Transpose', NROWS, SDIM,
     $                      SDIM, ONE, F( 1, IB1 ), LDF, DWORK( IQ3 ),
     $                      SDIM, ZERO, DWORK( IWRK2 ), LDW )
               CALL DLACPY( 'Full', NROWS, SDIM, DWORK( IWRK2 ), LDW,
     $                      F( 1, IB1 ), LDF )
               CALL DGEMM(  'Transpose', 'No Transpose', SDIM, NCOLS,
     $                      SDIM, ONE, DWORK( IQ3 ), SDIM,
     $                      F( IB1, IB3 ), LDF, ZERO, DWORK( IWRK2 ),
     $                      SDIM )
               CALL DLACPY( 'Full', SDIM, NCOLS, DWORK( IWRK2 ), SDIM,
     $                      F( IB1, IB3 ), LDF )
               CALL MB01RU( 'Upper', 'Transpose', SDIM, SDIM, ZERO, ONE,
     $                      F( IB1, IB1 ), LDF, DWORK( IQ3 ), SDIM,
     $                      F( IB1, IB1 ), LDF, DWORK( IWRK2 ),
     $                      LDWORK-IWRK2+1, INFO )
               CALL DSCAL( SDIM, HALF, F( IB1, IB1 ), LDF+1 )
C
               IF( LCMPQ ) THEN
C
C                 Update Q.
C
                  CALL DGEMM(  'No Transpose', 'No Transpose', UPDS,
     $                         SDIM, SDIM, ONE, Q( 1, IB1 ), LDQ,
     $                         DWORK( IQ1 ), SDIM, ZERO, DWORK( IWRK2 ),
     $                         UPDS )
                  CALL DLACPY( 'Full', UPDS, SDIM, DWORK( IWRK2 ), UPDS,
     $                         Q( 1, IB1 ), LDQ )
                  CALL DGEMM(  'No Transpose', 'No Transpose', UPDS,
     $                         SDIM, SDIM, ONE, Q( IUPD, M+IB1 ), LDQ,
     $                         DWORK( IQ3 ), SDIM, ZERO, DWORK( IWRK2 ),
     $                         UPDS )
                  CALL DLACPY( 'Full', UPDS, SDIM, DWORK( IWRK2 ), UPDS,
     $                         Q( IUPD, M+IB1 ), LDQ )
               END IF
C
               IF( LCMPU ) THEN
C
C                 Update U1.
C
                  CALL DGEMM(  'No Transpose', 'No Transpose', M, SDIM,
     $                         SDIM, ONE, U1( 1, IB1 ), LDU1,
     $                         DWORK( IQ2 ), SDIM, ZERO, DWORK( IWRK2 ),
     $                         M )
                  CALL DLACPY( 'Full', M, SDIM, DWORK( IWRK2 ), M,
     $                         U1( 1, IB1 ), LDU1 )
               END IF
C
               IF( LUPDU ) THEN
C
C                 Update U2.
C
                  CALL DGEMM(  'No Transpose', 'No Transpose', M, SDIM,
     $                         SDIM, ONE, U2( 1, IB1 ), LDU2,
     $                         DWORK( IQ2 ), SDIM, ZERO, DWORK( IWRK2 ),
     $                         M )
                  CALL DLACPY( 'Full', M, SDIM, DWORK( IWRK2 ), M,
     $                         U2( 1, IB1 ), LDU2 )
               END IF
C
C              Update index list IWORK(1:M) if a 1-by-1 and 2-by-2 block
C              have been swapped. IWORK(M+2:N+1) is not needed anymore,
C              so it is not necessary to update it.
C
               HLP = DIM2 - DIM1
               IF( HLP.EQ.1 ) THEN
C
C                 First block was 2-by-2.
C
                  IWORK( J+1 ) = IB1 + 1
               ELSE IF( HLP.EQ.-1 ) THEN
C
C                 Second block was 2-by-2.
C
                  IWORK( J+1 ) = IB1 + 2
               END IF
   50       CONTINUE
            MP = MP - 1
         END IF
         K = K - 1
      GO TO 40
C
C     END WHILE 40
C
      END IF
C
C     STEP 2: Reorder the remaining eigenvalues with negative real parts.
C
C     Set pointers for the inputs and outputs of MB03GD.
C
      IQUPLE = 1
      IUUPLE = IQUPLE + 16
      IZUPLE = IUUPLE + 16
      IHUPLE = IZUPLE + 16
      IWRK5  = IHUPLE + 16
      IWRK3  = IZUPLE
      IWRK4  = IWRK3  + 2*N
      ITMP1  = IWRK3  + N
      ITMP2  = ITMP1  + 4
      ITMP3  = ITMP2  + 4
C
      DO 70 K = R, MP, -1
C
C        I. Exchange the eigenvalues between two diagonal blocks.
C
         IR   = IWORK( R )
         DIM1 = IWORK( R+1 ) - IR
         SDIM = 2*DIM1
C
         IF( DIM1.EQ.2 ) THEN
            A( M, IR ) = ZERO
            C( IR, M ) = ZERO
C
C           Build the (small) symmetric matrix F(M-1:M,M-1:M).
C
            F( M, IR ) =  F( IR, M )
         END IF
C
C        Calculate position of submatrices in DWORK.
C
         IZUPRI = IZUPLE + DIM1*SDIM
         IZLORI = IZUPRI + DIM1
         IUUPRI = IUUPLE + DIM1*SDIM
         IQLOLE = IQUPLE + DIM1
         IQUPRI = IQUPLE + DIM1*SDIM
         IQLORI = IQUPRI + DIM1
C
C        Generate input matrices for MB03GD built of submatrices of A,
C        D, C, B, and F.
C
         CALL DLACPY( 'Upper', DIM1, DIM1, A( IR, IR ), LDA,
     $                DWORK( IZUPLE ), SDIM )
         CALL DLACPY( 'Full',  DIM1, DIM1, D( IR, IR ), LDD,
     $                DWORK( IZUPRI ), SDIM )
         CALL DLACPY( 'Lower', DIM1, DIM1, C( IR, IR ), LDC,
     $                DWORK( IZLORI ), SDIM )
         CALL DLACPY( 'Full', DIM1, DIM1, B( IR, IR ), LDB,
     $                DWORK( IHUPLE ), SDIM )
         CALL DLACPY( 'Upper', DIM1, DIM1, F( IR, IR ), LDB,
     $                DWORK( IHUPLE+DIM1*SDIM ), SDIM )
         IF( DIM1.EQ.2 ) THEN
            DWORK( IZUPLE+1    ) = ZERO
            DWORK( IZLORI+SDIM ) = ZERO
         END IF
C
C        Perform eigenvalue exchange.
C        Workspace: IWRK5 + 11, if SDIM = 4.
C
         CALL MB03GD( SDIM, DWORK( IZUPLE ), SDIM, DWORK( IHUPLE ),
     $                SDIM, PAR, DWORK( IQUPLE ), SDIM, DWORK( IUUPLE ),
     $                SDIM, DWORK( IWRK5 ), LDWORK-IWRK5+1, INFO )
         IF( INFO.GT.0 ) THEN
            INFO = 3
            RETURN
         END IF
C
         IF( DIM1.EQ.2 ) THEN
C
C           Update A by transformations from the right.
C           Workspace: IWRK3 + N - 1.
C
            CALL DLACPY( 'Full', M, DIM1, A( 1, IR ), LDA,
     $                   DWORK( IWRK3 ), M )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, DIM1, DIM1,
     $                   ONE, DWORK( IWRK3 ), M, DWORK( IQUPLE ), SDIM,
     $                   ZERO, A( 1, IR ), LDA )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, DIM1, DIM1,
     $                   ONE, D( 1, IR ), LDD, DWORK( IQLOLE ), SDIM,
     $                   ONE, A( 1, IR ), LDA )
C
C           Update D by transformations from the right.
C
            CALL DGEMM(  'No Transpose', 'No Transpose', M, DIM1, DIM1,
     $                   ONE, DWORK( IWRK3 ), M, DWORK( IQUPRI ), SDIM,
     $                   ZERO, DWORK( ITMP1 ), M )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, DIM1, DIM1,
     $                   ONE, D( 1, IR ), LDD, DWORK( IQLORI ), SDIM,
     $                   ONE, DWORK( ITMP1 ), M )
            CALL DLACPY( 'Full', M, DIM1, DWORK( ITMP1 ), M, D( 1, IR ),
     $                   LDD )
C
C           Compute intermediate product Cf*Q21, with
C           Cf = C(M-1:M,M-1:M).
C
            CALL DGEMM( 'No Transpose', 'No Transpose', DIM1, DIM1,
     $                  DIM1, ONE, C( IR, IR ), LDC, DWORK( IQLOLE ),
     $                  SDIM, ZERO, DWORK( ITMP1 ), DIM1 )
C
C           Update C by transformations from the right.
C
            CALL DGEMM(  'No Transpose', 'No Transpose', DIM1, DIM1,
     $                   DIM1, ONE, C( IR, IR ), LDC, DWORK( IQLORI ),
     $                   SDIM, ZERO, DWORK( IWRK3 ), DIM1 )
            CALL DLACPY( 'Full', DIM1, DIM1, DWORK( IWRK3 ), DIM1,
     $                   C( IR, IR ), LDC )
C
C           Update A by transformations from the left.
C
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                   ONE, DWORK( IUUPLE ), SDIM, A( IR, IR ), LDA,
     $                   ZERO, DWORK( IWRK3 ), DIM1 )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                   -ONE, DWORK( IUUPRI ), SDIM, DWORK( ITMP1 ),
     $                   DIM1, ONE, DWORK( IWRK3 ), DIM1 )
            CALL DLACPY( 'Full', DIM1, DIM1, DWORK( IWRK3 ), DIM1,
     $                   A( IR, IR ), LDA )
C
C           Update D by transformations from the left.
C
            CALL DLACPY( 'Full', DIM1, M, D( IR, 1 ), LDD,
     $                   DWORK( IWRK3 ), DIM1 )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M, DIM1,
     $                   ONE, DWORK( IUUPLE ), SDIM, DWORK( IWRK3 ),
     $                   DIM1, ZERO, D( IR, 1 ), LDD )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M, DIM1,
     $                   -ONE, DWORK( IUUPRI ), SDIM, C( IR, 1 ), LDC,
     $                   ONE, D( IR, 1 ), LDD )
C
C           Update C by transformations from the left.
C
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M, DIM1,
     $                   ONE, DWORK( IUUPRI ), SDIM, DWORK( IWRK3 ),
     $                   DIM1, ZERO, DWORK( ITMP1 ), DIM1 )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, M, DIM1,
     $                   ONE, DWORK( IUUPLE ), SDIM, C( IR, 1 ), LDC,
     $                   ONE, DWORK( ITMP1 ), DIM1 )
            CALL DLACPY( 'Full', DIM1, M, DWORK( ITMP1 ), DIM1,
     $                   C( IR, 1 ), LDC )
C
C           Update B by transformations from the right.
C
            CALL DLACPY( 'Full', M, DIM1, B( 1, IR ), LDB,
     $                   DWORK( IWRK3 ), M )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, DIM1, DIM1,
     $                   ONE, DWORK( IWRK3 ), M, DWORK( IQUPLE ), SDIM,
     $                   ZERO, B( 1, IR ), LDB )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, DIM1, DIM1,
     $                   ONE, F( 1, IR ), LDF, DWORK( IQLOLE ), SDIM,
     $                   ONE, B( 1, IR ), LDB )
C
C           Update F by transformations from the right.
C
            CALL DGEMM(  'No Transpose', 'No Transpose', M, DIM1, DIM1,
     $                   ONE, DWORK( IWRK3 ), M, DWORK( IQUPRI ), SDIM,
     $                   ZERO, DWORK( ITMP1 ), M )
            CALL DGEMM(  'No Transpose', 'No Transpose', M, DIM1, DIM1,
     $                   ONE, F( 1, IR ), LDF, DWORK( IQLORI ), SDIM,
     $                   ONE, DWORK( ITMP1 ), M )
            CALL DLACPY( 'Full', M, DIM1, DWORK( ITMP1 ), M, F( 1, IR ),
     $                   LDF )
C
C           Compute intermediate products Bf'*Q21 and Bf'*Q22, with
C           Bf = B(M-1:M,M-1:M).
C
            CALL DGEMM( 'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                  ONE, DWORK( IWRK3+M-DIM1 ), M, DWORK( IQLOLE ),
     $                  SDIM, ZERO, DWORK( ITMP1 ), DIM1 )
C
            CALL DGEMM( 'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                  ONE, DWORK( IWRK3+M-DIM1 ), M, DWORK( IQLORI ),
     $                  SDIM, ZERO, DWORK( ITMP2 ), DIM1 )
C
C           Update B by transformations from the left.
C
            CALL DLACPY( 'Full', DIM1, DIM1, B( IR, IR ), LDB,
     $                   DWORK( ITMP3 ), DIM1 )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                   ONE, DWORK( IQUPRI ), SDIM, DWORK( ITMP1 ),
     $                   DIM1, ZERO, B( IR, IR ), LDB )
            CALL DGEMM(  'Transpose', 'No Transpose', DIM1, DIM1, DIM1,
     $                   ONE, DWORK( IQLORI ), SDIM, DWORK( ITMP3 ),
     $                   DIM1, ONE, B( IR, IR ), LDB )
C
C           Update F by transformations from the left.
C
            CALL MB01RX( 'Left', 'Upper', 'Transpose', DIM1, DIM1, ZERO,
     $                   ONE, DWORK( ITMP1 ), DIM1, DWORK( IQLORI ),
     $                   SDIM, F( IR, IR ), LDF, INFO )
            CALL MB01RX( 'Left', 'Upper', 'Transpose', DIM1, DIM1, ONE,
     $                   ONE, DWORK( ITMP1 ), DIM1, DWORK( IQUPRI ),
     $                   SDIM, DWORK( ITMP2 ), DIM1, INFO )
            CALL DLACPY( 'Upper', DIM1, DIM1, DWORK( ITMP1 ), DIM1,
     $                   F( IR, IR ), LDF )
C
            IF( LCMPQ ) THEN
C
C              Update Q.
C              Workspace: IWRK4 + 2*N - 1.
C
               CALL DLACPY( 'Full', N, DIM1, Q( 1, IR ), LDQ,
     $                      DWORK( IWRK4 ), N )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, DWORK( IWRK4 ), N,
     $                      DWORK( IQUPLE ), SDIM, ZERO, Q( 1, IR ),
     $                      LDQ )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, Q( 1, M+IR ), LDQ,
     $                      DWORK( IQLOLE ), SDIM, ONE, Q( 1, IR ),
     $                      LDQ )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, DWORK( IWRK4 ), N,
     $                      DWORK( IQUPRI ), SDIM, ZERO, DWORK( IWRK3 ),
     $                      N )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, DIM1,
     $                      DIM1, ONE, Q( 1, M+IR ), LDQ,
     $                      DWORK( IQLORI ), SDIM, ONE, DWORK( IWRK3 ),
     $                      N )
               CALL DLACPY( 'Full', N, DIM1, DWORK( IWRK3 ), N,
     $                      Q( 1, M+IR ), LDQ )
            END IF
C
            IF( LCMPU ) THEN
C
C              Update U.
C              Workspace: ITMP1 + N - 1.
C
               CALL DLACPY( 'Full', M, DIM1, U1( 1, IR ), LDU1,
     $                      DWORK( ITMP1 ), M )
               CALL DGEMM(  'No Transpose', 'No Transpose', M, DIM1,
     $                      DIM1, ONE, DWORK( ITMP1 ), M,
     $                      DWORK( IUUPLE ), SDIM, ZERO, U1( 1, IR ),
     $                      LDU1 )
               CALL DGEMM(  'No Transpose', 'No Transpose', M, DIM1,
     $                      DIM1, -ONE, U2( 1, IR ), LDU2,
     $                      DWORK( IUUPRI ), SDIM, ONE,  U1( 1, IR ),
     $                      LDU1 )
               CALL DGEMM(  'No Transpose', 'No Transpose', M, DIM1,
     $                      DIM1, ONE, DWORK( ITMP1 ), M,
     $                      DWORK( IUUPRI ), SDIM, ZERO, DWORK( IWRK3 ),
     $                      M )
               CALL DGEMM(  'No Transpose', 'No Transpose', M, DIM1,
     $                      DIM1, ONE, U2( 1, IR ), LDU2,
     $                      DWORK( IUUPLE ), SDIM, ONE, DWORK( IWRK3 ),
     $                      M )
               CALL DLACPY( 'Full', M, DIM1, DWORK( IWRK3 ), M,
     $                      U2( 1, IR ), LDU2 )
            END IF
C
         ELSE
            U11 = DWORK( IUUPLE )
            U12 = DWORK( IUUPRI )
            Q11 = DWORK( IQUPLE )
            Q21 = DWORK( IQLOLE )
            Q12 = DWORK( IQUPRI )
            Q22 = DWORK( IQLORI )
C
C           Update A by transformations from the right.
C
            CALL DCOPY( M, A( 1, IR ), 1, DWORK( IWRK3 ), 1 )
            CALL DSCAL( M, Q11, A( 1, IR ), 1 )
            CALL DAXPY( M, Q21, D( 1, IR ), 1, A( 1, IR ), 1 )
C
C           Update D by transformations from the right.
C
            CALL DSCAL( M, Q22, D( 1, IR ), 1 )
            CALL DAXPY( M, Q12, DWORK( IWRK3 ), 1, D( 1, IR ), 1 )
C
C           Compute intermediate product C(M,M)*Q21.
C
            TMPC = C( IR, IR )*Q21
C
C           Update C by transformations from the right.
C
            C( IR, IR ) = C( IR, IR )*Q22
C
C           Update A by transformations from the left.
C
            A( IR, IR ) = U11*A( IR, IR ) - U12*TMPC
C
C           Update D by transformations from the left.
C
            CALL DCOPY( M, D( IR, 1 ), LDD, DWORK( IWRK3 ), 1 )
            CALL DSCAL( M,  U11, D( IR, 1 ), LDD )
            CALL DAXPY( M, -U12, C( IR, 1 ), LDC, D( IR, 1 ), LDD )
C
C           Update C by transformations from the left.
C
            CALL DSCAL( M, U11, C( IR, 1 ), LDC )
            CALL DAXPY( M, U12, DWORK( IWRK3 ), 1, C( IR, 1 ), LDC )
C
C           Update B by transformations from the right.
C
            CALL DCOPY( M-1, B( 1, IR ), 1, DWORK( IWRK3 ), 1 )
            CALL DSCAL( M-1, Q11, B( 1, IR ), 1 )
            CALL DAXPY( M-1, Q21, F( 1, IR ), 1, B( 1, IR ), 1 )
C
C           Update F by transformations from the right.
C
            CALL DSCAL( M-1, Q22, F( 1, IR ), 1 )
            CALL DAXPY( M-1, Q12, DWORK( IWRK3 ), 1, F( 1, IR ), 1 )
C
C           Update B by transformations from the left.
C
            B( M, M ) = -B( M, M )
C
            IF( LCMPQ ) THEN
C
C              Update Q.
C
               CALL DCOPY( N, Q( 1, IR ), 1, DWORK( IWRK4 ), 1 )
               CALL DSCAL( N, Q11, Q( 1, IR ), 1 )
               CALL DAXPY( N, Q21, Q( 1, IR+M ), 1, Q( 1, IR ), 1 )
               CALL DSCAL( N, Q22, Q( 1, IR+M ), 1 )
               CALL DAXPY( N, Q12, DWORK( IWRK4 ), 1, Q( 1, IR+M ), 1 )
            END IF
C
            IF( LCMPU ) THEN
C
C              Update U.
C
               CALL DCOPY( M, U1( 1, IR ), 1, DWORK( IWRK4 ), 1 )
               CALL DSCAL( M,  U11, U1( 1, IR ), 1 )
               CALL DAXPY( M, -U12, U2( 1, IR ), 1, U1( 1, IR ), 1 )
               CALL DSCAL( M,  U11, U2( 1, IR ), 1 )
               CALL DAXPY( M,  U12, DWORK( IWRK4 ), 1, U2( 1, IR ), 1 )
            END IF
         END IF
C
         MM = MM + 1
         DO 60 J = R - 1, MM, -1
            IB1  = IWORK( J )
            IB2  = IWORK( J+1 )
            IB3  = IWORK( J+2 )
            DIM1 = IB2  - IB1
            DIM2 = IB3  - IB2
            SDIM = DIM1 + DIM2
C
C           Copy the relevant part of A(ib1:ib3-1,ib1:ib3-1),
C           C(ib1:ib3-1,ib1:ib3-1), and B(ib1:ib3-1,ib1:ib3-1) to
C           DWORK as inputs for MB03CD. Also, set the additional
C           zero elements.
C
            CALL DLACPY( 'Upper', SDIM, SDIM, A( IB1, IB1 ), LDA,
     $                   DWORK( IA ), SDIM )
            CALL MA02AD( 'Lower', SDIM, SDIM, C( IB1, IB1 ), LDC,
     $                   DWORK( IC ), SDIM )
            CALL DLACPY( 'Upper', SDIM, SDIM, B( IB1, IB1 ), LDB,
     $                   DWORK( IB ), SDIM )
            CALL DCOPY(  SDIM-1, B( IB1+1, IB1 ), LDB+1, DWORK( IB+1 ),
     $                   SDIM+1 )
            IF( DIM1.EQ.2 ) THEN
               DWORK( IA+1 ) = ZERO
               DWORK( IC+1 ) = ZERO
            END IF
            IF( DIM2.EQ.2 ) THEN
               I1 = SDIM*( SDIM - 1 ) - 1
               DWORK( IA+I1 ) = ZERO
               DWORK( IC+I1 ) = ZERO
            END IF
            DWORK( IB+SDIM-1 ) = ZERO
            IF( SDIM.EQ.4 ) THEN
               DWORK( IB+2 ) = ZERO
               DWORK( IB+7 ) = ZERO
            END IF
C
C           Perform eigenvalue/matrix block exchange.
C
            CALL MB03CD( 'Upper', DIM1, DIM2, PREC, DWORK( IC ), SDIM,
     $                   DWORK( IA ),  SDIM,  DWORK( IB ), SDIM,
     $                   DWORK( IQ1 ), SDIM, DWORK( IQ2 ), SDIM,
     $                   DWORK( IQ3 ), SDIM, DWORK( IWRK1 ),
     $                   LDWORK-IWRK1+1, INFO )
            IF( INFO.GT.0 ) THEN
               INFO = 2
               RETURN
            END IF
C
C           Copy the transformed diagonal block of B, if sdim > 2.
C
            IF( SDIM.GT.2 ) THEN
               CALL DLACPY( 'Upper', SDIM, SDIM, DWORK( IB ), SDIM,
     $                      B( IB1, IB1 ), LDB )
               CALL DCOPY(  SDIM-1, DWORK( IB+1 ), SDIM+1,
     $                      B( IB1+1, IB1 ), LDB+1 )
            END IF
C
            NROWS = IB1 - 1
            NCOLS = M - IB3 + 1
            NROW  = IB3 - 1
            NCOL  = M - IB1 + 1
            CALL DLACPY( 'Lower', SDIM-1, SDIM-1, A( IB1+1, IB1 ), LDA,
     $                   DUM, 3 )
            CALL DLASET( 'Lower', SDIM-1, SDIM-1, ZERO, ZERO,
     $                   A( IB1+1, IB1 ), LDA )
            CALL DLACPY( 'Upper', SDIM-1, SDIM-1, C( IB1, IB1+1 ), LDC,
     $                   DUM( 1, 2 ), 3 )
            CALL DLASET( 'Upper', SDIM-1, SDIM-1, ZERO, ZERO,
     $                   C( IB1, IB1+1 ), LDC )
C
C           Update A.
C
            CALL DGEMM(  'No Transpose', 'No Transpose', NROW, SDIM,
     $                   SDIM, ONE, A( 1, IB1 ), LDA, DWORK( IQ1 ),
     $                   SDIM, ZERO, DWORK( IWRK2 ), NROW )
            CALL DLACPY( 'Full', NROW, SDIM, DWORK( IWRK2 ), NROW,
     $                   A( 1, IB1 ), LDA )
            CALL DGEMM(  'Transpose', 'No Transpose', SDIM, NCOL, SDIM,
     $                   ONE, DWORK( IQ2 ), SDIM, A( IB1, IB1 ), LDA,
     $                   ZERO, DWORK( IWRK2 ), SDIM )
            CALL DLACPY( 'Full', SDIM, NCOL, DWORK( IWRK2 ), SDIM,
     $                   A( IB1, IB1 ), LDA )
            CALL DLACPY( 'Lower', SDIM-1, SDIM-1, DUM, 3,
     $                   A( IB1+1, IB1 ), LDA )
C
C           Update C.
C
            CALL DGEMM(  'No Transpose', 'No Transpose', NCOL, SDIM,
     $                   SDIM, ONE, C( IB1, IB1 ), LDC, DWORK( IQ3 ),
     $                   SDIM, ZERO, DWORK( IWRK2 ), NCOL )
            CALL DLACPY( 'Full', NCOL, SDIM, DWORK( IWRK2 ), NCOL,
     $                   C( IB1, IB1 ), LDC )
            CALL DGEMM(  'Transpose', 'No Transpose', SDIM, NROW, SDIM,
     $                   ONE, DWORK( IQ2 ), SDIM, C( IB1, 1 ), LDC,
     $                   ZERO, DWORK( IWRK2 ), SDIM )
            CALL DLACPY( 'Full', SDIM, NROW, DWORK( IWRK2 ), SDIM,
     $                   C( IB1, 1 ), LDC )
            CALL DLACPY( 'Upper', SDIM-1, SDIM-1, DUM( 1, 2 ), 3,
     $                   C( IB1, IB1+1 ), LDC )
C
C           Update D.
C
            CALL DGEMM(  'No Transpose', 'No Transpose', M, SDIM, SDIM,
     $                   ONE, D( 1, IB1 ), LDD, DWORK( IQ3 ), SDIM,
     $                   ZERO, DWORK( IWRK2 ), M )
            CALL DLACPY( 'Full', M, SDIM, DWORK( IWRK2 ), M,
     $                   D( 1, IB1 ), LDD )
            CALL DGEMM(  'Transpose', 'No Transpose', SDIM, M, SDIM,
     $                   ONE, DWORK( IQ2 ), SDIM, D( IB1, 1 ), LDD,
     $                   ZERO, DWORK( IWRK2 ), SDIM )
            CALL DLACPY( 'Full', SDIM, M, DWORK( IWRK2 ), SDIM,
     $                   D( IB1, 1 ), LDD )
C
C           Update B.
C
            IF( SDIM.GT.2 ) THEN
               NROW = NROWS
               NCOL = NCOLS
               IBS  = IB3
               LDW  = MAX( 1, NROW )
            ELSE
               IBS  = IB1
               LDW  = NROW
            END IF
            CALL DGEMM(  'No Transpose', 'No Transpose', NROW, SDIM,
     $                   SDIM, ONE, B( 1, IB1 ), LDB, DWORK( IQ1 ),
     $                   SDIM, ZERO, DWORK( IWRK2 ), LDW )
            CALL DLACPY( 'Full', NROW, SDIM, DWORK( IWRK2 ), LDW,
     $                   B( 1, IB1 ), LDB )
            CALL DGEMM(  'Transpose', 'No Transpose', SDIM, NCOL, SDIM,
     $                   ONE, DWORK( IQ3 ), SDIM, B( IB1, IBS ), LDB,
     $                   ZERO, DWORK( IWRK2 ), SDIM )
            CALL DLACPY( 'Full', SDIM, NCOL, DWORK( IWRK2 ), SDIM,
     $                   B( IB1, IBS ), LDB )
C
C           Update F.
C
            CALL DGEMM(  'No Transpose', 'No Transpose', NROWS, SDIM,
     $                   SDIM, ONE, F( 1, IB1 ), LDF, DWORK( IQ3 ),
     $                   SDIM, ZERO, DWORK( IWRK2 ), LDW )
            CALL DLACPY( 'Full', NROWS, SDIM, DWORK( IWRK2 ), LDW,
     $                   F( 1, IB1 ), LDF )
            CALL DGEMM(  'Transpose', 'No Transpose', SDIM, NCOLS, SDIM,
     $                   ONE, DWORK( IQ3 ), SDIM, F( IB1, IB3 ), LDF,
     $                   ZERO, DWORK( IWRK2 ), SDIM )
            CALL DLACPY( 'Full', SDIM, NCOLS, DWORK( IWRK2 ), SDIM,
     $                   F( IB1, IB3 ), LDF )
            CALL MB01RU( 'Upper', 'Transpose', SDIM, SDIM, ZERO, ONE,
     $                   F( IB1, IB1 ), LDF, DWORK( IQ3 ), SDIM,
     $                   F( IB1, IB1 ), LDF, DWORK( IWRK2 ),
     $                   LDWORK-IWRK2+1, INFO )
            CALL DSCAL( SDIM, HALF, F( IB1, IB1 ), LDF+1 )
C
            IF( LCMPQ ) THEN
C
C              Update Q.
C
               CALL DGEMM(  'No Transpose', 'No Transpose', N, SDIM,
     $                      SDIM, ONE, Q( 1, IB1 ), LDQ, DWORK( IQ1 ),
     $                      SDIM, ZERO, DWORK( IWRK2 ), N )
               CALL DLACPY( 'Full', N, SDIM, DWORK( IWRK2 ), N,
     $                      Q( 1, IB1 ), LDQ )
               CALL DGEMM(  'No Transpose', 'No Transpose', N, SDIM,
     $                      SDIM, ONE, Q( 1, M+IB1 ), LDQ, DWORK( IQ3 ),
     $                      SDIM, ZERO, DWORK( IWRK2 ), N )
               CALL DLACPY( 'Full', N, SDIM, DWORK( IWRK2 ), N,
     $                      Q( 1, M+IB1 ), LDQ )
            END IF
C
            IF( LCMPU ) THEN
C
C              Update U.
C
               CALL DGEMM(  'No Transpose', 'No Transpose', M, SDIM,
     $                      SDIM, ONE, U1( 1, IB1 ), LDU1, DWORK( IQ2 ),
     $                      SDIM, ZERO, DWORK( IWRK2 ), M )
               CALL DLACPY( 'Full', M, SDIM, DWORK( IWRK2 ), M,
     $                      U1( 1, IB1 ), LDU1 )
               CALL DGEMM(  'No Transpose', 'No Transpose', M, SDIM,
     $                      SDIM, ONE, U2( 1, IB1 ), LDU2, DWORK( IQ2 ),
     $                      SDIM, ZERO, DWORK( IWRK2 ), M )
               CALL DLACPY( 'Full', M, SDIM, DWORK( IWRK2 ), M,
     $                      U2( 1, IB1 ), LDU2 )
            END IF
C
C           Update index list IWORK(1:M)if a 1-by-1 and 2-by-2 block
C           have been swapped.
C
            HLP = DIM2 - DIM1
            IF( HLP.EQ.1 ) THEN
C
C              First block was 2-by-2.
C
               IWORK( J+1 ) = IB1 + 1
            ELSE IF( HLP.EQ.-1 ) THEN
C
C              Second block was 2-by-2.
C
               IWORK( J+1 ) = IB1 + 2
            END IF
   60    CONTINUE
   70 CONTINUE
C
      IF( M.GT.1 ) THEN
C
C        Restore the elements A(M,M-1), C(M-1,M), and F(M,M-1).
C
         A( M, M-1 ) = A2
         C( M-1, M ) = C2
         F( M, M-1 ) = F2
      END IF
C
      IF( MM.GT.0 ) THEN
         NEIG = IWORK( MM+1 ) - 1
      ELSE
         NEIG = 0
      END IF
C
      RETURN
C *** Last line of MB03ID ***
      END
