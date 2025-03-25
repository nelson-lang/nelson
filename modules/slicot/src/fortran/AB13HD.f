      SUBROUTINE AB13HD( DICO, JOBE, EQUIL, JOBD, CKPROP, REDUCE, POLES,
     $                   N, M, P, RANKE, FPEAK, A, LDA, E, LDE, B, LDB,
     $                   C, LDC, D, LDD, NR, GPEAK, TOL, IWORK, DWORK,
     $                   LDWORK, ZWORK, LZWORK, BWORK, IWARN, INFO )
C
C     PURPOSE
C
C     To compute the L-infinity norm of a proper continuous-time or
C     causal discrete-time system, either standard or in the descriptor
C     form,
C
C                                     -1
C        G(lambda) = C*( lambda*E - A ) *B + D .
C
C     The norm is finite if and only if the matrix pair (A,E) has no
C     finite eigenvalue on the boundary of the stability domain, i.e.,
C     the imaginary axis, or the unit circle, respectively.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     DICO    CHARACTER*1
C             Specifies the type of the system, as follows:
C             = 'C':  continuous-time system;
C             = 'D':  discrete-time system.
C
C     JOBE    CHARACTER*1
C             Specifies whether E is an identity matrix, a general
C             square matrix, or a matrix in compressed form, as follows:
C             = 'I':  E is the identity matrix;
C             = 'G':  E is a general matrix;
C             = 'C':  E is in compressed form, i.e., E = [ T  0 ],
C                                                        [ 0  0 ]
C                     with a square full-rank matrix T.
C
C     EQUIL   CHARACTER*1
C             Specifies whether the user wishes to preliminarily
C             equilibrate the system (A,E,B,C) or (A,B,C), as follows:
C             = 'S':  perform equilibration (scaling);
C             = 'N':  do not perform equilibration.
C
C     JOBD    CHARACTER*1
C             Specifies whether or not a non-zero matrix D appears in
C             the given state space model:
C             = 'D':  D is present;
C             = 'Z':  D is assumed a zero matrix;
C             = 'F':  D is known to be well-conditioned (hence, to have
C                     full rank), for DICO = 'C' and JOBE = 'I'.
C             The options JOBD = 'D' and JOBD = 'F' produce the same
C             results, but much less memory is needed for JOBD = 'F'.
C
C     CKPROP  CHARACTER*1
C             If DICO = 'C' and JOBE <> 'I', specifies whether the user
C             wishes to check the properness of the transfer function of
C             the descriptor system, as follows:
C             = 'C':  check the properness;
C             = 'N':  do not check the properness.
C             If the test is requested and the system is found improper
C             then GPEAK and FPEAK are both set to infinity, i.e., their
C             second component is zero; in addition, IWARN is set to 2.
C             If the test is not requested, but the system is improper,
C             the resulted GPEAK and FPEAK may be wrong.
C             If DICO = 'D' or JOBE = 'I', this option is ineffective.
C
C     REDUCE  CHARACTER*1
C             If CKPROP = 'C', specifies whether the user wishes to
C             reduce the system order, by removing all uncontrollable
C             and unobservable poles before computing the norm, as
C             follows:
C             = 'R': reduce the system order;
C             = 'N': compute the norm without reducing the order.
C             If CKPROP = 'N', this option is ineffective.
C
C     POLES   CHARACTER*1
C             Specifies whether the user wishes to use all or part of
C             the poles to compute the test frequencies (in the non-
C             iterative part of the algorithm), or all or part of the
C             midpoints (in the iterative part of the algorithm), as
C             follows:
C             = 'A': use all poles with non-negative imaginary parts
C                    and all midpoints;
C             = 'P': use part of the poles and midpoints.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The order of the system.  N >= 0.
C
C     M       (input) INTEGER
C             The column size of the matrix B.  M >= 0.
C
C     P       (input) INTEGER
C             The row size of the matrix C.  P >= 0.
C
C     RANKE   (input) INTEGER
C             If JOBE = 'C', RANKE denotes the rank of the descriptor
C             matrix E or the size of the full-rank block T.
C             0 <= RANKE <= N.
C
C     FPEAK   (input/output) DOUBLE PRECISION array, dimension (2)
C             On entry, this parameter must contain an estimate of the
C             frequency where the gain of the frequency response would
C             achieve its peak value. Setting FPEAK(2) = 0 indicates an
C             infinite frequency. An accurate estimate could reduce the
C             number of iterations of the iterative algorithm. If no
C             estimate is available, set FPEAK(1) = 0, and FPEAK(2) = 1.
C             FPEAK(1) >= 0, FPEAK(2) >= 0.
C             On exit, if INFO = 0, this array contains the frequency
C             OMEGA, where the gain of the frequency response achieves
C             its peak value GPEAK, i.e.,
C
C                 || G ( j*OMEGA ) || = GPEAK ,  if DICO = 'C', or
C
C                         j*OMEGA
C                 || G ( e       ) || = GPEAK ,  if DICO = 'D',
C
C             where OMEGA = FPEAK(1), if FPEAK(2) > 0, and OMEGA is
C             infinite, if FPEAK(2) = 0. (If nonzero, FPEAK(2) = 1.)
C             For discrete-time systems, it is assumed that the sampling
C             period is Ts = 1. If Ts <> 1, the frequency corresponding
C             to the peak gain is OMEGA/Ts.
C
C     A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
C             On entry, the leading N-by-N part of this array must
C             contain the state dynamics matrix A.
C             On exit, if EQUIL = 'S' and CKPROP = 'N', the leading
C             N-by-N part of this array contains the state dynamics
C             matrix of an equivalent, scaled system.
C             On exit, if CKPROP = 'C', DICO = 'C', and JOBE <> 'I', the
C             leading NR-by-NR part of this array contains the state
C             dynamics matrix of an equivalent reduced, possibly scaled
C             (if EQUIL = 'S') system, used to check the properness.
C             Otherwise, the array A is unchanged.
C
C     LDA     INTEGER
C             The leading dimension of the array A.  LDA >= max(1,N).
C
C     E       (input/output) DOUBLE PRECISION array, dimension (LDE,K),
C             where K is N, RANKE, or 0, if JOBE = 'G', 'C', or 'I',
C             respectively.
C             On entry, if JOBE = 'G', the leading N-by-N part of this
C             array must contain the descriptor matrix E of the system.
C             If JOBE = 'C', the leading RANKE-by-RANKE part of this
C             array must contain the full-rank block T of the descriptor
C             matrix E.
C             If JOBE = 'I', then E is assumed to be the identity matrix
C             and is not referenced.
C             On exit, if EQUIL = 'S' and CKPROP = 'N', the leading
C             K-by-K part of this array contains the descriptor matrix
C             of an equivalent, scaled system.
C             On exit, if CKPROP = 'C', DICO = 'C', and JOBE <> 'I', the
C             leading MIN(K,NR)-by-MIN(K,NR) part of this array contains
C             the descriptor matrix of an equivalent reduced, possibly
C             scaled (if EQUIL = 'S') system, used to check the
C             properness.
C             Otherwise, the array E is unchanged.
C
C     LDE     INTEGER
C             The leading dimension of the array E.
C             LDE >= MAX(1,N),     if JOBE = 'G';
C             LDE >= MAX(1,RANKE), if JOBE = 'C';
C             LDE >= 1,            if JOBE = 'I'.
C
C     B       (input/output) DOUBLE PRECISION array, dimension (LDB,M)
C             On entry, the leading N-by-M part of this array must
C             contain the system input matrix B.
C             On exit, if EQUIL = 'S' and CKPROP = 'N', the leading
C             NR-by-M part of this array contains the system input
C             matrix of an equivalent, scaled system.
C             On exit, if CKPROP = 'C', DICO = 'C', and JOBE <> 'I', the
C             leading NR-by-M part of this array contains the system
C             input matrix of an equivalent reduced, possibly scaled (if
C             EQUIL = 'S') system, used to check the properness.
C             Otherwise, the array B is unchanged.
C
C     LDB     INTEGER
C             The leading dimension of the array B.  LDB >= max(1,N).
C
C     C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
C             On entry, the leading P-by-N part of this array must
C             contain the system output matrix C.
C             On exit, if EQUIL = 'S' and CKPROP = 'N', the leading
C             P-by-NR part of this array contains the system output
C             matrix of an equivalent, scaled system.
C             On exit, if CKPROP = 'C', DICO = 'C', and JOBE <> 'I', the
C             leading P-by-NR part of this array contains the system
C             output matrix of an equivalent reduced, possibly scaled
C             (if EQUIL = 'S') system, used to check the properness.
C             Otherwise, the array C is unchanged.
C
C     LDC     INTEGER
C             The leading dimension of the array C.  LDC >= max(1,P).
C
C     D       (input) DOUBLE PRECISION array, dimension (LDD,M)
C             If JOBD = 'D' or JOBD = 'F', the leading P-by-M part of
C             this array must contain the direct transmission matrix D.
C             The array D is not referenced if JOBD = 'Z'.
C
C     LDD     INTEGER
C             The leading dimension of array D.
C             LDD >= MAX(1,P), if JOBD = 'D' or JOBD = 'F';
C             LDD >= 1,        if JOBD = 'Z'.
C
C     NR      (output) INTEGER
C             If CKPROP = 'C', DICO = 'C', and JOBE <> 'I', the order of
C             the reduced system. Otherwise, NR = N.
C
C     GPEAK   (output) DOUBLE PRECISION array, dimension (2)
C             The L-infinity norm of the system, i.e., the peak gain
C             of the frequency response (as measured by the largest
C             singular value in the MIMO case), coded in the same way
C             as FPEAK.
C
C     Tolerances
C
C     TOL     DOUBLE PRECISION array, dimension K, where K = 2, if
C             CKPROP = 'N' or DICO = 'D' or JOBE = 'I', and K = 4,
C             otherwise.
C             TOL(1) is the tolerance used to set the accuracy in
C             determining the norm.  0 <= TOL(1) < 1.
C             TOL(2) is the threshold value for magnitude of the matrix
C             elements, if EQUIL = 'S': elements with magnitude less
C             than or equal to TOL(2) are ignored for scaling. If the
C             user sets TOL(2) >= 0, then the given value of TOL(2) is
C             used. If the user sets TOL(2) < 0, then an implicitly
C             computed, default threshold, THRESH, is used instead,
C             defined by THRESH = 0.1, if MN/MX < EPS, and otherwise,
C             THRESH = MIN( 100*(MN/(EPS**0.25*MX))**0.5, 0.1 ), where
C             MX and MN are the maximum and the minimum nonzero absolute
C             value, respectively, of the elements of A and E, and EPS
C             is the machine precision (see LAPACK Library routine
C             DLAMCH). TOL(2) = 0 is not always a good choice.
C             TOL(2) < 1. TOL(2) is not used if EQUIL = 'N'.
C             TOL(3) is the tolerance to be used in rank determinations
C             when transforming (lambda*E-A,B,C), if CKPROP = 'C'. If
C             the user sets TOL(3) > 0, then the given value of TOL(3)
C             is used as a lower bound for reciprocal condition numbers
C             in rank determinations; a (sub)matrix whose estimated
C             condition number is less than 1/TOL(3) is considered to be
C             of full rank.  If the user sets TOL(3) <= 0, then an
C             implicitly computed, default tolerance, defined by
C             TOLDEF1 = N*N*EPS, is used instead.  TOL(3) < 1.
C             TOL(4) is the tolerance to be used for checking the
C             singularity of the matrices A and E when CKPROP = 'C'.
C             If the user sets TOL(4) > 0, then the given value of
C             TOL(4) is used.  If the user sets TOL(4) <= 0, then an
C             implicitly computed, default tolerance, defined by
C             TOLDEF2 = N*EPS, is used instead. The 1-norms of A and E
C             are also taken into account.  TOL(4) < 1.
C
C     Workspace
C
C     IWORK   INTEGER array, dimension (LIWORK)
C             LIWORK >= 1, if MIN(N,P,M) = 0, or B = 0, or C = 0; else
C             LIWORK >= MAX(1,N), if DICO = 'C', JOBE = 'I', and
C                       JOBD <> 'D';
C             LIWORK >= 2*N + M + P + R + 12, otherwise, where
C                       R = 0, if M + P is even,
C                       R = 1, if M + P is odd.
C             On exit, if INFO = 0, IWORK(1) returns the number of
C             iterations performed by the iterative algorithm
C             (possibly 0).
C
C     DWORK   DOUBLE PRECISION array, dimension (LDWORK)
C             On exit, if INFO = 0, DWORK(1) contains the optimal value
C             of LDWORK.
C             On exit, if  INFO = -28,  DWORK(1)  returns the minimum
C             value of LDWORK. These values are also set when LDWORK = 0
C             on entry, but no error message related to LDWORK is issued
C             by XERBLA.
C
C     LDWORK  INTEGER
C             The dimension of the array DWORK.
C             LDWORK >= 1, if MIN(M,P) = 0 or ( JOBD = 'Z' and
C                                         ( N = 0 or B = 0 or C = 0 ) );
C             LDWORK >= P*M + x, if ( ( N = 0 and MIN(M,P) > 0 )
C                          or ( B = 0 or C = 0 ) ) and JOBD <> 'Z',
C                       where
C                       x = MAX( 4*MIN(M,P) + MAX(M,P), 6*MIN(M,P) ),
C                                       if DICO = 'C',
C                       x = 6*MIN(M,P), if DICO = 'D';
C             LDWORK >= MAX( 1, N*(N+M+P+2) + MAX( N*(N+M+2) + P*M + x,
C                                                  4*N*N + 9*N ) ),
C                       if DICO = 'C', JOBE = 'I' and JOBD = 'Z'.
C             LDWORK >= MAX( 1, (N+M)*(M+P) + P*P + x,
C                            2*N*(N+M+P+1) + N + MIN(P,M) +
C                            MAX( M*(N+P) + N + x, N*N +
C                                 MAX( N*(P+M) + MAX(M,P),
C                                      2*N*N + 8*N ) ) ),
C                       if DICO = 'C', JOBE = 'I' and JOBD = 'F'.
C             The formulas for other cases, e.g., for JOBE <> 'I' or
C             CKPROP = 'C', contain additional and/or other terms.
C             The minimum value of LDWORK for all cases can be obtained
C             in DWORK(1) when LDWORK is set to 0 on entry.
C             For good performance, LDWORK must generally be larger.
C
C             If LDWORK = -1, then a workspace query is assumed;
C             the routine only calculates the optimal size of the
C             DWORK array, returns this value as the first entry of
C             the DWORK array, and no error message related to LDWORK
C             is issued by XERBLA.
C
C     ZWORK   COMPLEX*16 array, dimension (LZWORK)
C             On exit, if INFO = 0, ZWORK(1) contains the optimal
C             LZWORK.
C             On exit, if  INFO = -30,  ZWORK(1)  returns the minimum
C             value of LZWORK. These values are also set when LZWORK = 0
C             on entry, but no error message related to LZWORK is issued
C             by XERBLA.
C             If LDWORK = 0 and LZWORK = 0 are both set on entry, then
C             on exit, INFO = -30, but both DWORK(1) and ZWORK(1) are
C             set the minimum values of LDWORK and LZWORK, respectively.
C
C     LZWORK  INTEGER
C             The dimension of the array ZWORK.
C             LZWORK >= 1,  if MIN(N,M,P) = 0, or B = 0, or C = 0;
C             LZWORK >= MAX(1, (N+M)*(N+P) + 2*MIN(M,P) + MAX(M,P)),
C                           otherwise.
C             For good performance, LZWORK must generally be larger.
C
C             If LZWORK = -1, then a workspace query is assumed;
C             the routine only calculates the optimal size of the
C             ZWORK array, returns this value as the first entry of
C             the ZWORK array, and no error message related to LZWORK
C             is issued by XERBLA.
C
C     BWORK   LOGICAL array, dimension (N)
C
C     Warning Indicator
C
C     IWARN   INTEGER
C             = 0:  no warning;
C             = 1:  the descriptor system is singular. GPEAK(1) and
C                   GPEAK(2) are set to 0. FPEAK(1) and FPEAK(2) are
C                   set to 0 and 1, respectively;
C             = 2:  the descriptor system is improper. GPEAK(1) and
C                   GPEAK(2) are set to 1 and 0, respectively,
C                   corresponding to infinity. FPEAK(1) and FPEAK(2) are
C                   set similarly. This warning can only appear if
C                   CKPROP = 'C'.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value;
C             = 1:  a matrix is (numerically) singular or the Sylvester
C                   equation is very ill-conditioned, when computing the
C                   largest singular value of G(infinity) (for
C                   DICO = 'C'); the descriptor system is nearly
C                   singular; the L-infinity norm could be infinite;
C             = 2:  the (periodic) QR (or QZ) algorithm for computing
C                   eigenvalues did not converge;
C             = 3:  the SVD algorithm for computing singular values did
C                   not converge;
C             = 4:  the tolerance is too small and the algorithm did
C                   not converge; this is a warning; 
C             = 5:  other computations than QZ iteration, or reordering
C                   of eigenvalues, failed in the LAPACK Library
C                   routines DHGEQZ or DTGSEN, respectively;
C             = 6:  the numbers of "finite" eigenvalues before and after
C                   reordering differ; the threshold used might be
C                   unsuitable.
C
C     METHOD
C
C     The routine implements the method presented in [2], which is an
C     extension of the method in [1] for descriptor systems. There are
C     several improvements and refinements [3-5] to increase numerical
C     robustness, accuracy and efficiency, such as the usage of
C     structure-preserving eigenvalue computations for skew-Hamiltonian/
C     Hamiltonian eigenvalue problems in the iterative method in [2].
C
C     REFERENCES
C
C     [1] Bruinsma, N.A. and Steinbuch, M.
C         A fast algorithm to compute the H-infinity-norm of a transfer
C         function matrix.
C         Systems & Control Letters, vol. 14, pp. 287-293, 1990.
C
C     [2] Voigt, M.
C         L-infinity-Norm Computation for Descriptor Systems.
C         Diploma Thesis, Fakultaet fuer Mathematik, TU Chemnitz,
C         http://nbn-resolving.de/urn:nbn:de:bsz:ch1-201001050.
C
C     [3] Benner, P., Sima, V. and Voigt, M.
C         L-infinity-norm computation for continuous-time descriptor
C         systems using structured matrix pencils.
C         IEEE Trans. Auto. Contr., AC-57, pp.233-238, 2012.
C
C     [4] Benner, P., Sima, V. and Voigt, M.
C         Robust and efficient algorithms for L-infinity-norm
C         computations for descriptor systems.
C         7th IFAC Symposium on Robust Control Design (ROCOND'12),
C         pp. 189-194, 2012.
C
C     [5] Benner, P., Sima, V. and Voigt, M.
C         Algorithm 961: Fortran 77 subroutines for the solution of
C         skew-Hamiltonian/Hamiltonian eigenproblems.
C         ACM Trans. Math. Softw, 42, pp. 1-26, 2016.
C
C     NUMERICAL ASPECTS
C
C     If the algorithm does not converge in MAXIT = 30 iterations
C     (INFO = 4), the tolerance must be increased, or the system is
C     improper.
C
C     FURTHER COMMENTS
C
C     Setting POLES = 'P' usually saves some computational effort. The
C     number of poles used is defined by the parameters BM, BNEICD,
C     BNEICM, BNEICX, BNEIR and SWNEIC.
C     Both real and complex optimal workspace sizes are computed if
C     either LDWORK = -1 or LZWORK = -1.
C
C     CONTRIBUTORS
C
C     M. Voigt, Max Planck Institute for Dynamics of Complex Technical
C     Systems, March 2011.
C     V. Sima, Research Institute for Informatics, Bucharest, Dec. 2011.
C     Partly based on the SLICOT Library routine AB13DD by D. Sima and
C     V. Sima.
C
C     REVISIONS
C
C     V. Sima, Mar. 2012, Apr. 2012, May 2012, June 2012, June-Nov.
C     2022, Jan. 2023, Mar.-Aug. 2023, Oct. 2023 - Jan. 2024.
C     M. Voigt, Apr. 2017, Sep. 2017.
C
C     KEYWORDS
C
C     H-infinity optimal control, robust control, system norm.
C
C     ******************************************************************
C
C     .. Parameters ..
C     BM specifies the maximum number of midpoints to be used in the
C     iterative part of the algorithm if POLES = 'P'; similarly,
C     BNEICD, BNEICM, and BNEICX specify the number of complex poles
C     (with positive imaginary part) to be used as test frequencies,
C     and BNEIR has the same purpose for real poles;
C     MAXIT is the maximum number of iterations;
C     SWNEIC is the system order when the number of complex poles to be
C     used is increased.
C                        
      INTEGER            BM, BNEICD, BNEICM, BNEICX, BNEIR, MAXIT,
     $                   SWNEIC
      PARAMETER          ( BM = 2, BNEICD = 10, BNEICM = 45,
     $                     BNEICX = 60, BNEIR = 3, MAXIT = 30,
     $                     SWNEIC = 300 )
      DOUBLE PRECISION   ZERO, P1, P25, ONE, TWO, FOUR, TEN, HUNDRD,
     $                   THOUSD
      PARAMETER          ( ZERO = 0.0D+0, P1  = 0.1D+0, P25  = 0.25D+0,
     $                     ONE  = 1.0D+0, TWO = 2.0D+0, FOUR = 4.0D+0,
     $                     TEN  = 1.0D+1, HUNDRD = 1.0D+2,
     $                     THOUSD = 1.0D+3 )
      COMPLEX*16         CONE
      PARAMETER          ( CONE  = ( 1.0D+0, 0.0D+0 ) )
C     ..
C     .. Scalar Arguments ..
      CHARACTER          CKPROP, DICO, EQUIL, JOBD, JOBE, POLES, REDUCE
      INTEGER            INFO, IWARN, LDA, LDB, LDC, LDD, LDE, LDWORK,
     $                   LZWORK, M, N, NR, P, RANKE
C     ..
C     .. Array Arguments ..
      COMPLEX*16         ZWORK(  * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * ),
     $                   D( LDD, * ), DWORK(  * ), E( LDE, * ),
     $                   FPEAK(  2 ), GPEAK(  2 ), TOL( * )
      INTEGER            IWORK(  * )
      LOGICAL            BWORK(  * )
C     ..
C     .. Local Scalars ..
      CHARACTER          EIGENV, JBDX, JOB, JOBEIG, JOBSYS, NCSING,
     $                   NEQUIL, NOVECT, NTRAN, QZVECT, RESTOR, SVEC,
     $                   TRANS, UPDATE, VECT
      LOGICAL            ALLPOL, CASE0, CASE1, CASE2, CASE3, CMPRE,
     $                   DISCR, FULLRD, GENE, ILASCL, ILESCL, IND1,
     $                   ISPROP, LEQUIL, LINF, LQUERY, NCMPRE, NODYN,
     $                   NSRT, REALW, UNITE, USEPEN, WCKPRP, WITHD,
     $                   WITHE, WNRMD, WREDUC, ZEROD
      INTEGER            I, I0, I1, I2, IA, IAS, IB, IBS, IBT, IBV, IC,
     $                   ICI, ICU, ICW, ID, IE, IERR, IES, IH, IH12,
     $                   IH22, IHC, IHI, II, IJ, IJ12, ILFT, ILO, IM,
     $                   IMIN, IQ, IR, IRHT, IRLW, IS, ISB, ISC, ISL,
     $                   IT, IT12, ITAU, ITER, IU, IV, IWRK, IZ, J, K,
     $                   L, LIW, LW, M0, MAXCWK, MAXPM, MAXWRK, MINCWK,
     $                   MINPM, MINWRK, MNW13X, MNWSVD, N1, N2, NBLK,
     $                   NBLK2, NC, NE, NEI, NEIC, NEIR, NINF, NK, NN,
     $                   NR2, NWS, ODW13X, ODWSVD, P0, PM, PMQ, Q, QP,
     $                   R, RNKE, SDIM, SDIM1, TN, TNR, WR13ID
      DOUBLE PRECISION   ANRM, ANRMTO, BIGNUM, BNORM, BOUND, CND, CNORM,
     $                   DIF, ENRM, ENRMTO, EPS, FPEAKI, FPEAKS, GAMMA,
     $                   GAMMAL, GAMMAS, MAXRED, OMEGA, OZ, PI, RAT,
     $                   RCOND, SAFMAX, SAFMIN, SCL, SMLNUM, STOL, SV1,
     $                   SVP, TD, TEPS, THRESH, TM, TMP, TMR, TOL1,
     $                   TOL2, TOLDEF, TOLER, TOLN, TOLP, TZER, WMAX,
     $                   WRMIN
C     ..
C     .. Local Arrays ..
      INTEGER            IDUM( 1 )
      DOUBLE PRECISION   DUM(  3 ), GAM( 1 ), MGAM( 1 ), ONES( 1 ),
     $                   TOLI( 3 )
C     ..
C     .. External Functions ..
      LOGICAL            AB13ID, LSAME
      DOUBLE PRECISION   AB13DX, DLAMCH, DLANGE, DLANHS, DLANTR, DLAPY2,
     $                   MA02SD
      EXTERNAL           AB13DX, AB13ID, DLAMCH, DLANGE, DLANHS, DLANTR,
     $                   DLAPY2, LSAME,  MA02SD
C     ..
C     .. External Subroutines ..
      EXTERNAL           DAXPY,  DCOPY,  DGEBAL, DGECON, DGEHRD, DGEMM,
     $                   DGEQRF, DGESVD, DGETRF, DGETRS, DGGBAK, DGGBAL,
     $                   DHGEQZ, DHSEQR, DLABAD, DLACPY, DLADIV, DLASCL,
     $                   DLASET, DLASRT, DORMHR, DORMQR, DSCAL,  DSWAP,
     $                   DSYRK,  DTGSEN, DTRCON, DTRSM, MA02AD,  MB01SD,
     $                   MB02RD, MB02SD, MB02TD, MB03XD, MB04BP, SB04OD,
     $                   TB01ID, TG01AD, TG01BD, XERBLA, ZGESVD
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, ATAN, ATAN2, COS, DBLE, INT, LOG, MAX,
     $                   MIN, MOD, SIN, SQRT
C     ..
C     .. Executable Statements ..
C
C     Test the input scalar parameters.
C
      NN     = N*N
      MINPM  = MIN( P, M )
      MAXPM  = MAX( P, M )
      IWARN  = 0
      INFO   = 0
      DISCR  = LSAME( DICO,   'D' )
      UNITE  = LSAME( JOBE,   'I' )
      GENE   = LSAME( JOBE,   'G' )
      CMPRE  = LSAME( JOBE,   'C' )
      LEQUIL = LSAME( EQUIL,  'S' )
      WITHD  = LSAME( JOBD,   'D' )
      FULLRD = LSAME( JOBD,   'F' )
      ZEROD  = LSAME( JOBD,   'Z' )
      WCKPRP = LSAME( CKPROP, 'C' )
      WREDUC = LSAME( REDUCE, 'R' )
      ALLPOL = LSAME( POLES,  'A' )
      WITHE  = GENE .OR. CMPRE
      LQUERY = LDWORK.EQ.-1 .OR. LZWORK.EQ.-1
C
      IF( .NOT. ( DISCR .OR. LSAME( DICO, 'C' ) ) ) THEN
         INFO = -1
      ELSE IF( .NOT. ( WITHE  .OR. UNITE ) ) THEN
         INFO = -2
      ELSE IF( .NOT. ( LEQUIL .OR. LSAME( EQUIL,  'N' ) ) ) THEN
         INFO = -3
      ELSE IF( .NOT. ( WITHD  .OR. FULLRD .OR. ZEROD ) ) THEN
         INFO = -4
      ELSE IF( .NOT. ( WCKPRP .OR. LSAME( CKPROP, 'N' ) ) ) THEN
         IF( .NOT.( DISCR .OR. UNITE ) )
     $      INFO = -5
      ELSE IF( .NOT. ( WREDUC .OR. LSAME( REDUCE, 'N' ) ) ) THEN
         IF( WCKPRP )
     $      INFO = -6
      ELSE IF( .NOT. ( ALLPOL .OR. LSAME( POLES, 'P' ) ) ) THEN
         INFO = -7
      ELSE IF( N.LT.0 ) THEN
         INFO = -8
      ELSE IF( M.LT.0 ) THEN
         INFO = -9
      ELSE IF( P.LT.0 ) THEN
         INFO = -10
      ELSE IF( CMPRE .AND. ( RANKE.LT.0 .OR. RANKE.GT.N ) ) THEN
         INFO = -11
      ELSE IF( MIN( FPEAK( 1 ), FPEAK( 2 ) ).LT.ZERO ) THEN
         INFO = -12
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -14
      ELSE IF( LDE.LT.1 .OR. ( GENE  .AND. LDE.LT.N ) .OR.
     $                       ( CMPRE .AND. LDE.LT.RANKE ) ) THEN
         INFO = -16
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -18
      ELSE IF( LDC.LT.MAX( 1, P ) ) THEN
         INFO = -20
      ELSE IF( LDD.LT.1 .OR. ( .NOT.ZEROD .AND. LDD.LT.P ) ) THEN
         INFO = -22
      ELSE IF( TOL( 1 ).LT.ZERO .OR. TOL( 1 ).GE.ONE ) THEN
         INFO = -25
      ELSE IF( .NOT.LEQUIL .AND. TOL( 2 ).GE.ONE ) THEN
         INFO = -25
      ELSE IF( .NOT.DISCR .AND. WITHE .AND. WCKPRP .AND.
     $         ( TOL( 3 ).GE.ONE .OR. TOL( 4 ).GE.ONE ) ) THEN
         INFO = -25
      ELSE
         NODYN = N.EQ.0
         IF( .NOT.NODYN ) THEN
            BNORM = DLANGE( '1-norm', N, M, B, LDB, DWORK )
            CNORM = DLANGE( '1-norm', P, N, C, LDC, DWORK )
            NODYN = MIN( BNORM, CNORM ).EQ.ZERO
         END IF
C
C        Option FULLRD is useless for discrete-time systems.
C
         IF( DISCR .AND. FULLRD )
     $      FULLRD = .FALSE.
         WITHD = WITHD .OR. FULLRD
C
C        Compute workspace.
C
C        Note: The workspace requirement is dominated by the solution of
C              the generalized skew-Hamiltonian/Hamiltonian eigenvalue
C              problem, when needed.
C
         MINCWK = 1
         MAXCWK = 1
         IF( MINPM.EQ.0 .OR. ( NODYN .AND. ZEROD ) ) THEN
            MINWRK = 1
            MAXWRK = 1
         ELSE
            USEPEN = WITHE .OR. DISCR
            EIGENV = 'E'
            NTRAN  = 'N'
            NOVECT = 'N'
            SVEC   = NOVECT
            TRANS  = 'T'
C
            IF( CMPRE ) THEN
               CMPRE  = RANKE.LT.N
               NCMPRE = .NOT.CMPRE
            ELSE
               NCMPRE = .FALSE.
               RANKE  = N
            END IF
C
            IF( DISCR .OR. UNITE ) THEN
               WCKPRP = .FALSE.
               WREDUC = .FALSE.
            END IF
C
C           Optimal and minimum local workspace for DGESVD/ZGESVD and
C           AB13DX.
C
            IF( LQUERY ) THEN
               CALL DGESVD( NOVECT, NOVECT, P, M, DWORK, P, DWORK,
     $                      DWORK, P, DWORK, M, DWORK, -1, IERR )
               ODWSVD = INT( DWORK( 1 ) )
               ODW13X = MINPM + ODWSVD
            END IF
C
            IU = 0
            IF( NODYN ) THEN
               IF( WITHD ) THEN
                  MNWSVD = MAX( 3*MINPM + MAXPM, 5*MINPM )
                  MINWRK = P*M + MINPM + MNWSVD
                  IF( LQUERY )
     $               MAXWRK = MAX( MINWRK, P*M + MINPM + ODWSVD )
               ELSE
                  MAXWRK = 1
               END IF
            ELSE
               I0 = ( N + M )*( N + P )
               IE = 0
               PM = P + M
               IF( LQUERY ) THEN
                  CALL ZGESVD( NOVECT, NOVECT, P, M, ZWORK, P, DWORK,
     $                         ZWORK, P, ZWORK, M, ZWORK, -1, DWORK,
     $                         IERR )
                  MAXCWK = I0 + INT( ZWORK( 1 ) )
               END IF
C
               MINCWK = I0 + 2*MINPM + MAXPM
               MNWSVD = 5*MINPM
               IF( DISCR ) THEN
                  MINWRK = 0
                  MAXWRK = 0
               ELSE
                  MNWSVD = MAX( MNWSVD, 3*MINPM + MAXPM )
                  IF( WITHD ) THEN
C
C                    Workspace for finding maximum singular value of D.
C
                     IF( UNITE ) THEN
                        IU   = N*PM + MINPM
                        IE   = IU
                        IWRK = IU + P*P + PM*M
                        SVEC = 'AllVec'
                        IF( LQUERY ) THEN
                           CALL DGESVD( SVEC, SVEC, P, M, DWORK, P,
     $                                  DWORK, DWORK, P, DWORK, M,
     $                                  DWORK, -1, IERR )
                           MAXWRK = IWRK + INT( DWORK( 1 ) )
                        END IF
                     ELSE
                        IWRK   = MINPM + P*M
                        MAXWRK = 0
                     END IF
                     MINWRK = IWRK + MNWSVD
                  ELSE
                     MINWRK = 0
                     MAXWRK = 0
                  END IF
               END IF
               MNW13X = MINPM + MNWSVD
C
               TN  = 2*N
               IB  = IE + NN
               IR  = IB + N*PM
               IBT = IR + TN
C
               IF( WITHE ) THEN
                  NSRT = CMPRE .OR. DISCR
                  IB   = IB  + NN
                  IR   = IR  + NN
                  IBT  = IBT + NN
                  IF( WCKPRP ) THEN
                     NEQUIL = 'NoEquil'
                     NCSING = 'NCkSing'
                     RESTOR = 'No'
                     IF( CMPRE ) THEN
                        JOBSYS = 'NotRed'
                     ELSE
                        JOBSYS = 'Reduce'
                     END IF
C
C                    Workspace for checking properness.
C
                     I0 = NN + 4*N
                     I1 = N  + MAXPM
                     IF( P.EQ.M ) THEN
                        I2 = IR
                     ELSE
                        I2 = IB + TN*MAXPM
                     END IF
C
                     IF( WREDUC ) THEN
C
C                       A reduced-order system is used for finding norm.
C
                        JOBEIG = 'Allnmn'
                        UPDATE = 'Upd'
                        I0     = I0 + 4
                        IF( CMPRE ) THEN
                           WR13ID = MAX( I0, I1 )
                        ELSE
                           WR13ID = MAX( I0, 2*( I1 - 1 ) )
                        END IF
                     ELSE
C
C                       Original system is used for finding norm.
C
                        JOBEIG = 'Infnmn'
                        UPDATE = 'NoUpd'
                        IF( CMPRE ) THEN
                           WR13ID = 4*N + 4
                        ELSE
                           WR13ID = MAX( I0, 2*( I1 - 1 ), 8 )
                        END IF
                     END IF
                     MINWRK = MAX( MINWRK, I2 + WR13ID )
                     IF( LQUERY ) THEN
                        DUM( 1 ) = ZERO
                        DUM( 2 ) = ZERO
                        DUM( 3 ) = ZERO
                        ISPROP = AB13ID( JOBSYS, JOBEIG, NEQUIL, NCSING,
     $                                   RESTOR, UPDATE, N, M, P, DWORK,
     $                                   N, DWORK, N, DWORK, N, DWORK,
     $                                   MAXPM, N, RNKE, DUM, IWORK,
     $                                   DWORK, -1, IWARN, IERR )
                        MAXWRK = MAX( MAXWRK, I2 + INT( DWORK( 1 ) ) )
                     END IF
                  END IF
C
                  IF( .NOT.DISCR ) THEN
                     I1 = ( N + P )*M
                     IF( CMPRE ) THEN
C
C                       Workspace for finding the largest singular value
C                       of G(infinity).
C
                        MINWRK = MAX( MINWRK, IR + MAX( N*( N + M + 4 ),
     $                                                  I1 + MNW13X ) )
                     END IF
                     IF( LQUERY )
     $                  MAXWRK = MAX( MAXWRK, I1 + ODW13X, MINWRK )
                  END IF
C
C                 Workspace for computing Hessenberg-triangular form.
C
                  I1 = IBT + N
                  IF( NSRT ) THEN
                     MINWRK = MAX( MINWRK, I1 + MAX( M, 2*NN + N ) )
                  ELSE
                     I1     = I1 + 2*NN + TN
                     MINWRK = MAX( MINWRK, I1 + TN )
                  END IF
                  IF( LQUERY ) THEN
                     CALL DGEQRF( N, N, A, LDA, DWORK, DWORK, -1, IERR )
                     CALL DORMQR( 'Left', TRANS, N, N, N, DWORK, N,
     $                            DWORK, DWORK, N, DUM, -1, IERR )
                     IF( NSRT ) THEN
                        CALL DORMQR( 'Left', TRANS, N, M, N, DWORK, N,
     $                               DWORK, DWORK, N, DUM( 2 ), -1,
     $                               IERR )
                     ELSE
                        CALL DORMQR( 'Right', NTRAN, N, N, N, DWORK, N,
     $                               DWORK, DWORK, N, DUM( 2 ), -1,
     $                               IERR )
                     END IF
                     MAXWRK = MAX( I1 + MAX( INT( DWORK( 1 ) ),
     $                                       INT( DUM( 1 ) ),
     $                                       INT( DUM( 2 ) ) ), MAXWRK )
                  END IF
C
                  IF( .NOT.NSRT ) THEN
C
C                    Workspace for moving finite eigenvalues to the top.
C
                     MINWRK = MAX( MINWRK, I1 + MAX( 4*N + 16,
     $                                               N*MAXPM ) )
C
C                    Workspace for finding the largest singular value of
C                    G(infinity).
C
                     I1     = IBT + N + P*M
                     I2     = I1  + ( N - 1 )*PM
                     MINWRK = MAX( MINWRK,
     $                             I2 + ( N - 1 )*( N + M + 2 ) )
                     IF( LQUERY ) THEN
                        CALL SB04OD( 'NoReduction', NTRAN, 'Fro', N, N,
     $                               DWORK, N, DWORK, N, DWORK, N,
     $                               DWORK, N, DWORK, N, DWORK, N, SCL,
     $                               DIF, DWORK, 1, DWORK, 1, DWORK, 1,
     $                               DWORK, 1, IWORK, DWORK, -1, IERR )
                        MAXWRK = MAX( MAXWRK, I2 + N*( N + 1 ) / 2 +
     $                                        INT( DWORK( 1 ) ) )
                     END IF
                     IF( WITHD ) THEN
                        MINWRK = MAX( MINWRK, I1 + MNW13X )
                        IF( LQUERY )
     $                     MAXWRK = MAX( MAXWRK, I1 + ODW13X )
                     END IF
                  END IF
C
               ELSE
                  II = IR + N
                  IF( LQUERY ) THEN
                     CALL DGEHRD( N, 1, N, DWORK, N, DWORK, DWORK, -1,
     $                            IERR )
                     CALL DORMHR( 'Left', TRANS, N, M, 1, N, DWORK, N,
     $                            DWORK, DWORK, N, DUM, -1, IERR )
                     CALL DORMHR( 'Right', NTRAN, P, N, 1, N, DWORK, N,
     $                            DWORK, DWORK, P, DUM( 2 ), -1, IERR )
                     CALL DHSEQR( EIGENV, NOVECT, N, 1, N, DWORK, N,
     $                            DWORK, DWORK, DWORK, 1, DUM( 3 ), -1,
     $                            IERR )
                     MAXWRK = MAX( II  + MAX( INT( DWORK( 1 ) ),
     $                                        INT( DUM( 1 ) ),
     $                                        INT( DUM( 2 ) ) ),
     $                             IBT + NN + INT( DUM( 3 ) ), MAXWRK )
                  END IF
               END IF
C
C              Workspace for finding eigenvalues on the boundary of
C              stability domain and the maximum singular value of G on
C              test frequencies.
C
               IWRK = IBT + N
               IF( WITHE )
     $            IWRK = IWRK + N
               IF( .NOT.DISCR )
     $            IWRK = IWRK + NN + M*( N + P )
               MINWRK = MAX( MINWRK, IWRK + MNW13X )
               IF( LQUERY )
     $            MAXWRK = MAX( MAXWRK, IWRK + ODW13X )
C
C              Workspace for the modified gamma iteration.
C
               IF( UNITE .AND. .NOT.DISCR .AND. ( ZEROD .OR. FULLRD ) )
     $               THEN
                  I0 = 2*NN + N
                  I1 = I0 + 7*N
                  I2 = IBT + I0
                  IF( LQUERY ) THEN
                     CALL MB03XD( 'Both', EIGENV, NOVECT, NOVECT, N,
     $                            DWORK, N, DWORK, N, DWORK, N, DUM, 1,
     $                            DUM, 1, DUM, 1, DUM, 1, DWORK, DWORK,
     $                            1, DWORK, DWORK, -1, IERR )
                     MAXWRK = MAX( MAXWRK, I2 + NN + N +
     $                             INT( DWORK( 1 ) ), II + ODW13X )
                  END IF
                  IF( ZEROD ) THEN
                     I2 = I2 + I1
                  ELSE
                     I2 = I2 + MAX( MAXPM + N*PM, I1 )
                  END IF
                  MINWRK = MAX( MINWRK, I2 )
               ELSE
                  R     = MOD( PM, 2 )
                  NBLK  = N + ( PM + R ) / 2
                  NBLK2 = NBLK*NBLK
                  I0    = IR + 7*NBLK2 + 5*NBLK
                  L     = 8*NBLK
                  IF( MOD( NBLK, 2 ).EQ.0 )
     $               L = L + 4
                  MINWRK = MAX( MINWRK, I0 + 4*NBLK2 + MAX( L, 36 ) )
               END IF
C
               IF( LQUERY ) THEN
                  MAXWRK = MAX( MINWRK, MAXWRK )
                  MAXCWK = MAX( MINCWK, MAXCWK )
               END IF
            END IF
         END IF
C
         IF( LQUERY ) THEN
            DWORK( 1 ) = MAXWRK
            ZWORK( 1 ) = MAXCWK
            RETURN
         ELSE
            IF( LDWORK.LT.MINWRK ) THEN
               INFO = -28
               DWORK( 1 ) = MINWRK
            END IF
            IF( LZWORK.LT.MINCWK ) THEN
               INFO = -30
               ZWORK( 1 ) = MINCWK
            END IF
         END IF
C
      END IF
C
      IF( INFO.NE.0 ) THEN
         IF( LDWORK.NE.0 .AND. LZWORK.NE.0 )
     $      CALL XERBLA( 'AB13HD', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      ITER = 0
C
      IF( MINPM.EQ.0 ) THEN
         GPEAK( 1 ) = ZERO
         GPEAK( 2 ) = ONE
         FPEAK( 1 ) = ZERO
         FPEAK( 2 ) = ONE
         DWORK( 1 ) = ONE
         ZWORK( 1 ) = CONE
         IWORK( 1 ) = ITER
         RETURN
      END IF
C
C     Determine the maximum singular value of G(infinity) = D,
C     if JOBD <> 'Z' and (N = 0, or B = 0, or C = 0, or
C     (DICO = 'C' and (JOBE = 'I' or (JOBE = 'C' and RANKE = N))) ).
C
C     If DICO = 'C' and JOBE = 'I', the full SVD of D, D = U*S*V', is
C     computed; then B and C are updated in the workspace and saved for
C     later use.
C
C     (Note: Comments in the code beginning "Workspace:" describe the
C     minimal amount of real workspace needed at that point in the
C     code, as well as the preferred amount for good performance.
C     NB refers to the optimal block size for the immediately
C     following subroutine, as returned by ILAENV.)
C
      IF( WITHD ) THEN
         OZ    = ONE
         WNRMD = NODYN .OR. ( ( UNITE .OR. NCMPRE ) .AND. .NOT.DISCR )
         IF( WNRMD ) THEN
            IU = IU + 1
            IF( .NOT.NODYN .AND. UNITE ) THEN
               IBV = 1
               ICU = IBV + N*M
               IS  = ICU + P*N
               IV  = IU  + P*P
               ID  = IV  + M*M
            ELSE
               IS  = 1
               IV  = 1
               ID  = 1 + MINPM
            END IF
            IWRK = ID + P*M
C
C           Workspace: need   P*M + MIN(P,M) + V +
C                             MAX( 3*MIN(P,M) + MAX(P,M), 5*MIN(P,M) ),
C                             where V = N*(M+P) + P*P + M*M, if
C                                       DICO = 'C', JOBE = 'I',
C                                       JOBD <> 'Z', B <> 0, and C <> 0,
C                                   V = 0, if (DICO = 'C', JOBE = 'C',
C                                       RANKE = N, JOBD <> 'Z'), or
C                                       B = 0, or C = 0;
C                      prefer larger.
C
            CALL DLACPY( 'Full', P, M, D, LDD, DWORK( ID ), P )
            CALL DGESVD( SVEC, SVEC, P, M, DWORK( ID ), P, DWORK( IS ),
     $                   DWORK( IU ), P, DWORK( IV ), M, DWORK( IWRK ),
     $                   LDWORK-IWRK+1, IERR )
            IF( IERR.GT.0 ) THEN
               INFO = 3
               RETURN
            END IF
            GAMMAL = DWORK( IS )
            MAXWRK = INT( DWORK( IWRK ) ) + IWRK - 1
C
            IF( .NOT.NODYN .AND. UNITE ) THEN
C
C              Standard continuous-time case, D <> 0: Compute B*V and
C              C'*U in the workspace.
C
               CALL DGEMM( NTRAN, TRANS, N, M, M, ONE, B, LDB,
     $                     DWORK( IV ), M, ZERO, DWORK( IBV ), N )
               CALL DGEMM( TRANS, NTRAN, N, P, P, ONE, C, LDC,
     $                     DWORK( IU ), P, ZERO, DWORK( ICU ), N )
C
C              U, V, and D copy are no longer needed: free their
C              memory space.
C              Total workspace here: need   N*(M+P) + MIN(P,M).
C
C              Save extremal singular values.
C
               SV1 = GAMMAL
               SVP = DWORK( IS+MINPM-1 )
            END IF
         ELSE
            GAMMAL = ZERO
            MAXWRK = 1
         END IF
      ELSE
         WNRMD  = WITHD
         OZ     = ZERO
         GAMMAL = ZERO
         MAXWRK = 1
      END IF
C
C     Quick return if possible.
C
      IF( NODYN ) THEN
         GPEAK( 1 ) = GAMMAL
         GPEAK( 2 ) = ONE
         FPEAK( 1 ) = ZERO
         FPEAK( 2 ) = ONE
         DWORK( 1 ) = MAXWRK
         ZWORK( 1 ) = CONE
         IWORK( 1 ) = ITER
         RETURN
      END IF
C
C     Get machine constants.
C
      EPS    = DLAMCH( 'Precision' )
      SAFMIN = DLAMCH( 'Safe minimum' )
      SAFMAX = ONE / SAFMIN
      CALL DLABAD( SAFMIN, SAFMAX )
      SMLNUM = SQRT( SAFMIN ) / EPS
      BIGNUM = ONE / SMLNUM
      TOLER  = SQRT( EPS )
      STOL   = SQRT( TOLER )
C
C     Initiate the transformation of the system to an equivalent one,
C     to be used for eigenvalue computations.
C
C     Additional workspace: need   N*N + N*d + 2*N, if JOBE = 'I';
C     (from IE)                  2*N*N + N*d + 2*N, otherwise,
C                           where  d = M + P.
C
      IE  = IE  + 1
      IB  = IB  + 1
      IR  = IR  + 1
      IBT = IBT + 1
      IF( WITHE ) THEN
         IA = IE + NN
      ELSE
         IA = IE
      END IF
      IC = IB + N*M
      II = IR + N
C
C     Scale A if maximum element is outside the range [SMLNUM,BIGNUM].
C
      ANRM   = DLANGE( 'Max', N, N, A, LDA, DWORK )
      ILASCL = .FALSE.
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
         ANRMTO = SMLNUM
         ILASCL = .TRUE.
      ELSE IF( ANRM.GT.BIGNUM ) THEN
         ANRMTO = BIGNUM
         ILASCL = .TRUE.
      END IF
      IF( ILASCL )
     $   CALL DLASCL( 'General', 0, 0, ANRM, ANRMTO, N, N, A, N, IERR )
C
      NR  = N
      NR2 = NN
      N1  = RANKE
C
      IF( WITHE ) THEN
C
C        Descriptor system.
C
C        Scale E if maximum element is outside the range
C        [SMLNUM,BIGNUM].
C
         ENRM   = DLANGE( 'Max', N1, N1, E, LDE, DWORK )
         ILESCL = .FALSE.
         IF( ENRM.GT.ZERO .AND. ENRM.LT.SMLNUM ) THEN
            ENRMTO = SMLNUM
            ILESCL = .TRUE.
         ELSE IF( ENRM.GT.BIGNUM ) THEN
            ENRMTO = BIGNUM
            ILESCL = .TRUE.
         END IF
         IF( ILESCL )
     $      CALL DLASCL( 'General', 0, 0, ENRM, ENRMTO, N1, N1, E, LDE,
     $                   IERR )
         CALL DLACPY( 'Full', N1, N1, E, LDE, DWORK( IE ), N )
C
         IF( CMPRE ) THEN
            CALL DLASET( 'Full', N-N1, N1, ZERO, ZERO, DWORK( IE+N1 ),
     $                   N )
            CALL DLASET( 'Full', N, N-N1, ZERO, ZERO, DWORK( IE+N*N1 ),
     $                   N )
         END IF
C
C        Set the tolerances.
C
         IF( LEQUIL ) THEN
            THRESH = TOL( 2 )
            IF( THRESH.LT.ZERO ) THEN
               TM  = MAX( ANRM, ENRM )
               TMP = MIN( MA02SD( N,  N,  A, LDA ),
     $                    MA02SD( N1, N1, E, LDE ) )
               IF( ( TMP / TM ).LT.EPS ) THEN
                  THRESH = P1
               ELSE
                  THRESH = MIN( HUNDRD*SQRT( TMP ) / SQRT( TM*STOL ), P1
     $                        )
               END IF
            END IF
            TOLI( 3 ) = THRESH
         END IF
         IF( WCKPRP ) THEN
            TOLDEF = TOL( 3 )
            IF( TOLDEF.LE.ZERO )
     $          TOLDEF = NN*EPS
            TZER = TOL( 4 )
            IF( TZER.LE.ZERO )
     $          TZER = N*EPS
            TOLI( 1 ) = TOLDEF
            TOLI( 2 ) = TZER
         END IF
C
C        Equilibrate the system, if required.
C
C        Additional workspace: need   8*N (from IA).
C
         IF( LEQUIL ) THEN
            IWRK = IA + TN
            IF( CMPRE ) THEN
               CALL TG01AD( 'All', N, N, M, P, THRESH, A, LDA,
     $                      DWORK( IE ), N, B, LDB, C, LDC, DWORK( IA ),
     $                      DWORK( IA+N ), DWORK( IWRK ), IERR )
               CALL DLACPY( 'Full', N1, N1, DWORK( IE ), N, E, LDE )
            ELSE
               CALL TG01AD( 'All', N, N, M, P, THRESH, A, LDA, E, LDE,
     $                      B, LDB, C, LDC, DWORK( IA ), DWORK( IA+N ),
     $                      DWORK( IWRK ), IERR )
               CALL DLACPY( 'Full', N1, N1, E, LDE, DWORK( IE ), N )
            END IF
         END IF
C
         IF( WCKPRP ) THEN
C
C           Check properness on the transformed system.
C
C           Additional workspace: need  (from IB)
C                  MAX(N*N+4*N+4,2*(MAX(M,P)+N-1)) + y,
C                                 if JOBSYS = 'R' and REDUCE = 'R',
C                  MAX(4*N+4,2*(MAX(M,P)+N-1),N*N+4*N) + y,
C                                 if JOBSYS = 'R' and REDUCE = 'N',
C                  MAX(N*N+4*N+4,MAX(M,P)+N) + y,
C                                 if JOBSYS = 'N' and REDUCE = 'R',
C                  4*N + 4 + y,   if JOBSYS = 'N' and REDUCE = 'N';
C                  where y = N*(M+P),      if M =  P,
C                        y = 2*N*MAX(M,P), if M <> P;
C                                 prefer larger.
C
C           Integer workspace: need  2*N+MAX(M,P)+7, if JOBSYS = 'R';
C                                    N,              if JOBSYS = 'N'.
C
C           Saving and restoring is not used.
C
            ICW = IB + N*MAXPM
            IF( P.EQ.M ) THEN
               IWRK = IR
            ELSE
               IWRK = ICW + MAXPM*N
            END IF
C
            IF( WREDUC ) THEN
               IF( CMPRE ) THEN
                  IF( P.EQ.M ) THEN
                     ISPROP = AB13ID( JOBSYS, JOBEIG, NEQUIL, NCSING,
     $                                RESTOR, UPDATE, N, M, P, A, LDA,
     $                                DWORK( IE ), N, B, LDB, C, LDC,
     $                                NR, RNKE, TOLI, IWORK,
     $                                DWORK( IWRK ), LDWORK-IWRK+1,
     $                                IWARN, INFO )
                     CALL DLACPY( 'Full', NR, M, B, LDB, DWORK( IB ),
     $                            N )
                     CALL DLACPY( 'Full', P, NR, C, LDC, DWORK( IC ),
     $                            P )
                  ELSE IF( P.LT.M ) THEN
                     CALL DLACPY( 'Full', P, N, C, LDC, DWORK( ICW ),
     $                            MAXPM )
                     ISPROP = AB13ID( JOBSYS, JOBEIG, NEQUIL, NCSING,
     $                                RESTOR, UPDATE, N, M, P, A, LDA,
     $                                DWORK( IE ), N, B, LDB,
     $                                DWORK( ICW ), MAXPM, NR, RNKE,
     $                                TOLI, IWORK, DWORK( IWRK ),
     $                                LDWORK-IWRK+1, IWARN, INFO )
                     CALL DLACPY( 'Full', NR, M, B, LDB, DWORK( IB ),
     $                            N )
                     CALL DLACPY( 'Full', P, NR, DWORK( ICW ), MAXPM, C,
     $                            LDC )
                     CALL DLACPY( 'Full', P, NR, C, LDC, DWORK( IC ),
     $                            P )
                  ELSE
                     CALL DLACPY( 'Full', N, M, B, LDB, DWORK( IB ), N )
                     ISPROP = AB13ID( JOBSYS, JOBEIG, NEQUIL, NCSING,
     $                                RESTOR, UPDATE, N, M, P, A, LDA,
     $                                DWORK( IE ), N, DWORK( IB ), N, C,
     $                                LDC, NR, RNKE, TOLI, IWORK,
     $                                DWORK( IWRK ), LDWORK-IWRK+1,
     $                                IWARN, INFO )
                     CALL DLACPY( 'Full', NR, M, DWORK( IB ), N, B,
     $                            LDB )
                     CALL DLACPY( 'Full', P, NR, C, LDC, DWORK( IC ),
     $                            P )
                  END IF
                  N1 = MIN( NR, N1 )
                  CALL DLACPY( 'Full', N1, N1, DWORK( IE ), N, E, LDE )
               ELSE
                  IF( P.EQ.M ) THEN
                     ISPROP = AB13ID( JOBSYS, JOBEIG, NEQUIL, NCSING,
     $                                RESTOR, UPDATE, N, M, P, A, LDA,
     $                                E, LDE, B, LDB, C, LDC, NR, RNKE,
     $                                TOLI, IWORK, DWORK( IWRK ),
     $                                LDWORK-IWRK+1, IWARN, INFO )
                     CALL DLACPY( 'Full', NR, M, B, LDB, DWORK( IB ),
     $                            N )
                     CALL DLACPY( 'Full', P, NR, C, LDC, DWORK( IC ),
     $                            P )
                  ELSE IF( P.LT.M ) THEN
                     CALL DLACPY( 'Full', P, N, C, LDC, DWORK( ICW ),
     $                            MAXPM )
                     ISPROP = AB13ID( JOBSYS, JOBEIG, NEQUIL, NCSING,
     $                                RESTOR, UPDATE, N, M, P, A, LDA,
     $                                E, LDE, B, LDB, DWORK( ICW ),
     $                                MAXPM, NR, RNKE, TOLI, IWORK,
     $                                DWORK( IWRK ), LDWORK-IWRK+1,
     $                                IWARN, INFO )
                     CALL DLACPY( 'Full', NR, M, B, LDB, DWORK( IB ),
     $                            N )
                     CALL DLACPY( 'Full', P, NR, DWORK( ICW ), MAXPM, C,
     $                            LDC )
                     CALL DLACPY( 'Full', P, NR, C, LDC, DWORK( IC ),
     $                            P )
                  ELSE
                     CALL DLACPY( 'Full', N, M, B, LDB, DWORK( IB ), N )
                     ISPROP = AB13ID( JOBSYS, JOBEIG, NEQUIL, NCSING,
     $                                RESTOR, UPDATE, N, M, P, A, LDA,
     $                                E, LDE, DWORK( IB ), N, C, LDC,
     $                                NR, RNKE, TOLI, IWORK,
     $                                DWORK( IWRK ), LDWORK-IWRK+1,
     $                                IWARN, INFO )
                     CALL DLACPY( 'Full', NR, M, DWORK( IB ), N, B,
     $                            LDB )
                     CALL DLACPY( 'Full', P, NR, C, LDC, DWORK( IC ),
     $                            P )
                  END IF
                  CALL DLACPY( 'Full', RNKE, RNKE, E, LDE, DWORK( IE ),
     $                         N )
                  CALL DLASET( 'Full', NR-RNKE, RNKE, ZERO, ZERO,
     $                         DWORK( IE+NR ), N )
                  CALL DLASET( 'Full', RNKE, NR-RNKE, ZERO, ZERO,
     $                         DWORK( IE+N*RNKE+1 ), N )
                  NR2 = NR*NR
               END IF
C
               CALL DLACPY( 'Full', NR, NR, A, LDA, DWORK( IA ), N )
C
            ELSE
C
               CALL DLACPY( 'Full', N, N, A, LDA, DWORK( IA ), N )
               CALL DLACPY( 'Full', N, M, B, LDB, DWORK( IB ), N )
               CALL DLACPY( 'Full', P, N, C, LDC, DWORK( ICW ), MAXPM )
               ISPROP = AB13ID( JOBSYS, JOBEIG, NEQUIL, NCSING, RESTOR,
     $                          UPDATE, N, M, P, DWORK( IA ), N,
     $                          DWORK( IE ), N, DWORK( IB ), N,
     $                          DWORK( ICW ), MAXPM, NR, RNKE, TOLI,
     $                          IWORK, DWORK( IWRK ), LDWORK-IWRK+1,
     $                          IWARN, INFO )
               CALL DLACPY( 'Full',  N,  N, A, LDA, DWORK( IA ), N )
               CALL DLACPY( 'Full', N1, N1, E, LDE, DWORK( IE ), N )
               CALL DLACPY( 'Full',  N,  M, B, LDB, DWORK( IB ), N )
               CALL DLACPY( 'Full',  P,  N, C, LDC, DWORK( IC ), P )
               NR = N
C
            END IF
C
            IF( .NOT.ISPROP ) THEN
               IWARN = 2
               GPEAK( 1 ) = ONE
               GPEAK( 2 ) = ZERO
               FPEAK( 1 ) = ONE
               FPEAK( 2 ) = ZERO
               GO TO 440
            ELSE IF( IWARN.EQ.1 ) THEN
               IWARN = 0
            END IF
            MAXWRK = MAX( MAXWRK, INT( DWORK( IWRK ) ) + IWRK - 1 )
C
         ELSE
            CALL DLACPY( 'Full', N, N, A, LDA, DWORK( IA ), N )
            CALL DLACPY( 'Full', N, M, B, LDB, DWORK( IB ), N )
            CALL DLACPY( 'Full', P, N, C, LDC, DWORK( IC ), P )
         END IF
C
         TEPS = TEN*EPS
         NINF = N - RANKE
         IF( CMPRE .AND. .NOT.DISCR ) THEN
C
C           Determine the largest singular value of G(infinity).
C           First, compute G(infinity) =
C              D-C(1:P,RANKE+1:N)*inv(A(RANKE+1:N,RANKE+1:N))*
C                B(RANKE+1:N,1:M).
C
C           Additional workspace:  NINF*(M + NINF + 4)  (from IR),
C                                  NINF = N-RANKE.
C           Integer    workspace:  NINF.
C
            IBS  = IR
            IAS  = IBS + NINF*M
            IWRK = IAS + NINF*NINF
            CALL DLACPY( 'Full', NINF, M, DWORK( IB+RANKE ), N,
     $                   DWORK( IBS ), NINF )
            CALL DLACPY( 'Full', NINF, NINF, DWORK( IA+RANKE*(N+1) ), N,
     $                   DWORK( IAS ), NINF )
            IF( ILASCL )
     $         CALL DLASCL( 'General', 0, 0, ANRMTO, ANRM, NINF, NINF,
     $                      DWORK( IAS ), NINF, IERR )
            TMP = DLANGE( '1-norm', NINF, NINF, DWORK( IAS ), NINF,
     $                    DWORK )
            CALL DGETRF( NINF, NINF, DWORK( IAS ), NINF, IWORK, IERR )
            IF( IERR.GT.0 ) THEN
C
C              The matrix A(RANKE+1:N,RANKE+1:N) is singular.
C              No safe computation of G(infinity) is possible.
C
               INFO = 1
               RETURN
            END IF
            CALL DGECON( '1-norm', NINF, DWORK( IAS ), NINF, TMP, RCOND,
     $                   DWORK( IWRK ), IWORK(NINF+1), IERR )
            IF( RCOND.LE.DBLE( NINF )*TEPS ) THEN
C
C              The matrix A(RANKE+1:N,RANKE+1:N) is numerically
C              singular, so the descriptor system is almost singular.
C              No safe computation of G(infinity) is possible.
C
               INFO = 1
               RETURN
            END IF
C
            CALL DGETRS( NTRAN, NINF, M, DWORK( IAS ), NINF, IWORK,
     $                   DWORK( IBS ), NINF, IERR )
C
C           Additional workspace:  NINF*M + P*M  (from IR).
C
            ID = IAS
            IS = ID + P*M
            IF( WITHD )
     $         CALL DLACPY( 'Full', P, M, D, LDD, DWORK( ID ), P )
            CALL DGEMM( NTRAN, NTRAN, P, M, NINF, -ONE,
     $                  DWORK( IC+RANKE*P ), P, DWORK( IBS ), NINF,
     $                  OZ, DWORK( ID ), P )
C
C           Compute the maximum singular value of G(infinity).
C
C           Additional workspace: need   MAX( 4*MIN(P,M) + MAX(P,M),
C           (from IS)                         6*MIN(P,M) );
C                                 prefer larger.
C
            IWRK = IS + MINPM
            CALL DGESVD( NOVECT, NOVECT, P, M, DWORK( ID ), P,
     $                   DWORK( IS ), DWORK, 1, DWORK, 1, DWORK( IWRK ),
     $                   LDWORK-IWRK+1, IERR )
            IF( IERR.GT.0 ) THEN
C
C              The SVD algorithm did not converge, no computation of the
C              largest singular value of G(infinity) is possible.
C
               INFO = 3
               RETURN
            END IF
            MAXWRK = MAX( INT( DWORK( IWRK ) ) + IWRK - 1, MAXWRK )
            GAMMAL = DWORK( IS )
         END IF
C
         IES = IBT + N
         IAS = IES + NR2
         IQ  = IES
         IZ  = IAS
C
C        For efficiency of later calculations, the system (A,E,B,C),
C        saved in workspace, is reduced to an equivalent one with the
C        state matrix A in Hessenberg form, and E upper triangular.
C        First, permute (A,E) to make it more nearly triangular.
C
C        Additional workspace: need   0,           if NSRT = .TRUE.;
C        (from IBT)                   2*N*N + 3*N, otherwise.
C        One additional location needed by DGGBAL is counted at TG01BD
C        call below.
C
         IF( NSRT ) THEN
            ILFT = IR
            IRHT = II
         ELSE
            ILFT = IZ   + NR2
            IRHT = ILFT + N
         END IF
         IWRK = IRHT + N
C
         CALL DGGBAL( 'Permute', NR, DWORK( IA ), N, DWORK( IE ), N,
     $                ILO, IHI, DWORK( ILFT ), DWORK( IRHT ),
     $                DWORK( IWRK ), IERR )
C
         IF( NSRT ) THEN
C
C           Apply the permutations to (the copies of) B and C.
C
            DO 10 I = NR - 1, IHI, -1
               K = DWORK( IR+I ) - 1
               IF( K.NE.I )
     $            CALL DSWAP( M, DWORK( IB+I ), N, DWORK( IB+K ), N )
               K = DWORK( II+I ) - 1
               IF( K.NE.I )
     $            CALL DSWAP( P, DWORK( IC+I*P ), 1, DWORK( IC+K*P ),
     $                        1 )
   10       CONTINUE
C
            DO 20 I = 0, ILO - 2
               K = DWORK( IR+I ) - 1
               IF( K.NE.I )
     $            CALL DSWAP( M, DWORK( IB+I ), N, DWORK( IB+K ), N )
               K = DWORK( II+I ) - 1
               IF( K.NE.I )
     $            CALL DSWAP( P, DWORK( IC+I*P ), 1, DWORK( IC+K*P ),
     $                        1 )
   20       CONTINUE
C
            VECT = 'N'
            M0   = M
            P0   = P
         ELSE
            VECT = 'I'
            M0   = 0
            P0   = 0
         END IF
C
C        Reduce (A,E) to generalized Hessenberg form. Apply the
C        transformations to B and C, if NSRT = .TRUE., i.e., (JOBE = 'C'
C        and RANKE < N) or DICO = 'D'.
C
C        Additional workspace: need   N + MAX(N,M0);
C                              prefer N + MAX(N,M0)*NB.
C
         CALL TG01BD( 'General', VECT, VECT, NR, M0, P0, ILO, IHI,
     $                DWORK( IA ), N, DWORK( IE ), N, DWORK( IB ), N,
     $                DWORK( IC ), P, DWORK( IQ ), NR, DWORK( IZ ), NR,
     $                DWORK( IWRK ), LDWORK-IWRK+1, IERR )
C
C        Perform QZ algorithm, computing eigenvalues.
C
C        Additional workspace: need   2*N*N + 2*N, if NSRT = .TRUE.;
C        (from IBT)                   2*N*N + 4*N, otherwise.
C                              prefer larger.
C
         IF( NSRT ) THEN
            IWRK = IAS + NR2
C
C           The generalized Hessenberg form will be used.
C
            CALL DLACPY( 'Full', NR, NR, DWORK( IA ), N, DWORK( IAS ),
     $                   NR )
            CALL DLACPY( 'Full', NR, NR, DWORK( IE ), N, DWORK( IES ),
     $                   NR )
            CALL DHGEQZ( EIGENV, VECT, VECT, NR, ILO, IHI, DWORK( IAS ),
     $                   NR, DWORK( IES ), NR, DWORK( IR ), DWORK( II ),
     $                   DWORK( IBT ), DWORK, NR, DWORK, NR,
     $                   DWORK( IWRK ), LDWORK-IWRK+1, IERR )
         ELSE
C
C           The generalized Schur form will be used.
C
            QZVECT = 'V'
            CALL DHGEQZ( 'Schur', QZVECT, QZVECT, NR, ILO, IHI,
     $                   DWORK( IA ), N, DWORK( IE ), N, DWORK( IR ),
     $                   DWORK( II ), DWORK( IBT ), DWORK( IQ ), NR,
     $                   DWORK( IZ ), NR, DWORK( IWRK ), LDWORK-IWRK+1,
     $                   IERR )
         END IF
C
         IF( IERR.GE.NR+1 ) THEN
            INFO = 5
            RETURN
         ELSE IF( IERR.NE.0 ) THEN
            INFO = 2
            RETURN
         END IF
         MAXWRK = MAX( INT( DWORK( IWRK ) ) + IWRK - 1, MAXWRK )
C
         IF( .NOT.NSRT ) THEN
C
C           Reorder finite eigenvalues to the top and infinite
C           eigenvalues to the bottom and update B and C.
C
C           Undo scaling on eigenvalues before selecting them.
C
            IF( ILASCL ) THEN
               CALL DLASCL( 'Hessenberg', 0, 0, ANRMTO, ANRM, N, N,
     $                      DWORK( IA ), N, IERR )
               CALL DLASCL( 'General', 0, 0, ANRMTO, ANRM, N, 1,
     $                      DWORK( IR ), N, IERR )
               CALL DLASCL( 'General', 0, 0, ANRMTO, ANRM, N, 1,
     $                      DWORK( II ), N, IERR )
            END IF
            IF( ILESCL ) THEN
               CALL DLASCL( 'Upper', 0, 0, ENRMTO, ENRM, N, N,
     $                      DWORK( IE ), N, IERR )
               CALL DLASCL( 'General', 0, 0, ENRMTO, ENRM, N, 1,
     $                      DWORK( IBT ), N, IERR )
            END IF
C
C           Select eigenvalues.
C
            SDIM1 = 0
            WMAX  = ZERO
            WRMIN = SAFMAX
C
            DO 30 I = 0, NR - 1
               IF( DWORK( II+I ).LT.ZERO ) THEN
                  SDIM1 = SDIM1 + 1
               ELSE
                  TM  = ABS( DWORK( IR+I ) )
                  TMP = ABS( DWORK( II+I ) )
                  BWORK( I+1 ) = DWORK( IBT+I ).NE.ZERO
                  IF( BWORK( I+1 ) ) THEN
                     IF( MIN( TM, TMP ).EQ.ZERO ) THEN
                        TMP = MAX( TM, TMP )
                     ELSE
                        TMP = DLAPY2( TM, TMP )
                     END IF
                     IF( DWORK( IBT+I ).GE.ONE .OR.
     $                 ( DWORK( IBT+I ).LT.ONE .AND.
     $                   TMP.LT.DWORK( IBT+I )*SAFMAX ) ) THEN
                        SDIM1 = SDIM1 + 1
                        TMP   = TMP /  DWORK( IBT+I )
                        WMAX  = MAX( WMAX,  TMP )
                        WRMIN = MIN( WRMIN, TMP )
                     ELSE
                        BWORK( I+1 ) = .FALSE.
                     END IF
                  ELSE
                     IF( MAX( TM, TMP ).EQ.ZERO )
     $                  GO TO 420
                  END IF
               END IF
   30       CONTINUE
C
            IF( WRMIN.GT.ONE ) THEN
               RAT = WMAX / WRMIN
            ELSE IF( WMAX.LT.WRMIN*SAFMAX ) THEN
               RAT = WMAX / WRMIN
            ELSE
               RAT = SAFMAX
            END IF
C
            IF( WREDUC .AND. ( DBLE( NR )*TEPS )*RAT.GT.ONE ) THEN
C
C              Set GPEAK to infinity, FPEAK = 0.
C
               GPEAK( 1 ) = ONE
               GPEAK( 2 ) = ZERO
               FPEAK( 1 ) = ZERO
               FPEAK( 2 ) = ONE
               GO TO 440
            END IF
C
            IF( SDIM1.LT.NR ) THEN
C
C              Reorder eigenvalues.
C
C              Additional workspace: need   4*N+16;
C                                    prefer larger.
C
               CALL DTGSEN( 0, .TRUE., .TRUE., BWORK, NR, DWORK( IA ),
     $                      N, DWORK( IE ), N, DWORK( IR ), DWORK( II ),
     $                      DWORK( IBT ), DWORK( IQ ), NR, DWORK( IZ ),
     $                      NR, SDIM, CND, CND, DUM, DWORK( IWRK ),
     $                      LDWORK-IWRK+1, IDUM, 1, IERR )
               IF( IERR.EQ.1 ) THEN
C
C                 The eigenvalue computation succeeded, but the reordering
C                 failed.
C
                  INFO = 5
                  RETURN
               END IF
C
               IF( SDIM.NE.SDIM1 ) THEN
                  INFO = 6
                  RETURN
               END IF
               MAXWRK = MAX( INT( DWORK( IWRK ) ) + IWRK - 1, MAXWRK )
C
C              Annihilate the last NR-SDIM elements of DWORK(IBT:IBT+NR-1).
C
               DUM( 1 ) = ZERO
               CALL DCOPY( NR-SDIM, DUM( 1 ), 0, DWORK( IBT+SDIM ), 1 )
            ELSE
               SDIM = NR
            END IF
C
C           Apply back-permutation to Q and Z.
C
            CALL DGGBAK( 'Permute', 'Left',  NR, ILO, IHI,
     $                   DWORK( ILFT ), DWORK( IRHT ), NR, DWORK( IQ ),
     $                   NR, IERR )
C
            CALL DGGBAK( 'Permute', 'Right', NR, ILO, IHI,
     $                   DWORK( ILFT ), DWORK( IRHT ), NR, DWORK( IZ ),
     $                   NR, IERR )
C
C           Update B and C.
C
C           Additional workspace: need   N*MAX(M,P) (from IWRK).
C
            CALL DGEMM( TRANS, NTRAN, NR, M, NR, ONE, DWORK( IQ ), NR,
     $                  DWORK( IB ), N, ZERO, DWORK( IWRK ), NR )
            CALL DLACPY( 'Full', NR, M, DWORK( IWRK ), NR, DWORK( IB ),
     $                   N )
C
            CALL DGEMM( NTRAN, NTRAN, P, NR, NR, ONE, DWORK( IC ), P,
     $                  DWORK( IZ ), NR, ZERO, DWORK( IWRK ), P )
            CALL DLACPY( 'Full', P, NR, DWORK( IWRK ), P, DWORK( IC ),
     $                   P )
C
C           Determine the largest singular value of G(infinity).
C
C           Additional workspace: need   P*M (from IBT+N).
C
            ID   = IES
            IS   = ID + P*M
            NINF = NR - SDIM
C
            IF( NINF.NE.0 ) THEN
               SDIM1 = MAX( 1, SDIM )
               IBS   = IS  + P*NINF
               IES   = IBS + M*NINF
C
C              Save E(1:SDIM,SDIM+1:NR) and use the 1-norms of
C              E(SDIM+1:NR,SDIM+1:NR) and A(SDIM+1:NR,SDIM+1:NR) to
C              decide if the algebraic index of the system is 1 or not.
C
C              Additional workspace: need   SDIM*NINF
C              (from IES = IBT + N + P*M + ( P + M )*NINF).
C
               CALL DLACPY( 'Full', SDIM, NINF, DWORK( IE+SDIM*N ), N,
     $                      DWORK( IES ), SDIM1 )
               TM   = DLANTR( '1-norm', 'Upper', 'NonUnit', NINF, NINF,
     $                        DWORK( IE+SDIM*( N+1 ) ), N, DWORK )
               TMP  = DLANHS( '1-norm', NINF, DWORK( IA+SDIM*( N+1 ) ),
     $                        N, DWORK )
               IND1 = TM.LT.DBLE( MAX( SDIM, NINF ) )*EPS*TMP
C
               IF( MAX( TM, TMP ).EQ.ZERO ) THEN
                   GO TO 420
C
               ELSE IF( IND1 ) THEN
C
C                 The system has algebraic index one.
C                 Solve NINF linear systems of equations
C                    E(1:SDIM,1:SDIM)*Y = -E(1:SDIM,SDIM+1:NR).
C                 Check first whether E(1:SDIM,1:SDIM) is nonsingular.
C
C                 Additional workspace: SDIM*NINF + 3*SDIM (from IES).
C                 Integer    workspace: SDIM.
C
                  IWRK = IES + SDIM*NINF
                  CALL DTRCON( '1-norm', 'Upper', 'NonUnit', SDIM,
     $                         DWORK( IE ), N, RCOND, DWORK( IWRK ),
     $                         IWORK, IERR )
                  IF( RCOND.LE.DBLE( SDIM )*TEPS ) THEN
C
C                    The matrix E(1:SDIM,1:SDIM) is numerically singular,
C                    no safe computation of G(infinity) is possible.
C                    This will not happen if the system is proper.
C
                     INFO = 1
                     RETURN
                  END IF
                  CALL DTRSM( 'Left', 'Upper', NTRAN, 'NonUnit', SDIM,
     $                        NINF, -ONE, DWORK( IE ), N, DWORK( IES ),
     $                        SDIM1 )
               ELSE
C
C                 The system has higher algebraic index.
C                 Solve the generalized Sylvester equation
C
C                    A(1:SDIM,1:SDIM)*Y + Z*A(SDIM+1:NR,SDIM+1:NR) +
C                       A(1:SDIM,SDIM+1:NR) = 0,
C                    E(1:SDIM,1:SDIM)*Y + Z*E(SDIM+1:NR,SDIM+1:NR) +
C                       E(1:SDIM,SDIM+1:NR) = 0,
C
C                 in order to decouple the system into its slow and fast
C                 parts.
C
C                 Additional workspace: need   4*SDIM*NINF (from IES);
C                                       prefer larger.
C                 Integer    workspace: need   NR+6.
C
                  IAS  = IES + SDIM*NINF
                  IWRK = IAS + SDIM*NINF
                  CALL DLACPY( 'Full', SDIM, NINF, DWORK( IA+SDIM*N ),
     $                         N, DWORK( IAS ), SDIM1 )
                  CALL DSCAL( 2*SDIM*NINF, -ONE, DWORK( IES ), 1 )
C
C                 Solve the generalized Sylvester equation.
C
                  CALL SB04OD( 'NoReduction', NTRAN, 'Fro', SDIM, NINF,
     $                         DWORK( IE ), N, DWORK( IE+SDIM*( N+1 ) ),
     $                         N, DWORK( IES ), SDIM1, DWORK( IA ), N,
     $                         DWORK( IA+SDIM*( N+1 ) ), N,
     $                         DWORK( IAS ), SDIM1, SCL, DIF, DWORK, 1,
     $                         DWORK, 1, DWORK, 1, DWORK, 1, IWORK,
     $                         DWORK( IWRK ), LDWORK-IWRK+1, IERR )
                  BOUND = EPS*THOUSD
                  IF( IERR.GT.0 .OR. DIF.GT.ONE / BOUND ) THEN
C
C                    The generalized Sylvester equation is very
C                    ill-conditioned, no safe computation of G(infinity)
C                    is possible.
C
                     INFO = 1
                     RETURN
                  END IF
                  MAXWRK = MAX( INT( DWORK( IWRK ) ) + IWRK - 1,
     $                          MAXWRK )
C
C                 Estimate the condition of the transformation matrix.
C
                  CND = DLANGE( '1-norm', SDIM, NINF, DWORK( IES ),
     $                          SDIM1, DWORK )
                  IF( CND.GT.ONE / TOLER ) THEN
C
C                    The (right) transformation matrix is very
C                    ill-conditioned, no safe computation of G(infinity)
C                    is possible.
C
                     INFO = 1
                     RETURN
                  END IF
               END IF
C
C              Update C in DWORK(IS).
C
               CALL DLACPY( 'Full', P, NINF, DWORK( IC+P*SDIM ), P,
     $                      DWORK( IS ), P )
               CALL DGEMM( NTRAN, NTRAN, P, NINF, SDIM, ONE,
     $                     DWORK( IC ), P, DWORK( IES ), SDIM1, ONE,
     $                     DWORK( IS ), P )
C
C              Compute G(infinity) =
C                 D-C(1:P,SDIM+1:NR)*inv(A(SDIM+1:NR,SDIM+1:NR))*
C                   B(SDIM+1:NR,1:M).
C
C              Additional workspace: need  NINF*( NINF+M+3 ) (from IES).
C              Integer    workspace: need  2*NINF.
C
               IAS  = IES + NINF*M
               IWRK = IAS + NINF*NINF
               CALL DLACPY( 'Full', NINF, M, DWORK( IB+SDIM ), N,
     $                      DWORK( IBS ), NINF )
               CALL DLACPY( 'Upper', NINF, NINF,
     $                      DWORK( IA+SDIM*( N+1 ) ), N, DWORK( IAS ),
     $                      NINF )
               CALL DCOPY(  NINF-1, DWORK( IA+SDIM*( N+1 )+1 ), N+1,
     $                      DWORK( IAS+1 ), NINF+1 )
               TMP = DLANHS( '1-norm', NINF, DWORK( IAS ), NINF, DWORK )
               CALL MB02SD( NINF, DWORK( IAS ), NINF, IWORK, INFO )
               CALL MB02TD( '1-norm', NINF, TMP, DWORK( IAS ), NINF,
     $                      IWORK, RCOND, IWORK(NINF+1), DWORK( IWRK ),
     $                      INFO )
               IF( RCOND.LE.DBLE( NINF )*TEPS ) THEN
C
C                 The matrix A(SDIM+1:NR,SDIM+1:NR) is numerically
C                 singular.
C
                  INFO = 1
                  RETURN
               END IF
               CALL MB02RD( NTRAN, NINF, M, DWORK( IAS ), NINF, IWORK,
     $                      DWORK( IBS ), NINF, IERR )
C
               IF( WITHD )
     $            CALL DLACPY( 'Full', P, M, D, LDD, DWORK( ID ), P )
               CALL DGEMM( NTRAN, NTRAN, P, M, NINF, -ONE, DWORK( IS ),
     $                     P, DWORK( IBS ), NINF, OZ, DWORK( ID ), P )
C
            ELSE IF( WITHD ) THEN
               CALL DLACPY( 'Full', P, M, D, LDD, DWORK( ID ), P )
            END IF
C
            IF( NINF.NE.0 .OR. WITHD ) THEN
C
C              Compute the maximum singular value of G(infinity).
C
C              Additional workspace: need   MAX( 3*MIN(P,M) + MAX(P,M),
C              (from IWRK)                       5*MIN(P,M) );
C                                    prefer larger.
C
               IWRK = IS + MINPM
               CALL DGESVD( NOVECT, NOVECT, P, M, DWORK( ID ), P,
     $                      DWORK( IS ), DWORK, 1, DWORK, 1,
     $                      DWORK( IWRK ), LDWORK-IWRK+1, IERR )
               IF( IERR.GT.0 ) THEN
C
C                 The SVD algorithm did not converge, computation of the
C                 largest singular value of G(infinity) is not possible.
C
                  INFO = 3
                  RETURN
               END IF
               MAXWRK = MAX( INT( DWORK( IWRK ) ) + IWRK - 1, MAXWRK )
               GAMMAL = DWORK( IS )
            END IF
C
         ELSE
            SDIM = NR
         END IF
C
C        Check if unscaling would cause over/underflow; if so, rescale
C        eigenvalues (DWORK( IR+I-1 ),DWORK( II+I-1 ),DWORK( IBT+I-1 ))
C        so that DWORK( IBT+I-1 ) is on the order of E(I,I) and
C        DWORK( IR+I-1 ) and DWORK( II+I-1 ) are on the order of A(I,I),
C        for I = 1, N.
C
         IF( ILASCL ) THEN
C
            DO 40 I = 0, NR - 1
               IF( DWORK( IR+I ).NE.ZERO ) THEN
                  IF( ( DWORK( IR+I ) / SAFMAX ).GT.( ANRMTO / ANRM )
     $                                                              .OR.
     $                ( SAFMIN / DWORK( IR+I ) ).GT.( ANRM / ANRMTO )
     $              ) THEN
                     TM = ABS( DWORK( IA+I*(N+1) ) / DWORK( IR+I ) )
                     DWORK( IBT+I ) = DWORK( IBT+I )*TM
                     DWORK(  IR+I ) = DWORK(  IR+I )*TM
                     DWORK(  II+I ) = DWORK(  II+I )*TM
                  ELSE IF( ( DWORK( II+I ) / SAFMAX ).GT.
     $                     ( ANRMTO / ANRM ) .OR. DWORK( II+I ).NE.ZERO
     $                                                             .AND.
     $                ( SAFMIN / DWORK( II+I ) ).GT.( ANRM / ANRMTO ) )
     $                     THEN
                     TM = ABS( DWORK( IA+I*(N+1)+N ) / DWORK( II+I ) )
                     DWORK( IBT+I ) = DWORK( IBT+I )*TM
                     DWORK(  IR+I ) = DWORK(  IR+I )*TM
                     DWORK(  II+I ) = DWORK(  II+I )*TM
                  END IF
               END IF
   40       CONTINUE
C
         END IF
C
         IF( ILESCL ) THEN
C
            DO 50 I = 0, NR - 1
               IF( DWORK( IBT+I ).NE.ZERO ) THEN
                  IF( ( DWORK( IBT+I ) / SAFMAX ).GT.( ENRMTO / ENRM )
     $                                                              .OR.
     $                ( SAFMIN / DWORK( IBT+I ) ).GT.( ENRM / ENRMTO )
     $              ) THEN
                     TM = ABS( DWORK( IE+I*(N+1) ) / DWORK( IBT+I ) )
                     DWORK( IBT+I ) = DWORK( IBT+I )*TM
                     DWORK(  IR+I ) = DWORK(  IR+I )*TM
                     DWORK(  II+I ) = DWORK(  II+I )*TM
                  END IF
               END IF
   50       CONTINUE
C
         END IF
C
C        Undo scaling.
C
         IF( NSRT ) THEN 
            IF( ILASCL ) THEN
               CALL DLASCL( 'Hessenberg', 0, 0, ANRMTO, ANRM, N, N,
     $                      DWORK( IA ), N, IERR )
               CALL DLASCL( 'General', 0, 0, ANRMTO, ANRM, N, 1,
     $                      DWORK( IR ), N, IERR )
               CALL DLASCL( 'General', 0, 0, ANRMTO, ANRM, N, 1,
     $                      DWORK( II ), N, IERR )
            END IF
C
            IF( ILESCL ) THEN
               CALL DLASCL( 'Upper', 0, 0, ENRMTO, ENRM, N, N,
     $                      DWORK( IE ), N, IERR )
               CALL DLASCL( 'General', 0, 0, ENRMTO, ENRM, N, 1,
     $                      DWORK( IBT ), N, IERR )
            END IF
         END IF
C
      ELSE
C
C        Standard state-space system.
C
         SDIM = N
         IF( LEQUIL ) THEN
C
C           Equilibrate the system.
C
            MAXRED = HUNDRD
            CALL TB01ID( 'All', N, M, P, MAXRED, A, LDA, B, LDB, C, LDC,
     $                   DWORK( II ), IERR )
            IF( WNRMD ) THEN
C
               DO 60 I = 0, N - 1
                  TMP = DWORK( II+I )
                  IF( TMP.NE.ONE ) THEN
                     CALL DSCAL( M, ONE / TMP, DWORK( IBV+I ), N )
                     CALL DSCAL( P, TMP, DWORK( ICU+I ), N )
                  END IF
   60          CONTINUE
C
            END IF
         END IF
C
C        For efficiency of later calculations, the system (A,B,C) is
C        reduced to a similar one with the state matrix in Hessenberg
C        form.
C
         CALL DLACPY( 'Full', N, N, A, LDA, DWORK( IA ), N )
         CALL DLACPY( 'Full', N, M, B, LDB, DWORK( IB ), N )
         CALL DLACPY( 'Full', P, N, C, LDC, DWORK( IC ), P )
C
C        First, permute the matrix A to make it more nearly triangular
C        and apply the permutations to B and C.
C
         CALL DGEBAL( 'Permute', N, DWORK( IA ), N, ILO, IHI,
     $                DWORK( IR ), IERR )
C
         DO 70 I = N - 1, IHI, -1
            K = DWORK( IR+I ) - 1
            IF( K.NE.I ) THEN
               CALL DSWAP( M, DWORK( IB+I ),   N, DWORK( IB+K ),   N )
               CALL DSWAP( P, DWORK( IC+I*P ), 1, DWORK( IC+K*P ), 1 )
            END IF
   70    CONTINUE
C
         DO 80 I = 0, ILO - 2
            K = DWORK( IR+I ) - 1
            IF( K.NE.I ) THEN
               CALL DSWAP( M, DWORK( IB+I ),   N, DWORK( IB+K ),   N )
               CALL DSWAP( P, DWORK( IC+I*P ), 1, DWORK( IC+K*P ), 1 )
            END IF
   80    CONTINUE
C
C        Reduce A to upper Hessenberg form and apply the transformations
C        to B and C.
C
C        Additional workspace: need   N;   (from II)
C                              prefer N*NB.
C
         ITAU = IR
         IWRK = II
         CALL DGEHRD( N, ILO, IHI, DWORK( IA ), N, DWORK( ITAU ),
     $                DWORK( IWRK ), LDWORK-IWRK+1, IERR )
         MAXWRK = MAX( INT( DWORK( IWRK ) ) + IWRK - 1, MAXWRK )
C
C        Additional workspace: need   M;
C                              prefer M*NB.
C
         CALL DORMHR( 'Left', TRANS, N, M, ILO, IHI, DWORK( IA ), N,
     $                DWORK( ITAU ), DWORK( IB ), N, DWORK( IWRK ),
     $                LDWORK-IWRK+1, IERR )
         MAXWRK = MAX( INT( DWORK( IWRK ) ) + IWRK - 1, MAXWRK )
C
C        Additional workspace: need   P;
C                              prefer P*NB.
C
         CALL DORMHR( 'Right', NTRAN, P, N, ILO, IHI, DWORK( IA ), N,
     $                DWORK( ITAU ), DWORK( IC ), P, DWORK( IWRK ),
     $                LDWORK-IWRK+1, IERR )
         MAXWRK = MAX( INT( DWORK( IWRK ) ) + IWRK - 1, MAXWRK )
C
C        Compute the eigenvalues. The Hessenberg form is preserved for
C        later use.
C
C        Additional workspace:  need   N*N + N;   (from IBT)
C                               prefer larger.
C
         IAS  = IBT
         IWRK = IAS + NN
         CALL DLACPY( 'Full', N, N, DWORK( IA ), N, DWORK( IAS ), N )
         CALL DHSEQR( EIGENV, NOVECT, N, ILO, IHI, DWORK( IAS ), N,
     $                DWORK( IR ), DWORK( II ), DWORK, 1, DWORK( IWRK ),
     $                LDWORK-IWRK+1, IERR )
         IF( IERR.GT.0 ) THEN
            INFO = 2
            RETURN
         END IF
         MAXWRK = MAX( INT( DWORK( IWRK ) ) + IWRK - 1, MAXWRK )
C
C        Annihilate the lower part of the Hessenberg matrix.
C
         IF( N.GT.2 )
     $      CALL DLASET( 'Lower', N-2, N-2, ZERO, ZERO, DWORK( IA+2 ),
     $                   N )
C
         IF( ILASCL ) THEN
C
C           Undo scaling for the Hessenberg form of A and eigenvalues.
C
            CALL DLASCL( 'Hessenberg', 0, 0, ANRMTO, ANRM, N, N,
     $                   DWORK( IA ), N, IERR )
            CALL DLASCL( 'General', 0, 0, ANRMTO, ANRM, N, 1,
     $                   DWORK( IR ), N, IERR )
            CALL DLASCL( 'General', 0, 0, ANRMTO, ANRM, N, 1,
     $                   DWORK( II ), N, IERR )
         END IF
C
      END IF
C
C     Look for (generalized) eigenvalues on the boundary of the
C     stability domain. (Their existence implies an infinite norm.)
C
C     Additional workspace:  need   N.   (from IM)
C
      IM = IBT
      IF( WITHE )
     $   IM = IM + N
      IAS   = IM + N
      IMIN  = II
      WRMIN = SAFMAX
C
C     NEI defines the number of finite eigenvalues (with moduli at most
C     pi, in the discrete-time case). The eigenvalues with negative
C     imaginary parts are not counted. BWORK(J) is set to .TRUE. if the
C     eigenvalue J is real. At the end of each of the four loops below,
C     DWORK(IM:IM+NEI-1) contains the additional test frequencies OMEGA.
C
      NEI  = 0
      NEIC = 0
      NEIR = 0
      LINF = .FALSE.
C
      IF( DISCR ) THEN
C
C        For discrete-time case, compute the logarithms of the non-zero
C        eigenvalues with magnitude at most pi, as well as their moduli
C        and real parts. This transformation maps the unit circle to the
C        imaginary axis of the complex plane. Also, find the minimum
C        distance of the original eigenvalues to the unit circle; a zero
C        value of this minimum implies an infinite L-infinity norm.
C
         PI = FOUR*ATAN( ONE )
C
         IF( WITHE ) THEN
C
            DO 90 I = 0, NR - 1
               TMR = DWORK( IR+I )
               TMP = DWORK( II+I )
               IF( TMP.GE.ZERO ) THEN
                  REALW = TMP.EQ.ZERO
                  IF( REALW ) THEN
                     TM = ABS( TMR )
                  ELSE
                     TM = DLAPY2( TMR, TMP )
                  END IF
                  IF( DWORK( IBT+I ).GE.ONE .OR.
     $                DWORK( IBT+I ).GE.TM / PI ) THEN
C
C                    Finite eigenvalues with moduli less than pi.
C
                     TM = TM / DWORK( IBT+I )
                     IF( TM.NE.ZERO .AND. TM.LT.PI ) THEN
                        IF( REALW ) THEN
                           IF( TMR.GT.ZERO) THEN
                              TMP = ZERO
                           ELSE
                              TMP = PI
                           END IF
                           NEIR = NEIR + 1
                        ELSE
                           TMP  = ATAN2( TMP, TMR )
                           NEIC = NEIC + 1
                        END IF
                        TMR = LOG( TM )
                        TD  = ABS( ONE - TM )
                        TM  = DLAPY2( TMR, TMP )
                        IF( TD.EQ.ZERO ) THEN
                           LINF = .TRUE.
                           IMIN = II + NEI
                           DWORK( IMIN ) = TMP
                           GO TO 130
                        END IF
                        RAT = ONE - TWO*( TMR / TM )**2
                        IF( RAT.LE.P25 ) THEN
                           DWORK( IM+NEI ) = TM / TWO
                        ELSE
                           DWORK( IM+NEI ) = TM*SQRT( MAX( P25, RAT ) )
                        END IF
                        BWORK( NEI+1 ) = REALW
                        NEI = NEI + 1
                     END IF
                  END IF
               END IF
   90       CONTINUE
C
         ELSE
C
            DO 100 I = 0, NR - 1
              TMR = DWORK( IR+I )
              TMP = DWORK( II+I )
              IF( TMP.GE.ZERO ) THEN
                  REALW = TMP.EQ.ZERO
                  IF( REALW ) THEN
                     TM = ABS( TMR )
                  ELSE
                     TM = DLAPY2( TMR, TMP )
                  END IF
                  IF( TM.NE.ZERO .AND. TM.LT.PI ) THEN
                     IF( REALW ) THEN
                        IF( TMR.GT.ZERO) THEN
                           TMP = ZERO
                        ELSE
                           TMP = PI
                        END IF
                        NEIR = NEIR + 1
                     ELSE
                        TMP  = ATAN2( TMP, TMR )
                        NEIC = NEIC + 1
                     END IF
                     TMR = LOG( TM )
                     TD  = ABS( ONE - TM )
                     TM  = DLAPY2( TMR, TMP )
                     IF( TD.EQ.ZERO ) THEN
                        LINF = .TRUE.
                        IMIN = II + NEI
                        DWORK( IMIN ) = TMP
                        GO TO 130
                     END IF
                     RAT = ONE - TWO*( TMR / TM )**2
                     IF( RAT.LE.P25 ) THEN
                        DWORK( IM+NEI ) = TM / TWO
                     ELSE
                        DWORK( IM+NEI ) = TM*SQRT( MAX( P25, RAT ) )
                     END IF
                     BWORK( NEI+1 ) = REALW
                     NEI = NEI + 1
                  END IF
               END IF
  100       CONTINUE
C
         END IF
C
      ELSE
C
C        For continuous-time case, compute moduli and absolute real
C        parts of finite eigenvalues and find the minimum absolute real
C        part; a zero value of this minimum implies an infinite
C        L-infinity norm.
C
         IF( WITHE ) THEN
C
            DO 110 I = 0, NR - 1
               TMR = ABS( DWORK( IR+I ) )
               TMP =      DWORK( II+I )
               IF( TMP.GE.ZERO ) THEN
                  REALW = TMP.EQ.ZERO
                  IF( REALW ) THEN
                     IF( TMR.EQ.ZERO ) THEN
                        IF( DWORK( IBT+I ).EQ.ZERO )
     $                     GO TO 420
                     END IF
                     TM = TMR
                  ELSE
                     TM = DLAPY2( TMR, TMP )
                  END IF
                  IF( TMR.EQ.ZERO ) THEN
                     LINF = .TRUE.
                     IMIN = II + I
                     GO TO 130
                  ELSE IF( DWORK( IBT+I ).GE.ONE .OR.
     $                   ( DWORK( IBT+I ).LT.ONE .AND.
     $                     TM.LT.DWORK( IBT+I )*SAFMAX ) ) THEN
                     TMR = TMR / DWORK( IBT+I )
                     TM  = TM / DWORK( IBT+I )
                     IF( REALW ) THEN
                        DWORK( IM+NEI ) = TM / TWO
                        NEIR = NEIR + 1
                     ELSE
                        RAT = ONE - TWO*( TMR / TM )**2
                        DWORK( IM+NEI ) = TM*SQRT( MAX( P25, RAT ) )
                        NEIC = NEIC + 1
                     END IF
                     BWORK( NEI+1 ) = REALW
                     NEI = NEI + 1
                  END IF
               END IF
  110       CONTINUE
C
         ELSE
C
            DO 120 I = 0, NR - 1
               TMR = ABS( DWORK( IR+I ) )
               TMP =      DWORK( II+I )
               IF( TMP.GE.ZERO ) THEN
                  IF( TMR.EQ.ZERO ) THEN
                     LINF = .TRUE.
                     IMIN = II + I
                     GO TO 130
                  END IF
                  REALW = TMP.EQ.ZERO
                  IF( REALW ) THEN
                     DWORK( IM+NEI ) = TMR / TWO
                     NEIR = NEIR + 1
                  ELSE
                     TM  = DLAPY2( TMR, TMP )
                     RAT = ONE - TWO*( TMR / TM )**2
                     DWORK( IM+NEI ) = TM*SQRT( MAX( P25, RAT ) )
                     NEIC = NEIC + 1
                  END IF
                  BWORK( NEI+1 ) = REALW
                  NEI = NEI + 1
               END IF
  120       CONTINUE
C
         END IF
C
      END IF
C
  130 CONTINUE
C
      IF( LINF ) THEN
C
C        The L-infinity norm was found as infinite.
C
         GPEAK( 1 ) = ONE
         GPEAK( 2 ) = ZERO
         TM = DWORK( IMIN )
         IF( WITHE .AND. .NOT.DISCR )
     $      TM = TM / DWORK( IBT+IMIN-II )
         FPEAK( 1 ) = TM
         FPEAK( 2 ) = ONE
C
         GO TO 440
      END IF
C
C     Determine the maximum singular value of
C        G(lambda) = C*inv(lambda*E - A)*B + D,
C     over a selected set of frequencies. Besides the frequencies w = 0,
C     w = pi (if DICO = 'D'), and the given value FPEAK, this test set
C     contains the peak frequency for each mode (or an approximation
C     of it for non-resonant modes). The (generalized) Hessenberg form
C     of the system is used. If POLES = 'P', only part of the modes
C     are used.
C
C     First, determine the maximum singular value of G(0) and set FPEAK
C     accordingly.
C
C     Additional workspace:
C           complex: need   1, if DICO = 'C' and OMEGA = 0;
C                           (N+M)*(N+P)+2*MIN(P,M)+MAX(P,M), otherwise;
C                    prefer larger;
C           real:    need   LDW1+LDW2+N (from IAS), where
C                           LDW1 = N*N+N*M+P*M, if DICO = 'C';
C                           LDW1 = 0,           if DICO = 'D';
C                           LDW2 = MAX(4*MIN(P,M)+MAX(P,M), 6*MIN(P,M)),
C                                               if DICO = 'C';
C                           LDW2 = 6*MIN(P,M),  if DICO = 'D'.
C                    prefer larger.
C     Integer    workspace: need   N.
C
      IF( WITHE ) THEN
         JOB = 'G'
      ELSE
         JOB = 'I'
      END IF
      OMEGA = ZERO
C
      IF( DISCR ) THEN
         JBDX = JOBD
         IWRK = IAS
         GAMMA = AB13DX( DICO, JOB, JBDX, NR, M, P, OMEGA, DWORK( IA ),
     $                   N, DWORK( IE ), N, DWORK( IB ), N, DWORK( IC ),
     $                   P, D, LDD, IWORK, DWORK( IWRK ), LDWORK-IWRK+1,
     $                   ZWORK, LZWORK, IERR )
         MAXCWK = MAX( INT( ZWORK( 1 ) ), MAXCWK )
      ELSE
         IBS = IAS + NR*NR
         ID  = IBS + NR*M
         CALL DLACPY( 'Upper', NR, NR, DWORK( IA ), N, DWORK( IAS ),
     $                NR )
         CALL DCOPY(  NR-1, DWORK( IA+1 ), N+1, DWORK( IAS+1 ), NR+1 )
         CALL DLACPY( 'Full', NR, M, DWORK( IB ), N, DWORK( IBS ), NR )
         IF( WITHD ) THEN
            CALL DLACPY( 'Full', P, M, D, LDD, DWORK( ID ), P )
            JBDX = 'D'
            IWRK = ID + P*M
         ELSE
            JBDX = 'Z'
            IWRK = ID
         END IF
C
         GAMMA = AB13DX( DICO, JOB, JBDX, NR, M, P, OMEGA, DWORK( IAS ),
     $                   NR, DWORK( IE ), N, DWORK( IBS ), NR,
     $                   DWORK( IC ), P, DWORK( ID ), P, IWORK,
     $                   DWORK( IWRK ), LDWORK-IWRK+1, ZWORK, LZWORK,
     $                   IERR )
      END IF
C
      IF( IERR.GT.0 )
     $   GO TO 430
      MAXWRK = MAX( INT( DWORK( IWRK ) ) + IWRK - 1, MAXWRK )
C
      FPEAKI = FPEAK( 2 )
      IF( FPEAKI.EQ.ZERO ) THEN
         FPEAKS = SAFMAX
      ELSE
         FPEAKS = FPEAK( 1 ) / FPEAKI
      END IF
      IF( GAMMAL.GT.GAMMA ) THEN
         IF( ABS( ONE - GAMMA / GAMMAL ).LE.EPS ) THEN
            FPEAK( 1 ) = ZERO
            FPEAK( 2 ) = ONE
         ELSE IF( .NOT.DISCR ) THEN
            FPEAK( 1 ) = ONE
            FPEAK( 2 ) = ZERO
         END IF
      ELSE
         GAMMAL     = GAMMA
         FPEAK( 1 ) = ZERO
         FPEAK( 2 ) = ONE
      END IF
C
      IF( DISCR ) THEN
         OMEGA = PI
C
C        Try the frequency w = pi.
C
         GAMMA = AB13DX( DICO, JOB, JBDX, NR, M, P, OMEGA, DWORK( IA ),
     $                   N, DWORK( IE ), N, DWORK( IB ), N, DWORK( IC ),
     $                   P, D, LDD, IWORK, DWORK( IWRK ), LDWORK-IWRK+1,
     $                   ZWORK, LZWORK, IERR )
         IF( IERR.GT.0 )
     $      GO TO 430
C
         IF( GAMMAL.LT.GAMMA ) THEN
            GAMMAL     = GAMMA
            FPEAK( 1 ) = OMEGA
            FPEAK( 2 ) = ONE
         END IF
C
      ELSE
         IWRK = IAS
      END IF
C
C     Build the remaining set of frequencies.
C     Complex workspace:  need   (N+M)*(N+P)+2*MIN(P,M)+MAX(P,M);
C                         prefer larger.
C     Real workspace:     need   LDW2 = 6*MIN(P,M) (from IWRK);
C                         prefer larger.
C
      IF( FPEAKS.NE.ZERO .OR. ( DISCR .AND. FPEAKS.NE.PI ) ) THEN
C
C        Compute also the norm at the given (finite) frequency.
C
         OMEGA = FPEAKS
         GAMMA = AB13DX( DICO, JOB, JBDX, NR, M, P, OMEGA, DWORK( IA ),
     $                   N, DWORK( IE ), N, DWORK( IB ), N, DWORK( IC ),
     $                   P, D, LDD, IWORK, DWORK( IWRK ), LDWORK-IWRK+1,
     $                   ZWORK, LZWORK, IERR )
         IF( DISCR ) THEN
            OMEGA = ABS( ATAN2( SIN( OMEGA ), COS( OMEGA ) ) )
         ELSE
            MAXWRK = MAX( INT( DWORK( IWRK ) ) + IWRK - 1, MAXWRK )
            MAXCWK = MAX( INT( ZWORK( 1 ) ), MAXCWK )
         END IF
C
         IF( IERR.GT.0 )
     $      GO TO 430
C
         IF( GAMMAL.LT.GAMMA ) THEN
            GAMMAL     = GAMMA
            FPEAK( 1 ) = OMEGA
            FPEAK( 2 ) = ONE
         END IF
C
      END IF
C
      IF( ALLPOL .OR. NEIR.EQ.NEI .OR. NEIC.EQ.NEI ) THEN
         CALL DLASRT( 'Increase', NEI, DWORK( IM ), IERR )
C
         IF( .NOT.ALLPOL ) THEN
            IF( NEIR.EQ.NEI ) THEN
               NEI = MIN( NEI, BNEIR )
            ELSE
               IF( DISCR ) THEN
                  NEI = MIN( NEI, BNEICD )
               ELSE
                  IF( NEI.GE.SWNEIC ) THEN
                     NEI = BNEICX
                  ELSE
                     NEI = MIN( NEI, BNEICM )
                  END IF
               END IF
            END IF
         END IF
C
         DO 140 I = 0, NEI - 1
            OMEGA = DWORK( IM+I )
C
            GAMMA = AB13DX( DICO, JOB, JBDX, NR, M, P, OMEGA,
     $                      DWORK( IA ), N, DWORK( IE ), N, DWORK( IB ),
     $                      N, DWORK( IC ), P, D, LDD, IWORK,
     $                      DWORK( IWRK ), LDWORK-IWRK+1, ZWORK, LZWORK,
     $                      IERR )
            IF( IERR.GT.0 )
     $         GO TO 430
C
            IF( GAMMAL.LT.GAMMA ) THEN
               IF( ABS( ONE - GAMMA / GAMMAL ).GT.EPS ) THEN
                  FPEAK( 1 ) = OMEGA
                  FPEAK( 2 ) = ONE
               END IF
               GAMMAL = GAMMA
            END IF
  140    CONTINUE
C
      ELSE
C
C        Prepare for using separately part of real and complex poles.
C
         NEIC = 0
         NEIR = 0
C   
         DO 150 I = 0, NEI - 1
            IF( BWORK( I+1 ) ) THEN
               DWORK( IR+NEIR ) = DWORK( IM+I )
               NEIR = NEIR + 1
            ELSE
               DWORK( IM+NEIC ) = DWORK( IM+I )
               NEIC = NEIC + 1
            END IF
  150    CONTINUE
C
C        Look over real poles.
C
         CALL DLASRT( 'Increase', NEIR, DWORK( IR ), IERR )
C
         I    = 1
         NEIR = MIN( NEIR, BNEIR )
C
C        WHILE ( I <= NEIR ) DO
C
  160    CONTINUE
         IF( I.LE.NEIR ) THEN
            OMEGA = DWORK( IR+I-1 )
C
            GAMMA = AB13DX( DICO, JOB, JBDX, NR, M, P, OMEGA,
     $                      DWORK( IA ), N, DWORK( IE ), N, DWORK( IB ),
     $                      N, DWORK( IC ), P, D, LDD, IWORK,
     $                      DWORK( IWRK ), LDWORK-IWRK+1, ZWORK, LZWORK,
     $                      IERR )
            IF( IERR.GT.0 )
     $         GO TO 430
C
            I = I + 1
            TMP = ABS( ONE - GAMMA / GAMMAL )
            IF( GAMMAL.LT.GAMMA ) THEN
               GAMMAL = GAMMA
               IF( TMP.GT.EPS ) THEN
                  FPEAK( 1 ) = OMEGA
                  FPEAK( 2 ) = ONE
               END IF
            END IF
C
C           END WHILE
C
            GO TO 160
         END IF
C
C        Look over complex poles with positive imaginary parts.
C
         CALL DLASRT( 'Increase', NEIC, DWORK( IM ), IERR )
C
         I = 1
         IF( DISCR ) THEN
            NEIC = MIN( NEIC, BNEICD )
         ELSE
            IF( NEIC.GE.SWNEIC ) THEN
               NEIC = BNEICX
            ELSE
               NEIC = MIN( NEIC, BNEICM )
            END IF
         END IF
C
C        WHILE ( I <= NEIC ) DO
C
  170    CONTINUE
         IF( I.LE.NEIC ) THEN
            OMEGA = DWORK( IM+I-1 )
C
            GAMMA = AB13DX( DICO, JOB, JBDX, NR, M, P, OMEGA,
     $                      DWORK( IA ), N, DWORK( IE ), N, DWORK( IB ),
     $                      N, DWORK( IC ), P, D, LDD, IWORK,
     $                      DWORK( IWRK ), LDWORK-IWRK+1, ZWORK, LZWORK,
     $                      IERR )
C
            IF( IERR.GT.0 )
     $         GO TO 430
C
            I = I + 1
            TMP = ABS( ONE - GAMMA / GAMMAL )
            IF( GAMMAL.LT.GAMMA ) THEN
               GAMMAL = GAMMA
               IF( TMP.GT.EPS ) THEN
                  FPEAK( 1 ) = OMEGA
                  FPEAK( 2 ) = ONE
               END IF
            END IF
C
C           END WHILE
C
            GO TO 170
         END IF
C
      END IF
C
C     Return if the lower bound is zero.
C
      IF( GAMMAL.EQ.ZERO ) THEN
         GPEAK( 1 ) = ZERO
         GPEAK( 2 ) = ONE
         FPEAK( 1 ) = ZERO
         FPEAK( 2 ) = ONE
         GO TO 440
      END IF
C
C     Start the modified gamma iteration for the Bruinsma-Steinbuch
C     algorithm.
C
C     Use the structure-preserving method on a Hamiltonian matrix or a
C     skew-Hamiltonian/Hamiltonian pencil.
C
      TOL1  = HUNDRD*EPS
      TOL2  = TEN*TOLER
      TOLN  = TOL( 1 )
      TOLP  = ONE + P1*TOLN
      GAMMA = ( ONE + TOLN )*GAMMAL
C
      IF( .NOT.USEPEN .AND. WITHD .AND. .NOT.FULLRD ) THEN
C
C        Check whether one can use an explicit Hamiltonian matrix:
C        compute
C        min(rcond(GAMMA**2*Im - S'*S), rcond(GAMMA**2*Ip - S*S')).
C
         IF( MINPM.GT.1 ) THEN
            RCOND = ( ( GAMMA - SV1 ) / ( GAMMA - SVP ) )*
     $              ( ( GAMMA + SV1 ) / ( GAMMA + SVP ) )
         ELSE
            RCOND = ONE
         END IF
C
         USEPEN = RCOND.LT.HUNDRD*TOLER
      END IF
C
      IF( USEPEN ) THEN
C
C        Add at most one auxiliary variable.
C
         K     = ( PM + R ) / 2
         Q     = NR   - K
         NBLK  = NR   + K
         II    = IR   + NBLK
         IBT   = II   + NBLK
         IH    = IBT  + NBLK
         NBLK2 = NBLK * NBLK
         IH12  = IH   + NBLK2
         IJ    = IH12 + NBLK2 + NBLK
         IJ12  = IJ   + NBLK2
         IT    = IJ12 + NBLK2 + NBLK
         IT12  = IT   + NBLK2
         IH22  = IT12 + NBLK2
         IWRK  = IH22 + NBLK2
         LIW   = 2*NBLK + 12
         QP    = Q  + P
         TNR   = 2*NR
         N2    = MIN( NR, K )
         CASE0 = Q.GE.0
         CASE1 = Q.GT.0
         CASE2 = .NOT.CASE0 .AND. QP.GE.0
         CASE3 = QP.LT.0
         IF( CMPRE ) THEN
            NE = MIN( RANKE, K )
         ELSE
            NE = N2
         END IF
         IF( DISCR ) THEN
            IF( CASE0 ) THEN
               NK = K
            ELSE
               NK = N
               IF( CASE3 ) THEN
                  NC = P
               ELSE
                  NC = -Q
               END IF
            END IF
         ELSE
            IF( CASE0 ) THEN
               ICI = 1
               IHC = IH + Q*NBLK + NR
               NC  = P
               PMQ = PM
            ELSE IF( CASE2 ) THEN
               ICI = 1 - Q
               IHC = IH + NR
               NC  = QP
            END IF
         END IF
         IF( .NOT.CASE0 )
     $      PMQ = PM + Q
         ONES( 1 ) = ONE
      ELSE
         IH   = IBT
         IH12 = IH   + NN
         ISL  = IH12 + NN + N
         ISC  = ISL + MAX( M, P )
         ISB  = ISC + P*N
         IWRK = ISL + NN + N
      END IF
C
C     WHILE ( Iteration may continue ) DO
C
  180 CONTINUE
C
         ITER = ITER + 1
C
         IF( .NOT.USEPEN ) THEN
C
C           Primary additional workspace: need   2*N*N+N   (from IBT)
C           (for building the relevant part of the Hamiltonian matrix).
C
            IF( ZEROD ) THEN
C
C              Standard continuous-time case with D = 0.
C              Form the needed part of the Hamiltonian matrix explicitly
C                 H = H11 - H12*inv(H22)*H21/g.
C
               CALL DLACPY( 'Full', N, N, A, LDA, DWORK( IH ), N )
C
C              Compute triangles of -C'*C/GAMMA and B*B'/GAMMA.
C
               CALL DSYRK( 'Lower', TRANS, N, P, -ONE / GAMMA, C, LDC,
     $                     ZERO, DWORK( IH12 ), N )
               CALL DSYRK( 'Upper', NTRAN, N, M,  ONE / GAMMA, B, LDB,
     $                     ZERO, DWORK( IH12+N ), N )
C
            ELSE
C
C              Standard continuous-time case with D <> 0 and the SVD of
C              D can be used. Compute explicitly the needed part of the
C              Hamiltonian matrix:
C
C              H =
C               (A+B1*S'*inv(g^2*Ip-S*S')*C1' g*B1*inv(g^2*Im-S'*S)*B1')
C               (                                                      )
C               (  -g*C1*inv(g^2*Ip-S*S')*C1'            -H11'         )
C
C              where g = GAMMA, B1 = B*V, C1 = C'*U, and H11 is the first
C              block of H.
C
C              Compute C1*sqrt(inv(g^2*Ip-S*S')) .
C
C              Additional workspace: need   MAX(M,P)+N*P (from ISL).
C
               DO 190 I = 0, MINPM - 1
                  DWORK( ISL+I ) = ONE / SQRT( GAMMA - DWORK( IS+I ) )
     $                                 / SQRT( GAMMA + DWORK( IS+I ) )
  190          CONTINUE
C
               IF( M.LT.P ) THEN
                  DWORK( ISL+M ) = ONE / GAMMA
                  CALL DCOPY( P-M-1, DWORK( ISL+M ), 0,
     $                        DWORK( ISL+M+1 ), 1 )
               END IF
               CALL DLACPY( 'Full', N, P, DWORK( ICU ), N, DWORK( ISC ),
     $                      N )
               CALL MB01SD( 'Column', N, P, DWORK( ISC ), N, DWORK,
     $                      DWORK( ISL ) )
C
C              Compute B1*S' .
C
C              Additional workspace: need   N*M (from ISB).
C
               CALL DLACPY( 'Full', N, M, DWORK( IBV ), N, DWORK( ISB ),
     $                      N )
               CALL MB01SD( 'Column', N, MINPM, DWORK( ISB ), N, DWORK,
     $                      DWORK( IS ) )
C
C              Compute B1*S'*sqrt(inv(g^2*Ip-S*S')) .
C
               CALL MB01SD( 'Column', N, MINPM, DWORK( ISB ), N, DWORK,
     $                      DWORK( ISL ) )
C
C              Compute H11 .
C
               CALL DLACPY( 'Full', N, N, A, LDA, DWORK( IH ), N )
               CALL DGEMM( NTRAN, TRANS, N, N, MINPM, ONE, DWORK( ISB ),
     $                     N, DWORK( ISC ), N, ONE, DWORK( IH ), N )
C
C              Compute B1*sqrt(inv(g^2*Im-S'*S)) .
C
               IF( P.LT.M ) THEN
                  DWORK( ISL+P ) = ONE / GAMMA
                  CALL DCOPY( M-P-1, DWORK( ISL+P ), 0,
     $                        DWORK( ISL+P+1 ), 1 )
               END IF
               CALL DLACPY( 'Full', N, M, DWORK( IBV ), N, DWORK( ISB ),
     $                      N )
               CALL MB01SD( 'Column', N, M, DWORK( ISB ), N, DWORK,
     $                      DWORK( ISL ) )
C
C              Compute the lower triangle of H21 and the upper triangle
C              of H12.
C
               CALL DSYRK( 'Lower', NTRAN, N, P, -GAMMA, DWORK( ISC ),
     $                     N, ZERO, DWORK( IH12 ), N )
               CALL DSYRK( 'Upper', NTRAN, N, M, GAMMA, DWORK( ISB ),
     $                     N, ZERO, DWORK( IH12+N ), N )
            END IF
C
C           Compute the eigenvalues of the Hamiltonian matrix by the
C           symplectic URV and the periodic Schur decompositions.
C
C           Additional workspace: need   (N+7)*N   (from IWRK);
C                                 prefer larger.
C
            CALL MB03XD( 'Both', EIGENV, NOVECT, NOVECT, N, DWORK( IH ),
     $                   N, DWORK( IH12 ), N, DWORK( ISL ), N, DUM, 1,
     $                   DUM, 1, DUM, 1, DUM, 1, DWORK( IR ),
     $                   DWORK( II ), ILO, DWORK( IWRK-N ),
     $                   DWORK( IWRK ), LDWORK-IWRK+1, IERR )
            IF( IERR.GT.0 ) THEN
               INFO = 2
               RETURN
            END IF
C
         ELSE
C
C           Use a skew-Hamiltonian/Hamiltonian pencil.
C           Initialize the pencil by zero.
C
            CALL DLASET( 'Full', 4*NBLK2+2*NBLK, 1, ZERO, ZERO,
     $                   DWORK( IH ), 1 )
            GAM( 1 )  =  GAMMA
            MGAM( 1 ) = -GAMMA
C
            IF( DISCR ) THEN
C
C              Set up the needed parts of a skew-Hamiltonian/Hamiltonian
C              pencil, based on the pencil (J,H),
C
C                  ( H11  H12 )         ( J11  J12 )
C              H = (          ),    J = (          ),
C                  ( H21  H22 )         ( J21  J22 )
C
C              with
C
C                    ( -A+E  0  )          (  0   B  )
C              H11 = (          ),   H12 = (         ),
C                    (   0   D' )          ( B' -g*I )
C
C                    ( 0   C' )
C              H21 = (        ),     H22 = -H11',
C                    ( C  g*I )
C
C                    ( A+E  0 )
C              J11 = (        ),     J12 = 0,
C                    (  0   0 )
C
C                    (  0  C' )
C              J21 = (        ),     J22 = J11',
C                    ( -C  0  )
C
C              where B, C, and D are extended such that the number of
C              inputs and outputs are equal (= MAX(P,M)), and g = GAMMA.
C
C              Additional workspace: need  7*NBLK*NBLK+5*NBLK (from IR).
C
C              Construct H11 and J11.
C
               I1 = 0
C
               IF( GENE ) THEN
C
                  DO 200 J = K + 1, N
                     CALL DAXPY( N, -ONE, E( 1, J ), 1, DWORK( IH+I1 ),
     $                           1 )
                     CALL DCOPY( N,  DWORK( IH+I1 ), 1, DWORK( IJ+I1 ),
     $                           1 )
                     CALL DAXPY( N,  ONE, A( 1, J ), 1, DWORK( IH+I1 ),
     $                           1 )
                     CALL DAXPY( N, -ONE, A( 1, J ), 1, DWORK( IJ+I1 ),
     $                           1 )
                     I1 = I1 + NBLK
  200             CONTINUE
C
               ELSE
C
                  DO 210 J = K + 1, N
                     CALL DCOPY( N, A( 1, J ), 1, DWORK( IH+I1 ), 1 )
                     CALL DAXPY( N, -ONE, A( 1, J ), 1, DWORK( IJ+I1 ),
     $                           1 )
                     IF( UNITE ) THEN
                        DWORK( IH+I1+J-1 ) = DWORK( IH+I1+J-1 ) - ONE
                        DWORK( IJ+I1+J-1 ) = DWORK( IJ+I1+J-1 ) - ONE
                     ELSE IF( RANKE.GE.J ) THEN
                        CALL DAXPY( RANKE, -ONE, E( 1, J ), 1,
     $                              DWORK( IH+I1 ), 1 )
                        CALL DAXPY( RANKE, -ONE, E( 1, J ), 1,
     $                              DWORK( IJ+I1 ), 1 )
                     END IF
                     I1 = I1 + NBLK
  210             CONTINUE
C
               END IF
C
C              Construct the rest of H11 and J11.
C
               IF( CASE0 ) THEN
                  CALL MA02AD( 'Full', P, K, C, LDC, DWORK( IH+I1+N ),
     $                         NBLK )
                  CALL DLACPY( 'Full', K, P, DWORK( IH+I1+N ), NBLK,
     $                         DWORK( IJ+I1+N ), NBLK )
                  I1 = QP*NBLK
C
                  DO 220 J = 1, M
                     CALL DAXPY( N, -ONE, B( 1, J ), 1, DWORK( IH+I1 ),
     $                           1 )
                     I1 = I1 + NBLK
  220             CONTINUE
C
               ELSE IF( CASE2 ) THEN
                  IF( QP.GT.0 ) THEN
                     CALL MA02AD( 'Full', QP, N, C( 1-Q, 1 ), LDC,
     $                            DWORK( IH+N ), NBLK )
                     CALL DLACPY( 'Full', N, QP, DWORK( IH+N ), NBLK,
     $                            DWORK( IJ+N ), NBLK )
                     I1 = QP*NBLK
                  END IF
C
                  DO 230 J = 1, M
                     I2 = I1 + TNR
                     CALL DAXPY( N, -ONE, B( 1, J ), 1, DWORK( IH+I1 ),
     $                           1 )
                     IF( WITHD )
     $                  CALL DAXPY( -Q, -ONE, D( 1, J ), 1,
     $                              DWORK( IH+I2 ), 1 )
                     I1 = I1 + NBLK
  230             CONTINUE
C
               ELSE
C
                  DO 240 J = 1 - QP, M
                     I2 = I1 + TNR
                     CALL DAXPY( N, -ONE, B( 1, J ), 1, DWORK( IH+I1 ),
     $                           1 )
                     IF( WITHD )
     $                  CALL DAXPY( P, -ONE, D( 1, J ), 1,
     $                              DWORK( IH+I2 ), 1 )
                     I1 = I1 + NBLK
  240             CONTINUE
C
               END IF
C
C              Construct the lower triangular parts of H21 and J21 and
C              the upper triangular parts of H12 and J12.
C
               IF( .NOT.CASE0 )
     $            CALL DCOPY( PMQ, GAM, 0, DWORK( IH12 ), NBLK+1 )
C
               IF( CASE0 ) THEN
                  CALL DLACPY( 'Full', P, Q, C( 1, K+1 ), LDC,
     $                         DWORK( IH12+Q ), NBLK )
                  I1 = Q*NBLK + Q
                  CALL DCOPY( PM, GAM, 0, DWORK( IH12+I1 ), NBLK+1 )
                  IF( WITHD ) THEN
                     I1 = I1 + P
C
                     DO 250 I = 1, P
                        CALL DAXPY( M, -ONE, D( I, 1 ), LDD,
     $                              DWORK( IH12+I1 ), 1 )
                        I1 = I1 + NBLK
  250                CONTINUE
C
                  END IF
                  I1 = Q
C
                  DO 260 J = K + 1, N
                     CALL DAXPY( P, -ONE, C( 1, J ), 1,
     $                           DWORK( IJ12+I1 ), 1 )
                     I1 = I1 + NBLK
  260             CONTINUE
C
               ELSE IF( CASE2 .AND. WITHD ) THEN
                  I1 = QP
C
                  DO 270 I = 1 - Q, P
                     CALL DAXPY( M, -ONE, D( I, 1 ), LDD,
     $                           DWORK( IH12+I1 ), 1 )
                     I1 = I1 + NBLK
  270             CONTINUE
C
               END IF
C
               I1 = ( N + 1 )*NBLK
               I2 = ( N + P )*NBLK + I1
C
               IF( GENE ) THEN
C
                  DO 280 J = 1, NK
                     CALL DCOPY( N, E( 1, J ), 1, DWORK( IJ12+I1 ), 1 )
                     CALL DAXPY( N, ONE, A( 1, J ), 1,
     $                           DWORK( IJ12+I1 ), 1 )
                     CALL DCOPY( N, E( 1, J ), 1, DWORK( IH12+I1 ), 1 )
                     CALL DAXPY( N, -ONE, A( 1, J ), 1,
     $                           DWORK( IH12+I1 ), 1 )
                     I1 = I1 + NBLK
  280             CONTINUE
C
               ELSE
C
                  DO 290 J = 1, NK
                     CALL DCOPY( N, A( 1, J ), 1, DWORK( IJ12+I1 ), 1 )
                     CALL DAXPY( N, -ONE, A( 1, J ), 1,
     $                           DWORK( IH12+I1 ), 1 )
                     IF( UNITE ) THEN
                        DWORK( IJ12+I1+J-1 ) = DWORK( IJ12+I1+J-1 ) +
     $                                         ONE
                        DWORK( IH12+I1+J-1 ) = DWORK( IH12+I1+J-1 ) +
     $                                         ONE
                     ELSE IF( RANKE.GE.J ) THEN
                        CALL DAXPY( RANKE, ONE, E( 1, J ), 1,
     $                              DWORK( IJ12+I1 ), 1 )
                        CALL DAXPY( RANKE, ONE, E( 1, J ), 1,
     $                              DWORK( IH12+I1 ), 1 )
                     END IF
                     I1 = I1 + NBLK
  290             CONTINUE
C
               END IF
C
               IF( .NOT.CASE0 ) THEN
                  I1 = I1 + N
                  I0 = I1 + N
C
                  DO 300 I = 1, NC
                     CALL DAXPY( N, -ONE, C( I, 1 ), LDC,
     $                           DWORK( IH12+I1 ), 1 )
                     CALL DCOPY( N, DWORK( IH12+I1 ), 1,
     $                           DWORK( IJ12+I1 ), 1 )
                     I1 = I1 + NBLK
  300             CONTINUE
C
                  CALL DCOPY( -Q, MGAM, 0, DWORK( IH12+I0 ), NBLK+1 )
                  IF( CASE3 ) THEN
                     CALL DLACPY( 'Full', N, -QP, B, LDB,
     $                            DWORK( IH12+I2 ), NBLK )
                     IF( WITHD )
     $                  CALL DLACPY( 'Full', P, -QP, D, LDD,
     $                            DWORK( IH12+I2+TN ), NBLK )
                  END IF
C
               END IF
C
               IF( R.GT.0 )
     $            DWORK( IH12+NBLK2-1 ) = ONE
C
            ELSE
C
C              Set up the needed parts of a skew-Hamiltonian/Hamiltonian
C              pencil, based on the pencil (H,J),
C
C                  ( H11  H12 )        ( S11   0  )        (  0   I )
C              H = (          ) ,  S = (          ) ,  J = (        ) ,
C                  ( H21  H22 )        (  0   S22 )        ( -I   0 )
C
C              with
C
C                    ( A  B )            ( 0   0  )            ( E  0 )
C              H11 = (      ),     H12 = (        ),     S11 = (      ),
C                    ( C  D )            ( 0 -g*I )            ( 0  0 )
C
C              H21 = -H12,         H22 = -H11',          S22 = S11',
C
C              where B and D are extended with one zero column if M+P is
C              odd, and g = GAMMA.
C
C              Additional workspace: need  7*NBLK*NBLK+5*NBLK (from IR).
C
C              Construct H11.
C
               IF( CASE1 )
     $            CALL DLACPY( 'Full', NR, Q, A( 1, K+1 ), LDA,
     $                         DWORK( IH ), NBLK )
               IF( QP.GE.0 ) THEN
                  CALL MA02AD( 'Full', NC, N2, C( ICI, 1 ), LDC,
     $                         DWORK( IHC ), NBLK )
C
                  I1 = QP*NBLK
                  I2 = I1 + TNR
C
                  DO 310 I0 = 1, M
                     CALL DAXPY( NR, -ONE, B( 1, I0 ), 1,
     $                           DWORK( IH+I1 ), 1 )
                     I1 = I1 + NBLK
  310             CONTINUE
C
                  IF( .NOT.CASE0 .AND. WITHD )
     $               CALL DLACPY( 'Full', -Q, M, D, LDD, DWORK( IH+I2 ),
     $                            NBLK )
               ELSE
C
                  CALL DLACPY( 'Full', NR, PMQ, B( 1, 1-QP ), LDB,
     $                         DWORK( IH ), NBLK )
C
                  IF( WITHD )
     $               CALL DLACPY( 'Full', P, PMQ, D( 1, 1-QP ), LDD,
     $                            DWORK( IH+TNR ), NBLK )
               END IF
C
C              Construct J11.
C
               IF( CASE1 ) THEN
C
                  IF( GENE ) THEN
                     CALL DLACPY( 'Full', N1, Q, E( 1, K+1 ), LDE,
     $                            DWORK( IJ ), NBLK )
C
                  ELSE IF( CMPRE ) THEN
                     IF( RANKE.GT.K )
     $                  CALL DLACPY( 'Full', N1, RANKE-K, E( 1, K+1 ),
     $                               LDE, DWORK( IJ ), NBLK )
C
                  ELSE
                     CALL DCOPY( Q, ONES, 0, DWORK( IJ ), NBLK+1 )
                  END IF
C
               END IF
C
C              Construct the lower triangular part of H21.
C
               IF( CASE1 ) THEN
                  I1 = Q
C
                  DO 320 I = K + 1, NR
                     CALL DAXPY( P, -ONE, C( 1, I ), 1,
     $                           DWORK( IH12+I1 ), 1 )
                     I1 = I1 + NBLK
  320             CONTINUE
C
               ELSE
                  I1 = 0
               END IF
C
               CALL DCOPY( PMQ, GAM, 0, DWORK( IH12+I1 ), NBLK+1 )
C
               IF( WITHD ) THEN
                  IF( CASE0 ) THEN
                     CALL MA02AD( 'Full', P, M, D, LDD,
     $                            DWORK( IH12+I1+P ), NBLK )
                  ELSE IF( CASE2 ) THEN
                     CALL MA02AD( 'Full', QP, M, D( 1-Q, 1 ), LDD,
     $                            DWORK( IH12+QP ), NBLK )
                  END IF
               END IF
C
               IF( R.EQ.1 )
     $            DWORK( IH12+NBLK2-1 ) = ONE
C
C              Construct the upper triangular parts of H12 and J12.
C
               I1 = ( NR + 1 )*NBLK
               I0 = I1
               CALL DLACPY( 'Full', NR, N2, A, LDA, DWORK( IH12+I1 ),
     $                      NBLK )
C
               I2 = I1 + NR*NBLK
               I1 = I2 + NR
C
               IF( .NOT.CASE0 ) THEN
                  IF( CASE2 ) THEN
C
                     DO 330 I = 1, -Q
                        CALL DAXPY( NR, -ONE, C( I, 1 ), LDC,
     $                              DWORK( IH12+I1 ), 1 )
                        I1 = I1 + NBLK
  330                CONTINUE
C
                  ELSE
                     CALL MA02AD( 'Full', P, NR, C, LDC,
     $                            DWORK( IH12+I1 ), NBLK )
                  END IF
                  CALL DCOPY( -Q, MGAM, 0, DWORK( IH12+I2+TNR ),
     $                         NBLK+1 )
               END IF
C
               IF( CASE3 ) THEN
                  I2 = I2 + P*NBLK
                  CALL DLACPY( 'Full', NR, -QP, B, LDB,
     $                         DWORK( IH12+I2 ), NBLK )
                  IF( WITHD )
     $               CALL DLACPY( 'Full', P, -QP, D, LDD,
     $                            DWORK( IH12+I2+TNR ), NBLK )
               END IF
C
               IF( UNITE ) THEN
                  CALL DCOPY( N2, ONES, 0, DWORK( IJ12+I0 ), NBLK+1 )
               ELSE
                  CALL DLACPY( 'Full', N1, NE, E, LDE, DWORK( IJ12+I0 ),
     $                         NBLK )
               END IF
C
            END IF
C
C           Compute the generalized eigenvalues using the structure-
C           preserving method for skew-Hamiltonian/Hamiltonian pencils.
C
C           Additional workspace: need    4*NBLK*NBLK + MAX(L,36), where
C           (from IWRK)             L = 8*NBLK + 4, if NBLK is even, and
C                                   L = 8*NBLK,     if NBLK is odd;
C                                 prefer larger.
C           Integer    workspace: need   2*NBLK + 12.
C
            IERR = -1
            CALL MB04BP( EIGENV, NOVECT, NOVECT, 2*NBLK, DWORK( IJ ),
     $                   NBLK, DWORK( IJ12 ), NBLK, DWORK( IH ), NBLK,
     $                   DWORK( IH12 ), NBLK, DWORK, 1, DWORK, 1,
     $                   DWORK( IT ), NBLK, DWORK( IT12 ), NBLK,
     $                   DWORK( IH22 ), NBLK, DWORK( IR ), DWORK( II ),
     $                   DWORK( IBT ), IWORK, LIW, DWORK( IWRK ),
     $                   LDWORK-IWRK+1, IERR )
C
            IF( IERR.EQ.1 .OR. IERR.EQ.2 ) THEN
               INFO = 2
               RETURN
            END IF
         END IF
C
         MAXWRK = MAX( INT( DWORK( IWRK ) ) + IWRK - 1, MAXWRK )
C
C        Detect finite eigenvalues on the boundary of the stability.
C        domain. The test is based on a round-off level of eps*rho(H)
C        (after balancing) resulting in worst-case perturbations of
C        order sqrt(eps*rho(H)), on the real part of poles of
C        multiplicity two (typical as GAMMA approaches the infinity
C        norm). Above, rho(H) is the maximum modulus of eigenvalues.
C        This test is valid also in the discrete-time case, since the
C        unit circle has been mapped on the imaginary axis.
C        NEI is the number of detected eigenvalues on the boundary.
C
C        Compute maximum eigenvalue modulus and check the absolute real
C        parts (if DICO = 'C'), or moduli (if DICO = 'D').
C
         WMAX = ZERO
C
         IF( USEPEN ) THEN
            IM = IBT + NBLK
C
            DO 340 I = 0, NBLK - 1
               TM = DWORK( II+I )
               IF( TM.GE.ZERO ) THEN
                  TM = DLAPY2( DWORK( IR+I ), TM )
                  IF( ( DWORK( IBT+I ).GE.ONE ) .OR.
     $                ( DWORK( IBT+I ).LT.ONE  .AND.
     $                  TM.LT.DWORK( IBT+I )*SAFMAX ) ) THEN
                     TM = TM / DWORK( IBT+I )
                     IF( TOL1*TM.LT.STOL ) THEN
                        WMAX = MAX( WMAX, TM )
                        DWORK( IM+I ) = TM
                     ELSE
                        DWORK( IM+I ) = -ONE
                     END IF
                  ELSE
                     DWORK( IM+I ) = -ONE
                  END IF
               ELSE
                  DWORK( IM+I ) = -ONE
               END IF
  340       CONTINUE
C
         ELSE
C
            DO 350 I = 0, NR - 1
               TM   = DLAPY2( DWORK( IR+I ), DWORK( II+I ) )
               WMAX = MAX( WMAX, TM )
               DWORK( IM+I ) = TM
  350       CONTINUE
C
         END IF
C
         NEI = 0
C
         IF( USEPEN ) THEN
C
            DO 360 I = 0, NBLK - 1
               TM = DWORK( IM+I )
               IF( TM.GE.ZERO ) THEN
                  TMR = ABS( DWORK( IR+I ) ) / DWORK( IBT+I )
                  IF( TMR.LT.TOL2*( ONE + TM ) + TOL1*WMAX ) THEN
                     DWORK( II+NEI ) = DWORK( II+I ) / DWORK( IBT+I )
                     NEI = NEI + 1
                  END IF
               END IF
  360       CONTINUE
C
         ELSE
C
            DO 370 I = 0, NR - 1
               TM  = DWORK( IM+I )
               TMR = ABS( DWORK( IR+I ) )
               IF( TMR.LT.TOL2*( ONE + TM ) + TOL1*WMAX ) THEN
                  DWORK( II+NEI ) = DWORK( II+I )
                  NEI = NEI + 1
               END IF
  370       CONTINUE
C
         END IF
C
         IF( NEI.EQ.0 ) THEN
C
C           There is no eigenvalue on the boundary of the stability
C           domain for G = ( 1 + TOLN )*GAMMAL. The norm was found.
C
            GPEAK( 1 ) = GAMMAL
            GPEAK( 2 ) = ONE
            GO TO 440
         END IF
C
C        Compute the NWS frequencies where the gain G is attained and
C        generate new test frequencies.
C
         NWS = 0
         J   = 0
C
         IF( DISCR ) THEN
C
            DO 380 I = 0, NEI - 1
C
C              Back transformation of eigenvalues.
C
               CALL DLADIV( ONE + DWORK( II+I ), ZERO, ONE,
     $                      DWORK( II+I ), TMR, TMP )
               CALL DLADIV( ONE - DWORK( II+I ), ZERO, ONE,
     $                      -DWORK( II+I ), TM, TD )
               DWORK( IR+I ) = TMR*TM - TMP*TD
               CALL DLADIV( TWO*DWORK( II+I ), ZERO, ONE, DWORK( II+I ),
     $                      TMR, TMP )
               CALL DLADIV( ONE, ZERO, ONE, -DWORK( II+I ), TM, TD )
               DWORK( II+I ) = TMR*TM - TMP*TD
C
               TM = ABS( ATAN2( DWORK( II+I ), DWORK( IR+I ) ) )
               IF( TM.LT.PI ) THEN
                  IF( TM.GT.EPS ) THEN
                     DWORK( IR+NWS ) = TM
                     NWS = NWS + 1
                  ELSE IF( TM.EQ.EPS ) THEN
                     IF( J.EQ.0 ) THEN
                        DWORK( IR+NWS ) = EPS
                        NWS = NWS + 1
                     END IF
                     J = J + 1
                  END IF
               END IF
  380       CONTINUE
C
         ELSE
C
            DO 390 I = 0, NEI - 1
               TM = DWORK( II+I )
               IF( TM.GT.EPS ) THEN
                  DWORK( IR+NWS ) = TM
                  NWS = NWS + 1
               ELSE IF( TM.EQ.EPS ) THEN
                  IF( J.EQ.0 ) THEN
                     DWORK( IR+NWS ) = EPS
                     NWS = NWS + 1
                  END IF
                  J = J + 1
               END IF
  390       CONTINUE
C
         END IF
C
         IF( NWS.EQ.0 ) THEN
C
C           There is no eigenvalue on the boundary of the stability
C           domain for G = ( 1 + TOLN )*GAMMAL. The norm was found.
C
            GPEAK( 1 ) = GAMMAL
            GPEAK( 2 ) = ONE
            GO TO 440
         END IF
C
         CALL DLASRT( 'Increase', NWS, DWORK( IR ), IERR )
         LW = 1
C
         DO 400 I = 0, NWS - 1
            IF( DWORK( IR+LW-1 ).NE.DWORK( IR+I ) ) THEN
               DWORK( IR+LW ) = DWORK( IR+I )
               LW = LW + 1
            END IF
  400    CONTINUE
C
         IF( LW.EQ.1 ) THEN
C
C           Duplicate the frequency trying to force iteration.
C
            DWORK( IR+1 ) = DWORK( IR )
            LW = LW + 1
         END IF
C
C        Form the vector of mid-points and compute the gain at new test
C        frequencies. Save the current lower bound.
C
         IF( .NOT.ALLPOL )
     $      LW = MIN( LW, BM )
C
         IRLW   = IR + LW
         GAMMAS = GAMMAL
C
         DO 410 I = 0, LW - 2
            IF( DISCR ) THEN
               OMEGA = ( DWORK( IR+I ) + DWORK( IR+I+1 ) ) / TWO
            ELSE
               OMEGA = SQRT( DWORK( IR+I )*DWORK( IR+I+1 ) )
            END IF
C
C           Additional workspace:  need   LDW2, see above (from IRLW);
C                                  prefer larger.
C
            GAMMA = AB13DX( DICO, JOB, JBDX, NR, M, P, OMEGA,
     $                      DWORK( IA ), N, DWORK( IE ), N, DWORK( IB ),
     $                      N, DWORK( IC ), P, D, LDD, IWORK,
     $                      DWORK( IRLW ), LDWORK-IRLW+1, ZWORK, LZWORK,
     $                      IERR )
            IF( DISCR )
     $         OMEGA = ABS( ATAN2( SIN( OMEGA ), COS( OMEGA ) ) )
            IF( IERR.GT.0 )
     $         GO TO 430
C
            IF( GAMMAL.LT.GAMMA ) THEN
               GAMMAL     = GAMMA
               FPEAK( 1 ) = OMEGA
               FPEAK( 2 ) = ONE
            END IF
  410    CONTINUE
C
C        If the lower bound has not been improved, return. (This is a
C        safeguard against undetected modes of Hamiltonian matrix or
C        skew-Hamiltonian/Hamiltonian matrix pencil on the boundary of
C        the stability domain.)
C
         IF( LW.LE.1 .OR. GAMMAL.LT.GAMMAS*TOLP ) THEN
            GPEAK( 1 ) = GAMMAL
            GPEAK( 2 ) = ONE
            GO TO 440
         END IF
C
C     END WHILE
C
      IF( ITER.LE.MAXIT ) THEN
         GAMMA = ( ONE + TOLN )*GAMMAL
         GO TO 180
      ELSE
         INFO = 4
         GPEAK( 1 ) = GAMMAL
         GPEAK( 2 ) = ONE
         GO TO 440
      END IF
C
  420 CONTINUE
C
C     Singular descriptor system. Set GPEAK = NAN, FPEAK = 0.
C
      IWARN = 1
      GPEAK( 1 ) = ZERO
      GPEAK( 2 ) = ZERO
      FPEAK( 1 ) = ZERO
      FPEAK( 2 ) = ONE
      GO TO 440
C
  430 CONTINUE
      IF( IERR.EQ.NR+1 ) THEN
         INFO = 3
         RETURN
      ELSE IF( IERR.GT.0 ) THEN
         GPEAK( 1 ) = ONE
         GPEAK( 2 ) = ZERO
         FPEAK( 1 ) = OMEGA
         FPEAK( 2 ) = ONE
      END IF
C
  440 CONTINUE
C
      IWORK( 1 ) = ITER
      DWORK( 1 ) = MAXWRK
      ZWORK( 1 ) = MAXCWK
      RETURN
C *** Last line of AB13HD ***
      END
