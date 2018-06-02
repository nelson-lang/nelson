/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static doublereal c_b5 = 0.;
static doublereal c_b6 = 1.;
static doublereal c_b8 = -1.;
static integer c__5 = 5;
static integer c__1 = 1;

EXPORTSYMBOL /* Subroutine */ int bd02ad_(def, nr, dpar, ipar, vec, n, m, p, e, lde, a, lda, b, ldb,
    c__, ldc, d__, ldd, note, dwork, ldwork, info, def_len, note_len) char* def;
integer* nr;
doublereal* dpar;
integer* ipar;
logical* vec;
integer *n, *m, *p;
doublereal* e;
integer* lde;
doublereal* a;
integer* lda;
doublereal* b;
integer* ldb;
doublereal* c__;
integer* ldc;
doublereal* d__;
integer* ldd;
char* note;
doublereal* dwork;
integer *ldwork, *info;
ftnlen def_len;
ftnlen note_len;
{
    /* Initialized data */
    static logical vecdef[8] = { TRUE_, TRUE_, TRUE_, FALSE_, TRUE_, TRUE_, TRUE_, FALSE_ };
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, d_dim1, d_offset, e_dim1,
        e_offset, i__1, i__2;
    olist o__1;
    cllist cl__1;
    /* Builtin functions */
    /* Subroutine */ int s_copy();
    integer f_open(), s_rsle(), do_lio(), e_rsle(), f_clos(), s_wsfi(), do_fio(), e_wsfi();
    /* Local variables */
    static doublereal temp;
    static integer i__, j;
    static char dataf[12];
    extern logical lsame_();
    extern /* Subroutine */ int dlaset_();
    static integer status;
    /* Fortran I/O blocks */
    static cilist io___4 = { 1, 1, 1, 0, 0 };
    static icilist io___7 = { 0, dataf, 0, "(A,I2.2,A)", 11, 1 };
    static cilist io___8 = { 1, 1, 1, 0, 0 };
    static cilist io___9 = { 1, 1, 1, 0, 0 };
    /*     SLICOT RELEASE 5.0. */
    /*     Copyright (c) 2002-2010 NICONET e.V. */
    /*     This program is free software: you can redistribute it and/or */
    /*     modify it under the terms of the GNU General Public License as */
    /*     published by the Free Software Foundation, either version 2 of */
    /*     the License, or (at your option) any later version. */
    /*     This program is distributed in the hope that it will be useful, */
    /*     but WITHOUT ANY WARRANTY; without even the implied warranty of */
    /*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the */
    /*     GNU General Public License for more details. */
    /*     You should have received a copy of the GNU General Public License */
    /*     along with this program.  If not, see */
    /*     <http://www.gnu.org/licenses/>. */
    /*     PURPOSE */
    /*     To generate benchmark examples for time-invariant, */
    /*     discrete-time dynamical systems */
    /*       E x_k+1 = A x_k + B u_k */
    /*           y_k = C x_k + D u_k */
    /*     E, A are real N-by-N matrices, B is N-by-M, C is P-by-N, and */
    /*     D is P-by-M. In many examples, E is the identity matrix and D is */
    /*     the zero matrix. */
    /*     This routine is an implementation of the benchmark library */
    /*     DTDSX (Version 1.0) described in [1]. */
    /*     ARGUMENTS */
    /*     Mode Parameters */
    /*     DEF     CHARACTER*1 */
    /*             Specifies the kind of values used as parameters when */
    /*             generating parameter-dependent and scalable examples */
    /*             (i.e., examples with NR(1) = 2, 3, or 4): */
    /*             = 'D':  Default values defined in [1] are used; */
    /*             = 'N':  Values set in DPAR and IPAR are used. */
    /*             This parameter is not referenced if NR(1) = 1. */
    /*             Note that the scaling parameter of examples with */
    /*             NR(1) = 3 or 4 is considered as a regular parameter in */
    /*             this context. */
    /*     Input/Output Parameters */
    /*     NR      (input) INTEGER array, dimension (2) */
    /*             Specifies the index of the desired example according */
    /*             to [1]. */
    /*             NR(1) defines the group: */
    /*                   1 : parameter-free problems of fixed size */
    /*                   2 : parameter-dependent problems of fixed size */
    /*                   3 : parameter-free problems of scalable size */
    /*                   4 : parameter-dependent problems of scalable size */
    /*             NR(2) defines the number of the benchmark example */
    /*             within a certain group according to [1]. */
    /*     DPAR    (input/output) DOUBLE PRECISION array, dimension (7) */
    /*             On entry, if DEF = 'N' and the desired example depends on */
    /*             real parameters, then the array DPAR must contain the */
    /*             values for these parameters. */
    /*             For an explanation of the parameters see [1]. */
    /*             For Example 2.1, DPAR(1), ..., DPAR(3) define the */
    /*             parameters 'tau', 'delta', 'K', respectively. */
    /*             On exit, if DEF = 'D' and the desired example depends on */
    /*             real parameters, then the array DPAR is overwritten by the */
    /*             default values given in [1]. */
    /*     IPAR    (input/output) INTEGER array, dimension (1) */
    /*             On entry, if DEF = 'N' and the desired example depends on */
    /*             integer parameters, then the array IPAR must contain the */
    /*             values for these parameters. */
    /*             For an explanation of the parameters see [1]. */
    /*             For Example 3.1, IPAR(1) defines the parameter 'n'. */
    /*             On exit, if DEF = 'D' and the desired example depends on */
    /*             integer parameters, then the array IPAR is overwritten by */
    /*             the default values given in [1]. */
    /*     VEC     (output) LOGICAL array, dimension (8) */
    /*             Flag vector which displays the availabilty of the output */
    /*             data: */
    /*             VEC(1), ..., VEC(3) refer to N, M, and P, respectively, */
    /*             and are always .TRUE.. */
    /*             VEC(4) is .TRUE. iff E is NOT the identity matrix. */
    /*             VEC(5), ..., VEC(7) refer to A, B, and C, respectively, */
    /*             and are always .TRUE.. */
    /*             VEC(8) is .TRUE. iff D is NOT the zero matrix. */
    /*     N       (output) INTEGER */
    /*             The actual state dimension, i.e., the order of the */
    /*             matrices E and A. */
    /*     M       (output) INTEGER */
    /*             The number of columns in the matrices B and D. */
    /*     P       (output) INTEGER */
    /*             The number of rows in the matrices C and D. */
    /*     E       (output) DOUBLE PRECISION array, dimension (LDE,N) */
    /*             The leading N-by-N part of this array contains the */
    /*             matrix E. */
    /*             NOTE that this array is overwritten (by the identity */
    /*             matrix), if VEC(4) = .FALSE.. */
    /*     LDE     INTEGER */
    /*             The leading dimension of array E.  LDE >= N. */
    /*     A       (output) DOUBLE PRECISION array, dimension (LDA,N) */
    /*             The leading N-by-N part of this array contains the */
    /*             matrix A. */
    /*     LDA     INTEGER */
    /*             The leading dimension of array A.  LDA >= N. */
    /*     B       (output) DOUBLE PRECISION array, dimension (LDB,M) */
    /*             The leading N-by-M part of this array contains the */
    /*             matrix B. */
    /*     LDB     INTEGER */
    /*             The leading dimension of array B.  LDB >= N. */
    /*     C       (output) DOUBLE PRECISION array, dimension (LDC,N) */
    /*             The leading P-by-N part of this array contains the */
    /*             matrix C. */
    /*     LDC     INTEGER */
    /*             The leading dimension of array C.  LDC >= P. */
    /*     D       (output) DOUBLE PRECISION array, dimension (LDD,M) */
    /*             The leading P-by-M part of this array contains the */
    /*             matrix D. */
    /*             NOTE that this array is overwritten (by the zero */
    /*             matrix), if VEC(8) = .FALSE.. */
    /*     LDD     INTEGER */
    /*             The leading dimension of array D.  LDD >= P. */
    /*     NOTE    (output) CHARACTER*70 */
    /*             String containing short information about the chosen */
    /*             example. */
    /*     Workspace */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*             NOTE that DWORK is not used in the current version */
    /*             of BD02AD. */
    /*     LDWORK  INTEGER */
    /*             LDWORK >= 1. */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0:  successful exit; */
    /*             < 0:  if INFO = -i, the i-th argument had an illegal */
    /*                   value; in particular, INFO = -3 or -4 indicates */
    /*                   that at least one of the parameters in DPAR or */
    /*                   IPAR, respectively, has an illegal value; */
    /*             = 1:  data file can not be opened or has wrong format. */
    /*     REFERENCES */
    /*     [1]  Kressner, D., Mehrmann, V. and Penzl, T. */
    /*          DTDSX - a Collection of Benchmark Examples for State-Space */
    /*          Realizations of Discrete-Time Dynamical Systems. */
    /*          SLICOT Working Note 1998-10. 1998. */
    /*     NUMERICAL ASPECTS */
    /*     None */
    /*     CONTRIBUTOR */
    /*     D. Kressner, V. Mehrmann, and T. Penzl (TU Chemnitz) */
    /*     For questions concerning the collection or for the submission of */
    /*     test examples, please contact Volker Mehrmann */
    /*     (Email: volker.mehrmann@mathematik.tu-chemnitz.de). */
    /*     REVISIONS */
    /*     June 1999, V. Sima. */
    /*     KEYWORDS */
    /*     discrete-time dynamical systems */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. Local Arrays .. */
    /*     .. External Functions .. */
    /*     . LAPACK . */
    /*     .. External Subroutines .. */
    /*     . LAPACK . */
    /*     .. Data Statements .. */
    /*     . default values for availabities . */
    /* Parameter adjustments */
    --nr;
    --dpar;
    --ipar;
    --vec;
    e_dim1 = *lde;
    e_offset = e_dim1 + 1;
    e -= e_offset;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    b_dim1 = *ldb;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    c_dim1 = *ldc;
    c_offset = c_dim1 + 1;
    c__ -= c_offset;
    d_dim1 = *ldd;
    d_offset = d_dim1 + 1;
    d__ -= d_offset;
    --dwork;
    /* Function Body */
    /*     .. Executable Statements .. */
    *info = 0;
    for (i__ = 1; i__ <= 8; ++i__) {
        vec[i__] = vecdef[i__ - 1];
        /* L10: */
    }
    if (nr[1] == 1) {
        if (nr[2] == 1) {
            s_copy(note, "Laub 1979, Ex. 2: uncontrollable-unobservable data", 70L, 50L);
            *n = 2;
            *m = 1;
            *p = 1;
            if (*lde < *n) {
                *info = -10;
            }
            if (*lda < *n) {
                *info = -12;
            }
            if (*ldb < *n) {
                *info = -14;
            }
            if (*ldc < *p) {
                *info = -16;
            }
            if (*ldd < *p) {
                *info = -18;
            }
            if (*info != 0) {
                return 0;
            }
            dlaset_("A", n, n, &c_b5, &c_b6, &e[e_offset], lde, 1L);
            a[a_dim1 + 1] = 4.;
            a[a_dim1 + 2] = -4.5;
            a[(a_dim1 << 1) + 1] = 3.;
            a[(a_dim1 << 1) + 2] = -3.5;
            dlaset_("A", n, m, &c_b8, &c_b6, &b[b_offset], ldb, 1L);
            c__[c_dim1 + 1] = 3.;
            c__[(c_dim1 << 1) + 1] = 2.;
            dlaset_("A", p, m, &c_b5, &c_b5, &d__[d_offset], ldd, 1L);
        } else if (nr[2] == 2) {
            s_copy(note, "Laub 1979, Ex. 3", 70L, 16L);
            *n = 2;
            *m = 2;
            *p = 2;
            if (*lde < *n) {
                *info = -10;
            }
            if (*lda < *n) {
                *info = -12;
            }
            if (*ldb < *n) {
                *info = -14;
            }
            if (*ldc < *p) {
                *info = -16;
            }
            if (*ldd < *p) {
                *info = -18;
            }
            if (*info != 0) {
                return 0;
            }
            dlaset_("A", n, n, &c_b5, &c_b6, &e[e_offset], lde, 1L);
            dlaset_("A", n, n, &c_b5, &c_b5, &a[a_offset], lda, 1L);
            a[a_dim1 + 1] = .9512;
            a[(a_dim1 << 1) + 2] = .9048;
            b[b_dim1 + 1] = 4.877;
            b[(b_dim1 << 1) + 1] = 4.877;
            b[b_dim1 + 2] = -1.1895;
            b[(b_dim1 << 1) + 2] = 3.569;
            dlaset_("A", p, n, &c_b5, &c_b6, &c__[c_offset], ldc, 1L);
            dlaset_("A", p, m, &c_b5, &c_b5, &d__[d_offset], ldd, 1L);
        } else if (nr[2] == 3) {
            s_copy(note, "Van Dooren 1981, Ex. II", 70L, 23L);
            *n = 2;
            *m = 1;
            *p = 1;
            if (*lde < *n) {
                *info = -10;
            }
            if (*lda < *n) {
                *info = -12;
            }
            if (*ldb < *n) {
                *info = -14;
            }
            if (*ldc < *p) {
                *info = -16;
            }
            if (*ldd < *p) {
                *info = -18;
            }
            if (*info != 0) {
                return 0;
            }
            dlaset_("A", n, n, &c_b5, &c_b6, &e[e_offset], lde, 1L);
            a[a_dim1 + 1] = 2.;
            a[a_dim1 + 2] = 1.;
            a[(a_dim1 << 1) + 1] = -1.;
            a[(a_dim1 << 1) + 2] = 0.;
            dlaset_("A", n, m, &c_b5, &c_b6, &b[b_offset], ldb, 1L);
            dlaset_("A", p, n, &c_b6, &c_b5, &c__[c_offset], ldc, 1L);
            d__[d_dim1 + 1] = 0.;
        } else if (nr[2] == 4) {
            s_copy(note, "Ionescu/Weiss 1992", 70L, 18L);
            *n = 2;
            *m = 2;
            *p = 2;
            if (*lde < *n) {
                *info = -10;
            }
            if (*lda < *n) {
                *info = -12;
            }
            if (*ldb < *n) {
                *info = -14;
            }
            if (*ldc < *p) {
                *info = -16;
            }
            if (*ldd < *p) {
                *info = -18;
            }
            if (*info != 0) {
                return 0;
            }
            dlaset_("A", n, n, &c_b5, &c_b6, &e[e_offset], lde, 1L);
            dlaset_("A", n, n, &c_b5, &c_b5, &a[a_offset], lda, 1L);
            a[(a_dim1 << 1) + 1] = 1.;
            a[(a_dim1 << 1) + 2] = -1.;
            dlaset_("A", n, m, &c_b5, &c_b6, &b[b_offset], ldb, 1L);
            b[b_dim1 + 2] = 2.;
            dlaset_("A", p, n, &c_b5, &c_b6, &c__[c_offset], ldc, 1L);
            dlaset_("A", p, m, &c_b5, &c_b5, &d__[d_offset], ldd, 1L);
        } else if (nr[2] == 5) {
            s_copy(note, "Jonckheere 1981", 70L, 15L);
            *n = 2;
            *m = 1;
            *p = 2;
            if (*lde < *n) {
                *info = -10;
            }
            if (*lda < *n) {
                *info = -12;
            }
            if (*ldb < *n) {
                *info = -14;
            }
            if (*ldc < *p) {
                *info = -16;
            }
            if (*ldd < *p) {
                *info = -18;
            }
            if (*info != 0) {
                return 0;
            }
            dlaset_("A", n, n, &c_b5, &c_b6, &e[e_offset], lde, 1L);
            dlaset_("A", n, n, &c_b5, &c_b5, &a[a_offset], lda, 1L);
            a[(a_dim1 << 1) + 1] = 1.;
            dlaset_("A", n, m, &c_b6, &c_b5, &b[b_offset], ldb, 1L);
            dlaset_("A", p, n, &c_b5, &c_b6, &c__[c_offset], ldc, 1L);
            dlaset_("A", p, m, &c_b5, &c_b5, &d__[d_offset], ldd, 1L);
        } else if (nr[2] == 6) {
            s_copy(note, "Ackerson/Fu 1970: satellite control problem", 70L, 43L);
            *n = 4;
            *m = 2;
            *p = 4;
            if (*lde < *n) {
                *info = -10;
            }
            if (*lda < *n) {
                *info = -12;
            }
            if (*ldb < *n) {
                *info = -14;
            }
            if (*ldc < *p) {
                *info = -16;
            }
            if (*ldd < *p) {
                *info = -18;
            }
            if (*info != 0) {
                return 0;
            }
            dlaset_("A", n, n, &c_b5, &c_b6, &e[e_offset], lde, 1L);
            dlaset_("A", p, n, &c_b5, &c_b6, &c__[c_offset], ldc, 1L);
            dlaset_("A", p, m, &c_b5, &c_b5, &d__[d_offset], ldd, 1L);
        } else if (nr[2] == 7) {
            s_copy(note, "Litkouhi 1983: system with slow and fast modes", 70L, 46L);
            *n = 4;
            *m = 2;
            *p = 4;
            if (*lde < *n) {
                *info = -10;
            }
            if (*lda < *n) {
                *info = -12;
            }
            if (*ldb < *n) {
                *info = -14;
            }
            if (*ldc < *p) {
                *info = -16;
            }
            if (*ldd < *p) {
                *info = -18;
            }
            if (*info != 0) {
                return 0;
            }
            dlaset_("A", n, n, &c_b5, &c_b6, &e[e_offset], lde, 1L);
            dlaset_("A", p, n, &c_b5, &c_b6, &c__[c_offset], ldc, 1L);
            dlaset_("A", p, m, &c_b5, &c_b5, &d__[d_offset], ldd, 1L);
        } else if (nr[2] == 8) {
            s_copy(note, "Lu/Lin 1993, Ex. 4.3", 70L, 20L);
            *n = 4;
            *m = 4;
            *p = 4;
            if (*lde < *n) {
                *info = -10;
            }
            if (*lda < *n) {
                *info = -12;
            }
            if (*ldb < *n) {
                *info = -14;
            }
            if (*ldc < *p) {
                *info = -16;
            }
            if (*ldd < *p) {
                *info = -18;
            }
            if (*info != 0) {
                return 0;
            }
            dlaset_("A", n, n, &c_b5, &c_b6, &e[e_offset], lde, 1L);
            dlaset_("U", p, n, &c_b6, &c_b6, &c__[c_offset], ldc, 1L);
            c__[c_dim1 * 3 + 1] = 2.;
            c__[(c_dim1 << 2) + 1] = 4.;
            c__[(c_dim1 << 2) + 2] = 2.;
            dlaset_("A", p, m, &c_b5, &c_b5, &d__[d_offset], ldd, 1L);
        } else if (nr[2] == 9) {
            s_copy(note, "Gajic/Shen 1993, Section 2.7.4: chemical plant", 70L, 46L);
            *n = 5;
            *m = 2;
            *p = 5;
            if (*lde < *n) {
                *info = -10;
            }
            if (*lda < *n) {
                *info = -12;
            }
            if (*ldb < *n) {
                *info = -14;
            }
            if (*ldc < *p) {
                *info = -16;
            }
            if (*ldd < *p) {
                *info = -18;
            }
            if (*info != 0) {
                return 0;
            }
            dlaset_("A", n, n, &c_b5, &c_b6, &e[e_offset], lde, 1L);
            dlaset_("A", p, n, &c_b5, &c_b6, &c__[c_offset], ldc, 1L);
            dlaset_("A", p, m, &c_b5, &c_b5, &d__[d_offset], ldd, 1L);
        } else if (nr[2] == 10) {
            s_copy(note, "Davison/Wang 1974", 70L, 17L);
            *n = 6;
            *m = 2;
            *p = 2;
            if (*lde < *n) {
                *info = -10;
            }
            if (*lda < *n) {
                *info = -12;
            }
            if (*ldb < *n) {
                *info = -14;
            }
            if (*ldc < *p) {
                *info = -16;
            }
            if (*ldd < *p) {
                *info = -18;
            }
            if (*info != 0) {
                return 0;
            }
            vec[8] = TRUE_;
            dlaset_("A", n, n, &c_b5, &c_b6, &e[e_offset], lde, 1L);
            dlaset_("A", n, n, &c_b5, &c_b5, &a[a_offset], lda, 1L);
            a[(a_dim1 << 1) + 1] = 1.;
            a[a_dim1 * 3 + 2] = 1.;
            a[a_dim1 * 5 + 4] = 1.;
            a[a_dim1 * 6 + 5] = 1.;
            dlaset_("A", n, m, &c_b5, &c_b5, &b[b_offset], ldb, 1L);
            b[b_dim1 + 3] = 1.;
            b[(b_dim1 << 1) + 6] = 1.;
            dlaset_("A", p, n, &c_b5, &c_b5, &c__[c_offset], ldc, 1L);
            c__[c_dim1 + 1] = 1.;
            c__[(c_dim1 << 1) + 1] = 1.;
            c__[(c_dim1 << 2) + 2] = 1.;
            c__[c_dim1 * 5 + 2] = -1.;
            dlaset_("A", p, m, &c_b5, &c_b5, &d__[d_offset], ldd, 1L);
            d__[d_dim1 + 1] = 1.;
            d__[d_dim1 + 2] = 1.;
        } else if (nr[2] == 11) {
            s_copy(note, "Patnaik et al. 1980: tubular ammonia reactor", 70L, 44L);
            *n = 9;
            *m = 3;
            *p = 2;
            if (*lde < *n) {
                *info = -10;
            }
            if (*lda < *n) {
                *info = -12;
            }
            if (*ldb < *n) {
                *info = -14;
            }
            if (*ldc < *p) {
                *info = -16;
            }
            if (*ldd < *p) {
                *info = -18;
            }
            if (*info != 0) {
                return 0;
            }
            dlaset_("A", n, n, &c_b5, &c_b6, &e[e_offset], lde, 1L);
            dlaset_("A", p, n, &c_b5, &c_b5, &c__[c_offset], ldc, 1L);
            c__[c_dim1 + 1] = 1.;
            c__[c_dim1 * 5 + 2] = 1.;
            dlaset_("A", p, m, &c_b5, &c_b5, &d__[d_offset], ldd, 1L);
        } else if (nr[2] == 12) {
            s_copy(note, "Smith 1969: two-stand cold rolling mill", 70L, 39L);
            *n = 10;
            *m = 3;
            *p = 5;
            if (*lde < *n) {
                *info = -10;
            }
            if (*lda < *n) {
                *info = -12;
            }
            if (*ldb < *n) {
                *info = -14;
            }
            if (*ldc < *p) {
                *info = -16;
            }
            if (*ldd < *p) {
                *info = -18;
            }
            if (*info != 0) {
                return 0;
            }
            vec[8] = TRUE_;
            dlaset_("A", n, n, &c_b5, &c_b6, &e[e_offset], lde, 1L);
            dlaset_("A", n, n, &c_b5, &c_b5, &a[a_offset], lda, 1L);
            dlaset_("A", n, n, &c_b5, &c_b6, &a[a_dim1 + 2], lda, 1L);
            a[a_dim1 * 10 + 1] = .112;
            dlaset_("A", n, m, &c_b5, &c_b5, &b[b_offset], ldb, 1L);
            b[b_dim1 + 1] = 2.76;
            b[(b_dim1 << 1) + 1] = -1.35;
            b[b_dim1 * 3 + 1] = -.46;
            dlaset_("A", p, n, &c_b5, &c_b5, &c__[c_offset], ldc, 1L);
            c__[c_dim1 + 1] = 1.;
            c__[c_dim1 * 10 + 2] = .894;
            c__[c_dim1 * 10 + 3] = -16.93;
            c__[c_dim1 * 10 + 4] = .07;
            c__[c_dim1 * 10 + 5] = .398;
            o__1.oerr = 1;
            o__1.ounit = 1;
            o__1.ofnmlen = 11;
            o__1.ofnm = "BD02112.dat";
            o__1.orl = 0;
            o__1.osta = "OLD";
            o__1.oacc = 0;
            o__1.ofm = 0;
            o__1.oblnk = 0;
            status = f_open(&o__1);
            if (status != 0) {
                *info = 1;
            } else {
                i__1 = *p;
                for (i__ = 1; i__ <= i__1; ++i__) {
                    status = s_rsle(&io___4);
                    if (status != 0) {
                        goto L100001;
                    }
                    i__2 = *m;
                    for (j = 1; j <= i__2; ++j) {
                        status = do_lio(&c__5, &c__1, (char*)&d__[i__ + j * d_dim1],
                            (ftnlen)sizeof(doublereal));
                        if (status != 0) {
                            goto L100001;
                        }
                    }
                    status = e_rsle();
                L100001:
                    if (status != 0) {
                        *info = 1;
                    }
                    /* L110: */
                }
            }
            cl__1.cerr = 0;
            cl__1.cunit = 1;
            cl__1.csta = 0;
            f_clos(&cl__1);
        } else {
            *info = -2;
        }
        if (nr[2] >= 6 && nr[2] <= 9 || nr[2] == 11) {
            /*         .. loading data files */
            s_wsfi(&io___7);
            do_fio(&c__1, "BD021", 5L);
            do_fio(&c__1, (char*)&nr[2], (ftnlen)sizeof(integer));
            do_fio(&c__1, ".dat", 4L);
            e_wsfi();
            o__1.oerr = 1;
            o__1.ounit = 1;
            o__1.ofnmlen = 11;
            o__1.ofnm = dataf;
            o__1.orl = 0;
            o__1.osta = "OLD";
            o__1.oacc = 0;
            o__1.ofm = 0;
            o__1.oblnk = 0;
            status = f_open(&o__1);
            if (status != 0) {
                *info = 1;
            } else {
                i__1 = *n;
                for (i__ = 1; i__ <= i__1; ++i__) {
                    status = s_rsle(&io___8);
                    if (status != 0) {
                        goto L100002;
                    }
                    i__2 = *n;
                    for (j = 1; j <= i__2; ++j) {
                        status = do_lio(
                            &c__5, &c__1, (char*)&a[i__ + j * a_dim1], (ftnlen)sizeof(doublereal));
                        if (status != 0) {
                            goto L100002;
                        }
                    }
                    status = e_rsle();
                L100002:
                    if (status != 0) {
                        *info = 1;
                    }
                    /* L120: */
                }
                i__1 = *n;
                for (i__ = 1; i__ <= i__1; ++i__) {
                    status = s_rsle(&io___9);
                    if (status != 0) {
                        goto L100003;
                    }
                    i__2 = *m;
                    for (j = 1; j <= i__2; ++j) {
                        status = do_lio(
                            &c__5, &c__1, (char*)&b[i__ + j * b_dim1], (ftnlen)sizeof(doublereal));
                        if (status != 0) {
                            goto L100003;
                        }
                    }
                    status = e_rsle();
                L100003:
                    if (status != 0) {
                        *info = 1;
                    }
                    /* L130: */
                }
            }
            cl__1.cerr = 0;
            cl__1.cunit = 1;
            cl__1.csta = 0;
            f_clos(&cl__1);
        }
    } else if (nr[1] == 2) {
        if (!(lsame_(def, "D", 1L, 1L) || lsame_(def, "N", 1L, 1L))) {
            *info = -1;
            return 0;
        }
        if (nr[2] == 1) {
            s_copy(note, "Pappas et al. 1980: process control of paper machine", 70L, 52L);
            if (lsame_(def, "D", 1L, 1L)) {
                dpar[1] = 1e8;
                dpar[2] = 1.;
                dpar[3] = 1.;
            }
            if (dpar[1] == 0.) {
                *info = -3;
            }
            *n = 4;
            *m = 1;
            *p = 1;
            if (*lde < *n) {
                *info = -10;
            }
            if (*lda < *n) {
                *info = -12;
            }
            if (*ldb < *n) {
                *info = -14;
            }
            if (*ldc < *p) {
                *info = -16;
            }
            if (*ldd < *p) {
                *info = -18;
            }
            if (*info != 0) {
                return 0;
            }
            temp = dpar[2] / dpar[1];
            dlaset_("A", n, n, &c_b5, &c_b6, &e[e_offset], lde, 1L);
            dlaset_("A", n, n, &c_b5, &c_b5, &a[a_offset], lda, 1L);
            i__1 = *n - 1;
            i__2 = *n - 1;
            dlaset_("A", &i__1, &i__2, &c_b5, &c_b6, &a[a_dim1 + 2], lda, 1L);
            a[a_dim1 + 1] = 1. - temp;
            dlaset_("A", n, m, &c_b5, &c_b5, &b[b_offset], ldb, 1L);
            b[b_dim1 + 1] = dpar[3] * temp;
            dlaset_("A", p, n, &c_b5, &c_b5, &c__[c_offset], ldc, 1L);
            c__[(c_dim1 << 2) + 1] = 1.;
            dlaset_("A", p, m, &c_b5, &c_b5, &d__[d_offset], ldd, 1L);
        } else {
            *info = -2;
        }
    } else if (nr[1] == 3) {
        if (!(lsame_(def, "D", 1L, 1L) || lsame_(def, "N", 1L, 1L))) {
            *info = -1;
            return 0;
        }
        if (nr[2] == 1) {
            s_copy(note, "Pappas et al. 1980, Ex. 3", 70L, 25L);
            if (lsame_(def, "D", 1L, 1L)) {
                ipar[1] = 100;
            }
            if (ipar[1] < 2) {
                *info = -4;
            }
            *n = ipar[1];
            *m = 1;
            *p = *n;
            if (*lde < *n) {
                *info = -10;
            }
            if (*lda < *n) {
                *info = -12;
            }
            if (*ldb < *n) {
                *info = -14;
            }
            if (*ldc < *p) {
                *info = -16;
            }
            if (*ldd < *p) {
                *info = -18;
            }
            if (*info != 0) {
                return 0;
            }
            dlaset_("A", n, n, &c_b5, &c_b6, &e[e_offset], lde, 1L);
            dlaset_("A", n, n, &c_b5, &c_b5, &a[a_offset], lda, 1L);
            i__1 = *n - 1;
            i__2 = *n - 1;
            dlaset_("A", &i__1, &i__2, &c_b5, &c_b6, &a[(a_dim1 << 1) + 1], lda, 1L);
            dlaset_("A", n, m, &c_b5, &c_b5, &b[b_offset], ldb, 1L);
            b[*n + b_dim1] = 1.;
            dlaset_("A", p, n, &c_b5, &c_b6, &c__[c_offset], ldc, 1L);
            dlaset_("A", p, m, &c_b5, &c_b5, &d__[d_offset], ldd, 1L);
        } else {
            *info = -2;
        }
    } else {
        *info = -2;
    }
    return 0;
    /* *** Last Line of BD02AD *** */
} /* bd02ad_ */
