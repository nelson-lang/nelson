/* Translated by Nelson f2c (version 20170901).
   You must link the resulting object file with the libraries:
    -lnlsf2c -lm   (in that order)
*/

#include "nelson_f2c.h"

/* Table of constant values */

static doublereal c_b9 = 0.;
static doublereal c_b10 = 1.;

EXPORTSYMBOL /* Subroutine */ int mb04wr_(job, trans, n, ilo, q1, ldq1, q2, ldq2, cs, tau, dwork,
    ldwork, info, job_len, trans_len) char *job,
    *trans;
integer *n, *ilo;
doublereal* q1;
integer* ldq1;
doublereal* q2;
integer* ldq2;
doublereal *cs, *tau, *dwork;
integer *ldwork, *info;
ftnlen job_len;
ftnlen trans_len;
{
    /* System generated locals */
    integer q1_dim1, q1_offset, q2_dim1, q2_offset, i__1, i__2;
    /* Local variables */
    static integer ierr, i__, j;
    extern /* Subroutine */ int mb04wd_();
    extern logical lsame_();
    static logical ltran, compu;
    static integer nh;
    extern /* Subroutine */ int dlaset_(), xerbla_();
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
    /*     To generate orthogonal symplectic matrices U or V, defined as */
    /*     products of symplectic reflectors and Givens rotators */
    /*     U = diag( HU(1),HU(1) )  GU(1)  diag( FU(1),FU(1) ) */
    /*         diag( HU(2),HU(2) )  GU(2)  diag( FU(2),FU(2) ) */
    /*                              .... */
    /*         diag( HU(n),HU(n) )  GU(n)  diag( FU(n),FU(n) ), */
    /*     V = diag( HV(1),HV(1) )       GV(1)   diag( FV(1),FV(1) ) */
    /*         diag( HV(2),HV(2) )       GV(2)   diag( FV(2),FV(2) ) */
    /*                                   .... */
    /*         diag( HV(n-1),HV(n-1) )  GV(n-1)  diag( FV(n-1),FV(n-1) ), */
    /*     as returned by the SLICOT Library routines MB04TS or MB04TB. The */
    /*     matrices U and V are returned in terms of their first N/2 rows: */
    /*                 [  U1   U2 ]           [  V1   V2 ] */
    /*             U = [          ],      V = [          ]. */
    /*                 [ -U2   U1 ]           [ -V2   V1 ] */
    /*     ARGUMENTS */
    /*     Input/Output Parameters */
    /*     JOB     CHARACTER*1 */
    /*             Specifies whether the matrix U or the matrix V is */
    /*             required: */
    /*             = 'U':  generate U; */
    /*             = 'V':  generate V. */
    /*     TRANS   CHARACTER*1 */
    /*             If  JOB = 'U'  then TRANS must have the same value as */
    /*             the argument TRANA in the previous call of MB04TS or */
    /*             MB04TB. */
    /*             If  JOB = 'V'  then TRANS must have the same value as */
    /*             the argument TRANB in the previous call of MB04TS or */
    /*             MB04TB. */
    /*     N       (input) INTEGER */
    /*             The order of the matrices Q1 and Q2. N >= 0. */
    /*     ILO     (input) INTEGER */
    /*             ILO must have the same value as in the previous call of */
    /*             MB04TS or MB04TB. U and V are equal to the unit matrix */
    /*             except in the submatrices */
    /*             U([ilo:n n+ilo:2*n], [ilo:n n+ilo:2*n]) and */
    /*             V([ilo+1:n n+ilo+1:2*n], [ilo+1:n n+ilo+1:2*n]), */
    /*             respectively. */
    /*             1 <= ILO <= N, if N > 0; ILO = 1, if N = 0. */
    /*     Q1      (input/output) DOUBLE PRECISION array, dimension (LDQ1,N) */
    /*             On entry, if  JOB = 'U'  and  TRANS = 'N'  then the */
    /*             leading N-by-N part of this array must contain in its i-th */
    /*             column the vector which defines the elementary reflector */
    /*             FU(i). */
    /*             If  JOB = 'U'  and  TRANS = 'T'  or  TRANS = 'C' then the */
    /*             leading N-by-N part of this array must contain in its i-th */
    /*             row the vector which defines the elementary reflector */
    /*             FU(i). */
    /*             If  JOB = 'V'  and  TRANS = 'N'  then the leading N-by-N */
    /*             part of this array must contain in its i-th row the vector */
    /*             which defines the elementary reflector FV(i). */
    /*             If  JOB = 'V'  and  TRANS = 'T'  or  TRANS = 'C' then the */
    /*             leading N-by-N part of this array must contain in its i-th */
    /*             column the vector which defines the elementary reflector */
    /*             FV(i). */
    /*             On exit, if  JOB = 'U'  and  TRANS = 'N'  then the leading */
    /*             N-by-N part of this array contains the matrix U1. */
    /*             If  JOB = 'U'  and  TRANS = 'T'  or  TRANS = 'C' then the */
    /*             leading N-by-N part of this array contains the matrix */
    /*             U1**T. */
    /*             If  JOB = 'V'  and  TRANS = 'N'  then the leading N-by-N */
    /*             part of this array contains the matrix V1**T. */
    /*             If  JOB = 'V'  and  TRANS = 'T'  or  TRANS = 'C' then the */
    /*             leading N-by-N part of this array contains the matrix V1. */
    /*     LDQ1    INTEGER */
    /*             The leading dimension of the array Q1.  LDQ1 >= MAX(1,N). */
    /*     Q2      (input/output) DOUBLE PRECISION array, dimension (LDQ2,N) */
    /*             On entry, if  JOB = 'U'  then the leading N-by-N part of */
    /*             this array must contain in its i-th column the vector */
    /*             which defines the elementary reflector HU(i). */
    /*             If  JOB = 'V'  then the leading N-by-N part of this array */
    /*             must contain in its i-th row the vector which defines the */
    /*             elementary reflector HV(i). */
    /*             On exit, if  JOB = 'U'  then the leading N-by-N part of */
    /*             this array contains the matrix U2. */
    /*             If  JOB = 'V'  then the leading N-by-N part of this array */
    /*             contains the matrix V2**T. */
    /*     LDQ2    INTEGER */
    /*             The leading dimension of the array Q2.  LDQ2 >= MAX(1,N). */
    /*     CS      (input) DOUBLE PRECISION array, dimension (2N) */
    /*             On entry, if  JOB = 'U'  then the first 2N elements of */
    /*             this array must contain the cosines and sines of the */
    /*             symplectic Givens rotators GU(i). */
    /*             If  JOB = 'V'  then the first 2N-2 elements of this array */
    /*             must contain the cosines and sines of the symplectic */
    /*             Givens rotators GV(i). */
    /*     TAU     (input) DOUBLE PRECISION array, dimension (N) */
    /*             On entry, if  JOB = 'U'  then the first N elements of */
    /*             this array must contain the scalar factors of the */
    /*             elementary reflectors FU(i). */
    /*             If  JOB = 'V'  then the first N-1 elements of this array */
    /*             must contain the scalar factors of the elementary */
    /*             reflectors FV(i). */
    /*     Workspace */
    /*     DWORK   DOUBLE PRECISION array, dimension (LDWORK) */
    /*             On exit, if INFO = 0,  DWORK(1)  returns the optimal */
    /*             value of LDWORK. */
    /*             On exit, if  INFO = -12,  DWORK(1)  returns the minimum */
    /*             value of LDWORK. */
    /*     LDWORK  INTEGER */
    /*             The length of the array DWORK. */
    /*             LDWORK >= MAX(1,2*(N-ILO+1)). */
    /*     Error Indicator */
    /*     INFO    INTEGER */
    /*             = 0:  successful exit; */
    /*             < 0:  if INFO = -i, the i-th argument had an illegal */
    /*                   value. */
    /*     REFERENCES */
    /*     [1] Benner, P., Mehrmann, V., and Xu, H. */
    /*         A numerically stable, structure preserving method for */
    /*         computing the eigenvalues of real Hamiltonian or symplectic */
    /*         pencils. Numer. Math., Vol 78 (3), pp. 329-358, 1998. */
    /*     [2] Kressner, D. */
    /*         Block algorithms for orthogonal symplectic factorizations. */
    /*         BIT, 43 (4), pp. 775-790, 2003. */
    /*     CONTRIBUTORS */
    /*     D. Kressner, Technical Univ. Berlin, Germany, and */
    /*     P. Benner, Technical Univ. Chemnitz, Germany, December 2003. */
    /*     REVISIONS */
    /*     V. Sima, June 2008 (SLICOT version of the HAPACK routine DOSGSU). */
    /*     KEYWORDS */
    /*     Elementary matrix operations, Hamiltonian matrix, orthogonal */
    /*     symplectic matrix. */
    /*     ****************************************************************** */
    /*     .. Parameters .. */
    /*     .. Scalar Arguments .. */
    /*     .. Array Arguments .. */
    /*     .. Local Scalars .. */
    /*     .. External Functions .. */
    /*     .. External Subroutines .. */
    /*     .. Intrinsic Functions .. */
    /*     .. Executable Statements .. */
    /*     Check the scalar input parameters. */
    /* Parameter adjustments */
    q1_dim1 = *ldq1;
    q1_offset = q1_dim1 + 1;
    q1 -= q1_offset;
    q2_dim1 = *ldq2;
    q2_offset = q2_dim1 + 1;
    q2 -= q2_offset;
    --cs;
    --tau;
    --dwork;
    /* Function Body */
    *info = 0;
    ltran = lsame_(trans, "T", 1L, 1L) || lsame_(trans, "C", 1L, 1L);
    compu = lsame_(job, "U", 1L, 1L);
    if (!compu && !lsame_(job, "V", 1L, 1L)) {
        *info = -1;
    } else if (!ltran && !lsame_(trans, "N", 1L, 1L)) {
        *info = -2;
    } else if (*n < 0) {
        *info = -3;
    } else if (*ilo < 1 || *ilo > max(1, *n)) {
        *info = -4;
    } else if (*ldq1 < max(1, *n)) {
        *info = -6;
    } else if (*ldq2 < max(1, *n)) {
        *info = -8;
    } else /* if(complicated condition) */
    {
        /* Computing MAX */
        i__1 = 1, i__2 = *n - *ilo + 1 << 1;
        if (*ldwork < max(i__1, i__2)) {
            /* Computing MAX */
            i__1 = 1, i__2 = *n - *ilo + 1 << 1;
            dwork[1] = (doublereal)max(i__1, i__2);
            *info = -12;
        }
    }
    /*     Return if there were illegal values. */
    if (*info != 0) {
        i__1 = -(*info);
        xerbla_("MB04WR", &i__1, 6L);
        return 0;
    }
    /*     Quick return if possible. */
    if (*n == 0) {
        dwork[1] = 1.;
        return 0;
    }
    if (compu) {
        i__1 = *ilo - 1;
        dlaset_("All", n, &i__1, &c_b9, &c_b10, &q1[q1_offset], ldq1, 3L);
        i__1 = *ilo - 1;
        i__2 = *n - *ilo + 1;
        dlaset_("All", &i__1, &i__2, &c_b9, &c_b9, &q1[*ilo * q1_dim1 + 1], ldq1, 3L);
        i__1 = *ilo - 1;
        dlaset_("All", n, &i__1, &c_b9, &c_b9, &q2[q2_offset], ldq2, 3L);
        i__1 = *ilo - 1;
        i__2 = *n - *ilo + 1;
        dlaset_("All", &i__1, &i__2, &c_b9, &c_b9, &q2[*ilo * q2_dim1 + 1], ldq2, 3L);
        nh = *n - *ilo + 1;
    }
    if (compu && !ltran) {
        /*        Generate U1 and U2. */
        if (nh > 0) {
            mb04wd_("No Transpose", "No Transpose", &nh, &nh, &nh, &q1[*ilo + *ilo * q1_dim1], ldq1,
                &q2[*ilo + *ilo * q2_dim1], ldq2, &cs[*ilo], &tau[*ilo], &dwork[1], ldwork, &ierr,
                12L, 12L);
        }
    } else if (compu && ltran) {
        /*        Generate U1**T and U2. */
        if (nh > 0) {
            mb04wd_("Transpose", "No Transpose", &nh, &nh, &nh, &q1[*ilo + *ilo * q1_dim1], ldq1,
                &q2[*ilo + *ilo * q2_dim1], ldq2, &cs[*ilo], &tau[*ilo], &dwork[1], ldwork, &ierr,
                9L, 12L);
        }
    } else if (!compu && !ltran) {
        /*        Generate V1**T and V2**T. */
        /*        Shift the vectors which define the elementary reflectors one */
        /*        column to the bottom, and set the first ilo rows and */
        /*        columns to those of the unit matrix. */
        i__1 = *n;
        for (i__ = 1; i__ <= i__1; ++i__) {
            i__2 = max(i__, *ilo) + 1;
            for (j = *n; j >= i__2; --j) {
                q1[j + i__ * q1_dim1] = 0.;
                /* L10: */
            }
            i__2 = *ilo + 1;
            for (j = max(i__, *ilo); j >= i__2; --j) {
                q1[j + i__ * q1_dim1] = q1[j - 1 + i__ * q1_dim1];
                /* L20: */
            }
            for (j = *ilo; j >= 1; --j) {
                q1[j + i__ * q1_dim1] = 0.;
                /* L30: */
            }
            if (i__ <= *ilo) {
                q1[i__ + i__ * q1_dim1] = 1.;
            }
            /* L40: */
        }
        i__1 = *n;
        for (i__ = 1; i__ <= i__1; ++i__) {
            i__2 = max(i__, *ilo) + 1;
            for (j = *n; j >= i__2; --j) {
                q2[j + i__ * q2_dim1] = 0.;
                /* L50: */
            }
            i__2 = *ilo + 1;
            for (j = max(i__, *ilo); j >= i__2; --j) {
                q2[j + i__ * q2_dim1] = q2[j - 1 + i__ * q2_dim1];
                /* L60: */
            }
            for (j = *ilo; j >= 1; --j) {
                q2[j + i__ * q2_dim1] = 0.;
                /* L70: */
            }
            /* L80: */
        }
        nh = *n - *ilo;
        if (nh > 0) {
            mb04wd_("Transpose", "Transpose", &nh, &nh, &nh, &q1[*ilo + 1 + (*ilo + 1) * q1_dim1],
                ldq1, &q2[*ilo + 1 + (*ilo + 1) * q2_dim1], ldq2, &cs[*ilo], &tau[*ilo], &dwork[1],
                ldwork, &ierr, 9L, 9L);
        }
    } else if (!compu && ltran) {
        /*        Generate V1 and V2**T. */
        /*        Shift the vectors which define the elementary reflectors one */
        /*        column to the right/bottom, and set the first ilo rows and */
        /*        columns to those of the unit matrix. */
        i__1 = *ilo + 1;
        for (j = *n; j >= i__1; --j) {
            i__2 = j - 1;
            for (i__ = 1; i__ <= i__2; ++i__) {
                q1[i__ + j * q1_dim1] = 0.;
                /* L90: */
            }
            i__2 = *n;
            for (i__ = j + 1; i__ <= i__2; ++i__) {
                q1[i__ + j * q1_dim1] = q1[i__ + (j - 1) * q1_dim1];
                /* L100: */
            }
            /* L110: */
        }
        dlaset_("All", n, ilo, &c_b9, &c_b10, &q1[q1_offset], ldq1, 3L);
        i__1 = *n;
        for (i__ = 1; i__ <= i__1; ++i__) {
            i__2 = max(i__, *ilo) + 1;
            for (j = *n; j >= i__2; --j) {
                q2[j + i__ * q2_dim1] = 0.;
                /* L120: */
            }
            i__2 = *ilo + 1;
            for (j = max(i__, *ilo); j >= i__2; --j) {
                q2[j + i__ * q2_dim1] = q2[j - 1 + i__ * q2_dim1];
                /* L130: */
            }
            for (j = *ilo; j >= 1; --j) {
                q2[j + i__ * q2_dim1] = 0.;
                /* L140: */
            }
            /* L150: */
        }
        nh = *n - *ilo;
        if (nh > 0) {
            mb04wd_("No Transpose", "Transpose", &nh, &nh, &nh,
                &q1[*ilo + 1 + (*ilo + 1) * q1_dim1], ldq1, &q2[*ilo + 1 + (*ilo + 1) * q2_dim1],
                ldq2, &cs[*ilo], &tau[*ilo], &dwork[1], ldwork, &ierr, 12L, 9L);
        }
    }
    return 0;
    /* *** Last line of MB04WR *** */
} /* mb04wr_ */
