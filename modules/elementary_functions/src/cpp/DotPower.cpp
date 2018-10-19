//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <limits>
#include <complex>
#include "DotPower.hpp"
#include "MatrixCheck.hpp"
#include "complex_abs.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
power_zi(double* p, const double* a, int b)
{
    std::complex<double> ca(a[0], a[1]);
    std::complex<double> cc = std::pow(ca, b);
    p[0] = cc.real();
    p[1] = cc.imag();
}
//=============================================================================
void
power_zz(double* c, const double* a, const double* b)
{
    double mag = complex_abs<double>(a[0], a[1]);
    if (mag == 0) {
        c[0] = 0;
        c[1] = 0;
        return;
    }
    std::complex<double> ca(a[0], a[1]);
    std::complex<double> cb(b[0], b[1]);
    std::complex<double> cc;
    if (ca.imag() == 0) {
        cc = pow(ca.real(), cb);
    } else {
        cc = std::exp(cb * log(ca));
    }
    c[0] = cc.real();
    if (ca.imag() == 0 && !std::isfinite(cc.imag())) {
        c[1] = 0;
    } else {
        c[1] = cc.imag();
    }
}
//=============================================================================
double
power_di(double a, int b)
{
    return std::pow(a, b);
}
//=============================================================================
double
power_dd(double a, double b)
{
    if (b == 0) {
        return a;
    }
    if (b == 1) {
        return a;
    }
    if (std::isnan(a)) {
        return std::nan("NaN");
    }
    if (std::isnan(b)) {
        return std::nan("NaN");
    }
    if (a == 0) {
        if (std::isinf(b) && b < 0) {
            return std::numeric_limits<double>::infinity();
        }
        if (std::isinf(b) && b > 0) {
            return 0;
        }
        if (b < 0) {
            return std::numeric_limits<double>::infinity();
        }
        return 0;
    }
    if (a == -1 && std::isinf(b)) {
        return 1;
    }
    if (std::isinf(b) && b > 0) {
        if (abs(a) > 1) {
            return std::numeric_limits<double>::infinity();
        }
    }
    if (std::isinf(b) && b < 0) {
        if (abs(a) > 1) {
            return 0;
        }
    }
    if (std::isinf(b) && b > 0) {
        if (abs(a) < 1) {
            return 0;
        }
    }
    if (std::isinf(b) && b < 0) {
        if (abs(a) < 1) {
            return std::numeric_limits<double>::infinity();
        }
    }
    if (std::isinf(a) && a > 0) {
        if (b > 0) {
            return std::numeric_limits<double>::infinity();
        }
        if (b < 0) {
            return 0;
        }
    }
    if (std::isinf(a) && a < 0) {
        return pow(-0, -b);
    }
    if (a < 0 && std::trunc(b) == b) {
        return std::nan("NaN");
    }
    return pow(a, b);
}
//=============================================================================
void
cicpower(int n, single* c, single* a, int stride1, int* b, int stride2)
{
    int m, p;
    double z1[2];
    double z3[2];
    m = 0;
    p = 0;
    for (int i = 0; i < n; i++) {
        z1[0] = a[2 * m];
        z1[1] = a[2 * m + 1];
        power_zi(z3, z1, b[p]);
        c[2 * i] = (single)z3[0];
        c[2 * i + 1] = (single)z3[1];
        m += stride1;
        p += stride2;
    }
}
//=============================================================================
void
cfcpower(int n, single* c, single* a, int stride1, single* b, int stride2)
{
    int m, p;
    double z1[2];
    double z2[2];
    double z3[2];
    m = 0;
    p = 0;
    for (int i = 0; i < n; i++) {
        z1[0] = a[2 * m];
        z1[1] = a[2 * m + 1];
        z2[0] = b[p];
        z2[1] = 0;
        power_zz(z3, z1, z2);
        c[2 * i] = (single)z3[0];
        c[2 * i + 1] = (single)z3[1];
        m += stride1;
        p += stride2;
    }
}
//=============================================================================
void
zdzpower(int n, double* c, double* a, int stride1, double* b, int stride2)
{
    int m, p;
    double z1[2];
    double z2[2];
    double z3[2];
    m = 0;
    p = 0;
    for (int i = 0; i < n; i++) {
        z1[0] = a[2 * m];
        z1[1] = a[2 * m + 1];
        z2[0] = b[p];
        z2[1] = 0;
        power_zz(z3, z1, z2);
        c[2 * i] = z3[0];
        c[2 * i + 1] = z3[1];
        m += stride1;
        p += stride2;
    }
}
//=============================================================================
void
cccpower(int n, single* c, single* a, int stride1, single* b, int stride2)
{
    int m, p;
    double z1[2];
    double z2[2];
    double z3[2];
    m = 0;
    p = 0;
    for (int i = 0; i < n; i++) {
        z1[0] = a[2 * m];
        z1[1] = a[2 * m + 1];
        z2[0] = b[2 * p];
        z2[1] = b[2 * p + 1];
        power_zz(z3, z1, z2);
        c[2 * i] = (single)z3[0];
        c[2 * i + 1] = (single)z3[1];
        m += stride1;
        p += stride2;
    }
}
//=============================================================================
void
zzzpower(int n, double* c, double* a, int stride1, double* b, int stride2)
{
    int m, p;
    double z1[2];
    double z2[2];
    double z3[2];
    m = 0;
    p = 0;
    for (int i = 0; i < n; i++) {
        z1[0] = a[2 * m];
        z1[1] = a[2 * m + 1];
        z2[0] = b[2 * p];
        z2[1] = b[2 * p + 1];
        power_zz(z3, z1, z2);
        c[2 * i] = z3[0];
        c[2 * i + 1] = z3[1];
        m += stride1;
        p += stride2;
    }
}
//=============================================================================
void
zizpower(int n, double* c, double* a, int stride1, int* b, int stride2)
{
    int m, p;
    double z1[2];
    double z3[2];
    m = 0;
    p = 0;
    for (int i = 0; i < n; i++) {
        z1[0] = a[2 * m];
        z1[1] = a[2 * m + 1];
        power_zi(z3, z1, b[p]);
        c[2 * i] = z3[0];
        c[2 * i + 1] = z3[1];
        m += stride1;
        p += stride2;
    }
}
//=============================================================================
void
didpower(int n, double* c, double* a, int stride1, int* b, int stride2)
{
    int m = 0, p = 0;
    for (int i = 0; i < n; i++) {
        c[i] = power_di(a[m], b[p]);
        m += stride1;
        p += stride2;
    }
}
//=============================================================================
void
dddpower(int n, double* c, double* a, int stride1, double* b, int stride2)
{
    int m = 0, p = 0;
    for (int i = 0; i < n; i++) {
        c[i] = power_dd(a[m], b[p]);
        m += stride1;
        p += stride2;
    }
}
//=============================================================================
void
fifpower(int n, single* c, single* a, int stride1, int* b, int stride2)
{
    int m = 0, p = 0;
    for (int i = 0; i < n; i++) {
        c[i] = (single)power_di(a[m], b[p]);
        m += stride1;
        p += stride2;
    }
}
//=============================================================================
void
fffpower(int n, single* c, single* a, int stride1, single* b, int stride2)
{
    int m = 0, p = 0;
    for (int i = 0; i < n; i++) {
        c[i] = (single)power_dd(a[m], b[p]);
        m += stride1;
        p += stride2;
    }
}
//=============================================================================
typedef void (*vvfun)(indexType length, void* result, const void* arg1, const int stride1,
    const void* arg2, const int stride2);
//=============================================================================
inline ArrayOf
doPowerAssist(ArrayOf A, Class AClass, ArrayOf B, Class BClass, Class CClass, vvfun exec)
{
    ArrayOf C;
    A.promoteType(AClass);
    B.promoteType(BClass);
    if (A.isScalar()) {
        indexType Blen(B.getLength());
        C = ArrayOf(CClass, B.getDimensions(), NULL);
        void* Cp = ArrayOf::allocateArrayOf(CClass, Blen * C.getElementSize());
        exec(Blen, Cp, A.getDataPointer(), 0, B.getDataPointer(), 1);
        C.setDataPointer(Cp);
    } else if (B.isScalar()) {
        indexType Alen(A.getLength());
        C = ArrayOf(CClass, A.getDimensions(), NULL);
        void* Cp = ArrayOf::allocateArrayOf(CClass, Alen * C.getElementSize());
        exec(Alen, Cp, A.getDataPointer(), 1, B.getDataPointer(), 0);
        C.setDataPointer(Cp);
    } else {
        indexType Alen(A.getLength());
        C = ArrayOf(CClass, A.getDimensions(), NULL);
        void* Cp = ArrayOf::allocateArrayOf(CClass, Alen * C.getElementSize());
        exec(Alen, Cp, A.getDataPointer(), 1, B.getDataPointer(), 1);
        C.setDataPointer(Cp);
    }
    return C;
}
//=============================================================================
#define OPCASE(t, o)                                                                               \
    case t:                                                                                        \
        opType = o;                                                                                \
        break;
#define MAPOP(o, a, b, c, f)                                                                       \
    case o:                                                                                        \
        return doPowerAssist(A, a, B, b, c, f);
//=============================================================================
inline ArrayOf
DoPowerTwoArgFunction(ArrayOf A, ArrayOf B)
{
    ArrayOf C;
    bool Anegative;
    stringVector dummySV;
    Class AClass, BClass;
    int opType;

    if (A.isEmpty() || B.isEmpty()) {
        Dimensions dims;
        if (A.isEmpty()) {
            dims = A.getDimensions();
        } else {
            dims = B.getDimensions();
        }
        return ArrayOf::emptyConstructor(dims);
    }
    CheckNumeric(A, B, "^");
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    if (!(SameSizeCheck(dimsA, dimsB) || A.isScalar() || B.isScalar())) {
        Error(_W("Size mismatch on arguments to power (^) operator."));
    }
    // If A is not at least a single type, promote it to double
    AClass = A.getDataClass();
    BClass = B.getDataClass();
    if (AClass < NLS_SINGLE)
        AClass = NLS_DOUBLE;
    if (BClass < NLS_INT64)
        BClass = NLS_INT64;
    // Get a read on if A is positive
    Anegative = !(A.isPositive());
    // Check through the different type cases...
    opType = 0;
    if (AClass == NLS_SCOMPLEX) {
        switch (BClass) {
        default:
            Error(_W("Unhandled type for second argument to A^B"));
            OPCASE(NLS_INT64, 1);
            OPCASE(NLS_SINGLE, 2);
            OPCASE(NLS_DOUBLE, 3);
            OPCASE(NLS_SCOMPLEX, 4);
            OPCASE(NLS_DCOMPLEX, 5);
        }
    } else if (AClass == NLS_DCOMPLEX) {
        switch (BClass) {
        default:
            Error(_W("Unhandled type for second argument to A^B"));
            OPCASE(NLS_INT64, 6);
            OPCASE(NLS_SINGLE, 3);
            OPCASE(NLS_DOUBLE, 3);
            OPCASE(NLS_SCOMPLEX, 5);
            OPCASE(NLS_DCOMPLEX, 5);
        }
    } else if (AClass == NLS_DOUBLE && Anegative) {
        switch (BClass) {
        default:
            Error(_W("Unhandled type for second argument to A^B"));
            OPCASE(NLS_INT64, 7);
            OPCASE(NLS_SINGLE, 5);
            OPCASE(NLS_DOUBLE, 5);
            OPCASE(NLS_SCOMPLEX, 5);
            OPCASE(NLS_DCOMPLEX, 5);
        }
    } else if (AClass == NLS_DOUBLE && (!Anegative)) {
        switch (BClass) {
        default:
            Error(_("Unhandled type for second argument to A^B"));
            OPCASE(NLS_INT64, 7);
            OPCASE(NLS_SINGLE, 8);
            OPCASE(NLS_DOUBLE, 8);
            OPCASE(NLS_SCOMPLEX, 5);
            OPCASE(NLS_DCOMPLEX, 5);
        }
    } else if (AClass == NLS_SINGLE && Anegative) {
        switch (BClass) {
        default:
            Error(_("Unhandled type for second argument to A^B"));
            OPCASE(NLS_INT64, 9);
            OPCASE(NLS_SINGLE, 4);
            OPCASE(NLS_DOUBLE, 5);
            OPCASE(NLS_SCOMPLEX, 4);
            OPCASE(NLS_DCOMPLEX, 5);
        }
    } else if (AClass == NLS_SINGLE && (!Anegative)) {
        switch (BClass) {
        default:
            Error(_("Unhandled type for second argument to A^B"));
            OPCASE(NLS_INT64, 9);
            OPCASE(NLS_SINGLE, 10);
            OPCASE(NLS_DOUBLE, 8);
            OPCASE(NLS_SCOMPLEX, 4);
            OPCASE(NLS_DCOMPLEX, 5);
        }
    }
    // Invoke the appropriate case
    switch (opType) {
    default:
        Error(_W("Unhandled type combination for A^B"));
        MAPOP(1, NLS_SCOMPLEX, NLS_INT64, NLS_SCOMPLEX, (vvfun)cicpower);
        MAPOP(2, NLS_SCOMPLEX, NLS_SINGLE, NLS_SCOMPLEX, (vvfun)cfcpower);
        MAPOP(3, NLS_DCOMPLEX, NLS_DOUBLE, NLS_DCOMPLEX, (vvfun)zdzpower);
        MAPOP(4, NLS_SCOMPLEX, NLS_SCOMPLEX, NLS_SCOMPLEX, (vvfun)cccpower);
        MAPOP(5, NLS_DCOMPLEX, NLS_DCOMPLEX, NLS_DCOMPLEX, (vvfun)zzzpower);
        MAPOP(6, NLS_DCOMPLEX, NLS_INT64, NLS_DCOMPLEX, (vvfun)zizpower);
        MAPOP(7, NLS_DOUBLE, NLS_INT64, NLS_DOUBLE, (vvfun)didpower);
        MAPOP(8, NLS_DOUBLE, NLS_DOUBLE, NLS_DOUBLE, (vvfun)dddpower);
        MAPOP(9, NLS_SINGLE, NLS_INT64, NLS_SINGLE, (vvfun)fifpower);
        MAPOP(10, NLS_SINGLE, NLS_SINGLE, NLS_SINGLE, (vvfun)fffpower);
    }
    return ArrayOf();
}
//=============================================================================
ArrayOf
DotPower(ArrayOf& A, ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    if (A.isSparse() || B.isSparse()) {
        needToOverload = true;
        return ArrayOf();
    }
    Class destinationClass;
    if (A.getDataClass() != B.getDataClass()) {
        if ((A.isDoubleClass() || A.isSingleClass()) && (B.isDoubleClass() || B.isSingleClass())) {
            if (A.isComplex() || B.isComplex()) {
                destinationClass = NLS_DCOMPLEX;
                A.promoteType(NLS_DCOMPLEX);
                B.promoteType(NLS_DCOMPLEX);
            } else {
                destinationClass = NLS_DOUBLE;
                A.promoteType(NLS_DOUBLE);
                B.promoteType(NLS_DOUBLE);
            }
        } else {
            needToOverload = true;
            return ArrayOf();
        }
    } else {
        switch (A.getDataClass()) {
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
        case NLS_CHAR: {
            destinationClass = A.getDataClass();
        } break;
        case NLS_LOGICAL:
        case NLS_HANDLE:
        case NLS_CELL_ARRAY:
        case NLS_STRING_ARRAY:
        case NLS_STRUCT_ARRAY:
        default: {
            needToOverload = true;
            return ArrayOf();
        } break;
        }
    }
    ArrayOf res = DoPowerTwoArgFunction(A, B);
    if (res.getDataClass() == NLS_DCOMPLEX) {
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } else if (res.getDataClass() == NLS_SCOMPLEX) {
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } else {
        res.promoteType(destinationClass);
    }
    return res;
}
//=============================================================================
}
//=============================================================================
