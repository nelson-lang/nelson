//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#if _MSC_VER
#pragma warning(disable : 4146)
#endif
//=============================================================================
#include <limits>
#include <complex>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "IntegerOperations.hpp"
#include "DotPower.hpp"
#include "MatrixCheck.hpp"
#include "complex_abs.hpp"
#include "IEEEFP.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <typename T>
static inline T
powi(T a, T b)
{
    unsigned long u;
    T p = 1;
    T x = a;
    T n = b;

    if (n != 0) {
        if (n < 0) {
            if (std::is_signed<T>()) {
                n = -n;
            }
            x = 1 / x;
        }
        for (u = (unsigned long)n;;) {
            if (u & 01) {
                p = scalar_scalar_integer_times(p, x);
            }
            if (u >>= 1) {
                x = scalar_scalar_integer_times(x, x);
            } else {
                break;
            }
        }
    }
    return (p);
}
//=============================================================================
void
power_zi(double* p, const double* a, int b)
{
    std::complex<double> ca(a[0], a[1]);
    std::complex<double> cc = std::pow(ca, b);
    if (fabs(cc.real()) < std::numeric_limits<single>::epsilon()) {
        p[0] = 0;
    } else {
        p[0] = cc.real();
    }
    if (fabs(cc.imag()) < std::numeric_limits<double>::epsilon()) {
        p[1] = 0;
    } else {
        p[1] = cc.imag();
    }
}
//=============================================================================
void
power_zz(double* c, const double* a, const double* b)
{
    if (b[0] == 0 && b[1] == 0) {
        c[0] = 1;
        c[1] = 0;
        return;
    }
    auto mag = complex_abs<double>(a[0], a[1]);
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
    if (fabs(cc.real()) < std::numeric_limits<single>::epsilon()) {
        c[0] = 0;
    } else {
        c[0] = cc.real();
    }
    if (ca.imag() == 0 && !std::isfinite(cc.imag())) {
        c[1] = 0;
    } else {
        if (fabs(cc.imag()) < std::numeric_limits<single>::epsilon()) {
            c[1] = 0;
        } else {
            c[1] = cc.imag();
        }
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
        return 1;
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
cicpower(int n, single* c, const single* a, int stride1, int* b, int stride2)
{
    if (stride1 == 1 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z3[2];
            z1[0] = a[2 * i];
            z1[1] = a[2 * i + 1];
            power_zi(z3, z1, b[i]);
            c[2 * i] = static_cast<single>(z3[0]);
            c[2 * i + 1] = static_cast<single>(z3[1]);
        }
    } else if (stride1 == 1 && stride2 == 0) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z3[2];
            z1[0] = a[2 * i];
            z1[1] = a[2 * i + 1];
            power_zi(z3, z1, b[0]);
            c[2 * i] = static_cast<single>(z3[0]);
            c[2 * i + 1] = static_cast<single>(z3[1]);
        }
    } else if (stride1 == 0 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z3[2];
            z1[0] = a[2 * 0];
            z1[1] = a[2 * 0 + 1];
            power_zi(z3, z1, b[i]);
            c[2 * i] = static_cast<single>(z3[0]);
            c[2 * i + 1] = static_cast<single>(z3[1]);
        }
    }
}
//=============================================================================
void
cfcpower(int n, single* c, const single* a, int stride1, const single* b, int stride2)
{
    if (stride1 == 1 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z2[2];
            double z3[2];
            z1[0] = a[2 * i];
            z1[1] = a[2 * i + 1];
            z2[0] = b[i];
            z2[1] = 0;
            power_zz(z3, z1, z2);
            c[2 * i] = static_cast<single>(z3[0]);
            c[2 * i + 1] = static_cast<single>(z3[1]);
        }
    } else if (stride1 == 0 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z2[2];
            double z3[2];
            z1[0] = a[2 * 0];
            z1[1] = a[2 * 0 + 1];
            z2[0] = b[i];
            z2[1] = 0;
            power_zz(z3, z1, z2);
            c[2 * i] = static_cast<single>(z3[0]);
            c[2 * i + 1] = static_cast<single>(z3[1]);
        }
    } else if (stride1 == 1 && stride2 == 0) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z2[2];
            double z3[2];
            z1[0] = a[2 * i];
            z1[1] = a[2 * i + 1];
            z2[0] = b[0];
            z2[1] = 0;
            power_zz(z3, z1, z2);
            c[2 * i] = static_cast<single>(z3[0]);
            c[2 * i + 1] = static_cast<single>(z3[1]);
        }
    }
}
//=============================================================================
void
zdzpower(int n, double* c, const double* a, int stride1, const double* b, int stride2)
{
    if (stride1 == 1 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z2[2];
            double z3[2];
            z1[0] = a[2 * i];
            z1[1] = a[2 * i + 1];
            z2[0] = b[i];
            z2[1] = 0;
            power_zz(z3, z1, z2);
            c[2 * i] = z3[0];
            c[2 * i + 1] = z3[1];
        }
    } else if (stride1 == 0 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z2[2];
            double z3[2];
            z1[0] = a[2 * 0];
            z1[1] = a[2 * 0 + 1];
            z2[0] = b[i];
            z2[1] = 0;
            power_zz(z3, z1, z2);
            c[2 * i] = z3[0];
            c[2 * i + 1] = z3[1];
        }
    } else if (stride1 == 1 && stride2 == 0) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z2[2];
            double z3[2];
            z1[0] = a[2 * i];
            z1[1] = a[2 * i + 1];
            z2[0] = b[0];
            z2[1] = 0;
            power_zz(z3, z1, z2);
            c[2 * i] = z3[0];
            c[2 * i + 1] = z3[1];
        }
    }
}
//=============================================================================
void
cccpower(int n, single* c, const single* a, int stride1, const single* b, int stride2)
{
    if (stride1 == 1 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z2[2];
            double z3[2];
            z1[0] = a[2 * i];
            z1[1] = a[2 * i + 1];
            z2[0] = b[2 * i];
            z2[1] = b[2 * i + 1];
            power_zz(z3, z1, z2);
            c[2 * i] = static_cast<single>(z3[0]);
            c[2 * i + 1] = static_cast<single>(z3[1]);
        }
    } else if (stride1 == 1 && stride2 == 0) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z2[2];
            double z3[2];
            z1[0] = a[2 * i];
            z1[1] = a[2 * i + 1];
            z2[0] = b[2 * 0];
            z2[1] = b[2 * 0 + 1];
            power_zz(z3, z1, z2);
            c[2 * i] = static_cast<single>(z3[0]);
            c[2 * i + 1] = static_cast<single>(z3[1]);
        }
    } else if (stride1 == 0 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z2[2];
            double z3[2];
            z1[0] = a[2 * 0];
            z1[1] = a[2 * 0 + 1];
            z2[0] = b[2 * i];
            z2[1] = b[2 * i + 1];
            power_zz(z3, z1, z2);
            c[2 * i] = static_cast<single>(z3[0]);
            c[2 * i + 1] = static_cast<single>(z3[1]);
        }
    }
}
//=============================================================================
void
zzzpower(int n, double* c, const double* a, int stride1, const double* b, int stride2)
{
    if (stride1 == 1 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z2[2];
            double z3[2];

            z1[0] = a[2 * i];
            z1[1] = a[2 * i + 1];
            z2[0] = b[2 * i];
            z2[1] = b[2 * i + 1];
            power_zz(z3, z1, z2);
            c[2 * i] = z3[0];
            c[2 * i + 1] = z3[1];
        }
    } else if (stride1 == 0 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z2[2];
            double z3[2];
            z1[0] = a[0];
            z1[1] = a[1];
            z2[0] = b[2 * i];
            z2[1] = b[2 * i + 1];
            power_zz(z3, z1, z2);
            c[2 * i] = z3[0];
            c[2 * i + 1] = z3[1];
        }
    } else if (stride1 == 1 && stride2 == 0) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z2[2];
            double z3[2];
            z1[0] = a[2 * i];
            z1[1] = a[2 * i + 1];
            z2[0] = b[2 * 0];
            z2[1] = b[2 * 0 + 1];
            power_zz(z3, z1, z2);
            c[2 * i] = z3[0];
            c[2 * i + 1] = z3[1];
        }
    }
}
//=============================================================================
void
zizpower(int n, double* c, const double* a, int stride1, int* b, int stride2)
{
    if (stride1 == 1 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z3[2];
            z1[0] = a[2 * i];
            z1[1] = a[2 * i + 1];
            power_zi(z3, z1, b[i]);
            c[2 * i] = z3[0];
            c[2 * i + 1] = z3[1];
        }
    } else if (stride1 == 0 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z3[2];
            z1[0] = a[2 * 0];
            z1[1] = a[2 * 0 + 1];
            power_zi(z3, z1, b[i]);
            c[2 * i] = z3[0];
            c[2 * i + 1] = z3[1];
        }
    } else if (stride1 == 1 && stride2 == 0) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            double z1[2];
            double z3[2];
            z1[0] = a[2 * i];
            z1[1] = a[2 * i + 1];
            power_zi(z3, z1, b[0]);
            c[2 * i] = z3[0];
            c[2 * i + 1] = z3[1];
        }
    }
}
//=============================================================================
void
didpower(int n, double* c, double* a, int stride1, int* b, int stride2)
{
    if (stride1 == 1 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            c[i] = power_di(a[i], b[i]);
        }
    } else if (stride1 == 1 && stride2 == 0) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            c[i] = power_di(a[i], b[0]);
        }
    } else if (stride1 == 0 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            c[i] = power_di(a[0], b[i]);
        }
    }
}
//=============================================================================
void
dddpower(int n, double* c, double* a, int stride1, double* b, int stride2)
{
    if (stride1 == 1 && stride2 == 0) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            c[i] = power_dd(a[i], b[0]);
        }
    } else if (stride1 == 0 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            c[i] = power_dd(a[0], b[i]);
        }
    } else if (stride1 == 1 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            c[i] = power_dd(a[i], b[i]);
        }
    }
}
//=============================================================================
void
fifpower(int n, single* c, single* a, int stride1, int* b, int stride2)
{
    if (stride1 == 1 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            c[i] = static_cast<single>(power_di(a[i], b[i]));
        }
    } else if (stride1 == 1 && stride2 == 0) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            c[i] = static_cast<single>(power_di(a[i], b[0]));
        }
    } else if (stride1 == 0 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            c[i] = static_cast<single>(power_di(a[0], b[i]));
        }
    }
}
//=============================================================================
void
fffpower(int n, single* c, single* a, int stride1, single* b, int stride2)
{
    if (stride1 == 1 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            c[i] = static_cast<single>(power_dd(a[i], b[i]));
        }
    } else if (stride1 == 1 && stride2 == 0) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            c[i] = static_cast<single>(power_dd(a[i], b[0]));
        }
    } else if (stride1 == 0 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < static_cast<ompIndexType>(n); i++) {
            c[i] = static_cast<single>(power_dd(a[0], b[i]));
        }
    }
}
//=============================================================================
using vvfun = void (*)(indexType, void*, const void*, const int, const void*, const int);
//=============================================================================
inline ArrayOf
doPowerAssist(
    ArrayOf A, NelsonType AClass, ArrayOf B, NelsonType BClass, NelsonType CClass, vvfun exec)
{
    ArrayOf C;
    A.promoteType(AClass);
    B.promoteType(BClass);
    if (A.isScalar()) {
        indexType Blen(B.getElementCount());
        C = ArrayOf(CClass, B.getDimensions(), nullptr);
        void* Cp
            = ArrayOf::allocateArrayOf(CClass, Blen * C.getElementSize(), stringVector(), true);
        exec(Blen, Cp, A.getDataPointer(), 0, B.getDataPointer(), 1);
        C.setDataPointer(Cp);
    } else if (B.isScalar()) {
        indexType Alen(A.getElementCount());
        C = ArrayOf(CClass, A.getDimensions(), nullptr);
        void* Cp
            = ArrayOf::allocateArrayOf(CClass, Alen * C.getElementSize(), stringVector(), true);
        exec(Alen, Cp, A.getDataPointer(), 1, B.getDataPointer(), 0);
        C.setDataPointer(Cp);
    } else {
        indexType Alen(A.getElementCount());
        C = ArrayOf(CClass, A.getDimensions(), nullptr);
        void* Cp
            = ArrayOf::allocateArrayOf(CClass, Alen * C.getElementSize(), stringVector(), true);
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
template <typename T>
static void
powerInteger(
    const ArrayOf& A, const ArrayOf& B, indexType n, indexType stride1, indexType stride2, void* Cp)
{
    T* a = (T*)A.getDataPointer();
    T* b = (T*)B.getDataPointer();
    T* c = static_cast<T*>(Cp);
    if (stride1 == 1 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < (ompIndexType)n; i++) {
            c[i] = powi<T>(a[i], b[i]);
        }
    } else if (stride1 == 1 && stride2 == 0) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < (ompIndexType)n; i++) {
            c[i] = powi<T>(a[i], b[0]);
        }
    } else if (stride1 == 0 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < (ompIndexType)n; i++) {
            c[i] = powi<T>(a[0], b[i]);
        }
    }
}
//=============================================================================
ArrayOf
DoPowerTwoArgFunction(const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf C;
    bool Anegative;
    NelsonType AClass;
    NelsonType BClass;
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
    AClass = A.getDataClass();
    BClass = B.getDataClass();
    if ((A.isIntegerType() || A.isNdArrayIntegerType())
        && (B.isIntegerType() || B.isNdArrayIntegerType())) {
        if (!B.isPositive()) {
            Error(_W("Only positive integers expected."));
        }
        void* Cp = nullptr;
        indexType stride1 = 0;
        indexType stride2 = 0;
        indexType n = 0;
        if (A.isScalar()) {
            indexType Blen(B.getElementCount());
            C = ArrayOf(AClass, B.getDimensions(), nullptr);
            Cp = ArrayOf::allocateArrayOf(AClass, Blen * C.getElementSize(), stringVector(), false);
            stride1 = 0;
            stride2 = 1;
            n = Blen;
        } else if (B.isScalar()) {
            indexType Alen(A.getElementCount());
            C = ArrayOf(AClass, A.getDimensions(), nullptr);
            Cp = ArrayOf::allocateArrayOf(AClass, Alen * C.getElementSize(), stringVector(), false);
            stride1 = 1;
            stride2 = 0;
            n = Alen;
        } else {
            indexType Alen(A.getElementCount());
            C = ArrayOf(AClass, A.getDimensions(), nullptr);
            Cp = ArrayOf::allocateArrayOf(AClass, Alen * C.getElementSize(), stringVector(), false);
            stride1 = 1;
            stride2 = 1;
            n = Alen;
        }
        indexType m = 0;
        indexType p = 0;
        switch (AClass) {
        case NLS_INT8: {
            powerInteger<int8>(A, B, n, stride1, stride2, Cp);
        } break;
        case NLS_UINT8: {
            powerInteger<uint8>(A, B, n, stride1, stride2, Cp);
        } break;
        case NLS_INT16: {
            powerInteger<int16>(A, B, n, stride1, stride2, Cp);
        } break;
        case NLS_UINT16: {
            powerInteger<uint16>(A, B, n, stride1, stride2, Cp);
        } break;
        case NLS_INT32: {
            powerInteger<int32>(A, B, n, stride1, stride2, Cp);
        } break;
        case NLS_UINT32: {
            powerInteger<uint32>(A, B, n, stride1, stride2, Cp);
        } break;
        case NLS_INT64: {
            powerInteger<int64>(A, B, n, stride1, stride2, Cp);
        } break;
        case NLS_UINT64: {
            powerInteger<uint64>(A, B, n, stride1, stride2, Cp);
        } break;
        default: {
            // never here
        } break;
        }
        C.setDataPointer(Cp);
        return C;
    }
    // If A is not at least a single type, promote it to double
    if (AClass != BClass) {
        if (AClass < NLS_SCOMPLEX) {
            AClass = NLS_DCOMPLEX;
        }
    }

    // Get a read on if A is positive
    Anegative = !(A.isPositive());
    // Check through the different type cases...
    opType = 0;
    if (AClass == NLS_SCOMPLEX) {
        switch (BClass) {
        default: {
            Error(_W("Unhandled type for second argument to A^B"));
        } break;
            OPCASE(NLS_INT64, 1);
            OPCASE(NLS_SINGLE, 2);
            OPCASE(NLS_DOUBLE, 3);
            OPCASE(NLS_SCOMPLEX, 4);
            OPCASE(NLS_DCOMPLEX, 5);
        }
    } else if (AClass == NLS_DCOMPLEX) {
        switch (BClass) {
        default: {
            Error(_W("Unhandled type for second argument to A^B"));
        } break;
            OPCASE(NLS_INT64, 6);
            OPCASE(NLS_SINGLE, 3);
            OPCASE(NLS_DOUBLE, 3);
            OPCASE(NLS_SCOMPLEX, 5);
            OPCASE(NLS_DCOMPLEX, 5);
        }
    } else if (AClass == NLS_DOUBLE && Anegative) {
        switch (BClass) {
        default: {
            Error(_W("Unhandled type for second argument to A^B"));
        } break;
            OPCASE(NLS_INT64, 7);
            OPCASE(NLS_SINGLE, 5);
            OPCASE(NLS_DOUBLE, 5);
            OPCASE(NLS_SCOMPLEX, 5);
            OPCASE(NLS_DCOMPLEX, 5);
        }
    } else if (AClass == NLS_DOUBLE && (!Anegative)) {
        switch (BClass) {
        default: {
            Error(_W("Unhandled type for second argument to A^B"));
        } break;
            OPCASE(NLS_INT64, 7);
            OPCASE(NLS_SINGLE, 8);
            OPCASE(NLS_DOUBLE, 8);
            OPCASE(NLS_SCOMPLEX, 5);
            OPCASE(NLS_DCOMPLEX, 5);
        }
    } else if (AClass == NLS_SINGLE && Anegative) {
        switch (BClass) {
        default: {
            Error(_W("Unhandled type for second argument to A^B"));
        } break;
            OPCASE(NLS_INT64, 9);
            OPCASE(NLS_SINGLE, 4);
            OPCASE(NLS_DOUBLE, 5);
            OPCASE(NLS_SCOMPLEX, 4);
            OPCASE(NLS_DCOMPLEX, 5);
        }
    } else if (AClass == NLS_SINGLE && (!Anegative)) {
        switch (BClass) {
        default: {
            Error(_("Unhandled type for second argument to A^B"));
        } break;
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
    return {};
}
//=============================================================================
ArrayOf
DotPower(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    if (A.isSparse() || B.isSparse()) {
        needToOverload = true;
        return {};
    }

    if (A.getDataClass() != B.getDataClass()) {
        if ((A.isDoubleClass() && B.isSingleClass()) || (A.isSingleClass() && B.isDoubleClass())) {
            bool isComplex = A.isComplex() || B.isComplex();
            ArrayOf AA = A;
            ArrayOf BB = B;
            if (isComplex) {
                AA.promoteType(NLS_SCOMPLEX);
                BB.promoteType(NLS_SCOMPLEX);
            } else {
                AA.promoteType(NLS_SINGLE);
                BB.promoteType(NLS_SINGLE);
            }
            return DotPower(AA, BB, needToOverload);
        }

        if (A.isIntegerType()) {
            bool isCompatible = (B.getDataClass() == NLS_DOUBLE) && B.isScalar();
            if (!isCompatible) {
                Error(_W("Integers can only be combined with integers of the same class, or scalar "
                         "doubles."));
            }
            if (B.isComplex()) {
                Error(_W("Complex integer not allowed for arithmetic operator ") + L"*");
            }
            ArrayOf AA = A;
            AA.promoteType(B.getDataClass());
            ArrayOf res = DotPower(AA, B, needToOverload);
            if (!needToOverload) {
                res.promoteType(A.getDataClass());
            }
            return res;
        } else if (B.isIntegerType()) {
            bool isCompatible = (A.getDataClass() == NLS_DOUBLE) && A.isScalar();
            if (!isCompatible) {
                Error(_W("Integers can only be combined with integers of the same class, or scalar "
                         "doubles."));
            }
            if (A.isComplex()) {
                Error(_W("Complex integer not allowed for arithmetic operator ") + L"*");
            }
            ArrayOf BB = B;
            BB.promoteType(A.getDataClass());
            ArrayOf res = DotPower(A, BB, needToOverload);
            if (!needToOverload) {
                res.promoteType(B.getDataClass());
            }
            return res;
        }
    }

    switch (A.getDataClass()) {
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64: {
        return DoPowerTwoArgFunction(A, B);
    } break;
    case NLS_SINGLE:
    case NLS_SCOMPLEX: {
        ArrayOf res = DoPowerTwoArgFunction(A, B);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
        return res;
    } break;
    case NLS_DOUBLE:
    case NLS_DCOMPLEX: {
        ArrayOf res = DoPowerTwoArgFunction(A, B);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
        return res;
    } break;
    case NLS_CHAR: {
        ArrayOf res = DoPowerTwoArgFunction(A, B);
        res.promoteType(NLS_DOUBLE);
        return res;
    } break;
    case NLS_LOGICAL:
    case NLS_GO_HANDLE:
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_CLASS_ARRAY:
    case NLS_FUNCTION_HANDLE:
    default: {
        needToOverload = true;
    } break;
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
