//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "MatrixMultiplication.hpp"
#include "IntegerOperations.hpp"
#include "MatrixCheck.hpp"
#include "Error.hpp"
#include "NewWithException.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isSizeMismatch(const ArrayOf& A, const ArrayOf& B)
{
    if (!A.isScalar() && !B.isScalar()) {
        return A.getColumns() != B.getRows();
    }
    bool isVectorOrScalar = ((A.isVector() && B.isScalar()) || (B.isVector() && A.isScalar())
        || (A.isRowVector() && B.isColumnVector()) || (B.isRowVector() && A.isColumnVector())
        || (A.isScalar() || B.isScalar()));
    return !isVectorOrScalar;
}
//=============================================================================
template <class T>
static ArrayOf
real_mtimes(NelsonType currentClass, const ArrayOf& A, const ArrayOf& B)
{
    Dimensions Cdim;
    if (A.isScalar() && B.isScalar()) {
        T* ptrC = (T*)ArrayOf::allocateArrayOf(currentClass, 1);
        if (A.getDataClass() == currentClass && B.getDataClass() == currentClass) {
            ptrC[0] = ((T*)A.getDataPointer())[0] * ((T*)B.getDataPointer())[0];
        } else {
            ArrayOf _A(A);
            _A.promoteType(currentClass);
            ArrayOf _B(B);
            _B.promoteType(currentClass);
            ptrC[0] = ((T*)_A.getDataPointer())[0] * ((T*)_B.getDataPointer())[0];
        }
        return ArrayOf(currentClass, Dimensions(1, 1), ptrC, false);
    }
    if (A.isVector() && B.isScalar()) {
        Cdim = A.getDimensions();
    } else if (B.isVector() && A.isScalar()) {
        Cdim = B.getDimensions();
    } else if ((A.isRowVector() && B.isColumnVector()) || (B.isRowVector() && A.isColumnVector())) {
        Cdim[0] = A.getRows();
        Cdim[1] = B.getColumns();
    } else {
        if (A.isScalar()) {
            Cdim = B.getDimensions();
        } else if (B.isScalar()) {
            Cdim = A.getDimensions();
        } else {
            Cdim[0] = A.getRows();
            Cdim[1] = B.getColumns();
        }
    }
    ArrayOf _A(A);
    ArrayOf _B(B);
    _A.promoteType(currentClass);
    _B.promoteType(currentClass);

    indexType Clen = Cdim.getElementCount();
    void* Cp = new_with_exception<T>(Clen, false);
    T* ptrC = (T*)Cp;
    size_t mC = Cdim.getRows();
    size_t nC = Cdim.getColumns();
    Eigen::Map<Eigen::Matrix<T, -1, -1>> matC((T*)Cp, mC, nC);
    Dimensions dimA = A.getDimensions();
    size_t mA = dimA.getRows();
    size_t nA = dimA.getColumns();
    Dimensions dimB = B.getDimensions();
    size_t mB = dimB.getRows();
    size_t nB = dimB.getColumns();
    if (A.isScalar()) {
        T* ptrA = (T*)_A.getDataPointer();
        T* ptrB = (T*)_B.getDataPointer();
        ompIndexType elementCount = (ompIndexType)dimB.getElementCount();
#if WITH_OPENMP
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            ptrC[k] = ptrA[0] * ptrB[k];
        }
#else
        Eigen::Map<Eigen::Matrix<T, -1, -1>> matB((T*)_B.getDataPointer(), mB, nB);
        matC = ptrA[0] * matB.array();
#endif
    } else if (B.isScalar()) {
        T* ptrA = (T*)_A.getDataPointer();
        T* ptrB = (T*)_B.getDataPointer();
        ompIndexType elementCount = (ompIndexType)dimA.getElementCount();
#if WITH_OPENMP
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            ptrC[k] = ptrA[k] * ptrB[0];
        }
#else
        Eigen::Map<Eigen::Matrix<T, -1, -1>> matA((T*)_A.getDataPointer(), mA, nA);
        matC = matA.array() * ptrB[0];
#endif
    } else {
        Eigen::Map<Eigen::Matrix<T, -1, -1>> matA((T*)_A.getDataPointer(), mA, nA);
        Eigen::Map<Eigen::Matrix<T, -1, -1>> matB((T*)_B.getDataPointer(), mB, nB);
        matC = matA * matB;
    }
    return ArrayOf(currentClass, Cdim, Cp, false);
}
//=============================================================================
template <class T>
static ArrayOf
integer_mtimes(const ArrayOf& A, const ArrayOf& B)
{
    Dimensions Cdim;
    if (A.isVector() && B.isScalar()) {
        Cdim = A.getDimensions();
    } else if (B.isVector() && A.isScalar()) {
        Cdim = B.getDimensions();
    } else if ((A.isRowVector() && B.isColumnVector()) || (B.isRowVector() && A.isColumnVector())) {
        Cdim[0] = A.getRows();
        Cdim[1] = B.getColumns();
    } else {
        if (A.isScalar()) {
            Cdim = B.getDimensions();
        } else if (B.isScalar()) {
            Cdim = A.getDimensions();
        } else {
            Cdim[0] = A.getRows();
            Cdim[1] = B.getColumns();
        }
    }
    indexType Clen = Cdim.getElementCount();
    void* Cp = new_with_exception<T>(Clen, false);
    size_t mC = Cdim.getRows();
    size_t nC = Cdim.getColumns();
    Dimensions dimA = A.getDimensions();
    size_t mA = dimA.getRows();
    size_t nA = dimA.getColumns();
    Dimensions dimB = B.getDimensions();
    size_t mB = dimB.getRows();
    size_t nB = dimB.getColumns();
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    T* ptrC = (T*)Cp;
    if (A.isScalar()) {
        ompIndexType elementCountB = (ompIndexType)dimB.getElementCount();
        OMP_PARALLEL_FOR_LOOP(elementCountB)
        for (ompIndexType k = 0; k < elementCountB; k++) {
            ptrC[k] = scalar_scalar_integer_times<T>(ptrA[0], ptrB[k]);
        }
    } else if (B.isScalar()) {
        ompIndexType elementCountA = (ompIndexType)dimA.getElementCount();
        OMP_PARALLEL_FOR_LOOP(elementCountA)
        for (ompIndexType k = 0; k < elementCountA; k++) {
            ptrC[k] = scalar_scalar_integer_times<T>(ptrA[k], ptrB[0]);
        }
    } else {
        Error(_W("At least one input argument must be scalar."));
    }
    return ArrayOf(A.getDataClass(), Cdim, Cp, false);
}
//=============================================================================
template <class T>
static ArrayOf
complex_mtimes(NelsonType currentClass, const ArrayOf& A, const ArrayOf& B)
{
    Dimensions Cdim;
    ArrayOf AA = A;
    ArrayOf BB = B;
    AA.promoteType(currentClass);
    BB.promoteType(currentClass);
    if (A.isVector() && B.isScalar()) {
        Cdim = A.getDimensions();
    } else if (B.isVector() && A.isScalar()) {
        Cdim = B.getDimensions();
    } else if ((A.isRowVector() && B.isColumnVector()) || (B.isRowVector() && A.isColumnVector())) {
        Cdim[0] = A.getRows();
        Cdim[1] = B.getColumns();
    } else {
        if (A.isScalar()) {
            Cdim = B.getDimensions();
        } else if (B.isScalar()) {
            Cdim = A.getDimensions();
        } else {
            Cdim[0] = A.getRows();
            Cdim[1] = B.getColumns();
        }
    }
    indexType Clen = Cdim.getElementCount();
    void* Cp = new_with_exception<T>(Clen * 2, false);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(Cp);
    size_t mC = Cdim.getRows();
    size_t nC = Cdim.getColumns();
    Eigen::Map<Eigen::Matrix<std::complex<T>, -1, -1>> matC(Cz, mC, nC);
    Dimensions dimA = A.getDimensions();
    size_t mA = dimA.getRows();
    size_t nA = dimA.getColumns();
    Dimensions dimB = B.getDimensions();
    size_t mB = dimB.getRows();
    size_t nB = dimB.getColumns();
    if (A.isScalar() && B.isScalar()) {
        std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)AA.getDataPointer());
        std::complex<T> cxa = Az[0];
        std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>((T*)BB.getDataPointer());
        std::complex<T> cxb = Bz[0];
        if ((cxa.real() == 0.) && (cxa.imag() == 0.) || (cxb.real() == 0.) && (cxb.imag() == 0.)) {
            T* pd = (T*)Cp;
            delete[] pd;
            pd = nullptr;
            if (currentClass == NLS_DCOMPLEX) {
                return ArrayOf::doubleConstructor(0);
            }
            return ArrayOf::singleConstructor(0);
        }
        Cz[0] = cxa * cxb;

    } else if (A.isScalar()) {
        std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)AA.getDataPointer());
        std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>((T*)BB.getDataPointer());
        if ((Az[0].real() == 0.) && (Az[0].imag() == 0.)) {
            T* pd = (T*)Cp;
            delete[] pd;
            pd = nullptr;
            Cp = ArrayOf::allocateArrayOf(NLS_DOUBLE, Cdim.getElementCount(), stringVector(), true);
            return ArrayOf(NLS_DOUBLE, Cdim, Cp);
        }
        ompIndexType elementCount = (ompIndexType)dimB.getElementCount();
#if WITH_OPENMP
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            Cz[k] = Az[0] * Bz[k];
        }
#else
        Eigen::Map<Eigen::Matrix<std::complex<T>, -1, -1>> matB(Bz, mB, nB);
        matC = Az[0] * matB.array();
#endif

    } else if (B.isScalar()) {
        std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>((T*)BB.getDataPointer());
        std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)AA.getDataPointer());
        Eigen::Map<Eigen::Matrix<std::complex<T>, -1, -1>> matA(Az, mA, nA);
        matC = matA.array() * Bz[0];
        if ((Bz[0].real() == 0.) && (Bz[0].imag() == 0.)) {
            T* pd = (T*)Cp;
            delete[] pd;
            pd = nullptr;
            Cp = ArrayOf::allocateArrayOf(
                A.getDataClass(), Cdim.getElementCount(), stringVector(), true);
            return ArrayOf(A.getDataClass(), Cdim, Cp);
        }
        ompIndexType elementCount = (ompIndexType)dimA.getElementCount();
#if WITH_OPENMP
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            Cz[k] = Az[k] * Bz[0];
        }
#else
        matC = matA.array() * Bz[0];
#endif

    } else {
        std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)AA.getDataPointer());
        Eigen::Map<Eigen::Matrix<std::complex<T>, -1, -1>> matA(Az, mA, nA);
        std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>((T*)BB.getDataPointer());
        Eigen::Map<Eigen::Matrix<std::complex<T>, -1, -1>> matB(Bz, mB, nB);
        matC = matA * matB;
    }
    return ArrayOf(currentClass, Cdim, Cp, false);
}
//=============================================================================
template <class T>
ArrayOf
T_mtimes_T(NelsonType realClass, NelsonType complexClass, const ArrayOf& A, const ArrayOf& B)
{
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    bool withScalar = A.isScalar() || B.isScalar();
    if ((dimsA.getLength() > 2 || dimsB.getLength() > 2) && !withScalar) {
        Error(ERROR_WRONG_ARGUMENTS_SIZE_2D_MATRIX_EXPECTED);
    }
    if (A.isEmpty() || B.isEmpty()) {
        dimsA.simplify();
        dimsB.simplify();
        // [] * 2
        // 2 * []
        if (A.isScalar() || B.isScalar()) {
            if (A.isScalar()) {
                return ArrayOf(B);
            }
            return ArrayOf(A);
        }
        // [] * [] = []
        if (A.isEmpty(true) && B.isEmpty(true)) {
            ArrayOf res = ArrayOf::emptyConstructor(dimsA);
            res.promoteType(realClass);
            return res;
        }
        // [](mx0) * [](0xn) = 0(mxn)
        if ((dimsA[1] == 0) && (dimsB[0] == 0) && (dimsB.getLength() < 3)) {
            Dimensions dimsC(dimsA[0], dimsB[1]);
            T* pT = (T*)ArrayOf::allocateArrayOf(
                realClass, dimsC.getElementCount(), stringVector(), true);
            return ArrayOf(realClass, dimsC, pT, false);
        }
        // [](0xm) * M(mxn) = [](0xn)
        if ((dimsA[0] == 0) && (dimsA[1] == dimsB[0])) {
            Dimensions dimsC(0, dimsB[1]);
            ArrayOf res = ArrayOf::emptyConstructor(dimsC);
            res.promoteType(realClass);
            return res;
        }
        // M(mxn) * [](nx0) = [](mx0)
        if ((dimsB[0] == dimsA[1]) && (dimsB.getLength() < 3)) {
            Dimensions dimsC(dimsA[0], 0);
            ArrayOf res = ArrayOf::emptyConstructor(dimsC);
            res.promoteType(realClass);
            return res;
        }
        Error(_("Size mismatch on arguments to arithmetic operator") + " " + "*");
    }
    if ((!A.is2D() || !B.is2D()) && !withScalar) {
        Error(ERROR_WRONG_ARGUMENTS_SIZE_2D_MATRIX_EXPECTED);
    }
    if (isSizeMismatch(A, B)) {
        Error(_("Size mismatch on arguments to arithmetic operator") + " " + "*");
    }
    if (A.isEmpty()) {
        size_t mA = dimsA.getRows();
        size_t nA = dimsA.getColumns();
        if (mA == nA) {
            if (B.isScalar()) {
                // [] * X returns []
                return { B.getDataClass() };
            }
            Error(_W("using operator '*' \n Matrix dimensions must agree."));
        }
    }
    if (A.isComplex() || B.isComplex()) {
        ArrayOf res = complex_mtimes<T>(complexClass, A, B);
        if (res.allReal()) {
            res.promoteType(realClass);
        }
        return res;
    }
    return real_mtimes<T>(realClass, A, B);
}
//=============================================================================
template <class T>
ArrayOf
integer_mtimes_integer(const ArrayOf& A, const ArrayOf& B)
{
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    if (dimsA.getLength() > 2 || dimsB.getLength() > 2) {
        Error(ERROR_WRONG_ARGUMENTS_SIZE_2D_MATRIX_EXPECTED);
    }
    if (A.isEmpty() || B.isEmpty()) {
        dimsA.simplify();
        dimsB.simplify();
        // [] * 2
        // 2 * []
        if (A.isScalar() || B.isScalar()) {
            if (A.isScalar()) {
                return ArrayOf(B);
            }
            return ArrayOf(A);
        }
        // [] * [] = []
        if (A.isEmpty(true) && B.isEmpty(true)) {
            ArrayOf res = ArrayOf::emptyConstructor(dimsA);
            res.promoteType(A.getDataClass());
            return res;
        }
        // [](mx0) * [](0xn) = 0(mxn)
        if ((dimsA[1] == 0) && (dimsB[0] == 0) && (dimsB.getLength() < 3)) {
            Dimensions dimsC(dimsA[0], dimsB[1]);
            T* pT = (T*)ArrayOf::allocateArrayOf(
                A.getDataClass(), dimsC.getElementCount(), stringVector(), true);
            return ArrayOf(A.getDataClass(), dimsC, pT, false);
        }
        // [](0xm) * M(mxn) = [](0xn)
        if ((dimsA[0] == 0) && (dimsA[1] == dimsB[0])) {
            Dimensions dimsC(0, dimsB[1]);
            ArrayOf res = ArrayOf::emptyConstructor(dimsC);
            res.promoteType(A.getDataClass());
            return res;
        }
        // M(mxn) * [](nx0) = [](mx0)
        if ((dimsB[0] == dimsA[1]) && (dimsB.getLength() < 3)) {
            Dimensions dimsC(dimsA[0], 0);
            ArrayOf res = ArrayOf::emptyConstructor(dimsC);
            res.promoteType(A.getDataClass());
            return res;
        }
        Error(_("Size mismatch on arguments to arithmetic operator") + " " + "*");
    }
    if (!A.is2D() || !B.is2D()) {
        Error(ERROR_WRONG_ARGUMENTS_SIZE_2D_MATRIX_EXPECTED);
    }
    if (isSizeMismatch(A, B)) {
        Error(_("Size mismatch on arguments to arithmetic operator") + " " + "*");
    }
    if (A.isEmpty()) {
        size_t mA = dimsA.getRows();
        size_t nA = dimsA.getColumns();
        if (mA == nA) {
            if (B.isScalar()) {
                // [] * X returns []
                return { B.getDataClass() };
            }
            Error(_W("using operator '*' \n Matrix dimensions must agree."));
        }
    }
    return integer_mtimes<T>(A, B);
}
//=============================================================================
ArrayOf
matrixMultiplication(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    if (A.isSparse() || B.isSparse()) {
        needToOverload = true;
        return {};
    }

    if (A.isDoubleClass() && B.isDoubleClass()) {
        return T_mtimes_T<double>(NLS_DOUBLE, NLS_DCOMPLEX, A, B);
    }
    if (A.isSingleClass() && B.isSingleClass()) {
        return T_mtimes_T<single>(NLS_SINGLE, NLS_SCOMPLEX, A, B);
    }
    if (A.isSingleClass() && B.isDoubleClass()) {
        return T_mtimes_T<single>(NLS_SINGLE, NLS_SCOMPLEX, A, B);
    }
    if (A.isDoubleClass() && B.isSingleClass()) {
        return T_mtimes_T<single>(NLS_SINGLE, NLS_SCOMPLEX, A, B);
    }
    if (A.getDataClass() == B.getDataClass()) {
        switch (A.getDataClass()) {
        case NLS_UINT8:
            return integer_mtimes_integer<uint8>(A, B);
        case NLS_INT8:
            return integer_mtimes_integer<int8>(A, B);
        case NLS_UINT16:
            return integer_mtimes_integer<uint16>(A, B);
        case NLS_INT16:
            return integer_mtimes_integer<int16>(A, B);
        case NLS_UINT32:
            return integer_mtimes_integer<uint32>(A, B);
        case NLS_INT32:
            return integer_mtimes_integer<int32>(A, B);
        case NLS_UINT64:
            return integer_mtimes_integer<uint64>(A, B);
        case NLS_INT64:
            return integer_mtimes_integer<int64>(A, B);
        default:
            needToOverload = true;
            break;
        }
    } else {
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
            ArrayOf res = matrixMultiplication(AA, B, needToOverload);
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
            ArrayOf res = matrixMultiplication(A, BB, needToOverload);
            if (!needToOverload) {
                res.promoteType(B.getDataClass());
            }
            return res;
        }
    }
    needToOverload = true;
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
