//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "nlsBuildConfig.h"
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include "ElementWiseMultiplication.hpp"
#include "MatrixCheck.hpp"
#include "NewWithException.hpp"
#include "i18n.hpp"
#include "IntegerOperations.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static ArrayOf
matrix_matrix_elementWiseMultiplication(
    NelsonType classDestination, const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsC = a.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(classDestination, Clen);
    ArrayOf res = ArrayOf(classDestination, dimsC, Cp, false);
#ifdef _NLS_WITH_OPENMP
    T* ptrA = (T*)a.getDataPointer();
    T* ptrB = (T*)b.getDataPointer();
    T* ptrC = (T*)Cp;
    if (IS_INTEGER_TYPE(classDestination)) {
        bool mustCastAsLongDouble = mustCastIntegerAsLongDouble(classDestination);
        bool mustCastAsDouble = mustCastIntegerAsDouble(classDestination);
#pragma omp parallel for
        for (ompIndexType k = 0; k < (ompIndexType)Clen; k++) {
            if (mustCastAsLongDouble) {
                ptrC[k] = static_cast<T>(
                    static_cast<long double>(ptrA[k]) * static_cast<long double>(ptrB[k]));

            } else if (mustCastAsDouble) {
                ptrC[k]
                    = static_cast<T>(static_cast<double>(ptrA[k]) * static_cast<double>(ptrB[k]));

            } else {
                ptrC[k]
                    = static_cast<T>(static_cast<single>(ptrA[k]) * static_cast<single>(ptrB[k]));
            }
        }
    } else {
#pragma omp parallel for
        for (ompIndexType k = 0; k < (ompIndexType)Clen; k++) {
            ptrC[k] = ptrA[k] * ptrB[k];
        }
    }
#else
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matC((T*)Cp, 1, Clen);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matA(
        (T*)a.getDataPointer(), 1, Clen);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matB(
        (T*)b.getDataPointer(), 1, Clen);
    if (IS_INTEGER_TYPE(classDestination)) {
        if (mustCastIntegerAsLongDouble(classDestination)) {
            matC = matA.template cast<long double>()
                       .cwiseProduct(matB.template cast<long double>())
                       .array()
                       .round()
                       .unaryExpr(std::ref(numeric_cast<long double, T>));
        } else if (mustCastIntegerAsDouble(classDestination)) {
            matC = matA.template cast<double>()
                       .cwiseProduct(matB.template cast<double>())
                       .array()
                       .round()
                       .unaryExpr(std::ref(numeric_cast<double, T>));
        } else {
            matC = matA.template cast<single>()
                       .cwiseProduct(matB.template cast<single>())
                       .array()
                       .round()
                       .unaryExpr(std::ref(numeric_cast<single, T>));
        }
    } else {
        matC = matA.cwiseProduct(matB);
    }
#endif
    return res;
}
//=============================================================================
template <class T>
static ArrayOf
complex_matrix_matrix_elementWiseMultiplication(
    NelsonType classDestination, const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsC = a.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = new_with_exception<T>(Clen * 2, false);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(Cp);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matC(Cz, 1, Clen);
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)a.getDataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matA(Az, 1, Clen);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>((T*)b.getDataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matB(Bz, 1, Clen);
    matC = matA.cwiseProduct(matB);
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
static ArrayOf
scalar_matrix_elementWiseMultiplication(NelsonType classDestination, ArrayOf& a, ArrayOf& b)
{
    Dimensions dimsC = b.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = new_with_exception<T>(Clen, false);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matC((T*)Cp, 1, Clen);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>> matB(
        (T*)b.getDataPointer(), 1, Clen);
    T* ptrA = (T*)a.getDataPointer();
    if (IS_INTEGER_TYPE(classDestination)) {
        if (mustCastIntegerAsLongDouble(classDestination)) {
            matC = ((long double)ptrA[0] * matB.template cast<long double>().array())
                       .array()
                       .round()
                       .unaryExpr(std::ref(numeric_cast<long double, T>));
        } else if (mustCastIntegerAsDouble(classDestination)) {
            matC = ((double)ptrA[0] * matB.template cast<double>().array())
                       .array()
                       .round()
                       .unaryExpr(std::ref(numeric_cast<double, T>));
        } else {
            matC = ((single)ptrA[0] * matB.template cast<single>().array())
                       .array()
                       .round()
                       .unaryExpr(std::ref(numeric_cast<single, T>));
        }
    } else {
        matC = ptrA[0] * matB.array();
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
static ArrayOf
complex_scalar_matrix_elementWiseMultiplication(NelsonType classDestination, ArrayOf& a, ArrayOf& b)
{
    Dimensions dimsC = b.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = new_with_exception<T>(Clen * 2, false);
    T* da = (T*)a.getDataPointer();
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(da);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(Cp);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matC(Cz, 1, Clen);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>((T*)b.getDataPointer());
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> matB(Bz, 1, Clen);
    matC = Az[0] * matB.array();
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
static void
vector_elementWiseMultiplication(
    NelsonType classDestination, T* C, const T* A, indexType NA, const T* B, indexType NB)
{
    indexType m = 0;
    if (IS_INTEGER_TYPE(classDestination)) {
        if (mustCastIntegerAsLongDouble(classDestination)) {
            for (indexType i = 0; i < NA; i++) {
                for (indexType j = 0; j < NB; j++) {
                    C[m] = numeric_cast<long double, T>((long double)A[i] * (long double)B[j]);
                    m++;
                }
            }
        } else if (mustCastIntegerAsDouble(classDestination)) {
            for (indexType i = 0; i < NA; i++) {
                for (indexType j = 0; j < NB; j++) {
                    C[m] = numeric_cast<double, T>((double)A[i] * (double)B[j]);
                    m++;
                }
            }
        } else {
            for (indexType i = 0; i < NA; i++) {
                for (indexType j = 0; j < NB; j++) {
                    C[m] = numeric_cast<single, T>((single)A[i] * (single)B[j]);
                    m++;
                }
            }
        }
    } else {
        for (indexType i = 0; i < NA; i++) {
            for (indexType j = 0; j < NB; j++) {
                C[m] = A[i] * B[j];
                m++;
            }
        }
    }
}
//=============================================================================
template <class T>
static void
complex_vector_elementWiseMultiplication(T* C, T* A, indexType NA, T* B, indexType NB)
{
    indexType m = 0;
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(A);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>(B);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(C);
    for (indexType i = 0; i < NA; i++) {
        for (indexType j = 0; j < NB; j++) {
            Cz[m] = Az[i] * Bz[j];
            m++;
        }
    }
}
//=============================================================================
template <class T>
static ArrayOf
vector_matrix_elementWiseMultiplication(
    NelsonType classDestination, const ArrayOf& a, const ArrayOf& b)
{
    const T* ptrA = (const T*)a.getDataPointer();
    const T* ptrB = (const T*)b.getDataPointer();
    indexType q = 0;
    Dimensions dimsC = b.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = new_with_exception<T>(Clen, false);
    T* C = (T*)Cp;

    if (IS_INTEGER_TYPE(classDestination)) {
        if (mustCastIntegerAsLongDouble(classDestination)) {
            for (indexType i = 0; i < dimsC.getRows(); i++) {
                for (indexType j = 0; j < dimsC.getColumns(); j++) {
                    indexType m = i + j * a.getRows();
                    C[m]
                        = numeric_cast<long double, T>((long double)ptrB[m] * (long double)ptrA[q]);
                }
                q++;
            }
        } else if (mustCastIntegerAsDouble(classDestination)) {
            for (indexType i = 0; i < dimsC.getRows(); i++) {
                for (indexType j = 0; j < dimsC.getColumns(); j++) {
                    indexType m = i + j * a.getRows();
                    C[m] = numeric_cast<double, T>((double)ptrB[m] * (double)ptrA[q]);
                }
                q++;
            }
        } else {
            for (indexType i = 0; i < dimsC.getRows(); i++) {
                for (indexType j = 0; j < dimsC.getColumns(); j++) {
                    indexType m = i + j * a.getRows();
                    C[m] = numeric_cast<single, T>((single)ptrB[m] * (single)ptrA[q]);
                }
                q++;
            }
        }
    } else {
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * a.getRows();
                C[m] = ptrB[m] * ptrA[q];
            }
            q++;
        }
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
static ArrayOf
complex_vector_matrix_elementWiseMultiplication(
    NelsonType classDestination, const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsC = b.getDimensions();
    indexType q = 0;
    indexType Clen = dimsC.getElementCount();
    T* ptrA = (T*)a.getDataPointer();
    T* ptrB = (T*)b.getDataPointer();
    void* Cp = new_with_exception<T>(Clen * 2, false);
    T* C = (T*)Cp;
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(ptrA);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>(ptrB);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(C);
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * a.getRows();
            Cz[m] = Bz[m] * Az[q];
        }
        q++;
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
static ArrayOf
vector_column_elementWiseMultiplication(
    NelsonType classDestination, const ArrayOf& a, const ArrayOf& b)
{
    const T* ptrA = (const T*)a.getDataPointer();
    const T* ptrB = (const T*)b.getDataPointer();
    Dimensions dimsC = b.getDimensions();
    indexType Clen = dimsC.getElementCount();
    void* Cp = new_with_exception<T>(Clen, false);
    T* C = (T*)Cp;
    if (IS_INTEGER_TYPE(classDestination)) {
        if (mustCastIntegerAsLongDouble(classDestination)) {
            for (indexType i = 0; i < dimsC.getRows(); i++) {
                for (indexType j = 0; j < dimsC.getColumns(); j++) {
                    indexType m = i + j * b.getRows();
                    C[m]
                        = numeric_cast<long double, T>((long double)ptrB[m] * (long double)ptrA[j]);
                }
            }
        } else if (mustCastIntegerAsDouble(classDestination)) {
            for (indexType i = 0; i < dimsC.getRows(); i++) {
                for (indexType j = 0; j < dimsC.getColumns(); j++) {
                    indexType m = i + j * b.getRows();
                    C[m] = numeric_cast<double, T>((double)ptrB[m] * (double)ptrA[j]);
                }
            }

        } else {
            for (indexType i = 0; i < dimsC.getRows(); i++) {
                for (indexType j = 0; j < dimsC.getColumns(); j++) {
                    indexType m = i + j * b.getRows();
                    C[m] = numeric_cast<single, T>((single)ptrB[m] * (single)ptrA[j]);
                }
            }
        }
    } else {
        for (indexType i = 0; i < dimsC.getRows(); i++) {
            for (indexType j = 0; j < dimsC.getColumns(); j++) {
                indexType m = i + j * b.getRows();
                C[m] = ptrB[m] * ptrA[j];
            }
        }
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
static ArrayOf
complex_vector_column_elementWiseMultiplication(
    NelsonType classDestination, const ArrayOf& a, const ArrayOf& b)
{
    indexType q = 0;
    Dimensions dimsC = b.getDimensions();
    indexType Clen = dimsC.getElementCount();
    T* ptrA = (T*)a.getDataPointer();
    T* ptrB = (T*)b.getDataPointer();
    void* Cp = new_with_exception<T>(Clen * 2, false);
    T* C = (T*)Cp;
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(ptrA);
    std::complex<T>* Bz = reinterpret_cast<std::complex<T>*>(ptrB);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(C);
    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * b.getRows();
            Cz[m] = Bz[m] * Az[j];
        }
    }
    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
template <class T>
ArrayOf
elementWiseMultiplication(NelsonType classDestination, ArrayOf a, ArrayOf b)
{
    void* Cp = nullptr;
    if (a.isScalar() && b.isScalar()) {
        T* ptrA = (T*)a.getDataPointer();
        T* ptrB = (T*)b.getDataPointer();
        void* Cp = ArrayOf::allocateArrayOf(classDestination, 1);
        T* C = (T*)Cp;
        if (IS_INTEGER_TYPE(classDestination)) {
            if (mustCastIntegerAsLongDouble(classDestination)) {
                C[0] = numeric_cast<long double, T>((long double)ptrA[0] * (long double)ptrB[0]);
            } else if (mustCastIntegerAsDouble(classDestination)) {
                C[0] = numeric_cast<double, T>((double)ptrA[0] * (double)ptrB[0]);
            } else {
                C[0] = numeric_cast<single, T>((single)ptrA[0] * (single)ptrB[0]);
            }
        } else {
            C[0] = ptrA[0] * ptrB[0];
        }
        Dimensions dimsC(1, 1);
        return ArrayOf(classDestination, dimsC, Cp, false);
    }
    if (a.isEmpty() || b.isEmpty()) {
        if (a.isScalar() || b.isScalar()) {
            if (a.isScalar()) {
                return ArrayOf(b);
            }
            return ArrayOf(a);
        }
        Dimensions dimsA = a.getDimensions();
        Dimensions dimsB = b.getDimensions();
        if (!(SameSizeCheck(dimsA, dimsB))) {
            Error(_("Size mismatch on arguments to arithmetic operator") + " " + "*");
        }
        return ArrayOf(b);
    }
    Dimensions dimsA = a.getDimensions();
    Dimensions dimsB = b.getDimensions();
    if (SameSizeCheck(dimsA, dimsB)) {
        if (a.isScalar()) {
            T* ptrC = static_cast<T*>(ArrayOf::allocateArrayOf(classDestination, 1));
            ptrC[0] = ((T*)a.getDataPointer())[0] * ((T*)b.getDataPointer())[0];
            return ArrayOf(classDestination, Dimensions(1, 1), ptrC, false);
        }
        return matrix_matrix_elementWiseMultiplication<T>(classDestination, a, b);
    }
    if (a.isScalar() || b.isScalar()) {
        if (a.isScalar()) {
            return scalar_matrix_elementWiseMultiplication<T>(classDestination, a, b);
        } // b.isScalar()
        return scalar_matrix_elementWiseMultiplication<T>(classDestination, b, a);

    } else {
        Dimensions dimsC;
        if (a.isVector() || b.isVector()) {
            if (a.isRowVector() && b.isColumnVector()) {
                dimsC = Dimensions(std::max(a.getDimensions().getMax(), b.getDimensions().getMax()),
                    std::min(a.getDimensions().getMax(), b.getDimensions().getMax()));
                indexType Clen = dimsC.getElementCount();
                Cp = new_with_exception<T>(Clen, false);
                vector_elementWiseMultiplication(classDestination, (T*)Cp,
                    (const T*)a.getDataPointer(), a.getElementCount(), (const T*)b.getDataPointer(),
                    b.getElementCount());
            } else if (a.isColumnVector() && b.isRowVector()) {
                dimsC = Dimensions(std::max(a.getDimensions().getMax(), b.getDimensions().getMax()),
                    std::min(a.getDimensions().getMax(), b.getDimensions().getMax()));
                indexType Clen = dimsC.getElementCount();
                Cp = new_with_exception<T>(Clen, false);
                vector_elementWiseMultiplication<T>(classDestination, (T*)Cp,
                    (const T*)b.getDataPointer(), b.getElementCount(), (const T*)a.getDataPointer(),
                    a.getElementCount());
            } else if ((a.isRowVector() && b.isRowVector())
                || (a.isColumnVector() && b.isColumnVector())) {
                Error(_("Size mismatch on arguments to arithmetic operator") + " " + "*");
            } else {
                if ((a.getRows() == b.getRows()) && (a.getRows() != 1)) {
                    if (a.isVector()) {
                        return vector_matrix_elementWiseMultiplication<T>(classDestination, a, b);
                    }
                    return vector_matrix_elementWiseMultiplication<T>(classDestination, b, a);
                }
                if (a.getColumns() == b.getColumns()) {
                    if (a.isVector()) {
                        return vector_column_elementWiseMultiplication<T>(classDestination, a, b);
                    }
                    return vector_column_elementWiseMultiplication<T>(classDestination, b, a);
                }
                Error(_("Size mismatch on arguments to arithmetic operator") + " " + "*");
            }
        } else {
            Error(_("Size mismatch on arguments to arithmetic operator") + " " + "*");
        }
        return ArrayOf(classDestination, dimsC, Cp, false);
    }

    return {};
}
//=============================================================================
template <class T>
ArrayOf
complex_elementWiseMultiplication(NelsonType classDestination, ArrayOf a, ArrayOf b)
{
    a.promoteType(classDestination);
    b.promoteType(classDestination);
    void* Cp = nullptr;
    if (a.isScalar() && b.isScalar()) {
        T* ptrA = (T*)a.getDataPointer();
        T* ptrB = (T*)b.getDataPointer();
        std::complex<T> ca(ptrA[0], ptrA[1]);
        std::complex<T> cb(ptrB[0], ptrB[1]);
        std::complex<T> res = ca * cb;
        if (classDestination == NLS_DCOMPLEX) {
            return ArrayOf::dcomplexConstructor((double)res.real(), (double)res.imag());
        }
        return ArrayOf::complexConstructor((single)res.real(), (single)res.imag());
    }
    Dimensions dimsC;
    if (a.isEmpty() || b.isEmpty()) {
        if (a.isScalar() || b.isScalar()) {
            if (a.isScalar()) {
                return ArrayOf(b);
            }
            return ArrayOf(a);
        }
        Dimensions dimsA = a.getDimensions();
        Dimensions dimsB = b.getDimensions();
        if (!(SameSizeCheck(dimsA, dimsB))) {
            Error(_("Size mismatch on arguments to arithmetic operator") + " " + "*");
        }
        return ArrayOf(b);
    }
    Dimensions dimsA = a.getDimensions();
    Dimensions dimsB = b.getDimensions();
    if (SameSizeCheck(dimsA, dimsB)) {
        return complex_matrix_matrix_elementWiseMultiplication<T>(classDestination, a, b);
    }
    if (a.isScalar() || b.isScalar()) {
        if (a.isScalar()) {
            return complex_scalar_matrix_elementWiseMultiplication<T>(classDestination, a, b);
        } // b.isScalar()
        return complex_scalar_matrix_elementWiseMultiplication<T>(classDestination, b, a);
    }
    if (a.isVector() || b.isVector()) {
        if (a.isRowVector() && b.isColumnVector()) {
            dimsC = Dimensions(std::min(a.getDimensions().getMax(), b.getDimensions().getMax()),
                std::max(a.getDimensions().getMax(), b.getDimensions().getMax()));
            indexType Clen = dimsC.getElementCount();
            Cp = new_with_exception<T>(Clen * 2, false);
            complex_vector_elementWiseMultiplication<T>((T*)Cp, (T*)a.getDataPointer(),
                a.getElementCount(), (T*)b.getDataPointer(), b.getElementCount());
        } else if (a.isColumnVector() && b.isRowVector()) {
            dimsC = Dimensions(std::min(a.getDimensions().getMax(), b.getDimensions().getMax()),
                std::max(a.getDimensions().getMax(), b.getDimensions().getMax()));
            indexType Clen = dimsC.getElementCount();
            Cp = new_with_exception<T>(Clen * 2, false);
            complex_vector_elementWiseMultiplication((T*)Cp, (T*)b.getDataPointer(),
                b.getElementCount(), (T*)a.getDataPointer(), a.getElementCount());
        } else if ((a.isRowVector() && b.isRowVector())
            || (a.isColumnVector() && b.isColumnVector())) {
            Error(_("Size mismatch on arguments to arithmetic operator") + " " + "*");
        } else {
            T* ptrA = (T*)a.getDataPointer();
            T* ptrB = (T*)b.getDataPointer();

            if (a.getRows() == b.getRows()) {
                if (a.isVector()) {
                    return complex_vector_matrix_elementWiseMultiplication<T>(
                        classDestination, a, b);
                }
                return complex_vector_matrix_elementWiseMultiplication<T>(classDestination, b, a);
            }
            if (a.getColumns() == b.getColumns()) {
                if (a.isVector()) {
                    return complex_vector_column_elementWiseMultiplication<T>(
                        classDestination, a, b);
                }
                return complex_vector_column_elementWiseMultiplication<T>(classDestination, b, a);
            }
            Error(_("Size mismatch on arguments to arithmetic operator") + " " + "*");
        }
    } else {
        Error(_("Size mismatch on arguments to arithmetic operator") + " " + "*");
    }

    return ArrayOf(classDestination, dimsC, Cp, false);
}
//=============================================================================
ArrayOf
double_elementWiseMultiplication_double(const ArrayOf& a, const ArrayOf& b)
{
    if (a.isComplex() || b.isComplex()) {
        ArrayOf res = complex_elementWiseMultiplication<double>(NLS_DCOMPLEX, a, b);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
        return res;
    }
    return elementWiseMultiplication<double>(NLS_DOUBLE, a, b);
}
//=============================================================================
ArrayOf
single_elementWiseMultiplication_single(const ArrayOf& a, const ArrayOf& b)
{
    if (a.isComplex() || b.isComplex()) {
        ArrayOf res = complex_elementWiseMultiplication<single>(NLS_SCOMPLEX, a, b);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
        return res;
    }
    return elementWiseMultiplication<single>(NLS_SINGLE, a, b);
}
//=============================================================================
ArrayOf
integer_elementWiseMultiplication_integer(const ArrayOf& a, const ArrayOf& b)
{
    switch (a.getDataClass()) {
    case NLS_INT8:
        return elementWiseMultiplication<int8>(NLS_INT8, a, b);
    case NLS_UINT8:
        return elementWiseMultiplication<uint8>(NLS_UINT8, a, b);
    case NLS_INT16:
        return elementWiseMultiplication<int16>(NLS_INT16, a, b);
    case NLS_UINT16:
        return elementWiseMultiplication<uint16>(NLS_UINT16, a, b);
    case NLS_INT32:
        return elementWiseMultiplication<int32>(NLS_INT32, a, b);
    case NLS_UINT32:
        return elementWiseMultiplication<uint32>(NLS_UINT32, a, b);
    case NLS_INT64:
        return elementWiseMultiplication<int64>(NLS_INT64, a, b);
    case NLS_UINT64:
        return elementWiseMultiplication<uint64>(NLS_UINT64, a, b);
    default:
        Error(_W("Integer type not managed."));
        break;
    }
    return {};
}
//=============================================================================
ArrayOf
elementWiseMultiplication(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    if (A.getDataClass() == B.getDataClass()) {
        switch (A.getDataClass()) {
        case NLS_INT8:
            return elementWiseMultiplication<int8>(NLS_INT8, A, B);
        case NLS_UINT8:
            return elementWiseMultiplication<uint8>(NLS_UINT8, A, B);
        case NLS_INT16:
            return elementWiseMultiplication<int16>(NLS_INT16, A, B);
        case NLS_UINT16:
            return elementWiseMultiplication<uint16>(NLS_UINT16, A, B);
        case NLS_INT32:
            return elementWiseMultiplication<int32>(NLS_INT32, A, B);
        case NLS_UINT32:
            return elementWiseMultiplication<uint32>(NLS_UINT32, A, B);
        case NLS_INT64:
            return elementWiseMultiplication<int64>(NLS_INT64, A, B);
        case NLS_UINT64:
            return elementWiseMultiplication<uint64>(NLS_UINT64, A, B);
        case NLS_LOGICAL: {
            if (A.isSparse() || B.isSparse()) {
                needToOverload = true;
                return {};
            }
            return elementWiseMultiplication<logical>(NLS_LOGICAL, A, B);
        } break;
        case NLS_DOUBLE:
        case NLS_DCOMPLEX:
            if (A.isSparse() || B.isSparse()) {
                needToOverload = true;
                return {};
            }
            return double_elementWiseMultiplication_double(A, B);
        case NLS_SINGLE:
        case NLS_SCOMPLEX:
            return single_elementWiseMultiplication_single(A, B);
        default: {
            needToOverload = true;
        } break;
        }
    } else {
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
            return elementWiseMultiplication(AA, BB, needToOverload);
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
            ArrayOf res = elementWiseMultiplication(AA, B, needToOverload);
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
            ArrayOf res = elementWiseMultiplication(A, BB, needToOverload);
            if (!needToOverload) {
                res.promoteType(B.getDataClass());
            }
            return res;
        }
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
