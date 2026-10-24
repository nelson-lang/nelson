//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include <Eigen/SparseLU>
#include <algorithm>
#include <cmath>
#include <initializer_list>
#include <type_traits>
#include <vector>
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "FunctionDef.hpp"
#include "SparseArrayOf.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T> using SparseAdvancedMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
template <class T> using SparseAdvancedVector = Eigen::Matrix<T, Eigen::Dynamic, 1>;
template <class T> using DenseAdvancedMatrix = Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>;
//=============================================================================
inline bool
isRealFiniteScalar(const ArrayOf& value)
{
    if (!value.isScalar() || !value.isNumeric() || !value.isReal()) {
        return false;
    }
    double scalar = value.asDoubleScalar();
    return std::isfinite(scalar);
}
//=============================================================================
inline double
optionalScalar(const ArrayOfVector& argIn, size_t index, double defaultValue)
{
    if (argIn.size() <= index || argIn[index].isEmpty()) {
        return defaultValue;
    }
    if (!isRealFiniteScalar(argIn[index])) {
        Error(L"Nelson:linear_algebra:expectedFiniteRealScalar", L"Expected a finite real scalar.");
    }
    return argIn[index].asDoubleScalar();
}
//=============================================================================
inline double
optionalTolerance(const ArrayOfVector& argIn, size_t index, double defaultValue)
{
    double value = optionalScalar(argIn, index, defaultValue);
    if (value < 0) {
        Error(
            L"Nelson:linear_algebra:invalidTolerance", L"Tolerance must be a nonnegative scalar.");
    }
    return value;
}
//=============================================================================
inline indexType
optionalIterationCount(const ArrayOfVector& argIn, size_t index, indexType defaultValue)
{
    if (argIn.size() <= index || argIn[index].isEmpty()) {
        return defaultValue;
    }
    if (!argIn[index].isScalar() || !argIn[index].isNumeric() || !argIn[index].isReal()) {
        Error(L"Nelson:linear_algebra:expectedNonNegativeInteger",
            L"Expected a nonnegative integer scalar.");
    }
    double value = argIn[index].asDoubleScalar();
    if (value < 0 || !std::isfinite(value) || std::floor(value) != value) {
        Error(L"Nelson:linear_algebra:expectedNonNegativeInteger",
            L"Expected a nonnegative integer scalar.");
    }
    return static_cast<indexType>(value);
}
//=============================================================================
inline bool
isDoubleOrDoubleComplexClass(const ArrayOf& value)
{
    return value.dataClass() == NLS_DOUBLE || value.dataClass() == NLS_DCOMPLEX;
}
//=============================================================================
inline bool
isFloatingPointClass(NelsonType dataClass)
{
    return dataClass == NLS_DOUBLE || dataClass == NLS_DCOMPLEX || dataClass == NLS_SINGLE
        || dataClass == NLS_SCOMPLEX;
}
//=============================================================================
inline bool
anyArgumentHasDataClass(
    const ArrayOfVector& argIn, std::initializer_list<size_t> indices, NelsonType dataClass)
{
    for (size_t index : indices) {
        if (argIn.size() > index && !argIn[index].isEmpty() && argIn[index].isNumeric()
            && argIn[index].dataClass() == dataClass) {
            return true;
        }
    }
    return false;
}
//=============================================================================
template <class T>
NelsonType
nelsonTypeForScalar()
{
    if constexpr (std::is_same<T, double>::value) {
        return NLS_DOUBLE;
    } else if constexpr (std::is_same<T, doublecomplex>::value) {
        return NLS_DCOMPLEX;
    } else if constexpr (std::is_same<T, single>::value) {
        return NLS_SINGLE;
    } else if constexpr (std::is_same<T, singlecomplex>::value) {
        return NLS_SCOMPLEX;
    }
    return NLS_DOUBLE;
}
//=============================================================================
template <class T, class V>
T
realValueToScalar(V value)
{
    using RealScalar = typename Eigen::NumTraits<T>::Real;
    return T(static_cast<RealScalar>(value));
}
//=============================================================================
inline bool
isCompatibleDoubleClass(const ArrayOf& value, NelsonType dataClass)
{
    if (dataClass == NLS_DOUBLE) {
        return value.dataClass() == NLS_DOUBLE;
    }
    if (dataClass == NLS_DCOMPLEX) {
        return isDoubleOrDoubleComplexClass(value);
    }
    return false;
}
//=============================================================================
inline bool
isCompatibleFloatingClass(const ArrayOf& value, NelsonType dataClass)
{
    if (dataClass == NLS_DOUBLE) {
        return value.dataClass() == NLS_DOUBLE;
    }
    if (dataClass == NLS_DCOMPLEX) {
        return value.dataClass() == NLS_DOUBLE || value.dataClass() == NLS_DCOMPLEX;
    }
    if (dataClass == NLS_SINGLE) {
        return value.dataClass() == NLS_SINGLE;
    }
    if (dataClass == NLS_SCOMPLEX) {
        return value.dataClass() == NLS_SINGLE || value.dataClass() == NLS_SCOMPLEX;
    }
    return false;
}
//=============================================================================
template <class T>
SparseAdvancedMatrix<T>
arrayToSparseMatrix(const ArrayOf& value)
{
    if (!value.is2D()) {
        Error(L"Nelson:linear_algebra:expected2DMatrix", L"2-D matrix expected.");
    }
    if (value.isSparse()) {
        if (value.dataClass() == NLS_DOUBLE) {
            const auto* sparse
                = static_cast<const SparseAdvancedMatrix<double>*>(value.sparseDataPointer());
            if constexpr (std::is_same<T, double>::value) {
                return *sparse;
            } else {
                SparseAdvancedMatrix<T> converted(sparse->rows(), sparse->cols());
                std::vector<Eigen::Triplet<T, signedIndexType>> triplets;
                triplets.reserve(static_cast<size_t>(sparse->nonZeros()));
                for (signedIndexType col = 0; col < sparse->outerSize(); ++col) {
                    for (SparseAdvancedMatrix<double>::InnerIterator it(*sparse, col); it; ++it) {
                        triplets.emplace_back(it.row(), it.col(), realValueToScalar<T>(it.value()));
                    }
                }
                converted.setFromTriplets(triplets.begin(), triplets.end());
                return converted;
            }
        }
        if (value.dataClass() == NLS_SINGLE) {
            const auto* sparse
                = static_cast<const SparseAdvancedMatrix<single>*>(value.sparseDataPointer());
            if constexpr (std::is_same<T, single>::value) {
                return *sparse;
            } else if constexpr (std::is_same<T, singlecomplex>::value) {
                SparseAdvancedMatrix<T> converted(sparse->rows(), sparse->cols());
                std::vector<Eigen::Triplet<T, signedIndexType>> triplets;
                triplets.reserve(static_cast<size_t>(sparse->nonZeros()));
                for (signedIndexType col = 0; col < sparse->outerSize(); ++col) {
                    for (SparseAdvancedMatrix<single>::InnerIterator it(*sparse, col); it; ++it) {
                        triplets.emplace_back(it.row(), it.col(), realValueToScalar<T>(it.value()));
                    }
                }
                converted.setFromTriplets(triplets.begin(), triplets.end());
                return converted;
            }
        }
        if constexpr (std::is_same<T, doublecomplex>::value) {
            if (value.dataClass() == NLS_DCOMPLEX) {
                return *static_cast<const SparseAdvancedMatrix<T>*>(value.sparseDataPointer());
            }
        }
        if constexpr (std::is_same<T, singlecomplex>::value) {
            if (value.dataClass() == NLS_SCOMPLEX) {
                return *static_cast<const SparseAdvancedMatrix<T>*>(value.sparseDataPointer());
            }
        }
        Error(L"Nelson:linear_algebra:inputTypeMismatch",
            L"Input type must match requested numeric class.");
    }
    DenseAdvancedMatrix<T> dense(value.rows(), value.columns());
    if (value.dataClass() == NLS_DOUBLE) {
        const double* data = static_cast<const double*>(value.dataPointer());
        for (indexType col = 0; col < value.columns(); ++col) {
            for (indexType row = 0; row < value.rows(); ++row) {
                dense(static_cast<signedIndexType>(row), static_cast<signedIndexType>(col))
                    = realValueToScalar<T>(data[row + col * value.rows()]);
            }
        }
    } else if (value.dataClass() == NLS_SINGLE) {
        const single* data = static_cast<const single*>(value.dataPointer());
        if constexpr (!std::is_same<T, single>::value && !std::is_same<T, singlecomplex>::value) {
            Error(L"Nelson:linear_algebra:inputTypeMismatch",
                L"Input type must match requested numeric class.");
        }
        for (indexType col = 0; col < value.columns(); ++col) {
            for (indexType row = 0; row < value.rows(); ++row) {
                dense(static_cast<signedIndexType>(row), static_cast<signedIndexType>(col))
                    = realValueToScalar<T>(data[row + col * value.rows()]);
            }
        }
    } else if constexpr (std::is_same<T, doublecomplex>::value) {
        if (value.dataClass() != NLS_DCOMPLEX) {
            Error(L"Nelson:linear_algebra:inputTypeMismatch",
                L"Input type must match requested numeric class.");
        }
        const T* data = static_cast<const T*>(value.dataPointer());
        for (indexType col = 0; col < value.columns(); ++col) {
            for (indexType row = 0; row < value.rows(); ++row) {
                dense(static_cast<signedIndexType>(row), static_cast<signedIndexType>(col))
                    = data[row + col * value.rows()];
            }
        }
    } else if constexpr (std::is_same<T, singlecomplex>::value) {
        if (value.dataClass() != NLS_SCOMPLEX) {
            Error(L"Nelson:linear_algebra:inputTypeMismatch",
                L"Input type must match requested numeric class.");
        }
        const T* data = static_cast<const T*>(value.dataPointer());
        for (indexType col = 0; col < value.columns(); ++col) {
            for (indexType row = 0; row < value.rows(); ++row) {
                dense(static_cast<signedIndexType>(row), static_cast<signedIndexType>(col))
                    = data[row + col * value.rows()];
            }
        }
    } else {
        Error(L"Nelson:linear_algebra:inputTypeMismatch",
            L"Input type must match requested numeric class.");
    }
    return dense.sparseView();
}
//=============================================================================
template <class T>
SparseAdvancedVector<T>
arrayToVector(const ArrayOf& value)
{
    if (!value.isVector()) {
        Error(L"Nelson:linear_algebra:expectedVector", L"Vector expected.");
    }
    SparseAdvancedVector<T> result(value.elementCount());
    result.setZero();
    if (value.isSparse()) {
        if (value.dataClass() == NLS_DOUBLE) {
            const auto* sparse
                = static_cast<const SparseAdvancedMatrix<double>*>(value.sparseDataPointer());
            for (signedIndexType col = 0; col < sparse->outerSize(); ++col) {
                for (SparseAdvancedMatrix<double>::InnerIterator it(*sparse, col); it; ++it) {
                    indexType linearIndex
                        = static_cast<indexType>(it.row() + it.col() * sparse->rows());
                    result(static_cast<signedIndexType>(linearIndex))
                        = realValueToScalar<T>(it.value());
                }
            }
        } else if constexpr (std::is_same<T, doublecomplex>::value) {
            if (value.dataClass() != NLS_DCOMPLEX) {
                Error(L"Nelson:linear_algebra:inputTypeMismatch",
                    L"Input type must match requested numeric class.");
            }
            const auto* sparse
                = static_cast<const SparseAdvancedMatrix<T>*>(value.sparseDataPointer());
            for (signedIndexType col = 0; col < sparse->outerSize(); ++col) {
                for (typename SparseAdvancedMatrix<T>::InnerIterator it(*sparse, col); it; ++it) {
                    indexType linearIndex
                        = static_cast<indexType>(it.row() + it.col() * sparse->rows());
                    result(static_cast<signedIndexType>(linearIndex)) = it.value();
                }
            }
        } else if constexpr (std::is_same<T, single>::value) {
            if (value.dataClass() != NLS_SINGLE) {
                Error(L"Nelson:linear_algebra:inputTypeMismatch",
                    L"Input type must match requested numeric class.");
            }
            const auto* sparse
                = static_cast<const SparseAdvancedMatrix<T>*>(value.sparseDataPointer());
            for (signedIndexType col = 0; col < sparse->outerSize(); ++col) {
                for (typename SparseAdvancedMatrix<T>::InnerIterator it(*sparse, col); it; ++it) {
                    indexType linearIndex
                        = static_cast<indexType>(it.row() + it.col() * sparse->rows());
                    result(static_cast<signedIndexType>(linearIndex)) = it.value();
                }
            }
        } else if constexpr (std::is_same<T, singlecomplex>::value) {
            if (value.dataClass() == NLS_SINGLE) {
                const auto* sparse
                    = static_cast<const SparseAdvancedMatrix<single>*>(value.sparseDataPointer());
                for (signedIndexType col = 0; col < sparse->outerSize(); ++col) {
                    for (SparseAdvancedMatrix<single>::InnerIterator it(*sparse, col); it; ++it) {
                        indexType linearIndex
                            = static_cast<indexType>(it.row() + it.col() * sparse->rows());
                        result(static_cast<signedIndexType>(linearIndex))
                            = realValueToScalar<T>(it.value());
                    }
                }
            } else if (value.dataClass() == NLS_SCOMPLEX) {
                const auto* sparse
                    = static_cast<const SparseAdvancedMatrix<T>*>(value.sparseDataPointer());
                for (signedIndexType col = 0; col < sparse->outerSize(); ++col) {
                    for (typename SparseAdvancedMatrix<T>::InnerIterator it(*sparse, col); it;
                        ++it) {
                        indexType linearIndex
                            = static_cast<indexType>(it.row() + it.col() * sparse->rows());
                        result(static_cast<signedIndexType>(linearIndex)) = it.value();
                    }
                }
            } else {
                Error(L"Nelson:linear_algebra:inputTypeMismatch",
                    L"Input type must match requested numeric class.");
            }
        } else {
            Error(L"Nelson:linear_algebra:inputTypeMismatch",
                L"Input type must match requested numeric class.");
        }
    } else {
        if (value.dataClass() == NLS_DOUBLE) {
            const double* data = static_cast<const double*>(value.dataPointer());
            for (indexType k = 0; k < value.elementCount(); ++k) {
                result(static_cast<signedIndexType>(k)) = realValueToScalar<T>(data[k]);
            }
        } else if (value.dataClass() == NLS_SINGLE) {
            const single* data = static_cast<const single*>(value.dataPointer());
            if constexpr (!std::is_same<T, single>::value
                && !std::is_same<T, singlecomplex>::value) {
                Error(L"Nelson:linear_algebra:inputTypeMismatch",
                    L"Input type must match requested numeric class.");
            }
            for (indexType k = 0; k < value.elementCount(); ++k) {
                result(static_cast<signedIndexType>(k)) = realValueToScalar<T>(data[k]);
            }
        } else if constexpr (std::is_same<T, doublecomplex>::value) {
            if (value.dataClass() != NLS_DCOMPLEX) {
                Error(L"Nelson:linear_algebra:inputTypeMismatch",
                    L"Input type must match requested numeric class.");
            }
            const T* data = static_cast<const T*>(value.dataPointer());
            for (indexType k = 0; k < value.elementCount(); ++k) {
                result(static_cast<signedIndexType>(k)) = data[k];
            }
        } else if constexpr (std::is_same<T, singlecomplex>::value) {
            if (value.dataClass() != NLS_SCOMPLEX) {
                Error(L"Nelson:linear_algebra:inputTypeMismatch",
                    L"Input type must match requested numeric class.");
            }
            const T* data = static_cast<const T*>(value.dataPointer());
            for (indexType k = 0; k < value.elementCount(); ++k) {
                result(static_cast<signedIndexType>(k)) = data[k];
            }
        } else {
            Error(L"Nelson:linear_algebra:inputTypeMismatch",
                L"Input type must match requested numeric class.");
        }
    }
    return result;
}
//=============================================================================
template <class T>
ArrayOf
vectorToArrayOf(
    NelsonType dataClass, const Dimensions& dimensions, const SparseAdvancedVector<T>& values)
{
    T* data = static_cast<T*>(ArrayOf::allocateArrayOf(dataClass, values.size()));
    for (signedIndexType k = 0; k < values.size(); ++k) {
        data[k] = values(k);
    }
    return ArrayOf(dataClass, dimensions, data, false);
}
//=============================================================================
inline ArrayOf
doubleColumnVector(const std::vector<double>& values)
{
    double* data = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, values.size()));
    for (size_t k = 0; k < values.size(); ++k) {
        data[k] = values[k];
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(values.size(), 1), data, false);
}
//=============================================================================
inline ArrayOf
doubleRowVector(const std::vector<double>& values)
{
    double* data = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, values.size()));
    for (size_t k = 0; k < values.size(); ++k) {
        data[k] = values[k];
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(1, values.size()), data, false);
}
//=============================================================================
template <class T>
ArrayOf
sparseMatrixToArrayOf(
    NelsonType dataClass, indexType rows, indexType cols, const SparseAdvancedMatrix<T>& matrix)
{
    auto* copied = new SparseAdvancedMatrix<T>(matrix);
    return makeSparseArrayOfFromEigen<T>(dataClass, Dimensions(rows, cols), copied, true);
}
//=============================================================================
template <class T>
bool
isLowerTriangular(const SparseAdvancedMatrix<T>& matrix)
{
    for (signedIndexType col = 0; col < matrix.outerSize(); ++col) {
        for (typename SparseAdvancedMatrix<T>::InnerIterator it(matrix, col); it; ++it) {
            if (it.row() < it.col() && it.value() != T(0)) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
template <class T>
bool
isUpperTriangular(const SparseAdvancedMatrix<T>& matrix)
{
    for (signedIndexType col = 0; col < matrix.outerSize(); ++col) {
        for (typename SparseAdvancedMatrix<T>::InnerIterator it(matrix, col); it; ++it) {
            if (it.row() > it.col() && it.value() != T(0)) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
template <class T>
bool
isDiagonalMatrix(const SparseAdvancedMatrix<T>& matrix)
{
    if (matrix.rows() != matrix.cols()) {
        return false;
    }
    for (signedIndexType col = 0; col < matrix.outerSize(); ++col) {
        for (typename SparseAdvancedMatrix<T>::InnerIterator it(matrix, col); it; ++it) {
            if (it.row() != it.col() && std::abs(it.value()) != 0) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
template <class T>
SparseAdvancedVector<T>
diagonalVectorFromMatrix(const SparseAdvancedMatrix<T>& matrix)
{
    SparseAdvancedVector<T> values(matrix.rows());
    for (signedIndexType k = 0; k < matrix.rows(); ++k) {
        values(k) = matrix.coeff(k, k);
        if (std::abs(values(k)) == 0) {
            Error(L"Nelson:linear_algebra:preconditionerZeroDiagonal",
                L"Preconditioner diagonal must not contain zeros.");
        }
    }
    return values;
}
//=============================================================================
template <class T>
void
validateNonZeroDiagonal(const SparseAdvancedMatrix<T>& matrix)
{
    const signedIndexType diagonalSize = std::min(matrix.rows(), matrix.cols());
    for (signedIndexType k = 0; k < diagonalSize; ++k) {
        if (std::abs(matrix.coeff(k, k)) == 0) {
            Error(L"Nelson:linear_algebra:preconditionerZeroDiagonal",
                L"Preconditioner diagonal must not contain zeros.");
        }
    }
}
//=============================================================================
template <class T> class ExplicitPreconditioner
{
    enum class Kind
    {
        None,
        Diagonal,
        SparseLower,
        SparseUpper,
        SparseLu,
        FunctionHandle,
    };

    Kind kind = Kind::None;
    NelsonType dataClass = NLS_DOUBLE;
    Evaluator* evaluator = nullptr;
    FunctionDef* functionPreconditioner = nullptr;
    bool functionUsesTransposeFlag = false;
    SparseAdvancedVector<T> diagonal;
    SparseAdvancedMatrix<T> matrix;
    mutable Eigen::SparseLU<SparseAdvancedMatrix<T>, Eigen::COLAMDOrdering<signedIndexType>> solver;

public:
    ExplicitPreconditioner() = default;

    bool
    isIdentity() const
    {
        return kind == Kind::None;
    }

    explicit ExplicitPreconditioner(const ArrayOf& value, bool preferLower,
        signedIndexType expectedSize = 0, Evaluator* eval = nullptr, bool useTransposeFlag = false)
    {
        compute(value, preferLower, expectedSize, eval, useTransposeFlag);
    }

    void
    compute(const ArrayOf& value, bool preferLower, signedIndexType expectedSize = 0,
        Evaluator* eval = nullptr, bool useTransposeFlag = false)
    {
        kind = Kind::None;
        dataClass = NLS_DOUBLE;
        evaluator = nullptr;
        functionPreconditioner = nullptr;
        functionUsesTransposeFlag = false;
        diagonal.resize(0);
        matrix.resize(0, 0);
        if (value.isEmpty()) {
            return;
        }
        if (value.isFunctionHandle()) {
            function_handle handle = value.asFunctionHandle();
            if (handle.anonymousHandle == nullptr) {
                Error(L"Nelson:linear_algebra:invalidFunctionHandle",
                    L"Argument must be a valid function_handle.");
            }
            functionPreconditioner = reinterpret_cast<FunctionDef*>(handle.anonymousHandle);
            evaluator = eval;
            functionUsesTransposeFlag = useTransposeFlag;
            kind = Kind::FunctionHandle;
            dataClass = nelsonTypeForScalar<T>();
            return;
        }
        dataClass = nelsonTypeForScalar<T>();
        if (!isCompatibleFloatingClass(value, dataClass)) {
            Error(L"Nelson:linear_algebra:preconditionerTypeMismatch",
                L"Preconditioner type must match input type.");
        }
        if (value.isVector() && !value.isSquare()) {
            if (expectedSize > 0
                && static_cast<signedIndexType>(value.elementCount()) != expectedSize) {
                Error(L"Nelson:linear_algebra:preconditionerDimensionMismatch",
                    L"Preconditioner dimensions must agree.");
            }
            diagonal = arrayToVector<T>(value);
            for (signedIndexType k = 0; k < diagonal.size(); ++k) {
                if (std::abs(diagonal(k)) == 0) {
                    Error(L"Nelson:linear_algebra:preconditionerZeroDiagonal",
                        L"Preconditioner diagonal must not contain zeros.");
                }
            }
            kind = Kind::Diagonal;
            return;
        }
        if (!value.isSquare()) {
            Error(L"Nelson:linear_algebra:preconditionerNotSquare",
                L"Preconditioner must be square.");
        }
        if (expectedSize > 0 && static_cast<signedIndexType>(value.rows()) != expectedSize) {
            Error(L"Nelson:linear_algebra:preconditionerDimensionMismatch",
                L"Preconditioner dimensions must agree.");
        }
        matrix = arrayToSparseMatrix<T>(value);
        if (isDiagonalMatrix<T>(matrix)) {
            diagonal = diagonalVectorFromMatrix<T>(matrix);
            kind = Kind::Diagonal;
            return;
        }
        if (preferLower && isLowerTriangular<T>(matrix)) {
            validateNonZeroDiagonal<T>(matrix);
            kind = Kind::SparseLower;
        } else if (!preferLower && isUpperTriangular<T>(matrix)) {
            validateNonZeroDiagonal<T>(matrix);
            kind = Kind::SparseUpper;
        } else {
            kind = Kind::SparseLu;
            solver.compute(matrix);
            if (solver.info() != Eigen::Success) {
                Error(L"Nelson:linear_algebra:preconditionerFactorizationFailed",
                    L"Preconditioner factorization failed.");
            }
        }
    }

    SparseAdvancedVector<T>
    solve(const SparseAdvancedVector<T>& rhs) const
    {
        switch (kind) {
        case Kind::None:
            return rhs;
        case Kind::FunctionHandle: {
            if (evaluator == nullptr || functionPreconditioner == nullptr) {
                Error(L"Nelson:linear_algebra:invalidFunctionHandle",
                    L"Argument must be a valid function_handle.");
            }
            ArrayOfVector inputs;
            inputs << vectorToArrayOf<T>(dataClass, Dimensions(rhs.size(), 1), rhs);
            if (functionUsesTransposeFlag) {
                inputs << ArrayOf::characterArray("notransp");
            }
            ArrayOfVector outputs = functionPreconditioner->evaluateFunction(evaluator, inputs, 1);
            if (outputs.empty()) {
                Error(L"Nelson:linear_algebra:preconditionerNotVector",
                    L"Function preconditioner must return a vector.");
            }
            SparseAdvancedVector<T> result = arrayToVector<T>(outputs[0]);
            if (result.size() != rhs.size()) {
                Error(L"Nelson:linear_algebra:preconditionerDimensionMismatch",
                    L"Preconditioner dimensions must agree.");
            }
            return result;
        }
        case Kind::Diagonal:
            if (diagonal.size() != rhs.size()) {
                Error(L"Nelson:linear_algebra:preconditionerDimensionMismatch",
                    L"Preconditioner dimensions must agree.");
            }
            return rhs.cwiseQuotient(diagonal);
        case Kind::SparseLower:
            return matrix.template triangularView<Eigen::Lower>().solve(rhs);
        case Kind::SparseUpper:
            return matrix.template triangularView<Eigen::Upper>().solve(rhs);
        case Kind::SparseLu:
            return solver.solve(rhs);
        }
        return rhs;
    }

    SparseAdvancedVector<T>
    solveAdjoint(const SparseAdvancedVector<T>& rhs) const
    {
        switch (kind) {
        case Kind::None:
            return rhs;
        case Kind::FunctionHandle:
            if (!functionUsesTransposeFlag) {
                Error(L"Nelson:linear_algebra:preconditionerAdjointUnsupported",
                    L"Function preconditioner adjoint is not supported.");
            }
            if (evaluator == nullptr || functionPreconditioner == nullptr) {
                Error(L"Nelson:linear_algebra:invalidFunctionHandle",
                    L"Argument must be a valid function_handle.");
            }
            {
                ArrayOfVector inputs;
                inputs << vectorToArrayOf<T>(dataClass, Dimensions(rhs.size(), 1), rhs);
                inputs << ArrayOf::characterArray("transp");
                ArrayOfVector outputs
                    = functionPreconditioner->evaluateFunction(evaluator, inputs, 1);
                if (outputs.empty()) {
                    Error(L"Nelson:linear_algebra:preconditionerNotVector",
                        L"Function preconditioner must return a vector.");
                }
                SparseAdvancedVector<T> result = arrayToVector<T>(outputs[0]);
                if (result.size() != rhs.size()) {
                    Error(L"Nelson:linear_algebra:preconditionerDimensionMismatch",
                        L"Preconditioner dimensions must agree.");
                }
                return result;
            }
        case Kind::Diagonal: {
            if (diagonal.size() != rhs.size()) {
                Error(L"Nelson:linear_algebra:preconditionerDimensionMismatch",
                    L"Preconditioner dimensions must agree.");
            }
            SparseAdvancedVector<T> result(rhs.size());
            for (signedIndexType k = 0; k < rhs.size(); ++k) {
                result(k) = rhs(k) / Eigen::numext::conj(diagonal(k));
            }
            return result;
        }
        case Kind::SparseLower:
            return matrix.adjoint().template triangularView<Eigen::Upper>().solve(rhs);
        case Kind::SparseUpper:
            return matrix.adjoint().template triangularView<Eigen::Lower>().solve(rhs);
        case Kind::SparseLu:
            return solver.adjoint().solve(rhs);
        }
        return rhs;
    }
};
//=============================================================================
template <class T>
SparseAdvancedVector<T>
applyPreconditioners(const ExplicitPreconditioner<T>& first,
    const ExplicitPreconditioner<T>& second, const SparseAdvancedVector<T>& rhs)
{
    if (first.isIdentity()) {
        return second.solve(rhs);
    }
    if (second.isIdentity()) {
        return first.solve(rhs);
    }
    return second.solve(first.solve(rhs));
}
//=============================================================================
template <class T>
SparseAdvancedVector<T>
applyPreconditionersAdjoint(const ExplicitPreconditioner<T>& first,
    const ExplicitPreconditioner<T>& second, const SparseAdvancedVector<T>& rhs)
{
    if (first.isIdentity()) {
        return second.solveAdjoint(rhs);
    }
    if (second.isIdentity()) {
        return first.solveAdjoint(rhs);
    }
    return first.solveAdjoint(second.solveAdjoint(rhs));
}
//=============================================================================
template <class T>
void
validateSystem(const SparseAdvancedMatrix<T>& A, const SparseAdvancedVector<T>& b)
{
    if (A.rows() != A.cols()) {
        Error(L"Nelson:linear_algebra:expectedSquareMatrix", L"Square matrix expected.");
    }
    if (A.rows() != b.size()) {
        Error(L"Nelson:linear_algebra:dimensionMismatch",
            L"Matrix and vector dimensions must agree.");
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
