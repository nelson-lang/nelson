//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "icholBuiltin.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cctype>
#include <map>
#include <vector>
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
#include "SparseAdvancedHelpers.hpp"
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
struct IcholOptions
{
    bool ict = false;
    double droptol = 0;
    double diagcomp = 0;
    bool michol = false;
    bool upper = false;
};
//=============================================================================
std::string
lowerString(const std::string& value)
{
    std::string result = value;
    std::transform(result.begin(), result.end(), result.begin(),
        [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    return result;
}
//=============================================================================
double
fieldScalar(const ArrayOf& options, const std::string& name)
{
    ArrayOf value = options.field(name);
    if (!isRealFiniteScalar(value)) {
        Error(L"Nelson:linear_algebra:optionValueNotScalar", L"%s must be a finite real scalar.",
            L"Option value");
    }
    return value.asDoubleScalar();
}
//=============================================================================
std::string
fieldText(const ArrayOf& options, const std::string& name)
{
    ArrayOf value = options.field(name);
    if (!value.isRowVectorCharacterArray() && !value.isScalarStringArray()) {
        Error(L"Nelson:linear_algebra:optionValueNotString",
            L"Option value must be a character row vector or a string scalar.");
    }
    return value.asUtf8String();
}
//=============================================================================
template <class T>
T
mapValue(const std::map<signedIndexType, T>& row, signedIndexType column)
{
    auto found = row.find(column);
    return found == row.end() ? T(0) : found->second;
}
//=============================================================================
template <class T>
T
rowHermitianDot(const std::map<signedIndexType, T>& first,
    const std::map<signedIndexType, T>& second, signedIndexType limit)
{
    T result = T(0);
    auto itFirst = first.begin();
    auto itSecond = second.begin();
    while (itFirst != first.end() && itSecond != second.end()) {
        if (itFirst->first >= limit || itSecond->first >= limit) {
            break;
        }
        if (itFirst->first == itSecond->first) {
            result += itFirst->second * Eigen::numext::conj(itSecond->second);
            ++itFirst;
            ++itSecond;
        } else if (itFirst->first < itSecond->first) {
            ++itFirst;
        } else {
            ++itSecond;
        }
    }
    return result;
}
//=============================================================================
template <class T>
double
realPart(const T& value)
{
    return Eigen::numext::real(value);
}
//=============================================================================
IcholOptions
parseIcholOptions(const ArrayOfVector& argIn)
{
    IcholOptions options;
    if (argIn.size() < 2 || argIn[1].isEmpty()) {
        return options;
    }
    if (!argIn[1].isStruct() || !argIn[1].isScalar()) {
        Error(L"Nelson:linear_algebra:invalidOptions", L"Options must be a scalar structure.");
    }
    for (const auto& field : argIn[1].fieldNames()) {
        if (field == "type") {
            std::string type = lowerString(fieldText(argIn[1], field));
            if (type == "ict") {
                options.ict = true;
            } else if (type != "nofill") {
                Error(L"Nelson:linear_algebra:unsupportedValue", L"Unsupported %s.",
                    L"incomplete Cholesky type");
            }
        } else if (field == "droptol") {
            options.droptol = fieldScalar(argIn[1], field);
            if (options.droptol < 0) {
                Error(L"Nelson:linear_algebra:mustBeNonnegativeScalar",
                    L"%s must be a nonnegative scalar.", L"droptol");
            }
        } else if (field == "diagcomp") {
            options.diagcomp = fieldScalar(argIn[1], field);
            if (options.diagcomp < 0) {
                Error(L"Nelson:linear_algebra:mustBeNonnegativeScalar",
                    L"%s must be a nonnegative scalar.", L"diagcomp");
            }
        } else if (field == "michol") {
            std::string value = lowerString(fieldText(argIn[1], field));
            if (value == "on") {
                options.michol = true;
            } else if (value == "off") {
                options.michol = false;
            } else {
                Error(L"Nelson:ichol:invalidMichol", L"michol must be 'on' or 'off'.");
            }
        } else if (field == "shape") {
            std::string value = lowerString(fieldText(argIn[1], field));
            if (value == "lower") {
                options.upper = false;
            } else if (value == "upper") {
                options.upper = true;
            } else {
                Error(L"Nelson:ichol:invalidShape", L"shape must be 'lower' or 'upper'.");
            }
        } else {
            Error(L"Nelson:linear_algebra:unsupportedValue", L"Unsupported %s.",
                L"incomplete Cholesky option");
        }
    }
    return options;
}
//=============================================================================
template <class T>
bool
isStructurallyDropped(const std::vector<std::map<signedIndexType, T>>& lowerA,
    const std::vector<double>& columnScale, signedIndexType row, signedIndexType column,
    const IcholOptions& options)
{
    if (!options.ict) {
        return false;
    }
    T original = mapValue<T>(lowerA[static_cast<size_t>(row)], column);
    if (std::abs(original) == 0) {
        return false;
    }
    return std::abs(original) <= options.droptol * columnScale[static_cast<size_t>(column)];
}
//=============================================================================
template <class T>
std::vector<T>
modifiedDiagonalShift(const std::vector<std::map<signedIndexType, T>>& lowerA,
    const std::vector<double>& columnScale, const IcholOptions& options)
{
    std::vector<T> shift(lowerA.size(), T(0));
    if (!options.michol || !options.ict) {
        return shift;
    }
    for (signedIndexType row = 0; row < static_cast<signedIndexType>(lowerA.size()); ++row) {
        for (const auto& entry : lowerA[static_cast<size_t>(row)]) {
            signedIndexType column = entry.first;
            if (row <= column) {
                continue;
            }
            if (isStructurallyDropped<T>(lowerA, columnScale, row, column, options)) {
                shift[static_cast<size_t>(row)] += entry.second;
                shift[static_cast<size_t>(column)] += Eigen::numext::conj(entry.second);
            }
        }
    }
    return shift;
}
//=============================================================================
template <class T>
SparseAdvancedMatrix<T>
incompleteCholesky(const SparseAdvancedMatrix<T>& A, const IcholOptions& options)
{
    using RealScalar = typename Eigen::NumTraits<T>::Real;
    const signedIndexType n = A.rows();
    if (!A.isApprox(A.adjoint())) {
        if constexpr (!Eigen::NumTraits<T>::IsComplex) {
            Error(L"Nelson:ichol:matrixMustBeSymmetric", L"Matrix must be symmetric.");
        } else {
            Error(L"Nelson:ichol:matrixMustBeHermitian", L"Matrix must be Hermitian.");
        }
    }
    std::vector<std::map<signedIndexType, T>> lowerA(n);
    std::vector<std::map<signedIndexType, T>> lowerL(n);
    std::vector<double> columnScale(static_cast<size_t>(n), 0);
    for (signedIndexType column = 0; column < A.outerSize(); ++column) {
        for (typename SparseAdvancedMatrix<T>::InnerIterator it(A, column); it; ++it) {
            columnScale[static_cast<size_t>(column)]
                = std::max(columnScale[static_cast<size_t>(column)],
                    static_cast<double>(std::abs(it.value())));
            if (it.row() >= it.col()) {
                lowerA[it.row()][it.col()] = it.value();
            }
        }
    }
    for (signedIndexType k = 0; k < n; ++k) {
        if (lowerA[k].find(k) == lowerA[k].end()) {
            Error(L"Nelson:ichol:zeroDiagonal",
                L"Input matrix must have a structurally nonzero diagonal.");
        }
    }
    std::vector<T> diagonalShift = modifiedDiagonalShift<T>(lowerA, columnScale, options);

    for (signedIndexType k = 0; k < n; ++k) {
        T originalDiagonal = mapValue<T>(lowerA[k], k);
        RealScalar diagonalScale = static_cast<RealScalar>(1. + options.diagcomp);
        double diagonal = realPart<T>(originalDiagonal * diagonalScale
            + diagonalShift[static_cast<size_t>(k)] - rowHermitianDot<T>(lowerL[k], lowerL[k], k));
        if (diagonal <= 0 || !std::isfinite(diagonal)) {
            Error(L"Nelson:linear_algebra:factorizationFailed", L"%s failed.",
                L"Incomplete Cholesky factorization");
        }
        RealScalar lkk = static_cast<RealScalar>(std::sqrt(diagonal));
        lowerL[k][k] = T(lkk);
        for (signedIndexType i = k + 1; i < n; ++i) {
            T original = mapValue<T>(lowerA[i], k);
            if (!options.ict && std::abs(original) == 0) {
                continue;
            }
            T value = (original - rowHermitianDot<T>(lowerL[i], lowerL[k], k)) / lkk;
            double dropThreshold = options.droptol * columnScale[static_cast<size_t>(k)];
            bool structurallyDropped = isStructurallyDropped<T>(lowerA, columnScale, i, k, options);
            if (std::abs(value) != 0
                && (!options.ict || (!structurallyDropped && std::abs(value) > dropThreshold))) {
                lowerL[i][k] = value;
            }
        }
    }

    std::vector<Eigen::Triplet<T, signedIndexType>> triplets;
    for (signedIndexType row = 0; row < n; ++row) {
        for (const auto& entry : lowerL[row]) {
            triplets.emplace_back(row, entry.first, entry.second);
        }
    }
    SparseAdvancedMatrix<T> L(n, n);
    L.setFromTriplets(triplets.begin(), triplets.end());
    return L;
}
//=============================================================================
template <class T>
ArrayOfVector
icholTyped(const ArrayOfVector& argIn, NelsonType dataClass)
{
    ArrayOf Aarg = argIn[0];
    if (!Aarg.isSparse() || Aarg.dataClass() != dataClass) {
        OverloadRequired("ichol");
    }
    if (!Aarg.isSquare()) {
        Error(L"Nelson:linear_algebra:squareMatrixExpected", L"%s expected.", L"Square matrix");
    }
    IcholOptions options = parseIcholOptions(argIn);
    SparseAdvancedMatrix<T> A = arrayToSparseMatrix<T>(Aarg);
    SparseAdvancedMatrix<T> L = incompleteCholesky<T>(A, options);
    if (options.upper) {
        L = L.adjoint();
    }
    ArrayOfVector retval;
    retval << sparseMatrixToArrayOf<T>(dataClass, Aarg.rows(), Aarg.columns(), L);
    return retval;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::icholBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    if (!argIn[0].isSparse() || !isFloatingPointClass(argIn[0].dataClass())) {
        OverloadRequired("ichol");
    }
    if (argIn[0].dataClass() == NLS_SCOMPLEX) {
        return icholTyped<singlecomplex>(argIn, NLS_SCOMPLEX);
    }
    if (argIn[0].dataClass() == NLS_DOUBLE) {
        return icholTyped<double>(argIn, NLS_DOUBLE);
    }
    if (argIn[0].dataClass() == NLS_DCOMPLEX) {
        return icholTyped<doublecomplex>(argIn, NLS_DCOMPLEX);
    }
    if (argIn[0].dataClass() == NLS_SINGLE) {
        return icholTyped<single>(argIn, NLS_SINGLE);
    }
    OverloadRequired("ichol");
    return {};
}
//=============================================================================
