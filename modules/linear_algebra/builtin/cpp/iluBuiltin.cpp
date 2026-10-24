//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "iluBuiltin.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cctype>
#include <cmath>
#include <map>
#include <utility>
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
struct IluOptions
{
    bool nofill = true;
    double droptol = 1e-4;
    int fillfactor = 10;
    bool udiag = false;
    double thresh = 1.;
};
//=============================================================================
template <class T> struct IluFactors
{
    SparseAdvancedMatrix<T> lower;
    SparseAdvancedMatrix<T> pivotedLower;
    SparseAdvancedMatrix<T> upper;
    std::vector<signedIndexType> rowPermutation;
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
IluOptions
parseIluOptions(const ArrayOfVector& argIn)
{
    IluOptions options;
    if (argIn.size() < 2 || argIn[1].isEmpty()) {
        return options;
    }
    if (!argIn[1].isStruct() || !argIn[1].isScalar()) {
        Error(L"Nelson:linear_algebra:invalidOptions", L"Options must be a scalar structure.");
    }
    for (const auto& field : argIn[1].fieldNames()) {
        if (field == "type") {
            std::string type = lowerString(fieldText(argIn[1], field));
            if (type == "nofill") {
                options.nofill = true;
                options.droptol = 0;
                options.fillfactor = 1;
            } else if (type == "ilutp") {
                options.nofill = false;
            } else {
                Error(L"Nelson:linear_algebra:unsupportedValue", L"Unsupported %s.",
                    L"incomplete LU type");
            }
        } else if (field == "droptol") {
            options.droptol = fieldScalar(argIn[1], field);
            if (options.droptol < 0) {
                Error(L"Nelson:linear_algebra:mustBeNonnegativeScalar",
                    L"%s must be a nonnegative scalar.", L"droptol");
            }
        } else if (field == "fillfactor") {
            double value = fieldScalar(argIn[1], field);
            if (value < 1 || std::floor(value) != value) {
                Error(L"Nelson:ilu:invalidFillfactor", L"%s must be a positive integer scalar.",
                    L"fillfactor");
            }
            options.fillfactor = static_cast<int>(value);
        } else if (field == "udiag") {
            ArrayOf value = argIn[1].field(field);
            if (!value.isLogical() || !value.isScalar()) {
                Error(L"Nelson:ilu:invalidUdiag", L"udiag must be a logical scalar.");
            }
            options.udiag = value.asLogicalScalar();
        } else if (field == "thresh") {
            options.thresh = fieldScalar(argIn[1], field);
            if (options.thresh < 0. || options.thresh > 1.) {
                Error(L"Nelson:ilu:invalidThresh", L"thresh must be a scalar between 0 and 1.");
            }
        } else {
            Error(L"Nelson:linear_algebra:unsupportedValue", L"Unsupported %s.",
                L"incomplete LU option");
        }
    }
    return options;
}
//=============================================================================
template <class T>
IluFactors<T>
nofillIlu(const SparseAdvancedMatrix<T>& A, double dropTolerance = 0.)
{
    const signedIndexType n = A.rows();
    std::vector<std::map<signedIndexType, T>> lowerA(n);
    std::vector<std::map<signedIndexType, T>> upperA(n);
    std::vector<std::map<signedIndexType, T>> lowerL(n);
    std::vector<std::map<signedIndexType, T>> upperU(n);
    for (signedIndexType column = 0; column < A.outerSize(); ++column) {
        for (typename SparseAdvancedMatrix<T>::InnerIterator it(A, column); it; ++it) {
            if (it.row() > it.col()) {
                lowerA[it.row()][it.col()] = it.value();
            } else {
                upperA[it.row()][it.col()] = it.value();
            }
        }
    }

    for (signedIndexType i = 0; i < n; ++i) {
        for (const auto& entry : lowerA[i]) {
            signedIndexType k = entry.first;
            T sum = T(0);
            for (const auto& lij : lowerL[i]) {
                if (lij.first >= k) {
                    break;
                }
                sum += lij.second * mapValue<T>(upperU[lij.first], k);
            }
            T pivot = mapValue<T>(upperU[k], k);
            if (std::abs(pivot) == 0) {
                Error(L"Nelson:linear_algebra:factorizationFailed", L"%s failed.",
                    L"Incomplete LU factorization");
            }
            T value = (entry.second - sum) / pivot;
            if (std::abs(value) > dropTolerance) {
                lowerL[i][k] = value;
            }
        }
        lowerL[i][i] = T(1);
        for (const auto& entry : upperA[i]) {
            signedIndexType k = entry.first;
            T sum = T(0);
            for (const auto& lij : lowerL[i]) {
                if (lij.first >= i) {
                    break;
                }
                sum += lij.second * mapValue<T>(upperU[lij.first], k);
            }
            T value = entry.second - sum;
            if (k == i && std::abs(value) == 0) {
                Error(L"Nelson:linear_algebra:factorizationFailed", L"%s failed.",
                    L"Incomplete LU factorization");
            }
            if (k == i || std::abs(value) > dropTolerance) {
                upperU[i][k] = value;
            }
        }
    }

    std::vector<Eigen::Triplet<T, signedIndexType>> lowerTriplets;
    std::vector<Eigen::Triplet<T, signedIndexType>> upperTriplets;
    for (signedIndexType row = 0; row < n; ++row) {
        for (const auto& entry : lowerL[row]) {
            lowerTriplets.emplace_back(row, entry.first, entry.second);
        }
        for (const auto& entry : upperU[row]) {
            upperTriplets.emplace_back(row, entry.first, entry.second);
        }
    }
    SparseAdvancedMatrix<T> L(n, n);
    SparseAdvancedMatrix<T> U(n, n);
    L.setFromTriplets(lowerTriplets.begin(), lowerTriplets.end());
    U.setFromTriplets(upperTriplets.begin(), upperTriplets.end());
    IluFactors<T> result;
    result.lower = L;
    result.pivotedLower = L;
    result.upper = U;
    result.rowPermutation.resize(static_cast<size_t>(n));
    for (signedIndexType row = 0; row < n; ++row) {
        result.rowPermutation[static_cast<size_t>(row)] = row;
    }
    return result;
}
//=============================================================================
template <class T>
void
dropSmallEntries(
    std::map<signedIndexType, T>& row, double dropTolerance, signedIndexType keepColumn)
{
    if (dropTolerance <= 0.) {
        return;
    }
    for (auto it = row.begin(); it != row.end();) {
        if (it->first != keepColumn && std::abs(it->second) <= dropTolerance) {
            it = row.erase(it);
        } else {
            ++it;
        }
    }
}
//=============================================================================
template <class T>
void
limitRowFill(std::map<signedIndexType, T>& row, signedIndexType keepColumn, size_t maxEntries)
{
    if (maxEntries == 0 || row.size() <= maxEntries) {
        return;
    }
    std::vector<std::pair<double, signedIndexType>> ordered;
    ordered.reserve(row.size());
    for (const auto& entry : row) {
        if (entry.first != keepColumn) {
            ordered.emplace_back(std::abs(entry.second), entry.first);
        }
    }
    std::sort(ordered.begin(), ordered.end(), [](const auto& left, const auto& right) {
        if (left.first == right.first) {
            return left.second < right.second;
        }
        return left.first > right.first;
    });
    while (row.size() > maxEntries && !ordered.empty()) {
        row.erase(ordered.back().second);
        ordered.pop_back();
    }
}
//=============================================================================
template <class T>
IluFactors<T>
ilutpIlu(const SparseAdvancedMatrix<T>& A, const IluOptions& options)
{
    const signedIndexType n = A.rows();
    std::vector<std::map<signedIndexType, T>> lowerL(n);
    std::vector<std::map<signedIndexType, T>> upperU(n);
    std::vector<signedIndexType> permutation(n);
    for (signedIndexType row = 0; row < n; ++row) {
        permutation[static_cast<size_t>(row)] = row;
        lowerL[static_cast<size_t>(row)][row] = T(1);
    }
    for (signedIndexType column = 0; column < A.outerSize(); ++column) {
        for (typename SparseAdvancedMatrix<T>::InnerIterator it(A, column); it; ++it) {
            upperU[static_cast<size_t>(it.row())][it.col()] = it.value();
        }
    }

    const size_t maxEntries = static_cast<size_t>(std::max(2, options.fillfactor * 2 + 1));
    for (signedIndexType k = 0; k < n; ++k) {
        signedIndexType maxPivotRow = k;
        double pivotMagnitude = 0.;
        for (signedIndexType row = k; row < n; ++row) {
            double magnitude = std::abs(mapValue<T>(upperU[static_cast<size_t>(row)], k));
            if (magnitude > pivotMagnitude) {
                pivotMagnitude = magnitude;
                maxPivotRow = row;
            }
        }
        signedIndexType pivotRow = maxPivotRow;
        double diagonalMagnitude = std::abs(mapValue<T>(upperU[static_cast<size_t>(k)], k));
        if (diagonalMagnitude != 0. && diagonalMagnitude >= options.thresh * pivotMagnitude) {
            pivotRow = k;
            pivotMagnitude = diagonalMagnitude;
        }
        if (pivotMagnitude == 0.) {
            if (!options.udiag) {
                Error(L"Nelson:linear_algebra:factorizationFailed", L"%s failed.",
                    L"Incomplete LU factorization");
            }
            continue;
        }
        if (pivotRow != k) {
            std::swap(upperU[static_cast<size_t>(pivotRow)], upperU[static_cast<size_t>(k)]);
            std::swap(
                permutation[static_cast<size_t>(pivotRow)], permutation[static_cast<size_t>(k)]);
            for (signedIndexType col = 0; col < k; ++col) {
                std::swap(lowerL[static_cast<size_t>(pivotRow)][col],
                    lowerL[static_cast<size_t>(k)][col]);
            }
        }
        T pivot = mapValue<T>(upperU[static_cast<size_t>(k)], k);
        for (signedIndexType row = k + 1; row < n; ++row) {
            T current = mapValue<T>(upperU[static_cast<size_t>(row)], k);
            if (std::abs(current) == 0.) {
                continue;
            }
            T factor = current / pivot;
            if (std::abs(factor) > options.droptol) {
                lowerL[static_cast<size_t>(row)][k] = factor;
            }
            upperU[static_cast<size_t>(row)].erase(k);
            for (const auto& pivotEntry : upperU[static_cast<size_t>(k)]) {
                if (pivotEntry.first <= k) {
                    continue;
                }
                T value = mapValue<T>(upperU[static_cast<size_t>(row)], pivotEntry.first)
                    - factor * pivotEntry.second;
                if (std::abs(value) > options.droptol || pivotEntry.first == row) {
                    upperU[static_cast<size_t>(row)][pivotEntry.first] = value;
                } else {
                    upperU[static_cast<size_t>(row)].erase(pivotEntry.first);
                }
            }
            dropSmallEntries<T>(upperU[static_cast<size_t>(row)], options.droptol, row);
            limitRowFill<T>(upperU[static_cast<size_t>(row)], row, maxEntries);
        }
    }

    std::vector<Eigen::Triplet<T, signedIndexType>> pivotedLowerTriplets;
    std::vector<Eigen::Triplet<T, signedIndexType>> originalLowerTriplets;
    std::vector<Eigen::Triplet<T, signedIndexType>> upperTriplets;
    for (signedIndexType row = 0; row < n; ++row) {
        signedIndexType originalRow = permutation[static_cast<size_t>(row)];
        for (const auto& entry : lowerL[static_cast<size_t>(row)]) {
            pivotedLowerTriplets.emplace_back(row, entry.first, entry.second);
            originalLowerTriplets.emplace_back(originalRow, entry.first, entry.second);
        }
        for (const auto& entry : upperU[static_cast<size_t>(row)]) {
            upperTriplets.emplace_back(row, entry.first, entry.second);
        }
    }
    SparseAdvancedMatrix<T> pivotedL(n, n);
    SparseAdvancedMatrix<T> originalL(n, n);
    SparseAdvancedMatrix<T> U(n, n);
    pivotedL.setFromTriplets(pivotedLowerTriplets.begin(), pivotedLowerTriplets.end());
    originalL.setFromTriplets(originalLowerTriplets.begin(), originalLowerTriplets.end());
    U.setFromTriplets(upperTriplets.begin(), upperTriplets.end());
    IluFactors<T> result;
    result.lower = originalL;
    result.pivotedLower = pivotedL;
    result.upper = U;
    result.rowPermutation = permutation;
    return result;
}
//=============================================================================
SparseAdvancedMatrix<double>
permutationToSparseMatrix(const std::vector<signedIndexType>& permutation)
{
    const signedIndexType n = static_cast<signedIndexType>(permutation.size());
    std::vector<Eigen::Triplet<double, signedIndexType>> triplets;
    triplets.reserve(permutation.size());
    for (signedIndexType row = 0; row < n; ++row) {
        triplets.emplace_back(row, permutation[static_cast<size_t>(row)], 1.);
    }
    SparseAdvancedMatrix<double> P(n, n);
    P.setFromTriplets(triplets.begin(), triplets.end());
    return P;
}
//=============================================================================
template <class T>
SparseAdvancedMatrix<T>
combinedIluFactor(const SparseAdvancedMatrix<T>& lower, const SparseAdvancedMatrix<T>& upper)
{
    SparseAdvancedMatrix<T> result = upper;
    const signedIndexType diagonalSize = std::min(lower.rows(), lower.cols());
    for (signedIndexType col = 0; col < lower.outerSize(); ++col) {
        for (typename SparseAdvancedMatrix<T>::InnerIterator it(lower, col); it; ++it) {
            if (it.row() > it.col()) {
                result.coeffRef(it.row(), it.col()) = it.value();
            }
        }
    }
    for (signedIndexType k = 0; k < diagonalSize; ++k) {
        if (upper.coeff(k, k) == T(0)) {
            result.coeffRef(k, k) = T(0);
        }
    }
    result.prune(T(0));
    result.makeCompressed();
    return result;
}
//=============================================================================
template <class T>
ArrayOfVector
iluTyped(int nLhs, const ArrayOfVector& argIn, NelsonType dataClass)
{
    ArrayOf Aarg = argIn[0];
    if (!Aarg.isSparse() || Aarg.dataClass() != dataClass) {
        OverloadRequired("ilu");
    }
    if (!Aarg.isSquare()) {
        Error(L"Nelson:linear_algebra:squareMatrixExpected", L"%s expected.", L"Square matrix");
    }
    IluOptions options = parseIluOptions(argIn);
    SparseAdvancedMatrix<T> A = arrayToSparseMatrix<T>(Aarg);
    IluFactors<T> factors = options.nofill ? nofillIlu<T>(A, 0.) : ilutpIlu<T>(A, options);
    ArrayOfVector retval;
    if (nLhs <= 1) {
        SparseAdvancedMatrix<T> combined
            = combinedIluFactor<T>(factors.pivotedLower, factors.upper);
        retval << sparseMatrixToArrayOf<T>(dataClass, Aarg.rows(), Aarg.columns(), combined);
        return retval;
    }
    const SparseAdvancedMatrix<T>& lower = nLhs > 2 ? factors.pivotedLower : factors.lower;
    retval << sparseMatrixToArrayOf<T>(dataClass, Aarg.rows(), Aarg.columns(), lower);
    retval << sparseMatrixToArrayOf<T>(dataClass, Aarg.rows(), Aarg.columns(), factors.upper);
    if (nLhs > 2) {
        SparseAdvancedMatrix<double> P = permutationToSparseMatrix(factors.rowPermutation);
        retval << sparseMatrixToArrayOf<double>(NLS_DOUBLE, Aarg.rows(), Aarg.columns(), P);
    }
    return retval;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::iluBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 3);
    if (!argIn[0].isSparse() || !isFloatingPointClass(argIn[0].dataClass())) {
        OverloadRequired("ilu");
    }
    if (argIn[0].dataClass() == NLS_SCOMPLEX) {
        return iluTyped<singlecomplex>(nLhs, argIn, NLS_SCOMPLEX);
    }
    if (argIn[0].dataClass() == NLS_DOUBLE) {
        return iluTyped<double>(nLhs, argIn, NLS_DOUBLE);
    }
    if (argIn[0].dataClass() == NLS_DCOMPLEX) {
        return iluTyped<doublecomplex>(nLhs, argIn, NLS_DCOMPLEX);
    }
    if (argIn[0].dataClass() == NLS_SINGLE) {
        return iluTyped<single>(nLhs, argIn, NLS_SINGLE);
    }
    OverloadRequired("ilu");
    return {};
}
//=============================================================================
