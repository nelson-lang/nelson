//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "rootsBuiltin.hpp"
#include "EigenDecomposition.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cmath>
#include <complex>
#include <cstring>
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
template <typename T>
static bool
isZero(T value)
{
    return value == T(0);
}
//=============================================================================
template <typename T>
static bool
isZero(std::complex<T> value)
{
    return value.real() == T(0) && value.imag() == T(0);
}
//=============================================================================
template <typename T>
static bool
isFinite(T value)
{
    return std::isfinite(static_cast<double>(value));
}
//=============================================================================
template <typename T>
static bool
isFinite(std::complex<T> value)
{
    return std::isfinite(static_cast<double>(value.real()))
        && std::isfinite(static_cast<double>(value.imag()));
}
//=============================================================================
template <typename T>
static bool
tailHasInfiniteRatio(const std::vector<T>& coefficients)
{
    const T lead = coefficients.front();
    for (size_t k = 1; k < coefficients.size(); ++k) {
        if (!isFinite(coefficients[k] / lead)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
template <typename T>
static std::vector<T>
activeCoefficients(const T* data, indexType count, indexType& trailingZeros)
{
    bool found = false;
    indexType first = 0;
    indexType last = 0;
    for (indexType k = 0; k < count; ++k) {
        if (!isZero(data[k])) {
            if (!found) {
                first = k;
                found = true;
            }
            last = k;
        }
    }
    if (!found) {
        trailingZeros = 0;
        return {};
    }

    trailingZeros = count - last - 1;
    std::vector<T> coefficients(data + first, data + last + 1);
    while (coefficients.size() > 1 && tailHasInfiniteRatio(coefficients)) {
        coefficients.erase(coefficients.begin());
    }
    return coefficients;
}
//=============================================================================
template <typename T>
static ArrayOf
emptyColumn(NelsonType dataClass, indexType rows)
{
    void* data = ArrayOf::allocateArrayOf(dataClass, rows, stringVector(), true);
    return ArrayOf(dataClass, Dimensions(rows, 1), data);
}
//=============================================================================
template <typename T>
static ArrayOf
companionMatrix(NelsonType dataClass, const std::vector<T>& coefficients)
{
    const indexType n = static_cast<indexType>(coefficients.size() - 1);
    T* data = static_cast<T*>(ArrayOf::allocateArrayOf(dataClass, n * n, stringVector(), true));
    for (indexType k = 0; k < n - 1; ++k) {
        data[(k + 1) + n * k] = T(1);
    }
    const T lead = coefficients.front();
    for (indexType k = 0; k < n; ++k) {
        data[n * k] = -(coefficients[k + 1] / lead);
    }
    return ArrayOf(dataClass, Dimensions(n, n), data);
}
//=============================================================================
template <typename T>
static ArrayOf
prependZeros(NelsonType dataClass, indexType zeroCount, const ArrayOf& values)
{
    if (zeroCount == 0) {
        return values;
    }
    const indexType valueCount = values.elementCount();
    const indexType outputCount = zeroCount + valueCount;
    T* data
        = static_cast<T*>(ArrayOf::allocateArrayOf(dataClass, outputCount, stringVector(), true));
    const T* source = static_cast<const T*>(values.dataPointer());
    std::copy(source, source + valueCount, data + zeroCount);
    return ArrayOf(dataClass, Dimensions(outputCount, 1), data);
}
//=============================================================================
template <typename T>
static ArrayOf
rootsFloating(const ArrayOf& input, NelsonType dataClass)
{
    const auto* data = static_cast<const T*>(input.dataPointer());
    const indexType coefficientCount = input.elementCount();
    for (indexType k = 0; k < coefficientCount; ++k) {
        if (!isFinite(data[k])) {
            Error(L"Nelson:roots:inputMustBeFinite",
                L"Input to roots function must not contain NaN or Inf.");
        }
    }

    indexType trailingZeros = 0;
    std::vector<T> coefficients = activeCoefficients(data, coefficientCount, trailingZeros);
    if (coefficients.size() <= 1) {
        return emptyColumn<T>(dataClass, trailingZeros);
    }

    ArrayOf companion = companionMatrix(dataClass, coefficients);
    ArrayOf eigenvalues;
    bool needToOverload = false;
    bool success = false;
    std::wstring errorMessage;
    success = EigenDecompositionCompactGeneral(
        companion, true, eigenvalues, needToOverload, errorMessage);
    if (needToOverload) {
        OverloadRequired("eig");
    }
    if (!success) {
        Error(L"Nelson:roots:computationFailed", errorMessage);
    }
    if (eigenvalues.allReal()) {
        if (eigenvalues.dataClass() == NLS_DCOMPLEX) {
            eigenvalues.promoteType(NLS_DOUBLE);
        } else if (eigenvalues.dataClass() == NLS_SCOMPLEX) {
            eigenvalues.promoteType(NLS_SINGLE);
        }
    }

    switch (eigenvalues.dataClass()) {
    case NLS_DOUBLE:
        return prependZeros<double>(NLS_DOUBLE, trailingZeros, eigenvalues);
    case NLS_SINGLE:
        return prependZeros<single>(NLS_SINGLE, trailingZeros, eigenvalues);
    case NLS_DCOMPLEX:
        return prependZeros<std::complex<double>>(NLS_DCOMPLEX, trailingZeros, eigenvalues);
    case NLS_SCOMPLEX:
        return prependZeros<std::complex<single>>(NLS_SCOMPLEX, trailingZeros, eigenvalues);
    default:
        return eigenvalues;
    }
}
//=============================================================================
template <typename T>
static ArrayOf
zeroOnlyRoots(const ArrayOf& input, NelsonType dataClass)
{
    const auto* data = static_cast<const T*>(input.dataPointer());
    const indexType coefficientCount = input.elementCount();
    for (indexType k = 0; k < coefficientCount; ++k) {
        if (!isFinite(data[k])) {
            Error(L"Nelson:roots:inputMustBeFinite",
                L"Input to roots function must not contain NaN or Inf.");
        }
    }

    indexType trailingZeros = 0;
    std::vector<T> coefficients = activeCoefficients(data, coefficientCount, trailingZeros);
    if (coefficients.size() > 1) {
        Error(L"Nelson:roots:eigFailed",
            L"Check for incorrect argument data type or missing argument in call to function "
            L"'eig'.");
    }
    return emptyColumn<T>(dataClass, trailingZeros);
}
//=============================================================================
ArrayOfVector
Nelson::PolynomialFunctionsGateway::rootsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);

    ArrayOf input(argIn[0]);
    if (input.isEmpty()) {
        return ArrayOfVector { input };
    }
    if (!input.isVector()) {
        Error(L"Nelson:roots:inputMustBeVector", L"Input must be a vector.");
    }
    if (!input.isNumeric()) {
        Error(L"Nelson:roots:inputMustBeVector", L"Input must be a vector.");
    }

    ArrayOfVector retval;
    switch (input.dataClass()) {
    case NLS_DOUBLE:
        retval << rootsFloating<double>(input, NLS_DOUBLE);
        break;
    case NLS_SINGLE:
        retval << rootsFloating<single>(input, NLS_SINGLE);
        break;
    case NLS_DCOMPLEX:
        retval << rootsFloating<std::complex<double>>(input, NLS_DCOMPLEX);
        break;
    case NLS_SCOMPLEX:
        retval << rootsFloating<std::complex<single>>(input, NLS_SCOMPLEX);
        break;
    case NLS_INT8:
        retval << zeroOnlyRoots<int8>(input, NLS_INT8);
        break;
    case NLS_UINT8:
        retval << zeroOnlyRoots<uint8>(input, NLS_UINT8);
        break;
    case NLS_INT16:
        retval << zeroOnlyRoots<int16>(input, NLS_INT16);
        break;
    case NLS_UINT16:
        retval << zeroOnlyRoots<uint16>(input, NLS_UINT16);
        break;
    case NLS_INT32:
        retval << zeroOnlyRoots<int32>(input, NLS_INT32);
        break;
    case NLS_UINT32:
        retval << zeroOnlyRoots<uint32>(input, NLS_UINT32);
        break;
    case NLS_INT64:
        retval << zeroOnlyRoots<int64>(input, NLS_INT64);
        break;
    case NLS_UINT64:
        retval << zeroOnlyRoots<uint64>(input, NLS_UINT64);
        break;
    case NLS_LOGICAL:
        retval << zeroOnlyRoots<logical>(input, NLS_LOGICAL);
        break;
    default:
        Error(L"Nelson:roots:inputMustBeVector", L"Input must be a vector.");
    }
    return retval;
}
//=============================================================================
