//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__filter__Builtin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
#include <complex>
#include <vector>
//=============================================================================
namespace Nelson::ElementaryFunctionsGateway {
//=============================================================================
static Dimensions
extendedDimensions(const Dimensions& inputDimensions, indexType dim)
{
    indexType length = std::max<indexType>(2, inputDimensions.length());
    length = std::max<indexType>(length, dim + 1);
    Dimensions dims(length);
    for (indexType k = 0; k < length; k++) {
        dims[k] = inputDimensions.dimensionLength(k);
    }
    return dims;
}
//=============================================================================
static std::vector<indexType>
stridesFromDimensions(const Dimensions& dims)
{
    std::vector<indexType> strides(dims.length(), 1);
    for (indexType k = 1; k < dims.length(); k++) {
        strides[k] = strides[k - 1] * dims.dimensionLength(k - 1);
    }
    return strides;
}
//=============================================================================
static std::vector<indexType>
remainingDimensions(indexType length, indexType dim)
{
    std::vector<indexType> remaining;
    remaining.reserve(length - 1);
    for (indexType k = 0; k < length; k++) {
        if (k != dim) {
            remaining.push_back(k);
        }
    }
    return remaining;
}
//=============================================================================
static indexType
signalBaseOffset(indexType signalIndex, const Dimensions& dims,
    const std::vector<indexType>& strides, const std::vector<indexType>& remaining)
{
    indexType base = 0;
    indexType value = signalIndex;
    for (indexType dim : remaining) {
        indexType dimLength = dims.dimensionLength(dim);
        indexType coordinate = value % dimLength;
        value /= dimLength;
        base += coordinate * strides[dim];
    }
    return base;
}
//=============================================================================
static Dimensions
stateDimensions(indexType filterOrder, const Dimensions& dims, indexType dim)
{
    Dimensions stateDims(dims.length());
    stateDims[0] = filterOrder;
    indexType outDim = 1;
    for (indexType k = 0; k < dims.length(); k++) {
        if (k != dim) {
            stateDims[outDim++] = dims.dimensionLength(k);
        }
    }
    stateDims.simplify();
    return stateDims;
}
//=============================================================================
static void
validateInitialState(const ArrayOf& zi, indexType filterOrder, indexType numberOfSignals)
{
    if (filterOrder == 0 || zi.isEmpty()) {
        return;
    }
    if (zi.isVector() && numberOfSignals == 1) {
        if (zi.elementCount() != filterOrder) {
            Error(L"Nelson:elementary_functions:initialConditionsIncompatibleSize",
                L"Initial conditions have incompatible size.");
        }
        return;
    }
    if (zi.rows() != filterOrder || zi.elementCount() != filterOrder * numberOfSignals) {
        Error(L"Nelson:elementary_functions:initialConditionsIncompatibleSize",
            L"Initial conditions have incompatible size.");
    }
}
//=============================================================================
static void
filterReal(const double* b, const double* a, const double* x, double* y, double* state,
    indexType filterOrder, indexType sampleCount, indexType numberOfSignals, indexType sampleStride,
    const Dimensions& dims, const std::vector<indexType>& strides,
    const std::vector<indexType>& remaining)
{
    for (indexType signalIndex = 0; signalIndex < numberOfSignals; signalIndex++) {
        indexType base = signalBaseOffset(signalIndex, dims, strides, remaining);
        double* signalState = state + signalIndex * filterOrder;
        for (indexType sampleIndex = 0; sampleIndex < sampleCount; sampleIndex++) {
            indexType offset = base + sampleIndex * sampleStride;
            double sample = x[offset];
            double output = b[0] * sample;
            if (filterOrder != 0) {
                output += signalState[0];
                for (indexType stateIndex = 0; stateIndex + 1 < filterOrder; stateIndex++) {
                    signalState[stateIndex] = signalState[stateIndex + 1]
                        + b[stateIndex + 1] * sample - a[stateIndex + 1] * output;
                }
                signalState[filterOrder - 1] = b[filterOrder] * sample - a[filterOrder] * output;
            }
            y[offset] = output;
        }
    }
}
//=============================================================================
static void
filterComplex(const std::complex<double>* b, const std::complex<double>* a,
    const std::complex<double>* x, std::complex<double>* y, std::complex<double>* state,
    indexType filterOrder, indexType sampleCount, indexType numberOfSignals, indexType sampleStride,
    const Dimensions& dims, const std::vector<indexType>& strides,
    const std::vector<indexType>& remaining)
{
    for (indexType signalIndex = 0; signalIndex < numberOfSignals; signalIndex++) {
        indexType base = signalBaseOffset(signalIndex, dims, strides, remaining);
        std::complex<double>* signalState = state + signalIndex * filterOrder;
        for (indexType sampleIndex = 0; sampleIndex < sampleCount; sampleIndex++) {
            indexType offset = base + sampleIndex * sampleStride;
            std::complex<double> sample = x[offset];
            std::complex<double> output = b[0] * sample;
            if (filterOrder != 0) {
                output += signalState[0];
                for (indexType stateIndex = 0; stateIndex + 1 < filterOrder; stateIndex++) {
                    signalState[stateIndex] = signalState[stateIndex + 1]
                        + b[stateIndex + 1] * sample - a[stateIndex + 1] * output;
                }
                signalState[filterOrder - 1] = b[filterOrder] * sample - a[filterOrder] * output;
            }
            y[offset] = output;
        }
    }
}
//=============================================================================
static ArrayOfVector
filterDouble(const ArrayOf& b, const ArrayOf& a, ArrayOf& x, ArrayOf& zi, indexType dim, int nLhs)
{
    x.promoteType(NLS_DOUBLE);
    zi.promoteType(NLS_DOUBLE);
    Dimensions dims = extendedDimensions(x.dimensions(), dim);
    indexType sampleCount = dims.dimensionLength(dim);
    indexType totalCount = x.elementCount();
    indexType numberOfSignals = sampleCount == 0 ? 0 : totalCount / sampleCount;
    indexType filterOrder = b.elementCount() - 1;
    validateInitialState(zi, filterOrder, numberOfSignals);

    auto* yPtr = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DOUBLE, totalCount, stringVector(), false));
    std::vector<double> state(filterOrder * numberOfSignals, 0);
    if (filterOrder != 0 && !zi.isEmpty()) {
        auto* ziPtr = static_cast<const double*>(zi.dataPointer());
        std::copy(ziPtr, ziPtr + state.size(), state.begin());
    }
    std::vector<indexType> strides = stridesFromDimensions(dims);
    std::vector<indexType> remaining = remainingDimensions(dims.length(), dim);
    filterReal(static_cast<const double*>(b.dataPointer()),
        static_cast<const double*>(a.dataPointer()), static_cast<const double*>(x.dataPointer()),
        yPtr, state.data(), filterOrder, sampleCount, numberOfSignals, strides[dim], dims, strides,
        remaining);

    ArrayOfVector retval;
    retval << ArrayOf(NLS_DOUBLE, x.dimensions(), yPtr);
    if (nLhs > 1) {
        if (filterOrder == 0) {
            retval << ArrayOf::empty();
        } else {
            auto* zfPtr = static_cast<double*>(
                ArrayOf::allocateArrayOf(NLS_DOUBLE, state.size(), stringVector(), false));
            std::copy(state.begin(), state.end(), zfPtr);
            retval << ArrayOf(NLS_DOUBLE, stateDimensions(filterOrder, dims, dim), zfPtr);
        }
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
filterDoubleComplex(ArrayOf& b, ArrayOf& a, ArrayOf& x, ArrayOf& zi, indexType dim, int nLhs)
{
    b.promoteType(NLS_DCOMPLEX);
    a.promoteType(NLS_DCOMPLEX);
    x.promoteType(NLS_DCOMPLEX);
    zi.promoteType(NLS_DCOMPLEX);
    Dimensions dims = extendedDimensions(x.dimensions(), dim);
    indexType sampleCount = dims.dimensionLength(dim);
    indexType totalCount = x.elementCount();
    indexType numberOfSignals = sampleCount == 0 ? 0 : totalCount / sampleCount;
    indexType filterOrder = b.elementCount() - 1;
    validateInitialState(zi, filterOrder, numberOfSignals);

    auto* yPtr = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DCOMPLEX, totalCount, stringVector(), false));
    auto* yComplex = reinterpret_cast<std::complex<double>*>(yPtr);
    std::vector<std::complex<double>> state(filterOrder * numberOfSignals, { 0, 0 });
    if (filterOrder != 0 && !zi.isEmpty()) {
        auto* ziPtr = reinterpret_cast<const std::complex<double>*>(
            static_cast<const double*>(zi.dataPointer()));
        std::copy(ziPtr, ziPtr + state.size(), state.begin());
    }
    std::vector<indexType> strides = stridesFromDimensions(dims);
    std::vector<indexType> remaining = remainingDimensions(dims.length(), dim);
    filterComplex(
        reinterpret_cast<const std::complex<double>*>(static_cast<const double*>(b.dataPointer())),
        reinterpret_cast<const std::complex<double>*>(static_cast<const double*>(a.dataPointer())),
        reinterpret_cast<const std::complex<double>*>(static_cast<const double*>(x.dataPointer())),
        yComplex, state.data(), filterOrder, sampleCount, numberOfSignals, strides[dim], dims,
        strides, remaining);

    ArrayOfVector retval;
    retval << ArrayOf(NLS_DCOMPLEX, x.dimensions(), yPtr);
    if (nLhs > 1) {
        if (filterOrder == 0) {
            retval << ArrayOf::empty();
        } else {
            auto* zfPtr = static_cast<double*>(
                ArrayOf::allocateArrayOf(NLS_DCOMPLEX, state.size(), stringVector(), false));
            auto* zfComplex = reinterpret_cast<std::complex<double>*>(zfPtr);
            std::copy(state.begin(), state.end(), zfComplex);
            retval << ArrayOf(NLS_DCOMPLEX, stateDimensions(filterOrder, dims, dim), zfPtr);
        }
    }
    return retval;
}
//=============================================================================
ArrayOfVector
__filter__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 5);
    nargoutcheck(nLhs, 0, 2);
    ArrayOf b = argIn[0];
    ArrayOf a = argIn[1];
    ArrayOf x = argIn[2];
    ArrayOf zi = argIn[3];
    indexType dim = argIn[4].asScalarIndex() - 1;

    if (!b.isNumeric() || !a.isNumeric() || !x.isNumeric() || (!zi.isEmpty() && !zi.isNumeric())) {
        Error(L"Nelson:validators:mustBeNumeric", L"%s must be numeric.", L"Input arguments");
    }
    if (b.isSparse() || a.isSparse() || x.isSparse() || zi.isSparse()) {
        Error(L"Nelson:elementary_functions:sparseInputsSupported",
            L"Sparse inputs are not supported.");
    }
    if (b.isEmpty() || a.isEmpty() || b.elementCount() != a.elementCount()) {
        Error(L"Nelson:elementary_functions:filterCoefficientsEmpty",
            L"Filter coefficients must not be empty.");
    }

    bool useComplex = b.isComplex() || a.isComplex() || x.isComplex() || zi.isComplex();
    if (useComplex) {
        return filterDoubleComplex(b, a, x, zi, dim, nLhs);
    }
    b.promoteType(NLS_DOUBLE);
    a.promoteType(NLS_DOUBLE);
    return filterDouble(b, a, x, zi, dim, nLhs);
}
//=============================================================================
} // namespace Nelson::ElementaryFunctionsGateway
//=============================================================================
