//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__upfirdn__Builtin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <complex>
//=============================================================================
namespace Nelson::SignalProcessingGateway {
//=============================================================================
static Dimensions
outputDimensions(const Dimensions& inputDimensions, indexType outputLength, indexType dim);
static Dimensions
outputDimensions(const Dimensions& inputDimensions, indexType outputLength, indexType outputSignals,
    indexType dim);
static indexType
upfirdnOutputLength(indexType inputLength, indexType filterLength, indexType p, indexType q);
static void
validateFilterColumns(indexType signalCount, indexType hColumns);
template <class T>
static T
polyphaseSample(const T* x, indexType xStride, indexType inputLength, const T* h,
    indexType filterLength, indexType p, indexType upsampledIndex);
template <class T>
static void
upfirdnDim1(const T* x, const T* h, T* y, indexType rows, indexType cols, indexType filterLength,
    indexType hColumns, indexType outputCols, indexType p, indexType q, indexType outputRows);
template <class T>
static void
upfirdnDim2(const T* x, const T* h, T* y, indexType rows, indexType cols, indexType filterLength,
    indexType hColumns, indexType outputRows, indexType p, indexType q, indexType outputCols);
template <class T>
static ArrayOf
upfirdnOfClass(
    ArrayOf& x, ArrayOf& h, NelsonType destinationClass, indexType p, indexType q, indexType dim);
//=============================================================================
ArrayOfVector
__upfirdn__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 5);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf x = argIn[0];
    ArrayOf h = argIn[1];
    indexType p = argIn[2].asScalarIndex();
    indexType q = argIn[3].asScalarIndex();
    indexType dim = argIn[4].asScalarIndex() - 1;

    if (!x.isNumeric() || !h.isNumeric()) {
        Error(L"Nelson:types:numericExpected", L"Input arguments must be numeric.");
    }
    if (x.isSparse() || h.isSparse()) {
        Error(L"Nelson:upfirdn:sparseNotSupported", L"Sparse inputs are not supported.");
    }
    if (h.isEmpty()) {
        Error(L"Nelson:validators:mustBeNonempty", L"%s must be nonempty.", L"Filter coefficients");
    }
    if (dim > 1) {
        Error(L"Nelson:signal:dimensionMustBe1Or2", L"Dimension must be 1 or 2.");
    }
    ArrayOfVector retval;
    if (x.isComplex() || h.isComplex()) {
        retval << upfirdnOfClass<std::complex<double>>(x, h, NLS_DCOMPLEX, p, q, dim);
    } else {
        retval << upfirdnOfClass<double>(x, h, NLS_DOUBLE, p, q, dim);
    }
    return retval;
}
//=============================================================================
static Dimensions
outputDimensions(const Dimensions& inputDimensions, indexType outputLength, indexType dim)
{
    Dimensions dims(inputDimensions);
    dims[dim] = outputLength;
    dims.simplify();
    return dims;
}
//=============================================================================
static Dimensions
outputDimensions(const Dimensions& inputDimensions, indexType outputLength, indexType outputSignals,
    indexType dim)
{
    Dimensions dims = outputDimensions(inputDimensions, outputLength, dim);
    dims[dim == 0 ? 1 : 0] = outputSignals;
    dims.simplify();
    return dims;
}
//=============================================================================
static indexType
upfirdnOutputLength(indexType inputLength, indexType filterLength, indexType p, indexType q)
{
    if (inputLength == 0 || filterLength == 0) {
        return 0;
    }
    indexType fullLength = (inputLength - 1) * p + filterLength;
    return ((fullLength - 1) / q) + 1;
}
//=============================================================================
static void
validateFilterColumns(indexType signalCount, indexType hColumns)
{
    if (hColumns != 1 && signalCount != 1 && hColumns != signalCount) {
        Error(L"Nelson:dimagree:signalXFilterHNumberColumns",
            L"Signal X and filter H must have the same number of columns.");
    }
}
//=============================================================================
// Value of the full convolution of the upsampled signal with h at the given
// upsampled index. The upsampled signal is zero except every p-th sample, so
// only the taps congruent to that index modulo p meet an input sample: they
// are visited in increasing tap order, as a tap by tap convolution would.
template <class T>
static T
polyphaseSample(const T* x, indexType xStride, indexType inputLength, const T* h,
    indexType filterLength, indexType p, indexType upsampledIndex)
{
    T value {};
    indexType firstTap = upsampledIndex % p;
    if (firstTap >= filterLength) {
        return value;
    }
    indexType lastInput = upsampledIndex / p;
    indexType firstShift = (lastInput >= inputLength) ? (lastInput - inputLength + 1) : 0;
    indexType lastShift = std::min((filterLength - 1 - firstTap) / p, lastInput);
    const T* tap = h + firstTap + firstShift * p;
    const T* sample = x + (lastInput - firstShift) * xStride;
    for (indexType shift = firstShift; shift <= lastShift; shift++) {
        value += *tap * *sample;
        tap += p;
        sample -= xStride;
    }
    return value;
}
//=============================================================================
template <class T>
static void
upfirdnDim1(const T* x, const T* h, T* y, indexType rows, indexType cols, indexType filterLength,
    indexType hColumns, indexType outputCols, indexType p, indexType q, indexType outputRows)
{
    for (indexType col = 0; col < outputCols; col++) {
        const T* xColumn = x + (cols == 1 ? 0 : col) * rows;
        const T* hColumn = h + (hColumns == 1 ? 0 : col) * filterLength;
        T* yColumn = y + col * outputRows;
        for (indexType outRow = 0; outRow < outputRows; outRow++) {
            yColumn[outRow]
                = polyphaseSample<T>(xColumn, 1, rows, hColumn, filterLength, p, outRow * q);
        }
    }
}
//=============================================================================
template <class T>
static void
upfirdnDim2(const T* x, const T* h, T* y, indexType rows, indexType cols, indexType filterLength,
    indexType hColumns, indexType outputRows, indexType p, indexType q, indexType outputCols)
{
    for (indexType row = 0; row < outputRows; row++) {
        const T* xRow = x + (rows == 1 ? 0 : row);
        const T* hRow = h + (hColumns == 1 ? 0 : row) * filterLength;
        for (indexType outCol = 0; outCol < outputCols; outCol++) {
            y[outCol * outputRows + row]
                = polyphaseSample<T>(xRow, rows, cols, hRow, filterLength, p, outCol * q);
        }
    }
}
//=============================================================================
template <class T>
static ArrayOf
upfirdnOfClass(
    ArrayOf& x, ArrayOf& h, NelsonType destinationClass, indexType p, indexType q, indexType dim)
{
    x.promoteType(destinationClass);
    h.promoteType(destinationClass);
    indexType rows = x.rows();
    indexType cols = x.columns();
    indexType filterLength = h.isVector() ? h.elementCount() : h.rows();
    indexType hColumns = h.isVector() ? 1 : h.columns();
    indexType signalCount = dim == 0 ? cols : rows;
    validateFilterColumns(signalCount, hColumns);
    indexType outputSignals = std::max(signalCount, hColumns);
    indexType inputLength = dim == 0 ? rows : cols;
    indexType outputLength = upfirdnOutputLength(inputLength, filterLength, p, q);
    Dimensions dims = outputDimensions(x.dimensions(), outputLength, outputSignals, dim);
    void* y
        = ArrayOf::allocateArrayOf(destinationClass, dims.elementCount(), stringVector(), false);
    const T* xData = static_cast<const T*>(x.dataPointer());
    const T* hData = static_cast<const T*>(h.dataPointer());
    T* yData = static_cast<T*>(y);
    if (dim == 0) {
        upfirdnDim1<T>(xData, hData, yData, rows, cols, filterLength, hColumns, outputSignals, p, q,
            outputLength);
    } else {
        upfirdnDim2<T>(xData, hData, yData, rows, cols, filterLength, hColumns, outputSignals, p, q,
            outputLength);
    }
    return ArrayOf(destinationClass, dims, y);
}
//=============================================================================
} // namespace Nelson::SignalProcessingGateway
//=============================================================================
