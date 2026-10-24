//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <type_traits>
#include "bitgetBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
NelsonType
typeFromName(const std::wstring& name)
{
    if (name == L"int8") {
        return NLS_INT8;
    }
    if (name == L"uint8") {
        return NLS_UINT8;
    }
    if (name == L"int16") {
        return NLS_INT16;
    }
    if (name == L"uint16") {
        return NLS_UINT16;
    }
    if (name == L"int32") {
        return NLS_INT32;
    }
    if (name == L"uint32") {
        return NLS_UINT32;
    }
    if (name == L"int64") {
        return NLS_INT64;
    }
    if (name == L"uint64") {
        return NLS_UINT64;
    }
    return NLS_UNKNOWN;
}
//=============================================================================
int
typeWidth(NelsonType dataClass)
{
    switch (dataClass) {
    case NLS_INT8:
    case NLS_UINT8:
        return 8;
    case NLS_INT16:
    case NLS_UINT16:
        return 16;
    case NLS_INT32:
    case NLS_UINT32:
        return 32;
    case NLS_INT64:
    case NLS_UINT64:
    case NLS_DOUBLE:
        return 64;
    default:
        return 0;
    }
}
//=============================================================================
Dimensions
resultDimensions(const ArrayOf& A, const ArrayOf& bit)
{
    if (A.dimensions().equals(bit.dimensions())) {
        return A.dimensions();
    }
    if (A.isScalar()) {
        return bit.dimensions();
    }
    if (bit.isScalar()) {
        return A.dimensions();
    }
    Error(L"Nelson:dimagree:inputsSizeX", L"Inputs must have the same size.");
    return {};
}
//=============================================================================
void
validateBitPositions(const ArrayOf& bit, int width)
{
    (void)width;
    if (!bit.isNumeric() || bit.isComplex()) {
        Error(L"Nelson:operators:invalidBitPosition",
            L"Bit position must be a real positive integer.");
    }
    if (!bit.isIntegerType() && !bit.isIntegerValue() && !bit.isEmpty()) {
        Error(L"Nelson:operators:invalidBitPosition",
            L"Bit position must be a real positive integer.");
    }
}
//=============================================================================
template <class B>
int64
bitPosition(const B* ptr, indexType idx)
{
    return static_cast<int64>(ptr[idx]);
}
//=============================================================================
template <class T, class B>
ArrayOf
bitgetIntegerWithBit(const ArrayOf& value, const ArrayOf& bit, NelsonType outputClass,
    bool outputDouble, Dimensions dims, indexType len, int width)
{
    const T* valuePtr = static_cast<const T*>(value.dataPointer());
    const B* bitPtr = static_cast<const B*>(bit.dataPointer());
    bool valueScalar = value.isScalar();
    bool bitScalar = bit.isScalar();
    using U = typename std::make_unsigned<T>::type;
    if (outputDouble) {
        double* out = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, len));
        for (indexType k = 0; k < len; ++k) {
            U v = static_cast<U>(valuePtr[valueScalar ? 0 : k]);
            int64 position = bitPosition(bitPtr, bitScalar ? 0 : k);
            if (position < 1 || position > width) {
                Error(L"Nelson:operators:bitOutOfRange", L"Bit position out of range.");
            }
            out[k] = static_cast<double>((v >> (position - 1)) & static_cast<U>(1));
        }
        return ArrayOf(NLS_DOUBLE, dims, out, false);
    }
    T* out = static_cast<T*>(ArrayOf::allocateArrayOf(outputClass, len));
    for (indexType k = 0; k < len; ++k) {
        U v = static_cast<U>(valuePtr[valueScalar ? 0 : k]);
        int64 position = bitPosition(bitPtr, bitScalar ? 0 : k);
        if (position < 1 || position > width) {
            Error(L"Nelson:operators:bitOutOfRange", L"Bit position out of range.");
        }
        out[k] = static_cast<T>((v >> (position - 1)) & static_cast<U>(1));
    }
    return ArrayOf(outputClass, dims, out, false);
}
//=============================================================================
template <class T>
ArrayOf
bitgetIntegerByBitType(
    const ArrayOf& value, const ArrayOf& bit, NelsonType outputClass, bool outputDouble, int width)
{
    Dimensions dims = resultDimensions(value, bit);
    indexType len = dims.elementCount();
    switch (bit.dataClass()) {
    case NLS_DOUBLE:
        return bitgetIntegerWithBit<T, double>(
            value, bit, outputClass, outputDouble, dims, len, width);
    case NLS_SINGLE:
        return bitgetIntegerWithBit<T, single>(
            value, bit, outputClass, outputDouble, dims, len, width);
    case NLS_INT8:
        return bitgetIntegerWithBit<T, int8>(
            value, bit, outputClass, outputDouble, dims, len, width);
    case NLS_UINT8:
        return bitgetIntegerWithBit<T, uint8>(
            value, bit, outputClass, outputDouble, dims, len, width);
    case NLS_INT16:
        return bitgetIntegerWithBit<T, int16>(
            value, bit, outputClass, outputDouble, dims, len, width);
    case NLS_UINT16:
        return bitgetIntegerWithBit<T, uint16>(
            value, bit, outputClass, outputDouble, dims, len, width);
    case NLS_INT32:
        return bitgetIntegerWithBit<T, int32>(
            value, bit, outputClass, outputDouble, dims, len, width);
    case NLS_UINT32:
        return bitgetIntegerWithBit<T, uint32>(
            value, bit, outputClass, outputDouble, dims, len, width);
    case NLS_INT64:
        return bitgetIntegerWithBit<T, int64>(
            value, bit, outputClass, outputDouble, dims, len, width);
    case NLS_UINT64:
        return bitgetIntegerWithBit<T, uint64>(
            value, bit, outputClass, outputDouble, dims, len, width);
    default:
        Error(L"Nelson:operators:bitOutOfRange", L"Bit position out of range.");
    }
    return {};
}
//=============================================================================
template <class T>
ArrayOf
bitgetInteger(
    const ArrayOf& value, const ArrayOf& bit, NelsonType outputClass, bool outputDouble, int width)
{
    return bitgetIntegerByBitType<T>(value, bit, outputClass, outputDouble, width);
}
//=============================================================================
ArrayOf
bitgetUint32Uint32(const ArrayOf& value, const ArrayOf& bit)
{
    Dimensions dims = resultDimensions(value, bit);
    indexType len = dims.elementCount();
    const uint32* valuePtr = static_cast<const uint32*>(value.dataPointer());
    const uint32* bitPtr = static_cast<const uint32*>(bit.dataPointer());
    bool valueScalar = value.isScalar();
    bool bitScalar = bit.isScalar();
    uint32* out = static_cast<uint32*>(ArrayOf::allocateArrayOf(NLS_UINT32, len));
    if (bitScalar) {
        uint32 shift = bitPtr[0] - 1U;
        if (shift >= 32U) {
            Error(L"Nelson:operators:bitOutOfRange", L"Bit position out of range.");
        }
        for (indexType k = 0; k < len; ++k) {
            out[k] = (valuePtr[valueScalar ? 0 : k] >> shift) & 1U;
        }
    } else {
        for (indexType k = 0; k < len; ++k) {
            uint32 shift = bitPtr[k] - 1U;
            if (shift >= 32U) {
                Error(L"Nelson:operators:bitOutOfRange", L"Bit position out of range.");
            }
            out[k] = (valuePtr[valueScalar ? 0 : k] >> shift) & 1U;
        }
    }
    return ArrayOf(NLS_UINT32, dims, out, false);
}
//=============================================================================
ArrayOf
bitgetDispatch(
    const ArrayOf& value, const ArrayOf& bit, NelsonType outputClass, bool outputDouble, int width)
{
    if (!outputDouble && value.dataClass() == NLS_UINT32 && bit.dataClass() == NLS_UINT32
        && width == 32) {
        return bitgetUint32Uint32(value, bit);
    }
    switch (value.dataClass()) {
    case NLS_INT8:
        return bitgetInteger<int8>(value, bit, outputClass, outputDouble, width);
    case NLS_UINT8:
        return bitgetInteger<uint8>(value, bit, outputClass, outputDouble, width);
    case NLS_INT16:
        return bitgetInteger<int16>(value, bit, outputClass, outputDouble, width);
    case NLS_UINT16:
        return bitgetInteger<uint16>(value, bit, outputClass, outputDouble, width);
    case NLS_INT32:
        return bitgetInteger<int32>(value, bit, outputClass, outputDouble, width);
    case NLS_UINT32:
        return bitgetInteger<uint32>(value, bit, outputClass, outputDouble, width);
    case NLS_INT64:
        return bitgetInteger<int64>(value, bit, outputClass, outputDouble, width);
    case NLS_UINT64:
        return bitgetInteger<uint64>(value, bit, outputClass, outputDouble, width);
    default:
        Error(L"Nelson:operators:invalidBitInput:firstInputIntegerDoubleArray",
            L"First input must be an integer or double array.");
    }
    return {};
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::bitgetBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf value(argIn[0]);
    ArrayOf bit(argIn[1]);
    if (value.isSparse() || bit.isSparse()) {
        Error(L"Nelson:operators:sparseNotSupported", L"Inputs must be full.");
    }
    bool outputDouble = false;
    NelsonType workClass = value.dataClass();
    NelsonType outputClass = value.dataClass();
    if (argIn.size() == 3) {
        if (!value.isDoubleType(true)) {
            Error(L"Nelson:operators:assumedTypeDoubleOnly",
                L"Assumed type is valid only for double input.");
        }
        workClass = typeFromName(argIn[2].asWideString());
        if (workClass == NLS_UNKNOWN) {
            Error(L"Nelson:operators:invalidAssumedType",
                L"ASSUMEDTYPE must be an integer type name.");
        }
        outputClass = NLS_DOUBLE;
        outputDouble = true;
    } else if (value.isDoubleType(true)) {
        if (value.isComplex()) {
            Error(L"Nelson:operators:invalidBitInput", L"First input must be real.");
        }
        if (!value.isIntegerValue() && !value.isEmpty()) {
            Error(L"Nelson:operators:invalidBitInput:firstInputContainIntegerValues",
                L"First input must contain integer values.");
        }
        if (!value.isPositive()) {
            Error(L"Nelson:operators:invalidBitInput:firstInputContainNonnegativeValues",
                L"First input must contain nonnegative values.");
        }
        workClass = NLS_UINT64;
        outputClass = NLS_DOUBLE;
        outputDouble = true;
    } else if (!value.isIntegerType()) {
        Error(L"Nelson:operators:invalidBitInput:firstInputIntegerDoubleArray",
            L"First input must be an integer or double array.");
    }
    int width = typeWidth(workClass);
    validateBitPositions(bit, width);
    value.promoteType(workClass);
    ArrayOfVector retval;
    retval << bitgetDispatch(value, bit, outputClass, outputDouble, width);
    return retval;
}
//=============================================================================
