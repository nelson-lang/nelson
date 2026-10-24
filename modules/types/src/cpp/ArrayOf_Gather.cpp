//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "Dimensions.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "Decomplexify.hpp"
#include <complex>
#include <type_traits>
//=============================================================================
namespace Nelson {
//=============================================================================
// Typed single-pass gathers used by linear indexing (vectorSubset).
// Only plain POD element classes are handled here, so class types never
// need to be propagated to the result.
//=============================================================================
template <typename T>
static void
gatherMaskedElements(const void* srcPtr, const logical* mask, indexType maskLen, void* dstPtr);
//=============================================================================
template <typename T>
static bool
gatherOrdinalElements(const void* srcPtr, indexType bound, const ArrayOf& index, void* dstPtr);
//=============================================================================
Dimensions
ArrayOf::linearSubsetDimensions(const ArrayOf& index) const
{
    Dimensions retdims(index.dimensions());
    if (!isScalar() && isVector() && index.isVector()) {
        retdims = isRowVector() ? Dimensions(1, index.elementCount())
                                : Dimensions(index.elementCount(), 1);
    }
    retdims.simplify();
    return retdims;
}
//=============================================================================
bool
ArrayOf::tryLogicalMaskGather(const ArrayOf& mask, ArrayOf& result)
{
    switch (dp->dataClass) {
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
    case NLS_CHAR:
        break;
    default:
        return false;
    }
    const auto* maskData = static_cast<const logical*>(mask.dp->data());
    const indexType maskLen = mask.elementCount();
    const indexType bound = elementCount();
    indexType selectedCount = 0;
    indexType lastSelected = 0;
    for (indexType i = 0; i < maskLen; ++i) {
        if (maskData[i] != 0) {
            ++selectedCount;
            lastSelected = i;
        }
    }
    if (selectedCount != 0 && lastSelected >= bound) {
        Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
    }
    // mirror the ordinal-index shape rules: the mask orientation drives the
    // output unless the source itself is a (non-scalar) vector
    Dimensions retdims
        = mask.isRowVector() ? Dimensions(1, selectedCount) : Dimensions(selectedCount, 1);
    if (!isScalar() && isVector() && retdims.isVector()) {
        retdims = isRowVector() ? Dimensions(1, selectedCount) : Dimensions(selectedCount, 1);
    }
    retdims.simplify();
    void* qp = allocateArrayOf(dp->dataClass, selectedCount, stringVector(), false);
    const void* src = dp->data();
    switch (dp->dataClass) {
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
        gatherMaskedElements<uint8>(src, maskData, maskLen, qp);
        break;
    case NLS_UINT16:
    case NLS_INT16:
        gatherMaskedElements<uint16>(src, maskData, maskLen, qp);
        break;
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_SINGLE:
        gatherMaskedElements<uint32>(src, maskData, maskLen, qp);
        break;
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_DOUBLE:
    case NLS_SCOMPLEX:
        gatherMaskedElements<uint64>(src, maskData, maskLen, qp);
        break;
    case NLS_DCOMPLEX:
        gatherMaskedElements<std::complex<double>>(src, maskData, maskLen, qp);
        break;
    case NLS_CHAR:
        gatherMaskedElements<charType>(src, maskData, maskLen, qp);
        break;
    default:
        break;
    }
    result = ArrayOf(dp->dataClass, retdims, qp, false);
    if (result.isComplex()) {
        result = decomplexify(result);
    }
    return true;
}
//=============================================================================
bool
ArrayOf::tryOrdinalGather(const ArrayOf& index, ArrayOf& result)
{
    switch (index.dataClass()) {
    case NLS_DOUBLE:
    case NLS_SINGLE:
    case NLS_INT8:
    case NLS_UINT8:
    case NLS_INT16:
    case NLS_UINT16:
    case NLS_INT32:
    case NLS_UINT32:
    case NLS_INT64:
    case NLS_UINT64:
        break;
    default:
        return false;
    }
    const indexType bound = elementCount();
    const void* src = dp->data();
    void* qp = nullptr;
    bool gathered = false;
    switch (dp->dataClass) {
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
        qp = allocateArrayOf(dp->dataClass, index.elementCount(), stringVector(), false);
        gathered = gatherOrdinalElements<uint8>(src, bound, index, qp);
        break;
    case NLS_UINT16:
    case NLS_INT16:
        qp = allocateArrayOf(dp->dataClass, index.elementCount(), stringVector(), false);
        gathered = gatherOrdinalElements<uint16>(src, bound, index, qp);
        break;
    case NLS_CHAR:
        qp = allocateArrayOf(dp->dataClass, index.elementCount(), stringVector(), false);
        gathered = gatherOrdinalElements<charType>(src, bound, index, qp);
        break;
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_SINGLE:
        qp = allocateArrayOf(dp->dataClass, index.elementCount(), stringVector(), false);
        gathered = gatherOrdinalElements<uint32>(src, bound, index, qp);
        break;
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_DOUBLE:
    case NLS_SCOMPLEX:
        qp = allocateArrayOf(dp->dataClass, index.elementCount(), stringVector(), false);
        gathered = gatherOrdinalElements<uint64>(src, bound, index, qp);
        break;
    case NLS_DCOMPLEX:
        qp = allocateArrayOf(dp->dataClass, index.elementCount(), stringVector(), false);
        gathered = gatherOrdinalElements<std::complex<double>>(src, bound, index, qp);
        break;
    default:
        return false;
    }
    if (!gathered) {
        // invalid index: the generic path raises the usual error
        deleteArrayOf(qp, dp->dataClass);
        return false;
    }
    result = ArrayOf(dp->dataClass, linearSubsetDimensions(index), qp, false);
    if (result.isComplex()) {
        result = decomplexify(result);
    }
    return true;
}
//=============================================================================
template <typename T>
static void
gatherMaskedElements(const void* srcPtr, const logical* mask, indexType maskLen, void* dstPtr)
{
    const T* src = static_cast<const T*>(srcPtr);
    T* dst = static_cast<T*>(dstPtr);
    for (indexType i = 0; i < maskLen; ++i) {
        if (mask[i] != 0) {
            *dst++ = src[i];
        }
    }
}
//=============================================================================
template <typename T, typename I>
static bool
gatherByIndex(const T* src, indexType bound, const I* idx, indexType count, T* dst)
{
    if constexpr (std::is_floating_point_v<I>) {
        const double upper = static_cast<double>(bound);
        for (indexType i = 0; i < count; ++i) {
            const double value = static_cast<double>(idx[i]);
            // rejects NaN too
            if (!(value >= 1.0 && value <= upper)) {
                return false;
            }
            const auto k = static_cast<indexType>(value);
            if (static_cast<double>(k) != value) {
                return false;
            }
            dst[i] = src[k - 1];
        }
    } else {
        for (indexType i = 0; i < count; ++i) {
            const I value = idx[i];
            if (value < 1 || static_cast<uint64>(value) > static_cast<uint64>(bound)) {
                return false;
            }
            dst[i] = src[static_cast<indexType>(value) - 1];
        }
    }
    return true;
}
//=============================================================================
template <typename T>
static bool
gatherOrdinalElements(const void* srcPtr, indexType bound, const ArrayOf& index, void* dstPtr)
{
    const T* src = static_cast<const T*>(srcPtr);
    T* dst = static_cast<T*>(dstPtr);
    const void* idx = index.dataPointer();
    const indexType count = index.elementCount();
    switch (index.dataClass()) {
    case NLS_DOUBLE:
        return gatherByIndex(src, bound, static_cast<const double*>(idx), count, dst);
    case NLS_SINGLE:
        return gatherByIndex(src, bound, static_cast<const single*>(idx), count, dst);
    case NLS_INT8:
        return gatherByIndex(src, bound, static_cast<const int8*>(idx), count, dst);
    case NLS_UINT8:
        return gatherByIndex(src, bound, static_cast<const uint8*>(idx), count, dst);
    case NLS_INT16:
        return gatherByIndex(src, bound, static_cast<const int16*>(idx), count, dst);
    case NLS_UINT16:
        return gatherByIndex(src, bound, static_cast<const uint16*>(idx), count, dst);
    case NLS_INT32:
        return gatherByIndex(src, bound, static_cast<const int32*>(idx), count, dst);
    case NLS_UINT32:
        return gatherByIndex(src, bound, static_cast<const uint32*>(idx), count, dst);
    case NLS_INT64:
        return gatherByIndex(src, bound, static_cast<const int64*>(idx), count, dst);
    case NLS_UINT64:
        return gatherByIndex(src, bound, static_cast<const uint64*>(idx), count, dst);
    default:
        return false;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
