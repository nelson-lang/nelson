//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ArrayOf_AssignFastPath.hpp"
#include <cstring>
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isPlainDenseIndexedAssignClass(NelsonType type)
{
    switch (type) {
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
        return true;
    default:
        return false;
    }
}
//=============================================================================
bool
trySetDenseVectorSubsetDirect(ArrayOf& destination, const ArrayOf& rightData, constIndexPtr index,
    indexType length, int advance)
{
    if (destination.isSparse() || rightData.isSparse()
        || destination.dataClass() != rightData.dataClass()
        || !isPlainDenseIndexedAssignClass(destination.dataClass())) {
        return false;
    }
    const indexType elementSize = destination.elementSize();
    if (elementSize <= 0) {
        return false;
    }
    char* target = static_cast<char*>(destination.readWriteDataPointer());
    const char* source = static_cast<const char*>(rightData.dataPointer());
    indexType sourceIndex = 0;
    for (indexType i = 0; i < length; ++i) {
        std::memcpy(
            target + (index[i] - 1) * elementSize, source + sourceIndex * elementSize, elementSize);
        sourceIndex += advance;
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
