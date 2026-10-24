//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "PageTranspose.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isSupportedType(const ArrayOf& X)
{
    switch (X.dataClass()) {
    case NLS_LOGICAL:
    case NLS_CHAR:
    case NLS_INT8:
    case NLS_UINT8:
    case NLS_INT16:
    case NLS_UINT16:
    case NLS_INT32:
    case NLS_UINT32:
    case NLS_INT64:
    case NLS_UINT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
        return !X.isSparse();
    default:
        return false;
    }
}
//=============================================================================
ArrayOf
PageTranspose(const ArrayOf& X, bool& needToOverload)
{
    needToOverload = false;
    if (!isSupportedType(X)) {
        needToOverload = true;
        return {};
    }
    Dimensions dims = X.dimensions();
    indexType r = dims.at(0, false);
    indexType c = dims.at(1, false);
    indexType pages = 1;
    for (indexType i = 2; i < (indexType)dims.length(); ++i) {
        pages *= dims.at(i, false);
    }
    Dimensions retDims;
    retDims[0] = c;
    retDims[1] = r;
    for (indexType i = 2; i < (indexType)dims.length(); ++i) {
        retDims[i] = dims.at(i, false);
    }
    retDims.simplify();

    void* out = ArrayOf::allocateArrayOf(X.dataClass(), retDims.elementCount());
    ArrayOf res(X.dataClass(), retDims, out);
    if (X.isEmpty()) {
        return res;
    }
    const char* src = (const char*)X.dataPointer();
    char* dst = (char*)out;
    size_t es = (size_t)X.elementSize();
    indexType pageElems = r * c;
    OMP_PARALLEL_FOR_LOOP(pages)
    for (ompIndexType p = 0; p < (ompIndexType)pages; ++p) {
        const char* baseS = src + (indexType)p * pageElems * es;
        char* baseD = dst + (indexType)p * pageElems * es;
        for (indexType j = 0; j < c; ++j) {
            for (indexType i = 0; i < r; ++i) {
                // src(i,j) at (i + j*r) -> dst(j,i) at (j + i*c)
                std::memcpy(baseD + (j + i * c) * es, baseS + (i + j * r) * es, es);
            }
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
