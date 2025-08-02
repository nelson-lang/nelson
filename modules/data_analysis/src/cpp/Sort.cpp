//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <complex>
#include "Sort.hpp"
#include "SortHelpers.hpp"
#include "SortIntegerHelpers.hpp"
#include "SortCellHelpers.hpp"
#include "SortRealHelpers.hpp"
#include "SortComplexHelpers.hpp"
#include "SortStringHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOfVector
Sort(const ArrayOf& arrayIn, size_t nargin, bool withIndex, indexType dim, bool ascend,
    MISSING_PLACEMENT placement, COMPARISON_METHOD comparisonMethod, bool& needToOverload)
{
    ArrayOfVector res;
    needToOverload = false;
    if (arrayIn.isClassType()) {
        needToOverload = true;
        return res;
    }
    if (arrayIn.isEmpty()) {
        switch (arrayIn.getDataClass()) {
        case NLS_CELL_ARRAY: {
            if (nargin != 1) {
                Error(_W("Only one input parameter is supported for cell arrays."));
            }
            res.push_back(ArrayOf::emptyConstructor(arrayIn.getDimensions()));
        } break;
        case NLS_STRING_ARRAY:
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
        case NLS_CHAR: {
            res.push_back(ArrayOf::emptyConstructor(arrayIn.getDimensions()));
            if (withIndex) {
                res.push_back(ArrayOf::emptyConstructor(arrayIn.getDimensions()));
            }
        } break;
        case NLS_HANDLE:
        case NLS_STRUCT_ARRAY:
        case NLS_CLASS_ARRAY:
        case NLS_FUNCTION_HANDLE:
        default: {
            needToOverload = true;
            return res;
        } break;
        }
    } else {
        indexType dimOperate;
        Dimensions inDim(arrayIn.getDimensions());
        if (dim == 0) {
            indexType lenInDim = inDim.getLength();
            indexType d = 0;
            while (d < lenInDim && inDim.getAt(d) == 1) {
                d++;
            }
            dimOperate = d;
        } else {
            dimOperate = dim - 1;
        }
        Dimensions outDim(inDim);
        indexType linesize = inDim[dimOperate];
        indexType planesize = 1;
        for (indexType d = 0; d < dimOperate; d++) {
            planesize *= inDim[d];
        }
        indexType planecount = 1;
        for (indexType d = dimOperate + 1; d < inDim.getLength(); d++) {
            planecount *= inDim[d];
        }
        switch (arrayIn.getDataClass()) {
        case NLS_HANDLE: {
            needToOverload = true;
            return res;
        } break;
        case NLS_CELL_ARRAY: {
            if (nargin != 1) {
                Error(_W("Only one input parameter is supported for cell arrays."));
            }
            res = sortCell(
                arrayIn, withIndex, linesize, planecount, planesize, outDim, dim, needToOverload);
        } break;
        case NLS_CLASS_ARRAY:
        case NLS_FUNCTION_HANDLE:
        case NLS_STRUCT_ARRAY: {
            needToOverload = true;
            return res;
        } break;
        case NLS_STRING_ARRAY: {
            res = sortString(arrayIn, withIndex, linesize, planecount, planesize, outDim, dim,
                placement, ascend);
        } break;
        case NLS_LOGICAL: {
            res = sortInteger<logical>(arrayIn, NLS_UINT8, withIndex, linesize, planecount,
                planesize, outDim, dim, ascend);
        } break;
        case NLS_UINT8: {
            res = sortInteger<uint8>(arrayIn, NLS_UINT8, withIndex, linesize, planecount, planesize,
                outDim, dim, ascend);
        } break;
        case NLS_INT8: {
            res = sortInteger<int8>(
                arrayIn, NLS_INT8, withIndex, linesize, planecount, planesize, outDim, dim, ascend);
        } break;
        case NLS_UINT16: {
            res = sortInteger<uint16>(arrayIn, NLS_UINT16, withIndex, linesize, planecount,
                planesize, outDim, dim, ascend);
        } break;
        case NLS_INT16: {
            res = sortInteger<int16>(arrayIn, NLS_INT16, withIndex, linesize, planecount, planesize,
                outDim, dim, ascend);
        } break;
        case NLS_UINT32: {
            res = sortInteger<uint32>(arrayIn, NLS_UINT32, withIndex, linesize, planecount,
                planesize, outDim, dim, ascend);
        } break;
        case NLS_INT32: {
            res = sortInteger<int32>(arrayIn, NLS_INT32, withIndex, linesize, planecount, planesize,
                outDim, dim, ascend);
        } break;
        case NLS_UINT64: {
            res = sortInteger<uint64>(arrayIn, NLS_UINT64, withIndex, linesize, planecount,
                planesize, outDim, dim, ascend);
        } break;
        case NLS_INT64: {
            res = sortInteger<int64>(arrayIn, NLS_INT64, withIndex, linesize, planecount, planesize,
                outDim, dim, ascend);
        } break;
        case NLS_SINGLE: {
            res = sortReal<single>(arrayIn, NLS_SINGLE, withIndex, linesize, planecount, planesize,
                outDim, dim, ascend, placement);
        } break;
        case NLS_DOUBLE: {
            res = sortReal<double>(arrayIn, NLS_DOUBLE, withIndex, linesize, planecount, planesize,
                outDim, dim, ascend, placement);
        } break;
        case NLS_SCOMPLEX: {
            res = sortComplex<single>(arrayIn, NLS_SCOMPLEX, withIndex, linesize, planecount,
                planesize, outDim, dim, ascend, placement, comparisonMethod);
        } break;
        case NLS_DCOMPLEX: {
            res = sortComplex<double>(arrayIn, NLS_DCOMPLEX, withIndex, linesize, planecount,
                planesize, outDim, dim, ascend, placement, comparisonMethod);
        } break;
        case NLS_CHAR: {
            res = sortInteger<charType>(
                arrayIn, NLS_CHAR, withIndex, linesize, planecount, planesize, outDim, dim, ascend);
        } break;
        default: {
            needToOverload = true;
            return res;
        } break;
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
