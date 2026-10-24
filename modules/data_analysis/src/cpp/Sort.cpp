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
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// sort(real, 'ComparisonMethod', 'abs'): reuse the complex sort (which orders
// by magnitude, then by phase for ties) on the real values promoted to complex,
// then take the real part back.
template <class T>
static ArrayOfVector
sortRealByAbsolute(const ArrayOf& arrayIn, NelsonType realClass, NelsonType complexClass,
    bool withIndex, indexType linesize, indexType planecount, indexType planesize,
    Dimensions& outDim, indexType dim, bool ascend, MISSING_PLACEMENT placement,
    COMPARISON_METHOD comparisonMethod)
{
    ArrayOf cplx(arrayIn);
    cplx.promoteType(complexClass);
    ArrayOfVector res = sortComplex<T>(cplx, complexClass, withIndex, linesize, planecount,
        planesize, outDim, dim, ascend, placement, comparisonMethod);
    ArrayOf& sortedComplex = res[0];
    indexType n = sortedComplex.elementCount();
    const std::complex<T>* cp = (const std::complex<T>*)sortedComplex.dataPointer();
    T* rp = (T*)ArrayOf::allocateArrayOf(realClass, n, stringVector(), false);
    for (indexType k = 0; k < n; ++k) {
        rp[k] = cp[k].real();
    }
    res[0] = ArrayOf(realClass, sortedComplex.dimensions(), rp);
    return res;
}
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
        switch (arrayIn.dataClass()) {
        case NLS_CELL_ARRAY: {
            if (nargin != 1) {
                Error(L"Nelson:data_analysis:oneInputParameterSupportedCell",
                    L"Only one input parameter is supported for cell arrays.");
            }
            res.push_back(ArrayOf::empty(arrayIn.dimensions()));
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
            // the sorted values of an empty array keep its class
            ArrayOf sortedValues = ArrayOf::empty(arrayIn.dimensions());
            if (arrayIn.dataClass() != NLS_STRING_ARRAY) {
                sortedValues.promoteType(arrayIn.dataClass());
            }
            res.push_back(sortedValues);
            if (withIndex) {
                res.push_back(ArrayOf::empty(arrayIn.dimensions()));
            }
        } break;
        case NLS_HANDLE:
        case NLS_STRUCT_ARRAY:
        case NLS_CLASS_ARRAY:
        case NLS_CLASSDEF_ARRAY:
        case NLS_FUNCTION_HANDLE:
        default: {
            needToOverload = true;
            return res;
        } break;
        }
    } else {
        indexType dimOperate;
        Dimensions inDim(arrayIn.dimensions());
        if (dim == 0) {
            indexType lenInDim = inDim.length();
            indexType d = 0;
            while (d < lenInDim && inDim.at(d) == 1) {
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
        for (indexType d = dimOperate + 1; d < inDim.length(); d++) {
            planecount *= inDim[d];
        }
        switch (arrayIn.dataClass()) {
        case NLS_HANDLE: {
            needToOverload = true;
            return res;
        } break;
        case NLS_CELL_ARRAY: {
            if (nargin != 1) {
                Error(L"Nelson:data_analysis:oneInputParameterSupportedCell",
                    L"Only one input parameter is supported for cell arrays.");
            }
            res = sortCell(
                arrayIn, withIndex, linesize, planecount, planesize, outDim, dim, needToOverload);
        } break;
        case NLS_CLASS_ARRAY:
        case NLS_CLASSDEF_ARRAY:
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
            if (comparisonMethod == COMPARISON_METHOD::ABS_METHOD) {
                res = sortIntegerByAbsolute<int8>(
                    arrayIn, NLS_INT8, withIndex, linesize, planecount, planesize, outDim, ascend);
            } else {
                res = sortInteger<int8>(arrayIn, NLS_INT8, withIndex, linesize, planecount,
                    planesize, outDim, dim, ascend);
            }
        } break;
        case NLS_UINT16: {
            res = sortInteger<uint16>(arrayIn, NLS_UINT16, withIndex, linesize, planecount,
                planesize, outDim, dim, ascend);
        } break;
        case NLS_INT16: {
            if (comparisonMethod == COMPARISON_METHOD::ABS_METHOD) {
                res = sortIntegerByAbsolute<int16>(
                    arrayIn, NLS_INT16, withIndex, linesize, planecount, planesize, outDim, ascend);
            } else {
                res = sortInteger<int16>(arrayIn, NLS_INT16, withIndex, linesize, planecount,
                    planesize, outDim, dim, ascend);
            }
        } break;
        case NLS_UINT32: {
            res = sortInteger<uint32>(arrayIn, NLS_UINT32, withIndex, linesize, planecount,
                planesize, outDim, dim, ascend);
        } break;
        case NLS_INT32: {
            if (comparisonMethod == COMPARISON_METHOD::ABS_METHOD) {
                res = sortIntegerByAbsolute<int32>(
                    arrayIn, NLS_INT32, withIndex, linesize, planecount, planesize, outDim, ascend);
            } else {
                res = sortInteger<int32>(arrayIn, NLS_INT32, withIndex, linesize, planecount,
                    planesize, outDim, dim, ascend);
            }
        } break;
        case NLS_UINT64: {
            res = sortInteger<uint64>(arrayIn, NLS_UINT64, withIndex, linesize, planecount,
                planesize, outDim, dim, ascend);
        } break;
        case NLS_INT64: {
            if (comparisonMethod == COMPARISON_METHOD::ABS_METHOD) {
                res = sortIntegerByAbsolute<int64>(
                    arrayIn, NLS_INT64, withIndex, linesize, planecount, planesize, outDim, ascend);
            } else {
                res = sortInteger<int64>(arrayIn, NLS_INT64, withIndex, linesize, planecount,
                    planesize, outDim, dim, ascend);
            }
        } break;
        case NLS_SINGLE: {
            if (comparisonMethod == COMPARISON_METHOD::ABS_METHOD) {
                res = sortRealByAbsolute<single>(arrayIn, NLS_SINGLE, NLS_SCOMPLEX, withIndex,
                    linesize, planecount, planesize, outDim, dim, ascend, placement,
                    comparisonMethod);
            } else {
                res = sortReal<single>(arrayIn, NLS_SINGLE, withIndex, linesize, planecount,
                    planesize, outDim, dim, ascend, placement);
            }
        } break;
        case NLS_DOUBLE: {
            if (comparisonMethod == COMPARISON_METHOD::ABS_METHOD) {
                res = sortRealByAbsolute<double>(arrayIn, NLS_DOUBLE, NLS_DCOMPLEX, withIndex,
                    linesize, planecount, planesize, outDim, dim, ascend, placement,
                    comparisonMethod);
            } else {
                res = sortReal<double>(arrayIn, NLS_DOUBLE, withIndex, linesize, planecount,
                    planesize, outDim, dim, ascend, placement);
            }
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
