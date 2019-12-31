//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <Eigen/Dense>
#include <complex>
#include "Sort.hpp"
#include "SortHelpers.hpp"
#include "SortIntegerHelpers.hpp"
#include "SortCellHelpers.hpp"
#include "SortRealHelpers.hpp"
#include "SortComplexHelpers.hpp"
#include "SortStringHelpers.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOfVector
Sort(ArrayOf arrayIn, size_t nargin, bool withIndex, indexType dim, bool ascend,
    MISSING_PLACEMENT placement, COMPARISON_METHOD comparisonMethod, bool& needToOverload)
{
    ArrayOfVector res;
    needToOverload = false;
    if (arrayIn.isEmpty()) {
        res.push_back(ArrayOf::emptyConstructor(arrayIn.getDimensions()));
        if (withIndex) {
            res.push_back(ArrayOf::emptyConstructor(arrayIn.getDimensions()));
        }
    } else {
        if (arrayIn.isClassStruct()) {
            needToOverload = true;
            return res;
        }
        indexType dimOperate;
        Dimensions inDim(arrayIn.getDimensions());
        if (dim == 0) {
            indexType d = 0;
            while (inDim[d] == 1) {
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
