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
#include <algorithm>
#include <Eigen/Dense>
#include "Addition.hpp"
#include "MatrixCheck.hpp"
#include "Exception.hpp"
#include "StringArrayAddition.hpp"
#include "AdditionReal.hpp"
#include "AdditionComplex.hpp"
#include "AdditionInteger.hpp"
#include "FindCommonClass.hpp"
#include "BinaryOperatorsHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
Addition(const ArrayOf& A, const ArrayOf& B, Class commonClass)
{
    ArrayOf res;
    if (A.isEmpty() || B.isEmpty()) {
        if (A.isScalar() || B.isScalar()) {
            if (A.isScalar()) {
                res = B;
            } else {
                res = A;
            }
        } else {
            res = binaryOperatorEmptyMatrixEmptryMatrix(A, B, commonClass, "+");
        }
        if (res.getDataClass() == NLS_DCOMPLEX) {
            if (res.allReal()) {
                res.promoteType(NLS_DOUBLE);
            }
        }
        if (res.getDataClass() == NLS_SCOMPLEX) {
            if (res.allReal()) {
                res.promoteType(NLS_SINGLE);
            }
        }
        return res;
    }
    if (commonClass >= NLS_UINT8 && commonClass <= NLS_INT64) {
        switch (commonClass) {
        case NLS_INT8:
            return integer_addition<int8>(NLS_INT8, A, B);
        case NLS_UINT8:
            return integer_addition<uint8>(NLS_UINT8, A, B);
        case NLS_INT16:
            return integer_addition<int16>(NLS_INT16, A, B);
        case NLS_UINT16:
            return integer_addition<uint16>(NLS_UINT16, A, B);
        case NLS_INT32:
            return integer_addition<int32>(NLS_INT32, A, B);
        case NLS_UINT32:
            return integer_addition<uint32>(NLS_UINT32, A, B);
        case NLS_INT64:
            return integer_addition<int64>(NLS_INT64, A, B);
        case NLS_UINT64:
            return integer_addition<uint64>(NLS_UINT64, A, B);
        default:
            Error(_W("Type not managed."));
            break;
        }
    } else {
        switch (commonClass) {
        case NLS_SINGLE: {
            res = real_addition<single>(NLS_SINGLE, A, B);
        } break;
        case NLS_DOUBLE: {
            res = real_addition<double>(NLS_DOUBLE, A, B);
        } break;
        case NLS_SCOMPLEX: {
            res = complex_addition<single>(NLS_SCOMPLEX, A, B);
            if (res.allReal()) {
                res.promoteType(NLS_SINGLE);
            }
        } break;
        case NLS_DCOMPLEX: {
            res = complex_addition<double>(NLS_DCOMPLEX, A, B);
            if (res.allReal()) {
                res.promoteType(NLS_DOUBLE);
            }
        } break;
        default:
            Error(_W("Type not managed."));
            break;
        }
    }
    return res;
}
//=============================================================================
ArrayOf
Addition(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf res;
    if (A.isStringArray() || B.isStringArray()) {
        ArrayOf a = ArrayOf::toStringArray(A, needToOverload);
        if (needToOverload) {
            return res;
        }
        ArrayOf b = ArrayOf::toStringArray(B, needToOverload);
        if (needToOverload) {
            return res;
        }
        return stringArray_plus_stringArray(a, b);
    }
    Class commonClass = FindCommonClass(A, B, needToOverload);
    if (!needToOverload) {
        if (A.getDataClass() == commonClass && B.getDataClass() == commonClass) {
            res = Addition(A, B, commonClass);
        } else {
            ArrayOf a = A;
            a.promoteType(commonClass);
            ArrayOf b = B;
            b.promoteType(commonClass);
            res = Addition(a, b, commonClass);
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
