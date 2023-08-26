//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "DotRightDivide.hpp"
#include "MatrixCheck.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "DotRightDivideReal.hpp"
#include "DotRightDivideComplex.hpp"
#include "DotRightDivideInteger.hpp"
#include "FindCommonClass.hpp"
#include "BinaryOperatorsHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
DotRightDivide(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    NelsonType commonType = A.getDataClass();
    ArrayOf res;
    if (A.isEmpty() || B.isEmpty()) {
        if (A.isScalar() || B.isScalar()) {
            if (A.isScalar()) {
                res = B;
            } else {
                res = A;
            }
        } else {
            res = binaryOperatorEmptyMatrixEmptryMatrix(A, B, commonType, "./");
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
    if (A.isSparse() || B.isSparse()) {
        needToOverload = true;
        return {};
    }
    switch (commonType) {
    case NLS_INT8:
        return integer_dotRightDivide<int8>(NLS_INT8, A, B);
    case NLS_UINT8:
        return integer_dotRightDivide<uint8>(NLS_UINT8, A, B);
    case NLS_INT16:
        return integer_dotRightDivide<int16>(NLS_INT16, A, B);
    case NLS_UINT16:
        return integer_dotRightDivide<uint16>(NLS_UINT16, A, B);
    case NLS_INT32:
        return integer_dotRightDivide<int32>(NLS_INT32, A, B);
    case NLS_UINT32:
        return integer_dotRightDivide<uint32>(NLS_UINT32, A, B);
    case NLS_INT64:
        return integer_dotRightDivide<int64>(NLS_INT64, A, B);
    case NLS_UINT64:
        return integer_dotRightDivide<uint64>(NLS_UINT64, A, B);
    case NLS_SINGLE: {
        res = real_dotRightDivide<single>(NLS_SINGLE, A, B);
    } break;
    case NLS_DOUBLE: {
        res = real_dotRightDivide<double>(NLS_DOUBLE, A, B);
    } break;
    case NLS_SCOMPLEX: {
        res = complex_dotRightDivide<single>(NLS_SCOMPLEX, A, B);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_DCOMPLEX: {
        res = complex_dotRightDivide<double>(NLS_DCOMPLEX, A, B);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    default:
        needToOverload = true;
        break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
