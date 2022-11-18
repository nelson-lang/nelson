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
static ArrayOf
DotRightDivide(const ArrayOf& A, const ArrayOf& B, NelsonType commonClass)
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
            res = binaryOperatorEmptyMatrixEmptryMatrix(A, B, commonClass, "./");
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
        default:
            Error(_W("Type not managed."));
            break;
        }
    } else {
        switch (commonClass) {
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
            Error(_W("Type not managed."));
            break;
        }
    }
    return res;
}
//=============================================================================
ArrayOf
DotRightDivide(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf res;
    if (A.isStringArray() || B.isStringArray()) {
        needToOverload = true;
        return res;
    }
    NelsonType commonClass = FindCommonClass(A, B, needToOverload);
    if (!needToOverload) {
        if (A.getDataClass() == commonClass && B.getDataClass() == commonClass) {
            res = DotRightDivide(A, B, commonClass);
        } else {
            ArrayOf a = A;
            a.promoteType(commonClass);
            ArrayOf b = B;
            b.promoteType(commonClass);
            res = DotRightDivide(a, b, commonClass);
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
