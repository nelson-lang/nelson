//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
#include "BinaryOperatorsHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
DotRightDivide(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
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
            res = binaryOperatorEmptyMatrixEmptryMatrix(A, B,
                A.getDataClass() > B.getDataClass() ? A.getDataClass() : B.getDataClass(), "./");
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

    if (A.getDataClass() != B.getDataClass()) {
        if ((A.isDoubleClass() && B.isSingleClass()) || (A.isSingleClass() && B.isDoubleClass())) {
            bool isComplex = A.isComplex() || B.isComplex();
            ArrayOf AA = A;
            ArrayOf BB = B;
            if (isComplex) {
                AA.promoteType(NLS_SCOMPLEX);
                BB.promoteType(NLS_SCOMPLEX);
            } else {
                AA.promoteType(NLS_SINGLE);
                BB.promoteType(NLS_SINGLE);
            }
            return DotRightDivide(AA, BB, needToOverload);
        }

        if (A.isIntegerType()) {
            bool isCompatible = (B.getDataClass() == NLS_DOUBLE) && B.isScalar();
            if (!isCompatible) {
                Error(_W("Integers can only be combined with integers of the same class, or scalar "
                         "doubles."));
            }
            if (B.isComplex()) {
                Error(_W("Complex integer not allowed for arithmetic operator ") + L"*");
            }
            ArrayOf AA = A;
            AA.promoteType(B.getDataClass());
            ArrayOf res = DotRightDivide(AA, B, needToOverload);
            if (!needToOverload) {
                res.promoteType(A.getDataClass());
            }
            return res;
        } else if (B.isIntegerType()) {
            bool isCompatible = (A.getDataClass() == NLS_DOUBLE) && A.isScalar();
            if (!isCompatible) {
                Error(_W("Integers can only be combined with integers of the same class, or scalar "
                         "doubles."));
            }
            if (A.isComplex()) {
                Error(_W("Complex integer not allowed for arithmetic operator ") + L"*");
            }
            ArrayOf BB = B;
            BB.promoteType(A.getDataClass());
            ArrayOf res = DotRightDivide(A, BB, needToOverload);
            if (!needToOverload) {
                res.promoteType(B.getDataClass());
            }
            return res;
        }
    }
    NelsonType commonType = A.getDataClass();

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
