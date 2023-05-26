//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FindCommonClass.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NelsonType
FindCommonClass(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    NelsonType commonClass = NLS_UNKNOWN;
    if (A.isSparse() || B.isSparse()) {
        needToOverload = true;
        return commonClass;
    }
    bool isDoubleA = (A.isDoubleType() || A.isNdArrayDoubleType());
    bool isDoubleB = (B.isDoubleType() || B.isNdArrayDoubleType());
    bool isSingleA = (A.isSingleType() || A.isNdArraySingleType());
    bool isSingleB = (B.isSingleType() || B.isNdArraySingleType());
    if ((isDoubleA || isSingleA) && (isDoubleB || isSingleB)) {
        if (isDoubleA && isDoubleB) {
            if (A.getDataClass() == NLS_DCOMPLEX || B.getDataClass() == NLS_DCOMPLEX) {
                commonClass = NLS_DCOMPLEX;
            } else {
                commonClass = NLS_DOUBLE;
            }
        } else if (isSingleA && isSingleB) {
            if (A.getDataClass() == NLS_SCOMPLEX || B.getDataClass() == NLS_SCOMPLEX) {
                commonClass = NLS_SCOMPLEX;
            } else {
                commonClass = NLS_SINGLE;
            }
        } else {
            if (A.getDataClass() == NLS_DOUBLE) {
                commonClass = NLS_SINGLE;
            } else {
                commonClass = NLS_SCOMPLEX;
            }
            if (B.getDataClass() == NLS_DOUBLE) {
                commonClass = NLS_SINGLE;
            } else {
                commonClass = NLS_SCOMPLEX;
            }
        }
    } else {
        bool isIntegerA = A.isIntegerType() || A.isNdArrayIntegerType();
        bool isIntegerB = B.isIntegerType() || B.isNdArrayIntegerType();
        if (isIntegerA && isIntegerB) {
            if (A.getDataClass() == B.getDataClass()) {
                commonClass = A.getDataClass();
            } else {
                Error(_W("Integers of the same class expected."));
            }
        } else {
            if (isIntegerA && isDoubleB) {
                commonClass = A.getDataClass();
            } else if (isIntegerB && isDoubleA) {
                commonClass = B.getDataClass();
            } else {
                needToOverload = true;
            }
        }
    }
    return commonClass;
}
//=============================================================================
}
//=============================================================================
