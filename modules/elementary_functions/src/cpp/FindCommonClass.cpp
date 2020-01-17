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
#include "FindCommonClass.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Class
FindCommonClass(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    Class commonClass = NLS_NOT_TYPED;
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
