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
#include "FindConcatenateClass.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Class
FindConcatenateClass(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = true;
    if (A.isSparse() && B.isSparse()) {
        return NLS_NOT_TYPED;
    }
    Class Aclass = A.getDataClass();
    Class Bclass = B.getDataClass();
    if (Aclass == Bclass) {
        needToOverload = false;
        return Aclass;
    }
    if (Aclass == NLS_CELL_ARRAY) {
        needToOverload = false;
        return Aclass;
    }
    if (Bclass == NLS_CELL_ARRAY) {
        needToOverload = false;
        return Bclass;
    }
    bool isIntegerA = A.isIntegerType();
    bool isIntegerB = B.isIntegerType();
    if (isIntegerA && isIntegerB) {
        if (Aclass == Bclass) {
            needToOverload = false;
            return Aclass;
        } else {
            needToOverload = false;
            if (Aclass > Bclass) {
                return Aclass;
            } else {
                return Bclass;
            }
        }
    }
    if ((Aclass == NLS_CHAR && isIntegerB) || (Bclass == NLS_CHAR && isIntegerA)) {
        needToOverload = false;
        return NLS_CHAR;
    }
    if (((Aclass == NLS_CHAR) && (Bclass == NLS_SINGLE))
        || ((Bclass == NLS_CHAR) && (Aclass == NLS_SINGLE))) {
        needToOverload = false;
        return NLS_CHAR;
    }
    if (((Aclass == NLS_CHAR) && (Bclass == NLS_SCOMPLEX))
        || ((Bclass == NLS_CHAR) && (Aclass == NLS_SCOMPLEX))) {
        Error(_W("Complex values cannot be converted to chars."));
    }
    if (isIntegerA && Bclass == NLS_SINGLE) {
        needToOverload = false;
        return Aclass;
    }
    if (isIntegerB && Aclass == NLS_SINGLE) {
        needToOverload = false;
        return Bclass;
    }
    if (isIntegerA && Bclass == NLS_DOUBLE) {
        needToOverload = false;
        return Aclass;
    }
    if (isIntegerB && Aclass == NLS_DOUBLE) {
        needToOverload = false;
        return Bclass;
    }
    if ((isIntegerA && Bclass == NLS_SCOMPLEX) || (isIntegerB && Aclass == NLS_SCOMPLEX)
        || (isIntegerA && Bclass == NLS_DCOMPLEX) || (isIntegerB && Aclass == NLS_DCOMPLEX)) {
        Error(_W("Complex values cannot be converted to integers."));
    }
    if (((Aclass == NLS_LOGICAL) && (Bclass == NLS_CHAR))
        || ((Bclass == NLS_LOGICAL) && (Aclass == NLS_CHAR))) {
        Error(_W("Conversion to char from logical is not possible."));
    }

    if (((Aclass == NLS_CHAR) && (Bclass == NLS_DOUBLE))
        || ((Bclass == NLS_CHAR) && (Aclass == NLS_DOUBLE))
        || ((Aclass == NLS_CHAR) && (Bclass == NLS_SINGLE))
        || ((Bclass == NLS_CHAR) && (Aclass == NLS_SINGLE))) {
        needToOverload = false;
        return NLS_CHAR;
    }

    bool isComplexA = A.isComplex();
    bool isComplexB = B.isComplex();

    if ((isComplexA && Bclass == NLS_CHAR) || (isComplexB && Aclass == NLS_CHAR)) {
        Error(_W("Conversion to char from complex is not possible."));
    }

    if (isIntegerA && Bclass == NLS_LOGICAL) {
        needToOverload = false;
        return Aclass;
    }
    if (isIntegerB && Aclass == NLS_LOGICAL) {
        needToOverload = false;
        return Bclass;
    }
    if ((Aclass == NLS_SINGLE && Bclass == NLS_LOGICAL)
        || (Bclass == NLS_SINGLE && Aclass == NLS_LOGICAL)
        || (Aclass == NLS_SINGLE && Bclass == NLS_DOUBLE)
        || (Bclass == NLS_SINGLE && Aclass == NLS_DOUBLE)) {
        needToOverload = false;
        return NLS_SINGLE;
    }
    if ((Aclass == NLS_DOUBLE && Bclass == NLS_LOGICAL)
        || (Bclass == NLS_DOUBLE && Aclass == NLS_LOGICAL)) {
        needToOverload = false;
        return NLS_DOUBLE;
    }
    if ((Aclass == NLS_DCOMPLEX && Bclass == NLS_LOGICAL)
        || (Bclass == NLS_DCOMPLEX && Aclass == NLS_LOGICAL)
        || (Aclass == NLS_DOUBLE && Bclass == NLS_DCOMPLEX)
        || (Aclass == NLS_DCOMPLEX && Bclass == NLS_DOUBLE)) {
        needToOverload = false;
        return NLS_DCOMPLEX;
    }
    if ((Aclass == NLS_SCOMPLEX && Bclass == NLS_LOGICAL)
        || (Bclass == NLS_SCOMPLEX && Aclass == NLS_LOGICAL)
        || (Aclass == NLS_SINGLE && Bclass == NLS_DCOMPLEX)
        || (Bclass == NLS_SINGLE && Aclass == NLS_DCOMPLEX)
        || (Bclass == NLS_SINGLE && Aclass == NLS_SCOMPLEX)
        || (Aclass == NLS_SINGLE && Bclass == NLS_SCOMPLEX)
        || (Aclass == NLS_SCOMPLEX && Bclass == NLS_SINGLE)
        || (Aclass == NLS_DCOMPLEX && Bclass == NLS_SCOMPLEX)
        || (Bclass == NLS_DCOMPLEX && Aclass == NLS_SCOMPLEX)) {
        needToOverload = false;
        return NLS_SCOMPLEX;
    }
    return NLS_NOT_TYPED;
}
//=============================================================================
}
//=============================================================================
