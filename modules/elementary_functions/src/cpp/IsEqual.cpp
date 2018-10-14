//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "IsEqual.hpp"
#include "ImagPart.hpp"
#include "RealPart.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isequalornan(double a, double b)
{
    if (std::isnan(a) && std::isnan(b)) {
        return true;
    }
    return (a == b);
}
//=============================================================================
bool
IsEqual(ArrayOf& A, ArrayOf& B, bool sameTypes, bool withNaN, bool& needToOverload)
{
    needToOverload = false;
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    if (!dimsA.equals(dimsB)) {
        return false;
    }
    if (sameTypes) {
        if (A.getDataClass() != B.getDataClass()) {
            return false;
        }
    }
    if (A.isSparse() || B.isSparse()) {
        try {
            if (!A.isSparse()) {
                A.makeSparse();
            }
            if (!B.isSparse()) {
                B.makeSparse();
            }
            needToOverload = true;
            return false;
        } catch (const Exception&) {
            return false;
        }
    }
    if (A.isStruct() || B.isStruct()) {
        if (A.isStruct() && B.isStruct()) {
            needToOverload = true;
        }
        return false;
    }
    if (A.isCell() && B.isCell()) {
        needToOverload = true;
        return false;
    }
    indexType nbElementsA = dimsA.getElementCount();
    if (A.isStringArray() || B.isStringArray()) {
        if (A.isStringArray() && B.isStringArray()) {
            ArrayOf* elementA = (ArrayOf*)A.getDataPointer();
            ArrayOf* elementB = (ArrayOf*)B.getDataPointer();
            for (indexType k = 0; k < nbElementsA; k++) {
                ArrayOf el1 = elementA[k];
                ArrayOf el2 = elementB[k];
                bool res = IsEqual(el1, el2, sameTypes, withNaN, needToOverload);
                if (needToOverload) {
                    return false;
                } else {
                    if (!res) {
                        return false;
                    }
                }
            }
            return true;
        }
        return false;
    }
    bool isComplexA = A.getDataClass() == NLS_DCOMPLEX || A.getDataClass() == NLS_SCOMPLEX;
    bool isComplexB = B.getDataClass() == NLS_DCOMPLEX || B.getDataClass() == NLS_SCOMPLEX;
    bool isRealA = A.getDataClass() == NLS_DOUBLE || A.getDataClass() == NLS_SINGLE;
    bool isRealB = B.getDataClass() == NLS_DOUBLE || B.getDataClass() == NLS_SINGLE;
    bool isSingleOrDoubleA = A.getDataClass() == NLS_SINGLE || A.getDataClass() == NLS_DOUBLE;
    bool isSingleOrDoubleB = B.getDataClass() == NLS_SINGLE || B.getDataClass() == NLS_DOUBLE;
    if ((isSingleOrDoubleA || isComplexA) && (isSingleOrDoubleB || isComplexB)) {
        if (isRealA && isRealB) {
            A.promoteType(NLS_DOUBLE);
            B.promoteType(NLS_DOUBLE);
            double* ptrA = (double*)A.getDataPointer();
            double* ptrB = (double*)B.getDataPointer();
            if (withNaN) {
                for (indexType k = 0; k < nbElementsA; k++) {
                    if (!isequalornan(ptrA[k], ptrB[k])) {
                        return false;
                    }
                }
            } else {
                for (indexType k = 0; k < nbElementsA; k++) {
                    if (ptrA[k] != ptrB[k]) {
                        return false;
                    }
                }
            }
            return true;
        } else {
            A.promoteType(NLS_DCOMPLEX);
            B.promoteType(NLS_DCOMPLEX);
            ArrayOf realPartA = RealPart(A);
            ArrayOf realPartB = RealPart(B);
            ArrayOf imagPartA = ImagPart(A);
            ArrayOf imagPartB = ImagPart(B);
            double* ptrRealA = (double*)realPartA.getDataPointer();
            double* ptrRealB = (double*)realPartB.getDataPointer();
            double* ptrImagA = (double*)imagPartA.getDataPointer();
            double* ptrImagB = (double*)imagPartB.getDataPointer();
            if (withNaN) {
                for (indexType k = 0; k < nbElementsA; k++) {
                    if (!isequalornan(ptrRealA[k], ptrRealB[k])
                        || !isequalornan(ptrImagA[k], ptrImagB[k])) {
                        return false;
                    }
                }
            } else {
                for (indexType k = 0; k < nbElementsA; k++) {
                    if ((ptrRealA[k] != ptrRealB[k]) || (ptrImagA[k] != ptrImagB[k])) {
                        return false;
                    }
                }
            }
            return true;
        }
    } else {
        try {
            A.promoteType(NLS_DOUBLE);
            B.promoteType(NLS_DOUBLE);
        } catch (const Exception&) {
            needToOverload = true;
            return false;
        }
        double* ptrA = (double*)A.getDataPointer();
        double* ptrB = (double*)B.getDataPointer();
        if (withNaN) {
            for (indexType k = 0; k < nbElementsA; k++) {
                if (!isequalornan(ptrA[k], ptrB[k])) {
                    return false;
                }
            }
        } else {
            for (indexType k = 0; k < nbElementsA; k++) {
                if (ptrA[k] != ptrB[k]) {
                    return false;
                }
            }
        }
        return true;
    }
    needToOverload = true;
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
