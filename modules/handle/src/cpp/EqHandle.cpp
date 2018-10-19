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
#include "EqHandle.hpp"
#include "Error.hpp"
#include "Exception.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "MatrixCheck.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
EqHandle(ArrayOf A, ArrayOf B)
{
    ArrayOf res;
    if (!A.isHandle() && !B.isHandle()) {
        Error(_W("handle expected."));
    }
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    if (!(SameSizeCheck(dimsA, dimsB) || A.isScalar() || B.isScalar())) {
        Error(std::string(_("Size mismatch on arguments to arithmetic operator ")) + "eq");
    }
    int Astride = 0, Bstride = 0;
    void* Cp = nullptr;
    indexType Clen = 0;
    Dimensions Cdim;
    if (A.isScalar()) {
        Astride = 0;
        Bstride = 1;
        Cdim = dimsB;
    } else if (B.isScalar()) {
        Astride = 1;
        Bstride = 0;
        Cdim = dimsA;
    } else {
        Astride = 1;
        Bstride = 1;
        Cdim = dimsA;
    }
    Clen = Cdim.getElementCount();
    Cp = new_with_exception<logical>(Clen);
    logical* C = (logical*)Cp;
    if (A.isHandle() && B.isHandle()) {
        nelson_handle* hA = (nelson_handle*)A.getDataPointer();
        nelson_handle* hB = (nelson_handle*)B.getDataPointer();
        if ((Astride == 1) && (Bstride == 1)) {
            for (indexType i = 0; i < Clen; i++) {
                C[i] = (hA[i] == hB[i]) ? 1 : 0;
            }
        } else {
            if (Astride) {
                for (indexType i = 0; i < Clen; i++) {
                    C[i] = (hA[i] == hB[0]) ? 1 : 0;
                }
            } else {
                for (indexType i = 0; i < Clen; i++) {
                    C[i] = (hA[0] == hB[i]) ? 1 : 0;
                }
            }
        }
    }
    return ArrayOf(NLS_LOGICAL, Cdim, Cp);
}
//=============================================================================
}
//=============================================================================
