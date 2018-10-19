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
#include "IsValidHandle.hpp"
#include "Error.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
IsValidHandle(Evaluator* eval, ArrayOf A)
{
    ArrayOf res;
    if (A.isHandle()) {
        Dimensions dimsA = A.getDimensions();
        nelson_handle* qp = (nelson_handle*)A.getDataPointer();
        if (qp) {
            logical* resArray
                = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.getElementCount());
            for (size_t k = 0; k < dimsA.getElementCount(); k++) {
                nelson_handle hl = qp[k];
                HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
                if (hlObj != nullptr) {
                    if (hlObj->getPointer()) {
                        resArray[k] = true;
                    } else {
                        resArray[k] = false;
                    }
                } else {
                    resArray[k] = false;
                }
            }
            res = ArrayOf(NLS_LOGICAL, dimsA, resArray);
        } else {
            res = ArrayOf::emptyConstructor(dimsA);
        }
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_HANDLE_EXPECTED);
    }
    return res;
}
//=============================================================================
}
//=============================================================================
