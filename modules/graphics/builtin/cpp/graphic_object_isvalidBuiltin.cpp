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
#include "graphic_object_isvalidBuiltin.hpp"
#include "GraphicObject.hpp"
#include "GraphicObjectGet.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GraphicsGateway::graphic_object_isvalidBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    ArrayOf paramGo = argIn[0];
    if (paramGo.getDataClass() != NLS_GO_HANDLE) {
        retval.push_back(ArrayOf::logicalConstructor(false));
    }
    Dimensions dims = paramGo.getDimensions();
    logical* res = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, dims.getElementCount(), stringVector(), true);
    ArrayOf values = ArrayOf(NLS_LOGICAL, dims, res);
    nelson_handle* ptrGO = (nelson_handle*)paramGo.getDataPointer();
    if (!dims.isEmpty(false)) {
        if (ptrGO != nullptr) {
            for (size_t k = 0; k < dims.getElementCount(); ++k) {
                auto* go = (GraphicObject*)NELSON_HANDLE_TO_PTR(ptrGO[k]);
                if ((go == nullptr) || (go->referenceCount() == 0)) {
                    res[k] = false;
                } else {
                    res[k] = true;
                }
            }
        }
    }
    retval.push_back(values);
    return retval;
}
//=============================================================================
