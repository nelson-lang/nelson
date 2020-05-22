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
#include "graphic_object_deleteBuiltin.hpp"
#include "GraphicObject.hpp"
#include "GraphicObjectDelete.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GraphicsGateway::graphic_object_deleteBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    ArrayOf paramGo = argIn[0];
    auto* ptrGO = (nelson_handle*)paramGo.getDataPointer();
    if (ptrGO != nullptr) {
        indexType nbElements = paramGo.getDimensions().getElementCount();
        for (indexType k = 0; k < nbElements; ++k) {
            auto* go = (GraphicObject*)NELSON_HANDLE_TO_PTR(ptrGO[k]);
            if (go != nullptr) {
                if (!graphicObjectDelete(go)) {
                    Error(_W("Cannot delete graphic_object."));
                }
            }
        }
    }
    return retval;
}
//=============================================================================
