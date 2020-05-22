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
#include "graphic_object_classBuiltin.hpp"
#include "GraphicObject.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GraphicsGateway::graphic_object_classBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }

    ArrayOf paramGo = argIn[0];
    if (paramGo.getDataClass() != NLS_GO_HANDLE) {
        Error(_W("graphic_object expected."));
    }
    Dimensions dims = paramGo.getDimensions();
    std::string classType = NLS_GO_HANDLE_STR;
    if (!dims.isEmpty(false)) {
        if (dims.isScalar()) {
            auto* go = (GraphicObject*)paramGo.getContentAsGraphicObjectScalar();
            if (go != nullptr) {
                classType = go->getType();
            }
        } else {
            auto* qp = (nelson_handle*)paramGo.getDataPointer();
            auto* go = (GraphicObject*)NELSON_HANDLE_TO_PTR(qp[0]);
            if (go != nullptr) {
                classType = go->getType();
            }
            for (size_t k = 1; k < dims.getElementCount(); ++k) {
                go = (GraphicObject*)NELSON_HANDLE_TO_PTR(qp[k]);
                if (go != nullptr) {
                    if (classType != go->getType()) {
                        classType = NLS_GO_HANDLE_STR;
                        break;
                    }
                } else {
                    classType = NLS_GO_HANDLE_STR;
                    break;
                }
            }
        }
    }
    retval.push_back(ArrayOf::characterArrayConstructor(classType));
    return retval;
}
//=============================================================================
