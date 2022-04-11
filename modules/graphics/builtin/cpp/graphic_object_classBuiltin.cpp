//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
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
    retval << ArrayOf::characterArrayConstructor(classType);
    return retval;
}
//=============================================================================
