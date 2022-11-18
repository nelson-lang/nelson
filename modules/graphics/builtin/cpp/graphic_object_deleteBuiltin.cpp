//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "graphic_object_deleteBuiltin.hpp"
#include "GraphicObject.hpp"
#include "GraphicObjectDelete.hpp"
#include "Error.hpp"
#include "i18n.hpp"
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
        indexType nbElements = paramGo.getElementCount();
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
