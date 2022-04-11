//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
        retval << ArrayOf::logicalConstructor(false);
    }
    Dimensions dims = paramGo.getDimensions();
    logical* res = (logical*)ArrayOf::allocateArrayOf(
        NLS_LOGICAL, dims.getElementCount(), stringVector(), true);
    ArrayOf values = ArrayOf(NLS_LOGICAL, dims, res);
    auto* ptrGO = (nelson_handle*)paramGo.getDataPointer();
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
    retval << values;
    return retval;
}
//=============================================================================
