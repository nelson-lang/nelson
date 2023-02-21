//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "graphics_object_isequalBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GraphicsGateway::graphics_object_isequalBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf paramGo1 = argIn[0];
    ArrayOf paramGo2 = argIn[1];
    if (paramGo1.getDataClass() != NLS_GO_HANDLE) {
        Error(_W("graphics_object expected."));
    }
    if (paramGo2.getDataClass() != NLS_GO_HANDLE) {
        Error(_W("graphics_object expected."));
    }

    Dimensions dims1 = paramGo1.getDimensions();
    Dimensions dims2 = paramGo2.getDimensions();
    if (!dims1.equals(dims2)) {
        retval << ArrayOf::logicalConstructor(false);
        return retval;
    }
    auto* ptrGO1 = (nelson_handle*)paramGo1.getDataPointer();
    auto* ptrGO2 = (nelson_handle*)paramGo2.getDataPointer();
    if (ptrGO1 == ptrGO2) {
        retval << ArrayOf::logicalConstructor(true);
        return retval;
    }
    for (indexType k = 0; k < dims1.getElementCount(); ++k) {
        if (ptrGO1[k] != ptrGO2[k]) {
            retval << ArrayOf::logicalConstructor(false);
            return retval;
        }
    }
    retval << ArrayOf::logicalConstructor(true);
    return retval;
}
//=============================================================================
