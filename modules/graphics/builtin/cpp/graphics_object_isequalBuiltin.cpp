//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "graphics_object_isequalBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GraphicsGateway::graphics_object_isequalBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0, 1);

    ArrayOf paramGo1 = argIn[0];
    if (!paramGo1.isGraphicsObject()) {
        return ArrayOf::logicalConstructor(false);
    }
    Dimensions dims1 = paramGo1.getDimensions();
    auto* ptrGO1 = (nelson_handle*)paramGo1.getDataPointer();

    for (size_t k = 1; k < argIn.size(); k++) {
        ArrayOf paramGo2 = argIn[k];
        if (!paramGo2.isGraphicsObject()) {
            return ArrayOf::logicalConstructor(false);
        }
        Dimensions dims2 = paramGo2.getDimensions();
        if (!dims1.equals(dims2)) {
            return ArrayOf::logicalConstructor(false);
        }
        auto* ptrGO2 = (nelson_handle*)paramGo2.getDataPointer();
        if (ptrGO1 == ptrGO2) {
            return ArrayOf::logicalConstructor(true);
        }
        for (indexType j = 0; j < dims1.getElementCount(); ++j) {
            if (ptrGO1[j] != ptrGO2[j]) {
                return ArrayOf::logicalConstructor(false);
            }
        }
    }
    return ArrayOf::logicalConstructor(true);
}
//=============================================================================
