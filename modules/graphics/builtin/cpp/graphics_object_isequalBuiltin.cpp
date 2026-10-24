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
        return ArrayOf::logicalScalar(false);
    }
    Dimensions dims1 = paramGo1.dimensions();
    auto* ptrGO1 = (nelson_handle*)paramGo1.dataPointer();

    for (size_t k = 1; k < argIn.size(); k++) {
        ArrayOf paramGo2 = argIn[k];
        if (!paramGo2.isGraphicsObject()) {
            return ArrayOf::logicalScalar(false);
        }
        Dimensions dims2 = paramGo2.dimensions();
        if (!dims1.equals(dims2)) {
            return ArrayOf::logicalScalar(false);
        }
        auto* ptrGO2 = (nelson_handle*)paramGo2.dataPointer();
        if (ptrGO1 == ptrGO2) {
            return ArrayOf::logicalScalar(true);
        }
        for (indexType j = 0; j < dims1.elementCount(); ++j) {
            if (ptrGO1[j] != ptrGO2[j]) {
                return ArrayOf::logicalScalar(false);
            }
        }
    }
    return ArrayOf::logicalScalar(true);
}
//=============================================================================
