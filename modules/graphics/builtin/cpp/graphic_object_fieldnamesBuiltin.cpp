//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "graphic_object_fieldnamesBuiltin.hpp"
#include "GraphicObject.hpp"
#include "Error.hpp"
#include "ToCellString.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GraphicsGateway::graphic_object_fieldnamesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf paramGo = argIn[0];
    if (paramGo.getDataClass() != NLS_GO_HANDLE) {
        Error(_W("graphic_object expected."));
    }
    if (!paramGo.isScalar()) {
        Error(_W("Scalar graphic_object expected."));
    }
    auto* go = (GraphicObject*)paramGo.getContentAsGraphicObjectScalar();
    retval << ToCellStringAsColumn(go->getPropertiesName());
    return retval;
}
//=============================================================================
