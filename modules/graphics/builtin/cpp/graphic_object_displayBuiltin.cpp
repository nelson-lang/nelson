//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "graphic_object_displayBuiltin.hpp"
#include "GraphicObject.hpp"
#include "GraphicObjectDisplay.hpp"
#include "GOFigure.hpp"
#include "DisplayVariableHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GraphicsGateway::graphic_object_displayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf paramGo = argIn[0];
    if (paramGo.getDataClass() != NLS_GO_HANDLE) {
        Error(_W("graphic_object expected."));
    }
    auto* ptrGO = (nelson_handle*)paramGo.getDataPointer();
    std::wstring name;
    if (argIn.size() == 2) {
        name = argIn[1].getContentAsWideString();
    }
    Interface* io = eval->getInterface();
    DisplayVariableHeader(io, argIn[0], name, false);
    Dimensions dims = paramGo.getDimensions();
    graphicObjectDisplay(io, dims, ptrGO);
    DisplayVariableFooter(io, name.empty());
    return retval;
}
//=============================================================================
