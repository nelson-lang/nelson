//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "graphics_object_displayBuiltin.hpp"
#include "GraphicsObjectDisplay.hpp"
#include "DisplayVariableHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
GraphicsGateway::graphics_object_displayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf paramGo = argIn[0];
    if (paramGo.getDataClass() != NLS_GO_HANDLE) {
        Error(_W("graphics_object expected."));
    }
    auto* ptrGO = (nelson_handle*)paramGo.getDataPointer();
    std::wstring name;
    if (argIn.size() == 2) {
        name = argIn[1].getContentAsWideString();
    }
    Interface* io = eval->getInterface();
    DisplayVariableHeader(io, argIn[0], name, false);
    Dimensions dims = paramGo.getDimensions();
    graphicsObjectDisplay(io, dims, ptrGO);
    DisplayVariableFooter(io, name.empty());
    return retval;
}
//=============================================================================
