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
#include "NelsonConfiguration.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static void
displayVariableName(Interface* io, const ArrayOf& A, const std::wstring& name)
{
    if (name.empty()) {
        return;
    }
    if (NelsonConfiguration::getInstance()->getLineSpacingDisplay() == NLS_LINE_SPACING_LOOSE) {
        io->outputMessage(L"\n");
    }
    io->outputMessage(name + displayHeaderEqualSign(A) + L"\n");
}
//=============================================================================
ArrayOfVector
GraphicsGateway::graphics_object_displayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf paramGo = argIn[0];
    if (paramGo.dataClass() != NLS_GO_HANDLE) {
        Error(L"Nelson:graphics:expectedGraphicsObject:graphicsObjectExpected",
            L"graphics_object expected.");
    }
    auto* ptrGO = (nelson_handle*)paramGo.dataPointer();
    std::wstring name;
    if (argIn.size() == 2) {
        name = argIn[1].asWideString();
    }
    Interface* io = eval->getInterface();
    Dimensions dims = paramGo.dimensions();
    if (dims.isScalar()) {
        displayVariableName(io, paramGo, name);
    } else {
        DisplayVariableHeader(io, argIn[0], name, false);
    }
    graphicsObjectDisplay(io, dims, ptrGO);
    DisplayVariableFooter(io, name.empty());
    return retval;
}
//=============================================================================
