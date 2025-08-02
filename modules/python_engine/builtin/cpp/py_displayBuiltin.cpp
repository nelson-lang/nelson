//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "py_displayBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PythonObjectHandle.hpp"
#include "DisplayVariableHelpers.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static void
DisplayPyVariableHeader(
    Interface* io, PythonObjectHandle* poh, const std::wstring& name, bool asDisp);
//=============================================================================
ArrayOfVector
Nelson::Python_engineGateway::py_displayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (!eval) {
        Error(_W("Evaluator not available."));
    }
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 2);
    ArrayOf param1 = argIn[0];
    std::wstring name;
    if (argIn.size() == 2) {
        name = argIn[1].getContentAsWideString();
    }
    if (param1.isHandle() && param1.getHandleCategory() == NLS_HANDLE_PYOBJECT_CATEGORY_STR) {
        Interface* io = eval->getInterface();
        PythonObjectHandle* poh = (PythonObjectHandle*)param1.getContentAsHandleScalar();
        if (!poh || poh->isMainPythonInterpreter()) {
            return {};
        }
        DisplayPyVariableHeader(io, poh, name, name.empty());

        if (poh) {
            poh->display(io);
        }
        DisplayVariableFooter(io, name.empty());
    } else {
        Error(_W("Python object expected."));
    }
    return retval;
}
//=============================================================================
void
DisplayPyVariableHeader(
    Interface* io, PythonObjectHandle* poh, const std::wstring& name, bool asDisp)
{
    if (!asDisp) {
        if (NelsonConfiguration::getInstance()->getLineSpacingDisplay() == NLS_LINE_SPACING_LOOSE
            && !name.empty()) {
            io->outputMessage(L"\n");
        }

        if (!name.empty()) {
            io->outputMessage(name + L" =\n");
            if (NelsonConfiguration::getInstance()->getLineSpacingDisplay()
                == NLS_LINE_SPACING_LOOSE) {
                io->outputMessage(L"\n");
            }
        }
    }
}
//=============================================================================
