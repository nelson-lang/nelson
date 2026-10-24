//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "functionsBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOf
buildWorkspaceCell(AnonymousMacroFunctionDef* cp);
//=============================================================================
ArrayOfVector
Nelson::FunctionHandleGateway::functionsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    ArrayOf arg1 = argIn[0];
    if (!arg1.isFunctionHandle()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"function handle");
    }
    function_handle fh = arg1.asFunctionHandle();
    AnonymousMacroFunctionDef* cp
        = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
    if (cp == nullptr) {
        Error(L"Nelson:functions:invalidFunctionHandle", L"Invalid function handle.");
    }
    stringVector fieldnames;
    ArrayOfVector fieldvalues;
    if (cp->isFunctionHandle()) {
        // Named function handle (e.g. @sin): fields function, type, file.
        fieldnames.push_back("function");
        fieldvalues << ArrayOf::characterArray(cp->toString());
        fieldnames.push_back("type");
        fieldvalues << ArrayOf::characterArray(std::string("simple"));
        fieldnames.push_back("file");
        fieldvalues << ArrayOf::characterArray(std::string(""));
    } else {
        // Anonymous function: fields function, type, file, within_file_path,
        // workspace (a 1x1 cell holding the captured variables as a struct).
        fieldnames.push_back("function");
        fieldvalues << ArrayOf::characterArray(cp->toString());
        fieldnames.push_back("type");
        fieldvalues << ArrayOf::characterArray(std::string("anonymous"));
        fieldnames.push_back("file");
        fieldvalues << ArrayOf::characterArray(std::string(""));
        fieldnames.push_back("within_file_path");
        fieldvalues << ArrayOf::characterArray(std::string(""));
        fieldnames.push_back("workspace");
        fieldvalues << buildWorkspaceCell(cp);
    }
    retval << ArrayOf::structScalar(fieldnames, fieldvalues);
    return retval;
}
//=============================================================================
ArrayOf
buildWorkspaceCell(AnonymousMacroFunctionDef* cp)
{
    stringVector names = cp->getVariableNames();
    std::vector<ArrayOf> values = cp->getVariables();
    ArrayOfVector fieldvalues;
    stringVector fieldnames;
    size_t count = names.size();
    if (values.size() < count) {
        count = values.size();
    }
    for (size_t k = 0; k < count; ++k) {
        fieldnames.push_back(names[k]);
        fieldvalues << values[k];
    }
    ArrayOf workspaceStruct = ArrayOf::structScalar(fieldnames, fieldvalues);
    ArrayOf* elements = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, 1));
    elements[0] = workspaceStruct;
    return ArrayOf(NLS_CELL_ARRAY, Dimensions(1, 1), elements);
}
//=============================================================================
