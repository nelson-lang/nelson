//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "editorHookBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#ifndef __EMSCRIPTEN__
#include "TextEditor.hpp"
#endif
//=============================================================================
using namespace Nelson;
//=============================================================================
// __editor__(action, ...) drives the text editor headlessly for the tests
ArrayOfVector
Nelson::TextEditorGateway::editorHookBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 2);
    if (!argIn[0].isRowVectorCharacterArray() && !argIn[0].isScalarStringArray()) {
        Error(L"Nelson:editor:invalidArgument:wrongTypeInputArgument",
            L"Wrong type for input argument #%d.", 1);
    }
#ifdef __EMSCRIPTEN__
    Error(L"Nelson:editor:unavailable", L"The text editor hook is unavailable.");
    return {};
#else
    std::wstring action = argIn[0].asWideString();
    // the action's own arguments (ListVector has no range constructor)
    ArrayOfVector arguments;
    for (size_t k = 1; k < argIn.size(); ++k) {
        arguments.push_back(argIn[k]);
    }
    return textEditorHook(eval, action, arguments);
#endif
}
//=============================================================================
