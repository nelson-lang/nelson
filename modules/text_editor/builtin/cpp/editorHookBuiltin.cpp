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
//=============================================================================
using namespace Nelson;
//=============================================================================
// __editor__(action, ...) drove the embedded editor headlessly for the tests.
// The embedded editor is served by the desktop (Monaco), which has no such
// hook, so the headless hook is unavailable here.
ArrayOfVector
Nelson::TextEditorGateway::editorHookBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 2);
    if (!argIn[0].isRowVectorCharacterArray() && !argIn[0].isScalarStringArray()) {
        Error(L"Nelson:editor:invalidArgument:wrongTypeInputArgument",
            L"Wrong type for input argument #%d.", 1);
    }
    Error(L"Nelson:editor:hookUnavailable", L"The text editor hook is unavailable.");
    return {};
}
//=============================================================================
