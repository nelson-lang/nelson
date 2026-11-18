//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "whatBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "What.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::whatBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 2);
    nargincheck(argIn, 0, 1);
    if (argIn.size() == 1) {
        ArrayOf param1 = argIn[0];
        if (!param1.isRowVectorCharacterArray() && !(param1.isStringArray() && param1.isScalar())) {
            Error(_W("Wrong type for argument #1: a module name (a string) is expected."));
        }
        std::wstring moduleName = param1.asWideString();
        if (!WhatIsModule(moduleName)) {
            Error(_W("Unknown module name:") + L" '" + moduleName + L"'.");
        }
        retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(
            WhatListOfBuiltin(eval, moduleName));
        if (nLhs == 2) {
            retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(
                WhatListOfMacro(eval, moduleName));
        }
        return retval;
    }
    retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(WhatListOfBuiltin(eval));
    if (nLhs == 2) {
        retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(WhatListOfMacro(eval));
    }
    return retval;
}
//=============================================================================
