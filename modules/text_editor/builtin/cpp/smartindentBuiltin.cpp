//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "smartindentBuiltin.hpp"
#include "Error.hpp"
#include "SmartIndent.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TextEditorGateway::smartindentBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    std::wstring filename;
    indexType tabSize = 2;
    logical doBackup = false;
    switch (argIn.size()) {
    case 3: {
        doBackup = argIn[2].getContentAsLogicalScalar();
    }
    case 2: {
        ArrayOf param1 = argIn[1];
        tabSize = param1.getContentAsScalarIndex(false);
    }
    case 1: {
        filename = argIn[0].getContentAsWideString();
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    smartIndent(filename, (int)tabSize, doBackup ? true : false);
    return retval;
}
//=============================================================================
