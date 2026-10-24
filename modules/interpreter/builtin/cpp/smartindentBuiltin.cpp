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
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "NelsonCodeIndent.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::InterpreterGateway::smartindentBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    std::wstring filename;
    indexType tabSize = 2;
    logical doBackup = false;
    logical fullFormat = true;
    nargincheck(argIn, 1, 4);
    switch (argIn.size()) {
    case 4: {
        fullFormat = argIn[3].asLogicalScalar();
    }
    case 3: {
        doBackup = argIn[2].asLogicalScalar();
    }
    case 2: {
        ArrayOf param1 = argIn[1];
        tabSize = param1.asScalarIndex(false);
    }
    case 1: {
        filename = argIn[0].asWideString();
    } break;
    }
    smartIndentFile(filename, (int)tabSize, doBackup ? true : false, fullFormat ? true : false);
    return retval;
}
//=============================================================================
