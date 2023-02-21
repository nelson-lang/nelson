//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "inmemBuiltin.hpp"
#include "Error.hpp"
#include "ToCellString.hpp"
#include "PathFuncManager.hpp"
#include "MacroFunctionDef.hpp"
#include "MexFunctionDef.hpp"
#include "characters_encoding.hpp"
#include "FunctionsInMemory.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::inmemBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 2);
    nargincheck(argIn, 0, 1);

    bool withCompleteNames = false;
    if (argIn.size() == 1) {
        std::wstring param1 = argIn[0].getContentAsWideString();
        if (param1 == L"-completenames") {
            withCompleteNames = true;
        }
    }

    wstringVector mFunctions
        = FunctionsInMemory::getInstance()->getMacroInMemory(withCompleteNames);
    retval << ToCellStringAsColumn(mFunctions);
    if (nLhs > 1) {
        wstringVector mexFunctions
            = FunctionsInMemory::getInstance()->getMexInMemory(withCompleteNames);
        retval << ToCellStringAsColumn(mexFunctions);
    }
    if (nLhs > 2) {
        retval << ToCellStringAsColumn(stringVector());
    }
    return retval;
}
//=============================================================================
