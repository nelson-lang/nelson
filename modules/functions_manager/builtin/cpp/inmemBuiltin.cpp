//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "inmemBuiltin.hpp"
#include "Error.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "MacroFunctionDef.hpp"
#include "MexFunctionDef.hpp"
#include "characters_encoding.hpp"
#include "FunctionsInMemory.hpp"
#include "InputOutputArgumentsCheckers.hpp"
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
    retval << ArrayOf::toCellArrayOfCharacterColumnVectors(mFunctions);
    if (nLhs > 1) {
        wstringVector mexFunctions
            = FunctionsInMemory::getInstance()->getMexInMemory(withCompleteNames);
        retval << ArrayOf::toCellArrayOfCharacterColumnVectors(mexFunctions);
    }
    if (nLhs > 2) {
        retval << ArrayOf::toCellArrayOfCharacterColumnVectors(stringVector());
    }
    return retval;
}
//=============================================================================
