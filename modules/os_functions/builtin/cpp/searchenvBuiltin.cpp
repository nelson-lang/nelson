//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "searchenvBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "SearchVariableEnvironment.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OsFunctionsGateway::searchenvBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    std::wstring varEnvName;
    std::wstring fileToSearch;
    if (argIn[0].isRowVectorCharacterArray()) {
        fileToSearch = argIn[0].asWideString();
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    if (argIn.size() == 2) {
        varEnvName = argIn[1].asWideString();
    }
    wstringVector res = SearchVariableEnvironmentW(fileToSearch, varEnvName);
    retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(res);
    return retval;
}
//=============================================================================
