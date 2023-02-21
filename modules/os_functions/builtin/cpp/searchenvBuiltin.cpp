//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "searchenvBuiltin.hpp"
#include "Error.hpp"
#include "SearchVariableEnvironment.hpp"
#include "ToCellString.hpp"
#include "CheckerHelpers.hpp"
#include "PredefinedErrorMessages.hpp"
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
        fileToSearch = argIn[0].getContentAsWideString();
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    if (argIn.size() == 2) {
        varEnvName = argIn[1].getContentAsWideString();
    }
    wstringVector res = SearchVariableEnvironmentW(fileToSearch, varEnvName);
    retval << ToCellStringAsColumn(res);
    return retval;
}
//=============================================================================
