//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "gatewayinfoBuiltin.hpp"
#include "Error.hpp"
#include "GatewayInfo.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ModulesManagerGateway::gatewayinfoBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 2);
    if (argIn[0].isRowVectorCharacterArray()) {
        std::wstring dynlibName = argIn[0].getContentAsWideString();
        std::wstring moduleName;
        stringVector builtinList;
        std::wstring errorMessage;
        bool bRes = GatewayInfo(dynlibName, moduleName, builtinList, errorMessage);
        if (bRes) {
            retval << ArrayOf::characterArrayConstructor(moduleName);
            retval << ArrayOf::toCellArrayOfCharacterColumnVectors(builtinList);
        } else {
            Error(errorMessage);
        }
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    return retval;
}
//=============================================================================
