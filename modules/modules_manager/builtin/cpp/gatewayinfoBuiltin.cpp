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
#include "ErrorID.hpp"
#include "GatewayInfo.hpp"
#include "GatewaysManager.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ModulesManagerGateway::gatewayinfoBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 3);
    if (argIn[0].isRowVectorCharacterArray()) {
        std::wstring dynlibName = argIn[0].asWideString();
        std::wstring moduleName;
        stringVector builtinList;
        std::wstring errorMessage;
        // a lazy gateway is described by the cache: do not load its library
        // (and its dependencies) just to answer the query.
        bool bRes = GatewaysManager::getInstance()->getLazyGatewayInfo(
            dynlibName, moduleName, builtinList);
        if (!bRes) {
            bRes = GatewayInfo(dynlibName, moduleName, builtinList, errorMessage);
        }
        if (bRes) {
            retval << ArrayOf::characterArray(moduleName);
            retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(builtinList);
            if (nLhs > 2) {
                std::wstring state
                    = GatewaysManager::getInstance()->getGatewayState(dynlibName, errorMessage);
                if (state.empty()) {
                    Error(L"Nelson:gatewayinfo:invalidGateway", errorMessage);
                }
                retval << ArrayOf::characterArray(state);
            }
        } else {
            Error(L"Nelson:gatewayinfo:invalidGateway", errorMessage);
        }
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    return retval;
}
//=============================================================================
