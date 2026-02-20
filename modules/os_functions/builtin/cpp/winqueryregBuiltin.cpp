//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "winqueryregBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
#include "WindowsQueryRegistry.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OsFunctionsGateway::winqueryregBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
#ifdef _MSC_VER
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    switch (argIn.size()) {
    case 2: {
        std::wstring rootkey = argIn[0].getContentAsWideString();
        std::wstring subkey = argIn[1].getContentAsWideString();
        if (rootkey == L"name") {
            raiseError(L"Nelson:os_functions:ERROR_NAME_ARGUMENT_REQUIRES_3_INPUT_ARGUMENTS",
                ERROR_NAME_ARGUMENT_REQUIRES_3_INPUT_ARGUMENTS);
        }
        std::wstring errorMessage;
        ArrayOf res = windowsQueryRegistryValueName(rootkey, subkey, L"", errorMessage);
        if (!errorMessage.empty()) {
            Error(errorMessage, L"Nelson:os_functions:winqueryreg");
        }
        retval << res;
    } break;
    case 3: {
        std::wstring errorMessage;
        ArrayOf res;
        std::wstring rootkey = argIn[0].getContentAsWideString();
        if (rootkey == L"name") {
            rootkey = argIn[1].getContentAsWideString();
            std::wstring subkey = argIn[2].getContentAsWideString();
            res = windowsQueryRegistryAllValuesNames(rootkey, subkey, errorMessage);
        } else {
            std::wstring subkey = argIn[1].getContentAsWideString();
            std::wstring valname = argIn[2].getContentAsWideString();
            res = windowsQueryRegistryValueName(rootkey, subkey, valname, errorMessage);
        }
        if (!errorMessage.empty()) {
            Error(errorMessage, L"Nelson:os_functions:winqueryreg");
        }
        retval << res;
    } break;
    default: {
        raiseError2(_E("nelson:arguments:wrongNumberOfInputs"));
    } break;
    }
#else
    raiseError(L"Nelson:os_functions:ERROR_NOT_IMPLEMENTED_ON_PLATFORM",
        ERROR_NOT_IMPLEMENTED_ON_PLATFORM);
#endif
    return retval;
}
//=============================================================================
