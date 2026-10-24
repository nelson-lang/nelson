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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "WindowsQueryRegistry.hpp"
#include "InputOutputArgumentsCheckers.hpp"
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
        std::wstring rootkey = argIn[0].asWideString();
        std::wstring subkey = argIn[1].asWideString();
        if (rootkey == L"name") {
            Error(L"Nelson:winqueryreg:invalidArgument",
                L"'name' argument requires 3 input arguments.");
        }
        std::wstring errorMessage;
        ArrayOf res = windowsQueryRegistryValueName(rootkey, subkey, L"", errorMessage);
        if (!errorMessage.empty()) {
            Error(L"Nelson:winqueryreg:queryFailed", errorMessage);
        }
        retval << res;
    } break;
    case 3: {
        std::wstring errorMessage;
        ArrayOf res;
        std::wstring rootkey = argIn[0].asWideString();
        if (rootkey == L"name") {
            rootkey = argIn[1].asWideString();
            std::wstring subkey = argIn[2].asWideString();
            res = windowsQueryRegistryAllValuesNames(rootkey, subkey, errorMessage);
        } else {
            std::wstring subkey = argIn[1].asWideString();
            std::wstring valname = argIn[2].asWideString();
            res = windowsQueryRegistryValueName(rootkey, subkey, valname, errorMessage);
        }
        if (!errorMessage.empty()) {
            Error(L"Nelson:winqueryreg:queryFailed", errorMessage);
        }
        retval << res;
    } break;
    default:
        break;
    }
#else
    Error(L"Nelson:os_functions:notImplemented", L"Not implemented on this platform.");
#endif
    return retval;
}
//=============================================================================
