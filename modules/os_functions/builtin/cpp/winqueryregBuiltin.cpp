//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "winqueryregBuiltin.hpp"
#include "Error.hpp"
#include "WindowsQueryRegistry.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OsFunctionsGateway::winqueryregBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
#ifdef _MSC_VER
    if (argIn.size() < 2 || argIn.size() > 3) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    switch (argIn.size()) {
    case 2: {
        std::wstring rootkey = argIn[0].getContentAsWideString();
        std::wstring subkey = argIn[1].getContentAsWideString();
        if (rootkey == L"name") {
            Error(_W("'name' argument requires 3 input arguments."));
        }
        std::wstring errorMessage;
        ArrayOf res = windowsQueryRegistryValueName(rootkey, subkey, L"", errorMessage);
        if (!errorMessage.empty()) {
            Error(errorMessage);
        }
        retval.push_back(res);
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
            Error(errorMessage);
        }
        retval.push_back(res);
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
#else
    Error(_W("Not implemented on this platform."));
#endif
    return retval;
}
//=============================================================================
