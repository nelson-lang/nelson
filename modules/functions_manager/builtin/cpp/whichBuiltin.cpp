//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "whichBuiltin.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "Which.hpp"
#include "characters_encoding.hpp"
#include "NelsonPrint.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::whichBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 2);
    if (argIn.size() == 1) {
        if (argIn[0].isRowVectorCharacterArray()
            || (argIn[0].isStringArray() && argIn[0].isScalar())) {
            std::string functionname = argIn[0].asUtf8String();
            if (nLhs == 0) {
                FunctionDefPtr fptr = nullptr;
                bool found = BuiltInFunctionDefManager::getInstance()->find(functionname, fptr);
                std::wstring path = Which(functionname);
                if (found) {
                    NelsonPrint(_W("built-in") + L" (" + path + L")");
                } else {
                    if (path.empty()) {
                        NelsonPrint(L"'" + utf8_to_wstring(functionname) + L"' "
                            + Nelson::resolveMessageTemplate(
                                L"Nelson:functions_manager:notFound", L"not found."));
                    } else {
                        NelsonPrint(path);
                    }
                }
            } else {
                retval << ArrayOf::characterArray(Which(functionname));
            }
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"string");
        }
    } else {
        std::string functionname;
        if (argIn[0].isRowVectorCharacterArray()
            || (argIn[0].isStringArray() && argIn[0].isScalar())) {
            functionname = argIn[0].asUtf8String();
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"string");
        }
        std::wstring wparam2;
        if (argIn[1].isRowVectorCharacterArray()
            || (argIn[1].isStringArray() && argIn[1].isScalar())) {
            wparam2 = argIn[1].asWideString();
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
                L"string");
        }
        if (wparam2 == L"-all") {
            wstringVector res = WhichAll(functionname);
            if (nLhs == 0) {
                for (size_t k = 0; k < res.size(); k++) {
                    if (k == 0) {
                        NelsonPrint(res[k] + L"\n");
                    } else {
                        NelsonPrint(res[k] + L" % " + _W("Shadowed") + L"\n");
                    }
                }
            } else {
                retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(res);
            }
        } else if (wparam2 == L"-module") {
            wstringVector res = WhichModule(functionname);
            if (nLhs == 0) {
                for (const auto& re : res) {
                    NelsonPrint(re + L"\n");
                }
            } else {
                retval << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(res);
            }
        } else {
            Error(
                L"Nelson:which:invalidOption", L"#%d argument must be \'-all\' or \'-module\'.", 2);
        }
    }
    return retval;
}
//=============================================================================
