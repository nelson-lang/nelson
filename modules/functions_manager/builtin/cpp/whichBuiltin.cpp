//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "whichBuiltin.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "ToCellString.hpp"
#include "Which.hpp"
#include "characters_encoding.hpp"
#include "NelsonPrint.hpp"
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
            std::wstring wfunctionname = argIn[0].getContentAsWideString();
            if (nLhs == 0) {
                FunctionDefPtr fptr = nullptr;
                bool found = BuiltInFunctionDefManager::getInstance()->find(
                    wstring_to_utf8(wfunctionname), fptr);
                std::wstring path = Which(wfunctionname);
                if (found) {
                    NelsonPrint(_W("built-in") + L" (" + path + L")");
                } else {
                    if (path.empty()) {
                        NelsonPrint(L"'" + wfunctionname + L"' " + _W("not found."));
                    } else {
                        NelsonPrint(path);
                    }
                }
            } else {
                retval << ArrayOf::characterArrayConstructor(Which(wfunctionname));
            }
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
    } else {
        // case argIn.size() == 2
        std::wstring wfunctionname;
        if (argIn[0].isRowVectorCharacterArray()
            || (argIn[0].isStringArray() && argIn[0].isScalar())) {
            wfunctionname = argIn[0].getContentAsWideString();
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        std::wstring wparam2;
        if (argIn[1].isRowVectorCharacterArray()
            || (argIn[1].isStringArray() && argIn[1].isScalar())) {
            wparam2 = argIn[1].getContentAsWideString();
        } else {
            Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
        }
        if (wparam2 == L"-all") {
            wstringVector res = WhichAll(wfunctionname);
            if (nLhs == 0) {
                for (size_t k = 0; k < res.size(); k++) {
                    if (k == 0) {
                        NelsonPrint(res[k] + L"\n");
                    } else {
                        NelsonPrint(res[k] + L" % " + _W("Shadowed") + L"\n");
                    }
                }
            } else {
                retval << ToCellStringAsColumn(res);
            }
        } else if (wparam2 == L"-module") {
            wstringVector res = WhichModule(wfunctionname);
            if (nLhs == 0) {
                for (const auto& re : res) {
                    NelsonPrint(re + L"\n");
                }
            } else {
                retval << ToCellStringAsColumn(res);
            }
        } else {
            Error(_W("#2 Argument must be \'-all\' or  \'-module\'."));
        }
    }
    return retval;
}
//=============================================================================
