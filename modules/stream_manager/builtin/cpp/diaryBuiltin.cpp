//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "diaryBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "Interface.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::diaryBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    Interface* io = eval->getInterface();
    if (argIn.size() == 1) {
        if (argIn[0].isRowVectorCharacterArray()) {
            std::wstring param = argIn[0].asWideString();
            if (param.compare(L"on") == 0) {
                nargoutcheck(nLhs, 0);
                io->diary.setState(true);
            } else if (param.compare(L"off") == 0) {
                nargoutcheck(nLhs, 0);
                io->diary.setState(false);
            } else {
                nargoutcheck(nLhs, 0);
                bool bRes = io->diary.SetFilename(param);
                if (!bRes) {
                    Error(L"Nelson:diary:diaryError", L"Error using diary.");
                }
            }
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"string");
        }
    } else if (argIn.empty()) {
        nargoutcheck(nLhs, 0, 0);
        io->diary.toggle();
    } else if (argIn.size() == 2) {
        if ((argIn[0].isRowVectorCharacterArray()) && (argIn[1].isRowVectorCharacterArray())) {
            nargoutcheck(nLhs, 0, 1);
            std::wstring param1 = argIn[0].asWideString();
            if (param1.compare(L"get") != 0) {
                Error(L"Nelson:diary:invalidArgument", L"#%d argument \'get\' expected.", 1);
            }
            std::wstring param2 = argIn[1].asWideString();
            if (param2.compare(L"Diary") == 0) {
                if (io->diary.getState()) {
                    retval << ArrayOf::characterArray("on");
                } else {
                    retval << ArrayOf::characterArray("off");
                }
            } else if (param2.compare(L"DiaryFile") == 0) {
                retval << ArrayOf::characterArray(io->diary.getFilename());
            } else {
                Error(L"Nelson:diary:invalidArgument:nArgumentDiaryDiaryFileExpected",
                    L"#%d argument \'Diary\' or \'DiaryFile\' expected.", 2);
            }
        } else {
            if (!argIn[0].isRowVectorCharacterArray()) {
                Error(L"Nelson:diary:invalidArgument", L"#%d argument \'get\' expected.", 1);
            } else {
                Error(L"Nelson:diary:invalidArgument:nArgumentDiaryDiaryFileExpected",
                    L"#%d argument \'Diary\' or \'DiaryFile\' expected.", 2);
            }
        }
    } else if (argIn.size() == 3) {
        if ((argIn[0].isRowVectorCharacterArray()) && (argIn[1].isRowVectorCharacterArray())
            && (argIn[2].isRowVectorCharacterArray())) {
            std::wstring param1 = argIn[0].asWideString();
            if (param1.compare(L"set") != 0) {
                Error(L"Nelson:diary:invalidArgument:nArgumentSetExpected",
                    L"#%d argument \'set\' expected.", 1);
            }
            std::wstring param3 = argIn[2].asWideString();
            std::wstring param2 = argIn[1].asWideString();
            if (param2.compare(L"Diary") == 0) {
                if ((param3.compare(L"off") == 0) || (param3.compare(L"on") == 0)) {
                    if (param3.compare(L"off") == 0) {
                        io->diary.setState(false);
                    } else {
                        io->diary.setState(true);
                    }
                } else {
                    Error(L"Nelson:diary:invalidArgument:nArgumentOffExpected",
                        L"#%d argument \'on\' or \'off\' expected.", 3);
                }
            } else if (param2.compare(L"DiaryFile") == 0) {
                bool bRes = io->diary.SetFilename(param3);
                if (!bRes) {
                    Error(L"Nelson:diary:diaryError", L"Error using diary.");
                }
            } else {
                Error(L"Nelson:diary:invalidArgument:nArgumentDiaryDiaryFileExpected",
                    L"#%d argument \'Diary\' or \'DiaryFile\' expected.", 2);
            }
        } else {
            if (!argIn[0].isRowVectorCharacterArray()) {
                Error(L"Nelson:diary:invalidArgument:nArgumentSetExpected",
                    L"#%d argument \'set\' expected.", 1);
            } else if (!argIn[1].isRowVectorCharacterArray()) {
                Error(L"Nelson:diary:invalidArgument:nArgumentDiaryDiaryFileExpected",
                    L"#%d argument \'Diary\' or \'DiaryFile\' expected.", 2);
            } else {
                Error(L"Nelson:diary:invalidArgument:nArgumentAStringExpected",
                    L"#%d argument a string expected.", 3);
            }
        }
    } else {
        Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
    }
    return retval;
}
//=============================================================================
