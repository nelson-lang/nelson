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
            std::wstring param = argIn[0].getContentAsWideString();
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
                    Error(_W("Error using diary."));
                }
            }
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
    } else if (argIn.empty()) {
        nargoutcheck(nLhs, 0, 0);
        io->diary.toggle();
    } else if (argIn.size() == 2) {
        // get
        if ((argIn[0].isRowVectorCharacterArray()) && (argIn[1].isRowVectorCharacterArray())) {
            nargoutcheck(nLhs, 0, 1);
            std::wstring param1 = argIn[0].getContentAsWideString();
            if (param1.compare(L"get") != 0) {
                Error(_W("#1 Argument \'get\' expected."));
            }
            std::wstring param2 = argIn[1].getContentAsWideString();
            if (param2.compare(L"Diary") == 0) {
                if (io->diary.getState()) {
                    retval << ArrayOf::characterArrayConstructor("on");
                } else {
                    retval << ArrayOf::characterArrayConstructor("off");
                }
            } else if (param2.compare(L"DiaryFile") == 0) {
                retval << ArrayOf::characterArrayConstructor(io->diary.getFilename());
            } else {
                Error(_W("#2 Argument \'Diary\' or \'DiaryFile\' expected."));
            }
        } else {
            if (!argIn[0].isRowVectorCharacterArray()) {
                Error(_W("#1 Argument \'get\' expected."));
            } else {
                Error(_W("#2 Argument \'Diary\' or \'DiaryFile\' expected."));
            }
        }
    } else if (argIn.size() == 3) {
        if ((argIn[0].isRowVectorCharacterArray()) && (argIn[1].isRowVectorCharacterArray())
            && (argIn[2].isRowVectorCharacterArray())) {
            std::wstring param1 = argIn[0].getContentAsWideString();
            if (param1.compare(L"set") != 0) {
                Error(_W("#1 Argument \'set\' expected."));
            }
            std::wstring param3 = argIn[2].getContentAsWideString();
            std::wstring param2 = argIn[1].getContentAsWideString();
            if (param2.compare(L"Diary") == 0) {
                if ((param3.compare(L"off") == 0) || (param3.compare(L"on") == 0)) {
                    if (param3.compare(L"off") == 0) {
                        io->diary.setState(false);
                    } else {
                        io->diary.setState(true);
                    }
                } else {
                    Error(_W("#3 Argument \'on\' or \'off\' expected."));
                }
            } else if (param2.compare(L"DiaryFile") == 0) {
                bool bRes = io->diary.SetFilename(param3);
                if (!bRes) {
                    Error(_W("Error using diary."));
                }
            } else {
                Error(_W("#2 Argument \'Diary\' or \'DiaryFile\' expected."));
            }
        } else {
            if (!argIn[0].isRowVectorCharacterArray()) {
                Error(_W("#1 Argument \'set\' expected."));
            } else if (!argIn[1].isRowVectorCharacterArray()) {
                Error(_W("#2 Argument \'Diary\' or \'DiaryFile\' expected."));
            } else {
                Error(_W("#3 Argument a string expected."));
            }
        }
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
