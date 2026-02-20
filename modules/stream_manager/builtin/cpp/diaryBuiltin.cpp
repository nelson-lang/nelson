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
#include "PredefinedErrorMessages.hpp"
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
                    raiseError(L"Nelson:stream:ERROR_DIARY_ERROR_USING_DIARY",
                        ERROR_DIARY_ERROR_USING_DIARY);
                }
            }
        } else {
            raiseError2(_E("nelson:validators:mustBeTypeAtPosition"), 1, NLS_STRING_ARRAY_STR);
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
                raiseError(
                    L"Nelson:stream:ERROR_DIARY_ARG1_GET_EXPECTED", ERROR_DIARY_ARG1_GET_EXPECTED);
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
                raiseError(L"Nelson:stream:ERROR_DIARY_ARG2_DIARY_OR_DIARYFILE_EXPECTED",
                    ERROR_DIARY_ARG2_DIARY_OR_DIARYFILE_EXPECTED);
            }
        } else {
            if (!argIn[0].isRowVectorCharacterArray()) {
                raiseError(
                    L"Nelson:stream:ERROR_DIARY_ARG1_GET_EXPECTED", ERROR_DIARY_ARG1_GET_EXPECTED);
            } else {
                raiseError(L"Nelson:stream:ERROR_DIARY_ARG2_DIARY_OR_DIARYFILE_EXPECTED",
                    ERROR_DIARY_ARG2_DIARY_OR_DIARYFILE_EXPECTED);
            }
        }
    } else if (argIn.size() == 3) {
        if ((argIn[0].isRowVectorCharacterArray()) && (argIn[1].isRowVectorCharacterArray())
            && (argIn[2].isRowVectorCharacterArray())) {
            std::wstring param1 = argIn[0].getContentAsWideString();
            if (param1.compare(L"set") != 0) {
                raiseError(
                    L"Nelson:stream:ERROR_DIARY_ARG1_SET_EXPECTED", ERROR_DIARY_ARG1_SET_EXPECTED);
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
                    raiseError(L"Nelson:stream:ERROR_DIARY_ARG3_ON_OFF_EXPECTED",
                        ERROR_DIARY_ARG3_ON_OFF_EXPECTED);
                }
            } else if (param2.compare(L"DiaryFile") == 0) {
                bool bRes = io->diary.SetFilename(param3);
                if (!bRes) {
                    raiseError(L"Nelson:stream:ERROR_DIARY_ERROR_USING_DIARY",
                        ERROR_DIARY_ERROR_USING_DIARY);
                }
            } else {
                raiseError(L"Nelson:stream:ERROR_DIARY_ARG2_DIARY_OR_DIARYFILE_EXPECTED",
                    ERROR_DIARY_ARG2_DIARY_OR_DIARYFILE_EXPECTED);
            }
        } else {
            if (!argIn[0].isRowVectorCharacterArray()) {
                raiseError(
                    L"Nelson:stream:ERROR_DIARY_ARG1_SET_EXPECTED", ERROR_DIARY_ARG1_SET_EXPECTED);
            } else if (!argIn[1].isRowVectorCharacterArray()) {
                raiseError(L"Nelson:stream:ERROR_DIARY_ARG2_DIARY_OR_DIARYFILE_EXPECTED",
                    ERROR_DIARY_ARG2_DIARY_OR_DIARYFILE_EXPECTED);
            } else {
                raiseError(L"Nelson:stream:ERROR_DIARY_ARG3_STRING_EXPECTED",
                    ERROR_DIARY_ARG3_STRING_EXPECTED);
            }
        }
    } else {
        raiseError2(_E("nelson:arguments:wrongNumberOfInputs"));
    }
    return retval;
}
//=============================================================================
