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
#include "diaryBuiltin.hpp"
#include "Error.hpp"
#include "Interface.hpp"
#include "OverloadFunction.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::diaryBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    // Call overload if it exists
    if (argIn.size() >= 1) {
        bool bSuccess = false;
        if (eval->mustOverloadBasicTypes()) {
            retval = OverloadFunction(eval, nLhs, argIn, "diary", bSuccess);
            if (bSuccess) {
                return retval;
            }
        }
    }
    Interface* io = eval->getInterface();
    if (argIn.size() == 1) {
        if (argIn[0].isRowVectorCharacterArray()) {
            std::wstring param = argIn[0].getContentAsWideString();
            if (param.compare(L"on") == 0) {
                if (nLhs != 0) {
                    Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
                }
                io->diary.setState(true);
            } else if (param.compare(L"off") == 0) {
                if (nLhs != 0) {
                    Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
                }
                io->diary.setState(false);
            } else {
                if (nLhs != 0) {
                    Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
                }
                bool bRes = io->diary.SetFilename(param);
                if (!bRes) {
                    Error(_W("Error using diary."));
                }
            }
        } else {
            bool bSuccess = false;
            retval = OverloadFunction(eval, nLhs, argIn, "diary", bSuccess);
            if (!bSuccess) {
                Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
            }
            return retval;
        }
    } else if (argIn.size() == 0) {
        if (nLhs != 0) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        io->diary.toggle();
    } else if (argIn.size() == 2) {
        // get
        if ((argIn[0].isRowVectorCharacterArray()) && (argIn[1].isRowVectorCharacterArray())) {
            bool bLhs = (nLhs == 0) || (nLhs == 1);
            if (!bLhs) {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            }
            std::wstring param1 = argIn[0].getContentAsWideString();
            if (param1.compare(L"get") != 0) {
                Error(_W("#1 Argument \'get\' expected."));
            }
            std::wstring param2 = argIn[1].getContentAsWideString();
            if (param2.compare(L"Diary") == 0) {
                if (io->diary.getState()) {
                    retval.push_back(ArrayOf::characterArrayConstructor("on"));
                } else {
                    retval.push_back(ArrayOf::characterArrayConstructor("off"));
                }
            } else if (param2.compare(L"DiaryFile") == 0) {
                retval.push_back(ArrayOf::characterArrayConstructor(io->diary.getFilename()));
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
