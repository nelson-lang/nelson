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
#include "whichBuiltin.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "Error.hpp"
#include "ToCellString.hpp"
#include "Which.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::whichBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if ((argIn.size() != 1) && (argIn.size() != 2)) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } else if (argIn.size() == 1) {
        if (argIn[0].isRowVectorCharacterArray()) {
            std::wstring wfunctionname = argIn[0].getContentAsWideString();
            if (nLhs == 0) {
                Interface* io = eval->getInterface();
                if (io) {
                    FuncPtr fptr = nullptr;
                    bool found = BuiltInFunctionDefManager::getInstance()->find(
                        wstring_to_utf8(wfunctionname), fptr);
                    std::wstring path = Which(wfunctionname);
                    if (found) {
                        io->outputMessage(_W("built-in") + L" (" + path + L")");
                    } else {
                        if (path == L"") {
                            io->outputMessage(L"'" + wfunctionname + L"' " + _W("not found."));
                        } else {
                            io->outputMessage(path);
                        }
                    }
                }
            } else {
                retval.push_back(ArrayOf::characterArrayConstructor(Which(wfunctionname)));
            }
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
    } else {
        // case argIn.size() == 2
        std::wstring wfunctionname;
        if (argIn[0].isRowVectorCharacterArray()) {
            wfunctionname = argIn[0].getContentAsWideString();
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        std::wstring wparam2;
        if (argIn[1].isRowVectorCharacterArray()) {
            wparam2 = argIn[1].getContentAsWideString();
        } else {
            Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
        }
        if (wparam2.compare(L"-all") == 0) {
            wstringVector res = WhichAll(wfunctionname);
            if (nLhs == 0) {
                Interface* io = eval->getInterface();
                if (io) {
                    for (size_t k = 0; k < res.size(); k++) {
                        if (k == 0) {
                            io->outputMessage(res[k] + L"\n");
                        } else {
                            io->outputMessage(res[k] + L" % " + _W("Shadowed") + L"\n");
                        }
                    }
                } else {
                    retval.push_back(ToCellStringAsColumn(res));
                }
            } else {
                retval.push_back(ToCellStringAsColumn(res));
            }
        } else if (wparam2.compare(L"-module") == 0) {
            wstringVector res = WhichModule(wfunctionname);
            if (nLhs == 0) {
                Interface* io = eval->getInterface();
                if (io) {
                    for (size_t k = 0; k < res.size(); k++) {
                        io->outputMessage(res[k] + L"\n");
                    }
                } else {
                    retval.push_back(ToCellStringAsColumn(res));
                }
            } else {
                retval.push_back(ToCellStringAsColumn(res));
            }
        } else {
            Error(_W("#2 Argument must be \'-all\' or  \'-module\'."));
        }
    }
    return retval;
}
//=============================================================================
