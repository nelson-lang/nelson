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
#include "echoBuiltin.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::echoBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOfVector retval;
    if (argIn.size() == 0) {
        bool toggle = eval->getEchoMode();
        if (nLhs == 0) {
            eval->setEchoMode(!toggle);
        } else {
            if (toggle) {
                retval.push_back(ArrayOf::characterArrayConstructor("on"));
            } else {
                retval.push_back(ArrayOf::characterArrayConstructor("off"));
            }
        }
    } else if (argIn.size() == 1) {
        if (argIn[0].isRowVectorCharacterArray()) {
            bool bMode = false;
            std::wstring arg = argIn[0].getContentAsWideString();
            if (arg.compare(L"on") == 0) {
                bMode = true;
            } else if (arg.compare(L"off") == 0) {
                bMode = false;
            } else {
                Error(ERROR_WRONG_ARGUMENT_1_VALUE);
            }
            eval->setEchoMode(bMode);
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
