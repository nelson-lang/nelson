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
#include "evalinBuiltin.hpp"
#include "Error.hpp"
#include "EvaluateCommand.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::evalinBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (argIn.size() != 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    SCOPE_LEVEL scope;
    std::wstring command;
    std::wstring catchCommand;
    if (argIn[0].isRowVectorCharacterArray()) {
        std::wstring scopeName = argIn[0].getContentAsWideString();
        if (scopeName == L"caller" || scopeName == L"base" || scopeName == L"local") {
            if (scopeName == L"caller") {
                scope = SCOPE_LEVEL::CALLER_SCOPE;
            }
            if (scopeName == L"base") {
                scope = SCOPE_LEVEL::BASE_SCOPE;
            }
            if (scopeName == L"local") {
                scope = SCOPE_LEVEL::LOCAL_SCOPE;
            }
        } else {
            Error(_W("Argument #1 : 'base', 'local' or 'caller' expected."));
        }
    } else {
        Error(_W("#1 string expected."));
    }
    if (argIn[1].isRowVectorCharacterArray()) {
        command = argIn[1].getContentAsWideString();
    } else {
        Error(_W("#2 string expected."));
    }
    return EvaluateInCommand(eval, nLhs, scope, command);
}
//=============================================================================
