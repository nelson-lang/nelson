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
#include "parsestringBuiltin.hpp"
#include "Error.hpp"
#include "ParserInterface.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::InterpreterGateway::parsestringBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() != 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    std::string command;
    if (argIn[0].isRowVectorCharacterArray()) {
        command = argIn[0].getContentAsCString();
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    ParserState parserState = ParseError;
    Exception previousException(eval->getLastErrorException());
    try {
        parserState = parseString(command + "\n");
    } catch (const Exception&) {
        parserState = ParseError;
        eval->setLastErrorException(previousException);
    }
    switch (parserState) {
    case ScriptBlock: {
        retval.push_back(ArrayOf::characterArrayConstructor("script"));
    } break;
    case FuncDef: {
        MacroFunctionDef* cp = getParsedFunctionDef();
        if (cp) {
            retval.push_back(ArrayOf::characterArrayConstructor("function"));
        } else {
            retval.push_back(ArrayOf::characterArrayConstructor("script"));
        }
    } break;
    default: {
        retval.push_back(ArrayOf::characterArrayConstructor("error"));
    } break;
    }
    return retval;
}
//=============================================================================
