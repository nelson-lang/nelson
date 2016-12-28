//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include "ParserInterface.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::InterpreterGateway::parsestringBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() != 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    std::string command;
    if (argIn[0].isSingleString())
    {
        command = argIn[0].getContentsAsCString();
    }
    else
    {
        Error(eval, ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    ParserState parserState = ParseError;
    Exception previousException(eval->getLastException());
    try
    {
        parserState = parseString(command + "\n");
    }
    catch (const Exception &e)
    {
        parserState = ParseError;
        eval->setLastException(previousException);
    }
    switch (parserState)
    {
        case ScriptBlock:
        {
            retval.push_back(ArrayOf::stringConstructor("script"));
        }
        break;
        case FuncDef:
        {
            MacroFunctionDef *cp = getParsedFunctionDef();
            if (cp)
            {
                retval.push_back(ArrayOf::stringConstructor("function"));
            }
            else
            {
                retval.push_back(ArrayOf::stringConstructor("script"));
            }
        }
        break;
        default:
        {
            retval.push_back(ArrayOf::stringConstructor("error"));
        }
        break;
    }
    return retval;
}
//=============================================================================
