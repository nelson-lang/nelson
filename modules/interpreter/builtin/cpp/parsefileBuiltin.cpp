//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
//=============================================================================
#include "parsefileBuiltin.hpp"
#include "Error.hpp"
#include "ParseFile.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::InterpreterGateway::parsefileBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    std::wstring filename;
    if (argIn[0].isRowVectorCharacterArray()) {
        filename = argIn[0].getContentAsWideString();
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    ParserState parserState = ParseFile(eval, filename);
    switch (parserState) {
    case ScriptBlock: {
        retval << ArrayOf::characterArrayConstructor("script");
    } break;
    case FuncDef: {
        retval << ArrayOf::characterArrayConstructor("function");
    } break;
    default: {
        retval << ArrayOf::characterArrayConstructor("error");
    } break;
    }
    return retval;
}
//=============================================================================
