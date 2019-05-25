//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
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
#include "iskeywordBuiltin.hpp"
#include "Error.hpp"
#include "Keywords.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::InterpreterGateway::iskeywordBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() == 0) {
        wstringVector keys = GetKeywords(true);
        ArrayOf* elements = new_with_exception<ArrayOf>(keys.size());
        for (size_t k = 0; k < keys.size(); k++) {
            elements[k] = ArrayOf::characterArrayConstructor(keys[k]);
        }
        Dimensions dims(keys.size(), 1);
        ArrayOf c = ArrayOf(NLS_CELL_ARRAY, dims, elements);
        retval.push_back(c);
    } else {
        if (!argIn[0].isRowVectorCharacterArray()) {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        std::wstring warg = argIn[0].getContentAsWideString();
        retval.push_back(ArrayOf::logicalConstructor(isKeyword(warg)));
    }
    return retval;
}
//=============================================================================
