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
#include "iskeywordBuiltin.hpp"
#include "Error.hpp"
#include "Keywords.hpp"
#include "Exception.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::InterpreterGateway::iskeywordBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    if (argIn.size() == 0) {
        wstringVector keys = GetKeywords(true);
        ArrayOf* elements = new_with_exception<ArrayOf>(keys.size(), false);
        for (size_t k = 0; k < keys.size(); k++) {
            elements[k] = ArrayOf::characterArrayConstructor(keys[k]);
        }
        Dimensions dims(keys.size(), 1);
        retval << ArrayOf(NLS_CELL_ARRAY, dims, elements);
    } else {
        if (!argIn[0].isRowVectorCharacterArray()) {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        std::wstring warg = argIn[0].getContentAsWideString();
        retval << ArrayOf::logicalConstructor(isKeyword(warg));
    }
    return retval;
}
//=============================================================================
