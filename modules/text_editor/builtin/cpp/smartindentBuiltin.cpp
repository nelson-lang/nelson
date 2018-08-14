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
#include "smartindentBuiltin.hpp"
#include "Error.hpp"
#include "SmartIndent.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TextEditorGateway::smartindentBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs != 0) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    std::wstring filename;
    indexType tabSize = 2;
    logical doBackup = false;
    switch (argIn.size()) {
    case 3: {
        doBackup = argIn[2].getContentAsLogicalScalar();
    }
    case 2: {
        ArrayOf param1 = argIn[1];
        tabSize = param1.getContentAsScalarIndex(false);
    }
    case 1: {
        filename = argIn[0].getContentAsWideString();
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    smartIndent(filename, (int)tabSize, doBackup ? true : false);
    return retval;
}
//=============================================================================
