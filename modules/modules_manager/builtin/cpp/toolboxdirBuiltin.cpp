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
#include "toolboxdirBuiltin.hpp"
#include "Error.hpp"
#include "ModulePath.hpp"
#include "ModulesHelpers.hpp"
#include "ModulesManager.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ModulesManagerGateway::toolboxdirBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    std::wstring moduleshortname;
    if (argIn[0].isRowVectorCharacterArray()) {
        moduleshortname = argIn[0].getContentAsWideString();
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    if (IsExistingModuleName(moduleshortname)) {
        retval.push_back(ArrayOf::characterArrayConstructor(GetModulePath(moduleshortname)));
    } else {
        Error(_W("invalid module name."));
    }
    return retval;
}
//=============================================================================
