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
#include "overloadbasictypesBuiltin.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OverloadGateway::overloadbasictypesBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() > 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    bool previousValue = eval->mustOverloadBasicTypes();
    if (argIn.size() == 1) {
        bool newValue = argIn[0].getContentAsLogicalScalar();
        if (newValue) {
            eval->enableOverloadBasicTypes();
        } else {
            eval->disableOverloadBasicTypes();
        }
        if (nLhs > 0) {
            retval.push_back(ArrayOf::logicalConstructor(previousValue));
        }
    }
    retval.push_back(ArrayOf::logicalConstructor(previousValue));
    return retval;
}
//=============================================================================
