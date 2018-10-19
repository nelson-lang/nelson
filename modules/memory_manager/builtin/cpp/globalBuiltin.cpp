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
#include "globalBuiltin.hpp"
#include "Error.hpp"
#include "IsValidVariableName.hpp"
#include "StringFormat.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MemoryGateway::globalBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs != 0) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    Context* context = eval->getContext();
    for (size_t k = 0; k < argIn.size(); k++) {
        if (!argIn[k].isRowVectorCharacterArray()) {
            Error(StringFormat(ERROR_WRONG_ARGUMENT_X_TYPE_STRING_EXPECTED.c_str(), k + 1));
        }
        std::string arg = argIn[k].getContentAsCString();
        if (!IsValidVariableName(arg)) {
            Error(_W("Argument must contain a valid variable name."));
        }
        if (context->isLockedVariable(arg)) {
            Error(_W("variable is locked."));
        }
    }
    for (size_t k = 0; k < argIn.size(); k++) {
        std::string arg = argIn[k].getContentAsCString();
        context->addGlobalVariable(arg);
    }
    return retval;
}
//=============================================================================
