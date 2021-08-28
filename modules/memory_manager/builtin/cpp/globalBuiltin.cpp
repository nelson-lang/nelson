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
#include <fmt/printf.h>
#include <fmt/format.h>
#include "globalBuiltin.hpp"
#include "Error.hpp"
#include "IsValidVariableName.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MemoryGateway::globalBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    Context* context = eval->getContext();
    for (size_t k = 0; k < argIn.size(); k++) {
        if (!argIn[k].isRowVectorCharacterArray()) {
            Error(fmt::sprintf(ERROR_WRONG_ARGUMENT_X_TYPE_STRING_EXPECTED, k + 1));
        }
        std::string arg = argIn[k].getContentAsCString();
        if (!IsValidVariableName(arg)) {
            Error(_W("Argument must contain a valid variable name."));
        }
        if (context->isLockedVariable(arg)) {
            Error(_W("variable is locked."));
        }
    }
    for (const auto& k : argIn) {
        std::string arg = k.getContentAsCString();
        context->addGlobalVariable(arg);
    }
    return retval;
}
//=============================================================================
