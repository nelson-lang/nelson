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
#include "varlockBuiltin.hpp"
#include "Error.hpp"
#include "IsValidVariableName.hpp"
#include "LockVariable.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MemoryGateway::varlockBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 2, 2);
    if (!argIn[0].isRowVectorCharacterArray()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    std::string scopename = argIn[0].getContentAsCString();
    if (!((scopename == "global") || (scopename == "base") || (scopename == "caller")
            || (scopename == "local"))) {
        Error(_W("#1 Argument must contain a string: \'global\', \'base\', \'local\' or \'caller\' "
                 "expected."));
    }
    Context* context = eval->getContext();
    Scope* scope = nullptr;
    if (scopename == "global") {
        scope = context->getGlobalScope();
    }
    if (scopename == "base") {
        scope = context->getBaseScope();
    }
    if (scopename == "caller") {
        scope = context->getCallerScope();
    }
    if (scopename == "local") {
        scope = context->getCurrentScope();
    }
    if (!argIn[1].isRowVectorCharacterArray()) {
        Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
    }
    std::string varname = argIn[1].getContentAsCString();
    if (!IsValidVariableName(varname)) {
        Error(_W("#2 Argument must contain a valid variable name."));
    }
    if ((scope != nullptr) && !scope->isVariable(varname)) {
        Error(_W("#2 Argument must be an existing variable name."));
    }
    LockVariable(varname, scope);
    return retval;
}
//=============================================================================
