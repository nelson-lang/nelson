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
#include "Who.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
stringVector
Who(Evaluator* eval, SCOPE_LEVEL scopeLevel, bool withPersistent)
{
    stringVector names;
    switch (scopeLevel) {
    case GLOBAL_SCOPE: {
        names = eval->getContext()->getGlobalScope()->getVariablesList(withPersistent);
    } break;
    case BASE_SCOPE: {
        names = eval->getContext()->getBaseScope()->getVariablesList(withPersistent);
    } break;
    case CALLER_SCOPE: {
        names = eval->getContext()->getCallerScope()->getVariablesList(withPersistent);
    } break;
    case LOCAL_SCOPE: {
        names = eval->getContext()->getCurrentScope()->getVariablesList(withPersistent);
    } break;
    default: {
        Error(_W("Wrong scope."));
    } break;
    }
    return names;
}
//=============================================================================
}
//=============================================================================
