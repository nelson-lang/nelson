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
#include <algorithm>
#include "Who.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
stringVector
Who(Evaluator* eval, SCOPE_LEVEL scopeLevel, bool withPersistent)
{
    Scope* scope = nullptr;
    switch (scopeLevel) {
    case GLOBAL_SCOPE: {
        scope = eval->getContext()->getGlobalScope();
    } break;
    case BASE_SCOPE: {
        scope = eval->getContext()->getBaseScope();
    } break;
    case CALLER_SCOPE: {
        scope = eval->getContext()->getCallerScope();
    } break;
    case LOCAL_SCOPE: {
        scope = eval->getContext()->getCurrentScope();
    } break;
    default: {
        Error(_W("Wrong scope."));
    } break;
    }
    return Who(eval, scope, withPersistent);
}
//=============================================================================
stringVector
Who(Evaluator* eval, Scope* scope, bool withPersistent)
{
    stringVector names;
    if (scope != nullptr) {
        scope->getVariablesList(withPersistent, names);
    }
    if (!names.empty()) {
        std::sort(names.begin(), names.end());
    }
    return names;
}
//=============================================================================
stringVector
Who(Evaluator* eval, bool withPersistent)
{
    stringVector names;
    Context* context = eval->getContext();
    if (context->getCurrentScope()->getName() == "base") {
        stringVector baseVariables = Who(eval, context->getBaseScope(), withPersistent);
        stringVector globalVariables = Who(eval, context->getGlobalScope(), withPersistent);
        names = baseVariables;
        names.insert(names.end(), globalVariables.begin(), globalVariables.end());
        if (!names.empty()) {
            std::sort(names.begin(), names.end());
            names.erase(std::unique(names.begin(), names.end()), names.end());
        }
    } else {
        names = Who(eval, context->getCurrentScope(), withPersistent);
    }
    return names;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
