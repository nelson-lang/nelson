//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "Who.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "ParallelSort.hpp"
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
        parallelSort(names);
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
        names = Who(eval, context->getBaseScope(), withPersistent);
    } else {
        names = Who(eval, context->getCurrentScope(), withPersistent);
    }
    return names;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
