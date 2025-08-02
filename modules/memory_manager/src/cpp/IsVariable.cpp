//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "IsVariable.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
IsVariable(Evaluator* eval, SCOPE_LEVEL scopeLevel, const std::wstring& name)
{
    bool res = false;
    std::string uname = wstring_to_utf8(name);
    switch (scopeLevel) {
    case GLOBAL_SCOPE: {
        res = eval->getContext()->getGlobalScope()->isVariable(uname);
    } break;
    case BASE_SCOPE: {
        res = eval->getContext()->getBaseScope()->isVariable(uname);
    } break;
    case CALLER_SCOPE: {
        res = eval->getContext()->getCallerScope()->isVariable(uname);
    } break;
    case LOCAL_SCOPE: {
        res = eval->getContext()->getCurrentScope()->isVariable(uname);
    } break;
    default: {
        res = false;
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
