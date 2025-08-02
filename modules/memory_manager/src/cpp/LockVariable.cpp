//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LockVariable.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
LockVariable(const std::wstring& variable, Scope* scope)
{
    return LockVariable(wstring_to_utf8(variable), scope);
}
//=============================================================================
bool
LockVariable(const std::string& variable, Scope* scope)
{
    return scope->lockVariable(variable);
}
//=============================================================================
bool
UnlockVariable(const std::wstring& variable, Scope* scope)
{
    return UnlockVariable(wstring_to_utf8(variable), scope);
}
//=============================================================================
bool
UnlockVariable(const std::string& variable, Scope* scope)
{
    return scope->unlockVariable(variable);
}
//=============================================================================
bool
IsLockedVariable(const std::wstring& variable, Scope* scope)
{
    return IsLockedVariable(wstring_to_utf8(variable), scope);
}
//=============================================================================
bool
IsLockedVariable(const std::string& variable, Scope* scope)
{
    return scope->isLockedVariable(variable);
}
//=============================================================================
stringVector
GetLockedVariables(const std::string& variable, Scope* scope)
{
    return scope->getLockedVariables();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
