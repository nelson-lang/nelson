//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include "LockVariable.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    bool LockVariable(std::wstring variable, Scope * scope)
    {
        return LockVariable(wstring_to_utf8(variable), scope);
    }
    //=============================================================================
    bool LockVariable(std::string variable, Scope * scope)
    {
        return scope->lockVariable(variable);
    }
    //=============================================================================
    bool UnlockVariable(std::wstring variable, Scope * scope)
    {
        return UnlockVariable(wstring_to_utf8(variable), scope);
    }
    //=============================================================================
    bool UnlockVariable(std::string variable, Scope * scope)
    {
        return scope->unlockVariable(variable);
    }
    //=============================================================================
    bool IsLockedVariable(std::wstring variable, Scope * scope)
    {
        return IsLockedVariable(wstring_to_utf8(variable), scope);
    }
    //=============================================================================
    bool IsLockedVariable(std::string variable, Scope * scope)
    {
        return scope->isLockedVariable(variable);
    }
    //=============================================================================
    stringVector GetLockedVariables(std::string variable, Scope * scope)
    {
        return scope->getLockedVariables();
    }
    //=============================================================================
}
//=============================================================================
