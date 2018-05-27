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
#include "IsVariable.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
IsVariable(Evaluator* eval, SCOPE_LEVEL scopeLevel, std::wstring name)
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
}
//=============================================================================
