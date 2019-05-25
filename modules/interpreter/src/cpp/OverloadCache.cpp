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
#include "OverloadCache.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace Overload {
    //=============================================================================
    static std::string cachedFunctionNameUnary;
    static FunctionDef* cachedFunctionPointerUnary;
    static std::string cachedFunctionNameBinary;
    static FunctionDef* cachedFunctionPointerBinary;
    static std::string cachedFunctionNameTernary;
    static FunctionDef* cachedFunctionPointerTernary;
    //=============================================================================
    void
    setCachedFunction(OverloadClass oclass, const std::string& functionName, FunctionDef* funcptr)
    {
        switch (oclass) {
        case UNARY:
            cachedFunctionNameUnary = std::move(functionName);
            cachedFunctionPointerUnary = funcptr;
            break;
        case BINARY:
            cachedFunctionNameBinary = std::move(functionName);
            cachedFunctionPointerBinary = funcptr;
            break;
        case TERNARY:
            cachedFunctionNameTernary = std::move(functionName);
            cachedFunctionPointerTernary = funcptr;
            break;
        default: { } break; }
    }
    //=============================================================================
    std::string
    getPreviousCachedFunctionName(OverloadClass oclass)
    {
        switch (oclass) {
        case UNARY:
            return cachedFunctionNameUnary;
        case BINARY:
            return cachedFunctionNameBinary;
        case TERNARY:
            return cachedFunctionNameTernary;
        default: { } break; }
        return "";
    }
    //=============================================================================
    FunctionDef*
    getPreviousCachedFunctionDefinition(OverloadClass oclass)
    {
        switch (oclass) {
        case UNARY:
            return cachedFunctionPointerUnary;
        case BINARY:
            return cachedFunctionPointerBinary;
        case TERNARY:
            return cachedFunctionPointerTernary;
        default: { } break; }
        return nullptr;
    }
    //=============================================================================
    void
    clearPreviousCachedFunctionDefinition()
    {
        cachedFunctionNameUnary.clear();
        cachedFunctionPointerUnary = nullptr;
        cachedFunctionNameBinary.clear();
        cachedFunctionPointerBinary = nullptr;
        cachedFunctionNameTernary.clear();
        cachedFunctionPointerTernary = nullptr;
    }
    //=============================================================================
} // namespace Overload
//=============================================================================
} // namespace Nelson
//=============================================================================
