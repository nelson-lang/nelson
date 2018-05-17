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
#include "OverloadCache.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    namespace Overloading {
        //=============================================================================
        static std::string cachedFunctionNameUnary;
        static FunctionDef *cachedFunctionPointerUnary;
        static std::string cachedFunctionNameBinary;
        static FunctionDef *cachedFunctionPointerBinary;
        static std::string cachedFunctionNameTrinary;
        static FunctionDef *cachedFunctionPointerTrinary;
        //=============================================================================
        void setCachedFunction(OverloadClass oclass, std::string functionName, FunctionDef *funcptr)
        {
            switch (oclass)
            {
                case UNARY:
                    cachedFunctionNameUnary = functionName;
                    cachedFunctionPointerUnary = funcptr;
                    break;
                case BINARY:
                    cachedFunctionNameBinary = functionName;
                    cachedFunctionPointerBinary = funcptr;
                    break;
                case TRINARY:
                    cachedFunctionNameTrinary = functionName;
                    cachedFunctionPointerTrinary = funcptr;
                    break;
            }
        }
        //=============================================================================
        std::string getPreviousCachedFunctionName(OverloadClass oclass)
        {
            switch (oclass)
            {
                case UNARY:
                    return cachedFunctionNameUnary;
                case BINARY:
                    return cachedFunctionNameBinary;
                case TRINARY:
                    return cachedFunctionNameTrinary;
            }
        }
        //=============================================================================
        FunctionDef * getPreviousCachedFunctionDefinition(OverloadClass oclass)
        {
            switch (oclass)
            {
                case UNARY:
                    return cachedFunctionPointerUnary;
                case BINARY:
                    return cachedFunctionPointerBinary;
                case TRINARY:
                    return cachedFunctionPointerTrinary;
            }
        }
        //=============================================================================
        void clearPreviousCachedFunctionDefinition()
        {
            cachedFunctionNameUnary.clear();
            cachedFunctionPointerUnary = nullptr;
            cachedFunctionNameBinary.clear();
            cachedFunctionPointerBinary = nullptr;
            cachedFunctionNameTrinary.clear();
            cachedFunctionPointerTrinary = nullptr;
        }
        //=============================================================================
    }
    //=============================================================================
}
//=============================================================================

