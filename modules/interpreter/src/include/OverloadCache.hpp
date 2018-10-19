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
#pragma once
//=============================================================================
#include "nlsInterpreter_exports.h"
#include "FunctionDef.hpp"
#include "Overload.hpp"
//=============================================================================
namespace Nelson {
namespace Overload {
    //=============================================================================
    NLSINTERPRETER_IMPEXP std::string
    getPreviousCachedFunctionName(OverloadClass oclass);
    NLSINTERPRETER_IMPEXP void
    setCachedFunction(OverloadClass oclass, std::string functionName, FunctionDef* funcptr);
    NLSINTERPRETER_IMPEXP FunctionDef*
    getPreviousCachedFunctionDefinition(OverloadClass oclass);
    NLSINTERPRETER_IMPEXP void
    clearPreviousCachedFunctionDefinition();
}
}
//=============================================================================
