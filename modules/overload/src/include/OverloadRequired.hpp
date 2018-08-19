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
#include "Evaluator.hpp"
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "ClassName.hpp"
#include "Overload.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static inline void
OverloadRequired(Evaluator* eval, const ArrayOfVector& argIn, Overload::OverloadClass otype,
    const std::string& functionName = "")
{
    std::string _functionName = eval->getCurrentFunctionName();
    if (!functionName.empty()) {
        _functionName = functionName;
    }
    std::string OverloadName("");
    switch (otype) {
    case Overload::OverloadClass::BINARY:
        OverloadName = ClassName(argIn[0]) + "_" + _functionName + "_" + ClassName(argIn[1]);
        break;
    case Overload::OverloadClass::TERNARY:
        OverloadName = _functionName + "_" + ClassName(argIn[0]) + "_" + ClassName(argIn[1]) + "_"
            + ClassName(argIn[2]);
        break;
    case Overload::OverloadClass::UNARY:
    case Overload::OverloadClass::FUNCTION:
        OverloadName = ClassName(argIn[0]) + "_" + _functionName;
        break;
    default:
        Error(_W("Wrong Overloading::OverloadClass."));
        break;
    }
    Error(_("function") + " " + OverloadName + " " + _("undefined."));
}
//=============================================================================
}
//=============================================================================
