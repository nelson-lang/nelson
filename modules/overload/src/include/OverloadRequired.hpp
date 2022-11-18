//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "Evaluator.hpp"
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "i18n.hpp"
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
    std::string OverloadName;
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
} // namespace Nelson
//=============================================================================
