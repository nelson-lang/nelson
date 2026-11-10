//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "ArrayOf.hpp"
#include "ClassName.hpp"
#include "Evaluator.hpp"
#include "OverloadHelpers.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// calls the overload of the first object or handle argument, if any
inline bool
callObjectArgumentOverload(Evaluator* eval, int nLhs, const ArrayOfVector& argIn,
    const std::string& functionName, ArrayOfVector& result)
{
    if (eval == nullptr || !eval->withOverload) {
        return false;
    }
    for (const ArrayOf& arg : argIn) {
        NelsonType c = arg.dataClass();
        if (isOverloadObjectType(c)) {
            bool found = false;
            result = callOverloadedFunction(eval,
                NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), nLhs, argIn,
                functionName, ClassName(arg), c, found);
            return found;
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
