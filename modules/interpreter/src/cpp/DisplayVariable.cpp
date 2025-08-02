//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Evaluator.hpp"
#include "DisplayVariable.hpp"
#include "OverloadHelpers.hpp"
#include "OverloadRequired.hpp"
#include "Profiler.hpp"
#include "ProfilerHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
Evaluator::display(
    const ArrayOf& A, const std::string& name, bool asDispBuiltin, bool withProfiling)
{
    bool wasFound = false;

    std::string functionName = asDispBuiltin ? "disp" : "display";
    ArrayOfVector args;
    args << A;
    if (!name.empty()) {
        args << ArrayOf::characterArrayConstructor(name);
    }
    callOverloadedFunction(this,
        NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), args, functionName,
        ClassName(A), A.getDataClass(), wasFound, 0);
    if (wasFound) {
        return;
    }
    uint64 ticProfile = withProfiling ? Profiler::getInstance()->tic() : 0U;

    bool needToOverload = false;
    DisplayVariable(this->getID(), this->getInterface(), A, name, asDispBuiltin, needToOverload);
    if (ticProfile != 0U) {
        internalProfileFunction stack = computeProfileStack(this, functionName, L"evaluator");
        Profiler::getInstance()->toc(ticProfile, stack);
    }
    if (needToOverload) {
        callOverloadedFunction(this, NLS_OVERLOAD_OBJECT_TYPES_ONLY, args, functionName,
            ClassName(A), A.getDataClass(), wasFound, 0);
        if (!wasFound) {
            OverloadRequired(functionName);
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
