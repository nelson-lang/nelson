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
#include "nlsParallel_exports.h"
#include "HandleGenericObject.hpp"
#include "FutureObject.hpp"
#include "FevalFutureObject.hpp"
#include "FunctionDef.hpp"
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSPARALLEL_IMPEXP AfterEachFutureObject : public HandleGenericObject, public FutureObject
{
public:
    AfterEachFutureObject(
        const std::wstring& functionName, const std::vector<FutureObject*>& predecessors);
    void
    afterEach(FunctionDef* funcDef, int nLhs, bool uniformOutput = false);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
