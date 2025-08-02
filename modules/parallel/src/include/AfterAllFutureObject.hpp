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
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSPARALLEL_IMPEXP AfterAllFutureObject : public HandleGenericObject, public FutureObject
{
public:
    AfterAllFutureObject(
        const std::wstring& functionName, const std::vector<FutureObject*>& predecessors);
    void
    afterAll(FunctionDef* funcDef, int nLhs, bool uniformOutput = false);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
