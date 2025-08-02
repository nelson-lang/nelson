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
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSPARALLEL_IMPEXP FevalFutureObject : public HandleGenericObject, public FutureObject
{
public:
    FevalFutureObject(const std::wstring& functionName);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
