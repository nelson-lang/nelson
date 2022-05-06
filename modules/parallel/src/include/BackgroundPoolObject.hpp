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
#include "nlsParallel_exports.h"
#include "HandleGenericObject.hpp"
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define BACKGROUNDPOOL_CATEGORY_STR L"BackgroundPool"
//=============================================================================
class NLSPARALLEL_IMPEXP BackgroundPoolObject : public HandleGenericObject
{
public:
    BackgroundPoolObject();
    ~BackgroundPoolObject() override;

private:
    size_t numWorkers;
    bool busy;
    wstringVector propertiesNames;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
