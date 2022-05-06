//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "BackgroundPoolObject.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
BackgroundPoolObject::BackgroundPoolObject()
    : HandleGenericObject(std::wstring(BACKGROUNDPOOL_CATEGORY_STR), this, false)
{
    propertiesNames = { L"NumWorkers", L"Busy" };
    numWorkers = 1;
    busy = false;
}
//=============================================================================
BackgroundPoolObject::~BackgroundPoolObject() {}
//=============================================================================
} // namespace Nelson
//=============================================================================
