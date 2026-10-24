//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <atomic>
#include "PackageUpdatesCount.hpp"
#include "WebDesktopBridge.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::atomic<int> packageUpdatesCount { 0 };
//=============================================================================
void
setPackageUpdatesCount(int count)
{
    const int value = count < 0 ? 0 : count;
    if (packageUpdatesCount.exchange(value) != value) {
        WebDesktopBridge::getInstance()->pushWebPackageUpdates(value);
    }
}
//=============================================================================
int
getPackageUpdatesCount()
{
    return packageUpdatesCount.load();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
