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
#include "nlsMemory_manager_exports.h"
//=============================================================================
namespace Nelson {
NLSMEMORY_MANAGER_IMPEXP double
getTotalVirtualMemory();
NLSMEMORY_MANAGER_IMPEXP double
getTotalVirtualMemoryUsed();
NLSMEMORY_MANAGER_IMPEXP double
getTotalVirtualMemoryByNelson();
NLSMEMORY_MANAGER_IMPEXP double
getTotalPhysicalMemory();
NLSMEMORY_MANAGER_IMPEXP double
getTotalPhysicalMemoryUsed();
NLSMEMORY_MANAGER_IMPEXP double
getTotalPhysicalMemoryByNelson();
} // namespace Nelson
//=============================================================================
