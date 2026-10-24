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
#include "nlsNelson_manager_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Number of installed packages with a newer registry version, as last computed
// by the package manager window (nmm_gui) in this session, 0 until then. The
// desktops show it as a badge on their Package Manager action: the Qt desktop
// reads it, the web desktop receives it through WebDesktopBridge.
NLSNELSON_MANAGER_IMPEXP void
setPackageUpdatesCount(int count);
//=============================================================================
NLSNELSON_MANAGER_IMPEXP int
getPackageUpdatesCount();
//=============================================================================
} // namespace Nelson
//=============================================================================
