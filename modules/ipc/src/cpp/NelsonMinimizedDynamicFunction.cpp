//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonMinimizedDynamicFunction.hpp"
#include "WebDesktopBridge.hpp"
//=============================================================================
// Minimizing the main window is a desktop service: the Qt main window was
// removed, so the request is forwarded to the desktop through the bridge.
// Both calls are no-ops (false) when no desktop registered a handler.
//=============================================================================
bool
setNelsonMinimizedDynamicFunction(bool minimize)
{
    return Nelson::WebDesktopBridge::getInstance()->requestWebMinimize(minimize);
}
//=============================================================================
bool
getNelsonMinimizedDynamicFunction()
{
    return Nelson::WebDesktopBridge::getInstance()->queryWebMinimized();
}
//=============================================================================
