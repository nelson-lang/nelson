//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GraphicsClipboard.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    GraphicsClipboardProvider clipboardProvider = nullptr;
    //=============================================================================
} // namespace
//=============================================================================
void
setGraphicsClipboardProvider(GraphicsClipboardProvider provider)
{
    clipboardProvider = provider;
}
//=============================================================================
bool
copyGraphicsToNativeClipboard(GOFigureHost* host)
{
    return clipboardProvider != nullptr && clipboardProvider(host);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
