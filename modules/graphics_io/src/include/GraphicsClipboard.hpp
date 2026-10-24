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
#include "nlsGraphics_io_exports.h"
//=============================================================================
namespace Nelson {
class GOFigureHost;
using GraphicsClipboardProvider = bool (*)(GOFigureHost*);
NLSGRAPHICS_IO_IMPEXP void
setGraphicsClipboardProvider(GraphicsClipboardProvider provider);
NLSGRAPHICS_IO_IMPEXP bool
copyGraphicsToNativeClipboard(GOFigureHost* host);
//=============================================================================
} // namespace Nelson
//=============================================================================
