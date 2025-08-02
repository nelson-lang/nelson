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
#include <string>
#include <vector>
#include "nlsGraphics_exports.h"
#include "Types.hpp"
#include "GraphicsObject.hpp"
#include "GOFigure.hpp"
#include "GOWindow.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define MAX_FIGS (int64)(2147483647 - 1)
#define HANDLE_OFFSET_OBJECT (int64)(2147483647)
#define HANDLE_OFFSET_FIGURE 1
#define HANDLE_ROOT_OBJECT -2
#define NO_FIGURE -1
//=============================================================================
NLSGRAPHICS_IMPEXP void
checkIdValidity(int64 id);
//=============================================================================
NLSGRAPHICS_IMPEXP bool
deleteGraphicsObject(int64 handle, bool repaintParentFigure, bool removeRefInParent);
//=============================================================================
NLSGRAPHICS_IMPEXP GraphicsObject*
findGraphicsObject(int64 handle, bool throwError = true);
//=============================================================================
NLSGRAPHICS_IMPEXP GOFigure*
findGOFigure(int64 handle);
//=============================================================================
NLSGRAPHICS_IMPEXP GOWindow*
findGOWindows(int64 handle);
//=============================================================================
NLSGRAPHICS_IMPEXP void
validateGO(int64 handle);
//=============================================================================
NLSGRAPHICS_IMPEXP ArrayOf
uniformizeStringVector(const ArrayOf& arg, wstringVector& asWideStringVector);
//=============================================================================
NLSGRAPHICS_IMPEXP void
Tokenize(
    const std::wstring& str, std::vector<std::wstring>& tokens, const std::wstring& delimiters);
//=============================================================================
NLSGRAPHICS_IMPEXP void
freeGraphicsObject(int64 handle);
//=============================================================================
NLSGRAPHICS_IMPEXP bool
isDeletedGraphicsObject(int64 handle);
//=============================================================================
NLSGRAPHICS_IMPEXP int64
assignGraphicsObject(GraphicsObject* hp);
//=============================================================================
}
//=============================================================================
