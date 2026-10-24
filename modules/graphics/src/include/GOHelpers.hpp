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
#include "GOFigureHost.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define MAX_FIGS (int64)(2147483647 - 1)
#define HANDLE_OFFSET_OBJECT (int64)(2147483647)
#define HANDLE_OFFSET_FIGURE 1
#define HANDLE_ROOT_OBJECT -2
#define HANDLE_UNASSIGNED_OBJECT -1
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
NLSGRAPHICS_IMPEXP GOFigureHost*
findGOWindows(int64 handle);
//=============================================================================
NLSGRAPHICS_IMPEXP void
validateGO(int64 handle);
//=============================================================================
NLSGRAPHICS_IMPEXP ArrayOf
uniformizeStringVector(
    const ArrayOf& arg, wstringVector& asWideStringVector, bool pipeDelimited = true);
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
NLSGRAPHICS_IMPEXP bool
isUnassignedGraphicsObject(int64 handle);
//=============================================================================
NLSGRAPHICS_IMPEXP int64
assignGraphicsObject(GraphicsObject* hp);
//=============================================================================
NLSGRAPHICS_IMPEXP int64
findGraphicsObjectHandle(GraphicsObject* hp);
//=============================================================================
NLSGRAPHICS_IMPEXP GraphicsObject*
graphicsObjectFromHandle(int64 handle, bool throwError = true);
//=============================================================================
NLSGRAPHICS_IMPEXP ArrayOf
graphicsObjectHandle(int64 handle, bool throwError = true);
//=============================================================================
NLSGRAPHICS_IMPEXP ArrayOf
graphicsObjectVector(const std::vector<int64>& handles);
//=============================================================================
// A container that stacks its children draws them one over another, so the last
// one added ends up on top. Nelson reports the children of
// such a container newest first, which puts the object drawn on top at the head
// of the list. Containers that lay their children out side by side -- a tab
// group, a tree, a grid -- report them in the order they were added instead,
// because that is the order they appear in.
NLSGRAPHICS_IMPEXP bool
stacksChildren(GraphicsObject* object);
//=============================================================================
// Children as the user sees them. The handles are stored back to front, which is
// the order the renderer paints in; a stacking container reverses that on the way
// out so that the head of the list is the object on top.
NLSGRAPHICS_IMPEXP std::vector<int64>
reportedChildren(GraphicsObject* object);
//=============================================================================
NLSGRAPHICS_IMPEXP bool
isVisibleGraphicsHandle(int64 handle);
//=============================================================================
// Drop the object from every figure that still names it as its current object or
// its current axes. Whoever destroys an object outside deleteGraphicsObject has
// to call this, or those properties keep handing out a handle to freed memory.
NLSGRAPHICS_IMPEXP void
clearFigureCurrentReferences(int64 handle);
//=============================================================================
NLSGRAPHICS_IMPEXP void
setCurrentObject(int64 handle);
//=============================================================================
} // namespace Nelson
//=============================================================================
