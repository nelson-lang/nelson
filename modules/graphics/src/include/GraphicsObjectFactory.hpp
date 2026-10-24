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
#include "nlsGraphics_exports.h"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class GraphicsObject;
//=============================================================================
NLSGRAPHICS_IMPEXP GraphicsObject*
newGraphicsObjectForType(const std::wstring& type, GraphicsObject* parent = nullptr,
    bool includeInternalObjects = false);
//=============================================================================
NLSGRAPHICS_IMPEXP GraphicsObject*
newGraphicsObjectForSource(GraphicsObject* source);
//=============================================================================
NLSGRAPHICS_IMPEXP bool
canCreateGraphicsObjectForType(const std::wstring& type, GraphicsObject* parent = nullptr,
    bool includeInternalObjects = false);
//=============================================================================
NLSGRAPHICS_IMPEXP bool
isGraphicsTreeLinkProperty(const std::wstring& name);
//=============================================================================
NLSGRAPHICS_IMPEXP bool
containsGraphicsRuntimeValue(const ArrayOf& value);
//=============================================================================
NLSGRAPHICS_IMPEXP bool
canCopyGraphicsProperty(GraphicsObject* source, GraphicsObject* target, const std::wstring& name);
//=============================================================================
} // namespace Nelson
//=============================================================================
