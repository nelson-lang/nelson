//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
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
//=============================================================================
namespace Nelson {
struct WebDrawPrimitive;
bool
isWebFigureAnnotation(const WebDrawPrimitive& primitive);
bool
rasterizeWebFigureBase(
    const std::vector<WebDrawPrimitive>& primitives, int width, int height, std::string& base64);
//=============================================================================
} // namespace Nelson
//=============================================================================
