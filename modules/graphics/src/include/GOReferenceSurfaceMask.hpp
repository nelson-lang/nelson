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
#include "Types.hpp"
#include "nlsGraphics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Hides inherited-but-not-applicable properties from the user-facing introspection surface:
// get(h) with no property, properties(h), fieldnames(h), isprop(h, ...) and one-argument set(h).
// The properties still exist: findProperty keeps working for rendering, copyobj, savefig and
// dotted access.
//
// The lookup is keyed on the effective object type (getType()), not on the C++ class, so a base
// type can keep a property that a derived type must not expose and vice versa (e.g. patch hides
// XDataMode/YDataMode while its derived area keeps XDataMode). Instance-conditional cases
// (cartesian vs polar axes, cartesian vs polar line/scatter) are handled by dedicated
// getVisibleFieldnames overrides.
//
// Returns nullptr when the type has nothing to hide.
NLSGRAPHICS_IMPEXP const wstringVector*
referenceSurfaceHiddenProperties(const std::wstring& type);
//=============================================================================
} // namespace Nelson
//=============================================================================
