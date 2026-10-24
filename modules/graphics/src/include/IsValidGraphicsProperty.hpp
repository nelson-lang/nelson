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
//=============================================================================
namespace Nelson {
//=============================================================================
NLSGRAPHICS_IMPEXP bool
IsValidGraphicsProperty(const std::wstring& GOTypename, const std::wstring& GOPropertyName);
//=============================================================================
// Resolve a (possibly abbreviated) property name to its canonical form for a
// graphics object type, using the shortest unambiguous leading-substring rule.
// Returns an empty string when the name is neither an exact nor an unambiguous
// prefix match. The type's property names are cached, so repeated lookups (the
// .m plotting wrappers parse property pairs on every call) stay cheap.
NLSGRAPHICS_IMPEXP std::wstring
CanonicalGraphicsProperty(const std::wstring& GOTypename, const std::wstring& GOPropertyName);
//=============================================================================
} // namespace Nelson
//=============================================================================
