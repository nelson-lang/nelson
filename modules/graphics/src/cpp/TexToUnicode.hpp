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
#include <vector>
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
wstringVector
texToUnicode(const wstringVector& strs);
//=============================================================================
std::wstring
texToUnicode(const std::wstring& str);
//=============================================================================
// A single run of TeX text at one vertical level. level is cumulative:
// 0 = baseline, +N = N superscripts deep, -N = N subscripts deep (nested
// groups such as e^{-x^2} raise the inner x^2 one level further). Used by the
// paint path to lay out true raised/lowered runs instead of approximating them
// with Unicode glyphs.
struct TexRun
{
    std::wstring text;
    int level = 0;
};
//=============================================================================
// True when the string contains a TeX super/subscript marker (^ or _) that is
// followed by content (so an actual run would be produced). Lets the paint path
// take the run-layout branch only when there is something to lay out.
bool
texHasScript(const std::wstring& str);
//=============================================================================
// Parse a TeX string into left-to-right runs. Greek letters and symbols are
// replaced first (as texToUnicode does), then ^ / _ are split into runs:
// both braced forms (^{...}, _{...}) and the bare single-character forms
// (^c, _c). Unlike texToUnicode, ^ / _ are NOT converted to Unicode
// super/subscript glyphs here; the caller renders each run at its own font.
std::vector<TexRun>
texToRuns(const std::wstring& str);
//=============================================================================
} // namespace Nelson
//=============================================================================
