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
#include "nlsDisplay_format_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
template <typename I>
std::wstring
hexifyInteger(I w, size_t hex_len = sizeof(I) << 1)
{
    static const wchar_t* digits = L"0123456789abcdef";
    std::wstring rc(hex_len, '0');
    for (size_t i = 0, j = (hex_len - 1) * 4; i < hex_len; ++i, j -= 4) {
        rc[i] = digits[(w >> j) & 0x0f];
    }
    return rc;
}
//=============================================================================
NLSDISPLAY_FORMAT_IMPEXP
std::wstring
single2hexastr(float d);
//=============================================================================
NLSDISPLAY_FORMAT_IMPEXP
std::wstring
double2hexastr(double d);
//=============================================================================
}
//=============================================================================
