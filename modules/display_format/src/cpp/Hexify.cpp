//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Hexify.hpp"
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
single2hexastr(float d)
{
    if (d == 0.) {
        return L"00000000";
    }
    if (std::isnan(d)) {
        return L"ffc00000";
    }
    return fmt::sprintf(L"%x", *reinterpret_cast<int*>(&d));
}
//=============================================================================
std::wstring
double2hexastr(double d)
{
    if (d == 0.) {
        return L"0000000000000000";
    }
    if (std::isnan(d)) {
        return L"fff8000000000000";
    }
    return fmt::sprintf(L"%llx", *reinterpret_cast<unsigned long long*>(&d));
}
//=============================================================================
}
//=============================================================================
