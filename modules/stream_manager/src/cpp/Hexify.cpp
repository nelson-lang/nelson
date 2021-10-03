//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "Hexify.hpp"
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
