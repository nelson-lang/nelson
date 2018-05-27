//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "helpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Class
precisionFromString(const std::wstring& str, bool& bOK)
{
    bOK = true;
    if (str == L"logical") {
        return NLS_LOGICAL;
    } else if ((str == L"double") || (str == L"real*8") || (str == L"float64")) {
        return NLS_DOUBLE;
    } else if ((str == L"single") || (str == L"real*4") || (str == L"float32")) {
        return NLS_SINGLE;
    } else if (str == L"int") {
        return NLS_INT32;
    } else if ((str == L"int8") || (str == L"integer*1") || (str == L"schar")) {
        return NLS_INT8;
    } else if ((str == L"int16") || (str == L"integer*2")) {
        return NLS_INT16;
    } else if ((str == L"int32") || (str == L"integer*4")) {
        return NLS_INT32;
    } else if ((str == L"int64") || (str == L"integer*8")) {
        return NLS_INT64;
    } else if ((str == L"uint8") || (str == L"uchar")) {
        return NLS_UINT8;
    } else if (str == L"uint16") {
        return NLS_UINT16;
    } else if (str == L"uint32") {
        return NLS_UINT32;
    } else if (str == L"uint64") {
        return NLS_UINT64;
    } else if (str == L"char") {
        return NLS_CHAR;
    }
    bOK = false;
    return NLS_UINT8;
}
//=============================================================================
}
//=============================================================================
