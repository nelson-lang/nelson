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
#include "StringToClass.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define TOWSTRING_(x) L##x
#define TOWSTRING(x) TOWSTRING_(x)
//=============================================================================
Class
StringToClass(const std::wstring& classname, bool& haveError)
{
    Class destClass = NLS_NOT_TYPED;
    haveError = false;
    if (classname.compare(TOWSTRING(NLS_GO_HANDLE_STR)) == 0) {
        destClass = NLS_GO_HANDLE;
    } else if (classname.compare(TOWSTRING(NLS_HANDLE_STR)) == 0) {
        destClass = NLS_HANDLE;
    } else if (classname.compare(TOWSTRING(NLS_INT8_STR)) == 0) {
        destClass = NLS_INT8;
    } else if (classname.compare(TOWSTRING(NLS_INT16_STR)) == 0) {
        destClass = NLS_INT16;
    } else if (classname.compare(TOWSTRING(NLS_INT32_STR)) == 0) {
        destClass = NLS_INT32;
    } else if (classname.compare(TOWSTRING(NLS_INT64_STR)) == 0) {
        destClass = NLS_INT64;
    } else if (classname.compare(TOWSTRING(NLS_UINT8_STR)) == 0) {
        destClass = NLS_UINT8;
    } else if (classname.compare(TOWSTRING(NLS_UINT16_STR)) == 0) {
        destClass = NLS_UINT16;
    } else if (classname.compare(TOWSTRING(NLS_UINT32_STR)) == 0) {
        destClass = NLS_UINT32;
    } else if (classname.compare(TOWSTRING(NLS_UINT64_STR)) == 0) {
        destClass = NLS_UINT64;
    } else if (classname.compare(TOWSTRING(NLS_SINGLE_STR)) == 0) {
        destClass = NLS_SINGLE;
    } else if (classname.compare(TOWSTRING(NLS_DOUBLE_STR)) == 0) {
        destClass = NLS_DOUBLE;
    } else if (classname.compare(TOWSTRING(NLS_LOGICAL_STR)) == 0) {
        destClass = NLS_LOGICAL;
    } else if (classname.compare(TOWSTRING(NLS_CHAR_STR)) == 0) {
        destClass = NLS_CHAR;
    } else if (classname.compare(TOWSTRING(NLS_CELL_ARRAY_STR)) == 0) {
        destClass = NLS_CELL_ARRAY;
    } else if (classname.compare(TOWSTRING(NLS_STRING_ARRAY_STR)) == 0) {
        destClass = NLS_STRING_ARRAY;
    } else if (classname.compare(TOWSTRING(NLS_STRUCT_ARRAY_STR)) == 0) {
        destClass = NLS_STRUCT_ARRAY;
    } else {
        haveError = true;
    }
    return destClass;
}
//=============================================================================
Class
StringToClass(const std::wstring& classname)
{
    bool haveError;
    Class destClass = StringToClass(classname, haveError);
    if (haveError) {
        Error(_W("input must be a valid class name."));
    }
    return destClass;
}
//=============================================================================
Class
StringToClass(const std::string& classname)
{
    return StringToClass(utf8_to_wstring(classname));
}
//=============================================================================
Class
StringToClass(const std::string& classname, bool& haveError)
{
    return StringToClass(utf8_to_wstring(classname), haveError);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
