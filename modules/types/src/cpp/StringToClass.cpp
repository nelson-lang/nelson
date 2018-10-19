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
#include "StringToClass.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Class
StringToClass(std::wstring classname)
{
    Class destClass;
    if (classname.compare(L"handle") == 0) {
        destClass = NLS_HANDLE;
    } else if (classname.compare(L"int8") == 0) {
        destClass = NLS_INT8;
    } else if (classname.compare(L"int16") == 0) {
        destClass = NLS_INT16;
    } else if (classname.compare(L"int32") == 0) {
        destClass = NLS_INT32;
    } else if (classname.compare(L"int64") == 0) {
        destClass = NLS_INT64;
    } else if (classname.compare(L"uint8") == 0) {
        destClass = NLS_UINT8;
    } else if (classname.compare(L"uint16") == 0) {
        destClass = NLS_UINT16;
    } else if (classname.compare(L"uint32") == 0) {
        destClass = NLS_UINT32;
    } else if (classname.compare(L"uint64") == 0) {
        destClass = NLS_UINT64;
    } else if (classname.compare(L"single") == 0) {
        destClass = NLS_SINGLE;
    } else if (classname.compare(L"double") == 0) {
        destClass = NLS_DOUBLE;
    } else if (classname.compare(L"logical") == 0) {
        destClass = NLS_LOGICAL;
    } else if (classname.compare(L"char") == 0) {
        destClass = NLS_CHAR;
    } else if (classname.compare(L"cell") == 0) {
        destClass = NLS_CELL_ARRAY;
    } else if (classname.compare(L"string") == 0) {
        destClass = NLS_STRING_ARRAY;
    } else if (classname.compare(L"struct") == 0) {
        destClass = NLS_STRUCT_ARRAY;
    } else {
        Error(_W("input must be a valid class name."));
    }
    return destClass;
}
//=============================================================================
}
//=============================================================================
