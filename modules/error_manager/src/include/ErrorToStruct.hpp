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
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
ErrorToStruct(Exception& e)
{
    ArrayOf result;
    wstringVector fieldnames;
    ArrayOfVector fieldvalues;
    fieldnames.push_back(L"file");
    fieldnames.push_back(L"name");
    fieldnames.push_back(L"line");
    ArrayOf stack;
    if (e.isEmpty()) {
        Dimensions dims(0, 1);
        stack = ArrayOf::emptyStructConstructor(fieldnames, dims);
    } else {
        if ((e.getFilename() == L"") || (e.getFilename() == L"EvaluateScript")
            || (e.getFunctionName() == L"") || (e.getLine() == -1)) {
            Dimensions dim(0, 1);
            stack = ArrayOf::emptyStructConstructor(fieldnames, dim);
        } else {
            fieldvalues.push_back(ArrayOf::characterArrayConstructor(e.getFilename()));
            fieldvalues.push_back(ArrayOf::characterArrayConstructor(e.getFunctionName()));
            fieldvalues.push_back(ArrayOf::doubleConstructor(e.getLine()));
            stack = ArrayOf::structConstructor(fieldnames, fieldvalues);
        }
    }
    fieldnames.clear();
    fieldnames.push_back(L"message");
    fieldnames.push_back(L"identifier");
    fieldnames.push_back(L"stack");
    fieldvalues.clear();
    fieldvalues.push_back(ArrayOf::characterArrayConstructor(e.getMessage()));
    fieldvalues.push_back(ArrayOf::characterArrayConstructor(e.getIdentifier()));
    fieldvalues.push_back(stack);
    result = ArrayOf::structConstructor(fieldnames, fieldvalues);
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
