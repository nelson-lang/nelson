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
#include <algorithm>
#include <cctype>
#include <string>
#include "StringDeblank.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring&
rtrim(std::wstring& s)
{
    const wchar_t* ws = L" \t\n\r\f\v";
    s.erase(s.find_last_not_of(ws) + 1);
    return s;
}
//=============================================================================
static std::wstring&
Deblank(std::wstring& s)
{
    return rtrim(s);
}
//=============================================================================
ArrayOf
StringDeblank(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf res;
    if (A.isEmpty()) {
        res = ArrayOf::emptyConstructor();
        res.promoteType(A.getDataClass());
        return res;
    }
    if (A.isRowVectorCharacterArray()) {
        std::wstring str = A.getContentAsWideString();
        res = ArrayOf::characterArrayConstructor(Deblank(str));
    } else if (A.getDataClass() == NLS_CELL_ARRAY) {
        res = ArrayOf(A);
        res.ensureSingleOwner();
        ArrayOf* element = (ArrayOf*)(res.getDataPointer());
        for (indexType k = 0; k < A.getDimensions().getElementCount(); k++) {
            if (!element[k].isRowVectorCharacterArray()) {
                Error(ERROR_TYPE_CELL_OF_STRINGS_EXPECTED);
            }
            std::wstring str = element[k].getContentAsWideString();
            element[k] = ArrayOf::characterArrayConstructor(Deblank(str));
        }
    } else if (A.getDataClass() == NLS_STRING_ARRAY) {
        res = ArrayOf(A);
        res.ensureSingleOwner();
        ArrayOf* element = (ArrayOf*)(res.getDataPointer());
        for (indexType k = 0; k < A.getDimensions().getElementCount(); k++) {
            if (element[k].isRowVectorCharacterArray()) {
                std::wstring str = element[k].getContentAsWideString();
                element[k] = ArrayOf::characterArrayConstructor(Deblank(str));
            }
        }
    } else {
        needToOverload = true;
    }
    return res;
}
//=============================================================================
}
//=============================================================================
