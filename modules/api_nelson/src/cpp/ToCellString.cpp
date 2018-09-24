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
#include "ToCellString.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
ToCellStringAs(stringVector vectorStr, bool bAsColumn)
{
    ArrayOf* elements = nullptr;
    size_t nbElements = vectorStr.size();
    if (nbElements > 0) {
        try {
            elements = new ArrayOf[nbElements];
        } catch (const std::bad_alloc& e) {
            e.what();
            Error(ERROR_MEMORY_ALLOCATION);
        }
        for (size_t k = 0; k < nbElements; k++) {
            elements[k] = ArrayOf::characterArrayConstructor(vectorStr[k]);
        }
    }
    ArrayOf c;
    if (bAsColumn) {
        Dimensions dims(nbElements, 1);
        c = ArrayOf(NLS_CELL_ARRAY, dims, elements);
    } else {
        Dimensions dims(1, nbElements);
        c = ArrayOf(NLS_CELL_ARRAY, dims, elements);
    }
    return c;
}
//=============================================================================
static ArrayOf
ToCellStringAs(wstringVector vectorStr, bool bAsColumn)
{
    ArrayOf* elements = nullptr;
    size_t nbElements = vectorStr.size();
    if (nbElements > 0) {
        try {
            elements = new ArrayOf[nbElements];
        } catch (const std::bad_alloc& e) {
            e.what();
            Error(ERROR_MEMORY_ALLOCATION);
        }
        for (size_t k = 0; k < nbElements; k++) {
            elements[k] = ArrayOf::characterArrayConstructor(vectorStr[k]);
        }
    }
    ArrayOf c;
    if (bAsColumn) {
        Dimensions dims(nbElements, 1);
        c = ArrayOf(NLS_CELL_ARRAY, dims, elements);
    } else {
        Dimensions dims(1, nbElements);
        c = ArrayOf(NLS_CELL_ARRAY, dims, elements);
    }
    return c;
}
//=============================================================================
ArrayOf
ToCellStringAsRow(wstringVector vectorStr)
{
    return ToCellStringAs(vectorStr, false);
}
//=============================================================================
ArrayOf
ToCellStringAsColumn(wstringVector vectorStr)
{
    return ToCellStringAs(vectorStr, true);
}
//=============================================================================
ArrayOf
ToCellStringAsRow(stringVector vectorStr)
{
    return ToCellStringAs(vectorStr, false);
}
//=============================================================================
ArrayOf
ToCellStringAsColumn(stringVector vectorStr)
{
    return ToCellStringAs(vectorStr, true);
}
//=============================================================================
}
//=============================================================================
