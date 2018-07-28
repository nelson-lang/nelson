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
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
const bool
ArrayOf::isStringArray() const
{
    return (dp->dataClass == NLS_STRING_ARRAY);
}
//=============================================================================
const bool
ArrayOf::isNdArrayString() const
{
    return (dp->dataClass == NLS_STRING_ARRAY) && !is2D();
}
//=============================================================================
ArrayOf
ArrayOf::stringArrayConstructor(const std::string &value)
{ 
	stringVector strVector;
    strVector.push_back(value);
    return stringArrayConstructor(strVector, Dimensions(1, 1));
}
//=============================================================================
ArrayOf
ArrayOf::stringArrayConstructor(const std::wstring &value)
{
    wstringVector strVector;
    strVector.push_back(value);
    return stringArrayConstructor(strVector, Dimensions(1, 1));
}
//=============================================================================
ArrayOf
ArrayOf::stringArrayConstructor(const stringVector values, Dimensions dims)
{
    ArrayOf* elements = nullptr;
    size_t nbElements = dims.getElementCount();
    if (nbElements != values.size()) {
        throw Exception(_W("Invalid dimensions."));
	}
    if (nbElements > 0) {
        try {
            elements = new ArrayOf[nbElements];
        } catch (std::bad_alloc& e) {
            e.what();
            throw Exception(ERROR_MEMORY_ALLOCATION);
        }
        for (size_t k = 0; k < nbElements; k++) {
            elements[k] = ArrayOf::characterArrayConstructor(values[k]);
        }
    }
    return ArrayOf(NLS_STRING_ARRAY, dims, elements);
}
//=============================================================================
ArrayOf
ArrayOf::stringArrayConstructor(const wstringVector values, Dimensions dims)
{
    ArrayOf* elements = nullptr;
    size_t nbElements = dims.getElementCount();
    if (nbElements != values.size()) {
        throw Exception(_W("Invalid dimensions."));
    }
    if (nbElements > 0) {
        try {
            elements = new ArrayOf[nbElements];
        } catch (std::bad_alloc& e) {
            e.what();
            throw Exception(ERROR_MEMORY_ALLOCATION);
        }
        for (size_t k = 0; k < nbElements; k++) {
            elements[k] = ArrayOf::characterArrayConstructor(values[k]);
        }
    }
    return ArrayOf(NLS_STRING_ARRAY, dims, elements);
}
//=============================================================================
}
//=============================================================================