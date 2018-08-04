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
ArrayOf
ArrayOf::stringArrayConstructor(ArrayOfMatrix& matrix)
{
    indexType columnCount = 0, rowCount = 0;
    ArrayOf* qp = nullptr;
    try {
        ArrayOfMatrix::iterator i = matrix.begin();
        while (i != matrix.end()) {
            ArrayOfVector ptr = *i;
            /**
             * If this is the first row in the matrix def, then we
             * record its size in columnCount.
             */
            if (i == matrix.begin()) {
                columnCount = ptr.size();
            } else {
                /**
                 * Otherwise, make sure the column counts are all the same...
                 */
                if (ptr.size() != columnCount) {
                    throw Exception(
                        _W("String definition must have same number of elements in each row."));
                }
            }
            ++i;
        }
        /**
         * At this point, we know how many columns our string array has,
         * and the number of rows is also known (size of m).  So, set
         * up our dimensions, and allocate the output.
         */
        rowCount = matrix.size();
        Dimensions retDims(2);
        retDims[0] = rowCount;
        retDims[1] = columnCount;
        /**
         * Allocate storage space for the contents.
         */
        qp = (ArrayOf*)allocateArrayOf(NLS_STRING_ARRAY, retDims.getElementCount());
        ArrayOf* sp = qp;
        /**
         * Loop through the rows.
         */
        i = matrix.begin();
        while (i != matrix.end()) {
            ArrayOfVector ptr = *i;
            ArrayOf* cp = sp;
            for (sizeType j = 0; j < (sizeType)ptr.size(); j++) {
                *cp = ptr[j];
                cp += rowCount;
            }
            ++i;
            sp++;
        }
        return ArrayOf(NLS_STRING_ARRAY, retDims, qp);
    } catch (Exception& e) {
        ArrayOf* rp = (ArrayOf*)qp;
        delete[] rp;
        rp = nullptr;
        qp = nullptr;
        e.what();
        throw;
    }
}
//=============================================================================
/**
 * Print this object when it is an element of a string array.
 */
void
ArrayOf::summarizeStringArray(Interface *io) const
{
    if (isEmpty()) {
        io->outputMessage("\"\"");
    } else {
        if (dp->dataClass == NLS_CHAR) {
            Dimensions dims = dp->dimensions;
            if (dims.isRowVector()) {
                if (dims.getColumns() < (indexType)(io->getTerminalWidth() - 3)) {
                    std::wstring str = getContentAsWideString();
                    str = L"\"" + str + L"\"";
                    io->outputMessage(str);
                    return;
                }
            }
            io->outputMessage("[");
            dp->dimensions.printMe(io);
            io->outputMessage(" string]");
        } else {
            throw Exception(_W("character array expected."));
        }
    }
}
//=============================================================================
}
//=============================================================================