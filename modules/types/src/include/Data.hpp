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
// Copyright (c) 2002, 2003 Samit Basu
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

#pragma once
#include "ArrayOf.hpp"
#include "nlsTypes_exports.h"

namespace Nelson {

/**
 * This is a helper class that is used by the ArrayOf class to
 * support the reference counting scheme used by the ArrayOf objects.
 * It essentially stores blocks of data along with a count of
 * how many owners of the data block there are (standard reference
 * counting).  The ArrayOf class is declared a friend, as all the
 * methods (including the constructors) are private.
 */
class NLSTYPES_IMPEXP Data
{
    friend class ArrayOf;

private:
    /**
     * The Class of the data block.  Useful for determining how the data
     * block must be treated.
     */
    Class dataClass;
    /**
     * Sparsity flag - true if we are a sparse array.
     */
    bool sparse;
    /**
     * Pointer to the data block.
     */
    void* cp;
    /**
     * Number of owners for the data block.
     */
    int owners;
    /**
     * The dimensions of the data block.
     */
    Dimensions dimensions;
    /**
     * The field names of the array - used only for structure array types.
     */
    stringVector fieldNames;

    std::string structTypeName = std::string(NLS_STRUCT_ARRAY_STR);
    /**
     * struct class name by default "struct"
     */

    /**
     * Construct a Data object with the given arguments.
     * the owner count is initialized to 1.
     */
    Data(Class aClass, const Dimensions& dims, void* s, bool sparseflag = false,
        const stringVector& fields = stringVector());
    /**
     * The destructor.  Calls freeDataBlock member function.
     */
    ~Data();
    /**
     * Get a copy to us - increments the data counter by one, and
     * returns the "this" pointer.
     */
    Data*
    getCopy();
    /**
     * Change the contents of the Data object.  The data pointer,
     * class and size are given.  The behavior/return value depends
     * on the number of owners:
     *   - If there is only one owner for this Data block, then
     *     freeDataBlock is called to delete the current data, and
     *     the members are simply reassigned to the given arguments.
     *     The return value is the "this" pointer.
     *   - Otherwise, the number of owners for the current block
     *     is decreased by one, and a new Data object is returned
     *     with the given contents.
     */
    Data*
    putData(Class aClass, const Dimensions& dims, void* s, bool sparseflag = false,
        const stringVector& fields = stringVector());
    /**
     * Decrement the reference count (owners) by one.
     */
    int
    deleteCopy();
    /**
     * Get a read-only pointer to the data.
     */
    const void*
    getData() const;
    /**
     * Get the dimensions for the data block.
     */
    const Dimensions&
    getDimensions() const;
    /**
     * Get the field names for the data block
     */

    std::string
    getStructTypeName();
    /**
     * get struct type name (by default: struct)
     */

    void
    setStructTypeName(std::string typeName);
    /**
     * set struct type name (by default: struct)
     */

    const stringVector&
    getFieldNames() const;
    /**
     * Set the dimensions for the data block.
     */
    void
    setDimensions(const Dimensions&);
    /**
     * Set the field names for the data block.
     */
    void
    setFieldNames(const stringVector& fields);
    /**
     * Get a read-write pointer to the data.
     */
    void*
    getWriteableData();
    /**
     * Get the number of owners.
     */
    int
    numberOfOwners() const;
    /**
     * If the data pointer is non-null, we take one of
     * the following options:
     *   - For reference types, a deep delete is performed
     *   - For non-reference types, the data block is
     *     simply freed.
     */
    void
    freeDataBlock();
    /**
     * Check sparsity.
     */
    bool
    isSparse();
};
} // namespace Nelson
