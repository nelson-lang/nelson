//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// Copyright (c) 2002, 2003 Samit Basu
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "nlsTypes_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
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
    NelsonType dataClass;
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
    indexType owners;
    /**
     * The dimensions of the data block.
     */
    Dimensions dimensions;
    /**
     * The field names of the array - used only for structure array types.
     */
    stringVector fieldNames;

    std::string classTypeName = std::string(NLS_CLASS_ARRAY_STR);
    /**
     * struct class name by default "struct"
     */

    /**
     * Construct a Data object with the given arguments.
     * the owner count is initialized to 1.
     */
    Data(NelsonType aClass, const Dimensions& dims, void* s, bool sparseflag = false,
        stringVector fields = stringVector());
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
    putData(NelsonType aClass, const Dimensions& dims, void* s, bool sparseflag = false,
        const stringVector& fields = stringVector());
    /**
     * Decrement the reference count (owners) by one.
     */
    indexType
    deleteCopy();
    /**
     * Get a read-only pointer to the data.
     */
    [[nodiscard]] const void*
    getData() const;
    /**
     * Get the dimensions for the data block.
     */
    [[nodiscard]] const Dimensions&
    getDimensions() const;
    /**
     * Get the field names for the data block
     */

    [[nodiscard]] std::string
    getClassTypeName() const;
    /**
     * get struct type name (by default: struct)
     */

    void
    setClassTypeName(const std::string& typeName);
    /**
     * set struct type name (by default: struct)
     */

    void
    promoteStructToClass();
    /**
     * promote struct to class type
     */

    void
    promoteClassToStruct();
    /**
     * promote class to struct type
     */

    void
    promoteStructToFunctionHandle();
    /**
     * promote struct to function_handle type
     */

    [[nodiscard]] const stringVector&
    getFieldNames() const;
    /**
     * Set the dimensions for the data block.
     */
    void
    setDimensions(const Dimensions& /*dim*/);
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
    [[nodiscard]] indexType
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
    //=============================================================================
    /**
     * Check sparsity.
     */
    [[nodiscard]] bool
    isSparse() const;
    //=============================================================================
    void
    refreshDimensionCache();
    //=============================================================================
    [[nodiscard]] inline indexType
    getElementCount() const
    {
        return getElementCountCache;
    }
    //=============================================================================
    [[nodiscard]] inline bool
    isScalar() const
    {
        return isScalarCache;
    }
    //=============================================================================
    [[nodiscard]] inline indexType
    getRows() const
    {
        return getRowsCache;
    }
    //=============================================================================
    [[nodiscard]] inline indexType
    getColumns() const
    {
        return getColumnsCache;
    }
    //=============================================================================
    [[nodiscard]] inline bool
    is2D() const
    {
        return is2DCache;
    }
    //=============================================================================
    [[nodiscard]] inline bool
    isVector() const
    {
        return isVectorCache;
    }
    //=============================================================================
    bool isVectorCache;
    bool is2DCache;
    bool isScalarCache;
    indexType getColumnsCache;
    indexType getRowsCache;
    indexType getElementCountCache;
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
