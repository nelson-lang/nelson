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
#include "ClassdefLayout.hpp"
#include "nlsTypes_exports.h"
#include <atomic>
#include <cstring>
#include <unordered_map>
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * Reference-counted storage block used by ArrayOf. ArrayOf is its only owner;
 * the private interface keeps copy-on-write decisions in one place.
 */
class NLSTYPES_IMPEXP Data
{
    friend class ArrayOf;

private:
    NelsonType dataClass;
    bool sparse;
    /**
     * Requested sparse capacity (nzmax), kept independent of the Eigen
     * allocation across ArrayOf copies.
     */
    indexType sparseNzmax;
    bool isInline;
    /**
     * Inline payload for scalars: holds one double complex value without a
     * heap allocation.
     */
    static constexpr size_t INLINE_BUFFER_SIZE = 2 * sizeof(double);
    alignas(double) unsigned char inlineBuffer[INLINE_BUFFER_SIZE];
    /**
     * Pointer to the owned payload block, or inlineBuffer when isInline is set.
     */
    void* cp;
    std::atomic<indexType> owners;
    Dimensions dimensions_;
    /**
     * Allocated element capacity. Dense vectors grown by scalar indexed
     * assignment may reserve more elements than the dimensions expose.
     */
    indexType allocatedElementCount;
    stringVector fieldNames_;
    /**
     * Number of per-element field slots allocated in cp, normally
     * fieldNames_.size(). A uniquely owned scalar structure grown one field at a
     * time doubles it so repeated appends are O(1) amortized. A fresh payload
     * (constructor, putData) resets it to the exact count; element-major
     * indexing always uses fieldNames_.size() as the stride.
     */
    indexType allocatedFieldCount;
    bool fieldIndexValid_;
    /**
     * Lazy field-name -> ordinal cache. Built only above a small field count
     * (linear search wins for small structures), kept in sync on in-place
     * append and invalidated when the field names are replaced.
     */
    std::unordered_map<std::string, indexType> fieldNameToIndex_;

    std::string classTypeName_ = std::string(NLS_CLASS_ARRAY_STR);
    ClassdefLayoutPtr classdefLayout_;

    /**
     * Take ownership of the supplied payload. The owner count starts at 1.
     */
    Data(NelsonType aClass, const Dimensions& dims, void* s, bool sparseflag = false,
        stringVector fields = stringVector());
    Data(NelsonType aClass, const Dimensions& dims, const void* scalarData, size_t dataSize);
    ~Data();
    Data*
    copy();
    /**
     * Replace the payload. A single-owner block is reused after releasing the
     * old payload; a shared block is detached and a new Data is returned.
     */
    Data*
    putData(NelsonType aClass, const Dimensions& dims, void* s, bool sparseflag = false,
        const stringVector& fields = stringVector(), indexType allocatedCount = 0);
    indexType
    deleteCopy();
    [[nodiscard]] const void*
    data() const;
    [[nodiscard]] const Dimensions&
    dimensions() const;
    [[nodiscard]] std::string
    classTypeName() const;
    void
    setClassTypeName(const std::string& typeName);
    [[nodiscard]] ClassdefLayoutPtr
    classdefLayout() const;
    void
    setClassdefLayout(ClassdefLayoutPtr layout);
    void
    promoteStructToClass();
    void
    promoteStructToClassdef();
    void
    promoteClassToStruct();
    void
    promoteStructToFunctionHandle();

    [[nodiscard]] const stringVector&
    fieldNames() const;
    void
    setDimensions(const Dimensions& /*dim*/);
    void
    setFieldNames(const stringVector& fields);
    void
    invalidateFieldIndexCache();
    /**
     * Ordinal of a field name, or -1 when absent. Not const: it may build the
     * lazy index.
     */
    [[nodiscard]] int64
    fieldIndexFromName(const std::string& name);
    /**
     * Record a field appended at the end so a live cache stays in sync.
     */
    void
    noteAppendedField(const std::string& name, indexType newIndex);
    void*
    writableData();
    [[nodiscard]] indexType
    numberOfOwners() const;
    /**
     * Release the payload by class: reference classes delete nested objects,
     * value classes free the raw block.
     */
    void
    freeDataBlock();
    //=============================================================================
    [[nodiscard]] bool
    isSparse() const;
    //=============================================================================
    void
    refreshDimensionCache();
    //=============================================================================
    [[nodiscard]] inline indexType
    elementCount() const
    {
        return elementCountCache;
    }
    //=============================================================================
    [[nodiscard]] inline bool
    isScalar() const
    {
        return isScalarCache;
    }
    //=============================================================================
    [[nodiscard]] inline indexType
    rows() const
    {
        return rowsCache;
    }
    //=============================================================================
    [[nodiscard]] inline indexType
    columns() const
    {
        return columnsCache;
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
    indexType columnsCache;
    indexType rowsCache;
    indexType elementCountCache;
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
