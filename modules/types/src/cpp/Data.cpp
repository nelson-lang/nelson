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
#include <utility>
#include <atomic>
#include <cstring>
//=============================================================================
#include "Data.hpp"
#include "HandleState.hpp"
#include "SparseType.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Data::Data(NelsonType aClass, const Dimensions& dims, void* s, bool sparseflag, stringVector fields)
    : cp(s)
    , owners(1)
    , dimensions_(dims)
    , allocatedElementCount(dims.elementCount())
    , fieldNames_(std::move(fields))
    , allocatedFieldCount(static_cast<indexType>(fieldNames_.size()))
    , fieldIndexValid_(false)
    , dataClass(aClass)
    , sparseNzmax(0)
    , isInline(false)
{
    sparse = sparseflag;
    refreshDimensionCache();
}
//=============================================================================
Data::Data(NelsonType aClass, const Dimensions& dims, const void* scalarData, size_t dataSize)
    : owners(1)
    , dimensions_(dims)
    , allocatedElementCount(dims.elementCount())
    , allocatedFieldCount(0)
    , fieldIndexValid_(false)
    , dataClass(aClass)
    , sparse(false)
    , sparseNzmax(0)
    , isInline(true)
{
    std::memcpy(inlineBuffer, scalarData, dataSize);
    cp = inlineBuffer;
    refreshDimensionCache();
}
//=============================================================================
Data::~Data() { freeDataBlock(); }
//=============================================================================
Data*
Data::copy()
{
    owners.fetch_add(1, std::memory_order_relaxed);
    return this;
}
//=============================================================================
Data*
Data::putData(NelsonType aClass, const Dimensions& dims, void* s, bool sparseflag,
    const stringVector& fields, indexType allocatedCount)
{
    const indexType actualAllocatedCount
        = allocatedCount > 0 ? allocatedCount : dims.elementCount();
    if ((owners.load() <= 1)) {
        freeDataBlock();
        cp = s;
        isInline = false;
        dataClass = aClass;
        dimensions_ = dims;
        allocatedElementCount = actualAllocatedCount;
        fieldNames_ = fields;
        allocatedFieldCount = static_cast<indexType>(fieldNames_.size());
        invalidateFieldIndexCache();
        if (aClass != NLS_CLASSDEF_ARRAY) {
            classdefLayout_.reset();
        } else if (classdefLayout_) {
            fieldNames_.clear();
        }
        sparse = sparseflag;
        sparseNzmax = 0;
        owners = 1;
        refreshDimensionCache();
        return this;
    }
    owners--;
    Data* copy = new Data(aClass, dims, s, sparseflag, fields);
    copy->allocatedElementCount = actualAllocatedCount;
    if (aClass == NLS_CLASSDEF_ARRAY) {
        copy->classdefLayout_ = classdefLayout_;
        if (copy->classdefLayout_) {
            copy->fieldNames_.clear();
            copy->allocatedFieldCount = 0;
        }
    }
    return copy;
}
//=============================================================================
void
Data::promoteStructToClass()
{
    dataClass = NLS_CLASS_ARRAY;
}
//=============================================================================
void
Data::promoteStructToClassdef()
{
    dataClass = NLS_CLASSDEF_ARRAY;
}
//=============================================================================
void
Data::promoteClassToStruct()
{
    if (dataClass == NLS_CLASSDEF_ARRAY && classdefLayout_) {
        fieldNames_ = classdefLayout_->storagePropertyNames;
        allocatedFieldCount = static_cast<indexType>(fieldNames_.size());
        classdefLayout_.reset();
    }
    invalidateFieldIndexCache();
    dataClass = NLS_STRUCT_ARRAY;
}
//=============================================================================
void
Data::promoteStructToFunctionHandle()
{
    dataClass = NLS_FUNCTION_HANDLE;
}
//=============================================================================
indexType
Data::deleteCopy()
{
    return owners.fetch_sub(1, std::memory_order_acq_rel);
}
//=============================================================================
const void*
Data::data() const
{
    return cp;
}
//=============================================================================
void*
Data::writableData()
{
    return cp;
}
//=============================================================================
const Dimensions&
Data::dimensions() const
{
    return dimensions_;
}
//=============================================================================
const stringVector&
Data::fieldNames() const
{
    if (dataClass == NLS_CLASSDEF_ARRAY && classdefLayout_) {
        return classdefLayout_->storagePropertyNames;
    }
    return fieldNames_;
}
//=============================================================================
void
Data::setDimensions(const Dimensions& dim)
{
    dimensions_ = dim;
    refreshDimensionCache();
}
//=============================================================================
void
Data::setFieldNames(const stringVector& fields)
{
    fieldNames_ = fields;
    allocatedFieldCount = static_cast<indexType>(fieldNames_.size());
    invalidateFieldIndexCache();
}
//=============================================================================
void
Data::invalidateFieldIndexCache()
{
    fieldIndexValid_ = false;
    if (!fieldNameToIndex_.empty()) {
        fieldNameToIndex_.clear();
    }
}
//=============================================================================
int64
Data::fieldIndexFromName(const std::string& name)
{
    // A linear scan beats hashing for the overwhelmingly common small
    // structures; the cache only pays off once the field count grows large.
    static constexpr size_t FIELD_INDEX_HASH_THRESHOLD = 16;
    const stringVector& names = fieldNames();
    const size_t count = names.size();
    if (count <= FIELD_INDEX_HASH_THRESHOLD
        || (dataClass == NLS_CLASSDEF_ARRAY && classdefLayout_)) {
        for (size_t i = 0; i < count; ++i) {
            if (names[i] == name) {
                return static_cast<int64>(i);
            }
        }
        return -1;
    }
    if (!fieldIndexValid_) {
        fieldNameToIndex_.clear();
        fieldNameToIndex_.reserve(count);
        for (size_t i = 0; i < count; ++i) {
            fieldNameToIndex_.emplace(names[i], static_cast<indexType>(i));
        }
        fieldIndexValid_ = true;
    }
    auto it = fieldNameToIndex_.find(name);
    if (it == fieldNameToIndex_.end()) {
        return -1;
    }
    return static_cast<int64>(it->second);
}
//=============================================================================
void
Data::noteAppendedField(const std::string& name, indexType newIndex)
{
    if (fieldIndexValid_) {
        fieldNameToIndex_.emplace(name, newIndex);
    }
}
//=============================================================================
std::string
Data::classTypeName() const
{
    return classTypeName_;
}
//=============================================================================
void
Data::setClassTypeName(const std::string& typeName)
{
    classTypeName_ = typeName;
}
//=============================================================================
ClassdefLayoutPtr
Data::classdefLayout() const
{
    return classdefLayout_;
}
//=============================================================================
void
Data::setClassdefLayout(ClassdefLayoutPtr layout)
{
    classdefLayout_ = std::move(layout);
    if (dataClass == NLS_CLASSDEF_ARRAY && classdefLayout_) {
        fieldNames_.clear();
        allocatedFieldCount = 0;
    }
    invalidateFieldIndexCache();
}
//=============================================================================
indexType
Data::numberOfOwners() const
{
    return owners.load(std::memory_order_relaxed);
}
//=============================================================================
void
Data::freeDataBlock()
{
    if (isInline) {
        cp = nullptr;
        isInline = false;
        return;
    }
    if (cp) {
        switch (dataClass) {
        case NLS_MISSING_ARRAY: {
            auto* rp = static_cast<double*>(cp);
            delete[] rp;
        } break;
        case NLS_CELL_ARRAY: {
            auto* rp = static_cast<ArrayOf*>(cp);
            delete[] rp;
        } break;
        case NLS_STRING_ARRAY: {
            auto* rp = static_cast<ArrayOf*>(cp);
            delete[] rp;
        } break;
        case NLS_HANDLE: {
            auto* rp = static_cast<HandleRef*>(cp);
            // delete[] destroys elements last to first; a handle array releases
            // its objects in index order so that their destructors run in that
            // order (only handle arrays pay this pass).
            const indexType count = dimensions_.elementCount();
            for (indexType k = 0; k < count; ++k) {
                rp[k].reset();
            }
            delete[] rp;
        } break;
        case NLS_GO_HANDLE: {
            auto* rp = static_cast<nelson_handle*>(cp);
            delete[] rp;
        } break;
        case NLS_FUNCTION_HANDLE: {
            auto* rp = static_cast<ArrayOf*>(cp);
            delete[] rp;
        } break;
        case NLS_CLASS_ARRAY: {
            auto* rp = static_cast<ArrayOf*>(cp);
            delete[] rp;
        } break;
        case NLS_CLASSDEF_ARRAY: {
            auto* rp = static_cast<ArrayOf*>(cp);
            delete[] rp;
        } break;
        case NLS_STRUCT_ARRAY: {
            auto* rp = static_cast<ArrayOf*>(cp);
            delete[] rp;
        } break;
        case NLS_LOGICAL: {
            if (sparse) {
                Eigen_DeleteSparseMatrix(dataClass, dimensions_[0], dimensions_[1], &cp);
            } else {
                auto* rp = static_cast<logical*>(cp);
                delete[] rp;
            }
        } break;
        case NLS_UINT8: {
            auto* rp = static_cast<uint8*>(cp);
            delete[] rp;
        } break;
        case NLS_INT8: {
            auto* rp = static_cast<int8*>(cp);
            delete[] rp;
        } break;
        case NLS_UINT16: {
            auto* rp = static_cast<uint16*>(cp);
            delete[] rp;
        } break;
        case NLS_INT16: {
            auto* rp = static_cast<int16*>(cp);
            delete[] rp;
        } break;
        case NLS_UINT32: {
            auto* rp = static_cast<uint32*>(cp);
            delete[] rp;
        } break;
        case NLS_INT32: {
            auto* rp = static_cast<int32*>(cp);
            delete[] rp;
        } break;
        case NLS_UINT64: {
            auto* rp = static_cast<uint64*>(cp);
            delete[] rp;
        } break;
        case NLS_INT64: {
            auto* rp = static_cast<int64*>(cp);
            delete[] rp;
        } break;
        case NLS_SINGLE: {
            if (sparse) {
                Eigen_DeleteSparseMatrix(dataClass, dimensions_[0], dimensions_[1], &cp);
            } else {
                auto* rp = static_cast<single*>(cp);
                delete[] rp;
            }
        } break;
        case NLS_DOUBLE: {
            if (sparse) {
                Eigen_DeleteSparseMatrix(dataClass, dimensions_[0], dimensions_[1], &cp);
            } else {
                auto* rp = static_cast<double*>(cp);
                delete[] rp;
            }
        } break;
        case NLS_SCOMPLEX: {
            if (sparse) {
                Eigen_DeleteSparseMatrix(dataClass, dimensions_[0], dimensions_[1], &cp);
            } else {
                auto* rp = static_cast<single*>(cp);
                delete[] rp;
            }
        } break;
        case NLS_DCOMPLEX: {
            if (sparse) {
                Eigen_DeleteSparseMatrix(dataClass, dimensions_[0], dimensions_[1], &cp);
            } else {
                auto* rp = static_cast<double*>(cp);
                delete[] rp;
            }
        } break;
        case NLS_CHAR: {
            auto* rp = static_cast<charType*>(cp);
            delete[] rp;
        } break;
        default: {
        } break;
        }
        cp = nullptr;
    }
}
//=============================================================================
bool
Data::isSparse() const
{
    return sparse;
}
//=============================================================================
void
Data::refreshDimensionCache()
{
    isVectorCache = dimensions_.isVector();
    is2DCache = dimensions_.is2D();
    isScalarCache = dimensions_.isScalar();
    columnsCache = dimensions_.columns();
    rowsCache = dimensions_.rows();
    elementCountCache = dimensions_.elementCount();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
