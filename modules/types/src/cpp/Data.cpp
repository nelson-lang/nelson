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
//=============================================================================
#include "Data.hpp"
#include "SparseDynamicFunctions.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Data::Data(NelsonType aClass, const Dimensions& dims, void* s, bool sparseflag, stringVector fields)
    : cp(s), owners(1), dimensions(dims), fieldNames(std::move(fields)), dataClass(aClass)
{
    sparse = sparseflag;
    refreshDimensionCache();
}
//=============================================================================
Data::~Data() { freeDataBlock(); }
//=============================================================================
Data*
Data::getCopy()
{
    owners++;
    return this;
}
//=============================================================================
Data*
Data::putData(
    NelsonType aClass, const Dimensions& dims, void* s, bool sparseflag, const stringVector& fields)
{
    if ((owners <= 1)) {
        freeDataBlock();
        cp = s;
        dataClass = aClass;
        dimensions = dims;
        fieldNames = fields;
        sparse = sparseflag;
        owners = 1;
        refreshDimensionCache();
        return this;
    }
    owners--;
    return new Data(aClass, dims, s, sparseflag, fields);
}
//=============================================================================
void
Data::promoteStructToClass()
{
    dataClass = NLS_CLASS_ARRAY;
}
//=============================================================================
void
Data::promoteClassToStruct()
{
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
    return owners--;
}
//=============================================================================
const void*
Data::getData() const
{
    return cp;
}
//=============================================================================
void*
Data::getWriteableData()
{
    return cp;
}
//=============================================================================
const Dimensions&
Data::getDimensions() const
{
    return dimensions;
}
//=============================================================================
const stringVector&
Data::getFieldNames() const
{
    return fieldNames;
}
//=============================================================================
void
Data::setDimensions(const Dimensions& dim)
{
    dimensions = dim;
    refreshDimensionCache();
}
//=============================================================================
void
Data::setFieldNames(const stringVector& fields)
{
    fieldNames = fields;
}
//=============================================================================
std::string
Data::getClassTypeName() const
{
    return classTypeName;
}
//=============================================================================
void
Data::setClassTypeName(const std::string& typeName)
{
    classTypeName = typeName;
}
//=============================================================================
indexType
Data::numberOfOwners() const
{
    return owners;
}
//=============================================================================
void
Data::freeDataBlock()
{
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
        case NLS_GO_HANDLE: {
            auto* rp = static_cast<nelson_handle*>(cp);
            delete[] rp;
        } break;
        case NLS_HANDLE: {
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
        case NLS_STRUCT_ARRAY: {
            auto* rp = static_cast<ArrayOf*>(cp);
            delete[] rp;
        } break;
        case NLS_LOGICAL: {
            if (sparse) {
                DeleteSparseMatrixDynamicFunction(dataClass, dimensions[0], dimensions[1], cp);
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
            auto* rp = static_cast<single*>(cp);
            delete[] rp;
        } break;
        case NLS_DOUBLE: {
            if (sparse) {
                DeleteSparseMatrixDynamicFunction(dataClass, dimensions[0], dimensions[1], cp);
            } else {
                auto* rp = static_cast<double*>(cp);
                delete[] rp;
            }
        } break;
        case NLS_SCOMPLEX: {
            auto* rp = static_cast<single*>(cp);
            delete[] rp;
        } break;
        case NLS_DCOMPLEX: {
            if (sparse) {
                DeleteSparseMatrixDynamicFunction(dataClass, dimensions[0], dimensions[1], cp);
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
    isVectorCache = dimensions.isVector();
    is2DCache = dimensions.is2D();
    isScalarCache = dimensions.isScalar();
    getColumnsCache = dimensions.getColumns();
    getRowsCache = dimensions.getRows();
    getElementCountCache = dimensions.getElementCount();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
