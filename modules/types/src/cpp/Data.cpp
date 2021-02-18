//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
//=============================================================================
#include <utility>
//=============================================================================
#include "Data.hpp"
#include "SparseDynamicFunctions.hpp"
#include "SparseType.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Data::Data(Class aClass, const Dimensions& dims, void* s, bool sparseflag, stringVector fields)
    : cp(s), owners(1), dimensions(dims), fieldNames(std::move(fields)), dataClass(aClass)
{
    sparse = sparseflag;
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
    Class aClass, const Dimensions& dims, void* s, bool sparseflag, const stringVector& fields)
{
    if ((owners <= 1)) {
        freeDataBlock();
        cp = s;
        dataClass = aClass;
        dimensions = dims;
        fieldNames = fields;
        sparse = sparseflag;
        owners = 1;
        return this;
    }
    owners--;
    return new Data(aClass, dims, s, sparseflag, fields);
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
}
//=============================================================================
void
Data::setFieldNames(const stringVector& fields)
{
    fieldNames = fields;
}
//=============================================================================
std::string
Data::getStructTypeName()
{
    return structTypeName;
}
//=============================================================================
void
Data::setStructTypeName(const std::string& typeName)
{
    structTypeName = typeName;
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
        if (ArrayOf::isDataClassReferenceType(dataClass)) {
            auto* rp = static_cast<ArrayOf*>(cp);
            delete[] rp;
        } else if (sparse) {
            DeleteSparseMatrixDynamicFunction(dataClass, dimensions[0], dimensions[1], cp);
        } else {
            switch (dataClass) {
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
            case NLS_STRUCT_ARRAY: {
                auto* rp = static_cast<ArrayOf*>(cp);
                delete[] rp;
            } break;
            case NLS_LOGICAL: {
                auto* rp = static_cast<logical*>(cp);
                delete[] rp;
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
                auto* rp = static_cast<double*>(cp);
                delete[] rp;
            } break;
            case NLS_SCOMPLEX: {
                auto* rp = static_cast<single*>(cp);
                delete[] rp;
            } break;
            case NLS_DCOMPLEX: {
                auto* rp = static_cast<double*>(cp);
                delete[] rp;
            } break;
            case NLS_CHAR: {
                auto* rp = static_cast<charType*>(cp);
                delete[] rp;
            } break;
            default: { } break; }
            cp = nullptr;
        }
    }
}
//=============================================================================
bool
Data::isSparse()
{
    return sparse;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
