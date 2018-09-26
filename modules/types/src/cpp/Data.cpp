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

#include "Data.hpp"
#include "SparseDynamicFunctions.hpp"
#include "SparseType.hpp"

namespace Nelson {

Data::Data(
    Class aClass, const Dimensions& dims, void* s, bool sparseflag, const stringVector& fields)
    : cp(s), owners(1), dimensions(dims), fieldNames(fields), dataClass(aClass)
{
    sparse = sparseflag;
}

Data::~Data() { freeDataBlock(); }

Data*
Data::getCopy()
{
    owners++;
    return this;
}

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
    } else {
        owners--;
        return new Data(aClass, dims, s, sparseflag, fields);
    }
}

int
Data::deleteCopy()
{
    return owners--;
}

const void*
Data::getData() const
{
    return cp;
}

void*
Data::getWriteableData()
{
    return cp;
}

const Dimensions&
Data::getDimensions() const
{
    return dimensions;
}

const stringVector&
Data::getFieldNames() const
{
    return fieldNames;
}

void
Data::setDimensions(const Dimensions& dim)
{
    dimensions = dim;
}

void
Data::setFieldNames(const stringVector& fields)
{
    fieldNames = fields;
}

std::string
Data::getStructTypeName()
{
    return structTypeName;
}

void
Data::setStructTypeName(std::string typeName)
{
    structTypeName = typeName;
}

int
Data::numberOfOwners() const
{
    return owners;
}

void
Data::freeDataBlock()
{
    if (cp) {
        if (ArrayOf::isDataClassReferenceType(dataClass)) {
            ArrayOf* rp = (ArrayOf*)cp;
            delete[] rp;
        } else if (sparse) {
            DeleteSparseMatrixDynamicFunction(dataClass, dimensions[0], dimensions[1], cp);
        } else {
            switch (dataClass) {
            case NLS_CELL_ARRAY: {
                ArrayOf* rp = (ArrayOf*)cp;
                delete[] rp;
            } break;
            case NLS_STRING_ARRAY: {
                ArrayOf* rp = (ArrayOf*)cp;
                delete[] rp;
            } break;
            case NLS_HANDLE: {
                nelson_handle* rp = (nelson_handle*)cp;
                delete[] rp;
            } break;
            case NLS_STRUCT_ARRAY: {
                ArrayOf* rp = (ArrayOf*)cp;
                delete[] rp;
            } break;
            case NLS_LOGICAL: {
                logical* rp = (logical*)cp;
                delete[] rp;
            } break;
            case NLS_UINT8: {
                uint8* rp = (uint8*)cp;
                delete[] rp;
            } break;
            case NLS_INT8: {
                int8* rp = (int8*)cp;
                delete[] rp;
            } break;
            case NLS_UINT16: {
                uint16* rp = (uint16*)cp;
                delete[] rp;
            } break;
            case NLS_INT16: {
                int16* rp = (int16*)cp;
                delete[] rp;
            } break;
            case NLS_UINT32: {
                uint32* rp = (uint32*)cp;
                delete[] rp;
            } break;
            case NLS_INT32: {
                int32* rp = (int32*)cp;
                delete[] rp;
            } break;
            case NLS_UINT64: {
                uint64* rp = (uint64*)cp;
                delete[] rp;
            } break;
            case NLS_INT64: {
                int64* rp = (int64*)cp;
                delete[] rp;
            } break;
            case NLS_SINGLE: {
                single* rp = (single*)cp;
                delete[] rp;
            } break;
            case NLS_DOUBLE: {
                double* rp = (double*)cp;
                delete[] rp;
            } break;
            case NLS_SCOMPLEX: {
                single* rp = (single*)cp;
                delete[] rp;
            } break;
            case NLS_DCOMPLEX: {
                double* rp = (double*)cp;
                delete[] rp;
            } break;
            case NLS_CHAR: {
                charType* rp = (charType*)cp;
                delete[] rp;
            } break;
            }
            cp = nullptr;
        }
    }
}

bool
Data::isSparse()
{
    return sparse;
}
}
