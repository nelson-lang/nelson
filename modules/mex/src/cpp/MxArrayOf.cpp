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
#include "MxArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static mwSize*
GetDimensions(const ArrayOf& array, mwSize& numdims)
{
    numdims = (int)array.getDimensions().getLength();
    mwSize* dim_vec = new mwSize[numdims];
    for (mwSize i = 0; i < numdims; i++) {
        dim_vec[i] = array.getDimensions()[i];
    }
    return dim_vec;
}
//=============================================================================
template <class T, class S>
void
ArrayOfRealToMexReal(T* src, S* dst, size_t count)
{
    for (size_t i = 0; i < count; i++) {
        dst[i] = (S)src[i];
    }
}
//=============================================================================
template <class T, class S>
void
MexRealToArrayOfReal(T* src, S* dst, size_t count)
{
    for (size_t i = 0; i < count; i++) {
        dst[i] = (S)src[i];
    }
}
//=============================================================================
template <class T, class S>
void
MexComplexToArrayOfComplex(T* src_real, T* src_imag, S* dst, size_t count)
{
    for (size_t i = 0; i < count; i++) {
        dst[2 * i] = (S)src_real[i];
        dst[2 * i + 1] = (S)src_imag[i];
    }
}
//=============================================================================
template <class mxType, class nlsType>
mxArray*
ArrayOfComplexToMexArray(const ArrayOf& array, mxClassID classID)
{
    mwSize num_dim;
    mwSize* dim_vec = GetDimensions(array, num_dim);
    mxArray* ret = mxCreateNumericArray(num_dim, dim_vec, classID, mxCOMPLEX);
    free(dim_vec);
    dim_vec = nullptr;
    nlsType* sp = (nlsType*)array.getDataPointer();
    mxType* dp_r = (mxType*)ret->realdata;
    mxType* dp_i = (mxType*)ret->imagdata;
    size_t N = mxGetNumberOfElements(ret);
    for (size_t i = 0; i < N; i++) {
        dp_r[i] = (mxType)sp[2 * i];
        dp_i[i] = (mxType)sp[2 * i + 1];
    }
    return ret;
}
//=============================================================================
template <class mxType, class nlsType>
mxArray*
ArrayOfRealToMexArray(const ArrayOf& array, mxClassID classID)
{
    mwSize num_dim;
    mwSize* dim_vec = GetDimensions(array, num_dim);
    mxArray* ret = mxCreateNumericArray(num_dim, dim_vec, classID, mxREAL);
    free(dim_vec);
    dim_vec = nullptr;
    nlsType* sp = (nlsType*)array.getDataPointer();
    mxType* dp = (mxType*)ret->realdata;
    size_t N = mxGetNumberOfElements(ret);
    for (size_t i = 0; i < N; i++) {
        dp[i] = (mxType)sp[i];
    }
    return ret;
}
//=============================================================================
mxArray*
ArrayOfToMxArray(const ArrayOf& nlsArrayOf)
{
    mxArray* res = nullptr;
    if (nlsArrayOf.isSparse()) {
        Error(_("C MEX type not managed."));
    }
    switch (nlsArrayOf.getDataClass()) {
    case NLS_CELL_ARRAY: {
        mwSize num_dim;
        mwSize* dim_vec = GetDimensions(nlsArrayOf, num_dim);
        res = mxCreateCellArray(num_dim, dim_vec);
        free(dim_vec);
        dim_vec = nullptr;
        ArrayOf* sp = (ArrayOf*)nlsArrayOf.getDataPointer();
        mxArray** dp = (mxArray**)res->realdata;
        size_t N = mxGetNumberOfElements(res);
        for (size_t i = 0; i < N; i++) {
            dp[i] = ArrayOfToMxArray(sp[i]);
        }
    } break;
    case NLS_STRUCT_ARRAY: {
        mxArray* ret = (mxArray*)malloc(sizeof(mxArray));
        if (ret) {
            mwSize num_dim;
            mwSize* dim_vec = GetDimensions(nlsArrayOf, num_dim);
            ret->number_of_dims = num_dim;
            ret->dims = dim_vec;
            ret->classID = mxSTRUCT_CLASS;
            ret->issparse = false;
            ret->iscomplex = false;
            ret->imagdata = nullptr;
            ret->realdata = nullptr;
            ArrayOf* ptr = new ArrayOf(nlsArrayOf);
            ptr->ensureSingleOwner();
            ret->ptr = (uint64_t*)ptr;
        }
        return ret;
    } break;
    case NLS_LOGICAL: {
        res = ArrayOfRealToMexArray<mxLogical, logical>(nlsArrayOf, mxLOGICAL_CLASS);
    } break;
    case NLS_UINT8: {
        res = ArrayOfRealToMexArray<mxUint8, uint8>(nlsArrayOf, mxUINT8_CLASS);
    } break;
    case NLS_INT8: {
        res = ArrayOfRealToMexArray<mxInt8, int8>(nlsArrayOf, mxINT8_CLASS);
    } break;
    case NLS_UINT16: {
        res = ArrayOfRealToMexArray<mxUint16, uint16>(nlsArrayOf, mxUINT16_CLASS);
    } break;
    case NLS_INT16: {
        res = ArrayOfRealToMexArray<mxInt16, int16>(nlsArrayOf, mxINT16_CLASS);
    } break;
    case NLS_UINT32: {
        res = ArrayOfRealToMexArray<mxUint32, uint32>(nlsArrayOf, mxUINT32_CLASS);
    } break;
    case NLS_INT32: {
        res = ArrayOfRealToMexArray<mxInt32, int32>(nlsArrayOf, mxINT32_CLASS);
    } break;
    case NLS_UINT64: {
        res = ArrayOfRealToMexArray<mxUint64, uint64>(nlsArrayOf, mxUINT64_CLASS);
    } break;
    case NLS_INT64: {
        res = ArrayOfRealToMexArray<mxInt64, int64>(nlsArrayOf, mxINT64_CLASS);
    } break;
    case NLS_SINGLE: {
        res = ArrayOfRealToMexArray<mxSingle, single>(nlsArrayOf, mxSINGLE_CLASS);
    } break;
    case NLS_DOUBLE: {
        res = ArrayOfRealToMexArray<mxDouble, double>(nlsArrayOf, mxDOUBLE_CLASS);
    } break;
    case NLS_SCOMPLEX: {
        res = ArrayOfComplexToMexArray<mxSingle, single>(nlsArrayOf, mxSINGLE_CLASS);
    } break;
    case NLS_DCOMPLEX: {
        res = ArrayOfComplexToMexArray<mxDouble, double>(nlsArrayOf, mxDOUBLE_CLASS);
    } break;
    case NLS_CHAR: {
        res = ArrayOfRealToMexArray<mxChar, charType>(nlsArrayOf, mxCHAR_CLASS);
    } break;
    default: {
        Error(_("C MEX type not managed."));
    } break;
    }
    return res;
}
//=============================================================================
ArrayOf
MxArrayToArrayOf(mxArray* pm)
{
    ArrayOf res;
    if (pm == nullptr || (pm->dims == nullptr && pm->number_of_dims == 0)) {
        Dimensions dims(1, 0);
        return ArrayOf::emptyConstructor(dims);
    } else {
        Class destClass = NLS_NOT_TYPED;
        stringVector fieldnames;
        Dimensions dim;
        void* cp = nullptr;
        for (mwSize i = 0; i < pm->number_of_dims; i++) {
            dim[i] = pm->dims[i];
        }
        size_t N = mxGetNumberOfElements(pm);
        switch (pm->classID) {
        case mxCELL_CLASS: {
            destClass = NLS_CELL_ARRAY;
            mxArray** dp = (mxArray**)pm->realdata;
            cp = ArrayOf::allocateArrayOf(destClass, N);
            ArrayOf* elements = (ArrayOf*)cp;
            for (size_t i = 0; i < N; i++) {
                elements[i] = MxArrayToArrayOf(dp[i]);
            }
        } break;
        case mxSTRUCT_CLASS: {
            ArrayOf* ptr = (ArrayOf*)pm->ptr;
            res = ArrayOf(*ptr);
            res.ensureSingleOwner();
            return res;
        } break;
        case mxLOGICAL_CLASS: {
            destClass = NLS_LOGICAL;
            if (pm->iscomplex) {
                Error(_("C MEX type not managed."));
            }
            cp = ArrayOf::allocateArrayOf(destClass, N);
            MexRealToArrayOfReal<mxLogical, logical>((mxLogical*)pm->realdata, (logical*)cp, N);
        } break;
        case mxCHAR_CLASS: {
            destClass = NLS_CHAR;
            if (pm->iscomplex) {
                Error(_("C MEX type not managed."));
            }
            cp = ArrayOf::allocateArrayOf(destClass, N);
            MexRealToArrayOfReal<mxChar, charType>((mxChar*)pm->realdata, (charType*)cp, N);
        } break;
        case mxDOUBLE_CLASS: {
            if (pm->iscomplex) {
                destClass = NLS_DCOMPLEX;
                cp = ArrayOf::allocateArrayOf(destClass, N);
                MexComplexToArrayOfComplex<mxDouble, double>(
                    (mxDouble*)pm->realdata, (mxDouble*)pm->imagdata, (double*)cp, N);
            } else {
                destClass = NLS_DOUBLE;
                cp = ArrayOf::allocateArrayOf(destClass, N);
                MexRealToArrayOfReal<mxDouble, double>((mxDouble*)pm->realdata, (double*)cp, N);
            }
        } break;
        case mxSINGLE_CLASS: {
            if (pm->iscomplex) {
                destClass = NLS_SCOMPLEX;
                cp = ArrayOf::allocateArrayOf(destClass, N);
                MexComplexToArrayOfComplex<mxSingle, single>(
                    (mxSingle*)pm->realdata, (mxSingle*)pm->imagdata, (single*)cp, N);
            } else {
                destClass = NLS_SINGLE;
                cp = ArrayOf::allocateArrayOf(destClass, N);
                MexRealToArrayOfReal<mxSingle, single>((mxSingle*)pm->realdata, (single*)cp, N);
            }
        } break;
        case mxINT8_CLASS: {
            destClass = NLS_INT8;
            if (pm->iscomplex) {
                Error(_("C MEX type not managed."));
            }
            cp = ArrayOf::allocateArrayOf(destClass, N);
            MexRealToArrayOfReal<mxInt8, int8>((mxInt8*)pm->realdata, (int8*)cp, N);
        } break;
        case mxUINT8_CLASS: {
            destClass = NLS_UINT8;
            if (pm->iscomplex) {
                Error(_("C MEX type not managed."));
            }
            cp = ArrayOf::allocateArrayOf(destClass, N);
            MexRealToArrayOfReal<mxUint8, uint8>((mxUint8*)pm->realdata, (uint8*)cp, N);
        } break;
        case mxINT16_CLASS: {
            destClass = NLS_INT16;
            if (pm->iscomplex) {
                Error(_("C MEX type not managed."));
            }
            cp = ArrayOf::allocateArrayOf(destClass, N);
            MexRealToArrayOfReal<mxInt16, int16>((mxInt16*)pm->realdata, (int16*)cp, N);
        } break;
        case mxUINT16_CLASS: {
            destClass = NLS_UINT16;
            if (pm->iscomplex) {
                Error(_("C MEX type not managed."));
            }
            cp = ArrayOf::allocateArrayOf(destClass, N);
            MexRealToArrayOfReal<mxUint16, uint16>((mxUint16*)pm->realdata, (uint16*)cp, N);
        } break;
        case mxINT32_CLASS: {
            destClass = NLS_INT32;
            if (pm->iscomplex) {
                Error(_("C MEX type not managed."));
            }
            cp = ArrayOf::allocateArrayOf(destClass, N);
            MexRealToArrayOfReal<mxInt32, int32>((mxInt32*)pm->realdata, (int32*)cp, N);
        } break;
        case mxUINT32_CLASS: {
            destClass = NLS_UINT32;
            if (pm->iscomplex) {
                Error(_("C MEX type not managed."));
            }
            cp = ArrayOf::allocateArrayOf(destClass, N);
            MexRealToArrayOfReal<mxUint32, uint32>((mxUint32*)pm->realdata, (uint32*)cp, N);
        } break;
        case mxINT64_CLASS: {
            destClass = NLS_INT64;
            if (pm->iscomplex) {
                Error(_("C MEX type not managed."));
            }
            cp = ArrayOf::allocateArrayOf(destClass, N);
            MexRealToArrayOfReal<mxInt64, int64>((mxInt64*)pm->realdata, (int64*)cp, N);
        } break;
        case mxUINT64_CLASS: {
            destClass = NLS_UINT64;
            if (pm->iscomplex) {
                Error(_("C MEX type not managed."));
            }
            cp = ArrayOf::allocateArrayOf(destClass, N);
            MexRealToArrayOfReal<mxUint64, uint64>((mxUint64*)pm->realdata, (uint64*)cp, N);
        } break;
        default: {
            Error(_("C MEX type not managed."));
        } break;
        }
        res = ArrayOf(destClass, dim, cp);
    }
    return res;
}
//=============================================================================
}
//=============================================================================
