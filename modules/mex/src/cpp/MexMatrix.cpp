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
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include <limits>
#include <cstdlib>
#include <set>
#include <algorithm>
#include <cmath>
#include <cstring>
#include "matrix.h"
#include "characters_encoding.hpp"
#include "mex.h"
#include "i18n.hpp"
#include "ArrayOf.hpp"
#include "MexConverters.hpp"
//=============================================================================
static std::set<void*> registeredMxPointers;
//=============================================================================
static void
registerMexPointer(void* ptr)
{
    if (ptr) {
        registeredMxPointers.insert(ptr);
    }
}
//=============================================================================
static void
deRegisterMexPointer(void* ptr)
{
    if (ptr) {
        registeredMxPointers.erase(ptr);
    }
}
//=============================================================================
static mwSize
countElements(mwSize ndim, const mwSize* dims)
{
    mwSize count = 1;
    for (mwSize i = 0; i < ndim; i++) {
        count *= dims[i];
    }
    return count;
}
//=============================================================================
static mwSize*
copyDims(mwSize ndim, const mwSize* dims)
{
    mwSize* p = (mwSize*)malloc(sizeof(mwSize) * ndim);
    if (p) {
        for (mwSize i = 0; i < ndim; i++) {
            p[i] = dims[i];
        }
    }
    return p;
}
//=============================================================================
static mwSize
sizeFromClass(mxClassID classid)
{
    switch (classid) {
    case mxCELL_CLASS:
        return sizeof(mxArray*);
    case mxSTRUCT_CLASS:
        return sizeof(mxArray*);
    case mxLOGICAL_CLASS:
        return sizeof(mxLogical);
    case mxCHAR_CLASS:
        return sizeof(mxChar);
    case mxDOUBLE_CLASS:
        return sizeof(mxDouble);
    case mxSINGLE_CLASS:
        return sizeof(mxSingle);
    case mxINT8_CLASS:
        return sizeof(mxInt8);
    case mxUINT8_CLASS:
        return sizeof(mxInt8);
    case mxINT16_CLASS:
        return sizeof(mxInt16);
    case mxUINT16_CLASS:
        return sizeof(mxUint16);
    case mxINT32_CLASS:
        return sizeof(mxInt32);
    case mxUINT32_CLASS:
        return sizeof(mxUint32);
    case mxINT64_CLASS:
        return sizeof(mxInt64);
    case mxUINT64_CLASS:
        return sizeof(mxUint64);
    default:
        return 0;
    }
}
//=============================================================================
void*
mxCalloc(mwSize n, mwSize size)
{
    void* p = calloc(n, size);
    registerMexPointer(p);
    return p;
}
//=============================================================================
void*
mxMalloc(mwSize n)
{
    void* p = malloc(n);
    registerMexPointer(p);
    return p;
}
//=============================================================================
void
mxFree(void* ptr)
{
    if (ptr) {
        deRegisterMexPointer(ptr);
        free(ptr);
    }
}
//=============================================================================
void*
mxRealloc(void* ptr, mwSize size)
{
    if (ptr) {
        deRegisterMexPointer(ptr);
    }
    ptr = realloc(ptr, size);

    if (ptr) {
        registerMexPointer(ptr);
    }
    return ptr;
}
//=============================================================================
void
mxDestroyArray(mxArray* pm)
{
    if (pm) {
        if (pm->classID == mxCELL_CLASS) {
            mxArray** gp = (mxArray**)pm->realdata;
            size_t L = mxGetNumberOfElements(pm);
            for (size_t i = 0; i < L; i++) {
                mxArray* p = gp[i];
                mxDestroyArray(p);
            }
        }
        if (pm->classID == mxSTRUCT_CLASS) {
            Nelson::ArrayOf* ptr = (Nelson::ArrayOf*)pm->ptr;
            delete ptr;
            pm->ptr = nullptr;
        }
        mxFree(pm->realdata);
        mxFree(pm->imagdata);
        mxFree(pm);
    }
}
//=============================================================================
mxArray*
mxNewArray()
{
    return (mxArray*)malloc(sizeof(mxArray));
}
//=============================================================================
static mxArray*
mxAllocateRealArray(
    mwSize ndim, const mwSize* dims, size_t size, mxClassID classID, bool initialized = true)
{
    mxArray* ret = mxNewArray();
    if (ret) {
        ret->classID = classID;
        ret->number_of_dims = ndim;
        ret->dims = copyDims(ndim, dims);
        ret->issparse = false;
        ret->iscomplex = false;
        if (initialized) {
            ret->realdata = mxCalloc(countElements(ndim, dims), size);

        } else {
            ret->realdata = mxMalloc(countElements(ndim, dims));
        }
        ret->imagdata = nullptr;
    }
    return ret;
}
//=============================================================================
static mxArray*
mxAllocateComplexArray(
    mwSize ndim, const mwSize* dims, size_t size, mxClassID classID, bool initialized = true)
{
    mxArray* ret = mxNewArray();
    if (ret) {
        ret->classID = classID;
        ret->number_of_dims = ndim;
        ret->dims = copyDims(ndim, dims);
        ret->issparse = false;
        ret->iscomplex = true;
        if (initialized) {
            ret->realdata = mxCalloc(countElements(ndim, dims), size);
            ret->imagdata = mxCalloc(countElements(ndim, dims), size);
        } else {
            ret->realdata = mxMalloc(countElements(ndim, dims));
            ret->imagdata = mxMalloc(countElements(ndim, dims));
        }
    }
    return ret;
}
//=============================================================================
size_t
mxGetNumberOfElements(const mxArray* array_ptr)
{
    return (size_t)countElements(array_ptr->number_of_dims, array_ptr->dims);
}
//=============================================================================
mxArray*
mxCreateNumericArray(mwSize ndim, const mwSize* dims, mxClassID classid, mxComplexity ComplexFlag)
{
    if (ComplexFlag == mxREAL) {
        return mxAllocateRealArray(ndim, dims, sizeFromClass(classid), classid);
    }
    return mxAllocateComplexArray(ndim, dims, sizeFromClass(classid), classid);
}
//=============================================================================
char*
mxArrayToString(const mxArray* array_ptr)
{
    if (array_ptr->classID != mxCHAR_CLASS) {
        return nullptr;
    }
    mxChar* p = (mxChar*)(array_ptr->realdata);
    size_t N = mxGetNumberOfElements(array_ptr);
    char* res = (char*)malloc(N + 1);
    if (res) {
        for (size_t i = 0; i < N; i++) {
            res[i] = (char)p[i];
        }
        res[N] = 0;
    }
    return res;
}
//=============================================================================
char*
mxArrayToUTF8String(const mxArray* array_ptr)
{
    if (array_ptr->classID != mxCHAR_CLASS) {
        return nullptr;
    }
    mxChar* p = (mxChar*)(array_ptr->realdata);
    size_t N = mxGetNumberOfElements(array_ptr);
    wchar_t* res = new wchar_t[N + (size_t)1];
    if (res) {
        for (size_t i = 0; i < N; i++) {
            res[i] = p[i];
        }
        res[N] = 0;
        std::string ustr = Nelson::wstring_to_utf8(res);
        delete[] res;
        char* utfres = (char*)malloc(sizeof(char) * (ustr.size() + 1));
        if (utfres) {
            strcpy(utfres, ustr.c_str());
        }
        return utfres;
    }
    return nullptr;
}
//=============================================================================
int
mxGetString(const mxArray* pm, char* str, mwSize strlen)
{
    mxChar* ptr;
    size_t i;
    size_t elcount, tocopy;
    if (pm->classID != mxCHAR_CLASS) {
        return 1;
    }
    ptr = (mxChar*)pm->realdata;
    elcount = mxGetNumberOfElements(pm);
    tocopy = std::min((mwSize)elcount, (mwSize)(strlen - 1));
    for (i = 0; i < tocopy; i++) {
        str[i] = (char)ptr[i];
    }
    str[tocopy] = 0;
    if (tocopy < elcount) {
        return 1;
    }
    return 0;
}
//=============================================================================
mxArray*
mxCreateString(const char* str)
{
    return mxCreateCharMatrixFromStrings(1, &str);
}
//=============================================================================
mxArray*
mxCreateCharMatrixFromStrings(mwSize m, const char** str)
{
    mwSize dims[2];
    size_t maxlen = 0;
    mwSize i, j;
    mxChar* ptr;

    for (i = 0; i < m; i++) {
        maxlen = std::max(maxlen, strlen(str[i]));
    }
    dims[0] = m;
    dims[1] = maxlen;
    mxArray* res = mxCreateCharArray(2, dims);
    ptr = (mxChar*)res->realdata;
    for (i = 0; i < m; i++) {
        for (j = 0; j < strlen(str[i]); j++) {
            ptr[i + j * m] = str[i][j];
        }
    }
    return res;
}
//=============================================================================
mxArray*
mxCreateCharArray(mwSize ndim, const mwSize* dims)
{
    return mxAllocateRealArray(ndim, dims, sizeof(mxChar), mxCHAR_CLASS);
}
//=============================================================================
mxArray*
mxCreateDoubleMatrix(mwSize m, mwSize n, mxComplexity ComplexFlag)
{
    mwSize dims[2];
    dims[0] = m;
    dims[1] = n;
    if (ComplexFlag == mxREAL) {
        return mxAllocateRealArray(2, dims, sizeof(mxDouble), mxDOUBLE_CLASS);
    }
    return mxAllocateComplexArray(2, dims, sizeof(mxDouble), mxDOUBLE_CLASS);
}
//=============================================================================
double
mxGetNaN(void)
{
    return std::nan("NaN");
}
//=============================================================================
double
mxGetInf(void)
{
    return std::numeric_limits<double>::infinity();
}
//=============================================================================
double
mxGetEps(void)
{
    return std::numeric_limits<double>::epsilon();
}
//=============================================================================
bool
mxIsNumeric(const mxArray* pm)
{
    if (pm == nullptr) {
        return false;
    }
    return (pm->classID == mxDOUBLE_CLASS || pm->classID == mxSINGLE_CLASS
        || pm->classID == mxINT8_CLASS || pm->classID == mxUINT8_CLASS
        || pm->classID == mxINT16_CLASS || pm->classID == mxUINT16_CLASS
        || pm->classID == mxINT32_CLASS || pm->classID == mxUINT32_CLASS
        || pm->classID == mxINT64_CLASS || pm->classID == mxUINT64_CLASS);
}
//=============================================================================
bool
mxIsComplex(const mxArray* pm)
{
    if (pm == nullptr) {
        return false;
    }
    return (pm->iscomplex != 0);
}
//=============================================================================
mwSize
mxGetNumberOfDimensions(const mxArray* pm)
{
    if (pm == nullptr) {
        return 0;
    }
    return (pm->number_of_dims);
}
//=============================================================================
size_t
mxGetElementSize(const mxArray* pm)
{
    if (pm == nullptr) {
        return 0;
    }
    return sizeFromClass(pm->classID);
}
//=============================================================================
const mwSize*
mxGetDimensions(const mxArray* pm)
{
    if (pm == nullptr) {
        return nullptr;
    }
    return pm->dims;
}
//=============================================================================
int
mxSetDimensions(mxArray* pm, const mwSize* dims, mwSize ndim)
{
    if (pm == nullptr) {
        return 1;
    }
    while ((ndim > 2) && (dims[ndim - 1] == 1)) {
        ndim--;
    }
    pm->number_of_dims = ndim;
    free(pm->dims);
    pm->dims = copyDims(ndim, dims);
    return 0;
}
//=============================================================================
mwIndex
mxCalcSingleSubscript(const mxArray* pm, mwSize nsubs, mwIndex* subs)
{
    mwIndex index = 0;
    mwIndex iMult = 1;
    if (pm != nullptr) {
        const mwSize* dims = mxGetDimensions(pm);
        for (mwSize i = 0; i < nsubs; i++) {
            index += subs[i] * iMult;
            iMult *= dims[i];
        }
    }
    return index;
}
//=============================================================================
size_t
mxGetM(const mxArray* pm)
{
    if (pm != nullptr) {
        return (size_t)pm->dims[0];
    }
    return 0;
}
//=============================================================================
size_t
mxGetN(const mxArray* pm)
{
    if (pm != nullptr) {
        return (size_t)pm->dims[1];
    }
    return 0;
}
//=============================================================================
void
mxSetM(mxArray* pm, mwSize m)
{
    if (pm != nullptr) {
        pm->dims[0] = m;
    }
}
//=============================================================================
void
mxSetN(mxArray* pm, mwSize n)
{
    if (pm != nullptr) {
        pm->dims[1] = n;
    }
}
//=============================================================================
bool
mxIsEmpty(const mxArray* pm)
{
    return (mxGetNumberOfElements(pm) == 0);
}
//=============================================================================
bool
mxIsFromGlobalWS(const mxArray* pm)
{
    mexErrMsgTxt("mxIsFromGlobalWS() is not yet unimplemented.");
    return false;
}
//=============================================================================
bool
mxIsInf(double value)
{
    return std::isinf(value);
}
//=============================================================================
bool
mxIsNaN(double value)
{
    return std::isnan(value);
}
//=============================================================================
bool
mxIsFinite(double value)
{
    return std::isfinite(value);
}
//=============================================================================
bool
mxIsClass(const mxArray* pm, const char* classname)
{
    bool res = false;
    if (pm == nullptr) {
        return false;
    }
    if (strcmp(classname, "cell") == 0) {
        return (pm->classID == mxCELL_CLASS);
    }
    if (strcmp(classname, "char") == 0) {
        return (pm->classID == mxCHAR_CLASS);
    }
    if (strcmp(classname, "double") == 0) {
        return (pm->classID == mxDOUBLE_CLASS);
    }
    if (strcmp(classname, "function_handle") == 0) {
        return (pm->classID == mxFUNCTION_CLASS);
    }
    if (strcmp(classname, "int8") == 0) {
        return (pm->classID == mxINT8_CLASS);
    }
    if (strcmp(classname, "int16") == 0) {
        return (pm->classID == mxINT16_CLASS);
    }
    if (strcmp(classname, "int32") == 0) {
        return (pm->classID == mxINT32_CLASS);
    }
    if (strcmp(classname, "int64") == 0) {
        return (pm->classID == mxINT64_CLASS);
    }
    if (strcmp(classname, "logical") == 0) {
        return (pm->classID == mxLOGICAL_CLASS);
    }
    if (strcmp(classname, "single") == 0) {
        return (pm->classID == mxSINGLE_CLASS);
    }
    if (strcmp(classname, "struct") == 0) {
        return (pm->classID == mxSTRUCT_CLASS);
    }
    if (strcmp(classname, "uint8") == 0) {
        return (pm->classID == mxUINT8_CLASS);
    }
    if (strcmp(classname, "uint16") == 0) {
        return (pm->classID == mxUINT16_CLASS);
    }
    if (strcmp(classname, "uint32") == 0) {
        return (pm->classID == mxUINT32_CLASS);
    }
    if (strcmp(classname, "uint64") == 0) {
        return (pm->classID == mxUINT64_CLASS);
    }
    if (strcmp(classname, "unknown") == 0) {
        return (pm->classID == mxUNKNOWN_CLASS);
    }
    return res;
}
//=============================================================================
void
mexPrintAssertion(const char* test, const char* fname, int linenum, const char* message)
{
    if (test != nullptr) {
        if (message && message[0]) {
            mexErrMsgIdAndTxt("Nelson:MEX",
                _("Assertion failed: %s, at line %d of file \"%s\".\n%s\n").c_str(), test, linenum,
                fname, message);
        } else {
            mexErrMsgIdAndTxt("Nelson:MEX",
                _("Assertion failed: %s, at line %d of file \"%s\".\n").c_str(), test, linenum,
                fname);
        }
    }
}
//=============================================================================
mxArray*
mxCreateDoubleScalar(double value)
{
    mxArray* ret = mxCreateDoubleMatrix(1, 1, mxREAL);
    if (ret) {
        ((double*)ret->realdata)[0] = value;
    }
    return ret;
}
//=============================================================================
mxArray*
mxCreateNumericMatrix(mwSize m, mwSize n, mxClassID classid, mxComplexity ComplexFlag)
{
    mwSize dims[2];
    dims[0] = m;
    dims[1] = n;
    return mxCreateNumericArray(2, dims, classid, ComplexFlag);
}
//=============================================================================
mxArray*
mxCreateUninitNumericMatrix(size_t m, size_t n, mxClassID classid, mxComplexity ComplexFlag)
{
    mwSize dims[2];
    dims[0] = m;
    dims[1] = n;
    if (ComplexFlag == mxREAL) {
        return mxAllocateRealArray(2, dims, sizeFromClass(classid), classid, false);
    }
    return mxAllocateComplexArray(2, dims, sizeFromClass(classid), classid, false);
}
//=============================================================================
mxArray*
mxCreateUninitNumericArray(size_t ndim, size_t* dims, mxClassID classid, mxComplexity ComplexFlag)
{
    if (ComplexFlag == mxREAL) {
        return mxAllocateRealArray(ndim, dims, sizeFromClass(classid), classid, false);
    }
    return mxAllocateComplexArray(ndim, dims, sizeFromClass(classid), classid, false);
}
//=============================================================================
bool
mxIsScalar(const mxArray* array_ptr)
{
    return (mxGetNumberOfElements(array_ptr) == 1);
}
//=============================================================================
double
mxGetScalar(const mxArray* pm)
{
    if (pm == nullptr) {
        return 0;
    }
    if (mxIsEmpty(pm)) {
        return 0;
    }
    switch (pm->classID) {
    case mxCELL_CLASS:
        return 0;
    case mxSTRUCT_CLASS:
        return 0;
    case mxLOGICAL_CLASS:
        return ((mxLogical*)pm->realdata)[0];
    case mxCHAR_CLASS:
        return ((mxChar*)pm->realdata)[0];
    case mxDOUBLE_CLASS:
        return ((mxDouble*)pm->realdata)[0];
    case mxSINGLE_CLASS:
        return ((mxSingle*)pm->realdata)[0];
    case mxINT8_CLASS:
        return ((mxInt8*)pm->realdata)[0];
    case mxUINT8_CLASS:
        return ((mxUint8*)pm->realdata)[0];
    case mxINT16_CLASS:
        return ((mxInt16*)pm->realdata)[0];
    case mxUINT16_CLASS:
        return ((mxUint16*)pm->realdata)[0];
    case mxINT32_CLASS:
        return ((mxInt32*)pm->realdata)[0];
    case mxUINT32_CLASS:
        return ((mxUint32*)pm->realdata)[0];
    case mxINT64_CLASS:
        return (mxDouble)((mxInt64*)pm->realdata)[0];
    case mxUINT64_CLASS:
        return (mxDouble)((mxUint64*)pm->realdata)[0];
    default:
        return 0;
    }
    return 0;
}
//=============================================================================
bool
mxIsDouble(const mxArray* pm)
{
    if (pm) {
        return (pm->classID == mxDOUBLE_CLASS);
    }
    return false;
}
//=============================================================================
mxDouble*
mxGetDoubles(const mxArray* pa)
{
    return nullptr;
}
//=============================================================================
int
mxSetDoubles(mxArray* pa, mxDouble* dt)
{
    return 0;
}
//=============================================================================
bool
mxIsSingle(const mxArray* pm)
{
    if (pm) {
        return (pm->classID == mxSINGLE_CLASS);
    }
    return false;
}
//=============================================================================
mxSingle*
mxGetSingles(const mxArray* pa)
{
    return nullptr;
}
//=============================================================================
int
mxSetSingles(mxArray* pa, mxSingle* dt)
{
    return 0;
}
//=============================================================================
mxDouble*
mxGetPr(const mxArray* pm)
{
    if (pm) {
        return (mxDouble*)pm->realdata;
    }
    return nullptr;
}
//=============================================================================
void
mxSetPr(mxArray* pm, double* pr)
{
    if (pm) {
        pm->realdata = pr;
    }
}
//=============================================================================
mxArray*
mxCreateCellArray(mwSize ndim, const mwSize* dims)
{
    return mxAllocateRealArray(ndim, dims, sizeof(void*), mxCELL_CLASS);
}
//=============================================================================
mxArray*
mxCreateCellMatrix(mwSize m, mwSize n)
{
    mwSize dims[2];
    dims[0] = m;
    dims[1] = n;
    return mxCreateCellArray(2, dims);
}
//=============================================================================
bool
mxIsCell(const mxArray* pm)
{
    if (pm) {
        return (pm->classID == mxCELL_CLASS);
    }
    return false;
}
//=============================================================================
mxArray*
mxGetCell(const mxArray* pm, mwIndex index)
{
    if (pm) {
        return (((mxArray**)pm->realdata)[index]);
    }
    return nullptr;
}
//=============================================================================
void
mxSetCell(mxArray* pm, mwIndex index, mxArray* value)
{
    if (pm == nullptr) {
        return;
    }
    if (!mxIsCell(pm)) {
        return;
    }
    ((mxArray**)pm->realdata)[index] = value;
}
//=============================================================================
static mwSize*
GetDimensions(const Nelson::ArrayOf& array, mwSize& numdims)
{
    numdims = (int)array.getDimensions().getLength();
    mwSize* dim_vec = new mwSize[numdims];
    for (mwSize i = 0; i < numdims; i++) {
        dim_vec[i] = array.getDimensions()[i];
    }
    return dim_vec;
}
//=============================================================================
mxArray*
mxDuplicateArray(const mxArray* in)
{
    if (in == nullptr) {
        return nullptr;
    }
    size_t L = mxGetNumberOfElements(in);
    mxArray* ret = nullptr;
    switch (in->classID) {
    case mxCELL_CLASS: {
        ret = mxAllocateRealArray(
            in->number_of_dims, in->dims, sizeFromClass(in->classID), in->classID);
        mxArray** g = (mxArray**)ret->realdata;
        mxArray** h = (mxArray**)in->realdata;
        for (size_t i = 0; i < L; i++) {
            g[i] = mxDuplicateArray(h[i]);
        }
    } break;
    case mxSTRUCT_CLASS: {
        Nelson::ArrayOf* inPtr = (Nelson::ArrayOf*)in->ptr;
        ret = (mxArray*)malloc(sizeof(mxArray));
        if (ret) {
            mwSize num_dim;
            mwSize* dim_vec = GetDimensions(*inPtr, num_dim);
            ret->number_of_dims = num_dim;
            ret->dims = dim_vec;
            ret->classID = mxSTRUCT_CLASS;
            ret->issparse = false;
            ret->iscomplex = false;
            ret->imagdata = nullptr;
            ret->realdata = nullptr;
            Nelson::ArrayOf* ptr = new Nelson::ArrayOf(*inPtr);
            ptr->ensureSingleOwner();
            ret->ptr = (uint64_t*)ptr;
        }
    } break;
    case mxLOGICAL_CLASS:
    case mxCHAR_CLASS:
    case mxDOUBLE_CLASS:
    case mxSINGLE_CLASS:
    case mxINT8_CLASS:
    case mxUINT8_CLASS:
    case mxINT16_CLASS:
    case mxUINT16_CLASS:
    case mxINT32_CLASS:
    case mxUINT32_CLASS:
    case mxINT64_CLASS:
    case mxUINT64_CLASS: {
        if (in->iscomplex) {
            ret = mxAllocateComplexArray(
                in->number_of_dims, in->dims, sizeFromClass(in->classID), in->classID);
            memcpy(ret->realdata, in->realdata, mxGetElementSize(in) * L);
            memcpy(ret->imagdata, in->imagdata, mxGetElementSize(in) * L);
        } else {
            ret = mxAllocateRealArray(
                in->number_of_dims, in->dims, sizeFromClass(in->classID), in->classID);
            memcpy(ret->realdata, in->realdata, mxGetElementSize(in) * L);
        }
        return ret;
    } break;
    default: {
        mexErrMsgTxt("C MEX type not managed.");
    } break;
    }
    return ret;
}
//=============================================================================
mxArray*
mxCreateStructArray(mwSize ndim, const mwSize* dims, int nfields, const char** fieldnames)
{
    mxArray* ret = mxNewArray();
    if (ret) {
        Nelson::stringVector _fieldnames;
        for (size_t k = 0; k < nfields; ++k) {
            _fieldnames.push_back(fieldnames[k]);
        }
        Nelson::Dimensions _dims;
        for (mwSize k = 0; k < ndim; ++k) {
            _dims[k] = dims[k];
        }
        Nelson::ArrayOf* st = (Nelson::ArrayOf*)Nelson::ArrayOf::allocateArrayOf(
            Nelson::NLS_STRUCT_ARRAY, _dims.getElementCount(), _fieldnames, true);
        Nelson::ArrayOf s
            = Nelson::ArrayOf(Nelson::NLS_STRUCT_ARRAY, _dims, st, false, _fieldnames);

        mwSize num_dim;
        mwSize* dim_vec = GetDimensions(s, num_dim);
        ret->number_of_dims = num_dim;
        ret->dims = dim_vec;
        ret->classID = mxSTRUCT_CLASS;
        ret->issparse = false;
        ret->iscomplex = false;
        ret->imagdata = nullptr;
        ret->realdata = nullptr;
        Nelson::ArrayOf* ptr = new Nelson::ArrayOf(s);
        ptr->ensureSingleOwner();
        ret->ptr = (uint64_t*)ptr;
    }
    return ret;
}
//=============================================================================
mxArray*
mxCreateStructMatrix(mwSize m, mwSize n, int nfields, const char** fieldnames)
{
    mwSize dims[2];
    dims[0] = m;
    dims[1] = n;
    return mxCreateStructArray(2, dims, nfields, fieldnames);
}
//=============================================================================
bool
mxIsStruct(const mxArray* pm)
{
    if (pm) {
        return (pm->classID == mxSTRUCT_CLASS);
    }
    return false;
}
//=============================================================================
int
mxGetNumberOfFields(const mxArray* pm)
{
    if (mxIsStruct(pm)) {
        Nelson::ArrayOf* ptr = (Nelson::ArrayOf*)pm->ptr;
        Nelson::stringVector fieldnames = ptr->getFieldNames();
        return (int)fieldnames.size();
    }
    return 0;
}
//=============================================================================
mxArray*
mxGetFieldByNumber(const mxArray* pm, mwIndex index, int fieldnumber)
{
    if (!mxIsStruct(pm)) {
        return nullptr;
    }
    if (index >= mxGetNumberOfElements(pm) || index < 0) {
        return nullptr;
    }
    if (fieldnumber >= mxGetNumberOfFields(pm) || fieldnumber < 0) {
        return nullptr;
    }
    Nelson::ArrayOf* ptr = (Nelson::ArrayOf*)pm->ptr;
    const Nelson::ArrayOf* qp = (const Nelson::ArrayOf*)ptr->getDataPointer();
    size_t fieldCount = ptr->getFieldNames().size();
    Nelson::ArrayOf field = qp[index * fieldCount + fieldnumber];
    return Nelson::ArrayOfToMxArray(field);
}
//=============================================================================
mxArray*
mxCreateLogicalScalar(mxLogical value)
{
    mxArray* ret = mxCreateLogicalMatrix(1, 1);
    ((mxLogical*)ret->realdata)[0] = value;
    return ret;
}
//=============================================================================
mxArray*
mxCreateLogicalMatrix(mwSize m, mwSize n)
{
    mwSize dims[2];
    dims[0] = m;
    dims[1] = n;
    return mxCreateLogicalArray(2, dims);
}
//=============================================================================
mxArray*
mxCreateLogicalArray(mwSize ndim, const mwSize* dims)
{
    return mxAllocateRealArray(ndim, dims, sizeof(mxLogical), mxLOGICAL_CLASS);
}
//=============================================================================
