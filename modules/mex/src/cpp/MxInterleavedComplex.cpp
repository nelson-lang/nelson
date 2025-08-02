//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "matrix.h"
#include "i18n.hpp"
#include "MxHelpers.hpp"
//=============================================================================
mxComplexDouble*
mxGetComplexDoublesInterleavedComplex(const mxArray* pa)
{
    mxComplexDouble* result = nullptr;
    if (pa != nullptr) {
        bool isExpectedType = mxIsComplex(pa) && mxIsDouble(pa);
        if (isExpectedType) {
            result = reinterpret_cast<mxComplexDouble*>(pa->realdata);
        } else {
            mexErrMsgTxt(_("mxGetDoubles complex expected.").c_str());
        }
    }
    return result;
}
//=============================================================================
mxDouble*
mxGetDoublesInterleavedComplex(const mxArray* pa)
{
    mxDouble* result = nullptr;
    if (pa != nullptr) {
        bool isExpectedType = !mxIsComplex(pa) && mxIsDouble(pa);
        if (isExpectedType) {
            result = (mxDouble*)pa->realdata;
        } else {
            mexErrMsgTxt(_("mxGetDoubles real expected.").c_str());
        }
    }
    return result;
}
//=============================================================================
int
mxSetDoublesInterleavedComplex(mxArray* pa, mxDouble* dt)
{
    int retCode = 0;
    if (pa != nullptr) {
        if (dt == nullptr || !mxIsRegisteredPointer(dt)) {
            return retCode;
        }
        bool isExpectedType = !mxIsComplex(pa) && mxIsDouble(pa);
        if (isExpectedType) {
            pa->realdata = dt;
            retCode = 1;
        }
    }
    return retCode;
}
//=============================================================================
int
mxSetComplexDoublesInterleavedComplex(mxArray* pa, mxComplexDouble* dt)
{
    int retCode = 0;
    if (pa != nullptr) {
        if (dt == nullptr || !mxIsRegisteredPointer(dt)) {
            return retCode;
        }
        bool isExpectedType = mxIsComplex(pa) && mxIsDouble(pa);
        if (isExpectedType) {
            pa->realdata = dt;
            retCode = 1;
        }
    }
    return retCode;
}
//=============================================================================
mxComplexSingle*
mxGetComplexSinglesInterleavedComplex(const mxArray* pa)
{
    mxComplexSingle* result = nullptr;
    if (pa != nullptr) {
        bool isExpectedType = mxIsComplex(pa) && mxIsSingle(pa) && !mxIsSparse(pa);
        if (isExpectedType) {
            result = reinterpret_cast<mxComplexSingle*>(pa->realdata);
        } else {
            mexErrMsgTxt(_("mxGetSingles complex expected.").c_str());
        }
    }
    return result;
}
//=============================================================================
mxSingle*
mxGetSinglesInterleavedComplex(const mxArray* pa)
{
    mxSingle* result = nullptr;
    if (pa != nullptr) {
        bool isExpectedType = !mxIsComplex(pa) && mxIsSingle(pa) && !mxIsSparse(pa);
        if (isExpectedType) {
            result = (mxSingle*)pa->realdata;
        } else {
            mexErrMsgTxt(_("mxGetSingles real expected.").c_str());
        }
    }
    return result;
}
//=============================================================================
int
mxSetSinglesInterleavedComplex(mxArray* pa, mxSingle* dt)
{
    int retCode = 0;
    if (pa != nullptr) {
        if (dt == nullptr || !mxIsRegisteredPointer(dt)) {
            return retCode;
        }
        bool isExpectedType = !mxIsComplex(pa) && mxIsSingle(pa) && !mxIsSparse(pa);
        if (isExpectedType) {
            pa->realdata = dt;
            retCode = 1;
        }
    }
    return retCode;
}
//=============================================================================
int
mxSetComplexSinglesInterleavedComplex(mxArray* pa, mxComplexSingle* dt)
{
    int retCode = 0;
    if (pa != nullptr) {
        if (dt == nullptr || !mxIsRegisteredPointer(dt)) {
            return retCode;
        }
        bool isExpectedType = mxIsComplex(pa) && mxIsSingle(pa) && !mxIsSparse(pa);
        if (isExpectedType) {
            pa->realdata = dt;
            retCode = 1;
        }
    }
    return retCode;
}
//=============================================================================
template <class TREAL, class TCPLX>
int
mxMakeArrayRealTemplate(mxArray* pa, void* newPointerOnValues, mwSize nbElements)
{
    TCPLX* cplx = (TCPLX*)(pa->realdata);
    TREAL* r = (TREAL*)newPointerOnValues;
    for (mwSize k = 0; k < nbElements; ++k) {
        r[k] = (TREAL)cplx[k].real;
    }
    mxFree(pa->realdata);
    pa->iscomplex = false;
    pa->realdata = newPointerOnValues;
    return 1;
}
//=============================================================================
int
mxMakeArrayRealInterleavedComplex(mxArray* pa)
{
    if (mxIsNumeric(pa)) {
        if (!pa->iscomplex) {
            return 1;
        }
        if (pa->issparse) {
            mwSize nbElements = pa->nzmax;
            void* ptr = mxCalloc(nbElements, sizeFromClass(pa->classID));
            switch (pa->classID) {
            case mxDOUBLE_CLASS:
                return mxMakeArrayRealTemplate<mxDouble, mxComplexDouble>(pa, ptr, nbElements);
            case mxSINGLE_CLASS:
                return mxMakeArrayRealTemplate<mxSingle, mxComplexSingle>(pa, ptr, nbElements);
            default: {
            } break;
            }
        } else {
            mwSize nbElements = countElements(pa->number_of_dims, pa->dims);
            void* ptr = mxCalloc(nbElements, sizeFromClass(pa->classID));
            switch (pa->classID) {
            case mxDOUBLE_CLASS:
                return mxMakeArrayRealTemplate<mxDouble, mxComplexDouble>(pa, ptr, nbElements);
            case mxSINGLE_CLASS:
                return mxMakeArrayRealTemplate<mxSingle, mxComplexSingle>(pa, ptr, nbElements);
            case mxINT8_CLASS:
                return mxMakeArrayRealTemplate<mxInt8, mxComplexInt8>(pa, ptr, nbElements);
            case mxUINT8_CLASS:
                return mxMakeArrayRealTemplate<mxUint8, mxComplexUint8>(pa, ptr, nbElements);
            case mxINT16_CLASS:
                return mxMakeArrayRealTemplate<mxInt16, mxComplexInt16>(pa, ptr, nbElements);
            case mxUINT16_CLASS:
                return mxMakeArrayRealTemplate<mxUint16, mxComplexUint16>(pa, ptr, nbElements);
            case mxINT32_CLASS:
                return mxMakeArrayRealTemplate<mxInt32, mxComplexInt32>(pa, ptr, nbElements);
            case mxUINT32_CLASS:
                return mxMakeArrayRealTemplate<mxUint32, mxComplexUint32>(pa, ptr, nbElements);
            case mxINT64_CLASS:
                return mxMakeArrayRealTemplate<mxInt64, mxComplexInt64>(pa, ptr, nbElements);
            case mxUINT64_CLASS:
                return mxMakeArrayRealTemplate<mxUint64, mxComplexUint64>(pa, ptr, nbElements);
            default: {
            } break;
            }
        }
    }
    return 0;
}
//=============================================================================
static mwSize
sizeComplex(mxClassID classId)
{
    switch (classId) {
    case mxDOUBLE_CLASS:
        return sizeof(mxComplexDouble);
    case mxSINGLE_CLASS:
        return sizeof(mxComplexDouble);
    case mxINT8_CLASS:
        return sizeof(mxComplexInt8);
    case mxUINT8_CLASS:
        return sizeof(mxComplexUint8);
    case mxINT16_CLASS:
        return sizeof(mxComplexInt16);
    case mxUINT16_CLASS:
        return sizeof(mxComplexUint16);
    case mxINT32_CLASS:
        return sizeof(mxComplexInt32);
    case mxUINT32_CLASS:
        return sizeof(mxComplexUint32);
    case mxINT64_CLASS:
        return sizeof(mxComplexInt64);
    case mxUINT64_CLASS:
        return sizeof(mxComplexUint64);
    default: {
    } break;
    }
    return 0;
}
//=============================================================================
template <class TCPLX, class TREAL>
int
mxMakeArrayComplexTemplate(mxArray* pa, void* newPointerOnValues, mwSize nbElements)
{
    TCPLX* cpx = (TCPLX*)newPointerOnValues;
    for (mwSize k = 0; k < nbElements; ++k) {
        cpx[k].real = ((TREAL*)pa->realdata)[k];
        cpx[k].imag = (TREAL)0;
    }
    mxFree(pa->realdata);
    pa->iscomplex = true;
    pa->realdata = newPointerOnValues;
    return 1;
}
//=============================================================================
int
mxMakeArrayComplexInterleavedComplex(mxArray* pa)
{
    if (mxIsNumeric(pa)) {
        if (pa->iscomplex) {
            return 1;
        }
        if (pa->issparse) {
            mwSize nbElements = pa->nzmax;
            void* ptr = mxCalloc(nbElements, sizeComplex(pa->classID));
            switch (pa->classID) {
            case mxDOUBLE_CLASS:
                return mxMakeArrayComplexTemplate<mxComplexDouble, mxDouble>(pa, ptr, nbElements);
            case mxSINGLE_CLASS:
                return mxMakeArrayComplexTemplate<mxComplexSingle, mxSingle>(pa, ptr, nbElements);
            default: {
            } break;
            }
        } else {
            mwSize nbElements = countElements(pa->number_of_dims, pa->dims);
            void* ptr = mxCalloc(nbElements, sizeComplex(pa->classID));
            switch (pa->classID) {
            case mxDOUBLE_CLASS:
                return mxMakeArrayComplexTemplate<mxComplexDouble, mxDouble>(pa, ptr, nbElements);
            case mxSINGLE_CLASS:
                return mxMakeArrayComplexTemplate<mxComplexSingle, mxSingle>(pa, ptr, nbElements);
            case mxINT8_CLASS:
                return mxMakeArrayComplexTemplate<mxComplexInt8, mxInt8>(pa, ptr, nbElements);
            case mxUINT8_CLASS:
                return mxMakeArrayComplexTemplate<mxComplexUint8, mxUint8>(pa, ptr, nbElements);
            case mxINT16_CLASS:
                return mxMakeArrayComplexTemplate<mxComplexInt16, mxInt16>(pa, ptr, nbElements);
            case mxUINT16_CLASS:
                return mxMakeArrayComplexTemplate<mxComplexUint16, mxUint16>(pa, ptr, nbElements);
            case mxINT32_CLASS:
                return mxMakeArrayComplexTemplate<mxComplexInt32, mxInt32>(pa, ptr, nbElements);
            case mxUINT32_CLASS:
                return mxMakeArrayComplexTemplate<mxComplexUint32, mxUint32>(pa, ptr, nbElements);
            case mxINT64_CLASS:
                return mxMakeArrayComplexTemplate<mxComplexInt64, mxInt64>(pa, ptr, nbElements);
            case mxUINT64_CLASS:
                return mxMakeArrayComplexTemplate<mxComplexUint64, mxUint64>(pa, ptr, nbElements);
            default: {
            } break;
            }
        }
    }
    return 0;
}
//=============================================================================
