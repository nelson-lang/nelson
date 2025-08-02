//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <string>
#include "MxInteger.h"
#include "i18n.hpp"
//=============================================================================
static bool
mxIsInteger(const mxArray* pm, mxClassID classExpected)
{
    if (pm != nullptr) {
        return pm->classID == classExpected;
    }
    return false;
}
//=============================================================================
bool
mxIsInt8(const mxArray* pm)
{
    return mxIsInteger(pm, mxINT8_CLASS);
}
//=============================================================================
bool
mxIsInt16(const mxArray* pm)
{
    return mxIsInteger(pm, mxINT16_CLASS);
}
//=============================================================================
bool
mxIsInt32(const mxArray* pm)
{
    return mxIsInteger(pm, mxINT32_CLASS);
}
//=============================================================================
bool
mxIsInt64(const mxArray* pm)
{
    return mxIsInteger(pm, mxINT64_CLASS);
}
//=============================================================================
bool
mxIsUint8(const mxArray* pm)
{
    return mxIsInteger(pm, mxUINT8_CLASS);
}
//=============================================================================
bool
mxIsUint16(const mxArray* pm)
{
    return mxIsInteger(pm, mxUINT16_CLASS);
}
//=============================================================================
bool
mxIsUint32(const mxArray* pm)
{
    return mxIsInteger(pm, mxUINT32_CLASS);
}
//=============================================================================
bool
mxIsUint64(const mxArray* pm)
{
    return mxIsInteger(pm, mxUINT64_CLASS);
}
//=============================================================================
template <class T>
T*
mxGetComplexIntegerInterleavedComplex(
    const mxArray* pa, const std::string& typemsg, bool isExpectedType)
{
    if (isExpectedType) {
        return (T*)pa->realdata;
    } else {
        std::string msg = typemsg + "expected.";
        mexErrMsgTxt(_(msg).c_str());
    }
    return nullptr;
}
//=============================================================================
template <class T>
int
mxSetIntegerInterleavedComplex(mxArray* pa, T* dt, const std::string& typemsg, bool isExpectedType)
{
    if (pa != nullptr) {
        if (!isExpectedType) {
            std::string msg = typemsg + " expected.";
            mexErrMsgTxt(_(msg).c_str());
        }
        if (!mxIsRegisteredPointer(dt)) {
            std::string msg = typemsg + " array not allocated with mxMalloc or mxCalloc.";
            mexErrMsgTxt(_(msg).c_str());
        }
        pa->realdata = dt;
        return 1;
    }
    return 0;
}
//=============================================================================
template <class T>
int
mxSetComplexIntegerInterleavedComplex(
    mxArray* pa, T* dt, const std::string& typemsg, bool isExpectedType)
{
    if (pa != nullptr) {
        if (!pa->iscomplex || !isExpectedType) {
            std::string msg = typemsg + " complex expected.";
            mexErrMsgTxt(_(msg).c_str());
        }
        if (!mxIsRegisteredPointer(dt)) {
            std::string msg = typemsg + " complex array not allocated with mxMalloc or mxCalloc.";
            mexErrMsgTxt(_(msg).c_str());
        }
        pa->realdata = dt;
        return 1;
    }
    return 0;
}
//=============================================================================
mxInt8*
mxGetInt8sInterleavedComplex(const mxArray* pa)
{
    return mxGetComplexIntegerInterleavedComplex<mxInt8>(pa, "int8", mxIsInt8(pa));
}
//=============================================================================
int
mxSetInt8sInterleavedComplex(mxArray* pa, mxInt8* dt)
{
    return mxSetIntegerInterleavedComplex<mxInt8>(pa, dt, "int8", mxIsInt8(pa));
}
//=============================================================================
mxUint8*
mxGetUint8sInterleavedComplex(const mxArray* pa)
{
    return mxGetComplexIntegerInterleavedComplex<mxUint8>(pa, "uint8", mxIsUint8(pa));
}
//=============================================================================
int
mxSetUint8sInterleavedComplex(mxArray* pa, mxUint8* dt)
{
    return mxSetIntegerInterleavedComplex<mxUint8>(pa, dt, "uint8", mxIsUint8(pa));
}
//=============================================================================
mxInt16*
mxGetInt16sInterleavedComplex(const mxArray* pa)
{
    return mxGetComplexIntegerInterleavedComplex<mxInt16>(pa, "int16", mxIsInt16(pa));
}
//=============================================================================
int
mxSetInt16sInterleavedComplex(mxArray* pa, mxInt16* dt)
{
    return mxSetIntegerInterleavedComplex<mxInt16>(pa, dt, "int16", mxIsInt16(pa));
}
//=============================================================================
mxUint16*
mxGetUint16sInterleavedComplex(const mxArray* pa)
{
    return mxGetComplexIntegerInterleavedComplex<mxUint16>(pa, "uint16", mxIsUint16(pa));
}
//=============================================================================
int
mxSetUint16sInterleavedComplex(mxArray* pa, mxUint16* dt)
{
    return mxSetIntegerInterleavedComplex<mxUint16>(pa, dt, "uint16", mxIsUint16(pa));
}
//=============================================================================
mxInt32*
mxGetInt32sInterleavedComplex(const mxArray* pa)
{
    return mxGetComplexIntegerInterleavedComplex<mxInt32>(pa, "int32", mxIsInt32(pa));
}
//=============================================================================
int
mxSetInt32sInterleavedComplex(mxArray* pa, mxInt32* dt)
{
    return mxSetIntegerInterleavedComplex<mxInt32>(pa, dt, "int32", mxIsInt32(pa));
}
//=============================================================================
mxUint32*
mxGetUint32sInterleavedComplex(const mxArray* pa)
{
    return mxGetComplexIntegerInterleavedComplex<mxUint32>(pa, "uint32", mxIsUint32(pa));
}
//=============================================================================
int
mxSetUint32sInterleavedComplex(mxArray* pa, mxUint32* dt)
{
    return mxSetIntegerInterleavedComplex<mxUint32>(pa, dt, "uint32", mxIsUint32(pa));
}
//=============================================================================
mxInt64*
mxGetInt64sInterleavedComplex(const mxArray* pa)
{
    return mxGetComplexIntegerInterleavedComplex<mxInt64>(pa, "int64", mxIsInt64(pa));
}
//=============================================================================
int
mxSetInt64sInterleavedComplex(mxArray* pa, mxInt64* dt)
{
    return mxSetIntegerInterleavedComplex<mxInt64>(pa, dt, "int64", mxIsInt64(pa));
}
//=============================================================================
mxUint64*
mxGetUint64sInterleavedComplex(const mxArray* pa)
{
    return mxGetComplexIntegerInterleavedComplex<mxUint64>(pa, "uint64", mxIsUint64(pa));
}
//=============================================================================
int
mxSetUint64sInterleavedComplex(mxArray* pa, mxUint64* dt)
{
    return mxSetIntegerInterleavedComplex<mxUint64>(pa, dt, "uint64", mxIsUint64(pa));
}
//=============================================================================
mxComplexInt8*
mxGetComplexInt8sInterleavedComplex(const mxArray* pa)
{
    return mxGetComplexIntegerInterleavedComplex<mxComplexInt8>(pa, "int8", mxIsInt8(pa));
}
//=============================================================================
int
mxSetComplexInt8sInterleavedComplex(mxArray* pa, mxComplexInt8* dt)
{
    return mxSetComplexIntegerInterleavedComplex<mxComplexInt8>(pa, dt, "int8", mxIsInt8(pa));
}
//=============================================================================
mxComplexUint8*
mxGetComplexUint8sInterleavedComplex(const mxArray* pa)
{
    return mxGetComplexIntegerInterleavedComplex<mxComplexUint8>(pa, "uint8", mxIsUint8(pa));
}
//=============================================================================
int
mxSetComplexUint8sInterleavedComplex(mxArray* pa, mxComplexUint8* dt)
{
    return mxSetComplexIntegerInterleavedComplex<mxComplexUint8>(pa, dt, "uint8", mxIsUint8(pa));
}
//=============================================================================
mxComplexInt16*
mxGetComplexInt16sInterleavedComplex(const mxArray* pa)
{
    return mxGetComplexIntegerInterleavedComplex<mxComplexInt16>(pa, "int16", mxIsInt16(pa));
}
//=============================================================================
int
mxSetComplexInt16sInterleavedComplex(mxArray* pa, mxComplexInt16* dt)
{
    return mxSetComplexIntegerInterleavedComplex<mxComplexInt16>(pa, dt, "int16", mxIsInt16(pa));
}
//=============================================================================
mxComplexUint16*
mxGetComplexUint16sInterleavedComplex(const mxArray* pa)
{
    return mxGetComplexIntegerInterleavedComplex<mxComplexUint16>(pa, "uint16", mxIsUint16(pa));
}
//=============================================================================
int
mxSetComplexUint16sInterleavedComplex(mxArray* pa, mxComplexUint16* dt)
{
    return mxSetComplexIntegerInterleavedComplex<mxComplexUint16>(pa, dt, "uint16", mxIsUint16(pa));
}
//=============================================================================
mxComplexInt32*
mxGetComplexInt32sInterleavedComplex(const mxArray* pa)
{
    return mxGetComplexIntegerInterleavedComplex<mxComplexInt32>(pa, "int32", mxIsInt32(pa));
}
//=============================================================================
int
mxSetComplexInt32sInterleavedComplex(mxArray* pa, mxComplexInt32* dt)
{
    return mxSetComplexIntegerInterleavedComplex<mxComplexInt32>(pa, dt, "int32", mxIsInt32(pa));
}
//=============================================================================
mxComplexUint32*
mxGetComplexUint32sInterleavedComplex(const mxArray* pa)
{
    return mxGetComplexIntegerInterleavedComplex<mxComplexUint32>(pa, "uint32", mxIsUint32(pa));
}
//=============================================================================
int
mxSetComplexUint32sInterleavedComplex(mxArray* pa, mxComplexUint32* dt)
{
    return mxSetComplexIntegerInterleavedComplex<mxComplexUint32>(pa, dt, "uint32", mxIsUint32(pa));
}
//=============================================================================
mxComplexInt64*
mxGetComplexInt64sInterleavedComplex(const mxArray* pa)
{
    return mxGetComplexIntegerInterleavedComplex<mxComplexInt64>(pa, "int64", mxIsInt64(pa));
}
//=============================================================================
int
mxSetComplexInt64sInterleavedComplex(mxArray* pa, mxComplexInt64* dt)
{
    return mxSetComplexIntegerInterleavedComplex<mxComplexInt64>(pa, dt, "int64", mxIsInt64(pa));
}
//=============================================================================
mxComplexUint64*
mxGetComplexUint64sInterleavedComplex(const mxArray* pa)
{
    return mxGetComplexIntegerInterleavedComplex<mxComplexUint64>(pa, "uint64", mxIsUint64(pa));
}
//=============================================================================
int
mxSetComplexUint64sInterleavedComplex(mxArray* pa, mxComplexUint64* dt)
{
    return mxSetComplexIntegerInterleavedComplex<mxComplexUint64>(pa, dt, "uint64", mxIsUint64(pa));
}
//=============================================================================
