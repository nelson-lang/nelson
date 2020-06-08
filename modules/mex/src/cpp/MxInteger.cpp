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
#include "MxInteger.h"
//=============================================================================
bool
mxIsInt8(const mxArray* pm)
{
    if (pm != nullptr) {
        return pm->classID == mxINT8_CLASS;
    }
    return false;
}
//=============================================================================
bool
mxIsInt16(const mxArray* pm)
{
    if (pm != nullptr) {
        return pm->classID == mxINT16_CLASS;
    }
    return false;
}
//=============================================================================
bool
mxIsInt32(const mxArray* pm)
{
    if (pm != nullptr) {
        return pm->classID == mxINT32_CLASS;
    }
    return false;
}
//=============================================================================
bool
mxIsInt64(const mxArray* pm)
{
    if (pm != nullptr) {
        return pm->classID == mxINT64_CLASS;
    }
    return false;
}
//=============================================================================
bool
mxIsUint8(const mxArray* pm)
{
    if (pm != nullptr) {
        return pm->classID == mxUINT8_CLASS;
    }
    return false;
}
//=============================================================================
bool
mxIsUint16(const mxArray* pm)
{
    if (pm != nullptr) {
        return pm->classID == mxUINT16_CLASS;
    }
    return false;
}
//=============================================================================
bool
mxIsUint32(const mxArray* pm)
{
    if (pm != nullptr) {
        return pm->classID == mxUINT32_CLASS;
    }
    return false;
}
//=============================================================================
bool
mxIsUint64(const mxArray* pm)
{
    if (pm != nullptr) {
        return pm->classID == mxUINT64_CLASS;
    }
    return false;
}
//=============================================================================
mxInt8*
mxGetInt8sInterleavedComplex(const mxArray* pa)
{
    if (pa != nullptr) {
        if (mxIsInt8(pa)) {
            return (mxInt8*)pa->realdata;
        } else {
            mexErrMsgTxt("int8 expected.");
        }
    }
    return nullptr;
}
//=============================================================================
int
mxSetInt8sInterleavedComplex(mxArray* pa, mxInt8* dt)
{
    if (pa != nullptr) {
        if (mxIsInt8(pa)) {
            if (!mxIsRegisteredPointer(dt)) {
                mexErrMsgTxt("int8 array not allocated with mxMalloc or mxCalloc.");
            }
            pa->realdata = dt;
            return 1;
        } else {
            mexErrMsgTxt("int8 expected.");
        }
    }
    return 0;
}
//=============================================================================
mxUint8*
mxGetUint8sInterleavedComplex(const mxArray* pa)
{
    if (pa != nullptr) {
        if (mxIsUint8(pa)) {
            return (mxUint8*)pa->realdata;
        } else {
            mexErrMsgTxt("uint8 expected.");
        }
    }
    return nullptr;
}
//=============================================================================
int
mxSetUint8sInterleavedComplex(mxArray* pa, mxUint8* dt)
{
    if (pa != nullptr) {
        if (mxIsUint8(pa)) {
            if (!mxIsRegisteredPointer(dt)) {
                mexErrMsgTxt("uint8 array not allocated with mxMalloc or mxCalloc.");
            }
            pa->realdata = dt;
            return 1;
        } else {
            mexErrMsgTxt("uint8 expected.");
        }
    }
    return 0;
}
//=============================================================================
mxInt16*
mxGetInt16sInterleavedComplex(const mxArray* pa)
{
    if (pa != nullptr) {
        if (mxIsInt16(pa)) {
            return (mxInt16*)pa->realdata;
        } else {
            mexErrMsgTxt("int16 expected.");
        }
    }
    return nullptr;
}
//=============================================================================
int
mxSetInt16sInterleavedComplex(mxArray* pa, mxInt16* dt)
{
    if (pa != nullptr) {
        if (mxIsInt16(pa)) {
            if (!mxIsRegisteredPointer(dt)) {
                mexErrMsgTxt("int16 array not allocated with mxMalloc or mxCalloc.");
            }
            pa->realdata = dt;
            return 1;
        } else {
            mexErrMsgTxt("int16 expected.");
        }
    }
    return 0;
}
//=============================================================================
mxUint16*
mxGetUint16sInterleavedComplex(const mxArray* pa)
{
    if (pa != nullptr) {
        if (mxIsUint16(pa)) {
            return (mxUint16*)pa->realdata;
        } else {
            mexErrMsgTxt("uint16 expected.");
        }
    }
    return nullptr;
}
//=============================================================================
int
mxSetUint16sInterleavedComplex(mxArray* pa, mxUint16* dt)
{
    if (pa != nullptr) {
        if (mxIsUint16(pa)) {
            if (!mxIsRegisteredPointer(dt)) {
                mexErrMsgTxt("uint16 array not allocated with mxMalloc or mxCalloc.");
            }
            pa->realdata = dt;
            return 1;
        } else {
            mexErrMsgTxt("uint16 expected.");
        }
    }
    return 0;
}
//=============================================================================
mxInt32*
mxGetInt32sInterleavedComplex(const mxArray* pa)
{
    if (pa != nullptr) {
        if (mxIsInt32(pa)) {
            return (mxInt32*)pa->realdata;
        } else {
            mexErrMsgTxt("int32 expected.");
        }
    }
    return nullptr;
}
//=============================================================================
int
mxSetInt32sInterleavedComplex(mxArray* pa, mxInt32* dt)
{
    if (pa != nullptr) {
        if (mxIsInt32(pa)) {
            if (!mxIsRegisteredPointer(dt)) {
                mexErrMsgTxt("int32 array not allocated with mxMalloc or mxCalloc.");
            }
            pa->realdata = dt;
            return 1;
        } else {
            mexErrMsgTxt("int32 expected.");
        }
    }
    return 0;
}
//=============================================================================
mxUint32*
mxGetUint32sInterleavedComplex(const mxArray* pa)
{
    if (pa != nullptr) {
        if (mxIsUint32(pa)) {
            return (mxUint32*)pa->realdata;
        } else {
            mexErrMsgTxt("uint32 expected.");
        }
    }
    return nullptr;
}
//=============================================================================
int
mxSetUint32sInterleavedComplex(mxArray* pa, mxUint32* dt)
{
    if (pa != nullptr) {
        if (mxIsUint32(pa)) {
            if (!mxIsRegisteredPointer(dt)) {
                mexErrMsgTxt("uint32 array not allocated with mxMalloc or mxCalloc.");
            }
            pa->realdata = dt;
            return 1;
        } else {
            mexErrMsgTxt("uint32 expected.");
        }
    }
    return 0;
}
//=============================================================================
mxInt64*
mxGetInt64sInterleavedComplex(const mxArray* pa)
{
    if (pa != nullptr) {
        if (mxIsInt64(pa)) {
            return (mxInt64*)pa->realdata;
        } else {
            mexErrMsgTxt("int64 expected.");
        }
    }
    return nullptr;
}
//=============================================================================
int
mxSetInt64sInterleavedComplex(mxArray* pa, mxInt64* dt)
{
    if (pa != nullptr) {
        if (mxIsInt64(pa)) {
            if (!mxIsRegisteredPointer(dt)) {
                mexErrMsgTxt("int64 array not allocated with mxMalloc or mxCalloc.");
            }
            pa->realdata = dt;
            return 1;
        } else {
            mexErrMsgTxt("int64 expected.");
        }
    }
    return 0;
}
//=============================================================================
mxUint64*
mxGetUint64sInterleavedComplex(const mxArray* pa)
{
    if (pa != nullptr) {
        if (mxIsUint64(pa)) {
            return (mxUint64*)pa->realdata;
        } else {
            mexErrMsgTxt("uint64 expected.");
        }
    }
    return nullptr;
}
//=============================================================================
int
mxSetUint64sInterleavedComplex(mxArray* pa, mxUint64* dt)
{
    if (pa != nullptr) {
        if (mxIsUint64(pa)) {
            if (!mxIsRegisteredPointer(dt)) {
                mexErrMsgTxt("uint64 array not allocated with mxMalloc or mxCalloc.");
            }
            pa->realdata = dt;
            return 1;
        } else {
            mexErrMsgTxt("uint64 expected.");
        }
    }
    return 0;
}
//=============================================================================
mxComplexInt8*
mxGetComplexInt8sInterleavedComplex(const mxArray* pa)
{
    if (mxIsInt8(pa)) {
        return (mxComplexInt8*)pa->realdata;
    } else {
        mexErrMsgTxt("int8 expected.");
    }
    return nullptr;
}
//=============================================================================
int
mxSetComplexInt8sInterleavedComplex(mxArray* pa, mxComplexInt8* dt)
{
    if (pa != nullptr) {
        if (!pa->iscomplex || !mxIsInt8(pa)) {
            mexErrMsgTxt("int8 complex expected.");
        }
        if (!mxIsRegisteredPointer(dt)) {
            mexErrMsgTxt("int8 complex array not allocated with mxMalloc or mxCalloc.");
        }
        pa->realdata = dt;
        return 1;
    }
    return 0;
}
//=============================================================================
mxComplexUint8*
mxGetComplexUint8sInterleavedComplex(const mxArray* pa)
{ 
    if (mxIsUint8(pa)) {
        return (mxComplexUint8*)pa->realdata;
    } else {
        mexErrMsgTxt("uint8 expected.");
    }
    return nullptr;
}
//=============================================================================
int
mxSetComplexUint8sInterleavedComplex(mxArray* pa, mxComplexUint8* dt)
{
    if (pa != nullptr) {
        if (!pa->iscomplex || !mxIsUint8(pa)) {
            mexErrMsgTxt("uint8 complex expected.");
        }
        if (!mxIsRegisteredPointer(dt)) {
            mexErrMsgTxt("uint8 complex array not allocated with mxMalloc or mxCalloc.");
        }
        pa->realdata = dt;
        return 1;
    }
    return 0;
}
//=============================================================================
mxComplexInt16*
mxGetComplexInt16sInterleavedComplex(const mxArray* pa)
{
    if (mxIsInt16(pa)) {
        return (mxComplexInt16*)pa->realdata;
    } else {
        mexErrMsgTxt("int16 expected.");
    }
    return nullptr;
}
//=============================================================================
int
mxSetComplexInt16sInterleavedComplex(mxArray* pa, mxComplexInt16* dt)
{
    if (pa != nullptr) {
        if (!pa->iscomplex || !mxIsInt16(pa)) {
            mexErrMsgTxt("int16 complex expected.");
        }
        if (!mxIsRegisteredPointer(dt)) {
            mexErrMsgTxt("int16 complex array not allocated with mxMalloc or mxCalloc.");
        }
        pa->realdata = dt;
        return 1;
    }
    return 0;
}
//=============================================================================
mxComplexUint16*
mxGetComplexUint16sInterleavedComplex(const mxArray* pa)
{
    if (mxIsUint16(pa)) {
        return (mxComplexUint16*)pa->realdata;
    } else {
        mexErrMsgTxt("uint16 expected.");
    }
    return nullptr;
}
//=============================================================================
int
mxSetComplexUint16sInterleavedComplex(mxArray* pa, mxComplexUint16* dt)
{
    if (pa != nullptr) {
        if (!pa->iscomplex || !mxIsUint16(pa)) {
            mexErrMsgTxt("uint16 complex expected.");
        }
        if (!mxIsRegisteredPointer(dt)) {
            mexErrMsgTxt("uint16 complex array not allocated with mxMalloc or mxCalloc.");
        }
        pa->realdata = dt;
        return 1;
    }
    return 0;
}
//=============================================================================
mxComplexInt32*
mxGetComplexInt32sInterleavedComplex(const mxArray* pa)
{
    if (mxIsInt32(pa)) {
        return (mxComplexInt32*)pa->realdata;
    } else {
        mexErrMsgTxt("int32 expected.");
    }
    return nullptr;
}
//=============================================================================
int
mxSetComplexInt32sInterleavedComplex(mxArray* pa, mxComplexInt32* dt)
{
    if (pa != nullptr) {
        if (!pa->iscomplex || !mxIsInt32(pa)) {
            mexErrMsgTxt("int32 complex expected.");
        }
        if (!mxIsRegisteredPointer(dt)) {
            mexErrMsgTxt("int32 complex array not allocated with mxMalloc or mxCalloc.");
        }
        pa->realdata = dt;
        return 1;
    }
    return 0;
}
//=============================================================================
mxComplexUint32*
mxGetComplexUint32sInterleavedComplex(const mxArray* pa)
{
    if (mxIsUint32(pa)) {
        return (mxComplexUint32*)pa->realdata;
    } else {
        mexErrMsgTxt("uint32 expected.");
    }
    return nullptr;
}
//=============================================================================
int
mxSetComplexUint32sInterleavedComplex(mxArray* pa, mxComplexUint32* dt)
{
    if (pa != nullptr) {
        if (!pa->iscomplex || !mxIsUint32(pa)) {
            mexErrMsgTxt("uint32 complex expected.");
        }
        if (!mxIsRegisteredPointer(dt)) {
            mexErrMsgTxt("uint32 complex array not allocated with mxMalloc or mxCalloc.");
        }
        pa->realdata = dt;
        return 1;
    }
    return 0;
}
//=============================================================================
mxComplexInt64*
mxGetComplexInt64sInterleavedComplex(const mxArray* pa)
{
    if (mxIsInt64(pa)) {
        return (mxComplexInt64*)pa->realdata;
    } else {
        mexErrMsgTxt("int64 expected.");
    }
    return nullptr;
}
//=============================================================================
int
mxSetComplexInt64sInterleavedComplex(mxArray* pa, mxComplexInt64* dt)
{
    if (pa != nullptr) {
        if (!pa->iscomplex || !mxIsInt64(pa)) {
            mexErrMsgTxt("int64 complex expected.");
        }
        if (!mxIsRegisteredPointer(dt)) {
            mexErrMsgTxt("int64 complex array not allocated with mxMalloc or mxCalloc.");
        }
        pa->realdata = dt;
        return 1;
    }
    return 0;
}
//=============================================================================
mxComplexUint64*
mxGetComplexUint64sInterleavedComplex(const mxArray* pa)
{
    if (mxIsUint64(pa)) {
        return (mxComplexUint64*)pa->realdata;
    } else {
        mexErrMsgTxt("uint64 expected.");
    }
    return nullptr;
}
//=============================================================================
int
mxSetComplexUint64sInterleavedComplex(mxArray* pa, mxComplexUint64* dt)
{
    if (pa != nullptr) {
        if (!pa->iscomplex || !mxIsUint64(pa)) {
            mexErrMsgTxt("uint64 complex expected.");
        }
        if (!mxIsRegisteredPointer(dt)) {
            mexErrMsgTxt("uint64 complex array not allocated with mxMalloc or mxCalloc.");
        }
        pa->realdata = dt;
        return 1;
    }
    return 0;
}
//=============================================================================
