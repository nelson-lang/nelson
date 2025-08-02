//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "nlsMex_exports.h"
#include "matrix.h"
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsInt8(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsInt16(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsInt32(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsInt64(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsUint8(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsUint16(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsUint32(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsUint64(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    mxInt8*
    mxGetInt8sInterleavedComplex(const mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxSetInt8sInterleavedComplex(mxArray* pa, mxInt8* dt);
    //=============================================================================
    NLSMEX_IMPEXP
    mxUint8*
    mxGetUint8sInterleavedComplex(const mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxSetUint8sInterleavedComplex(mxArray* pa, mxUint8* dt);
    //=============================================================================
    NLSMEX_IMPEXP
    mxInt16*
    mxGetInt16sInterleavedComplex(const mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxSetInt16sInterleavedComplex(mxArray* pa, mxInt16* dt);
    //=============================================================================
    NLSMEX_IMPEXP
    mxUint16*
    mxGetUint16sInterleavedComplex(const mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxSetUint16sInterleavedComplex(mxArray* pa, mxUint16* dt);
    //=============================================================================
    NLSMEX_IMPEXP
    mxInt32*
    mxGetInt32sInterleavedComplex(const mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxSetInt32sInterleavedComplex(mxArray* pa, mxInt32* dt);
    //=============================================================================
    NLSMEX_IMPEXP
    mxUint32*
    mxGetUint32sInterleavedComplex(const mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxSetUint32sInterleavedComplex(mxArray* pa, mxUint32* dt);
    //=============================================================================
    NLSMEX_IMPEXP
    mxInt64*
    mxGetInt64sInterleavedComplex(const mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxSetInt64sInterleavedComplex(mxArray* pa, mxInt64* dt);
    //=============================================================================
    NLSMEX_IMPEXP
    mxUint64*
    mxGetUint64sInterleavedComplex(const mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxSetUint64sInterleavedComplex(mxArray* pa, mxUint64* dt);
    //=============================================================================
    NLSMEX_IMPEXP
    mxComplexInt8*
    mxGetComplexInt8sInterleavedComplex(const mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxSetComplexInt8sInterleavedComplex(mxArray* pa, mxComplexInt8* dt);
    //=============================================================================
    NLSMEX_IMPEXP
    mxComplexUint8*
    mxGetComplexUint8sInterleavedComplex(const mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxSetComplexUint8sInterleavedComplex(mxArray* pa, mxComplexUint8* dt);
    //=============================================================================
    NLSMEX_IMPEXP
    mxComplexInt16*
    mxGetComplexInt16sInterleavedComplex(const mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxSetComplexInt16sInterleavedComplex(mxArray* pa, mxComplexInt16* dt);
    //=============================================================================
    NLSMEX_IMPEXP
    mxComplexUint16*
    mxGetComplexUint16sInterleavedComplex(const mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxSetComplexUint16sInterleavedComplex(mxArray* pa, mxComplexUint16* dt);
    //=============================================================================
    NLSMEX_IMPEXP
    mxComplexInt32*
    mxGetComplexInt32sInterleavedComplex(const mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxSetComplexInt32sInterleavedComplex(mxArray* pa, mxComplexInt32* dt);
    //=============================================================================
    NLSMEX_IMPEXP
    mxComplexUint32*
    mxGetComplexUint32sInterleavedComplex(const mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxSetComplexUint32sInterleavedComplex(mxArray* pa, mxComplexUint32* dt);
    //=============================================================================
    NLSMEX_IMPEXP
    mxComplexInt64*
    mxGetComplexInt64sInterleavedComplex(const mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxSetComplexInt64sInterleavedComplex(mxArray* pa, mxComplexInt64* dt);
    //=============================================================================
    NLSMEX_IMPEXP
    mxComplexUint64*
    mxGetComplexUint64sInterleavedComplex(const mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxSetComplexUint64sInterleavedComplex(mxArray* pa, mxComplexUint64* dt);
    //=============================================================================
#ifdef MX_HAS_INTERLEAVED_COMPLEX
#define mxGetInt8s mxGetInt8sInterleavedComplex
#define mxSetInt8s mxSetInt8sInterleavedComplex
#define mxGetUint8s mxGetUint8sInterleavedComplex
#define mxSetUint8s mxSetUint8sInterleavedComplex
#define mxGetInt16s mxGetInt16sInterleavedComplex
#define mxSetInt16s mxSetInt16sInterleavedComplex
#define mxGetUint16s mxGetUint16sInterleavedComplex
#define mxSetUint16s mxSetUint16sInterleavedComplex
#define mxGetInt32s mxGetInt32sInterleavedComplex
#define mxSetInt32s mxSetInt32sInterleavedComplex
#define mxGetUint32s mxGetUint32sInterleavedComplex
#define mxSetUint32s mxSetUint32sInterleavedComplex
#define mxGetInt64s mxGetInt64sInterleavedComplex
#define mxSetInt64s mxSetInt64sInterleavedComplex
#define mxGetUint64s mxGetUint64sInterleavedComplex
#define mxSetUint64s mxSetUint64sInterleavedComplex
#define mxGetComplexInt8s mxGetComplexInt8sInterleavedComplex
#define mxSetComplexInt8s mxSetComplexInt8sInterleavedComplex
#define mxGetComplexUint8s mxGetComplexUint8sInterleavedComplex
#define mxSetComplexUint8s mxSetComplexUint8sInterleavedComplex
#define mxGetComplexInt16s mxGetComplexInt16sInterleavedComplex
#define mxSetComplexInt16s mxSetComplexInt16sInterleavedComplex
#define mxGetComplexUint16s mxGetComplexUint16sInterleavedComplex
#define mxSetComplexUint16s mxSetComplexUint16sInterleavedComplex
#define mxGetComplexInt32s mxGetComplexInt32sInterleavedComplex
#define mxSetComplexInt32s mxSetComplexInt32sInterleavedComplex
#define mxGetComplexUint32s mxGetComplexUint32sInterleavedComplex
#define mxSetComplexUint32s mxSetComplexUint32sInterleavedComplex
#define mxGetComplexInt64s mxGetComplexInt64sInterleavedComplex
#define mxSetComplexInt64s mxSetComplexInt64sInterleavedComplex
#define mxGetComplexUint64s mxGetComplexUint64sInterleavedComplex
#define mxSetComplexUint64s mxSetComplexUint64sInterleavedComplex
#endif
//=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
