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
#include "MxHelpers.hpp"
#include "i18n.hpp"
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
        return static_cast<double>(((mxLogical*)pm->realdata)[0]);
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
    if (pm != nullptr) {
        return (pm->classID == mxDOUBLE_CLASS);
    }
    return false;
}
//=============================================================================
bool
mxIsSingle(const mxArray* pm)
{
    if (pm != nullptr) {
        return (pm->classID == mxSINGLE_CLASS);
    }
    return false;
}
//=============================================================================
NLSMEX_IMPEXP
mxDouble*
mxGetPrInterleavedComplex(const mxArray* pm)
{
    mexErrMsgTxt(_("mxGetPr not allowed with interleaved complex.").c_str());
    return nullptr;
}
//=============================================================================
mxDouble*
mxGetPrSeparatedComplex(const mxArray* pm)
{
    if (pm != nullptr) {
        return (mxDouble*)pm->realdata;
    }
    return nullptr;
}
//=============================================================================
void
mxSetPr(mxArray* pm, double* pr)
{
    if (pm != nullptr) {
        pm->realdata = pr;
    }
}
//=============================================================================
