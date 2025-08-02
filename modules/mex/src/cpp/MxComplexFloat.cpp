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
mxDouble*
mxGetPiSeparatedComplex(const mxArray* pm)
{
    return (mxDouble*)mxGetImagDataSeparatedComplex(pm);
}
//=============================================================================
void
mxSetPiSeparatedComplex(mxArray* pm, double* pr)
{
    mxSetImagDataSeparatedComplex(pm, pr);
}
//=============================================================================
mxDouble*
mxGetPiInterleavedComplex(const mxArray* pm)
{
    mexErrMsgTxt(_("mxGetPi not allowed with interleaved complex.").c_str());
    return nullptr;
}
//=============================================================================
void
mxSetPiInterleavedComplex(mxArray* pm, double* pr)
{
    mexErrMsgTxt(_("mxGetPi not allowed with interleaved complex.").c_str());
}
//=============================================================================
void*
mxGetImagDataSeparatedComplex(const mxArray* pm)
{
    if (pm != nullptr) {
        if (!pm->iscomplex) {
            return nullptr;
        }
        return pm->imagdata;
    }
    return nullptr;
}
//=============================================================================
void
mxSetImagDataSeparatedComplex(mxArray* pm, void* pi)
{
    if (pm != nullptr) {
        pm->iscomplex = true;
        pm->imagdata = pi;
    }
}
//=============================================================================
