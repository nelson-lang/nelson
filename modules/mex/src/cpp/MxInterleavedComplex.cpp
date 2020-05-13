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
#include "matrix.h"
#include "i18n.hpp"
#include "MxHelpers.hpp"
//=============================================================================
mxComplexDouble*
mxGetComplexDoublesInterleavedComplex(const mxArray* pa)
{
    mxComplexDouble* result = nullptr;
    if (pa != nullptr) {
        bool isExpectedType = pa->iscomplex && pa->classID == mxDOUBLE_CLASS
            && pa->issparse == false;
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
        bool isExpectedType
            = pa->iscomplex == false && pa->classID == mxDOUBLE_CLASS && pa->issparse == false;
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
        bool isExpectedType
            = pa->iscomplex == false && pa->classID == mxDOUBLE_CLASS && pa->issparse == false;
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
        bool isExpectedType
            = pa->iscomplex == true && pa->classID == mxDOUBLE_CLASS && pa->issparse == false;
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
        bool isExpectedType
            = pa->iscomplex == true && pa->classID == mxSINGLE_CLASS && pa->issparse == false;
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
        bool isExpectedType
            = pa->iscomplex == false && pa->classID == mxSINGLE_CLASS && pa->issparse == false;
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
        bool isExpectedType
            = pa->iscomplex == false && pa->classID == mxSINGLE_CLASS && pa->issparse == false;
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
        bool isExpectedType
            = pa->iscomplex == true && pa->classID == mxSINGLE_CLASS && pa->issparse == false;
        if (isExpectedType) {
            pa->realdata = dt;
            retCode = 1;
        }
    }
    return retCode;
}
//=============================================================================
