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
#pragma once
//=============================================================================
#include <matrix.h>
#include <stdarg.h>
#include "nlsMex_exports.h"
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
    //=============================================================================
#define MX_CURRENT_API_VER 0x08000000
#define MX_LAST_SEPARATE_COMPLEX_VER 0x07300000
    //=============================================================================
    typedef void (*mex_exit_fn)(void);
    //=============================================================================
    /** defined in mxGateway.c */
    const char*
    mexFunctionName(void);
    //=============================================================================
    /** defined in mxGateway.c */
    void
    mexLock(void);
    //=============================================================================
    /** defined in mxGateway.c */
    void
    mexUnlock(void);
    //=============================================================================
    /** defined in mxGateway.c */
    bool
    mexIsLocked(void);
    //=============================================================================
    /** defined in mxGateway.c */
    int
    mexAtExit(mex_exit_fn exit_fcn);
    //=============================================================================
    /** defined in mxGateway.c */
    void
    mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]);
    //=============================================================================
    NLSMEX_IMPEXP int
    mexPrintf(const char* format, ...);
    //=============================================================================
    NLSMEX_IMPEXP void
    mexErrMsgTxt(const char* error_msg);
    //=============================================================================
    NLSMEX_IMPEXP void
    mexErrMsgIdAndTxt(const char* identifier, const char* err_msg, ...);
    //=============================================================================
    NLSMEX_IMPEXP void
    mexWarnMsgTxt(const char* warn_msg);
    //=============================================================================
    NLSMEX_IMPEXP void
    mexWarnMsgIdAndTxt(const char* warningid, const char* warningmsg, ...);
    //=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
