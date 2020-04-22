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
    typedef void (*mex_exit_fn)(void);
    //=============================================================================
#if !defined(NLSMEX_EXPORTS) && !defined(NLSENGINE_EXPORTS)
#ifndef MEX_AT_EXIT_ONCE
#define MEX_AT_EXIT_ONCE
    extern mex_exit_fn _exitFcn;
    extern bool _isLocked;
    //=============================================================================
    const char*
    mexFunctionName(void);
    //=============================================================================
    static int
    mexAtExit(mex_exit_fn exit_fcn)
    {
        _exitFcn = exit_fcn;
        return 0;
    }
    //=============================================================================
    static void
    mexLock(void)
    {
        _isLocked = true;
    }
    //=============================================================================
    static void
    mexUnlock(void)
    {
        _isLocked = false;
    }
    //=============================================================================
    static bool
    mexIsLocked(void)
    {
        return _isLocked;
    }
#endif
#endif
    //=============================================================================
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
