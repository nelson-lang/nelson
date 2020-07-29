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
    /**
     * Name of current MEX function
     * defined in mxGateway.c
     * @return Name of the current MEX function.
     */
    const char*
    mexFunctionName(void);
    //=============================================================================
    /**
     * Prevent clearing MEX-file from memory
     * defined in mxGateway.c
     */
    void
    mexLock(void);
    //=============================================================================
    /**
     * Allow clearing MEX-file from memory
     * defined in mxGateway.c
     */
    void
    mexUnlock(void);
    //=============================================================================
    /**
     * Determine if MEX- file is locked
     * defined in mxGateway.c
     * @return Logical 1 (true) if the MEX-file is locked; logical 0 (false) if the file is
     unlocked.
     */
    bool
    mexIsLocked(void);
    //=============================================================================
    /**
     * Register function to call when MEX function clears or Nelson terminates
     * defined in mxGateway.c
     * @param exit_fcn Pointer to function you want to run on exit
     * @return always returns 0.
     */
    int
    mexAtExit(mex_exit_fn exit_fcn);
    //=============================================================================
    /**
     * Entry point to C/C++ MEX function built with C Matrix API
     * defined in mxGateway.c
     * @param nlhs Number of output arguments
     * @param plhs Array of pointers to the expected mxArray output arguments.
     * @param nrhs Number of input arguments
     * @param prhs Array of pointers to the mxArray input arguments.
     * Never changing the data in these read-only mxArrays.
     */
    void
    mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[]);
    //=============================================================================
    /**
     * ANSI C PRINTF-style output routine
     * @param format String to display.
     * @return Number of characters printed including characters specified with backslash codes.
     */
    NLSMEX_IMPEXP int
    mexPrintf(const char* format, ...);
    //=============================================================================
    /**
     * Display error message and return to Nelson prompt
     * @param error_msg error message to display
     */
    NLSMEX_IMPEXP void
    mexErrMsgTxt(const char* error_msg);
    //=============================================================================
    /**
     * Display error message with identifier and return to Nelson prompt
     * @param identifier message identifier.
     * @param err_msg String to display
     */
    NLSMEX_IMPEXP void
    mexErrMsgIdAndTxt(const char* identifier, const char* err_msg, ...);
    //=============================================================================
    /**
     * Warning message
     * @param warn_msg warning message.
     */
    NLSMEX_IMPEXP void
    mexWarnMsgTxt(const char* warn_msg);
    //=============================================================================
    /**
     * Warning message with identifier
     * @param warningid message identifier.
     * @param warningmsg String to display
     */
    NLSMEX_IMPEXP void
    mexWarnMsgIdAndTxt(const char* warningid, const char* warningmsg, ...);
    //=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
