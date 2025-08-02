//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include "mex.h"
#include "MxAsserts.h"
#include "i18n.hpp"
//=============================================================================
void
mexPrintAssertion(const char* test, const char* fname, int linenum, const char* message)
{
    if (test != nullptr && strlen(test) > 0) {
        if ((message != nullptr) && (message[0] != 0)) {
            mexErrMsgIdAndTxt("Nelson:MEX",
                _("Assertion failed: %s, at line %d of file \"%s\".\n%s\n").c_str(), test, linenum,
                fname, message);
        } else {
            mexErrMsgIdAndTxt("Nelson:MEX",
                _("Assertion failed: %s, at line %d of file \"%s\".\n").c_str(), test, linenum,
                fname);
        }
    } else {
        if ((message != nullptr) && (message[0] != 0)) {
            mexErrMsgIdAndTxt("Nelson:MEX",
                _("Assertion failed: at line %d of file \"%s\".\n%s\n").c_str(), linenum, fname,
                message);
        } else {
            mexErrMsgIdAndTxt("Nelson:MEX",
                _("Assertion failed: at line %d of file \"%s\".\n").c_str(), linenum, fname);
        }
    }
}
//=============================================================================
