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
#include <string.h>
#include "mex.h"
#include "MxAsserts.h"
#include "i18n.hpp"
//=============================================================================
void
mexPrintAssertion(const char* test, const char* fname, int linenum, const char* message)
{
    if (test != nullptr && strlen(test) > 0) {
        if (message && message[0]) {
            mexErrMsgIdAndTxt("Nelson:MEX",
                _("Assertion failed: %s, at line %d of file \"%s\".\n%s\n").c_str(), test, linenum,
                fname, message);
        } else {
            mexErrMsgIdAndTxt("Nelson:MEX",
                _("Assertion failed: %s, at line %d of file \"%s\".\n").c_str(), test, linenum,
                fname);
        }
    } else {
        if (message && message[0]) {
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
