//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "CloseAllFiles.hpp"
#include <cstdio>
//=============================================================================
#ifdef _MSC_VER
#define fcloseall _fcloseall
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
void
CloseAllFiles(void)
{
#ifdef _MSC_VER
    _fcloseall();
#else
#if defined(__APPLE__) || defined(__MACH__)
    FILE* fds_to_close[3]; /* the size being hardcoded to '3' is temporary */
    int i; /* loop counter */
    fds_to_close[0] = stdin;
    fds_to_close[1] = stdout;
    fds_to_close[2] = stderr;
    /* max iterations being hardcoded to '3' is temporary: */
    for ((i = 0); (i < 3); i++) {
        fclose(fds_to_close[i]);
    }
#else
    fcloseall();
#endif
#endif
}
//=============================================================================
}
//=============================================================================
