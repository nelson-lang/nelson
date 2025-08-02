//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
CloseAllFiles()
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
} // namespace Nelson
//=============================================================================
