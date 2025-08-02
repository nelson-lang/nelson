//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileTell.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
int64
FileTell(File* fp)
{
    int64 pos = -1L;
    if (fp) {
        if (fp->isInterfaceMethod()) {
            return pos;
        }
        FILE* fileptr = static_cast<FILE*>(fp->getFilePointer());
        if (fileptr) {
            pos = static_cast<int64>(NLSFTELL(fileptr));
        }
    }
    return pos;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
