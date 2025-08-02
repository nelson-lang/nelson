//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include <cstring>
#include "FileError.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
FileError(FilesManager* fm, int no, bool withClear, int& errorCode, std::string& errorMessage)
{
    errorMessage.clear();
    if (fm) {
        if (fm->isStdStream(no)) {
            errorCode = 0;
            errorMessage = "";
            return true;
        }
        FILE* fptr = static_cast<FILE*>(fm->getFilePointer(no));
        if (fptr) {
            errorCode = -(ferror(fptr));
            if (errorCode != 0) {
                errorMessage = std::string(strerror(errorCode));
            }
            if (withClear) {
                clearerr(fptr);
            }
            return true;
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
