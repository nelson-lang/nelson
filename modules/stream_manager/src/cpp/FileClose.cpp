//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FileClose.hpp"
#include "FileBackend.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
FileClose(FilesManager* fm, int no)
{
    if (fm) {
        File* file = fm->getFile(no);
        if (file != nullptr && file->getBackend() != nullptr) {
            // A backend finishes its data here; the identifier is released even on error.
            try {
                file->getBackend()->close();
            } catch (...) {
                fm->removeFile(no);
                throw;
            }
        }
        return fm->removeFile(no);
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
