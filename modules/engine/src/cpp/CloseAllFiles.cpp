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
#include "FilesManager.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
CloseAllFiles()
{
    auto* fm = static_cast<FilesManager*>(NelsonConfiguration::getInstance()->getFileManager());
    if (fm == nullptr) {
        return;
    }
    auto* io = static_cast<Interface*>(fm->getFilePointer(1));
    auto* newFileManager = new FilesManager(io);
    delete fm;
    NelsonConfiguration::getInstance()->setFileManager(newFileManager);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
