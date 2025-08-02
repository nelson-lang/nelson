//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <mz_compat.h>
#include <mz_zip.h>
#include <string>
#include <vector>
#include <iostream>
#include <ctime>
//=============================================================================
namespace Nelson {
//=============================================================================
class Zipper
{
private:
    zipFile m_zipFile { nullptr };
    bool m_entryOpen { false };

    void
    getTime(tm_zip& tmZip);

public:
    Zipper();
    ~Zipper();

    bool
    open(const char* filename, bool append = false);
    void
    close();
    bool
    isOpen();

    bool
    addEntry(const char* filename, uint32_t attributes);

    void
    closeEntry();
    bool
    isOpenEntry();

    Zipper&
    operator<<(std::istream& is);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
