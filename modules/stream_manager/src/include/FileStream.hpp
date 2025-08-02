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
#include "Stream.hpp"
#include "nlsStream_manager_exports.h"
#include <cstdio>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSSTREAM_MANAGER_IMPEXP FileStream : public Stream
{
private:
    bool autoclose;
    FILE* fp;

public:
    FileStream(const std::wstring& filename, const std::wstring& accessmode);
    FileStream(FILE* afp);
    // Close the file
    ~FileStream();
    // Write a sequence of bytes to the file
    void
    writeBytes(const void* data, int len) override;
    // Read a sequence of bytes from the file
    void
    readBytes(void* data, int len) override;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
