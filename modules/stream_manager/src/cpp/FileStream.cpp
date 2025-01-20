//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include "FileStream.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
FileStream::FileStream(const std::wstring& filename, const std::wstring& accessmode)
{
#ifdef _MSC_VER
    fp = _wfopen(filename.c_str(), accessmode.c_str());
#else
    fp = fopen(wstring_to_utf8(filename).c_str(), wstring_to_utf8(accessmode).c_str());
#endif
    if (fp == nullptr) {
        Error(_W("Unable to open file: ") + filename);
    }
    autoclose = true;
}
//=============================================================================
FileStream::FileStream(FILE* afp)
{
    fp = afp;
    autoclose = false;
}
//=============================================================================
FileStream::~FileStream()
{
    if (autoclose) {
        fclose(fp);
    }
}
//=============================================================================
void
FileStream::writeBytes(const void* data, int len)
{
    if (fp) {
        fwrite(data, sizeof(char), len, fp);
    }
}
//=============================================================================
void
FileStream::readBytes(void* data, int len)
{
    if (fp && (fread(data, sizeof(char), len, fp) != static_cast<size_t>(len))) {
        // Optional: Log or handle the incomplete read
        // This avoids ignoring the result outright
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
