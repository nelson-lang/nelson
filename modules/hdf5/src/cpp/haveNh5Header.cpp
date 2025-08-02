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
#include "haveNh5Header.hpp"
#include "characters_encoding.hpp"
#include "h5SaveLoadHelpers.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
haveNh5Header(const std::wstring& filename, std::wstring& header, int16& version, int16& endian)
{
    char* header_saturated = nullptr;
    try {
        header_saturated = new char[128];
    } catch (std::bad_alloc&) {
        return false;
    }
    char* header_offset = nullptr;
    try {
        header_offset = new char[8];
    } catch (std::bad_alloc&) {
        delete[] header_saturated;
        return false;
    }
    FILE* fp = nullptr;
#ifdef _MSC_VER
    fp = _wfopen(filename.c_str(), L"rb");
#else
    fp = fopen(wstring_to_utf8(filename).c_str(), "rb");
#endif
    if (!fp) {
        delete[] header_saturated;
        delete[] header_offset;
        return false;
    }
    size_t bytesread = 0;
    bytesread += fread(header_saturated, 1, 116, fp);
    header_saturated[116] = '\0';
    bytesread += fread(header_offset, 1, 8, fp);
    int16 temp2 = 0;
    bytesread += 2 * fread(&temp2, 2, 1, fp);
    int16 temp1 = 0;
    bytesread += fread(&temp1, 1, 2, fp);
    fclose(fp);
    bool res = false;
    if (128 == bytesread) {
        header = utf8_to_wstring(header_saturated);
        StringHelpers::trim_right(header);
        if (StringHelpers::starts_with(utf8_to_wstring(header_saturated), NELSON_HEADER)) {
            endian = temp1;
            version = temp2;
            res = true;
        }
    }
    delete[] header_saturated;
    delete[] header_offset;
    return res;
}
//=============================================================================//=============================================================================
} // namespace Nelson
//=============================================================================
