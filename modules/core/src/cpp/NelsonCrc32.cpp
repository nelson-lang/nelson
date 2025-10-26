//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <sstream>
#include <fstream>
#include <cstdint>
#include "NelsonCrc32.hpp"
#include "characters_encoding.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class CRC32
{
private:
    static constexpr uint32_t POLYNOMIAL = 0xEDB88320;
    uint32_t table[256];
    uint32_t crc;

    void
    generateTable()
    {
        for (uint32_t i = 0; i < 256; i++) {
            uint32_t c = i;
            for (int j = 0; j < 8; j++) {
                if (c & 1) {
                    c = POLYNOMIAL ^ (c >> 1);
                } else {
                    c >>= 1;
                }
            }
            table[i] = c;
        }
    }

public:
    CRC32() : crc(0xFFFFFFFF) { generateTable(); }

    void
    process_bytes(const void* buffer, size_t byte_count)
    {
        const unsigned char* data = static_cast<const unsigned char*>(buffer);
        for (size_t i = 0; i < byte_count; i++) {
            crc = table[(crc ^ data[i]) & 0xFF] ^ (crc >> 8);
        }
    }

    uint32_t
    checksum() const
    {
        return crc ^ 0xFFFFFFFF;
    }

    void
    reset()
    {
        crc = 0xFFFFFFFF;
    }
};
//=============================================================================
std::wstring
computeStringToCrc32(const std::wstring& str)
{
    std::string utf8str = wstring_to_utf8(str);
    CRC32 crc;
    crc.process_bytes(utf8str.data(), utf8str.size());
    std::stringstream ss;
    ss << std::hex << crc.checksum();
    return utf8_to_wstring(StringHelpers::to_upper_copy(ss.str()));
}
//=============================================================================
#ifndef PRIVATE_BUFFER_SIZE
#define PRIVATE_BUFFER_SIZE 1024
#endif
//=============================================================================
std::wstring
computeFileToCrc32(const std::wstring& filename)
{
    std::wstring res = L"";
#ifdef _MSC_VER
    std::ifstream ifs(filename.c_str(), std::ios_base::binary);
#else
    std::ifstream ifs(wstring_to_utf8(filename).c_str(), std::ios_base::binary);
#endif
    if (ifs) {
        CRC32 crc;
        do {
            char buffer[PRIVATE_BUFFER_SIZE];
            ifs.read(buffer, PRIVATE_BUFFER_SIZE);
            crc.process_bytes(buffer, static_cast<size_t>(ifs.gcount()));
        } while (ifs);
        ifs.close();
        std::stringstream ss;
        ss << std::hex << crc.checksum();
        res = utf8_to_wstring(StringHelpers::to_upper_copy(ss.str()));
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
