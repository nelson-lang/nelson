//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <picosha2.h>
#include <vector>
#include <fstream>
#include "NelsonSHA256.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
computeStringToSHA256(const std::wstring& str)
{
    std::string sha256;
    picosha2::hash256_hex_string(wstring_to_utf8(str), sha256);
    return utf8_to_wstring(sha256);
}
//=============================================================================
std::wstring
computeFileToSHA256(const std::wstring& filename)
{
#ifdef _MSC_VER
    std::ifstream inputFile(filename.c_str(), std::ios::in | std::ios::binary);
#else
    std::ifstream inputFile(wstring_to_utf8(filename).c_str(), std::ios::in | std::ios::binary);
#endif
    std::string sha256;
    if (inputFile.is_open()) {
        std::vector<unsigned char> hashVec(picosha2::k_digest_size);
        picosha2::hash256(inputFile, hashVec.begin(), hashVec.end());
        picosha2::bytes_to_hex_string(hashVec.begin(), hashVec.end(), sha256);
        inputFile.close();
    }
    return utf8_to_wstring(sha256);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
