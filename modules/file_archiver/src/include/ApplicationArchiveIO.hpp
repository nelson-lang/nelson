//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#pragma once
//=============================================================================
#include "ApplicationArchiveFormat.hpp"
#include "picoSHA2/picosha2.h"
#include <cstdio>
//=============================================================================
namespace Nelson::ApplicationArchive {
inline std::string
transfer(std::istream& input, FILE* output, uint64_t length)
{
    picosha2::hash256_one_by_one hash;
    std::array<char, 32768> block;
    while (length > 0) {
        const auto count = static_cast<size_t>(std::min<uint64_t>(block.size(), length));
        input.read(block.data(), count);
        require(input.good(), "Cannot read application archive data.");
        require(output == nullptr || fwrite(block.data(), 1, count, output) == count,
            "Cannot write application archive data.");
        hash.process(block.begin(), block.begin() + count);
        length -= count;
    }
    hash.finish();
    return picosha2::get_hash_hex_string(hash);
}
//=============================================================================
inline FILE*
createFile(const std::filesystem::path& path)
{
#ifdef _WIN32
    FILE* file = nullptr;
    _wfopen_s(&file, path.c_str(), L"wbx");
#else
    FILE* file = fopen(path.c_str(), "wbx");
#endif
    require(file != nullptr, "Cannot create application file; destination must not exist.");
    return file;
}
//=============================================================================
} // namespace Nelson::ApplicationArchive
//=============================================================================
