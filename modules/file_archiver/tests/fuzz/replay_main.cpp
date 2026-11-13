//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <iterator>
#include <vector>
//=============================================================================
// Without libFuzzer: runs the fuzz target once on every file of the folders given
// (seed corpus and saved crashes), as a regression test any compiler can build.
extern "C" int
LLVMFuzzerTestOneInput(const uint8_t* data, size_t size);
//=============================================================================
int
main(int argc, char** argv)
{
    size_t inputs = 0;
    for (int k = 1; k < argc; ++k) {
        for (const auto& item : std::filesystem::recursive_directory_iterator(argv[k])) {
            if (!item.is_regular_file()) {
                continue;
            }
            std::ifstream file(item.path(), std::ios::binary);
            const std::vector<uint8_t> bytes(
                (std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
            LLVMFuzzerTestOneInput(bytes.data(), bytes.size());
            ++inputs;
        }
    }
    std::cout << inputs << " inputs replayed\n";
    return inputs == 0 ? 1 : 0;
}
//=============================================================================
