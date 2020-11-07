//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <picosha2.h>
#include <vector>
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
