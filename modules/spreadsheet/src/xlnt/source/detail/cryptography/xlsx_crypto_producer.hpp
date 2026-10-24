// Copyright (c) 2014-2022 Thomas Fussell
// Copyright (c) 2024-2026 xlnt-community
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE
//
// @license: http://www.opensource.org/licenses/mit-license.php
// @author: see AUTHORS file

#include <cstdint>
#include <string>
#include <vector>

#include <xlnt/xlnt_config.hpp>
#include <detail/xlnt_config_impl.hpp>
#include <xlnt/internal/features.hpp>

#if XLNT_HAS_INCLUDE(<string_view>) && XLNT_HAS_FEATURE(U8_STRING_VIEW)
#include <string_view>
#endif

// NOTE: encryption and decryption is not part of the OOXML specification (ECMA-376).
// This implementation is based on the "[MS-OFFCRYPTO]: Office Document Cryptography Structure"
// specification.
namespace xlnt {
namespace detail {

    XLNT_API_INTERNAL std::vector<std::uint8_t>
    encrypt_xlsx(const std::vector<std::uint8_t>& bytes, const std::string& password);

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
    XLNT_API_INTERNAL std::vector<std::uint8_t>
    encrypt_xlsx(const std::vector<std::uint8_t>& bytes, std::u8string_view password);
#endif

} // namespace detail
} // namespace xlnt
