//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstdio>
#include <cstddef>
#include <fstream>
#include <filesystem>
#include <vector>
#include <memory>
#include <system_error>
#include <algorithm>
#include "StringHelpers.hpp"
#include "MapFileRead.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
constexpr std::size_t SMALL_FILE_MAX_SIZE = 1024 * 1024; // 1MB
constexpr std::size_t MIN_BLOCK_SIZE = 64 * 1024; // 64 KB
constexpr std::size_t MAX_BLOCK_SIZE = 4 * 1024 * 1024; // 4 MB
constexpr std::size_t STANDARD_BLOCK_SIZE = 1024 * 1024; // 1MB
//=============================================================================
inline void
normalize_eol_inplace(std::string& str, const std::string& target_eol)
{
    if (str.empty())
        return;

    // Step 1: Replace \r\n with \n (more efficient using reserve)
    size_t crlf_count = 0;
    size_t pos = 0;
    while ((pos = str.find("\r\n", pos)) != std::string::npos) {
        ++crlf_count;
        pos += 2;
    }

    // If there are no CRLF sequences, we can skip the replacement
    if (crlf_count > 0) {
        std::string temp;
        temp.reserve(str.size() - crlf_count); // Reserve exact size
        pos = 0;
        size_t last_pos = 0;
        while ((pos = str.find("\r\n", last_pos)) != std::string::npos) {
            temp.append(str, last_pos, pos - last_pos);
            temp.push_back('\n');
            last_pos = pos + 2;
        }
        temp.append(str, last_pos);
        str = std::move(temp);
    }

    if (target_eol != "\n" && !target_eol.empty()) {
        size_t lf_count = 0;
        pos = 0;
        while ((pos = str.find('\n', pos)) != std::string::npos) {
            ++lf_count;
            ++pos;
        }

        if (lf_count > 0) {
            std::string temp;
            temp.reserve(str.size() + lf_count * (target_eol.size() - 1));
            pos = 0;
            size_t last_pos = 0;
            while ((pos = str.find('\n', last_pos)) != std::string::npos) {
                temp.append(str, last_pos, pos - last_pos);
                temp.append(target_eol);
                last_pos = pos + 1;
            }
            temp.append(str, last_pos);
            str = std::move(temp);
        }
    }
}
//=============================================================================
inline bool
validateFileParameters(const std::wstring& filename, std::wstring& errorMessage)
{
    if (filename.empty()) {
        errorMessage = _W("Empty filename.");
        return false;
    }

    try {
        std::filesystem::path fileAsPath(filename);
        std::error_code ec;

        if (!std::filesystem::exists(fileAsPath, ec) || ec) {
            errorMessage = _W("File does not exist.");
            return false;
        }

        if (!std::filesystem::is_regular_file(fileAsPath, ec) || ec) {
            errorMessage = _W("Not a regular file.");
            return false;
        }

        return true;
    } catch (const std::filesystem::filesystem_error& e) {
        errorMessage = _W("Filesystem error: ") + utf8_to_wstring(e.what());
        return false;
    }
}
//=============================================================================
inline bool
convertToUnicode(std::string& data, const std::wstring& encoding, const std::string& eol_utf8,
    std::wstring& content, std::wstring& errorMessage)
{
    if (data.empty()) {
        content = L"";
        return true;
    }

    try {
        if (encoding == L"auto") {
            std::string encodingDetected = detectBestEncoding(data);
            std::string asUtf8;
            if (!charsetToUtf8Converter(data, encodingDetected, asUtf8)) {
                errorMessage = _W("Cannot convert to unicode.");
                return false;
            }
            normalize_eol_inplace(asUtf8, eol_utf8);
            content = utf8_to_wstring(asUtf8);
        } else if (encoding == L"UTF-8") {
            normalize_eol_inplace(data, eol_utf8);
            content = utf8_to_wstring(data);
        } else {
            std::string asUtf8;
            if (!charsetToUtf8Converter(data, wstring_to_utf8(encoding), asUtf8)) {
                errorMessage = _W("Cannot convert to unicode.");
                return false;
            }
            normalize_eol_inplace(asUtf8, eol_utf8);
            content = utf8_to_wstring(asUtf8);
        }
        return true;
    } catch (const std::exception& e) {
        errorMessage = _W("Encoding conversion error: ") + utf8_to_wstring(e.what());
        return false;
    }
}
//=============================================================================
bool
MapFileReadInternal(const std::wstring& filename, const std::wstring& eol,
    const std::wstring& encoding, std::wstring& content, std::wstring& errorMessage,
    size_t fileSize)
{
    content.clear();
    errorMessage.clear();

    // Validate parameters and file
    if (!validateFileParameters(filename, errorMessage)) {
        return false;
    }

    try {
        std::filesystem::path fileAsPath(filename);

        if (fileSize == 0) {
            content = L"";
            return true;
        }

        // Open the file
        std::ifstream fileStream(fileAsPath, std::ios::binary);
        if (!fileStream.is_open()) {
            errorMessage = _W("Cannot open file.");
            return false;
        }

        std::string data;
        std::string eol_utf8 = wstring_to_utf8(eol);

        // Choose reading strategy based on file size
        if (fileSize <= SMALL_FILE_MAX_SIZE) {
            // Small files: read entire file at once
            data.reserve(fileSize);
            data.assign(
                std::istreambuf_iterator<char>(fileStream), std::istreambuf_iterator<char>());
        } else {
            // Large files: read in adaptive blocks
            constexpr std::size_t ADAPTIVE_BLOCK_SIZE_DIVISOR = 100;
            std::size_t blockSize = std::min(
                MAX_BLOCK_SIZE, std::max(MIN_BLOCK_SIZE, fileSize / ADAPTIVE_BLOCK_SIZE_DIVISOR));

            data.reserve(fileSize);
            std::vector<char> buffer(blockSize);

            while (fileStream && data.size() < fileSize) {
                fileStream.read(buffer.data(), blockSize);
                std::streamsize bytesRead = fileStream.gcount();
                if (bytesRead > 0) {
                    size_t remainingSpace = fileSize - data.size();
                    size_t bytesToCopy = std::min(static_cast<size_t>(bytesRead), remainingSpace);
                    data.append(buffer.data(), bytesToCopy);
                }
            }
        }

        // Convert to unicode with common logic
        return convertToUnicode(data, encoding, eol_utf8, content, errorMessage);

    } catch (const std::filesystem::filesystem_error& e) {
        errorMessage = _W("Filesystem error: ") + utf8_to_wstring(e.what());
        return false;
    } catch (const std::bad_alloc&) {
        errorMessage = _W("Memory allocation failed - file too large.");
        return false;
    } catch (const std::exception& e) {
        errorMessage = _W("Unexpected error: ") + utf8_to_wstring(e.what());
        return false;
    } catch (...) {
        errorMessage = _W("Unknown error occurred.");
        return false;
    }
}
//=============================================================================
bool
MapFileRead(const std::wstring& filename, const std::wstring& eol, const std::wstring& encoding,
    std::wstring& content, std::wstring& errorMessage)
{
    content.clear();
    errorMessage.clear();

    try {
        // Quick file size check to determine strategy
        std::filesystem::path fileAsPath(filename);
        std::error_code ec;
        auto fileSize = std::filesystem::file_size(fileAsPath, ec);

        if (ec) {
            errorMessage = _W("Cannot determine file size.");
            return false;
        }

        return MapFileReadInternal(filename, eol, encoding, content, errorMessage, fileSize);

    } catch (...) {
        errorMessage = _W("Unexpected error occurred.");
        return false;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
