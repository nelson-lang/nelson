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
#include <cstdint>
#include <cstring>
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
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
constexpr std::size_t SMALL_FILE_MAX_SIZE = 1024 * 1024;
constexpr std::size_t MIN_BLOCK_SIZE = 64 * 1024;
constexpr std::size_t MAX_BLOCK_SIZE = 4 * 1024 * 1024;
constexpr std::size_t STANDARD_BLOCK_SIZE = 1024 * 1024;
//=============================================================================
inline void
normalize_eol_inplace(std::string& str, const std::string& target_eol)
{
    if (str.empty() || target_eol.empty()) {
        return;
    }

    size_t crlf_count = 0;
    size_t pos = 0;
    while ((pos = str.find("\r\n", pos)) != std::string::npos) {
        ++crlf_count;
        pos += 2;
    }

    if (crlf_count > 0) {
        std::string temp;
        temp.reserve(str.size() - crlf_count);
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
        errorMessage = Nelson::resolveMessageTemplate(
            L"Nelson:stream_manager:emptyFilename", L"Empty filename.");
        return false;
    }

    try {
        std::filesystem::path fileAsPath(filename);
        std::error_code ec;

        if (!std::filesystem::exists(fileAsPath, ec) || ec) {
            errorMessage = Nelson::resolveMessageTemplate(
                L"Nelson:load:fileDoesNotExist", L"File does not exist.");
            return false;
        }

        if (!std::filesystem::is_regular_file(fileAsPath, ec) || ec) {
            errorMessage = Nelson::resolveMessageTemplate(
                L"Nelson:stream_manager:regularFile", L"Not a regular file.");
            return false;
        }

        return true;
    } catch (const std::filesystem::filesystem_error& e) {
        errorMessage = formatI18nMessage(
            Nelson::resolveMessageTemplate(
                L"Nelson:stream_manager:filesystemError", L"Filesystem error: %s"),
            utf8_to_wstring(e.what()));
        return false;
    }
}
//=============================================================================
// Strict UTF-8 check (no overlong forms, no surrogates, at most U+10FFFF).
// Lets "auto" skip the costly charset detection for valid UTF-8 text.
inline bool
isValidUtf8(const std::string& data)
{
    const auto* p = reinterpret_cast<const unsigned char*>(data.data());
    const size_t n = data.size();
    size_t i = 0;
    while (i < n) {
        while (i + 8 <= n) {
            uint64_t block;
            std::memcpy(&block, p + i, sizeof(block));
            if ((block & 0x8080808080808080ULL) != 0) {
                break;
            }
            i += 8;
        }
        if (i >= n) {
            break;
        }
        unsigned char c = p[i];
        if (c < 0x80) {
            ++i;
            continue;
        }
        size_t len = 0;
        unsigned char low = 0x80;
        unsigned char high = 0xBF;
        if (c >= 0xC2 && c <= 0xDF) {
            len = 2;
        } else if (c >= 0xE0 && c <= 0xEF) {
            len = 3;
            low = (c == 0xE0) ? 0xA0 : 0x80;
            high = (c == 0xED) ? 0x9F : 0xBF;
        } else if (c >= 0xF0 && c <= 0xF4) {
            len = 4;
            low = (c == 0xF0) ? 0x90 : 0x80;
            high = (c == 0xF4) ? 0x8F : 0xBF;
        } else {
            return false;
        }
        if (i + len > n || p[i + 1] < low || p[i + 1] > high) {
            return false;
        }
        for (size_t k = 2; k < len; ++k) {
            if ((p[i + k] & 0xC0) != 0x80) {
                return false;
            }
        }
        i += len;
    }
    return true;
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
        if (encoding == L"auto" && !isValidUtf8(data)) {
            std::string encodingDetected = detectBestEncoding(data);
            std::string asUtf8;
            if (!charsetToUtf8Converter(data, encodingDetected, asUtf8)) {
                errorMessage = Nelson::resolveMessageTemplate(
                    L"Nelson:stream_manager:cannotConvertUnicode", L"Cannot convert to unicode.");
                return false;
            }
            normalize_eol_inplace(asUtf8, eol_utf8);
            content = utf8_to_wstring(asUtf8);
        } else if (encoding == L"UTF-8" || encoding == L"auto") {
            normalize_eol_inplace(data, eol_utf8);
            content = utf8_to_wstring(data);
        } else {
            std::string asUtf8;
            if (!charsetToUtf8Converter(data, wstring_to_utf8(encoding), asUtf8)) {
                errorMessage = Nelson::resolveMessageTemplate(
                    L"Nelson:stream_manager:cannotConvertUnicode", L"Cannot convert to unicode.");
                return false;
            }
            normalize_eol_inplace(asUtf8, eol_utf8);
            content = utf8_to_wstring(asUtf8);
        }
        return true;
    } catch (const std::exception& e) {
        errorMessage = formatI18nMessage(
            Nelson::resolveMessageTemplate(
                L"Nelson:stream_manager:encodingConversionError", L"Encoding conversion error: %s"),
            utf8_to_wstring(e.what()));
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

    if (!validateFileParameters(filename, errorMessage)) {
        return false;
    }

    try {
        std::filesystem::path fileAsPath(filename);

        if (fileSize == 0) {
            content = L"";
            return true;
        }

        std::ifstream fileStream(fileAsPath, std::ios::binary);
        if (!fileStream.is_open()) {
            errorMessage = Nelson::resolveMessageTemplate(
                L"Nelson:stream_manager:cannotOpenFile", L"Cannot open file.");
            return false;
        }

        std::string data;
        std::string eol_utf8 = wstring_to_utf8(eol);

        if (fileSize <= SMALL_FILE_MAX_SIZE) {
            data.reserve(fileSize);
            data.assign(
                std::istreambuf_iterator<char>(fileStream), std::istreambuf_iterator<char>());
        } else {
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

        return convertToUnicode(data, encoding, eol_utf8, content, errorMessage);

    } catch (const std::filesystem::filesystem_error& e) {
        errorMessage = formatI18nMessage(
            Nelson::resolveMessageTemplate(
                L"Nelson:stream_manager:filesystemError", L"Filesystem error: %s"),
            utf8_to_wstring(e.what()));
        return false;
    } catch (const std::bad_alloc&) {
        errorMessage = resolveMessageTemplate(L"Nelson:stream_manager:memoryAllocationFailedFile",
            L"Memory allocation failed - file too large.");
        return false;
    } catch (const std::exception& e) {
        errorMessage = formatI18nMessage(
            Nelson::resolveMessageTemplate(
                L"Nelson:stream_manager:unexpectedError", L"Unexpected error: %s"),
            utf8_to_wstring(e.what()));
        return false;
    } catch (...) {
        errorMessage = resolveMessageTemplate(
            L"Nelson:stream_manager:unknownErrorOccurred", L"Unknown error occurred.");
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
        std::filesystem::path fileAsPath(filename);
        std::error_code ec;
        auto fileSize = std::filesystem::file_size(fileAsPath, ec);

        if (ec) {
            errorMessage = resolveMessageTemplate(
                L"Nelson:stream_manager:cannotDetermineFileSize", L"Cannot determine file size.");
            return false;
        }

        return MapFileReadInternal(filename, eol, encoding, content, errorMessage, fileSize);

    } catch (...) {
        errorMessage = Nelson::resolveMessageTemplate(
            L"Nelson:stream_manager:unexpectedErrorOccurred", L"Unexpected error occurred.");
        return false;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
