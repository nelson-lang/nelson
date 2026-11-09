//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cctype>
#include <cstdio>
#include <cstring>
#ifdef _MSC_VER
#include <share.h>
#endif
#include "TextFileWriter.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static FILE*
openFile(const std::wstring& filename, const wchar_t* mode)
{
#ifdef _MSC_VER
    return _wfsopen(filename.c_str(), mode, _SH_DENYNO);
#else
    return fopen(wstring_to_utf8(filename).c_str(), wstring_to_utf8(mode).c_str());
#endif
}
//=============================================================================
static bool
isCharRow(const ArrayOf& value)
{
    // 1-by-N or 0-by-0 only: a 0-by-3 char is not text.
    if (!value.isCharacterArray() || !value.is2D()) {
        return false;
    }
    Dimensions dims = value.dimensions();
    return dims.rows() == 1 || (dims.rows() == 0 && dims.columns() == 0);
}
//=============================================================================
static TextSpan
charSpan(const ArrayOf& value)
{
    auto length = static_cast<size_t>(value.elementCount());
    return { length > 0 ? static_cast<const wchar_t*>(value.dataPointer()) : nullptr, length };
}
//=============================================================================
bool
textSpans(const ArrayOf& value, std::vector<TextSpan>& spans)
{
    spans.clear();
    if (value.isCharacterArray()) {
        if (!isCharRow(value)) {
            return false;
        }
        spans.push_back(charSpan(value));
        return true;
    }
    if (!value.isStringArray() && !value.isCell()) {
        return false;
    }
    const auto* elements = static_cast<const ArrayOf*>(value.dataPointer());
    auto nbElements = static_cast<size_t>(value.elementCount());
    spans.reserve(nbElements);
    for (size_t k = 0; k < nbElements; ++k) {
        const ArrayOf& element = elements[k];
        if (isCharRow(element)) {
            spans.push_back(charSpan(element));
        } else if (value.isStringArray()) {
            spans.push_back({ nullptr, 0 });
        } else {
            spans.clear();
            return false;
        }
    }
    return true;
}
//=============================================================================
static void
pushCodePoint(std::string& out, uint32_t cp)
{
    if (cp < 0x80) {
        out.push_back(static_cast<char>(cp));
    } else if (cp < 0x800) {
        out.push_back(static_cast<char>(0xC0 | (cp >> 6)));
        out.push_back(static_cast<char>(0x80 | (cp & 0x3F)));
    } else if (cp < 0x10000) {
        out.push_back(static_cast<char>(0xE0 | (cp >> 12)));
        out.push_back(static_cast<char>(0x80 | ((cp >> 6) & 0x3F)));
        out.push_back(static_cast<char>(0x80 | (cp & 0x3F)));
    } else {
        out.push_back(static_cast<char>(0xF0 | (cp >> 18)));
        out.push_back(static_cast<char>(0x80 | ((cp >> 12) & 0x3F)));
        out.push_back(static_cast<char>(0x80 | ((cp >> 6) & 0x3F)));
        out.push_back(static_cast<char>(0x80 | (cp & 0x3F)));
    }
}
//=============================================================================
void
appendUtf8(std::string& out, const wchar_t* text, size_t length)
{
    // ASCII prefix copied without per-character branching on the output.
    size_t start = out.size();
    out.resize(start + length);
    char* dest = out.data() + start;
    size_t i = 0;
    for (; i < length; ++i) {
        auto c = static_cast<uint32_t>(text[i]);
        if (c >= 0x80) {
            break;
        }
        dest[i] = static_cast<char>(c);
    }
    out.resize(start + i);
    constexpr uint32_t REPLACEMENT = 0xFFFD;
    for (; i < length; ++i) {
        auto c = static_cast<uint32_t>(text[i]);
        if (c >= 0xD800 && c <= 0xDFFF) {
            bool paired = sizeof(wchar_t) == 2 && c <= 0xDBFF && i + 1 < length
                && static_cast<uint32_t>(text[i + 1]) >= 0xDC00
                && static_cast<uint32_t>(text[i + 1]) <= 0xDFFF;
            if (paired) {
                c = 0x10000 + ((c - 0xD800) << 10) + (static_cast<uint32_t>(text[i + 1]) - 0xDC00);
                ++i;
            } else {
                c = REPLACEMENT;
            }
        } else if (c > 0x10FFFF) {
            c = REPLACEMENT;
        }
        pushCodePoint(out, c);
    }
}
//=============================================================================
static bool
isUtf8Name(const std::string& encoding)
{
    std::string lower(encoding);
    for (char& c : lower) {
        c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
    }
    return lower == "utf-8" || lower == "utf8";
}
//=============================================================================
bool
encodeUtf8Text(const std::string& utf8, const std::string& encoding, std::string& encoded)
{
    if (utf8.empty() || isUtf8Name(encoding)) {
        encoded = utf8;
        return true;
    }
    return utf8ToCharsetConverter(utf8, encoded, encoding);
}
//=============================================================================
TextWriteStatus
writeUtf8TextFile(
    const std::wstring& filename, const std::string& utf8, const std::string& encoding, bool append)
{
    std::string converted;
    const std::string* bytes = &utf8;
    if (!utf8.empty() && !isUtf8Name(encoding)) {
        if (!encodeUtf8Text(utf8, encoding, converted)) {
            return TextWriteStatus::ENCODING_FAILED;
        }
        bytes = &converted;
    }
    FILE* fp = openFile(filename, append ? L"ab" : L"wb");
    if (fp == nullptr) {
        return TextWriteStatus::OPEN_FAILED;
    }
    size_t written = bytes->empty() ? 0 : fwrite(bytes->data(), 1, bytes->size(), fp);
    bool closed = fclose(fp) == 0;
    return (written == bytes->size() && closed) ? TextWriteStatus::OK
                                                : TextWriteStatus::WRITE_FAILED;
}
//=============================================================================
bool
fileEndsWith(const std::wstring& filename, const std::string& suffix, uint64_t& fileSize)
{
    fileSize = 0;
    FILE* fp = openFile(filename, L"rb");
    if (fp == nullptr) {
        return false;
    }
#ifdef _MSC_VER
    bool sought = _fseeki64(fp, 0, SEEK_END) == 0;
    int64_t size = sought ? _ftelli64(fp) : -1;
#else
    bool sought = fseeko(fp, 0, SEEK_END) == 0;
    int64_t size = sought ? static_cast<int64_t>(ftello(fp)) : -1;
#endif
    bool matches = false;
    if (size > 0) {
        fileSize = static_cast<uint64_t>(size);
        auto n = static_cast<int64_t>(suffix.size());
        if (n > 0 && size >= n) {
            std::string tail(suffix.size(), '\0');
#ifdef _MSC_VER
            bool positioned = _fseeki64(fp, size - n, SEEK_SET) == 0;
#else
            bool positioned = fseeko(fp, static_cast<off_t>(size - n), SEEK_SET) == 0;
#endif
            matches = positioned && fread(tail.data(), 1, tail.size(), fp) == tail.size()
                && tail == suffix;
        }
    }
    fclose(fp);
    return matches;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
