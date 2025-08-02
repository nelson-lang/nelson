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
#pragma warning(disable : 4018)
#endif
//=============================================================================
#include "StringHelpers.hpp"
#include <cstring>
#include <cstdio>
#include <memory>
#include <stdexcept>
#include <algorithm>
#include "FileGetLine.hpp"
#include "FileSeek.hpp"
#include "FileTell.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define BUFFER_LINE_SIZE (64 * 2)
//=============================================================================
static bool
getline(std::string& line, FILE* fp, bool& isCRLF)
{
    isCRLF = false;
    line.clear();
    if (fp == nullptr) {
        return false;
    }
    auto posBegin = NLSFTELL(fp);
    char chunk[BUFFER_LINE_SIZE];
    memset(chunk, '\0', sizeof(char) * BUFFER_LINE_SIZE);
    size_t len = BUFFER_LINE_SIZE;
    while (fgets(chunk, sizeof(chunk), fp) != nullptr) {
        line.append(chunk);
        memset(chunk, '\0', sizeof(char) * BUFFER_LINE_SIZE);
        if (line[line.size() - 1] == '\n') {
            auto posEnd = NLSFTELL(fp);
            isCRLF = (posEnd - posBegin) > line.size();
            return true;
        }
        if (feof(fp)) {
            auto posEnd = NLSFTELL(fp);
            isCRLF = (posEnd - posBegin) > line.size();
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
FileGetLine(File* fp, int nchar, bool bWithNewLine, std::wstring& result)
{
    bool bOK = false;
    result.clear();
    if (!fp) {
        return false;
    }
    if (fp->isInterfaceMethod()) {
        return false;
    }
    FILE* fileptr = static_cast<FILE*>(fp->getFilePointer());
    if (!fileptr) {
        return false;
    }
    if (feof(fileptr) || ferror(fileptr)) {
        return false;
    }
    std::string readline;
    bool isCRLF = false;
#ifdef _MSC_VER
    if (nchar != -1) {
        // Very old bug in CRT. it will be certainly never fixed for compatibility ...
        // https://developercommunity.visualstudio.com/content/problem/425878/fseek-ftell-fail-in-text-mode-for-unix-style-text.html
        setvbuf(fileptr, nullptr, _IONBF, 0);
    }
#endif
    if (!getline(readline, fileptr, isCRLF)) {
        bOK = false;
    } else {
        if (!bWithNewLine) {
            if (readline.length() > 0) {
                if (readline[readline.length() - 1] == '\n') {
                    readline.pop_back();
                }
            }
        }
        if (nchar == 0) {
            result.clear();
            return true;
        }
        std::string encoding = wstring_to_utf8(fp->getEncoding());
        if (encoding == "UTF-8") {
            result = utf8_to_wstring(readline);
            bOK = true;
        } else {
            std::string asUtf8;
            bOK = charsetToUtf8Converter(readline, encoding, asUtf8);
            result = utf8_to_wstring(asUtf8);
        }
        if (nchar == -1) {
            return bOK;
        }
        size_t nbChars = std::min((size_t)nchar, (size_t)result.length());
        std::wstring w = result;
        result.resize(nbChars);
        try {
            w = w.substr(nbChars);
            if (isCRLF) {
                StringHelpers::replace_all(w, L"\n", L"\r\n");
            }
        } catch (const std::out_of_range&) {
            result.clear();
            return false;
        }
        std::string u;
        if (encoding == "UTF-8") {
            u = wstring_to_utf8(w);
        } else {
            std::string asUtf8 = wstring_to_utf8(w);
            if (!utf8ToCharsetConverter(asUtf8, u, encoding)) {
                result.clear();
                return false;
            }
        }
        auto nseek = static_cast<int64>(u.length());
        FileSeek(fp, -nseek, 0);
        bOK = true;
    }
    return bOK;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
