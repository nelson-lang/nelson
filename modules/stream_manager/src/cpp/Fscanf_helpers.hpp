//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <cstring>
#include <cstdio>
#include <string>
#include <ctype.h>
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
enum OutputType
{
    AS_STRING,
    AS_DOUBLE,
    AS_INT64,
    AS_UINT64,
    AS_MIXED,
    AS_NONE
};
//=============================================================================
static int
flagChar(char c)
{
    return ((c == '#') || (c == '0') || (c == '-') || (c == ' ') || (c == '+'));
}
//=============================================================================
static int
convspec(wchar_t c)
{
    return ((c == 'd') || (c == 'i') || (c == 'o') || (c == 'u') || (c == 'x') || (c == 'e')
        || (c == 'f') || (c == 'g') || (c == 'c') || (c == 's'));
}
//=============================================================================
static char*
validateScanFormatSpec(char* cp)
{
    if (*cp == '%') {
        return cp + 1;
    }
    if (*cp == '*') {
        cp++;
    }
    while ((*cp) && flagChar(*cp)) {
        cp++;
    }
    while ((*cp) && isdigit(*cp)) {
        cp++;
    }
    while ((*cp) && (*cp == '.')) {
        cp++;
    }
    while ((*cp) && isdigit(*cp)) {
        cp++;
    }
    if ((*cp) && (convspec(*cp) || (*cp == 'l'))) {
        return cp + 1;
    }
    return nullptr;
}
//=============================================================================
// Reads a '%c' or '%s' conversion into a dynamic string, so long '%s' tokens
// cannot overflow a fixed stack buffer.
// 'spec' starts at '%' (for example "%c", "%5c%n", "%s", "%10s%n"). '%c' reads
// exactly 'width' characters (default 1, whitespace included). '%s' skips leading
// whitespace, then reads non-whitespace characters up to 'width' (unbounded by
// default) and pushes the terminating character back.
// Returns false on matching failure or EOF.
static bool
scanfReadCharsOrString(FILE* fp, const char* spec, bool asChar, std::string& value)
{
    const char* p = spec;
    if (*p == '%') {
        p++;
    }
    while (*p && flagChar(*p)) {
        p++;
    }
    long width = -1;
    if (*p && isdigit((unsigned char)*p)) {
        width = 0;
        while (*p && isdigit((unsigned char)*p)) {
            width = width * 10 + (*p - '0');
            p++;
        }
    }
    if (*p == '.') {
        p++;
        while (*p && isdigit((unsigned char)*p)) {
            p++;
        }
    }
    value.clear();
    if (asChar) {
        long toRead = (width < 0) ? 1 : width;
        for (long k = 0; k < toRead; ++k) {
            int c = fgetc(fp);
            if (c == EOF) {
                break;
            }
            value.push_back((char)c);
        }
        return !value.empty();
    }
    int c;
    while ((c = fgetc(fp)) != EOF && isspace((unsigned char)c)) { }
    if (c == EOF) {
        return false;
    }
    long maxChars = (width < 0) ? -1 : width;
    long count = 0;
    while (c != EOF && !isspace((unsigned char)c) && (maxChars < 0 || count < maxChars)) {
        value.push_back((char)c);
        ++count;
        c = fgetc(fp);
    }
    if (c != EOF) {
        ungetc(c, fp);
    }
    return !value.empty();
}
//=============================================================================
// Skips a suppressed conversion such as "%*d" or "%*s" without storing a value.
// A trailing "%n" detects how many characters were consumed.
// Returns false on matching failure or EOF before the field completes.
static bool
scanfConsumeSuppressed(FILE* fp, const std::string& spec)
{
    if (feof(fp)) {
        return false;
    }
    std::string fmt = spec;
    fmt += "%n";
    int consumed = -1;
    int r = fscanf(fp, fmt.c_str(), &consumed);
    (void)r;
    return consumed > 0;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
