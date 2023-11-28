//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4244)
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include <cstdio>
#include <cstring>
#include <vector>
#include "StringHelpers.hpp"
#include "PrintfFunction.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PrintfHelper.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
printfFunction(const ArrayOfVector& args, std::wstring& errorMessage, std::wstring& result)
{
    ArrayOf format(args[0]);
    std::wstring frmt = format.getContentAsWideString();

    std::vector<wchar_t> buffer(frmt.length() + 1);
    wcsncpy(&buffer[0], frmt.c_str(), frmt.length() + 1);

    wchar_t* dp = &buffer[0];
#define BUFFER_SIZE_MAX 65534
    wchar_t nbuff[BUFFER_SIZE_MAX];
    errorMessage.clear();
    memset(nbuff, 0, BUFFER_SIZE_MAX * sizeof(wchar_t));
    PrintfHelper ps(args);
    while ((*dp) || ps.HasMoreData()) {
        if (!(*dp) && ps.HasMoreData()) {
            if (ps.WasDataUsed()) {
                dp = &buffer[0];
            } else {
                errorMessage = _W("Format string had no usable format specs.");
                return false;
            }
        }
        wchar_t* np = dp;
        int nbuf_ind = 0;
        while ((*dp) && (*dp != L'%') && nbuf_ind < BUFFER_SIZE_MAX) {
            if (PrintfHelper::isEscape(dp)) {
                switch (*(dp + 1)) {
                case L'\\': {
                    *(nbuff + nbuf_ind) = L'\\';
                } break;
                case L'n': {
                    *(nbuff + nbuf_ind) = L'\n';
                } break;
                case L't': {
                    *(nbuff + nbuf_ind) = L'\t';
                } break;
                case L'r': {
                    *(nbuff + nbuf_ind) = L'\r';
                } break;
                case L'b': {
                    *(nbuff + nbuf_ind) = L'\b';
                } break;
                case L'v': {
                    *(nbuff + nbuf_ind) = L'\v';
                } break;
                case L'f': {
                    *(nbuff + nbuf_ind) = L'\f';
                } break;
                }
                dp += 2;
                ++nbuf_ind;
            } else {
                *(nbuff + nbuf_ind++) = *(dp++);
            }
        }
        *(nbuff + nbuf_ind) = 0;
        int nprn = nbuf_ind;
        result = result + nbuff;
        if (*dp == L'%' && (*(dp + 1) == L'\0' || *(dp + 1) == L'%')) {
            nprn = swprintf(nbuff, BUFFER_SIZE_MAX, L"%%");
            nbuff[std::min(nprn + 1, BUFFER_SIZE_MAX - 1)] = L'\0';
            result = result + nbuff;
            if (*(dp + 1) == L'\0') {
                return true;
            }
            dp += 2;
        }
        if (*dp == L'%' && *(dp + 1)) {
            np = PrintfHelper::validateFormatSpec(dp + 1);
            if (!np) {
                errorMessage = _W("Erroneous format specification ") + std::wstring(dp);
                return false;
            }
            wchar_t sv = 0;
            if (*(np - 1) == L'%') {
                nprn = swprintf(nbuff, BUFFER_SIZE_MAX, L"%%");
                nbuff[std::min(nprn + 1, BUFFER_SIZE_MAX - 1)] = L'\0';
                result = result + nbuff;
                sv = 0;
            } else if (*(np - 1) == L's') {
                std::wstring str;
                if (!ps.GetNextVariableAsString(str, errorMessage)) {
                    return false;
                }
                sv = *np;
                *np = 0;
                result = result + str;
            } else {
                sv = *np;
                *np = 0;
                switch (*(np - 1)) {
                case L'd':
                case L'i':
                case L'o':
                case L'u':
                case L'x':
                case L'X':
                case L'c': {
                    long long data;
                    bool isEmpty;
                    if (!ps.GetNextVariableAsLongLong(data, errorMessage, isEmpty)) {
                        return false;
                    }
                    if (!isEmpty) {
                        if (*(np - 1) == L'u') {
                            std::wstring f(dp);
                            StringHelpers::replace_all(f, L"u", L"llu");
                            nprn = swprintf(nbuff, BUFFER_SIZE_MAX, f.c_str(), data);
                        } else if (*(np - 1) == L'd') {
                            std::wstring f(dp);
                            StringHelpers::replace_all(f, L"d", L"lld");
                            nprn = swprintf(nbuff, BUFFER_SIZE_MAX, f.c_str(), data);
                        } else {
                            std::wstring fmt = std::wstring(dp);
                            nprn = swprintf(nbuff, BUFFER_SIZE_MAX, fmt.c_str(), data);
                        }
                        nbuff[nprn + 1] = L'\0';
                        result = result + nbuff;
                    }
                } break;
                case L'e':
                case L'E':
                case L'f':
                case L'F':
                case L'g':
                case L'G': {
                    bool isEmpty;
                    double data;
                    if (!ps.GetNextVariableAsDouble(data, errorMessage, isEmpty)) {
                        return false;
                    }
                    if (!isEmpty) {
                        std::wstring fmt = std::wstring(dp);
                        nprn = swprintf(nbuff, BUFFER_SIZE_MAX, fmt.c_str(), data);
                        nbuff[nprn + 1] = L'\0';
                        result = result + nbuff;
                    }
                } break;
                }
            }
            if (sv) {
                *np = sv;
            }
            dp = np;
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
