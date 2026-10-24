//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#if defined(_WIN32) && !defined(NOMINMAX)
#define NOMINMAX
#endif
#if defined(_WIN32)
#include <Windows.h>
#endif
#include "ToLower.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include <algorithm>
#include "StringHelpers.hpp"
#include <cctype>
#include <cwctype>
#include <string>
#if !defined(_WIN32)
#include <clocale>
#if defined(__APPLE__) || defined(__FreeBSD__)
#include <xlocale.h>
#endif
#endif
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#if !defined(_WIN32)
namespace {
    // Locale-invariant simple Unicode case mapping, independent of the process
    // locale. On POSIX platforms std::towlower follows the global LC_CTYPE,
    // which on macOS stays "C" (ASCII-only) unless a UTF-8 locale is applied;
    // a dedicated locale_t mirrors the Windows LOCALE_INVARIANT behaviour.
    static locale_t
    caseMappingLocale()
    {
        static locale_t loc = []() -> locale_t {
            const char* candidates[] = { "en_US.UTF-8", "C.UTF-8", "UTF-8" };
            for (const char* name : candidates) {
                locale_t l = newlocale(LC_CTYPE_MASK, name, (locale_t)0);
                if (l != (locale_t)0) {
                    return l;
                }
            }
            return (locale_t)0;
        }();
        return loc;
    }
    //=============================================================================
} // namespace
#endif
//=============================================================================
ArrayOf
ToLower(const ArrayOf& A, bool& needToOverload)
{
    needToOverload = false;
    ArrayOf res;
    if (A.dataClass() == NLS_CHAR) {
        if (A.isEmpty()) {
            return ArrayOf(A);
        }
        res = ArrayOf(A);
        res.ensureSingleOwner();
        auto* ptr = (charType*)(res.dataPointer());
        indexType len = A.elementCount();
        std::wstring str(ptr, ptr + len);
        str = ToLower(str);
        for (indexType k = 0; k < len; ++k) {
            ptr[k] = str[k];
        }
        return res;
    }
    if (A.dataClass() == NLS_CELL_ARRAY) {
        if (A.isEmpty()) {
            return ArrayOf(A);
        }
        res = ArrayOf(A);
        res.ensureSingleOwner();
        auto* element = (ArrayOf*)(res.dataPointer());
        indexType elementCount = A.elementCount();
        for (indexType k = 0; k < elementCount; k++) {
            if (!element[k].isRowVectorCharacterArray()) {
                Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                    L"cell of strings");
            }
            element[k] = ArrayOf::characterArray(ToLower(element[k].asWideString()));
        }
        return res;
    }
    if (A.dataClass() == NLS_STRING_ARRAY) {
        if (A.isEmpty()) {
            return ArrayOf(A);
        }
        res = ArrayOf(A);
        res.ensureSingleOwner();
        auto* element = (ArrayOf*)(res.dataPointer());
        ompIndexType elementCount = A.elementCount();
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            if (element[k].isRowVectorCharacterArray()) {
                element[k] = ArrayOf::characterArray(ToLower(element[k].asWideString()));
            } else {
                element[k] = ArrayOf::empty();
            }
        }
        return res;
    }
    needToOverload = true;

    return res;
}
//=============================================================================
std::wstring
ToLower(const std::wstring& A)
{
    if (A.empty()) {
        return A;
    }
#if defined(_WIN32)
    std::wstring result(A.size(), L'\0');
    int converted = ::LCMapStringW(LOCALE_INVARIANT, LCMAP_LOWERCASE, A.c_str(),
        static_cast<int>(A.size()), &result[0], static_cast<int>(result.size()));
    if (converted <= 0) {
        return StringHelpers::to_lower_copy(A);
    }
    result.resize(static_cast<size_t>(converted));
    return result;
#else
    locale_t loc = caseMappingLocale();
    std::wstring result(A);
    std::transform(result.begin(), result.end(), result.begin(), [loc](wchar_t c) -> wchar_t {
        auto wc = static_cast<std::wint_t>(c);
        return static_cast<wchar_t>(loc != (locale_t)0 ? towlower_l(wc, loc) : std::towlower(wc));
    });
    return result;
#endif
}
//=============================================================================
} // namespace Nelson
//=============================================================================
