//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringTextMatch.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "StringHelpers.hpp"
#include "ToLower.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <typename Match>
static ArrayOf
textMatch(const ArrayOf& A, const ArrayOf& Pattern, Match match);
static void
checkMatchesArgument(const ArrayOf& value, int position);
static bool
isMissingStringElement(const ArrayOf& element);
//=============================================================================
ArrayOf
StringContains(const ArrayOf& A, const ArrayOf& Pattern, bool bCaseSensitive)
{
    // an empty pattern is contained in any text; case folding is Unicode
    return textMatch(A, Pattern, [bCaseSensitive](const std::wstring& s, const std::wstring& p) {
        return p.empty()
            || (bCaseSensitive ? StringHelpers::contains(s, p)
                               : StringHelpers::contains(ToLower(s), ToLower(p)));
    });
}
//=============================================================================
ArrayOf
StringStartsWith(const ArrayOf& A, const ArrayOf& Pattern, bool bCaseSensitive)
{
    return textMatch(A, Pattern, [bCaseSensitive](const std::wstring& s, const std::wstring& p) {
        return bCaseSensitive ? StringHelpers::starts_with(s, p)
                              : StringHelpers::starts_with(ToLower(s), ToLower(p));
    });
}
//=============================================================================
ArrayOf
StringEndsWith(const ArrayOf& A, const ArrayOf& Pattern, bool bCaseSensitive)
{
    return textMatch(A, Pattern, [bCaseSensitive](const std::wstring& s, const std::wstring& p) {
        return bCaseSensitive ? StringHelpers::ends_with(s, p)
                              : StringHelpers::ends_with(ToLower(s), ToLower(p));
    });
}
//=============================================================================
ArrayOf
StringMatches(ArrayOf A, ArrayOf B, bool ignoreCase)
{
    checkMatchesArgument(A, 1);
    checkMatchesArgument(B, 2);
    if (B.isStringArray() && B.isScalar()
        && isMissingStringElement(((ArrayOf*)B.dataPointer())[0])) {
        // a missing pattern matches nothing
        B = ArrayOf::emptyCell(Dimensions(0, 0));
    }
    return textMatch(A, B, [ignoreCase](const std::wstring& s, const std::wstring& p) {
        return ignoreCase ? StringHelpers::iequals(s, p) : (s == p);
    });
}
//=============================================================================
static void
checkMatchesArgument(const ArrayOf& value, int position)
{
    if (!value.isRowVectorCharacterArray() && !value.isStringArray()
        && !value.isCellArrayOfCharacterVectors()) {
        Error(L"Nelson:string:invalidArgumentType", L"Wrong type for argument #%d: %s expected.",
            position, L"string array or character vector or cell array of character vectors");
    }
}
//=============================================================================
static bool
isMissingStringElement(const ArrayOf& element)
{
    return !element.isCharacterArray();
}
//=============================================================================
static bool
isTextScalar(const ArrayOf& A)
{
    return A.isCharacterArray() || (A.isStringArray() && A.isScalar());
}
//=============================================================================
static bool
isTextArray(const ArrayOf& A)
{
    return A.isStringArray() || A.isCellArrayOfCharacterVectors();
}
//=============================================================================
template <typename Match>
static bool
matchAny(const std::wstring& text, const wstringVector& patterns, Match match)
{
    for (const std::wstring& pattern : patterns) {
        if (match(text, pattern)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
template <typename Match>
static ArrayOf
textMatch(const ArrayOf& A, const ArrayOf& Pattern, Match match)
{
    if (A.isStringArray() && A.isScalar()
        && isMissingStringElement(((ArrayOf*)A.dataPointer())[0])) {
        return ArrayOf::logicalScalar(false);
    }
    bool isSupportedA = isTextScalar(A) || isTextArray(A);
    bool isSupportedPattern = isTextScalar(Pattern) || isTextArray(Pattern);
    if (!isSupportedA || !isSupportedPattern) {
        Error(L"Nelson:string:charVectorCellStringsExpected",
            L"character vector or cell of strings expected.");
    }
    wstringVector patterns;
    if (isTextScalar(Pattern)) {
        patterns.push_back(Pattern.asWideString());
    } else {
        auto* cellPattern = (ArrayOf*)(Pattern.dataPointer());
        indexType nbPattern = Pattern.dimensions().elementCount();
        patterns.reserve(nbPattern);
        for (indexType k = 0; k < nbPattern; k++) {
            if (!isMissingStringElement(cellPattern[k])) {
                patterns.push_back(cellPattern[k].asWideString());
            }
        }
    }
    if (isTextScalar(A)) {
        return ArrayOf::logicalScalar(matchAny(A.asWideString(), patterns, match));
    }
    Dimensions dimA = A.dimensions();
    indexType nbA = dimA.elementCount();
    logical* result
        = static_cast<logical*>(ArrayOf::allocateArrayOf(NLS_LOGICAL, nbA, stringVector(), true));
    auto* cellA = (ArrayOf*)(A.dataPointer());
    for (indexType k = 0; k < nbA; k++) {
        if (!isMissingStringElement(cellA[k])) {
            result[k] = static_cast<logical>(matchAny(cellA[k].asWideString(), patterns, match));
        }
    }
    return ArrayOf(NLS_LOGICAL, dimA, result);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
