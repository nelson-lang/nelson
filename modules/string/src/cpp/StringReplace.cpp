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
#define _SCL_SECURE_NO_WARNINGS
#endif
#include "StringHelpers.hpp"
#include <vector>
#include "StringReplace.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
#include "nlsBuildConfig.h"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isCharacterRowVector(const ArrayOf& A);
static std::wstring
replaceAny(const std::wstring& text, const wstringVector& olds, const wstringVector& news);
//=============================================================================
ArrayOf
StringReplace(const ArrayOf& STR, const ArrayOf& OLD, const ArrayOf& NEW, bool doOverlaps,
    bool& needToOverload)
{
    // A character matrix cannot be split into one string per row here: the result
    // would be built as a character array out of an ArrayOf buffer and corrupt the
    // heap. Only a single row (or no row at all) is a valid character input.
    if (!isCharacterRowVector(STR) || !isCharacterRowVector(OLD) || !isCharacterRowVector(NEW)) {
        Error(L"Nelson:string:charRowVectorExpected", L"Char inputs must be row vectors.");
    }
    needToOverload = false;
    wstringVector wstr;
    wstringVector wold;
    wstringVector wnew;
    try {
        wstr = STR.asWideStringVector(false);
        wold = OLD.asWideStringVector(false);
        wnew = NEW.asWideStringVector(false);
    } catch (const NelsonException&) {
        needToOverload = true;
        return {};
    }
    // An empty character array is the empty string, not an empty list of values:
    // strrep(char(zeros(0, 3)), 'a', 'bb') keeps the 0x3 shape of its input, and an
    // empty OLD or NEW behaves as '' (replace nothing, or replace by nothing).
    if (STR.isCharacterArray() && STR.isEmpty()) {
        return ArrayOf(STR);
    }
    if (OLD.isCharacterArray() && wold.empty()) {
        wold.emplace_back();
    }
    if (NEW.isCharacterArray() && wnew.empty()) {
        wnew.emplace_back();
    }
    size_t nbOutput;
    Dimensions outputDims;
    if (wstr.size() == 1 && wold.size() == 1) {
        nbOutput = wnew.size();
        outputDims = NEW.dimensions();
    } else if (wstr.size() == 1 && wnew.size() == 1) {
        nbOutput = wold.size();
        outputDims = OLD.dimensions();
    } else if (wold.size() == 1 && wnew.size() == 1) {
        nbOutput = wstr.size();
        outputDims = STR.dimensions();
    } else if (wstr.size() == 1) {
        if (!OLD.dimensions().equals(NEW.dimensions())) {
            Error(L"Nelson:string:sameSizeExpected", L"Same size expected.");
        }
        nbOutput = wold.size();
        outputDims = OLD.dimensions();
    } else if (wold.size() == 1) {
        if (!STR.dimensions().equals(NEW.dimensions())) {
            Error(L"Nelson:string:sameSizeExpected", L"Same size expected.");
        }
        nbOutput = wstr.size();
        outputDims = STR.dimensions();
    } else if (wnew.size() == 1) {
        if (!STR.dimensions().equals(OLD.dimensions())) {
            Error(L"Nelson:string:sameSizeExpected", L"Same size expected.");
        }
        nbOutput = wstr.size();
        outputDims = STR.dimensions();
    } else {
        if ((!STR.dimensions().equals(OLD.dimensions()))
            || (!STR.dimensions().equals(NEW.dimensions()))) {
            Error(L"Nelson:string:sameSizeExpected", L"Same size expected.");
        }
        nbOutput = wstr.size();
        outputDims = STR.dimensions();
    }
    NelsonType outputClass = NLS_CHAR;
    if (STR.isCell() || OLD.isCell() || NEW.isCell()) {
        outputClass = NLS_CELL_ARRAY;
    }
    if (STR.isStringArray()) {
        outputClass = NLS_STRING_ARRAY;
    }
    ArrayOf res;
    if (nbOutput == 1) {
        if (OLD.isCell() && OLD.isEmpty()) {
            ArrayOf* elements = nullptr;
            res = ArrayOf(outputClass, Dimensions(0, 0), elements);
        } else {
            std::wstring result = stringReplace(wstr[0], wold[0], wnew[0], doOverlaps);
            if (outputClass == NLS_CELL_ARRAY || outputClass == NLS_STRING_ARRAY) {
                // An empty result string still fills one element: strrep({'abc'},
                // 'abc', '') is a 1x1 cell holding '', not an empty cell.
                ArrayOf* elements = nullptr;
                try {
                    elements = new ArrayOf[nbOutput];
                } catch (const std::bad_alloc&) {
                    Error(L"Nelson:outOfMemory",
                        L"Memory allocation error... You may have run out of memory!");
                }
                elements[0] = ArrayOf::characterArray(result);
                res = ArrayOf(outputClass, Dimensions(1, 1), elements);
            } else {
                res = ArrayOf::characterArray(result);
            }
        }
    } else {
        if (nbOutput == 0) {
            if (OLD.isCharacterArray() && wold.empty()) {
                res = ArrayOf(STR);
            } else {
                res = ArrayOf::characterArray("");
                res.promoteType(outputClass);
            }
        } else {
            int str_incr = (wstr.size() == 1 ? 0 : 1);
            int old_incr = (wold.size() == 1 ? 0 : 1);
            int new_incr = (wnew.size() == 1 ? 0 : 1);
            ArrayOf* elements = nullptr;
            try {
                elements = new ArrayOf[nbOutput];
            } catch (const std::bad_alloc&) {
                Error(L"Nelson:outOfMemory",
                    L"Memory allocation error... You may have run out of memory!");
            }
            for (size_t i = 0; i < nbOutput; i++) {
                size_t idx_str = (i)*str_incr;
                size_t idx_old = (i)*old_incr;
                size_t idx_new = (i)*new_incr;
                elements[i] = ArrayOf::characterArray(
                    stringReplace(wstr[idx_str], wold[idx_old], wnew[idx_new], doOverlaps));
            }
            res = ArrayOf(outputClass, outputDims, elements);
        }
    }
    return res;
}
//=============================================================================
std::wstring
stringReplace(const std::wstring& originStr, const std::wstring& subStr,
    const std::wstring& replaceStr, bool doOverlaps)
{
    std::wstring modifiedString = originStr;
    size_t start = 0;
    if (!doOverlaps) {
        if (subStr.empty()) {
            return originStr;
        }
        bool replacementDone = false;
        while ((start = modifiedString.find(subStr, start)) != std::wstring::npos) {
            modifiedString.replace(start, subStr.length(), replaceStr);
            // Resume just past the text written: start + replaceStr.length() is the character
            // after the original match. No size_t subtraction, so no underflow when subStr is
            // longer than replaceStr.
            start += replaceStr.length();
            replacementDone = true;
        }
        if (replacementDone) {
            return modifiedString;
        }
    } else {
        if (subStr.empty()) {
            return originStr;
        }
        std::vector<size_t> positions;
        size_t pos = 0;
        while ((pos = originStr.find(subStr, pos)) != std::wstring::npos) {
            positions.push_back(pos);
            pos += 1;
        }
        if (positions.empty()) {
            return originStr;
        }
        // Emit the replacement at each match start and copy only the characters not covered
        // by a match. This gives the overlapping-replace semantics without signed/unsigned
        // wrap-around.
        std::vector<bool> isStart(originStr.size(), false);
        std::vector<bool> covered(originStr.size(), false);
        for (size_t p : positions) {
            isStart[p] = true;
            for (size_t k = p; k < p + subStr.length() && k < originStr.size(); ++k) {
                covered[k] = true;
            }
        }
        std::wstring result;
        result.reserve(originStr.size());
        for (size_t i = 0; i < originStr.size(); ++i) {
            if (isStart[i]) {
                result += replaceStr;
            }
            if (!covered[i]) {
                result += originStr[i];
            }
        }
        return result;
    }
    return originStr;
}
//=============================================================================
ArrayOf
Replace(const ArrayOf& STR, const ArrayOf& OLD, const ArrayOf& NEW, bool& needToOverload)
{
    ArrayOf res;
    wstringVector wstr;
    wstringVector wold;
    wstringVector wnew;
    needToOverload = false;
    try {
        wstr = STR.asWideStringVector(false);
        wold = OLD.asWideStringVector(false);
        wnew = NEW.asWideStringVector(false);
    } catch (const NelsonException&) {
        needToOverload = true;
        return res;
    }
    if ((wstr.size() == 1) && (OLD.isCell() || OLD.isStringArray())
        && (NEW.isCell() || NEW.isStringArray())
        && (OLD.dimensions().equals(NEW.dimensions()) || NEW.elementCount() == 1)) {
        wstr[0] = replaceAny(wstr[0], wold, wnew);
        if (STR.isCell() || STR.isStringArray()) {
            ArrayOf* elements = nullptr;
            try {
                elements = new ArrayOf[wstr.size()];
            } catch (const std::bad_alloc&) {
                Error(L"Nelson:outOfMemory",
                    L"Memory allocation error... You may have run out of memory!");
            }
            elements[0] = ArrayOf::characterArray(wstr[0]);
            if (STR.isStringArray()) {
                res = ArrayOf(NLS_STRING_ARRAY, Dimensions(1, 1), elements);
            } else {
                res = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, 1), elements);
            }
        } else {
            res = ArrayOf::characterArray(wstr[0]);
        }
        return res;
    }
    if ((wstr.size() == 1) && (OLD.isCell() || OLD.isStringArray())
        && (NEW.isCharacterArray() || NEW.isStringArray())) {
        wstr[0] = replaceAny(wstr[0], wold, wstringVector { wnew[0] });
        if (STR.isCell() || STR.isStringArray()) {
            ArrayOf* elements = nullptr;
            try {
                elements = new ArrayOf[wstr.size()];
            } catch (const std::bad_alloc&) {
                Error(L"Nelson:outOfMemory",
                    L"Memory allocation error... You may have run out of memory!");
            }
            elements[0] = ArrayOf::characterArray(wstr[0]);
            if (STR.isStringArray()) {
                res = ArrayOf(NLS_STRING_ARRAY, Dimensions(1, 1), elements);
            } else {
                res = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, 1), elements);
            }
        } else {
            res = ArrayOf::characterArray(wstr[0]);
        }
        return res;
    }
    if ((STR.isCharacterArray() || (STR.isStringArray() && STR.isScalar()))
        && (OLD.isCharacterArray() || (OLD.isStringArray() && OLD.isScalar()))
        && (NEW.isCharacterArray() || (NEW.isStringArray() && NEW.isScalar()))) {
        if (wstr.size() == 0) {
            wstr.push_back(L"");
        }
        if (wold.size() == 0) {
            wold.push_back(L"");
        }
        if (wnew.size() == 0) {
            wnew.push_back(L"");
        }
        if (STR.isStringArray()) {
            return ArrayOf::stringArray(Replace(wstr[0], wold[0], wnew[0]));
        }
        return ArrayOf::characterArray(Replace(wstr[0], wold[0], wnew[0]));

    } else {
        size_t nbOutput;
        Dimensions outputDims;
        if (wstr.size() == 1 && wold.size() == 1) {
            nbOutput = wnew.size();
            outputDims = NEW.dimensions();
        } else if (wstr.size() == 1 && wnew.size() == 1) {
            nbOutput = wold.size();
            outputDims = OLD.dimensions();
        } else if (wold.size() == 1 && wnew.size() == 1) {
            nbOutput = wstr.size();
            outputDims = STR.dimensions();
        } else if (wstr.size() == 1) {
            if (!OLD.dimensions().equals(NEW.dimensions())) {
                Error(L"Nelson:string:sameSizeExpected", L"Same size expected.");
            }
            nbOutput = wold.size();
            outputDims = OLD.dimensions();
        } else if (wold.size() == 1) {
            if (!STR.dimensions().equals(NEW.dimensions())) {
                Error(L"Nelson:string:sameSizeExpected", L"Same size expected.");
            }
            nbOutput = wstr.size();
            outputDims = STR.dimensions();
        } else if (wnew.size() == 1) {
            if (!STR.dimensions().equals(OLD.dimensions())) {
                Error(L"Nelson:string:sameSizeExpected", L"Same size expected.");
            }
            nbOutput = wstr.size();
            outputDims = STR.dimensions();
        } else {
            if (!NEW.dimensions().equals(OLD.dimensions())) {
                Error(L"Nelson:string:sameSizeExpected", L"Same size expected.");
            }
            nbOutput = wstr.size();
            outputDims = STR.dimensions();
        }
        if (OLD.isEmpty() && (OLD.isCell() || OLD.isStringArray())) {
            res = STR;
            res.ensureSingleOwner();
            return res;
        }
        if ((OLD.isCell() || OLD.isStringArray()) && (NEW.isCell() || NEW.isStringArray())) {
            Dimensions oldDims = OLD.dimensions();
            Dimensions newDims = NEW.dimensions();
            if (!(OLD.isScalar() || NEW.isScalar())) {
                if (!newDims.equals(oldDims)) {
                    Error(L"Nelson:string:sameSizeExpected", L"Same size expected.");
                }
            }
        } else {
            if ((OLD.isCharacterArray() || (OLD.isStringArray() && OLD.isScalar()))
                && (NEW.isCell() || NEW.isStringArray())) {
                if (wold.size() != wnew.size()) {
                    Error(L"Nelson:string:sameSizeExpected", L"Same size expected.");
                }
            }
        }
        NelsonType outputClass = NLS_CHAR;
        if ((STR.isCharacterArray() || (STR.isStringArray() && STR.isScalar()))
            && ((OLD.isCell() || OLD.isStringArray()) && !OLD.isScalar())) {
            if (STR.isStringArray()) {
                outputClass = NLS_STRING_ARRAY;
            } else {
                outputClass = NLS_CELL_ARRAY;
            }
        }
        if (STR.isCell()) {
            outputClass = NLS_CELL_ARRAY;
        }
        if (STR.isStringArray()) {
            outputClass = NLS_STRING_ARRAY;
        }
        if (nbOutput == 1) {
            if ((OLD.isCell() || OLD.isStringArray()) && OLD.isEmpty()) {
                ArrayOf* elements = nullptr;
                res = ArrayOf(outputClass, Dimensions(0, 0), elements);
            } else {
                std::wstring result = Replace(wstr[0], wold[0], wnew[0]);
                if (outputClass == NLS_CELL_ARRAY || outputClass == NLS_STRING_ARRAY) {
                    ArrayOf* elements = nullptr;
                    if (result.empty()) {
                        res = ArrayOf(outputClass, Dimensions(0, 0), elements);
                    } else {
                        try {
                            elements = new ArrayOf[nbOutput];
                        } catch (const std::bad_alloc&) {
                            Error(L"Nelson:outOfMemory",
                                L"Memory allocation error... You may have run out of memory!");
                        }
                        elements[0] = ArrayOf::characterArray(result);
                        res = ArrayOf(outputClass, Dimensions(1, 1), elements);
                    }
                } else {
                    res = ArrayOf::characterArray(result);
                }
            }
        } else {
            if (nbOutput == 0) {
                if (OLD.isCharacterArray() && wold.empty()) {
                    std::wstring result = Replace(wstr[0], L"", wnew[0]);
                    res = ArrayOf::characterArray(result);
                } else {
                    if (outputClass == NLS_CELL_ARRAY || outputClass == NLS_STRING_ARRAY) {
                        ArrayOf* elements = nullptr;
                        try {
                            elements = new ArrayOf[nbOutput];
                        } catch (const std::bad_alloc&) {
                            Error(L"Nelson:outOfMemory",
                                L"Memory allocation error... You may have run out of memory!");
                        }
                        res = ArrayOf(outputClass, outputDims, elements);
                    } else {
                        res = ArrayOf::characterArray("");
                    }
                }
            } else {
                int str_incr = (wstr.size() == 1 ? 0 : 1);
                int old_incr = (wold.size() == 1 ? 0 : 1);
                int new_incr = (wnew.size() == 1 ? 0 : 1);
                ArrayOf* elements = nullptr;
                try {
                    elements = new ArrayOf[nbOutput];
                } catch (const std::bad_alloc&) {
                    Error(L"Nelson:outOfMemory",
                        L"Memory allocation error... You may have run out of memory!");
                }
                if (wold.size() == wnew.size() && wnew.size() > 1) {
                    for (size_t i = 0; i < nbOutput; i++) {
                        size_t idx_str = (i)*str_incr;
                        elements[i]
                            = ArrayOf::characterArray(replaceAny(wstr[idx_str], wold, wnew));
                    }
                } else if (wold.size() > 1 && wnew.size() == 1 && wold.size() == wstr.size()
                    && (OLD.isCell() || OLD.isStringArray())) {
                    // one replacement for every pattern
                    for (size_t i = 0; i < nbOutput; i++) {
                        elements[i] = ArrayOf::characterArray(replaceAny(wstr[i], wold, wnew));
                    }
                } else {
                    for (size_t i = 0; i < nbOutput; i++) {
                        size_t idx_str = (i)*str_incr;
                        size_t idx_old = (i)*old_incr;
                        size_t idx_new = (i)*new_incr;
                        elements[i] = ArrayOf::characterArray(
                            Replace(wstr[idx_str], wold[idx_old], wnew[idx_new]));
                    }
                }
                res = ArrayOf(outputClass, outputDims, elements);
            }
        }
    }
    return res;
}
//=============================================================================
std::wstring
Replace(const std::wstring& originStr, const std::wstring& subStr, const std::wstring& replaceStr)
{
    std::wstring result = originStr;
    StringHelpers::replace_all(result, subStr, replaceStr);
    return result;
}
//=============================================================================
bool
isCharacterRowVector(const ArrayOf& A)
{
    if (!A.isCharacterArray()) {
        return true;
    }
    Dimensions dims(A.dimensions());
    return dims.is2D() && (dims.at(0) <= 1);
}
//=============================================================================
// Every pattern is searched at once, left to right without overlap: at a
// given position the first pattern of the list that matches is replaced by
// its replacement (a single replacement serves every pattern). An empty
// pattern matches everywhere, but not right after a replaced text.
static std::wstring
replaceAny(const std::wstring& text, const wstringVector& olds, const wstringVector& news)
{
    bool hasEmptyPattern = false;
    for (const auto& old : olds) {
        hasEmptyPattern = hasEmptyPattern || old.empty();
    }
    std::wstring out;
    size_t pos = 0;
    if (!hasEmptyPattern) {
        while (pos < text.size()) {
            size_t best = std::wstring::npos;
            size_t bestIndex = 0;
            for (size_t k = 0; k < olds.size(); ++k) {
                size_t found = text.find(olds[k], pos);
                if (found < best) {
                    best = found;
                    bestIndex = k;
                }
            }
            if (best == std::wstring::npos) {
                break;
            }
            out.append(text, pos, best - pos);
            out.append(news.size() == 1 ? news[0] : news[bestIndex]);
            pos = best + olds[bestIndex].size();
        }
        out.append(text, pos, std::wstring::npos);
        return out;
    }
    size_t previousEnd = std::wstring::npos;
    for (;;) {
        size_t match = olds.size();
        for (size_t k = 0; k < olds.size() && match == olds.size(); ++k) {
            if (olds[k].empty() || text.compare(pos, olds[k].size(), olds[k]) == 0) {
                match = k;
            }
        }
        if (match < olds.size() && !olds[match].empty()) {
            out.append(news.size() == 1 ? news[0] : news[match]);
            pos += olds[match].size();
            previousEnd = pos;
            continue;
        }
        if (match < olds.size() && pos != previousEnd) {
            out.append(news.size() == 1 ? news[0] : news[match]);
        }
        if (pos >= text.size()) {
            break;
        }
        out.push_back(text[pos++]);
    }
    return out;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
