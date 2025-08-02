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
#include "Exception.hpp"
#include "nlsBuildConfig.h"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
StringReplace(const ArrayOf& STR, const ArrayOf& OLD, const ArrayOf& NEW, bool doOverlaps,
    bool& needToOverload)
{
    needToOverload = false;
    wstringVector wstr;
    wstringVector wold;
    wstringVector wnew;
    try {
        wstr = STR.getContentAsWideStringVector(false);
        wold = OLD.getContentAsWideStringVector(false);
        wnew = NEW.getContentAsWideStringVector(false);
    } catch (const Exception&) {
        needToOverload = true;
        return {};
    }
    size_t nbOutput;
    Dimensions outputDims;
    if (wstr.size() == 1 && wold.size() == 1) {
        nbOutput = wnew.size();
        outputDims = NEW.getDimensions();
    } else if (wstr.size() == 1 && wnew.size() == 1) {
        nbOutput = wold.size();
        outputDims = OLD.getDimensions();
    } else if (wold.size() == 1 && wnew.size() == 1) {
        nbOutput = wstr.size();
        outputDims = STR.getDimensions();
    } else if (wstr.size() == 1) {
        if (!OLD.getDimensions().equals(NEW.getDimensions())) {
            Error(ERROR_SAME_SIZE_EXPECTED);
        }
        nbOutput = wold.size();
        outputDims = OLD.getDimensions();
    } else if (wold.size() == 1) {
        if (!STR.getDimensions().equals(NEW.getDimensions())) {
            Error(ERROR_SAME_SIZE_EXPECTED);
        }
        nbOutput = wstr.size();
        outputDims = STR.getDimensions();
    } else if (wnew.size() == 1) {
        if (!STR.getDimensions().equals(OLD.getDimensions())) {
            Error(ERROR_SAME_SIZE_EXPECTED);
        }
        nbOutput = wstr.size();
        outputDims = STR.getDimensions();
    } else {
        if ((!STR.getDimensions().equals(OLD.getDimensions()))
            || (!STR.getDimensions().equals(NEW.getDimensions()))) {
            Error(ERROR_SAME_SIZE_EXPECTED);
        }
        nbOutput = wstr.size();
        outputDims = STR.getDimensions();
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
                ArrayOf* elements = nullptr;
                if (result.empty()) {
                    res = ArrayOf(outputClass, Dimensions(0, 0), elements);
                } else {
                    try {
                        elements = new ArrayOf[nbOutput];
                    } catch (const std::bad_alloc&) {
                        Error(ERROR_MEMORY_ALLOCATION);
                    }
                    elements[0] = ArrayOf::characterArrayConstructor(result);
                    res = ArrayOf(outputClass, Dimensions(1, 1), elements);
                }
            } else {
                res = ArrayOf::characterArrayConstructor(result);
            }
        }
    } else {
        if (nbOutput == 0) {
            if (OLD.isCharacterArray() && wold.empty()) {
                res = ArrayOf(STR);
            } else {
                res = ArrayOf::characterArrayConstructor("");
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
                Error(ERROR_MEMORY_ALLOCATION);
            }
            for (size_t i = 0; i < nbOutput; i++) {
                size_t idx_str = (i)*str_incr;
                size_t idx_old = (i)*old_incr;
                size_t idx_new = (i)*new_incr;
                elements[i] = ArrayOf::characterArrayConstructor(
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
        bool replacementDone = false;
        size_t offset = 1 + replaceStr.length() - subStr.length();
        while ((start = modifiedString.find(subStr, start)) != std::string::npos) {
            modifiedString.replace(start, subStr.length(), replaceStr);
            start += offset;
            replacementDone = true;
        }
        if (replacementDone) {
            return modifiedString;
        }
    } else {
        if (subStr.empty()) {
            return originStr;
        }
        std::vector<size_t> indexSubStr, lengthSubStr;
        start = -1;
        int64 offset = 0;
        int64 offsetIncr = replaceStr.length() - subStr.length();
        while ((start = modifiedString.find(subStr, start + 1)) != std::string::npos) {
            if (indexSubStr.size() == 0) {
                indexSubStr.push_back(start);
                lengthSubStr.push_back(subStr.length());
                offset += offsetIncr;
            } else {
                size_t endLastIndex = indexSubStr.back() + replaceStr.length();
                size_t startIndex = start + (size_t)offset;
                if (startIndex < endLastIndex) {
                    lengthSubStr.back() -= endLastIndex - startIndex;
                    offset += endLastIndex - startIndex;
                }
                indexSubStr.push_back(start + (size_t)offset);
                lengthSubStr.push_back(subStr.length());
                offset += offsetIncr;
            }
        }
        if (indexSubStr.size()) {
            for (size_t i = 0; i < indexSubStr.size(); i++) {
                modifiedString.replace(indexSubStr[i], lengthSubStr[i], replaceStr);
            }
            return modifiedString;
        }
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
        wstr = STR.getContentAsWideStringVector(false);
        wold = OLD.getContentAsWideStringVector(false);
        wnew = NEW.getContentAsWideStringVector(false);
    } catch (const Exception&) {
        needToOverload = true;
        return res;
    }
    if ((wstr.size() == 1) && (OLD.isCell() || OLD.isStringArray())
        && (NEW.isCell() || NEW.isStringArray())
        && OLD.getDimensions().equals(NEW.getDimensions())) {
        ompIndexType elementCount = OLD.getElementCount();
        for (ompIndexType k = 0; k < elementCount; k++) {
            wstr[0] = Replace(wstr[0], wold[k], wnew[k]);
        }
        if (STR.isCell() || STR.isStringArray()) {
            ArrayOf* elements = nullptr;
            try {
                elements = new ArrayOf[wstr.size()];
            } catch (const std::bad_alloc&) {
                Error(ERROR_MEMORY_ALLOCATION);
            }
            elements[0] = ArrayOf::characterArrayConstructor(wstr[0]);
            if (STR.isStringArray()) {
                res = ArrayOf(NLS_STRING_ARRAY, Dimensions(1, 1), elements);
            } else {
                res = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, 1), elements);
            }
        } else {
            res = ArrayOf::characterArrayConstructor(wstr[0]);
        }
        return res;
    }
    if ((wstr.size() == 1) && (OLD.isCell() || OLD.isStringArray())
        && (NEW.isCharacterArray() || NEW.isStringArray())) {
        ompIndexType elementCount = OLD.getElementCount();
        for (ompIndexType k = 0; k < elementCount; k++) {
            wstr[0] = Replace(wstr[0], wold[k], wnew[0]);
        }
        if (STR.isCell() || STR.isStringArray()) {
            ArrayOf* elements = nullptr;
            try {
                elements = new ArrayOf[wstr.size()];
            } catch (const std::bad_alloc&) {
                Error(ERROR_MEMORY_ALLOCATION);
            }
            elements[0] = ArrayOf::characterArrayConstructor(wstr[0]);
            if (STR.isStringArray()) {
                res = ArrayOf(NLS_STRING_ARRAY, Dimensions(1, 1), elements);
            } else {
                res = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, 1), elements);
            }
        } else {
            res = ArrayOf::characterArrayConstructor(wstr[0]);
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
            return ArrayOf::stringArrayConstructor(Replace(wstr[0], wold[0], wnew[0]));
        }
        return ArrayOf::characterArrayConstructor(Replace(wstr[0], wold[0], wnew[0]));

    } else {
        size_t nbOutput;
        Dimensions outputDims;
        if (wstr.size() == 1 && wold.size() == 1) {
            nbOutput = wnew.size();
            outputDims = NEW.getDimensions();
        } else if (wstr.size() == 1 && wnew.size() == 1) {
            nbOutput = wold.size();
            outputDims = OLD.getDimensions();
        } else if (wold.size() == 1 && wnew.size() == 1) {
            nbOutput = wstr.size();
            outputDims = STR.getDimensions();
        } else if (wstr.size() == 1) {
            if (!OLD.getDimensions().equals(NEW.getDimensions())) {
                Error(ERROR_SAME_SIZE_EXPECTED);
            }
            nbOutput = wold.size();
            outputDims = OLD.getDimensions();
        } else if (wold.size() == 1) {
            if (!STR.getDimensions().equals(NEW.getDimensions())) {
                Error(ERROR_SAME_SIZE_EXPECTED);
            }
            nbOutput = wstr.size();
            outputDims = STR.getDimensions();
        } else if (wnew.size() == 1) {
            if (!STR.getDimensions().equals(OLD.getDimensions())) {
                Error(ERROR_SAME_SIZE_EXPECTED);
            }
            nbOutput = wstr.size();
            outputDims = STR.getDimensions();
        } else {
            if (!NEW.getDimensions().equals(OLD.getDimensions())) {
                Error(ERROR_SAME_SIZE_EXPECTED);
            }
            nbOutput = wstr.size();
            outputDims = STR.getDimensions();
        }
        if (OLD.isEmpty() && (OLD.isCell() || OLD.isStringArray())) {
            res = STR;
            res.ensureSingleOwner();
            return res;
        }
        if ((OLD.isCell() || OLD.isStringArray()) && (NEW.isCell() || NEW.isStringArray())) {
            Dimensions oldDims = OLD.getDimensions();
            Dimensions newDims = NEW.getDimensions();
            if (!(OLD.isScalar() || NEW.isScalar())) {
                if (!newDims.equals(oldDims)) {
                    Error(ERROR_SAME_SIZE_EXPECTED);
                }
            }
        } else {
            if ((OLD.isCharacterArray() || (OLD.isStringArray() && OLD.isScalar()))
                && (NEW.isCell() || NEW.isStringArray())) {
                if (wold.size() != wnew.size()) {
                    Error(ERROR_SAME_SIZE_EXPECTED);
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
                            Error(ERROR_MEMORY_ALLOCATION);
                        }
                        elements[0] = ArrayOf::characterArrayConstructor(result);
                        res = ArrayOf(outputClass, Dimensions(1, 1), elements);
                    }
                } else {
                    res = ArrayOf::characterArrayConstructor(result);
                }
            }
        } else {
            if (nbOutput == 0) {
                if (OLD.isCharacterArray() && wold.empty()) {
                    std::wstring result = Replace(wstr[0], L"", wnew[0]);
                    res = ArrayOf::characterArrayConstructor(result);
                } else {
                    if (outputClass == NLS_CELL_ARRAY || outputClass == NLS_STRING_ARRAY) {
                        ArrayOf* elements = nullptr;
                        try {
                            elements = new ArrayOf[nbOutput];
                        } catch (const std::bad_alloc&) {
                            Error(ERROR_MEMORY_ALLOCATION);
                        }
                        res = ArrayOf(outputClass, outputDims, elements);
                    } else {
                        res = ArrayOf::characterArrayConstructor("");
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
                    Error(ERROR_MEMORY_ALLOCATION);
                }
                if (wold.size() == wnew.size() && wnew.size() > 1) {
                    for (size_t i = 0; i < nbOutput; i++) {
                        size_t idx_str = (i)*str_incr;
                        for (size_t k = 0; k < wnew.size(); k++) {
                            wstr[idx_str] = Replace(wstr[idx_str], wold[k], wnew[k]);
                        }
                        elements[i] = ArrayOf::characterArrayConstructor(wstr[idx_str]);
                    }
                } else {
                    for (size_t i = 0; i < nbOutput; i++) {
                        size_t idx_str = (i)*str_incr;
                        size_t idx_old = (i)*old_incr;
                        size_t idx_new = (i)*new_incr;
                        elements[i] = ArrayOf::characterArrayConstructor(
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
    std::wstring result;
    if (subStr.empty()) {
        for (wchar_t c : originStr) {
            result = result + replaceStr + c;
        }
        result = result + replaceStr;
        return result;
    }
    result = originStr;
    StringHelpers::replace_all(result, subStr, replaceStr);
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
