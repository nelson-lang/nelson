//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cwchar>
#include "StringHelpers.hpp"
#include <unordered_map>
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
#include "i18n.hpp"
#include "Exception.hpp"
#include "BaseToDecimal.hpp"
#include "characters_encoding.hpp"
#include "Transpose.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::unordered_map<std::wstring, double> baseConverted;
static size_t lastBase = 10;
//=============================================================================
static uint64
base2dec(const std::wstring& wstr, int base)
{
    uint64 res = 0;
    if (wstr.empty()) {
        return res;
    }
    wchar_t* str = nullptr;
    res = wcstoull(wstr.c_str(), &str, base);
    if (wcslen(str) > 0) {
        raiseError(L"Nelson:elementary_functions:ERROR_CANNOT_CONVERT_TO_STR_WITH_VALUE",
            ERROR_CANNOT_CONVERT_TO_STR_WITH_VALUE, wstr);
    }
    return res;
}
//=============================================================================
static double
BaseToDecimal(const std::wstring& element, int base)
{
    double res;
    if (element.empty()) {
        res = 0;
    } else {
        std::wstring upperCase = StringHelpers::erase_all_copy(element, L" ");
        StringHelpers::to_upper(upperCase);
        res = (double)base2dec(upperCase, base);
    }
    return res;
}
//=============================================================================
ArrayOf
BaseToDecimal(ArrayOf& A, ArrayOf& Base, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    double dbase = Base.getContentAsDoubleScalar();
    double fbase = std::floor(dbase);
    if (dbase != fbase) {
        raiseError(L"Nelson:elementary_functions:ERROR_BASE_MUST_BE_INTEGER_BETWEEN_2_AND_36",
            ERROR_BASE_MUST_BE_INTEGER_BETWEEN_2_AND_36);
    }
    int ibase = static_cast<int>(fbase);
    if (ibase < 2 || ibase > 36) {
        raiseError(L"Nelson:elementary_functions:ERROR_BASE_MUST_BE_INTEGER_BETWEEN_2_AND_36",
            ERROR_BASE_MUST_BE_INTEGER_BETWEEN_2_AND_36);
    }
    if (lastBase != ibase) {
        baseConverted.clear();
        lastBase = ibase;
    }
    Dimensions dimsA = A.getDimensions();
    if (dimsA.isEmpty(false)) {
        baseConverted.clear();
        res = ArrayOf::emptyConstructor();
    } else {
        wstringVector elements;
        switch (A.getDataClass()) {
        case NLS_CHAR: {
            if (A.isRowVector()) {
                elements.push_back(A.getContentAsWideString());
            } else {
                if (A.isNdArrayCharacterType()) {
                    raiseError(L"Nelson:elementary_functions:ERROR_2D_CHAR_ARRAY_EXPECTED",
                        ERROR_2D_CHAR_ARRAY_EXPECTED);
                }
                bool dummy;
                ArrayOf Transposed = Transpose(A, dummy);
                std::wstring wstr = Transposed.getContentAsArrayOfCharacters();
                size_t commonLength = dimsA.getColumns();
                elements.reserve(dimsA.getElementCount() / commonLength);
                indexType elementCount = dimsA.getElementCount();
                for (indexType k = 0; k < elementCount; k = k + commonLength) {
                    std::wstring s = wstr.substr(k, commonLength);
                    elements.push_back(s);
                }
            }
        } break;
        case NLS_CELL_ARRAY: {
            elements = A.getContentAsWideStringVector(false);
        } break;
        case NLS_STRING_ARRAY: {
            elements = A.getContentAsWideStringVector(false);
        } break;
        default: {
            needToOverload = true;
            return {};
        } break;
        }
        Dimensions dims(elements.size(), 1);
        double* ptr
            = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, elements.size(), stringVector(), false);
        res = ArrayOf(NLS_DOUBLE, dims, ptr);
        size_t k = 0;
        for (const std::wstring& e : elements) {
            if (baseConverted.count(e) != 0) {
                ptr[k] = baseConverted[e];
            } else {
                if (baseConverted.size() > baseConverted.max_size() - 1000) {
                    baseConverted.clear();
                }
                ptr[k] = BaseToDecimal(e, ibase);
                baseConverted[e] = ptr[k];
            }
            k++;
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
