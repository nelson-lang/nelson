//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <unordered_map>
#include <iostream>
#include <regex>
#include "GOColorHelpers.hpp"
#include "GOPropertyValues.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ParseColorToRGB(const std::wstring& colorString, bool withNone, std::vector<double>& data)
{
    std::unordered_map<std::wstring, std::vector<double>> colors;
    if (withNone) {
        colors[L"none"] = { -1, -1, -1 };
    }
    colors[L"y"] = { 1, 1, 0 };
    colors[L"yellow"] = { 1, 1, 0 };
    colors[L"m"] = { 1, 0, 1 };
    colors[L"magenta"] = { 1, 0, 1 };
    colors[L"c"] = { 0, 1, 1 };
    colors[L"cyan"] = { 0, 1, 1 };
    colors[L"r"] = { 1, 0, 0 };
    colors[L"red"] = { 1, 0, 0 };
    colors[L"g"] = { 0, 1, 0 };
    colors[L"green"] = { 0, 1, 0 };
    colors[L"b"] = { 0, 0, 1 };
    colors[L"blue"] = { 0, 0, 1 };
    colors[L"w"] = { 1, 1, 1 };
    colors[L"white"] = { 1, 1, 1 };
    colors[L"k"] = { 0, 0, 0 };
    colors[L"black"] = { 0, 0, 0 };

    bool bOK = false;

    if (StringHelpers::starts_with(colorString, L"#")) {
        std::wregex colorPattern(L"^#([0-9A-Fa-f]{3}|[0-9A-Fa-f]{6})$");
        bOK = std::regex_match(colorString, colorPattern);
        data.clear();
        if (bOK) {
            std::wstring hexDigits = colorString.substr(1);
            if (hexDigits.size() == 3) {
                hexDigits = { hexDigits[0], hexDigits[0], hexDigits[1], hexDigits[1], hexDigits[2],
                    hexDigits[2] };
            }
            std::wstringstream ss(hexDigits);
            unsigned int colorValue;
            ss >> std::hex >> colorValue;

            int r = (colorValue >> 16) & 0xFF;
            int g = (colorValue >> 8) & 0xFF;
            int b = colorValue & 0xFF;

            data.push_back(r / 255.0);
            data.push_back(g / 255.0);
            data.push_back(b / 255.0);
        }
    } else {
        bOK = colors.count(colorString) == 1;
        if (bOK) {
            data = colors[colorString];
        } else {
            data.clear();
        }
    }
    return bOK;
}
//=============================================================================
template <class T>
void
ParseUnsignedIntegerColorToRGB(
    const ArrayOf& arg, indexType nbElements, double maxValue, std::vector<double>& data)
{
    const T* ptr = static_cast<const T*>(arg.dataPointer());
    data.reserve(nbElements);
    for (indexType k = 0; k < nbElements; ++k) {
        data.push_back((double)ptr[k] / maxValue);
    }
}
//=============================================================================
template <class T>
void
ParseSignedIntegerColorToRGB(const ArrayOf& arg, indexType nbElements, double minValue,
    double maxValue, std::vector<double>& data)
{
    const T* ptr = static_cast<const T*>(arg.dataPointer());
    data.reserve(nbElements);
    for (indexType k = 0; k < nbElements; ++k) {
        data.push_back(static_cast<double>(ptr[k] + minValue) / maxValue);
    }
}
//=============================================================================
bool
ParseColorToRGB(const ArrayOf& arg, std::vector<double>& data, bool allowAlpha)
{
    if (arg.isRowVectorCharacterArray() || (arg.isStringArray() && arg.isScalar())) {
        return ParseColorToRGB(arg.asWideString(), true, data);
    } else {
        indexType nbElements = arg.elementCount();
        if (nbElements != 3 && !(allowAlpha && nbElements == 4)) {
            return false;
        }
        data.clear();
        data.reserve(nbElements);
        switch (arg.dataClass()) {
        case NLS_INT8: {
            ParseSignedIntegerColorToRGB<int8>(arg, nbElements, 128, 255.0, data);
        } break;
        case NLS_UINT8: {
            ParseUnsignedIntegerColorToRGB<uint8>(arg, nbElements, 255.0, data);
        } break;
        case NLS_INT16: {
            ParseSignedIntegerColorToRGB<int16>(arg, nbElements, 32768, 65535.0, data);
        } break;
        case NLS_UINT16: {
            ParseUnsignedIntegerColorToRGB<uint16>(arg, nbElements, 65535.0, data);
        } break;
        case NLS_INT32: {
            ParseSignedIntegerColorToRGB<int32>(arg, nbElements, 2147483648, 4294967295.0, data);
        } break;
        case NLS_UINT32: {
            ParseUnsignedIntegerColorToRGB<uint32>(arg, nbElements, 4294967295.0, data);
        } break;
        case NLS_INT64: {
            ParseSignedIntegerColorToRGB<int64>(arg, nbElements,
                static_cast<double>(std::numeric_limits<int64_t>::min()),
                static_cast<double>(std::numeric_limits<uint64_t>::max()), data);
        } break;
        case NLS_UINT64: {
            ParseUnsignedIntegerColorToRGB<uint64>(arg, nbElements, 18446744073709551615.0, data);
        } break;
        case NLS_SCOMPLEX: {
            ArrayOf sarray(arg);
            sarray.promoteType(NLS_SINGLE);
            return ParseColorToRGB(sarray, data, allowAlpha);
        } break;
        case NLS_DCOMPLEX: {
            ArrayOf darray(arg);
            darray.promoteType(NLS_DOUBLE);
            return ParseColorToRGB(darray, data, allowAlpha);
        } break;
        case NLS_SINGLE: {
            const single* sp = static_cast<const single*>(arg.dataPointer());
            for (indexType k = 0; k < nbElements; ++k) {
                if ((sp[k] < 0) || (sp[k] > 1)) {
                    return false;
                }
            }
            for (indexType k = 0; k < nbElements; ++k) {
                data.push_back((double)sp[k]);
            }
        } break;
        case NLS_DOUBLE: {
            const double* dp = static_cast<const double*>(arg.dataPointer());
            for (indexType k = 0; k < nbElements; ++k) {
                if ((dp[k] < 0) || (dp[k] > 1)) {
                    return false;
                }
            }
            for (indexType k = 0; k < nbElements; ++k) {
                data.push_back(dp[k]);
            }
        } break;
        default: {
            return false;
        } break;
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
