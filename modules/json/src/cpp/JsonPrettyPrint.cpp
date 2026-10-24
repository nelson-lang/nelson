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
#include <vector>
#include "StringHelpers.hpp"
#include "JsonPrettyPrint.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
/*
MIT License

Copyright (c) 2018 Mantas Aramavičius

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
//=============================================================================
enum class Colons
{
    TIGHT,
    SPACED,
    AFTER
};
//=============================================================================
std::wstring
JSONPrettify(const std::wstring& json, const Nelson::Colons spacing = Nelson::Colons::TIGHT,
    int indentWidth = 4, bool collapseEmpty = false)
{
    std::wstring result;
    result.reserve(json.size() * 2);
    int depth = 0;
    bool inString = false;
    bool escape = false;

    for (size_t i = 0; i < json.size(); ++i) {
        wchar_t c = json[i];

        if (escape) {
            result.push_back(c);
            escape = false;
            continue;
        }

        if (c == L'\\') {
            result.push_back(c);
            escape = true;
            continue;
        }

        if (c == L'"') {
            inString = !inString;
            result.push_back(c);
            continue;
        }

        if (inString) {
            result.push_back(c);
            continue;
        }

        switch (c) {
        case L'{':
        case L'[': {
            if (collapseEmpty) {
                // An empty object or array stays on a single line.
                wchar_t closing = (c == L'{') ? L'}' : L']';
                size_t j = i + 1;
                while (j < json.size() && iswspace(json[j])) {
                    ++j;
                }
                if (j < json.size() && json[j] == closing) {
                    result.push_back(c);
                    result.push_back(closing);
                    i = j;
                    break;
                }
            }
            result.push_back(c);
            result.push_back(L'\n');
            ++depth;
            result.append(depth * indentWidth, L' ');
        } break;
        case L'}':
        case L']':
            result.push_back(L'\n');
            --depth;
            result.append(depth * indentWidth, L' ');
            result.push_back(c);
            break;
        case L',':
            result.push_back(c);
            result.push_back(L'\n');
            result.append(depth * indentWidth, L' ');
            break;
        case L':':
            if (spacing == Nelson::Colons::SPACED) {
                result.append(L" : ");
            } else if (spacing == Nelson::Colons::AFTER) {
                result.append(L": ");
            } else {
                result.push_back(L':');
            }
            break;
        default:
            if (!iswspace(c)) {
                result.push_back(c);
            }
            break;
        }
    }
    return result;
}
//=============================================================================
ArrayOf
jsonPrettyPrint(const std::wstring& stringToPrettify)
{
    // <nlohmann/json> does not manage NaN
    std::wstring jsonPrettified = JSONPrettify(stringToPrettify, Colons::TIGHT);
    return ArrayOf::characterArray(jsonPrettified);
}
//=============================================================================
std::wstring
jsonPrettyPrintForEncode(const std::wstring& compactJson)
{
    // Two-space indentation, one space after each key colon, empty collections inline.
    return JSONPrettify(compactJson, Colons::AFTER, 2, true);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
