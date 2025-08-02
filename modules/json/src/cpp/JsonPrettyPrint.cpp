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
    SPACED
};
//=============================================================================
std::wstring
JSONPrettify(const std::wstring& json, const Nelson::Colons spacing = Nelson::Colons::TIGHT)
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
        case L'[':
            result.push_back(c);
            result.push_back(L'\n');
            ++depth;
            result.append(depth * 4, L' ');
            break;
        case L'}':
        case L']':
            result.push_back(L'\n');
            --depth;
            result.append(depth * 4, L' ');
            result.push_back(c);
            break;
        case L',':
            result.push_back(c);
            result.push_back(L'\n');
            result.append(depth * 4, L' ');
            break;
        case L':':
            if (spacing == Nelson::Colons::SPACED) {
                result.append(L" : ");
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
    return ArrayOf::characterArrayConstructor(jsonPrettified);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
