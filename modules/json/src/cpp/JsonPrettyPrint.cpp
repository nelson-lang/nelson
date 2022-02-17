//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <boost/algorithm/string.hpp>
#include <boost/container/vector.hpp>
#include <boost/regex.hpp>
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
namespace implementation {
    //=============================================================================
    enum class Position
    {
        TAB = 0,
        COMMA = 1,
        OBJ_START = 2,
        OBJ_END = 3,
        ARRAY_START = 4,
        ARRAY_END = 5
    };
    //=============================================================================
    struct RegexPos
    {
        size_t pos;
        long length;
    };
    //=============================================================================
    std::wstring
    generateSpaces(int l)
    {
        if (l > 0) {
            return std::wstring(l * 4, ' ');
        }
        return std::wstring();
    }
    //=============================================================================
    long
    lowestOf(boost::container::vector<size_t>& of)
    {
        boost::container::vector<size_t>::iterator result
            = std::min_element(std::begin(of), std::end(of));
        return (long)std::distance(std::begin(of), result);
    }
    //=============================================================================
    void
    insertColonSpaces(std::wstring& j)
    {
        boost::wregex colon = boost::wregex(LR"(\s*?\:\s*?(?=\S))");
        j.assign(boost::regex_replace(j, colon, L" : "));
    }
    //=============================================================================
    RegexPos
    findRegexFirstPosition(const std::wstring& json, const long& start_pos, const boost::wregex& rx)
    {
        size_t at = -1;
        long len = 0;
        std::wstring ss(json.begin() + start_pos, json.end());
        boost::wsmatch m;
        if (boost::regex_search(ss, m, rx)) {
            at = m.position();
            len = (long)m[0].str().size();
        }
        if (at < json.size()) {
            at += start_pos;
        }
        return { at, len };
    }
    //=============================================================================
} // namespace implementation
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
    using namespace Nelson::implementation;
    std::wstring pretty;
    bool bInQuote = false;
    pretty.reserve(json.size());
    for (wchar_t c : json) {
        if (c == L'\"') {
            bInQuote = !bInQuote;
            pretty.push_back(c);
        } else {
            if (bInQuote) {
                pretty.push_back(c);
            } else {
                if (!iswspace(c)) {
                    pretty.push_back(c);
                }
            }
        }
    }
    const boost::wregex var = boost::wregex(
        LR"(-Inf|Inf|NaN|(\".+?\"[^\,]*?((\".*?\")|(\d*?))(?=\n*?\s*?(\,|\{|\}|\[|\])))|(\d+?)|(\".*?\"))");
    long it = 0;
    int depth = 0;
    while (it < (long)pretty.size()) {
        RegexPos pos_tab = findRegexFirstPosition(pretty, it, var);
        auto pos_comma = pretty.find(L',', it);
        auto pos_obj_start = pretty.find(L'{', it);
        auto pos_obj_end = pretty.find(L'}', it);
        auto pos_array_start = pretty.find(L'[', it);
        auto pos_array_end = pretty.find(L']', it);
        long old_it = it;
        Position work_with;
        {
            boost::container::vector<size_t> _temp = { pos_tab.pos, pos_comma, pos_obj_start,
                pos_obj_end, pos_array_start, pos_array_end };
            auto at = lowestOf(_temp);
            if (_temp[at] > pretty.size()) {
                break;
            }
            work_with = static_cast<Position>(at);
        }
        switch (work_with) {
        case (Position::TAB): {
            std::wstring insert = generateSpaces(depth);
            pretty.insert(pos_tab.pos, insert);
            it = static_cast<long>(pos_tab.pos) + static_cast<long>(insert.size()) + pos_tab.length;
            break;
        }
        case (Position::COMMA): {
            pretty.insert(pos_comma + 1, L"\n");
            it = static_cast<long>(pos_comma) + 1;
            break;
        }
        case (Position::OBJ_START): {
            std::wstring insert = L"\n";
            pretty.insert(pos_obj_start + 1, insert);
            it = static_cast<long>(pos_obj_start) + static_cast<long>(insert.size());
            depth += 1;
            if (pos_obj_start - 1 > pretty.size()) {
                continue;
            }
            if (pretty.at(pos_obj_start - 1) != L':') {
                std::wstring extra = generateSpaces(depth - 1);
                pretty.insert(pos_obj_start, extra);
                it += static_cast<long>(extra.size());
            }
            break;
        }
        case (Position::OBJ_END): {
            std::wstring insert = L"\n" + generateSpaces(depth - 1);
            pretty.insert(pos_obj_end, insert);
            it = static_cast<long>(pos_obj_end) + static_cast<long>(insert.size())
                + static_cast<long>(1);
            depth -= 1;
            break;
        }
        case (Position::ARRAY_START): {
            std::wstring insert = L"\n";
            pretty.insert(pos_array_start + 1, insert);
            it = static_cast<long>(pos_array_start) + static_cast<long>(insert.size());
            depth += 1;
            break;
        }
        case (Position::ARRAY_END): {
            std::wstring insert = L"\n" + generateSpaces(depth - 1);
            pretty.insert(pos_array_end, insert);
            it = static_cast<long>(pos_array_end) + static_cast<long>(insert.size())
                + static_cast<long>(1);
            depth -= 1;
            break;
        }
        default: {
            break;
        }
        };
        if (it == old_it) {
            break;
        }
    }
    if (spacing == Colons::SPACED) {
        insertColonSpaces(pretty);
    }
    return pretty;
}
//=============================================================================
ArrayOf
jsonPrettyPrint(const std::wstring& stringToPrettify)
{
    std::wstring jsonPrettified = JSONPrettify(stringToPrettify, Colons::TIGHT);
    return ArrayOf::characterArrayConstructor(jsonPrettified);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
