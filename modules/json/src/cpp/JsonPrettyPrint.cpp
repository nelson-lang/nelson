//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/algorithm/string.hpp>
#include "JsonPrettyPrint.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    ArrayOf jsonPrettyPrint(std::wstring stringToPrettify)
    {
        std::wstring jsonPrettified = L"";
        if (stringToPrettify.length() != 0)
        {
            std::wstring copyInput = boost::trim_copy(stringToPrettify);
            std::wstring temp;
            bool bInQuote = false;
            for (std::wstring::const_iterator it = copyInput.begin(), end_it = copyInput.end(); it != end_it; ++it)
            {
                wchar_t c = *it;
                if (c == L'\"')
                {
                    bInQuote = !bInQuote;
                    temp.push_back(c);
                }
                else
                {
                    if (bInQuote)
                    {
                        temp.push_back(c);
                    }
                    else
                    {
                        if (!iswspace(c))
                        {
                            temp.push_back(c);
                        }
                    }
                }
            }
            swap(copyInput, temp);
            std::wstring indent = L"";
            std::wstring curr = L"";
            size_t i = 0;
            size_t len = copyInput.length();
            while (i < len)
            {
                curr.push_back(copyInput[i]);
                switch (copyInput[i])
                {
                    case L'{':
                    case L'[':
                    {
                        i++;
                        jsonPrettified = jsonPrettified + curr + L"\n";
                        if (i < len && (copyInput[i] != L'}' || copyInput[i] != L']'))
                        {
                            indent.push_back(L'\t');
                        }
                        curr = indent;
                    }
                    break;
                    case L'}':
                    case L']':
                    {
                        i++;
                        if (i < len && copyInput[i] == ',')
                        {
                            break;
                        }
                        jsonPrettified = jsonPrettified + curr + L"\n";
                        if (i < len && (copyInput[i] == L'}' || copyInput[i] == L']'))
                        {
                            if (!indent.empty())
                            {
                                indent.pop_back();
                            }
                        }
                        curr = indent;
                    }
                    break;
                    case L',':
                    {
                        jsonPrettified = jsonPrettified + curr + L"\n";
                        curr = indent;
                        i++;
                    }
                    break;
                    case L':':
                    {
                        i++;
                        if (copyInput[i] == L'{' || copyInput[i] == L'[')
                        {
                            jsonPrettified = jsonPrettified + curr + L"\n";
                            curr = indent;
                        }
                    }
                    break;
                    default:
                    {
                        i++;
                        if (i < len && (copyInput[i] == L'}' || copyInput[i] == L']'))
                        {
                            jsonPrettified = jsonPrettified + curr + L"\n";
                            if (!indent.empty())
                            {
                                indent.pop_back();
                            }
                            curr = indent;
                        }
                    }
                    break;
                }
            }
        }
        return ArrayOf::stringConstructor(jsonPrettified);
    }
    //=============================================================================
}
//=============================================================================
