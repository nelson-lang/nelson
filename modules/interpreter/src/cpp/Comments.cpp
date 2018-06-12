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
#include "Comments.hpp"
#include <boost/algorithm/string.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
void
getSupportedCommentSymbols(stringVector& comments)
{
    comments.clear();
    comments.push_back("//");
    comments.push_back("%");
    comments.push_back("#");
}
//=============================================================================
void
getSupportedCommentSymbols(wstringVector& comments)
{
    comments.clear();
    comments.push_back(L"//");
    comments.push_back(L"%");
    comments.push_back(L"#");
}
//=============================================================================
bool
isCommentedLine(std::string line)
{
    stringVector comments;
    getSupportedCommentSymbols(comments);
    std::string str = boost::algorithm::trim_left_copy(line);
    for (size_t k = 0; k < comments.size(); k++) {
        if (boost::algorithm::starts_with(str, comments[k])) {
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
isCommentedLine(std::wstring line)
{
    wstringVector comments;
    getSupportedCommentSymbols(comments);
    std::wstring str = boost::algorithm::trim_left_copy(line);
    for (size_t k = 0; k < comments.size(); k++) {
        if (boost::algorithm::starts_with(str, comments[k])) {
            return true;
        }
    }
    return false;
}
//=============================================================================
}
//=============================================================================
