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
#include "HeadComments.hpp"
#include "Comments.hpp"
#include "Error.hpp"
#include "ParseFile.hpp"
#include "characters_encoding.hpp"
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <fstream>
#include <iostream>
#include <sstream>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::ifstream&
safegetline(std::ifstream& os, std::string& line)
{
    std::string myline;
    if (getline(os, myline)) {
        if (myline.size() && myline[myline.size() - 1] == '\r') {
            line = myline.substr(0, myline.size() - 1);
        } else {
            line = myline;
        }
    }
    return os;
}
//=============================================================================
static bool
isEmptyLine(std::string line)
{
    std::string str = boost::algorithm::trim_left_copy(line);
    return str == "";
}
//=============================================================================
static bool
isFunctionDefinitionLine(std::string line)
{
    if (!isCommentedLine(line)) {
        if (boost::algorithm::contains(line, "function")) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static std::string
removeCommentCharacters(std::string line)
{
    std::string res;
    stringVector comments;
    getSupportedCommentSymbols(comments);
    std::string str = boost::algorithm::trim_left_copy(line);
    for (size_t k = 0; k < comments.size(); k++) {
        if (boost::algorithm::starts_with(str, comments[k])) {
            boost::algorithm::erase_first(str, comments[k]);
            res = str;
            break;
        }
    }
    return res;
}
//=============================================================================
wstringVector
HeadComments(Evaluator* eval, std::wstring filename, HEADCOMMENTS_ERROR& headError)
{
    wstringVector comments;
    headError = HEADCOMMENTS_ERROR::MACRO_OK;
    boost::filesystem::path pathFile(filename);
    bool bIsFile
        = boost::filesystem::exists(pathFile) && !boost::filesystem::is_directory(pathFile);
    if (!bIsFile) {
        headError = HEADCOMMENTS_ERROR::FILE_NOT_EXIST;
        return comments;
    }
    ParserState parserState = ParseFile(eval, filename);
    if (parserState != ParserState::FuncDef) {
        headError = HEADCOMMENTS_ERROR::NOT_A_MACRO;
        return comments;
    }
    std::ifstream istream;
#ifdef _MSC_VER
    istream.open(filename);
#else
    istream.open(wstring_to_utf8(filename));
#endif
    if (istream.is_open()) {
        std::string line;
        while (!istream.eof()) {
            safegetline(istream, line);
            if (!isCommentedLine(line) && (!isEmptyLine(line))) {
                break;
            }
        }
        if (isFunctionDefinitionLine(line)) {
            while (!istream.eof()) {
                safegetline(istream, line);
                if (!isEmptyLine(line)) {
                    break;
                }
            }
            if (isCommentedLine(line)) {
                comments.push_back(utf8_to_wstring(removeCommentCharacters(line)));
                while (!istream.eof()) {
                    safegetline(istream, line);
                    if (isCommentedLine(line)) {
                        comments.push_back(utf8_to_wstring(removeCommentCharacters(line)));
                    } else {
                        break;
                    }
                }
            }
        }
        istream.close();
    }
    return comments;
}
//=============================================================================
}
//=============================================================================
