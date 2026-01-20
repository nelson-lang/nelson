//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringHelpers.hpp"
#include <fstream>
#include <iostream>
#include <sstream>
#include "HeadComments.hpp"
#include "Comments.hpp"
#include "Error.hpp"
#include "ParseFile.hpp"
#include "characters_encoding.hpp"
#include "FileSystemWrapper.hpp"
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
isEmptyLine(const std::string& line)
{
    std::string str = StringHelpers::trim_left_copy(line);
    return str.empty();
}
//=============================================================================
static bool
isFunctionDefinitionLine(const std::string& line)
{
    if (!isCommentedLine(line)) {
        if (StringHelpers::contains(line, "function")) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static std::string
removeCommentCharacters(const std::string& line)
{
    std::string res;
    const std::string commentSymbol = getCommentSymbol();
    std::string str = StringHelpers::trim_left_copy(line);
    if (StringHelpers::starts_with(str, commentSymbol)) {
        StringHelpers::erase_first(str, commentSymbol);
        res = str;
    }
    return res;
}
//=============================================================================
wstringVector
HeadComments(Evaluator* eval, const std::wstring& filename, HEADCOMMENTS_ERROR& headError)
{
    wstringVector comments;
    headError = HEADCOMMENTS_ERROR::MACRO_OK;
    FileSystemWrapper::Path pathFile(filename);
    bool bIsFile = FileSystemWrapper::Path::is_regular_file(pathFile);
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

            std::string trimmed = StringHelpers::trim_left_copy(line);

            // Prefer multiline %{ ... %} blocks if present
            if (StringHelpers::starts_with(trimmed, "%{")) {
                // Content after opening on same line
                size_t openPos = trimmed.find("%{");
                std::string afterOpen = trimmed.substr(openPos + 2);
                // If closing on same line
                size_t closePosSame = afterOpen.find("%}");
                if (closePosSame != std::string::npos) {
                    std::string content
                        = StringHelpers::trim_copy(afterOpen.substr(0, closePosSame));
                    if (!content.empty()) {
                        comments.push_back(utf8_to_wstring(content));
                    }
                } else {
                    // If there is content after %{ on the same line, include it
                    std::string firstLineContent = StringHelpers::trim_copy(afterOpen);
                    if (!firstLineContent.empty()) {
                        comments.push_back(utf8_to_wstring(firstLineContent));
                    }
                    // Read until we find %}
                    while (!istream.eof()) {
                        safegetline(istream, line);
                        std::string tline = StringHelpers::trim_copy(line);
                        size_t closePos = tline.find("%}");
                        if (closePos != std::string::npos) {
                            std::string beforeClose
                                = StringHelpers::trim_copy(tline.substr(0, closePos));
                            if (!beforeClose.empty()) {
                                comments.push_back(utf8_to_wstring(beforeClose));
                            }
                            break;
                        } else {
                            // Entire line is content of the comment block
                            comments.push_back(utf8_to_wstring(tline));
                        }
                    }
                }
            }
            // Handle single-line comment blocks starting with '%' (existing behavior)
            else if (isCommentedLine(line)) {
                comments.push_back(utf8_to_wstring(removeCommentCharacters(line)));
                while (!istream.eof()) {
                    safegetline(istream, line);
                    if (isCommentedLine(line)) {
                        comments.push_back(utf8_to_wstring(removeCommentCharacters(line)));
                    } else {
                        // If the next non-comment line is a %{ block, handle it too
                        std::string nextTrim = StringHelpers::trim_left_copy(line);
                        if (StringHelpers::starts_with(nextTrim, "%{")) {
                            // process multiline block starting on this line
                            std::string afterOpen = nextTrim.substr(nextTrim.find("%{") + 2);
                            size_t closePosSame = afterOpen.find("%}");
                            if (closePosSame != std::string::npos) {
                                std::string content
                                    = StringHelpers::trim_copy(afterOpen.substr(0, closePosSame));
                                if (!content.empty()) {
                                    comments.push_back(utf8_to_wstring(content));
                                }
                            } else {
                                std::string firstLineContent = StringHelpers::trim_copy(afterOpen);
                                if (!firstLineContent.empty()) {
                                    comments.push_back(utf8_to_wstring(firstLineContent));
                                }
                                // Read until close
                                while (!istream.eof()) {
                                    safegetline(istream, line);
                                    std::string tline = StringHelpers::trim_copy(line);
                                    size_t closePos = tline.find("%}");
                                    if (closePos != std::string::npos) {
                                        std::string beforeClose
                                            = StringHelpers::trim_copy(tline.substr(0, closePos));
                                        if (!beforeClose.empty()) {
                                            comments.push_back(utf8_to_wstring(beforeClose));
                                        }
                                        break;
                                    } else {
                                        comments.push_back(utf8_to_wstring(tline));
                                    }
                                }
                            }
                        }
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
