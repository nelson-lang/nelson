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
#define _CRT_SECURE_NO_WARNINGS
#define _SCL_SECURE_NO_WARNINGS
//=============================================================================
#ifndef _MSC_VER
#define BOOST_NO_CXX11_SCOPED_ENUMS
#endif
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
#ifndef _MSC_VER
#undef BOOST_NO_CXX11_SCOPED_ENUMS
#endif
#include "GetPreferencesPath.hpp"
#include "HistoryManager.hpp"
#include "characters_encoding.hpp"
#include <boost/algorithm/string.hpp>
#include <ctime>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
//=============================================================================
namespace Nelson {
//=============================================================================
#define DEFAULT_SAVELASTNCOMMANDS 250000
#define DEFAULT_SAVEAFTERNCOMMANDS 0
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
HistoryManager::HistoryManager()
{
    bAllowDuplicatedLines = false;
    commands.clear();
    saveLastNCommands = DEFAULT_SAVELASTNCOMMANDS;
    saveAfterNCommands = DEFAULT_SAVEAFTERNCOMMANDS;
    nbCommands = 0;
    // commands.reserve(4096);
    setFilename(GetPreferencesPath() + std::wstring(L"/Nelson.history"));
    token.clear();
    token_position = 0;
    tokens_found.clear();
    bEmptyLineAtNextState = false;
    appendHeader();
    bRemoveExit = true;
    bSaveEnabled = true;
}
//=============================================================================
HistoryManager::~HistoryManager()
{
    commands.clear();
    tokens_found.clear();
    token.clear();
    filename.clear();
    token_position = 0;
    bEmptyLineAtNextState = false;
}
//=============================================================================
void
HistoryManager::setSaveEnabled(bool bSave)
{
    bSaveEnabled = bSave;
}
//=============================================================================
bool
HistoryManager::getSaveEnabled()
{
    return bSaveEnabled;
}
//=============================================================================
void
HistoryManager::setRemoveExit(bool bRemove)
{
    bRemoveExit = bRemove;
}
//=============================================================================
bool
HistoryManager::getRemoveExit()
{
    return bRemoveExit;
}
//=============================================================================
void
HistoryManager::setAllowDuplicatedLines(bool bAllow)
{
    bAllowDuplicatedLines = bAllow;
}
//=============================================================================
bool
HistoryManager::getAllowDuplicatedLines()
{
    return bAllowDuplicatedLines;
}
//=============================================================================
void
HistoryManager::setSaveAfterNCommands(size_t nLines)
{
    saveAfterNCommands = nLines;
}
//=============================================================================
size_t
HistoryManager::getSaveAfterNCommands()
{
    return saveAfterNCommands;
}
//=============================================================================
void
HistoryManager::setLastNCommandsSize(size_t newsize)
{
    if (newsize == 0) {
        saveLastNCommands = DEFAULT_SAVELASTNCOMMANDS;
    } else {
        saveLastNCommands = newsize;
    }
}
//=============================================================================
size_t
HistoryManager::getLastNCommandsSize()
{
    return saveLastNCommands;
}
//=============================================================================
bool
HistoryManager::appendHeader()
{
    std::wstring pre = L"%% -- ";
    std::wstring post = L" -- %%";
#ifdef __GNUC__
    /* std::put_time only in GCC >= 5 ... */
    time_t timer = time(NULL);
    struct tm t_result;
    const struct tm* timeptr = localtime_r(&timer, &t_result);
    std::wstring month;
    if (timeptr->tm_mon + 1 < 10) {
        month = L"0" + std::to_wstring(int(timeptr->tm_mon + 1));
    } else {
        month = std::to_wstring(int(timeptr->tm_mon + 1));
    }
    std::wstring day;
    if (timeptr->tm_mday < 10) {
        day = L"0" + std::to_wstring(int(timeptr->tm_mday));
    } else {
        day = std::to_wstring(int(timeptr->tm_mday));
    }
    std::wstring year = std::to_wstring(int(1900 + timeptr->tm_year));
    std::wstring hour;
    if (timeptr->tm_hour < 10) {
        hour = L"0" + std::to_wstring(int(timeptr->tm_hour));
    } else {
        hour = std::to_wstring(int(timeptr->tm_hour));
    }
    std::wstring minutes;
    if (timeptr->tm_min < 10) {
        minutes = L"0" + std::to_wstring(int(timeptr->tm_min));
    } else {
        minutes = std::to_wstring(int(timeptr->tm_min));
    }
    std::wstring sec;
    if (timeptr->tm_sec < 10) {
        sec = L"0" + std::to_wstring(int(timeptr->tm_sec));
    } else {
        sec = std::to_wstring(int(timeptr->tm_sec));
    }
    std::wstring line = pre + month + std::wstring(L"-") + day + std::wstring(L"-") + year
        + std::wstring(L" ") + hour + std::wstring(L":") + minutes + std::wstring(L":") + sec
        + post;
#else
    auto t = std::time(nullptr);
    auto tm = *std::localtime(&t);
    std::stringstream ss;
    ss << std::put_time(&tm, "%m-%d-%Y %H:%M:%S");
    std::wstring line = pre + utf8_to_wstring(ss.str()) + post;
#endif
    commands.push_back(line);
    return true;
}
//=============================================================================
bool
HistoryManager::appendLine(const std::wstring& line)
{
    wstringVector strs;
    boost::split(strs, line, boost::is_any_of(L"\n"));
    wstringVector lines;
    for (auto s : strs) {
        if ((s != L"\n") && (!s.empty())) {
            lines.push_back(s);
        }
    }
    strs.clear();
    if (!bAllowDuplicatedLines) {
        if (commands.size() > 0) {
            for (size_t k = 0; k < lines.size(); k++) {
                if (commands[commands.size() - 1] != lines[k]) {
                    commands.push_back(lines[k]);
                    nbCommands++;
                }
            }
        } else {

            for (auto s : strs) {
                commands.push_back(s);
                nbCommands++;
            }
        }
    } else {
        for (auto l : lines) {
            commands.push_back(l);
            nbCommands++;
        }
    }
    setToken(L"");
    if (saveAfterNCommands > 0) {
        if (nbCommands == saveAfterNCommands) {
            saveToFile();
            nbCommands = 0;
        }
    }
    return true;
}
//=============================================================================
bool
HistoryManager::appendLines(const wstringVector& lines)
{
    for (auto l : lines) {
        if ((l != L"\n") && (!l.empty())) {
            commands.push_back(l);
        }
    }
    setToken(L"");
    return true;
}
//=============================================================================
bool
HistoryManager::remove(size_t pos)
{
    if (commands.size() > 0) {
        if (pos < commands.size()) {
            commands.erase(commands.begin() + pos);
            setToken(L"");
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
HistoryManager::remove(size_t firstpos, size_t lastpos)
{
    if (commands.size() > 0) {
        size_t nbElements = lastpos - firstpos;
        if (firstpos < commands.size() && lastpos < commands.size()) {
            for (size_t k = 0; k < nbElements; k++) {
                commands.erase(commands.begin() + firstpos);
            }
            setToken(L"");
            return true;
        }
    }
    return false;
}
//=============================================================================
size_t
HistoryManager::getCurrentSize()
{
    return commands.size();
}
//=============================================================================
std::wstring
HistoryManager::getFirstLine()
{
    std::wstring line = L"";
    if (!commands.empty()) {
        line = commands[commands.size() - 1];
    }
    return line;
}
//=============================================================================
std::wstring
HistoryManager::getLastLine()
{
    std::wstring line = L"";
    if (!commands.empty()) {
        line = commands[0];
    }
    return line;
}
//=============================================================================
wstringVector
HistoryManager::get()
{
    wstringVector res = commands;
    return res;
}
//=============================================================================
std::wstring
HistoryManager::get(size_t pos)
{
    if (commands.size() > 0) {
        return commands[pos];
    }
    return L"";
}
//=============================================================================
wstringVector
HistoryManager::get(size_t firstpos, size_t lastpos)
{
    wstringVector res;
    if (commands.size() > 0) {
        size_t nbElements = lastpos - firstpos;
        if (firstpos < commands.size() && lastpos < commands.size()) {
            for (size_t k = 0; k < nbElements; k++) {
                res.push_back(commands[firstpos + k]);
            }
        }
    }
    return res;
}
//=============================================================================
std::wstring
HistoryManager::getPreviousLine()
{
    std::wstring line = L"";
    if (bEmptyLineAtNextState) {
        token_position++;
    }
    if (token_position <= 0) {
        token_position = 0;
    } else {
        token_position--;
    }

    if (token_position < (int64)commands.size()) {
        if (token.empty()) {
            line = commands[(size_t)token_position];

        } else {
            if ((size_t)token_position < tokens_found.size()) {
                line = tokens_found[(size_t)token_position];
            }
        }
    }
    bEmptyLineAtNextState = false;
    return line;
}
//=============================================================================
std::wstring
HistoryManager::getNextLine()
{
    std::wstring line = L"";
    if (token.empty()) {
        if (token_position < (int64)commands.size()) {
            token_position++;
        }
        if ((token_position >= 0) && (token_position < (int64)commands.size())) {
            line = commands[(size_t)token_position];
        }
        if (token_position == commands.size()) {
            token_position--;
        }
    } else {
        if (token_position < (int64)tokens_found.size()) {
            token_position++;
        }
        if ((token_position >= 0) && (token_position < (int64)tokens_found.size())) {
            line = tokens_found[(size_t)token_position];
        }
        if (token_position == tokens_found.size()) {
            line = token;
            token_position--;
        }
    }
    bEmptyLineAtNextState = true;
    return line;
}
//=============================================================================
bool
HistoryManager::loadFromFile()
{
    return loadFromFile(this->filename);
}
//=============================================================================
bool
HistoryManager::loadFromFile(const std::wstring& _filename)
{
    commands.clear();
    std::ifstream istream;
#ifdef _MSC_VER
    istream.open(_filename);
#else
    istream.open(wstring_to_utf8(_filename));
#endif
    if (istream.is_open()) {
        while (!istream.eof()) {
            std::string line;
            safegetline(istream, line);
            commands.push_back(utf8_to_wstring(line));
        }
        istream.close();
    }
    appendHeader();
    setToken(L"");
    return true;
}
//=============================================================================
bool
HistoryManager::setFilename(const std::wstring& filename)
{
    this->filename.assign(filename);
    return true;
}
//=============================================================================
std::wstring
HistoryManager::getFilename()
{
    return this->filename;
}
//=============================================================================
bool
HistoryManager::clear(bool bWithHeader)
{
    commands.clear();
    token.clear();
    token_position = 0;
    tokens_found.clear();
    if (bWithHeader) {
        appendHeader();
    }
    return true;
}
//=============================================================================
size_t
HistoryManager::getNumberOfLines()
{
    return commands.size();
}
//=============================================================================
std::wstring
HistoryManager::getNthLine(size_t N)
{
    std::wstring res = L"";
    if (N < commands.size()) {
        res = commands[N];
    }
    return res;
}
//=============================================================================
bool
HistoryManager::setToken(const std::wstring& _token)
{
    token.assign(_token);
    tokens_found.clear();
    if (!token.empty()) {
        for (size_t k = 0; k < commands.size(); k++) {
            tokens_found.reserve(commands.size());
            if (boost::algorithm::starts_with(commands[k], token)) {
                tokens_found.push_back(commands[k]);
            }
        }
        token_position = tokens_found.size();
    } else {
        token_position = commands.size();
    }
    bEmptyLineAtNextState = false;
    return true;
}
//=============================================================================
bool
HistoryManager::copyPreviousFile(const std::wstring& _filename)
{
    boost::filesystem::path src(_filename);
    boost::filesystem::path dst(_filename + L".bak");
    bool bRes = false;
    try {
        bRes = boost::filesystem::exists(src) && !boost::filesystem::is_directory(src);
    } catch (const boost::filesystem::filesystem_error&) {
        bRes = false;
    }
    if (bRes) {
        try {
            boost::filesystem::copy_file(
                src, dst, boost::filesystem::copy_option::overwrite_if_exists);
            bRes = true;
        } catch (const boost::filesystem::filesystem_error&) {
            bRes = false;
        }
    }
    return bRes;
}
//=============================================================================
bool
HistoryManager::detectExit(const std::wstring& line)
{
    if ((line == L"exit") || (line == L"quit")) {
        return true;
    }
    return false;
}
//=============================================================================
bool
HistoryManager::saveToFile(const std::wstring& _filename)
{
    if (bSaveEnabled) {
        copyPreviousFile(_filename);
        FILE* fw;
#ifdef _MSC_VER
        fw = _wfopen(_filename.c_str(), L"wt");
#else
        fw = fopen(wstring_to_utf8(_filename).c_str(), "wt");
#endif
        if (fw != nullptr) {
            size_t firstIndex = 0;
            if (commands.size() > saveLastNCommands) {
                firstIndex = commands.size() - saveLastNCommands;
            }
            for (size_t k = firstIndex; k < commands.size(); k++) {
                if (k == commands.size() - 1) {
                    if (bRemoveExit) {
                        if (!detectExit(commands[k])) {
                            if (k != firstIndex) {
                                fprintf(fw, "\n");
                            }
                            fprintf(fw, "%s", wstring_to_utf8(commands[k]).c_str());
                        }
                    }
                } else {
                    if (k != firstIndex) {
                        fprintf(fw, "\n");
                    }
                    fprintf(fw, "%s", wstring_to_utf8(commands[k]).c_str());
                }
            }
            fclose(fw);
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
HistoryManager::saveToFile()
{
    return saveToFile(filename);
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
