//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
//=============================================================================
#include <boost/algorithm/string/replace.hpp>
#include "HistoryManager.hpp"
#include "NelsonHistory.hpp"
#include "NelsonHistory.h"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
static char* _line = nullptr;
//=============================================================================
bool
Nelson::History::addLine(const std::wstring& line)
{
    auto* hist
        = static_cast<HistoryManager*>(NelsonConfiguration::getInstance()->getHistoryManager());
    if (hist != nullptr) {
        std::wstring mline;
        mline.assign(line);
        boost::replace_last(mline, L"\n", L"");
        return hist->appendLine(mline);
    }
    return false;
}
//=============================================================================
bool
Nelson::History::setToken(const std::wstring& line)
{
    auto* hist
        = static_cast<HistoryManager*>(NelsonConfiguration::getInstance()->getHistoryManager());
    if (hist != nullptr) {
        return hist->setToken(line);
    }
    return false;
}
//=============================================================================
std::wstring
Nelson::History::getNextLine()
{
    auto* hist
        = static_cast<HistoryManager*>(NelsonConfiguration::getInstance()->getHistoryManager());
    if (hist != nullptr) {
        return hist->getNextLine();
    }
    return L"";
}
//=============================================================================
std::wstring
Nelson::History::getPreviousLine()
{
    auto* hist
        = static_cast<HistoryManager*>(NelsonConfiguration::getInstance()->getHistoryManager());
    if (hist != nullptr) {
        return hist->getPreviousLine();
    }
    return L"";
}
//=============================================================================
void
NelsonHistorySetToken(const char* token)
{
    if (token != nullptr) {
        Nelson::History::setToken(Nelson::utf8_to_wstring(token));
    }
}
//=============================================================================
const char*
NelsonHistoryGetNextLine(void)
{
    std::wstring wline = Nelson::History::getNextLine();
    std::string line = Nelson::wstring_to_utf8(wline);
    if (_line != nullptr) {
        delete[] _line;
        _line = nullptr;
    }
    try {
        _line = new char[line.size() + 1];
        strcpy(_line, line.c_str());
    } catch (std::bad_alloc&) {
        _line = nullptr;
    }
    return _line;
}
//=============================================================================
const char*
NelsonHistoryGetPreviousLine(void)
{
    std::wstring wline = Nelson::History::getPreviousLine();
    std::string line = Nelson::wstring_to_utf8(wline);
    if (_line != nullptr) {
        delete[] _line;
        _line = nullptr;
    }
    try {
        _line = new char[line.size() + 1];
        strcpy(_line, line.c_str());
    } catch (std::bad_alloc&) {
        _line = nullptr;
    }
    return _line;
}
//=============================================================================
