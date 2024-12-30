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
#include <cstring>
#include "HistoryManager.hpp"
#include "NelsonHistory.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
#include "StringHelpers.hpp"
//=============================================================================
static char* _line = nullptr;
//=============================================================================
Nelson::wstringVector
Nelson::History::get()
{
    auto* hist
        = static_cast<HistoryManager*>(NelsonConfiguration::getInstance()->getHistoryManager());
    if (hist != nullptr) {
        return hist->get();
    }
    return {};
}
//=============================================================================
bool
Nelson::History::addLine(const std::wstring& line)
{
    auto* hist
        = static_cast<HistoryManager*>(NelsonConfiguration::getInstance()->getHistoryManager());
    if (hist != nullptr) {
        std::wstring mline;
        mline.assign(line);
        StringHelpers::replace_last(mline, L"\n", L"");
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
