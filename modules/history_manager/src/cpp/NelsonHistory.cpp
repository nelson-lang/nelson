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
//=============================================================================
#include "NelsonHistory.hpp"
#include "Evaluator.hpp"
#include "GetNelsonMainEvaluatorDynamicFunction.hpp"
#include "HistoryManager.hpp"
#include "NelsonHistory.h"
#include "characters_encoding.hpp"
#include <boost/algorithm/string/replace.hpp>
//=============================================================================
Nelson::Evaluator* mainEvaluator = nullptr;
static char* _line = nullptr;
//=============================================================================
bool
Nelson::History::addLine(const std::wstring& line)
{
    if (mainEvaluator == nullptr) {
        mainEvaluator = static_cast<Evaluator*>(GetNelsonMainEvaluatorDynamicFunction());
    }
    if (mainEvaluator == nullptr) {
        return false;
    }
    auto* hist = static_cast<HistoryManager*>(mainEvaluator->HistoryManager);
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
    if (mainEvaluator == nullptr) {
        mainEvaluator = static_cast<Evaluator*>(GetNelsonMainEvaluatorDynamicFunction());
    }
    if (mainEvaluator == nullptr) {
        return false;
    }
    auto* hist = static_cast<HistoryManager*>(mainEvaluator->HistoryManager);
    if (hist != nullptr) {
        return hist->setToken(line);
    }
    return false;
}
//=============================================================================
std::wstring
Nelson::History::getNextLine()
{
    if (mainEvaluator == nullptr) {
        mainEvaluator = static_cast<Evaluator*>(GetNelsonMainEvaluatorDynamicFunction());
    }
    if (mainEvaluator == nullptr) {
        return L"";
    }
    auto* hist = static_cast<HistoryManager*>(mainEvaluator->HistoryManager);
    if (hist != nullptr) {
        return hist->getNextLine();
    }
    return L"";
}
//=============================================================================
std::wstring
Nelson::History::getPreviousLine()
{
    if (mainEvaluator == nullptr) {
        mainEvaluator = static_cast<Evaluator*>(GetNelsonMainEvaluatorDynamicFunction());
    }
    if (mainEvaluator == nullptr) {
        return L"";
    }
    auto* hist = static_cast<HistoryManager*>(mainEvaluator->HistoryManager);
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
