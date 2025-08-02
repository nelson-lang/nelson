//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <utility>
#include "StringHelpers.hpp"
#include "FileSystemWrapper.hpp"
#include "Exception.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Exception::Exception()
{
    this->backtrace.clear();
    this->identifier.clear();
    this->msg.clear();
}
//=============================================================================
Exception::Exception(const std::string& msg_in, std::vector<PositionScript> positions,
    const std::string& identifier_in)
    : backtrace(std::move(positions))
    , identifier(utf8_to_wstring(identifier_in))
    , msg(utf8_to_wstring(msg_in))
{
}
//=============================================================================
Exception::Exception(
    std::wstring msg_in, std::vector<PositionScript> positions, std::wstring identifier_in)
    : backtrace(std::move(positions)), identifier(std::move(identifier_in)), msg(std::move(msg_in))
{
}
//=============================================================================
Exception::Exception(
    const std::string& msg_in, const PositionScript& position, const std::string& identifier_in)
{
    this->backtrace.clear();
    this->backtrace.push_back(position);
    this->msg = utf8_to_wstring(msg_in);
    this->identifier = utf8_to_wstring(identifier_in);
}
//=============================================================================
Exception::Exception(
    const std::wstring& msg_in, const PositionScript& position, const std::wstring& identifier_in)
{
    this->backtrace.clear();
    this->backtrace.push_back(position);
    this->msg = msg_in;
    this->identifier = identifier_in;
}
//=============================================================================
Exception::Exception(const std::string& msg_in, const std::string& identifier_in)
{
    this->backtrace.clear();
    this->msg = utf8_to_wstring(msg_in);
    this->identifier = utf8_to_wstring(identifier_in);
}
//=============================================================================
Exception::Exception(const std::wstring& msg_in, const std::wstring& identifier_in)
{
    this->backtrace.clear();
    this->msg = msg_in;
    this->identifier = identifier_in;
}
//=============================================================================
Exception::~Exception()
{
    this->backtrace.clear();
    this->msg.clear();
    this->identifier.clear();
}
//=============================================================================
Exception::Exception(const Exception& copy)
{
    this->msg = copy.msg;
    this->identifier = copy.identifier;
    this->backtrace = copy.backtrace;
}
//=============================================================================
void
Exception::operator=(const Exception& copy)
{
    if (this == &copy) {
        return;
    }
    this->msg = copy.msg;
    this->identifier = copy.identifier;
    this->backtrace = copy.backtrace;
}
//=============================================================================
void
Exception::printMe(Interface* io) const
{
    if (!msg.empty()) {
        io->errorMessage(getFormattedErrorMessage());
    }
}
//=============================================================================
bool
Exception::matches(const std::wstring& tst_msg)
{
    return (msg == tst_msg);
}
//=============================================================================
bool
Exception::matches(const std::string& tst_msg)
{
    return (msg == utf8_to_wstring(tst_msg));
}
//=============================================================================
void
Exception::setMessage(const std::wstring& message_in)
{
    msg = message_in;
}
//=============================================================================
void
Exception::setMessage(const std::string& message_in)
{
    msg = utf8_to_wstring(message_in);
}
//=============================================================================
int
Exception::getLine() const
{
    if (backtrace.empty()) {
        return -1;
    }
    return backtrace[0].getLine();
}
//=============================================================================
std::wstring
Exception::getFunctionName() const
{
    if (backtrace.empty()) {
        return L"";
    }
    return backtrace[0].getFunctionName();
}
//=============================================================================
std::wstring
Exception::getFilename() const
{
    if (backtrace.empty()) {
        return L"";
    }
    return backtrace[0].getFilename();
}
//=============================================================================
std::wstring
Exception::getFormattedErrorMessage() const
{
    std::wstring message = getMessage();
    std::vector<PositionScript> traces = getTrace();
    size_t nbTraces = traces.size();

    std::wstring functionName;
    std::wstring lineAsString;

    int i = -1;
    if (nbTraces != 0) {
        if (traces[0].getFunctionName() != L"error") {
            if (nbTraces > 1) {
                i = 0;
            }
        } else {
            if (nbTraces > 1) {
                if (traces[1].getFunctionName() != L"run") {
                    i = 1;
                }
            }
        }
    }

    if (i != -1) {
        size_t idx = i + 1;
        if (idx < nbTraces) {
            if (traces[idx].getFunctionName() != L"run") {
                functionName = traces[i].getFunctionName();
                if (traces[i].getLine() != 0) {
                    lineAsString = std::to_wstring(traces[i].getLine());
                }
            }
        }
    }

    if (functionName.empty()) {
        message = L"\n" + _W("Error: ") + L"\n" + message + L"\n";
    } else {
        if (!lineAsString.empty()) {
            message = L"\n" + _W("Error in ") + functionName + L" (" + _W("line") + L" "
                + lineAsString + L")\n" + message + L"\n";
        } else {
            message = L"\n" + _W("Error in ") + functionName + L"\n" + message + L"\n";
        }
    }
    if (nbTraces > 0) {
        message = message + L"\n";
    }

    for (size_t k = 0; k < nbTraces; k++) {
        if (traces[k].getFunctionName() == L"run") {
            if ((k >= 1) && traces[k - 1].getLine() != 0) {
                size_t pos = k - 1;
                FileSystemWrapper::Path pf(traces[pos].getFilename());
                std::wstring filename;
                if (traces[pos].getFilename().size() > 50) {
                    filename = pf.filename().wstring();
                } else {
                    filename = pf.wstring();
                }
                message = message
                    + fmt::sprintf(_W("at line %5d of \'%s\'\n"), traces[pos].getLine(), filename);
            }
        }
    }
    return message;
}
//=============================================================================
bool
Exception::isEmpty() const
{
    return backtrace.empty() && (this->msg.empty());
}
//=============================================================================
void
Exception::setIdentifier(const std::wstring& identifier_in)
{
    this->identifier = identifier_in;
}
//=============================================================================
void
Exception::setIdentifier(const std::string& identifier_in)
{
    this->identifier = utf8_to_wstring(identifier_in);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
