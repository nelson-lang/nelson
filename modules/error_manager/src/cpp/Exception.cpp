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
#include "Exception.hpp"
#include "characters_encoding.hpp"
#include <boost/algorithm/string.hpp>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
//=============================================================================
#ifdef _MSC_VER
#define strdup _strdup
#endif
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
{
    this->backtrace = positions;
    this->identifier = utf8_to_wstring(identifier_in);
    this->msg = utf8_to_wstring(msg_in);
}
//=============================================================================
Exception::Exception(const std::wstring& msg_in, std::vector<PositionScript> positions,
    const std::wstring& identifier_in)
{
    this->backtrace = positions;
    this->identifier = identifier_in;
    this->msg = msg_in;
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
    this->msg = L"";
    this->identifier = L"";
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
Exception::printMe(Interface* io)
{
    if (msg != L"") {
        io->errorMessage(getFormattedErrorMessage());
    }
}
//=============================================================================
bool
Exception::matches(const std::wstring& tst_msg)
{
    return (msg.compare(tst_msg) == 0);
}
//=============================================================================
bool
Exception::matches(const std::string& tst_msg)
{
    return (msg.compare(utf8_to_wstring(tst_msg)) == 0);
}
//=============================================================================
std::wstring
Exception::getMessage()
{
    return msg;
}
//=============================================================================
int
Exception::getLine()
{
    if (backtrace.empty()) {
        return -1;
    }
    return backtrace[0].getLine();
}
//=============================================================================
std::wstring
Exception::getFunctionName()
{
    if (backtrace.empty()) {
        return L"";
    }
    return backtrace[0].getFunctionName();
}
//=============================================================================
std::wstring
Exception::getFilename()
{
    if (backtrace.empty()) {
        return L"";
    }
    return backtrace[0].getFilename();
}
//=============================================================================
std::wstring
Exception::getFormattedErrorMessage()
{
    std::wstring formattedMessage;
    if (!msg.empty()) {
        formattedMessage.append(msg);
    }
    if (!backtrace.empty()) {
        std::wstring filename = backtrace[0].getFilename();
        std::wstring functionName = backtrace[0].getFunctionName();
        int line = backtrace[0].getLine();
        formattedMessage.append(L"\n");
        if (line == 0) {
            if (filename != L"") {
                formattedMessage = formattedMessage + std::wstring(L"In ") + filename + L"\n";
            }
        } else {
            if (functionName != L"") {
                formattedMessage = formattedMessage + std::wstring(L"In ") + filename
                    + L" function " + functionName + L" (line " + std::to_wstring(line) + L")\n";
            } else {
                formattedMessage = formattedMessage + std::wstring(L"In ") + filename + L" (line "
                    + std::to_wstring(line) + L")\n";
            }
        }
    }
    return formattedMessage;
}
//=============================================================================
bool
Exception::isEmpty()
{
    return backtrace.empty() && (this->msg == L"");
}
//=============================================================================
std::vector<PositionScript>
Exception::getTrace()
{
    return this->backtrace;
}
//=============================================================================
std::wstring
Exception::getIdentifier()
{
    return this->identifier;
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
