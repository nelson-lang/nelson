//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <iostream>
#include "Exception.hpp"
#include "characters_encoding.hpp"
//=============================================================================
#ifdef _MSC_VER
#define strdup _strdup
#endif
//=============================================================================
namespace Nelson {
    //=============================================================================
    int exceptionCount = 0;
    //=============================================================================
    Exception::Exception(std::string msg_in, std::string functionname, int line_in, int position_in, std::string filename_in, std::string identifier_in)
    {
        this->msg = utf8_to_wstring(msg_in);
        this->functionname = utf8_to_wstring(functionname);
        this->line = line_in;
        this->position = position_in;
        this->filename = utf8_to_wstring(filename_in);
        this->identifier = utf8_to_wstring(identifier_in);
        exceptionCount++;
    }
    //=============================================================================
    Exception::Exception(std::wstring msg_in, std::wstring functionname, int line_in, int position_in, std::wstring filename_in, std::wstring identifier_in)
    {
        this->msg = msg_in;
        this->functionname = functionname;
        this->line = line_in;
        this->position = position_in;
        this->filename = filename_in;
        this->identifier = identifier_in;
        exceptionCount++;
    }
    //=============================================================================
    Exception::~Exception()
    {
        this->msg = L"";
        this->line = -1;
        this->position = -1;
        this->functionname = L"";
        this->filename = L"";
        this->identifier = L"";
        exceptionCount--;
    }
    //=============================================================================
    Exception::Exception(const Exception& copy)
    {
        this->msg = copy.msg;
        this->line = copy.line;
        this->position = copy.position;
        this->functionname = copy.functionname;
        this->filename = copy.filename;
        this->identifier = copy.identifier;
        exceptionCount++;
    }
    //=============================================================================
    void Exception::operator=(const Exception &copy)
    {
        if (this == &copy)
        {
            return;
        }
        this->msg = copy.msg;
        this->functionname = copy.functionname;
        this->position = copy.position;
        this->line = copy.line;
        this->filename = copy.filename;
        this->identifier = copy.identifier;
    }
    //=============================================================================
    void Exception::printMe(Interface *io)
    {
        if (msg != L"")
        {
            io->errorMessage(getFormattedErrorMessage());
        }
    }
    //=============================================================================
    bool Exception::matches(const std::wstring &tst_msg)
    {
        return (msg.compare(tst_msg) == 0);
    }
    //=============================================================================
    bool Exception::matches(const std::string &tst_msg)
    {
        return (msg.compare(utf8_to_wstring(tst_msg)) == 0);
    }
    //=============================================================================
    std::wstring Exception::getMessage()
    {
        return msg;
    }
    //=============================================================================
    int Exception::getLine()
    {
        return line;
    }
    //=============================================================================
    int Exception::getPosition()
    {
        return position;
    }
    //=============================================================================
    void Exception::setLinePosition(int line_in, int position_in)
    {
        line = line_in;
        position = position_in;
    }
    //=============================================================================
    void Exception::setMessage(const std::string &msg_in)
    {
        msg = utf8_to_wstring(msg_in);
    }
    //=============================================================================
    void Exception::setMessage(const std::wstring &msg_in)
    {
        msg = msg_in;
    }
    //=============================================================================
    void Exception::setFunctionName(const std::wstring &functionname)
    {
        this->functionname = functionname;
    }
    //=============================================================================
    void Exception::setFunctionName(const std::string &functionname)
    {
        this->functionname = utf8_to_wstring(functionname);
    }
    //=============================================================================
    std::wstring Exception::getFunctionName()
    {
        return this->functionname;
    }
    //=============================================================================
    std::wstring Exception::getFormattedErrorMessage()
    {
        std::wstring formattedMessage;
        formattedMessage.append(msg);
        if ((functionname != L"") && (functionname != L"EvaluateScript"))
        {
            formattedMessage.append(L"\n");
            formattedMessage.append(_W("called from:\n"));
            if (line > 0 && position > 0)
            {
                formattedMessage.append(functionname);
                formattedMessage.append(_W("\nat line: ") + std::to_wstring(line) + _W(" position: ") + std::to_wstring(position));
            }
            else
            {
                formattedMessage.append(functionname);
            }
            formattedMessage.append(L"\n");
        }
        return formattedMessage;
    }
    //=============================================================================
    void Exception::setFileName(const std::wstring &filename)
    {
        this->filename = filename;
    }
    //=============================================================================
    bool Exception::isEmpty()
    {
        return ((this->msg == L"") &&
                (this->line == -1) &&
                (this->position == -1) &&
                (this->functionname == L"") &&
                (this->filename == L"") &&
                (this->identifier == L""));
    }
    //=============================================================================
    std::wstring Exception::getIdentifier()
    {
        return this->identifier;
    }
    //=============================================================================
    void Exception::setIdentifier(const std::wstring &identifier_in)
    {
        this->identifier = identifier_in;
    }
    //=============================================================================
    void Exception::setIdentifier(const std::string &identifier_in)
    {
        this->identifier = utf8_to_wstring(identifier_in);
    }
    //=============================================================================
}
//=============================================================================
