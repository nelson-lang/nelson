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
#include "EvaluateInterface.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
//=============================================================================
EvaluateInterface::EvaluateInterface() { outputBuffer.clear(); }
//=============================================================================
EvaluateInterface::~EvaluateInterface() { outputBuffer.clear(); }
//=============================================================================
std::wstring
EvaluateInterface::getLine(std::wstring prompt)
{
    return L"";
}
//=============================================================================
std::string
EvaluateInterface::getLine(std::string prompt)
{
    return "";
}
//=============================================================================
std::wstring
EvaluateInterface::getInput(std::wstring prompt)
{
    Error(_W("input function not allowed from evalc."));
    return L"";
}
//=============================================================================
size_t
EvaluateInterface::getTerminalWidth()
{
    return WIDTH;
}
//=============================================================================
void
EvaluateInterface::outputMessage(std::wstring msg)
{
    outputBuffer.append(msg);
}
//=============================================================================
void
EvaluateInterface::outputMessage(std::string msg)
{
    outputMessage(utf8_to_wstring(msg));
}
//=============================================================================
void
EvaluateInterface::errorMessage(std::wstring msg)
{
    outputMessage(msg);
}
//=============================================================================
void
EvaluateInterface::errorMessage(std::string msg)
{
    errorMessage(utf8_to_wstring(msg));
}
//=============================================================================
void
EvaluateInterface::warningMessage(std::wstring msg)
{
    outputMessage(msg);
}
//=============================================================================
void
EvaluateInterface::warningMessage(std::string msg)
{
    warningMessage(utf8_to_wstring(msg));
}
//=============================================================================
void
EvaluateInterface::clearTerminal()
{
    outputBuffer.clear();
}
//=============================================================================
bool
EvaluateInterface::isAtPrompt()
{
    return false;
}
//=============================================================================
std::wstring
EvaluateInterface::getOutputBuffer()
{
    return outputBuffer;
}
//=============================================================================
