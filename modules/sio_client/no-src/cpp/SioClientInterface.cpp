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
#include "SioClientInterface.hpp"
//=============================================================================
namespace Nelson {
SioClientInterface::SioClientInterface() {}
//=============================================================================
SioClientInterface::~SioClientInterface() {}
//=============================================================================
std::wstring
SioClientInterface::getLine(std::wstring prompt)
{
    return std::wstring();
}
//=============================================================================
std::string
SioClientInterface::getLine(std::string prompt)
{
    return std::string();
}
//=============================================================================
std::wstring
SioClientInterface::getInput(std::wstring prompt)
{
    return std::wstring();
}
//=============================================================================
size_t
SioClientInterface::getTerminalWidth()
{
    return 0;
}
//=============================================================================
void
SioClientInterface::outputMessage(std::wstring msg)
{
}
//=============================================================================
void
SioClientInterface::outputMessage(std::string msg)
{
}
//=============================================================================
void
SioClientInterface::errorMessage(std::wstring msg)
{
}
//=============================================================================
void
SioClientInterface::errorMessage(std::string msg)
{
}
//=============================================================================
void
SioClientInterface::warningMessage(std::wstring msg)
{
}
//=============================================================================
void
SioClientInterface::warningMessage(std::string msg)
{
}
//=============================================================================
void
SioClientInterface::clearTerminal()
{
}
//=============================================================================
bool
SioClientInterface::isAtPrompt()
{
    return false;
}
//=============================================================================
}
//=============================================================================
