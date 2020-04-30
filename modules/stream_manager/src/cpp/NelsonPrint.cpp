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
#include "NelsonPrint.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static Interface* currentInterface = nullptr;
//=============================================================================
void
setPrintInterface(Interface* io)
{
    currentInterface = io;
}
//=============================================================================
void
NelsonPrint(const std::wstring& msg)
{
    if (currentInterface) {
        currentInterface->outputMessage(msg);
    }
}
//=============================================================================
void
NelsonPrint(const std::string& msg)
{
    if (currentInterface) {
        currentInterface->outputMessage(msg);
    }
}
//=============================================================================
}
//=============================================================================
int
NelsonPrint(const wchar_t* msg)
{
    if (Nelson::currentInterface) {
        Nelson::currentInterface->outputMessage(msg);
        return (int)wcslen(msg);
    }
    return -1;
}
//=============================================================================
