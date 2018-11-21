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
#include <algorithm>
#include "SioClientRegister.hpp"
#include "SioClientCommand.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static stringVector nameList;
static stringVector reservedList
    = { "command", "stop", "command_received", "reply", "clc", "available", "sioemit", "prompt" };
//=============================================================================
void
sioregister(const std::string& name, const std::string& function_name)
{
    nameList.push_back(name);
    SioClientCommand::getInstance()->sioregister(name, function_name);
}
//=============================================================================
void
siounregister(const std::string& name)
{
    stringVector::iterator itr = std::find(nameList.begin(), nameList.end(), name);
    if (itr != nameList.end()) {
        nameList.erase(itr);
    }
    SioClientCommand::getInstance()->siounregister(name);
}
//=============================================================================
stringVector
sioregisterList()
{
    stringVector list;
    list = nameList;
    list.insert(list.end(), reservedList.begin(), reservedList.end());
    return list;
}
//=============================================================================
bool
issioregistered(const std::string& name)
{
    stringVector::iterator itr = std::find(nameList.begin(), nameList.end(), name);
    return (itr != nameList.end());
}
//=============================================================================
bool
issioreserved(const std::string& name)
{
    stringVector::iterator itr = std::find(reservedList.begin(), reservedList.end(), name);
    return (itr != reservedList.end());
}
//=============================================================================
}
//=============================================================================
