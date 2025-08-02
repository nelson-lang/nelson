//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
    auto itr = std::find(nameList.begin(), nameList.end(), name);
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
    auto itr = std::find(nameList.begin(), nameList.end(), name);
    return (itr != nameList.end());
}
//=============================================================================
bool
issioreserved(const std::string& name)
{
    auto itr = std::find(reservedList.begin(), reservedList.end(), name);
    return (itr != reservedList.end());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
