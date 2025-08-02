//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonInterprocess.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
sendVariableToNelsonInterprocessReceiver(int pidDestination, const ArrayOf& var,
    const std::wstring& name, const std::wstring& scope, bool withEventsLoop,
    std::wstring& errorMessage)
{
    return false;
}
//=============================================================================
bool
createNelsonInterprocessReceiver(int pid, bool withEventsLoop)
{
    return false;
}
//=============================================================================
bool
isMinimizedFromNelsonInterprocessReceiver(
    int pidDestination, bool withEventsLoop, std::wstring& errorMessage)
{
    return false;
}
//=============================================================================
bool
removeNelsonInterprocessReceiver(int pid, bool withEventsLoop)
{
    return false;
}
//=============================================================================
bool
evalCommandToNelsonInterprocessReceiver(int pidDestination, const std::wstring& command,
    bool withEventsLoop, std::wstring& result, std::wstring& errorMessage)
{

    return false;
}
//=============================================================================
ArrayOf
getVariableFromNelsonInterprocessReceiver(int pidDestination, const std::wstring& name,
    const std::wstring& scope, bool withEventsLoop, std::wstring& errorMessage)
{
    return {};
}
//=============================================================================
bool
sendMinimizeToNelsonInterprocessReceiver(
    int pidDestination, bool minimize, bool withEventsLoop, std::wstring& errorMessage)
{
    return false;
}
//=============================================================================
}
//=============================================================================
