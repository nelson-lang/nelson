//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
