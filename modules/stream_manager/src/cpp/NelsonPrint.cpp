//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonPrint.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
setPrintInterface(Interface* io)
{
    NelsonConfiguration::getInstance()->setMainIOInterface(io);
}
//=============================================================================
void
NelsonPrint(const std::wstring& msg)
{
    Interface* io = (Interface*)NelsonConfiguration::getInstance()->getMainIOInterface();
    if (io) {
        io->outputMessage(msg);
    }
}
//=============================================================================
void
NelsonPrint(const std::string& msg)
{
    Interface* io = (Interface*)NelsonConfiguration::getInstance()->getMainIOInterface();
    if (io) {
        io->outputMessage(msg);
    }
}
//=============================================================================
}
//=============================================================================
int
NelsonPrint(const wchar_t* msg)
{
    Nelson::Interface* io
        = (Nelson::Interface*)Nelson::NelsonConfiguration::getInstance()->getMainIOInterface();
    if (io) {
        io->outputMessage(msg);
        return (int)wcslen(msg);
    }
    return -1;
}
//=============================================================================
