//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <algorithm>
#include <functional>
#include "JuliaObjectHandle.hpp"
#include "JuliaTypesHelpers.hpp"
#include "JuliaHelpers.hpp"
#include "JuliaLibraryWrapper.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
JuliaObjectHandle::JuliaObjectHandle(void* _ptr)
    : HandleGenericObject(NLS_HANDLE_JULIA_CATEGORY_STR, _ptr, true)
{
}
//=============================================================================
JuliaObjectHandle::~JuliaObjectHandle() { }
//=============================================================================
void
JuliaObjectHandle::display(Interface* io)
{
    jl_value_t* jlObject = (jl_value_t*)this->getPointer();
    std::wstring msg = L"  [Julia Object]:";

    bool fails = false;
    std::string asString = jl_value_tRepresentation(
        jlObject, fails, io->getTerminalWidth(), io->getTerminalHeight());

    std::wstring rep;
    if (!fails) {
        rep = utf8_to_wstring(asString);
    }
    msg.append(L"\n");
    msg.append(L"\n");
    io->outputMessage(msg);
    io->outputMessage(std::wstring(L"    ") + rep + L"\n");
}
//=============================================================================
std::wstring
JuliaObjectHandle::getTypeName()
{
    return L"";
}
//=============================================================================
std::string
JuliaObjectHandle::getClassName()
{
    return NLS_HANDLE_JULIA_CATEGORY_STR;
}
//=============================================================================
}
//=============================================================================
