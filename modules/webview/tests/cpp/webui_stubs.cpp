//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// WebUiHelpers.cpp links the webui runtime, the configuration singleton and
// the encoding helpers for window icon handling; the helper tests only
// exercise pure path helpers, so these stand-ins satisfy the linker.
#include <cstddef>
#include <string>
#include "NelsonConfiguration.hpp"
#include "characters_encoding.hpp"
//=============================================================================
extern "C" void*
webui_get_hwnd(size_t window)
{
    (void)window;
    return nullptr;
}
//=============================================================================
extern "C" void
webui_set_native_icon_file(size_t window, const char* path)
{
    (void)window;
    (void)path;
}
//=============================================================================
extern "C" void
webui_set_native_title(size_t window, const char* title)
{
    (void)window;
    (void)title;
}
//=============================================================================
namespace Nelson {
//=============================================================================
std::string
wstring_to_utf8(const std::wstring& str)
{
    return std::string(str.begin(), str.end());
}
//=============================================================================
// Never reached by the icon-path tests, which do not open a window.
NelsonConfiguration*
NelsonConfiguration::getInstance()
{
    return nullptr;
}
//=============================================================================
std::wstring
NelsonConfiguration::getNelsonRootDirectory()
{
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
