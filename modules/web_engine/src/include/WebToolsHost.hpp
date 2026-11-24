//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "nlsWeb_engine_exports.h"
#include <functional>
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
// Web tools are the HTML applications a module ships (the examples gallery,
// the package manager, ...). On the desktop each one opens its own WebUI
// window; a host that embeds them (the desktop) registers itself here so
// the tool is shown as a docked panel instead, talks to the tool through its
// dispatcher and receives the tool's events.
//
// A tool dispatcher is a native library exporting
//   int symbol(const char* method, const char* paramsJson,
//              void (*sink)(void* context, const char* responseJson), void* context)
// handing the sink the JSON response ({"result": ...} or {"error": {"message": ...}}).
// Modules register it from etc/startup.m through
// __webview__('registerWebTool', name, libraryPath, symbol); the library is
// loaded on the first call, so a lazily loaded module costs nothing before.
NLSWEB_ENGINE_IMPEXP void
RegisterWebTool(
    const std::string& tool, const std::wstring& libraryPath, const std::string& symbol);
NLSWEB_ENGINE_IMPEXP bool
UnregisterWebTool(const std::string& tool);
NLSWEB_ENGINE_IMPEXP bool
IsWebToolRegistered(const std::string& tool);
// Response JSON of the tool for a method ({"error": ...} when the tool is
// unknown or its library cannot be loaded).
NLSWEB_ENGINE_IMPEXP std::string
DispatchWebTool(const std::string& tool, const std::string& method, const std::string& paramsJson);
//=============================================================================
struct WebToolHost
{
    // Shows the tool in the host; argsJson carries what the builtin was
    // called with (a deep link for instance). Returns false when the host
    // cannot show it, so the tool opens its own window.
    std::function<bool(const std::string& tool, const std::string& argsJson)> show;
    std::function<void(
        const std::string& tool, const std::string& channel, const std::string& payloadJson)>
        push;
};
//=============================================================================
// Empty functions clear the host.
NLSWEB_ENGINE_IMPEXP void
SetWebToolHost(const WebToolHost& host);
NLSWEB_ENGINE_IMPEXP bool
ShowWebToolInHost(const std::string& tool, const std::string& argsJson = "{}");
// False when no host receives events: the tool then targets its own window.
NLSWEB_ENGINE_IMPEXP bool
PushWebToolEvent(
    const std::string& tool, const std::string& channel, const std::string& payloadJson);
//=============================================================================
} // namespace Nelson
//=============================================================================
