//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstdlib>
#include <map>
#include <mutex>
#include <vector>
#include "terminalBuiltin.hpp"
#include "Error.hpp"
#include "GuiDialogProvider.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson::WebviewGateway {
//=============================================================================
// A scriptable terminal is a shell panel in the desktop, driven from Nelson
// code. The panel runs each command line (reusing the desktop's own shell
// execution), so the back end only tracks the session's properties and relays
// create / write / read / clear / focus / close through the desktop dialog seam
// (runGuiDialog). There is no long-lived shell process here: ProcessId is 0 and
// Running reflects whether the panel is still open.
//=============================================================================
namespace {
    //=============================================================================
    struct TerminalSession
    {
        std::wstring name;
        std::wstring shell;
        std::wstring theme = L"auto";
        std::wstring windowStyle = L"docked";
    };
    //=============================================================================
    std::mutex&
    sessionsMutex()
    {
        static std::mutex m;
        return m;
    }
    //=============================================================================
    std::map<long long, TerminalSession>&
    sessions()
    {
        static std::map<long long, TerminalSession> s;
        return s;
    }
    //=============================================================================
    long long&
    handleCounter()
    {
        static long long counter = 0;
        return counter;
    }
    //=============================================================================
    std::wstring
    defaultShell()
    {
#ifdef _WIN32
        return L"cmd.exe";
#else
        const char* sh = std::getenv("SHELL");
        if (sh != nullptr && *sh != '\0') {
            return utf8_to_wstring(sh);
        }
        return L"/bin/sh";
#endif
    }
    //=============================================================================
    wstringVector
    supportedThemes()
    {
        return { L"auto", L"light", L"dark" };
    }
    //=============================================================================
    std::wstring
    getTextArgument(const ArrayOf& value)
    {
        if (!(value.isRowVectorCharacterArray() || value.isScalarStringArray())) {
            RaiseError(_W("Argument must be a character vector or string scalar."));
        }
        return value.asWideString();
    }
    //=============================================================================
    long long
    getHandle(const ArrayOf& value)
    {
        return (long long)value.asDoubleScalar();
    }
    //=============================================================================
    void
    requireWebDesktop()
    {
        if (!hasGuiDialogProvider()) {
            RaiseError(
                _W("terminal is only available in the desktop."), L"Nelson:terminal:noWebDesktop");
        }
    }
    //=============================================================================
    // Fire-and-forget request to the desktop (open / input / clear / focus / close /
    // props). Carried on the dialog seam: progressId = handle, message = command
    // text, title = name, defaultPath = theme.
    void
    pushTerminal(const std::wstring& kind, long long handle, const std::wstring& message,
        const std::wstring& title, const std::wstring& defaultPath)
    {
        GuiDialogRequest request;
        request.kind = kind;
        request.progressId = handle;
        request.message = message;
        request.title = title;
        request.defaultPath = defaultPath;
        GuiDialogResponse response;
        runGuiDialog(request, response);
    }
    //=============================================================================
    // Blocking request: ask the panel for its current transcript.
    std::wstring
    readTerminal(long long handle)
    {
        GuiDialogRequest request;
        request.kind = L"terminalread";
        request.progressId = handle;
        GuiDialogResponse response;
        if (!runGuiDialog(request, response)) {
            return L"";
        }
        if (!response.values.empty()) {
            return response.values[0];
        }
        return L"";
    }
    //=============================================================================
    void
    readOption(const ArrayOf& options, const std::string& fieldName, std::wstring& destination)
    {
        if (options.isField(fieldName)) {
            destination = getTextArgument(options.field(fieldName));
        }
    }
    //=============================================================================
    ArrayOf
    createTerminal(const ArrayOf& optionsArg)
    {
        requireWebDesktop();
        if (!optionsArg.isStruct() || !optionsArg.isScalar()) {
            RaiseError(_W("Terminal options must be a scalar structure."),
                L"Nelson:terminal:invalidOptions");
        }
        TerminalSession session;
        session.shell = defaultShell();
        readOption(optionsArg, "Name", session.name);
        readOption(optionsArg, "Shell", session.shell);
        readOption(optionsArg, "WindowStyle", session.windowStyle);
        readOption(optionsArg, "Theme", session.theme);
        std::wstring startupCommand;
        readOption(optionsArg, "StartupCommand", startupCommand);
        // The classdef passes every option field, empty ones included, so re-apply
        // the defaults the empty values would otherwise wipe.
        if (session.shell.empty()) {
            session.shell = defaultShell();
        }
        if (session.windowStyle.empty()) {
            session.windowStyle = L"docked";
        }
        if (session.theme.empty()) {
            session.theme = L"auto";
        }
        long long handle = ++handleCounter();
        {
            std::lock_guard<std::mutex> lock(sessionsMutex());
            sessions()[handle] = session;
        }
        // windowStyle rides in the message slot (unused for open).
        pushTerminal(L"terminalopen", handle, session.windowStyle, session.name, session.theme);
        if (!startupCommand.empty()) {
            pushTerminal(L"terminalinput", handle, startupCommand, L"", L"");
        }
        return ArrayOf::doubleScalar((double)handle);
    }
    //=============================================================================
    ArrayOf
    getProperty(long long handle, const std::wstring& name)
    {
        TerminalSession session;
        bool open = false;
        {
            std::lock_guard<std::mutex> lock(sessionsMutex());
            auto it = sessions().find(handle);
            if (it != sessions().end()) {
                session = it->second;
                open = true;
            }
        }
        if (name == L"Name") {
            return ArrayOf::characterArray(session.name);
        }
        if (name == L"Shell") {
            return ArrayOf::characterArray(session.shell);
        }
        if (name == L"Place") {
            return ArrayOf::characterArray(L"nelson");
        }
        if (name == L"WindowStyle") {
            return ArrayOf::characterArray(session.windowStyle);
        }
        if (name == L"Theme") {
            return ArrayOf::characterArray(session.theme);
        }
        if (name == L"Running") {
            return ArrayOf::logicalScalar(open);
        }
        if (name == L"ExitCode") {
            return ArrayOf::doubleScalar(0.0);
        }
        if (name == L"ProcessId") {
            return ArrayOf::doubleScalar(0.0);
        }
        RaiseError(_W("Unknown terminal property."), L"Nelson:terminal:unknownProperty");
        return {};
    }
    //=============================================================================
    void
    setProperty(long long handle, const std::wstring& name, const ArrayOf& value)
    {
        const std::wstring text = getTextArgument(value);
        std::lock_guard<std::mutex> lock(sessionsMutex());
        auto it = sessions().find(handle);
        if (it == sessions().end()) {
            return;
        }
        if (name == L"Name") {
            it->second.name = text;
            pushTerminal(L"terminalprops", handle, L"", text, L"");
        } else if (name == L"Theme") {
            it->second.theme = text;
            pushTerminal(L"terminalprops", handle, L"", L"", text);
        } else if (name == L"Shell") {
            it->second.shell = text;
        } else if (name == L"WindowStyle") {
            it->second.windowStyle = text;
        }
    }
    //=============================================================================
    ArrayOf
    listTerminals()
    {
        std::vector<double> handles;
        {
            std::lock_guard<std::mutex> lock(sessionsMutex());
            for (const auto& entry : sessions()) {
                handles.push_back((double)entry.first);
            }
        }
        if (handles.empty()) {
            return ArrayOf::empty();
        }
        return ArrayOf::doubleRowVector(handles);
    }
    //=============================================================================
    void
    closeTerminal(long long handle)
    {
        {
            std::lock_guard<std::mutex> lock(sessionsMutex());
            sessions().erase(handle);
        }
        pushTerminal(L"terminalclose", handle, L"", L"", L"");
    }
    //=============================================================================
    void
    closeAllTerminals()
    {
        std::vector<long long> handles;
        {
            std::lock_guard<std::mutex> lock(sessionsMutex());
            for (const auto& entry : sessions()) {
                handles.push_back(entry.first);
            }
            sessions().clear();
        }
        for (long long handle : handles) {
            pushTerminal(L"terminalclose", handle, L"", L"", L"");
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
terminalBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1);
    const std::wstring action = getTextArgument(argIn[0]);
    ArrayOfVector retval;
    if (action == L"create") {
        nargincheck(argIn, 2, 2);
        nargoutcheck(nLhs, 0, 1);
        retval << createTerminal(argIn[1]);
        return retval;
    }
    if (action == L"write") {
        nargincheck(argIn, 3, 3);
        nargoutcheck(nLhs, 0, 0);
        requireWebDesktop();
        pushTerminal(L"terminalinput", getHandle(argIn[1]), getTextArgument(argIn[2]), L"", L"");
        return retval;
    }
    if (action == L"read") {
        nargincheck(argIn, 2, 2);
        nargoutcheck(nLhs, 0, 1);
        requireWebDesktop();
        retval << ArrayOf::characterArray(readTerminal(getHandle(argIn[1])));
        return retval;
    }
    if (action == L"clear") {
        nargincheck(argIn, 2, 2);
        nargoutcheck(nLhs, 0, 0);
        pushTerminal(L"terminalclear", getHandle(argIn[1]), L"", L"", L"");
        return retval;
    }
    if (action == L"focus") {
        nargincheck(argIn, 2, 2);
        nargoutcheck(nLhs, 0, 0);
        pushTerminal(L"terminalfocus", getHandle(argIn[1]), L"", L"", L"");
        return retval;
    }
    if (action == L"close") {
        nargincheck(argIn, 2, 2);
        nargoutcheck(nLhs, 0, 0);
        closeTerminal(getHandle(argIn[1]));
        return retval;
    }
    if (action == L"get") {
        nargincheck(argIn, 3, 3);
        nargoutcheck(nLhs, 0, 1);
        retval << getProperty(getHandle(argIn[1]), getTextArgument(argIn[2]));
        return retval;
    }
    if (action == L"set") {
        nargincheck(argIn, 4, 4);
        nargoutcheck(nLhs, 0, 0);
        setProperty(getHandle(argIn[1]), getTextArgument(argIn[2]), argIn[3]);
        return retval;
    }
    if (action == L"list") {
        nargincheck(argIn, 1, 1);
        nargoutcheck(nLhs, 0, 1);
        retval << listTerminals();
        return retval;
    }
    if (action == L"closeAll") {
        nargincheck(argIn, 1, 1);
        nargoutcheck(nLhs, 0, 0);
        closeAllTerminals();
        return retval;
    }
    if (action == L"themes") {
        nargincheck(argIn, 1, 1);
        nargoutcheck(nLhs, 0, 1);
        retval << ArrayOf::cellArrayFromArrayOfCharacterRowVectors(supportedThemes());
        return retval;
    }
    if (action == L"version") {
        nargincheck(argIn, 1, 1);
        nargoutcheck(nLhs, 0, 1);
        retval << ArrayOf::characterArray(L"1.0.0");
        return retval;
    }
    RaiseError(_W("Unsupported terminal action."), L"Nelson:terminal:unsupportedAction");
    return retval;
}
//=============================================================================
} // namespace Nelson::WebviewGateway
//=============================================================================
