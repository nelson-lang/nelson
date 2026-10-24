//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <functional>
#include <memory>
#include <string>
#include <string_view>
#include <vector>
#include "nlsEngine_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
struct NLSENGINE_IMPEXP PortableResources
{
    std::wstring rootDirectory;
    std::wstring modulesDirectory;
    std::wstring libraryDirectory;
    std::wstring binaryDirectory;
};
//=============================================================================
struct NLSENGINE_IMPEXP PortableConfiguration
{
    std::wstring language;
    std::vector<std::wstring> commandLineArguments;
    PortableResources resources;
    std::function<void(std::string_view)> stdoutCallback;
    std::function<void(std::string_view)> stderrCallback;
    std::function<void()> clearTerminalCallback;
    std::function<void(std::string_view)> htmlCallback;
    std::function<void(long long, std::string_view)> expandableTextCallback;
    std::function<void()> figureCallback;
};
//=============================================================================
struct NLSENGINE_IMPEXP EvaluationResult
{
    bool succeeded = false;
    bool interrupted = false;
    std::string stdoutText;
    std::string stderrText;
    std::string errorIdentifier;
    std::string errorMessage;
};
//=============================================================================
struct NLSENGINE_IMPEXP PortableDebuggerState
{
    bool running = false;
    std::string currentFile;
    size_t currentLine = 0;
    std::string currentFunction;
};
//=============================================================================
class NLSENGINE_IMPEXP PortableEngine
{
public:
    PortableEngine();
    ~PortableEngine();

    PortableEngine(const PortableEngine&) = delete;
    PortableEngine&
    operator=(const PortableEngine&)
        = delete;

    bool
    start(const PortableConfiguration& configuration = {});
    bool
    loadUserModules();
    EvaluationResult
    evaluate(std::string_view command);
    std::string
    debuggerStateJson() const;
    std::string
    debuggerGetBreakpointsJson(std::string_view file) const;
    std::string
    debuggerToggleBreakpointJson(std::string_view file, size_t line);
    void
    requestInterrupt();
    void
    pump();
    void
    reset();
    void
    stop();
    bool
    isStarted() const;

private:
    class Impl;
    std::unique_ptr<Impl> impl;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
