//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "PortableEngine.hpp"
#include "characters_encoding.hpp"
#include <iostream>
#include <string>
//=============================================================================
namespace {
bool
check(bool condition, const char* message)
{
    if (!condition) {
        std::cerr << message << '\n';
    }
    return condition;
}
//=============================================================================
} // namespace
//=============================================================================
int
main(int argc, char* argv[])
{
    std::string callbackOutput;
    std::string callbackError;
    Nelson::PortableConfiguration configuration;
    if (argc != 3) {
        std::cerr << "expected resource root and library directory\n";
        return 1;
    }
    const std::wstring root = Nelson::utf8_to_wstring(argv[1]);
    const std::wstring libraries = Nelson::utf8_to_wstring(argv[2]);
    configuration.resources.rootDirectory = root;
    configuration.resources.modulesDirectory = root + L"/modules";
    configuration.resources.libraryDirectory = libraries;
    configuration.resources.binaryDirectory = libraries;
    configuration.stdoutCallback
        = [&](std::string_view text) { callbackOutput.append(text.data(), text.size()); };
    configuration.stderrCallback
        = [&](std::string_view text) { callbackError.append(text.data(), text.size()); };
    Nelson::PortableEngine engine;
    if (!engine.start(configuration)) {
        std::cerr << "cannot start portable engine: " << callbackError << '\n';
        return 1;
    }
    auto assignment = engine.evaluate("answer = 41;");
    auto display = engine.evaluate("disp(answer + 1);");
    bool succeeded = check(assignment.succeeded, "assignment failed")
        && check(display.succeeded, "display failed")
        && check(display.stdoutText.find("42") != std::string::npos, "captured output is missing")
        && check(callbackOutput.find("42") != std::string::npos, "output callback is missing");
    engine.reset();
    auto afterReset = engine.evaluate("answer");
    succeeded = check(!afterReset.succeeded, "reset preserved the workspace") && succeeded;
    engine.stop();
    succeeded = check(engine.start(configuration), "cannot restart portable engine") && succeeded;
    auto restarted = engine.evaluate("disp(6 * 7);");
    succeeded = check(restarted.succeeded, "evaluation after restart failed")
        && check(
            restarted.stdoutText.find("42") != std::string::npos, "output after restart is missing")
        && succeeded;
    engine.stop();
    return succeeded ? 0 : 1;
}
//=============================================================================
