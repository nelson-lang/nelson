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
#include <memory>
#include <string>
#include "Types.hpp"
//=============================================================================
namespace Nelson::TestsManager {
//=============================================================================
class TestChildProcess
{
public:
    TestChildProcess();
    ~TestChildProcess();
    TestChildProcess(const TestChildProcess&) = delete;
    TestChildProcess&
    operator=(const TestChildProcess&)
        = delete;

    bool
    start(const std::wstring& executable, const std::wstring& arguments, bool pipeInput,
        std::wstring& error);
    bool
    writeLine(const std::string& value);
    void
    closeInput();
    void
    readAvailable(std::string& output, std::string& error);
    bool
    wait(uint64 timeoutMs, int& exitCode);
    void
    terminate(uint64 graceMs = 100);
    uint64
    pid() const;
    std::wstring
    policy() const;

private:
    struct Impl;
    std::unique_ptr<Impl> _impl;
};
//=============================================================================
} // namespace Nelson::TestsManager
//=============================================================================
