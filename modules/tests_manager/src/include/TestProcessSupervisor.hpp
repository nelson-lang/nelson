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
#include <string>
#include <vector>
#include "nlsTests_manager_exports.h"
#include "Types.hpp"
//=============================================================================
namespace Nelson::TestsManager {
//=============================================================================
struct TestProcessJob
{
    std::wstring executable;
    std::wstring arguments;
    uint64 timeout = 0;
    std::wstring kind;
    double index = 0;
    std::wstring metadata;
    std::wstring jobId;
    double scheduleOrder = 0;
    double weight = 1;
    bool reusable = false;
    std::wstring payload;
    std::wstring launcher;
    std::wstring resultFile;
    double attempt = 1;
    size_t inputPosition = 0;
};
//=============================================================================
struct TestProcessResult
{
    int exitCode = -1;
    bool timedOut = false;
    uint64 duration = 0;
    uint64 pid = 0;
    std::wstring reason = L"launch_error";
    std::wstring processPolicy;
    std::wstring output;
    std::wstring error;
};
//=============================================================================
struct TestProcessEvent
{
    std::wstring eventType;
    TestProcessResult result;
    TestProcessJob job;
    std::wstring payload;
    uint64 sequence = 0;
    uint64 workerId = 0;
};
//=============================================================================
NLSTESTS_MANAGER_IMPEXP uint64
startTestProcessSupervisor(const std::vector<TestProcessJob>& jobs, size_t workerCount);
//=============================================================================
NLSTESTS_MANAGER_IMPEXP std::vector<TestProcessEvent>
pollTestProcessSupervisor(uint64 handle, uint64 timeoutMs);
//=============================================================================
NLSTESTS_MANAGER_IMPEXP void
stopTestProcessSupervisor(uint64 handle);
//=============================================================================
NLSTESTS_MANAGER_IMPEXP uint64
stopAllTestProcessSupervisors();
//=============================================================================
} // namespace Nelson::TestsManager
//=============================================================================
