//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__nelson_unittest_run_processes__Builtin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ProcessSupervisorBuiltinHelpers.hpp"
#include "TestProcessSupervisor.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
class SupervisorGuard
{
public:
    explicit SupervisorGuard(uint64 handle) : _handle(handle) { }
    ~SupervisorGuard() { Nelson::TestsManager::stopTestProcessSupervisor(_handle); }

private:
    uint64 _handle;
};
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::TestsManagerGateway::__nelson_unittest_run_processes__Builtin(
    int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    std::vector<TestsManager::TestProcessJob> jobs = processSupervisorJobsFromInput(argIn);
    size_t workerCount = processSupervisorWorkerCount(argIn, jobs.size());
    uint64 handle = TestsManager::startTestProcessSupervisor(jobs, workerCount);
    SupervisorGuard guard(handle);
    std::vector<TestsManager::TestProcessResult> results(jobs.size());
    size_t completed = 0;
    bool done = false;
    while (!done) {
        std::vector<TestsManager::TestProcessEvent> events
            = TestsManager::pollTestProcessSupervisor(handle, 25);
        for (const TestsManager::TestProcessEvent& event : events) {
            if (event.eventType == L"done") {
                done = true;
            } else if (event.eventType == L"process" && event.job.inputPosition < results.size()) {
                results[event.job.inputPosition] = event.result;
                ++completed;
            }
        }
        done = done || completed >= jobs.size();
    }
    ArrayOfVector retval;
    retval << processSupervisorResultsToStruct(jobs, results);
    return retval;
}
//=============================================================================
