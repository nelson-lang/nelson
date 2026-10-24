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
ArrayOfVector
Nelson::TestsManagerGateway::__nelson_unittest_process_supervisor_start__Builtin(
    int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    std::vector<TestsManager::TestProcessJob> jobs = processSupervisorJobsFromInput(argIn);
    size_t workerCount = processSupervisorWorkerCount(argIn, jobs.size());
    uint64 handle = TestsManager::startTestProcessSupervisor(jobs, workerCount);
    ArrayOfVector retval;
    retval << ArrayOf::doubleScalar(static_cast<double>(handle));
    return retval;
}
//=============================================================================
