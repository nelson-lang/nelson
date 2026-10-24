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
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ProcessSupervisorBuiltinHelpers.hpp"
#include "TestProcessSupervisor.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TestsManagerGateway::__nelson_unittest_process_supervisor_poll__Builtin(
    int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    uint64 handle = static_cast<uint64>(argIn[0].asDoubleScalar());
    uint64 timeoutMs = 25;
    if (argIn.size() == 2) {
        double timeout = argIn[1].asDoubleScalar();
        if (timeout < 0) {
            Error(
                L"Nelson:tests_manager:invalidPollTimeout", L"poll timeout must be non-negative.");
        }
        timeoutMs = static_cast<uint64>(timeout);
    }
    ArrayOfVector retval;
    retval << processSupervisorEventsToStruct(
        TestsManager::pollTestProcessSupervisor(handle, timeoutMs));
    return retval;
}
//=============================================================================
