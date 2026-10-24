//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__nelson_unittest_cleanup_processes__Builtin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "TestProcessSupervisor.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TestsManagerGateway::__nelson_unittest_cleanup_processes__Builtin(
    int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    bool terminateRunning = false;
    if (!argIn.empty()) {
        terminateRunning = argIn[0].asLogicalScalar();
    }
    uint64 cleaned = terminateRunning ? TestsManager::stopAllTestProcessSupervisors() : 0;
    ArrayOfVector retval;
    retval << ArrayOf::doubleScalar(static_cast<double>(cleaned));
    return retval;
}
//=============================================================================
