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
#include "TestProcessSupervisor.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TestsManagerGateway::__nelson_unittest_process_supervisor_stop__Builtin(
    int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    uint64 handle = static_cast<uint64>(argIn[0].asDoubleScalar());
    TestsManager::stopTestProcessSupervisor(handle);
    ArrayOfVector retval;
    retval << ArrayOf::logicalScalar(true);
    return retval;
}
//=============================================================================
