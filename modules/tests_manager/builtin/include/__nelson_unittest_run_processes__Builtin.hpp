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
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson::TestsManagerGateway {
//=============================================================================
ArrayOfVector
__nelson_unittest_run_processes__Builtin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
__nelson_unittest_process_supervisor_start__Builtin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
__nelson_unittest_process_supervisor_poll__Builtin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
__nelson_unittest_process_supervisor_stop__Builtin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson::TestsManagerGateway
//=============================================================================
