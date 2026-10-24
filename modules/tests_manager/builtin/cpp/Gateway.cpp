//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NelsonGateway.hpp"
#if !defined(NELSON_TESTS_MANAGER_INLINE_ONLY)
#include "__nelson_unittest_cleanup_processes__Builtin.hpp"
#include "__nelson_unittest_run_processes__Builtin.hpp"
#include "__nelson_unittest_worker_emit__Builtin.hpp"
#include "__nelson_unittest_worker_receive__Builtin.hpp"
#include "TestProcessSupervisor.hpp"
#endif
#include "__nelson_unittest_parsetags__Builtin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"tests_manager";
//=============================================================================
static const nlsGateway gateway[] = {
#if !defined(NELSON_TESTS_MANAGER_INLINE_ONLY)
    { "__nelson_unittest_cleanup_processes__",
        (ptrBuiltin)Nelson::TestsManagerGateway::__nelson_unittest_cleanup_processes__Builtin, 1,
        1 },
#endif
    { "__nelson_unittest_parsetags__",
        (ptrBuiltin)Nelson::TestsManagerGateway::__nelson_unittest_parsetags__Builtin, -1, -1 },
#if !defined(NELSON_TESTS_MANAGER_INLINE_ONLY)
    { "__nelson_unittest_run_processes__",
        (ptrBuiltin)Nelson::TestsManagerGateway::__nelson_unittest_run_processes__Builtin, 1, 2 },
    { "__nelson_unittest_process_supervisor_start__",
        (ptrBuiltin)
            Nelson::TestsManagerGateway::__nelson_unittest_process_supervisor_start__Builtin,
        1, 2 },
    { "__nelson_unittest_process_supervisor_poll__",
        (ptrBuiltin)Nelson::TestsManagerGateway::__nelson_unittest_process_supervisor_poll__Builtin,
        1, 2 },
    { "__nelson_unittest_process_supervisor_stop__",
        (ptrBuiltin)Nelson::TestsManagerGateway::__nelson_unittest_process_supervisor_stop__Builtin,
        1, 1 },
    { "__nelson_unittest_worker_receive__",
        (ptrBuiltin)Nelson::TestsManagerGateway::__nelson_unittest_worker_receive__Builtin, 1, 0 },
    { "__nelson_unittest_worker_emit__",
        (ptrBuiltin)Nelson::TestsManagerGateway::__nelson_unittest_worker_emit__Builtin, 1, 1 },
#endif
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
static bool
finishTestsManagerModule(Nelson::Evaluator*)
{
#if !defined(NELSON_TESTS_MANAGER_INLINE_ONLY)
    Nelson::TestsManager::stopAllTestProcessSupervisors();
#endif
    return true;
}
//=============================================================================
NLS_GATEWAY_REMOVE_EXTENDED(gateway, (void*)finishTestsManagerModule)
//=============================================================================
