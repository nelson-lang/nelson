//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "EngineGatewayLifecycle.hpp"
#include "AddGateway.hpp"
#include "Evaluator.hpp"
#include "MainEvaluator.hpp"
#include "ModulesHelpers.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
#ifdef NELSON_STATIC_GATEWAYS
namespace Nelson {
bool
RegisterStaticGateways(Evaluator* evaluator);
bool
UnregisterStaticGateways(Evaluator* evaluator);
//=============================================================================
} // namespace Nelson
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
bool
registerEngineGateways(Evaluator* evaluator)
{
#ifdef NELSON_STATIC_GATEWAYS
#ifdef NELSON_WASM_PERSISTENT_SESSION
    static bool registered = false;
    if (registered) {
        return true;
    }
#endif
    const bool result = RegisterStaticGateways(evaluator);
#ifdef NELSON_WASM_PERSISTENT_SESSION
    registered = result;
#endif
    return result;
#else
    const auto& libraries = NelsonConfiguration::getInstance()->getNelsonLibraryDirectory();
    AddGateway(evaluator, ConstructDynamicLibraryFullname(libraries, L"core", true));
    AddGateway(evaluator, ConstructDynamicLibraryFullname(libraries, L"modules_manager", true));
    AddGateway(evaluator, ConstructDynamicLibraryFullname(libraries, L"string", true));
    return true;
#endif
}
//=============================================================================
void
prepareEngineEvaluator(Evaluator* evaluator)
{
#ifdef NELSON_WASM_PERSISTENT_SESSION
    evaluator->setExitCode(0);
#else
    (void)evaluator;
#endif
}
//=============================================================================
void
releaseEngineEvaluator(Evaluator* evaluator)
{
#ifdef NELSON_STATIC_GATEWAYS
#ifndef NELSON_WASM_PERSISTENT_SESSION
    UnregisterStaticGateways(evaluator);
#endif
#endif
#ifndef NELSON_WASM_PERSISTENT_SESSION
    destroyMainEvaluator();
#else
    (void)evaluator;
#endif
}
//=============================================================================
} // namespace Nelson
//=============================================================================
