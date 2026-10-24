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
#include "nflow_codeGenerateBuiltin.hpp"
#include "nflowBuiltin.hpp"
#include "nflow_exportFmuBuiltin.hpp"
#include "nflow_simulateBuiltin.hpp"
#include "nflow_linearizeBuiltin.hpp"
#include "nflow_expandVectorBuiltin.hpp"
#include "new_systemBuiltin.hpp"
#include "load_systemBuiltin.hpp"
#include "save_systemBuiltin.hpp"
#include "close_systemBuiltin.hpp"
#include "bdcloseBuiltin.hpp"
#include "bdIsLoadedBuiltin.hpp"
#include "bdIsDirtyBuiltin.hpp"
#include "add_blockBuiltin.hpp"
#include "delete_blockBuiltin.hpp"
#include "add_lineBuiltin.hpp"
#include "delete_lineBuiltin.hpp"
#include "nflow_connectBlocksBuiltin.hpp"
#include "set_paramBuiltin.hpp"
#include "get_paramBuiltin.hpp"
#include "find_systemBuiltin.hpp"
#include "bdrootBuiltin.hpp"
#include "gcsBuiltin.hpp"
#include "gcbBuiltin.hpp"
#include "gcbhBuiltin.hpp"
#include "getNFlowBlockHandleBuiltin.hpp"
#include "getfullnameBuiltin.hpp"
#include "nflow_layoutBuiltin.hpp"
#include "nflow_lineNameBuiltin.hpp"
#include "nflow_rerouteBuiltin.hpp"
#include "NFlowNelsonBridge.hpp"
#include "SolverBackend.hpp"
#include "SundialsSolverBackend.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"nflow_engine";
//=============================================================================
static const nlsGateway gateway[] = {
    { "nflow", (ptrBuiltin)Nelson::NFlowGateway::nflowBuiltin, 0, 1, CPP_BUILTIN_WITH_EVALUATOR },
    { "__nflow_export_fmu_sources__",
        (ptrBuiltin)Nelson::NFlowGateway::nflow_exportFmuSourcesBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "nflow_codegenerate", (ptrBuiltin)Nelson::NFlowGateway::nflow_codeGenerateBuiltin, 1, 5,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "__nflow_codegen_support__", (ptrBuiltin)Nelson::NFlowGateway::nflow_codegenSupportBuiltin, 2,
        1, CPP_BUILTIN },
    { "__nflow_codegen_asymmetries__",
        (ptrBuiltin)Nelson::NFlowGateway::nflow_codegenAsymmetriesBuiltin, 1, 0, CPP_BUILTIN },
    { "__nflow_codegen_toolchain__",
        (ptrBuiltin)Nelson::NFlowGateway::nflow_codegenToolchainBuiltin, 1, 1, CPP_BUILTIN },
    { "__nflow_simulate__", (ptrBuiltin)Nelson::NFlowGateway::nflow_simulateBuiltin, 1, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "__nflow_linearize__", (ptrBuiltin)Nelson::NFlowGateway::nflow_linearizeBuiltin, 1, 3,
        CPP_BUILTIN },
    { "__nflow_expand_vector__", (ptrBuiltin)Nelson::NFlowGateway::nflow_expandVectorBuiltin, 1, 1,
        CPP_BUILTIN },
    { "new_system", (ptrBuiltin)Nelson::NFlowGateway::new_systemBuiltin, 1, 1, CPP_BUILTIN },
    { "load_system", (ptrBuiltin)Nelson::NFlowGateway::load_systemBuiltin, 1, 1, CPP_BUILTIN },
    { "save_system", (ptrBuiltin)Nelson::NFlowGateway::save_systemBuiltin, 1, 2, CPP_BUILTIN },
    { "close_system", (ptrBuiltin)Nelson::NFlowGateway::close_systemBuiltin, 0, 2, CPP_BUILTIN },
    { "bdclose", (ptrBuiltin)Nelson::NFlowGateway::bdcloseBuiltin, 0, 1, CPP_BUILTIN },
    { "bdIsLoaded", (ptrBuiltin)Nelson::NFlowGateway::bdIsLoadedBuiltin, 1, 1, CPP_BUILTIN },
    { "bdIsDirty", (ptrBuiltin)Nelson::NFlowGateway::bdIsDirtyBuiltin, 1, 1, CPP_BUILTIN },
    { "add_block", (ptrBuiltin)Nelson::NFlowGateway::add_blockBuiltin, 1, -1, CPP_BUILTIN },
    { "delete_block", (ptrBuiltin)Nelson::NFlowGateway::delete_blockBuiltin, 0, 1, CPP_BUILTIN },
    { "add_line", (ptrBuiltin)Nelson::NFlowGateway::add_lineBuiltin, 1, -1, CPP_BUILTIN },
    { "delete_line", (ptrBuiltin)Nelson::NFlowGateway::delete_lineBuiltin, 0, 3, CPP_BUILTIN },
    { "__nflow_connectBlocks__", (ptrBuiltin)Nelson::NFlowGateway::nflow_connectBlocksBuiltin, 1,
        -1, CPP_BUILTIN },
    { "set_param", (ptrBuiltin)Nelson::NFlowGateway::set_paramBuiltin, 0, -1, CPP_BUILTIN },
    { "get_param", (ptrBuiltin)Nelson::NFlowGateway::get_paramBuiltin, 1, 2, CPP_BUILTIN },
    { "find_system", (ptrBuiltin)Nelson::NFlowGateway::find_systemBuiltin, 1, -1, CPP_BUILTIN },
    { "bdroot", (ptrBuiltin)Nelson::NFlowGateway::bdrootBuiltin, 1, 1, CPP_BUILTIN },
    { "gcs", (ptrBuiltin)Nelson::NFlowGateway::gcsBuiltin, 1, 0, CPP_BUILTIN },
    { "gcb", (ptrBuiltin)Nelson::NFlowGateway::gcbBuiltin, 1, 0, CPP_BUILTIN },
    { "gcbh", (ptrBuiltin)Nelson::NFlowGateway::gcbhBuiltin, 1, 0, CPP_BUILTIN },
    { "getNFlowBlockHandle", (ptrBuiltin)Nelson::NFlowGateway::getNFlowBlockHandleBuiltin, 1, 2,
        CPP_BUILTIN },
    { "getfullname", (ptrBuiltin)Nelson::NFlowGateway::getfullnameBuiltin, 1, 1, CPP_BUILTIN },
    { "nflow_layout", (ptrBuiltin)Nelson::NFlowGateway::nflow_layoutBuiltin, 0, 1, CPP_BUILTIN },
    { "nflow_lineName", (ptrBuiltin)Nelson::NFlowGateway::nflow_lineNameBuiltin, 1, 4,
        CPP_BUILTIN },
    { "nflow_reroute", (ptrBuiltin)Nelson::NFlowGateway::nflow_rerouteBuiltin, 0, 3, CPP_BUILTIN },
};
//=============================================================================
static bool
initializeNFlowEngineModule(Nelson::Evaluator*)
{
    // Registers the SUNDIALS backends of ode_solvers (CVODES variable-step, IDAS 'dae')
    // in the engine's solver-backend registry. The dependency points NFlow -> ode_solvers,
    // so ode_solvers never links nflow_blocks and loads in a math-only install. This
    // runs from the gateway initializer, not a file-static, so the registration
    // survives dead-code elimination in a static build (WebAssembly).
    Nelson::NFlow::registerSundialsSolverBackends(&Nelson::NFlow::registerSolverBackend);
    return true;
}
//=============================================================================
NLS_GATEWAY_FUNC_EXTENDED(gateway, (void*)initializeNFlowEngineModule)
//=============================================================================
static bool
finishNFlowEngineModule(Nelson::Evaluator*)
{
    Nelson::NFlowGateway::clearNelsonWorkspaceBridge();
    return true;
}
//=============================================================================
NLS_GATEWAY_REMOVE_EXTENDED(gateway, (void*)finishNFlowEngineModule)
//=============================================================================
