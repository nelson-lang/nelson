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
#include "OverloadName.hpp"
#include "iskeywordBuiltin.hpp"
#include "max_recursion_depthBuiltin.hpp"
#include "parsefileBuiltin.hpp"
#include "parsestringBuiltin.hpp"
#include "classdefParseDumpBuiltin.hpp"
#include "classdefTokenizeBuiltin.hpp"
#include "codeAnalyzerBuiltin.hpp"
#include "bytecodeArtifactBuiltin.hpp"
#include "applicationEnvironmentBuiltin.hpp"
#include "smartindentBuiltin.hpp"
#include "onCleanupBuiltin.hpp"
#include "onCleanup_getBuiltin.hpp"
#include "onCleanup_deleteBuiltin.hpp"
#include "onCleanup_cancelBuiltin.hpp"
#include "onCleanup_usedBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"interpreter";
//=============================================================================
static const nlsGateway gateway[] = {
    { "__nelsonc_environment__",
        (ptrBuiltin)Nelson::InterpreterGateway::applicationEnvironmentBuiltin, 2, 2,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "__bytecode_artifact__", (ptrBuiltin)Nelson::InterpreterGateway::bytecodeArtifactBuiltin, 1,
        3, CPP_BUILTIN_WITH_EVALUATOR },
    { "parsefile", (ptrBuiltin)Nelson::InterpreterGateway::parsefileBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "parsestring", (ptrBuiltin)Nelson::InterpreterGateway::parsestringBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "__classdef_introspect__", (ptrBuiltin)Nelson::InterpreterGateway::classdefIntrospectBuiltin,
        1, 2, CPP_BUILTIN_WITH_EVALUATOR },
    { "max_recursion_depth", (ptrBuiltin)Nelson::InterpreterGateway::max_recursion_depthBuiltin, 1,
        1, CPP_BUILTIN_WITH_EVALUATOR },
    { "iskeyword", (ptrBuiltin)Nelson::InterpreterGateway::iskeywordBuiltin, 1, 1 },
    { "__code_analyzer__", (ptrBuiltin)Nelson::InterpreterGateway::codeAnalyzerBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "smartindent", (ptrBuiltin)Nelson::InterpreterGateway::smartindentBuiltin, 0, -1 },
    //=============================================================================
    { "onCleanup", (ptrBuiltin)Nelson::InterpreterGateway::onCleanupBuiltin, 1, 1,
        CPP_BUILTIN_WITH_EVALUATOR },
    // { "onCleanup_used", (ptrBuiltin)Nelson::InterpreterGateway::onCleanup_usedBuiltin, 1, 0 },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_ONCLEANUP_CATEGORY_STR, "get"),
        (ptrBuiltin)Nelson::InterpreterGateway::onCleanup_getBuiltin, 1, 2, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_ONCLEANUP_CATEGORY_STR, "delete"),
        (ptrBuiltin)Nelson::InterpreterGateway::onCleanup_deleteBuiltin, 0, 1,
        CPP_BUILTIN_WITH_EVALUATOR, NLS_OVERLOAD_AUTO_OFF },
    { OVERLOAD_FUNCTION_NAME(NLS_HANDLE_ONCLEANUP_CATEGORY_STR, "cancel"),
        (ptrBuiltin)Nelson::InterpreterGateway::onCleanup_cancelBuiltin, 0, 1, CPP_BUILTIN,
        NLS_OVERLOAD_AUTO_OFF },

    //=============================================================================
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
