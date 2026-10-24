//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <atomic>
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "FunctionDef.hpp"
#include "NelsonGateway.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
using LazyGatewayResolver = bool (*)(Evaluator* eval, const std::wstring& dynlibname,
    const std::string& functionName, std::wstring& errorMessage);
//=============================================================================
NLSINTERPRETER_IMPEXP void
SetLazyGatewayResolver(LazyGatewayResolver resolver);
//=============================================================================
// Callbacks run once, before the builtin/gateway tables are torn down at
// shutdown. Modules that keep worker threads alive (e.g. the background pool
// used by parfeval) register here to drain those threads while the function
// definitions they may still dereference are alive.
using PreModulesTeardownCallback = void (*)();
NLSINTERPRETER_IMPEXP void
RegisterPreModulesTeardownCallback(PreModulesTeardownCallback callback);
NLSINTERPRETER_IMPEXP void
RunPreModulesTeardownCallbacks();
//=============================================================================
class NLSINTERPRETER_IMPEXP BuiltInFunctionDef : public FunctionDef
{
public:
    //=============================================================================
    /**
     * Location of the function's defining dynamic library.
     */
    std::wstring fileName;
    //=============================================================================
    /**
     * The number of return args for this function (-1 for variable).
     */
    int retCount;
    //=============================================================================
    /** The number of input args for this function (-1 for variable).
     */
    int argCount;
    //=============================================================================
    void* fptr;
    //=============================================================================
    size_t builtinPrototype;
    //=============================================================================
    /**
     * separated or interleaved complex
     * Nelson uses natively interleaved complex representation
     * MEX before 2018 version uses separated complex representation
     */
    bool interleavedComplex;
    bool callerContextAccess;
    // evaluator builtins are conservatively assumed to reach the calling
    // workspace; helpers that only use the evaluator to run class member
    // functions opt out so call sites skip the locals-to-context sync
    bool callerContextNeutral;
    std::atomic<bool> lazyGateway;
    std::wstring moduleName;
    //=============================================================================
    BuiltInFunctionDef(bool isOverload);
    //=============================================================================
    ~BuiltInFunctionDef() override;
    //=============================================================================
    [[nodiscard]] FunctionType
    type() const override
    {
        return NLS_BUILT_IN_FUNCTION;
    }
    //=============================================================================
    int
    inputArgCount() override
    {
        return argCount;
    }
    //=============================================================================
    int
    outputArgCount() override
    {
        return retCount;
    }
    //=============================================================================
    ArrayOfVector
    evaluateFunction(
        Evaluator* /*unused*/, const ArrayOfVector& /*unused*/, int /*unused*/) override;
    //=============================================================================
    bool
    updateCode() override
    {
        return false;
    }
    //=============================================================================
    bool
    mustSyncCallerContext() const
    {
        return (builtinPrototype == CPP_BUILTIN_WITH_EVALUATOR && !callerContextNeutral)
            || callerContextAccess;
    }
    //=============================================================================
    bool
    mustRefreshCallerContext() const
    {
        return callerContextAccess;
    }
    //=============================================================================
    bool
    isLazyGateway() const
    {
        return lazyGateway;
    }
    //=============================================================================
    // "built-in <name>" computed once: pushDebug runs on every builtin call.
    const std::string&
    getDebugDetail()
    {
        if (debugDetailCache.empty()) {
            debugDetailCache = std::string("built-in ") + getName();
        }
        return debugDetailCache;
    }
    //=============================================================================
private:
    std::string debugDetailCache;
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
