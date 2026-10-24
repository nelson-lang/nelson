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
#include <string>
#include "Evaluator.hpp"
#include "GatewayHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
using ptrBuiltin = void*;
//=============================================================================
enum BUILTIN_PROTOTYPE
{
    CPP_BUILTIN = 0,
    CPP_BUILTIN_WITH_EVALUATOR = 1,
    C_MEX_BUILTIN = 2
};
//=============================================================================
using nlsGateway = struct nlsGatewayStructType
{
    const char* functionName;
    ptrBuiltin fptr;
    int nLhs;
    int nRhs;
    BUILTIN_PROTOTYPE builtinPrototype;
    FunctionOverloadAutoMode builtinOverloadAutoMode;
};
//=============================================================================
using nlsGatewayDescriptor = struct nlsGatewayDescriptorStructType
{
    const wchar_t* gatewayName;
    const nlsGateway* gateway;
    size_t nbBuiltins;
    bool hasInitializer;
    bool hasFinalizer;
    // The initializer has effects consumed outside this gateway's own builtins
    // (class registration, IPC receiver, ...): the library must be loaded at
    // startup instead of on the first builtin call.
    bool requiresEagerLoad;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
#ifdef _MSC_VER
#define EXPORTSYMBOL __declspec(dllexport)
#define EXTERN_AS_C extern "C"
#else
#define EXPORTSYMBOL __attribute__((visibility("default")))
#define EXTERN_AS_C extern "C"
#endif
//=============================================================================
#define NLS_GATEWAY_CONCAT_IMPL(left, right) left##right
#define NLS_GATEWAY_CONCAT(left, right) NLS_GATEWAY_CONCAT_IMPL(left, right)
#ifdef NELSON_STATIC_GATEWAY_ID
#define NLS_GATEWAY_ADD_SYMBOL NLS_GATEWAY_CONCAT(AddGateway_, NELSON_STATIC_GATEWAY_ID)
#define NLS_GATEWAY_REMOVE_SYMBOL NLS_GATEWAY_CONCAT(RemoveGateway_, NELSON_STATIC_GATEWAY_ID)
#define NLS_GATEWAY_DESCRIPTOR_SYMBOL                                                              \
    NLS_GATEWAY_CONCAT(GetGatewayDescriptor_, NELSON_STATIC_GATEWAY_ID)
#else
#define NLS_GATEWAY_ADD_SYMBOL AddGateway
#define NLS_GATEWAY_REMOVE_SYMBOL RemoveGateway
#define NLS_GATEWAY_DESCRIPTOR_SYMBOL GetGatewayDescriptor
#endif
//=============================================================================
#define NLS_GATEWAY_FUNC_EXTENDED_IMPL(gateway, ptrInitializeFunction, eagerLoad)                  \
    static bool nlsGatewayHasInitializer() { return (void*)ptrInitializeFunction != nullptr; }     \
    static bool nlsGatewayRequiresEagerLoad() { return eagerLoad; }                                \
    EXTERN_AS_C EXPORTSYMBOL int NLS_GATEWAY_ADD_SYMBOL(void* eval, const wchar_t* moduleFilename) \
    {                                                                                              \
        return NelsonAddGatewayWithEvaluator(eval, moduleFilename, (void*)gateway,                 \
            sizeof(gateway) / sizeof(nlsGateway), gatewayName.c_str(),                             \
            (void*)ptrInitializeFunction);                                                         \
    }
//=============================================================================
// The initializer runs at startup and the library is loaded eagerly. Required
// when the initializer has effects consumed before any builtin of this gateway
// is called (see nlsGatewayDescriptor).
#define NLS_GATEWAY_FUNC_EXTENDED(gateway, ptrInitializeFunction)                                  \
    NLS_GATEWAY_FUNC_EXTENDED_IMPL(gateway, ptrInitializeFunction, true)
//=============================================================================
// The initializer only serves this gateway's own builtins: the library stays
// lazy and the initializer runs when the first builtin call resolves it.
#define NLS_GATEWAY_FUNC_EXTENDED_LAZY(gateway, ptrInitializeFunction)                             \
    NLS_GATEWAY_FUNC_EXTENDED_IMPL(gateway, ptrInitializeFunction, false)
//=============================================================================
// No initializer: nothing to run at startup, the library stays lazy.
#define NLS_GATEWAY_FUNC(gateway) NLS_GATEWAY_FUNC_EXTENDED_IMPL(gateway, NULL, false)
//=============================================================================
#define NLS_GATEWAY_REMOVE_EXTENDED(gateway, ptrFinishFunction)                                    \
    EXTERN_AS_C EXPORTSYMBOL const Nelson::nlsGatewayDescriptor* NLS_GATEWAY_DESCRIPTOR_SYMBOL()   \
    {                                                                                              \
        static const Nelson::nlsGatewayDescriptor descriptor = { gatewayName.c_str(), gateway,     \
            sizeof(gateway) / sizeof(nlsGateway), nlsGatewayHasInitializer(),                      \
            (void*)ptrFinishFunction != nullptr, nlsGatewayRequiresEagerLoad() };                  \
        return &descriptor;                                                                        \
    }                                                                                              \
    EXTERN_AS_C EXPORTSYMBOL bool NLS_GATEWAY_REMOVE_SYMBOL(                                       \
        void* eval, const wchar_t* moduleFilename)                                                 \
    {                                                                                              \
        return NelsonRemoveGatewayWithEvaluator(eval, moduleFilename, (void*)gateway,              \
            sizeof(gateway) / sizeof(nlsGateway), gatewayName.c_str(), (void*)ptrFinishFunction);  \
    }
//=============================================================================
#define NLS_GATEWAY_REMOVE(gateway) NLS_GATEWAY_REMOVE_EXTENDED(gateway, NULL)
//=============================================================================
