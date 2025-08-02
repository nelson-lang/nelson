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
    std::string functionName;
    ptrBuiltin fptr;
    int nLhs;
    int nRhs;
    BUILTIN_PROTOTYPE builtinPrototype;
    FunctionOverloadAutoMode builtinOverloadAutoMode;
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
#define NLSGATEWAYFUNCEXTENDED(gateway, ptrInitializeFunction)                                     \
    EXTERN_AS_C EXPORTSYMBOL int AddGateway(void* eval, const wchar_t* moduleFilename)             \
    {                                                                                              \
        return NelsonAddGatewayWithEvaluator(eval, moduleFilename, (void*)gateway,                 \
            sizeof(gateway) / sizeof(nlsGateway), gatewayName.c_str(),                             \
            (void*)ptrInitializeFunction);                                                         \
    }
//=============================================================================
#define NLSGATEWAYFUNC(gateway) NLSGATEWAYFUNCEXTENDED(gateway, NULL)
//=============================================================================
#define NLSGATEWAYNAME()                                                                           \
    EXTERN_AS_C EXPORTSYMBOL const wchar_t* GetGatewayName()                                       \
    {                                                                                              \
        return gatewayName.c_str();                                                                \
    }
//=============================================================================
#define NLSGATEWAYINFO(gateway)                                                                    \
    EXTERN_AS_C EXPORTSYMBOL Nelson::stringVector GetGatewayInfo()                                 \
    {                                                                                              \
        stringVector res;                                                                          \
        size_t nbBuiltins = sizeof(gateway) / sizeof(nlsGateway);                                  \
        res.reserve(nbBuiltins);                                                                   \
        for (size_t k = 0; k < nbBuiltins; k++) {                                                  \
            res.push_back(gateway[k].functionName);                                                \
        }                                                                                          \
        return res;                                                                                \
    }
//=============================================================================
#define NLSGATEWAYREMOVEEXTENDED(gateway, ptrFinishFunction)                                       \
    EXTERN_AS_C EXPORTSYMBOL bool RemoveGateway(void* eval, const wchar_t* moduleFilename)         \
    {                                                                                              \
        return NelsonRemoveGatewayWithEvaluator(eval, moduleFilename, (void*)gateway,              \
            sizeof(gateway) / sizeof(nlsGateway), gatewayName.c_str(), (void*)ptrFinishFunction);  \
    }
//=============================================================================
#define NLSGATEWAYREMOVE(gateway) NLSGATEWAYREMOVEEXTENDED(gateway, NULL)
//=============================================================================
