//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "BuiltInFunctionDefManager.hpp"
#include "Evaluator.hpp"
#include "GatewayHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
typedef enum
{
    CPP_BUILTIN,
    CPP_BUILTIN_WITH_EVALUATOR
} GATEWAY_PROTOTYPE;
//=============================================================================
typedef struct nlsGatewayStructType
{
    std::string functionName;
    BuiltInFuncPtr fptr;
    int nLhs;
    int nRhs;
    GATEWAY_PROTOTYPE gatewayPrototype;
} nlsGateway;
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
    EXTERN_AS_C EXPORTSYMBOL const wchar_t* GetGatewayName() { return gatewayName.c_str(); }
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
