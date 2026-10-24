//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GatewayHelpers.hpp"
#include <exception>
#include <string>
#include "Evaluator.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "FunctionsInMemory.hpp"
#include "RuntimeDispatch.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
mustDispatchGatewayToGuiThread(const wchar_t* gatewayName)
{
    if (!hasGuiServices() || isGuiThread() || gatewayName == nullptr) {
        return false;
    }
    std::wstring moduleName(gatewayName);
    return moduleName == L"gui" || moduleName == L"graphics" || moduleName == L"graphics_io"
        || moduleName == L"qml_engine" || moduleName == L"text_editor";
}
//=============================================================================
static bool
callGatewayLifecycle(
    PROC_InitializeGateway ptrFunc, Nelson::Evaluator* eval, const wchar_t* gatewayName)
{
    bool result = false;
    if (mustDispatchGatewayToGuiThread(gatewayName)) {
        std::exception_ptr guiException;
        callGuiSync([&]() {
            try {
                result = ptrFunc(eval);
            } catch (...) {
                guiException = std::current_exception();
            }
        });
        if (guiException) {
            std::rethrow_exception(guiException);
        }
        return result;
    }
    return ptrFunc(eval);
}
//=============================================================================
int
NelsonAddGatewayWithEvaluator(void* eval, const wchar_t* moduleFilename, void* gateway,
    int nbBuiltins, const wchar_t* gatewayName, void* ptrVoidInitializeFunction)
{
    auto ptrInitializeFunction = (PROC_InitializeGateway)ptrVoidInitializeFunction;
    const Nelson::nlsGateway* _gateway = (Nelson::nlsGateway*)gateway;
    auto* _eval = (Nelson::Evaluator*)eval;
    Context* ctx = _eval->getContext();
    if (ctx != nullptr) {
        for (size_t k = 0; k < (size_t)nbBuiltins; k++) {
            Nelson::BuiltInFunctionDefManager::getInstance()->add(_gateway[k].functionName,
                _gateway[k].fptr, _gateway[k].nRhs, _gateway[k].nLhs, moduleFilename, gatewayName,
                (size_t)_gateway[k].builtinPrototype, true, _gateway[k].builtinOverloadAutoMode);
        }
        if ((void*)ptrInitializeFunction != nullptr) {
            auto ptrFunc = reinterpret_cast<PROC_InitializeGateway>(ptrInitializeFunction);
            return callGatewayLifecycle(ptrFunc, _eval, gatewayName) ? 1 : 0;
        }
        return 1;
    }
    return 0;
}
//=============================================================================
int
NelsonRemoveGatewayWithEvaluator(void* eval, const wchar_t* moduleFilename, void* gateway,
    int nbBuiltins, const wchar_t* gatewayName, void* ptrVoidFinishFunction)
{
    auto ptrFinishFunction = (PROC_FinishGateway)ptrVoidFinishFunction;
    const Nelson::nlsGateway* _gateway = (Nelson::nlsGateway*)gateway;
    auto* _eval = (Nelson::Evaluator*)eval;
    Context* ctx = _eval->getContext();
    if (ctx != nullptr) {
        for (size_t k = 0; k < (size_t)nbBuiltins; ++k) {
            Nelson::FunctionsInMemory::getInstance()->deleteBuiltinFunctionReference(
                _gateway[k].functionName);
            Nelson::BuiltInFunctionDefManager::getInstance()->remove(_gateway[k].fptr);
        }
        if ((void*)ptrFinishFunction != nullptr) {
            auto ptrFunc = reinterpret_cast<PROC_FinishGateway>(ptrFinishFunction);
            return callGatewayLifecycle(ptrFunc, _eval, gatewayName) ? 1 : 0;
        }
        return 1;
    }
    return 0;
}
//=============================================================================
