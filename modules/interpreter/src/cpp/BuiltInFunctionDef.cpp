//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "BuiltInFunctionDef.hpp"
#include <exception>
#include <vector>
#include "EvaluateBuiltinCatchRuntimeException.hpp"
#include "Profiler.hpp"
#include "ProfilerHelpers.hpp"
#include "NelsonGateway.hpp"
#include "OverloadHelpers.hpp"
#include "OverloadDispatchArgument.hpp"
#include "ClassdefParser.hpp"
#include "ClassdefHandleObject.hpp"
#include "HandleState.hpp"
#include "RuntimeDispatch.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static LazyGatewayResolver lazyGatewayResolver = nullptr;
//=============================================================================
void
SetLazyGatewayResolver(LazyGatewayResolver resolver)
{
    lazyGatewayResolver = resolver;
}
//=============================================================================
static std::vector<PreModulesTeardownCallback>&
preModulesTeardownCallbacks()
{
    static std::vector<PreModulesTeardownCallback> callbacks;
    return callbacks;
}
//=============================================================================
void
RegisterPreModulesTeardownCallback(PreModulesTeardownCallback callback)
{
    if (callback != nullptr) {
        preModulesTeardownCallbacks().push_back(callback);
    }
}
//=============================================================================
void
RunPreModulesTeardownCallbacks()
{
    auto& callbacks = preModulesTeardownCallbacks();
    for (auto* callback : callbacks) {
        if (callback != nullptr) {
            callback();
        }
    }
    callbacks.clear();
}
//=============================================================================
static bool
mustDispatchBuiltinToGuiThread(const std::wstring& moduleName)
{
    return hasGuiServices() && !isGuiThread()
        && (moduleName == L"gui" || moduleName == L"graphics" || moduleName == L"graphics_io"
            || moduleName == L"qml_engine" || moduleName == L"text_editor");
}
//=============================================================================
// Listeners (event.listener) and dynamic property descriptors
// (meta.DynamicProperty) are native objects whose methods are registered by
// the handle gateway, never by a class file. Probing their dotted class name
// as a classdef would stat a +package/Class.m candidate in every path
// directory on each builtin call.
static bool
isNativeClassdefCompanion(const ArrayOf& value)
{
    if (!value.isScalar()) {
        return false;
    }
    const auto* refs = static_cast<const HandleRef*>(value.dataPointer());
    if (refs == nullptr || !refs[0]) {
        return false;
    }
    const HandleGenericObject* object = refs[0]->object();
    return dynamic_cast<const ClassdefListenerObject*>(object) != nullptr
        || dynamic_cast<const ClassdefDynamicPropertyObject*>(object) != nullptr;
}
//=============================================================================
BuiltInFunctionDef::BuiltInFunctionDef(bool isOverload) : FunctionDef(isOverload)
{
    fileName.clear();
    retCount = 0;
    argCount = 0;
    fptr = nullptr;
    builtinPrototype = (size_t)BUILTIN_PROTOTYPE::CPP_BUILTIN;
    interleavedComplex = true;
    callerContextAccess = false;
    callerContextNeutral = false;
    lazyGateway = false;
    moduleName.clear();
}
//=============================================================================
BuiltInFunctionDef::~BuiltInFunctionDef() = default;
//=============================================================================
ArrayOfVector
BuiltInFunctionDef::evaluateFunction(Evaluator* eval, const ArrayOfVector& inputs, int nargout)
{
    if (lazyGateway.load(std::memory_order_acquire)) {
        auto guard = lockGuard();
        if (lazyGateway.load(std::memory_order_acquire)) {
            if (lazyGatewayResolver == nullptr) {
                Error("Nelson:interpreter:gatewayResolverNotInitialized",
                    "Lazy gateway resolver is not initialized.");
            }
            std::wstring errorMessage;
            if (!lazyGatewayResolver(eval, getFilename(), getName(), errorMessage)) {
                if (errorMessage.empty()) {
                    Error("Nelson:interpreter:cannotLoadGateway", "Cannot load lazy gateway.");
                }
                RaiseError(errorMessage, L"Nelson:interpreter:cannotLoadGateway");
            }
            FunctionDefPtr resolved = nullptr;
            if (!BuiltInFunctionDefManager::getInstance()->find(getName(), resolved)
                || resolved == nullptr) {
                Error("Nelson:interpreter:cannotResolveGateway",
                    "Cannot resolve lazy gateway builtin.");
            }
            if (resolved != this) {
                guard.unlock();
                return resolved->evaluateFunction(eval, inputs, nargout);
            }
            if (lazyGateway.load(std::memory_order_acquire)) {
                Error("Nelson:interpreter:cannotResolveGateway",
                    "Cannot resolve lazy gateway builtin.");
            }
        }
    }
    if (eval->withOverload && inputs.size() > 0 && !this->isOverload()
        && this->overloadAutoMode == NLS_OVERLOAD_AUTO_ON) {
        const ArrayOf& dispatchInput = inputs[overloadDispatchArgumentIndex(inputs, getName())];
        // A classdef object built by a C++ builtin (e.g. a table returned by
        // readtable) may reach here before its class file was referenced by name,
        // leaving its method overloads unregistered. Load the class on demand so
        // overloads such as size/width resolve instead of using the generic builtin.
        if (dispatchInput.isClassType()) {
            classdefLoadClass(dispatchInput.classType());
        }
        bool wasFound = false;
        ArrayOfVector res = callOverloadedFunction(eval,
            NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), nargout, inputs,
            getName(), ClassName(dispatchInput), dispatchInput.dataClass(), wasFound);
        if (wasFound) {
            return res;
        }
        // callOverloadedFunction only checks the exact class; a classdef object
        // may overload this builtin through a method inherited from a superclass
        // (e.g. bndfun inherits feval/get from classicfun). Resolve the method
        // along the inheritance chain and dispatch to it.
        if (dispatchInput.isClassType()
            || (dispatchInput.isHandle() && !isNativeClassdefCompanion(dispatchInput))) {
            const std::string currentType = dispatchInput.isHandle()
                ? dispatchInput.handleCategory()
                : dispatchInput.classType();
            if (!currentType.empty() && classdefLoadClass(currentType)) {
                std::string inheritedOverloadName;
                if (classdefResolveMethodFunction(currentType, getName(), inheritedOverloadName,
                        eval->getClassdefAccessContext())) {
                    FunctionDef* inheritedDef = nullptr;
                    if (eval->getContext()->lookupFunction(inheritedOverloadName, inheritedDef)
                        && inheritedDef != nullptr && inheritedDef != this) {
                        return inheritedDef->evaluateFunction(eval, inputs, nargout);
                    }
                }
            }
        }
    }

    ArrayOfVector outputs;
    eval->callstack.pushDebug(this->getName(), this->getDebugDetail());
    size_t stackDepth = eval->callstack.size();
    uint64 tic = 0;
    try {
        tic = Profiler::getInstance()->tic();
        if (mustDispatchBuiltinToGuiThread(moduleName)) {
            std::exception_ptr guiException;
            callGuiSync([&]() {
                try {
                    outputs = EvaluateBuiltinCatchRuntimeException(
                        eval, fptr, inputs, nargout, builtinPrototype, interleavedComplex);
                } catch (...) {
                    guiException = std::current_exception();
                }
            });
            if (guiException) {
                std::rethrow_exception(guiException);
            }
        } else {
            outputs = EvaluateBuiltinCatchRuntimeException(
                eval, fptr, inputs, nargout, builtinPrototype, interleavedComplex);
        }
        if (tic != 0) {
            internalProfileFunction stack
                = computeProfileStack(eval, this->getName(), this->getFilename());
            Profiler::getInstance()->toc(tic, stack);
        }
        while (eval->callstack.size() > stackDepth) {
            eval->callstack.pop_back();
        }
        eval->callstack.popDebug();
        return outputs;
    } catch (const NelsonException&) {
        if (tic != 0) {
            internalProfileFunction stack
                = computeProfileStack(eval, this->getName(), this->getFilename());
            Profiler::getInstance()->toc(tic, stack);
        }
        while (eval->callstack.size() > stackDepth) {
            eval->callstack.pop_back();
        }
        eval->callstack.popDebug();
        throw;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
