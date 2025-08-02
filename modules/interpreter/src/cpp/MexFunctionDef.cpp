//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include "FileSystemWrapper.hpp"
#include "MexFunctionDef.hpp"
#include "DynamicLibrary.hpp"
#include "Profiler.hpp"
#include "ProfilerHelpers.hpp"
#include "characters_encoding.hpp"
#include "CallMexBuiltin.hpp"
#include "OverloadHelpers.hpp"
//=============================================================================
#define BUFFER_LENGTH_NAME 4096
#define MEXFILEREQUIREDAPIVERSION_ENTRY "mexfilerequiredapiversion"
#define MEXFUNCTION_ENTRY "mexFunction"
#define FUNCTIONNAME_ENTRY "functionName"
#define MEXCLEARATEXIT_ENTRY "mexClearAtExit"
#define MEXISLOCKED_ENTRY "mexIsLocked"
//=============================================================================
namespace Nelson {
//=============================================================================
using PROC_MexFileRequiredApiVersion = void (*)(int*, int*);
using PROC_MexClearAtExit = void (*)();
using PROC_MexIsLocked = bool (*)();
//=============================================================================
MexFunctionDef::MexFunctionDef(
    const std::wstring& filename, const std::wstring& name, bool isOverload)
    : FunctionDef(isOverload)
{
    FileSystemWrapper::Path p(filename);
    p = FileSystemWrapper::Path::absolute(p);

    this->setName(wstring_to_utf8(name));
    this->setFilename(p.generic_wstring());
    interleavedComplex = false;
    loaded = false;
    libraryPtr = nullptr;
    mexFunctionPtr = nullptr;
    mexIsLockedPtr = nullptr;

#ifdef _MSC_VER
    library_handle nlsMexHandleDynamicLibrary = Nelson::load_dynamic_libraryW(p.generic_wstring());
#else
    library_handle nlsMexHandleDynamicLibrary = Nelson::load_dynamic_library(p.generic_string());
#endif
    if (nlsMexHandleDynamicLibrary != nullptr) {

        generic_function_ptr PROC_mexFunctionPtr = nullptr;
        PROC_MexFileRequiredApiVersion PROC_MexFileRequiredApiVersionPtr = nullptr;
        PROC_MexClearAtExit PROC_MexClearAtExitPtr = nullptr;
        generic_function_ptr PROC_MexIsLockedPtr = nullptr;

        PROC_MexFileRequiredApiVersionPtr = reinterpret_cast<PROC_MexFileRequiredApiVersion>(
            get_function(nlsMexHandleDynamicLibrary, MEXFILEREQUIREDAPIVERSION_ENTRY));
        PROC_mexFunctionPtr = get_function(nlsMexHandleDynamicLibrary, MEXFUNCTION_ENTRY);
        PROC_MexClearAtExitPtr = reinterpret_cast<PROC_MexClearAtExit>(
            get_function(nlsMexHandleDynamicLibrary, MEXCLEARATEXIT_ENTRY));
        char* functionName = (char*)get_function(nlsMexHandleDynamicLibrary, FUNCTIONNAME_ENTRY);
        PROC_MexIsLockedPtr = get_function(nlsMexHandleDynamicLibrary, MEXISLOCKED_ENTRY);

        if (PROC_mexFunctionPtr == nullptr) {
            close_dynamic_library(nlsMexHandleDynamicLibrary);
            nlsMexHandleDynamicLibrary = nullptr;
            loaded = false;
            return;
        }
        std::string utf8name = wstring_to_utf8(name);
        if (functionName && strcmp(utf8name.c_str(), functionName)) {
            strncpy(functionName, utf8name.c_str(), BUFFER_LENGTH_NAME);
        }
        int buildRelease = 0;
        int targetApiVersion = 0;
        if (PROC_MexFileRequiredApiVersionPtr) {
            PROC_MexFileRequiredApiVersionPtr(&buildRelease, &targetApiVersion);
            interleavedComplex = targetApiVersion != 0x07300000;
        }
        loaded = true;

        libraryPtr = (void*)nlsMexHandleDynamicLibrary;
        mexFunctionPtr = (void*)PROC_mexFunctionPtr;
        mexClearAtExitFunctionPtr = (void*)PROC_MexClearAtExitPtr;
        mexIsLockedPtr = (void*)PROC_MexIsLockedPtr;
    }
}
//=============================================================================
bool
MexFunctionDef::clear()
{
    if (loaded) {
        if (mexClearAtExitFunctionPtr != nullptr) {
            auto exitFun = (PROC_MexClearAtExit)mexClearAtExitFunctionPtr;
            exitFun();
            return true;
        }
    }
    return false;
}
//=============================================================================
MexFunctionDef::~MexFunctionDef()
{
    clear();
    if (loaded) {
        close_dynamic_library((library_handle)libraryPtr);
    }
    loaded = false;
    libraryPtr = nullptr;
    mexFunctionPtr = nullptr;
    mexClearAtExitFunctionPtr = nullptr;
    mexIsLockedPtr = nullptr;
}
//=============================================================================
ArrayOfVector
MexFunctionDef::evaluateFunction(Evaluator* eval, const ArrayOfVector& inputs, int nargout)
{
    lock();
    if (eval->withOverload && inputs.size() > 0 && !this->isOverload()
        && this->overloadAutoMode == NLS_OVERLOAD_AUTO_ON) {
        bool wasFound = false;
        ArrayOfVector res = callOverloadedFunction(eval,
            NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), nargout, inputs,
            getName(), ClassName(inputs[0]), inputs[0].getDataClass(), wasFound);
        if (wasFound) {
            return res;
        }
    }

    ArrayOfVector outputs;
    eval->callstack.pushDebug(this->getName(), std::string("built-in ") + this->getName());
    size_t stackDepth = eval->callstack.size();
    uint64 tic = 0;
    try {
        tic = Profiler::getInstance()->tic();
        CallMexBuiltin(mexFunctionPtr, inputs, nargout, outputs, interleavedComplex);
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
    } catch (const Exception&) {
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
bool
MexFunctionDef::isLocked()
{
    if (loaded) {
        if (mexIsLockedPtr != nullptr) {
            auto mexIsLocked = (PROC_MexIsLocked)mexIsLockedPtr;
            return mexIsLocked();
        }
    }
    return false;
}
//=============================================================================
bool
MexFunctionDef::updateCode()
{
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
