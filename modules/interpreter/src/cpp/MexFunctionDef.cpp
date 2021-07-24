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
#include <boost/filesystem.hpp>
#include "MexFunctionDef.hpp"
#include "dynamic_library.hpp"
#include "Profiler.hpp"
#include "ProfilerHelpers.hpp"
#include "characters_encoding.hpp"
#include "CallMexBuiltin.hpp"
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
MexFunctionDef::MexFunctionDef(const std::wstring& filename, const std::wstring& name)
{
    boost::filesystem::path p(filename);
    p = boost::filesystem::absolute(p);

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

        if (PROC_MexFileRequiredApiVersionPtr == nullptr || PROC_mexFunctionPtr == nullptr
            || functionName == nullptr || PROC_MexClearAtExitPtr == nullptr
            || PROC_MexIsLockedPtr == nullptr) {
            close_dynamic_library(nlsMexHandleDynamicLibrary);
            nlsMexHandleDynamicLibrary = nullptr;
            loaded = false;
            return;
        }
        std::string utf8name = wstring_to_utf8(name);
        if (strcmp(utf8name.c_str(), functionName)) {
            strncpy(functionName, utf8name.c_str(), BUFFER_LENGTH_NAME);
        }
        int buildRelease = 0;
        int targetApiVersion = 0;
        PROC_MexFileRequiredApiVersionPtr(&buildRelease, &targetApiVersion);
        interleavedComplex = targetApiVersion != 0x07300000;
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
            PROC_MexClearAtExit exitFun = (PROC_MexClearAtExit)mexClearAtExitFunctionPtr;
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
            PROC_MexIsLocked mexIsLocked = (PROC_MexIsLocked)mexIsLockedPtr;
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
