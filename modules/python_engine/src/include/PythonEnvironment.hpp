//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "nlsPython_engine_exports.h"
#include "HandleGenericObject.hpp"
#include "Types.hpp"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSPYTHON_ENGINE_IMPEXP PythonEnvironment : HandleGenericObject
{
    //=============================================================================
protected:
    PythonEnvironment();

private:
    //=============================================================================
    static PythonEnvironment* instance;
    //=============================================================================
    std::wstring _version;
    std::wstring _executable;
    std::wstring _library;
    std::wstring _home;
    int _status;
    int _executionMode;
    wstringVector propertiesNames;
    //=============================================================================
    bool
    loadPreviousState();
    //=============================================================================
    std::wstring
    getStatusString();
    //=============================================================================
    std::wstring
    getExecutionModeString();
    //=============================================================================
public:
    static PythonEnvironment*
    getInstance()
    {
        if (!instance) {
            instance = new PythonEnvironment();
        }
        return instance;
    }
    //=============================================================================
    bool
    saveCurrentState();
    //=============================================================================
    std::wstring
    getVersion();
    std::wstring
    getExecutable();
    std::wstring
    getLibrary();
    std::wstring
    getHome();
    int
    getStatus();
    int
    getExecutionMode();
    //=============================================================================
    void
    setVersion(const std::wstring& version);
    void
    setExecutable(const std::wstring& executable);
    void
    setLibrary(const std::wstring& library);
    void
    setHome(const std::wstring& home);
    void
    setStatus(int status);
    void
    setExecutionMode(int mode);
    //=============================================================================
    wstringVector
    getProperties();
    //=============================================================================
    wstringVector
    getMethods();
    //=============================================================================
    bool
    get(const std::wstring& propertyName, ArrayOf& result);
    //=============================================================================
    void
    display(Interface* io);
    //=============================================================================
};
//=============================================================================
NLSPYTHON_ENGINE_IMPEXP void
configurePythonEnvironment(const std::wstring& executablePath);
//=============================================================================

}
//=============================================================================
