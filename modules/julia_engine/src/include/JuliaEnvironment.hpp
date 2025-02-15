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
#include "nlsJulia_engine_exports.h"
#include "HandleGenericObject.hpp"
#include "Types.hpp"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSJULIA_ENGINE_IMPEXP JuliaEnvironment : HandleGenericObject
{
    //=============================================================================
protected:
    JuliaEnvironment();

private:
    //=============================================================================
    static JuliaEnvironment* instance;
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
    bool
    loadPreviousStateFromEnvironmentVariables();
    //=============================================================================
    std::wstring
    getStatusString();
    //=============================================================================
    std::wstring
    getExecutionModeString();
    //=============================================================================
public:
    static JuliaEnvironment*
    getInstance()
    {
        if (!instance) {
            instance = new JuliaEnvironment();
        }
        return instance;
    }
    //=============================================================================
    bool
    saveCurrentState();
    //=============================================================================
    std::wstring
    getVersion() const;
    std::wstring
    getExecutable() const;
    std::wstring
    getLibrary() const;
    std::wstring
    getHome() const;
    int
    getStatus() const;
    int
    getExecutionMode() const;
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
NLSJULIA_ENGINE_IMPEXP bool
configureJuliaEnvironment(const JuliaEnvironment* juliaEnvironment);
//=============================================================================
}
//=============================================================================
