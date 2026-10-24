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
#include "nlsModules_manager_exports.h"
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson {
class NLSMODULES_MANAGER_IMPEXP GatewaysManager
{
    //=============================================================================
public:
    //=============================================================================
    static GatewaysManager*
    getInstance();
    //=============================================================================
    void
    destroy(Evaluator* eval);
    //=============================================================================
    bool
    addGateway(Evaluator* eval, const std::wstring& libraryFullName, std::wstring& errorMessage,
        bool forceLoaded = false);
    //=============================================================================
    bool
    removeGateway(Evaluator* eval, const std::wstring& libraryFullName, std::wstring& errorMessage);
    //=============================================================================
    bool
    resolveLazyGateway(Evaluator* eval, const std::wstring& libraryFullName,
        const std::string& functionName, std::wstring& errorMessage);
    //=============================================================================
    wstringVector
    getLibraryNames();
    //=============================================================================
    std::wstring
    getGatewayState(const std::wstring& libraryFullName, std::wstring& errorMessage);
    //=============================================================================
    // Gateway name and builtin list of a lazy (not yet loaded) gateway, served
    // from the descriptor cache without loading the library. Returns false when
    // the gateway is not registered lazily.
    bool
    getLazyGatewayInfo(
        const std::wstring& libraryFullName, std::wstring& moduleName, stringVector& builtins);
    //=============================================================================
private:
    GatewaysManager();
    //=============================================================================
    static GatewaysManager* m_pInstance;
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
