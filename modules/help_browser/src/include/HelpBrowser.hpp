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
#include "Types.hpp"
#include "nlsHelp_browser_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSHELP_BROWSER_IMPEXP HelpBrowser
{
public:
    static HelpBrowser*
    getInstance();
    void
    registerHelpFile(const std::wstring& filename);
    void
    unregisterHelpFile(const std::wstring& filename);
    void
    showDocByName(const std::wstring& name);
    void
    showDocByIdentifier(const std::wstring& identifier);
    bool
    startBrowser(std::wstring& msg);
    void
    closeBrowser();
    void
    syncBrowser();
    void
    sendCommand(const std::wstring& cmd);
    void
    destroy();
    void
    clearCache();
    std::wstring
    getCachePath();
    wstringVector
    getAttributes();

private:
    HelpBrowser();
    HelpBrowser(HelpBrowser const&){};
    std::wstring
    getQhcFilename();
    std::wstring
    getQhcPath();
    static HelpBrowser* m_pInstance;
    std::wstring
    getCacheFile();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
