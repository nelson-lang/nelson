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
#include "ArrayOf.hpp"
#include "QObjectHandleObject.hpp"
#include "nlsQml_engine_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSQML_ENGINE_IMPEXP QmlEngine
{
public:
    static QmlEngine*
    getInstance();
    QObjectHandleObject*
    loadQmlFile(const std::wstring& filename);
    QObjectHandleObject*
    setData(const std::wstring& data);
    QObjectHandleObject*
    createQQuickView(const std::wstring& filename);

    ArrayOf
    evaluateString(const std::wstring& program, bool& withOuput);
    ArrayOf
    evaluateFile(const std::wstring& filename, bool& withOuput);
    void
    collectGarbage();
    void
    clearComponentCache();
    wstringVector
    importPathList();
    wstringVector
    pluginPathList();
    std::wstring
    offlineStoragePath();

    void
    addImportPath(const std::wstring& path);
    void
    addPluginPath(const std::wstring& path);
    void
    setOfflineStoragePath(const std::wstring& dir);

    void
    destroy();

private:
    QmlEngine();
    static QmlEngine* m_pInstance;
};
//=============================================================================
NLSQML_ENGINE_IMPEXP void
terminateQmlEngine();
//=============================================================================
} // namespace Nelson
//=============================================================================
